//! P2.5: Trait & Vtable Lowering
//!
//! Generates VTable struct types, TraitObj fat-pointer types, static vtable
//! instances, and trait implementation methods.

use rustc_hash::FxHashMap;

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::ir::{Global, GlobalInit};
use crate::parser::ast::{self, FunctionBody, Item, Ownership, TraitItem, Type};
use crate::semantic::meta::{self as meta, DelayedMetaContext};

use super::context::LoweringContext;
use super::drops::DropScopeKind;
use super::exprs::lower_expr;
use super::generics;
use super::stmts::lower_block;

/// Return the stored ParamABI entries for a vtable method.
fn vtable_abis(vtable_method: &VTableMethod) -> Vec<super::context::ParamABI> {
    vtable_method.param_abis.clone()
}

/// Reserved name of the vtable's drop-glue slot (`void (*__drop)(void*)`).
/// Appended as the LAST vtable field so method indices stay stable; method
/// resolution is by-name and can never collide with it (user methods cannot
/// be named `__drop`).
pub const VTABLE_DROP_FIELD: &str = "__drop";

/// Information about a trait's vtable layout.
pub struct TraitVTableInfo {
    /// Name of the VTable struct type (e.g., "Shape_VTable").
    pub vtable_type_name: String,
    /// Name of the TraitObj struct type (e.g., "Shape_TraitObj").
    pub trait_obj_type_name: String,
    /// GIR TypeId for the VTable struct.
    pub vtable_type_id: TypeId,
    /// GIR TypeId for the TraitObj struct.
    pub trait_obj_type_id: TypeId,
    /// Ordered list of method slots.
    pub methods: Vec<VTableMethod>,
}

/// A method slot in a vtable.
pub struct VTableMethod {
    /// Method name.
    pub name: String,
    /// TypeId for the function pointer type in the TypeRegistry.
    pub fn_ptr_type_id: TypeId,
    /// Full parameter list including void* self as first param (resolved via resolve_param_type).
    /// Used for the vtable FnPtr signature and wrapper function params.
    pub param_types: Vec<TypeId>,
    /// Base (unresolved) parameter types for fn_sigs registration.
    /// pass-by-pointer checks at call sites need the base type to detect resource types.
    pub base_param_types: Vec<TypeId>,
    /// Method return type.
    pub return_type: TypeId,
    /// Whether self is `&self` (mutable borrow).
    pub self_is_mutable: bool,
    /// Per-parameter ABI (self + non-self params). Computed from ownership at trait definition.
    pub param_abis: Vec<super::context::ParamABI>,
}

/// Pre-scan: register VTable and TraitObj types for all non-generic trait definitions.
///
/// Returns a map of trait_name -> TraitVTableInfo for use by later passes.
pub fn register_trait_types(
    ctx: &mut LoweringContext,
    ast_module: &ast::Module,
) -> FxHashMap<String, TraitVTableInfo> {
    let mut trait_info = FxHashMap::default();

    // Pre-allocate void pointer types
    let void_ptr = ctx.register_ptr_type(UNIT_TYPE);
    let mut_void_ptr = ctx.register_mut_ptr_type(UNIT_TYPE);

    for item in &ast_module.items {
        if let Item::Trait(trait_def) = &item.node {
            // Skip generic traits (would need monomorphization)
            if trait_def.generic_params.is_some() {
                continue;
            }

            let trait_name = &trait_def.name.node;

            // Collect method slots from the trait definition.
            // Only instance methods (with a `self` parameter) can be dispatched via vtable.
            // Static methods are handled outside the vtable machinery.
            let mut methods = Vec::new();
            for trait_item in &trait_def.items {
                if let TraitItem::Method(method_def) = &trait_item.node {
                    // Skip static methods (no `self` parameter) — they cannot be in a vtable.
                    let has_self = method_def.params.first()
                        .map(|p| p.node.name.node == "self")
                        .unwrap_or(false);
                    if !has_self {
                        continue;
                    }

                    let method_name = &method_def.name.node;
                    let return_type = if let Some(throws) = method_def.throws.explicit_type() {
                        // `int parse(self, String input) throws String` →
                        // Result[int, String]. One source of truth
                        // (devbook-24 rule 3): synthesize via the shared helper
                        // — same path as the free-fn / equip-method pre-scans
                        // and the method-body lowering.
                        crate::ir::lowering::types::synthesize_throws_result_type(
                            &mut ctx.type_mapper,
                            &mut ctx.type_registry,
                            &method_def.return_type.node,
                            &throws.node,
                        )
                    } else {
                        ctx.type_mapper.map_ast_type(&method_def.return_type.node)
                    };

                    // Determine self mutability
                    let self_is_mutable = method_def
                        .params
                        .first()
                        .map(|p| {
                            p.node.name.node == "self"
                                && matches!(p.node.ownership, Ownership::MutableBorrow)
                        })
                        .unwrap_or(false);

                    let self_type = if self_is_mutable {
                        mut_void_ptr
                    } else {
                        void_ptr
                    };

                    // Build full parameter list: void* self + other params
                    // param_types uses resolve_param_type (for vtable FnPtr + wrapper fn signature).
                    // base_param_types uses map_ast_type (for fn_sigs pass-by-pointer at call sites).
                    let mut param_types = vec![self_type];
                    let mut base_param_types = vec![self_type];
                    let mut param_abis = vec![if self_is_mutable {
                        super::context::ParamABI::ByMutPtr
                    } else {
                        super::context::ParamABI::ByPtr
                    }];
                    for p in &method_def.params {
                        if p.node.name.node == "self" {
                            continue;
                        }
                        let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
                        let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
                        param_types.push(gir_type);
                        base_param_types.push(base_type);
                        param_abis.push(ctx.compute_param_abi(base_type, p.node.ownership));
                    }

                    // Create function pointer type in the registry
                    let fn_ptr_type_id = ctx.type_registry.insert(GirType::FnPtr {
                        params: param_types.clone(),
                        return_type,
                        // Vtable method slots take the sigils spelled on the
                        // trait method signature. Populate from the method's
                        // own AST params so a `&self`/`&arg` slot dispatches
                        // through `lower_call_arg` correctly at the read
                        // side (currently this vtable path doesn't reach
                        // the non-identifier arm, but the field keeps the
                        // invariant that every FnPtr writer sets it).
                        param_ownerships: {
                            let mut owns: Vec<Ownership> = vec![
                                if self_is_mutable { Ownership::MutableBorrow } else { Ownership::Borrow }
                            ];
                            for p in &method_def.params {
                                if p.node.name.node == "self" {
                                    continue;
                                }
                                owns.push(p.node.ownership);
                            }
                            owns
                        },
                    });

                    methods.push(VTableMethod {
                        name: method_name.clone(),
                        fn_ptr_type_id,
                        param_types,
                        base_param_types,
                        return_type,
                        self_is_mutable,
                        param_abis,
                    });
                }
            }

            if methods.is_empty() {
                continue;
            }

            // -- Create VTable struct TypeDef --
            let vtable_type_name = format!("{trait_name}_VTable");
            let mut vtable_fields: Vec<StructField> = methods
                .iter()
                .map(|m| StructField {
                    name: m.name.clone(),
                    type_id: m.fn_ptr_type_id,
                })
                .collect();
            // Drop-glue slot: `void (*__drop)(void* data_slot)` — points at
            // the concrete type's `Box__<Concrete>__drop` wrapper (drops the
            // boxed payload's own resources, then frees the data box and
            // nulls the slot). APPENDED LAST so positional method indices
            // (`TraitCall.method_idx`) stay stable; method resolution is
            // by-name and never matches `__drop`. Populated per-impl by
            // `emit_vtable_globals`; consumed by the trait-box drop path in
            // `src/lir/lower/drops.rs`.
            let drop_fn_ptr_type = ctx.type_registry.insert(GirType::FnPtr {
                params: vec![mut_void_ptr],
                return_type: UNIT_TYPE,
                param_ownerships: vec![Ownership::MutableBorrow],
            });
            vtable_fields.push(StructField {
                name: VTABLE_DROP_FIELD.to_string(),
                type_id: drop_fn_ptr_type,
            });

            ctx.type_registry.add_type_def(TypeDef {
                name: vtable_type_name.clone(),
                kind: TypeDefKind::Struct(StructDef {
                    fields: vtable_fields,
                }),
                metadata: TypeMetadata::default(),
            });
            let vtable_type_id = ctx
                .type_registry
                .insert(GirType::Named(vtable_type_name.clone()));
            ctx.type_mapper
                .register_named(vtable_type_name.clone(), vtable_type_id);

            // -- Create TraitObj struct TypeDef --
            let trait_obj_type_name = format!("{trait_name}_TraitObj");
            let vtable_ptr_type = ctx.register_ptr_type(vtable_type_id);
            ctx.type_registry.add_type_def(TypeDef {
                name: trait_obj_type_name.clone(),
                kind: TypeDefKind::Struct(StructDef {
                    fields: vec![
                        StructField {
                            name: "data".into(),
                            type_id: mut_void_ptr,
                        },
                        StructField {
                            name: "vtable".into(),
                            type_id: vtable_ptr_type,
                        },
                    ],
                }),
                metadata: TypeMetadata::default(),
            });
            let trait_obj_type_id = ctx
                .type_registry
                .insert(GirType::Named(trait_obj_type_name.clone()));
            ctx.type_mapper
                .register_named(trait_obj_type_name.clone(), trait_obj_type_id);

            trait_info.insert(
                trait_name.clone(),
                TraitVTableInfo {
                    vtable_type_name,
                    trait_obj_type_name,
                    vtable_type_id,
                    trait_obj_type_id,
                    methods,
                },
            );
        }
    }

    trait_info
}

/// Pre-scan: register trait implementation method signatures in fn_sigs.
pub fn register_trait_equip_sigs(
    ctx: &mut LoweringContext,
    trait_info: &FxHashMap<String, TraitVTableInfo>,
    ast_module: &ast::Module,
) {
    for item in &ast_module.items {
        if let Item::Equip(equip) = &item.node {
            let trait_name = match &equip.trait_ {
                Some(t) => extract_trait_name(&t.trait_name.node),
                None => continue,
            };

            if trait_name.is_empty() || equip.generic_params.is_some() {
                continue;
            }

            let type_name = match extract_type_name(&equip.type_.node) {
                Some(n) => n,
                None => continue,
            };

            let vtable_info = match trait_info.get(&trait_name) {
                Some(info) => info,
                None => continue,
            };

            let mut implemented_methods: Vec<String> = Vec::new();

            for method in &equip.items {
                let method_name = &method.node.name.node;
                let mangled =
                    format!("{trait_name}_for_{type_name}__{method_name}");
                implemented_methods.push(method_name.to_string());

                if let Some(vtable_method) = vtable_info
                    .methods
                    .iter()
                    .find(|m| m.name == *method_name)
                {
                    // Use base_param_types for fn_sigs so pass-by-pointer
                    // triggers correctly at call sites (is_resource_type check).
                    ctx.fn_sigs.insert(
                        mangled.clone(),
                        (
                            vtable_method.base_param_types.clone(),
                            vtable_method.return_type,
                        ),
                    );
                    // Compute ABI from vtable method info
                    ctx.fn_param_abis.insert(mangled, vtable_abis(vtable_method));
                } else {
                    // Method not in this trait's vtable — likely inherited from parent trait.
                    // Register as Type__method for direct dispatch.
                    let method_def = &method.node;
                    let has_self = method_def.params.first()
                        .map(|p| p.node.name.node == "self")
                        .unwrap_or(false);
                    let ret_type = ctx.type_mapper.map_ast_type(&method_def.return_type.node);
                    let mut param_types = Vec::new();
                    let mut abis = Vec::new();
                    if has_self {
                        let self_type_id = ctx.type_mapper.map_ast_type(&equip.type_.node);
                        let self_ptr_type = ctx.register_ptr_type(self_type_id);
                        param_types.push(self_ptr_type);
                        let self_is_mutable = method_def.params.first()
                            .map(|p| matches!(p.node.ownership, Ownership::MutableBorrow))
                            .unwrap_or(false);
                        abis.push(if self_is_mutable {
                            super::context::ParamABI::ByMutPtr
                        } else {
                            super::context::ParamABI::ByPtr
                        });
                    }
                    for p in &method_def.params {
                        if p.node.name.node == "self" { continue; }
                        let base = ctx.type_mapper.map_ast_type(&p.node.type_.node);
                        param_types.push(base);
                        abis.push(ctx.compute_param_abi(base, p.node.ownership));
                    }
                    let direct_name = format!("{type_name}__{method_name}");
                    ctx.fn_sigs.insert(direct_name.clone(), (param_types, ret_type));
                    ctx.fn_param_abis.insert(direct_name, abis);
                }
            }

            // Register sigs for trait default methods NOT overridden in the equip block
            for vtable_method in &vtable_info.methods {
                if implemented_methods.contains(&vtable_method.name) {
                    continue;
                }
                let mangled = format!(
                    "{trait_name}_for_{type_name}__{}",
                    vtable_method.name
                );
                ctx.fn_sigs.insert(
                    mangled.clone(),
                    (
                        vtable_method.base_param_types.clone(),
                        vtable_method.return_type,
                    ),
                );
                ctx.fn_param_abis.insert(mangled, vtable_abis(vtable_method));
            }

            // Register sigs for the trait's OWN (non-vtable) default methods
            // that reference `Self` and trait generic params. Mirrors the
            // unregistered-trait-equip path in
            // `register_unregistered_trait_equip_sigs` — needed here for
            // vtable-holding traits like `Iterator[T]` whose adapter
            // defaults (`Vector[T] collect(&self)` etc.) aren't in the
            // vtable. Without this, `CounterIter__collect` is absent from
            // fn_sigs at call-site lowering time and the call emits as
            // `void CounterIter__collect(...)` — return value dropped.
            if let Some(trait_def) = find_trait_def(ast_module, &trait_name) {
                // Build Self + trait-T subs against the impl.
                let mut self_subs: Vec<(String, ast::Type)> = vec![
                    ("Self".to_string(), equip.type_.node.clone()),
                ];
                let trait_args_for_sig: Vec<ast::Type> = equip.trait_.as_ref()
                    .and_then(|t| if let ast::Type::Named { generic_args, .. } = &t.trait_name.node {
                        Some(generic_args.iter().map(|a| a.node.clone()).collect())
                    } else { None })
                    .unwrap_or_default();
                if let Some(ref gp) = trait_def.generic_params {
                    for (param, concrete) in gp.node.params.iter().zip(trait_args_for_sig.iter()) {
                        let name = match &param.node {
                            ast::GenericParam::Type { name: n, .. } => n.node.clone(),
                            ast::GenericParam::Const { name: n, .. } => n.node.clone(),
                        };
                        self_subs.push((name, concrete.clone()));
                    }
                }
                for trait_item in &trait_def.items {
                    if let TraitItem::Method(dm) = &trait_item.node {
                        let method_name = &dm.name.node;
                        if implemented_methods.contains(method_name) {
                            continue;
                        }
                        // Skip methods already in the vtable (handled
                        // by the loop above).
                        if vtable_info.methods.iter().any(|m| m.name == *method_name) {
                            continue;
                        }
                        match &dm.body {
                            FunctionBody::Declaration | FunctionBody::Extern(_) => continue,
                            _ => {}
                        }
                        // Skip method-level-generic defaults — they go
                        // through per-call-site mono via
                        // register_method_instance_sigs.
                        if dm.generic_params.is_some() {
                            continue;
                        }
                        let substituted_ret = super::generics::substitute_type_pub(
                            &dm.return_type.node, &self_subs,
                        );
                        let ret_type = ctx.type_mapper.map_ast_type_mut(&substituted_ret, &mut ctx.type_registry);
                        let has_self = dm.params.first()
                            .map(|p| p.node.name.node == "self")
                            .unwrap_or(false);
                        let mut param_types = Vec::new();
                        let mut abis = Vec::new();
                        if has_self {
                            let self_type_id = ctx.type_mapper.map_ast_type(&equip.type_.node);
                            let self_is_mutable = dm.params.first()
                                .map(|p| matches!(p.node.ownership, Ownership::MutableBorrow))
                                .unwrap_or(false);
                            let self_ptr_type = if self_is_mutable {
                                ctx.register_mut_ptr_type(self_type_id)
                            } else {
                                ctx.register_ptr_type(self_type_id)
                            };
                            param_types.push(self_ptr_type);
                            abis.push(if self_is_mutable {
                                super::context::ParamABI::ByMutPtr
                            } else {
                                super::context::ParamABI::ByPtr
                            });
                        }
                        for p in &dm.params {
                            if p.node.name.node == "self" { continue; }
                            let subst_p = super::generics::substitute_type_pub(&p.node.type_.node, &self_subs);
                            let base = ctx.type_mapper.map_ast_type_mut(&subst_p, &mut ctx.type_registry);
                            param_types.push(base);
                            abis.push(ctx.compute_param_abi(base, p.node.ownership));
                        }
                        let direct_name = format!("{type_name}__{method_name}");
                        ctx.fn_sigs.insert(direct_name.clone(), (param_types, ret_type));
                        ctx.fn_param_abis.insert(direct_name, abis);
                    }
                }
            }

            // Register sigs for parent trait default methods as Type__method
            if let Some(trait_def) = find_trait_def(ast_module, &trait_name) {
                for parent in &trait_def.extends {
                    let parent_name = parent.node.name.node.clone();
                    if let Some(parent_def) = find_trait_def(ast_module, &parent_name) {
                        for trait_item in &parent_def.items {
                            if let TraitItem::Method(method_def) = &trait_item.node {
                                let method_name = &method_def.name.node;
                                if implemented_methods.contains(method_name) {
                                    continue;
                                }
                                match &method_def.body {
                                    FunctionBody::Declaration | FunctionBody::Extern(_) => continue,
                                    _ => {}
                                }
                                // Register as Type__method for direct dispatch
                                let direct_name = format!("{type_name}__{method_name}");
                                let ret_type = ctx.type_mapper.map_ast_type(&method_def.return_type.node);
                                let self_type_id = ctx.type_mapper.map_ast_type(&equip.type_.node);
                                let self_ptr_type = ctx.register_ptr_type(self_type_id);
                                let mut param_types = vec![self_ptr_type];
                                let mut abis = vec![super::context::ParamABI::ByPtr];
                                for p in &method_def.params {
                                    if p.node.name.node == "self" { continue; }
                                    let base = ctx.type_mapper.map_ast_type(&p.node.type_.node);
                                    param_types.push(base);
                                    abis.push(ctx.compute_param_abi(base, p.node.ownership));
                                }
                                ctx.fn_sigs.insert(direct_name.clone(), (param_types, ret_type));
                                ctx.fn_param_abis.insert(direct_name, abis);
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Lower trait equip blocks as functions with trait-mangled names.
///
/// Each method `draw(self)` in `equip Circle with Shape:` becomes a GIR function
/// `Shape_for_Circle__draw(void* self_void)` that casts self to `const Circle*`.
pub fn lower_trait_equip_methods(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    trait_info: &FxHashMap<String, TraitVTableInfo>,
    ast_module: &ast::Module,
) {
    for item in &ast_module.items {
        if let Item::Equip(equip) = &item.node {
            let trait_name = match &equip.trait_ {
                Some(t) => extract_trait_name(&t.trait_name.node),
                None => continue,
            };

            if trait_name.is_empty() || equip.generic_params.is_some() {
                continue;
            }

            let type_name = match extract_type_name(&equip.type_.node) {
                Some(n) => n,
                None => continue,
            };

            let vtable_info = match trait_info.get(&trait_name) {
                Some(info) => info,
                None => continue,
            };

            // Get the concrete self type for casting
            let concrete_type_id = ctx.type_mapper.map_ast_type(&equip.type_.node);
            let concrete_ptr_type = ctx.register_ptr_type(concrete_type_id);
            let concrete_mut_ptr_type =
                ctx.register_mut_ptr_type(concrete_type_id);

            // Track which methods are explicitly implemented in the equip block
            let mut implemented_methods: Vec<String> = Vec::new();

            for method in &equip.items {
                let method_def = &method.node;
                let method_name = &method_def.name.node;
                let mangled =
                    format!("{trait_name}_for_{type_name}__{method_name}");

                implemented_methods.push(method_name.clone());

                let vtable_method = match vtable_info
                    .methods
                    .iter()
                    .find(|m| m.name == *method_name)
                {
                    Some(m) => m,
                    None => {
                        // Not in vtable — inherited from parent trait.
                        // Lower as a regular Type__method equip method.
                        super::functions::lower_equip_method(
                            ctx,
                            module,
                            method_def,
                            &type_name,
                            &equip.type_.node,
                        );
                        continue;
                    }
                };

                lower_trait_method_body(
                    ctx, module, &mangled, method_def, vtable_method,
                    concrete_ptr_type, concrete_mut_ptr_type,
                );
            }

            // Emit implementations for methods NOT in the equip block.
            // If `via` delegation is active, forward through the field; otherwise
            // look for default implementations in the trait definition.
            if let Some(ref via_field) = equip.via_field {
                // Via delegation: forward un-overridden methods through the field
                let field_name = &via_field.node;
                // Look up the field type from struct_fields cache
                let field_info = ctx.lookup_field(&type_name, field_name);
                let field_type_name = field_info.and_then(|(_, type_id)| {
                    ctx.type_name_for_id(type_id).map(|s| s.to_string())
                });

                if let Some(field_type_name) = field_type_name {
                    for vtable_method in &vtable_info.methods {
                        if implemented_methods.contains(&vtable_method.name) {
                            continue;
                        }
                        emit_via_forwarding_function(
                            ctx, module, &trait_name, &type_name,
                            &field_type_name, field_name,
                            vtable_method, vtable_info,
                            concrete_ptr_type, concrete_mut_ptr_type,
                        );
                    }
                }
            } else if let Some(trait_def) = find_trait_def(ast_module, &trait_name) {
                // Extract concrete trait generic args from the equip's
                // trait annotation (`with Iterator[int]` → `[int]`).
                // Passed down so default-method bodies can substitute
                // the trait's own generic params (`T`) against
                // concrete types.
                let trait_args: Vec<ast::Type> = equip.trait_.as_ref()
                    .and_then(|t| if let ast::Type::Named { generic_args, .. } = &t.trait_name.node {
                        Some(generic_args.iter().map(|a| a.node.clone()).collect())
                    } else { None })
                    .unwrap_or_default();
                emit_default_methods_from_trait(
                    ctx, module, trait_def, &type_name, &equip.type_.node,
                    &trait_args, &implemented_methods, vtable_info,
                    concrete_ptr_type, concrete_mut_ptr_type,
                );
                // Also scan parent traits for default methods
                for parent in &trait_def.extends {
                    let parent_name = parent.node.name.node.clone();
                    if let Some(parent_def) = find_trait_def(ast_module, &parent_name) {
                        emit_default_methods_from_trait(
                            ctx, module, parent_def, &type_name, &equip.type_.node,
                            &trait_args, &implemented_methods, vtable_info,
                            concrete_ptr_type, concrete_mut_ptr_type,
                        );
                    }
                }
            }
        }
    }
}

/// Emit default method implementations from a trait definition for a concrete type.
/// Methods already in `implemented_methods` are skipped. If the method is in the vtable,
/// it's emitted with trait-mangled name. Otherwise it's emitted as Type__method.
fn emit_default_methods_from_trait(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    trait_def: &ast::TraitDef,
    type_name: &str,
    equipped_type: &ast::Type,
    trait_args: &[ast::Type],
    implemented_methods: &[String],
    vtable_info: &TraitVTableInfo,
    concrete_ptr_type: TypeId,
    concrete_mut_ptr_type: TypeId,
) {
    use crate::parser::ast::TraitItem;
    use super::generics;

    // Pre-bind Self → equipped_type + each trait generic param →
    // corresponding concrete trait arg for every default method we
    // emit here. Default bodies lifted onto generic traits (like the
    // `Iterator[T]` adapter constructors `TakeIter[Self, T] take(self,
    // int n)`) reference `Self` and the trait's own `T`; the
    // non-generic-impl emission paths below (`lower_equip_method` /
    // `lower_trait_method_body`) don't bind either themselves, so an
    // auto-loaded `std.iter` against a user-defined iterator like
    // `equip CounterIter with Iterator[int]` would emit
    // `TakeIter__unknown__T` without this.
    let mut self_subs: Vec<(String, ast::Type)> = vec![
        ("Self".to_string(), equipped_type.clone()),
    ];
    // Trait generic param → concrete trait arg. For `equip CounterIter
    // with Iterator[int]`, trait_def.generic_params = [T] and
    // `trait_args` (extracted from the `Iterator[int]` annotation on
    // the equip's trait line) supplies [int]. Pair by position.
    if let Some(ref gp) = trait_def.generic_params {
        for (param, concrete) in gp.node.params.iter().zip(trait_args.iter()) {
            let name = match &param.node {
                ast::GenericParam::Type { name: n, .. } => n.node.clone(),
                ast::GenericParam::Const { name: n, .. } => n.node.clone(),
            };
            self_subs.push((name, concrete.clone()));
        }
    }
    // Demand-gate: skip emission if the substituted return type
    // mentions a nominal that hasn't been registered as an instance
    // (mirrors the generic-template path in
    // `lower_generic_equip_methods_with_defaults`). Prevents dead-code
    // cascades of adapter methods for every `Iterator[T]` implementor.
    for trait_item in &trait_def.items {
        if let TraitItem::Method(default_method) = &trait_item.node {
            let method_name = &default_method.name.node;
            if implemented_methods.contains(method_name) {
                continue; // Already overridden
            }
            match &default_method.body {
                FunctionBody::Declaration | FunctionBody::Extern(_) => continue,
                FunctionBody::Block(_) | FunctionBody::Expression(_) => {}
            }
            // Skip defaults whose substituted return type mentions an
            // unregistered nominal (same check as the generic bulk path).
            let substituted_ret = generics::substitute_type_pub(
                &default_method.return_type.node, &self_subs,
            );
            if !super::functions::all_return_nominals_registered_pub(ctx, &substituted_ret) {
                continue;
            }
            // Pre-substitute the method body so `Self` references
            // resolve to the equipped type before lowering.
            let substituted_method = generics::substitute_function_body_pub(
                default_method, &self_subs,
            );
            // Bind `generic_type_params` (Self + trait T) for the body's
            // duration so static factory calls on type-variable receivers
            // — e.g. `T acc = T.default()` inside a `sum` default — resolve
            // through `lower_method_call`'s static path. Without this the
            // receiver `T` stays as `Identifier("T")`, falls out of the
            // primitive/named-type checks, and gets lowered as a value
            // expression that produces a bogus `int64_t__default(int64_t)`
            // sig collision with the prelude's no-arg static inline.
            // Snapshot + restore to avoid leaking state into siblings.
            let saved_gtp = ctx.generics.generic_type_params.clone();
            let saved_gpa = ctx.generics.generic_param_ast_types.clone();
            let saved_tns = ctx.generics.type_name_subs.clone();
            let saved_gfs = ctx.generics.generic_fragment_subs.clone();
            super::functions::build_generic_type_params(ctx, &self_subs);
            super::functions::build_type_name_subs(ctx, &self_subs);
            if let Some(vtable_method) = vtable_info.methods.iter().find(|m| m.name == *method_name) {
                let trait_name = &trait_def.name.node;
                let mangled = format!("{trait_name}_for_{type_name}__{method_name}");
                lower_trait_method_body(
                    ctx, module, &mangled, &substituted_method, vtable_method,
                    concrete_ptr_type, concrete_mut_ptr_type,
                );
            } else {
                // Not in vtable (inherited from parent) — emit as Type__method
                super::functions::lower_equip_method(
                    ctx, module, &substituted_method, type_name, equipped_type,
                );
            }
            ctx.generics.generic_type_params = saved_gtp;
            ctx.generics.generic_param_ast_types = saved_gpa;
            ctx.generics.type_name_subs = saved_tns;
            ctx.generics.generic_fragment_subs = saved_gfs;
        }
    }
}

/// Emit a forwarding function for a `via`-delegated trait method.
///
/// For `equip Outer with Showable via inner:`, un-overridden methods get:
/// ```c
/// Str Showable_for_Outer__show(const void* _1) {
///     const Outer* _2 = (const Outer*)_1;
///     const Inner* _3 = &_2->inner;
///     const void* _4 = (const void*)_3;
///     Str _5 = Showable_for_Inner__show(_4);
///     return _5;
/// }
/// ```
fn emit_via_forwarding_function(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    trait_name: &str,
    type_name: &str,
    field_type_name: &str,
    field_name: &str,
    vtable_method: &VTableMethod,
    _vtable_info: &TraitVTableInfo,
    concrete_ptr_type: TypeId,
    concrete_mut_ptr_type: TypeId,
) {
    let method_name = &vtable_method.name;
    let mangled = format!("{trait_name}_for_{type_name}__{method_name}");
    let target_fn = format!("{trait_name}_for_{field_type_name}__{method_name}");
    let return_type = vtable_method.return_type;

    // Build parameter list: same as the vtable method signature
    let params: Vec<(TypeId, Option<&str>)> = vtable_method.param_types.iter()
        .enumerate()
        .map(|(i, &ty)| {
            if i == 0 { (ty, Some("self_void")) }
            else { (ty, None) }
        })
        .collect();

    let mut builder = FunctionBuilder::new(mangled, return_type, &params);

    // _1 = self_void parameter (void*)
    // _2 = PtrCast<const TypeName*>(self_void)
    let cast_type = if vtable_method.self_is_mutable {
        concrete_mut_ptr_type
    } else {
        concrete_ptr_type
    };
    let self_cast = builder.ptr_cast(cast_type, FunctionBuilder::copy(LocalId(1)));

    // Look up field index and type (single lookup)
    let (field_idx, field_type_id) = ctx.lookup_field(type_name, field_name)
        .unwrap_or_else(|| {
            eprintln!("warning: vtable forwarding: field '{}' not found on type '{}', defaulting to field 0", field_name, type_name);
            (0, UNIT_TYPE)
        });
    let field_ptr_type = if vtable_method.self_is_mutable {
        ctx.register_mut_ptr_type(field_type_id)
    } else {
        ctx.register_ptr_type(field_type_id)
    };
    let field_place = Place {
        local: self_cast,
        projections: vec![Projection::Deref, Projection::Field(field_idx)],
    };
    let field_borrow_local = if vtable_method.self_is_mutable {
        builder.borrow_mut(field_place, field_ptr_type)
    } else {
        builder.borrow(field_place, field_ptr_type)
    };

    // _4 = PtrCast<void*>(field_ptr)  → cast back to void* for the target call
    let void_ptr_local = builder.ptr_cast(
        vtable_method.param_types[0], // same void* type as the vtable expects
        FunctionBuilder::copy(field_borrow_local),
    );

    // Build call arguments: void* field_ptr + any extra params forwarded
    let mut call_args = vec![FunctionBuilder::copy(void_ptr_local)];
    // Forward extra params (indices 2.. in the local list = params beyond self)
    for (i, &_param_type) in vtable_method.param_types.iter().enumerate().skip(1) {
        // Extra params start at LocalId(2) since _1 is self_void
        call_args.push(FunctionBuilder::copy(LocalId((i + 1) as u32)));
    }

    // _5 = Call target_fn(args)
    let result = if return_type == UNIT_TYPE {
        builder.call_void(target_fn, call_args);
        FunctionBuilder::const_unit()
    } else {
        let result_local = builder.call(target_fn, call_args, return_type);
        FunctionBuilder::copy(result_local)
    };

    builder.ret(result);
    module.functions.push(builder.build());
}

/// Find a trait definition by name in the AST module.
fn find_trait_def<'a>(ast_module: &'a ast::Module, name: &str) -> Option<&'a ast::TraitDef> {
    for item in &ast_module.items {
        if let Item::Trait(trait_def) = &item.node {
            if trait_def.name.node == name {
                return Some(trait_def);
            }
        }
    }
    None
}

/// Lower a single trait method body into a GIR function.
fn lower_trait_method_body(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    mangled: &str,
    method_def: &ast::FunctionDef,
    vtable_method: &VTableMethod,
    concrete_ptr_type: TypeId,
    concrete_mut_ptr_type: TypeId,
) {
    let return_type = vtable_method.return_type;

    // Build parameter list: void* self + other params
    // Use vtable's param types directly so the wrapper signature matches the vtable FnPtr.
    let mut params: Vec<(TypeId, Option<&str>)> =
        vec![(vtable_method.param_types[0], Some("self_void"))];
    let mut non_self_idx = 1; // vtable_method.param_types[0] is self
    for p in &method_def.params {
        if p.node.name.node == "self" {
            continue;
        }
        let vtable_type = vtable_method.param_types[non_self_idx];
        params.push((vtable_type, Some(p.node.name.node.as_str())));
        non_self_idx += 1;
    }

    let mut builder =
        FunctionBuilder::new(mangled.to_string(), return_type, &params);
    super::functions::begin_function_body(
        ctx,
        super::functions::FnBodyAst::from(&method_def.body),
    );

    // Register self_void as local _1
    ctx.register_local(
        "self_void",
        LocalId(1),
        vtable_method.param_types[0],
    );

    // Cast void* self to concrete type pointer
    let cast_type = if vtable_method.self_is_mutable {
        concrete_mut_ptr_type
    } else {
        concrete_ptr_type
    };
    let self_cast = builder
        .ptr_cast(cast_type, FunctionBuilder::copy(LocalId(1)));
    // Name the cast local `"self"` so `cow_materialize_alias` can rebind the
    // name-hint to the private copy after materialize. Without this, the
    // cast temp is anonymous: ownership is BareParam (materialize runs) but
    // the name map still points at the void* alias → write-through to caller
    // (D2 / plain-self trait equip). Mirrors equip-inherent LocalId(1) which
    // already carries the param name_hint from FunctionBuilder::new.
    builder.set_local_name(self_cast, "self");
    ctx.register_local("self", self_cast, cast_type);
    // Mark immutable (plain) self as BareParam so CoW materializes on mutation.
    // Mutable `&self`/`!self` keep write-through / consume semantics.
    if !vtable_method.self_is_mutable {
        ctx.set_bare_param(&mut builder, self_cast);
    }

    // Register other params using vtable types (must match wrapper signature)
    let mut param_idx = 2u32;
    let mut vt_idx = 1usize;
    for p in &method_def.params {
        if p.node.name.node == "self" {
            continue;
        }
        let vtable_type = vtable_method.param_types[vt_idx];
        let base_type = vtable_method.base_param_types[vt_idx];
        ctx.register_local(&p.node.name.node, LocalId(param_idx), vtable_type);
        // If this param is passed by pointer (base is resource type, vtable type is MutPtr),
        // mark as Borrowed { Param(self), Unique } so nested calls don't double-wrap.
        if vtable_type != base_type {
            ctx.set_param_borrow_unique(&mut builder, LocalId(param_idx));
            // `!` resource params: callee owns the pointee. Tag the local so the
            // LIR drop lowering uses the deref-aware path.
            if matches!(p.node.ownership, crate::parser::ast::Ownership::Move)
                && ctx.type_registry.is_resource_type(base_type)
            {
                ctx.set_owning_param(&mut builder, LocalId(param_idx));
            }
        }
        vt_idx += 1;
        param_idx += 1;
    }

    // Push Function drop scope and register params with drop elaborator
    ctx.drops.push_scope(DropScopeKind::Function);
    {
        let mut pidx = 2u32; // skip self_void at _1
        let mut vt_idx2 = 1usize;
        for p in &method_def.params {
            if p.node.name.node == "self" {
                continue;
            }
            let vtable_type = vtable_method.param_types[vt_idx2];
            let base_type = vtable_method.base_param_types[vt_idx2];
            ctx.drops.register_param(LocalId(pidx), vtable_type, &ctx.type_registry);
            if matches!(p.node.ownership, crate::parser::ast::Ownership::Move)
                && ctx.type_registry.is_resource_type(base_type)
            {
                ctx.drops.register_owning_param(LocalId(pidx), base_type, &ctx.type_registry);
            }
            vt_idx2 += 1;
            pidx += 1;
        }
    }

    // Track throws context for Result wrapping in return/throw statements
    ctx.func_state.current_throws_result_type = if method_def.throws.declares_throws() {
        Some(return_type)
    } else {
        None
    };

    // Lower the body
    match &method_def.body {
        FunctionBody::Block(block) => {
            lower_block(ctx, &mut builder, block);

            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                if return_type == UNIT_TYPE {
                    builder.ret(FunctionBuilder::const_unit());
                } else {
                    builder
                        .ret(FunctionBuilder::copy(LocalId(0)));
                }
            } else {
                ctx.drops.pop_scope_no_emit();
            }
        }
        FunctionBody::Expression(expr) => {
            let operand = lower_expr(ctx, &mut builder, expr);
            // Phase C: trait-impl expression bodies need the same Move-mode
            // staging as regular FunctionBody::Expression (mirrors
            // `assign_to_return_slot` in functions.rs).
            use crate::ir::instructions::AssignMode;
            let ret_mode = if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                let local_ty = builder.local_type(p.local);
                // Cluster 5 probe (2026-05-10): the disjunction
                // `needs_drop || is_resource_type` is NOT redundant.
                // See functions.rs:28 for the full reasoning. Disjunction
                // retained — load-bearing for trait-default iterator
                // adapter Move-mode staging (stdlib_iter_collect etc.).
                if p.projections.is_empty()
                    && (ctx.type_registry.needs_drop(local_ty)
                        || ctx.type_registry.is_resource_type(local_ty))
                {
                    AssignMode::Move
                } else {
                    AssignMode::Copy
                }
            } else {
                AssignMode::Copy
            };
            builder.assign_mode(ret_mode, Place::local(LocalId(0)), operand);
            ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            builder.ret(FunctionBuilder::copy(LocalId(0)));
        }
        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            ctx.drops.pop_scope_no_emit();
            return;
        }
    }

    module.functions.push(builder.build());
}

/// Generate vtable global constants for all trait equip blocks.
pub fn emit_vtable_globals(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    trait_info: &FxHashMap<String, TraitVTableInfo>,
    ast_module: &ast::Module,
) {
    for item in &ast_module.items {
        if let Item::Equip(equip) = &item.node {
            let trait_name = match &equip.trait_ {
                Some(t) => extract_trait_name(&t.trait_name.node),
                None => continue,
            };

            if trait_name.is_empty() || equip.generic_params.is_some() {
                continue;
            }

            let type_name = match extract_type_name(&equip.type_.node) {
                Some(n) => n,
                None => continue,
            };

            let vtable_info = match trait_info.get(&trait_name) {
                Some(info) => info,
                None => continue,
            };

            let vtable_global_name =
                format!("{trait_name}_for_{type_name}_vtable");

            // Build field initializers: each method slot -> FnRef to the impl function
            let mut fields: Vec<(String, GlobalInit)> = vtable_info
                .methods
                .iter()
                .map(|vtable_method| {
                    let impl_fn_name = format!(
                        "{trait_name}_for_{type_name}__{}",
                        vtable_method.name
                    );
                    (
                        vtable_method.name.clone(),
                        GlobalInit::FnRef(impl_fn_name),
                    )
                })
                .collect();

            // Drop-glue slot (appended LAST, mirroring the VTable TypeDef):
            // the concrete type's `Box__<Concrete>__drop` wrapper. Register
            // the `Box__<Concrete>` TypeDef here too, so the C backend's
            // typed `box_inner_type` discovery emits the wrapper (plus the
            // matching `__gorget_box_alloc/_free_<Concrete>` helpers) even
            // for concrete types the program never explicitly boxes — the
            // vtable global references the symbol unconditionally.
            if let Some(concrete_tid) = ctx.type_mapper.lookup_named(&type_name) {
                let box_mangled = format!("Box__{type_name}");
                if ctx.type_mapper.lookup_named(&box_mangled).is_none() {
                    let tid = ctx
                        .type_registry
                        .insert(GirType::Named(box_mangled.clone()));
                    ctx.type_mapper.register_named(box_mangled.clone(), tid);
                }
                super::exprs::type_reg::ensure_box_type_def(ctx, &box_mangled, concrete_tid);
                fields.push((
                    VTABLE_DROP_FIELD.to_string(),
                    GlobalInit::BoxDropRef(type_name.clone()),
                ));
            } else {
                // Concrete type not resolvable as a named boxable type (e.g.
                // `equip int with Trait` — no `Box__int` mangling exists, and
                // trait-object construction from a primitive is unsupported
                // today: baseline SEGFAULTS on primitive-equip dispatch, see
                // scout notes). Emit a NULL drop slot rather than referencing
                // a wrapper symbol that will never be emitted; the drop path
                // can't be reached for a trait object that can't be built.
                fields.push((VTABLE_DROP_FIELD.to_string(), GlobalInit::Zeroed));
            }

            module.globals.push(Global {
                name: vtable_global_name,
                type_id: vtable_info.vtable_type_id,
                init: GlobalInit::Struct {
                    type_name: vtable_info.vtable_type_name.clone(),
                    fields,
                },
            });
        }
    }
}

/// Register fn_sigs for trait equip blocks whose trait is not in `trait_info`.
///
/// These are built-in traits (From, Default, Equatable, Displayable, etc.) that
/// are not defined as `trait` items in the module AST. Uses fully mangled names
/// like `From__double_for_Celsius__from` or `Default_for_Point__default`.
pub fn register_unregistered_trait_equip_sigs(
    ctx: &mut LoweringContext,
    trait_info: &FxHashMap<String, TraitVTableInfo>,
    ast_module: &ast::Module,
) {
    for item in &ast_module.items {
        if let Item::Equip(equip) = &item.node {
            let trait_name = match &equip.trait_ {
                Some(t) => extract_trait_name(&t.trait_name.node),
                None => continue,
            };
            if trait_name.is_empty() || equip.generic_params.is_some() {
                continue;
            }
            // Only handle traits NOT already in trait_info
            if trait_info.contains_key(&trait_name) {
                continue;
            }
            let type_name = match extract_type_name(&equip.type_.node) {
                Some(n) => n,
                None => continue,
            };

            let trait_type = &equip.trait_.as_ref().unwrap().trait_name.node;

            let mut registered_methods: Vec<String> = Vec::new();

            for method in &equip.items {
                registered_methods.push(method.node.name.node.clone());

                let method_def = &method.node;

                let ret_type =
                    ctx.type_mapper.map_ast_type(&method_def.return_type.node);

                let has_self = method_def
                    .params
                    .first()
                    .map(|p| p.node.name.node == "self")
                    .unwrap_or(false);

                let mut param_types = Vec::new();
                let mut abis = Vec::new();
                if has_self {
                    let self_type_id =
                        ctx.type_mapper.map_ast_type(&equip.type_.node);
                    let self_is_mutable = method_def
                        .params
                        .first()
                        .map(|p| {
                            p.node.name.node == "self"
                                && matches!(
                                    p.node.ownership,
                                    Ownership::MutableBorrow
                                )
                        })
                        .unwrap_or(false);
                    let self_ptr_type = if self_is_mutable {
                        ctx.register_mut_ptr_type(self_type_id)
                    } else {
                        ctx.register_ptr_type(self_type_id)
                    };
                    param_types.push(self_ptr_type);
                    abis.push(if self_is_mutable {
                        super::context::ParamABI::ByMutPtr
                    } else {
                        super::context::ParamABI::ByPtr
                    });
                }
                for p in &method_def.params {
                    if p.node.name.node == "self" {
                        continue;
                    }
                    let base = ctx.type_mapper.map_ast_type(&p.node.type_.node);
                    param_types.push(base);
                    abis.push(ctx.compute_param_abi(base, p.node.ownership));
                }

                if has_self {
                    // Instance methods: use Type__method name (same as regular equip)
                    let mangled = format!(
                        "{type_name}__{}",
                        method_def.name.node
                    );
                    ctx.fn_sigs
                        .insert(mangled.clone(), (param_types, ret_type));
                    ctx.fn_param_abis.insert(mangled, abis);
                } else {
                    // Static methods: use Trait_for_Type__method to avoid conflicts
                    let mangled = mangle_trait_equip_name(
                        trait_type,
                        &type_name,
                        &method_def.name.node,
                        ctx,
                    );
                    ctx.fn_sigs
                        .insert(mangled.clone(), (param_types, ret_type));
                    ctx.fn_param_abis.insert(mangled, abis);
                }
            }

            // Also register signatures for default methods from the trait definition.
            // Substitute Self → equipped type AND each trait generic param
            // → the impl's corresponding concrete trait arg. Required for
            // defaults like `Vector[T] collect(&self)` on `Iterator[T]`
            // where a user impl `equip CounterIter with Iterator[int]`
            // needs the trait's `T` resolved to `int` before sig
            // registration; without the trait-T binding the return type
            // mangles to `Vector[unknown]` and the call site drops the
            // return value at IR lowering.
            let mut self_subs_sig: Vec<(String, ast::Type)> = vec![
                ("Self".to_string(), equip.type_.node.clone()),
            ];
            let trait_args_for_sig: Vec<ast::Type> = if let ast::Type::Named { generic_args, .. } = trait_type {
                generic_args.iter().map(|a| a.node.clone()).collect()
            } else {
                Vec::new()
            };
            if let Some(td) = find_trait_def(ast_module, &trait_name) {
                if let Some(ref gp) = td.generic_params {
                    for (param, concrete) in gp.node.params.iter().zip(trait_args_for_sig.iter()) {
                        let name = match &param.node {
                            ast::GenericParam::Type { name: n, .. } => n.node.clone(),
                            ast::GenericParam::Const { name: n, .. } => n.node.clone(),
                        };
                        self_subs_sig.push((name, concrete.clone()));
                    }
                }
            }
            if let Some(trait_def) = find_trait_def(ast_module, &trait_name) {
                for trait_item in &trait_def.items {
                    if let TraitItem::Method(default_method) = &trait_item.node {
                        let method_name = &default_method.name.node;
                        if registered_methods.contains(method_name) {
                            continue;
                        }
                        match &default_method.body {
                            FunctionBody::Declaration | FunctionBody::Extern(_) => continue,
                            FunctionBody::Block(_) | FunctionBody::Expression(_) => {}
                        }
                        let substituted_ret = generics::substitute_type_pub(
                            &default_method.return_type.node, &self_subs_sig,
                        );
                        let ret_type = ctx.type_mapper.map_ast_type_mut(&substituted_ret, &mut ctx.type_registry);
                        let has_self = default_method.params.first()
                            .map(|p| p.node.name.node == "self")
                            .unwrap_or(false);
                        let mut param_types = Vec::new();
                        let mut abis = Vec::new();
                        if has_self {
                            let self_type_id = ctx.type_mapper.map_ast_type(&equip.type_.node);
                            let self_is_mutable = default_method.params.first()
                                .map(|p| {
                                    p.node.name.node == "self"
                                        && matches!(p.node.ownership, Ownership::MutableBorrow)
                                })
                                .unwrap_or(false);
                            let self_ptr_type = if self_is_mutable {
                                ctx.register_mut_ptr_type(self_type_id)
                            } else {
                                ctx.register_ptr_type(self_type_id)
                            };
                            param_types.push(self_ptr_type);
                            abis.push(if self_is_mutable {
                                super::context::ParamABI::ByMutPtr
                            } else {
                                super::context::ParamABI::ByPtr
                            });
                        }
                        for p in &default_method.params {
                            if p.node.name.node == "self" { continue; }
                            let subst_p = generics::substitute_type_pub(&p.node.type_.node, &self_subs_sig);
                            let base = ctx.type_mapper.map_ast_type_mut(&subst_p, &mut ctx.type_registry);
                            param_types.push(base);
                            abis.push(ctx.compute_param_abi(base, p.node.ownership));
                        }
                        if has_self {
                            let mangled = format!("{type_name}__{method_name}");
                            ctx.fn_sigs.insert(mangled.clone(), (param_types, ret_type));
                            ctx.fn_param_abis.insert(mangled, abis);
                        } else {
                            let mangled = mangle_trait_equip_name(trait_type, &type_name, method_name, ctx);
                            ctx.fn_sigs.insert(mangled.clone(), (param_types, ret_type));
                            ctx.fn_param_abis.insert(mangled, abis);
                        }
                    }
                }
            }
        }
    }
}

/// Lower trait equip blocks whose trait is not in `trait_info`.
///
/// Uses fully-mangled names (e.g. `From__double_for_Celsius__from`) to avoid
/// conflicts when a type has multiple trait impls with the same method name.
pub fn lower_unregistered_trait_equip_methods(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    trait_info: &FxHashMap<String, TraitVTableInfo>,
    ast_module: &ast::Module,
) {
    for item in &ast_module.items {
        if let Item::Equip(equip) = &item.node {
            let trait_name = match &equip.trait_ {
                Some(t) => extract_trait_name(&t.trait_name.node),
                None => continue,
            };
            if trait_name.is_empty() || equip.generic_params.is_some() {
                continue;
            }
            if trait_info.contains_key(&trait_name) {
                continue;
            }
            let type_name = match extract_type_name(&equip.type_.node) {
                Some(n) => n,
                None => continue,
            };

            let trait_type =
                &equip.trait_.as_ref().unwrap().trait_name.node;

            let mut implemented_methods: Vec<String> = Vec::new();

            for method in &equip.items {
                implemented_methods.push(method.node.name.node.clone());

                let has_self = method
                    .node
                    .params
                    .first()
                    .map(|p| p.node.name.node == "self")
                    .unwrap_or(false);

                if has_self {
                    // Instance method — use Type__method name (same as regular equip)
                    super::functions::lower_equip_method(
                        ctx,
                        module,
                        &method.node,
                        &type_name,
                        &equip.type_.node,
                    );
                } else {
                    // Static method — use trait-mangled name for overload safety
                    let mangled = mangle_trait_equip_name(
                        trait_type,
                        &type_name,
                        &method.node.name.node,
                        ctx,
                    );
                    lower_static_trait_method(
                        ctx, module, &method.node, &mangled,
                    );
                }
            }

            // Emit default methods from the trait definition that are not overridden.
            if let Some(trait_def) = find_trait_def(ast_module, &trait_name) {
                // Build Self + trait-T subs. For `equip CounterIter
                // with Iterator[int]`, self_subs = [Self → CounterIter,
                // T → int]. Trait-T binding is needed once auto-loaded
                // `std.iter` starts emitting `Iterator[T]` default
                // adapter methods (`TakeIter[Self, T] take(...)` etc.)
                // for every user-defined iterator.
                let mut self_subs: Vec<(String, ast::Type)> = vec![
                    ("Self".to_string(), equip.type_.node.clone()),
                ];
                let trait_args: Vec<ast::Type> = if let ast::Type::Named { generic_args, .. } = trait_type {
                    generic_args.iter().map(|a| a.node.clone()).collect()
                } else {
                    Vec::new()
                };
                if let Some(ref gp) = trait_def.generic_params {
                    for (param, concrete) in gp.node.params.iter().zip(trait_args.iter()) {
                        let name = match &param.node {
                            ast::GenericParam::Type { name: n, .. } => n.node.clone(),
                            ast::GenericParam::Const { name: n, .. } => n.node.clone(),
                        };
                        self_subs.push((name, concrete.clone()));
                    }
                }
                for trait_item in &trait_def.items {
                    if let TraitItem::Method(default_method) = &trait_item.node {
                        let method_name = &default_method.name.node;
                        if implemented_methods.contains(method_name) {
                            continue;
                        }
                        match &default_method.body {
                            FunctionBody::Declaration | FunctionBody::Extern(_) => continue,
                            FunctionBody::Block(_) | FunctionBody::Expression(_) => {}
                        }
                        // Demand-gate: skip emitting a default whose
                        // substituted return type mentions an unregistered
                        // generic nominal. Prevents every Iterator
                        // implementor from speculatively emitting every
                        // adapter (`TakeIter[CounterIter, int]` etc. when
                        // no call site uses them).
                        let substituted_ret = super::generics::substitute_type_pub(
                            &default_method.return_type.node, &self_subs,
                        );
                        if !super::functions::all_return_nominals_registered_pub(ctx, &substituted_ret) {
                            continue;
                        }
                        // Pre-substitute Self + trait-T in the body so
                        // the struct-constructor type-arg lists resolve
                        // to concrete types before mangling.
                        let mut substituted_method = super::generics::substitute_function_body_pub(
                            default_method, &self_subs,
                        );
                        // Also evaluate delayed meta (e.g.
                        // `meta if Self is Enum:` /
                        // `meta for vname in variant_names(Self):`)
                        // against the bound Self. Without this,
                        // `trait_default_meta.gg`-style traits that
                        // unroll per-variant logic via meta loops
                        // emit empty bodies.
                        if let ast::FunctionBody::Block(ref mut block) = substituted_method.body {
                            let empty_env = rustc_hash::FxHashMap::default();
                            let delayed_ctx = DelayedMetaContext {
                                type_subs:      &self_subs,
                                features:       &[],
                                meta_env:       &empty_env,
                                items:          &[],
                                trait_registry: &ctx.analysis.traits,
                                type_registry:  &ctx.type_registry,
                            };
                            let mut meta_errors = Vec::new();
                            meta::evaluate_delayed_meta_block(block, &delayed_ctx, &mut meta_errors);
                            for e in &meta_errors {
                                eprintln!("[delayed-meta static-trait] {e:?}");
                            }
                        }
                        let has_self = substituted_method.params.first()
                            .map(|p| p.node.name.node == "self")
                            .unwrap_or(false);
                        // Bind `generic_type_params` (Self + trait T) for the
                        // body's duration so static factory calls on type-
                        // variable receivers — e.g. `T acc = T.default()`
                        // inside a `sum` default — resolve through
                        // `lower_method_call`'s static path. Without this the
                        // receiver `T` stays as `Identifier("T")`, falls out
                        // of every check, and gets lowered as the `0`
                        // constant placeholder, producing a bogus
                        // `int64_t__default(int64_t)` call that conflicts
                        // with the prelude's no-arg static inline. Snapshot
                        // + restore to avoid leaking state into siblings.
                        let saved_gtp = ctx.generics.generic_type_params.clone();
                        let saved_gpa = ctx.generics.generic_param_ast_types.clone();
                        let saved_tns = ctx.generics.type_name_subs.clone();
                        let saved_gfs = ctx.generics.generic_fragment_subs.clone();
                        super::functions::build_generic_type_params(ctx, &self_subs);
                        super::functions::build_type_name_subs(ctx, &self_subs);
                        if has_self {
                            super::functions::lower_equip_method(
                                ctx, module, &substituted_method, &type_name, &equip.type_.node,
                            );
                        } else {
                            let mangled = mangle_trait_equip_name(
                                trait_type, &type_name, method_name, ctx,
                            );
                            lower_static_trait_method(ctx, module, &substituted_method, &mangled);
                        }
                        ctx.generics.generic_type_params = saved_gtp;
                        ctx.generics.generic_param_ast_types = saved_gpa;
                        ctx.generics.type_name_subs = saved_tns;
                        ctx.generics.generic_fragment_subs = saved_gfs;
                    }
                }
            }
        }
    }
}

/// Lower a static trait method (no `self` parameter) into a GIR function.
///
/// Used for `From::from`, `Default::default`, and similar static trait methods.
fn lower_static_trait_method(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    method: &ast::FunctionDef,
    mangled: &str,
) {
    // Use map_ast_type_mut to auto-register generic instantiations like Option[Color].
    let return_type = ctx.type_mapper.map_ast_type_mut(&method.return_type.node, &mut ctx.type_registry);

    // Build parameters (skip self if somehow present).
    // Apply resolve_param_type to wrap Move-type params in Ptr/MutPtr based on ownership.
    let params: Vec<(TypeId, Option<String>)> = method
        .params
        .iter()
        .filter(|p| p.node.name.node != "self")
        .map(|p| {
            let base_type = ctx.type_mapper.map_ast_type_mut(&p.node.type_.node, &mut ctx.type_registry);
            let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
            (gir_type, Some(p.node.name.node.clone()))
        })
        .collect();
    let param_refs: Vec<(TypeId, Option<&str>)> = params.iter()
        .map(|(id, name)| (*id, name.as_deref()))
        .collect();

    let mut builder = FunctionBuilder::new(mangled, return_type, &param_refs);
    super::functions::begin_function_body(ctx, super::functions::FnBodyAst::from(&method.body));

    // Register params starting at _1, tracking pointer-wrapped params to avoid double-wrapping
    let mut param_idx = 1u32;
    for (i, p) in method.params.iter().filter(|p| p.node.name.node != "self").enumerate() {
        let (gir_type, _) = &params[i];
        ctx.register_local(&p.node.name.node, LocalId(param_idx), *gir_type);
        let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
        if ctx.is_ref_param(base_type, p.node.ownership) {
            ctx.set_bare_param(&mut builder, LocalId(param_idx));
        } else if ctx.is_mut_ref_param(base_type, p.node.ownership) {
            // & or ! MutPtr param. Per §6.2: typed shape Borrowed { Param(self), Unique }.
            ctx.set_param_borrow_unique(&mut builder, LocalId(param_idx));
            if matches!(p.node.ownership, crate::parser::ast::Ownership::Move)
                && ctx.type_registry.is_resource_type(base_type)
            {
                ctx.set_owning_param(&mut builder, LocalId(param_idx));
            }
        }
        param_idx += 1;
    }

    ctx.drops.push_scope(DropScopeKind::Function);

    // Register `!` resource params for owning-param drop at function exit.
    // The drop accountant emits `DropIfAlive { *local }` so the LIR drop-flag
    // dataflow controls whether the drop fires (suppressed if the body
    // transferred ownership onward).
    {
        let mut pidx = 1u32;
        for p in method.params.iter().filter(|p| p.node.name.node != "self") {
            let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
            if matches!(p.node.ownership, crate::parser::ast::Ownership::Move)
                && ctx.type_registry.is_resource_type(base_type)
            {
                ctx.drops.register_owning_param(LocalId(pidx), base_type, &ctx.type_registry);
            }
            pidx += 1;
        }
    }

    match &method.body {
        FunctionBody::Block(block) => {
            lower_block(ctx, &mut builder, block);

            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                if return_type == UNIT_TYPE {
                    builder.ret(FunctionBuilder::const_unit());
                } else {
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                }
            } else {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            }
        }
        FunctionBody::Expression(expr) => {
            let operand = lower_expr(ctx, &mut builder, expr);
            // Phase C: trait-impl expression bodies need the same Move-mode
            // staging as regular FunctionBody::Expression (mirrors
            // `assign_to_return_slot` in functions.rs).
            use crate::ir::instructions::AssignMode;
            let ret_mode = if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                let local_ty = builder.local_type(p.local);
                // Cluster 5 probe (2026-05-10): the disjunction
                // `needs_drop || is_resource_type` is NOT redundant.
                // See functions.rs:28 for the full reasoning. Disjunction
                // retained — load-bearing for trait-default iterator
                // adapter Move-mode staging (stdlib_iter_collect etc.).
                if p.projections.is_empty()
                    && (ctx.type_registry.needs_drop(local_ty)
                        || ctx.type_registry.is_resource_type(local_ty))
                {
                    AssignMode::Move
                } else {
                    AssignMode::Copy
                }
            } else {
                AssignMode::Copy
            };
            builder.assign_mode(ret_mode, Place::local(LocalId(0)), operand);
            ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            builder.ret(FunctionBuilder::copy(LocalId(0)));
        }
        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            return;
        }
    }

    module.functions.push(builder.build());
}

/// Extract a trait name from an AST Type.
pub fn extract_trait_name(ty: &Type) -> String {
    match ty {
        Type::Named { name, .. } => name.node.clone(),
        _ => String::new(),
    }
}

/// Extract a non-generic type name from an AST Type.
fn extract_type_name(ty: &Type) -> Option<String> {
    // Delegates to the canonical equip_target_name helper so
    // `equip String with Foo:` and `equip int with Foo:` produce the
    // same `GorgetString__method` / `int64_t__method` names the
    // call-site resolver looks for.
    super::types::equip_target_name(ty)
}

/// Build a fully-mangled trait equip method name.
///
/// Examples:
/// - `From[float]` on `Celsius`, method `from` → `From__double_for_Celsius__from`
/// - `Default` on `Point`, method `default` → `Default_for_Point__default`
/// - `Equatable` on `Circle`, method `eq` → `Equatable_for_Circle__eq`
/// Clone a `FunctionDef` and substitute `Self` (and other subs) in:
/// - return type
/// - parameter types
/// - body block (via delayed meta evaluation)
///
/// Used to instantiate default trait methods with a concrete equipped type.
/// Previously called from `lower_unregistered_trait_equip_methods`; the
/// new callers use `generics::substitute_function_body_pub` which also
/// walks expression types in the body. Kept here for the delayed-meta
/// evaluation it runs, in case a future caller needs it.
#[allow(dead_code)]
fn substitute_method_self(
    method: &ast::FunctionDef,
    self_subs: &[(String, Type)],
    ctx: &LoweringContext,
) -> ast::FunctionDef {
    let mut cloned = method.clone();

    // Substitute Self in return type.
    cloned.return_type.node = generics::substitute_type_pub(&method.return_type.node, self_subs);

    // Substitute Self in parameter types.
    for param in &mut cloned.params {
        param.node.type_.node = generics::substitute_type_pub(&param.node.type_.node, self_subs);
    }

    // Evaluate delayed meta in the body block.
    if let ast::FunctionBody::Block(ref mut block) = cloned.body {
        let empty_env = rustc_hash::FxHashMap::default();
        let delayed_ctx = DelayedMetaContext {
            type_subs:      self_subs,
            features:       &[],
            meta_env:       &empty_env,
            items:          &[],
            trait_registry: &ctx.analysis.traits,
            type_registry:  &ctx.type_registry,
        };
        let mut meta_errors = Vec::new();
        meta::evaluate_delayed_meta_block(block, &delayed_ctx, &mut meta_errors);
        for e in &meta_errors {
            eprintln!("[delayed-meta static-trait] {e:?}");
        }
    }

    cloned
}

fn mangle_trait_equip_name(
    trait_type: &Type,
    type_name: &str,
    method_name: &str,
    ctx: &LoweringContext,
) -> String {
    let trait_name = extract_trait_name(trait_type);

    let generic_suffix = match trait_type {
        Type::Named { generic_args, .. } if !generic_args.is_empty() => {
            let args: Vec<String> = generic_args
                .iter()
                .map(|a| mangle_c_type_name(&a.node, ctx))
                .collect();
            format!("__{}", args.join("__"))
        }
        _ => String::new(),
    };

    format!("{trait_name}{generic_suffix}_for_{type_name}__{method_name}")
}

/// Convert an AST type to a C-compatible name for mangling.
fn mangle_c_type_name(ty: &Type, _ctx: &LoweringContext) -> String {
    use ast::PrimitiveType;
    match ty {
        Type::Primitive(p) => match p {
            PrimitiveType::Int | PrimitiveType::Int64 => "int64_t".into(),
            PrimitiveType::Int8 => "int8_t".into(),
            PrimitiveType::Int16 => "int16_t".into(),
            PrimitiveType::Int32 => "int32_t".into(),
            PrimitiveType::Uint => "uint64_t".into(),
            PrimitiveType::Uint8 => "uint8_t".into(),
            PrimitiveType::Uint16 => "uint16_t".into(),
            PrimitiveType::Uint32 => "uint32_t".into(),
            PrimitiveType::Uint64 => "uint64_t".into(),
            PrimitiveType::Float | PrimitiveType::Float64 => "double".into(),
            PrimitiveType::Float32 => "float".into(),
            PrimitiveType::Bool => "bool".into(),
            PrimitiveType::CStr => "const_char_ptr".into(),
            PrimitiveType::StringType => "GorgetString".into(),
            PrimitiveType::Void => "void".into(),
        },
        Type::Named { name, .. } => name.node.clone(),
        _ => "void".into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::lowering::types::TypeMapper;

    #[test]
    fn extract_names() {
        use crate::span::Spanned;

        let ty = Type::Named {
            name: Spanned::dummy("Shape".to_string()),
            generic_args: vec![],
        };
        assert_eq!(extract_trait_name(&ty), "Shape");
        assert_eq!(extract_type_name(&ty), Some("Shape".to_string()));

        let generic_ty = Type::Named {
            name: Spanned::dummy("Vector".to_string()),
            generic_args: vec![Spanned::dummy(Type::Primitive(
                crate::parser::ast::PrimitiveType::Int,
            ))],
        };
        assert_eq!(extract_type_name(&generic_ty), None);
    }

    #[test]
    fn register_trait_vtable_type() {
        let source = "trait Shape:\n    float area(self)\n    void draw(self)\n\nvoid main():\n    pass\n";
        let mut parser = crate::parser::Parser::new(source);
        let ast_module = parser.parse_module();
        assert!(
            parser.errors.is_empty(),
            "Parse errors: {:?}",
            parser.errors
        );

        let analysis =
            Box::leak(Box::new(crate::ir::lowering::empty_analysis_for_test()));
        let mut reg = TypeRegistry::new();
        let mapper = TypeMapper::new(&mut reg);
        let mut ctx =
            crate::ir::lowering::context::LoweringContext::new(analysis, mapper, reg);

        let info = register_trait_types(&mut ctx, &ast_module);
        assert!(info.contains_key("Shape"), "Should have Shape trait info");

        let shape = &info["Shape"];
        assert_eq!(shape.vtable_type_name, "Shape_VTable");
        assert_eq!(shape.trait_obj_type_name, "Shape_TraitObj");
        assert_eq!(shape.methods.len(), 2);
        assert_eq!(shape.methods[0].name, "area");
        assert_eq!(shape.methods[0].return_type, F64_TYPE);
        assert_eq!(shape.methods[1].name, "draw");
        assert_eq!(shape.methods[1].return_type, UNIT_TYPE);

        // Verify TypeDefs were created
        assert!(
            ctx.type_registry.has_type_def("Shape_VTable"),
            "Should have Shape_VTable TypeDef"
        );
        assert!(
            ctx.type_registry.has_type_def("Shape_TraitObj"),
            "Should have Shape_TraitObj TypeDef"
        );

        // Verify VTable struct fields (2 method slots + trailing __drop glue slot)
        let vtable_def = ctx.type_registry.get_type_def("Shape_VTable").unwrap();
        if let TypeDefKind::Struct(ref s) = vtable_def.kind {
            assert_eq!(s.fields.len(), 3);
            assert_eq!(s.fields[0].name, "area");
            assert_eq!(s.fields[1].name, "draw");
            assert_eq!(s.fields[2].name, VTABLE_DROP_FIELD);
        } else {
            panic!("Expected Struct TypeDef for Shape_VTable");
        }

        // Verify TraitObj struct fields
        let obj_def = ctx
            .type_registry
            .get_type_def("Shape_TraitObj")
            .unwrap();
        if let TypeDefKind::Struct(ref s) = obj_def.kind {
            assert_eq!(s.fields.len(), 2);
            assert_eq!(s.fields[0].name, "data");
            assert_eq!(s.fields[1].name, "vtable");
        } else {
            panic!("Expected Struct TypeDef for Shape_TraitObj");
        }
    }
}
