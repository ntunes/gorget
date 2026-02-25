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

use super::context::LoweringContext;
use super::exprs::lower_expr;
use super::stmts::lower_block;

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
    /// Full parameter list including void* self as first param.
    pub param_types: Vec<TypeId>,
    /// Method return type.
    pub return_type: TypeId,
    /// Whether self is `&self` (mutable borrow).
    pub self_is_mutable: bool,
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

            // Collect method slots from the trait definition
            let mut methods = Vec::new();
            for trait_item in &trait_def.items {
                if let TraitItem::Method(method_def) = &trait_item.node {
                    let method_name = &method_def.name.node;
                    let return_type =
                        ctx.type_mapper.map_ast_type(&method_def.return_type.node);

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
                    let mut param_types = vec![self_type];
                    for p in &method_def.params {
                        if p.node.name.node == "self" {
                            continue;
                        }
                        param_types
                            .push(ctx.type_mapper.map_ast_type(&p.node.type_.node));
                    }

                    // Create function pointer type in the registry
                    let fn_ptr_type_id = ctx.type_registry.insert(GirType::FnPtr {
                        params: param_types.clone(),
                        return_type,
                    });

                    methods.push(VTableMethod {
                        name: method_name.clone(),
                        fn_ptr_type_id,
                        param_types,
                        return_type,
                        self_is_mutable,
                    });
                }
            }

            if methods.is_empty() {
                continue;
            }

            // -- Create VTable struct TypeDef --
            let vtable_type_name = format!("{trait_name}_VTable");
            let vtable_fields: Vec<StructField> = methods
                .iter()
                .map(|m| StructField {
                    name: m.name.clone(),
                    type_id: m.fn_ptr_type_id,
                })
                .collect();

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

            for method in &equip.items {
                let method_name = &method.node.name.node;
                let mangled =
                    format!("{trait_name}_for_{type_name}__{method_name}");

                if let Some(vtable_method) = vtable_info
                    .methods
                    .iter()
                    .find(|m| m.name == *method_name)
                {
                    ctx.fn_sigs.insert(
                        mangled,
                        (
                            vtable_method.param_types.clone(),
                            vtable_method.return_type,
                        ),
                    );
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

            for method in &equip.items {
                let method_def = &method.node;
                let method_name = &method_def.name.node;
                let mangled =
                    format!("{trait_name}_for_{type_name}__{method_name}");

                let vtable_method = match vtable_info
                    .methods
                    .iter()
                    .find(|m| m.name == *method_name)
                {
                    Some(m) => m,
                    None => continue,
                };

                let return_type = vtable_method.return_type;

                // Build parameter list: void* self + other params
                let mut params: Vec<(TypeId, Option<&str>)> =
                    vec![(vtable_method.param_types[0], Some("self_void"))];
                for p in &method_def.params {
                    if p.node.name.node == "self" {
                        continue;
                    }
                    let gir_type =
                        ctx.type_mapper.map_ast_type(&p.node.type_.node);
                    params.push((gir_type, Some(p.node.name.node.as_str())));
                }

                let mut builder =
                    FunctionBuilder::new(mangled, return_type, &params);
                ctx.clear_locals();

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
                ctx.register_local("self", self_cast, cast_type);

                // Register other params
                let mut param_idx = 2u32;
                for p in &method_def.params {
                    if p.node.name.node == "self" {
                        continue;
                    }
                    let gir_type =
                        ctx.type_mapper.map_ast_type(&p.node.type_.node);
                    ctx.register_local(
                        &p.node.name.node,
                        LocalId(param_idx),
                        gir_type,
                    );
                    param_idx += 1;
                }

                // Lower the body
                match &method_def.body {
                    FunctionBody::Block(block) => {
                        lower_block(ctx, &mut builder, block);

                        let last_block_idx = builder.current_block.0 as usize;
                        if builder.blocks[last_block_idx].terminator.is_none() {
                            if return_type == UNIT_TYPE {
                                builder.ret(FunctionBuilder::const_unit());
                            } else {
                                builder
                                    .ret(FunctionBuilder::copy(LocalId(0)));
                            }
                        }
                    }
                    FunctionBody::Expression(expr) => {
                        let operand = lower_expr(ctx, &mut builder, expr);
                        builder.assign(Place::local(LocalId(0)), operand);
                        builder.ret(FunctionBuilder::copy(LocalId(0)));
                    }
                    FunctionBody::Declaration | FunctionBody::Extern(_) => {
                        continue;
                    }
                }

                module.functions.push(builder.build());
            }
        }
    }
}

/// Generate vtable global constants for all trait equip blocks.
pub fn emit_vtable_globals(
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
            let fields: Vec<(String, GlobalInit)> = vtable_info
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

/// Extract a trait name from an AST Type.
fn extract_trait_name(ty: &Type) -> String {
    match ty {
        Type::Named { name, .. } => name.node.clone(),
        _ => String::new(),
    }
}

/// Extract a non-generic type name from an AST Type.
fn extract_type_name(ty: &Type) -> Option<String> {
    match ty {
        Type::Named {
            name, generic_args, ..
        } => {
            if generic_args.is_empty() {
                Some(name.node.clone())
            } else {
                None // Generic types handled separately
            }
        }
        _ => None,
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

        // Verify VTable struct fields
        let vtable_def = ctx.type_registry.get_type_def("Shape_VTable").unwrap();
        if let TypeDefKind::Struct(ref s) = vtable_def.kind {
            assert_eq!(s.fields.len(), 2);
            assert_eq!(s.fields[0].name, "area");
            assert_eq!(s.fields[1].name, "draw");
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
