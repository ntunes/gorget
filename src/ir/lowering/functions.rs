use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{self, FunctionBody, FunctionDef, GenericParam, Ownership, Type};
use crate::semantic::meta::{self as meta, DelayedMetaContext, MetaValue};
use crate::span::Spanned;

use super::context::LoweringContext;
use super::drops::DropScopeKind;
use super::exprs::lower_expr;
use super::generics;
use super::stmts::lower_block;

/// Lower a single function definition into the GIR module.
///
/// `name_override` — when `Some`, use this as the GIR/C function name instead of
/// `func.name.node`.  Used by module-scoped name mangling (Phase 5) so that functions
/// from non-entry modules get their module-path prefix in the emitted C symbol while
/// the rest of the lowering logic (body, params, drops) remains unchanged.
pub fn lower_function(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    func: &FunctionDef,
    name_override: Option<&str>,
) {
    let func_span = func.span;
    let name: &str = name_override.unwrap_or(func.name.node.as_str());
    let is_main = name == "main";

    // Map return type — use fn_sigs if available (handles `throws` → Result)
    let return_type = if is_main {
        I32_TYPE
    } else if let Some((_, ret_ty)) = ctx.fn_sigs.get(name) {
        *ret_ty
    } else {
        ctx.type_mapper.map_ast_type(&func.return_type.node)
    };

    // Map parameters — MutableBorrow params become MutPtr types
    let params: Vec<(TypeId, Option<&str>)> = func
        .params
        .iter()
        .map(|p| {
            let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
            let gir_type = match p.node.ownership {
                Ownership::MutableBorrow => ctx.register_mut_ptr_type(base_type),
                _ => base_type,
            };
            let param_name = p.node.name.node.as_str();
            (gir_type, Some(param_name))
        })
        .collect();

    let mut builder = FunctionBuilder::new(name.to_string(), return_type, &params);

    // Clear and register locals for this function
    ctx.clear_locals();

    // Register parameters as locals
    ctx.callable_return_types.clear();
    for (i, p) in func.params.iter().enumerate() {
        let local_id = LocalId((i + 1) as u32); // _1, _2, ...
        let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
        let gir_type = match p.node.ownership {
            Ownership::MutableBorrow => ctx.register_mut_ptr_type(base_type),
            _ => base_type,
        };
        ctx.register_local(&p.node.name.node, local_id, gir_type);
        // Register mutable borrow params for auto-deref at use sites
        if matches!(p.node.ownership, Ownership::MutableBorrow) {
            ctx.mut_capture_locals.insert(local_id, base_type);
        }
        // Track callable parameter return types
        if let Some(ret_type) = extract_callable_return_type(&p.node.type_.node, &[], ctx) {
            ctx.callable_return_types.insert(local_id, ret_type);
        }
    }

    // Track throws context for Result wrapping in return/throw
    ctx.current_throws_result_type = if func.throws.is_some() {
        Some(return_type)
    } else {
        None
    };

    // P2.6: Push Function drop scope
    ctx.drops.push_scope(DropScopeKind::Function);

    // Register function parameters with the drop elaborator so that ref-counted
    // types (Channel, Shared, Weak) passed by value are released at scope exit.
    for (i, p) in func.params.iter().enumerate() {
        let local_id = LocalId((i + 1) as u32);
        let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
        let gir_type = match p.node.ownership {
            Ownership::MutableBorrow => ctx.register_mut_ptr_type(base_type),
            _ => base_type,
        };
        ctx.drops.register_param(local_id, gir_type, &ctx.type_registry);
    }

    // Lower the body
    match &func.body {
        FunctionBody::Block(block) => {
            lower_block(ctx, &mut builder, block);

            // Add implicit return if the last block has no terminator
            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                // P2.6: Emit scope drops before implicit return
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                if is_main {
                    builder.assign(
                        Place::local(LocalId(0)),
                        FunctionBuilder::const_i32(0),
                    );
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                } else if return_type == UNIT_TYPE {
                    builder.ret(FunctionBuilder::const_unit());
                } else {
                    // Non-void function without explicit return — emit return _0
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                }
            } else {
                // Explicit return already handled drops via emit_early_exit_drops.
                // Just pop the scope tracking without emitting more drops.
                ctx.drops.pop_scope_no_emit();
            }
        }

        FunctionBody::Expression(expr) => {
            let operand = lower_expr(ctx, &mut builder, expr);
            builder.assign(Place::local(LocalId(0)), operand);
            // P2.6: Emit scope drops before return
            ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            builder.ret(FunctionBuilder::copy(LocalId(0)));
        }

        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            // Not handled in lowering — skip
            // Pop the Function scope we pushed
            ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            return;
        }
    }

    let mut func = builder.build();
    func.display_name = Some(name.to_string());
    func.def_span = Some(func_span);
    module.functions.push(func);
}

/// Lower an equip method into a standalone GIR function with mangled name.
pub fn lower_equip_method(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    method: &FunctionDef,
    type_name: &str,
    equipped_type: &Type,
) {
    let method_name = &method.name.node;
    let mangled_name = format!("{type_name}__{method_name}");

    let return_type = ctx.type_mapper.map_ast_type(&method.return_type.node);

    // Check if method has a self parameter (static methods don't)
    let has_self = method.params.first()
        .map(|p| p.node.name.node == "self")
        .unwrap_or(false);

    // Build parameters: optional self pointer + explicit params
    let mut params: Vec<(TypeId, Option<&str>)> = Vec::new();
    let self_ptr_type = if has_self {
        let self_type_id = ctx.type_mapper.map_ast_type(equipped_type);
        let self_is_mutable = method.params.first()
            .map(|p| matches!(p.node.ownership, Ownership::MutableBorrow))
            .unwrap_or(false);
        let spt = if self_is_mutable {
            ctx.register_mut_ptr_type(self_type_id)
        } else {
            ctx.register_ptr_type(self_type_id)
        };
        params.push((spt, Some("self")));
        Some(spt)
    } else {
        None
    };
    for p in &method.params {
        if p.node.name.node == "self" {
            continue; // self handled above
        }
        let gir_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
        params.push((gir_type, Some(p.node.name.node.as_str())));
    }

    let mut builder = FunctionBuilder::new(mangled_name, return_type, &params);

    // Clear and register locals
    ctx.clear_locals();
    ctx.callable_return_types.clear();

    // Register self as local _1 (only if method has self)
    let mut param_idx = if let Some(spt) = self_ptr_type {
        ctx.register_local("self", LocalId(1), spt);
        2u32
    } else {
        1u32
    };

    // Register other params
    for p in &method.params {
        if p.node.name.node == "self" {
            continue;
        }
        let gir_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
        ctx.register_local(&p.node.name.node, LocalId(param_idx), gir_type);
        // Track callable parameter return types for indirect call lowering
        if let Some(ret_type) = extract_callable_return_type(&p.node.type_.node, &[], ctx) {
            ctx.callable_return_types.insert(LocalId(param_idx), ret_type);
        }
        param_idx += 1;
    }

    // P2.6: Push Function drop scope
    ctx.drops.push_scope(DropScopeKind::Function);

    // Register function parameters with the drop elaborator so that ref-counted
    // types (Channel, Shared, Weak) passed by value are released at scope exit.
    {
        let mut pidx = if self_ptr_type.is_some() { 2u32 } else { 1u32 };
        for p in &method.params {
            if p.node.name.node == "self" {
                continue;
            }
            let gir_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
            ctx.drops.register_param(LocalId(pidx), gir_type, &ctx.type_registry);
            pidx += 1;
        }
    }

    // Lower the body
    match &method.body {
        FunctionBody::Block(block) => {
            // Evaluate delayed meta blocks (meta if/for) with Self bound to the equipped type.
            let mut block = block.clone();
            let self_subs = vec![("Self".to_string(), equipped_type.clone())];
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
            meta::evaluate_delayed_meta_block(&mut block, &delayed_ctx, &mut meta_errors);
            for e in &meta_errors {
                eprintln!("[delayed-meta equip] {e:?}");
            }
            lower_block(ctx, &mut builder, &block);

            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                if return_type == UNIT_TYPE {
                    builder.ret(FunctionBuilder::const_unit());
                } else {
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                }
            } else {
                ctx.drops.pop_scope_no_emit();
            }
        }

        FunctionBody::Expression(expr) => {
            let operand = lower_expr(ctx, &mut builder, expr);
            builder.assign(Place::local(LocalId(0)), operand);
            ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            builder.ret(FunctionBuilder::copy(LocalId(0)));
        }

        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            ctx.drops.pop_scope_no_emit();
            return;
        }
    }

    let mut func = builder.build();
    func.display_name = Some(format!("{type_name}.{method_name}"));
    module.functions.push(func);
}

/// Lower a monomorphized instance of a generic function.
///
/// `type_args` are the concrete type arguments (e.g., `[int]` for `identity[int]`).
/// `mangled_name` is the fully mangled name (e.g., `identity__int64_t`).
///
/// The function body is lowered with type parameter substitutions active,
/// so references to `T` in the template resolve to the concrete type.
pub fn lower_generic_function(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    template: &FunctionDef,
    type_args: &[Spanned<Type>],
    mangled_name: &str,
    meta_op_bindings: &[(String, ast::BinaryOp)],
) {
    let subs = build_subs(template.generic_params.as_ref(), type_args);

    // Build a meta env pre-populated with any compile-time operator bindings.
    // Used by evaluate_delayed_meta_block so MetaOpInfix nodes get substituted
    // to real BinaryOp expressions during monomorphization.
    let mut meta_env_map: rustc_hash::FxHashMap<String, MetaValue> =
        rustc_hash::FxHashMap::default();
    for (param_name, op) in meta_op_bindings {
        meta_env_map.insert(param_name.clone(), MetaValue::Op(*op));
    }

    // Evaluate delayed meta blocks (meta if/for inside generic bodies) with
    // the concrete type substitutions.  Modifies a local clone of the template
    // so the original template is left intact for subsequent instantiations.
    let template_with_meta_evaluated;
    let template = if subs.is_empty() && meta_env_map.is_empty() {
        template
    } else {
        let mut cloned = template.clone();
        let delayed_ctx = DelayedMetaContext {
            type_subs:      &subs,
            features:       &[],
            meta_env:       &meta_env_map,
            items:          &[],
            trait_registry: &ctx.analysis.traits,
            type_registry:  &ctx.type_registry,
        };
        if let FunctionBody::Block(ref mut block) = cloned.body {
            let mut errors = Vec::new();
            meta::evaluate_delayed_meta_block(block, &delayed_ctx, &mut errors);
            // Errors are non-fatal here (will surface as missing symbols); log if any.
            if !errors.is_empty() {
                for e in &errors {
                    eprintln!("[delayed-meta] {e:?}");
                }
            }
        }
        template_with_meta_evaluated = cloned;
        &template_with_meta_evaluated
    };

    // Build type name substitutions for struct init/method calls in the body
    build_type_name_subs(ctx, &subs);

    // Build generic type parameter → concrete TypeId substitutions
    build_generic_type_params(ctx, &subs);

    // Map return type with substitutions
    let return_type = substitute_and_map_type(ctx, &template.return_type.node, &subs);

    // Map parameters with substitutions — skip meta op params (no runtime representation),
    // MutableBorrow params become MutPtr
    let params: Vec<(TypeId, Option<String>)> = template
        .params
        .iter()
        .filter(|p| !p.node.is_meta_op)
        .map(|p| {
            let base_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
            let gir_type = match p.node.ownership {
                Ownership::MutableBorrow => ctx.register_mut_ptr_type(base_type),
                _ => base_type,
            };
            (gir_type, Some(p.node.name.node.clone()))
        })
        .collect();

    let param_refs: Vec<(TypeId, Option<&str>)> = params
        .iter()
        .map(|(tid, name)| (*tid, name.as_deref()))
        .collect();

    let mut builder = FunctionBuilder::new(mangled_name, return_type, &param_refs);

    // Clear and register locals — assign sequential LocalIds to runtime params only
    // (meta op params carry no runtime value and are skipped).
    ctx.clear_locals();
    ctx.callable_return_types.clear();

    let mut local_idx: u32 = 0;
    for p in template.params.iter() {
        if p.node.is_meta_op {
            continue;
        }
        local_idx += 1;
        let local_id = LocalId(local_idx);
        let base_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
        let gir_type = match p.node.ownership {
            Ownership::MutableBorrow => ctx.register_mut_ptr_type(base_type),
            _ => base_type,
        };
        ctx.register_local(&p.node.name.node, local_id, gir_type);
        // Register mutable borrow params for auto-deref at use sites
        if matches!(p.node.ownership, Ownership::MutableBorrow) {
            ctx.mut_capture_locals.insert(local_id, base_type);
        }
        // Track callable parameter return types for indirect call lowering
        if let Some(ret_type) = extract_callable_return_type(&p.node.type_.node, &subs, ctx) {
            ctx.callable_return_types.insert(local_id, ret_type);
        }
    }

    // P2.6: Push Function drop scope
    ctx.drops.push_scope(DropScopeKind::Function);

    // Register function parameters with the drop elaborator so that ref-counted
    // types (Channel, Shared, Weak) passed by value are released at scope exit.
    // Use register_param (not register_local) — params are borrowed from the caller,
    // so only Copy-with-drop types (refcounted) need dropping, not Move types.
    // Skip meta op params — they have no runtime local slot.
    let mut drop_idx: u32 = 0;
    for p in template.params.iter() {
        if p.node.is_meta_op {
            continue;
        }
        drop_idx += 1;
        let local_id = LocalId(drop_idx);
        let base_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
        let gir_type = match p.node.ownership {
            Ownership::MutableBorrow => ctx.register_mut_ptr_type(base_type),
            _ => base_type,
        };
        ctx.drops.register_param(local_id, gir_type, &ctx.type_registry);
    }

    // Lower the body
    match &template.body {
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
                ctx.drops.pop_scope_no_emit();
            }
        }

        FunctionBody::Expression(expr) => {
            let operand = lower_expr(ctx, &mut builder, expr);
            builder.assign(Place::local(LocalId(0)), operand);
            ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            builder.ret(FunctionBuilder::copy(LocalId(0)));
        }

        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            ctx.drops.pop_scope_no_emit();
            ctx.type_name_subs.clear();
            ctx.generic_type_params.clear();
            return;
        }
    }

    ctx.type_name_subs.clear();
    ctx.generic_type_params.clear();
    module.functions.push(builder.build());
}

/// Lower monomorphized equip methods for a generic type instantiation.
///
/// For each method in the equip block, creates a GIR function named
/// `{mangled_type_name}__{method_name}` with substituted types.
pub fn lower_generic_equip_methods(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    equip: &ast::EquipBlock,
    type_args: &[Spanned<Type>],
    mangled_type_name: &str,
) {
    lower_generic_equip_methods_with_defaults(ctx, module, equip, type_args, mangled_type_name, None);
}

/// Lower monomorphized equip methods for a generic type instantiation,
/// with optional AST module for default trait method emission.
pub fn lower_generic_equip_methods_with_defaults(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    equip: &ast::EquipBlock,
    type_args: &[Spanned<Type>],
    mangled_type_name: &str,
    ast_module: Option<&ast::Module>,
) {
    let subs = build_equip_subs(equip, type_args);

    // Substituted equipped type — used for Self binding in delayed meta evaluation.
    let substituted_equipped_type = generics::substitute_type_pub(&equip.type_.node, &subs);

    // Build type name substitutions for struct init/method calls in the body
    build_type_name_subs(ctx, &subs);

    // Add substitution for the equipped type itself (e.g., Pair__T → Pair__int64_t)
    // This handles cases where the method body references the struct/enum being equipped
    if let Type::Named { name, generic_args } = &equip.type_.node {
        let base_name = &name.node;
        if !generic_args.is_empty() {
            // Mangle the template name (with generic params as wildcards)
            // For Pair[T], we want "Pair__T"
            let template_mangled = super::types::mangle_generic_name(base_name, generic_args);
            // mangled_type_name is already the concrete name (e.g., "Pair__int64_t")
            if template_mangled != mangled_type_name {
                ctx.type_name_subs.insert(template_mangled, mangled_type_name.to_string());
            }
        }
    }

    // Build generic type parameter → concrete TypeId substitutions
    build_generic_type_params(ctx, &subs);

    for method in &equip.items {
        let method_def = &method.node;
        let method_mangled = format!("{mangled_type_name}__{}", method_def.name.node);

        // Use map_ast_type_mut so that generic return types like Option[T] get
        // registered (not silently resolved to UNIT_TYPE) after substitution.
        let substituted_ret = generics::substitute_type_pub(&method_def.return_type.node, &subs);
        let return_type = ctx.type_mapper.map_ast_type_mut(&substituted_ret, &mut ctx.type_registry);

        // Self pointer type — only for methods with a self parameter
        let has_self = method_def.params.first()
            .map(|p| p.node.name.node == "self")
            .unwrap_or(false);

        let self_type_id = ctx.type_mapper.lookup_named(mangled_type_name).unwrap_or(UNIT_TYPE);
        let self_is_mutable = method_def.params.first()
            .map(|p| {
                p.node.name.node == "self" &&
                matches!(p.node.ownership, Ownership::MutableBorrow)
            })
            .unwrap_or(false);

        let self_ptr_type = if self_is_mutable {
            ctx.register_mut_ptr_type(self_type_id)
        } else {
            ctx.register_ptr_type(self_type_id)
        };

        let mut params: Vec<(TypeId, Option<&str>)> = if has_self {
            vec![(self_ptr_type, Some("self"))]
        } else {
            vec![]
        };
        for p in &method_def.params {
            if p.node.name.node == "self" {
                continue;
            }
            let gir_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
            params.push((gir_type, Some(p.node.name.node.as_str())));
        }

        let mut builder = FunctionBuilder::new(method_mangled, return_type, &params);

        ctx.clear_locals();
        ctx.callable_return_types.clear();
        let mut param_idx = if has_self {
            ctx.register_local("self", LocalId(1), self_ptr_type);
            2u32
        } else {
            1u32
        };
        for p in &method_def.params {
            if p.node.name.node == "self" {
                continue;
            }
            let gir_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
            ctx.register_local(&p.node.name.node, LocalId(param_idx), gir_type);
            // Track callable parameter return types for indirect call lowering
            if let Some(ret_type) = extract_callable_return_type(&p.node.type_.node, &subs, ctx) {
                ctx.callable_return_types.insert(LocalId(param_idx), ret_type);
            }
            param_idx += 1;
        }

        // P2.6: Push Function drop scope
        ctx.drops.push_scope(DropScopeKind::Function);

        // Register function parameters with the drop elaborator so that ref-counted
        // types (Channel, Shared, Weak) passed by value are released at scope exit.
        {
            let mut pidx = if has_self { 2u32 } else { 1u32 };
            for p in &method_def.params {
                if p.node.name.node == "self" {
                    continue;
                }
                let gir_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
                ctx.drops.register_param(LocalId(pidx), gir_type, &ctx.type_registry);
                pidx += 1;
            }
        }

        match &method_def.body {
            FunctionBody::Block(block) => {
                // Evaluate delayed meta blocks (meta if/for) with Self bound to the equipped type.
                let mut block = block.clone();
                let self_subs = vec![("Self".to_string(), substituted_equipped_type.clone())];
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
                meta::evaluate_delayed_meta_block(&mut block, &delayed_ctx, &mut meta_errors);
                for e in &meta_errors {
                    eprintln!("[delayed-meta generic-equip] {e:?}");
                }
                lower_block(ctx, &mut builder, &block);

                let last_block_idx = builder.current_block.0 as usize;
                if builder.blocks[last_block_idx].terminator.is_none() {
                    ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                    if return_type == UNIT_TYPE {
                        builder.ret(FunctionBuilder::const_unit());
                    } else {
                        builder.ret(FunctionBuilder::copy(LocalId(0)));
                    }
                } else {
                    ctx.drops.pop_scope_no_emit();
                }
            }
            FunctionBody::Expression(expr) => {
                let operand = lower_expr(ctx, &mut builder, expr);
                builder.assign(Place::local(LocalId(0)), operand);
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                builder.ret(FunctionBuilder::copy(LocalId(0)));
            }
            FunctionBody::Declaration | FunctionBody::Extern(_) => {
                ctx.drops.pop_scope_no_emit();
                continue;
            }
        }

        module.functions.push(builder.build());
    }

    // Emit default trait methods that aren't overridden in the equip block
    if let (Some(ast_mod), Some(trait_ref)) = (ast_module, &equip.trait_) {
        use crate::parser::ast::{Item, TraitItem};
        let trait_name = super::traits::extract_trait_name(&trait_ref.trait_name.node);
        if !trait_name.is_empty() {
            let implemented: Vec<String> = equip.items.iter()
                .map(|m| m.node.name.node.clone())
                .collect();
            // Substituted equipped type already computed above; re-borrow for this scope.
            let substituted_type = substituted_equipped_type.clone();
            // Find trait def and emit defaults
            for item in &ast_mod.items {
                if let Item::Trait(trait_def) = &item.node {
                    if trait_def.name.node == trait_name {
                        for trait_item in &trait_def.items {
                            if let TraitItem::Method(default_method) = &trait_item.node {
                                let method_name = &default_method.name.node;
                                if implemented.contains(method_name) {
                                    continue;
                                }
                                match &default_method.body {
                                    FunctionBody::Declaration | FunctionBody::Extern(_) => continue,
                                    FunctionBody::Block(_) | FunctionBody::Expression(_) => {}
                                }
                                // Emit as {mangled_type_name}__{method_name}
                                lower_equip_method(
                                    ctx, module, default_method,
                                    mangled_type_name, &substituted_type,
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    ctx.type_name_subs.clear();
    ctx.generic_type_params.clear();
}

/// Build type parameter substitutions from generic params + concrete type args.
fn build_subs(
    generic_params: Option<&Spanned<ast::GenericParams>>,
    type_args: &[Spanned<Type>],
) -> Vec<(String, Type)> {
    let mut subs = Vec::new();
    if let Some(params) = generic_params {
        for (param, arg) in params.node.params.iter().zip(type_args.iter()) {
            let name = match &param.node {
                GenericParam::Type { name: s, .. } => s.node.clone(),
                GenericParam::Lifetime(s) => s.node.clone(),
                GenericParam::Const { name, .. } => name.node.clone(),
            };
            subs.push((name, arg.node.clone()));
        }
    }
    subs
}

/// Build type parameter substitutions for an equip block.
fn build_equip_subs(
    equip: &ast::EquipBlock,
    type_args: &[Spanned<Type>],
) -> Vec<(String, Type)> {
    if let Some(ref gp) = equip.generic_params {
        return build_subs(Some(gp), type_args);
    }
    // Fall back: extract params from the equipped type's generic args
    if let Type::Named { generic_args, .. } = &equip.type_.node {
        let mut subs = Vec::new();
        for (param_type, arg) in generic_args.iter().zip(type_args.iter()) {
            if let Type::Named { name, generic_args: inner } = &param_type.node {
                if inner.is_empty() {
                    subs.push((name.node.clone(), arg.node.clone()));
                }
            }
        }
        return subs;
    }
    Vec::new()
}

/// Substitute type parameters in an AST type and map to GIR TypeId.
fn substitute_and_map_type(
    ctx: &LoweringContext,
    ty: &Type,
    subs: &[(String, Type)],
) -> TypeId {
    let substituted = generics::substitute_type_pub(ty, subs);
    ctx.type_mapper.map_ast_type(&substituted)
}

/// Extract the return type of a callable/function parameter type.
///
/// For parameters like `Callable[T(int)]` or `int(int)`, extracts the return type
/// after applying generic substitutions. Returns None if the type isn't a callable.
fn extract_callable_return_type(
    ty: &Type,
    subs: &[(String, Type)],
    ctx: &LoweringContext,
) -> Option<TypeId> {
    match ty {
        // Callable[RetType(Params...)] or MutCallable[...] or ConsumeCallable[...]
        Type::Named { name, generic_args } => {
            let name_str = name.node.as_str();
            if name_str == "Callable" || name_str == "MutCallable" || name_str == "ConsumeCallable" {
                // The generic_args should contain a single Function type
                if let Some(func_type) = generic_args.first() {
                    if let Type::Function { return_type, .. } = &func_type.node {
                        let ret_type = substitute_and_map_type(ctx, &return_type.node, subs);
                        return Some(ret_type);
                    }
                }
            }
            None
        }
        // Direct function type: RetType(Params...)
        Type::Function { return_type, .. } => {
            let ret_type = substitute_and_map_type(ctx, &return_type.node, subs);
            Some(ret_type)
        }
        _ => None,
    }
}

/// Build generic type parameter → concrete TypeId substitutions.
///
/// For each type parameter (e.g., T), maps it to the concrete TypeId
/// (e.g., I64_TYPE for int). This enables `map_type_with_subs` to resolve
/// bare type parameters in variable declarations inside generic bodies.
fn build_generic_type_params(ctx: &mut LoweringContext, subs: &[(String, Type)]) {
    ctx.generic_type_params.clear();
    for (param_name, concrete_ty) in subs {
        let type_id = ctx.type_mapper.map_ast_type(concrete_ty);
        ctx.generic_type_params.insert(param_name.clone(), type_id);
    }
}

/// Build type name substitution map for generic body lowering.
///
/// For each registered type name that contains a type parameter placeholder
/// (e.g., `Container__T`), computes the concrete mangled name (e.g.,
/// `Container__int64_t`) and stores the mapping in ctx.type_name_subs.
fn build_type_name_subs(ctx: &mut LoweringContext, subs: &[(String, Type)]) {
    ctx.type_name_subs.clear();

    // Build a map of param-mangled-fragment → concrete-mangled-fragment.
    // E.g., for sub T → int:  "T" → "int64_t"
    // For sub T → str:  "T" → "Str"
    let fragment_subs: Vec<(String, String)> = subs.iter().map(|(param, concrete_ty)| {
        let concrete_name = super::types::mangle_type_for_name(concrete_ty);
        (param.clone(), concrete_name)
    }).collect();

    // Store fragment subs for on-the-fly resolution of names not in the pre-computed map
    ctx.generic_fragment_subs = fragment_subs.clone();

    // Scan all known type names in the registry for template patterns.
    // For each name like "Container__T", substitute "T" → "int64_t" to get "Container__int64_t".
    let type_names: Vec<String> = ctx.type_registry.type_defs().iter()
        .map(|def| def.name.clone())
        .collect();
    for name in type_names {
        let mut substituted = name.clone();
        let mut changed = false;
        for (param, concrete) in &fragment_subs {
            // Match `__T` at end of name or `__T__` in middle
            let pattern_end = format!("__{param}");
            let pattern_mid = format!("__{param}__");
            if substituted.ends_with(&pattern_end) {
                let prefix = &substituted[..substituted.len() - pattern_end.len()];
                substituted = format!("{prefix}__{concrete}");
                changed = true;
            } else if substituted.contains(&pattern_mid) {
                substituted = substituted.replace(&pattern_mid, &format!("__{concrete}__"));
                changed = true;
            }
        }
        if changed && name != substituted {
            ctx.type_name_subs.insert(name, substituted);
        }
    }
}
