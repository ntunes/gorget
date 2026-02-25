use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{self, FunctionBody, FunctionDef, GenericParam, Ownership, Type};
use crate::span::Spanned;

use super::context::LoweringContext;
use super::drops::DropScopeKind;
use super::exprs::lower_expr;
use super::generics;
use super::stmts::lower_block;

/// Lower a single function definition into the GIR module.
pub fn lower_function(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    func: &FunctionDef,
) {
    let name = &func.name.node;
    let is_main = name == "main";

    // Map return type
    let return_type = if is_main {
        I32_TYPE
    } else {
        ctx.type_mapper.map_ast_type(&func.return_type.node)
    };

    // Map parameters
    let params: Vec<(TypeId, Option<&str>)> = func
        .params
        .iter()
        .map(|p| {
            let gir_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
            let param_name = p.node.name.node.as_str();
            (gir_type, Some(param_name))
        })
        .collect();

    let mut builder = FunctionBuilder::new(name.clone(), return_type, &params);

    // Clear and register locals for this function
    ctx.clear_locals();

    // Register parameters as locals
    for (i, p) in func.params.iter().enumerate() {
        let local_id = LocalId((i + 1) as u32); // _1, _2, ...
        let gir_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
        ctx.register_local(&p.node.name.node, local_id, gir_type);
    }

    // P2.6: Push Function drop scope
    ctx.drops.push_scope(DropScopeKind::Function);

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
                // Explicit return already handled drops — just pop scope tracking
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
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

    module.functions.push(builder.build());
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

    // Determine self parameter type based on ownership
    let self_type_id = ctx.type_mapper.map_ast_type(equipped_type);
    let self_is_mutable = method.params.first()
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

    // Build parameters: self pointer + explicit params
    let mut params: Vec<(TypeId, Option<&str>)> = vec![(self_ptr_type, Some("self"))];
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

    // Register self as local _1
    ctx.register_local("self", LocalId(1), self_ptr_type);

    // Register other params
    let mut param_idx = 2u32;
    for p in &method.params {
        if p.node.name.node == "self" {
            continue;
        }
        let gir_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
        ctx.register_local(&p.node.name.node, LocalId(param_idx), gir_type);
        param_idx += 1;
    }

    // P2.6: Push Function drop scope
    ctx.drops.push_scope(DropScopeKind::Function);

    // Lower the body
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
            builder.assign(Place::local(LocalId(0)), operand);
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
) {
    let subs = build_subs(template.generic_params.as_ref(), type_args);

    // Map return type with substitutions
    let return_type = substitute_and_map_type(ctx, &template.return_type.node, &subs);

    // Map parameters with substitutions
    let params: Vec<(TypeId, Option<String>)> = template
        .params
        .iter()
        .map(|p| {
            let gir_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
            (gir_type, Some(p.node.name.node.clone()))
        })
        .collect();

    let param_refs: Vec<(TypeId, Option<&str>)> = params
        .iter()
        .map(|(tid, name)| (*tid, name.as_deref()))
        .collect();

    let mut builder = FunctionBuilder::new(mangled_name, return_type, &param_refs);

    // Clear and register locals
    ctx.clear_locals();

    for (i, p) in template.params.iter().enumerate() {
        let local_id = LocalId((i + 1) as u32);
        let gir_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
        ctx.register_local(&p.node.name.node, local_id, gir_type);
    }

    // P2.6: Push Function drop scope
    ctx.drops.push_scope(DropScopeKind::Function);

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
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            }
        }

        FunctionBody::Expression(expr) => {
            let operand = lower_expr(ctx, &mut builder, expr);
            builder.assign(Place::local(LocalId(0)), operand);
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
    let subs = build_equip_subs(equip, type_args);

    for method in &equip.items {
        let method_def = &method.node;
        let method_mangled = format!("{mangled_type_name}__{}", method_def.name.node);

        let return_type = substitute_and_map_type(ctx, &method_def.return_type.node, &subs);

        // Self pointer type
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

        let mut params: Vec<(TypeId, Option<&str>)> = vec![(self_ptr_type, Some("self"))];
        for p in &method_def.params {
            if p.node.name.node == "self" {
                continue;
            }
            let gir_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
            params.push((gir_type, Some(p.node.name.node.as_str())));
        }

        let mut builder = FunctionBuilder::new(method_mangled, return_type, &params);

        ctx.clear_locals();
        ctx.register_local("self", LocalId(1), self_ptr_type);

        let mut param_idx = 2u32;
        for p in &method_def.params {
            if p.node.name.node == "self" {
                continue;
            }
            let gir_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
            ctx.register_local(&p.node.name.node, LocalId(param_idx), gir_type);
            param_idx += 1;
        }

        // P2.6: Push Function drop scope
        ctx.drops.push_scope(DropScopeKind::Function);

        match &method_def.body {
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
                builder.assign(Place::local(LocalId(0)), operand);
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                builder.ret(FunctionBuilder::copy(LocalId(0)));
            }
            FunctionBody::Declaration | FunctionBody::Extern(_) => {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                continue;
            }
        }

        module.functions.push(builder.build());
    }
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
                GenericParam::Type(s) => s.node.clone(),
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
