//! Pattern matching and match-statement lowering.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{self, Block, Expr, Pattern};
use crate::span::Spanned;

use super::super::context::LoweringContext;
use super::super::drops::DropScopeKind;
use super::super::exprs::{lower_expr, infer_operand_type_full, resolve_none_tag};
use super::lower_block;

/// Lower a match statement to GIR using Branch chains.
pub(super) fn lower_match_stmt(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    scrutinee: &Spanned<Expr>,
    arms: &[ast::MatchItem],
    else_arm: &Option<Block>,
) {
    // Lower scrutinee to a temp local
    let scrut_op = lower_expr(ctx, builder, scrutinee);
    let scrut_type = infer_operand_type_full(ctx, &scrut_op, builder);
    let scrut_local = builder.add_local(scrut_type, None);
    builder.assign(Place::local(scrut_local), scrut_op);

    let merge_bb = builder.new_block();

    // Process each arm as a test-body chain (MetaFor items are always expanded before lowering)
    let concrete_arms: Vec<&ast::MatchArm> = arms.iter().filter_map(|i| i.arm()).collect();
    for (i, arm) in concrete_arms.iter().enumerate() {
        let arm_body_bb = builder.new_block();
        let next_test_bb = if i + 1 < concrete_arms.len() || else_arm.is_some() {
            builder.new_block()
        } else {
            merge_bb
        };

        // Emit pattern condition check
        let cond = lower_pattern_condition(ctx, builder, &arm.pattern, scrut_local, scrut_type);

        if arm.guard.is_some() {
            // Pattern match → check guard → arm body
            let guard_bb = builder.new_block();
            builder.branch(cond, guard_bb, next_test_bb);

            builder.switch_to(guard_bb);
            ctx.drops.push_scope(DropScopeKind::Block);
            emit_pattern_bindings(ctx, builder, &arm.pattern, scrut_local, scrut_type);
            let guard_cond = lower_expr(ctx, builder, arm.guard.as_ref().unwrap());
            builder.branch(guard_cond, arm_body_bb, next_test_bb);

            builder.switch_to(arm_body_bb);
            lower_expr(ctx, builder, &arm.body);
            if builder.is_terminated() {
                // Return/break/continue already emitted early-exit drops — don't double-drop.
                ctx.drops.pop_scope_no_emit();
            } else {
                ctx.drops.pop_scope(builder, &ctx.type_registry);
                builder.jump(merge_bb);
            }
        } else {
            builder.branch(cond, arm_body_bb, next_test_bb);

            // Arm body
            builder.switch_to(arm_body_bb);
            ctx.drops.push_scope(DropScopeKind::Block);
            emit_pattern_bindings(ctx, builder, &arm.pattern, scrut_local, scrut_type);
            lower_expr(ctx, builder, &arm.body);
            if builder.is_terminated() {
                // Return/break/continue already emitted early-exit drops — don't double-drop.
                ctx.drops.pop_scope_no_emit();
            } else {
                ctx.drops.pop_scope(builder, &ctx.type_registry);
                builder.jump(merge_bb);
            }
        }

        builder.switch_to(next_test_bb);
    }

    // Else arm
    if let Some(else_body) = else_arm {
        ctx.drops.push_scope(DropScopeKind::Block);
        lower_block(ctx, builder, else_body);
        if builder.is_terminated() {
            ctx.drops.pop_scope_no_emit();
        } else {
            ctx.drops.pop_scope(builder, &ctx.type_registry);
            builder.jump(merge_bb);
        }
    }

    builder.switch_to(merge_bb);
}

/// Lower a pattern condition to a boolean Operand.
pub fn lower_pattern_condition(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    pattern: &Spanned<Pattern>,
    scrut_local: LocalId,
    scrut_type: TypeId,
) -> Operand {
    match &pattern.node {
        Pattern::Wildcard => FunctionBuilder::const_bool(true),

        Pattern::Literal(expr) => {
            // None literal: compare enum tag instead of struct == NULL
            if matches!(expr.node, Expr::NoneLiteral) {
                let tag = builder.tag_of(FunctionBuilder::copy(scrut_local));
                let none_tag = resolve_none_tag(ctx, scrut_type);
                let cmp = builder.cmp(
                    CmpOp::Eq,
                    I32_TYPE,
                    FunctionBuilder::copy(tag),
                    Operand::Constant(Constant::I32(none_tag)),
                );
                return FunctionBuilder::copy(cmp);
            }
            let lit_op = lower_expr(ctx, builder, expr);
            let cmp = builder.cmp(
                CmpOp::Eq,
                scrut_type,
                FunctionBuilder::copy(scrut_local),
                lit_op,
            );
            FunctionBuilder::copy(cmp)
        }

        Pattern::Binding(name) => {
            // Check if this is an enum variant name (unit variant match)
            if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(name) {
                let tag = builder.tag_of(FunctionBuilder::copy(scrut_local));
                if let Some(variant_tag) = ctx.resolve_variant_tag(&enum_name, &variant_name) {
                    let cmp = builder.cmp(
                        CmpOp::Eq,
                        I32_TYPE,
                        FunctionBuilder::copy(tag),
                        Operand::Constant(Constant::I32(variant_tag as i32)),
                    );
                    return FunctionBuilder::copy(cmp);
                }
            }
            // Plain variable binding — always matches
            FunctionBuilder::const_bool(true)
        }

        Pattern::Constructor { path, .. } => {
            let variant_name = if let Some(last) = path.last() {
                last.node.clone()
            } else {
                return FunctionBuilder::const_bool(true);
            };
            // Qualified path (Color.Red): use first segment as enum name
            let (enum_name, variant_name) = if path.len() >= 2 {
                (path[0].node.clone(), variant_name)
            } else {
                // Bare variant: look up via enum_variants (prelude: Ok, Error, Some, None)
                match ctx.resolve_enum_variant(&variant_name) {
                    Some((en, vn)) => (en, vn),
                    None => return FunctionBuilder::const_bool(true),
                }
            };
            let tag = builder.tag_of(FunctionBuilder::copy(scrut_local));
            if let Some(variant_tag) = ctx.resolve_variant_tag(&enum_name, &variant_name) {
                let cmp = builder.cmp(
                    CmpOp::Eq,
                    I32_TYPE,
                    FunctionBuilder::copy(tag),
                    Operand::Constant(Constant::I32(variant_tag as i32)),
                );
                return FunctionBuilder::copy(cmp);
            }
            FunctionBuilder::const_bool(true)
        }

        Pattern::Or(alts) => {
            // Short-circuit OR: if any alternative matches, return true
            let result_id = builder.add_local(BOOL_TYPE, None);
            builder.assign(Place::local(result_id), FunctionBuilder::const_bool(false));

            let merge_bb = builder.new_block();

            for (i, alt) in alts.iter().enumerate() {
                let cond = lower_pattern_condition(ctx, builder, alt, scrut_local, scrut_type);
                let next_bb = if i + 1 < alts.len() {
                    builder.new_block()
                } else {
                    merge_bb
                };
                let true_bb = builder.new_block();
                builder.branch(cond, true_bb, next_bb);

                builder.switch_to(true_bb);
                builder.assign(Place::local(result_id), FunctionBuilder::const_bool(true));
                builder.jump(merge_bb);

                if i + 1 < alts.len() {
                    builder.switch_to(next_bb);
                }
            }

            builder.switch_to(merge_bb);
            FunctionBuilder::copy(result_id)
        }

        Pattern::Tuple(_) | Pattern::Rest => {
            // Structural match — always matches if types match
            FunctionBuilder::const_bool(true)
        }

        Pattern::DotShorthand { variant, .. } => {
            // Use scrutinee type to look up the enum name, then compare tag
            let enum_name = ctx.type_registry.type_name(scrut_type)
                .or_else(|| {
                    if let Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) = ctx.type_registry.get(scrut_type).cloned() {
                        ctx.type_registry.type_name(inner)
                    } else {
                        None
                    }
                })
                .or_else(|| ctx.resolve_enum_variant(&variant.node).map(|(en, _)| en));
            if let Some(ref en) = enum_name {
                let tag = builder.tag_of(FunctionBuilder::copy(scrut_local));
                if let Some(variant_tag) = ctx.resolve_variant_tag(en, &variant.node) {
                    let cmp = builder.cmp(
                        CmpOp::Eq,
                        I32_TYPE,
                        FunctionBuilder::copy(tag),
                        Operand::Constant(Constant::I32(variant_tag as i32)),
                    );
                    return FunctionBuilder::copy(cmp);
                }
            }
            FunctionBuilder::const_bool(true)
        }
    }
}

/// Emit pattern bindings — assign destructured values to local variables.
pub fn emit_pattern_bindings(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    pattern: &Spanned<Pattern>,
    scrut_local: LocalId,
    scrut_type: TypeId,
) {
    match &pattern.node {
        Pattern::Binding(name) => {
            // If not an enum variant, bind the scrutinee value
            if ctx.resolve_enum_variant(name).is_none() {
                ctx.register_local(name, scrut_local, scrut_type);
            }
        }

        Pattern::Constructor { path, fields } => {
            let variant_name = if let Some(last) = path.last() {
                last.node.clone()
            } else {
                return;
            };

            // Use scrutinee type to find the enum name (avoids ambiguous variant lookups
            // when multiple monomorphized enums share variant names like "Some"/"None"/"Ok"/"Err")
            // For qualified paths (Color.Red), path[0] gives us the explicit enum name.
            let enum_name = if path.len() >= 2 {
                Some(path[0].node.clone())
            } else {
                ctx.type_registry.type_name(scrut_type)
                    .or_else(|| {
                        // Fallback: pointer type → dereference to find pointee name
                        if let Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) = ctx.type_registry.get(scrut_type).cloned() {
                            ctx.type_registry.type_name(inner)
                        } else {
                            None
                        }
                    })
                    .or_else(|| {
                        // Last resort: use variant name lookup (may be ambiguous for generics)
                        ctx.resolve_enum_variant(&variant_name).map(|(en, _)| en)
                    })
            };
            let enum_name = if let Some(en) = enum_name {
                en
            } else {
                return;
            };

            for (i, field_pat) in fields.iter().enumerate() {
                // Determine the field type from the enum variant definition
                let field_type = if let Some(type_def) = ctx.type_registry.get_type_def(&enum_name) {
                    if let TypeDefKind::Enum(ref e) = type_def.kind {
                        if let Some(v) = e.variants.iter().find(|v| v.name == variant_name) {
                            if let Some(f) = v.fields.get(i) {
                                f.type_id
                            } else {
                                I64_TYPE
                            }
                        } else {
                            I64_TYPE
                        }
                    } else {
                        I64_TYPE
                    }
                } else {
                    I64_TYPE
                };

                let dst = builder.enum_field_load(
                    Place::local(scrut_local),
                    variant_name.clone(),
                    i as u32,
                    field_type,
                );

                // Recurse on sub-pattern
                emit_pattern_bindings(ctx, builder, field_pat, dst, field_type);
            }
        }

        Pattern::Tuple(elems) => {
            for (i, elem_pat) in elems.iter().enumerate() {
                // Use field_load with field index
                let elem_type = I64_TYPE; // placeholder — real type needs registry
                let dst = builder.field_load(Place::local(scrut_local), i as u32, elem_type);
                emit_pattern_bindings(ctx, builder, elem_pat, dst, elem_type);
            }
        }

        Pattern::DotShorthand { variant, fields } => {
            // Look up enum name from scrutinee type (same as Constructor)
            let enum_name = ctx.type_registry.type_name(scrut_type)
                .or_else(|| {
                    if let Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) = ctx.type_registry.get(scrut_type).cloned() {
                        ctx.type_registry.type_name(inner)
                    } else {
                        None
                    }
                })
                .or_else(|| ctx.resolve_enum_variant(&variant.node).map(|(en, _)| en));
            let enum_name = if let Some(en) = enum_name { en } else { return; };
            let variant_name = variant.node.clone();

            for (i, field_pat) in fields.iter().enumerate() {
                let field_type = if let Some(type_def) = ctx.type_registry.get_type_def(&enum_name) {
                    if let TypeDefKind::Enum(ref e) = type_def.kind {
                        if let Some(v) = e.variants.iter().find(|v| v.name == variant_name) {
                            v.fields.get(i).map(|f| f.type_id).unwrap_or(I64_TYPE)
                        } else { I64_TYPE }
                    } else { I64_TYPE }
                } else { I64_TYPE };

                let dst = builder.enum_field_load(
                    Place::local(scrut_local),
                    variant_name.clone(),
                    i as u32,
                    field_type,
                );
                emit_pattern_bindings(ctx, builder, field_pat, dst, field_type);
            }
        }

        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Or(_) | Pattern::Rest => {
            // No bindings
        }
    }
}
