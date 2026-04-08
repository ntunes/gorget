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
    // Lower scrutinee to a temp local.
    // For & params (MutPtr), lower_expr auto-derefs to a VALUE copy — creating
    // a shallow alias. For match, we want the original MutPtr so scrut_is_ptr
    // detects it and pattern extraction produces borrows, not copies.
    let (scrut_op, scrut_type) = if let Expr::Identifier(name) = &scrutinee.node {
        if let Some((local_id, type_id)) = ctx.lookup_local(name) {
            if ctx.func_state.mut_capture_locals.contains_key(&local_id) {
                // & or ! param — use the MutPtr local directly, skip auto-deref
                (Operand::Copy(Place::local(local_id)), type_id)
            } else {
                let op = lower_expr(ctx, builder, scrutinee);
                let ty = infer_operand_type_full(ctx, &op, builder);
                (op, ty)
            }
        } else {
            let op = lower_expr(ctx, builder, scrutinee);
            let ty = infer_operand_type_full(ctx, &op, builder);
            (op, ty)
        }
    } else {
        let op = lower_expr(ctx, builder, scrutinee);
        let ty = infer_operand_type_full(ctx, &op, builder);
        (op, ty)
    };

    // Check if scrutinee is dead after the match (last use at match site) AND
    // the operand is a simple local we can MoveZero. If both, we can skip the
    // pattern extraction clone for string fields — the extracted field takes
    // ownership, and both the scrutinee copy AND original are zeroed.
    let scrutinee_dead_original = if let Expr::Identifier(name) = &scrutinee.node {
        if ctx.is_last_use_at(name, scrutinee.span) {
            if let Operand::Copy(ref place) | Operand::Move(ref place) = scrut_op {
                if place.projections.is_empty() {
                    // Exclude Ptr originals — they're borrowed from the caller,
                    // and the existing scrut_is_ptr check already skips cloning.
                    let orig_type = builder.local_type(place.local);
                    let is_ptr = matches!(ctx.type_registry.get(orig_type),
                        Some(GirType::Ptr(_) | GirType::MutPtr(_)));
                    if !is_ptr { Some(place.local) } else { None }
                } else { None }
            } else { None }
        } else { None }
    } else { None };

    let scrut_local = builder.add_local(scrut_type, None);
    builder.assign(Place::local(scrut_local), scrut_op.clone());

    // Propagate ownership from the source operand to the scrutinee temp.
    // This lets pattern extraction know whether the scrutinee data is owned
    // (safe to drop) vs borrowed (from .get().unwrap() etc.).
    if let Operand::Copy(ref place) | Operand::Move(ref place) = scrut_op {
        if place.projections.is_empty() && ctx.is_owned_local(place.local) {
            ctx.set_owned(scrut_local);
        }
    }

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
            let saved_arm = ctx.save_locals();
            ctx.drops.push_scope(DropScopeKind::Block);
            emit_pattern_bindings(ctx, builder, &arm.pattern, scrut_local, scrut_type);
            let guard_cond = lower_expr(ctx, builder, arm.guard.as_ref().unwrap());
            builder.branch(guard_cond, arm_body_bb, next_test_bb);

            builder.switch_to(arm_body_bb);
            // Re-emit bindings in the body block — the guard block's SSA values
            // aren't visible here (different basic block).
            emit_pattern_bindings(ctx, builder, &arm.pattern, scrut_local, scrut_type);
            lower_expr(ctx, builder, &arm.body);
            if builder.is_terminated() {
                // Return/break/continue already emitted early-exit drops — don't double-drop.
                ctx.drops.pop_scope_no_emit();
            } else {
                ctx.drops.pop_scope(builder, &ctx.type_registry);
                builder.jump(merge_bb);
            }
            ctx.restore_locals(saved_arm);
        } else {
            builder.branch(cond, arm_body_bb, next_test_bb);

            // Arm body (non-guarded — safe to elide pattern clone if scrutinee is dead)
            builder.switch_to(arm_body_bb);
            let saved_arm = ctx.save_locals();
            ctx.drops.push_scope(DropScopeKind::Block);
            ctx.func_state.scrutinee_clone_elision = scrutinee_dead_original.is_some();
            emit_pattern_bindings(ctx, builder, &arm.pattern, scrut_local, scrut_type);
            ctx.func_state.scrutinee_clone_elision = false;
            if let Some(original_local) = scrutinee_dead_original {
                // Always zero the original in every arm — arms are mutually
                // exclusive at runtime, so each arm must independently zero.
                // The is_moved flag must not gate this because it leaks across
                // arms at compile time (arm 1 marks moved, arm 2 skips zero).
                ctx.move_zero_and_mark(builder, original_local);
            }
            lower_expr(ctx, builder, &arm.body);
            if builder.is_terminated() {
                // Return/break/continue already emitted early-exit drops — don't double-drop.
                ctx.drops.pop_scope_no_emit();
            } else {
                ctx.drops.pop_scope(builder, &ctx.type_registry);
                builder.jump(merge_bb);
            }
            ctx.restore_locals(saved_arm);
        }

        builder.switch_to(next_test_bb);
    }

    // Else arm
    if let Some(else_body) = else_arm {
        let saved_else = ctx.save_locals();
        ctx.drops.push_scope(DropScopeKind::Block);
        lower_block(ctx, builder, else_body);
        if builder.is_terminated() {
            ctx.drops.pop_scope_no_emit();
        } else {
            ctx.drops.pop_scope(builder, &ctx.type_registry);
            builder.jump(merge_bb);
        }
        ctx.restore_locals(saved_else);
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

        Pattern::Constructor { path, fields } => {
            let variant_name = if let Some(last) = path.last() {
                last.node.clone()
            } else {
                return FunctionBuilder::const_bool(true);
            };
            // Qualified path (Color.Red): use first segment as enum name.
            // Bare variant (Some, None, Ok, Error): prefer scrutinee type name
            // to avoid ambiguity when multiple monomorphizations exist.
            let (enum_name, variant_name) = if path.len() >= 2 {
                (path[0].node.clone(), variant_name)
            } else {
                let en = ctx.type_registry.type_name(scrut_type)
                    .or_else(|| {
                        if let Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) = ctx.type_registry.get(scrut_type).cloned() {
                            ctx.type_registry.type_name(inner)
                        } else {
                            None
                        }
                    })
                    .or_else(|| ctx.resolve_enum_variant(&variant_name).map(|(en, _)| en));
                match en {
                    Some(en) => (en, variant_name),
                    None => return FunctionBuilder::const_bool(true),
                }
            };
            let tag = builder.tag_of(FunctionBuilder::copy(scrut_local));
            if let Some(variant_tag) = ctx.resolve_variant_tag(&enum_name, &variant_name) {
                let tag_cmp = builder.cmp(
                    CmpOp::Eq,
                    I32_TYPE,
                    FunctionBuilder::copy(tag),
                    Operand::Constant(Constant::I32(variant_tag as i32)),
                );

                // Check whether any sub-pattern needs a nested condition check (e.g.,
                // Outer.Wrap(Inner.A(n)) must also check Inner's discriminant).
                let has_nested = fields.iter().any(|fp|
                    !matches!(fp.node, Pattern::Binding(_) | Pattern::Wildcard | Pattern::Rest)
                );

                if !has_nested {
                    return FunctionBuilder::copy(tag_cmp);
                }

                // Short-circuit: only check inner patterns when the outer tag matches.
                // This avoids extracting fields from the wrong variant.
                let result_id = builder.add_local(BOOL_TYPE, None);
                let inner_bb = builder.new_block();
                let merge_bb = builder.new_block();
                let false_bb = builder.new_block();

                builder.branch(FunctionBuilder::copy(tag_cmp), inner_bb, false_bb);

                // false_bb: outer tag didn't match → result = false
                builder.switch_to(false_bb);
                builder.assign(Place::local(result_id), FunctionBuilder::const_bool(false));
                builder.jump(merge_bb);

                // inner_bb: outer tag matched → check nested sub-patterns
                builder.switch_to(inner_bb);
                let mut inner_result: Option<LocalId> = None;
                for (i, field_pat) in fields.iter().enumerate() {
                    if matches!(field_pat.node, Pattern::Binding(_) | Pattern::Wildcard | Pattern::Rest) {
                        continue;
                    }
                    let field_type = ctx.type_registry.get_type_def(&enum_name)
                        .and_then(|td| {
                            if let TypeDefKind::Enum(ref e) = td.kind {
                                e.variants.iter()
                                    .find(|v| v.name == variant_name)
                                    .and_then(|v| v.fields.get(i))
                                    .map(|f| f.type_id)
                            } else {
                                None
                            }
                        })
                        .unwrap_or(I64_TYPE);

                    let field_local = builder.enum_field_load(
                        Place::local(scrut_local),
                        variant_name.clone(),
                        i as u32,
                        field_type,
                    );
                    let sub_cond = lower_pattern_condition(
                        ctx, builder, field_pat, field_local, field_type,
                    );
                    inner_result = Some(match inner_result {
                        None => {
                            let tmp = builder.add_local(BOOL_TYPE, None);
                            builder.assign(Place::local(tmp), sub_cond);
                            tmp
                        }
                        Some(prev) => builder.bin_op(
                            BinOp::BitAnd,
                            BOOL_TYPE,
                            FunctionBuilder::copy(prev),
                            sub_cond,
                        ),
                    });
                }
                let final_inner = inner_result.map_or_else(
                    || FunctionBuilder::const_bool(true),
                    FunctionBuilder::copy,
                );
                builder.assign(Place::local(result_id), final_inner);
                builder.jump(merge_bb);

                builder.switch_to(merge_bb);
                return FunctionBuilder::copy(result_id);
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

            // Is the scrutinee a Ptr (borrowed)?
            let scrut_is_ptr = matches!(
                ctx.type_registry.get(scrut_type),
                Some(GirType::Ptr(_) | GirType::MutPtr(_))
            );

            for (i, field_pat) in fields.iter().enumerate() {
                // Determine the field type from the enum variant definition
                let mut field_type = if let Some(type_def) = ctx.type_registry.get_type_def(&enum_name) {
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

                // When scrutinee is Ptr (borrowed param), resource-type variant
                // fields should be references into the enum's storage, not
                // shallow copies. This ensures borrows derived from the field
                // (e.g., d.get(key) on a borrowed Dict) remain valid for the
                // lifetime of the borrowed scrutinee.
                // Box types are excluded — user code explicitly dereferences
                // them with `*a`, which requires a Box value, not a Ptr.
                if scrut_is_ptr && ctx.type_registry.is_resource_type(field_type) {
                    let is_box = ctx.type_registry.type_name(field_type)
                        .map_or(false, |n| n.starts_with("Box__"));
                    if !is_box {
                        field_type = ctx.type_registry.insert(GirType::Ptr(field_type));
                    }
                }

                let dst = builder.enum_field_load(
                    Place::local(scrut_local),
                    variant_name.clone(),
                    i as u32,
                    field_type,
                );

                // Mark Ptr-extracted locals as ref_locals (no auto-deref, no drop)
                if matches!(ctx.type_registry.get(field_type), Some(GirType::Ptr(_))) {
                    ctx.set_ref(dst);
                }
                // Value scrutinee + droppable field (string, collection, user
                // struct with resource fields): register for drop at scope exit.
                // Pattern extraction is a shallow memcpy — the binding and the
                // scrutinee share the same heap buffer.
                // When scrutinee_clone_elision is set, the scrutinee is dead and
                // both the scrutinee copy AND the original variable will be zeroed
                // after extraction — the shallow copy takes ownership directly.
                // For non-elided cases, the scrutinee is MoveZeroed after
                // extraction (line ~538), so the binding still takes ownership.
                // Strings/collections clone to get an independent buffer;
                // user structs take ownership directly (no clone needed).
                else if !scrut_is_ptr
                    && ctx.type_registry.needs_drop(field_type)
                {
                    let is_string_or_collection =
                        field_type == ctx.type_mapper.owned_string_type
                        || ctx.type_registry.is_collection_type(field_type);

                    if is_string_or_collection {
                        // String/collection: original behavior. Clone elision
                        // registers directly; otherwise clone for independence.
                        if ctx.func_state.scrutinee_clone_elision {
                            ctx.drops.register_local(dst, field_type, &ctx.type_registry);
                            ctx.set_owned(dst);
                        } else if let Some(clone_fn) = ctx.clone_fn_for_ptr(field_type) {
                            let ptr_type = ctx.register_ptr_type(field_type);
                            let ptr = builder.add_local(ptr_type, None);
                            builder.emit_borrow(ptr, Place::local(dst));
                            let cloned = builder.call(
                                &clone_fn,
                                vec![FunctionBuilder::copy(ptr)],
                                field_type,
                            );
                            builder.assign(Place::local(dst), FunctionBuilder::copy(cloned));
                            ctx.drops.register_local(dst, field_type, &ctx.type_registry);
                            ctx.set_owned(dst);
                        }
                    } else if ctx.is_owned_local(scrut_local) {
                        // User struct without clone_fn: register directly, but
                        // ONLY when the scrutinee owns its data. Borrowed
                        // scrutinees (from .get().unwrap() etc.) share data with
                        // the source — dropping would corrupt it. The scrutinee
                        // is MoveZeroed after extraction (line ~538), so the
                        // binding takes ownership of the fields.
                        ctx.drops.register_local(dst, field_type, &ctx.type_registry);
                        ctx.set_owned(dst);
                    }
                }

                // Recurse on sub-pattern
                emit_pattern_bindings(ctx, builder, field_pat, dst, field_type);
            }
            // Move semantics: zero the scrutinee after extracting all variant fields.
            // Prevents double-free when both extracted values and the scrutinee are dropped.
            // Match arms are exclusive — only one arm executes, so zeroing is safe.
            let has_resource_field = fields.iter().enumerate().any(|(i, _)| {
                if let Some(type_def) = ctx.type_registry.get_type_def(&enum_name) {
                    if let TypeDefKind::Enum(ref e) = type_def.kind {
                        if let Some(v) = e.variants.iter().find(|v| v.name == variant_name) {
                            if let Some(f) = v.fields.get(i) {
                                return ctx.type_registry.is_resource_type(f.type_id);
                            }
                        }
                    }
                }
                false
            });
            if has_resource_field {
                builder.move_zero(Place::local(scrut_local));
                ctx.drops.mark_moved(scrut_local);
            }
        }

        Pattern::Tuple(elems) => {
            for (i, elem_pat) in elems.iter().enumerate() {
                let elem_type = super::super::exprs::resolve_tuple_field_type(ctx, scrut_type, i);
                let mode = if ctx.type_registry.is_resource_type(elem_type) {
                    FieldLoadMode::MoveZeroSource
                } else {
                    FieldLoadMode::Copy
                };
                let dst = builder.field_load_mode(mode, Place::local(scrut_local), i as u32, elem_type);
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
