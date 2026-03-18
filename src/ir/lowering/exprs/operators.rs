//! Binary and unary operator lowering, including short-circuit logic.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast;
use crate::span::Spanned;

use super::super::context::LoweringContext;
use super::{lower_expr, infer_operand_type_full, resolve_none_tag};

/// Lower a binary operation.
pub(super) fn lower_binary_op(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    left: &Spanned<ast::Expr>,
    op: ast::BinaryOp,
    right: &Spanned<ast::Expr>,
) -> Operand {
    use ast::BinaryOp as AstOp;

    // Short-circuit: `and` / `or`
    if op == AstOp::And || op == AstOp::Or {
        return lower_short_circuit(ctx, builder, left, op, right);
    }

    let lhs = lower_expr(ctx, builder, left);
    let rhs = lower_expr(ctx, builder, right);

    // Determine result type from lhs operand type (use _full to check builder temps too)
    let operand_type = infer_operand_type_full(ctx, &lhs, builder);
    let is_string = operand_type == ctx.type_mapper.str_type
        || operand_type == ctx.type_mapper.owned_string_type;

    match op {
        // Comparison operators → bool result
        AstOp::Eq | AstOp::Neq | AstOp::Lt | AstOp::Gt | AstOp::LtEq | AstOp::GtEq => {
            // Option/enum == None: compare tag instead of struct == NULL
            if matches!(op, AstOp::Eq | AstOp::Neq) {
                let (enum_operand, is_none) = match (&lhs, &rhs) {
                    (_, Operand::Constant(Constant::Null)) => (Some(&lhs), true),
                    (Operand::Constant(Constant::Null), _) => (Some(&rhs), true),
                    _ => (None, false),
                };
                if is_none {
                    if let Some(enum_op) = enum_operand {
                        // Get the tag of the enum value
                        let tag = builder.tag_of(enum_op.clone());
                        // None is conventionally the last variant (tag = num_variants - 1)
                        // For Option, None = tag 1; for most enums, last variant
                        // Look up variant count from type registry
                        let none_tag = resolve_none_tag(ctx, operand_type);
                        let cmp_op = if op == AstOp::Eq { CmpOp::Eq } else { CmpOp::Ne };
                        let dst = builder.cmp(
                            cmp_op,
                            I32_TYPE,
                            FunctionBuilder::copy(tag),
                            Operand::Constant(Constant::I32(none_tag)),
                        );
                        return FunctionBuilder::copy(dst);
                    }
                }
            }

            // String equality: use gorget_str_eq instead of pointer comparison
            if is_string && matches!(op, AstOp::Eq | AstOp::Neq) {
                let str_type = ctx.type_mapper.str_type;
                let dst = builder.call_extern("gorget_str_eq", vec![lhs, rhs], BOOL_TYPE);
                let _ = str_type;
                if op == AstOp::Neq {
                    let neg = builder.un_op(UnOp::Not, BOOL_TYPE, FunctionBuilder::copy(dst));
                    return FunctionBuilder::copy(neg);
                }
                return FunctionBuilder::copy(dst);
            }

            // Struct == / != : dispatch to Type__eq() if available
            if matches!(op, AstOp::Eq | AstOp::Neq) {
                if let Some(GirType::Named(type_name)) = ctx.type_registry.get(operand_type).cloned() {
                    let eq_method = format!("{type_name}__eq");
                    if ctx.fn_sigs.contains_key(&eq_method) {
                        // Borrow lhs for self parameter
                        let self_ptr = if let Operand::Copy(ref place) | Operand::Move(ref place) = lhs {
                            let ptr_type = ctx.register_ptr_type(operand_type);
                            let ptr_local = builder.add_local(ptr_type, None);
                            builder.emit_borrow(ptr_local, place.clone());
                            FunctionBuilder::copy(ptr_local)
                        } else {
                            lhs
                        };
                        let dst = builder.call(
                            eq_method,
                            vec![self_ptr, rhs],
                            BOOL_TYPE,
                        );
                        if op == AstOp::Neq {
                            let neg = builder.un_op(UnOp::Not, BOOL_TYPE, FunctionBuilder::copy(dst));
                            return FunctionBuilder::copy(neg);
                        }
                        return FunctionBuilder::copy(dst);
                    }
                }
            }

            // Comparable trait dispatch: Type__compare(self, other) → int, then compare with 0
            if matches!(op, AstOp::Lt | AstOp::Gt | AstOp::LtEq | AstOp::GtEq) {
                if let Some(GirType::Named(type_name)) = ctx.type_registry.get(operand_type).cloned() {
                    let compare_method = format!("{type_name}__compare");
                    let has_compare = ctx.fn_sigs.contains_key(&compare_method)
                        || ctx.fn_sigs.keys().any(|k| k.ends_with(&format!("_for_{type_name}__compare")));
                    if has_compare {
                        let effective_name = if ctx.fn_sigs.contains_key(&compare_method) {
                            compare_method
                        } else {
                            ctx.fn_sigs.keys()
                                .find(|k| k.ends_with(&format!("_for_{type_name}__compare")))
                                .cloned()
                                .unwrap_or(compare_method)
                        };
                        let self_ptr = if let Operand::Copy(ref place) | Operand::Move(ref place) = lhs {
                            let ptr_type = ctx.register_ptr_type(operand_type);
                            let ptr_local = builder.add_local(ptr_type, None);
                            builder.emit_borrow(ptr_local, place.clone());
                            FunctionBuilder::copy(ptr_local)
                        } else {
                            lhs.clone()
                        };
                        let cmp_result = builder.call(effective_name, vec![self_ptr, rhs], I64_TYPE);
                        let cmp_op = match op {
                            AstOp::Lt => CmpOp::Lt,
                            AstOp::Gt => CmpOp::Gt,
                            AstOp::LtEq => CmpOp::Le,
                            AstOp::GtEq => CmpOp::Ge,
                            _ => unreachable!(),
                        };
                        let dst = builder.cmp(cmp_op, I64_TYPE, FunctionBuilder::copy(cmp_result), Operand::Constant(Constant::I64(0)));
                        return FunctionBuilder::copy(dst);
                    }
                }
            }

            let cmp_op = match op {
                AstOp::Eq => CmpOp::Eq,
                AstOp::Neq => CmpOp::Ne,
                AstOp::Lt => CmpOp::Lt,
                AstOp::Gt => CmpOp::Gt,
                AstOp::LtEq => CmpOp::Le,
                AstOp::GtEq => CmpOp::Ge,
                _ => unreachable!(),
            };
            let dst = builder.cmp(cmp_op, operand_type, lhs, rhs);
            FunctionBuilder::copy(dst)
        }

        // String concatenation: use gorget_str_cat (returns GorgetString, not Str)
        AstOp::Add if is_string => {
            let owned_type = ctx.type_mapper.owned_string_type;
            let dst = builder.call_extern("gorget_str_cat", vec![lhs, rhs], owned_type);
            super::register_owned_string_for_drop(ctx, dst);
            FunctionBuilder::copy(dst)
        }

        // `in` operator → contains check
        AstOp::In => {
            // Determine collection type to use the right contains function
            let rhs_type = infer_operand_type_full(ctx, &rhs, builder);
            let rhs_type_name = ctx.type_name_for_id(rhs_type)
                .map(|s| s.to_string())
                .unwrap_or_default();
            let is_map = rhs_type_name.starts_with("Dict__") || rhs_type_name.starts_with("HashMap__");
            let is_set = rhs_type_name.starts_with("Set__") || rhs_type_name.starts_with("HashSet__");
            let is_string = rhs_type == ctx.type_mapper.str_type || rhs_type == ctx.type_mapper.owned_string_type;
            if is_map || is_set {
                // Map/Set contains: need pointer to collection and pointer to element
                let fn_name = if is_map { "gorget_map_contains" } else { "gorget_set_contains" };
                let coll_ptr_type = ctx.register_ptr_type(rhs_type);
                let coll_ptr = builder.add_local(coll_ptr_type, None);
                let rhs_local = match &rhs {
                    Operand::Copy(p) | Operand::Move(p) => p.local,
                    _ => {
                        let tmp = builder.add_local(rhs_type, None);
                        builder.assign(Place::local(tmp), rhs);
                        tmp
                    }
                };
                builder.emit_borrow(coll_ptr, Place::local(rhs_local));
                // Element also needs to be a pointer
                let lhs_type = infer_operand_type_full(ctx, &lhs, builder);
                let elem_local = builder.add_local(lhs_type, None);
                builder.assign(Place::local(elem_local), lhs);
                let elem_ptr_type = ctx.register_ptr_type(lhs_type);
                let elem_ptr = builder.add_local(elem_ptr_type, None);
                builder.emit_borrow(elem_ptr, Place::local(elem_local));
                let dst = builder.call_extern(fn_name, vec![FunctionBuilder::copy(coll_ptr), FunctionBuilder::copy(elem_ptr)], BOOL_TYPE);
                FunctionBuilder::copy(dst)
            } else if is_string {
                let dst = builder.call_extern("gorget_str_contains", vec![rhs, lhs], BOOL_TYPE);
                FunctionBuilder::copy(dst)
            } else {
                let dst = builder.call_extern("gorget_array_contains", vec![rhs, lhs], BOOL_TYPE);
                FunctionBuilder::copy(dst)
            }
        }

        // Arithmetic operators → check for operator overloading, then fall back to primitives
        _ => {
            // Check for operator overload method on Named types
            let op_method_name = match op {
                AstOp::Add => Some("add"),
                AstOp::Sub => Some("sub"),
                AstOp::Mul => Some("mul"),
                AstOp::Div => Some("div"),
                AstOp::Rem => Some("rem"),
                AstOp::Mod => Some("mod"),
                _ => None,
            };
            if let Some(method) = op_method_name {
                if let Some(GirType::Named(type_name)) = ctx.type_registry.get(operand_type).cloned() {
                    let mangled = format!("{type_name}__{method}");
                    let has_method = ctx.fn_sigs.contains_key(&mangled)
                        || ctx.fn_sigs.keys().any(|k| k.ends_with(&format!("_for_{type_name}__{method}")));
                    if has_method {
                        let effective_name = if ctx.fn_sigs.contains_key(&mangled) {
                            mangled
                        } else {
                            ctx.fn_sigs.keys()
                                .find(|k| k.ends_with(&format!("_for_{type_name}__{method}")))
                                .cloned()
                                .unwrap_or(mangled)
                        };
                        // Borrow lhs for self parameter
                        let self_ptr = if let Operand::Copy(ref place) | Operand::Move(ref place) = lhs {
                            let ptr_type = ctx.register_ptr_type(operand_type);
                            let ptr_local = builder.add_local(ptr_type, None);
                            builder.emit_borrow(ptr_local, place.clone());
                            FunctionBuilder::copy(ptr_local)
                        } else {
                            lhs.clone()
                        };
                        let dst = builder.call(effective_name, vec![self_ptr, rhs], operand_type);
                        return FunctionBuilder::copy(dst);
                    }
                }
            }

            let bin_op = match op {
                AstOp::Add => BinOp::Add,
                AstOp::Sub => BinOp::Sub,
                AstOp::Mul => BinOp::Mul,
                AstOp::Div => BinOp::Div,
                AstOp::Rem => BinOp::Rem,
                AstOp::Mod => BinOp::Mod,
                AstOp::BitAnd => BinOp::BitAnd,
                AstOp::BitOr => BinOp::BitOr,
                AstOp::BitXor => BinOp::BitXor,
                AstOp::Shl => BinOp::Shl,
                AstOp::Shr => BinOp::Shr,
                AstOp::AddWrap => BinOp::AddWrap,
                AstOp::SubWrap => BinOp::SubWrap,
                AstOp::MulWrap => BinOp::MulWrap,
                _ => BinOp::Add, // fallback
            };
            let dst = builder.bin_op(bin_op, operand_type, lhs, rhs);
            FunctionBuilder::copy(dst)
        }
    }
}

/// Lower short-circuit `and`/`or` via basic block branching.
fn lower_short_circuit(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    left: &Spanned<ast::Expr>,
    op: ast::BinaryOp,
    right: &Spanned<ast::Expr>,
) -> Operand {
    let lhs = lower_expr(ctx, builder, left);

    // Allocate a result local
    let result_id = builder.add_local(BOOL_TYPE, None);

    let rhs_bb = builder.new_block();
    let merge_bb = builder.new_block();

    match op {
        ast::BinaryOp::And => {
            let false_bb = builder.new_block();
            builder.branch(lhs, rhs_bb, false_bb);

            // false_bb: assign false, jump merge
            builder.switch_to(false_bb);
            builder.assign(Place::local(result_id), FunctionBuilder::const_bool(false));
            builder.jump(merge_bb);

            // rhs_bb: evaluate rhs, assign to result, jump merge
            builder.switch_to(rhs_bb);
            // If lhs was an `Is` expression, emit pattern bindings now (in the
            // true-branch, before the guard/rhs is evaluated).
            super::super::stmts::emit_is_bindings(ctx, builder, left);
            let rhs = lower_expr(ctx, builder, right);
            builder.assign(Place::local(result_id), rhs);
            builder.jump(merge_bb);
        }
        ast::BinaryOp::Or => {
            let true_bb = builder.new_block();
            builder.branch(lhs, true_bb, rhs_bb);

            // true_bb: assign true, jump merge
            builder.switch_to(true_bb);
            builder.assign(Place::local(result_id), FunctionBuilder::const_bool(true));
            builder.jump(merge_bb);

            // rhs_bb: evaluate rhs, assign to result, jump merge
            builder.switch_to(rhs_bb);
            let rhs = lower_expr(ctx, builder, right);
            builder.assign(Place::local(result_id), rhs);
            builder.jump(merge_bb);
        }
        _ => unreachable!(),
    }

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_id)
}

/// Lower a unary operation.
pub(super) fn lower_unary_op(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    op: ast::UnaryOp,
    operand: &Spanned<ast::Expr>,
) -> Operand {
    let val = lower_expr(ctx, builder, operand);
    let operand_type = infer_operand_type_full(ctx, &val, builder);

    // Check for unary operator overload (e.g., `-v` → Vec2__neg)
    if matches!(op, ast::UnaryOp::Neg) {
        if let Some(GirType::Named(type_name)) = ctx.type_registry.get(operand_type).cloned() {
            let mangled = format!("{type_name}__neg");
            let has_method = ctx.fn_sigs.contains_key(&mangled)
                || ctx.fn_sigs.keys().any(|k| k.ends_with(&format!("_for_{type_name}__neg")));
            if has_method {
                let effective_name = if ctx.fn_sigs.contains_key(&mangled) {
                    mangled
                } else {
                    ctx.fn_sigs.keys()
                        .find(|k| k.ends_with(&format!("_for_{type_name}__neg")))
                        .cloned()
                        .unwrap_or(mangled)
                };
                let self_ptr = if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                    let ptr_type = ctx.register_ptr_type(operand_type);
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow(ptr_local, place.clone());
                    FunctionBuilder::copy(ptr_local)
                } else {
                    val.clone()
                };
                let dst = builder.call(effective_name, vec![self_ptr], operand_type);
                return FunctionBuilder::copy(dst);
            }
        }
    }

    let gir_op = match op {
        ast::UnaryOp::Neg => UnOp::Neg,
        ast::UnaryOp::Not => UnOp::Not,
        ast::UnaryOp::BitNot => UnOp::BitNot,
    };

    let result_type = if gir_op == UnOp::Not { BOOL_TYPE } else { operand_type };
    let dst = builder.un_op(gir_op, result_type, val);
    FunctionBuilder::copy(dst)
}
