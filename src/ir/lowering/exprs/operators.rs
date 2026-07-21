//! Binary and unary operator lowering, including short-circuit logic.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast;
use crate::span::Spanned;

use super::super::context::LoweringContext;
use super::{
    emit_operator_overload_call, lower_expr, infer_operand_type_full, resolve_none_tag,
};

/// If an operand is a by-ref alias, emit a LoadRef to dereference it to the
/// underlying value. Two cases:
///   1. a CoW alias — `Ptr(T)` in ref_locals (bare-borrow propagation);
///   2. a `!`-move resource param slot — `MutPtr(T)` tagged `is_owning_param`.
///
/// Case 2 is why a `String !p` used as a binop operand must deref here. Unlike a
/// `&`-mut-borrow param (which auto-derefs at the identifier read, per
/// `is_param_borrow_unique`), a `!`-move resource param keeps `ownership = Owned`
/// (so the exit-drop frees the pointee exactly once — moving it to `Borrowed`
/// would route it through the leaky shared-mutation drop path). Its identifier
/// read therefore yields the raw `MutPtr` slot, and the binop is the consume site
/// that must shape it — mirroring the self-host, which detects the string by
/// unwrapping the ptr and borrows the operand at the concat/eq call
/// (`is_string_type_id` + `op_consume(CkCallArgBorrow)`). Without this deref a
/// `p + "..."` operand stays `MutPtr(String)`, `is_string` (exact
/// `== owned_string_type`) goes false, and the concat mis-lowers to integer
/// `(void*)a + (void*)b` — which cc/llc reject. The LoadRef is a shallow
/// borrow-read (no zero, no drop-registration), so `*p` stays owned and is
/// dropped once at function exit — no double-free, no leak.
///
/// Returns the dereferenced operand (type T), or the original if not by-ref.
fn cow_deref_if_ptr(
    ctx: &LoweringContext,
    builder: &mut FunctionBuilder,
    operand: Operand,
) -> Operand {
    if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
        let idx = place.local.0 as usize;
        let is_owning_param_slot = idx < builder.locals.len()
            && builder.locals[idx].is_owning_param;
        if place.projections.is_empty()
            && (ctx.is_ref_local(builder, place.local) || is_owning_param_slot)
        {
            let ptr_type = builder.local_type(place.local);
            if let Some(inner) = ctx.pointee_type(ptr_type) {
                let derefed = builder.load_ref(place.clone(), inner);
                return FunctionBuilder::copy(derefed);
            }
        }
    }
    operand
}

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

    // A binary op's OPERANDS have their own scalar/string types determined by
    // the operator, not the binop's destination. Clear the enclosing
    // `expected_type` so it doesn't leak into operand lowering. Without this,
    // a `return throws_call() + n` from a `throws` function sets
    // `expected_type = Result[T, E]` (the function's return slot, set by
    // `lower_return`), which suppresses `maybe_auto_propagate`'s peel on the
    // `throws_call()` operand — leaving a `Result[T, E] + int` mismatch at the
    // C layer. The throws-call operand must auto-prop like at any other
    // consumer position. (Mirror of the type-check side, where the
    // throws-fn-call peel is gated on `decl_type_hint`-is-Result, and a binop
    // operand is never bound to a Result destination directly.)
    let prev_expected = ctx.func_state.expected_type.take();
    let mut lhs = lower_expr(ctx, builder, left);
    let mut rhs = lower_expr(ctx, builder, right);
    ctx.func_state.expected_type = prev_expected;

    // CoW: if operands are Ptr(T) aliases, deref to get the underlying value.
    lhs = cow_deref_if_ptr(ctx, builder, lhs);
    rhs = cow_deref_if_ptr(ctx, builder, rhs);

    // Determine result type from lhs operand type (use _full to check builder temps too)
    let operand_type = infer_operand_type_full(ctx, &lhs, builder);
    let is_string = operand_type == ctx.type_mapper.owned_string_type;

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
                let dst = builder.call_extern("gorget_str_eq", vec![lhs, rhs], BOOL_TYPE);
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
                        let dst = emit_operator_overload_call(
                            ctx, builder, eq_method, vec![self_ptr, rhs], BOOL_TYPE,
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
                        let cmp_result = emit_operator_overload_call(
                            ctx, builder, effective_name, vec![self_ptr, rhs], I64_TYPE,
                        );
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

        // String concatenation: use gorget_str_cat (returns GorgetString, not Str).
        // `call_extern_tracked` already tags the result as fresh via the typed
        // `RuntimeSig.returns_fresh` flag on `RuntimeFn::StrCat`.
        AstOp::Add if is_string => {
            let owned_type = ctx.type_mapper.owned_string_type;
            // call_extern_tracked already upgrades the dst to FreshOwned via
            // is_fresh_allocating_extern — no extra mark needed here.
            let dst = ctx.call_extern_tracked(builder, "gorget_str_cat", vec![lhs, rhs], owned_type);
            FunctionBuilder::copy(dst)
        }

        // `in` operator → contains check
        AstOp::In => {
            // Determine collection type to use the right contains function.
            // Read typed `metadata.collection_kind` rather than name-prefix
            // probing — same answer, populated at every TypeDef registration
            // path via the protocol table.
            use crate::ir::types::CollectionKind;
            let rhs_type = infer_operand_type_full(ctx, &rhs, builder);
            let kind = ctx.type_registry.collection_kind(rhs_type);
            let is_map = matches!(kind, Some(CollectionKind::Map) | Some(CollectionKind::OrderedMap));
            let is_set = matches!(kind, Some(CollectionKind::Set) | Some(CollectionKind::OrderedSet));
            let is_string = ctx.type_mapper.is_string_type(rhs_type);
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
                        let dst = emit_operator_overload_call(
                            ctx, builder, effective_name, vec![self_ptr, rhs], operand_type,
                        );
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
            // Fault-`catch` routing (error-model.md §11.2): inside an active
            // fault scope, an integer faultable op (Add/Sub/Mul overflow,
            // Div/Rem div-by-zero) BRANCHES to the scope's handler on a fault
            // instead of panicking. Only ops emitted DIRECTLY into the wrapped
            // expression's own blocks are caught — a callee's or inline
            // closure's faults are lowered in a SEPARATE function pass whose
            // `func_state` (hence `fault_scope`) starts fresh, so they stay
            // deep and panic (the §11.5 basic-block reach). Typed gate (op kind
            // + integer type), never a name check.
            if let Some((overflow_handler, divzero_handler)) = fault_handler_for(ctx, operand_type, bin_op) {
                let dst = builder.bin_op_faultable(
                    bin_op, operand_type, lhs, rhs, overflow_handler, divzero_handler,
                );
                return FunctionBuilder::copy(dst);
            }
            let dst = builder.bin_op(bin_op, operand_type, lhs, rhs);
            FunctionBuilder::copy(dst)
        }
    }
}

/// Decide whether the binary op at the current site should be a fault-`catch`able
/// op, and if so return the per-category GIR handler blocks. Returns
/// `Some((overflow_handler, divzero_handler))` only when a fault scope is active
/// AND the op is one of the five integer faultable ops AND `operand_type` is an
/// integer. Add/Sub/Mul populate only the overflow handler; Div/Rem populate
/// BOTH (a single signed Div has two fault categories — `rhs == 0` → DivByZero
/// AND `TYPE_MIN/-1` → Overflow, error-model.md §11 Increment 2 (C) split). Each
/// returned slot is `Some` only when the scope catches that category; a slot
/// left `None` panics by default. `None` for the whole result → the op stays the
/// panic-by-default form (typed gate, never a name check).
fn fault_handler_for(
    ctx: &LoweringContext,
    operand_type: TypeId,
    op: BinOp,
) -> Option<(Option<crate::ir::types::BlockId>, Option<crate::ir::types::BlockId>)> {
    let scope = ctx.func_state.fault_scope?;
    // Integer types only (overflow/div0 are integer faults; floats don't trap).
    let is_integer = matches!(
        ctx.type_registry.get(operand_type),
        Some(GirType::I8) | Some(GirType::I16) | Some(GirType::I32) | Some(GirType::I64)
            | Some(GirType::U8) | Some(GirType::U16) | Some(GirType::U32) | Some(GirType::U64)
    );
    if !is_integer {
        return None;
    }
    match op {
        // Add/Sub/Mul: overflow only, and ONLY when the user catches it (else
        // the plain panic-by-default checked op — behaviour unchanged).
        BinOp::Add | BinOp::Sub | BinOp::Mul => {
            scope.overflow_handler.map(|h| (Some(h), None))
        }
        // Div/Rem: TWO categories, BOTH always handled at GIR — `TYPE_MIN/-1` →
        // user overflow entry OR the `"integer overflow"` panic block; `rhs == 0`
        // → user div0 entry OR the `"division by zero"` panic block. So the LIR
        // always branches (no LIR-level panic; uniform across both backends).
        BinOp::Div | BinOp::Rem => Some((
            Some(scope.overflow_handler.unwrap_or(scope.div_overflow_panic)),
            Some(scope.divzero_handler.unwrap_or(scope.div_zero_panic)),
        )),
        // Mod, bitwise, shifts, and the explicit `+%` wrap ops never fault.
        _ => None,
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
    // Auto-deref `Ref[bool] → bool` on both operands — matches the regular
    // binary-op path at lines 50-51. Without this, an inline method-chain
    // like `v.get(i).unwrap()` produces a `Ptr(bool)` whose non-null bits
    // get assigned into a `bool` slot, and `branch` reads the pointer's
    // bit pattern (always non-null when Some) instead of the bool payload.
    // Discovered 2026-05-21 while debugging self-host's wire_liveness pass
    // emitting zero OpMoves; see tests/fixtures/compound_and_method_chain_miscompile.gg.
    let lhs = lower_expr(ctx, builder, left);
    let lhs = cow_deref_if_ptr(ctx, builder, lhs);

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
            let rhs = cow_deref_if_ptr(ctx, builder, rhs);
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
            let rhs = cow_deref_if_ptr(ctx, builder, rhs);
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
    let mut val = lower_expr(ctx, builder, operand);
    // Mirror binary-op auto-deref: if operand is a `Ref[T]` (Ptr) from a
    // `.get(i).unwrap()` or borrowed param, deref before applying the unary
    // op. Without this, `-x` on a `Ref[float]` C-emits `-(void*)x`.
    val = cow_deref_if_ptr(ctx, builder, val);
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
                let dst = emit_operator_overload_call(
                    ctx, builder, effective_name, vec![self_ptr], operand_type,
                );
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
