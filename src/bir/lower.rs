//! LIR → BIR lowering pass.
//!
//! Expands LIR's canonical-level high-level ops into sequences of primitive
//! instructions that backends can emit directly. Each canonical op listed in
//! `docs/internals/lir-backend-lift-plan.md` gets an expansion here as the
//! corresponding LIR variant is added.
//!
//! ## Current canonical ops expanded
//!
//! - `SizeOf { dst, ty }` → `IConst { dst, ty: I64, value: N }` via the shared
//!   `c_sizeof_lir_type` table.
//! - `EnumInit { target, struct_id, variant_tag, variant_idx, payload }` →
//!   `FieldPtr`(tag) + `IConst`(tag) + `Store` + optional `FieldPtr`(payload) +
//!   `Store`/`Memcpy` (depending on payload type).
//! - `EnumCheck { dst, value, struct_id, variant_tag }` → `FieldPtr`(tag) +
//!   `Load` + `IConst` + `Cmp`.
//! - `EnumExtract { dst, value, struct_id, payload_field, ty }` →
//!   `FieldPtr`(payload) + `Load`.
//!
//! Future canonical ops (`StructInit`, `NamedFieldPtr`, `HofExpand`, …) get
//! expanded here in subsequent steps.

use crate::bir::BirError;
use crate::lir::lower::types::c_sizeof_lir_type;
use crate::lir::{
    BlockId, CmpOp, HofOp, Inst, LirFunction, LirModule, LirType, Overflow, StructDef, StructId,
    Term, ValueId,
};

/// Expand all canonical-level ops in `module` into primitive instructions.
///
/// Returns the rewritten module. The caller (typically
/// [`crate::bir::BirModule::from_lir`]) then runs the validator to confirm
/// the invariant holds.
pub fn lower_lir_to_bir(mut module: LirModule) -> Result<LirModule, BirError> {
    // Swap `functions` out so we can iterate them mutably while holding an
    // immutable reference to `module.structs`. The structs table is the only
    // piece of module metadata the expansion reads (for c_sizeof / payload
    // type lookups); swapping functions is O(1), cloning structs would be O(N).
    let mut funcs = std::mem::take(&mut module.functions);
    for func in funcs.iter_mut() {
        expand_func(func, &module.structs);
    }
    module.functions = funcs;
    Ok(module)
}

fn expand_func(func: &mut LirFunction, structs: &[StructDef]) {
    // Fast path: skip the entire rebuild if no canonical ops are present.
    // Walking insts without cloning is O(n); rebuilding is O(n) plus allocation.
    if !func_needs_expansion(func) {
        return;
    }

    // We borrow `func` mutably to rewrite each block's inst list, so we
    // can't simultaneously call `func.next_value()`. Shadow the ValueId
    // counter locally and write it back when we're done.
    let mut next = func.next_value_raw();

    // Index-based iteration rather than `func.blocks.iter_mut()` — some
    // expansions (AddressOf) need to allocate fresh stack slots via
    // `func.add_slot(...)`, which requires a mutable borrow of `func.slots`
    // that would conflict with an outstanding `&mut block` borrow.
    //
    // `while bb_idx < func.blocks.len()` (not `for bb_idx in 0..block_count`)
    // so that blocks appended by HOF expansion (check/body/done) get processed
    // on subsequent iterations — a `done` block may contain nested HofExpands.
    let mut bb_idx = 0;
    while bb_idx < func.blocks.len() {
        let old = std::mem::take(&mut func.blocks[bb_idx].insts);
        let mut new_insts: Vec<Inst> = Vec::with_capacity(old.len());
        let mut iter = old.into_iter();
        let mut hof_split = false;
        while let Some(inst) = iter.next() {
            match inst {
                Inst::SizeOf { dst, ty } => {
                    let value = c_sizeof_lir_type(&ty, structs) as i64;
                    new_insts.push(Inst::IConst { dst, ty: LirType::I64, value });
                }
                Inst::EnumInit { target, struct_id, variant_tag, fields } => {
                    // 1) Write tag field (field 0).
                    let tag_ptr = alloc_value(&mut next);
                    new_insts.push(Inst::FieldPtr {
                        dst: tag_ptr,
                        base: target,
                        struct_id,
                        field: 0,
                    });
                    let tag_val = alloc_value(&mut next);
                    new_insts.push(Inst::IConst {
                        dst: tag_val,
                        ty: LirType::I32,
                        value: variant_tag as i64,
                    });
                    new_insts.push(Inst::Store { ptr: tag_ptr, value: tag_val });

                    // 2) Write each payload field via a plain `Store`. Each
                    //    backend's Store handler already dispatches on
                    //    `val_types` to pick scalar-store vs aggregate-memcpy —
                    //    same mechanism `StructInit` relies on. EnumInit's
                    //    payload values come from heterogeneous sources
                    //    (`lower_operand` returns a scalar or aggregate value;
                    //    wrap helpers pass pointers from slot-addr); letting
                    //    the backend Store decide keeps the single source of
                    //    truth at the val_types layer.
                    for (field_idx, payload_val) in fields {
                        let payload_ptr = alloc_value(&mut next);
                        new_insts.push(Inst::FieldPtr {
                            dst: payload_ptr,
                            base: target,
                            struct_id,
                            field: field_idx,
                        });
                        new_insts.push(Inst::Store { ptr: payload_ptr, value: payload_val });
                    }
                }
                Inst::EnumCheck { dst, value, struct_id, variant_tag } => {
                    let tag_ptr = alloc_value(&mut next);
                    new_insts.push(Inst::FieldPtr {
                        dst: tag_ptr,
                        base: value,
                        struct_id,
                        field: 0,
                    });
                    let tag = alloc_value(&mut next);
                    new_insts.push(Inst::Load { dst: tag, ptr: tag_ptr, ty: LirType::I32 });
                    let expected = alloc_value(&mut next);
                    new_insts.push(Inst::IConst {
                        dst: expected,
                        ty: LirType::I32,
                        value: variant_tag as i64,
                    });
                    new_insts.push(Inst::Cmp {
                        dst,
                        op: crate::lir::CmpOp::Eq,
                        lhs: tag,
                        rhs: expected,
                    });
                }
                Inst::EnumExtract { dst, value, struct_id, payload_field, ty } => {
                    let payload_ptr = alloc_value(&mut next);
                    new_insts.push(Inst::FieldPtr {
                        dst: payload_ptr,
                        base: value,
                        struct_id,
                        field: payload_field,
                    });
                    new_insts.push(Inst::Load { dst, ptr: payload_ptr, ty });
                }
                Inst::StructInit { target, struct_id, fields } => {
                    // StructInit's field values come from `lower_operand` in GIR →
                    // LIR, which may yield a scalar value, an aggregate value, or a
                    // pointer to an aggregate. Emit a plain `Store` and let each
                    // backend's Store handler dispatch on val_types — the pre-op
                    // behaviour before StructInit was introduced.
                    for (field_idx, value) in fields {
                        let fptr = alloc_value(&mut next);
                        new_insts.push(Inst::FieldPtr {
                            dst: fptr,
                            base: target,
                            struct_id,
                            field: field_idx,
                        });
                        new_insts.push(Inst::Store { ptr: fptr, value });
                    }
                }
                Inst::NamedFieldPtr { dst, base, struct_name, field_name } => {
                    // Resolve via the opaque runtime layout table. If the struct
                    // has no known layout, fall through to looking up field index
                    // in the module's struct registry by field name.
                    let field_idx = lookup_named_field_index(&struct_name, &field_name, structs);
                    let struct_id = structs
                        .iter()
                        .position(|s| s.name == struct_name)
                        .map(|i| crate::lir::StructId(i as u32))
                        .unwrap_or(crate::lir::StructId(0));
                    new_insts.push(Inst::FieldPtr {
                        dst,
                        base,
                        struct_id,
                        field: field_idx,
                    });
                }
                Inst::CowClone { dst, src, ty } => {
                    // For strings: call gorget_string_copy_cow(&out, src).
                    // For now, only String is supported — other CoW types can
                    // extend the name resolution here.
                    let call_name = match &ty {
                        LirType::Struct(sid) => structs
                            .get(sid.0 as usize)
                            .map(|s| match s.name.as_str() {
                                "GorgetString" | "Str" => "gorget_string_copy_cow",
                                _ => "gorget_string_copy_cow",
                            })
                            .unwrap_or("gorget_string_copy_cow"),
                        _ => "gorget_string_copy_cow",
                    };
                    new_insts.push(Inst::CallExtern {
                        dst: Some(dst),
                        name: call_name.to_string(),
                        args: vec![src],
                        original_name: None,
                        arg_abis: vec![crate::ir::abi::AbiKind::Ptr],
                    });
                }
                Inst::AddressOf { dst, value, ty } => {
                    // Spill the SSA value into a fresh stack slot, then take
                    // its address. `dst` is the final address; `SlotAddr`
                    // binds it directly. For aggregate values arriving from
                    // elsewhere as pointers, the C/LLVM Store handlers
                    // dispatch on val_types — same mechanism StructInit/
                    // EnumInit rely on — so we don't need to special-case.
                    let slot = func.add_slot(ty.clone(), None);
                    new_insts.push(Inst::SlotStore {
                        slot,
                        value,
                        is_move: false,
                    });
                    new_insts.push(Inst::SlotAddr { dst, slot });
                }
                Inst::HofExpand {
                    coll,
                    hof_op,
                    element_ty,
                    value_ty: _,
                    closure,
                    closure_kind,
                    closure_ret_ty,
                    closure_arg_abis,
                    dst,
                    init,
                } => {
                    match hof_op {
                        HofOp::Each | HofOp::Any | HofOp::All | HofOp::Fold => {
                            // Capture the tail of the block; it moves to done_bb.
                            let remaining: Vec<Inst> = iter.by_ref().collect();
                            let orig_term = std::mem::replace(
                                &mut func.blocks[bb_idx].terminator,
                                Term::Unreachable,
                            );
                            func.blocks[bb_idx].insts = std::mem::take(&mut new_insts);
                            match hof_op {
                                HofOp::Each => expand_each(
                                    func,
                                    bb_idx,
                                    &mut next,
                                    structs,
                                    coll,
                                    element_ty,
                                    closure,
                                    closure_arg_abis,
                                    remaining,
                                    orig_term,
                                ),
                                HofOp::Any | HofOp::All => expand_any_all(
                                    func,
                                    bb_idx,
                                    &mut next,
                                    structs,
                                    hof_op,
                                    coll,
                                    element_ty,
                                    closure,
                                    closure_arg_abis,
                                    dst.expect("any/all must carry a dst ValueId"),
                                    remaining,
                                    orig_term,
                                ),
                                HofOp::Fold => expand_fold(
                                    func,
                                    bb_idx,
                                    &mut next,
                                    structs,
                                    coll,
                                    element_ty,
                                    closure,
                                    closure_arg_abis,
                                    closure_ret_ty,
                                    init.expect("fold must carry an init ValueId"),
                                    dst.expect("fold must carry a dst ValueId"),
                                    remaining,
                                    orig_term,
                                ),
                                _ => unreachable!(),
                            }
                            hof_split = true;
                            break;
                        }
                        _ => {
                            // Not yet migrated — keep as-is. If it ever
                            // leaks into BIR the validator will flag it.
                            new_insts.push(Inst::HofExpand {
                                coll,
                                hof_op,
                                element_ty,
                                value_ty: None,
                                closure,
                                closure_kind,
                                closure_ret_ty,
                                closure_arg_abis,
                                dst,
                                init,
                            });
                        }
                    }
                }
                Inst::BoxAlloc { dst, inner_ty, value } => {
                    // 1) size = sizeof(inner_ty)
                    let sz = c_sizeof_lir_type(&inner_ty, structs) as i64;
                    let size_val = alloc_value(&mut next);
                    new_insts.push(Inst::IConst {
                        dst: size_val,
                        ty: LirType::I64,
                        value: sz,
                    });
                    // 2) dst = __gorget_alloc(size) — dst is the heap ptr.
                    new_insts.push(Inst::CallExtern {
                        dst: Some(dst),
                        name: "__gorget_alloc".to_string(),
                        args: vec![size_val],
                        original_name: None,
                        arg_abis: vec![crate::ir::abi::AbiKind::Scalar],
                    });
                    // 3) Write the value into *dst. Plain `Store` lets each
                    //    backend dispatch on val_types (scalar vs aggregate
                    //    vs Ptr-to-aggregate), matching StructInit/EnumInit.
                    new_insts.push(Inst::Store { ptr: dst, value });
                }
                other => new_insts.push(other),
            }
        }
        if !hof_split {
            func.blocks[bb_idx].insts = new_insts;
        }
        bb_idx += 1;
    }

    func.set_next_value_raw(next);
}

/// Shared scaffold for the Vector HOF loop skeletons.
///
/// Emits the `check_bb` / `body_bb` / `done_bb` triple, wires the entry
/// jump, puts the length check into `check_bb`, and does the per-element
/// `ElemPtr` + (optional) `Load` in `body_bb` up to — but not including —
/// the `CallClosure`. Variants append the call plus their final
/// terminators.
///
/// `extra_check_inits` adds additional block params to `check_bb` (for
/// accumulators). The entry jump passes each init value alongside the
/// counter; variants fetch the matching ValueId out of
/// `ctx.extra_check_params` to read the accumulator inside `body_bb` and
/// to pass updates on the back-edge jump.
///
/// The `check_bb` and `body_bb` terminators are NOT set here — variants
/// do that to match their semantics (any/all early-exit, fold accumulator
/// pass-through, etc.).
#[allow(dead_code)]
struct HofLoopCtx {
    check_bb: BlockId,
    body_bb: BlockId,
    done_bb: BlockId,
    cond: ValueId,
    /// Loop-counter block param on `check_bb`. Currently only read by
    /// variants that thread additional state; kept in the ctx for future
    /// HOF lowerings that need it.
    i_val: ValueId,
    next_i: ValueId,
    /// The per-element arg the variant will feed `CallClosure`. For
    /// pointer-ABI closures this is a pointer to the element; otherwise
    /// the element has been `Load`-ed and this is the scalar/struct value.
    elem_arg: ValueId,
    /// ABI tag matching `elem_arg`.
    elem_abi: crate::ir::abi::AbiKind,
    /// Block-param ValueIds of each entry in `extra_check_inits`, in order.
    extra_check_params: Vec<ValueId>,
}

#[allow(clippy::too_many_arguments)]
fn emit_hof_loop_scaffold(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    element_ty: &LirType,
    elem_abi_hint: Option<crate::ir::abi::AbiKind>,
    extra_check_inits: Vec<(LirType, ValueId)>,
) -> HofLoopCtx {
    let gorget_array_sid = lookup_struct_id(structs, "GorgetArray")
        .unwrap_or(StructId(0));

    let check_bb = func.add_block();
    let body_bb = func.add_block();
    let done_bb = func.add_block();

    // check_bb block params: (i, extras...).
    let i_val = alloc_value(next);
    func.block_mut(check_bb).params.push((i_val, LirType::I64));
    let mut extra_check_params: Vec<ValueId> = Vec::with_capacity(extra_check_inits.len());
    for (ty, _) in &extra_check_inits {
        let p = alloc_value(next);
        func.block_mut(check_bb).params.push((p, ty.clone()));
        extra_check_params.push(p);
    }

    // current_bb → check_bb(0, init0, init1, ...)
    let zero = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32)).insts.push(Inst::IConst {
        dst: zero,
        ty: LirType::I64,
        value: 0,
    });
    let mut entry_args = Vec::with_capacity(1 + extra_check_inits.len());
    entry_args.push(zero);
    for (_, init) in &extra_check_inits {
        entry_args.push(*init);
    }
    func.block_mut(BlockId(current_bb as u32)).terminator =
        Term::Jump(check_bb, entry_args);

    // check_bb: load GorgetArray.len and compare.
    let lenp = alloc_value(next);
    func.block_mut(check_bb).insts.push(Inst::FieldPtr {
        dst: lenp,
        base: coll,
        struct_id: gorget_array_sid,
        field: 2, // GorgetArray.len
    });
    let len = alloc_value(next);
    func.block_mut(check_bb).insts.push(Inst::Load {
        dst: len,
        ptr: lenp,
        ty: LirType::I64,
    });
    let cond = alloc_value(next);
    func.block_mut(check_bb).insts.push(Inst::Cmp {
        dst: cond,
        op: CmpOp::Lt,
        lhs: i_val,
        rhs: len,
    });
    // Caller sets `check_bb.terminator`.

    // body_bb: get data ptr and element ptr.
    let datap_ptr = alloc_value(next);
    func.block_mut(body_bb).insts.push(Inst::FieldPtr {
        dst: datap_ptr,
        base: coll,
        struct_id: gorget_array_sid,
        field: 0, // GorgetArray.data
    });
    let datap = alloc_value(next);
    func.block_mut(body_bb).insts.push(Inst::Load {
        dst: datap,
        ptr: datap_ptr,
        ty: LirType::Ptr,
    });
    let elem_size = c_sizeof_lir_type(element_ty, structs) as u32;
    let elemp = alloc_value(next);
    func.block_mut(body_bb).insts.push(Inst::ElemPtr {
        dst: elemp,
        base: datap,
        index: i_val,
        elem_size,
    });

    // Element ABI: emitter tag wins when present, otherwise fall back to
    // `element_ty.is_aggregate()` (aggregates travel by pointer).
    let pass_by_ptr = match elem_abi_hint {
        Some(crate::ir::abi::AbiKind::Ptr) => true,
        Some(crate::ir::abi::AbiKind::ByValue) => false,
        Some(crate::ir::abi::AbiKind::Scalar) => false,
        _ => element_ty.is_aggregate(),
    };
    let elem_arg = if pass_by_ptr {
        elemp
    } else {
        let e = alloc_value(next);
        func.block_mut(body_bb).insts.push(Inst::Load {
            dst: e,
            ptr: elemp,
            ty: element_ty.clone(),
        });
        e
    };
    let elem_abi = if pass_by_ptr {
        crate::ir::abi::AbiKind::Ptr
    } else {
        crate::ir::abi::AbiKind::Scalar
    };

    // Precompute next_i = i + 1.
    let next_i = alloc_value(next);
    let one = alloc_value(next);
    func.block_mut(body_bb).insts.push(Inst::IConst {
        dst: one,
        ty: LirType::I64,
        value: 1,
    });
    func.block_mut(body_bb).insts.push(Inst::Add {
        dst: next_i,
        ty: LirType::I64,
        lhs: i_val,
        rhs: one,
        overflow: Overflow::Wrap,
    });

    HofLoopCtx {
        check_bb,
        body_bb,
        done_bb,
        cond,
        i_val,
        next_i,
        elem_arg,
        elem_abi,
        extra_check_params,
    }
}

/// Expand a `HofExpand { hof_op: Each, … }` into an explicit loop.
///
/// Layout produced:
/// ```text
///   current_bb:
///     <pre-HofExpand insts>
///     jmp check_bb(0_i64)
///   check_bb(i: i64):
///     len check → body_bb | done_bb
///   body_bb:
///     ElemPtr + (optional Load) + CallClosure(closure, [elem])
///     jmp check_bb(i + 1)
///   done_bb:
///     <remaining insts from original block>
///     <original terminator>
/// ```
#[allow(clippy::too_many_arguments)]
fn expand_each(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    element_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    remaining: Vec<Inst>,
    orig_term: Term,
) {
    let elem_abi_hint = closure_arg_abis.first().copied();
    let ctx = emit_hof_loop_scaffold(
        func,
        current_bb,
        next,
        structs,
        coll,
        &element_ty,
        elem_abi_hint,
        vec![],
    );

    // body_bb: CallClosure(closure, [elem]) returning Void.
    func.block_mut(ctx.body_bb).insts.push(Inst::CallClosure {
        dst: None,
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.elem_arg],
        arg_abis: vec![ctx.elem_abi],
        ret_ty: LirType::Void,
    });

    // check_bb: cond ? body_bb : done_bb (no args — each has no result).
    func.block_mut(ctx.check_bb).terminator = Term::Branch {
        cond: ctx.cond,
        then_block: ctx.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![],
    };

    // body_bb: jump back to check_bb with incremented counter.
    func.block_mut(ctx.body_bb).terminator =
        Term::Jump(ctx.check_bb, vec![ctx.next_i]);

    // done_bb: move the tail of the original block here.
    func.block_mut(ctx.done_bb).insts = remaining;
    func.block_mut(ctx.done_bb).terminator = orig_term;
}

/// Expand `HofExpand { hof_op: Any | All, dst, … }` into an explicit loop
/// with early exit.
///
/// For `any`, the loop exits early with `true` when the predicate returns
/// true; if no element matches, `done_bb` is entered with `false`. For
/// `all`, the logic is inverted: early exit with `false` on any
/// false-predicate element, otherwise fall through with `true`.
///
/// The result type on `done_bb` matches the caller's declared type for
/// the dst ValueId (from `func.value_types`): some frontends lift Bool
/// into I64, so the expansion emits I64 constants in that case and Bool
/// constants otherwise.
#[allow(clippy::too_many_arguments)]
fn expand_any_all(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    op: HofOp,
    coll: ValueId,
    element_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    dst: ValueId,
    remaining: Vec<Inst>,
    orig_term: Term,
) {
    let is_any = matches!(op, HofOp::Any);
    // Resolve the dst's declared type. value_types is populated after SSA
    // construction; BIR lowering runs after both, so this lookup is valid.
    let dst_ty = func
        .value_types
        .get(dst.0 as usize)
        .and_then(|t| t.as_ref())
        .cloned()
        .unwrap_or(LirType::I64);

    let elem_abi_hint = closure_arg_abis.first().copied();
    let ctx = emit_hof_loop_scaffold(
        func,
        current_bb,
        next,
        structs,
        coll,
        &element_ty,
        elem_abi_hint,
        vec![],
    );

    // body_bb: CallClosure(closure, [elem]) returning Bool.
    let pred = alloc_value(next);
    func.block_mut(ctx.body_bb).insts.push(Inst::CallClosure {
        dst: Some(pred),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.elem_arg],
        arg_abis: vec![ctx.elem_abi],
        ret_ty: LirType::Bool,
    });

    // Constants for true/false in the dst's declared type.
    let early = alloc_value(next);
    let fall = alloc_value(next);
    let (early_value, fall_value) = if is_any { (1, 0) } else { (0, 1) };
    let const_inst = |d, v| match &dst_ty {
        LirType::Bool => Inst::BoolConst {
            dst: d,
            value: v != 0,
        },
        _ => Inst::IConst {
            dst: d,
            ty: dst_ty.clone(),
            value: v,
        },
    };

    // Allocate cont_bb (loop-continue block). The done_bb block param
    // carries the result; we reuse the caller-supplied `dst` as the
    // block-param ValueId so instructions in `remaining` that reference
    // `dst` see the block argument transparently.
    let cont_bb = func.add_block();

    // done_bb(result: dst_ty): <remaining> <orig_term>
    func.block_mut(ctx.done_bb).params.push((dst, dst_ty.clone()));
    func.block_mut(ctx.done_bb).insts = remaining;
    func.block_mut(ctx.done_bb).terminator = orig_term;

    // Synthesize the "early" constant. We park it in the current block
    // so both branches that reach done_bb can read the same ValueId.
    //
    // But SSA requires the value to dominate all uses. The simpler fix
    // is to create two constants: `early` parked wherever its user lives.
    // For `any`, `early=true` flows from body_bb (found branch);
    // `fall=false` flows from check_bb (exhaustion branch).
    //
    // Emit `early` at the top of body_bb (after the CallClosure) and
    // `fall` at the top of check_bb (before the Cmp).
    // The scaffold already ran, so we insert into the back of the blocks
    // (before their terminators which are still unset).
    func.block_mut(ctx.body_bb).insts.push(const_inst(early, early_value));
    // check_bb already has insts (FieldPtr, Load, Cmp). Append `fall`
    // before we set its terminator.
    func.block_mut(ctx.check_bb).insts.push(const_inst(fall, fall_value));

    // check_bb: cond ? body_bb : done_bb(fall)
    func.block_mut(ctx.check_bb).terminator = Term::Branch {
        cond: ctx.cond,
        then_block: ctx.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![fall],
    };

    // body_bb: predicate match?
    //   any: if pred → done_bb(true), else → cont_bb
    //   all: if pred → cont_bb, else → done_bb(false)
    let (then_block, then_args, else_block, else_args) = if is_any {
        (ctx.done_bb, vec![early], cont_bb, vec![])
    } else {
        (cont_bb, vec![], ctx.done_bb, vec![early])
    };
    func.block_mut(ctx.body_bb).terminator = Term::Branch {
        cond: pred,
        then_block,
        then_args,
        else_block,
        else_args,
    };

    // cont_bb: jump back to check_bb with incremented counter.
    func.block_mut(cont_bb).terminator = Term::Jump(ctx.check_bb, vec![ctx.next_i]);
}

/// Expand `HofExpand { hof_op: Fold, init, dst, … }` into an explicit
/// loop with a scalar accumulator threaded through `check_bb` as a second
/// block parameter.
///
/// Layout produced:
/// ```text
///   current_bb:
///     <pre-HofExpand insts>
///     jmp check_bb(0_i64, init)
///   check_bb(i: i64, acc: T):
///     len check → body_bb | done_bb(acc)
///   body_bb:
///     ElemPtr + (optional Load) + CallClosure(closure, [acc, elem]) → new_acc
///     jmp check_bb(i + 1, new_acc)
///   done_bb(result: T):
///     <remaining insts from original block>
///     <original terminator>
/// ```
#[allow(clippy::too_many_arguments)]
fn expand_fold(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    element_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    closure_ret_ty: LirType,
    init: ValueId,
    dst: ValueId,
    remaining: Vec<Inst>,
    orig_term: Term,
) {
    // For fold, closure signature is (acc, elem). Acc is at index 0, elem
    // at index 1 in `closure_arg_abis`.
    let acc_abi_hint = closure_arg_abis.first().copied();
    let elem_abi_hint = closure_arg_abis.get(1).copied();

    let ctx = emit_hof_loop_scaffold(
        func,
        current_bb,
        next,
        structs,
        coll,
        &element_ty,
        elem_abi_hint,
        vec![(closure_ret_ty.clone(), init)],
    );
    let acc_val = ctx.extra_check_params[0];

    // Acc ABI: hint first, else aggregate → Ptr, scalar → Scalar.
    let acc_by_ptr = match acc_abi_hint {
        Some(crate::ir::abi::AbiKind::Ptr) => true,
        Some(crate::ir::abi::AbiKind::ByValue) => false,
        Some(crate::ir::abi::AbiKind::Scalar) => false,
        _ => closure_ret_ty.is_aggregate(),
    };
    let acc_arg = if acc_by_ptr {
        // Spill the block-param struct to a slot and take its address.
        // AddressOf is expanded later in the same pass, but because the
        // `while bb_idx < ...` outer loop advances to process new blocks,
        // we invoke the expansion directly via the canonical op.
        let p = alloc_value(next);
        func.block_mut(ctx.body_bb).insts.push(Inst::AddressOf {
            dst: p,
            value: acc_val,
            ty: closure_ret_ty.clone(),
        });
        p
    } else {
        acc_val
    };
    let acc_abi = if acc_by_ptr {
        crate::ir::abi::AbiKind::Ptr
    } else {
        crate::ir::abi::AbiKind::Scalar
    };

    // body_bb: CallClosure(closure, [acc, elem]) → new_acc.
    let new_acc = alloc_value(next);
    func.block_mut(ctx.body_bb).insts.push(Inst::CallClosure {
        dst: Some(new_acc),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![acc_arg, ctx.elem_arg],
        arg_abis: vec![acc_abi, ctx.elem_abi],
        ret_ty: closure_ret_ty.clone(),
    });

    // check_bb: cond ? body_bb : done_bb(acc).
    func.block_mut(ctx.check_bb).terminator = Term::Branch {
        cond: ctx.cond,
        then_block: ctx.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![acc_val],
    };

    // body_bb: jump back to check_bb with next_i + new_acc.
    func.block_mut(ctx.body_bb).terminator =
        Term::Jump(ctx.check_bb, vec![ctx.next_i, new_acc]);

    // done_bb(result = dst): remaining + orig_term.
    func.block_mut(ctx.done_bb).params.push((dst, closure_ret_ty));
    func.block_mut(ctx.done_bb).insts = remaining;
    func.block_mut(ctx.done_bb).terminator = orig_term;
}

fn lookup_struct_id(structs: &[StructDef], name: &str) -> Option<StructId> {
    structs
        .iter()
        .position(|s| s.name == name)
        .map(|i| StructId(i as u32))
}

/// Canonical field-index table for opaque runtime structs.
///
/// Must match the C runtime layouts in `src/backend/c/c_runtime.rs` and
/// `opaque_runtime_size`. Any discrepancy would be a data-layout bug.
fn lookup_named_field_index(struct_name: &str, field_name: &str, structs: &[StructDef]) -> u32 {
    // First, try the struct's LIR fields (for non-opaque types, this is
    // authoritative).
    if let Some(sd) = structs.iter().find(|s| s.name == struct_name) {
        if let Some(idx) = sd.fields.iter().position(|(n, _)| n == field_name) {
            return idx as u32;
        }
    }
    // Opaque runtime types follow the uniform view-discriminator layout:
    //   GorgetString { data, cap, len, alloc }
    //   GorgetArray  { data, cap, len, elem_size, ... }
    // Use the canonical ordering from `docs/internals/thin-pointer-string.md`.
    match (struct_name, field_name) {
        ("GorgetString" | "Str", "data") => 0,
        ("GorgetString" | "Str", "cap") => 1,
        ("GorgetString" | "Str", "len") => 2,
        ("GorgetString" | "Str", "alloc") => 3,
        ("GorgetArray", "data") => 0,
        ("GorgetArray", "cap") => 1,
        ("GorgetArray", "len") => 2,
        ("GorgetArray", "elem_size") => 3,
        _ => 0,
    }
}

/// Cheap scan: true iff any instruction in `func` is a canonical op.
fn func_needs_expansion(func: &LirFunction) -> bool {
    for block in &func.blocks {
        for inst in &block.insts {
            if matches!(
                inst,
                Inst::SizeOf { .. }
                    | Inst::EnumInit { .. }
                    | Inst::EnumCheck { .. }
                    | Inst::EnumExtract { .. }
                    | Inst::StructInit { .. }
                    | Inst::NamedFieldPtr { .. }
                    | Inst::CowClone { .. }
                    | Inst::TraitCall { .. }
                    | Inst::HofExpand { .. }
                    | Inst::AddressOf { .. }
                    | Inst::BoxAlloc { .. }
            ) {
                return true;
            }
        }
    }
    false
}

fn alloc_value(next: &mut u32) -> ValueId {
    let v = ValueId(*next);
    *next += 1;
    v
}

/// Emit either a plain `Store` (scalar) or a `Memcpy` (aggregate) to copy
/// `value` into the memory at `dst_ptr` assuming it holds a T of type `ty`.
///
/// For aggregate-valued payloads, `value` is expected to be a pointer to
/// the source aggregate (this matches how the LIR lifts pass aggregate
/// values around — via slot addresses).
#[allow(dead_code)]
fn emit_store_or_memcpy(
    insts: &mut Vec<Inst>,
    next: &mut u32,
    structs: &[StructDef],
    dst_ptr: ValueId,
    value: ValueId,
    ty: &LirType,
) {
    let sz = c_sizeof_lir_type(ty, structs);
    if ty.is_aggregate() && sz > 0 {
        let size_val = alloc_value(next);
        insts.push(Inst::IConst {
            dst: size_val,
            ty: LirType::I64,
            value: sz as i64,
        });
        insts.push(Inst::Memcpy {
            dst_ptr,
            src_ptr: value,
            size: size_val,
        });
    } else {
        insts.push(Inst::Store { ptr: dst_ptr, value });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lir::{BlockId, Block, LirFunction, SlotId, StructId, Term};

    fn empty_func() -> LirFunction {
        let mut f = LirFunction::new("test".to_string(), vec![], LirType::Void);
        let bb = f.add_block();
        f.block_mut(bb).terminator = Term::RetVoid;
        f
    }

    #[test]
    fn address_of_spills_then_addresses() {
        let mut func = empty_func();
        let value = func.next_value();
        let dst = func.next_value();
        let bb = BlockId(0);
        func.block_mut(bb).insts.push(Inst::IConst {
            dst: value, ty: LirType::I64, value: 42,
        });
        func.block_mut(bb).insts.push(Inst::AddressOf {
            dst,
            value,
            ty: LirType::I64,
        });
        let slots_before = func.slots.len();
        expand_func(&mut func, &[]);
        // IConst unchanged, AddressOf → SlotStore + SlotAddr.
        let insts = &func.blocks[0].insts;
        assert!(matches!(insts[0], Inst::IConst { .. }));
        assert!(matches!(insts[1], Inst::SlotStore { .. }),
            "expected SlotStore, got {:?}", insts[1]);
        assert!(matches!(insts[2], Inst::SlotAddr { dst: d, .. } if d == dst),
            "expected SlotAddr with dst={:?}, got {:?}", dst, insts[2]);
        assert_eq!(func.slots.len(), slots_before + 1,
            "AddressOf expansion should allocate exactly one fresh slot");
    }

    #[test]
    fn box_alloc_expands_to_size_alloc_store() {
        let mut func = empty_func();
        let value = func.next_value();
        let dst = func.next_value();
        let bb = BlockId(0);
        func.block_mut(bb).insts.push(Inst::IConst {
            dst: value, ty: LirType::I64, value: 42,
        });
        func.block_mut(bb).insts.push(Inst::BoxAlloc {
            dst,
            inner_ty: LirType::I64,
            value,
        });
        expand_func(&mut func, &[]);
        let insts = &func.blocks[0].insts;
        assert!(matches!(insts[0], Inst::IConst { value: 42, .. }));
        assert!(matches!(insts[1], Inst::IConst { ty: LirType::I64, value: 8, .. }),
            "expected sizeof(I64)=8, got {:?}", insts[1]);
        assert!(matches!(&insts[2], Inst::CallExtern { name, .. } if name == "__gorget_alloc"),
            "expected __gorget_alloc CallExtern, got {:?}", insts[2]);
        assert!(matches!(insts[3], Inst::Store { ptr: p, .. } if p == dst),
            "expected Store ptr=dst, got {:?}", insts[3]);
    }

    #[test]
    fn validator_rejects_unexpanded_address_of() {
        let mut func = empty_func();
        let value = func.next_value();
        let dst = func.next_value();
        func.block_mut(BlockId(0)).insts.push(Inst::AddressOf {
            dst,
            value,
            ty: LirType::I64,
        });
        let mut module = crate::lir::LirModule::new();
        module.functions.push(func);
        // Validator should reject unexpanded AddressOf.
        let err = crate::bir::validate::assert_primitives_only(&module);
        assert!(matches!(err, Err(crate::bir::BirError::UnloweredCanonicalOp { opcode: "AddressOf", .. })));
    }

    #[test]
    fn validator_rejects_unexpanded_box_alloc() {
        let mut func = empty_func();
        let value = func.next_value();
        let dst = func.next_value();
        func.block_mut(BlockId(0)).insts.push(Inst::BoxAlloc {
            dst,
            inner_ty: LirType::I64,
            value,
        });
        let mut module = crate::lir::LirModule::new();
        module.functions.push(func);
        let err = crate::bir::validate::assert_primitives_only(&module);
        assert!(matches!(err, Err(crate::bir::BirError::UnloweredCanonicalOp { opcode: "BoxAlloc", .. })));
    }

    // Unused imports in the test module.
    #[allow(dead_code)]
    fn _touch_unused(_: Block, _: SlotId, _: StructId) {}
}
