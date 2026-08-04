//! LIR → BIR lowering pass.
//!
//! Expands LIR's canonical-level high-level ops into sequences of primitive
//! instructions that backends can emit directly. Each canonical op described in
//! `docs/devbook/16-bir.md` gets an expansion here as the
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
//! Future canonical ops added to LIR get expanded here in subsequent steps.

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
///
/// Module-level synthesis (see `bir::synth`) runs as part of this pass:
/// a `SynthPool` collects synthesized `LirFunction`s that canonical-op
/// expansion requests, and at pass exit the new functions are appended
/// to `module.functions`. `value_types` for the appended functions is
/// computed before returning so later stages (the BIR validator, the
/// backends) see fully-populated function metadata.
pub fn lower_lir_to_bir(mut module: LirModule) -> Result<LirModule, BirError> {
    // Guardrail: the `__gg_synth_` prefix is reserved for synthesis output.
    // If any existing function already carries it, something upstream is
    // producing names in our namespace.
    crate::bir::synth::assert_no_synth_prefix(&module);

    let base_func_count = module.functions.len() as u32;
    let mut pool = crate::bir::synth::SynthPool::new(base_func_count);

    // Swap `functions` out so we can iterate them mutably while holding an
    // immutable reference to `module.structs`. The structs table is the only
    // piece of module metadata the expansion reads (for c_sizeof / payload
    // type lookups); swapping functions is O(1), cloning structs would be O(N).
    let mut funcs = std::mem::take(&mut module.functions);
    for func in funcs.iter_mut() {
        expand_func(func, &module.structs, &mut pool);
    }
    module.functions = funcs;

    // Splice synthesized functions onto the module tail.
    let synth_fns = pool.finish();
    if !synth_fns.is_empty() {
        debug_assert_eq!(
            module.functions.len() as u32,
            base_func_count,
            "lower_lir_to_bir: unexpected functions added outside the synth pool",
        );
        let synth_count = synth_fns.len();
        module.functions.extend(synth_fns);
        // Populate `value_types` for only the newly-appended functions.
        let start = module.functions.len() - synth_count;
        for i in start..module.functions.len() {
            crate::lir::types::compute_function_value_types_at(&mut module, i);
        }
    }

    Ok(module)
}

fn expand_func(
    func: &mut LirFunction,
    structs: &[StructDef],
    pool: &mut crate::bir::synth::SynthPool,
) {
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
        let old_spans_taken = std::mem::take(&mut func.blocks[bb_idx].span_map);
        // Reconcile if upstream emitter didn't yet write parallel spans.
        let old_spans: Vec<Option<crate::span::Span>> =
            if old_spans_taken.len() == old.len() {
                old_spans_taken
            } else {
                vec![None; old.len()]
            };
        let mut new_insts: Vec<Inst> = Vec::with_capacity(old.len());
        // Parallel span vec — grown in lockstep with `new_insts`. For
        // 1-to-N source-to-target expansions inside the match below, all
        // N target insts inherit the source inst's span (set after each
        // arm by replicating `current_src_span` over the count added).
        let mut new_spans: Vec<Option<crate::span::Span>> =
            Vec::with_capacity(old.len());
        let mut iter = old.into_iter();
        let mut spans_iter = old_spans.into_iter();
        let mut hof_split = false;
        while let Some(inst) = iter.next() {
            let current_src_span = spans_iter.next().flatten();
            let pre_push_len = new_insts.len();
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
                Inst::TraitCall {
                    dst,
                    object,
                    trait_obj_struct,
                    method_idx,
                    args,
                    arg_abis: _,
                    param_tys,
                    ret_ty,
                } => {
                    // Rewrite the TraitCall into `Call(helper)` where
                    // `helper` is a synth'd function (one per unique
                    // `(trait_obj_struct, method_idx, signature)`)
                    // whose body carries the vtable dispatch chain
                    // (FieldPtr+Load×3 + CallPtr). The helper has a
                    // typed signature, so backend Call coercion
                    // handles aggregate args without the CallPtr
                    // arg-ABI ambiguity.
                    //
                    // `param_tys` carries the method's user-param LIR
                    // types as resolved at emit time from the VTable
                    // FnPtr — which has the concrete Str-by-value /
                    // aggregate-struct types (not the opaque `void*`
                    // the extern declaration carries). See
                    // `try_emit_trait_call` in `lir::lower::insts`.
                    let fid = pool.get_or_emit_trait_helper(
                        structs,
                        trait_obj_struct,
                        method_idx,
                        &param_tys,
                        &ret_ty,
                    );

                    let mut call_args = Vec::with_capacity(1 + args.len());
                    call_args.push(object);
                    call_args.extend(args);
                    new_insts.push(Inst::Call {
                        dst: if matches!(ret_ty, LirType::Void) {
                            None
                        } else {
                            dst
                        },
                        func: fid,
                        args: call_args,
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
                    value_ty: value_ty_inner,
                    closure,
                    closure_kind,
                    closure_ret_ty,
                    closure_arg_abis,
                    dst,
                    init,
                } => {
                    match hof_op {
                        HofOp::SortBy
                        | HofOp::SortedBy
                        | HofOp::SortByKey
                        | HofOp::SortedByKey => {
                            // Sort family: synthesize a dedicated sort_impl
                            // function (shared across in-place and returning
                            // variants) and rewrite the call site.
                            let is_key =
                                matches!(hof_op, HofOp::SortByKey | HofOp::SortedByKey);
                            let is_returning =
                                matches!(hof_op, HofOp::SortedBy | HofOp::SortedByKey);
                            let fid = if is_key {
                                pool.get_or_emit_sort_by_key_impl(
                                    structs,
                                    hof_op,
                                    &element_ty,
                                    &closure_arg_abis,
                                    &closure_ret_ty,
                                )
                            } else {
                                pool.get_or_emit_sort_impl(
                                    structs,
                                    hof_op,
                                    &element_ty,
                                    &closure_arg_abis,
                                    &closure_ret_ty,
                                )
                            };
                            let gorget_array_sid = structs
                                .iter()
                                .position(|s| s.name == "GorgetArray")
                                .map(|i| StructId(i as u32))
                                .unwrap_or(StructId(0));
                            if is_returning {
                                // Inline: ret = gorget_array_clone(coll);
                                //         ret_ptr = &ret;
                                //         sort_impl(ret_ptr, closure);
                                //         dst = *ret_ptr
                                let d = dst.expect(
                                    "SortedBy/SortedByKey must carry a dst ValueId",
                                );
                                let clone_val = alloc_value(&mut next);
                                new_insts.push(Inst::CallExtern {
                                    dst: Some(clone_val),
                                    name: "gorget_array_clone".to_string(),
                                    args: vec![coll],
                                    arg_abis: vec![crate::ir::abi::AbiKind::Ptr],
                                });
                                let slot =
                                    func.add_slot(LirType::Struct(gorget_array_sid), None);
                                new_insts.push(Inst::SlotStore {
                                    slot,
                                    value: clone_val,
                                    is_move: true,
                                });
                                let clone_ptr = alloc_value(&mut next);
                                new_insts.push(Inst::SlotAddr {
                                    dst: clone_ptr,
                                    slot,
                                });
                                new_insts.push(Inst::Call {
                                    dst: None,
                                    func: fid,
                                    args: vec![clone_ptr, closure],
                                });
                                new_insts.push(Inst::SlotLoad {
                                    dst: d,
                                    slot,
                                    ty: LirType::Struct(gorget_array_sid),
                                });
                            } else {
                                // Direct call: sort_impl(coll, closure).
                                new_insts.push(Inst::Call {
                                    dst: None,
                                    func: fid,
                                    args: vec![coll, closure],
                                });
                            }
                        }
                        HofOp::Each
                        | HofOp::Any
                        | HofOp::All
                        | HofOp::Fold
                        | HofOp::Reduce
                        | HofOp::Count
                        | HofOp::Find
                        | HofOp::FindIndex
                        | HofOp::Filter
                        | HofOp::Map
                        | HofOp::FlatMap
                        | HofOp::DictEach
                        | HofOp::DictFold
                        | HofOp::DictAny
                        | HofOp::DictAll
                        | HofOp::DictFilter
                        | HofOp::SetEach
                        | HofOp::SetFold
                        | HofOp::SetAny
                        | HofOp::SetAll
                        | HofOp::SetFilter => {
                            // Capture the tail of the block; it moves to done_bb.
                            let remaining: Vec<Inst> = iter.by_ref().collect();
                            // Parallel-capture the tail spans so done_bb's
                            // span_map stays in sync. spans_iter is fed in
                            // lockstep with `iter` so collecting both here
                            // preserves alignment.
                            let remaining_spans: Vec<Option<crate::span::Span>> =
                                spans_iter.by_ref().collect();
                            let orig_term = std::mem::replace(
                                &mut func.blocks[bb_idx].terminator,
                                Term::Unreachable,
                            );
                            let orig_term_span = std::mem::take(
                                &mut func.blocks[bb_idx].terminator_span,
                            );
                            func.blocks[bb_idx].insts = std::mem::take(&mut new_insts);
                            func.blocks[bb_idx].span_map = std::mem::take(&mut new_spans);
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
                                    remaining_spans,
                                    orig_term,
                                    orig_term_span,
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
                                    remaining_spans,
                                    orig_term,
                                    orig_term_span,
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
                                    remaining_spans,
                                    orig_term,
                                    orig_term_span,
                                ),
                                HofOp::Reduce => expand_reduce(
                                    func,
                                    bb_idx,
                                    &mut next,
                                    structs,
                                    coll,
                                    element_ty,
                                    closure,
                                    closure_arg_abis,
                                    closure_ret_ty,
                                    dst.expect("reduce must carry a dst ValueId"),
                                    remaining,
                                    remaining_spans,
                                    orig_term,
                                    orig_term_span,
                                ),
                                HofOp::Count => expand_count(
                                    func,
                                    bb_idx,
                                    &mut next,
                                    structs,
                                    coll,
                                    element_ty,
                                    closure,
                                    closure_arg_abis,
                                    dst.expect("count must carry a dst ValueId"),
                                    remaining,
                                    remaining_spans,
                                    orig_term,
                                    orig_term_span,
                                ),
                                HofOp::FindIndex => expand_find_index(
                                    func,
                                    bb_idx,
                                    &mut next,
                                    structs,
                                    coll,
                                    element_ty,
                                    closure,
                                    closure_arg_abis,
                                    dst.expect("find_index must carry a dst ValueId"),
                                    remaining,
                                    remaining_spans,
                                    orig_term,
                                    orig_term_span,
                                ),
                                HofOp::Filter => expand_filter(
                                    func,
                                    bb_idx,
                                    &mut next,
                                    structs,
                                    coll,
                                    element_ty,
                                    closure,
                                    closure_arg_abis,
                                    dst.expect("filter must carry a dst ValueId"),
                                    remaining,
                                    remaining_spans,
                                    orig_term,
                                    orig_term_span,
                                ),
                                HofOp::Map => expand_map(
                                    func,
                                    bb_idx,
                                    &mut next,
                                    structs,
                                    coll,
                                    element_ty,
                                    closure,
                                    closure_arg_abis,
                                    closure_ret_ty,
                                    dst.expect("map must carry a dst ValueId"),
                                    remaining,
                                    remaining_spans,
                                    orig_term,
                                    orig_term_span,
                                ),
                                HofOp::FlatMap => expand_flat_map(
                                    func,
                                    bb_idx,
                                    &mut next,
                                    structs,
                                    coll,
                                    element_ty,
                                    closure,
                                    closure_arg_abis,
                                    closure_ret_ty,
                                    dst.expect("flat_map must carry a dst ValueId"),
                                    remaining,
                                    remaining_spans,
                                    orig_term,
                                    orig_term_span,
                                ),
                                HofOp::DictEach => {
                                    // For Dict, `element_ty` carries K and
                                    // `value_ty_inner` carries V. Emitter
                                    // fills both.
                                    let val_ty = value_ty_inner
                                        .clone()
                                        .unwrap_or(LirType::I64);
                                    expand_dict_each(
                                        func,
                                        bb_idx,
                                        &mut next,
                                        structs,
                                        coll,
                                        element_ty,
                                        val_ty,
                                        closure,
                                        closure_arg_abis,
                                        remaining,
                                        remaining_spans,
                                        orig_term,
                                        orig_term_span,
                                    );
                                }
                                HofOp::DictFold => {
                                    let val_ty = value_ty_inner
                                        .clone()
                                        .unwrap_or(LirType::I64);
                                    expand_dict_fold(
                                        func,
                                        bb_idx,
                                        &mut next,
                                        structs,
                                        coll,
                                        element_ty,
                                        val_ty,
                                        closure,
                                        closure_arg_abis,
                                        closure_ret_ty,
                                        init.expect("DictFold must carry init"),
                                        dst.expect("DictFold must carry dst"),
                                        remaining,
                                        remaining_spans,
                                        orig_term,
                                        orig_term_span,
                                    );
                                }
                                HofOp::SetEach => {
                                    // `value_ty` encodes is_ordered:
                                    // Some(Void) → ordered (Set__),
                                    // Some(Ptr)  → unordered (HashSet__).
                                    let is_ordered = !matches!(
                                        value_ty_inner,
                                        Some(LirType::Ptr)
                                    );
                                    expand_set_each(
                                        func,
                                        bb_idx,
                                        &mut next,
                                        structs,
                                        coll,
                                        element_ty,
                                        closure,
                                        closure_arg_abis,
                                        is_ordered,
                                        remaining,
                                        remaining_spans,
                                        orig_term,
                                        orig_term_span,
                                    );
                                }
                                HofOp::SetFold => {
                                    let is_ordered = !matches!(
                                        value_ty_inner,
                                        Some(LirType::Ptr)
                                    );
                                    expand_set_fold(
                                        func,
                                        bb_idx,
                                        &mut next,
                                        structs,
                                        coll,
                                        element_ty,
                                        closure,
                                        closure_arg_abis,
                                        closure_ret_ty,
                                        init.expect("SetFold must carry init"),
                                        dst.expect("SetFold must carry dst"),
                                        is_ordered,
                                        remaining,
                                        remaining_spans,
                                        orig_term,
                                        orig_term_span,
                                    );
                                }
                                HofOp::SetAny | HofOp::SetAll => {
                                    let is_ordered = !matches!(
                                        value_ty_inner,
                                        Some(LirType::Ptr)
                                    );
                                    expand_set_any_all(
                                        func,
                                        bb_idx,
                                        &mut next,
                                        structs,
                                        hof_op,
                                        coll,
                                        element_ty,
                                        closure,
                                        closure_arg_abis,
                                        dst.expect("SetAny/All must carry dst"),
                                        is_ordered,
                                        remaining,
                                        remaining_spans,
                                        orig_term,
                                        orig_term_span,
                                    );
                                }
                                HofOp::SetFilter => {
                                    let is_ordered = !matches!(
                                        value_ty_inner,
                                        Some(LirType::Ptr)
                                    );
                                    expand_set_filter(
                                        func,
                                        bb_idx,
                                        &mut next,
                                        structs,
                                        coll,
                                        element_ty,
                                        closure,
                                        closure_arg_abis,
                                        dst.expect("SetFilter must carry dst"),
                                        is_ordered,
                                        remaining,
                                        remaining_spans,
                                        orig_term,
                                        orig_term_span,
                                    );
                                }
                                HofOp::DictAny | HofOp::DictAll => {
                                    let val_ty = value_ty_inner
                                        .clone()
                                        .unwrap_or(LirType::I64);
                                    expand_dict_any_all(
                                        func,
                                        bb_idx,
                                        &mut next,
                                        structs,
                                        hof_op,
                                        coll,
                                        element_ty,
                                        val_ty,
                                        closure,
                                        closure_arg_abis,
                                        dst.expect("DictAny/All must carry dst"),
                                        remaining,
                                        remaining_spans,
                                        orig_term,
                                        orig_term_span,
                                    );
                                }
                                HofOp::DictFilter => {
                                    let val_ty = value_ty_inner
                                        .clone()
                                        .unwrap_or(LirType::I64);
                                    expand_dict_filter(
                                        func,
                                        bb_idx,
                                        &mut next,
                                        structs,
                                        coll,
                                        element_ty,
                                        val_ty,
                                        closure,
                                        closure_arg_abis,
                                        dst.expect("DictFilter must carry dst"),
                                        remaining,
                                        remaining_spans,
                                        orig_term,
                                        orig_term_span,
                                    );
                                }
                                HofOp::Find => {
                                    // The dst's declared type (`Struct(Option__T)`)
                                    // is carried on HofExpand.value_ty by the emitter
                                    // so the expansion can allocate a slot of the
                                    // right layout without reaching back into GIR.
                                    let d = dst.expect("find must carry a dst ValueId");
                                    let option_sid = match &value_ty_inner {
                                        Some(LirType::Struct(sid)) => *sid,
                                        _ => {
                                            new_insts.push(Inst::HofExpand {
                                                coll,
                                                hof_op,
                                                element_ty,
                                                value_ty: value_ty_inner,
                                                closure,
                                                closure_kind,
                                                closure_ret_ty,
                                                closure_arg_abis,
                                                dst,
                                                init,
                                            });
                                            continue;
                                        }
                                    };
                                    expand_find(
                                        func,
                                        bb_idx,
                                        &mut next,
                                        structs,
                                        coll,
                                        element_ty,
                                        closure,
                                        closure_arg_abis,
                                        option_sid,
                                        d,
                                        remaining,
                                        remaining_spans,
                                        orig_term,
                                        orig_term_span,
                                    );
                                }
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
                                value_ty: value_ty_inner,
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
                        arg_abis: vec![crate::ir::abi::AbiKind::Scalar],
                    });
                    // 3) Write the value into *dst. Plain `Store` lets each
                    //    backend dispatch on val_types (scalar vs aggregate
                    //    vs Ptr-to-aggregate), matching StructInit/EnumInit.
                    new_insts.push(Inst::Store { ptr: dst, value });
                }
                Inst::CallRuntime { dst, callee, args, arg_abis } => {
                    // CallRuntime is a typed-callee form of CallExtern; the
                    // backends still pattern-match on `name` (B1 lifts them
                    // to enum-aware). Rewrite to CallExtern using the
                    // variant's stable C symbol name.
                    new_insts.push(Inst::CallExtern {
                        dst,
                        name: callee.c_name().to_string(),
                        args,
                        arg_abis,
                    });
                }
                Inst::CollectionCtor { dst, kind, args, arg_abis, with_capacity, str_keyed, .. } => {
                    // Pick the runtime constructor by (kind, with-capacity?, str-keyed?).
                    // The original CallExtern's args (key_size, val_size,
                    // capacity, …) pass through unchanged — the promote pass
                    // captured them verbatim. A follow-up will derive sizes
                    // from `elem_or_key` / `val` at this layer.
                    use crate::lir::CollectionCtorKind as K;
                    let runtime_name = match (kind, with_capacity, str_keyed) {
                        (K::Vector, false, _) | (K::Deque, false, _) => "gorget_array_new",
                        (K::Vector, true, _)  | (K::Deque, true, _)  => "gorget_array_with_capacity",
                        (K::Dict, _, false) => "gorget_dict_new",
                        (K::Dict, _, true)  => "gorget_dict_new_str",
                        (K::HashMap, _, false) => "gorget_map_new",
                        (K::HashMap, _, true)  => "gorget_map_new_str",
                        (K::Set, _, false) => "gorget_ordered_set_new",
                        (K::Set, _, true)  => "gorget_ordered_set_new_str",
                        (K::HashSet, _, false) => "gorget_set_new",
                        (K::HashSet, _, true)  => "gorget_set_new_str",
                    };
                    new_insts.push(Inst::CallExtern {
                        dst: Some(dst),
                        name: runtime_name.to_string(),
                        args,
                        arg_abis,
                    });
                }
                other => new_insts.push(other),
            }
            // Pad `new_spans` to match the number of insts the arm just
            // appended to `new_insts`, all inheriting the source inst's
            // span. For 1-to-N expansions every emitted inst points back
            // at the originating source — the right default for trace
            // attribution. The HOF arm transfers ownership of `new_insts`
            // to the source block mid-iteration; the matching transfer
            // for `new_spans` happens there and resets `pre_push_len`
            // before this padding fires.
            let added = new_insts.len().saturating_sub(pre_push_len);
            for _ in 0..added {
                new_spans.push(current_src_span);
            }
        }
        if !hof_split {
            func.blocks[bb_idx].insts = new_insts;
            func.blocks[bb_idx].span_map = new_spans;
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
    /// Pointer to the current element (the `ElemPtr` result). Always a
    /// `Ptr` to the element at `coll.data + i * elem_size`. Variants that
    /// need to push this element to another collection (`filter`,
    /// `map`, `flat_map`) use it directly without going through a
    /// scalar load.
    elem_ptr: ValueId,
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
    start_counter: Option<ValueId>,
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

    // current_bb → check_bb(start_counter, init0, init1, ...)
    let start = start_counter.unwrap_or_else(|| {
        let z = alloc_value(next);
        func.block_mut(BlockId(current_bb as u32))
            .push_synthetic(Inst::IConst {
                dst: z,
                ty: LirType::I64,
                value: 0,
            });
        z
    });
    let mut entry_args = Vec::with_capacity(1 + extra_check_inits.len());
    entry_args.push(start);
    for (_, init) in &extra_check_inits {
        entry_args.push(*init);
    }
    func.block_mut(BlockId(current_bb as u32)).terminator =
        Term::Jump(check_bb, entry_args);

    // check_bb: load GorgetArray.len and compare.
    let lenp = alloc_value(next);
    func.block_mut(check_bb).push_synthetic(Inst::FieldPtr {
        dst: lenp,
        base: coll,
        struct_id: gorget_array_sid,
        field: 2, // GorgetArray.len
    });
    let len = alloc_value(next);
    func.block_mut(check_bb).push_synthetic(Inst::Load {
        dst: len,
        ptr: lenp,
        ty: LirType::I64,
    });
    let cond = alloc_value(next);
    func.block_mut(check_bb).push_synthetic(Inst::Cmp {
        dst: cond,
        op: CmpOp::Lt,
        lhs: i_val,
        rhs: len,
    });
    // Caller sets `check_bb.terminator`.

    // body_bb: get data ptr and element ptr.
    let datap_ptr = alloc_value(next);
    func.block_mut(body_bb).push_synthetic(Inst::FieldPtr {
        dst: datap_ptr,
        base: coll,
        struct_id: gorget_array_sid,
        field: 0, // GorgetArray.data
    });
    let datap = alloc_value(next);
    func.block_mut(body_bb).push_synthetic(Inst::Load {
        dst: datap,
        ptr: datap_ptr,
        ty: LirType::Ptr,
    });
    let elem_size = c_sizeof_lir_type(element_ty, structs) as u32;
    let elemp = alloc_value(next);
    func.block_mut(body_bb).push_synthetic(Inst::ElemPtr {
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
        func.block_mut(body_bb).push_synthetic(Inst::Load {
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
    func.block_mut(body_bb).push_synthetic(Inst::IConst {
        dst: one,
        ty: LirType::I64,
        value: 1,
    });
    func.block_mut(body_bb).push_synthetic(Inst::Add {
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
        elem_ptr: elemp,
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
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
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
        None,
    );

    // body_bb: CallClosure(closure, [elem]) returning Void.
    func.block_mut(ctx.body_bb).push_synthetic(Inst::CallClosure {
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
    let done = ctx.done_bb;
    let pre_len = func.block(done).insts.len();
    let pre_spans_len = func.block(done).span_map.len();
    func.block_mut(done).insts = remaining;
    // Synthetic scaffold insts already pushed into done_bb (e.g. a
    // SlotLoad of the result) keep their existing `None` spans; tail
    // insts inherit the spans we captured from the original block.
    let mut combined_spans: Vec<Option<crate::span::Span>> =
        if pre_spans_len == pre_len { func.block(done).span_map.clone() } else { vec![None; pre_len] };
    combined_spans.extend(remaining_spans);
    func.block_mut(done).span_map = combined_spans;
    func.block_mut(done).terminator = orig_term;
    func.block_mut(done).terminator_span = orig_term_span;
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
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
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
        None,
    );

    // body_bb: CallClosure(closure, [elem]) returning Bool.
    let pred = alloc_value(next);
    func.block_mut(ctx.body_bb).push_synthetic(Inst::CallClosure {
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
    let done = ctx.done_bb;
    let pre_len = func.block(done).insts.len();
    let pre_spans_len = func.block(done).span_map.len();
    func.block_mut(done).params.push((dst, dst_ty.clone()));
    func.block_mut(done).insts = remaining;
    // Synthetic scaffold insts already pushed into done_bb keep their
    // existing `None` spans; tail insts inherit the captured source spans.
    let mut combined_spans: Vec<Option<crate::span::Span>> =
        if pre_spans_len == pre_len { func.block(done).span_map.clone() } else { vec![None; pre_len] };
    combined_spans.extend(remaining_spans);
    func.block_mut(done).span_map = combined_spans;
    func.block_mut(done).terminator = orig_term;
    func.block_mut(done).terminator_span = orig_term_span;

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
    func.block_mut(ctx.body_bb).push_synthetic(const_inst(early, early_value));
    // check_bb already has insts (FieldPtr, Load, Cmp). Append `fall`
    // before we set its terminator.
    func.block_mut(ctx.check_bb).push_synthetic(const_inst(fall, fall_value));

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
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
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
        None,
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
        func.block_mut(ctx.body_bb).push_synthetic(Inst::AddressOf {
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
    func.block_mut(ctx.body_bb).push_synthetic(Inst::CallClosure {
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
    let done = ctx.done_bb;
    let pre_len = func.block(done).insts.len();
    let pre_spans_len = func.block(done).span_map.len();
    func.block_mut(done).insts = remaining;
    // Synthetic scaffold insts already pushed into done_bb (e.g. a
    // SlotLoad of the result) keep their existing `None` spans; tail
    // insts inherit the spans we captured from the original block.
    let mut combined_spans: Vec<Option<crate::span::Span>> =
        if pre_spans_len == pre_len { func.block(done).span_map.clone() } else { vec![None; pre_len] };
    combined_spans.extend(remaining_spans);
    func.block_mut(done).span_map = combined_spans;
    func.block_mut(done).terminator = orig_term;
    func.block_mut(done).terminator_span = orig_term_span;
}

/// Expand `HofExpand { hof_op: Reduce, dst, … }` — like `fold` but the
/// initial accumulator is `coll[0]` and the loop starts at `i = 1`.
///
/// Matches the prior backend semantics: on an empty collection this
/// reads whatever bytes sit at `coll.data + 0`, which is the same
/// undefined behavior the C and LLVM backends produce today. Callers
/// expected to check `.len() > 0` themselves; we preserve that contract
/// rather than silently adding a guard.
#[allow(clippy::too_many_arguments)]
fn expand_reduce(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    element_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    closure_ret_ty: LirType,
    dst: ValueId,
    remaining: Vec<Inst>,
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
) {
    let gorget_array_sid =
        lookup_struct_id(structs, "GorgetArray").unwrap_or(StructId(0));

    // current_bb: pre-load first element as the initial accumulator.
    //   datap = *FieldPtr(coll, data)
    //   first_elem = *ElemPtr(datap, 0, elem_size)     (scalar only)
    //   i0 = 1_i64
    let datap_ptr = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32))
        .push_synthetic(Inst::FieldPtr {
            dst: datap_ptr,
            base: coll,
            struct_id: gorget_array_sid,
            field: 0,
        });
    let datap = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32))
        .push_synthetic(Inst::Load {
            dst: datap,
            ptr: datap_ptr,
            ty: LirType::Ptr,
        });
    let elem_size = c_sizeof_lir_type(&element_ty, structs) as u32;
    let zero_i64 = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32))
        .push_synthetic(Inst::IConst {
            dst: zero_i64,
            ty: LirType::I64,
            value: 0,
        });
    let first_ptr = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32))
        .push_synthetic(Inst::ElemPtr {
            dst: first_ptr,
            base: datap,
            index: zero_i64,
            elem_size,
        });
    let first_elem = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32))
        .push_synthetic(Inst::Load {
            dst: first_elem,
            ptr: first_ptr,
            ty: element_ty.clone(),
        });
    let one_i64 = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32))
        .push_synthetic(Inst::IConst {
            dst: one_i64,
            ty: LirType::I64,
            value: 1,
        });

    // Scaffold with start=1 and acc init = first_elem. From here the
    // structure mirrors fold.
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
        vec![(closure_ret_ty.clone(), first_elem)],
        Some(one_i64),
    );
    let acc_val = ctx.extra_check_params[0];

    let acc_by_ptr = match acc_abi_hint {
        Some(crate::ir::abi::AbiKind::Ptr) => true,
        Some(crate::ir::abi::AbiKind::ByValue) => false,
        Some(crate::ir::abi::AbiKind::Scalar) => false,
        _ => closure_ret_ty.is_aggregate(),
    };
    let acc_arg = if acc_by_ptr {
        let p = alloc_value(next);
        func.block_mut(ctx.body_bb).push_synthetic(Inst::AddressOf {
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

    let new_acc = alloc_value(next);
    func.block_mut(ctx.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(new_acc),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![acc_arg, ctx.elem_arg],
        arg_abis: vec![acc_abi, ctx.elem_abi],
        ret_ty: closure_ret_ty.clone(),
    });

    func.block_mut(ctx.check_bb).terminator = Term::Branch {
        cond: ctx.cond,
        then_block: ctx.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![acc_val],
    };
    func.block_mut(ctx.body_bb).terminator =
        Term::Jump(ctx.check_bb, vec![ctx.next_i, new_acc]);
    func.block_mut(ctx.done_bb).params.push((dst, closure_ret_ty));
    let done = ctx.done_bb;
    let pre_len = func.block(done).insts.len();
    let pre_spans_len = func.block(done).span_map.len();
    func.block_mut(done).insts = remaining;
    // Synthetic scaffold insts already pushed into done_bb (e.g. a
    // SlotLoad of the result) keep their existing `None` spans; tail
    // insts inherit the spans we captured from the original block.
    let mut combined_spans: Vec<Option<crate::span::Span>> =
        if pre_spans_len == pre_len { func.block(done).span_map.clone() } else { vec![None; pre_len] };
    combined_spans.extend(remaining_spans);
    func.block_mut(done).span_map = combined_spans;
    func.block_mut(done).terminator = orig_term;
    func.block_mut(done).terminator_span = orig_term_span;
}

/// Expand `HofExpand { hof_op: Count, dst, … }` — count elements for
/// which the predicate returns true.
///
/// Threads an i64 accumulator through `check_bb` as the second block
/// param, incrementing it by `(Bool→I64) IntCast` of the closure's
/// result on each iteration. This matches the prior backend behaviour
/// of `zext i1 pred to i64; add i64 cnt, inc`.
#[allow(clippy::too_many_arguments)]
fn expand_count(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    element_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    dst: ValueId,
    remaining: Vec<Inst>,
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
) {
    let elem_abi_hint = closure_arg_abis.first().copied();

    // Accumulator init = 0_i64. Park the constant in current_bb.
    let zero = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32))
        .push_synthetic(Inst::IConst {
            dst: zero,
            ty: LirType::I64,
            value: 0,
        });

    let ctx = emit_hof_loop_scaffold(
        func,
        current_bb,
        next,
        structs,
        coll,
        &element_ty,
        elem_abi_hint,
        vec![(LirType::I64, zero)],
        None,
    );
    let cnt_val = ctx.extra_check_params[0];

    // body_bb: CallClosure(closure, [elem]) → pred: Bool.
    let pred = alloc_value(next);
    func.block_mut(ctx.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(pred),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.elem_arg],
        arg_abis: vec![ctx.elem_abi],
        ret_ty: LirType::Bool,
    });
    // inc = (i64) pred.
    let inc = alloc_value(next);
    func.block_mut(ctx.body_bb).push_synthetic(Inst::IntCast {
        dst: inc,
        value: pred,
        to: LirType::I64,
    });
    // cnt_new = cnt + inc.
    let cnt_new = alloc_value(next);
    func.block_mut(ctx.body_bb).push_synthetic(Inst::Add {
        dst: cnt_new,
        ty: LirType::I64,
        lhs: cnt_val,
        rhs: inc,
        overflow: Overflow::Wrap,
    });

    // check_bb: cond ? body_bb : done_bb(cnt).
    func.block_mut(ctx.check_bb).terminator = Term::Branch {
        cond: ctx.cond,
        then_block: ctx.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![cnt_val],
    };
    // body_bb: jump back with next_i + cnt_new.
    func.block_mut(ctx.body_bb).terminator =
        Term::Jump(ctx.check_bb, vec![ctx.next_i, cnt_new]);
    // done_bb: param = dst.
    func.block_mut(ctx.done_bb).params.push((dst, LirType::I64));
    let done = ctx.done_bb;
    let pre_len = func.block(done).insts.len();
    let pre_spans_len = func.block(done).span_map.len();
    func.block_mut(done).insts = remaining;
    // Synthetic scaffold insts already pushed into done_bb (e.g. a
    // SlotLoad of the result) keep their existing `None` spans; tail
    // insts inherit the spans we captured from the original block.
    let mut combined_spans: Vec<Option<crate::span::Span>> =
        if pre_spans_len == pre_len { func.block(done).span_map.clone() } else { vec![None; pre_len] };
    combined_spans.extend(remaining_spans);
    func.block_mut(done).span_map = combined_spans;
    func.block_mut(done).terminator = orig_term;
    func.block_mut(done).terminator_span = orig_term_span;
}

/// Expand `HofExpand { hof_op: FindIndex, dst, … }` — returns the index of
/// the first element for which the predicate returns true, or `-1`
/// when no element matches. The caller's `dst` is an `i64`; the
/// existing upstream Option-wrapping machinery converts the sentinel
/// into an `Option[int]` at use sites that need it (same semantics as
/// the prior backend inliner).
#[allow(clippy::too_many_arguments)]
fn expand_find_index(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    element_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    dst: ValueId,
    remaining: Vec<Inst>,
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
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
        None,
    );

    // body_bb: CallClosure(closure, [elem]) → pred: Bool.
    let pred = alloc_value(next);
    func.block_mut(ctx.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(pred),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.elem_arg],
        arg_abis: vec![ctx.elem_abi],
        ret_ty: LirType::Bool,
    });

    // Sentinel `-1` produced in check_bb (taken on exhaustion).
    let neg_one = alloc_value(next);
    func.block_mut(ctx.check_bb).push_synthetic(Inst::IConst {
        dst: neg_one,
        ty: LirType::I64,
        value: -1,
    });

    // check_bb: cond ? body_bb : done_bb(-1).
    func.block_mut(ctx.check_bb).terminator = Term::Branch {
        cond: ctx.cond,
        then_block: ctx.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![neg_one],
    };
    // body_bb: pred ? done_bb(i) : cont_bb.
    let cont_bb = func.add_block();
    // `i_val` is the counter block-param of check_bb; we thread it as
    // the result when the predicate matches on the current element.
    let i_val = ctx.i_val;
    func.block_mut(ctx.body_bb).terminator = Term::Branch {
        cond: pred,
        then_block: ctx.done_bb,
        then_args: vec![i_val],
        else_block: cont_bb,
        else_args: vec![],
    };
    // cont_bb: jump back with next_i.
    func.block_mut(cont_bb).terminator = Term::Jump(ctx.check_bb, vec![ctx.next_i]);

    // done_bb(result: i64): remaining + orig_term.
    func.block_mut(ctx.done_bb).params.push((dst, LirType::I64));
    let done = ctx.done_bb;
    let pre_len = func.block(done).insts.len();
    let pre_spans_len = func.block(done).span_map.len();
    func.block_mut(done).insts = remaining;
    // Synthetic scaffold insts already pushed into done_bb (e.g. a
    // SlotLoad of the result) keep their existing `None` spans; tail
    // insts inherit the spans we captured from the original block.
    let mut combined_spans: Vec<Option<crate::span::Span>> =
        if pre_spans_len == pre_len { func.block(done).span_map.clone() } else { vec![None; pre_len] };
    combined_spans.extend(remaining_spans);
    func.block_mut(done).span_map = combined_spans;
    func.block_mut(done).terminator = orig_term;
    func.block_mut(done).terminator_span = orig_term_span;
}

/// Expand `HofExpand { hof_op: Find, dst, … }` — returns `Some(elem)` for
/// the first matching element, or `None` if no element matches.
///
/// The `dst` slot is typed `Option[T]`. Rather than thread an aggregate
/// through SSA block args, the expansion uses a fresh stack slot of
/// `Option[T]`, writes the tag + payload into it during the loop, then
/// `SlotLoad`s it at `done_bb` into `dst`. This matches the C and LLVM
/// backends' current behavior (both use alloca + memcpy on match).
///
/// For now, element types must be scalar — the `Store` into the payload
/// field assumes a scalar value. Aggregate-element `find` still flows
/// through the backend handler.
#[allow(clippy::too_many_arguments)]
fn expand_find(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    element_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    option_sid: StructId,
    dst: ValueId,
    remaining: Vec<Inst>,
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
) {
    // Allocate the Option[T] result slot.
    let option_ty = LirType::Struct(option_sid);
    let option_slot = func.add_slot(option_ty.clone(), None);
    let cur = BlockId(current_bb as u32);

    // current_bb: init slot to None (tag=1) via primitive FieldPtr + Store
    // rather than Inst::EnumInit — the BIR pass won't re-scan current_bb
    // for canonical ops after this point.
    let out_addr = alloc_value(next);
    func.block_mut(cur).push_synthetic(Inst::SlotAddr {
        dst: out_addr,
        slot: option_slot,
    });
    let tag_ptr0 = alloc_value(next);
    func.block_mut(cur).push_synthetic(Inst::FieldPtr {
        dst: tag_ptr0,
        base: out_addr,
        struct_id: option_sid,
        field: 0,
    });
    let none_tag = alloc_value(next);
    func.block_mut(cur).push_synthetic(Inst::IConst {
        dst: none_tag,
        ty: LirType::I32,
        value: 1, // None
    });
    func.block_mut(cur).push_synthetic(Inst::Store {
        ptr: tag_ptr0,
        value: none_tag,
    });

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
        None,
    );

    // body_bb: CallClosure(closure, [elem]) → pred: Bool.
    let pred = alloc_value(next);
    func.block_mut(ctx.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(pred),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.elem_arg],
        arg_abis: vec![ctx.elem_abi],
        ret_ty: LirType::Bool,
    });

    // check_bb: cond ? body_bb : done_bb.
    func.block_mut(ctx.check_bb).terminator = Term::Branch {
        cond: ctx.cond,
        then_block: ctx.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![],
    };

    // body_bb: pred ? found_bb : cont_bb.
    let found_bb = func.add_block();
    let cont_bb = func.add_block();
    func.block_mut(ctx.body_bb).terminator = Term::Branch {
        cond: pred,
        then_block: found_bb,
        then_args: vec![],
        else_block: cont_bb,
        else_args: vec![],
    };

    // found_bb: set tag=0 (Some) and write the payload. For scalar
    // elements a plain `Store` does the job; for aggregate elements
    // (String, user struct) we `Memcpy` from the source pointer into
    // the payload field because Store on a `Ptr` value wouldn't copy
    // the struct bytes, only the pointer. This is why find was
    // scalar-only before.
    let tag_ptr1 = alloc_value(next);
    func.block_mut(found_bb).push_synthetic(Inst::FieldPtr {
        dst: tag_ptr1,
        base: out_addr,
        struct_id: option_sid,
        field: 0,
    });
    let some_tag = alloc_value(next);
    func.block_mut(found_bb).push_synthetic(Inst::IConst {
        dst: some_tag,
        ty: LirType::I32,
        value: 0, // Some
    });
    func.block_mut(found_bb).push_synthetic(Inst::Store {
        ptr: tag_ptr1,
        value: some_tag,
    });
    let pay_ptr = alloc_value(next);
    func.block_mut(found_bb).push_synthetic(Inst::FieldPtr {
        dst: pay_ptr,
        base: out_addr,
        struct_id: option_sid,
        field: 1,
    });
    if element_ty.is_aggregate() {
        let size = c_sizeof_lir_type(&element_ty, structs) as i64;
        let size_val = alloc_value(next);
        func.block_mut(found_bb).push_synthetic(Inst::IConst {
            dst: size_val,
            ty: LirType::I64,
            value: size,
        });
        func.block_mut(found_bb).push_synthetic(Inst::Memcpy {
            dst_ptr: pay_ptr,
            src_ptr: ctx.elem_ptr,
            size: size_val,
        });
    } else {
        func.block_mut(found_bb).push_synthetic(Inst::Store {
            ptr: pay_ptr,
            value: ctx.elem_arg,
        });
    }
    func.block_mut(found_bb).terminator = Term::Jump(ctx.done_bb, vec![]);

    // cont_bb: jump back to check with next_i.
    func.block_mut(cont_bb).terminator = Term::Jump(ctx.check_bb, vec![ctx.next_i]);

    // done_bb: SlotLoad the option result into dst.
    func.block_mut(ctx.done_bb).push_synthetic(Inst::SlotLoad {
        dst,
        slot: option_slot,
        ty: option_ty,
    });
    func.block_mut(ctx.done_bb).insts.extend(remaining);
    // Mirror the extend on `span_map` so the parallel-array invariant
    // holds — `done_bb` already has spans for synthetic scaffold insts
    // pushed by this expander; appending `remaining_spans` aligns the
    // tail with the corresponding `remaining` insts.
    {
        let done = ctx.done_bb;
        let new_len = func.block(done).insts.len();
        if func.block(done).span_map.len() < new_len {
            // Pre-extend span_map to a clean parallel state if it had
            // drifted, then extend with remaining_spans.
            let cur_len = func.block(done).span_map.len();
            let scaffold_len = new_len - remaining_spans.len();
            if cur_len < scaffold_len {
                func.block_mut(done)
                    .span_map
                    .resize(scaffold_len, None);
            }
            func.block_mut(done).span_map.extend(remaining_spans);
        } else {
            func.block_mut(done).span_map.extend(remaining_spans);
        }
    }
    func.block_mut(ctx.done_bb).terminator = orig_term;
    func.block_mut(ctx.done_bb).terminator_span = orig_term_span;
}

/// Expand `HofExpand { hof_op: Filter, dst, … }` — build a fresh
/// `GorgetArray` containing only the elements for which the predicate
/// returns true.
///
/// The result slot is allocated at the function level and the final
/// array is loaded into `dst` at `done_bb`. On match, the source
/// element's pointer (from the scaffold's `ElemPtr`) is passed
/// directly to `gorget_array_push` — mirroring the C backend's
/// existing inlining (`gorget_array_push(&__result, &__elem)`).
#[allow(clippy::too_many_arguments)]
fn expand_filter(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    element_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    dst: ValueId,
    remaining: Vec<Inst>,
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
) {
    let garray_sid = lookup_struct_id(structs, "GorgetArray").unwrap_or(StructId(0));
    let garray_ty = LirType::Struct(garray_sid);
    let cur = BlockId(current_bb as u32);

    // current_bb: allocate result slot + init via gorget_array_new.
    let result_slot = func.add_slot(garray_ty.clone(), None);
    let elem_size_val = alloc_value(next);
    let elem_size = c_sizeof_lir_type(&element_ty, structs) as i64;
    func.block_mut(cur).push_synthetic(Inst::IConst {
        dst: elem_size_val,
        ty: LirType::I64,
        value: elem_size,
    });
    let arr_val = alloc_value(next);
    func.block_mut(cur).push_synthetic(Inst::CallExtern {
        dst: Some(arr_val),
        name: "gorget_array_new".to_string(),
        args: vec![elem_size_val],
        arg_abis: vec![crate::ir::abi::AbiKind::Scalar],
    });
    func.block_mut(cur).push_synthetic(Inst::SlotStore {
        slot: result_slot,
        value: arr_val,
        is_move: true,
    });

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
        None,
    );

    // body_bb: CallClosure(closure, [elem]) → pred: Bool.
    let pred = alloc_value(next);
    func.block_mut(ctx.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(pred),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.elem_arg],
        arg_abis: vec![ctx.elem_abi],
        ret_ty: LirType::Bool,
    });

    // check_bb: cond ? body_bb : done_bb.
    func.block_mut(ctx.check_bb).terminator = Term::Branch {
        cond: ctx.cond,
        then_block: ctx.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![],
    };

    // body_bb: pred ? push_bb : cont_bb.
    let push_bb = func.add_block();
    let cont_bb = func.add_block();
    func.block_mut(ctx.body_bb).terminator = Term::Branch {
        cond: pred,
        then_block: push_bb,
        then_args: vec![],
        else_block: cont_bb,
        else_args: vec![],
    };

    // push_bb: push elem_ptr into the result array.
    let result_addr = alloc_value(next);
    func.block_mut(push_bb).push_synthetic(Inst::SlotAddr {
        dst: result_addr,
        slot: result_slot,
    });
    func.block_mut(push_bb).push_synthetic(Inst::CallExtern {
        dst: None,
        name: "gorget_array_push".to_string(),
        args: vec![result_addr, ctx.elem_ptr],
        arg_abis: vec![
            crate::ir::abi::AbiKind::Ptr,
            crate::ir::abi::AbiKind::Ptr,
        ],
    });
    func.block_mut(push_bb).terminator = Term::Jump(cont_bb, vec![]);

    // cont_bb: jump back to check with next_i.
    func.block_mut(cont_bb).terminator = Term::Jump(ctx.check_bb, vec![ctx.next_i]);

    // done_bb: SlotLoad the result array into dst + remaining + orig_term.
    func.block_mut(ctx.done_bb).push_synthetic(Inst::SlotLoad {
        dst,
        slot: result_slot,
        ty: garray_ty,
    });
    func.block_mut(ctx.done_bb).insts.extend(remaining);
    // Mirror the extend on `span_map` so the parallel-array invariant
    // holds — `done_bb` already has spans for synthetic scaffold insts
    // pushed by this expander; appending `remaining_spans` aligns the
    // tail with the corresponding `remaining` insts.
    {
        let done = ctx.done_bb;
        let new_len = func.block(done).insts.len();
        if func.block(done).span_map.len() < new_len {
            // Pre-extend span_map to a clean parallel state if it had
            // drifted, then extend with remaining_spans.
            let cur_len = func.block(done).span_map.len();
            let scaffold_len = new_len - remaining_spans.len();
            if cur_len < scaffold_len {
                func.block_mut(done)
                    .span_map
                    .resize(scaffold_len, None);
            }
            func.block_mut(done).span_map.extend(remaining_spans);
        } else {
            func.block_mut(done).span_map.extend(remaining_spans);
        }
    }
    func.block_mut(ctx.done_bb).terminator = orig_term;
    func.block_mut(ctx.done_bb).terminator_span = orig_term_span;
}

/// Expand `HofExpand { hof_op: Map, dst, … }` — build a fresh
/// `GorgetArray` of the closure's return values.
///
/// Like `filter`, the result slot is allocated at the function level
/// and loaded into `dst` at `done_bb`. Unlike `filter`, every iteration
/// produces a value (`new_elem`) that must be pushed — the SSA result
/// is spilled to a fresh slot via `Inst::AddressOf` so
/// `gorget_array_push` can take its address. AddressOf expansion
/// is idempotent across loop iterations: it allocates one slot at
/// BIR-lowering time and the slot is reused on every push.
///
/// For this pathfinder the closure return is gated to scalar types in
/// the LIR emitter; the BIR expansion itself works for any type as
/// long as the generated `Store` in AddressOf's expansion does the
/// right thing (aggregates require memcpy, which the backend Store
/// dispatch handles via `val_types`).
#[allow(clippy::too_many_arguments)]
fn expand_map(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    element_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    closure_ret_ty: LirType,
    dst: ValueId,
    remaining: Vec<Inst>,
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
) {
    let garray_sid = lookup_struct_id(structs, "GorgetArray").unwrap_or(StructId(0));
    let garray_ty = LirType::Struct(garray_sid);
    let cur = BlockId(current_bb as u32);

    // current_bb: allocate result slot + init via gorget_array_new,
    // sized by the closure's return type (not the source element type).
    let result_slot = func.add_slot(garray_ty.clone(), None);
    let ret_sz = c_sizeof_lir_type(&closure_ret_ty, structs) as i64;
    let ret_sz_val = alloc_value(next);
    func.block_mut(cur).push_synthetic(Inst::IConst {
        dst: ret_sz_val,
        ty: LirType::I64,
        value: ret_sz,
    });
    let arr_val = alloc_value(next);
    func.block_mut(cur).push_synthetic(Inst::CallExtern {
        dst: Some(arr_val),
        name: "gorget_array_new".to_string(),
        args: vec![ret_sz_val],
        arg_abis: vec![crate::ir::abi::AbiKind::Scalar],
    });
    func.block_mut(cur).push_synthetic(Inst::SlotStore {
        slot: result_slot,
        value: arr_val,
        is_move: true,
    });

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
        None,
    );

    // body_bb: call closure → new_elem; AddressOf new_elem; push.
    let new_elem = alloc_value(next);
    func.block_mut(ctx.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(new_elem),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.elem_arg],
        arg_abis: vec![ctx.elem_abi],
        ret_ty: closure_ret_ty.clone(),
    });
    let new_elem_ptr = alloc_value(next);
    func.block_mut(ctx.body_bb).push_synthetic(Inst::AddressOf {
        dst: new_elem_ptr,
        value: new_elem,
        ty: closure_ret_ty,
    });
    let result_addr = alloc_value(next);
    func.block_mut(ctx.body_bb).push_synthetic(Inst::SlotAddr {
        dst: result_addr,
        slot: result_slot,
    });
    func.block_mut(ctx.body_bb).push_synthetic(Inst::CallExtern {
        dst: None,
        name: "gorget_array_push".to_string(),
        args: vec![result_addr, new_elem_ptr],
        arg_abis: vec![
            crate::ir::abi::AbiKind::Ptr,
            crate::ir::abi::AbiKind::Ptr,
        ],
    });

    // check_bb: cond ? body_bb : done_bb.
    func.block_mut(ctx.check_bb).terminator = Term::Branch {
        cond: ctx.cond,
        then_block: ctx.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![],
    };
    // body_bb: jump back to check with next_i.
    func.block_mut(ctx.body_bb).terminator =
        Term::Jump(ctx.check_bb, vec![ctx.next_i]);

    // done_bb: SlotLoad result into dst + remaining + orig_term.
    func.block_mut(ctx.done_bb).push_synthetic(Inst::SlotLoad {
        dst,
        slot: result_slot,
        ty: garray_ty,
    });
    func.block_mut(ctx.done_bb).insts.extend(remaining);
    // Mirror the extend on `span_map` so the parallel-array invariant
    // holds — `done_bb` already has spans for synthetic scaffold insts
    // pushed by this expander; appending `remaining_spans` aligns the
    // tail with the corresponding `remaining` insts.
    {
        let done = ctx.done_bb;
        let new_len = func.block(done).insts.len();
        if func.block(done).span_map.len() < new_len {
            // Pre-extend span_map to a clean parallel state if it had
            // drifted, then extend with remaining_spans.
            let cur_len = func.block(done).span_map.len();
            let scaffold_len = new_len - remaining_spans.len();
            if cur_len < scaffold_len {
                func.block_mut(done)
                    .span_map
                    .resize(scaffold_len, None);
            }
            func.block_mut(done).span_map.extend(remaining_spans);
        } else {
            func.block_mut(done).span_map.extend(remaining_spans);
        }
    }
    func.block_mut(ctx.done_bb).terminator = orig_term;
    func.block_mut(ctx.done_bb).terminator_span = orig_term_span;
}

/// Expand `HofExpand { hof_op: FlatMap, dst, … }` — build a
/// `GorgetArray` by concatenating the Vector returned by the closure
/// for each source element.
///
/// Per iteration:
///     sub = closure(elem)               // Vector<T> (GorgetArray)
///     gorget_array_extend(result, &sub)
///
/// `gorget_array_extend(dst, src)` appends all elements of `src` into
/// `dst` (memcpy + len bump) and leaves `src` logically consumed —
/// matching what the backend inliners do today.
#[allow(clippy::too_many_arguments)]
fn expand_flat_map(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    element_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    closure_ret_ty: LirType,
    dst: ValueId,
    remaining: Vec<Inst>,
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
) {
    let garray_sid = lookup_struct_id(structs, "GorgetArray").unwrap_or(StructId(0));
    let garray_ty = LirType::Struct(garray_sid);
    let cur = BlockId(current_bb as u32);

    // current_bb: allocate result slot + init via gorget_array_new.
    // Result elem_size = source elem_size (flat_map preserves element
    // type; the closure returns a Vector of the SAME element type).
    let result_slot = func.add_slot(garray_ty.clone(), None);
    let elem_size_val = alloc_value(next);
    let elem_size = c_sizeof_lir_type(&element_ty, structs) as i64;
    func.block_mut(cur).push_synthetic(Inst::IConst {
        dst: elem_size_val,
        ty: LirType::I64,
        value: elem_size,
    });
    let arr_val = alloc_value(next);
    func.block_mut(cur).push_synthetic(Inst::CallExtern {
        dst: Some(arr_val),
        name: "gorget_array_new".to_string(),
        args: vec![elem_size_val],
        arg_abis: vec![crate::ir::abi::AbiKind::Scalar],
    });
    func.block_mut(cur).push_synthetic(Inst::SlotStore {
        slot: result_slot,
        value: arr_val,
        is_move: true,
    });

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
        None,
    );

    // body_bb: sub = closure(elem); AddressOf(sub) → sub_ptr;
    //          gorget_array_extend(result_addr, sub_ptr).
    let sub_vec = alloc_value(next);
    func.block_mut(ctx.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(sub_vec),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.elem_arg],
        arg_abis: vec![ctx.elem_abi],
        ret_ty: closure_ret_ty.clone(),
    });
    let sub_ptr = alloc_value(next);
    func.block_mut(ctx.body_bb).push_synthetic(Inst::AddressOf {
        dst: sub_ptr,
        value: sub_vec,
        ty: closure_ret_ty,
    });
    let result_addr = alloc_value(next);
    func.block_mut(ctx.body_bb).push_synthetic(Inst::SlotAddr {
        dst: result_addr,
        slot: result_slot,
    });
    func.block_mut(ctx.body_bb).push_synthetic(Inst::CallExtern {
        dst: None,
        name: "gorget_array_extend".to_string(),
        args: vec![result_addr, sub_ptr],
        arg_abis: vec![
            crate::ir::abi::AbiKind::Ptr,
            crate::ir::abi::AbiKind::Ptr,
        ],
    });

    // check_bb: cond ? body_bb : done_bb.
    func.block_mut(ctx.check_bb).terminator = Term::Branch {
        cond: ctx.cond,
        then_block: ctx.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![],
    };
    // body_bb: jump back to check with next_i.
    func.block_mut(ctx.body_bb).terminator =
        Term::Jump(ctx.check_bb, vec![ctx.next_i]);

    // done_bb: SlotLoad result into dst + remaining + orig_term.
    func.block_mut(ctx.done_bb).push_synthetic(Inst::SlotLoad {
        dst,
        slot: result_slot,
        ty: garray_ty,
    });
    func.block_mut(ctx.done_bb).insts.extend(remaining);
    // Mirror the extend on `span_map` so the parallel-array invariant
    // holds — `done_bb` already has spans for synthetic scaffold insts
    // pushed by this expander; appending `remaining_spans` aligns the
    // tail with the corresponding `remaining` insts.
    {
        let done = ctx.done_bb;
        let new_len = func.block(done).insts.len();
        if func.block(done).span_map.len() < new_len {
            // Pre-extend span_map to a clean parallel state if it had
            // drifted, then extend with remaining_spans.
            let cur_len = func.block(done).span_map.len();
            let scaffold_len = new_len - remaining_spans.len();
            if cur_len < scaffold_len {
                func.block_mut(done)
                    .span_map
                    .resize(scaffold_len, None);
            }
            func.block_mut(done).span_map.extend(remaining_spans);
        } else {
            func.block_mut(done).span_map.extend(remaining_spans);
        }
    }
    func.block_mut(ctx.done_bb).terminator = orig_term;
    func.block_mut(ctx.done_bb).terminator_span = orig_term_span;
}

/// Scaffold for Dict (`GorgetMap`) HOF loops.
///
/// Dict iteration walks either the LEGACY sparse table (`cap` slots
/// with an occupancy filter on `states[i] == 1`) or the DENSE packed
/// entries array (`entries_len` slots, no state check). The runtime
/// discriminator is `m->entries_keys != NULL` — set at ctor time and
/// invariant for the map's lifetime — so we dispatch ONCE at loop
/// entry and emit two independent loops that both funnel into a shared
/// `done_bb`. Per-iteration blocks:
///
/// ```text
///   current_bb:
///     <pre-HofExpand insts>
///     disc = Load(FieldPtr(coll, 19))   # entries_keys
///     is_dense = Cmp(Ne, disc, NULL)
///     branch is_dense, dense.check_bb(0, extras...), legacy.check_bb(0, extras...)
///
///   ── LEGACY branch (sparse hash table walk) ──
///   legacy.check_bb(i: i64, extras...):
///     cap = Load(FieldPtr(coll, 1))
///     cond = Cmp(Lt, i, cap)
///     branch cond, legacy_state_bb, done_bb
///   legacy_state_bb:
///     states = Load(FieldPtr(coll, 3))
///     state_i = Load(ElemPtr(states, i, 1), U8)
///     occupied = Cmp(Eq, state_i, 1_u8)
///     branch occupied, legacy.body_bb, legacy.advance_bb
///   legacy.body_bb:
///     keys = Load(FieldPtr(coll, 0))
///     key_ptr = ElemPtr(keys, i, key_size)
///     values = Load(FieldPtr(coll, 2))
///     val_ptr = ElemPtr(values, i, val_size)
///     <variant appends CallClosure + terminator>
///   legacy.advance_bb:
///     next_i = Add(i, 1); jmp legacy.check_bb(next_i, carried-extras...)
///
///   ── DENSE branch (packed entries array walk) ──
///   dense.check_bb(i: i64, extras...):
///     len = Load(FieldPtr(coll, 21))   # entries_len
///     cond = Cmp(Lt, i, len)
///     branch cond, dense.body_bb, done_bb
///   dense.body_bb:
///     ek = Load(FieldPtr(coll, 19))    # entries_keys
///     key_ptr = ElemPtr(ek, i, key_size)
///     ev = Load(FieldPtr(coll, 20))    # entries_values
///     val_ptr = ElemPtr(ev, i, val_size)
///     <variant appends CallClosure + terminator>
///   dense.advance_bb:
///     next_i = Add(i, 1); jmp dense.check_bb(next_i, carried-extras...)
///
///   done_bb: (variant fills insts + terminator)
/// ```
///
/// `body_bb` terminators are NOT set — the caller extends each (each's
/// variant jumps to the matching advance_bb after `CallClosure`; other
/// variants may branch differently). Both `advance_bb`s' terminators
/// ARE set to the back-edge; the caller fills carried-extras via each
/// branch's `advance_extra_params`.
#[allow(dead_code)]
struct DictHofLoopBranch {
    check_bb: BlockId,
    body_bb: BlockId,
    advance_bb: BlockId,
    i_val: ValueId,
    next_i: ValueId,
    cap_cond: ValueId,
    key_ptr: ValueId,
    val_ptr: ValueId,
    key_arg: ValueId,
    val_arg: ValueId,
    key_abi: crate::ir::abi::AbiKind,
    val_abi: crate::ir::abi::AbiKind,
    /// Block-param ValueIds on this branch's `check_bb` (for each entry in
    /// `extra_check_inits`). Read from body_bb to access the current
    /// iteration's accumulator.
    extra_check_params: Vec<ValueId>,
}

#[allow(dead_code)]
struct DictHofLoopCtx {
    legacy: DictHofLoopBranch,
    legacy_state_bb: BlockId,
    dense: DictHofLoopBranch,
    done_bb: BlockId,
}

#[allow(clippy::too_many_arguments)]
fn emit_dict_hof_loop_scaffold(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    key_ty: &LirType,
    val_ty: &LirType,
    key_abi_hint: Option<crate::ir::abi::AbiKind>,
    val_abi_hint: Option<crate::ir::abi::AbiKind>,
    extra_check_inits: Vec<(LirType, ValueId)>,
) -> DictHofLoopCtx {
    let gmap_sid = lookup_struct_id(structs, "GorgetMap").unwrap_or(StructId(0));
    let key_size = c_sizeof_lir_type(key_ty, structs) as u32;
    let val_size = c_sizeof_lir_type(val_ty, structs) as u32;

    // ── LEGACY loop blocks ──
    let l_check_bb = func.add_block();
    let l_state_bb = func.add_block();
    let l_body_bb = func.add_block();
    let l_advance_bb = func.add_block();
    // ── DENSE loop blocks (no state_bb — packed) ──
    let d_check_bb = func.add_block();
    let d_body_bb = func.add_block();
    let d_advance_bb = func.add_block();
    // Shared exit.
    let done_bb = func.add_block();

    // ─────────────── Per-branch block params ───────────────
    // LEGACY check_bb params: counter + extras.
    let l_i_val = alloc_value(next);
    func.block_mut(l_check_bb).params.push((l_i_val, LirType::I64));
    let mut l_extra_check_params: Vec<ValueId> = Vec::with_capacity(extra_check_inits.len());
    for (ty, _) in &extra_check_inits {
        let p = alloc_value(next);
        func.block_mut(l_check_bb).params.push((p, ty.clone()));
        l_extra_check_params.push(p);
    }
    let mut l_advance_extra_params: Vec<ValueId> = Vec::with_capacity(extra_check_inits.len());
    for (ty, _) in &extra_check_inits {
        let p = alloc_value(next);
        func.block_mut(l_advance_bb).params.push((p, ty.clone()));
        l_advance_extra_params.push(p);
    }
    // DENSE check_bb params.
    let d_i_val = alloc_value(next);
    func.block_mut(d_check_bb).params.push((d_i_val, LirType::I64));
    let mut d_extra_check_params: Vec<ValueId> = Vec::with_capacity(extra_check_inits.len());
    for (ty, _) in &extra_check_inits {
        let p = alloc_value(next);
        func.block_mut(d_check_bb).params.push((p, ty.clone()));
        d_extra_check_params.push(p);
    }
    let mut d_advance_extra_params: Vec<ValueId> = Vec::with_capacity(extra_check_inits.len());
    for (ty, _) in &extra_check_inits {
        let p = alloc_value(next);
        func.block_mut(d_advance_bb).params.push((p, ty.clone()));
        d_advance_extra_params.push(p);
    }

    // ─────────────── Entry: discriminator dispatch ───────────────
    // `disc = Load(FieldPtr(coll, 19 /* entries_keys */)); is_dense = disc != NULL`
    // Field 19 is beyond the LIR StructDef's tracked slots (13 fields),
    // so FieldPtr emits the byte-offset fallback (field * sizeof(void*) = 152B),
    // which matches the C struct layout (all fields 8 bytes, no padding).
    let disc_ptr = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32)).push_synthetic(Inst::FieldPtr {
        dst: disc_ptr,
        base: coll,
        struct_id: gmap_sid,
        field: 19, // entries_keys
    });
    let disc = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32)).push_synthetic(Inst::Load {
        dst: disc,
        ptr: disc_ptr,
        ty: LirType::Ptr,
    });
    let null_ptr = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32)).push_synthetic(Inst::IConst {
        dst: null_ptr,
        ty: LirType::Ptr,
        value: 0,
    });
    let is_dense = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32)).push_synthetic(Inst::Cmp {
        dst: is_dense,
        op: CmpOp::Ne,
        lhs: disc,
        rhs: null_ptr,
    });
    // Entry args: (0, init0, init1, ...) for both branches.
    let zero = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32)).push_synthetic(Inst::IConst {
        dst: zero,
        ty: LirType::I64,
        value: 0,
    });
    let mut entry_args_dense: Vec<ValueId> = Vec::with_capacity(1 + extra_check_inits.len());
    entry_args_dense.push(zero);
    for (_, init) in &extra_check_inits {
        entry_args_dense.push(*init);
    }
    let entry_args_legacy = entry_args_dense.clone();
    func.block_mut(BlockId(current_bb as u32)).terminator = Term::Branch {
        cond: is_dense,
        then_block: d_check_bb,
        then_args: entry_args_dense,
        else_block: l_check_bb,
        else_args: entry_args_legacy,
    };

    // ─────────────── LEGACY branch ───────────────
    // check_bb: cap load + bounds.
    let cap_ptr = alloc_value(next);
    func.block_mut(l_check_bb).push_synthetic(Inst::FieldPtr {
        dst: cap_ptr,
        base: coll,
        struct_id: gmap_sid,
        field: 1, // GorgetMap.cap
    });
    let cap = alloc_value(next);
    func.block_mut(l_check_bb).push_synthetic(Inst::Load {
        dst: cap,
        ptr: cap_ptr,
        ty: LirType::I64,
    });
    let l_cap_cond = alloc_value(next);
    func.block_mut(l_check_bb).push_synthetic(Inst::Cmp {
        dst: l_cap_cond,
        op: CmpOp::Lt,
        lhs: l_i_val,
        rhs: cap,
    });
    // Caller sets l_check_bb.terminator to Branch(l_cap_cond, l_state_bb, done_bb, ...).

    // state_bb: read states[i], check == 1.
    let states_ptr_field = alloc_value(next);
    func.block_mut(l_state_bb).push_synthetic(Inst::FieldPtr {
        dst: states_ptr_field,
        base: coll,
        struct_id: gmap_sid,
        field: 3,
    });
    let states_ptr = alloc_value(next);
    func.block_mut(l_state_bb).push_synthetic(Inst::Load {
        dst: states_ptr,
        ptr: states_ptr_field,
        ty: LirType::Ptr,
    });
    let state_i_ptr = alloc_value(next);
    func.block_mut(l_state_bb).push_synthetic(Inst::ElemPtr {
        dst: state_i_ptr,
        base: states_ptr,
        index: l_i_val,
        elem_size: 1,
    });
    let state_val = alloc_value(next);
    func.block_mut(l_state_bb).push_synthetic(Inst::Load {
        dst: state_val,
        ptr: state_i_ptr,
        ty: LirType::U8,
    });
    let one_u8 = alloc_value(next);
    func.block_mut(l_state_bb).push_synthetic(Inst::IConst {
        dst: one_u8,
        ty: LirType::U8,
        value: 1,
    });
    let occupied = alloc_value(next);
    func.block_mut(l_state_bb).push_synthetic(Inst::Cmp {
        dst: occupied,
        op: CmpOp::Eq,
        lhs: state_val,
        rhs: one_u8,
    });
    let l_skip_args: Vec<ValueId> = l_extra_check_params.iter().copied().collect();
    func.block_mut(l_state_bb).terminator = Term::Branch {
        cond: occupied,
        then_block: l_body_bb,
        then_args: vec![],
        else_block: l_advance_bb,
        else_args: l_skip_args,
    };

    // legacy body_bb: keys[i] + values[i].
    let (l_key_ptr, l_val_ptr, l_key_arg, l_val_arg, l_key_abi, l_val_abi) =
        emit_dict_body_prelude(
            func,
            next,
            l_body_bb,
            coll,
            gmap_sid,
            l_i_val,
            key_ty,
            val_ty,
            key_size,
            val_size,
            key_abi_hint,
            val_abi_hint,
            /* dense */ false,
        );

    // legacy advance_bb: next_i, jump back.
    let l_next_i = emit_advance_backedge(
        func, next, l_advance_bb, l_check_bb, l_i_val, &l_advance_extra_params,
    );

    // ─────────────── DENSE branch ───────────────
    // check_bb: entries_len (field 21) as bound.
    let elen_ptr = alloc_value(next);
    func.block_mut(d_check_bb).push_synthetic(Inst::FieldPtr {
        dst: elen_ptr,
        base: coll,
        struct_id: gmap_sid,
        field: 21, // entries_len
    });
    let elen = alloc_value(next);
    func.block_mut(d_check_bb).push_synthetic(Inst::Load {
        dst: elen,
        ptr: elen_ptr,
        ty: LirType::I64,
    });
    let d_cap_cond = alloc_value(next);
    func.block_mut(d_check_bb).push_synthetic(Inst::Cmp {
        dst: d_cap_cond,
        op: CmpOp::Lt,
        lhs: d_i_val,
        rhs: elen,
    });
    // Caller sets d_check_bb.terminator to Branch(d_cap_cond, d_body_bb, done_bb, ...).

    // dense body_bb: entries_keys[i] + entries_values[i]. Dense is
    // packed — no state_bb needed.
    let (d_key_ptr, d_val_ptr, d_key_arg, d_val_arg, d_key_abi, d_val_abi) =
        emit_dict_body_prelude(
            func,
            next,
            d_body_bb,
            coll,
            gmap_sid,
            d_i_val,
            key_ty,
            val_ty,
            key_size,
            val_size,
            key_abi_hint,
            val_abi_hint,
            /* dense */ true,
        );

    // dense advance_bb.
    let d_next_i = emit_advance_backedge(
        func, next, d_advance_bb, d_check_bb, d_i_val, &d_advance_extra_params,
    );

    DictHofLoopCtx {
        legacy: DictHofLoopBranch {
            check_bb: l_check_bb,
            body_bb: l_body_bb,
            advance_bb: l_advance_bb,
            i_val: l_i_val,
            next_i: l_next_i,
            cap_cond: l_cap_cond,
            key_ptr: l_key_ptr,
            val_ptr: l_val_ptr,
            key_arg: l_key_arg,
            val_arg: l_val_arg,
            key_abi: l_key_abi,
            val_abi: l_val_abi,
            extra_check_params: l_extra_check_params,
        },
        legacy_state_bb: l_state_bb,
        dense: DictHofLoopBranch {
            check_bb: d_check_bb,
            body_bb: d_body_bb,
            advance_bb: d_advance_bb,
            i_val: d_i_val,
            next_i: d_next_i,
            cap_cond: d_cap_cond,
            key_ptr: d_key_ptr,
            val_ptr: d_val_ptr,
            key_arg: d_key_arg,
            val_arg: d_val_arg,
            key_abi: d_key_abi,
            val_abi: d_val_abi,
            extra_check_params: d_extra_check_params,
        },
        done_bb,
    }
}

/// Shared body prelude for Dict HOF loops. Emits key_ptr / val_ptr
/// (and, per ABI hint, key_arg / val_arg loads) for the given loop
/// mode. `dense=false` reads the legacy sparse `keys` / `values`
/// pointers (fields 0 / 2); `dense=true` reads the packed
/// `entries_keys` / `entries_values` pointers (fields 19 / 20).
#[allow(clippy::too_many_arguments)]
fn emit_dict_body_prelude(
    func: &mut LirFunction,
    next: &mut u32,
    body_bb: BlockId,
    coll: ValueId,
    gmap_sid: StructId,
    i_val: ValueId,
    key_ty: &LirType,
    val_ty: &LirType,
    key_size: u32,
    val_size: u32,
    key_abi_hint: Option<crate::ir::abi::AbiKind>,
    val_abi_hint: Option<crate::ir::abi::AbiKind>,
    dense: bool,
) -> (
    ValueId,
    ValueId,
    ValueId,
    ValueId,
    crate::ir::abi::AbiKind,
    crate::ir::abi::AbiKind,
) {
    let (keys_field_idx, vals_field_idx) = if dense { (19, 20) } else { (0, 2) };
    let keys_field = alloc_value(next);
    func.block_mut(body_bb).push_synthetic(Inst::FieldPtr {
        dst: keys_field,
        base: coll,
        struct_id: gmap_sid,
        field: keys_field_idx,
    });
    let keys_ptr = alloc_value(next);
    func.block_mut(body_bb).push_synthetic(Inst::Load {
        dst: keys_ptr,
        ptr: keys_field,
        ty: LirType::Ptr,
    });
    let key_ptr = alloc_value(next);
    func.block_mut(body_bb).push_synthetic(Inst::ElemPtr {
        dst: key_ptr,
        base: keys_ptr,
        index: i_val,
        elem_size: key_size,
    });
    let vals_field = alloc_value(next);
    func.block_mut(body_bb).push_synthetic(Inst::FieldPtr {
        dst: vals_field,
        base: coll,
        struct_id: gmap_sid,
        field: vals_field_idx,
    });
    let vals_ptr = alloc_value(next);
    func.block_mut(body_bb).push_synthetic(Inst::Load {
        dst: vals_ptr,
        ptr: vals_field,
        ty: LirType::Ptr,
    });
    let val_ptr = alloc_value(next);
    func.block_mut(body_bb).push_synthetic(Inst::ElemPtr {
        dst: val_ptr,
        base: vals_ptr,
        index: i_val,
        elem_size: val_size,
    });

    let key_pass_by_ptr = match key_abi_hint {
        Some(crate::ir::abi::AbiKind::Ptr) => true,
        Some(crate::ir::abi::AbiKind::ByValue) => false,
        Some(crate::ir::abi::AbiKind::Scalar) => false,
        _ => key_ty.is_aggregate(),
    };
    let key_arg = if key_pass_by_ptr {
        key_ptr
    } else {
        let k = alloc_value(next);
        func.block_mut(body_bb).push_synthetic(Inst::Load {
            dst: k,
            ptr: key_ptr,
            ty: key_ty.clone(),
        });
        k
    };
    let key_abi = if key_pass_by_ptr {
        crate::ir::abi::AbiKind::Ptr
    } else {
        crate::ir::abi::AbiKind::Scalar
    };
    let val_pass_by_ptr = match val_abi_hint {
        Some(crate::ir::abi::AbiKind::Ptr) => true,
        Some(crate::ir::abi::AbiKind::ByValue) => false,
        Some(crate::ir::abi::AbiKind::Scalar) => false,
        _ => val_ty.is_aggregate(),
    };
    let val_arg = if val_pass_by_ptr {
        val_ptr
    } else {
        let v = alloc_value(next);
        func.block_mut(body_bb).push_synthetic(Inst::Load {
            dst: v,
            ptr: val_ptr,
            ty: val_ty.clone(),
        });
        v
    };
    let val_abi = if val_pass_by_ptr {
        crate::ir::abi::AbiKind::Ptr
    } else {
        crate::ir::abi::AbiKind::Scalar
    };
    (key_ptr, val_ptr, key_arg, val_arg, key_abi, val_abi)
}

/// Emit `next_i = i + 1; jmp check_bb(next_i, carried-extras...)`
/// in the given advance block. Returns the `next_i` ValueId.
fn emit_advance_backedge(
    func: &mut LirFunction,
    next: &mut u32,
    advance_bb: BlockId,
    check_bb: BlockId,
    i_val: ValueId,
    advance_extra_params: &[ValueId],
) -> ValueId {
    let next_i = alloc_value(next);
    let one_i64 = alloc_value(next);
    func.block_mut(advance_bb).push_synthetic(Inst::IConst {
        dst: one_i64,
        ty: LirType::I64,
        value: 1,
    });
    func.block_mut(advance_bb).push_synthetic(Inst::Add {
        dst: next_i,
        ty: LirType::I64,
        lhs: i_val,
        rhs: one_i64,
        overflow: Overflow::Wrap,
    });
    let mut back_args = Vec::with_capacity(1 + advance_extra_params.len());
    back_args.push(next_i);
    back_args.extend(advance_extra_params.iter().copied());
    func.block_mut(advance_bb).terminator = Term::Jump(check_bb, back_args);
    next_i
}

/// Expand `HofExpand { hof_op: Each }` for Dict — call closure
/// `(key, val)` for every occupied slot.
#[allow(clippy::too_many_arguments)]
fn expand_dict_each(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    key_ty: LirType,
    val_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    remaining: Vec<Inst>,
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
) {
    let key_abi_hint = closure_arg_abis.first().copied();
    let val_abi_hint = closure_arg_abis.get(1).copied();
    let ctx = emit_dict_hof_loop_scaffold(
        func,
        current_bb,
        next,
        structs,
        coll,
        &key_ty,
        &val_ty,
        key_abi_hint,
        val_abi_hint,
        vec![],
    );

    // LEGACY: check_bb: cap_cond ? state_bb : done_bb.
    func.block_mut(ctx.legacy.check_bb).terminator = Term::Branch {
        cond: ctx.legacy.cap_cond,
        then_block: ctx.legacy_state_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![],
    };
    // LEGACY body_bb: CallClosure, then jump to advance.
    func.block_mut(ctx.legacy.body_bb).push_synthetic(Inst::CallClosure {
        dst: None,
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.legacy.key_arg, ctx.legacy.val_arg],
        arg_abis: vec![ctx.legacy.key_abi, ctx.legacy.val_abi],
        ret_ty: LirType::Void,
    });
    func.block_mut(ctx.legacy.body_bb).terminator = Term::Jump(ctx.legacy.advance_bb, vec![]);

    // DENSE: check_bb: cap_cond ? body_bb : done_bb (no state check — packed).
    func.block_mut(ctx.dense.check_bb).terminator = Term::Branch {
        cond: ctx.dense.cap_cond,
        then_block: ctx.dense.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![],
    };
    // DENSE body_bb: CallClosure, then jump to advance.
    func.block_mut(ctx.dense.body_bb).push_synthetic(Inst::CallClosure {
        dst: None,
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.dense.key_arg, ctx.dense.val_arg],
        arg_abis: vec![ctx.dense.key_abi, ctx.dense.val_abi],
        ret_ty: LirType::Void,
    });
    func.block_mut(ctx.dense.body_bb).terminator = Term::Jump(ctx.dense.advance_bb, vec![]);

    // done_bb: remaining + orig_term.
    let done = ctx.done_bb;
    let pre_len = func.block(done).insts.len();
    let pre_spans_len = func.block(done).span_map.len();
    func.block_mut(done).insts = remaining;
    // Synthetic scaffold insts already pushed into done_bb (e.g. a
    // SlotLoad of the result) keep their existing `None` spans; tail
    // insts inherit the spans we captured from the original block.
    let mut combined_spans: Vec<Option<crate::span::Span>> =
        if pre_spans_len == pre_len { func.block(done).span_map.clone() } else { vec![None; pre_len] };
    combined_spans.extend(remaining_spans);
    func.block_mut(done).span_map = combined_spans;
    func.block_mut(done).terminator = orig_term;
    func.block_mut(done).terminator_span = orig_term_span;
}

/// Expand `HofExpand { hof_op: DictFold, init, dst }` — thread a
/// scalar accumulator through the hash-table walk. Closure signature:
/// `(acc, K, V) -> acc`.
#[allow(clippy::too_many_arguments)]
fn expand_dict_fold(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    key_ty: LirType,
    val_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    closure_ret_ty: LirType,
    init: ValueId,
    dst: ValueId,
    remaining: Vec<Inst>,
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
) {
    // closure signature: (acc, K, V). closure_arg_abis = [acc_abi, key_abi, val_abi].
    let acc_abi_hint = closure_arg_abis.first().copied();
    let key_abi_hint = closure_arg_abis.get(1).copied();
    let val_abi_hint = closure_arg_abis.get(2).copied();
    let ctx = emit_dict_hof_loop_scaffold(
        func,
        current_bb,
        next,
        structs,
        coll,
        &key_ty,
        &val_ty,
        key_abi_hint,
        val_abi_hint,
        vec![(closure_ret_ty.clone(), init)],
    );

    // Acc passing: closure expects either by-ptr or by-value.
    let acc_by_ptr = match acc_abi_hint {
        Some(crate::ir::abi::AbiKind::Ptr) => true,
        Some(crate::ir::abi::AbiKind::ByValue) => false,
        Some(crate::ir::abi::AbiKind::Scalar) => false,
        _ => closure_ret_ty.is_aggregate(),
    };
    let acc_abi = if acc_by_ptr {
        crate::ir::abi::AbiKind::Ptr
    } else {
        crate::ir::abi::AbiKind::Scalar
    };

    // done_bb receives the final acc (from whichever branch ran).
    func.block_mut(ctx.done_bb).params.push((dst, closure_ret_ty.clone()));

    // ── LEGACY branch ──
    let l_acc_val = ctx.legacy.extra_check_params[0];
    let l_acc_arg = if acc_by_ptr {
        let p = alloc_value(next);
        func.block_mut(ctx.legacy.body_bb).push_synthetic(Inst::AddressOf {
            dst: p,
            value: l_acc_val,
            ty: closure_ret_ty.clone(),
        });
        p
    } else {
        l_acc_val
    };
    func.block_mut(ctx.legacy.check_bb).terminator = Term::Branch {
        cond: ctx.legacy.cap_cond,
        then_block: ctx.legacy_state_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![l_acc_val],
    };
    let l_new_acc = alloc_value(next);
    func.block_mut(ctx.legacy.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(l_new_acc),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![l_acc_arg, ctx.legacy.key_arg, ctx.legacy.val_arg],
        arg_abis: vec![acc_abi, ctx.legacy.key_abi, ctx.legacy.val_abi],
        ret_ty: closure_ret_ty.clone(),
    });
    func.block_mut(ctx.legacy.body_bb).terminator =
        Term::Jump(ctx.legacy.advance_bb, vec![l_new_acc]);

    // ── DENSE branch ──
    let d_acc_val = ctx.dense.extra_check_params[0];
    let d_acc_arg = if acc_by_ptr {
        let p = alloc_value(next);
        func.block_mut(ctx.dense.body_bb).push_synthetic(Inst::AddressOf {
            dst: p,
            value: d_acc_val,
            ty: closure_ret_ty.clone(),
        });
        p
    } else {
        d_acc_val
    };
    func.block_mut(ctx.dense.check_bb).terminator = Term::Branch {
        cond: ctx.dense.cap_cond,
        then_block: ctx.dense.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![d_acc_val],
    };
    let d_new_acc = alloc_value(next);
    func.block_mut(ctx.dense.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(d_new_acc),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![d_acc_arg, ctx.dense.key_arg, ctx.dense.val_arg],
        arg_abis: vec![acc_abi, ctx.dense.key_abi, ctx.dense.val_abi],
        ret_ty: closure_ret_ty.clone(),
    });
    func.block_mut(ctx.dense.body_bb).terminator =
        Term::Jump(ctx.dense.advance_bb, vec![d_new_acc]);
    let done = ctx.done_bb;
    let pre_len = func.block(done).insts.len();
    let pre_spans_len = func.block(done).span_map.len();
    func.block_mut(done).insts = remaining;
    // Synthetic scaffold insts already pushed into done_bb (e.g. a
    // SlotLoad of the result) keep their existing `None` spans; tail
    // insts inherit the spans we captured from the original block.
    let mut combined_spans: Vec<Option<crate::span::Span>> =
        if pre_spans_len == pre_len { func.block(done).span_map.clone() } else { vec![None; pre_len] };
    combined_spans.extend(remaining_spans);
    func.block_mut(done).span_map = combined_spans;
    func.block_mut(done).terminator = orig_term;
    func.block_mut(done).terminator_span = orig_term_span;
}

/// Expand `HofExpand { hof_op: DictAny | DictAll, dst }` — early-exit
/// predicate over a Dict. Matches the shape of `expand_any_all` for
/// Vector but iterates via the Dict scaffold (which skips
/// non-occupied slots).
#[allow(clippy::too_many_arguments)]
fn expand_dict_any_all(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    op: HofOp,
    coll: ValueId,
    key_ty: LirType,
    val_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    dst: ValueId,
    remaining: Vec<Inst>,
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
) {
    let is_any = matches!(op, HofOp::DictAny);
    let dst_ty = func
        .value_types
        .get(dst.0 as usize)
        .and_then(|t| t.as_ref())
        .cloned()
        .unwrap_or(LirType::I64);

    let key_abi_hint = closure_arg_abis.first().copied();
    let val_abi_hint = closure_arg_abis.get(1).copied();
    let ctx = emit_dict_hof_loop_scaffold(
        func,
        current_bb,
        next,
        structs,
        coll,
        &key_ty,
        &val_ty,
        key_abi_hint,
        val_abi_hint,
        vec![],
    );

    // Constants for true/false in the dst's declared type. For Bool
    // dst we emit BoolConst; otherwise IConst (matches the Vector
    // any/all convention where Bool can be lifted into i64 slots at
    // some call sites). One pair of consts per branch: `early` parks
    // in the branch's body_bb (after the CallClosure, its only user);
    // `fall` parks in the branch's check_bb (the exhaustion branch
    // reads it).
    let (early_value, fall_value) = if is_any { (1, 0) } else { (0, 1) };
    let const_inst = |d, v, dst_ty: &LirType| match dst_ty {
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

    // done_bb(result: dst_ty) — shared exit; either branch feeds it.
    func.block_mut(ctx.done_bb).params.push((dst, dst_ty.clone()));
    {
        let done = ctx.done_bb;
        let pre_len = func.block(done).insts.len();
        let pre_spans_len = func.block(done).span_map.len();
        func.block_mut(done).insts = remaining;
        let mut combined_spans: Vec<Option<crate::span::Span>> =
            if pre_spans_len == pre_len { func.block(done).span_map.clone() } else { vec![None; pre_len] };
        combined_spans.extend(remaining_spans);
        func.block_mut(done).span_map = combined_spans;
        func.block_mut(done).terminator = orig_term;
        func.block_mut(done).terminator_span = orig_term_span;
    }

    // ── LEGACY branch ──
    let l_early = alloc_value(next);
    let l_fall = alloc_value(next);
    func.block_mut(ctx.legacy.body_bb).push_synthetic(const_inst(l_early, early_value, &dst_ty));
    func.block_mut(ctx.legacy.check_bb).push_synthetic(const_inst(l_fall, fall_value, &dst_ty));
    func.block_mut(ctx.legacy.check_bb).terminator = Term::Branch {
        cond: ctx.legacy.cap_cond,
        then_block: ctx.legacy_state_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![l_fall],
    };
    let l_pred = alloc_value(next);
    func.block_mut(ctx.legacy.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(l_pred),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.legacy.key_arg, ctx.legacy.val_arg],
        arg_abis: vec![ctx.legacy.key_abi, ctx.legacy.val_abi],
        ret_ty: LirType::Bool,
    });
    let (l_then_block, l_then_args, l_else_block, l_else_args) = if is_any {
        (ctx.done_bb, vec![l_early], ctx.legacy.advance_bb, vec![])
    } else {
        (ctx.legacy.advance_bb, vec![], ctx.done_bb, vec![l_early])
    };
    func.block_mut(ctx.legacy.body_bb).terminator = Term::Branch {
        cond: l_pred,
        then_block: l_then_block,
        then_args: l_then_args,
        else_block: l_else_block,
        else_args: l_else_args,
    };

    // ── DENSE branch ──
    let d_early = alloc_value(next);
    let d_fall = alloc_value(next);
    func.block_mut(ctx.dense.body_bb).push_synthetic(const_inst(d_early, early_value, &dst_ty));
    func.block_mut(ctx.dense.check_bb).push_synthetic(const_inst(d_fall, fall_value, &dst_ty));
    func.block_mut(ctx.dense.check_bb).terminator = Term::Branch {
        cond: ctx.dense.cap_cond,
        then_block: ctx.dense.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![d_fall],
    };
    let d_pred = alloc_value(next);
    func.block_mut(ctx.dense.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(d_pred),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.dense.key_arg, ctx.dense.val_arg],
        arg_abis: vec![ctx.dense.key_abi, ctx.dense.val_abi],
        ret_ty: LirType::Bool,
    });
    let (d_then_block, d_then_args, d_else_block, d_else_args) = if is_any {
        (ctx.done_bb, vec![d_early], ctx.dense.advance_bb, vec![])
    } else {
        (ctx.dense.advance_bb, vec![], ctx.done_bb, vec![d_early])
    };
    func.block_mut(ctx.dense.body_bb).terminator = Term::Branch {
        cond: d_pred,
        then_block: d_then_block,
        then_args: d_then_args,
        else_block: d_else_block,
        else_args: d_else_args,
    };
}

/// Expand `HofExpand { hof_op: DictFilter, dst }` — build a fresh
/// `GorgetMap` of the entries that satisfy the predicate.
///
/// current_bb: CallExtern `gorget_map_new_like(src)` → result; SlotStore
/// into result_slot. The helper mirrors key_size/val_size/hash/eq and
/// all drop/clone/materialize hooks from src so the result works for
/// any K/V without per-type ctor dispatch.
///
/// body_bb: `pred = closure(key, val)`; Branch(pred, push_bb, advance_bb).
/// push_bb: `gorget_map_put_cloned(result_addr, key_ptr, val_ptr)` then
/// Jump(advance_bb). done_bb SlotLoads the result into dst.
#[allow(clippy::too_many_arguments)]
fn expand_dict_filter(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    key_ty: LirType,
    val_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    dst: ValueId,
    remaining: Vec<Inst>,
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
) {
    let gmap_sid = lookup_struct_id(structs, "GorgetMap").unwrap_or(StructId(0));
    let gmap_ty = LirType::Struct(gmap_sid);
    let cur = BlockId(current_bb as u32);

    // current_bb: allocate result slot + init via gorget_map_new_like(coll).
    let result_slot = func.add_slot(gmap_ty.clone(), None);
    let map_val = alloc_value(next);
    func.block_mut(cur).push_synthetic(Inst::CallExtern {
        dst: Some(map_val),
        name: "gorget_map_new_like".to_string(),
        args: vec![coll],
        arg_abis: vec![crate::ir::abi::AbiKind::Ptr],
    });
    func.block_mut(cur).push_synthetic(Inst::SlotStore {
        slot: result_slot,
        value: map_val,
        is_move: true,
    });

    let key_abi_hint = closure_arg_abis.first().copied();
    let val_abi_hint = closure_arg_abis.get(1).copied();
    let ctx = emit_dict_hof_loop_scaffold(
        func,
        current_bb,
        next,
        structs,
        coll,
        &key_ty,
        &val_ty,
        key_abi_hint,
        val_abi_hint,
        vec![],
    );

    // ── LEGACY branch ──
    let l_pred = alloc_value(next);
    func.block_mut(ctx.legacy.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(l_pred),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.legacy.key_arg, ctx.legacy.val_arg],
        arg_abis: vec![ctx.legacy.key_abi, ctx.legacy.val_abi],
        ret_ty: LirType::Bool,
    });
    func.block_mut(ctx.legacy.check_bb).terminator = Term::Branch {
        cond: ctx.legacy.cap_cond,
        then_block: ctx.legacy_state_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![],
    };
    let l_push_bb = func.add_block();
    func.block_mut(ctx.legacy.body_bb).terminator = Term::Branch {
        cond: l_pred,
        then_block: l_push_bb,
        then_args: vec![],
        else_block: ctx.legacy.advance_bb,
        else_args: vec![],
    };
    let l_result_addr = alloc_value(next);
    func.block_mut(l_push_bb).push_synthetic(Inst::SlotAddr {
        dst: l_result_addr,
        slot: result_slot,
    });
    func.block_mut(l_push_bb).push_synthetic(Inst::CallExtern {
        dst: None,
        name: "gorget_map_put_cloned".to_string(),
        args: vec![l_result_addr, ctx.legacy.key_ptr, ctx.legacy.val_ptr],
        arg_abis: vec![
            crate::ir::abi::AbiKind::Ptr,
            crate::ir::abi::AbiKind::Ptr,
            crate::ir::abi::AbiKind::Ptr,
        ],
    });
    func.block_mut(l_push_bb).terminator = Term::Jump(ctx.legacy.advance_bb, vec![]);

    // ── DENSE branch ──
    let d_pred = alloc_value(next);
    func.block_mut(ctx.dense.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(d_pred),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.dense.key_arg, ctx.dense.val_arg],
        arg_abis: vec![ctx.dense.key_abi, ctx.dense.val_abi],
        ret_ty: LirType::Bool,
    });
    func.block_mut(ctx.dense.check_bb).terminator = Term::Branch {
        cond: ctx.dense.cap_cond,
        then_block: ctx.dense.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![],
    };
    let d_push_bb = func.add_block();
    func.block_mut(ctx.dense.body_bb).terminator = Term::Branch {
        cond: d_pred,
        then_block: d_push_bb,
        then_args: vec![],
        else_block: ctx.dense.advance_bb,
        else_args: vec![],
    };
    let d_result_addr = alloc_value(next);
    func.block_mut(d_push_bb).push_synthetic(Inst::SlotAddr {
        dst: d_result_addr,
        slot: result_slot,
    });
    func.block_mut(d_push_bb).push_synthetic(Inst::CallExtern {
        dst: None,
        name: "gorget_map_put_cloned".to_string(),
        args: vec![d_result_addr, ctx.dense.key_ptr, ctx.dense.val_ptr],
        arg_abis: vec![
            crate::ir::abi::AbiKind::Ptr,
            crate::ir::abi::AbiKind::Ptr,
            crate::ir::abi::AbiKind::Ptr,
        ],
    });
    func.block_mut(d_push_bb).terminator = Term::Jump(ctx.dense.advance_bb, vec![]);

    // done_bb: SlotLoad the result map into dst + remaining + orig_term.
    func.block_mut(ctx.done_bb).push_synthetic(Inst::SlotLoad {
        dst,
        slot: result_slot,
        ty: gmap_ty,
    });
    func.block_mut(ctx.done_bb).insts.extend(remaining);
    // Mirror the extend on `span_map` so the parallel-array invariant
    // holds — `done_bb` already has spans for synthetic scaffold insts
    // pushed by this expander; appending `remaining_spans` aligns the
    // tail with the corresponding `remaining` insts.
    {
        let done = ctx.done_bb;
        let new_len = func.block(done).insts.len();
        if func.block(done).span_map.len() < new_len {
            // Pre-extend span_map to a clean parallel state if it had
            // drifted, then extend with remaining_spans.
            let cur_len = func.block(done).span_map.len();
            let scaffold_len = new_len - remaining_spans.len();
            if cur_len < scaffold_len {
                func.block_mut(done)
                    .span_map
                    .resize(scaffold_len, None);
            }
            func.block_mut(done).span_map.extend(remaining_spans);
        } else {
            func.block_mut(done).span_map.extend(remaining_spans);
        }
    }
    func.block_mut(ctx.done_bb).terminator = orig_term;
    func.block_mut(ctx.done_bb).terminator_span = orig_term_span;
}

/// Scaffold for Set (`GorgetSet` — a GorgetMap with no val array)
/// HOF loops. Two iteration shapes:
///   * ordered (`Set__T`): walk `order[j]` for `j in 0..order_len`,
///     resolve `i = order[j]`, check `states[i]`, read `keys[i]`.
///     Matches the insertion-order semantics of the existing
///     `emit_set_helper` ordered case.
///   * unordered (`HashSet__T`): walk `i in 0..cap`, check
///     `states[i]`, read `keys[i]`. Same shape as Dict but with no
///     value array.
///
/// The scaffold exposes `elem_arg` (loaded or pointer, per closure
/// ABI hint) and the same block-param threading pattern as the Dict
/// scaffold (advance_bb carries extras forward on both skip and
/// body paths).
#[allow(dead_code)]
struct SetHofLoopBranch {
    check_bb: BlockId,
    body_bb: BlockId,
    advance_bb: BlockId,
    counter_val: ValueId,
    next_counter: ValueId,
    cap_cond: ValueId,
    elem_ptr: ValueId,
    elem_arg: ValueId,
    elem_abi: crate::ir::abi::AbiKind,
    extra_check_params: Vec<ValueId>,
}

#[allow(dead_code)]
struct SetHofLoopCtx {
    legacy: SetHofLoopBranch,
    legacy_state_bb: BlockId,
    dense: SetHofLoopBranch,
    done_bb: BlockId,
}

#[allow(clippy::too_many_arguments)]
fn emit_set_hof_loop_scaffold(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    elem_ty: &LirType,
    elem_abi_hint: Option<crate::ir::abi::AbiKind>,
    extra_check_inits: Vec<(LirType, ValueId)>,
    is_ordered: bool,
) -> SetHofLoopCtx {
    // GorgetSet aliases GorgetMap — reuse its struct id / field layout.
    let gmap_sid = lookup_struct_id(structs, "GorgetMap").unwrap_or(StructId(0));
    let elem_size = c_sizeof_lir_type(elem_ty, structs) as u32;

    // ── LEGACY loop blocks ──
    let l_check_bb = func.add_block();
    let l_state_bb = func.add_block();
    let l_body_bb = func.add_block();
    let l_advance_bb = func.add_block();
    // ── DENSE loop blocks (no state_bb — packed, no ordering distinction) ──
    let d_check_bb = func.add_block();
    let d_body_bb = func.add_block();
    let d_advance_bb = func.add_block();
    let done_bb = func.add_block();

    // ─────────────── Per-branch block params ───────────────
    // LEGACY.
    let l_counter_val = alloc_value(next);
    func.block_mut(l_check_bb).params.push((l_counter_val, LirType::I64));
    let mut l_extra_check_params: Vec<ValueId> = Vec::with_capacity(extra_check_inits.len());
    for (ty, _) in &extra_check_inits {
        let p = alloc_value(next);
        func.block_mut(l_check_bb).params.push((p, ty.clone()));
        l_extra_check_params.push(p);
    }
    let mut l_advance_extra_params: Vec<ValueId> = Vec::with_capacity(extra_check_inits.len());
    for (ty, _) in &extra_check_inits {
        let p = alloc_value(next);
        func.block_mut(l_advance_bb).params.push((p, ty.clone()));
        l_advance_extra_params.push(p);
    }
    // DENSE.
    let d_counter_val = alloc_value(next);
    func.block_mut(d_check_bb).params.push((d_counter_val, LirType::I64));
    let mut d_extra_check_params: Vec<ValueId> = Vec::with_capacity(extra_check_inits.len());
    for (ty, _) in &extra_check_inits {
        let p = alloc_value(next);
        func.block_mut(d_check_bb).params.push((p, ty.clone()));
        d_extra_check_params.push(p);
    }
    let mut d_advance_extra_params: Vec<ValueId> = Vec::with_capacity(extra_check_inits.len());
    for (ty, _) in &extra_check_inits {
        let p = alloc_value(next);
        func.block_mut(d_advance_bb).params.push((p, ty.clone()));
        d_advance_extra_params.push(p);
    }

    // ─────────────── Entry: discriminator dispatch ───────────────
    let disc_ptr = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32)).push_synthetic(Inst::FieldPtr {
        dst: disc_ptr,
        base: coll,
        struct_id: gmap_sid,
        field: 19, // entries_keys
    });
    let disc = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32)).push_synthetic(Inst::Load {
        dst: disc,
        ptr: disc_ptr,
        ty: LirType::Ptr,
    });
    let null_ptr = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32)).push_synthetic(Inst::IConst {
        dst: null_ptr,
        ty: LirType::Ptr,
        value: 0,
    });
    let is_dense = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32)).push_synthetic(Inst::Cmp {
        dst: is_dense,
        op: CmpOp::Ne,
        lhs: disc,
        rhs: null_ptr,
    });
    let zero = alloc_value(next);
    func.block_mut(BlockId(current_bb as u32)).push_synthetic(Inst::IConst {
        dst: zero,
        ty: LirType::I64,
        value: 0,
    });
    let mut entry_args_dense: Vec<ValueId> = Vec::with_capacity(1 + extra_check_inits.len());
    entry_args_dense.push(zero);
    for (_, init) in &extra_check_inits {
        entry_args_dense.push(*init);
    }
    let entry_args_legacy = entry_args_dense.clone();
    func.block_mut(BlockId(current_bb as u32)).terminator = Term::Branch {
        cond: is_dense,
        then_block: d_check_bb,
        then_args: entry_args_dense,
        else_block: l_check_bb,
        else_args: entry_args_legacy,
    };

    // ─────────────── LEGACY branch ───────────────
    // check_bb: load bound (order_len for ordered, cap for unordered).
    let bound_field_idx: u32 = if is_ordered { 9 } else { 1 };
    let bound_ptr = alloc_value(next);
    func.block_mut(l_check_bb).push_synthetic(Inst::FieldPtr {
        dst: bound_ptr,
        base: coll,
        struct_id: gmap_sid,
        field: bound_field_idx,
    });
    let bound = alloc_value(next);
    func.block_mut(l_check_bb).push_synthetic(Inst::Load {
        dst: bound,
        ptr: bound_ptr,
        ty: LirType::I64,
    });
    let l_cap_cond = alloc_value(next);
    func.block_mut(l_check_bb).push_synthetic(Inst::Cmp {
        dst: l_cap_cond,
        op: CmpOp::Lt,
        lhs: l_counter_val,
        rhs: bound,
    });

    // state_bb: resolve `i` (either `order[j]` or `counter`) and check states[i] == 1.
    let l_i_val = if is_ordered {
        let order_field = alloc_value(next);
        func.block_mut(l_state_bb).push_synthetic(Inst::FieldPtr {
            dst: order_field,
            base: coll,
            struct_id: gmap_sid,
            field: 8,
        });
        let order_ptr = alloc_value(next);
        func.block_mut(l_state_bb).push_synthetic(Inst::Load {
            dst: order_ptr,
            ptr: order_field,
            ty: LirType::Ptr,
        });
        let order_j_ptr = alloc_value(next);
        func.block_mut(l_state_bb).push_synthetic(Inst::ElemPtr {
            dst: order_j_ptr,
            base: order_ptr,
            index: l_counter_val,
            elem_size: 8,
        });
        let i = alloc_value(next);
        func.block_mut(l_state_bb).push_synthetic(Inst::Load {
            dst: i,
            ptr: order_j_ptr,
            ty: LirType::I64,
        });
        i
    } else {
        l_counter_val
    };

    let states_field = alloc_value(next);
    func.block_mut(l_state_bb).push_synthetic(Inst::FieldPtr {
        dst: states_field,
        base: coll,
        struct_id: gmap_sid,
        field: 3,
    });
    let states_ptr = alloc_value(next);
    func.block_mut(l_state_bb).push_synthetic(Inst::Load {
        dst: states_ptr,
        ptr: states_field,
        ty: LirType::Ptr,
    });
    let state_i_ptr = alloc_value(next);
    func.block_mut(l_state_bb).push_synthetic(Inst::ElemPtr {
        dst: state_i_ptr,
        base: states_ptr,
        index: l_i_val,
        elem_size: 1,
    });
    let state_val = alloc_value(next);
    func.block_mut(l_state_bb).push_synthetic(Inst::Load {
        dst: state_val,
        ptr: state_i_ptr,
        ty: LirType::U8,
    });
    let one_u8 = alloc_value(next);
    func.block_mut(l_state_bb).push_synthetic(Inst::IConst {
        dst: one_u8,
        ty: LirType::U8,
        value: 1,
    });
    let occupied = alloc_value(next);
    func.block_mut(l_state_bb).push_synthetic(Inst::Cmp {
        dst: occupied,
        op: CmpOp::Eq,
        lhs: state_val,
        rhs: one_u8,
    });
    let l_skip_args: Vec<ValueId> = l_extra_check_params.iter().copied().collect();
    func.block_mut(l_state_bb).terminator = Term::Branch {
        cond: occupied,
        then_block: l_body_bb,
        then_args: vec![],
        else_block: l_advance_bb,
        else_args: l_skip_args,
    };

    // legacy body_bb: keys[i] load.
    let (l_elem_ptr, l_elem_arg, l_elem_abi) = emit_set_body_prelude(
        func,
        next,
        l_body_bb,
        coll,
        gmap_sid,
        l_i_val,
        elem_ty,
        elem_size,
        elem_abi_hint,
        /* dense */ false,
    );

    // legacy advance_bb.
    let l_next_counter = emit_advance_backedge(
        func,
        next,
        l_advance_bb,
        l_check_bb,
        l_counter_val,
        &l_advance_extra_params,
    );

    // ─────────────── DENSE branch ───────────────
    // check_bb: entries_len (field 21) as bound.
    let elen_ptr = alloc_value(next);
    func.block_mut(d_check_bb).push_synthetic(Inst::FieldPtr {
        dst: elen_ptr,
        base: coll,
        struct_id: gmap_sid,
        field: 21, // entries_len
    });
    let elen = alloc_value(next);
    func.block_mut(d_check_bb).push_synthetic(Inst::Load {
        dst: elen,
        ptr: elen_ptr,
        ty: LirType::I64,
    });
    let d_cap_cond = alloc_value(next);
    func.block_mut(d_check_bb).push_synthetic(Inst::Cmp {
        dst: d_cap_cond,
        op: CmpOp::Lt,
        lhs: d_counter_val,
        rhs: elen,
    });

    // dense body_bb: entries_keys[i]. Packed — no state check, no order
    // indirection (insertion order is naturally preserved).
    let (d_elem_ptr, d_elem_arg, d_elem_abi) = emit_set_body_prelude(
        func,
        next,
        d_body_bb,
        coll,
        gmap_sid,
        d_counter_val,
        elem_ty,
        elem_size,
        elem_abi_hint,
        /* dense */ true,
    );

    // dense advance_bb.
    let d_next_counter = emit_advance_backedge(
        func,
        next,
        d_advance_bb,
        d_check_bb,
        d_counter_val,
        &d_advance_extra_params,
    );

    SetHofLoopCtx {
        legacy: SetHofLoopBranch {
            check_bb: l_check_bb,
            body_bb: l_body_bb,
            advance_bb: l_advance_bb,
            counter_val: l_counter_val,
            next_counter: l_next_counter,
            cap_cond: l_cap_cond,
            elem_ptr: l_elem_ptr,
            elem_arg: l_elem_arg,
            elem_abi: l_elem_abi,
            extra_check_params: l_extra_check_params,
        },
        legacy_state_bb: l_state_bb,
        dense: SetHofLoopBranch {
            check_bb: d_check_bb,
            body_bb: d_body_bb,
            advance_bb: d_advance_bb,
            counter_val: d_counter_val,
            next_counter: d_next_counter,
            cap_cond: d_cap_cond,
            elem_ptr: d_elem_ptr,
            elem_arg: d_elem_arg,
            elem_abi: d_elem_abi,
            extra_check_params: d_extra_check_params,
        },
        done_bb,
    }
}

/// Shared body prelude for Set HOF loops. `dense=false` reads sparse
/// `keys[i]` (field 0); `dense=true` reads packed `entries_keys[i]`
/// (field 19). `elem_ptr` = ElemPtr(base, i, elem_size), with an
/// optional Load producing `elem_arg` per the closure ABI hint.
#[allow(clippy::too_many_arguments)]
fn emit_set_body_prelude(
    func: &mut LirFunction,
    next: &mut u32,
    body_bb: BlockId,
    coll: ValueId,
    gmap_sid: StructId,
    i_val: ValueId,
    elem_ty: &LirType,
    elem_size: u32,
    elem_abi_hint: Option<crate::ir::abi::AbiKind>,
    dense: bool,
) -> (ValueId, ValueId, crate::ir::abi::AbiKind) {
    let keys_field_idx: u32 = if dense { 19 } else { 0 };
    let keys_field = alloc_value(next);
    func.block_mut(body_bb).push_synthetic(Inst::FieldPtr {
        dst: keys_field,
        base: coll,
        struct_id: gmap_sid,
        field: keys_field_idx,
    });
    let keys_ptr = alloc_value(next);
    func.block_mut(body_bb).push_synthetic(Inst::Load {
        dst: keys_ptr,
        ptr: keys_field,
        ty: LirType::Ptr,
    });
    let elem_ptr = alloc_value(next);
    func.block_mut(body_bb).push_synthetic(Inst::ElemPtr {
        dst: elem_ptr,
        base: keys_ptr,
        index: i_val,
        elem_size,
    });

    let pass_by_ptr = match elem_abi_hint {
        Some(crate::ir::abi::AbiKind::Ptr) => true,
        Some(crate::ir::abi::AbiKind::ByValue) => false,
        Some(crate::ir::abi::AbiKind::Scalar) => false,
        _ => elem_ty.is_aggregate(),
    };
    let elem_arg = if pass_by_ptr {
        elem_ptr
    } else {
        let e = alloc_value(next);
        func.block_mut(body_bb).push_synthetic(Inst::Load {
            dst: e,
            ptr: elem_ptr,
            ty: elem_ty.clone(),
        });
        e
    };
    let elem_abi = if pass_by_ptr {
        crate::ir::abi::AbiKind::Ptr
    } else {
        crate::ir::abi::AbiKind::Scalar
    };
    (elem_ptr, elem_arg, elem_abi)
}

/// Expand `HofExpand { hof_op: SetEach }` — call closure `(elem)` for
/// every occupied slot.
#[allow(clippy::too_many_arguments)]
fn expand_set_each(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    elem_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    is_ordered: bool,
    remaining: Vec<Inst>,
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
) {
    let elem_abi_hint = closure_arg_abis.first().copied();
    let ctx = emit_set_hof_loop_scaffold(
        func,
        current_bb,
        next,
        structs,
        coll,
        &elem_ty,
        elem_abi_hint,
        vec![],
        is_ordered,
    );

    // ── LEGACY ──
    func.block_mut(ctx.legacy.check_bb).terminator = Term::Branch {
        cond: ctx.legacy.cap_cond,
        then_block: ctx.legacy_state_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![],
    };
    func.block_mut(ctx.legacy.body_bb).push_synthetic(Inst::CallClosure {
        dst: None,
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.legacy.elem_arg],
        arg_abis: vec![ctx.legacy.elem_abi],
        ret_ty: LirType::Void,
    });
    func.block_mut(ctx.legacy.body_bb).terminator = Term::Jump(ctx.legacy.advance_bb, vec![]);
    // ── DENSE ──
    func.block_mut(ctx.dense.check_bb).terminator = Term::Branch {
        cond: ctx.dense.cap_cond,
        then_block: ctx.dense.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![],
    };
    func.block_mut(ctx.dense.body_bb).push_synthetic(Inst::CallClosure {
        dst: None,
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.dense.elem_arg],
        arg_abis: vec![ctx.dense.elem_abi],
        ret_ty: LirType::Void,
    });
    func.block_mut(ctx.dense.body_bb).terminator = Term::Jump(ctx.dense.advance_bb, vec![]);

    let done = ctx.done_bb;
    let pre_len = func.block(done).insts.len();
    let pre_spans_len = func.block(done).span_map.len();
    func.block_mut(done).insts = remaining;
    // Synthetic scaffold insts already pushed into done_bb (e.g. a
    // SlotLoad of the result) keep their existing `None` spans; tail
    // insts inherit the spans we captured from the original block.
    let mut combined_spans: Vec<Option<crate::span::Span>> =
        if pre_spans_len == pre_len { func.block(done).span_map.clone() } else { vec![None; pre_len] };
    combined_spans.extend(remaining_spans);
    func.block_mut(done).span_map = combined_spans;
    func.block_mut(done).terminator = orig_term;
    func.block_mut(done).terminator_span = orig_term_span;
}

/// Expand `HofExpand { hof_op: SetFold, init, dst }` — thread a
/// scalar accumulator through the set walk. Closure signature:
/// `(acc, elem) -> acc`.
#[allow(clippy::too_many_arguments)]
fn expand_set_fold(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    elem_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    closure_ret_ty: LirType,
    init: ValueId,
    dst: ValueId,
    is_ordered: bool,
    remaining: Vec<Inst>,
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
) {
    // closure signature: (acc, elem). arg_abis = [acc_abi, elem_abi].
    let acc_abi_hint = closure_arg_abis.first().copied();
    let elem_abi_hint = closure_arg_abis.get(1).copied();
    let ctx = emit_set_hof_loop_scaffold(
        func,
        current_bb,
        next,
        structs,
        coll,
        &elem_ty,
        elem_abi_hint,
        vec![(closure_ret_ty.clone(), init)],
        is_ordered,
    );

    let acc_by_ptr = match acc_abi_hint {
        Some(crate::ir::abi::AbiKind::Ptr) => true,
        Some(crate::ir::abi::AbiKind::ByValue) => false,
        Some(crate::ir::abi::AbiKind::Scalar) => false,
        _ => closure_ret_ty.is_aggregate(),
    };
    let acc_abi = if acc_by_ptr {
        crate::ir::abi::AbiKind::Ptr
    } else {
        crate::ir::abi::AbiKind::Scalar
    };

    // done_bb receives the final acc from whichever branch ran.
    func.block_mut(ctx.done_bb).params.push((dst, closure_ret_ty.clone()));

    // ── LEGACY ──
    let l_acc_val = ctx.legacy.extra_check_params[0];
    let l_acc_arg = if acc_by_ptr {
        let p = alloc_value(next);
        func.block_mut(ctx.legacy.body_bb).push_synthetic(Inst::AddressOf {
            dst: p,
            value: l_acc_val,
            ty: closure_ret_ty.clone(),
        });
        p
    } else {
        l_acc_val
    };
    func.block_mut(ctx.legacy.check_bb).terminator = Term::Branch {
        cond: ctx.legacy.cap_cond,
        then_block: ctx.legacy_state_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![l_acc_val],
    };
    let l_new_acc = alloc_value(next);
    func.block_mut(ctx.legacy.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(l_new_acc),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![l_acc_arg, ctx.legacy.elem_arg],
        arg_abis: vec![acc_abi, ctx.legacy.elem_abi],
        ret_ty: closure_ret_ty.clone(),
    });
    func.block_mut(ctx.legacy.body_bb).terminator =
        Term::Jump(ctx.legacy.advance_bb, vec![l_new_acc]);

    // ── DENSE ──
    let d_acc_val = ctx.dense.extra_check_params[0];
    let d_acc_arg = if acc_by_ptr {
        let p = alloc_value(next);
        func.block_mut(ctx.dense.body_bb).push_synthetic(Inst::AddressOf {
            dst: p,
            value: d_acc_val,
            ty: closure_ret_ty.clone(),
        });
        p
    } else {
        d_acc_val
    };
    func.block_mut(ctx.dense.check_bb).terminator = Term::Branch {
        cond: ctx.dense.cap_cond,
        then_block: ctx.dense.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![d_acc_val],
    };
    let d_new_acc = alloc_value(next);
    func.block_mut(ctx.dense.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(d_new_acc),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![d_acc_arg, ctx.dense.elem_arg],
        arg_abis: vec![acc_abi, ctx.dense.elem_abi],
        ret_ty: closure_ret_ty.clone(),
    });
    func.block_mut(ctx.dense.body_bb).terminator =
        Term::Jump(ctx.dense.advance_bb, vec![d_new_acc]);
    let done = ctx.done_bb;
    let pre_len = func.block(done).insts.len();
    let pre_spans_len = func.block(done).span_map.len();
    func.block_mut(done).insts = remaining;
    // Synthetic scaffold insts already pushed into done_bb (e.g. a
    // SlotLoad of the result) keep their existing `None` spans; tail
    // insts inherit the spans we captured from the original block.
    let mut combined_spans: Vec<Option<crate::span::Span>> =
        if pre_spans_len == pre_len { func.block(done).span_map.clone() } else { vec![None; pre_len] };
    combined_spans.extend(remaining_spans);
    func.block_mut(done).span_map = combined_spans;
    func.block_mut(done).terminator = orig_term;
    func.block_mut(done).terminator_span = orig_term_span;
}

/// Expand `HofExpand { hof_op: SetAny | SetAll, dst }` — early-exit
/// predicate over a Set.
#[allow(clippy::too_many_arguments)]
fn expand_set_any_all(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    op: HofOp,
    coll: ValueId,
    elem_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    dst: ValueId,
    is_ordered: bool,
    remaining: Vec<Inst>,
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
) {
    let is_any = matches!(op, HofOp::SetAny);
    let dst_ty = func
        .value_types
        .get(dst.0 as usize)
        .and_then(|t| t.as_ref())
        .cloned()
        .unwrap_or(LirType::I64);

    let elem_abi_hint = closure_arg_abis.first().copied();
    let ctx = emit_set_hof_loop_scaffold(
        func,
        current_bb,
        next,
        structs,
        coll,
        &elem_ty,
        elem_abi_hint,
        vec![],
        is_ordered,
    );

    let (early_value, fall_value) = if is_any { (1, 0) } else { (0, 1) };
    let const_inst = |d, v, dst_ty: &LirType| match dst_ty {
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

    func.block_mut(ctx.done_bb).params.push((dst, dst_ty.clone()));
    {
        let done = ctx.done_bb;
        let pre_len = func.block(done).insts.len();
        let pre_spans_len = func.block(done).span_map.len();
        func.block_mut(done).insts = remaining;
        let mut combined_spans: Vec<Option<crate::span::Span>> =
            if pre_spans_len == pre_len { func.block(done).span_map.clone() } else { vec![None; pre_len] };
        combined_spans.extend(remaining_spans);
        func.block_mut(done).span_map = combined_spans;
        func.block_mut(done).terminator = orig_term;
        func.block_mut(done).terminator_span = orig_term_span;
    }

    // ── LEGACY ──
    let l_early = alloc_value(next);
    let l_fall = alloc_value(next);
    func.block_mut(ctx.legacy.body_bb).push_synthetic(const_inst(l_early, early_value, &dst_ty));
    func.block_mut(ctx.legacy.check_bb).push_synthetic(const_inst(l_fall, fall_value, &dst_ty));
    func.block_mut(ctx.legacy.check_bb).terminator = Term::Branch {
        cond: ctx.legacy.cap_cond,
        then_block: ctx.legacy_state_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![l_fall],
    };
    let l_pred = alloc_value(next);
    func.block_mut(ctx.legacy.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(l_pred),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.legacy.elem_arg],
        arg_abis: vec![ctx.legacy.elem_abi],
        ret_ty: LirType::Bool,
    });
    let (l_then_block, l_then_args, l_else_block, l_else_args) = if is_any {
        (ctx.done_bb, vec![l_early], ctx.legacy.advance_bb, vec![])
    } else {
        (ctx.legacy.advance_bb, vec![], ctx.done_bb, vec![l_early])
    };
    func.block_mut(ctx.legacy.body_bb).terminator = Term::Branch {
        cond: l_pred,
        then_block: l_then_block,
        then_args: l_then_args,
        else_block: l_else_block,
        else_args: l_else_args,
    };

    // ── DENSE ──
    let d_early = alloc_value(next);
    let d_fall = alloc_value(next);
    func.block_mut(ctx.dense.body_bb).push_synthetic(const_inst(d_early, early_value, &dst_ty));
    func.block_mut(ctx.dense.check_bb).push_synthetic(const_inst(d_fall, fall_value, &dst_ty));
    func.block_mut(ctx.dense.check_bb).terminator = Term::Branch {
        cond: ctx.dense.cap_cond,
        then_block: ctx.dense.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![d_fall],
    };
    let d_pred = alloc_value(next);
    func.block_mut(ctx.dense.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(d_pred),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.dense.elem_arg],
        arg_abis: vec![ctx.dense.elem_abi],
        ret_ty: LirType::Bool,
    });
    let (d_then_block, d_then_args, d_else_block, d_else_args) = if is_any {
        (ctx.done_bb, vec![d_early], ctx.dense.advance_bb, vec![])
    } else {
        (ctx.dense.advance_bb, vec![], ctx.done_bb, vec![d_early])
    };
    func.block_mut(ctx.dense.body_bb).terminator = Term::Branch {
        cond: d_pred,
        then_block: d_then_block,
        then_args: d_then_args,
        else_block: d_else_block,
        else_args: d_else_args,
    };
}

/// Expand `HofExpand { hof_op: SetFilter, dst }` — build a fresh
/// `GorgetSet` of elements that satisfy the predicate.
///
/// current_bb: CallExtern `gorget_set_new_like(src)` → result;
/// SlotStore into result_slot. The helper mirrors hash/eq/drop/
/// clone/materialize from src, so the result works for any element
/// type without per-type ctor dispatch.
///
/// body_bb: `pred = closure(elem)`; Branch(pred, push_bb, advance_bb).
/// push_bb: `gorget_map_put_cloned(result_addr, elem_ptr, NULL)` then
/// Jump(advance_bb). done_bb SlotLoads the result into dst.
#[allow(clippy::too_many_arguments)]
fn expand_set_filter(
    func: &mut LirFunction,
    current_bb: usize,
    next: &mut u32,
    structs: &[StructDef],
    coll: ValueId,
    elem_ty: LirType,
    closure: ValueId,
    closure_arg_abis: Vec<crate::ir::abi::AbiKind>,
    dst: ValueId,
    is_ordered: bool,
    remaining: Vec<Inst>,
    remaining_spans: Vec<Option<crate::span::Span>>,
    orig_term: Term,
    orig_term_span: Option<crate::span::Span>,
) {
    let gset_sid = lookup_struct_id(structs, "GorgetSet")
        .or_else(|| lookup_struct_id(structs, "GorgetMap"))
        .unwrap_or(StructId(0));
    let gset_ty = LirType::Struct(gset_sid);
    let cur = BlockId(current_bb as u32);

    // current_bb: allocate result slot + init via gorget_set_new_like(coll).
    let result_slot = func.add_slot(gset_ty.clone(), None);
    let set_val = alloc_value(next);
    func.block_mut(cur).push_synthetic(Inst::CallExtern {
        dst: Some(set_val),
        name: "gorget_set_new_like".to_string(),
        args: vec![coll],
        arg_abis: vec![crate::ir::abi::AbiKind::Ptr],
    });
    func.block_mut(cur).push_synthetic(Inst::SlotStore {
        slot: result_slot,
        value: set_val,
        is_move: true,
    });

    let elem_abi_hint = closure_arg_abis.first().copied();
    let ctx = emit_set_hof_loop_scaffold(
        func,
        current_bb,
        next,
        structs,
        coll,
        &elem_ty,
        elem_abi_hint,
        vec![],
        is_ordered,
    );

    // ── LEGACY ──
    let l_pred = alloc_value(next);
    func.block_mut(ctx.legacy.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(l_pred),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.legacy.elem_arg],
        arg_abis: vec![ctx.legacy.elem_abi],
        ret_ty: LirType::Bool,
    });
    func.block_mut(ctx.legacy.check_bb).terminator = Term::Branch {
        cond: ctx.legacy.cap_cond,
        then_block: ctx.legacy_state_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![],
    };
    let l_push_bb = func.add_block();
    func.block_mut(ctx.legacy.body_bb).terminator = Term::Branch {
        cond: l_pred,
        then_block: l_push_bb,
        then_args: vec![],
        else_block: ctx.legacy.advance_bb,
        else_args: vec![],
    };
    let l_result_addr = alloc_value(next);
    func.block_mut(l_push_bb).push_synthetic(Inst::SlotAddr {
        dst: l_result_addr,
        slot: result_slot,
    });
    let l_null_val = alloc_value(next);
    func.block_mut(l_push_bb).push_synthetic(Inst::NullPtr { dst: l_null_val });
    func.block_mut(l_push_bb).push_synthetic(Inst::CallExtern {
        dst: None,
        name: "gorget_map_put_cloned".to_string(),
        args: vec![l_result_addr, ctx.legacy.elem_ptr, l_null_val],
        arg_abis: vec![
            crate::ir::abi::AbiKind::Ptr,
            crate::ir::abi::AbiKind::Ptr,
            crate::ir::abi::AbiKind::Ptr,
        ],
    });
    func.block_mut(l_push_bb).terminator = Term::Jump(ctx.legacy.advance_bb, vec![]);

    // ── DENSE ──
    let d_pred = alloc_value(next);
    func.block_mut(ctx.dense.body_bb).push_synthetic(Inst::CallClosure {
        dst: Some(d_pred),
        kind: crate::lir::ClosureDispatchKind::EscapedClosure,
        closure,
        args: vec![ctx.dense.elem_arg],
        arg_abis: vec![ctx.dense.elem_abi],
        ret_ty: LirType::Bool,
    });
    func.block_mut(ctx.dense.check_bb).terminator = Term::Branch {
        cond: ctx.dense.cap_cond,
        then_block: ctx.dense.body_bb,
        then_args: vec![],
        else_block: ctx.done_bb,
        else_args: vec![],
    };
    let d_push_bb = func.add_block();
    func.block_mut(ctx.dense.body_bb).terminator = Term::Branch {
        cond: d_pred,
        then_block: d_push_bb,
        then_args: vec![],
        else_block: ctx.dense.advance_bb,
        else_args: vec![],
    };
    let d_result_addr = alloc_value(next);
    func.block_mut(d_push_bb).push_synthetic(Inst::SlotAddr {
        dst: d_result_addr,
        slot: result_slot,
    });
    let d_null_val = alloc_value(next);
    func.block_mut(d_push_bb).push_synthetic(Inst::NullPtr { dst: d_null_val });
    func.block_mut(d_push_bb).push_synthetic(Inst::CallExtern {
        dst: None,
        name: "gorget_map_put_cloned".to_string(),
        args: vec![d_result_addr, ctx.dense.elem_ptr, d_null_val],
        arg_abis: vec![
            crate::ir::abi::AbiKind::Ptr,
            crate::ir::abi::AbiKind::Ptr,
            crate::ir::abi::AbiKind::Ptr,
        ],
    });
    func.block_mut(d_push_bb).terminator = Term::Jump(ctx.dense.advance_bb, vec![]);

    // done_bb: SlotLoad the result set into dst + remaining + orig_term.
    func.block_mut(ctx.done_bb).push_synthetic(Inst::SlotLoad {
        dst,
        slot: result_slot,
        ty: gset_ty,
    });
    func.block_mut(ctx.done_bb).insts.extend(remaining);
    // Mirror the extend on `span_map` so the parallel-array invariant
    // holds — `done_bb` already has spans for synthetic scaffold insts
    // pushed by this expander; appending `remaining_spans` aligns the
    // tail with the corresponding `remaining` insts.
    {
        let done = ctx.done_bb;
        let new_len = func.block(done).insts.len();
        if func.block(done).span_map.len() < new_len {
            // Pre-extend span_map to a clean parallel state if it had
            // drifted, then extend with remaining_spans.
            let cur_len = func.block(done).span_map.len();
            let scaffold_len = new_len - remaining_spans.len();
            if cur_len < scaffold_len {
                func.block_mut(done)
                    .span_map
                    .resize(scaffold_len, None);
            }
            func.block_mut(done).span_map.extend(remaining_spans);
        } else {
            func.block_mut(done).span_map.extend(remaining_spans);
        }
    }
    func.block_mut(ctx.done_bb).terminator = orig_term;
    func.block_mut(ctx.done_bb).terminator_span = orig_term_span;
}

fn lookup_struct_id(structs: &[StructDef], name: &str) -> Option<StructId> {
    structs
        .iter()
        .position(|s| s.name == name)
        .map(|i| StructId(i as u32))
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
                    | Inst::CowClone { .. }
                    | Inst::TraitCall { .. }
                    | Inst::HofExpand { .. }
                    | Inst::AddressOf { .. }
                    | Inst::BoxAlloc { .. }
                    | Inst::CallRuntime { .. }
                    | Inst::CollectionCtor { .. }
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
        func.block_mut(bb).push_synthetic(Inst::IConst {
            dst: value, ty: LirType::I64, value: 42,
        });
        func.block_mut(bb).push_synthetic(Inst::AddressOf {
            dst,
            value,
            ty: LirType::I64,
        });
        let slots_before = func.slots.len();
        let mut pool = crate::bir::synth::SynthPool::new(0);
        expand_func(&mut func, &[], &mut pool);
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
        func.block_mut(bb).push_synthetic(Inst::IConst {
            dst: value, ty: LirType::I64, value: 42,
        });
        func.block_mut(bb).push_synthetic(Inst::BoxAlloc {
            dst,
            inner_ty: LirType::I64,
            value,
        });
        let mut pool = crate::bir::synth::SynthPool::new(0);
        expand_func(&mut func, &[], &mut pool);
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
        func.block_mut(BlockId(0)).push_synthetic(Inst::AddressOf {
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
        func.block_mut(BlockId(0)).push_synthetic(Inst::BoxAlloc {
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
