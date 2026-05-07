//! Per-function backward-dataflow liveness analysis on the GIR.
//!
//! For each `(block, instruction_index)` position we compute the set of
//! locals that are *live after* that point — i.e., read by some
//! instruction reachable from this position via the function's CFG.
//!
//! This is the foundation pass for Tier 2a (CoW consume-site discipline).
//! At every consuming position (`push` / `put` / `insert` / `send` /
//! `IndexStore` / `EnumInit` / `StructInit` / `BoxNew` / function arg with
//! `ParamABI::ByValue`) the IR mode of the source operand must match its
//! `LocalOwnership` state. Detecting "owned but live past this call" — the
//! shape that today emits `Operand::Copy(p)` plus a stale
//! `drops.unregister(p)` — requires real liveness, not the
//! `drops.is_registered` proxy.
//!
//! Algorithm: standard backward dataflow.
//!
//! ```text
//! OUT[B] = ∪ IN[S]   for each successor S of B
//! IN[B]  = (OUT[B] - DEFS[B]) ∪ USES[B]
//! ```
//!
//! Iterate to fixpoint. Block sizes are small (~10–100 instructions) and
//! function CFGs are small, so an O(N²) worklist is plenty.
//!
//! See `docs/internals/structural-guards.md` Tier 2a for the broader
//! consume-site discipline this enables, and the `validate` module's
//! `ConsumeSite` walker for the consumer.

use rustc_hash::{FxHashMap, FxHashSet};

use super::instructions::{Instruction, Operand, Place, Projection, Terminator};
use super::types::{BlockId, LocalId};
use super::Function;

/// Backward-dataflow liveness for a single function.
///
/// `is_live_after(local, block, inst_index)` answers: "is `local` read by
/// some instruction reachable after position `(block, inst_index)`?".
pub struct Liveness {
    /// `live_after[b][i]` = set of locals live after instruction `i` in block `b`.
    /// Length = number of blocks; inner length = number of instructions in that block.
    /// At index `inst_count` of each inner Vec we store the live set after the
    /// terminator (== OUT[B] — useful for callers asking "live after the last
    /// instruction" without special-casing terminator-only blocks).
    live_after: Vec<Vec<FxHashSet<u32>>>,
}

impl Liveness {
    /// Compute liveness for `func`. Empty / malformed functions return an
    /// empty result — the validator simply sees no live-out information,
    /// which is the safe default for the consume-site rule (it errs toward
    /// "source is dead" only when explicitly proven so).
    pub fn compute(func: &Function) -> Self {
        let n_blocks = func.blocks.len();
        if n_blocks == 0 {
            return Self { live_after: Vec::new() };
        }

        // Per-block USES (read before def in the block) and DEFS (locals
        // defined anywhere in the block, including the terminator). Standard
        // gen/kill formulation; computed once per block, reused each fixpoint
        // iteration.
        let mut uses: Vec<FxHashSet<u32>> = vec![FxHashSet::default(); n_blocks];
        let mut defs: Vec<FxHashSet<u32>> = vec![FxHashSet::default(); n_blocks];
        for (bi, bb) in func.blocks.iter().enumerate() {
            let (u, d) = compute_block_use_def(bb);
            uses[bi] = u;
            defs[bi] = d;
        }

        // Precompute successors per block (cheap, simplifies the IN/OUT loop).
        let succs: Vec<Vec<usize>> = func.blocks.iter()
            .map(|bb| match &bb.terminator {
                Some(t) => terminator_successors(t),
                None => Vec::new(),
            })
            .collect();

        // IN[B] sets, indexed by block. OUT[B] = union of IN[succ].
        // Iterate to fixpoint.
        let mut in_sets: Vec<FxHashSet<u32>> = vec![FxHashSet::default(); n_blocks];
        loop {
            let mut changed = false;
            // Reverse order tends to converge faster for forward CFGs but
            // the worklist semantics are the same; this is plenty fast.
            for bi in (0..n_blocks).rev() {
                let mut out_b: FxHashSet<u32> = FxHashSet::default();
                for &s in &succs[bi] {
                    out_b.extend(in_sets[s].iter().copied());
                }
                // IN[B] = (OUT[B] - DEFS[B]) ∪ USES[B]
                let mut new_in = out_b.clone();
                for d in &defs[bi] {
                    new_in.remove(d);
                }
                new_in.extend(uses[bi].iter().copied());
                if new_in != in_sets[bi] {
                    in_sets[bi] = new_in;
                    changed = true;
                }
            }
            if !changed { break; }
        }

        // Now walk each block backward to derive per-instruction live-after.
        // live_after[b][k] = set of locals live AFTER position k.
        // - live_after[b][n_inst] (the terminator slot) = OUT[B].
        // - For k = n_inst-1 down to 0:
        //     live_after[b][k] = (live_after[b][k+1] - defs(inst k)) ∪ uses(inst k_next?)
        //   No — clearer: define live_before(inst) = (live_after(inst) - defs(inst)) ∪ uses(inst).
        //   Then live_after of the previous instruction = live_before(this inst).
        let mut live_after: Vec<Vec<FxHashSet<u32>>> = Vec::with_capacity(n_blocks);
        for bi in 0..n_blocks {
            let bb = &func.blocks[bi];
            let n_inst = bb.instructions.len();
            // OUT[B] for the terminator slot.
            let mut out_b: FxHashSet<u32> = FxHashSet::default();
            for &s in &succs[bi] {
                out_b.extend(in_sets[s].iter().copied());
            }
            // The terminator itself reads operands (e.g. `Return(value)`,
            // `Branch.cond`). Those reads are live BEFORE the terminator —
            // i.e., live AFTER the last instruction. So we compute
            // live_after(inst[n_inst-1]) = (out_b - term_defs) ∪ term_uses.
            let term_uses = match &bb.terminator {
                Some(t) => collect_terminator_reads(t),
                None => FxHashSet::default(),
            };
            // Terminators don't define locals (Invoke's dst is on the
            // normal-edge entry point, which is treated as a use for
            // simplicity — see [collect_terminator_reads]; we don't
            // remove anything here).
            let mut after_last = out_b.clone();
            after_last.extend(term_uses);

            // Build per-instruction live_after backward. Vec length = n_inst+1.
            let mut per_inst: Vec<FxHashSet<u32>> = vec![FxHashSet::default(); n_inst + 1];
            // The "after the terminator" slot = OUT[B] (no additions —
            // term uses are already covered by `after_last` for the last
            // instruction, and OUT[B] is what survives leaving the block).
            per_inst[n_inst] = out_b;
            // Walk instructions in reverse.
            let mut current = after_last;
            for k in (0..n_inst).rev() {
                // live_after(inst k) = `current` (which represents the live-set
                // entering instruction k+1, i.e. live AFTER k).
                per_inst[k] = current.clone();
                let inst = &bb.instructions[k];
                // live_before(inst k) = (live_after(k) - defs(k)) ∪ uses(k).
                for d in collect_inst_defs(inst) {
                    current.remove(&d);
                }
                current.extend(collect_inst_reads(inst));
            }
            live_after.push(per_inst);
        }

        Self { live_after }
    }

    /// Is `local` read by some instruction reachable after position
    /// `(block, inst_index)`? `inst_index` is interpreted as the
    /// instruction at that index — we ask whether the local survives past
    /// it (i.e., is live AFTER it).
    ///
    /// `inst_index == bb.instructions.len()` is a valid query meaning
    /// "live after the terminator" — equal to OUT[B].
    pub fn is_live_after(&self, local: LocalId, block: BlockId, inst_index: usize) -> bool {
        let bi = block.0 as usize;
        if bi >= self.live_after.len() { return false; }
        let per_inst = &self.live_after[bi];
        if inst_index >= per_inst.len() { return false; }
        per_inst[inst_index].contains(&local.0)
    }
}

/// Locals defined (written) by an instruction. Definitions kill liveness:
/// after `_x = ...` the previous live-set for `_x` is rewritten by this
/// new definition; uses inside the same instruction's RHS are read first
/// (we add them in `collect_inst_reads`).
fn collect_inst_defs(inst: &Instruction) -> Vec<u32> {
    let mut defs = Vec::new();
    match inst {
        Instruction::Assign { dst, .. } => {
            // Only bare-local assigns kill the local. Projection writes
            // (field stores, deref stores) are partial — they don't kill
            // the whole local.
            if dst.projections.is_empty() {
                defs.push(dst.local.0);
            }
        }
        Instruction::BinOp { dst, .. }
        | Instruction::UnOp { dst, .. }
        | Instruction::Cmp { dst, .. }
        | Instruction::Cast { dst, .. }
        | Instruction::BitCast { dst, .. }
        | Instruction::PtrCast { dst, .. }
        | Instruction::FieldLoad { dst, .. }
        | Instruction::IndexLoad { dst, .. }
        | Instruction::EnumFieldLoad { dst, .. }
        | Instruction::HeapAlloc { dst, .. }
        | Instruction::HeapAllocArray { dst, .. }
        | Instruction::StructInit { dst, .. }
        | Instruction::EnumInit { dst, .. }
        | Instruction::TupleInit { dst, .. }
        | Instruction::TagOf { dst, .. }
        | Instruction::Borrow { dst, .. }
        | Instruction::BorrowMut { dst, .. }
        | Instruction::LoadThreadLocal { dst, .. }
        | Instruction::LoadRef { dst, .. } => {
            defs.push(dst.0);
        }
        Instruction::Call { dst: Some(d), .. }
        | Instruction::CallIndirect { dst: Some(d), .. }
        | Instruction::CallExtern { dst: Some(d), .. } => {
            defs.push(d.0);
        }
        Instruction::MoveZero { place } if place.projections.is_empty() => {
            // MoveZero conceptually transfers ownership and zeroes the
            // source slot. For liveness purposes this is a "kill": the
            // local's value stops being live (the next read would be
            // UseAfterMove). Treating MoveZero as a def here is what
            // makes the consume-site rule's "live_after = false after
            // a Move" hold automatically.
            defs.push(place.local.0);
        }
        // No definition: side-effects only, projections, drops.
        _ => {}
    }
    defs
}

/// Locals read by an instruction. Mirrors [`super::validate::collect_read_locals_for_validate`]
/// but returns a typed iterator suitable for FxHashSet construction.
///
/// Specifically:
/// - Operand reads contribute the operand's place local plus any Index
///   projection locals.
/// - Place reads (FieldLoad/IndexLoad base, EnumFieldLoad base, Borrow/BorrowMut)
///   contribute the base local plus Index projection locals.
/// - Drop / DropIfAlive are *not* reads (they're scope-exit side effects
///   on the OWNED local — by the time control reaches them the value is
///   already known to be dead via MoveZero or natural scope end).
/// - MoveZero is *not* counted as a read. It's a def that zeroes the
///   source; treating it as a read would defeat the purpose (every
///   move would mark its source as live-before).
fn collect_inst_reads(inst: &Instruction) -> Vec<u32> {
    let mut reads = Vec::new();
    let push_op = |reads: &mut Vec<u32>, op: &Operand| {
        if let Operand::Copy(p) | Operand::Move(p) = op {
            reads.push(p.local.0);
            for proj in &p.projections {
                if let Projection::Index(id) = proj { reads.push(id.0); }
            }
        }
    };
    let push_place = |reads: &mut Vec<u32>, p: &Place| {
        reads.push(p.local.0);
        for proj in &p.projections {
            if let Projection::Index(id) = proj { reads.push(id.0); }
        }
    };

    match inst {
        Instruction::Assign { dst, value, .. } => {
            // Projection writes read the dst place (computing the address).
            if !dst.projections.is_empty() {
                push_place(&mut reads, dst);
            }
            push_op(&mut reads, value);
        }
        Instruction::BinOp { lhs, rhs, .. } | Instruction::Cmp { lhs, rhs, .. } => {
            push_op(&mut reads, lhs);
            push_op(&mut reads, rhs);
        }
        Instruction::UnOp { operand, .. }
        | Instruction::Cast { value: operand, .. }
        | Instruction::BitCast { value: operand, .. }
        | Instruction::PtrCast { value: operand, .. }
        | Instruction::TagOf { operand, .. } => {
            push_op(&mut reads, operand);
        }
        Instruction::FieldLoad { base, .. } | Instruction::EnumFieldLoad { base, .. } => {
            push_place(&mut reads, base);
        }
        Instruction::IndexLoad { base, index, .. } => {
            push_place(&mut reads, base);
            push_op(&mut reads, index);
        }
        Instruction::Call { args, .. } | Instruction::CallExtern { args, .. } => {
            for a in args { push_op(&mut reads, a); }
        }
        Instruction::CallIndirect { callee, args, .. } => {
            push_op(&mut reads, callee);
            for a in args { push_op(&mut reads, a); }
        }
        Instruction::StructInit { fields, .. } | Instruction::EnumInit { fields, .. } => {
            for f in fields { push_op(&mut reads, f); }
        }
        Instruction::TupleInit { elements, .. } => {
            for e in elements { push_op(&mut reads, e); }
        }
        Instruction::Borrow { place, .. } | Instruction::BorrowMut { place, .. } => {
            push_place(&mut reads, place);
        }
        Instruction::HeapAlloc { allocator, .. } => { push_op(&mut reads, allocator); }
        Instruction::HeapAllocArray { count, allocator, .. } => {
            push_op(&mut reads, count);
            push_op(&mut reads, allocator);
        }
        Instruction::Dealloc { ptr, allocator } => {
            push_op(&mut reads, ptr);
            push_op(&mut reads, allocator);
        }
        Instruction::LoadRef { src, .. } => {
            push_place(&mut reads, src);
        }
        Instruction::StoreRef { dst, value } => {
            // StoreRef writes through dst — read the address, read the value.
            push_place(&mut reads, dst);
            push_op(&mut reads, value);
        }
        Instruction::PushAllocator { allocator } => { push_op(&mut reads, allocator); }
        Instruction::GlobalAssign { value, .. } => { push_op(&mut reads, value); }
        // Drops / MoveZero / no-ops contribute nothing — see the
        // doc-comment for the rationale.
        Instruction::Drop { .. } | Instruction::DropIfAlive { .. }
        | Instruction::MoveZero { .. }
        | Instruction::PopAllocator | Instruction::Nop
        | Instruction::InlineC { .. } | Instruction::LoadThreadLocal { .. } => {}
    }
    reads
}

/// Per-block USE / DEF sets for the dataflow.
///
/// Standard formulation:
/// - USE[B] = locals read in B before any definition of them in B.
/// - DEF[B] = locals defined anywhere in B.
fn compute_block_use_def(bb: &super::BasicBlock) -> (FxHashSet<u32>, FxHashSet<u32>) {
    let mut use_set: FxHashSet<u32> = FxHashSet::default();
    let mut def_set: FxHashSet<u32> = FxHashSet::default();
    for inst in &bb.instructions {
        // Reads are USE only if not yet DEFined in this block.
        for r in collect_inst_reads(inst) {
            if !def_set.contains(&r) {
                use_set.insert(r);
            }
        }
        for d in collect_inst_defs(inst) {
            def_set.insert(d);
        }
    }
    // Terminator reads are USEs (after all instructions). If a terminator
    // operand reads a local and that local was DEFed in the block, the
    // read is satisfied locally — don't add to USE. Otherwise add.
    if let Some(t) = &bb.terminator {
        for r in collect_terminator_reads(t) {
            if !def_set.contains(&r) {
                use_set.insert(r);
            }
        }
    }
    (use_set, def_set)
}

/// Locals read by a terminator. `Invoke` is treated as reading args; its
/// `dst` is a definition that activates only on the `normal` edge — for
/// liveness purposes we keep it simple and treat the value as defined at
/// the start of the `normal` block (in practice every callsite is the
/// last instruction, and the Invoke's dst flows into the block via Phi-like
/// mechanisms today). The conservative answer (don't kill `dst` here) is
/// safe: we may report a value as live one block later than necessary,
/// which never produces a false consume-site violation.
fn collect_terminator_reads(t: &Terminator) -> FxHashSet<u32> {
    let mut s: FxHashSet<u32> = FxHashSet::default();
    let push_op = |s: &mut FxHashSet<u32>, op: &Operand| {
        if let Operand::Copy(p) | Operand::Move(p) = op {
            s.insert(p.local.0);
            for proj in &p.projections {
                if let Projection::Index(id) = proj { s.insert(id.0); }
            }
        }
    };
    match t {
        Terminator::Return(v) => push_op(&mut s, v),
        Terminator::Branch { cond, .. } => push_op(&mut s, cond),
        Terminator::Switch { value, .. } => push_op(&mut s, value),
        Terminator::Invoke { args, .. } => {
            for a in args { push_op(&mut s, a); }
        }
        Terminator::Jump(_) | Terminator::Unreachable => {}
    }
    s
}

/// Successor blocks of a terminator. `Unreachable` has none; `Return` has
/// none; everything else lists its targets.
fn terminator_successors(t: &Terminator) -> Vec<usize> {
    match t {
        Terminator::Jump(b) => vec![b.0 as usize],
        Terminator::Branch { then_block, else_block, .. } => {
            vec![then_block.0 as usize, else_block.0 as usize]
        }
        Terminator::Switch { cases, default, .. } => {
            let mut v: Vec<usize> = cases.iter().map(|(_, b)| b.0 as usize).collect();
            v.push(default.0 as usize);
            v
        }
        Terminator::Invoke { normal, error, .. } => {
            vec![normal.0 as usize, error.0 as usize]
        }
        Terminator::Return(_) | Terminator::Unreachable => Vec::new(),
    }
}

// Suppress unused import warnings — keep the explicit FxHashMap import
// for symmetry with the validate module's usage; future incremental
// liveness queries will reuse the type.
#[allow(dead_code)]
fn _force_use() -> FxHashMap<u32, u32> { FxHashMap::default() }

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, Function, Local, LocalOwnership, SlotKind};
    use crate::ir::instructions::*;
    use crate::ir::types::{I64_TYPE, UNIT_TYPE};

    fn mk_local(ty_id: crate::ir::types::TypeId) -> Local {
        Local {
            type_id: ty_id,
            name_hint: None,
            ownership: LocalOwnership::default(),
            slot_kind: SlotKind::default(),
            is_owning_param: false,
        }
    }

    /// Minimal CFG: single linear block.
    /// _0 = return slot (I64). _1 = const. _2 = _1 + 1. return _2.
    /// _1 should be live after `_1 = 5` (its only use is in the BinOp).
    /// _1 should NOT be live after the BinOp.
    /// _2 should be live after the BinOp (used in Return).
    #[test]
    fn linear_block_basic() {
        let func = Function {
            name: "f".into(),
            params: vec![],
            return_type: I64_TYPE,
            locals: vec![mk_local(I64_TYPE), mk_local(I64_TYPE), mk_local(I64_TYPE)],
            blocks: vec![BasicBlock {
                instructions: vec![
                    // _1 = 5
                    Instruction::Assign {
                        mode: AssignMode::Copy,
                        dst: Place::local(LocalId(1)),
                        value: Operand::Constant(Constant::I64(5)),
                    },
                    // _2 = _1 + 1
                    Instruction::BinOp {
                        dst: LocalId(2),
                        op: BinOp::Add,
                        type_id: I64_TYPE,
                        lhs: Operand::Copy(Place::local(LocalId(1))),
                        rhs: Operand::Constant(Constant::I64(1)),
                    },
                ],
                terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(2))))),
                span_map: vec![None, None],
                terminator_span: None,
            }],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
        };

        let live = Liveness::compute(&func);
        // After inst 0 (`_1 = 5`): _1 must be live (BinOp reads it next).
        assert!(live.is_live_after(LocalId(1), BlockId(0), 0));
        // After inst 1 (BinOp): _1 dead, _2 live (Return reads it).
        assert!(!live.is_live_after(LocalId(1), BlockId(0), 1));
        assert!(live.is_live_after(LocalId(2), BlockId(0), 1));
        // After terminator: nothing live.
        assert!(!live.is_live_after(LocalId(1), BlockId(0), 2));
        assert!(!live.is_live_after(LocalId(2), BlockId(0), 2));
    }

    /// If/else merge: `if c: x = a else: x = b; return x`.
    /// `a` is live in the then-branch only, `b` in the else-branch only;
    /// at the merge point neither is live (already consumed into x).
    #[test]
    fn if_else_merge() {
        let func = Function {
            name: "f".into(),
            params: vec![],
            return_type: I64_TYPE,
            locals: vec![
                mk_local(I64_TYPE),  // _0 ret
                mk_local(I64_TYPE),  // _1 = a
                mk_local(I64_TYPE),  // _2 = b
                mk_local(I64_TYPE),  // _3 = c
                mk_local(I64_TYPE),  // _4 = x (defined in both branches)
            ],
            blocks: vec![
                // bb0: setup, branch on _3
                BasicBlock {
                    instructions: vec![
                        Instruction::Assign { mode: AssignMode::Copy,
                            dst: Place::local(LocalId(1)), value: Operand::Constant(Constant::I64(10)) },
                        Instruction::Assign { mode: AssignMode::Copy,
                            dst: Place::local(LocalId(2)), value: Operand::Constant(Constant::I64(20)) },
                        Instruction::Assign { mode: AssignMode::Copy,
                            dst: Place::local(LocalId(3)), value: Operand::Constant(Constant::I64(1)) },
                    ],
                    terminator: Some(Terminator::Branch {
                        cond: Operand::Copy(Place::local(LocalId(3))),
                        then_block: BlockId(1),
                        else_block: BlockId(2),
                    }),
                    span_map: vec![None, None, None],
                    terminator_span: None,
                },
                // bb1 (then): _4 = _1; jump bb3
                BasicBlock {
                    instructions: vec![
                        Instruction::Assign { mode: AssignMode::Copy,
                            dst: Place::local(LocalId(4)),
                            value: Operand::Copy(Place::local(LocalId(1))) },
                    ],
                    terminator: Some(Terminator::Jump(BlockId(3))),
                    span_map: vec![None],
                    terminator_span: None,
                },
                // bb2 (else): _4 = _2; jump bb3
                BasicBlock {
                    instructions: vec![
                        Instruction::Assign { mode: AssignMode::Copy,
                            dst: Place::local(LocalId(4)),
                            value: Operand::Copy(Place::local(LocalId(2))) },
                    ],
                    terminator: Some(Terminator::Jump(BlockId(3))),
                    span_map: vec![None],
                    terminator_span: None,
                },
                // bb3: return _4
                BasicBlock {
                    instructions: vec![],
                    terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(4))))),
                    span_map: vec![],
                    terminator_span: None,
                },
            ],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
        };

        let live = Liveness::compute(&func);
        // After bb0's setup of _1 (inst 0): _1 must be live (used in bb1 then-branch).
        assert!(live.is_live_after(LocalId(1), BlockId(0), 0));
        // After bb0's setup of _2 (inst 1): _2 must be live (used in bb2 else-branch).
        assert!(live.is_live_after(LocalId(2), BlockId(0), 1));
        // _3 (cond) must be live after inst 2 (consumed by Branch terminator).
        assert!(live.is_live_after(LocalId(3), BlockId(0), 2));
        // After the Branch terminator, _3 is dead (consumed); _1 stays live
        // along the then edge, _2 along the else edge — so OUT[bb0] = {_1, _2}
        // (union of in-sets).
        assert!(live.is_live_after(LocalId(1), BlockId(0), 3));
        assert!(live.is_live_after(LocalId(2), BlockId(0), 3));
        assert!(!live.is_live_after(LocalId(3), BlockId(0), 3));
        // In bb1 after the assignment to _4 from _1: _1 is dead, _4 is live.
        assert!(!live.is_live_after(LocalId(1), BlockId(1), 0));
        assert!(live.is_live_after(LocalId(4), BlockId(1), 0));
        // Same for bb2.
        assert!(!live.is_live_after(LocalId(2), BlockId(2), 0));
        assert!(live.is_live_after(LocalId(4), BlockId(2), 0));
    }

    /// Loop with back-edge: `while c: x = x + 1`. `x` must remain live on
    /// the back-edge — the standard fixpoint test.
    #[test]
    fn loop_back_edge() {
        let func = Function {
            name: "f".into(),
            params: vec![],
            return_type: I64_TYPE,
            locals: vec![
                mk_local(I64_TYPE),  // _0 ret
                mk_local(I64_TYPE),  // _1 = x
                mk_local(I64_TYPE),  // _2 = c
            ],
            blocks: vec![
                // bb0: _1 = 0; jump bb1
                BasicBlock {
                    instructions: vec![
                        Instruction::Assign { mode: AssignMode::Copy,
                            dst: Place::local(LocalId(1)), value: Operand::Constant(Constant::I64(0)) },
                    ],
                    terminator: Some(Terminator::Jump(BlockId(1))),
                    span_map: vec![None],
                    terminator_span: None,
                },
                // bb1 (header): _2 = _1 < 10; branch _2 → bb2 (body) | bb3 (exit)
                BasicBlock {
                    instructions: vec![
                        Instruction::Cmp {
                            dst: LocalId(2), op: CmpOp::Lt, type_id: I64_TYPE,
                            lhs: Operand::Copy(Place::local(LocalId(1))),
                            rhs: Operand::Constant(Constant::I64(10)),
                        },
                    ],
                    terminator: Some(Terminator::Branch {
                        cond: Operand::Copy(Place::local(LocalId(2))),
                        then_block: BlockId(2),
                        else_block: BlockId(3),
                    }),
                    span_map: vec![None],
                    terminator_span: None,
                },
                // bb2 (body): _1 = _1 + 1; jump bb1
                BasicBlock {
                    instructions: vec![
                        Instruction::BinOp {
                            dst: LocalId(1),
                            op: BinOp::Add, type_id: I64_TYPE,
                            lhs: Operand::Copy(Place::local(LocalId(1))),
                            rhs: Operand::Constant(Constant::I64(1)),
                        },
                    ],
                    terminator: Some(Terminator::Jump(BlockId(1))),
                    span_map: vec![None],
                    terminator_span: None,
                },
                // bb3 (exit): return _1
                BasicBlock {
                    instructions: vec![],
                    terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(1))))),
                    span_map: vec![],
                    terminator_span: None,
                },
            ],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
        };

        let live = Liveness::compute(&func);
        // After bb0's _1 = 0: _1 must be live (the loop body and the final
        // return both use it).
        assert!(live.is_live_after(LocalId(1), BlockId(0), 0));
        // In bb1's Cmp: _1 is read; OUT of Cmp, _1 must still be live
        // (the body uses it again, and the exit return uses it).
        assert!(live.is_live_after(LocalId(1), BlockId(1), 0));
        // _2 (the cmp result) is live until consumed by the Branch.
        assert!(live.is_live_after(LocalId(2), BlockId(1), 0));
        // After the Branch terminator (live-after slot for "after term"):
        // _1 is live (next iteration body OR exit's return); _2 dead.
        assert!(live.is_live_after(LocalId(1), BlockId(1), 1));
        assert!(!live.is_live_after(LocalId(2), BlockId(1), 1));
        // In bb2 body (the increment): after the BinOp, _1 must be live
        // (the back-edge feeds bb1 which reads it).
        assert!(live.is_live_after(LocalId(1), BlockId(2), 0));
    }

    /// Unreachable block: an isolated block with no predecessor and no
    /// useful successors should not contaminate other blocks' liveness.
    #[test]
    fn unreachable_block_isolated() {
        let func = Function {
            name: "f".into(),
            params: vec![],
            return_type: I64_TYPE,
            locals: vec![mk_local(I64_TYPE), mk_local(I64_TYPE)],
            blocks: vec![
                // bb0: return _1
                BasicBlock {
                    instructions: vec![
                        Instruction::Assign { mode: AssignMode::Copy,
                            dst: Place::local(LocalId(1)),
                            value: Operand::Constant(Constant::I64(42)) },
                    ],
                    terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(1))))),
                    span_map: vec![None],
                    terminator_span: None,
                },
                // bb1 (unreachable): assigns _1 then unreachable.
                BasicBlock {
                    instructions: vec![
                        Instruction::Assign { mode: AssignMode::Copy,
                            dst: Place::local(LocalId(1)),
                            value: Operand::Constant(Constant::I64(99)) },
                    ],
                    terminator: Some(Terminator::Unreachable),
                    span_map: vec![None],
                    terminator_span: None,
                },
            ],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
        };

        let live = Liveness::compute(&func);
        // bb0's analysis is unaffected by the orphan bb1.
        assert!(live.is_live_after(LocalId(1), BlockId(0), 0));
        // After bb0's terminator: nothing live (Unreachable has no successors,
        // and Return consumes _1 into the result).
        assert!(!live.is_live_after(LocalId(1), BlockId(0), 1));
    }

    /// MoveZero kills its source: after `move_zero _x` the local must
    /// not be reported live (it's been transferred out).
    #[test]
    fn move_zero_kills_source() {
        let func = Function {
            name: "f".into(),
            params: vec![],
            return_type: UNIT_TYPE,
            locals: vec![mk_local(UNIT_TYPE), mk_local(I64_TYPE)],
            blocks: vec![BasicBlock {
                instructions: vec![
                    Instruction::Assign { mode: AssignMode::Copy,
                        dst: Place::local(LocalId(1)),
                        value: Operand::Constant(Constant::I64(7)) },
                    Instruction::MoveZero { place: Place::local(LocalId(1)) },
                ],
                terminator: Some(Terminator::Return(Operand::Constant(Constant::Unit))),
                span_map: vec![None, None],
                terminator_span: None,
            }],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
        };

        let live = Liveness::compute(&func);
        // After the MoveZero (inst 1): _1 is dead.
        assert!(!live.is_live_after(LocalId(1), BlockId(0), 1));
    }

    /// Empty function (no blocks) returns an empty Liveness without panic.
    #[test]
    fn empty_function() {
        let func = Function {
            name: "f".into(),
            params: vec![],
            return_type: UNIT_TYPE,
            locals: vec![mk_local(UNIT_TYPE)],
            blocks: Vec::new(),
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
        };
        let live = Liveness::compute(&func);
        assert!(!live.is_live_after(LocalId(0), BlockId(0), 0));
    }
}
