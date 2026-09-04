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
//! **Storage:** the live-sets use a dense [`LocalBitSet`] (one bit per
//! local ID) rather than `FxHashSet<u32>`. Local IDs are dense 0..N within
//! a function — typically a few dozen to a few hundred per function — so a
//! bitset is both smaller in memory and dramatically faster for the bulk
//! `union` / `clone` / `equality` operations that dominate fixpoint
//! iteration. See the 2026-05-17 perf commit for measurements.
//!
//! See `docs/devbook/25-structural-guards.md` Tier 2a for the broader
//! consume-site discipline this enables, and the `validate` module's
//! `ConsumeSite` walker for the consumer.

use super::instructions::{Instruction, Operand, Place, Projection, Terminator};
use super::types::{BlockId, LocalId};
use super::Function;

/// Dense bitset over local IDs (0..N).
///
/// Local IDs are dense within a function, so a bitset gives O(1)
/// insert/remove/contains and O(N/64) bulk ops (clone, equality, union,
/// difference) — substantially faster than `FxHashSet<u32>` for the
/// hot path of backward-dataflow liveness, where every fixpoint
/// iteration clones and unions per-block IN-sets across the CFG.
#[derive(Clone, Default, Debug, PartialEq, Eq)]
struct LocalBitSet {
    /// Each word stores 64 bit-flags. `bits[i] & (1 << (id % 64))` is set
    /// iff the local with ID `i*64 + (id % 64)` is in the set.
    bits: Vec<u64>,
}

impl LocalBitSet {
    /// New empty set sized for `n_locals` local IDs (rounded up to the
    /// next multiple of 64).
    #[inline]
    fn with_capacity(n_locals: usize) -> Self {
        let words = n_locals.div_ceil(64).max(1);
        Self { bits: vec![0u64; words] }
    }

    /// Ensure capacity to hold a bit for `id`. Used when growing on
    /// insert (some locals can appear in operand streams above the
    /// nominal `func.locals.len()` cap during construction — keep the
    /// API forgiving rather than asserting).
    #[inline]
    fn ensure(&mut self, id: u32) {
        let word = (id as usize) / 64;
        if word >= self.bits.len() {
            self.bits.resize(word + 1, 0);
        }
    }

    #[inline]
    fn insert(&mut self, id: u32) {
        self.ensure(id);
        let word = (id as usize) / 64;
        let bit = id & 63;
        self.bits[word] |= 1u64 << bit;
    }

    #[inline]
    fn remove(&mut self, id: u32) {
        let word = (id as usize) / 64;
        if word < self.bits.len() {
            let bit = id & 63;
            self.bits[word] &= !(1u64 << bit);
        }
    }

    #[inline]
    fn contains(&self, id: u32) -> bool {
        let word = (id as usize) / 64;
        if word >= self.bits.len() {
            return false;
        }
        let bit = id & 63;
        (self.bits[word] & (1u64 << bit)) != 0
    }

    /// `self |= other`. Grows `self` if `other` is larger.
    #[inline]
    fn union_with(&mut self, other: &Self) {
        if other.bits.len() > self.bits.len() {
            self.bits.resize(other.bits.len(), 0);
        }
        for (s, o) in self.bits.iter_mut().zip(other.bits.iter()) {
            *s |= *o;
        }
    }
}

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
    live_after: Vec<Vec<LocalBitSet>>,
}

/// Whether `MoveZero` counts as a DEF (a kill) for a liveness run.
///
/// The two policies answer two genuinely different questions, and the
/// consume-site validator needs both:
///
/// * [`MoveZeroPolicy::Kills`] — the DEFAULT and the semantic truth for
///   every consumer that asks "may this local still be read?". After
///   `move_zero _x` the slot is logically dead; a later read is
///   `UseAfterMove` and a separate validator's business.
/// * [`MoveZeroPolicy::Blind`] — asks "would this local still be read if
///   the `MoveZero` were not there?". This is the ONLY question that can
///   observe a *staging* move that invents its own `MoveZero`: such a site
///   emits `[Mv] dst = copy src; move_zero src` on a source that is read
///   again downstream, so under `Kills` **the defect is its own alibi** and
///   no walk over the instruction stream can see it.
///
/// `Blind` is consulted ONLY as a second opinion, for the `Assign`
/// consume-site class, and only after the `Kills` run has already returned
/// "sound" (`src/ir/validate.rs`). The shared kill edge itself is never
/// altered — every other `is_live_after` consumer keeps the `Kills`
/// semantics.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MoveZeroPolicy {
    /// `MoveZero` is a def: the local's value stops being live there.
    Kills,
    /// `MoveZero` contributes neither a def nor a read — reads downstream
    /// of it keep the local live across the move.
    Blind,
}

impl Liveness {
    /// Compute liveness for `func`. Empty / malformed functions return an
    /// empty result — the validator simply sees no live-out information,
    /// which is the safe default for the consume-site rule (it errs toward
    /// "source is dead" only when explicitly proven so).
    pub fn compute(func: &Function) -> Self {
        Self::compute_with(func, MoveZeroPolicy::Kills)
    }

    /// [`Liveness::compute`] with `MoveZero` treated as neither def nor
    /// read — see [`MoveZeroPolicy::Blind`] for why this second instrument
    /// exists and what it is allowed to be used for.
    pub fn compute_move_zero_blind(func: &Function) -> Self {
        Self::compute_with(func, MoveZeroPolicy::Blind)
    }

    fn compute_with(func: &Function, mz: MoveZeroPolicy) -> Self {
        let n_blocks = func.blocks.len();
        if n_blocks == 0 {
            return Self { live_after: Vec::new() };
        }

        // Bitset width follows `func.locals.len()`; on the rare occasion
        // an operand references a higher id (transient lowering artifact),
        // `LocalBitSet::insert` auto-grows.
        let n_locals = func.locals.len();

        // Per-block USES (read before def in the block) and DEFS (locals
        // defined anywhere in the block, including the terminator). Standard
        // gen/kill formulation; computed once per block, reused each fixpoint
        // iteration.
        let mut uses: Vec<LocalBitSet> = Vec::with_capacity(n_blocks);
        let mut defs: Vec<LocalBitSet> = Vec::with_capacity(n_blocks);
        for bb in func.blocks.iter() {
            let (u, d) = compute_block_use_def(bb, n_locals, mz);
            uses.push(u);
            defs.push(d);
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
        let mut in_sets: Vec<LocalBitSet> =
            (0..n_blocks).map(|_| LocalBitSet::with_capacity(n_locals)).collect();
        // Reusable scratch buffer for new_in computation — avoids per-iteration
        // allocations in the dataflow loop.
        let mut new_in = LocalBitSet::with_capacity(n_locals);
        loop {
            let mut changed = false;
            // Reverse order tends to converge faster for forward CFGs but
            // the worklist semantics are the same; this is plenty fast.
            for bi in (0..n_blocks).rev() {
                // Build new_in = (∪ in_sets[s] for s in succs[bi]) \ defs[bi] ∪ uses[bi]
                // Reuse the scratch buffer: clear, fill, compare.
                for w in new_in.bits.iter_mut() { *w = 0; }
                for &s in &succs[bi] {
                    new_in.union_with(&in_sets[s]);
                }
                // Subtract defs (per-word AND-NOT), then OR in uses (per-word OR).
                // Both ops are intrinsically O(words); no growth needed since
                // all three operands share the same n_locals capacity.
                let dwords = defs[bi].bits.len();
                let uwords = uses[bi].bits.len();
                if new_in.bits.len() < dwords.max(uwords) {
                    new_in.bits.resize(dwords.max(uwords), 0);
                }
                for (i, w) in new_in.bits.iter_mut().enumerate() {
                    let dw = defs[bi].bits.get(i).copied().unwrap_or(0);
                    *w &= !dw;
                    let uw = uses[bi].bits.get(i).copied().unwrap_or(0);
                    *w |= uw;
                }
                if new_in != in_sets[bi] {
                    in_sets[bi].bits.clear();
                    in_sets[bi].bits.extend_from_slice(&new_in.bits);
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
        let mut live_after: Vec<Vec<LocalBitSet>> = Vec::with_capacity(n_blocks);
        let mut inst_defs_scratch: Vec<u32> = Vec::new();
        let mut inst_reads_scratch: Vec<u32> = Vec::new();
        for bi in 0..n_blocks {
            let bb = &func.blocks[bi];
            let n_inst = bb.instructions.len();
            // OUT[B] for the terminator slot.
            let mut out_b = LocalBitSet::with_capacity(n_locals);
            for &s in &succs[bi] {
                out_b.union_with(&in_sets[s]);
            }
            // The terminator itself reads operands (e.g. `Return(value)`,
            // `Branch.cond`). Those reads are live BEFORE the terminator —
            // i.e., live AFTER the last instruction. So we compute
            // live_after(inst[n_inst-1]) = (out_b - term_defs) ∪ term_uses.
            // Terminators don't define locals (Invoke's dst is on the
            // normal-edge entry point, which is treated as a use for
            // simplicity — see [collect_terminator_reads]; we don't
            // remove anything here).
            let mut after_last = out_b.clone();
            if let Some(t) = &bb.terminator {
                collect_terminator_reads_into(t, &mut after_last);
            }

            // Build per-instruction live_after backward. Vec length = n_inst+1.
            let mut per_inst: Vec<LocalBitSet> =
                (0..=n_inst).map(|_| LocalBitSet::with_capacity(n_locals)).collect();
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
                inst_defs_scratch.clear();
                collect_inst_defs_into(inst, &mut inst_defs_scratch, mz);
                for d in &inst_defs_scratch {
                    current.remove(*d);
                }
                inst_reads_scratch.clear();
                collect_inst_reads_into(inst, &mut inst_reads_scratch);
                for r in &inst_reads_scratch {
                    current.insert(*r);
                }
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
        per_inst[inst_index].contains(local.0)
    }
}

/// Locals defined (written) by an instruction. Definitions kill liveness:
/// after `_x = ...` the previous live-set for `_x` is rewritten by this
/// new definition; uses inside the same instruction's RHS are read first
/// (we add them in `collect_inst_reads`).
fn collect_inst_defs_into(inst: &Instruction, defs: &mut Vec<u32>, mz: MoveZeroPolicy) {
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
        | Instruction::FaultableBinOp { dst, .. }
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
        Instruction::MoveZero { place }
            if place.projections.is_empty() && mz == MoveZeroPolicy::Kills =>
        {
            // MoveZero conceptually transfers ownership and zeroes the
            // source slot. For liveness purposes this is a "kill": the
            // local's value stops being live (the next read would be
            // UseAfterMove). Treating MoveZero as a def here is what
            // makes the consume-site rule's "live_after = false after
            // a Move" hold automatically.
            //
            // ⚠ That automatic hold is also a BLIND SPOT, which is why
            // [`MoveZeroPolicy::Blind`] exists: a staging site that emits
            // its own `move_zero` on a still-read source satisfies the
            // consume-site rule *because of the instruction that creates
            // the bug*. The `Blind` policy skips this arm so the validator
            // can ask the counterfactual. See `MoveZeroPolicy`.
            defs.push(place.local.0);
        }
        // No definition: side-effects only, projections, drops.
        _ => {}
    }
}

/// Locals read by an instruction. Mirrors [`super::validate::collect_read_locals_for_validate`]
/// but writes into a reusable `Vec<u32>` to avoid per-call allocation.
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
fn collect_inst_reads_into(inst: &Instruction, reads: &mut Vec<u32>) {
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
                push_place(reads, dst);
            }
            push_op(reads, value);
        }
        Instruction::BinOp { lhs, rhs, .. }
        | Instruction::FaultableBinOp { lhs, rhs, .. }
        | Instruction::Cmp { lhs, rhs, .. } => {
            push_op(reads, lhs);
            push_op(reads, rhs);
        }
        Instruction::UnOp { operand, .. }
        | Instruction::Cast { value: operand, .. }
        | Instruction::BitCast { value: operand, .. }
        | Instruction::PtrCast { value: operand, .. }
        | Instruction::TagOf { operand, .. } => {
            push_op(reads, operand);
        }
        Instruction::FieldLoad { base, .. } | Instruction::EnumFieldLoad { base, .. } => {
            push_place(reads, base);
        }
        Instruction::IndexLoad { base, index, .. } => {
            push_place(reads, base);
            push_op(reads, index);
        }
        Instruction::Call { args, .. } | Instruction::CallExtern { args, .. } => {
            for a in args { push_op(reads, a); }
        }
        Instruction::CallIndirect { callee, args, .. } => {
            push_op(reads, callee);
            for a in args { push_op(reads, a); }
        }
        Instruction::StructInit { fields, .. } | Instruction::EnumInit { fields, .. } => {
            for f in fields { push_op(reads, f); }
        }
        Instruction::TupleInit { elements, .. } => {
            for e in elements { push_op(reads, e); }
        }
        Instruction::Borrow { place, .. } | Instruction::BorrowMut { place, .. } => {
            push_place(reads, place);
        }
        Instruction::HeapAlloc { allocator, .. } => { push_op(reads, allocator); }
        Instruction::HeapAllocArray { count, allocator, .. } => {
            push_op(reads, count);
            push_op(reads, allocator);
        }
        Instruction::Dealloc { ptr, allocator } => {
            push_op(reads, ptr);
            push_op(reads, allocator);
        }
        Instruction::LoadRef { src, .. } => {
            push_place(reads, src);
        }
        Instruction::StoreRef { dst, value } => {
            // StoreRef writes through dst — read the address, read the value.
            push_place(reads, dst);
            push_op(reads, value);
        }
        Instruction::PushAllocator { allocator } => { push_op(reads, allocator); }
        Instruction::GlobalAssign { value, .. } => { push_op(reads, value); }
        // Drops / MoveZero / no-ops contribute nothing — see the
        // doc-comment for the rationale.
        Instruction::Drop { .. } | Instruction::DropIfAlive { .. }
        | Instruction::MoveZero { .. }
        | Instruction::PopAllocator | Instruction::Nop
        | Instruction::InlineC { .. } | Instruction::LoadThreadLocal { .. } => {}
    }
}

/// Per-block USE / DEF sets for the dataflow.
///
/// Standard formulation:
/// - USE[B] = locals read in B before any definition of them in B.
/// - DEF[B] = locals defined anywhere in B.
fn compute_block_use_def(
    bb: &super::BasicBlock,
    n_locals: usize,
    mz: MoveZeroPolicy,
) -> (LocalBitSet, LocalBitSet) {
    let mut use_set = LocalBitSet::with_capacity(n_locals);
    let mut def_set = LocalBitSet::with_capacity(n_locals);
    let mut reads_scratch: Vec<u32> = Vec::new();
    let mut defs_scratch: Vec<u32> = Vec::new();
    for inst in &bb.instructions {
        // Reads are USE only if not yet DEFined in this block.
        reads_scratch.clear();
        collect_inst_reads_into(inst, &mut reads_scratch);
        for r in &reads_scratch {
            if !def_set.contains(*r) {
                use_set.insert(*r);
            }
        }
        defs_scratch.clear();
        collect_inst_defs_into(inst, &mut defs_scratch, mz);
        for d in &defs_scratch {
            def_set.insert(*d);
        }
    }
    // Terminator reads are USEs (after all instructions). If a terminator
    // operand reads a local and that local was DEFed in the block, the
    // read is satisfied locally — don't add to USE. Otherwise add.
    if let Some(t) = &bb.terminator {
        let mut term_reads = LocalBitSet::with_capacity(n_locals);
        collect_terminator_reads_into(t, &mut term_reads);
        // use_set |= (term_reads & !def_set)
        let need = term_reads.bits.len().max(def_set.bits.len());
        if use_set.bits.len() < need {
            use_set.bits.resize(need, 0);
        }
        for (i, w) in use_set.bits.iter_mut().enumerate() {
            let tw = term_reads.bits.get(i).copied().unwrap_or(0);
            let dw = def_set.bits.get(i).copied().unwrap_or(0);
            *w |= tw & !dw;
        }
    }
    (use_set, def_set)
}

/// Locals read by a terminator, written directly into a [`LocalBitSet`].
/// `Invoke` is treated as reading args; its `dst` is a definition that
/// activates only on the `normal` edge — for liveness purposes we keep it
/// simple and treat the value as defined at the start of the `normal`
/// block (in practice every callsite is the last instruction, and the
/// Invoke's dst flows into the block via Phi-like mechanisms today). The
/// conservative answer (don't kill `dst` here) is safe: we may report a
/// value as live one block later than necessary, which never produces a
/// false consume-site violation.
fn collect_terminator_reads_into(t: &Terminator, set: &mut LocalBitSet) {
    let push_op = |set: &mut LocalBitSet, op: &Operand| {
        if let Operand::Copy(p) | Operand::Move(p) = op {
            set.insert(p.local.0);
            for proj in &p.projections {
                if let Projection::Index(id) = proj { set.insert(id.0); }
            }
        }
    };
    match t {
        Terminator::Return(v) => push_op(set, v),
        Terminator::Branch { cond, .. } => push_op(set, cond),
        Terminator::Switch { value, .. } => push_op(set, value),
        Terminator::Invoke { args, .. } => {
            for a in args { push_op(set, a); }
        }
        Terminator::Jump(_) | Terminator::Unreachable => {}
    }
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
            deref_of_owning_param: None,
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
            takes_env: false,
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
            takes_env: false,
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
            takes_env: false,
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
            takes_env: false,
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
            takes_env: false,
        };

        let live = Liveness::compute(&func);
        // After the MoveZero (inst 1): _1 is dead.
        assert!(!live.is_live_after(LocalId(1), BlockId(0), 1));
    }

    /// The staging-move shape, and the two policies giving OPPOSITE answers on
    /// it — which is the whole reason [`MoveZeroPolicy::Blind`] exists.
    ///
    /// ```text
    /// 0: _1 = 7
    /// 1: [Mv] _2 = copy _1        <- the staging assign
    /// 2:      move_zero _1        <- emitted by the same site
    /// 3:      _3 = copy _1        <- a REAL later read
    /// ```
    ///
    /// Under `Kills`, `_1` is dead after instruction 1 — the site's own
    /// `MoveZero` vouches for it, so the consume-site validator reads the
    /// staging assign as sound. Under `Blind`, `_1` is live after instruction
    /// 1, because the read at 3 is real. The `Blind` answer is the one that
    /// matches what the backend actually does: it elides the zero exactly when
    /// drop-tracking proves the read unobservable, leaving `_1` and `_2`
    /// aliasing one buffer.
    #[test]
    fn move_zero_blind_sees_the_read_the_kill_policy_hides() {
        let func = Function {
            name: "f".into(),
            params: vec![],
            return_type: UNIT_TYPE,
            locals: vec![
                mk_local(UNIT_TYPE),
                mk_local(I64_TYPE),
                mk_local(I64_TYPE),
                mk_local(I64_TYPE),
            ],
            blocks: vec![BasicBlock {
                instructions: vec![
                    Instruction::Assign { mode: AssignMode::Copy,
                        dst: Place::local(LocalId(1)),
                        value: Operand::Constant(Constant::I64(7)) },
                    Instruction::Assign { mode: AssignMode::Move,
                        dst: Place::local(LocalId(2)),
                        value: Operand::Copy(Place::local(LocalId(1))) },
                    Instruction::MoveZero { place: Place::local(LocalId(1)) },
                    Instruction::Assign { mode: AssignMode::Copy,
                        dst: Place::local(LocalId(3)),
                        value: Operand::Copy(Place::local(LocalId(1))) },
                ],
                terminator: Some(Terminator::Return(Operand::Constant(Constant::Unit))),
                span_map: vec![None, None, None, None],
                terminator_span: None,
            }],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
            takes_env: false,
        };

        let kills = Liveness::compute(&func);
        let blind = Liveness::compute_move_zero_blind(&func);
        // At the staging assign (inst 1) the two policies DISAGREE — that
        // disagreement is exactly the `StagingMoveIntoOwnedSlot` finding.
        assert!(!kills.is_live_after(LocalId(1), BlockId(0), 1));
        assert!(blind.is_live_after(LocalId(1), BlockId(0), 1));
        // And they AGREE everywhere the disagreement would be a false
        // positive: after the real read, `_1` is dead under both.
        assert!(!kills.is_live_after(LocalId(1), BlockId(0), 3));
        assert!(!blind.is_live_after(LocalId(1), BlockId(0), 3));
    }

    /// The correct sibling: same staging shape, NO later read. Both policies
    /// must agree that the source is dead, or the guard built on `Blind` would
    /// fire on every honest move in the corpus.
    #[test]
    fn move_zero_blind_agrees_when_there_is_no_later_read() {
        let func = Function {
            name: "f".into(),
            params: vec![],
            return_type: UNIT_TYPE,
            locals: vec![mk_local(UNIT_TYPE), mk_local(I64_TYPE), mk_local(I64_TYPE)],
            blocks: vec![BasicBlock {
                instructions: vec![
                    Instruction::Assign { mode: AssignMode::Copy,
                        dst: Place::local(LocalId(1)),
                        value: Operand::Constant(Constant::I64(7)) },
                    Instruction::Assign { mode: AssignMode::Move,
                        dst: Place::local(LocalId(2)),
                        value: Operand::Copy(Place::local(LocalId(1))) },
                    Instruction::MoveZero { place: Place::local(LocalId(1)) },
                ],
                terminator: Some(Terminator::Return(Operand::Constant(Constant::Unit))),
                span_map: vec![None, None, None],
                terminator_span: None,
            }],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
            takes_env: false,
        };

        let kills = Liveness::compute(&func);
        let blind = Liveness::compute_move_zero_blind(&func);
        assert!(!kills.is_live_after(LocalId(1), BlockId(0), 1));
        assert!(!blind.is_live_after(LocalId(1), BlockId(0), 1));
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
            takes_env: false,
        };
        let live = Liveness::compute(&func);
        assert!(!live.is_live_after(LocalId(0), BlockId(0), 0));
    }

    /// Bitset bulk-op smoke test: ensures `union_with` grows correctly
    /// when the right operand is larger than self, and that high-id
    /// inserts auto-grow via `ensure`.
    #[test]
    fn bitset_grows() {
        let mut a = LocalBitSet::with_capacity(64);
        let mut b = LocalBitSet::with_capacity(200);
        b.insert(150);
        b.insert(199);
        a.union_with(&b);
        assert!(a.contains(150));
        assert!(a.contains(199));
        assert!(!a.contains(151));
        a.insert(500);
        assert!(a.contains(500));
        a.remove(150);
        assert!(!a.contains(150));
        assert!(a.contains(199));
    }
}
