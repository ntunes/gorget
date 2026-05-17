//! Drop elaboration pass — static dataflow analysis over LIR.
//!
//! Replaces runtime `__gorget_drop_if_alive_open__SIZE` / `__gorget_drop_if_alive_close`
//! guards with compile-time knowledge via forward dataflow over the CFG:
//!
//! * **Definitely Uninitialized** → delete the entire guard + drop sequence.
//! * **Definitely Initialized** → strip guard wrappers, keep unconditional drop.
//! * **MaybeInitialized** → replace with a stack-local `bool` drop flag:
//!   `flag := true` at entry, `false` at each `MoveSlot`, checked at the drop site
//!   via `if (flag) { Type__drop(&slot); }`.
//!
//! The dataflow uses `MoveSlot { slot }` (zero-cost annotation) to detect ownership
//! transfer, and `SlotStore` / `Memset` for (re)initialization.  After elaboration:
//! - Companion `Memset`s for deleted guards are removed.
//! - All `MoveSlot` annotations are swept from all blocks.
//! - A follow-up DCE pass cleans orphaned `SlotAddr` / `IConst` values.

use std::collections::{HashMap, HashSet, VecDeque};

use super::{BlockId, DropGuardKind, Inst, LirFunction, LirModule, SlotId, ValueId};

// ── Initialization lattice ───────────────────────────────────────────────────

/// Per-slot initialization state at a program point.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
enum InitState {
    /// The slot holds a live resource value on every predecessor path.
    Initialized,
    /// The slot has been zeroed / moved on every predecessor path.
    Uninitialized,
    /// Some paths initialized, some zeroed — runtime guard still required.
    MaybeInitialized,
}

impl InitState {
    /// Lattice meet: join over predecessor out-states.
    fn meet(a: Self, b: Self) -> Self {
        match (a, b) {
            (Self::Initialized, Self::Initialized) => Self::Initialized,
            (Self::Uninitialized, Self::Uninitialized) => Self::Uninitialized,
            _ => Self::MaybeInitialized,
        }
    }
}

/// Dense per-slot initialization state, indexed by `SlotId.0 as usize`.
///
/// Replaces an earlier `HashMap<SlotId, InitState>`: drop elaboration clones the
/// state per block-visit during the worklist dataflow and again per block in
/// `elaborate_block`. With `InitState` at 1 byte and typical slot counts of
/// 20-100, a dense `Vec` is a memcpy of a few dozen bytes vs hashmap allocator
/// churn. The lattice semantics are unchanged — bounds checks at every read
/// site default to `MaybeInitialized` (safe), matching the prior `.get(...).
/// unwrap_or(MaybeInitialized)` shape.
type SlotStates = Vec<InitState>;

/// Read a slot's state from a dense `SlotStates` vector, defaulting to
/// `MaybeInitialized` for out-of-bounds indices (same safety behaviour as the
/// previous `HashMap::get(...).copied().unwrap_or(MaybeInitialized)`).
#[inline]
fn slot_state(states: &SlotStates, slot: SlotId) -> InitState {
    states
        .get(slot.0 as usize)
        .copied()
        .unwrap_or(InitState::MaybeInitialized)
}

// ── Helper: value → slot map ─────────────────────────────────────────────────

/// Build a map from every ValueId that comes from a `SlotAddr` instruction to
/// the corresponding `SlotId`.  This lets us trace the argument of a
/// `__gorget_drop_if_alive_open__*` call (or a `Memset`) back to its slot.
fn build_val_to_slot(func: &LirFunction) -> HashMap<ValueId, SlotId> {
    let mut map = HashMap::new();
    for block in &func.blocks {
        for inst in &block.insts {
            if let Inst::SlotAddr { dst, slot } = inst {
                map.insert(*dst, *slot);
            }
        }
    }
    map
}

// ── Forward dataflow ─────────────────────────────────────────────────────────

/// Compute the per-block entry (in) states via a standard worklist-based
/// forward dataflow analysis.
///
/// **Entry seeding (bb0 in-state):** by GIR convention, slot 0 is the
/// reserved return-value local and slots `1..=N` are the function
/// parameters; everything beyond is a user local. At function entry,
/// param slots are `Initialized` (the caller wrote the value), every
/// other slot is `Uninitialized` (no user-level store has happened yet).
/// Other blocks start at lattice Top (`None` — no predecessor info yet);
/// the worklist propagates concrete states over the CFG until fixpoint.
///
/// This precise seed is what lets `insert_drop_flags` skip the
/// "blanket false at bb0 + rely on the param SlotStore to fix it"
/// dance: with params seeded `Initialized`, the bb0 flag init below
/// directly emits `flag := true` for owning params, even if their
/// explicit param-`SlotStore` were absent.  See
/// `docs/internals/unified-resource-model.md` §8.1.
fn forward_dataflow(
    func: &LirFunction,
    val_to_slot: &HashMap<ValueId, SlotId>,
) -> Vec<SlotStates> {
    let n = func.blocks.len();
    if n == 0 {
        return Vec::new();
    }

    // bb0 entry: params are Initialized (caller-supplied); slot 0 (return
    // local) and slots beyond the parameter range start Uninitialized.
    let num_params = func.params.len();
    let n_slots = func.slots.len();
    let mut bb0_init: SlotStates = vec![InitState::Uninitialized; n_slots];
    for i in 1..=num_params {
        if i < n_slots {
            bb0_init[i] = InitState::Initialized;
        }
    }

    // Non-entry blocks start as "no information yet" (`None`), which is the
    // lattice Top: meeting Top with any predecessor's out-state adopts that
    // out-state directly. On subsequent predecessor merges we do a real
    // element-wise meet. This avoids the prior bias where seeding non-entry
    // blocks as `all_init` would collapse a sole `Uninitialized` predecessor
    // into `MaybeInitialized` (because `meet(Init, Uninit) = Maybe`), which
    // over-flagged definitely-dead slots.
    let mut in_states: Vec<Option<SlotStates>> = vec![None; n];
    let mut out_states: Vec<SlotStates> = vec![Vec::new(); n];
    in_states[0] = Some(bb0_init);

    // Worklist starts with only the entry block; successors get queued as
    // their in-states acquire information from at least one predecessor.
    let mut worklist: VecDeque<BlockId> = VecDeque::new();
    worklist.push_back(BlockId(0));

    while let Some(bid) = worklist.pop_front() {
        let idx = bid.0 as usize;
        // A block whose in-state is still None has no predecessor processed
        // yet — skip until we get info. (Unreachable blocks stay None.)
        let in_state = match &in_states[idx] {
            Some(s) => s.clone(),
            None => continue,
        };
        let out = compute_transfer(&func.blocks[idx], &in_state, val_to_slot);
        if out != out_states[idx] {
            out_states[idx] = out.clone();
            for succ in func.blocks[idx].terminator.successors() {
                let si = succ.0 as usize;
                if si >= n {
                    continue;
                }
                // Meet the predecessor's out into the successor's in.
                // None on the successor side means "Top" → adopt out as-is;
                // otherwise do an element-wise lattice meet.
                let new_in = match &in_states[si] {
                    None => out.clone(),
                    Some(prev) => meet_states(&out, prev, n_slots),
                };
                if Some(&new_in) != in_states[si].as_ref() {
                    in_states[si] = Some(new_in);
                    worklist.push_back(succ);
                }
            }
        }
    }

    // Unreachable blocks keep an empty in-state (safe default — guard
    // elaboration's `unwrap_or(MaybeInitialized)` via `slot_state` will not
    // eliminate anything for slots without entries).
    in_states.into_iter().map(|s| s.unwrap_or_default()).collect()
}

/// Transfer function for one block: apply each instruction's effect on the
/// slot-initialization state.
fn compute_transfer(
    block: &super::Block,
    in_state: &SlotStates,
    val_to_slot: &HashMap<ValueId, SlotId>,
) -> SlotStates {
    let mut state = in_state.clone();
    for inst in &block.insts {
        apply_inst_effect(inst, &mut state, val_to_slot);
    }
    state
}

/// Apply the effect of a single instruction on the running slot states.
///
/// Writes are bounds-checked against `state.len()` — slot indices beyond the
/// pre-sized vector silently drop their update (matches prior HashMap
/// behaviour where an out-of-range slot simply wasn't in the map; the safe
/// default at every read site is `MaybeInitialized`).
#[inline]
fn apply_inst_effect(
    inst: &Inst,
    state: &mut SlotStates,
    val_to_slot: &HashMap<ValueId, SlotId>,
) {
    match inst {
        // Writing to a slot → definitely initialized.
        Inst::SlotStore { slot, .. } | Inst::ClosurePack { slot, .. } => {
            let idx = slot.0 as usize;
            if let Some(cell) = state.get_mut(idx) {
                *cell = InitState::Initialized;
            }
        }
        // MoveSlot annotation → definitely uninitialized (V4).
        Inst::MoveSlot { slot } => {
            let idx = slot.0 as usize;
            if let Some(cell) = state.get_mut(idx) {
                *cell = InitState::Uninitialized;
            }
        }
        // memset-to-zero of a slot address → definitely uninitialized.
        // Only projected MoveZero (field-level moves) still emits Memset.
        Inst::Memset { ptr, .. } => {
            if let Some(&slot) = val_to_slot.get(ptr) {
                let idx = slot.0 as usize;
                if let Some(cell) = state.get_mut(idx) {
                    *cell = InitState::Uninitialized;
                }
            }
        }
        _ => {}
    }
}

/// Join two slot-state vectors (one entry per slot).  Both inputs are expected
/// to carry concrete information — the `None`/Top case is handled at the
/// call site in `forward_dataflow`. For slots beyond either input's length we
/// assume `MaybeInitialized` (safe default — don't eliminate unknown drops);
/// in practice both vectors are pre-sized to `n_slots`, so the slow path is
/// only taken if the caller is wrong about pre-sizing.
fn meet_states(a: &SlotStates, b: &SlotStates, n_slots: usize) -> SlotStates {
    let mut result = Vec::with_capacity(n_slots);
    for i in 0..n_slots {
        let a_s = a.get(i).copied().unwrap_or(InitState::MaybeInitialized);
        let b_s = b.get(i).copied().unwrap_or(InitState::MaybeInitialized);
        result.push(InitState::meet(a_s, b_s));
    }
    result
}

// ── Guard sequence helpers ────────────────────────────────────────────────────

/// Find the index of the matching `DropGuardClose` for the guard open at
/// `open_idx`, handling nested guard pairs correctly.
///
/// Panics if no matching close is found within the same block. Drop-guard
/// open/close pairs are emitted in matched form by `lir/lower/drops.rs`;
/// any LIR pass that reorders, deletes, or splits guards across blocks
/// MUST preserve nesting, or this assertion fires.
///
/// Hard error rather than silent skip: a missing close means an earlier
/// pass corrupted the LIR. Silently passing through would leave a runtime
/// guard the elaborator promised to optimize away (leak), or worse, leave
/// a stale `DropGuardClose` that paints a later open's drop as ours
/// (silent miscompile). Panic up-front so the responsible pass surfaces.
fn find_matching_close(insts: &[Inst], open_idx: usize) -> usize {
    let mut depth = 1usize;
    for i in (open_idx + 1)..insts.len() {
        match &insts[i] {
            Inst::DropGuardOpen { .. } => depth += 1,
            Inst::DropGuardClose => {
                depth -= 1;
                if depth == 0 {
                    return i;
                }
            }
            _ => {}
        }
    }
    panic!(
        "drop_elab: orphan DropGuardOpen at index {open_idx} in {}-inst block — \
         no matching DropGuardClose. nesting depth at end-of-block: {depth}. \
         opens/closes must stay paired through every LIR pass; check the \
         most recent pre-elaboration transform for a deletion that took an \
         open without its close (or vice versa).",
        insts.len()
    );
}

// ── Per-block elaboration ─────────────────────────────────────────────────────

/// Elaborate drop guards in a single block given the block's entry slot states.
///
/// * `deleted_slots` — accumulates slots whose DropIfAlive guards were fully deleted
///   in the Uninitialized case.  The companion Memsets for these slots are removed in
///   the second pass (`remove_companion_memsets`).
/// * `maybe_init_slots` — accumulates slots that are `MaybeInitialized` at a guard site.
///   These slots get bool drop flags in Phase 3 (`insert_drop_flags`).
///
/// Returns the number of instruction indices added to the deletion set.
fn elaborate_block(
    block: &mut super::Block,
    in_state: &SlotStates,
    val_to_slot: &HashMap<ValueId, SlotId>,
    deleted_slots: &mut HashSet<SlotId>,
    maybe_init_slots: &mut HashSet<SlotId>,
) -> usize {
    let insts = &block.insts;
    let mut to_delete: HashSet<usize> = HashSet::new();
    let mut current_state = in_state.clone();
    let mut i = 0;

    while i < insts.len() {
        let inst = &insts[i];

        // ── Update running state ──────────────────────────────────────────
        apply_inst_effect(inst, &mut current_state, val_to_slot);

        // ── Check for a conditional-drop guard open ───────────────────────
        if let Inst::DropGuardOpen { kind: DropGuardKind::NonZero { .. }, value } = inst {
            {
                // Try to resolve the guarded slot from the guard value.
                let guarded_slot = val_to_slot.get(value).copied();

                if let Some(slot) = guarded_slot {
                    // `find_matching_close` panics on orphan opens — no need to
                    // re-check the result against `insts.len()`.
                    let close_idx = find_matching_close(insts, i);
                    let state = slot_state(&current_state, slot);

                    match state {
                        InitState::Uninitialized => {
                            // Delete the entire guard sequence: open + body + close.
                            // The drop is provably dead — the slot was zeroed on all paths.
                            to_delete.extend(i..=close_idx);
                            // Record that this slot's guard (including the drop call) was
                            // eliminated; companion Memsets for this slot can be removed too.
                            deleted_slots.insert(slot);
                            i = close_idx + 1;
                            continue;
                        }
                        InitState::Initialized => {
                            // Drop is unconditionally live — remove the guard wrapper,
                            // keep the inner drop calls.
                            to_delete.insert(i);        // delete open
                            to_delete.insert(close_idx); // delete close
                            // Fall through to process the inner instructions normally.
                        }
                        InitState::MaybeInitialized => {
                            // Record for Phase 3 (bool drop flag insertion).
                            maybe_init_slots.insert(slot);
                        }
                    }
                }
            }
        }

        i += 1;
    }

    if to_delete.is_empty() {
        return 0;
    }

    let eliminated = to_delete.len();
    // Sweep: retain instructions NOT in the deletion set. Use `Vec::retain`
    // in lockstep on `insts` and `span_map` via a tracking index — both
    // walks visit their owning vec in order, so the same source index
    // lines up 1:1. Avoids allocating a fresh pair of vecs each call.
    let had_parallel_spans = block.span_map.len() == block.insts.len();
    let mut idx = 0usize;
    block.insts.retain(|_| {
        let keep = !to_delete.contains(&idx);
        idx += 1;
        keep
    });
    if had_parallel_spans {
        let mut idx = 0usize;
        block.span_map.retain(|_| {
            let keep = !to_delete.contains(&idx);
            idx += 1;
            keep
        });
    } else {
        // Out-of-sync span_map (pre-1b path): reseed to parallel-empty.
        block.span_map = vec![None; block.insts.len()];
    }
    eliminated
}

// ── Post-elaboration Memset removal ──────────────────────────────────────────

/// Remove `Memset`-to-zero instructions whose target slot had its guard + drop
/// fully deleted (Uninitialized case).  These Memsets are dead — no runtime check
/// or drop will ever read the zeroed data.
///
/// Returns the number of `Memset` instructions removed.
fn remove_companion_memsets(
    func: &mut LirFunction,
    val_to_slot: &HashMap<ValueId, SlotId>,
    deleted_slots: &HashSet<SlotId>,
) -> usize {
    if deleted_slots.is_empty() {
        return 0;
    }
    let mut removed = 0;
    for block in &mut func.blocks {
        // Build a keep mask in-place; if no Memset is dead we skip the
        // mutation step entirely. When deletions are needed, use
        // `Vec::retain` so we drop dead insts in-place rather than
        // allocating a fresh pair of vecs each call.
        let mut dead_in_block = 0usize;
        let keep: Vec<bool> = block
            .insts
            .iter()
            .map(|inst| {
                if let Inst::Memset { ptr, .. } = inst {
                    if let Some(&slot) = val_to_slot.get(ptr) {
                        if deleted_slots.contains(&slot) {
                            dead_in_block += 1;
                            return false;
                        }
                    }
                }
                true
            })
            .collect();
        if dead_in_block == 0 {
            continue;
        }
        removed += dead_in_block;
        let had_parallel_spans = block.span_map.len() == block.insts.len();
        let mut idx = 0usize;
        block.insts.retain(|_| {
            let k = keep[idx];
            idx += 1;
            k
        });
        if had_parallel_spans {
            let mut idx = 0usize;
            block.span_map.retain(|_| {
                let k = keep[idx];
                idx += 1;
                k
            });
        } else {
            block.span_map = vec![None; block.insts.len()];
        }
    }
    removed
}

// ── Phase 3: Bool drop flags for MaybeInitialized slots ─────────────────────

/// For every slot in `maybe_init_slots`, allocate a `bool` drop-flag slot and
/// rewrite the remaining `__gorget_drop_if_alive_open__SIZE` / `_close` guard
/// sequences to use the flag instead of runtime `memcmp`.
///
/// Returns the number of guard sequences replaced with drop flags.
fn insert_drop_flags(
    func: &mut LirFunction,
    val_to_slot: &HashMap<ValueId, SlotId>,
    maybe_init_slots: &HashSet<SlotId>,
    bb0_in_state: &SlotStates,
) -> usize {
    if maybe_init_slots.is_empty() {
        return 0;
    }

    // 1. Allocate a bool flag slot for each MaybeInitialized slot.
    let mut slot_to_flag: HashMap<SlotId, SlotId> = HashMap::new();
    // Sort by slot id for deterministic output.
    let mut sorted_slots: Vec<SlotId> = maybe_init_slots.iter().copied().collect();
    sorted_slots.sort_by_key(|s| s.0);
    for slot in &sorted_slots {
        let flag_id = func.add_slot(
            super::LirType::Bool,
            Some(format!("_df_{}", slot.0)),
        );
        slot_to_flag.insert(*slot, flag_id);
    }

    // 2. Insert flag initialization at the start of the entry block, seeded
    //    from the dataflow's bb0 in-state for each slot.
    //
    //   The flag must reflect whether the slot CURRENTLY holds an owned
    //   resource. At function entry, only function parameters hold a value
    //   (the caller wrote it); locals are still uninitialized memory. The
    //   `forward_dataflow` seed encodes this directly:
    //     - `Initialized` at bb0 ⇒ a parameter (or another unconditionally-
    //       live slot). Init flag = `true`.
    //     - `Uninitialized` at bb0 ⇒ a local that hasn't been stored yet,
    //       or the reserved return-value slot. Init flag = `false`.
    //     - `MaybeInitialized` at bb0 (rare, since bb0 has no predecessors
    //       and the seed is definite) ⇒ conservatively start `false`, the
    //       SlotStore instrumentation below will lift it to `true` on the
    //       first store.
    //
    //   The instrumentation in step 3 transitions the flag to `true` after
    //   each SlotStore and back to `false` after each MoveSlot / Memset, so
    //   the run-time value always tracks the true ownership state.
    //
    //   The previous "blanket false" seed was correct only because the
    //   param-SlotStore at bb0 (lir/lower::lower) flipped param flags to
    //   true at first sight. Seeding from dataflow makes the param case
    //   work without that explicit fix-up — the flag is correct at bb0
    //   entry by construction. See unified-resource-model §8.1.
    //
    //   The loop-reinit pattern (iter N moves slot, iter N+1 re-stores) is
    //   handled by step 3's SlotStore arm — see `drop_loop_reinit.gg`.
    {
        let mut inits: Vec<Inst> = Vec::with_capacity(sorted_slots.len() * 2);
        for slot in &sorted_slots {
            let flag_slot = slot_to_flag[slot];
            let initial = matches!(
                slot_state(bb0_in_state, *slot),
                InitState::Initialized
            );
            let v_init = func.next_value();
            inits.push(Inst::BoolConst { dst: v_init, value: initial });
            inits.push(Inst::SlotStore { slot: flag_slot, value: v_init, is_move: false });
        }
        // Prepend to bb0 so the flags are initialized before any other code.
        // Mirror the prepend on `span_map` so the parallel-array invariant
        // holds — synthetic drop-flag inits carry no source span.
        let n_inits = inits.len();
        let mut combined = inits;
        combined.append(&mut func.blocks[0].insts);
        func.blocks[0].insts = combined;
        let bb0 = &mut func.blocks[0];
        if bb0.span_map.len() + n_inits == bb0.insts.len() {
            let mut combined_spans: Vec<Option<crate::span::Span>> =
                vec![None; n_inits];
            combined_spans.append(&mut bb0.span_map);
            bb0.span_map = combined_spans;
        } else {
            // Pre-1b or out-of-sync span_map: re-seed parallel-empty.
            bb0.span_map = vec![None; bb0.insts.len()];
        }
    }

    // 3. Instrument flag transitions:
    //   * After each SlotStore that writes a flagged slot → flag := true.
    //   * After each MoveSlot / Memset that empties a flagged slot → flag := false.
    //
    //   The SlotStore arm covers re-initialization across iterations
    //   (the loop-reinit pattern that the prior bb0-only init missed)
    //   AND the function-param case (the param-SlotStore at bb0 is
    //   redundant with the dataflow-derived bb0 init now, but harmless —
    //   it just rewrites `flag := true` over an already-true value).
    //
    //   We must avoid instrumenting the SlotStore that writes the FLAG
    //   itself — the flag slot is in `slot_to_flag.values()`, never in
    //   `slot_to_flag.keys()`. Looking up the flag-store's slot in
    //   `slot_to_flag` returns None, so the SlotStore arm naturally
    //   skips its own writes.
    let flag_slots: HashSet<SlotId> = slot_to_flag.values().copied().collect();
    for bi in 0..func.blocks.len() {
        let old_insts = std::mem::take(&mut func.blocks[bi].insts);
        let old_spans = std::mem::take(&mut func.blocks[bi].span_map);
        let had_parallel_spans = old_spans.len() == old_insts.len();
        let mut new_insts: Vec<Inst> = Vec::with_capacity(old_insts.len());
        let mut new_spans: Vec<Option<crate::span::Span>> =
            Vec::with_capacity(old_insts.len());
        for (i, inst) in old_insts.into_iter().enumerate() {
            let src_span = if had_parallel_spans {
                old_spans.get(i).copied().flatten()
            } else {
                None
            };
            let (flag_slot, new_flag_value) = match &inst {
                // SlotStore to a flagged user slot → flag := true (init / re-init).
                // Skip stores to flag slots themselves (we emit those here).
                Inst::SlotStore { slot, .. } if !flag_slots.contains(slot) => {
                    (slot_to_flag.get(slot).copied(), true)
                }
                // MoveSlot annotation → flag := false (moved out).
                Inst::MoveSlot { slot } => {
                    (slot_to_flag.get(slot).copied(), false)
                }
                // Projected MoveZero (field-level moves) emits Memset.
                Inst::Memset { ptr, .. } => {
                    let s = val_to_slot.get(ptr)
                        .and_then(|s| slot_to_flag.get(s))
                        .copied();
                    (s, false)
                }
                _ => (None, false),
            };
            new_insts.push(inst);
            new_spans.push(src_span);
            if let Some(fs) = flag_slot {
                let v = func.next_value();
                // Synthetic flag-tracking writes inherit the triggering
                // instruction's span — they're emitted "for" that store
                // and any trace-time attribution should point there.
                new_insts.push(Inst::BoolConst { dst: v, value: new_flag_value });
                new_spans.push(src_span);
                new_insts.push(Inst::SlotStore { slot: fs, value: v, is_move: false });
                new_spans.push(src_span);
            }
        }
        func.blocks[bi].insts = new_insts;
        func.blocks[bi].span_map = new_spans;
    }

    // 4. Replace guard-open / guard-close sequences.
    //    Only replace guards for known MaybeInitialized slots.  Guards whose
    //    slot can't be traced (e.g., the open arg doesn't map via val_to_slot)
    //    are left untouched.  Track nesting to match closes with their opens.
    let mut replaced = 0;
    for bi in 0..func.blocks.len() {
        let old_insts = std::mem::take(&mut func.blocks[bi].insts);
        let old_spans = std::mem::take(&mut func.blocks[bi].span_map);
        let had_parallel_spans = old_spans.len() == old_insts.len();
        let mut new_insts: Vec<Inst> = Vec::with_capacity(old_insts.len());
        let mut new_spans: Vec<Option<crate::span::Span>> =
            Vec::with_capacity(old_insts.len());
        // Depth counters for nesting: replaced opens vs. passthrough opens.
        let mut flag_depth: usize = 0;
        let mut passthrough_depth: usize = 0;
        for (i, inst) in old_insts.into_iter().enumerate() {
            let src_span = if had_parallel_spans {
                old_spans.get(i).copied().flatten()
            } else {
                None
            };
            match &inst {
                Inst::DropGuardOpen { kind: DropGuardKind::NonZero { .. }, value } => {
                    if let Some(&flag_slot) = val_to_slot.get(value)
                        .and_then(|s| slot_to_flag.get(s))
                    {
                        // Load the bool flag and emit a flag-based guard open.
                        // Both replacement insts inherit the original guard's
                        // span — they're 1:1 stand-ins for it.
                        let v_flag = func.next_value();
                        new_insts.push(Inst::SlotLoad {
                            dst: v_flag,
                            slot: flag_slot,
                            ty: super::LirType::Bool,
                        });
                        new_spans.push(src_span);
                        new_insts.push(Inst::DropGuardOpen {
                            kind: DropGuardKind::Bool,
                            value: v_flag,
                        });
                        new_spans.push(src_span);
                        flag_depth += 1;
                        replaced += 1;
                    } else {
                        // Unknown slot — keep the original guard.
                        passthrough_depth += 1;
                        new_insts.push(inst);
                        new_spans.push(src_span);
                    }
                }
                Inst::DropGuardClose => {
                    if flag_depth > 0 {
                        // Matches a replaced open — emit flag close.
                        flag_depth -= 1;
                        new_insts.push(Inst::DropGuardClose);
                        new_spans.push(src_span);
                    } else if passthrough_depth > 0 {
                        // Matches a passthrough open — keep original close.
                        passthrough_depth -= 1;
                        new_insts.push(inst);
                        new_spans.push(src_span);
                    } else {
                        // Orphan close — keep as-is (shouldn't happen).
                        new_insts.push(inst);
                        new_spans.push(src_span);
                    }
                }
                _ => {
                    new_insts.push(inst);
                    new_spans.push(src_span);
                }
            }
        }
        func.blocks[bi].insts = new_insts;
        func.blocks[bi].span_map = new_spans;
    }

    replaced
}

// ── Public entry point ────────────────────────────────────────────────────────

/// Statistics returned by `elaborate_drops`.
#[derive(Debug, Default)]
pub struct ElabStats {
    /// LIR instructions eliminated by guard elaboration (opens + drop calls + closes).
    pub guards_eliminated: usize,
    /// `Memset`-to-zero instructions removed after guard elimination.
    pub memsets_removed: usize,
    /// Guard sequences replaced with bool drop flags (MaybeInitialized → flag check).
    pub flags_inserted: usize,
    /// `MoveSlot` annotations consumed and removed.
    pub move_slots_removed: usize,
}

impl ElabStats {
    /// Total instructions eliminated across all phases.
    pub fn total(&self) -> usize {
        self.guards_eliminated + self.memsets_removed + self.move_slots_removed
    }
}

/// Run drop elaboration on every function in the module:
///   1. Guard elimination — Uninitialized → delete, Initialized → strip.
///   2. Companion Memset removal for deleted (Uninitialized) slots.
///   3. Bool drop flags for MaybeInitialized slots.
///   4. MoveSlot sweep — remove consumed annotations.
pub fn elaborate_drops(module: &mut LirModule) -> ElabStats {
    let mut stats = ElabStats::default();
    for func in &mut module.functions {
        // Build the val→slot map once; it remains valid across all passes because
        // elaborate_block only removes instructions inside guard ranges, never the
        // SlotAddr instructions that precede Memsets.
        let val_to_slot = build_val_to_slot(func);
        let in_states = forward_dataflow(func, &val_to_slot);

        // Phase 1: elaborate guard sequences (Uninitialized → delete, Initialized → strip).
        let mut deleted_slots: HashSet<SlotId> = HashSet::new();
        let mut maybe_init_slots: HashSet<SlotId> = HashSet::new();
        for (i, block) in func.blocks.iter_mut().enumerate() {
            if let Some(state) = in_states.get(i) {
                stats.guards_eliminated += elaborate_block(
                    block, state, &val_to_slot,
                    &mut deleted_slots, &mut maybe_init_slots,
                );
            }
        }

        // Phase 2: remove companion Memsets for slots whose guards (and drop calls)
        // were fully deleted.  Only runs if Phase 1 found Uninitialized guard deletions.
        stats.memsets_removed +=
            remove_companion_memsets(func, &val_to_slot, &deleted_slots);

        // Phase 3: insert bool drop flags for MaybeInitialized slots, replacing the
        // remaining memcmp-based guards with cheap bool checks. Seed the bb0 flag
        // values from the dataflow's bb0 in-state so params start `true` and
        // locals start `false` without relying on the param-SlotStore fix-up.
        let bb0_in_state = in_states
            .first()
            .cloned()
            .unwrap_or_default();
        stats.flags_inserted +=
            insert_drop_flags(func, &val_to_slot, &maybe_init_slots, &bb0_in_state);

        // Phase 4: remove consumed MoveSlot annotations.  They served as dataflow
        // signals for phases 1-3 and have no runtime effect. Build a `keep`
        // mask in-place and skip the block entirely when none are found; for
        // blocks with MoveSlots, use `Vec::retain` lockstep on insts +
        // span_map. Avoids two vec allocations per block per function.
        for block in &mut func.blocks {
            let mut dead = 0usize;
            let keep: Vec<bool> = block
                .insts
                .iter()
                .map(|inst| {
                    if matches!(inst, Inst::MoveSlot { .. }) {
                        dead += 1;
                        false
                    } else {
                        true
                    }
                })
                .collect();
            if dead == 0 {
                continue;
            }
            stats.move_slots_removed += dead;
            let had_parallel_spans = block.span_map.len() == block.insts.len();
            let mut idx = 0usize;
            block.insts.retain(|_| {
                let k = keep[idx];
                idx += 1;
                k
            });
            if had_parallel_spans {
                let mut idx = 0usize;
                block.span_map.retain(|_| {
                    let k = keep[idx];
                    idx += 1;
                    k
                });
            } else {
                block.span_map = vec![None; block.insts.len()];
            }
        }
    }
    stats
}
