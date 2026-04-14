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

use super::{BlockId, Inst, LirFunction, LirModule, SlotId, ValueId};

// ── Initialization lattice ───────────────────────────────────────────────────

/// Per-slot initialization state at a program point.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
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

type SlotStates = HashMap<SlotId, InitState>;

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
/// Initial assumption: every slot is `Initialized` (optimistic — we only
/// propagate `Uninitialized` from confirmed `Memset`-to-zero sites).
fn forward_dataflow(
    func: &LirFunction,
    val_to_slot: &HashMap<ValueId, SlotId>,
) -> Vec<SlotStates> {
    let n = func.blocks.len();
    if n == 0 {
        return Vec::new();
    }

    // All slots start Initialized.
    let all_init: SlotStates = (0..func.slots.len() as u32)
        .map(|i| (SlotId(i), InitState::Initialized))
        .collect();

    let mut in_states: Vec<SlotStates> = vec![all_init.clone(); n];
    let mut out_states: Vec<SlotStates> = vec![HashMap::new(); n];
    in_states[0] = all_init;

    // Process every block at least once.
    let mut worklist: VecDeque<BlockId> = (0..n as u32).map(BlockId).collect();

    while let Some(bid) = worklist.pop_front() {
        let idx = bid.0 as usize;
        let out = compute_transfer(&func.blocks[idx], &in_states[idx], val_to_slot);
        if out != out_states[idx] {
            out_states[idx] = out.clone();
            for succ in func.blocks[idx].terminator.successors() {
                let si = succ.0 as usize;
                if si >= n {
                    continue;
                }
                let new_in = meet_states(&out, &in_states[si], func.slots.len());
                if new_in != in_states[si] {
                    in_states[si] = new_in;
                    worklist.push_back(succ);
                }
            }
        }
    }

    in_states
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
#[inline]
fn apply_inst_effect(
    inst: &Inst,
    state: &mut SlotStates,
    val_to_slot: &HashMap<ValueId, SlotId>,
) {
    match inst {
        // Writing to a slot → definitely initialized.
        Inst::SlotStore { slot, .. } => {
            state.insert(*slot, InitState::Initialized);
        }
        // MoveSlot annotation → definitely uninitialized (V4).
        Inst::MoveSlot { slot } => {
            state.insert(*slot, InitState::Uninitialized);
        }
        // memset-to-zero of a slot address → definitely uninitialized.
        // Only projected MoveZero (field-level moves) still emits Memset.
        Inst::Memset { ptr, .. } => {
            if let Some(&slot) = val_to_slot.get(ptr) {
                state.insert(slot, InitState::Uninitialized);
            }
        }
        _ => {}
    }
}

/// Join two slot-state maps (one entry per slot).  For slots absent in a map
/// we assume `MaybeInitialized` (safe default — don't eliminate unknown drops).
fn meet_states(a: &SlotStates, b: &SlotStates, n_slots: usize) -> SlotStates {
    let mut result = HashMap::with_capacity(n_slots);
    for i in 0..n_slots as u32 {
        let sid = SlotId(i);
        let a_s = a.get(&sid).copied().unwrap_or(InitState::MaybeInitialized);
        let b_s = b.get(&sid).copied().unwrap_or(InitState::MaybeInitialized);
        result.insert(sid, InitState::meet(a_s, b_s));
    }
    result
}

// ── Guard sequence helpers ────────────────────────────────────────────────────

/// Return `true` if `name` is a `__gorget_drop_if_alive_open__*` call.
#[inline]
fn is_guard_open(name: &str) -> bool {
    name.starts_with("__gorget_drop_if_alive_open__")
}

/// Return `true` if `name` is the `__gorget_drop_if_alive_close` call.
#[inline]
fn is_guard_close(name: &str) -> bool {
    name == "__gorget_drop_if_alive_close"
}

/// Find the index of the matching `__gorget_drop_if_alive_close` for the guard
/// open at `open_idx`, handling nested guard pairs correctly.
///
/// Returns `insts.len()` (past-the-end) if no matching close is found (which
/// indicates malformed LIR — shouldn't happen in well-formed output).
fn find_matching_close(insts: &[Inst], open_idx: usize) -> usize {
    let mut depth = 1usize;
    for i in (open_idx + 1)..insts.len() {
        match &insts[i] {
            Inst::CallExtern { name, .. } if is_guard_open(name) => depth += 1,
            Inst::CallExtern { name, .. } if is_guard_close(name) => {
                depth -= 1;
                if depth == 0 {
                    return i;
                }
            }
            _ => {}
        }
    }
    // No matching close found — past-the-end signals caller to skip elaboration.
    insts.len()
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
        if let Inst::CallExtern { name, args, .. } = inst {
            if is_guard_open(name) {
                // Try to resolve the guarded slot from the first argument.
                let guarded_slot = args
                    .first()
                    .and_then(|v| val_to_slot.get(v))
                    .copied();

                if let Some(slot) = guarded_slot {
                    let close_idx = find_matching_close(insts, i);
                    // Only act if we found a valid matching close.
                    if close_idx < insts.len() {
                        let state = current_state
                            .get(&slot)
                            .copied()
                            .unwrap_or(InitState::MaybeInitialized);

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
        }

        i += 1;
    }

    if to_delete.is_empty() {
        return 0;
    }

    let eliminated = to_delete.len();
    // Sweep: retain instructions NOT in the deletion set.
    let mut new_insts = Vec::with_capacity(insts.len() - eliminated);
    for (idx, inst) in block.insts.drain(..).enumerate() {
        if !to_delete.contains(&idx) {
            new_insts.push(inst);
        }
    }
    block.insts = new_insts;
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
        let before = block.insts.len();
        block.insts.retain(|inst| {
            if let Inst::Memset { ptr, .. } = inst {
                if let Some(&slot) = val_to_slot.get(ptr) {
                    if deleted_slots.contains(&slot) {
                        return false; // remove this companion Memset
                    }
                }
            }
            true
        });
        removed += before - block.insts.len();
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

    // 2. Insert flag initialization (`true`) at the start of the entry block (bb0).
    {
        let mut inits: Vec<Inst> = Vec::with_capacity(sorted_slots.len() * 2);
        for slot in &sorted_slots {
            let flag_slot = slot_to_flag[slot];
            let v_true = func.next_value();
            inits.push(Inst::BoolConst { dst: v_true, value: true });
            inits.push(Inst::SlotStore { slot: flag_slot, value: v_true, is_move: false });
        }
        // Prepend to bb0 so the flags are initialized before any other code.
        let mut combined = inits;
        combined.append(&mut func.blocks[0].insts);
        func.blocks[0].insts = combined;
    }

    // 3. After each MoveSlot / Memset that moves a flagged slot, insert `flag := false`.
    for bi in 0..func.blocks.len() {
        let old_insts = std::mem::take(&mut func.blocks[bi].insts);
        let mut new_insts: Vec<Inst> = Vec::with_capacity(old_insts.len());
        for inst in old_insts {
            let flag_slot = if let Inst::MoveSlot { slot } = &inst {
                slot_to_flag.get(slot).copied()
            } else if let Inst::Memset { ptr, .. } = &inst {
                // Projected MoveZero (field-level moves) still emits Memset.
                val_to_slot.get(ptr)
                    .and_then(|s| slot_to_flag.get(s))
                    .copied()
            } else {
                None
            };
            new_insts.push(inst);
            if let Some(fs) = flag_slot {
                let v_false = func.next_value();
                new_insts.push(Inst::BoolConst { dst: v_false, value: false });
                new_insts.push(Inst::SlotStore { slot: fs, value: v_false, is_move: false });
            }
        }
        func.blocks[bi].insts = new_insts;
    }

    // 4. Replace guard-open / guard-close sequences.
    //    Only replace guards for known MaybeInitialized slots.  Guards whose
    //    slot can't be traced (e.g., the open arg doesn't map via val_to_slot)
    //    are left untouched.  Track nesting to match closes with their opens.
    let mut replaced = 0;
    for bi in 0..func.blocks.len() {
        let old_insts = std::mem::take(&mut func.blocks[bi].insts);
        let mut new_insts: Vec<Inst> = Vec::with_capacity(old_insts.len());
        // Depth counters for nesting: replaced opens vs. passthrough opens.
        let mut flag_depth: usize = 0;
        let mut passthrough_depth: usize = 0;
        for inst in old_insts {
            match &inst {
                Inst::CallExtern { name, args, .. } if is_guard_open(name) => {
                    if let Some(&flag_slot) = args.first()
                        .and_then(|v| val_to_slot.get(v))
                        .and_then(|s| slot_to_flag.get(s))
                    {
                        // Load the bool flag and emit a flag-based guard open.
                        let v_flag = func.next_value();
                        new_insts.push(Inst::SlotLoad {
                            dst: v_flag,
                            slot: flag_slot,
                            ty: super::LirType::Bool,
                        });
                        new_insts.push(Inst::CallExtern {
                            dst: None,
                            name: "__gorget_drop_flag_open".to_string(),
                            args: vec![v_flag],
                            original_name: None,
                            arg_abis: vec![],
                        });
                        flag_depth += 1;
                        replaced += 1;
                    } else {
                        // Unknown slot — keep the original guard.
                        passthrough_depth += 1;
                        new_insts.push(inst);
                    }
                }
                Inst::CallExtern { name, .. } if is_guard_close(name) => {
                    if flag_depth > 0 {
                        // Matches a replaced open — emit flag close.
                        flag_depth -= 1;
                        new_insts.push(Inst::CallExtern {
                            dst: None,
                            name: "__gorget_drop_flag_close".to_string(),
                            args: vec![],
                            original_name: None,
                            arg_abis: vec![],
                        });
                    } else if passthrough_depth > 0 {
                        // Matches a passthrough open — keep original close.
                        passthrough_depth -= 1;
                        new_insts.push(inst);
                    } else {
                        // Orphan close — keep as-is (shouldn't happen).
                        new_insts.push(inst);
                    }
                }
                _ => new_insts.push(inst),
            }
        }
        func.blocks[bi].insts = new_insts;
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
        // remaining memcmp-based guards with cheap bool checks.
        stats.flags_inserted +=
            insert_drop_flags(func, &val_to_slot, &maybe_init_slots);

        // Phase 4: remove consumed MoveSlot annotations.  They served as dataflow
        // signals for phases 1-3 and have no runtime effect.
        for block in &mut func.blocks {
            let before = block.insts.len();
            block.insts.retain(|inst| !matches!(inst, Inst::MoveSlot { .. }));
            stats.move_slots_removed += before - block.insts.len();
        }
    }
    stats
}
