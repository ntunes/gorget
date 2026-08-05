//! LIR invariant checking.
//!
//! Validates structural properties of LIR modules:
//! - Every value use is dominated by its definition
//! - Block parameter counts match jump argument counts
//! - Block IDs are sequential and in-range
//! - No duplicate value definitions
//! - Terminators reference valid blocks

use super::*;
use std::collections::{HashMap, HashSet};

/// Validation error with context.
#[derive(Debug, Clone)]
pub struct LirError {
    pub func: String,
    pub block: Option<BlockId>,
    pub message: String,
}

impl std::fmt::Display for LirError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "in @{}", self.func)?;
        if let Some(bb) = self.block {
            write!(f, " {bb}")?;
        }
        write!(f, ": {}", self.message)
    }
}

/// Validate a full LIR module. Returns a list of errors (empty = valid).
///
/// Checks structural invariants plus CFG / SSA-form invariants (no critical
/// edges, reducible CFG, every value use dominated by its definition).
/// The SSA invariants are required post-`construct_ssa`; the critical-edge
/// invariant is also required pre-SSA so `construct_ssa` (Braun et al.)
/// and a future WASM backend (structured CFG) both have what they need.
/// See `docs/devbook/25-structural-guards.md` (the CFG/SSA tier).
pub fn validate_module(module: &LirModule) -> Vec<LirError> {
    let mut errors = Vec::new();

    // Validate struct references
    for func in &module.functions {
        validate_function(func, module, &mut errors);
    }

    // Tier E §8.2 — CFG + SSA invariants. Run after the structural checks
    // so simpler errors surface first when both fire on the same module.
    for func in &module.functions {
        check_no_critical_edges(func, &mut errors);
        check_reducible_cfg(func, &mut errors);
        errors.extend(validate_ssa_dominance(func));
    }

    errors
}

/// Type alias for a validator: takes a borrowed module, returns a list of
/// errors. Empty list ⇒ this validator is satisfied.
pub type ValidatorFn = fn(&LirModule) -> Vec<LirError>;

/// Registry of per-pass validators. Each entry runs after every LIR pass when
/// `assert_module_valid` is active (debug builds or `GG_VALIDATE_PASSES` set).
///
/// Adding a new shape invariant: append a `fn(&LirModule) -> Vec<LirError>`
/// here. Phase C's `validate_resource_moves` (`docs/devbook/13-ownership-in-ir.md`)
/// plugs in via this exact registry — same shape, same per-pass invocation.
///
/// Tier 1d (`validate_box_inner_type`): every regular Box StructDef must carry
/// typed inner-type metadata for the per-type drop-wrapper / box-helper emitter
/// (snag #13's family — see commit `c7a652f0`). Cheap (~one pass over
/// `module.structs`); per-pass invocation locks the contract under the
/// invariant framework.
///
/// Tier 1a (`validate_drop_completeness`): every droppable field of a type with
/// a registered `type_drop_fns` entry must appear in that entry's `field_drops`
/// (or `enum_variants` for enums). Catches the snag #24 class — the validator
/// would have rejected the original Option/Result-skip lines that silently left
/// resource-payload Option/Result fields un-dropped at scope exit. See
/// `docs/devbook/25-structural-guards.md` §1a.
///
/// Tier 1a inverse (`validate_drop_fn_presence`): every StructDef whose
/// `expects_drop_fn` flag is set (mirroring GIR's `DropStrategy::Recursive |
/// Custom(_)`) must have a corresponding `type_drop_fns` entry. Catches the
/// populator-skip class — a Recursive struct whose field walk emerged empty
/// because every field type fell into a `_ => continue` arm (non-Named,
/// non-FnPtr GIR types like fixed-size `Array`, bare `Generic`, etc.) and
/// thus produced no drop fn at all — silent leak. See
/// `docs/devbook/25-structural-guards.md` §1a.
///
/// Tier 1d inverse (`validate_box_inner_type_consistency`): every StructDef
/// carrying `box_inner_type: Some(_)` metadata must have a `Box__` name
/// prefix. Catches stray Box metadata on a non-Box struct — e.g., a future
/// copy-bug in the `c_runtime_alias` clone path or any other registrar
/// that accidentally propagates the `box_inner_type` field where it
/// doesn't belong. The forward `validate_box_inner_type` only walks
/// `Box__*`-named structs, so a non-Box with stray metadata slips through.
const VALIDATORS: &[ValidatorFn] = &[validate_module, validate_box_inner_type, validate_box_inner_type_consistency, validate_drop_completeness, validate_drop_fn_presence, validate_resource_arity];

/// Assert that `module` satisfies every registered LIR invariant. Panics with
/// a descriptive message tagged by `after` (the name of the pass that just
/// completed) if any validator reports an error.
///
/// Cost model: in debug builds this runs unconditionally — cheap relative to
/// debug compilation overall. In release builds it is a no-op unless the
/// `GG_VALIDATE_PASSES` environment variable is set, which lets release
/// integration runs opt in to catching shape regressions without paying the
/// cost on the default release path.
///
/// See `docs/devbook/14-lir-ssa.md` (validator after every pass).
#[inline]
pub fn assert_module_valid(module: &LirModule, after: &str) {
    // The fast path is "debug build, no allocation" — but we still want
    // release builds to be opt-in via env var, so the dispatch is a single
    // cfg!() check + a (cached-by-getenv) env-var probe in release.
    if cfg!(debug_assertions) {
        run_validators(module, after);
    } else if std::env::var_os("GG_VALIDATE_PASSES").is_some() {
        run_validators(module, after);
    }
}

#[cold]
#[inline(never)]
fn run_validators(module: &LirModule, after: &str) {
    for validator in VALIDATORS {
        let errors = validator(module);
        if !errors.is_empty() {
            // Render every error so the user sees the full picture, not just
            // the first one. The leading line names the pass; subsequent lines
            // are the LirError Display output ("in @fn bbN: message").
            let mut msg = format!("LIR invariant violated after {after}:");
            for e in &errors {
                msg.push_str("\n  ");
                msg.push_str(&e.to_string());
            }
            panic!("{msg}");
        }
    }
}

fn validate_function(func: &LirFunction, module: &LirModule, errors: &mut Vec<LirError>) {
    let num_blocks = func.blocks.len();
    let num_slots = func.slots.len();

    // Check block IDs are sequential
    for (i, block) in func.blocks.iter().enumerate() {
        if block.id.0 as usize != i {
            errors.push(LirError {
                func: func.name.clone(),
                block: Some(block.id),
                message: format!("block id {} at position {i}", block.id),
            });
        }
    }

    // Collect all defined values and check for duplicates
    let mut defined: HashMap<ValueId, BlockId> = HashMap::new();

    // Block params define values
    for block in &func.blocks {
        for (vid, _) in &block.params {
            if let Some(prev_block) = defined.insert(*vid, block.id) {
                errors.push(LirError {
                    func: func.name.clone(),
                    block: Some(block.id),
                    message: format!(
                        "duplicate definition of {vid} (also defined in {prev_block})"
                    ),
                });
            }
        }

        // Instructions define values
        for inst in &block.insts {
            if let Some(dst) = inst.dst() {
                if let Some(prev_block) = defined.insert(dst, block.id) {
                    errors.push(LirError {
                        func: func.name.clone(),
                        block: Some(block.id),
                        message: format!(
                            "duplicate definition of {dst} (also defined in {prev_block})"
                        ),
                    });
                }
            }
        }
    }

    // Check each block
    for block in &func.blocks {
        // Validate slot references
        for inst in &block.insts {
            check_slot_refs(inst, num_slots, func, block.id, errors);
            check_struct_refs(inst, module, func, block.id, errors);
            check_runtime_call(inst, func, block.id, errors);
            check_call_by_ref(inst, func, &defined, block.id, errors);
        }

        // Validate terminator
        validate_terminator(
            &block.terminator,
            func,
            block.id,
            num_blocks,
            errors,
        );
    }
}

/// Tier E §8.6 sanity check: `Inst::CallByRef.fref` must reference a defined
/// SSA value (block param or instruction dst). The full SSA-dominance
/// validator (`validate_ssa_dominance`) also catches this, but we surface a
/// CallByRef-specific error here so the message points at the `fref` operand
/// directly rather than a generic "undefined value" terminator note.
fn check_call_by_ref(
    inst: &Inst,
    func: &LirFunction,
    defined: &HashMap<ValueId, BlockId>,
    block: BlockId,
    errors: &mut Vec<LirError>,
) {
    if let Inst::CallByRef { fref, .. } = inst {
        if !defined.contains_key(fref) {
            errors.push(LirError {
                func: func.name.clone(),
                block: Some(block),
                message: format!("CallByRef.fref {fref} references an undefined value"),
            });
        }
    }
}

/// Type-check `Inst::CallRuntime` against the static signature of the called
/// `RuntimeFn` variant. Catches mismatched arg counts at LIR validation time.
///
/// Doesn't enforce per-arg `LirType` matching yet — that requires full
/// `value_types` lookup which the current validator doesn't take. (Plan: B1
/// adds a typed cross-check once the runtime declaration table is canonical.)
fn check_runtime_call(
    inst: &Inst,
    func: &LirFunction,
    block: BlockId,
    errors: &mut Vec<LirError>,
) {
    if let Inst::CallRuntime { callee, args, arg_abis, .. } = inst {
        let sig = callee.signature();
        if args.len() != sig.params.len() {
            errors.push(LirError {
                func: func.name.clone(),
                block: Some(block),
                message: format!(
                    "CallRuntime {:?} expects {} arg(s) per signature, got {}",
                    callee,
                    sig.params.len(),
                    args.len(),
                ),
            });
        }
        if arg_abis.len() != sig.params.len() {
            errors.push(LirError {
                func: func.name.clone(),
                block: Some(block),
                message: format!(
                    "CallRuntime {:?}: arg_abis has {} tag(s) but signature has {} param(s)",
                    callee,
                    arg_abis.len(),
                    sig.params.len(),
                ),
            });
        }
    }
}

fn check_slot_refs(
    inst: &Inst,
    num_slots: usize,
    func: &LirFunction,
    block: BlockId,
    errors: &mut Vec<LirError>,
) {
    let slot = match inst {
        Inst::SlotStore { slot, .. }
        | Inst::ClosurePack { slot, .. }
        | Inst::SlotLoad { slot, .. }
        | Inst::SlotAddr { slot, .. }
        | Inst::MoveSlot { slot } => Some(*slot),
        _ => None,
    };

    if let Some(s) = slot {
        if s.0 as usize >= num_slots {
            errors.push(LirError {
                func: func.name.clone(),
                block: Some(block),
                message: format!("slot {s} out of range (function has {num_slots} slots)"),
            });
        }
    }
}

fn check_struct_refs(
    inst: &Inst,
    module: &LirModule,
    func: &LirFunction,
    block: BlockId,
    errors: &mut Vec<LirError>,
) {
    let struct_id = match inst {
        Inst::FieldPtr { struct_id, field, .. } => {
            // Also check field index
            if let Some(def) = module.structs.get(struct_id.0 as usize) {
                if *field as usize >= def.fields.len() {
                    errors.push(LirError {
                        func: func.name.clone(),
                        block: Some(block),
                        message: format!(
                            "field index {field} out of range for {} (has {} fields)",
                            def.name,
                            def.fields.len()
                        ),
                    });
                }
            }
            Some(*struct_id)
        }
        // Bridge inst's key_struct is resolved by `wire_collection_bridges`;
        // the validator double-checks the index didn't go stale through a
        // later struct-removing pass (DCE, etc.).
        Inst::SetCollectionBridge { key_struct, .. } => Some(*key_struct),
        // TraitCall's trait_obj_struct is resolved at LIR construction time
        // in `try_emit_trait_call`. The method_idx is checked against the
        // matching `*_VTable` struct's field count below.
        Inst::TraitCall { trait_obj_struct, method_idx, .. } => {
            // Locate the paired VTable struct by name and bound-check
            // method_idx against its field count. A stale index would
            // point past the vtable's last method slot.
            if let Some(obj_def) = module.structs.get(trait_obj_struct.0 as usize) {
                if let Some(trait_name) = obj_def.name.strip_suffix("_TraitObj") {
                    let vtable_name = format!("{trait_name}_VTable");
                    if let Some(vtable_def) = module.structs.iter().find(|s| s.name == vtable_name) {
                        if *method_idx as usize >= vtable_def.fields.len() {
                            errors.push(LirError {
                                func: func.name.clone(),
                                block: Some(block),
                                message: format!(
                                    "method index {method_idx} out of range for {vtable_name} \
                                     (has {} fields)",
                                    vtable_def.fields.len()
                                ),
                            });
                        }
                    }
                }
            }
            Some(*trait_obj_struct)
        }
        _ => None,
    };

    if let Some(sid) = struct_id {
        if sid.0 as usize >= module.structs.len() {
            errors.push(LirError {
                func: func.name.clone(),
                block: Some(block),
                message: format!(
                    "{sid} out of range (module has {} structs)",
                    module.structs.len()
                ),
            });
        }
    }
}

fn validate_terminator(
    term: &Term,
    func: &LirFunction,
    block: BlockId,
    num_blocks: usize,
    errors: &mut Vec<LirError>,
) {
    // Check all successor blocks are valid and arg counts match
    let check_target = |target: BlockId, args: &[ValueId], errors: &mut Vec<LirError>| {
        if target.0 as usize >= num_blocks {
            errors.push(LirError {
                func: func.name.clone(),
                block: Some(block),
                message: format!("terminator references invalid {target}"),
            });
        } else {
            let target_params = func.blocks[target.0 as usize].params.len();
            if args.len() != target_params {
                errors.push(LirError {
                    func: func.name.clone(),
                    block: Some(block),
                    message: format!(
                        "jump to {target} provides {} args but block expects {target_params}",
                        args.len()
                    ),
                });
            }
        }
    };

    match term {
        Term::Ret(_) | Term::RetVoid | Term::Unreachable => {}
        Term::Jump(target, args) => check_target(*target, args, errors),
        Term::Branch {
            then_block,
            then_args,
            else_block,
            else_args,
            ..
        } => {
            check_target(*then_block, then_args, errors);
            check_target(*else_block, else_args, errors);
        }
        Term::Switch {
            cases,
            default,
            default_args,
            ..
        } => {
            for (_, target, args) in cases {
                check_target(*target, args, errors);
            }
            check_target(*default, default_args, errors);
        }
    }
}

/// Assert that no critical edges remain in `func`'s CFG.
///
/// A *critical edge* runs from a block with multiple successors to a block
/// with multiple predecessors. The pre-SSA `split_critical_edges` pass
/// eliminates them; this validator catches regressions if a later pass
/// reintroduces one (e.g. a block-merging optimization that fuses non-linear
/// shapes). Both pre- and post-SSA, the property must hold — Braun et al.
/// SSA construction relies on it, and so does any structured-CFG backend.
pub fn check_no_critical_edges(func: &LirFunction, errors: &mut Vec<LirError>) {
    let n = func.blocks.len();
    if n == 0 {
        return;
    }
    let mut pred_count = vec![0u32; n];
    for block in &func.blocks {
        for succ in block.terminator.successors() {
            let i = succ.0 as usize;
            if i < n {
                pred_count[i] += 1;
            }
        }
    }
    for block in &func.blocks {
        let succs = block.terminator.successors();
        if succs.len() <= 1 {
            continue;
        }
        let nsuccs = succs.len();
        for succ in succs {
            let i = succ.0 as usize;
            if i < n && pred_count[i] > 1 {
                errors.push(LirError {
                    func: func.name.clone(),
                    block: Some(block.id),
                    message: format!(
                        "critical edge {} → {} (source has {nsuccs} succs, target has {} preds)",
                        block.id,
                        succ,
                        pred_count[i],
                    ),
                });
            }
        }
    }
}

/// Assert that `func`'s CFG is reducible: every back-edge in any DFS from
/// the entry block targets a node that dominates its source.
///
/// WASM's structured control flow can encode reducible CFGs directly via
/// nested `loop`/`block` constructs; irreducible CFGs require a relooper or
/// node-splitting. We enforce reducibility at the LIR level so the WASM
/// backend (and any future structured-CFG consumer) can assume it.
pub fn check_reducible_cfg(func: &LirFunction, errors: &mut Vec<LirError>) {
    let n = func.blocks.len();
    if n == 0 {
        return;
    }

    // Compute dominators (Cooper-Harvey-Kennedy). Same shape as
    // `validate_ssa_dominance` so the two stay consistent.
    let mut preds: Vec<Vec<usize>> = vec![vec![]; n];
    for block in &func.blocks {
        let idx = block.id.0 as usize;
        for succ in block.terminator.successors() {
            let s = succ.0 as usize;
            if s < n {
                preds[s].push(idx);
            }
        }
    }
    let rpo = compute_rpo(n, &func.blocks);
    let mut rpo_order: Vec<usize> = vec![usize::MAX; n];
    for (pos, &block_idx) in rpo.iter().enumerate() {
        rpo_order[block_idx] = pos;
    }
    let mut idom: Vec<usize> = vec![usize::MAX; n];
    idom[rpo[0]] = rpo[0];
    let intersect = |mut a: usize, mut b: usize, idom: &[usize], rpo_order: &[usize]| -> usize {
        while a != b {
            while rpo_order[a] > rpo_order[b] {
                a = idom[a];
            }
            while rpo_order[b] > rpo_order[a] {
                b = idom[b];
            }
        }
        a
    };
    let mut changed = true;
    while changed {
        changed = false;
        for &b in &rpo[1..] {
            let mut new_idom = usize::MAX;
            for &p in &preds[b] {
                if idom[p] != usize::MAX {
                    new_idom = if new_idom == usize::MAX {
                        p
                    } else {
                        intersect(new_idom, p, &idom, &rpo_order)
                    };
                }
            }
            if new_idom != usize::MAX && idom[b] != new_idom {
                idom[b] = new_idom;
                changed = true;
            }
        }
    }
    let dominates = |d: usize, b: usize| -> bool {
        let mut cur = b;
        loop {
            if cur == d {
                return true;
            }
            if idom[cur] == cur || idom[cur] == usize::MAX {
                return false;
            }
            cur = idom[cur];
        }
    };

    // Classify edges via a structural DFS. Edge `u → v` is a *back-edge*
    // when `v` is an ancestor of `u` in the DFS tree (still on the stack).
    // Reducible iff every back-edge target dominates its source.
    enum Color {
        White,
        Gray,
        Black,
    }
    let mut color: Vec<Color> = (0..n).map(|_| Color::White).collect();
    let entry = rpo[0];
    let mut stack: Vec<(usize, usize)> = vec![(entry, 0)];
    color[entry] = Color::Gray;

    while let Some((u, i)) = stack.last().copied() {
        let succs = func.blocks[u].terminator.successors();
        if i < succs.len() {
            stack.last_mut().unwrap().1 = i + 1;
            let v = succs[i].0 as usize;
            if v >= n {
                continue;
            }
            match color[v] {
                Color::White => {
                    color[v] = Color::Gray;
                    stack.push((v, 0));
                }
                Color::Gray => {
                    if !dominates(v, u) {
                        errors.push(LirError {
                            func: func.name.clone(),
                            block: Some(BlockId(u as u32)),
                            message: format!(
                                "irreducible CFG: back-edge bb{u} → bb{v} but \
                                 bb{v} does not dominate bb{u}"
                            ),
                        });
                    }
                }
                Color::Black => {
                    // Forward or cross edge — fine.
                }
            }
        } else {
            color[u] = Color::Black;
            stack.pop();
        }
    }
}

/// Validate SSA dominance: every value use is dominated by its definition.
/// Uses Cooper-Harvey-Kennedy iterative dominator algorithm.
pub fn validate_ssa_dominance(func: &LirFunction) -> Vec<LirError> {
    let mut errors = Vec::new();
    let n = func.blocks.len();
    if n == 0 { return errors; }

    // Build predecessor lists
    let mut preds: Vec<Vec<usize>> = vec![vec![]; n];
    for block in &func.blocks {
        let idx = block.id.0 as usize;
        for succ in block.terminator.successors() {
            let s = succ.0 as usize;
            if s < n {
                preds[s].push(idx);
            }
        }
    }

    // Compute RPO (reverse postorder)
    let rpo = compute_rpo(n, &func.blocks);

    // Iterative dominator computation (Cooper-Harvey-Kennedy 2001)
    // idom[i] = immediate dominator of block i (in RPO numbering)
    let mut rpo_order: Vec<usize> = vec![usize::MAX; n]; // block_idx → RPO position
    for (pos, &block_idx) in rpo.iter().enumerate() {
        rpo_order[block_idx] = pos;
    }

    let mut idom: Vec<usize> = vec![usize::MAX; n];
    idom[rpo[0]] = rpo[0]; // entry dominates itself

    let intersect = |mut a: usize, mut b: usize, idom: &[usize], rpo_order: &[usize]| -> usize {
        while a != b {
            while rpo_order[a] > rpo_order[b] { a = idom[a]; }
            while rpo_order[b] > rpo_order[a] { b = idom[b]; }
        }
        a
    };

    let mut changed = true;
    while changed {
        changed = false;
        for &b in &rpo[1..] { // skip entry
            let mut new_idom = usize::MAX;
            for &p in &preds[b] {
                if idom[p] != usize::MAX {
                    new_idom = if new_idom == usize::MAX { p } else { intersect(new_idom, p, &idom, &rpo_order) };
                }
            }
            if new_idom != usize::MAX && idom[b] != new_idom {
                idom[b] = new_idom;
                changed = true;
            }
        }
    }

    // Build dominance set for each block: which blocks dominate block b?
    // A block d dominates b if d is on the path from entry to b via idom chain.
    let dominates = |d: usize, b: usize| -> bool {
        let mut cur = b;
        loop {
            if cur == d { return true; }
            if idom[cur] == cur || idom[cur] == usize::MAX { return false; }
            cur = idom[cur];
        }
    };

    // Collect where each value is defined
    let mut def_block: HashMap<ValueId, usize> = HashMap::new();
    for block in &func.blocks {
        let idx = block.id.0 as usize;
        for (vid, _) in &block.params {
            def_block.insert(*vid, idx);
        }
        for inst in &block.insts {
            if let Some(dst) = inst.dst() {
                def_block.insert(dst, idx);
            }
        }
    }

    // Check: every use must be in a block dominated by the definition block.
    // For uses within the same block as the definition, verify instruction ordering.
    for block in &func.blocks {
        let use_idx = block.id.0 as usize;
        // Track which values have been defined so far in this block (for intra-block ordering)
        let mut defined_before: HashSet<ValueId> = HashSet::new();
        for (vid, _) in &block.params {
            defined_before.insert(*vid);
        }

        for inst in &block.insts {
            for used in inst.uses() {
                match def_block.get(&used) {
                    None => {
                        errors.push(LirError {
                            func: func.name.clone(),
                            block: Some(block.id),
                            message: format!("use of undefined value {used}"),
                        });
                    }
                    Some(&def_b) => {
                        if def_b == use_idx {
                            // Same block: must be defined before this instruction
                            if !defined_before.contains(&used) {
                                errors.push(LirError {
                                    func: func.name.clone(),
                                    block: Some(block.id),
                                    message: format!("use of {used} before its definition in same block"),
                                });
                            }
                        } else if !dominates(def_b, use_idx) {
                            errors.push(LirError {
                                func: func.name.clone(),
                                block: Some(block.id),
                                message: format!("{used} defined in bb{def_b} which does not dominate bb{use_idx}"),
                            });
                        }
                    }
                }
            }
            if let Some(dst) = inst.dst() {
                defined_before.insert(dst);
            }
        }

        // Check terminator uses
        for used in block.terminator.uses() {
            match def_block.get(&used) {
                None => {
                    errors.push(LirError {
                        func: func.name.clone(),
                        block: Some(block.id),
                        message: format!("use of undefined value {used} in terminator"),
                    });
                }
                Some(&def_b) => {
                    if def_b == use_idx {
                        if !defined_before.contains(&used) {
                            errors.push(LirError {
                                func: func.name.clone(),
                                block: Some(block.id),
                                message: format!("use of {used} before its definition in terminator"),
                            });
                        }
                    } else if !dominates(def_b, use_idx) {
                        errors.push(LirError {
                            func: func.name.clone(),
                            block: Some(block.id),
                            message: format!("{used} defined in bb{def_b} which does not dominate bb{use_idx} (terminator)"),
                        });
                    }
                }
            }
        }
    }

    errors
}

/// Tier 1d structural guard: every regular `Box[T]` `StructDef` in the module
/// must carry typed `box_inner_type: Some(<T>)` metadata.
///
/// Recognition step (registrar boundary — name pattern is the legitimate
/// allowlisted use per `docs/devbook/24-layering-discipline.md` / `docs/devbook/25-structural-guards.md` Tier 3a):
/// a "regular Box" is a `StructDef` whose name starts with `Box__` AND has the
/// single-pointer `_0` field shape produced by `lir/lower/mod.rs:790` (the
/// `is_regular_box` registration site). Trait boxes (`Box[dyn Trait]`) share
/// the `Box__` prefix but use the canonical `{data, vtable}` 16-byte layout
/// and intentionally leave `box_inner_type = None`; this validator skips them
/// by shape, not by name.
///
/// Why fatal: the C backend's `emit_box_drop_wrappers` and `emit_runtime_helpers`
/// passes scan `module.structs` for `box_inner_type.is_some()` to emit the
/// per-type `Box__<inner>__drop` wrapper and `__gorget_box_alloc_<inner>` /
/// `__gorget_box_free_<inner>` helpers. A regular Box StructDef registered
/// without populating `box_inner_type` would link-fail at runtime with
/// undefined symbols (snag #13's family — see commit `c7a652f0`). The
/// validator locks in that any future Box registration site (or refactor that
/// touches struct cloning across modules) cannot regress the contract.
///
/// One source of truth: the field is set once at the regular-Box registration
/// site in `src/lir/lower/mod.rs:790-803`; this validator is the read-side
/// invariant check.
pub fn validate_box_inner_type(module: &LirModule) -> Vec<LirError> {
    let mut errors = Vec::new();
    for sd in &module.structs {
        if !sd.name.starts_with("Box__") {
            continue;
        }
        // Trait-box shape: read the typed flag set at registration time.
        // Skip — these legitimately have `box_inner_type: None` because the
        // trait-obj layout doesn't carry a single inner pointee type.
        if sd.is_trait_box {
            continue;
        }
        // Regular-Box shape: single field named `_0`. Other shapes (e.g. an
        // empty placeholder from the deferred Pass-2 path before fields are
        // filled) shouldn't reach this validator — but if they do, flag them
        // too: any `Box__` StructDef without `_0` AND without trait-box layout
        // is a structural inconsistency that the registrar wrote wrong.
        let is_regular_box = sd.fields.len() == 1 && sd.fields[0].0 == "_0";
        if !is_regular_box {
            errors.push(LirError {
                func: String::new(),
                block: None,
                message: format!(
                    "Box StructDef {:?} has unexpected field shape {:?} (expected single `_0` field or trait-obj `[data, vtable]`)",
                    sd.name,
                    sd.fields.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>(),
                ),
            });
            continue;
        }
        // Regular Box: `box_inner_type` MUST be set, AND must equal the suffix
        // of the StructDef name after `Box__`. The latter check guards against
        // a future copy-bug where one Box's metadata is cloned into another
        // Box's StructDef (e.g. the cross-module `c_runtime_alias` clone path
        // in `src/lir/lower/mod.rs:858` if it were ever applied to a Box).
        let suffix = &sd.name["Box__".len()..];
        match &sd.box_inner_type {
            None => errors.push(LirError {
                func: String::new(),
                block: None,
                message: format!(
                    "Box StructDef {:?} has `box_inner_type: None` (regular-Box shape requires `Some({:?})`); the per-type `Box__<inner>__drop` / `__gorget_box_alloc_<inner>` emitter scans this field — missing it produces link-time undefined symbols",
                    sd.name, suffix,
                ),
            }),
            Some(inner) if inner != suffix => errors.push(LirError {
                func: String::new(),
                block: None,
                message: format!(
                    "Box StructDef {:?} has `box_inner_type: Some({:?})` but name suffix is {:?}; the inner-type metadata must match the registered Box mangling",
                    sd.name, inner, suffix,
                ),
            }),
            Some(_) => {} // ok
        }
    }
    errors
}

/// Tier 1d inverse-direction validator. Forward `validate_box_inner_type`
/// walks `Box__`-named structs and asserts `box_inner_type: Some(suffix)`.
/// The inverse: any struct carrying `box_inner_type: Some(_)` metadata
/// MUST have a `Box__` name prefix. Catches stray metadata on a non-Box
/// struct — e.g., a future copy-bug in the `c_runtime_alias` clone path
/// at `src/lir/lower/mod.rs:858` that accidentally propagates the
/// `box_inner_type` field across registrations. No known violations
/// today; the validator future-proofs the registrar.
pub fn validate_box_inner_type_consistency(module: &LirModule) -> Vec<LirError> {
    let mut errors = Vec::new();
    for sd in &module.structs {
        let Some(inner) = sd.box_inner_type.as_ref() else { continue };
        if sd.name.starts_with("Box__") {
            // Forward validator covers Box__-named structs (incl. suffix mismatch).
            continue;
        }
        errors.push(LirError {
            func: String::new(),
            block: None,
            message: format!(
                "StructDef {:?} carries `box_inner_type: Some({:?})` but its name does not start with `Box__` — stray Box metadata on a non-Box struct; the per-type Box emitter scans this field and would produce a `Box__<inner>__drop` / `__gorget_box_alloc_<inner>` for the wrong type",
                sd.name, inner,
            ),
        });
    }
    errors
}

/// Tier 1a — drop completeness. Per `docs/devbook/25-structural-guards.md` §1a:
/// every droppable field of a type with a registered `type_drop_fns` entry must
/// appear in that entry's `field_drops` (or, for enums, in `enum_variants`).
///
/// Without this, a struct field of type `Option[Box[Resource]]` could silently
/// leak the box at scope exit because the populating pass forgot to add the
/// field to the drop table. The skip-line at `lir/lower/mod.rs:599/650` was
/// exactly this class — it silently filtered Option/Result fields out of the
/// table even when their payloads were resource types. The skip was removed in
/// commit `ff8cf782` after Snag #24 + #25b/c/d closed the issues that masked
/// it; this validator locks the rule so future regressions halt the build
/// instead of leaking at runtime.
///
/// **Droppability** (LIR-level, no GIR access required):
/// 1. The field's struct has its own `type_drop_fns` entry (recursive/custom).
/// 2. The field's struct is a runtime resource: GorgetString / GorgetArray /
///    GorgetMap / GorgetSet / GorgetClosure.
/// 3. The field's struct has `c_runtime_alias` to one of those.
/// 4. The field's struct is a `Box__T` (always heap-allocated).
///
/// Primitive LIR types (I64/Bool/Ptr/...) are non-droppable.
pub fn validate_drop_completeness(module: &LirModule) -> Vec<LirError> {
    let mut errors = Vec::new();

    let is_droppable_type = |ty: &LirType| -> bool {
        let sid = match ty {
            LirType::Struct(sid) => *sid,
            _ => return false,
        };
        let sd = match module.structs.get(sid.0 as usize) {
            Some(s) => s,
            None => return false,
        };
        if module.type_drop_fns.contains_key(&sd.name) {
            return true;
        }
        if let Some(ref alias) = sd.c_runtime_alias {
            if matches!(alias.as_str(),
                "GorgetString" | "GorgetArray" | "GorgetMap" | "GorgetSet" | "GorgetClosure")
            {
                return true;
            }
        }
        if matches!(sd.name.as_str(),
            "GorgetString" | "GorgetArray" | "GorgetMap" | "GorgetSet" | "GorgetClosure")
        {
            return true;
        }
        // Box[T] structs are droppable via __gorget_box_free_<inner>. Read the
        // typed `box_inner_type` (set at every Box-struct LIR registration
        // site) rather than a name-prefix probe. Trait-object boxes are
        // also tagged via `is_trait_box` and are droppable too.
        if sd.box_inner_type.is_some() || sd.is_trait_box {
            return true;
        }
        false
    };

    for (type_name, info) in &module.type_drop_fns {
        let sd = match module.structs.iter().find(|s| s.name == *type_name) {
            Some(s) => s,
            None => continue,
        };

        if let Some(ref enum_variants) = info.enum_variants {
            // Enum: each droppable variant payload (named e.g. `Some_0`,
            // `Ok_0`, `Error_0`) must be in the enum_variants table by
            // field-name match.
            let recorded: HashSet<&str> = enum_variants.iter()
                .map(|(_vi, _vn, fname, _df, _ft)| fname.as_str())
                .collect();
            // Skip field 0 (the `tag`).
            for (fname, fty) in sd.fields.iter().skip(1) {
                if !is_droppable_type(fty) {
                    continue;
                }
                if !recorded.contains(fname.as_str()) {
                    let field_ty_name = match fty {
                        LirType::Struct(sid) => module.structs.get(sid.0 as usize)
                            .map(|s| s.name.as_str()).unwrap_or("?"),
                        _ => "?",
                    };
                    errors.push(LirError {
                        func: String::new(),
                        block: None,
                        message: format!(
                            "Drop completeness: enum {:?} has droppable variant payload {:?} (type {:?}) missing from type_drop_fns enum_variants — scope-exit drop will leak the payload",
                            type_name, fname, field_ty_name,
                        ),
                    });
                }
            }
        } else {
            // Struct: every droppable field by name must appear in field_drops.
            let recorded: HashSet<&str> = info.field_drops.iter()
                .map(|(fname, _fn, _ft)| fname.as_str())
                .collect();
            for (fname, fty) in &sd.fields {
                if !is_droppable_type(fty) {
                    continue;
                }
                if !recorded.contains(fname.as_str()) {
                    let field_ty_name = match fty {
                        LirType::Struct(sid) => module.structs.get(sid.0 as usize)
                            .map(|s| s.name.as_str()).unwrap_or("?"),
                        _ => "?",
                    };
                    errors.push(LirError {
                        func: String::new(),
                        block: None,
                        message: format!(
                            "Drop completeness: struct {:?} has droppable field {:?} (type {:?}) missing from type_drop_fns field_drops — scope-exit drop will leak the field",
                            type_name, fname, field_ty_name,
                        ),
                    });
                }
            }
        }
    }

    errors
}

/// Tier 1a inverse-direction validator. For every StructDef whose
/// `expects_drop_fn` flag is set (writer-side: `populate_type_drop_fns`
/// marks structs/enums with GIR `DropStrategy::Recursive | Custom(_)`),
/// assert that `module.type_drop_fns` contains a matching entry.
///
/// Catches the populator-skip class: a Recursive struct/enum whose field
/// walk emerged empty because every field type fell into a `_ => continue`
/// arm at `src/lir/lower/mod.rs` (covers GIR variants other than `Named` /
/// `FnPtr` — fixed-size `Array`, bare `Generic`, `Tuple` pre-mono, etc.),
/// producing no `type_drop_fns` entry and therefore no drop fn at all —
/// silent leak. Today no known violations exist (Tuple/Generic monomorphise
/// before this pass; fixed-size Array fields aren't carried as TypeDef
/// fields in any current type). The validator future-proofs the populator
/// against new field-type variants and against `Custom` strategies whose
/// dispatch is delegated to a user fn but emit no field_drops.
pub fn validate_drop_fn_presence(module: &LirModule) -> Vec<LirError> {
    let mut errors = Vec::new();
    for sd in &module.structs {
        if !sd.expects_drop_fn {
            continue;
        }
        if module.type_drop_fns.contains_key(&sd.name) {
            continue;
        }
        errors.push(LirError {
            func: String::new(),
            block: None,
            message: format!(
                "Drop fn presence: StructDef {:?} has expects_drop_fn=true (GIR DropStrategy::Recursive | Custom) but no module.type_drop_fns entry — scope-exit drop will be a no-op and any droppable field/payload will leak",
                sd.name,
            ),
        });
    }
    errors
}

/// Item 7e Phase 3/4: every `LirType::Resource { kind, params }` operand
/// must have `params.len() == LirType::expected_resource_arity(kind)`.
///
/// Catches writer-site bugs where a constructor populates `Resource` with
/// the wrong number of params (e.g. forgetting the value type on a Dict,
/// or accidentally adding an extra param to a Vector). The arity invariant
/// is the SSoT for "how many element types does this resource shape carry"
/// — see `src/lir/mod.rs::expected_resource_arity`.
///
/// Walks: function param types, function return types, slot types, block
/// param types, and per-instruction type fields where a `LirType` is
/// declared inline (IConst.ty, FConst.ty, Load.ty, ParamRef.ty, etc.).
/// Value-types inferred by `compute_module_value_types` aren't covered
/// here because they're derived, not authored — a derivation bug is best
/// caught by adding the check to that pass, not by re-walking its output.
pub fn validate_resource_arity(module: &LirModule) -> Vec<LirError> {
    let mut errors = Vec::new();
    let mut check = |func: &str, ty: &LirType, where_: &str| {
        if let LirType::Resource { kind, params } = ty {
            let expected = LirType::expected_resource_arity(*kind);
            if params.len() != expected {
                errors.push(LirError {
                    func: func.to_string(),
                    block: None,
                    message: format!(
                        "Resource arity mismatch at {where_}: kind={kind:?}, expected {expected} params, got {} ({params:?})",
                        params.len(),
                    ),
                });
            }
            for (i, p) in params.iter().enumerate() {
                // Recurse into nested Resource params — e.g. Vector[Box[T]]
                // carries Resource { GorgetArray, [Resource { RefCounted, [...] }] }.
                if let LirType::Resource { .. } = p {
                    let nested_where = format!("{where_} (params[{i}])");
                    let mut nested = Vec::new();
                    let _orig_len = errors.len();
                    // Inline recursion to avoid threading the closure type.
                    let _ = walk_nested(func, p, &nested_where, &mut nested);
                    errors.extend(nested);
                }
            }
        }
    };
    fn walk_nested(func: &str, ty: &LirType, where_: &str, errors: &mut Vec<LirError>) {
        if let LirType::Resource { kind, params } = ty {
            let expected = LirType::expected_resource_arity(*kind);
            if params.len() != expected {
                errors.push(LirError {
                    func: func.to_string(),
                    block: None,
                    message: format!(
                        "Resource arity mismatch at {where_}: kind={kind:?}, expected {expected} params, got {} ({params:?})",
                        params.len(),
                    ),
                });
            }
            for (i, p) in params.iter().enumerate() {
                if let LirType::Resource { .. } = p {
                    walk_nested(func, p, &format!("{where_} (params[{i}])"), errors);
                }
            }
        }
    }

    for func in &module.functions {
        for (i, pty) in func.params.iter().enumerate() {
            check(&func.name, pty, &format!("param {i}"));
        }
        check(&func.name, &func.return_type, "return_type");
        for (sid, slot) in func.slots.iter().enumerate() {
            check(&func.name, &slot.ty, &format!("slot s{sid}"));
        }
        for (bi, block) in func.blocks.iter().enumerate() {
            for (vi, (_, ty)) in block.params.iter().enumerate() {
                check(&func.name, ty, &format!("bb{bi} block-param {vi}"));
            }
        }
    }
    errors
}

/// Compute reverse postorder of blocks via DFS from block 0.
fn compute_rpo(n: usize, blocks: &[Block]) -> Vec<usize> {
    let mut visited = vec![false; n];
    let mut postorder = Vec::with_capacity(n);

    fn dfs(b: usize, blocks: &[Block], visited: &mut Vec<bool>, postorder: &mut Vec<usize>) {
        if b >= blocks.len() || visited[b] { return; }
        visited[b] = true;
        for succ in blocks[b].terminator.successors() {
            dfs(succ.0 as usize, blocks, visited, postorder);
        }
        postorder.push(b);
    }

    dfs(0, blocks, &mut visited, &mut postorder);
    postorder.reverse();
    postorder
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_valid_module() -> LirModule {
        let mut module = LirModule::new();
        module.add_struct(StructDef {
            name: "Point".into(),
            fields: vec![("x".into(), LirType::F64), ("y".into(), LirType::F64)],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
                      });

        let mut func = LirFunction::new("main".into(), vec![], LirType::I32);
        let bb = func.add_block();
        let v0 = func.next_value();
        func.block_mut(bb).insts.push(Inst::IConst {
            dst: v0,
            ty: LirType::I32,
            value: 0,
        });
        func.block_mut(bb).terminator = Term::Ret(v0);
        module.add_function(func);
        module
    }

    #[test]
    fn valid_module_passes() {
        let module = make_valid_module();
        let errors = validate_module(&module);
        assert!(errors.is_empty(), "errors: {errors:?}");
    }

    #[test]
    fn detects_invalid_block_target() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("bad".into(), vec![], LirType::Void);
        let bb = func.add_block();
        func.block_mut(bb).terminator = Term::Jump(BlockId(99), vec![]);
        module.add_function(func);

        let errors = validate_module(&module);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("invalid bb99"));
    }

    #[test]
    fn detects_param_count_mismatch() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("bad".into(), vec![], LirType::Void);

        let bb0 = func.add_block();
        let bb1 = func.add_block();

        // bb1 expects one param but jump provides none
        let v_param = func.next_value();
        func.block_mut(bb1).params.push((v_param, LirType::I64));
        func.block_mut(bb1).terminator = Term::Ret(v_param);

        func.block_mut(bb0).terminator = Term::Jump(bb1, vec![]); // 0 args, expects 1

        module.add_function(func);

        let errors = validate_module(&module);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("provides 0 args but block expects 1"));
    }

    #[test]
    fn detects_duplicate_value_def() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("bad".into(), vec![], LirType::I32);
        let bb = func.add_block();

        // Define v0 twice
        func.block_mut(bb).insts.push(Inst::IConst {
            dst: ValueId(0),
            ty: LirType::I32,
            value: 1,
        });
        func.block_mut(bb).insts.push(Inst::IConst {
            dst: ValueId(0), // duplicate!
            ty: LirType::I32,
            value: 2,
        });
        func.block_mut(bb).terminator = Term::Ret(ValueId(0));
        // Manually set next_value to avoid assertion
        module.add_function(func);

        let errors = validate_module(&module);
        assert!(errors.iter().any(|e| e.message.contains("duplicate definition")));
    }

    #[test]
    fn detects_slot_out_of_range() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("bad".into(), vec![], LirType::Void);
        let bb = func.add_block();

        func.block_mut(bb).insts.push(Inst::SlotStore {
            slot: SlotId(5), // no slots exist
            value: ValueId(0),
            is_move: false,
        });
        func.block_mut(bb).terminator = Term::RetVoid;
        module.add_function(func);

        let errors = validate_module(&module);
        assert!(errors.iter().any(|e| e.message.contains("slot s5 out of range")));
    }

    #[test]
    fn detects_struct_out_of_range() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("bad".into(), vec![], LirType::Void);
        let bb = func.add_block();

        func.block_mut(bb).insts.push(Inst::FieldPtr {
            dst: ValueId(0),
            base: ValueId(1),
            struct_id: StructId(99), // doesn't exist
            field: 0,
        });
        func.block_mut(bb).terminator = Term::RetVoid;
        module.add_function(func);

        let errors = validate_module(&module);
        assert!(errors.iter().any(|e| e.message.contains("struct.99 out of range")));
    }

    #[test]
    fn detects_field_out_of_range() {
        let mut module = LirModule::new();
        let sid = module.add_struct(StructDef {
            name: "Tiny".into(),
            fields: vec![("x".into(), LirType::I32)],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
                      });

        let mut func = LirFunction::new("bad".into(), vec![], LirType::Void);
        let bb = func.add_block();

        func.block_mut(bb).insts.push(Inst::FieldPtr {
            dst: ValueId(0),
            base: ValueId(1),
            struct_id: sid,
            field: 5, // only 1 field
        });
        func.block_mut(bb).terminator = Term::RetVoid;
        module.add_function(func);

        let errors = validate_module(&module);
        assert!(errors.iter().any(|e| e.message.contains("field index 5 out of range")));
    }

    #[test]
    fn ssa_dominance_detects_undefined() {
        let mut func = LirFunction::new("bad".into(), vec![], LirType::I64);
        let bb = func.add_block();

        // Use v99 which is never defined
        func.block_mut(bb).insts.push(Inst::Neg {
            dst: ValueId(0),
            ty: LirType::I64,
            operand: ValueId(99),
        });
        func.block_mut(bb).terminator = Term::Ret(ValueId(0));

        let errors = validate_ssa_dominance(&func);
        assert!(errors.iter().any(|e| e.message.contains("undefined value v99")));
    }

    #[test]
    fn ssa_dominance_passes_valid() {
        let mut func = LirFunction::new("ok".into(), vec![], LirType::I64);
        let bb = func.add_block();

        let v0 = func.next_value();
        let v1 = func.next_value();
        func.block_mut(bb).insts.push(Inst::IConst {
            dst: v0,
            ty: LirType::I64,
            value: 42,
        });
        func.block_mut(bb).insts.push(Inst::Neg {
            dst: v1,
            ty: LirType::I64,
            operand: v0,
        });
        func.block_mut(bb).terminator = Term::Ret(v1);

        let errors = validate_ssa_dominance(&func);
        assert!(errors.is_empty(), "errors: {errors:?}");
    }

    #[test]
    fn ssa_dominance_detects_use_before_def_same_block() {
        let mut func = LirFunction::new("bad".into(), vec![], LirType::I64);
        let bb = func.add_block();

        let v0 = func.next_value();
        let v1 = func.next_value();
        // Use v1 before it's defined
        func.block_mut(bb).insts.push(Inst::Neg {
            dst: v0,
            ty: LirType::I64,
            operand: v1,
        });
        func.block_mut(bb).insts.push(Inst::IConst {
            dst: v1,
            ty: LirType::I64,
            value: 42,
        });
        func.block_mut(bb).terminator = Term::Ret(v0);

        let errors = validate_ssa_dominance(&func);
        assert!(errors.iter().any(|e| e.message.contains("before its definition")),
            "expected use-before-def error, got: {errors:?}");
    }

    #[test]
    fn assert_module_valid_passes_for_valid_module() {
        // Should not panic on a structurally valid module.
        let module = make_valid_module();
        assert_module_valid(&module, "test_pass");
    }

    #[test]
    #[cfg_attr(
        not(debug_assertions),
        ignore = "assert_module_valid is a no-op in release without GG_VALIDATE_PASSES"
    )]
    #[should_panic(expected = "LIR invariant violated after test_pass")]
    fn assert_module_valid_panics_with_pass_name() {
        // Construct a module with an invalid jump target so the structural
        // validator reports an error and `assert_module_valid` panics.
        let mut module = LirModule::new();
        let mut func = LirFunction::new("bad".into(), vec![], LirType::Void);
        let bb = func.add_block();
        func.block_mut(bb).terminator = Term::Jump(BlockId(99), vec![]);
        module.add_function(func);
        assert_module_valid(&module, "test_pass");
    }

    #[test]
    #[cfg_attr(
        not(debug_assertions),
        ignore = "assert_module_valid is a no-op in release without GG_VALIDATE_PASSES"
    )]
    #[should_panic(expected = "invalid bb99")]
    fn assert_module_valid_includes_validator_error() {
        // Verify the validator's per-error text reaches the panic message.
        let mut module = LirModule::new();
        let mut func = LirFunction::new("bad".into(), vec![], LirType::Void);
        let bb = func.add_block();
        func.block_mut(bb).terminator = Term::Jump(BlockId(99), vec![]);
        module.add_function(func);
        assert_module_valid(&module, "test_pass");
    }

    #[test]
    fn ssa_dominance_passes_diamond_cfg() {
        // bb0 → bb1, bb2 → bb3 (diamond)
        let mut func = LirFunction::new("diamond".into(), vec![], LirType::I64);
        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();
        let bb3 = func.add_block();

        let v_cond = func.next_value();
        let v_one = func.next_value();
        let v_two = func.next_value();
        let v_result = func.next_value();

        // bb0: define cond, branch
        func.block_mut(bb0).insts.push(Inst::BoolConst { dst: v_cond, value: true });
        func.block_mut(bb0).terminator = Term::Branch {
            cond: v_cond,
            then_block: bb1, then_args: vec![],
            else_block: bb2, else_args: vec![],
        };

        // bb1: define v_one, jump to bb3
        func.block_mut(bb1).insts.push(Inst::IConst { dst: v_one, ty: LirType::I64, value: 1 });
        func.block_mut(bb1).terminator = Term::Jump(bb3, vec![v_one]);

        // bb2: define v_two, jump to bb3
        func.block_mut(bb2).insts.push(Inst::IConst { dst: v_two, ty: LirType::I64, value: 2 });
        func.block_mut(bb2).terminator = Term::Jump(bb3, vec![v_two]);

        // bb3: receive via block param, return
        func.block_mut(bb3).params.push((v_result, LirType::I64));
        func.block_mut(bb3).terminator = Term::Ret(v_result);

        let errors = validate_ssa_dominance(&func);
        assert!(errors.is_empty(), "errors: {errors:?}");
    }

    // ── Tier E §8.2 — critical edges, reducibility, dominance ──────────────

    /// CFG with one critical edge (bb0 →then→ bb2): bb0 has 2 succs,
    /// bb2 has 2 preds via the bb1 fall-through path.
    fn make_critical_edge_cfg() -> LirFunction {
        let mut func = LirFunction::new("ce".into(), vec![], LirType::Void);
        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();

        let v = func.next_value();
        func.block_mut(bb0).insts.push(Inst::BoolConst { dst: v, value: true });
        func.block_mut(bb0).terminator = Term::Branch {
            cond: v,
            then_block: bb2, then_args: vec![],
            else_block: bb1, else_args: vec![],
        };
        func.block_mut(bb1).terminator = Term::Jump(bb2, vec![]);
        func.block_mut(bb2).terminator = Term::RetVoid;
        func
    }

    #[test]
    fn critical_edge_detected_and_then_split() {
        let mut func = make_critical_edge_cfg();
        let mut errs = Vec::new();
        check_no_critical_edges(&func, &mut errs);
        assert_eq!(errs.len(), 1, "exactly one critical edge expected, got {errs:?}");
        assert!(errs[0].message.contains("critical edge"));

        crate::lir::split_edges::split_critical_edges(&mut func);
        let mut errs = Vec::new();
        check_no_critical_edges(&func, &mut errs);
        assert!(errs.is_empty(), "post-split CFG should have no critical edges: {errs:?}");
    }

    #[test]
    fn clean_diamond_has_no_critical_edges() {
        let mut func = LirFunction::new("clean".into(), vec![], LirType::Void);
        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();
        let bb3 = func.add_block();
        let v = func.next_value();
        func.block_mut(bb0).insts.push(Inst::BoolConst { dst: v, value: true });
        func.block_mut(bb0).terminator = Term::Branch {
            cond: v,
            then_block: bb1, then_args: vec![],
            else_block: bb2, else_args: vec![],
        };
        func.block_mut(bb1).terminator = Term::Jump(bb3, vec![]);
        func.block_mut(bb2).terminator = Term::Jump(bb3, vec![]);
        func.block_mut(bb3).terminator = Term::RetVoid;

        let mut errs = Vec::new();
        check_no_critical_edges(&func, &mut errs);
        assert!(errs.is_empty(), "diamond is critical-edge-free: {errs:?}");
    }

    #[test]
    fn reducible_loop_passes() {
        // bb0 → bb1 (loop header) ↔ bb1 (self-loop), bb1 → bb2 (exit).
        let mut func = LirFunction::new("loop".into(), vec![], LirType::Void);
        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();
        let v = func.next_value();
        func.block_mut(bb0).terminator = Term::Jump(bb1, vec![]);
        func.block_mut(bb1).insts.push(Inst::BoolConst { dst: v, value: true });
        func.block_mut(bb1).terminator = Term::Branch {
            cond: v,
            then_block: bb1, then_args: vec![],
            else_block: bb2, else_args: vec![],
        };
        func.block_mut(bb2).terminator = Term::RetVoid;

        let mut errs = Vec::new();
        check_reducible_cfg(&func, &mut errs);
        assert!(errs.is_empty(), "self-loop is reducible: {errs:?}");
    }

    #[test]
    fn irreducible_cfg_detected() {
        // Classic irreducible: two entry points (bb1 and bb2) into a cycle
        // bb1 ↔ bb2; neither bb1 nor bb2 dominates the other.
        let mut func = LirFunction::new("irreducible".into(), vec![], LirType::Void);
        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();
        let bb3 = func.add_block(); // exit
        let v = func.next_value();
        func.block_mut(bb0).insts.push(Inst::BoolConst { dst: v, value: true });
        func.block_mut(bb0).terminator = Term::Branch {
            cond: v,
            then_block: bb1, then_args: vec![],
            else_block: bb2, else_args: vec![],
        };
        let v1 = func.next_value();
        func.block_mut(bb1).insts.push(Inst::BoolConst { dst: v1, value: true });
        func.block_mut(bb1).terminator = Term::Branch {
            cond: v1,
            then_block: bb2, then_args: vec![],
            else_block: bb3, else_args: vec![],
        };
        let v2 = func.next_value();
        func.block_mut(bb2).insts.push(Inst::BoolConst { dst: v2, value: true });
        func.block_mut(bb2).terminator = Term::Branch {
            cond: v2,
            then_block: bb1, then_args: vec![],
            else_block: bb3, else_args: vec![],
        };
        func.block_mut(bb3).terminator = Term::RetVoid;

        let mut errs = Vec::new();
        check_reducible_cfg(&func, &mut errs);
        assert!(
            errs.iter().any(|e| e.message.contains("irreducible CFG")),
            "expected irreducibility error, got: {errs:?}"
        );
    }

    #[test]
    fn dominance_violation_detected_by_validate_module() {
        // v_one defined in bb1 (then-branch) and used in bb3, but bb3 is
        // also reachable from bb2 — bb1 does NOT dominate bb3, so the use
        // is illegal in SSA.
        let mut module = LirModule::new();
        let mut func = LirFunction::new("baddom".into(), vec![], LirType::I64);
        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();
        let bb3 = func.add_block();

        let v_cond = func.next_value();
        let v_one = func.next_value();
        func.block_mut(bb0).insts.push(Inst::BoolConst { dst: v_cond, value: true });
        func.block_mut(bb0).terminator = Term::Branch {
            cond: v_cond,
            then_block: bb1, then_args: vec![],
            else_block: bb2, else_args: vec![],
        };
        func.block_mut(bb1).insts.push(Inst::IConst { dst: v_one, ty: LirType::I64, value: 1 });
        func.block_mut(bb1).terminator = Term::Jump(bb3, vec![]);
        func.block_mut(bb2).terminator = Term::Jump(bb3, vec![]);
        func.block_mut(bb3).terminator = Term::Ret(v_one);
        module.add_function(func);

        let errors = validate_module(&module);
        assert!(
            errors.iter().any(|e| e.message.contains("does not dominate")),
            "expected dominance violation, got: {errors:?}"
        );
    }

    #[test]
    fn validate_module_accepts_split_critical_edges() {
        let mut module = LirModule::new();
        let mut func = make_critical_edge_cfg();
        crate::lir::split_edges::split_critical_edges(&mut func);
        module.add_function(func);
        let errors = validate_module(&module);
        assert!(errors.is_empty(), "post-split CFG should validate cleanly: {errors:?}");
    }

    // ── Tier 1d: Box-inner-type completeness ────────────────────────────────

    fn make_box_struct(name: &str, inner: Option<&str>) -> StructDef {
        StructDef {
            name: name.into(),
            fields: vec![("_0".into(), LirType::Ptr)],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None,
            elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None,
            c_runtime_alias: None,
            box_inner_type: inner.map(String::from),
            is_trait_box: false, expects_drop_fn: false,
        }
    }

    fn make_trait_box_struct(name: &str) -> StructDef {
        StructDef {
            name: name.into(),
            fields: vec![("data".into(), LirType::Ptr), ("vtable".into(), LirType::Ptr)],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: Some(16), computed_c_align: Some(8),
            elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None,
            c_runtime_alias: None,
            box_inner_type: None, // trait box: legitimately None
            is_trait_box: true, expects_drop_fn: false,
        }
    }

    #[test]
    fn box_inner_type_validator_accepts_well_formed_box() {
        let mut module = LirModule::new();
        module.add_struct(make_box_struct("Box__int64_t", Some("int64_t")));
        module.add_struct(make_box_struct("Box__SpannedExpr", Some("SpannedExpr")));
        let errors = validate_box_inner_type(&module);
        assert!(errors.is_empty(), "well-formed Boxes should validate: {errors:?}");
    }

    #[test]
    fn box_inner_type_validator_skips_trait_boxes() {
        let mut module = LirModule::new();
        // Trait-box layout: legitimately `box_inner_type: None` because the
        // 16-byte data+vtable layout doesn't carry a single inner pointee.
        module.add_struct(make_trait_box_struct("Box__Iterator"));
        let errors = validate_box_inner_type(&module);
        assert!(errors.is_empty(), "trait box should be skipped: {errors:?}");
    }

    #[test]
    fn box_inner_type_validator_flags_missing_inner() {
        let mut module = LirModule::new();
        // Regular Box shape but `box_inner_type: None` — the bug class.
        module.add_struct(make_box_struct("Box__int64_t", None));
        let errors = validate_box_inner_type(&module);
        assert_eq!(errors.len(), 1);
        assert!(
            errors[0].message.contains("box_inner_type: None")
                && errors[0].message.contains("Box__int64_t"),
            "expected missing-inner-type error, got: {errors:?}",
        );
    }

    #[test]
    fn box_inner_type_validator_flags_mismatched_inner() {
        let mut module = LirModule::new();
        // Name says Box__int64_t but inner says String — copy-paste bug.
        module.add_struct(make_box_struct("Box__int64_t", Some("GorgetString")));
        let errors = validate_box_inner_type(&module);
        assert_eq!(errors.len(), 1);
        assert!(
            errors[0].message.contains("box_inner_type: Some(\"GorgetString\")")
                && errors[0].message.contains("name suffix is \"int64_t\""),
            "expected mismatch error, got: {errors:?}",
        );
    }

    #[test]
    fn box_inner_type_validator_ignores_non_box_structs() {
        let mut module = LirModule::new();
        module.add_struct(StructDef {
            name: "MyStruct".into(),
            fields: vec![("x".into(), LirType::I64)],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None,
            elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None,
            c_runtime_alias: None,
            box_inner_type: None,
            is_trait_box: false, expects_drop_fn: false,
        });
        let errors = validate_box_inner_type(&module);
        assert!(errors.is_empty(), "non-Box struct should be ignored: {errors:?}");
    }

    /// Defense-in-depth for the third branch of `validate_box_inner_type`:
    /// a `Box__`-prefixed StructDef with neither the regular-Box `_0` shape
    /// nor the trait-box `[data, vtable]` shape is a structural inconsistency
    /// — the registrar wrote it wrong. The validator flags this with the
    /// "unexpected field shape" error so the construction-site bug surfaces
    /// at the LIR exit boundary.
    #[test]
    fn box_inner_type_validator_flags_unexpected_field_shape() {
        let mut module = LirModule::new();
        // A `Box__`-prefixed struct with two arbitrary fields — neither the
        // regular-Box single-`_0` nor the trait-box `data`/`vtable` shape.
        // Reaches this validator only when the registrar's shape decision
        // missed both legitimate cases.
        module.add_struct(StructDef {
            name: "Box__Garbage".into(),
            fields: vec![("a".into(), LirType::I64), ("b".into(), LirType::I32)],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None,
            elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None,
            c_runtime_alias: None,
            box_inner_type: None,
            is_trait_box: false, expects_drop_fn: false,
        });
        let errors = validate_box_inner_type(&module);
        assert_eq!(errors.len(), 1);
        assert!(
            errors[0].message.contains("unexpected field shape")
                && errors[0].message.contains("Box__Garbage"),
            "expected unexpected-field-shape error, got: {errors:?}",
        );
    }

    /// Defense-in-depth for the construction-site `box_inner_type` invariant:
    /// the regular-Box registration site at `src/lir/lower/mod.rs:790-803`
    /// is the single source of truth. This integration-style test wires a
    /// well-formed regular-Box StructDef into the otherwise-valid module and
    /// asserts the validator's full `assert_module_valid` machinery accepts
    /// it — locking the contract between the registrar and the validator at
    /// the public API boundary the rest of the LIR pipeline calls into.
    #[test]
    fn box_inner_type_validator_runs_under_assert_module_valid() {
        let mut module = make_valid_module();
        module.add_struct(make_box_struct("Box__SpannedExpr", Some("SpannedExpr")));
        // Should not panic — the validator accepts the well-formed Box
        // alongside the rest of the registered validators.
        assert_module_valid(&module, "test");
    }

    // ── Tier 1a: validate_drop_completeness ──────────────────────────────

    /// Helper: minimal module with a `GorgetString` runtime resource StructDef
    /// (the validator's droppability check recognises this name as a resource).
    fn drop_test_module() -> LirModule {
        let mut module = LirModule::new();
        module.add_struct(StructDef {
            name: "GorgetString".into(),
            fields: vec![],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None,
            elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None,
            box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        });
        module
    }

    #[test]
    fn drop_completeness_accepts_complete_struct() {
        let mut module = drop_test_module();
        let gs_sid = StructId(0);
        module.add_struct(StructDef {
            name: "User".into(),
            fields: vec![
                ("id".into(), LirType::I64),
                ("name".into(), LirType::Struct(gs_sid)),
            ],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None,
            elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None,
            box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        });
        module.type_drop_fns.insert("User".into(), TypeDropInfo {
            drop_fn_name: "User__drop".into(),
            field_drops: vec![("name".into(), "gorget_string_free".into(), "GorgetString".into())],
            user_drop_fn: None,
            enum_variants: None,
        });
        let errors = validate_drop_completeness(&module);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn drop_completeness_detects_missing_struct_field() {
        let mut module = drop_test_module();
        let gs_sid = StructId(0);
        module.add_struct(StructDef {
            name: "Leaky".into(),
            fields: vec![
                ("id".into(), LirType::I64),
                ("name".into(), LirType::Struct(gs_sid)), // droppable, but not registered
            ],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None,
            elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None,
            box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        });
        // Drop info present but missing the `name` field — the violation.
        module.type_drop_fns.insert("Leaky".into(), TypeDropInfo {
            drop_fn_name: "Leaky__drop".into(),
            field_drops: vec![],
            user_drop_fn: None,
            enum_variants: None,
        });
        let errors = validate_drop_completeness(&module);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("Leaky"));
        assert!(errors[0].message.contains("name"));
        assert!(errors[0].message.contains("GorgetString"));
    }

    #[test]
    fn drop_completeness_accepts_complete_enum() {
        let mut module = drop_test_module();
        let gs_sid = StructId(0);
        // Option__GorgetString: { tag, Some_0: GorgetString }
        module.add_struct(StructDef {
            name: "Option__GorgetString".into(),
            fields: vec![
                ("tag".into(), LirType::I32),
                ("Some_0".into(), LirType::Struct(gs_sid)),
            ],
            enum_kind: EnumKind::Option,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None,
            elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None,
            box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        });
        module.type_drop_fns.insert("Option__GorgetString".into(), TypeDropInfo {
            drop_fn_name: "Option__GorgetString__drop".into(),
            field_drops: vec![],
            user_drop_fn: None,
            enum_variants: Some(vec![(0, "Some".into(), "Some_0".into(), "gorget_string_free".into(), "GorgetString".into())]),
        });
        let errors = validate_drop_completeness(&module);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn drop_completeness_detects_missing_enum_variant_payload() {
        let mut module = drop_test_module();
        let gs_sid = StructId(0);
        // Option__GorgetString: { tag, Some_0: GorgetString } — but enum_variants
        // is empty. This is exactly the snag #24 class: the lowering pass forgot
        // to register the droppable variant payload.
        module.add_struct(StructDef {
            name: "Option__GorgetString".into(),
            fields: vec![
                ("tag".into(), LirType::I32),
                ("Some_0".into(), LirType::Struct(gs_sid)),
            ],
            enum_kind: EnumKind::Option,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None,
            elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None,
            box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        });
        module.type_drop_fns.insert("Option__GorgetString".into(), TypeDropInfo {
            drop_fn_name: "Option__GorgetString__drop".into(),
            field_drops: vec![],
            user_drop_fn: None,
            enum_variants: Some(vec![]), // empty — the violation
        });
        let errors = validate_drop_completeness(&module);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("Option__GorgetString"));
        assert!(errors[0].message.contains("Some_0"));
    }

    #[test]
    fn drop_completeness_skips_non_droppable_fields() {
        // Struct with all-trivial fields should not produce errors even if
        // it has a (degenerate) type_drop_fns entry.
        let mut module = drop_test_module();
        module.add_struct(StructDef {
            name: "Trivial".into(),
            fields: vec![
                ("a".into(), LirType::I64),
                ("b".into(), LirType::Bool),
                ("c".into(), LirType::Ptr),
            ],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None,
            elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None,
            box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        });
        module.type_drop_fns.insert("Trivial".into(), TypeDropInfo {
            drop_fn_name: "Trivial__drop".into(),
            field_drops: vec![],
            user_drop_fn: Some("user_trivial_drop".into()),
            enum_variants: None,
        });
        let errors = validate_drop_completeness(&module);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    // ── Tier 1a inverse: validate_drop_fn_presence ──────────────────────

    // ── Tier 1d inverse: validate_box_inner_type_consistency ────────────

    #[test]
    fn box_inner_type_consistency_skips_box_named_struct() {
        // Box__Inner with box_inner_type set: forward validator covers it.
        let mut module = drop_test_module();
        module.add_struct(StructDef {
            name: "Box__Inner".into(),
            fields: vec![("_0".into(), LirType::Ptr)],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None,
            elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None,
            box_inner_type: Some("Inner".into()), is_trait_box: false, expects_drop_fn: false,
        });
        let errors = validate_box_inner_type_consistency(&module);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn box_inner_type_consistency_skips_struct_without_metadata() {
        // Regular struct without box_inner_type — fine.
        let mut module = drop_test_module();
        module.add_struct(StructDef {
            name: "MyStruct".into(),
            fields: vec![("a".into(), LirType::I64)],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None,
            elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None,
            box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        });
        let errors = validate_box_inner_type_consistency(&module);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn box_inner_type_consistency_detects_stray_metadata() {
        // Non-Box struct carrying box_inner_type → violation.
        let mut module = drop_test_module();
        module.add_struct(StructDef {
            name: "RandomStruct".into(),
            fields: vec![("a".into(), LirType::I64)],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None,
            elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None,
            box_inner_type: Some("Inner".into()), is_trait_box: false, expects_drop_fn: false,
        });
        let errors = validate_box_inner_type_consistency(&module);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("RandomStruct"));
        assert!(errors[0].message.contains("stray Box metadata"));
    }

    #[test]
    fn drop_fn_presence_accepts_flagged_struct_with_entry() {
        // A struct flagged expects_drop_fn=true and present in type_drop_fns
        // is the happy path — no violation.
        let mut module = drop_test_module();
        module.add_struct(StructDef {
            name: "RecursiveStruct".into(),
            fields: vec![
                ("s".into(), LirType::Struct(StructId(1))), // GorgetString
            ],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None,
            elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None,
            box_inner_type: None, is_trait_box: false, expects_drop_fn: true,
        });
        module.type_drop_fns.insert("RecursiveStruct".into(), TypeDropInfo {
            drop_fn_name: "RecursiveStruct__drop".into(),
            field_drops: vec![
                ("s".into(), "gorget_string_free".into(), "GorgetString".into()),
            ],
            user_drop_fn: None,
            enum_variants: None,
        });
        let errors = validate_drop_fn_presence(&module);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn drop_fn_presence_detects_missing_entry() {
        // Struct flagged expects_drop_fn=true but missing from type_drop_fns
        // is the violation we want to catch — populator skipped despite
        // GIR strategy demanding a drop fn.
        let mut module = drop_test_module();
        module.add_struct(StructDef {
            name: "LeakyRecursive".into(),
            fields: vec![
                ("s".into(), LirType::Struct(StructId(1))),
            ],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None,
            elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None,
            box_inner_type: None, is_trait_box: false, expects_drop_fn: true,
        });
        // Intentionally do NOT add a type_drop_fns entry.
        let errors = validate_drop_fn_presence(&module);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("LeakyRecursive"), "unexpected message: {}", errors[0].message);
        assert!(errors[0].message.contains("expects_drop_fn=true"));
        assert!(errors[0].message.contains("no module.type_drop_fns entry"));
    }

    #[test]
    fn drop_fn_presence_ignores_unflagged_struct() {
        // Struct with expects_drop_fn=false is unflagged → not checked,
        // even if it happens to have no type_drop_fns entry. Trivial
        // structs (no resource fields) are this case.
        let mut module = drop_test_module();
        module.add_struct(StructDef {
            name: "Trivial".into(),
            fields: vec![
                ("a".into(), LirType::I64),
            ],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None,
            elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None,
            box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        });
        let errors = validate_drop_fn_presence(&module);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }
}
