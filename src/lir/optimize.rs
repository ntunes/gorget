//! LIR optimization passes.
//!
//! All references are explicit in LIR, making optimization straightforward.

use super::*;
use std::collections::{HashMap, HashSet};
use std::time::{Duration, Instant};

/// Statistics from optimization passes.
#[derive(Debug, Default)]
pub struct OptStats {
    pub dead_functions_eliminated: usize,
    pub dead_globals_eliminated: usize,
    pub dead_instructions_eliminated: usize,
    pub dead_blocks_eliminated: usize,
    pub copies_propagated: usize,
    pub constants_folded: usize,
    pub branches_folded: usize,
    pub algebraic_simplified: usize,
    pub cse_eliminated: usize,
    /// Guard instructions eliminated by the drop elaboration pass (V1).
    pub drops_elaborated: usize,
    /// Companion `Memset`-to-zero instructions removed after guard elimination (V2).
    pub memsets_removed: usize,
    /// Guard sequences replaced with bool drop flags (V3).
    pub drop_flags_inserted: usize,
    /// `MoveSlot` annotations consumed and removed (V4).
    pub move_slots_removed: usize,
    /// Per-pass cumulative wall-clock time, summed across all fixpoint iterations
    /// and all functions for per-function passes; one entry per module-level pass.
    /// Surfaced in `gg profile` JSON so we can see which pass dominates the
    /// `lir_optimize` phase without instrumenting individual call sites.
    pub pass_times: HashMap<&'static str, Duration>,
}

impl OptStats {
    #[inline]
    fn time<R>(&mut self, name: &'static str, f: impl FnOnce() -> R) -> R {
        let t = Instant::now();
        let r = f();
        *self.pass_times.entry(name).or_default() += t.elapsed();
        r
    }
}

/// Run all optimization passes on an LIR module.
pub fn optimize_module(module: &mut LirModule) -> OptStats {
    let mut stats = OptStats::default();
    stats.dead_functions_eliminated = stats.time("eliminate_dead_functions", || {
        eliminate_dead_functions(module)
    });
    stats.dead_globals_eliminated = stats.time("eliminate_dead_globals", || {
        eliminate_dead_globals(module)
    });
    // Snapshot per-pass total before the per-function loop so overhead can be
    // computed by subtraction without underflow risk.
    let pre_loop_sum: Duration = stats.pass_times.values().copied().sum();
    let t = Instant::now();
    for func in &mut module.functions {
        optimize_function(func, &mut stats);
    }
    let loop_elapsed = t.elapsed();
    let post_loop_sum: Duration = stats.pass_times.values().copied().sum();
    let inner_pass_time = post_loop_sum.saturating_sub(pre_loop_sum);
    // Overhead = total per-function-loop wall time minus time attributed to
    // inner passes. Captures the for-iteration cost (function dispatch,
    // progress accounting, the fixpoint convergence check itself).
    *stats.pass_times.entry("fixpoint_loop_overhead").or_default() +=
        loop_elapsed.saturating_sub(inner_pass_time);
    // Validate the post-DCE/fold/CSE shape before drop-elaboration so any
    // shape regression introduced by the inner fixed-point loop surfaces
    // attributed to the optimizer rather than to drop_elab. Tier E §8.3.
    stats.time("validate_post_fixpoint", || {
        super::validate::assert_module_valid(module, "optimize-fixpoint");
    });
    // Drop elaboration (V1–V4).
    // Runs after the main optimization loop so DCE has already cleaned up the LIR.
    let elab = stats.time("drop_elaboration", || super::drop_elab::elaborate_drops(module));
    stats.drops_elaborated = elab.guards_eliminated;
    stats.memsets_removed = elab.memsets_removed;
    stats.drop_flags_inserted = elab.flags_inserted;
    stats.move_slots_removed = elab.move_slots_removed;
    stats.time("validate_post_drop_elab", || {
        super::validate::assert_module_valid(module, "drop-elaboration");
    });
    // Follow-up DCE to remove orphaned SlotAddr/IConst values left by deleted guards
    // and Memsets.
    if elab.total() > 0 || elab.flags_inserted > 0 {
        let t = Instant::now();
        for func in &mut module.functions {
            stats.dead_instructions_eliminated += eliminate_dead_code(func);
        }
        *stats.pass_times.entry("post_elab_dce").or_default() += t.elapsed();
    }
    stats
}

/// Run optimization passes on a single function, iterating until fixpoint.
/// Convergence is detected via the per-pass "changes made" counters — a
/// snapshot of `(blocks.len(), inst_count)` alone misses progress made
/// by passes that rewrite values without adding/removing instructions
/// (constant folding, algebraic simplification, copy propagation),
/// which can unblock downstream DCE/CSE on the next iteration.
fn optimize_function(func: &mut LirFunction, stats: &mut OptStats) {
    // Hard cap as a safety net — fixpoint convergence is the primary
    // termination signal. Bumped from a tight 3 (which silently stopped
    // before convergence on some functions) to a generous bound; the
    // per-pass change counters mean we exit as soon as no pass made
    // progress. See `docs/devbook/14-lir-ssa.md` (the fixpoint loop).
    const MAX_ITERS: usize = 32;
    for _ in 0..MAX_ITERS {
        let folded = stats.time("fold_constants", || fold_constants(func));
        let algebraic = stats.time("simplify_algebraic", || simplify_algebraic(func));
        let cse = stats.time("cse", || eliminate_common_subexpressions(func));
        let branches = stats.time("fold_constant_branches", || fold_constant_branches(func));
        let dead_blocks = stats.time("eliminate_dead_blocks", || eliminate_dead_blocks(func));
        let blocks_before_merge = func.blocks.len();
        stats.time("merge_linear_blocks", || merge_linear_blocks(func));
        let merged = blocks_before_merge.saturating_sub(func.blocks.len());
        let dead_insts = stats.time("eliminate_dead_code", || eliminate_dead_code(func));
        let copies = stats.time("propagate_copies", || propagate_copies(func));
        stats.constants_folded += folded;
        stats.algebraic_simplified += algebraic;
        stats.cse_eliminated += cse;
        stats.branches_folded += branches;
        stats.dead_blocks_eliminated += dead_blocks;
        stats.dead_instructions_eliminated += dead_insts;
        stats.copies_propagated += copies;
        let progress = folded + algebraic + cse + branches + dead_blocks + merged + dead_insts + copies;
        if progress == 0 {
            break; // fixpoint — no pass made any change this iteration
        }
    }
}

// ── Dead Function Elimination ───────────────────────────────────────────────

/// Remove functions that are never called or referenced.
/// Returns the number of functions eliminated.
pub fn eliminate_dead_functions(module: &mut LirModule) -> usize {
    if module.functions.is_empty() {
        return 0;
    }

    let live = find_live_functions(module);
    let original = module.functions.len();

    // Build old→new FuncId remapping.
    let mut remap: Vec<Option<FuncId>> = vec![None; original];
    let mut new_idx = 0u32;
    for (old_idx, _) in module.functions.iter().enumerate() {
        if live.contains(&FuncId(old_idx as u32)) {
            remap[old_idx] = Some(FuncId(new_idx));
            new_idx += 1;
        }
    }

    // Remove dead functions.
    let mut i = 0;
    module.functions.retain(|_| {
        let keep = live.contains(&FuncId(i as u32));
        i += 1;
        keep
    });

    // Rewrite FuncId references in surviving functions.
    for func in &mut module.functions {
        for block in &mut func.blocks {
            for inst in &mut block.insts {
                rewrite_func_refs(inst, &remap);
            }
        }
    }

    // Rewrite FuncId references in globals.
    for global in &mut module.globals {
        rewrite_global_func_refs(&mut global.init, &remap);
    }

    original - module.functions.len()
}

fn find_live_functions(module: &LirModule) -> rustc_hash::FxHashSet<FuncId> {
    // FxHashSet over FuncId (u32-newtype) — the transitive-closure walk does
    // an `insert`/`contains` per function reference across every inst of every
    // live function; stdlib SipHash is wasted overhead on integer keys.
    // Same FxHashMap playbook as propagate_copies (commit 469d7942).
    let mut live: rustc_hash::FxHashSet<FuncId> = rustc_hash::FxHashSet::default();
    let mut worklist: Vec<FuncId> = Vec::new();

    // Roots: main, __test*, __suite_*, __Closure_N__call, and spawned functions.
    // __Closure_N__call functions are dispatched indirectly through
    // GorgetClosure.fn_ptr — not visible in the LIR call graph.
    // Spawned functions are called from generated __spawn_run_X helpers
    // (emitted by the C backend), not from LIR Call instructions.
    let spawned_names: HashSet<&str> = module.spawned_fns.iter()
        .map(|sf| sf.fn_name.as_str())
        .chain(module.thread_spawned_fns.iter().map(|tf| tf.fn_name.as_str()))
        .collect();
    // Hot-reload entry points: init, tick, reload are called from the host binary.
    let hot_reload_roots: HashSet<&str> = if module.hot_reload {
        ["init", "tick", "reload"].iter().copied().collect()
    } else {
        HashSet::new()
    };
    for (i, func) in module.functions.iter().enumerate() {
        let fid = FuncId(i as u32);
        if func.name == "main"
            || func.name.starts_with("__test")
            || func.name.starts_with("__bench_")
            || func.name.starts_with("__suite_")
            // Bug B: synthetic static-init fns are referenced ONLY by raw C
            // text in the module-init prologue (`__lir_g<N> = name();`),
            // invisible to the LIR call graph (`collect_global_func_refs` only
            // sees `FuncAddr`/`Struct`, not `Extern{name}`). Seed as a root or
            // DCE prunes them and the prologue call link-fails.
            || func.name.starts_with("__gg_static_init_")
            // Closure call bodies are reached only through function pointers
            // (`ClosurePack` / `FuncAddr` into a `GorgetClosure` slot), which the
            // LIR call graph does not walk, so they are seeded as DCE roots.
            // CARRIER #3, not `name.contains("__call")` — that substring test
            // also rooted every USER method spelled `call*` (`Runner__call`),
            // which is not the same fact. Vtable-dispatched user methods stay
            // live through the `FuncAddr` refs in their trait-object globals;
            // `closure_arg_user_method_named_call_trait_equip.gg` pins that.
            || func.takes_env
            || spawned_names.contains(func.name.as_str())
            || hot_reload_roots.contains(func.name.as_str())
        {
            if live.insert(fid) {
                worklist.push(fid);
            }
        }
    }

    // Functions referenced by type_drop_fns (user Drop trait impls called
    // from generated __gorget_dtor_* wrappers via CallExtern, invisible to DCE).
    for info in module.type_drop_fns.values() {
        if let Some(ref user_fn) = info.user_drop_fn {
            for (i, func) in module.functions.iter().enumerate() {
                if func.name == *user_fn {
                    let fid = FuncId(i as u32);
                    if live.insert(fid) {
                        worklist.push(fid);
                    }
                }
            }
        }
    }

    // Hashable / Equatable impls for user types used as Dict / Set keys.
    // The `__gorget_ktable_hash__T` / `__gorget_ktable_eq__T` bridges
    // emitted in the C backend (see c_lir/mod.rs::emit_hashable_key_bridges)
    // call `T__hash` / `T__eq` directly from the runtime GorgetMap at
    // lookup time — that call isn't visible in the LIR call graph, so
    // regular DCE prunes the methods and the bridges link-fail. Treat
    // them as roots.
    let hashable_key_types = find_hashable_key_types(module);
    for key_type in &hashable_key_types {
        let candidates = [
            format!("{key_type}__hash"),
            format!("{key_type}__eq"),
            format!("Hashable_for_{key_type}__hash"),
            format!("Equatable_for_{key_type}__eq"),
        ];
        for (i, func) in module.functions.iter().enumerate() {
            if candidates.iter().any(|c| c == &func.name) {
                let fid = FuncId(i as u32);
                if live.insert(fid) {
                    worklist.push(fid);
                }
            }
        }
    }

    // Functions referenced by global initializers.
    for global in &module.globals {
        collect_global_func_refs(&global.init, &mut |fid| {
            if live.insert(fid) {
                worklist.push(fid);
            }
        });
    }

    // Build a name→FuncId lookup for resolving CallExtern call targets
    // (CallExtern stores a name string, not a FuncId).
    let name_to_fid: rustc_hash::FxHashMap<&str, FuncId> = module.functions.iter()
        .enumerate()
        .map(|(i, f)| (f.name.as_str(), FuncId(i as u32)))
        .collect();

    // Transitive closure: walk called/referenced functions.
    while let Some(fid) = worklist.pop() {
        let func = &module.functions[fid.0 as usize];
        for block in &func.blocks {
            for inst in &block.insts {
                collect_inst_func_refs(inst, &mut |ref_fid| {
                    if live.insert(ref_fid) {
                        worklist.push(ref_fid);
                    }
                });
                // CallExtern targets are resolved by name at emit time —
                // if a matching function exists in the module, treat it
                // as a live reference. Without this, functions like
                // `FxHasher__write_int` (called by user `__hash` impls
                // via the mangled name, not via LIR FuncId) get DCE'd
                // and the bridges link-fail.
                if let Inst::CallExtern { name, .. } = inst {
                    if let Some(&ref_fid) = name_to_fid.get(name.as_str()) {
                        if live.insert(ref_fid) {
                            worklist.push(ref_fid);
                        }
                    }
                }
                // Same logic for CallRuntime — runtime callees rarely overlap
                // with module function names, but the lookup is cheap and
                // protects against edge cases (e.g. a module that re-exports
                // a runtime-named symbol).
                if let Inst::CallRuntime { callee, .. } = inst {
                    if let Some(&ref_fid) = name_to_fid.get(callee.c_name()) {
                        if live.insert(ref_fid) {
                            worklist.push(ref_fid);
                        }
                    }
                }
            }
        }
    }

    live
}

/// Scan the LIR for Dict / Set / HashMap / HashSet constructor calls and
/// return the set of user-struct key types referenced. The Hashable /
/// Equatable impls on these types are reachability roots even when the
/// Gorget program never calls them directly — the runtime map looks them
/// up by function-pointer from within the hash-bucket machinery. Runtime
/// primitives (int64_t, GorgetString, etc.) are excluded because their
/// hash paths are handled by dedicated runtime helpers.
fn find_hashable_key_types(module: &LirModule) -> HashSet<String> {
    // Iterate `Inst::SetCollectionBridge` insts — `wire_collection_bridges`
    // already filtered to user-keyed Dict / HashMap / Set / HashSet
    // constructors; their `key_struct: StructId` field names the user
    // type whose `__hash` / `__eq` methods must stay live. This is a
    // strict superset of what the previous string-parse path produced
    // (it doesn't miss any user-key shape `wire_collection_bridges`
    // recognized) and is typed end-to-end.
    let mut types: HashSet<String> = HashSet::new();
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::SetCollectionBridge { key_struct, .. } = inst {
                    if let Some(sd) = module.structs.get(key_struct.0 as usize) {
                        types.insert(sd.name.clone());
                    }
                }
            }
        }
    }
    types
}

fn collect_inst_func_refs(inst: &Inst, cb: &mut dyn FnMut(FuncId)) {
    match inst {
        Inst::Call { func, .. } => cb(*func),
        Inst::FuncAddr { func, .. } => cb(*func),
        Inst::ClosurePack { call_func, .. } => cb(*call_func),
        _ => {}
    }
}

fn collect_global_func_refs(init: &LirGlobalInit, cb: &mut dyn FnMut(FuncId)) {
    match init {
        LirGlobalInit::FuncAddr(fid) => cb(*fid),
        LirGlobalInit::Struct { fields, .. } => {
            for f in fields {
                collect_global_func_refs(f, cb);
            }
        }
        _ => {}
    }
}

fn rewrite_func_refs(inst: &mut Inst, remap: &[Option<FuncId>]) {
    match inst {
        Inst::Call { func, .. } => {
            if let Some(new_id) = remap[func.0 as usize] {
                *func = new_id;
            }
        }
        Inst::FuncAddr { func, .. } => {
            if let Some(new_id) = remap[func.0 as usize] {
                *func = new_id;
            }
        }
        Inst::ClosurePack { call_func, .. } => {
            if let Some(new_id) = remap[call_func.0 as usize] {
                *call_func = new_id;
            }
        }
        _ => {}
    }
}

fn rewrite_global_func_refs(init: &mut LirGlobalInit, remap: &[Option<FuncId>]) {
    match init {
        LirGlobalInit::FuncAddr(fid) => {
            if let Some(new_id) = remap[fid.0 as usize] {
                *fid = new_id;
            }
        }
        LirGlobalInit::Struct { fields, .. } => {
            for f in fields {
                rewrite_global_func_refs(f, remap);
            }
        }
        _ => {}
    }
}

// ── Dead Global Elimination ─────────────────────────────────────────────────

/// Remove globals that are never referenced. Returns count eliminated.
pub fn eliminate_dead_globals(module: &mut LirModule) -> usize {
    if module.globals.is_empty() {
        return 0;
    }

    // FxHashSet over GlobalId (u32-newtype). Same playbook as propagate_copies
    // (commit 469d7942): one insert per `Inst::GlobalAddr` across every inst of
    // every function, then `contains` once per global at retain time. Integer
    // keys + hot scan == stdlib SipHash is wasted work.
    let mut referenced: rustc_hash::FxHashSet<GlobalId> = rustc_hash::FxHashSet::default();
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::GlobalAddr { global, .. } = inst {
                    referenced.insert(*global);
                }
            }
        }
    }

    // Keep globals that contain function addresses (e.g., vtable constants).
    // These may be referenced indirectly (e.g., through pointer casts in the C backend)
    // rather than through explicit GlobalAddr instructions.
    for (i, global) in module.globals.iter().enumerate() {
        if global_has_func_addrs(&global.init) {
            referenced.insert(GlobalId(i as u32));
        }
    }

    let original = module.globals.len();

    // Build old→new index remapping table before removing anything.
    let mut old_to_new: Vec<Option<u32>> = Vec::with_capacity(original);
    let mut new_idx = 0u32;
    for old_idx in 0..original {
        if referenced.contains(&GlobalId(old_idx as u32)) {
            old_to_new.push(Some(new_idx));
            new_idx += 1;
        } else {
            old_to_new.push(None);
        }
    }

    let mut i = 0;
    module.globals.retain(|_| {
        let keep = referenced.contains(&GlobalId(i as u32));
        i += 1;
        keep
    });

    let eliminated = original - module.globals.len();
    if eliminated > 0 {
        // Remap GlobalAddr instructions in all functions to account for removed globals.
        for func in &mut module.functions {
            for block in &mut func.blocks {
                for inst in &mut block.insts {
                    if let Inst::GlobalAddr { global, .. } = inst {
                        if let Some(new) = old_to_new[global.0 as usize] {
                            *global = GlobalId(new);
                        }
                    }
                }
            }
        }
    }

    eliminated
}

/// Check if a global initializer contains any FuncAddr references.
fn global_has_func_addrs(init: &LirGlobalInit) -> bool {
    match init {
        LirGlobalInit::FuncAddr(_) => true,
        LirGlobalInit::Struct { fields, .. } => fields.iter().any(|f| global_has_func_addrs(f)),
        _ => false,
    }
}

// ── Dead Code Elimination ───────────────────────────────────────────────────

/// Remove instructions whose results are never used. Returns count eliminated.
pub fn eliminate_dead_code(func: &mut LirFunction) -> usize {
    // Build use counts for each ValueId.
    let val_count = func.value_count() as usize;
    let mut use_count = vec![0u32; val_count];

    for block in &func.blocks {
        for inst in &block.insts {
            for used in inst.uses() {
                if (used.0 as usize) < val_count {
                    use_count[used.0 as usize] += 1;
                }
            }
        }
        for used in block.terminator.uses() {
            if (used.0 as usize) < val_count {
                use_count[used.0 as usize] += 1;
            }
        }
        // Block params used in terminators of other blocks are counted above.
    }

    let mut eliminated = 0;
    for block in &mut func.blocks {
        // Build a keep mask in-place; if no dead inst is found we skip the
        // mutation step entirely (the fixpoint loop fires this pass many
        // times — the no-op case dominates after the first iteration).
        // When deletions are needed, use `Vec::retain` so we drop dead
        // insts in-place rather than allocating a fresh pair of vecs each
        // call.
        let mut dead_in_block = 0usize;
        let keep: Vec<bool> = block
            .insts
            .iter()
            .map(|inst| {
                if has_side_effects(inst) {
                    return true;
                }
                if let Some(dst) = inst.dst() {
                    if (dst.0 as usize) < val_count && use_count[dst.0 as usize] > 0 {
                        return true;
                    }
                    dead_in_block += 1;
                    false
                } else {
                    true
                }
            })
            .collect();
        if dead_in_block == 0 {
            continue; // nothing to remove — leave insts/span_map untouched
        }
        eliminated += dead_in_block;
        // In-place retain on both `insts` and `span_map`, lockstep-indexed
        // via a tracking counter inside each closure. Both walks visit
        // their owning vec in order, so the same `keep[i]` indexes line up
        // 1:1 — no allocation, no `mem::take`.
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
            // Out-of-sync span_map (pre-1b path): reseed to parallel-empty.
            block.span_map = vec![None; block.insts.len()];
        }
    }

    eliminated
}

fn has_side_effects(inst: &Inst) -> bool {
    // Integer Div/Rem/Mod and all shift ops can trap at runtime (divide-by-zero
    // and shift-out-of-range guards emitted by the C backend), so DCE must not
    // eliminate them even when the result is unused — otherwise `int boom = 10
    // / z` silently runs to completion when boom is dead. Float Div/Rem/Mod
    // are defined under IEEE-754 (NaN/Inf for /0) and have no trap, so they
    // remain eliminable.
    match inst {
        Inst::Div { ty, .. } | Inst::Rem { ty, .. } | Inst::Mod { ty, .. } => {
            !matches!(ty, LirType::F32 | LirType::F64)
        }
        Inst::Shl { .. } | Inst::Shr { .. } => true,
        _ => matches!(
            inst,
            Inst::SlotStore { .. }
                | Inst::ClosurePack { .. }
                | Inst::Store { .. }
                | Inst::Memset { .. }
                | Inst::Memcpy { .. }
                | Inst::Call { .. }
                | Inst::CallExtern { .. }
                | Inst::CallPtr { .. }
                | Inst::CallByRef { .. }
                | Inst::CallClosure { .. }
                | Inst::DropGuardOpen { .. }
                | Inst::DropGuardClose
                | Inst::BoundsCheck { .. }
                | Inst::DivCheck { .. }
                | Inst::Trap { .. }
                | Inst::Printf { .. }
                | Inst::Fprintf { .. }
                | Inst::MoveSlot { .. }
                | Inst::SetCollectionBridge { .. }
                | Inst::Nop
        ),
    }
}

// ── Constant Folding ──────────────────────────────────────────────────────

/// Known constant value for a ValueId.
#[derive(Clone, Debug)]
enum KnownConst {
    Int(i64, LirType),
    Float(f64, LirType),
    Bool(bool),
}

/// Evaluate constant integer binary operations at compile time.
/// Returns the count of instructions folded.
pub fn fold_constants(func: &mut LirFunction) -> usize {
    let val_count = func.value_count() as usize;
    let mut known: Vec<Option<KnownConst>> = vec![None; val_count];

    // First pass: collect known constants.
    for block in &func.blocks {
        for inst in &block.insts {
            match inst {
                Inst::IConst { dst, value, ty } => {
                    known[dst.0 as usize] = Some(KnownConst::Int(*value, ty.clone()));
                }
                Inst::FConst { dst, bits, ty } => {
                    known[dst.0 as usize] = Some(KnownConst::Float(f64::from_bits(*bits), ty.clone()));
                }
                Inst::BoolConst { dst, value } => {
                    known[dst.0 as usize] = Some(KnownConst::Bool(*value));
                }
                _ => {}
            }
        }
    }

    // Second pass: fold arithmetic on known constants.
    let mut folded = 0;
    for block in &mut func.blocks {
        for inst in &mut block.insts {
            if let Some(replacement) = try_fold(inst, &known) {
                // Record the new constant for cascading folds.
                if let Some(dst) = replacement.dst() {
                    match &replacement {
                        Inst::IConst { value, ty, .. } => {
                            known[dst.0 as usize] = Some(KnownConst::Int(*value, ty.clone()));
                        }
                        Inst::FConst { bits, ty, .. } => {
                            known[dst.0 as usize] = Some(KnownConst::Float(f64::from_bits(*bits), ty.clone()));
                        }
                        Inst::BoolConst { value, .. } => {
                            known[dst.0 as usize] = Some(KnownConst::Bool(*value));
                        }
                        _ => {}
                    }
                }
                *inst = replacement;
                folded += 1;
            }
        }
    }

    folded
}

/// Try to fold a single instruction. Returns `Some(replacement)` if foldable.
fn try_fold(inst: &Inst, known: &[Option<KnownConst>]) -> Option<Inst> {
    let get_int = |v: ValueId| -> Option<(i64, LirType)> {
        match known.get(v.0 as usize)? {
            Some(KnownConst::Int(val, ty)) => Some((*val, ty.clone())),
            _ => None,
        }
    };
    let get_float = |v: ValueId| -> Option<(f64, LirType)> {
        match known.get(v.0 as usize)? {
            Some(KnownConst::Float(val, ty)) => Some((*val, ty.clone())),
            _ => None,
        }
    };
    let get_bool = |v: ValueId| -> Option<bool> {
        match known.get(v.0 as usize)? {
            Some(KnownConst::Bool(val)) => Some(*val),
            _ => None,
        }
    };

    match inst {
        // Integer arithmetic
        Inst::Add { dst, lhs, rhs, overflow, .. } => {
            if let (Some((a, ty)), Some((b, _))) = (get_int(*lhs), get_int(*rhs)) {
                // Don't fold if overflow would trap at runtime.
                if *overflow == Overflow::Trap && a.checked_add(b).is_none() {
                    return None;
                }
                return Some(Inst::IConst { dst: *dst, value: a.wrapping_add(b), ty });
            }
            if let (Some((a, ty)), Some((b, _))) = (get_float(*lhs), get_float(*rhs)) {
                return Some(Inst::FConst { dst: *dst, bits: (a + b).to_bits(), ty });
            }
            None
        }
        Inst::Sub { dst, lhs, rhs, overflow, .. } => {
            if let (Some((a, ty)), Some((b, _))) = (get_int(*lhs), get_int(*rhs)) {
                if *overflow == Overflow::Trap && a.checked_sub(b).is_none() {
                    return None;
                }
                return Some(Inst::IConst { dst: *dst, value: a.wrapping_sub(b), ty });
            }
            if let (Some((a, ty)), Some((b, _))) = (get_float(*lhs), get_float(*rhs)) {
                return Some(Inst::FConst { dst: *dst, bits: (a - b).to_bits(), ty });
            }
            None
        }
        Inst::Mul { dst, lhs, rhs, overflow, .. } => {
            if let (Some((a, ty)), Some((b, _))) = (get_int(*lhs), get_int(*rhs)) {
                if *overflow == Overflow::Trap && a.checked_mul(b).is_none() {
                    return None;
                }
                return Some(Inst::IConst { dst: *dst, value: a.wrapping_mul(b), ty });
            }
            if let (Some((a, ty)), Some((b, _))) = (get_float(*lhs), get_float(*rhs)) {
                return Some(Inst::FConst { dst: *dst, bits: (a * b).to_bits(), ty });
            }
            None
        }
        Inst::Div { dst, lhs, rhs, .. } => {
            // Use `checked_div` — returns None on both `b == 0` and
            // `INT_MIN / -1` overflow. Don't fold either case; the runtime
            // guard in the C backend will trap with the appropriate message.
            // Pre-fix, `wrapping_div` silently folded INT_MIN/-1 to INT_MIN,
            // bypassing the runtime overflow guard entirely.
            if let (Some((a, ty)), Some((b, _))) = (get_int(*lhs), get_int(*rhs)) {
                if let Some(q) = a.checked_div(b) {
                    return Some(Inst::IConst { dst: *dst, value: q, ty });
                }
            }
            if let (Some((a, ty)), Some((b, _))) = (get_float(*lhs), get_float(*rhs)) {
                return Some(Inst::FConst { dst: *dst, bits: (a / b).to_bits(), ty });
            }
            None
        }
        Inst::Rem { dst, lhs, rhs, .. } => {
            if let (Some((a, ty)), Some((b, _))) = (get_int(*lhs), get_int(*rhs)) {
                // `checked_rem` also handles INT_MIN % -1 (C UB, Rust returns 0
                // via wrapping_rem but checked_rem returns None — good, we trap
                // in the runtime C guard or emit 0 per our Rem lowering).
                if let Some(r) = a.checked_rem(b) {
                    return Some(Inst::IConst { dst: *dst, value: r, ty });
                }
            }
            None
        }
        Inst::Mod { dst, lhs, rhs, .. } => {
            if let (Some((a, ty)), Some((b, _))) = (get_int(*lhs), get_int(*rhs)) {
                if let Some(r) = a.checked_rem(b) {
                    let result = if r != 0 && (r ^ b) < 0 { r + b } else { r };
                    return Some(Inst::IConst { dst: *dst, value: result, ty });
                }
            }
            None
        }
        Inst::Neg { dst, operand, .. } => {
            if let Some((a, ty)) = get_int(*operand) {
                return Some(Inst::IConst { dst: *dst, value: a.wrapping_neg(), ty });
            }
            if let Some((a, ty)) = get_float(*operand) {
                return Some(Inst::FConst { dst: *dst, bits: (-a).to_bits(), ty });
            }
            None
        }

        // Bitwise
        Inst::BitAnd { dst, lhs, rhs, .. } => {
            let (a, ty) = get_int(*lhs)?;
            let (b, _) = get_int(*rhs)?;
            Some(Inst::IConst { dst: *dst, value: a & b, ty })
        }
        Inst::BitOr { dst, lhs, rhs, .. } => {
            let (a, ty) = get_int(*lhs)?;
            let (b, _) = get_int(*rhs)?;
            Some(Inst::IConst { dst: *dst, value: a | b, ty })
        }
        Inst::BitXor { dst, lhs, rhs, .. } => {
            let (a, ty) = get_int(*lhs)?;
            let (b, _) = get_int(*rhs)?;
            Some(Inst::IConst { dst: *dst, value: a ^ b, ty })
        }
        Inst::BitNot { dst, operand, .. } => {
            let (a, ty) = get_int(*operand)?;
            Some(Inst::IConst { dst: *dst, value: !a, ty })
        }
        Inst::Shl { dst, lhs, rhs, .. } => {
            let (a, ty) = get_int(*lhs)?;
            let (b, _) = get_int(*rhs)?;
            if b >= 0 && b < 64 {
                Some(Inst::IConst { dst: *dst, value: a.wrapping_shl(b as u32), ty })
            } else {
                None
            }
        }
        Inst::Shr { dst, lhs, rhs, .. } => {
            let (a, ty) = get_int(*lhs)?;
            let (b, _) = get_int(*rhs)?;
            if b >= 0 && b < 64 {
                Some(Inst::IConst { dst: *dst, value: a.wrapping_shr(b as u32), ty })
            } else {
                None
            }
        }

        // Comparison
        Inst::Cmp { dst, op, lhs, rhs } => {
            if let (Some((a, _)), Some((b, _))) = (get_int(*lhs), get_int(*rhs)) {
                let result = match op {
                    CmpOp::Eq => a == b,
                    CmpOp::Ne => a != b,
                    CmpOp::Lt => a < b,
                    CmpOp::Le => a <= b,
                    CmpOp::Gt => a > b,
                    CmpOp::Ge => a >= b,
                };
                return Some(Inst::BoolConst { dst: *dst, value: result });
            }
            if let (Some((a, _)), Some((b, _))) = (get_float(*lhs), get_float(*rhs)) {
                let result = match op {
                    CmpOp::Eq => a == b,
                    CmpOp::Ne => a != b,
                    CmpOp::Lt => a < b,
                    CmpOp::Le => a <= b,
                    CmpOp::Gt => a > b,
                    CmpOp::Ge => a >= b,
                };
                return Some(Inst::BoolConst { dst: *dst, value: result });
            }
            None
        }
        Inst::Not { dst, operand } => {
            let b = get_bool(*operand)?;
            Some(Inst::BoolConst { dst: *dst, value: !b })
        }

        // Integer casts between known constants
        Inst::IntCast { dst, value, to } => {
            let (a, _) = get_int(*value)?;
            let truncated = match to {
                LirType::I8 => (a as i8) as i64,
                LirType::I16 => (a as i16) as i64,
                LirType::I32 => (a as i32) as i64,
                LirType::I64 => a,
                LirType::U8 => (a as u8) as i64,
                LirType::U16 => (a as u16) as i64,
                LirType::U32 => (a as u32) as i64,
                LirType::U64 => a, // stored as i64 bits
                _ => return None,
            };
            Some(Inst::IConst { dst: *dst, value: truncated, ty: to.clone() })
        }
        Inst::IntToFloat { dst, value, to } => {
            let (a, _) = get_int(*value)?;
            let f = a as f64;
            Some(Inst::FConst { dst: *dst, bits: f.to_bits(), ty: to.clone() })
        }
        Inst::FloatToInt { dst, value, to } => {
            let (a, _) = get_float(*value)?;
            let i = a as i64;
            Some(Inst::IConst { dst: *dst, value: i, ty: to.clone() })
        }

        _ => None,
    }
}

// ── Algebraic Simplification ────────────────────────────────────────────────

/// Simplify arithmetic/logic instructions using algebraic identities:
///   x + 0 → x,  0 + x → x
///   x - 0 → x
///   x * 0 → 0,  x * 1 → x,  1 * x → x
///   x / 1 → x
///   x & 0 → 0,  x | 0 → x
///   x ^ 0 → x
///   x << 0 → x,  x >> 0 → x
///   x * 2 → x + x  (strength reduction)
/// Returns count of instructions simplified.
fn simplify_algebraic(func: &mut LirFunction) -> usize {
    let val_count = func.value_count() as usize;
    let mut known_int: Vec<Option<i64>> = vec![None; val_count];

    // Collect known integer constants.
    for block in &func.blocks {
        for inst in &block.insts {
            if let Inst::IConst { dst, value, .. } = inst {
                if (dst.0 as usize) < val_count {
                    known_int[dst.0 as usize] = Some(*value);
                }
            }
        }
    }

    fn get_int(known: &[Option<i64>], v: ValueId) -> Option<i64> {
        known.get(v.0 as usize).copied().flatten()
    }

    let mut simplified = 0;
    for block in &mut func.blocks {
        for inst in &mut block.insts {
            let replacement = match inst {
                // x + 0 → x, 0 + x → x
                Inst::Add { dst, ty, lhs, rhs, .. } => {
                    if get_int(&known_int, *rhs) == Some(0) {
                        Some(identity(*dst, ty.clone(), *lhs))
                    } else if get_int(&known_int, *lhs) == Some(0) {
                        Some(identity(*dst, ty.clone(), *rhs))
                    } else {
                        None
                    }
                }
                // x - 0 → x
                Inst::Sub { dst, ty, lhs, rhs, .. } => {
                    if get_int(&known_int, *rhs) == Some(0) {
                        Some(identity(*dst, ty.clone(), *lhs))
                    } else {
                        None
                    }
                }
                // x * 0 → 0, x * 1 → x, 1 * x → x, x * 2 → x + x
                Inst::Mul { dst, ty, lhs, rhs, overflow } => {
                    match (get_int(&known_int, *lhs), get_int(&known_int, *rhs)) {
                        (_, Some(0)) | (Some(0), _) => {
                            Some(Inst::IConst { dst: *dst, value: 0, ty: ty.clone() })
                        }
                        (_, Some(1)) => Some(identity(*dst, ty.clone(), *lhs)),
                        (Some(1), _) => Some(identity(*dst, ty.clone(), *rhs)),
                        // Strength reduction: x * 2 → x + x
                        (_, Some(2)) => Some(Inst::Add {
                            dst: *dst, ty: ty.clone(), lhs: *lhs, rhs: *lhs,
                            overflow: *overflow,
                        }),
                        (Some(2), _) => Some(Inst::Add {
                            dst: *dst, ty: ty.clone(), lhs: *rhs, rhs: *rhs,
                            overflow: *overflow,
                        }),
                        _ => None,
                    }
                }
                // x / 1 → x
                Inst::Div { dst, ty, lhs, rhs } => {
                    if get_int(&known_int, *rhs) == Some(1) {
                        Some(identity(*dst, ty.clone(), *lhs))
                    } else {
                        None
                    }
                }
                // x & 0 → 0, x | 0 → x, x ^ 0 → x
                Inst::BitAnd { dst, ty, lhs, rhs } => {
                    if get_int(&known_int, *rhs) == Some(0) || get_int(&known_int, *lhs) == Some(0) {
                        Some(Inst::IConst { dst: *dst, value: 0, ty: ty.clone() })
                    } else {
                        None
                    }
                }
                Inst::BitOr { dst, ty, lhs, rhs } => {
                    if get_int(&known_int, *rhs) == Some(0) {
                        Some(identity(*dst, ty.clone(), *lhs))
                    } else if get_int(&known_int, *lhs) == Some(0) {
                        Some(identity(*dst, ty.clone(), *rhs))
                    } else {
                        None
                    }
                }
                Inst::BitXor { dst, ty, lhs, rhs } => {
                    if get_int(&known_int, *rhs) == Some(0) {
                        Some(identity(*dst, ty.clone(), *lhs))
                    } else if get_int(&known_int, *lhs) == Some(0) {
                        Some(identity(*dst, ty.clone(), *rhs))
                    } else {
                        None
                    }
                }
                // x << 0 → x, x >> 0 → x
                Inst::Shl { dst, ty, lhs, rhs } | Inst::Shr { dst, ty, lhs, rhs } => {
                    if get_int(&known_int, *rhs) == Some(0) {
                        Some(identity(*dst, ty.clone(), *lhs))
                    } else {
                        None
                    }
                }
                _ => None,
            };
            if let Some(new_inst) = replacement {
                // Update known constants for cascading
                if let Inst::IConst { dst, value, .. } = &new_inst {
                    if (dst.0 as usize) < val_count {
                        known_int[dst.0 as usize] = Some(*value);
                    }
                }
                *inst = new_inst;
                simplified += 1;
            }
        }
    }
    simplified
}

/// Create an identity instruction: dst = copy of src value.
/// Uses IntCast as a no-op (same type to same type); DCE will clean up if unused.
fn identity(dst: ValueId, ty: LirType, src: ValueId) -> Inst {
    Inst::IntCast { dst, value: src, to: ty }
}

// ── Common Subexpression Elimination ────────────────────────────────────────

/// Within each basic block, eliminate redundant computations.
/// If `v1 = Add(a, b)` and later `v2 = Add(a, b)` with the same operands,
/// replace v2's definition with a copy of v1. Returns count eliminated.
fn eliminate_common_subexpressions(func: &mut LirFunction) -> usize {
    use rustc_hash::FxHashMap;

    /// Key for CSE: instruction kind + operands (without dst).
    #[derive(Hash, Eq, PartialEq, Clone)]
    enum CseKey {
        BinOp { kind: u8, lhs: ValueId, rhs: ValueId },
        UnOp { kind: u8, operand: ValueId },
        Cmp { kind: u8, lhs: ValueId, rhs: ValueId },
        FieldPtr { base: ValueId, struct_id: StructId, field: u32 },
    }

    let mut eliminated = 0;

    for block in &mut func.blocks {
        // FxHashMap: same playbook as the 2026-05-17 `propagate_copies` swap
        // (commit 469d7942). Stdlib HashMap's SipHash is cryptographic and
        // wasted on compiler-internal integer-keyed maps; FxHashMap is
        // measurably faster on the per-block-per-fixpoint allocation here.
        let mut seen: FxHashMap<CseKey, ValueId> = FxHashMap::default();
        let mut replacements: Vec<(usize, ValueId, ValueId)> = Vec::new(); // (index, old_dst, existing_val)

        for (i, inst) in block.insts.iter().enumerate() {
            let (key, dst) = match inst {
                Inst::Add { dst, lhs, rhs, .. } => (Some(CseKey::BinOp { kind: 0, lhs: *lhs, rhs: *rhs }), Some(*dst)),
                Inst::Sub { dst, lhs, rhs, .. } => (Some(CseKey::BinOp { kind: 1, lhs: *lhs, rhs: *rhs }), Some(*dst)),
                Inst::Mul { dst, lhs, rhs, .. } => (Some(CseKey::BinOp { kind: 2, lhs: *lhs, rhs: *rhs }), Some(*dst)),
                Inst::Div { dst, lhs, rhs, .. } => (Some(CseKey::BinOp { kind: 3, lhs: *lhs, rhs: *rhs }), Some(*dst)),
                Inst::Rem { dst, lhs, rhs, .. } => (Some(CseKey::BinOp { kind: 4, lhs: *lhs, rhs: *rhs }), Some(*dst)),
                Inst::BitAnd { dst, lhs, rhs, .. } => (Some(CseKey::BinOp { kind: 5, lhs: *lhs, rhs: *rhs }), Some(*dst)),
                Inst::BitOr { dst, lhs, rhs, .. } => (Some(CseKey::BinOp { kind: 6, lhs: *lhs, rhs: *rhs }), Some(*dst)),
                Inst::BitXor { dst, lhs, rhs, .. } => (Some(CseKey::BinOp { kind: 7, lhs: *lhs, rhs: *rhs }), Some(*dst)),
                Inst::Shl { dst, lhs, rhs, .. } => (Some(CseKey::BinOp { kind: 8, lhs: *lhs, rhs: *rhs }), Some(*dst)),
                Inst::Shr { dst, lhs, rhs, .. } => (Some(CseKey::BinOp { kind: 9, lhs: *lhs, rhs: *rhs }), Some(*dst)),
                Inst::Neg { dst, operand, .. } => (Some(CseKey::UnOp { kind: 0, operand: *operand }), Some(*dst)),
                Inst::Not { dst, operand, .. } => (Some(CseKey::UnOp { kind: 1, operand: *operand }), Some(*dst)),
                Inst::BitNot { dst, operand, .. } => (Some(CseKey::UnOp { kind: 2, operand: *operand }), Some(*dst)),
                Inst::Cmp { dst, op, lhs, rhs, .. } => (Some(CseKey::Cmp { kind: *op as u8, lhs: *lhs, rhs: *rhs }), Some(*dst)),
                Inst::FieldPtr { dst, base, struct_id, field } => (Some(CseKey::FieldPtr { base: *base, struct_id: *struct_id, field: *field }), Some(*dst)),
                // Don't CSE instructions with side effects (calls, stores, loads)
                _ => (None, None),
            };

            if let (Some(key), Some(dst)) = (key, dst) {
                if let Some(&existing) = seen.get(&key) {
                    replacements.push((i, dst, existing));
                } else {
                    seen.insert(key, dst);
                }
            }

            // Invalidate after calls (they can modify memory)
            if matches!(inst, Inst::Call { .. } | Inst::CallExtern { .. } | Inst::CallPtr { .. } | Inst::CallByRef { .. } | Inst::CallClosure { .. } | Inst::Store { .. }) {
                seen.clear();
            }
        }

        // Apply replacements: replace the instruction with an identity copy.
        // For pointer-producing ops (FieldPtr) use PtrCast — IntCast emits
        // `dst = (void*)(value);` which the C backend's null-store path can't
        // size-propagate through (it loses pointee tracking, and a subsequent
        // `store dst, NULL` falls back to the 8-byte `*(void**)dst = NULL;`
        // form — partially zeroing 32-byte Str fields and breaking the
        // `data=NULL ⟹ len=0` invariant). PtrCast propagates pointee.
        for (idx, _old_dst, existing) in &replacements {
            let old_inst = &block.insts[*idx];
            if let Some(dst) = old_inst.dst() {
                let new_inst = match old_inst {
                    Inst::FieldPtr { .. } => Inst::PtrCast { dst, value: *existing },
                    Inst::Cmp { .. } | Inst::Not { .. } => Inst::IntCast { dst, value: *existing, to: LirType::Bool },
                    inst => {
                        let ty = match inst {
                            Inst::Add { ty, .. } | Inst::Sub { ty, .. } | Inst::Mul { ty, .. }
                            | Inst::Div { ty, .. } | Inst::Rem { ty, .. } | Inst::Neg { ty, .. }
                            | Inst::BitAnd { ty, .. } | Inst::BitOr { ty, .. } | Inst::BitXor { ty, .. }
                            | Inst::BitNot { ty, .. } | Inst::Shl { ty, .. } | Inst::Shr { ty, .. } => ty.clone(),
                            _ => LirType::I64,
                        };
                        Inst::IntCast { dst, value: *existing, to: ty }
                    }
                };
                block.insts[*idx] = new_inst;
                eliminated += 1;
            }
        }
    }

    eliminated
}

// ── Constant Branch Folding ──────────────────────────────────────────────────

/// Replace Branch terminators with known-constant conditions with unconditional
/// Jumps. Replace Switch terminators with known-constant discriminants with
/// unconditional Jumps. Returns count of branches folded.
pub fn fold_constant_branches(func: &mut LirFunction) -> usize {
    // Collect known bool/int constants from all blocks.
    let val_count = func.value_count() as usize;
    let mut known_bool: Vec<Option<bool>> = vec![None; val_count];
    let mut known_int: Vec<Option<i64>> = vec![None; val_count];

    for block in &func.blocks {
        for inst in &block.insts {
            match inst {
                Inst::BoolConst { dst, value } => {
                    known_bool[dst.0 as usize] = Some(*value);
                }
                Inst::IConst { dst, value, .. } => {
                    known_int[dst.0 as usize] = Some(*value);
                }
                _ => {}
            }
        }
    }

    let mut folded = 0;
    for block in &mut func.blocks {
        let replacement = match &block.terminator {
            Term::Branch { cond, then_block, then_args, else_block, else_args, .. } => {
                known_bool.get(cond.0 as usize).and_then(|v| *v).map(|val| {
                    if val {
                        Term::Jump(*then_block, then_args.clone())
                    } else {
                        Term::Jump(*else_block, else_args.clone())
                    }
                })
            }
            Term::Switch { value, cases, default, default_args, .. } => {
                known_int.get(value.0 as usize).and_then(|v| *v).map(|val| {
                    for (case_val, target, args) in cases {
                        if *case_val == val {
                            return Term::Jump(*target, args.clone());
                        }
                    }
                    Term::Jump(*default, default_args.clone())
                })
            }
            _ => None,
        };
        if let Some(new_term) = replacement {
            block.terminator = new_term;
            folded += 1;
        }
    }

    folded
}

// ── Dead Block Elimination ──────────────────────────────────────────────────

/// Remove blocks that are unreachable from bb0. Returns count eliminated.
pub fn eliminate_dead_blocks(func: &mut LirFunction) -> usize {
    if func.blocks.is_empty() {
        return 0;
    }

    // BFS from bb0 to find reachable blocks.
    let num_blocks = func.blocks.len();
    let mut reachable = vec![false; num_blocks];
    let mut queue = std::collections::VecDeque::new();
    reachable[0] = true;
    queue.push_back(0usize);

    while let Some(idx) = queue.pop_front() {
        for target in func.blocks[idx].terminator.successors() {
            let t = target.0 as usize;
            if t < num_blocks && !reachable[t] {
                reachable[t] = true;
                queue.push_back(t);
            }
        }
    }

    let dead_count = reachable.iter().filter(|&&r| !r).count();
    if dead_count == 0 {
        return 0;
    }

    // Build old→new block ID mapping.
    let mut new_id = vec![BlockId(0); num_blocks];
    let mut next = 0u32;
    for (old, &is_reachable) in reachable.iter().enumerate() {
        if is_reachable {
            new_id[old] = BlockId(next);
            next += 1;
        }
    }

    // Remove dead blocks and remap block IDs.
    let mut kept = Vec::with_capacity(num_blocks - dead_count);
    for (i, block) in func.blocks.drain(..).enumerate() {
        if reachable[i] {
            kept.push(block);
        }
    }

    // Update block IDs and terminator targets.
    for (new_idx, block) in kept.iter_mut().enumerate() {
        block.id = BlockId(new_idx as u32);
        remap_term_targets(&mut block.terminator, &new_id);
    }

    func.blocks = kept;
    dead_count
}

/// Remap block IDs in a terminator.
fn remap_term_targets(term: &mut Term, map: &[BlockId]) {
    match term {
        Term::Jump(target, _) => {
            *target = map[target.0 as usize];
        }
        Term::Branch { then_block, else_block, .. } => {
            *then_block = map[then_block.0 as usize];
            *else_block = map[else_block.0 as usize];
        }
        Term::Switch { cases, default, .. } => {
            for (_, target, _) in cases.iter_mut() {
                *target = map[target.0 as usize];
            }
            *default = map[default.0 as usize];
        }
        Term::Ret(_) | Term::RetVoid | Term::Unreachable => {}
    }
}

// ── Block Merging ───────────────────────────────────────────────────────────

/// Merge a block into its sole predecessor when the predecessor unconditionally
/// jumps to it (with no block args) and it has exactly one predecessor.
pub fn merge_linear_blocks(func: &mut LirFunction) {
    if func.blocks.len() <= 1 {
        return;
    }

    // Count predecessors for each block.
    let num_blocks = func.blocks.len();
    let mut pred_count = vec![0u32; num_blocks];
    for block in &func.blocks {
        for succ in block.terminator.successors() {
            let s = succ.0 as usize;
            if s < num_blocks {
                pred_count[s] += 1;
            }
        }
    }

    // Greedily merge: find blocks where predecessor jumps unconditionally
    // to a single-predecessor block (no block params needed).
    let mut changed = true;
    while changed {
        changed = false;
        for i in 0..func.blocks.len() {
            let target = match &func.blocks[i].terminator {
                Term::Jump(target, args) if args.is_empty() => {
                    let t = target.0 as usize;
                    if t < func.blocks.len() && t != i && pred_count[t] == 1
                        && func.blocks[t].params.is_empty()
                    {
                        Some(t)
                    } else {
                        None
                    }
                }
                _ => None,
            };
            if let Some(t) = target {
                // Merge: append target's instructions and take its terminator.
                // Carry both `span_map` and `terminator_span` across the
                // merge so the parallel-array invariant + terminator
                // attribution survive the block coalesce.
                let target_insts = std::mem::take(&mut func.blocks[t].insts);
                let target_spans = std::mem::take(&mut func.blocks[t].span_map);
                let target_term = std::mem::replace(
                    &mut func.blocks[t].terminator,
                    Term::Unreachable,
                );
                let target_term_span =
                    std::mem::take(&mut func.blocks[t].terminator_span);
                // Reconcile target spans if they were out of sync (pre-1b
                // emitters): pad with `None` to keep parallel-arrays valid.
                let target_spans_padded: Vec<Option<crate::span::Span>> =
                    if target_spans.len() == target_insts.len() {
                        target_spans
                    } else {
                        vec![None; target_insts.len()]
                    };
                // Same reconciliation for the source block.
                if func.blocks[i].span_map.len() != func.blocks[i].insts.len() {
                    func.blocks[i].span_map =
                        vec![None; func.blocks[i].insts.len()];
                }
                func.blocks[i].insts.extend(target_insts);
                func.blocks[i].span_map.extend(target_spans_padded);
                func.blocks[i].terminator = target_term;
                func.blocks[i].terminator_span = target_term_span;
                pred_count[t] = 0; // mark as dead
                changed = true;
            }
        }
    }

    // Remove dead blocks (those with 0 predecessors except entry).
    let mut reachable = vec![false; func.blocks.len()];
    let mut queue = std::collections::VecDeque::new();
    reachable[0] = true;
    queue.push_back(0usize);
    while let Some(idx) = queue.pop_front() {
        for succ in func.blocks[idx].terminator.successors() {
            let s = succ.0 as usize;
            if s < func.blocks.len() && !reachable[s] {
                reachable[s] = true;
                queue.push_back(s);
            }
        }
    }

    if reachable.iter().all(|&r| r) {
        return;
    }

    // Remap and compact.
    let mut new_id = vec![BlockId(0); func.blocks.len()];
    let mut next = 0u32;
    for (old, &is_r) in reachable.iter().enumerate() {
        if is_r {
            new_id[old] = BlockId(next);
            next += 1;
        }
    }

    let mut kept = Vec::new();
    for (i, block) in func.blocks.drain(..).enumerate() {
        if reachable[i] {
            kept.push(block);
        }
    }
    for (new_idx, block) in kept.iter_mut().enumerate() {
        block.id = BlockId(new_idx as u32);
        remap_term_targets(&mut block.terminator, &new_id);
    }
    func.blocks = kept;
}

// ── Copy Propagation ────────────────────────────────────────────────────────

/// Propagate trivial block params (phis where all incoming values are identical).
/// Returns count of copies propagated.
pub fn propagate_copies(func: &mut LirFunction) -> usize {
    if func.blocks.is_empty() {
        return 0;
    }

    // Collect predecessor info: for each block, what values are passed as jump args.
    // Map: (target_block, param_index) → set of argument ValueIds.
    let mut param_sources: rustc_hash::FxHashMap<(u32, usize), HashSet<ValueId>> =
        rustc_hash::FxHashMap::default();

    for block in &func.blocks {
        let collect_args = |target: BlockId, args: &[ValueId]| {
            let mut entries = Vec::new();
            for (i, &arg) in args.iter().enumerate() {
                entries.push(((target.0, i), arg));
            }
            entries
        };

        let entries: Vec<((u32, usize), ValueId)> = match &block.terminator {
            Term::Jump(target, args) => collect_args(*target, args),
            Term::Branch { then_block, then_args, else_block, else_args, .. } => {
                let mut e = collect_args(*then_block, then_args);
                e.extend(collect_args(*else_block, else_args));
                e
            }
            Term::Switch { cases, default, default_args, .. } => {
                let mut e = Vec::new();
                for (_, target, args) in cases {
                    e.extend(collect_args(*target, args));
                }
                e.extend(collect_args(*default, default_args));
                e
            }
            Term::Ret(_) | Term::RetVoid | Term::Unreachable => Vec::new(),
        };

        for ((block_id, param_idx), arg) in entries {
            param_sources.entry((block_id, param_idx)).or_default().insert(arg);
        }
    }

    // Find trivial params: all predecessors pass the same value.
    // Track both the substitution map and which (block, param_index) to remove.
    let mut subst: rustc_hash::FxHashMap<ValueId, ValueId> = rustc_hash::FxHashMap::default();
    let mut removed_params: rustc_hash::FxHashMap<u32, HashSet<usize>> =
        rustc_hash::FxHashMap::default();

    for block in &func.blocks {
        for (param_idx, (param_vid, _)) in block.params.iter().enumerate() {
            if let Some(sources) = param_sources.get(&(block.id.0, param_idx)) {
                if sources.len() == 1 {
                    let &single_val = sources.iter().next().unwrap();
                    if single_val != *param_vid {
                        subst.insert(*param_vid, single_val);
                        removed_params.entry(block.id.0).or_default().insert(param_idx);
                    }
                }
            }
        }
    }

    if subst.is_empty() {
        return 0;
    }

    // Chase substitution chains: if a → b and b → c, then a → c.
    let keys: Vec<ValueId> = subst.keys().copied().collect();
    for key in &keys {
        let mut val = subst[key];
        while let Some(&next) = subst.get(&val) {
            if next == val { break; }
            val = next;
        }
        subst.insert(*key, val);
    }

    let count = subst.len();

    // Apply substitutions to all instructions and terminators.
    for block in &mut func.blocks {
        for inst in &mut block.insts {
            subst_inst_uses(inst, &subst);
        }
        subst_term_uses(&mut block.terminator, &subst);
        block.params.retain(|(vid, _)| !subst.contains_key(vid));
    }

    // Remove the corresponding jump args from predecessors.
    for block in &mut func.blocks {
        remove_jump_args_for_removed_params(&mut block.terminator, &removed_params);
    }

    count
}

/// Substitute ValueIds in an instruction's operands.
fn subst_inst_uses(inst: &mut Inst, subst: &rustc_hash::FxHashMap<ValueId, ValueId>) {
    // Apply substitution to all used values.
    let mut uses = inst.uses();
    let mut any_changed = false;
    for u in &mut uses {
        if let Some(&replacement) = subst.get(u) {
            *u = replacement;
            any_changed = true;
        }
    }
    if !any_changed {
        return;
    }
    // Write substituted values back into the instruction.
    // We use the same match structure as `uses()` to maintain correspondence.
    let mut idx = 0;
    let next = |uses: &[ValueId], idx: &mut usize| -> ValueId { let v = uses[*idx]; *idx += 1; v };

    match inst {
        Inst::SlotStore { value, .. } => { *value = next(&uses, &mut idx); }
        Inst::ClosurePack { env_ptr, .. } => { *env_ptr = next(&uses, &mut idx); }
        Inst::SlotLoad { .. } | Inst::SlotAddr { .. } => {}
        Inst::IConst { .. } | Inst::FConst { .. } | Inst::BoolConst { .. }
        | Inst::NullPtr { .. } | Inst::FuncAddr { .. } | Inst::NamedFuncAddr { .. }
        | Inst::GlobalAddr { .. }
        | Inst::StrLit { .. } | Inst::ParamRef { .. } | Inst::MoveSlot { .. }
        | Inst::SizeOf { .. }
        | Inst::Nop | Inst::InlineC { .. } => {}
        Inst::Add { lhs, rhs, .. } | Inst::Sub { lhs, rhs, .. }
        | Inst::Mul { lhs, rhs, .. } | Inst::Div { lhs, rhs, .. }
        | Inst::Rem { lhs, rhs, .. } | Inst::Mod { lhs, rhs, .. }
        | Inst::FaultCheck { lhs, rhs, .. } => {
            *lhs = next(&uses, &mut idx); *rhs = next(&uses, &mut idx);
        }
        Inst::Neg { operand, .. } => { *operand = next(&uses, &mut idx); }
        Inst::BitAnd { lhs, rhs, .. } | Inst::BitOr { lhs, rhs, .. }
        | Inst::BitXor { lhs, rhs, .. } | Inst::Shl { lhs, rhs, .. }
        | Inst::Shr { lhs, rhs, .. } => {
            *lhs = next(&uses, &mut idx); *rhs = next(&uses, &mut idx);
        }
        Inst::BitNot { operand, .. } => { *operand = next(&uses, &mut idx); }
        Inst::Cmp { lhs, rhs, .. } => { *lhs = next(&uses, &mut idx); *rhs = next(&uses, &mut idx); }
        Inst::Not { operand, .. } => { *operand = next(&uses, &mut idx); }
        Inst::IntCast { value, .. } | Inst::FloatCast { value, .. }
        | Inst::IntToFloat { value, .. } | Inst::FloatToInt { value, .. }
        | Inst::PtrCast { value, .. } | Inst::Bitcast { value, .. } => {
            *value = next(&uses, &mut idx);
        }
        Inst::Load { ptr, .. } => { *ptr = next(&uses, &mut idx); }
        Inst::Store { ptr, value } => { *ptr = next(&uses, &mut idx); *value = next(&uses, &mut idx); }
        Inst::FieldPtr { base, .. } => { *base = next(&uses, &mut idx); }
        Inst::ElemPtr { base, index, .. } => { *base = next(&uses, &mut idx); *index = next(&uses, &mut idx); }
        Inst::Memset { ptr, byte, size } => {
            *ptr = next(&uses, &mut idx); *byte = next(&uses, &mut idx); *size = next(&uses, &mut idx);
        }
        Inst::Memcpy { dst_ptr, src_ptr, size } => {
            *dst_ptr = next(&uses, &mut idx); *src_ptr = next(&uses, &mut idx); *size = next(&uses, &mut idx);
        }
        Inst::Call { args, .. } => { for a in args { *a = next(&uses, &mut idx); } }
        Inst::CallExtern { args, .. } => { for a in args { *a = next(&uses, &mut idx); } }
        Inst::CallRuntime { args, .. } => { for a in args { *a = next(&uses, &mut idx); } }
        Inst::CollectionCtor { args, .. } => { for a in args { *a = next(&uses, &mut idx); } }
        Inst::CallPtr { callee, args, .. } => {
            *callee = next(&uses, &mut idx); for a in args { *a = next(&uses, &mut idx); }
        }
        Inst::CallByRef { fref, args, .. } => {
            *fref = next(&uses, &mut idx); for a in args { *a = next(&uses, &mut idx); }
        }
        Inst::CallClosure { closure, args, .. } => {
            *closure = next(&uses, &mut idx); for a in args { *a = next(&uses, &mut idx); }
        }
        Inst::DropGuardOpen { value, .. } => { *value = next(&uses, &mut idx); }
        Inst::DropGuardClose => {}
        Inst::BoundsCheck { index, len } => { *index = next(&uses, &mut idx); *len = next(&uses, &mut idx); }
        Inst::DivCheck { divisor } => { *divisor = next(&uses, &mut idx); }
        Inst::Trap { .. } => {}
        Inst::Printf { args, .. } => { for a in args { *a = next(&uses, &mut idx); } }
        Inst::Fprintf { fd, args, .. } => {
            *fd = next(&uses, &mut idx); for a in args { *a = next(&uses, &mut idx); }
        }
        Inst::EnumInit { target, fields, .. } => {
            *target = next(&uses, &mut idx);
            for (_, v) in fields.iter_mut() { *v = next(&uses, &mut idx); }
        }
        Inst::EnumCheck { value, .. } => { *value = next(&uses, &mut idx); }
        Inst::EnumExtract { value, .. } => { *value = next(&uses, &mut idx); }
        Inst::StructInit { target, fields, .. } => {
            *target = next(&uses, &mut idx);
            for (_, v) in fields.iter_mut() { *v = next(&uses, &mut idx); }
        }
        Inst::CowClone { src, .. } => { *src = next(&uses, &mut idx); }
        Inst::TraitCall { object, args, .. } => {
            *object = next(&uses, &mut idx);
            for a in args { *a = next(&uses, &mut idx); }
        }
        Inst::HofExpand { coll, closure, init, .. } => {
            *coll = next(&uses, &mut idx);
            *closure = next(&uses, &mut idx);
            if let Some(i) = init { *i = next(&uses, &mut idx); }
        }
        Inst::AddressOf { value, .. } => { *value = next(&uses, &mut idx); }
        Inst::BoxAlloc { value, .. } => { *value = next(&uses, &mut idx); }
        Inst::SetCollectionBridge { collection, .. } => { *collection = next(&uses, &mut idx); }
    }
}

/// Substitute ValueIds in a terminator's operands.
fn subst_term_uses(term: &mut Term, subst: &rustc_hash::FxHashMap<ValueId, ValueId>) {
    let s = |v: &mut ValueId| {
        if let Some(&replacement) = subst.get(v) {
            *v = replacement;
        }
    };

    match term {
        Term::Ret(v) => s(v),
        Term::RetVoid | Term::Unreachable => {}
        Term::Jump(_, args) => { for a in args { s(a); } }
        Term::Branch { cond, then_args, else_args, .. } => {
            s(cond);
            for a in then_args { s(a); }
            for a in else_args { s(a); }
        }
        Term::Switch { value, cases, default_args, .. } => {
            s(value);
            for (_, _, args) in cases { for a in args { s(a); } }
            for a in default_args { s(a); }
        }
    }
}

/// Remove jump args at indices corresponding to removed block params.
fn remove_jump_args_for_removed_params(
    term: &mut Term,
    removed: &rustc_hash::FxHashMap<u32, HashSet<usize>>,
) {
    let filter_args = |target: BlockId, args: &mut Vec<ValueId>| {
        if let Some(indices) = removed.get(&target.0) {
            let mut new_args = Vec::new();
            for (i, arg) in args.iter().enumerate() {
                if !indices.contains(&i) {
                    new_args.push(*arg);
                }
            }
            *args = new_args;
        }
    };

    match term {
        Term::Jump(target, args) => filter_args(*target, args),
        Term::Branch { then_block, then_args, else_block, else_args, .. } => {
            filter_args(*then_block, then_args);
            filter_args(*else_block, else_args);
        }
        Term::Switch { cases, default, default_args, .. } => {
            for (_, target, args) in cases {
                filter_args(*target, args);
            }
            filter_args(*default, default_args);
        }
        Term::Ret(_) | Term::RetVoid | Term::Unreachable => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_module_with_dead_fn() -> LirModule {
        let mut module = LirModule::new();

        // fn main() -> i32 { call helper(); return 0; }
        let mut main_fn = LirFunction::new("main".into(), vec![], LirType::I32);
        let bb = main_fn.add_block();
        let v0 = main_fn.next_value();
        let v1 = main_fn.next_value();
        main_fn.block_mut(bb).insts = vec![
            Inst::Call {
                dst: Some(v0),
                func: FuncId(1), // helper
                args: vec![],
            },
            Inst::IConst {
                dst: v1,
                ty: LirType::I32,
                value: 0,
            },
        ];
        main_fn.block_mut(bb).terminator = Term::Ret(v1);
        module.add_function(main_fn);

        // fn helper() -> i64 { return 42; }
        let mut helper = LirFunction::new("helper".into(), vec![], LirType::I64);
        let bb = helper.add_block();
        let v0 = helper.next_value();
        helper.block_mut(bb).insts.push(Inst::IConst {
            dst: v0,
            ty: LirType::I64,
            value: 42,
        });
        helper.block_mut(bb).terminator = Term::Ret(v0);
        module.add_function(helper);

        // fn dead() -> i64 { return 99; } — never called
        let mut dead = LirFunction::new("dead".into(), vec![], LirType::I64);
        let bb = dead.add_block();
        let v0 = dead.next_value();
        dead.block_mut(bb).insts.push(Inst::IConst {
            dst: v0,
            ty: LirType::I64,
            value: 99,
        });
        dead.block_mut(bb).terminator = Term::Ret(v0);
        module.add_function(dead);

        module
    }

    #[test]
    fn dead_function_elimination() {
        let mut module = make_module_with_dead_fn();
        assert_eq!(module.functions.len(), 3);

        let eliminated = eliminate_dead_functions(&mut module);
        assert_eq!(eliminated, 1, "should eliminate 1 dead function");
        assert_eq!(module.functions.len(), 2);
        assert_eq!(module.functions[0].name, "main");
        assert_eq!(module.functions[1].name, "helper");

        // Verify FuncId in main's Call was remapped.
        let call = &module.functions[0].blocks[0].insts[0];
        if let Inst::Call { func, .. } = call {
            assert_eq!(*func, FuncId(1), "helper should be FuncId(1) after remap");
        } else {
            panic!("expected Call instruction");
        }
    }

    #[test]
    fn dead_fn_transitive() {
        let mut module = LirModule::new();

        // main calls a, a calls b, c is dead
        let mut main_fn = LirFunction::new("main".into(), vec![], LirType::I32);
        let bb = main_fn.add_block();
        let v0 = main_fn.next_value();
        main_fn.block_mut(bb).insts.push(Inst::Call {
            dst: Some(v0),
            func: FuncId(1), // a
            args: vec![],
        });
        main_fn.block_mut(bb).terminator = Term::Ret(v0);
        module.add_function(main_fn);

        let mut fn_a = LirFunction::new("a".into(), vec![], LirType::I32);
        let bb = fn_a.add_block();
        let v0 = fn_a.next_value();
        fn_a.block_mut(bb).insts.push(Inst::Call {
            dst: Some(v0),
            func: FuncId(2), // b
            args: vec![],
        });
        fn_a.block_mut(bb).terminator = Term::Ret(v0);
        module.add_function(fn_a);

        let mut fn_b = LirFunction::new("b".into(), vec![], LirType::I32);
        let bb = fn_b.add_block();
        let v0 = fn_b.next_value();
        fn_b.block_mut(bb).insts.push(Inst::IConst {
            dst: v0,
            ty: LirType::I32,
            value: 42,
        });
        fn_b.block_mut(bb).terminator = Term::Ret(v0);
        module.add_function(fn_b);

        let mut fn_c = LirFunction::new("c_dead".into(), vec![], LirType::I32);
        let bb = fn_c.add_block();
        fn_c.block_mut(bb).terminator = Term::RetVoid;
        module.add_function(fn_c);

        let eliminated = eliminate_dead_functions(&mut module);
        assert_eq!(eliminated, 1);
        assert_eq!(module.functions.len(), 3);
        assert!(module.functions.iter().all(|f| f.name != "c_dead"));
    }

    #[test]
    fn funcaddr_keeps_alive() {
        let mut module = LirModule::new();

        // main takes FuncAddr of helper (no direct call)
        let mut main_fn = LirFunction::new("main".into(), vec![], LirType::I32);
        let bb = main_fn.add_block();
        let v0 = main_fn.next_value();
        let v1 = main_fn.next_value();
        main_fn.block_mut(bb).insts = vec![
            Inst::FuncAddr {
                dst: v0,
                func: FuncId(1),
            },
            Inst::IConst {
                dst: v1,
                ty: LirType::I32,
                value: 0,
            },
        ];
        main_fn.block_mut(bb).terminator = Term::Ret(v1);
        module.add_function(main_fn);

        let mut helper = LirFunction::new("helper".into(), vec![], LirType::Void);
        let bb = helper.add_block();
        helper.block_mut(bb).terminator = Term::RetVoid;
        module.add_function(helper);

        let eliminated = eliminate_dead_functions(&mut module);
        assert_eq!(eliminated, 0, "FuncAddr reference should keep helper alive");
    }

    #[test]
    fn dead_code_elimination() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::I32);
        let bb = func.add_block();

        let v0 = func.next_value();
        let v1 = func.next_value(); // dead
        let v2 = func.next_value(); // dead
        let v3 = func.next_value();

        func.block_mut(bb).insts = vec![
            Inst::IConst {
                dst: v0,
                ty: LirType::I32,
                value: 42,
            },
            Inst::IConst {
                dst: v1,
                ty: LirType::I64,
                value: 99, // dead — never used
            },
            Inst::IConst {
                dst: v2,
                ty: LirType::I64,
                value: 100, // dead — never used
            },
            Inst::IConst {
                dst: v3,
                ty: LirType::I32,
                value: 0,
            },
        ];
        func.block_mut(bb).terminator = Term::Ret(v3);

        let eliminated = eliminate_dead_code(&mut func);
        assert_eq!(eliminated, 3, "should eliminate 3 dead constants");
        assert_eq!(func.blocks[0].insts.len(), 1);
    }

    #[test]
    fn side_effect_instructions_kept() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::Void);
        let bb = func.add_block();

        func.block_mut(bb).insts = vec![
            Inst::CallExtern {
                dst: None,
                name: "puts".into(),
                args: vec![],
                arg_abis: vec![],
            },
            Inst::Printf {
                fmt: "hello\n".into(),
                args: vec![],
            },
            Inst::Nop,
        ];
        func.block_mut(bb).terminator = Term::RetVoid;

        let eliminated = eliminate_dead_code(&mut func);
        assert_eq!(eliminated, 0, "side-effect instructions should be kept");
        assert_eq!(func.blocks[0].insts.len(), 3);
    }

    #[test]
    fn dead_global_elimination() {
        let mut module = LirModule::new();

        module.add_global(LirGlobal {
            name: "used_global".into(),
            ty: LirType::I64,
            init: LirGlobalInit::Zeroed,
            is_const: false,
        });
        module.add_global(LirGlobal {
            name: "dead_global".into(),
            ty: LirType::I64,
            init: LirGlobalInit::Zeroed,
            is_const: false,
        });

        let mut func = LirFunction::new("main".into(), vec![], LirType::I32);
        let bb = func.add_block();
        let v0 = func.next_value();
        let v1 = func.next_value();
        func.block_mut(bb).insts = vec![
            Inst::GlobalAddr {
                dst: v0,
                global: GlobalId(0), // references used_global
            },
            Inst::IConst {
                dst: v1,
                ty: LirType::I32,
                value: 0,
            },
        ];
        func.block_mut(bb).terminator = Term::Ret(v1);
        module.add_function(func);

        let eliminated = eliminate_dead_globals(&mut module);
        assert_eq!(eliminated, 1);
        assert_eq!(module.globals.len(), 1);
        assert_eq!(module.globals[0].name, "used_global");
    }

    #[test]
    fn full_optimization_pass() {
        let mut module = make_module_with_dead_fn();
        let stats = optimize_module(&mut module);
        assert_eq!(stats.dead_functions_eliminated, 1);
        assert_eq!(module.functions.len(), 2);
    }

    #[test]
    fn empty_module_optimization() {
        let mut module = LirModule::new();
        let stats = optimize_module(&mut module);
        assert_eq!(stats.dead_functions_eliminated, 0);
        assert_eq!(stats.dead_globals_eliminated, 0);
        assert_eq!(stats.dead_instructions_eliminated, 0);
    }

    #[test]
    fn fold_constant_branch_true() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::I32);
        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();

        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb0).insts.push(Inst::BoolConst { dst: v0, value: true });
        func.block_mut(bb0).terminator = Term::Branch {
            cond: v0,
            then_block: bb1, then_args: vec![],
            else_block: bb2, else_args: vec![],
        };
        func.block_mut(bb1).insts.push(Inst::IConst { dst: v1, ty: LirType::I32, value: 1 });
        func.block_mut(bb1).terminator = Term::Ret(v1);
        func.block_mut(bb2).insts.push(Inst::IConst { dst: v2, ty: LirType::I32, value: 2 });
        func.block_mut(bb2).terminator = Term::Ret(v2);

        let folded = fold_constant_branches(&mut func);
        assert_eq!(folded, 1);
        // bb0 should now jump to bb1
        assert!(matches!(func.blocks[0].terminator, Term::Jump(BlockId(1), _)));
    }

    #[test]
    fn dead_block_elimination_after_branch_fold() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::I32);
        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block(); // will become dead

        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb0).insts.push(Inst::BoolConst { dst: v0, value: false });
        func.block_mut(bb0).terminator = Term::Branch {
            cond: v0,
            then_block: bb1, then_args: vec![],
            else_block: bb2, else_args: vec![],
        };
        func.block_mut(bb1).insts.push(Inst::IConst { dst: v1, ty: LirType::I32, value: 1 });
        func.block_mut(bb1).terminator = Term::Ret(v1);
        func.block_mut(bb2).insts.push(Inst::IConst { dst: v2, ty: LirType::I32, value: 2 });
        func.block_mut(bb2).terminator = Term::Ret(v2);

        // First fold the branch (condition=false → jump to bb2)
        fold_constant_branches(&mut func);
        assert!(matches!(func.blocks[0].terminator, Term::Jump(BlockId(2), _)));

        // Now eliminate dead blocks — bb1 should be removed
        let eliminated = eliminate_dead_blocks(&mut func);
        assert_eq!(eliminated, 1, "bb1 should be dead");
        assert_eq!(func.blocks.len(), 2);
    }

    #[test]
    fn merge_linear_chain() {
        // bb0 → bb1 → bb2 (all unconditional, no params)
        // Should merge into a single block.
        let mut func = LirFunction::new("test".into(), vec![], LirType::I32);
        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();

        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb0).insts.push(Inst::IConst { dst: v0, ty: LirType::I32, value: 1 });
        func.block_mut(bb0).terminator = Term::Jump(bb1, vec![]);

        func.block_mut(bb1).insts.push(Inst::IConst { dst: v1, ty: LirType::I32, value: 2 });
        func.block_mut(bb1).terminator = Term::Jump(bb2, vec![]);

        func.block_mut(bb2).insts.push(Inst::IConst { dst: v2, ty: LirType::I32, value: 3 });
        func.block_mut(bb2).terminator = Term::Ret(v2);

        merge_linear_blocks(&mut func);
        // All three blocks should be merged into one.
        assert_eq!(func.blocks.len(), 1, "should merge 3 blocks into 1");
        assert_eq!(func.blocks[0].insts.len(), 3, "merged block has all 3 insts");
        assert!(matches!(func.blocks[0].terminator, Term::Ret(_)));
    }

    #[test]
    fn fold_constant_switch() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::I32);
        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();
        let bb3 = func.add_block(); // default

        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();
        let v3 = func.next_value();

        func.block_mut(bb0).insts.push(Inst::IConst { dst: v0, ty: LirType::I64, value: 2 });
        func.block_mut(bb0).terminator = Term::Switch {
            value: v0,
            cases: vec![(1, bb1, vec![]), (2, bb2, vec![])],
            default: bb3,
            default_args: vec![],
        };
        func.block_mut(bb1).insts.push(Inst::IConst { dst: v1, ty: LirType::I32, value: 10 });
        func.block_mut(bb1).terminator = Term::Ret(v1);
        func.block_mut(bb2).insts.push(Inst::IConst { dst: v2, ty: LirType::I32, value: 20 });
        func.block_mut(bb2).terminator = Term::Ret(v2);
        func.block_mut(bb3).insts.push(Inst::IConst { dst: v3, ty: LirType::I32, value: 99 });
        func.block_mut(bb3).terminator = Term::Ret(v3);

        let folded = fold_constant_branches(&mut func);
        assert_eq!(folded, 1);
        // Should jump to bb2 (case 2)
        assert!(matches!(func.blocks[0].terminator, Term::Jump(BlockId(2), _)));

        // Dead block elimination should remove bb1 and bb3
        let eliminated = eliminate_dead_blocks(&mut func);
        assert_eq!(eliminated, 2);
        assert_eq!(func.blocks.len(), 2);
    }

    #[test]
    fn constant_fold_add() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::I64);
        let bb = func.add_block();
        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb).insts = vec![
            Inst::IConst { dst: v0, ty: LirType::I64, value: 10 },
            Inst::IConst { dst: v1, ty: LirType::I64, value: 32 },
            Inst::Add { dst: v2, ty: LirType::I64, lhs: v0, rhs: v1, overflow: Overflow::Wrap },
        ];
        func.block_mut(bb).terminator = Term::Ret(v2);

        let folded = fold_constants(&mut func);
        assert_eq!(folded, 1, "should fold one Add");
        // v2 should now be IConst 42
        match &func.blocks[0].insts[2] {
            Inst::IConst { value: 42, .. } => {}
            other => panic!("expected IConst 42, got {:?}", other),
        }
    }

    #[test]
    fn constant_fold_cascading() {
        // v0=3, v1=4, v2=v0*v1=12, v3=v2+8=20 — two folds
        let mut func = LirFunction::new("test".into(), vec![], LirType::I64);
        let bb = func.add_block();
        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();
        let v3 = func.next_value();
        let v4 = func.next_value();

        func.block_mut(bb).insts = vec![
            Inst::IConst { dst: v0, ty: LirType::I64, value: 3 },
            Inst::IConst { dst: v1, ty: LirType::I64, value: 4 },
            Inst::Mul { dst: v2, ty: LirType::I64, lhs: v0, rhs: v1, overflow: Overflow::Wrap },
            Inst::IConst { dst: v3, ty: LirType::I64, value: 8 },
            Inst::Add { dst: v4, ty: LirType::I64, lhs: v2, rhs: v3, overflow: Overflow::Wrap },
        ];
        func.block_mut(bb).terminator = Term::Ret(v4);

        let folded = fold_constants(&mut func);
        assert_eq!(folded, 2, "should cascade: fold mul then add");
        match &func.blocks[0].insts[4] {
            Inst::IConst { value: 20, .. } => {}
            other => panic!("expected IConst 20, got {:?}", other),
        }
    }

    #[test]
    fn constant_fold_comparison() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::Bool);
        let bb = func.add_block();
        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb).insts = vec![
            Inst::IConst { dst: v0, ty: LirType::I64, value: 5 },
            Inst::IConst { dst: v1, ty: LirType::I64, value: 10 },
            Inst::Cmp { dst: v2, op: CmpOp::Lt, lhs: v0, rhs: v1 },
        ];
        func.block_mut(bb).terminator = Term::Ret(v2);

        let folded = fold_constants(&mut func);
        assert_eq!(folded, 1);
        match &func.blocks[0].insts[2] {
            Inst::BoolConst { value: true, .. } => {}
            other => panic!("expected BoolConst true, got {:?}", other),
        }
    }

    #[test]
    fn constant_fold_bitwise() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::I64);
        let bb = func.add_block();
        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb).insts = vec![
            Inst::IConst { dst: v0, ty: LirType::I64, value: 0xFF },
            Inst::IConst { dst: v1, ty: LirType::I64, value: 0x0F },
            Inst::BitAnd { dst: v2, ty: LirType::I64, lhs: v0, rhs: v1 },
        ];
        func.block_mut(bb).terminator = Term::Ret(v2);

        let folded = fold_constants(&mut func);
        assert_eq!(folded, 1);
        match &func.blocks[0].insts[2] {
            Inst::IConst { value: 0x0F, .. } => {}
            other => panic!("expected IConst 0x0F, got {:?}", other),
        }
    }

    #[test]
    fn constant_fold_float() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::F64);
        let bb = func.add_block();
        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb).insts = vec![
            Inst::FConst { dst: v0, ty: LirType::F64, bits: 2.5_f64.to_bits() },
            Inst::FConst { dst: v1, ty: LirType::F64, bits: 3.0_f64.to_bits() },
            Inst::Mul { dst: v2, ty: LirType::F64, lhs: v0, rhs: v1, overflow: Overflow::Wrap },
        ];
        func.block_mut(bb).terminator = Term::Ret(v2);

        let folded = fold_constants(&mut func);
        assert_eq!(folded, 1);
        match &func.blocks[0].insts[2] {
            Inst::FConst { bits, .. } => {
                assert_eq!(f64::from_bits(*bits), 7.5);
            }
            other => panic!("expected FConst 7.5, got {:?}", other),
        }
    }

    #[test]
    fn constant_fold_div_by_zero_not_folded() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::I64);
        let bb = func.add_block();
        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb).insts = vec![
            Inst::IConst { dst: v0, ty: LirType::I64, value: 42 },
            Inst::IConst { dst: v1, ty: LirType::I64, value: 0 },
            Inst::Div { dst: v2, ty: LirType::I64, lhs: v0, rhs: v1 },
        ];
        func.block_mut(bb).terminator = Term::Ret(v2);

        let folded = fold_constants(&mut func);
        assert_eq!(folded, 0, "division by zero should not be folded");
    }

    #[test]
    fn copy_propagation_trivial_phi() {
        // bb0: v0 = 42; jmp bb1(v0)
        // bb1(v1): ret v1
        // → v1 should be replaced by v0, bb1 param removed.
        let mut func = LirFunction::new("test".into(), vec![], LirType::I64);
        let bb0 = func.add_block();
        let bb1 = func.add_block();

        let v0 = func.next_value();
        let v1 = func.next_value();

        func.block_mut(bb0).insts.push(Inst::IConst {
            dst: v0, ty: LirType::I64, value: 42,
        });
        func.block_mut(bb0).terminator = Term::Jump(bb1, vec![v0]);

        func.block_mut(bb1).params.push((v1, LirType::I64));
        func.block_mut(bb1).terminator = Term::Ret(v1);

        let propagated = propagate_copies(&mut func);
        assert_eq!(propagated, 1);
        assert!(func.blocks[1].params.is_empty(), "trivial param should be removed");
        // Terminator should now use v0 directly.
        assert_eq!(func.blocks[1].terminator, Term::Ret(v0));
        // Jump args should be empty.
        match &func.blocks[0].terminator {
            Term::Jump(_, args) => assert!(args.is_empty()),
            _ => panic!("expected Jump"),
        }
    }

    #[test]
    fn copy_propagation_non_trivial_phi_preserved() {
        // bb0: v0 = 1; jmp bb2(v0)
        // bb1: v1 = 2; jmp bb2(v1)
        // bb2(v2): ret v2
        // → v2 has different sources, should NOT be propagated.
        let mut func = LirFunction::new("test".into(), vec![], LirType::I64);
        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();

        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb0).insts.push(Inst::IConst {
            dst: v0, ty: LirType::I64, value: 1,
        });
        func.block_mut(bb0).terminator = Term::Jump(bb2, vec![v0]);

        func.block_mut(bb1).insts.push(Inst::IConst {
            dst: v1, ty: LirType::I64, value: 2,
        });
        func.block_mut(bb1).terminator = Term::Jump(bb2, vec![v1]);

        func.block_mut(bb2).params.push((v2, LirType::I64));
        func.block_mut(bb2).terminator = Term::Ret(v2);

        let propagated = propagate_copies(&mut func);
        assert_eq!(propagated, 0, "non-trivial phi should not be propagated");
        assert_eq!(func.blocks[2].params.len(), 1, "param should be preserved");
    }
}
