//! P2.6: Drop & Move Elaboration
//!
//! Inserts explicit `Drop` and `MoveZero` instructions based on type
//! ownership metadata. Tracks drop scopes (function, loop, block) and
//! emits cleanup code at scope exits and early returns.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::Place;
use crate::ir::types::*;

/// Tracks owned locals and emits drop instructions at scope boundaries.
///
/// **Contract with the backend**: This elaborator decides WHEN to drop by
/// emitting `Drop { place }` / `DropIfAlive { place }` instructions.
/// The backend decides HOW to drop by looking up the type's `DropStrategy`
/// from the `TypeRegistry`. See `TypeMetadata` docs for valid combinations.
///
/// Registration rules:
/// - `register_local`: registers Move-type locals + any type with non-None drop
/// - `register_param`: registers Copy-type params with non-None drop (ref-counted)
/// - Move-type params are NOT registered here (handled by body-level mechanisms)
pub struct DropElaborator {
    /// Stack of drop scopes, innermost last.
    scopes: Vec<DropScope>,
    /// Borrow dependency edges: borrower LocalId → Vec<source LocalId>.
    /// A borrower must be dropped BEFORE its sources.
    borrow_deps: rustc_hash::FxHashMap<LocalId, Vec<LocalId>>,
}

/// A drop scope corresponds to a language construct that owns locals.
struct DropScope {
    kind: DropScopeKind,
    /// Locals registered in this scope, in declaration order.
    /// Drops are emitted in reverse (LIFO) order.
    entries: Vec<DropEntry>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DropScopeKind {
    Function,
    Loop,
    Block,
    /// Expression-statement temporary scope. Pushed around `Stmt::Expr`
    /// only. `pop_statement_guard_temps` drops `GuardKind` entries at
    /// statement end (releases Mutex/RWLock guards so sequential
    /// acquires do not self-deadlock) and re-registers non-Guard
    /// droppables into the parent scope. Named binds / VarDecl / Assign
    /// / `with` do **not** use this kind (MVP).
    Statement,
}

/// An entry tracking a local that needs dropping at scope exit.
#[derive(Clone)]
struct DropEntry {
    local: LocalId,
    type_id: TypeId,
    /// If true, the value may have been moved — use DropIfAlive instead of Drop.
    maybe_moved: bool,
    /// If true, bypass needs_drop checks at emission time.
    force_drop: bool,
    /// If true, the local is a `!`-sigil resource parameter: the slot
    /// holds a pointer (MutPtr) but the callee owns the pointee. The
    /// drop must dereference through the pointer; the GIR `DropIfAlive`
    /// is emitted on a `Place { local, projections: [Deref] }` so the
    /// LIR lowering goes through the correct addr-load path. The slot
    /// is `Initialized` at bb0 (caller-supplied), so the LIR drop_elab
    /// dataflow seeds the drop flag to `true` automatically; subsequent
    /// `MoveZero`/`MoveSlot` emissions on the slot (when the function
    /// transfers ownership onward via `consume`/`push`/`put`/etc.) flip
    /// the flag to `false` and suppress the exit drop.
    owning_param: bool,
}

impl DropElaborator {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            borrow_deps: rustc_hash::FxHashMap::default(),
        }
    }

    /// Register that `borrower` borrows from `source`.
    /// At scope exit, `borrower` will be dropped before `source`.
    pub fn add_borrow_dep(&mut self, borrower: LocalId, source: LocalId) {
        self.borrow_deps.entry(borrower).or_default().push(source);
    }

    /// Push a new drop scope.
    pub fn push_scope(&mut self, kind: DropScopeKind) {
        self.scopes.push(DropScope {
            kind,
            entries: Vec::new(),
        });
    }

    /// Pop the current drop scope and emit drops for all registered locals.
    /// Uses topological ordering if borrow dependencies exist.
    pub fn pop_scope(&mut self, builder: &mut FunctionBuilder, registry: &TypeRegistry) {
        if let Some(scope) = self.scopes.pop() {
            emit_scope_drops_ordered(builder, registry, &scope.entries, &self.borrow_deps);
        }
    }

    /// Pop the current drop scope WITHOUT emitting drops.
    /// Used when drops were already emitted via emit_early_exit_drops (e.g., explicit return).
    pub fn pop_scope_no_emit(&mut self) {
        self.scopes.pop();
    }

    /// Pop a `DropScopeKind::Statement` frame, dropping only GuardKind temps
    /// and re-registering every other droppable into the parent scope.
    ///
    /// Guard temps minted by `Mutex.lock` / `RWLock.read` / `RWLock.write`
    /// under an expression statement must release at statement end so a
    /// follow-up acquire on the same handle does not self-deadlock. Non-Guard
    /// droppables (String temps, collections, …) keep their enclosing
    /// Function/Block lifetime — re-registering them is required; skipping
    /// registration would leak.
    ///
    /// `is_guard` is the typed GuardKind predicate (typically
    /// `|tid| type_mapper.guard_kind(tid).is_some()`). No name-matching.
    ///
    /// Callers that already emitted drops via `emit_early_exit_drops` (the
    /// block is terminated) must use `pop_scope_no_emit` instead — same
    /// contract as Block scopes.
    pub fn pop_statement_guard_temps(
        &mut self,
        builder: &mut FunctionBuilder,
        registry: &TypeRegistry,
        is_guard: impl Fn(TypeId) -> bool,
    ) {
        let Some(scope) = self.scopes.pop() else {
            return;
        };
        debug_assert_eq!(
            scope.kind,
            DropScopeKind::Statement,
            "pop_statement_guard_temps expects a Statement scope"
        );

        // Partition in declaration order; emit guards LIFO; re-register the
        // rest into the parent (still in declaration order so later parent
        // LIFO drops preserve relative order among the re-homed entries).
        let mut guards: Vec<DropEntry> = Vec::new();
        let mut non_guards: Vec<DropEntry> = Vec::new();
        for entry in scope.entries {
            if is_guard(entry.type_id) {
                guards.push(entry);
            } else {
                non_guards.push(entry);
            }
        }

        if !guards.is_empty() {
            emit_scope_drops_ordered(builder, registry, &guards, &self.borrow_deps);
        }

        if !non_guards.is_empty() {
            if let Some(parent) = self.scopes.last_mut() {
                parent.entries.extend(non_guards);
            }
            // No parent: drop the entries on the floor only if the elaborator
            // is empty (should not happen mid-function). Prefer not to free
            // them here — that would invent a lifetime. debug path only.
            else {
                debug_assert!(
                    false,
                    "pop_statement_guard_temps: non-Guard temps with no parent scope"
                );
            }
        }
    }

    /// Register an owned (Move-type) local in the current scope.
    ///
    /// Only registers locals whose type has Move semantics — Copy types
    /// don't need dropping.
    pub fn register_local(&mut self, local: LocalId, type_id: TypeId, registry: &TypeRegistry) {
        if !needs_drop(type_id, registry) {
            return;
        }
        if let Some(scope) = self.scopes.last_mut() {
            scope.entries.push(DropEntry {
                local,
                type_id,
                maybe_moved: false,
                force_drop: false,
                owning_param: false,
            });
        }
    }

    /// Force-register for Option/Result with resource payloads. Bypasses needs_drop.
    pub fn register_local_unconditional(&mut self, local: LocalId, type_id: TypeId) {
        for scope in self.scopes.iter().rev() {
            if scope.entries.iter().any(|e| e.local == local) { return; }
        }
        if let Some(scope) = self.scopes.last_mut() {
            scope.entries.push(DropEntry {
                local, type_id, maybe_moved: true, force_drop: true,
                owning_param: false,
            });
        }
    }

    /// Register a Move-type local at the outermost (Function) scope, even if the current
    /// scope is a nested Block or Loop. Used for GorgetString temps that back Str views —
    /// they must survive until the function exits, not just the current block scope.
    /// Skips if the local is already registered in any scope (prevents double-registration).
    pub fn register_local_at_function_scope(&mut self, local: LocalId, type_id: TypeId, registry: &TypeRegistry) {
        if !needs_drop(type_id, registry) {
            return;
        }
        // Check if already registered in any scope
        for scope in &self.scopes {
            if scope.entries.iter().any(|e| e.local == local) {
                return;
            }
        }
        // Find the outermost Function scope
        for scope in self.scopes.iter_mut() {
            if scope.kind == DropScopeKind::Function {
                scope.entries.push(DropEntry {
                    local,
                    type_id,
                    maybe_moved: false,
                    force_drop: false,
                    owning_param: false,
                });
                return;
            }
        }
        // Fallback to current scope if no Function scope exists
        if let Some(scope) = self.scopes.last_mut() {
            scope.entries.push(DropEntry {
                local,
                type_id,
                maybe_moved: false,
                force_drop: false,
                owning_param: false,
            });
        }
    }

    /// Register a function parameter for drop at scope exit — **only** for Copy-semantics
    /// types that have a non-None drop strategy (e.g., Channel, Shared, Weak).
    ///
    /// Move-semantics parameters are already tracked via VarDecl/body mechanisms;
    /// registering them again would cause double-free.
    pub fn register_param(&mut self, local: LocalId, type_id: TypeId, registry: &TypeRegistry) {
        if !needs_param_drop(type_id, registry) {
            return;
        }
        if let Some(scope) = self.scopes.last_mut() {
            scope.entries.push(DropEntry {
                local,
                type_id,
                maybe_moved: false,
                force_drop: false,
                owning_param: false,
            });
        }
    }

    /// Register a `!`-sigil resource parameter for drop at function exit.
    /// The parameter slot holds a `MutPtr` (caller-supplied address), but the
    /// callee owns the pointee. Drop must dereference through the pointer; the
    /// emitted `DropIfAlive` carries a `Place { local, projections: [Deref] }`
    /// so the LIR lowering goes through the correct addr-load path and
    /// `lower_drop`'s `is_pure_borrow_for` Nop short-circuit (which only fires
    /// for empty-projection places) is bypassed.
    ///
    /// `type_id` is the BASE type (e.g. `R`), not the slot's `MutPtr<R>`.
    /// `DropIfAlive` is emitted unconditionally for owning-param entries (the
    /// drop-flag dataflow at the LIR layer controls firing — `Initialized`
    /// → unconditional drop, `MoveSlot` from inner `consume`/`push`/etc.
    /// flips the slot to `Uninitialized`/`MaybeInitialized` → drop guard
    /// strips/becomes a flag check). We do NOT set `maybe_moved=true` here:
    /// that flag also feeds `is_moved(local)`, which is consulted by other
    /// passes to skip post-call `MoveZero` emission. A `!`-param starts
    /// alive at function entry and should appear "live" to those passes.
    pub fn register_owning_param(&mut self, local: LocalId, type_id: TypeId, registry: &TypeRegistry) {
        if !needs_drop(type_id, registry) {
            return;
        }
        if let Some(scope) = self.scopes.last_mut() {
            scope.entries.push(DropEntry {
                local,
                type_id,
                maybe_moved: false,
                force_drop: false,
                owning_param: true,
            });
        }
    }

    /// Completely remove a local from drop tracking. Used when a GorgetString temp
    /// is consumed by a str view assignment — the view may escape the scope, so the
    /// GorgetString must NOT be freed (it will leak, same as pre-drop-registration).
    pub fn unregister(&mut self, local: LocalId) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(pos) = scope.entries.iter().position(|e| e.local == local) {
                scope.entries.remove(pos);
                return;
            }
        }
    }

    /// Check whether a local is registered for drop in any scope.
    pub fn is_registered(&self, local: LocalId) -> bool {
        self.scopes.iter().rev().any(|scope| {
            scope.entries.iter().any(|e| e.local == local)
        })
    }

    /// Check whether a local has been marked as "maybe moved".
    pub fn is_moved(&self, local: LocalId) -> bool {
        for scope in self.scopes.iter().rev() {
            for entry in &scope.entries {
                if entry.local == local {
                    return entry.maybe_moved;
                }
            }
        }
        false
    }

    /// Mark a local as "maybe moved" — future drops will use DropIfAlive.
    pub fn mark_moved(&mut self, local: LocalId) {
        for scope in self.scopes.iter_mut().rev() {
            for entry in &mut scope.entries {
                if entry.local == local {
                    entry.maybe_moved = true;
                    return;
                }
            }
        }
    }

    /// Clear the "maybe moved" flag on a local — typically called after a
    /// reassignment (`[Mv] _x = copy ...`) that gives the slot a fresh,
    /// owning value. Without this, a prior consume site (e.g. `vec.push(x)`
    /// in an earlier branch) leaves `maybe_moved=true` on the slot even
    /// though the slot has since been re-bound to a live value; downstream
    /// consume-site staging then treats the live local as already-moved
    /// and skips the required post-consume `move_zero`, causing a double
    /// free when the unconditional scope-exit `drop_if_alive` sees the
    /// non-null slot. Surfaced when AST-level last-use analysis began
    /// recognising `StructLiteral` arg identifiers as last uses (gorget-js
    /// snag #3, 2026-05-17).
    pub fn clear_moved(&mut self, local: LocalId) {
        for scope in self.scopes.iter_mut().rev() {
            for entry in &mut scope.entries {
                if entry.local == local {
                    entry.maybe_moved = false;
                    return;
                }
            }
        }
    }

    /// Snapshot all `maybe_moved` flags across every live scope.
    ///
    /// Used by branch lowerings (`lower_if`, `lower_match`, etc.) to restore the
    /// pre-branch view of which locals have been moved before lowering each
    /// alternative branch — without this, a `mark_moved(_6)` in the then-branch
    /// pollutes the elif-branch's view (`is_moved(_6) == true`), causing the
    /// elif-branch's `f.b = items` field-store to skip the required
    /// `move_zero _6` and leak the heap allocation through to scope-exit drop.
    /// See snag #8 (2026-05-05).
    pub fn snapshot_moved(&self) -> Vec<(usize, usize, bool)> {
        let mut out = Vec::new();
        for (sidx, scope) in self.scopes.iter().enumerate() {
            for (eidx, entry) in scope.entries.iter().enumerate() {
                out.push((sidx, eidx, entry.maybe_moved));
            }
        }
        out
    }

    /// Restore `maybe_moved` flags from a snapshot. Entries added since the
    /// snapshot keep their current `maybe_moved` (they're newer than the
    /// snapshot frame). Use after a branch finishes to drop branch-local
    /// move tracking before the next alternative branch.
    pub fn restore_moved(&mut self, snapshot: &[(usize, usize, bool)]) {
        for &(sidx, eidx, was_moved) in snapshot {
            if let Some(scope) = self.scopes.get_mut(sidx) {
                if let Some(entry) = scope.entries.get_mut(eidx) {
                    entry.maybe_moved = was_moved;
                }
            }
        }
    }

    /// Union the `maybe_moved` flags from a post-branch snapshot into the
    /// current state. Conservative join: if any branch moved a local, the
    /// post-join state treats it as maybe-moved (drops use DropIfAlive). Use
    /// after restoring + lowering each branch to merge their move sets.
    pub fn union_moved(&mut self, branch_snapshot: &[(usize, usize, bool)]) {
        for &(sidx, eidx, branch_moved) in branch_snapshot {
            if !branch_moved { continue; }
            if let Some(scope) = self.scopes.get_mut(sidx) {
                if let Some(entry) = scope.entries.get_mut(eidx) {
                    entry.maybe_moved = true;
                }
            }
        }
    }

    /// Update the recorded type for a local after type re-inference.
    ///
    /// Called in VarDecl when `auto` or closure types are re-inferred after lowering
    /// the RHS: the initial registration used the pre-inference type (often I64_TYPE),
    /// but now we know the real type. If the local wasn't yet registered (because the
    /// original type had no drop semantics), this registers it fresh.
    pub fn update_or_register_type(&mut self, local: LocalId, type_id: TypeId, registry: &TypeRegistry) {
        // Try to find and update an existing entry
        for scope in self.scopes.iter_mut().rev() {
            for entry in &mut scope.entries {
                if entry.local == local {
                    entry.type_id = type_id;
                    return;
                }
            }
        }
        // Not yet registered — register at function scope. This handles the case
        // where a local was originally Ptr (no drop needed) but got upgraded to an
        // owned type by CoW materialization inside a loop body. Registering at the
        // current (Loop) scope would incorrectly drop it at each iteration end.
        self.register_local_at_function_scope(local, type_id, registry);
    }

    /// Emit cleanup drops for an early exit (return, break, continue).
    ///
    /// Drops all locals from the current scope back to (and excluding)
    /// the target scope kind. For `return`, drops everything up to Function.
    /// For `break`/`continue`, drops up to the enclosing Loop.
    ///
    /// `exclude` optionally skips a local (e.g., the return value being moved out).
    pub fn emit_early_exit_drops(
        &self,
        builder: &mut FunctionBuilder,
        registry: &TypeRegistry,
        target: DropScopeKind,
        exclude: Option<LocalId>,
    ) {
        // Walk scopes from innermost to outermost
        for scope in self.scopes.iter().rev() {
            emit_scope_drops_excluding(builder, registry, &scope.entries, exclude);
            if scope.kind == target {
                break;
            }
        }
    }

    /// Check if there are any active scopes.
    pub fn has_scopes(&self) -> bool {
        !self.scopes.is_empty()
    }
}

/// Check whether a Copy-semantics type needs dropping when passed as a function parameter.
/// Delegates to `TypeRegistry::needs_param_drop()`.
fn needs_param_drop(type_id: TypeId, registry: &TypeRegistry) -> bool {
    registry.needs_param_drop(type_id)
}

/// Check whether a type needs dropping based on its metadata.
/// Delegates to `TypeRegistry::needs_drop()`.
fn needs_drop(type_id: TypeId, registry: &TypeRegistry) -> bool {
    registry.needs_drop(type_id)
}

/// Emit Drop/DropIfAlive for a list of entries in LIFO order.
fn emit_scope_drops(
    builder: &mut FunctionBuilder,
    registry: &TypeRegistry,
    entries: &[DropEntry],
) {
    emit_scope_drops_excluding(builder, registry, entries, None);
}

/// Emit drops with borrow-aware topological ordering.
/// Borrowers are dropped before their sources to prevent use-after-free.
fn emit_scope_drops_ordered(
    builder: &mut FunctionBuilder,
    registry: &TypeRegistry,
    entries: &[DropEntry],
    borrow_deps: &rustc_hash::FxHashMap<LocalId, Vec<LocalId>>,
) {
    if entries.is_empty() { return; }

    // Build index: LocalId → position in entries
    let local_to_idx: rustc_hash::FxHashMap<LocalId, usize> = entries.iter()
        .enumerate()
        .map(|(i, e)| (e.local, i))
        .collect();

    // Check if any borrow deps touch entries in this scope
    let mut has_constraints = false;
    let n = entries.len();
    let mut must_precede: Vec<Vec<usize>> = vec![Vec::new(); n];
    let mut in_degree = vec![0u32; n];

    for (i, entry) in entries.iter().enumerate() {
        if let Some(sources) = borrow_deps.get(&entry.local) {
            for source_local in sources {
                if let Some(&j) = local_to_idx.get(source_local) {
                    if i != j {
                        // entry[i] borrows from entry[j]
                        // => drop entry[i] before entry[j]
                        // => i must come before j in drop order
                        must_precede[i].push(j);
                        in_degree[j] += 1;
                        has_constraints = true;
                    }
                }
            }
        }
    }

    if !has_constraints {
        // Fast path: plain LIFO (no borrow constraints in this scope)
        emit_scope_drops(builder, registry, entries);
        return;
    }

    // Topological sort (Kahn's algorithm) with LIFO tiebreaker.
    // Use a max-heap so later-declared entries (higher index) get priority
    // when there's no constraint forcing a different order.
    let mut heap = std::collections::BinaryHeap::new();
    for i in 0..n {
        if in_degree[i] == 0 {
            heap.push(i);
        }
    }

    let mut order = Vec::with_capacity(n);
    while let Some(i) = heap.pop() {
        order.push(i);
        for &j in &must_precede[i] {
            in_degree[j] -= 1;
            if in_degree[j] == 0 {
                heap.push(j);
            }
        }
    }

    // Append any remaining (cycle — shouldn't happen with well-formed borrows)
    if order.len() < n {
        for i in (0..n).rev() {
            if !order.contains(&i) {
                order.push(i);
            }
        }
    }

    // Emit drops in computed order
    for &idx in &order {
        let entry = &entries[idx];
        if !entry.force_drop && !needs_drop(entry.type_id, registry) { continue; }
        let place = drop_place_for(entry);
        // Owning-`!`-param entries always use DropIfAlive — the LIR drop-flag
        // dataflow controls whether the drop actually fires (suppressed when
        // the body emitted a `MoveZero` on the param slot).
        // Defensive: always emit `DropIfAlive`. The LIR `drop_elab` pass
        // statically elides the runtime check when slot init is provably
        // unconditional, so we don't lose codegen quality. Snag #30
        // (2026-05-10): the `maybe_moved` tracking across nested matches +
        // early-return paths produced a false negative — `_11.maybe_moved`
        // was true at the join point but the bb8 (None-arm) drop emission
        // saw it as false, producing unconditional `drop _11` and a
        // double-free. Always-conditional drop is the safe contract; the
        // optimizer recovers the unconditional shape when flow proves it.
        let _ = entry.maybe_moved; // kept for future invariant audits
        let _ = entry.owning_param;
        builder.drop_if_alive(place);
    }
}

/// Emit Drop/DropIfAlive for a list of entries in LIFO order,
/// optionally excluding a specific local (e.g., the return value being moved out).
fn emit_scope_drops_excluding(
    builder: &mut FunctionBuilder,
    registry: &TypeRegistry,
    entries: &[DropEntry],
    exclude: Option<LocalId>,
) {
    for entry in entries.iter().rev() {
        // Skip excluded local (return value being moved out)
        if let Some(excl) = exclude {
            if entry.local == excl {
                continue;
            }
        }
        // Verify the type still needs dropping (defense-in-depth).
        if !entry.force_drop && !needs_drop(entry.type_id, registry) {
            continue;
        }

        let place = drop_place_for(entry);
        // Defensive: always emit `DropIfAlive`. The LIR `drop_elab` pass
        // statically elides the runtime check when slot init is provably
        // unconditional, so we don't lose codegen quality. Snag #30
        // (2026-05-10): the `maybe_moved` tracking across nested matches +
        // early-return paths produced a false negative — `_11.maybe_moved`
        // was true at the join point but the bb8 (None-arm) drop emission
        // saw it as false, producing unconditional `drop _11` and a
        // double-free. Always-conditional drop is the safe contract; the
        // optimizer recovers the unconditional shape when flow proves it.
        let _ = entry.maybe_moved; // kept for future invariant audits
        let _ = entry.owning_param;
        builder.drop_if_alive(place);
    }
}

/// Build the GIR `Place` to emit for a drop entry. Owning-`!`-param entries
/// emit `*local` (Deref projection) so the LIR drop lowering goes through
/// the addr-load path that resolves to the underlying resource type — the
/// `is_pure_borrow_for` Nop in `lir/lower/drops.rs` only fires for
/// empty-projection places, so the explicit Deref opt-in keeps that
/// soundness check in place for genuine `&` borrows. Other entries emit
/// the bare `local` place.
fn drop_place_for(entry: &DropEntry) -> Place {
    if entry.owning_param {
        Place {
            local: entry.local,
            projections: vec![crate::ir::instructions::Projection::Deref],
        }
    } else {
        Place::local(entry.local)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::builder::FunctionBuilder;
    use crate::ir::instructions::Instruction;

    fn make_move_registry() -> TypeRegistry {
        let mut reg = TypeRegistry::new();
        // Register a Move type with Trivial drop
        reg.add_type_def(TypeDef {
            name: "OwnedString".into(),
            kind: TypeDefKind::Struct(StructDef {
                fields: vec![
                    StructField { name: "data".into(), type_id: U8_TYPE },
                    StructField { name: "len".into(), type_id: U64_TYPE },
                ],
            }),
            metadata: TypeMetadata {
                size: Some(16),
                align: Some(8),
                drop_strategy: DropStrategy::Trivial("gorget_string_free".into()),
                copy_semantics: CopySemantics::Resource,
                ..Default::default()
            },
        });
        reg.insert(GirType::Named("OwnedString".into()));
        reg
    }

    #[test]
    fn needs_drop_primitives() {
        let reg = TypeRegistry::new();
        assert!(!needs_drop(I64_TYPE, &reg));
        assert!(!needs_drop(BOOL_TYPE, &reg));
        assert!(!needs_drop(F64_TYPE, &reg));
        assert!(!needs_drop(UNIT_TYPE, &reg));
    }

    #[test]
    fn needs_drop_move_type() {
        let reg = make_move_registry();
        let owned_string_id = TypeId(12); // first inserted after primitives
        assert!(needs_drop(owned_string_id, &reg));
    }

    #[test]
    fn drop_elaborator_scope_lifecycle() {
        let reg = make_move_registry();
        let owned_string_id = TypeId(12);

        let mut elab = DropElaborator::new();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        // Function scope
        elab.push_scope(DropScopeKind::Function);
        elab.register_local(LocalId(1), I64_TYPE, &reg); // Copy — not registered
        let s_local = builder.add_local(owned_string_id, Some("s"));
        elab.register_local(s_local, owned_string_id, &reg); // Move — registered

        // Pop scope should emit DropIfAlive for owned_string (Snag #30:
        // all drops emit DropIfAlive defensively; LIR drop_elab elides
        // when slot init is provably unconditional).
        elab.pop_scope(&mut builder, &reg);

        let block = &builder.blocks[0];
        assert!(
            block.instructions.iter().any(|inst| matches!(inst, Instruction::DropIfAlive { .. })),
            "Should emit DropIfAlive instruction for Move-type local"
        );
    }

    #[test]
    fn drop_elaborator_maybe_moved() {
        let reg = make_move_registry();
        let owned_string_id = TypeId(12);

        let mut elab = DropElaborator::new();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        elab.push_scope(DropScopeKind::Function);
        let s_local = builder.add_local(owned_string_id, Some("s"));
        elab.register_local(s_local, owned_string_id, &reg);

        // Mark as moved
        elab.mark_moved(s_local);

        // Pop scope should emit DropIfAlive instead of Drop
        elab.pop_scope(&mut builder, &reg);

        let block = &builder.blocks[0];
        assert!(
            block.instructions.iter().any(|inst| matches!(inst, Instruction::DropIfAlive { .. })),
            "Should emit DropIfAlive for maybe-moved local"
        );
        assert!(
            !block.instructions.iter().any(|inst| matches!(inst, Instruction::Drop { .. })),
            "Should NOT emit plain Drop for maybe-moved local"
        );
    }

    #[test]
    fn drop_elaborator_nested_scopes() {
        let reg = make_move_registry();
        let owned_string_id = TypeId(12);

        let mut elab = DropElaborator::new();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        // Function scope with one owned local
        elab.push_scope(DropScopeKind::Function);
        let s1 = builder.add_local(owned_string_id, Some("s1"));
        elab.register_local(s1, owned_string_id, &reg);

        // Inner block scope with another owned local
        elab.push_scope(DropScopeKind::Block);
        let s2 = builder.add_local(owned_string_id, Some("s2"));
        elab.register_local(s2, owned_string_id, &reg);

        // Pop inner scope — should drop s2 (DropIfAlive after Snag #30 fix)
        elab.pop_scope(&mut builder, &reg);

        let drop_count_1 = builder.blocks[0].instructions.iter()
            .filter(|inst| matches!(inst, Instruction::DropIfAlive { .. }))
            .count();
        assert_eq!(drop_count_1, 1, "Should DropIfAlive s2 from inner scope");

        // Pop outer scope — should drop s1
        elab.pop_scope(&mut builder, &reg);

        let drop_count_2 = builder.blocks[0].instructions.iter()
            .filter(|inst| matches!(inst, Instruction::DropIfAlive { .. }))
            .count();
        assert_eq!(drop_count_2, 2, "Should DropIfAlive both s1 and s2 total");
    }

    #[test]
    fn drop_elaborator_early_return() {
        let reg = make_move_registry();
        let owned_string_id = TypeId(12);

        let mut elab = DropElaborator::new();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        elab.push_scope(DropScopeKind::Function);
        let s1 = builder.add_local(owned_string_id, Some("s1"));
        elab.register_local(s1, owned_string_id, &reg);

        elab.push_scope(DropScopeKind::Block);
        let s2 = builder.add_local(owned_string_id, Some("s2"));
        elab.register_local(s2, owned_string_id, &reg);

        // Early return — should drop s2 (Block) + s1 (Function); DropIfAlive
        // after Snag #30 defensive change.
        elab.emit_early_exit_drops(&mut builder, &reg, DropScopeKind::Function, None);

        let drop_count = builder.blocks[0].instructions.iter()
            .filter(|inst| matches!(inst, Instruction::DropIfAlive { .. }))
            .count();
        assert_eq!(drop_count, 2, "Early return should DropIfAlive both s2 and s1");
    }

    #[test]
    fn copy_types_not_registered() {
        let mut reg = TypeRegistry::new();
        // Register a Copy type (no drop needed)
        reg.add_type_def(TypeDef {
            name: "Point".into(),
            kind: TypeDefKind::Struct(StructDef {
                fields: vec![
                    StructField { name: "x".into(), type_id: F64_TYPE },
                    StructField { name: "y".into(), type_id: F64_TYPE },
                ],
            }),
            metadata: TypeMetadata::default(), // Copy, DropStrategy::None
        });
        let point_id = reg.insert(GirType::Named("Point".into()));

        let mut elab = DropElaborator::new();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        elab.push_scope(DropScopeKind::Function);
        let p = builder.add_local(point_id, Some("p"));
        elab.register_local(p, point_id, &reg);

        // Pop scope — should NOT emit any drops for Copy type
        elab.pop_scope(&mut builder, &reg);

        let drop_count = builder.blocks[0].instructions.iter()
            .filter(|inst| matches!(inst, Instruction::Drop { .. } | Instruction::DropIfAlive { .. }))
            .count();
        assert_eq!(drop_count, 0, "Should not drop Copy types");
    }

    /// Statement scope drops only GuardKind entries and re-homes non-Guard
    /// droppables into the parent (Round XIX Track Y).
    #[test]
    fn drop_elaborator_statement_guard_temps() {
        let mut reg = make_move_registry();
        let owned_string_id = TypeId(12);
        // Synthetic "guard" type that needs drop (mirrors Guard[T] Resource+Trivial).
        reg.add_type_def(TypeDef {
            name: "FakeGuard".into(),
            kind: TypeDefKind::Struct(StructDef {
                fields: vec![StructField {
                    name: "lock".into(),
                    type_id: U64_TYPE,
                }],
            }),
            metadata: TypeMetadata {
                size: Some(8),
                align: Some(8),
                drop_strategy: DropStrategy::Trivial("FakeGuard__drop".into()),
                copy_semantics: CopySemantics::Resource,
                ..Default::default()
            },
        });
        let guard_id = reg.insert(GirType::Named("FakeGuard".into()));

        let mut elab = DropElaborator::new();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        elab.push_scope(DropScopeKind::Function);
        elab.push_scope(DropScopeKind::Statement);
        let g = builder.add_local(guard_id, Some("g"));
        let s = builder.add_local(owned_string_id, Some("s"));
        elab.register_local(g, guard_id, &reg);
        elab.register_local(s, owned_string_id, &reg);

        // Only the guard type is "is_guard" — string re-homes to Function.
        elab.pop_statement_guard_temps(&mut builder, &reg, |tid| tid == guard_id);

        let drop_count = builder.blocks[0]
            .instructions
            .iter()
            .filter(|inst| matches!(inst, Instruction::DropIfAlive { .. }))
            .count();
        assert_eq!(drop_count, 1, "Statement pop drops only the GuardKind temp");

        // Non-guard still registered on Function — will drop on function pop.
        assert!(elab.is_registered(s), "non-Guard temp re-registered into parent");
        assert!(
            !elab.is_registered(g),
            "GuardKind temp consumed by statement-end drop"
        );

        elab.pop_scope(&mut builder, &reg);
        let drop_count_2 = builder.blocks[0]
            .instructions
            .iter()
            .filter(|inst| matches!(inst, Instruction::DropIfAlive { .. }))
            .count();
        assert_eq!(
            drop_count_2, 2,
            "Function pop drops the re-homed non-Guard temp"
        );
    }
}
