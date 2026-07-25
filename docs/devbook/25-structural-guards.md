# 25 — Structural guards

This chapter is about the *writer-side validators* that turn Gorget's soundness invariants into assertions: pure structural walks over an IR/LIR module that run after lowering, report no error when the invariant holds, and (once a class is migrated) halt the build on the first violation. The mechanism lives in three files — one per IR layer — plus a small env-gated migration framework:

- `src/ir/validate.rs` — the GIR validator (structural well-formedness + the resource/consume-site soundness guards).
- `src/lir/validate.rs` — the LIR validator (SSA/CFG invariants + drop-table completeness + Box/resource-shape guards), driven by a per-pass `VALIDATORS` registry.
- `src/bir/validate.rs` — the BIR validator (asserts every instruction is a primitive — covered in depth in [Chapter 16](16-bir.md); this chapter treats it as one instance of the framework).

The design narrative folded here comes from the former `structural-guards.md` and `unified-resource-model.md` §5 (Phase C) / §8.3 (Tier E) deep-dives. All figures and line numbers below were re-derived from current source.

## What a structural guard is, and why

A soundness invariant that is documented but unchecked is a regression waiting to happen. The bar Gorget sets:

> Every soundness invariant is a writer-side static guard. Every guard is fatal once migrated. Every bug fixed leaves a permanent counterexample and a validator that locks the class shut.

A guard is *not* "a test that covers this case." It is "we fail the build if the invariant is ever violated, in any input, anywhere." The validator runs over the module structure after lowering and checks the property a human reviewer would check by reading the lowering code — but mechanically, on every program, every time. The cost is a structural walk: no codegen, no runtime cost.

This is the enforcement arm of the layering discipline ([Chapter 02](02-foundations.md), [Chapter 24](24-layering-discipline.md)). When layering-discipline says "if a downstream pass reconstructs information from names, the boundary upstream was drawn wrong," a structural guard is what makes that statement load-bearing: without the guard the claim is editorial; with it the build fails until the boundary is fixed.

## The three concrete validators at a glance

The three layers expose their validators with deliberately different shapes, because each runs in a different place.

| Layer | Entry point | Returns | Driven by | Fatal where |
|-------|-------------|---------|-----------|-------------|
| GIR | `validate(&Module)` plus a family of `validate_*` fns | `Vec<ValidationError>` / `Vec<ResourceMoveWarning>` / etc. | called explicitly from the GIR lowering driver | `src/ir/lowering/mod.rs` (one `panic!` per class) |
| LIR | `validate_module(&LirModule)` + the `VALIDATORS` registry | `Vec<LirError>` | `assert_module_valid` after every pass | `run_validators` panics |
| BIR | `assert_primitives_only(&LirModule)` | `Result<(), BirError>` | `BirModule::from_lir` | `?`-propagated `Err` |

### GIR — `src/ir/validate.rs`

The GIR validator splits into two tiers of its own.

**Structural well-formedness** is the top-level `validate` (`src/ir/validate.rs:108`). It runs duplicate-name checks (`check_duplicate_functions`/`_type_names`/`_globals`), `check_drop_metadata_consistency`, then per-function walks (`check_function`, `:169`): every block has a terminator, the `span_map` length matches the instruction count, every `LocalId`/`BlockId`/call target is in range, `StructInit`/`EnumInit` field counts match the `TypeDef`, the return place `_0`'s type matches `func.return_type`, all local `TypeId`s resolve, `Drop`/`DropIfAlive` target a type that actually needs dropping (`check_drop_targets`, `:528`), and an intra-block use-after-`MoveZero` scan (`check_use_after_move`, `:653`). The error kinds are the `ValidationErrorKind` enum (`:21`). This is called once from the driver at `src/ir/lowering/mod.rs:1594`; all errors are fatal *except* `EnumFieldCountMismatch`, which is downgraded to a warning because cross-module imported enums can legitimately disagree on `Vector[T]` field counts (`mod.rs:1600`).

**Resource/ownership soundness** is the Phase C / Tier 1-2 family. These are separate `pub fn`s returning richer warning types so the migration framework (below) can gate them independently. The key ones:

- `validate_resource_sites_all` (`:1066`) — the consolidated Phase C walker. It does a *single* pass per function and partitions findings into five buckets (`ResourceSiteFindings`, `:1021`): Assign-Copy, Call/CallExtern args, IndexLoad, EnumFieldLoad, FieldLoad. Pre-consolidation these were five back-to-back full-module walks (~30% of the `gir_lower` phase on a large workload); the collapse runs one walk.
- `validate_move_follow_through` (`:1761`) — Tier 1b: a `Move`-mode assign of a drop-registered source must be followed by a `MoveZero` of that source in the same block before any subsequent drop.
- `validate_type_metadata_coherence` (Tier 1c), `validate_consume_sites` (Tier 2a), `validate_drop_pre_rebind` (Tier 2c) — the tiered ownership invariants (detailed in "The backlog" below).
- `validate_no_null_assign_to_option_slot` (`:2188`) — the separate Snag #32-family gate (no tier): it flags an `Assign` of `Constant::Null` into a tagged-enum slot (`Option__T` / `Result__T__E`), which the C backend would render as a 40-byte zero-store that, under the `Some=0 / None=1` discriminator layout, silently produces a `Some(empty payload)` zombie. The lowering writer must materialise the variant via `coerce_null_to_option_none` / `materialise_none_for_expected_type` first.

All of these are wired as separate fatal gates in the driver, each with its own `panic!` and (for several) an env-var diagnostic path — see `src/ir/lowering/mod.rs:1575`, `:1629`, `:1724`, `:1758`, `:1813`, `:1897`.

#### The unified resource-read rule

The five resource-read classes share exactly one rule. Each per-instruction walker extracts a typed `ReadSite` (`:1317`) describing the conceptual read — its `ReadMode`, the source type, and per-class metadata (`ReadSiteClass`, `:1332`) — and routes it through the single predicate `validate_read` (`:1367`):

```rust
fn validate_read(site: ReadSite<'_>, registry: &TypeRegistry) -> Option<ResourceMoveWarning> {
    if !registry.is_resource_type(site.source_ty) { return None; }
    match site.mode {
        ReadMode::Borrow | ReadMode::Move | ReadMode::Clone => None,
        ReadMode::Copy => Some(ResourceMoveWarning { /* ... */ }),
    }
}
```

A `Copy` of a resource-typed value is a shallow alias of an owned resource — a latent double-free or aliased-mutable-state bug; the CoW contract ([Chapter 11](11-copy-on-write.md)) mandates Move / Clone / Borrow at every such site. Adding a future read class is a one-shot extension: add a `ReadSiteClass` variant, register one extractor in `for_each_read_site` (`:1419`), and the rule itself is untouched. The mode is *synthesised* per instruction shape — e.g. a `Ptr`-typed FieldLoad dst is borrow-shaped (`:1442`); a FieldLoad immediately followed by a `MoveZero` of the same field is the `!self` consuming-self idiom and reads as Move (`next_inst_zeroes_field`, `:1675`); an `EnumFieldLoad` of a resource payload is Move because the LIR auto-zeros the source field after extraction (`:1502`). These syntheses are the validator mirroring the lowering's actual behavior — if the lowering changes, the synthesis must too.

The Assign class doesn't fit `for_each_read_site`'s instruction-discriminant match (it peeks at the dst's projections and the source's ownership tag), so it has its own extractor `assign_read_site` (`:1119`), which folds in all the legitimate skips — non-Copy modes, projected dsts, constant sources, self-assigns, auto-deref `dst:T = copy src:Ptr<T>`, cross-type generic-mono noise, and `Borrowed`/`View`-ownership sources (`:1176`) — before handing the same shallow-copy shape to `validate_read`.

### LIR — `src/lir/validate.rs`

The LIR validator is registry-driven. `assert_module_valid(module, after)` (`:112`) runs after every LIR pass in debug builds, or in release when `GG_VALIDATE_PASSES` is set, and panics (via the `#[cold]` `run_validators`, `:125`) with a message naming the pass that just ran. The set of checks is the `VALIDATORS` constant (`:98`):

```rust
const VALIDATORS: &[ValidatorFn] = &[validate_module, validate_box_inner_type,
    validate_box_inner_type_consistency, validate_drop_completeness,
    validate_drop_fn_presence, validate_resource_arity];
```

`validate_module` (`:39`) itself bundles the structural and SSA/CFG checks:

- **Structural** (`validate_function`, `:142`): sequential block IDs, no duplicate value definitions, in-range slot/struct/field references (`check_slot_refs`/`check_struct_refs`), `CallRuntime` arg count matches the `RuntimeFn` signature (`check_runtime_call`, `:240`), `CallByRef.fref` references a defined value (`check_call_by_ref`), terminator targets in range with matching block-param/jump-arg counts (`validate_terminator`).
- **CFG/SSA** (Tier E §8.2): `check_no_critical_edges` (`:440`) — no edge from a multi-successor block to a multi-predecessor block; `check_reducible_cfg` (`:485`) — every back-edge target dominates its source (so a structured-CFG/WASM backend can assume reducibility); `validate_ssa_dominance` (`:605`) — every value use is dominated by its definition, using the Cooper-Harvey-Kennedy iterative dominator algorithm. SSA invariants are required post-`construct_ssa`; the critical-edge invariant is also required pre-SSA so Braun-et-al. SSA construction has what it needs.

The shape-soundness guards in the registry are the LIR-layer half of the Tier 1 invariants:

- `validate_drop_completeness` (`:896`, Tier 1a): for every type with a registered `module.type_drop_fns` entry, every *droppable* struct field (or enum-variant payload) appears in `field_drops` / `enum_variants`. "Droppable" is decided LIR-locally (`is_droppable_type`, `:899`): the field's struct has its own drop-fn entry, is a runtime resource (`GorgetString`/`GorgetArray`/`GorgetMap`/`GorgetSet`/`GorgetClosure`) directly or via `c_runtime_alias`, or carries `box_inner_type` / `is_trait_box`. A missing entry means a scope-exit drop leaks the field.
- `validate_drop_fn_presence` (`:1013`, Tier 1a inverse): the forward validator only walks entries *in* `type_drop_fns` and can't see structs that *should* have one but don't. This walks `module.structs` and asserts every `StructDef` flagged `expects_drop_fn` (set at the populator when GIR strategy is `Recursive`/`Custom`) has a matching entry. Catches the silent-skip class where a Recursive struct's field walk emerged empty.
- `validate_box_inner_type` (`:788`, Tier 1d) and its inverse `validate_box_inner_type_consistency` (`:855`): every regular `Box__` `StructDef` (single `_0` field, `is_trait_box == false`) must carry `box_inner_type: Some(suffix)` matching its name mangling; and conversely no non-`Box__` struct may carry stray `box_inner_type`. The C backend scans this field to emit the per-type `Box__<inner>__drop` / `__gorget_box_alloc_<inner>` symbols; missing or stray metadata link-fails at runtime (snag #13's family).
- `validate_resource_arity` (`:1049`): every `LirType::Resource { kind, params }` must have `params.len() == LirType::expected_resource_arity(kind)`, recursing into nested resource params. Catches a constructor that populates a resource shape with the wrong element-type count.

`validate_box_inner_type` is the canonical illustration of the *registrar-boundary exception* to the no-name-matching rule: it legitimately matches the `Box__` name prefix because the validator *is* the registrar-side check, and the name is read once at the recognition step, not used to drive a downstream semantic decision (`:768` documents this explicitly).

### BIR — `src/bir/validate.rs`

The BIR validator is the simplest instance, and the only one phrased as a graduation check rather than a soundness gate. `assert_primitives_only` (`:36`) walks every instruction and asserts none is a *canonical* (high-level) op — anything that should have been expanded by `bir::lower`. It is run at the end of `BirModule::from_lir` (`src/bir/mod.rs:79`) and its `Err(BirError::UnloweredCanonicalOp { fn_name, block_id, opcode })` propagates via `?`.

Its match has exactly two kinds of arm (`check_inst`, `:47`): an explicit arm per canonical op returning the error (`SizeOf`, `EnumInit`, `EnumCheck`, `EnumExtract`, `StructInit`, `CowClone`, `TraitCall`, `HofExpand`, `AddressOf`, `BoxAlloc`, `CollectionCtor`), and a catch-all `_ => Ok(())` treating everything else as a primitive. This makes the maintenance cost asymmetric by design: adding a new *primitive* requires zero validator changes; adding a new *canonical op* requires exactly one arm; *deleting* an arm (after writing its expansion in `bir::lower`) is how an op graduates from "must lower" to "no longer a valid LIR op." Full context for BIR's role is in [Chapter 16](16-bir.md).

## The migration framework

A guard rarely ships fatal on day one — the codebase usually has existing violations. Phase C established the standard burn-down pattern that every new guard reuses:

1. **Write the validator** — a pure structural walk returning a `Vec` of typed warnings, each carrying function/block/instruction-index/type-name context.
2. **Gate behind an env var** — `GG_VALIDATE_<NAME>=<log-path>` writes per-class violation counts and detail to the log; the build does *not* panic. A sweep over the integration fixtures reveals the migration size.
3. **File a TODO** tracking the burn-down by class.
4. **Migrate** one class at a time — each commit drops one class's count to zero while the integration sweep stays green. Classes are independent, so different contributors can migrate them in parallel.
5. **Promote to fatal** once a class's count is zero — replace the env-gated write with an unconditional `panic!`. The class is permanently closed.
6. **Move the TODO to DONE.md** with the commit chain.

In current source you can see this pattern crystallized in the GIR driver: each resource-site bucket has its own fatal `panic!` (`src/ir/lowering/mod.rs:1640`, `:1647`, `:1654`, `:1661`, `:1668`), while `validate_drop_pre_rebind` keeps an env-gated diagnostic path (`GG_VALIDATE_DROP_PRE_REBIND`, `mod.rs:1763`) *and* the fatal panic (`:1800`), and `validate_consume_sites` does the same with `GG_VALIDATE_CONSUME_SITES` (`:1941`, `:1982`). The LIR side uses the `assert_module_valid`/`GG_VALIDATE_PASSES` switch (`src/lir/validate.rs:112`) as its gate instead of per-validator env vars, because its validators are cheap registry entries run on every pass.

A growing family of companion guards lives as Rust lint tests in `tests/lints.rs` rather than IR walks, because what they police is *the source tree itself* — the live roster is that file's `#[test]` list (any count quoted here would rot). The founding members were a sidecar-absence lint (no parallel `HashMap` duplicating a typed metadata axis) and two ratchets locking the current count of name-prefix routing sites and Phase-D proxy reads; later additions follow the same two shapes — exact-set enumeration guards (e.g. the #37 lazy-CoW view-producer enumeration, below) and count budgets — so new violations fail CI without forcing an immediate full migration.

## The backlog and its tiers

The former `structural-guards.md` deep-dive organized the invariants into three tiers; that doc's per-item status claims are historical and should be treated as presumed-stale (the live status is whatever the validator's wiring in source says today). The tiering itself is the evergreen part:

- **Tier 1 — invariants with known violations, concrete burn-downs.** Drop completeness (1a, LIR + its GIR counterpart 1c), move follow-through (1b), Box-inner-type completeness (1d). Tiers 1a and 1c are *distinct* invariants at different layers that compose: 1c (`validate_type_metadata_coherence`, GIR) locks the GIR→LIR handoff by checking a `TypeDef`'s recorded `(drop_strategy, copy_semantics)` matches a fresh transitive field walk; 1a (`validate_drop_completeness`, LIR) locks the LIR→C-emit handoff by checking the populated drop table reaches every droppable field. Both must hold for the snag #24 leak class to stay shut.
- **Tier 2 — invariants we should have.** CoW consume-site discipline (2a, `validate_consume_sites`: the source's IR mode must match its typed `LocalOwnership` at every consuming position — the classes its walker actually emits are `StructInit`, `EnumInit`, `CollectionMutator` (push/put/insert/send), `CallByValueArg`, `CallExternByValueArg`, and `AssignIntoOwnedSlot`, per the table in `CLAUDE.md`'s *Ownership at Consuming Positions*). A `ConsumeSiteClass::BoxNew` variant is *defined* (`:2277`) but currently never constructed by the walker; Box.new ownership is policed instead by the 2c drop-pre-rebind validator. Drop-pre-rebind correctness (2c, `validate_drop_pre_rebind`: a heap-allocating shallow-copy consumer's source — driven by the typed `Module::heap_alloc_consumer_externs` set populated at each Box.new emission — must be `MoveZero`'d before any later drop); sidecar absence (2d, the `tests/lints.rs` lint). Tier 2b (match-scrutinee discipline) is subsumed — it is the same shape that Phase C's `validate_resource_moves` already catches.
  The walker's `Instruction::Assign` arm carries a seventh class, `AssignIntoReturnSlot`, for a store into the function RETURN PLACE. It is structurally the same consume as `AssignIntoOwnedSlot` — the CALLER will drop what the slot holds — but it was invisible to that class, because `_0` is minted `Untracked` and the `AssignIntoOwnedSlot` gate accepts only `Owned | FreshOwned`. That gap is why an entire family of return-borrow double-frees walked past an always-fatal validator unseen: the walker *visited* the store; the *gate* dropped it. The predicate lives at the validator rather than in a writer that tags `_0`, because `_0`-is-the-return-place is a structural IR invariant of `FunctionBuilder::new`, whereas mutating the default tag would silently no-op the `set_ref(LocalId(0))` on the return path's Ptr-propagation leg — leaving `_0` `Owned` while it holds a borrowed pointer, i.e. a fresh double-free injected by the guard itself.

  The lesson generalizes: **when a validator misses a class, ask whether the walker never looked or whether a gate discarded the finding.** A new enum variant fixes the first; only the gate fixes the second.

- **Tier 3 — discipline meta-invariants** policing the design rules themselves: the no-name-matching ratchet, the Phase-D state-coherence ratchet, and the two convergence meters — `ratchet_b_materialize_site_count` (direct calls to the mutation-root materialize helpers) and `ratchet_c_handrolled_materialize_bypass_count` (clone emissions in `src/ir/lowering/**` outside `context.rs`, where the shared boundary chokepoints live) — all in `tests/lints.rs`. A convergence meter needs a **countable predicate named in its own doc-comment**, plus the counted set enumerated there: a meter over the *absence* of routing has no token to count, so an executor-invented predicate would quietly become the campaign's whole convergence claim.
- **View-producer enumeration (#37 lazy-CoW).** *Class:* a new cap=0 `Str` view producer shipping without a GIR materialize hook (W3a-W3d) is a use-after-free generator under the lazy-CoW default, and the class is proven ASan-blind — three producer routes (runtime-`.c` caller, backend-synthetic `.rs` emitter, direct struct-literal manufacture) were each missed by an earlier prose enumeration rule. *Guard:* three lints in `tests/lints.rs` — `str_view_producer_enumeration_is_closed` (exact-set, four arms: runtime-C callers, `.rs` emitter allowlist, `sig(`-never-`sig_fresh(` registry reconciliation, `returns_view` GIR-axis routing), `no_growth_in_lir_view_callee_rewrites` (budget over view-callee mentions in `src/lir` — the IndexLoad-rewrite class), `no_growth_in_runtime_c_direct_view_manufacture` (budget over raw `{ .data = ..., .cap = 0 }` literals — the `gorget_string_borrow_view` bypass route). *Escalation state:* FATAL from day one — no env-gate burn-down was needed because the enumeration is closed at introduction (zero pre-existing violations). The honest residual (dynamic callee names, dominance-breaking moves, budget-slot reuse, `src/backend` callee rewrites) stays prose in [Chapter 11](11-copy-on-write.md)'s enumeration-rule section.
- **LLVM entry-block alloca hoisting.** *Class:* a frontend-emitted `alloca` left in a loop-body block leaks a fresh stack slot per iteration (LLVM never reclaims non-entry-block allocas across iterations), so a body-block temp turns a large-module fixpoint into a stack-overflow SIGSEGV — and the enumerable hand-list of emit sites that spill temps is exactly the wrong fix (it missed ~30 sites). *Guard:* after `emit_function` hoists every body alloca to the entry block, it asserts zero `= alloca ` lines remain in the body buffer, so the next emit arm that spills a body temp is caught structurally rather than by the next overflow ([Chapter 19](19-llvm-backend.md)'s entry-block alloca section).

## How to add one

The checklist (from the former `structural-guards.md`, condensed):

1. Name the invariant in one sentence. If you can't, the framing isn't crisp enough.
2. Identify the **writer site** — where the violating shape is produced. The validator runs after it.
3. Identify the **reader site** that depends on the invariant. If nothing depends on it, it may not be load-bearing.
4. Implement a pure structural walk returning typed warnings with full location context.
5. Wire the env gate (`GG_VALIDATE_<NAME>`) — log counts, don't panic yet.
6. File a TODO; sweep to get initial counts by class.
7. Migrate one class at a time, integration sweep green each commit.
8. Promote to fatal.
9. Move the TODO to DONE.md; record the guard in the tier backlog above ([The backlog and its tiers](#the-backlog-and-its-tiers)).

When the fix you sketch for a localized bug is *intrinsically complex* (save/restore around branches, phi insertion, scope-tracking name maps), that complexity is the tell that you're patching a symptom at the read site — the real bug is a one-line oversight at the write site one layer up ([Chapter 02](02-foundations.md)'s debugging heuristic). A structural guard at that write site is precisely what makes the downstream complex fix unnecessary, because it catches the bug at the layer where it was introduced.

## In the self-host

The self-host lowerer ports the writer-side structural guards, not just the frontend. `tests/fixtures/self_host_lowerer/validate.gg` (a 497-line, ~25KB independent copy — not symlinked from any other self-host dir) is the self-host instance of the migration framework, and its header says so explicitly: *"Migration framework (mirror of `structural-guards.md`)"* (`validate.gg:9`).

It reimplements both the GIR resource guards and the LIR structural checks:

- **GIR resource guards** — `validate_resource_moves` (`validate.gg:93`, the `OpCopy`-on-a-resource-local class), `validate_resource_field_reads` (`:162`, the `__field_read_` into a `LoOwned`/`LoMaybeOwned` resource slot class), and `validate_resource_call_args` (`:228`, the `OpMove`-on-a-`LoBorrowed`-source-at-a-call-arg class). These mirror the Rust GIR `validate.rs` family one class each. All three are wired as **fatal-on-violation** (the framework's step-5 "promote to fatal" state) in `run_validators` (`:288`), with the `GG_VALIDATE_RESOURCE_MOVES=<log-path>` diagnostic gate (`:306`) preserved for sweep tooling.
- **LIR structural checks** — `validate_lir_function` (`:403`) walks each LIR function for sequential block ids, unique block-param value definitions, in-range instruction slot refs (`check_inst_slot_refs`, `:422`), and terminator-successor sanity with jump-arg/block-param-count parity (`check_term_successors`, `:446` → `check_args_match_params`, `:480`) — the same "args.len() must match the target's params.len()" invariant the Rust LIR `validate_terminator` enforces, called out in-source as the bug class the `pending_phis` retirement hit.

The per-pass dispatcher mirrors Rust's `assert_module_valid(module, after)`: `validate_gir_after` (`:361`) and `validate_lir_after` (`:383`) are no-ops unless `GG_VALIDATE_PASSES` is set (`validate_passes_enabled`, `:351`), and when set they run their layer's validators with a pass-name-tagged failure message. `driver.gg` imports the three entry points (`driver.gg:14`) and calls them as a validate-after-every-pass pipeline: `run_validators(&gir)` after lowering (`:72`), `validate_gir_after` (`:66`), and `validate_lir_after` after GIR→LIR lowering, SSA construction, and drop elaboration (`:80`, `:88`, `:96`).

So the per-pass + `GG_VALIDATE_<NAME>` env-gate pattern this chapter describes *is* exercised in self-host. The one gap is the BIR layer: the self-host has no separate BIR newtype layer (no `bir_*.gg`; the canonical-op expansion helpers are folded into `lir_lower.gg`), so `assert_primitives_only` has no self-host analogue. The GIR and LIR guards are exercised through `c_emit_comparison` and `self_host_bootstrap_fixed_point`.

## The parity floors — the north-star number as an executable gate

Since round 32 the two headline comparison harnesses are no longer
diagnostic-only: `c_emit_comparison` asserts `Matched >= C_EMIT_MATCH_FLOOR`
and `self_host_runtime_diff` asserts `MATCH >= RUNTIME_DIFF_MATCH_FLOOR`
(named consts in `tests/integration.rs`, each with the regeneration command in
the adjacent comment — bump the floor when a round lands new MATCHes, never
pad it). The asserts sit at the END of each test fn, after all backlog
listings print (a floor that fires must not suppress the diagnostics you need
to debug it), and they gate only where the measurement is meaningful: linux,
default C backend, `GG_PARITY_FLOOR_OFF=1` as a loud escape hatch, and (for
the timeout-jitter-sensitive runtime_diff count) release builds only. The
c_emit floor runs in default CI and is the real regression gate; the
runtime_diff floor fires on every intentional `GG_RUNTIME_DIFF=1` north-star
run. The remaining stage comparisons (lexer/parser/resolver/typecheck/lowerer)
stay diagnostic — regressions there surface downstream through the floored
c_emit gate; floor them individually only with their own seeded release run
and red/green proof.
