# Structural Guards

How Gorget makes soundness invariants statically enforceable, and the discipline for adding new ones.

> **Status:** Foundational. Cite this in PRs that introduce or migrate structural guards.
> **Builds on:** `layering-discipline.md`, `unified-resource-model.md`.
> **Companion in `AGENTS.md`:** *No name matching*, *Layering discipline*, *Debugging heuristic — fix complexity as a wrong-layer signal*.

---

## The bar

Gorget's goal is to be a reference implementation of a most-correct, most-robust compiler. That goal is hollow without enforcement: a soundness invariant that's documented but unchecked is a regression waiting to happen.

The bar is therefore:

> **Every soundness invariant is a writer-side static guard. Every guard is fatal once migrated. Every bug we fix leaves a permanent counterexample and a validator that locks the class shut.**

Phase C of the unified resource model demonstrated this end-to-end: four shallow-copy-of-resource shapes (Assign-Copy, FieldLoad, IndexLoad, EnumFieldLoad, Call/CallExtern args) were each migrated from "let downstream lowering paper over it" to "any GIR module containing a violation halts the build." The lowering passes can no longer silently emit the unsound shape, *anywhere*, in any user program or stdlib module. Phase C is this document's template.

---

## The five principles

### 1. Every invariant has a writer-side validator

Not "we have a test that covers this case" — *"we fail the build if the invariant is ever violated, in any input, anywhere in the codebase or any user program."*

The validator runs over the IR (or LIR) module structure after lowering. It checks the same property a human reviewer would check by reading the lowering code — but mechanically, on every program, every time. Phase C's `validate_resource_moves` / `validate_resource_field_reads` / `validate_resource_call_args` are the template: a few hundred lines of structural walking, no codegen, no runtime cost.

If a property is *important enough that violating it is a bug*, it's important enough to write a validator for.

### 2. Every bug leaves a permanent counterexample

A user-reported bug ships two artefacts:

- **(a) A fixture** capturing the user-facing symptom (`tests/fixtures/<name>.gg` + `.expected`, wired into the integration suite).
- **(b) A validator** capturing the *structural* violation that produced the symptom — when applicable.

Without (a), nobody notices when the surface behaviour regresses. Without (b), the next regression has the same shape and produces the same symptom in a different fixture, with no early signal. The validator is what makes the class permanently closed; the fixture is what makes the class observable in user terms.

Not every bug has a structural framing — some are genuinely ad-hoc. But before declaring a bug "ad-hoc", apply the *fix-complexity heuristic* (`AGENTS.md` Layering discipline): if the fix you're sketching is intrinsically complex, you're patching a symptom, and the structural framing exists upstream.

### 3. No silent skips

When a lowering pass takes a conservative shortcut to dodge a known issue (the snag #24 root cause was `recursive_drop_structs` skipping Option/Result fields entirely to avoid a double-free with shallow-copy patterns), the shortcut leaves two things behind:

- A **self-documenting marker** in the data structure (`__clone_only:`, `__inline_option_drop:`, `__drop_then_clone:`) so a human reading the table sees "this is deliberately incomplete."
- A **validator** that treats the marker as a violation, gated behind an env var until the migration is complete.

The marker is the evidence; the validator is what makes the evidence load-bearing. A skip without a marker is invisible; a marker without a validator is decorative. Both together is what the discipline requires.

### 4. The migration framework is a primitive

Phase C established a standard pattern. Any new structural guard ships with it:

```
(1) Write the validator.
(2) Gate behind GG_VALIDATE_<NAME> env var → log per-class counts to a file.
    Build does not panic; sweeping reveals the migration size.
(3) File a TODO entry tracking the burn-down by class.
(4) Migrate lowering sites one class at a time. Each commit drops one
    class's count to zero and demonstrates the integration sweep stays
    green. Migrations are independent — different agents can pick up
    different classes in parallel.
(5) Promote the validator to fatal (panic on any violation, no env var
    needed) once the count is zero. The class is permanently closed.
(6) Move the TODO entry to DONE.md with the commit chain.
```

Every step is concrete, observable, parallelisable. New guards take days, not weeks, because the pattern is reusable.

### 5. Layering discipline becomes enforcement

The four rules of `layering-discipline.md` (lossless on invariants, typed metadata not name-matched, one source of truth per axis, resolve once / write through) are the *design principles*. Structural guards are how those principles are *enforced*.

When `layering-discipline.md` says "if a downstream pass reconstructs information from names, the boundary upstream was drawn wrong", a structural guard is what makes that statement load-bearing. Without the guard, "was drawn wrong" is editorial; with it, the build fails until the boundary is fixed.

The same applies to `AGENTS.md`'s *Debugging heuristic — fix complexity as a wrong-layer signal*: a structural guard at the upstream write site is precisely what makes a downstream complex fix unnecessary. The guard catches the bug at the layer where it was introduced.

---

## What's already in place

These structural guards exist today and are load-bearing:

| Guard | Class | Site | Status |
|-------|-------|------|--------|
| `validate_resource_moves` | Assign-Copy of resource | `src/ir/validate.rs` | Fatal |
| `validate_resource_field_reads` | FieldLoad shallow-copy of resource | `src/ir/validate.rs` | Fatal |
| `validate_resource_index_reads` | IndexLoad shallow-copy of resource | `src/ir/validate.rs` | Fatal |
| `validate_resource_enum_reads` | EnumFieldLoad shallow-copy of resource payload | `src/ir/validate.rs` | Fatal |
| `validate_resource_call_args` | Call / CallExtern resource arg | `src/ir/validate.rs` | Fatal |
| Cycle check (snag #21) | Unbounded recursive type at typecheck | `src/semantic/cycle_check.rs` | Fatal |
| Cross-module type collision (snag #20) | Two modules publish same TypeDef name | `src/semantic/scope.rs` | Fatal |
| GIR module validation | Generic LIR-shape soundness | `src/ir/lowering/mod.rs:1505` | Fatal |
| `register_signatures_recursive` ExternBlock arm (snag #12) | Extern functions get FunctionInfo / def.type_id | `src/semantic/typecheck.rs` | Fatal |
| `validate_move_follow_through` (Tier 1b) | Move-mode assign of drop-registered source without follow-through MoveZero | `src/ir/validate.rs` | Fatal |
| `validate_box_inner_type` (Tier 1d) | Regular `Box[T]` StructDef missing typed inner-type metadata | `src/lir/validate.rs` | Fatal |
| `validate_type_metadata_coherence` (Tier 1c) | TypeDef registered with implicit `(None, Trivial)` whose fields/variant-payloads are transitively droppable | `src/ir/validate.rs` | Env-gated (`GG_VALIDATE_TYPE_METADATA_COHERENCE`) — partial close, Option/Result wrapper migration deferred to Cluster 1 follow-on |
| `validate_consume_sites` (Tier 2a) | Source ownership mismatch at every consume position (Call/Init/Mutator/HeapAlloc/`Inst::Assign`) | `src/ir/validate.rs` | Fatal |
| `validate_drop_pre_rebind` (Tier 2c) | Heap-allocating consumer (Box.new shallow-copy) source not `MoveZero`'d before subsequent same-block Drop | `src/ir/validate.rs` | Fatal |
| `no_typed_metadata_sidecars` (Tier 2d) | Parallel `HashMap<*, T>` sidecar where `T` is a typed `TypeMetadata` / `Local` field (DropStrategy / CopySemantics / CollectionKind / EnumKind / EnumCategory / LocalOwnership / BorrowOrigin) | `tests/lints.rs` | Fatal (BUDGET=0) |
| `no_growth_in_phase_d_proxy_reads` (Tier 3b) | Proxy reads of `is_named_local` / `is_owned_local` / `drops.is_registered` / `drops.is_moved` (sidecar to Phase D's `Local.ownership`) | `tests/lints.rs` | Ratchet (BUDGET=64, decreases as migration proceeds) |

Plus the migration framework itself — `GG_VALIDATE_*` env gate, per-class file logging, the gate-→-zero-→-promote pattern from Phase C — is reusable as-is.

### Drop-emission contract: defensive-by-default (Snag #30, 2026-05-10)

The GIR drop accountant (`src/ir/lowering/drops.rs`) emits **`DropIfAlive` unconditionally** for every resource-typed scope-exit drop, regardless of the local's `maybe_moved` flag. The LIR `drop_elab` pass then statically elides the runtime drop-flag check when slot init is provably unconditional, so codegen quality is preserved.

**Why defensive-by-default:** Snag #30's minimal repro (struct-field alias from variant payload + trailing match on a separate Option → double-free) revealed that `maybe_moved` tracking across nested matches with early-return paths can produce false negatives. A local marked moved in the first match's Some arm (via `move_zero_and_mark`) appeared as not-moved at the second match's None-arm `emit_early_exit_drops` callsite, leading to unconditional `Drop` emission that double-freed the heap aliased between the move-zero'd source slot and the move'd destination slot. The `lower_match_expr` (expression-form) doesn't `snapshot_moved`/`restore_moved`/`union_moved` between arms the way `lower_match_stmt` does, so cross-match propagation has hidden correctness gaps the always-`DropIfAlive` contract sidesteps.

The `DropEntry::maybe_moved` sidecar is preserved (with its setters) for future invariant audits, but it's no longer load-bearing for soundness — the LIR pass is the source of truth for whether the runtime check fires.

**Litmus test:** if a future change wants to skip the runtime drop-flag check at GIR emission (emit raw `Drop` instead of `DropIfAlive`), the change MUST first prove the local is alive on EVERY incoming control-flow path — a CFG-aware analysis, not a per-arm `maybe_moved` flag. Until that exists, the always-conditional contract holds.

---

## Backlog

Tiered by maturity. Tier 1 = invariants we know are violated today, with concrete burn-downs. Tier 2 = invariants we should have but haven't designed validators for. Tier 3 = discipline meta-invariants policing the design rules.

### Tier 1 — invariants with known violations today

#### 1a. Drop completeness *(addresses snag #24)*

**Invariant.** Every droppable field of every type T must be reachable through T's emitted drop function. If a field's type has `drop_strategy ≠ None`, the field appears in `module.recursive_drop_structs[T]` (or `recursive_drop_enums[T]`) with a non-skip drop entry.

**Validator sketch.** Walk every struct/enum LIR StructDef. For each field whose type has a non-None drop strategy, check the recursive-drop table contains a corresponding non-`__clone_only:` entry. The `__clone_only:` markers self-incriminate.

**Why it matters.** Snag #24's class. A struct field of type `Option[Box[Resource]]` silently leaks the box at scope exit because `lir/lower/mod.rs:412-423` records the field as `__clone_only:` (skip drop). The validator turns this leak from "discovered when a user runs ASAN" into "the build halts the moment the lowering pass produces an incomplete drop table".

**Burn-down (in order).**
1. Build validator + env gate. Initial sweep gives the migration size.
2. Inline-drop scheme for Option/Result struct fields (prototype documented in TODO snag #24).
3. CoW clone-on-consume for enum constructors with resource args (the deeper bug snag #24 exposed).
4. Match-scrutinee staging migration so the resulting Borrow shape doesn't trip Phase C's read-site validators.
5. Promote.

**Estimate.** 1-2 sessions for the validator, ~3-5 sessions for the migration.

#### 1b. Move follow-through *(SHIPPED 2026-05-07; commit `3e49a03a`)*

Validator at `src/ir/validate.rs:1642` (`validate_move_follow_through`); fatal panic block at `src/ir/lowering/mod.rs:1619`. The dominant violation class — f-string interp segment temps emitting `Move`-mode assigns of drop-registered sources without follow-through — was closed in commit `1d3ccd5b` at `src/ir/lowering/exprs/calls.rs:1517` and `:1533`. Sweep of record (`c_emit_comparison`, 1066 fixtures): 0 violations. See DONE.md for the full chain.

#### 1c. Type-metadata coherence at registration *(IN PROGRESS — foundation + safe-class migrations shipped 2026-05-11; Option/Result wrappers deferred)*

**Invariant.** Whenever a TypeDef is registered with `copy_semantics: Trivial`, none of its fields/variant-payloads has `copy_semantics: Resource` or `drop_strategy ≠ None`. Said differently: the upgrade pass (`upgrade_types_from_fields`) is unnecessary because metadata is correct at the construction site.

**Validator.** `validate_type_metadata_coherence` at `src/ir/validate.rs:1776` walks every TypeDef and compares the recorded `(drop_strategy, copy_semantics)` against what `TypeRegistry::compute_drop_strategy_for_struct/_for_enum` returns from the live registry. Mismatch (helper says Recursive+Resource, recorded was None+Trivial) is a violation. Env-gated via `GG_VALIDATE_TYPE_METADATA_COHERENCE=<log>`. **Smart-pointer-wrapper carve-out:** single-field struct `{ _0: T }` registered with explicit `(Trivial, None)` and no `enum_category` / `collection_kind` is the structural signature of Mutex/RWLock-style "permanent singleton" wrappers — the writer chose Trivial+None EXPLICITLY (these handles aren't freed at the GIR level; inner T's lifecycle is managed at the C runtime layer). The validator skips this case structurally, no name-matching.

**Why it matters.** The snag #24 root cause includes a timing class: `make_option_type_def` registers Options with `..Default::default()` (DropStrategy::None), and the global `upgrade_types_from_fields` pass that fixes this only runs once at module-start. Lazy registrations during expression lowering miss the upgrade. Promoting *coherence-at-construction* to a hard invariant eliminates the timing class entirely.

**Shipped 2026-05-11 (commits `3556b390`, `1f463392`):**
1. **Foundation:** `TypeRegistry::compute_drop_strategy_for_struct(&[StructField]) -> (DropStrategy, CopySemantics)` and `compute_drop_strategy_for_enum(&[EnumVariant]) -> (DropStrategy, CopySemantics)` extracted from `upgrade_types_from_fields` semantics — narrower than `needs_drop` (excludes FnPtr fields, matching the post-hoc pass).
2. **Validator + env gate:** `validate_type_metadata_coherence` + `GG_VALIDATE_TYPE_METADATA_COHERENCE` env gate + smart-pointer wrapper carve-out. 4 unit tests.
3. **Safe-class migrations (registration sites that don't surface latent shallow-copy lowering bugs):** `register_struct_type` / `register_enum_type` / `register_newtype` in `lowering/types.rs`; `monomorphize_enum` (user generic enums, with Option/Result carve-out) in `lowering/generics/mod.rs`.

**Deferred (Cluster 1 follow-on required first):**
- `make_option_type_def` / `make_result_type_def` and all their callers (`map_ast_type_mut`, `register_builtin_option`/`_result`, three `Result_T_E`-from-throws sites in mod.rs/traits.rs/functions.rs, `map_err`/`map` in methods.rs, `ensure_option_type_registered` in context.rs).
- `monomorphize_enum` carve-out for Option/Result.
- `monomorphize_struct` (user generic structs) — initial migration regressed 8 `tensor_*` fixtures via missing per-mono Shared/Weak `__drop` wrapper emission (the C backend emits wrappers on-demand and a field-level recursive drop call doesn't register the dependency). Reverted in commit `7c20e379`. Migration blocked on per-mono wrapper emission dependency tracking at LIR-exit, in addition to the Cluster 1 lowering follow-on.
- Tuple registration in `map_ast_type_mut` and `exprs/type_reg.rs::register_tuple_type` (late-registered).
- Closure capture struct in `closures.rs` (late-registered).

Migrating these surfaces ~93 latent shallow-copy lowering issues that Phase C's `validate_resource_moves` correctly flags — same class as the 2026-05-07 Cluster 1 revert. The blocker: FieldLoad/EnumFieldLoad lowering needs to emit `Borrow` not `Copy` when the source is a borrowed Option/Result return. Estimated 1-2 weeks; tracked in the existing Cluster 1 / Snag #24 TODO entries.

**Remaining burn-down.**
1. Cluster 1 / Snag #24 follow-on lowering migration (1-2 weeks).
2. Migrate the deferred wrappers above.
3. Promote validator to fatal once the count hits zero.

#### 1d. Box-inner-type completeness *(SHIPPED 2026-05-07; commit `bfb6bb67`, defense-in-depth tests `095ff22f`)*

Validator at `src/lir/validate.rs:764` (`validate_box_inner_type`); fatal at `src/lir/lower/types.rs:615` (LIR module-exit, unconditional) AND under `assert_module_valid` per-pass via the `VALIDATORS` registry at `src/lir/validate.rs:74`. Both wirings active: per-pass under debug + `GG_VALIDATE_PASSES`, plus unconditional release-build fatal. 7 unit tests cover well-formed Boxes, trait-box skip, missing-inner detection, mismatched-suffix detection, non-Box ignore, unexpected-field-shape branch, and full-pipeline `assert_module_valid` integration. See DONE.md for the full chain.

### Tier 2 — invariants we should have, designed but not yet built

#### 2a. CoW consume-site discipline *(deeper class than #1a)* — **FULLY SHIPPED 2026-05-10**

**Phase 1 + 2A/B/C/E SHIPPED 2026-05-08.** Validator + non-fatal env-gate (`7ab736c0`), writer-site tagging (`81014df4`/`d0c2f2f6`/`6851c877`/`26145106`), and three migration commits (`e1214312`) drove violations for Calls/Inits/CollectionMutators/BoxNew to zero across all 1068 fixtures. Validator promoted to fatal (`9cd32876`). Phase 2E (`10abfbef`/`6242fc0a`) replaced `preceded_by_clone`'s name-list with typed `RuntimeFn::returns_fresh` + `clone_fn_names_set`. See DONE.md for the full chain.

**Phase 3 SHIPPED 2026-05-10.** Snag #28's match-arm-result borrow-clone bug exposed a gap: the existing validator covers consume-site SHAPES (calls, inits, runtime mutators) but not plain `Inst::Assign` whose dst is an owned-required slot. The Snag #28 shape — `[Mv] _result = copy _ptr` materialising as memcpy of a borrowed pointee struct — passed silently. New `ConsumeSiteClass::AssignIntoOwnedSlot` (commit `2846baf4`) walks `Inst::Assign` and gates on dst-type resource-ness (the existing `validate_consume` gates on source-type, which Ptr<T> sources don't satisfy). **Migration: 11,129 → 0 violations across 1078 fixtures, validator promoted to fatal alongside the Phase 1/2 classes.** All AssignIntoOwnedSlot violations now halt the build on first hit.

Phase 3 progress through 2026-05-10: **11,129 → 64 violations (-99.4%)** across 1078 fixtures, via six inference-pass extensions, one literal-tagging fix, and three lowering-level fixes:
- Validator + non-fatal env-gate (commit `2c0d53e9` after squashed rebase onto main).
- Validator dst-filter narrowed to Owned/FreshOwned only.
- EnumInit/StructInit/TupleInit dst-tagging.
- Dict-literal `set_owned` (mirrors array-literal pattern).
- BinOp/UnOp dst-tagging (resource-typed binary ops produce fresh values).
- `Inst::Assign { dst, value: Constant::Str }` dst-tagging (string-literal init materialises fresh heap).
- `IndexLoad { read: ReadMode::Clone }` dst-tagging (collection element clone).
- Deref-then-MoveZero dst-tagging (`!`-move param consume shape, commit `9e706783`).
- **Bare-param Ptr-alias optimization retired** (commit `bca88f29`): the historical `lower_var_decl` branch silently changed `String x = some_param` declarations into `*String x = &some_param`, causing downstream auto-deref-and-memcpy bugs at consume sites. Removing it (flow falls through to the sound `clone_fn_for_ptr` clone branch) closed 412 → 216 Borrowed violations and the new fixture `vardecl_owned_call_double_drop.gg` (Snag #29c repro) passes.
- **Match-scrutinee Move-mode gated on `source_at_last_use` + validator skips Borrow-mode assigns** (commit `a3380c96`): the staging assign emitted Move on a source that `tag_of` and pattern extraction would re-read; gating Move on liveness routes those to Borrow mode, and the validator's added Borrow-skip recognizes Borrow as the alias contract. **Owned-but-live class closed (303 → 0).**
- **Field_origin propagation retired** (commit `edd8bf9e`): same architectural bug as bare-param at a different propagation path. Bisected via probes (cow_borrow stays load-bearing for self-host's CoW alias optimization; field_origin was not load-bearing for any test). Closed 219 → 17 Borrowed.
- **Residual closure (commits `6d1ca1f8`, `ec766c8b`, plus the strict-`safe_in_loop` cow-borrow tightening): five lowering / inference fixes drove 64 → 0.** `BinaryOp::Add` recursion in `infer_closure_return_type` (closes IIFE `ty4` cluster); `EnumFieldLoad`/`FieldLoad` self-zero rule in `tag_ownership` (`IoError`/`Frontmatter`/`Big`); bare-local Assign-then-MoveZero rule (match-arm-result merge); `lower_catch_expr` emits `move_zero(val_local)` after extract (`error_conditional_throw`); `resolve_tuple_field_type` peels `Ptr`/`MutPtr` (`closure_tuple_destructure`); Range-index returns base type for slice (`vector_capacity`); `interp_temp_mode` clones for `Borrowed`/`View` sources (`exec_output_captures_stderr`); Assign + dst-zeroed inference rule (`lower___lower_call`); strict `safe_in_loop` cow-borrow propagation in `lower_var_decl` (string-reassign-loop / cow-borrow-basic / self-host's `format_*_lines` and `join` defaults).

**Status 2026-05-10**: 0 violations. Validator FATAL across all consume-site classes. Promoted at `src/ir/lowering/mod.rs` (folded `AssignIntoOwnedSlot` into the unconditional panic block).

**Invariant.** At every consuming position (push / put / insert / send / `v[i] = x` / enum-init / struct-init / Box.new / function arg with `Ownership::Move` / **plain `Inst::Assign` into a resource-typed Owned/FreshOwned slot**), the IR mode of the source matches its typed `LocalOwnership` state per the rules in `AGENTS.md`'s *Ownership at Consuming Positions*:

| Source state | Required IR shape |
|--------------|-------------------|
| Owns AND dead at this call | Move (transfer ownership) |
| Borrow OR owned but live past this call | Clone (deep copy) |
| Static literal | Runtime `*_materialize` |

**Validator sketch.** For each `Inst::Call` / `Inst::CallExtern` / `Inst::EnumInit` / `Inst::StructInit` / push / put / insert / send / index-store / **`Inst::Assign`** (Phase 3), check the source operand's `LocalOwnership` against the IR mode. Mismatch is a violation. The two helpers `validate_consume` (source-type-gated) and `validate_assign_consume` (dst-type-gated) split the resource-ness check axis appropriately for each consume shape.

**Why it matters.** This is the *deepest* layer of the snag #24 class. Today the IR emits `enum_init Node::VarDecl { copy _3 }` even when `_3 = decls` is owned and live past the call (read at scope exit). The "owned but live → clone" rule isn't enforced at the enum-constructor consume site. The skip in `recursive_drop_structs` was masking this; once drops fire correctly, the aliasing produces use-after-free. Snag #28 is the same bug shape at a plain `Inst::Assign` boundary — Phase 3 closes the validator gap and is driving the remaining 3,052 violations to zero.

**Burn-down (Phase 3 — COMPLETE 2026-05-10).**
1. ✅ Validator + non-fatal gate (commit `2846baf4`).
2. ✅ Constructor dst-tagging in inference pass + dict-literal `set_owned` + filter narrowing (commit `2e8a38f7`).
3. ✅ Residual 64 → 0 across five lowering / inference fixes plus the strict-`safe_in_loop` cow-borrow tightening (commits `6d1ca1f8`, `ec766c8b`, plus this session's final commit).
4. ✅ Promote `AssignIntoOwnedSlot` to fatal — folded into the unconditional panic block at `src/ir/lowering/mod.rs`.

**Estimate (delivered).** Phase 1/2 was ~6 sessions; Phase 3 closed in 2 sessions.

#### 2b. Match-scrutinee discipline

**Invariant.** Every match scrutinee staging assign of a resource-typed scrutinee uses `AssignMode::Borrow` or `AssignMode::Move`, never `AssignMode::Copy`. Phase C covers most of this; the Option/Result-of-resource case (which the snag #24 attempts surfaced) is the remaining gap.

**Validator sketch.** Walk `Inst::Assign` instructions whose dst is the result of `stage_match_scrutinee`. For resource scrutinee types, mode must not be Copy.

**Why it matters.** When snag #24's drop-completeness fix promotes Option/Result-of-resource to Resource, the existing match-scrutinee staging emits Copy mode (because today's classifier doesn't see Option as a resource) and trips Phase C's validator. Closing this gap unblocks the snag #24 migration and locks the rule that match never consumes its scrutinee.

**Burn-down.** Validator + a single migration commit + promote. ~1 session.

#### 2c. Drop-tracking pre-rebind correctness *(snag #23 class)* — **SHIPPED 2026-05-10**

**Validator at `src/ir/validate.rs:1790`** (`validate_drop_pre_rebind`); fatal panic block at `src/ir/lowering/mod.rs:1664`. Initial sweep (1078 fixtures, env-gated via `GG_VALIDATE_DROP_PRE_REBIND=<log-path>`): **0 violations** — Snag #23's writer-side fix at `4ebefe44` (Box.new emits `move_zero_and_mark` after the alloc) closed the only known violation class, and no other shallow-copy heap-allocating consumer exists today. The validator locks the rule for any future consumer.

Recognition is typed: the validator reads `Module::heap_alloc_consumer_externs`, populated at the writer site every time the GIR lowering emits a `__gorget_box_alloc_<T>` extern call (3 sites — `calls.rs`, `methods.rs`, `mod.rs`). No `name.starts_with("__gorget_box_alloc_")` substring match survives — adding a new shallow-copy heap-allocating consumer at any future writer site is a single `module.heap_alloc_consumer_externs.insert(...)` call. Per CLAUDE.md "No name matching".

**Invariant.** When a value flows into a heap-allocating consumer (currently `__gorget_box_alloc_<T>`; deep-clone consumers are out of scope — see below), the source's drop registration is retired *before* any subsequent drop emission targets the source — i.e., the source slot is move-zeroed AND `drops.mark_moved` is set, not just `drops.unregister`.

**Scope.** Shallow-copy only. `gorget_string_clone_to_owned` / `gorget_array_clone` / `gorget_map_clone` / `gorget_set_clone` produce a fresh independent value — source's storage is untouched and a later `Drop` of source is correct. Including them would produce 138 false positives (sweep of record from the validator's first iteration). Box.new is the only shallow-copy heap-allocating consumer in the codebase today.

**Why it matters.** Snag #23's Box.new segfault. The fix shipped at `4ebefe44`; the validator locks the class so a future heap-allocating consumer can't forget the move-zero step.

See DONE.md for the full commit chain.

#### 2d. Sidecar absence — **SHIPPED 2026-05-10**

**Invariant.** For each typed metadata field on a TypeDef / StructDef / FunctionInfo, no parallel `HashMap<TypeId, X>` or `HashMap<String, X>` sidecar exists in the codebase tracking the same fact.

**Validator at `tests/lints.rs::no_typed_metadata_sidecars`** — Rust test grep-walking `src/**/*.rs` for `HashMap<key, value>` / `FxHashMap<...>` / `BTreeMap<...>` declarations where `value` is in the watched set: `DropStrategy`, `CopySemantics`, `CollectionKind`, `EnumKind`, `EnumCategory`, `LocalOwnership`, `BorrowOrigin`. The watched set names every typed field whose canonical home is `TypeMetadata` (`src/ir/types.rs`) or `Local` (`src/ir/mod.rs`). Comment-line matches are skipped (a doc-comment that *describes* a retired sidecar is not a sidecar). The pattern accepts qualified-path prefixes (`crate::ir::types::TypeId`) so it catches both bare and fully-qualified declarations.

**Initial sweep + post-sweep (1078 fixtures): 0 sidecars.** The post-Phase-D / post-Phase-A floor is clean — historical sidecars (`mut_capture_locals: FxHashMap<LocalId, TypeId>` retired 2026-05-04 commit `404c8716`; `view_returning_temps: FxHashSet<LocalId>` retired in commit `9dc2cf4d`; `is_resource: &Fn(TypeId) -> bool` callback retired in `ec31fc34`) were all closed during the typed-metadata migration sessions. The lint locks the floor at BUDGET=0.

**Why it matters.** *Layering discipline rule 3*: one source of truth per axis. Sidecars accumulate quietly and produce multi-step inconsistencies. The lint catches new sidecars at introduction time — a Rule 3 regression now fails CI before it lands.

**If the lint fires:** the offending file:line names a `Map<key, T>` declaration where `T` is a typed-metadata axis. Fix is one of:
1. Migrate the lookup to read the typed field directly via the canonical accessor (e.g. `registry.get_type_def(name).map(|td| td.metadata.drop_strategy)`).
2. If the map is a per-pass scratch (computed from the typed field, not a parallel persistent registry), add an allowlist entry to `SIDECAR_VALUE_TYPES` with file:line + comment justifying why it's not a sidecar.

**Coverage extensions.** Adding a new typed-metadata axis (e.g. a future `is_view_type: bool` field on TypeMetadata) requires adding the type to `SIDECAR_VALUE_TYPES` so the lint protects it. The watchlist is the explicit registry of what's protected; adding a field without watchlist entry leaves it un-locked.

### Tier 3 — discipline meta-invariants

#### 3a. No name-matching at consumer boundaries

**Rule.** `name.starts_with("Box__")` / `.starts_with("Vector__")` / `.starts_with("Option__")` / similar at non-registrar sites is a violation. The legitimate registrar sites (LIR registrars, the C-emit boundary contract layer) form an explicit allowlist.

**Validator sketch.** Lint pass over the compiler source tree (Rust). Match `&str::starts_with` calls whose argument is a string literal matching a known mangled-type prefix; flag anything outside the allowlist.

**Why it matters.** *Layering discipline rule 2*: typed metadata, not name-matched. The rule has been violated quietly multiple times across the project's history; each violation produced a snag (snag #13's helper-emission gap, snag #20's collision detector gap, snag #24's skip class). Promoting the rule to a lint catches the next instance at PR time.

**Burn-down.** One-off lint pass. Allowlist starts small; grows as legitimate registrar sites are discovered.

#### 3b. Phase D state coherence — **SHIPPED 2026-05-10** (ratchet locks current floor)

**Rule.** `LocalOwnership` is the source of truth for ownership and borrow tracking. Any consumer reading `drops.is_registered`, `is_named_local`, `is_owned_local`, `drops.is_moved`, etc. as proxies for ownership is a discipline violation that should migrate to the typed accessor.

**Validator at `tests/lints.rs::no_growth_in_phase_d_proxy_reads`** — Rust test grep-walking `src/**/*.rs` for proxy callsites: `\.is_named_local\s*\(`, `\.is_owned_local\s*\(`, `drops\s*\.\s*is_registered\s*\(`, `drops\s*\.\s*is_moved\s*\(`. Comment-line and `fn`-definition-line matches are skipped (the proxies' own implementations are the canonical site, not violations).

**Baseline 2026-05-10: 64 proxy reads** across `src/ir/lowering/...`. The ratchet locks the floor; new proxy reads fail the test until either:
1. The new site is migrated to `builder.locals[local.0 as usize].ownership` (or `ctx.source_ownership(...)` for operands).
2. The proxy is genuinely needed (e.g. inside the proxy's own `fn` definition), excluded via the comment-skip / fn-def-skip already in the lint.

**Burn-down.** Phase D4 retired `is_named_local` from `lower_var_decl_assign_mode`'s decision tree (2026-05-10). The remaining 64 sites are scattered across pattern lowering, drop accountant interactions, branch handling. Each migration that retires a site decreases the budget — one-way ratchet. Full retirement is multi-session.

**Coverage extensions.** New proxy methods that read Phase D state (a hypothetical `is_borrowed_local`, `drops.was_alias_at`, etc.) need to be added to `PHASE_D_PROXY_PATTERNS` so the lint protects them. The watchlist is the explicit registry of what's banned.

---

## How to add a structural guard

Concrete checklist for shipping a new validator. Cite this section in the commit message that introduces the validator.

1. **Name the invariant in one sentence.** If you can't, the framing isn't crisp enough; refine before writing code.
2. **Identify the writer site.** Where in the lowering pipeline does the violating shape get produced? The validator runs *after* this site, on the resulting IR.
3. **Identify the reader site that depends on the invariant.** What downstream code assumes the property holds? If nothing assumes it, the property may not be load-bearing — reconsider.
4. **Implement the validator.** Pure structural walk of the IR / LIR module. No codegen, no runtime cost. Returns a Vec of typed warnings, each carrying enough context (function, block, instruction index, type names) for the user to locate the violation.
5. **Wire the env gate.** `GG_VALIDATE_<NAME>=<log-path>` writes per-class violation counts and details to `<log-path>`. Build does not panic at this stage. Default unset; opt-in.
6. **File a TODO entry** under "Phase X extension" or a similar umbrella. Track the burn-down by class, with a sweep-of-record showing initial counts. Reference the validator's source file and the env var name.
7. **Migrate.** One class at a time; each commit drops a class's count to zero and demonstrates the integration sweep stays green. Different agents can run different migrations in parallel — classes are independent.
8. **Promote to fatal.** Replace the env-gate write with an unconditional panic (matching Phase C's pattern at `src/ir/lowering/mod.rs:1505-1586`). The class is permanently closed.
9. **Move the TODO entry to `DONE.md`** with the commit chain, and add a row to the *What's already in place* table at the top of this document.

The expected timeline for a typical guard is days to ~2 weeks, depending on migration size. Validators with small initial sweeps (single-digit violations) often ship in one session including the migration.

---

## Relationship to other docs

- **`layering-discipline.md`** describes *the design principles* (what each layer can and can't do, where information must cross). This document describes *the enforcement mechanism* that makes those principles load-bearing.
- **`unified-resource-model.md`** describes *the resource model* (Owned / Borrowed / View / MaybeOwned / etc.) and Phase A/B/C/D migrations. Phase C is the worked example this document generalises.
- **`copy-on-write.md`** describes *the consume-site rules* (Tier 2a above). When 2a's validator ships, this doc and copy-on-write.md form a closed loop: copy-on-write states the rule, 2a enforces it.
- **`AGENTS.md`** carries the operational discipline (no name matching, fix-complexity-as-wrong-layer-signal, don't redesign around compiler gaps). This document is *why* those rules exist as enforcement, not just style.

---

## Why this is worth doing

Most compilers ship correctness as an aspiration: the implementers know the invariants, code review catches the egregious violations, fuzzers catch the rest, and users file the remainder as bugs.

The reference-implementation bar is different: invariants are *machine-checked at every build*. A bug becomes a permanent counterexample within a few hours of being reported, with a structural guard locking the class shut. New contributors don't need to learn the invariants — the build fails when they violate them.

Phase C demonstrated this is achievable at modest cost (a few hundred lines of validator per class, plus migration time amortised across multiple sessions and contributors). Generalising the discipline to every soundness invariant in the compiler is the program of work this document scopes.
