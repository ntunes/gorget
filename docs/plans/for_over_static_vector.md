# FIDELITY brief v2 — `for x in <static Vector>` loop-drop + the RESOURCES-table activation it forces

**v2 folds review #1 (BLOCKING):** Part B-b was a non-fix (self-host `lower_for`
is type-name-driven, not place-guarded); the real self-host fix is registration
only. Part-C trigger corrected: **Part A (the Rust fix) ALONE arms the divergent
RESOURCES table** in every self-host gate. The fn-count/stability gates DON'T
catch metadata drift → a **byte-level `--lir-c` content diff** is required. Staged
so Part A lands + is content-verified in ISOLATION first.

## Goal
`for x in TABLE` (a module-level `static Vector[…]`) silently emits NOTHING —
the loop body never runs (root-caused 2026-06-03; confirmed). Fix it in Rust `gg`
(the oracle) and the self-host. ⚠ **The Rust fix ACTIVATES the self-host's
currently-inert `lookup_resource_table` (it iterates `for entry in RESOURCES`,
itself a static Vector), which is KNOWN to diverge from the hardcoded
`build_resource_metadata` fallback the self-host has silently been using.** So
this chain is really TWO things: (1) the for-loop fix, and (2) surfacing/handling
the RESOURCES-table-vs-fallback reconciliation the dead loop was masking. STAGE
Part A first and let the divergence surface cleanly.

## Verified root cause (review-confirmed, RAN the repro)
Module-level `static` → `Operand::Constant(GlobalRef(name))`, NOT a place. Both
lowerers detect `collection_kind` only for a *place* operand → a constant →
`collection_kind=None` → fall to the user-iter path → `else { return }` → loop
emits nothing. `iter_type` IS resolved correctly to `Vector__T` — purely a
place-vs-constant guard, NOT type inference. `/tmp/static_for_repro.gg`: Rust `gg`
prints `0`, should print `slice/trim/2`; LOCAL Vector + `TABLE.len()`/`.get(0)`
work — only the for-loop over the static is broken.

## ───────── PHASE 0: reconcile the RESOURCES table to the fallback (NEW — the prerequisite) ─────────
The staged execution PROVED (byte-level `--lir-c` diff + `self_host_runtime`
CC-FAIL + Rust-oracle + `grep`) that activating the table (which Part A does)
emits DANGLING Box symbols. Fix the table to match the fallback FIRST:
- `compiler/data/resources.gg`: `Box__T` entry (~:160-168) — set `clone_fn` →
  `None` and `drop_fn` → `None` (the schema field is `drop_fn`, arg position 3,
  `schema.gg:65`; was `Some("__gorget_box_free")` (`resources.gg:164`)/`clone_fn`
  `Some("__gorget_box_clone")` (`:165`) — both 0 runtime + 0 codegen hits = dangling; Box
  clone at a CoW boundary is a shallow handle `memcpy`, Box free is per-mono
  `__gorget_box_free_<inner>` emitted by codegen, NOT a table-driven symbol).
- `compiler/data/resources.gg`: bare-Box `BkTraitBox` entry (~:186-191) — set
  `drop_fn` → `None` (was `Some("__gorget_trait_box_free")` (`resources.gg:190`) — also 0 hits =
  dangling). ⚠ This entry ALSO diverges on size (table 16 vs fallback 8) and
  lir_type (table `LtStructBase` vs fallback `LtPtr`) — those are LATENT (no
  named fixture exercised the trait-box path). When you run the FULL suite with
  A1 (Phase 1), watch for any trait-object/dyn-dispatch fixture CC-FAILing; if one
  does, reconcile those fields to the fallback too (verify vs the Rust oracle).
- ⚠ This is a VALUE change, NOT a schema change → NO `SCHEMA_VERSION` bump, NO
  row-count change (`resources_load_clean` count assertions unaffected — confirm).
- It is parity-neutral on its own (the table is still inert until Part A) — but it
  must precede Part A so the activation is clean.
- ⚠ The table is READ by Rust too (`src/ir/resources.rs` walker). Confirm Rust's
  behavior is unchanged (Rust already ignores these fields → emits per-mono /
  shallow-memcpy; `cargo test --lib` + the box fixtures' `--emit-c-lir` unchanged).

## ───────── PHASE 1: Part A (Rust) IN ISOLATION ─────────
### A1 — the fix (`src/ir/lowering/stmts/for_loops.rs`, `lower_for` ~187-216)
Derive `collection_kind` from the already-pointee-resolved `iter_type`
(`ctx.type_registry.get(iter_type)` → `TypeDef.metadata.collection_kind`) instead
of re-deriving it from the `iter_op` place (the place-keyed lookup at :194-207 is
REDUNDANT with `iter_type`). Re-pin by content.
- ⚠ **KEEP the pointee unwrap** (`ctx.pointee_type(iter_type).unwrap_or(iter_type)`)
  before the `get_type_def` lookup — the `:170-171 else` path can leave `iter_type`
  as `Ptr(_)`, which the place-path's `:197` unwrap currently absorbs; without it a
  `Ptr` → `None` → drop.
- Review-CONFIRMED equivalent for the common cases: `register_collection_alias`
  sets `collection_kind` from the protocol (`types.rs:824`), same enum the
  place-path reads; element-type extraction is `iter_type`-keyed and downstream
  (`lower_for_array`/`_dict`), so it's UNTOUCHED. Range returns early (:119-128);
  user-iter `None` path is already `iter_type`-dispatched (:226). Borrow/drop is
  handled — `init_borrow_iter_local` (:49-63) already borrows resource iters, so
  the static is iterated by reference, not dropped.
- Verify: `/tmp/static_for_repro.gg` → `slice/trim/2`; LOCAL control still works.

### A2 — ⚠ the RESOURCES activation (the crux — surface it, don't paper over)
A1 makes `for entry in RESOURCES` (`lir_lower.gg:196`, `RESOURCES` = `public
static Vector[ResourceEntry]`, `resources.gg:57`) WORK in the **Rust-compiled
driver** — so `lookup_resource_table` goes LIVE in EVERY self-host gate
(`c_emit_comparison`/`lowerer_comparison`/`self_host_runtime`/`bootstrap`
stage-0 all run the Rust-compiled driver). The table diverged from
the `build_resource_metadata` fallback (`lir_lower.gg:208-404`) on Box (now FIXED
in Phase 0): `Box__T` drop/clone `Some(...)` vs fallback `None` (Phase-0 target);
bare `Box` ALSO diverges on size (16 vs 8) + lir_type (`LtStructBase` vs `LtPtr`)
— LATENT (no named fixture hit the trait-box path; watch the full suite). The
review confirmed every NON-Box family's table drop/clone EQUALS the fallback, so
the drop/clone reconciliation is Box-only. (`Mutex__T` `runtime_name`/
`c_typedef_name` did NOT surface in emitted C — the `GorgetMutex` typedef comes
from the runtime preamble, not the metadata field.) These fields
ARE consumed (`runtime_name`→`type_runtime_map` :725/733; `method_prefix`→
dispatch :442/457/824; `box_kind`→:621/811), so activation WILL change emitted C
for Box/Mutex/string code.
- ⚠ **The standard gates MISS this:** `c_emit_comparison` compares only user-fn
  COUNT (not content); `bootstrap_fixed_point` checks only stage-N==stage-N+1
  STABILITY (a *stable* divergence converges GREEN). So a metadata-only body/
  typedef/dispatch change is INVISIBLE to them.
- **REQUIRED — byte-level content diff:** build the **self-host driver** (`driver.gg`
  compiled by Rust `gg`) BEFORE A1 (current gorget-1 Rust `gg`) and AFTER A1, then
  run each driver on the SAME fixture and diff the emitted C. ⚠ Use the driver's
  **`--lir-c`** flag CONSISTENTLY for both before/after (body-only, matches
  `c_emit_comparison`'s granularity) — do NOT mix with `--emit-c` (full-program,
  includes preamble). (Review note: `--lir-c`/`--emit-c` are the self-host
  DRIVER's flags, `driver.gg:54-57`; the Rust `gg` binary's own dump flag is
  `--emit-c-lir` — but A2 diffs the DRIVER's emit, which is the activation point,
  so the driver flags are correct.) Concrete fixtures that exercise the diverging
  symbols (review-named): Box → `box_heap.gg`/`box_callable.gg`; Mutex →
  `mutex_basic.gg`/`async_mutex_lock.gg`; String-method → `bench_string_methods.gg`/
  `cow_materialization_points.gg`. Report EXACTLY what changes (before=fallback,
  after=table).
- **DECISION GATE** (Phase 0 already fixed the KNOWN Box divergence, so this
  re-diff should now be CLEAN for Box — it CONFIRMS the Phase-0 fix worked AND
  catches any OTHER symbol Phase 0 didn't cover):
  - If the content diff is CLEAN (table == fallback for every exercised symbol —
    EXPECTED post-Phase-0) AND the full suite is green → commit Phase 0
    (`compiler/data/resources.gg`) + Part A (`for_loops.rs`) as separate commits,
    proceed to Phase 2.
  - If it STILL DIVERGES on a symbol Phase 0 didn't cover (or the FULL suite
    surfaces a new table-vs-fallback CC-FAIL — e.g. a trait-object fixture hitting
    the bare-Box size/lir_type divergence) → reconcile THAT symbol to the fallback
    too (set the table field to `build_resource_metadata`'s value, verified vs the
    Rust oracle) and re-run. ⚠ If the residual set is LARGE/unclear (the table was
    authored but NEVER read until now — likely several latent wrong entries) →
    **STOP and REPORT the full divergence list** rather than open-ended whack-a-mole;
    do NOT force, do NOT reshape the fallback. A1 + Phase 0 stay UNCOMMITTED until
    the reconciliation is complete (a for-loop fix that silently rewires ABI is not
    safe to ship alone). A surfaced divergence-list is a valid deliverable.

### A gates (FULL suite — frontend change, whole-corpus blast radius)
`cargo test --lib` GREEN (baseline 1072/0); FULL `cargo test --test integration
--release -- --test-threads=4` ZERO new failures; `bootstrap_fixed_point` GREEN;
`c_emit_comparison` ≥887 / `lowerer_comparison` ≥958 (count); `self_host_runtime`
regressed=0; `runtime_diff` MATCH ≥334 (report the total + any change). PLUS the
A2 byte-level content diff (the real catcher).

## ───────── PHASE 2 (only if Part A landed clean): Part B-a (self-host) ─────────
### B-a — register composite-init module-level statics (`lower.gg`)
The `IStaticDecl` handler (`~:11579-11613`, the `else: pass` at :11610) +
`lower_static_ref_ident` (`~:3370-3377`, returns -1 for unregistered → `[bug]`)
register ONLY int/float/runtime-call/None statics. Add registration for a
`static Vector[T] X = [literal]` (composite initializer) so the static-ref emits a
properly `Vector__T`-typed local. Mirror how Rust registers a static GlobalRef
with its `Vector__T` type. ⚠ **There is NO self-host `lower_for` change** — the
self-host `lower_for` (`lower.gg:8415-8505`) is type-name-driven
(`local_type_name(coll_local)`→`resource_meta_for`→`collection_kind`); once the
static-ref local is correctly typed `Vector__T`, the loop routes to
`lower_for_vector` automatically. (Review-confirmed; the v1 "port the place-guard"
instruction was a non-fix — DROPPED.)
- ⚠ **Registration must EMIT the literal storage, not just type-register it**
  (review note 2): `static_put` (`gir.gg:~501`) makes `local_type_name`→`Vector__T`
  and routes the loop, but the composite static's literal initializer
  (`[Decl("slice",true), …]`) must also be EMITTED as global storage in the C —
  else the loop iterates a correctly-typed but uninitialized/garbage static. The
  runtime verification below (`static_collection`/`static_vec_literal` MATCH +
  `/tmp/static_for_repro.gg`→`slice/trim/2`) covers this; confirm the emitted C
  contains the static's element data.
- Verify by RUNNING through the driver: `static_collection` + `static_vec_literal`
  → MATCH (they use `.get()`/`.len()`/`.push()` on a static Vector → gated on
  registration, NOT the for-loop); `/tmp/static_for_repro.gg` via the driver →
  `slice/trim/2`.

### B gates
Force-rebuild driver; `self_host_runtime` regressed=0 → regen (report adds AND
drops — a both-wrong for-over-static snapshot may go stale vs the corrected oracle;
the stability-gated regen validates against the LIVE `gg run` oracle and drops
non-matching, which is honest de-inflation, NOT a regression); `runtime_diff`
report new total; `lowerer`/`c_emit` ≥ baseline; `bootstrap_fixed_point` GREEN.

## Staging / commits (review #7)
1. Part A in ISOLATION → A2 content diff → DECISION GATE. Commit A ONLY if clean.
2. (If A landed) Part B-a as a SEPARATE commit → regen snapshots.
3. Do NOT couple them into one commit; do NOT touch the self-host until A is green
   AND content-verified. If A2 diverges, the chain's deliverable is the divergence
   report + an UNCOMMITTED A1 patch (or A1 committed only after reconciliation).

## Worktree discipline (executor)
`pwd` + `git rev-parse --show-toplevel` FIRST; inside your worktree, NEVER
`/workspace/gorget-1`. `git merge --ff-only gorget-1`. Stage ONLY the exact
touched paths (`src/ir/lowering/stmts/for_loops.rs`; then `lower.gg` + the
new/removed `runtime_snapshots/*.out` via `git add`/`git rm`); NEVER `git add -A`.
Do NOT merge to gorget-1 — leave commits on your branch; the orchestrator
integrates. If A2 surfaces the table divergence, STOP and REPORT (that's a
SUCCESS — a clean surfaced blocker beats a silent ABI rewire).

## Report
Per phase: commit hash(es) or "uncommitted (A2 diverged)"; the exact edits +
final line numbers; the A2 byte-level content-diff result (which Box/Mutex/string
symbols changed, or "clean"); all gate results; the parity total + snapshot
adds/drops; and — if A2 diverged — the table-vs-fallback divergence table + a
recommendation (is the table or the fallback the intended truth per symbol?).
