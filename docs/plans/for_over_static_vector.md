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
stage-0 all run the Rust-compiled driver). The table is **KNOWN to diverge** from
the `build_resource_metadata` fallback (`lir_lower.gg:208-404`): e.g. `Box__T`
free/clone `Some(...)` vs fallback `None`; `Mutex__T` `runtime_name="Mutex"`/
`c_typedef_name="GorgetMutex*"` vs fallback `"Ptr"`/`None`; bare `Box` size 16
vs 8; `GorgetString` `method_prefix=Some("gorget_str")` vs `None`. These fields
ARE consumed (`runtime_name`→`type_runtime_map` :725/733; `method_prefix`→
dispatch :442/457/824; `box_kind`→:621/811), so activation WILL change emitted C
for Box/Mutex/string code.
- ⚠ **The standard gates MISS this:** `c_emit_comparison` compares only user-fn
  COUNT (not content); `bootstrap_fixed_point` checks only stage-N==stage-N+1
  STABILITY (a *stable* divergence converges GREEN). So a metadata-only body/
  typedef/dispatch change is INVISIBLE to them.
- **REQUIRED — byte-level content diff:** build the Rust driver BEFORE and AFTER
  A1 and diff `--lir-c` (or `--emit-c`) output for a Box-using, a Mutex-using, and
  a String-using fixture (pick ones that exercise those symbols; e.g. a `Box[T]`
  fixture, `sync_*`/`shared_*`, a string-method fixture). Report EXACTLY what
  changes.
- **DECISION GATE:**
  - If the content diff is CLEAN (table == fallback for every exercised symbol) AND
    the full suite is green → **land Part A**, proceed to Phase 2.
  - If it DIVERGES (near-certain for Box/Mutex/string) → **STOP. Do NOT force, do
    NOT reshape the table.** Report: which symbols diverge, which direction (does
    the table's richer metadata look MORE correct than the fallback's `None`/`Ptr`,
    or are the fallback's values load-bearing?), and whether any corpus fixture's
    RUNTIME output (`self_host_runtime`/`runtime_diff`) actually changes. The
    RESOURCES-table-vs-fallback reconciliation is then the REAL next chain (decide
    the intended metadata per symbol, fix the divergence, THEN activate). A1 stays
    UNCOMMITTED until the reconciliation lands (a correct for-loop fix that
    silently rewires Box/Mutex/string ABI is not safe to ship alone).

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
