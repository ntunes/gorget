# Executor brief: CoW Track 1B — self-host value index-element field write-through

> **Status:** v1 — pass-1 review folded (5 reservations: fixture SCOPED to M1-covered shapes —
> the full inline-test body would MISMATCH the auto-enrolled self-host lane on the nested +
> value-field-method shapes and RAISE parity WRONG; the untrack/`gorget_array_get_ptr` framing
> corrected — no such symbol exists, no self-host untrack pass exists; static base added to the
> arm's scope; wave-3 lint deferral made explicit; lower_expr.gg edits flagged). Awaiting the
> next fresh pass. **Campaign:** `cow-writethrough-materialize-closed-set.md` (v3) — this is the
> campaign's designated first PR. **Wave-0 basis:** `cow-wave0-measure.md` — gap B re-verified
> broken this session (self-host only; Rust GIR is correct).
> **Model policy:** executor + brief-reviews Opus; output-review before integration on Fable.

## Objective

`v[0].x = 88` on a Vector element (value struct) SILENTLY LOSES the write in the self-host
lowerer: the driver reads the element via `gorget_array_get` (a VALUE COPY), writes the field
into the copy, and the store dies — reading back gives the stale value. Rust gg is correct
(R39-T1: element Ptr + deref-store). Fix the self-host to match, and promote the Rust-only
inline regression to a three-lane corpus fixture.

Spec: `docs/language-design.md` §3.1 — an owned place (`v[i].f`) is an unbroken chain to the
owner → **write-through**. Expected output derivation per the campaign's principle 1: adjudicate
the new fixture with **ggdef first** (if the vector-index field-store shape is in ggdef's
subset, the ggdef verdict IS the expected stdout; if out-of-subset, note that in the fixture
comment and derive from §3.1 — never from what a compiler happens to print).

## Root cause (scout-verified 2026-07-16 + plan's code map)

`lower_place_base` (`tests/fixtures/self_host_lowerer/lower_stmt.gg:1514`) special-cases only
Identifier/static/Deref bases; an **Index** base falls through to `lower_expr` → value copy →
the subsequent field write targets the copy. Wave-0 measured: driver emits `gorget_array_get`
where Rust GIR emits the `get_ptr` + deref-store shape.

## Fix shape (mirror Rust R39-T1 — fix the CLASS at the place resolver)

**Mechanism (pass-1-corrected — the campaign plan's `gorget_array_get_ptr` is an ASPIRATIONAL
name from a comment at `src/ir/lowering/exprs/mod.rs:2543`; no such symbol exists anywhere):**
Rust's Index arm (`exprs/mod.rs:2553-2595`) emits `Inst::ElemPtr` (inline `base + idx*elem_size`).
The self-host's real equivalent is what its resource-element READ path already does at
`lower_expr.gg:4971-4973`: call `gorget_array_get` (which returns a pointer INTO the buffer —
`runtime_array.c:31-36`) with the dst typed **`GtPtr(elem)`** and tagged **`LoBorrowed`**.
Mirror that in `lower_place_base`'s new **EIndex arm**: element place = `gorget_array_get` →
`GtPtr(elem)` dst, `LoBorrowed` tag, field store THROUGH the pointer. There is NO self-host
untrack pass to call (Rust's `untrack_transient_element_refs_in_range`, `context.rs:2543`, has
no twin) — the `LoBorrowed` tag IS the untrack-equivalent: it keeps the element Ptr out of
drop-tracking. **The real risk is mis-tagging the dst `LoOwned` → the buffer element gets
drop-registered → double-free — this is what the ASan gate is for.**

**Static bases are in scope (pass-1):** the Rust arm explicitly materializes a `GlobalRef` base
(`exprs/mod.rs:2559-2569`); the fixture body includes `static Vector[Point] PTS` shapes. Handle
a static collection base in the new EIndex arm (mirror the Rust shape) and verify
`PTS[0].x = 99` / `PTS[1].x += 100` end-to-end on the driver. If the static path turns out to
be structurally larger than a mirror (report the evidence), scope the fixture to locals and
file the static case — do not silently drop it.

Do it in the shared place-base path (`lower_place_base`), NOT per-statement-kind — Track 1C
(Dict/Set) and 2F (nested `&`) will extend the same resolver; leave the EIndex arm shaped so a
Map/Set kind can be added without copying the logic (Core #4). The place-resolver
`CollectionKind`-exhaustiveness arm-count lint is DEFERRED to campaign Wave 3 by plan — noted
here so the class-guard is visibly tracked, not forgotten.

Primary file: `tests/fixtures/self_host_lowerer/lower_stmt.gg` (place base + field write,
~1514–1638; the EIndex fall-through to `lower_expr` is at `:1550-1552`). Keep the fix in
`lower_stmt.gg` where feasible; if you must touch `lower_expr.gg` (shared with future tracks
1A-SH/2D/2E), FLAG it in your report for the parent's integration sweep. Do NOT touch
`lower_loops.gg` (Track 1A's zone) or the typechecker dir (two other tracks are active there —
coarse-kind split in `typecheck.gg`, tuple-DefId in `parser.gg`/`resolve.gg`; stay out).

## Milestones

1. **M1 — the fix** in `lower_place_base` (+ helpers). Checkpoint
   `git diff > /tmp/recover_cow1b_m1.patch`.
2. **M2 — corpus fixture promotion, SCOPED (pass-1: promoting the FULL inline-test body would
   MISMATCH the self-host lane and RAISE the parity WRONG count — the opposite of the campaign
   goal):** create `tests/fixtures/cow_value_index_field_writethrough.gg` (+ `.expected`)
   containing ONLY the M1-covered shapes from the inline Rust test
   `rust_value_index_element_field_writethrough` (`tests/integration.rs:21130`, body at
   `:21169-21197`): the plain (`v[0].x = 88`, `PTS[0].x = 99`) and compound (`v[1].y += 5`,
   `PTS[1].x += 100`) single-level index-element field stores. The NESTED (`ns[0].inner.val = 99`)
   and value-field-METHOD-receiver (`hs[0].c.bump()`) shapes route through
   `lower_place_base(EFieldAccess-base)` — NOT fixed by the EIndex arm — so they STAY in the
   inline Rust test: **KEEP the inline twin** (its comment already documents why it's not
   corpus), and file the two residual shapes as the follow-up they are (they belong to the
   nested-place class, Track 2F's neighborhood). Adjudicate the fixture's expected output with
   ggdef per above; wire the standard three-lane integration tests (C + LLVM; the self-host lane
   auto-enrolls via the runtime-parity corpus). Verify new files are not gitignore-hidden
   (`git status` must show them).
3. **M3 — negative-space probes** (cheap, in-worktree, not committed unless they find something):
   multilevel `v[i].inner.f = x` (expected: still broken — confirm + report, it's 2F's class),
   `v[i].f.push(y)` (projected chain through the element), compound `v[i].f += 1` — confirm
   write-through lands or report the residual shape (report, don't scope-creep the fix).
4. **M4 — gates (FOREGROUND, generous timeouts; chunk >600s gates by test name)**:
   rebuild the self-host lowerer driver (`GG_BUILD_TIMEOUT_SECS=600`) · the new fixture green on
   all three lanes · `self_host_runtime` targeted run · **ASan** on the new fixture + the
   existing multilevel `cow_index_*` fixtures (the #1 risk is mis-tagging the element-Ptr dst
   `LoOwned` → drop-registered buffer element → DOUBLE-FREE; a leak-check needs a positive
   control before trusting "clean") · `cargo test --lib` ·
   `lower_comparison`/`type_comparison` diagnostic runs (print the counts) · targeted
   `cow_*` integration filter.

## Out of scope

- Dict/Set element field stores (Track 1C), `for x in &coll` (Track 1A), nested-`&` (2F),
  plain-`self` (2E). If your fix naturally generalizes, leave the seam clean and NOTE it.
- Any Rust-side lowering change (Rust is correct here).
- The bootstrap fixed-point + full C/LLVM sweeps + parity regen: the PARENT runs these
  (combined with the other in-flight tracks). You run build + targeted + lib only.

## Process contract (non-negotiable)

Run `pwd` and `git rev-parse --show-toplevel` FIRST and confirm both point inside your worktree.
NEVER touch `/workspace/gorget` (main) or `/workspace/gorget-1`; worktree-relative paths only
(your worktree nests UNDER the main checkout — an absolute `/workspace/gorget/...` path writes
into MAIN). NEVER `git stash`; checkpoint to /tmp after every milestone. Stage by EXPLICIT file
name only. On an Edit-tool desync, re-Read and retry the Edit tool — never a shell heredoc with
an absolute path. Commit when green (`fix(self-host): ...`). Report any NEW pre-existing bug
(file:line + repro) — file-don't-fix.

## Acceptance

`v[0].x = 88; print(v[0].x)` prints `88` through the self-host lowerer; the promoted fixture is
green on C + LLVM + self-host runtime lanes with a ggdef-adjudicated (or explicitly
prose-derived) expected file; ASan clean on the new + multilevel index fixtures; lib green;
comparisons not regressed; zero changes outside the lowerer dir + the fixture/test wiring.
