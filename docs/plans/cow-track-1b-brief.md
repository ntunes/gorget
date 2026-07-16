# Executor brief: CoW Track 1B — self-host value index-element field write-through

> **Status:** v0 — awaiting ≥3 sequential fresh brief-reviews (fold after each; stop only on a
> clean pass). **Campaign:** `cow-writethrough-materialize-closed-set.md` (v3) — this is the
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

In the self-host lowerer's place resolution: when a field-store base is an Index expression on
an Array-kind collection, resolve the element as a **Ptr place** (the self-host equivalent of
`gorget_array_get_ptr`), emit the field store THROUGH that pointer, and **untrack the transient
element CoW handle** (the same class as Rust's `untrack_transient_element_refs_in_range` —
a missed untrack here is a use-after-free; an over-track is a double-drop).
Do it in the shared place-base path (`lower_place_base`), NOT per-statement-kind — Track 1C
(Dict/Set) and 2F (nested `&`) will extend the same resolver; leave the Index arm shaped so a
Map/Set kind can be added without copying the logic (Core #4).

Primary file: `tests/fixtures/self_host_lowerer/lower_stmt.gg` (place base + field write,
~1514–1638). Possibly `lower_expr.gg` if the index helpers live there. Do NOT touch
`lower_loops.gg` (Track 1A's zone) or the typechecker dir (two other tracks are active there —
coarse-kind split in `typecheck.gg`, tuple-DefId in `parser.gg`/`resolve.gg`; stay out).

## Milestones

1. **M1 — the fix** in `lower_place_base` (+ helpers). Checkpoint
   `git diff > /tmp/recover_cow1b_m1.patch`.
2. **M2 — corpus fixture promotion**: create `tests/fixtures/cow_value_index_field_writethrough.gg`
   (+ `.expected`) from the body of the inline Rust test `rust_value_index_element_field_writethrough`
   (`tests/integration.rs:~21089`); adjudicate expected output with ggdef per above; wire the
   standard three-lane integration tests (C + LLVM + self-host runtime; follow an existing
   `cow_*` fixture's wiring). Keep or delete the inline Rust test per what the wiring covers —
   if the corpus fixture fully covers it, delete the inline twin (no duplicate assertions).
   Verify new files are not gitignore-hidden (`git status` must show them).
3. **M3 — negative-space probes** (cheap, in-worktree, not committed unless they find something):
   multilevel `v[i].inner.f = x`, `v[i].f.push(y)` (projected chain through the element),
   compound `v[i].f += 1` — confirm write-through lands or report the residual shape (report,
   don't scope-creep the fix).
4. **M4 — gates (FOREGROUND, generous timeouts; chunk >600s gates by test name)**:
   rebuild the self-host lowerer driver (`GG_BUILD_TIMEOUT_SECS=600`) · the new fixture green on
   all three lanes · `self_host_runtime` targeted run · **ASan** on the new fixture + the
   existing multilevel `cow_index_*` fixtures (UAF/double-free on the untrack path is the #1
   risk — a leak-check needs a positive control before trusting "clean") · `cargo test --lib` ·
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
