# Executor brief: RV-F — four ggdef oracle divergences (liveness / Copy / Callable)

> **Status:** v0 — awaiting ≥3 sequential fresh brief-reviews (fold after each; stop only on a
> clean pass). **Scout basis (read both FIRST):**
> `docs/plans/define-gorget/scouts/scout-rvf-oracle.md` (premise table, the calibration
> findings, position matrix) + the PROVEN patch
> `docs/plans/define-gorget/scouts/patches/rvf_proto.patch` (132 lines,
> `spec/ggdef/src/elaborate/{mod.rs,liveness.rs}` only; full ggdef suite green; 32/33 repros
> agree with production; converter_agreement AGREE 198 unchanged; zero committed-fixture flips).
> **Model policy:** executor + brief-reviews Opus; output-review on Fable.

## Objective

Fix the four confirmed oracle divergences so ggdef models the RATIFIED rules (production is
corroboration, not justification — but here each fix was calibrated against production AND the
ledger): (#11) Copy axis = production's `is_copy_type` extent via a new `ty_is_copy()`
(Prim + tuple-of-Copy + non-tainted all-Copy-fields struct/enum; Option/Result are NOT Copy;
tainted never Copy) at BOTH consumer sites; (#13) assign-revive also seeds the innermost
`loop_locals`; (#14) the for-element var is NOT loop-local — keyed on the TYPED
`Source::BorrowView` (only the for-desugar emits it; user binds are `Source::Copy`) — no shape
heuristics; (#15) single-owner Callable bare-init rejection at the FULL position class (bind /
whole-reassign / struct-literal / struct-ctor / enum-variant — mirroring production's
`require_explicit_move_for_single_owner_init` sites), and NOT at return/push/collection-
literal/capture (production accepts bare `return f` for callables — the drop-taint axis
differs at return; do not blur them).

## Milestones

1. **M1** — apply the proven patch (`git apply --check`; re-read hunks on drift). The #15
   full-class shape is the RATIFIED scope (orchestrator endorses the reference-grade
   completion over the literal one-position filing). Checkpoint /tmp/recover_rvf_exec_1.patch.
2. **M2 — fixtures pinning BOTH directions per position** (from the scout's plan): #11
   `f(&x, x.p)` all-scalar POS + non-Copy field NEG(E_BorrowConflict); #13
   reassign-then-move-in-loop POS; #14 `for x in v: sink(!x)` NEG(E_MoveInLoop) + fresh-local
   move-in-body POS; #15 NEGs at bind/ctor/enum-variant + POS at `!f`-bind / `return f` /
   `v.push(f)`. Populate `expect:` via `ggdef gen`; every new fixture must ALSO pass the
   production `spec_conformance` lanes (they are four-lane spectests — invariant #9).
3. **M3 — gates (FOREGROUND):** full `cargo test -p ggdef` (all 7 test files) · main-repo
   `GG_BUILD_TIMEOUT_SECS=600 cargo test --test spec_conformance -- --test-threads=1
   --nocapture` (expect the floors + the new fixtures green on all lanes — print totals) ·
   `cargo test --lib` (Rust-side untouched, cheap insurance). NOT bootstrap-gated.

## Out of scope / zone carve

The `Stmt::While` arm (`liveness.rs:~539`) is RV-H's seam — the patch touches nothing ≥ line
~490 in liveness.rs; keep it that way. The `.cb()` callable-field subset gap (filed — the
axis-extension track). The compound-assign double-eval (RV-C's ggdef leg). spectests/ beyond
the new fixtures.

## Process contract

Standard (worktree-verified pwd; no stash; explicit staging; /tmp checkpoints; retry transient
cargo errors; Edit-tool-only writes). Commit when green
(`fix(ggdef): RV-F — oracle divergences: Copy axis, loop revive-seeding, for-var MoveInLoop,
Callable single-owner init class`), trailers: Co-Authored-By Claude Opus + Claude-Session.
Report any NEW pre-existing bug (file-don't-fix).

## Acceptance

All four divergences closed with both-direction four-lane fixtures; full ggdef suite green;
spec_conformance green all lanes; converter AGREE unchanged; liveness.rs While-seam untouched;
2 files in src-space + fixtures.
