# Executor brief: RV-F — four ggdef oracle divergences (liveness / Copy / Callable)

> **Status:** v2 — pass-2 folded (1 BLOCKING: the R1 `local_mode != Borrow` discriminator was
> TOO BROAD — `Borrow` marks {param, for-var, match-binding, self} while production skips ONLY
> params; the gate as-folded wrongly ACCEPTS for-var/match-binding callable binds that
> production REJECTS (proven by probe: s6/s7) — a wrong-ACCEPT the whole suite misses. The
> discriminator is now PARAM-SPECIFIC. Minor: the `!f`-bind/`return f` POS fixtures must use
> LOCAL owned sources — a param `!f`-bind diverges, production rejects E_UseAfterSourceMoved.)
> Pass-1 folded (R1 BLOCKING: the patch's #15 bind/assign reject must gate on
> `local_mode != Borrow` — a callable PARAM bare-bind is production-ACCEPTED and the patch
> as-proven over-rejects it (a divergence the patch CREATED; the green suite missed it — the
> corpus doesn't exercise it); ctor/enum/struct-literal sites correctly do NOT exclude params
> (production rejects `Holder(f)` for params too). R2 BLOCKING: `v.push(f)` dropped from the
> POS set — it check-accepts then ICEs production's shared lowering (filed HIGH, ≥2 bugs);
> use `!f`-bind + `return f` (both verified build+run) and/or the closure-LITERAL push form.
> R3: acceptance language carved — #15 closes for EXPLICITLY-TYPED callable sources;
> auto-inferred closure literals are out-of-subset in ggdef's typing (noted, not chased).
> Minor: Channel/Shared/Weak/Mutex Copy-treatment is a latent pre-existing subset divergence —
> out-of-scope line added.) Awaiting the next fresh pass. **Scout basis (read both FIRST):**
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
2. **M2 — fixtures pinning BOTH directions per position** (pass-2-corrected): the executor
   FIRST adds the R1 gate as a **PARAM-SPECIFIC** discriminator — a `param_names` set (or
   equivalent typed param bit) populated in the param-binding loop at `mod.rs:341-362`; do
   NOT use `local_mode` (Borrow also marks for-vars/match-bindings/self — production skips
   ONLY params, `check_stmt.rs:1464 !def.is_param`). Bind/assign sites skip param sources;
   ctor sites unchanged. Fixtures pinning ALL THREE cells: callable-PARAM bare-bind →
   ACCEPTED; callable **for-var** bare-bind → REJECTED (E_MoveWithoutOperator); callable
   **match-binding** bare-bind → REJECTED (production probes s6/s7). Then: #11
   `f(&x, x.p)` all-scalar POS + non-Copy field NEG(E_BorrowConflict); #13
   reassign-then-move-in-loop POS; #14 `for x in v: sink(!x)` NEG(E_MoveInLoop) + fresh-local
   move-in-body POS; #15 NEGs at bind/ctor/enum-variant + POS at `!f`-bind / `return f` **with LOCAL owned sources only** (a param `!f`-bind
   diverges — production rejects E_UseAfterSourceMoved; NOT `v.push(f)` — filed ICE; the
   closure-LITERAL push form is acceptable if a push POS is wanted). Populate `expect:` via `ggdef gen`; every new fixture must ALSO pass the
   production `spec_conformance` lanes (they are four-lane spectests — invariant #9).
3. **M3 — gates (FOREGROUND):** full `cargo test -p ggdef` (all 7 test files) · main-repo
   `GG_BUILD_TIMEOUT_SECS=600 cargo test --test spec_conformance -- --test-threads=1
   --nocapture` (expect the floors + the new fixtures green on all lanes — print totals) ·
   `cargo test --lib` (Rust-side untouched, cheap insurance). NOT bootstrap-gated.

## Out of scope / zone carve

- Auto-inferred closure-literal sources for #15 (ggdef typing gap — noted in acceptance; the
  axis-extension track's neighborhood). Channel/Shared/Weak/Mutex Copy-treatment (latent
  pre-existing subset divergence; not corpus-exercised).
- The filed `v.push(f)` production ICE (its own track).

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

Divergences #11/#13/#14 closed universally; #15 closed for explicitly-typed callable sources (auto-closure carve noted in fixture comments) with the param-gate NEG-accepts pinned; both-direction four-lane fixtures; full ggdef suite green;
spec_conformance green all lanes; converter AGREE unchanged; liveness.rs While-seam untouched;
2 files in src-space + fixtures.
