# Executor brief: RV-D — self-host safety-walk soundness cluster (holes #6/#7/#8/#9 + Copy-axis subset)

> **Status:** v1 — pass-1 folded (R1 the 21 probe sources are now COMMITTED at
> `scouts/patches/rvd_probes.tgz` — M2 unpacks them, no re-derivation; R2 the #9 catch-all
> honesty note below; R3 line/count fixups). Pass-1 independently reproduced ALL decisive
> gates (patch clean on tip, 23/23 lanes, 21/21 probes exact, type_comparison ==85).
> Awaiting the next fresh pass. **Scout basis (read both FIRST):**
> `docs/plans/define-gorget/scouts/scout-rvd-safety-walk.md` (premise table, per-hole measured
> before/after, gates) + the PROVEN patch
> `docs/plans/define-gorget/scouts/patches/rvd_proto.patch` (ONE file,
> `tests/fixtures/self_host_typechecker/typecheck.gg`, +160/−45; 23/23 driver lanes green;
> the full ~18K-line frontend compiles with ZERO new diagnostics).
> **Model policy:** executor + brief-reviews Opus; output-review on Fable.

## Objective

Close four soundness holes in the self-host unified safety walk (three UNDER-rejections that
accept use-after-move/dangling-view programs, one OVER-rejection that refuses a legal
move-then-reinit shape) plus the Copy-axis subset broadening — all in one coherent change-set
over the walk, matching production's model per-hole:

1. **#6 closures**: body checked against `safety_snapshot(state)` with loop-ctx reset, then
   discarded — a moved-capture read now rejects E_UseAfterMove (was: fresh empty state).
2. **#7 comprehensions**: snapshot + `loop_depth+1` + fresh loop_locals — `!enclosing` moves
   inside comprehensions now reject E_MoveInLoop; the iteration var is NOT loop-local
   (matching production).
3. **#8 slices**: drop the range-index carve-out in `place_projection_path` — the D10(b)
   mirror now sees slice args (`f(&v, v[0..2])` on non-Copy rejects). ⚠ `expr_is_place`
   keeps ITS range carve-out — production also carves there; the asymmetry is deliberate.
   ⚠ The Copy-element case (`Vector[int]` slice) stays ACCEPTED at parity with production —
   that hole is RV-E's (production-side classifier), applied to both compilers by THAT track.
4. **#9 branch-join**: `safety_commit` REPLACES `state.moved` with the union of
   REACHING-branch end-states (fall-through only when no unconditional else/catch-all —
   the new `elsebranches_have_uncond`/`match_has_catch_all` helpers) — move-then-reinit-in-
   both-arms now accepts. **HONESTY NOTE (pass-1): on the exotic shape "reinit in ALL arms
   incl. a bare `case _:`/`case x:` wildcard arm", the self-host is STRICTLY MORE PRECISE
   than production** (production still pushes the pre-branch fall-through unless there is a
   separate `else_arm` — `check_stmt.rs:1217-1222` — and so over-rejects; the self-host
   correctly treats an unguarded wildcard/binding arm as exhaustive). SOUND (never accepts an
   unsafe program), dormant, and per rust-not-sacrosanct the precision stays; the PRODUCTION
   over-rejection is FILED as its own small entry. Do not "fix" the self-host down to
   production's imprecision, and do not write a production-parity fixture on this shape. The OWNER-RULED-KEEP `live_reinit` workaround (`:1186-1190`)
   stays untouched.
5. **Copy axis (subset)**: `arg_place_is_copy` → recursive `resolved_type_is_copy`
   (scalar/tuple/ref/handle-generics). The struct-of-scalars completion is FILED (needs a
   `DefInfo.is_copy` pass — out of scope here; TODO entry cites the scout).

## Milestones

1. **M1** — apply the proven patch (`git apply --check`; ⚠ coarse-kind landed in this file's
   EMIT sites `c082ae96` and the scout verified no collision — re-verify on the current tip;
   re-read hunks on drift). Checkpoint `/tmp/recover_rvd_exec_1.patch`.
2. **M2 — fixtures**: promote the scout's 21 probes — **unpack the COMMITTED archive
   `docs/plans/define-gorget/scouts/patches/rvd_probes.tgz`** (18 named + 3 diagnostic
   probes; verbatim sources, no re-derivation) — into committed driver
   reject/accept fixtures: per hole ≥1 reject (the soundness shape) + ≥1 accept (the
   over-rejection guard), including the divergence/nesting edges (move-one-arm /
   reinit-one-arm / match-move-all REJECT; reinit-both / match-reinit-else / diverge-reinit
   ACCEPT). Wire into the `self_host_driver_rejects_*`/`accepts_*` suites with exact
   `error[E_<code>]` assertions (the coarse-kind precedent). Fixtures must not be
   gitignore-hidden; `gg fmt`-idempotent.
3. **M3 — gates (FOREGROUND; chunk >600s by test name):** driver rebuild
   (`GG_BUILD_TIMEOUT_SECS=600`) · ALL `self_host_driver_*` lanes (expect 23/23 + the new
   fixtures) · **`self_host_bootstrap_fixed_point` YOURSELF** (single monolithic test,
   `GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600`, no chunking — this track's explicit
   exception: over-tightening the walk = self-host source rejected, and the bootstrap is the
   definitive over-rejection gate) · `type_comparison` (mismatched must stay ≤ the 85
   baseline — print the counts) · `cargo test --lib` · `cargo test --test lints` ·
   `cargo test -p ggdef` (cheap insurance; no spectests are touched).

## Out of scope

RV-E (the production slice-classifier twin — both-compiler fix belongs there); the
struct-of-scalars Copy completion (filed); RV-H (while-condition loop gate — separate
three-lane track; do not touch the While arm's condition handling); the safety-walk EMIT
sites (coarse-kind's landed zone); spectests/.

## Process contract (non-negotiable)

Run `pwd` + `git rev-parse --show-toplevel` FIRST; confirm inside your worktree. NEVER touch
`/workspace/gorget` or `/workspace/gorget-1`; worktree-relative paths only (worktrees nest
UNDER main). NEVER `git stash`; checkpoint to /tmp per milestone. Stage by EXPLICIT file
name. Edit-tool desync → re-Read + retry; never a heredoc with an absolute path. Transient
cargo errors under contention: retry. Commit when green
(`fix(self-host): RV-D — safety-walk soundness cluster (closure state, comprehension loops,
slice places, branch-join union) + Copy-axis subset`), trailers: Co-Authored-By Claude Opus +
the Claude-Session line. Report any NEW pre-existing bug (file-don't-fix).

## Acceptance

All four holes closed with committed both-direction fixtures (soundness rejects + guard
accepts, exact codes); bootstrap fixed-point green; type_comparison ≤85; driver lanes green;
one file changed in src-space (typecheck.gg) + fixtures/tests; zero spectests/floor movement.
