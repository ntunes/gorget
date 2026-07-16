# Executor brief: RV-F — four ggdef oracle divergences (liveness / Copy / Callable)

> **Status:** v4 — ✅ PASS 5 SIGNED OFF CLEAN (2026-07-16; 13-cell matrix all-agree on both compilers; gauntlet 1:2res 2:1res 3:1res 4:1res 5✓) — **EXECUTING**. NEG-fixture convention (pass-5 FYI): `ggdef gen` auto-populates POS expects only; NEG fixtures are hand-authored per the reject_move_in_loop.gg convention (`reject: E_X` + `exit: 1` + `stdout: ""`) — the brief enumerates every NEG's exact code. Pass-4 folded (1: the two patches DO NOT COMPOSE — `rvf_paramgate_r3.patch`
> is a SUPERSET re-derivation of `rvf_proto.patch` (same base blobs; overlapping hunks) — the
> executor applies **r3 ALONE**; pass-4 re-proved the FULL probe matrix on r3-alone, suite
> 140/0 green. Notes: fixture shapes use vector LITERALS (`Vector()` ctor is out of ggdef's
> subset — follow closure_compose.gg's idiom); the #11 Copy-field POS fixture should pick a
> shape the SELFHOST lane already accepts (existing copy_struct_*.gg shapes — the self-host
> Copy-axis twin is RV-D's zone; never weaken ggdef to dodge a lane); the mod.rs:2036-2051
> cite is the POST-patch range (pre-patch: 1925-1951).) Pass-3 folded (1 BLOCKING: the match-binding REJECT cell was UNSATISFIABLE —
> ggdef never TYPES match-arm bindings (mod.rs:2036-2051), so the type-gated reject can never
> fire there; proven orthogonal to the param gate (s7 survives param_names emptied). The cell
> is DROPPED from M2 and the substrate gap FILED (MED, pairs with the axis-extension track).
> Pass-2's "(proven by probe s6/s7)" was HALF-right: the param-gate fixes s6 (for-var, typed,
> Borrow-exempt), NOT s7 (untyped binding — different cause). Minor: fixtures MUST use the
> `Callable[...]` spelling — bare `int(int)` under-rejects in ggdef (pre-existing, filed LOW,
> out-of-scope line added). The pass-3 reviewer's param-gate prototype is captured at
> `scouts/patches/rvf_paramgate_r3.patch` — the executor starts from it.) Pass-2 folded (1 BLOCKING: the R1 `local_mode != Borrow` discriminator was
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

1. **M1** — apply **`docs/plans/define-gorget/scouts/patches/rvf_paramgate_r3.patch` ALONE**
   (`git apply --check` first). It SUBSUMES rvf_proto.patch's #11/#13/#14/#15 changes PLUS
   the R1 param-gate — do NOT also apply rvf_proto (same base, they don't compose; pass-4
   proved the failure and re-proved the full matrix on r3-alone). The #15 full-class shape is
   the RATIFIED scope. Checkpoint /tmp/recover_rvf_exec_1.patch.
2. **M2 — fixtures pinning BOTH directions per position** (pass-2-corrected): r3 ALREADY CONTAINS the R1 param-specific gate (the `param_names`
   set populated in the param-binding loop) — VERIFY it is present after M1; your M2
   deliverable is the FIXTURES. Do NOT re-derive the gate and do NOT use `local_mode` (Borrow also marks for-vars/match-bindings/self — production skips
   ONLY params, `check_stmt.rs:1464 !def.is_param`). Bind/assign sites skip param sources;
   ctor sites unchanged. Fixtures pinning TWO cells: callable-PARAM bare-bind → ACCEPTED; callable **for-var**
   bare-bind → REJECTED (E_MoveWithoutOperator). The match-binding cell is OUT (filed —
   untyped-binding substrate gap; do NOT attempt match-arm type inference here). ALL fixtures
   use the `Callable[...]` spelling, never bare `int(int)` (filed under-reject). Then: #11
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

- Auto-inferred closure-literal sources for #15 AND match-arm pattern bindings AND the bare
  `int(int)` function-type spelling (all ggdef typing-substrate gaps — filed; the
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
