# Scout Report — teach ggdef-elaborate the static may-move dataflow (Phase 2)

Agent `ae8fb6af`, 2026-07-15. Prototype `scouts/patches/ggdef-elaborate-move-proto.patch` (== the scout's
`02-final-complete.patch`; **eval fix + elaborate gate COMBINED**; applies CLEAN to main; 5 files, +731/-9).

## VERDICT: FEASIBLE, lands cleanly, MEASURED. `verdict = check_liveness ∘ eval`.
A ~535-line syntax-directed dataflow in `spec/ggdef/src/elaborate/liveness.rs`, gated at the top of
`eval::run()`. Pure Rust under `spec/ggdef/`, NOT bootstrap-gated, import-fence clean.

## Measured (build + `ggdef run` + `gg check` diff)
- Conditional-move-then-use (`bool f():false` then `if f(): sink(!x)` then `print(x)`) → **IllFormed
  E_UseAfterMove exit 102** (was `Value`) — **the divergence CLOSES, matches `gg check`**.
- straight move-then-read → E_UseAfterMove; double-move → E_DoubleMove; consume-double → E_DoubleMove;
  MoveInLoop for enclosing-var moves in loops — all from ELABORATE, before eval.
- `reinit_accept.gg` → still **Value "new"** (revive works, NOT over-rejected).
- All 15 `tests/fixtures/liveness/*.gg`: **9 reject (102, correct codes) / 6 accept (0), EXACT.**
- **ZERO over-rejection:** full 195-fixture conformance corpus **195/195 MATCH**; ggdef suite **127/0**.
- **GUARD-RAIL — NO production disagreement:** `gg check` vs the gate on **25 programs** → 100% ACCEPT/REJECT
  agreement AND matching error codes (E_UseAfterMove/E_DoubleMove/E_MoveInLoop). Models `:2390` §9.5 exactly
  as production; divergence-filter (guard-clause `else: return`), rebind-guard (`acc=bump(!acc)`), match-arm
  union all agree. **Nothing to escalate for prose adjudication.**

## Substrate (honest)
- GGC is rich enough (stmt sequences, If/While/Loop/Match stmt+expr, `Source::{Move,Copy,BorrowView,
  WriteThrough,Value}`, `CallValue{consumes_callee}`).
- **ONE gap:** elaborate's locals are NAME-keyed (flat `local_ty: HashMap<String,Ty>`, cleared per fn, no
  scope discipline — the collision hazard the self-host/production avoid via DefId). So the pass builds its
  own **tiny lexical resolver** (fresh `BindingId` per binding, innermost-first → correct scope-exit; a
  sibling-scope same-name move never leaks past the join). ~40 of the 535 lines; reference-grade (mirrors
  DefId keying). Verified: sibling-scope + shadow-inner-moved/outer-read cases ACCEPT correctly.
- `!self`-consuming methods need NO special handling (`elaborate_user_method_call` already lowers the
  receiver as `Source::Move` arg0 — left-to-right Move walk handles them, matching production's self-first).
- `for` desugars to `While` with element+body locals bound INSIDE the body → loop-locals → moving `tmp` legal,
  moving enclosing `n` = MoveInLoop. Verified.
- The gate walks ALL functions + closure bodies (required — `apply_once` move-tracks a `ConsumeCallable`
  param). Closure bodies walk with captures+params as fresh live locals; capturing a moved var checked at the
  creation site. **No fixture exercises the closure-capture arm → faithful-but-unproven → add a targeted test.**

## 🔊 Scope-expanding: the STDOUT-semantics shift (owner-awareness; reference-grade default is clear)
A static-before-eval gate rejects BEFORE execution → statically-ill-formed programs now emit **EMPTY stdout**
instead of "output preserved up to the dynamic fault". This is CORRECT (matches `gg check`, which never runs
a rejected program). It flips exactly ONE existing ggdef assertion: `move_then_read_is_illformed`
(`tests.rs:186`) asserted stdout `"hi\n"` (old dynamic preserved-to-fault) → must assert `""`. Grepped: the
only affected assertion (the other two stdout-on-non-Value asserts are Trap outcomes, untouched). NO
conformance fixture affected (all 195 are Value/exit-0). If the owner wants partial-stdout retained as a
distinct diagnostic axis, that's a design call to surface — but matching `gg check` (empty) is the
reference-grade default and what's prototyped.

## Shared transition-table + boundary-note + migration
- **Shared table:** one row per shape, two assertion columns — eval asserts the per-path verdict (`c=false`→
  Value), elaborate asserts the UNION verdict (`c=false`→IllFormed); the branch-merge row differs BY DESIGN.
  Since the gate lives in `run()`, `go(src).outcome` already returns IllFormed for static-reject rows (no new
  harness). Extend the 6 eval-fix tests with conditional-move + both-arms + diverge-filter rows.
- **Boundary-note rewrite:** `verdict = check_liveness ∘ eval`; elaborate owns ALL ratified static rejections
  (UAM, double-move, MoveInLoop, conditional-move-then-use); eval owns per-path dynamic semantics. The old
  escape-hatch list → EMPTY of ownership carve-outs (only honest ggdef *subset* limits remain: generics,
  it-lambdas, B2 constructs). The eval-scout's "dynamic-oracle boundary" paragraph is OBSOLETE for the
  conditional-move class (ggdef rejects it too now).
- **Fixtures → ordinary cross-lane conformance:** conditional-move + consume-double + consuming_self +
  move_in_loop rejects (currently self-host-driver-only in `integration.rs`) now ALSO reject in ggdef →
  migrate to ggdef-adjudicated reject-tier spectests (all lanes agree, no per-lane split). `KNOWN-ORACLE-BUG`
  headers come off. ggdef run-tier floor stays **195** (the gate adds no run-tier fixtures; the +2/195→197 is
  the eval-fix's separate `reinit_accept` migration).

## Size: `liveness.rs` 535 + `mod.rs` +31 + `eval.rs` +51 (run-gate) + `ggc.rs` +7 + `tests.rs` +116. Pure Rust, `spec/ggdef/`, NOT bootstrap-gated.

## Consequence for planning: the combined prototype (eval + elaborate) is one coherent patch → the two-phase split (which existed because Phase 2 was un-scouted) can now MERGE into one "elaborate ∘ eval" landing (owner sequencing call).
