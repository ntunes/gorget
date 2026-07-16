# Scout Report — the "codegen HIGH family" (Bug 1 `is Some(x)` mis-bind + Bug 2 Dict.remove coalesce)

Agent `a73023187907c0255`, 2026-07-16. **Finding: TWO DISTINCT ROOTS, not one family — SPLIT.**
Bug-1 prototype `scouts/patches/is-scrutinee-single-eval-proto.patch` (6 files, +146/−8; applies CLEAN to main).

## VERDICT
- **Bug 1 — FIXED + MEASURED green on BOTH backends.** Root is a **GIR-lowering double-evaluation**, NOT the
  C-backend / coalescing (the TODO's hypothesis was wrong). Fix at the write site; `resolve.gg` workaround
  retired. Ready to brief.
- **Bug 2 — NOT the same root, currently DORMANT.** A distinct C-backend defect that no longer reproduces
  (commit `567f053e` shifted the coalesce layout). SEPARATE track, and the "coalesce" attribution must be
  re-litigated (red-herring precedent) before a fix is designed.

## Bug 1 — the double-eval (backend-agnostic; one fix corrects C + LLVM)
An `is`-scrutinee in an `if`/`elif`/`while`/`and`-chain condition was lowered **twice**: once as the boolean
tag-test (`Expr::Is` value lowering, `src/ir/lowering/exprs/mod.rs:791`) and again to extract the payload
binding (`emit_is_bindings` RE-lowered the scrutinee, `src/ir/lowering/stmts/mod.rs`). For a side-effecting
scrutinee (a mutating `&self` method returning `Option`) the method is **called twice**, and the payload binds
from the SECOND call. The self-host "mis-bind to 0" was the second `scopes.define(...)` hitting the
already-defined `define_replace_or_fail` path. `match` was always correct (it lowers its scrutinee once).
Proven with a side-effect counter: `define()` printed 101 then 102; `x` bound to the second call.

### The fix (grounded in `docs/devbook/24` layering + "fix at the write site")
Evaluate the scrutinee ONCE, memoize the single scrutinee local keyed by the `Expr::Is` node's span, and have
`emit_is_bindings` **reuse** it — mirroring how `match` lowers its scrutinee once. Three parts:
- `src/ir/lowering/context.rs`: per-function `is_scrut_memo: FxHashMap<usize,(LocalId,TypeId)>` (auto-cleared).
- `src/ir/lowering/exprs/mod.rs:791` (`Expr::Is` value lowering): record `(scrut_local, scrut_type)` under
  `expr.span.start` for non-negated nodes.
- `src/ir/lowering/stmts/mod.rs` (`emit_is_bindings`): **READ (not remove)** the memo and reuse the local;
  fall back to re-lower only on a miss. Read-not-remove is load-bearing — an `and`-chain binds its LEFT operand
  in TWO dominated blocks (`lower_short_circuit`'s rhs block + the outer then-block); removing on first read
  re-triggered the double-eval on the left operand (measured `1001,1002` → fixed to single `1001`).

### Measured (both backends C + `GG_BACKEND=llvm`, before→after)
- `if m.method() is Some(x)`: side-effect count 2→**1**; payload 60/43,44,45 → **59/42,43,44**.
- `while … is Some`, `elif … is`, `and`-chain (`d.step() is Some(a) and e.step() is Some(b)`), nested 3-way
  and-chain: each scrutinee fires exactly once. `match` control: unchanged (correct).
- New fixture `tests/fixtures/is_scrutinee_single_eval.gg` (wired in `tests/integration.rs`): green C + LLVM.
- `cargo test --lib`: **1107/0.** Targeted integration subset (is_bindings/is_pattern_binding/pattern_is/
  match_option_result/paren_as_and_if_oneliner/…): green both backends.
- `resolver_comparison`: **1545/1399 matched / 146 mismatched / 0 crashed — IDENTICAL** to the pre-fix
  baseline (the double-eval at the other `scopes.define(...) is Some` sites was benign for COMPARED output; the
  146 mismatches are pre-existing const_match/drop_match, unrelated). So the fix + the retired workaround
  produce identical self-host comparison output.
- **Memory:** GIR-lowering-only; REMOVES a redundant re-lowering (for a method call, re-emitting the whole call
  + its clones) → strictly reduces work; does NOT touch the coalesce path → no clone/RSS concern.

### Workaround retirement (in the patch; bootstrap-gated)
`resolve.gg` `define_pattern_bindings`: the bind-to-local dodge (`Option[int] pat_def = scopes.define(...)`
then `if pat_def is Some(def_id):`) is retired to the idiomatic direct `if scopes.define(name, DkVariable(),
Span(0,0)) is Some(def_id):`, matching the siblings (resolve.gg:419/569/1032). Bootstrap-safe: the self-host
LOWERER already single-evals (its `EIs` value-lowering binds via `lower_pattern_match` at the SINGLE
value-lowering site — `lower_expr.gg:5721-5765`, comment "collapsed into one site"; `lower_if` has no separate
re-lowering pass), so no self-host-lowerer change is needed and the retired resolve.gg single-evals when
compiled by the fixed Rust gg. **Parent gate: `self_host_bootstrap_fixed_point` (self-host source change).**

## Bug 2 — dormant + likely misattributed (SEPARATE track)
The discarded `Dict[_,int].remove` coalesce miscompile (`incompatible types … __gg_Option__int64_t from
int32_t`, workaround comment still at `typecheck.gg:1184-1185`) does NOT reproduce on current source — neither
synthetically nor by inlining `live_reinit`'s discarded `state.moved.remove(key)` back into `check_safety_stmt`.
**Why dormant:** `567f053e` (2026-07-15, the DefId re-key) rewrote the `state.moved`/`live_reinit` code AFTER
the bug was filed (07-14), shifting the coalesce value-layout so the collision no longer occurs; the c_lir
coalesce code itself is unchanged since 07-13. **Caution (red-herring precedent):** DONE.md [2026-07-13]
records a near-identical `… from int32_t` C error (the FieldAccess bug) whose `coalesce_assign_exact` diagnosis
was a RED HERRING — the real defect was the TYPECHECKER accepting a bad program. Bug 2's "coalesce" attribution
must be re-litigated (rule out "typechecker wrongly accepts the discarded remove") before committing to a
coalesce-site fix. A future scout must first construct a RELIABLE repro (the 07-14 trigger layout is gone).
Keep the `live_reinit` workaround (owner-ruled to keep; also dormant → removing it risks re-triggering a latent
miscompile).

## Aside (pre-existing, orthogonal — file LOW)
An expression-position `if` with an `is`-binding (`int r = (if x is Some(v): v else: -1)`) is REJECTED by the
resolver (`v` undefined) → the `emit_is_bindings` sibling at `exprs/mod.rs:4149` never runs with a binding in
valid programs. So that sibling is safe today; but the expr-position `if is`-binding gap is a latent LOW.
