# FIDELITY brief — `EDo` / `EBlock` block-expression lowering (self-host)

## Goal
Make `do:` blocks (and `{}` block-expressions) used as **values** lower to their
tail value, instead of dropping it. Net parity target: **`block_expr`
(WRONG→MATCH) + `test_do_block_expr` (CC-FAIL→MATCH)**, ~+2. This is the next
item in the already-landed `lower.gg` expression-position cluster
(EIf-as-value → EMatch-as-value → **EDo/EBlock**), reusing the helpers those
rounds shipped.

## Verified current state (RUN end-to-end at gorget-1 tip `eb649a04`)
- `block_expr.gg` (uses `do:` = `EDo`): oracle `15 / 9 / 30 / 11 / 20`; self-host
  prints `0 / 0 / 0 / 0 / 0` → **WRONG-OUTPUT**.
- `test_do_block_expr.gg` (uses `do:`): oracle 10 lines (`42 30 10 3 15 25
  "hello world" 30 17 true`); self-host **CC-FAILs** — `error: void value not
  ignored as it ought to be` (the unit/void do-block result assigned into an
  `int`). (String-tail case 7 + bool/int tail cases 9/10 confirm `lower_block_expr`
  must carry the tail's real type — reviewer verified SVarDecl reads the tail
  local's type_id, so this is handled.)
- `test_scope_blocks.gg` uses `scope:`/named-scope blocks (a DIFFERENT feature),
  already WRONG on a separate root — **OUT OF SCOPE**; only requirement is *no
  regression* of its committed snapshot (it is not in the passing set, so a
  no-change is fine; do not try to fix it here).
- Only 3 fixtures use `do:` corpus-wide: those three.

## Root cause (writer-site, single layer)
In `tests/fixtures/self_host_lowerer/lower.gg`, the value-position `lower_expr`
match:
- **`EBlock` arm (lower.gg:5845–5848)** lowers the block via `lower_stmts(...)`
  then returns a **fresh `UNIT_TYPE` local** — the block's tail value is dropped.
- **`EDo`** has **no arm at all** — it falls into the `else` (lower.gg:6015–6019)
  which returns a `UNIT_TYPE` local assigned `OpConstUnit()`.

Both should lower the block via the existing `lower_block_expr` helper
(lower.gg:4134) and return its tail value — exactly how the EMatch arm
(lower.gg:5993–5997 → `lower_match_expr`) and the EIf path already work.

`lower_block_expr` (lower.gg:4134–4149) already: empty-block → unit; else lowers
all-but-last as statements via `lower_stmt`, then the LAST stmt as a tail value
via `lower_stmt_as_tail_value` (lower.gg:4161), which handles `SExpr`, `SIf`
(→ `lower_if_chain_expr`), and `SMatch` (→ `lower_match_expr`). `block_expr.gg`
exercises all four tail shapes (bare-expr, arithmetic, if-tail, match-tail), so
the helper already covers them.

## The fix (mechanical, ~3 lines, no new machinery)
1. Replace the `EBlock` arm body (lower.gg:5845–5848):
   ```
   case EBlock(block_stmts):
       return lower_block_expr(&ctx, block_stmts, &gmod)
   ```
2. Add an `EDo` arm immediately before/after EBlock, mirroring it:
   ```
   case EDo(block_stmts):
       return lower_block_expr(&ctx, block_stmts, &gmod)
   ```
   (Confirm the AST node shape is `EDo(Vector[Stmt])` — the free-var walkers use
   `case EDo(stmts):` at lower.gg:9595/9877/10141, and the `stmt_kind`
   stringifier shows `EDo(_)` at :367. Match that exact constructor arity.)
3. Remove `EDo` from the `else`-arm comment (lower.gg:6016 "Remaining: ESetComp,
   EDictComp, EListComp, EDo, ERange") so the comment stays honest.

Mirror Rust `lower_block_expr` / `Expr::Do` / `Expr::Block` dispatch
(`src/ir/lowering/exprs/mod.rs` — the same `:3203` helper the comments cite).

## Scope discipline / things to NOT do
- Do **not** touch `lower_block_expr`, `lower_stmt_as_tail_value`,
  `lower_match_expr`, or `lower_if_chain_expr` — they are landed and correct.
- Do **not** reshape any fixture or `lib/` to dodge anything (CLAUDE.md "Don't
  redesign around compiler gaps").
- Statement-position `do:`/block (value discarded): after the fix, `lower_expr`
  returns the tail local instead of a unit; the unused local is harmless. BUT
  verify the last stmt being an `SIf`/`SMatch` lowered as a *value* (vs as a
  statement) does not change drop behavior for the discard case — the EMatch/EIf
  value rounds already crossed this and the snapshot net is the guard. If a
  statement-position regression appears, flag it (do NOT paper over).

## Blast radius (pass-2 finding — wider than the 3 `do:` fixtures, net-positive)
`EBlock` is NEVER constructed by the self-host parser (match-arm bodies are
`Vector[Stmt]`, not `EBlock`) → changing it is dead-code-safe. `EDo` is live in
TWO parser productions: value-position `do:` (parser.gg:2428) AND **multi-line
`catch` recovery bodies** (parser.gg:1700-1713), lowered via
`lower_expr(recovery)` in `lower_catch_expr` (lower.gg:8051). Verified by the
pass-2 reviewer (built+ran the patched driver, full passing set **284/284, ZERO
regressions → 286**): `catch_divergent_arm` stays MATCH, **`error_catch_in_loop`
IMPROVES MISMATCH→MATCH**, `trait_method_throws` stays CC-FAIL on a pre-existing
unrelated bug. So the change is net-positive beyond the 3 `do:` fixtures.
Drop-correctness: the EDo/EBlock arm does NOT push a per-block `DSK_BLOCK` scope
(unlike `lower_match_expr` arm bodies), but drop suppression is slot/liveness
driven (a do-block-local resource simply drops at function exit vs block
boundary — invisible to stdout, NOT a double-free; case-7 String tail MATCHes).

## File zone
ONLY `tests/fixtures/self_host_lowerer/lower.gg`. (Disjoint from the PERF and
CLEANUP chains' edit hunks even though CLEANUP also touches this file —
CLEANUP edits line 177 + ctor sites 8899/9093/10347/11891, far from 5845/6015.)

## Gates (all must hold; force-rebuild the driver first —
`rm tests/fixtures/self_host_lowerer/driver{,.c}`)
- `block_expr` and `test_do_block_expr` reach MATCH vs `gg run` (whole stdout).
- No MATCH→worse anywhere: `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` parity
  ≥ 284 (target 286).
- `self_host_runtime` ≥ 284/0 (regenerate the 2 new snapshots via
  `GG_REGEN_RUNTIME_SNAPSHOT=1` for block_expr + test_do_block_expr, byte-exact).
- `lowerer_comparison` ≥ 954, `c_emit_comparison` ≥ 883 (UNCHANGED expected —
  this is a value-position fix; fn-counts shouldn't move).
- `self_host_bootstrap_fixed_point` GREEN (the driver self-compiles `do:`/block
  exprs? grep driver sources — if it does, fixed_point re-converging is the
  load-bearing neutrality proof; if not, it's a pure regression guard).
- `cargo test --lib` 1066/0 (no `src/` touched, so unchanged).
