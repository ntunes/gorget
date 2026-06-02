# Brief — self-host `EIf`-as-expression (if-chain-as-value) lowering

FIDELITY round (1:1:1:1 cadence). Self-host-dir only (Rust gg already correct).
Re-verified by RUNNING the code at `362a54a9`. ⚠ Needs ≥3 fresh sequential reviews
before the executor launches.

## Bug (re-verified: emit-C + cc + run + diff vs Rust)
`test_if_expressions` is **WRONG** (not MATCH — a stale diagnostic-grep said MATCH;
running it proved otherwise). `int x = if true: 10 else: 20` should set `x=10`; the
self-host produces garbage/0. Root: the self-host's `EIf`-as-expression lowering
(`lower.gg:5317-5330`) is a **broken stub**:
```
case EIf(cond_box, then_body, else_branches):
    int cond = lower_expr(&ctx, *cond_box, &gmod)
    ... branch then_bb/else_bb/merge_bb ...
    switch_to(then_bb); lower_stmts(then_body) ; jump merge   # lowers then_body as STATEMENTS, drops its value
    switch_to(else_bb); jump merge                            # else_branches NEVER lowered
    switch_to(merge_bb)
    int ifdst = add_local(I64_TYPE)                           # UNASSIGNED → garbage/0
    return ifdst
```
It lowers `then_body` as statements (not a tail value), never lowers `else_branches`,
and returns an unassigned I64. So EVERY if-expression yields garbage.

This is a BROADLY-broken feature: `EMatch`-as-expression has no `lower_expr` case at
all (falls to `else` → Unit), and `EDo` (do:-blocks, e.g. `block_expr.gg`) is likewise
unhandled. **This round scopes to `EIf`-as-value** (the foundational piece +
`test_if_expressions`); `EMatch`-as-value and `EDo` block-expr REUSE the same helpers
and are logged follow-ups.

## Rust reference (the exact behavior to mirror)
- `build_if_chain_expr` — `src/ir/lowering/exprs/mod.rs:3414-3500`. Lowers cond → branch;
  each branch (then / each elif / else) lowers its block via `lower_block_expr` →
  `assign_match_arm_to_result(result_id, branch_val)` → jump merge; the result-slot type
  is refined from the first non-Unit branch value (or `expected_type`). merge → result.
- `lower_block_expr` — `:3203-3220`: lower all-but-last stmts as statements, then the
  LAST stmt as a tail value via `lower_stmt_as_tail_value`.
- `lower_stmt_as_tail_value` — `:3236-3254`: `Stmt::Expr(e)` → `lower_expr(e)`;
  `Stmt::If{...}` → `build_if_chain_expr(...)` (recursion); `Stmt::Match{...}` →
  `lower_match_stmt_as_expr(...)`; any other stmt → lower as a statement, return None
  (block-as-Unit / no tail value).

## Fix (self-host) — 2 helpers + rewrite the EIf case
**Edit 1 — add `lower_block_expr(ctx, block_stmts, gmod) -> int` to `lower.gg`.** Mirror
Rust `:3203`: **if `block_stmts` is empty → return a Unit local** (the `is_empty()` guard,
Rust `:3208-3210` — avoids a `len()-1` underflow on a degenerate empty block); else lower
`block_stmts[0..n-1]` via `lower_stmts` (as statements), then lower the LAST stmt as a tail
value (Edit 2). Return the tail value's local (or a Unit local if the last stmt produced no
value / returned -1).

**Edit 2 — add `lower_stmt_as_tail_value(ctx, last_stmt, gmod) -> int` (returns -1 for
"no tail value").** Mirror Rust `:3236`:
- `SExpr(e)` → `return lower_expr(&ctx, e, &gmod)`.
- `SIf(cond, then_body, elif_branches)` → call the if-chain-as-value path (Edit 3 — share
  the implementation; the tail `if`/`elif`/`else` chain becomes a value).
- `SMatch(...)` → for THIS round, lower as a statement + return -1 (match-as-value is the
  logged follow-up; do NOT block this round on it). [If cheap + a reviewer confirms a
  reusable match-as-value path exists, may include — else defer.]
- else → `lower_stmt(&ctx, last_stmt, ...)`; return -1.

⚠ **`op_consume` full signature is `op_consume(&ctx, &gmod, val, CkAssign())`** (`lower.gg:1413`)
— the shorthand `op_consume(val, …)` elsewhere in this brief omits `&ctx, &gmod`. ⚠ **Guard
against a `-1` tail value:** if `lower_stmt_as_tail_value` returns -1 (the SMatch/else
deferral), do NOT `GIAssign(result, op_consume(-1, …))` (that emits `OpCopy(-1)` = junk);
treat -1 like the no-value/no-else case (assign the typed default — see Edit 3).

**Edit 3 — rewrite the `EIf` case (`lower.gg:5317`) to produce the branch tail values.**
Mirror `build_if_chain_expr`: a `result` local; lower cond → `GTBranch`; then_bb: lift
`then_body` via `lower_block_expr` → `GIAssign(result, op_consume(&ctx, &gmod, then_val,
CkAssign()))` → jump merge; walk the `else_branches` (elif chain + final else), each:
branch on the elif cond, lift its block via `lower_block_expr` → assign `result` → jump
merge; merge_bb: return `result`. Refine `result`'s type from the first non-Unit branch
value (or `ctx.expected_type` if set — the SVarDecl/SReturn path sets it to the declared
type). ⚠ Guard each `assign + jump` on "block not already terminated" (a branch ending in
`return`/`break` must not also jump to merge — mirror Rust's `!builder.is_terminated()`).

⚠ **NO-ELSE DEFAULT (BLOCKING — pass-2 caught; silently un-tested today).** The self-host
parser emits a `None`-condition (else) `ElseBranch` ONLY for an explicit `else`
(`parser.gg:2501-2505`). With no `else`, `else_branches` is empty/elif-only, so the
all-conditions-false path reaches merge with `result` **NEVER ASSIGNED** → the exact
garbage-I64 bug this round fixes. Mirror Rust `exprs/mod.rs:3493-3496`: at the TERMINAL
else block (after the elif walk, when no `None`-condition branch exists), emit
`GIAssign(result, OpConstI64(0))` + `GTJump(merge_bb)` (or a typed default if
`ctx.expected_type` is a non-I64 — but I64(0) matches Rust). **`result` MUST be assigned on
EVERY path reaching merge** — also covers the `-1` tail-value case above (assign the
default instead of `OpCopy(-1)`). No corpus fixture exercises no-else-if-as-value, so a
miss here is INVISIBLE to the gate — get it right from the spec.

⚠ **Reuse the self-host's existing primitives** (do NOT reinvent): `new_block`,
`switch_to`, `set_terminator(GTBranch/GTJump)`, `add_local`, `GIAssign`, `op_consume`,
`lower_stmts`, `type_id_to_name` — all used by the current EIf stub + the `EIs` merge
pattern (`lower.gg:5558-5602`) and the `unwrap_or` tail-merge (`:4894-4912`). The
result-type refinement: read the then_val local's `type_id` (via `ctx.locals.get`), like
the unwrap_or template sizes `uo_slot_tid`.

## Scope / expected outcome — +1 PARITY (if_expr_resource_arms), foundational
**The clean parity demo is `if_expr_resource_arms.gg`** (verified WRONG at `362a54a9`:
oracle line 1 `pos`, self-host empty; NOT snapshotted). It's a NON-closure if-expr fixture
with RESOURCE (String) branches — so it also exercises the `op_consume` clone path in the
merge (a stronger test than int branches). The EIf fix should flip it WRONG → MATCH (**+1**;
re-measure for others). Snapshot it after the fix.

⚠ The OTHER dedicated fixture `test_if_expressions` will become correct for cases 1-19 but
its **case 20 is a CLOSURE body** (`auto pick = (int n): if ...`) and the self-host does
NOT lower closure bodies (they emit a `return _0` stub, `lower.gg:8835-8841` — a SEPARATE
deep gap). So do **NOT** snapshot `test_if_expressions` (case 20 keeps it WRONG;
snapshotting the accidental output would wire a bug as canonical — forbidden). Use it only
as a manual cases-1-19 emit-C check.

**Secondary validation = the self-host driver itself:** it USES if-as-value internally and
currently MISCOMPILES it (`format_gir.gg:177,180,249`, `format_lir.gg` ×9, all String).
The fix corrects the driver's own code → `self_host_bootstrap_fixed_point` is a PRIMARY
validation + risk surface (see Gate).

**The round's REAL value + validation is the self-host driver itself:** the driver USES
if-as-value internally and currently MISCOMPILES it — e.g. `format_gir.gg:177` /
`format_lir.gg:255-289` `String prefix = if dst >= 0: f"..." else: "..."` currently lower
to empty/0. Fixing EIf-as-value CORRECTS the driver's own code. **`self_host_bootstrap_
fixed_point` is the PRIMARY validation** (the driver self-compiles its if-exprs through
the new path) — see the Gate.

⚠ **No-op safety:** the EIf rewrite must not regress `if`-as-STATEMENT (that path is `SIf`
in `lower_stmt` → `lower_if`, a DIFFERENT site — do NOT touch it). Only the
EIf-in-`lower_expr` case changes. ⚠ **Log to TODO** (out of scope, each its own round):
(a) the closure-body `return _0` stub gap (`lower.gg:8835`) — blocks `test_if_expressions`
case 20 + `auto_types` closure-capture; (b) `EMatch`-as-value (no `lower_expr` case); (c)
`EDo` block-expr (`block_expr.gg`) AND `EBlock` (`lower.gg:5546-5549` — the identical
value-dropping stub: `lower_stmts` then returns a fresh UNIT local). (b)+(c) reuse this
round's `lower_block_expr` / `lower_stmt_as_tail_value` helpers.

## Gate (self-host-dir only — no `src/`)
1. `cargo build` + `cargo test --lib` green.
2. Force-rebuild the driver; emit-C `test_if_expressions` → the EIf merge now ASSIGNS each
   branch's value to `result` (no unassigned I64) → cc → run → cases 1-19 correct (case 20
   = closure stub, expected-wrong, out of scope). Also emit-C a STRING-if branch case
   (cases 15-17: `String s = if c: "a" else: "b"`) — confirm the result local types as
   GorgetString (not I64) so no `Str = int64_t` clash.
3. **`self_host_bootstrap_fixed_point` GREEN — the PRIMARY validation + RISK SURFACE.**
   The driver self-compiles its OWN if-exprs (`format_gir.gg:177` f-string-in-if-branch;
   `format_lir.gg:255-289`), CURRENTLY miscompiled. The fixed point compares stage2/3/4
   (stage1 excluded), so it converges today on the WRONG-but-deterministic output. After
   the fix, stage-1 output CHANGES (prefixes correct) → it must RE-CONVERGE. A subtle bug
   in the merge/type-refinement (esp. the f-string-in-branch shape) could break
   convergence or corrupt the driver's emitted formatting. ⚠ Treat the
   f-string-in-if-branch (`format_gir.gg:177`) as an EXPLICIT test point — emit-C the
   driver and eyeball that prefix. NOT a free pass; a red fixed_point here means the fix is
   wrong — fix it, do not work around.
4. **FULL `cargo test --test integration -- --test-threads=4`** — `lowerer_comparison` /
   `c_emit_comparison` unchanged-or-better (may IMPROVE if the driver's fixed if-exprs
   correct a diagnostic dump), `self_host_runtime` lock-in ≥247/0 (no regression).
5. `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` → re-measure parity; report which (if any)
   moved (expect `if_expr_resource_arms` WRONG→MATCH = +1, plus any others). Re-seed
   snapshots additively (`GG_REGEN_RUNTIME_SNAPSHOT=1`) — `if_expr_resource_arms.out`
   SHOULD appear as a NEW snapshot; confirm zero existing `.out` modified; do NOT snapshot
   `test_if_expressions` (case 20 blocks it).

## Files (stage by name only — never `-a`)
`tests/fixtures/self_host_lowerer/lower.gg`. No `src/`; `lir_codegen.gg` should NOT need
changes (reuses existing GIAssign/GTBranch/merge codegen). Log the EMatch-as-value + EDo
block-expr follow-ups to TODO.
