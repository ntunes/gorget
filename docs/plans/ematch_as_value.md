# Brief — self-host `EMatch`-as-value lowering (match expression in value position)

FIDELITY round (1:1:1:1 cadence). Self-host-dir only, **`lower.gg` ONLY** — runs as a
PARALLEL chain file-disjoint from the arena_checkpoint chain (`loader.gg`) and the enum
drop-name cleanup (`lir_lower.gg`). Re-verified by RUNNING the code (scout, 2026-06-02).
⚠ Needs ≥3 fresh sequential reviews before the executor launches.

## Bug (re-verified empirically: emit-C + cc + run)
`lower_expr` (`lower.gg:4254`, returns `int` = a local/slot id) has **no `EMatch` arm**.
`EMatch` appears in `lower.gg` at exactly ONE place — `lower.gg:303-304`, a debug
stringifier (`case EMatch(_, _): return "EMatch"`). So a `match` used in VALUE position
(`X v = match s: case ...`, `return match n: ...`) falls through to the top-level `else` at
`lower.gg:5819-5824`:
```
else:
    # Remaining: EIt, EImplicitClosure, ESetComp, EDictComp, EListComp, EDo, ERange
    int fallback = add_local(&ctx, UNIT_TYPE, NO_NAME)
    emit(&ctx, GIAssign(fallback, OpConstUnit()))
    return fallback
```
(`EMatch` isn't even in that comment's intended-fall-through list — it was overlooked.) The
entire match — every arm, every value — is dropped; the expression yields Unit-as-0.

**Empirical proof (scout RAN it):**
- `match_expr_block_arms.gg` (pure int/struct match-as-value, NO closures): oracle prints
  `got: hello / err: bad input / default: default / 0 / 1 / 2`; self-host prints
  `got: 0 / got: 0 / default: <garbage ptr> / 0 / 0 / 0` — **WRONG**. Smoking-gun C for
  `int classify(int n): return match n: case 0: 0 case 1: 1 else: 2`:
  ```c
  int64_t classify(int64_t __p0) {
  __bb0:
      __v0 = __p0;            // scrutinee lowered, arms DROPPED
      __v1 = (int32_t)0LL;    // OpConstUnit from the else fall-through
      __s2 = __v1; __v2 = __s2;
      return __v2;            // always 0 — never 1 or 2
  }
  ```
- `match_expr_diverging_arm.gg` (struct/String arms + diverging `exit()` arms): self-host
  binary **CRASHES (exit 139 / SIGSEGV)**; oracle prints `a.is_some=true / one / code=1`.
- Neither fixture is in the committed passing set (`tests/fixtures/runtime_snapshots/`).

## Fix — add a value-producing match lowering in `lower.gg` (TWO wire-in sites)
Add a helper that lowers a match into a single **result local**, then call it from both the
expression path and the statement-tail path.

### (1) New helper — mirror `lower_if_chain_expr`'s skeleton, drive branches with the match machinery
`lower_if_chain_expr` (`lower.gg:4167-4240`) is the value-merge TEMPLATE; `lower_match_stmt`
(`lower.gg:7822`) is the SCRUTINEE-STAGING + per-arm-branching structural reference. The new
helper is `lower_match_stmt`'s shape but, per arm, lowers the arm BODY to a value and assigns
it into one result local (gated on `block_terminated`), exactly as `lower_if_chain_expr` does
per branch. Suggested signature (match the surrounding style):
```
int lower_match_expr(LowerCtx &ctx, SpannedExpr scrutinee_expr, Vector[MatchArm] arms, GirModule &gmod)
```
Algorithm (mirror Rust `lower_match_expr`, `src/ir/lowering/exprs/mod.rs:2569`, dispatched
from Rust `lower_expr` at `:391`; identical merge algorithm to `lower_match_stmt_as_expr`
`:3257`):
1. Lower + stage the scrutinee and infer its enum type — copy how `lower_match_stmt`
   (`:7822`) does it (scrutinee local + `match_enum_type(...)`).
2. Allocate ONE `result` local up front (size from `expected_type` if the ctx carries one,
   else `I64_TYPE` — see how `lower_if_chain_expr` seeds it); create a `merge_bb`.
3. **Per arm** (iterate the uniform `Vector[MatchArm]` — see AST note below):
   `lower_pattern_match(&ctx, scrutinee, arm.pattern, enum_name, arm_body_bb, next_test_bb,
   &gmod)` (`lower.gg:7314`) to branch — `lower_pattern_match` ITSELF emits the pattern
   bindings into the matched block (as in `lower_match_stmt:7841-7847`; do NOT emit them
   separately) — then in `arm_body_bb`: `int val = lower_block_expr(&ctx, arm.body, &gmod)`
   (`lower.gg:4108` — `arm.body` is `Vector[Stmt]`, exactly what `lower_block_expr` consumes).
4. **Merge — ONLY `if not block_terminated(&ctx)`** (this guard is LOAD-BEARING for
   diverging arms; copy `lower_if_chain_expr`'s gating EXACTLY): `refine_local_type(&ctx,
   result, <val's type>)` from the first non-Unit arm (`lower.gg:4248`), assign via
   `op_consume(&ctx, &gmod, val, CkAssign())` into `result` (`lower.gg:1413`), then jump to
   `merge_bb`. A terminated arm (one whose body `return`s / calls `exit()`/noreturn) gets NO
   assign-and-jump — its terminator stands.
5. **No-else / fall-through after the last concrete arm: emit ONLY `set_terminator(
   GTJump(merge_bb))` — NO result-default assignment.** ⚠ (pass-1 [BLOCKING] correction — do
   NOT copy `lower_if_chain_expr`'s `OpConstI64(0)` no-else default here.) In the self-host
   that default is INLINED inside `lower_if_chain_expr` itself (`lower.gg:4234-4238`, named
   after Rust's separate `build_if_chain_expr`) — i.e. it's in the very function step 1 points
   you at as the template, so do NOT carry that one tail-assignment over. Rust's MATCH lowering
   `lower_match_expr` (`exprs/mod.rs:2705-2723`) emits NO default, and the self-host's own
   `lower_match_stmt` (`lower.gg:7856-7859`) emits only `GTJump(merge_bb)`. Reason: both target fixtures' first
   match returns a STRUCT (`match_expr_block_arms.parse_or_default` → `Frontmatter`;
   `match_expr_diverging_arm` → `Big`) via an exhaustive `Ok/Error` match with NO `else:` (so
   no `PWildcard` arm). An `OpConstI64(0)` default would emit `GIAssign(result, OpConstI64(0))`
   into a struct/String `result` slot → C error `incompatible types … from 'int32_t'` (dead
   code is still C-typechecked) → the fix would FAIL to compile the very fixtures it targets.
   For an exhaustive match the unmatched fall-through is dynamically unreachable, so `result`
   legitimately stays unassigned there — that is correct (C has no definite-assignment
   requirement; Rust relies on exactly this). Do NOT "helpfully" re-add a default. (If a
   genuinely non-exhaustive INT match ever needs one, it must be gated on the result type
   being integral — not needed here, and not what Rust does. The `classify`/`code`/`tag`
   cases carry an explicit `else:` → a `PWildcard` arm that always matches + assigns, so they
   need no default either way.)
6. `switch_to(merge_bb)`; return bare `result` (as `lower_if_chain_expr:4240` does — no
   `copy()` wrapper at this self-host boundary).

### (2) Wire-in site A — `lower_expr` EMatch arm
Add `case EMatch(scrut_box, arms): return lower_match_expr(&ctx, *scrut_box, arms, &gmod)`
to `lower_expr` (before the `else` at `:5819`). AST: `EMatch(Box[SpannedExpr],
Vector[MatchArm])` (`ast.gg:68`) — deref the `Box`.

### (3) Wire-in site B — `lower_stmt_as_tail_value` SMatch arm
`lower_stmt_as_tail_value` (`lower.gg:4135`) currently has a documented deferral
(`:4132-4134`: "`SMatch`-as-value … lower as a plain statement + return -1 for now") and its
`else` (`:4141-4143`) lowers SMatch as a statement, returning -1. Add an `SMatch` arm that
routes through `lower_match_expr` (so a `match` ending a block/arm body becomes the block's
value), mirroring Rust `lower_stmt_as_tail_value` (`exprs/mod.rs:3246-3248`). This makes the
new helper reachable from block-tail position too (and `lower_block_expr` already calls
`lower_stmt_as_tail_value` for the tail stmt, so EMatch-tail blocks compose for free).

## ⚠ AST / behavioral constraints (do NOT regress)
- **No separate else_arm.** Self-host `EMatch` has NO else field — the `else:` is folded into
  `Vector[MatchArm]` as a `PWildcard` arm (`Pattern::PWildcard`, `ast.gg:39`). So iterate the
  uniform arm vector (closer to `lower_match_stmt`'s loop than to Rust's split arms/else_arm);
  the wildcard arm is just the last arm and always matches.
- **Guards.** `MatchArm` has `guard: Option[SpannedExpr]` (`ast.gg:216-219`). The existing
  `lower_match_stmt` (`:7822`) does NOT handle guards. The new helper must MATCH that existing
  (no-guard) behavior — do NOT silently mis-lower a guarded arm, and do NOT invent guard
  handling. If a fixture needs guard-in-value-match, log it as a follow-up (see below).
- **Diverging arms.** `match_expr_diverging_arm` uses `exit()` / bare `return` arms — the
  `block_terminated`-gated assign+jump is what prevents double-termination. Copy
  `lower_if_chain_expr`'s gate verbatim (Rust documents the same gated-jump as its Snag #33,
  `mod.rs:3384-3395`).

## Scope / expected outcome
**+2 runtime-parity** (clean wins, both closure-free, both empirically confirmed):
`match_expr_block_arms` (WRONG→MATCH) and `match_expr_diverging_arm` (CRASH→MATCH).
⚠ Do **NOT** over-promise: ~22 fixtures use value-position match, but most have ADDITIONAL
blockers and will NOT reach MATCH from this fix alone — `const_match_pattern` (needs
const-pattern resolution), `nested_match_expr_enum_result` (CC-FAIL `undefined E__B` =
separate generic-type-param-leak gap), `nested_match_return_from_inner_arm` (CC-FAIL "void
value not ignored"), `match_arm_borrow_clone` (CC-FAIL `__gg_new`), `variant_user_enum_call_type`,
`cow_enum_bare_assign`, `snag50_match_as_expr_arm_locals_leak`. Snapshot ONLY the fixtures
that actually reach MATCH after the fix (re-measure). `return_in_if_in_match` is already
passing (statement-form match via `lower_match_stmt`, NOT value position) — leave it.

## Validation gate (self-host-dir only — no `src/`)
1. `cargo build` + `cargo test --lib` green.
2. Force-rebuild the driver; emit-C for `match_expr_block_arms` and `match_expr_diverging_arm`
   — confirm the arms are now lowered (result local assigned per arm), cc → run → MATCH the
   Rust oracle (`cargo run --release -- run tests/fixtures/<name>.gg`).
3. **`self_host_bootstrap_fixed_point`** GREEN — LOAD-BEARING: the driver self-compiles its
   OWN value-position matches (e.g. `format_gir.gg`'s tag-dispatch), so re-convergence is the
   real validation (same role the EIf round leaned on). Run `--test-threads=1`.
4. `self_host_runtime` lock-in net GREEN and ≥248/0 (becomes 250/0 after adding the two new
   snapshots in step 6).
5. `c_emit_comparison` / `lowerer_comparison` (`--nocapture`) unchanged-or-better; report counts.
6. Additive snapshots — create `tests/fixtures/runtime_snapshots/match_expr_block_arms.out`
   and `match_expr_diverging_arm.out` (Rust-oracle output; follow the regen mechanism in the
   `self_host_runtime` test body). Add ONLY fixtures that reach MATCH; modify ZERO existing.
7. The PARENT runs the full `cargo test --test integration` sweep + the `GG_RUNTIME_DIFF=1`
   parity re-measure at integration — the executor runs the targeted gates above only.

## Follow-ups to LOG in TODO.md (out of scope — do NOT bundle)
- Guard-in-value-match (`MatchArm.guard`) — neither this helper nor `lower_match_stmt`
  handles it; wire when a fixture needs it.
- **Per-arm name-map snapshot/restore (Rust Snag #50).** Rust `lower_match_expr`
  snapshots/restores the arm-local name map (`saved_arm_locals`, `exprs/mod.rs:2680/2700`) so
  a CoW `&v`-materialization rebind in one arm doesn't leak into a sibling arm. The new helper
  omits this — but so does the existing self-host `lower_match_stmt` (it relies on per-arm
  drop-scope push/pop instead), which is this brief's structural reference, and NEITHER target
  fixture triggers it. Match `lower_match_stmt`'s existing behavior (no regression); log the
  snapshot/restore as a separate fidelity follow-up.
- `EDo`/`EBlock` block-expr value-dropping stub (the next lower.gg cluster item — reuses the
  same `lower_block_expr`/`lower_stmt_as_tail_value` helpers).
- The multi-blocker value-position-match fixtures listed under Scope (each its own root).

## Files (stage by name only — never `-a`)
`tests/fixtures/self_host_lowerer/lower.gg` + the 1-2 new `tests/fixtures/runtime_snapshots/
*.out`. Do NOT touch `loader.gg`, `lir_*.gg`, `src/`, or `TODO.md`/`DONE.md` (the parent owns
TODO/DONE across the parallel chains).
