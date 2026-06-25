# Scout: self-host `: return` / `: throw` expression-body tail

**Status:** ROOT-CAUSED + PROTOTYPED + MEASURED-GREEN in this scout's worktree
(branch `worktree-agent-a99b47933bbb42d49`, base gorget-1 `e02627ae`). C backend.
**Measured parity: MATCH 756 → 757 (+1), zero regressions.** All gates green.

This is the SELF-HOST parallel of the Rust-gg fix `c65df9be`
(`src/ir/lowering/functions.rs`, "guard expr-body tail return — don't clobber
a terminated block's slot"). The self-host gap is **TWO bugs, not one**, and the
brief's "port the lowering guard" premise was **incomplete** — there is a PARSER
bug *upstream* of the lowering clobber.

---

## TL;DR — three findings

1. **Parser bug (PRIMARY, was the `undefined name pick` symptom).** The self-host
   `parse_prefix` (`tests/fixtures/self_host_typechecker/parser.gg`) has **no
   `return`/`throw` arm** — Rust's `parse_prefix` (`src/parser/expr.rs:515-525`)
   wraps a `return`/`throw` statement in `Expr::Block` so it's valid in
   expression position. The self-host inline-body path (`parse_function_def`
   `parser.gg:3636-3643`) calls `parse_expr()` for `: <same-line-content>`, which
   routes to `parse_prefix`; on a `return`/`throw` keyword it falls through to the
   "expected expression" fallback (`parser.gg:2737`), advances past `return`, and
   returns `EIntLiteral(0)`, orphaning the real expression and **derailing the
   parse** (which then mis-consumes the following defs → the `undefined name`
   cascade, e.g. an equip block swallows `void main()`). FIX: add `return`/`throw`
   arms to `parse_prefix` that wrap in `EDo([stmt])` (the self-host's
   `Expr::Block` equivalent). **Mirrors Rust exactly.**

2. **Lowering clobber (SECONDARY, the actual `c65df9be` parallel).** Once the
   parser accepts `: return EXPR`, the body parses as
   `SReturn(Some(EDo([SReturn(Some(EXPR))])))`. The OUTER `SReturn` lowering
   (`lower_stmt.gg:478`) calls `lower_expr` on the `EDo`, which lowers the INNER
   `SReturn` — assigning `_0` AND **terminating the block** — then returns a Unit
   tail local. The outer `SReturn` then `emit(GIAssign(0, unit))` **clobbers `_0`**
   into the already-terminated block (and re-sets the terminator, because the
   self-host `set_terminator`, `lower.gg:951`, is NOT hardened against
   re-termination, unlike Rust's). RESULT: `int withret(int a,int b): return a*b`
   prints **0**, exactly the Rust bug. FIX: a `if block_terminated(&ctx): return`
   guard after `lower_expr` in the outer `SReturn` arm — mirrors Rust's
   `if !builder.is_terminated()`.

3. **`return_expr_body.gg` is ALSO blocked by a THIRD, PRE-EXISTING, DEEP gap —
   NOT in scope.** The committed fixture calls `pick(7, 9)` (a generic free fn
   `T pick[T]`) with **no turbofish**. The self-host only discovers generic
   FREE-fn bodies for TURBOFISH calls (`discover_generic_calls_expr` gates on
   `targs.len() > 0`, `lower_generics.gg:379`); a bare-inferred call is skipped →
   the body is never emitted → **linker error `undefined reference to pick`**.
   This is the documented multi-day, SCOUT-KILLED (`a43f7dcb`/`a16a896f`) gap
   (TODO.md lines 56/186 — bare-targ free-fn body discovery needs the
   typecheck-injects-inferred-`generic_args` re-arch, Rust `generics/mod.rs:1071`).
   **So the return-expr-body fix MOVES `return_expr_body.gg` DRIVER-FAIL →
   CC-FAIL; it does NOT flip it to MATCH.** Verified end-to-end: a turbofish
   variant (`pick[int](7,9)`) MATCHes byte-exact (`12|50|99|7|hi bob|42`).

**The measured +1 MATCH comes from a DIFFERENT fixture: `dop_throw_rhs`**
(`o ?? throw "no value"` — `throw` in expression position, previously
WRONG-OUTPUT). The fix is a genuine, measured win; it just lands on the
`throw`-as-expr fixture, not the `return_expr_body` fixture (which the third gap
holds back).

---

## 1. Measured repro (RUN, self-host driver, C backend)

Driver: `tests/fixtures/self_host_lowerer/driver`; invocation
`driver F lib --emit-c --runtime-dir=<abs>` → cc → run (mirrors
`self_host_emit_cc_run`, `tests/integration.rs:17515`).

| Program | Baseline (no fix) | After fix (parser+lowering) | Oracle |
|---|---|---|---|
| `int withret(int a,int b): return a*b` → `withret(3,4)` | **0** | **12** | 12 |
| `int scaled(self,int x): return self.base*x` (equip) → `c.scaled(5)` | (parse cascade) | **50** | 50 |
| `U echo[U](self,U x): return x` (equip method-generic) → `c.echo(99)` | **undefined name main** (cascade) | **99** | 99 |
| `T pick[T](T a,T b): return a` → `pick[int](7,9)` (turbofish) | **0** | **7** | 7 |
| `String greet(String name): return "hi "+name` → `greet("bob")` | (cascade) | **hi bob** | hi bob |
| `int risky(int x) throws String: return x+1` → `risky(41) catch(e):0` | (cascade) | **42** | 42 |
| `int f(int x) throws String: throw "nope"` → catch 99 | (cascade) | **99** | 99 |
| `String build(String p): return p+"!"` (resource, 2 calls) | — | **hello!/world!** (no leak/UAF) | same |
| `Vector[int] mk(int n): return [n,n+1,n+2]` (collection) | — | **10/12** (clean) | same |
| `T pick[T](T a,T b): return a` → `pick(7,9)` (**bare, gap #3**) | undefined name pick | **link error `undefined reference to pick`** | 7 |

The drop-correctness rows (`String build`, `Vector[int] mk`) confirm the
`block_terminated` early-return does NOT double-drop or leak: the inner `SReturn`
already ran `emit_drops_for_early_exit(DSK_FUNCTION)`, so skipping the outer is
correct.

---

## 2. Root cause — file:line (post-split self-host)

### 2.1 Parser (the `undefined name` cascade)

- `parse_function_def` inline-body path:
  `tests/fixtures/self_host_typechecker/parser.gg:3636-3643` — for
  `: <same-line content>` it does `SpannedExpr body_expr = self.parse_expr()` then
  `body.push(SReturn(Some(body_expr)))`. (Same shape as Rust `mod.rs:1786`.)
- `parse_expr` → `parse_prefix` (`parser.gg:2338`). **No `KW_RETURN`/`KW_THROW`
  arm** (verified: grep of lines 2338-2740 finds neither). The `return` keyword is
  `TkKeyword(KW_RETURN)` → ptag is a keyword tag, none of the prefix cases match →
  the "Fallback" at `parser.gg:2737-2739` pushes `"expected expression"`, advances
  past `return`, returns `EIntLiteral(0)`. The trailing operand (`a`/`x`) is left
  in the stream → the enclosing `equip`/module loop mis-parses, dropping following
  defs (hence `undefined name main`/`pick`/`scaled`).
- `KW_RETURN`/`KW_THROW` ARE handled in `parse_statement` (`parser.gg:2828`,
  `:2861`) — the statement parser — but NOT in expression position. **The Rust
  parser handles them in BOTH** (`parse_statement` AND `parse_prefix`
  `expr.rs:515-525`). That asymmetry IS the bug.

### 2.2 Lowering (the slot clobber, == Rust `c65df9be`)

- Outer `SReturn(Some(EDo[...]))` lowering:
  `tests/fixtures/self_host_lowerer/lower_stmt.gg:478-577` (`case SReturn`).
  `int val = lower_expr(&ctx, ret_expr, &gmod)` (`:500`) lowers the `EDo`.
- `EDo` lowering: `lower_expr.gg:3254` → `lower_block_expr` (`:100`). Its single
  stmt `SReturn(Some(EXPR))` is NOT a tail-value shape
  (`lower_stmt_as_tail_value` `:128`, only `SExpr`/`SIf`/`SMatch`), so the `else`
  arm calls `lower_stmt(SReturn…)` → assigns `_0 = EXPR`, terminates with
  `GTReturn`, returns -1 → `lower_block_expr` materializes a Unit local and
  returns it (`:113`).
- Back in the outer `SReturn` (`lower_stmt.gg:570-572`): `emit(GIAssign(0,
  ret_op))` writes `_0 = unit` into the **already-terminated block** → clobber.
- `set_terminator` (`lower.gg:951`) is `ctx.block_terms.set(...)` UNCONDITIONALLY
  — no terminated-block guard (unlike Rust's, hardened for Snag #33/#39). So the
  trailing `set_terminator(GTReturn(...))` ALSO overwrites the inner terminator.
  The clobber + re-terminate together produce `return 0`.

### 2.3 Why it's a write-site bug (layering §24)

Same as Rust's analysis: the outer `SReturn` *writes* the slot and never checked
the typed invariant that the divergent tail already terminated the block. The
fix is the one-line `block_terminated` guard at that write site (the `EDo`-as-value
path the closure lowerer already respects via `block_terminated` checks at
`lower_expr.gg:187/210`).

---

## 3. The fix (prototyped — exact changes)

Two files. Net +41 lines (mostly the parser arms + comments).

### 3.1 `tests/fixtures/self_host_typechecker/parser.gg` — `parse_prefix`, after the `KW_DO` arm (`:2682`)

```gorget
# Divergent expressions in expression position: `return [expr]` and
# `throw expr`. Mirrors Rust parser (src/parser/expr.rs:515-525): wrap
# the statement in a synthetic EDo block so the inline expr-body form
# `RetType f(...): return EXPR` parses (the body-after-colon path calls
# parse_expr, which routes here). Block-as-expr lowering emits the
# early-exit terminator; the EDo value is irrelevant.
if self.check_kw(KW_RETURN):
    self.advance()
    Vector[Stmt] rbody = Vector[Stmt]()
    if self.check_tok(TOK_NEWLINE) or self.check_tok(TOK_DEDENT) or self.at_end():
        rbody.push(SReturn(None))
    else:
        SpannedExpr rval = self.parse_expr()
        if self.check_tok(TOK_COMMA):
            Vector[SpannedExpr] relems = Vector[SpannedExpr]()
            relems.push(rval)
            while self.match_tok(TOK_COMMA):
                relems.push(self.parse_expr())
            rval = SpannedExpr(ETupleLiteral(relems), Span(0, 0))
        rbody.push(SReturn(Some(rval)))
    return SpannedExpr(EDo(rbody), Span(start, self.peek().lex_start))

if self.check_kw(KW_THROW):
    self.advance()
    SpannedExpr tval = self.parse_expr()
    Vector[Stmt] tbody = Vector[Stmt]()
    tbody.push(SThrow(tval))
    return SpannedExpr(EDo(tbody), Span(start, self.peek().lex_start))
```

(The `return` arm replicates `parse_statement`'s bare-`return` + bare-tuple-return
handling so `: return` and `: return a, b` both work.)

### 3.2 `tests/fixtures/self_host_lowerer/lower_stmt.gg` — `case SReturn`/`Some`, after `lower_expr` (`:500`)

```gorget
int val = lower_expr(&ctx, ret_expr, &gmod)
ctx.expected_type = prev_expected_ret
# Divergent expr-body tail guard (mirrors Rust gg's
# `if !builder.is_terminated()` in `src/ir/lowering/functions.rs`):
# the inline expr-body form `RetType f(...): return EXPR` parses the body
# as `SReturn(Some(EDo([SReturn(Some(EXPR))])))` — the inner `return`/`throw`
# already assigned `_0` AND terminated this block. The outer assign/drops/ret
# below would clobber `_0` with the EDo's unit tail value. Bail out.
if block_terminated(&ctx):
    return
```

`block_terminated` is already imported in `lower_stmt.gg:39`; `lower_stmt` is
`void`, so the bare `return` is valid.

---

## 4. Measured parity + gates (end-to-end, RUN — not source-read)

Baseline = my stash-out (fix reverted, driver rebuilt). Fix = both edits.
Command: `GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration
--release self_host_runtime_diff -- --nocapture --test-threads=4`.

| Category | Baseline | With fix | Δ |
|---|---|---|---|
| **MATCH** | **756** | **757** | **+1** |
| WRONG-OUTPUT | 88 | 87 | −1 (`dop_throw_rhs` → MATCH) |
| CC-FAIL | 194 | 195 | +1 (`return_expr_body` DRIVER-FAIL→CC-FAIL, gap #3) |
| CRASH | 33 | 33 | 0 |
| DRIVER-FAIL | 1 | 0 | −1 (`return_expr_body` left) |

`PARITY = 757/1072 = 70.6%` (was 756/1072 = 70.5%). **Set-diff of every category
confirms exactly ONE improvement (`dop_throw_rhs`) and ZERO regressions**
(CRASH set unchanged; the only new CC-FAIL is `return_expr_body` arriving from
DRIVER-FAIL). `dop_throw_rhs` (`o ?? throw "no value"`) is the `throw`-in-expr-RHS
fixture the parser arm fixes; it MATCHes byte-exact.

Other gates (all RUN, all green):
- `cargo test --lib` → **1084/0** (Rust compiler unaffected, baseline confirm).
- `self_host_bootstrap_fixed_point` → **ok, 318.5s** — the driver self-compiles to
  a byte-identical fixed point WITH the fix. **REQUIRED gate (driver self-emits);
  the parser/lowerer change keeps the bootstrap converging.** (Driver's own source
  does not use `: return`, confirming no breakage from the changed paths.)
- `self_host_runtime` (lock-in net, default-running/build-breaking) → **2/0** — no
  snapshot regressions.

---

## 5. Self-host dir propagation

- `tests/fixtures/self_host_lowerer/parser.gg` and `self_host_check/parser.gg`
  are **SYMLINKS** to `self_host_typechecker/parser.gg` (md5 `5b5def7b`) — the
  parser edit covers the lowerer (runtime_diff) + checker + typechecker dirs in
  one write. `lower_stmt.gg` lives only in `self_host_lowerer/`.
- `self_host_parser/parser.gg` (md5 `a683dc38`) and `self_host_resolver/parser.gg`
  (`5600318`) are **INDEPENDENT copies**. Their `parse_prefix` has the SAME bug
  (their `KW_RETURN`/`KW_THROW` only appear in `parse_statement`, not the prefix).
  All required AST nodes (`EDo`/`SReturn`/`SThrow`/`ETupleLiteral`) exist in both
  their `ast.gg`, so the SAME parser edit applies cleanly.
  - **Required for the parity gate?** No — only the lowerer/typechecker copy
    feeds `self_host_runtime_diff` + `bootstrap_fixed_point`.
  - **Recommended for consistency.** The `parser_comparison` / `resolver_comparison`
    diagnostics (diagnostic-always-pass) would otherwise show a NEW mismatch on
    `return_expr_body.gg` (those copies mis-parse it vs the now-fixed Rust parser).
    Propagating keeps the comparison diagnostics honest. The executor should add
    the same `parse_prefix` arms to both independent copies after the `KW_DO` arm
    (`self_host_parser/parser.gg:~2404`, `self_host_resolver/parser.gg:~2387`).

---

## 6. Gate battery for the executor

1. **`self_host_bootstrap_fixed_point` — REQUIRED** (the driver self-compiles; the
   parser+lowerer change must keep the loop converging). Measured ok @ 318s.
   `GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600 cargo test --test integration
   --release self_host_bootstrap_fixed_point -- --test-threads=1`.
2. **`self_host_runtime`** (lock-in net) — must stay green (snapshot regression
   net). Measured 2/0.
3. **`self_host_runtime_diff`** (diagnostic) — confirm MATCH = 757 (the
   `dop_throw_rhs` win), `return_expr_body` is CC-FAIL (gap #3, expected), zero
   regressions.
4. **`cargo test --lib`** — 1084/0 (unaffected; sanity).
5. If propagating to the independent parser copies: rebuild + re-run
   `parser_comparison` / `resolver_comparison` (diagnostic) to confirm
   `return_expr_body.gg` no longer mismatches.
6. Full `cargo test --test integration` is the orchestrator's integration sweep.

---

## 7. Scope decision (Core invariant #8 / "don't redesign around gaps")

- **Land the return-expr-body fix as-is** (parser + lowering). It is correct,
  reference-grade (the form is documented at `docs/book/04-functions.md:219`
  and `docs/language-design.md:632` — fix-the-lowering, NOT reject), and a
  measured +1 MATCH with zero regressions. It correctly closes the `return`/`throw`
  -in-expression-position class.
- **Do NOT reshape `return_expr_body.gg`** to dodge gap #3 (e.g. don't turbofish
  `pick`). The fixture's bare `pick(7,9)` is idiomatic and Rust passes it; the
  third gap (bare-inferred generic-free-fn body discovery) is a SEPARATE,
  pre-existing, SCOUT-KILLED multi-day track. `return_expr_body.gg` legitimately
  stays a CC-FAIL until that gap is closed — the right outcome per "don't redesign
  around a gap" (the fixture's expected output already reflects intended behavior).
- TODO.md line 93 should be UPDATED: it currently conflates the parser/lowering
  parallel with the `pick` symptom. After this lands, the parser+lowering parallel
  is DONE; what remains for `return_expr_body.gg` to MATCH is the bare-targ
  generic-free-fn gap (TODO 56/186), which it should cross-reference.

---

## Appendix — grounding (docs consulted)

- `docs/plans/return-expr-body-scout.md` — the Rust-gg fix (root cause, the
  `if !builder.is_terminated()` guard, the 4 `FunctionBody::Expression` arms,
  doc-grounded fix-not-reject rationale). DONE.md `c65df9be`.
- `docs/book/04-functions.md:219` — `int add(int a,int b): return a + b` is a
  documented canonical inline-body form (fix-not-reject).
- `docs/language-design.md:632` — `: return GLOBAL_MAX` shown valid.
- `docs/devbook/12-gir-lowering.md` — GIR/lowering pipeline; `lower_function`
  pushes the function drop frame; tail/return semantics.
- TODO.md `lower.gg` MODULE MAP (lines 18/211) — the post-split `lower*.gg`
  layout used to re-locate `lower_function`/`lower_stmt`/`lower_block_expr`.
- TODO.md lines 56/186 — the bare-targ generic-free-fn body-discovery gap
  (gap #3, SCOUT-KILLED).
- Rust references: `src/parser/expr.rs:515-525` (return/throw-as-expr),
  `src/parser/mod.rs:1786` (inline expr-body), `src/ir/lowering/functions.rs`
  (the `is_terminated` guard).
