# Executor Brief — `is`-scrutinee single-eval (kill the GIR double-lowering)

**Track:** Rust gg GIR-lowering correctness (was mis-filed as a C-backend/coalesce bug — CORRECTED by the
scout). **Base:** main (re-check the patch applies). **Zone:** `src/ir/lowering/` (GIR) + `tests/fixtures/` +
`tests/integration.rs` + `tests/fixtures/self_host_typechecker/resolve.gg` (workaround retirement).
**BOOTSTRAP-GATED** (the `resolve.gg` change is self-host SOURCE). Parent gate = full C+LLVM integration sweep
+ `self_host_bootstrap_fixed_point`.

## 0. WORKTREE PREAMBLE (non-negotiable)
`pwd` + `git rev-parse --show-toplevel` FIRST; confirm inside your worktree. NEVER touch `/workspace/gorget`
(main) / `/workspace/gorget-1`. Worktree-RELATIVE paths only. Stage by file name (NEVER `git add -a`/
`commit -a`). NEVER `git stash` — `git diff > /tmp/isscrut_<name>.patch`. Checkpoint to /tmp EARLY. FINAL gates
FOREGROUND. On an Edit desync, re-Read + retry — never a shell heredoc with an absolute path.

## 1. GROUND IN THE SCOUT + THE CORRECTED DIAGNOSIS (read first)
- Scout report: `docs/plans/define-gorget/scouts/scout-codegen-is-scrutinee.md`. **The root is a GIR-lowering
  DOUBLE-EVALUATION, NOT a C-backend/coalesce bug** (the earlier TODO hypothesis was wrong). An `is`-scrutinee
  in an `if`/`elif`/`while`/`and`-chain was lowered TWICE — once as the boolean tag-test (`Expr::Is` value
  lowering, `src/ir/lowering/exprs/mod.rs:791`) and again in `emit_is_bindings` which RE-lowered the scrutinee
  (`src/ir/lowering/stmts/mod.rs`). A side-effecting scrutinee (a mutating `&self` method returning `Option`)
  is thus CALLED TWICE and the payload binds from the SECOND call. Backend-agnostic (GIR→LIR) → one fix
  corrects C + LLVM. `match` was always correct (it lowers its scrutinee once).
- Layering (`docs/devbook/24` + CLAUDE.md "fix at the write site"): the fix mirrors how `match` lowers its
  scrutinee ONCE — evaluate once, memoize, reuse. This is the write-site fix; a C-backend "materialize the temp
  before the read" patch would have been a read-side patch on a write-side bug.

## 2. APPLY THE PROVEN PATCH (verify, don't re-derive)
`git apply docs/plans/define-gorget/scouts/patches/is-scrutinee-single-eval-proto.patch` (6 files, +146/-8;
applies CLEAN). It:
- `src/ir/lowering/context.rs`: adds a per-function `is_scrut_memo: FxHashMap<usize,(LocalId,TypeId)>` on
  `FunctionState` (auto-cleared via `Default` at each function). **⚠ CORRECT THE FIELD DOC COMMENT
  (brief-review-1): the patch's `is_scrut_memo` field doc ends with a FALSE sentence — "each entry is REMOVED
  on consumption so a stale entry can never be reused" — asserting the EXACT OPPOSITE of the fix's load-bearing
  invariant (the impl READS, does NOT remove; read-not-remove is deliberate — an `and`-chain binds its left
  operand in TWO dominated blocks and both must reuse the single eval). After applying, EDIT this comment to
  state READ-not-remove + why + "cleared en masse per-function via `Default`." Leaving it is a
  false-historical-record landmine — a future contributor who "corrects" the code to `.remove()` silently
  re-breaks and-chains and NO gate catches it (CLAUDE.md layering / self-host false-historical-record rules).**
- `src/ir/lowering/exprs/mod.rs:791` (`Expr::Is` value lowering): records `(scrut_local, scrut_type)` under
  `expr.span.start` for NON-negated nodes.
- `src/ir/lowering/stmts/mod.rs` (`emit_is_bindings`): READS (does NOT remove) the memo and reuses the local;
  falls back to re-lower only on a MISS.
- `tests/fixtures/is_scrutinee_single_eval.gg` (new) + wired in `tests/integration.rs`.
- `tests/fixtures/self_host_typechecker/resolve.gg`: retires the `define_pattern_bindings` bind-to-local
  workaround → the idiomatic direct `if scopes.define(...) is Some(def_id):`.

VERIFY (measured, don't assume) — BOTH backends (C default + `GG_BACKEND=llvm`):
- `if m.method() is Some(x)` where `method` is a side-effecting mutating `&self` returning `Option`: the method
  fires ONCE (side-effect count 1, not 2); `x` binds the real payload (not 0/garbage).
- `while … is Some`, `elif … is`, an `and`-chain (`a.step() is Some(p) and b.step() is Some(q)`), and a nested
  3-way and-chain: each scrutinee fires exactly once per evaluation.
- `match` over the same scrutinee: unchanged (still correct).

## 3. THE SUBTLE CORRECTNESS POINTS (verify these hold — they are why read-not-remove + per-function are load-bearing)
- **READ-not-remove is load-bearing:** an `and`-chain binds its LEFT operand in TWO dominated blocks
  (`lower_short_circuit`'s rhs block `exprs/operators.rs:428` + the outer then-block). Removing the memo on the
  first read re-triggers the double-eval on the left operand (scout measured `1001,1002` → `1001` after
  read-not-remove). Confirm the patch READS.
- **Per-function clear** (`FunctionState`/`Default`): the memo must NOT leak a scrutinee local across function
  boundaries. Confirm it's on `FunctionState` and reset per function.
- **Loop re-evaluation (correct by CFG CONSTRUCTION — NOT by per-iteration memo rewriting):** lowering runs
  ONCE per `while` loop, not per iteration. In `lower_while` the `Expr::Is` value-lowering runs in the HEADER
  block (creating ONE `scrut_local` + inserting the memo once); `emit_is_bindings` reads it once in the BODY
  block. Per-iteration correctness is STRUCTURAL: the header block re-executes each iteration via the back-edge
  and re-assigns that same slot, and the header DOMINATES the body which reads it — so the body always observes
  the current iteration's value. Confirm this CFG shape (one header-emitted `scrut_local` dominating the body);
  the empirical gate is the fixture's per-iteration single-eval (`c.calls == 4`, both backends). There is NO
  per-iteration memo rewriting to look for — do not hunt for one.
- **Negated `is` / `is not`:** the patch records only NON-negated nodes — confirm a negated `is` (no binding)
  is unaffected and still correct.

## 4. THE WORKAROUND RETIREMENT (in the patch; bootstrap-gated)
`resolve.gg` `define_pattern_bindings` goes back to the idiomatic direct form, matching its siblings
(resolve.gg:419/569/1032). **Bootstrap-safe** because the self-host LOWERER already single-evals (its `EIs`
value-lowering binds via `lower_pattern_match` at the SINGLE value-lowering site, `lower_expr.gg:5721-5765`;
`lower_if` has no separate re-lowering pass) — so no self-host-lowerer change is needed, and the retired
`resolve.gg` single-evals when compiled by the fixed Rust gg. Do NOT add a self-host-lowerer change; if a
reviewer thinks one is needed, that's a signal to re-check (the self-host already lowers correctly).

## 5. GATES + REPORT
**Executor FOREGROUND gates:** `cargo test --lib` (run + report the count; ~1107 baseline, regenerate — no un-regenerated numbers) · `cargo test --test lints` · a TARGETED
integration subset on BOTH backends covering the `is`/pattern/Option/Dict/match surface (`is_bindings`,
`is_pattern_binding`, `pattern_is`, `match_option_result`, `paren_as_and_if_oneliner`, `option_box_enum`,
`test_match_advanced`, the new `is_scrutinee_single_eval`, plus while/if/result/iter fixtures) — run the
self-host-driver-touching ones `--release` to avoid DEBUG-concurrency timeouts · `resolver_comparison`
(`GG_RUNTIME_DIFF`/comparison harness — confirm it stays IDENTICAL to baseline, the double-eval was benign for
compared output). **Do NOT run the full `cargo test --test integration` sweep or the bootstrap fixed-point —
those are the PARENT's gates** (the resolve.gg retirement is bootstrap-gated; the GIR change needs the full
sweep). **Report:** commit hash; the side-effect-count-2→1 evidence + correct payloads (both backends, paste);
the `while`/`and`-chain single-eval evidence; `cargo test --lib` result; the targeted-subset result;
`resolver_comparison` identical; confirm the 4 subtle-correctness points (§3) hold; `git -C /workspace/gorget
status` CLEAN; NO `LANDED`/`DONE` breadcrumb in `TODO.md`.
**PARENT at integrate (NOT the executor):** the full C+LLVM integration sweep (`--test-threads=4` `--release`,
`tee`) + `self_host_bootstrap_fixed_point` (`GG_BUILD_TIMEOUT_SECS=600`) — because `resolve.gg` (self-host
source) changed. SERIALIZE the bootstrap with the self-host reject-diagnostic track (land one, re-establish the
fixed-point + sweep, rebase the other, land it). Then move this track's TODO entry to `DONE.md`.
