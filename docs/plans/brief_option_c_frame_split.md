# BRIEF — Option C: eliminate the >8MB self-host stack need (frame-split `lower_expr_inner` + `infer_expr_type`)

Status: v1 (orchestrator draft from scout `agent-aaf1b645c2400fb52`, GO,
2026-06-11, gorget-1 tip ~`5604a62f`+). Owner chose C-prioritized over the
lean-runtime `run_with_stack` opt-in (the opt-in is DROPPED; only the
plain-main revert survives, as C's final step). NEEDS ≥3 fresh brief-reviews
before the executor launches, AND must sequence AFTER Chain F integrates.

## Mission
Shrink the dominant GIR-lowering stack frame so the self-host compiler
self-compiles on a plain ~8MB stack — then revert the forced 64MB-pthread main
to a plain thread-0 main (the macOS-GUI fix; gorget-arena) and drop the
`run_with_stack` opt-in entirely. The frame shrink is a PROVABLY-PURE refactor:
extract each per-expression-kind match arm of `lower_expr_inner` (and
`infer_expr_type`) into its own helper function so each arm's locals live in
its own small frame instead of summing into the recursive frame.

## Why (measured root cause — do not re-derive, but RE-CONFIRM the frame number)
`lower_expr_inner` is a giant match-on-expr-kind; at -O0 the C compiler gives
every arm's locals a DISTINCT stack slot (no coalescing across mutually
exclusive arms) → a **182,640 B (~178KB)** frame paid PER AST LEVEL. The deep
path is a 51-term `+` concat chain (`derive.gg:172`) → ~51-deep binary
recursion → ~9.3MB today (~11.8MB flipped-CoW), overflowing 8MB. ⚠ **Only the
SELF-HOST overflows** — Rust `gg` is rustc-compiled and rustc coalesces stack
slots, so the Rust binary never overflows; the Rust-side extraction is for
STRUCTURAL PARITY only, not its own stack (do it LAST).

## Measured yield (scout, -fstack-usage on emitted C; the GO evidence)
| extraction | `lower_expr_inner` frame | per-binary-level | ×51 |
|---|---|---|---|
| baseline (no extraction) | 182,640 B | 182,928 B | 9.33 MB |
| 6 arms (EMethodCall+EBinaryOp+4 literals) | 94,176 B (−48%) | 116,400 B | 5.94 MB |
| **full extraction (est.)** | **~68,000 B** | 90,224 B | **4.60 MB** (non-flipped) / **5.82 MB** (flipped) |
Linear ~55–64 B/source-line across 3 data points. **Full extraction → ~6.3MB
incl. the ~0.5MB one-shot prefix (lower_module 317KB/lower_stmt 49KB) → +1.7MB
headroom on 8MB, on BOTH the current and flipped-CoW variant.** A PARTIAL
(6-arm) extraction clears 8MB today but is too tight under the flip → **scope
FULL, not partial.** Behavior-preservation MEASURED: `--emit-gir` byte-diff
over the FULL corpus **1,215/1,215 identical**, `--emit-c` 200/200 identical.

## The functions (re-grep — line numbers drift; the self-host SPLIT into modules)
- **Self-host `tests/fixtures/self_host_lowerer/lower_expr.gg:~266`** —
  `int lower_expr_inner(...)`, 39 arms, recursive via `lower_expr` wrapper
  (`:~248`). Heaviest arms (body lines): EMethodCall 832, EClosure 194,
  EBinaryOp 180, EFieldAccess 150, ECall 116, EIdentifier 110, EFString 109,
  EIndex 99, EDictLiteral 98, EArrayLiteral 88, ETupleLiteral 79 (top 11 ≈ 78%
  of the body). ~14 arms ALREADY delegate (EMatch/ECatch/ERethrow/EBlock/EIf →
  `lower_match_expr`/`lower_catch_expr`/…) — **the extraction pattern already
  ships; copy it.**
- **Self-host `tests/fixtures/self_host_typechecker/infer.gg:~105`** —
  `int infer_expr_type(...)`, 32 arms, ~131KB frame (per-arm `__gg_Expr`
  by-value copies). ⚠ SYMLINKED into `self_host_lowerer/infer.gg` — edit the
  real `self_host_typechecker` file; it benefits both drivers. Heaviest:
  EMethodCall, ECall, EIdentifier.
- **Rust `src/ir/lowering/exprs/mod.rs:~89`** — `lower_expr_inner`, 45 arms,
  already heavily extracted. Remaining fat inline arms: Spawn 305, DefaultOp
  184, Deref 122, TupleLiteral 97, Move 93, Identifier 75, MutableBorrow 64,
  SpawnBlocking 57, Await 54. PARITY ONLY (Rust doesn't overflow) → Phase 3.

## Extraction mechanic (per arm)
`int lower_<kind>(LowerCtx &ctx, <pattern-bound vars>, GirModule &gmod)` (the
existing `lower_match_expr` shape) — move the arm body VERBATIM, replace the
arm with a call. `&ctx`/`&gmod` are already mutable-borrow params (no CoW
issue). **Obstacles (mechanical, caught by the compiler):**
- Heavy arms capture a few ENCLOSING locals beyond their pattern bind
  (EMethodCall references `sexpr` for span/error context + `recv_box`) → pass
  them as extra params.
- Inner-local name collisions with new params → rename.
- After each extraction, `gg build` ONCE and let the resolver enumerate the
  undefined (captured) names — that's the checklist of extra params.
- Inner `break`/`continue` are loop-LOCAL (no labeled break in Gorget) → safe.

## Behavior-preservation gate (per extraction — the load-bearing check)
The refactor MUST be byte-pure. After EACH arm (or small batch):
1. `--emit-gir` (and `--emit-c`) byte-diff the extracted driver vs the
   pre-extraction baseline over the FULL fixture corpus → must be IDENTICAL
   (the scout's script: run both drivers, compare). A non-identical diff = a
   missed enclosing-local capture or a mis-aliased mutable local → fix.
2. `c_emit_comparison` + `lowerer_comparison` counts unchanged.
3. After a phase: `self_host_bootstrap_fixed_point` re-converges (the driver
   self-emits the extracted helpers — the end-to-end self-host validation).

## Phasing (each phase independently shippable + gated)
1. **Self-host `lower_expr.gg`, heaviest-arm-first:** EMethodCall → EBinaryOp
   (these two alone = −48%) → EClosure → EFieldAccess → ECall → EFString →
   EIdentifier → EIndex → EDictLiteral/EArrayLiteral/ETupleLiteral. Byte-diff
   gate after each. THIS is the critical path for the macOS fix.
2. **Self-host `infer.gg`** (EMethodCall + ECall + EIdentifier) — closes the
   131KB runner-up.
3. **Rust `exprs/mod.rs` symmetric extraction** (Spawn/DefaultOp/Deref/…) for
   structural parity — LAST, and AFTER Chain F lands (shared file).
4. **Plain-main revert + drop the opt-in + validate:** revert the forced
   pthread-main → plain thread-0 main in BOTH emitters
   (`src/backend/c_lir/mod.rs` + `lir_codegen.gg`; the reusable hunk is in
   `docs/plans/lean_runtime_prototype.diff` — take ONLY the plain-main part,
   NOT the `run_with_stack`/`thread.gg`/`driver.gg` opt-in). Then
   `self_host_bootstrap_fixed_point` GREEN on the PLAIN main (the self-host now
   fits in ~8MB) is the load-bearing proof; confirm a trivial program emits a
   plain `int main` (0 trampoline/pthread refs) = the macOS fix. ⚠ Update
   `stack_guard_deep_recursion.gg` + the `stack_guard_*` tests (a plain main
   removes the 64MB runtime-recursion budget; that guard fixture must either be
   re-scoped or the test bounds adjusted — decide per the test's intent).

## Sequencing vs Chain F (snag #11) — SEQUENCE, do not blind-merge
Chain F edits the `lower_expr` WRAPPER (span-param threading + the
`maybe_auto_propagate` call-sites, which live in the small/delegated
comprehension arms) + `emit_result_auto_propagate` + self-host `lower_match.gg`
— LARGELY DISJOINT from the heavy extractable arms. **Land Chain F FIRST, then
rebase the C extraction OVER it.** Phase 1 (self-host `lower_expr.gg`) touches
the same FILE Chain F threads spans through, but different arms; only an
extracted arm that itself contains a `maybe_auto_propagate` call must carry
Chain F's threaded span into its relocated copy — and the whole-corpus
emit-diff gate catches any drop. Phase 3 (Rust `exprs/mod.rs`) is the same file
Chain F edits → strictly after Chain F integrates.

## Gates (executor; parent re-runs the full battery)
- Per-extraction: full-corpus `--emit-gir`/`--emit-c` byte-diff IDENTICAL.
- `cargo build`; `cargo test --lib`; `lowerer_comparison` + `c_emit_comparison`
  (read counts).
- Per phase: `self_host_bootstrap_fixed_point` GREEN (`GG_BUILD_TIMEOUT_SECS=600`).
- Final (after the plain-main revert): `fixed_point` GREEN on the PLAIN main +
  trivial-program-emits-plain-main + `stack_guard_*` adjusted + macOS verified
  by the owner.

## Constraints
- Worktree preamble (`pwd`/toplevel check; `git merge --ff-only gorget-1`;
  never touch `/workspace/gorget-1`); explicit-file `git add`; no pushes; STOP
  on a contradicted premise (esp. if the emit-diff is NOT byte-identical, or a
  phase's `fixed_point` regresses).
- Zone (Phases 1-2): `tests/fixtures/self_host_lowerer/lower_expr.gg`,
  `tests/fixtures/self_host_typechecker/infer.gg`. Phase 3:
  `src/ir/lowering/exprs/mod.rs`. Phase 4: `src/backend/c_lir/mod.rs`,
  `tests/fixtures/self_host_lowerer/lir_codegen.gg`,
  `tests/fixtures/*stack_guard*`, `tests/integration.rs`. Plus TODO/DONE.
- Commit per phase; messages cite this brief + the scout; Co-Authored-By trailer.
