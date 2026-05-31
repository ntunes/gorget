# Executor Brief — R5 FIDELITY: synthesize the test-runner `main` in the self-host

**Status:** DRAFT — under fresh-review discipline before launch. Scope re-verified (ground-truthed) 2026-05-31.
**Risk:** MEDIUM (self-host codegen change, output-affecting → +13 c_emit). **Files (DISJOINT from the other
chains):** `tests/fixtures/self_host_lowerer/lir_codegen.gg` only.

## 0. Worktree discipline
Run `pwd` + `git rev-parse --show-toplevel` FIRST; confirm inside YOUR worktree. NEVER touch
`/workspace/gorget-1`; no `cd` there; no `/workspace/gorget-1/...` paths. `git add <specific files>` only.
⚠ **(brief-review pass-1 — coordination) BRANCH FROM THE POST-IMPORTED-CHECK BASE** (an imported-check fix
chain is landing that makes Rust gg STRICTLY type+exhaustiveness-check imported modules, incl. this
`lir_codegen.gg`). Your new runner code must be **type-clean and any `match` you add exhaustive** — once the
truncate-deletion lands, `gg build` checks this file strictly and a latent defect breaks every self-host
comparison/bootstrap test. (Low risk — the runner is pure `sb_push` string-building, no new `match` needed —
but don't introduce one carelessly.) If the imported-check chain isn't integrated yet when you start, rebase
onto it before your final gate run.
Commit in your worktree. FORCE-REBUILD the driver before comparison/bootstrap runs:
`rm -f tests/fixtures/self_host_lowerer/driver tests/fixtures/self_host_lowerer/driver.c`. Run `cargo build` +
`cargo test --lib` + the targeted gates below — NOT the full sweep (parent's job). `GG_BUILD_TIMEOUT_SECS=600
GG_TEST_TIMEOUT_SECS=120` if slow.

## 1. The gap (ground-truthed)
The self-host lowers each test body to a `__test_N(void) {}` function (the `ITest` arm, `lower.gg:10135`)
but NEVER synthesizes the **test-runner `main`** that Rust generates to call them. So for test fixtures with
NO user `main`, the self-host emits one FEWER user function than Rust (the missing runner main) → 13 fixtures
are all off-by-1 on `c_emit_comparison` (`test_basic`, `test_cleanup`, `test_skip`, `test_suite`,
`test_should_panic`, `test_tags`, `test_with_clause`, `test_timeout`, `test_process_timeout`, `test_process`,
`snapshot_basic`, `test_failure`, `test_option_resource_field`). Adding the runner flips all 13 to MATCH
(**+13: 849 → ~862**). NOTE: `test_traced` does NOT flip (it has an independent off-by-5 trace-fn gap —
leave it; it's out of scope). `test_coexist` has a user `main` and is already handled.

⚠ **Metadata trap:** the self-host's `LirModule.test_fns`/`is_test_module` are NEVER populated (`lir_lower.gg`
never writes them; constructed empty), so `is_test_or_bench` (`lir_codegen.gg:5638`) is ALWAYS false. You
CANNOT gate on that metadata. **Detect test mode by scanning `m.functions` for any name starting with
`__test_`** (the metadata-free route).

## 2. The fix (in `generate_c`, `lir_codegen.gg`)
Mirror Rust: `src/backend/c_lir/helpers.rs:1145` `emit_test_runner_main(out, module)` (called from
`src/backend/c_lir/mod.rs:1038-1039`), and the user-`main` skip when test-mode (`mod.rs:1010` `has_test_runner`
flag, skip at the function loop ~`:1027`). READ those Rust sites for the exact runner shape.
1. **Detect test mode:** `bool has_test_runner = <any m.functions[i].name starts_with "__test_">`. Compute
   once before/around the function-emission loop (`lir_codegen.gg:~5392-5404`).
2. **Skip the user `main`** when `has_test_runner`: in the function-emission loop (and/or the
   `func.name == "main"` handling near `lir_codegen.gg:4492`), do NOT emit the user-defined `main`. ⚠
   **(brief-review pass-1 — corrected rationale) This guard can NEVER actually fire in the self-host** —
   `lower.gg` gates ALL test/suite lowering on `if not has_main` (`:10138/:10179/:10186`), so a module emits
   `__test_N` ⟺ it has NO user `main` (verified: `test_coexist`, which has a user main, emits ZERO `__test_N`).
   So `has_test_runner ⟺ no user main`, and `has_test_runner && name=="main"` is unreachable. KEEP the guard
   anyway (harmless, mirrors Rust, future-proof) — but do NOT add complexity chasing a both-main-and-tests
   case that the self-host can't produce.
3. **Synthesize the runner `main`** after the function loop: emit `int main(int argc, char** argv) {` (reuse
   the EXACT existing main-signature line at `lir_codegen.gg:4492` — it already ends `) {`, which the gate's
   `user_fn_count` requires; do NOT split the signature across `sb_push` calls in a way that breaks the
   `) {` line-ending) ... body ... `}`. The body calls each emitted `__test_*` function and prints a
   pass/fail tally. ⚠ **(brief-review pass-1) `__test_N` indices are NON-CONTIGUOUS** — `@skip` tests get no
   body in either backend (`test_skip` emits `__test_0` and `__test_3`, not 0..3). So **collect the ACTUAL
   emitted `__test_*` names** (the same scan you used for `has_test_runner`) and call exactly those, in
   emission order — do NOT loop a `0..count` counter. PORT the STRUCTURE of `helpers.rs:emit_test_runner_main`,
   but a SIMPLIFIED runner suffices for the gate (the self-host doesn't model should_panic/skip/timeout/tags;
   `user_fn_count` counts FUNCTIONS not bodies). ⚠ **Emit the runner into `body_buf` AFTER the
   `// ── Function Definitions ──` marker** (pushed at `lir_codegen.gg:5392`, flushed to `out` at `:5417`) so
   `user_fn_count` (`integration.rs:13611`, counts post-marker lines starting `[A-Za-z_]` ending `) {`)
   actually counts it. Use the existing `StrBuf`/`sb_push` idiom — NOT `out = out + X` in a loop. Keep the
   call order faithful for the eventual runtime-parity harness (Chain 3).

## 3. Gates
- `cargo build` clean; `cargo test --lib` green.
- Force-rebuild the driver, then `c_emit_comparison --nocapture`: matched **849 → ~862** (+13; the 13 listed
  fixtures now MATCH). Confirm NO regression on the other 849 (the runner is emitted ONLY in test-mode, so
  non-test fixtures are byte-unchanged — verify a non-test fixture's `--lir-c`/`--emit-c` is unchanged).
- `lowerer_comparison` UNCHANGED (it compares GIR, which is unaffected — the runner is a C-emit-stage synth).
- `self_host_bootstrap_fixed_point` GREEN (the self-host's OWN sources are not test modules → has_test_runner
  false → zero change to its self-compile; confirm byte-reconvergence).

## 4. Report back
The diff + commit; the before/after `c_emit` count + which of the 13 fixtures flipped to MATCH (and confirm
`test_traced` stayed mismatched for the documented trace reason); confirmation non-test fixtures are
byte-unchanged + `fixed_point` GREEN.

## 5. Don't-dodge rule
If a fixture in the 13 does NOT flip to MATCH, INVESTIGATE why (a real second gap) and report it — don't
reshape the runner to force the count. The runner must be a faithful test-main, not a count-padding stub.
