# Executor Brief — R5 FIDELITY: synthesize the test-runner `main` in the self-host

**Status:** DRAFT — under fresh-review discipline before launch. Scope re-verified (ground-truthed) 2026-05-31.
**Risk:** MEDIUM (self-host codegen change, output-affecting → +13 c_emit). **Files (DISJOINT from the other
chains):** `tests/fixtures/self_host_lowerer/lir_codegen.gg` only.

## 0. Worktree discipline
Run `pwd` + `git rev-parse --show-toplevel` FIRST; confirm inside YOUR worktree. NEVER touch
`/workspace/gorget-1`; no `cd` there; no `/workspace/gorget-1/...` paths. `git add <specific files>` only.
Commit in your worktree. FORCE-REBUILD the driver before comparison/bootstrap runs:
`rm -f tests/fixtures/self_host_lowerer/driver tests/fixtures/self_host_lowerer/driver.c`. Run `cargo build` +
`cargo test --lib` + the targeted gates below — NOT the full sweep (parent's job). `GG_BUILD_TIMEOUT_SECS=600
GG_TEST_TIMEOUT_SECS=120` if slow.

## 1. The gap (ground-truthed)
The self-host lowers each test body to a `__test_N(void) {}` function (the `ITest` arm, ~`lower.gg:10003`)
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
   `func.name == "main"` handling near `lir_codegen.gg:4492`), do NOT emit the user-defined `main` — the
   runner becomes the entry point (mirrors Rust's skip). (For the 13 no-`main` fixtures this is a no-op; it
   matters for correctness on fixtures that have both a `main` and tests.)
3. **Synthesize the runner `main`** after the function loop: emit a `int main(...)` (or the project's main
   signature) that calls each `__test_N()` in declaration order and prints a pass/fail tally. PORT the
   STRUCTURE of `helpers.rs:emit_test_runner_main` — but a SIMPLIFIED runner suffices for the gate: the
   self-host doesn't model should_panic/skip/timeout/tags attributes, so a straight "call each `__test_N`,
   count, print summary" runner matches `user_fn_count` (what the gate measures). Use the existing `StrBuf`/
   `sb_push` idiom (the file's string-building convention) — NOT `out = out + X` in a loop.
   ⚠ Collect the `__test_N` names in the SAME order Rust does (declaration/emission order) so the runner's
   body matches; the gate counts FUNCTIONS not bodies, but keep it faithful for the eventual runtime-parity
   harness (Chain 3).

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
