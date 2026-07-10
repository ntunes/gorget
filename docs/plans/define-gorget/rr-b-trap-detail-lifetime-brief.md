# R-B brief — test-mode trap/panic detail lifetime fix + `gorget_trap_fmt` dedup

> **Round:** review-residuals (xhigh review of `f42eea96..7aad1844`, TODO High entry
> "D11/D23-wave RESIDUALS" items (b) + the `gorget_trap_fmt` part of (f)).
> **Zone:** `src/backend/c/runtime/` + one new test fixture + its `run_gg_test`
> harness entry in `tests/integration.rs` (pass-1 D3 — the entry is required; note
> `gg_command("test")` auto-applies `--backend=llvm` under `GG_BACKEND=llvm`,
> `integration.rs:144-153`, so both-backend coverage needs no extra wiring). No
> lint edits (verified: the trap ratchets scan c_lir/llvm/self-host, not runtime
> .c shape; lints at HEAD = 52/0 — the scout's 45 was stale).
> **Scout:** report `/tmp/scout_rr_b_report.md`, prototype `/tmp/scout_rr_b_prototype.patch`
> (7 files, +106/−83), measured end-to-end at `cab529cd`.
> **Status:** v2 — pass-1 reviewed (3 folds: D1 gate-4 count corrected to 10/0 —
> the "30/0" was the scout's OR-filter figure transplanted onto the `trap` filter;
> D2 the regression fixture now pins BOTH consumers — pass-1 proved the P3
> heap-UAF is DETERMINISTIC with a named String local, ASan-verified, not "luck";
> D3 zone wording includes the harness entry). Awaiting pass 2.

## Verified premises (scout, empirical)

**P1 — CONFIRMED: stack-use-after-scope across `longjmp`, and it is roulette in
BOTH directions.** `panic_test.c:52` (pre-fix) stores the caller's `detail`
pointer raw, then runs `__gorget_cleanup_run` + `longjmp` (:53-54). 27 producer
sites pass a stack-local `char __gg_detail[96]` (runtime_array.c 7,
runtime_string.c 2, shared_runtime.c 2, bytes_runtime.c 8, bytes_f32_runtime.c 8);
26 span-less `gorget_trap`, 1 (`gorget_array_get_at`, runtime_array.c:49-53)
`gorget_trap_at`. Reproduced under `gg test`:
- False FAIL: `FAIL: expected panic containing "index out of bounds", got: H����`.
- **False PASS (worse, the review missed it):** a
  `@should_panic("index out of bounds: index 10, length 3")` test PASSED by
  reading stale-but-coincidentally-intact dead-stack bytes. `@should_panic`
  detail matching is currently UB roulette both ways.

`panic_normal.c` is clean by construction (both paths fprintf in-frame then exit,
:3-6, :17-20) — normal mode needs no change.

**P2 — CONFIRMED: the 3-line `__gg_detail` pattern is copy-pasted exactly 27×**
(one strict regex matched all 27; zero manual edits needed for the migration).

**P3 — NEW (scout finding): `gorget_panic_at` has the SAME class, heap-UAF
flavor.** `panic_test.c:31` stores `msg` raw; a user `panic(str)` marshals
`(const char*)str.data` (`emit_call_extern.rs:55-64`) — a heap pointer that
`__gorget_cleanup_run` may free BEFORE the longjmp. It currently "works" by
freed-heap luck. The same one-site consumer fix covers it.

## Design (prototyped; land correctness + dedup as ONE track)

**Correctness — fix at the ONE consumer (Core #4: the class, not the 27
instances).** In `panic_test.c`: `static _Thread_local char
__gorget_test_fail_buf[256]` + a `__gorget_test_msg_copy()` helper (snprintf
`%s` → safe truncation; NEVER returns NULL — NULL means "test passed" to the
generated runner, `helpers.rs:1281`). BOTH `gorget_panic_at` (P3) and
`gorget_trap_at` copy into the buffer BEFORE `__gorget_cleanup_run` and longjmp.

*Thread-safety (verified):* the generated test runner (`helpers.rs:1185-1306`) is
straight-line C in `main()` — sequential; `gg test` parallelism is
process-sharded via `GORGET_PARALLEL_ID/TOTAL`. `_Thread_local` matches the
adjacent cleanup-stack convention and the LLVM strip-static transform explicitly
preserves it (`main.rs:1389-1395`). Trap-on-spawned-thread longjmp is
pre-existing UB, unchanged, out of scope.

**Dedup — `gorget_trap_fmt(code, fmt, ...)` + `gorget_trap_at_fmt(code, file,
line, col, fmt, ...)`** (span params BEFORE fmt because varargs must trail),
defined in BOTH `panic_normal.c` and `panic_test.c` (exactly one is emitted per
binary — mirrors how `gorget_trap` itself is duplicated; a shared third .c would
need wiring at all 4 assembly points + the embed-table lint, not worth it).
Migrate all 27 producer sites (mechanical; format strings byte-identical).
Build-order verified at ALL FOUR assembly points (emit_types.rs:2035→2051+;
main.rs:1262-1270→1273+; self-host lir_codegen.gg:8445-8447→8463+; hot-reload
mod.rs:482→485); `runtime_string.c` is emitted before panic_*.c in the C backend
and already forward-declares `gorget_trap` for exactly that reason (:457-465) —
add the 2 `_fmt` forward decls there. The self-host lane inherits automatically
(it embeds the same .c text, `driver.gg:72-73`).

**Why one track:** the dedup is what makes the fix UN-REGRESSABLE — after it, no
per-site stack-buffer pattern remains to copy-paste back into existence.

## New regression fixture (REQUIRED — today nothing pins trap-detail matching)

ONE fixture, TWO tests, run under BOTH backends (only assert-message matching is
pinned today, `test_should_panic.gg`):
1. `@should_panic("index out of bounds: index 10, length 3")` on an OOB `v[i]` —
   pins the `gorget_trap_at` consumer (P1).
2. (pass-1 D2) `String msg = f"boom with dynamic payload {x}"` + `panic(msg)`
   under `@should_panic("boom with dynamic payload 42")` — pins the
   `gorget_panic_at` consumer (P3). Pass-1 demonstrated this case FAILS
   deterministically pre-fix with a named String local (cleanup frees `str.data`
   before the runner's `strstr`; ASan: heap-use-after-free) — without this test,
   a partial revert of the panic_at hunk stays green.
This fixture is what converts the false-PASS/false-FAIL roulette into a
deterministic guard — for BOTH consumers.

## Measured after (scout prototype)

Repro 2/2 PASS on C AND under `--backend=llvm` (LLVM links the same runtime .c —
verified `main.rs:1256-1442`); a mismatched expectation now FAILs readably
(`got: index out of bounds: index 10, length 3`); dynamic `panic(f"… {x}")`
@should_panic PASSes (P3 class); normal mode byte-identical
(`trap[T_Bounds]: … at file:3:13`, exit 101).

Scout gates: lib 1105/0 · integration `test_` 84/0 · `trap` 30/0 incl.
`self_host_bootstrap_fixed_point` (with `GG_BUILD/TEST_TIMEOUT_SECS=600` — the
120s default flakes under multi-agent contention) · LLVM `trap` (skip self_host)
7/0.

## Risks / accepted residuals

1. 256-byte truncation of >255-char panic messages in test-mode FAIL output
   (normal mode prints full — minor asymmetry; all 27 runtime details fit 96).
2. The false-PASS window CLOSES — a fixture that previously passed via
   stale-stack luck would now surface honestly. `test_` 84/0 says none exists in
   the targeted set; the parent's full sweep is the real check.
3. `gorget_format` leak on assert-fail (`runtime_tostr.c:129`): pre-existing,
   bounded — leave alone; optional follow-up could free post-copy.

## Executor protocol (multi-agent rules in full)

Worktree-isolated; worktree-relative paths only; no `git stash`; checkpoint diff
to /tmp after each work item; stage by explicit file name; final gates FOREGROUND
with generous timeouts (`GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600` for
anything matching `self_host`). Base: apply `/tmp/scout_rr_b_prototype.patch`,
re-derive judgment hunk by hunk (you own it), add the regression fixture.

## Gate list (executor, foreground, tee'd)

1. `cargo build`
2. `cargo test --lib` — 1105/0
3. `cargo test --test integration test_ -- --test-threads=4` — 84 + new fixture /0
4. `GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600 cargo test --test integration trap -- --test-threads=4`
   — **10/0** (7 direct `*_traps` + `self_host_bootstrap`,
   `self_host_bootstrap_fixed_point`, `self_host_unwrap_traps`; pass-1 D1
   regenerated via `--list` — the scout's "30/0" was a different OR-filter's
   figure, do NOT gate against it)
5. `GG_BACKEND=llvm cargo test --test integration --release trap -- --test-threads=4 --skip self_host` — 7/0
6. The new fixture under BOTH backends, plus a paste of the before/after repro
   transcript (false-FAIL and false-PASS both demonstrated fixed).
7. `cargo test --test lints` — no ratchet deltas expected.

Parent (NOT executor): full both-backend sweep + bootstrap at integration.
