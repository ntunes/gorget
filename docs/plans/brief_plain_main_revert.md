# BRIEF — Plain-main revert (the macOS fix for gorget-arena; Option A)

Status: v1 (orchestrator draft, 2026-06-11). GATED on slot-coalescing being
integrated + `fixed_point` GREEN (now merged `6a5f7251`; the coalescing makes
the self-host fit a plain ~8MB stack — the scout/executor proved a `ulimit -s
8192` self-compile works). NEEDS ≥2 fresh brief-reviews before launch.

## Mission
Revert the forced 64MB-pthread `main` (Fix B, `79842a7f`) back to a PLAIN
thread-0 `main` in BOTH emitters, so a compiled binary runs `main` on the OS
main thread (thread 0). This is the macOS fix: gorget-arena (SDL/Cocoa) requires
UI init on thread 0, but the pthread-main runs user `main` on a secondary thread
→ `NSInternalInconsistencyException: setting the main menu on a non-main thread`.
The coalescing (just landed) lets the self-host COMPILER bootstrap on a plain
~8MB stack, so the 64MB pthread is no longer needed. **OWNER DECISION (Option A,
2026-06-11):** keep the honest OS-default stack — NO `run_with_stack` opt-in;
deep NON-tail user recursion (`stack_guard_deep_recursion`) becomes
EXPECT-FAIL/documented (a plain binary overflows it like C/Rust).

## The current emission to revert (BOTH emitters — keep them byte-identical)
- Rust: `src/backend/c_lir/mod.rs` — the `is_native_target` gate (~`:1071`) +
  the `func.name == "main" && is_native_target` block (~`:1976-2015`): emits
  `static int __gorget_user_main(void)` (the body) + `__gorget_main_trampoline`
  + the `int main(argc,argv)` wrapper that does `sigfillset`/`pthread_sigmask`
  routing + `pthread_attr_setstacksize(64MB)` + `pthread_create`/`pthread_join`.
- Self-host twin: `tests/fixtures/self_host_lowerer/lir_codegen.gg`
  `emit_pthread_main_runner` (~`:5003-5035`) — the identical emitted text.
  ⚠ `c_emit_comparison` counts the `) {` body-openers on both sides, so the
  revert MUST change both emitters IDENTICALLY.

## The revert (the plain main to emit)
Emit a PLAIN `int main(int argc, char** argv)` that runs the user body on thread
0 — the pre-Fix-B shape, PLUS the one real improvement Fix B added (keep it):
- `gorget_init_args(argc, argv);` + the trace-init (if `module.trace_filename`).
- Run the user body on thread 0 directly. KEEP the **void-main → exit-0
  contract** (Fix B's legit fix: a void Gorget main returns 0, not an undefined
  register) — for a void-returning main emit the body then `return 0;`; for an
  int-returning main `return <body result>;`. (The LIR already injects a
  synthetic `return 0` tail for void mains per the lean-runtime scout — confirm
  whether an explicit `return 0` is still needed or already present.)
- DROP entirely: `__gorget_main_trampoline`, the `__gorget_main_sigmask` +
  `sigfillset`/`pthread_sigmask` routing (only needed because user code ran on a
  secondary thread — plain main makes signals hit thread 0 naturally), the
  `pthread_attr_setstacksize`/`pthread_create`/`pthread_join`. The
  `lean_runtime_prototype.diff` plain-main hunk is a REFERENCE only — do a CLEAN
  revert (no `if (false)` / dead `is_native_target` prototype shortcuts; if
  `is_native_target` becomes unused after the revert, remove it or keep only if
  still gating the FREESTANDING vs native preamble split — check `emit_types.rs`
  freestanding early-return still works).
- FREESTANDING (`module.target.starts_with("freestanding")`) already emits a
  plain main today (no pthreads) — confirm the revert doesn't disturb it.

## Pthread linking
`add_thread_flags` (`src/main.rs:~208`) currently passes `-lpthread` for every
native binary (since Fix B). KEEP it (the Task/async scheduler + spawn still use
pthreads; it's harmless for non-threaded programs). Do NOT make it conditional
in this change.

## stack_guard tests (Option A)
- `tests/fixtures/stack_guard_deep_recursion.gg` (`deep(200000)`, non-tail,
  ~22MB) — currently passes ONLY via the 64MB main. After plain main it
  SIGSEGVs at the OS default. Per Option A: make its integration test
  EXPECT-FAILURE (expect the overflow / non-zero exit) OR retire the fixture,
  with a comment that a plain binary overflows deep non-tail recursion at the OS
  stack (like C/Rust); TCO (`## Low`) is the eventual cure for the tail subset.
- `tests/fixtures/stack_guard_self_host_driver_deep_lowering*` — this guards the
  COMPILER's deep lowering, which COALESCING fixed → it must STILL PASS on plain
  main (it's the positive validation that the frame fix works). Confirm.
- Re-grep ALL `stack_guard_*` integration tests (`tests/integration.rs`) and
  adjust each per its intent (compiler-recursion guard = pass; runtime-recursion
  guard = expect-fail).

## Gates (executor: build + lib + a FAST self-compile check, then COMMIT; the
## PARENT runs fixed_point — do NOT run fixed_point yourself, you will park ~8min)
- `cargo build`; `cargo test --lib`; `cargo test --test lints`.
- **Trivial-program plain-main proof (the macOS fix):** `gg build` a trivial
  program; the emitted C `int main` has ZERO `__gorget_main_trampoline` /
  `pthread_create` / `pthread_attr_setstacksize` references and runs on thread 0;
  it runs + exits 0 (void main) / the right code.
- **Fast bootstrap-viability check:** build the self-host driver (now plain-main)
  and run it self-compiling its OWN source under `ulimit -s 8192` → it must
  succeed (no stack overflow) — this proves the coalesced self-host fits a plain
  8MB stack on plain main. (Do NOT run the full multi-stage `fixed_point`.)
- `stack_guard_*` tests adjusted + passing/expect-failing per Option A.
- COMMIT after the above. The PARENT runs `self_host_bootstrap_fixed_point`
  (GREEN on plain main is THE load-bearing proof) + the full battery +
  `c_emit_comparison` (the main shape changed in both emitters → must stay
  matched).

## Constraints
- Worktree preamble; explicit-file `git add`; no push; STOP-and-report on a
  contradicted premise (esp. the self-host NOT fitting 8MB on plain main, or a
  `c_emit_comparison` mismatch from asymmetric emitter edits).
- Zone: `src/backend/c_lir/mod.rs`, `tests/fixtures/self_host_lowerer/lir_codegen.gg`,
  `tests/integration.rs` (the `stack_guard_*` tests),
  `tests/fixtures/stack_guard_deep_recursion.gg` (maybe), `src/main.rs` (only if
  `is_native_target`/thread-flag cleanup needed), TODO/DONE.
- After this lands + the owner verifies gorget-arena runs on macOS, the
  gorget-arena macOS regression is CLOSED.
- Commit cites this brief + Co-Authored-By.
