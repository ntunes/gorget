# BRIEF — Plain-main revert (the macOS fix for gorget-arena; Option A)

Status: v1 (orchestrator draft, 2026-06-11). GATED on slot-coalescing being
integrated + `fixed_point` GREEN (now merged `6a5f7251`; the coalescing makes
the self-host fit a plain ~8MB stack — the scout/executor proved a `ulimit -s
8192` self-compile works). Pass-1 fold (fresh reviewer, core design PASS, 4
verification-hardenings): void→exit-0 is ALREADY guaranteed at the GIR layer
(i32-main predates Fix B) → NO explicit return 0, NO tail-append (wrong for
early-returning mains) + prove by running bare-return/no-return void mains; add
`stack_guard_self_host_driver_deep_lowering` to the fast gate (the ulimit-8MB
proxy doesn't cover its deeper recursion) + rewrite its stale "Fix B regressed"
message; corrected names/lines; LLVM is already plain (out of scope).
✅ REVIEW-CLEAN: pass-2 (fresh) DESIGN SIGN OFF — re-verified void→exit-0 by
RUNNING a void-main with a non-collapsible early return (both arms emit
`return 0`, zero bare `return;`) + confirmed all v2 folds + the design. Its one
reservation was NON-BLOCKING (elegance): retire Fix B's now-false pthread
comments (folded as the "Retire Fix B's now-FALSE comments" section — comment-
only). 2 fresh passes (substantive folds → design sign-off). READY for the
executor.

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
  the `func.name == "main" && is_native_target` pthread-main block (~`:2040-2079`
  — pass-1-verified; re-grep): emits `static int __gorget_user_main(void)` (the
  body, ~`:1076`) + `__gorget_main_trampoline` (~`:2049`) + the `int
  main(argc,argv)` wrapper that does `sigfillset`/`pthread_sigmask` routing
  (~`:2065`) + `pthread_attr_setstacksize(64MB)` (~`:2072`) +
  `pthread_create`/`pthread_join` (~`:2073`/`:2076`).
- Self-host twin: `tests/fixtures/self_host_lowerer/lir_codegen.gg`
  `emit_pthread_main_runner` (~`:5611`, invoked ~`:5595`; main signature
  ~`:5350` — pass-1-verified) — the identical emitted text.
  ⚠ `c_emit_comparison`'s `user_fn_count` counts `) {` body-openers
  (`tests/integration.rs:~14053`): the pthread shape emits **3**
  (`__gorget_user_main` + `__gorget_main_trampoline` + `int main`), plain main
  emits **1** — so the revert MUST change both emitters IDENTICALLY or parity
  diverges by 2/fixture.

## The revert (the plain main to emit)
Emit a PLAIN `int main(int argc, char** argv)` that runs the user body on thread
0 — the pre-Fix-B shape, PLUS the one real improvement Fix B added (keep it):
- `gorget_init_args(argc, argv);` + the trace-init (if `module.trace_filename`).
- Run the user body on thread 0 directly. **void-main → exit-0 is ALREADY
  GUARANTEED at the GIR layer (pass-1-verified by source-read AND running 4
  void-main shapes):** `main` is given an `I32_TYPE` return type
  (`src/ir/lowering/functions.rs:~638`, which PRE-DATES Fix B), the implicit
  tail injects `const_i32(0)`, and explicit/bare void returns are unit→i32-0
  coerced — so EVERY void-main return path emits `return 0` and the C-backend
  `Term::RetVoid → "return;"` is NEVER reached for main (main arrives non-void).
  So: emit a plain `int main` running the body — **do NOT add an explicit
  `return 0`, and do NOT tail-append one** (a single tail-append would be WRONG
  for an EARLY-returning void main; the body already returns 0 at every path).
  PROVE it: build+run a void main ending in a bare `return` AND a void main with
  no `return` → confirm ZERO bare `return;` in the emitted body and exit 0.
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

## Retire Fix B's now-FALSE comments (CLAUDE.md "stale justification = tech debt")
Fix B left several comments justifying pthreads via "the native main runner runs
the body on a pthread" — FALSE after this revert (the things must STAY, but for
the SCHEDULER/async/sync reason, not the reverted main runner). Rewrite each
(pass-2-found; re-grep exact lines):
- `src/backend/c/runtime/runtime_preamble.c:~34` — the unconditional
  `#include <pthread.h>` (added by Fix B). KEEP it (the Task/async scheduler +
  sync runtime need `pthread.h`); rewrite its comment (~`:25-30`) to cite the
  scheduler, not the main runner.
- `src/main.rs:~204-206` — the `add_thread_flags` doc comment ("Since the Fix B
  … pthread main runner, EVERY native binary needs pthreads") → rewrite to cite
  the scheduler/spawn/sync.
- `tests/integration.rs:~23519-23522` — the `stack_guard_self_host_driver_deep_lowering`
  DOCSTRING (says "its body runs on the 64MB pthread") → rewrite (it now
  validates SLOT-COALESCING on a plain 8MB main).
- `src/backend/llvm/mod.rs:~2157-2165` — the LLVM `@main` TODO ("port the runner
  shape here to close the gap") → the gap CEASES to exist post-revert (LLVM
  `@main` on the host stack IS the intended plain-main behavior now); revisit/
  close it (no LLVM code change — comment only).
These are comment-only edits (no behavior change); they keep the revert from
leaving the same false-history debt Fix B is being reverted out of.

## stack_guard tests (Option A) — there are EXACTLY TWO (pass-1-verified)
- **`stack_guard_runtime_deep_recursion`** (test fn at `tests/integration.rs:~23599`;
  fixture `stack_guard_deep_recursion.gg`, `deep(200000)` non-tail ~22MB; asserts
  SUCCESS ~`:23633`) — RUNTIME-recursion, passes ONLY via the 64MB main. Per
  Option A → make it EXPECT-FAILURE (expect the overflow / non-zero exit) OR
  retire the fixture, with a comment that a plain binary overflows deep non-tail
  recursion at the OS stack (like C/Rust); TCO (`## Low`) is the eventual cure
  for the tail subset.
- **`stack_guard_self_host_driver_deep_lowering`** (test fn at `~:23525`; driver
  under `ulimit -s 8192` lowering a 200-term concat chain; asserts SUCCESS
  ~`:23560`) — COMPILER-recursion, which COALESCING fixed → it must STILL PASS on
  plain main (the positive validation that the frame fix works). ⚠ Its
  failure-assertion MESSAGE (`~:23562`) still says "the Fix B pthread main runner
  regressed" — REWRITE it: this test now validates SLOT-COALESCING on a plain
  8MB main, NOT the 64MB pthread reserve.
- Both `skip_under_llvm()`. Re-grep ALL `stack_guard_*` tests to confirm these
  are the only two; adjust each per intent (compiler-recursion = pass +
  message-rewrite; runtime-recursion = expect-fail).

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
- **ALSO run `cargo test --test integration stack_guard_self_host_driver_deep_lowering`**
  (a single fast targeted test) — the `ulimit -s 8192` self-compile above does
  NOT cover the DEEPER 200-term-concat-chain recursion this test exercises;
  catching a residual frame problem here is cheap vs. the parent's ~8min
  `fixed_point`. It must PASS on plain main.
- `stack_guard_runtime_deep_recursion` adjusted to expect-fail per Option A; the
  full `stack_guard_*` set passing/expect-failing as classified above.
- COMMIT after the above. The PARENT runs `self_host_bootstrap_fixed_point`
  (GREEN on plain main is THE load-bearing proof) + the full battery +
  `c_emit_comparison` (the main shape changed in both emitters → must stay
  matched).

## Constraints
- ⚠ **LLVM backend is OUT OF SCOPE — already plain (pass-1-verified):** `@main`
  (`src/backend/llvm/mod.rs:~2166`) already emits a plain `define i32 @main` on
  thread 0; Fix B explicitly scoped LLVM out and never gave it the pthread
  runner, and the `stack_guard_*` tests `skip_under_llvm()`. So the LLVM leg is
  already consistent with the new plain-C behavior — NO change there; don't go
  hunting.
- The `is_native_target` local (`mod.rs:~1071`) is used in exactly 3 sites, all
  in `emit_function` (pass-1-verified): the pthread block (delete), the
  main-signature branch (collapse — both native and freestanding emit the
  identical plain `int main` + `gorget_init_args` + trace-init the freestanding
  branch already emits), leaving `is_native_target` UNUSED → remove it. The
  freestanding preamble split lives in `emit_types.rs` keyed on `module.target`
  (NOT this local), so it's undisturbed.
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
