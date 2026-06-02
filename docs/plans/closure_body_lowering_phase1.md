# Brief — self-host closure-body lowering, PHASE 1 (non-capturing / direct-call)

FOUNDATIONAL FIDELITY round, PHASE 1 of 2 (Phase 2 = captures/env, queued separately).
Self-host-dir only, **`lower.gg` ONLY**. Re-verified by RUNNING the code (scout, 2026-06-02).
⚠ Needs ≥3 fresh sequential reviews before the executor launches.

## Bug (re-verified empirically: emit-C + cc + run)
The self-host synthesizes a `__Closure_N__call` function whose body is a STUB — the closure
BODY is NEVER lowered. THREE stub sites in `lower.gg` (line numbers drifted from the
handover's ~9009):
- **`lower.gg:9103`** (real closures, in the closure pre-pass `scan_expr_for_closures`
  `:9043`): `Vector[BasicBlock] blocks = [BasicBlock([], GTReturn(OpCopy(0)))]` then
  `gmod.functions.push(GirFunction(call_name, ...))` (`:9104`). Params + return type ARE
  registered (`:9099-9102`); only the body is the stub. The body is recursed ONLY to find
  NESTED closures (`scan_stmts_for_closures`, `:9108`).
- **`lower.gg:9039`** (`emit_it_closure` — implicit-`it` closures): same stub.
- **`lower.gg:9121`** (`EImplicitClosure`): same stub.

So `__Closure_N__call(void* env, args...)` returns the zero-init return place `_0`.
**Empirical (scout RAN it):** `int main(): auto f = (int x): x * 2; print(f(3))` → Rust oracle
`6`, self-host prints **`1`** (garbage). Emitted stub C (`__Closure_0__call`):
```c
int64_t __Closure_0__call(void* __p0, int64_t __p1) {
    __v0 = __p0;     // env ptr (unused)
    __v1 = __p1;     // arg x=3 (unused)
    return __v3;     // __v3 UNINITIALIZED — body `x*2` never lowered
}
```

## Fix — lower the closure body into `__Closure_N__call`'s blocks (mirror Rust)
Rust's `emit_closure_call_function` (`src/ir/lowering/closures.rs:349`, judged REFERENCE-GRADE
by the scout — no scar tissue) builds a fresh function builder, registers params, sets
`expected_type` to the closure's return type, and lowers the body through the SAME
`lower_stmt_as_tail_value` / `lower_block_expr` helpers used everywhere else
(`closures.rs:499-519`), funneling the tail value into `LocalId(0)` via the implicit-return.
Mirror that in the self-host.

At each of the 3 stub sites, REPLACE the `[BasicBlock([], GTReturn(OpCopy(0)))]` stub with a
REAL lowering of the closure body:
1. Build a FRESH `LowerCtx` for the `__Closure_N__call` function — use `lower_function`
   (`lower.gg:8460`) as the template for ctx setup / param registration / block assembly.
   (Prefer a SHARED helper if `lower_function`'s body-lowering core can be factored cleanly —
   reference-grade — else inline mirroring it. The reviewers will check for idiomatic reuse.)
2. Register the function's params as named locals: **param 0 = the env `void*` ptr** (present
   for the `__callable_N` ABI; UNUSED in Phase 1 — no captures), **params 1..N = the closure's
   declared params** by name (so the body can reference them).
3. Set the ctx's `expected_type` to the closure's declared return type (so a tail expr's
   result local types correctly — same mechanism the EIf/EMatch rounds rely on).
4. Lower the body via `lower_block_expr(&ctx, body, &gmod)` (`lower.gg:4108`) — the closure
   body is a `Vector[Stmt]` (`EClosure(Vector[Param], Vector[Stmt])`, `ast.gg:69`), exactly
   what `lower_block_expr` consumes; an expr-body closure is a single tail `SExpr`. Funnel the
   returned tail-value local into `_0` (the return place) and emit `GTReturn(OpCopy(0))` — i.e.
   `GIAssign(0, op_consume(&ctx, &gmod, tail_val, CkReturn()/CkAssign()))` then return — mirror
   how `lower_function` finalizes a normal function's return.
5. Replace the stub `blocks` with `ctx`'s assembled blocks before the `gmod.functions.push`.

Do the SAME at all 3 sites (`:9103`, `:9039`, `:9121`). The `emit_it_closure`/`EImplicitClosure`
bodies are also `Vector[Stmt]`/expr — route them through the same path.

## ⚠ Phase-1 SCOPE + the capturing-closure HAZARD (read carefully)
Phase 1 fixes **non-capturing, direct-called** closures only. Phase 2 (queued) does
captures/env. BUT the pre-pass lowers EVERY closure's body, including CAPTURING ones — and a
capturing closure's body references a captured var (e.g. `k`) that is NOT a registered local
in the fresh closure-function ctx (Phase 2 will load it from the env struct; Phase 1 has no
env). **This must NOT silently miscompile and must NOT panic/crash the self-host driver.**
- Determine empirically how the body lowering resolves an unresolved free-var reference in the
  fresh ctx (lower_fail/drop? a `[bug]` placeholder? a hard error?). The ACCEPTABLE outcomes
  are: the capturing fixture stays WRONG-OUTPUT or becomes CC-FAIL (no worse than today's
  garbage). The UNACCEPTABLE outcomes are: (a) the self-host DRIVER panics/crashes while
  compiling a capturing-closure fixture (would turn a WRONG fixture into a driver-crash, and
  could be a worse failure mode), or (b) a silent wrong-but-plausible resolution of the
  capture to some other local/global.
- If lowering a capturing body risks a driver panic, GUARD it: detect that the body references
  a name that is not a param (a free var) and, for Phase 1, FALL BACK to the existing stub for
  that closure (so capturing closures keep today's behavior — still broken, but not worse) and
  log it for Phase 2. A clean free-var check (does the body reference any `EIdentifier` whose
  name is not a param and not a global fn?) mirrors Rust's `collect_free_vars`
  (`closures.rs:604`) — if it finds captures, Phase 1 leaves the stub. Prefer this guard over
  risking a regression. (The reviewers must confirm the chosen behavior is safe.)

## Scope / expected outcome
**Candidate fixtures that should move (non-capturing, direct-call) — re-measure, snapshot ONLY
those that reach MATCH:** `closure_block_tail_expr`, `snag51_closure_block_tail_value`, the
non-capturing `.map((int x): x*2)` / `closure_compose` family, and similar. ⚠ Some will STILL
CC-FAIL on ADJACENT pre-existing gaps the scout found (function-type-param `__callable_N`
arg-coercion; `'self' undeclared`) — those are **Phase 1.5**, out of scope; do NOT force them.
⚠ The handover-named `auto_types` + `test_if_expressions` case 20 are **CAPTURING → Phase 2**,
they will NOT move here — do not expect or snapshot them.

## Validation gate (self-host-dir only — no `src/`)
⚠ **`bootstrap_fixed_point` is NOT the validation signal here** (unlike the EIf round — the
self-host DRIVER sources define no lambdas/`.map`, so the driver's own output is unaffected).
It must still stay GREEN as a REGRESSION guard, but the real validation is the runtime-diff.
1. `cargo build` + `cargo test --lib` green.
2. Force-rebuild the driver. Emit-C for a non-capturing direct-call fixture (e.g.
   `closure_block_tail_expr` + a minimal `auto f=(int x):x*2; print(f(3))`): confirm
   `__Closure_N__call` now LOWERS the body (no uninitialized `return _0`), cc → run → MATCH the
   Rust oracle.
3. **Capturing-closure safety check:** emit-C + (attempt) cc for a capturing fixture
   (`int k=10; auto f=(int x):x+k; print(f(3))`): confirm the driver does NOT panic and the
   outcome is WRONG-OUTPUT or CC-FAIL (NOT a driver crash, NOT a silent wrong-plausible value).
   Report exactly what happens.
4. **`self_host_runtime` lock-in net: GREEN, ≥256/0** — NO existing snapshot may regress
   (none of the 256 should depend on stub closure behavior, but PROVE it). Add new snapshots
   ONLY for fixtures that newly reach MATCH.
5. **`GG_RUNTIME_DIFF=1 … self_host_runtime_diff`**: report the parity delta + confirm NO
   fixture goes MATCH→worse (especially no MATCH→CRASH from a capturing body now panicking).
6. `c_emit_comparison` / `lowerer_comparison` (`--nocapture`): report counts, unchanged-or-better.
7. `self_host_bootstrap_fixed_point`: GREEN (regression guard).
Pipe long runs through `tee /tmp/closure-<name>-$RANDOM.log`.

## Follow-ups to LOG in TODO.md (out of scope — do NOT bundle)
- **Phase 2 — captures + env** (the re-architecture; see the TODO entry for the full scoping).
- **Phase 1.5 — adjacent direct-call gaps:** `__callable_N` function-type-param arg-coercion
  (CC-FAILs `closure_multiline_return`); `'self' undeclared` in closure block-tail
  (`snag51_closure_block_tail_value`, if it doesn't clear with body-lowering alone).
- The `lir_codegen.gg:3714` `name.starts_with("__make_closure_")` `.env=NULL` name-matching
  wart (CLAUDE.md "no name matching") — Phase 2 replaces it with a real env StructInit.

## Files (stage by name only — never `-a`)
`tests/fixtures/self_host_lowerer/lower.gg` + any new `tests/fixtures/runtime_snapshots/*.out`.
Do NOT touch `loader.gg`, `lir_*.gg`, `src/`, `TODO.md`/`DONE.md` (parent owns TODO/DONE).
