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
   ⚠ **(pass-1 nit-2 — MANDATORY) you MUST run `lower_function`'s body-FINALIZATION post-passes
   on the closure ctx, not just assemble blocks:** `push_drop_scope`/`pop_drop_scope` around the
   body, then `compute_liveness` + `wire_liveness_into_modes` + `flush_drop_queue`
   (`lower.gg:8592-8605`) BEFORE assembling `ctx.block_insts`/`block_terms` into blocks. These
   set the OpMove/OpClone operand modes + emit drops; skipping them leaves every consuming read
   at its default mode → a SILENT MISCOMPILE for any closure body that touches a resource. The
   RIGHT way to guarantee this is to **factor a SHARED helper** out of `lower_function`'s
   post-body core (drop-scope pop → liveness → wire-modes → flush → block-assemble → return
   finalize) and call it from BOTH `lower_function` and the closure-call synthesis (reference-
   grade reuse, mirrors why Rust shares its body path). Inlining a partial copy that omits these
   passes is the failure mode to avoid. (Two other fresh-ctx precedents exist —
   method `lower.gg:8727`, test-fn `:10788` — confirming the pattern.)
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

Do the SAME at all 3 sites (`:9103`, `:9039`, `:9121`), with these per-site specifics
(pass-1 nit-1 — the 3 sites are NOT symmetric):
- **`:9103` real closures** — body is `Vector[Stmt]` → `lower_block_expr(&ctx, body, &gmod)`.
- **`:9121` `EImplicitClosure`** — its body is a SINGLE `Box[SpannedExpr]` (NOT a `Vector[Stmt]`)
  with an implicit param named `"it"` → lower via `lower_expr(&ctx, *body_box, &gmod)` (or wrap
  the expr as a one-element tail block); register the `"it"` param. The free-var guard still
  applies (a body referencing a non-`it`, non-param name is a capturer → keep its stub).
- **`:9039` `emit_it_closure`** — ⚠ its current signature `int emit_it_closure(int next_id,
  GirModule &gmod)` takes **NO body parameter** (called at `:9136`/`:9143` knowing only
  `expr_has_it(arg)`). To lower the implicit-`it` body you MUST change the signature to accept
  the body expr and update BOTH call sites to pass `arg`. Then lower via `lower_expr` as for
  `:9121` (implicit param `"it"`). Mechanical, but don't miss it.

## ⚠ Phase-1 SCOPE + the capturing-closure HAZARD (read carefully)
Phase 1 fixes **non-capturing, direct-called** closures only. Phase 2 (queued) does
captures/env. BUT the pre-pass lowers EVERY closure's body, including CAPTURING ones — and a
capturing closure's body references a captured var (e.g. `k`) that is NOT a registered local
in the fresh closure-function ctx (Phase 2 will load it from the env struct; Phase 1 has no
env). **This must NOT silently miscompile and must NOT panic/crash the self-host driver.**
- **EMPIRICALLY DETERMINED (pass-1 built the driver + traced it):** an unresolved free-var
  `EIdentifier` (a capture) hits `lower_expr`'s `EIdentifier` fallback (`lower.gg:4451-4460`)
  → `diag_bug(...)` (just prints a harmless `/* [bug] ... */` C comment, NO abort, NO
  error-count) + `GIAssign(unk, OpConstI64(0))`. So WITHOUT a guard, a capturing closure body
  COMPILES and RUNS but **silently collapses the capture to 0** (`x + k` → `x + 0` → prints
  `3`, not `13`; driver exits 0, cc exits 0). There is NO driver crash and NO CC-FAIL — it is
  the **(b) UNACCEPTABLE silent-wrong-but-plausible** outcome.
- **THEREFORE THE GUARD IS REQUIRED (not optional).** Detect captures with a free-var check
  mirroring Rust's `collect_free_vars` (`closures.rs:604`): does the body reference any
  `EIdentifier` whose name is NOT one of the closure's params and NOT a global fn / const? If
  YES (the closure captures), Phase 1 leaves the STUB for that closure (keeps today's behavior
  — broken, but Phase 2 fixes it) and does NOT lower its body. Two reasons the guard is
  load-bearing: (1) it prevents converting today's uninitialized garbage into a DETERMINISTIC
  silent-wrong value, and (2) — more important — it prevents **false-parity inflation**: a
  capture-collapses-to-0 body could coincidentally match an oracle and get snapshotted as a
  "MATCH" that is actually a miscompile (the forbidden anti-pattern). Only NON-capturing
  closures get their body lowered in Phase 1. (Reuse the same free-var walk for the
  `emit_it_closure`/`EImplicitClosure` variants — an implicit-`it` closure body that references
  a non-`it`, non-param free var is likewise a capturer → keep its stub.)

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
