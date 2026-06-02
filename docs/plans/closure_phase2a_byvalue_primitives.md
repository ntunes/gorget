# Brief — closure PHASE 2a: re-architecture + ByValue PRIMITIVE captures

FOUNDATIONAL FIDELITY round, PHASE 2a of Phase 2 (2b = ByValue resource/CoW, 2c = ByMutRef —
both QUEUED, NOT in scope here). Self-host-dir only. Touches `lower.gg` + `gir.gg` +
`lir_lower.gg` + `lir_codegen.gg` (NOT disjoint — serialize against other lower-layer chains).
Re-verified by RUNNING the code (two parallel scouts, 2026-06-02). ⚠ Needs ≥3 fresh sequential
reviews before the executor launches — this is a RE-ARCHITECTURE; expect blocking findings.

## Goal + what 2a delivers
Phase 1 lowered NON-capturing closure bodies; CAPTURING closures keep a stub (`.env=NULL`,
empty env struct, captures lost → wrong output). 2a makes **ByValue PRIMITIVE captures** work
by mirroring Rust's reference-grade TWO-LAYER closure construction, and builds the shared
re-architecture (lifted-closure record + positive free-var collection + post-pass body emit)
that 2b/2c will extend. **Unblocks** `auto_types` (`int base`), `test_if_expressions` c20
(`c1..c4` ints), `test_closures_edge_cases` primitive cases, and the primitive-capture parts of
`closures.gg`. Resource-capturing (CoW) and ByMutRef closures STAY STUBBED in 2a (deferred).

## Architecture (mirror Rust's two-layer split — judged reference-grade by both scouts)
Rust: GIR `lower_closure` (`src/ir/lowering/closures.rs:79`) builds a stack env struct via the
generic `StructInit` + records a `LiftedClosure`; LIR `try_closure_pack`
(`src/lir/lower/operands.rs:1233`) promotes a `__Closure_N`-typed value assigned into a
`GorgetClosure` slot to a HEAP env (`env_alloc` + memcpy) + `Inst::ClosurePack`; C-emit
`ClosurePack` is fully typed (no name-match). The self-host's current `__make_closure_`
pseudo-call with hardcoded `.env=NULL` (`lir_codegen.gg:3707-3715`) is **strictly worse than
Rust's worst name-touch** (it replaces the whole env mechanism with NULL). 2a replaces it with
the real path. ⚠ KEY RECONCILED FACT: there is NO GIR-level struct-init op — `lir_lower.gg`'s
**user-struct-ctor intercept** (`:2294`, emits `IStructInit` at `:2355`/`:2767`) turns a normal
struct-constructor call into `IStructInit`. So `__Closure_N` must be built like a NORMAL struct
(a GIR struct-constructor), which already lowers to `IStructInit` — no new GIR/LIR op needed.

## The fix (4 files; the work is overwhelmingly GIR-side — LIR/C machinery already exists)

### (1) `gir.gg` — typed records (add fields at the END per the gir.gg positional-ctor convention)
- `Vector[LiftedClosure] lifted_closures` field on `GirModule` (END of the struct; gir.gg
  already documents "added at END to avoid shifting positional ctor call sites"). ⚠ VERIFY
  `GirModule` is in-memory IR (NOT the serialized resource schema) → NO `SCHEMA_VERSION` bump
  needed (that version gates `compiler/data/{schema,resources}.gg`, a different registry).
- `LiftedClosure{int id, String struct_name, String call_name, Vector[ClosureCapture] captures,
  Vector[String] param_names, Vector[int] param_types, int return_type, Vector[Stmt] body}`.
- `ClosureCapture{String name, int type_id, int local_id, int mode}` — `mode`: int const
  `CAP_BY_VALUE = 0` (2a). Reserve `CAP_BY_MUT_REF = 1` for 2c (don't implement it). The capture
  VECTOR ORDER is the single source of truth: env-struct field order == struct-ctor arg order ==
  body `GIFieldLoad` index order.
- Fix the STALE comment at `gir.gg:126` ("GIFieldLoad … DEAD CODE") — it is LIVE (lowered at
  `lir_lower.gg:3324`); 2a makes it an emit site.

### (2) `lower.gg` — capture collection (positive), env-struct registration, make-site, post-pass body
- **Positive free-var collector** (replaces the Phase-1 INVERTED guard for the lowering decision).
  In `lower_expr`'s `EClosure` arm (`lower.gg:5651-5666`) — where `ctx.named_locals` IS live —
  walk the body; a bare `EIdentifier(name)` that is NOT a closure param / `it` / body-local
  (reuse the Phase-1 walk's pattern-binding + nested-closure-skip + body-local tracking
  structure verbatim) and FOR WHICH `nl_contains(&ctx, name)` is TRUE is a CAPTURE — record
  `(name, local_id = nl_get(&ctx,name), type_id = ctx.locals.get(local_id).type_id)`. (This is
  Rust's positive `lookup_local` test, `closures.rs:636`.)
- **2a CLASSIFICATION GUARD (load-bearing — keeps 2a in scope):** for each capture, 2a handles it
  ONLY IF it is a **ByValue primitive** — i.e. (a) the capture's `type_id` is a primitive
  (int/float/bool/char — NOT a resource/String/Vector/struct/enum), AND (b) it is NOT mutated in
  the body (no assignment/compound-assign to it — mirror Rust `detect_mutations` `:781`, narrow
  to `Block` bodies). If a closure has ANY capture that is a resource (→ 2b) or mutated (→ 2c),
  2a KEEPS THE STUB for that whole closure (exactly as Phase 1 does today) and does NOT lift it.
  So 2a lowers: non-capturing closures (Phase-1's wins) + closures whose every capture is a
  ByValue primitive. Everything else stays stubbed.
- **Register `__Closure_N` as a real struct** — `gmod.type_infos["__Closure_N"]` gets one
  `GirFieldInfo(capture.name, type_name_of(capture.type_id))` per capture in capture order
  (replaces the `{char __pad;}` placeholder; mirror the `IStruct` registration pattern at
  `lower.gg:10825-10831`). LIR Pass-2 (`lir_lower.gg:883-894`) then fills `LirStructDef.fields`
  automatically and codegen stops emitting `char __pad`.
- **Make-site** — replace `GICallExtern(cdst, "__make_closure_N", [])` with a normal
  **struct-constructor** of `__Closure_N` whose field args are the capture sources (for 2a
  primitives: `OpCopy(local_id)` per capture — bit-copy, NO CoW). This routes through the
  existing user-struct-ctor → `IStructInit` path. The resulting `__Closure_N` value is then
  assigned into the closure's `GorgetClosure` slot (which the LIR promotion in step 3 packs).
  Record the `LiftedClosure` here too (params + body + captures), for the post-pass.
- **Post-pass body emit** — move closure-body lowering OUT of the context-less pre-pass into a
  POST-PASS that drains `gmod.lifted_closures` (mirror Rust `mod.rs:1429`), calling
  `lower_closure_body` with captures KNOWN. The pre-pass (`scan_expr_for_closures`,
  `lower.gg:9662`) KEEPS emitting the `__Closure_N` struct-reg + the call-fn SIGNATURE (so ids /
  `fn_sigs` ordering stay stable), but NOT the body. In `lower_closure_body` (`lower.gg:9598`),
  BEFORE lowering the body, for each capture emit `GIFieldLoad(fdst, env_local=_1,
  capture_index)` (value-typed dst for 2a primitives) and `nl_put` the capture name → fdst
  (mirror `emit_closure_call_function:383-427`). ⚠ This DISSOLVES the Phase-1 nested-closure
  counter-desync guard (`expr_has_nested_closure`, `lower.gg:9406`) — with body-emit in a
  post-pass + capture info, a nested EClosure routes through the same lifted machinery. Verify
  the guard can be retired (or is inert) and that the gmod vs pre-pass closure-id counters
  (`gmod_next_closure_id` `lower.gg:3515` / the pre-pass local counter `lower.gg:11736`) stay
  aligned through the re-architecture.

### (3) `lir_lower.gg` — closure-pack promotion (the `try_closure_pack` equivalent)
When a `__Closure_N`-typed value is assigned into a `GorgetClosure` slot, emit
`__gorget_closure_env_alloc(sizeof(env))` (runtime, `runtime_string.c:139`) + `IMemcpy(heap_ptr,
&stack_env, size)` + `IClosurePack(slot, heap_ptr, call_func, false)`. Mirror Rust
`try_closure_pack` (`operands.rs:1233-1355`). All these LIR ops exist (`lir.gg`: `ISlotAddr`
`:107`, `IMemcpy` `:154`, `IStructInit` `:167`, `IClosurePack`/`ICallClosure` `:191/:197`); the
`IClosurePack`/`ICallClosure` codegen (`lir_codegen.gg:3390/3397`) is READY but currently dead.

### (4) `lir_codegen.gg` — DELETE the wart
Remove the `name.starts_with("__make_closure_")` branch (`:3707-3715`) and its return-type entry
(`:2472-2474`). Once the make-site builds a real struct + the LIR promotion packs it, the
`__make_closure_` pseudo-call no longer exists. The env struct (real fields), field-store, and
field-load codegen are unchanged (already correct).

## ⚠ Out-of-scope adjacents (KEEP STUBBED / log, do NOT fix here)
- **ByValue RESOURCE captures (String/Vector/struct) → Phase 2b** (needs make-site CoW
  clone/move + `is_closure_env`-style ownership + per-field recursive drop of `__Closure_N`).
- **ByMutRef (mutated) captures → Phase 2c** (`MutPtr` fields + `emit_borrow_mut` +
  `GIDerefStore` write-back; riskiest — drop interaction, no borrow-check pass).
- **String-PARAM closure-call-ABI gap** (`lir_codegen.gg:3672` casts to
  `(void*(*)(void*,void*))` but a `(String name):` closure takes `Str` by value → `cc` error on
  `closure_multiline_return`). Independent of captures; blocks String-PARAM closures regardless.
  Phase 1.5. 2a (primitive captures, primitive params) avoids it.

## Validation gate (self-host-dir only — no `src/`)
⚠ **`bootstrap_fixed_point` is NOT the validation signal** (the self-host driver uses no
lambdas/`.map`) — it must stay GREEN as a REGRESSION guard, but real validation is the
runtime-diff.
1. `cargo build` + `cargo test --lib` green.
2. Force-rebuild driver. Emit-C for `int main(): int k=10; auto f=(int x): x+k; print(f(3))`:
   confirm `__Closure_0` now has a `k` field, the make-site allocs+populates a real env (not
   `.env=NULL`), the body `GIFieldLoad`s `k`, cc → run → prints **`13`** (Rust oracle).
3. **MUST NOT regress Phase 1:** the 4 Phase-1 snapshots (`closure_block_tail_expr`,
   `closure_as_callback`, `closure_capture_loop_var`, `consume_callable_once`) must still MATCH
   — they now route through the re-architected post-pass. `self_host_runtime` ≥260/0.
4. New MATCHes: `auto_types`, `test_if_expressions` c20 (if it fully clears — it has 4 int
   captures), `test_closures_edge_cases` primitive cases. Add snapshots ONLY for fixtures that
   actually reach MATCH; capturing-resource/ByMutRef fixtures stay stubbed (WRONG) — do NOT
   snapshot them, and confirm they don't FALSE-MATCH (collapse) or driver-crash.
5. `GG_RUNTIME_DIFF=1 … self_host_runtime_diff`: report parity delta + NO fixture MATCH→worse.
6. `c_emit_comparison` (baseline 880) / `lowerer_comparison` (952) unchanged-or-better.
7. `self_host_bootstrap_fixed_point` GREEN (regression guard — proves the re-arch didn't break
   the closure-free driver path).

## Follow-ups to LOG (out of scope)
- Phase 2b (ByValue resource/CoW captures), Phase 2c (ByMutRef), Phase 1.5 (String-param ABI).
- If retiring the Phase-1 nested-closure safety guard, confirm nested closures still id-align.

## Files (stage by name only — never `-a`)
`tests/fixtures/self_host_lowerer/{lower.gg, gir.gg, lir_lower.gg, lir_codegen.gg}` + new
`tests/fixtures/runtime_snapshots/*.out`. Do NOT touch `loader.gg`, `src/`, `TODO.md`/`DONE.md`.
