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
- **Positive free-var COLLECTOR** (replaces the Phase-1 INVERTED guard for the lowering decision).
  In `lower_expr`'s `EClosure` arm (`lower.gg:5651-5666`) — where `ctx.named_locals` IS live —
  walk the body; a bare `EIdentifier(name)` that is NOT a closure param / `it` / body-local and
  FOR WHICH `nl_contains(&ctx, name)` is TRUE is a CAPTURE — record `(name, local_id =
  nl_get(&ctx,name), type_id = ctx.locals.get(local_id).type_id)`. (Rust's positive `lookup_local`
  test, `closures.rs:636`.) ⚠ **(pass-1 BLOCKING-1) the collector MUST DEDUP** — carry a `seen`
  name-set and skip an already-collected capture (mirror Rust `FreeVarCollector.seen`,
  `closures.rs:625/634/637`); else a body referencing a capture twice (`base + base + x`) pushes
  `base` into the capture vector TWICE → two env fields + two struct-ctor args → trips the
  intercept's `args.len() != sdef.fields.len()` guard (`lir_lower.gg:2333`, falls back to the
  miscompiling path) OR an off-by-one `GIFieldLoad` index = silent miscompile / false-MATCH.
  ⚠ **(pass-1 nit) this is a COLLECTOR (ordered deduped list + types), not the Phase-1 DETECTOR
  (returns bool)** — MIRROR the Phase-1 walk's STRUCTURE (pattern-binding tracking, body-local
  tracking, nested-closure-skip, EIf-branch coverage for c20) but it's net-new code returning
  `Vector[ClosureCapture]`, not a `reuse-verbatim`.
- **2a CLASSIFICATION GUARD (load-bearing — keeps 2a in scope):** 2a lifts a closure ONLY IF
  ALL of: (a) every capture's `type_id` is a bit-copyable PRIMITIVE — `BOOL_TYPE..F64_TYPE`,
  i.e. `type_id < UNIT_TYPE` (⚠ pass-1 nit: do NOT use `< PRIM_COUNT` — that includes
  `UNIT_TYPE=11`; a unit-typed capture is an upstream mis-type, exclude it); NOT a resource /
  String / Vector / struct / enum; AND (b) NO capture is mutated in the body (no
  assign/compound-assign — mirror Rust `detect_mutations` `:781`, block-bodies only); AND (c)
  **⚠ (pass-1 BLOCKING-2) the body contains NO NESTED closure** — see the post-pass note: the
  nested-closure counter-desync hazard does NOT dissolve for 2a, so a closure whose body contains
  an `EClosure`/`EImplicitClosure` stays STUBBED (retain the Phase-1 `expr_has_nested_closure`
  guard, `lower.gg:9406`). If a closure fails ANY of (a)/(b)/(c) — resource capture (→2b),
  mutated capture (→2c), or nested closure — 2a KEEPS THE STUB for that whole closure (as Phase 1
  does today) and does NOT lift it. So 2a lowers: non-capturing closures (Phase-1's wins) +
  closures whose every capture is a ByValue primitive AND which contain no nested closure.
  Everything else stays stubbed. (Conservative: when in doubt → stub.)
- **Register `__Closure_N`'s FIELDS at the MAKE-SITE** (⚠ pass-1 nit — NOT in the pre-pass):
  `gmod.type_infos["__Closure_N"]` gets one `GirFieldInfo(capture.name,
  type_name_of(capture.type_id))` per capture in capture order, written at the make-site (the
  only place captures are known), replacing the `{char __pad;}` placeholder (mirror the `IStruct`
  registration pattern at `lower.gg:10825-10831`). The pre-pass only `lookup_or_register_named`s
  the bare named type (`lower.gg:9668`) and never writes `type_infos` fields for it — so there's
  no clobber, but the executor must NOT add empty-field writes in the pre-pass. LIR Pass-2
  (`lir_lower.gg:883-894`) then fills `LirStructDef.fields` automatically and codegen stops
  emitting `char __pad`. NOTE: the make-site positive collector is now the SOLE capture-decision
  for lowering — the Phase-1 INVERTED detector (`closure_body_captures`, `:9383`) is no longer
  consulted for the lowering decision (verify it's unused elsewhere or remove it), resolving the
  two-detector / two-sources-of-truth smell pass-1 flagged.
- **★ DIVISION OF LABOR (pass-1 BLOCKING-2 — the brief's prior pre-pass/post-pass split was
  contradictory and risked a DUPLICATE `__Closure_N__call` symbol; resolved here, Rust-faithful):**
  - **Make-site (`lower_expr` EClosure arm, ctx live) = the SOLE id source + recorder + sig
    register + make-site-VALUE builder.** For EVERY closure: take `cid = gmod_next_closure_id`
    (this becomes the ONE counter — see below); run the deduped positive collector + the 2a guard;
    register the call-fn SIGNATURE in `fn_sigs`; push a `LiftedClosure{cid, captures, params,
    body, lowerable_flag}`. Build the make-site VALUE: **if 2a-lowerable** → a normal
    struct-constructor of `__Closure_N` with the capture sources as field args (2a primitives:
    `OpCopy(local_id)` per capture — bit-copy, NO CoW), assigned into the `GorgetClosure` slot
    (the step-3 LIR promotion packs it); **if STUBBED (resource/ByMutRef/nested) OR non-capturing**
    → KEEP the existing `GICallExtern(cdst, "__make_closure_N", [])` NULL-env path unchanged (so
    they do NOT regress — non-capturing bodies don't read env; stubbed stay WRONG as today).
  - **Post-pass (drains `gmod.lifted_closures`, mirror Rust `mod.rs:1429`) = the SOLE PUSHER of
    `__Closure_N__call` to `gmod.functions`.** For each record: **lowerable** → `lower_closure_body`
    with captures known — BEFORE lowering the body, per capture emit `GIFieldLoad(fdst,
    env_local=_1, capture_index)` (value-typed dst for 2a primitives) + `nl_put` capture name →
    fdst (mirror `emit_closure_call_function:383-427`); **stubbed** → push the stub
    (`[BasicBlock([], GTReturn(OpCopy(0)))]`, as Phase 1).
  - **Pre-pass (`scan_*_for_closures`, `lower.gg:9662`) — REMOVE its closure-FUNCTION-PUSH role**
    (the post-pass now owns the single push; the brief's earlier "pre-pass keeps the signature +
    post-pass pushes the body" would DOUBLE-push → duplicate symbol). ⚠ EXECUTOR: the pre-pass may
    have OTHER responsibilities (spawn-wrapper closure-var collection, `collect_closure_vars_*`,
    `lower.gg:9875-9937`) — KEEP those; only the `__Closure_N__call` push moves. Verify exactly
    what else the pre-pass does before removing its push.
  - **COUNTER UNIFICATION:** with the make-site as the sole id source, the pre-pass's separate
    local `closure_id` counter (`lower.gg:11736`) is removed along with its push role → ONE
    counter (`gmod_next_closure_id`, `lower.gg:3515`) → the dual-counter fragility DISSOLVES.
  - **NESTED CLOSURES — guard RETAINED for 2a (pass-1 BLOCKING-2):** the counter-desync does NOT
    dissolve for 2a. A nested `EClosure` inside a lifted body would, when the post-pass lowers
    that body, re-enter `lower_expr` → bump `gmod_next_closure_id` AND append to `lifted_closures`
    WHILE it is being drained. So 2a STUBS any closure whose body contains a nested closure
    (guard (c) above; retain `expr_has_nested_closure` `lower.gg:9406`) — the post-pass therefore
    never lowers a nested-containing body, no mid-drain re-entry. (2b/2c handle nested properly.)
  - ⚠ EXECUTOR + reviewers VERIFY the load-bearing assumption that `lower_expr`'s EClosure arm is
    reached for EVERY closure during `lower_function` (so the make-site sees them all and the
    pre-pass push is genuinely redundant). If some closure is NOT reached at the make-site, that
    one needs its record created where it IS seen.

### (3) `lir_lower.gg` — closure-pack promotion (the `try_closure_pack` equivalent)
When a `__Closure_N`-typed value is assigned into a `GorgetClosure` slot, emit
`__gorget_closure_env_alloc(sizeof(env))` (runtime, `runtime_string.c:139`) + `IMemcpy(heap_ptr,
&stack_env, size)` + `IClosurePack(slot, heap_ptr, call_func, false)`. Mirror Rust
`try_closure_pack` (`operands.rs:1233-1355`). All these LIR ops exist (`lir.gg`: `ISlotAddr`
`:107`, `IMemcpy` `:154`, `IStructInit` `:167`, `IClosurePack`/`ICallClosure` `:191/:197`); the
`IClosurePack`/`ICallClosure` codegen (`lir_codegen.gg:3390/3397`) is READY but currently dead.

### (4) `lir_codegen.gg` — the wart stays (transitional) in 2a; do NOT delete it yet
⚠ **(pass-1) the `__make_closure_` NULL-env branch (`:3707-3715`) + its return-type entry
(`:2472-2474`) must REMAIN in 2a** — the make-site still routes STUBBED + non-capturing closures
through `__make_closure_` (per the division of labor above), so deleting it now would break those.
2a does NOT touch `lir_codegen.gg`'s wart. The wart is fully deleted only when EVERY closure
class is lowered through the real struct path (end of Phase 2) — log that as the Phase-2-final
cleanup. (The env struct's real fields, field-store, and field-load codegen are already correct
and need no change — they activate automatically once `gmod.type_infos["__Closure_N"]` has
capture fields.) So 2a's only `lir_codegen.gg`/`lir_lower.gg` work is the step-3 LIR closure-pack
promotion; the C-emit codegen is untouched.

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
3. **MUST NOT regress Phase 1:** the 4 Phase-1 closure snapshots (`closure_block_tail_expr`,
   `closure_as_callback`, `closure_capture_loop_var`, `consume_callable_once`) must still MATCH
   — they now route through the re-architected post-pass. (The 5th closure snapshot
   `closure_in_spawn` is `spawn` of async fns — no lambda — unaffected; confirm it stays green.)
   `self_host_runtime` ≥260/0.
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
