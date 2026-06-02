# Brief — closure Phase-2 STEP A: universal make-site + counter unification (refactor, NO captures)

FOUNDATIONAL REFACTOR (prerequisite for Step B = ByValue captures). Self-host-dir only,
**`lower.gg` + `gir.gg`** (NO `lir_*`). Owner chose "make make-site universal" (2026-06-02).
Re-verified by RUNNING + instrumenting (scout, 2026-06-02). ⚠ Needs ≥3 fresh sequential reviews
before the executor — this is a re-architecture of the closure-id/push sequencing.

## Why (the bug Step A fixes — empirically confirmed)
The self-host has TWO closure-id counters that disagree:
- **Make-site** `lower_expr` EClosure arm (`lower.gg:5651-5666`) uses `gmod_next_closure_id`
  (`lower.gg:3515`), reached for EXPLICIT closures ONLY. `EImplicitClosure`/`EIt` fall into the
  `lower_expr` `else` fallback (`lower.gg:5826-5831`) → UNIT, never advance the counter.
- **Pre-pass** `scan_*_for_closures` uses its OWN local counter (`int closure_id = 0`,
  `lower.gg:11736`), counting ALL closures (explicit + implicit-it) in AST order, and is the one
  that PUSHES the `__Closure_N__call` functions.

⇒ In a MIXED module (`xs.map(it*2)` THEN `auto f=(int x): x+1`), the make-site gives the
explicit closure `cid=0` (it never saw the implicit-it), but the pre-pass numbers implicit-it=0,
explicit=1. **Scout-proven in emitted C** (`mixed.c`): the explicit closure's value wires
`.fn_ptr=__Closure_0__call`, but `__Closure_0__call`'s BODY is the implicit-it `it*2` — the
explicit `f(5)` dispatches the WRONG body. (This particular fixture also CC-FAILs on an adjacent
`it`-closure-call ABI gap, so the wrong value never reaches a binary today — but the mis-wiring
is unambiguous and is purely the numbering disagreement; pure-explicit `two_explicit` runs
correctly, proving the split is the cause.)

Per CLAUDE.md's "fix-complexity = wrong layer": the desync is a SYMPTOM of the make-site not
seeing all closure classes. **Rust has ONE universal entry** — `Expr::Closure`,
`Expr::ImplicitClosure` (desugars to a `Closure` with a synthetic `it` param), `Expr::It`, and
spawn-inline ALL route through `lower_closure` (`src/ir/lowering/exprs/mod.rs:378/952-963/417`,
`closures.rs:79`), which is the SOLE id source + recorder + value-builder; a single post-pass
(`mod.rs:1428-1432`) is the SOLE pusher; Rust has NO closure-counting pre-pass. **Reference-grade
= mirror Rust** (the two-counter split is scar tissue; the self-host even documents the desync
hazard at `lower.gg:9396-9405` but only guards the nested sub-case).

## The Step-A refactor (mirror Rust's universal lowering; NO captures)
### (1) `gir.gg` — the lifted-closure record (append at END per the positional-ctor convention)
- Add `Vector[LiftedClosure] lifted_closures` to `GirModule` (END of the struct, `gir.gg:~419/426`
  convention; single ctor update at `lower.gg:~10968`). `GirModule` is in-memory IR → NO
  `SCHEMA_VERSION` bump.
- `LiftedClosure{int cid, String call_name, Vector[String] param_names, Vector[int] param_types,
  int return_type, Vector[Stmt] body, bool is_implicit, bool lowerable}`. (NO `captures` field yet
  — Step B adds it.)

### (2) `lower.gg` — universal make-site (`lower_expr` sees ALL closure classes)
- Add an **`EIt`** arm to `lower_expr` (remove `EIt` from the `else` list at `:5827`): mirror
  Rust `exprs/mod.rs:417` — `if nl_contains(&ctx, "it"): return nl_get(&ctx, "it")` else a UNIT
  local.
- Add an **`EImplicitClosure(body_box)`** arm (remove from the `else` list): desugar to the
  implicit-it closure shape (synthetic `"it"` param, body = `*body_box`) and do the SAME make-site
  work as the EClosure arm — `cid = gmod_next_closure_id(&gmod)`, build the make-site VALUE (the
  UNCHANGED `GICallExtern(cdst, "__make_closure_<cid>", [])` NULL-env path — Step A has NO captures,
  NO env struct), record a `LiftedClosure{cid, …, is_implicit=true, lowerable=<Phase-1 guard>}`,
  return `cdst`. This SUBSUMES `emit_it_closure`'s value-building; its body-lowering moves to the
  post-pass.
- The **`EClosure`** arm (`:5651`) ADDITIONALLY records a `LiftedClosure` (today it only emits the
  value; keep the value emission, add the record). `lowerable` = the Phase-1 guard result
  (non-capturing/2a-eligible vs stub — Step A PRESERVES Phase-1's stub-the-capturers behavior
  exactly; the ONLY behavior change is the unified numbering).

### (3) `lower.gg` — single post-pass = SOLE pusher
- Add a post-pass AFTER all `lower_function` calls (mirror Rust `mod.rs:1428-1432`) that drains
  `gmod.lifted_closures`: for each record, if `lowerable` → call `lower_closure_body(...)`
  (`lower.gg:9598`, self-contained, reusable AS-IS — it already takes the body since Phase 1 and
  pushes the `GirFunction`); else → push the stub (`[BasicBlock([], GTReturn(OpCopy(0)))]`). For
  implicit-it records, inline `emit_it_closure`'s body-lowering logic (it's `lower_closure_body`
  with the `"it"` param).

### (4) `lower.gg` — REMOVE the pre-pass closure-fn-push + its counter
- Remove the `__Closure_N__call` PUSH sites in `scan_*_for_closures` (`:9664` EClosure, `:9744`
  EImplicitClosure, `:9638` `emit_it_closure`) and the local `closure_id` counter
  (`:11736-11743`). The post-pass owns the single push; the make-site owns the single id.
- ⚠ **KEEP the pre-pass's NON-push roles:** `collect_closure_vars_*` (`:9875-9937`, driven
  `:11781-11790`), `collect_shared_vars`/`scan_spawn_wrappers` (`:11752-11776`), and the ESpawn
  inline-closure `__spawn_wrap_` emission (`:9803-9822`). These are spawn-wrapper concerns, NOT
  closure-fn pushes.
- ⚠⚠ **RISKIEST (scout-flagged) — the spawn-wrapper naming depends on the removed counter.** The
  ESpawn `__spawn_wrap___Closure_N` name (`lower.gg:9816-9817`) is built from the pre-pass
  `closure_id`. After Step A removes that counter, the spawn-wrapper MUST source its `N` from the
  make-site id (`gmod_next_closure_id` / the recorded `cid`) instead — ELSE `closure_in_spawn` (a
  committed snapshot) regresses. This is the sharpest executor check; verify `closure_in_spawn`
  emits the same C + MATCHes after the change.

### (5) Result
The make-site sees ALL closure classes (explicit, implicit-it, spawn-inline via
ESpawn→ECall→EClosure recursion, method-call args via `lower.gg:5272-5277`), assigns ids from ONE
counter, and the post-pass is the sole pusher. `__make_closure_N` ↔ `__Closure_N__call` agree by
construction. The mixed-desync dissolves.

## ⚠ Scope / what NOT to do in Step A
- NO captures, NO `ClosureCapture`, NO positive free-var collector, NO env-struct fields, NO
  `GIFieldLoad`, NO `lir_lower.gg`/`lir_codegen.gg` changes. The `__make_closure_N` NULL-env path
  (`lir_codegen.gg:3707-3715`) STAYS unchanged. (All of that is Step B.)
- PRESERVE Phase-1's guards (`closure_body_captures`/`implicit_closure_body_captures`/
  `expr_has_nested_closure`) and their stub-the-capturers behavior — Step A only unifies the
  numbering + the push, it does NOT change which closures are lowered-vs-stubbed.

## Validation gate (self-host-dir only; behavior-PRESERVING refactor + a latent bug-fix)
⚠ **`bootstrap_fixed_point` is a REGRESSION GUARD, not the signal** (driver sources have ZERO
lambdas/implicit-it — scout-verified — so it can't validate closure behavior; it must stay GREEN
to prove the re-arch didn't break the closure-free driver path).
1. `cargo build` + `cargo test --lib` → 1066/0.
2. **The 5 Phase-1 closure snapshots MUST still MATCH** (`closure_block_tail_expr`,
   `closure_as_callback`, `closure_capture_loop_var`, `consume_callable_once`, **`closure_in_spawn`**
   — the spawn one is the canary for risk (5)). `self_host_runtime` ≥260/0.
3. **NEW mixed fixture — a C-EMIT WIRING check, NOT a runnable binary** (⚠ scout: the implicit-it
   `.map` path CC-FAILs on an adjacent `it`-closure-call ABI gap until Phase 1.5/2a, so NO
   implicit-it fixture can RUN-match regardless of Step A — do NOT chase a green run). Add a
   fixture `xs.map(it*2)` THEN `auto f=(int x):x+1; print(f(5))`; emit-C and CONFIRM the explicit
   closure's `__make_closure_<N>` value points at ITS OWN `__Closure_<N>__call` (whose body is
   `x+1`), not the implicit-it body. Document this as the proof; do NOT snapshot it (it can't run
   yet).
4. **Implicit-it fixtures must STAY CC-FAIL** (not false-match, not collapse, not driver-crash):
   `implicit_it`, `closure_compose`, `closure_higher_order_chain`, `option_map`, `result_map`,
   `iterator_adapters`, etc. (the `.map(it`/`.filter(it` set). Re-running the diagnostic, NONE
   should move MATCH→worse.
5. `c_emit_comparison` (RE-CONFIRM the baseline from `--nocapture`, don't trust the figure) /
   `lowerer_comparison` (952) unchanged-or-better.
6. `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` — parity unchanged (Step A is latent/behavior-
   preserving; no fixture MATCH→worse).

## Follow-ups to LOG (out of scope)
- Step B (ByValue-primitive captures on the unified make-site: add `captures` to `LiftedClosure`,
  positive deduped collector, env-struct fields, `GIFieldLoad`, LIR closure-pack promotion, the
  `__make_closure_` wart deletion). The existing brief `docs/plans/closure_phase2a_byvalue_
  primitives.md` becomes the Step-B brief (re-scope: "assumes Step A landed").
- Phase 1.5: the `it`-closure-call ABI gap (`lir_codegen.gg:3672` casts → `(void)x*(void)y`,
  `void value not ignored`) that blocks every implicit-it RUN — needed before implicit-it
  fixtures can MATCH.
- 2b (resource/CoW captures), 2c (ByMutRef).

## Files (stage by name only — never `-a`)
`tests/fixtures/self_host_lowerer/lower.gg` + `tests/fixtures/self_host_lowerer/gir.gg` + the NEW
mixed C-emit-wiring fixture under `tests/fixtures/` (do NOT snapshot it). Do NOT touch `lir_*.gg`,
`loader.gg`, `src/`, `TODO`/`DONE`. (A2/A3 below also stay in `lower.gg`.)

---

## ⚠ FULL-UNIVERSAL GROUNDING (scout 2026-06-02 — owner chose "full universal" after Step-A pass-1 found the make-site doesn't reach 3 closure classes; this section EXPANDS/CORRECTS the body above where they differ. ⚠ NEEDS ≥3 fresh reviews of the EXPANDED brief before any executor.)

**Re-decompose Step A into THREE executors (the full-universal is too much for one safe diff):**

- **A1 — universal VALUE make-site + drain-until-empty (fixes the desync; `lower.gg`+`gir.gg`).** Add `EClosure`-record + `EImplicitClosure`(synthetic `"it"` param) + `EIt`(local lookup of `"it"`, else UNIT) arms to `lower_expr` (remove EIt/EImplicitClosure from the `else` at `:5826`); add the post-pass (after the `lower_function` loop, ~`:11735`) that DRAINS `gmod.lifted_closures` **as a worklist re-checking length each iteration (drain-until-empty), NOT a fixed snapshot** — because `lower_closure_body`→`lower_block_expr`→`lower_expr` hits a NESTED `EClosure` arm and APPENDS a new `LiftedClosure` mid-drain (a snapshot `for` would drop the inner `__Closure_N__call` or crash). Remove the pre-pass `__Closure_N__call` push sites (`:9664`/`:9744`/`emit_it_closure :9638`) + the pre-pass `closure_id` counter (`:11736`). Preserve the `lowerable = NOT(closure_body_captures OR stmts_have_nested_closure)` guard (`:9727`) so a nested-containing outer is still STUBBED. **A1 alone fixes Fact #4 (the mixed desync).**
- **A2 — inline-spawn Case A/B (`lower.gg`).** Today `spawn ((): body)(args)` is DROPPED at the ECall arm's `else:pass` (`:4709-4711`, callee match only handles `EIdentifier`/`EFieldAccess`) — the `__Closure_N__call`+`__spawn_wrap_` come entirely from the pre-pass. Add an `EClosure`-callee case (ideally detect inline-spawn in the ESpawn arm `:5774` by peeking `inner=ECall(EClosure,…)`, so a bare non-spawn IIFE isn't mis-treated): record the `LiftedClosure` via the unified counter, build the spawn wrapper `__spawn_wrap___Closure_<cid>` from the RECORDED cid, emit the `__gorget_spawn_` call. Remove the pre-pass ESpawn wrapper-push (`:9803-9822`). Mirror Rust `exprs/mod.rs:1057-1091` (Case B) + `spawn.rs:367-387` (`__spawn_wrap_{struct_name}`, NO separate spawn counter).
- **A3 — named-closure-spawn re-pointing (`lower.gg`).** `collect_closure_vars_stmts` (`:9934`, driven `:11781-11790`) replays a SEPARATE `ncv_id` counter to build `Dict[varname → __Closure_N]`, consumed by `emit_named_closure_spawn_stmts` (`:10477`); its docstring (`:9873`) demands "visit in the SAME ORDER as scan_stmts_for_closures." Replace: when the make-site records a `LiftedClosure` for a `SVarDecl(name, EClosure…)`, also record `varname → __Closure_<cid>` on `gmod`; `emit_named_closure_spawn_stmts` reads that map (removes the same-order fragility). Mirror Rust Case A (`exprs/mod.rs:1036-1052`, keys off the local's closure type).

**⚠⚠ REFERENCE-GRADE CORRECTION (the load-bearing finding — [[feedback-rust-not-sacrosanct]] in action):** the post-pass drain is at Rust `src/ir/lowering/mod.rs:1427-1432` (NOT `exprs/mod.rs`), and it is a **SNAPSHOT `for`, which CRASHES Rust on nested closures** (`duplicate type name '__Closure_0'`, panic `mod.rs:1607` — the nested closure appends to the freshly-`mem::take`n `ctx.closures` with a reset `next_id`). **Rust is NOT reference-grade for nested closures.** The self-host TODAY handles nested correctly (pre-pass recurse + stub-guard). So MIRROR Rust for the universal single-counter make-site + single post-pass pusher (explicit/implicit-it/inline-spawn), but for nested closures use **drain-until-empty (a worklist)** — do NOT mirror Rust's snapshot loop. Step A must NOT regress to Rust's crash.

**Fact corrections to the body above:** (Fact #4) the `.map(it)` inline→`__make_closure_N` change is **fn-count-NEUTRAL** (only an extra `call_extern` instruction inside the caller; the `__Closure_N__call` set is unchanged) — confirmed empirically. (Fact #2) nested-closure fixtures: corpus scan found ZERO true closure-in-closure-body fixtures, so nested is robustness/future-proofing (don't regress), not a current gate mover.

**GATE corrections (re-measured this session at tip `7c91ec75`):** `lowerer_comparison` = **953** (body's "952" is stale-low by 1); `c_emit_comparison` = **881**; both must stay unchanged-or-better (the naive plan would REGRESS them by dropping nested/inline-spawn `__Closure_N__call`). The real fn-count canaries: `spawn_closure_inline` (3 fns), `spawn_unchecked` (5, TWO inline closures → `__spawn_wrap___Closure_0` AND `_1` in order), `spawn_closure_void` (3), `spawn_closure_copy` (4), `spawn_closure_shared` (3), `shared_closure_inline_error` (rust=0/ErrorOnly). ⚠ **`closure_in_spawn` is NOT a closure canary** (it's a named-async-fn spawn `spawn compute(3)` — zero `__Closure_N`); the body above mis-named it. Implicit-it set (`closure_compose`/`closure_higher_order_chain`/`closure_iife`/`closure_partial_application`/`closure_returning_closure`/`test_multiline_closures`) all MATCH today (none truly nested) — must stay MATCH.

**Smells to clean during A1 (scout-found):** (a) `LowerCtx.next_closure_id` (`lower.gg:151`) is DEAD — the real counter is `gmod.named_types["__closure_counter"]` via `gmod_next_closure_id` (`:3515`); remove or wire it. (b) `emit_it_closure` (`:9638`) is effectively DEAD for the common path (`.map(it*2)` parses as `EImplicitClosure`; `expr_has_it` `:9006` has no `EImplicitClosure` arm → routes to the `:9744` scan arm, not `emit_it_closure`) — the `EImplicitClosure` make-site arm should subsume it; likely delete it.

**Riskiest:** A2's spawn-wrapper cid must match what the pre-pass currently produces, AND the cross-A1+A2 ordering (a module with both top-level and inline-spawn closures) must be re-verified; A1's drain order must reproduce the pre-pass AST-order numbering so `__make_closure_N ↔ __Closure_N__call` agree.
