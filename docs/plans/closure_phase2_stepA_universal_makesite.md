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
`loader.gg`, `src/`, `TODO`/`DONE`.
