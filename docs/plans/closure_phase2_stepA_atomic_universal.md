# Brief — closure Phase-2 STEP A (ATOMIC UNIVERSAL closure refactor; A1+A2+A3 in one diff)

FOUNDATIONAL REFACTOR, prerequisite for Step B (ByValue captures). Self-host-dir only:
**`tests/fixtures/self_host_lowerer/lower.gg` + `gir.gg`** (NO `lir_*`, NO `loader.gg`, NO `src/`).
Owner DECISION 2026-06-02: do the **full-universal refactor ATOMICALLY** (A1+A2+A3 together) —
the prior 3-way split is NOT independently shippable (empirically confirmed: a standalone "A1"
that removes the scan's closure-push regresses the inline-spawn canaries `spawn_closure_inline`
2→1 and `spawn_unchecked` 3→1, because those closures' `__Closure_N__call` is pushed by the very
scan arm A1 removes, while the make-site never sees them until A2; and the named-spawn `ncv_id`
replay is coupled to the same scan numbering). So all three land in one diff.

⚠ This brief is the SOLE coherent artifact — it supersedes the body+addendum of
`closure_phase2_stepA_universal_makesite.md` (kept for history). Needs ≥3 fresh sequential
reviews before the executor. Re-verified by RUNNING + instrumenting (two scouts, 2026-06-02).

---

## 1. The bug being fixed (empirically reproduced)
The self-host has TWO never-reconciled closure-id counters:
- **Make-site** counter `gmod_next_closure_id` (`lower.gg:3515`, a `__closure_counter` stored in
  `gmod.named_types`), bumped in `lower_expr`'s `EClosure` arm (`lower.gg:5651-5666`) — reached for
  EXPLICIT closures ONLY. `EImplicitClosure`/`EIt` fall to the `lower_expr` `else` (`:5826-5831`) →
  UNIT, never bumping it.
- **Pre-pass scan** counter `closure_id` (a local threaded as `next_id`, seeded `lower.gg:11736`),
  counting explicit + implicit-it + inline-spawn (all closures) in AST order — and it is what
  PUSHES the `__Closure_N__call` functions.

The two run at different times (the make-site runs DURING the `lower_function` loop; the scan runs
AFTER it, `lower.gg:11735-11745`, near the end of `lower_module`). They agree only by identical
traversal order — and they DON'T for mixed modules. Reproduced: `xs.map(it*2)` then
`auto f=(int x):x+1` → `__Closure_0__call` body is `it*2`, `__Closure_1__call` body is `x+1`, but
the explicit closure's value wires `.fn_ptr=__Closure_0__call` (the WRONG body). Per CLAUDE.md's
"fix-complexity = wrong layer", the desync is a symptom of the make-site not being the single id
source. Rust has ONE universal entry; we mirror that idea (single make-site id source + single
post-pass pusher) but with a **drain-until-empty worklist, NOT Rust's snapshot loop** (Rust CRASHES
on nested closures — `duplicate type name '__Closure_0'` — so it is NOT reference-grade there;
[[feedback-rust-not-sacrosanct]]).

## 2. Target architecture (after this refactor)
1. **Make-site (`lower_expr`) = the SOLE closure-id source + SOLE recorder.** Every closure class —
   explicit `EClosure`, implicit `EImplicitClosure`, and inline-spawn `spawn((): …)(args)` — takes
   its cid from `gmod_next_closure_id` and pushes a `LiftedClosure` record into `gmod.lifted_closures`.
   `EIt` is a value (resolve `it` to the live param), NOT a closure → no cid.
2. **Post-pass (after the `lower_function` loop, replacing the scan closure-walk) = the SOLE PUSHER
   of `__Closure_N__call`,** draining `gmod.lifted_closures` as a **worklist re-checking `.len()`
   each iteration (drain-until-empty)** — because lowering a lowerable body re-enters `lower_expr`
   and can APPEND a new record mid-drain (a `.map(it*2)` inside a lowerable body — the
   nested-closure guard does NOT see `EImplicitClosure`, so such a body IS lowered and its implicit
   closure is appended during the drain; a snapshot `for` would drop or crash on it).
3. **Scan closure-walk (`scan_stmts/stmt/expr_for_closures` + its `closure_id` counter) DELETED.**
   Its two jobs — pushing `__Closure_N__call` and pushing the inline-spawn `__spawn_wrap_` — both
   move (the call-fn to the post-pass via make-site records; the inline wrapper to the make-site
   ESpawn handling).
4. **Retained, UNTOUCHED (separate spawn concerns):** `collect_shared_vars` + `scan_spawn_wrappers`
   (shared-token / method-spawn wrappers, `lower.gg:11752-11776`).
5. **Named-closure-spawn pass RE-POINTED (NOT untouched):** `emit_named_closure_spawn_*` is kept but
   re-pointed to read make-site-recorded cids; `collect_closure_vars_*` + the `ncv_id` counter are
   DELETED (`lower.gg:11781-11790`). See §6 for the design — its cid replay is NOT consistent with
   the make-site after the refactor, so the cid is sourced from the make-site instead.

## 3. `gir.gg` — the lifted-closure record (append at END; in-memory IR → NO SCHEMA_VERSION bump)
- ⚠ **IMPORT: add `from ast import Stmt` to `gir.gg`** (its imports `gir.gg:4-11` do NOT include
  `ast`; `LiftedClosure` references `Vector[Stmt]` → unknown-type error without it). Safe — `ast.gg`
  imports only `std.collections`, so no import cycle. (`Param` is also needed only transitively via
  the helper, which lives in `lower.gg` where `ast` is already imported; the RECORD stores
  `body: Vector[Stmt]` + the already-resolved `int`/`String` sig fields, so only `Stmt` is new to
  `gir.gg`.)
- Add TWO new fields at the **END** of `GirModule` (after `strip_asserts`, `gir.gg:~428`; the struct
  documents "appended at END to avoid shifting positional ctor call sites"), in this order:
  `Vector[LiftedClosure] lifted_closures` then `Dict[String, String] closure_var_cids` (§6).
- Update the SINGLE ctor call (`lower.gg:~10968`, currently ends `…, !enum_variant_parent_idx,
  false)`) — append **BOTH** initializers: `, [], {}` (empty `lifted_closures`, empty
  `closure_var_cids`). (Verified there is exactly one `GirModule(...)` ctor call.)
- `LiftedClosure{int cid, String call_name, int self_ptr, Vector[int] abi_param_types,
  Vector[int] body_param_types, Vector[String] body_param_names, int return_type,
  Vector[Stmt] body, bool is_implicit, bool lowerable}`. (NO `captures` field — Step B adds it.)
  The fields map 1:1 onto `lower_closure_body`'s params (§4 helper produces them all):
  `abi_param_types` = the full ABI vector `[self_ptr, p0, p1, …]` (passed as `!abi_param_types`);
  `self_ptr` = the env-ptr type id; `body_param_types` = the closure's OWN param types
  `[p0, p1, …]` (i.e. `abi_param_types` minus the leading `self_ptr`, stored explicitly so the
  post-pass never re-slices/re-derives); `body_param_names` = the closure's param names (for
  implicit-it: `["it"]`). Store what `lower_closure_body` needs so the post-pass does NOT re-derive.

## 4. `lower.gg` — make-site (single id source + recorder + value builder)
**Extract a shared signature helper** so the make-site (and the deleted scan's logic) compute
identical metadata: `compute_closure_sig(int cid, Vector[Param] params, Vector[Stmt] body,
GirModule &gmod) -> (abi_param_types, self_ptr, body_param_types, body_param_names, ret_type)`. ⚠ The
helper MUST take `cid` — `self_ptr` is derived from the env type `__Closure_<cid>`: the scan does
`closure_name = "__Closure_" + int_to_str(cid)` (`:9666`) → `closure_tid =
lookup_or_register_named(&gmod, closure_name)` (`:9668`) → `self_ptr = register_ptr(&gmod,
closure_tid)` (`:9669`); without `cid` the helper cannot reproduce `self_ptr`. Lift the EXISTING scan
logic verbatim: env-type+self_ptr (`9666-9669`), param-type mapping `9671-9676`, return-type
inference `9678-9715` (incl. the float-param `9697-9700` and multi-stmt-void `9703-9712` heuristics +
STRING_MARKER resolve `9713-9715`). For implicit-it, the analogous `[I64_TYPE]`/`["it"]` param shape
+ `guess_return_type(body)` (`9644/9656-9658/9753/9767-9768`) — pass `is_implicit` or provide a
sibling helper. This guarantees the recorded sig == what the body-lowering expects.

### (a) `EClosure` arm (`lower.gg:5651-5666`) — KEEP value, ADD record
Keep the two existing emissions verbatim — `gmod.fn_sigs.put(make_fn, closure_tid)` (`:5664`) and
`emit(GICallExtern(cdst, "__make_closure_<cid>", []))` (`:5665`), returning the `GorgetClosure`
local. ADDITIONALLY: compute the sig via the helper, compute
`lowerable = NOT(closure_body_captures(params, body, &gmod) OR stmts_have_nested_closure(body))`
(the EXACT Phase-1 guard, `lower.gg:9727`), and push `LiftedClosure{cid, call_name=
"__Closure_<cid>__call", self_ptr, abi_param_types, body_param_types, body_param_names, ret_type,
body, is_implicit=false, lowerable}`.

### (b) NEW `EImplicitClosure(body_box)` arm (remove from the `else` at `:5827`)
Desugar to the implicit-it shape: synthetic single param `"it"` (type `I64_TYPE`), body =
`[SExpr(*body_box)]`. Take `cid = gmod_next_closure_id`, build the make-site value EXACTLY like the
EClosure arm (`gmod.fn_sigs.put` + `GICallExtern("__make_closure_<cid>", [])` → `GorgetClosure`
local), compute the implicit sig + `lowerable = NOT(implicit_closure_body_captures(*body_box, &gmod)
OR expr_has_nested_closure(*body_box))`, push `LiftedClosure{… is_implicit=true, lowerable}`, return
the value local. ⚠ This SUBSUMES `emit_it_closure` (`:9638`) — see §5(c).

### (c) NEW `EIt` arm (remove from the `else` at `:5827`) — value, NOT a closure
Mirror Rust `exprs/mod.rs:417`: `if nl_contains(&ctx, "it"): return nl_get(&ctx, "it")` else a UNIT
local (`add_local(UNIT_TYPE) + GIAssign(OpConstUnit())`). NO cid bump, NO record. (This is how `it`
resolves to the live implicit param when an implicit-it body is lowered in the post-pass.)

### (d) Inline-spawn at the make-site `ESpawn` arm (`lower.gg:5774`)
Today `ESpawn` is a bare `return lower_expr(*inner_box)` and inline-spawn `spawn(ECall(EClosure,
args))` is DROPPED (the ECall make-site arm `:4646` handles only `EIdentifier`/`EFieldAccess`;
EClosure callee → `else:pass` → UNIT). Change `ESpawn` to peek: if `inner_box` is
`ECall(callee=EClosure(params, body), args)`, then:
  - take `cid = gmod_next_closure_id`; compute sig + `lowerable` (same guard as (a)); push a
    `LiftedClosure{cid, …, is_implicit=false, lowerable}` (so the post-pass pushes its
    `__Closure_<cid>__call`).
  - push the inline-spawn wrapper stub `__spawn_wrap___Closure_<cid>` (the EXACT stub the scan
    emitted, `lower.gg:9818-9821`: empty params, one `GirLocal(UNIT_TYPE…)`, body
    `[BasicBlock([], GTReturn(OpConstUnit()))]`, UNIT return) — gate dedups via the shared
    `emitted_wrappers`? NO — the scan pushed it unconditionally; keep parity (push unconditionally
    here; the named-spawn dedup set is for the OTHER passes). ⚠ Verify against `spawn_unchecked`
    (TWO inline closures → `__spawn_wrap___Closure_0` AND `_1`, in source order).
  - lower the closure's `args` (so a capturing inline closure's args aren't silently dropped — match
    or improve on today, which drops them; fn-count is the gate so keep it ≥ current). Return a value
    local. (Real `__gorget_spawn_`/`__gorget_await_` wiring is OUT OF SCOPE — spawn parity is
    fn-count only today; do NOT regress the count, do NOT chase a runnable spawn binary.)
  Else (non-inline spawn, e.g. `spawn compute(3)`): keep the bare `return lower_expr(*inner_box)`.
  Apply the same to `ESpawnBlocking` only if it currently inline-spawns (it does not today — bare
  passthrough `:5776-5777`; leave it).
  ⚠ EMISSION-ORDER NOTE (harmless): today the scan interleaves inline fns `wrap_0, call_0, wrap_1,
  call_1`; after the refactor the make-site pushes wrappers during `lower_function` (`wrap_0,
  wrap_1`) and the post-pass pushes call-fns after (`call_0, call_1`) → order becomes `wrap_0,
  wrap_1, call_0, call_1`. fn-COUNT is unchanged and both comparison gates count fn-body openings
  order-INSENSITIVELY, so this is benign — do not be alarmed by the reordered diff; confirm the
  counts, not the order.

### (e) Named-closure varname→cid — see §6: record `name → "__Closure_<cid>"` into
`gmod.closure_var_cids` during `SVarDecl` lowering (a new make-site-side write, keyed by
`ctx.current_fn_name + "\0" + name`), read by the re-pointed `emit_named_closure_spawn_*`.

## 5. `lower.gg` — the post-pass (sole pusher) + the deletions
### (a) Post-pass — REPLACE the scan closure-walk (`lower.gg:11735-11745`) with a drain
```
int di = 0
while di < gmod.lifted_closures.len():         # re-read len() each iter = drain-until-empty
    LiftedClosure lc = gmod.lifted_closures.get(di).unwrap()
    if lc.lowerable:
        lower_closure_body(lc.call_name, !lc.abi_param_types, lc.self_ptr,
                           lc.body_param_types, lc.body_param_names, lc.return_type, lc.body, &gmod)
        # ^ lowering re-enters lower_expr → may APPEND new records past the current len; the while
        #   picks them up (worklist). lower_closure_body pushes the GirFunction itself (:9635).
    else:
        push the STUB GirFunction(lc.call_name, abi params, return_type, [ret-local + param-locals],
                                  [BasicBlock([], GTReturn(OpCopy(0)))])   # = the Phase-1 stub
    di = di + 1
```
⚠ The stub's locals must match the Phase-1 stub shape (`lower.gg:9729-9734` for explicit;
`9763-9765` for implicit-it — implicit-it stub has the extra `GirLocal(I64_TYPE, Some("it") …)`).
Use `lc.is_implicit` to pick the right stub local-set. (Folding both into `lower_closure_body` for
the stub case is cleaner if it doesn't change emitted shape — executor's call, but DON'T change the
stub's emitted locals/blocks.)

### (a′) ⚠ NESTED CLOSURES INSIDE A STUBBED OUTER — fn-count parity hazard (review-found)
The OLD scan recursed into a closure body (`scan_stmts_for_closures(body)`, `:9726`) and pushed
nested closures' `__Closure_M__call` **BEFORE** the outer's capture check (`:9727`) — so even a
STUBBED (capturing) outer still got its nested closures pushed. The new make-site records ONLY the
outer; nested closures are discovered solely by LOWERING the outer body in the drain. A LOWERABLE
outer lowers its body → nested recorded mid-drain ✓. But a STUBBED outer's body is NEVER lowered →
its nested closures are never recorded → their `__Closure_M__call` are **LOST** → per-fixture
fn-count DROP. (drain-until-empty covers nested-in-LOWERABLE, NOT nested-in-stub.)
- **PRIMARY (verify-it-is-empty): require the executor to PROVE no MATCHED fixture has a nested
  closure inside a stubbed (capturing OR nested-containing) outer** — i.e. the Phase-1 docstring
  claim (`lower.gg:~9404`: no current candidate nests closures) still holds. Evidence: per-fixture
  `user_fn_count` is unchanged across the whole corpus (the gate already measures this) AND a grep
  for closure-in-closure shapes finds none in MATCHED fixtures. Since Rust also crashes on nested
  closures, the corpus almost certainly has zero. If proven, the refactor is fn-count-preserving with
  NO extra code.
- **IF any such fixture exists (option a): the post-pass STUB case must record nested closures via a
  RECORD-ONLY walk** of `lc.body` — for each directly-nested `EClosure`/`EImplicitClosure`, assign a
  fresh `gmod_next_closure_id`, compute its sig + `lowerable`, push a `LiftedClosure` (drain picks it
  up); recurse the walk ONLY into NON-lowerable nested bodies (a lowerable nested will be LOWERED by
  the drain, which discovers ITS nested — recursing here too would double-record → duplicate symbol).
  This reproduces the OLD unconditional-nested-push. Implement only if PRIMARY fails.
- **LOG either way:** "nested-closure-inside-stubbed-outer" handling (option a) as a follow-up if not
  needed now. Flag in §7.

### (b) DELETE the scan closure-walk + counter
Delete `scan_expr_for_closures` / `scan_stmt_for_closures` / `scan_stmts_for_closures`
(`lower.gg:9662-9870`) AND the driver loop + counter (`lower.gg:11735-11745`, the `int closure_id =
0` block). All their roles moved (closure-push → post-pass; inline wrapper → §4(d)).
⚠ After deletion, update/remove now-stale docstrings on RETAINED helpers that reference the deleted
scan/`emit_it_closure` (e.g. `lower.gg:8461-8462`, `:9042`, `:9390`) — cleanliness, but do it so the
self-host reads correctly (CLAUDE.md self-host-as-showcase).
⚠ Before deleting, grep for any OTHER caller of these three functions (the named-spawn collector
`collect_closure_vars_*` is SEPARATE — confirm it does not call the scan). If something else calls
them, stop and re-scope.

### (c) DELETE `emit_it_closure` (`lower.gg:9638-9660`) — verified dead-for-common-path
`.map(it*2)` parses as `EImplicitClosure` (handled by the new make-site arm (b)); `expr_has_it`
(`:9006`) has no `EImplicitClosure` case. The new `EImplicitClosure` make-site arm subsumes it.
⚠ EXECUTOR: grep all remaining callers of `emit_it_closure` (today only the scan ECall/EMethodCall
arms at `:9782/:9789`, which are being deleted). If a live caller remains, the implicit-it path it
handled must be routed through the make-site arm instead. Also remove the now-dead `LowerCtx.
next_closure_id` field (`lower.gg:151`) ONLY if trivially safe (it is dead — never incremented; but
it touches all 4 `LowerCtx(...)` positional ctors at `:8543/:8737/:9608/:11434` → if that balloons
the diff, LOG it as a separate cleanup instead of bundling).

## 6. Named-closure-spawn pass — RE-POINT to make-site cids (the A3 concern)
The named-spawn pass (`collect_closure_vars_*` `:9875-9937` builds `Dict[varname→"__Closure_N"]`
— the `cmap.put` is in `collect_closure_vars_stmt` `:9907` — via
its own module-global `ncv_id` `:11781`; `emit_named_closure_spawn_expr` `:10445` is where the
`__spawn_wrap___Closure_N` push lives — dispatched into by `emit_named_closure_spawn_stmts` `:10477`
for `spawn f(...)`, dedup via the shared `emitted_wrappers`) currently re-derives each closure's cid
by REPLAYING `ncv_id` over the source `m.items` IFunction bodies.

⚠ **CORRECTION (do NOT assume consistency):** after the refactor the make-site assigns cids
**inside the `lower_function` loop** (`lower.gg:~11406`), which also lowers closures in **test
bodies, generic monomorphizations, trait-default methods (`~11597`), and equip methods** — a
SUPERSET of the scan's `m.items` IFunction/IEquip walk, in **monomorphization order, not source
order**. So `ncv_id` (re-deriving from 0 over source IFunction bodies only) is **NOT guaranteed** to
match the make-site cids; it only happens to agree for single-`main` fixtures with no
test/generic/equip closures preceding a named-spawn. (`collect_closure_vars_expr` `:9875-9899` also
has NARROWER expr coverage than the make-site — no EBinaryOp/EUnaryOp/EIf/EFieldAccess/EIndex/EFString
recursion — a second reason not to assume agreement.) The cid the wrapper needs MUST be the
closure's make-site cid (so `__spawn_wrap___Closure_N` ↔ the post-pass-pushed `__Closure_N__call`
agree).

- **PRIMARY (robust — matches the owner's "re-point to recorded cids" intent): record the cid at
  the make-site, read it at emit.** Add `Dict[String, String] closure_var_cids` to `gmod` as the
  SECOND new END field (after `lifted_closures`; key → `"__Closure_<cid>"`). When lowering a
  `SVarDecl(name, init)` whose `init` is an `EClosure`/`EImplicitClosure`, after `lower_expr(init)`
  returns, read the just-pushed record's cid
  (`gmod.lifted_closures.get(gmod.lifted_closures.len()-1).unwrap().cid`) and `put` it. ⚠ **SCOPING:
  key by the CURRENT FUNCTION + varname** — use `ctx.current_fn_name + "\0" + name` (the dialect's
  lexer supports `\0` but NOT `\x00`; an embedded NUL is a safe key since runtime Strings are
  length-prefixed and the Str-key Dict hashes/compares by `len`+`memcmp`). A global `varname→cid` map
  is WRONG (two functions each declaring `auto f = …` collide; the emit pass runs per-function AFTER
  all make-sites, so a plain overwrite map yields the LAST function's cid for every function's
  `spawn f()`). `LowerCtx.current_fn_name` EXISTS (`lower.gg:172`, set to `fdef.name` at
  `lower_function` `:8543`) and is reachable at the SVarDecl-lowering site (`:6177`/`:6226`). Then
  give `emit_named_closure_spawn_stmts` (`:10477`) / `_expr` (`:10445`) a `String fn_name` param
  (replacing the per-function `closure_vars` Dict param) and have it read
  `gmod.closure_var_cids.get(fn_name + "\0" + spawned_var)` (instead of `closure_vars.contains/get`
  on a bare varname). DELETE `collect_closure_vars_*` + the `ncv_id` counter; `:11781-11790` becomes:
  for each `IFunction(fdef)`, call `emit_named_closure_spawn_stmts(fdef.body, fdef.name, …)`
  (it no-ops if no matching key exists). (`collect_closure_vars_*` is called ONLY from this driver
  block — safe to delete.)
- **ALTERNATIVE (only if PRIMARY proves too invasive AND consistency is EMPIRICALLY proven): keep
  `collect_closure_vars_*` + `ncv_id`** — but ONLY after the executor demonstrates, with a
  CONSTRUCTED fixture placing a closure in a test/generic/equip body BEFORE a named-closure spawn,
  that the `ncv_id` cid still equals the make-site cid (per the correction above it likely will
  NOT). Do not choose this on the single-`main` canaries alone.
- **Either way the gate is the same:** `spawn_closure_void` / `spawn_closure_copy` keep their
  fn-count (3 / 4) and each `__spawn_wrap___Closure_N` matches its closure's `__Closure_N__call`.

## 7. Risks (ranked) + what each canary catches
1. **Inline-spawn fn-count parity (HIGHEST).** Canaries `spawn_closure_inline` (must stay 3 fns:
   `main` + `__spawn_wrap___Closure_0` + `__Closure_0__call`) and `spawn_unchecked` (5 fns: main +
   wrapper/call for `_0` AND `_1`). §4(d) + post-pass must reproduce these EXACTLY. ⚠ The two
   measured a moment ago as 3 and 5 (current self-host) — re-measure in-worktree; the gate is
   "no per-fixture drop" + aggregate `lowerer ≥ 953` / `c_emit ≥ 881`.
2. **Counter unification / make-site sees ALL classes the scan did.** If a closure class reaches the
   scan but NOT the make-site during `lower_function`, its `__Closure_N__call` is never recorded →
   dropped. The classes: explicit (SVarDecl init, call arg, method arg, return, …) — confirm
   `lower_expr` reaches the EClosure arm for ALL of them (method-call args go via `:5272-5277`).
   Inline-spawn handled by §4(d). Named closures are explicit EClosures → make-site sees them.
3. **drain-until-empty correctness.** `while di < gmod.lifted_closures.len()` (NOT a snapshot) — a
   mid-drain append (implicit-it inside a lowerable body) must be picked up; verify `len()` is
   re-read each iteration and `di` advances monotonically (no infinite loop — each record is lowered
   once; lowering appends only strictly-new records with fresh cids).
4. **Named-spawn numbering (§6)** — caught by `spawn_closure_void`/`spawn_closure_copy`.
5. **emit_it_closure deadness (§5c)** — grep before delete.
6. **The make-site EClosure arm's `lowerable` guard must be byte-identical to the scan's** (`:9727`
   for explicit, `:9651/:9762` for implicit) so the SAME closures stub vs lower as Phase 1 → parity
   neutral except the desync fix.
7. **Nested-closure-inside-a-STUBBED-outer (§5a′) — fn-count DROP class.** The old scan pushed
   nested call-fns unconditionally (before the capture check); the new design discovers nested only
   by lowering the outer body, so a stubbed outer loses its nested. Mitigated by §5a′ PRIMARY (prove
   the corpus has none — per-fixture `user_fn_count` unchanged + grep). Caught by the per-fixture
   fn-count gate. If it triggers, implement §5a′ option (a).
8. **§4(b) is NOT purely behavior-preserving for `.map(it*2)`/`.filter(it…)` (acknowledge, don't be
   alarmed):** the new EImplicitClosure make-site arm emits a `__make_closure_<cid>` value where the
   old `else` emitted only a UNIT local → the implicit-it arg's local changes UNIT→GorgetClosure and
   a `call_extern @__make_closure_N` appears. This is **fn-count-NEUTRAL** (`__make_closure_N`
   expands inline at `lir_codegen.gg:3708-3715`, not a fn body; `user_fn_count` counts `) {`
   openings), and the affected implicit-it fixtures CC-FAIL today, so it cannot regress a runnable
   binary. Expect C-emit diffs on `.map`/`.filter` fixtures — confirm fn-counts, not byte-identity.

## 8. Validation gate (self-host-dir only; behavior-preserving refactor + latent desync fix)
⚠ `bootstrap_fixed_point` is a REGRESSION GUARD only (the driver sources have ZERO
lambdas/implicit-it/spawn — it can't validate closure behavior; it must stay GREEN to prove the
re-arch didn't break the closure-free driver path). Real validation = fn-count canaries + runtime
diff + the mixed C-emit wiring check.
1. `cargo build` + `cargo test --lib` green (1066/0 expected).
2. **Force-rebuild the driver** (`rm tests/fixtures/self_host_lowerer/driver{,.c}`; the test
   rebuilds) before EVERY comparison/diff run — the OnceLock-cached driver can be stale and segfault
   all fixtures (false 0-matched).
3. `lowerer_comparison` ≥ **953** matched, `c_emit_comparison` ≥ **881** matched (RE-CONFIRM the
   baseline from `--nocapture` first; both must be unchanged-or-better). Per-canary check:
   `spawn_closure_inline` (3), `spawn_unchecked` (5), `spawn_closure_void` (3), `spawn_closure_copy`
   (4), `spawn_closure_shared` (3) — none drops.
4. **The 5 Phase-1 closure snapshots MUST still MATCH** (`closure_block_tail_expr`,
   `closure_as_callback`, `closure_capture_loop_var`, `consume_callable_once`, `closure_in_spawn`).
   `self_host_runtime` ≥ **260/0** (lock-in net).
5. **`GG_RUNTIME_DIFF=1 … self_host_runtime_diff`** — parity MATCH ≥ **261**, and NO fixture moves
   MATCH→worse (this refactor is latent/behavior-preserving for everything except the mixed desync,
   which currently CC-FAILs anyway). The implicit-it/spawn fixtures mostly CC-FAIL on an adjacent
   `it`-closure-call ABI gap (Phase 1.5) — do NOT chase a green run that can't exist yet.
6. **NEW mixed C-emit WIRING check (proof of the desync fix, do NOT snapshot — can't run yet):** a
   fixture `xs.map(it*2)` THEN `auto f=(int x):x+1; print(f(5))`; emit-C and CONFIRM the explicit
   closure's `__make_closure_<N>` value points at ITS OWN `__Closure_<N>__call` (body `x+1`), not
   the implicit-it body. Document as the proof. ⚠ This new fixture raises the comparison Total by 1
   (re-confirm the exact Total from `--nocapture`); it is a normal program (`main` + 2 closure-call
   fns) so it must itself MATCH on fn-count in BOTH comparisons (+1 to matched), keeping the totals
   ≥953/≥881. Confirm it MATCHes (not mismatches) after adding it.
7. `bootstrap_fixed_point` GREEN.

## 9. Files (stage by name only — never `-a`)
`tests/fixtures/self_host_lowerer/lower.gg` + `tests/fixtures/self_host_lowerer/gir.gg` + the NEW
mixed-wiring fixture under `tests/fixtures/` (do NOT snapshot it). Do NOT touch `lir_*.gg`,
`loader.gg`, `src/`, `TODO.md`/`DONE.md`.

## 10. Out of scope (LOG, do NOT do here)
- **Step B** = ByValue-PRIMITIVE captures on this unified ground (`docs/plans/
  closure_phase2a_byvalue_primitives.md`; re-scope "assumes Step A landed"): add `captures` to
  `LiftedClosure`, positive deduped collector at the make-site, `__Closure_N` env-struct fields,
  `GIFieldLoad` in the post-pass body, LIR closure-pack promotion, delete the `__make_closure_`
  NULL-env wart.
- Phase 1.5: the `it`-closure-call ABI gap (`lir_codegen.gg:3672` casts → `(void)x*(void)y`) that
  blocks every implicit-it RUN. Real spawn `__gorget_spawn_`/`__gorget_await_` wiring. 2b/2c.
- The `LowerCtx.next_closure_id` dead-field removal if it balloons the diff (§5c).
