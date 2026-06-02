# Brief — closure Phase-2 STEP B: ByValue PRIMITIVE captures (on the Step-A unified ground)

ASSUMES STEP A LANDED (gorget-1 `231abb78`). Self-host-dir only. Touches
`tests/fixtures/self_host_lowerer/{lower.gg, gir.gg, lir_lower.gg, lir_codegen.gg}` (NOT disjoint —
this is a cross-layer re-architecture; no parallel chain alongside it). NO `loader.gg`, NO `src/`,
NO other self_host_* dir. Re-grounded against `231abb78` by a fresh scout (2026-06-02) — current
line numbers below are post-Step-A. ⚠ Needs ≥3 fresh sequential reviews before the executor —
expect blocking findings on the LIR closure-pack promotion (the highest-risk, least-existing piece).

⚠ Per [[feedback-rust-not-sacrosanct]]: Rust's two-layer closure-pack split (GIR builds a stack env
struct via the generic `StructInit`; LIR promotes it to a heap env via `try_closure_pack`) was
judged reference-grade by the Step-A-era scouts (NO scar tissue) — mirror it. But verify before
copying any specific line.

## 0. What Step A already built (do NOT redo)
- `gir.gg:330` `LiftedClosure{int cid, String call_name, int self_ptr, Vector[int] abi_param_types,
  Vector[int] body_param_types, Vector[String] body_param_names, int return_type, Vector[Stmt] body,
  bool is_implicit, bool lowerable}` (NO `captures` yet — Step B adds it at END).
- `gir.gg:458/468` `GirModule.lifted_closures` + `.closure_var_cids`.
- `lower.gg:5689-5713` make-site EClosure arm: emits the value (`__make_closure_<cid>` NULL-env,
  `fn_sigs.put` + `GICallExtern`), computes the sig via `compute_closure_sig` (`:9705-9754`, which
  registers the `__Closure_<cid>` named type + `*__Closure_<cid>` self_ptr but writes NO
  `type_infos` fields), and records the `LiftedClosure` with `lowerable = not (closure_body_captures
  OR stmts_have_nested_closure)`.
- `lower.gg:5912/5917` EImplicitClosure / EIt arms; `lower.gg:11670-11701` the drain-until-empty
  post-pass (sole pusher; lowerable → `lower_closure_body` `:9820`, else → Phase-1 stub).
- `lir_lower.gg:2309` user-struct-ctor INTERCEPT (`try_lower_user_struct_ctor` → `IStructInit`
  `:2355`; guard `args.len() != sdef.fields.len()` `:2333`; called `:2771`); `lir_lower.gg:883-966`
  LIR Pass-2 fills `LirStructDef.fields` from `gmod.type_infos`; `lir_lower.gg:3324` `GIFieldLoad`
  lowering (LIVE — fix the stale `gir.gg:127` "DEAD CODE" comment). `gir.gg:129`
  `GIFieldLoad(dst, base_local, field_index)`.
- `lir_codegen.gg:3707-3715` the `__make_closure_` NULL-env wart (+ return-type entry `:2472-2474`,
  the `name.starts_with("__make_closure_")` name-match `:3714`); `lir_codegen.gg:3390-3395`
  `IClosurePack` codegen (READY, currently dead); `:3397+` `ICallClosure` codegen.
- `lir.gg`: `ISlotAddr:107`, `IMemcpy:154`, `IStructInit:167`, `IClosurePack:191`, `ICallClosure:197`.
- Type constants (`gir.gg:16-28`): `BOOL_TYPE=0` … `F64_TYPE=10`, `UNIT_TYPE=11`, `PRIM_COUNT=12`.

## 1. Goal + scope of 2a
Make **ByValue PRIMITIVE captures** work: a closure capturing only bit-copyable primitives
(`BOOL_TYPE..F64_TYPE`, i.e. `type_id < UNIT_TYPE`), not mutated, not nesting a closure, gets a REAL
env struct + heap pack + field-loads in its body. Unblocks `auto_types` (`int base`),
`test_if_expressions` c20 (4 int captures), `test_closures_edge_cases` primitive cases, `closures.gg`
primitive cases. **OUT OF SCOPE (KEEP STUBBED, log):** ByValue RESOURCE captures (String/Vector/
struct → 2b), ByMutRef/mutated captures (→ 2c), the String-PARAM closure-call ABI gap (Phase 1.5).
⚠ Some targets (e.g. `auto_types`) may have a SECOND unrelated root (const-auto reads 0); only
snapshot a fixture that ACTUALLY reaches MATCH — do not assume captures alone fix it.

## 2. Architecture (Rust two-layer, mirror it)
GIR: the make-site builds a NORMAL struct-constructor of `__Closure_N` (capture sources as field
args) → the existing user-struct-ctor intercept (`lir_lower.gg:2309`) turns it into `IStructInit`
(a STACK env) → assigning that `__Closure_N` value into the `GorgetClosure` closure slot triggers a
NEW LIR closure-pack promotion (`try_closure_pack` equivalent) = `__gorget_closure_env_alloc(size)` +
`IMemcpy(heap, &stack_env, size)` + `IClosurePack(slot, heap, call_fn, false)`. The body reads each
capture via `GIFieldLoad(fdst, env_ptr=_1, field_index)`. There is **NO new GIR/LIR op** — every op
exists; the work is (a) collect captures, (b) register env fields, (c) build the struct-ctor value at
the make-site for capturing-lowerable closures, (d) field-load captures in the post-pass body, (e)
add the LIR closure-pack promotion.

## 3. `gir.gg` — typed capture record (append at END of the positional ctor)
- Add `Vector[ClosureCapture] captures` as the LAST field of `LiftedClosure` (`:330`). Update BOTH
  push sites in `lower.gg` (the EClosure record `:5712` + the implicit-it record in
  `record_implicit_it_closure` `~:9790`) to pass the captures vector (empty `[]` for implicit-it in
  2a — implicit-it capture support is deferred; implicit-it stays stubbed-if-capturing as today).
- `ClosureCapture{String name, int type_id, int local_id, int mode}` — `mode`: int const
  `CAP_BY_VALUE = 0` (2a). Reserve `CAP_BY_MUT_REF = 1` for 2c (do NOT implement). The capture VECTOR
  ORDER is the SINGLE source of truth: env-field order == struct-ctor arg order == body `GIFieldLoad`
  index order.
- Fix the STALE `gir.gg:127` comment ("GIFieldLoad … DEAD CODE") — it is LIVE (`lir_lower.gg:3324`);
  2a makes it an emit site.

## 4. `lower.gg` — collector, classification, env-field registration, make-site value, post-pass body
### (a) Positive deduped free-var COLLECTOR (new; at the make-site EClosure arm `:5689`, ctx live)
Walk the closure body; a bare `EIdentifier(name)` that is NOT a closure param / `it` / body-local
(SVarDecl / for-pattern / match-arm pattern — track with the SAME lexical-scope discipline the
Phase-1 INVERTED guard `closure_body_captures` uses) AND for which `nl_contains(&ctx, name)` is TRUE
is a CAPTURE → record `ClosureCapture(name, type_id = ctx.locals.get(nl_get(&ctx,name)).unwrap().
type_id, local_id = nl_get(&ctx,name), CAP_BY_VALUE)`. ⚠ **MUST DEDUP** (carry a `seen` name-set;
mirror Rust `FreeVarCollector.seen`) — a body referencing a capture twice (`base + base`) must push
`base` ONCE, else two env fields + two ctor args trip the intercept's `args.len() !=
sdef.fields.len()` guard (`lir_lower.gg:2333`, falls back to a miscompile) OR an off-by-one
`GIFieldLoad` index = silent miscompile / false-MATCH. ⚠ This is a COLLECTOR (ordered deduped list +
types), NOT the Phase-1 DETECTOR (returns bool) — MIRROR the Phase-1 walk's STRUCTURE (pattern-
binding, body-local, nested-skip, EIf-branch coverage for c20) but it's net-new code returning
`Vector[ClosureCapture]`. (`ESelfExpr` → treat as a capture only if a fixture needs it; 2a targets
don't — log otherwise.)

### (b) 2a CLASSIFICATION GUARD (load-bearing — keeps 2a in scope; REPLACES the Phase-1 lowerable test)
2a lifts a closure ONLY IF ALL of: (a) EVERY capture's `type_id < UNIT_TYPE` (=11) — bit-copyable
primitive; ⚠ NOT `< PRIM_COUNT` (=12, includes UNIT — a unit-typed capture is an upstream mis-type,
exclude it); NOT resource/String/Vector/struct/enum; AND (b) NO capture is mutated in the body (no
assign / compound-assign to a capture name — mirror Rust `detect_mutations`, block-bodies only); AND
(c) NO nested closure (retain the Phase-1 `stmts_have_nested_closure`/`expr_has_nested_closure`
guard). If a closure FAILS any of (a)/(b)/(c) — resource capture (→2b), mutated (→2c), or nested —
2a KEEPS THE STUB for that whole closure (exactly as Phase 1 does) and does NOT lift it. So the NEW
`lowerable` decision = `(no captures OR all-captures-2a-primitive-unmutated) AND no nested closure`.
A NON-capturing lowerable closure is unchanged from Step A (Phase-1 win). Conservative: doubt → stub.
⚠ The make-site positive collector is now the SOLE lowering-decision input — the Phase-1 INVERTED
`closure_body_captures` (`:9505`) is NO LONGER consulted for the lowering decision (verify it's
unused elsewhere; if only the make-site used it, remove it — resolves the two-detector smell).

### (c) Register `__Closure_N`'s FIELDS at the MAKE-SITE (NOT in `compute_closure_sig`)
For a 2a-lowerable closure WITH captures, write `gmod.type_infos["__Closure_<cid>"]` =
`GirTypeInfo("__Closure_<cid>", [GirFieldInfo(cap.name, type_name_of(cap.type_id)) for cap in
captures (in order)], <no variants>, false)` — mirror the IStruct registration pattern
(`lower.gg:10751`). This REPLACES the implicit `{char __pad;}` placeholder (today `__Closure_N` is
registered as a bare named type with no `type_infos` fields → codegen emits `char __pad`). LIR
Pass-2 (`lir_lower.gg:883`) then fills `LirStructDef.fields` automatically and codegen stops emitting
`char __pad`. ⚠ Write this at the make-site (the ONLY place captures are known), NOT in
`compute_closure_sig` (which runs for stub closures too). A non-capturing closure writes NO fields
(keeps the placeholder; its body reads no env).

### (d) Make-site VALUE — branch on (lowerable, has-captures)
In the EClosure arm (`:5689`), after collecting captures + the 2a guard:
- **2a-lowerable AND has captures →** build a struct-ctor of `__Closure_<cid>`: `int env_tmp =
  add_local(&ctx, closure_struct_tid, NO_NAME)` (the `__Closure_<cid>` named type id from
  `compute_closure_sig`/`lookup_or_register_named`), `emit(GICallExtern(env_tmp, "__Closure_<cid>",
  [OpCopy(cap.local_id) for cap in captures]))` (the intercept lowers this to `IStructInit` — a STACK
  env; primitives are bit-copied, NO CoW). Then assign into the `GorgetClosure` closure slot: `int
  cdst = add_local(&ctx, GorgetClosure_tid, NO_NAME); emit(GIAssign(cdst, OpCopy(env_tmp)))` — the
  step-5 LIR promotion packs `env_tmp`→heap into `cdst`. Return `cdst`. Do NOT emit `__make_closure_`
  for this case. ⚠ VERIFY the GIR assign-from-`__Closure_N`-typed-into-`GorgetClosure`-typed is what
  the LIR promotion (step 5) keys on; the executor must make the promotion fire on exactly this
  shape (typed: src local type == `__Closure_<cid>` named, dst slot type == `GorgetClosure`).
- **lowerable AND no captures →** KEEP the Step-A `GICallExtern(cdst, "__make_closure_<cid>", [])`
  NULL-env path unchanged (body reads no env → correct).
- **stubbed →** KEEP `__make_closure_<cid>` (NULL-env; stays WRONG as today; 2b/2c fix).

### (e) Post-pass body — field-load captures (in the drain, `:11670`, lowerable branch)
Before `lower_closure_body` lowers the body (or inside it, before the body stmts), for a record WITH
captures, per capture in order emit `GIFieldLoad(fdst, env_local=_1, capture_index)` (a value-typed
`fdst` for 2a primitives) + `nl_put(capture.name → fdst)` so body references resolve to the loaded
value — mirror Rust `emit_closure_call_function`. `_1` is the env-ptr param `lower_closure_body`
already registers (`self_ptr`, `:9836` era). ⚠ The capture's `nl_put` must happen BEFORE the body is
lowered so a bare `EIdentifier(cap.name)` in the body resolves to `fdst`, NOT fall to the `[bug]`
EIdentifier fallback. The `LiftedClosure.captures` vector (recorded at the make-site) carries the
order + names into the post-pass. (`lower_closure_body` may need a `captures` param, or read it off
the record — thread it cleanly.)

## 5. `lir_lower.gg` — closure-pack promotion (the `try_closure_pack` equivalent; HIGHEST RISK — does NOT exist yet)
When a `__Closure_<cid>`-typed value is assigned into a `GorgetClosure`-typed slot, REPLACE the plain
copy with: `__gorget_closure_env_alloc(sizeof(__Closure_<cid>))` (runtime extern → heap ptr) +
`IMemcpy(heap_ptr, ISlotAddr(stack_env_slot), size)` + `IClosurePack(dst_slot, heap_ptr, call_func,
false)`. Mirror Rust `try_closure_pack` (`operands.rs:1233-1355` — VERIFY against current Rust before
copying). ⚠ The promotion needs the `call_func` (the `__Closure_<cid>__call` LIR function index) for
the `__Closure_<cid>` struct — resolve it via TYPED metadata, NOT a `starts_with("__Closure_")` name
match where avoidable (CLAUDE.md no-name-matching; the env struct name → its call fn is exactly the
kind of mapping that belongs on typed metadata — if no typed channel exists, the cleanest 2a path is
to look it up in `gmod.functions`/`fn_sigs` by the derived `"__Closure_<cid>__call"` name as a
documented C-emit-boundary exception, but PREFER threading the cid). ⚠ `__gorget_closure_env_alloc`
must be ensured-as-extern (it exists in the Rust runtime; the self-host must declare it). VERIFY the
`sizeof` is computed correctly for the `__Closure_<cid>` struct (Pass-2 has filled its fields by
promotion time? — check pass ORDERING: the `type_infos` fields are written at the make-site (GIR),
Pass-2 fills `LirStructDef.fields` before codegen; confirm the promotion can get the size).

## 6. `lir_codegen.gg` — the wart STAYS (transitional); minimal change
The `__make_closure_` NULL-env branch (`:3707-3715`) + its return-type entry (`:2472-2474`) REMAIN —
stubbed + non-capturing closures still route through it. Do NOT delete it in 2a (that's the end-of-
Phase-2 cleanup; log it). The `IClosurePack`/`ICallClosure` codegen (`:3390/:3397`) is READY; the env
struct's field-store (`IStructInit`) + field-load (`GIFieldLoad`) codegen are already correct and
ACTIVATE automatically once `gmod.type_infos["__Closure_<cid>"]` has fields. So 2a's `lir_codegen.gg`
work is ideally ZERO (or only ensuring `__gorget_closure_env_alloc` is declared, if that lives here).

## 7. Risks (ranked)
1. **LIR closure-pack promotion (§5) — does not exist; the make-or-break piece.** Getting the
   `__Closure_N`→`GorgetClosure` detection, the env_alloc/memcpy/IClosurePack sequence, the `call_fn`
   resolution, and the size right. Validated by `auto_types` (or the minimal `int k=10; auto
   f=(int x):x+k; print(f(3))` → 13) RUNNING correctly.
2. **Collector dedup + field-index alignment (§4a).** Off-by-one or a duplicate trips the intercept
   guard or silently mis-loads. Validate: a closure capturing one var used twice still has ONE field.
3. **Classification guard correctness (§4b).** Must KEEP STUB for resource/mutated/nested (no
   regression, no false-MATCH); must LIFT all-primitive-unmutated-no-nested. `< UNIT_TYPE`, not
   `< PRIM_COUNT`.
4. **Make-site value branch (§4d).** A capturing-lowerable closure must build the struct-ctor (not
   `__make_closure_`); a non-capturing lowerable + stubbed must keep `__make_closure_` (no regression
   on Step-A/Phase-1 wins). The `__Closure_N`-typed→`GorgetClosure` assign must be the promotion key.
5. **Post-pass field-load nl_put ordering (§4e).** Captures bound BEFORE body lowering, else `[bug]`
   collapse.
6. **Pass ordering for `sizeof`/fields (§5).** `type_infos` (make-site) → Pass-2 fill → promotion
   size. Confirm.

## 8. Validation gate (self-host-dir only; FORCE-REBUILD the driver before each comparison/diff run)
1. `cargo build` + `cargo build --release` + `cargo test --lib` (≈1066/0).
2. Minimal proof: emit-C + run `int main(): int k=10; auto f=(int x): x+k; print(f(3))` → confirm
   `__Closure_0` has a `k` field, the make-site builds + packs a REAL env (not `.env=NULL`), the body
   `GIFieldLoad`s `k`, → prints **13** (Rust oracle).
3. **MUST NOT regress:** the 4 Phase-1 closure snapshots (`closure_block_tail_expr`,
   `closure_as_callback`, `closure_capture_loop_var`, `consume_callable_once`) + `closure_in_spawn` +
   the Step-A spawn canaries (`spawn_closure_inline` 3, `spawn_unchecked` 5, `spawn_closure_void` 3,
   `spawn_closure_copy` 4, `spawn_closure_shared` 3). `self_host_runtime` ≥ **260/0**.
4. NEW MATCHes (snapshot ONLY those that ACTUALLY reach MATCH — verify by running; some have a 2nd
   root): candidates `auto_types`, `test_if_expressions` c20, `test_closures_edge_cases` primitives,
   `closures.gg` primitives. Capturing-RESOURCE / ByMutRef fixtures STAY STUBBED (WRONG) — do NOT
   snapshot them; confirm they don't FALSE-MATCH (collapse) or driver-crash.
5. `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` → parity MATCH ≥ **261** + report the delta; NO
   fixture moves MATCH→worse.
6. `lowerer_comparison` ≥ **954** / `c_emit_comparison` ≥ **882** (re-confirm baselines from
   `--nocapture`; new env-field structs may change c_emit counts — must be unchanged-or-better;
   investigate any drop).
7. `bootstrap_fixed_point` GREEN (REGRESSION guard — the driver uses no lambdas, so NOT the
   validation signal; real validation is the runtime diff + the `→13` proof).

## 9. Files (stage by name only — never `-a`)
`tests/fixtures/self_host_lowerer/{lower.gg, gir.gg, lir_lower.gg, lir_codegen.gg}` + new
`tests/fixtures/runtime_snapshots/*.out` for fixtures that reach MATCH. Do NOT touch `loader.gg`,
`src/`, `TODO.md`/`DONE.md`, other self_host_* dirs.

## 10. Out of scope (LOG)
- Phase 2b (ByValue RESOURCE/CoW captures — make-site clone/move + `is_closure_env` ownership +
  per-field recursive `__Closure_N` drop), Phase 2c (ByMutRef — `MutPtr` fields + `emit_borrow_mut` +
  `GIDerefStore` write-back). Phase 1.5 (String-param closure-call ABI). End-of-Phase-2 deletion of
  the `__make_closure_` wart (`lir_codegen.gg:3707`). The dead `LowerCtx.next_closure_id` field.
