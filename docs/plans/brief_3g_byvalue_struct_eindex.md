# Brief v2 — by-value-struct EIndex read (3g + 3h-struct) — +3, READY (re-reviewed; blast-radius folded)

Scout RUN-measured **+3 parity (375→378 = 40.7%)**, lowerer 960, c_emit 891 (+1), fixed_point GREEN.
A fresh brief-review (v1) CONFIRMED the root + mechanism but raised two BLOCKING reservations now
folded into this v2: (1) the change rewrites codegen for EVERY non-resource non-scalar struct `v[i]`
across the corpus, so the blast radius is far broader than the 4 named fixtures — the executor MUST
gate on the full **`self_host_runtime` lock-in net** (build-breaking) staying green, not just the
runtime_diff count; (2) `leak_game_loop` (a currently-PASSING lock-in snapshot) is directly in the
blast radius. Re-pinned line numbers below (v1 drifted).

## Root (the 3g "bare-Option" framing was a downstream symptom — CONFIRMED by review)
The 3h EIndex-read fix's dst-typing 3-way split (`lower.gg`, the split is at **5964-5973**, inside
`case EIndex(base_box, idx_box):` at **5895**) has **case (c) = the `else:` at 5970-5973**, which for a
NON-resource NON-scalar struct element falls to a deliberate `OpConstI64(0)` STUB (`OpConstI64(0)` at
**5972**). So `v[i]` for a struct element produced `const 0i64`, never calling `gorget_array_get`. The
bare un-mangled `Option` in emitted C was a downstream symptom (the I64-typed EIndex result feeds the
match-scrutinee prelude fallback at `lower.gg:~8277-8281`). `.get(i).unwrap()` works because it routes
a DIFFERENT LIR shape (`gorget_array_safe_get` → `Option__Ref__T` + `option_ref_payload` lift at
`lower.gg:4084` / `lir_lower.gg:1475`), never the EIndex path. (Rust separates them via a dedicated
`IndexLoad` GIR inst — `methods.rs:3264-3274` computes `result_type` = `elem_type` BY VALUE for the
non-resource/non-task/non-string case (3272), then emits `IndexLoad` + a typed `Load` deref; the
self-host has no `IndexLoad`, so EIndex collapses onto the getter.)

## The fix (2 files — exactly the scout-measured prototype)
1. **`lower.gg` EIndex dst-typing case (c) (the `else:` at `~5970-5973`):** change the
   non-resource-non-scalar guard from the I64/0 stub to `elif eix_elem_tid >= 0:` → type `dst` as the
   BARE element type BY VALUE (only a truly-UNREGISTERED elem `< 0` falls to the I64/0 stub).
   `eix_elem_tid` is in scope (declared at **5954**, `resolve_field_gir_type(...)`); the 3-way-split
   doc comment is at 5955-5963. So a `Vector[Item]` / `Vector[Option[int]]` `v[i]` reads the real struct.
2. **`lir_lower.gg` (the lift-skip + aggregate copy-out):** the Tier-1 lift fires at
   `if dst_local >= 0 and is_collection_void_return(call_name):` (**3269**, inside
   `case GICallExtern(dst_local, func_name, args)` at **3065**). `is_collection_void_return` (**2082**)
   INCLUDES `gorget_array_get` (**2083**) and `gorget_map_get` (**2093**) — so for an EIndex getter with
   an aggregate dst this DOUBLE-wraps (void*→Option/aggregate). Fix: skip the lift when the UNMAPPED
   `func_name == "gorget_array_get" or func_name == "gorget_map_get"`, and instead emit an aggregate-dst
   `ILoad` (a C struct copy-out). **The aggregate-ILoad extension point is the existing
   `returns_ptr_to_element` + `ILoad` block at `lir_lower.gg:3308-3315`** (the aggregate `*(T*)(ptr)`
   copy-out codegen already exists at `lir_codegen.gg:3179-3180`). ⚠ **v1's `deref_elem` / `eindex_getter`
   names are INVENTED — there is no such function/var; do NOT grep for them. Work in the real region.**
   ⚠⚠ **THE DICT TRAP (caught by RUNNING — keying on the MAPPED name dropped parity to 359 / +22
   crashes):** disambiguate on the **UNMAPPED `func_name`**, NOT `call_name`/the mapped name.
   `Vector.get()` → `gorget_array_safe_get` (`map_array_method` at **1475**, distinct), so
   `gorget_array_get` IS EIndex-exclusive; but `Dict.get()` → `gorget_map_get` via `map_dict_method`
   (**1530**) — the SAME symbol as `Dict[key]`. The DIFFERENCE: a `Dict.get()` METHOD call arrives as
   `GICallExtern(mdst, <mono'd Dict__K__V__get>, …)` (method-call lowering at `lower.gg:5760`) that
   `map_runtime_name` rewrites; an EIndex `Dict[key]` emits the RAW `gorget_map_get` symbol directly
   (the only emitters of the raw `gorget_array_get`/`gorget_map_get` GICallExtern symbols are the EIndex
   arm, `lower.gg:5919/5921/5923`). So key the skip + the new aggregate-ILoad on the raw/unmapped
   `func_name`, SCOPED to exactly `func_name ∈ {gorget_array_get, gorget_map_get}` (the broad version
   SIGSEGVs `.unwrap()`'s `Option__Ref__T` dsts).

## ⚠ BLAST RADIUS + LOCK-IN GATE (folded from review reservations 1 & 2 — LOAD-BEARING)
This change rewrites the codegen of EVERY `v[i]` whose element is a non-resource non-scalar struct,
across the WHOLE corpus — not just the 4 fixtures that flip to MATCH. ~25 lock-in fixtures declare
`Vector[StructType]` + index reads; several hit case (c) today and currently emit I64/0 garbage that is
unobserved (so they "pass" lock-in). If the aggregate copy-out is even slightly off they cc-fail
("incompatible types from void\*" — the exact failure the case-(c) comment warns about) or shift output
→ **breaks the build-breaking `self_host_runtime` lock-in test.** Therefore the executor MUST:
- After implementing, **force-rebuild the driver** (`rm tests/fixtures/self_host_lowerer/driver{,.c}`),
  then run the FULL **`self_host_runtime`** lock-in net (default-running / build-breaking). It MUST stay
  green: the committed snapshot set grows by EXACTLY the new MATCHes (the +3), and **NO existing
  snapshot may change**. If it goes red, a case-(c) lock-in fixture regressed.
- Spot-check the reviewer's named at-risk case-(c) lock-in fixtures by hand if anything is red:
  **`leak_game_loop`** (`leak_game_loop.gg:51` `DrawRange dr = draws[di]` on `Vector[DrawRange]`; `:65`
  `Some(points[0])` on `Vector[Vec3]` — both currently execute but produce unobserved I64/0 garbage),
  **`struct_vector_bare_param2`**, **`drop_struct_collection_fields`**.

## Reviewers verify (load-bearing)
1. Case (c) (the `else:` at 5970-5973) types only REGISTERED struct elems by-value (`eix_elem_tid >= 0`;
   unregistered `< 0` still stubs); `Vector[Item][i].field`/`.method()` reads the real struct.
2. **The Dict trap:** the lift-skip + aggregate-ILoad key on the UNMAPPED `func_name`, scoped to exactly
   the 2 EIndex getters — confirm `Dict.get()`/`.unwrap()`/`Vector.get()` (the `Option__Ref__T` +
   safe_get paths) are NOT affected (the broad version crashed them). RUN dict_*/option_* fixtures.
3. **Lock-in net stays green** (reservation 1/2): `self_host_runtime` green after the change; exactly +3
   new snapshots, zero existing-snapshot changes. `leak_game_loop`/`struct_vector_bare_param2`/
   `drop_struct_collection_fields` confirmed unbroken.
4. **+3 exact, zero-regress (runtime_diff set-diff):** parity 375→378 (`test_struct_methods`,
   `coroutine_struct_methods`, `test_generic_struct` flip MATCH); CRASH 31→31 (no new); lowerer 960 /
   c_emit 891 (the +1 is one of the flips); `bootstrap_fixed_point` GREEN; `cargo test --lib` 1072/0.
5. SERIAL gates; `git checkout -- .` to revert (NEVER `git stash`).

## Out of scope (log to TODO)
- **`spawn_vector_await` is ORTHOGONAL to this fix** (review reservation 3 — v1's causal framing was
  WRONG). `Task[int]` is a RESOURCE type, so `tasks[j]` routes through case (b) (the `is_resource_type_name`
  branch at 5967-5969), NOT case (c) — fix #1 does not touch it, and fix #2's lift-skip doesn't change
  case (b). `spawn_vector_await` stays non-MATCH either way (its real gap is that `.await()` method
  dispatch on an EIndex-read result isn't lowered). Do NOT claim this fix shifts it; do NOT re-stub Task
  to dodge it. Log: wire `.await()` (and method dispatch generally) on an EIndex-read result.
- **Port Rust's typed `IndexLoad` GIR inst** (review reservation 4 — the architecturally-correct fix).
  Keying the lift-skip on `func_name == "gorget_array_get"` is a name-match on a runtime symbol to make
  a semantic (lift-vs-copy-out) decision. It is acceptable here because the surrounding region ALREADY
  routes via name-lists (`is_collection_void_return`/`returns_ptr_to_element`) and these are
  C-emit-adjacent runtime symbols — but the principled fix is a dedicated `IndexLoad` GIR inst
  (mirror `methods.rs:3264-3274`) so EIndex doesn't collapse onto the getter. Log to TODO.

## Integration
`lower.gg` (the EIndex case-(c) `else:` at ~5970-5973) + `lir_lower.gg` (the `is_collection_void_return`
lift-skip at ~3269 + the aggregate-ILoad in the `returns_ptr_to_element`+ILoad region ~3308-3315). The
lower.gg edit is in the SAME EIndex-read region as the landed 3h — re-pin by symbol (`case EIndex` /
`eix_elem_tid`). Regen → +3 new snapshots (`test_struct_methods.out`, `coroutine_struct_methods.out`,
`test_generic_struct.out`), purely additive; NO existing snapshot changes. Target 375→378.
