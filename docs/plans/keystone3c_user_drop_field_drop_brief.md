# ③(c) user-Drop → auto-field-drop — executor brief (self-host)

> Keystone-③ continuation (after ③(a) `5320b872`, ③(b) `65f5f591`). Oracle:
> `src/lir/lower/drops.rs:307-346` (`DropStrategy::Custom`) + `mod.rs:507-602`.
> Foundations: `docs/plans/keystone3_drop_model_foundations.md`.
> All premises RUN-verified end-to-end (review pass 1 prototyped the COMPLETE 3-part fix: parity 408→409/940 +1, fixed_point GREEN, lowerer 971/c_emit 902, ASan leak-reduced no-double-free). Re-verify against CURRENT source before editing.
> ⚠ Line numbers were written at tip `5320b872` and DRIFT (±20 from later commits): e.g. `emit_type_drop_fns`≈`lir_codegen.gg:5216`, `emit_struct_drops`≈`:4954`, skip-recursive≈`:5226`. ANCHOR on the function NAMES + the surrounding code, not the literal line numbers. THREE edits are required: Part 1 (`populate_drop_metadata`), Part 2 (`emit_struct_drops`), **Part 3 (`compute_reachable_fns` DCE seed — link-critical, do NOT skip)**.

## The gap (RUN-verified)
For a user type with BOTH a `Drop` impl AND droppable fields (`drop_struct_fields.gg`'s `Container`: `Inner inner` + `String tag` + `equip Container with Drop`), the self-host's drop runs the user `Container__drop` method ONLY — the per-field drops (`Inner__drop(&self->inner)`, `gorget_string_free(&self->tag)`) **never fire after it**.

Proof (`drop_struct_fields.gg`):
- Oracle: `… / drop container box / drop inner nested / drop inner auto`
- Self-host (current): `… / drop container box / drop inner auto` — **missing `drop inner nested`** (Container's `Inner` field is not dropped after the user method). (`Wrapper`, which has NO user Drop, correctly auto-drops its `Inner` field via the generated `Wrapper__drop` → "drop inner auto".)

This is a CLASS bug: every user-`Drop` type with a droppable field leaks its fields; `drop_struct_fields` just makes it print-observable (its String fields are `cap==0` literals so no heap leak, but the missing `Inner__drop` print is the visible symptom; a user-Drop type with a heap field would genuinely leak).

## Oracle (Rust) — two distinct mechanisms; the self-host has only ONE
1. **`DropStrategy::Custom(fn_name)`** (`drops.rs:307-346`): a type with a user `Drop` IMPL. The unified `__gorget_dtor_Type` = `DropGuardOpen` → call the user drop fn → `lower_field_drops` (per-field drops, AFTER the user fn) → `DropGuardClose`. Drop SITES for a Custom type route to `__gorget_dtor_Type`. **← this is ③(c); the self-host is MISSING it.**
2. **`drop_collision_types` + `params.len() > 1`** (`mod.rs:531-544`): a type whose `{T}__drop` name clashes with a NON-destructor user METHOD (e.g. `DataFrame.drop(cols)`, 2 params). Renames the auto FIELD-drop glue to `__gorget_dtor_T` (field-drops ONLY — the user method is unrelated and can't be called). **← the self-host HAS this (`lir_lower.gg:3841`).**

The distinguishing factor is param count: a real `Drop` destructor `drop(!self)` has **exactly 1 param** (self); `DataFrame.drop(cols)` has 2.

## Self-host current state (the scaffolding ALREADY exists — wiring is split)
- `TypeDropInfo` (`lir.gg:369-373`) ALREADY has `String user_drop_fn  # "" if none`. Currently ALWAYS populated as `""` (`lir_lower.gg:1010/1075/3713`).
- `emit_type_drop_fns` (`lir_codegen.gg:5194`) ALREADY emits `user_drop_fn(__p)` (`:5220-5221`) THEN field drops (structs) / variant drops (enums). BUT it **skips any type in `recursive_drop_structs`/`recursive_drop_enums`** (`:5204-5209`).
- A user-Drop-with-fields type (Container) IS in `recursive_drop_structs` (it has droppable fields) → `emit_type_drop_fns` skips it → it's handled by `emit_struct_drops` (`:4938`), whose body emits ONLY field drops (`:4982-4990`), NEVER `user_drop_fn`.
- AND the collision-detect loop (`lir_lower.gg:3838-3849`) only fires for `param_types.len() > 1`, so a 1-param `Container__drop` destructor is NOT added to `drop_collision_types`. Result: `populate_drop_metadata` (`:3703`) sets `drop_fn_name = "Container__drop"` (no rename); `emit_struct_drops` sees `fn_exists("Container__drop")==true` (the user method) → SKIPS generating the glue (`:4976-4978`); the drop SITE calls the user `Container__drop` alone.

## The fix (self-host) — populate + consume the existing `user_drop_fn` slot

### Part 1 — detect + populate (`lir_lower.gg`, `populate_drop_metadata`, struct loop ~3686-3714)
After `field_drops` is computed for `type_name` AND inside the `if field_drops.len() > 0 or is_resource_struct:` block (so the gate "has droppable content" is satisfied — see ⚠ below), detect a user `Drop` impl and, if present, mark the type as a Custom-drop type:
```gorget
# Detect a user Drop impl: a GIR fn named "{type_name}__drop" with EXACTLY
# 1 param (the `!self` destructor). >1 params = a non-destructor method
# (DataFrame.drop(cols)) handled by the existing collision loop; 0/absent =
# no user Drop. Mirrors Rust DropStrategy::Custom (the type has a Drop impl).
String user_drop = ""
int uf = 0
while uf < gmod.functions.len():
    GirFunction gf = gmod.functions.get(uf).unwrap()
    if gf.name == type_name + "__drop" and gf.param_types.len() == 1:
        user_drop = gf.name
    uf += 1
```
Then where `drop_fn_name` is chosen (`:3703-3705`) and the `TypeDropInfo` is built (`:3713`):
```gorget
String drop_fn_name = type_name + "__drop"
if m.drop_collision_types.contains(type_name) or user_drop != "":
    drop_fn_name = "__gorget_dtor_" + type_name
    # Route drop SITES to the renamed glue. (drop_fn_for_type reads
    # drop_collision_types; without this the site keeps calling the user
    # method directly.) For the user-Drop case this ALSO records the
    # rename so the GIDropIfAlive lowering resolves __gorget_dtor_T.
    m.drop_collision_types.put(type_name, true)
…
m.type_drop_fns.put(type_name, TypeDropInfo(drop_fn_name, fd_copy, user_drop, no_evd))
```
(Replace the `""` in the existing `TypeDropInfo(drop_fn_name, fd_copy, "", no_evd)` with `user_drop`.)

⚠ **Why gate on `field_drops.len() > 0` (do NOT mark fieldless user-Drop types):** a fieldless user-Drop type is NOT in `recursive_drop_structs` and may not be in `type_drop_fns` → no emitter generates `__gorget_dtor_T` → routing the drop site there would be an UNDEFINED SYMBOL link error. A fieldless user-Drop type keeps calling its user `{T}__drop` directly (current behaviour, complete). The detection lives inside the `if field_drops.len() > 0 or is_resource_struct:` block, so this gate is automatic. (RUN-verify: confirm no fieldless user-Drop type gets routed to a missing `__gorget_dtor_`.)

### Part 2 — emit the user-fn call before field drops (`lir_codegen.gg`, `emit_struct_drops` ~4979-4991)
For each `type_name`, look up its `user_drop_fn` and, if non-empty, emit the call before the field-drop loop:
```gorget
String cname = find_c_name(type_name, &m, &sn)
Vector[FieldDrop] drops = m.recursive_drop_structs.get(type_name).unwrap()
out = out + "static inline void " + drop_name + "(" + cname + "* self) {\n"
# ③(c): a user Drop impl runs FIRST, then the per-field auto-drops (mirror
# Rust drops.rs:322-341). The user method is recorded on TypeDropInfo.
String udf = ""
if m.type_drop_fns.contains(type_name):
    udf = m.type_drop_fns.get(type_name).unwrap().user_drop_fn
if udf != "":
    out = out + "    " + udf + "(self);\n"
int di = 0
while di < drops.len():
    …existing field-drop loop…
```
(The user method's C signature is `void {T}__drop(void* __p0)`; passing `self` — a `cname*` — to the `void*` param is a valid implicit cast, exactly as `Inner__drop(&self->inner)` already does.)

### Part 3 — seed the user drop fn as a DCE root (`lir_codegen.gg`, `compute_reachable_fns`, struct-drop seed loop ~1070-1085) — REQUIRED (link-critical, found by review pass 1)
After the fix, the user `{T}__drop` (e.g. `Container__drop`) is no longer called from any LIR drop site — it is called ONLY from inside the generated `__gorget_dtor_{T}` glue via a C STRING (`emit_struct_drops`' `udf + "(self);"`), which is invisible to the LIR call graph. So DCE prunes it → `undefined reference to 'Container__drop'` LINK FAILURE. (RUN-proven by review pass 1: removing this seed link-fails.) Mirror Rust `src/lir/optimize.rs:228-241` ("functions referenced by type_drop_fns are invisible to DCE → seed them"). The existing struct-drop seed loop already seeds `dn` (= `type_drop_fns[dkey].drop_fn_name`, i.e. `__gorget_dtor_{T}`) and `{T}__clone`; ADD the `user_drop_fn` to the same root set:
```gorget
    while dk < drop_keys.len():
        String dkey = drop_keys.get(dk).unwrap()
        String dn = dkey + "__drop"
        String udn = ""        # ③(c): the user Drop fn called from __gorget_dtor_{T}
        if m.type_drop_fns.contains(dkey):
            dn = m.type_drop_fns.get(dkey).unwrap().drop_fn_name
            udn = m.type_drop_fns.get(dkey).unwrap().user_drop_fn
        String cn = dkey + "__clone"
        int dfi = 0
        while dfi < n:
            String fname_d = m.functions.get(dfi).unwrap().name
            if (fname_d == dn or fname_d == cn or (udn != "" and fname_d == udn)) and not reachable.get(dfi).unwrap():
                reachable.set(dfi, true)
                worklist.push(dfi)
            dfi += 1
        dk += 1
```
⚠ Do NOT add the symmetric seed to the ENUM seed loop (~1086-1101): the enum EMIT path (`emit_enum_drops`) is NOT being changed (no user-Drop enum in the corpus), so an enum `user_drop_fn` would never be called → seeding it would keep a dead fn alive (a `c_emit` fn-count regression). Struct loop only.

### Why this is correct
- `drop_struct_fields`: Container → `user_drop != ""` → `drop_collision_types` gets Container → `drop_fn_name = __gorget_dtor_Container`. The drop SITE routes to `__gorget_dtor_Container` (via `drop_fn_for_type`'s collision branch, `lir_lower.gg:3648`). `emit_struct_drops` generates `__gorget_dtor_Container(self)` = `Container__drop(self);` (user, prints "drop container box") THEN `Inner__drop(&self->inner)` ("drop inner nested") + `gorget_string_free(&self->tag)`. Output gains **"drop inner nested"** → MATCH (+1). Wrapper (no user Drop) is unchanged.
- **No double-free:** user `drop(!self)` bodies read fields by CLONE (`String l = self.label` → `gorget_string_clone_to_owned`), so the fields stay valid for the auto-drops after. RUN-verified for `drop_struct_fields`/`drop_reassign`/`drop_raii`. (Add an ASan check.)
- **No regression to existing collision types** (DataFrame, `params>1`): their `user_drop_fn` stays `""` (the detection requires `param_types.len()==1`), so `emit_struct_drops` emits field-drops only for them — unchanged.

## Scope / NOT in scope
- ⚠ **The detection is a NAME-SHAPE MATCH** (`gf.name == type_name + "__drop" and param_types.len() == 1`), NOT a typed signal. Rust derives `DropStrategy::Custom(fn_name)` from the `equip…with Drop` AST node at GIR construction (`src/ir/lowering/mod.rs:264`) and reads the typed enum (`mod.rs:673`); the self-host's `GirTypeInfo` (`gir.gg:~270`) / `ResourceMetadata` (`schema.gg`) carry NO drop-strategy flag, so "has a Drop impl" can only be reconstructed from the name today. This is the CLAUDE.md "No name matching" pattern — accepted here as a documented, **NOT-new** violation: the EXISTING `>1` collision loop in the same function (`lir_lower.gg`) already name-matches identically, and Rust ALSO name-matches for collision detection (`mod.rs:540-541`). The architecturally-pure fix (add a typed `has_user_drop: bool` / a `DropStrategy` to `GirTypeInfo`, set when `equip…with Drop` is lowered) belongs to the ① typed-enum_category/ownership-subsystem chain — log it as a ① follow-up; do NOT expand ③(c) into it.
- ONLY the struct path (`emit_struct_drops` + `populate_drop_metadata` struct loop + the struct DCE-seed loop). **No user-Drop ENUM exists in the corpus** → the `emit_enum_drops`/enum-variant + enum-DCE-seed symmetry is NOT exercised; log a TODO ("if a user-Drop enum with droppable payloads appears, mirror the user_drop_fn call into the enum emit path AND its DCE seed"). Do NOT speculatively edit the enum path.
- `drop_match_partial_init` (drop-ORDERING: `drop r1` after vs before "after-match"), `named_scope_drop` (missing inner-scope `a`/`b` drops), `drop_raii` (undefined `gorget_box_get`), `owning_param_drop_at_exit` (`!`-param ABI type mismatch) are SEPARATE pre-existing gaps — NOT ③(c). Do not touch.
- ⚠ Layering note (document, do not fix here): `drop_collision_types` is populated for the user-Drop case in `populate_drop_metadata`, which runs AFTER `lower_type_defs` (where Option/Result variant-drop names are built, `lir_lower.gg:3830` comment). So a user-Drop struct used as an `Option[T]`/`Result[T,_]` PAYLOAD would route its variant-drop to the user method (no field drops) — but this is the SAME as today's behaviour (no regression; Container isn't an Option payload in the corpus), so it is a documented follow-up, not a blocker. (The existing `>1` collision loop runs before lower_type_defs; if review deems the Option-payload case worth closing now, the 1-param destructor detection can ALSO be added to that early loop with a field-droppability check — but that needs `drop_fn_for_type`'s deps populated early; verify before doing so.)

## Files
- `tests/fixtures/self_host_lowerer/lir_lower.gg` (`populate_drop_metadata`).
- `tests/fixtures/self_host_lowerer/lir_codegen.gg` (`emit_struct_drops`).
- Confirm no other self_host dir has independent copies needing the same edit (`ls tests/fixtures/self_host_*/lir_lower.gg tests/fixtures/self_host_*/lir_codegen.gg`; the lowerer's are the ones the runtime/c_emit/fixed_point gates use).

## Snapshot
After the fix, RUN `drop_struct_fields`, confirm it MATCHes the oracle, and add `tests/fixtures/runtime_snapshots/drop_struct_fields.out` with the exact oracle stdout (verify with `cargo run -q -- run tests/fixtures/drop_struct_fields.gg | od -c`):
```
created wrapper
created container
created config
drop container box
drop inner nested
drop inner auto
```

## Gates (in order; STOP + report on any red) — GG_BUILD_TIMEOUT_SECS=600
1. Force-rebuild driver: `rm -f tests/fixtures/self_host_lowerer/driver tests/fixtures/self_host_lowerer/driver.c`
2. `cargo build` + `cargo test --lib` (1072/0 debug).
3. `self_host_bootstrap_fixed_point` — MUST stay GREEN (does the DRIVER define user-Drop types with fields? if so, they now get field-drops-after-user-drop — must be byte-identical across stages + no double-free):
   `GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_bootstrap_fixed_point -- --nocapture`
4. Parity (expect 409, the +1 from drop_struct_fields):
   `GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_runtime_diff -- --nocapture` — read `PARITY = …`.
5. `self_host_runtime` (lock-in, +1 new snapshot, 0 existing changed): `cargo test --test integration --release self_host_runtime -- --nocapture`
6. `lowerer_comparison` (>= 971) + `c_emit_comparison` — ⚠ watch the fn-count: this ADDS one `__gorget_dtor_{T}` per user-Drop-with-fields type. Rust ALSO emits `__gorget_dtor_T` for `DropStrategy::Custom` types, so the count should MATCH Rust (parity may even improve). If c_emit DROPS below 902, investigate whether Rust emits the same glue for that type (it should) before proceeding.
7. ASan: RUN `drop_struct_fields` + `drop_reassign` + `drop_raii`(if it compiles) under `-fsanitize=address` — confirm NO double-free / NO UAF (a benign leak inside a user-drop body is the pre-existing class, OK).
   (PARENT runs the full `cargo test --test integration` at integration.)

## Worktree discipline (NON-NEGOTIABLE)
- `pwd` + `git rev-parse --show-toplevel` FIRST; confirm inside YOUR worktree, NOT `/workspace/gorget-1`. `git merge --ff-only gorget-1` FIRST.
- Stage ONLY by name: `git add tests/fixtures/self_host_lowerer/lir_lower.gg tests/fixtures/self_host_lowerer/lir_codegen.gg tests/fixtures/runtime_snapshots/drop_struct_fields.out docs/plans/keystone3c_user_drop_field_drop_brief.md` — NEVER `git add -a`/`.`.
- Commit on your branch; do NOT merge to gorget-1 (the parent integrates after a fresh output review).
