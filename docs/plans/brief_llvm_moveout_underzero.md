# Brief — LLVM move-out field-zero under-zeroes resource-bearing structs (double-free)

**Track:** LLVM-green residual (5th track). **Scout:** aad81fa8 (RUN-verified end-to-end).
**Fixtures fixed:** `snag41_match_scrutinee_consume` (SIGABRT double-free), `toml_stringify` (SIGSEGV). Both correct on C.

## Root cause (single shared write-site)
A move-out of a struct/enum field zeroes the moved-from field via a shared LIR `Inst::Store { ptr: <FieldPtr to payload>, value: <Null> }`. Drop-elaboration is **shared** by both backends (`src/main.rs:790`, before the backend split at `:808`) — both receive identical post-optimized BIR. The divergence is purely in how each backend lowers that null-store:

- **C backend (CORRECT, the oracle)** — `src/backend/c_lir/mod.rs:2726-2734`: reads the canonical pointee table `ptr_pointee`, seeded from `func.pointee_types` (recomputed AFTER optimization, `src/main.rs:792`, keyed on final value-ids). Sees the dest points to a `V` (40 bytes) → emits `memset(__v49, 0, sizeof(__gg_V))`. Zeroes the whole payload. ✅
- **LLVM backend (BUGGY)** — `src/backend/llvm/mod.rs:3582-3647`: in the `else if matches!(val_ty, Some(Ptr)|Some(FuncRef))` branch it re-derives `dest_field_ty` via a **fragile manual FieldPtr-scan** (`:3598-3608`, `if d.0 == ptr.0`). Because the backend runs on post-BIR-optimized LIR (copy-prop / value-renumbering), the Store's `ptr` value-id no longer equals the original `FieldPtr`'s `dst` value-id → the scan returns `None` → the `Some(LirType::Struct(sid)) => memset` arm (`:3626-3635`) is SKIPPED → falls to `_ =>` (`:3645`) which emits `store ptr null` zeroing only the first **8 bytes** (the enum tag). ❌

Consequence: the heap `String` pointer at offset 8 of the payload survives the "zero". It is also copied into the destination local; at scope exit both copies are dropped → the same pointer is freed twice → double-free (snag41) / heap corruption → SIGSEGV (toml, which has 59 such move-zeros).

Empirically confirmed: the LLVM `Struct(sid) => memset` arm NEVER fires for these enum-payload null-stores (zero non-prelude memsets; all move-zeros are 8-byte `store ptr null`); the C backend emits a full-struct `memset` for every one.

## The fix (write-site, layering-discipline — one source of truth)
In `src/backend/llvm/mod.rs`, the `Inst::Store` null-zeroing path (the `value_is_null` + `matches!(val_ty, Some(Ptr)|Some(FuncRef))` branch, ~`:3618-3647`):

**Size the zero from the canonical `func.pointee_types[ptr.0]`** (the SAME source the C backend uses, already in scope — `func: &LirFunction` is a param to `emit_inst`). See **THE RULE** below for the exact, pass-2-corrected condition: match `LirType::Struct(sid)` ONLY (NOT `PtrTo`), emit `call ptr @memset(ptr %v{ptr}, i32 0, i64 {sizeof_lir_type(&LirType::Struct(sid), &module.structs, snames)})` instead of the 8-byte `store ptr null`; everything else keeps the existing 8-byte / scalar fallback.

This mirrors the C backend's `dst_pointee`-driven memset (`c_lir/mod.rs:2729-2737`) and makes the two backends byte-size-identical at this site.

### THE RULE (corrected by brief-review pass 2 — empirically instrumented; this SUPERSEDES the earlier A/B)
Under the `value_is_null` arm of the `Inst::Store` null-zero path (`~:3618-3635`), **replace** the fragile `dest_field_ty` FieldPtr-scan result with a read of the canonical `func.pointee_types.get(ptr.0)`:
- **if it is `LirType::Struct(sid)` → `memset(ptr, 0, sizeof_lir_type(&LirType::Struct(sid), &module.structs, snames))`** (`sizeof_lir_type` at `:725`, signature verified → `Struct(sid)` returns `computed_c_size`, the true size);
- **otherwise → keep the existing 8-byte `store ptr null` / scalar fallback unchanged.**

This is BYTE-IDENTICAL to the C oracle (`c_lir/mod.rs:2729-2737`: `size_ty = pointee.or(dst_pointee)` → `memset(p,0,sizeof(c_type_named(size_ty)))`, where `c_type_named(Struct)=sizeof(name)` and `c_type_named(PtrTo)=void*`=8).

**WHY `Struct(sid)` ONLY (pass-2 empirical finding — do NOT match `PtrTo`):** instrumentation of `emit_inst` over min4 + snag41 + toml showed that for EVERY `is_null` move-out store, `func.pointee_types[ptr]` is `Struct(sid)` — NEVER `PtrTo`. This is structural: `FieldPtr` seeds the DECLARED field LirType, and a by-value struct field (the move-out target) is declared `Struct(sid)`. The `PtrTo` pointees that DO exist (32 in toml) are all on NON-null genuine pointer-value stores — exactly the case that MUST stay an 8-byte store. **Matching `PtrTo(sid)` here would OVER-ZERO a genuine pointer field and DIVERGE from the C oracle** (the precise over-zero the Risk section forbids). So: match `Struct(sid)` ONLY. The earlier Note-B "extract sid from BOTH Struct AND PtrTo" was WRONG — dropped.
- (Note A subsumed: matching `Struct(sid)` specifically already excludes bare-`Ptr`/`PtrTo`/`Void`/`FuncRef`/scalar — those stay 8-byte, correct.)

**Reservation 2 (advisory — fold the framing):** the `dest_field_ty` scan returns `None` for the null-zero class for EVERY move-out (empirically dead here), so this is a REPLACE (read the canonical table instead of the ad-hoc scan), not a new branch bolted ahead of a live fallback. The scan is retained only for the NON-null `memcpy` arm. Mirror exactly how the C backend reads `ptr_pointee.get(ptr.0)`.

**⚠ Executor caution (pass-3):** "replace the scan RESULT" does NOT mean delete `dest_field_ty` — it stays for the non-null memcpy/`PtrTo` arms. ONLY the null-zero SIZING path switches to reading `func.pointee_types.get(ptr.0)`. Reading "replace … result" in isolation risks an over-aggressive deletion — don't.

**Pass-3 completeness (corpus-wide):** a sweep of all 1122 buildable fixtures found ZERO `size_ty=None` move-out fallbacks targeting a non-`Struct` droppable aggregate — `Struct(sid)`-only is complete, not just for the two targets. toml's `GorgetMap`/`GorgetArray`-typed fields (`TomlValue.data.Tbl_0`/`.Arr_0`) lower to `LirType::Struct(sid)` (the runtime singleton's StructId), NOT `LirType::Resource` (which only appears in the collection-element/`ElemMeta` path), so `sizeof_lir_type(&Struct(sid))` correctly sizes them. The only fields on the 8-byte path are genuine `Ptr`/`PtrTo` pointer fields, correctly left at 8.

- **C (minor citation):** `func.pointee_types` is read at `:6435` for Auto-ABI disambiguation, NOT this purpose; the Store path does NOT currently read it (that's the bug). It IS in scope on `func: &LirFunction` (and `module`, `snames` per `emit_inst` `:2989`), populated post-opt.
- **Root-cause sub-detail (verified pass 2):** the scan returns `None` because the store's `ptr` is a `Cast`/byte-`getelementptr` result (e.g. `getelementptr i8, ptr %v31, i64 96` → `bitcast` → `store`), NOT an `Inst::FieldPtr` dst (`d.0 == ptr.0` never matches) — not value-renumbering. `compute_module_pointee_types` (`src/lir/types.rs:814`) seeds FieldPtr dsts (`:832-836`) AND propagates through `Bitcast` (`:845-851`), so `pointee[ptr]` carries `Struct(sid)`. Runs `src/main.rs:792` (post-`optimize_module` `:790`, pre-split `:808`).

## Minimal repro (sharper than snag41) — heap String required
The bug needs a HEAP String (a static literal has a null alloc-ptr so the partial-zero is harmless). Repro `/tmp/min4.gg` from the scout (recreate it):
```
enum V:
    StringV(String)
enum C:
    Normal(V)
    Other(V)
bool peek(V v):
    match v:
        case V.StringV(s):
            return s.len() > 0
C build_c(String src) throws String:
    return C.Normal(V.StringV(src))
C run(String src) throws String:
    C cc = match build_c(src):
        case Ok(x):
            !x
        case Error(e):
            throw e
    V cv = match cc:
        case C.Normal(v):
            !v
        else:
            return !cc
    if peek(cv):
        return C.Normal(V.StringV("then"))
    return C.Normal(V.StringV("end"))
void main():
    String h = "abc"
    String h2 = h + "def"
    match run(h2):
        case Ok(_): print("ok")
        case Error(_): print("err")
```
C → `ok`; LLVM (build + run binary) → `free(): double free detected in tcache 2`.

## Verify (the crash is MASKED by `gg run`)
`gg run --backend=llvm` prints `ok` and hides the crash. You MUST: `./target/release/gg build --backend=llvm <fixture>` then run the produced binary directly (this is what `tests/integration.rs:269-283` does). Capture the real signal.

## Gate battery
- `cargo build` + `cargo test --lib`.
- `GG_BACKEND=llvm cargo test --test integration --release snag41_match_scrutinee_consume` AND `toml_stringify` → both must pass (run the binary, not `gg run`).
- A broad LLVM regression check: re-run a sample of move-out-heavy LLVM fixtures (enum/struct payload programs) to confirm no over-zero/regression. Parent runs the full `GG_BACKEND=llvm` integration sweep at `--test-threads=1`.
- C backend must be untouched (the fix is in `src/backend/llvm/` only) — spot-check a C run still passes.

## Risk
Low. The change only ever INCREASES the zeroed size from the buggy 8 bytes up to the true field size (from the field's own pointee type) — it can fix under-zeros but cannot introduce over-zeros. Fallback preserved for `None` pointee. LLVM-only; C oracle unchanged; narrows LLVM toward C parity.

## Discipline
Worktree off gorget-1 (`git merge --ff-only gorget-1` first). Stage ONLY `src/backend/llvm/mod.rs`. No `git add -a`.
