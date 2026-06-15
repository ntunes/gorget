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

**Size the zero from the canonical `func.pointee_types[ptr.0]`** (the SAME source the C backend uses, already available — `func` is a param to this emitter; it is read at `:6434` for exactly this purpose). When `value_is_null` and `func.pointee_types.get(ptr.0)` yields a sized type (`Struct(sid)` or any sized aggregate), emit `call ptr @memset(ptr %v{ptr}, i32 0, i64 {sizeof_lir_type(that_ty)})` instead of the 8-byte `store ptr null`. Use `sizeof_lir_type(&LirType::Struct(sid), &module.structs, snames)` (`:725`) for the byte count.

**Keep the existing `dest_field_ty` scan + scalar arms as the FALLBACK** when `func.pointee_types[ptr.0]` is `None`, so non-pointee cases are unchanged.

This mirrors the C backend's `dst_pointee`-driven memset (`c_lir/mod.rs:2728-2734`) and makes the two backends byte-size-identical at this site.

### Precision requirements (from brief-review pass 1, SIGN OFF — HARD requirements)
- **A (hard) — match `LirType::Struct(sid)` SPECIFICALLY; do NOT memset bare-`Ptr`/`PtrTo`-to-nonstruct/`Void`/`FuncRef`/scalar pointees.** A genuine pointer-typed field (e.g. DictIter's `source: ptr`, guarded by the existing comment at `:3589-3597`) has pointee `Ptr` → `sizeof` 8, correct only by accident; matching it as "any sized type" is the wrong-layer trap. Only the aggregate/struct case needs the full memset; the 8-byte `store ptr null` is already correct for a true pointer field.
- **B (hard) — size from the UNDERLYING struct, not `sizeof_lir_type(PtrTo)`.** The `Str`/`GorgetString` move-out (`Error_0` field, 32 bytes) is one of the under-zeros. If `func.pointee_types[ptr.0]` holds `PtrTo(GorgetString)` (not `Struct(GorgetString)`), `sizeof_lir_type(&LirType::PtrTo(_))` returns **8** → no fix. Extract `sid` from BOTH `Struct(sid)` AND `PtrTo(sid)`, then call `sizeof_lir_type(&LirType::Struct(sid), &module.structs, snames)` (`:725`, signature verified). Mirrors how the C backend's `c_type_named`+`sizeof(ty_name)` resolves both to the struct name.
- **C (minor) — reword the `:6434` citation:** `func.pointee_types` is read at `:6435` for Auto-ABI disambiguation, NOT this purpose; the Store path currently does NOT read it at all (that's the bug). The load-bearing fact (it's in scope on `func: &LirFunction` and populated post-opt) holds.
- **Root-cause sub-detail correction:** the FieldPtr-scan returns `None` because the store's `ptr` is a `Cast`/byte-`getelementptr` result, NOT an `Inst::FieldPtr` dst (so `d.0 == ptr.0` never matches) — not value-renumbering. `compute_module_pointee_types` (`src/lir/types.rs:814`) seeds FieldPtr dsts (`:832-836`) AND propagates through `PtrCast`/`Bitcast` (`:845-851`), so the Cast result DOES carry the struct pointee in `func.pointee_types`. Runs at `src/main.rs:792` (post-`optimize_module` `:790`, pre-split `:808`).

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
