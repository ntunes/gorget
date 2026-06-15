# Brief — self-host Box[T]-local construction + drop miscompile (bugs #1 & #2)

**Track:** FIDELITY (self-host parity). **Scout:** aafd352c (RUN-verified, premise reframed).
**Scope THIS brief = bugs #1 + #2 only** (the shared-misclassification foundation). Bugs #3 (`Box.new` static-method mangling, `lower_expr.gg:977-1018`) and #4 (`box.get()`/`box.set()`) are QUEUED for a follow-up chain — do NOT touch `lower_expr.gg` here (keeps this disjoint from the concurrent comp-filter track).

## Repro (the contested premise is SETTLED)
Trigger = ANY `Box[T]` local that is read back or dropped — no struct / Vector / param needed:
```
from std.collections import Box
void main():
    Box[int] b = Box(5)
    print(f"{*b}")
```
Rust gg → `5`. Self-host → **SEGFAULT** (and `Box[P] b = Box(P(3,7)); P x = *b` → `munmap_chunk(): invalid pointer`).

⚠ **The committed `tests/fixtures/self_host_lowerer/driver` binary is STALE** (segfaults on any user struct ctor). REBUILD it fresh first (`./target/release/gg build tests/fixtures/self_host_lowerer/driver.gg ...`, ~60s) or you will see spurious driver-crashes masking the real bug. Re-read how `self_host_runtime` / `self_host_runtime_diff` in `tests/integration.rs` build+invoke the driver and replicate.

## Root cause (one shared misclassification)
`Box__T` is registered as a struct (`type-id >= LT_STRUCT_BASE`), but in C it is `typedef void* Box__T` (pointer-represented). `lir_is_aggregate(ty)` (`lir.gg:45`: `ty >= LT_STRUCT_BASE`) therefore MISCLASSIFIES a Box value as an inline aggregate, and two paths take the aggregate branch before the Box-aware guard:

**Bug #1 — construction store (SEGFAULT, SCALAR-inner boxes only), `lir_codegen.gg:3338-3341`:**
(Review note R3: this `memcpy`-garbage mechanism fires ONLY for `Box[int]`/scalar-inner — `infer_inst_type_with_ret` returns `LT_PTR` for the IBoxAlloc result (`:2513`) so the `vty==LT_PTR` aggregate branch hits the bad `memcpy` `:3341`. For STRUCT-inner boxes (`Box[P]`) the IBoxAlloc result is typed `box_sid` (`:2511`), `vty==slot_ty`, and the store already falls to a correct plain copy `:3358` → a struct-inner box crashes PURELY on Bug #2 (drop), not Bug #1. The hoisted fix is still correct + safe for both.)
```
case ISlotStore(slot, value, is_move):
    if lir_is_aggregate(slot_ty):              # Box__int64_t is aggregate → TRUE
        if vty == LT_PTR or ...:
            return "memcpy(&" + s(slot) + ", " + v(value) + ", sizeof(...));"   # ← BUG
```
This `memcpy(&__s3, __v2, sizeof(Box))` copies `*__v2` (the payload bytes) into the slot → the slot holds a garbage pointer (e.g. `10`) → `*b` reads address 10 → SEGFAULT. The correct Box-aware guard at `lir_codegen.gg:3367` (`slot_ty == LT_PTR and lir_type_is_box(vty)` → plain `slot = value`) is DEAD for an aggregate-typed Box slot because it's gated on `slot_ty == LT_PTR`, but `emit_box_alloc` (`lir_lower.gg:2629`) stores into a slot typed `Box__T` (aggregate).

**Bug #2 — drop/free (munmap abort), `lir_lower.gg:3899-3900` + GIDropIfAlive ~`:3543/:3562`:**
`drop_fn_for_type` maps `runtime == "Box"` → `"free"`. The `GIDropIfAlive` lowering passes `dia_drop_arg = dia_addr` (= `&slot`, via `ISlotAddr`) unless `dia_owns_ptr` (a `PvOwnedPtr` `!`-param). A plain `Box[T]` local is not a `PvOwnedPtr` → emits `free(&__s5)` (the stack-slot ADDRESS, not the heap pointer) → `munmap_chunk(): invalid pointer`.

## Rust reference (mirror exactly — Rust is already correct)
- Construction: Rust stores the box pointer as a plain pointer copy (the existing `:3367` guard already does `slot = value`).
- Drop: `src/lir/lower/drops.rs:241` sets `dst: box_val, slot, ty: LirType::Ptr` — it LOADS the pointer value (`ISlotLoad`) and frees that. `lir_type_is_box` is in scope there (same file `:714`).

## The fix (self-host only)
1. **Construction — `lir_codegen.gg`, ISlotStore arm ~`:3338`:** hoist a `lir_type_is_box(slot_ty, &m)` check ABOVE the `lir_is_aggregate(slot_ty)` branch — if the slot is a Box type, return `slot = value` (plain pointer copy) regardless of aggregate classification. `lir_type_is_box` already exists (`:714`) and is in scope.
2. **Drop — `lir_lower.gg`, GIDropIfAlive arm ~`:3562`:** when the local's type `lir_type_is_box`, set `dia_drop_arg` to a LOADED pointer (`ISlotLoad(slot, LT_PTR)`), not `dia_addr`. Keep the guard/liveness on `&slot`/`ISlotAddr` per the `drop_elab` `build_val_to_slot` note (`:3545-3550`) — ONLY the drop-fn ARG changes. (Bare `free(ptr)` of the correct pointer is sufficient; upgrading `drop_fn_for_type` Box→`__gorget_box_free_<inner>` is optional and out of scope here.) Mirrors Rust `src/lir/lower/drops.rs:~265` (`SlotLoad{ty:Ptr}` + free the loaded value).
   - **⚠ R2 (REQUIRED or the build breaks):** `lir_type_is_box` is defined in `lir_codegen.gg:714` but `lir_lower.gg` does NOT import it. For fix #2, EITHER add `from lir_codegen import lir_type_is_box` to `lir_lower.gg` (verified NO import cycle — `lir_codegen` does not import `lir_lower`), OR inline the box check (it needs only `m.structs` + `m.type_runtime_map`, both already in scope at the GIDropIfAlive site).

## Fixtures / yield (CORRECTED per review R1 — yield is HONEST, ~0 corpus flips)
⚠ **`snag41_audit_box_string_deref` does NOT flip on #1/#2 alone** — review applied both fixes and it STILL SEGFAULTS. It is a #1/#2 **+ box-as-param** fixture: a `Box[String]` local passed to a param (`String f(Box[String] b)`) is lowered via the by-pointer ABI as `ISlotAddr` (`__v4 = &__s3`) → passes the box-slot ADDRESS, not the box pointer value → `*b` reads garbage → segfault. That THIRD site is in the GICall/GICallExtern arg-lowering in `lir_lower.gg` (see comment `:2679-2680` "GICallExtern converts to SlotAddr when `needs_ptr_arg` is true"), NOT `lower_expr.gg`. Do NOT add a snapshot for it (it stays failing).
- **Honest yield of #1/#2: 0 existing corpus fixtures flip.** The value is (a) fixing a real crash class, (b) the prerequisite that unblocks the whole Box-local class, (c) retiring the self-host's Box-dodge fossil (later). Add NEW minimal regression fixtures with snapshots that DO flip (RUN-verified by review):
  - `box_int_local_deref.gg` — `Box[int] b = Box(5); print(f"{*b}")` → `5`.
  - `box_struct_inner_deref.gg` — `Box[P] b = Box(P(3,7)); P x = *b; print(...)` → `3 7` (exercises Bug #2 / struct-inner).
- **NEW sibling gap to QUEUE (report to parent for TODO):** box-as-param (the by-pointer-ABI `ISlotAddr`-passes-`&slot`-not-value site in `lir_lower.gg` GICall arg-lowering) — needed for `snag41_audit_box_string_deref`; queue alongside #3 (`Box.new`) / #4 (`box.get`/`box.set`).
- Removing the self-host's deliberate Box-dodge fossil (`lower_expr.gg:2938-2942`) is a #3/#4-era cleanup — note it, do NOT do it here.

## Gate battery
- `cargo build` + `cargo test --lib`.
- `lowerer_comparison`, `c_emit_comparison` (must not regress matched-counts; both backends build).
- `self_host_bootstrap_fixed_point` (`GG_BUILD_TIMEOUT_SECS=600`) — THE load-bearing canary (Box bugs are self-host-only; fixed_point proves no regression to the self-host's own code, which never stores Box to locals → structurally insulated).
- New Box-fixture runtime snapshots pass via `self_host_runtime`.
- ASan on the new Box fixtures (the fix touches ownership/drop).

## Risk
Low. Both fixes activate ONLY on Box-typed slots; the self-host never stores a Box[T] to a local (confirmed by grep), so no `fixed_point`/`c_emit` shape change for the self-host's own code. Both mirror Rust exactly.

## Discipline
Worktree off gorget-1 (`git merge --ff-only gorget-1` first). Stage ONLY `tests/fixtures/self_host_lowerer/lir_codegen.gg`, `tests/fixtures/self_host_lowerer/lir_lower.gg`, the new fixture `.gg` + `tests/fixtures/runtime_snapshots/*.out`, and `tests/integration.rs` if wiring a test. Do NOT touch `lower_expr.gg`. No `git add -a`.
