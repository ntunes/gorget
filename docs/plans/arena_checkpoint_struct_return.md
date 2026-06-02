# Brief — self-host struct-returning extern equip methods (arena_checkpoint)

FIDELITY round (1:1:1:1 cadence). Self-host-dir only. **Runs as a PARALLEL chain
file-disjoint from the EIf chain** (this touches `loader.gg` ONLY; EIf touches `lower.gg`).
Re-verified by RUNNING the code at `362a54a9`. ⚠ Needs ≥3 fresh sequential reviews before
the executor launches.

## Bug (re-verified: emit-C + cc)
`arena_checkpoint.gg` CC-FAILs:
- `error: incompatible types when assigning to type 'int64_t' from type
  'GorgetArenaCheckpoint'` at `__v24 = gorget_arena_checkpoint(__v2);`
- `error: incompatible type for argument 2 of 'gorget_arena_restore'` (the `cp` flows in
  mis-typed).

`lib/std/alloc.gg:17` declares `extern ArenaCheckpoint checkpoint() = "gorget_arena_checkpoint"`.
The method's return type is the STRUCT `ArenaCheckpoint`, but the self-host types the call
result as `int64_t` (default) → the C assign `int64_t = GorgetArenaCheckpoint` is rejected.

## Root cause (scouted + verified)
The loader's extern-equip-stub return-type registration (`loader.gg:854-866`) registers
the return type only for `TPrimitive` returns `void`/`bool` (the R10 + sync-round work).
It has **no `TNamed` (struct) case**, so for `checkpoint()` (return type
`TNamed("ArenaCheckpoint")`) `stub_register_ret` stays false → no fn_sigs stub is pushed →
the equip pre-pass (`lower.gg:10373-10392`, which ALREADY handles TNamed via
`map_fn_return_type`) never sees it → the call site (`lower.gg:~5124`) defaults `ret_tid`
to `I64_TYPE`. The downstream type-mapping is ALREADY CORRECT and needs NO changes:
`lir_to_runtime_name` maps `ArenaCheckpoint → GorgetArenaCheckpoint` (`lir_codegen.gg:172`),
`is_runtime_defined_named` skips emitting it (uses the runtime typedef, `:146`), and the
`GorgetArenaCheckpoint` typedef is spliced from `runtime_arena_alloc.c` whenever a
`gorget_arena_*`/`GorgetArena*` symbol appears (`lir_codegen.gg:5761-5762`). So once the
call result types as `ArenaCheckpoint`, everything else falls into place.

## Fix — ONE edit in `loader.gg` (register TNamed struct extern returns)
Extend the `loader.gg:854-861` match to register `TNamed` struct return types, so the
empty-body stub carries the struct return type and the fn_sigs pre-pass registers
`Arena__checkpoint → ArenaCheckpoint`:
```
bool stub_register_ret = false
match emdef.return_type.ty:
    case TPrimitive(stub_rt):
        if stub_rt == "void" or stub_rt == "bool":
            stub_register_ret = true
    case TNamed(stub_tn, _):
        # Struct-returning extern equip method (ArenaCheckpoint, File, UdpAddr, …):
        # register so the call result types as the struct (maps to the runtime C
        # type via lir_to_runtime_name), not the I64 default → fixes the
        # `int64_t = GorgetArenaCheckpoint` cc error.
        if stub_tn != "String" and stub_tn != "Str":   # see ⚠ below — String/cstr carve-out
            stub_register_ret = true
    else:
        pass
```
⚠ **String/Str/cstr carve-out — RESOLVED SAFE by pass-1 (empirical).** `String`/`str` are
TYPE KEYWORDS (`lexer.gg:147-150` → `KwStr`/`KwStringType`), so a `String`/`str` return
parses as **`TPrimitive("String")`/`TPrimitive("str")`** (`parser.gg:1213`) — it takes the
EXISTING `TPrimitive` arm and NEVER reaches the new `TNamed` case. So the carve-out is
STRUCTURALLY guaranteed; the `!= "String"`/`!= "Str"` guard is dead-but-harmless
belt-and-suspenders (keep it; note the keyword is lowercase `"str"`). Pass-1 also verified
empirically (applied the patch to a scratch driver): **arena_checkpoint CC-FAIL → MATCH
(output-identical to oracle), ZERO regressions across the full 247-fixture passing set**;
the broader blast radius (the `TNamed` case also registers ~30 other equip-extern
struct/generic returns — `read_all`→`Result[..]`, `local_addr`→`UdpAddr`, …) is benign
(the ~10 affected network fixtures were already CC-FAIL for unrelated reasons and stay so;
the only bare-struct returns — ArenaCheckpoint/ExecResult/UdpAddr — all have
`lir_to_runtime_name` mappings, so no unmapped-struct emit). **Executor nicety:** update the
`loader.gg:822-853` comment block to describe the new TNamed struct-handle case (+ that
String/str can't reach it because they're TPrimitive) — keep the self-host reading like the
manual.

## Scope / expected outcome
`arena_checkpoint` CC-FAIL → MATCH (**+1**). The same fix unlocks 4 other struct-returning
extern equip methods (`io.gg` File `_std*_handle()` ×3, `udp.gg` `local_addr()`) — only
`arena_checkpoint` is fixture-tested, but re-measure for any others that move. Do NOT
reshape the fixture or `alloc.gg`.

## ⚠ File-disjointness (PARALLEL-chain discipline)
Touches `tests/fixtures/self_host_lowerer/loader.gg` ONLY. Does NOT touch `lower.gg`
(the Arena CONSTRUCTOR `Arena(4096)` is already handled by R9's
`allocator_constructor_runtime_name` in lower.gg — unchanged), nor `lir_lower.gg`/
`lir_codegen.gg` (type-mapping already correct). So it integrates cleanly alongside the
EIf chain (lower.gg). ⚠ The executor must `git merge --ff-only gorget-1` on entry and stage
ONLY `loader.gg` (+ TODO/DONE) — never `lower.gg`.

## Gate (self-host-dir only — no `src/`)
1. `cargo build` + `cargo test --lib` green.
2. Force-rebuild the driver; emit-C `arena_checkpoint` → `__vNN = gorget_arena_checkpoint(...)`
   now types as `GorgetArenaCheckpoint` (not `int64_t`) → cc → run → MATCH `gg run`.
3. **FULL `cargo test --test integration -- --test-threads=4`** — esp. that NO
   String-returning extern equip method regressed (the carve-out); `lowerer_comparison` /
   `c_emit_comparison` unchanged-or-better, `self_host_runtime` lock-in ≥247/0,
   `self_host_bootstrap_fixed_point` GREEN.
4. `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` → parity (expect +1); report which moved.
5. Additive snapshot re-seed — `arena_checkpoint.out` SHOULD appear; zero existing modified.

## Files (stage by name only — never `-a`)
`tests/fixtures/self_host_lowerer/loader.gg` + `TODO.md`/`DONE.md`. No `src/`, no `lower.gg`,
no `lir_*.gg`.
