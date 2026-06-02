# Brief — struct-returning FREE externs register their return type (loader.gg)

Self-host-dir only, **`tests/fixtures/self_host_lowerer/loader.gg` ONLY.** File-DISJOINT from the
closure cluster (`lower.gg`/`gir.gg`/`lir_lower.gg`/`lir_codegen.gg`) → safe to run in parallel with
a closure chain. RUNNING-verified by a scout (2026-06-02, tip `4d68c76d`). Mirrors the already-landed
arena_checkpoint fix (`4189e55f`). ⚠ Needs ≥3 fresh sequential reviews before the executor.

## The bug (RUNNING-verified)
`exec_output_captures_stderr` + `test_process` CC-FAIL with `incompatible types when assigning to
type 'int64_t' from type 'ExecResult'`. Emitted C: `__v2 = gorget_exec_output(...)` where
`gorget_exec_output` returns the `ExecResult` struct but `__v2` is typed `int64_t` (the default).
Both Rust oracles pass. Root: the `IExternBlock` arm (`loader.gg:982-1010`) registers the
`call_redirect` (`exec_output → gorget_exec_output`) but pushes NO fn_sig stub, so the extern's
return type never reaches `fn_sigs` → the call result defaults to I64. This is the SAME class the
arena_checkpoint fix (`4189e55f`) solved — but that fix only patched the **equip-block** stub arm
(`loader.gg:885-905`); the **free-extern (`extern "C":`) block** arm (`IExternBlock`) was left
registration-free.

## The fix (mirror loader.gg:885-905 into the IExternBlock loop)
In the `IExternBlock` arm (`loader.gg:982-1010`), for each extern fn decl `ext_fd` whose return type
is a TNamed struct (or bool), push an empty-body `FunctionDef` stub carrying `ext_fd.return_type` into
`fn_sigs` — exactly as the equip-block arm does at `loader.gg:885-905`. The downstream read sites
(the `call_redirect` resolution `lower.gg:6182`, the fn_sigs refine pass) already exist and need NO
change. (Re-confirm the exact field/match shape of the equip-block arm and replicate it.)

## ⚠ Carve-outs (CRITICAL — same reasoning as the arena fix)
Register the return type ONLY for **TNamed-struct** + **bool** returns. EXCLUDE:
- **`cstr` returns** (e.g. `_parse_last_error`, `path_*`) — they rely on the `is_cstr_returning_call`
  coercion path (`lower.gg:6458`); registering a fn_sig would disrupt it.
- **`String` returns** — own handling path; do not register.
- Primitive returns are already handled / harmless — focus the new arm on TNamed-struct + bool.
String/str cannot reach the arm as TNamed anyway (type keywords → TPrimitive), but be explicit in
the guard so a future struct named oddly doesn't slip a cstr/String through.

## Parity (re-verify by running)
+2 confirmed (`exec_output_captures_stderr`, `test_process`). Possibly `http_patch` later (a harder
`Result__Socket__GorgetString` Result-wrapping shape — do NOT bundle; verify separately). Snapshot
ONLY fixtures that ACTUALLY reach MATCH.

## Validation gate (self-host-dir only; FORCE-REBUILD driver before each comparison/diff run)
1. `cargo build` + `cargo build --release` + `cargo test --lib` (~1066/0).
2. Force-rebuild driver (`rm -f tests/fixtures/self_host_lowerer/driver{,.c}`; `GG_BUILD_TIMEOUT_SECS=600`).
3. `self_host_runtime` ≥ **267/0** + 2 new snapshots (`exec_output_captures_stderr`, `test_process`)
   if they MATCH; verify each vs `cargo run -- run`.
4. `lowerer_comparison` ≥ **954**, `c_emit_comparison` ≥ **882** (re-confirm from `--nocapture`).
   ⚠ Registering new fn_sig stubs could in principle add user-fn-count entries — verify the stubs are
   externs (not user fns) so the comparison counts are unchanged-or-better; investigate any change.
5. `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` → MATCH ≥ **267** (target 269), NO MATCH→worse. Watch
   the spawn/process families + the cstr fixtures (`path_*`, error fixtures) for any regression from
   the new registration (the carve-out protects them — confirm).
6. `bootstrap_fixed_point` GREEN.

## Files (stage by name only)
`tests/fixtures/self_host_lowerer/loader.gg` + new `tests/fixtures/runtime_snapshots/*.out` for new
MATCHes. Do NOT touch `lower.gg`/`gir.gg`/`lir_lower.gg`/`lir_codegen.gg`/`src/`/`TODO`/`DONE`.

## Follow-ups to LOG
- `http_patch` (Result-wrapping-a-struct free extern) — harder shape, separate.
- Any other free-extern struct/bool returns surfaced by the diff after this lands.
