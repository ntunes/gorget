# Brief — closure-call typed ABI cast (Gap A; scalar args/returns)

Self-host-dir only, **`tests/fixtures/self_host_lowerer/lir_codegen.gg` ONLY** (scalar core needs no
`lir_lower.gg` change). File-DISJOINT from any chain touching `lower.gg`/`gir.gg`/`lir_lower.gg`.
RUNNING-verified by a scout (2026-06-02, tip `4d68c76d`) that HAND-PATCHED the emitted C and proved
the typed cast flips the scalar `int(int)` closures from broken→correct. ⚠ Needs ≥3 fresh sequential
reviews before the executor.

## The bug (RUNNING-verified)
`ICallClosure` codegen (`lir_codegen.gg:3397-3424`, `case ICallClosure(dst, kind, closure, args,
arg_abis, ret_ty)`) IGNORES both `ret_ty` and the arg types and hardcodes a fully-`void*` fn-pointer:
it emits `((void*(*)(void*, void*, …))(fp))(ep, args…)`. Every arg and the return are cast to
`void*`. For a `(int x): x+k` closure called `f(3)` this "works" only under `-w`; for a `(String
name): …` closure, passing a `Str` struct where `void*` is expected is a hard `cc` error
(`incompatible type for argument … void *`), and the return is mis-typed. Rust's `Inst::CallClosure`
(`src/backend/c_lir/mod.rs:2786-2877`) builds the REAL fn-ptr type: the return C-type + each arg's
real C-type.

## The fix (one site, scalar scope; mirror Rust mod.rs:2786)
In the `ICallClosure` arm (`lir_codegen.gg:3397-3424`):
- Build `ret_c = c_type_name(ret_ty, sn)` (the helper is at `lir_codegen.gg:74`; `sn`/the
  struct-name map is in scope in `emit_inst`).
- For each arg, build its real C-type via `c_type_name(val_types.get(arg).unwrap(), sn)` (the
  per-value type map `val_types` is in scope in `emit_inst` — confirm the accessor name against the
  surrounding arms).
- Emit the call as `((ret_c(*)(void*, arg_c0, arg_c1, …))(fp))(ep, args…)` and assign into `dst`
  (typed `ret_c`), instead of the all-`void*` form. The env pointer (first param) stays `void*`.
- Keep `kind`/`closure`/`ep`/`fp` resolution exactly as today — only the CAST TYPES change.

## ⚠ Scope / explicit NON-claims (RUNNING-verified by the scout — do NOT let the brief overreach)
1. **Scalar args/returns ONLY.** String/struct-RETURNING closures need a SEPARATE upstream fix: the
   result slot is mis-typed (`__v23` declared `int64_t` for a `String(String)` closure because
   `cl_ret` reads a mis-typed slot — `lir_lower.gg:3250` sources `cl_ret` from the dst slot type).
   The cast alone does NOT fix String/struct returns (scout hand-patched the cast and `greet` still
   printed garbage). Do NOT claim this chain fixes String-returning closures — LOG that as a
   follow-up (upstream slot-typing).
2. **Does NOT fix capturing-closure WRONG-OUTPUT** (`closures`, `test_multiline_closures`) — those
   are env-wiring/capture issues (Step 2b territory), unaffected by the cast (scout-verified via
   hand-patch). Do NOT snapshot them.
3. **`arg_abis`/`cl_abis` is built EMPTY** (`lir_lower.gg:3248`) — Rust uses `arg_abis` only for the
   ByValue-non-resource-AGGREGATE deref refinement (Option/Result aggregate args). NOT needed for the
   scalar core; use `val_types` for the per-arg C-type. Leave `lir_lower.gg` untouched.
4. **The Option/Result inline-combinator path** (`lir_codegen.gg:3587-3645`) shares the same `void*`
   disease (causes the `option_map`/`result_map` CRASHES) but is a SEPARATE code site. If the
   executor finds it's the identical fix shape it MAY fold it (re-verify by running option_map →
   MATCH), else LOG it as an adjacent follow-up. Do not let it block the scalar core.

## Parity (re-verify by running)
~4-6 scalar fixtures expected to flip to MATCH (the pure scalar function-type-param cases:
`callable_ref_param`, `generic_callable_ref`, the `int(int)` half of `closure_multiline_return`, …).
Snapshot ONLY fixtures that ACTUALLY reach MATCH (verify each by running vs `cargo run -- run`).

## Validation gate (self-host-dir only; FORCE-REBUILD driver before each comparison/diff run)
1. `cargo build` + `cargo build --release` + `cargo test --lib` (~1066/0).
2. Force-rebuild driver (`rm -f tests/fixtures/self_host_lowerer/driver{,.c}`; `GG_BUILD_TIMEOUT_SECS=600`).
3. Proof: emit-C + run a scalar function-type-param fixture (e.g. `callable_ref_param`); confirm the
   call site casts to the REAL types (e.g. `int64_t(*)(void*, int64_t)`), cc clean, output == Rust.
4. `self_host_runtime` ≥ **267/0** (no regression; add snapshots only for new MATCHes).
5. `lowerer_comparison` ≥ **954**, `c_emit_comparison` ≥ **882** (re-confirm from `--nocapture`;
   investigate any drop — the cast change is codegen-only, fn-COUNT should be unchanged).
6. `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` → MATCH ≥ **267**, report new MATCHes, NO MATCH→worse.
7. `bootstrap_fixed_point` GREEN (driver uses no closures → regression guard, not the signal).

## Files (stage by name only)
`tests/fixtures/self_host_lowerer/lir_codegen.gg` + new `tests/fixtures/runtime_snapshots/*.out` for
new MATCHes. Do NOT touch `lower.gg`/`gir.gg`/`lir_lower.gg`/`loader.gg`/`src/`/`TODO`/`DONE`.

## Follow-ups to LOG
- String/struct-returning closure slot-typing (upstream `cl_ret` source, `lir_lower.gg:3250`).
- The Option/Result inline-combinator `void*` site (`lir_codegen.gg:3587-3645`) if not folded.
- Devirtualizing statically-known closures to direct calls (Rust does; bigger scope).
- The 33-fixture collection-HOF inlining gap (no `HofExpand` in self-host) — its own multi-chain item.
