# Brief — closure 2b-1: copy-struct capture widen — +1, RUN-VERIFIED

A RUN-verify scout built + ran + measured this end-to-end (at baseline 385): **+1**, sole flip
`copy_struct_closure_capture`, ZERO regressions. Single-file, low-risk. (Current baseline is 392 after the
String-trim chain landed; this is independent → **392 → 393**.)

## Root
The closure make-site capture collector has a "2a guard" (`lower.gg:~6163`, the `{BOOL_TYPE, I64_TYPE,
F64_TYPE}` capture-type allowlist) that rejects any capture whose type isn't a `{bool,int64,double}` scalar.
`copy_struct_closure_capture.gg` captures a `Config{int,int}` (a plain COPY struct — no resources) in a
closure; that capture is rejected → the closure falls to the `__make_closure_N()` NULL-env stub
(`lower.gg:~6223`) → the env pointer is uninitialized → printed as garbage (baseline: `L0 oracle="480000"
self="281474954374592"`).

## The fix (1 file — the scout's exact RUN-verified prototype)
Widen the 2a guard (`lower.gg:~6163`) to ALSO accept a capture whose type is a **registered struct** (in
`gmod.type_infos`) that is **`not is_resource_type_name(...)`** AND **`not is_enum`** — i.e. a byte-copyable
COPY struct with no resource fields. The downstream machinery already handles it: the env-struct field is
built via `type_id_to_name`, and the capture is loaded by a GIFieldLoad **byte-copy** — NO clone/drop
machinery is needed for a copy-struct (that's exactly why it's safe and 2b-1, not 2b-2/2b-3).

⚠ **Scope strictly:** do NOT accept resource-containing structs (they alias their interior on byte-copy →
need make-site CoW clone + per-field `__Closure_N` drop = Phase 2b-2/2b-3, deep) NOR enums (variant/payload
layout). The guard must remain `{bool,int64,double}` ∪ {registered-struct ∧ ¬resource ∧ ¬enum}.

## Reviewers verify
1. **Predicate correctness:** the widened guard accepts ONLY copy-structs — `gmod.type_infos.contains(tname)`
   (registered struct) AND `not is_resource_type_name(tname, &gmod)` (no resource fields → byte-copyable) AND
   `not is_enum`. Confirm a resource-containing struct (String/Vector field) and an enum are STILL rejected
   (fall to the stub — they'd need 2b-2/2b-3 / variant handling). Re-pin the guard line + the predicate names.
2. **Downstream handles copy-struct captures:** confirm the env-field build (`type_id_to_name`) and the
   GIFieldLoad byte-copy in `lower_closure_body` (~:10926-10959) already work for a struct-typed capture (the
   scout's +1/0-regress confirms; verify the path doesn't assume scalar-only).
3. **Zero regress:** no closure fixture that currently (correctly) stays stubbed via the resource/enum branch
   now mis-captures. The make-site NULL-env stub path stays for resource/enum captures.

## Gates (executor; force-rebuild driver; baseline 392)
- `self_host_runtime` lock-in **393/0** (1 new snapshot `copy_struct_closure_capture.out`; NO existing
  snapshot changes); `runtime_diff` 392→**393** (only `copy_struct_closure_capture` flips, ZERO regressions).
- `lowerer_comparison` 960 / `c_emit_comparison` 891 — report (a real env-struct closure now emits where a
  NULL-env stub did → c_emit fn-count COULD shift by +1 if it emits a `__Closure_N__call`; confirm + explain
  if so — this would be a legitimate emission, not a regression).
- `bootstrap_fixed_point` GREEN; `cargo test --lib` 1072/0.
- Stage ONLY `tests/fixtures/self_host_lowerer/lower.gg` + `runtime_snapshots/copy_struct_closure_capture.out`.

## Out of scope (already logged to TODO)
Phase 2b-2 (resource-capture make-site CoW) + 2b-3 (per-field `__Closure_N` env drop/clone — type-erased
`gorget_closure_free`/`_clone_to_owned` need typed dispatch; entangled with the drop machinery) stay
deferred. This widen is the cheap copy-struct subset only.
