# Brief — self-host reference-grade defect, Case B: reject undefined identifiers

**Track:** the reference-grade defect "the self-host BUILD path silently ACCEPTS ill-typed programs"
(TODO High Priority, Core-#8). This is **Case B (undefined identifier)** — the cleaner of the two root
causes; Case A (explicit-VarDecl type-mismatch, which needs a from-scratch type-compatibility predicate)
is a SEPARATE later brief. Scout: `a3b28b2c` (2026-06-22, premises RUN-confirmed + the naive fix
PROTOTYPED-and-measured).

**Executor constraints:** worktree off `gorget-1` (`git merge --ff-only gorget-1` FIRST; verify pwd is
your worktree, NOT /workspace/gorget-1). `git add` ONLY the files you change. NEVER `git stash` (shared
stack — use `cp`). This is a self-host FRONTEND (resolver) change → `bootstrap_fixed_point` is a
load-bearing gate (the driver self-resolves its own source).

## The defect (RUN-confirmed)
`print(undefined_thing)` BUILDs clean on the self-host (exit 0, emits a
`/* [bug] EIdentifier … unknown identifier … returning OpConstI64(0) placeholder (WRONG) */`, binary
prints `0`) where Rust gg correctly REJECTS it (exit 1, `error: undefined name \`undefined_thing\``, no
binary). The CLI plumbing is CORRECT — `driver.gg` build (`:382-387`) and check (`:494-497`) both call
`resolve_module` → `type_check_module` → `has_errors(ctx.diagnostics)` → `exit(1)`. The defect is that
the RESOLVER never PRODUCES the diagnostic.

## Root cause (verify against current source)
`tests/fixtures/self_host_typechecker/resolve.gg:594-598`, `resolve_expr` EIdentifier arm:
```
case EIdentifier(name):
    if scopes.lookup(name) is Some(def_id):
        ctx.resolution_map.put(expr.span.start, def_id)
    elif not is_builtin(name):
        pass        # ← Rust emits UndefinedName here (resolve.rs:1498-1509)
```
(`resolve.gg` is the real file in `self_host_typechecker/`; it is SYMLINKED into `self_host_lowerer/`
and `self_host_check/` — confirm with `ls -l`, and `grep -rn "case EIdentifier" tests/fixtures/self_host_*`
to be exhaustive about the arm location after a possible line-drift.)

## ⚠ Why the NAIVE fix is WRONG (scout measured this — do NOT ship it)
Simply replacing `pass` with a `ctx.diagnostics.push(...)` self-compiles clean AND fixes the target
case BYTE-IDENTICAL to Rust — **but spuriously rejects 238/1285 corpus fixtures** (measured by the scout
via a full-corpus `check` scan; re-measure, don't trust the count). The false positives are three classes
of legitimately-defined names the self-host resolver can't see by `scopes.lookup`, because the self-host's
import architecture differs from Rust's:
- **Imported stdlib functions** (the bulk): the self-host MANGLES imports to `mod_prefix+name`
  (`loader.gg:758-765`) and registers the bare name ONLY in `call_redirects` (resolved at LOWERING, not
  resolution) — so `scopes.lookup(bare_name)` legitimately misses. (Rust merges imports under their real
  names into scope BEFORE resolution, so Rust's resolver needs no import allow-list; the self-host does.)
  Examples seen: `b64_char_value`, `_parse_int_raw`, `__dt_decompose`, extern stubs `__bytes_*`, imported
  types like `Entity`.
- **Bare enum-variant constructors** (`Red`, `Blue`): Rust skips via `scopes.is_known_variant_name(name)`
  (`resolve.rs:1500`); the self-host has no equivalent guard at this site.
- **Synthetic `__return__`** + compiler-internal intrinsics (`__dict_iter_key`, `__set_drain_entry`):
  Rust excludes `name != "__return__"` (`resolve.rs:1499`).

## What to implement — the diagnostic push GUARDED by a 3-class allow-set
The shippable fix = emit the `UndefinedName` diagnostic ONLY when the name is none of: a builtin
(existing `is_builtin`), a known enum-variant name, `__return__` / a compiler intrinsic, OR an imported
symbol. Two sub-steps (land together — B1 alone doesn't fix the target case, and the diagnostic must NOT
fire until the allow-set is complete or it regresses the 238):

- **B1 — self-contained exclusions (mirror Rust):** add to the `elif not is_builtin(name):` guard a
  check for (a) known-variant names — find/port the self-host equivalent of Rust's
  `scopes.is_known_variant_name` (search `scopes.gg`/`resolve.gg` for variant registration; if no such
  query exists, add one over the variant table the resolver already builds), and (b) `name == "__return__"`
  + the intrinsic-name set (the `__`-prefixed compiler synthetics — enumerate them; prefer a typed
  predicate/registry over a name-prefix test per "no name matching", but a `__`-prefix carve-out for
  KNOWN synthetic names is acceptable if centralized and commented, since these are compiler-internal,
  not user identifiers).
- **B2 — thread the imported-name set into the resolver:** `resolve_module(module, &scopes, &types)`
  (`resolve.gg:1020`) does NOT currently receive the imported-name set; it's built by `load_imports`
  (`loader.gg`) BEFORE `resolve_module`. Thread the bare imported-fn/type names (the `call_redirects`
  keys and/or `imported_fns`) into `resolve_module` → `resolve_expr` and skip them in the guard. Decide
  the cleanest carrier (a `Set[String]`/`Dict` of bare imported names passed as a param, or hung on the
  `ResolveContext`) — match how the resolver already receives scope/type state. THEN flip the `pass` to:
  ```
  ctx.diagnostics.push(Diagnostic.error(expr.span, DkUndefinedName(), "undefined name `" + name + "`"))
  ```
  (add `DkUndefinedName` to the `from diagnostic import …` — verify the variant exists in
  `diagnostic.gg`; if not, add it mirroring the other `Dk*` error kinds + Rust's `ResolveError`/diagnostic
  rendering so the message matches `undefined name \`<n>\``).

**No state snapshot/restore needed** (this is a `Vector.push` onto `ctx.diagnostics`, no TypeTable
mutation — unlike Case A). The cost here is entirely the allow-set, especially B2's import threading.

**Reference-grade bar:** the target is to match Rust's REJECTION (exit nonzero + the diagnostic + no
binary), not to match any current self-host behavior. Do NOT reshape any fixture to dodge this.

## Guard (REQUIRED — the defect is metric-INVISIBLE to `*_comparison`)
The `type_comparison`/`check_comparison` tests diff TYPE lines, not DIAG lines (diagnostics drain to
stderr; stdout stays type-only — `resolve.gg:39-44`), which is WHY this hid. So the guard MUST route
through a DIAG-surfacing driver path:
- Add `tests/fixtures/<name>.gg` = `void main(): print(undefined_thing)` (or the minimal undefined-name
  shape).
- Add an integration test `self_host_rejects_undefined_name` that runs it through the self-host
  `build`/`check` subcommand (the path that prints diagnostics to stderr + `exit(1)`s on `has_errors`)
  — model on the existing `#[ignore]`'d `self_host_check_rejects_illtyped` (`integration.rs:16172-16218`)
  and `build_gg_dir("self_host_typechecker", "driver.gg")` (DIAG→stderr). Do NOT use the bare
  `check_comparison` driver path (it drains DIAG silently → vacuous).
- Assert: `!status.success()` AND stderr/stdout contains `undefined name`. VERIFY it FAILS on current
  code (today: exit 0, prints `ok`) and PASSES after the fix.

## Gates (your worktree; parent runs the full both-backend sweep)
- The new `self_host_rejects_undefined_name` test passes; the prior `pass`-behavior is gone.
- **NO corpus regression:** run the full `check`/build over the corpus (the 238-fixture trap) — confirm
  ZERO spurious rejections remain. This is the load-bearing gate for the allow-set: build the self-host
  driver and run it over every `tests/fixtures/*.gg` (or the `check_comparison`/`runtime_diff` corpus
  list), asserting no fixture that Rust ACCEPTS is now rejected. Report the count.
- `GG_BUILD_TIMEOUT_SECS=600 … self_host_bootstrap_fixed_point` GREEN (the resolver self-resolves the
  driver's own source — the new diagnostic path must not fire on it + must re-converge).
- `cargo test --lib`; `self_host_runtime` 0 regressed; `resolver_comparison`/`type_comparison`/
  `check_comparison`/`lowerer_comparison`/`c_emit_comparison` no regression (structurally neutral for
  well-typed programs — the diagnostic only fires on genuinely-undefined names).

## Riskiest part
B2's import-name threading (getting the allow-set COMPLETE so zero legitimate names are rejected) — the
naive fix's 238 regressions are the proof this is the hard part. Re-measure the corpus-rejection count
before AND after; the fix is not done until it's 0 spurious. Keep the `bootstrap_fixed_point` green
(the driver's own source must resolve clean under the new path).

## Follow-up (NOT this brief)
Case A (explicit-VarDecl type-mismatch, `int x = "s"`): the self-host has NO type-compatibility helper;
needs a coercion-aware `types_compatible(declared, inferred)` predicate built from scratch + the snag-#11
unconditional `infer_expr_type` call with 7-field snapshot/restore (`typecheck.gg:1069-1077`). Narrow the
first A increment to the clearly-incompatible scalar-primitive case (declared `{int,bool,float,char}` vs
inferred string-id and the reverse) to avoid the same over-rejection trap. Guard: un-`#[ignore]` the
existing `self_host_check_rejects_illtyped`. Separate scout-confirmed brief.
