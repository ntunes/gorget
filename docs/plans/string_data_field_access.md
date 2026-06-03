# FIDELITY brief — `String.data` field-access (self-host, bug 2)

## Goal
`print(s.data)` / `f"{s.data}"` where `s` is a **value `GorgetString`** prints `0`
and the self-host emits `[bug] EFieldAccess: unknown field 'data' on base type
'GorgetString'`. Oracle (`gg run`): prints the string content (`one`). Fix the
self-host `EFieldAccess` lowering to special-case `GorgetString.data`. Targets
`match_expr_diverging_arm` (its last line `print(tag.data)`) → MATCH, plus any
other `.data`-on-String fixture. **Scope: small, single-site, mirrors Rust.**

## ⚠ This is DISTINCT from the borrowed-String fix `7c43abcf`
That fix handled formatting a `Ptr(GorgetString)` (a borrow alias). THIS is a
`.data` FIELD ACCESS on a **value** `GorgetString` — the field lookup itself
fails (the field isn't in GorgetString's `type_infos`), so it hits the
`[bug]`-placeholder fallthrough. Different code path, different root.

## Root cause (writer-site, single layer)
`tests/fixtures/self_host_lowerer/lower.gg`, the value-position `EFieldAccess`
arm (~lower.gg:5419, `case EFieldAccess(base_box, field_name):`). It resolves the
base's struct type, looks the field up in `gmod.type_infos` (~:5487-5496). For a
`GorgetString` base + field `"data"`, the field is NOT in `type_infos` (the
runtime String struct's fields aren't user-registered) → `found_fi` stays `-1` →
falls through to the HARD-BUG placeholder (~lower.gg:5528-5537) that emits
`diag_bug("EFieldAccess", "... unknown field 'data' on base type 'GorgetString'
...")` + `add_local(I64_TYPE)` + `GIAssign(OpConstI64(0))` → prints `0`.

## Rust reference (the oracle's behavior — MIRROR IT)
`src/ir/lowering/exprs/mod.rs:2105-2108`:
```
// Special case: GorgetString.data — return the GorgetString itself.
// ... accessing .data is valid for printf (%.*s handles it correctly).
if type_name == "GorgetString" && field_name == "data" { <return the base value> }
```
So Rust treats `s.data` on a `GorgetString` as **the GorgetString itself**
(identity) — the formatter then prints it via `%s`/`%.*s`. The executor must read
the exact Rust arm (re-pin by content — line numbers drift) to mirror the precise
return (it returns the base operand/local, typed `GorgetString`).

## The fix (executor: implement + RE-VERIFY by RUNNING)
In the self-host `EFieldAccess` arm, BEFORE the `found_fi < 0` placeholder
fallthrough, add a special case: if `base_type_name == "GorgetString"` (after the
existing Ptr/MutPtr/Box unwrap that the arm already does) AND `field_name ==
"data"`, return the **base local itself** (typed `GorgetString`, the value), NOT
an I64(0). Mirror Rust `mod.rs:2108`.
- Verify the base is the right operand to return (the arm computes `base` via
  `lower_place_base`; for a value `GorgetString` the base local is the string —
  confirm by RUNNING the repro).
- ⚠ Edge: if the base arrives as a `Ptr(GorgetString)` borrow (e.g. `param.data`
  where param is `&String`), returning the Ptr must still format correctly —
  note that `7c43abcf` made the formatters deref `Ptr(GorgetString)`, so
  returning the Ptr-typed base is fine (it gets deref'd at the format site).
  Confirm both `String s="one"; print(s.data)` AND a borrowed variant.
- Do NOT add a real `"data"` field to GorgetString's `type_infos` (that would
  ripple into every struct-field codegen path) — the identity special-case is the
  surgical, Rust-faithful fix.
- Do NOT reshape any fixture.

## File zone
ONLY `tests/fixtures/self_host_lowerer/lower.gg` (the EFieldAccess arm, ~:5419).
File-disjoint from the parallel method-resolution chain (`src/`) — but NOTE it is
the SAME FILE the borrowed-String fix `7c43abcf` already touched (now landed) and
the SAME FILE other self-host chains use; run in an ISOLATED WORKTREE and the
orchestrator merges.

## Gates (force-rebuild driver first: `rm tests/fixtures/self_host_lowerer/driver{,.c}`)
- `String s="one"; print(s.data)` → `one` (was `0`); `match_expr_diverging_arm` → MATCH.
- `runtime_diff` parity ≥ 329 (target +1 or more); no MATCH→worse.
- `self_host_runtime` regressed=0 (then regen → new passing set).
- `lowerer_comparison` ≥958, `c_emit_comparison` ≥887 (a value-position fix;
  fn-counts shouldn't move).
- `bootstrap_fixed_point` GREEN.

## Worktree discipline (executor preamble)
Run `pwd` + `git rev-parse --show-toplevel` FIRST; confirm inside your worktree,
NEVER `/workspace/gorget-1`. Open with `git merge --ff-only gorget-1` to
fast-forward the worktree to the current gorget-1 tip (it branches from `main`,
which lags). `git add tests/fixtures/self_host_lowerer/lower.gg` +
`tests/fixtures/runtime_snapshots/<new>.out` ONLY — never `git add -A`. Run
`cargo build` + the targeted gates above; do NOT run the full 15-min integration
suite (the orchestrator owns the integration sweep at merge).
