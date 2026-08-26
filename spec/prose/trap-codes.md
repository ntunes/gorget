# Trap-code registry

> **Phase-1 deliverable (RFC §4; D11 trap normalization).** Stable, symbolic
> `T_`-codes for the language's dynamic **traps**: this registry maps **code →
> trap class**. An uncaught trap terminates the program with
> `trap[T_X]: detail at file:line:col` on stderr and exit **101**. Conformance
> compares the `T_` code + exit only — never the human detail (the trailing
> ` at file:line:col` is normalized out). All traps are uncatchable (D25 removed
> the lexical fault-catch recovery form; use the fallible arithmetic operators
> `+!`/`-!`/`*!`/`/!`/`%!`/`<<!`/`>>!` — §7.5 — to surface arithmetic failure into
> the ordinary `throws` / `Result` channel instead).

> **Exit-code context.** Trap + ICE = exit **101** is one tier of the toolchain's
> fixed exit-code taxonomy (ratified 2026-07-15): `0` success · `1` **static
> rejection** (parse OR semantic OR flow-sensitive may-move liveness — ONE
> compile-error class, `error[E_Code]:`) · `2` usage · `101` **trap + ICE** (this
> registry) · `103` **fuel** (`ggdef`-only, outside the cross-lane compared set).
> Static rejection (never ran, stdout empty) and a runtime trap (ran and died)
> are DELIBERATELY distinct codes. Full table: `docs/language-reference.md` §10.10.

## Source of truth & the ratchet

The codes are defined — one per variant, no catch-all — on the trap registry:

- `TrapKind::code()` (`spec/ggdef/src/eval.rs`) → the `T_` codes below. This is
  the **definition's** copy and the source expectations flow FROM.
- T2a adds a production copy (`src/trap.rs`) pinned to this one by a **parity
  lint**, so the two registries can never drift.

Each `code()` is an **exhaustive `match` mirroring the variant set** with **no**
`_` catch-all, so `rustc`'s exhaustiveness check IS the registry ratchet: a new
trap class added without a code is a hard build error, never a silent gap
(CLAUDE.md core-invariant #6 — convert a recurring class into an executable
guard). This mirrors the `E_`/`W_` scheme in [`diagnostic-codes.md`](diagnostic-codes.md).

## Naming scheme

The scheme is uniform **`T_<VariantName>`** — the code is mechanically the trap
variant's identity with a `T_` prefix, exactly as `E_<VariantName>` is for
`SemanticErrorKind` (`src/semantic/errors.rs::code`). It is simultaneously
**systematic** (derived, exhaustive by construction) and **meaningful** (the
variant names are descriptive). Tying the code to the variant identity — not a
sequence number — keeps codes stable against re-numbering and collision-free
across branches. Stability is modulo variant RENAME: renaming a variant changes
its code, so **this registry file is the stability contract**.

## Trap classes (`T_`)

| Code | Class (`TrapKind`) |
|---|---|
| `T_Overflow` | `Overflow` — an overflowing checked `+`/`-`/`*`/`/`/`%`/`**`/unary-neg, a signed `TYPE_MIN / -1` (or `% -1`), an integer `**` with a negative exponent (domain fault — D28 amendment R3), or an out-of-range shift count (owner ruling 2026-07-10: shift-out-of-range normalizes to `T_Overflow`, no separate class) |
| `T_DivByZero` | `DivByZero` — an **integer** `/` or `%` with a zero divisor. FLOAT division by zero does NOT trap: it follows IEEE 754, yielding `±inf` for a nonzero numerator and `NaN` for `0.0/0.0`. The asymmetry is representability, not convention — integer division by zero has no answer in the integer domain, while float division by zero has a defined one. (Owner ruling 2026-08-26.) |
| `T_Bounds` | `Bounds` — an out-of-bounds index |
| `T_UnwrapNone` | `UnwrapNone` — `.unwrap()` on a `None` |
| `T_UnwrapError` | `UnwrapError` — `.unwrap()` on an `Error` |
| `T_UnwrapErrorOnOk` | `UnwrapErrorOnOk` — `.unwrap_error()` on an `Ok` |
| `T_AssertFailed` | `AssertFailed` — a failing `assert` |
| `T_Panic` | `Panic` — an explicit `panic(msg)` |

All traps are **uncatchable**: they always panic and exit 101 (D25 — the earlier
lexical fault-catch recovery form is removed).

## Rendering

An uncaught trap renders on stderr as:

```text
trap[T_X]: <detail> at <file>:<line>:<col>
```

and the process exits **101**. The `<detail>` is impl-defined human text (ggdef
and production provably diverge — e.g. `arithmetic overflow` vs `integer
overflow`) and is **NEVER** compared by conformance; the language contract is
`{T_ code, exit 101}`. No scope-exit drops run on the unwind of an uncaught trap.

<!-- cites: spec/ggdef/src/eval.rs::TrapKind::code -->
<!-- cites: spec/ggdef/src/eval.rs::EXIT_TRAP -->
