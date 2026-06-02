# Brief — int-literal narrowing vs a Ref/Owned-wrapped integer operand (Rust gg typecheck)

CORRECTNESS fix in the RUST compiler (`src/semantic/typecheck.rs`). NOT self-host. Runs as a
PARALLEL chain file-disjoint from the closure Phase-2a self-host chain. Re-verified by RUNNING
+ instrumenting (scout, 2026-06-02). ⚠ Needs ≥3 fresh sequential reviews before the executor.

## Bug (CONFIRMED by running `gg check`)
An int literal fails to narrow to `uint8` in a comparison when the OTHER operand resolves to a
**`Ref`/`Owned`-wrapped** integer type — e.g. an inline `Vector[uint8].get(i).unwrap()` (which
types as `ResolvedType::Ref(uint8)`, a borrowed view into the element). It narrows FINE when the
operand is a bare `Primitive(uint8)` (a declared `uint8` local, a plain-fn `uint8` return,
`data[offset]` indexing). Repro: `if data.get(offset).unwrap() == 0:` errors `cannot implicitly
convert int to uint8`; `uint8 b = data.get(offset).unwrap(); if b == 0:` and `if plain_byte()
== 0:` build. External impact: gorget-arena `md3.gg:32`, `bsp.gg:171`/`:403` (the
`read_fixed_string` loops), unmasked by `c462cffa`. Repro file (ephemeral):
`/tmp/gorget_uint8_unwrap_gap.gg` (also preserved verbatim in the TODO entry).

## Root cause (scout-verified, file:line)
In `infer_expr`'s `Expr::BinaryOp` arm (`src/semantic/typecheck.rs`):
- **The narrowing gate (lines 1238–1246):** an RHS bare-`IntLiteral` operand gets a `rhs_hint` =
  the LHS operand's resolved type, but ONLY IF `self.types.get(self.resolve_type(left_type))`
  matches `ResolvedType::Primitive(p) if is_integer_type(p)`. The inline `.get(i).unwrap()` chain
  resolves to `ResolvedType::Ref(uint8)` — which the bare-`Primitive` match MISSES. (Instrumented:
  case-3 left `resolved=Ref(TypeId)`, `ref_inner=Some(Primitive(Uint8))`; cases 1/2
  `resolved=Primitive(Uint8)`.)
- Failure flow: gate misses → literal `0` stays `int` → `unify(left=Ref(uint8), right=int)`
  (line ~893 auto-derefs the Ref) → `unify(uint8, int)` → `is_safe_integer_widening(int, uint8)`
  false → `UnsafeIntegerConversion` error (lines 870–886).
- The narrowing is genuinely shape-gated, NOT type-driven — scout's probes confirm it's the
  `Ref`-wrapping (not "method chain"/"generic"): `data[offset]==0` builds (bare Primitive),
  `auto x = data.get(i).unwrap(); x==0` ERRORS (`x` is itself `Ref(uint8)`).

## Fix (~5 lines — peel Ref/Owned before the Primitive check; mirror the existing precedent)
At the gate (lines 1238–1246), before the `Primitive` check, peel `Ref`/`Owned` wrappers and
thread the PEELED-INNER typeid as the hint (the `IntLiteral` consumer at line ~1007 requires a
`Primitive` hint, so thread the inner P's typeid, NOT the `Ref` typeid). Mirror the EXISTING
reference-grade peel idiom in the SAME file at **lines 2667–2670** (the `src_inner` cast
auto-deref; pass-1 corrected the cite from 2676 — 2676 is the adjacent `tgt_castable` block):
```rust
let src_inner = match self.types.get(src) {
    ResolvedType::Ref(inner) | ResolvedType::Owned(inner) => self.resolve_type(*inner),
    _ => src,
};
```
i.e. resolve `left_type`; if it's `Ref(inner)`/`Owned(inner)`, peel to `resolve_type(inner)`; then
apply the existing `Primitive(p) if is_integer_type(p)` check on the peeled type and thread that
peeled typeid as `rhs_hint`. This makes narrowing TYPE-DRIVEN (auto-deref), consistent with how
`unify` already auto-derefs `Ref`/`Owned` (lines ~893–909). The bare-`Primitive`-only gate is
scar tissue (an oversight), not an intentional guard — per the owner's "Rust-not-sacrosanct"
directive, this IS improving the Rust impl toward reference-grade (the peel precedent at 2676 is
the established idiom; the gate just never adopted it).

## ⚠ Scope / what NOT to touch
- Do NOT add a symmetric LHS-literal narrowing path (the scout found `0 == chain` already BUILDS
  by widening luck — `unify(int, Ref(uint8))` derefs to `unify(int, uint8)`, `uint8→int` is safe
  widening). Full operand-order symmetry is a separate, larger change — out of scope. Only the
  `chain == literal` (RHS-literal) form is broken; this fix closes exactly that.
- Do NOT touch the overflow guard (lines ~1008–1020) — it must keep firing on the threaded
  (inner) hint so `someByteRef == 300` still errors "out of range".
- Leave unbound `Var` operands un-peeled/un-narrowed (they won't match `Primitive`, correct —
  `resolve_type` already follows substituted `Var` chains, line ~568).

## Blast radius (scout-assessed — essentially nil)
- Overflow guard runs on the threaded inner `Primitive(uint8)` typeid → identical to `uint8 x =
  300` (still errors). No over-narrowing.
- The ONLY newly-accepted programs are `int-literal <cmp/arith/bitwise> <Ref/Owned-wrapped-int
  operand>` — currently a FALSE rejection. No should-error program becomes accepted.
- No `tests/fixtures/*.gg` currently does the inline-sized-int-chain-vs-literal form (why the bug
  stayed hidden) → near-zero risk of breaking existing programs.
- No overload/generic-inference interaction (the hint flows only into the `IntLiteral` arm).

## Validation gate (Rust `src/` change → the FULLER suite)
1. `cargo build` + `cargo test --lib` green.
2. `gg check` the repro: all 3 cases type-check (no error); confirm case (3) no longer errors.
3. **Targeted regression (must stay green):** `cargo test --test integration --release` for
   `value_out_of_range_error`, `string_coerce_args`, `struct_string_coerce`, `char_str_coerce`,
   `int_range`, `match_int_ranges`, `overflow_add`/`overflow_sub`/`overflow_mul`/`overflow_wrap`,
   `panic_location_overflow`; `cargo test --test security` (esp. `sec_40_integer_narrowing` —
   uses explicit `as`, must be unaffected). ⚠ **(pass-1 baseline flag) `sec_19_field_borrow_escape`
   and `sec_80_char_construction_edges` FAIL on the clean tree WITHOUT this fix** (pre-existing,
   unrelated to integer narrowing — confirmed by stashing the fix). Do NOT read those 2 reds as a
   regression from this change; everything else in the security suite must stay green.
4. **FULL** `cargo test --lib` + `cargo test --test integration -- --test-threads=4` (parent
   runs the full sweep at integration — this is a compiler change, broad blast radius in
   principle even if narrow in practice). ⚠ Because the Rust compiler builds the self-host driver,
   the combined-tree gate also re-runs the self-host comparison/runtime tests.
5. **The regression fixture ALREADY EXISTS — UN-IGNORE it (do NOT create a duplicate).**
   `tests/fixtures/narrow_int_literal_vs_ref_operand.gg` (exercises all 3 forms — declared-local,
   plain-fn, inline `.get().unwrap()` — with deterministic stdout, and the inline-chain comparison
   branches are actually TAKEN: data `[7,0]`, prints `local: seven` / `plain: seven` /
   `inline: seven` / `inline: zero`) is already wired in `tests/integration.rs` as
   `narrow_int_literal_vs_ref_operand`, currently `#[ignore]`d (the inline case won't type-check
   until this fix lands). The executor must: REMOVE the `#[ignore = "..."]` attribute on that test
   + confirm it now PASSES (`cargo test --test integration --release narrow_int_literal_vs_ref_operand`).
   It already follows "Don't redesign around compiler gaps" (expected output = correct behavior).
   Do NOT author a new fixture.

## Files (stage by name only — never `-a`)
`src/semantic/typecheck.rs` (the fix) + `tests/integration.rs` (REMOVE the `#[ignore]` on
`narrow_int_literal_vs_ref_operand` — the fixture `.gg` + its registration already exist, do NOT
re-add them). No self-host `.gg` files; no `TODO`/`DONE` (parent owns them).
