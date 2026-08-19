# Rust gg bug — Callable-inferred `Struct &` closure param falls back to int64_t

**Discovered:** R39 Phase 2e Sub-task 0 probe (2026-08-09).

**Fixture:** `repro.gg` (this directory).
**Integration test:** `rust_gg_bug_callable_amp_struct_closure_literal`
in `tests/integration.rs` (currently `#[ignore]`d).
**TODO entry:** `TODO.md` under `## Compiler / Rust gg`.

## Repro

```
./target/release/gg run tests/fixtures/known_gaps/rust_gg_bug_callable_amp_struct_closure_literal/repro.gg
```

## Observed vs intended

- **Observed:** prints `0`.  In some variants (closure body calls a
  method on the mis-inferred parameter, e.g. `(p): p.method_call()`),
  the linker fails with `undefined reference to 'int64_t__method_call'`
  (proves the closure body was type-checked with `p: int64_t`).
- **Intended:** prints `1` — the first `Thing`'s `.val` extracted
  through the Callable-typed HOF.

## Bug shape

The closure literal `(p): p.val` has an UNTYPED parameter `p`.
Rust gg's closure-type-inference pass should propagate the
Callable's declared param type — here `Thing &` — into `p`.
Instead, it falls back to `int64_t` (numeric default), and the
resulting closure struct + emitted C treat `.val` as an integer-
offset "field access" that returns garbage.

Passing a NAMED FUNCTION with a properly-typed `Thing &` signature
side-steps this bug (see
`rust_gg_bug_callable_amp_struct_iterator_segv/repro.gg` for the
named-fn variant, which confirms the closure-inference path is the
specific failure here). ⚠ Updated R43 Track C: that named-fn variant
had a SECOND, unrelated bug — the indirect-call argument ABI — which
is now FIXED and its test is live. The closure-literal defect on this
page is untouched by that fix: its root is parameter-type inference,
not the calling convention.

## Fix direction (speculative)

Closure-literal untyped-param inference must consult the ENCLOSING
call's expected function type: if the enclosing call is
`hof(callable_arg, ...)` where `hof`'s N-th param is
`Callable[R(P1, P2, ...)]`, the closure literal's untyped params
inherit `P1, P2, ...` in order.  Today's behavior appears to bail
to `int64_t` when `P1` is a struct-ref (`Struct &`) — the
scalar-arg branch (`(n): n * 10` with `Callable[T(int)]`) works
correctly per top-level fixture `generic_callable.gg`.

## Impact

- R39 Phase 2e Option C helper design was blocked (closure literal
  `(p): p.parse_type_with_ownership()` for the parse-item callback
  would have been the cleanest idiomatic shape, but this bug forces
  either a NAMED function callback (also broken AT THE TIME — see
  `rust_gg_bug_callable_amp_struct_iterator_segv`, FIXED in R43 Track C)
  or a design rewrite without Callable-typed params).
- The fallback (single `bool consume_comma_or_tok(&self, int)`
  helper with no Callable / no closure literal) shipped instead.

## Related bugs

- `rust_gg_bug_callable_amp_struct_iterator_segv` — named-fn callback
  with the same Callable-typed struct-ref param. **FIXED (R43 Track C)**:
  it was the indirect-call argument ABI, guessed from the argument's
  shape instead of written from the callee's declared ownership; its
  test is now LIVE. A different root from this page's defect.
- `rust_gg_bug_generic_mono_parser_scale` — related but only
  manifests at parser.gg scale; may or may not be a downstream of
  this bug.
