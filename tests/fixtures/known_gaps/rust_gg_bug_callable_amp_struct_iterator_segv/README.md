# Rust gg bug — `Callable[T(Struct &)]` + Vector-iterator callback → SIGSEGV

**Discovered:** R39 Phase 2e Sub-task 0 probe (2026-08-09).

**Fixture:** `repro.gg` (this directory).
**Integration test:** `rust_gg_bug_callable_amp_struct_iterator_segv`
in `tests/integration.rs` (currently `#[ignore]`d).
**TODO entry:** `TODO.md` under `## Compiler / Rust gg`.

## Repro

```
./target/release/gg run tests/fixtures/known_gaps/rust_gg_bug_callable_amp_struct_iterator_segv/repro.gg
```

## Observed vs intended

- **Observed:** `gg: /tmp/.tmpXXXXXX/repro terminated by SIGSEGV (signal 11)`
- **Intended:** prints `1\n2` (each Thing's .val extracted through the
  Callable-typed HOF, one per iteration).

## Bug shape

The pattern in `map_thing`:

```gorget
Thing t = xs.get(i).unwrap()   # bind Vector element to local
out.push(f(&t))                # pass &t through Callable[int(Thing &)]
```

triggers the crash when `f` is a NAMED FUNCTION callback (not a
closure literal — the closure-literal shape is a distinct bug,
see `rust_gg_bug_callable_amp_struct_closure_literal/`).

Simpler variant that WORKS (control): direct `apply(f, &t)` with
no iterator + no Vector unwrap.  See
`tests/fixtures/callable_ref_param.gg` at top-level for the
passing shape (`with_counter(f, &c)` — but no iteration).

Adding `Vector.get().unwrap()` into the shape flips it to SIGSEGV.

## Bug shape / mechanism

Speculative: the `Thing t = xs.get(i).unwrap()` binding may not
produce a durable stack slot lasting through the `f(&t)` call, or
the drop-tracking may free/invalidate `t` too early.  Alternately
the vtable dispatch for the fn-pointer Callable might mismatch the
`Thing &` param calling convention.

## Fix direction (speculative)

Unclear.  Instrumentation of the emitted C around the `f(&t)`
call site would reveal whether:
- The `t` slot's address is stale by the time f is invoked,
- The closure/fn-pointer wrapper mangles the `Thing &` param,
- Some drop-elab decision frees `t` in the loop body pre-call.

## Impact

- R39 Phase 2e Option C helper design (with a `Callable[T(Parser &)]`
  parse-item callback invoked in a while-loop against a Vector-
  hosted parser cursor) was blocked by this bug.
- The fallback (single `bool consume_comma_or_tok(&self, int)` helper,
  no Callable) shipped instead.
