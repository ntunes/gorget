# Rust gg bug — closure struct capture doesn't persist mutations

**Discovered:** R39 Phase 2e Sub-task 0 probe (2026-08-09) while probing
alternative HOF-callback designs for the trailing-comma-cascade helper.

**Fixture:** `repro.gg` (this directory).
**Integration test:** `rust_gg_bug_closure_struct_capture_no_persist` in
`tests/integration.rs` (currently `#[ignore]`d; un-ignore when fixed).
**TODO entry:** `TODO.md` under `## Compiler / Rust gg`.

## Repro

```
./target/release/gg run tests/fixtures/known_gaps/rust_gg_bug_closure_struct_capture_no_persist/repro.gg
```

## Observed vs intended

- **Observed:** prints `1\n1\n1`.
- **Intended:** prints `1\n2\n3` — three successive `step()` calls
  should each observe the previous mutation to `c.value` through the
  captured `Counter c` local.

The `closures.gg` fixture (top-level) confirms that mutable capture
works for `int count = 0; auto increment = (): count = count + 1;`
(prints `3` after 3 calls).  The bug is specifically with STRUCT
captures + method-with-`&self` mutation: successive calls appear to
mutate a per-call clone of the captured `Counter` rather than a
shared reference to the local.

## Bug shape / mechanism

Unknown without instrumentation.  The closure body compiles to
some `__Closure_N__call(this)` function.  The captured `c` is
stored somewhere in the closure's environment struct; the question
is whether it's stored by-value (cloned per call) or by-reference
(shared).  Given that the primitive-int capture works, the bug
appears specific to struct-typed captures where `&self`-taking
methods are invoked.

## Fix direction (speculative)

Struct captures inside a closure that will be invoked more than
once (or invoked via `Callable[T()]` indirection) MUST be stored
by-reference in the closure environment, not cloned.  Or if
by-value copies are required for CoW-consistency, the mutation
must write back through the closure's environment slot.

## Impact

- R39 Phase 2e alternative design (Callable[T()] + captured Parser)
  was ruled infeasible because of this bug — the closure would need
  to invoke `parser.match_tok(TOK_COMMA)` (a `&self` method that
  mutates `parser.pos`) N times and expect the position to advance;
  under this bug, each call would see the same starting pos.
