# Rust gg bug — generic mono skipped at parser.gg scale, cc type-mismatch downstream

**Discovered:** R39 Phase 2e Sub-task 0 probe (2026-08-09) attempting to
add a `parse_comma_separated_list[T]` helper to
`tests/fixtures/self_host_typechecker/parser.gg` (~5000 LOC).

**Fixture:** `repro.gg` (this directory) — SEED ONLY, does not
reproduce in isolation.  The DURABLE REPRO is the procedure below.
**Integration test:** `rust_gg_bug_generic_mono_parser_scale`
in `tests/integration.rs` (currently `#[ignore]`d; asserts the
observed failure signature until minimized).
**TODO entry:** `TODO.md` under `## Compiler / Rust gg`.

## Repro (the procedure — the isolated file does NOT trip it)

1. Check out HEAD `bc21127d5` (R39 Phase 2e fix landed).
2. Revert the migration inside `tests/fixtures/self_host_typechecker/parser.gg`
   BUT keep the helper.  I.e., replace the deployed
   `bool consume_comma_or_tok(&self, int terminator)` helper with:

   ```gorget
   Vector[T] parse_comma_separated_list[T](Parser &self, int terminator, Callable[T(Parser &)] parse_item):
       Vector[T] items = []
       if self.check_tok(terminator):
           return items
       items.push(parse_item(&self))
       while self.match_tok(TOK_COMMA):
           if self.check_tok(terminator):
               break
           items.push(parse_item(&self))
       return items
   ```

   and migrate ONE call site (e.g. `parse_type_args`) to:

   ```gorget
   Vector[SpannedType] args = parse_comma_separated_list(&self, TOK_RBRACKET, (p): p.parse_type_with_ownership())
   ```

3. Rebuild: `rm -f tests/fixtures/self_host_lowerer/driver.c tests/fixtures/self_host_lowerer/driver;
   ./target/release/gg build tests/fixtures/self_host_lowerer/driver.gg`

4. Observed: `cc` fails with
   ```
   tests/fixtures/self_host_lowerer/driver.c:<LINE>: error: incompatible types
   when assigning to type 'GorgetArray' from type 'int64_t'
   ```
   The `parse_comma_separated_list` emission in `driver.c` is a
   SINGLE declaration `int64_t parse_comma_separated_list(void*, int64_t, __gg___Closure_0)`
   with NO definition — the T=SpannedType monomorphization was
   NOT generated.  The call site at the failing line was
   `__v10 = parse_comma_separated_list(...);` with `__v10` typed
   as `GorgetArray` (the array-return slot for `Vector[SpannedType]`).

## Isolated file behavior

Running `./target/release/gg run repro.gg` (this dir) produces
`0` — which is the RUNTIME manifestation of the DIFFERENT bug
`rust_gg_bug_callable_amp_struct_closure_literal` (untyped closure
`(p): p.pos` infers `p` as `int64_t`).  The SCALE-DEPENDENT
monomorphization skip does NOT trigger here — small file, few
generics.

## Bug shape — speculative

The mono-walker may bail on the SpannedType instantiation because:
- The concrete `T=SpannedType` is only reachable through a closure
  literal's inferred body type (see the related closure-inference
  bug), which the mono walker doesn't consult; OR
- A generic-fn population step's ordering assumption breaks when
  the file has thousands of other definitions (function count,
  const count, or the equip-block count exceeds an internal
  threshold); OR
- The declaration is emitted from a different code path than the
  definition (declaration from a forward-decl pass, definition
  from a body-walk pass), and one of those passes misses the T.

## Fix direction (speculative)

Instrument `src/backend/c_lir/emit_types.rs` and
`src/monomorph.rs` (or equivalent) to log every generic-fn
instantiation request + emitted definition; find the missing pair
at parser.gg scale.  If the issue is closure-body inference
side-stepping the walker, extend the walker to follow closure
env types.

## Impact

- R39 Phase 2e Option C helper design (owner-chosen) was blocked
  by this bug and its 3 sibling bugs (see the other
  `rust_gg_bug_*` known_gaps entries).
- The fallback (single `bool consume_comma_or_tok(&self, int)`
  boolean helper — no generics, no Callable) shipped instead.
