# 9. Type inference & checking (Pass 4)

Type checking is the pass that assigns a concrete `TypeId` to every
expression, resolves every method call to a `DefId`, reports type errors
(mismatches, non-exhaustive matches, unsatisfied trait bounds, out-of-range
literals), and — crucially for the backend — **records the per-call-site
type arguments that monomorphization needs**. It lives almost entirely in
`src/semantic/typecheck.rs` (~6900 lines), with the trait/impl lookup
machinery it leans on in `src/semantic/traits.rs` and the two follow-on
lint/sync passes in `src/semantic/lint_suggest_throws.rs` and back in
`typecheck.rs`. It runs as **Pass 4** of semantic analysis, after the trait
registry is built and after struct/enum field types are populated on
`DefInfo`, and before borrow checking (the pass order is in
`src/semantic/mod.rs:234-349`).

> *Verified against the source as of the commit this chapter was written
> against. Every file:line below was re-derived from current source; the
> figures inside the folded internals doc are presumed stale and were not
> copied — re-derive again if they've drifted.*

## What this pass produces

`check_module` (`typecheck.rs:5489`) drives everything and returns a
four-tuple consumed by the orchestrator (`mod.rs:291`):

```rust
// typecheck.rs:5501
) -> (FxHashMap<Span, TypeId>, FxHashMap<usize, DefId>,
      FxHashMap<usize, Vec<Type>>, FxHashMap<usize, Vec<Type>>)
```

1. **`expr_types`** — span → inferred `TypeId`. Populated as a side effect of
   `infer_expr`; downstream consumers (borrow checker, `lint_suggest_throws`,
   IR-lowering's chained-receiver inference) read it instead of re-running
   inference (`typecheck.rs:414`).
2. **`method_resolutions`** — method-call `span.start` → resolved method
   `DefId`. Written at every `resolve_method` hit (`typecheck.rs:1815`) so the
   borrow checker knows which ownership signature a call uses.
3. **`inferred_method_targs`** — method-call `method.span.start` → inferred
   method-level generic type args, for Pass 4.5 (`typecheck.rs:439`).
4. **`inferred_call_targs`** — generic free-function call's callee
   `span.start` → inferred type args, also for Pass 4.5 (`typecheck.rs:446`).

Beyond the tuple, the pass mutates the `ScopeTable`: it writes resolved
`type_id`s onto `DefInfo` for locals, params, consts, and statics, and after
body checking it deep-resolves every def's stored type so codegen sees
concrete types rather than dangling type variables (`typecheck.rs:5513-5521`).

`check_module` runs in two phases: a signature pre-pass
(`register_signatures_recursive`, `typecheck.rs:5507`) so callers can infer
return types of functions declared later, then the body walk
(`check_items_recursive_tc`, `typecheck.rs:5509`).

## Unknown type names: caught here, not at resolve

An *undefined* type name (`Floobar x = 5`, or `u8 n = 2` — `u8` isn't a Gorget
keyword) used to degrade silently to `error_id` and default to `unit`
downstream, so `gg check` reported "OK" and the program ran wrong or hit a C
"void value not ignored" link error. The fix surfaces an `UndefinedName` error —
but **the only sound site to hard-error is the typecheck-pass VarDecl annotation**
(`unknown_named_type`, `types.rs`; raised at the VarDecl site, `typecheck.rs`),
*not* the resolve pass:

- **Resolve runs too early for cross-module forward refs.** A type can be
  unknown on first sight in the resolve pass and defined later in another module;
  the cross-module fixup is return-type-only, so hard-erroring at resolve-pass
  param/throws/extern sites would spuriously reject a legitimate forward ref. By
  the typecheck pass every *defined* type is in scope, so a still-unknown
  `Type::Named` is genuinely undefined.
- **Generic-param timing.** Function parameter types resolve in the collect pass
  *before* the function's generic params are registered in scope, so a
  resolve-pass error would fire on a legit `T` in `fn foo[T](T a)`. The typecheck
  pass reaches in-scope generics via two scope roots — free fns through the
  enclosing `current_fn_scope` ancestor chain, equip blocks through the
  equip-generics list — and a name is unknown only when *both* miss. Equip
  target-implicit generics (`equip X[T]:`, where `T` is never a scope def) are
  suppressed explicitly.

The `types.rs` `ast_type_to_resolved` chokepoint keeps returning
`Ok(error_id)` for the unknown case (zero blast radius to its other callers,
preserves resolve-pass forward-ref tolerance); only the typecheck VarDecl site
opts into the hard error. Unknown type names at *param* and struct/enum *field*
positions remain a tracked follow-up — they need the generic-param-timing
ordering fixed first. (Landed `bd54f223`; see DONE.md.)

## The unifier

The whole inference engine is one function: `unify(a, b, span)`
(`typecheck.rs:697`). It is a standard union-find-style Hindley-Milner
unifier — substitution map `u32 var-id → TypeId` (`typecheck.rs:403`), fresh
vars from a monotonic counter (`fresh_type_var`, `typecheck.rs:501`), an
occurs-check before binding (`occurs_in`, `typecheck.rs:168`; called at
`726`/`733`) — but with a large coercion ladder bolted on, because Gorget is
not pure HM. `unify` resolves both sides, short-circuits on equality, and
then dispatches on the `(ResolvedType, ResolvedType)` pair.

The notable non-HM behaviours, all in `unify`:

- **Error/Never absorb.** `error_id` unifies with anything (returns
  `error_id`); `never_id` unifies with anything (returns the *other* side, so
  a diverging arm composes with a concrete arm) — `typecheck.rs:705-719`.
- **Structural recursion** for `Generic`/`Tuple`/`Array`/`Slice`/`Function`
  with matching arity (`typecheck.rs:740-806`). Function unification also
  rejects mismatched param-ownership vectors as a `TypeMismatch`
  (`typecheck.rs:782-795`).
- **Callable subtyping.** `Callable → MutCallable → ConsumeCallable` coerces
  upward, and bare `Function` types auto-coerce into any callable variant and
  auto-box into `BoxedCallable` (`typecheck.rs:807-867`). This is where a
  function pointer becomes acceptable wherever a closure trait is expected.
- **Safe integer widening only.** Two integer primitives unify iff the found
  type *safely widens* to the expected one (`is_safe_integer_widening`,
  `typecheck.rs:123`); otherwise it emits `UnsafeIntegerConversion`
  (`typecheck.rs:868-886`). Narrowing or sign changes require an explicit
  `as`.
- **Transparent wrappers.** `Ref(T)`, `Owned(T)` unify through to `T`
  (auto-deref / owned-is-transparent, `typecheck.rs:892-909`); the
  shared-wrapper generics `Mutex`/`Shared`/`RWLock` unify through their single
  arg (`typecheck.rs:913-946`); `cstr ↔ String` and `AtomicInt ↔ int` /
  `AtomicBool ↔ bool` coerce (`typecheck.rs:887-891`, `947-985`).
- Anything else with unequal shapes is a `TypeMismatch` (`typecheck.rs:986`).

`unify` is the *only* place `TypeMismatch` is raised for assignment/arg
positions, which is why so many call sites guard it with
`is_auto_propagation_compatible` / `is_result_capture_compatible` (see
below) before calling it — those are the "don't error, this is legal" escape
hatches that `unify` itself doesn't know about.

## Expression inference

`infer_expr` (`typecheck.rs:1003`) is the giant match over `Expr` that
returns a `TypeId` and inserts into `expr_types` along the way. Highlights:

### Literals and the declared-type hint

Integer literals consult `decl_type_hint` (`typecheck.rs:1006`): if a hint of
a sized integer primitive is in scope, the literal **coerces to the hint
type** and its value is range-checked against `int_range` (`typecheck.rs:86`),
emitting `ValueOutOfRange` on overflow (`typecheck.rs:1011-1022`). Otherwise
it is the default `int`. The hint is a single `Option<TypeId>` field
(`typecheck.rs:420`) saved/restored around each typed context — VarDecl with a
declared type (`typecheck.rs:2813`), call args set the hint to the param type
(`typecheck.rs:1401`), collection-literal elements, struct-field init,
assignment targets. This is how `uint8 x = 5` checks the `5` against
`uint8`'s range, and how `f(small_literal)` coerces the literal to the param.

### Throws functions return `Result` at the call boundary

A call to a `throws E` function is **typed as `Result[T, E]`**
(`typecheck.rs:1509-1524`). This is the single rule that makes auto-propagation
type-safe: `int n = throws_fn()` then correctly fails unification
(`Result[int, E]` vs `int`) *unless* the site opts into capture (declared
`Result[T, E]`) or the enclosing function can propagate. Those two escapes are
`is_result_capture_compatible` (`typecheck.rs:4127`) and
`is_auto_propagation_compatible` (`typecheck.rs:3980`), checked at every
VarDecl, assign, and call-arg site before falling through to `unify`
(`typecheck.rs:1415`, `2832`, `2885`). `noreturn` externs type as `Never`
(`typecheck.rs:1507`).

### async / await / spawn

`async fn` exposes `Future[T]` as its return type at call sites — applied at
signature registration (`typecheck.rs:5336-5341`). `Expr::Await`
(`typecheck.rs:2143`) unwraps `Future[T]`/`Task[T]` to `T`, rejects
double-await (`DoubleAwait`) and await-outside-async (`AwaitOutsideAsync`).
`Expr::Spawn` wraps a `Future[T]` (or a bare call's return type) into `Task[T]`
(`typecheck.rs:2175`); `SpawnBlocking` does the same for any call
(`typecheck.rs:2207`).

### Generic free-function calls (and `inferred_call_targs`)

When the callee is a `Function` type, its signature was registered with
`Defined(generic_param_def_id)` placeholders for `T`/`U`/`E`.
`instantiate_generic_params` (`typecheck.rs:3484`) walks the sig and replaces
each unique generic-param `DefId` with a **fresh `Var`, sharing one var across
all positions** so e.g. both `E`s in `Result[T, E] → Result[U, E]` link
(`typecheck.rs:1374-1379`). Args then unify against the instantiated params,
binding the vars.

If the call was generic *and* had no explicit `[T, …]`, the pass reads each
generic param's fresh var back out of the per-call `subst`, deep-resolves it,
and — only if **every** param resolved to a concrete (non-`Var`, non-`Error`)
type that projects back to an AST type via `typeid_to_ast_type`
(`typecheck.rs:4159`) — records the inferred args into `inferred_call_targs`
keyed on the callee span (`typecheck.rs:1453-1503`). Pass 4.5 then writes them
into the AST. Without this, IR-lowering's monomorphizer has no mangled symbol
to dispatch to and link-fails.

## Method resolution and dispatch

The `MethodCall` arm (`typecheck.rs:1721`) is the most intricate part of the
pass. It is a *fork* with several fallbacks, in this order:

1. **Static method on a type name** (`int.parse()`, `float.default()`) via
   `resolve_static_method_type` (`typecheck.rs:1728`, `4695`).
2. **Qualified enum-variant constructor** (`Color.Red()`) — receiver
   identifier resolves to an enum `DefId`, method matches a variant; routes
   through `infer_variant_constructor` (`typecheck.rs:1741-1762`).
3. Otherwise: infer the receiver type, then **method-level generic
   inference** (Pass 2c — see next section), which runs *before* dispatch so
   it fires even when `resolve_method` will miss (`typecheck.rs:1768-1809`).
4. **`traits.resolve_method(receiver_type, name)`** (`typecheck.rs:1812`) —
   the TypeId-keyed lookup. `resolve_method` (`traits.rs:157`) checks inherent
   impls first, then trait-impl overrides, then the first trait whose default
   body covers the method. On a *trait-default* hit, the returned sig has
   `Self` and the trait `T` erased to `error_id`, so the arm rebuilds it
   against the concrete receiver via `substitute_default_method_sig`
   (`typecheck.rs:1827-1833`, helper at `4032`). Args unify against
   `sig.params`; `WrongArgCount` if the counts differ.
5. **Name-based trait-default fallback** (`typecheck.rs:1853-1897`) — for
   generic-template impls (`equip [T] VectorIter[T]:`) whose impl TypeId
   doesn't match the concrete receiver, the default is only reachable by base
   name via `resolve_method_by_name` (`traits.rs:273`). Run *before*
   `infer_closure_method_type` so a real trait default wins over the hardcoded
   iterator-adapter shortcut.
6. **`infer_closure_method_type`** (`typecheck.rs:1900`, `4366`) — the
   hardcoded builtin-HOF table: `Option`/`Result` `.map`/`.and_then`/etc.,
   `Vector`/`Dict`/`Set` `.map`/`.filter`/`.fold`/`.reduce`, and the
   `Iterator[T]` adapter path (`try_iterator_adapter_type`,
   `typecheck.rs:4506`). These shortcut the mono pipeline: they compute the
   result type directly (e.g. `Vector.map` → `Vector[closure-return-type]`)
   and the LIR `HofExpand` does the actual emission.
7. **`builtin_method_type`** (`typecheck.rs:1908`, `4771`) — string/collection
   builtin method return types.
8. **Final name-based fallback**, then `NoMethodFound` — but only for types
   with inherent-only impls and only for non-auto-derivable methods
   (`clone`/`debug`/`display`/`hash` are exempt — they may be synthesized at
   IR time without appearing in any equip block) — `typecheck.rs:1911-1972`.

The order matters and the comments at each site explain *why* a given
fallback must come before the next. The recurring failure mode is a
generic-template impl whose registered self-type `TypeId` doesn't equal the
concrete receiver's, so anything gated on `resolve_method` succeeding gets
skipped — hence the parallel base-name lookups (`resolve_method_shape_by_name`
at `traits.rs:247`, `resolve_method_by_name` at `traits.rs:273`).

## Method-level generic inference (the real Pass 2c)

> **Status note (folded from the former `method-level-inference.md` deep-dive):**
> that doc's header said "Not yet implemented" — it was **stale**. The work shipped.
> `try_infer_method_targs` (`typecheck.rs:4262`) exists, is wired into the
> `MethodCall` arm (`typecheck.rs:1805`), is plumbed through Pass 4.5
> (`apply_inferred_method_targs`, `typecheck.rs:5536`), and the typed failure
> error and its side-table (`inference_failures`, `typecheck.rs:455`) are live.
> The narrative below is re-derived from current source; only the design
> rationale (the three shapes) is lifted from the doc.

The problem the doc states: calling a method-level-generic user method
(`Vector[U] my_map[U, F](self, F f)`) used to require explicit type args at
every call site (`v.my_map[int, int(int)](double)`). The fix infers the
method-level generics from the actual arg types and synthesizes them back into
the call's AST so the per-call-site mono pipeline picks them up — exactly like
explicit args.

The inference kernel is `try_infer_method_targs(shape, arg_types,
method_span_start)` (`typecheck.rs:4262`). It takes a `MethodSigShape`
(`traits.rs:34` — the method's AST-level `generic_params`, `param_types`,
`return_type`, populated only for method-level-generic methods) and the
already-inferred arg types, and binds the generics in two structural passes
matching the doc's shapes:

- **Shape 1 (predicate):** bind a generic `G` when it appears as a bare
  `Type::Named { name: G, generic_args: [] }` directly in a param slot — bind
  it from that arg's resolved type (`typecheck.rs:4278-4293`). Covers
  `any[F]`, `find[F]`, `for_each[F]` and the `init: A` slot of `fold[A, F]`.
- **Shape 2 (map):** for each still-unbound `G` that appears in the *return*
  type, look for exactly one arg that resolved to a `Function`, and bind `G =
  that function's return type` (`typecheck.rs:4295-4332`). This materializes
  the body-level constraint `U = F.return_type` for
  `Vector[U] map[U, F](self, F f)`. Multiple function args with differing
  return types → ambiguous → bail (`typecheck.rs:4316-4322`).

Binding happens in declaration order; each bound `TypeId` is projected back to
an AST `Type` via `typeid_to_ast_type` (`typecheck.rs:4350`). If any param
stays unbound or can't be projected, inference fails — and it records the
unresolved param + a human reason into `inference_failures`
(`typecheck.rs:4334-4358`). That side-table is read at the `NoMethodFound`
emission site, where it swaps in the typed
`SemanticErrorKind::MethodGenericInferenceFailed { method, type_, unresolved,
reason }` (`errors.rs:219`) that points at the specific generic and suggests
passing it explicitly (`typecheck.rs:1946-1966`). Per the doc's risk #3, that
swap fires narrowly — only when every fallback dispatch also fails.

On success, the inferred args go into `inferred_method_targs` keyed on
`method.span.start` (`typecheck.rs:1805-1807`). Note this is *separate* from
the older hardcoded `("Vector", "map")` arm in `infer_closure_method_type`
(`typecheck.rs:4445`), which the doc identified as the original kernel — that
arm still exists for the builtin dispatch; `try_infer_method_targs` is the
generalization lifted onto the shape-lookup path.

## Pass 4.5 — `apply_inferred_method_targs` / `apply_inferred_call_targs`

These two functions (`typecheck.rs:5536` and `5724`) are the **real Pass 4.5**,
invoked from the orchestrator immediately after typecheck
(`mod.rs:307-315`). Each is a full AST walker that visits every `MethodCall`
(resp. `Expr::Call`) and, when the node's `span.start` is a key in the
corresponding `inferred` map **and the node has no explicit args**, sets
`generic_args = Some(<inferred types>)` (method-call set at
`typecheck.rs:5654`, free-call set at `5822`). Explicit user args are
authoritative and never overwritten.

The design point (CLAUDE.md layering discipline, rule 4 "resolve once, write
through"): the entire downstream IR-lowering / generic-collector path reads
`MethodCall.generic_args` **uniformly** — it cannot and does not distinguish
"user wrote `[T1, T2]`" from "Pass 4.5 inferred them". The folded doc's
audit (its risk #1, marked AUDITED-clean) confirmed every post-typecheck
consumer of the field either ignores `None` or treats `Some` as
user-supplied; the borrow checker and resolver discard the field via `..`. So
the side-table-then-AST-mutation approach is safe. The orchestrator skips both
walks entirely when the maps are empty (`mod.rs:309`, `312`).

There is also a **Pass 2.6** sibling, `apply_collect_target_rewrites`
(`typecheck.rs:5896`), which runs *before* typecheck (`mod.rs:266`) and uses
the LHS declared type of a `.collect()` binding to pick the collection target
— a different problem (expected-type plumbing, not arg-type inference).

## Pass 4.6 — `lint:suggest_throws`

`lint_suggest_throws::check_module` (`lint_suggest_throws.rs:50`) runs after
typecheck because it consumes `expr_types` (`mod.rs:324`). It flags functions
returning `Result[T, E]` (and not already `throws`) that contain the verbose
`match Result` rethrow shape — `T x = match expr: case Ok(v): v; case
Error(e): return Error(e)` — and suggests converting to `throws E` +
auto-propagation. It emits a `SuggestThrowsRefactor` *warning*, one per
function, anchored at the function-name span.

It is a deliberate example of the "No name matching" rule
(`lint_suggest_throws.rs:22-33`): `Ok`/`Error` are identified by **`DefId`**
(resolved once via the scope table into `ContextIds`,
`lint_suggest_throws.rs:67-89`), not by string compare; the scrutinee's
`Result`-ness is read from the typed `expr_types` map (which is why typecheck
records the match scrutinee's type at `typecheck.rs:2260`), not from AST shape.
Detection is precision-over-recall — it accepts a few well-known rethrow
shapes and skips anything ambiguous, because false positives are
user-hostile.

## Exhaustiveness checking

Match exhaustiveness is checked in `check_match_exhaustiveness`
(`typecheck.rs:3248`), called from the `Expr::Match` arm
(`typecheck.rs:2275`) and the `Stmt::Match` path. The algorithm:

- Bail if there's an `else` arm or any `MetaFor` item (the latter expands at
  mono time and may cover everything) — `typecheck.rs:3255-3262`.
- Only enum scrutinees are checked; resolve the scrutinee to its enum `DefId`
  and pull the variant list from `enum_variants` (`typecheck.rs:3264-3279`).
- Walk unguarded arms collecting covered variant names via
  `collect_covered_variants` (`typecheck.rs:3305`). Guarded arms are skipped —
  a guard doesn't guarantee coverage (`typecheck.rs:3285`). Wildcard, rest, and
  non-variant bindings set a catch-all flag and short-circuit; `Or` patterns
  recurse; `None`-literal patterns cover the `None` variant.
- Any variant not covered → `NonExhaustiveMatch { missing_variants }`
  (`typecheck.rs:3299-3301`).

Integer / non-enum scrutinees are *not* exhaustiveness-checked (no closed
universe). Pattern-bound variables get their types assigned separately by
`assign_pattern_types` (`typecheck.rs:3355`), which destructures the scrutinee
type through constructor/tuple/or/dot-shorthand patterns and writes field
types onto the binding `DefInfo`s — this is what makes `case Error(e):` give
`e` a real type for interpolation and further use.

## Trait-bound checking

Two entry points, both producing `UnsatisfiedTraitBound`:

- **`check_trait_bounds`** (`typecheck.rs:5149`) — for generic *function*
  calls with explicit type args. Maps each generic param name to its concrete
  type arg, then for each declared bound checks
  `traits.has_trait_impl_by_name(concrete, trait)` (`traits.rs:331`). A bound
  is also satisfied **transitively** if the type arg is itself a generic param
  of the enclosing function carrying a matching-or-super bound
  (`trait_satisfies`, `traits.rs:370`) — `typecheck.rs:5180-5195`.
- **`check_struct_type_bounds`** (`typecheck.rs:5204`) — for struct/enum
  instantiations (`Dict[K: Hashable, V]`), called from VarDecl
  (`typecheck.rs:2810`). Same transitive rule; skips unresolved type-var args
  (they're checked when the outer generic is instantiated,
  `typecheck.rs:5227-5231`).

## Generic monomorphization inputs

Type checking does not monomorphize — but it produces the three inputs the IR
mono pipeline needs:

1. **`inferred_call_targs` / `inferred_method_targs`** synced into the AST by
   Pass 4.5, so every generic call carries explicit `generic_args` regardless
   of whether the user wrote them.
2. **`method_resolutions`** so each call knows its target method `DefId`.
3. **Concrete `DefInfo.type_id`s** after the deep-resolve sweep
   (`typecheck.rs:5513-5521`), so monomorphization sees concrete element types
   rather than dangling type vars.

The actual instance discovery and body specialization happen later in
`src/ir/lowering/generics/`; the folded doc's section "Where It Slots In"
describes that downstream plumbing (e.g. `try_register_method_instance` reading
`generic_args`), which lives outside this pass.

## Imported-module error truncation

`check_items_recursive_tc` (`typecheck.rs:6127`) type-checks imported-module
bodies to populate `expr_types`/`method_resolutions` but **discards their type
errors** — library code can produce false positives in a foreign scope
(unbound generic vars, auto-prop holes). It snapshots `errors.len()` before
recursing into an `Item::Module` and truncates back afterward
(`typecheck.rs:6143-6146`).

The exception: **hard errors survive**. A concrete-vs-concrete type mismatch
at a *call-argument* site in an imported module — one that survived `unify`'s
full coercion ladder with both sides fully concrete (`is_fully_concrete`,
`typecheck.rs:516`) — is mirrored into `checker.hard_errors`
(`typecheck.rs:1434-1448`) and re-appended after the truncate
(`typecheck.rs:6147-6151`). This catches the "silent `to_uint32(float_arg)` in
a 6000-line imported file" class without re-surfacing foreign-scope noise.

## In the self-host

The Gorget-in-Gorget typechecker lives in
`tests/fixtures/self_host_typechecker/` — `typecheck.gg` + `infer.gg` +
`types.gg` + `format_types.gg` + `traits.gg` + `derive.gg` + `meta.gg` +
`driver.gg`. Parity is measured by `type_comparison` (`tests/integration.rs:12997`),
which runs the self-host typechecker over every `tests/fixtures/*.gg` and
diffs its type output against Rust `gg`'s. It is **diagnostic-always-pass**:
it prints `exact / superset / total / mismatched / crashed` counts
(`tests/integration.rs:13145`) but asserts nothing about parity, so a green
`cargo test` says nothing. To read parity:

```bash
cargo test --test integration type_comparison -- --nocapture
```

and read the printed `exact:` + `superset:` counts. (`superset` = the
self-host produced *more* type information than Rust, which the philosophy
treats as acceptable, not a regression.)

The self-host typechecker carries its own **trait registry** mirroring the
Rust one: `TraitRegistry`, `TraitInfo`, `EquipInfo`, `FunctionSig`,
`MethodSigShape` all live in `types.gg` (`from types import FunctionSig,
MethodSigShape, TraitInfo, EquipInfo` at `traits.gg:25`), and the registry
rides on the `TypeTable` as `trait_registry` (`types.gg:147`). `traits.gg`
(~850 lines) owns the builder + resolver: `new_trait_registry`
(`traits.gg:41`), `build_method_sig_shape` (`traits.gg:133`),
`resolve_method_full` / `resolve_method_by_name` / `resolve_method_shape`,
and the method-level inference kernel `infer_method_targs(MethodSigShape
shape, …)` (`traits.gg:806`).

That kernel performs the **same Shape-1 / Shape-2 inference** as the Rust
`try_infer_method_targs` (`traits.gg:6-8` documents both): Shape-1 binds a
generic `G` when it appears bare as a param type; Shape-2 binds `G` when it
appears in the return type and a function-typed arg's return type fills it
in. It is wired into the `EMethodCall` return-type path: at `infer.gg:525`
the inferer calls `resolve_method_shape`, then `infer_method_targs` over the
arg types, then `substitute_return_type` to type the method call's result
from the inferred generics. So the self-host **does** infer method-level
generics and **does** use them — to compute return types.

Two things still diverge from Rust gg:

1. **Residual name-based HOF arms coexist with the registry path.** The
   `EMethodCall` arm (`typecheck.gg:695`) first calls the registry-driven
   `resolve_method_full` (`typecheck.gg:705`, falling back to
   `resolve_method_by_name` at `717`) to type closure args, but a hardcoded
   name table for the known iterator ops
   (`filter`/`any`/`all`/`each`/`map`/`fold`/`reduce`) still sits alongside it
   at `typecheck.gg:723-737`. These are not yet folded into the registry
   lookup — a self-host cleanup, not a parity blocker.
2. **No threading of inferred args back into the AST.** Rust gg synthesizes
   the inferred targs into `MethodCall.generic_args` (Pass 4.5) so the
   per-call-site mono pipeline can mangle a
   `Vector__T__filter__GorgetClosure`-style symbol. The self-host typechecker
   is typecheck-only — it has no backend to feed and never lowers to
   per-call-site mono — so it infers the targs purely to type the return and
   never writes them back. There is no `apply_inferred_method_targs` analogue,
   and that is correct for its role.

Generic *free-function* call inference is likewise present
(`typecheck.gg:869-910` builds a `var_id → concrete type` map and substitutes
it into the return type).

### The primitive-receiver reject

The ladder that types builtin methods answers from the method NAME, and that
is only sound once the RECEIVER has had its say. A non-String primitive has a
very short method table — `hash`, `debug`, `display`, `mod`, plus the
auto-derivable `clone` — so `infer.gg` gates the whole ladder on the receiver's
resolved `RTPrimitive` before any name arm runs: a numeric or `bool` receiver
gets those five answers and `NO_TYPE` for everything else, and never reaches
the String or collection arms below. Anything the gate has no answer for is
then REFUSED by `typecheck.gg::reject_no_method_on_primitive`, the self-host
mirror of Rust's `E_NoMethodFound` chokepoint — lower-or-reject, rather than
minting a `Str` slot for a numeric payload.

That reject is a sibling of `reject_wrong_receiver_combinator` rather than a
tenth arm of it: an ABSENT method is a different class from a method on the
wrong side of a two-sided pair, and the combinator function's arm set is a
ratified nine.

Its admission test is deliberately weaker than Rust's, and the reason is worth
knowing before tightening it. Rust dispatches on the receiver's OWN table; the
self-host consults the UNION of every primitive's table, plus a registry-wide
name check, so it refuses only a name that is *nobody's* method. The self-host
resolves some identifier receivers to the wrong definition — its resolution map
is keyed on `span.start`, and both `match`-arm bindings and f-string
interpolations can make that key lie — so a per-receiver-kind table refuses
valid programs. Corroborating the type through lexical scope lookup catches the
f-string half but not the other, because both avenues read the same map. The
bound is measured, filed with its repro, and tightens on its own once the
resolution defect is fixed.
