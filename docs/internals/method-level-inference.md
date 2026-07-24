# Method-Level Generic Type Inference

> Design doc, 2026-04-20. Not yet implemented. Highest-leverage item
> blocking the remaining stdlib-design.md Phase 2c work.

## TL;DR

Today, calling a method-level-generic user-space method requires
**explicit type args at every call site**. `v.my_map(double)` doesn't
work; you have to write `v.my_map[int, int(int)](double)`. This makes
"thin Vector wrappers around `iter().map(f).collect()`" a usability
regression vs the existing builtin `v.map(f)` path, so the wrappers
can't replace the builtin and Phase 2c stays half-shipped.

The fix: when typecheck resolves a method whose AST has unresolved
method-level generic params and the call has no explicit `[…]` args,
**infer the params from the actual arg types** and synthesize them
back into the call's AST so the per-call-site mono pipeline picks
them up.

The hardcoded `("Vector", "map")` arm in `infer_closure_method_type`
(`src/semantic/typecheck.rs:2935`) already does this for the builtin
Vector dispatch — **lift the same kernel to the trait-resolution
path** and generalise it across method shapes.

## The Problem (Concrete)

User-space wrapper:

```gorget
equip [T] Vector[T]:
    Vector[U] my_map[U, F](self, F f):
        return self.iter().map[U, F](f).collect()
```

Verified end-to-end on 2026-04-20 with explicit args:

```gorget
Vector[int] result = v.my_map[int, int(int)](double)  # ✓ works
```

Regresses on no-arg form:

```gorget
Vector[int] result = v.my_map(double)
# C output: gorget_array_my_map(__v133, __v135);
# Falls through to the LIR HofExpand path on Vector__T__my_map,
# which has no real body — link error / wrong type.
```

The dispatch path that succeeded for the builtin was
`infer_closure_method_type` → `("Vector", "map")` arm:

```rust
// src/semantic/typecheck.rs:2935
("Vector", "map") => {
    let closure_type = self.infer_expr(&args.first()?.node.value);
    let u_type = self.extract_fn_return_type(closure_type)?;
    Some(self.types.intern_generic(def_id, vec![u_type]))
}
```

That arm is hardcoded to (Vector, map) and bakes in the result shape
(`Vector[U]`). It works because:
1. it's the only thing on the dispatch path that infers from args,
2. it returns the call's result type directly, sidestepping the
   per-call-site mono pipeline (the LIR layer's `HofExpand` does the
   actual code emission).

For user-space wrappers, neither shortcut applies — the typecheck
must infer the method-level generic args AND register a method
instance so the IR emits the specialised body.

## The Three Method Shapes

The investigation identified three structurally different inference
shapes. Each needs distinct rules. **Don't ship one without
explicitly handling the others** — their differences are why a
naïve "unify params" pass doesn't work.

### Shape 1: Predicate (single generic, fully constrained by args)

```gorget
bool any[F](&self, F pred)
Option[T] find[F](&self, F pred)
void for_each[F](&self, F f)
```

`F` is the only method-level generic and appears as a sig param.
**Inference rule:** bind `F` from the corresponding arg's type.
`v.iter().any(is_even)` ⇒ `F = bool(int)`.

The return type contains no method-level generics (it's `bool` /
`Option[T]` / `void` where T is the trait param). No further
inference needed.

This is the easy case. ~40% of method-generic methods in the iter
trait fit this shape.

### Shape 2: Map (return type depends on closure return)

```gorget
Vector[U] map[U, F](self, F f)
Vector[U] flat_map[U, F](self, F f)
Vector[U] filter_map[U, F](self, F f)
MapIter[Self, T, U, F] map[U, F](self, F f)         # default-method form
```

`F` appears as a sig param. `U` appears only in the return type.
The semantic relationship `U = F.return_type` is **a body-level
constraint** (the body calls `f(x)` and propagates the result), not
expressible in the sig.

**Inference rule:** bind `F` from arg type; then if `F` resolves to
a function type AND there's a leftover method-generic `U` that only
appears inside `F.return_type`'s slot in the sig, bind `U =
F.return_type`. Pattern-match the structural relationship; don't
require an explicit declaration.

Detection: walk the sig structure; for each method-generic param `G`
that's NOT bound by direct arg matching, look for a callable param
`F` such that `G` appears in `F`'s return-type position. If found,
bind `G = F.return_type`. If multiple candidates, error (ambiguous).

This is the load-bearing case for Phase 2c. Without it,
`v.map(f)` / `v.flat_map(f)` / `v.filter_map(f)` all force explicit
args.

### Shape 3: Fold (init binds accumulator)

```gorget
A fold[A, F](self, A init, F f)        # init: A, f: A(A, T)
T reduce[F](self, F f)                  # f: T(T, T)
```

`fold` has two method-generics: `A` and `F`. `A` appears as `init`'s
type AND in `F`'s param/return slots. The simpler binding: infer `A`
from the `init` arg, then bind `F` from the closure arg.

`reduce` has just `F`; bind from arg.

**Inference rule:** for each method-generic `G`, look for a
non-callable sig param of type `G` and bind from the corresponding
arg type FIRST. Then re-run the shape-1 / shape-2 rules on remaining
unresolved generics.

Order matters: bind concrete args (init: A) before structural args
(closure F that mentions A in its sig).

## Where It Slots In

Three coordinated changes:

### 1. typecheck.rs `MethodCall` arm (~150 lines)

Today (`src/semantic/typecheck.rs:1346`):

```rust
if let Some((def_id, sig)) = self.traits.resolve_method(...) {
    self.method_resolutions.insert(...);
    let sig = sig.clone();
    // unify args against sig.params, return sig.return_type
}
```

Change: after `resolve_method` returns the sig, look up the
method's AST `FunctionDef` (already accessible via the trait
registry — `EquipInfo.methods` stores `(DefId, FunctionSig)`; need
to also store or re-fetch the AST for the method). Check
`method.generic_params`. If non-empty AND the call's
`generic_args` is `None` or empty:

1. Walk `sig.params` zipped with `args`; build a binding
   `Map<DefId of generic param → resolved TypeId>` per the rules
   above.
2. If all method-generic params bind, substitute into `sig.params`
   and `sig.return_type`. Use the substituted versions for the
   arg-unify loop and the result type.
3. Synthesize the inferred type args back into the AST node's
   `generic_args` (or thread them through a side-table keyed on
   `expr.span`) so the generic collector picks them up downstream.

If inference fails (some generic stays unbound or ambiguous),
fall through to the existing dispatch (which may then hit the
builtin path or error).

### 2. Generic collector method-instance discovery (~50 lines)

`src/ir/lowering/generics/mod.rs:996` `try_register_method_instance`
walks `Expr::MethodCall` nodes with `generic_args: Some([...])` and
registers a `MethodInstance` for per-call-site mono. It currently
ignores calls with `generic_args: None`.

Change: also consult the typecheck-populated side-table (or
post-inference-mutated AST) so calls with inferred type args also
get registered. The downstream `lower_method_instance` is unchanged
— it just needs the args to be present somewhere.

### 3. AST or side-table plumbing (~50 lines)

The cleanest path is mutating the AST `MethodCall.generic_args`
from `None` to `Some(inferred_types)`. Risk: AST is shared across
passes, and other consumers may assume `generic_args == None` means
"not method-generic".

Alternative: a new side-table on the typecheck output, keyed on
`span.start`, mapping to the inferred type args. The generic
collector reads from it. Cleaner but adds another piece to thread.

**Recommendation:** start with the side-table. Audit who else reads
`generic_args` first; if no surprises, fold to AST mutation in a
follow-up.

## Test Plan

Pair every shipping commit with a fixture that exercises the new
inference at the call site **without explicit type args**.

### Shape 1 (predicate)
```gorget
# tests/fixtures/iter_predicate_inference.gg
from std.iter import VectorIter
int main():
    Vector[int] v = [1, 2, 3, 4, 5]
    print(v.iter().any(is_even))                      # F inferred
    print(v.iter().find(is_even))                     # F inferred
    return 0
```

### Shape 2 (map)
```gorget
# tests/fixtures/iter_map_inference.gg + Vector wrapper consumer
from std.iter import VectorIter
equip [T] Vector[T]:
    Vector[U] map_v2[U, F](self, F f):
        return self.iter().map[U, F](f).collect()    # WITH inference, drop [U,F]
int main():
    Vector[int] v = [1, 2, 3]
    Vector[int] doubled = v.map_v2(double)            # inferred
    Vector[String] labels = v.map_v2(int_to_label)    # U=String
    return 0
```

### Shape 3 (fold)
```gorget
# tests/fixtures/iter_fold_inference.gg
int main():
    Vector[int] v = [1, 2, 3, 4]
    print(v.iter().fold(0, sum))                      # A inferred from init
    print(v.iter().fold(1.0, mul_int_into_float))     # A=float, U=float
    return 0
```

### Self-host bootstrap MUST stay green

The Phase 2c trait-bounded-defaults work (commit `139f8170`)
documented why adding bound-implicit defaults regressed self-host.
Inference doesn't touch bounds, but it touches the same
default-method emission path. After implementing, run the full
integration suite (especially `self_host_bootstrap`) before
declaring done.

## What This Does NOT Solve

- **Per-method trait-bound declarations.** `min` / `max` / `sum`
  with `T: Comparable` need bound syntax + emission-skip logic;
  inference alone doesn't help. Separate plan.
- **Self substitution in trait sigs at call site.** Adapter
  constructor defaults (`TakeIter[Self, T] take(self, n)`) need
  Self → equipping_type substitution that's orthogonal to method-
  level generic inference. Adjacent work (might share the
  substitution kernel — see "Substitution Kernel" below).

  **SHIPPED 2026-04-21.** Two commits:

  - `2f9a5d01` — typecheck + IR sig registration infrastructure.
  - `86999a37` — adapter-constructor defaults lifted onto
    `Iterator[T]`; demand-driven discovery + emission gating so
    the mono cost stays linear.

  Layered plumbing:

  1. **Typecheck** (`src/semantic/typecheck.rs`,
     `src/semantic/traits.rs`). `TraitInfo` carries
     `trait_generic_params: Vec<String>` +
     `default_method_sigs: FxHashMap<String, DefaultMethodSig>`
     (AST-level return/param types for default-bodied methods).
     `EquipInfo` carries the impl's AST-level `self_type_ast` +
     `impl_generic_params`. On a trait-default hit in
     `resolve_method` / `resolve_method_by_name`, typecheck walks
     the impl's self_type AST against the receiver's concrete type
     (`bind_template_generics`) to bind impl locals, substitutes
     trait generic args, binds `Self → receiver`, and rebuilds an
     owned `FunctionSig` with the substituted AST resolved back to
     TypeIds. Name-based trait default resolution runs before
     `try_iterator_adapter_type` so real defaults win over the
     hardcoded Vector-adapter shortcut when the receiver actually
     implements `Iterator[T]`. `substitute_ast_type` extended to
     treat `Self` as a bindable placeholder.

  2. **IR sig registration**
     (`generics/mod.rs::register_equip_sigs_with_defaults`,
     `register_method_instance_sigs`). `("Self",
     substituted_equipped)` added to the subs driving
     `substitute_and_map_mut` for default-method sig registration,
     so fn_sigs holds concrete adapter return types (e.g.
     `TakeIter__VectorIter__int64_t__int64_t`) rather than the
     `TakeIter__unknown__...` shape that Self-unsubstituted
     mangling used to produce.

  3. **IR body lowering**
     (`functions.rs::lower_method_instance`). Method-level-generic
     default bodies reach `lower_method_instance` via
     `find_default_trait_method`; Self is now bound in the subs so
     body pre-substitution hits concrete types before mangling.
     `try_register_method_instance` in the generic collector
     similarly binds Self in its scan subs.

  4. **Demand-driven instance discovery**
     (`generics/mod.rs::try_register_default_return_type` +
     `walk_expr_for_method_calls`). For every non-method-generic
     MethodCall whose method resolves to a trait default, infer
     the receiver's AST type, substitute `Self → receiver` + impl
     locals, and `scan_type` the substituted return so
     `TakeIter[VectorIter[int], int]` registers as a struct
     instance ahead of IR lowering. Only covers the DIRECT
     return-type nominal — newly-registered instances' own trait
     defaults stay dormant. This guard prevents the earlier
     `TakeIter[TakeIter[..], int]` infinite cascade (an earlier
     `discover_transitive`-based attempt did that eagerly and hung
     the compiler).

  5. **Demand-gated bulk emission**
     (`functions.rs::lower_generic_equip_methods_with_defaults` +
     `all_return_nominals_registered`). Before emitting a default
     method for an instance, substitute its return type against
     the equip + Self subs and verify every nominal it mentions is
     already registered. If any is missing → skip. Without this,
     lifting `.take()` to a default emits
     `TakeIter__X__take` for every iterator X the collector has
     seen, and each body references `TakeIter[X, ...]` which
     needs registration, and so on — a dead-code cascade that
     trips the GIR validator.

  6. **Rewrite-pass fallback**
     (`semantic/rewrite.rs::rewrite_struct_calls`). When the
     resolution map has no entry for a callee identifier, fall
     back to `scopes.lookup(name)`. Trait default-method bodies
     aren't walked by the resolver today, so without this fallback
     `TakeIter[Self, T](self, n)` inside an `Iterator[T]` default
     stays an `Expr::Call` and lowers as a call to
     `TakeIter__VectorIter__int64_t__int64_t(...)` (undefined at
     link time) instead of a struct literal that emits field-by-
     field init.

  7. **Chain inference through defaults**
     (`generics/mod.rs::infer_expr_ast_type`). When the equip
     block doesn't define the method, fall back to the trait
     default. Without this, `v.iter().take(n).filter(p)` can't
     infer `.take(n)`'s return type (`TakeIter[VectorIter[int],
     int]`) and `.filter` dispatches to the wrong receiver — the
     hardcoded `try_iterator_adapter_type` hit fires and returns
     `Vector[error]`.

  What didn't land (and isn't needed for the shipped adapter
  surface): `chain[Other]` / `zip[Other]` on `Iterator[T]`. Those
  need an iterator-generic `Other` parameter that threads the
  other iterator's concrete type through the adapter struct's
  field — a Shape-2-style inference the current method-generic
  path doesn't cover. `chain` + `zip` + `lazy_windows` +
  `lazy_chunks` stay on VectorIter.

  Fixture: `tests/fixtures/iter_chain_past_one_step.gg`
  (`v.iter().take(4).filter(is_even).map(double).collect()`).
  Uses an explicit `Vector[int]` annotation on the `.collect()`
  binding because `auto out = chain.collect()` can't flow the
  element type back through the chain — that gap is tracked under
  "collect-target inference" below.
- **collect-target inference** (`Vector[int] xs = it.collect()`
  picking Vector from the LHS). Different problem — that's
  expected-type plumbing, not arg-type inference. Separate plan.

## Substitution Kernel — Maybe

Both this work and the Self-substitution work want to take a
`FunctionSig` plus a binding `Map<DefId → TypeId>` and produce a
substituted sig. Worth extracting as a shared helper if the
Self-substitution plan reuses ≥80% of the implementation. **Do
NOT speculatively build a unified substituter** — let Self-sub
reveal what wants extracting. If after both ship the duplication
is real, refactor in a third commit.

## Risks and Open Questions

1. **AST sharing.** Mutating `generic_args` from typecheck might
   break passes that read the AST after typecheck. Audit:
   `grep -rn "generic_args" src/ | grep -v "test\|//"`. If anyone
   pattern-matches on `None`, side-table approach is safer.

   **AUDITED 2026-04-21 — clean.** Six post-typecheck consumers of
   `MethodCall.generic_args` exist, all in `src/ir/lowering/`:
   `generics/substitute.rs:246`, `generics/mod.rs:613` (scan_expr),
   `generics/mod.rs:938` (walk for method instance discovery),
   `generics/mod.rs:1193` (chain receiver type inference),
   `exprs/mod.rs:177` (lower-method-call entry), and the
   downstream `exprs/methods.rs:1234` mangled-symbol dispatch.
   Each either ignores `None` (no-op) or treats `Some(targs)` as
   "user-supplied type args" — exactly the right behavior whether
   the args came from the user or from Pass 4.5 inference. The
   borrow checker (`semantic/safety/*`) and resolver
   (`semantic/resolve.rs`) discard the field via `..` so they're
   invisible to the mutation. LIR, BIR, and backend never
   touch it. Pre-typecheck consumers (`semantic/meta.rs`,
   `semantic/derive.rs`, `semantic/rewrite.rs`,
   `semantic/traits.rs`, parser, loader, formatter) run before
   the mutation can happen, so they see the original user-set
   value regardless. **No consumer distinguishes "user wrote no
   args" from "inference filled them in."** A future pass that
   needed that distinction would require a typed marker
   (`enum GenericArgs { Explicit(Vec<...>), Inferred(Vec<...>),
   None }`) — punt until a real consumer demands it.
2. **Inference stability across compilation order.** Generic
   functions are typechecked in module order today. If method-
   level inference depends on the receiver type being inferred,
   chains like `v.iter().my_map(f)` need typecheck to walk
   left-to-right (which it does — confirmed earlier).
3. **Error messages.** When inference fails (ambiguous, unbound),
   the error needs to point at the specific arg / unresolved
   param. Today's `WrongArgCount` / `NoMethodFound` errors won't
   suffice. Add a `SemanticErrorKind::MethodGenericInferenceFailed`
   with the param name and the reason (no candidate, ambiguous, …).

   **PARTIAL — variant added 2026-04-21.**
   `SemanticErrorKind::MethodGenericInferenceFailed { method,
   type_, unresolved, reason }` ships in `src/semantic/errors.rs`
   with a typed Display: "could not infer method-level generic
   `F` for `Vector[int].my_filter` (no callable arg's return type
   matches its slot in the sig); pass it explicitly via
   `my_filter[<types>](...)`". `try_infer_method_targs` populates
   `TypeChecker.inference_failures` (side-table keyed on
   `method.span.start`) with the unresolved param + a short reason
   when inference returns None. The MethodCall dispatch fork's
   NoMethodFound emission site swaps to the typed variant when
   the side-table has an entry.

   Coverage today is intentionally narrow: the swap fires only
   when (a) the method is registered with a method-generic shape,
   (b) inference fails, AND (c) every fallback dispatch path also
   fails (the call hits NoMethodFound). In practice that's rare
   because name-based fallback (`resolve_method_by_name`) often
   succeeds with `error_id` in the sig — silently typing the call
   even though the generic stayed unbound. Broadening the trigger
   (eager warning on every inference failure, OR detecting
   error_id-only return as a signal) is follow-up work; the
   variant + machinery are in place for it.
4. **Performance.** Inference walks sig structure per call. For
   chains 4-5 methods deep this is fine. Worst case: a hot fn body
   with many method-generic calls. Measure if a fixture trips.

   **MEASURED 2026-04-21 — non-issue.** Heaviest HOF-using
   fixtures (`test_option_all.gg` 18 calls, `string_higher_order.gg`
   14, `test_vector_all.gg` 12) compile in 0.10s end-to-end via
   `gg run`. Inference contributes microseconds at most — O(n) per
   call where n is param count (typically 1–2). Per-method
   instance registration is the actual cost driver and that scales
   with call sites regardless of inference. No fixture trips perf
   concerns. Re-measure if a real codebase ever does.

5. **Self-host typechecker parity.** The Gorget-in-Gorget
   typechecker (`tests/fixtures/self_host_typechecker/`) doesn't
   track method-level generic args at all — its `EMethodCall`
   handling at `typecheck.gg:539,839` matches by method name and
   has hardcoded arms for known iter HOFs (`filter`, `map`, `fold`,
   `any`, `each`). The `targs` field on `EMethodCall` is
   destructured but unused. No trait registry / EquipInfo /
   MethodSigShape equivalent exists.

   This stays green via name-based dispatch: self-host doesn't
   need to mangle `Vector__T__filter__GorgetClosure`-style
   symbols because it doesn't lower to per-call-site mono. The
   `type_comparison` / `lowerer_comparison` integration tests are
   diagnostic-only (always pass), so any output divergence
   surfaces as a recorded mismatch, not a failure.

   Real parity work waits until self-host gains a trait registry
   AND per-call-site mono. That's a much bigger lift (touches
   `scope.gg`, `resolve.gg`, `typecheck.gg`, plus a new
   `traits.gg`). Out of scope for this design doc; tracked in
   TODO Phase 2c (h).

6. **F-string interpolation bypass.** Surfaced 2026-04-21 during
   the convenience-wrapper migration: `f"{v.iter().any(p)}"`
   link-fails because IR-lowering re-parses the interp segment
   text, bypassing typecheck, the AST rewriter, and every other
   semantic pass. Bind-to-local is the workaround; the real fix
   ("parse interp segments to real Expr nodes at parse time")
   has its own design doc at
   `docs/internals/fstring-interp-as-expr.md`. Worth tracking
   here because the inference work is the symptom that surfaced
   it, not because inference owns the fix.

## Sequencing After This Lands

1. Replace the BuiltinTypeProtocol Vector HOF arms (`map`, `filter`,
   `fold`, etc.) with user-space wrappers in std.iter that call
   `iter().method().collect()`. The LIR `HofExpand` variants for
   those ops become unreachable; deletion is a follow-up commit
   (per BIR plan §Relationship Summary).
2. Retire the `_iter` free functions in std.iter (`count_iter`,
   `find_iter`, etc.) — callers migrate to `.method()` form.
3. Move on to Self-substitution work, which unblocks adapter
   constructor defaults and adapter chains past one step.

## Files Touched (estimated)

| File | Change | Lines |
|---|---|---|
| `src/semantic/typecheck.rs` | inference logic in MethodCall arm + helper for sig substitution | ~150 |
| `src/semantic/traits.rs` | expose method AST (or at least `generic_params`) from EquipInfo | ~30 |
| `src/ir/lowering/generics/mod.rs` | extend `discover_method_instances` to read inferred args | ~50 |
| `src/semantic/errors.rs` | new `MethodGenericInferenceFailed` variant + display | ~20 |
| `tests/fixtures/iter_*_inference.gg` (new) | shape 1/2/3 fixtures | ~80 |
| `tests/integration.rs` | wire fixtures | ~20 |

Net: ~350 lines new + tests. Sequence as 3-5 commits:

1. Helper: sig substitution against a binding map (no behaviour change).
2. Predicate-shape inference (shape 1) + fixture. Lowest risk,
   exercises the side-table plumbing in isolation.
3. Fold-shape inference (shape 3) + fixture.
4. Map-shape inference (shape 2 — needs structural matching) + fixture.
5. Vector wrapper consumer commit: replace one builtin HOF
   (`each` first — terminal, no return-type-from-closure) with the
   user-space iter-chain wrapper. Verify `HofExpand` variant
   becomes unreachable; defer deletion to a separate cleanup commit.
