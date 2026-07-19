# Chapter 11 — Copy-on-write & view provenance

This chapter describes how Gorget gives every value Rust-like single-ownership
semantics *without* a borrow lattice in the IR: by defaulting every assignment,
argument pass, and collection read to a zero-cost **borrow** (a `Ptr` alias),
and inserting a **clone** only at the handful of points where a borrowed value
crosses into something that must *own* it. There is no `provenance.rs` and no
standalone provenance pass — the machinery lives in the GIR lowering context
(`src/ir/lowering/context.rs`), the assignment/method lowering that calls into
it (`src/ir/lowering/stmts/assigns.rs`, `src/ir/lowering/exprs/methods.rs`), the
C runtime's resource-struct layout (`src/backend/c/c_runtime.rs`), and a
separate *diagnostic* borrow-origin tracker in the safety pass
(`src/semantic/safety/origins.rs`). The user-facing contract is in `CLAUDE.md`
("Ownership at Consuming Positions") and `docs/language-reference.md`; this
chapter is the implementation.

The one-line model: **everything is a reference until ownership is demanded.**
If a value is only ever read, no clone ever happens — and where ownership is
demanded by a *mutation*, the clone is deferred to the mutation itself, so a
mutation path that never executes never pays for a copy ([§ Full lazy
materialization](#full-lazy-materialization-37--the-lazy-cow-default), the
default in both compilers). The decisions are all made
at compile time during lowering — no reference counting, no runtime ownership
checks (the runtime's only CoW participation is the cheap `cap==0` view check
described in [§ The view discriminator](#the-view-discriminator-cap0)).

## Two layers, one model

CoW is enforced at two layers that must not be conflated:

1. **GIR lowering** (`src/ir/lowering/`) does the real work: it tags each local
   with an ownership state, propagates borrows at zero cost, and emits clone
   calls at ownership boundaries. This is where the move-vs-clone-vs-borrow
   decision is made and where `Drop`/`MoveZero` instructions are inserted.

2. **The safety pass** (`src/semantic/safety/`) runs *earlier* and is purely
   diagnostic for CoW purposes: it tracks `BorrowOrigin`
   (`src/semantic/safety/mod.rs:68`) for lifetime errors (use-after-move,
   return-of-local-reference, borrow-across-await) and emits
   `MoveWithoutOperator` for the single-owner-by-design carve-outs
   (`src/semantic/safety/check_stmt.rs:1217`). It does *not* decide where clones
   go. Note that `BorrowOrigin` exists as **two unrelated types** with the same
   name: the safety one above, and the IR one
   (`crate::ir::BorrowOrigin`, used inside `LocalOwnership` — see
   [§ Local ownership state](#local-ownership-state-the-tag-on-every-local)).

## CoW default: borrow everywhere

At every non-boundary position — bare-identifier assignment (`String b = a`),
ordinary function-call arguments, match scrutinees, collection reads
(`.get()`, `v[i]`) — the lowering propagates a `Ptr` alias at zero cost. No
clone, no move, no runtime work. The aliasing relationship is recorded in the
source local's `LocalOwnership` (see below) so that a *later* mutation can sever
it.

Resource types are **never** memcpy'd by value across a function boundary; bare
resource params are passed as a const pointer (`ParamABI::ByPtr`,
`src/ir/lowering/context.rs:33`) and the callee receives a borrow. This is a
correctness requirement, not an optimization: a by-value struct copy would
create two owners of one heap buffer and double-free at drop.

The exceptions are the **single-owner-by-design** types, where a silent alias
would be a semantic bug. For these, the safety pass forces the user to write
`!source` or `source.clone()` at a bare-assign site by emitting
`MoveWithoutOperator`. The exact carve-out set is in
`src/semantic/safety/check_stmt.rs:1217`:

```rust
let needs_explicit_move = match resolved {
    ResolvedType::Function { .. }
    | ResolvedType::CallableTrait(_)
    | ResolvedType::MutCallableTrait(_)
    | ResolvedType::ConsumeCallableTrait(_)
    | ResolvedType::BoxedCallable { .. }
    | ResolvedType::Owned(_) => true,
    ResolvedType::Generic(def_id, _) => {
        matches!(self.scopes.get_def(def_id).name.as_str(),
            "Box" | "Task" | "TaskGroup" | "Guard")
    }
    _ => false,
};
```

That is: closures / `Callable[T]` (all three flavours) / `BoxedCallable`,
`Owned[T]`, and the named generics `Box`, `Task`, `TaskGroup`, `Guard`. Copy
types (`is_copy_type`, checked at `check_stmt.rs:1211`) and the owned `String`
type short-circuit before this point and never require `!`.

## Local ownership state (the tag on every local)

Each GIR local carries a `LocalOwnership` (a typed field on `Local`, read back
through `source_ownership`, `src/ir/lowering/context.rs:1077`). This is the
single source of truth for "does this local own its data?" — the legacy
`func_state.local_ownership` sidecar map was retired (Phase D4.5; see the note
at `context.rs:2502`). The variants that matter for CoW are `Owned`,
`Borrowed { origin, mutability }`, `View { source }`, and `SharedHeap { source }`.

The `origin` inside `Borrowed`/`View` is `crate::ir::BorrowOrigin` (distinct
from the safety one). The relevant variants, used through the helper queries:

- `Alias(LocalId)` — a CoW alias (`String b = a`); resolved transitively to the
  root via `cow_resolve_root` (`context.rs:2562`).
- `CollectionElement(LocalId)` / `FieldPath(String)` — an element borrowed out
  of a collection (`v.get(i)`) or a field-path collection (`self.data.get(i)`),
  identified by `CollectionId` (`context.rs:19`). Recognised by `is_cow_borrow`
  (`context.rs:2384`).
- `Field { base, .. }` — a borrow of a struct field (`String x = imp.field`).
- `RuntimeView(LocalId)` — a `cap==0` string view returned by a view method,
  set by `set_view_of` (`context.rs:2399`).
- `CowBorrowPending` — a placeholder before the source collection is resolved.

`flush_ownership_to_locals` (`context.rs:2512`) derives each local's `slot_kind`
(`BorrowedPtr` / `OwnedPtr` / `Value`) from `(type, ownership)` at the GIR→LIR
boundary so the backend never re-derives ownership from names or shapes.

## The two enforcement helpers

All clone-at-boundary logic funnels through **two** methods on `LoweringContext`
(`src/ir/lowering/context.rs`). This replaces the old per-site ad-hoc clone
emission; per the layering doctrine, the decision lives in one place and the
diagnostic (`warn_implicit_clone`) fires from the same chokepoint.

### `ensure_owned_at_boundary` (`context.rs:1792`)

Unconditional "clone if this is any kind of borrow." Used at boundaries that
have **no concept of last-use** — the function body is leaving the value behind
regardless. Its decision tree:

- **`Constant::GlobalRef(name)`** for a resource-typed module global → clone
  through `GlobalRefPtr` (the LIR `GlobalAddr+Load` is a shallow struct copy
  that aliases the global's heap buffer). Skipped for
  `string_literal_view_globals` — those are immortal `.rodata` `cap==0` views,
  so the consumer's drop is a no-op (`context.rs:1823`).
- **`Ptr(T)`** → clone the pointee via `clone_fn_for_ptr(T)`. Cannot move
  through a `Ptr` (the callee can't know whether the caller still needs it); the
  param is recorded via `record_param_cloned` so the caller can later suggest
  `!` at last-use sites (`context.rs:1868`).
- **By-value resource that is a borrow** (`is_ref_local || is_bare_param ||
  is_cow_borrow`, or an `Untracked` resource) → clone via `clone_fn_for_ptr`
  (`context.rs:1901`). One carve-out: a *last-use* bare-param borrow that is
  drop-tracked moves instead of cloning (`context.rs:1915`).
- **Owned drop-tracked locals and non-resource locals** → pass through unchanged.

### `ensure_owned_at_consuming_arg` (`context.rs:1963`)

Last-use-*aware* "clone if borrow OR not last-use." Used at consuming positions
where the caller *might* still use the local after the call, so the helper takes
the AST argument expression to call `is_last_use_at(name, span)`
(`context.rs:1043`) on named-local identifiers. Its rule:

1. `Ptr(T)` borrow → clone through the pointer (`context.rs:1977`) — **except an
   *owning* `!` resource param** (recorded via `is_owning_param` / `set_owning_param`,
   the caller transferred ownership) at its **last use**, non-string, single-use:
   that **MOVES** — `set_owned` + `move_zero_and_mark` on the param pointer slot
   (`context.rs:~2217`, gorget-arena snag #1). This restores the `!`=move=zero-cost
   contract: a `!` param is owned, so putting it into a collection / returning it /
   passing it to another consuming position transfers rather than copies. (The
   explicit-`!` push `out.push(!item)` routes through `consuming_clone_temps`
   `methods.rs:~2681`, guarded identically by `is_owning_param_ptr`.) The `is_single_use`
   gate is conservative — a param reassigned in a loop (`lhs = f(!lhs)`) must NOT move.
2. By-value resource:
   - non-identifier expression (a temp, owning by construction) → no clone, the
     caller will `MoveZero` after the call (`context.rs:2029`);
   - named local that is a borrow (**bare** param / ref-state / cow-borrow) → clone
     (a bare param is a *borrow*; the caller keeps ownership, so an owning
     destination must be handed a copy — contrast the owning `!` param in rule 1);
   - named local **not** at its last use → clone (source still live);
   - last-use, drop-tracked, owned named local → no clone (caller `MoveZero`s).

Both helpers emit the clone via `clone_fn_for_ptr(T)` (`context.rs:1687`), which
reads the type's clone-fn name off its `TypeDef`
(`clone_fn_name_for_def`) — resolving to `gorget_string_clone_to_owned`,
`gorget_array_clone`, `gorget_map_clone`, `gorget_set_clone`, or a
compiler-generated `{Type}__clone`. No name matching: the runtime symbol comes
from typed registry metadata, never a substring test on the type name.

### Why two helpers

`ensure_owned_at_boundary` has no last-use hint because its sites are
unconditional leave-behinds (return, tuple/struct field init, match-arm escape,
closure capture; enum field init is the same shape but goes through the
`emit_enum_init_owned` sibling). At a consuming-position arg the caller may keep
using the local, so the last-use check is what distinguishes "transfer
ownership" from "clone and keep."

## Materialization points — the enforced boundary set

> **Finding (re-derived 2026-05-29 from current source; reframed 2026-05-30).**
> Several docs carry a numbered list of "materialization points": the former
> internals doc (since folded into this chapter) and the authoritative spec
> `docs/language-reference.md` historically enumerated **SEVEN**; `CLAUDE.md`
> and `feedback_cow_design_clarity.md` said **SIX**. The disagreement was not
> that the list was *garbage* — it is the validator-enforced ownership-boundary
> set, just historically **under-listed**. Materialization boundaries are
> exactly the consuming positions the validator guards: `validate_consume_sites`
> (`src/ir/validate.rs`) classifies each via `ConsumeSiteClass`, and every
> implicit clone is emitted tagged with an `ImplicitCloneReason` (`src/ir/mod.rs`)
> — those two enums are the source of truth. At the lowering level the clone is
> driven structurally by the two helpers above (called from ~16 sites), so the
> live call-site grep below is the operational inventory; the completed,
> human-readable boundary list lives in the spec
> (`docs/language-reference.md` §9.6, kept in sync with the two enums). A fixed
> hard-coded count in a doc drifts; the enums don't.

Every implicit clone now carries its `ImplicitCloneReason` not just in the
side-car diagnostic but **on the emitted instruction** — a typed
`Instruction::Call.reason: Option<ImplicitCloneReason>`, stamped at the producer
through the single `emit_clone` / `call_clone` chokepoint and asserted by an
env-gated ratchet (`GG_VALIDATE_CLONE_REASONS`, always-on strict in debug: no
compiler-emitted clone may reach the backend untagged). That turns the reason
enum into the **planner's directive table**: it is the per-clone annotation
(WHICH boundary demanded this clone — consuming arg, struct-field init, return,
at-site CoW, or a loop-pre-header hoist, split out as its own
`LoopPreHeaderMaterialize` so once-per-loop costs are distinguishable from
per-iteration ones); a future materialization planner reads the same field as an
instruction to CHOOSE a strategy per boundary (clone here, hoist there, elide
where liveness proves it safe). See devbook/24 Rule 1 for why the fact rides the
instruction rather than a name-keyed side table, and the "GIR-only today" scope
note (it is dropped at GIR→LIR until a LIR consumer needs it).

**The explicit table has LANDED (planner round 3): `MaterializePlan`.** The
per-function `MaterializePlan` (a `Vec<MaterializeDirective>` on the
`LoweringContext`, reset per function through `clear_locals` — the universal
per-function-body reset every lowering entry funnels through) is the reason enum's
directive table made concrete: each `MaterializeDirective { root: LocalId, reason:
ImplicitCloneReason, position: MaterializePosition }` records WHICH root to break
the alias on, WHY (the cost tag stamped on the emitted clone), and at WHICH
`MaterializePosition` — `AtSite { mutation }`, `LoopPreHeader { anchor }`, or
`BranchPreHeader { anchor }`, keyed by the applied span (matching the self-host
lane's `cow_scope_muts` "anchor@name" keying — Core #9). Every materialize routes
through the ONE apply funnel `apply_materialize_directive` →
`cow_before_mutation_planned` (which owns the lone `.cow_before_mutation(` call and
stamps the directive's reason), so all three positions are genuinely constructed
and recorded: the at-site class via `plan_materialize_at_site`, the loop and branch
pre-header consumers via their directives. Today the table is populated + recorded
(observability/costing + the future planner's working set) and applied
unconditionally; the planner that READS it back to choose hoist/elide strategies is
the campaign's next phase. **Honest scope:** the *decision* (which severs fire) is
data-dependent on live Phase-D ownership, so the directive ENCODES root+reason+
position and the apply funnel READS live ownership at apply time (`cow_before_mutation`)
— mirroring the SH split (`cow_scope_muts` records WHICH root statically;
`cow_materialize_root_by_name` executes against live ownership). devbook/24 rule 4
"resolve once, write through": the prescan/site resolves WHICH root, the funnel
writes the clone, downstream never re-derives.

**The campaign's acceptance bar — the CoW charter (owner 2026-07-19).** Implicit
clones must be as good as the best hand-written clones would have been; cloning
more than absolutely necessary is the model missing its own charter
(`language-design.md` §3.1 — and the README's public promise, "as if you'd
written every copy by hand", which makes charter-suspect volume a gap between
the shipped README and reality). Operationally: every `ImplicitCloneReason` is either
**charter-justified** (a real ownership boundary a hand-writing author also pays —
a consuming position on a live source, a closure capture, a genuine write to a
shared root) or **charter-suspect** (a clone the hand-written version would replace
with a borrow — `VarDeclFromBorrow`/`ReturnFromBorrow` on read-only use, defensive
`&`-formation for a read-only callee). The charter meter is charter-suspect volume
trending to ~0 (per-reason budgets, filed); rounds are judged against the
hand-written ideal — not against the previous release, and not against the other
lane (the lane ratio is a different measurement; both lanes can be above the bar
together).

Live call-site inventory (re-derive with the grep, do not transcribe — these
move):

`ensure_owned_at_boundary` (unconditional-leave-behind boundaries):

| Site | Boundary |
|------|----------|
| `functions.rs:824,1099,1374,1767` | `return` value (`Ptr→T` auto-clone) |
| `exprs/mod.rs:480` | tuple-literal field init (`Expr::TupleLiteral` in `lower_expr_inner`) |
| `exprs/mod.rs:1950` | struct field init (`lower_struct_literal`) |
| `exprs/mod.rs:2538` | match-arm value escaping an arm |
| `closures.rs:319,559` | closure capture |

Enum-variant init is **not** in this table: enum constructors route their
borrow-clone through a dedicated helper, `emit_enum_init_owned` (called at
`exprs/mod.rs:1409,1420,1478,1510,1536`), rather than `ensure_owned_at_boundary`.

`ensure_owned_at_consuming_arg` (last-use-aware consuming positions):

| Site | Boundary |
|------|----------|
| `exprs/methods.rs:1915` | `push`/`put`/`set`/`add`/`extend`/`send`/`insert`/`push_back`/`push_front` |
| `exprs/methods.rs:116` | `Box.new(value)` / `Box(value)` |
| `stmts/assigns.rs:822,837,839` | `v[i]=x` / `d[k]=v` index-assign sugar |
| `stmts/assigns.rs:636` | **field store** `self.field = x` (`clone_ptr_rhs_if_needed`) |
| `exprs/calls.rs:612` | bare-name `Box(value)` constructor consuming value arg (sibling of the `Box.new(value)` site at `exprs/methods.rs:116`) |

Note one specific drift from the internals doc: it claims field store (its
"point 7") is handled by a bespoke `Ptr`-detect-and-clone inside
`lower_field_assign`. In current source the field-store RHS routes through the
**shared** `ensure_owned_at_consuming_arg` helper
(`clone_ptr_rhs_if_needed`, `assigns.rs:629`); the bespoke path described in the
doc is gone (it was unified 2026-05-05 per the comment at `assigns.rs:623`).

`var_decl` is deliberately *not* in either list: a typed binding
(`String x = expr`) defaults to **borrow** like everything else and clones only
on later mutation — the internals doc's "point 1 (assignment)" is the CoW
default-borrow path now, not a clone site.

## For-loop elements: borrow the element, don't clone it

`for x in vec:` is a collection read, so by the CoW default the loop variable
`x` is a borrow alias into the collection, not a fresh per-iteration clone. The
element is read in place; a copy is made only if the body carries it past an
owning boundary. This holds uniformly across element types — a string element
reads as a zero-copy `cap==0` view, and a recursive-drop struct or enum element
reads through a pointer alias. The distinction matters on a hot read-only loop:
the body that only inspects its elements pays one deep clone per iteration if
the element is copied, versus none if it is borrowed. On the self-host
self-compile — where the compiler repeatedly walks large `LirFunction`
collections — that per-iteration clone is the dominant allocation cost, so
borrowing the element rather than copying it is what keeps the self-compile
cheap.

`lower_for_array` (`src/ir/lowering/stmts/for_loops.rs:397`) binds such an
element as a `Ptr(elem)` borrow alias instead. The gate is fully typed — no
name matching:

```rust
let is_recursive_struct = !is_string
    && elem_is_struct_or_enum                       // TypeDefKind::Struct | Enum
    && ctx.type_registry.is_resource_type(elem_type)
    && !ctx.type_registry.is_collection_type(elem_type);
let elem_is_binding = matches!(pattern.node, Pattern::Binding(_));
```

(`for_loops.rs:458-471`). When it fires, the element is read through
`index_load_borrow` (which returns the raw element pointer on a `Ptr`-typed dst,
`src/lir/lower/insts.rs:1014-1024`) and tagged `set_cow_borrow`
(`context.rs:2397`), with **no** `drops.register_local` — the collection owns the
buffer, so a per-element drop would double-free it. The element's
`BorrowOrigin` is `CollectionElement` / `CowBorrowPending`, recognised
downstream by `is_cow_borrow` (`context.rs:2420`). Tuple-destructuring patterns
and direct-collection elements keep the old clone path.

**Why this is sound.** The loop body can only do three things with `x`, and each
is already handled:

- **Read through it.** `x.field` auto-derefs the `Ptr` base (`FieldLoad`'s
  `is_ref_local` skip); enum tag/payload reads (`r.is_ok()`, `r.unwrap()`,
  match scrutinees) resolve through a `Ptr` base too — see the enum extension
  below. No copy needed.
- **Carry it past an owning boundary** (`out.push(x)`, `return Some(x)`, struct
  field init, closure capture). Every such boundary deep-clones a `Ptr` source
  *unconditionally*: `ensure_owned_at_boundary` Case 1 (`context.rs:1854`) and
  `ensure_owned_at_consuming_arg` rule 1 (`context.rs:1963`) both clone through
  the pointer via `clone_fn_for_ptr`. This is the exact mechanism that makes a
  borrowed `Vector[T]` parameter safe — the for-element alias is no different.
- **Consume it** (`consume(!x)`). Statically rejected by the safety pass:
  `check_move` (`src/semantic/safety/origins.rs:495-502`) emits `MoveInLoop` for
  a `!`-move of a non-loop-local inside a loop body, and the for-pattern binding
  is *not* a loop-local — it carries the iterable's borrow origin
  (`check_stmt.rs:736-741`). So the one shape that would alias a moved-out
  element away from the collection can't be written.

The clone-on-boundary apparatus is therefore the safety net; eliding the
*per-iteration* clone just defers each clone to the (often-not-taken) boundary,
the same lazy-CoW logic as everywhere else in this chapter.

### Enum elements: the `build_enum_recv_ptr` carve-out

Extending the alias from structs to **enums** (Option / Result / user enums)
needed one more piece, because the Option/Result builtins
(`__option_is_some` / `__option_unwrap` / `__result_unwrap_error`, …) take their
receiver *by pointer* and the default path `emit_borrow`s the receiver place to
get its address. For a value receiver that is correct; for a for-element that is
*already* a `Ptr(enum)` it would produce `Ptr(Ptr(enum))` — a double indirection
that mis-reads the tag — and it would invalidate the source (`Move` +
`drops.unregister` + `move_zero_and_mark`), zeroing a collection element.

`build_enum_recv_ptr` (`src/ir/lowering/exprs/methods.rs:63`) is the chokepoint
that distinguishes the two. The carve-out is, again, fully typed:

```rust
let is_collection_borrow = recv_is_ptr
    && place.projections.is_empty()
    && ctx.is_cow_borrow(builder, place.local);
```

(`methods.rs:77-79`). When true it passes the pointer **through** (the slot
already holds the `Ptr(enum)`) and returns an `is_collection_borrow` flag that
tells the four enum-extern call sites (`methods.rs:733,887,1256`, and the
`unwrap_or`/`unwrap` pair at `:794`/`:824`) to skip the `Move` signal and the
`move_zero_and_mark`. Match scrutinees need no migration: `TagOf` /
`EnumFieldLoad` already auto-deref a `Ptr` base (`resolve_struct_id` peels the
`Ptr`; `EnumFieldLoad`'s Ptr-base `Load`, `src/lir/lower/insts.rs:1315-1323`).

The one shape deliberately **excluded** from the carve-out is a `Field`-origin
borrow (`w.name.unwrap()` — a struct-field Option). Its underlying struct *is*
owned and *will* drop, so the unwrap must keep its source-invalidating
behaviour — that is what prevents a double-free when a struct-field Option is
unwrapped (guarded by `test_option_resource_field`). The discriminator is `place.projections.is_empty()
&& is_cow_borrow` — a field borrow has a non-empty projection and is not in the
`is_cow_borrow` set, so it falls to the default `emit_borrow` + invalidate path.

### Per-loop-kind status

| Loop kind | Element handling | Status |
|-----------|------------------|--------|
| `for x in array` — string element | `index_load_borrow` zero-copy `cap==0` view | borrow |
| `for x in array` — recursive struct element | `Ptr(elem)` alias, no clone, no drop reg | borrow |
| `for x in array` — enum element (Option/Result/user) | `Ptr(elem)` alias + `build_enum_recv_ptr` | borrow |
| `for (i, x) in array.enumerate()` — recursive struct/enum element | `Ptr(elem)` alias, same gate as the plain array loop | borrow |
| `for x in array` — tuple-destructure / direct-collection element | clone | eager |
| `for k, v in dict` | out-param accessors write resource-cloned, drop-registered locals | eager (`lower_for_dict`) |
| `for x in set` | out-param accessor writes a resource-cloned, drop-registered local | eager (`lower_for_set`) |

The dict and set loops still materialize an owned per-iteration copy: their
`gorget_map_iter_key`/`_value` out-params hand back a clone, registered for drop
so the per-iteration value does not leak. Carrying the borrow alias to those
siblings — behind one shared element-binding helper, so a new loop kind cannot
silently fall back to cloning — is the natural next step.

### Read mode is a layer invariant

The borrow alias rests on a layering rule (see
[Chapter 24](24-layering-discipline.md)). The `ReadMode::Borrow` set on the
`IndexLoad` at the GIR layer (`for_loops.rs`) is a typed invariant — *this
element is a view, do not clone it* — and the LIR reader is obliged to honour it
for every element type, not just strings. The hazard is a
narrower-than-necessary reader: if the collection-element lowering
(`src/lir/lower/insts.rs`) honours the borrow only when the clone function is the
string one, a recursive-drop struct element falls through to the `{Type}__clone`
arm regardless of the read mode the producer set — a typed invariant silently
dropped at the layer boundary. The alias is therefore bound at the GIR producer,
where the read mode is known, and the LIR reader returns the raw element pointer
for a `Ptr`-typed destination; the general form of that reader — honouring
`ReadMode::Borrow` for any recursive-drop element — is the shape the boundary is
meant to take.

## Mutation severs the alias: `cow_before_mutation`

**The rule this section implements (the spec).** A resource value is a
borrow until a write reaches it; the write **materializes** the value
(copy-on-write) at the closest context where it is immutable, and the
write lands on that private copy. Write-through to the original happens
only along an unbroken chain of `&` (mutable) access to a real owner —
the instant that chain hits an immutable binding (a bare local, a bare
parameter, a bare alias, a `for x in coll` element) the copy is taken
*there* and the original is left untouched. This is deliberately **more
tolerant than Rust**, which rejects a mutation through an immutable
borrow; Gorget copies instead. The user-facing statement is in
`docs/language-design.md` §3 and `docs/language-reference.md` §9.6; the
mechanism is `cow_before_mutation` and its materialize routines below.
The current enforcement is only partial — see
[§ Implementation status](#implementation-status--what-materializes-today-and-the-gaps).

A borrow is only as cheap as the absence of a write. When the lowering is about
to mutate a local — a mutating method call (`exprs/methods.rs:1590,1623`), a
reassignment, an index/field assign (`assigns.rs:71,489,730`) — it first calls
`cow_before_mutation(local, span)` (`context.rs:2658`) to clone out everything
that aliases the value being mutated, so each alias keeps the value it observed.
The cases it severs:

- **bare Ptr param** → clone to owned in place (Case at `context.rs:2665`);
- **local is an alias** → clone the source into the local (`Case 1`);
- **local is itself an element/field borrow being mutated in place** →
  materialize it into an independent owned copy first, so the mutation lands in
  the local's own buffer and the source collection/struct is left untouched
  (`Case 1b`, via `cow_materialize_collection_ref`). Symmetric to `Case 3`
  (which severs when the *collection* is mutated) — this is the "the element ref
  is the thing being mutated" direction, e.g. `x.bump()` on a
  `T x = coll.get(i).unwrap()` bind;
- **local is a source with aliases** → clone into each alias (`Case 2`);
- **local is a collection with element refs** → materialize each ref
  (`Case 3`, via `cow_materialize_collection_ref`);
- **local is a string with live views** → materialize each view (`Case 4`,
  recursing through transitive views, `context.rs:2710`);
- **SharedHeap value-aliases** → drop the tag only (heap was already deep-owned
  at the `gorget_string_copy_cow` boundary, `Case 5`);
- **struct with live named field-borrows** → materialize each (`Case 6`).

The three materialize routines (`cow_materialize_alias` `:2872`,
`cow_materialize_view` `:2833`, `cow_materialize_collection_ref` `:2910`) all
share the shape: call `clone_fn_for_ptr`, then bind the cloned value into a
fresh owned local with `AssignMode::Move` (not Copy — Copy would alias the clone
and leak the original; see the comment at `context.rs:2853`), register it for
drop, and rebind the name. The Move-mode detail was a real bug fix: the earlier
shallow-copy variant produced a Phase-C validator violation that a now-removed
named-local guard was masking.

`cow_before_field_mutation` (`context.rs:2765`) is the field-path sibling
(`self.data.push(x)` materializes `CollectionRef`s borrowing `"self.data"`), and
`cow_sever_all_aliases_from` (`context.rs:2782`) handles the reassign case
(aliases keep the *old* value).

### Implementation status — converging to the uniform rule

The single rule above is the **design target**, and the implementation is
**converging** to it — one uniform materialize-at-mutation chokepoint, not
`cow_before_mutation` extended shape-by-shape. Today `cow_before_mutation`
enforces the rule for the **tracked-source** cases (the `is_bare_param`
direct-param clone and the `Alias`/`CollectionElement`/`FieldPath`/
`RuntimeView` origins off `LocalOwnership`), **plus** the bare-rooted
**projected mutation** shapes (`v[i].field = x`, `v.field.push()`,
`v[i].method()` through a bare param — G1, landed `d1b1744a`, round-33)
**and** the bare-rooted **`&`-of-value FORMATION** shapes (`&t` / `&self`
on a bare value, and projected `f(&s.field)` / `auto r = &s.field` — G2,
landed `2c7fbf04`, round-34).

The former "one remaining unconverged shape — **untracked alias chains**"
(`&x.slice()[i]`, a mutation whose root is a view-returning method that
`resolve_projection_root_local` cannot name) is **CLOSED** (planner round 3, the
2D audit). The filed premise "it still writes through to a live source" was
**REFUTED** by an end-to-end sweep (4 view-returning methods × field/index/
compound, C + LLVM): NO reachable shape corrupts a live source. The disposition of
every unnameable-projection-root mutation is one of three, none a silent
write-through:

- **Reject** — the only shape that would reach a live source is a String
  index-assign (`base.slice(0,3)[i] = x`); a String slice is a read-only codepoint
  view, so this hard-rejects with `E_StringIndexAssign` (both lanes — pinned by
  `string_slice_index_assign_error.gg` + `self_host_driver_rejects_string_slice_index_assign`).
  This satisfies Core #10 (lower-or-reject) for the corruption path.
- **Dead-temp accept** — a container-returning method (`slice` on a `Vector`,
  `view_of(x)`, …) returns an OWNED copy (the `ReturnFromBorrow` boundary), so the
  write lands on a private temp no one reads; the live source is provably unchanged
  (`view_of(x)[0] = 999` prints the source's original `10`, C + LLVM). These are
  **ACCEPTED today and ruled onto the WARNING track**: the existing dead-write warn
  class ("writes to a private copy that is never read — did you mean `&o`?") is the
  precedent, and extending that warning to method-rooted dead temps is a filed
  **D2-rider** follow-up (a convergence path, not a miscompile). Pinned by
  `known_gaps/cow_2d_method_result_dead_write.gg`.
- **ICE-file** — two pre-existing panics on this surface are filed separately (not
  2D write-through): `push_char` on a String-view `&` binding (`emit_types.rs:850`,
  the String-view mutating-arg ABI) and a nested index-assign rooted at
  `windows`/`chunks` (`assigns.rs:1100`/`:1809`, "index-assign found no setter").
  See `known_gaps/string_view_push_char_ice.gg` /
  `known_gaps/vector_windows_nested_index_assign_ice.gg`.

The reference-grade posture is therefore **reject-not-resolve**: the plan pass does
NOT invent a resolver to name the view's source (there is no live aliasing source to
materialize). A RESOLVE path (extend the root oracle via the existing View-provenance
`cow_borrow_source` machinery) is only needed IF a view-returning method on a
*mutable* container is added; until then, reject + dead-temp-warn is the close. So
`cow_before_mutation`'s presence may now be read as "the uniform materialize rule is
enforced" — no unconverged silent-write-through class remains.

### Write-through place for an index element — value elements too

Distinct from the materialize-at-mutation above — which fires when the *root*
of the store is a bare/immutable value — is the write-lvalue for a field-store
*into an owned collection's element*: `v[i].field = x` must resolve the ADDRESS
of `v[i]` and write `.field` through it, or the store lands in a throwaway copy
and is silently lost. `try_resolve_field_place`'s `Expr::Index` arm forces a
write-through `Ptr` into the element for this — and, since round-39 (`24efcf53`),
it does so for a **value-type** element (`struct Point`) exactly as it already
did for a resource element, closing a value-vs-resource asymmetry: previously
`lower_index_access` returned a value COPY for a value elem (write-through was
reserved for resource elems), so `v[0].x = 99` dropped the write on *both*
backends — a reference miscompile, symmetric across local, `static`, compound,
and nested roots. The arm is `CollectionKind::Array`-gated (a `Dict`/`Set`
element's write-through field-lvalue is a separate key-typed path, still filed),
and it hoists the round-33 CoW untrack across its callers (including the
compound-index arm and the `methods.rs` method-receiver sibling) so the
transient element ref cannot be left CoW-tracked into a dangling Case-3 clone.

### Remaining increments (campaign state 2026-07-17)

The uniform rule (§3.1: a resource is a borrow until a write; the write
write-throughs along an unbroken `&` chain to a real owner, or materializes a
private copy at the immutable binding where the chain breaks) is a **closed
set** of positions, and the campaign to make every one reference-grade on
**both** production lanes (Rust gg → C/LLVM) **and** the self-host is tracked
increment by increment. This subsection is the durable map of what is settled
and what remains; live status stays in `TODO.md` (the "CoW WAVE 2" queue).

**Settled (wave 1 — the place write-through class).** The three write-through
gaps that silently dropped element stores are closed on both lanes, each pinned
by cross-lane fixtures whose expected output is **ggdef-adjudicated when the
shape is in ggdef's phase-0 subset, otherwise derived from §3.1/D1** — never
copied from "whatever production printed", because pinning production's wrong
output would lock a Core-#8 both-wrong bug as canonical:

- Value-type `v[i].field = x` on a Vector element (the self-host mirror of the
  round-39 Rust fix) writes through instead of losing the store.
- `for x in &coll` element mutation write-throughs; the bare `for x in coll`
  twin correctly materializes a private copy per element; the comprehension
  `[e for x in &coll]` **read** yields the real elements. These share one
  mode-driven iterable helper per lane (bare = borrow element, `&` = MutPtr
  write-through place) rather than parallel per-form fixes.
- Dict `d[k].field = x` write-throughs with the collection producer evaluated
  exactly once (no double-eval of a side-effecting `make()[k].field = x`). The
  HashMap element-typing analog is a separate, still-open upstream bug.

**Remaining (wave 2 — materialize completeness).** These positions still either
write through where the chain is broken (should materialize) or are undecided;
each is its own scout → brief → gauntlet track, and any track that **flips or
adds** a fixture expectation runs the **full ggdef suite** in its gates (a
new top-level `cow_*`/`deadwrite_*` fixture is harvested by the `corpus_b`/`b1`
ggdef lanes, so it is a definition-lane event, not just a runtime one):

| Increment | Position | Lanes | Disposition |
|-----------|----------|-------|-------------|
| **2T** ✅ LANDED | drop-tainted value at **any** materialize-on-write site | both + ggdef | **Ruled REJECT** (owner 2026-07-17); **landed all lanes 2026-07-17**. Materialize is an implicit copy, so a custom-`Drop` value must not be silently duplicated here any more than at the six consuming positions — the user writes `&self`/`&param` (write-through) or an explicit `.clone()`/move. Emits the `E_MoveWithoutOperator` family (the `write_through_available` discriminator leads with the `&self`/`&<param>` write-through remedy) at every materialize position — **assign, compound-assign, mutating-builtin receiver, AND the `&`-of-value FORMATION arg** (`f(&s.field)`, whole `&p` / `&self`; the last was the wave-2 Core-#8 double-close fix). Negative fixtures `cow_taint_*`; decoupled from the dead-write LINT's tracking (guard `tests/lints.rs::tainted_reject_never_reads_lint_state`). |
| **2E** ✅ LANDED | plain `self` (not `&self`) | both | **Landed all lanes 2026-07-17.** Bare `self` ≡ bare-param materialize (value struct → pointee deref-copy; resource → clone; the self-host tags plain non-scalar self `LoParam` so `cow_materialize_projected_root` privatises it); `&self` write-throughs. Shipped **in the same landing** as the ratified D2-rider **dead-bare-param-write** diagnostic (uniform over all bare params — `self` is just the first — flagging exactly the write-to-a-never-read private copy: *"this writes to a private copy that is never read — the caller's value is unchanged; did you mean `&self`?"*, an on-by-default `W_` promoted to `E_` after corpus burn-down; the read hooks cover the Identifier, f-string interpolation, AND SelfExpr read paths). Gated behind 2T for drop-tainted receivers. |
| **2G** ✅ LANDED (both lanes, 2026-07-18 — Rust + the self-host mirror in the same round) | loop-carried bare-param materialize | both | The root-caused **deadwrite while-loop wrong-code** class: a bare-param CoW write inside a loop threw away its private copy every iteration — the materialize-on-first-write rebind happens inside `lower_block(body)`, but the loop's `restore_locals` reverts it each iteration AND the condition/exit blocks resolve the pre-loop param-borrow slot — so `while i < 2: xs.pop()` printed 4 instead of 2 and `while xs.len() > 2: xs.pop()` **infinite-looped** (the condition re-read the stale borrow). Fixed at the **write** site: a shared pre-header helper (`materialize_loop_carried_bare_params`) runs at every loop-lowering entry (`lower_while` *before* the condition is lowered and before `save_locals`; `lower_loop`; `lower_for` before the iterable) and calls the **existing** `cow_before_mutation` for each in-scope bare param the loop mutates. The fresh owned local is now a pre-loop slot that LIR-SSA phis at the header (the same loop-carried substrate `emit_lazy_loopcarried_borrow` relies on), and the rebind is captured by `save_locals`. **Eager here is observationally lazy**: a bare param's private copy starts equal to the caller's bytes, so a pre-header clone is indistinguishable from clone-at-first-write, and it only fires when the body *statically* mutates the param — over-approximation costs one extra clone, under-approximation reviving the per-iteration throwaway. This is a **write**-site fix, never phi-repair at the loop head. Detection is routed through the **shared CoW prescan** collectors (`cow_after_block`/`cow_after_stmt`/`cow_after_expr_moves`, run fresh over the loop's own statements + the `while` condition) — one source of truth (devbook/24), no parallel AST walker — and the same routing hardened the prescan program-wide: the mutating-method check reads the typed `is_mutating_builtin_method` predicate (not a drift-prone hand-list), and the collectors now see index/tuple-field roots (with the dotless-root insert), the loop/if/match condition + scrutinee, non-block match-arm bodies, the nested-For `else` body, `select` channel ops, and place-projection sub-expressions. The self-host lane carries the equivalent pre-header materialize in its own loop lowering (a mirror of this design over its whole-function CoW scan, not a mechanical port). The **branch-body** sibling (the same save/restore shape *outside* loops) is closed by **consumer #1** (row below). Still open: the **comprehension emitters** (a *different* root — the synthesized list/string/dict/set loops have NO save/restore, so the in-body clone lands in the re-executing body; it needs a synthesized-loop pre-header hook, not the scope-dispatch hoist) and the user `&self`-mutator receiver that hides from the untyped prescan — each filed with a `known_gaps/` fixture asserting the intended output. |
| **Consumer #1** ✅ LANDED (both lanes, 2026-07-18 — Rust + the self-host mirror in the same round) | branch/scope-carried bare-param materialize (the **planner campaign's first consumer**) | both | The **branch sibling of 2G**, generalized to the FULL non-loop save/restore scope class (Core #4: fix the class, not the instance). A bare-param mutation inside ANY of `if`/elif/else · `unsafe` · `with` · named-scope · `match` arms (bodies + guards) · `select` recv arms — plus a `for … else:` / `while … else:` **else** body — was thrown away exactly as the pre-fix loop body was: the scope's per-branch/per-arm `restore_locals` (or `lower_block_scoped`'s save/restore) reverts the in-body CoW rebind, and the post-scope read resolves the stale pre-scope param-borrow (`cow_loop_bare_param_if_branch` printed 4,4; ggdef-adjudicated 3,4 — a **Core-#8 both-production-backends-wrong** miscompile). Fixed at the **write** site, hoisting BEFORE the scope: the six non-loop scope forms materialize at their `lower_stmt` **dispatch arm** through ONE shared entry (`materialize_scope_carried_bare_params`, stamping `BranchPreHeaderMaterialize`); the two loop-**else** rows ride the EXISTING `materialize_loop_carried_bare_params` hoist (extended to also scan the else body, keeping `LoopPreHeaderMaterialize` so per-position costing stays honest). Conditional scopes hoist to the dominating pre-scope point (fresh owned local dominates the merge → no phi); the straight-line scopes (with/unsafe/named-scope, single predecessor) materialize at entry — **never phi-repair at a merge** (devbook/24). Detection is the SAME shared collector as 2G: `cow_mutations_in_stmt` **is** `cow_after_stmt` run over the whole scope statement (NOT a hand-mirror of its per-form arms — the proto's `cow_mutations_in_branches` had already drifted by missing the elif conditions; that partial parallel walker is deleted), so elif/if conditions, match guards, `with` bindings, arm-Expr-vs-Block bodies, else bodies and nested scopes are all covered as *properties of the collector*, one source of truth (devbook/24). The elif-CONDITION sub-shape mattered twice over: the SH's `cow_scan_stmts`/`cow_scan_expr` If arms had the SAME drift (branch bodies walked, elif conditions skipped — match guards were already scanned), fixed in the same round; and on the Rust side the old at-site materialize for an elif-cond `&arg` rebound in the conditionally-reached else-chain block — a NON-dominating rebind that made the then-taken path read an undef local (base printed 0,0,4 on both backends). `cow_scope_bare_param_elif_cond_then` pins the fix at 0,4,4 and `cow_scope_bare_param_elif_cond` pins the elif-taken 1,3,4 — both all-lane-agreed, ggdef-adjudicated. The `is_bare_param` gate keeps `&`/`!` write-through intact (regression fixture `cow_scope_bare_param_amp_guard` → 3,3) and makes nested composition safe (a loop pre-header hoist already materialized the param → the inner branch hoist sees an owned local and no-ops — `cow_scope_bare_param_nested_if_loop` → 2,4, no double clone). The self-host mirror is SUBTREE-scoped like the Rust side, via **one-pass scope sets** (bootstrap-hotfix 2026-07-18, a two-layer regression fix measured on the stage-1 bootstrap: (1) read-only String builtins missing from `BUILTIN_METHOD_MUTATES` (`starts_with` et al.) were conservatively classified mutating and the first mirror's whole-fn suffix scan turned that into an entry clone per call in hot classifier helpers — stage1→stage2 blew the 600s deadline at 1332s; (2) stack-sampling then exposed the dominant residual: the SH typechecker's bare `ScopeTable` params are mutated via USER `&self` methods that the SH's TYPED scan marks but Rust's untyped prescan is blind to — the R38/user-mutator shared gap — so the SH branch hoist drew a deep `ScopeTable__clone` per recursive `check_safety_stmt` visit, 7× runtime clones, while Rust-lowered stage-0 never fired). Design: the whole-fn scan captures per-scope-statement subtree mutation sets DURING its single forward walk (`cow_scope_anchor_stack` + flat `"anchor@name"` keys in `cow_scope_muts` — never a per-scope re-walk, which multiplies scan cost by nesting depth, and never a nested Dict); the `SIf`/`SMatch`/`SNamedScope` dispatch hoists are pure lookups through the ONE shared funnel (`materialize_carried_bare_params_core`, single `cow_materialize_root_by_name` site — SH ratchet stays 8; anchors: cond start / scrutinee start / `stmts_first_pos(body)`, one shared helper on both keying sites). **Lane-symmetry filter:** scope-set marks pass `rust_prescan_marks_method` (typed builtin `is_mutating` UNION the mirrored `RUST_PRESCAN_MUTATOR_FALLBACK` list), so both lanes' branch hoists fire on IDENTICAL shapes; user-`&self`-mutator / unknown-method receiver marks stay in the whole-fn maps only (bind-flip safety + loop hoist keep the SH's richer typed detection), and the user-mutator-in-scope throwaway remains the SHARED filed gap on both lanes (the `cow_loop_bare_param_user_mutator` class) — when the Rust prescan learns typed user receivers, both lanes lift together and the mirrored fallback list is deleted on both sides. The read-only String surface (`starts_with`/`ends_with`/`byte_at`/`split`/…) is now typed into `BUILTIN_METHOD_MUTATES` (`replace` deliberately kept conservative — it sits in Rust's fallback for unresolvable user methods). The LOOP hoist keeps the whole-fn suffix scan (a loop re-executes; at/after-the-anchor is the right question there — and the 2G-era suffix-scan asymmetry note now applies to loops only). SH `with` has no name-map snapshot (no hole), and the SH lowerer has no `unsafe`/`select` arm (out of the self-host subset). Measured close: base 1487ms/4.96M array clones vs fixed 1546ms/6.38M on the dataframe A/B; both bootstrap tests green solo (fixed_point 867s, bootstrap 597s, each stage within its 600s deadline). Ratchet is **FLAT** (Rust 20, SH 8 — additive coverage routed through the existing funnels, no new `cow_before_mutation` site); the convergence-meter *decrease* is a Phase-3 at-site-conversion property. Fixtures: in-subset ggdef-adjudicated shapes top-level (`cow_loop_bare_param_if_branch` + `cow_scope_bare_param_{if_else,if_both,if_elif,nested_if_loop,amp_guard,match_arm}`); out-of-ggdef-subset shapes in `known_gaps/` validated on C+LLVM (`cow_scope_bare_param_{unsafe,named,while_else,for_else,match_guard,with,select}`). The explicit per-function `MaterializePlan` **table** (keyed by applied span, typed `MaterializePosition`) **LANDED planner round 3** with its first at-site querying client, the Class-A assign-target-root conversion (design-unified-slotprovenance: substrate built with a real consumer). Consumer #1's two pre-header hoists now route through it as `LoopPreHeader` / `BranchPreHeader` directives (see the Class-A row below). |
| **Class A** ✅ LANDED (Rust, planner round 3) | assign-target-root at-site materialize → the `MaterializePlan` | Rust (SH already consolidated) | The **first at-site client** of the table: `lower_field_assign` / `lower_index_assign` / the compound path funnelled their six open-coded `cow_before_mutation` calls through the shared `materialize_assign_target_root` → `plan_materialize_at_site` (Core #4 sibling-drift consolidation). **Ratchet 20 → 14** — the convergence meter's first decrease. Behavior-NEUTRAL (zero attribution delta, byte-identical clone sites/counts/reasons), so NOT a definition-lane event (Core #9 exempt: lanes share semantics, not implementation). The SH lane is ALREADY planner-shaped (`cow_scope_muts` table + the consolidated `cow_materialize_projected_root`, census 8 = ceiling), so no SH change is required for the substrate; the SH ratchet stays 8. |
| **2D** ✅ CLOSED (planner round 3) | untracked alias chains — REJECT-not-resolve | both | A mutation whose root is a view-returning method (`&x.slice()[i]`) that `resolve_projection_root_local` cannot name. The filed "still writes through to a live source" premise was **REFUTED** by an end-to-end C+LLVM sweep (see "Implementation status — converging" above): every reachable shape is a hard REJECT (String index-assign — `string_slice_index_assign_error.gg`, both lanes), a DEAD-TEMP write (owned/view method result — `known_gaps/cow_2d_method_result_dead_write.gg`, warning-track via a filed D2-rider), or a separately-filed ICE (`known_gaps/{string_view_push_char_ice,vector_windows_nested_index_assign_ice}.gg`). No resolver invented (there is no live aliasing source to materialize); the RESOLVE path is deferred until a view-returning method on a *mutable* container exists. The "converging to the uniform rule" marker is **removed**. |
| **2F** | nested `&` field place (snag #53) | both | `void set(Outer &o): o.inner.raw[k] = v` is a silent no-op — the nested MutPtr place chain isn't built. Un-ignore `known_gaps/snag53_*` when green. |
| **2H** | generic-equip bare named-receiver materialize | self-host residual | The generic case of the `&self`-mutation-inference (`compute_method_mutates_self`) classification. |
| **I (write)** | comprehension element write-through | both | `[f(x) for x in &v]` mutating `x.field` lands on a private element copy and does not reach the collection (the read path is correct everywhere; the comprehension threads `write_through=false`). Out of the ggdef phase-0 subset (no comprehension arm). |

Two further write facets are wave-assigned as they are scoped: bare
`v[i].method()` with a mutating receiver (the method-receiver analog of the
value-element write-through), and the comprehension write facet above.

#### The at-site conversion roadmap (planner Phase 3)

Class A is the first of six at-site materialize CLASSES the Rust ratchet counts
(the 20 `.cow_before_mutation(` sites at round-3 start, mapped to classes A–F +
the funnel's own call). Each future class conversion moves its count OUT of the
text census INTO the plan (ratchet decreases). The order is a **deliberate
choice** because two rankings **diverge** — the parent picks per round:

- **VALUE order (heat-first): C → then E/D.** The self-compile heat is dominated
  by CLASS C **`&`-formation** args, not A/E: one site — `coal_disjoint(&lb, …)`
  at `lir_codegen.gg:6754` — is the single hottest at-site materialize, and it
  plus `meet_states(&…)` / enumerate `&`-formations are almost all **defensive
  `&`-formation clones whose callee provably doesn't write** (read-only checks).
  That is the planner's biggest prize: elide the `&`-formation materialize when
  liveness/signature proves the callee never writes (the DEEP-1 "borrow-elidable"
  class). C is therefore the highest-VALUE conversion — but it needs the funnel's
  **richer return** first (a `-> Option<LocalId>` did-materialize + new-root
  variant, so E/C/D can re-resolve the rebound name + arm the transient-handle
  untrack); a naive C/E/D conversion that drops the re-resolve/untrack would
  reintroduce the round-33 heap-UAF class.
- **RATCHET-CHEAP order (convergence-first): F → B.** CLASS F (for-loop
  alias-root sever, 2 sites, already pre-header-shaped → a `LoopPreHeader`
  directive) and CLASS B (whole-value reassign sever, 1 site, `AtSite`, stays
  `UNTRACK_EXEMPT`) are fire-and-forget LOW-complexity conversions that move the
  ratchet cheaply (14 → 12 → 11) without the funnel upgrade. They prove more of
  the substrate at near-zero risk but yield ~no runtime win.

The heat figures above were measured by the round-3 scout (driver.gg self-compile,
`--clones=stats`); **regenerate before acting on them** — a `--clones=stats` build
prints the `[clone-stats]` per-reason rollup and the per-site heat. The durable
structural findings (not the drifting counts): CLASS C `&`-formation is the
elision prize concentrated at `lir_codegen.gg:6754`; A/F/B are fire-and-forget,
E/C/D need the richer funnel return. Live status + the picked order stays in
`TODO.md`.

**Self-host file-zone serialization** (these tracks share lowerer files and
must serialize or rebase often against each other and the enforcement wave):

| Zone | Increments |
|------|------------|
| `lower_stmt.gg` place / field / index | 1B, 1C-SH, 2F |
| `lower_loops.gg` + comprehension desugar | 1A, I, 2G |
| `lower_expr.gg` / `lower.gg` CoW | 1A-SH helpers, 2D, 2E, 2H, 2T |
| Rust `for_loops.rs` + comprehension | 1A, I |
| Rust `exprs/mod.rs`, `assigns.rs`, `methods.rs`, `context.rs` | 1C, 2D, 2E, 2T |

**Verified anchors** (re-grep the function before trusting a line number —
`lower*.gg` was split, and the Rust lowerer moves):
`try_resolve_field_place` Index-Ptr arm `exprs/mod.rs:~2472`;
`resolve_projection_root_local` `exprs/mod.rs:~2374`;
`cow_before_mutation` `context.rs:~3325`; the self-host place/field-write
producer `lower_stmt.gg:~1514–1638`; the self-host for-vector binding
`lower_loops.gg:~224`; D2 in `docs/define-gorget/decisions.md`.

**Wave-2 close gate** (and any track that flips or adds a fixture expectation):
full C **and** full LLVM integration, `--lib`, `--test lints`, the bootstrap
fixed-point, `spec_conformance` + the **full ggdef suite**, ASan on new
materialize/untrack fixtures, and a **parity regen** whose WRONG-OUTPUT count
must **drop** relative to the pre-wave baseline (never inflate parity by
excluding a self-host miscompile). Wave 3 is the spec lock: spectest-lane wiring
for the in-subset positions once the elaborator covers them, the language-design
/ book / this-chapter status updates, a place-resolver `CollectionKind`
exhaustiveness lint, and a clone-count baseline that must not regress on the
self-host self-compile.

## Full lazy materialization (#37) — the lazy-CoW default

The design goal (`docs/language-design.md`, the Performance pillar): **value
semantics at hand-optimal clone cost.** The user writes plain value
semantics; the compiler places the minimal clone set, as if every copy had
been written by hand. Borrow-by-default (above) already makes reads free.
This section is the other half: when a borrowed value's source **is mutated
on some path**, the clone is deferred to the latest *sound* moment — the
mutation itself. A mutation path that never executes at runtime costs **0
clones**; a taken path costs exactly **1**. This is the `ViewOf(source)`
provenance model documented in `docs/language-design.md` §23 ("String Types
in Depth"), and it is the production default in **both compilers**. The Rust
lowering and the self-host lowering reach it through different mechanics —
read-site materialize hooks vs. direct provenance — with identical observable
behavior; the difference is dictated by their local-variable substrates and
is explained per-implementation below.

### The shared mechanism

Both implementations lower the lazy-eligible bind — a String bound from a
CoW element borrow whose source collection is mutated on a forward path
(e.g. `String s = v.get(0).unwrap()` followed by a conditional `v.push(..)`)
— to the same runtime shape:

- **Bind** = a String VALUE slot holding a cap=0 view
  (`gorget_string_borrow_view`, `src/backend/c/runtime/runtime_string.c`: a
  shallow 32-byte header copy with `cap` FORCED to 0 — drop-safe in both
  states, because the cap-driven free no-ops on a view), plus a
  `materialized = false` flag slot.
- **Mutation site** = a flag-guarded **clone-once, in-place materialize**:
  `if (!flag) { slot = clone_to_owned(&slot); flag = true; }`. The deep
  clone happens at most once, reads from the still-valid view, and writes
  back into the SAME slot — no fresh local, no name rebind.
- **Loop safety** comes from slot placement, not special-casing: both slots
  are created before the loop, so LIR-SSA phis them at the header and the
  post-loop read of `s` is correct in both the materialized and
  never-materialized branches.
- **Escape safety** is unchanged from the eager model: consume positions
  still clone/move per the boundary rules above, and a view that reaches an
  owning collection slot is upgraded by the runtime `elem_materialize` hook
  ([§ Runtime `*_materialize` hooks](#runtime-_materialize-hooks)).

**Typed eligibility (devbook/24).** Laziness is gated on typed metadata, not
names, and **String is the only eligible element type** in both compilers:
its free is view-aware (`cap==0` no-ops), whereas `gorget_array_free` runs
`elem_drop` whenever `data != NULL` regardless of cap — a cap=0 collection
view would double-drop every element (Dict/Set similar; user structs have no
view discriminator). Each implementation carries the axis on its own typed
metadata: Rust reads `TypeMetadata.borrow_view_fn` (a sibling axis of
`clone_fn`/`materialize_fn`, mirrored on
`BuiltinTypeProtocol::borrow_view_fn`, read via
`LoweringContext::borrow_view_fn_for`); the self-host reads
`ResourceMetadata.materialize_fn` *presence* via `resource_meta_for` (the
materialize itself always calls the typed deep-copy `pointee_clone_fn` →
`clone_to_owned`, never `copy_cow` — which passes cap=0 views through — and
never `materialize_fn` itself). Collections join when their frees become
view-aware (TODO).

**Mechanical safety insight** (why `push`/`insert`/`sort` need no special
handling): the cap=0 view copies the element's 32-byte `Str` header — `data`
points at the element's character buffer, not the array backing store.
Operations that move headers cannot invalidate it. Only element-destroying
ops can (element overwrite/`set`, `remove`/`clear`/`pop` via `elem_drop`,
collection drop/reassign/move) — and each routes through the
`cow_before_mutation` family, which materializes first.

### Lazy loop-carried materialization (the Rust lowering)

The eligible bind (typed axis above, AND the source is a
**`CollectionId::Local`** collection — FieldPath sources stay eager, see
[§ Open items](#open-items)) is detected at `lower_var_decl`
(`stmts/mod.rs`) when the prescan proves the source mutated on a forward
path (`source_mut_unsafe` / `compute_cow_reassigned_after`), and lowered by
`emit_lazy_loopcarried_borrow` (`context.rs`): a pre-loop view slot + a
pre-loop `__cow_mat = false` flag, the pair recorded in
`FunctionState::cow_lazy_mat_flag`. The in-place materialize is
`cow_materialize_view_lazy_in_place` (`context.rs`), dispatched from Case 3
of `cow_before_mutation`, from `cow_sever_all_aliases_from`'s
collection-ref walk, and from the four read hooks below.

**Why read hooks (the D1 class).** Rust's GIR locals are SSA-versioned: a
derived binding gets a *fresh local*, so a read that captures the lazy
view's VALUE or ADDRESS into another binding loses provenance to the
collection — Case 3 can no longer fix the captured copy, and the result is
wrong output or a view-UAF. Rather than propagate provenance through every
derived local, the Rust lowering materializes the SOURCE before any such
capturing read. One shared helper (`materialize_lazy_source_if_needed`,
`context.rs`: projection-free Copy/Move local present in
`cow_lazy_mat_flag` → flag-guarded in-place materialize), four call sites:

| Hook | Site | Covers |
|------|------|--------|
| W3a | `lower_var_decl` trailing-assign entry (`stmts/mod.rs`) | `String x = s` alias (Branch C) and move-steal (F/G) binds |
| W3b | `returns_view` method receivers, PRE-call (`exprs/methods.rs`) | `s.substring(..)` temps and named binds — the call copies the header at call time; the post-call View tag is too late |
| W3c | `lower_index_access` place-arm (`exprs/methods.rs`) | `s[i]` / `s[a..b]` — the index route never consults `returns_view`; results carry NO View tag |
| W3d | `lower_for_string` source (`stmts/for_loops.rs`) | `for c in s:` — the synthetic `gorget_str_codepoint_at` is emitted as a `gorget_str_view_region` view by both backends |

**Multi-site dominance argument.** `restore_locals` reverts per-branch
ownership, so each branch-arm mutation site re-finds the tag and emits its
own guard — two guard callsites, first dynamically dead, exactly one runtime
clone. Same-straight-line later sites are dominated by the first guard's
continuation block, so their guards are runtime no-ops.

**Write sites clear the pair.** `lower_assign`'s Identifier arm and
`lower_compound_assign` (BOTH its string-concat early-return fast path and
its generic tail) remove the `cow_lazy_mat_flag` entry and the
`Borrowed{CollectionElement}` tag AFTER the RHS is lowered — after, not
before, because a self-referential RHS that mutates the source
mid-expression (`s = s + poke(&v)`) needs the `&v` dispatch to still find
the tag. A stale pair would emit a pointless guarded clone at the next
collection mutation and leak the new buffer via the materialize's
Move-assign overwrite. `consume(!s)` needs no clearing: the string-`!`
short-circuit (`exprs/calls.rs`) passes a const-Ptr borrow with no MoveZero.

### Phase 2 in the self-host — provenance-direct lazy CoW

The self-host lowerer (`tests/fixtures/self_host_lowerer/`) implements the
same default **provenance-directly** — the documented `ViewOf(source)`
design realized literally, with NO read hooks. The eligible bind is the
`LazyViewBind` arm of `decide_svardecl_emission` (`lower.gg`; the
whole-function mutation evidence comes from the CoW forward-scan in
`lower_cow.gg`), emitted in `lower_stmt.gg`'s SVarDecl arm as the cap=0
`gorget_string_borrow_view` slot + flag pair, recorded as a
`LazyMember{root, slot, flag, slot_type, clone_fn, stmt_scoped}` on
`LowerCtx.cow_lazy_members`, keyed by the source collection's root name.
Every mutation site of that root emits the shared flag-guarded in-place
materialize (`cow_lazy_emit_guard` / `cow_lazy_materialize_family`,
`lower.gg`) — five hook positions, enumerated as a closed class table in
[§ The soundness apparatus](#the-soundness-apparatus). Drop registration
uses the typed override `register_lazy_slot_for_drop` (`lower_drops.gg`):
`register_local_for_drop` correctly skips `LoView` aliases, but a lazy slot
OWNS its buffer once materialized — the unconditional `DropEntry` is safe in
both states because the pre-materialize cap=0 free no-ops.

**Provenance-by-slot-aliasing** is what makes the direct design sound on
this substrate where it was unsound on Rust's SSA-versioned locals: the
self-host keeps a flat `named_locals` memory-slot model in which no name
ever rebinds, so a same-type alias (`String x = s`) lowers to a POINTER TO
THE SLOT and derefs at read time — it observes the materialized value for
free. The derivation route that needed Rust's W3a hook needs NO code here,
and LIR-SSA phis the slot+flag across loop back-edges with zero
`lir_ssa.gg` changes. The two derivation routes that DO copy a view header
out of the slot each have one JOIN:

- **returns_view results** (join a, the ONE String-view tag site in
  `lower_expr.gg`'s method-call arm): a view result whose receiver is a
  family member joins the family with its OWN flag, `stmt_scoped=true` — it
  retires after the enclosing statement (`cow_lazy_retire_stmt_temps`).
  Covers the `cow_lazy_w3b_*` shapes (view-temp arg / concat operand
  computed before a mutating call in the same statement). The join's loop
  lives in the standalone helper `cow_lazy_join_view_result` (`lower.gg`),
  NOT inline at the tag site — `lower_expr_inner` is the cliff-critical
  frame of the bootstrap's deepest recursion, and inlining the loop's
  locals there once pushed its -O0 frame over the stack cliff (see
  [§ At-scale lessons](#at-scale-lessons--the-stack-cliff-and-measurement-hygiene)).
  Out-of-line keeps the hot frame small.
- **for-string source** (join b, `lower_for_string` entry,
  `lower_loops.gg`): the loop's per-iteration codepoint views alias the
  source buffer for the whole loop, so a lazy source materializes AT LOOP
  ENTRY (`cow_lazy_materialize_slot`, keyed by slot). Covers
  `cow_lazy_w3d_for_string`.
- **index/slice** (`s[i]` / `s[a..b]`) is DEFERRED behind F2: string
  index/slice currently miscompiles in the self-host before CoW matters
  (`cow_lazy_w3c_*` are expected-wrong in both modes; TODO).

**Eligibility gates beyond the typed axis.** The bound name must be
**pristine** — never written anywhere in the function — so no tag-clearing
machinery exists (the trade vs. Rust's write-site clearing is recorded in
[§ Open items](#open-items)). A source name that appears under a non-call
`!move` anywhere in the function is excluded via `cow_moved_names`
(move-shape semantics in [§ The soundness apparatus](#the-soundness-apparatus)),
and mutations reached only through a CLOSURE body are excluded via
`cow_closure_mutated` — a closure body cannot host the enclosing function's
materialize guard.

**The self-host is strictly lazier on alias shapes.** Because the alias
route is free, `cow_lazy_d1_alias_deadpath` executes **0** clones in the
self-host vs **1** under Rust's W3a hook (taken path 1 vs 1; witness family
0/1/0 in both — figures regenerated 2026-06-10 via the transient
`GG_CLONE_TRACE` runtime instrumentation; re-instrument to re-derive, never
quote the dated counts). The emitted-C shape is locked in on both sides:
`witness_never_emitted_c_clone_shape` (Rust) and
`witness_never_self_host_emitted_c_clone_shape` (self-host) in
`tests/integration.rs` each assert the borrow_view bind plus exactly one
dynamically-dead guarded `clone_to_owned` in the user-main body.

### The soundness apparatus

The safety argument is executable, not prose:

#### View-producer enumeration rule

Every producer of a cap=0 view aliasing another buffer must be covered by a
materialize hook, provably safe (owned elements, immediate byte reads,
boundary clones), or unreachable. Before adding any new view-returning
path, grep `gorget_str_view_region` across **all of `src/`** — the runtime
`.c` files AND the backend `.rs` emitters (synthetic callees like
`gorget_str_codepoint_at` never appear in the runtime source) — and walk
each hit to its GIR producer. The live source of truth for the producer set
is the `STR_VIEW_PRODUCERS` table asserted by the lint below, not any prose
snapshot. The sibling-site lesson was
paid for twice: a consumer-side grep missed the index/slice route (W3c), and
a runtime-only producer grep missed the synthetic for-string route (W3d).

**The rule is EXECUTABLE** (`tests/lints.rs`, fatal from day one):
`str_view_producer_enumeration_is_closed` (exact-set, four arms: runtime-C
callers of `gorget_str_view_region` == the `STR_VIEW_PRODUCERS` table; `.rs`
files spelling it on non-comment lines == the emitter allowlist; every
producer declared `sig(`, never `sig_fresh(`, in `src/lir/runtime.rs`; every
`returns_view: true` decl in `builtins.rs` routes to a table producer),
`no_growth_in_lir_view_callee_rewrites` (budget fence over view-callee
mentions in `src/lir` — the IndexLoad-rewrite class), and
`no_growth_in_runtime_c_direct_view_manufacture` (budget fence over raw
single-line `{ .data = ..., .cap = 0 }` literals, field-order-independent).
The third lint exists because the grep alone was INCOMPLETE:
`gorget_string_borrow_view` manufactures its cap=0 header via a DIRECT
struct literal, never spelling `gorget_str_view_region` — invisible to the
grep rule above. What the guard CANNOT see stays a prose obligation of this
section — four residuals: (1) dynamically-constructed callee names;
(2) passes that move/duplicate an existing view call so a hook no longer
dominates it (semantic, not greppable); (3) same-commit budget-slot reuse
(retiring one mention frees a slot a new unsound site could silently spend);
(4) backend-emit-layer callee rewrites (`src/backend` name-level
substitutions — all view→view today; a NEW backend rewrite targeting a view
callee spells no `view_region` and sits outside the LIR fence's
`src/lir` root).

#### The mutation-route class table (self-host)

The dual of the producer enumeration: every route by which the SOURCE can be
mutated must reach a materialize hook. The sibling-site rule made executable
as a table — scan arm × lowering position, one fixture per row; the
code-comment twin lives at `cow_lazy_emit_guard` in `lower.gg`:

| # | scan arm (`cow_mark_*` producer) | lowering position(s) | coverage | fixture |
|---|----------------------------------|----------------------|----------|---------|
| 1 | EMethodCall mutating receiver | EMethodCall arm (`lower_expr.gg`) | hook 1 | `witness_*`, `cow_lazy_multisite` |
| 2 | ECall sig-args (`&`/`!`, F1 signature-driven, redirect-resolved) | `lower_call` arg loop | hook 2 | `mutarg_probe`, `cow_lazy_move_consume` |
| 3 | EMethodCall sig-args (`&`/`!`) | EMethodCall marg loop | hook 3 | `cow_lazy_method_arg` |
| 4 | SAssign target root | `lower_stmt` SAssign | hook 4 | `cow_lazy_reassign_source` |
| 5 | SCompoundAssign target | `lower_stmt` SCompoundAssign | hook 5 (collection-root compound is `lower_fail` today — class-rule completeness; member compound is excluded by the pristine gate) | `cow_lazy_compound` |
| 6 | EMove (non-call `!name`) | decl-init / assign-RHS / return / literal element / match scrutinee — NO choke point | **eligibility EXCLUSION** (`cow_moved_names`) | `cow_lazy_move_bind`, `cow_lazy_move_reassign` |
| – | EMutableBorrow (`&name`) | call args strip the sigil in the parser; survivors lower as passthroughs | rows 2/3 carry the signature-driven routes; `&s` on the member itself is excluded by the pristine gate | `cow_lazy_mut_borrow_write` |
| – | SWith / spawn / comprehension | no distinct route — they only recurse; their lowerings route through the hooked paths | (two PRE-EXISTING mode-independent gaps in TODO: inline-closure-spawn-arg; bare collection alias `w = v` then mutate) | — |

#### Move shapes: the oracle exception and the open Rust EMove bug

The self-host's `cow_moved_names` exclusion is whole-fn, per-source-NAME:
one `!v` anywhere — even on a never-taken branch — makes every bind from `v`
in that fn eager (run-proven 1 clone where lazy would be 0). Acceptable
because the borrow checker independently rejects conditional-move-then-use
("use of moved value"), so the practical loss window is narrow.

⚠ **Rust gg is VALUE-WRONG on both EMove shapes** (move-bind
`Vector[String] w = !v` and move-reassign `w = !v`, each followed by a
mutation through `w` and a read of the lazy-bound `s`): the lazy
read-through prints the post-mutation value where eager semantics print the
pre-mutation one. Memory-safe but a behavior bug, open as a HIGH `TODO.md`
item. Until it lands, the move-shape fixtures (`cow_lazy_move_bind`,
`cow_lazy_move_reassign`) assert EAGER semantics through the SELF-HOST
route — the self-host's exclusion is the reference behavior — and are
expected-wrong rows in `runtime_diff` (the oracle is the buggy side); they
are deliberately not snapshotted.

#### ASan is NOT the safety net

The D1 wrong-output class (alias of a pre-materialize slot) and the
W3b/W3c/W3d view-UAF class are both proven ASan-SILENT (the latter even
with real heap UAFs — likely a pool-allocator free path). The stdout
fixture battery (`witness_*`, `cow_lazy_*` in `tests/fixtures/`, plus the
self-host `self_host_runtime` snapshot net and the emitted-C clone-shape
lock-ins) is the PRIMARY net; the sanitizer is defense-in-depth only.
Future debuggers of this machinery: do not interpret a clean sanitizer run
as absence of a lazy-CoW bug.

### At-scale lessons — the stack cliff and measurement hygiene

Running lazy-by-default through the bootstrap surfaced two at-scale facts.
Both initially presented
as lazy-mode defects; neither was one — the first was a real host-resource
limit misattributed to the lazy lowering, the second a measurement
artifact. Both are closed structurally:

1. **The stack cliff.** What looked like a lazy-mode miscompile of stage-1
   was pure STACK CAPACITY: the bootstrap's deepest recursion (~51 levels
   of `lower_expr` ↔ `lower_expr_inner` lowering `derive.gg`'s 51-term `+`
   chain, ~226KB/frame at -O0) consumed ~11.8MB of a 12.2MB host ulimit —
   ANY +960B of frame crossed the cliff, and the 2 lazy binds in
   `lower_expr_inner` added +9KB. Under a raised ulimit every "corrupt"
   variant ran green with BYTE-IDENTICAL output, and `ulimit -s 11000`
   killed the GREEN eager baseline — causality both ways. Closed
   structurally by **dead-decl elision** (emitted-body scan in both C
   emitters: only referenced `__v`/`__s` ids are declared — the decl set
   used to be `0..max_val` regardless of use, ~124K dead decls
   module-wide) and **slot coalescing** (backward-liveness
   interval-coloring in both C emitters: SSA temps whose live ranges never
   overlap share one C decl, shrinking the per-call lowering frame). A
   stopgap 64MB-pthread main runner (Fix B) was the original fix but was
   REVERTED 2026-06-12 — it ran the program body on a secondary thread,
   breaking macOS/Cocoa UI init (which must be on thread 0). Coalescing made
   it unnecessary: the program runs as a plain `int main` on thread 0, and
   real self-host code self-compiles under a plain ~8MB stack. The honest
   OS-default stack (Option A — no big-stack opt-in) is pinned by two
   executable guards in `tests/integration.rs`:
   `stack_guard_self_host_driver_deep_lowering` (the DRIVER self-compiling
   its OWN source under an 8MB ulimit — the frame-bloat regression net) and
   `stack_guard_runtime_deep_recursion` (EXPECT-FAIL: a PRODUCED BINARY at
   recursion depth 200000 ≈ 22MB overflows a plain 8MB stack exactly like
   C/Rust; TCO is the eventual cure for the tail subset). A pathological
   200-deep single expression still needs ~32MB to lower, but that is not
   the contract — like clang/gcc, deeply nested exprs can overflow the
   compiler stack.
2. **Measurement hygiene.** A reported "~7x lazy emission slowdown" did not
   reproduce under controlled conditions — sequential idle-box timing pairs
   measured 1.11x at -O2 and 0.98x at -O0; the original figure compared a
   lazy run under parallel-cargo CPU thrash (a documented 4-8x wall
   multiplier) against an idle eager baseline. The standing rules, each of
   which cost a full diagnosis cycle: emission timings are only comparable
   SEQUENTIAL ON AN IDLE BOX, never under parallel cargo; any
   bootstrap-scale conclusion must state the stack ulimit it was measured
   under; and perf claims about clones count EXECUTED clones (runtime
   instrumentation), never source-read estimates.

### Open items

Honest residuals, all tracked with detail in `TODO.md` (the "LAZY-CoW
FOLLOW-UPS" block — that file is authoritative for status; this list names
the classes):

- **Rust EMove value-bug** (HIGH; the oracle exception above stands until it
  lands).
- **FieldPath and EIndex sources stay eager in Rust** (Phase 1b):
  `cow_before_field_mutation` has no lazy routing, `lower_field_assign`
  does not walk descendant FieldPath refs on root-struct mutation (the
  `empty_literal_struct_field` UAF shape — `cow_lazy_fieldpath_excluded`
  locks the exclusion), and `String s = v[i]` never sets the
  borrow-sources sidecar.
- **Self-host index/slice derivation join** blocked on the pre-existing F2
  string index/slice miscompile.
- **The pristine-gate trade**: member-reassignment shapes
  (`cow_lazy_staletag`, member compound assigns) stay eager at 1 clone
  where Rust pays 0 — outputs identical, invariant simpler; porting Rust's
  write-site clearing (W4) is the recorded parity lever.
- **EMove per-position upgrade** (prototyped: materialize-family-before-move
  at the SVarDecl-EMove and SAssign-EMove-RHS sites) if move-shape laziness
  ever matters.
- **Collection generalization** blocked on view-safe frees (the typed
  `borrow_view_fn` axis is in place; populate per-protocol once
  `gorget_array_free` and friends check cap).
- **Rust 1b provenance back-port**: the self-host's provenance-direct
  design beats the hook design on alias shapes; porting it to Rust gg is
  the recorded laziness upgrade there.
- **Driver-emission cost lever**: `generate_c` dominates driver self-compile
  time in both CoW modes via a per-extern-call `LirFunction` deep-clone — a CoW
  consumer cost, not a lazy-materialization one.

## `MoveZero` and post-call ownership transfer

When a consuming position is move-eligible (the source owns the data and is dead
at the call), no clone is emitted; instead the IR carries a **`MoveZero`** on the
source local after the consuming call. Move-eligibility unpacks to exactly three
shapes (the `CLAUDE.md` contract): `!arg` (user opt-in), an expression temp
(last-use + owning by construction), or a named local at its last use bound to an
owned value (not from `.get()`, a view method, or a parameter — those bind
borrows).

`MoveZero` is a **backend correctness detail, not move semantics**: the source
slot is logically dead the instant the move happens. The backend zeros the slot
only when drop-tracking would otherwise re-drop the value, and elides the zero
when liveness proves the read unobservable. `Drop` and `MoveZero` are inserted by
the drop elaborator (`src/ir/lowering/drops.rs`).

The consuming-call lowering tracks any clone temps it emits (the
`pre_call_clone_temps` list, `exprs/methods.rs:1908`) and `MoveZero`s each one
right after the call so the scope-exit drop pass doesn't double-free the
freshly-cloned temp.

**A layering correction over the internals doc.** `copy-on-write.md:359` states
that the C backend emits the post-call source zero via a `zero_arg_indices`
table in `emit_call_extern.rs`. That is **stale**: the table is gone. The
post-call zero is now emitted as a GIR `MoveZero` at the lowering layer, and the
C backend's old per-runtime-fn zero was removed because it duplicated the same
bytes — see the comment block at
`src/backend/c_lir/emit_call_extern.rs:900`. There is no `zero_arg_indices` in
the backend anymore.

## The view discriminator (`cap==0`)

The runtime side of CoW is a single shared convention: the field at **offset +8**
of every resource struct is `cap`, and `cap == 0` means **view** (non-owning),
`cap > 0` means **owned**. This is uniform across `GorgetString`/`Str`
(`src/backend/c/c_runtime.rs:1440`), `GorgetArray` (`c_runtime.rs:309`), and
`GorgetMap`/`GorgetSet` (`c_runtime.rs:322`). The generic check is
`gorget_is_view` (`c_runtime.rs:1458`): `((const size_t*)resource)[1] == 0`.

`Str` is a 32-byte struct `{ char* data; size_t cap; size_t len;
GorgetAllocator* alloc }` (`c_runtime.rs:1440`). Invariants
(`c_runtime.rs:1429`): `cap == 0 ⟺ view` (data may point into `.rodata` or
another `Str`; drop is a no-op), `cap > 0 ⟺ owned` (freed via
`alloc->dealloc(data, cap)` at drop). String **literals** are zero-alloc
`.rodata` views: `static const Str __slit_N = { .data="hello", .cap=0,
.len=5, .alloc=NULL }`.

View-returning string methods (`slice`/`substring`/`trim`/`strip`/`char_at`/…)
return a `cap==0` view into the receiver's buffer at zero allocation cost. The
lowering tags the result `LocalOwnership::View { RuntimeView(receiver) }` so that
`cow_before_mutation` Case 4 materializes it before the source is mutated. The
write site is gated on the **typed** `returns_view` protocol flag, not a name
list: `builtin_returns_view` (`context.rs:778`) reads
`protocol.methods[…].returns_view`, and the tag is applied at
`exprs/methods.rs:2595`.

### Runtime `*_materialize` hooks

Pushing a view into an owning collection is where the runtime earns its keep.
Every consuming runtime function (`gorget_array_push`, `gorget_map_put`, …)
obeys the same three-step shape (`gorget_array_push`, `c_runtime.rs:5244`):
`memcpy` the caller's bytes into the slot, then call the `elem_materialize`
function-pointer hook, then return. For string elements the hook is
`gorget_string_materialize_inplace` (`c_runtime.rs:1653`):

```c
static inline void gorget_string_materialize_inplace(void* p) {
    Str* s = (Str*)p;
    if (s->cap == 0 && s->len > 0) {
        *s = str_alloc_copy((const char*)s->data, s->len, __gorget_current_alloc);
    }
}
```

So a `cap==0` view that lands in a collection slot is upgraded to an owned copy
on the spot; an already-owned string (`cap>0`) is a no-op; non-string element
types have a NULL hook and skip the step. The hook pointers
(`elem_materialize` at offset 56 on arrays; `key_materialize`/`val_materialize`
on maps) are wired by the LIR collection-constructor pass, not the C backend —
see the inventory comment at `emit_call_extern.rs:887`. Return boundaries use the
bulk variants `gorget_array_materialize_all` (`c_runtime.rs:5286`) and
`gorget_map_materialize_keys` (`c_runtime.rs:5298`).

`gorget_string_clone_to_owned` / `gorget_string_clone` is the unconditional
deep-clone used by the compiler-emitted boundary clones; the `*_materialize`
hooks are the *lazy* `cap==0`-only variant used inside the runtime consuming
functions. `GorgetString.push`/`push_line`/`push_char` are StringBuilder appends
that *read* their argument (copy the bytes into the builder) and are therefore
**not** consuming positions.

## Observability — `--clones` and per-site attribution

All clone diagnostics live under one default-silent flag (`--clones[=MODE,…]`,
parsed by `parse_clone_modes` in `src/main.rs`). Four surfaces, one source of
truth (the `ImplicitCloneWarning` vector minted during GIR lowering):

- **`--clones=sites` / `--clones=verbose`** — the compile-time Clone Report
  (per-site `file:line:col`, type, reason; `verbose` adds CloneId, size,
  runtime fn), span-deduplicated for human reading.
- **`--clones=sites-tsv=PATH`** — the machine-readable static table: EVERY
  CloneId (no span dedup — monomorphized siblings share a span but have
  distinct ids) as TSV `id, file, line, col, type, reason, size_bytes,
  runtime_fn`.
- **`--clones=stats`** — runtime instrumentation. The aggregate `[clone-stats]`
  atexit line (always-compiled-in counters in `runtime_preamble.c`, including
  `string_clone` = calls to `gorget_string_clone_to_owned`; that counter also
  absorbs runtime-internal per-element clones via
  `gorget_string_clone_inplace`, so it legitimately exceeds the attributed
  per-site hits), **plus per-CloneId attribution**: the lowering pairs every
  `warn_implicit_clone` with a `__gorget_clone_site_hit(<CloneId>)` bump
  emitted immediately before the clone call — through the single producer
  helper `LoweringContext::warn_clone_and_hit` (straight-line sites), or
  split with the hit INSIDE the cloning branch at the three conditional sites
  (allowlisted in `tests/lints.rs::clone_warn_hit_pairing`). The backend
  sizes a `static _Atomic` counter table from `LirModule::clone_site_count`
  (`runtime_clone_sites.c`, emitted only under the flag) and reports
  `[clone-sites] cap=… distinct=… total_site_hits=…` + the hottest
  `[clone-site] #id=count` lines at exit (top 50 by default;
  `GG_CLONE_SITES_TOP=N` on the compiled binary widens, `0` = all — a
  truncated report says so explicitly, never a silent cap). Join the runtime
  `#id` counts against the `sites-tsv` id column for the full per-site
  profile. Flag-off builds emit NONE of this — no hit calls, no table, no
  symbols. Multi-module builds (shared-lib + exe) each carry their own
  counter table + hit function — the same per-module pattern as the
  `RUNTIME_CLONE_STATS` blob; the hit fn is `static` so each module's calls
  bind to its own correctly-sized table.
- **Not yet:** `--clones=stats` under `--backend=llvm` is rejected with an
  explicit error (the LLVM path composes its runtime by hand in
  `compile_llvm_pipeline` and never runs `emit_runtime_modules`; see the
  `TODO(llvm-clone-stats)` at the reject site in `src/main.rs`). The
  compile-time modes work on every backend.

Un-attributed residuals (clone emissions with no CloneId — they appear in
`[clone-stats]` aggregates but not in `[clone-site]` lines) are tracked in
`TODO.md`: the LIR-layer clone emissions (`src/lir/lower/insts.rs`,
`lifts.rs` — no CloneId concept exists at that layer) and the GIR interp-temp
staging path (`exprs/calls.rs` `AssignMode::Clone`).

## In the self-host

The self-host lowerer (`tests/fixtures/self_host_lowerer/`; since the
module split, `lower.gg` is the core plus `lower_expr.gg` / `lower_stmt.gg`
/ `lower_loops.gg` / `lower_drops.gg` / `lower_liveness.gg` /
`lower_cow.gg` and friends) implements the same model with the same
ownership taxonomy — including the lazy default, whose self-host mechanics
are in [§ Phase 2 in the self-host](#phase-2-in-the-self-host--provenance-direct-lazy-cow)
above. `LocalOwnership` is an enum in
`gir.gg:178` with `LoOwned` / `LoBorrowed` / `LoView` / `LoParam` /
`LoMaybeOwned`, and the IR `BorrowOrigin` mirror is `gir.gg:240`
(`BoNone`/`BoParam`/`BoCollectionElement`/`BoField`/`BoRuntimeView`/`BoAlias`/
`BoFieldPath`/`BoTupleElement`/`BoCowBorrowPending`) — the docstring there
explicitly cites the Rust `src/ir/mod.rs` source it mirrors.

The clone-vs-move-vs-borrow decision lives in `op_consume`
(`lower.gg:1638`), which is the self-host analogue of
`ensure_owned_at_consuming_arg`. It uses a typed **`ConsumeKind`** position-class
enum (`gir.gg:195`) so each call site names whether the position is consuming;
at non-consume kinds it unconditionally returns `OpBorrow`, and at consume kinds
it dispatches on the source's ownership tag (the resource arm at the tail of
`op_consume`):

```gorget
match loc.ownership:
    case LoBorrowed(): return OpClone(lid)
    case LoView():     return OpClone(lid)
    else:              return OpMove(lid)
```

`Ptr(T)`/`MutPtr(T)` resource sources at a consume position route through
`decide_ptr_consume` (routed from `op_consume`'s `GtPtr`/`GtMutPtr` arms,
defined at `lower.gg:1904`), the self-host equivalent of
`ensure_owned_at_consuming_arg`'s `Ptr` arm — clone-through, never
shallow-alias.

The self-host's for-loop lowering already binds the for-element as a borrow
alias — `lower_for_vector` (`lower_loops.gg:206`) tags the element
`LoBorrowed` with `BoCollectionElement(coll_local)` and *no* owned-drop
registration (`lower_loops.gg:240-267`), and `lower_for_string`
(`lower_loops.gg:307`) does the same for codepoints. That is, the for-element
elision documented above is the *Rust* compiler matching behaviour the
self-host already had — it closes a Rust↔self-host backend-parity gap rather
than introducing a new idea. The dict/set self-host loops mirror Rust:
their runtime accessors hand back a clone, bound owned and drop-registered
(`lower_loops.gg:431-442`).

View tagging reads the typed `returns_view` column off the String builtin
table (`STRING_BUILTIN_METHODS` via `string_builtin_method`, consumed at the
LoView tag sites in `lower_expr.gg` — the method-call arm, PLUS the
slice-/index-syntax arms `s[i:j]`/`s[i]` → `gorget_str_slice`/`gorget_str_index`,
which bypass the method-builtin table entirely) — the old `is_string_view_method`
name-match this paragraph once documented has been retired. The tag is
load-bearing on every view-producing site: a mis-tagged method slice view
(tagged owned instead of `LoView`) once move-elided a `.clone()` and injected
NUL bytes into the multi-MB `generate_c` output; separately, the slice-/index-
syntax dsts were left untagged (`LoOwned`) and so were moved by value and freed
by the SAssign drop-on-overwrite *before* the view aliased the source buffer
(a UAF — garbage output on `fmt_basic`/`fmt_edge`), until both dsts were tagged
`LoView()/BoNone()` so `op_consume` materializes at the owning position. That same tag site is also where a
lazy family member's view results join the family (the returns_view
derivation join described in the Phase-2 section above). There is also a
fuller `decide_operand_at_consuming_arg` (`lower.gg:1809`) that splits the
decide/emit concerns. It is wired in: `wire_one_operand`
(`lower_liveness.gg:921`) delegates to it, and that shim is driven by the
live `wire_liveness_into_modes` pass (defined `lower_liveness.gg:849`, run
from `lower_closures.gg` and `lower_loops.gg`). Its own header docstring
(`lower.gg:1753`) still reads "dead code in this commit. No caller exists."
but that status is **stale**: the caller exists, and the comment is a
self-host cleanup target (see TODO).

**Parity is a procedure, not a number.** The self-host lowerer's fidelity here is
exercised by the lowerer comparison test; to read its current state run

```
cargo test --test integration lowerer_comparison -- --nocapture
```

and read the printed matched-count — the `*_comparison` tests are
diagnostic-always-pass (a green run asserts nothing about parity). The C/LLVM
**backends** are not self-hosted, so the `cap==0` runtime materialize hooks and
the `MoveZero`-to-zero-slot codegen have **no self-host coverage** — they exist
only in `src/backend/`.

### The `&self` mutation-inference pass — gating named-receiver materialize

When a bare (immutable-rooted) by-value receiver is mutated through a method
call, `cow_before_mutation` must materialize a private copy of the root (§
[Mutation severs the alias](#mutation-severs-the-alias-cow_before_mutation)).
For a **named** user receiver — `x.set_name("Y")` where `x` is a bare `Res`
param — the self-host needs to know whether the method actually *writes* its
receiver: materialize only if it does. A blanket `&self`⇒mutating
over-approximation is not a correctness bug (the extra clone is harmless), but
it is a *memory* bomb: every read-only `&self` getter or `.clone()` in a hot
driver loop would deep-clone the whole root, and the self-host — unlike the
Rust reference — must compile **itself**, a program whose hot loops call such
getters densely. Measured, the naive over-approximation is a ~12–14 GB clone
bomb that OOM-kills the self-compile / `bootstrap_fixed_point` (only peak RSS,
not a green sweep or ASan, catches it). Rust's CoW gate uses the analogous
name/signature over-approximation (`method_mutates_receiver`) but is never
compiled through these hot loops, so it never balloons — this classifier is a
**self-host-only** need, standing in for the Pass-5 purity inference the
self-host driver otherwise lacks.

`compute_method_mutates_self` (`lower.gg:1401`, run from the pre-pass) computes
the answer precisely: a monotone fixpoint over self-callee edges. It seeds each
`&self`/`!self` equip method (non-generic equips only) with a direct-mutation
flag from `mutinf_scan_stmts` / `mutinf_scan_expr` (which recognise
`self`-rooted writes via `mutinf_expr_is_self_rooted`), records the set of
`self`-method calls each makes as edges, then propagates mutation along those
edges until fixed. The result is keyed `Type__mname` on the typed
`GirModule.method_mutates_self` map (`gir.gg:621`) — one source of truth for
this axis, alongside the existing `fn_borrow_params` / `fn_move_params`
name-keyed caches. The self-convention (whether idx-0 is `&self`/`!self`) is
read from `fn_borrow_params`/`fn_move_params`, **not** from `mi_meth.params` —
`apply_collect_target_rewrites` resets the reconstructed `self` param's
ownership to bare (a filed footgun).

The materialize gate (`lower_expr.gg`, the method-call arm's `_r37_mut` block)
and the scan both run **USER→BUILTIN→leaf** order, mirroring Rust's
`method_mutates_receiver`: the user-method classification (via the
`method_mutates_self` map) is consulted *before* the name-based
`builtin_method_mutates` table, so a user `&self`-mutator whose name collides
with a read-only builtin (`get`/`map`/`peek`/`values`/…) still materializes.
The name-collision guard resolves the receiver's element/field type
(`mutinf_recv_type_name` — the typed local slot + `index_value_type_name` +
`GirTypeInfo.fields`) for both a named receiver (`mutinf_named_recv_writes_self`)
and a projected one (`v[i].get()` / `s.v[i].get()` / `o.inner.get()`,
`mutinf_projected_recv_writes_self`), so classification stays precise across
receiver shapes. It is bomb-safe by construction: a genuine projected builtin
call `v[i].len()` resolves to `Elem__len`, absent from `method_mutates_self`,
so it stays read-only and clones nothing. A false read-only (an unclassified
generic-equip instance, or a name-collision on an *unresolvable* projection)
degrades to the pre-existing BASE write-through — no new miscompile, since the
materialize is itself double-gated on a bare-value root — and those residual
sub-cases are filed in `TODO.md`.
