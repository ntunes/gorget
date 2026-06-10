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
If a value is only ever read, no clone ever happens. The decisions are all made
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

### `ensure_owned_at_boundary` (`context.rs:1756`)

Unconditional "clone if this is any kind of borrow." Used at boundaries that
have **no concept of last-use** — the function body is leaving the value behind
regardless. Its decision tree:

- **`Constant::GlobalRef(name)`** for a resource-typed module global → clone
  through `GlobalRefPtr` (the LIR `GlobalAddr+Load` is a shallow struct copy
  that aliases the global's heap buffer). Skipped for
  `string_literal_view_globals` — those are immortal `.rodata` `cap==0` views,
  so the consumer's drop is a no-op (`context.rs:1782`).
- **`Ptr(T)`** → clone the pointee via `clone_fn_for_ptr(T)`. Cannot move
  through a `Ptr` (the callee can't know whether the caller still needs it); the
  param is recorded via `record_param_cloned` so the caller can later suggest
  `!` at last-use sites (`context.rs:1830`).
- **By-value resource that is a borrow** (`is_ref_local || is_bare_param ||
  is_cow_borrow`, or an `Untracked` resource) → clone via `clone_fn_for_ptr`
  (`context.rs:1865`). One carve-out: a *last-use* bare-param borrow that is
  drop-tracked moves instead of cloning (`context.rs:1882`).
- **Owned drop-tracked locals and non-resource locals** → pass through unchanged.

### `ensure_owned_at_consuming_arg` (`context.rs:1927`)

Last-use-*aware* "clone if borrow OR not last-use." Used at consuming positions
where the caller *might* still use the local after the call, so the helper takes
the AST argument expression to call `is_last_use_at(name, span)`
(`context.rs:1028`) on named-local identifiers. Its rule:

1. `Ptr(T)` borrow → always clone through the pointer (`context.rs:1942`).
2. By-value resource:
   - non-identifier expression (a temp, owning by construction) → no clone, the
     caller will `MoveZero` after the call (`context.rs:1992`);
   - named local that is a borrow (bare param / ref-state / cow-borrow) → clone;
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

## Mutation severs the alias: `cow_before_mutation`

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

## Lazy loop-carried materialization (the default since #37 Phase 1)

A String bound from a CoW element borrow whose source collection **is mutated
on a forward path** (`source_mut_unsafe`, prescan
`compute_cow_reassigned_after`) used to EAGER-clone at the bind — paying the
clone even when the mutating branch never ran. The default lowering is now
**fully lazy** (`stmts/mod.rs`, the `emit_lazy_loopcarried_borrow` branch):

- **Bind** = a pre-loop String VALUE slot holding a cap=0 view
  (`gorget_string_borrow_view`: shallow header copy with cap FORCED to 0 —
  drop-safe, the cap-driven free no-ops) + a pre-loop `__cow_mat = false`
  flag, the pair recorded in `FunctionState::cow_lazy_mat_flag`.
- **Mutation site** = `cow_materialize_view_lazy_in_place`: a flag-guarded
  IN-PLACE `clone_to_owned` of the slot (clone at most once, from the
  still-valid borrow, written back into the SAME slot — no fresh local, no
  name rebind, so it survives `restore_locals`). Dispatched from Case 3 of
  `cow_before_mutation`, from `cow_sever_all_aliases_from`'s
  collection-ref walk, and from the four read hooks below.
- Both slots are created BEFORE the loop (lid < the loop's `save_locals`
  boundary), so LIR-SSA phis them at the header and the post-loop read of
  `s` is correct in both the materialized and never-materialized branches.
  Dead mutation path → **0 clones**; taken path → exactly 1.

**Multi-site dominance argument.** `restore_locals` reverts per-branch
ownership, so each branch-arm mutation site re-finds the tag and emits its
own guard — two guard callsites, first dynamically dead, exactly one runtime
clone. Same-straight-line later sites are dominated by the first guard's
continuation block, so their guards are runtime no-ops.

**The four lazy-source READ hooks (W3a-W3d).** A read that captures the lazy
view's VALUE or ADDRESS into another binding loses provenance to the
collection — Case 3 can no longer fix the captured copy. One shared helper
(`materialize_lazy_source_if_needed`: projection-free Copy/Move local present
in `cow_lazy_mat_flag` → flag-guarded in-place materialize), four call sites:

| Hook | Site | Covers |
|------|------|--------|
| W3a | `lower_var_decl` trailing-assign entry (`stmts/mod.rs`) | `String x = s` alias (Branch C) and move-steal (F/G) binds |
| W3b | `returns_view` method receivers, PRE-call (`exprs/methods.rs`) | `s.substring(..)` temps and named binds — the call copies the header at call time; the post-call View tag is too late |
| W3c | `lower_index_access` place-arm (`exprs/methods.rs`) | `s[i]` / `s[a..b]` — the index route never consults `returns_view`; results carry NO View tag |
| W3d | `lower_for_string` source (`stmts/for_loops.rs`) | `for c in s:` — the synthetic `gorget_str_codepoint_at` is emitted as a `gorget_str_view_region` view by both backends |

**View-producer enumeration rule (for future hook siblings).** Before adding
any new view-returning path, grep `gorget_str_view_region` across **all of
`src/`** — the runtime `.c` files AND the backend `.rs` emitters (synthetic
callees like `gorget_str_codepoint_at` never appear in the runtime source) —
and walk each hit to its GIR producer. Every producer must be covered by one
of the four hooks, provably safe (owned elements, immediate byte reads,
boundary clones), or unreachable. The v7 brief
(`docs/plans/brief_37_phase1_lazy_default.md`, Appendix A) holds the full
23-row enumeration this default shipped against. The sibling-site lesson was
paid for twice: a consumer-side grep missed the index/slice route (W3c), and
a runtime-only producer grep missed the synthetic for-string route (W3d).

**The rule is now EXECUTABLE** (`tests/lints.rs`, fatal from day one):
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

**Typed eligibility (devbook/24).** The bind is eligible only when:

- `TypeMetadata.borrow_view_fn` is `Some` — the new metadata axis (sibling
  of `clone_fn`/`materialize_fn`, mirrored on
  `BuiltinTypeProtocol::borrow_view_fn`), read via
  `LoweringContext::borrow_view_fn_for`. Phase 1: **String only**.
  Collections cannot join until their frees are view-aware —
  `gorget_array_free` runs `elem_drop` whenever `data != NULL` regardless of
  cap, so a cap=0 array view would double-drop every element (Dict/Set
  similar; user structs have no view discriminator).
- the source is a **`CollectionId::Local`** collection. FieldPath sources
  stay eager: `cow_before_field_mutation` has no lazy routing, and
  `lower_field_assign` does not walk descendant FieldPath refs on
  root-struct mutation (the `empty_literal_struct_field` UAF shape).

**ASan is NOT the safety net here.** The D1 wrong-output class (alias of a
pre-materialize slot) and the W3b/W3c/W3d view-UAF class are both proven
ASan-SILENT (the latter even with real heap UAFs — likely a pool-allocator
free path). The stdout fixture battery (`witness_*`, `cow_lazy_*` in
`tests/fixtures/`) is the PRIMARY net; the sanitizer is defense-in-depth
only. Future debuggers of this machinery: do not interpret a clean sanitizer
run as absence of a lazy-CoW bug.

**Mechanical safety insight** (why `push`/`insert`/`sort` don't need a hook):
the cap=0 view copies the element's 32-byte `Str` header — `data` points at
the element's character buffer, not the array backing store. Operations that
move headers cannot invalidate it. Only element-destroying ops can (element
overwrite/`set`, `remove`/`clear`/`pop` via `elem_drop`, collection
drop/reassign/move) — and each routes through the `cow_before_mutation`
family, which materializes first.

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

## In the self-host

The self-host lowerer (`tests/fixtures/self_host_lowerer/`) implements the same
model with the same ownership taxonomy. `LocalOwnership` is an enum in
`gir.gg:150` with `LoOwned` / `LoBorrowed` / `LoView` / `LoParam` /
`LoMaybeOwned`, and the IR `BorrowOrigin` mirror is `gir.gg:212`
(`BoNone`/`BoParam`/`BoCollectionElement`/`BoField`/`BoRuntimeView`/`BoAlias`/
`BoFieldPath`/`BoTupleElement`/`BoCowBorrowPending`) — the docstring there
explicitly cites the Rust `src/ir/mod.rs` source it mirrors.

The clone-vs-move-vs-borrow decision lives in `op_consume`
(`lower.gg:1389`), which is the self-host analogue of
`ensure_owned_at_consuming_arg`. It uses a typed **`ConsumeKind`** position-class
enum (`gir.gg:167`) so each call site names whether the position is consuming;
at non-consume kinds it unconditionally returns `OpBorrow`, and at consume kinds
it dispatches on the source's ownership tag (`lower.gg:1465`):

```gorget
match loc.ownership:
    case LoBorrowed(): return OpClone(lid)
    case LoView():     return OpClone(lid)
    else:              return OpMove(lid)
```

`Ptr(T)`/`MutPtr(T)` resource sources at a consume position route through
`decide_ptr_consume` (routed from `op_consume`'s `GtPtr`/`GtMutPtr` arms at
`lower.gg:1423`/`1442`, defined at `lower.gg:1610`), the self-host equivalent of
`ensure_owned_at_consuming_arg`'s `Ptr` arm — clone-through, never
shallow-alias.

View tagging is the one notable **divergence from Rust**: the self-host
identifies view-returning string methods by **name match** in
`is_string_view_method` (`lower.gg:475` — `slice`/`substring`/`trim`/…) rather
than reading a typed `returns_view` flag off a protocol table the way Rust does
(`builtin_returns_view`). This is a smell by the project's own "No name matching"
rule, and it is load-bearing: the comment at `lower.gg:462` records that a
mis-tagged slice view (tagged owned instead of `LoView`) move-elided a `.clone()`
and injected NUL bytes into the multi-MB `generate_c` output. There is also a
fuller `decide_operand_at_consuming_arg` (defined at `lower.gg:1542`, doc-comment
block from `lower.gg:1473`) that splits the decide/emit concerns. It is now wired
in: `wire_one_operand` (`lower.gg:2479`) delegates to it at `lower.gg:2508`, and
that shim is driven by the live `wire_liveness_into_modes` pass (defined
`lower.gg:2407`, run at `lower.gg:7487`/`7722`) — the "Phase 2.2" thin-shim path
noted at `lower.gg:2462`. Its own header docstring at `lower.gg:1486` still reads
"dead code in this commit. No caller exists." but that status is **stale**: the
caller exists, and the comment is a self-host cleanup target (see TODO).

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
