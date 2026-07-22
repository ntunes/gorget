# CoW Transient-View Model — making the mutation resolver total

> **STATUS: DESIGN NOTE — owner-led brainstorm, 2026-07-22.** Parts ruled, parts
> leaning, parts open (see the status ledger at the end). **NOT ratified as a
> whole and NOT implemented.** Do not treat any of this as shipped behavior.
> When ratified + shipped it graduates to `docs/language-design.md` §3 and
> `docs/book/12-borrowing.md`; until then this file is the single durable record
> of the design so far. Pairs with the return-view (#13) ruling in
> [`unified-resource-model.md`](unified-resource-model.md) §6.

## The problem this solves

Gorget's CoW model promises that an unbroken borrow chain writes through to the
real owner: `a.b.c[i].field = v` mutates the collection, `for x in &coll: x.f = v`
reaches the elements. The compiler delivers that by resolving a mutation target
back to a **named root local** (`resolve_projection_root_local`,
`src/ir/lowering/exprs/mod.rs`) and materializing / writing through *there*.

That resolver is **not total.** It can walk identifiers, `self`, and
field/index/tuple projections, but when a chain passes through a **method call**
it only descends a hardcoded whitelist — `get`/`first`/`last`/`unwrap`/`expect`
on a builtin collection. Every other method return falls to `None`: the compiler
cannot name a root, so the write silently lands on a throwaway copy (a **lost
write** — a soundness hole) or forces a murky reject-vs-materialize decision.

The whitelist is a name-matched band-aid (a Core #2 violation living in the
resolver) standing in for the real missing fact: **is this return a view of
something, and of what?** The model below makes that a typed property and the
resolver total.

## The model — three rules

### Rule 1 — an assignment target must be a *place*; a call is a place iff it returns a view

The universal lvalue rule: `5 = 3` and `foo() = 3` are rejected. A call that
returns an **owned** value is a value, not a place, so a mutation threaded
through it (`g.at(0).n = v` where `at` returns owned) is rejected *as a
non-lvalue* — the same rejection as `foo() = 3`, with nothing to do with CoW.
This is an extension of the landed `E_InvalidAssignTarget` gate.

### Rule 2 — a returned view is a view of `self`, only

A callable's return is either **owned** or a **view of its receiver** — never a
view of an argument, a global, or a body-local. This single constraint buys three
things at once:

1. **Unambiguous provenance ⇒ no lifetimes.** Rust needs `'a` on
   `fn f<'a>(x: &'a T, y: &T) -> &'a T` because a borrowed return could come from
   either input. If a view can *only* be a view of `self`, there is nothing to
   disambiguate — the annotation Rust needs has no information to carry.
2. **Resolver totality ⇒ it composes.** View-of-self is a compositional rewrite:
   `view_root(recv.m()) = root(recv)`. So `f.blocks.get(0).unwrap().term` threads
   `unwrap`→`get`→`f.blocks`→`f`. No method is un-nameable, because view-ness is a
   typed flag on the signature, not a name in a list.
3. **Dangling-return safety, for free.** A method that returns a view of a
   body-local (`Vector[int] tmp = build(); return tmp.slice(0,2)`) is rejected by
   the *same* check — the returned view's root is `tmp`, not `self`. Rust's
   "cannot return a reference to a local" falls out with no separate escape
   analysis and no lifetime.

**Free functions** have no `self`, so they cannot return views — they return
owned. "A view of X" is naturally a *method on X*. The multi-source ambiguous
case (`hotter(a, b)` returning a view of one) simply cannot be spelled as a view;
it returns owned, and mutating through it is a plain non-place rejection. No
ambiguity survives anywhere.

This is a strict **generalization** of `language-design.md` §3.7, not a reversal:
today's rule is "returns always transfer ownership"; the new rule is "returns
transfer ownership *or* return a view-of-`self`" — still no lifetimes, because a
view's source is fixed. Today's language is the special case where nothing opts
into a view return.

### Rule 3 — views are transient; there are no stored borrows

A view never outlives the expression that produced it. This needs **no new
mechanism and no borrow checker**, because every position where a value could
come to *rest* — a local bind, a field init, a closure capture, a collection
`push`, a `return`-as-owned — is *already* an ownership boundary that
materializes (the "Ownership at Consuming Positions" set). A view that reaches
any storage position hits the existing clone and becomes owned. So "no stored
borrows" is the boundary set doing its existing job.

Consequently the whole model is a **small delta**: (1) a typed view-of-self
return flag replacing the name whitelist, and (2) the place-lvalue gate.
Materialize-on-store is unchanged.

## Why `Ref[T]` is not a stored borrow

`Ref[T]` is the type of a view. The reason it is not a "stored borrow" — and
therefore needs no lifetime — is a restriction on **where the type may appear**:

> `Ref[T]` is allowed in **return and parameter** position and **forbidden in
> binding, field, capture, and collection-element** position.

It describes a value **in transit** — flowing out of a function, into one — but
the type system refuses to let it come to **rest**. The instant you try to store
it, you hit an ownership boundary and it materializes to owned `T`:

```gorget
Ref[Cell] at(self, int i): ...   # OK — Ref in return position (a value in transit)

cells.get(0).n = 99              # OK — the Ref[Cell] is consumed transiently, never stored
Cell c = cells.get(0)            # bind = boundary → c is owned Cell; the Ref evaporated
Ref[Cell] c = cells.get(0)       # ILLEGAL — Ref[T] is not a valid type for a local
```

Because it cannot be stored, it cannot outlive its expression; because it cannot
outlive its expression, it can never observe a later mutation of its source;
because it can never go stale, there is nothing for a borrow checker to check.
Transience is enforced by the type being *unspellable in any resting position*,
not by analysis.

The contrast with Rust is the whole story:

| | Rust `&'a T` | Gorget `Ref[T]` |
|---|---|---|
| Storable in a local/field? | **Yes** (`let r: &T = …`) | **No** — materializes to `T` at the boundary |
| Can outlive its expression? | Yes | No |
| Therefore needs lifetimes? | **Yes** (`'a` tracks validity) | **No** (never lives long enough to go invalid) |

Rust's reference is first-class and storable, which is *exactly why* it needs
`'a`. Gorget's `Ref[T]` is transit-only, which is *exactly why* it does not. The
shelved `a = &f()` (see below) was an attempt to give `Ref[T]` a resting place —
which is what would have dragged lifetimes back in. "No stored borrows" and
"`Ref[T]` is unstorable" are the same rule stated two ways.

## View-ness is annotated, not inferred

View-of-self-ness is written in the signature as the return type `Ref[T]` (view)
vs `T` (owned) — not inferred from the body. The deciding argument:

> Under this model, whether a return is a view is **accept/reject-determining**:
> it decides whether a caller's `g.at(0).n = v` compiles-and-writes-through or is
> rejected as a non-place. A property that decides whether *other people's code
> compiles* belongs in the signature, not hidden in a body a caller can't see.

Infer would make a callee **body** edit (adding a `.clone()`) silently flip every
caller between accept and reject — action-at-a-distance on legality. Annotate
makes view↔owned a visible, breaking **signature** change, with the mismatch
error local to the method being edited.

```gorget
Ref[Cell] at(self, int i):                     # declares "view of self"
    return self.cells.get(i).unwrap()
Cell      copy_at(self, int i):                # declares "owned"
    return self.cells.get(i).unwrap()          # materializes at the boundary

g.at(0).n = 99          # accepted (Ref return) → writes through to g.cells[0]
g.copy_at(0).n = 99     # rejected — copy_at returns a value, not a place

# refactor at() to clone → the error lands HERE, locally, not at remote callers:
Ref[Cell] at(self, int i):
    return self.cells.get(i).unwrap().clone()  # ERROR: declared Ref[Cell], returns owned Cell
```

This is the same "infer the invisible, annotate the legality-affecting" principle
that governs `&mut`/`throws`/`async` across languages. **The compiler still
infers alias-of-self anyway** — it needs that fact for the #13 optimization below
— so the annotation is not compiler-necessity; it is *human-visible contract* and
*API stability*. Inference and annotation coexist, serving different consumers
(compiler-perf vs reader-contract).

Ceremony lands only where warranted: most returns are owned `T` (no annotation),
and `Ref[T]` appears only on genuine mutable accessors — exactly the APIs where a
visible, stable view contract is worth stating.

### Why `Ref[T]`-as-default is rejected

Tempting (less ceremony): make every projection-of-self return a view by default,
since a bind materializes anyway. But apply the legality test:

- **Perf gained: none.** #13 (below) already makes an owned `T` return borrow —
  not clone — for read-only use, and binds materialize either way. A `Ref[T]`
  default and a `T` default are physically identical for reads and for binds.
- **The only thing it changes is mutate-through** — it makes `g.at(0).n = v`
  write through *by default* for any projection return. That is precisely an
  **accept/reject change**, and a body-dependent one: whether `grid.at(x,y).mark()`
  mutates the grid or a dead copy would hinge on whether `at`'s body returns a
  projection or an owned value, invisible to the caller.

So `Ref[T]`-as-default moves a legality-affecting fact into the invisible-default
zone — the wrong side of the "a change must not affect legality" line — while
buying nothing on perf. It also inverts Gorget's value-semantics ethos (reading/
owning is the common case; mutation-through is the deliberate exception; you
should not default to the exception). **Keep `T` as the default**; the
less-ceremony win is delivered by #13 (invisible, legality-safe), not by the
default view.

## Articulation with #13 (return-view lazy materialization)

`Ref[T]` and #13 are **orthogonal axes** — the same infer/annotate principle
applied twice:

- **`Ref[T]` annotation** = a *visible semantic contract*: the caller may mutate
  through this view. Changes what is legal → annotated.
- **#13** = an *invisible physical optimization* on owned `T` returns: don't clone
  at the return boundary when the caller only reads; materialize lazily the moment
  ownership is needed. Changes only performance → inferred.

```gorget
# READ:  both physically borrow — #13 elides copy_at's clone (read-only, transient)
int a = g.view_at(0).n     # view: already a borrow, no clone
int b = g.copy_at(0).n     # owned T, but #13 borrows-and-reads → no clone either

# MUTATE-THROUGH: only the Ref contract permits it
g.view_at(0).n = 99        # ✓ writes through
g.copy_at(0).n = 99        # ✗ rejected — owned return, not a place

# BIND: both materialize (no stored borrows) — #13 does NOT apply to binds
Cell c = g.view_at(0)      # owned copy; the Ref evaporates
Cell d = g.copy_at(0)      # owned copy
```

Reading the columns: **#13 makes `T` and `Ref[T]` returns physically identical
for read-only use; binds materialize for both; the only observable difference the
annotation buys is the mutate-through line.** `Ref[T]` is "owned semantics that
#13 already makes cheap to read, **plus** permission to mutate the transient
through to the source."

### #13 is not a stored borrow (and needs no borrow checker)

"No stored borrows" bans the **visible, guaranteed, mutable** kind (`a = &f()`).
#13's borrow is a different animal on every axis:

| | Shelved (`&f()` stored borrow) | #13 (lazy read-only borrow) |
|---|---|---|
| Visible to the user? | Yes — you write `&` | No — return type is owned `T` |
| Guaranteed? | Yes — borrow or error | No — **materialize-when-unsure** |
| Mutable through? | Yes | No — read-only; write ⇒ materialize |
| Needs a borrow checker? | **Yes** (validity guarantee) | **No** — clone-biased optimization |

Because #13 is materialize-when-unsure, its "is this borrow safe?" analysis is an
**optimization gate** (can't prove safe ⇒ clone), never a **correctness gate**
(unsafe ⇒ reject). A clone-biased analysis can only miss an elision, never
produce a use-after-free — so it introduces no borrow checker. This also handles
the realloc hazard (R6): if the source could change under the borrow
(`a.push()` reallocs while `c = a.get(i)` aliases it), the analysis cannot prove
stability, so it materializes. The UAF case is exactly the case #13 declines to
elide — soundness and the optimization point the same way.

## The shelved piece — `a = &f()` (stored borrows)

Binding a call result as a persistent, mutable borrow (`Vector[int] b = &g.slice()`
living across statements) is the one construct that reopens stored-borrow validity
— a view that outlives its expression and can observe a later mutation of its
source, i.e. the exact thing lifetimes/borrow-checking exist to police.
**Owner-ruled (2026-07-22): no stored borrows; `&f()` shelved indefinitely.** It
returns only if Gorget ever decides it wants lifetime-flavored reasoning, which it
does not. Shelving it costs the soundness model nothing — it was only ever the
optional perf/expressiveness layer, and the `&`-on-a-call sigil disappears
entirely (no bind-of-a-view ambiguity to resolve). `&` on *parameters* and
*transient positions* is unaffected (it dies with the call/statement).

## Costs (accepted, named for the record)

- **No cheap long-lived views.** `b = big.slice(...)` held across statements
  clones rather than aliases. A genuinely long-lived alias is better spelled as an
  index/handle into the owner (a value; stores freely; re-derive the transient
  view per access). Consistent with materialize-when-unsure + no-refcount.
- **A concession on §3.7's "no user-visible borrowed-return type."** Surfacing
  `Ref[T]` as a writable return type reintroduces a borrowed-return *type* — but
  **without lifetimes** (view-of-self ⇒ unambiguous source), appearing only on
  genuine view APIs. §3.7's line becomes "no borrowed-return type *with
  lifetimes*." A conscious shift, not a free lunch.

## Sequencing / relation to the planner + D6

The typed view-of-self return flag is the same substrate as #13 and the
`SlotProvenance`/D6 layer (`unified-resource-model.md` §6): the materialization
planner is the intra-procedural decision table, D6 persists the decision through
GIR→LIR, and view-return provenance carries it across the call boundary. Soundness
(total resolver) and perf (#13 reclaim) fall out of one mechanism. Build order:
soundness rules (Rule 1 + Rule 2 typed flag) → planner table + D6 → #13 elision.

## Status ledger

- **RULED (owner 2026-07-22):** no stored borrows; `a = &f()` shelved. Transient
  views only.
- **LEANING (orchestrator, owner-aligned):** view-ness **annotated** as `Ref[T]`
  return type (not inferred); `T` stays the default (not `Ref[T]`-default); the
  place-lvalue gate rejects mutate-through of owned returns.
- **OPEN:** exact surface spelling of the view return (`Ref[T]` vs a keyword);
  whether a conservative *inference-to-reject* assist is worthwhile (unsure ⇒
  owned ⇒ reject, never a silent lost write) on top of annotation; migration for
  programs that currently silent-no-op a mutate-through-owned.
- **NOT IMPLEMENTED.** No code exists for user-writable `Ref[T]` returns,
  view-of-self for user methods, or the place-lvalue gate beyond the landed
  `E_InvalidAssignTarget`.
