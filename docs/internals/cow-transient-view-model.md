# CoW Transient-View Model — making the mutation resolver total

> **Status: `RATIFIED-UNBUILT`** — owner-led brainstorm, 2026-07-22. **NOT
> implemented**: the place-gate extension and the typed view axis do not exist
> at HEAD. Do not treat any of this as shipped behavior.
>
> **The ratified part is `D41`** (views internal to builtins only, no
> user-visible `Ref[T]`, user methods return owned, closures are the sanctioned
> mutate-through path), recorded in
> [`docs/define-gorget/decisions.md`](../define-gorget/decisions.md). That entry
> is the ruling; this note elaborates it, and the user-`Ref[T]` material below is
> retained as rejected-alternative context. The rest is leaning or open — see the
> status ledger at the end. When shipped it graduates to
> `docs/language-design.md` §3 and `docs/book/12-borrowing.md`. Pairs with `D40`
> (return-view lazy materialization).
>
> **Scope split (2026-07-28):** this note owns the **legality** axis — what is a
> place, what may be mutated through, accept/reject. The **cost** axis (the
> per-signature ownership summary, arg- and return-boundary elision including #13,
> the guaranteed-elision set, and the `implicit_clones` knob) is
> [`cow-cost-contract.md`](cow-cost-contract.md). `returns_view_of[i]` is defined
> HERE — it is legality-determining — and merely *read* there; § Articulation with
> #13 below is retained as context, with the live cost design in that note.

## Resolved direction (owner-ruled 2026-07-22) — internal views only, no user `Ref[T]`

The model below is written up with a *user-visible* `Ref[T]` return type. **The
owner has ruled that path out.** The chosen direction is the strictly simpler
subset:

- **Views are internal to builtins only.** The `returns_view` flag on
  `BuiltinMethodDecl` replaces the name whitelist (Core #2); `Ref` never appears
  in user source. **§3.7's "no user-visible borrowed-return type" stays true** —
  the concession the critique flagged simply evaporates.
- **User methods return owned.** Mutating through a user method's result is
  rejected as a non-place (Rule 1) — today it silently loses the write, so this is
  a strict soundness improvement, not a regression. `grid.at(x,y).mark()` on a
  user `at` → honest error.
- **Closures are the sanctioned way to mutate an encapsulated element**
  (`grid.update(x,y,(Cell &c): c.mark())`) — legal today, writes through. "One
  correct way" (Pythonic), chosen over adding a borrow-type concept for call-site
  sugar.
- **Transitive-unstorable becomes an internal compiler invariant + guard** (no LIR
  storage slot holds a `Ref`-containing type — a stored `Ref` is a UAF), not a
  user-facing type rule, since `Ref` is never user-spellable.

So the live design is: **place-gate (Rule 1) + typed builtin views (Rule 2,
internal) + transient/unstorable as an internal guard (Rule 3) + closures for user
mutate-through.** The user-`Ref[T]` material below (annotate-vs-infer, the §3.7
concession, phase B) is **retained only as rejected-alternative context** — it is
NOT the chosen path. A "place alias" (a pure-projection method that expands to a
place, no `Ref` type) is shelved next to `&f()` unless call-site readability later
earns exactly one small concept.

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

**This must be transitive** (Grok review, 2026-07-22): the restriction applies to
any type *containing* `Ref` at any depth, or "unstorable" leaks through
higher-kinded wrappers. `Vector[Ref[Cell]]` would be a *stored collection of
borrows*; binding `Option[Ref[Cell]] x = v.get(i)` would store a `Ref` inside a
resting local. So a type that mentions `Ref` anywhere is a **transit-only kind** —
legal in return/param/transient-expression position, illegal in any resting
position — and it materializes (the inner `Ref`s become owned) the moment it
crosses a storage boundary. `Cell c = v.get(i)` is fine (`c` is owned `Cell`; the
`Option[Ref]` never rests); `Option[Ref[Cell]] c = v.get(i)` is not. **Open audit:
does binding `Option[Ref[T]]` occur in the current tree today?** If so, the
unstorable rule is a behavior change that needs a migration.

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

- **The decisive objection is legality.** `Ref[T]`-default makes `g.at(0).n = v`
  write through *by default* for any projection-of-self return. That is precisely
  an **accept/reject change**, and a body-dependent one: whether
  `grid.at(x,y).mark()` mutates the grid or a dead copy would hinge on whether
  `at`'s body returns a projection or an owned value — invisible to the caller.
  This stands on its own, independent of any perf claim.
- **Perf gained: post-#13, none; pre-#13, a read-clone (corrected 2026-07-22).**
  The original draft argued "Ref-default buys no perf because #13 makes owned `T`
  reads free." That leaned on #13 being shipped — which it is **not** (its yield is
  unproven, ~3.5% of leaf clones). Pre-#13 an owned `T` return **clones on read**,
  so Ref-default *would* save that clone. The perf argument is therefore
  contingent; the **legality** argument above is the standalone reason.

So `Ref[T]`-as-default moves a legality-affecting fact into the invisible-default
zone — the wrong side of the "a change must not affect legality" line. It also inverts Gorget's value-semantics ethos (reading/
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

Reading the columns: **once #13 ships**, `T` and `Ref[T]` returns are physically
identical for read-only use; binds materialize for both; the only observable
difference the annotation buys is the mutate-through line. `Ref[T]` is "owned
semantics that #13 makes cheap to read, **plus** permission to mutate the
transient through to the source." ⚠ **This read-parity is #13's goal, not a
current fact** — pre-#13 the owned `T` return still clones on read. The soundness
and annotate decisions do **not** depend on it (see the corrections section).

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

## Critical review & corrections (2026-07-22)

Two independent critical passes (orchestrator self-review + an external "Grok"
review) converged: **good north star, wrong as a big-bang ship.** The corrections
they produced:

- **"Total resolver" is scoped to the method-call costume.** View-of-self typing
  makes the *method-chain* resolution total; it does **not** address the other
  silent-lost-write costumes — scalar `&c.fd` formation (TODO L112), nested
  `&outer.inner` (snag #53), closure-body `&`-formation (L106). Those are
  `&`-*formation* bugs (the root IS nameable), fixed by their own tracks, not by
  this model. This model fixes one costume, not the whole lost-write class.

- **Soundness ≠ the feature.** The soundness win is Rule 1 (place-gate + reject
  genuinely-unresolvable) plus typing the *builtin* view-chains (the Core #2
  whitelist replacement — behavior-preserving). That is small, cheap, and needs
  **no** §3.7 concession. User-declared `Ref[T]` mutable accessors are a separable
  **language feature** — that is where the borrowed-return-type concession lives.

- **The unexamined fork: closure mutators.** `grid.update(x, y, (Cell &c): c.mark())`
  — a transient, scoped, already-legal `&`-param closure — covers the user
  mutate-through use case (chaining via block closure, returning a value,
  encapsulation) with **zero new type surface and no §3.7 concession**. This makes
  user-visible `Ref[T]` largely *call-site sugar*. Phase B below is therefore
  **contestable, possibly never** — not "inevitable, later." Decide surface-`Ref`
  vs closure-mutators on ergonomics before building it.

- **Migration is narrower than "half the cost."** Turning owned-return mutate-
  through from silent-no-op into a reject is cheap and good (surfaces latent bugs;
  no correct program depended on a discarded write). The real cost is **not
  regressing currently-working `.get()` chains** when the whitelist is replaced by
  type-driven descent: in phase A, prove the type-driven descent covers every
  chain the whitelist did (corpus audit) *before* arming the Rule-1 reject.

- **Do not sell this as "the #13 round."** #13's read-clone reclaim is unshipped
  and unproven-yield; the soundness slice must stand without it (it does).

**Phasing (both reviews agree):**
- **A — soundness slice (near-term-eligible):** typed view-of-self flag (builtins
  first, replacing the name whitelist, coverage-proven) + Rule 1 place-gate reject
  for owned-return mutate-through. Kills the method-chain lost-write costume. No
  surface `Ref[T]`, no §3.7 concession.
- **B — user surface (contestable):** user-writable `Ref[T]` returns + view-of-self
  for user methods. Gated on the closure-mutator decision. This is a language-
  design pivot, not a CoW increment — treat the §3.7 concession as a real cost.
- **C — #13 reclaim (measurement-gated):** the read-clone elision, behind leaf-
  yield measurement + memory gates.

**Not the next round.** The near-term excellence moves remain the both-lane
soundness bugs this model does NOT cover (scalar `&`-formation, snag #53, closure
`&`-formation) + the SH Core #9 lag pack + #13's SH-excess/leaf measurement. This
note is the architecture that eventually unifies the lost-write class and #13 —
not a free win tomorrow.

## Status ledger

- **RULED (owner 2026-07-22):** no stored borrows; `a = &f()` shelved. Transient
  views only. **No user-visible `Ref[T]`** — views internal to builtins only;
  user-method mutate-through is rejected; **closures are the sanctioned user
  mutate-through path** ("one correct way"). §3.7 stays true. Phase B (user Ref)
  and the "place alias" are shelved.
- **CHOSEN DESIGN:** Rule 1 place-gate + typed builtin `returns_view` (replacing
  the whitelist) + transitive-unstorable as an internal compiler guard (no storage
  slot holds a `Ref`-containing type) + closures for user mutation.
- **OPEN:** migration = prove type-driven builtin descent covers every current
  `.get()`-chain before arming the Rule-1 reject (corpus audit); the internal
  transitive-unstorable audit (does any current path bind/store an
  `Option[Ref[T]]`?); whether a conservative *inference-to-reject* assist helps.
- **NOT IMPLEMENTED.** No code exists for user-writable `Ref[T]` returns,
  view-of-self for user methods, or the place-lvalue gate beyond the landed
  `E_InvalidAssignTarget`.
