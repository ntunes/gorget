# CoW Cost Contract — making the copy cost legible, elidable, and assertable

> **STATUS: DESIGN NOTE — owner-led brainstorm, 2026-07-28.** The knob spelling is
> owner-chosen; the rest is proposed/leaning. **NOT ratified as a whole and NOT
> implemented.** Do not treat any of this as shipped behavior. When ratified +
> shipped, the guaranteed-elision set graduates to `docs/language-reference.md`
> and the knob to `docs/language-reference.md` §5.11 + §"Directives"; until then
> this file is the single durable record of the design.
>
> **Pairs with** [`cow-transient-view-model.md`](cow-transient-view-model.md) (the
> *legality* axis — place-gate, `returns_view`, no user `Ref[T]`) and
> [`unified-resource-model.md`](unified-resource-model.md) §6 (#13, the
> return-boundary elision, which this note absorbs — see § Articulation).
> This note owns the **cost** axis: nothing here changes accept/reject.

## The problem

CoW's contract is *"clones are always correct"* — and that is true. The 2026-04-05
model note retired the old implicit-clone warnings on the grounds that clones are
*"always correct **and intentional**"* (`copy-on-write.md:190`). The second half is
false, and the gap it hides is measured below: **correctness was never the
question; the COUNT was, and nobody chose the count.**

A bare param is materialized when the callee writes to it — one private copy per
frame, which is the minimum for value semantics. Under recursion the multiplicity
is unbounded and invisible in the source:

```gorget
struct Big:
    Vector[int] xs

void mutate(Big &b):
    b.xs.push(1)

void walk(int d, Big b):          # bare param — value semantics
    mutate(&b)                    # materialize: this frame's private copy
    if d > 0:
        walk(d - 1, b)            # bare arg → callee borrows → callee materializes
```

Measured at depth 200 (regenerate: build with `--clones=stats`, run, read the
`[clone-stats] array_clone=` field):

| shape | spelling | array clones |
|---|---|---|
| linear recursion, depth 200 | bare param, bare arg | **201** |
| " | hand-optimal (`Big !b` worker + one explicit clone at the boundary) | **1** |
| branching recursion, depth 12 | bare param, bare args | **8191** |
| " | hand-optimal | **4096** |

All four print the same result — the caller's value is untouched in every variant.
So the 201 is pure waste and the 8191 is half waste.

The remaining three probes, in full (the shape above is probe 1; these are the
fixture seeds §3 refers to — same `Big` / `mutate` preamble in each):

```gorget
# probe 2 — linear, hand-optimal: 1 clone at depth 200
void walk_inner(int d, Big !b):        # consumes: no per-frame materialize
    mutate(&b)
    if d > 0:
        walk_inner(d - 1, !b)
void walk(int d, Big b):               # value semantics preserved at the boundary
    Big local = b.clone()              # the ONE clone, explicit
    walk_inner(d, !local)

# probe 3 — branching, today: 8191 clones at depth 12
void rec(int d, Big b):
    mutate(&b)
    if d > 0:
        rec(d - 1, b)
        rec(d - 1, b)

# probe 4 — branching, hand-optimal: 4096 clones at depth 12 (the 2x ceiling)
void rec_inner(int d, Big !b):
    mutate(&b)
    if d > 0:
        Big c = b.clone()              # child 1 needs its own — b is still live
        rec_inner(d - 1, !c)
        rec_inner(d - 1, !b)           # child 2 is the last use — move
void rec(int d, Big b):
    Big local = b.clone()
    rec_inner(d, !local)
```

Note what probe 2 demonstrates about the *user's* options today: the elision is
not expressible at the call site. `walk(d - 1, !b)` on a bare param is
`E_OwnershipMismatch` ("the parameter only borrows — it does not consume the
argument"), so reaching the hand-optimal count requires changing the callee's
**signature** — a different API contract, not an optimization. That is the gap §2
closes without any user syntax at all.

Three properties of this defect class matter for the design:

1. **It is a TIME bomb, not a space bomb.** Each frame's copy dies at frame exit,
   so peak RSS is `O(depth × |Big|)`. Per-site counters are the right instrument;
   RSS would never show it.
2. **The exponential in the branching case is a property of the program's
   MEANING, not of our lowering.** At every node the first child's copy is
   genuinely observable (the parent still needs its pre-child value for the second
   call). No optimizer reaches below `2^d`; the measured ceiling is exactly 2×.
   The linear case, by contrast, is fully reclaimable: `O(depth) → O(1)`.
3. **The instrument that found it in production was a test deadline.** The
   self-host bootstrap's recursive `check_safety_stmt` visitor took a deep
   `ScopeTable__clone` per frame and blew its 600s stage deadline (recorded in
   [`devbook/11`](../devbook/11-copy-on-write.md), Consumer #1 row — a historical
   measurement, not a live figure). Root-caused by stack-sampling; no diagnostic
   existed at the time.

**What already ships — and the gap it leaves.** Round X (Track I, `1c594a4f`,
2026-07-28) landed **`W_RecursiveBareParamMaterialize`**, on the owner ruling to
*keep* §3.1's tolerance and steer users rather than reject the mutation. It fires
on a bare resource param mutated via any of five sites and reaching a bare-borrow
arg of a **direct self-recursive** call, and its message names both reference-grade
fixes (`&param` + `&arg` at callers, or an explicit `param.clone()`). Verified
firing on probe 1 at HEAD. That closes the *acute* problem — the linear cliff is no
longer silent — and everything below is the sequel, not a replacement.

The residual, **measured 2026-07-28 at HEAD**: the warning is *direct*
self-recursion only. A two-function cycle pays the identical cost in silence —
`ping → pong → ping` at depth 200 emits **zero warnings** and clones **201** times,
byte-for-byte the shape that does warn when it recurses directly. Generalising the
predicate from "direct self-call" to "reaches a bare-borrow arg of a call inside
this function's SCC" is the call-graph-summary version of the same question, and it
is a natural first consumer of §1 (the SCC is a by-product of the call-graph pass
the summary needs anyway).

**The failure quadrant.** Gorget's runtime stack overflow is a bare `SIGSEGV`, no
message (verify: build a `deep(100000000)` recursion and run the binary directly —
`rc=139`; since Round XXIV Track B, `gg run` also propagates 128+signo as exit
128+N with a stderr diagnostic, so `rc=139` reads off `gg run` too). The clone bomb is worse
in one respect, because a SIGSEGV at least *stops*. Post-Track-I the clone bomb has
left the silent quadrant for direct self-recursion; it remains there for **mutual
recursion** (measured above) and for any non-recursive path whose multiplicity is
high but statically unbounded. The stack overflow is still fully silent — the
C-without-stack-probes position, not the Rust guard-page-plus-probe position.

## What this is NOT

- **NOT a bug in the materialize.** Under value semantics the copy is an
  implementation detail of the semantics. The write site is doing the only sound
  thing: `&`-of-a-bare-param must materialize, or the callee's write-through would
  reach the caller and break the param's value semantics.
- **NOT a case for rejecting the mutation.** Owner-ruled 2026-07-28 (Round X):
  §3.1's tolerance stays, because the caller-side "bare means you keep your value"
  guarantee is load-bearing for D31's contract-at-the-call-site model. The remedy
  is to steer (`W_RecursiveBareParamMaterialize`, shipped) and then to elide and
  let the user assert (this note).
- **NOT a case for marking the copy in source.** A sigil for "implicit copy
  happens here" was considered and rejected: it taxes every honest by-value helper
  to catch a pattern that only bites under recursion and in hot loops, and it
  contradicts value semantics (the copy is ours to optimize, not the user's to
  annotate). What the user is owed is that the compiler *elide what it can* and
  *say so when it cannot*.
- **NOT a legality change anywhere in this note.** Every mechanism below is
  accept/reject-neutral by construction. The knob (§4) is the sole exception and it
  is **opt-in**, never a default.

## The design — four layers

### 1. The signature summary (the missing layer)

Two facts must cross the function boundary, per param, on the **signature**:

| fact | direction | today |
|---|---|---|
| `materializes_param[i]` — does the callee privatise this param? | in | `fn_consumed_params` — a lowering-time **sidecar map keyed by function-name string** (`src/ir/lowering/context.rs:615`), populated *during* lowering by `record_param_cloned` (`:1205`), so it is order-dependent and absent for not-yet-lowered callees |
| `returns_view_of[i]` — is the result a view of this param? | out | `returns_view` on `BuiltinMethodDecl` (builtins only); `language-design.md` §3.6 lists transitive user-function view provenance as **(Planned)** |

Two half-built halves of one summary, in two places, neither typed onto the
signature. Per [layering discipline](../devbook/24-layering-discipline.md) rule 3
(*one source of truth per axis, read through one accessor*) and rule 4 (*resolve
once, write through*), `fn_consumed_params` is exactly the parallel sidecar map the
doctrine forbids.

**The layer:** a per-signature ownership summary, computed **bottom-up over the
monomorphized call graph** with a **fixed point** for recursion (start optimistic:
assume the recursive callee does not materialize, iterate to stability). Read via
typed accessors. `returns_view_of[i]` remains *owned* by the transient-view model
(it is legality-determining there); this layer reads it, never redefines it.

Consumers: arg-side elision (§2), #13's return-boundary elision, and the knob (§4).
Three consumers, one substrate — the pattern the `MaterializePlan` round used
("substrate built with a real consumer").

### 2. Arg-side elision (consumer 1 — inferred, no syntax)

At a call site where the arg is an owned local at its last use and the callee
materializes that param, **transfer ownership instead of borrowing**; the callee
skips its materialize. Observationally identical — the caller provably never reads
the value again. This is Swift's `@guaranteed` → `@owned` convention
specialization.

The predicate is **not** `materializes_param[i] ∧ is_last_use`. It is:

```
materializes_param[i] ∧ is_last_use ∧ ¬returns_view_of[i]
```

**Why the third conjunct** (the hazard that proves the summary must be ONE layer):
for a method, `self` *is* arg 0, and Rule 2 of the transient-view model says a view
is a view of `self`. Given `Ref[Cell] at(self, int i)` and a receiver at last use,
eliding by move hands `at`'s frame ownership of the buffer; `at` returns a view
into it and drops it at return — **the returned view dangles.** Two independent
sidecar tables would have shipped this. One summary, consulted as a conjunction,
cannot.

**Polarity:** materialize-when-unsure, exactly as #13. A summary that cannot prove
deadness clones. Unlike #13 there is no live alias to invalidate, so this consumer
has **no UAF class** — it is the low-risk half of the same axis, with a measured
201× on the shape #13's own ruling calls unproven at ~3.5%.

**Precondition already computed, currently mis-consumed.** `calls.rs:1880-1920`
evaluates *this exact conjunction's first two terms* and spends it on a
`MoveSuggestion` — advice to write `!arg`, which is `E_OwnershipMismatch` at a bare
param (filed, with a durable repro, at `TODO.md`'s
`sound_move_suggestion_advises_rejected_code` entry). That filing's disposition is
"stop suggesting", which is right about the advice (D31 ratified `!` as contractual
consumption, not a perf knob) and would retire a valid signal along with it. **Fold
the two: stop advising, and route the precondition into the elision**, where no user
syntax is required at all.

### 3. The guaranteed-elision set (the spec)

The C++17 move. Before C++17, RVO was best-effort and nobody could rely on it;
making elision *mandatory in named cases* is what made returning big values by
value idiomatic. Best-effort elision is unusable for writing performance-critical
code, because nothing in the source tells you which mode you got.

**Publish, in `language-reference.md`, the set of shapes where no implicit clone is
emitted — guaranteed — and pin each with a clone-count fixture.** The instrument
already exists (`--clones=sites|verbose|stats|sites-tsv`, the `[clone-stats]`
atexit line, `scripts/clone_attribution.sh`, and `tests/integration.rs`'s existing
`[clone-stats]` parser). What is missing is the **contract**. This converts
`--clones` from a debugging aid into a **conformance surface**, and it is Core #6
applied to the feature as a whole: a prose promise about performance rots, a
clone-count ratchet does not.

The four probes in § The problem are the seed of that fixture set (they are recorded
here rather than left in `/tmp`; each is ~15 lines and belongs in the corpus with an
asserted clone count).

**This layer is a prerequisite for §4, not an optional companion** — see the
compatibility hazard below.

### 4. The knob — `implicit_clones`, three scopes, one name

The `tailrec` design generalized: an annotation that does nothing except make the
compiler **prove a property or error**. `tailrec` does not make your function
tail-recursive; it tells you when it isn't.

This is the sequel to `W_RecursiveBareParamMaterialize`, not a duplicate of it. The
warning **steers** on one recognised shape the compiler chose to flag; the knob lets
the author **assert** the property over a function, module, or project and have it
discharged — including on shapes no heuristic nominates (a hot non-recursive path, a
mutual cycle, a `.map()` receiver). Warning: compiler picks the shape, user reads.
Knob: user picks the scope, compiler proves.

| scope | spelling |
|---|---|
| project | `--implicit-clones=deny` |
| module | `directive implicit-clones=deny` |
| function | `@implicit_clones(deny)` |

Values: **`allow`** (default) · **`warn`** · **`deny`**.

```gorget
directive implicit-clones=deny            # module policy

@implicit_clones(deny)
void walk(int d, Big b):
    mutate(&b)      # ERROR: implicit clone of `Big`; `Big &b` writes through,
                    #        or clone explicitly
    walk(d - 1, b)

@implicit_clones(allow)                   # this one function opts back out
void slow_path(Big b): …
```

**The contract:** *every copy in this function is one I wrote.* An **explicit
`.clone()` is exempt** — only *implicit* clones are diagnosed. So the mechanical
fix for a violation is to write the clone, which is the good outcome: the cost
becomes visible in the function whose author asked to see it, and stays an
invisible implementation detail everywhere else.

**Why this spelling** (owner-chosen 2026-07-28, over `@explicit_clones_only`,
`@no_implicit_clones`, `@clone_budget(0)`, `@zero_copy`):

- **The escape hatch needs the same name.** A boolean spelling requires a second,
  differently-named antiknob (`@allow_implicit_clones`) to exempt one function
  inside an opted-in module — two names and two validation paths for one axis. The
  valued form is the `#[allow(...)]` shape everyone reads fluently.
- **The warn tier is not optional and comes free.** Every comparable change in this
  tree burned down through a warning first (row 2E's dead-bare-param-write shipped
  as an on-by-default `W_` promoted to `E_` after corpus burn-down; 2T the same). A
  project-wide `deny` over an existing corpus is unusable without `warn` as a
  staging level, and under a boolean name `warn` has no home.
- **The shape already exists, validated identically.** `directive scheduler=X` ↔
  `--scheduler=X` (`language-reference.md:5122`), with a closed value set checked at
  `src/semantic/mod.rs:173-186` — adjacent to the code that rejects unknown
  attributes.
- **It keeps the tree's vocabulary.** "Implicit clone" is the ratified term across
  `ImplicitCloneReason`, the `--clones` family, `language-design.md:416`,
  `language-reference.md:2528`, and `TODO.md`'s *"Never implicitly clone except when
  absolutely necessary for safety."* The knob should use the same word as the
  instrument that measures it.
- **Rejected: `clone_budget(N)`** — a static site count is not an execution count,
  so "budget 3" invites exactly the site-vs-multiplicity confusion that produced
  this class (201 executions, **one** site). **Rejected: `zero_copy`** — already
  means I/O without buffer copies (`sendfile`/`mmap`).

**Spelling mechanics (verified at HEAD):** attribute names are lexed as
identifiers, so hyphens do not parse in attribute position (`@explicit-clones-only`
→ `expected type, found '-'`); attributes are snake_case (`should_panic`,
`timeout`, `derive`) and directives are kebab-case (`strip-asserts`, `hot-reload`),
hence the punctuation split in the table above. Attribute names are **closed-world
validated** — `validate_attributes` (`src/semantic/mod.rs:215-218`) rejects any
unknown name with `E_UnknownDirective`. That property is load-bearing: an
annotation that can be silently misspelled into a no-op is a guard that
green-lights its own class, i.e. worse than none.

**Two rules that decide whether it works:**

- **It is interprocedural or it is a lie.** `@implicit_clones(deny)` on a function
  calling `mutate(&b)` must consult `mutate`'s summary. So the knob is a *consumer*
  of §1, not an independent feature. For **un-summarizable callees** (`extern`,
  dynamic dispatch through `Callable`/vtable) the checker **fails the annotation**:
  cannot prove ⇒ error. Note the polarity is the *opposite* of the optimizer's
  (clone-biased for safety vs error-biased for honesty) — both are "assume the
  worst", and getting it backwards yields an annotation that lies. The escape for
  extern is an **assertion** on the extern declaration ("trust me"), which must be
  kept typographically and conceptually distinct from the *proven* claim on a
  Gorget function; conflating them turns a checked contract into a hope.
- **Transitive guarantee, non-transitive obligation.** Because summaries are
  computed bottom-up with a fixed point, checking `f` already accounts for every
  implicit clone performed anywhere beneath `f` — a wrapper three levels up is
  covered. What does *not* propagate is the **obligation**: callees need no
  annotation. Transitive obligation would demand an annotation where a fact
  suffices — forcing "annotate the world" churn (the `const fn`/`constexpr`
  disease, where transitivity is *necessary* because no summary can substitute;
  here it is not), exporting a local performance requirement as a global API
  constraint, and expanding the blast radius of any callee body edit from "my
  callers" to "everything transitively upstream". The one legitimate benefit of
  transitivity — a library author publishing "I am clone-free" as a stable promise
  — is already served by that author putting the attribute on their own function.
  (`noexcept` is the counter-precedent: it deliberately does not require callees to
  be `noexcept`, and would have been unusable if it had.)

## Articulation with transient-views and #13 — merge the cost axis, not the legality axis

The transient-view model already draws the decisive line, for `Ref[T]` vs #13:

> `Ref[T]` annotation = a *visible semantic contract* … changes what is legal →
> **annotated**. #13 = an *invisible physical optimization* … changes only
> performance → **inferred**.

Everything in this note sits on the #13 side of that line. So the cleave is not
three-into-one:

- **#13 merges INTO this note.** Return-boundary elision and arg-boundary elision
  are the same mechanism at opposite ends of the call — propagate a
  provenance/liveness fact across the boundary so a materialize can be elided;
  materialize-when-unsure; invisible; zero legality change. They share §1 as
  substrate and they must share it (the dangling-view conjunction in §2 is not
  expressible across two documents' worth of separately-owned tables). The
  transient-view note's "Articulation with #13" section becomes a cross-reference
  here.
- **The transient-view model stays separate.** It owns the *legality* axis: Rule 1
  place-gate, the typed `returns_view` flag, transitive-unstorable as an internal
  guard, closures as the sanctioned user mutate-through path, no user `Ref[T]`. It
  has RULED items and an agreed A/B/C phasing; folding an unratified cost design
  into it would blur what is ruled vs leaning vs open, and would re-open settled
  questions for no gain.
- **`returns_view_of[i]` has one owner and two consumers.** Defined by the
  transient-view model (where it is legality-determining), stored as a typed
  signature field, read by the place-gate *and* by the elision predicate. One
  source of truth, two readers — layering rules 3 and 4, satisfied.

Concretely: do **not** merge #13's risk profile into the arg-side elision's
schedule. #13 is self-described as high-risk (UAF-prone, R6 realloc hazard) and
measurement-gated at unproven ~3.5% yield; the arg side has no live alias and a
measured 201×. Sharing substrate is not sharing gates. The transient-view note's own
warning applies verbatim: *"Do not sell this as 'the #13 round'."*

## Risks and open questions

- **The compatibility hazard, and the ordering it forces.** An annotation whose
  meaning depends on the optimizer's *current* precision pins user code to compiler
  internals: a future analysis change, or a legitimate callee body edit, can flip a
  caller's annotation remotely — action-at-a-distance on legality, which is the
  exact objection the transient-view note used to reject `Ref[T]`-by-default.
  Opt-in bounds the blast radius to code that asked for it, but does not remove it.
  **Mitigation is the ordering: §3 before §4.** `deny` must mean "I stay inside the
  *specified* elision set", not "the optimizer happened to manage it this week."
  Without §3 the knob is a promise about implementation details; with it, it is a
  language feature with a stable meaning. **This sequencing is load-bearing, not a
  nicety.**
- **Summary computation vs. lowering order.** `fn_consumed_params` is populated
  during lowering today. A pre-pass over the monomorphized call graph is new
  machinery, and the fixed point for mutual recursion needs a soundness argument
  (optimistic start + iterate to stability, with the *pessimistic* direction used
  for the §4 checker).
- **Convention specialization cost.** A callee reached both ways needs either two
  entry points (`walk$owned`, code-size cost) or a uniform convention. Not a
  flag-and-branch at runtime.
- **Open:** does `warn` fire per site or per function? Per site is more actionable;
  per function is quieter. Leaning per site with the standard dedup.
- **Open:** does the knob interact with the *runtime* tripwire idea (an
  `__gorget_clone_site_hit` threshold that names a site once it crosses N)? They are
  complements — the knob is static and local, the tripwire catches unbounded
  multiplicity the static check cannot see (a `deny`-clean function can still be
  called 2^N times). The tripwire is a small delta over the existing templated
  `runtime_clone_sites.c` and is *not* specified here.
- **Open:** the branching-recursion residue. After perfect elision the measured
  ceiling is 2× — `2^d` survives, by the program's meaning. The knob makes the
  *site* visible; nothing makes the *count* visible statically. This is the case
  where a diagnostic is the only remaining service, and it is why the tripwire
  question above is not decorative.

## Phasing

- **A — the summary layer + arg-side elision.** `materializes_param[i]` /
  `returns_view_of[i]` as typed signature fields over the monomorphized call graph
  (fixed point for recursion), with the arg-side elision as its first consumer and
  the `MoveSuggestion` filing folded in. Measured end-to-end on the self-host's
  recursive visitors. **No user-visible change whatsoever.**
- **B — the guaranteed-elision set.** Specify in `language-reference.md`; pin with
  clone-count fixtures promoted from the four probes above. Turns `--clones` into a
  conformance surface.
- **C — the knob.** `@implicit_clones` / `directive implicit-clones` /
  `--implicit-clones`, `allow|warn|deny`, checked against B's specified set.
  Corpus burn-down through `warn` before any `deny` default anywhere.
- **D — #13**, riding A, whenever its own measurement gate justifies its UAF risk.

Order is not negotiable between B and C (see the compatibility hazard). A is
independently valuable and independently shippable.

## Status ledger

- **OWNER-CHOSEN (2026-07-28):** the knob spelling and scoping — `implicit_clones`
  with `allow|warn|deny` values, three scopes (attribute / directive / CLI flag),
  explicit `.clone()` exempt. Selected over `@explicit_clones_only` /
  `@no_implicit_clones` / `@clone_budget(N)` / `@zero_copy`.
- **LEANING (this note, unratified):** the four-layer design; #13 merging into the
  cost axis while the transient-view model keeps the legality axis;
  transitive-guarantee / non-transitive-obligation; error-biased checker vs
  clone-biased optimizer; §3-before-§4 ordering.
- **ALREADY SHIPPED (Round X, `1c594a4f`, 2026-07-28):**
  `W_RecursiveBareParamMaterialize` — the steering diagnostic for *direct*
  self-recursion, with 7 RED-verified fixtures including a load-bearing
  false-positive control. This note does NOT re-propose it.
- **MEASURED AT HEAD (2026-07-28, post-Round-X):** 201→1 linear, 8191→4096
  branching (the 2× ceiling), all variants observationally identical; the warning
  fires on the linear bare shape; **mutual recursion `ping→pong→ping` clones 201
  and warns ZERO times** (the SCC residual); stack overflow is a bare `rc=139`;
  `@inline` is rejected at HEAD though `language-reference.md:595` advertises it;
  hyphens do not lex in attribute position.
- **OPEN:** everything under § Risks and open questions; whether the runtime
  tripwire is in scope at all.
- **NOT IMPLEMENTED.** No code exists for the summary layer, either elision
  direction, the specified elision set, or the knob.
