# Define Gorget — Decision Ledger

> The working ledger of the **Define Gorget** project (owner-approved 2026-07-05): building the
> executable definition of Gorget's semantics and verifying all implementations against it.
> Raw scout material in [`scouts/`](scouts/). Every decision here is subject to two standing rules:
>
> 1. **Docs write-through is part of landing a decision** (owner directive 2026-07-05): a decision
>    is not DONE until `docs/language-design.md`, the book, and the devbook agree with it.
> 2. A decision "lands" fully only when the executable rule (in the definitional interpreter),
>    its conformance tests, and the doc sections all agree — same-PR-gated once the spec
>    infrastructure exists (see Architecture direction below).
>
> Context: the self-host, gorget-js, and gorget-arena are parallel END-TO-END CONFIRMATION efforts
> (real programs proving Gorget works); this project is the COHERENCE effort (proving the language
> means one thing). They complement, not substitute.

## DECIDED (owner, 2026-07-05 — batch 1)

### D1. Observable semantics = VALUE SEMANTICS, plus a normative copy-guarantees annexe
The spec's dynamic semantics is **eager value semantics**: a bind captures the value as-of the
bind point; copy TIMING and placement are unobservable implementation freedom. Lazy CoW is not
part of the meaning — it is each implementation's **refinement obligation** ("CoW is an
optimization; value semantics is the meaning"). Additionally, a small **normative annexe of
copy guarantees** makes the language's zero-copy promises testable spec (bare binds, reads,
borrow-passing MUST NOT allocate — the README's promises, enforced via clone-stats/allocator
probes on designated annexe tests, not via the value-semantics evaluator). An
eager-copy-everything implementation is nonconformant (annexe), and a lazy implementation that
changes an observable value is nonconformant (semantics).

**Derived consequences (no further decision needed):**
- **EMove value-bug** (devbook/11 "Rust gg VALUE-WRONG on both EMove shapes") is definitively a
  Rust-gg implementation bug. Pre-mutation value is the spec. → conformance tests + fix.
- **All write-through gaps are implementation bugs toward §3.1**, not open questions:
  `for x in &coll` element write-through lost (TODO:23), `v[i].field = x` dropped writes
  (TODO:35-36), untracked alias chains writing through (devbook/11 "one remaining unconverged
  shape"). Direction: materialize/write-through per the documented model.
- **Closure captures are capture-by-VALUE** (value as-of closure creation) — forced by D1.
  The docs' phrase "immutable borrow capture" is a naming error to fix (write-through: design
  §7.3, book ch.4/16): captures copy (with CoW machinery free to defer the physical copy).
  One genuine sub-fork remains → O2 below (mutable captures).
- **`auto e = v.get(0).unwrap(); v.push(...)` is legal and `e` keeps its bind-time value** —
  a value bind, never an in-place borrow that push can invalidate. Book ch.12's
  "MutationWhileBorrowed = compile error" claim is wrong and gets rewritten; the latent
  UAF-after-realloc shape (TODO:664) is an implementation bug (materialize on invalidation),
  not a semantics question. Resolves scout-A Q22 / scout-B A4.
- **Allocator introspection (`pool.bytes_used()`, `--clones=stats`) observes the
  implementation, not the language** — spec text states this explicitly (like measuring time).
- The self-host-vs-Rust clone-count divergence (0 vs 1 on alias-deadpath, devbook/11:646-656)
  is ALLOWED variation under D1 semantics — but see O1: it must be unobservable through drops.

### D2. Plain-`self` mutation = UNIFORM CoW (materialize a private copy)
`self` follows the same rule as every bare binding: a write through plain `self` materializes a
private copy; the caller is untouched; `&self` is the write-through opt-in. This ratifies what
§3.1/§4.5 already say and makes both compilers wrong today (both write through — TODO:948).
**Consequences:** fix BOTH compilers; one-time migration sweep of self-host / gorget-js /
gorget-arena for methods relying on write-through (the DeadBareParamWrite lint catches
fully-dead copies; write-then-read sites need the sweep); extend the lint to `self`; docs
already agree (confirm + strengthen with the decision reference).

### D3. Assurance bar v1 = Wasm-grade
Executable spec (definitional interpreter) + versioned conformance suite + continuous
differential verification of all implementations. Core calculus designed mechanization-friendly
(small, total, fuel-indexed evaluation, explicit nondeterminism) but no proofs in v1.
Mechanization is a clean later phase (Lean 4; Aeneas/Charon translation path exists).

## OPEN — batch 2 (put to owner 2026-07-05)

### O1. Drop side effects × clone placement — the direct corollary of D1
A custom `Drop` makes clone count observable: every implicit clone is an owned value whose drop
runs (a printing `drop` puts clone count in stdout). D1 says copy placement is implementation
freedom — those compose only if implicit-clone drops can't be observed. Scout-A Q12: "the fork
that turns lazy-vs-eager from perf detail into correctness."
Options: (a) **Drop-purity rule (recommended)** — implicit clones are only available to types
whose transitive drop is side-effect-free; a type with custom `Drop` anywhere in its graph is
move-or-explicit-clone at ownership boundaries. Bonus: the single-owner carve-out list
{Box, Task, TaskGroup, Guard, Owned, Callable} stops being an ad-hoc enumeration and becomes a
DERIVED rule ("single-owner-by-default = has identity or side-effectful drop"), and user types
with custom Drop join automatically — resolves scout-A Q20 too. (b) eager drop-count is
normative (kills clone elision for Drop types; spec must count clones). (c) drop side effects
non-normative (cross-implementation nondeterministic stdout — violates invariant #8; rejected
unless owner overrides).

### O2. Mutable closure captures: inferred vs explicit sigil
Today the compiler INFERS mutable capture (pointer to the outer slot — real write-through
aliasing) from whether the closure body mutates the variable (§7.3). Everywhere else in Gorget,
mutation reaching an outer owner requires a visible `&`. Inferred mutable capture means adding
a mutation to a closure body silently CHANGES the capture semantics of the variable.
Options: (a) keep inference (ergonomic: counters in closures just work; current behavior);
(b) require an explicit marker for write-through captures (uniform with `&` everywhere else;
loud; bare captures become always-by-value).

### O3. Unbound bare `Ok(e)`/`Error(e)` combinator chains (error-model §9 Q17)
`Ok(5).map(...).unwrap_or(0)` with no `Result[T,E]` in scope has no `E` — both compilers
currently miscompile it (Core #8, latent, TODO:153). Options: (a) reject, require annotation
(smallest, monomorphization-friendly); (b) default-to-existential error type (`anyerror`-style —
a new feature); (c) infer error sets from the function body (Zig-style — a Result redesign).

## OPEN — queue (later batches; from scout-B List A, scout-A tiers)

- A5 resource-valued `Dict.get_or`/`get_or_put` ownership (both compilers double-free today)
- A6 slice representation: reject-escape is already filed as the near-term fix (TODO:465);
  the open question is whether a real slice fat-pointer ever ships
- A7 `n.to_string()` on primitives: implement vs reject-pointing-at-display
- A9 meta-stmt inside `on error`: evaluate vs reject (both compilers silently drop it)
- A10 bare allocator locals: destroy semantics vs documented leak (escalated: `.destroy()`
  before a live RAII value = heap-UAF from safe code)
- A11 UFCS: decide+implement or remove from design targets (collides with §4.5 auto-borrow)
- A12 book stdlib-contract contradictions (`parse_int` error type; `read_file` throws-vs-panics)
- A13 `Break(Some(e))` binding; A14 op-overload compound-assign on resource elements;
  A15 bare `return` in `int throws E`; A16 error-model §9 residuals (fast knob, meta-overflow,
  `Never` spelling, Result reconciliation, fault set)
- A17 `gg sim` disposition (owner-decision entry already in TODO)
- A18–A28 "ratify the rejection" batch (one-compiler-only or obvious-reject items) — gated by
  the owner's phase rule (TODO C12: bounded rejections now, general enforcement pass after
  parity)

## RATIFIED-BY-FIXTURE rules to encode in the spec (scout-B List C)
C1 dead-branch alias bind = zero clones, correct output · C2 bare-assign aliases sever on first
mutation · C3 `&` of an owned root writes through with no materialize · C4 mutating method on
`param.field` via bare param materializes the root · C5 sever-order/staletag rules ·
C6 Set ordered / HashSet unordered · C7 bare bind of a resource static deep-clones ·
C8 mutated-after-bind deferred clones fire eagerly at the bind · C9 operator-overload dispatch =
bare `Type__method`, no vtable · C10 `String.display()` = identity · C11 unbounded recursion →
OS-guard SIGSEGV accepted by design · C12 rejection-phasing meta-rule.

## CONFORMANCE-ONLY backlog (intended semantics clear; no decision needed)
Scout-B List B (B1–B19): each becomes a spectest with the already-known-correct expectation.
Highlights: B1 dead-branch alias SIGSEGV (`9`), B2 `String !p`+concat (`ablog`), B3 `for in &`
write-through (`101`), B6 `Shared[resource]` inner-payload deep-drop, B7 `!`-param single-drop,
B9 lock = single-owner Resource, B10 Task join-on-drop, B16 arena escape = reject.

## ARCHITECTURE DIRECTION (pre-RFC; from scout-C prior art)
- **The triad in one repo** (Wasm model): spec prose + definitional interpreter + conformance
  suite, merge-gated together. Names (tentative): core language **GGC** (Gorget Core), binary
  **`ggdef`**, suite **`spectests/`**. Charter sentence: *clarity and simplicity, not speed.*
- **`ggdef` = hand-written definitional interpreter in safe Rust** (not K, not DSL-first, not
  Lean-first, not Gorget-itself yet), MiniRust-style: one fuel-indexed eval function (CakeML),
  explicit `Nondet` for the only admitted nondeterminism (hash order, scheduling; seed-swept),
  UB defined as what the interpreter detects.
- **Sits over a desugared core, sharing ONLY the lexer+parser** (trusted, prose-spec'd); a NEW
  simple spec elaboration AST→GGC that must never import `src/ir/` or `src/semantic/` —
  enforced by an import ratchet lint. **`gg sim` is permanently disqualified as the definition**
  (it consumes GIR = the compiler's own decisions; the Miri trap).
- **`ggdef` implements eager value semantics** (per D1); lazy CoW is the implementations'
  refinement obligation, differentially tested now, provable later (first theorem when
  mechanization starts: lazy refines eager on GGC).
- **spectests**: fixture frontmatter (run/static-error/parse-error/nondet-seeds/since/features),
  **expectations GENERATED by `ggdef` and human-review-diffed** (never copied from whichever
  backend came first), per-implementation always-pass conformance diagnostics + monotone floors
  in `tests/lints.rs`, a `staging/` low-bar tier, spec versioned+tagged with a changelog.
- **Process**: same-PR gating for semantics-visible changes (prose Δ + ggdef Δ + spectests +
  implementations green or floor-tracked exemption); spec diffs justified by design INTENT,
  never by "matches the implementation" (invariant #8 promoted into spec process).
- Static semantics v1 = prose + expected-error-code fixtures; executable typecheck later.

## LOG
- 2026-07-05: project approved; 3 scouts ran (docs sweep, bug sweep, prior art); batch-1
  decisions D1–D3 taken; batch-2 questions O1–O3 put to owner; ledger created.
