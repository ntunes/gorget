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
**Owner note (2026-07-05):** production Gorget MUST implement this via borrows + lazy CoW (or a
future strategy that is equally fast AND provably achieves the same semantics — the owner is
open to alternatives); the eager formulation is spec-only, never a production strategy. The
annexe is the enforcement: its MUST-NOT-ALLOCATE positions make an eager production
implementation nonconformant by construction.

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

## DECIDED (owner, 2026-07-05 — batch 2)

### D4. Drop-purity rule
**Implicit clones are only available to types whose transitive drop is side-effect-free.** A
type with a custom `Drop` anywhere in its field graph is single-owner-by-default: move (`!x`)
or explicit `x.clone()` at ownership boundaries, compile error otherwise (same error family and
UX as Box/Task today). Explicit `.clone()` stays legal (Rust allows Clone+Drop). Copy elision
is thereby provably unobservable — lazy CoW stays a pure optimization under D1. Precedents:
Rust's Copy∧Drop mutual exclusion; C++'s Rule of Three as the cautionary tale this rule makes
unrepresentable.
**Consequences:** the single-owner carve-out becomes ONE PRINCIPLED RULE (side-effectful drop ⇒
single-owner) PLUS two by-design members (Box, Callable — pure drops, unique by design); user
Drop types join automatically. Implementation: a typed `is_drop_tainted` flag on the type decl
(set at registration, transitively computed, read via accessor — layering rule 2, no
name-matching); `E_MoveWithoutOperator` extends to tainted types at bare-assign + ctor/field-init
sites; any implicit-materialize point for a tainted type = compile error with the move/clone/`&`
fix-it. Migration measured ≈ zero (custom Drop: 22 fixtures — mostly drop_* tests — 1 stdlib
use, 2 self-host uses). RFC pins: bare-assign of tainted types = error (Box-identical, chosen
over borrow-allowed-mutation-forbidden for one-rule simplicity). Drop-count determinism
spectests become writable. Docs write-through: language-design §3.3/§3.4.1/§9, book ch.11
(carve-out section) + ch.16, devbook 10/11/15.

### D5. Closure captures: bare = by-value, write-through requires an explicit sigil
Bare captures are ALWAYS capture-by-value (per D1: value as-of closure creation; CoW machinery
free to defer the physical copy). Write-through (aliasing the outer slot) requires an explicit
marker — exact syntax designed in the RFC (V2's per-variable capture list reserves the space;
placeholder `&(): ...` / `(&name)(...)`). Body-driven INFERENCE of mutable capture is retired:
mutation inside a bare closure mutates the closure's own copy (uniform bare-binding rule; the
DeadBareParamWrite lint family applies).
**Consequences:** docs stop calling by-value captures "immutable borrows" (naming fix:
language-design §7.3 AND §7.4 — whose V2 capture-list examples show bare-name borrow captures,
doubly wrong under D5 — book ch.4/16); migration sweep for closures relying on inferred
write-through (self-host/gorget-js/arena) — loud where the new rule rejects, silent-behavior-
change where a bare closure mutates-then-reads (sweep needed, same shape as D2's); closure KIND
classification (Callable/MutCallable/ConsumeCallable) is a separate axis and may remain
body-inferred — only outer-aliasing becomes explicit. Syntax designed in RFC §2.5 (per-variable
capture list, liveness-based exclusivity) — pending owner ratification at RFC approval.

### D6. Unbound bare `Ok(e)`/`Error(e)` carrier chains = REJECT with annotation fix-it
A bare carrier chain with no inferable `E` (from destination type, function return, or
arguments) is a compile error ("annotate the Result type"). Resolves error-model §9 Q17 for
spec-v1; forward-compatible with existential/inferred-set designs later. Both compilers get the
rejection + negative fixtures (today: silent miscompile in both, TODO:153).
**Consequences:** docs write-through to `docs/plans/error-model.md` §9 Q17 (mark resolved) +
language-design §6; belongs to the bounded-rejection phase per the C12 gating rule.

## DECIDED (owner, 2026-07-05 — batch 3, at RFC review completion)

### D7. Capture-syntax package RATIFIED (completes D5)
Per-variable capture lists (`(&count)(): ...`, `(!name, &total)(x): ...`) promoting the §7.4
V2 reservation; bare names REJECTED in capture lists (disambiguates `(f)(x):`); `!():`
move-all sugar kept (accepted asymmetry); **`&`-capture exclusivity is LIVENESS-based**
(borrow ends at the closure's last use) — production's current scope-based behavior is a
filed conformance gap. Grammar work (two-group lookahead) belongs to the D5 implementation
track, not spec phase 0. RFC §2.5 is the normative text.

### D8. Float formatting = SHORTEST ROUND-TRIP, everywhere
One normative algorithm (Ryū/Grisu-class shortest representation that parses back exactly)
for BOTH `print` and `float_to_str` — healing the current `%f`-fixed-6 vs `%g` inconsistency.
Load-bearing rationale: distinct floats must always print distinctly, or stdout-diff
conformance can hide real divergences. Migration rides Phase 1's converter (expectations
regenerate via `ggdef -- gen` anyway). Docs write-through: the spec formatting appendix +
language-design/book examples showing float output.

**RFC status note:** the RFC itself is ON HOLD pending the owner's personal read
(2026-07-05); D7/D8 are decided regardless and fold into the final text.

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
- 2026-07-05 (later): batch-2 decided → D4 (drop-purity), D5 (explicit capture sigil),
  D6 (reject unbound carrier chains). Owner clarification round on D4 recorded in-message
  (drop-purity implications + honest correction re Box/Callable). RFC drafting begins.
