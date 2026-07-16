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

**Standing owner directives (added 2026-07-05, at project GO):**
- **Dogfood findings feed the ledger:** every ugly pattern, workaround, or awkward idiom found
  while writing real Gorget (self-host, gorget-js, gorget-arena, spectests) is a
  LANGUAGE-DESIGN FINDING to file against this ledger — not merely a bug or style nit. This
  generalizes "self-host as the elegance showcase" into the evidence channel that validates
  and revises D-decisions; the spec is versioned-living precisely so revisions stay cheap.
- **LLM-correctness KPI:** stand up a measured, ratcheted "LLM zero-shot correctness" benchmark
  (fixed task suite; model given only the generated semantic context pack; score
  compiles-first-try and runs-correct rates) as a first-class language-quality metric. Filed
  in TODO; depends on the phase-2 context pack.
- **Actively RETIRE sidecars + parallel structures (added 2026-07-13):** "all opportunities to
  reduce sidecars and parallel structures should be taken." This UPGRADES layering rule 3 (one
  source of truth per axis; no parallel sidecar maps) from a *don't-add* rule to a *hunt-and-fold*
  mandate — when a single logical fact is split across index-aligned parallel vectors or a
  hand-synced side-table, normalize it into one cohesive record/typed field. The `CallArg{name,
  ownership, value}` normalization (LOG 2026-07-13) is the exemplar (retires the parallel
  `Vector[String]` names-vector). A dedicated audit sweep (both compilers) is queued in TODO to
  enumerate + rank the rest. **Caveat (do NOT conflate):** a SYSTEMATIC data-oriented layout
  (arena/index-based nodes, ECS/struct-of-arrays for MEASURED cache-hot metadata, single node-id
  indexing all component arrays with enforced/typed accessors) is NOT the ad-hoc hand-synced
  sidecar this targets — it is the disciplined form. This directive kills the bug-prone kind;
  perf-motivated SoA is a separate, MEASURE-FIRST decision (see the ECS profiling track in TODO).

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

- **A30 → RATIFIED 2026-07-07 as D23 (see LOG): the throws totality invariant.** Normative sentence: "a throws call is an expression of type T in
  EVERY position; its Result-ness is unobservable except at a Result-typed binding or a
  catch." Plus the diagnostic contract: the checker never says "found Result[T,E]" —
  always "this call throws E; declare `throws E` or catch it." Enforcement: ggdef models
  the invariant; smith gains a throws-in-every-expression-position fuzz tier that
  asserts production REJECTS each unhandled throws (an inverted rejection oracle — a
  check-SUCCESS is a slip; ggdef's D23 rejection is an ElabError→GGDEF-SKIP, never
  SPEC-DIVERGE). Retroactively reclassifies the snag-#9/#10/#13
  class + the expr-body asymmetry as violations of ONE rule.
- **A31 (error composition): inferred error sets (Zig-style) as the TARGET; explicit
  From-conversion as the FALLBACK.** Scout-first: the hard core Zig doesn't have is
  PAYLOADS — Gorget errors are owned enums, so an inferred union {IoError, ParseError}
  = anonymous tagged union-of-enums + member-type match syntax + ownership/drop through
  it (phase-scale type-system feature). Also the semver story (public APIs must narrow
  to a named type — lint). Evidence pro: kills the conversion tax+soundness surface AND
  the error-type zoo; single-currency dogfood proof says optimize the common case, the
  sets serve stdlib-composition boundaries (which the D17 class sweep will create).
- **A32 (HOF effect-polymorphism — design BEFORE the surface calcifies): a HOF throws
  iff its function argument throws** (rethrows-style, but designed as real effect
  polymorphism since async composes: throws × async × faults as one effect algebra is
  the phase-3 research bet — async semantics land there anyway). Compiler evidence the
  retrofit is already hurting: the ad-hoc fault-slot closure adapters + the wild-write
  adapter fix.
- **A33 (deep-fault prep, small): spec the supervised-boundary HOOK now** — a
  T_-code-carrying fault value convertible to a catchable Error at a defined isolation
  point (Task join is the natural site) — so the phase-3 supervised boundary composes
  with ratified D11 instead of re-litigating it. Keep panic-by-default + lexical
  fault-catch exactly as-is. **⚡ OWNER RIDER (ratified 2026-07-07 with D23): faults
  enter the error/value world ONLY via explicit conversion points (lexical catch, the
  boundary, explicitly fallible APIs) — NEVER by implicit membership in signatures or
  inferred sets. Binding on any A31 union design; revisit only when catchable faults
  are implemented.**
  **⚡ OWNER UPDATE (2026-07-11 design discussion): SPEC PULLED FORWARD — scout NOW,
  ruling next batch, census-based.** The discussion (triggered by the review-residuals
  pushback on the lexical-catch ceiling + the finding-(b) pre-check idiom) settled
  three things and widened the mandate to three coupled questions (ONE design: "where
  may a fault become a value?"): (1) DEEP/dynamic fault catch REJECTED on the merits
  (breaks D1-refinement, D11 no-drops, the A33 rider's spirit; imports 4-implementation
  unwinding costs; conflates data errors [the channel's job] with bug containment
  [isolation's job] — the eventual spec prose carries the full "why not dynamic
  exceptions" argument); (2) the owner SECOND-GUESSES lexical fault-catch's existence
  — the removal hypothesis (Swift model: faults uniformly uncatchable; boundary = the
  ONLY conversion; catchable-subset concept deleted) goes to census (pre-census:
  `catch Fault` in 39 fixture files, ZERO gorget-js files); (3) `checked_add()`-style
  methods rejected on ergonomics — evaluate the FALLIBLE-OPERATOR family (`+?` `-?`
  `*?` `/?` `%?`: `T` in every position per D23, throws a prelude arith error into
  the ONE channel, auto-propagates, catchable via the existing `catch (e):` form) as
  the replacement + innovation candidate. Errors-vs-faults taxonomy pinned: errors =
  the one channel (throws/Result); faults = termination, NOT a channel; "catching" =
  explicit conversion. Candidate catchability principle if the catch SURVIVES census:
  implicit machine checks catchable, explicit programmer assertions never. Scout
  mandate: [`a33-fault-model-scout-mandate.md`](a33-fault-model-scout-mandate.md)
  (launched 2026-07-11); candidate rulings D24 (boundary spec) · D25 (fault-catch
  disposition) · D26 (fallible operators).

- **A29 (owner question, 2026-07-05): CONSOLIDATE the `&`-exclusivity rules into one
  static-semantics prose section + fixtures.** The intended rule is Rust-style (readers XOR
  one writer, language-design §3.5) and is a PREMISE of D1's refinement claim (same-call
  aliasing `f(v, &v)` rejection closed a lazy/eager divergence channel in RFC review pass 1) —
  but it is currently scattered (§3.5 prose; D7 pins capture duration only; reference-doc
  same-call rejection; open A3 local `&`-binds) and enforcement is implementation-uneven
  (production stricter-than-spec on capture duration, accepts-and-miscompiles `auto a = &b`,
  self-host has NO borrow-check pass). Sub-decisions to bring to the owner with the section:
  (a) the GENERAL duration model — liveness-based (NLL-style, matching D7's capture rule) vs
  scope-based; (b) A3's local `&`-bind disposition; (c) same-call aliasing rejection stated as
  normative. Note: exclusivity violations are NOT dynamically detectable in ggdef (aliased
  writes just sequence) — this rule lives entirely in the static layer, hence v1 prose +
  fixtures, executable at v1.5.

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
bare `Type__method`, no vtable · C10 `String.display()` = identity · **C11 ResourceExhausted
(stack depth, OOM, other host/tool resource limits)** — named *event class*, not a language
outcome and not a D11 TrapKind: production may die on the OS guard / allocator; ggdef models
only its own fuel bound (`FuelExhausted`, tool-level). Conformance **does not adjudicate**
ResourceExhausted runs (non-comparable; neither MATCH nor MISMATCH). Not "defined as SIGSEGV"
· C12 rejection-phasing meta-rule.

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

### D9. Float formatting detail: integral-valued floats print with ".0" (owner, 2026-07-06)
Under D8's shortest-round-trip rule, `print(3.0)` → `"3.0"` (never `"3"`): the printed form must
parse back AS A FLOAT in Gorget itself ("3" parses as int), preserving D8's round-trip rationale
and type visibility. Gates the D8 formatting appendix; ggdef `format_value` needs the
integral-float fix (Rust `{}` prints "3" — use `{:?}`-style or ryu-with-".0"). Recorded at the
P1-infra reviewers' recommendation.

## LOG

- 2026-07-16 (later) — **STAGING RULING (owner): wrapper deref access REJECTS until implemented.**
  `Box[T].field` / wrapper method auto-deref (§9.4 deref coercion) is UNIMPLEMENTED end-to-end
  (field read returns 0; method deref cc-fails — RV-A scout measured). The earlier staged
  acceptance (`check_gg_ok` + `#[ignore]`d run-test) is REVERSED: a fixture blessing silent
  wrong output is the Core-#8 red-flag pattern regardless of who wrote it. Until the
  deref-coercion backend track lands, wrapper deref access is an E_-reject with a
  "not yet implemented" message; that track flips acceptance + run-tests together.
  Enforcement: the RV-A track.
- 2026-07-16 — **D10(b) ADDENDUM 2 RATIFIED (owner, in-discussion): compound-assign aliasing.**
  **Rule:** the compound-assign LHS (`v[i] += rhs`, all `op=` forms, index and field-path
  places) is an implicit exclusive WRITER for the statement; the D10(b) live-alias
  place-overlap rule extends to the RHS — a `&`-borrow, `!`-move, mutating method-receiver,
  or non-Copy bare read of `root(LHS)` inside the RHS is REJECTED (`E_BorrowConflict`).
  Copy bare reads stay exempt (`v[0] += v[1]` on ints — the ratified Copy-read snapshot cut).
  **Rider:** with aliasing rejected, evaluation order is unobservable for every accepted
  program → lowering unifies on READ-FIRST (drop the resource-only `rhs_pre` reorder from
  R1/A2-R2-M1, keep its borrow-in-place ICE fix). **Why:** measured (RV-C scout 2026-07-16):
  the order was ELEMENT-TYPE-DEPENDENT (`v[0] += mutate(&v)` = 11 for `Vector[int]`,
  110 for a custom-Drop element — an implementation leak, not semantics), and
  pin-an-order-keep-accepting forces choosing between the R1 use-after-free (read-first +
  RHS reallocs) and permanently type-dependent behavior. Rejecting dissolves the dilemma;
  READ-FIRST matches the 3 already-uniform lanes (self-host, ggdef, production-nonresource).
  **Census:** blast radius ZERO across 2,179 in-repo .gg files (the only aliasing sites are
  R1's own UAF counterfactual probes → they become the negative fixtures). **Honest cost:**
  non-Copy `v[i] += v[j]` (same root, statically indistinguishable indices) rejects with a
  one-token `.clone()` remediation — mirrors Rust; zero current sites. **Rejected
  alternatives:** pin-order-keep-accepting (the UAF-vs-confusion dilemma); reject-only-realloc
  (not statically decidable); reject+keep-reorder (diverges from 3 lanes, churns ~20
  snapshots). Full matrix + fix-shape per lane: `scouts/scout-rvc-compound-assign.md`.
  Enforcement = the RV-C track (all three lanes + the ggdef compound-index double-eval fix
  riding its ggdef leg; queues behind RV-D/RV-F).
- 2026-07-15 — **C11 AMENDED (owner-agreed honesty fix) + NAMING REFINED same day.**
  Prior C11 ("unbounded recursion → OS-guard SIGSEGV accepted by design") half-implied
  stack death was a *defined language outcome*. **"Out of conformance" is the wrong primary
  name** — that is a *suite process* relation, not what happened to the program. Prefer the
  three-layer framing:
  1. **Language outcomes (closed):** `Value` | `Trap(kind)` | `IllFormed`.
  2. **Tool outcome (ggdef only):** `FuelExhausted` — the definitional interpreter's fuel
     bound; not host stack/OOM; not a D11 TrapKind production must emit.
  3. **Event class `ResourceExhausted`:** host/tool resource limits hit (stack guard, OOM,
     …). **Not** a language outcome and **not** "defined as SIGSEGV." Production may die on
     the OS/allocator; ggdef must not pretend to model that. Conformance **does not
     adjudicate** these runs (non-comparable — neither MATCH nor MISMATCH). Elevating
     ResourceExhausted to a peer of Trap (normalized `trap[T_ResourceExhausted]`, portable
     stack/OOM detection) is a **future decision**, not smuggled by renaming.
  Docs write-through: C11 line + D11 residual + RFC §2.2. No change to production's practical
  deep-recursion behavior — only to naming and what we claim the language defines.

- 2026-07-15 — **D10 local `&`-binds: owner open to reopening** provided they introduce no
  CoW soundness holes. Stance recorded (not a formal D-number yet): exclusive local binds
  are compatible with "one exclusive writer" when the bind *is* that writer for its live
  range (overlapping root uses rejected, liveness-based duration). Reopen only after D10
  place-overlap enforcement is solid; ban remains the safe default until then.

- 2026-07-15 — **D15 CLARIFICATION (owner question: "I want free views — why are we
  losing that?"): we are not.** D15 removes the *user-visible borrowed-slice surface type*
  (`T[]` / fat-pointer-as-type — the lifetime-annotation trap). It does **not** require
  eager deep-clone of every `v[a:b]`. Per D1 and the batch-4 proposal: an owned-value
  sub-sequence and a lazy CoW sub-range share are observationally identical; production
  SHOULD keep free views (String already does via `cap==0`; Vector slices may grow the same
  invisible opt). Spec meaning = value-as-of-slice-point; free view = refinement.

- 2026-07-13 — **`str.data` FOSSIL RETIRED (owner-decided; dogfood finding via the
  FieldAccess soundness fix).** `str.data` (reading a `String`/`GorgetString`'s internal
  `.data`) compiled+ran in BOTH compilers as a NO-OP — an undocumented
  internal-representation leak special-cased to `return obj`/`return base` (Rust
  `ir/lowering/exprs/mod.rs:2261-2263` + self-host `lower_expr.gg:4660-4661`), i.e.
  `print(str.data)` ≡ `print(str)`. The FieldAccess soundness fix (rejects a bogus
  field on a fieldless receiver) correctly rejects it; the full C sweep flushed out 2
  fixtures relying on the fossil. **Owner ruled RETIRE** (Core #8 — the agreed-on
  behavior in both compilers was itself wrong): reject `str.data`, fix the 5 no-op call
  sites (`print(x.data)`→`print(x)`, byte-identical), delete the dead special-case in
  both compilers. A `String` is opaque — no user-visible fields. LESSON pinned: SLICE
  validation ≠ the over-rejection gate; the FULL sweep (Core #7) is the gate, and it
  caught what the scout's slice + 3 empirical brief-passes missed.

- 2026-07-13 — **SELF-HOST ARG MODEL: converge on Rust's typed `CallArg{name,
  ownership, value}` record (owner RATIFIED, FIRM — "make the sigil fix the CallArg
  normalization itself").** The self-host call/method-argument representation moves
  from `Vector[SpannedExpr] args` + a PARALLEL `Vector[String]` names sidecar (with
  ownership encoded — or DROPPED — by wrapping the arg in `EMove`/`EMutableBorrow`) to
  a single `Vector[CallArg]` where `CallArg{ name: Option[String], ownership, value }`
  carries all per-arg passing metadata as TYPED FIELDS, mirroring Rust gg
  (`Vec<Spanned<CallArg>>`, `src/parser/expr.rs:1992`). **Rationale (three-fold, and
  the correctness leg is decisive):** (1) REFERENCE-GRADE — per-arg passing metadata
  (name, ownership) are facts of the same category and belong as typed fields, not
  split between a parallel-names-vector and an expression-shape wrapper; reading
  ownership becomes `arg.ownership`, not `match arg.value: case EMove` (layering rule
  2 typed-not-shape + rule 3 one-source-of-truth); it retires the parallel-names
  sidecar (itself a rule-3 smell) in the same move. (2) SAFE — the arg VALUE stays a
  BARE expression, so the self-host LOWERER sees exactly what it saw before → the
  ownership refactor structurally CANNOT reproduce the miscompile class that reverted
  the wrapper approach (see the dogfood finding below). (3) It is the natural shape
  for B2's D10(b) place-overlap mirror (iterate args, read `arg.ownership`). **Scope
  of the "expression-context `!x`" stays put:** `R b = !a` is genuinely a
  value-producing move-EXPRESSION and remains `EMove`; ONLY call/method args get the
  `CallArg` record. **B2 must HONOR this** (read `arg.ownership`, never shape-match) —
  owner directive. **DOGFOOD FINDING that forced "now" (per the standing dogfood
  directive):** the minimal wrapper fix (shape (a): wrap `!x`→EMove / `&x`→
  EMutableBorrow at call args) SILENTLY MISCOMPILED `&`/`!`-arg programs on the
  self-host (`mutarg_probe` → garbage, `static_ref_param` → `0` not `42`) — wrapping
  a `&`-arg in `EMutableBorrow` made the LOWERER's `EMutableBorrow` arm fire where it
  was dead for call args, changing lowering. It was reverted (`a2f6df25`) because the
  scout's + executor's gates (lowerer_comparison emitted-C diff + bootstrap +
  box_deref ASan) did NOT include `self_host_runtime` (the runtime-OUTPUT snapshot
  suite that catches compiles-fine-runs-wrong). **Enforcement lesson pinned:
  `self_host_runtime` + the full runtime-output suite are MANDATORY gates for any
  self-host lowering-adjacent change.** The correctness failure of the wrapper is
  exactly why the typed `CallArg` (bare value) is not merely more elegant but the only
  safe model — so the convergence lands NOW, as the fix.

- 2026-07-12 — **D10(b) ADDENDUM (owner RATIFIED): the place-overlap rule ranges over
  LIVE ALIASES, not syntactic reads — Copy reads are value snapshots and participate
  in no overlap.** Raised by the Batch-B scout: when one call arg is `&whole` (a
  writer) and another is a bare READ of an overlapping sub-place, does the read
  conflict? Ruled: **state the rule ONCE — the place-overlap check ranges over live
  aliases (`&` writers, `!`/`^` movers, AND non-Copy bare reads); a Copy-typed bare
  read is a value snapshot that participates in no overlap.** The principled reading,
  NOT a carve-out: D10 exists to close the lazy/eager CoW-divergence channel, whose
  hazard is a *live alias* that can observe or miss a mutation through the writer. A
  bare read of a Copy scalar at a call position is evaluated at the call site into an
  INDEPENDENT value before the callee runs — no memory edge remains between it and
  the writer's region, so there is nothing to diverge against; rejecting
  `f(&whole, whole.int_field)` would reject a program with zero observable hazard,
  spelling the rule over syntax ("read overlapping a writer") instead of semantics
  ("live alias overlapping a writer"). A non-Copy bare read IS a borrow under
  CoW-default — a live pointer into the writer's region for the whole call — which is
  exactly the divergence channel, so it is REJECTED. The cut falls precisely on
  ALIAS vs VALUE, the axis D10 was ratified to police. **Reference sanity-check
  (independent):** this is Rust's two-phase-borrow behavior — `f(&mut s, s.int_field)`
  compiles (Copy read snapshots before the `&mut` activates); `f(&mut s, &s.vec)` is
  rejected (live shared borrow overlapping a `&mut`). Our principled derivation and
  the reference land on the same cut independently.
  **Rider 1 (movers, uniform — owner):** extend the same cut to movers. `f(!x, x.copy_field)`
  is LEGAL (snapshot pre-call; the move transfers after, per the ownership table); a
  non-Copy bare read overlapping a mover is a live alias into a moved-away value —
  REJECT. The addendum states writer AND mover cases together; do not leave the mover
  case to be re-derived at the next fork.
  **⚠ RIDER 1 REVISED 2026-07-14 (owner, firm — the B1 pass-2 review + measurement corrected
  the mover HALF):** `f(!x, x.copy_field)` is **NOT legal — it is correctly REJECTED**, but by
  the LIVENESS rule (`E_UseAfterMove`), NOT by place-overlap. The original "snapshot pre-call"
  premise was derived from runtime mechanics without probing the static model — the
  un-measured-premise mistake the gauntlet exists to catch. Measured on gg @ HEAD: `f(!s, s.n)`
  → **E_UseAfterMove** ("value moved here"); `f(&s, s.n)` (writer+Copy) → **accepted**;
  `f(s.n, !s)` (read-BEFORE-move, left-to-right eval) → **accepted + runs correctly**. The
  dissolution: the mover-Copy case was never the aliasing rule's to decide —
  **`&` borrows** (a deferred-activation/two-phase story is coherent, and a Copy read genuinely
  has nothing to alias) **while `!`/`^` CONSUMES** — after `!x` the slot is logically dead, so
  reading `x.copy_field` is a LIVENESS violation the move-tracker rejects one layer BEFORE
  place-overlap is consulted. Two one-sentence rules on different axes: **(1) D10(b)
  place-overlap governs LIVE ALIASES** — Copy reads participate in no overlap, uniformly for
  writers AND movers (this part of the rider STANDS); **(2) E_UseAfterMove governs LIVENESS** —
  a Copy read of a moved source is dead-slot access regardless of overlap. So the mover clause
  now reads: *the place-overlap rule neither needs nor grants a mover exemption for Copy reads;
  `f(!x, x.copy_field)` is rejected upstream by the move-tracker (E_UseAfterMove), and that
  rejection is correct — a move consumes the storage; there is no two-phase machinery for moves
  in this language or in the reference (Rust rejects the identical program).* Option-2
  (make it legal) loses on the MERITS, not just cost: it needs the backend to provably read
  `x.copy_field` before the transfer/zeroing (else a silent miscompile — the worst class), it
  makes "after `!x`, `x` is dead" order-and-Copy-conditional (fuzzy boundary: `f(!x,x.a,x.b)`?
  `f(!x,g(x.copy_field))`?), and the idiomatic rewrite is one clearer line (`auto n = x.copy_field`
  then `f(!x, n)`, or reorder to `f(x.copy_field, !x)`). **B1 fixtures pin this layering:** the
  `f(!x,x.copy_field)` NEG asserts the diagnostic is **E_UseAfterMove** (NOT an aliasing error —
  if a refactor makes it fail with the overlap error, the move-tracker silently lost a case; the
  fixture catches that drift); the order-twin `f(x.copy_field, !x)` is a **POS** (pins the rule
  as evaluation-order-sensitive, not a blanket "no reads of `x` in a call that moves `x`").
  **⚠ GGDEF VERDICT = ELABORATE ∘ EVAL — ELABORATE OWNS ALL RATIFIED STATIC REJECTIONS, RATIFIED 2026-07-15 (owner, firm; corrects a boundary-note flaw):**
  A ggdef program's verdict is `elaborate ∘ eval`: **ggdef-elaborate models EVERY ratified static rejection
  within its subset — INCLUDING flow-sensitive may-move liveness — and ggdef-eval owns pure per-path dynamic
  semantics.** THE CORRECTION: an earlier boundary note (this session) assigned the flow-sensitive static axis
  to "prose + spectests (enumerated escape-hatch list)". That preserved "implementations don't own semantics"
  but left the axis with **NO EXECUTABLE ARBITER** — Core #6 INVERTED (prose rots, guards don't). Proof: the
  self-host shipped for months with NO liveness pass and nothing caught it, precisely because the definition
  didn't model the static liveness judgment, so the conformance lane had no negative that could MISMATCH. The
  "fuel-bounded / can't explore all paths" argument proves only that EVAL can't own the rule — it does NOT
  follow that ggdef can't, because ggdef has never been just eval: **elaborate already makes static judgments
  (D10(a) binds, D12 six positions, D10 place-overlap, throws totality).** The may-move merge rule is NOT
  all-paths execution — it is textbook flow-sensitive static dataflow (one syntax-directed walk: moved-set,
  kill on move, revive on whole-local reassign, union at joins = "moved in ANY arm ⇒ moved after", filter
  diverging arms, moved-in-loop-body ⇒ MoveInLoop, emit `E_UseAfterMove`/`E_DoubleMove` as IllFormed BEFORE
  eval) — deterministic, terminating, NO fuel, NO path enumeration (abstracts branches by union, never executes
  them). It mirrors production `origins.rs` + the self-host's `check_safety_*` walk (~few hundred lines, in
  `spec/ggdef/src/elaborate/`, Rust-side, NOT bootstrap-gated). The conditional-move-then-use program then gets
  `E_UseAfterMove` from elaborate (matching production + self-host) and never reaches eval — exactly as it never
  reaches a binary in production; eval's "Value on the c=false path" verdict is unreachable-but-still-true.
  **CONSEQUENCES:** (1) the proposed `static-only:` per-lane-split frontmatter tag DIES — with elaborate
  rejecting the same programs, no lane needs a by-design MISMATCH (it was a wart). (2) The transition-table
  test suite is the SHARED spec for BOTH layers — same cells; the branch-merge column differs BY DESIGN (eval
  asserts the per-path verdict, elaborate asserts the union verdict) — that contrast, pinned in tests, IS the
  dynamic/static distinction documented executably. (3) The smith soundness guard survives (check-accepted
  programs must run dynamically clean under eval) and now ALSO guards the soundness relation between ggdef's
  OWN two layers. (4) The boundary doc note is REWRITTEN (not softened): `verdict = elaborate ∘ eval`; the
  old enumerated escape-hatch list → EMPTY except honest ggdef *subset* gaps (generics, it-lambdas — subset
  limits, NOT ownership carve-outs). (5) The conditional-move + consume-double fixtures become ORDINARY
  cross-lane conformance fixtures (elaborate rejects them → all lanes agree). **GUARD-RAIL (owner, so the
  pendulum doesn't overswing):** elaborate models the RATIFIED CONSERVATIVE rule (reference `:2390`) — it must
  NOT become a place where whatever precision production's analysis happens to have gets silently canonized.
  If production rejects something elaborate accepts (or vice versa) on a liveness shape, that is a finding to
  adjudicate against the PROSE rule, same as any divergence. **The definition LEADS; it does not trail.**
  SEQUENCING: the eval transition-table fix (revive + consume-call-kill, proven) + the elaborate may-move pass
  land as ONE merged change (owner ok'd merge 2026-07-15 once both prototypes proved out; combined patch
  `scouts/patches/ggdef-elaborate-move-proto.patch`, ggdef 127/0, conformance 195/195, 100% production
  agreement / 25 probes). **THE VERDICT TRIPLE for a static rejection (owner, 2026-07-15): channels = layers.
  stdout is EVAL's channel (what the program printed); stderr is ELABORATE's/the-judge's channel (why rejected;
  and at runtime, the trap).** A statically-rejected program never ran → **stdout = "" is semantically correct
  and stays EXACTLY empty (= "the program never executed", which IS the verdict); stderr = `error[E_Code]: …
  at span`; exit = the check-failure code.** Pins: (1) FORMAT mirrors the RATIFIED diagnostic shape (production
  `gg check` family + ggdef's location-suffixed trap render), NOT a ggdef-private terse form — ggdef is the
  definition, its stderr is what a human reads adjudicating a lane diff, so it is the BEST-rendered of the four
  lanes. (2) EXIT distinguishes "never ran" (static rejection = **exit 1**, the compile-error code — matches
  production's check/build-failure exit AND the rustc/clang/gcc/tsc consensus) from "ran and died" (trap = 101)
  — they MUST be distinct so a runtime crash can't masquerade as a correct reject. (3) CONFORMANCE compares the
  `E_` code + exit CLASS only; prose detail + span quality stay impl-defined (D11 trap precedent) — `ggdef -- gen`
  records the CODE for rejection fixtures, not the message; span comparison is not a conformance axis until
  deliberately ratified.
  **⚠ TOOLCHAIN EXIT-CODE SCHEME (Option A), RATIFIED 2026-07-15 (owner, firm; research-backed):** the whole
  `gg`/`ggdef` toolchain uses **`0` success · `1` static rejection (parse OR semantic OR may-move IllFormed —
  ONE class) · `2` usage/CLI · `101` runtime trap + ICE · `103` fuel (ggdef-ONLY, outside the compared set).**
  Rationale: the NUMBERS are pure consensus, deliberately un-novel (0 universal; compile=1 per
  rustc/clang/gcc/tsc/swiftc/javac — Go's 2 is the lone outlier; usage=2 per GNU/argparse; 101 = Rust's
  panic/ICE, and rustc's exact compile=1 / panic=ICE=101 split is the DIRECT precedent for the one distinction
  Gorget structurally needs; `sysexits.h` 64/65/70 is followed by NO mainstream compiler — rejected). Where
  Gorget is the REFERENCE is not a novel number but making the taxonomy a first-class EXECUTABLY-ENFORCED
  contract (ggdef is the definition; conformance compares `E_` code + exit CLASS across four lanes) — every
  other compiler has 1-vs-101 as de-facto behavior nobody wrote down. **RECONCILE: production `gg` ALREADY
  conforms** (compile=1, trap=101 via `gorget_trap_at`, ICE=101 for free); the ONLY code change is `ggdef` (~6
  edits, 2 files): `EXIT_ILLFORMED 102→1` (`eval.rs:48`); route `FrontendError::{Parse,Elaborate}` 2→1 (a source
  error is a static rejection, NOT usage — `main.rs:97-100`); keep `USAGE=2` (`main.rs:21`); keep `EXIT_FUEL=103`
  re-doc'd as ggdef-only (`eval.rs:50`); rewrite the header taxonomy comment (`main.rs:11-14`). THE CLINCHER: the
  ggdef conformance harness raw-compares the exit integer (`spec_conformance_ggdef.rs:102`) and production emits
  1, so ggdef MUST emit 1 (this OVERRULES the brief-review-1 tentative "keep 102"). **ICE folds into 101 —
  Option C (distinct ICE code) DEFERRED** (owner 2026-07-15; rustc doesn't bother, marginal value vs custom
  panic hooks). Two LOW production follow-ups filed (TODO): usage errors collapse into 1 (should be 2); internal
  runtime panics (OOM/closed-channel, `panic_normal.c:5`) exit 1, colliding with the compile-error code — route
  to a distinct code or make them traps. Write-through: this scheme + a consolidated exit-code table into
  `docs/language-reference.md`, cross-ref static=1/fuel=103 in `spec/prose/trap-codes.md`, and note both
  `static-error`/`parse-error` tiers → exit 1 in `rfc-ggc-ggdef.md`.
  **⚠ B2 SCOPE + LIVENESS-PASS + PASS-ORDER, RATIFIED 2026-07-14 (owner, firm — two calls, one layering principle):**
  Raised by the B2 self-host-mirror scout (the self-host has NO move-tracker → accepts every use-after-move).
  **(1) B2 mirrors the FULL D10 RULE, not production's exact code — the mover-mover arm is IN.** `f(!x, !x)`
  is the MAXIMAL place-overlap ("at most one writer OR mover" — D10's ratified text). Production's
  `check_call_aliasing` EXCLUDES `(Move,Move)` only because `E_DoubleMove` preempts it one pass earlier; the
  self-host has no such upstream pass, so the faithful mirror lets the arm become REACHABLE and FIRE —
  rejecting `f(!x,!x)` with the OVERLAP code. That is not a fudge: reject-with-a-different-code strictly beats
  ACCEPT (silently admitting a broken program to preserve diagnostic cosmetics is backwards). Interim
  divergence (self-host overlap-code vs production/ggdef `E_DoubleMove`) DOCUMENTED at the arm + cited to the
  filed liveness entry; the mover-mover fixture stays OUT of the self-host conformance lane (or is a
  self-host-targeted rejection test) until the liveness pass lands and preempts it. **BUT `f(!x, x.copy_field)`
  stays EXEMPT — B2 must NOT catch it** (per Rider 1 REVISED: the Copy read is a SNAPSHOT, not an alias; bending
  the overlap rule to mop it up is the phantom-alias mistake re-smuggled through the self-host). The self-host
  ACCEPTS that program until the liveness track lands — an honest, filed, pre-existing divergence.
  **(2) The self-host LIVENESS PASS is its OWN track, filed HIGH.** The self-host missing the entire liveness
  axis is a first-order Core-#8 gap (`E_DoubleMove`/`E_UseAfterMove` are ratified registry diagnostics, the
  definition models them, a conformance lane that can't emit them isn't conformant — same class as the (a2)
  D23 gate). Sequence it AFTER A2-S (landed) — the port is then "add an axis to an existing walk." **STRUCTURAL
  (the innovation): do NOT bolt on a third standalone pass — mirror production's `src/semantic/safety/` MODULE
  LAYOUT: ONE self-host safety walk where drop-purity (A2-S) + place-overlap (B2) + liveness are ARMS reading
  typed metadata** (three independently-grown `.gg` passes = sibling-drift; the self-host is the elegance
  showcase). **Acceptance set = ggdef (the executable DEFINITION), fixture-for-fixture** (ggdef already models
  read-of-moved→IllFormed `eval.rs:21/745`), NOT eyeballing the Rust tracker. **Scope minimal:** Gorget's move
  rules are `!`-driven + last-use-based (not full NLL), so a per-function forward walk (moved-set, kill on move,
  error on use-of-killed, union at merges) covers it; scout calibrates against production's move-tracker size
  (~250-400 lines est., comparable to A2-S). Parallelize per-module if the blast radius reveals many real
  self-host-source use-after-moves.
  **(3) PASS-ORDER RIDER (ratified): LIVENESS diagnoses PRECEDE ALIASING diagnoses** (production's + the
  definition's pass order). This makes the self-host's interim overlap-code rejection of `f(!x,!x)`
  "known-nonconformant-but-safe, closes when liveness lands" rather than an ambiguous mismatch — and it protects
  the D11 exact-code fixture expectations from ever being argued backwards from an implementation's pass order.
  **Rider 2 (implementation — owner):** the check MUST read the TYPED Copy axis
  (A2-R1 just built the Copy∧Drop machinery — read the same accessor, rule 2, NO
  shape heuristics); ggdef models the IDENTICAL rule in the same track, with fixtures
  pinning BOTH directions per position (legal Copy-read positive + non-Copy-read
  rejection negative); the track gates on the FULL ggdef suite (Batch A's lesson:
  this track flips/adds expectations).
  **Provenance-bit fork resolved (B), owner-confirmed ("not even a close call"):**
  call args already carry typed `CallArg.ownership` at the exact site D10(b) consumes
  (Batch-B scout, `src/parser/expr.rs`), so D10(b) reads typed metadata — no
  shape-walk, no sibling-drift. Building the typed borrow-provenance bit now would be
  a ~69-site moderate-HIGH-risk perturbation of type-equality/coercion (+ a self-host
  twin) with ZERO D10(b) consumer — tripping the standing "build only with a real
  consumer" principle. The bit's real motivation was A3's expression-shaped
  tail-walking fragility (the value-position family, TODO no-op-`&`); the
  value-position track's scout re-evaluates it there with a real consumer in hand and
  builds it only if it wins. Deferring is the reference-grade choice, not the lazy
  one.

- 2026-07-11 — **D10(a) ADDENDUM (owner, considered-and-DECLINED): local MOVE-binds
  stay LEGAL — the rejection criterion is ALIASING, not sigils-at-binds.** Raised
  after A3 landed the `&`-bind rejection: should `R b = !a` (post-D27: `^a`) be
  rejected symmetrically? Ruled NO. The two constructs do OPPOSITE things to the
  source: a `&`-bind creates a SECOND live writable path to a place (the
  exclusivity violation — the lazy/eager divergence channel D10 exists to close),
  while a move-bind KILLS the source — one live name before, one after; no
  aliasing ever exists, so there is nothing for exclusivity or the D1 refinement
  obligation to trip over. Rejecting move-binds would also contradict the
  ratified ledger four ways: (1) `!a`-at-bind is D4/D12's PRESCRIBED remedy for
  drop-tainted bare-assigns (the A2-R1 enforcement's primary fix-it); (2) ggdef's
  normative D4 suite uses `R b = !a` as its LEGAL counterparts — the executable
  definition blesses the spelling; (3) the single-owner family (Owned/Box/Task/
  Guard) REQUIRES move-binds — `E_MoveWithoutOperator` exists to force them;
  (4) the CoW contract's three move-eligible shapes treat bind- and call-position
  moves identically. The genuinely dodgy neighbor — the PROJECTION move
  `R b = !h.r` (a partial move) — is ALREADY rejected by the existing machinery
  (`E_UseAfterMove`; `.clone()` is the remedy; measured by the A2-R gauntlet),
  drawing the boundary where it belongs: whole-identifier moves at binds legal,
  field/index-place moves rejected. Rust-style destructuring partial moves remain
  a possible future WIDENING, undecided. (A pure-rename style lint — `R b = !a`
  with `a` otherwise unused — was noted and deliberately NOT filed.)

- 2026-07-11 — **DECISION BATCH 5 CLOSES: D24 + D25 + D26 RATIFIED by owner (census
  packet review, `scouts/scout-wave-census.md`) — with D27 + D28 (below), the full
  fault-model + operator-surface redesign is ruled.**
  - **D24 — THE SUPERVISED BOUNDARY, ADOPTED, ALL 8 CODES (spec-only now; impl =
    phase 3):** Task join is the ONLY fault→value conversion point in the language.
    A supervised task's death by ANY trap class (incl. AssertFailed/Panic/Unwrap*)
    becomes a `TaskFault` error value — closed TrapCode mirroring the registry —
    at an ordinary `throws` join, flowing through the existing `catch (e):`/D23
    machinery. Whole-unit discard: no drops on the trap path (D11), no
    continuation into partial state; `gg test`'s cleanup-stack boundary is the
    in-repo mechanism precedent. Rationale for all-8: a boundary is BUG
    containment, and asserts/unwraps are the most common bug class — a boundary
    that lets `T_AssertFailed` kill the process is one nobody can rely on.
    Panic-by-default unchanged for unsupervised code. The A33 rider TIGHTENS to
    its final form: faults enter the error/value world ONLY via the supervised
    boundary + explicitly fallible APIs (the lexical-catch conversion point is
    deleted by D25). Docs write-through: §10.9 + book point at the sanctioned
    shape; the "why not dynamic exceptions" prose (A33 scout appendix) ships
    with it. A33 CLOSED.
  - **D25 — LEXICAL/DEEP FAULT-CATCH REMOVED (Swift model), gated on D26 landing
    with-or-before:** faults become uniformly uncatchable in-process; the
    catchable-subset concept, the `Fault` prelude enum, `is_catchable()`, and the
    registry's catchability column are DELETED. Census (twice-verified): ZERO
    organic uses in any corpus — all 31 fixture files are feature-tests;
    §10.5 was STALE (shipped behavior was half-deep via fault-slot params — the
    worst of both). Removal retires ~2,000 lines of both-compiler machinery
    (fault_participation.rs, fill_fault_return_block + re-panic sites, NULL-slot
    closure adapters) and CANCELS 8 filed tracks. ggdef models no catch — the
    definition is already complete under removal. ~10 fixtures migrate to D26
    positives, 2-3 become rejection negatives, rest delete.
  - **D26 — FALLIBLE OPERATORS ADOPTED:** `+! -! *! /! %!` + `<<! >>!` + `**!`
    (D28), throwing the payload-free prelude `enum ArithError { Overflow,
    DivByZero }` into the ONE error channel; D23-total (an `int` in every
    position; auto-propagates; existing `catch (e):`; `Result` capture);
    precedence = base operators; integer-only v1 (floats rejected + fix-it);
    `INT_MIN / -1` and shift-range → `Overflow` (mirrors the registry); compound
    `+!=` excluded v1; catch-in-const rejected v1. Glyphs pinned by D27. Prior
    art: Pony's partial arithmetic (operators), Zig's std.math error unions
    (semantics); the typed+auto-propagating operator combination is novel.
  - **THE WAVE PLAN RATIFIED (out-of-repo DEFERRED):** Batch A (D19 + D12
    straight-to-error + D10(a); near-zero blast — census: D12 ≤13 sites all in
    the drop-fixture family, D19 zero + the self-host `format.gg:471` arm,
    D10(a) the 2 known fixtures) → Batch B (D10(b) place-overlap: the IN-REPO
    hand-hoists — self-host 8, lib 3 incl. the `p2p.gg:2057` double-writer,
    fixtures' 2 existing negatives) → Batch C: C1 operators (D26+D28 combined
    round; gates C2) → C2 fault-catch removal → C3 the composed `gg fmt` sweep
    (D27 sigils + D22 `.slice()` + D28 `pow()` — composition test PASSED, one
    pass per corpus) over the IN-REPO corpora (fixtures/spectests/self-host/lib).
    **gorget-js (24 D10(b) sites + ~34 sigils), arena, gglox, gorget-conformance
    migrate in a LATER coordination round with those projects** (owner ruling).
    D28's xor-as-pow lint pinned to the GCC-12 2/10-literal-base shape (the
    broad literal^literal form false-fires on the canonical XOR fixtures).

- 2026-07-11 — **D28 RATIFIED by owner (in-discussion): THE POWER OPERATOR, full
  package.** `**` with Python precedence/associativity (right-assoc; tighter than
  unary minus: `-2**2 = -4`); `int ** int` → `int`, CHECKED → trap `T_Overflow`
  on overflow AND on negative exponents (the registry's result-not-representable
  class — the `INT_MIN / -1` precedent; no new trap class); `float ** float|int`
  → IEEE pow, never faults (D8/D18 float philosophy); `**!` joins the D26
  fallible family; `**=` compound (uniform with `+=`/`*=`); wrapping `**%`
  DEFERRED (pure widening). `lib/std/math.gg pow()` RETIRES after migration
  (D20 one-canonical-way pattern). Plus the xor-used-as-pow teaching lint
  (GCC-12 precedent): `W_` diagnostic on `literal ^ literal` shapes (`2 ^ 8` is
  XOR = 10 — "did you mean `2 ** 8`?") — permanently defuses the math-prior
  misread of infix `^`, and settles `^` cleanly as XOR-infix/move-prefix
  alongside D27. Origin: owner question during the D27 discussion revealed
  Gorget had NO power operator and NO integer power at all (float-only free-fn
  `pow`). Own implementation track, sequenced with the D26/D27 work.

- 2026-07-11 — **D27 RATIFIED by owner (in-discussion, after the A33+fault-model
  scout's Q4 census): THE SIGIL ECONOMY.** `!` = the error channel (the D26
  fallible-operator family is glyph-pinned `+! -! *! /! %!`); `?` = optionals
  (ratifying the already-shipped `?.`/`??` convention; bare postfix `?` stays
  dormant/free for future Option sugar, e.g. `v[i]?`); **`^` = the MOVE sigil,
  replacing `!`** (prefix-only; infix `^` stays XOR — the same prefix/infix
  disambiguation `&` already uses for borrow-vs-bitand). Road-not-taken record:
  `|` was the owner's first instinct (shell-pipe = "value flows to the next
  consumer") — rejected because flow describes ALL argument passing while move
  uniquely means THE SOURCE DIES; the direction breaks at assign/param positions;
  `|` is Gorget's busiest glyph (bitor, `|=`, live or-patterns incl. a leading-`|`
  parse arm, `pattern.rs:32-34`); and the flow prior is better saved for future
  `|>`-style dataflow. `move` keyword rejected (owner prefers a sigil; it remains
  the strongest-prior fallback — already reserved, already parses in closure
  position). `take` rejected (live `.take(n)` method, 135+ hits). `~` disqualified
  (prefix bitnot). `&&` runner-up (C++ rvalue prior, two chars). Scout census:
  ~870 move-sigil sites across four corpora (⚠ CORRECTED by the wave-census scout
  2026-07-11: **~1,114** — the A33 figure omitted `lib/` [224, a missing FIFTH
  corpus], spectests [70], and the gglox/gorget-conformance projects [66]; see
  `scouts/scout-wave-census.md`); D7 capture lists have ZERO corpus
  uses → the capture re-spelling (`(^name, &total)(x):`, `^():` move-all) is a
  pure spec rider on D7. Implementation = its own bootstrap-gated track (lexer/
  parser/formatter both compilers + `E_MoveWithoutOperator` and the `expr.rs:593`
  move-hint diagnostics + docs sweep; `gg fmt` is the auto-migration vehicle).
  D24 (boundary) / D25 (fault-catch removal) / D26 (fallible operators) remain
  RECOMMENDED-pending-formal-ratification from the scout report
  (`/tmp/scout_a33_report.md`, mandate `a33-fault-model-scout-mandate.md`).

- 2026-07-07 — **D23 RATIFIED by owner (was queue item A30): THE THROWS TOTALITY
  INVARIANT.** Normative: "a throws call is an expression of type T in EVERY position;
  its Result-ness is unobservable except at a Result-typed binding or a catch." Plus the
  diagnostic contract: the checker never surfaces the desugar ("found Result[T,E]" is
  banned from user-facing diagnostics) — violations of the virality rule say "this call
  throws E; declare `throws E` or handle it (catch/rethrow/Result capture)."
  NO semantic change — the virality is pre-existing; this pins coverage totality + UX.
  Enforcement rides the trap-normalization wave: ggdef models the invariant; smith gains
  a throws-in-every-expression-position fuzz tier that asserts production REJECTS each
  unhandled throws (an inverted rejection oracle — a check-SUCCESS is a slip; ggdef's D23
  rejection is an ElabError→GGDEF-SKIP, never SPEC-DIVERGE); a diagnostics
  ratchet asserts no unhandled-throws message contains "Result["; reference §10.1 gains
  the sentence. Retroactively owns the seam-bug class (gorget-js snags #9/#10/#13, the
  expr-body asymmetry).
- 2026-07-07 — **A33 RIDER PINNED by owner (ratified alongside D23): faults may enter
  the error/value world ONLY via explicit conversion points** — lexical fault-catch, the
  future supervised boundary, or explicitly fallible APIs (try_-style contracts) —
  **never by implicit membership in signatures or inferred error sets.** Preserves
  §10.9's "arithmetic doesn't infect types" under any future A31 union design while
  keeping fault handling (incl. OOM) permanently reachable by choice. Owner: revisit
  when/if catchable faults are implemented.

- 2026-07-07 — **gorget-js DOGFOOD FINDINGS on the error model fed to the ledger** (owner
  relayed the project agent's field report; per the standing dogfood directive). Evidence
  of record: (a) the type-directed-propagation holes all sat at positions where the
  expected type wasn't threaded inward (binary operands, match-arm tails, scrutinees) and
  the no-marker failure mode LEAKS THE DESUGAR ("expected T, found Result[T,E]"); (b)
  gorget-js PRE-CHECKS BOUNDS to dodge faults it cannot catch across calls — a
  workaround-idiom proving lexical-only fault recovery is an insufficient ceiling for
  runtime/server authors; (c) single-error-currency scaled beautifully (one error type,
  zero conversion boilerplate) — the common case is healthy; the multi-type story is the
  gap; (d) cross-type propagation via conversion was both a tax and a soundness surface
  (the fixed bit-cast miscompile). THREE NEW OPEN-QUEUE CANDIDATES filed below (A30-A32);
  the earlier From-conversion idea is DEMOTED to explicit fallback inside A31.

- 2026-07-07 — **D14 why-a-view ADDENDUM recorded** (owner second-guessed the ruling; the
  re-derivation that settled it is now saved in decision-batch-4-proposal.md §D14: the
  collection owns → copy/move/view are the only possible read-returns; the owned copy is
  the option that silently BREAKS `.push()` chaining (throwaway mutation + the measured
  round-8 double-drop); the view is what ENABLES receiver-position write-through; the
  sole rejected chain (`get_or(...).push`) guards miss-path aliasing, not view-ness.
  The addendum is flagged MUST-REACH-THE-BOOK in the D14 write-through.

- 2026-07-06 — **D22 RATIFIED by owner: colon-slice `v[a:b]` is canonical; `.slice()`
  removed after migration.** Four open forms v1 (`a:b`, `a:`, `:b`, `:`); bounds CLAMP
  (Python-style, not a fault); strings slice by CODEPOINT; desugars to D15 owned-value
  semantics. Negatives + step DEFERRED — design reasons, not difficulty: negatives
  collide with §10.9's negative-index-is-a-Bounds-fault (a ratified safety net against
  underflow bugs) and deserve their own decision with usage data; step's real use is
  `.reversed()`'s job and a stride breaks the future offset+len CoW-share repr. Both
  pure widenings later. D15's removal track becomes the COMBINED slice-surface track.

- 2026-07-06 — **D11 RATIFIED IN FULL (registry shape approved; owner clarified the
  governing rule: CLAUDE.md's NO NAME-MATCHING / NO SIDECARS discipline — typed metadata
  on a closed enum with derived codes, read through one accessor, never string-matched
  message formats or hand-synced parallel lists; today's three stderr formats are
  exactly the rule-2 violation the registry retires).** ONE closed `TrapKind` registry
  (Overflow, DivByZero, Bounds, UnwrapNone, UnwrapError, UnwrapErrorOnOk, AssertFailed,
  Panic), T_ codes derived from variant identity (E_ convention), rendered
  `trap[T_X]: detail at file:line:col` + exit 101; §10.9 `Fault` re-founded as the
  catchable SUBSET (the three; fault-catch untouched; parity lint); no-drops-on-trap
  normative v1; per-code catchability deferred to deep-fault; **ResourceExhausted
  (stack / OOM / host limits) is outside the defined language outcome set** — see C11
  (amended 2026-07-15; naming refined same day): not a TrapKind, not "accepted SIGSEGV,"
  non-adjudicable by conformance.
  **WITH THIS, DECISION BATCH 4 CLOSES — D10 through D21 all ratified 2026-07-06.**

- 2026-07-06 — **D21 RATIFIED (owner GO, judgment delegated + rendered): `gg sim` is
  RETIRED.** Miri's three reasons-to-exist (unsafe trapdoor, aliasing research, no
  executable spec) all map to Gorget non-needs: no unsafe surface, D10's static
  one-sentence aliasing rule, and ggdef — the definition Miri only approximates. The
  Miri ROLE is covered: ggdef/lanes/smith (semantic oracle), sanitizers on emitted C
  (memory-UB), 4-impl differential (compiler correctness). Deletion track = salvage
  scan first, then remove src/sim + command + tests + doc mentions. PHASE-3 PIN:
  data-race detection returns as a ggdef interleaving extension + TSan — not a GIR
  interpreter. A17 CLOSED.

- 2026-07-06 — **D20 RATIFIED by owner (as recommended): f-strings are THE canonical
  text conversion.** `.display()` = the underlying trait method (one concept, two
  positions, not a second way); reject `to_string`-on-primitives AND `String(x)` with
  fix-its naming `f"{x}"` / `.display()`; type-specific `to_string` (UUID/DateTime)
  stays as API. Corpus evidence: ~3885 f-string uses vs 30 `.to_string()` (self-host 0).
  Write-through: language-design:80 ("calls .to_string()" — false; it's Displayable).
  A7 CLOSED. Track = the two already-filed bug entries (String(3) silent miscompile M;
  fix-it polish L) + the doc fix, consolidated.

- 2026-07-06 — **D18 RATIFIED by owner AS THE GENERAL RULE: const-eval mirrors runtime
  semantics exactly, except runtime FAULTS become COMPILE ERRORS.** (Owner: "keep it
  simple, consistent and coherent" — never revisit per-operation.) Falls out: const `+`
  overflow rejects (today it silently WRAPS — semantic/meta.rs:1278 wrapping_* is the
  bug); const `+%` wraps; const div-zero rejects; const float overflow → inf (IEEE,
  mirrors runtime). Implementation track filed; A16's last live residual closes.

- 2026-07-06 — **D12 RATIFIED by owner: D4 enforcement lands in production, STRAIGHT TO
  ERROR** (scout measures blast radius first — surprises are reports, not downgrades).
  `E_MoveWithoutOperator` for drop-tainted types at the six implicit-copy positions,
  matching ggdef exactly + negative fixtures per position + the D4 docs write-through
  (reference/design/book still show the closed single-owner set). Compound-assign rides
  along: `v[i] += x` moves the dead element (D4 move-at-last-use — no residual question;
  clone would be the violation); the resource-element ICE dies with it.
- 2026-07-06 — **D15 RATIFIED by owner: slices are *semantically* owned values + `int[]`/`T[]`
  REMOVED from the surface entirely** (supersedes the filed reject-escape; owner: simplify and
  uniformize now, re-add later as a widening if C-interop demands — then as a dedicated
  FFI type, not the general slice). One sequence type on the surface. **Free views are NOT
  retired:** under D1, a zero-alloc sub-range share (String `cap==0`, future Vector offset+len
  CoW) is observationally identical to an eager owned sub-sequence and remains the production
  *refinement* — the fat pointer / view is an INVISIBLE optimization, never a user-visible
  borrowed-slice type that needs lifetime annotations. Removal track gated on a live-use scan.
  A6 CLOSED. (Clarified 2026-07-15 — see LOG.)

- 2026-07-06 — **D17 RATIFIED by owner: `read_file` is FALLIBLE (`throws`).** Owner:
  avoid panics, keep the server running, recover where possible — recorded as the
  **stdlib fallibility principle** (environmental failures = throws; explicit `_or_panic`
  variants are the opt-in, never the default). Record correction: the docs (book/10,
  language-design §6.4) already said throws; the IMPL is what lags (bare String +
  runtime exit(1)) — the track is impl + doc sweep, and it sweeps the CLASS (all
  env-failure-panicking stdlib fns), not the instance. `parse_int` book typo
  (Result[int,String] → ParseError) rides the same sweep. A12 CLOSED.

- 2026-07-06 — **D10 RATIFIED by owner (deletion rider SIGNED): the exclusivity package.**
  One rule at three sites: for any two access paths in a call, bind, or live capture
  whose PLACES overlap (root + projection prefix), at most one writer (`&`) or mover
  (`!`) during the borrow's live range — a compile error otherwise. Exclusivity is
  production's LICENSE for lazy CoW (violations = the lazy/eager divergence channel; not
  Rust-style lifetime safety — ggdef cannot even observe them). Consequences: duration =
  liveness-normative (extends D7), scope-based permitted as stricter impl; **local
  `&`-binds rejected in v1 — both forms — retiring the round-38 T-D write-through**
  (owner: "should not be legal and it also conflicts with only one exclusive writer";
  reviewed live examples incl. the ASLR-garbage read-back); same-call rejection keyed on
  place overlap, all sigil pairs (closes the accepted-and-miscompiled `f(v,!v)` and the
  name-exact `f(b.data,&b)` miss). Frame-scoped borrows untouched. A29 + A3 CLOSED;
  implementation tracks filed HIGH.

- 2026-07-06 — **D19 RATIFIED by owner: `break <value>` / loop-as-expression REMOVED from
  the v1 surface** ("no loop-as-expression in gorget as for now"). It was a half-wired
  grammar stub: zero fixtures, loop-as-expression unparseable in assignment position, and
  break-value type inference unsound-by-sharing (walked against the FUNCTION return type).
  Removing the half-feature = uniformity; re-adding properly later = pure widening.
  Removal track filed in TODO (grammar + typecheck arms + reference §6.7 + negative
  fixture). A13 CLOSED as a decision.

- 2026-07-06 — **D16 RATIFIED by owner: general UFCS ABANDONED as a design target**
  (language-design.md:85 rewritten same commit — curated trait-exposed duals stay:
  `len`/`Measurable`, `map`/`filter`). Rationale: universal free-fn↔method equivalence
  would (a) let any `&`-taking free fn mutate through method syntax with no `&` at the
  call site — gutting the §4.5 mutation-acknowledgment invariant — and (b) manufacture a
  second spelling for every call, the "multiple ways" the same design table rejects.
  Narrower immutable-receiver-only variant un-foreclosed, not a target. A11 CLOSED.

- 2026-07-06 — **D14 RATIFIED by owner (held once for the write-through interrogation,
  re-confirmed): `get_or`/`get_or_put`/`get_or_else` return VIEWS** (uniform with `.get()`;
  retires the round-8 unconditional clone). **`get_or_put` IS the write-through form**
  (always dict-resident → receiver auto-borrow; Python setdefault precedent);
  **`get_or` is read-only — mutation through it REJECTED + fix-it** (miss-path aliases the
  caller's default). Ratified WITH the no-stored-write-through-variable story: mutation
  flows through PLACES (`d[k] = v`, `d[k].push(x)`, `f(&d[k])`) — "you can never mutate
  `d` through a name that isn't rooted at `d`"; multi-statement idiom = read-modify-
  writeback (one clone, exclusivity-elidable later). Temp-default: live place or
  consumed-within-expression, else reject. Implementation track filed HIGH in TODO.

- 2026-07-06 — **D13 RATIFIED by owner: allocators go TWO-STEP.** Step 1 now: REJECT bare
  (non-`with`) allocator locals at check (closes the silent safe-code heap-UAF —
  `.destroy()` while a backed RAII value lives — AND the leak, immediately); docs
  write-through: book §19 bare-local example + reference `alloc=`/Fallback examples →
  `with` form, phantom `checkpoint`/`restore` removed. Step 2 target (filed): full RAII
  drop-registration with value→allocator ordering via the existing `borrow_deps` primitive;
  bare locals return as a widening. Owner: "safety holes don't wait politely."

- 2026-07-06 — **D11 exit-code sub-decision RATIFIED by owner: uncaught-trap exit = `101`.**
  MSB-range (e.g. 129) rejected: collides with the shell `128+N` signal-death convention AND
  WASI's 0–125 restriction (Wasm-grade bar). `70`/EX_SOFTWARE considered, declined (moribund
  standard, no recognition). 101 = Rust-panic precedent, exact semantic match, massive
  acquired recognition incl. LLM training priors. Language contract = {program's own code,
  101}; ggdef 102/103 stay tool-level; signal deaths stay OS-reported. Full D11 (T_ registry,
  normalized stderr line, `trap:` frontmatter) still PROPOSED in decision-batch-4-proposal.md.
- 2026-07-06 (later): D9 decided (3.0 → "3.0"); P1-infra brief at v3 (2 passes folded);
  matcluster brief at v2 (1 pass folded).
- 2026-07-06: PHASE 1 opened. P1-infra scout complete (float sequencing = hold-floats OPTION A
  w/ 3 prerequisites; ggdef adjudicable ceiling measured; smith ggdef-lane prototype caught the
  A29 same-call-aliasing check-accepts hole — filed HIGH; 2 ggdef defects filed HIGH as
  converter prerequisites). RFC §4 MECHANICS AMENDMENT (no semantic change): conformance
  MATCH-floors are inline dynamic floors per runner (c_emit_comparison precedent) — lints.rs
  ratchets are static/grep-based and unsuitable; RFC wording "floors in tests/lints.rs" is
  superseded on that point.
- 2026-07-05: project approved; 3 scouts ran (docs sweep, bug sweep, prior art); batch-1
  decisions D1–D3 taken; batch-2 questions O1–O3 put to owner; ledger created.
- 2026-07-05 (later): batch-2 decided → D4 (drop-purity), D5 (explicit capture sigil),
  D6 (reject unbound carrier chains). Owner clarification round on D4 recorded in-message
  (drop-purity implications + honest correction re Box/Callable). RFC drafting begins.
