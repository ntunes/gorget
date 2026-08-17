# Define Gorget — Decision Ledger

> The working ledger of the **Define Gorget** project (owner-approved 2026-07-05): building the
> executable definition of Gorget's semantics and verifying all implementations against it.
> Raw scout material is preserved in git history (the former define-gorget `scouts/` tree, removed in the 2026-07-17 repo-hygiene slice). Every decision here is subject to two standing rules:
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
- **Syntax-budget section required on syntax-adding decisions (added 2026-07-18, guards slice):**
  any decision that adds surface syntax — a sigil, operator, keyword, call-site marker, or
  punctuation form — must include a **"Syntax budget"** section stating: (a) the FULL current
  inventory of sigils/operators restated in one place (so creep is visible at ratification
  time, not discovered later); (b) what the addition costs a newcomer (what it can be confused
  with, what it looks like it means but doesn't); (c) collisions/near-collisions with existing
  forms. Rationale: each addition is locally justified; the SUM is a growing symbol vocabulary
  in a language whose pitch is Python-like readability — the budget makes the sum a reviewed
  quantity. A pre-book-freeze consolidation review of the whole inventory is queued in TODO.
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
> **⚠ SUPERSEDED IN PART BY D34 (owner 2026-07-26).** D5's *capture-by-value / "value as-of closure
> creation"* default is **replaced** by borrow-by-default: a closure that stays local **borrows** its
> captures and observes **current** values; an **escaping** closure materialises them at the escape.
> D5's other clauses **STAND**: write-through requires an explicit sigil, body-driven inference of
> mutable capture is retired, and bare names in a capture list are rejected. ⚠ D5's consequence
> *"docs stop calling by-value captures 'immutable borrows'"* is **void** — under D34 a bare capture
> IS an immutable borrow, and the docs should say so.
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
**Consequences:** docs write-through to the error-model notes §9 Q17 (derivation in git history) (mark resolved) +
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
  From-conversion as the FALLBACK.** ⚡ **→ RATIFIED 2026-08-10 as part of D45 (see LOG):**
  `|` spelling; named sets are `type` aliases (`type AppError = IoError | ParseError`);
  closed-by-default + an open marker (glyph TBD at the E2 scout); written-public keyed on
  D43; REQUIRED decl-site `!` on
  inferred-fallible non-public fns; implicit `From` at marks DELETED (widening replaces it —
  the "FALLBACK" in this entry's headline is dead); `gg semver-check` lands the same round;
  impl = the synthesized-union path below + D45's ONE set-algebra module. Implementation =
  round E2 (TODO define-gorget section). ⚡ **ELEVATED 2026-08-10 to the LOAD-BEARING error-model
  decision — see the ERROR-MODEL NORTH STAR entry in the LOG.** Not an ergonomics upgrade:
  Gorget currently has typed errors in signatures with no inference and no set algebra, which
  is the exact mechanical setup that collapsed Java's checked exceptions, and the escape hole
  is already measurable (`throws String`, 148 uses — more than every other error type combined,
  taught 9× in the book — i.e. `throws Exception` in different clothes). A31 also RETIRES the
  implicit-`From` hidden-control-flow hole as a side effect, because set widening needs no
  conversion. The library/semver answers (inference is internal-only, `public` sets are written,
  open/closed sets, closed by default) were PROPOSED in that LOG entry; RATIFIED 2026-08-10
  via D45 (see the head of this entry).
  ⚡ **SIZING CHALLENGED 2026-08-10 — scout the cheaper path before accepting "phase-scale".**
  The framing below (anonymous tagged union-of-enums) may be what has kept this parked. Cheaper
  candidate: **synthesized union enums with structural identity** — `throws IoError | ParseError`
  desugars to a compiler-generated enum whose variants are the member types, so drop, ownership,
  clone and match exhaustiveness all come FREE (it is a real enum, and the compiler already
  monomorphizes). The genuinely new work then reduces to structural identity — normalizing
  `IoError | ParseError` ≡ `ParseError | IoError` over a sorted member list, stable across
  compilation units — which is a type-interning problem, not a new kind of type. If that holds,
  A31 is a round, not a phase. Scout it before sequencing.
  Original framing: the hard core Zig doesn't have is
  PAYLOADS — Gorget errors are owned enums, so an inferred union {IoError, ParseError}
  = anonymous tagged union-of-enums + member-type match syntax + ownership/drop through
  it (phase-scale type-system feature). Also the semver story (public APIs must narrow
  to a named type — lint). Evidence pro: kills the conversion tax+soundness surface AND
  the error-type zoo; single-currency dogfood proof says optimize the common case, the
  sets serve stdlib-composition boundaries (which the D17 class sweep will create).
- **A32 → RATIFIED 2026-07-16 as basic design (forks A1–G1; see LOG): a HOF is fallible
  iff a function argument it invokes is fallible** (throws or declared-Result callback).
  Evidence: scout-a32-hof (derivation in git history). **+ A1×E1 COMPOSITION PIN
  ratified same day (see LOG): rethrows only through opt-in latent params `U(T)!`;
  unannotated function types concretely infallible everywhere (E1 uniform); packet
  scout-a32-a1xe1-composition (derivation in git history).** Impl
  track separate (not opened here; after D29 call-sites when scheduled). Full throws×async
  algebra later. ⚡ **D45 rider (2026-08-10): the impl consumes A31's ONE set-algebra module
  (D45 pin 9) — never a second implementation of error-set union.**
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
  mandate: a33-fault-model-scout-mandate (derivation in git history)
  (launched 2026-07-11); candidate rulings D24 (boundary spec) · D25 (fault-catch
  disposition) · D26 (fallible operators).

- **A34 (owner-noted CANDIDATE, 2026-08-10 — NOT ratified) ⚡ → SPLIT RATIFIED 2026-08-10 as
  part of D45 (see LOG): A34a (top-level render + exit 102) lands at round E0; A34b (the
  chain) is debug-only, NOT value-reachable in v1, and sequenced after D45 pin 6's
  value-position auto-prop kill; the format trigger becomes "a `throw` consuming a catch
  binding" (D45 retired `rethrow`); the paired implicit-`From` hole is resolved by DELETION
  (D45 pin 3), not the purity-gating candidate at the end of this entry. Original entry kept
  below for the chain's design detail: — the ANNOTATED CAUSAL CHAIN
  — error diagnosability as a first-class feature.** Raised in the Zig
  "no-hidden-control-flow" comparison. Today an error value carries only its own payload;
  there is no propagation trace at all (language-design §6.3 formerly documented
  `.context()` / `e.trace()` / `e.source()` / `GORGET_BACKTRACE=1` — an entire subsystem
  that has NEVER existed; corrected 2026-08-10 to *Not yet implemented* + this pointer).
  **The opportunity:** D29 made the fallible mark MANDATORY, so the compiler already knows
  every propagation site statically — that is exactly the handle Zig's error-return-traces
  are built on, and Gorget has it by construction at every hop. Zig's trace carries return
  ADDRESSES and its errors carry NO payload; Rust/anyhow carries context STRINGS but loses
  the typed intermediates; neither carries both. Gorget can: the chain records the span +
  enclosing function at each `!`, and at each `rethrow` ALSO the pre-transform rendering,
  so the trace reads as *what was being attempted at every layer* rather than a list of
  addresses. **Proposed cost split (the design's load-bearing idea): a PURE propagation hop
  (`f()!`, no rethrow) pushes one static descriptor pointer — Zig-cheap; only a `rethrow`
  hop, which the programmer explicitly chose to annotate, pays a format.** Debug builds
  pay; release compiles the records out entirely and `!` degrades to today's branch.
  **Open questions for a scout:** (a) record layout + where the chain lives (per-thread ring
  à la Zig vs carried beside the error value — the latter is a DEBUG-ONLY error-channel ABI
  difference and must be checked against C/LLVM/self-host lane parity, Core #9); (b) whether
  the chain is reachable as a VALUE (`e.chain()`) or only rendered at the top-level handler —
  reachability makes it observable and therefore semantics, not diagnostics, which would bind
  ggdef; (c) measured debug-build cost (Core: perf work measures MEMORY too — the retained
  renderings are the balloon risk); (d) interaction with `on error` cleanup running at each
  hop; (e) faults/traps are NOT in the channel and have no chain — separate concern, do not
  conflate. **Pairs with the other finding from the same discussion:** implicit `From`-conversion
  at a `!` site runs arbitrary user code invisibly (measured: a `From` impl that prints and
  panics does both, mid-propagation, exit 101) — a live hidden-control-flow hole that D4's
  drop-purity doctrine already answers for the analogous case (implicit clones require a
  provably effect-free drop). Candidate remedy: gate implicitly-invoked error `From` on the
  same purity/totality proof, with `rethrow` as the visible spelling for conversions that do
  real work. Both items are the owner's call; neither is ratified.

- **A37 (owner-directed, 2026-08-10 — direction RATIFIED, two phases): ONE DIAGNOSTIC
  NAMESPACE; SEVERITY IS TABLE DATA, NOT A NAME PREFIX.** Owner proposal: name every
  diagnostic uniformly and put its default level in a table. **Why it is right:** severity
  encoded in an identifier prefix is exactly what Layering rule 2 forbids — deciding meaning
  from a name string instead of a typed field read via an accessor. The current scheme is
  ALREADY broken by it: `W_DeadBareParamWrite`'s registry entry declares it "promotes to a
  reserved `E_DeadBareParamWrite` after corpus burn-down" — ONE concept, TWO reserved
  identifiers, because changing severity currently requires a RENAME. With A36's configurable
  levels ratified, the first lint set to `error` would render as `error[W_…]`, the name
  contradicting the output. **Decisive project-specific argument: "land as a warning, burn it
  down, make it fatal" IS the standard operating procedure here (Core #6 spells out that exact
  pipeline), so a naming scheme that demands a rename at the final step is hostile to the way
  this tree actually works** — and each such rename costs a reserved-name pair plus every
  reference. (gcc lands differently, keeping `-Wunused-variable` named `W` under `-Werror`
  because there the prefix means "lint-family", not "currently a warning" — a defensible
  cheaper design, REJECTED here because this project's promotion pipeline crosses that category
  boundary routinely and gcc's does not.)
  **⚡ SPELLING PIN (owner-confirmed 2026-08-10): DROP THE PREFIX ENTIRELY — not `E_<something>`.**
  If severity is data, a prefix saying "error" reintroduces the same lie one layer down. Today's
  render says "error" twice (`error[E_UseAfterMove]`); the rendered level already carries
  severity, so the code becomes a pure identifier: `error[UseAfterMove]` / `warning[UnusedResult]`.
  **Registry columns:** `{ code, default_level, configurable, group, since }` — the A36 table
  plus two. ⚡ **D45 rider (2026-08-10): A38 (D45 pin 10) extends these to `{ …, fix_direction,
  tombstoned, summary, example }`, shipped as machine-readable toolchain data (`gg explain`);
  the `E_MissingFallibleMark` split-by-fix-direction (D45 pin 10(b) — FOUR reasons at HEAD,
  census the enum at split time) rides phase 2's rename round.** `configurable: false` is REQUIRED because not everything can be demoted
  (`type-mismatch = none` must not be a legal setting); the virtue of one namespace is that the
  fixed/configurable boundary then MOVES AS DATA rather than as a rename.
  **PHASE 1 — adopt the table** as the single source of truth (severity, configurability, group,
  `since`, default). Needed for A36 regardless; NO renames. **PHASE 2 — drop the prefixes:**
  mechanical, and it MUST land with the conformance-harness update in the SAME round, because
  the codes are a CROSS-LANE COMPARED VALUE (language-reference §10.10: conformance compares
  "the `E_` code plus the exit class"). **Measured 2026-08-10 (regenerate before acting, Core #5):**
  133 distinct codes (114 `E_` + 19 `W_`); 1,767 references repo-wide; 1,165 of those in
  `tests/` + `spec/` — i.e. the majority are user-visible conformance pins, not source.

- **A35 (owner-directed CANDIDATE, 2026-08-10 — pins ratified below, whole not yet ratified):
  `_ = expr` DISCARD FORM + `W_UnusedResult`.** Today a discarded value is invisible: bare
  `pure_val()`, `v.pop()`, and even `x + 1` as a statement all check clean, and there is NO
  discard form at all — neither `_ = f()` nor `int _ = f()` parses, so the only way to say
  "I don't want this" is to invent a name (`int _unused = …`). **Rule SUBJECT (pinned): the
  EXPRESSION STATEMENT, not the call** — an expression statement whose type is non-`void`
  warns; `_ = ` acknowledges it. Keying it on the call would miss `x + 1`; keying it on
  fallibility (the original framing) was rejected as a category error — D29 already killed
  silent ERROR discard, what remains is a plain unused-VALUE question, and fallible calls
  are if anything MORE likely to be legitimate effect-calls (`file.write(d)!`).
  **Spelling `_ = expr`, not `(void) expr` (owner 2026-08-10):** `_` is ALREADY Gorget's
  wildcard binder — `for _ in xs:` and `catch (_):` both check clean at HEAD — and assignment
  is the one binding position that rejects it, so this CLOSES an inconsistency rather than
  adding syntax. `(void) expr` was rejected: Gorget casts are postfix (`x as float`), bare
  parens are tuple syntax, and it would be the language's only C-ism.
  **⚡ OWNER PINS (ratified 2026-08-10):** (a) `_ = act()` where `act()` returns `void` is
  REJECTED — nothing to discard, and allowing it invites `_ =` as noise-punctuation;
  (b) `_` stays WRITE-ONLY — reading `_` remains a parse error, else it becomes a real
  variable that shadows; (c) stays a WARNING for now — no `E_` promotion ratchet yet
  (contrast `W_DeadBareParamWrite`, whose registry entry declares a promotion path).
  **@must_use was CONSIDERED AND DROPPED (owner 2026-08-10)** — "not yet"; it stays additive
  later as a SUPPRESSOR (marking values whose discard is fine) rather than an enabler, which
  is the better shape and keeps one uniform rule now.
  **Measured blast radius (2026-08-10, regenerate before acting — Core #5):** the common
  mutators return `void` (`push`/`put`/`add`/`set`/`sort` probed at HEAD), so the dominant
  idioms never fire; 14,906 bare-statement calls corpus-wide are dominated by exactly those.
  Confirmed non-void discards among known builtins: 111 (`pop` 74, `remove` 34, `unwrap` 2,
  `clone` 1) — a LOWER BOUND, covering builtin method names only, not user functions
  (`expect_tok`/`advance`/… return types unverified). Census commands in the round brief;
  do not reuse these figures without regenerating. Migration is mechanical (the checker knows
  every call's type — same codemod vehicle as D29's `!` insertion).

- **A36 (owner-REQUIRED, 2026-08-10 — direction ratified, shape is the candidate):
  WARNINGS ANNOUNCE THEIR CODE + A GENERIC SUPPRESSION MECHANISM.** Two owner rulings from
  the A35 discussion: **(1) warnings render their code exactly as errors do.** Today errors
  print `error[E_MissingFallibleMark]: …` while warnings print a bare `warning: …` — all 17
  `W_` codes exist (`SemanticWarningKind::code()`, registry `spec/prose/diagnostic-codes.md:160`
  under the heading "not rendered in phase 1") and feed ONLY the exhaustiveness ratchet and
  that table, because `report_semantic_warning` (`src/errors.rs:459`) never threads
  `.with_code(...)`. One function, and every existing warning gains its code. Sequenced WITH
  A35, whose warning would otherwise be the highest-volume diagnostic in the language and the
  only one users cannot name. **(2) a generic suppression mechanism is REQUIRED** — none
  exists today (no `#[allow]`, no lint levels; `--implicit-clones=…` is a bespoke per-feature
  flag). **The shape is already ratified: GENERALIZE D42** (`decisions.md:1472` — "ONE NAME,
  THREE SCOPES, `allow`/`warn`/`deny`", ratified 2026-07-28, NOT IMPLEMENTED) from the single
  `implicit_clones` knob to every lint.
  **⚡ OWNER PINS (all ratified 2026-08-10):**
  (a) **LEVELS ARE `none` / `warn` / `error` — AMENDS D42**, which pinned `allow`/`warn`/`deny`
  (unimplemented, so the amendment costs nothing; recorded as an amendment, not silent drift).
  Rationale: `error` names the CONSEQUENCE, `deny` named the compiler's DISPOSITION and was
  ambiguous ("deny the clone? deny the build?"). Evidence it was ambiguous: the OWNER misread
  his own ratified D42 from the name, reading `@implicit_clones(deny)` as a functionality
  toggle when D42 defines it as "an annotation that changes no semantics and only makes the
  compiler prove a property or error." A severity word cannot be misread as a toggle. Precedent:
  ESLint/Biome's `off`/`warn`/`error`. `none` over `off` — at a use site `@unused_result(none)`
  describes the CODE, which is what an attribute attaches to.
  (b) **TWO AXES (gcc's shape, owner-directed)** — enablement is separate from severity:
  `--warn=<group>` selects the SET (the `-Wall` axis), `--warn-as-error[=<lint>]` escalates
  (the `-Werror` axis), `--<lint>=none|warn|error` sets one lint at three scopes. Precedence:
  explicit per-lint > group > registry default.
  (c) **`--warn=all` IS BOUNDED BY `since`** — each lint records the language version it was
  introduced in; `all` means "every lint at or before the version the manifest declares". This
  makes gcc's *policy* ("new warnings don't auto-join `-Wall`") MECHANICAL (Core #6): a new lint
  cannot join your `all` without a reviewed version bump, so `--warn=all --warn-as-error` is
  upgrade-safe by construction. **Requires a language-version field in `gorget.toml`** — only
  `[package] version` exists today (`src/manifest.rs:22`).
  (d) **LINT NAMES DERIVE MECHANICALLY FROM DIAGNOSTIC CODES**, never a parallel registry
  (`UnusedResult` → `unused-result` / `@unused_result`): one source of truth per axis (Layering
  rule 3), and the rendered code tells the reader exactly what to write in the suppression.
  (e) **SOURCE-SCOPE LINTS USE A `lint` KEYWORD, NOT `directive`.** Every existing directive
  CHANGES THE PROGRAM (`strip-asserts` removes runtime assertion checks — semantics; `trace`,
  `hot-reload`, `scheduler=…`), while a lint changes only what is REPORTED. Mixing them means a
  reader cannot tell whether a module header is load-bearing for semantics — the wrong ambiguity
  in a language built on "no hidden control flow". So: `directive strip-asserts` (changes the
  program) vs `lint unused-result = none` (changes only reporting). CLI and attributes need NO
  split — the closed value set `none|warn|error` disambiguates them, and only `directive
  name=value` has an open enough grammar to blur.
  (f) **PER-LINT DEFAULT LEVELS** — each lint declares its own default, since D42's
  `implicit_clones` defaults `none` (opt-IN checking) while A35's `unused-result` defaults
  `warn` (opt-OUT); the default belongs on the lint's typed registration.
  (g) **`error` LEVEL IS FATAL** — supplying the lint→hard-error promotion path: a class burns
  down under `error` in the gates before any default flips.
  **BLANKET `--deny=warnings`: CONSIDERED AND DEPRIORITIZED (2026-08-10).** The goal it serves
  (warning rot — a project sitting on 400 warnings effectively has zero) is served STRICTLY
  BETTER in-tree by the existing shrink-only ratchet (`tests/lints.rs`,
  `docs/devbook/25-structural-guards.md`): a ratchet permits a NONZERO baseline while forbidding
  growth (`fmt_no_new_move_bang_in_migrated_corpora` ceiling 861, shrink-only), is per-class so
  one new warning cannot detonate every gate, and already runs in the round-close battery. A
  blanket flag has no middle state — which is exactly why it cannot be adopted mid-migration.
  Its cost: it makes COMPILER UPGRADES BREAKING CHANGES (a new warning fails builds of unchanged
  code — the hazard Rust now advises against baking into project config). Residual value is for
  DOWNSTREAM users, who have no `tests/lints.rs`. **If it ever ships, PIN IT CLI-ONLY — never a
  `directive`, never an attribute** — so it lives with whoever controls the toolchain version for
  that invocation and cannot be baked into source that outlives it. NOT on A36's critical path.
  NOTE the ordering consequence: shipping A35 without A36(2) means `_ = ` per-site is
  the ONLY escape hatch — acceptable (it is explicit and per-site), but it is a one-way door
  for anyone with a file full of legitimate discards.

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
- A17 → CLOSED 2026-07-06 as D21 (see LOG): `gg sim` disposition — RETIRED.
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

- 2026-07-17 — **🎯 2T RATIFIED (owner): drop-taint × materialize = REJECT — materialize-on-write
  is the SEVENTH implicit-copy position.** At every materialize-on-write site (a write to a
  bare/borrowed binding that would CoW-materialize a private copy), a DROP-TAINTED source
  REJECTS with the D12 family (`E_MoveWithoutOperator`: write `!x` to move or `x.clone()` to
  copy) instead of silently materializing — a silent materialize of a drop-tainted value is a
  hidden clone, so the drop side-effect runs twice (the double-fd-close class). Plain VALUE
  types keep silent materialize (the intended CoW semantics; the ratified D2-rider dead-write
  diagnostic covers the dead-copy footgun); `&` write-through and explicit `!`/`.clone()`
  unaffected. Grounding (wave-0 measured): the same taint gate is ALREADY LIVE on all three
  judges at the assign position; ggdef already rejects drop-tainted plain-`self`; production
  currently writes through AND skips the gate (worse-than-filed; ≥2 bugs per Core #8).
  Obligations: negative fixtures per materialize position; all lanes per Core #9; messaging
  aligned with the D12 family; **HARD PREREQUISITE to 2E** (without it, 2E's plain-`self`
  flip converts today's write-through into silent clone→double-drop for tainted receivers).
  Unblocks CoW WAVE 2 (2T→2E leads; wave 2 opens as its own focused round after the D29
  chain integrates + the repo-hygiene slice).
  **LANDED 2026-07-17 (wave-2 landing 1), all four lanes.** Rust safety (decoupled
  `reject_tainted_materialize_on_write` at assign/compound/receiver + the shared
  `reject_tainted_formation_arg` for the `&`-of-value FORMATION position), ggdef
  (`call_arg_source` formation reject), self-host (`reject_if_tainted_materialize_root`
  self-root-aware core + `is_mutating_builtin_method` receiver gate). A `write_through_available`
  discriminator on `E_MoveWithoutOperator` leads with the `&self`/`&<param>` write-through
  remedy at materialize sites only. Fixtures `cow_taint_self_field_write` / `_self_builtin` /
  `_underscore_param` / `_pop_value_pos` / `_compound_projected` / `_formation_self` /
  `_formation_param` / `_whole_amp_param` / `_whole_amp_self`. **Scope-widening Core-#8 catch:
  whole `&p` on a tainted bare param (`void poke(FH p): reader(&p)`) MEASURED a double-close of
  the same fd on BOTH production and ggdef pre-fix — the formation gate now covers whole `&p`/`&self`,
  not just projections.** Guards: `tests/lints.rs::tainted_reject_never_reads_lint_state` +
  `::tainted_formation_arg_gate_sites`.

- 2026-07-17 — **🎯 D29 CAPTURE AMENDMENT RATIFIED (owner; supersedes the catch-attachment
  pin's "Result destination is a disposition; still requires `!`" clause — that clause ONLY).**
  Normative pin: **`!` marks error-channel ACTIVATION — the three control-flow dispositions
  (propagate / `catch` / `rethrow`) (⚡ D45 pin 4: `rethrow` retires at E3; the set becomes
  propagate / `catch` / Result-capture, transform spelled `catch (e): throw wrap(e)`) — on
  BOTH call kinds. A fallible call with NO mark is
  legal exactly where its full `Result[T,E]` is captured by an EXPLICITLY Result-annotated
  destination (binding / param / return): `Result[int, Error] r = f()`. Mark + Result
  destination together is an ERROR (fix-it: remove the `!` — one way to write everything).
  Inferred/`auto` destinations do NOT capture — they type as `T` and require the mark (no
  silent capture through inference). Bare-discard of a fallible call's outcome is ILLEGAL
  for both kinds (mark it to propagate, or attach a handler — silent Error-discard is what
  D29 exists to kill; for kind-2 this is the one new breakage class, census it). Match
  scrutinee stays `T`-typed per D23 (mark required: `match f()!:`; bind to a Result first to
  match Ok/Error). D23 ADDENDUM: a throws call types as `T` in every position EXCEPT an
  explicitly Result-annotated capture position, where it types as `Result[T,E]` (the capture
  form). Kind-2 calls stay Result-typed everywhere as today; their `!` peels and activates
  the channel. `E_MissingFallibleMark` teaches all three exits: mark it, handle it, or
  capture it. The never-surface-`Result[`-in-unhandled-diagnostics contract is unchanged.**
  Derivation: proposed by the OWNER when the D29 impl scout invalidated the blast-radius
  premise (kind-2 = 206 decls / 146 in lib; a literal always-mark forced `r.and_then(f)!`
  absurdities and the `match parse_int(s)!:` peel contradiction). The principled line: catch
  and rethrow are channel-ACTIVE (control flow on Error) and KEEP their marks — the owner's
  "handled calls carry it too" amendment survives; value-plane capture was never handling,
  it is holding, and the Result annotation carries the visibility on the same line.
  CONSEQUENCES: the combinator question dissolves (combinators operate on Result VALUES,
  not fallible calls — no receiver-type predicate needed); the D29↔D17 "Decision 2"
  sequencing dilemma dissolves (kind-2 bind/match/pass/chain sites — lib's 146 APIs — need
  ZERO changes; the migration = 267 throws-kind marks + the kind-2 bare-discard census =
  ONE round, no staging, no softening). Honest trade-off recorded: fallible-call sites are
  no longer 100% greppable by `!` alone — capture sites signal via the annotation.

- 2026-07-16 (session, post-A32) — **🎯 A32/A1×E1 COMPOSITION PIN RATIFIED (owner).**
  Resolves the seam between A1 (inferred rethrows) and E1 (no silent coerce), which read
  literally together deadlock (E1 rejects the very callback-pass A32's inference needs).
  Normative pin: **Unannotated function types are concretely infallible (E1, uniform, every
  position). Latent/inferred effect on a callable is spelled `U(T)!` — A31's bare-`!`
  meaning at the type position. A32 inferred rethrows applies only through `!`-marked
  function-type parameters; inside a HOF body a latent-effect invocation carries the D29
  mark (`f(x)!`), vacuous under an infallible instantiation. Fallible callables never coerce
  to effect-less types; storage of a known-throws callable spells the effect
  (`U(T) throws E` / `Callable[…]`, per Fork C1). Param-position spelling pre-D27 is
  bracketed-only: `Callable[U(T)!] f`. Result/Option combinator callbacks stay unmarked per
  D1 (a throwing callback there rejects — a feature, not an omission). Doctrine wording
  extended: `!` marks the error channel at the site where the channel appears — use,
  declaration, or type; keywords still name concrete contracts (`throws E`). Recorded as a
  REFINEMENT completing A1 (not a re-litigation): inferred rethrows through opt-in latent
  params; implementers must NOT keep the pre-pin "any `U(T)` param is effect-generic"
  reading.** The three callable-type spellings: `U(T)` = concretely infallible ·
  `U(T)!` = latent set (may resolve empty) · `U(T) throws E` = concrete error type.
  Process: the seam was found at the `e44b6120` review; full packet with deadlock examples,
  alternatives (positional rule = "Option 1 in a trench coat" rejected; template-style /
  registry-only / all-latent / wrap-coerce rejected), and the external fresh-pass SIGN OFF
  (fold-ins R1–R5, all folded): scout-a32-a1xe1-composition (git history). Binds the future
  A32 impl brief; changes nothing in D29 call-sites v1.

- 2026-07-16 (session) — **🎯 A32 BASIC DESIGN RATIFIED (owner): forks A1–G1.** HOF effect
  polymorphism for the **error channel only** (v1). Normative pins (evidence:
  scout-a32-hof (git history)):
  - **Rule:** a HOF call is fallible iff it **invokes** a function-argument that is
    fallible (`throws E` or declared `Result[_,E]` return — same one-mark rule as D29).
  - **A1 inferred rethrows:** HOF source decls stay without `throws`; effect is computed
    from the callback(s) at each call (no surface `effect E` / no `rethrows` keyword v1).
  - **B1 same-E:** all invoked fallible callbacks must share one `E`; else type error
    (A31 unions = later widening, not v1).
  - **C1 throws callables:** closures may be `(params) throws E: body`; function types
    carry effect (`R(args) throws E` / safe `Callable[…]` form); lifts Snag #44 when the
    closure is a throws frame. Prerequisite substrate for A32.
  - **D1 split:** collection/iterator HOFs + user HOFs that call `f` **rethrow**;
    Result/Option combinators stay primarily data-plane; fallible callback there requires
    `E_callback == E_receiver` (Result) rather than inventing a second error lattice in v1.
  - **E1 no coerce:** throwing/fallible callable does not silently coerce to an
    infallible function type.
  - **F1 traits:** same rethrows rule for trait/equip methods (including defaults).
  - **G1 async deferred:** forward-compat only — same polymorphism should later cover
    `async` callables; v1 specifies **error effect only**.
  - **Doctrine:** no permanent `try_map`/`try_filter` duals; interim = loops or
    Result-as-data. **D29 impl does not include A32 impl.** D17 env APIs unblocked;
    **fallible stdlib combinators blocked on A32 impl.** Function/closure types must gain
    an error-effect slot at impl time. D29 call-site marks: fallible HOF use → `hof(f)!`.
  Implementation = separate track (after D29 call-sites when scheduled). Async×throws
  full algebra remains phase research.

- 2026-07-16 (session) — **A32 DESIGN PASS OPENED (owner): basic HOF effect polymorphism
  now; D29 implementation NOT resumed.** Scope: syntax + semantics for “HOF fallible iff
  a function argument it invokes is fallible” (throws + declared-Result callbacks per D29
  one-mark rule). Async×throws algebra deferred to a forward-compat sentence only (v1 =
  error effect). Process: scout+design draft → owner forks → LOG ratify → impl later
  (after D29 call-sites when that track runs). **No permanent try_map.** D17 env APIs not
  blocked; fallible stdlib *combinators* blocked on A32 impl. Draft:
  scout-a32-hof (git history) (forks A–G open). Queue item A32 remains open until owner
  pins forks; this entry records the design-pass start only. **Superseded same session:
  forks A1–G1 ratified (entry above).**

- 2026-07-16 (session) — **D29 ↔ D17 SEQUENCING + DOGFOOD HARDENING (owner confirmed).**
  (1) **D29 call-sites land first** (already before C1/C3) — grammar, checks, fmt `!`
  insert, `E_MissingFallibleMark` / `E_UnhandledThrows`, dispositions; small blast radius
  while lib/self-host have zero throws. (2) **D17 class sweep is the dogfood gate** —
  stdlib env failures become `throws`; callers gain `!` via fmt; do **not** block D29 on
  D17, but do **not** call the error surface "closed" until D17 dogfoods real APIs.
  (3) **Final readability re-check post-D17** — pre-D17 pages are illustrative; re-render
  at least one page with real `std.fs` (or peer) after the sweep. (4) **Hardening (in D29
  track):** ship an **integration fixture** that uses a fallible stdlib-shaped API —
  prefer real `read_file` / peer once D17 has landed for that symbol; until then a thin
  local `throws` wrapper with the same call shape is acceptable so always-mark +
  disposition (`!` / `catch` / Result capture) is exercised end-to-end before the stdlib
  flood. Fixture expected output = principled fallible behavior (Core #8 / no redesign
  around gaps).

- 2026-07-16 (session) — **D29 DIAGNOSTIC CODES SPLIT (owner confirmed).** Two codes, not one
  template: (1) **`E_MissingFallibleMark`** — bare fallible call (throws callee or
  declared-`Result` return); message teaches mark `f()!` and lists dispositions
  (`f()! catch …` / `f()! rethrow …` / `Result[T,E] r = f()!`). (2) **`E_UnhandledThrows`**
  — marked call that cannot propagate here (non-`throws` fn, no disposition); message
  teaches handle with catch/rethrow/Result bind **or declare `throws E` to propagate**
  (⚡ D45 pin 4: the message contracts drop `rethrow` when E3 retires it).
  Never primary-fix-it to signature `!` / `! E`. Never surface desugar as type-mismatch
  `found Result[…]` for these cases. Fix-it: insert `!` (or `! ` before `=`). Registry +
  smith/D23 ratchets gain the new code; both compilers + ggdef. Terminology of the codes
  themselves may be revisited if a channel-vocabulary rename lands (owner brainstorm same
  session — not yet decided).

- 2026-07-16 (session) — **D29 FALLIBLE-CALL RULE: ONE MARK FOR BOTH CALL KINDS (owner confirmed).**
  Mandatory postfix `!` applies to **every fallible call**, not only `throws`-declared
  callees: (1) calls/methods whose callee is `throws E`; (2) calls/methods whose
  **declared return type** is `Result[T,E]`. Same dispositions (prop / `catch` /
  `rethrow` / Result-bind) (⚡ D45 pin 4: `rethrow` retires at E3). Bare fallible call
  remains always illegal. Scope of the
  mark = Call/MethodCall whose resolved callee is throws **or** returns Result — not
  every expression of type Result (locals/combinators are separate). **`Result[T,E]`
  stays a first-class value type** (deferred handling, collections, combinators,
  non-throws boundaries); D29 does not abolish it. Doctrine: functions declare
  fallibility with `throws` (or return Result when the API is data-first); every
  fallible *call* is marked `!`; Result is how you hold an outcome as a value.
  Declaration-style preference (stdlib prefer-throws vs Result-returning APIs) left
  open as style/guidance, not a deletion. Census/fmt instruments must cover both paths.

- 2026-07-16 (session) — **D29 PACKET/READABILITY WRITE-THROUGH DONE (owner confirmed).**
  scout-d29-packet + scout-d29-readability (git history) brought to currency:
  LOG is normative; `!` joins `throws` (no signature `! E`); call-sites-only migration;
  always-mark + disposition table; diagnostics prefer `throws E`; readability AFTER pages
  use `throws E` + call-site `!`. Full book/reference sweep still rides the D29 landing.

- 2026-07-16 (session) — **D29 CATCH-ATTACHMENT / DISPOSITION GRAMMAR PINNED (owner confirmed).**
  Bare fallible call is **always illegal**. Disposition attaches to the **marked** expression
  (Swift always-mark; supersedes the scout packet's Rust-`?` "handlers eat bare calls" wording):
  (1) propagate — `f()!` inside `throws E`; (2) recover — `f()! catch (e): fallback` (postfix
  on the marked call); (3) transform+rethrow — `f()! rethrow (e): wrap(e)`; (4) capture as
  data — `Result[T,E] r = f()!` (Result destination is a disposition; still requires `!`).
  Precedence: `!` binds to the call first; then `catch`/`rethrow` attach to that marked expr
  (`(f()!) catch …`). Nested: each fallible call carries its own mark (`g(f()!)! catch …`).
  No second mark and no "handle without `!`" form. Remaining D29 open items: A32 HOF
  path; handled-sites census; `!=` maximal-munch tests. (Result-returning-call `!`,
  diagnostic split, D17 sequencing+fixture hardening: PINNED same session.)
  ⚡ **AMENDED by D45 (2026-08-10): disposition (3) is RETIRED — `rethrow` leaves the grammar;
  its meaning is spelled `catch (e): throw wrap(e)`, one handler construct (D45 pin 4).**

- 2026-07-16 (formal ratification; the six follow-through pins above postdate it) — **🎯 D29 RATIFIED (owner, formal — packet-backed: census + accept-both
  prototype + collision corners + readability pages, scout-d29-packet (git history)): VISIBLE
  ERROR PROPAGATION.** Final scope with both same-day amendments consolidated:
  (a) **call-site `!` is MANDATORY on every fallible call — the uniform FALLIBLE-USE MARKER**
  (Swift model): handled calls carry it too (`f()! catch (e): …`); unhandled marked call in a
  non-throws fn = E_UnhandledThrows ("handle it with `catch`, or declare the function to
  propagate"); bare fallible call = always an error ("mark the fallible call: `f()!`"). D26's
  `+!`/`**!` are instances of the one rule: suffix-`!` on any operation = fallible.
  (b) **`!` NEVER takes a type**: bare signature `int f(args)!:` is grammar-locked as A31's
  inferred-error-set spelling (parses, rejects with the teaching diagnostic until A31);
  `throws E` REMAINS the explicit contract spelling — `!` JOINS `throws` (sigils = flow at
  use-sites; keywords = contracts at declarations). (c) Implementation = CALL-SITES ONLY
  (~61 propagation + the handled-sites count to be measured; the signature migration is
  cancelled); `gg fmt` inserts mechanically; sequenced BEFORE C1/C3; the readability census
  renders post-D29 pages. **Catch-attachment grammar: PINNED 2026-07-16.
  Packet/readability write-through: DONE 2026-07-16.** Remaining open items for the
  implementation brief: the handled-sites census; `!=` maximal-munch parse tests
  (disposition proven); Result-returning-call `!` and diagnostic wording (owner queue).
- 2026-07-16 (late) — **D29 DIRECTION AGREED (superseded by the ratification above; kept for
  the derivation record): visible error propagation — `!` mandatory
  at THROWS CALL SITES + `!` replaces `throws` in signatures.** The critique (no way to see a
  throws call at the site) is valid by the language's own sigils-mark-effects principle; the
  fix makes D26 an INSTANCE of a general rule (suffix-`!` on any operation = fallible, failure
  propagates from here: `+!`, `**!`, `f()!` — one grammar). Owner-pinned sub-answers:
  (1) **AMENDED (owner 2026-07-16, post-readability-read): `!` NEVER takes a type** — the bare
  signature `int f(args)!:` is the ONLY `!`-signature form and means A31 inferred error sets;
  the explicit-contract spelling REMAINS `throws E` (pending A31's design — likely permanently,
  per the Zig public-API guidance: inferred sets are unstable contracts on API boundaries).
  D29's framing is therefore "`!` JOINS `throws`", not replaces: sigils mark FLOW at use-sites,
  keywords state CONTRACTS at declarations. Consequences: D29's implementation shrinks to
  CALL-SITES ONLY (~61 + the handled-sites count; the 179-signature migration is CANCELLED —
  no double churn when A31 lands); interim rule: bare signature-`!` PARSES but rejects with the
  teaching diagnostic "inferred error sets are not yet implemented — declare `throws E`" (the
  grammar locks now, A31 flips one switch). (2) **AMENDED (owner, 2026-07-16 same-day): `!` = the uniform FALLIBLE-USE MARKER, not
  "propagate"** — every fallible call carries `!` INCLUDING handled ones
  (`a = f()! catch (e): …`); the Swift model (mark always; disposition is the handler's job),
  NOT Rust's ?-operator model. Derivation: D26's operators keep their `!` in handled contexts
  (`a +! b catch …` — the operator IS the fallible variant), so calls must too or the
  one-general-rule elegance collapses. Unhandled marked call in a non-`!` function =
  E_UnhandledThrows, message: "handle it with `catch`, or declare the function `!` to
  propagate"; bare fallible call = always an error ("mark the fallible call: `f()!`").
  CONSEQUENCES for the packet: (a) the CATCH-ATTACHMENT grammar is a new ratification
  sub-question (postfix expression-catch binding to the marked expression vs today's
  statement forms — the implementation brief pins it against the existing catch syntax);
  (b) the census needs a SECOND count — fallible calls inside HANDLED contexts (the scout's
  61 counted the propagation chokepoint only); (c) the E_UnhandledThrows message rewords. (3) Migration is compiler-driven and mechanical (the
  checker knows every throws call site; `gg fmt` inserts) — rides the C3 fmt vehicle or a
  sibling; census required; the READABILITY CENSUS must render post-D29 pages (not just
  post-D27). Honest cost recorded: amends auto-propagation's invisibility (semantics stay
  automatic; syntax becomes visible — the Swift/Rust/Zig convergence point). Scout launched
  2026-07-16: census + grammar prototype + collision corners (`a()!=b` maximal-munch, chains
  `f()!.m()!`, function-type spelling `int(int)!`, optional interactions) + the packet.
- 2026-07-16 (later still) — **D2 RIDER RATIFIED (owner): the DEAD-BARE-PARAM-WRITE diagnostic.**
  No grammar special case for `self` — bare `self` stays legal (it is the CORRECT zero-cost
  spelling for read-only methods; banning it would force write-through `&self` borrows on
  getters, lying about intent and tripping D10 exclusivity needlessly). The footgun is exactly
  the DEAD-WRITE subclass: **a write to ANY bare parameter whose materialized private copy is
  never subsequently read is flagged, uniformly — `self` just falls out as the first
  parameter.** The scratch-copy idiom (mutate-then-READ, e.g. sort-a-private-copy) stays legal.
  Ratified message (verbatim, does the teaching): *"this writes to a private copy that is
  never read — the caller's value is unchanged; did you mean `&self`?"* (param-flavored
  variant: "…did you mean `&<param>`?"). Severity: on-by-default `W_` shipping IN THE SAME
  LANDING as 2E's behavior flip (the warning must not lag the flip), promoted to `E_` after
  corpus burn-down (Core-#6 ratchet). Enforcement: the CoW campaign Track 2E carries it as a
  mandate; registry row + prose land with it (all lanes per invariant #9; ggdef within subset).
  **LANDED 2026-07-17 (wave-2 landing 1) as an on-by-default `W_` (production-only surface).**
  `self` seeded into the dead-write tracking set (Copy-exclusion skipped — self is always
  pointer-passed, so it materialises); the scratch-copy idiom stays legal via read hooks on the
  Identifier, f-string interpolation, AND SelfExpr read paths. Fixtures `deadwrite_warn_self`
  (fires, `&self` flavor), `cow_self_scratch_read` + `cow_self_scratch_fstring` (silent). Corpus is
  at 1 true-positive post-migration (`generic_equip_method` → `&self`), so the `E_` promotion is
  filed as a follow-up (owner's call on whether `W_` rides one full round first).
- 2026-07-16 (later) — **STAGING RULING (owner): wrapper deref access REJECTS until implemented.**
  `Box[T].field` / wrapper method auto-deref (§9.4 deref coercion) is UNIMPLEMENTED end-to-end
  (field read returns 0; method deref cc-fails — RV-A scout measured). The earlier staged
  acceptance (`check_gg_ok` + `#[ignore]`d run-test) is REVERSED: a fixture blessing silent
  wrong output is the Core-#8 red-flag pattern regardless of who wrote it. Until the
  deref-coercion backend track lands, wrapper deref access is an E_-reject with a
  "not yet implemented" message; that track flips acceptance + run-tests together.
  Enforcement: the RV-A track.
  **SCOPE CLARIFICATION (2026-07-16, recorded after RV-A passes 1-2 measured ALL EIGHT
  wrappers; supersedes any broader reading):** the reject partition is
  **GUARDS-vs-CONTAINERS**. Guard/ReadGuard/WriteGuard field access stays ACCEPTED.
  ⚠ **PREMISE CORRECTED 2026-07-27 — the original wording, "Guard/ReadGuard/WriteGuard
  field access WORKS today (green run fixtures, correct output)", is FALSE at the WRITE
  faces, and false even at a guard LOCAL root (no parameter indirection).** Measured at
  HEAD, all `gg check`-clean and all building cleanly: on a `ReadGuard[Config]` LOCAL from
  `RWLock.read()`, `inc(&rg.fd)` prints **10** (write silently dropped), `rg.fd = 99`
  prints **10**, and `rg.fd += 5` prints **10**; the `WriteGuard` control `inc(&wg.fd)`
  prints **11** and is correct. So the accurate scope of the ruling's exemption is
  **guard field READS, plus WriteGuard writes** — a ReadGuard write is silently swallowed,
  which is exactly the blessed-silent-wrong-output shape the ruling exists to refuse, and
  the intended answer there is a check-time REJECT (the producer's own read-only-guard arm
  already carries the comment "type checker should reject in future"). Repro:
  `known_gaps/sound_readguard_write_faces_dropped.gg`. A separate, filed defect makes the
  same reads wrong through a PARAMETER (`known_gaps/sound_guard_param_field_unsafe.gg`).
  The exemption itself stands — these are not the wrapper-deref-coercion class — but it
  must not be cited as evidence that guard field access is correct.
  Box/Shared/Weak/Mutex/RWLock DIRECT field access
  all print silent garbage-0 (measured) — they REJECT, with a 3-way diagnostic split:
  field-present-on-inner AND the wrapper is a documented deref-coercion target — **§9.4
  names ONLY Box** (design-doc :1707-1712, sole example) → E_DerefCoercionUnimplemented;
  field-absent-on-inner OR primitive-inner OR the wrapper is NOT a §9.4 target
  (**Shared/Weak/Mutex/RWLock** — Weak's design access is `.upgrade()`, never deref, §9.2;
  Mutex/RWLock via .lock()/.read(); promoting Shared to a deref target would be a §9.4 doc
  change + a one-line enum reseed, an owner call) → E_NoFieldFound. Wrapper METHOD auto-deref
  stays with the deref-backend track (fails loudly today). Owner notified in-conversation;
  the principle is unchanged — reject where broken, accept where working.
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
  snapshots). Full matrix + fix-shape per lane: scout-rvc-compound-assign (git history).
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
  the ggdef-elaborate-move prototype (git history), ggdef 127/0, conformance 195/195, 100% production
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
  (⚡ AMENDED by D45 pin 7, 2026-08-10: class **`102` = uncaught channel error** joins the
  scheme; `main throws E` legalized for ANY error type with `E_MainThrowsNonInt` retiring at
  round E0; `101` keeps trap + ICE; `103` unchanged.)
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
  `static-error`/`parse-error` tiers → exit 1 in the ggc/ggdef RFC (git history).
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
  packet review, scout-wave-census (git history)) — with D27 + D28 (below), the full
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
  scout-wave-census (git history)); D7 capture lists have ZERO corpus
  uses → the capture re-spelling (`(^name, &total)(x):`, `^():` move-all) is a
  pure spec rider on D7. Implementation = its own bootstrap-gated track (lexer/
  parser/formatter both compilers + `E_MoveWithoutOperator` and the `expr.rs:593`
  move-hint diagnostics + docs sweep; `gg fmt` is the auto-migration vehicle).
  D24 (boundary) / D25 (fault-catch removal) / D26 (fallible operators) remain
  RECOMMENDED-pending-formal-ratification from the scout report
  (`/tmp/scout_a33_report.md`, mandate a33-fault-model-scout-mandate (git history)).

- 2026-07-07 — **D23 RATIFIED by owner (was queue item A30): THE THROWS TOTALITY
  INVARIANT.** Normative: "a throws call is an expression of type T in EVERY position;
  its Result-ness is unobservable except at a Result-typed binding or a catch." Plus the
  diagnostic contract: the checker never surfaces the desugar ("found Result[T,E]" is
  banned from user-facing diagnostics) — violations of the virality rule say "this call
  throws E; declare `throws E` or handle it (catch/rethrow/Result capture)." (⚡ D45 pin 4:
  contract wording drops `rethrow` when E3 retires it.)
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
  re-derivation that settled it is now saved in the batch-4 proposal §D14 (git history): the
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
  101}; ggdef 102/103 stay tool-level (⚡ superseded twice: the 2026-07-15 scheme moved
  ILLFORMED off 102, and D45 pin 7 later assigned 102 a COMPARED cross-lane meaning —
  uncaught channel error); signal deaths stay OS-reported. Full D11 (T_ registry,
  normalized stderr line, `trap:` frontmatter) still PROPOSED in the batch-4 proposal (git history).
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
- 2026-07-19 — **DIAGNOSTIC-POLICY RULING (owner): `NeedlessMutableBorrow` stays a WARNING, never promoted to an error.** A read-only `&` param is charter-suspect noise (an unnecessary clone at the formation site) but not unsound — the program means what it says and runs correctly. The warning is the vocal teaching instrument ("consider removing `&`"); a hard `E_` reject was judged too much. Pairs with the same-day dead-temp-write ruling (writes landing on owned temps nobody reads: warning track via the D2-rider convergence, not a reject). Both cohere as one stance: UNSOUND shapes reject; WASTEFUL-but-correct shapes warn.
- 2026-07-19 — **D30 RATIFIED (owner, in-session Q&A with probe evidence): NARROW-TYPE OVERFLOW TRAPS, UNIFORM WITH `int`.** Probe at `99924c2e`-era HEAD: `int8 x = 127; x = x + 1` printed 128 and `uint8 255+1` printed 256 (the declared bound never enforced — the narrow type was a runtime fiction), while 64-bit `int` overflow already trapped `T_Overflow`. Ruling: plain arithmetic on int8/16/32 + uint8/16/32/64 (every width) traps `T_Overflow` on overflow — ONE rule for all widths; D26's fallible family (`+! -! *! /! %!`) is the recoverable path (ArithError); deliberate wrapping gets an explicit std function (no new sigil, per the syntax-budget rule). Rejected alternatives recorded: defined-wrap (silent corruption + inconsistent with int's trap), debug-trap/release-wrap (two behaviors for one program — anti-ggdef), reject-plain-narrow-arithmetic (ergonomics tax). Implementation = its own cross-lane track (semantic change: programs that printed out-of-range values now trap → conformance fixtures both compilers + ggdef adjudication where in-subset; the C backend must emit checks on the narrow paths like the wide path already does, never signed-overflow UB).
- 2026-07-19 — **D28 AMENDED (owner-ratified after the cross-language survey): `**` KEPT, with THREE RIDERS.** The owner second-guessed `**` vs `pow()`; the survey settled it (the readability lineage Python/Ruby/JS all spell `**`; TC39's fresh 2016 choice ADDED `**` with a guardrail; the C-family's pow() is historical accident; `^` is doubly taken in Gorget — infix XOR + prefix move). Riders: (R1) **unparenthesized unary minus with `**` is a compile REJECT** (`-x ** 2` → error, write `-(x ** 2)` or `(-x) ** 2` — the JS/TC39 guardrail); (R2) **right-associative** (`2 ** 3 ** 2` = 512 — the universal convention since Fortran; document, don't innovate); (R3) **no type-switching**: `int ** int → int` (negative exponent = domain-fault trap; a fallible `**!` deferred until demanded — syntax budget), `float ** float → float`, mixed operands REJECT per no-silent-lossy. Forward-compat note: power-`**` is infix-only, so prefix-`**` stays free (splat-style syntax later, same prefix/infix disambiguation as `&`/`^`). PROBE EVIDENCE (2026-07-19): `**` is ratified-but-UNIMPLEMENTED — today `2 ** 10` lexes as `2 * (*10)` → misleading `E_DerefNonBox` (a newcomer trap in its own right). Implementation vehicle: enforcement-wave Batch C1 (D26+D28 operators) — build the riders in from the start, nothing to migrate; R1's reject + R3's typing land WITH cross-lane fixtures (Core #9) and a proper not-yet-implemented parse error is NOT an acceptable interim (implement, don't excuse).
- 2026-07-19 — **D31 RATIFIED (owner): METHOD call sites require call-site `&` for `&` params — the asymmetry closes on the STRICT side.** Probe-evidenced ruling: production rejected `add_all(v)` into `void add_all(Vector[int] &vals)` (E_OwnershipMismatch) but silently accepted the IDENTICAL contract as a method (`c.add_all(v)` — the check_call_ownership check is invoked only from the free-call arm, `check_expr.rs:315`; method calls never reach it). Owner: "Tighten methods to match free functions. This is the original gorget design (described in README and likely elsewhere too)" — the README's call-site-visibility promise ("moves and borrows are marked at call sites... ownership transfers are visible where they happen") is the design authority; the method leniency was an enforcement GAP, not a rule. CONSEQUENCES (the implementation track, Core #9 all-lanes + the expectation-flipping lesson — gates on the FULL ggdef suite): (1) route method calls through the same ownership check (Rust safety pass + the SH typechecker lane); (2) corpus migration — bare method args into `&` params gain the sigil (census via a report-mode sweep before the flip; `method_mut_borrow_arg` respells `&vals` [output stays 6] and the bare spelling becomes a NEG fixture); (3) ggdef flips its method-path write-through to the same ElabError (it already implements both faces post-1a — a small edit); (4) conformance pins on every lane. SCOPE NOTE: this ruling covers ARGS; the `&self` RECEIVER's call-site implicitness (`c.add_all(v)` does not mark that `c` mutates) is a SEPARATE open question deferred to the sigil-consolidation sitting — not silently expanded here.
- 2026-07-20 — **D31 ADDENDUM RATIFIED (owner, after the census + history memo): THE SIGIL RULE — ownership sigils mark NAMED PLACES; expression temporaries are EXEMPT.** History: the free-fn `!`-on-temporary rejection was an UNRULED ACCIDENT (`a62b0998` 2026-02-09 — a syntactic expected!=found compare never asked the temp question); the adjacent ratified rulings lean the other way (§9.1 "a fresh temporary is not a live place and moves without an operator"; D10(a) "bind- and call-position moves treated identically"); the tree's idiom is unanimous (42× `!named`, 0× `!Ctor(...)` ever); and the census showed 231 of 239 would-be `!`-additions (96.7%) were temporaries. Owner's ratified justification: a temp is last-use BY DEFINITION and is already optimized to an implicit move even without the sigil — requiring `!` there annotates a definitionally-true, already-acted-upon fact (pure ceremony). Precision: the exemption keys on the compiler's PLACE/TEMP distinction (`expr_is_place`), never on syntax shape — a NAMED local at last use still spells `!` (there the sigil carries reader-facing binding-dies-here information + a checked deadness assertion). THE COMPLETE RULE: `&` required at ALL call sites (free-fn AND method — D31 base) for `&` params; `!` required on NAMED-place args to `!` params at ALL call sites; expression temporaries pass BARE into any param kind. IMPLEMENTATION (one track, ~30 sites): (1) fix the free-fn accident — temp exemption in `check_call_ownership`; (2) extend the (corrected) check to method calls; (3) migrate ~22 `&`-adds + 8 named-local `!`-adds; (4) ggdef mirrors both (its post-1a check gains the temp exemption + the method form); (5) SH lane: the filed no-ownership-check-at-all gap gets the corrected rule when built — never the accidental one; (6) docs reconcile (§9.3 gains the explicit temp rule; §9.1 cross-references). Gates: the full ggdef suite + conformance + parity (expectation-flipping lesson).
- 2026-07-20 — **D31 ADDENDUM-2 RATIFIED (owner; SUPERSEDES Addendum-1's temp exemption): FULL STRICT, with the DX RIDER.** The deliberation arc, recorded honestly: the temp exemption was ratified earlier the same day (Addendum-1, on the last-use-by-definition argument), then re-examined when the owner probed the reader-facing costs and REJECTED in favor of full strict. THE RULE, one sentence: **where consumption is part of the API contract (a `!` param), it is always spelled at the call site — bare = borrow, `&` = the callee writes through, `!` = the callee consumes, at every call site, free-fn and method, named place or temporary.** Rationale (owner + orchestrator aligned): (1) REFACTOR-STABILITY — the spelling is invariant under name↔temp refactors (the exemption made call sites churn); (2) SIGNATURE-FREE READING — the call site alone declares the contract (`f(!Tok(1))` vs `g(Tok(1))` distinguishes consuming from borrowing APIs without opening signatures — the README's visible-where-it-happens identity, held deliberately against Rust's invisible moves); (3) ONE TEACHABLE SENTENCE, no footnotes; (4) the D26 precedent (`+!` made fallibility visible at every use site). PRECISION for the ledger: full strict governs CONTRACTUAL consumption only — bare values at non-`!` consuming positions (push/ctor/return/capture) still auto-move-when-dead per CoW (unobservable optimization, not contract; no sigil marks it). The free-fn strict check (`a62b0998`), accidental at birth, is hereby RATIFIED as the rule. **THE DX RIDER (binding):** the bare-temp `E_OwnershipMismatch` must be excellent and auto-fixable ("this call consumes the value — add `!`", exact-span suggestion); a fix-it/tooling item (gg fmt-adjacent auto-insertion) files with the track. Detail: temps into `&` params = probe production and likely REJECT outright (a write-through to a temp is a dead write by definition) — the track decides with evidence. Migration: the censused ~262 sites (22 `&`-adds + 231 temp-`!`-adds + 8 named-`!`-adds + lib); ggdef mirrors full-strict both forms; SH gets the corrected-full rule when its ownership check is built. Gates: full ggdef + conformance + parity (expectation-flipping).
- 2026-07-20 — **D31 size confirmation (owner):** the true census (612 sites — the per-directory lexer/parser/resolver copies, 2.6× the estimate at ratification) is explicitly CONFIRMED fine; and bugs exposed as a consequence of the strict check are WELCOMED ("if we find bugs as a consequence of this change, that's also good by me"). First such find already banked: the loop-carried owning-`!`-param move_zero lowering bug (`b36637c5` — valid code, wrong GIR, honest validator), latent until the migration spelled the moves.
- 2026-07-21 — **BARE-PARAM `.get()`-CHAIN MUTATION MATERIALIZES (owner-ratified, Option A of 3, after the SH-CoW scout's 3-lane table).** The open question: what does mutation through a BARE (non-`&`) param's `.get()`-chain root mean — `void f(Func f): f.blocks.get(bb).unwrap().term = t`? Three lanes disagreed: ggdef MATERIALIZES (caller unchanged, `20`) · self-host MATERIALIZES (`20`) · Rust gg WRITES THROUGH (`21`). Three positions were put to the owner — (a) materialize (bare = immutable borrow → the ratified lazy-CoW "mutation materializes at the closest immutable context"), (b) write-through (treats bare like `&`, contradicts bare=immutable-borrow), (c) reject (require `&` to mutate through). **Owner ruled (a) MATERIALIZE.** Rationale reinforcing the ruling: it is the UNIFORM reading — Rust ALREADY materializes the sibling projections on a bare param (`f.blocks.push`, `f.blocks[i]=`, `f.n=`); only the `.get()`-chain STORE wrongly wrote through, so Rust was internally inconsistent with its own behavior. THE RULE: bare-param mutation materializes a private copy at every projection path INCLUDING a `.get()`-chain store; write-through requires `&`. PROCESS NOTE (owner-important): ggdef is the MEASUREMENT instrument, NOT the legislator — the orchestrator was corrected for prematurely crowning ggdef "correct"; the owner ratifies, and only then does a lane's disagreement become a defect. CONSEQUENCES (all cross-lane, Core #9): ggdef + self-host already conform (materialize); **Rust gg's `.get()`-chain bare-root write-through is now a CONFIRMED oracle-hygiene DEFECT** — fix Rust to materialize the bare `.get()`-chain root + emit the dead-write warning it already emits for the sibling projections (own scout; find where Rust's auto-borrow-from-get skips the bare-root materialize that `cow_before_mutation` applies to the siblings; a bare-param materialize + dead-write fixture pins it). ORTHOGONAL to the SH-CoW Face-A fix (owned-local + `&`-param write-through, where all three lanes already agree) — Face-A does not touch bare-param and does not depend on this ruling.

- 2026-07-25 — **D10(b) ADDENDUM 2 RIDER RATIFIED (owner): PLAIN `=` DOES NOT JOIN `op=` — there is no conflict, so it is ACCEPTED.** The question: ADDENDUM 2 makes the compound-assign LHS an implicit exclusive writer for the statement, so a writer in the RHS on the same root rejects (`v[0] += grow(&v)`). Does plain `=` (`v[0] = grow(&v)`) join it? **Ruled: NO.** **The canonical statement DERIVES the distinction — it is not a carve-out:** a conflict requires two access paths whose **live ranges INTERSECT**. `v[0] += rhs` must **READ the LHS first** to compute the result, so that read is **live across the RHS** and a writer inside the RHS intersects it → conflict → reject. `v[0] = rhs` **only WRITES the LHS**; its place is computed **after** the RHS evaluates, so its live range **never intersects** the RHS → **no conflict → accept**. The two forms differ because one holds a live read across the RHS and the other does not. **MEASURED (2026-07-25):** `v[0] = grow_and_name(&v)` with a 4096-push realloc in the RHS — `gg check` ACCEPT, **ASan-clean**, prints `grown`, and **ggdef agrees** (`grown`). **Cost of the rejected alternative, stated for the record:** extending the reject would be pure widening with **no soundness gain**, and would break **5 currently-clean self-host sites** (`typecheck.gg:4346`, `meta.gg:292/296/1073/1147`) — forcing `.clone()` or restructuring to satisfy a rule protecting nothing, which is charter-adverse. ⚠ **Two live caveats, both OUT of this rider's scope and unchanged by it:** (1) **ADDENDUM 2's `op=` reject is RATIFIED but NOT YET IMPLEMENTED** — measured 2026-07-25, `v[0] += grow_and_name(&v)` is still ACCEPTED today; enforcement is the filed RV-C track. (2) ADDENDUM 2 recorded that the `op=` hazard is **ELEMENT-TYPE-DEPENDENT** (`Vector[int]` vs a custom-`Drop` element behaved differently), so RV-C's own probe set must include a custom-`Drop` element — a `String`-element probe running clean does **not** clear that side. Neither caveat touches the plain-`=` answer, which rests on the LHS never being read.

- 2026-07-25 — **D10 CANONICAL STATEMENT RATIFIED (owner) — the whole exclusivity rule in three sentences, superseding the "writer or mover" phrasing wherever it appears.** The rule:
  > **Every resource value is a borrow until it is mutated or consumed. Two access paths CONFLICT when their storage OVERLAPS, their LIVE RANGES INTERSECT, and at least one of them can WRITE that storage during the intersection. A conflict is REJECTED — unless the conflicting path is a READER and the compiler can place its clone LAZILY, at a visible mutation point, in which case it MATERIALIZES instead.**

  **What each clause does.** *Overlap* keeps `f(&m.a, &m.b)` legal (disjoint sub-places, two writers). *Live-range intersection* is what the implementation kept under-approximating — it compared only top-level args of a single call, missing get-chains, `static` roots, `a = w` aliases, struct/tuple literals, operator operands, and writers nested one call deep. *Ability to write* replaces the coarse "writer **or mover**" test (see the ability-to-write rider on ADDENDUM 3): a move transfers a pointer, so `Pair(v[0], !v)` accepts at an inline site while `f(v[0], !v)` rejects at an opaque call — same sigil, opposite verdicts, one question. *The reader-only lazy escape* is the asymmetry, and it is DERIVED, not stipulated: cloning a reader preserves meaning, cloning a writer would silently discard its writes, so only readers are rescuable. **Verified against every shape measured in the 2026-07-25 round** — `f(&v,&v)` reject · `f(&m.a,&m.b)` accept · `f(v[0],&v)` reject · `s=v.get(0); grow(&v); print(s)` materialize (1 clone taken / 0 untaken) · `Pair(v[0],!v)` accept · `f(v[0],!v)` reject · `Pair(v[0],mutate(&v))` reject · `f(!x,!x)` not this rule's business (`E_DoubleMove`, liveness axis). **Docs write-through:** `language-design.md` §3.5 rewritten — it previously stated RUST's rule ("never both simultaneously"), which Gorget does not implement, since the lazy escape accepts exactly the case Rust rejects.

- 2026-07-25 — **D10(b) ADDENDUM 3 RATIFIED (owner): AGGREGATE CONSTRUCTION IS AN OWNERSHIP BOUNDARY, NOT A BORROW POSITION.** The question, forced by the D10 structural-chokepoint track: does the place-overlap rule range over the args of a struct / tuple / array / enum-variant literal the way it ranges over CALL args? **Ruled: NO at owned-field positions.** A ctor's bare arg is a **consuming** position — the field must own, so the compiler already clones there — which makes it a **VALUE, not a live alias**, and D10(b)'s ratified ALIAS-vs-VALUE cut exempts it. **Grounding (all pre-existing, none new):** `docs/devbook/11-copy-on-write.md` — tuple/struct field init are "unconditional leave-behinds" cloned via `ensure_owned_at_boundary`, enum field init via `emit_enum_init_owned`; `CLAUDE.md` "Ownership at Consuming Positions" — boundaries are "collection puts, **constructor / struct / enum field init** like `S(name)` / `Some(name)`, returns, closure captures … **uniform across all of them — there is no push-vs-constructor split**"; and the standing owner ruling that auto-clone is uniform at ALL ownership boundaries, ctors included. **MEASURED (ASan, heap-forced, this session):** `Pair(v[0], !v)`, `Pair(v.get(0).unwrap(), !v)` and `(v[0], !v)` are all CLEAN and print the **pre-move** value — the boundary clone fires; the genuinely hazardous order `Pair(!v, v[0])` **already rejects `E_UseAfterMove`**, i.e. on the LIVENESS axis, which is the correct one. There is no divergence channel, and a `.clone()` demanded here would be a clone **the compiler already places** — a charter breach (excess implicit cloning), not a safety win. **THE PREDICATE IS PER-POSITION-TYPED, never blanket:** an *owned* field position ⇒ consuming-exempt value (no participant); a `Ref[T]` / `MutRef[T]` field position ⇒ a **genuine borrow participant** (the field stores a reference, so the arg IS a live alias — read the already-typed `field_ref_flags` / `compute_struct_field_ref_flags`, do not re-derive); a `!` wrapper ⇒ a **mover** on the liveness / `E_DoubleMove` axes. **CONSEQUENCE — a WIDENING that fixes a live over-rejection:** enum ctors today REJECT the same sound shape (`E.Two(v[0], !v)` → `E_BorrowConflict`, measured) because `DotShorthand` routes through the CALL-shaped check (bare = borrow) at a BOUNDARY position (bare = consume). That is a defect; it is fixed by this ruling. No fixture ratified it — `place_overlap_bare_move_error` pins only the FREE-FN shape — and no currently-ACCEPTED program changes. **WHY THE ANSWER IS FORCED:** one structural chokepoint means ONE predicate for aggregate construction; it cannot treat struct/tuple one way and enum another, so the ratified boundary model wins and the enum fix falls out. **CORE-#8 PRECISION (recorded because it was misapplied first):** `f(v[0], !v)` and `Pair(v[0], !v)` are **NOT two spellings of one program** — a call's bare arg is a BORROW, a ctor's bare arg is a CONSUMING BOUNDARY, by ratified design. Core #8 forbids excusing a known DEFECT by cross-implementation agreement; it does not forbid two positions having different ratified semantics. **⚠⚠ AMENDMENT, SAME DAY (2026-07-25) — THE EXEMPTION IS NARROWER THAN FIRST RATIFIED; the rationale above is REFUTED BY MEASUREMENT.** Brief-review pass 2 disproved the "clones-before-the-callee-runs" reasoning: the boundary clone is emitted at the `struct_init`/`tuple_init`, i.e. **AFTER every sibling arg has been evaluated**. GIR, measured:
> `_8 = index_load _1, 0` (raw ptr into `v`) … `_11 = borrow_mut _1` … `_12 = call @mutate(_11)` **← reallocs `v`, `_8` now dangles** … `_13 = call @gorget_string_clone_to_owned(_8)` **← the boundary clone reads FREED memory** … `_14 = struct_init Pair { _13, _12 }`
So `Pair(v[0], mutate(&v))` and its tuple twin are **ACCEPTED at HEAD and heap-use-after-free** (independently reproduced by the orchestrator: `gg check` OK, ASan `heap-use-after-free … in gorget_string_clone_to_owned`). **An owned-field ctor arg IS a live alias across sibling evaluation.**
**THE CORRECTED RULE — exempt on the MOVER axis only:** at an aggregate init, an owned-field position is exempt from participating against a sibling **mover** (`!v`), because a move transfers a pointer and cannot invalidate a sibling read — that is why `Pair(v[0], !v)` measured safe and why the enum widening below is still correct. It **STILL PARTICIPATES against a sibling WRITER** (`&v`, a mutating method-receiver, or a writer nested inside a sibling arg), because a writer CAN realloc before the boundary clone runs. `Pair(v[0], mutate(&v))` therefore **REJECTS**. Everything else in this addendum stands: the per-position typing (owned / `Ref`-field / `!`), the enum-ctor widening, the Core-#8 precision, and the conclusion that a blanket *borrow-position* model would over-reject.
**LESSON (the second one this entry has earned):** "this position already clones" is NOT sufficient to prove a value is not a live alias — **WHEN the clone is emitted relative to sibling evaluation is the load-bearing fact**, and it is only visible in the GIR. Reason about emission ORDER, not merely emission.

**⚖ RIDER RATIFIED (owner 2026-07-25) — D10 CONFLICTS ON ABILITY-TO-WRITE, NOT ON SIGIL.** D10's base text reads *"at most one writer (`&`) or **mover** (`!`) during the borrow's live range"*. Read literally that rejects `Pair(v[0], !v)` — reader × mover — which this addendum ratifies as ACCEPTED. The clause is too COARSE, and the refinement is: **a path conflicts when it can WRITE the overlapping storage during the intersection of the two live ranges — not when it merely carries a `&` or `!` sigil.** A **move transfers a pointer without touching the buffer**, so at an **INLINE** site (struct / tuple / array / dict / enum-variant literal, where no body runs between the sibling evaluations) a mover cannot damage a live reader → no conflict. At an **OPAQUE** site (any call) the same `!v` DOES conflict, because the callee owns the moved-in value and may mutate or drop it while the reader is still live — which is why the ratified NEG `f(v[0], !v)` (`place_overlap_bare_move_error`) stands unchanged. **The same sigil, two verdicts, derived from one question — "can this path write that storage while the other is live?"** — rather than from two carve-outs. Corollary: `f(!x, !x)` (mover × mover) is NOT this rule's business at all; neither path writes the buffer, and it is correctly `E_DoubleMove`'s on the LIVENESS axis. Corollary 2: overlap remains required — `f(&m.a, &m.b)` is two writers on DISJOINT sub-places and stays legal. **PROVENANCE NOTE (owner-corrected 2026-07-25):** an earlier framing of this round claimed D10 "needed a second clause" because a live-range-only prototype let `f(&v, &v)` through. That was wrong: D10's exclusivity clause already covers writer × writer and always did — the prototype had REPLACED the existing check instead of extending it, and dropped the case on the way out. **D10 stands as ratified; what was defective was the IMPLEMENTATION'S REACH** — it under-approximated "access path" (missing get-chains, `static` roots, `a = w` aliases, struct/tuple literals, operator operands, and writers nested one call deep) and "live range" (comparing only top-level args of a single call). Extend the reach; do not swap out the rule.

**HISTORY, recorded so it is not re-derived wrong:** the Track-A brief v1 had this INVERTED — it read the accept/reject asymmetry as the bug and planned a NEG fixture for `Pair(v[0], !v)`, which would have shipped a new over-rejection. Brief-review pass 1 refuted it with end-to-end measurements. The lesson generalizes: **before treating an accept/reject asymmetry as a defect, ask whether the two positions have different ratified SEMANTICS.**

- 2026-07-26 — **D32 RATIFIED (owner): `&` IS A BOUNDARY MODIFIER, NOT AN EXPRESSION OPERATOR. `Expr::MutableBorrow` LEAVES THE AST.** The question was forced by a round in which ten sequential review passes kept enumerating new *positions* where a stray `&` misbehaves — the classic wrong-layer signal (CLAUDE.md's debugging heuristic: complexity at the read site means a writer upstream lost an invariant). The owner asked the right question of a filed defect — *"what is `int y = &c.fd + 1` supposed to express? `&` is for boundaries; I don't see the meaning"* — and the parser answers it.

  **THE STRUCTURAL FACT (measured, `gg parse`).** `&` reaches the AST down exactly one of two disjoint paths. At a call argument, `parse_call_arg` (`src/parser/expr.rs:2013`) calls `parse_ownership_modifier` (`src/parser/mod.rs:236`) FIRST, which CONSUMES the `&` and returns `Ownership::MutableBorrow` into the typed field `CallArg.ownership` (`src/parser/ast.rs:846`) — **no `Expr::MutableBorrow` node is produced.** The for-loop iterable behaves the same way (`for_loops.rs:116`: the `&` "arrives here, NOT as `Expr::MutableBorrow`"). Everywhere else, `&` falls through to the general prefix operator (`Token::Ampersand => parse_expr_bp(33)`, `expr.rs:612`) and becomes `Expr::MutableBorrow`. Confirmed on one file: `bump(&c.fd)` → `ownership: MutableBorrow`, no node; `int y = 1 + &c.fd` → `node: MutableBorrow { … }`.

  **THEREFORE — the load-bearing observation, and the reason this is a deletion rather than a check:** `Expr::MutableBorrow` is **not a parallel representation of a legitimate fact.** Its domain is **exactly the set of positions where `&` has no write-through semantics.** Every node of that type in the AST is, by construction, a `&` the language has no meaning for. (An earlier framing in this round called these "two sources of truth for one axis" — that was wrong and weaker; they are DISJOINT by construction.)

  **THE RULE.** `&` is legal only where it modifies a boundary: a **call argument**, a **for-loop iterable**, a **parameter declaration** (`int &x`), and the **`&self` receiver** in an `equip` block. ⚠ **CORRECTION (scout, 2026-07-26) — THAT WHITELIST WAS NOT TOTAL.** There are **eight** `parse_ownership_modifier`/inline consumers; two had no subject here: **function-TYPE parameters** (`src/parser/types.rs:201` — legal, unaffected, merely unnamed) and, materially, **LIST COMPREHENSIONS** (`parse_list_comprehension`, `src/parser/expr.rs:1754`) — a genuine FIFTH expression-level position, which needs its own ruling (rider below). The last two are declaration syntax, not expression nodes, so at the EXPRESSION level the whitelist is: call arg, for-loop iterable. Everywhere else `&` is **rejected**. `int y = &c.fd + 1` becomes a parse error, which is what it always should have been.

  **WHY A WHITELIST AND NOT A LIST OF BAD POSITIONS.** The pre-existing ruling ("REJECT the value-position `&`", owner, during the paused CoW-aliasing round) enumerated a LIST — `[&a]`, `{k: &a}`, tuple-destructure, default-param-value, `return &v`. That framing is precisely WHY the operand family (match scrutinee, binary/comparison operands, index expression, f-string interpolation, closure expression body) was found by review pass 9 to have **no subject in any filed census** — a list can always miss a member, and it missed at least five. **Inverting to a whitelist closes the enumeration by construction** (Core #15e Q3/Q4 answered structurally rather than by vigilance). D32 SUBSUMES that earlier ruling; it does not conflict with it.

  **⚠ CORRECTION (scout, 2026-07-26) — THE "NORMALIZATION WRINKLE" THIS ENTRY PRESCRIBED DOES NOT EXIST. Do not implement it.** The entry originally claimed `&(a.enumerate())` produces BOTH the ownership field and a node, and asked for a parse-time normalization. **Measured: it produces ZERO nodes**, and `for x in (&a)` is *already* a parse error. The two four-arm matches in `stmts/for_loops.rs` collapse to `matches!(ownership, MutableBorrow)` — their `ST_C`/`EN_C` arms are unreachable by any spelling. (Also found there: `for_loops.rs:183-184` is a FALSE invariant comment (Core #14), and `for x in & &a` compiles silently today.)

  **WHAT THIS RETIRES.** ⚠ **CORRECTION (scout, 2026-07-26): OVERSTATED — D32 retires 4 of the 7 owning-position costumes, not all 7.** `ctor` / `enum` / `push` spell `&` at a **call-argument** boundary, which produces zero nodes, so they are untouched and stay broken. With that correction, D32 retires: the **operand-position family** (silent wrong output on C, `llc` hard-fail on LLVM, and a tainted twin that DUPLICATES a user `Drop`); the **owning-position family** (7 committed costumes); the tainted-`Drop` duplication at every non-`CallArg` position — which also makes `reject_tainted_formation_arg`'s two-`CallArg`-site wiring sufficient **by construction** instead of by luck. And it makes the **standalone `&`-formation site in `exprs/mod.rs` DEAD BY CONSTRUCTION** — dissolving the "third omitted cell" that consumed three Track-C review passes, whose reachability the orchestrator got wrong twice.

  **⚠ WHAT THIS DOES *NOT* FIX, stated so it is not assumed:** the by-value write-through defect at the LEGAL position — `f(&c.fd)` silently dropping the callee's write (measured 2 OK / 18 WRONG) — is untouched. That is Track C's defect and still needs Track C's fix; D32 only shrinks it to ONE formation site. Track B2's clone/move chokepoint work is orthogonal. **D32 kills the whack-a-mole surface, not the round's actual bug.**

  **⚖ RIDER RATIFIED (owner 2026-07-26) — LIST COMPREHENSIONS ALIGN TO D33: ITERABLE-SIDE, and the dead field gets WIRED.** Was open; owner ruled option (a).
  **THE RULE:** `[e for x in &xs]` is the legal spelling and means what `for x in &xs` means — **the collection is opened for write-through, and writes reach the source**. The pre-`in` binding-side spelling (`[e for x & in xs]`) is **RETIRED**. One axis, one side, in the language's only construct that had drifted off it.
  **TWO DELIVERABLES, not one — and shipping only the first would be worse than shipping neither:**
  1. **Retire the pre-`in` spelling** (a parse-level removal ⇒ rides D32's gauntlet and its accept/reject surface).
  **⚖ SEMANTICS, RATIFIED (owner 2026-07-26) — `&xs` IS A PERMISSION, NOT A WRITE.** *"`[e for x in &xs]` does nothing by itself, but allows `[bump(&x) for x in &xs]` to work."* A comprehension BUILDS A NEW COLLECTION; it never mutates its source. So the iterable sigil has **no observable effect on its own** — its entire job is to make the BINDING a write-through place, so an inner `&` boundary in the element expression can reach the source. **This inverts the obvious test: `[x*2 for x in &xs]` and `[x*2 for x in xs]` MUST print the same thing** — that body does not mutate, so identical output is CORRECT, not evidence of a no-op. ⚠ **The orchestrator originally filed the no-op on exactly that non-mutating pair; it demonstrated nothing.** The discriminating probe needs a MUTATING body — measured: `int bump_ret(int &n): n = n + 100; return n` with `[bump_ret(&x) for x in &a]` gives `ra[0]=101` but **`a[0]=1`, the write is LOST**, and the bare `for x in a` form is **byte-identical**, confirming the sigil is inert. ⚠ **SUB-QUESTION NOT SETTLED HERE:** the bare form is ACCEPTED while silently mutating a materialized copy — whether that should warn (the dead-write diagnostic exists at other positions) or reject is left open.
  2. **MAKE `ListComprehension.ownership` REACH THE LOWERING.** ⚠⚠ **CORRECTION (brief-review pass 1, 2026-07-26): the earlier claim here — "dead today, read by nobody (all 10 destructuring sites use `..`)" — is FALSE.** `src/formatter/mod.rs:1933` destructures it and `:1939-1943` maps it to `""`/`"&"`/`"!"`. **`gg fmt` already NORMALIZES the pre-`in` spelling to the iterable side** — independent corroboration of this rider's direction, and a reason an executor must NOT delete the field. The genuine gap is that the field never reaches **`lower_list_comprehension`** (`src/ir/lowering/exprs/collections.rs:594`), a SEPARATE loop emitter from `lower_for_*` whose own comment records that comprehension element write-through is deferred.
  **⚖ RATIFIED (owner 2026-07-26) — TAKE THE PARSE-SITE MOVE; and the owner's framing behind it: *"for-loops and comprehensions are really the same thing."*** That framing is not a preference — **the self-host already implements it, deliberately and by name.** `tests/fixtures/self_host_lowerer/lower_loops.gg:80-92` documents `for_iterable_mode` as *"Shared by the statement-for path (`lower_for`) AND the list-comprehension site (`lower_list_comprehension`) so the `&`-strip lives in ONE place (**Core #4**) and the comprehension can never drift from the loop's handling."* **Rust gg is the lane that treats them as two things** — a reference-lags-the-self-host data point, i.e. a succession milestone, not an embarrassment.
  **WHY THE MOVE IS THE ONLY CLEAN IMPLEMENTATION (the exemption is MECHANICAL, not positional).** D32's whitelist is defined by the *mechanism* that absorbs the `&` — `parse_ownership_modifier` consuming it into a typed field, producing NO node — while the reject is implemented at the general prefix operator. Measured today: `for x in &a` → **0 nodes / 1 field**; `[x*2 for x in &a]` → **1 node / 0 fields**. So the comprehension iterable is a whitelisted position that does **not use the whitelisted route**. After the move, **"produces `Expr::MutableBorrow`" and "is illegal" become EXACTLY EQUIVALENT** — no exemption list inside the reject, no context-sensitivity; the parser's own structure encodes the whitelist. The alternative (a context-sensitive reject that spares comprehensions) keeps the node D32 deletes and reintroduces the position-by-position special-casing D32 exists to remove.
  **CONSEQUENCE FOR DELIVERABLE 2 — this likely DISSOLVES the three sub-decisions rather than answering them.** Instead of inventing an element-binding shape, a sever policy, and a read path for `lower_list_comprehension`, **route the comprehension through the same helper the statement-for path uses**, which is what the self-host does. ⚠ Note the self-host still passes `write_through=false` for comprehensions (its own deferred write facet) — so it shares the READ path today, and the WRITE facet is the new work on both lanes.
  ⚠ **SELF-HOST COST IS LIKELY HIGHER THAN THE SCOUT ESTIMATED.** The scout scoped the SH lane as three sites (`parser.gg:2266`, `ast.gg:80`, `format.gg:379`). But the SH parser *also* wraps a `&`-iterable in `EMutableBorrow` (`parse_prefix`) for **both** for-loops and comprehensions, and `for_iterable_mode` is built around stripping that node. Removing the node changes that helper too. **Re-scope the SH lane before launch.**

  3. ⚠ **MOVE THE PARSE SITE — RATIFIED ABOVE. Without it, deliverables 1 and 2 are MUTUALLY UNSATISFIABLE.** `parse_list_comprehension` calls `parse_ownership_modifier` **BEFORE** `in` (`src/parser/expr.rs:1754`); `parse_for_stmt` calls it **AFTER** (`src/parser/stmt.rs:287`). Measured: `[x*2 for x & in a]` → 0 nodes + `ownership: MutableBorrow`; `[x*2 for x in &a]` → **1 node**. So retiring the pre-`in` spelling alone deletes the ONLY route to a non-`Borrow` ownership, and D32's blanket prefix-`&` reject then makes the ratified spelling a PARSE ERROR. **Move the call to after `expect_keyword(Keyword::In)`, mirroring `stmt.rs:287`.**
  4. ⚠ **A `gg fmt` ROUND-TRIP FIXTURE IS OWED.** Because fmt rewrites the field spelling into the node spelling, D32-without-the-move makes the formatter emit a program the parser rejects — exactly what `assert_fmt_round_trips` exists to catch, and **no existing fmt test covers a comprehension `&`**.
  **WHY THIS COMPOSES:** the inner and outer sigils are different boundaries and both stay legal — `[bump(&x) for x in &xs]` has `&x` at a CALL-ARG boundary (ownership field, no node) inside a comprehension whose `&xs` opens the collection. Today the outer sigil is inert, so that inner write lands in a materialized copy — a dead write. Wiring the field is what makes the composition mean what it reads as.
  **GROUNDING:** D33's `!`-sibling argument applies unchanged — `[e for x in !xs]` would consume the COLLECTION, so the sigil belongs on the iterable in comprehensions for the same structural reason it does in for-loops.

  **⚠ SUPERSEDED — the open framing, kept for provenance: LIST COMPREHENSIONS CONTRADICTED D33.** The scout found a fifth expression-level position with two problems, both measured:
  1. **The `&` there is a NO-OP today — a live Core #10 silent drop.** `[x*2 for x in xs]` and `[x*2 for x in &xs]` both print the same thing (`6`); `ListComprehension.ownership` is parsed and then read by **nobody** (all 10 destructuring sites use `..`). The user writes a sigil, the compiler accepts it, and it means nothing.
  2. **Its sigil sits on the OPPOSITE SIDE from the for-loop.** `[e for x & in xs]` puts the `&` in the ownership field (pre-`in`, binding-side); **`[e for x in &xs]` — the spelling that mirrors the ratified `for x in &b` — produces a NODE instead.** So the comprehension is the language's only live `for &a in b` precedent, and it points the other way from D33.
  **This must be settled before D32 lands**, because D32 decides whether `[e for x in &xs]`'s node is legal or rejected — and answering that silently would settle the placement question D33 just ruled on. Options: (a) align comprehensions to D33 (iterable-side `in &xs`, retire the pre-`in` spelling), (b) keep both spellings and make the field actually mean something, (c) reject `&` in comprehensions entirely until the semantics are defined. **Recommendation: (a)** — one axis, one side, consistent with D33's `!`-sibling argument; and the no-op field is a bug to close either way.

  **⚖ THE UNIVERSAL STATEMENT (owner-requested 2026-07-26) — the prose D32 needs, because a LIST cannot generate an answer for a position nobody enumerated.** Every failure in this campaign came from reasoning position-by-position: the value-position ruling listed five and missed the operand family; the D32 whitelist listed four and missed two; the node-deletion has hit FOUR separate meaning-deriving paths. The fix is a rule that GENERATES the whitelist:

  > **The sigils describe what the OTHER SIDE of a boundary may do to a value: bare = READ it · `&` = WRITE to it · `!` = CONSUME it. They are not operators. They mark a POSITION, and they appear at BOTH ENDS of a boundary — on the declaration that will act, and on the argument that grants it.**
  >
  > | position | end | who acts |
  > |---|---|---|
  > | `void f(int &x)` | declaration | the function writes into the caller's value |
  > | `f(&x)` | grant | the callee writes |
  > | `equip T: void m(&self)` | declaration | the method writes into the receiver |
  > | `for x in &xs` | grant | the loop body writes |
  > | `[e for x in &xs]` | grant | the element expression writes |
  > | `Callable[void(int &)]` | declaration (type) | a function of this type writes | *(spelling superseded by D35; this entry predates it)* |
  >
  > **Where there is no boundary — no callee, no loop body, no caller — a sigil grants nothing and is REJECTED.** `int y = &a + 1` is rejected because `+` only READS its operands: there is no other side that could write.

  **⚖ "BOUNDARY" DEFINED (owner-confirmed 2026-07-26) — a value CROSSES FROM ONE SCOPE INTO ANOTHER, and the sigil is written at the crossing.** Raised as a blocking circularity by the prose review (*"legal where there's a boundary" + boundary = "where it's legal"*). The scope-crossing test is **non-circular and decides all nine measured positions**: call arg ✓ · for-loop iterable ✓ · comprehension iterable ✓ · param decl ✓ · `case Some(&p)` ✗ (nothing crosses — `p` names part of a value already in scope) · `do: &a` ✗ · `String w = &v` ✗ · `&a + 1` ✗ · `f((&x))` ✗ (the crossing point is the argument; parenthesising evaluates the sigil before the crossing). ⚠ **It also re-derives the resting-position rejection as a CONSEQUENCE, not a rule** — `String w = &v` fails because NOTHING CROSSED, not because borrows are special in a name.
  **⚠⚠ CLOSURES: THE MODEL RE-DERIVED A DECISION ALREADY RATIFIED — see D7 (capture-syntax package, completing D5).** The boundary model predicts the sigil belongs AT THE CAPTURE; **D7 ratified exactly that**: per-variable capture lists `(&count)(): …` / `(!name, &total)(x): …`, bare names REJECTED in a capture list, `!():` move-all sugar, and **`&`-capture exclusivity LIVENESS-based** (the borrow ends at the closure's last use). Independent derivation reaching a ratified shape is the strongest corroboration this model has. ⚠ **NOT IMPLEMENTED — `(&count)():` is a parse error at HEAD**; the grammar work (two-group lookahead) belongs to the D5 implementation track, and D7 already records production's scope-based `&`-capture exclusivity as a **filed conformance gap** against the ratified liveness-based rule. ⚠ **An earlier draft of this entry wrote "Gorget chose INFERENCE over spelling" into the language reference. That was FALSE — it described the unimplemented present as if it were the design.** Inference-from-the-body is the current implementation, and it is why an in-body sigil is inert: it sits after the crossing.
  **THE MEASURED PRESENT (kept, because it is what an implementer meets):** Measured: there is **no capture-list syntax**; a closure that assigns a captured local captures it **mutably** and the exclusivity rule then rejects an outside read (`E_ReadWhileMutCaptured`); and a sigil written INSIDE the body (`bumpi(&a)`) **prints 10 where 11 is correct — silently inert**. So the crossing is at CAPTURE, the mode is inferred there, there is nowhere to spell a sigil, and an in-body sigil sits on the wrong side of the crossing. ⚠ **That in-body inertness is a filed defect, not a design choice** — two spellings of one intent disagree (assignment triggers mutable capture; `&`-through-a-call does nothing).

  **WHY THIS IS THE RIGHT SHAPE:** it derives every legal position from one question (*"is there another side, and may it write?"*), so a position nobody has enumerated still gets the right answer — match scrutinee, index expression, f-string interpolation all reject without needing a row. It also explains the comprehension exactly as ratified: `&xs` grants the permission and does nothing by itself; the inner `&x` in `[bump(&x) for x in &xs]` is a SECOND, NESTED boundary that uses it.
  **⚖ EXTENDED TO `!` (owner 2026-07-26: *"we should cover `!` too — don't the same rules apply?"*, and *"bare, `&` and `!` are the crux of gorget; this needs to be clear as water"*). MEASURED FIRST, and the answer is YES at operand positions, NO at resting positions — one rule meeting two kinds of result.**
  - **OPERAND positions — SYMMETRIC, both reject.** An operator only READS its operands. Measured: `s + "b"` and `!s + "b"` are **byte-identical, 0 clones both ways** (`--clones=stats`), so `!` there has no effect on the RESULT. ⚠ **CORRECTION (2026-07-26, gauntlet pass 14): that measurement does NOT support the stronger claim this entry originally made — "accepted and completely INERT, the same silent-no-op class as the comprehension `&`".** The operand `!` still **moves the source**: `String r = !s + "b"` followed by `print(s)` is `E_UseAfterMove` (and likewise `if !s == "a":`), while the `&` control leaves `s` live. So it is result-inert and source-MOVING, which is an observable accept/reject effect and NOT the comprehension `&`'s class (that one is a true no-op: `[bump_ret(&x) for x in &a]` leaves `a[0]=1`). **A clone-count instrument is structurally blind to a liveness effect — Core #13, pick an instrument that can SEE the failure class.** The reject-surface probe sees it; `--clones=stats` never could. ⚠ The RULING (both operand positions reject) is unaffected and is not re-litigated; only its supporting evidence is corrected. And `&a + 1` is accepted by `gg check` then **BUILD-FAILS** on C while **llc hard-fails** on LLVM. Neither sigil has anyone to grant anything to; both reject.
  - **RESTING positions — ASYMMETRIC, and the reason is principled, not a carve-out.** Measured: `String w = !v` → **OK**; `String w = &v` → **`error[E_LocalBorrowBind]`**. A move produces an **OWNED VALUE**, so it can rest wherever a value can. A borrow produces an **ALIAS**, which would need a **lifetime** to bound its validity — and Gorget has none, by design (the ratified no-stored-borrows rule). **One rule; two kinds of result.**
  ⚠ **CONSEQUENCE — a NEW accept/reject change beyond D32's scope:** `!` in an operand position is accepted-and-inert today and must become a reject. It rides D32's gauntlet and its lane census, and it needs its own NEG fixtures. ⚠ **An earlier draft of this entry scoped the rule to `&` only, on the reasoning that an operator "does consume" its operands. That reasoning was not supported by the clone measurement above, which shows only that `+` does not CLONE; the source is still moved (see the correction above), so `+` reads its operands while the `!` consumes the binding.**

  **⚖ ESCAPE-SAFETY IS A SEPARATE CONCERN (owner 2026-07-26: *"I accept that escape-safety is a separate concern"*).** Three successive attempts to make ONE sentence generate every answer were each refuted by measurement — the third added an "the owner outlives that scope" clause which gauntlet pass 2 killed with **five counterexamples**: an escaping `&`-capture (the doc contradicted itself two paragraphs apart — *"a closure CAN outlive the scope that made it"*), `spawn` (a spawned callee does not finish first, by design), `return &v` of a **`&`-parameter** (the referent is NOT gone — it lives in the caller), and the receiver cases. ⚠ **AND THE EXCLUSIVITY RATIONALE COULD NOT EXPLAIN THE CASE IT WAS ADDED FOR:** in the escaping-capture bug there is exactly **ONE** writable path once the owner is gone, so the one-writer rule is SATISFIED and the program still dangles; the compiler's own diagnostic there is `E_DanglingReturn`, a **lifetime** message.
  **THE RATIFIED SHAPE:** the sigil vocabulary says what may happen to a value **across one boundary**, and says **nothing** about how long a borrow stays valid once it has crossed. Escape-safety is governed by its own rules with their own reasons — no returned or field-stored borrows · no local `&`-binding (exclusivity) · a closure holds a reference capture exclusively while live. **Gorget has no lifetimes by design, so it constrains where a borrow may TRAVEL rather than tracking how long it LIVES, and that constraint is not derivable from the table.** ⚠ **LESSON: the table survived every pass; every attempt to extend one rule over escape-safety failed. Stating the uniform part as the rule and NAMING the separate concern is the honest shape — a fourth unification attempt would have been the same error faster.**
  ⚠ **OPEN, owner-raised 2026-07-26, NOT decided: *"I don't think `return !f` makes sense to support."*** Returning a closure that captured a local is the shape behind the escaping-capture memory bug. Options span: reject escaping closures that hold a `&`-capture (implementable without lifetimes) · require by-value/`!` captures for any closure that escapes · keep it and enforce via a real escape check. **Not folded into anything; decide it as its own question.**

  **SCOPE:** accept/reject surface change ⇒ full gauntlet, all three lanes (Rust gg C+LLVM · ggdef · self-host), NEG fixture per rejected position, doc write-through (`language-reference.md` §9.3, `language-design.md`). Own scout → brief → ≥3 fresh reviews → executor → output-review.

- 2026-07-26 — **D33 RATIFIED (owner): THE FOR-LOOP SIGIL STAYS ON THE ITERABLE (`for a in &b`). Question raised, researched, and CLOSED the same day.** Raised while ratifying D32 (*"whether `&` should live at the iterator or the iteratee"* — `for &a in b` · `for a in &b` · `for &a in &b`), deferred, then resolved once the design record was actually consulted. Owner: *"keep the for-loop syntax as is."*

  **THE QUESTION'S PREMISE INVERTS, AND THAT IS THE WHOLE ANSWER.** The framing was *"what led us to REQUIRE `&` instead of always considering it a borrow?"* — but **it already IS always a borrow.** `for x in v` is an **immutable** borrow. `&` does not add borrowing; it selects among **three iteration modes**, which are the same bare/`&`/`!` vocabulary the language uses at every other ownership boundary (`language-design.md:1245-1256`, all three verified working):
  > `for item in collection:` — immutable borrow, the default · `for item in &collection:` — mutable borrow, writes reach the source · `for item in !collection:` — consuming, takes ownership of each item

  So the for-loop is not a construct with an extra sigil bolted on; it is an ordinary instance of the uniform ownership axis.

  **WHY A BARE BINDING CANNOT SIMPLY WRITE THROUGH** (`language-reference.md:2470-2478`): mutation through a bare binding **MATERIALIZES a private copy** rather than erroring — Gorget's deliberate divergence from Rust, which rejects it. Given that, if `for x in v` wrote through, the for-loop would become **the ONLY position in the language where a bare binding reaches its source**, and read-only iteration would have no spelling left.

  **WHY THE ITERABLE SIDE, AND NOT THE BINDING — the `!` sibling is the decisive argument.** `for item in !collection` consumes the **COLLECTION**; nobody would write `for !item in collection`. `&` sits on that same axis, so binding-side placement would **split one uniform ownership axis across two syntactic positions**, in the one construct that expresses all three modes identically. Supporting, not load-bearing: `&` marks the storage being EXPOSED everywhere else (`f(&x)`), i.e. the CAUSE rather than the consequence; binding-side would be ambiguous about the root (a bare root under CoW materializes, so `for &x in b` would be a dead write) and would collapse into needing BOTH sigils — which D31's own rationale rules out, since double-spelling exists to make an INVISIBLE signature contract visible and a for-loop's binding and iterable sit on the same line. Rust independently uses iterable-side (`for x in &mut v`) — corroboration, not authority.

  **MEASURED:** `for &x in v` and `for &x in &v` are **parse errors today** (`expected pattern, found '&'`), so the alternatives were grammar ADDITIONS, not re-spellings — a higher bar than "reads slightly better".

  **⚠ ONE GENUINE DEFECT SURVIVES THIS RULING, and it is orthogonal to placement: the ADAPTER AMBIGUITY.** In `for i, x in &a.enumerate()` it is unclear whether `&` binds to the call or the receiver, which is why `src/ir/lowering/stmts/for_loops.rs:156-160` carries a four-arm match to strip every shape. This is a grammar defect about what `&` BINDS TO — it would afflict `!a.enumerate()` identically — not evidence the sigil is on the wrong side. **FALSIFIABLE TEST, recorded so this can be re-opened on evidence rather than taste: after D32 normalizes the parenthesized `&` into the ownership field, that four-arm match should COLLAPSE TO ONE. If it does not, the ambiguity is deeper than this ruling assumes and binding-side placement deserves reconsideration.**

  **⚠ NOT A SYNTAX ISSUE — a live bug in the same area, recorded so the two are never conflated:** `for x in &v` on a **SCALAR element** LOSES the write (measured: prints `1`, want `101`; the filed `sound_for_amp_scalar_elem_writethrough` gap). That is the same projected-TYPE axis as the `&`-write-through Family-1 defect. Changing the placement would not have fixed it, and fixing the mechanism does not settle the placement.

- 2026-07-26 — **D34 RATIFIED (owner): THE CLOSURE-CAPTURE BOUNDARY MOVES FROM CAPTURE-TIME TO ESCAPE-TIME — as the intended mechanism. TODAY'S CAPTURE-TIME BEHAVIOUR STANDS UNCHANGED until the analysis exists.**

  **HOW THE QUESTION AROSE, because the framing is the valuable part.** The owner asked why closures capture by value when the language's universal rule is *"everything is a borrow until mutated"*, and proposed that captures should honour it like every other position. Checking the record turned that the other way: the ratified CoW contract **already lists closure captures as an ownership boundary**, in the same breath as collection puts, constructor field init, and returns — *"the rule is uniform across all of them"*. So by-value capture is not an exception to borrow-until-mutated; it is that rule's **boundary clause**, at a position already classified as a boundary.

  **BUT THE INSTINCT SURVIVES ONE LEVEL DOWN, AND THAT IS THE RULING.** Ask why each of those is a boundary and the answer is identical: *the destination may outlive the source*. For a capture, **"may" is doing all the work**. Everywhere else CoW draws the boundary **precisely** — `clone-if-the-source-is-live, move-if-it-is-dead` is a LIVENESS test. For captures alone it draws it **conservatively, at capture time**, because it cannot see whether the closure escapes. **A closure that never escapes has no reason to own anything.**

  **THE INTENDED MECHANISM:** a closure that stays local **borrows its captures** — zero cost, and it observes current values rather than a snapshot. A closure that **escapes** (returned, stored, spawned) **materialises its captures at the escape**, which is an ownership boundary exactly like a return. That is CoW applied precisely instead of pessimistically, and it removes the one position where the model approximates.

  **⚖ D34 SUPERSEDES D5's BY-VALUE DEFAULT (owner 2026-07-26, on being shown the conflict).** Owner: *"I value a simple mental model and would expect closure captures to honor the same sigils as everyone else."* So a bare capture is an **immutable borrow**, exactly as at every other position — bare reads, `&` writes through, `!` takes — and the capture stops being the one place with its own rule.
  ⚠ **PROCESS FAILURE, recorded because it is the recurring one:** D34 was drafted **without grepping for the existing ratified capture rule**, so it silently contradicted D5 (*"value as-of closure creation"*) and the conflict was found by the owner reading a week of git log, not by the drafter. "Consult history before proposing a design" exists for exactly this, and was skipped on a design decision.
  **THE ARGUMENT THE RULING DID NOT NEED BUT HAS:** the standard objection to borrow-capture is the Python/JS loop-variable trap — `for i in xs: handlers.push((): print(i))` printing the last `i` from every closure. **Under D34 that trap cannot form**, because pushing the closure into `handlers` IS an escape, so its captures materialise at that moment, per iteration. The escape rule does not merely make borrow-capture sound; it makes it correct in the case usually cited against it.
  **WHAT CHANGES OBSERVABLY:** a non-escaping closure now sees current values — `int n = 1; auto f = (): print(n); n = 2; f()` prints **2** under D34 and **1** under D5. Docs must be updated **from** the by-value description (which correctly described D5 and the current compiler) **to** borrow-by-default with the gap to the implementation filed.

  **⚠⚠ COST CORRECTION (2026-07-26, measured after ratification — the entry below overstated it).** This entry said the cost is *"escape analysis — not lifetimes, but a real analysis that does not exist"*. **That is FALSE: it exists and is correct at the return position.** `check_expr_for_escaping_closures` (`src/semantic/safety/helpers.rs:1766-1795`) walks the returned expression, finds closures with captures, tests whether the captured def is a **local Variable and not a param**, and emits `E_ClosureEscapesScope` — *"cannot return closure `f`: captures local variable `n` which will be dropped"*. **What is missing is not the analysis; it is the RESPONSE.**
  **AND THE PRESENT BEHAVIOUR IS WRONG IN BOTH DIRECTIONS — measured, same program, only the capture body differs:**

  | | `return f` | `return !f` |
  |---|---|---|
  | **read-only** capture | **REJECTED** `E_ClosureEscapesScope` | runs, prints `42` — **safe** |
  | **mutating** capture | rejected | **ACCEPTED → garbage address** |

  So the check **over-rejects** an escape that is provably safe (a read-only capture is by value, so the closure is self-contained — the `!` spelling demonstrates it), and **`!` bypasses it** for the mutating escape that genuinely dangles. `!` is being read as *"I know what I'm doing"*, but moving a closure does not move a POINTER capture; the pointer still aims at the dead frame.
  **D34 RESOLVES BOTH, and makes the check unnecessary rather than smarter:** materialise the captures **at the escape**. A read-only capture is already a value, so it is allowed. A mutating capture becomes a value, so it is allowed too — and the mutation lands on the closure's own state, which is correct because the scope it pointed into is gone. **The work is a MATERIALIZE at the escape point, not a new analysis.**

  **⚠ NOT A BEHAVIOUR CHANGE TODAY. Capture-time materialisation STANDS**; the docs describe it as current and must not describe escape-time as though it were live. **COST: escape analysis** — not lifetimes. ⚠ SEE THE COST CORRECTION ABOVE: this clause originally read "a real analysis that does not exist", which measurement refuted — it exists and is correct at the return position. **INTERACTION: this lands directly on the open `return !f` question** — under escape-time semantics, returning a closure IS the escape that forces materialisation, which either gives `return !f` a well-defined meaning or identifies it as the thing to reject. Decide them together.

  **⚖ SEPARABLE AND ADOPTED NOW (owner, same exchange): a by-value SNAPSHOT is spelled as a closure PARAMETER, not as a capture.** If a closure needs a value fixed at a moment, pass it: `(String snap): ...`. Explicit, no inference, no analysis, and it is what parameters are for. This is the reliable spelling under *either* boundary rule, and the docs did not mention it.

- 2026-07-26 — **D35 RATIFIED (owner): AN UNNAMED PARAMETER'S SIGIL GOES AFTER THE TYPE — `Callable[void(int &)]`, NOT `Callable[void(&int)]`.** Raised by the owner as a preference ("a bit like C prototypes"); measurement turned it into a correction, because the current spelling contradicts the language's own stated rule.

  **THE INCONSISTENCY, stated twice in the same document twelve lines apart.** `AGENTS.md:113` and the reference both say the sigil goes **immediately before the argument NAME, not before the type** — `void modify(Message &msg)` ✓, `void modify(&Message msg)` ✗. Then `language-reference.md` says of the unnamed form: *"the sigil sits before the type"*, and requires `Callable[void(&int)]` — **precisely the shape the named rule rejects**. So the rule "the sigil marks the BINDING, not the type" was true for named parameters and inverted for unnamed ones, and no reader had cause to notice because the two statements never appear together.

  **THE RULE, now uniform.** The sigil occupies the NAME's position. When there is a name it precedes it (`int &x`); when there is none it stays in that slot with nothing after it (`int &`). Nothing about the sigil attaches to the type in either case. C's `void f(int *)` has the same shape, which is where the owner's intuition came from, but the argument here is internal consistency, not analogy.

  **MEASURED AT HEAD (2026-07-26), before the change — both sigils behave identically, so the change is symmetric:**
  | spelling | today |
  |---|---|
  | `Callable[void(&int)]` / `Callable[void(& int)]` | accepted |
  | `Callable[void(int &)]` / `Callable[void(int&)]` | **parse error** |
  | `Callable[void(!String)]` | accepted |
  | `Callable[void(String !)]` / `Callable[void(String!)]` | **parse error** |

  **RATIFIED SHAPE.** `Type &` and `Type !` are the ONLY unnamed-parameter forms. **`&Type` / `!Type` are RETIRED, not co-accepted** — two spellings for one concept is worse than either, and D32's thesis is closing the enumeration by construction rather than accumulating accepted forms. Whitespace-insensitive: `int &` and `int&` both parse, as `&int`/`& int` both do today.

  **RIDES D32.** This is a boundary-modifier position, so the parse must route through `parse_ownership_modifier` — the same mechanism D32 uses to define the legal-position whitelist by construction. Landing it as a separate special case would fight that design and re-open the position-by-position enumeration D32 exists to close.

  **⚠⚠ BUNDLED WITH THE `Callable`-FUNCTION-TYPE SEGFAULT, BY OWNER DECISION — the construct being re-spelled DOES NOT WORK.** Measured (gauntlet pass 13, parent-reproduced): `void g(Callable[void(&int)] cb): int a = 1; cb(&a); print(a)` → `gg check` OK, **exit 139 on C AND LLVM**, no output; the `Callable[void(int)]` control runs clean. This is the reference's only `&`-in-a-function-type example — the §9.1 position table's row and the spelling note's sole unnamed-parameter example — so the canonical legal spelling crashes. A syntax change whose only test shape segfaults cannot be validated on its own, so the fix and the re-spelling land in ONE round with one fixture set. Filed on the `Callable`-costume census in `TODO.md`; ⚠ CORRECTED 2026-07-26 (gauntlet pass 15): an earlier draft of this clause claimed the axis is the callable's PROVENANCE — "green through a Callable LOCAL, fatal through a Callable PARAMETER". That is FALSE and was mis-derived from a filing about a plain FUNCTION reference. Measured: `Callable[void(&int)] cb = bump; cb(&a)` through a LOCAL also exits 139 on both backends. Both provenances segfault, so PROVENANCE is not the axis. ⚠ CORRECTED AGAIN 2026-07-27: the same clause's follow-on *"there is no green cell here"* is ALSO false, and the green cells identify the real axis. Measured at HEAD, all `gg check`-clean: `Callable[void(&int)]` PARAM called `cb(a)` where `a` is an `int &` parameter → **11/11, correct**; `Callable[int(&Counter)]` PARAM called `f(&c)` where `c` is a `Counter &` parameter → **correct** (measured separately; ⚠ **UNPINNED — no in-tree fixture covers this cell**). ⚠ **The LIVE, GREEN in-tree fixture `tests/fixtures/callable_ref_param.gg` spells the call BARE — `return f(c)` at `:12`, NOT `f(&c)`** — so the pin it provides covers the bare-argument cell only. **The axis is whether the ARGUMENT's root is already a pointer.** The `__callable_N` arg loop in `src/ir/lowering/exprs/calls.rs` forwards a pointer only when `is_param_borrow_unique` holds of the argument local (i.e. the argument is itself a borrow parameter); for any other root it lowers the argument by value and hands an `int` to a callee expecting `int*`. So the fix is to drive that choice from the call-site sigil plus the callee's declared function-type parameter, and `callable_ref_param.gg` is the regression pin that must stay green through it.

  **LANE CENSUS (Core #9) — accept/reject surface, so all three lanes plus the formatter:**
  1. **Rust gg** — parser: accept `Type &`/`Type !` in a function-type parameter list via `parse_ownership_modifier`; reject `&Type`/`!Type` with a diagnostic that names the replacement.
  2. **ggdef** — same accept/reject within its subset; out-of-subset shapes noted explicitly.
  3. **Self-host** — the THIRD reject lane (the 2026-07-21 lesson: an accept/reject change's lane census must include it, not just Rust+ggdef).
  4. **`gg fmt`** — a round-trip fixture. The formatter rewrites sigil spellings, so a fmt that emits the retired form against a parser that rejects it is exactly what `assert_fmt_round_trips` exists to catch, and no existing fmt test covers an unnamed-parameter sigil.
  5. **Migration** — every in-tree `&Type`/`!Type` occurrence in a function type (`lib/`, `tests/fixtures/`, self-host sources, docs) moves in the same commit; the retirement is a hard break, disclosed, per the robust-API-over-compat ruling.

  **DOCS.** `language-reference.md`'s spelling note and §9.1 position table, `language-design.md`, and `AGENTS.md`'s quick-reference all state the named-vs-unnamed pair; the prose is folded to the ratified spelling immediately (this entry's own round), marked as specification until the parser lands.

- 2026-07-27 — **D36 RATIFIED (owner): SMART-POINTER METHOD AUTO-DEREF IS RECEIVER-ONLY (no `*boxed` operator), PER-FACE ON THE WRAPPER, AND ITS RESOLUTION IS WRITTEN INTO `method_resolutions`'S EXTENDED VALUE — NOT A PARALLEL CHANNEL.** Ratifies the shape §9.4 and §9.3's `guard.push(42)` teach, without importing Rust's `Deref` trait or its `*` operator. Answers all three of Track E2's previously-open design questions in one entry, unblocking E2's executor plan.

  **THE THREE QUESTIONS THIS SETTLES.**

  1. **§9.4 stands (Q3).** `docs/language-design.md:1841-1847`'s example — `Box[String]` → `boxed.len()` auto-derefs to `String.len()` — is the ratified shape. Adds ONE clarifying sentence: *"the coercion is scoped to method-call receivers; there is no `*boxed` operator."* §9.3's `guard.push(42)` gets pinned in the same paragraph as ratified per-face. The two alternatives (full Rust-style Deref including `*boxed`; deleting §9.4 entirely) were rejected as either importing Rust's type-system complexity without importing the type system that motivates it, or sacrificing the ergonomic shape §9.3 already teaches for concurrency.

  2. **GuardAccept scope: three-way per-face split (Q1).**
     - **Read face** (`.method()` with `&Inner` self): accept for **Guard + ReadGuard + WriteGuard + Box**. Uniform. Reading through any of them is sound; no invariant to break.
     - **Write face** (`.method()` with `&mut Inner` self): accept for **Guard + WriteGuard + Box**; **REJECT ReadGuard**. Mirrors Track D's `for_write` gate landed this round (`amp_object_base` in `tests/fixtures/self_host_lowerer/lower_expr.gg`) — one invariant (writes rejected through a shared-read view), two enforcement points. Without this, `read_guard.push(42)` compiles while `&read_guard.field` at a `push` argument gets rejected — an inconsistency users cannot rationalise.
     - **Consuming face** (`.method()` with `!Inner` self): accept for **Box only**; **REJECT Guard, WriteGuard, ReadGuard**. Consuming through a guard breaks the guard's Drop invariant — `!guard.into_inner()` moves the inner OUT, then the guard's Drop (`gorget_mutex_unlock`) releases the mutex, and the next `.lock()` returns a Guard over the moved-out slot. Box has no orphaned invariant (`Box.into_inner(!self)` transfers the inner AND drops the box's allocation together).
     - **`Shared` / `Mutex` / `RWLock` / `Weak`** (`NonDerefContainer`): reject for every face — users write `.lock()` / `.read()` / `.write()` / `.upgrade()` explicitly. Track E1 already ratified this reject (`d2a6e6c4` on branch `trackE1-fabrication-reject`).

  3. **Extend `method_resolutions`'s value type, do NOT add a parallel channel (Q2).** The scout proposed a new `method_call_auto_deref: FxHashMap<usize, DerefWrapperKind>` keyed on the same `method.span.start` as `method_resolutions`. Rejected under this ledger's standing directive of 2026-07-13 (*"all opportunities to reduce sidecars and parallel structures should be taken … when a single logical fact is split across index-aligned parallel vectors or a hand-synced side-table, normalize it into one cohesive record/typed field"*). Extend the map's value from `DefId` to `MethodResolution { def_id, auto_deref: Option<DerefWrapperKind> }`. `guard_of` (`src/ir/lowering/exprs/shared.rs`) stays useful for its lowering role (peel `Ptr`/`MutPtr`, project through `emit_guard_get_ptr`); the *decision* "did auto-deref fire, and against which wrapper kind" comes from the extended `method_resolutions` value — because that decision was made at the CHECKER when method resolution ran against the inner type (Layering rule 4: resolve once, write through). Cost: every current consumer of `method_resolutions` gets a `.def_id` field access instead of using the value directly. Mechanical.

  **WHY THIS IS THE REFERENCE-GRADE SHAPE, NOT RUST'S.**

  §3.7 ratified the trade already: "no user-visible borrowed-return type … the compiler handles everything internally" (`docs/language-design.md:747`). Receiver synthesis IS that — internal, transient, never surfaces. Rust needs `*boxed` because its type system exposes `&T` as a first-class user type; adding `*boxed` to Gorget imports the ergonomic shape but not the type-system machinery that keeps it honest, and lands users on a silent-clone-at-bind trap (CoW Rule 3 — views are transient, so a bind materializes; see `D41` and § "Articulation with #13" in `docs/internals/cow-transient-view-model.md`) that Rust's borrow checker would surface but Gorget's model would swallow.

  The ratified stack under D36 stays intact: no stored borrows (owner-ruled 2026-07-22) — receiver-only means there is nothing to store. CoW Rule 3 (views are transient) — the synthesized `&Inner` never outlives the call. D31 full-strict on consuming — `!` on the wrapper for the one legal consuming case (Box). No new type surface; the whole ratification reads a typed `DerefWrapperKind` that already discriminates the containers (per-face wrapper dispositions), so writes rejected through a ReadGuard reject uniformly at either entry point (`&rg.field` in an argument, `rg.push(42)` as a method call) with one invariant, no third asymmetry to explain.

- 2026-08-02 — **D37 RATIFIED (owner): CONTAINER ELEMENTS DROP IN FORWARD ORDER; LOCALS KEEP REVERSE-DECLARATION (LIFO). THE TWO RULES ARE DIFFERENT AXES, NOT A CONFLICT.** Raised as a blocking "owner-level design call" on an *asymmetry* between two fixtures; the asymmetry does not exist, and the ruling exists mainly to write down the second axis, which had never been specified.

  **THE PREMISE WAS FALSE — recorded because it cost a round.** The filed blocker (Round XXVI Track E → XXVII Track A, deferred twice) read: *"production has INSERTION-vs-REVERSE-LIFO asymmetry between similar fixtures."* The fixtures are not similar. `tests/fixtures/drop_collection_custom_elem.gg` declares **four separate locals** — `va`(10), `vb`(20), `dv`(30), `sk`(40) — each holding **exactly one** element; its expected `drop 40/30/20/10` (`tests/integration.rs:6893-6896`) is reverse *declaration* order across the four locals, and it says nothing whatsoever about element order because no container in it holds two elements. `drop_collection_custom_elem_leak.gg` declares **one** Vector with two elements pushed 1, 2; its expected `drop 1 / drop 2` (`tests/integration.rs:6933-6936`) is forward element order. Production is self-consistent on both.

  The escalation was manufactured by a bad citation: this ledger's own row-2 prescription (`TODO.md`) told the implementer to walk elements *"in reverse-decl / LIFO order (mirror production, e.g. `drop_collection_custom_elem` expected `drop 40/30/20/10` for push order 10/20/30/40)"* — but 10/20/30/40 were never pushed into one container. The XXVII scout built the prototype from that sentence, produced `drop 2 / drop 1`, saw it contradict the other fixture, and escalated the contradiction as a language question. **Core #15e Q1 in its purest form** (*"is this asymmetry a DEFECT, or two positions with different RATIFIED semantics?"*), compounded by Q4 (the rule's SUBJECT — "elements of a container" — never covered the four-locals fixture at all).

  **THE RULE.** Locals at scope exit: **reverse declaration order**, unchanged, already specified (`docs/language-reference.md`, Drop section). Elements within one container: **forward order** — index order for `Vector`/`Deque`, insertion order for `Dict`/`Set` and their unordered counterparts. **Why forward is forced rather than chosen:** the existing LIFO rule carries its own rationale — *"values declared later, which may reference earlier values, are cleaned up first"* — and that dependency argument has no purchase on container elements, which are peers and cannot reference each other. With no dependency to unwind, forward order is the reading with nothing arbitrary in it. Matches Rust's `Vec`, and matches what both backends already emit. The two rules compose: a block holding two vectors drops the second vector first, and each vector's elements front to back.

  **CONSEQUENCE — row 2 is unblocked and mechanical.** ggdef's `drop_value_transitively` helper walks elements FORWARD. ⚠ **The per-container-kind order for struct fields / enum payload / dict entries / tuple fields is NOT ruled here** — those cells were never measured, and the filed entry's blanket "reverse-decl / LIFO" is exactly the unverified premise this entry retires. They are a MEASUREMENT task against production, not a design call; only the Vector cell was verified when this was written.

- 2026-08-02 — **D38 RATIFIED (owner): `Set`:`HashSet` MIRRORS `Dict`:`HashMap` — `Set` IS INSERTION-ORDERED. POSITIONAL INDEXING (`s[i]`) IS REJECTED ON BOTH, ON AMBIGUITY GROUNDS, NOT ORDERING GROUNDS.** Owner framing: *"we want to keep the same semantics for Set/HashSet as the ones for Map/HashMap."* Confirmed — and the symmetry already holds at every layer, so the ordering half of this entry ratifies the status quo and deletes a contradicting doc sentence rather than changing behaviour.

  **THE SYMMETRY IS REAL AND ALREADY IMPLEMENTED.** `Dict` ordered / `HashMap` unordered (`docs/language-reference.md`, Collections); `Set` ordered / `HashSet` unordered. The C runtime carries the same split explicitly — `src/backend/c/runtime/runtime_set.c:12` reads `// Ordered Set: preserves insertion order (like Dict vs HashMap)`, with `gorget_ordered_set_new` delegating to `gorget_dict_new` while plain `gorget_set_new` delegates to `gorget_map_new`, and the clone path copying an `order` array only for the ordered variant. The lowering makes the same choice independently at four sites (`src/lir/lower/calls.rs:392-394`, `src/bir/lower.rs:866`, `src/ir/lowering/mod.rs:2980`, `src/ir/lowering/exprs/collections.rs:272/1046`). ggdef agrees (it returns the insertion-ordered element). **The single defect was one sentence in the book** — `docs/book/05-collections.md` asserted *"`Set[T]` is an unordered collection of unique elements"*, contradicting the authoritative reference **and** its own neighbours fifty lines later (*"`HashSet[T]` is the unordered counterpart to `Set[T]`"*, and the summary table's *"Same API as `Set`, unordered, faster"*). Diagnosis: an imported Python assumption. Python's `dict` is insertion-ordered since 3.7 but Python's `set` is not, and has no ordered counterpart; **Gorget deliberately closes that asymmetry**, which is why the sentence reads plausibly and is wrong. The book now says so explicitly, so the next reader reasoning from Python does not "correct" it back.

  **`s[i]` REJECTS — and the reason is NOT that sets are unordered (they are ordered).** Ordering and indexing are separate capabilities. `Dict` is insertion-ordered too, and `d[k]` is a **key** lookup, not a position — `[]` on a keyed collection means lookup. A set's elements *are* its keys, so over a `Set[int]` the expression `s[0]` cannot be distinguished from a lookup of the element `0`. The operator is left undefined rather than given an arbitrary reading. Ordinal access materialises first: `s.items()[i]` (verified end-to-end at ratification — builds and prints insertion order), or iterate. **Precedent:** Python refuses set indexing outright (`TypeError: 'set' object is not subscriptable`; neither `set` nor `frozenset` defines `__getitem__`) despite constant user demand, for this same ambiguity — while `{0: 'zero'}[0]` is perfectly legal as a key lookup.

  **CORE #8 RESOLUTION — three lanes, one ruling.** ggdef (`spec/ggdef/src/eval.rs:944-948`) accepts and returns the insertion-ordered element; Rust gg (C+LLVM) and the self-host silently accept and return **garbage** (a raw address; scout-measured `187650795045536`). Under this ruling ggdef's positional accept is the **spec defect** (adopt-and-reject), and the silent garbage on the other two lanes is a **check-time reject** (Core #10: lower-or-reject). All three lanes align in one round; `tests/fixtures/known_gaps/set_index_ggdef_divergence.gg` graduates with them.

  ⚠ **CONSEQUENCE TO RE-EXAMINE, not ruled here.** Round XXVII Track D landed a check-time reject for `.enumerate()` on **both** `Set` and `HashSet`. On a confirmed-ordered `Set`, `.enumerate()` is a meaningful operation, and Python permits `enumerate()` over even its unordered set. The `Set` half of that reject is therefore suspect and is filed for re-examination while it is one round old; the `HashSet` half is not in question. **⚠ AMENDED 2026-08-03 (D39 discussion): that flag was WRONG about its basis.** Track D rejects on Iterable-vs-Iterator typing (`Set` impls `Iterable[T]`, `.enumerate()` is an Iterator adapter) — the same reasoning holds whether or not `Set` is ordered, and the capability is available as `.iter().enumerate()`. The residue is only that `Vector` has direct `.enumerate()` sugar and `Set` does not; re-triaged to LOW. What the flag *did* surface is a live HIGH defect: `s.iter().enumerate()` — the exact remedy `E_EnumerateOnNonIterator` prints — panics lir-lowering (`known_gaps/set_iter_enumerate_lir_panic.gg`).

- 2026-08-03 — **D39 RATIFIED (owner): `Dict`/`Set` MOVE TO THE DENSE INDEX-MAP LAYOUT; THE TOMBSTONE LAYOUT SURVIVES AS `StableMap`/`StableSet`; ORDINAL ACCESS IS A NAMED ACCESSOR, NOT `[]`. D38's `s[i]` REJECT STANDS.** Owner framing that opened it: *"semantically I see Sets as closer to Vector/List with the guarantee of no repeated elements — that's why I miss the ordinal access."* The model is sound and it is what the layout change serves; what it does **not** license is the `[]` spelling (see the ambiguity finding below, which is D38's rationale re-derived from the owner's own `Dict[int, String]` example).

  **WHY TODAY IS O(n).** There are three ways to make a hash collection ordered, and Gorget picked the one that cannot index: **(a)** an intrusive linked list through the buckets (Java `LinkedHashMap`) — ordered, O(n) rank, permanently; **(b)** hash-slot-primary storage plus a side vector of slot indices — **Gorget today**: `gorget_map_remove` tombstones the slot (`states[idx] = 2`) and never touches `order`, while `gorget_map_keys` walks `order[0..order_len]` skipping dead slots, so the *i*-th live element is NOT `order[i]`; **(c)** a dense entries array plus a hash index table — O(1) rank. Gorget moves from (b) to (c).

  **THE CHOSEN SHAPE — six types, two clean axes, no aliases.** Bare name = ordered, `Hash` prefix = unordered, `Stable` prefix = ordered-and-stable-under-removal. `Dict`/`Set` become dense (O(1) key lookup, O(1) ordinal, order-preserving `remove` at O(n)). `HashMap`/`HashSet` are **unchanged** — they keep open addressing with linear probing, tombstones, *and* tombstone reuse (which ordered mode deliberately disables to keep the order vector valid), plus no index-table indirection on lookup; that combination is their entire reason to exist and going dense would erase it. `StableMap`/`StableSet` retain today's tombstone layout for its genuine profile — O(1) order-preserving removal and address stability — and live in their own module `lib/std/stablemap.gg`, following the `std.slotmap` precedent for specialist collections, which tiers them out of `std.collections` and out of the book's collections chapter without inventing a convention.

  **REMOVAL SEMANTICS — DERIVED, NOT CHOSEN.** `remove` preserves the relative order of the remaining elements. This was never open: the reference already states iteration is in insertion order, which is a *global* invariant a swap-style removal would silently break. It also matches `Vector`, which already ships exactly this pair — `remove(index)` shifting subsequent elements (O(n)) and `swap_remove(i)` documented "O(1) removal — moves last element into the hole (order-destroying)". So under the owner's own Set-is-a-Vector model, O(n) order-preserving removal is the *consistent* answer, not a regression: order-preserving removal from the middle of a contiguous array is O(n) by array semantics. **Naming ruled:** keep `swap_remove`; reject `fast_remove`. Name the hazard, not the reward — the dangerous property is that it silently reorders the collection, which `swap_remove` predicts and `fast_remove` conceals; "fast" is a relative claim that rots as the cost table changes; and `swap_remove` is already shipped on `Vector` and is the dominant convention (Rust `Vec`/`indexmap`, Zig `swapRemove` — Zig carries the identical pair on `ArrayList` *and* `ArrayHashMap`, the closest precedent that exists to Gorget's situation). **The asymmetry is deliberate:** the safe operation keeps the unmarked name `remove`; the hazardous one carries the qualifier. `indexmap` marks both (`shift_remove`/`swap_remove`); Gorget follows Rust's `Vec` instead, so the default requires no thought.

  **THE COST IS SWAPPED, NOT REMOVED — and O(1)/O(1) is IMPOSSIBLE.** Today: O(1) removal (amortized — ordered mode never reuses tombstones and counts them in the load factor, so churn already forces repeated rehashes) and O(n) rank. Dense: O(n) order-preserving removal, O(1) rank, plus O(1) `swap_remove`. You cannot have both: order-preserving removal decrements the rank of every subsequent element, so any structure answering "the k-th live element" in O(1) must materialise ranks, and one removal invalidates O(n) of them — this is the dynamic partial-sums / list-indexing problem, which carries a known logarithmic-ish lower bound (Fredman–Saks, cell-probe model). **Consequence: an O(log n) rank + O(log n) removal shape saturates that bound and is the theoretical optimum for wanting both** — implementation shape recorded in `docs/devbook/23-stdlib.md` §23.10. It is the named fallback if measurement shows remove-heavy ordered workloads matter; it is not the default, because O(log n) `[]` invites the same "is this cheap?" confusion O(n) does.

  **THE `[]` QUESTION — D38 SURVIVES, AND ITS RATIONALE WAS RIGHT THE FIRST TIME.** Mid-discussion the orchestrator ranked D38's two arguments backwards, softening the ambiguity argument and calling the O(n) cost load-bearing, then twice offered to withdraw D38's reject. Both offers are withdrawn. The owner's `Dict[int, String]` example is what settled it: **`d[0]` = the value for KEY 0 while `s[0]` = the element at POSITION 0** — same operator, same operand type, both hash-backed collections, different meanings, and *no layout change touches this*. The O(n) argument dissolves under a dense layout; the ambiguity does not. That inverts the ranking: **the ambiguity is the durable objection.** The consistent rule it implies, stated for the record: **`[]` takes the collection's lookup key** — `Vector` → index, `Dict` → `K`, `Set` → there is no lookup key (membership is `.contains`), so `[]` is undefined. Ordinal access is therefore a **named accessor** (`.nth(i)` on sets, `.key_at(i)`/`.entry_at(i)` on maps), O(1) once dense lands, unambiguous at every key type, symmetric across `Set` and `Dict`, and requiring no migration. ⚠ **Do NOT name it `.ordinal(…)` or hang it off an `Ordinal`-ish trait** — `Ordinal` is taken (`src/semantic/traits.rs:627-637`, `int ordinal(self)`, derivable on enums for the variant's declaration index, paired with the `enum_ordinal`/`enum_from_ordinal` meta builtins) and runs in the *inverse* direction (thing → position); its true analogue is `enum_from_ordinal`.

  **TWO REJECTED ALTERNATIVES, recorded so they are not re-proposed.** **(1) Split `Dict` from `Map`, restrict `Dict` to `String` keys, and disambiguate `[]` by operand type (`String` → key, `int` → index).** Rejected: it moves the collision from *between two types* to *inside one instance* — `d["0"]` (key `"0"`) and `d[0]` (position 0) both valid on the same object, differing by two quote characters; it does not solve the case that motivated the discussion, since the `Set[int]`↔`Map[int,V]` collision survives relocated; it is a breaking restriction on `Dict[K,V]`, which accepts any `Hashable` key today; it adds a second type axis (String-key/any-key) against pillar one; and JavaScript already ships "object keys are strings", where `obj[0]` coerces to `obj["0"]` — so a JS-shaped intuition lands on exactly the wrong answer, silently. **(2) `[]` means ordinal and `{}` means key access** (`d[0]` → position, `d{0}` → key). This is genuinely the *only* formulation that makes `[]` uniform family-wide — the orchestrator had claimed none existed, and it is coherent, with operator availability *deriving* from the ordered/unordered axis; parsing is likely tractable precisely because Gorget's blocks are indentation rather than braces (the reason Rust cannot do this), and migration would be mechanically automatable via `gg fmt` since `[]` on a map-typed receiver is unambiguously key access today. Rejected on **syntax budget** grounds: key lookup is among the hottest operations in any program while map/set ordinal access is rare, so the proposal puts the unfamiliar syntax on the frequent operation — permanently, for exactly the Python-shaped audience pillar one targets — to buy four characters on a rare one. The precedent is Perl (`$hash{key}` vs `$array[i]`), which is not the ergonomics model Gorget wants. **Revisitable** if real Gorget code shows ordinal access is pervasive; that decision would owe a Syntax-budget section per this ledger's 2026-07-18 directive. **(3) Name the dense type `DenseHashMap` with `Dict` as an alias.** Rejected: `Hash` is this family's marker for *unordered*, so `DenseHashMap` carries the token meaning "no order guarantee" while being the ordered type, burying the salient difference and advertising the storage layout; "Dense" predicts nothing a user needs (not ordering, not O(1) lookup, not O(1) rank, not O(n) removal) and names an implementation that this very decision changes — had the type been named `TombstoneHashMap` it would need renaming now; marking the *default* inverts the unmarked-default/marked-opt-in asymmetry ratified above for `remove`/`swap_remove`; and an alias forces every diagnostic, `gg fmt` normalisation, doc table and ggdef corpus entry to pick one spelling, confusing users of the other. Neither Python (`dict`) nor Rust (`HashMap`) aliases its map to an implementation-descriptive name. **Names describe guarantees, not implementations** — which is why `Stable` is acceptable and `Dense` is not.

- 2026-08-04 — **D40 RATIFIED (owner 2026-07-21; RECORDED here 2026-08-04): RETURN-VIEW LAZY MATERIALIZATION — STATIC PROVENANCE, NEVER A RUNTIME REFCOUNT; MATERIALIZE-WHEN-UNSURE, NEVER REJECT.** Ruled during the CoW cost discussion and, until now, recorded only in `docs/internals/unified-resource-model.md` §6 — which `docs/language-reference.md` was deferring to for a *user-facing* contract. This entry is the ruling's home; the design note elaborates it. **STATUS: RULED, NOT IMPLEMENTED** — today both compilers materialise at the return boundary (`ensure_owned_at_boundary` → `ReturnFromBorrow`).

  **THE DESIGN.** A function that returns a projection of its receiver or a parameter carries typed **view-return provenance** on its signature (the `returns_view` axis, today builtin-only, lifted to user functions); the caller propagates that `BorrowOrigin`; and the view materialises **lazily** — only where a conflicting mutation of the source is *statically* reachable while the view is live. This extends lazy CoW across the function-return boundary.

  **RULING 1 — STATIC PROVENANCE, NOT A RUNTIME REFCOUNT.** A Swift-style refcounted-buffer CoW (mutation asks "am I shared?" and copies if so) is **rejected**: it taxes every mutation with a shared-check and pins the old buffer in memory, breaking the zero-runtime-overhead pillar. Provenance is tracked at compile time on the local. This is what buys "no user-visible lifetimes" *without* a runtime tax.

  **RULING 2 — MATERIALIZE WHEN UNSURE, NEVER REJECT.** Where the static escape/effect analysis cannot *prove* the view stays valid, it materialises (today's clone); it never rejects the program. Consistent with the reject-gate killed 2026-07-02. **Sound by construction** — there is no runtime backstop, so the analysis must be clone-biased, and conservatism can only add clones, never a use-after-free.

  **HONEST SCOPE.** Yield is bounded by the analysis's reach: short-lived views provably dead before mutation reclaim; views held across a source mutation, escaped into a long-lived field, or crossing a boundary the effect analysis cannot follow stay clones. It needs (a) typed view-return provenance on **user** signatures and (b) an **inter-procedural effect analysis**. The reclaim **MUST be end-to-end-measured on leaf clone volume before committing** — the attributed figure is a small fraction of leaf clones, so the real yield is unproven. This is also the highest-UAF-risk CoW increment, and ASan is blind to view-into-element UAFs, so stdout fixtures are the primary net.

- 2026-08-04 — **D41 RATIFIED (owner 2026-07-22; RECORDED here 2026-08-04): NO STORED BORROWS, NO USER-VISIBLE `Ref[T]`; CLOSURES ARE THE SANCTIONED MUTATE-THROUGH PATH.** Ruled during the CoW-aliasing round and recorded only in `docs/internals/cow-transient-view-model.md`. This ledger already *cited* that file by line inside the D36 ratification while never recording the ruling itself — this entry closes that loop. **STATUS: partly shipped** (a `returns_view` flag exists on `BuiltinMethodDecl` but carries the String view axis; the projection-root name whitelist survives). The accept/reject change below is **unbuilt** and owes all lanes per Core #9 when it lands.

  **VIEWS ARE INTERNAL TO BUILTINS ONLY.** `Ref` never appears in user source. The typed `returns_view` flag replaces the projection-root name whitelist (Core #2). §3.7's "no user-visible borrowed-return type" **stays true** — the concession an earlier critique flagged evaporates rather than being paid.

  **USER METHODS RETURN OWNED.** Mutating through a user method's result is **rejected as a non-place**. Today it silently loses the write, so this is a strict soundness improvement, not a regression: `grid.at(x, y).mark()` on a user `at` becomes an honest error instead of a dropped mutation.

  **CLOSURES ARE THE ONE CORRECT WAY** to mutate an encapsulated element — `grid.update(x, y, (Cell &c): c.mark())` — which is legal today and writes through. Chosen over adding a borrow-type concept purely for call-site sugar: one correct way, in the Pythonic spirit of pillar one.

  **TRANSITIVE-UNSTORABLE IS AN INTERNAL INVARIANT, NOT A USER TYPE RULE.** No LIR storage slot may hold a `Ref`-containing type — a stored `Ref` is a use-after-free. Because `Ref` is never user-spellable, this is a compiler guard, not a rule users must learn.

  **REJECTED ALTERNATIVES, recorded so they are not re-proposed.** **(1) A user-visible `Ref[T]` return type** — rejected; it imports a borrow-type concept into the surface language for call-site convenience and reopens §3.7. **(2) `a = &f()`** — shelved. **(3) A "place alias"** (a pure-projection method expanding to a place, with no `Ref` type) — shelved next to `&f()` unless call-site readability later earns exactly one small concept.

- 2026-08-04 — **D42 RATIFIED (owner 2026-07-28; RECORDED here 2026-08-04): THE `implicit_clones` KNOB — ONE NAME, THREE SCOPES, `allow`/`warn`/`deny`; EXPLICIT `.clone()` IS EXEMPT.** The spelling and scoping were owner-chosen during the CoW cost-contract discussion and recorded only in `docs/internals/cow-cost-contract.md` — user-visible surface syntax (an attribute, a directive, and a CLI flag) living outside the ledger. **STATUS: NOT IMPLEMENTED** — no code exists, and `implicit_clones` is not yet in the attribute whitelist.

  **THE SHAPE.** Project: `--implicit-clones=deny`. Module: `directive implicit-clones=deny`. Function: `@implicit_clones(deny)`. Values are **`allow`** (default) · **`warn`** · **`deny`**. This is the `tailrec` design generalised — an annotation that does nothing except make the compiler **prove a property or error**. `tailrec` does not make a function tail-recursive; it tells you when it isn't.

  **THE CONTRACT — "every copy in this function is one I wrote."** An **explicit `.clone()` is exempt**; only *implicit* clones are diagnosed. So the mechanical fix for a violation is to write the clone, which is the good outcome: the cost becomes visible in the function whose author asked to see it, and stays an invisible implementation detail everywhere else. This is the sequel to the recursive-bare-param materialize warning, not a duplicate: the warning **steers** on one shape the compiler nominates; the knob lets the author **assert** the property over a chosen scope and have it discharged, including on shapes no heuristic would flag.

  **REJECTED SPELLINGS, recorded so they are not re-proposed** — `@explicit_clones_only`, `@no_implicit_clones`, `@clone_budget(0)`, `@zero_copy`. **The escape hatch needs the same name:** a boolean spelling requires a second, differently-named antiknob (`@allow_implicit_clones`) to exempt one function inside an opted-in module — two names and two validation paths for one axis. The valued form is the `#[allow(...)]` shape readers already parse fluently. **The warn tier is not optional and comes free:** every comparable change in this tree burned down through a warning first, and a project-wide `deny` over an existing corpus is unusable without `warn` as the staging tier.

- 2026-08-08 — **D43 RATIFIED (owner 2026-08-08): VISIBILITY = THREE LEVELS, TWO KEYWORDS, AND THE DEFAULT UNIT IS THE DIRECTORY.** *Everything is visible within its **directory** unless marked `public` (visible everywhere) or `private` (this file only).* The middle tier is the default, so it needs no keyword. This is Java's `public`/package-private/`private` and C#'s `public`/`internal`/`private`. **STATUS: NOT IMPLEMENTED** — today the default is `public`, the unit is the file, and two of the three tiers are unenforced.

  **WHAT IS BROKEN TODAY, measured 2026-08-08 by build-and-run probes.** Item-level `private` IS enforced (`E_PrivateImport`, `src/semantic/resolve.rs:303`; plus scope non-promotion). **Struct FIELD and `equip` METHOD visibility are parsed and then read only by the formatter** — `StructField.visibility` has exactly one consumer in the tree, `src/formatter/mod.rs:613`. A cross-module private-field read AND write both compile and run (`p.internal_id = 99.0` prints `99.000000`); a cross-module private-method call runs. `PrivateTypeInPublicSignature` (`src/semantic/safety/validation.rs:83-148`) matches only `Item::Function` and `Item::Equip`, so a private type leaks unchecked through a public struct field, an enum variant payload, a trait method signature, and a public `const`/`static`. `private` in the ENTRY file is a no-op (`private_names` is computed per `Item::Module`; the entry file is not wrapped). Imports validate but do not FILTER: `from lib import Point` also makes `public_helper` visible, and a three-module chain leaks `deep_fn` into an app that imported only `mid`.

  **WHY PRIVATE-BY-DEFAULT.** Gorget ships a package manager, so public-by-default makes every internal helper part of the semver surface by accident. The corpus shows the failure directly: `lib/xtd` has ~1109 top-level decls, 89 marked `private`, and **zero** explicit top-level `public`; per-module marking is incoherent (`toml.gg` 19/27 private, `json.gg` 2/17). Nobody can say what `xtd/json`'s API is, because the language never made anyone say. It also matches the language's posture everywhere else — ownership, `&`/`!`, mutation acknowledgment: the powerful thing is opt-in and visible, and exporting a name is the powerful thing.

  **WHY THE DIRECTORY AND NOT THE FILE.** Under file-granularity, a helper shared with a sibling file must be marked `public` — exported to the world — so the mark stops meaning "this is my API". Measured: **70** distinct names are imported within a package (std→std + xtd→xtd) versus **50** that genuinely cross a package boundary (xtd→std), 8 overlapping, so file-granularity needs ~112 marks where directory-granularity needs 50. The qualitative cost is worse than the count: a reader cannot tell whether `public foo()` means "std's API" or "socket.gg needs this", so 70 meaningless marks dilute the 50 meaningful ones. The in-tree witness is `_errno_to_io_error` — declared unmarked at `lib/std/io.gg:280` (raw C errno → `IoError`, pure implementation detail) and imported by three siblings (`tls.gg:4`, `socket.gg:4`, `udp.gg:4`). **File-granularity would force that helper into std's published API precisely because its author declared, by the only means available — a Python underscore the compiler does not enforce — that it is not API.** ⚠ **Honesty about the strength of this witness: it is the ONLY one in the corpus.** Exactly 1 distinct underscore-prefixed name appears in any import list across `lib/` (an earlier draft claimed ~65, from a regex missing a word boundary that matched substrings of ordinary snake_case — `server_socket_bind` → `_bind`; regenerate with `grep -rh "^from .* import " --include=*.gg lib/ | sed 's/.*import //' | tr ',' '\n' | tr -d ' ' | grep '^_' | sort -u`). The corpus cannot supply more, because under public-by-default nobody was ever forced to declare an API surface, so the absence of an underscore on the other 69 within-package names is not evidence they are intended-public. **The 70-vs-50 ceremony measurement, not the underscore count, is what carries this section.**

  **WHY THE DIRECTORY AND NOT THE MANIFEST.** A first draft scoped visibility to the `gorget.toml`-rooted tree, reasoning that the package is the versioned unit and semver is about breaking dependents. That conflates two questions: semver blast radius is about DEPENDENCY change, visibility blast radius is about CODE ORGANISATION. **Go separates them deliberately — `package` = a directory = the visibility unit; `module` = the `go.mod` root = the versioning unit** — as do Java and Python. Decoupling also means the manifest's FILENAME is a free branding choice, `lib/std` and `lib/xtd` need no manifest in order to have privacy, the 2854-of-2856 in-repo `.gg` files with no manifest ancestor just work, no `PackageId` need be plumbed (the directory is already in the module path), and the two filed manifest-lookup defects (unbounded upward walk; resolution depending on invocation form) stay packaging bugs instead of becoming semantics bugs. The corpus is already shaped for it: `lib/std`, `lib/xtd`, `lib/gg`, `compiler/data` and all six `self_host_*` dirs contain zero subdirectories.

  **WHY THREE LEVELS AND NOT TWO.** Dropping `private` for a one-keyword model is a breaking change with **20 known casualties**: 20 distinct `private` names in `lib/xtd` collide across files — `escape_string` and `make_indent` are each declared `private` in both `json.gg` and `toml.gg`, alongside `arr_set`, `arr_delete`, `collect_recursive`, `del_recursive`, `eval_predicate`, `json_as_cmp_num`, `json_as_cmp_str`, `json_is_numeric` and ten more. Two independent format serializers both needing an `escape_string` is what independent modules do. Cross-file collisions are already `E_DuplicateDefinition`, with flat C mangling behind them. **And `private` already means file-only today**, so the file tier is not a new concept — it is existing behaviour preserved while the default narrows from world to directory. Removing it was the breaking direction. Migration is correspondingly cheap: **~50 `public` marks to add, 0 `private` marks to delete, 0 collisions to resolve.**

  **"DIRECTORY" MEANS THE IMMEDIATE PARENT, NOT THE SUBTREE — NON-RECURSIVE.** A file's visibility group is the set of `.gg` files sharing its immediate parent directory. Subdirectories are SEPARATE groups: if `lib/std/net/` is ever created, `std/net/socket.gg` does **not** see `lib/std/io.gg`'s unmarked names, and vice versa. This is Go's and Java's rule. It is stated here because the corpus is flat TODAY (`lib/std`, `lib/xtd`, `lib/gg`, `compiler/data` and all six `self_host_*` dirs have zero subdirectories), so the question has never come up — and it is far cheaper to nail before someone builds a tree on the recursive assumption.

  **A COROLLARY WORTH TEACHING: the common encapsulation case needs no keyword at all.** `public struct File:` with an unmarked `int handle` field gives exactly the intended shape — the world sees the type, only files in the same directory see the handle. So `private` is reserved for hiding something from *directory siblings*, not for ordinary encapsulation, and plain-data structs are not taxed with a `public` on every field (an unmarked `Point.x` is visible wherever `Point` is usable within the directory). This is a real ergonomic advantage over file-granular private-by-default, which would force a mark on every field of every shared struct.

  **THE COST, ACCEPTED: a package cannot span directories.** A multi-directory app gets one package per directory, so cross-directory helpers need `public`. That is Go's model, and it pushes toward the flat layouts this corpus already has. A related consequence to state rather than discover: **all 28 files of `lib/std` are one visibility bubble** and see each other's unmarked names. That is Go-like and intended; **if a subsystem grows too cozy the fix is to SPLIT THE DIRECTORY, not to add a keyword** — say so in the book, so readers do not reach for a `pub(crate)` tier the language deliberately defers.

  **A FOURTH TIER IS DEFERRED, NOT REJECTED** — the distribution/workspace level between world and directory (Rust `pub(crate)`, C# `internal`), for internals shared by `std`+`xtd` but hidden from users. Adding a level ABOVE the default is purely additive and non-breaking, so it waits for evidence; the file tier could not wait because it already exists. **The measurement that would justify it, not yet done:** classify the 50 `xtd`→`std` names as genuine API versus plumbing. Go is the precedent both ways — two levels in 2009, the `internal/` directory rule in Go 1.4 (2014), as a convention rather than a keyword.

  **SUB-RULINGS, all measured at HEAD.** Enum variants **inherit** the enum's visibility; no per-variant marks (note bare `Red()` does resolve cross-module today via `loader.rs::merge_modules` unambiguous qualification — verified running). Visibility belongs on the `equip` **method**, not the block (`private equip` currently parses as a silent no-op — make it a parse error). **Every file is a module, including the entry file** (fixes the entry-file `private` no-op). `main` needs no carve-out — it is already special by name (`src/semantic/typecheck.rs:9577`, `src/semantic/rewrite.rs:72`). **THE PER-KIND DEFAULT CARVE-OUTS ARE RETIRED — `static` and struct fields follow the one uniform rule** (owner 2026-08-08, correcting a first draft of this entry that said "`static` is the one item kind that does not change: it is private-by-default today and stays so" — that sentence was WRONG, and reintroduced the very asymmetry D43 exists to remove). Today `private` means FILE-only, so "statics stay private-by-default" would leave `static` as the sole kind defaulting to the FILE while every other kind defaults to the DIRECTORY. **The carve-out's own justification evaporates under D43.** It exists because the default was WORLD-visible, and the spec states the reason outright: *"mutable module-level state should be explicitly exported"* (`docs/language-reference.md` §12). D43 makes the default the DIRECTORY, so the accident the carve-out prevents — mutable global state escaping to the world unmarked — can no longer happen. It was a patch compensating for a bad default; fixing the default retires the patch. **Nothing is lost:** `private static int counter = 0` still gives file-only scoping, one word away. Two parser sites implement the carve-out and both go: `src/parser/mod.rs:652` (`Token::Keyword(Keyword::Static)`) and `:692` (the implicit `TypeName varname = expr` module-var form). ⚠ **The struct-field carve-out is worse than a carve-out — it is a SPEC-VS-PARSER CONTRADICTION, measured 2026-08-08:** §12 says fields are private by default, but `Parser::parse_visibility_modifier` (`src/parser/mod.rs:282-290`) documents and implements "Defaults to `Public` if neither is present", and field parsing at `:1030` calls it — so fields are parsed PUBLIC while the spec claims private, and nothing enforces either way. D43 resolves it by uniformity rather than by picking a side: an unmarked field is directory-visible, exactly like Java's package-private fields, and `private` is available for tighter scoping. If mutable directory-wide state is judged a smell later, the tool for that is a lint or a warning, **not** a per-kind default asymmetry.

  **REJECTED ALTERNATIVES, recorded so they are not re-proposed.** **(1) Public-by-default with `private` opt-out** (options A / A′) — the status quo; makes accidental API permanent once a package ecosystem depends on you, and the corpus shows it produces no declared API surface at all. **(2) Name-based visibility** (Go capitalization, or an enforced `_` prefix) — couples the name to the API decision, so renaming is an API change, and it fights the type-first spelling. **(3) A module-level export list** (`export foo, bar`, or an OCaml-style signature file) — a second mechanism to keep in sync with the declarations. **(4) Fields inherit the struct's visibility** — cannot express a public type with a private field, which `File.handle`-shaped opaque handles already need.

  **OBLIGATIONS WHEN IT LANDS.** Core #9 — a semantic change lands on every lane the same round (ggdef within its subset, Rust gg C+LLVM, self-host), pinned by cross-lane fixtures. Core #11/#12 — RED-verified fixtures per tier and per leak surface, axis-complete over the item kinds (field · method · variant · trait signature · const/static), not one thin pin. The enforcement gap is the bulk of the work: field and method checks do not exist at all today. Doc write-through to `docs/language-reference.md` §12 and `docs/book/15-modules.md`, both of which currently document public-by-default. The companion "imports bind / no transitive leak" work is what makes `public` mean anything; qualified `mod.name` additionally requires module-qualified C mangling first, since today's flat mangling is why cross-module name collisions are a hard `E_DuplicateDefinition` (`src/semantic/resolve.rs:930`).

- 2026-08-08 — **D44 RATIFIED (owner 2026-08-08): THE MANIFEST FILE IS `gorget.toml`, AND IT IS DECLARATIVE TOML, NOT GORGET SOURCE.** **STATUS: SHIPPED.**

  ⚠ **AMENDED 2026-08-09 (owner) — THE FILENAME HALF OF THIS DECISION WAS REVERSED. The format half stands unchanged.** As first ratified, D44 renamed the manifest `gorget.toml` → `manifest.toml`, with a deprecation fallback. That rename is REVERTED: the file is `gorget.toml`, and the fallback is deleted rather than deprecated (`manifest.toml` existed for one day in one repo, so there was nothing to stay compatible with). **The reasoning failed at a specific step, recorded so it is not re-walked:** the case against `package.toml` rested on three legs — collision risk under the unbounded upward walk, STUTTER against its own `[package]` table, and searchability/branding. D43's decoupling of visibility from the manifest retired the collision leg. The rename then justified itself on the STUTTER leg — but stutter was only ever an argument *against `package.toml`*; `gorget.toml` never stuttered, so it said nothing in favour of renaming. The branding leg was never refuted and still favours `gorget.toml`. **Net: decoupling made a generic name ACCEPTABLE, never BETTER — and "acceptable" does not justify renaming a file already in use.** The analytical gap was scoring the rename's MECHANICAL cost ("55 references, an afternoon") while never scoring its INERTIA cost across dogfood apps, docs and shipped history. Cheap to change is not free to change. **What survives from the rename:** `manifest::MANIFEST_NAME` + `manifest_path_in` + `find_manifest_in`, which replaced 20 inline `"gorget.toml"` literals across `manifest.rs`/`resolver.rs`/`main.rs`. The name is now a one-constant edit, so this reversal cost a single line of code. **Do not re-propose the rename without a NEW argument** — the stutter/role-naming one is spent.

  **WHY A GENERIC NAME BECAME *ACCEPTABLE* (the fact that survives the amendment; it is not a reason to rename).** A first analysis argued *against* a generic name, on the grounds that `manifest::find_project_root` walks UP the directory tree, so a stray ancestor file could claim a project — and under the then-current D43 draft the manifest defined the PRIVACY BOUNDARY, making a false positive silently rescope every declaration beneath it. **D43 as ratified decouples the two:** the visibility unit is the DIRECTORY, and the manifest governs only dependencies and versioning. With that coupling gone, a mislocated manifest is a packaging error, not a semantics change, and the filename is a branding choice rather than a load-bearing one.

  **WHY `gorget.toml` OVER THE ALTERNATIVES.** It is unambiguous, greppable and searchable — for a young language, a user hitting an error and searching the filename should land on Gorget, not on generic noise. It is also collision-proof against the unbounded upward walk, which is still UNFIXED and filed separately (unbounded walk; resolution depending on invocation form); D43's decoupling downgraded a false positive from a semantics change to a packaging error, but a branded name remains free insurance until those land. `package.toml` additionally stutters against its own `[package]` table. Nearly every language brands its manifest — `Cargo.toml`, `pyproject.toml`, `go.mod`, `Gemfile`, `pom.xml`, `build.zig.zon`, `pubspec.yaml`, `mix.exs`, `Package.swift`; npm's `package.json` is the notable generic, landed in 2010 with no competing toolchain, and its upward resolution is a known source of "which config won?" confusion.

  **WHY IT STAYS TOML AND IS NOT WRITTEN IN GORGET.** `compiler/data/resources.gg` is the in-tree precedent for data-in-Gorget, but its determining reason does not transfer: it is a single source of truth consumed by TWO compilers, one of which is written in Gorget. A manifest has no such constraint — `lib/xtd/toml.gg` (1788 lines) already lets the self-host read TOML. And `resources.gg` is itself consumed as INERT data: `src/resources.rs` parses it and walks "the literal-only subset", never evaluating. So the precedent supports a data-only format, not a manifest-as-program. **The survey argument:** every package manager that made its manifest executable was later forced to generate a declarative sidecar so metadata could be read without running code — Arch `.SRCINFO`, Gentoo `md5-cache`, FreeBSD ports `INDEX`, Homebrew's JSON API, Python's `setup.py` → `pyproject.toml` under PEP 517/518/621. The declarative ones (Cargo, Maven, `go.mod`, Deb822 `control`) never needed one. The near-universal shape is metadata declarative + build steps executable: `Cargo.toml`/`build.rs`, `control`/`rules`, spec-header/`%build`, `build.zig.zon`/`build.zig`. Gorget follows it.

  **REJECTED ALTERNATIVES, recorded so they are not re-proposed.** **(1) `package.toml`** — stutters against `[package]`, and generic besides. **(2) `manifest.toml`** — tried and REVERTED 2026-08-09; see the amendment above. **(3) `gg.toml`** — shorter and matches the binary users type, a reasonable alternative that simply did not beat the incumbent by enough to justify churn. **(3) A manifest written in Gorget** (full language) — makes dependency resolution execute untrusted third-party code, the npm-`postinstall` / SwiftPM-`Package.swift` shape. **(4) A ZON-style data-only Gorget subset** — coherent, and Zig ships it, but it costs a comment-preserving serializer for `gg add`/`gg remove` (which round-trip through serde today for free) and hands every external tool a parsing problem, for a file that is four keys and a dependency table.

  **ONE SOURCE OF TRUTH FOR THE NAME.** `manifest::MANIFEST_NAME` plus `manifest_path_in` / `find_manifest_in`; no call site spells the string. This is the durable win of the D44 round and it outlived the rename it was built for — it is what made the 2026-08-09 reversal a one-line change.

- 2026-08-10 — **🎯 ERROR-MODEL NORTH STAR (owner-directed, recorded at owner request):
  THE BAR IS "THE MODEL OTHERS ASPIRE TO", NOT PARITY WITH ANY ONE REFERENCE.** Opened as a
  Zig "no hidden control flow" comparison; the owner widened the mandate — *"we should not be
  comparing only to Zig… Gorget is to become the leader."* This entry is the strategic frame
  for A31/A33/A34 and for every future error-model call. **Ratification status: the GOAL is
  owner-directed; the four-property doctrine below is the synthesis recorded at his request;
  the library/semver design in the last section is PROPOSED, NOT RATIFIED — ⚡ 2026-08-10:
  that section is now RATIFIED via D45 (see its head annotation), and the four-property
  doctrine below is SUPERSEDED by D45's seven-guarantee form.**

  **⚠⚠ CORRECTED 2026-08-10 (same day) BY AN ADVERSARIAL FABLE REVIEW — TWO CLAIMS IN THIS
  ENTRY WERE WRONG; BOTH CORRECTIONS RE-VERIFIED INDEPENDENTLY BY THE ORCHESTRATOR.**
  **(1) "The census shows it already happening" is OVERSTATED — the model has ~ZERO production
  users, which is a WORSE finding than the one it replaces.** Measured: `lib/std/*.gg` contains
  **0** `throws` declarations (`read_file` is typed INFALLIBLE — `lib/std/fs.gg:6`), and the
  self-host contains **0** `throws` signature declarations across all six directories
  (`grep -rhoE "\) throws [A-Za-z_]+:" tests/fixtures/self_host_*/*.gg` → 0; the ~338 raw
  "throws" hits are the compiler's own handling of the keyword, e.g. the `throws_type` field and
  `elif s == "throws"` — an orchestrator grep that did NOT distinguish these was the source of
  the error). The self-host accumulates `Diagnostic` VALUES instead. So the 148 `throws String`
  measures TEST-FIXTURE CONVENIENCE, not escape pressure: **there are no layers to collapse
  yet.** The Java risk is PROSPECTIVE, not underway. But the reason it is not underway is worse
  than if it were: **the error model's most important consumer bypasses it entirely, and the C
  runtime calls `exit(1)` 59 times** (`find src/backend/c -name '*.c' -not -name sqlite3.c
  -exec grep -c 'exit(1)' {} +` → 59) — errno-style hidden fatal failure, the named anti-goal,
  AND the *static-rejection* exit class, violating §10.10's own "the classes MUST be distinct so
  a runtime crash can never masquerade as a correct static rejection".
  **(2) "Gorget already holds property 2 (visible flow) outright" is FALSE at HEAD.** A bare
  `Result`-typed value in a `T` position still silently unwraps-or-early-returns with NO mark
  anywhere — surviving pre-D29 value-plane auto-propagation. Orchestrator-verified: with
  `Result[int,String] r = fails()` and `int consume(int x)`, the call `consume(r)` passes
  `gg check` clean, and at runtime **the callee never runs** and the error propagates. D29
  governs fallible CALLS; a Result-typed VALUE coerced to `T` at an argument position is a
  second, unmarked propagation channel with no subject in the rule (Core #15e Q4 — a case the
  rule's subject does not cover). Two `known_gaps/sound_autoprop_*` fixtures record adjacent
  shapes with headers calling the free-call behaviour INTENDED, i.e. two ratified doctrines in
  direct conflict (dispositions refined in D45 pin 6 — they are OPPOSITE: one header
  overturned, one vindicated). **Consequence for A34: its premise ("the mandatory mark means the compiler
  knows every propagation hop") has a HOLE until value-position auto-prop is killed — that fix
  is a PREREQUISITE, not a parallel track.** Further corrections from the same review:
  **OCaml's polymorphic variants are payload-carrying inferred open/closed sets with
  handler-side narrowing, which FALSIFIES the novelty claim for A31**, and **Elm's enforced
  API-diff semver is prior art for frontier (a)** — it is enforcement-in-a-package-manager that
  is unclaimed, not the idea. Full report + ~20 compiled probes: the review also found the book's
  flagship example does not compile against the real stdlib (orchestrator-verified:
  `read_file(path)!` → `E_MissingFallibleMark`, because `read_file` is infallible), and that
  `catch` on a VOID fallible call miscompiles in every spelling (orchestrator-verified:
  `gg check` clean, `gg build` fails with a raw C error `void value not ignored as it ought to
  be` leaking to the user). Both filed to `TODO.md`.

  **⚠ THE FINDING (as originally written — read WITH correction (1) above) — THE JAVA FAILURE
  MODE IS THE STRUCTURAL RISK.** Java's checked exceptions had the RIGHT idea (errors stated in the
  signature, compiler-enforced) and collapsed for a specific mechanical reason: no inference
  and no set algebra, so every layer had to restate or wrap its callees' errors, and the
  pressure escaped through the widest available hole — `throws Exception`. **Gorget today has
  typed errors in signatures, no inference, and no set algebra; the escape hole is
  `throws String`.** Measured 2026-08-10 (regenerate before acting, Core #5): 148 uses across
  `tests/fixtures`, more than every other error type combined (next: `throws E` 36,
  `throws int` 17), and `docs/book/10-errors.md` teaches it 9 times. `language-design.md:81`
  names stringly-typed errors as an explicit ANTI-GOAL and error-set inference as a GOAL — so
  the anti-goal is winning precisely because the goal is unbuilt. **The concrete harm is not
  aesthetic: a `String` error cannot be DISCRIMINATED.** Every caller can only `catch (e)` and
  get text — never match a variant, never handle `NotFound` differently from `BadPort` — so the
  moment a library throws `String`, every consumer's handling collapses to blanket recovery or
  string comparison. Honest caveat on the number: fixtures are small programs and some of the
  148 is test convenience; the load-bearing evidence is the BOOK teaching it plus the named
  anti-goal, not the raw count. **Consequence: A31 is not an ergonomics upgrade, it is the
  load-bearing decision** — the difference between Gorget's typed-error model working and being
  routed around the way Java's was.

  **THE FIELD (surveyed 2026-08-10, so later calls are not re-derived).** C: no type-level
  signal. C++: RAII cleanup, but invisible flow, `throw()` specs abandoned, two models coexist.
  Java: right idea, no composition ⇒ the failure above. C#: gave up on the type level. Go:
  errors are values, flow maximally visible, `%w` chains — but verbose, interface-typed (no
  exhaustiveness), trivially ignorable. Rust: typed payloads, exhaustive, `must_use` — but
  `From` is a hidden conversion, there is no automatic trace (`Error::source()` is manual and
  span-less), and composition is boilerplate (hence `thiserror`). Swift: uniform mandatory mark,
  typed throws since 6.0 — no inference, no traces. Zig: inference, `errdefer`, **error return
  traces** — but NO payloads, globally interned error names, and inferred sets leak from public
  APIs (its own docs warn against them there). Erlang/Elixir: supervision + isolation as a
  SEPARATE axis from handling, nothing typed. Koka/Eff: row-polymorphic algebraic effects — the
  real solution to composition, at a conceptual cost most programmers will not pay. **Midori
  (Joe Duffy, unshipped): the definitive doctrine — recoverable errors ≠ bugs, typed contracts
  plus fail-fast abandonment. Gorget's ratified errors-vs-faults split IS Midori's model, and
  Midori never shipped, so its unfinished business is unclaimed.**

  **THE DOCTRINE — four properties, and no language has all four (⚡ SUPERSEDED 2026-08-10
  by D45's seven-guarantee form; kept for derivation).** *An error is a value with
  a typed CONTRACT, a visible FLOW, a provable HANDLING obligation, and a recorded HISTORY —
  and none of those costs anything you did not ask for.* (1) **Typed contract** — payloads, and
  public error sets that are WRITTEN, not inferred. (2) **Visible flow** — a mandatory mark at
  every channel-active site. (3) **Provable handling** — exhaustiveness, open/closed sets, no
  silent discard. (4) **Recorded history** — the causal chain, automatic, and free in release.
  **Scored:** Rust has 1+3. Swift has 2, and 1 since 6.0. Zig has 3, partial 2, partial 4, fails
  1. Go has 2 and a manual 4. **Gorget holds 2 ON PAPER (D29) — but see correction (2): the
  value-position auto-prop hole means it does NOT hold it at HEAD — and most of 1 and 3;
  A31 completes 1 and 3; A34 delivers 4.** ⚡ **The doctrine is INCOMPLETE — the Fable review
  names three missing axes, and the third is the important one:** (5) **MULTIPLICITY** — one
  error at a time is an assumption, not a law; the self-host proves the need by routing around
  the channel entirely to accumulate `Diagnostic` values, so accumulation must be a BLESSED
  named pattern (`Vector[Result[T,E]]`) even if it stays out of the channel; (6) **COST
  CONTRACTS** — what the error path is allowed to allocate/do, the `on error`-must-be-infallible
  class; (7) **THE MACHINE CONSUMER** — an agentic harness runs read-diagnostic → localize →
  edit → re-run → verify, and every property that shortens or destabilizes that loop is part of
  the error model's design, not its packaging. **(7) is argued to be the real 2026 frontier and
  is largely SEMANTICS-FREE work** (stable one-code-one-fix identities, structured fix-its with
  applicability tags, JSON diagnostics + `NO_COLOR`, byte offsets, a `check`-accept ⇒
  `build`-success guard ratchet, frozen runtime-error grammar, determinism pinned by fixture).
  Note the pleasing corollary the review draws: **Gorget's mark-everything doctrine is,
  unintentionally, the most agent-friendly error design in any language** — a machine edits from
  a context window and cannot go ask, so invisible remote facts (implicit `From`, value-position
  auto-prop, D26's silent signature rewrite) tax machines harder than humans. That is a reason to
  DEFEND D29 against "it's noise" pressure, and to advertise it.

  **THE FRONTIER — three things NO language has, which is what turns "best" into "aspired to".**
  (a) **Machine-checked error contracts across versions** — once public sets are explicit the
  compiler holds both the old and new set, so error-compatibility is mechanically verifiable at
  package resolution (`manifest.rs` + `lockfile.rs` already exist). Rust needs external
  `cargo-semver-checks` and cannot do it fully. (b) **Errors through structured concurrency** —
  every language degrades here (Go's errgroup, Rust's `JoinError`); a `TaskGroup`'s error set
  should be the UNION of its tasks', composing under the same set algebra as sequential code.
  The ledger currently files throws×async as "later"; this elevates it. (c) **The fault↔error
  bridge at a supervised boundary** — Midori argued the split and never shipped the bridge;
  Erlang shipped supervision with no types. **A33's hook is exactly that unclaimed
  intersection**, and this is the reason to keep it scoped to ONE defined isolation point.

  **WHAT NOT TO COPY (recorded so it is not re-proposed).** Java's restate-or-wrap tax.
  C++/C#'s invisible flow. Go's verbosity as the price of visibility — D29 already proved ONE
  CHARACTER buys it. Effect-system syntax: Koka solves composition correctly, but
  row-polymorphism-in-the-signature is a tax the "easier than Python" pillar cannot pay;
  open/closed sets get most of the benefit at a fraction of the concept count.

  **PROPOSED (NOT RATIFIED) — how error sets work at a LIBRARY boundary.** ⚡ **→ RATIFIED
  2026-08-10 as D45 (see that entry), with amendments: union spelling is `|` (the braces in
  (iv)–(v) below are DEAD — `throws NotFound | Denied`); named sets spell as `type` aliases;
  handler-side subtraction added at member granularity.** Owner asked the three
  questions that decide whether sets are an asset or a liability: how do they work in libraries,
  can a function suddenly fail with an unlisted error, and how do we know everything is handled.
  (i) **Under naive inference the answer to Q2 is YES, and the change can originate two packages
  away** — a dependency adds a variant, it flows into your inferred set, and your public API
  changes without you touching a line, silently re-exporting your dependency's error taxonomy
  and coupling your semver to theirs. So inference must be INTERNAL only. (ii) **Proposed rule:
  a `public` function's error set must be WRITTEN; inference applies within a directory-
  visibility group** — keyed on ratified D43, whose own rationale already argues this shape
  ("public-by-default makes every internal helper part of the semver surface by accident"; an
  inferred public error set is the same accident one layer down). A checker rule, not a lint.
  (iii) **Consequence worth stating: library signatures DO NOT CHANGE.** `throws AppError` over
  a named enum already IS a closed named error set; A31 adds inference for internal code and
  set algebra for composition, so the library migration story is "nothing happens".
  (iv) **Q3 answered by an open/closed distinction visible IN THE SIGNATURE:**
  `throws {NotFound, Denied}` closed ⇒ compiler proves exhaustiveness, no `else` needed, adding
  a member is deliberately breaking; `throws {NotFound, Denied, ...}` open ⇒ compiler REQUIRES
  an `else` arm, so adding a member is non-breaking by construction. Either way there is no
  unhandled path. Better than Rust's `#[non_exhaustive]`, where you must go read a distant enum
  declaration to learn whether your match needs a wildcard. **Proposed default: CLOSED**, with
  `...` as the opt-in to growth (⚡ D45: open-marker glyph `..` vs `...` TBD at the E2 grammar
  scout) — matching D43's posture that the powerful thing is opt-in and
  visible (here, an author reserving the right to change the contract). (v) **Widening is free:**
  calling a `{A,B}` callee from a `{A,B,C}` caller is a pure set widening, no conversion and no
  user code — **which is what RETIRES the implicit-`From` hidden-control-flow hole rather than
  patching it** (see the live defect filed with A34). (vi) **⚠ SCOPE THE GUARANTEE HONESTLY:**
  a library can still terminate the process via a FAULT (overflow, out-of-bounds, div-by-zero),
  a failed assert, or OOM — deliberately out of the channel and uncatchable. The accurate claim
  is **every ERROR is handled; not every TERMINATION is recoverable.** That is the ratified
  errors-vs-faults split working as designed, and it is why A33 stays a separate question.

- 2026-08-10 — **🎯 D45 RATIFIED (owner, in-session design iteration on the NORTH STAR + the
  adversarial Fable review): THE ERROR-MODEL CONSOLIDATION — ONE CHANNEL, SEVEN GUARANTEES;
  NAMED SETS VIA `type`; SUBTRACTIVE `catch`; `rethrow` RETIRED.** The owner ratified the v2
  consolidation package in full (the eight owner calls), then settled the two open spellings in
  a follow-up exchange (named-set spelling = `type` aliases; `rethrow` retirement confirmed).
  **STATUS: DESIGN RATIFIED, NOT IMPLEMENTED** — implementation is rounds E0–E4, filed in
  `TODO.md` (define-gorget section). Every probe and census figure in this entry was
  regenerated 2026-08-10 (Core #5); commands quoted where load-bearing.

  **THE DOCTRINE (final form — supersedes the NORTH STAR's four-properties-plus-three-missing-
  axes statement).** *An error is a value on one visible channel. It carries a typed CONTRACT —
  written where it can be depended on, inferred where it cannot. Its FLOW is marked at every
  site. Its HANDLING is provable, and shrinks what remains. Its HISTORY is recorded for free.
  Its PATHS have priced costs. And everything it tells a human, it tells a MACHINE as data.*
  Seven guarantees: **(1) typed contract** (payload-carrying error types; public sets written;
  set algebra so composition never taxes); **(2) visible flow** (D29 marks, and NO second
  channel — value-position auto-prop dead, implicit `From` dead, D26 silent auto-infer dead);
  **(3) provable, SUBTRACTIVE handling** (exhaustiveness + open/closed sets + residual
  subtraction + A35 no-silent-discard); **(4) recorded history** (A34 chain: automatic, free in
  release, rendered at the top by default); **(5) bounded multiplicity** (the channel is
  one-error BY DESIGN; N-error shapes are blessed DATA patterns, never a channel mode);
  **(6) priced paths** (zero happy-path overhead, no hidden allocation on the error path,
  provably infallible cleanup); **(7) machine-grade surface** (diagnostics are an API —
  guarantee 7 has the same standing as the other six). The errors≠faults split (Midori) stands
  unchanged beneath all seven; A33's supervised boundary stays the only aperture.

  **PIN 1 — NAMED ERROR SETS SPELL AS `type` ALIASES: `type AppError = IoError | ParseError |
  ConfigError`.** Zero new keywords — Gorget already owns the transparent/nominal split as
  `type`/`newtype` (`src/lexer/token.rs:294-295`; live at HEAD: `type Count = int`
  in `type_alias_usage.gg`, `newtype UserId(int)` in `newtype.gg`), and a named error set IS a
  transparent alias. Rules (sharpened by pass-2, then CORRECTED by pass-3 — the
  position-list rule had a case with no subject): the `|` union form denotes an ERROR-SET
  TYPE whose VALUES are ordinary values — errors are values (guarantee 1) — so a catch
  binding's union-typed value can be bound, stored, passed to a helper, and its type WRITTEN
  (via the alias or the inline `|` form) in any type position. What is CHANNEL-ONLY is
  INTRODUCTION: a union value is BORN only at a `throw` (injection at the throw site) and at
  widening (`!` marks and catch-transform `throw`s) — there is NO general injection:
  `AnyVal x = 5` is a type error and no constructor exists. THAT is the checker rule that
  closes the door to using error sets as a second general-sum spelling: you cannot CONSTRUCT
  one outside the channel, only observe, hold, and match what the channel produced (general
  sum types remain `enum`; the "multiple ways" anti-goal stands). Whether subset→superset
  re-tagging is legal at a PLAIN assignment (`AppError e2 = e1`, `e1` in a subset) or only
  through channel verbs is an E2 scout question. Member types are UNCONSTRAINED at v1
  (`throws int` / `throws String` are legal today and stay legal; pin 13's lint is the
  hygiene layer; whether a member predicate — e.g. equips `Error` — is wanted is an E2
  scout question). One more E2 scout question at the pin-7 seam (pass-5 catch): once sets
  reach `main`, which path does an escaping `int` MEMBER of a `main` error set take — the
  exit-code idiom or render+102? Decided BEFORE sets touch `main`, not discovered there.
  The alias is STRUCTURAL and
  TRANSPARENT: the name is documentation, a display anchor, and the semver anchor — never a
  nominal wall; nominal error wrappers stay available via `newtype`. Open sets compose the same
  way: `type NetFail = NetError | TlsError | ..` (open-marker glyph `..` vs `...` decided at
  the E2 grammar scout). Widening into a named set is tag injection — free, no user code —
  which is what kills the restate-or-wrap boundary tax (Java's collapse mechanism) that
  named-ENUMS-as-public-currency would have reintroduced the moment implicit `From` died.
  **Alternatives considered and rejected (recorded so they are not re-proposed):** `error set
  Name = …` — viable runner-up, self-documenting, also zero reserved words (the `on error`
  contextual-identifier precedent, `src/parser/stmt.rs:70-72`), but adds a declaration head for
  what `type` already expresses; `throws Name = …` — the head over-claims (these names also
  appear in `Result[T, E]` and catch patterns); `enum Name = …` — conflates nominal (new
  constructors) with transparent (referenced types), a reader cannot tell which; `ErrorSet[]
  Name = [A, B]` — types in value-literal position (a `Vector[int] v = [1,2,3]` declares a
  runtime VALUE with values on the RHS; this declares a TYPE with types on the RHS — the
  category the type-first style exists to keep crisp), and `ErrorSet[]`'s empty brackets mean
  nothing in the generics grammar.

  **PIN 2 — A31 AMENDMENTS RATIFIED.** Union spelling is `|` everywhere — the NORTH STAR's
  brace form (`throws {NotFound, Denied}`) is DEAD (annotated at that entry). Closed by
  default; an open marker is the opt-in to growth (glyph `..` vs `...` finalized at the E2
  grammar scout, per pin 1). Public sets are WRITTEN (checker rule keyed on D43
  visibility); inference applies interior-side with the decl-site `!` REQUIRED on every
  inferred-fallible non-public function (the D29(b) grammar-locked form `Config load(String
  p)!:` — flow visible at both ends, and the machine consumer reads fallibility off the decl
  line instead of running whole-program inference). ⚠ **PREREQUISITE (pass-3 catch): D43 is
  ratified but NOT IMPLEMENTED and currently sequenced behind the D27 chain — at minimum the
  public/non-public distinction must be ENFORCED before this rule has a subject** (at HEAD
  the default is public and the unit is the file, so "written-public" would degenerate to
  "every function writes its set" — the Java tax this pin exists to kill). The cross-chain
  ordering (D27 chain → D43's public/non-public minimum → E2) is the owner's sequencing
  call at round-open; recorded in SEQUENCING and the TODO E2 track. Set algebra pins: commutative, flattening,
  idempotent (`A|B ≡ B|A`, `(A|B)|C ≡ A|B|C`, `A|A ≡ A`); aliases expand transparently.
  Implementation = the sizing note's synthesized union enums with structural interning.
  **`gg semver-check` lands in the SAME round as closed-by-default** (public error-set diff:
  member added to a closed set ⇒ MAJOR; added behind the open marker ⇒ MINOR; removed ⇒ MINOR) — the tool
  is what makes the default livable (Elm-enforced, error-set-specific), not an add-on.

  **PIN 3 — IMPLICIT `From` AT `!` SITES IS DELETED.** Widening replaces conversion; no user
  code ever runs at a mark. The purity-gating candidate recorded with A34 is retired UNUSED —
  deletion, not gating. A conversion that does real work is spelled `catch (e): throw wrap(e)`.
  The wrapper-enum + `equip … with From[…]` teaching retires with it — doc set derived by
  CENSUS (`grep -rn "with From\[" docs/` — pass-7 catch: the earlier §6.2-only cite was a
  selection; the census also returns `language-design.md` §36.3, the flagship worked example,
  whose `!`-mark comment "IoError → ProcessError" teaches exactly the deleted conversion).
  Write-through is OWNED BY THE E2 TRACK (not by the tombstoned From-on-bare-`rethrow` TODO
  entry, which is deleted at E3).

  **PIN 4 — `rethrow` IS RETIRED: `throw` is the one raising form, `catch` the one handling
  form.** The channel surface becomes `throws` / `throw` / `catch` / `on error` / `!` —
  keyword count net −1 in the same package that adds sets. Evidence at HEAD (probe this
  session): `int v = f(x)! catch (e): throw "wrapped"` compiles and runs (success path prints
  the value; error path wraps into the enclosing channel) — `throw` already functions as a
  diverging catch body, so the keyword is pure sugar. Rationale: (a) pin 1 makes boundary
  conversion rare (widening is free), so the keyword serves a now-rare operation; (b) ONE
  checker path retires the per-form handler-defect class at the root (the void-`catch`
  miscompile, the unchecked recovery type, and the match-arm-throw ICE all lived on the split
  surface — Core #4; the recovery-type cell CLOSED at R40, between ratification and the
  rebase onto the R40 base — see the E0 note in SEQUENCING); (c) bare `rethrow 3`'s payload erasure becomes VISIBLE — `catch (_):
  throw 3` shows the discard, uniform with A35's acknowledgment principle (the top-level-
  silence finding stayed invisible partly because `rethrow` hid the discard); (d) A34's format
  trigger becomes "a `throw` that consumes a catch binding" — same information, still
  mechanical. Census 2026-08-10 (regenerate before acting): 107 mention lines across 41 files
  in `tests/fixtures` (`grep -rn "rethrow" tests/fixtures --include="*.gg" | wc -l` for the
  lines; `-rln` for the files), bare `rethrow N` exactly 2, 32 mention lines across `lib` +
  `docs/book` + the reference at ratification time (the fold's own D45 pointers joined that
  count the same day — regenerate, never inherit). Mechanics:
  the keyword is TOMBSTONED — token stays in the lexer, the parser emits a teaching diagnostic
  with a machine-applicable fix-it (`rethrow (e): X` → `catch (e): throw X`; bare `rethrow N`
  → `catch (_): throw N`), the first customer of A38's `gg fix`; the diagnostic code is never
  reused (A38 tombstone column). The `main throws int` exit-code idiom is PRESERVED, now
  spelled `throw N` / `catch (_): throw N`.

  **PIN 5 — HANDLER SIDE: `catch` GROWS `case` ARMS AND SUBTRACTS, AT MEMBER GRANULARITY.**
  `catch (e): expr` stays as bind-all sugar; the general form is `catch:` + case arms. Patterns
  are ORDINARY match patterns — probe this session: `catch (DbError.Timeout(ms)):` is a parse
  error at HEAD, so today partial handling has no direct spelling at all — with variants
  qualified per the existing rule (which also resolves duplicate variant names across members,
  `IoError.Other` vs `ParseError.Other`, with zero new rules). The member-level pattern is
  TYPE-FIRST: `case IoError e:` — a type-first declaration in pattern position, the signature
  idiom. **THE RULE (one sentence, normative): a catch block subtracts member `M` from the
  callee's set iff its arms cover `M` exhaustively; the residual — the union of not-fully-
  covered members — propagates and must be covered by the enclosing signature.** The type
  never narrows below a member (want variant-precise residuals? restructure the sets — make
  that variant its own member type). Exhaustiveness per member is decided by the ordinary
  match checker. **Variant-level narrowing is REJECTED for v1** (recorded so it is not
  re-proposed casually: it is unification-grade flow typing, hostile to the nominal system and
  to ggdef). `else` is required exactly when the matched set is open; a fully-covered closed
  set needs no channel at all (a fully-handled catch in `main` stays legal). D26 operator
  dispositions ride the SAME unified path — the operator-vs-call catch divergence (probe this
  session: `(big +! 1) catch (e): -99` in `main` → `E_UnhandledThrows`, a diagnostic
  recommending the exact spelling it rejects) dies with the unification.

  **PIN 6 — THE VALUE-POSITION AUTO-PROP HOLE IS KILLED (the two-doctrines conflict resolved
  by owner).** A bare `Result`-typed value in a `T` position is a TYPE ERROR; propagation
  happens at `!`-marked calls only. Fixture dispositions at E0 are OPPOSITE (pass-1 review
  correction — the first fold lumped them): `known_gaps/sound_autoprop_method_arg_rejected.gg`'s
  "INTENDED: both accept" header is OVERTURNED (pre-D29 doctrine) — under this pin BOTH call
  kinds reject the bare `Result` at an `int &x` param, and the fixture flips to a NEGATIVE.
  `known_gaps/sound_autoprop_indirect_bare_arg_skips_call.gg` is VINDICATED, not overturned:
  its callees take `Result[int, int] &x` — Result-typed positions, nothing to unwrap — and its
  INTENDED ("each callee runs") is exactly what the kill mandates; the bug it pins is the
  auto-prop machinery erroneously FIRING there, so it GRADUATES as a passing positive at E0
  (its header's PRESCRIBED fix — set `expected_type` on the indirect-call paths — is dead
  under this pin, for the reason the mechanism note gives; re-header at E0).
  **MECHANISM NOTE (pass-3 review correction — the machinery SURVIVES, re-gated; it is NOT
  deleted).** At HEAD `should_auto_propagate` (`src/ir/lowering/exprs/mod.rs:4530`) gates on
  the operand TYPE alone (Result + enclosing-can-propagate — no mark provenance exists at
  lowering), and the SAME path implements D23's RATIFIED marked-call peel (`Wrap(to_n(sel)!)`
  typing as `T` in every position) — pinned live by the positive family
  `throws_autoprop_{ctor_field,method_arg,binop_operand,dict_value,if_branch,list_element,
  match_arm,tuple_element}.gg`. Deleting the machinery wholesale would red that family and
  silently un-ratify D23. The E0 fix is TWO-SIDED: CHECK-side, bare `Result` in a `T`
  position = `E_TypeMismatch`; LOWERING-side, the peel RE-GATES on `!`-mark provenance
  written AT the mark as a typed field (Layering rule 4 — resolve once at the writer; never
  a second type test at the read site). Under the re-gate an unmarked position never peels —
  which is also what fixes the vindicated fixture (no mark at the indirect call ⇒ no peel ⇒
  the callee runs), independent of `expected_type`. E0's scout CENSUSES the corpus for
  bare-Result-in-`T`-position reliance BEFORE landing (the D29 precedent for a new breakage
  class): "pure defect closure" is the expectation, the census is the evidence.
  Prerequisite of A34 — the chain's premise ("the compiler knows every hop") is false until
  this lands.

  **PIN 7 — A34 SPLIT + THE EXIT CLASS.** **A34a lands at E0, and it CARRIES ITS ENABLING
  ACCEPT/REJECT CHANGE (pass-2 review caught the first fold assuming it silently): `main
  throws E` becomes LEGAL for ANY error type — `E_MainThrowsNonInt`
  (`src/semantic/typecheck.rs:9707-9714`; mirrored in the SH lane at
  `self_host_typechecker/typecheck.gg:3010`. The DOC write-through set is derived by CENSUS —
  `grep -rni "only throw.*int\|other than .int" docs/` — never by this list (pass-5 catch:
  a cited list is a selection); exemplars by SECTION ANCHOR only, no line numbers — these
  pointers drifted TWICE inside this very fold (pass-6 catch), which is what line numbers do:
  reference §10.6 "Throws on Main" (the most normative statement), §10.9, §3.4,
  `docs/book/02-types.md`, `docs/book/10-errors.md`. The EXIT-TAXONOMY write-through
  additionally sweeps the SPEC corpus BY CENSUS (pass-7 caught the docs-only scope; pass-8
  caught the replacement two-item list being itself a selection):
  `grep -rn "102\|one remaining non\|one nonzero NON\|four total outcomes" spec/` —
  exemplars: `spec/prose/trap-codes.md`'s "Exit-code context" block, `spec/ggdef/src/main.rs`'s
  header, `frontmatter.rs`'s "(102 is retired…)" comment — the literal negation of this pin —
  and `eval.rs`'s four-outcome `Outcome` model, the ggdef modeling site. The census also
  surfaces a REAL E0 LANE QUESTION, decided at E0 not discovered there: the conformance
  expect-block keys `trap:` ⟺ 101 and `reject:` ⟺ 1, so a compared exit-102 outcome needs
  its own frontmatter field and rule)
  RETIRES at E0**, all lanes per Core #9, its negative fixtures flipping. Without this the
  102 class has no reachable trigger — the only error that can escape `main` today is an
  int. Semantics: an error reaching the top of `main` renders `error: <Displayable of
  payload>` to stderr — one line, frozen grammar, the channel sibling of `trap[T_X]: … at
  file:line:col` — with `GG_ERROR_FORMAT=json` for machines, and the process exits **102**;
  `main throws int`'s escaping int KEEPS the exit-code idiom (the user chose the exit
  contract; whether it also renders in debug builds is an E0 track detail). The FULL
  taxonomy — total enumeration, amending the 2026-07-15 Option-A scheme in place (rider
  added there): **0 success · 1 static rejection · 2 usage · 101 trap + ICE · 102 uncaught
  channel error (NEW) · 103 ggdef fuel (outside the compared set)** — distinct classes are a
  machine API, and reusing 101 would re-blur errors≠faults at the one place a harness reads
  it. **THE NUMBER 102: owner-challenged ("why not 3?") and RECONFIRMED 2026-08-10 —
  recorded so it is not re-litigated.** No ecosystem has a consensus code for "uncaught
  error" (Python/Node/JVM exit 1, Go panic 2, Rust panic 101), so the scheme's
  "deliberately un-novel" principle resolves by extending the already-ratified family.
  Rejected: **1** — taken by static rejection; reusing it recreates the exact
  runtime-masquerades-as-compile-error hole this package kills; **3** (and the small-int
  band generally) — no Unix consensus meaning, MSVC `abort()` exits 3 on Windows (a FAULT
  smell on the wrong class), and `main throws int` hands the small-int band to the USER as
  their exit API, so reserved classes live away from it (a user can still `throw 102` and
  spoof the class — same caveat as Rust's `process::exit(101)`; distance minimizes the
  accidental collisions); **sysexits 65/70** — EX_SOFTWARE=70 is the bug face (our 101),
  EX_DATAERR=65 is a sendmail-era convention modern toolchains ignore, and half-adopting
  one number buys no recognition. 102 extends Rust's 101 into a self-describing band —
  10x = the language runtime ended the process; the digit says which face (101 bug, 102
  error, 103 ggdef fuel) — clear of the shell's 126/127/128+N territory. **A34b later:**
  the chain per the A34 design — static descriptor per `!` hop, format only at a
  throw-consuming-a-catch-binding, debug-only, and **NOT value-reachable in v1** (reachability
  would make it semantics, bind ggdef, and falsify the release-compiles-out promise) —
  sequenced after pin 6. The E0 freeze admits exactly ONE extension (pass-3 seam catch):
  A34b's chain renders BELOW the frozen first line as additional debug-only lines with their
  own grammar; the first line stays byte-stable across debug and release.

  **PIN 8 — D26'S SILENT AUTO-INFER RETIRES INTO GENERAL INFERENCE (at E2).** `+!` contributes
  `ArithError` to the inferred set like any fallible operation; the inferred-fallible function
  carries the decl-site `!` (pin 2); a PUBLIC function using `+!` writes `throws ArithError`
  like any other member. The bespoke closure-walker fix shape filed for the closure gap
  (`known_gaps/c1_d26_closure_body_no_auto_infer.gg`) is superseded — that gap dissolves in
  the general rule. Whether an ANONYMOUS closure carries pin 2's decl-site `!` is an E2 scout
  question (pin 2's subject is named non-public functions); the fixture stays, its INTENDED
  shape is re-derived from that answer, and it graduates at E2.

  **PIN 9 — ONE SET-ALGEBRA MODULE (Core #4 applied to the type checker).** Representation,
  interning, union, subtraction, membership are built ONCE in A31's implementation and
  consumed by A32's latent-effect slot, D26's `ArithError` contribution, the TaskGroup union
  join, and any future async row. There will never be a second implementation of error-set
  union in this compiler. Surface effect rows: NEVER (reaffirms the NORTH STAR's
  what-not-to-copy; the internal module is the non-speculative form of the effects question —
  if a real row lattice is ever warranted it grows out of this module, not beside it).

  **PIN 10 — A38 FILED: THE MACHINE-CONSUMER PACKAGE (guarantee 7 as doctrine, not backlog;
  largely semantics-free and parallelizable).** (a) Registry columns extend A37's to `{ code,
  default_level, configurable, group, since, fix_direction, tombstoned, summary, example }`,
  shipped as machine-readable data IN the toolchain (`gg explain <Code>` prints it); codes are
  permanent identifiers (tombstone, never reuse). (b) One code = one fix DIRECTION: split
  `E_MissingFallibleMark` BY FIX DIRECTION — at HEAD its `FallibleMarkReason` payload has
  FOUR variants, not three (`Bare` / `RedundantOnCapture` / `ResultArmsOnPeeled` /
  `MarkOnInfallible`, `src/semantic/errors.rs:331-348`; pass-2 review correction — census
  the enum at split time, never inherit this count), with fix directions insert-the-mark /
  remove-the-mark / capture-then-match / remove-the-mark; the split lands one code per
  DIRECTION (the two remove-mark reasons may merge; names finalized at A37 phase 2 since
  codes are cross-lane-compared values). The enum's stale "One code, two messages" doc
  comment was corrected in the same fold (Core #14).
  (c) Structured fix-its `{span, replacement, applicability}` with applicability ∈
  machine-applicable / choice / advisory — a machine-applicable edit must at minimum COMPILE;
  `gg fix --only=<code>` is the standing codemod vehicle; the `f()!=` maximal-munch trap gets
  a targeted `choice` fix-it ("`f()! != x` or `f() != x`?"), never a silent fmt rewrite.
  (d) `--diagnostics=json` (NDJSON, versioned schema, rustc's proven shape) + isatty detection
  + `NO_COLOR` + `--color=never` + byte offsets + secondary spans (`UnhandledThrows` spans the
  call AND the enclosing signature — today the second edit site is prose). (e) The check⇒build
  contract becomes a GUARD: a ratchet asserting `gg check`-accept ⇒ backend-compile success
  over the corpus, plus the harness greps build stderr for C-compiler ERROR lines (`.c:`
  co-occurring with `error:`) on every fixture — NOT bare `.c:` (pass-5 falsified that
  predicate at HEAD: a pre-existing `-Wmissing-field-initializers` leak prints 3 `.c:`
  WARNING lines on a two-line hello-world, so the bare grep is red-on-arrival everywhere;
  the warning leak itself is filed with the A38 track). A C ERROR reaching the user is BY
  DEFINITION an ICE (render `internal error` with the Gorget span, exit 101;
  `language-design.md:120` already promises this, now executable per Core #6).
  (f) Determinism pinned by fixture: same source ⇒ byte-identical diagnostics across runs and
  parallelism. (g) The runtime grammars are FROZEN API: the trap line and pin 7's error
  render. The payoff coupling this to A31: enumerable fix menus — on `UnhandledThrows` over a
  known closed set the compiler emits the complete `catch: case` skeleton as a `choice`
  fix-it, so an agent fills holes instead of inventing structure.

  **PIN 11 — MULTIPLICITY STAYS OUT OF THE CHANNEL.** Blessed DATA patterns —
  `TaskGroup.join_all() → Vector[Result[T, E]]` and the accumulate-Diagnostics shape (the
  self-host's own) — get a named book section at E1; the channel never grows an N-error mode.
  **TaskGroup policy (PROPOSED, pending concurrency-round dogfood; E2's design must not
  foreclose it):** fail-fast — first error cancels siblings; `group.join()!` throws the UNION
  of the tasks' sets (free under pin 2); sibling errors and cancellation outcomes are recorded
  in the A34 chain (HISTORY absorbs multiplicity, the TYPE stays one union member); faults
  surface only at the A33 join boundary.

  **PIN 12 — THE COST ANNEXE (guarantee 6; spec'd at implementation).** The happy path of `!`
  is a branch on a tag and nothing else; the error path allocates nothing beyond the payload
  the user constructed; `on error` blocks are CHECKED infallible (a throwing cleanup is a
  compile error — the Midori/Rust-drop lesson made static); the release-mode chain is zero
  bytes. **Round owner (pass-4 catch — a ratified reject class must not be unowned): the
  `on error`-infallibility check is a NEW accept/reject rule and lands at E3** (the
  handler-surface round), with its negative fixtures, all lanes per Core #9; filed in the
  TODO E3 track. "Provably no-alloc error path" as a user-checkable property is research —
  filed (the `@[no_alloc]` annotation item in `TODO.md`), not blocking.

  **PIN 13 — `stringly-error` LINT.** Public `throws String` (or `String` as a member of a
  public set) defaults WARN — the book's demotion of `throws String` made mechanical
  (Core #6). DEPENDENCY (pass-3 catch): A36's lint-level machinery is NOT IMPLEMENTED — at
  E1 this ships as a registry warning that RENDERS ITS CODE (A36(1) is a one-function change
  and rides along), gains per-lint configurability when A36's D42-generalization lands, and
  is NEVER a bespoke per-feature flag (the trap A36 exists to retire). Interior/prototyping
  use stays clean; `String` remains a legal set member for uniformity.

  **SEQUENCING (each round per the Round lifecycle; Core #9 all-lanes; ggdef models union
  identity and subtraction where in-subset — real spec work — and stays structurally blind to
  the memory face per Core #13):** **E0** "kill the fictions" (pure defect closure; its scout censuses bare-Result-in-`T`
  corpus reliance first, per pin 6): pin 6 +
  the handler cells (void-`catch` + match-arm-throw ICE; axis-complete net per Core #12 —
  the THIRD cell, the `catch` recovery-type check, CLOSED between ratification and the
  rebase onto the R40 base: R40's stale-scan landed both reject cells + the SH port and
  graduated `catch_recovery_type_unchecked` to a live NEG, see DONE.md R40; its residuals
  stay on their own filed entries — the SH ECatch diagnostic-emission mirror and the ggdef
  catch-subset gap) + A34a/exit 102 + the munch-trap fix-it, ∥ the semantics-free A38 subtracks.
  **E1** "the model gets real users": the D17 class sweep + the 59 runtime `exit(1)`
  retirements + the book ch.10 FIRST pass (against the surface AS SHIPPED AT E1: real
  stdlib; the `catch (e): throw …` idiom, legal at HEAD; no `From`-wrapper; `throws String`
  demoted) + the stdlib LEAF-enum taxonomy (`IoError`/`ParseError`/`NetError`…) + pin 13 +
  pin 11's book section — set-alias syntax does not parse until E2, so composite-API set
  aliases and the book's set/subtraction teaching are the chapter's SECOND pass at E2/E3;
  E1 must not teach syntax that cannot run (the exact defect the book entry files; pass-2
  review caught the first fold doing it). **E2** "sets": pins 1–3/8/9 +
  `gg semver-check`, one scout under the existing A31 sizing challenge with these pins as the
  spec. **E3** "subtraction": pins 4–5 (the catch grammar + the tombstone/codemod migration)
  + pin 12's `on error`-infallible check (a new reject class — negative fixtures, all lanes).
  **E4** "history + concurrency": A34b + pin 11's TaskGroup + A32 impl consuming the module.
  Honest sizing: E2+E3 is 2–3 rounds of type-system work. **Sequencing rule: E2 does not open
  before E0+E1 land** — sets composing signatures that do not exist would be building the
  second floor first — **and E2 additionally requires D43's public/non-public distinction
  IMPLEMENTED (pin 2's prerequisite; D43 currently sits behind the D27 chain — the
  cross-chain ordering is the owner's round-open call).**

  **DERIVATION + WHAT WAS GIVEN UP (priced honestly).** The v2 iteration closed four gaps in
  the adversarial review's own §4 design: the boundary wrap tax (pin 1), unspecified
  subtraction granularity (pin 5), the unpinned exit class (pin 7), and the doctrine-as-
  patches statement (the seven guarantees). Given up: surface novelty (the mechanism is OCaml
  polymorphic variants + Swift marks + Zig named sets/errdefer + Midori's split, integrated —
  cite all four); resumable handlers (never); variant-level narrowing (v1); accumulation in
  the channel (never — it is a data pattern, pin 11).

- 2026-08-10 — **🎯 FILL-PACK ARGUMENT WRAPPING RATIFIED (owner, at R41 planning): when a
  horizontally-broken list exceeds the width limit, `gg fmt` FILLS each line with as many
  elements as fit before breaking to the next — never one-element-per-line explosion.**
  Owner's words: "fill long lines before breaking to the next." Scope (fn signatures, call
  args, tuple/collection literals, generic-arg lists — every horizontally-broken list),
  width (100 per the 2026-08-09 ruling), the `doc::group_fill` implementation sketch,
  reference precedents (rustfmt `Compressed`, Prettier `fill()`), and the fixture plan live
  in the filed TODO entry (Tooling/CLI, "fill-pack long argument lists"); the track's scout
  settles trailing-comma-in-fill and the per-list-kind cells within this principle.
  **SEQUENCED PRE-A2 (R41): canonical-output changes land BEFORE the bulk `!`→`^` sweep so
  the ~1,600-file corpus canonicalizes ONCE** — deciding it post-sweep would re-churn the
  corpus; this supersedes the TODO entry's earlier "NOT blocking any current work" note.
  Small formatter track (T-FP): own scout→brief→≥3 reviews→executor; gates = per-kind
  fixture pairs (short-fits-inline vs long-must-wrap) + `fmt_idempotent` + the A1
  scratch-tree gate + bootstrap. ⚡ Width amended same day: 120, not 100 — see the FMT CANON
  PAIR entry below.

- 2026-08-10 — **🎯 MULTI-LINE STRING LAYOUT RATIFIED (owner, from gorget-arena snag #4;
  recorded 2026-08-10 after plan-review pass 6 caught the ruling living only in TODO prose
  while `appendix-formatting.md:85-97`'s "canonical, not verbatim" doctrine has no multi-line
  carve-out):** `gg fmt` PRESERVES the physical line layout of a multi-line `"""` string —
  owner's words: "Expected: preserve the physical line layout of multi-line string literals."
  NARROW ruling: it governs PHYSICAL NEWLINES in the `"""` form only; the wider
  escape-canon-vs-verbatim question for single-line strings remains OPEN as owner question Q1
  (T-FMT-B's gate). The appendix gains the multi-line carve-out as this ruling's write-through
  WHEN the fix lands (same track); FIXTURES AND LIB SOURCES carrying physical `"""` blocks
  (`multiline_strings.gg`, `string_multiline_ops.gg`, `lib/xtd/toml.gg` — the pass covers the
  WHOLE sweep root set, not `tests/fixtures/*.gg` only; pass-8 wording fix) join the A2
  subject-preservation pass.

- 2026-08-10 — **🎯 FMT CANON PAIR RATIFIED (owner, at R41 planning; owner invited
  objections, orchestrator raised none):** **(1) MAX LINE WIDTH = 120** — `gg fmt` breaks a
  line only past 120 columns; SUPERSEDES the 2026-08-09 100-char ruling; composes with
  fill-pack (fill to 120, then wrap). **(2) TRAILING-COMMENT GAP = 4 SPACES** — a LONE
  inline comment starts exactly 4 spaces after code end (measured current behavior: 2 —
  this changes it); consecutive-comment GROUPS keep R40's STRIDE=4 alignment with the
  4-gap as the anchor/floor (group column = max code end in group + 4; exact stride
  rounding = T-FMT scout detail). The owner's "most of them are already like that" matches
  the measured group-alignment output — the change is the lone-comment gap and the floor.
  Both land in the PRE-A2 fmt-hardening wave (canonical-output changes precede the sweep).
  **A2 DELIVERY REQUIREMENT (owner, same session): the bulk sweep lands as ONE commit
  containing ONLY the `gg fmt` pass — no fixes mixed in — so the diff is analyzable for
  suboptimal format changes**; T-FMT fixes + fill-pack + this canon pair land in separate
  PRIOR commits, and the A2 track delivers a hunk-classification summary beside the commit
  to guide that analysis.

- 2026-08-10 — **🎯 SUITE-LAYOUT FORM PRESERVATION RATIFIED (owner, from a gorget-arena
  diff): `gg fmt` NEVER collapses a next-line suite or arm body onto its header line — the
  author's break is preserved.** Owner's words: "keep the original style and leave it on the
  next line." Measured violating cell (reproduced + cell-mapped same day, fresh binary): an
  EXPRESSION-POSITION match's `else` arm with a single-expression body collapses
  (`else:` + next-line `^b` → `else: ^b`) and gains an inserted blank line, while the
  sibling `case` arms preserve — inconsistent within one construct; statement-position
  matches unaffected; the mangle is idempotent (stable, not oscillating). Same
  form-preservation family as R40's radix / comment-blank / escape rulings. Fix rides R41's
  PRE-A2 fmt-hardening wave (with the fill-pack track) — a canonical-output mangle must die
  before the bulk sweep bakes it into the corpus. Filed with repro in TODO (Tooling/CLI,
  beside the fill-pack entry).

- 2026-08-11 — **🎯 FMT FORM-PRESERVATION TRIO + RENDER FALLBACK + R41 SCOPE RATIFIED (owner:
  "Let's go with your recommendations. I tend to agree on all", after a field survey across
  gofmt/rustfmt/Black/Prettier/zig-fmt/clang-format + Rust/Swift/Python/Java top-level error
  handling).** **(Q1) LITERAL FORM = VERBATIM:** `gg fmt` never decodes escapes, changes
  radix, or rewrites a literal's value-spelling when the span round-trips — the field is
  unanimous (no major formatter does; Black/Prettier touch only casing and quote marks).
  SUPERSEDES `appendix-formatting.md` §85-97's "canonical, not verbatim" doctrine, which
  T-FMT-B rewrites as its write-through; subsumes the byte/char/float/`\u`/f-string classes.
  **(Q2) SUITE LAYOUT = SYMMETRIC PRESERVATION:** fmt neither collapses NOR explodes the
  author's inline-vs-next-line suite choice — completes the 2026-08-10 never-collapse ruling;
  the author-signal precedent (Prettier's object rule, gofmt's statement preservation) chosen
  over Black's force-expand for internal consistency with the ratified form-preservation
  family. **(Q3) SYNONYM PAIRS = PRESERVE:** `byte`↔`uint8`, set-literal↔vector, await forms,
  `with`-as, `public`, static-insert, paren churn — the field's division of labor is
  structural (formatters format; construct choice is clippy/`eslint --fix`/`go fix`
  territory): canonical synonyms, if ever wanted, are a `gg fix` rule, NEVER `gg fmt`.
  Q1 PRECEDES Q3 on overlapping hunks. **(Q5) TOP-LEVEL RENDER FALLBACK = `display()` else
  `debug()`** — no `E: Displayable` requirement (Rust's Debug-require rejected: Displayable
  is derive-only here, so it would import Rust's mechanism without Rust's ubiquity and mint a
  fresh reject class; the Swift/Python/Java always-renderable shape wins; pin-1-consistent).
  UNBLOCKS E0c-102 (the `E_MainThrowsNonInt` retirement + the whole exit-102 surface).
  **(SCOPE) R41 keeps full W2 scope** + a clause-(a)-sized T-PRUNE + the honest close-time
  ask if short. **The formatter's contract is now ONE SENTENCE: `gg fmt` changes layout it
  owns and nothing the author spelled.**
- 2026-08-11 — **🎯 FILL SHAPE + ELIF PAIR RATIFIED (owner, live session, on the
  orchestrator's recommendations after the T-FMT-D pass-2 / T-FMT-B pass-2 evidence).**
  **(1) FILL-PACK CONTINUATION = BLOCK INDENT** — a wrapped list's continuation lines
  start at one indent level, never aligned under the open delimiter. Grounds put to the
  owner and accepted: the 2026-08-10 ratification covered only the packing principle
  ("fill long lines before breaking to the next" — the paren-aligned TODO example was
  agent-authored paraphrase); paren-alignment measured architecturally unsound with the
  formatter's pre-rendered elements (absolute sub-render columns ⇒ nested fills
  mis-column) and rename-fragile; rustfmt `Compressed` and Prettier `fill()` both
  block-indent. Supersedes the visual example embedded in the fill-pack TODO entry.
  **(2) `else if` ↔ `elif` JOINS the Q3 PRESERVE family** — fmt keeps the author's
  spelling; the Q3 pair list is thereby confirmed extensible by the contract sentence
  ("`gg fmt` changes layout it owns and nothing the author spelled"), and
  `appendix-formatting.md`'s use of `elif` as the canonicalization example is rewritten
  by T-FMT-B's write-through. Both had been derived-and-flagged; both are now owner
  calls, closing the last open formatter-canon knobs before the A2 sweep.
- 2026-08-11 — **🎯 ELIF RULING SUPERSEDED SAME-DAY (owner, live session, after a
  requested recommendation): `else if` → `elif` CANONICALIZATION STANDS — the pair
  LEAVES the preserve family.** The owner reopened the morning's preserve ruling; the
  accepted principle: the Q3 preserve family covers spellings that carry AUTHORIAL
  INFORMATION (radix, escape form, `byte`, set-literal, await style, `with`-as,
  `public`, parens); a pure keyword synonym with ZERO information content is
  formatter-owned, and `elif`↔`else if` is the only member today. Canonical spelling =
  **`elif`** (owner considered promoting `else if`, decided against on the evidence:
  the corpus is unanimous — 4 `else if` sites in ~2,700 files; the surface is
  Python-first; the book/appendix already teach `elif`; and elif-canonical is today's
  shipped fmt behavior, zero code change). The parser KEEPS accepting both (the
  D27-style welcome: accept muscle memory, format to house style). The contract
  sentence gains its one documented carve-out, stated as the intent-content principle
  above, not an ad-hoc exception. Supersedes clause (2) of today's "FILL SHAPE + ELIF
  PAIR" entry; `appendix-formatting.md`'s elif example remains TRUE and stays.
- 2026-08-11 — **🎯 VISIBILITY-ON-EQUIP/TEST/BENCH = REJECT (owner, live session, on
  the T-FMT-B pass-4 find that `public equip Foo:` is accepted and silently
  DISCARDED at parse — a Core #10 silent-drop).** `public`/`private` on an `equip`
  block, `test`, or `bench` is a PARSE-TIME REJECTION with a teaching diagnostic
  ("visibility goes on the methods inside the equip block, not on the block") —
  the Rust E0449 shape; an equip block exports nothing of its own and per-method
  visibility already exists. The Swift-style member-default (`public extension`)
  remains an ADDABLE D43-era extension, deliberately not chosen now. Accept/reject
  change ⇒ Core #9: lands cross-lane with a NEG conformance fixture + the SH parser
  mirrors, same round as its fix (a small W2 rider, not the fmt track). SEPARATE
  axis — RULED 2026-08-16, see below: ATTRIBUTES above `extern`/`equip` blocks.
- 2026-08-16 — **🎯 ATTRIBUTES ABOVE `equip` / `extern` BLOCKS = REJECT (owner, live
  session; closes the h5 knob the 2026-08-11 entry left open).** An attribute written
  above an `equip` or `extern "C":` block is a PARSE-TIME REJECTION with a teaching
  diagnostic, joining the h1-h4 reject-rider (same mechanism: `parse_item` consumes
  the modifier/attribute list, then dispatches to sub-parsers that take neither, so
  the annotation is discarded before any later pass — `gg fmt` included — can see it;
  a Core #10 silent-drop). Rejecting makes the fmt-deletion defect impossible by
  construction rather than fixing it downstream. **The `@link`-as-missing-CARRY-feature
  premise the earlier entry recorded is WITHDRAWN as unfounded, measured 2026-08-16:**
  `@link` is not a Gorget feature and never was — it occurs exactly ONCE tree-wide, in
  the formatter fixture `tests/fixtures/fmt_form_preservation/name_string_attr_string.gg`
  where the name is merely a vehicle for testing escape round-tripping inside an
  attribute argument; the manifest has no link directives; and native linking is
  hardcoded in `src/main.rs` (`-lm`, `-ldl`) with SQLite bundled into the runtime so
  users need no external flag. The language reference specifies attributes only on
  function / struct / enum / trait definitions (`@test`, `@inline`), never on blocks.
  If real FFI link directives are ever wanted, their home is the manifest
  (`gorget.toml`, D44) — linking is a build concern, not a source annotation — and
  that is a feature proposal, not this axis. Accept/reject change ⇒ Core #9: NEG
  conformance fixture + SH parser mirrors, same round as the fix.
- 2026-08-16 — **🎯 `case` NULLARY-VARIANT PATTERN SPELLING = PRESERVE THE AUTHOR'S
  (owner, live session, at the A2 style review).** `gg fmt` must not rewrite
  `case X():` → `case X:` (nor the reverse, nor the qualified forms): `X()` stays
  `X()`, `X` stays `X`, `Color.Red` stays `Color.Red`, `Color.Red()` stays
  `Color.Red()`. An explicit `()` mandate was considered and WITHDRAWN. Rationale —
  a formatter may canonicalize two spellings only when they are semantically
  identical in ALL cases (the `elif`/`else if` test); these are identical only when
  the name resolves to a variant of the scrutinee, and when it does not, one is a
  constructor pattern that should fail to resolve while the other is silently a
  variable binding, i.e. a catch-all. Measured: the A2 sweep rewrote 912 lines /
  109 files, of which 790 changed the AST node kind (`Pattern::Constructor` →
  `Pattern::Binding`) and 38 became `Pattern::Literal`; only the 84 qualified sites
  were AST-identical. Same class of harm as author-paren deletion (R42 Track D), so
  shipping both in one release would have been incoherent. The bare form stays legal
  and documented; what the spec still OWES is the resolution rule and its failure
  case — the docs never state what a bare name means when it is NOT a variant of the
  scrutinee, nor which scope resolves it, and that silence is what let Rust gg and
  ggdef diverge on the reference's own example. The two related questions that entry
  left open are RULED in the next entry.
- 2026-08-16 — **🎯 A2 STYLE REVIEW: FIVE FORMATTER CANON CALLS RULED (owner, live
  session).** All five ride ONE regeneration of the A2 sweep, sequenced after the
  case-spelling fix.
  **(1) KEYWORD-ARGUMENT SPACING = SPACED — `f(a = 1)`. ⚠ REVERSED SAME DAY; the
  first ruling (tight `f(x=1)`) is WITHDRAWN and rested on a PARTIAL reading of PEP 8.**
  PEP 8 is not one rule here but TWO, discriminated by ANNOTATION: it says omit the
  spaces "when used to indicate a keyword argument, or … a default value for an
  UNANNOTATED function parameter", and then explicitly "**however, if a parameter has
  an annotation, use spaces around the `=` sign**" (`def munge(sep: AnyStr = None)`).
  Gorget is TYPE-FIRST — every parameter is annotated, always — so PEP 8's own
  discriminator selects the SPACED branch, and the corpus already agrees
  (`String greet(String name = "world")`, `int f(int x = boom())`). Tight call sites
  would split the language's own `=` spacing between declaration and call; Python
  escapes that only because its unannotated defs are tight too. The
  reads-like-an-assignment rationale is also weak here: introducing a variable in
  Gorget is type-first (`int x = 1`) and there is no assignment-as-expression, so
  `x = 1` inside a call can only be a named argument. Neighbours agree — among
  languages spelling named arguments `name = value`, Kotlin
  (`drawSquare(sideLength = 10, …)`) and Scala both use spaces; C#/Swift use `label:`
  and Ada `=>`. NET EFFECT: the formatter's existing spaced behaviour is correct and
  this canon call needs NO code change (it leaves R42 Phase 4b). ⊕ Doc defect to fix in
  the write-through: `docs/language-reference.md:5032` spells `cap=n` tight, against
  the spaced form used elsewhere.
  **(2) BLANK LINES = PRESERVE-AND-CAP, IN EVERY MEMBER CONTAINER.** The rule already
  ratified for top-level items on 2026-08-09 (preserve AUTHOR-written blanks; collapse
  runs of 2+ to exactly one) now governs `trait`, `equip` and `extern "C":` bodies too.
  This REPLACES two opposite-direction behaviours the sweep exposed: unconditional
  INSERTION of one blank between `trait`/`equip` members (+192 measured) and DELETION
  of author blank lines separating comment-headed groups inside `extern "C":` (−117
  measured, entirely comment-adjacent — the members themselves were always packed,
  `format_extern_block` has no blank emitter at all). Reference point: gofmt, rustfmt
  and Black all preserve-and-cap; none manufactures blanks and none deletes a single
  separator.
  **(3)+(4) THE MAGIC TRAILING COMMA (Black's rule) IS ADOPTED.** A trailing comma in
  a list is the AUTHOR'S SIGNAL to keep that list exploded; its absence means the
  formatter may pack. This replaces BOTH the vertical collapse of hand-aligned literals
  (61 hunks / 10 files, every 4×4 matrix in `math3d.gg`) AND the absence of any
  trailing-comma canon (today the formatter deletes the comma when packing and adds one
  when exploding, so the token carries no meaning). One mechanism, two calls: the
  trailing comma stops being noise the formatter shuffles and becomes explicit,
  greppable author intent.
  **(5) `compiler/data/resources.gg` = NO SPECIAL RULING; RE-MEASURE after (2)+(3).**
  The hand-built data table lost its record structure (3130 → 1106 lines, 353 → 12
  blank lines; each `ResourceEntry(...)` was exploded so related fields group per line
  and the sweep packed them to the 120 budget). Most of that is downstream of (2) and
  (3); what survives them is the honest signal that this file wants a `# fmt: off`-style
  escape rather than an exception carved into the canon.
  **(2-bis) BLANK LINES INSIDE AN EXPLODED CONTAINER = PRESERVE-AND-CAP TOO (owner,
  2026-08-16, folded at the scout's measurement).** Ruling (2) extends from member
  containers to multi-line list literals: blank lines the author wrote BETWEEN elements
  of an exploded container are preserved, runs of 2+ collapsed to one. Measured on
  `compiler/data/resources.gg`: 3130 lines / 353 blanks today → 1106/12 under the
  current canon → 1139/12 with the other six fixes → **1468/341 with this rule**, which
  is the only variant that restores the file's paragraphing. Same principle, same
  mechanism, one more position.
  **(6) IMPORT NAME LISTS = PRESERVE AUTHOR ORDER.** The alphabetical sort of imported
  member names is REMOVED (both `from X import a, b, c` and `import X.{a, b, c}`;
  `src/formatter/mod.rs:3183` and the `ImportStmt::From` twin at `:3219`). It destroyed
  deliberate reading order — `CollectionKind, CkNotCollection, CkVector, …` became
  `CkDeque, …, CollectionKind`, moving the enum TYPE from the front of its own variant
  list to the end. Prior art was surveyed and mandates nothing here: isort and rustfmt
  sort flat (rustfmt's only privileged item is the `self` keyword, not a semantic
  relation); gofmt sorts import PATHS and Go has no member lists; Prettier and Black do
  not sort members at all. The languages that care about type-with-variants express it
  in SYNTAX rather than in a sort — Haskell's `import Data.Map (Map(..))`, Elm's
  `exposing (Msg(..))` make the type and its constructors ONE import item. A
  semantic sort is also not implementable here without a layering inversion: the
  formatter would need to resolve names across modules to know `CkVector` belongs to
  `CollectionKind`. ⊕ Removing the sort also CLOSES a filed defect documented at the
  sort site: because sorting makes emitted order differ from source order, the
  forward-only comment cursor cannot keep a comment with its name, so "a comment inside
  a grouped import still leaves the group". POSSIBLE FUTURE, filed separately, not part
  of this: a bundled import form (`from schema import CollectionKind(..)`) that makes
  the relationship expressible.
- 2026-08-17 — **🎯 THE ATTRIBUTE-POSITION RULE, in one sentence (owner, live session):
  ATTRIBUTES ATTACH TO DECLARATIONS, NOT TO GROUPINGS.** A construct that introduces a
  NAME can carry an attribute — function, method (INCLUDING an equip method), struct,
  enum, trait, `static`, `const`, type alias, newtype, `test`, `bench`. A construct that
  only GROUPS other declarations and introduces no name of its own — `equip …:` and
  `extern "C":` — cannot; attach to the members instead. **Every position either HONOURS
  an attribute or REJECTS it with a teaching diagnostic; the silently-dropping set is
  empty BY CONSTRUCTION** (measured 2026-08-17: 14 of 22 positions silently drop today,
  which is the defect this rule retires).
  **This SUPERSEDES the case-by-case treatment and subsumes h5** (2026-08-16, attributes
  above `equip`/`extern` blocks = parse-time REJECT), which stops being a special case
  and becomes a CONSEQUENCE — an `equip` block exports nothing of its own, exactly the
  reasoning h5 already used for visibility. It also RESOLVES a self-contradiction the
  orchestrator introduced the same day by unilaterally ruling the `equip`-block position
  supported (clause (e) of the `@fmt(skip)` entry, now withdrawn); that call reversed a
  ratified owner ruling and should have been an owner ask.
  **Consequence for `@fmt(skip)`:** it works on equip METHODS and every other declaration
  — which is what the corpus needs, since all 16 `math3d.gg` markers are methods, and
  method granularity freezes less formatting than a whole block would.
  ⚠ One position the rule does not obviously settle: `import` / `from … import`
  introduces names from ELSEWHERE rather than declaring them. Decide it explicitly when
  the class guard lands rather than letting it fall out of an implementation detail.
  Accept/reject change ⇒ Core #9: all three lanes + cross-lane NEG conformance fixtures.
- 2026-08-17 — **🎯 THE `for x in xs` IDIOM: GUIDANCE SHIPS AS A REPO LINT + THE BOOK;
  NO WARN-BY-DEFAULT COMPILER WARNING, "at least not yet" (owner, live session).**
  The broad "prefer `for-in` over an index loop" guidance is STYLE, and style
  warnings in the compiler devalue the whole warning channel — once readers scroll
  past style output they scroll past the probable-defect warnings too. Every
  neighbouring language keeps this class outside the compiler: Go ships no warnings
  at all, Rust's `rustc` warns conservatively while style lives in the OPT-IN clippy
  (and the exact analogue, `needless_range_loop`, is a clippy lint, not a compiler
  warning — warn-by-default for eleven years and still carrying open issues where its
  suggestion does not compile or silently deletes a panic), TypeScript defers to
  ESLint, C# to IDE-level configurable severity.
  **The counter-argument, recorded because it is real and may reopen this:** a repo
  lint reaches only this corpus, while agents and newcomers writing Gorget never run
  it — so the compiler is the ONLY channel that teaches the language's own idiom to
  the people the LLM-correctness goal is about. That asymmetry does not apply to most
  languages and is why "not yet" rather than "no".
  **A NARROW, DEFECT-SHAPED subset remains a legitimate future warning** and is
  explicitly NOT what this ruling refuses: `.get(i).unwrap()` where `i` is bounded by
  that same collection's `len()` is provably infallible — dead error handling that
  lies to the reader about a failure that cannot occur. Mechanically detectable,
  unambiguous fix, near-zero false positives. That is a probable-defect diagnostic,
  not a style one.
  **BLOCKER on any compiler diagnostic, either shape: there is no statement-level
  suppression.** Attributes attach to ITEMS, so the only "I meant this" is
  function-granularity — one legitimate byte-cursor would silence the diagnostic for
  its whole function, exactly backwards. Adding `@allow(...)` at statement level is
  new syntax AND reopens the same-day ruling that closed the `@fmt(...)` family at one
  member. Settle suppression before revisiting warn-by-default.
  ⚠ **Sequencing constraint on the ruled work itself (measured, not assumed):** the
  lint must not precede the TYPE HOLES, or it fires where no legal idiomatic spelling
  exists; and the book must not teach the idiom while its MUTABLE form double-frees
  (the CRITICAL filing of the same day) or while it teaches `zip(a,b)` /
  `enumerate(c)`, neither of which exists as a free function. Fix-first, promote-second.
- 2026-08-16 — **🎯 `@fmt(skip)` ON ITEMS = RATIFIED, AND THE FAMILY IS CLOSED AT ONE
  MEMBER (owner, live session).** The formatter escape hatch is an ATTRIBUTE, not a
  magic comment: `@fmt(skip)` above an item leaves that item's source emitted
  BYTE-FOR-BYTE. **Why an attribute beats `# fmt: skip`:** Gorget already validates
  attributes against an allowlist and rejects the rest (`E_UnknownDirective`,
  `src/semantic/mod.rs:165-219`), so a typo'd `@fmt(skpi)` FAILS THE BUILD while a
  typo'd `# fmt: skpi` silently does nothing and is discovered only when a sweep
  quietly reformats the thing it was meant to protect — the silent-no-op shape this
  round ruled against three times. Black / Prettier / clang-format use magic comments
  because Python, JS and C++ have no lightweight validated annotation at those
  positions; Gorget does, and rustfmt — the language shaped like this one — uses it
  (`#[rustfmt::skip]`). **Granularity is sufficient, measured:** both real cases are
  item-granular — `compiler/data/resources.gg`'s table is a top-level
  `public static Vector[ResourceEntry] RESOURCES = [...]`, and `lib/xtd/math3d.gg`'s
  4x4 matrices sit inside methods (`Mat4 identity():`) whose entire body IS the matrix.
  Nothing needs to reach an expression, so the grammar is not extended past items.
  **THE FAMILY IS DELIBERATELY CLOSED AT ONE MEMBER.** `@fmt(explode)` / `@fmt(pack)`
  are REJECTED as a category: they are STYLE OPTIONS, not escapes, and they are also
  redundant — the MAGIC TRAILING COMMA ratified the same day already expresses
  explode-vs-pack, in the code itself rather than in metadata above it; a second
  spelling for one meaning is what the one-canonical-way rule forbids (the rule that
  retired `pow()` for `**`). `@fmt(width=...)` and any per-file configuration are
  refused for the gofmt reason: strictness is the feature, and every knob is a
  bikeshed plus a fragmented corpus. The closure is ENFORCED, not merely stated — an
  unratified `@fmt(align)` is rejected by the existing directive allowlist until
  somebody deliberately adds it. Deferred, not chosen: a REGION form (`@fmt(off)` /
  paired markers) — the next-item form covers both known cases, and unbalanced region
  markers fail silently; revisit only if a genuine multi-item case appears.
  **Scope of work — ⚠ CORRECTED 2026-08-17 by the implementation scout; the original
  paragraph under-scoped this in one load-bearing way.** (a) **EQUIP METHODS ARE
  REQUIRED, not a "verify" footnote:** every one of `lib/xtd/math3d.gg`'s 41 sweep hunks
  is inside an equip METHOD and not one is a top-level item, so a top-level-only
  implementation covers **0%** of that target. Equip-method support is the harder half
  and carries the round's only self-host change (~11 lines in `parse_equip_item`, in 3
  real `parser.gg` copies; unmarked it yields 2 parse errors plus a phantom
  `void fmt(skip)` method — measured by running a self-host driver). (b) **THE
  SILENT-DROP SURFACE IS 14 OF 22 ATTRIBUTE POSITIONS** (corrected 2026-08-17 by the
  brief gauntlet, which probed all 22 at HEAD: 6 REJECT / 14 silently dropped / 2 parse
  error, `import` and `from … import` being two separate dropping arms; the scout's
  "12 of 19" undercounted), not just `static`/`const`:
  trait, bench, equip method, equip block, static, module-var, const, type alias,
  newtype, extern block, import, suite and meta items all accept an attribute and
  silently discard it — `@fmt(skip)` on a type alias would have BEEN the silent-no-op
  bug this feature exists to prevent. The class fix is CHEAPER than per-position AST
  fields and directly discharges the h5 ruling: ONE `reject_dropped_attributes` guard at
  `parse_item`'s dispatch makes the drop set empty by construction (corpus census shows
  zero conflicts, so it can be fatal on day one). (c) **The skip RE-INDENTS — it is not
  a raw byte copy.** Interior layout byte-exact, indentation level canonical: a raw copy
  emits files that DO NOT RE-PARSE when the source indent differs from canonical (`gg
  check` rejected the formatter's own output in the prototype). (d) The whole-buffer
  blank-line normalizer (`src/formatter/mod.rs:910-927`) collapses real double blanks
  inside a skipped span — it needs protected byte ranges. (e) ⚠ **WITHDRAWN 2026-08-17 — it contradicted the
  same-day h5 ruling.** The orchestrator had ruled the `equip` BLOCK position supported;
  h5 rejects attributes there. h5 STANDS, and the unifying rule below supersedes both. ggdef needs nothing
  (it reuses the Rust parser, never reads attributes). Canonical printers do not shift.
  ⊕ Measured end-to-end on the prototype: `resources.gg` 3130→1106 lines unmarked
  becomes 3132→3123 with 67,667 bytes of table BYTE-EXACT; all 16 marked math3d methods
  byte-exact; both idempotent. ⚠ Marking an item BLINDS every test asserting on its
  swept form — `COLLECTION_BUILTIN_METHODS` must NOT be marked (it drops
  `fmt_resources_gg_comment_positions_preserved`'s indent-12 comment count 15→0, and
  that guard's own doc says NEVER LOWER THIS FLOOR); the correct marked set is 2 statics,
  not 6. "Layout changed" is not "layout damaged".
  Accept/reject change ⇒ Core #9: all three lanes + cross-lane conformance fixtures.
  **⚠ SEQUENCING: this BLOCKS the A2 sweep.** The sweep is a one-way flattening of
  hand-built structure, so the marker must exist and the affected files must be marked
  BEFORE the sweep is regenerated.
- 2026-08-16 — **🎯 THE SILENT ARM-KILLER = REJECT, on two rules (owner, live session).**
  A `case` naming something that does not exist must not silently become a catch-all.
  Ratified pair, both compile ERRORS:
  **(1) UNREACHABLE-ARM.** Any arm following a catch-all — a bare binding, `_`, or
  `else` — is a compile error (Rust's `unreachable_patterns`, promoted from lint to
  error). This is the broad net: it catches the typo'd VARIANT, the typo'd CONSTANT,
  and it works on non-enum scrutinees.
  **(2) BARE NAME ON AN ENUM SCRUTINEE MUST RESOLVE.** Where the scrutinee's type is
  an enum, a bare identifier in the arm's TOP-LEVEL pattern must resolve to a variant
  of that enum or to a `const`/`static`; otherwise it is an error whose diagnostic
  points at `else:` for a catch-all. This closes rule (1)'s one blind spot — a typo in
  the LAST arm, where nothing below it becomes unreachable.
  **Bindings are NOT removed and cannot be:** `Pattern::Binding` is how destructuring
  names payloads (`case Custom(r, g, b):`), how guards name the scrutinee
  (`case x if is_even(x):`), and how the `is` operator introduces names into the
  following `then` block (reference:1930). All six bare-binding sites in the corpus
  match on `int` and are unaffected by (2).
  **Why the semantics were not changed instead:** a bare identifier in a pattern is a
  DECLARATION, so `case Satx:` is shape-identical to an intentional binding — no rule
  can distinguish typo from intent by shape, and deciding it by CAPITALISATION (the
  Haskell/OCaml answer) is banned here as semantics-from-identifier-shape. Rust has
  the identical hazard and answers it with diagnostics, not semantics
  (`unreachable_patterns` + `non_snake_case` + `bindings_with_variant_name`); this is
  that answer, with (2) added because Gorget also resolves bare names to CONSTANTS.
  **Precedent — this project has already been bitten by this exact class:** before
  2026-05-13 a bare `case CONST_NAME:` always shadowed the constant with a fresh
  binding, "so the FIRST `case` arm always matched (every input went down it)", found
  during self-host `format_gir.gg` work (`tests/fixtures/const_match_pattern.gg`
  header). It was fixed for constants only; the variant flavour stayed live until now.
  **Measured exposure:** the arm-killer reproduces at HEAD on both lanes with
  `gg check` clean, and the two engines print DIFFERENT output for the same accepted
  program (Rust gg `typo-arm/typo-arm` — two arms dead; ggdef `mon-arm/sat-arm`),
  which is Core #8 ≥2 bugs.
  **OPEN for the implementing scout:** whether rule (2) extends to NESTED pattern
  positions — `case Some(v):` where the payload type is itself an enum would reject a
  legitimate destructuring bind under a naive reading, while `case Some(None):` must
  keep working. Decide with the scout, not in the brief.
  Accept/reject change ⇒ Core #9: lands on all three lanes with cross-lane NEG
  conformance fixtures, in its OWN round (not the R42 formatter round).
- 2026-08-11 — **🎯 PARENTHESIZED IMPORT FORM RATIFIED (owner, live session, after
  measuring that Gorget's import name-list is the language's ONE undelimited list —
  hence unwrappable — while every peer ships a bracketed form).** `from env import
  (a, b, c)` becomes legal: the parens suspend newline-significance exactly as in
  expression context (Python-identical; PEP 8's preferred wrap). Consequences:
  `gg fmt` fill-packs import name lists at the 120 budget like every other list
  once the form lands; the split-into-repeated-statements workaround stops being
  needed; backslash continuation stays rejected. Until the form lands, import
  lines are EXEMPT from the width budget (the self-host's long-line convention is
  stated doctrine, not an accident — the appendix documents the exemption with a
  pointer here). Accept-surface change ⇒ Core #9: lands cross-lane (Rust parser +
  the three SH parser copies) with POS/NEG conformance fixtures, its own small
  track, not R41's. fmt-merge of adjacent same-module imports (the isort
  precedent) was NOT ratified — a separate knob if ever wanted.

## PHANTOM `private` ENUM VARIANT (h4) — REJECT (owner 2026-08-14)

**`private` (or any visibility keyword) in enum-variant position is a parse-time
REJECT with a teaching diagnostic** — joining the ratified equip/test/bench
visibility rejects (owner 2026-08-11): same `parse_item` modifier-discard family,
same Rust-E0449-shaped diagnostic. At HEAD the parser reads `private` as a
VARIANT NAME and mints a phantom unit variant (`enum Color:` with `private Red()`
yields variants `[private, Red, Green]`, `gg check`-clean) — a Core #8
wrong-accept. **Variant-level visibility is NOT foreclosed:** if a need arises it
can be promoted to a real feature by a future ruling; until then the position
rejects. Implementation rides the W2 reject-rider with h1-h3; the accept-surface
change lands cross-lane per Core #9 with NEG conformance fixtures per Core #11
(the known_gaps repro owed by the visibility entry's deferred-until-ruled logic is
discharged by those fixtures landing WITH the reject).
