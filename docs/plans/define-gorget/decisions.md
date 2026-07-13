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

### D9. Float formatting detail: integral-valued floats print with ".0" (owner, 2026-07-06)
Under D8's shortest-round-trip rule, `print(3.0)` → `"3.0"` (never `"3"`): the printed form must
parse back AS A FLOAT in Gorget itself ("3" parses as int), preserving D8's round-trip rationale
and type visibility. Gates the D8 formatting appendix; ggdef `format_value` needs the
integral-float fix (Rust `{}` prints "3" — use `{:?}`-style or ryu-with-".0"). Recorded at the
P1-infra reviewers' recommendation.

## LOG

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
  normative v1; per-code catchability deferred to deep-fault; SIGSEGV/OOM outside v1.
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
- 2026-07-06 — **D15 RATIFIED by owner: slices are owned values + `int[]`/`T[]` REMOVED
  from the surface entirely** (supersedes the filed reject-escape; owner: simplify and
  uniformize now, re-add later as a widening if C-interop demands — then as a dedicated
  FFI type, not the general slice). One sequence type; the fat pointer survives only as
  a possible future INVISIBLE CoW optimization. Removal track gated on a live-use scan.
  A6 CLOSED.

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
