# A33 + fault-model design scout — MANDATE

> **Owner-directed (2026-07-10/11 design discussion).** The A33 supervised-boundary
> spec is pulled forward: scout NOW, owner ruling in the NEXT decision batch. The
> discussion widened the mandate from one question to three — they are ONE design
> ("where may a fault become a value?") and must be ruled together.
> **Status:** mandate v1 — scout launched 2026-07-11. Read-only; no code changes;
> deliverable = report + option-questions with recommendations and previews.

## Context the scout must absorb FIRST (docs before code — CLAUDE.md scout rule)

- `docs/plans/define-gorget/decisions.md` — D11 (trap registry + `Fault` = catchable
  subset + no-drops-on-trap), D18 (const-eval mirror), D23 (throws totality + the
  diagnostic contract), the A31/A32/A33 queue entries INCLUDING the A33 owner rider
  (faults enter the error/value world ONLY via explicit conversion points), and the
  2026-07-07 gorget-js dogfood LOG entry (finding (b): pre-checking bounds to dodge
  uncatchable-across-calls faults).
- `docs/language-reference.md` §10.5 (fault catch is LOCAL AND LEXICAL) + §10.9
  (faults out of signatures) + the arithmetic section (`+%` wrapping family,
  "no global mode changes `+`").
- `spec/prose/trap-codes.md` — the ratified registry + catchability column.
- `docs/plans/error-model.md` §9 residuals; `docs/language-design.md` philosophy.
- The 2026-07-11 owner discussion conclusions (recorded in the A33 queue entry):
  errors vs faults taxonomy (errors = the ONE channel, throws/Result; faults =
  termination, not a channel; "catching" = conversion at explicit points); the
  catchability principle candidate (implicit machine checks vs explicit programmer
  assertions); the deep-catch rejection rationale (D1-refinement / no-drops / A33
  rider / 4-implementation unwinding cost — the eventual spec prose carries a
  "why not dynamic exceptions" section, drafted by this scout).

## Q1 — The supervised-boundary hook spec (the original A33 mandate)

Pin, at SPEC level only (no implementation, scheduling semantics stay phase-3):

1. **The conversion point's shape** — Task join only? Also a lexical
   `supervise`-style block? What does the owner ratify as the v1 hook surface?
2. **The fault value's shape** — a `T_`-code-carrying value; its payload (detail
   string? location?); its conversion contract into a catchable Error in the ONE
   channel; interaction with the D6 unbound-carrier rule and A31's future sets.
3. **The permanence set** — what stays true forever: panic-by-default, faults out
   of signatures, the A33 explicit-conversion rider; plus teardown semantics at
   the boundary (whole-unit discard — NO continuation into partial state; relate
   to D11's no-drops-on-trap and to the EXISTING in-repo precedent: `gg test`
   mode's setjmp/longjmp + registered-cleanup-stack boundary in
   `src/backend/c/runtime/panic_test.c` — the mechanism R-B just hardened).
4. Precedent survey: Erlang/OTP supervisors ("let it crash"), Rust
   catch_unwind + UnwindSafe (the cautionary tale), Go recover, Pony, Swift's
   trap-uncatchable model + server-side isolation practice.

## Q2 — Should the lexical fault-catch survive v1 AT ALL? (owner second-guess, 2026-07-11)

The removal hypothesis: faults become uniformly uncatchable (Swift model); the
boundary (Q1) becomes the ONLY fault→value conversion; the catchable-subset
concept, the `Fault` prelude enum, and the registry's catchability column are
deleted; recovery ergonomics are covered by Q3's fallible forms + existing safe
APIs (`.get()` → Option).

Evidence to gather (CENSUS FIRST — the ruling is census-gated):

1. **Live-use census of `catch Fault`** across `tests/fixtures/`, the self-host
   dirs, gorget-arena (`target/gorget-arena/`), and **gorget-js at
   `/workspace/gorget/.worktrees/gorget-js` (READ-ONLY — see constraints)**.
   Pre-census (orchestrator, 2026-07-11): 39 fixture files, 0 gorget-js files.
   Classify each use: real recovery need vs test-of-the-feature-itself.
2. **The gorget-js pre-check census** (the finding-(b) sites): classify EVERY
   pre-check-to-dodge-fault site as (a) expressible with existing fallible APIs
   (`.get()`, `throws` calls), (b) needs boundary-level isolation (Q1 covers it),
   (c) genuinely needs mid-stack recovery (evidence AGAINST the model — report
   loudly if found).
3. **Machinery inventory — what removal retires**: the fault-slot lowering
   (fault-scope blocks, `fill_fault_return_block` + the cross-frame re-panic
   sites in BOTH compilers — the open both-compiler re-panic normalization TODO
   entry), the fault-slot closure adapters (already cited as retrofit pain in the
   A32 queue entry), the filed dead-fault-catch lint, ggdef's catch-absence
   (ggdef models NO catch today — removal means the definition is already
   complete). Estimate the deletion scope per implementation.
4. **Replacement completeness**: for each catchable fault class, the value-level
   twin that must exist BEFORE removal (D13 pattern — replacements land with or
   before): Bounds → `.get()` (exists); Overflow → Q3's fallible form; DivByZero
   (+ `INT_MIN / -1`) → Q3's fallible form; shift-range → Q3.
5. If the census supports KEEPING the catch: state the catchability principle for
   the registry docs instead (implicit machine checks catchable, explicit
   assertions never) — the fallback resolution of review-pushback #4.

## Q3 — Fallible-arithmetic ergonomics (owner: "I don't like checked_add(); find an operator; can we innovate?")

Evaluate the **fallible-operator family** as the primary candidate:
`+?  -?  *?  /?  %?` (and whether `<<?`/`>>?` join) — checked operations that are
FIRST-CLASS CITIZENS OF THE ONE ERROR CHANNEL: `a +? b` is an `int` expression
that **throws** a prelude arithmetic error on overflow (D23 totality applies —
it is `T` in every position, auto-propagates in `throws` contexts, is catchable
with the EXISTING contract-error `catch (e):` form, capturable as
`Result[int, E]`). The symmetry to pin:

| form | semantics | on failure |
|---|---|---|
| `a + b` | checked assertion | trap `T_Overflow` (a bug) |
| `a +% b` | wrapping | never fails (defined) |
| `a +? b` | fallible | throws into the channel (data) |

Design points the scout must settle or option-ize:

1. **Error type**: one prelude enum (e.g. `ArithError { Overflow, DivByZero, … }`)?
   Payload or payload-free? Interaction with D6 (inference is trivial — the type
   is fixed by the operator) and with A31's future inferred sets.
2. **Lexing/grammar feasibility**: `?` adjacency with the existing `?` propagation
   operator and any other `?` uses; precedence identical to the base operator;
   compound-assign forms (`+?=`?) — recommend include/exclude with reasons.
3. **Precedent survey (answer the owner's "how does Swift do it?")**: Swift —
   default `+` traps, `&+`/`&-`/`&*` are WRAPPING (Gorget already has this as
   `+%`), checked = `addingReportingOverflow()` → `(partialValue, Bool)` tuples,
   universally considered clunky, no checked OPERATOR ever landed. Rust —
   `checked_*` methods → Option + `Wrapping<T>`/`Saturating<T>` wrapper types.
   Zig — `+%` wrap, `+|` saturate, `std.math.add` → error union composing with
   `try` (the closest prior art to `+?`, but as a FUNCTION; Gorget's operator +
   auto-propagation would leapfrog it). Conclusion to verify: no mainstream
   language ships fallible arithmetic OPERATORS wired into its error channel —
   this is a genuine innovation opening.
4. **Saturating**: is `+|` (Zig-style) worth reserving now? (Likely: note as a
   pure future widening, don't spec.)
5. **D18 interaction**: const-eval of `a +? b` — mirror-runtime says it should
   produce the ERROR value at compile time (a const Result), NOT a compile error
   (that's the FAULT rule). Confirm coherence or flag for the ruling.
6. **Migration preview**: the 39 `catch Fault` fixture files re-expressed in the
   new form (spot-sample 5, show before/after); the fault-catch removal track's
   fixture migration cost.

## Q4 — Sigil economy: `!` = errors, `?` = optionals, move rehomed (owner proposal, 2026-07-11)

Added mid-scout (owner discussion). Orchestrator pre-findings to verify and build on:
Gorget uses the `not` KEYWORD for negation (prefix `!` is exclusively the move
sigil, `Token::Bang`, 10 parser sites); the `?`-family is ALREADY Optional-flavored
in the shipped surface (`?.` QuestionDot optional-chaining + `??` DoubleQuestion
coalescing — `??` in 19 fixture files); bare postfix `?` is essentially DORMANT
(one parser lookahead mention, `expr.rs:1468`; propagation is automatic — the
`Expr::Propagate` node is elaboration-level). Therefore the owner's split is
ALREADY the de-facto convention except for `!`-as-move, and Q3's fallible family
should be re-glyphed **`+!` `-!` `*!` `/!` `%!`** (errors), NOT `+?` (which would
collide with the in-language `?`=Option convention).

Scout deliverables for Q4:

1. **Census the `!`-move surface**: every move-sigil site class (call-site `!arg`,
   bare-assign `!source`, param `T !name`, D7 capture lists `(!name, ...)` +
   `!()` move-all sugar) counted across fixtures / self-host / gorget-js / arena.
   Also confirm bare-`?` postfix liveness (grep + parser reading) and `??`/`?.`
   usage counts. Frequency argument to test: moves are RARE-BY-DESIGN in
   CoW-default-borrow Gorget (Huffman: the rare op can afford a keyword; the
   common fallible ops deserve the glyph).
2. **Move-rehoming candidates evaluated at ALL FOUR positions** (call, assign,
   param, capture list): (a) `move` keyword (C++/Rust prior — strongest human+LLM
   prior; check the param-position reading `void consume(Message move msg)` vs
   the established sigil-before-name convention and the `&msg` asymmetry);
   (b) `take` keyword; (c) `^` sigil (keeps sigil symmetry with `&`; weak/foreign
   priors — Obj-C blocks, Go xor). Include the D7 impact (capture-list re-spelling
   + move-all sugar redesign = a D7 rider) and the diagnostics/docs surface
   (E_MoveWithoutOperator message, CLAUDE.md quick-ref, README, book).
3. **Lexing/grammar for `+!` under the rehomed grammar**: once prefix `!` is gone
   (negation is `not`, move rehomed), verify `+!` tokenizes without ambiguity
   (incl. against `!=` and `a + !b` legacy shapes during migration); precedence =
   base op; compound forms `+!=`? Recommend include/exclude.
4. **Prior-collision analysis** (feeds the LLM-correctness KPI, an owner-directive
   metric): Zig reads `!`=error-union (correct); Swift/TS/Kotlin read `!` as
   force/non-null-assert (they may misread `+!` as trapping — but plain `+`
   already traps, and a misused `+!` produces a LOUD D23 unhandled-throws
   compile error, never a silent behavior surprise — verify this reasoning).
5. **Candidate ruling D27** (sigil economy) drafted as an option-question:
   full-swap (recommended?) vs `!`-stays-move + fallible ops get another glyph vs
   status quo. Include migration-cost estimate from the census (mechanical,
   grep-able classes).
6. Q3's deliverables update accordingly: the fallible family is evaluated as `+!`
   (primary) with `+?` documented as the rejected-for-collision alternative; the
   optional-family future sugar (`v[i]?` → Option indexing, chaining with the
   existing `??`) noted as a separate, later widening (do not spec it here beyond
   one paragraph).

## Constraints

- READ-ONLY everywhere. **gorget-js at `/workspace/gorget/.worktrees/gorget-js` is
  outside your worktree and under the PROTECTED main checkout — read files and run
  read-only `git -C` queries there ONLY; never write, never build, never run `gg`
  against it in place (copy sources into your own scratch if you must run
  anything).** Same for gorget-arena.
- No implementation, no prototypes that mutate the tree; lexer-feasibility may be
  argued from the grammar + `src/lexer/` reading, not implemented.
- Deliverable: report to /tmp + final message — census tables, the machinery
  inventory, the three option-questions for the owner (candidate rulings: D24
  boundary spec · D25 fault-catch disposition · D26 fallible operators), each with
  a recommendation, previews, and the evidence that would change it. Draft the
  "why not dynamic exceptions" spec-prose section as an appendix.
