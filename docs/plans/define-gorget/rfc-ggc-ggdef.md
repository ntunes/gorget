# RFC: Gorget Core (GGC) and `ggdef` — the executable definition of Gorget

> **STATUS: DRAFT v2 — under sequential fresh-agent review (pass 1 folded; pass 2 pending). Do
> not implement from this document until it carries a clean SIGN OFF and says APPROVED.**
> Companion: [`decisions.md`](decisions.md) (D1–D6 are normative inputs),
> [`scouts/scout-c-prior-art.md`](scouts/scout-c-prior-art.md) (architecture rationale).

## 1. Goals and non-goals

**Goal.** Give Gorget a single, executable source of truth for its dynamic semantics — the
triad, in one repo, merge-gated together (the WebAssembly model):

1. **Spec prose** — `spec/prose/` (grows out of `language-design.md`'s normative parts).
2. **`ggdef`** — a definitional interpreter whose behavior IS the meaning of a Gorget program.
3. **`spectests/`** — a conformance suite every implementation runs.

Charter (crate root, verbatim): **"`ggdef` is written for clarity and simplicity, not speed.
It is a device for nailing down exact semantics, not a runtime."** Model slogan: **"CoW is an
optimization; value semantics is the meaning"** (D1).

**Non-goals (v1).** Speed; a second production frontend (share the existing lexer+parser);
executable static semantics (v1: prose + expected-error-code fixtures; executable GGC
well-formedness checker is v1.5); mechanized proofs (D3); shared-state and concurrency
semantics (`Shared`/`Weak`/`Mutex`/`RwLock`/atomics/`shared`/tasks/channels — phase 3, §2.6);
`meta` beyond nothing (phase 2, §2.6); replacing `gg sim` as a CLI feature (separate owner
decision; sim is permanently disqualified as the *definition* — it consumes GIR; the Miri trap).

## 2. GGC — the core language

The desugared core `ggdef` evaluates. Bar: small enough to hold in one head, total enough to
mechanize later, faithful enough that every **in-scope** surface program elaborates into it
without semantic invention. §2.6 is the closed coverage map — nothing is silently out.

### 2.1 Construct inventory (v1)

**Values:** `int`/`float`/`bool` scalars (checked/wrapping arithmetic; `Fault` trap conditions
Overflow/DivByZero/Bounds); `String` (ONE value type; view/owned provenance is implementation
detail under D1); `Vector[T]`, `Dict[K,V]`, `Set[T]` (Dict/Set insertion-ordered per C6;
`HashMap`/`HashSet` order = the admitted `Nondet`, seed-swept); structs; enums with payloads
(Option/Result not special in GGC); tuples; **`Box[T]`** (unique-owner box value — needed for
recursive types and as a D4 by-design single-owner anchor); closures (capture-by-value per D5;
write-through captures carry the capture-mode tag from elaboration; a closure value is a code
ref + captured environment record).

**Places & modes:** locals, struct fields, collection elements; the three modes as
**elaboration-resolved tags** on each use — `Borrow` (bare), `WriteThrough` (`&`), `Move` (`!`).
GGC never re-infers a mode.

**Operations:** bind; read; write-to-place; call (per-arg mode tags); construct; index/field
read/write; `match` with bindings; `if`/`while`/`loop` (surface `for` elaborates to explicit
loops with the element-binding mode tag); `return`/`break`/`continue`; explicit `clone`;
scope-exit drop markers (reverse declaration order); Result-desugared error flow (throws is
sugar; GGC sees `Result` values + match-on-carrier); fault-catch as a **distinct guarded-op
form** (error-model Phase 1); **`print` as the GGC output effect** — stdout is THE observable,
so output exists in the core, and the formatting of each printable value (including float
formatting — a known divergence surface) is NORMATIVE, specified in a formatting appendix of
the prose.

### 2.2 The ownership model in GGC (per D1/D2/D4/D5)

`ggdef` implements **eager value semantics**:

- A **bind** (bare) conceptually copies the value as-of the bind point. (Production compilers
  use borrows + CoW; D1 makes the difference unobservable; the annexe (§5.4) polices the
  performance side separately.)
- A **`WriteThrough` place** aliases the owner; writes land on it.
- A **`Move`** transfers the value and kills the source.
- **Ownership boundaries** (collection put, ctor/field init, return, capture) receive the value
  by copy (or move when tagged). Clone-vs-move liveness optimization does not exist in `ggdef`.
- **`self`** is a bare binding (D2).
- **Drop-purity (D4):** GGC types carry a `drop_tainted` bit from elaboration. Elaboration
  REJECTS implicit-copy positions for tainted types (`E_MoveWithoutOperator` family); `ggdef`
  never implicitly copies a tainted value; custom drops run at scope exit in reverse
  declaration order; drop count/order for tainted types is normative and byte-tested.
  ⚠ Prerequisite bug: collection-element custom-Drop loss on named-local push (TODO HIGH,
  filed 2026-07-05) must be fixed before drop-count spectests can gate implementations.

### 2.3 Evaluator outcomes — and why GGC has no UB

`eval(fuel, state, expr)` is total with exactly four outcomes:

1. **Value** — defined result (+ trace events).
2. **Trap(Fault)** — Overflow/DivByZero/Bounds per the fault model (catchable by the guarded
   form; uncaught = defined panic with defined output).
3. **IllFormed** — a *statically ill-formed program detected dynamically* (e.g. read of a
   moved-out slot, including via an eager bind-copy of a partially-moved aggregate). This is
   defense-in-depth, not semantics: a `mode: run` fixture that hits IllFormed is an INVALID
   fixture and reclassifies to `static-error`, adjudicated in v1 by production typecheck +
   prose (see §4). IllFormed can never be a legal program's meaning.
4. **FuelExhausted** — a distinct, swept outcome (makes the function total; mechanization-ready).

There is deliberately no undefined behavior: every condition is Defined, Trap, or IllFormed.
This rule also closes the one eager/lazy residue found in review (an eager bind-copy touching a
dead slot that a lazy implementation never reads): such programs are ill-formed, not
divergently-defined.

### 2.4 Elaboration (surface AST → GGC) — the honest cost statement

A NEW spec-owned pass sharing the production **lexer + parser only** (verified import-clean:
the AST depends only on `crate::span`). It must never import `src/ir/` or `src/semantic/` —
enforced by an import ratchet lint in `tests/lints.rs` from day one (allowlist: lexer, parser,
AST, span/diagnostic plumbing). Sharing is source-level (ggdef links the gorget lib); the
ratchet is the real fence.

Elaboration is **not thin** — it is a small type-directed front half, and that is the price of
definitional independence, stated plainly: desugaring (throws→Result, for→loop, comprehensions,
method→call, dot-shorthand, `it`); its OWN name resolution (production resolver drags in the
TypeTable — sharing it defeats the ratchet); **local type inference** (`auto`, receiver types
for method→call, D6's inferable-`E` judgment); **a simple monomorphizer** (GGC is monomorphic);
trait-method resolution, with **trait objects elaborated to a closure-record + Box** (dynamic
dispatch = record of code refs — no vtable machinery in GGC); the D4 transitive taint
computation; the D4/D5/D6 rejections (elaboration is their normative home; production
typecheck mirrors them — "both, elaboration normative"). Elaboration-vs-production-typechecker
disagreements are themselves conformance findings.

### 2.5 D5 capture syntax (designed here, as the ledger delegates)

**Proposed surface** (owner ratifies at RFC approval): the per-variable capture list that
§7.4 of language-design already reserves, promoted from V2:

```gorget
auto f = (): print(count)              # bare: ALL captures by-value (D5)
auto g = (&count)(): count += 1        # capture list: &count = write-through, explicit
auto h = (!name, &total)(int x): ...   # per-variable: move name in, alias total
auto k = !(): consume(data)            # existing sugar: move ALL captures (unchanged)
```

Rules: a capture list may contain only `&name` / `!name` entries (bare names are redundant —
by-value is the default — and rejected to keep one spelling); `&`-captured variables follow the
same exclusivity rules as any `&` borrow; body-driven inference of write-through is retired
(D5), with a migration diagnostic ("closure mutates captured `count`, which is now a private
copy — capture it `(&count)` to write through") during the transition window.

### 2.6 Surface-coverage map (closed — nothing silently out)

| Surface area | v1 disposition |
|---|---|
| bindings/params/`self` modes, structs, enums, tuples, match, loops, closures, comprehensions, throws/Result/fault-catch, operators (incl. user overloads → calls per C9), newtypes (→ struct), type aliases (elab-time), Vector/Dict/Set, Box, String, print/f-strings | **GGC or elaborated sugar (in v1)** |
| traits/equip: static dispatch, generics/monomorph, trait objects | **elaborated (§2.4), in v1** |
| `Shared`/`Weak`/`Mutex`/`RwLock`/atomics/`shared`/tasks/channels/`select`/async | **phase 3** — true sharing needs a reference-cell value kind + the Nondet scheduler; B6/B9/B10 spectests wait for it |
| `meta` (all forms) | **phase 2** — elaboration-time evaluation; v1 spectests exclude meta fixtures via `features:` gating (4.9K-LOC production meta module is off-limits; a minimal spec meta is its own deliverable) |
| allocators/arenas, `bytes_used()` | **implementation-observation** (outside semantic equivalence per D1); programs calling them are excluded from output-comparison conformance, annexe-side only |
| FFI/extern, unsafe, GPU/SDL/net stdlib | **out of spec v1** (impl-defined; spec boundary = the pure language + collections + print) |
| slices `T[]` | **rejected surface** (per the filed reject-escape fix; GGC has no slice value) |

### 2.7 Interpreter discipline

Safe Rust only, no `unsafe`, no deps beyond the shared frontend; pure data; one fuel-indexed
functional-big-step eval; explicit `Nondet<T>` (hash order now, scheduling in phase 3), seeded
and swept; every ownership-relevant event (bind-copy, move, explicit clone, drop, write,
**annexe-tagged no-copy positions**) emits a trace event with provenance — raw material for
`gg explain` (phase 2) and for divergence debugging.

## 3. Crate & directory layout

```
spec/
  ggdef/          # crate (workspace member; replicate [lints.rust] warnings="deny")
    src/elaborate/  src/ggc.rs  src/eval.rs  src/trace.rs
  prose/          # normative prose, section-per-construct, cross-citing eval.rs (HaMLet-style)
spectests/
  run/            # frontmatter + expected stdout/exit — ggdef-GENERATED
  static-error/   # expected diagnostic codes (see §4 adjudication + §5.5 registry)
  parse-error/
  annexe/         # copy-guarantees probes (§5.4) — implementation-facing
  staging/        # low-bar tier (test262 model), no gate
```
`cargo run -p ggdef -- run|gen|trace file.gg`.

## 4. Conformance wiring

- **Frontmatter:** `mode: run|static-error|parse-error`; `expect:` (stdout+exit / diagnostic
  code); `args:`/`stdin:`/`files:` (the harness's run_gg_with_args/_with_stdin/_dir shapes);
  `nondet: seeds=N`; `since:`; `features:` (gates meta/shared/etc. until their phase);
  `adjudicator: ggdef | production-v1` (see below). Trap output is normalized (defined panic
  message shape; file:line normalized) — the normalization rule is spec text.
- **Expectation provenance:** `run/` and elaboration-owned rejections (D4/D5/D6 + parse) are
  **generated by `ggdef`** and human-review-diffed. Other `static-error/` expectations are
  production-derived in v1 (no executable typecheck yet) and carry
  `adjudicator: production-v1` — an explicit, tracked inversion retired at v1.5, not a silent
  one.
- **Runners:** thin adapters over existing machinery (integration harness for C/LLVM,
  the self-host driver lane, ggdef itself); each prints a `spec_conformance_<impl>`
  always-pass diagnostic with monotone floors in `tests/lints.rs`.
- **smith:** ggdef joins as the **verdict lane** — divergences adjudicated against the
  definition; tri-state triage (impl bug / spec bug / spec silent); spec changes justified only
  by design intent, never "matches the implementation" (invariant #8 as spec process).
- **Speed:** CI runs ggdef on `spectests/` (curated); nightly runs the full corpus.

## 5. Process

**5.1 Same-PR gating** (post-skeleton): semantics-visible change = prose Δ + ggdef Δ +
spectests + implementations green (or floor-tracked exemption). Social first, then a coverage
lint (arm-count pattern).
**5.2 Versioning:** `spec-v0.x` tags + changelog; `since:` on fixtures; living artifact
(the SML freeze is the named anti-pattern); 1.0 is a spec release.
**5.3 Docs write-through** (owner directive): each landed rule updates language-design, book,
devbook in the same series; the ledger tracks write-through debt per decision.
**5.4 The copy-guarantees annexe (D1's second half):** a small CLOSED list of
MUST-NOT-ALLOCATE positions (bare bind/read/param-pass, borrow field/element read — README:50's
promises). Tested implementation-side: `spectests/annexe/` programs under `--clones=stats` / a
counting allocator asserting zero clones at guaranteed positions. **v1 gates the C backend
only** — `--clones=stats` is rejected under `--backend=llvm` (open TODO(llvm-clone-stats)) and
the self-host has only transient instrumentation; LLVM/self-host annexe lanes are floor-tracked
debt, not silent. Spec-side counterpart: ggdef trace-tags annexe positions, making the list
machine-readable from the definition.
**5.5 Diagnostic code registry (named Phase-1 item):** stable `E_`-codes for spec-referenced
diagnostics (today: enum variants + message-substring tests; `E_` appears once in the docs).
The registry maps code → prose section → fixtures; production diagnostics adopt codes
incrementally.

## 6. Phased delivery

- **Phase 0 — walking skeleton:** ggdef + elaboration for the GGC subset covering the `cow_*`
  family shapes (binds, aliases, mutation-severing, Vector/struct/String, bare/&/! params,
  scope drops, print). Acceptance: (a) runs the cow_* + deadwrite_* families **modulo
  feature-gated exceptions** (e.g. AtomicInt fixtures) with output matching the ratified
  expectations (C1–C10); (b) adjudicates the two smith bugs (`9` / `ablog`) and (c) the EMove
  question (pre-mutation value, per D1) from the definition. Import ratchet lands with the
  crate. W3a-d String shapes are IN (highest-value adjudications); their clone-count side is
  D1-allowed variation → annexe, not ggdef — that split is stated in the prose.
- **Phase 1 — coverage + floors:** frontmatter migration (converter from the 1,212 literal
  run_gg pairs → `ggdef -- gen` regeneration → human-reviewed diff, every diff a finding);
  per-impl conformance reports + floors; ggdef verdict lane in smith; enums/match/closures/
  error-model/traits/generics; D4/D5/D6 rejections in elaboration + production + negative
  fixtures; diagnostic-code registry (§5.5).
- **Phase 2 — annexe + `gg explain` v0** (trace-backed provenance, human + JSON) + minimal
  spec meta.
- **Phase 3 — sharing & concurrency:** reference-cell value kind, Shared/Mutex/RwLock/tasks,
  seed-swept scheduling Nondet; B6/B9/B10 spectests. **v1.5 — executable well-formedness
  checker** (retires `adjudicator: production-v1`). Then mechanization prep (Aeneas trial on
  eval.rs).

## 7. Risks and guards
Rot → ggdef in CI day 0. Spec-lags-impl → same-PR gate + coverage lint. Divergence-by-
convenience → generated expectations + intent-based review + fresh-pass gauntlet. Perf creep →
charter; sim absorbs speed pressure. Scope creep → §2.6 is the fence; changing it is a
reviewed spec change. Miri trap → import ratchet. Freeze → §5.2. Coverage illusion → smith
across all lanes + seed sweeps.

## 8. Remaining open items
1. **Owner ratification of the §2.5 capture-list syntax** (delegated to this RFC by D5;
   flagged for explicit sign-off at RFC approval).
2. Formatting appendix contents (float formatting normative choice: the C backend's `%g`
   behavior vs a spec-owned algorithm — needs one decision when the appendix is written).
3. The `E_`-code numbering scheme (bikeshed; Phase 1).
