# RFC: Gorget Core (GGC) and `ggdef` — the executable definition of Gorget

> **STATUS: APPROVED — owner, 2026-07-05** (after 5-pass review gauntlet + owner read + Q&A on
> §2.2 categories; §8 items 1-2 ratified as D7/D8). Owner conditions recorded in the ledger:
> production implementations MUST use borrows+lazy-CoW or an equally-fast proven-equivalent
> strategy (D1 owner note — the eager formulation is spec-only). Phase 0 is GO per
> [`phase0-brief.md`](phase0-brief.md); orchestration handover in [`HANDOVER.md`](HANDOVER.md).
> Companion: [`decisions.md`](decisions.md) (D1–D6 normative), [`scouts/`](scouts/).

## 1. Goals and non-goals

**Goal.** A single, executable source of truth for Gorget's dynamic semantics — the triad, in
one repo, merge-gated together (the WebAssembly model): **spec prose** (`spec/prose/`),
**`ggdef`** (a definitional interpreter whose behavior IS the meaning), **`spectests/`**
(conformance suite every implementation runs).

Charter (crate root, verbatim): **"`ggdef` is written for clarity and simplicity, not speed.
It is a device for nailing down exact semantics, not a runtime."** Model slogan: **"CoW is an
optimization; value semantics is the meaning"** (D1).

**Non-goals (v1).** Speed; a second production frontend (share lexer+parser only); executable
static semantics (v1: prose + coded fixtures; executable well-formedness checker at v1.5);
mechanized proofs (D3); shared-state & concurrency (`Shared`/`Weak`/`Mutex`/`RwLock`/atomics/
`shared`/tasks/channels/`select`/async — phase 3); `meta` (phase 2); FFI/unsafe/platform
stdlib (out of spec; impl-defined); replacing `gg sim` as a CLI feature (separate owner
decision; sim is permanently disqualified as the *definition* — it consumes GIR; the Miri trap).

## 2. GGC — the core language

The desugared core `ggdef` evaluates. Bar: small enough to hold in one head, total enough to
mechanize, faithful enough that every **in-scope** surface program elaborates into it without
semantic invention. §2.6 is the closed coverage map.

### 2.1 Construct inventory (v1)

**Values:** sized integers `int8/16/32/64`, `uint8/16/32/64` (+ aliases `int`/`uint`/`byte`),
`float32/64`, `bool` — with checked arithmetic, the wrapping operators, and **`as`-cast
semantics as spec rules** (float→int **saturates**, ratifying the 2026-04-24 both-backend fix;
`language-reference.md`'s "truncates toward zero" text is stale and gets the docs write-
through); `String` (ONE value type; view/owned provenance is implementation detail under D1);
`Vector[T]`, `Dict[K,V]`, `Set[T]` (insertion-ordered: Dict per language-reference, Set per C6);
`HashMap[K,V]`, `HashSet[T]` (iteration order = the admitted `Nondet`, seed-swept); structs;
enums with payloads (Option/Result not special); tuples; `Box[T]` (unique-owner box — recursive
types; a D4 by-design single-owner anchor); closures (capture-by-value per D5; capture-mode
tags from elaboration; a closure value = code ref + environment record).

**Places & modes:** locals, struct fields, collection elements; modes as **elaboration-resolved
tags** — `Borrow` (bare), `WriteThrough` (`&`), `Move` (`!`). GGC never re-infers a mode.

**Operations:** bind; read; write-to-place; call (per-arg mode tags); construct; index/field
read/write; `match` with bindings; `if`/`while`/`loop`; `return`/`break`/`continue`; explicit
`clone`; scope-exit drop markers (reverse declaration order); Result-desugared error flow;
fault-catch as a distinct guarded-op form; **`assert` as a checked op** whose failure is a
defined panic (spectests always run asserts-on; `--strip-asserts` builds are an implementation
option outside conformance runs); **`print` as the GGC output effect** — stdout is THE
observable; formatting of every printable value is NORMATIVE via a formatting appendix in the
prose (float formatting is decision §8.2).

### 2.2 The ownership model in GGC (per D1/D2/D4/D5)

`ggdef` implements **eager value semantics**:

- **Implicit-copy positions (closed set, matching the D4 ledger):** bare-assign binds,
  constructor/struct/enum field init, collection put, return, closure capture, and
  **materialize-on-write (below)**. An implicit copy is **a read of a LIVE PLACE** at one of
  these positions: a fresh expression temp (constructor or call result) has no continuing
  owner, so elaboration tags it `Move` — a STRUCTURAL fact, not the retired liveness
  optimization. (`Res r = Res(1)`, `return make()`, and `with Res(1) as r:` are moves, never
  copies.) At the first five positions a live-place value is conceptually copied as-of that
  point (or moved when tagged `Move`). Clone-vs-move liveness optimization does not exist in
  `ggdef`.
- **`Borrow` positions (bare params, reads, receivers) are non-owning views for ALL types on
  READ** — no copy, no drop in the borrower, for reading. **A WRITE through a Borrow binding
  MATERIALIZES**: at the first write, the binding becomes a persistent private copy holding the
  pre-write value with the write applied; all subsequent reads AND writes through that binding
  see the copy; the owner is untouched; the copy drops in the borrower's scope. This is the
  language's core CoW rule (language-design §3.1) stated as eager semantics — it is what makes
  the deadwrite family and D2 evaluable. **`self` is a bare binding (D2): a write through
  plain `self` materializes exactly as above.** **Match pattern bindings and `for`-loop
  variables are Borrow-mode bindings** (views of the scrutinee/element; materialize-on-write
  applies) — per language-design §3.1's enumeration of bare positions.
- **`WriteThrough` places** alias the owner; writes land on it. **`Move`** transfers and kills
  the source.
- **Drop-purity (D4):** types carry a transitive `drop_tainted` bit from elaboration.
  Elaboration REJECTS all SIX implicit-copy positions above for tainted types **when the
  source is a live place** (fresh temps move — the ledger's "Box-identical" pin) — including
  materialize-on-write: a write through a Borrow binding of a tainted type is a compile error
  with the move/clone/`&` fix-it (`E_MoveWithoutOperator` family) — so `ggdef` never implicitly
  copies a tainted value; custom drops run at scope exit in reverse declaration order; **drop
  count/order for tainted types is normative and byte-tested**. A pleasing consequence:
  because tainted materialize is rejected, any binding that DOES conditionally materialize is
  drop-pure, so its scope-exit drop is unobservable — the observable semantics needs no
  dynamic drop-flags. ⚠ Prerequisite: the
  collection-element custom-Drop loss on named-local push (TODO HIGH, 2026-07-05) must be
  fixed before drop-count spectests gate implementations.
- **ResourceExhausted (stack depth, OOM, other host limits)** — named *event class*, not a
  language outcome and not "defined as SIGSEGV" (ledger C11 as amended/refined 2026-07-15).
  Production may die on the OS guard / allocator. Conformance does **not adjudicate** these
  runs (non-comparable). Not a D11 TrapKind. `ggdef` is total via its own fuel bound
  (`FuelExhausted`, tool-level) and never claims to model real-stack or OOM behavior.

### 2.3 Evaluator outcomes — and why GGC has no UB

`eval(fuel, state, expr)` is total with exactly four outcomes (trace events accompany ALL
four — a mechanizer and a divergence-debugger both need the events leading up to a Trap or
IllFormed, not just successful runs): **Value**;
**Trap(Fault)** (Overflow/DivByZero/Bounds/assert-failure — catchable per the fault model;
uncaught = defined panic with normalized output); **IllFormed** (a statically-ill-formed
program detected dynamically — e.g. a read of a moved-out slot, incl. via an eager bind-copy
of a partially-moved aggregate); **FuelExhausted** (distinct, swept). No undefined behavior:
every condition is Defined, Trap, or IllFormed.

IllFormed is defense-in-depth, never a program's meaning. A `mode: run` fixture that hits it
is INVALID and reclassifies to `static-error`. Adjudication: if production typecheck rejects
it, expectation = that rejection (v1). **If production typecheck ACCEPTS it, that is an
invariant-#8 finding** — file the both-compiler bug, write the `static-error` fixture with a
prose-derived code, and count every implementation as MISMATCH until the rejection ships.
(This closes the circular-adjudicator hole; it also means the IllFormed rule can impose NEW
production rejections — that's the point.) Tests of the IllFormed detector itself are `ggdef`
unit tests, not language conformance fixtures.

### 2.3.1 The verdict boundary — `verdict = check_liveness ∘ eval` (ratified 2026-07-15)

A ggdef program's verdict is **`elaborate ∘ eval`**, and the elaborate half now includes a
flow-sensitive **may-move liveness** gate: `verdict = check_liveness ∘ eval`.

- **ggdef-elaborate owns EVERY ratified static rejection within ggdef's subset — including the
  flow-sensitive ones.** Use-after-move, double-move, move-in-loop, and conditional-move-then-use
  are rejected at elaboration with an `IllFormed` outcome carrying an `E_` code
  (`E_UseAfterMove` / `E_DoubleMove` / `E_MoveInLoop`) BEFORE eval runs. This mirrors production
  `src/semantic/` (`origins.rs`) and the self-host `check_safety_*` walk. The may-move merge is
  textbook flow-sensitive dataflow — one syntax-directed walk over a moved-set: kill on move,
  revive on whole-local reassignment, union at joins ("moved in ANY arm ⇒ moved after"), filter
  diverging arms, moved-in-loop-body ⇒ `E_MoveInLoop` — deterministic, terminating, no fuel, no
  path enumeration. It is NOT all-paths execution; it abstracts branches by union.
- **ggdef-eval owns pure per-path dynamic semantics.** A valid re-init (`x` moved, then
  whole-rebound, then read) REVIVES the slot and RUNS to a Value — the dynamic revive-on-reassign
  rule. Eval's `IllFormed` outcome remains defense-in-depth (§2.3), but for the ratified static
  rejections the elaborate gate fires first, so those programs never reach eval.
- **The escape-hatch list is EMPTY of ownership carve-outs.** ggdef-elaborate owns the ENTIRE
  flow-sensitive static ownership axis with an executable arbiter (the `check_liveness` gate +
  the conformance lane) — no ownership rule is left to prose alone without a guard that can
  MISMATCH (Core #6). The only honest gaps that remain are ggdef *subset* limits (generics,
  `it`-lambdas, other B2 constructs outside the modeled core) — subset limits, NOT ownership
  carve-outs.
- **GUARD-RAIL (owner, so the pendulum does not overswing):** elaborate models the RATIFIED
  CONSERVATIVE may-move rule (the merge rule specified in `docs/language-reference.md` §4.2). It
  must NOT become a place where whatever precision production's analysis happens to have gets
  silently canonized. If production rejects something elaborate accepts (or vice versa) on a
  liveness shape, that is a finding to adjudicate against the PROSE rule, exactly like any other
  cross-lane divergence. **The definition LEADS; it does not trail.**

Both the `static-error` tier (semantic / may-move rejection) and the `parse-error` tier map to
**exit 1** — the compile-error class. See the consolidated toolchain exit-code table in
`docs/language-reference.md` (`0` success · `1` static rejection · `2` usage · `101` trap+ICE ·
`103` ggdef-only fuel).

### 2.4 Elaboration (surface AST → GGC) — the honest cost statement

A NEW spec-owned pass sharing the production **lexer + parser only** (verified import-clean;
AST depends only on `crate::span`). Never imports `src/ir/` or `src/semantic/` — enforced by an
import ratchet lint from day one. Sharing is source-level; the ratchet is the real fence.

Elaboration is **not thin** — a small type-directed front half, priced in deliberately:
desugaring (throws→Result, for→loops, comprehensions, method→call, dot-shorthand, `it`,
ranges/`in`, `with`-resource, named scopes, `do:`); its OWN name resolution (the production
resolver drags in the TypeTable; sharing defeats the ratchet); **local type inference**
(`auto`, receiver types, D6's inferable-`E` judgment); **a simple monomorphizer** (GGC is
monomorphic; phase 0 needs the concrete-equip slice; the generic-equip-on-builtin slice is
OPTIONAL in phase 0 per §6's exclusion list);
trait-method resolution with **trait objects elaborated to closure-records + Box**; the D4
transitive taint computation; the D4/D5/D6 rejections (**elaboration is their normative home**;
production mirrors them). Elaboration-vs-production disagreements are conformance findings.

### 2.5 D5 capture syntax (designed here, as the ledger delegates)

**Proposed surface** (owner ratifies at RFC approval): the per-variable capture list that
language-design §7.4 reserves for V2, promoted now:

```gorget
auto f = (): print(count)        # bare closure: ALL captures by-value (D5)

auto g = (&count)():             # capture list: &count = write-through, explicit
    count += 1                   # (multi-line body: assignment is a statement —
                                 #  inline expression bodies can't contain `+=`)

auto h = (!name, &total)(int x): # per-variable: move `name` in, alias `total`
    total += x
    process(!name)

auto k = !(): consume(data)      # existing sugar: move ALL captures (unchanged)
```

Design notes (load-bearing, from pass-2 grammar verification):
- **Bare names are REJECTED in capture lists** — not just style: the rejection is what
  disambiguates a capture list from a parenthesized-callee call `(f)(x):` in expression
  position. Every entry must be `&name` or `!name`.
- The parser needs a **two-group lookahead** extension (`looks_like_closure` currently checks
  `:` after the first paren group only). `(&count)` / `(!a, &b)` are grammatically live today
  as paren/tuple expressions but calling them is semantically dead — no legal program collides.
- Move keeps two spellings (`!()` move-all sugar; `(!name)()` per-variable) — accepted
  asymmetry: move-all is the common idiom (thread spawn), per-variable is the precise form.
- **Exclusivity duration for `&`-captures is LIVENESS-BASED** (NLL-style): the write-through
  borrow ends at the closure value's last use, after which the variable is readable/writable
  again. (Verified: production is currently scope-based — `print(count)` after the last `g()`
  is rejected — which would make the D5 counter idiom unusable; that gap becomes a
  conformance finding, not the spec.)
- Body-driven inference of write-through is retired (D5); migration diagnostic: "closure
  mutates captured `count`, which is now a private copy — capture it `(&count)` to write
  through."

### 2.6 Surface-coverage map (closed; changing a row is a reviewed spec change)

| Surface area | v1 disposition |
|---|---|
| binding/param/`self` modes; structs; enums; tuples; match; if/while/loop/for; closures; comprehensions; throws/Result/fault-catch; `assert`; operators incl. user overloads (→ calls per C9); newtypes (→ single-field struct, `.0`); type aliases (elab-time); ranges + `in` (→ sugar); `with expr as name:` (→ scoped bind + drop-at-exit); named scopes + `do:` (→ blocks); Vector/Dict/Set/HashMap/HashSet; Box; String; print/f-strings (formatting normative) | **GGC or elaborated sugar (v1)** |
| traits/equip static dispatch; generics/monomorph; trait objects (→ closure-record + Box) | **elaborated (§2.4), v1** |
| `const` locals; module `static` globals (init order, mutation, program-exit drops) | **phase 1** (statics are observable global state — need explicit GGC store rules) |
| **spec stdlib boundary**: the pure helpers spectests use | **enumerated shim list, v1** (measured against the target families): `std.collections.{Vector, Set, Dict}` (import→builtin-value mapping; Vector/Dict also prelude-available) + `std.conv.int_to_str`. Elaboration maps each to a GGC intrinsic or a pure GGC-level definition shipped with `ggdef`; anything NOT on the list is out of spec v1; growing the list is a reviewed, versioned change. (`std.sync` atomics are NOT shims — phase 3.) |
| `Shared`/`Weak`/`Mutex`/`RwLock`/atomics/`shared`/tasks/channels/`select`/async | **phase 3** (reference-cell value kind + scheduler Nondet; B6/B9/B10 spectests wait) |
| `meta` (all forms) | **phase 2** (elaboration-time; production meta module is off-limits; v1 spectests exclude meta fixtures) |
| directives | **dispositioned individually**: `strip-asserts` = impl option outside conformance; others (scheduler, trace, hot-reload…) = impl options, out of spec; any directive that changes language semantics must instead become a spec-versioned feature (none known today — verify in phase 1) |
| allocators/arenas, `bytes_used()`, `--clones` | **implementation-observation** (annexe-side only; such programs excluded from output-comparison) |
| FFI/extern, unsafe, GPU/net/platform stdlib | **out of spec v1** |
| slices `T[]` | **rejected surface** (per the filed reject-escape fix; GGC has no slice value) |

### 2.7 Interpreter discipline

Safe Rust only, no `unsafe`, no deps beyond the shared frontend; pure data; one fuel-indexed
functional-big-step eval; explicit seeded `Nondet<T>` (hash order now, scheduling phase 3),
swept in conformance; every ownership-relevant event (bind-copy, move, explicit clone, drop,
write, annexe-tagged no-copy positions) emits a provenance trace event — raw material for
`gg explain` (phase 2) and divergence debugging.

## 3. Crate & directory layout

```
spec/
  ggdef/            # workspace member (replicate [lints.rust] warnings="deny")
    src/elaborate/  src/ggc.rs  src/eval.rs  src/trace.rs  src/shims.rs
  prose/            # section-per-construct, cross-citing eval.rs (HaMLet-style)
spectests/
  run/  static-error/  parse-error/  annexe/  staging/   # staging = low-bar tier, no gate
```
`cargo run -p ggdef -- run|gen|trace file.gg`.

## 4. Conformance wiring

- **Frontmatter:** `mode: run|static-error|parse-error`; `expect:` (stdout+exit / diagnostic
  code); `args:`/`stdin:`/`files:`; `nondet: seeds=N`; `since:`; `features:`;
  `adjudicator: ggdef | production-v1 | prose` (`prose` = expectations derived from spec prose
  for rejections NEITHER ggdef-elaboration nor production yet implements — the §2.3
  invariant-#8 findings live here until the rejection ships). Trap output normalized (defined
  panic message shape, file:line normalized) — the normalization rule is spec text.
- **Expectation provenance:** `run/` + elaboration-owned rejections (D4/D5/D6) are
  **ggdef-generated**, human-review-diffed. `parse-error/` expectations derive from the shared
  production parser (trusted-declared — an inversion in name only; stated for honesty). Other
  `static-error/` expectations are production-derived in v1 under `adjudicator: production-v1`
  — a tracked inversion retired at v1.5.
- **Runners:** thin adapters over existing machinery (integration harness for C/LLVM; the
  self-host driver lane; ggdef). `gg sim` is NOT a conformance lane (pending its disposition
  decision, TODO A17; if kept, it may consume spectests informally). Each lane prints a
  `spec_conformance_<impl>` always-pass diagnostic with monotone floors in `tests/lints.rs`.
- **smith:** ggdef joins as the **verdict lane**; tri-state triage (impl bug / spec bug /
  spec silent); spec changes justified only by design intent (invariant #8 as process).
- **Speed:** CI runs ggdef on `spectests/`; nightly runs the full corpus.

## 5. Process

**5.1 Same-PR gating** (post-skeleton): semantics-visible change = prose Δ + ggdef Δ +
spectests + implementations green (or floor-tracked exemption); coverage lint later.
**5.2 Versioning:** `spec-v0.x` tags + changelog; `since:`; living artifact; 1.0 is a spec
release.
**5.3 Docs write-through** (owner directive): each landed rule updates language-design
(incl. §7.4's capture-list examples — currently show bare-name borrow captures, doubly wrong
under D5/§2.5), the book, and the devbook in the same series; the ledger tracks write-through
debt per decision.
**5.4 Copy-guarantees annexe (D1's second half):** small CLOSED list of MUST-NOT-ALLOCATE
positions (bare bind/read/param-pass, borrow field/element read — README:50). Tested
implementation-side via `spectests/annexe/` under `--clones=stats` / counting allocator.
**v1 gates the C backend only** (LLVM rejects `--clones=stats`, open TODO; self-host has only
transient instrumentation) — LLVM/self-host annexe lanes are floor-tracked debt. Spec-side
counterpart: ggdef trace-tags annexe positions.
**5.5 Diagnostic code registry (Phase-1 item):** stable `E_`-codes for spec-referenced
diagnostics; registry maps code → prose section → fixtures; production adopts incrementally.

## 6. Phased delivery

- **Phase 0 — walking skeleton.** Elaborator+evaluator for the HONEST subset the target
  fixtures actually use (measured, passes 2-3): binds/aliases/**materialize-on-write**;
  Vector/**Dict/Set**/struct/String; **Option+Result with `.unwrap()`/`.unwrap_or()`**; **match + user
  payload enums + pattern bindings**; bare/&/! params; **concrete equip method→call incl.
  `equip T with Drop`**; **f-strings (int/string interpolations; no float-printing fixtures
  exist in the target families — §8.2 doesn't block phase 0)**; for/**while** loops;
  **ranges + string slices `s[a..b]`** (the W3c shapes); **named-arg construction**
  (`Point(x=1, y=2)`); **`with expr as name:`**; imports + the v1 shim list (§2.6);
  by-value closures; scope drops; print. **Exclusions are a HARDCODED fixture list, not
  `features:`** (frontmatter is phase 1): the 3 generic-equip cow fixtures
  (`cow_element_borrow_alias_mutate`, `cow_p3_alias_chain_mutate`, `cow_p3_index_mutate`) —
  generic-equip-on-builtin is optional; pull it in only if cheap — plus
  `deadwrite_ok_atomic_add` (std.sync atomics are phase 3; its intended stdout requires
  interior-mutability write-through that v1 GGC cannot express).
  Acceptance: (a) runs the cow_* family (95 programs) minus the exclusion list with output
  matching the ratified expectations (C1–C10); (b) the deadwrite_* PROGRAMS minus the
  exclusion list execute under ggdef with their D1/D2-implied stdout (expectations newly
  ratified; the lint's stderr assertions remain production-side — DeadBareParamWrite is a
  production diagnostic, not GGC semantics); (c) adjudicates the two smith bugs (`9` /
  `ablog`) and the EMove question (pre-mutation value, per D1) from the definition. Import
  ratchet lands with the crate. W3a-d String shapes (devbook/11's W-table, ~:563-566) are IN;
  their clone-count side is D1-allowed variation → annexe.
- **Phase 1 — coverage completion + floors:** the §2.6 rows-1-2 remainder beyond the phase-0
  subset (closures with capture lists, traits/generics/trait objects, comprehensions, the
  long tail of stdlib-free constructs) plus row 3's statics; frontmatter migration (converter
  from the ~1,218 literal harness expectation pairs — 1,212 `run_gg(` + the with_args/
  with_stdin/bench variants — → `ggdef -- gen` regeneration → human-reviewed diff —
  blocked for float-printing fixtures on §8.2); per-impl conformance reports + floors; ggdef
  verdict lane in smith; D4/D5/D6 rejections in elaboration + production + negative fixtures;
  diagnostic-code registry.
- **Phase 2 — annexe + `gg explain` v0 + minimal spec meta.**
- **Phase 3 — sharing & concurrency** (reference cells, scheduler Nondet, B6/B9/B10).
  **v1.5 — executable well-formedness checker** (retires `adjudicator: production-v1`).
  Then mechanization prep (Aeneas trial on eval.rs).

## 7. Risks and guards
Rot → ggdef in CI day 0. Spec-lags-impl → same-PR gate + coverage lint. Divergence-by-
convenience → generated expectations + intent-based review + fresh-pass gauntlet. Perf creep →
charter. Scope creep → §2.6 is the fence. Miri trap → import ratchet. Freeze → §5.2.
Coverage illusion → smith across all lanes + seed sweeps.

## 8. Open items — status
1. **§2.5 capture package: RATIFIED by owner 2026-07-05 (ledger D7).** Liveness-based
   exclusivity included; production's scope-based behavior = filed conformance gap.
2. **Float formatting: DECIDED — shortest round-trip everywhere (ledger D8),** for both
   `print` and `float_to_str`; the formatting appendix specifies the algorithm; Phase-1
   converter unblocked (float-fixture expectations regenerate under the new rule).
3. `E_`-code numbering scheme (bikeshed; Phase 1) — still open.

## Appendix: Prior art & rationale

The design above is a set of choices; this appendix records the prior art those choices
answer to, and why the executable definition takes the shape it does rather than the
alternatives. Two facts about Gorget's own tree frame it. First, `gg sim` (`src/sim/`, a
tree-walking interpreter over GIR, self-described as "analogous to Rust's miri") already
exists — but it consumes GIR, so it executes the clone/move/drop decisions the production
lowering *already made*; it can validate backends against the lowering, but it is
structurally incapable of *defining* what the lowering must mean. It is the Miri trap made
concrete, and it is why `ggdef` must be a separate pass that shares only the frontend
(§2.4's import ratchet is the fence). Second, the enforcement culture `ggdef` plugs into —
ratchet lints, a bootstrap fixed-point test, diagnostic-always-pass `*_comparison` tests
with MATCH counts, an ASan gate, a deterministic-stdout fixture corpus — already supplies
the machinery every guard below needs.

### Prior-art systems

**WebAssembly — the gold-standard triad.** One repository holds three artifacts: the formal
spec (typing + reduction rules over abstract syntax), an OCaml **reference interpreter**
written "for clarity and simplicity, not speed … a device for nailing down exact semantics,"
and a `.wast` **test suite** (`assert_return` / `assert_trap` / `assert_invalid` /
`assert_malformed`). What kept spec and implementations in sync was *process, not tooling*:
the proposal phases require the tests updated at phase 2, a complete reference interpreter at
phase 3, and two independent VMs passing before phase 5 merges spec text + interpreter +
tests together. SpecTec (PLDI 2024) is the earned second generation — a single-source-of-truth
DSL generating the formal spec, prose, and a meta-interpreter that ran all 49,833 spec tests
and found 10 errors in in-flight proposals. **Lesson:** sync is a merge-gate, not a wish; no
feature exists until prose, reference interpreter, and tests land together, and
implementations are measured against the tests, not the prose. (This is §5.1's same-PR gate
and the charter line copied verbatim into the crate root.)

**Standard ML + CakeML.** SML is the canonical "designed with a formal definition" language:
typing rules and operational semantics on paper, bought unambiguous portability and multiple
agreeing implementations. What rotted is instructive: the rules were *paper, never executed*,
and dozens of defects accumulated for a decade until Rossberg built HaMLet, a faithful
rule-by-rule transcription that finally *ran* them; the "final revision" framing also froze
the language into a tombstone. CakeML then showed the modern shape — semantics as a
**functional big-step evaluation function with a fuel clock**, a recursive function rather
than a relation, which is the structure compiler-verification proofs want (divergence
preservation and simulation diagrams as equational reasoning). **Lesson:** a spec nobody
executes silently rots; and if proofs are ever wanted, writing the definition as a
fuel-indexed evaluation function from day one is what ports to a prover mechanically (§2.7).

**Rust's landscape — what starting late costs.** Rust needed four partial artifacts because
its executable definition came a decade late, and each is compromised in a different way: the
**Ferrocene FLS** is descriptive-only and explicitly non-normative (borrow-checker and
const-eval semantics out of scope) — it documents what rustc does, it cannot arbitrate what
rustc *should* do; **Miri** interprets rustc's own MIR, the best UB oracle Rust has but
entangled with the implementation it should judge (exactly `gg sim`'s position); **RustBelt**
is a heroic Coq/Iris soundness proof of an idealized core that permanently trails the real
language. **MiniRust** is the most relevant artifact: an idealized MIR-like core whose
semantics *is an interpreter* written in specr-lang ("the spec itself is code"). Its design
case — precision and accessibility are at odds, math is precise-but-unreadable and prose
readable-but-imprecise, and an interpreter in the community's own language is both — is the
argument for a hand-written definitional interpreter over either a DSL or a proof assistant
first. MiniRust's discipline is also the scoping model: UB is *defined as what the interpreter
detects*, non-determinism is explicit, and scope is deliberately cut (no typeck, no borrow
check, no surface syntax) with incompleteness preferred over premature closure. **Lesson:** a
new language can make the MiniRust-shaped artifact the *first-class* definition while the
language is still small — and must not mistake its GIR-level interpreter for it.

**K Framework (KEVM).** A rewriting-logic framework: write the semantics once, derive
parser/interpreter/symbolic-executor/verifier. KEVM passes the official 40,683-test EVM
conformance suite, the credibility event that proves semantics-first *works*. The catches for
Gorget: KEVM targets a small *frozen* bytecode VM, not a moving surface language; the toolchain
is heavyweight and centered on one vendor; and — telling — WebAssembly, with K available,
chose to build its own DSL rather than adopt it. **Lesson:** semantics-first works, but betting
a living language's definition on an external framework is a tooling/bus-factor risk; the
pragmatic v1 is a hand-written definitional interpreter in the contributors' own language,
with a SpecTec-style DSL as an *earned* v2.

**Others.** *test262* makes tests the currency of proposal advancement (a proposal cannot
advance without coverage), with a low-bar `staging/` tier and machine-readable per-test
frontmatter — the model for §4's frontmatter and `staging/`. *Go* shows that even without
formal semantics a versioned behavior corpus plus a compatibility promise does most of the
conformance work — but Go has no novel semantics axis to pin down; Gorget's lazy CoW does.
*CompCert* states correctness as a **refinement** — compiled behavior refines source
semantics, proved as composed forward simulations — which is the exact shape of Gorget's novel
claim: the definitional interpreter implements **eager value semantics**, and each production
implementation's lazy-CoW output must be observationally equivalent (differentially now,
provably later). *Aeneas/Charon* already translates real Rust (via MIR/LLBC) into Lean4/Coq/F*
and is used in production verification — so a disciplined safe-Rust interpreter keeps a
mechanization path off the shelf.

### Failure modes and the structural guard for each

The precedents above each failed in a characteristic way. The definition is built so that
Gorget's existing enforcement toolkit guards against every one:

| # | Failure mode (precedent) | Structural guard in Gorget |
|---|---|---|
| 1 | Nobody-runs-it rot — SML's paper rules rotted for a decade until HaMLet executed them | `ggdef` runs the full fixture corpus in CI from day 0, gated like `self_host_bootstrap_fixed_point`; a required executor, not a side project |
| 2 | Spec lags implementation — Rust's decade gap; the FLS reduced to describing rustc | Same-PR gate + coverage lint (new fixture / AST node ⇒ spec manifest touched), `tests/lints.rs` style |
| 3 | Divergence-by-convenience — spec quietly edited to match an implementation | Expected outputs generated by `ggdef` + human-reviewed diffs; spec changes justified by design-intent docs only; sequential fresh-agent review passes |
| 4 | Performance creep makes the spec unreadable | Written charter ("clarity, not speed"); `gg sim` absorbs all speed pressure; lint bans `unsafe` and parallelism deps in the spec crate |
| 5 | Scope creep into full formalization — RustBelt-style idealization that never ships | MiniRust scoping: dynamic semantics of GGC first; typeck stays prose + negative fixtures; incompleteness explicit and tracked |
| 6 | The Miri trap — promoting the existing interpreter to "spec" | Hard rule: `gg sim` consumes GIR and is permanently disqualified; the import ratchet keeps `ggdef` off `src/ir/` |
| 7 | Spec freeze — SML's "final revision" killed evolution | Versioned spec releases + changelog + the same-PR process; the spec is a living artifact on the compiler's cadence |
| 8 | Coverage illusion — suite passes, spec still wrong (SpecTec found 10 such errors) | Later: seeded program-generator fuzzing across all executors; seed sweeps for the admitted nondeterminism |
| 9 | UB-agreement trap — "both backends agree / benign UB" verdicts (Core invariant #8's red flag) | The spec interpreter is the arbiter; anything it rejects becomes a negative fixture every implementation must reject; comparison MATCH floors ratchet monotonically |

### Mechanization roadmap (later, non-blocking)

The disciplined subset and the fuel-indexed evaluator keep three doors open, none of which
block v1: (a) **Aeneas/Charon** auto-translation of the Rust interpreter to Lean4; (b) a hand
port to Lean4 (functional big-step ports mechanically — CakeML's clock lesson); (c) a
specr-style transpiler if the subset stays tight. Lean4 is preferred over Coq (Aeneas's
recommended backend, and the community gravity). The first theorem worth mechanizing is the
novel one — **lazy CoW refines eager value semantics on GGC**, a per-construct forward
simulation in the CompCert shape; type soundness comes after. Neither blocks v1; the
differential harness carries the confidence until then.

### Naming and precedent nuggets

*Reference interpreter* and the "clarity, not speed" charter (Wasm); "the spec itself is
code," UB = what the interpreter detects, and pluggable models behind one interface (MiniRust
— Gorget's analog is a pluggable *ownership* model, eager-copy definitional vs. instrumented);
"single source of truth" (SpecTec, the earned v2); the `staging/` tier and YAML frontmatter
(test262); functional big-step with a clock (CakeML); refinement by composed forward
simulations (CompCert — the vocabulary for "lazy CoW refines eager value semantics"); the
faithful-transcription ideal where spec code and reference sections cross-cite (HaMLet);
"semantics-first, credibility = passing the official suite" (KEVM). The names this RFC adopts —
core language **GGC**, binary **`ggdef`**, suite directory **`spectests/`**, and the model
slogan **"CoW is an optimization; value semantics is the meaning"** — come from this survey.

**Key sources:** WebAssembly spec + reference-interpreter README + phase process; SpecTec
(PLDI 2024, `10.1145/3656440`); MiniRust (`github.com/minirust/minirust`) + Ralf Jung's design
post; Ferrocene FLS; Rossberg's SML defects list + HaMLet; CakeML functional big-step semantics
(ESOP '16); KEVM (CSF 2018); test262 contributing guide + TC39 process; Aeneas
(`github.com/AeneasVerif/aeneas`).
