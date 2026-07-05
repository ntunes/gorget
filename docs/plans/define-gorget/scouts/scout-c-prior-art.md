# Scout C — Prior Art Survey: Executable Language Definition for Gorget
Status: COMPLETE (2026-07-05). All web claims verified this session; repo claims verified against current worktree source.

## Repo grounding (verified this session)
- Pipeline: `.gg → AST → GIR → LIR → BIR (newtype over LirModule) → C | LLVM backend` (devbook/16). BIR's newtype makes it type-impossible for backends to see canonical ops.
- **CoW decisions are made in GIR lowering** (`src/ir/lowering/context.rs`, `stmts/assigns.rs`, `exprs/methods.rs`): each local carries `LocalOwnership`; borrows propagate as `Ptr` aliases; clones inserted only at ownership boundaries; fully-lazy = clone deferred to the mutation site. Safety pass is diagnostic-only for CoW (devbook/11).
- **Gorget already has a Miri**: `gg sim` (`src/sim/`, ~10.4K LOC) is a tree-walking interpreter **over GIR**, self-described "analogous to Rust's miri" (`src/sim/mod.rs:6`), with `tests/sim_ub/` differential harness and `--many-seeds` (devbook/21). Because it consumes GIR, it executes clone/move/drop decisions the production lowering ALREADY MADE — it can validate backends against the lowering, but it cannot *define* what the lowering must mean. It is structurally incapable of being the spec of the CoW model.
- Implementations to conform: (1) Rust gg → C backend, (2) Rust gg → LLVM backend, (3) self-hosted frontend; plus sim as a 4th executor.
- Enforcement toolkit already in the culture: ratchet lints (`tests/lints.rs`, e.g. floors runtime_diff 1093), bootstrap fixed-point test, diagnostic-always-pass `*_comparison` tests with MATCH counts, ASan gate, ~1069 deterministic-stdout fixtures.

---

## (a) Prior-art systems

### 1. WebAssembly spec — the gold standard triad
One repo (`WebAssembly/spec`) holds three artifacts: the formal spec document (typing + reduction rules over *abstract syntax*), the OCaml reference interpreter (`interpreter/`), and the spec test suite (`test/core/*.wast`). The interpreter README states it is "written for clarity and simplicity, not speed … a device for nailing down exact semantics," in a "declarative, speccy" style — an explicit anti-performance charter. Tests are `.wast` scripts: modules plus `assert_return` / `assert_trap` / `assert_invalid` / `assert_malformed` assertions; the reference interpreter runs them natively, and engines consume them via converters (wast2json → JSON manifest + `.wasm` binaries) and the `WebAssembly/testsuite` mirror repo. What kept spec and implementations in sync was **process, not tooling**: the proposal phase gates require the test suite updated at Phase 2, a *complete* reference-interpreter implementation at Phase 3, and two independent VMs passing the tests at Phase 4, before Phase 5 merges spec text + interpreter + tests into main together. SpecTec (PLDI 2024) is the second-generation move: a DSL as single source of truth generating the typeset formal spec, prose pseudocode, and a meta-interpreter that ran all 49,833 spec tests in 58s and found 10 errors in 5 in-flight proposals.
**Transferable lesson:** sync is a merge-gate, not a wish — no feature exists until spec text, reference interpreter, and tests land in the same tree, and implementations are measured against the tests, not the prose.
Sources: https://github.com/WebAssembly/spec/blob/main/interpreter/README.md · https://github.com/WebAssembly/meetings/blob/main/process/phases.md · https://dl.acm.org/doi/10.1145/3656440

### 2. The Definition of Standard ML + CakeML
SML is the canonical "language designed with a formal definition": typing rules + operational semantics on paper (1990, revised 1997), with metatheory as an explicit design goal. It bought unambiguous portability and multiple independently developed implementations that actually agreed. What rotted: the rules were **paper**, never executed — Kahrs' "Mistakes and Ambiguities" and Rossberg's defects list (https://people.mpi-sws.org/~rossberg/sml-defects.html) accumulated dozens of errors found only when Rossberg built HaMLet, a "faithful transcription" implementation whose code mirrors the Definition rule-by-rule. The Definition's "final revision" framing also froze the language — the spec became a tombstone rather than a living contract. CakeML then showed the modern shape: semantics as a HOL definition in **functional big-step style with a clock (fuel)** — a recursive evaluation *function*, not a relation — which the CakeML papers argue is the structure best suited to compiler-verification proofs (divergence preservation, simulation diagrams as equational reasoning) (https://cakeml.org/esop16.pdf).
**Transferable lesson:** a spec nobody executes accumulates silent defects for a decade; and if you ever want proofs, write the definition as a *fuel-indexed evaluation function* from day one — that structure ports to a theorem prover mechanically.

### 3. Rust's landscape — what starting late costs
- **Ferrocene FLS**: descriptive English spec built for toolchain *qualification*, donated to the Rust project 2025. Explicitly NOT normative, NOT a conformance vehicle between compilers; borrow-checker mechanics and const-eval semantics are explicitly out of scope (https://spec.ferrocene.dev/general.html). It documents what rustc does; it cannot arbitrate what rustc *should* do.
- **Miri**: interprets rustc's own MIR — the best UB oracle Rust has, but it inherits every lowering decision rustc makes; it is entangled with the implementation it should judge. Gorget's `gg sim` is precisely this (over GIR).
- **MiniRust** (https://github.com/minirust/minirust): the most relevant artifact. An "idealized MIR-like" core language whose semantics is *an interpreter* written in **specr lang** — pseudo-Rust that `specr-transpile` turns into real, runnable Rust ("the spec itself is code"). Ralf Jung's design case (https://www.ralfj.de/blog/2022/08/08/minirust.html): precision and accessibility are at odds; math is precise but unreadable, prose readable but imprecise; an interpreter in the community's own language is both — "anyone who knows Rust should immediately be able to understand" it. Key mechanics: UB is *defined as what the interpreter detects*, checked at every step; non-determinism is explicit via `Nondet<T>` (daemonic/angelic); the memory model is **pluggable** (Basic, Tree Borrows, int-ptr casts live side by side under one interface); scope is deliberately cut — no type checking, no borrow checking, no surface syntax (elaboration is someone else's problem; a-mir-formality owns traits/typeck). Tooling: `tooling/minimize` drives rustc to translate real Rust (via MIR) into MiniRust so real test programs execute against the spec; Miri divergences are triaged as Miri bugs. Explicitly, proudly incomplete (no floats yet) — incompleteness over premature closure.
- **Stacked/Tree Borrows**: aliasing models shipped as executable checkers inside Miri, iterated by running them over the crates.io corpus — an executable model enables *empirical* semantics design.
- **RustBelt**: Coq/Iris soundness for an idealized core (λ_Rust); heroic, valuable, and permanently trailing the real language.
**Transferable lesson (the big one):** Rust needed four partial artifacts because the executable definition came 10+ years late; each is either impl-entangled (Miri), descriptive-only (FLS), idealized (RustBelt), or still catching up (MiniRust). A new language can make the MiniRust-shaped artifact the *first-class* definition while the language is still small — and Gorget's `gg sim` must not be mistaken for it.

### 4. K Framework (KEVM, KCC)
K is a rewriting-logic framework: write the semantics once, derive parser/interpreter/symbolic-executor/verifier. KEVM is its flagship: a complete EVM semantics that passes the official 40,683-test EVM conformance suite, with the generated interpreter about one order of magnitude slower than the C++ reference; first release-quality KEVM took ~2 dev-months (https://fsl.cs.illinois.edu/publications/hildenbrandt-saxena-zhu-rodrigues-daian-guth-moore-zhang-park-rosu-2018-csf.pdf). The catches for Gorget: KEVM's target is a small, *frozen* bytecode VM, not a moving 100K-line surface language; the K toolchain is heavyweight (its own frontend, Haskell/LLVM backends), commercially centered on one company (Runtime Verification); K definitions have rarely become community-normative outside blockchain; and — telling — WebAssembly, with K available and a K-Wasm semantics existing, chose to build its own DSL (SpecTec) rather than adopt K.
**Transferable lesson:** semantics-first *works* (KEVM's credibility event was passing the official suite), but betting a living language's definition on an external framework is a tooling/bus-factor risk; the pragmatic v1 is a hand-written definitional interpreter in the contributors' own language, with a SpecTec-style DSL as an *earned* v2, not a starting point.

### 5. Others
- **test262** (https://github.com/tc39/test262): one canonical conformance suite; TC39 proposals *cannot advance* (Stage 2.7→4) without sufficient test262 coverage; a `test/staging/` directory takes lower-bar, mechanically-converted tests from engines' private suites to get early cross-engine interop, but staging doesn't count toward stage-advancement coverage. Tests carry YAML frontmatter metadata (features, flags, negative-expectations) so any engine's harness can select and run them. **Lesson:** tests-as-stage-gate + a low-friction staging tier + machine-readable per-test metadata.
- **Go**: prose spec (readable, non-formal) + a `test/` behavior-test corpus in-tree + the Go 1 compatibility promise; secondary implementations (gccgo, TinyGo) track the suite. **Lesson:** even without formal semantics, a versioned behavior corpus plus a compatibility promise does most of the conformance work — but Go has no novel semantics axis to pin down; Gorget's lazy CoW does, so Gorget needs more than Go had.
- **CompCert**: correctness stated as a *refinement* — compiled behavior refines source semantics — proved as per-pass forward simulations composed transitively. **Lesson (the shape of Gorget's novel claim):** state lazy CoW as a refinement: the definitional interpreter implements **eager value semantics** (conceptual deep-copy at every ownership boundary, no clone-placement cleverness), and each production implementation's lazy-CoW output must be observationally equivalent — checked differentially now, provable as a simulation later.
- **Aeneas/Charon** (https://github.com/AeneasVerif/aeneas): working toolchain translating real Rust (via MIR/LLBC) into Lean4/Coq/F*/HOL4 pure models; used in Microsoft's SymCrypt Rust verification. **Lesson:** if the definitional interpreter is written in a disciplined safe-Rust subset, a Rust→Lean4 mechanization path already exists off the shelf.

---

## (b) Recommendation

### Formalism: hand-written definitional interpreter, in Rust, in a "spec subset"
- **Not K**: external-toolchain bet, bus factor, integration friction with a 181K-LOC Rust monorepo CI; even Wasm built its own thing instead. **Not a DSL/SpecTec-first**: that's a second compiler project; Wasm earned it after a decade of the hand-written triad. **Not Lean4-first**: blocks v1 on mechanization; MiniRust's accessibility argument applies squarely. **Not Gorget-itself** (yet): circular trust pre-1.0 — a spec that can't run when the language it defines has a bug is a broken oracle; a Gorget port later is a great extra conformance point and showcase.
- Write it as MiniRust writes specr: **safe Rust only, no unsafe, no interior mutability tricks, pure data, and one fuel-indexed step/eval function** (CakeML's functional-big-step-with-clock structure — this single choice keeps Lean4 mechanization mechanical later). Explicit `Nondet` points for the only nondeterminism the language admits (HashMap iteration order, task scheduling — reuse the `--many-seeds` idea already in `gg sim`).
- Charter sentence copied from Wasm, written into the crate root: *"clarity and simplicity, not speed."* `gg sim` stays the fast oracle so nobody is ever tempted to optimize the spec.

### Where it sits: a desugared core, sharing only the parser
Define **Gorget Core (GGC)** — a small, sugar-free, explicitly-resolved core language — and split the definition MiniRust-style:
1. **Syntax**: reuse the production lexer+parser (declared trusted; the grammar's paper spec stays in `docs/language-reference.md`). Parse behavior is still conformance-tested via negative fixtures. Rationale: the parser is big, boring, and not where Gorget's novelty lives; duplicating it is where this project would die of scope.
2. **Spec elaboration** (part of the spec): a NEW, simple, independent pass AST → GGC. It must NOT reuse GIR lowering — GIR lowering is where the production compiler makes clone/move/borrow placement decisions, which are exactly the decisions the definition must not inherit. Enforce with an import lint: the spec crate may depend only on lexer/parser/AST modules (ratchet lint in `tests/lints.rs` on the spec crate's imports).
3. **Definitional interpreter over GGC** implementing **eager value semantics**: assignment/argument-pass/collection-read behave as conceptual copies of immutable values (plus the explicit single-owner carve-outs: Box/Task/Owned/closures move); drops at scope exit in specified order; UB-class conditions *detected and reported*, MiniRust-style. Lazy CoW does not exist in the spec — it is each implementation's private refinement obligation. This is the CompCert-shaped claim: **lazy CoW refines eager value semantics**, differential-tested now, provable later.
4. **Static semantics**: v1 keeps type inference and the safety diagnostics as prose + negative-fixture conformance (expected error codes); v1.5 adds an executable *well-formedness checker over GGC* (cheap, catches elaboration bugs); full executable typechecking is a later milestone. This is MiniRust's scoping discipline — cut typeck, keep the operational core precise.
5. **`gg sim` is explicitly NOT the spec.** Keep it as the Miri-analog: it validates backends against the production lowering; the definitional interpreter validates the lowering itself. Two oracles, different layers, both wired into CI.

Repo shape: new workspace crate (e.g. `spec/` or `def/`, binary `ggdef`), living in the same monorepo as compiler + tests (Wasm lesson: one tree, one merge gate).

### Conformance suite: structure, format, versioning
- Evolve `tests/fixtures/*.gg` into the spec-test corpus. Add machine-readable frontmatter per fixture (test262 YAML + wast assertions, adapted): `run` + expected stdout/exit, `static-error <E_code>` (+ span), `parse-error`, `nondet: seeds=N` for seed-swept tests, `since: <spec-version>`, `features: [...]`.
- **The spec interpreter generates the expected outputs; humans review the diff; the files are committed.** Implementations are then compared against those committed expectations by their own thin runners (the existing harness already dispatches C/LLVM via `GG_BACKEND`; self-host via the comparison harness). This inverts today's arrangement — expectations flow *from the definition*, not from whichever backend was written first.
- Conformance scoring: a `spec_conformance_<impl>` diagnostic-always-pass test per executor (C, LLVM, self-host, sim) printing MATCH/MISMATCH counts — exactly the existing `*_comparison` pattern — with monotone floors ratcheted in `tests/lints.rs` (exactly the existing runtime_diff-floor pattern). Divergences triaged tri-state and recorded: impl bug / spec bug / spec silent. Never silently patch the spec to match an implementation.
- A `staging/` tier (test262 lesson): low-bar fixtures contributed from debugging sessions run everywhere immediately but don't count toward feature-coverage requirements until promoted.
- Versioning: spec-interp, tests, and implementations version together in the monorepo; tag spec releases (`spec-v0.x`) with a changelog; tests carry `since:`. Export a generated mirror (Wasm `testsuite` model) only if external implementations ever appear.

### Evolution process for a moving pre-1.0 language
- **Same-PR gating, not spec-first.** Strict spec-first is too heavy pre-1.0. The rule: any semantics-visible change lands as one series containing (1) `language-reference.md` prose delta, (2) `ggdef` implementation, (3) spec tests, (4) all production implementations passing — or an explicit, floor-tracked exemption in TODO.md. Enforce with an arm-count-style lint: new fixture or new AST node ⇒ spec-coverage manifest touched.
- **Review rule against divergence-by-convenience:** a spec diff is justified by `docs/language-design.md` intent, never by "matches the implementation." This is Core invariant #8 ("reference-grade, not parity") promoted into the spec process — "both backends agree" is not conformance; the spec-interp is the arbiter, and programs the spec rejects must be rejected by every implementation.
- Compressed Wasm phase gates for larger features: prototype behind a flag (staging tests ok) → `ggdef` complete + spec tests promoted → all implementations pass → prose merged. No feature is "done" at fewer than all four artifacts.

### Mechanization path (later, non-blocking)
- The disciplined subset + fuel-indexed evaluator keeps three doors open: (a) **Aeneas/Charon** auto-translation of the Rust interpreter to Lean4 (toolchain exists today, SymCrypt-proven); (b) hand-port to Lean4 (functional big-step ports mechanically — CakeML's clock lesson); (c) a specr-style transpiler if the subset stays tight.
- First theorem worth mechanizing is the novel one: **lazy CoW refines eager value semantics on GGC** — per-construct forward simulation, CompCert-shaped. Type soundness comes after. Neither blocks v1; the differential harness carries the confidence until then.
- Prefer Lean4 over Coq: Aeneas's recommended backend, momentum, and the Rust community gravity.

---

## (c) Risks / anti-patterns and structural guards (wired to Gorget's toolkit)

| # | Failure mode (precedent) | Structural guard in Gorget |
|---|---|---|
| 1 | **Nobody-runs-it rot** — SML's paper rules accumulated defects for a decade until HaMLet executed them | `ggdef` runs the full fixture corpus in CI from day 0, gated like `self_host_bootstrap_fixed_point`; it is a required executor, not a side project |
| 2 | **Spec lags implementation** — Rust's 10-year gap; FLS reduced to describing rustc | Same-PR gate + coverage lint (new fixture/AST node ⇒ spec manifest touched), `tests/lints.rs` style |
| 3 | **Divergence-by-convenience** — spec quietly edited to match impl | Expected outputs generated by `ggdef` + human-reviewed diffs; spec changes justified by design-intent docs only; sequential fresh-agent review passes (existing culture) |
| 4 | **Performance creep makes the spec unreadable** | Written charter ("clarity, not speed", Wasm); `gg sim` absorbs all speed pressure; lint: no `unsafe`, no parallelism deps in the spec crate |
| 5 | **Scope creep into full formalization** — RustBelt-style idealization that never ships | MiniRust scoping: dynamic semantics of GGC first; typeck stays prose+negative-fixtures; incompleteness is explicit and tracked, not hidden |
| 6 | **The Miri trap** — promoting the existing interpreter to "spec" | Hard rule: `gg sim` consumes GIR (production clone/move decisions baked in) and is permanently disqualified as the definition; import lint keeps `ggdef` off `src/ir/` |
| 7 | **Spec freeze** — SML's "final revision" killed evolution | Versioned spec releases + changelog + the same-PR process; the spec is a living artifact with the same cadence as the compiler |
| 8 | **Coverage illusion** — suite passes, spec still wrong (SpecTec found 10 errors in proposals that "passed") | Later: seeded program-generator fuzzing across all 5 executors (spec, C, LLVM, self-host, sim); seed sweeps for nondet (HashMap order, scheduling) |
| 9 | **UB-agreement trap** — "both backends agree / benign UB" verdicts (Core invariant #8's exact red flag) | The spec-interp is the arbiter; anything it rejects becomes a negative fixture all implementations must reject; comparison MATCH floors ratchet monotonically |

## (d) Naming / precedent nuggets worth stealing
- **Wasm's triad**: "formal notation + reference interpreter + test suite," one repo, phase-gated (tests @P2, complete ref-interp @P3, two passing implementations @P4). The word *reference interpreter* and the "clarity, not speed" charter line.
- **MiniRust's stance**: "the spec itself is code"; UB = what the interpreter detects; `Nondet<T>`; pluggable memory/aliasing models behind one interface (Gorget analog: pluggable *ownership* model — eager-copy definitional vs. instrumented variants); "an idealized Miri"; `minimize` (real programs lowered into the core to test the spec at scale).
- **SpecTec**: "single source of truth" — the earned v2 where one artifact generates prose, formal text, and interpreter.
- **test262**: staging directory; YAML frontmatter; tests as stage-advancement currency.
- **CakeML**: functional big-step with a clock — the interpreter shape that proofs love.
- **CompCert**: refinement by composed forward simulations — the vocabulary for "lazy CoW refines eager value semantics."
- **HaMLet**: the "faithful transcription" ideal — spec-interp code and `language-reference.md` sections cross-cite each other.
- **KEVM**: "semantics-first"; credibility = passing the official suite.
- Suggested names: core language **"Gorget Core" (GGC)**; binary **`ggdef`**; suite directory **`spectests/`**; the refinement claim **"CoW is an optimization, value semantics is the meaning."**

## Key sources
- https://github.com/WebAssembly/spec/blob/main/interpreter/README.md
- https://github.com/WebAssembly/meetings/blob/main/process/phases.md
- https://dl.acm.org/doi/10.1145/3656440 (SpecTec, PLDI 2024)
- https://github.com/minirust/minirust · https://www.ralfj.de/blog/2022/08/08/minirust.html
- https://spec.ferrocene.dev/general.html
- https://people.mpi-sws.org/~rossberg/sml-defects.html · https://people.mpi-sws.org/~rossberg/hamlet/ · https://dl.acm.org/doi/10.1145/3386336 (History of SML)
- https://cakeml.org/esop16.pdf (Functional Big-step Semantics)
- https://fsl.cs.illinois.edu/publications/hildenbrandt-saxena-zhu-rodrigues-daian-guth-moore-zhang-park-rosu-2018-csf.pdf (KEVM)
- https://github.com/tc39/test262/blob/main/CONTRIBUTING.md · https://tc39.es/process-document/
- https://github.com/AeneasVerif/aeneas
