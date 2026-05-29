# Gorget Compiler Internals Book ("devbook") — Plan

> **Status:** Plan APPROVED via 6-pass fresh-agent review (2026-05-29) — converged from 10 reservations to a clean SIGN OFF. TOC + fold-map + fold protocol are settled. Execution (scaffold → per-chapter fold) NOT yet started — see Execution sequence. This doc is the committed reference; the devbook itself will live at `docs/devbook/`.

## Goal & audience

A **reference-first** internals book for the Gorget *compiler developers* (NOT end users writing Gorget application code), teaching a newcomer the compiler frontend→backend. Plain markdown at `docs/devbook/`, mirroring `docs/book/` conventions (numbered chapters + README TOC). "Reference-first" = coverage accuracy is the priority; a narrative "follow the value" spine and a contributor playbook are stubbed now, deepened later.

Distinct from the other corpora by axis (see decision 10): `docs/book/` = how to *use* the language; `language-reference.md` = *what* the language is (normative); `language-design.md` = *why* (philosophy); **devbook = how it's *implemented***.

## Strategy decisions

1. **One authoritative source** (= layering-discipline rule 3, applied to docs). The book is the sole authoritative home for *design narrative / architecture / the "why"*. For *live behavioral facts* (comparison scores, ABI offsets, branch tables, which-fn-does-what, numeric budgets, `file:line`), the *code/tests* stay authoritative and the book **cites** them — never copies. **All numeric figures and `file:line` cites inside folded docs are presumed stale**: re-derive each from current source at fold time; never transcribe. Cross-doc numeric disagreements → TODO.md as CONTRADICTION (illustrative, non-exhaustive examples: Tier-3b proxy-read budget reads 77/64 in docs vs **78** in `tests/lints.rs:467`; CoW materialization-point count **7** in `copy-on-write.md:215` + `self_host_lowerer/lower.gg:447` vs **6** in `feedback_cow_design_clarity.md`).

2. **Fold + repoint, then delete.** Each chapter absorbs its `docs/internals/` deep-dive(s); that subsystem's source `// See docs/internals/X.md` comments repoint to the chapter's stable anchor; `docs/internals/` shrinks to empty and is then deleted. Repoint scope = ALL live citations: `src/**`, `tests/**`, `CLAUDE.md`, `docs/book/**`. Historical logs (`DONE.md`, closed `TODO.md` items) are NOT rewritten; live `TODO.md` items are repointed.

3. **Lazy migration — touch nothing yet.** No internals file is moved/deleted and no source comment repointed until the chapter that absorbs it is actually written. Migration happens per chapter, not up front.

4. **Section-granular citable anchors from day one.** Chapters use stable `#section` anchors so source comments can point at e.g. `docs/devbook/16-bir.md#the-only-primitive-ops-invariant`, preserving the `§N`-level precision that existing citations carry.

5. **Delete the leaf handover doc.** `handover-option-c-bir-synthesis.md` is cited nowhere (grep-confirmed) → deleted outright (not archived) when Ch.16 lands.

6. **Fold protocol — four dispositions, IMPL-AHEAD is universal.** When folding any (hybrid) internals doc, classify each piece of content as:
   - **(a) evergreen** architecture/rationale → lift to chapter, re-derived from current source (cite, never copy);
   - **(b) live roadmap** → route to TODO.md;
   - **(c) dead status** → drop;
   - **(d) IMPL-AHEAD stale status** — body describes now-shipped work in false present/future tense → lift only the evergreen rationale, re-derive all status from source, log the doc-vs-source drift as IMPL-AHEAD → TODO (decision 10).

   **Disposition (d) applies to EVERY folded doc by default** — it is NOT gated on a hand-maintained "which docs are stale" list (that list's completeness was load-bearing and drift-prone — the exact anti-pattern this book fights). The trigger is per-sentence and mechanical: every status / present-tense / future-tense claim is presumed stale and re-verified against current source at fold time. The "known offenders" list below is **illustrative and explicitly non-exhaustive**; a stale doc not on it is still caught.
   *Known IMPL-AHEAD offenders (examples):* `unified-resource-model.md` (§8.x, §6.8), `clone-emission-at-calls.md` ("in progress" — shipped), `extern-modules.md` ("Future: borrowed qualifier" — shipped), `lir-design.md` (§"Phase 5 LLVM — Planned") + `llvm-backend-plan.md` (plan for an already-shipped backend), `ownership-ir.md` ("future" LoadRef/StoreRef — shipped), `self-host-resource-model.md` ("Phase C IN PROGRESS"), `method-level-inference.md` ("Not yet implemented" header), `stdlib-design.md` (roadmap markers). (`safety-checker.md` verified clean — the universal check is "re-verify, most pass," not "rewrite everything.")

7. **Self-host mirror — same book, not separate.** Each phase/area chapter carries an **"In the self-host"** section: how the Gorget self-host implements the same area, divergences from the Rust `gg`, and the current parity. The parity number is obtained via a stated **procedure** (`cargo test --test integration <name>_comparison -- --nocapture`, read the printed matched-count) plus an optional freshness-stamped dated reading — NOT cited as a stable fact, because the `*_comparison` tests are diagnostic-always-pass (single `!fixtures.is_empty()` sanity `assert!`; counts only `eprintln!`'d). Where self-host has no/partial coverage (most backend chapters; `c_emit` ~64% w/ crashes), the section says so plainly and points at the gap. Ch.26/27 remain the *system-level* self-host treatment; per-chapter sections are the *area-level* mirror.

8. **Pre-migration dangling-link repair.** A complete `grep -rn "docs/internals/[a-z0-9-]*\.md" src tests docs CLAUDE.md` enumerates the full dangling set before migration. Known dead docs (do not exist):
   - `closure-capture.md` ← `src/ir/validate.rs:1943`, `src/ir/lowering/closures.rs:149` (→Ch.12), `docs/internals/structural-guards.md:162` (→Ch.25). Repoint all 3.
   - `lir-correctness-roadmap.md` (superseded by `unified-resource-model.md`, per its line 3) ← `src/lir/runtime.rs:6` (→Ch.18); the unified:3 self-reference is removed when unified folds.
   "No dangling link" is the **end state** the migration achieves (the tree has these dangling at baseline); a final grep must return zero dangling.

9. **Corrected pipeline + subsystem ordering.** `internals/README.md`'s pipeline diagram lists a non-existent "Pass 4.5 provenance.rs" — stale. Actual pipeline (`src/semantic/mod.rs`): Pass 0 meta → 0.5 derive → 1–2 resolve → 2.5 rewrite → 3 traits → 3.6 cycle_check → 4 typecheck → 4.5 `apply_inferred_method_targs` → 4.6 `lint_suggest_throws` → 5 safety. There is NO provenance pass and NO `provenance.rs`. Ch.1 folds a corrected diagram. Chapters are **subsystem-ordered, not pass-ordered** (stated in Ch.0).

10. **Cross-doc consistency & honesty gate.** Each chapter's accuracy pass triangulates THREE corpora — `source+tests` / `language-reference.md`+`language-design.md` / `docs/book/` — and classifies every disagreement as **CONSISTENT / DOC-AHEAD-OF-IMPL / IMPL-AHEAD-OF-DOC / CONTRADICTION**. DOC-AHEAD subdivides into *implement-candidate* vs *cleanup-of-rejected-feature* (e.g. Cell/RefCell are rejected → cleanup-only). DOC-AHEAD items come to the user for the implement-vs-cleanup call; all findings → TODO.md. The book cites `language-reference.md` for normative facts and never restates the spec. The devbook gate is the forcing function that keeps all four corpora honest.

## TOC + fold-map (28 chapters, 9 parts)

| # | Chapter | Primary source | Folds in (disposition) |
|---|---------|----------------|------------------------|
| **P0** | **Orientation** | | |
| 0 | How to read this book | — | one-source contract; honesty mandate; fold protocol; freshness stamps; subsystem-ordered note; "In the self-host" convention |
| 1 | Pipeline & the `gg` driver | `src/main.rs`, `src/tui.rs` | corrected `internals/README.md` pipeline; all subcommands (lex/parse/check/build/run/profile/test/fmt/sim/new/add) + REPL |
| 2 | Foundations: spans, interning & diagnostics | `src/span.rs`, `src/intern.rs`, `src/errors.rs` | — |
| **P1** | **Frontend** | | |
| 3 | Lexer & indentation | `src/lexer` | — |
| 4 | Parser & AST | `src/parser` | `fstring-interp-as-expr.md` (uncited → fold+delete, no repoint; cross-cutting — forward-ptrs from Ch.7/9) |
| 5 | The formatter (`gg fmt`) | `src/formatter` | — |
| **P2** | **Semantic analysis (pass order)** | | |
| 6 | Meta & derive (Pass 0 / 0.5) | `meta.rs`, `derive.rs` | `meta.md` |
| 7 | Name resolution & scopes (Pass 1–2) | `src/semantic/resolve.rs` | — |
| 8 | Traits & impl registry (Pass 3) | `traits.rs` | — |
| 9 | Type inference & checking (Pass 4/4.5/4.6) | `typecheck.rs` | `method-level-inference.md` (IMPL-AHEAD) |
| 10 | Ownership: moves & borrows (Pass 5 safety) | `src/semantic/safety/` | `safety-checker.md`, `shared-keyword-design.md` |
| **P3** | **Ownership, CoW & resources** | | |
| 11 | Copy-on-write & view provenance | `src/ir/lowering/{context,stmts/assigns,exprs/methods}.rs`, `src/backend/c_lir/emit_call_extern.rs`, `src/semantic/safety/origins.rs` | `copy-on-write.md` |
| **P4** | **IR lowering (GIR)** | | |
| 12 | GIR & lowering: monomorphization, drop insertion, closures | `src/ir`, `src/ir/lowering/closures.rs` | `unified §1-2`; repoints `closure-capture.md` |
| 13 | Ownership in the IR | `src/ir/lowering` | `ownership-ir.md` (IMPL-AHEAD), `clone-emission-at-calls.md` (IMPL-AHEAD), `unified §3 Phase A` + `§6.1-6.7 Phase D` |
| **P5** | **LIR** | | |
| 14 | LIR & SSA | `src/lir` | `lir-design.md` (IMPL-AHEAD), `lift-plan` LIR-side cites, `unified §6.8` + `§8.2/8.5/8.6` |
| 15 | Drop elaboration & optimization | `src/lir/drop_elab.rs` | `unified §8.1 + §8.4` |
| **P6** | **BIR & backends** | | |
| 16 | BIR: backend-agnostic synthesis & validation | `src/bir` (6253 LOC) | `bir-module-synthesis-plan.md`, `lift-plan` BIR-side cites; cite 11-arm validator + `main.rs:658/705/1467`; DELETE `handover-option-c-bir-synthesis.md`; validate.rs = instance of Ch.25 framework |
| 17 | The C backend | `src/backend/c_lir` | `codegen-gap-spike.md`, `tier1c-cluster1-burn-down.md` |
| 18 | The runtime & the backend ABI contract | `src/backend/c` (`c_runtime.rs`) + shipped runtime-decl infra (`RuntimeFn`, `compiler/data/resources.gg` SSoT, `crate::ir::resources::table()`, `src/compiler_data.rs`, `src/ir/resources.rs`, `src/ir/resource_schema.rs`) | `unified §3.6`; repoints `calls.rs:278` + `runtime.rs:6` (RUNTIME_DECLS/resources.toml build-tooling = roadmap → TODO, not a source) |
| 19 | The LLVM backend | `src/backend/llvm` (~6735 LOC, shipped, C-parity) | `llvm-backend-plan.md` (IMPL-AHEAD — lift rationale, re-derive status) |
| 20 | Extern, interop & GPU backends | extern, Metal/GL | `extern-modules.md` (IMPL-AHEAD); GPU half net-new (no internals doc) |
| **P7** | **Other consumers & tooling** | | |
| 21 | The simulator / interpreter (`gg sim`) | `src/sim` (~10K LOC GIR reference oracle) | — |
| 22 | Modules, loading & package management | `src/loader.rs`, `lockfile.rs`, `manifest.rs`, `src/resolver.rs` (dep resolver — ≠ Ch.7 name resolution) | — |
| 23 | The stdlib narrow waist | stdlib, `typecheck.rs` | `stdlib-design.md` (IMPL-AHEAD) |
| **P8** | **Cross-cutting laws** | | |
| 24 | Layering discipline | all layers | `layering-discipline.md` |
| 25 | Structural guards | `src/ir/validate.rs` + `src/lir/validate.rs` + `src/bir/validate.rs` (framework + 3 instances) | `structural-guards.md`, `unified §5 Phase C` + `§8.3` |
| **P9** | **The self-host (system-level)** | | |
| 26 | The self-host frontend | `tests/fixtures/self_host_*` | `self-host-resource-model.md` (IMPL-AHEAD) |
| 27 | Comparison, bootstrap & report generation | `tests/integration.rs` (`*_comparison`, `bootstrap_fixed_point`, parity north-star), `src/report.rs`, harnesses (`lir_ab.rs`, `lints.rs`, `security.rs`, `str_runtime.rs`, `sim_ub`) | — |
| **App A** | Subsystem → file map | — | dir → responsibility + LOC; disambiguate `resolver.rs` (deps) vs `resolve.rs` (names) |
| **App B** | Glossary | — | GIR/LIR/BIR/SSA/CoW/MoveZero/view/narrow-waist/provenance/RuntimeFn… |
| **Stubs** | Follow-the-value walkthrough; contributor playbook (debugging heuristic, "don't redesign around gaps," worked snags #13/#17) | — | deepened post-coverage |

## `unified-resource-model.md` §→chapter split (fans across the most chapters)

| Section | Home | Notes |
|---------|------|-------|
| §1-2 overview | Ch.12 | intro context |
| §3 Phase A type-axis metadata (GIR-side) | Ch.13 | |
| §3.6 runtime declaration table (`RuntimeFn` + `resources.gg` SSoT) | Ch.18 | RUNTIME_DECLS/resources.toml build-tooling is unshipped → TODO |
| §4 Phase B (deferred) | — | dropped/noted, not a source |
| §5 Phase C strict move/clone validation | Ch.25 | validator framework |
| §6.1-§6.7 Phase D (GIR-side LocalOwnership/BorrowOrigin) | Ch.13 | |
| §6.8 Phase D LIR-side per-value provenance (`Slot.origin`) | Ch.14 | IMPL-AHEAD |
| §8.1 drop-flag + §8.4 optimizer fixpoint | Ch.15 | IMPL-AHEAD |
| §8.2 SSA/critical-edge invariants + §8.5 cross-block opts + §8.6 `LirType::FuncRef` | Ch.14 | IMPL-AHEAD |
| §8.3 per-pass validator framework | Ch.25 | |
| §8.7 "already shipped" + §9 sequencing + §11 open-questions | — | TODO.md / dropped |

## Documentation-honesty audit — seed catalog (decision 10)

Findings surfaced during planning (each → TODO.md as it's confirmed):
1. **IMPL-AHEAD — BIR `mod.rs` header** says "Step 0 passthrough / empty allowlist"; reality is a 6253-LOC shipped subsystem. (Logged.)
2. **DOC-AHEAD (cleanup-of-rejected) — Cell/RefCell + Weak cycle-breaking.** `docs/book/16-smart-pointers.md:109-127` + `language-reference.md §4.5` describe rejected primitives; cycle-refs settled on arena/ECS (`std.slotmap`). (Logged.)
3. **CONTRADICTION — `language-design.md` ⟷ `language-reference.md` normative overlap.** ~3400/3561 lines of design.md restate normative spec that reference.md owns (which reference.md:5 declares as design.md's job — design.md violates the split). Candidate: trim design.md to philosophy-only.
4. **CONTRADICTION — numeric drift** (decision 1): Tier-3b proxy-read budget 77/64 vs 78; CoW materialization count 6 vs 7.

## Execution sequence

1. ✅ Plan written + 6-pass fresh-agent review → clean SIGN OFF (this doc).
2. **Scaffold** `docs/devbook/` — README (this TOC, anchored) + per-chapter stubs (scope + source pointers + fold targets/dispositions + "In the self-host" placeholder + anchors). Touches nothing in `internals/`.
3. **Chapter generation** (reference-first fan-out): each chapter mapped against real source with `file:line`, then an **adversarial accuracy pass** (skeptic re-derives every claim from code) + the **cross-doc honesty triangulation** (decision 10). Fold dispositions per decision 6; figures/cites re-derived per decision 1.
4. **Repoint + fold + retire** per landed chapter: repoint that subsystem's `// See` comments, fold + delete the absorbed internals doc(s). `internals/` dies of attrition; deleted when empty.

## Review provenance

6 sequential fresh-agent passes (2026-05-29), each reviewing the artifact after the prior pass's findings were folded: pass 1 (10 reservations) → 2 (1 blocking + 4 minor) → 3 (1 blocking + 2 moderate + 3 minor) → 4 (1 blocking + 1 moderate + 1 minor) → 5 (1 blocking + 1 moderate) → **6 (SIGN OFF)**. Per CLAUDE.md "review plans with a fresh agent."
