export const meta = {
  name: 'devbook-generate',
  description: 'Generate all 28 chapters + 2 appendices of the Gorget compiler internals book (author -> adversarial accuracy verify -> revise)',
  phases: [
    { title: 'Author', detail: 'write each chapter from source + folded internals doc' },
    { title: 'Verify', detail: 'adversarially re-derive every claim from source + cross-doc honesty triangulation' },
    { title: 'Revise', detail: 'apply corrections; emit honesty findings' },
  ],
}

// Shared conventions every agent must honor (the approved plan: docs/plans/devbook_plan.md).
const CONV = `
AUDIENCE: Gorget *compiler developers* who read Rust and know what an AST/pass/IR is, but do NOT know Gorget's internals. This is a HOW-IT-IS-IMPLEMENTED reference book at docs/devbook/, NOT an end-user language guide.
STYLE: reference-grade, precise, cite-heavy, NOT padded. Markdown. Use ## / ### headings (they become stable section anchors that source comments will later point at). Open with a one-paragraph "what this subsystem does + where it lives". Prefer short code/IR excerpts with file:line over long prose.
ONE AUTHORITATIVE SOURCE: the book owns the *design narrative / why*. For *live facts* (which fn does what, ABI offsets, scores, numeric budgets, exact file:line) the CODE is authoritative — CITE it (path:line), never transcribe. ALL numbers and file:line citations found inside the OLD internals docs are presumed STALE: re-derive every one from CURRENT source; never copy a figure or line number out of an internals doc.
FOLD PROTOCOL (when folding an internals doc into this chapter): classify each piece as (a) evergreen architecture/rationale -> lift, re-derived from current source; (b) live roadmap -> DO NOT put in the chapter (it is a TODO item, just note it exists); (c) dead status -> drop; (d) IMPL-AHEAD stale status (the doc describes now-SHIPPED work in false present/future tense) -> lift only the rationale, re-derive the actual status from source. Apply (d) UNIVERSALLY: treat EVERY present/future-tense status claim in a folded doc as presumed stale and verify it against current source before repeating it.
SELF-HOST: where indicated, add an "## In the self-host" section: how the Gorget self-host (tests/fixtures/self_host_*) implements the SAME area, divergences from Rust gg, and current parity. State parity as a PROCEDURE ("run cargo test --test integration <name>_comparison -- --nocapture and read the printed matched-count"), NOT a fixed number — those tests are diagnostic-always-pass. If the self-host has NO coverage of this area (e.g. backends), say so plainly and point at the gap.
DO NOT restate the language spec (that is language-reference.md's job); describe the IMPLEMENTATION and cite the spec where relevant.
HARD RULES: you run in the main working tree. Write/modify ONLY your one assigned chapter file under docs/devbook/. Run NO git commands (no add/commit/stash/reset). Do not touch any other file. Read freely (src/, tests/, docs/internals/, language-reference.md, language-design.md, docs/book/).
`

const VERDICT_SCHEMA = {
  type: 'object',
  additionalProperties: false,
  required: ['verdict', 'corrections', 'honestyFindings'],
  properties: {
    verdict: { type: 'string', enum: ['clean', 'needs-revision'] },
    corrections: {
      type: 'array',
      items: {
        type: 'object', additionalProperties: false,
        required: ['claim', 'problem', 'evidence', 'fix'],
        properties: {
          claim: { type: 'string', description: 'the wrong/unverifiable statement in the chapter' },
          problem: { type: 'string', enum: ['false', 'stale-figure', 'stale-line-cite', 'unverifiable', 'spec-restated', 'impl-ahead-copied'] },
          evidence: { type: 'string', description: 'file:line from CURRENT source proving the problem' },
          fix: { type: 'string', description: 'the corrected statement or "delete"' },
        },
      },
    },
    honestyFindings: {
      type: 'array',
      description: 'cross-doc disagreements vs language-reference/design/book (for TODO, not necessarily this chapter)',
      items: {
        type: 'object', additionalProperties: false,
        required: ['kind', 'detail', 'evidence'],
        properties: {
          kind: { type: 'string', enum: ['DOC-AHEAD-implement', 'DOC-AHEAD-cleanup', 'IMPL-AHEAD', 'CONTRADICTION'] },
          detail: { type: 'string' },
          evidence: { type: 'string', description: 'file:line in both the doc and the source/other-doc' },
        },
      },
    },
  },
}

const REVISE_SCHEMA = {
  type: 'object', additionalProperties: false,
  required: ['file', 'revised', 'correctionsApplied', 'findings'],
  properties: {
    file: { type: 'string' },
    revised: { type: 'boolean' },
    correctionsApplied: { type: 'integer' },
    findings: {
      type: 'array',
      items: { type: 'object', additionalProperties: false, required: ['kind', 'detail'], properties: { kind: { type: 'string' }, detail: { type: 'string' } } },
    },
  },
}

const CH = [
  { n:'00', file:'00-how-to-read.md', title:'How to read this book', src:'docs/devbook/README.md, docs/plans/devbook_plan.md', folds:'(none)', selfhost:'n/a', scope:'The one-authoritative-source contract; the fold protocol; freshness "verified against <commit>" stamps; why chapters are subsystem-ordered not pass-ordered; the "In the self-host" convention. A fuller version of the README how-to-read section. Meta-chapter: no source code to cite beyond the plan.' },
  { n:'01', file:'01-pipeline-and-driver.md', title:'The pipeline & the gg driver', src:'src/main.rs (subcommand dispatch), src/tui.rs (REPL), src/semantic/mod.rs (pass ordering)', folds:'docs/internals/README.md pipeline diagram — but it is STALE (lists a non-existent "Pass 4.5 provenance.rs"); produce a CORRECTED diagram from src/semantic/mod.rs', selfhost:'n/a', scope:'The end-to-end pipeline with REAL pass numbers (verify against src/semantic/mod.rs: meta 0, derive 0.5, resolve 1-2, rewrite 2.5, traits 3, cycle_check 3.6, typecheck 4, apply_inferred_targs 4.5, lint_suggest_throws 4.6, safety 5). Every gg subcommand (lex/parse/check/build/run/profile/test/fmt/sim/new/add) and what it does. The REPL. NO provenance pass exists.' },
  { n:'02', file:'02-foundations.md', title:'Foundations: spans, interning & diagnostics', src:'src/span.rs, src/intern.rs, src/errors.rs', folds:'(none)', selfhost:'n/a', scope:'The Span model (used across every layer), Symbol interning (interned at lex time, used everywhere), and the diagnostic/error reporting infrastructure. The cross-layer foundation everything else builds on.' },
  { n:'03', file:'03-lexer.md', title:'The lexer & indentation', src:'src/lexer/', folds:'(none)', selfhost:'self_host_lexer (lex.gg); lexer parity is green', scope:'The Logos-based tokenizer, the indentation-tracking state machine (the non-obvious part), f-string scanning with quote_char.' },
  { n:'04', file:'04-parser-ast.md', title:'The parser & the AST', src:'src/parser/', folds:'docs/internals/fstring-interp-as-expr.md (UNCITED by source -> fold its evergreen content, no repoint needed; the f-string feature is cross-cutting lexer->parser->resolve->typecheck->lowering, so note forward-references to Ch.7/Ch.9)', selfhost:'self_host_parser; run parser_comparison for parity', scope:'Recursive descent + Pratt expression parsing, the AST shape, indentation-based blocks, bare-tuple syntax, the f-string interpolation-segment sidecar on StringLiteral.' },
  { n:'05', file:'05-formatter.md', title:'The formatter (gg fmt)', src:'src/formatter/', folds:'(none)', selfhost:'no self-host formatter — say so', scope:'AST -> canonical source, gg fmt, how it produces canonical output (e.g. suppresses match guards, SMetaFor/SMetaIf output).' },
  { n:'06', file:'06-meta-derive.md', title:'Meta & derive (Pass 0 / 0.5)', src:'src/semantic/meta.rs, src/semantic/derive.rs', folds:'docs/internals/meta.md', selfhost:'self_host_typechecker meta.gg + derive.gg', scope:'Compile-time meta if/for/const evaluation (Pass 0); @derive attribute expansion (Pass 0.5); meta type-alias expansion; the field_value/field_set compile-time rewrites.' },
  { n:'07', file:'07-name-resolution.md', title:'Name resolution & scopes (Pass 1-2)', src:'src/semantic/resolve.rs', folds:'(none)', selfhost:'self_host_resolver; run resolver_comparison for parity', scope:'Two-pass name resolution, scope building, the resolution_map, DefId assignment. Note: ECall type args are NOT resolved by the resolver.' },
  { n:'08', file:'08-traits.md', title:'Traits & the impl registry (Pass 3)', src:'src/semantic/traits.rs', folds:'(none)', selfhost:'self_host_typechecker (trait handling in typecheck.gg)', scope:'The trait registry, equip blocks, default methods, trait inheritance, via delegation, impl validation, @derive-generated impls.' },
  { n:'09', file:'09-type-checking.md', title:'Type inference & checking (Pass 4)', src:'src/semantic/typecheck.rs', folds:'docs/internals/method-level-inference.md (IMPL-AHEAD: its header says "Not yet implemented" but it is at least partially shipped — verify actual state at typecheck.rs and re-derive)', selfhost:'self_host_typechecker; run type_comparison for parity', scope:'Type inference, exhaustiveness, method resolution, generic monomorphization inputs. Pass 4.5 apply_inferred_method_targs (the REAL Pass 4.5), Pass 4.6 lint_suggest_throws.' },
  { n:'10', file:'10-ownership-safety.md', title:'Ownership: moves & borrows — the safety checker (Pass 5)', src:'src/semantic/safety/ (9 submodules)', folds:'docs/internals/safety-checker.md, docs/internals/shared-keyword-design.md', selfhost:'self_host check.gg', scope:'The 9 safety submodules (mod/type_utils/origins/helpers/check_expr/check_stmt/return_borrows/validation), BorrowOrigin tracking, branch save/restore, concurrency + shared + spawn-safety checks, Pass 5a return_borrows.' },
  { n:'11', file:'11-copy-on-write.md', title:'Copy-on-write & view provenance', src:'src/ir/lowering/context.rs, src/ir/lowering/stmts/assigns.rs, src/ir/lowering/exprs/methods.rs, src/backend/c_lir/emit_call_extern.rs, src/semantic/safety/origins.rs', folds:'docs/internals/copy-on-write.md (NOTE: a CONTRADICTION exists — it says SEVEN materialization points while feedback_cow_design_clarity.md says SIX; re-derive the actual count from source and flag the disagreement as a finding)', selfhost:'self_host_lowerer (lower.gg LoView/LoOwned tagging)', scope:'CoW default-borrow everywhere; the consuming-position (push/put/set/insert/send) move-vs-clone-vs-borrow decision (the CLAUDE.md table); materialization points; MoveZero; the view discriminator (cap==0). There is NO standalone provenance pass / provenance.rs.' },
  { n:'12', file:'12-gir-lowering.md', title:'GIR & lowering: monomorphization, drops, closures', src:'src/ir/, src/ir/lowering/closures.rs', folds:'docs/internals/unified-resource-model.md sections 1-2 overview only (IMPL-AHEAD doc — lift overview rationale, re-derive status); REPOINT the dangling docs/internals/closure-capture.md citations (src/ir/validate.rs:1943, src/ir/lowering/closures.rs:149) — that doc does NOT exist, so write the closure-lowering content here', selfhost:'self_host_lowerer', scope:'GIR shape, monomorphization, drop insertion, closure lowering + capture. The two-pass type registration.' },
  { n:'13', file:'13-ownership-in-ir.md', title:'Ownership in the IR', src:'src/ir/lowering/, src/ir/instructions.rs', folds:'docs/internals/ownership-ir.md (IMPL-AHEAD: it lists LoadRef/StoreRef as "future" but they SHIP at src/ir/instructions.rs ~176/183 — verify + re-derive), docs/internals/clone-emission-at-calls.md (IMPL-AHEAD: "in progress" but the 3-site receiver dispatch shipped — verify in lower.gg), docs/internals/unified-resource-model.md section 3 Phase A + sections 6.1-6.7 Phase D', selfhost:'self_host_lowerer', scope:'AssignMode/FieldLoadMode/ArgOwnership on GIR, LocalOwnership (Phase D), BorrowOrigin provenance through GIR, the clone-vs-move-vs-borrow-at-calls decision tree.' },
  { n:'14', file:'14-lir-ssa.md', title:'LIR & SSA', src:'src/lir/', folds:'docs/internals/lir-design.md (IMPL-AHEAD: section "Phase 5 LLVM Planned" is stale — LLVM shipped), lir-backend-lift-plan.md LIR-side parts (cited from src/lir/mod.rs, src/lir/lower/insts.rs), unified-resource-model.md section 6.8 (LIR-side Slot.origin) + section 8.2 (critical-edge/SSA invariants — verify the stale claim that the dominance check is not called from validate_module; it IS called at src/lir/validate.rs ~50-52) + section 8.5 + section 8.6 (LirType::FuncRef)', selfhost:'self_host (lir_lower.gg, lir_ssa.gg, lir_codegen.gg)', scope:'SSA construction, the LIR instruction set, the Backend trait boundary, critical-edge splitting + dominance validation, LirType::FuncRef. Re-derive every section-8 status claim from current source (many are shipped).' },
  { n:'15', file:'15-drop-elaboration.md', title:'Drop elaboration & optimization', src:'src/lir/drop_elab.rs, src/lir/optimize.rs', folds:'docs/internals/unified-resource-model.md section 8.1 drop-flag + section 8.4 optimizer fixpoint (IMPL-AHEAD: section 8.4 says "runs three iterations and stops" which is FALSE — optimize.rs runs a 32-iter snapshot-equality fixpoint; verify and re-derive)', selfhost:'self_host drop_elab.gg', scope:'Drop elaboration with packed 2-bit SlotStates (meet == bitwise-OR), the forward dataflow, the optimizer fixpoint loop (verify iteration count from source).' },
  { n:'16', file:'16-bir.md', title:'BIR: backend-agnostic synthesis & validation', src:'src/bir/ (mod.rs, lower.rs, synth.rs, validate.rs — ~6253 LOC), src/main.rs:658/705/1467 (BirModule::from_lir wiring)', folds:'docs/internals/bir-module-synthesis-plan.md, lir-backend-lift-plan.md BIR-side parts. DELETE handover-option-c-bir-synthesis.md is a later step (do not delete now). CRITICAL: the src/bir/mod.rs:20-26 + validate.rs:23-27 "Step 0 / trivial passthrough / accepts every instruction" docstrings are STALE — the real validate.rs has ~11 canonical-op rejection arms (validate.rs:54-104) and lower.rs is ~4058 lines of expansions. Describe the SHIPPED reality, cite the validator arms, NOT the stale docstrings.', selfhost:'NONE — the self-host has no BIR layer (backend not self-hosted). Say so plainly and point at the c_emit parity gap.', scope:'BIR = Backend IR: the AST->GIR->LIR->BIR->machine layer; a newtype over LirModule that GUARANTEES canonical ops (HofExpand/EnumInit/TraitCall/StructInit/CowClone/BoxAlloc/...) are expanded to primitives (type-system-enforced so backends can never receive unlowered LIR); synth.rs helper generation (sort/sort_by_key/trait helpers). validate.rs is an INSTANCE of the structural-guard framework (Ch.25) — point there for the framework.' },
  { n:'17', file:'17-c-backend.md', title:'The C backend', src:'src/backend/c_lir/', folds:'docs/internals/codegen-gap-spike.md, docs/internals/tier1c-cluster1-burn-down.md', selfhost:'self-host c_emit is the biggest parity gap (~64% w/ crashes) — run c_emit_comparison; say so', scope:'LIR -> C emission, map_monomorphized_to_runtime, collection self-by-ptr, cstr handling, the "dumb backend" principle (all ownership logic upstream).' },
  { n:'18', file:'18-runtime-abi.md', title:'The runtime & the backend ABI contract', src:'src/backend/c/c_runtime.rs, src/compiler_data.rs, src/ir/resources.rs, src/ir/resource_schema.rs, compiler/data/resources.gg, compiler/data/schema.gg, src/lir/runtime.rs (RuntimeFn enum)', folds:'docs/internals/unified-resource-model.md section 3.6 "runtime declaration table". CRITICAL: RUNTIME_DECLS / resources.toml / build.rs is UNSHIPPED roadmap (grep RUNTIME_DECLS returns nothing) — describe the SHIPPED infra instead: the RuntimeFn enum + the compiler/data/resources.gg SSoT + crate::ir::resources::table() (read at src/lir/lower/calls.rs:278); mention RUNTIME_DECLS only as a future direction. REPOINT the dangling src/lir/runtime.rs:6 -> lir-correctness-roadmap.md citation here (that doc was superseded by unified-resource-model).', selfhost:'n/a', scope:'GorgetString/GorgetArray layout (cap at index 1, cap==0 == view), the view discriminator, clone ABI, the runtime declaration table as the single-source-of-truth for runtime symbol signatures across C/LLVM, the narrow waist.' },
  { n:'19', file:'19-llvm-backend.md', title:'The LLVM backend', src:'src/backend/llvm/ (~6735 LOC — SHIPPED, GG_BACKEND=llvm at C-parity per CLAUDE.md)', folds:'docs/internals/llvm-backend-plan.md (IMPL-AHEAD: it is framed as a PLAN for a backend that has SHIPPED — lift the LIR->LLVM mapping rationale, re-derive the actual current state, do NOT repeat "planned"/"will add")', selfhost:'n/a', scope:'The LLVM IR backend, the LIR->LLVM 1:1 mapping, runtime linked as a separate .o, parity with the C backend, GG_BACKEND=llvm.' },
  { n:'20', file:'20-extern-gpu.md', title:'Extern, interop & GPU backends', src:'src/parser/ (extern blocks), src/backend/c/c_runtime.rs (METAL_RUNTIME/GL_RUNTIME), src/backend (gen_metal_module/gen_gl_module)', folds:'docs/internals/extern-modules.md (IMPL-AHEAD: "Future: borrowed qualifier" is SHIPPED — verify at src/parser/mod.rs ~1666-1705, field returns_borrowed). The GPU half has no internals doc — write it from c_runtime.rs + MEMORY/CLAUDE notes.', selfhost:'n/a', scope:'extern "C" blocks, T* contextual pointer syntax, the returns_borrowed qualifier (shipped), the Metal/GL/GPU runtime architecture (opaque int64 handles, conditional runtime inclusion).' },
  { n:'21', file:'21-simulator.md', title:'The simulator / interpreter (gg sim)', src:'src/sim/ (~10K LOC)', folds:'(none)', selfhost:'n/a', scope:'The GIR interpreter used as a reference oracle, the gg sim subcommand, how interpreted execution differs from compiled, its role as a differential check (sim_ub).' },
  { n:'22', file:'22-modules-packages.md', title:'Modules, loading & package management', src:'src/loader.rs, src/lockfile.rs, src/manifest.rs, src/resolver.rs', folds:'(none)', selfhost:'self_host loader.gg', scope:'import / from-import resolution, the loader fallback chain, lockfile + manifest, the dependency resolver. IMPORTANT disambiguation: src/resolver.rs is the PACKAGE/DEP resolver (versions, cycles) and is DISTINCT from src/semantic/resolve.rs (name resolution, Ch.7) — make this explicit.' },
  { n:'23', file:'23-stdlib.md', title:'The standard library narrow waist', src:'lib/std/, lib/xtd/, src/stdlib.rs, src/semantic/typecheck.rs (stdlib typing)', folds:'docs/internals/stdlib-design.md (IMPL-AHEAD: carries roadmap markers — lift the shipped narrow-waist design, route still-future API consolidation to a note, re-derive what is actually shipped)', selfhost:'n/a', scope:'The narrow-waist API architecture, Iterator/Writer/Reader traits, capacity constructors, lazy iterators, how stdlib is registered and typed.' },
  { n:'24', file:'24-layering-discipline.md', title:'Layering discipline', src:'all IR layers (AST->GIR->LIR->BIR->backend)', folds:'docs/internals/layering-discipline.md', selfhost:'n/a', scope:'The four rules (lossless on invariants / typed-metadata-not-name-matched / one-source-of-truth-per-axis / resolve-once-write-through), the "no name matching" rule, the litmus test, the debugging heuristic (fix-complexity = wrong layer), worked examples (Snag #17 self_conv flag; Snag #13 Box inner-type).' },
  { n:'25', file:'25-structural-guards.md', title:'Structural guards', src:'src/ir/validate.rs, src/lir/validate.rs, src/bir/validate.rs', folds:'docs/internals/structural-guards.md, docs/internals/unified-resource-model.md section 5 Phase C + section 8.3', selfhost:'n/a', scope:'Writer-side validators that turn soundness invariants into assertions; the framework + its THREE concrete validator instances (ir/lir/bir validate.rs); the migration framework; Tier 1/2/3. The BIR validator (Ch.16) is one instance.' },
  { n:'26', file:'26-self-host-frontend.md', title:'The self-host frontend', src:'tests/fixtures/self_host_lexer/, self_host_parser/, self_host_resolver/, self_host_typechecker/, self_host_lowerer/', folds:'docs/internals/self-host-resource-model.md (IMPL-AHEAD: "Phase C IN PROGRESS" markers — re-derive current state)', selfhost:'this chapter IS the self-host overview', scope:'The self-host architecture, the per-comparison driver dirs (and which are symlinked vs independent copies), the self-host as simultaneously stress-test + regression-net + idiomatic-Gorget showcase, the no-defensive-workarounds rule.' },
  { n:'27', file:'27-comparison-bootstrap.md', title:'Comparison, bootstrap & report generation', src:'tests/integration.rs (the *_comparison tests + self_host_bootstrap_fixed_point), src/report.rs, tests/lir_ab.rs, tests/lints.rs, tests/security.rs, tests/str_runtime.rs', folds:'(none)', selfhost:'this chapter explains the self-host test machinery', scope:'How the *_comparison tests work and WHY they are diagnostic-always-pass (a green suite says nothing about parity — only the printed matched-counts do); the bootstrap fixed-point (stage2==stage3==stage4); report generation; the parity north-star (parity with Rust gg, not just green).' },
  { n:'A', file:'appendix-a-file-map.md', title:'Appendix A — Subsystem to file map', src:'all of src/ (walk the top-level dirs + key files)', folds:'(none)', selfhost:'n/a', scope:'A table mapping each src/ directory and key file to its responsibility + approximate LOC. MUST disambiguate src/resolver.rs (dependency resolver) from src/semantic/resolve.rs (name resolution). Derive LOC from actual wc -l.' },
  { n:'B', file:'appendix-b-glossary.md', title:'Appendix B — Glossary', src:'(synthesize from the other chapters + source)', folds:'(none)', selfhost:'n/a', scope:'Define every load-bearing term: GIR, LIR, BIR, SSA, CoW, MoveZero, view (cap==0), narrow waist, provenance, BorrowOrigin, RuntimeFn, monomorphization, drop elaboration, structural guard, the consuming position, the self-host, comparison test, bootstrap fixed-point. One crisp sentence each, cross-linked.' },
]

log(`Generating ${CH.length} units (28 chapters + 2 appendices) — author -> verify -> revise per unit.`)

const results = await pipeline(
  CH,
  // Stage 1 — AUTHOR
  (c) => agent(
    `${CONV}\n\n# YOUR TASK: author chapter ${c.n} — "${c.title}"\n` +
    `Write the file docs/devbook/${c.file} using the Write tool.\n` +
    `PRIMARY SOURCE to study and cite: ${c.src}\n` +
    `INTERNALS DOC(S) to fold (apply the fold protocol; re-derive all figures/line-cites from current source): ${c.folds}\n` +
    `IN THE SELF-HOST: ${c.selfhost}\n` +
    `SCOPE: ${c.scope}\n\n` +
    `Read the primary source and the folded doc(s) first. Then write a focused, accurate reference chapter (## section headings as anchors). Every load-bearing claim must be backed by a path:line you actually verified. Then Write the file. Your returned text should just confirm the file was written and list the 3-6 most load-bearing claims you made, each with its path:line.`,
    { label: `author:${c.n}`, phase: 'Author' }
  ),
  // Stage 2 — VERIFY (adversarial accuracy + cross-doc honesty)
  (_s1, c) => agent(
    `${CONV}\n\n# YOUR TASK: adversarially verify chapter ${c.n} ("${c.title}") at docs/devbook/${c.file}\n` +
    `Read docs/devbook/${c.file}. For EVERY load-bearing claim, re-derive it from CURRENT source (${c.src} and anything else relevant) and try to REFUTE it. Flag: false claims, stale figures, stale line-cites, unverifiable claims, places where it restated the language spec instead of describing the implementation, and any IMPL-AHEAD roadmap framing copied from a stale internals doc.\n` +
    `ALSO do the cross-doc honesty triangulation: compare what the chapter says the compiler does against docs/language-reference.md, docs/language-design.md, and docs/book/. Classify any disagreement as DOC-AHEAD-implement / DOC-AHEAD-cleanup / IMPL-AHEAD / CONTRADICTION (these go to TODO, not necessarily into this chapter).\n` +
    `Do NOT modify the file. Return the structured verdict.`,
    { label: `verify:${c.n}`, phase: 'Verify', schema: VERDICT_SCHEMA }
  ),
  // Stage 3 — REVISE (apply corrections, emit findings)
  (verdict, c) => agent(
    `${CONV}\n\n# YOUR TASK: revise chapter ${c.n} ("${c.title}") at docs/devbook/${c.file}\n` +
    `A skeptic reviewer produced this verdict:\n${JSON.stringify(verdict)}\n\n` +
    `Read docs/devbook/${c.file}. Apply EVERY correction: fix false/stale claims (re-derive the right value/line from source), delete unverifiable ones, remove spec-restatement and IMPL-AHEAD roadmap framing. Re-Write the corrected file with the Write tool. Keep it accurate and tight. ` +
    `If verdict was "clean", make only trivial polish. Return the structured result: file, whether you revised, how many corrections you applied, and the honestyFindings (carry them through from the verdict for aggregation).`,
    { label: `revise:${c.n}`, phase: 'Revise', schema: REVISE_SCHEMA }
  ),
)

const done = results.filter(Boolean)
const allFindings = done.flatMap(r => (r.findings || []).map(f => ({ ...f, file: r.file })))
log(`Done: ${done.length}/${CH.length} chapters written. ${allFindings.length} cross-doc honesty findings collected.`)

return {
  chaptersWritten: done.map(r => r.file),
  totalCorrectionsApplied: done.reduce((s, r) => s + (r.correctionsApplied || 0), 0),
  honestyFindings: allFindings,
  missing: CH.filter(c => !done.find(r => r.file === c.file)).map(c => c.file),
}
