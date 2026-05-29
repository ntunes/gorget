export const meta = {
  name: 'devbook-review',
  description: 'Fresh-agent review of the generated devbook: per-chapter accuracy re-derivation + cross-cutting consistency sweep, before integration',
  phases: [
    { title: 'ChapterReview', detail: 'a fresh reviewer per chapter re-derives every claim from current source' },
    { title: 'Consistency', detail: 'cross-cutting reviewers: links/anchors, terminology/glossary, duplication/contradiction, honesty/fold-discipline' },
  ],
}

const COMMON = `
You are a FRESH reviewer (you did NOT write or previously verify this chapter). Re-derive everything from CURRENT source — do NOT trust the chapter's own claims or cites. The chapters were machine-generated then auto-revised once; a fold can leave a stale remnant or introduce a NEW error, which is exactly what you exist to catch.
AUDIENCE/PURPOSE: docs/devbook/ is a HOW-IT-IS-IMPLEMENTED reference book for Gorget compiler developers. The bar: every load-bearing claim backed by a path:line that is actually correct in the current tree; describes the IMPLEMENTATION (cites the spec, never restates it); no IMPL-AHEAD/roadmap framing for shipped work; no stale figures/line-cites copied from old internals docs; reads as reference-grade (clear, structured, not padded, no hallucinated APIs).
You are READ-ONLY: read chapters under docs/devbook/ and any source/tests/docs you need. Do NOT modify any file. Run NO git commands.
Return your structured verdict. Do NOT rubber-stamp, and do NOT invent reservations to avoid signing off — if it is accurate and reference-grade, SIGN OFF. Cite path:line for every issue.
`

const REVIEW_SCHEMA = {
  type: 'object', additionalProperties: false,
  required: ['file', 'verdict', 'issues', 'qualityNote'],
  properties: {
    file: { type: 'string' },
    verdict: { type: 'string', enum: ['sign-off', 'reservations'] },
    qualityNote: { type: 'string', description: 'one-line overall read of the chapter quality' },
    issues: {
      type: 'array',
      items: {
        type: 'object', additionalProperties: false,
        required: ['severity', 'kind', 'claim', 'evidence', 'fix'],
        properties: {
          severity: { type: 'string', enum: ['blocking', 'major', 'minor'] },
          kind: { type: 'string', enum: ['inaccuracy', 'stale-cite', 'impl-ahead-leak', 'spec-restated', 'unverifiable', 'coverage-gap', 'broken-ref', 'quality'] },
          claim: { type: 'string', description: 'the statement in the chapter that is wrong/weak (quote it)' },
          evidence: { type: 'string', description: 'path:line in CURRENT source proving the issue' },
          fix: { type: 'string', description: 'the concrete correction' },
        },
      },
    },
  },
}

const CONSISTENCY_SCHEMA = {
  type: 'object', additionalProperties: false,
  required: ['dimension', 'verdict', 'findings'],
  properties: {
    dimension: { type: 'string' },
    verdict: { type: 'string', enum: ['clean', 'issues'] },
    findings: {
      type: 'array',
      items: {
        type: 'object', additionalProperties: false,
        required: ['severity', 'detail', 'evidence'],
        properties: {
          severity: { type: 'string', enum: ['blocking', 'major', 'minor'] },
          detail: { type: 'string' },
          evidence: { type: 'string', description: 'which chapter(s) + path:line / anchor' },
        },
      },
    },
  },
}

// file + intended primary source + scope (so reviewers can also catch coverage GAPS vs intent)
const CH = [
  { n:'00', file:'00-how-to-read.md', src:'docs/devbook/README.md, docs/plans/devbook_plan.md', scope:'one-source contract, fold protocol, freshness stamps, subsystem-ordering, In-the-self-host convention' },
  { n:'01', file:'01-pipeline-and-driver.md', src:'src/main.rs, src/tui.rs, src/semantic/mod.rs', scope:'full pass pipeline with REAL pass numbers (no provenance pass), all gg subcommands, REPL, BIR placement' },
  { n:'02', file:'02-foundations.md', src:'src/span.rs, src/intern.rs, src/errors.rs', scope:'Span, Symbol interning (lexer-only reach), diagnostics' },
  { n:'03', file:'03-lexer.md', src:'src/lexer/', scope:'Logos tokenizer, indentation state machine (spaces-only), f-string scanning' },
  { n:'04', file:'04-parser-ast.md', src:'src/parser/', scope:'recursive descent + Pratt, AST, indentation blocks, f-string interp-segment synthetic spans' },
  { n:'05', file:'05-formatter.md', src:'src/formatter/', scope:'AST->canonical source, gg fmt incl --check' },
  { n:'06', file:'06-meta-derive.md', src:'src/semantic/meta.rs, src/semantic/derive.rs', scope:'meta if/for/const (Pass 0), @derive (Pass 0.5), meta type aliases' },
  { n:'07', file:'07-name-resolution.md', src:'src/semantic/resolve.rs', scope:'two-pass resolution, scopes, resolution_map; import as/wildcard' },
  { n:'08', file:'08-traits.md', src:'src/semantic/traits.rs', scope:'trait registry, equip, default methods, via, builtin traits (note Ordinal gap), @derive impls' },
  { n:'09', file:'09-type-checking.md', src:'src/semantic/typecheck.rs', scope:'inference, exhaustiveness, method resolution, Pass 4.5 apply_inferred_targs, 4.6 lint_suggest_throws' },
  { n:'10', file:'10-ownership-safety.md', src:'src/semantic/safety/', scope:'9 submodules, BorrowOrigin, branch save/restore, concurrency/shared/spawn checks, Pass 5a' },
  { n:'11', file:'11-copy-on-write.md', src:'src/ir/lowering/context.rs, stmts/assigns.rs, exprs/methods.rs, src/backend/c_lir/emit_call_extern.rs, src/semantic/safety/origins.rs', scope:'CoW default-borrow, consuming-position move/clone/borrow, materialization (no fixed count), view discriminator, no provenance pass' },
  { n:'12', file:'12-gir-lowering.md', src:'src/ir/, src/ir/lowering/closures.rs', scope:'GIR, monomorphization, drop insertion, closure lowering + capture (2 capture modes)' },
  { n:'13', file:'13-ownership-in-ir.md', src:'src/ir/lowering/, src/ir/instructions.rs', scope:'AssignMode/FieldLoadMode/ArgOwnership, LocalOwnership, BorrowOrigin, clone-at-calls tree, LoadRef shipped/StoreRef not-emitted' },
  { n:'14', file:'14-lir-ssa.md', src:'src/lir/', scope:'SSA construction, LIR insts, Backend trait, critical-edge/dominance validation, LirType::FuncRef' },
  { n:'15', file:'15-drop-elaboration.md', src:'src/lir/drop_elab.rs, src/lir/optimize.rs', scope:'packed 2-bit SlotStates, forward dataflow, optimizer fixpoint (32-iter, NOT 3)' },
  { n:'16', file:'16-bir.md', src:'src/bir/', scope:'Backend IR newtype, 11-arm validator, canonical-op expansion, synth helpers; stale Step-0 docstring flagged not repeated' },
  { n:'17', file:'17-c-backend.md', src:'src/backend/c_lir/', scope:'LIR->C emission, map_monomorphized_to_runtime, dumb-backend, HofExpand unreachable (lowered in BIR)' },
  { n:'18', file:'18-runtime-abi.md', src:'src/backend/c/c_runtime.rs, src/compiler_data.rs, src/ir/resources.rs, src/lir/runtime.rs', scope:'GorgetString/Array layout, view discriminator, runtime-decl table (RuntimeFn+resources.gg SSoT, NOT unshipped RUNTIME_DECLS)' },
  { n:'19', file:'19-llvm-backend.md', src:'src/backend/llvm/', scope:'LLVM backend SHIPPED (not planned), LIR->LLVM mapping, runtime as .o, parity with C' },
  { n:'20', file:'20-extern-gpu.md', src:'src/parser/, src/backend/c/c_runtime.rs', scope:'extern C blocks, T* syntax, returns_borrowed (shipped), Metal/GL/GPU runtime' },
  { n:'21', file:'21-simulator.md', src:'src/sim/', scope:'GIR interpreter as reference oracle, gg sim + its REAL flags (--seed/--many-seeds/--ignore-leaks/--disable-isolation/--backtrace; no --max-steps)' },
  { n:'22', file:'22-modules-packages.md', src:'src/loader.rs, src/lockfile.rs, src/manifest.rs, src/resolver.rs', scope:'import resolution, loader fallback, lockfile/manifest, DEP resolver (distinct from name resolver); no semver/dev-deps/gg-update' },
  { n:'23', file:'23-stdlib.md', src:'lib/std/, lib/xtd/, src/stdlib.rs', scope:'narrow-waist, Iterator/Writer/Reader (free fns), capacity ctors, lazy iterators, drain shipped' },
  { n:'24', file:'24-layering-discipline.md', src:'all IR layers; docs/internals/layering-discipline.md', scope:'four rules, no-name-matching, litmus test, fix-complexity heuristic, Snag #17/#13 worked examples' },
  { n:'25', file:'25-structural-guards.md', src:'src/ir/validate.rs, src/lir/validate.rs, src/bir/validate.rs', scope:'writer-side validators, framework + 3 instances, migration framework, Tier 1/2/3' },
  { n:'26', file:'26-self-host-frontend.md', src:'tests/fixtures/self_host_*', scope:'self-host architecture, driver dirs (symlinked vs copies), stress-test/regression-net/showcase, lexer NOT green (keyword drift)' },
  { n:'27', file:'27-comparison-bootstrap.md', src:'tests/integration.rs, src/report.rs', scope:'*_comparison diagnostic-always-pass, bootstrap fixed-point, report gen, parity north-star' },
  { n:'A', file:'appendix-a-file-map.md', src:'all of src/', scope:'dir->responsibility+LOC table; resolver.rs vs resolve.rs disambiguation; LOC figures must match wc -l' },
  { n:'B', file:'appendix-b-glossary.md', src:'(synthesized)', scope:'GIR/LIR/BIR/SSA/CoW/MoveZero/view/narrow-waist/provenance/RuntimeFn/etc — one crisp accurate sentence each' },
]

const DIMENSIONS = [
  { key:'links-anchors', prompt:
    `Cross-reference & navigation integrity across docs/devbook/. Verify: (1) every link in docs/devbook/README.md resolves to a file that exists; (2) inter-chapter references ("Ch. N", "see Chapter X", "(Ch. 24)") point to the chapter that actually covers that topic; (3) any referenced section anchor (#some-heading) exists as a heading in the target chapter; (4) the README TOC matches the actual chapter files (no missing/extra). Use grep across docs/devbook/*.md for link patterns and headings. Report broken/mismatched references with the citing chapter + the bad target.` },
  { key:'terminology-glossary', prompt:
    `Terminology consistency + glossary coverage. Verify: (1) load-bearing terms (GIR, LIR, BIR, SSA, CoW, MoveZero, view/cap==0, narrow waist, provenance, BorrowOrigin, RuntimeFn, monomorphization, drop elaboration, structural guard, consuming position, comparison test, bootstrap fixed-point) are used consistently across chapters and defined in appendix-b-glossary.md; (2) no term is defined two different ways in two chapters; (3) the glossary entries are accurate (spot-check 5-6 against source). Report inconsistent usage or wrong/ missing glossary entries.` },
  { key:'duplication-contradiction', prompt:
    `Cross-chapter contradiction hunt. Several topics are touched by multiple chapters — verify they agree: (a) BIR / the pipeline shape (chapters 01, 14, 16, 17, 24, 25); (b) CoW / ownership (11, 12, 13); (c) the pass pipeline order (01 vs any chapter that restates it); (d) "sole production backend" vs "LLVM is a second production backend" wording (01, 17, 19, appendix-a); (e) the self-host lexer parity claim (03 vs 26). Read the relevant chapters and report any place two chapters state contradictory facts, with both citations.` },
  { key:'honesty-fold-discipline', prompt:
    `Honesty + fold-discipline audit of the prose. Read docs/plans/devbook_honesty_audit.md first (the known findings), then scan all 30 chapters for: (1) IMPL-AHEAD/roadmap framing for SHIPPED work that leaked in (e.g. describing something as "planned/future/not yet" when it ships); (2) restating the language spec instead of describing the implementation; (3) figures or file:line cites that look transcribed from a stale internals doc rather than re-derived (spot-check a few against current source); (4) hallucinated APIs/flags that do not exist in source. Report leaks with chapter + evidence.` },
]

log(`Fresh review: ${CH.length} per-chapter reviewers + ${DIMENSIONS.length} consistency dimensions.`)

const chapterVerdicts = await parallel(CH.map(c => () => agent(
  `${COMMON}\n\n# REVIEW chapter ${c.n} — docs/devbook/${c.file}\n` +
  `INTENDED PRIMARY SOURCE: ${c.src}\n` +
  `INTENDED SCOPE (use to catch coverage GAPS): ${c.scope}\n\n` +
  `Read docs/devbook/${c.file}. Re-derive its load-bearing claims from current source and try to REFUTE each. Check: factual accuracy, correct & current file:line cites, no IMPL-AHEAD framing for shipped work, no spec-restatement, no hallucinated APIs, coverage of the intended scope, valid internal cross-references/anchors, and reference-grade quality. Return the structured verdict (sign-off or reservations + cited issues).`,
  { label: `review:${c.n}`, phase: 'ChapterReview', schema: REVIEW_SCHEMA }
).then(v => v || { file: c.file, verdict: 'reservations', qualityNote: 'reviewer returned nothing (skipped/error)', issues: [] })))

const consistency = await parallel(DIMENSIONS.map(d => () => agent(
  `${COMMON}\n\n# CROSS-CUTTING CONSISTENCY REVIEW — dimension: ${d.key}\n${d.prompt}\n\nReturn the structured findings (dimension="${d.key}").`,
  { label: `consist:${d.key}`, phase: 'Consistency', schema: CONSISTENCY_SCHEMA }
).then(v => v || { dimension: d.key, verdict: 'issues', findings: [{ severity: 'minor', detail: 'reviewer returned nothing', evidence: 'n/a' }] })))

const withIssues = chapterVerdicts.filter(v => v.verdict === 'reservations')
const blocking = chapterVerdicts.flatMap(v => (v.issues || []).filter(i => i.severity === 'blocking').map(i => ({ file: v.file, ...i })))
const major = chapterVerdicts.flatMap(v => (v.issues || []).filter(i => i.severity === 'major').map(i => ({ file: v.file, ...i })))
const consistencyIssues = consistency.filter(c => c.verdict === 'issues')

log(`Reviewed ${chapterVerdicts.length} chapters: ${chapterVerdicts.filter(v=>v.verdict==='sign-off').length} sign-off, ${withIssues.length} with reservations (${blocking.length} blocking, ${major.length} major). ${consistencyIssues.length}/${DIMENSIONS.length} consistency dimensions flagged issues.`)

return {
  signedOff: chapterVerdicts.filter(v => v.verdict === 'sign-off').map(v => v.file),
  chaptersWithReservations: withIssues.map(v => ({ file: v.file, qualityNote: v.qualityNote, issues: v.issues })),
  blockingIssues: blocking,
  majorIssues: major,
  consistencyFindings: consistency,
  summary: {
    total: chapterVerdicts.length,
    signOff: chapterVerdicts.filter(v => v.verdict === 'sign-off').length,
    reservations: withIssues.length,
    blocking: blocking.length,
    major: major.length,
  },
}
