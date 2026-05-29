export const meta = {
  name: 'devbook-fix',
  description: 'Fold the fresh-review reservations into the 13 flagged devbook chapters, then a fresh confirming pass per chapter',
  phases: [
    { title: 'Fix', detail: 'apply the verified review fixes to each flagged chapter, re-deriving from source' },
    { title: 'Confirm', detail: 'a fresh reviewer re-derives the corrected claims; sign-off or remaining issues' },
  ],
}

const COMMON = `
docs/devbook/ is a HOW-IT-IS-IMPLEMENTED reference book for Gorget compiler developers. You run in the main working tree. READ freely (src/, tests/, docs/). Touch ONLY your one assigned chapter file under docs/devbook/. Run NO git commands.
The reviewers were verified accurate against source by the orchestrator — apply their fixes, do NOT re-litigate confirmed facts. Re-derive any cite you change from CURRENT source so the new cite is correct. Make ONLY the cited corrections plus obvious knock-on consistency (e.g. if a fact appears 3x, fix all 3); do NOT rewrite unrelated prose or restyle the chapter.
`

// Orchestrator-verified ground truth for the substantive fixes (do not re-derive these — they are confirmed):
const GROUND_TRUTH = `
VERIFIED FACTS (confirmed against source by the orchestrator — use these directly):
- SELF-HOST scope (chapters 24/25/26): the self-host DOES implement the LIR layer (tests/fixtures/self_host_lowerer/lir_lower.gg, lir_ssa.gg), drop elaboration (drop_elab.gg), a LIR→C backend (lir_codegen.gg, 5265 lines, incl. emit_box_allocators spelling __gorget_box_alloc_<inner>), AND the writer-side structural validators (validate.gg: validate_resource_moves :93, validate_resource_field_reads, validate_resource_call_args). It is exercised by c_emit_comparison and self_host_bootstrap_fixed_point. Any chapter text claiming the self-host lacks a LIR layer / C backend / ported validators is WRONG — rewrite it to reflect this, reading the cited self-host files for accurate detail.
- SELF-HOST OpClone (chapter 26): op_consume returns OpClone (not OpBorrow) for LoBorrowed/LoView at consume positions (lower.gg ~1466), and the OpClone operand mode lowers to a REAL runtime clone call (lir_lower.gg ~2351 OpClone case + ICallExtern to T__clone / gorget_*_clone, ~2102/2158), so clone codegen HAS shipped. Only OpMove/OpBorrow/OpView still lower to a plain ISlotLoad. The claim "all four operand modes lower to the same ISlotLoad" is FALSE. The operand-emission site is lower_operand, not the operand_lir_type type-helper at lir_lower.gg:2349-2394.
- ch01 LIR optimizer: in the normal try_build_ir path the LIR optimizer runs ONCE, post-BIR (src/main.rs:712). main.rs:601 and :663 are inside the mutually-exclusive --emit-lir / --emit-c-lir short-circuit branches. What actually runs TWICE is the type-computation triple compute_module_pointee_types/value_types/value_origins: pre-BIR (main.rs:699-701) and post-BIR (main.rs:714-716).
- ch17 / BIR canonical ops: there are TEN ops in the C-backend unreachable arm (src/backend/c_lir/mod.rs:2044-2055): SizeOf, EnumInit, EnumCheck, EnumExtract, StructInit, CowClone, TraitCall, HofExpand, AddressOf, BoxAlloc. (The BIR validator rejects 11 — those ten plus CollectionCtor — src/bir/validate.rs:54-108.) Any "five ops" statement is wrong; fix every occurrence (chapter ~18-19, ~59, ~227).
- ch19 LirGlobalInit: the enum (src/lir/mod.rs:1587) has variants Zeroed/Bytes/FuncAddr/Extern/Struct — there is NO RuntimeCall variant (it was replaced by Extern { name, args } at mod.rs:1603). The runtime-ctor global path dispatches off Extern.
- ch11 ensure_owned_at_boundary table: exprs/mod.rs:480 is the TUPLE-literal field-init boundary (inside Expr::TupleLiteral, :469); exprs/mod.rs:1950 is the STRUCT field-init boundary (inside fn lower_struct_literal, :1555). There is NO ensure_owned_at_boundary call for enum-variant init (enum constructors use emit_enum_init_owned). The third real call site is :2538 (match arm). The function is lower_struct_literal (NOT lower_struct_init). ParamABI::ByPtr is at context.rs:33 (the enum is at :30).
- ch24 symlink: AGENTS.md is the real file; CLAUDE.md is the symlink to it (CLAUDE.md -> AGENTS.md). The chapter has the direction backwards.
- ch13 / ch25: the GIR resource-move check fn is validate_resource_moves (src/ir/validate.rs:995) + the inline assign_read_site extractor (validate.rs:1119) — there is NO check_resource_moves (the only occurrence is a stale rustdoc reference at validate.rs:1416).
- CoW materialization count (ch13 ↔ ch11): Chapter 11's "SIX-vs-SEVEN" callout is correct — there is no fixed numbered count in source (driven by ~33 ensure_owned_at_* call sites). Chapter 13 must NOT assert a contradicting fixed count; align it with ch11.
- sole-vs-second backend (ch17 ↔ ch19): harmonize the wording — C is the DEFAULT production backend; LLVM is a second backend behind --backend=llvm held at parity. Do not bald-assert "sole" in a way that contradicts ch19; phrase consistently.
`

const FIX_SCHEMA = {
  type: 'object', additionalProperties: false,
  required: ['file', 'fixesApplied', 'notes'],
  properties: { file: { type: 'string' }, fixesApplied: { type: 'integer' }, notes: { type: 'string' } },
}
const CONFIRM_SCHEMA = {
  type: 'object', additionalProperties: false,
  required: ['file', 'verdict', 'remainingIssues'],
  properties: {
    file: { type: 'string' },
    verdict: { type: 'string', enum: ['clean', 'remaining'] },
    remainingIssues: {
      type: 'array',
      items: { type: 'object', additionalProperties: false, required: ['severity', 'detail', 'evidence'], properties: { severity: { type: 'string', enum: ['blocking','major','minor'] }, detail: { type: 'string' }, evidence: { type: 'string' } } },
    },
  },
}

const FLAGGED = [
  '01-pipeline-and-driver.md',
  '06-meta-derive.md',
  '11-copy-on-write.md',
  '13-ownership-in-ir.md',
  '17-c-backend.md',
  '19-llvm-backend.md',
  '21-simulator.md',
  '24-layering-discipline.md',
  '25-structural-guards.md',
  '26-self-host-frontend.md',
  '27-comparison-bootstrap.md',
  'appendix-a-file-map.md',
  'appendix-b-glossary.md',
]

log(`Fixing ${FLAGGED.length} flagged chapters (fix -> fresh confirm per chapter).`)

const results = await pipeline(
  FLAGGED,
  // Stage 1 — FIX
  (file) => agent(
    `${COMMON}\n${GROUND_TRUTH}\n\n# FIX chapter docs/devbook/${file}\n` +
    `1. Read docs/plans/artifacts/devbook_review_findings.json. Find this chapter's reservations under result.chaptersWithReservations[] where file == "docs/devbook/${file}", AND scan result.consistencyFindings[].findings[] for any whose evidence names "${file}".\n` +
    `2. For each issue, apply the reviewer's cited fix. For the substantive ones, use the VERIFIED FACTS above directly (don't re-derive them). For self-host section rewrites, READ the cited self-host files (tests/fixtures/self_host_lowerer/*.gg) so your replacement prose is accurate and specific.\n` +
    `3. Re-derive any file:line you touch from current source so it is correct. Re-Write docs/devbook/${file} with the corrections. Change ONLY what the issues require + obvious knock-ons (e.g. a fact stated 3x).\n` +
    `Return {file, fixesApplied, notes}.`,
    { label: `fix:${file}`, phase: 'Fix', schema: FIX_SCHEMA }
  ),
  // Stage 2 — CONFIRM (fresh re-derivation of the corrected chapter)
  (_fix, file) => agent(
    `${COMMON}\n\n# CONFIRM the corrected chapter docs/devbook/${file}\n` +
    `You are a FRESH reviewer. Read docs/devbook/${file}. Re-derive its load-bearing claims from CURRENT source. Specifically re-check the items that were just supposed to be fixed (read docs/plans/artifacts/devbook_review_findings.json for this file's original issues and confirm each is now correct), AND look for any NEW error the fix may have introduced (a fold can introduce defects). Return verdict "clean" (no remaining issues) or "remaining" with the specific cited issues. Do not invent issues; do not rubber-stamp.`,
    { label: `confirm:${file}`, phase: 'Confirm', schema: CONFIRM_SCHEMA }
  ),
)

const done = results.filter(Boolean)
const clean = done.filter(r => r.verdict === 'clean')
const stillOpen = done.filter(r => r.verdict === 'remaining')
log(`Fixed + confirmed: ${clean.length}/${FLAGGED.length} now clean; ${stillOpen.length} still have remaining issues.`)

return {
  nowClean: clean.map(r => r.file),
  stillOpen: stillOpen.map(r => ({ file: r.file, remainingIssues: r.remainingIssues })),
  summary: { flagged: FLAGGED.length, clean: clean.length, remaining: stillOpen.length },
}
