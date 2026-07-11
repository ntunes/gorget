# Wave A1 brief — D19: `break <value>` / loop-as-expression REMOVAL

> **Batch A, track 1** (ratified wave plan, TODO + ledger LOG 2026-07-11).
> **Zone:** Rust parser/semantic/lowering break arms + ALL self-host copies +
> ggdef elaborate + reference/design docs + 1 new negative fixture. ⚠ A2/A3 run
> concurrently and touch the SAME `src/semantic/{typecheck,resolve,rewrite}.rs` +
> `safety/check_stmt.rs` files with DISJOINT single-arm hunks — the parent
> integrates sequentially; do not touch drop-taint or `&`-bind areas.
> **Scout:** `/tmp/scout_wA1_report.md`, prototype `/tmp/scout_wA1_prototype.patch`
> (38 files, +146/−172 per `git apply --stat`, applies clean; INCLUDES the fixture
> + gitignore pair), measured end-to-end.
> **Status:** v3 — pass-1 folds (R1 driver-diagnostic corrected + gap filed; R2
> grep-gates inlined; R3 counts dropped) + pass-2 CLEAN except one one-line fold
> (the TODO citation tag now greps: "(parse-error surfacing)" added to the entry
> title). Pass-2 independently reproduced: both driver behaviors, all 3 grep-gates
> incl. mutation tests, fmt idempotency, edge probes (`break -1`/`break (2)`
> reject identically cross-compiler), doc-surface sweep, 2 spot gates exact.
> Awaiting pass 3 (the ≥3-minimum confirming pass).

## Verified premises (scout, this session)

1. Zero corpus uses re-confirmed (fixtures/lib/examples/tests; only formatter
   string-literals match).
2. TODO's old line numbers had drifted: the typecheck walk arms are
   `src/semantic/typecheck.rs:7550`/`:7738`; **the unsound arm is `:7972`**
   (`apply_collect_target_rewrites` rewrote the break value against the FUNCTION
   return type — the D19 rationale, confirmed live). Lowering silently discards
   the value (`src/ir/lowering/stmts/mod.rs:265`).
3. The census's `format.gg:471` correction EXTENDED: payload sites across THREE
   divergent self-host copies (parser/resolver/typechecker dirs; check+lowerer
   symlink the typechecker's — pass-1's independent census: ~32 self-host sites
   in 20 real .gg files, ~33 Rust sites in 14 files; completeness is proven by
   the RESIDUE GREPS below, not by counts), incl. 3 `SBreak(None)` synthesis
   sites in `lower_expr.gg` HOF lowering (`:6549/:6707/:6753` — migrate to
   `SBreak()`, driver builds clean through them); plus two surfaces nobody had
   filed: `spec/ggdef/src/elaborate/mod.rs:743-746` (payload consumer — already
   a reject; now simplifies) and the harness canonical formatter
   (`tests/integration.rs:12988`).
4. Doc surface: reference §6.7's two grammar copies AND `docs/language-design.md`
   §5.9 ("Loop as Expression", whole section + EBNF) — deleting §5.9 also fixes
   a pre-existing §18 xref off-by-one.

## Design (prototyped end-to-end — the executor applies + re-derives)

- **Full `Option`-payload removal in BOTH compilers** (AST `Break(Option<…>)` →
  `Break`; all arms), NOT reject-with-dead-payload — the half-feature dies whole.
- **Parse-level typed teaching error**: `ParseErrorKind::BreakWithValue` →
  "break takes no value; loops are not expressions; help: assign to a variable
  declared before the loop". Self-host parsers push the SAME message on each
  copy's native error channel — ⚠ they DIVERGE: the parser dir has Diagnostic
  machinery; resolver/typechecker dirs only have `Vector[String] errors` (the
  scout learned this via a driver build failure — do not unify them here).
- ggdef: simplify the now-dead `elaborate/mod.rs:743-746` payload reject.
- Docs: reference §6.7 (both grammar copies) + delete language-design §5.9 +
  fix the §18 xref.
- **Fixture trap 1 (KEEP):** the negative fixture MUST live in its own directory
  — `tests/fixtures/break_value_removed_error/main.gg` (precedent
  `expr_nesting_too_deep_error/`); top-level placement breaks `fmt_idempotent`
  and pollutes comparison/parity denominators (scout hit this empirically).
- **Fixture trap 2 (KEEP):** `tests/fixtures/.gitignore` is default-deny — the
  prototype includes the required allowlist pair; verify the fixture is TRACKED
  after staging (`git status` must show it).

## Scout-measured gates (the executor re-runs; expectations are exact)

lib 1105/0 · ggdef 104/0 · lints 53/0 · spec_conformance 3/0 (195×3 lanes) ·
integration `break` 5/0 (incl. the new negative) · `loop` 45/0 · `while` 10/0 ·
`for_` 13/0 · `fmt` 5/0. **All five self-host comparison suites byte-identical**
(scout methodology: revert→baseline→re-apply→compare; parser 1489/26, resolver
1493/22, type 1433/82, check 1432/83, lowerer 1223/145/127/20 — diagnostic
suites, only the printed counts mean anything; pass-1 independently reproduced
the parser pair with an identical mismatch SET). c_emit floor 1241 ≥ 1180.
**Driver diagnostics (pass-1 R1 correction): only the self_host_parser driver
emits the teaching DIAG; the check/lowerer lanes ignore ALL parse errors BY
DESIGN today** (`parse_source`, `self_host_typechecker/parser.gg:4630-4655` —
symlinked into check+lowerer — constructs the Parser and drops `p.errors` on the
floor; matches Rust's `rust_check_output` lane shape). ⚠ Do NOT "fix" that lane
property in this track — it is FILED as its own gap (TODO: "self-host
parse-error surfacing").

## Executor protocol

Standard multi-agent rules in full (worktree isolation; relative paths; no stash;
checkpoint diffs to /tmp per milestone; explicit-file staging — the 38-file list
is in the patch; FOREGROUND gates, chunked when >600s). Apply
`/tmp/scout_wA1_prototype.patch`, re-derive judgment hunk by hunk (you own it;
if /tmp is gone, the Design section + the pass-1 review suffice to re-derive).
**Three residue grep-gates, ALL must be empty at commit (pass-1 R2, inlined):**
1. `grep -rn "SBreak(Some\|SBreak(None\|SBreak(_\|SBreak(opt\|SBreak(oe\|SBreak(val\|SBreak(Option" tests/fixtures --include="*.gg"`
2. `grep -rn "Break(Some\|Break(None\|Break(_" src spec tests/*.rs`
3. `grep -rn '"break" \[ expr \]' docs` Final gates: the table above + `self_host_bootstrap_fixed_point`
(FOREGROUND, `GG_BUILD_TIMEOUT_SECS=600` — every touched self-host file is
bootstrap surface). The parent runs the full both-backend sweep at integration.
Commit message: `feat(lang): A1/D19 — remove break-value/loop-as-expression
(whole half-feature, both compilers) + teaching rejection` + standard trailers.
