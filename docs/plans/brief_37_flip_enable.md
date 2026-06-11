# BRIEF — Chain E: enable the #37 Phase-2 default flip (Fix A + Fix B + flip)

Status: v1 (orchestrator draft from scout report 2026-06-11; scout worktree
`agent-a07fcfddc870f61c5`; artifacts rescued to
`docs/plans/chainE_artifacts/` — `strip_dead_decls.py` is the run-proven
Fix-A simulation, `measurements.log` carries every number + command,
`scout_stage1.sh` is the stage-1 repro harness, `bp_check.py` the
edge/param checker that refuted the corruption theory).

## Mission

Make lazy CoW the SELF-HOST DEFAULT by landing the two scout-proven fixes
and removing the `cow_lazy_enabled()` conjunct. The scout REFUTED both
recorded blockers AND the "miscompile class": there is no miscompile (a
stack-capacity cliff — the GREEN bootstrap survives by <49KB on a 12.2MB
ulimit and dies on a stock 8MB one) and no 7x (1.11x at -O2 / 0.98x at -O0;
the prior figure was parallel-cargo CPU thrash). The flipped fixed point is
PROVEN GREEN prototype-assisted (stage-2 == stage-3 byte-identical with the
strip applied). TODO hygiene: the three now-refuted/superseded HIGH entries
(two flip blockers + the miscompile class) move to DONE with the corrected
root cause; Fix C (below) gets its own HIGH entry.

## Scout ground truth (all measured 2026-06-11; commands in measurements.log)

- Stack math: emitted `lower_expr_inner` frame at -O0 = 230,976B (~14,000
  function-scope C locals; 29% of `__v` and 58% of `__s` decls DEAD);
  legitimate ~51-deep `lower_expr↔lower_expr_inner` recursion from the
  51-term `+` concat chain at `derive.gg:172`; 51 × ~231KB ≈ 11.8MB vs
  12.2MB ulimit. ANY +960B crashes; the 2 lazy binds in `lower_expr_inner`
  (`dict_kname`/`dict_vname`, `lower_expr.gg:2512-2513`) add +9,072B → the
  "blocker". `ulimit -s 16384` makes every "corrupt" variant run green with
  BYTE-IDENTICAL output; `ulimit -s 11000` kills the green eager baseline.
  `bp_check.py`: 0 block-param mismatches (1787/1795 edges) — no corruption.
- Emission: `generate_c` = 97% of total time BOTH modes; lazy guard cost
  +1.06% C bytes, +144 clone sites. The REAL emission cost is a Rust-gg
  clone-bomb: a whole-`LirFunction` deep clone PER EXTERN CALL at the
  `m.functions.get(callee_idx).unwrap().params…` reads
  (`lir_codegen.gg:4601-4603`, Rust's Option[Ref]-lift clone branch) —
  self-host-compiled stage-1 emits the same 19.6MB in ~30s vs stage-0's
  295s (-O2). That is **Fix C — NOT in this chain's scope**: file it as its
  own HIGH TODO (a ~10x driver-emission win; likely a Rust `src/` borrow
  improvement in the Option[Ref]-lift, or a self-host source reshape;
  needs its own scout).
- Fix-A prototype effect: ~124K dead decls elided module-wide; frame
  226.5→213.5KB (−5.7%) — clears the cliff. Stripped variants: the
  ex-"miscompile" shape green + byte-identical to eager; lazy-all-17 green.

## The work

### W1 — Fix A: dead-decl elision in BOTH emitters (the flip enabler)
In `lir_codegen.gg emit_function` (self-host) and the Rust twin
(`src/backend/c_lir`, same disease — 6,308/15,691 dead decls): declare only
`__v` ids and `__s` slots actually REFERENCED in the emitted body, not
`0..max_val`. Per devbook/24 rule 3: derive "referenced" from ONE shared
typed operand enumerator — add `lir_inst_operands` (or equivalent) to
`lir.gg`/`src/lir`, shared with `lir_ssa`'s `substitute_inst`
walker, and add an arm-count lint pairing the two (a new LIR instruction
arm added to one walker must fail the suite until added to both).
`docs/plans/chainE_artifacts/strip_dead_decls.py` is the exact semantics to
reproduce (it ran the proofs). Gates: fixed_point (eager) GREEN; full
gate-OFF byte-identity is NOT expected (decl lines vanish) — instead gate on
fixed_point + `self_host_runtime` 0-regress + comparisons baseline-relative
+ the full lazy gated battery + frame-size measurement (report
`lower_expr_inner` frame before/after via the scout's method).

### W2 — Fix B: big-stack main runner + the executable guard
Both backends' emitted `main` (`src/backend/c_lir/mod.rs:1064`;
`lir_codegen.gg` main arm): run the program body on a thread with a large
explicit stack reserve (pthread, e.g. 64MB; keep the shape minimal and
identical in both emitters). This is the CLASS fix — recursion depth scales
with user-program expression nesting, and the bootstrap must not depend on
host ulimits (a stock 8MB kills today's GREEN stage-0). Guard per CLAUDE.md
rule 6: a ~200-term concat-chain fixture compiled through the SELF-HOST
driver (the `self_host_emit_cc_run` route) that crashes pre-Fix-B on an
8MB-equivalent budget and passes post — plus the same fixture as a plain
Rust-gg `run_gg` test. Gates: fixed_point, runtime net, the new guard.

### W3 — the flip + bookkeeping
Remove the `cow_lazy_enabled()` conjunct (`lower.gg:1344` area; re-grep).
Re-prove ON PRISTINE SOURCE what the scout proved prototype-assisted:
`self_host_bootstrap_fixed_point` GREEN flipped; `self_host_runtime`
0-regress (the EMove pair stays unsnapshotted/EXPECTED-WRONG per the
Phase-2 oracle exception); runtime_diff ≥ baseline (record Step-0 FIRST on
the pristine worktree); the lazy battery + canaries; ASan sweep vs a Step-0
eager table; emission timing driver eager-vs-lazy SEQUENTIAL ON AN IDLE BOX
(expect ≤1.2x; never measure under parallel cargo — the documented thrash
multiplier produced the 7x myth); self-compile RSS vs Step-0. Docs:
devbook/11 §Phase-2 updated to DEFAULT (remove the env-gated framing),
language-design §23 parenthetical removed (`bb338f10` added it), devbook/11
gains the stack-cliff + measurement-hygiene lessons (sequential timing;
ulimit-dependence). TODO/DONE: move the two blocker entries + the
miscompile-class entry to DONE with the corrected root cause (they were
refuted/superseded — never leave refuted diagnoses in TODO); ADD Fix C
(HIGH, the clone-bomb, with the scout's numbers + file:line); keep the
GG_COW_LAZY env-var REMOVED from the code (no dead gates).

## Gates summary (executor 0-5; parent re-runs battery + full suite)
0. Step-0 pristine: runtime_diff PARITY, comparisons counts, eager ASan
   table, RSS baseline, frame-size baseline.
1. Per-commit: lib + lints (10) + targeted fixtures; W1 also fixed_point.
2. W2: fixed_point + the new stack guard both routes.
3. W3 flipped: fixed_point GREEN (GG_BUILD_TIMEOUT_SECS=900 first run),
   runtime net, runtime_diff, battery, ASan, sequential emission timing,
   RSS.
4. LLVM spot-checks: stdout parity only (sanitize is dropped on LLVM —
   known).
5. Final: full integration suite on the executor tree.

## Constraints
Standard worktree preamble; explicit adds; no pushes; STOP on contradicted
premises with fresh evidence. File zone: `tests/fixtures/self_host_lowerer/
{lir_codegen.gg,lir.gg,lir_ssa.gg,lower.gg}`, `src/backend/c_lir/`,
`src/lir/` (the Rust walker), `tests/lints.rs` (the arm-count pairing lint),
new fixtures, `tests/integration.rs` (append), devbook/11,
language-design.md, TODO.md, DONE.md. Chains C/D are PARKED (C touches
`src/lir/lower/` — different files than the Rust walker zone; the parent
serializes if both run). Commit order: W1 → W2 → W3 (each gated). Messages
cite this brief + the scout; Co-Authored-By trailer. Line numbers are
scout-fresh at `814f6857` — re-grep.
