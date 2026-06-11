# BRIEF — Chain E: enable the #37 Phase-2 default flip (Fix A + Fix B + flip)

Status: v3 (pass-2 review folded 2026-06-11: ⚠ the pthread main is GATED on
the typed `LirModule.target` — FREESTANDING keeps the plain main (shares
the emission site, has no pthreads, zero gate coverage) [p2-R1]; guard
process-binding pinned — leg (i) budget binds the DRIVER, leg (ii) the
produced binary [p2-R2]; exact-token scan mandated (`__[vs]\d+\b`) [p2-R3];
TODO moves cited precisely (:32 + :34 move, :33 stays) + stale-prose sweep
[p2-R4]; LLVM link already `-pthread` on non-macOS noted. Pass 2 verified
all five false-elision routes at file:line, buffer-then-scan feasibility in
both emitters, the sole `cow_lazy_enabled()` use, and post-Fix-B
determinism. v2 was pass-1 folded 2026-06-11: Fix-A "referenced" derivation
pinned to EMITTED-BODY scanning — the typed-walker alternative cannot see
block-param head copies, terminator args, slots, InlineC-rewritten locals,
or cleanup glue [R1]; reuse Rust's existing `Inst::dst()`/`uses()` + add a
`slots()` sibling rather than a fourth walker; the self-host pairing lint
requires rewriting `substitute_inst`'s silent else into explicit arms [R2];
LLVM's own `@main` named as the third main-emission site [R3]; pthread
include+link made UNCONDITIONAL in `gg build` — today gated on
std.async/has_spawn, and the harness's own `-lpthread` would MASK the break
[R4]; the stack guard pins RLIMIT_STACK in-test (post-Fix-B pthread stacks
are mmap'd → PASS-after deterministic) [R5]; the run_gg guard leg is
RUNTIME deep recursion, not a concat chain (which stresses the compiler,
not the program) [R6]; the false-record comment at `lower.gg:1524-1532` and
the lock-in test's env/comments updated at flip [R7]; fixed_point runs SOLO
— its stage deadlines are HARDCODED 600s, the env knob doesn't reach them
[R8]; cross-chain contention stated precisely [R9]; counts not ratios
[R10]. Pass 1 RE-PROVED the stack-cliff causality BOTH directions
(`ulimit -s 11000` kills the green baseline; 12500 passes), the frame size
to the byte (230,976B), the strip end-to-end (124,206 decls elided,
stage-2 byte-identical), and the Fix-C structural 11x. v1 was the
orchestrator draft from scout `agent-a07fcfddc870f61c5`; artifacts at
`docs/plans/chainE_artifacts/`.)

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

- Stack math: emitted `lower_expr_inner` frame at -O0 = 230,976B
  (pass-1-verified to the byte; ~14,000 function-scope C locals; DEAD decl
  COUNTS [p1-R10 — carry counts, not ratios]: 3,140 `__v` + 1,902 `__s`
  self-host, 6,308 Rust-emitted);
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
In `lir_codegen.gg emit_function` (self-host, decls at `:4803-4853`) and
the Rust twin (`src/backend/c_lir/mod.rs:1706-1765`, same disease — 6,308
dead decls): declare only `__v` ids and `__s` slots actually REFERENCED,
not `0..max_val`. ⚠ [p1-R1, the load-bearing spec point] "REFERENCED" is
derived from the **EMITTED BODY** — emit the blocks to a side buffer, scan
for `__v`/`__s` tokens, then emit decls + buffer (the prototype's PROVEN
semantics, `chainE_artifacts/strip_dead_decls.py`). [p2-R3] The token scan
MUST be EXACT-TOKEN (digit-boundary: after `__v<digits>`/`__s<digits>` the
next char is a non-digit — the prototype's `__[vs]\d+\b` semantics,
`strip_dead_decls.py:23`); a substring-`contains` scan is correctness-safe
but retains every id that prefixes a longer live id (`__v1` vs `__v12`),
materially blunting the frame win the W1 gate measures. A typed
inst-operand walker is INSUFFICIENT and would FALSE-ELIDE needed decls: it cannot see
(a) block-param head copies `__vN = __bpN` (`mod.rs:1855` /
`lir_codegen.gg:4915`), (b) terminator-arg copies/returns
(`lir_codegen.gg:4717-4739`, `mod.rs:3032`), (c) slots (SlotLoad/SlotStore/
SlotAddr/MoveSlot/ClosurePack carry them; `substitute_inst` substitutes
ValueIds only), (d) InlineC-rewritten locals (`mod.rs:2919-2935` →
`rewrite_inline_c_locals`; the protective dummy SlotAddrs are DCE-bait —
latent, no live callers, but the emit arm is live), (e) test-cleanup glue
(`helpers.rs:1959/1969/1976`). If an enumerator-based derivation is ever
preferred later, its contract must cover ALL FIVE — file that as a note,
implement body-scan now. [p1-R2] On the Rust side REUSE the existing
`Inst::dst()` (`src/lir/mod.rs:1031`) / `Inst::uses()` (~`:1099`) if any
typed support is needed — do NOT add a fourth walker. The self-host
arm-count pairing lint (new LIR arm must hit both `substitute_inst` and any
shared enumerator) requires first rewriting `substitute_inst`'s silent
`else: return inst` into explicit arms. Gates: fixed_point (eager) GREEN; full
gate-OFF byte-identity is NOT expected (decl lines vanish) — instead gate on
fixed_point + `self_host_runtime` 0-regress + comparisons baseline-relative
+ the full lazy gated battery + frame-size measurement (report
`lower_expr_inner` frame before/after via the scout's method).

### W2 — Fix B: big-stack main runner + the executable guard
THREE main-emission sites [p1-R3]: `src/backend/c_lir/mod.rs:1062+`,
`lir_codegen.gg:4754`, AND the LLVM backend's own `define i32 @main`
(`src/backend/llvm/mod.rs:2157`) — fix all three or explicitly scope
LLVM out with a TODO + the GG_BACKEND=llvm-sweep implication stated. Shape:
run the program body on a pthread with a large explicit stack reserve
(e.g. 64MB via `pthread_attr_setstacksize`), minimal and identical across
emitters. ⚠ [p2-R1, BLOCKER-class] GATE THE RUNNER SHAPE ON THE TYPED
`LirModule.target` (`src/lir/mod.rs:1752-1754`): NATIVE gets the pthread
main; **FREESTANDING keeps today's plain main** — it shares the same main
emission (no target branch at `mod.rs:1064`), swaps the preamble at
`emit_types.rs:1864-1868`, builds `-ffreestanding` with `uefi_stub.c`
(`src/main.rs:988-1063`), has NO pthreads, and has ZERO integration
coverage to catch the break. The `<pthread.h>` include lands in the NATIVE
`RUNTIME_PREAMBLE` only (the freestanding branch early-returns before it). ⚠ [p1-R4
build-breaker]: pthread include+link is CONDITIONAL today (`-lpthread`
gated on `std.async`/`has_spawn` — `src/main.rs:874/1120`,
`add_thread_flags :206`; the preamble has no `<pthread.h>` for non-spawn
programs, `c_runtime.rs:68`). Fix B MUST make both UNCONDITIONAL in the
NATIVE `gg build` C path — otherwise every non-async program link-fails
while the TEST HARNESS (which already passes `-lpthread`,
`integration.rs:14099/14246` and `:15714`) stays green and MASKS it; verify
with a bare `gg build hello.gg` OUTSIDE the harness. [p2-note] The LLVM
link already passes `-pthread` unconditionally on non-macOS
(`src/main.rs:~1416`) — only the C paths at `:874/:1120` are conditional.
The freestanding link path early-returns before `:1120` — naturally scoped. This is the CLASS fix — recursion depth scales with
user-program nesting, and the bootstrap must not depend on host ulimits (a
stock 8MB kills today's GREEN stage-0). Guards per CLAUDE.md rule 6, BOTH
deterministic [p1-R5/R6, process-binding pinned per p2-R2]: (i) the
~200-term concat-chain fixture through the SELF-HOST route — this stresses
the COMPILER's recursion, so the pinned budget must bind the **DRIVER
process** (`self_host_emit_cc_run` spawns driver `integration.rs:15691` →
cc `:15709` → binary `:15738`; the helper has no rlimit hook today — add a
wrapped variant or `pre_exec` setrlimit around the DRIVER spawn): FAILS
pre-Fix-B, PASSES post (explicit `pthread_attr_setstacksize` stacks are
mmap'd, not RLIMIT_STACK-bound → deterministic); (ii) the `run_gg` leg is
a RUNTIME-deep-recursion fixture (recursive Gorget fn, depth × frame >
budget) whose pinned budget binds the **PRODUCED BINARY's execution**
(wrapping the whole pipeline is acceptable — Rust gg itself is fine on
8MB; rlimits inherit) — a concat chain is VACUOUS there (the emitted C
evaluates it in one frame). Gates: fixed_point, runtime net, both guards
both ways.

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
ulimit-dependence). [p1-R7] ALSO: rewrite the now-false-record comment at
`lower.gg:1524-1532` ("MISCOMPILED by the self-host… See TODO" — refuted;
the true rationale is that out-of-line keeps the cliff-critical frame
small) and update the lock-in test's `.env("GG_COW_LAZY","1")` + comments
(`integration.rs:23141-23259`) to default-behavior framing when the gate is
removed. TODO/DONE [p2-R4, precise]: the entries to MOVE to DONE with the
corrected root cause are **TODO.md:32** (the umbrella "FLIP BLOCKER, 2
findings" entry) and **TODO.md:34** (the miscompile-class entry) — both
refuted/superseded; **TODO.md:33 (Rust EMove) STAYS**. Then a stale-prose
sweep: `grep -n "GG_COW_LAZY\|FLIP BLOCKER\|BLOCKED" TODO.md` — the
umbrella header at `:31` and the gate-status prose at `:9`/`:24` also go
stale at flip; update each. ADD Fix C (HIGH, the clone-bomb, with the
scout's numbers + `lir_codegen.gg:4601-4603`); keep the GG_COW_LAZY env-var
REMOVED from the code (no dead gates).

## Gates summary (executor 0-5; parent re-runs battery + full suite)
0. Step-0 pristine: runtime_diff PARITY, comparisons counts, eager ASan
   table, RSS baseline, frame-size baseline.
1. Per-commit: lib + lints (10) + targeted fixtures; W1 also fixed_point.
2. W2: fixed_point + the new stack guard both routes.
3. W3 flipped: fixed_point GREEN — run it SOLO [p1-R8]: its stage-emission
   deadlines are HARDCODED 600s (`integration.rs:14058/14218/14259/14340`),
   the env knob does not reach them, and the flip's ~11% on a ~350s
   emission fits solo but NOT under parallel-cargo thrash; runtime net,
   runtime_diff, battery, ASan, sequential emission timing, RSS.
4. LLVM spot-checks: stdout parity only (sanitize is dropped on LLVM —
   known).
5. Final: full integration suite on the executor tree.

## Constraints
Standard worktree preamble; explicit adds; no pushes; STOP on contradicted
premises with fresh evidence. File zone: `tests/fixtures/self_host_lowerer/
{lir_codegen.gg,lir.gg,lir_ssa.gg,lower.gg}`, `src/backend/c_lir/`,
`src/lir/` (the Rust walker), `tests/lints.rs` (the arm-count pairing lint),
new fixtures, `tests/integration.rs` (append), devbook/11,
language-design.md, TODO.md, DONE.md, `src/main.rs` (the R4 unconditional
pthread link) + `src/backend/c/c_runtime.rs` (the include). [p1-R9]
Cross-chain precision: any Rust-side typed support lands in
`src/lir/mod.rs` (the `dst()`/`uses()` home) — DISJOINT from parked Chain
C's `src/lir/lower/{operands,insts}.rs`+`optimize.rs` and Chain D's
`src/lir/lower/` + `src/main.rs:~1162`; the REAL contention with C/D is
`tests/integration.rs` appends + `src/main.rs` + TODO/DONE — the parent
merges/serializes at integration (D before C per the standing order). Commit order: W1 → W2 → W3 (each gated). Messages
cite this brief + the scout; Co-Authored-By trailer. Line numbers are
scout-fresh at `814f6857` — re-grep.
