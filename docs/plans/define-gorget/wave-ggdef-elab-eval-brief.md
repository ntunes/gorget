# Executor Brief — ggdef verdict = `elaborate ∘ eval` + the verdict triple (ONE landing)

**Track:** ggdef definition-integrity (owner-ruled HIGH). Lands the `elaborate ∘ eval` may-move pass AND the
ratified verdict triple (stdout="" · stderr=`error[E_Code]: … at span` · exit=1) AND the production-lane
`reject:` adjudication, as ONE coherent change. **Base:** main `b180b9d2` (or later — re-check the patch
applies). **Contained to `spec/ggdef/` + `docs/` + `spectests/run/` + the MAIN-crate test files
`tests/spec_conformance.rs` / `tests/integration.rs`.** NOT bootstrap-gated (no `.gg` self-host SOURCE change —
the one migrated fixture is a spectest, not self-host source). Parent gate = `cargo test -p ggdef` **AND**
`cargo test --test spec_conformance` **AND** the full C+LLVM integration sweep.

## 0. WORKTREE PREAMBLE (non-negotiable)
`pwd` + `git rev-parse --show-toplevel` FIRST; confirm inside your worktree. NEVER touch `/workspace/gorget`
(main) / `/workspace/gorget-1`. Worktree-RELATIVE paths only. Stage by file name (NEVER `git add -a`/
`commit -a`). NEVER `git stash` — `git diff > /tmp/ggvt_<name>.patch`. Checkpoint to /tmp EARLY. FINAL gates
FOREGROUND. On an Edit desync, re-Read + retry — never a shell heredoc with an absolute path.

## 1. GROUND IN THE RULING + THE PROVEN SCOUT (read first)
- **The ruling:** `docs/plans/define-gorget/decisions.md` → "GGDEF VERDICT = ELABORATE ∘ EVAL" (elaborate owns
  ALL ratified static rejections incl. flow-sensitive may-move liveness; eval owns per-path dynamic semantics)
  + the GUARD-RAIL (elaborate models the ratified `:2390` rule; NEVER canonizes production's precision;
  divergences adjudicate against the PROSE — the definition LEADS) + **THE VERDICT TRIPLE** (stdout="" ·
  stderr=`error[E_Code]: … at span` · exit=1) + **THE TOOLCHAIN EXIT-CODE SCHEME (Option A)**: `0` success ·
  `1` static rejection (parse OR semantic OR may-move IllFormed — ONE class) · `2` usage · `101` trap+ICE ·
  `103` ggdef-only fuel.
- **The scout report (the proof):** `scouts/scout-ggdef-verdict-triple.md` — the ggdef verdict triple + the
  production reject arm, all MEASURED green (3 lanes fully; self-host held — see the SPLIT below). Read it: it
  is the map of what the patch does and why.
- **RIDER 1 is DEAD.** Elaborate rejects the same programs, so no lane needs a by-design mismatch. Do NOT build
  any `static-only:` per-lane-split machinery.
- **RIDER 2 SURVIVES** — the existing smith soundness guard; VERIFY, don't rebuild (§9).

## 2. THE SPLIT (owner-confirmed 2026-07-15) — THIS landing is 3-lane-affirm + an honest held self-host floor
The scout found the self-host driver REJECTS a use-after-move correctly (exit 1, empty stdout, right semantic
verdict) but renders a BARE `error:` headline, NOT the ratified `error[E_<code>]:` — so it can't be
E_-code-compared on the reject axis. **That is a SEPARATE HIGH follow-up track** (bootstrap-gated,
self-host-wide diagnostic-format change; filed in `TODO.md` "Self-host reject-diagnostic-rendering
alignment"). For THIS landing:
- ggdef + C + LLVM lanes AFFIRM the reject and compare the E_ code (fully green).
- The self-host lane MISMATCHes the ONE proof reject fixture — **honestly**: `SELFHOST_MATCH_FLOOR` held at its
  current count (one below `MIN_FIXTURES`), the MISMATCH visible in the table, documented at the source. NOT
  silent, NOT faked. The follow-up track raises the floor to four-lane-green.
- This is the owner-blessed divide-and-conquer: excellence is the COMMITTED end result (the self-host track is
  the immediate next work), not a filed-and-hope residual.

## 3. APPLY THE PROVEN 4-LANE PATCH (verify, don't re-derive) — IT IS THE CODE
`git apply docs/plans/define-gorget/scouts/patches/ggdef-vt-4lane-proto.patch`. **11 files, +1183/-69**, applies
CLEAN to `b180b9d2`. It is the SUPERSET (engine + verdict triple + production reject arm + floors + ONE proof
fixture) — it SUPERSEDES the engine-only `ggdef-elaborate-move-proto.patch`. It does ALL of:
- **The may-move engine** (`elaborate/liveness.rs` new + `elaborate/mod.rs` + `eval.rs` gate + `ggc.rs`): the
  syntax-directed dataflow, gated at the top of `eval::run()` so `verdict = check_liveness ∘ eval`; the eval
  fix (revive-on-reassign + consume-call-kill so a valid re-init RUNS).
- **The verdict triple (all THREE pins IMPLEMENTED — not just specified):**
  - **Exit (pin 2, scheme):** `EXIT_ILLFORMED 102→1` (`eval.rs`), `FrontendError::{Parse,Elaborate} 2→1`
    (`main.rs`), `EXIT_USAGE=2` kept, `EXIT_FUEL=103` kept + re-doc'd ggdef-only, header taxonomy rewritten.
    `102` retired.
  - **Stderr (pin 1):** `Outcome::IllFormed` renders `error[{E_Code}]: {msg}{loc}` via the SAME
    `offset_to_location` machinery as the trap arm (`main.rs`); the structured code is `enum MoveErrorKind`
    with `.code()` produced at the violation site (`liveness.rs`), the span is `cur_span` — carried on `Run`
    (NOT `Outcome`, so conformance identity excludes span), never re-parsed from prose (layering rule 2/4).
  - **E_-code comparison (pin 3):** `Run.reject_code` → `render_expect_block` `#   reject:` line (`lib.rs`) →
    `frontmatter.rs` `Expect.reject` → `spec_conformance_ggdef.rs` compares `got_reject == expect.reject`.
- **The production reject arm** (`tests/spec_conformance.rs`): `extract_reject_code` + the `reject:`
  adjudication (mirror of the `trap:` arm — CODE compared only, prose/span impl-defined). Floors regenerated to
  OBSERVED: `GGDEF 195→196`, `C 195→196`, `LLVM 195→196`, `SELFHOST 195` held, `MIN_FIXTURES 195→196`.
- **The ONE proof fixture** `spectests/run/reject_use_after_move.gg` + the `tests.rs` stdout-flip + 3
  reject-biconditional tests.

**VERIFY (measured, don't assume):** `cargo build -p ggdef` (no warnings); `cargo test -p ggdef` (130 lib +
conformance 196/196); `ggdef run` a use-after-move → `error[E_UseAfterMove]: … at f:l:c` on stderr, `""`
stdout, exit **1**; `reinit_accept.gg` → `Value "new"` (exit 0); `consume_callable_double_reject.gg` →
`error[E_DoubleMove]` exit 1; the E_-code proof — corrupt `reject_use_after_move.gg`'s expected code and
confirm `cargo test -p ggdef` conformance FAILS on the reject axis (then restore). `cargo test --test
spec_conformance` green at the new floors (C 196/196, LLVM 196/196, self-host 195/196 w/ the documented reject
MISMATCH).

## 4. THE STDOUT-FLIP (expected, correct — not a regression)
The gate rejects BEFORE eval, so `move_then_read_is_illformed` (`spec/ggdef/src/tests.rs`) now asserts stdout
`""` (was `"hi\n"`, the old dynamic-oracle "output preserved up to the fault"). The patch already flips it.
Confirm it is the ONLY assertion the gate flips — the other **three** stdout-on-non-Value asserts (`tests.rs`
~:306, :338, :925) are Trap outcomes the gate never touches, and no conformance fixture is affected.

## 5. DOCS WRITE-THROUGH (executor work — the patch does NOT touch docs)

### 5a. Boundary note — ADD it (do NOT hunt for old text to rewrite; the RFC has none)
`docs/plans/define-gorget/rfc-ggc-ggdef.md` contains NO prior liveness/flow-sensitive boundary note (the
retracted "prose+spectests / enumerated escape-hatch list" wording lives only in the SUPERSEDED
`wave-ggdef-liveness-brief.md` + `decisions.md` as the thing it corrects — do NOT ship it anywhere). ADD the
ratified note: **`verdict = check_liveness ∘ eval`; ggdef-elaborate owns ALL ratified static rejections**
(use-after-move, double-move, move-in-loop, conditional-move-then-use) — mirroring production `origins.rs` +
the self-host `check_safety_*`; **ggdef-eval owns per-path dynamic semantics** (revive so a valid re-init
RUNS). The escape-hatch list is EMPTY of ownership carve-outs; only honest ggdef *subset* limits remain
(generics, it-lambdas, B2 constructs). Include the GUARD-RAIL sentence (elaborate models the ratified `:2390`
rule, never canonizes production's precision; divergences adjudicate against the prose). Also note both
`static-error` and `parse-error` tiers map to **exit 1**.

### 5b. ConsumeCallable prose sentence — verify then ADD
`docs/language-reference.md` §4.2 Callable Trait Types is at ~:448. The re-init revival (`:1118`) and the
may-move merge rule (`:2390`) ALREADY exist — VERIFY, do NOT re-add. ADD after the coercion bullets:
> A `ConsumeCallable` is **single-owner**: calling it consumes the callable, so it can be called **at most
> once**. A second call is a compile-time **double-move** (`error[E_DoubleMove]`); any other use after the
> call is a **use-after-move** (`error[E_UseAfterMove]`). `Callable`/`MutCallable` are reusable.

### 5c. Exit-code scheme write-through
Add the ratified scheme + a CONSOLIDATED toolchain exit-code table (`0`/`1`/`2`/`101`/`103` with the
success/static-rejection/usage/trap+ICE/fuel meanings) to `docs/language-reference.md`; cross-reference
static=1 / fuel=103 in `spec/prose/trap-codes.md`. (The two LOW production follow-ups — usage errors 1→2;
internal-panic exit-1 collision — are already filed in `TODO.md`; do NOT fix them here.)

## 6. SHARED TRANSITION-TABLE TESTS + THE CLOSURE-CAPTURE GAP
- Extend the eval-fix tests into the SHARED transition table: same shapes, and **PIN BOTH columns** — eval's
  per-path verdict AND elaborate's union verdict — because that contrast IS the dynamic/static distinction
  documented executably (decisions.md). Since the gate lives in `run()`, eval's `c=false → Value` verdict is
  shadowed by the gate; **do NOT settle for "document it" — add a gate-BYPASSING eval entry (a helper that
  calls eval directly) and PIN eval's `Value` verdict**, so the per-path column is executably asserted, not
  merely described. Rows: conditional-move-then-use, moved-in-both-arms, diverging-arm-filter (guard-clause
  `else: return`), rebind-guard loop fold, sibling-scope/shadow (the `BindingId` resolver).
- **Add the closure-capture targeted test the scout flagged as faithful-but-unproven:** a closure that moves a
  capture / a `ConsumeCallable` param consumed inside a closure body — no corpus fixture exercises it.

## 7. FIXTURE MIGRATION — SCOPED (accept-migration NOW; reject-migration DEFERRED to the self-host track)
- **reinit_accept (TWO DISTINCT files — do NOT conflate; "migrate" ≠ "move the driver fixture out"):**
  (a) **CREATE a NEW** `spectests/run/reinit_accept.gg` (with `#!spectest` frontmatter + `expect:` exit 0 /
  stdout `"new\n"`) as a clean 4-lane accept run-spectest — all lanes agree, no self-host issue (it's an
  ACCEPT). (b) **SEPARATELY** strip the now-stale `KNOWN-ORACLE-BUG` header from the DRIVER fixture
  `tests/fixtures/liveness/reinit_accept.gg`, **KEEPING the file AND its entry** in the
  `self_host_driver_accepts_liveness` `legal_fixtures` list (`tests/integration.rs:~18954`, guarded by
  `assert!(fixture.exists())` — the over-rejection guard) — the SAME KEEP as the `consume_callable` bullet
  below. Regenerate the affected floors to observed (see §9).
- **Header strips + comment:** strip the `KNOWN-ORACLE-BUG` header from `consume_callable_double_reject.gg`;
  rewrite the `tests/integration.rs` (~:18855-18877) comment from "beyond ggdef… filed" to "now AGREE with
  ggdef." KEEP the `self_host_driver_{accepts,rejects}_liveness` assertions.
- **DEFER the bulk reject-fixture conformance migration** (conditional-move + consume-double + consuming_self +
  move_in_loop, currently self-host-driver-only in `integration.rs`) — migrating them to `spectests/run/`
  conformance NOW would pile up self-host MISMATCHes (the diagnostic gap). They migrate FOUR-LANE-GREEN as the
  CLOSING step of the self-host reject-diagnostic track (filed HIGH — its scope includes this migration + the
  floor raise). The ONE proof fixture `reject_use_after_move.gg` already in the patch demonstrates the
  machinery; that is sufficient for THIS landing.
- **FLOORS: regenerate, do NOT hardcode** (CLAUDE.md "no un-regenerated numbers"). Two files:
  `GGDEF_MATCH_FLOOR` in `spec/ggdef/tests/spec_conformance_ggdef.rs`; `C/LLVM/SELFHOST_MATCH_FLOOR` +
  `MIN_FIXTURES` in `tests/spec_conformance.rs`. The ggdef and production floors move INDEPENDENTLY, and the
  self-host floor stays one below the others (the documented gap) — EXPECT that divergence, do not "level" it.
  Run each lane, read the printed `MATCH=`, set it in the same commit. RIDER 1 is DEAD — no `static-only:`.

## 8. RIDER 2 — verify the existing smith guard (do NOT build a tier)
`tests/smith/main.rs` (~:597-611) already runs ggdef after `gg check` accepts and returns `SpecDiverge` on
`Outcome::IllFormed`, at tier 0. VERIFY it now ALSO guards the two-layer soundness relation (a program
elaborate accepts must run clean under eval). The code fix REMOVES a pre-existing FP in that lane (pre-fix a
check-accepted reinit-after-move → ggdef IllFormed → spurious SpecDiverge). Add a regression seed if coverage
is thin. Do NOT add a new tier/verdict.

## 9. GATES + REPORT
**Executor FOREGROUND gates:** `cargo test -p ggdef` (130+/0 incl. the reject-biconditional + transition-table
+ closure-capture tests + conformance green at the REGENERATED ggdef floor) · `cargo test --test
spec_conformance` (all lanes green at the REGENERATED floors) · `cargo test --lib` green · the TARGETED
`self_host_driver_accepts_liveness` + `self_host_driver_rejects_liveness` integration tests (they read the
fixtures you edit; run a `GG_BACKEND=llvm … --release` variant too per CLAUDE.md) · the smith run (RIDER 2).
**Do NOT run the full `cargo test --test integration` sweep — that is the PARENT's gate** (CLAUDE.md
multi-agent rule 4: the 15-20 min run stalls agents). NOT bootstrap-gated (no self-host SOURCE change — the
migrated fixture is a spectest).
**FLOORS — regenerate, quote, do NOT hardcode:** the patch CHECKPOINTS the corpus at 196
(`GGDEF/C/LLVM/MIN_FIXTURES`, self-host held 195). The §7 `reinit_accept` accept-migration adds +1 → the corpus
is **~197**; regenerate `GGDEF/C/LLVM/MIN_FIXTURES` to the observed count (~197) and `SELFHOST` to observed
(~196 — still one below, the documented gap), quoting the printed `MATCH=` from each lane. NEVER a literal.
**Report:** commit hash; the ggdef-suite + all-lane conformance counts (regenerated, quoted from the run — NOT
the 196 post-apply checkpoint); the use-after-move stderr WHY (paste `error[E_UseAfterMove]: … at …`) + empty
stdout + exit 1; the E_-code proof (corrupt→FAIL→restore); the stdout-flip is the only ggdef assertion changed;
the closure-capture test result; confirm the retracted boundary wording appears NOWHERE + the self-host
held-floor + its MISMATCH are documented (NOT silent); confirm the self-host reject-diagnostic follow-up is
filed HIGH; **do NOT add any `LANDED`/`DONE` breadcrumb to `TODO.md`** (breadcrumb-check); `git -C
/workspace/gorget status` CLEAN. Any elaborate-vs-production liveness disagreement → flag for prose
adjudication (guard-rail), do NOT silently match.
**PARENT at integrate (NOT the executor):** run the full C+LLVM integration sweep (`--test-threads=4`, `tee`;
LLVM `--release`); move the now-resolved `TODO.md` transition-table entry (the "🆕🐛 [HIGH —
DEFINITION-INTEGRITY … ggdef liveness state-transition table is INCOMPLETE]" entry, ~:261-266 — both phases +
Case 1 + Case 2 + the branch-merge cell + the write-through) to `DONE.md`; the deferred bulk reject-fixture
conformance migration already lives in the self-host follow-up (TODO).
