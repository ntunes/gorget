# T3b — smith "throws-in-every-position" fuzz tier (executor brief)

> **Wave position:** the follow-on to T3a (D23 enforcement, landed `9d9a6d83`). A smith fuzz
> tier that generates programs placing ONE unhandled `throws` call in each expression position
> and asserts production REJECTS it with `E_UnhandledThrows` — a combinatorial regression net over
> contexts the T3a per-position negative fixtures don't cover. **Scope: `tests/smith/` ONLY.**
> Disjoint from T2a-selfhost / T2b — runs in parallel.
>
> **Grounded in:** `docs/plans/define-gorget/scouts/scout-t3b-smith-throws-tier.md` (measured, with
> a prototype) and `decisions.md` D23.

## The two things the scout established (internalize)
1. **ggdef ALREADY models D23** — an unhandled `throws` call in a non-`throws` context is rejected at
   ELABORATION (`maybe_wrap_throws_call`, `spec/ggdef/src/elaborate/mod.rs:1358-1367`). So **T3b does
   NOT touch ggdef.** The prior D23-scout's "add D23 to ggdef" worry is moot.
2. **T3b is a REJECTION tier, not the tier-0 differential shape.** Post-T3a, production REJECTS at
   `gg check`; the existing smith harness classifies a check-failure as `Verdict::GenInvalid` and
   short-circuits BEFORE ggdef runs (`tests/smith/main.rs:522-531`). So the prior scout's
   "accept → ggdef IllFormed → SPEC-DIVERGE" detector does NOT apply. T3b needs an **inverted oracle**:
   for this tier, a check-FAILURE citing `E_UnhandledThrows` (no `Result[` leak) is the PASS; a
   check-SUCCESS is a FAIL (a real T3a slip). This new oracle SHAPE — not ggdef, not the grammar — is
   the real content of T3b; the reviewers should focus there.

## The single biggest correctness constraint (the false-positive hazard)
The generator MUST emit programs **well-formed EXCEPT for the one unhandled `throws`**. The scout
found by accident that a bare free-fn `int risky(self) throws String:` (no `equip`) is ACCEPTED
(exit 0 — `self` parses as a plain param), while the correct `equip S:` method form rejects. If the
generator emits an ill-formed-for-an-unrelated-reason program, the inverted oracle mis-fires a false
"slip" (or a false GEN-INVALID). Every generated program must be a program that COMPILES if you
remove/handle the single unhandled throws.

## Owner-question rulings (decided; execute these)
- **Q1/Q2:** T3b ships as a rejection tier (owner chose it; value = combinatorial hardening).
- **Q3 (docs write-through — DO IT):** `decisions.md` LOG (~:269, the D23 entry) currently says the
  smith enforcement is "leaks = SPEC-DIVERGE." That mechanism is now STALE: production REJECTS
  post-T3a, and ggdef's D23 = ElabError → smith GGDEF-SKIP, never SPEC-DIVERGE. Correct that sentence
  to the real mechanism: "the smith throws-position tier asserts production REJECTS each unhandled
  throws (an inverted rejection oracle); a check-SUCCESS is a slip." (One-sentence LOG fix — do NOT
  rewrite the ratified decision, just the enforcement-mechanism clause.)
- **Q4:** a slip (production ACCEPTS an unhandled-throws program) = a DEDICATED non-benign verdict
  `Verdict::UnhandledThrowsSlip`, NOT `GenInvalid` (a slip is a real T3a hole; it must stand out).
- **Q5:** the PASS assertion mirrors `check_gg_fails_no_desugar` (`tests/integration.rs:~7208-7239`):
  check-fails AND stderr cites `throws`/`E_UnhandledThrows` AND stderr does NOT contain `found \`Result[`.
- **Q6:** seed sweep via `GG_SMITH_SEEDS=A..B`; this is a CHECK-ONLY tier — SKIP the ~57s self-host
  driver build + the build/ggdef/LLVM lanes for it (only `gg check` is needed).
- **Q7 (FILE, do NOT do here):** a POSITIVE throws differential tier (T3c) — HANDLED throws through
  the full backend differential (ggdef already models it) — is arguably higher-value. FILE it as a
  TODO follow-up for the next agent; do NOT expand T3b.

## Work items

### W1 — the generator tier `program_throws_positions(seed)` (`tests/smith/generator.rs`)
- **MANDATORY core = the free-fn form.** Emit a free-fn throws helper `int risky() throws String:`
  (returning after a `throw` or an `Error(...)` so it's well-formed), then place ONE unhandled call to
  it in a fuzzed expression POSITION per seed: binop operand (`1 + risky()`), fn arg (`f(risky())`),
  plain bind (`int x = risky()`), match scrutinee, bare statement, match-arm tail. Fuzz the SURROUNDING
  context (nest in `if`/`elif`/`for`, as an inner operand) — this combinatorial context is T3b's added
  value over the T3a fixtures.
- **OPTIONAL add = the `equip` method form** (`s.m()` receiver position). This is NEW machinery, not
  "just another helper": tier-0 emits NO `struct`/`equip` (grep-confirmed), so it requires
  synthesizing a `struct S`, an in-scope `S` instance, AND an `equip S:` block with
  `int m(self) throws String:` — and this is EXACTLY where the scout's false-positive bit (a bare
  `int risky(self) throws String:` WITHOUT `equip` is ACCEPTED because `self` parses as a plain param;
  only the `equip S:` form rejects). T3a already covers the method-reject via
  `d23_unhandled_method{,_traitdefault,_xmod}` fixtures, so the free-fn form is the safe core; add the
  equip form ONLY if you spell out + verify the struct+instance+equip scaffolding produces a
  well-formed-except-for-the-one-throws program. Prefer shipping the free-fn core solid over a shaky
  equip form.
- **NEVER place the call** inside a `throws` fn, a `catch`/`rethrow`, a match-Ok/Error, or a
  `Result`-typed destination (those are LEGAL — the program must be rejected ONLY for the one
  unhandled throws). The program must otherwise compile.
- Dispatch: replace the `assert_eq!(tier, 0, …)` at `generator.rs:789-796` with a tier dispatch
  (`GG_SMITH_TIER`→`Config.tier`, `main.rs:387-390`, threaded via `run_seed` `main.rs:739`); keep
  tier-0 as the default. (Bumping `GENERATOR_VERSION` is harmless but NOT load-bearing here — tier is
  already embedded per-program, so seeds are disambiguated by tier; do it or don't, it's not the repro
  guard.)
- **Add a tier-N determinism guard** mirroring `generator_determinism` (`main.rs:826-842`, which pins
  only tier 0): assert the throws-tier `generate(seed, tier)` is byte-identical across two calls AND
  that the program contains `throws` + a helper call. Cheap insurance the diagnostic batch wouldn't
  cleanly surface; include it (or consciously omit with a one-line note — do not silently drop it).

### W2 — the inverted oracle (`tests/smith/main.rs` `classify`)
For the throws-position tier ONLY, `classify` produces one of FOUR outcomes — **name ALL four as
`Verdict` variants with an explicit `is_benign` classification** (`is_benign` is currently
`Match | GgdefSkip` at `main.rs:312-314`; a benign verdict gets its repro dir removed at `:743-744`
and lets the batch print "all seeds benign" at `:923-925` — so the PASS verdict MUST be benign or
every passing seed lands in `suspicious`/`gen_invalid` and the report never goes green):
1. **PASS** — `gg check` FAILS with stderr citing `throws`/`E_UnhandledThrows` and NO `found \`Result[`
   leak (Q5, mirror `check_gg_fails_no_desugar`) → new **`Verdict::UnhandledThrowsRejected`**, wired
   into `is_benign` (**`Match | GgdefSkip | UnhandledThrowsRejected`**) so it's quiet + repro-removed +
   counts toward all-benign. (This is the load-bearing verdict — without the `is_benign` wiring the
   report never greens.)
2. **SLIP** — `gg check` SUCCEEDS (production wrongly accepted an unhandled throws — a real T3a hole)
   → new **`Verdict::UnhandledThrowsSlip`**, NON-benign, listed, blocks the all-benign report.
3. **LEAK** — `gg check` fails but stderr contains `found \`Result[` (correctly rejected, but the
   message leaks the desugar — a diagnostic regression, distinct from a slip) → new
   **`Verdict::ThrowsDesugarLeak`**, NON-benign, listed. (Do NOT fold into SLIP — the program WAS
   rejected; only the message regressed.)
4. **GEN-INVALID** — `gg check` fails for an UNRELATED reason (no `throws`/`E_UnhandledThrows` in
   stderr → the generated program was malformed for some other reason, a generator bug) → the
   EXISTING `Verdict::GenInvalid`.
- Add the two new NON-benign variants (`UnhandledThrowsSlip`, `ThrowsDesugarLeak`) + the benign
  `UnhandledThrowsRejected` to the `Verdict` enum, `is_benign`, and the report/listing paths (mirror
  how `GenInvalid` is listed at `:893` + blocks the report at `:923-925`).
- SKIP the ggdef/build/self-host/LLVM lanes for this tier (check-only, Q6). **⚠ The self-host driver
  build is a SEPARATE unconditional call at `main.rs:875` (`let _ = driver_paths();`, which PANICS if
  the build fails, `:242-246`) OUTSIDE `classify` — an in-`classify` lane-skip does NOT skip it.**
  Gate that call on `cfg.tier != <throws-tier>` so the check-only tier doesn't pay the ~57s build (or
  spuriously panic if the self-host driver is broken for an unrelated reason).

### W3 — docs write-through (Q3)
- Correct the stale enforcement-mechanism clause in `decisions.md` (~:269, D23 LOG entry) per Q3.

### W4 — file the T3c follow-up (Q7)
- Add a TODO entry (Medium): a POSITIVE throws differential smith tier (T3c) — generate programs with
  HANDLED throws (catch/rethrow/Result-capture/auto-propagate) and diff production vs ggdef through
  the full backend differential (ggdef models handled throws). Higher-value than the negative tier;
  for the next round.

## Gate battery (run FOREGROUND, generous timeouts; PASTE output)
```
cargo build
cargo test --test smith 2>&1 | tee /tmp/t3b_smith_$$.log        # existing smith (tier-0) stays green
# run the new tier over a seed sweep — EVERY seed must PASS (production rejects each; no slips, no leaks):
GG_SMITH_TIER=<n> GG_SMITH_SEEDS=1..300 cargo test --test smith -- --nocapture 2>&1 | tee /tmp/t3b_tier_$$.log
cargo test --lib 2>&1 | tee /tmp/t3b_lib_$$.log                 # classify/verdict changes don't break the lib
```
Acceptance: builds; existing tier-0 smith green; the new throws tier over ≥300 seeds shows ZERO
`UnhandledThrowsSlip` and ZERO `ThrowsDesugarLeak` (every seed's unhandled throws is correctly
rejected with a clean message — a regression net over T3a), ZERO spurious `GenInvalid` (proving the
generator emits well-formed-except-for-the-one-throws programs — the false-positive-hazard guard),
and every seed classified benign `UnhandledThrowsRejected`. **If a seed
DOES produce `UnhandledThrowsSlip`, that is a REAL T3a hole — do NOT suppress it; report the seed +
the generated program so it can be triaged (it may be a genuine position T3a missed).** Do NOT run
the full `cargo test --test integration` (parent's job).

## Scope fences
- Touch ONLY: `tests/smith/{generator,main}.rs`, `decisions.md` (the one LOG-sentence fix), `TODO.md`
  (the T3c follow-up). Do NOT touch `spec/ggdef/*` (ggdef already models D23 — no change), the
  self-host `.gg` lowering (T2a-selfhost), `src/backend/*` (T2b), or `src/semantic/*` (T3a is landed).

## Worktree & agent discipline (NON-NEGOTIABLE)
`pwd` + `git rev-parse --show-toplevel` FIRST; confirm BOTH inside your worktree; STOP if either is
`/workspace/gorget` or `/workspace/gorget-1`. Paths RELATIVE to your worktree; on Edit desync re-Read
+ retry the Edit tool (NEVER a heredoc with an absolute path); after any non-Edit write `git -C
/workspace/gorget status` and STOP if it shows changes. Entry: `git merge --ff-only gorget-1
2>/dev/null || true`. **Checkpoint to `/tmp/t3b_report_$$.md` after each work item.** Run FINAL gates
FOREGROUND (do NOT background a long run then end — rule 9). Stage ONLY exact files by name; NEVER
`git add -a`/`.`/`commit -a`; NEVER `git stash`. Commit on your worktree branch, message ending:
```
Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01YWwxrY4NAvQ5uv43X4VjHL
```

## Deliverable
Per work item: files+file:line and one-line what. PASTED gate output — the throws-tier seed-sweep
summary (N seeds, all PASS, 0 slips, 0 leaks, 0 spurious GenInvalid). Any seed that produced an
`UnhandledThrowsSlip` (with the generated program — a real finding, not to be hidden). The T3c
follow-up you filed. Any brief premise that differed from reality (corrected). Branch + commit hash.
