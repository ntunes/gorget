# Executor Brief — ggdef liveness transition-table + the three-layer oracle boundary

**Track:** ggdef definition-integrity (owner-ruled HIGH, run now, ahead of Batch C). **Base:** main.
**Deliverable:** land the proven ggdef transition-table fix (revive + consume-call kill) AND document the
three-layer oracle boundary the audit surfaced AND wire the two operational riders that fall out of it —
so the fix closes the definition-integrity hole *and* makes the boundary self-enforcing. **Footprint
(pass-1 corrected): `spec/ggdef/` (the fix) + repo-root `spectests/run/` (the migrated seed) + `docs/` +
`tests/fixtures/liveness/` + the MAIN-crate test files `tests/integration.rs` (comment), `tests/spec_conformance.rs`
(RIDER-1 adjudication + floor bumps), `tests/smith/main.rs` (RIDER-2 verify).** Not "contained to
`spec/ggdef/`". **No `.gg` SOURCE change and no `bootstrap_fixed_point` re-run** (so not bootstrap-gated in
that narrow sense) — BUT the self-host DRIVER binary IS built and exercised on the new run-seed by
`spec_conformance_selfhost`, so the parent MUST run `cargo test --test spec_conformance` (+ the smith run),
not just `cargo test -p ggdef`.

## 0. WORKTREE PREAMBLE (non-negotiable)
Run `pwd` and `git rev-parse --show-toplevel` FIRST; confirm both inside your worktree. NEVER touch
`/workspace/gorget` (main) or `/workspace/gorget-1`. Worktree-RELATIVE paths only (an absolute
`/workspace/gorget/...` path writes into MAIN). Stage by explicit file name (NEVER `git add -a`/`commit -a`).
NEVER `git stash` — `git diff > /tmp/ggdefexec_<name>.patch`. Checkpoint to /tmp EARLY. Run FINAL gates
FOREGROUND with generous timeouts. On an Edit desync, re-Read + retry the Edit tool — never a shell heredoc
with an absolute path. After any non-Edit write, `git -C /workspace/gorget status` and STOP if it shows changes.

## 1. GROUND IN THE DOCS + THE PROVEN SCOUT
- Scout report `docs/plans/define-gorget/scouts/scout-ggdef-liveness.md` — the complete transition table,
  every-cell measurements, the branch-merge finding, the fix design, the migration plan. READ IT.
- Proven prototype `docs/plans/define-gorget/scouts/patches/ggdef-liveness-fix-proto.patch` — applies CLEAN;
  4 files under `spec/ggdef/src/`, +175/-8; ggdef suite 127/0. This is the CODE fix — apply it as your base.
- `docs/language-design.md` (ownership/move semantics); the reference sections you'll touch (below).

## 2. THE CODE FIX (apply the proven patch, verify)
`git apply docs/plans/define-gorget/scouts/patches/ggdef-liveness-fix-proto.patch`. It:
- adds typed `Ty::Callable{consuming}` resolved ONCE in `ty_of_type` (`ConsumeCallable`→true; `Callable`/
  `MutCallable`→false), read by `CallValue` — a consume-call kills the callee slot; a 2nd call is a
  double-move. (Rule 2/4: the surface type NAME is matched once at type-resolution into a typed field, read
  downstream — the CORRECT layering, not a call-site name-match. Confirm this is how it reads.)
- `resolve_write` Moved arm → `Revive` for a whole-local reassignment; stays IllFormed for a projected write.
- +6 tests (revive, revive-then-move-again, projected-write-stays-illformed, consume-double→IllFormed,
  consume-once-legal, plain-Callable-reusable).
Verify: `cargo build -p ggdef`; `ggdef run tests/fixtures/liveness/reinit_accept.gg` → exit 0 "new";
`ggdef run tests/fixtures/liveness/consume_callable_double_reject.gg` → exit 102; `cargo test -p ggdef` green.

## 3. WRITE-THROUGH PROSE (owner-ratified framing — get the wording exactly right)
**Two of the three normative rules ALREADY EXIST — verify + reference them, do NOT re-add:**
- Re-init revival: `docs/language-reference.md:1118` ("Reassigning a moved variable revives it"). The code
  fix brings ggdef INTO conformance with this — ggdef was lagging its own prose (an intra-definition
  disagreement; see the boundary note).
- May-move merge rule: `docs/language-reference.md:2390` ("if a variable is moved in any branch, it is
  treated as moved after the branch point"). THIS is the normative owner of the static conditional-move axis.

**(a) ADD the ConsumeCallable single-use sentence** — §4.2 Callable Trait Types (after the coercion-hierarchy
bullets, ~`:461`; the terse `# consuming: … (single use)` comment at ~`:455` is not a rule):
> A `ConsumeCallable` is **single-owner**: calling it consumes the callable (its captured environment is
> moved out), so it can be called **at most once**. A second call is a compile-time **double-move**
> (`error[E_DoubleMove]`); any other use of it after the call is a **use-after-move** (`error[E_UseAfterMove]`)
> — exactly as for any other moved value. `Callable` and `MutCallable` are reusable and carry no such restriction.

**(b) ADD the THREE-LAYER ORACLE BOUNDARY note** — this is the reference-grade closure of the audit and is
owner-SIGNED-OFF *with the ownership wording below*. Place it where the project's normative model lives:
a ratified entry in `docs/plans/define-gorget/decisions.md` (the ledger) AND a one-paragraph note in
`docs/plans/define-gorget/rfc-ggc-ggdef.md` (the ggc/ggdef RFC — there is NO `spec/ggdef/README.md`, only
`spec/ggdef/reports/*`; pass-1 pinned this as the natural home). The model has THREE
layers — **do not collapse to two, and do NOT write that implementations own any axis** (implementations
NEVER own normative semantics — ending implementation-defined semantics is the project's whole purpose):
> 1. **ggdef-eval is the sole oracle for DYNAMIC semantics.** A Value/trap/output verdict on a concrete
>    execution path is normative, full stop.
> 2. **ggdef-elaborate is the STATIC oracle for the position-based axes it models** — D10 place-overlap,
>    D12 positions, D10(a) binds, throws totality. This is how the enforcement wave gates; it is NOT demoted.
> 3. **Flow-sensitive static judgments ggdef structurally cannot model are normatively owned by PROSE +
>    SPECTESTS derived from it — never by an implementation.** This is an EXPLICIT, ENUMERATED list;
>    **currently it has exactly ONE member: conditional-move / may-move liveness** (the merge rule at
>    reference `:2390`). Production and the self-host CONFORM to that prose rule; their negative fixtures are
>    generated FROM it; they do not define it. **Adding an axis to this list is an OWNER DECISION** — so
>    "ggdef doesn't model it" can never become a lazy escape hatch for a future track.
> Why the list exists: ggdef is a fuel-bounded big-step interpreter (one concrete path per run) — its
> smallness/determinism/auditability is what makes it trustworthy. A conditional move like `if c: sink(!x)`
> then `use x` is memory-safe on the path where `c` is false, so ggdef-eval correctly returns `Value`; the
> static checkers reject it CONSERVATIVELY per the `:2390` rule. This is the inherent, one-directional
> static-vs-dynamic gap — NOT a ggdef bug, and NOT to be "fixed" with all-paths exploration (that would
> destroy the property that makes ggdef trustworthy).
Add one line noting the re-init bug's nature: *it was an INTRA-DEFINITION disagreement (the reference prose
said "revives"; ggdef-eval said IllFormed) — the definition's own twin-drift. The triple (prose, ggdef,
spectests) must agree; a disagreement is a defect in the DEFINITION regardless of which artifact is right.*

## 4. FIXTURE MIGRATION (close the KNOWN-ORACLE-BUG pins)
1. Strip the `KNOWN-ORACLE-BUG` headers from `tests/fixtures/liveness/reinit_accept.gg` (:3-5) and
   `consume_callable_double_reject.gg` (:4-6). Rewrite the comment block at `tests/integration.rs:~18855-18877`
   from "two axes go BEYOND ggdef… filed HIGH/MED" to "now AGREE with ggdef (fix landed)". KEEP the
   `self_host_driver_{accepts,rejects}_liveness` assertions (they pin the self-host lane).
2. **Only `reinit_accept` becomes a run-spectest** (it is a CLEAN 4-lane MATCH — C/LLVM/self-host all
   compile+run → `Value` "new"; ggdef → "new"). Add `spectests/run/liveness_reinit_accept.gg` with
   `#!spectest`/`# mode: run`/`# adjudicator: ggdef` frontmatter; `cargo run -p ggdef -- gen …` fills
   `expect:` (exit 0, stdout "new\n"). **`consume_callable_double` is NOT a run-spectest** — production
   COMPILE-REJECTS it (`E_DoubleMove` → `BuildFail`, not a runnable program), so it can't be a clean run
   seed (pass-1 review). It is ALREADY pinned cross-lane by: (a) the new ggdef unit test consume-double→
   IllFormed [in §2's 6 tests], (b) the existing production negative fixture `consume_callable_once_error.gg`,
   (c) the `self_host_driver_rejects_liveness` assertion. That IS "all lanes pin it permanently" — a
   compile-rejected program is pinned by negatives + unit tests, NOT run-spectests. Do NOT force it into the
   run-corpus (it would need RIDER-1-style machinery AND it isn't even RIDER-1's shape — ggdef IllFormeds it,
   doesn't `Value` it). State this reasoning in your report.
3. Floor bumps IN THE SAME COMMIT (bump-on-improvement rule): `GGDEF_MATCH_FLOOR` 195→**196** (only
   reinit_accept added, +1) in `spec_conformance_ggdef.rs:45`; AND the production-lane floors
   `C/LLVM/SELFHOST_MATCH_FLOOR` + `MIN_FIXTURES` 195→196 in `tests/spec_conformance.rs:~79-89` (reinit_accept
   is a clean +1 on each production lane too — pass-1 caught that the brief bumped only the ggdef floor).

## 5. RIDER 1 — the `static-only` discriminator (decided NOW; MAIN-crate work — pass-1 corrected the scope)
First, the **fixture-authoring rule** (state it in the docs boundary note + the spectest authoring guide):
when writing a liveness reject fixture, make the violating path the EXECUTED path wherever possible, so the
ggdef lane and the static lanes AGREE (this is why no current fixture is affected — verified). BUT the first
genuinely-conservative case — **maybe-moved, statically REJECTED, `Value` under ggdef by the `:2390` gap** —
mismatches BY DESIGN, and there is no mechanism to express it today.
**The real scope (pass-1 review — bigger than a frontmatter tag):**
- The run-tier `expect:` block (`spectests` `frontmatter.rs`, `Expect{exit,stdout,trap}`) has NO field for a
  static-rejection expectation. And **`tests/spec_conformance.rs`** (MAIN crate — the brief originally failed
  to name it) adjudicates the production/self-host lanes and treats a compile-reject as
  `Verdict::BuildFail` (a non-MATCH "defect surface", ~:504-508) — there is NO notion of an EXPECTED
  rejection. The ggdef lane needs NO change (it already reads `expect:`=Value).
- So the mechanism = (a) a new frontmatter discriminator (`static-only:` or an `expect: static-reject` on the
  production lanes — match the existing grammar) + (b) adjudication in `tests/spec_conformance.rs` so a tagged
  case's production/self-host lanes EXPECT a compile-reject (MATCH) while the ggdef lane expects `Value`.
- Implement the mechanism + ONE **synthetic** example that is RIDER 1's actual target: a conditional-move
  where the moving branch is NOT taken, e.g. `if <statically-false-ish cond>: sink(!x)` then `print(x)` —
  production/self-host may-move-REJECT it (per `:2390`), ggdef-eval returns `Value` (x never moved on the
  run path). Tag it; the ggdef lane expects `Value`, the static lanes expect the reject. (This is DISTINCT
  from `consume_callable_double`, which ggdef IllFormeds — see §4; do NOT use that as the RIDER-1 example.)
If the harness structure makes this materially different from described, REPORT it — don't force a wrong
shape. This is the mechanism that keeps §6's "conservative static checkers" clause from drifting into
"unconstrained," so it lands NOW even though only the synthetic case needs it today.

## 6. RIDER 2 — the soundness guard ALREADY EXISTS; verify + add a regression seed (Core #6)
The gap is ONE-DIRECTIONAL: static may over-reject relative to a single path, but **a statically-ACCEPTED
program MUST run dynamically clean under ggdef on every input** — an accepted program that hits a liveness
`IllFormed` in ggdef is a static-checker SOUNDNESS bug. **This guard is ALREADY IMPLEMENTED** (pass-1
review): `tests/smith/main.rs:~597-608` — right after `gg check` accepts, `ggdef::run_source(source,
GGDEF_FUEL)` runs in-process and `Outcome::IllFormed → Verdict::SpecDiverge{"gg check ACCEPTED but ggdef
IllFormed: …"}`. It runs at **tier 0 (default)**, whose grammar already generates move-shaped programs
(module doc ~:76-77). ("P1-E" is NOT a lane — it's a provenance label on the `GGDEF_FUEL` comment ~:522.)
So DO NOT add a new tier/verdict. Instead: (a) VERIFY the existing lane covers liveness-`IllFormed`-on-
accepted (read `main.rs:597-608` + confirm tier-0 grammar emits reinit-after-move / move-then-reassign
shapes); (b) if move-shape coverage is thin, add a targeted regression SEED and/or a small grammar emphasis
so the guard actually exercises the class; (c) **note in your report: the code fix REMOVES a pre-existing
FALSE-POSITIVE in this lane** — before the fix, any generated check-accepted program doing reinit-after-move
would trip a spurious SPEC-DIVERGE (ggdef said IllFormed on legal code). If the lane is NOT as described,
REPORT it — but the expectation is verify-and-augment, not build.

## 7. FILE (do NOT scope in) — a LOW idea
Append to `TODO.md` (Low): *a cheap audit pass that diffs the reference's normative move/ownership sentences
against ggdef-eval behavior would have caught the re-init intra-definition twin-drift — file as a LOW idea
for a future prose↔ggdef conformance auditor.* One entry; do not build it here.

## 8. GATES (agent, FOREGROUND)
- `cargo build -p ggdef` + `cargo test -p ggdef` (expect 127+/0 lib incl. your new tests + all 8 integration
  binaries green, esp. `spec_conformance_ggdef` at the bumped floor 197).
- The 2 migrated spectests pass; `ggdef run` on both fixtures gives the correct verdicts.
- Rider 1's synthetic example adjudicates correctly; Rider 2's existing lane verified (+ any regression seed).
- `cargo test --lib` (Rust unit) stays green.
**`cargo test --test spec_conformance`** is a LOAD-BEARING gate here (NOT just `cargo test -p ggdef`): it
builds `gg` AND the self-host driver and runs all four lanes over the new `reinit_accept` run-seed + the
RIDER-1 tagged example — this is where a floor-bump miss or a RIDER-1 adjudication bug surfaces. Plus the
smith run for RIDER 2. No `bootstrap_fixed_point` (no `.gg` source change), but this is NOT "ggdef only".

## 9. FINAL REPORT
Commit hash(es) + summary; the ggdef suite result (counts) + conformance floor; the 2 fixtures' verdicts
(reinit→Value/"new", consume-double→IllFormed); confirmation the boundary note uses the THREE-layer +
prose/spectests-own-it wording (NOT implementations-own-it); Rider 1 mechanism + example; Rider 2 tier +
its result; the filed LOW idea; anything that didn't fit the described mechanism (riders) flagged LOUDLY;
confirm `git -C /workspace/gorget status` CLEAN.
