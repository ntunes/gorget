# Executor Brief — ggdef liveness = `elaborate ∘ eval` (ONE merged landing)

**Track:** ggdef definition-integrity (owner-ruled HIGH). **Supersedes** the Phase-1-only
`wave-ggdef-liveness-brief.md` — owner ruled MERGE (2026-07-15): both proven prototypes land as one coherent
change. **Base:** main. **Contained to `spec/ggdef/` + `docs/` + `tests/fixtures/liveness/` + the MAIN-crate
test files `tests/integration.rs` / `tests/spec_conformance.rs`.** NOT bootstrap-gated (no `.gg` source
change, no `bootstrap_fixed_point`) — BUT `tests/spec_conformance*` build `gg` + the self-host driver, so the
parent gate is `cargo test -p ggdef` **AND** `cargo test --test spec_conformance*`.

## 0. WORKTREE PREAMBLE (non-negotiable)
`pwd` + `git rev-parse --show-toplevel` FIRST; confirm inside your worktree. NEVER touch `/workspace/gorget`
(main) / `/workspace/gorget-1`. Worktree-RELATIVE paths only. Stage by file name (NEVER `git add -a`/
`commit -a`). NEVER `git stash` — `git diff > /tmp/ggmerge_<name>.patch`. Checkpoint to /tmp EARLY. FINAL
gates FOREGROUND. On an Edit desync, re-Read + retry — never a shell heredoc with an absolute path.

## 1. GROUND IN THE RULING + THE TWO PROVEN SCOUTS (read first)
- **The ruling:** `docs/plans/define-gorget/decisions.md` → "GGDEF VERDICT = ELABORATE ∘ EVAL" — elaborate
  owns ALL ratified static rejections (incl. flow-sensitive may-move liveness); eval owns per-path dynamic
  semantics. + the GUARD-RAIL (elaborate models the ratified `:2390` rule; NEVER canonizes production's
  precision; divergences adjudicate against the PROSE — the definition LEADS).
- **Scout reports:** `scouts/scout-ggdef-liveness.md` (eval fix + transition table) + `scouts/scout-ggdef-elaborate-move.md`
  (the may-move pass; the substrate map; the guard-rail 25-probe 100%-agreement result; the stdout finding).
- **The proven combined prototype:** `scouts/patches/ggdef-elaborate-move-proto.patch` (eval fix + elaborate
  gate; 5 files under `spec/ggdef/`, +731/-9; applies CLEAN; ggdef suite 127/0; conformance 195/195 MATCH;
  100% production agreement across 25 probes). **This IS the code.**

## 2. APPLY THE PROVEN PATCH (verify, don't re-derive)
`git apply scouts/patches/ggdef-elaborate-move-proto.patch`. It:
- adds `spec/ggdef/src/elaborate/liveness.rs` (~535 lines): a syntax-directed may-move dataflow — moved-set
  keyed by a fresh `BindingId` per binding (a tiny innermost-first lexical resolver — reference-grade, since
  elaborate's `local_ty` is name-keyed with no scope discipline, the collision hazard); kill on move
  (`Source::Move` / `CallValue{consumes_callee}`); revive on whole-local reassign (projected write does NOT);
  union at branch joins ("moved in ANY arm ⇒ moved after"; diverging arms filtered); MoveInLoop for
  enclosing-scope moves in loops; emits `E_UseAfterMove`/`E_DoubleMove`/`E_MoveInLoop`.
- gates it at the top of `eval::run()` so `verdict = check_liveness ∘ eval` — static IllFormed BEFORE eval.
- includes the eval-layer fix (revive + consume-call-kill — needed so a valid `x=fresh()` RUNS correctly).
Verify: `cargo build -p ggdef` + `cargo test -p ggdef` (127/0); `ggdef run` on the conditional-move probe →
`IllFormed E_UseAfterMove`; `reinit_accept.gg` → `Value "new"`; `consume_callable_double_reject.gg` →
`IllFormed E_DoubleMove`; the full 195-fixture conformance → 195/195 MATCH.

## 3. THE VERDICT TRIPLE FOR A STATIC REJECTION (owner, 2026-07-15 — ratified in decisions.md)
**PRINCIPLE (channels = layers): stdout is EVAL's channel (what the program printed); stderr is
ELABORATE's/the-judge's channel (why it was rejected — and at runtime, the trap).** A statically-rejected
program printed NOTHING (it never ran), so **stdout = "" is semantically correct and must stay EXACTLY
empty** — it means "the program never executed," which IS the verdict. Putting the rejection on stdout would
smear the elaborate/eval boundary. So: **stdout = "" · stderr = `error[E_Code]: … at span` · exit =
check-failure code.** Three pins:
1. **FORMAT — mirror the RATIFIED diagnostic shape, not a ggdef-private one:** `error[E_UseAfterMove]: use of
   moved value 'x' at file:line:col` — same rendering family as production `gg check` and the same pattern as
   ggdef's existing location-suffixed TRAP render. ggdef is the DEFINITION; its stderr is what a human reads
   when adjudicating a lane diff, so it should be the **best-rendered** diagnostic of the four lanes, not the
   tersest. (Location: use ggdef's IR span if it carries one; the self-host's spans lagging production's is a
   lane-QUALITY concern, NOT a conformance axis — see pin 3.)
2. **EXIT CODE — distinguish "never ran" from "ran and died":** a static rejection exits with the
   **compile-error code (match production `gg check`/`gg build` failure exit — pin whatever that is)**; a
   runtime **trap stays 101**. These MUST be distinct — conflating them would let a lane that crashes at
   runtime masquerade as one that correctly rejected. The exit code is part of the verdict triple. VERIFY
   the prototype: `ggdef run <rejected fixture> 1>/tmp/o 2>/tmp/e; wc -c /tmp/o` (must be 0); `cat /tmp/e`
   (must carry the `error[E_Code]: … at span` WHY); `echo $?` (check-failure code, ≠ 101). If the reason
   currently lands on stdout / nowhere, or the exit conflates with the trap code, FIX it.
3. **CONFORMANCE COMPARES THE `E_` CODE + EXIT CLASS ONLY** — prose detail stays impl-defined (the D11
   precedent for traps). So `ggdef -- gen` must record, for a rejection fixture, the **CODE** (+ exit class),
   NOT the full message — else every lane mismatches on wording. Span comparison is NOT a conformance axis
   until someone deliberately ratifies it (self-host spans lag production; that's lane quality, not conformance).

## 4. THE STDOUT-FLIP (expected, correct — call it out, don't mistake it for a regression)
Because rejection now happens BEFORE eval, `move_then_read_is_illformed` (`tests.rs:~186`) which asserted
stdout `"hi\n"` (the OLD dynamic-oracle "output preserved up to the fault") must assert `""`. The prototype
already updates it. Confirm it's the ONLY affected assertion (the other two stdout-on-non-Value asserts are
Trap outcomes the gate never touches; no conformance fixture is affected — all 195 are Value/exit-0).

## 5. THE BOUNDARY-NOTE REWRITE (lands with THIS change — it's now TRUE)
Add the ratified note in `docs/plans/define-gorget/rfc-ggc-ggdef.md` (+ a pointer from `decisions.md` if not
already cross-referenced): **`verdict = check_liveness ∘ eval`. ggdef-elaborate owns ALL ratified static
rejections** (use-after-move, double-move, move-in-loop, conditional-move-then-use) — mirroring production's
`origins.rs` + the self-host's `check_safety_*`; **ggdef-eval owns per-path dynamic semantics** (revive so a
valid re-init RUNS). The old "flow-sensitive static owned by prose+spectests / enumerated escape-hatch list"
is RETRACTED — the list is EMPTY of ownership carve-outs; what remains are honest ggdef *subset* limits
(generics, it-lambdas, B2 constructs — surface gaps, NOT liveness carve-outs). Include the guard-rail
sentence (elaborate models the ratified `:2390` rule, never canonizes production's precision; divergences
adjudicate against the prose). Do NOT ship the retracted wording anywhere.

## 6. THE CONSUMECALLABLE PROSE SENTENCE (still valid from the eval-scout plan)
Add to `docs/language-reference.md` §4.2 Callable Trait Types (after the coercion bullets, ~:461):
> A `ConsumeCallable` is **single-owner**: calling it consumes the callable, so it can be called **at most
> once**. A second call is a compile-time **double-move** (`error[E_DoubleMove]`); any other use after the
> call is a **use-after-move** (`error[E_UseAfterMove]`). `Callable`/`MutCallable` are reusable.
(Re-init revival `:1118` and the may-move merge rule `:2390` ALREADY exist — verify, don't re-add.)

## 7. SHARED TRANSITION-TABLE TESTS + THE CLOSURE-CAPTURE GAP
- Extend the eval-fix tests into the **shared** transition-table: same shapes, the branch-merge row asserts
  the UNION verdict from elaborate (`c=false` → IllFormed) — distinct from eval's per-path (`c=false` →
  Value, which is now unreachable-but-still-true, so assert it via a helper that bypasses the gate OR document
  that the gate makes it IllFormed). Add rows: conditional-move-then-use, moved-in-both-arms, diverging-arm-
  filter (guard-clause `else: return`), rebind-guard loop fold, sibling-scope/shadow (the BindingId resolver).
- **Add the closure-capture targeted test the scout flagged as faithful-but-unproven:** a closure that moves
  a capture / a `ConsumeCallable` param consumed inside a closure body — no corpus fixture exercises it.

## 8. FIXTURE MIGRATION (now that elaborate rejects them — all lanes agree)
- Strip the `KNOWN-ORACLE-BUG` headers from `tests/fixtures/liveness/reinit_accept.gg` + `consume_callable_double_reject.gg`;
  rewrite the `tests/integration.rs:~18855-18877` comment ("beyond ggdef, filed" → "now AGREE with ggdef").
- The conditional-move + consume-double + consuming_self + move_in_loop reject fixtures now ALSO reject in
  ggdef → they can migrate to ggdef-adjudicated **reject-tier** spectests (all lanes agree, NO per-lane split
  — **RIDER 1 is DEAD**, do not build any `static-only:` machinery). `reinit_accept` migrates as a clean
  4-lane run-spectest (`Value "new"`).
- **FLOORS: regenerate, do NOT hardcode** (per `spec_conformance*.rs` module docs + CLAUDE.md "no
  un-regenerated numbers"): run each lane, read the observed MATCH count, set `GGDEF_MATCH_FLOOR` +
  `C/LLVM/SELFHOST_MATCH_FLOOR` + `MIN_FIXTURES` to the observed values in the SAME commit. Do NOT write a
  literal 196/197 — the corpus size depends on exactly which fixtures you migrate.

## 9. RIDER 2 — verify the existing smith guard (do NOT build a tier)
`tests/smith/main.rs:~597-608` already runs ggdef after `gg check` accepts and returns `SpecDiverge` on
`IllFormed`, at tier 0. VERIFY it now ALSO guards the two-layer soundness relation (a program elaborate
accepts must run clean under eval). Note the code fix REMOVES a pre-existing FP in that lane (pre-fix, a
check-accepted reinit-after-move → ggdef IllFormed → spurious SpecDiverge). Add a regression seed if coverage
is thin. Do NOT add a new tier/verdict.

## 10. GATES (FOREGROUND) + REPORT
`cargo test -p ggdef` (127+/0 incl. new tests + all integration bins) · `cargo test --test spec_conformance`
+ `--test spec_conformance_ggdef` (all lanes green at the REGENERATED floors) · the stderr-diagnostic check
(§3) · `cargo test --lib` green · the smith run (RIDER 2). Report: commit hash; ggdef-suite + conformance
counts (regenerated); the conditional-move/reinit/consume-double verdicts; the stderr WHY (paste it); the
stdout-flip is the only assertion changed; the closure-capture test result; confirm the retracted boundary
wording appears NOWHERE; `git -C /workspace/gorget status` CLEAN. Any elaborate-vs-production liveness
disagreement → flag for prose adjudication (guard-rail), do NOT silently match.
