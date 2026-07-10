# R-D brief — tighten the three substring-weak guards (D11/D23-wave verification)

> **Round:** review-residuals (xhigh review of `f42eea96..7aad1844`, findings filed in
> TODO.md "D11/D23-wave RESIDUALS", item (e)). **Zone:** `tests/` ONLY — `tests/smith/`,
> `tests/integration.rs` (the `check_gg_fails_no_desugar` harness + its call sites),
> `tests/lints.rs` (the trap parity lint). NO `src/` changes.
> **Scout:** report `/tmp/scout_rr_d_report.md`, prototype `/tmp/scout_rr_d_prototype.patch`
> (3 files, +191/−74), all premises CONFIRMED empirically at `cab529cd`.
> **Status:** v2 — pass-1 reviewed (2 reservations folded: the site count corrected
> 12→11 throughout — the scout's figure was miscounted; the stray EOF-whitespace hunk
> flagged for the executor to drop). Awaiting pass 2.

## Objective

Three guards shipped by the D11/D23 wave pass on the SUBSTRING "throws" or on
hand-listed arrays, so they would NOT catch the regressions they exist to catch.
Tighten each so its failure mode is loud, and pin each with permanent
mutation-style evidence (CLAUDE.md invariant #6: guards, not prose).

## Verified premises (scout, empirical, at `cab529cd`)

1. **smith benign-PASS is loose** — `tests/smith/main.rs:820`:
   `stderr.contains("E_UnhandledThrows") || stderr.contains("throws")` →
   `Verdict::UnhandledThrowsRejected`. Two LIVE misroutes reproduced with the built
   `gg`: (a) a malformed `int risky(int) throws String:` program whose parse-error
   snippet quotes the helper line; (b) an `E_ThrowInNonThrowingFunction` rejection.
   Both currently classify as the tier's benign PASS. The leak-first arm ordering
   (`main.rs:816`) is deliberate and must be preserved.
2. **integration lost its diagnostic pin** — 10 file-variant
   `check_gg_fails_no_desugar` call sites (`tests/integration.rs:4736, :4742,
   :4759, :4764, :4769, :4775, :4781, :4786, :4793, :4800`) + 1 dir-variant site
   (`:4807`) = **11 total** pass the loose `"throws"`; the harness (`:7208-7239`)
   asserts only fail + substring + no ``found `Result[`` leak. Sibling sweep done
   (re-verified by pass-1): the loose class is exactly these 11; the three other
   throws-adjacent `check_gg_fails` calls (`:22296`, `:22301`, `:26274`) pin full
   distinctive messages — leave them.
3. **the parity lint's ratchet is vacuous over its arrays** — `tests/lints.rs:4952-5016`:
   the `_p_exhaustive`/`_g_exhaustive` matches force a compile error on a new
   variant, but all three CHECKS run over separate hand-listed `prod`/`ggd` arrays.
   Demonstrated: deleting `UnwrapErrorOnOk` (non-catchable, so check (c) is blind
   to it) from both arrays leaves the lint PASSING vacuously.

**Load-bearing rendering fact (raw-bytes-verified, independently re-verified by
pass-1):** the `E_` code DOES render in `gg check` stderr —
`report_semantic_error` uses `.with_code(kind.code())` (`src/errors.rs:276`; code
string `src/semantic/errors.rs:687`), and codespan wraps the whole
`error[E_UnhandledThrows]` header in ONE ANSI color span, so it is a contiguous
substring even un-stripped. Verified for all 11 affected fixtures (10 files + the
xmod dir): all exit 1, all carry the code, none leak.

## Work items (the scout's prototype is the base — apply, re-derive judgment, verify)

**W1 — smith (`tests/smith/main.rs`).** Benign-PASS requires the
`E_UnhandledThrows` code; a rejection WITHOUT it (and without a leak) routes to the
EXISTING `GenInvalid` (it already means "generator bug OR compiler false-reject,
triage", `main.rs:556`, gates the green report `main.rs:1068-1072`, keeps repro
dirs). Ordered decision tree stays: slip → leak FIRST → exact-code PASS →
GenInvalid. Update the module docs. Add the PERMANENT `#[cfg(unix)]` unit test
`classify_throws_routing` (6 cases incl. an ANSI-colored PASS and
leak-beats-code ordering) — it FAILS against the old predicate (verified:
`left: "UNHANDLED-THROWS-REJECTED", right: "GEN-INVALID"`) and passes after.

**W2 — integration (`tests/integration.rs`).** Fix at the producer: DROP the
`expect` parameter from `check_gg_fails_no_desugar` (+ the dir variant) entirely;
both harnesses assert a shared `const D23_CODE = "error[E_UnhandledThrows]"`;
reorder the leak-check FIRST for sharper failures. Future call sites structurally
cannot be loose. All 11 call sites updated mechanically. ⚠ Executor: DROP the
prototype's final EOF-whitespace hunk in integration.rs (`@@ -30426,3 +30443,4 @@`
— a stray blank line at EOF unrelated to the change; `git apply` warns on it).

**W3 — lints (`tests/lints.rs`).** A local `trap_parity_pin!` macro: ONE variant
list generates BOTH catch-all-free matches (`V { .. }` patterns — verified to
compile for unit and tuple variants) AND both arrays, so the rustc exhaustiveness
ratchet now REACHES the assertions. Bonus property: a variant added to one enum
forces the other enum too. Mutation evidence: old code + array drift = vacuous
PASS; new code + the same drift on the macro list = hard
`error[E0004]: non-exhaustive patterns` at BOTH generated matches.

## Non-goals

- No `src/` changes of any kind (the E_ code rendering is consumed as-is).
- Do NOT touch the three exact-message `check_gg_fails` throws sites
  (`:22296/:22301/:26274`) — they are already tight.
- Do NOT change smith tier-0 differential classification or the leak-first
  ordering semantics.

## Known risks (accepted, by design)

- Deliberate literal-code coupling: renaming `E_UnhandledThrows` fails these
  guards loudly — desired behavior, not a defect.
- ANSI-span coupling: if codespan ever splits the `error[CODE]` header span, the
  harness fails loudly (visible, fixable).
- `ExitStatusExt::from_raw` in the new unit test is unix-only (`#[cfg(unix)]`);
  the tier-1 batch gate covers non-unix.

## Executor protocol (multi-agent rules apply in full)

Worktree-isolated; worktree-relative paths only; no `git stash`; checkpoint diff
to /tmp after each work item; stage by explicit file name
(`git add tests/smith/main.rs tests/integration.rs tests/lints.rs`); final gates
FOREGROUND with generous timeouts. Apply `/tmp/scout_rr_d_prototype.patch` as the
base, then re-derive judgment on each hunk (you own the result, not the scout).

## Gate list (executor, foreground, tee'd to /tmp)

1. `cargo build`
2. `cargo test --test lints` — expect 52/0 (no count change; the parity lint is
   rewritten, not added)
3. `cargo test --test integration throws -- --test-threads=4` — 36/0
4. `cargo test --test integration d23_ -- --test-threads=4` — 11/0
5. `cargo test --test smith` — 5/0 (4 pre-existing + the new routing unit test)
6. `GG_SMITH_TIER=1 GG_SMITH_SEEDS=1..100 cargo test --test smith -- --nocapture`
   — exactly 100 UNHANDLED-THROWS-REJECTED, empty slip/leak/gen-invalid lists
7. The E0004 mutation re-check (comment a variant out of the macro list, observe
   the compile error at both matches, revert) — paste the evidence.

Full integration sweep + bootstrap stay with the parent (the change is test-only,
but the parent re-runs the standard sweep at round close).
