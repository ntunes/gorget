# SCOUT REPORT — T3b: smith "throws-in-every-position" fuzz tier (D23 follow-on)

Read-only scout. Worktree `/workspace/gorget/.claude/worktrees/agent-a4a8d1485ca7a9e0e`.
`gg` + `ggdef` built here (`cargo build` OK). Every prototype `.gg` in `/tmp/t3b_proto_11707/`.

## TL;DR — T3b is SMALLER than the prior scout feared, but a DIFFERENT SHAPE than "add a tier"

1. **The prior scout's ggdef dependency is ALREADY SATISFIED.** ggdef models D23: an
   unhandled `throws` call in a non-`throws` context is rejected at ELABORATION
   (`maybe_wrap_throws_call`, `elaborate/mod.rs:1358-1367`). So "T3b also needs D23 added
   to ggdef" is FALSE — that work is done. **T3b = tests/smith only.**
2. **BUT the smith verdict lane cannot express T3b's PASS today, and the prior scout's
   SPEC-DIVERGE detection story does not hold post-T3a.** After T3a production REJECTS at
   `gg check` — which the harness classifies **GEN-INVALID** (a kept, listed, triage
   bucket) and which SHORT-CIRCUITS before ggdef ever runs (`main.rs:527-529`). There is no
   SPEC-DIVERGE and no clean PASS. So T3b is NOT "generate valid programs and diff backends"
   (the tier-0 differential shape) — it is a **REJECTION tier**: generate a program that is
   well-formed except for one unhandled `throws`, and ASSERT production rejects it with
   `E_UnhandledThrows`. That requires an **inverted oracle** in `classify`, not just a new
   grammar. That is the real T3b work, and it is contained to `tests/smith/`.
3. **Honest value framing:** T3a already shipped a per-position negative-fixture corpus
   (`d23_unhandled_{binop,arg,bind,scrutinee,statement,matcharm,method,method_traitdefault,
   method_xmod}.gg` + `check_gg_fails_no_desugar`). T3b's ADDED value is **combinatorial
   context** (the unhandled call nested in if/elif/for, as an inner operand, mixed with
   tier-0 shapes) — a hardening net that sweeps future positions automatically, NOT a
   critical gap. Worth doing (cheap, mechanical), but incremental; the owner should know it.

---

## SECTION 1 — PREMISE VERIFICATION

**P1 — tier knob at `generator.rs:789` (`assert_eq!(tier, 0, …)`) → CONFIRMED.**
`generator.rs:789-796`: `pub fn generate(seed, tier)` asserts `tier == 0` then returns
`Gen::new(seed).program(seed)`. Tier is read from `GG_SMITH_TIER` (`main.rs:387-390`,
default 0) into `Config.tier`, threaded to `generate` via `run_seed` (`main.rs:739`).
The knob is plumbed but only tier-0 grammar exists. Dispatch today = the assert.

**P2 — verdict/oracle lane classification + what happens on `gg check` FAIL → CONFIRMED, and it is the crux.**
`classify` (`main.rs:510-731`) runs oracles in cost order:
- **(1) `gg check`** (`main.rs:522-531`): `Ok(o) if !o.status.success()` → `return
  Verdict::GenInvalid`. **SHORT-CIRCUIT — ggdef and all later lanes are skipped.**
- (1b) ggdef (`main.rs:543-568`): runs ONLY after check accepts. `Outcome::IllFormed` →
  immediate `SpecDiverge` (`main.rs:548-555`); `FrontendError::Elaborate` →
  `GgdefOracle::Skip` = GGDEF-SKIP (`main.rs:565-567`); Value/Trap held for step 3b.
- (2) C build, (3) C run, (3b) ggdef reconcile, (4) self-host, (5) LLVM.

`GenInvalid` is doc'd "a generator bug OR a compiler false-reject; triage, not automatically
ours" (`main.rs:520-521`). It is **NOT benign** (`is_benign` = Match|GgdefSkip only,
`main.rs:312-314`), so its repro dir is kept (`main.rs:743`), it is printed
(`main.rs:768-779`), pushed to `gen_invalid[]` (`main.rs:893`), and a non-empty `gen_invalid`
blocks the "all seeds benign" report (`main.rs:923`). **So every T3b seed under the current
harness = a kept, listed GEN-INVALID. Not a PASS.** VERIFIED end-to-end (Section 2).

**P3 — does ggdef model D23? → CONFIRMED: YES, at elaboration (this is the big scope finding).**
`maybe_wrap_throws_call` (`elaborate/mod.rs:1331-1369`): for a call to a `fn_throws` callee,
`CaptureCtx::None` while `!current_fn_throws` (not a match-Ok/Error scrutinee, not a
`Result[_,_]`-typed dest) → `Err(ElabError::new("call to `throws` function `{callee}` in a
non-`throws` context that does not consume or capture the `Result`…"))` (`:1359-1367`). The
doc comment at `:207-209` states the rule: auto-propagates inside a `throws` fn, "a LOUD
error elsewhere."
- That is a `FrontendError::Elaborate` (`lib.rs:44-45`), which smith maps to **GGDEF-SKIP**,
  NOT SPEC-DIVERGE. `Outcome::IllFormed` (the SPEC-DIVERGE trigger) is an EVAL-time outcome
  (`eval.rs` `Halt::IllFormed`); the unhandled `throws` never reaches eval because
  elaboration rejects first. (`eval.rs:312` has a defensive top-frame `Propagate`-escape
  IllFormed, but elaboration pre-empts it.)
- **Consequence for the prior scout's §3e:** its detection story ("production ACCEPTS but
  ggdef returns IllFormed → SPEC-DIVERGE") is doubly moot post-T3a: (a) production doesn't
  accept, and (b) even if it did, ggdef's D23 rejection is an ElabError→GGDEF-SKIP, not
  IllFormed→SPEC-DIVERGE. The ggdef lane cannot be T3b's detector.

**P4 — how programs/seeds are generated → CONFIRMED.**
`Gen::program(seed)` (`generator.rs:754-782`): 0-3 helpers (`gen_helper`), then `void main():`
with fixed anchors (`v0/w0/i0/s0`), a 12-26-stmt body via `block`/`stmt`, trailing
`print("end")` sentinel. Pinned splitmix64 PRNG; `GENERATOR_VERSION=1` is the reproducibility
key (`generator.rs:51-54`). **tier-0 emits ZERO `throws`** (grep count 0) — the throws tier
is disjoint new territory. Seeds swept via `GG_SMITH_SEEDS=A..B` (inclusive,
`parse_seed_range`, `main.rs:792-806`); `GG_SMITH_TIER` selects the tier.

---

## SECTION 2 — THE KEY-QUESTION MEASUREMENT (scope determinant)

**Question:** does the verdict lane treat a production-rejected unhandled-throws program as a
SPEC-DIVERGE flood, or handle the rejection gracefully?

**Answer (measured):** NEITHER floods NOR provides a clean PASS. Production REJECTS →
**GEN-INVALID**, short-circuiting before ggdef. No SPEC-DIVERGE. ggdef is never consulted.

Prototype (binop position, `/tmp/t3b_proto_11707/binop.gg`):
```
int risky() throws String:
    throw "boom"
void main():
    int i0 = 5
    int x = 1 + risky()
    print(x)
    print("end")
```
```
$ ./target/debug/gg check binop.gg
error[E_UnhandledThrows]: this call throws `String` but the error is not handled here;
  declare the enclosing function `throws String`, or handle it with `catch`, `rethrow`,
  or by binding the result to a `Result[T, String]`   ... 1 error(s) found     EXIT=1
$ ./target/debug/ggdef run binop.gg
ggdef: elaboration error @ 89..94: call to `throws` function `risky` in a non-`throws`
  context that does not consume or capture the `Result` …                       EXIT=2
```
Every dangerous position rejects consistently (production `gg check` exit / ggdef `run` exit):

| position (in `main`)                       | gg check | ggdef run | pre-T3a mode |
|--------------------------------------------|:--------:|:---------:|--------------|
| binop operand `1 + risky()`                | 1 REJECT | 2 ElabErr | LEAK |
| bare statement `risky()`                   | 1 REJECT | 2 ElabErr | SILENT SWALLOW |
| match scrutinee `match risky():`           | 1 REJECT | 2 ElabErr | SILENT SWALLOW |
| equip-method `1 + s.risky()`               | 1 REJECT | 2 ElabErr | SILENT MISCOMPILE (garbage) |
| nested in a for-loop `int x = e1 + risky()`| 1 REJECT | 2 ElabErr | (fuzz context; not fixture-covered) |

gg-check exit 1 = the harness's step (1) `!success` branch = `Verdict::GenInvalid`. So:
**under the current harness every T3b seed is a kept, listed GEN-INVALID.** VERIFIED against
`main.rs:527-529` + the is_benign/gen_invalid chain (P2).

**FALSE-POSITIVE hazard discovered by accident (load-bearing for the design):** my first
method test used a bare free-fn `int risky(self) throws String:` (not an `equip` block) —
production ACCEPTED it (exit 0), because `self` parsed as an ordinary param and the call did
not resolve to a throws method. With the correct `equip S:` shape it REJECTS (exit 1,
`method2.gg`). Lesson: **the generator must emit programs that are well-formed EXCEPT for the
one unhandled `throws`.** If it emits a malformed/mis-resolving shape, production may accept
it and the inverted oracle will mis-fire a false "slip." This is the single biggest
correctness constraint on the tier.

---

## SECTION 3 — DESIGN PROPOSAL

### 3a. The tier is a REJECTION tier (inverted oracle), not a differential

Because the generated programs DON'T COMPILE, there is nothing to run/diff. The throws tier's
verdict is the inverse of tier-0's:

- **PASS** — `gg check` FAILS **and** stderr contains the D23 signal (`E_UnhandledThrows` or
  `throws`) **and** stderr does NOT contain the desugar leak `found `Result[`. (Mirrors the
  existing `check_gg_fails_no_desugar` contract, `integration.rs:7208-7239`.)
- **FAIL — the load-bearing catch** — `gg check` SUCCEEDS. A position that accepted an
  unhandled `throws` is exactly a T3a hole (swallow/garbage). New verdict, e.g.
  `UnhandledThrowsSlip`.
- **FAIL — leak regression** — `gg check` fails but stderr leaks `found `Result[` (the
  pre-T3a desugar surfaced as the found type). New/shared verdict.
- **GEN-INVALID** — `gg check` fails for some OTHER reason (no `throws`/`E_UnhandledThrows`
  in stderr): a genuine generator bug (a malformed program). Kept for triage — same bucket
  as today.

Steps 1b-5 (ggdef, C build/run, self-host, LLVM) are SKIPPED for this tier — there is no
compiled artifact. This is the cleanest design and needs NO ggdef, NO backend lanes.

### 3b. Where the inversion lives — contained branch in `classify`

`Config` already carries `tier` (`main.rs:377-394`). Two clean options:
- **A1 (recommended): branch `classify` on an "expected-reject" predicate** derived from
  `cfg.tier` (or an explicit `cfg.expect_reject`). If set, after step (1) run the four-way
  classification above and RETURN; never reach ggdef/build. ~30-40 lines + 1-2 `Verdict`
  variants (`label()`/`detail()`/`is_benign()` arms are exhaustive, so the compiler forces
  every arm — a free ratchet). `PASS` should be benign (quiet, repro removed).
- **A2 (alternative): a separate `smith_throws_batch` test** with its own `classify_reject`.
  Cleaner separation, more duplication (driver build/parallel-map plumbing). Recommend A1.

### 3c. The generator — `Gen::program_throws_positions(seed)`

- Emit a small throws-helper set once per program: at minimum `int risky() throws String:
  throw "boom"`; for variety a `String risky_s() throws String: throw "boom"` and an
  `equip`-method form (`equip S: int risky(self) throws String: …`) so the method receiver
  position is covered.
- Pick ONE position per seed (via the PRNG / `seed % N`) and place an UNHANDLED call to the
  helper there, wrapped in otherwise-valid tier-0-flavored code. Positions:
  binop operand, call arg (to a non-throws helper), bind RHS, match scrutinee (non-Result
  arms), bare statement, match-arm tail (expr-match), equip-method receiver-result.
- **Fuzz the CONTEXT** (the value-add over fixtures): optionally nest the position inside an
  `if`/`elif`/`for` body, or make it an inner operand of a multi-term expression, using the
  existing `cond`/`int_expr` builders. The unhandled call must land in a NON-throws scope
  (`main` is `void` → always non-throws; simplest to keep it in `main`).
- **Invariants the generator MUST hold** (from the Section-2 false-positive lesson):
  (i) every construct well-formed EXCEPT the unhandled `throws`; (ii) the call is genuinely
  unhandled — NEVER inside a `throws` fn, a `catch`/`rethrow`, a `match … case Ok/Error`, or
  a `Result[_,_]`-typed destination (those are VALID and would flip the expected verdict);
  (iii) keep `void main():` + trailing `print("end")` skeleton for consistency; (iv) bump
  `GENERATOR_VERSION` (the grammar changed) so tier-0 seed numbers aren't misread.

### 3d. Always-on determinism guard for the new tier

`generator_determinism` (`main.rs:826-842`) pins tier 0 explicitly. Add a sibling that
generates the throws tier twice per seed and asserts byte-equality + that the program
contains `throws` and a helper call — cheap, keeps the tier reproducible.

### 3e. Rejected alternative — make ggdef emit `Outcome::IllFormed` for D23 (so SPEC-DIVERGE catches holes)

The prior scout's framing implied this. Reject it: (a) it doesn't help the common case
(production rejects → GEN-INVALID before ggdef); (b) D23 is a static well-formedness property
correctly rejected at elaboration, NOT a runtime `IllFormed`; (c) it still gives no clean
PASS. The inverted oracle is strictly better and needs no ggdef change.

---

## SECTION 4 — RECOMMENDED SLICING + TRUE SCOPE + SIZE/RISK

**True scope: `tests/smith/` ONLY. ggdef needs NO change (D23 already modeled). One
agent-sized brief.**

| component | files | size | risk |
|-----------|-------|:----:|------|
| `program_throws_positions` generator + throws-helper emit + position/context placement; tier dispatch (replace the `assert_eq!`); `GENERATOR_VERSION` bump | `tests/smith/generator.rs` | M (~80-150 lines) | Med — MUST emit only-unhandled, well-formed programs (Section-2 false-positive constraint) |
| inverted-oracle branch in `classify` + 1-2 `Verdict` variants (`UnhandledThrowsSlip`, leak-regression) + benign PASS; skip lanes 1b-5 for the tier | `tests/smith/main.rs` | S-M (~40 lines) | Low-med — touches core `classify`; exhaustive `match` arms force completeness |
| always-on determinism guard for the tier | `tests/smith/main.rs` | S | Low |
| (optional) doc correction: the stale "leaks = SPEC-DIVERGE" LOG line | `docs/plans/define-gorget/decisions.md:269` | S | Low |

**Total: MEDIUM, ONE brief.** NOT bigger than "a smith tier" in FILE scope (tests/smith
only), but it is more than "add a grammar": it introduces a NEW oracle SHAPE (rejection vs
differential) to a harness that was purely differential. That architectural wrinkle — not
ggdef — is the real content. Flag it honestly in the brief so reviewers evaluate the
inverted-oracle design, not just the grammar.

**Dependencies:** NONE blocking. ggdef D23 modeling is done. T3a (the enforcement + fixtures)
is landed here (`9d9a6d83`, `131e0977`; `E_UnhandledThrows` at `errors.rs:395`). T3b is
disjoint from every other track (tests/smith is its own crate) and can run anytime.

**Does it split?** No — keep it one brief. Splitting the generator from the oracle would
land a grammar with no way to judge its output (or an oracle with nothing to feed it).

---

## SECTION 5 — OWNER DESIGN QUESTIONS (with recommendations)

**Q1 — Is a fuzz tier worth it OVER the existing per-position negative fixtures?**
T3a already asserts every enumerated position rejects (`d23_unhandled_*.gg` +
`check_gg_fails_no_desugar`). The fuzzer adds combinatorial CONTEXT and auto-sweeps future
positions. **Recommend: yes, but scope it as HARDENING, not a gap-closure.** It's cheap once
the inverted-oracle plumbing exists and future-proofs as the language grows (new expression
forms get swept free). If the owner wants minimal effort, an acceptable lighter alternative
is to just add ~5 nested-context fixtures — but that doesn't scale like the fuzzer.

**Q2 — Confirm the tier is a REJECTION tier (inverted oracle), not the tier-0 differential.**
This is the load-bearing design decision. **Recommend: yes** — the programs can't compile, so
there's nothing to diff; the assertion is "production must reject with `E_UnhandledThrows`."
Owner should sign off that a smith tier is allowed to be non-differential.

**Q3 — The ratified D23 text says "leaks = SPEC-DIVERGE" (`decisions.md:269`). That framing
predates T3a (when production LEAKED). Post-T3a production REJECTS → GEN-INVALID, and ggdef's
D23 rejection is ElabError→GGDEF-SKIP, never SPEC-DIVERGE.** **Recommend: correct the LOG line
to describe the rejection-tier mechanism (assert production rejects; a production ACCEPT =
slip), and note ggdef already models D23.** The decision's INTENT (fuzz every position)
stands; only the mechanism sentence is stale.

**Q4 — How does a "slip" (a position where production ACCEPTS) assert as a FAILURE?**
**Recommend:** a dedicated non-benign `Verdict::UnhandledThrowsSlip` printed with the seed +
repro, exactly like today's suspicious seeds — the batch is a diagnostic (never asserts
counts), so the slip surfaces in the divergent list and via minimize. Do NOT silently fold it
into GEN-INVALID (that bucket means "generator bug," the opposite conclusion).

**Q5 — PASS discipline: assert stderr cites `E_UnhandledThrows`/`throws` AND no `found
`Result[` leak?** **Recommend: yes** — mirror `check_gg_fails_no_desugar`. A bare "check
failed" PASS would green a program that fails for an UNRELATED reason (a generator bug), and
would miss a desugar-leak regression. Requiring the `throws` signal + no-leak makes the PASS
specifically about D23.

**Q6 — Seed range / determinism for the tier.** **Recommend:** same `GG_SMITH_SEEDS` sweep
(e.g. `1..500`); it's fast (check-only, no build/run/ggdef, no ~57s driver build needed for
this tier — consider skipping `driver_paths()` when the tier is reject-only). Add the always-
on determinism guard (3d).

**Q7 — Follow-up (OUT of T3b scope, worth filing): a POSITIVE throws differential tier
(T3c?).** A tier that generates HANDLED throws programs (auto-prop in a `throws` fn, `catch`,
`Result` capture) and runs the FULL differential (production vs ggdef vs C vs self-host vs
LLVM) would catch a backend MISCOMPILING the throws→Result desugar at runtime — arguably
higher value than the reject-regression, and ggdef already models the handled paths
(`Propagate`, §10.3 capture, `elaborate/mod.rs`). **Recommend: file as a separate track**;
do not expand T3b.

---

## Appendix — commands run (all in the worktree; read-only)
- `cargo build --bin gg`; `cargo build -p ggdef --bin ggdef` — both OK.
- `./target/debug/gg check <pos>.gg` for binop/stmt/scrut/method2/nested → all exit 1
  `E_UnhandledThrows`; malformed bare-`self` method → exit 0 (false-positive lesson).
- `./target/debug/ggdef run <pos>.gg` → all exit 2 (ElabError, elaboration-time D23 reject).
- `cargo test --test smith generator_determinism` → ok (smith crate builds on this base).
- greps: `grep -c "throws" generator.rs` = 0 (tier-0 emits no throws); is_benign/gen_invalid
  chain (`main.rs:312-314,743,893,923`); `maybe_wrap_throws_call` (`elaborate/mod.rs:1358-67`);
  `FrontendError::Elaborate → GgdefOracle::Skip` (`main.rs:565-567`).
