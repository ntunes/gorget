# RV-G Scout Report — gen⇄parse frontmatter biconditional break

**Zone:** `spec/ggdef/` + `tests/spec_conformance.rs` (Rust, not bootstrap-gated).
**Status:** brick reproduced end-to-end; design-derived fix prototyped, proven, all gates green.
**Patch:** `/tmp/recover_rvg_proto.patch` (2 files, +124 lines). **Report:** this file.

---

## 1. The brick — MEASURED end-to-end

**Root cause (confirmed at source):** the `gen` WRITER and the frontmatter READER
disagree on codeless exit-1 blocks.

- **Writer** `render_expect_block` (`spec/ggdef/src/lib.rs:148-153`): for
  `Outcome::IllFormed(_)`, `reject = run.reject_code`. An **eval-internal**
  IllFormed carries `reject_code = None` (`eval.rs:366`, `:396`), so the block is
  `#   exit: 1` + `#   stdout: ""` with **NO `#   reject:` line**.
- **Reader** `parse_frontmatter` (`frontmatter.rs:267-274`, `RejectExitWithoutCode`):
  the ratified biconditional hard-fails ANY `exit: 1` block lacking a reject code.
- **Harness** `run_lane` (`tests/spec_conformance.rs:582-624`) collects that as a
  `frontmatter_errs` entry and `assert!(frontmatter_errs.is_empty(), …)` fires —
  **independent of the floor escape hatch** (`GG_PARITY_FLOOR_OFF` does not save
  it). The **ggdef lane has the identical assert** (`spec_conformance_ggdef.rs:156-161`).
  Every lane reads the SAME `spectests/run/*.gg`, so **one bad seed → all four lanes red.**

**Shape 1 — no `main` (measured):** `ggdef gen no_main.gg` **exits 0** (thinks it
succeeded) and writes:
```
#   exit: 1
#   stdout: ""      ← no reject: line
```
`parse_frontmatter` on it → `` `expect.exit: 1` is a static rejection but no
`expect.reject:` `E_` code is declared `` (RejectExitWithoutCode).

**Shape 2 — defense-in-depth move (measured, confirms RV-H's hole):**
`while consume(!x):` with `bool consume(String !s)`. `ggdef run` prints
`error: read of moved-out value \`x\`` (note: **`error:` with NO `error[E_…]`
bracket** — the codeless path, `main.rs:163`), exit 1. The static gate
(`liveness.rs:508-511`) checks `check_expr(cond)` OUTSIDE `check_loop_body`, so it
misses the condition move; eval catches it defensively as a codeless IllFormed.
`gen` writes the same unparseable `exit: 1`/no-`reject:` block → same hard-fail.

> The underlying gate hole is **RV-H's** track — NOT touched here. RV-G only cares
> that eval's codeless IllFormed produces an unparseable block.

---

## 2. Design grounding

- **The verdict triple** (decisions.md:472-484, RATIFIED 2026-07-15): a static
  rejection is stdout `""` (eval channel — never ran) / stderr `error[E_Code]: … at
  span` (elaborate channel) / **exit 1**. **Conformance compares the `E_` code +
  exit CLASS only** (pin 3).
- **`verdict = elaborate ∘ eval`** (decisions.md:436-468, RATIFIED): **ELABORATE
  OWNS EVERY RATIFIED STATIC REJECTION** and emits its `E_` code; **eval's own
  IllFormed is defense-in-depth** (RFC §2.3) and carries **no ratified code** (the
  tests pin this — `tests.rs:388` "eval-internal IllFormed carries no ratified
  code"). So a codeless IllFormed is, by design, **not a ratified conformance
  outcome** the four production lanes can reproduce.
- **Exit-code scheme (Option A)** (decisions.md:485-506, RATIFIED): `0 · 1 static
  rejection (ONE class) · 2 usage · 101 trap+ICE · 103 fuel (ggdef-only)`. 102 was
  RETIRED. **The scheme reserves nothing for "eval-internal IllFormed"** — it
  deliberately folded all static rejection into 1.

The fix MUST preserve the biconditional for STATIC rejections (exit 1 ⟺ reject
code). The question the mandate poses: *what should a codeless EVAL-internal
IllFormed be?* Answer from the design: **not a generatable fixture at all.**

---

## 3. Candidate shapes — trade-off table

| # | Candidate | Verdict-triple fit | 4-lane soundness | Keeps RV-H VISIBLE | Surface cost | Recommend |
|---|-----------|--------------------|------------------|--------------------|--------------|-----------|
| **(a)** | Give eval-internal IllFormed its own `E_` code(s) | **Violates** — `E_` codes are for *ratified* rejections elaborate emits; production emits none of these | **Unsound** — a fake code the production lanes never produce | **No** — papers RV-H into a "legit-looking" reject | +registry churn | ✗ |
| **(b)** | **gen REFUSES + reports codeless-exit-1 as ungeneratable** | **Preserved** — no fixture is ever emitted that violates it | **Sound** — only elaborate-owned coded rejects (all 4 lanes reproduce) ever enter the corpus | **YES** — loud refusal points at the missing gate | +1 GenError variant | ✅ |
| **(c)** | Distinct exit code for eval-internal IllFormed | Re-fragments the just-unified taxonomy | **Unsound** — production has no such code; can't compare | Partial | re-opens ratified scheme | ✗ |
| **(d)** | Explicit `illformed:` frontmatter marker the reader accepts | Adds a 4th channel the COMPARED set has no room for | **Unsound** — enables committing fixtures with no cross-lane meaning (shape-2 would sit permanently red on RV-H rather than be fixed) | Weakly (permanent red, not a fix) | +frontmatter surface | ✗ |

**Why (b):** it is the exact analogue of the existing `migrate` `NonzeroExit`
tripwire (`lib.rs:283-289`) — the tool STOPS and reports rather than writing a seed
that would silently activate a hazard downstream. It makes writer⇄reader agree by
construction (gen never emits what parse refuses), preserves the ratified triple and
exit-code scheme **unchanged** (no new codes, no new markers), keeps the corpus sound
for all four lanes, and — critically — keeps the RV-H-class hole **VISIBLE** (an
author trying to add such a fixture gets a message naming the two causes: no `main`,
or a gate that missed a move).

*(Production-lane note for the record: for shape 1, production `gg` has no consistent
ratified `E_`-coded outcome for a run-tier fixture with no `main`; for shape 2,
production RUNS the program (RV-H) → garbage/nonzero, no consistent exit. Neither is
a comparable outcome — which is exactly why refusing to generate them is correct.)*

---

## 4. The prototype (proven)

`spec/ggdef/src/lib.rs`:
- New `GenError::CodelessIllFormed(String)` variant + a full Display (names both
  causes and the consequence, tells the author to fix the program or the gate).
- Guard in `gen_frontmatter` (`lib.rs`): after `run_source`, if `outcome` is
  `IllFormed` AND `reject_code.is_none()` → `Err(GenError::CodelessIllFormed(msg))`,
  **before** rendering/splicing. The migrate path (`MigrateError::Gen`) wraps it
  unchanged — belt-and-suspenders with the existing `NonzeroExit` tripwire.

`spec/ggdef/src/tests.rs`:
- **`gen_output_always_parses_round_trip`** — the Core #6 guard. For each
  GENERATABLE outcome (Value / Trap+T_ / elaborate-reject+E_ / FuelExhausted@103) it
  frames a representative program, runs `gen_frontmatter`, and asserts
  `parse_frontmatter` accepts the output. The two CODELESS cases (no `main`;
  while-move defense-in-depth) assert `gen` REFUSES with `CodelessIllFormed`. The
  while-move arm is **RV-H-independent**: if a future gate closes the hole and
  elaborate rejects WITH a code, the arm accepts `Ok` too, as long as the block
  parses — the invariant under test is purely "gen never emits an unparseable block."

**Measured evidence:**
- `cargo test -p ggdef --lib` → **140 passed / 0 failed** (was 139 + the new one).
- `ggdef gen no_main.gg` → refuses, **exit 2, file UNCHANGED** (no broken block written).
- `ggdef gen while_move.gg` → refuses with `(read of moved-out value \`x\`)`.
- `ggdef gen good.gg` (a Value fixture) → still succeeds, block parses (`exit=0`).
- `cargo test -p ggdef --test gen_idempotent --test converter_agreement` → both green.
- `cargo test --test spec_conformance --no-run` → compiles clean.

---

## 5. Blast radius

- **Committed corpus: ZERO.** All 202 `spectests/run/*.gg` seeds pass
  `gen_idempotent` (re-gens every ggdef seed) → none is a codeless IllFormed today.
  The 6 `exit: 1` seeds each carry a proper reject code
  (`reject_use_after_move` / `_branch` / `reject_double_move` /
  `reject_consume_callable_double` E_DoubleMove, `reject_consuming_self_use_after_move`
  E_UseAfterMove, `reject_move_in_loop` E_MoveInLoop). Nothing re-generates differently.
- **Behavior change** is confined to *previously-unparseable* gen output: those cases
  now fail LOUD at gen time instead of silently poisoning the suite later.
- **`migrate`**: only touches `Agree`/exit-0 fixtures; a codeless IllFormed is
  classified `NotValue`/`FrontendError` and skipped. If one ever reached gen, the new
  guard makes migrate stop loudly — strictly better than before. No regression.

---

## 6. Executor plan + gates

1. Apply `/tmp/recover_rvg_proto.patch` (`spec/ggdef/src/lib.rs`, `spec/ggdef/src/tests.rs`).
2. Stage explicitly: `git add spec/ggdef/src/lib.rs spec/ggdef/src/tests.rs`.
3. Gates (all foreground, ggdef-scoped — no full sweep needed):
   - `cargo test -p ggdef --lib` (expect 140/0; includes the new round-trip guard).
   - `cargo test -p ggdef --test gen_idempotent --test converter_agreement` (both green).
   - `cargo test --test spec_conformance --no-run` (compiles).
   - Optional sanity: `cargo test -p ggdef --test spec_conformance_ggdef -- --nocapture`
     (ggdef lane still green over the committed corpus).
4. Commit. This is the cheap, suite-protecting track — land it FIRST (before the
   RV-A/B/C production-miscompile tracks), per the RV-G "do FIRST" sequencing.

**Not in scope (leave for RV-H):** the `liveness.rs:508-511` while-condition gate
hole. RV-G deliberately does not fix it — it makes gen refuse the codeless block the
hole (and no-`main`) produces, keeping the hole visible until RV-H closes it.

---

## 7. New pre-existing findings (file-don't-fix)

- **(minor, non-blocking)** `cmd_gen`'s refusal path returns `EXIT_USAGE` (2) for a
  `CodelessIllFormed` (via the catch-all `gen_frontmatter` error arm, `main.rs:82-88`).
  That is defensible (gen was handed an ungeneratable input = a usage-class error) and
  needs no change, but if a future distinction is wanted, that is where it would go.
  Noting for completeness, not filing as a bug.
- No other new defect surfaced. RV-H (the gate hole exercised by shape 2) is already
  filed; this scout only confirmed it reproduces, it did not touch it.
