# SPEC — Self-host RUNTIME parity validation harness

**Status:** spec for review (≥3 fresh passes) → build next session / via background agent.
**Owner decision (2026-05-31):** runtime parity is the PRIMARY north-star metric; fn-count
`c_emit_comparison` is demoted to a SECONDARY structural diagnostic that overstates correctness.

## 1. Why (the blind spot)

Every existing self-host validation measures STRUCTURE, not BEHAVIOR:
- `c_emit_comparison` (849/1029 "matched") compares emitted-C **function counts** — a body-level
  miscompile (right count, wrong code) is invisible. The `~`-operator gap (`lower.gg` EUnaryOp silently
  dropped `~`, found 2026-05-31, fixed in `1289a7d7`) sat in this blind spot until a 2nd-order drop_elab
  double-free surfaced it.
- `fixed_point` only exercises features the self-host's OWN source uses.
- lexer/parser/resolver/type/check comparisons check diagnostic OUTPUT — they stop before codegen.

A throwaway bash diagnostic (compile the self-host's emitted C + run, diff vs Rust gg) over ~116 feature
fixtures found only **~10–25% produce CORRECT output** (vs 83% fn-count "match"), with **~30% RUN-BUT-WRONG**
(silent miscompiles: `bounds_check`, `char_methods`, `closures`, `auto_types`, `bare_tuples`, arena/async/
borrow families). The `~` bug was representative of a LARGE invisible class. **fn-counting cannot see a wrong
body. Runtime parity can.**

⚠ Those bash numbers are NOISY — see §4 (the preamble-splice is the key design risk). The proper harness
gives clean numbers.

## 2. Goal

A test that runs EVERY (eligible) fixture through the SELF-HOST compiler — compile its emitted C, run the
binary — and confirms the runtime output matches Rust gg (the oracle). Two mechanisms:
- **Diagnostic** (`self_host_runtime_diff`, on-demand/gated): report MATCH / WRONG-OUTPUT / CC-FAIL counts +
  per-fixture lists. Diagnostic-always-pass (read the count). This is the honest runtime-parity number.
- **Lock-in regression net** (`self_host_runtime`, build-breaking): assert the PASSING SET still matches;
  a fixture that regresses FAILS the build. The set only grows (add fixtures as gaps are fixed).

The WRONG-OUTPUT + CC-FAIL lists become the gap backlog — the real parity work for future 1:1:1:1 rounds.

## 3. The mechanism (reuse `self_host_bootstrap`'s proven path)

`self_host_bootstrap` (integration.rs:13739) already compiles-and-runs self-host-emitted C:
1. Build the self-host driver (cached): `build_gg_dir_cached("self_host_lowerer", "driver.gg")` → (driver
   exe, driver.c). The Rust-compiled `driver.c` carries the full runtime preamble.
2. Self-host emits a C BODY (NOT a full program): `driver FIXTURE lib --lir-c` → body C.
3. Splice a runtime preamble + the body → full C → `cc -O0 -w -o bin full.c -lm -lpthread` → binary.
4. Run the binary → stdout.

Oracle = Rust gg's output for the same fixture (`target/debug/gg run FIXTURE`, OR the existing `run_gg`
expected strings, OR a generated snapshot — see §6).

## 4. ⚠ THE KEY DESIGN RISK — the runtime preamble splice (the builder MUST resolve this first)

The self-host `--lir-c` is a body that needs a preamble. The bash diagnostic proved the splice is FRAGILE:
- Full `driver.gg` preamble (4694 lines, most complete runtime) for ALL fixtures: 28 MATCH / 37 WRONG / 51
  CC-FAIL over ~116.
- Per-fixture Rust preamble (`gg build --emit-c-lir FIXTURE`, cut at the first `\ntypedef struct __gg_`,
  leaner): 12 MATCH / 27 WRONG / **77 CC-FAIL** — the lean preamble misses runtime the self-host body calls
  → MORE cc-fails. So a wrong/incomplete preamble FABRICATES cc-fails that are not real self-host gaps.

**The builder must FIRST determine the reliable splice, in this order of preference:**
1. **Does the self-host emit a FULL self-contained program?** Check for a `--emit-c` / full-program mode (vs
   `--lir-c` body-only). If yes, compile it DIRECTLY (no splice) — cleanest, no preamble risk. (Likely NO,
   since `self_host_bootstrap` splices — but verify; the answer determines everything.)
2. **If body-only:** the preamble must be the SUPERSET runtime (the full `driver.gg` preamble) so no runtime
   symbol is missing, AND the boundary must align (the self-host body's user-typedefs must follow exactly
   what the preamble ends with). Validate by: for a fixture, does `Rust-preamble + Rust-body` compile+run
   (it must — it's the oracle)? Then `Rust-preamble + self-host-body` isolates the self-host CODEGEN. If
   that cc-fails, the self-host body is the only difference → a REAL gap (NOT a preamble artifact) — PROVIDED
   the preamble is the superset. Use the FULL driver preamble (not the lean per-fixture one) to avoid
   fabricated cc-fails.
3. **Distinguish real CC-FAIL from artifact:** a CC-FAIL is a REAL self-host gap iff Rust's full C for the
   SAME fixture compiles+runs (it does — oracle) AND the only swapped component is the body. With the
   superset preamble, every CC-FAIL is real. If using a per-fixture preamble, a missing-symbol cc-fail is
   ambiguous — DON'T use the lean preamble.

The builder should land on a splice where `Rust-preamble + Rust-body` ≈ the oracle (sanity check: it
compiles+runs == `gg run`), then swap in the self-host body. Document the chosen approach + WHY.

## 5. Categorization (the diagnostic's output)

Per eligible fixture:
- **MATCH** — self-host binary stdout == Rust stdout. (runtime-parity ✓)
- **WRONG-OUTPUT** — self-host runs, stdout != Rust → REAL silent miscompile (highest-value gap class).
- **CC-FAIL** — self-host body won't compile with the superset preamble → REAL gap (emits invalid/incomplete
  C). [Only real if §4's superset-preamble rule holds.]
- **SH-EMIT-FAIL / SH-TIMEOUT** — self-host crashes emitting / binary hangs → real gap (rare; c_emit shows 0
  self-host crashes).
- **EXCLUDED** — Rust gg rejects the fixture (error/`*_error.gg` fixtures), or it's non-deterministic
  (random/time/network: httpserver, p2p, random, async-sleep), or stress/bench/platform (metal/gl). The
  diagnostic auto-excludes via RUST-FAIL + a name/exclusion list. Document the exclusion list + reasons.

## 6. Oracle & snapshot (cost control)

Running `gg run` (full Rust build) per fixture every test run is expensive. Strategy:
- **Diagnostic** (on-demand): may run the Rust oracle live for the full, current picture.
- **Lock-in net** (every-build): assert against a SNAPSHOT of expected outputs (Rust-gg-generated, committed
  once). The self-host output must == snapshot. A `GG_REGEN_RUNTIME_SNAPSHOT=1` mode regenerates it (run Rust
  oracle over the passing set). The snapshot file (e.g. `tests/fixtures/self_host_runtime_expected.txt` or a
  dir) IS the locked passing set + expected outputs. New-passing fixtures → regenerate to add them.
- Reuse existing `run_gg` expected strings where present (already validated) to seed the snapshot.

## 7. The `gg test` framework angle (secondary)

18 fixtures carry `Item::Test` blocks (`loader.rs:246`). For those, ADDITIONALLY run their test assertions
via the self-host (compile + run the test-runner main). Lower priority — the stdout-comparison covers the
~1100 print-based fixtures. Note: the self-host's test-runner-main synthesis is a known gap (FIDELITY scout's
TEST_BLOCK class, 13 fixtures) — folding `gg test` in may require closing that first.

## 8. Cost management

~1121 fixtures × (cc ~1-2s + run) is heavy. Mitigations:
- Parallelize via `parallel_map_fixtures` (already in integration.rs).
- Gate the FULL diagnostic behind an env var (`GG_RUNTIME_DIFF=1`) — on-demand, not every `cargo test`.
- The lock-in net runs ONLY the passing set (smaller, bounded) — but still parallelize + use a generous
  timeout like `fixed_point` (`GG_*_TIMEOUT_SECS`).
- The cached driver + a single superset-preamble extraction (once) amortize.

## 9. Deliverable (what the builder produces)

1. `self_host_runtime_diff` — the diagnostic test (gated): MATCH/WRONG-OUTPUT/CC-FAIL counts + lists; prints
   the runtime-parity %. The honest north-star number.
2. `self_host_runtime` — the lock-in regression net: asserts the passing-set snapshot still matches; FAILS
   the build on a regression. Parallelized + timeout-gated.
3. The passing-set snapshot (committed) + a `GG_REGEN_RUNTIME_SNAPSHOT` regenerator.
4. The chosen preamble-splice approach, documented + validated (§4).
5. The clean runtime-parity number recorded to MEMORY/DONE; the WRONG-OUTPUT + CC-FAIL backlog logged to
   TODO (categorized by feature family) as the parity work for future rounds.

## 10. First-milestone acceptance

- The diagnostic runs clean (splice resolved per §4 — `Rust-preamble + Rust-body` sanity-checks == oracle).
- A trustworthy runtime-parity number (not the noisy bash estimate).
- The lock-in net is green on the passing set + would FAIL if a passing fixture regresses (prove it: revert
  the `~` fix in a scratch tree → `bitwise_ops` must turn WRONG-OUTPUT/fail).
- The backlog is logged. Then future 1:1:1:1 FIDELITY rounds fix WRONG-OUTPUT gaps, each growing the set.

## 11. Open questions for the reviewers

- Is there a self-host full-program emit mode (§4.1)? If yes, the whole splice risk evaporates.
- Is the superset-preamble (full driver.gg) genuinely sufficient for ALL fixtures' runtime needs, or do some
  fixtures need runtime driver.gg doesn't pull in (→ those need a different preamble or are excluded)?
- Snapshot vs live-oracle for the lock-in net — which is less brittle as fixtures evolve?
- Exclusion list completeness (non-deterministic fixtures that would flake).
