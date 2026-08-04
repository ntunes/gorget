# Profiling Pass #1 — recommendations

> **Note on threshold labelling**: the "≥3× faster than Python on every bench" and "within 2× of Rust on numerical benches" targets called out below are **orchestrator-stated operational targets**, NOT thresholds documented in the codebase. Treat them as the user's working perf goal for this branch, not as a regression contract.

> **Branch context**: gorget-1 tip = `be281480` (STEP 2.4b landed). Profiles run on a release build of the Rust gg compiler in this worktree (a Linux aarch64 environment); raw JSONs were captured by re-running each workload 3× and saving the second (median) run.

> **System noise caveat**: brief-12 and brief-13 are running in parallel worktrees on the same box. Absolute timings in Axis A may be 5–15 % noisier than a quiet machine, but the cross-workload **ratios** (per-instruction cost, super-linear scaling, dominant-phase identification) are unaffected.

## Summary

The dominant compile-time hotspot at the scale we ship the self-host is **LIR `drop_elaboration`** (29.7 % of `self_host_lowerer/driver.gg` compile, 133 ms of 759 ms, **5.6× per-instruction cost-inflation** from yaml_parse → lowerer), closely followed by **codegen** (132 ms / 17.5 %, 600 ns/inst on the lowerer vs 306 ns on yaml_parse). Runtime self-host memory has **regressed catastrophically** vs phase 3d's "THE WIN" baseline — peak RSS up 4.4× (270 MB → 1184 MB), `array_clone` up **203×** (7.4M → 1.5B), total_allocs up 54× — putting us roughly back to phase1 levels (the pre-CoW starting point). Runtime perf is uneven: numerical-heavy compute benches clear the ≥3× Python target with 3–7× margins, but **Dict/Set benches are 2–2.5× SLOWER than Python** and several string-heavy loops are 3–4× slower than Python; vs Rust the compute benches are 5–14× slower (~ 7× typical) — meaningfully below the within-2× operational target.

## Axis A — Compile-time hotspots

### Per-workload phase breakdown (median run)

| workload                   | src_lines | total_ms | peak_rss_kb | parse % | semantic % | gir_lower % | gir_opt % | lir_lower % | lir_ssa % | **lir_opt %** | codegen % |
|----------------------------|-----------|---------:|------------:|--------:|-----------:|------------:|----------:|------------:|----------:|--------------:|----------:|
| self_host_lowerer/driver   | 35349*    |    759.1 |     196 560 |     0.1 |        8.8 |        15.0 |       3.8 |         5.9 |       9.2 |      **29.7** |      17.5 |
| self_host_typechecker/drv  | 12073*    |    187.7 |      72 180 |     0.5 |       11.1 |        17.8 |       5.0 |         8.8 |       7.5 |      **21.3** |      15.2 |
| self_host_parser/driver    |  6022*    |     83.8 |      41 420 |     0.9 |       12.9 |        18.5 |       4.9 |        10.1 |       6.6 |      **16.8** |      14.1 |
| yaml_parse                 |   453     |     39.0 |      25 884 |     3.0 |        9.0 |        17.7 |       5.0 |        12.8 |       8.7 |      **17.5** |      14.6 |
| test_vector_all            |   240     |      8.3 |      11 932 |    14.7 |       10.3 |        15.7 |       2.7 |        26.2 |       4.1 |           8.4 |      11.3 |
| bench_basic                |     5     |      4.0 |       9 980 |    14.0 |       15.8 |        10.5 |       0.3 |        46.9 |       0.0 |           1.4 |       6.1 |
| bench_collections          |    86     |      4.0 |      10 140 |    20.8 |       14.2 |         9.3 |       0.0 |        43.9 |       0.0 |           1.3 |       5.5 |
| bench_compute              |    82     |      4.5 |      10 852 |    16.1 |       15.9 |        13.4 |       0.3 |        41.7 |       2.5 |           1.5 |       3.7 |
| bench_string               |   101     |      4.3 |      10 192 |    21.8 |       15.5 |        10.0 |       0.0 |        41.0 |       0.0 |           1.4 |       5.2 |
| bench_string_methods       |    62     |      4.2 |      10 192 |    19.8 |       14.4 |        10.8 |       0.0 |        43.3 |       0.0 |           1.2 |       5.5 |

\*self_host driver entry files are tiny (54–114 lines); the real workload size is the imported modules. Imported totals: lowerer 35 349 lines (28 files), typechecker 12 073 (16 files), parser 6 022 (7 files).

### Cross-workload scaling

| workload (sorted by lines)  | src_lines | lir_fns | lir_insts | total_ms | ms/src_line | lir_opt_ms | **lir_opt_ns/inst** | codegen_ms | **codegen_ns/inst** | drop_elab_ms |
|-----------------------------|----------:|--------:|----------:|---------:|------------:|-----------:|--------------------:|-----------:|--------------------:|-------------:|
| yaml_parse                  |       453 |      92 |    18 610 |     39.0 |        86.1 |        6.8 |                 365 |        5.7 |                 306 |          2.0 |
| self_host_parser            |     6 022 |     180 |    36 865 |     83.8 |        13.9 |       14.1 |                 382 |       11.8 |                 320 |          4.5 |
| self_host_typechecker       |    12 073 |     350 |    76 460 |    187.7 |        15.5 |       39.9 |                 522 |       28.5 |                 372 |         15.6 |
| self_host_lowerer           |    35 349 |     810 |   220 069 |    759.1 |        21.5 |      225.6 |              **1025** |      132.5 |              **602** |        **133** |

**Two clear super-linear signals**:

1. `lir_opt_ns/inst` 365 → 1025 (**2.8× per-instruction cost inflation** going small → big). `drop_elaboration` alone is even worse: 109 → 204 → 606 ns/inst (**5.6× inflation**). This is structural — not just "more work for more instructions". A per-function dataflow that is O(n_slots × n_blocks × #iter) becomes super-linear when individual functions get large (the lowerer has a few notorious match-heavy functions: `lower_call`, `lower_expr`, `lower_op_consume`, etc.).
2. `codegen_ns/inst` 306 → 602 (**~2× per-instruction codegen inflation**). Likely similar structural shape — large functions with many slots / many basic blocks emit C with more per-block overhead.

### Dominant sub-passes (self_host_lowerer, the worst case)

- **`lir_optimize` (225.6 ms / 29.7 % of total)**: `drop_elaboration` 133.3 ms (**59 %** of the phase), `eliminate_dead_code` 14.9, `propagate_copies` 14.1, `fold_constants` 7.5, `post_elab_dce` 6.9. drop_elab dominates by 9× over the next sub-pass.
- **`codegen` (132.5 ms / 17.5 %)**: no sub-pass breakdown in the profile JSON; structural inspection (§ Top hotspots § 2) suggests per-function block iteration is the likely shape.
- **`gir_lower` (113.8 ms / 15.0 %)**: `lower_functions` 56.4 (49.6 % of phase), `lower_function::body::lower_block` 46.1 (40.5 %), `validate_consume_sites` 18.6 (16.4 %). lower_block is the AST→GIR walk.
- **`semantic` (66.5 ms / 8.8 %)**: nearly even split — `safety_check_module` 20.7, `typecheck_module` 19.7, `meta_consts` 18.3. No single dominant sub-pass — these three could each be ~25 % wins separately but no individual one carries the phase.

### Outliers

- **`lir_lower` % is huge (40-47 %) on the bench fixtures** but those workloads have total_ms ≤ 4 ms — dominated by fixed per-process / per-module setup costs (LIR type prescan, etc.). Not a real hotspot signal.
- **drop_elab takes 133 ms / 18 % of TOTAL compile time** on the lowerer. At our scale that is the single biggest fraction of compile time spent in one named sub-pass.

## Axis B — Self-host memory regression vs phase 3d

**VERDICT: catastrophically regressed.** Captured fresh snapshot at `be281480` (`/tmp/profile-pass1-selfhost-mem.json`) and compared to `scripts/baselines/phase3d_case_c_active.json`:

| metric            | phase 3d (2026-04-22) | now (be281480, 2026-05-28) | delta            |
|-------------------|----------------------:|---------------------------:|-----------------:|
| peak_rss_kb       |               270 552 |                  1 183 852 | **+337.6 %** (4.4× bigger) |
| array_clone       |             7 388 593 |              1 505 962 203 | **+20 282 %** (203× more)  |
| array_new         |             7 779 112 |              1 508 301 253 | **+19 289 %** (194× more)  |
| box_alloc         |               747 742 |                  2 750 295 | +267.8 %                   |
| string_cat        |             2 537 696 |                  4 112 129 | +62 %                      |
| string_cow        |               825 102 |                     95 275 | −88.5 % (interesting — see below) |
| total_allocs      |            21 331 969 |              1 165 155 658 | **+5 362 %** (54.6× more)  |
| live_bytes        |           186 859 368 |                844 708 942 | +352 %                     |

Holy shit. We're 75 % of the way back to phase 1 levels (the pre-CoW baseline was 4 GB RSS / 1.3B clones / 800M allocs).

**Important sanity check**: the workload `scripts/self_host_mem_baseline.sh` runs is the **Rust-built `gg` compiling the self-host driver.gg on its own source** (`stage-0`, `--lir-c`). So the regression is in **what the Rust gg compiler emits for the self-host Gorget code** — runtime growth of code produced by `src/`. (The self-host's own runtime-clone counts are not what's being measured here.) That means the regressed phase is somewhere in the Rust compiler's ownership/CoW emission path, not in the self-host's `lower.gg`.

**Cluster of possible causes** (`git log --oneline --since="2026-04-22" -- src/ir/lowering` yields **307 commits** since phase 3d — far too many to bisect by hand from this brief alone):

- The `string_cow` count is DOWN 88.5 %, which is the *opposite* direction of every other counter. Phase 3d was characterized by trading eager clones for cow-borrow + materialize-on-mutation; if a recent commit gated off the materialize step but kept the eager-clone-on-borrow path, the cow count drops AND the clone count explodes. Worth a focused look at recent edits in `src/ir/lowering/cow*` / `src/ir/tag_ownership.rs` / `src/ir/abi.rs` / `src/semantic/safety/`.
- Multiple recent commits visible in the log explicitly fix clone-related cases (`gate consuming-position clone on ByValue ABI, not method name`, `clone module-level resource globals at ownership boundaries`, `materialize view-bound locals at StructLiteral arg boundary`, `bundle of 4 follow-ups (… + var_decl coalesce + …)` — landing during a perf-instrumentation push). Each individually plausible; cumulatively the array_clone explosion suggests a systemic re-introduction of an eager-clone path that phase 3d had elided.

**This is the highest-leverage finding of the pass and should be the next chain.** Per the brief's "don't propose fixes, identify and hypothesize" — proposed chain outline appears in § Top hotspots § 1.

## Axis C — Runtime perf vs Python (and Rust where attempted)

### bench_compute — gorget vs python

| bench                          | gorget ns/iter | python ns/iter | gorget × faster | clears ≥3×? |
|--------------------------------|---------------:|---------------:|----------------:|:-----------:|
| fib(20) recursive              |         62 280 |        464 793 |          7.46×  | ✓           |
| sum range 1000                 |          2 800 |         16 685 |          5.96×  | ✓           |
| for range 0..100               |            254 |          1 119 |          4.41×  | ✓           |
| Vec2 dot product x100          |          1 110 |          3 652 |          3.29×  | ✓           |
| while loop 10000               |         24 930 |        162 111 |          6.50×  | ✓           |
| nested loop 100x100            |         34 450 |        102 314 |          2.97×  | **FAIL** (close to 3× but under) |

### bench_collections — gorget vs python

| bench                              | gorget ns/iter | python ns/iter | gorget × faster | clears ≥3×? |
|------------------------------------|---------------:|---------------:|----------------:|:-----------:|
| Vector[int] push x1000             |          5 140 |         14 235 |          2.77×  | **FAIL**    |
| Vector[int] sum via for            |          1 090 |          2 547 |          2.34×  | **FAIL**    |
| Vector[int] index access x100      |          1 030 |          3 736 |          3.63×  | ✓           |
| **Dict[String,int] build+lookup x50** |       18 450 |          8 833 |        **0.48×**  | **FAIL — 2.1× SLOWER** |
| **Dict[String,int] iterate keys** |         12 500 |          4 903 |        **0.39×**  | **FAIL — 2.5× SLOWER** |
| **Set[int] add+contains x100**     |          9 440 |          4 118 |        **0.44×**  | **FAIL — 2.3× SLOWER** |
| **Set[String] add+contains x50**   |         16 540 |          8 588 |        **0.52×**  | **FAIL — 1.9× SLOWER** |

### bench_string — gorget vs python

| bench                              | gorget ns/iter | python ns/iter | gorget × faster | clears ≥3×? |
|------------------------------------|---------------:|---------------:|----------------:|:-----------:|
| string literal assign              |              4 |             19 |          4.75×  | ✓           |
| string len access                  |             19 |             24 |          1.26×  | **FAIL**    |
| **string len x32 loop**            |          2 890 |            673 |        **0.23×**  | **FAIL — 4.3× SLOWER** |
| string copy named-to-named         |              4 |             21 |          5.25×  | ✓           |
| **string concat**                  |             87 |             53 |        **0.61×**  | **FAIL — 1.6× slower** |
| **Vector[String] push fstring x100** |       9 600 |          4 496 |        **0.47×**  | **FAIL — 2.1× slower** |
| **Vector[String] iter+sum len**    |         11 640 |          5 872 |        **0.50×**  | **FAIL — 2.0× slower** |
| **Dict[Str,int] put fstring key x50** |     10 150 |          3 256 |        **0.32×**  | **FAIL — 3.1× slower** |
| **string slice hot loop**          |          2 970 |            966 |        **0.33×**  | **FAIL — 3.1× slower** |

### bench_basic — too trivial to be meaningful

`bench "addition"` (`int x = 1 + 2`) clocks 1 ns/iter, `bench "string concat"` (`String s = "hello" + " world"`) clocks 30 ns/iter. Both compile-time computable; both report 100M+ iters/run. **Flag as not useful for cross-impl comparison** — even bench_basic's runtime bench loop overhead dominates. Recommend retiring or growing into a real workload.

### bench_compute — gorget vs Rust (numerical-heavy track)

Quick Rust crate in `/tmp/profile-pass1-rs-equiv/bench_compute/`; `#[inline(never)]` and `black_box(...)` used to prevent loop deletion. (One iter `sum range 1000` got saturated to ~9 ns and is dropped from the comparison — the auto-iter-scale loop saturated above 1 G iters; the rest of the table is sound.)

| bench                       | gorget ns/iter | rust ns/iter | gorget / rust | clears within-2×? |
|-----------------------------|---------------:|-------------:|--------------:|:------------------:|
| fib(20) recursive           |         62 280 |       11 129 |      **5.60×** | **FAIL**           |
| for range 0..100            |            254 |           34 |      **7.47×** | **FAIL**           |
| Vec2 dot product x100       |          1 110 |           81 |     **13.70×** | **FAIL**           |
| while loop 10000            |         24 930 |        3 347 |      **7.45×** | **FAIL**           |
| nested loop 100x100         |         34 450 |        3 583 |      **9.61×** | **FAIL**           |

**Every compute bench is 5–14× slower than Rust.** The Vec2 dot product result is the worst (13.7×) — that hints at struct-by-value ABI overhead in the closure-/match-heavy lowering vs Rust's `#[derive(Copy)]` register-passing.

### Verdict (Axis C)

- Compute benches **clear** the ≥3× Python target with margin, except `nested loop 100x100` which scrapes 2.97×.
- **Dict / Set benches are slower than Python by 2–2.5× across the board.** This is *worse than failing the target* — we are net negative against the comparison.
- **Several string-heavy loops are 1.6–4× slower than Python.** `string len x32 loop`, `string slice hot loop`, all f-string + Vector/Dict insert benches all fail badly.
- Compute is **5–14× slower than Rust**; the within-2× operational target is comfortably missed across all 5 surveyed benches.

## Top hotspots (ranked by leverage)

Leverage scoring: `(estimated speedup) × (frequency in real workloads)`. Self-host compile time dominates real work; runtime correctness/memory dominates user-visible "is the language fast".

### 1. **Self-host runtime memory regression: 1.5 GB clone-bomb vs phase 3d's 7 M** ◀ TOP CHAIN
- **Axis**: B (regression check) — strictly orthogonal to Axes A/C.
- **Source location**: somewhere in `src/ir/lowering/` (~307 commits since phase 3d). Suspected upstream writer site: a CoW / ensure_owned_at_boundary path that has lost its "borrow + materialize" plumbing and regressed to "eager clone". The fact that **`string_cow` is DOWN 88.5 %** while **`array_clone` is UP 203×** is the strongest single tell: a materialization callback that *should* fire on borrow-then-mutate is silently no-oping, so the clone path is being taken eagerly instead of being elided.
- **Root-cause hypothesis**: a recent edit gated a CoW-emit on a typed predicate (`is_collection_kind` / `AbiKind == ByValue` / similar) that *was* true at phase 3d but now reads false for the dominant call shape — e.g. all the `c.method(arg)` sites on Vector/Map. The high-confidence candidates (by commit-message + file affinity) are the `gate consuming-position clone on ByValue ABI, not method name` family from late April–early May and the `bundle of 4 follow-ups (var_decl coalesce + ...)` perf series. **At 1.5B clones we should be able to reproduce the regression on a tiny fixture under `cargo run --release -- build … --lir-c` + alloc-report and bisect cheaply.**
- **Proposed chain brief outline**:
  1. **Scope**: identify the commit(s) since `phase3d_case_c_active` (2026-04-22) that doubled-or-more the array_clone or total_alloc count on the self-host driver workload.
  2. **Fix site**: per the layering-discipline heuristic, at the **writer** site (CoW / tag_ownership / ensure_owned_at_boundary in `src/ir/`), not at the consumer.
  3. **Tactic**: `git bisect` between phase3d's recorded git_rev (in `phase3d_case_c_active.json`) and `be281480`, criterion = `scripts/self_host_mem_baseline.sh` `array_clone < 50M`. (~8 bisect steps at 1–3 min/step → an afternoon.)
  4. **Success metric**: peak_rss_kb ≤ 300 MB AND array_clone ≤ 10 M (back within 10 % of phase 3d's baseline) on the snapshot script.
  5. **Verification gates**: `cargo test --lib`, `lowerer_comparison`, `self_host_bootstrap`, then a fresh snapshot delta vs `phase3d_case_c_active.json`.

### 2. **Compile-time: `drop_elaboration` per-instruction cost inflation (5.6×) on large functions**
- **Axis**: A.
- **Source location**: `src/lir/drop_elab.rs` — specifically `forward_dataflow` (workhorse, `:105-176`), `meet_states` (`:239-247`), and `elaborate_drops` (`:712`).
- **Root-cause hypothesis**: per-block worklist dataflow with a dense `Vec<InitState>` of size `n_slots` cloned at every node-pop + every successor edge. Cost shape on a single function: `O(n_blocks × n_slots × #iters_to_fixpoint)`. The lowerer has a handful of functions with many slots (each let-binding gets one) and many basic blocks (deep match nesting), so per-function cost grows worse-than-linearly even when total instruction count grows linearly. Confirmation: `drop_elab_ns/inst` goes 109 → 204 → 606 ns (5.6×) from yaml_parse → typechecker → lowerer; **no other sub-pass has this scaling profile.**
- **Proposed chain brief outline**:
  1. **Scope**: profile-driven micro-optimization of `forward_dataflow` (avoid the per-edge `out.clone()` if the predecessor already saturated; use a 2-bit per-slot bit-vector instead of `Vec<u8>` for cache); confirm fixpoint converges in ≤ 2 iterations on > 95 % of functions and add an early-exit on saturation.
  2. **Fix site**: `src/lir/drop_elab.rs:forward_dataflow` and `meet_states` (writer of `in_states`). Likely change `SlotStates` from `Vec<InitState>` to `[u64; (n_slots + 31) / 32]` packed (8× cache density, branch-free meet).
  3. **Success metric**: drop_elaboration sub-pass on self_host_lowerer drops from 133 ms to ≤ 40 ms (3.3× speedup) → `lir_optimize` total drops to ~ 130 ms → total compile drops ~ 12 %.
  4. **Verification gates**: `cargo test --lib`, full `cargo test --test integration` (Axis A change needs full suite; this is parent-driven not agent-driven per CLAUDE.md), drop-elab byte-output equality on a corpus of fixtures (no behaviour change allowed).

### 3. **Runtime: Dict / Set / String-heavy loops slower than Python (0.23–0.52× ratio)**
- **Axis**: C.
- **Source location**: `src/backend/c/runtime/runtime_map.c:33`+ (`__GORGET_MAP_HASH` macro, `__gorget_map_grow`, `gorget_map_put`, `gorget_map_get`) and the codegen for `.len()` / slicing in hot loops.
- **Root-cause hypothesis** (Dict/Set):
  - Default hash is FNV-1a (`__gorget_fnv1a` `:226`) — **way slower than CPython's SipHash + per-cell optimization** for short keys (CPython's dict hot path is hand-rolled assembly-ish C, beats FNV by ~2×). For non-string keys, the `hash_fn` / `eq_fn` function-pointer indirection (`:5753-5754`) is also unavoidably worse than CPython's specialized int-hash inlining.
  - Linear probing on `% new_cap` (`:5783, :5803`) — `%` on a non-compile-time-known cap is a 20-cycle divide; CPython uses bit-masking on a power-of-two capacity.
  - Every put incurs allocator calls for the order vector (`:5769, :5793`).
- **Root-cause hypothesis** (string-len-in-loop, slice-in-loop): plausibly Gorget is *not* hoisting `s.len()` / `s[a..b]` out of the loop, or emitting a clone of the slice view that goes through the allocator each iter. The hint: `string copy named-to-named` is 4 ns (fast — just a struct copy) but `string len x32 loop` averages 90 ns per inner-iter — 22× the cost of one `.len()` access standalone (`string len access` = 19 ns). Something is being allocated inside the loop body.
- **Proposed chain brief outline (Dict/Set)**:
  1. **Scope**: switch `__gorget_fnv1a` → wyhash or rapidhash for short keys; constrain `m->cap` to power-of-two and replace `% cap` with `& (cap - 1)`; specialize `__gorget_map_put_int` / `_str` to avoid the function-pointer dispatch on the hot path.
  2. **Fix site**: `src/backend/c/c_runtime.rs` (runtime, not compiler — pure C-level work).
  3. **Success metric**: `Dict[String,int] build+lookup x50` from 18.5 µs → ≤ 6 µs (≥ 3× Python); `Set[int] add+contains x100` from 9.4 µs → ≤ 3 µs.
  4. **Verification gates**: full integration suite (Dict/Set used pervasively); compare alloc-report deltas; ASan runs on the regex/json fixtures.
- **Proposed chain brief outline (string-in-loop)**:
  1. **Scope**: investigate why `.len()` inside a hot `while` loop costs 90 ns/iter when a single call costs 19 ns. Probably a borrow-vs-clone bug in the lowering of method calls on loop-local string vars.
  2. **Fix site**: TBD — likely `src/ir/lowering/builtins.rs` or whichever pass handles `String.method()` lowering.
  3. **Success metric**: `string len x32 loop` ≤ 700 ns (matches Python); `string slice hot loop` ≤ 1 µs.
  4. **Verification gates**: bench_string before/after; alloc-report on a tiny strlen-in-loop fixture.

### 4. **Compile-time: `codegen` 2× per-instruction inflation on large workloads**
- **Axis**: A.
- **Source location**: `src/backend/c_lir/mod.rs` (3178 lines), `helpers.rs` (1990 lines).
- **Root-cause hypothesis**: lacking sub-pass instrumentation inside codegen, but the `2×` inflation across the same workloads where drop_elab also inflates 5.6× strongly suggests **the same shape**: per-block iteration with per-instruction lookups against per-function maps (slot types, value types) that become deep on large functions. Speculative: linear searches in a per-fn data structure. Worth a profiler pass before committing to a fix shape.
- **Proposed chain brief outline**:
  1. **Scope**: add per-sub-pass timing to codegen (`emit_func_forward_decls`, `emit_function_body`, `emit_block`, `emit_inst`) — mirror `gir_lower`'s timing depth.
  2. **Fix site**: TBD once data is in hand.
  3. **Success metric**: codegen on self_host_lowerer ≤ 60 ms (≥ 2× speedup); total compile down 9 %.
  4. **Verification gates**: byte-identical C output on a corpus of fixtures.

### 5. **Runtime: compute benches 5–14× slower than Rust**
- **Axis**: C.
- **Source location**: cross-cutting — Vec2 dot at 13.7× suggests struct-by-value ABI; tight while loops at 7.5× suggest missed loop-strength optimizations or excess drop-flag check overhead.
- **Root-cause hypothesis**: the gap likely splits into three:
  - **Struct ABI overhead** (Vec2, the 13.7× case): we are passing `Vec2` (16 bytes) by-value but possibly through memory rather than registers, OR with a clone+drop pair at the call boundary. Rust's `#[derive(Copy)]` lets LLVM register-pass two f64s.
  - **Drop-flag checks in hot loops**: the `while i < N` loops we emit may carry a per-iter check that gets hoisted in Rust but not in our C output. `bench while loop 10000` at 24.9 µs / 10000 = 2.49 ns/iter (just add + cmp + branch) vs Rust's 0.33 ns/iter — the inner instruction count must differ.
  - **Recursion overhead** (fib at 5.6×): function-call ABI overhead — passing/returning i64 may be going through a wrapper for ownership tracking.
- **Proposed chain brief outline**:
  1. **Scope**: pick the Vec2 dot bench, disassemble both binaries (`objdump -d` on `/tmp/bench_compute` vs the Rust one), tabulate per-call instruction count, identify the gap.
  2. **Fix site**: depends on findings — likely `src/backend/c_lir/` for ABI emission, possibly `src/lir/lower/` for the struct-by-value pattern.
  3. **Success metric**: Vec2 dot product x100 ≤ 200 ns/iter (within 2.5× of Rust); fib(20) ≤ 25 µs (within 2.5× of Rust).
  4. **Verification gates**: all bench fixtures; correctness on `tests/fixtures/*struct*.gg`.

## Anomalies / surprises

- **`string_cow` DOWN 88.5 % while `array_clone` UP 203× since phase 3d** — clearest single tell of a materialize-on-mutation regression. The two counters move in opposite directions on a CoW-correctness regression; they should move in the same direction on a CoW-coverage gain.
- **bench_basic is essentially measuring printf+process-startup overhead.** Both benches in the file are loop-invariant constants. Recommend either retiring or growing the fixture, but not using it as a perf signal.
- **`string len access` (single `.len()`) is 1.26× Python — almost the same speed!** This is genuinely surprising — for a single struct-field read we should crush Python. Means we're paying ~ 15 ns per `.len()` for what *should* be a single load. Hint: the bench body wraps each `.len()` call in a fresh function invocation; per-call overhead dominates.
- **`string concat` at 0.61× of Python** — Python's small-string interning + extremely tight C path for `+` is hard to beat, but losing to it on a single concat means we're paying overhead per call that we shouldn't. Worth a profile.
- **`semantic` sub-passes are remarkably even** on the lowerer — typecheck 19.7 ms, meta_consts 18.3 ms, safety_check_module 20.7 ms, safety::check_items_recursive 18.0 ms. No single dominant offender. If we want to compress semantic % wholesale, we'd need to find a cross-cutting structural win (e.g. fewer module-wide repeated walks).
- **`gir_optimize` is 3.8 % of compile** on the lowerer (29 ms). Untimed at sub-pass level. Probably a worthwhile target later but not now.
- **No phase has super-linear scaling that's > 6× per-instruction** across the surveyed workloads — the hot spots are clear (drop_elab + codegen) but not pathological. The bigger story is the regression on the *runtime* side from Axis B and the Dict/Set/String runtime perf from Axis C.

## Raw data

- `/tmp/profile-pass1-self_host_lowerer.json`
- `/tmp/profile-pass1-self_host_typechecker.json`
- `/tmp/profile-pass1-self_host_parser.json`
- `/tmp/profile-pass1-yaml_parse.json`
- `/tmp/profile-pass1-test_vector_all.json`
- `/tmp/profile-pass1-bench_basic.json`
- `/tmp/profile-pass1-bench_collections.json`
- `/tmp/profile-pass1-bench_compute.json`
- `/tmp/profile-pass1-bench_string.json`
- `/tmp/profile-pass1-bench_string_methods.json`
- `/tmp/profile-pass1-selfhost-mem.json` (Axis B baseline snapshot)
- `/tmp/profile-pass1-py-equiv/bench_compute.py`
- `/tmp/profile-pass1-py-equiv/bench_collections.py`
- `/tmp/profile-pass1-py-equiv/bench_string.py`
- `/tmp/profile-pass1-rs-equiv/bench_compute/` (Rust crate)
- `/tmp/profile-pass1-bench-compute-py.txt`, `bench-collections-py.txt`, `bench-string-py.txt`
- `/tmp/profile-pass1-bench-compute-rs.txt`
- `/tmp/profile-pass1-bench-basic-out.txt`, `bench-collections-out.txt`, `bench-string-out.txt`, `bench-string-methods-out.txt`
- `/tmp/profile-analysis.txt` (per-workload phase % breakdown)
- `/tmp/profile-runs.log` (all 30 raw profile runs, 3 per workload)

## Suggested next step

Take **Hotspot #1** (self-host memory regression) FIRST — it is the largest single regression discovered in this pass and the bisect window between phase 3d (2026-04-22, git_rev recorded in the JSON) and `be281480` (2026-05-28) is bounded. The 307-commit range looks scary but bisect collapses it to ~ 9 steps. Hotspots #2 and #3 are independent and can run in parallel chains after #1 lands (or in parallel with #1's bisect if the bisect agent is a single-task agent).
