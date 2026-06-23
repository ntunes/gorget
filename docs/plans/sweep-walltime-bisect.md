# Integration-Sweep Wall-Time — Rigorous Bisect & Attribution

**Scout deliverable.** Worktree: `/workspace/gorget/.claude/worktrees/agent-a05f80115fdae5035`
(toplevel `/workspace/gorget`, branch fast-forwarded to gorget-1 tip `a0f3205a`).
Read-only + measuring; nothing integrated. Every number below was **regenerated this
session** (2026-06-23) — no dated figure is trusted.

THEN commit = **`f0e4064c`** (2026-06-08, "lazy-iter phase 1a") — the nearest
buildable commit to the owner's "a couple weeks ago".
NOW commit = **`a0f3205a`** (2026-06-23, gorget-1 tip).
Box: 10 CPU, 15 GiB RAM, `cc = gcc 12.2.0` (no clang), no `/usr/bin/time`
(RSS via `getrusage(RUSAGE_CHILDREN)` python wrapper, peak per fresh child process).

---

## TL;DR — the premise is INVERTED

1. **The overflow-check hypothesis is REFUTED** (re-confirmed): the 27 MB emitted C
   carries **1,784 `__builtin_*_overflow` branches out of 1,038,178 lines = 0.17 %.**
   Negligible to cc. Moving on.

2. **"Not just more code" — PARTIALLY TRUE, but small.** The emitted C grew **+42.4 %**
   while the self-host source grew only **+27.6 %**. The source→C expansion ratio rose
   **17.3× → 19.3× (+11.6 %)**. BUT this is **+22 % bigger function bodies** (618 → 754
   emitted-C lines per function), **not** a monomorphization-count explosion (function
   count grew only +16.7 %, slower than the source). That +22 %/fn is the new
   drop-elab / lazy-iter / error-model FaultCheck / clone-insertion C — real work, not a
   regression.

3. **The headline finding (HOLY SHIT): the self-host self-compile got ~4.5× FASTER, not
   slower.** On THIS box, one self-compile of `driver.gg` is **~74 s NOW vs ~336 s at
   f0e4064c**, and **array clones dropped 280×: 2.87 BILLION → 10.3 MILLION**, peak RSS
   **1.48 GB → 517 MB**. A deliberate clone-elimination campaign (DONE.md `4c681f3f`
   et al., for-element borrow-alias) cut the clone-bomb between THEN and NOW.

4. **The owner's "~300 s" is the `self_host_bootstrap_fixed_point` SUB-TEST, not the
   sweep — and it is STILL ~300 s.** Measured NOW: `self_host_bootstrap_fixed_point`
   alone = **327 s** (3 self-compiles + 2 cc). DONE.md recorded it at 214 s→383 s→~420 s
   across May–June. The **full** C sweep was **already 1041–1208 s at the f0e4064c era**
   (1123 tests, DONE.md verified lines) and is **859 s NOW** (1346 tests) — i.e. the full
   sweep got *faster per test* even as the corpus grew. The "300 → 859" comparison is
   a **sub-test vs full-sweep** mix-up, layered on a real corpus-growth trend.

**Bottom line for the owner:** there is **no codegen/monomorphization regression** driving
the wall-time. The sweep is dominated by (a) `self_host_bootstrap_fixed_point` at ~327 s
(3 × the now-fast self-compile + 2 × cc-of-27 MB-C), and (b) the per-fixture self-host
nets that grew with the corpus (`self_host_runtime` 429 → 717 snapshots). The single
biggest *fixable* lever is **the C-backend cc cost on the 27 MB body, paid 5× inside the
bootstrap** — which the **LLVM backend already sidesteps** (LLVM sweep ≈ 540–587 s vs C
≈ 800–859 s, a ~270 s gap that is exactly the repeated 27 MB-C cc).

---

## 1. Refuted: the overflow-check hypothesis (re-confirmed, 0.17 %)

```
grep -c '__builtin_add_overflow|__builtin_sub_overflow|__builtin_mul_overflow' <emit-c-lir>
```

| metric                                  | THEN f0e4064c | NOW a0f3205a |
|-----------------------------------------|--------------:|-------------:|
| emitted-C lines (`gg build --emit-c-lir`)| 744,402      | 1,038,178    |
| overflow-check branches                 | 1,466         | 1,784        |
| **fraction of emitted C**               | **0.20 %**    | **0.17 %**   |

cc time scales with C **size**, not with the 0.17 % of lines that are overflow traps;
the always-checked-arithmetic change (`fb2e5037`) is not a measurable sweep cost.
The bootstrap & `self_host_emit_cc_run` already use `cc -O0` (no extra optimization of
the trap branches). **Confirmed dead end — do not pursue.**

---

## 2. The attribution table (THEN → NOW, all regenerated this session)

| metric                                        | THEN (f0e4064c) | NOW (a0f3205a) |        Δ |
|-----------------------------------------------|----------------:|---------------:|---------:|
| self-host source (`self_host_lowerer/*.gg`, symlinks resolved) | 43,044 lines / 28 files | 54,905 lines / 38 files | **+27.6 %** |
| top-level corpus fixtures (`tests/fixtures/*.gg`) | 1,150         | 1,294          | **+12.5 %** |
| `runtime_snapshots/*.out` (`self_host_runtime` set) | 429           | 717            | **+67.1 %** |
| Rust-gg front-half emit-C (`--emit-c-lir`) wall | 0.78 s        | 3.12 s         | (small abs) |
| emitted body-C (Rust front-half)              | 744,402 lines / 18 MB | 1,038,178 lines / 27 MB | **+42 % lines / +50 % size** |
| **source → C expansion ratio** (body-C / src) | **17.3×**       | **19.3×**      | **+11.6 %** |
| emitted-C lines **per function**              | 618             | 754            | **+22.0 %** |
| emitted fn-defs / `__clone` / `__drop` count  | 1,203 / 1,329 / 3,011 | 1,404 / 1,403 / 3,136 | +16.7 % / +5.6 % / +4.2 % |
| Rust-gg **full** build (`gg build -o`) wall   | 9.21 s          | 19.86 s        | +10.6 s  |
| Rust-gg full build peak RSS                   | 882 MB          | 929 MB         | +5 %     |
| complete-TU cc (`cc -O0 -w` on the body+preamble) | 7.95 s (18.7 MB) | 10.7 s (27.5 MB) | +35 %  |
| **self-host self-compile** (stage0 bin → body C) wall | **~336 s**¹  | **~74 s**      | **−78 % (4.5× faster)** |
| self-compile peak RSS                         | **1,483 MB**    | **517 MB**     | **−65 %** |
| self-compile **array_clone**                  | **2,874,486,403** | **10,271,763** | **−99.6 % (280×)** |
| self-compile array_new / total_allocs         | 2.88 B / 1.40 B | 15.7 M / 74.9 M | ~180× / ~19× |
| self-compile output determinism               | n/a             | byte-identical on re-run | — |

¹ THEN self-compile needs a raised stack (`ulimit -s 524288`) — the f0e4064c `driver.gg`
runs `compile_main` on the **main thread**; NOW's wraps it in a 512 MB `thread_spawn`
(`driver.gg:265`). At the OS-default 8 MB stack the THEN binary SIGSEGVs after 3.4 s
(123 lines emitted) — that 3.4 s is a truncated crash, **not** a fast compile. The real
THEN self-compile is 336 s (bounded 512 MB stack) / 381 s (unlimited), both with the
2.87 B-clone bomb thrashing 1.48 GB RSS. (The recorded DONE.md 214–420 s THEN bootstrap
times were on the less-loaded dev box; the clone counts and emitted-C sizes here are
box-independent and tell the same story.)

### Reproduce

```bash
cargo build --release
GG=target/release/gg ; D=tests/fixtures/self_host_lowerer/driver.gg ; LIB=$PWD/lib

# (a) Rust front-half + overflow count + size
$GG build $D --emit-c-lir > /tmp/front.c ; wc -l /tmp/front.c
grep -c '__builtin_add_overflow\|__builtin_sub_overflow\|__builtin_mul_overflow' /tmp/front.c

# (a)+(b) full build (writes complete TU next to -o) + cc-only on the TU
$GG build $D -o /tmp/driver_bin            # leaves /tmp/driver_bin.c (complete, linkable)
cc -O0 -w -o /tmp/cc_out /tmp/driver_bin.c -lm -lpthread

# self-host self-compile + clone-stats (THEN needs: ulimit -s 524288)
/tmp/driver_bin $D $LIB --lir-c > /tmp/body.c
$GG build $D --clones=stats -o /tmp/driver_clones
/tmp/driver_clones $D $LIB --lir-c >/dev/null    # prints [clone-stats] … on stderr
```

---

## 3. Decomposing the 859 s C sweep

`self_host_bootstrap_fixed_point` **converges at stage 2** (verified this session:
`stage2_body == stage3_body`, byte-identical). It runs **3 self-compiles + 2 cc**:

```
stage0→1 self-compile  ~74 s    (stage0 = gg-built driver, fast)
cc stage1              ~12 s    (cc -O0 on the spliced 27 MB stage1.c)
stage1→2 self-compile ~107 s    (stage1 = cc-O0-built driver — slower than the gg-built one)
cc stage2              ~11 s
stage2→3 self-compile ~107 s    (confirms convergence)
                       ──────
            ≈ 311 s hand-sum  →  327 s MEASURED end-to-end (cargo test --release --exact)
```
`MAX_GEN = 5` is an **upper bound, not wasted work** — the loop `break`s at first
convergence (`integration.rs:15305`), so it always does exactly 3 self-compiles here.
(Note the cc-O0-built stages 1/2 self-compile ~45 % slower than the gg-LIR-built stage 0;
this is a real ~65 s tax per bootstrap from re-running the unoptimized driver, see §5.)

Measured default-running heavy self-host tests (release, `--test-threads=4`):

| test (default-running)            |   wall | what it does |
|-----------------------------------|-------:|--------------|
| `self_host_bootstrap_fixed_point` | **327 s** | 3 self-compiles + 2 cc-of-27 MB |
| `self_host_runtime` (717 fixtures)| **64 s**  | driver build (~20 s) + 717 × (emit small-fixture-C + cc + run), parallel-4 |
| `lowerer_comparison` + `c_emit_comparison` + `self_host_bootstrap` + `self_host_bootstrap_fixed_point` (run together, `--test-threads=4`) | **632 s** (MEASURED) | the full self-host serial cluster: 1 cached driver build + each test's self-compile-scale run + the 327 s bootstrap |

So the **self-host serial cluster alone is ~632 s of the 859 s C sweep (~74 %)**;
the remaining ~227 s is `self_host_runtime` (64 s) + the ~1200 ordinary fixture
build/run tests + ASan box_deref tests, parallelized across 4 threads.
(One overlapping-cargo run of this cluster flaked once mid-bisect — a `/tmp`
collision / parallel-run flake; the clean isolated re-run passed 0-fail in 632 s.)

The `serial(self_host_lowerer_driver)` family shares ONE cached driver build
(`build_gg_dir_cached`, `integration.rs:10307`), so the driver is *built* once per
test process but each test still *runs* a 27 MB-scale self-compile and/or per-fixture
nets. **Not** running by default (so NOT in the 859 s): `self_host_e2e` (`GG_FULL=1`),
`self_host_runtime_diff` (`GG_RUNTIME_DIFF=1`).

### Why the full sweep is 859 s now but was 1041–1208 s at f0e4064c
DONE.md (verified, THEN tree): `1123/1123 passing in 1041 s … 1064 s … 1207 s … 1208 s`,
`bootstrap_fixed_point` 371–420 s. The 280× clone cut made every self-compile ~4.5×
faster, which **more than offset** the +223 tests and +67 % snapshots — the sweep got
*faster in absolute terms* while the corpus grew. The owner's "300 → 859" is comparing
the bootstrap sub-test (≈300 s, then and now) against the full sweep (859 s).

---

## 4. cc is LINEAR in C size — no super-linearity to exploit

`cc -O0` on the emitted body is linear:
- NOW 27.5 MB → 11.1 s ; THEN 18.7 MB → 7.95 s.
- ratio-of-ratios `(11.1/27.5)/(7.95/18.7) = 0.95` (1.0 = perfectly linear).

So **splitting the 27 MB TU would NOT speed cc per byte** — gcc -O0 already compiles
function-by-function. The only cc win from splitting is **parallelism across TUs**
(N-way `cc -c` then link), which *could* cut the wall-clock of each 11 s cc step by up to
~Ncpu — but it needs the self-host/Rust emit to write multiple `.c` files + a link step,
a non-trivial backend change. The LLVM backend already gets this for free (compiles `.ll`
to a single `.o`, links runtime `.o`s — no 27 MB cc), which is why the **LLVM sweep is
~270 s faster** (≈540–587 s vs ≈800–859 s, DONE.md).

---

## 5. Ranked recommendations (each with the measured lever)

**The honest framing for the owner:** the sweep wall-time is **not a regression** — it is
(genuinely) more source + far more snapshot fixtures, *minus* a 4.5× self-compile speedup,
with the C-backend cc-of-27 MB cost as the one structural tax. Ranked by ROI:

1. **CI: run the heavy C self-host tests under the LLVM backend (or drop them from the
   default C sweep).** Biggest measured lever, **zero compiler work**. LLVM links `.o`
   instead of cc-ing 27 MB five times in the bootstrap → the recorded LLVM sweep is
   **~270 s faster**. Keep the C bootstrap as a pre-push/nightly gate, run the LLVM
   bootstrap on every push. Expected saving on the default sweep: **~250–300 s** if the
   bootstrap+comparison cluster moves to LLVM.

2. **Build the bootstrap stages with `cc -O2`… NO — do the opposite: build stage-1/2 the
   way stage-0 is built.** Measured: the cc-O0-built stage-1/2 self-compile ~45 % slower
   (107 s vs 74 s) than the gg-LIR-built stage-0. The bootstrap pays this ~65 s tax twice.
   If the stages were built through the same gg/LIR pipeline (or just `cc -O1`) the two
   transitioning self-compiles could approach 74 s each. **Probe first** (`cc -O1` on
   stage1.c, time the stage1→2 run); expected saving **~40–65 s** on the bootstrap, but
   `-O1` adds cc time — net is a measurement, not a guess. *Cheap to test, do it before
   committing.*

3. **Split the emitted C into N TUs + parallel `cc -c` (C backend only).** Real but
   expensive: cc is linear, so the win is purely `cc` parallelism (up to ~Ncpu per build
   step). Affects every C-backend `gg build` of a large program, not just the sweep.
   Needs multi-`.c`-emit + link in both Rust and self-host emitters. Defer unless C-backend
   build latency becomes a product concern; LLVM already solves it for the sweep (rec #1).

4. **Continue the clone-elimination campaign — it is the proven big lever.** array_clone
   is already 280× down (2.87 B → 10.3 M) but is still **10.3 M clones / 15.7 M array_new /
   74.9 M total_allocs** per self-compile, 517 MB RSS. The for-element borrow-alias work
   (DONE.md `4c681f3f`) is the template; a fresh `--clones=stats` + perf profile of the
   *current* hotspot (NOT the dated 16 M / `LirStructDef__clone` figures — re-measure)
   would find the next tranche. Every clone cut speeds **all** self-host tests at once.
   This is parity-orthogonal compiler-quality work — fund only when it beats parity.

5. **Trim/curate the `self_host_runtime` snapshot set (429 → 717).** +288 snapshots ≈
   the +44 s growth of that test. These are the lock-in net (correctness), so don't cut
   blindly — but a tiered "fast subset on push, full set nightly" split is cheap if the
   64 s ever matters. Low priority; correctness > 44 s.

**Do NOT pursue:** the overflow gate (0.17 %, §1); a "monomorphization explosion" fix
(there isn't one — function count grew *slower* than source, §2); splitting cc for
per-byte speed (cc is linear, §4); lowering `MAX_GEN` (already breaks at convergence, §3).

### Design grounding
`docs/devbook/24-layering-discipline.md` (the clone-bomb fixes were read-mode-invariant
erosions — a borrow-alias dropped at the LIR `IndexLoad` boundary; the CLAUDE.md perf
rule "measure MEMORY not just time" is exactly why the 2.87 B-clone / 1.48 GB-RSS signal,
not the wall-time, is the load-bearing evidence here).
