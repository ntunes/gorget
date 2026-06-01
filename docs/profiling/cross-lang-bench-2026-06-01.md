# Cross-language benchmark — Gorget vs Python vs Rust (2026-06-01)

Extends the prior Gorget-vs-Python runtime comparison (DONE.md 2026-05-29) to
include Rust. Microbenchmarks — read the caveats before quoting.

## Tooling
- **Gorget** `gg` release (C backend) — harness: `cargo run --release -- test --bench <f>.gg`
  (3 warmups, auto-calibrated iters until total ≥1s, `total_ns/iters` via CLOCK_MONOTONIC).
- **Python** 3.11.2 (CPython) — `timeit.repeat(number=N, repeat=7)`, median.
- **Rust** rustc 1.93.1, `rustc -O`, manual `Instant` loop, 7 reps median, `std::hint::black_box`
  on inputs AND outputs.
- Each times the SAME whole bench body per iteration (matches Gorget's harness).

## Results (median ns/iter; ratio >1× = Gorget faster)

| Case | Gorget ns | Python ns | Rust ns | Gorget vs Python | Gorget vs Rust |
|---|--:|--:|--:|--:|--:|
| fib(20) recursive          | 61,600 | 442,523 | 10,947 | **7.2× faster** | 5.6× slower |
| sum range 1000             |  2,820 |  14,932 |  1,288 | **5.3× faster** | 2.2× slower |
| for range 0..100           |    254 |   1,037 |     91 | **4.1× faster** | 2.8× slower |
| Vec2 dot product x100      |  1,080 |   3,952 |    203 | **3.7× faster** | 5.3× slower |
| string concat (2 literals) |     26 |      17 |     16 | 1.6× slower¹    | 1.6× slower |
| Vector[int] push x1000     |  4,820 |  13,300 |    389 | **2.8× faster** | 12.4× slower |
| Dict[Str,int] build+lookup x50 | 13,530 | 6,588² | 4,546 | 2.1× slower²  | 3.0× slower |

¹ unfair (Python constant-folds the literal concat). ² CPython's dict/str are tuned C.

**Summary:** on clean compute (fib/sum/for-range/Vec2-dot) Gorget is **~4.9× faster than
CPython** and **~3.7× slower than `rustc -O`** (geometric mean). It trails Python only where
CPython delegates to hand-tuned C internals (string interning, dict), not on compute.

## Caveats (honest accounting)
1. Microbenchmarks — tiny hot loops; not whole-program representative.
2. **Rust needed per-iter `black_box`** on sum-range/for-range/Vec2-dot — without it `rustc -O`
   closed-formed the loops (reported 0.4 ns, i.e. didn't run them). The barrier forces the same
   iteration count Gorget/Python run; it's an optimization fence, not added compute, but it's an
   asymmetry and is noted. fib/string/Vector/Dict needed no per-iter barrier.
3. **`string concat` is NOT a fair 3-way row** — both operands are compile-time literals; Python
   constant-folds it (disregard Python here); Gorget vs Rust both build a real string but with
   different representations. Illustrative only.
4. **Dict/string cases favor CPython's decades-tuned C internals** — the only category where
   Gorget trails Python; not a compute weakness.
5. **Gorget's harness has no result-observation barrier** — trivial cases (addition 1ns, literal
   assign 4ns, Point construct 10ns) are likely partially DCE'd and were EXCLUDED; the chosen
   compute cases produce work-proportional times (not elided).
6. Vec2 dot: Gorget passes structs by value; Python uses unpacked floats; Rust passes 4 scalars
   to `#[inline(never)]` — close but not byte-identical data shapes.

Scratch programs were under `/tmp/bench/` (Python + Rust equivalents of the `bench_*.gg` bodies);
Gorget bodies are the unmodified `tests/fixtures/bench_{compute,basic,collections}.gg`.
