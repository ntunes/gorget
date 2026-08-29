# `sanitize_selftest/` — the sanitize gate's positive and negative controls

`scripts/sanitize_sweep.sh` runs these four fixtures before it reports anything
about the corpus, and asserts that each of its detectors behaves. A guard that
has never been seen to fail is not evidence
(`docs/devbook/25-structural-guards.md`), and this gate's whole subject is
verdicts that cannot be trusted — so it watches itself fail on every invocation
rather than on a demonstration someone ran once.

| fixture | asserts |
|---|---|
| `selftest_clean.gg` | the clean path stays quiet — CLEAN, not flaky |
| `selftest_leak.gg` | the leak detector fires, with exactly ONE leak class at ONE record |
| `selftest_leak_twice.gg` | the SAME class at TWO records — and that the class check fires when a row tolerates only one |
| `selftest_alternating_leak.gg` | the flake detector fires on a row whose verdict is not unanimous |

**They live in a subdirectory on purpose.** The corpus walk is
`find tests/fixtures -maxdepth 1`, so a control committed beside the corpus
would become a permanently-red unlisted leak row *and* a permanently-flaky row,
forcing the very ceilings it exists to prove. Not `known_gaps/` either: that
directory is a shrink-only ledger of open defects and feeds the round's
convergence count, and these are instruments, not gaps.

**`selftest_alternating_leak.gg` alternates deterministically**, using a marker
file in its own working directory rather than a clock or a random source: run 1
is clean, run 2 leaks, run 3 is clean. A genuine coin flip would let the
self-test pass by luck whenever all `REPS` runs landed on the same face.

**If a control stops behaving, that is a real finding, not a broken test.**
`selftest_leak.gg` and `selftest_leak_twice.gg` leak through
`__gorget_closure_env_alloc` — every builtin higher-order call leaks its
environment, the largest single group on `tests/sanitize/LEAK_ALLOWLIST.txt`.
The day that defect is fixed these two go clean and the self-test fails
**loudly**, which is the correct outcome: re-point the controls at a leak class
that is still live, in the same commit that retires the old one. What must never
happen is a control that quietly stops proving anything.
