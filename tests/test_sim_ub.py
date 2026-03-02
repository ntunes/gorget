#!/usr/bin/env python3
"""
UB-detection test harness for `gg sim --ub-checks`.

For each .gg file in tests/sim_ub/, runs:
  cargo run -- sim --ub-checks --ignore-leaks <fixture>

and checks:
  1. Exit code is 0
  2. Stdout matches <fixture>.expected (if present)
  3. No UB error keywords appear in stderr

Future: when Instruction::Dealloc is emitted by the GIR lowerer,
add "positive UB" tests (expected UB substrings in .ub_expected files).

Usage:
  python3 tests/test_sim_ub.py [fixture_name_substring]
"""

import os
import subprocess
import sys
import time

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FIXTURES_DIR = os.path.join(REPO_ROOT, "tests", "sim_ub")
CARGO = "cargo"

# Keywords that should NOT appear in stderr for clean programs.
UB_ERROR_KEYWORDS = [
    "use-after-free",
    "double-free",
    "uninitialized read",
    "invalid bool value",
    "invalid enum tag",
]


def run_sim(fixture_path, extra_flags=None):
    """Run gg sim --ub-checks --ignore-leaks on a fixture."""
    cmd = [CARGO, "run", "--quiet", "--", "sim", "--ub-checks", "--ignore-leaks"]
    if extra_flags:
        cmd.extend(extra_flags)
    cmd.append(fixture_path)
    start = time.time()
    result = subprocess.run(cmd, capture_output=True, text=True, cwd=REPO_ROOT)
    elapsed = time.time() - start
    return result, elapsed


def load_expected(fixture_path):
    """Load .expected file alongside the fixture, or None if not present."""
    expected_path = fixture_path.replace(".gg", ".expected")
    if os.path.exists(expected_path):
        with open(expected_path) as f:
            return f.read()
    return None


def has_ub_error(stderr):
    """Check if stderr contains any UB error keywords."""
    low = stderr.lower()
    for kw in UB_ERROR_KEYWORDS:
        if kw in low:
            return kw
    return None


def main():
    filter_str = sys.argv[1] if len(sys.argv) > 1 else None

    fixtures = sorted(
        f for f in os.listdir(FIXTURES_DIR) if f.endswith(".gg")
    )
    if filter_str:
        fixtures = [f for f in fixtures if filter_str in f]

    if not fixtures:
        print(f"No fixtures found in {FIXTURES_DIR}")
        sys.exit(1)

    passed = 0
    failed = 0
    results = []

    for fname in fixtures:
        fixture_path = os.path.join(FIXTURES_DIR, fname)
        result, elapsed = run_sim(fixture_path)
        expected = load_expected(fixture_path)

        status = "PASS"
        reason = ""

        if result.returncode != 0:
            status = "FAIL"
            reason = f"exit code {result.returncode}"
        elif expected is not None and result.stdout != expected:
            status = "FAIL"
            reason = f"stdout mismatch\n  expected: {repr(expected[:80])}\n  got:      {repr(result.stdout[:80])}"
        elif (ub_kw := has_ub_error(result.stderr)) is not None:
            status = "FAIL"
            reason = f"unexpected UB error: {repr(ub_kw)}"

        results.append((fname, status, reason, elapsed))

        indicator = "." if status == "PASS" else "F"
        print(indicator, end="", flush=True)

    print()
    print()

    for fname, status, reason, elapsed in results:
        ms = int(elapsed * 1000)
        if status == "FAIL":
            print(f"  FAIL:  {fname} ({ms}ms)")
            for line in reason.splitlines():
                print(f"         {line}")
        else:
            print(f"  PASS:  {fname} ({ms}ms)")

    passed = sum(1 for _, s, _, _ in results if s == "PASS")
    failed = sum(1 for _, s, _, _ in results if s == "FAIL")
    total = len(results)

    print()
    print(f"=== Results ({total} tests) ===")
    print(f"PASS: {passed:>5}")
    print(f"FAIL: {failed:>5}")

    sys.exit(0 if failed == 0 else 1)


if __name__ == "__main__":
    main()
