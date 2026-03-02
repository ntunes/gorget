#!/usr/bin/env python3
"""
UB-detection test harness for `gg sim --ub-checks`.

Two modes for each .gg fixture in tests/sim_ub/:

CLEAN tests (no .ub_expected file):
  Runs:  cargo run -- sim --ub-checks --ignore-leaks <fixture>
  Checks:
    1. Exit code is 0
    2. Stdout matches <fixture>.expected (if present)
    3. No UB error keywords appear in stderr

POSITIVE UB tests (.ub_expected file present):
  Runs:  cargo run -- sim --ub-checks <fixture>   (no --ignore-leaks)
  Checks:
    1. Exit code is non-zero (UB was detected)
    2. The content of .ub_expected is a substring of stderr

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
    """Run gg sim --ub-checks on a fixture."""
    cmd = [CARGO, "run", "--quiet", "--", "sim", "--ub-checks"]
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


def load_ub_expected(fixture_path):
    """Load .ub_expected file alongside the fixture, or None if not present."""
    ub_path = fixture_path.replace(".gg", ".ub_expected")
    if os.path.exists(ub_path):
        with open(ub_path) as f:
            return f.read().strip()
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
        ub_expected = load_ub_expected(fixture_path)

        if ub_expected is not None:
            # ── Positive UB test: expect non-zero exit + error substring in stderr ──
            result, elapsed = run_sim(fixture_path)  # no --ignore-leaks
            status = "PASS"
            reason = ""

            if result.returncode == 0:
                status = "FAIL"
                reason = f"expected UB error but got exit 0 (stderr: {result.stderr[:200]!r})"
            elif ub_expected not in result.stderr:
                status = "FAIL"
                reason = (
                    f"expected {ub_expected!r} in stderr\n"
                    f"         got: {result.stderr[:300]!r}"
                )
        else:
            # ── Clean test: expect exit 0, no UB errors ──
            result, elapsed = run_sim(fixture_path, extra_flags=["--ignore-leaks"])
            expected = load_expected(fixture_path)
            status = "PASS"
            reason = ""

            if result.returncode != 0:
                status = "FAIL"
                reason = f"exit code {result.returncode}"
            elif expected is not None and result.stdout != expected:
                status = "FAIL"
                reason = (
                    f"stdout mismatch\n"
                    f"  expected: {expected[:80]!r}\n"
                    f"  got:      {result.stdout[:80]!r}"
                )
            elif (ub_kw := has_ub_error(result.stderr)) is not None:
                status = "FAIL"
                reason = f"unexpected UB error: {ub_kw!r}"

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
