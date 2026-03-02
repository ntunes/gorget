#!/usr/bin/env python3
"""Test all .gg fixtures through `gg sim` vs `gg run`."""
import subprocess, os, sys, glob, re

def normalize(text):
    """Normalize timing in test-runner output so (0ms) == (1ms) etc."""
    return re.sub(r'\(\d+ms\)', '(Nms)', text)

GG = ["cargo", "run", "--quiet", "--"]

def run_gg(args, timeout=60):
    return subprocess.run(GG + args, capture_output=True, text=True, timeout=timeout)

def test_one(gg_path):
    stem = os.path.splitext(os.path.basename(gg_path))[0]
    fixture_dir = os.path.dirname(gg_path)
    ref_bin = os.path.join(fixture_dir, stem)

    # ── 1. Build reference binary ──────────────────────────────────────────
    r = run_gg(["build", gg_path])
    if r.returncode != 0:
        ref_stderr = r.stderr
        # Semantic error → both pipelines should reject
        if "semantic error" in ref_stderr or "error(s) found" in ref_stderr:
            r2 = run_gg(["sim", "--disable-isolation", gg_path])
            if r2.returncode != 0:
                return ("PASS", stem, "", "", "both reject (expected compile error)")
            else:
                return ("MISMATCH", stem, "build fail", "sim success",
                        "sim accepted code that should be rejected")
        return ("SKIP", stem, "", "", "ref build fail (non-semantic)")

    # ── 1b. Skip hot-reload fixtures — native binary compiles a .so (different execution model)
    try:
        with open(gg_path) as f:
            src = f.read()
        if "directive hot-reload" in src or "directive hot_reload" in src:
            return ("SKIP", stem, "", "", "hot-reload: native model differs from sim")
    except Exception:
        pass

    # ── 2. Get reference output ─────────────────────────────────────────────
    try:
        r2 = subprocess.run([ref_bin], capture_output=True, text=True, timeout=10)
        expected = r2.stdout
        ref_ok = (r2.returncode == 0)
    except Exception as e:
        return ("SKIP", stem, "", "", f"ref run fail: {e}")

    # ── 3. Run gg sim ───────────────────────────────────────────────────────
    # Use --disable-isolation so I/O fixtures (file, network, exec) work correctly
    # in the regression harness.  Isolation enforcement is tested separately.
    try:
        r3 = run_gg(["sim", "--disable-isolation", gg_path], timeout=30)
    except subprocess.TimeoutExpired:
        return ("SIM_FAIL", stem, expected[:300], "", "sim timed out")

    # ── 4. Handle runtime-crash fixtures ───────────────────────────────────
    if not ref_ok and expected == "":
        # Reference crashed with no stdout → sim should also crash (non-zero exit)
        if r3.returncode != 0 and r3.stdout == "":
            return ("PASS", stem, "", "", "both crash at runtime")
        elif r3.returncode == 0:
            return ("MISMATCH", stem, "crash", "success", "sim should have crashed")
        else:
            return ("MISMATCH", stem, "", r3.stdout,
                    "sim crashed but produced unexpected stdout")

    # ── 4b. Handle fixtures that exit non-zero but produce matching output ──
    # e.g. test_failure.gg: both ref and sim exit 1 with the same test report
    if not ref_ok and expected != "" and r3.returncode != 0:
        if r3.stdout == expected or normalize(r3.stdout) == normalize(expected):
            return ("PASS", stem, "", "", "both exit non-zero with matching output")

    # ── 5. Check for unimplemented features ────────────────────────────────
    if r3.returncode != 0:
        stderr = r3.stderr
        if "unimplemented runtime function" in stderr:
            fn_name = ""
            for line in stderr.splitlines():
                if "unimplemented runtime function:" in line:
                    fn_name = line.split(":")[-1].strip()
                    break
            return ("SIM_FAIL", stem, expected[:300], r3.stdout[:300],
                    f"unimpl: {fn_name}")
        if "runtime error" in stderr or "panic" in stderr:
            msg = stderr.strip().split('\n')[-1]
            return ("SIM_FAIL", stem, expected[:300], r3.stdout[:300], msg[-150:])
        return ("SIM_FAIL", stem, expected[:300], r3.stdout[:300],
                f"exit {r3.returncode}")

    # ── 6. Compare output ───────────────────────────────────────────────────
    actual = r3.stdout
    if actual == expected:
        return ("PASS", stem, "", "", "")
    # Normalize timing in test-runner output (Nms vs 0ms differences)
    if normalize(actual) == normalize(expected):
        return ("PASS", stem, "", "", "timing-normalized")
    else:
        return ("MISMATCH", stem, expected[:300], actual[:300], "")


def main():
    fixtures = sorted(glob.glob("tests/fixtures/*.gg"))
    filt = sys.argv[1] if len(sys.argv) > 1 else ""
    if filt:
        fixtures = [f for f in fixtures if filt in os.path.basename(f)]

    results = {"PASS": [], "MISMATCH": [], "SIM_FAIL": [], "SKIP": []}

    for gg in fixtures:
        status, stem, exp, act, detail = test_one(gg)
        results[status].append((stem, detail))
        if status == "SIM_FAIL":
            print(f"  SIM_FAIL:  {stem}  ({detail})")
        elif status == "MISMATCH":
            print(f"  MISMATCH:  {stem}")
            if exp and act:
                exp_lines = exp.split('\n')
                act_lines = act.split('\n')
                for i, (e, a) in enumerate(zip(exp_lines, act_lines)):
                    if e != a:
                        print(f"    line {i+1}: exp={e!r} got={a!r}")
                        break
                if len(exp_lines) != len(act_lines):
                    print(f"    exp {len(exp_lines)} lines, got {len(act_lines)} lines")
        elif status == "SKIP":
            print(f"  SKIP:      {stem}  ({detail})")

    total = len(fixtures)
    print(f"\n=== Results ({total} tests) ===")
    print(f"PASS:     {len(results['PASS'])}")
    print(f"MISMATCH: {len(results['MISMATCH'])}")
    print(f"SIM_FAIL: {len(results['SIM_FAIL'])}")
    print(f"SKIP:     {len(results['SKIP'])}")

    if "--sim-fails" in sys.argv:
        print("\n--- SIM_FAIL details ---")
        # Group by failure reason
        unimpl = [(s, d) for s, d in results["SIM_FAIL"] if d.startswith("unimpl:")]
        other  = [(s, d) for s, d in results["SIM_FAIL"] if not d.startswith("unimpl:")]
        if unimpl:
            print("\nUnimplemented runtime functions:")
            by_fn = {}
            for stem, detail in unimpl:
                fn = detail.replace("unimpl: ", "")
                by_fn.setdefault(fn, []).append(stem)
            for fn, stems in sorted(by_fn.items()):
                print(f"  {fn}: {', '.join(stems[:5])}" + (" ..." if len(stems) > 5 else ""))
        if other:
            print("\nOther failures:")
            for stem, detail in other:
                print(f"  {stem}: {detail}")

if __name__ == "__main__":
    main()
