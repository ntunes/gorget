#!/usr/bin/env python3
"""Test all .gg fixtures through the --ir pipeline vs normal pipeline."""
import subprocess, os, sys, glob

def test_one(gg_path):
    stem = os.path.splitext(os.path.basename(gg_path))[0]
    fixture_dir = os.path.dirname(gg_path)
    ref_bin = os.path.join(fixture_dir, stem)
    ir_bin = f"/tmp/{stem}_ir"

    # Build reference (non-IR)
    r = subprocess.run(
        ["cargo", "run", "--quiet", "--", "build", gg_path],
        capture_output=True, text=True, timeout=60
    )

    if r.returncode != 0:
        # Reference build failed. If it's a semantic error, both pipelines should reject it.
        # If it's a linker/C error, it's a test-infra issue — keep skipping.
        ref_stderr = r.stderr
        if "semantic error" not in ref_stderr and "error(s) found" not in ref_stderr:
            return ("SKIP", stem, "", "", "ref build fail (non-semantic)")
        # Semantic error: verify GIR also rejects the program
        r_ir = subprocess.run(
            ["cargo", "run", "--quiet", "--", "build", "--ir", gg_path, "-o", ir_bin],
            capture_output=True, text=True, timeout=60
        )
        if r_ir.returncode != 0:
            return ("PASS", stem, "", "", "both reject (expected compile error)")
        else:
            return ("MISMATCH", stem, "build fail", "build success",
                    "GIR accepted code that should be rejected")

    # Get expected output from reference run
    try:
        r2 = subprocess.run([ref_bin], capture_output=True, text=True, timeout=10)
        expected = r2.stdout
        ref_ok = r2.returncode == 0
    except Exception as e:
        return ("SKIP", stem, "", "", f"ref run fail: {e}")

    if not ref_ok and expected == "":
        # Reference crashed with no stdout (e.g. div_by_zero, overflow, assert_fails).
        # Verify the GIR binary also crashes.
        r_ir = subprocess.run(
            ["cargo", "run", "--quiet", "--", "build", "--ir", gg_path, "-o", ir_bin],
            capture_output=True, text=True, timeout=60
        )
        if r_ir.returncode != 0:
            err = r_ir.stderr.strip().split('\n')[-1] if r_ir.stderr else ""
            return ("BUILD_FAIL", stem, "", "", err[-150:])
        try:
            r4 = subprocess.run([ir_bin], capture_output=True, text=True, timeout=10)
            if r4.returncode != 0 and r4.stdout == "":
                return ("PASS", stem, "", "", "both crash at runtime")
            elif r4.returncode == 0:
                return ("MISMATCH", stem, "crash(exit!=0)", "success(exit=0)",
                        "GIR should have crashed")
            else:
                return ("MISMATCH", stem, "", r4.stdout,
                        "GIR crashed but produced unexpected stdout")
        except Exception:
            return ("SKIP", stem, "", "", "ir run fail")

    # Build with IR
    r3 = subprocess.run(
        ["cargo", "run", "--quiet", "--", "build", "--ir", gg_path, "-o", ir_bin],
        capture_output=True, text=True, timeout=60
    )
    if r3.returncode != 0:
        err = r3.stderr.strip().split('\n')[-1] if r3.stderr else ""
        return ("BUILD_FAIL", stem, "", "", err[-150:])

    # Run IR binary
    try:
        r4 = subprocess.run([ir_bin], capture_output=True, text=True, timeout=10)
        actual = r4.stdout
    except:
        actual = ""

    if actual == expected:
        return ("PASS", stem, "", "", "")
    else:
        return ("MISMATCH", stem, expected[:300], actual[:300], "")

def main():
    fixtures = sorted(glob.glob("tests/fixtures/*.gg"))
    filt = sys.argv[1] if len(sys.argv) > 1 else ""
    if filt:
        fixtures = [f for f in fixtures if filt in os.path.basename(f)]

    results = {"PASS": [], "MISMATCH": [], "BUILD_FAIL": [], "SKIP": []}

    for gg in fixtures:
        status, stem, exp, act, detail = test_one(gg)
        results[status].append((stem, detail))
        if status == "BUILD_FAIL":
            print(f"  BUILD_FAIL: {stem}  Generated C file: /tmp/{stem}_ir.c")
        elif status == "MISMATCH":
            print(f"  MISMATCH:  {stem}")
            if exp and act:
                exp_lines = exp.split('\n')
                act_lines = act.split('\n')
                for i, (e, a) in enumerate(zip(exp_lines, act_lines)):
                    if e != a:
                        print(f"    line {i}: exp={e!r} got={a!r}")
                        break
                if len(exp_lines) != len(act_lines):
                    print(f"    exp {len(exp_lines)} lines, got {len(act_lines)} lines")
        elif status == "SKIP":
            print(f"  SKIP: {stem}  ({detail})")

    print(f"\n=== Results ({len(fixtures)} tests) ===")
    print(f"PASS:       {len(results['PASS'])}")
    print(f"MISMATCH:   {len(results['MISMATCH'])}")
    print(f"BUILD_FAIL: {len(results['BUILD_FAIL'])}")
    print(f"SKIP:       {len(results['SKIP'])}")

    if "--build-fails" in sys.argv:
        print("\n--- Build failures ---")
        for stem, detail in results["BUILD_FAIL"]:
            print(f"  {stem}: {detail}")

if __name__ == "__main__":
    main()
