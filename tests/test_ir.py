#!/usr/bin/env python3
"""Test all .gg fixtures through the --ir pipeline vs normal pipeline.

Every spawn goes through `scripts/proc_guard.run`, not `subprocess.run`. Three
of the seven sites below RUN A COMPILED FIXTURE BINARY, and a miscompiled fixture
that forks is exactly the shape that leaves a spinner behind: on expiry CPython
kills the DIRECT CHILD only and then blocks in `communicate()` on pipes the
surviving grandchild still holds open. `proc_guard` makes the child a
process-group leader and kills the group.

⚠ AND EVERY SITE CHECKS `timed_out` EXPLICITLY. `subprocess.run`'s timeout RAISED;
`proc_guard.run` RETURNS, because a timeout is a classification rather than a
control-flow accident. Without the check a hung BUILD returns nonzero with no
"semantic error" on stderr and is silently recorded as a build failure, and a hung
RUN is recorded as "the binary printed nothing" — the false-CLEAN shape this whole
change exists to retire, reintroduced by a mechanical call swap.
"""
import os, pathlib, sys, glob

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent / "scripts"))
import proc_guard  # noqa: E402  (path must be set first)

def test_one(gg_path):
    stem = os.path.splitext(os.path.basename(gg_path))[0]
    fixture_dir = os.path.dirname(gg_path)
    ref_bin = os.path.join(fixture_dir, stem)
    ir_bin = f"/tmp/{stem}_ir"

    # Build reference (non-IR)
    r = proc_guard.run(["cargo", "run", "--quiet", "--", "build", gg_path], timeout=60)
    if r.timed_out:
        return ("SKIP", stem, "", "", "ref BUILD HUNG (killed at 60s)")

    if r.returncode != 0:
        # Reference build failed. If it's a semantic error, both pipelines should reject it.
        # If it's a linker/C error, it's a test-infra issue — keep skipping.
        ref_stderr = r.stderr
        if "semantic error" not in ref_stderr and "error(s) found" not in ref_stderr:
            return ("SKIP", stem, "", "", "ref build fail (non-semantic)")
        # Semantic error: verify GIR also rejects the program
        r_ir = proc_guard.run(["cargo", "run", "--quiet", "--", "build", "--ir", gg_path, "-o", ir_bin], timeout=60)
        if r_ir.timed_out:
            return ("MISMATCH", stem, "reject", "HUNG (killed at 60s)",
                    "GIR build hung where the reference rejected")
        if r_ir.returncode != 0:
            return ("PASS", stem, "", "", "both reject (expected compile error)")
        else:
            return ("MISMATCH", stem, "build fail", "build success",
                    "GIR accepted code that should be rejected")

    # Get expected output from reference run
    try:
        r2 = proc_guard.run([ref_bin], timeout=10)
        if r2.timed_out:
            return ("SKIP", stem, "", "", "ref run HUNG (killed at 10s)")
        expected = r2.stdout
        ref_ok = r2.returncode == 0
    except Exception as e:
        return ("SKIP", stem, "", "", f"ref run fail: {e}")

    if not ref_ok and expected == "":
        # Reference crashed with no stdout (e.g. div_by_zero, overflow, assert_fails).
        # Verify the GIR binary also crashes.
        r_ir = proc_guard.run(["cargo", "run", "--quiet", "--", "build", "--ir", gg_path, "-o", ir_bin], timeout=60)
        if r_ir.timed_out:
            return ("BUILD_FAIL", stem, "", "", "GIR BUILD HUNG (killed at 60s)")
        if r_ir.returncode != 0:
            err = r_ir.stderr.strip().split('\n')[-1] if r_ir.stderr else ""
            return ("BUILD_FAIL", stem, "", "", err[-150:])
        try:
            r4 = proc_guard.run([ir_bin], timeout=10)
            if r4.timed_out:
                return ("MISMATCH", stem, "crash(exit!=0)", "HUNG (killed at 10s)",
                        "GIR hung where the reference crashed")
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
    r3 = proc_guard.run(["cargo", "run", "--quiet", "--", "build", "--ir", gg_path, "-o", ir_bin], timeout=60)
    if r3.timed_out:
        return ("BUILD_FAIL", stem, "", "", "GIR BUILD HUNG (killed at 60s)")
    if r3.returncode != 0:
        err = r3.stderr.strip().split('\n')[-1] if r3.stderr else ""
        return ("BUILD_FAIL", stem, "", "", err[-150:])

    # Run IR binary
    try:
        r4 = proc_guard.run([ir_bin], timeout=10)
        if r4.timed_out:
            return ("MISMATCH", stem, expected[:300], "HUNG (killed at 10s)",
                    "IR binary hung; a hang is a defect, not an empty stdout")
        actual = r4.stdout
    except Exception as e:
        return ("SKIP", stem, "", "", f"ir run fail: {e}")

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
