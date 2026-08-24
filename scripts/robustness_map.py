#!/usr/bin/env python3
"""Regenerate the beginner-code robustness map and diff it against the baseline.

The map answers "how much ordinary, common code actually works", which the main
suite cannot: 2113 fixtures sample what the compiler's own self-host does, not
what a person learning the language writes. Cells are derived from beginner
tutorials in OTHER languages, deliberately not from our own corpus.

Two properties this script exists to preserve:

  * EXPECTATIONS ARE HAND-DERIVED, NEVER CAPTURED. The expected column in
    MANIFEST.tsv was written before the cell was ever run. This script only ever
    READS it. Never "update" an expectation to match what the compiler prints --
    that pins a bug as canonical, which is the worst outcome available here.

  * PROGRESS IS MEASURED AGAINST A BASELINE. The bucket column records where each
    cell stood when last reviewed. A WORKS cell that goes WRONG is a REGRESSION
    and exits non-zero. A WRONG cell that goes WORKS is PROGRESS -- rerun with
    --accept to move the baseline forward, which is a reviewed change like any
    other.

Usage:
    python3 scripts/robustness_map.py                # report + regression gate
    python3 scripts/robustness_map.py --topic 06     # one topic
    python3 scripts/robustness_map.py --accept       # fold progress into baseline
"""
import argparse, os, pathlib, subprocess, sys, tempfile

ROOT = pathlib.Path(subprocess.run(["git", "rev-parse", "--show-toplevel"],
                                   capture_output=True, text=True,
                                   cwd=pathlib.Path(__file__).parent).stdout.strip())
MAP = ROOT / "tests/fixtures/robustness_map"
GG = ROOT / "target/debug/gg"
JOIN = " / "   # the manifest joins output lines with this; compare like-for-like


def classify(cell: pathlib.Path, expected: str, tmp: pathlib.Path):
    """Build and run one cell. Returns (bucket, actual). rc is read from the
    completed process, never off a pipeline -- a pipeline masks the real rc."""
    exe = tmp / cell.stem
    b = subprocess.run([str(GG), "build", str(cell), "-o", str(exe)],
                       capture_output=True, text=True)
    if b.returncode != 0:
        err = b.stderr
        if "panicked" in err:
            return "ICE", "compiler panic"
        if "error[" in err:
            return "REJECTED", "rejected at check"
        return "BUILD-FAIL", "codegen/link failure"
    r = subprocess.run([str(exe)], capture_output=True, text=True, timeout=30)
    actual = JOIN.join(r.stdout.strip().splitlines())
    if r.returncode != 0:
        # Some cells are SUPPOSED to trap -- divide-by-zero, index-out-of-range,
        # integer overflow. For those the trap IS the expected behaviour, so a
        # clean exit would be the defect. The manifest marks them by describing a
        # loud failure in the expectation rather than giving literal stdout.
        if "loud failure" in expected:
            return ("WORKS", f"trapped rc={r.returncode}") if actual.startswith("'before'") or "before" in actual \
                else ("WRONG", f"trapped rc={r.returncode} but stdout={actual!r}")
        return "TRAP", f"rc={r.returncode}"
    if "loud failure" in expected:
        # Exited 0 where a trap was required: the check silently did not fire.
        return "WRONG", f"NO TRAP (rc=0), stdout={actual!r}"
    return ("WORKS", actual) if actual == expected.strip() else ("WRONG", actual)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--topic", default="")
    ap.add_argument("--accept", action="store_true",
                    help="fold PROGRESS rows into the baseline (never expectations)")
    args = ap.parse_args()

    if not GG.exists():
        sys.exit(f"build the compiler first: {GG} not found")

    rows = [ln.rstrip("\n").split("\t")
            for ln in (MAP / "MANIFEST.tsv").read_text().splitlines()[1:] if ln.strip()]

    topics, regressions, progress, updated = {}, [], [], []
    with tempfile.TemporaryDirectory() as td:
        tmp = pathlib.Path(td)
        for row in rows:
            topic, cell, baseline, expected = row[0], row[1], row[2], row[3]
            if args.topic and not topic.startswith(args.topic):
                updated.append(row); continue
            path = MAP / "cells" / f"{cell}.gg"
            if not path.exists():
                updated.append(row); continue
            bucket, actual = classify(path, expected, tmp)
            # CONTROL cells are deliberately wrong: they prove the harness can see
            # a failure at all. A CONTROL that passes means the harness is blind.
            if baseline == "CONTROL":
                if bucket == "WORKS":
                    regressions.append((cell, "CONTROL PASSED - harness is blind"))
                updated.append(row); continue
            good = {"WORKS"}
            was_good, is_good = baseline in good, bucket in good
            if was_good and not is_good:
                regressions.append((cell, f"{baseline} -> {bucket}: {actual}"))
            elif not was_good and is_good:
                progress.append((cell, f"{baseline} -> {bucket}"))
                if args.accept:
                    row = [topic, cell, bucket] + row[3:]
            t = topics.setdefault(topic, {})
            t[bucket] = t.get(bucket, 0) + 1
            updated.append(row)

    buckets = ["WORKS", "WRONG", "REJECTED", "BUILD-FAIL", "ICE", "TRAP"]
    print(f"{'topic':<40} " + " ".join(f"{b:>10}" for b in buckets))
    tot = {}
    for topic in sorted(topics):
        c = topics[topic]
        print(f"{topic:<40} " + " ".join(f"{c.get(b, 0):>10}" for b in buckets))
        for b in buckets:
            tot[b] = tot.get(b, 0) + c.get(b, 0)
    n = sum(tot.values()) or 1
    print(f"{'TOTAL':<40} " + " ".join(f"{tot.get(b, 0):>10}" for b in buckets))
    print(f"\nWORKS: {tot.get('WORKS', 0)}/{n} = {100 * tot.get('WORKS', 0) / n:.1f}%")

    for cell, why in progress:
        print(f"  PROGRESS   {cell}: {why}")
    for cell, why in regressions:
        print(f"  REGRESSION {cell}: {why}")

    if args.accept and progress:
        hdr = (MAP / "MANIFEST.tsv").read_text().splitlines()[0]
        (MAP / "MANIFEST.tsv").write_text(
            hdr + "\n" + "\n".join("\t".join(r) for r in updated) + "\n")
        print(f"\nbaseline updated for {len(progress)} cell(s) - review this diff")

    if regressions:
        print(f"\n{len(regressions)} REGRESSION(S)")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
