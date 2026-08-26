#!/usr/bin/env python3
"""Regenerate the beginner-code robustness map and diff it against the baseline.

The map answers "how much ordinary, common code actually works", which the main
suite cannot: 2113 fixtures sample what the compiler's own self-host does, not
what a person learning the language writes. Cells are derived from beginner
tutorials in OTHER languages, deliberately not from our own corpus.

Three properties this script exists to preserve:

  * EXPECTATIONS ARE HAND-DERIVED, NEVER CAPTURED. The expected column in
    MANIFEST.tsv was written before the cell was ever run. This script only ever
    READS it. Never "update" an expectation to match what the compiler prints --
    that pins a bug as canonical, which is the worst outcome available here.

  * PROGRESS IS MEASURED AGAINST A BASELINE. The bucket columns record where each
    cell stood on each lane when last reviewed. A WORKS cell that goes WRONG is a
    REGRESSION and exits non-zero. A WRONG cell that goes WORKS is PROGRESS --
    rerun with --accept to move the baseline forward, which is a reviewed change
    like any other.

  * A CROSS-LANE DISAGREEMENT IS ITS OWN FINDING. Three lanes compile the same
    source: the C backend, the LLVM backend, and the self-host frontend. When
    they do not agree on what a program prints, that is a defect NO single-lane
    sweep can see -- and it is reported and gated separately from per-lane
    correctness, because two lanes can be individually "green" against a wrong
    expectation while disagreeing with each other, and two lanes can agree on a
    WRONG answer (AGENTS.md Core #8) while a third gets it right.

Lanes:
    c         `gg build`                       (default; the CI lane)
    llvm      `gg build --backend=llvm`        NB: --sanitize is silently dropped
                                               under --backend=llvm, so this lane
                                               is NOT sanitizer coverage
    selfhost  self-host driver --emit-c | cc   needs the driver built once:
                                               `gg build tests/fixtures/self_host_lowerer/driver.gg`

Usage:
    python3 scripts/robustness_map.py                    # C lane: report + gate
    python3 scripts/robustness_map.py --lanes c,llvm     # two lanes + divergences
    python3 scripts/robustness_map.py --lanes all        # everything
    python3 scripts/robustness_map.py --topic 06         # one topic
    python3 scripts/robustness_map.py --accept           # fold progress into baseline
"""
import argparse, concurrent.futures, os, pathlib, shutil, subprocess, sys, tempfile

ROOT = pathlib.Path(subprocess.run(["git", "rev-parse", "--show-toplevel"],
                                   capture_output=True, text=True,
                                   cwd=pathlib.Path(__file__).parent).stdout.strip())
MAP = ROOT / "tests/fixtures/robustness_map"
GG = ROOT / "target/debug/gg"
DRIVER = ROOT / "tests/fixtures/self_host_lowerer/driver"
JOIN = " / "   # the manifest joins output lines with this; compare like-for-like

# MANIFEST.tsv columns. The first six are the original schema and keep their
# positions, so every existing `cut -f3` / grep over the file still means what it
# meant. Lane baselines are APPENDED; an empty cell means "never measured on that
# lane", which is distinct from "measured and broken".
COL_TOPIC, COL_CELL, COL_C, COL_EXPECTED, COL_ACTUAL, COL_NOTE = range(6)
COL_LLVM, COL_SELFHOST = 6, 7
# Divergence needs a column of its OWN. Deriving it from the lane buckets alone
# cannot work: two lanes that are both WRONG with DIFFERENT values diverge while
# their buckets are identical, so a bucket-derived baseline reports that cell as
# a NEW divergence on every single run. Measured -- 5 cells did exactly that.
COL_DIVERGE = 8
NCOLS = 9
LANE_COL = {"c": COL_C, "llvm": COL_LLVM, "selfhost": COL_SELFHOST}
ALL_LANES = ["c", "llvm", "selfhost"]
BUCKETS = ["WORKS", "WRONG", "REJECTED", "BUILD-FAIL", "ICE", "TRAP"]


def _verdict(expected: str, r, actual: str):
    """Shared run-result adjudication: identical on every lane, so a lane can
    never disagree with another because of how its OUTCOME was read."""
    if r.returncode != 0:
        # Some cells are SUPPOSED to trap -- divide-by-zero, index-out-of-range,
        # integer overflow. For those the trap IS the expected behaviour, so a
        # clean exit would be the defect. The manifest marks them by describing a
        # loud failure in the expectation rather than giving literal stdout.
        if "loud failure" in expected:
            return ("WORKS", f"trapped rc={r.returncode}") if "before" in actual \
                else ("WRONG", f"trapped rc={r.returncode} but stdout={actual!r}")
        return "TRAP", f"rc={r.returncode}"
    if "loud failure" in expected:
        # Exited 0 where a trap was required: the check silently did not fire.
        return "WRONG", f"NO TRAP (rc=0), stdout={actual!r}"
    return ("WORKS", actual) if actual == expected.strip() else ("WRONG", actual)


def _classify_build_failure(err: str):
    """Split "the compiler REFUSED this program" from "the compiler ACCEPTED it and
    then failed to produce a binary". Those are opposite outcomes and must never
    share a bucket: a rejection is a diagnostic doing its job, while a BUILD-FAIL
    is the compiler having said yes and then not delivered -- a miscompile-class
    signal. The discriminator cannot be `error[` alone, because gg codes only its
    SEMANTIC diagnostics (`error[E_TypeMismatch]`); lexer and parser errors are
    uncoded (`error: expected 'case', found '='`) and used to land in BUILD-FAIL,
    which read as codegen breakage. Both end with the parse-error tally."""
    if "panicked" in err:
        return "ICE", "compiler panic"
    if "error[" in err:
        return "REJECTED", "rejected at check"
    if "parse error(s) found" in err:
        return "REJECTED", "rejected at parse"
    return "BUILD-FAIL", "codegen/link failure"


def run_gg(cell: pathlib.Path, expected: str, tmp: pathlib.Path, backend=None):
    """Build and run one cell through `gg`. Returns (bucket, actual). rc is read
    from the completed process, never off a pipeline -- a pipeline masks the real
    rc (AGENTS.md Core #15d, "never read a crash off a PIPELINE")."""
    exe = tmp / cell.stem
    cmd = [str(GG), "build", str(cell), "-o", str(exe)]
    if backend:
        cmd.append(f"--backend={backend}")
    try:
        b = subprocess.run(cmd, capture_output=True, text=True, timeout=300)
    except subprocess.TimeoutExpired:
        return "BUILD-FAIL", "build timed out"
    if b.returncode != 0:
        return _classify_build_failure(b.stderr)
    try:
        r = subprocess.run([str(exe)], capture_output=True, text=True, timeout=30)
    except subprocess.TimeoutExpired:
        return "TRAP", "run timed out"
    return _verdict(expected, r, JOIN.join(r.stdout.strip().splitlines()))


def run_selfhost(cell: pathlib.Path, expected: str, tmp: pathlib.Path):
    """Self-host lane: driver `--emit-c` -> `cc` -> run. Mirrors
    `selfhost_step` (tests/spec_conformance.rs:505) -- same driver, same lib dir,
    same ABSOLUTE --runtime-dir (a relative one only works by cwd luck), same cc
    flags. The driver is built ONCE, out of band, and reused for every cell."""
    stem = cell.stem
    c_path, exe = tmp / f"{stem}.c", tmp / stem
    try:
        e = subprocess.run(
            [str(DRIVER), str(cell), str(ROOT / "lib"), "--emit-c",
             f"--runtime-dir={ROOT / 'src/backend/c/runtime'}"],
            capture_output=True, timeout=300)
    except subprocess.TimeoutExpired:
        return "BUILD-FAIL", "self-host driver timed out"
    if e.returncode != 0:
        return _classify_build_failure(e.stderr.decode("utf-8", "replace"))
    c_path.write_bytes(e.stdout)
    try:
        c = subprocess.run(["cc", "-O0", "-w", "-o", str(exe), str(c_path),
                            "-lm", "-lpthread"], capture_output=True, text=True, timeout=300)
    except subprocess.TimeoutExpired:
        return "BUILD-FAIL", "cc (self-host) timed out"
    if c.returncode != 0:
        # The self-host emitted C that a C compiler refuses. That is never a
        # "rejection" -- the frontend ACCEPTED the program and then produced
        # something unbuildable, which is a miscompile, not a diagnostic.
        return "BUILD-FAIL", "cc rejected self-host C"
    try:
        r = subprocess.run([str(exe)], capture_output=True, text=True, timeout=30)
    except subprocess.TimeoutExpired:
        return "TRAP", "run timed out"
    return _verdict(expected, r, JOIN.join(r.stdout.strip().splitlines()))


LANE_RUNNER = {
    "c": lambda cell, exp, tmp: run_gg(cell, exp, tmp),
    "llvm": lambda cell, exp, tmp: run_gg(cell, exp, tmp, backend="llvm"),
    "selfhost": run_selfhost,
}


def measure(row, lanes, scratch):
    """Run one cell on every requested lane. Each lane gets its OWN scratch dir,
    so two lanes can never collide on an output path -- the self-host fixtures'
    fixed /tmp scratch paths are exactly this bug at a larger scale."""
    cell = MAP / "cells" / f"{row[COL_CELL]}.gg"
    expected = row[COL_EXPECTED]
    out = {}
    for lane in lanes:
        d = scratch / row[COL_CELL] / lane
        d.mkdir(parents=True, exist_ok=True)
        try:
            out[lane] = LANE_RUNNER[lane](cell, expected, d)
        finally:
            shutil.rmtree(d, ignore_errors=True)
    return out


def divergence_key(result):
    """What two lanes must agree on. Bucket AND value: two lanes that are both
    WRONG can be wrong DIFFERENTLY, and that is still a cross-lane defect. The
    error DETAIL is deliberately excluded for non-runnable buckets -- lanes word
    their diagnostics differently and that is not a semantic disagreement."""
    bucket, actual = result
    return bucket if bucket in ("REJECTED", "BUILD-FAIL", "ICE") else (bucket, actual)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--topic", default="")
    ap.add_argument("--lanes", default="c",
                    help="comma-separated: c, llvm, selfhost, or 'all'")
    ap.add_argument("--jobs", type=int, default=min(8, os.cpu_count() or 1))
    ap.add_argument("--detail", action="store_true",
                    help="print every non-WORKS cell with the value it actually produced")
    ap.add_argument("--accept", action="store_true",
                    help="fold PROGRESS rows into the baseline (never expectations)")
    args = ap.parse_args()

    lanes = ALL_LANES if args.lanes == "all" else [l.strip() for l in args.lanes.split(",") if l.strip()]
    for lane in lanes:
        if lane not in LANE_RUNNER:
            sys.exit(f"unknown lane {lane!r}; pick from {', '.join(ALL_LANES)} or 'all'")
    if not GG.exists():
        sys.exit(f"build the compiler first: {GG} not found")
    if "selfhost" in lanes and not DRIVER.exists():
        sys.exit("the self-host lane needs its driver built once:\n"
                 f"  {GG} build tests/fixtures/self_host_lowerer/driver.gg")

    raw = (MAP / "MANIFEST.tsv").read_text().splitlines()
    header = raw[0]
    rows = []
    for ln in raw[1:]:
        if not ln.strip():
            continue
        r = ln.rstrip("\n").split("\t")
        rows.append(r + [""] * (NCOLS - len(r)))   # tolerate the pre-lane schema

    selected = [r for r in rows
                if (not args.topic or r[COL_TOPIC].startswith(args.topic))
                and (MAP / "cells" / f"{r[COL_CELL]}.gg").exists()]

    with tempfile.TemporaryDirectory() as td:
        scratch = pathlib.Path(td)
        with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as pool:
            measured = dict(zip(
                (r[COL_CELL] for r in selected),
                pool.map(lambda r: measure(r, lanes, scratch), selected)))

    topics, regressions, progress, divergences, new_div = {}, [], [], [], []
    for row in rows:
        res = measured.get(row[COL_CELL])
        if res is None:
            continue

        # CONTROL cells are deliberately wrong: they prove the harness can see a
        # failure at all. A CONTROL that passes means the harness is blind -- and
        # it must be blind on NO lane, so every lane is checked.
        if row[COL_C] == "CONTROL":
            for lane, (bucket, _) in res.items():
                if bucket == "WORKS":
                    regressions.append((row[COL_CELL], f"CONTROL PASSED on {lane} - harness is blind"))
            continue

        keys = {lane: divergence_key(r) for lane, r in res.items()}
        diverges = len(set(keys.values())) > 1
        baseline_lanes = {lane: row[LANE_COL[lane]] for lane in lanes}
        # A divergence the baseline already records is a KNOWN one: it stays in
        # the report (that is the point of the category) but does not gate.
        baseline_diverges = (row[COL_DIVERGE] == "DIVERGENT"
                             or len({b for b in baseline_lanes.values() if b}) > 1)
        if diverges:
            divergences.append((row[COL_CELL], res, baseline_diverges))
            if not baseline_diverges:
                new_div.append(row[COL_CELL])

        for lane in lanes:
            bucket, actual = res[lane]
            base = row[LANE_COL[lane]]
            if base:                       # never measured => nothing to regress from
                if base == "WORKS" and bucket != "WORKS":
                    regressions.append((row[COL_CELL], f"[{lane}] {base} -> {bucket}: {actual}"))
                elif base != "WORKS" and bucket == "WORKS":
                    progress.append((row[COL_CELL], f"[{lane}] {base} -> WORKS"))
            if args.accept:
                row[LANE_COL[lane]] = bucket
            topics.setdefault(row[COL_TOPIC], {}).setdefault(lane, {})
            t = topics[row[COL_TOPIC]][lane]
            t[bucket] = t.get(bucket, 0) + 1
        # Only a multi-lane run can say anything about divergence; a single-lane
        # --accept must leave the recorded verdict alone rather than erase it.
        if args.accept and len(lanes) > 1:
            row[COL_DIVERGE] = "DIVERGENT" if diverges else ""

    for lane in lanes:
        print(f"\n=== lane: {lane} ===")
        print(f"{'topic':<52} " + " ".join(f"{b:>10}" for b in BUCKETS))
        tot = {}
        for topic in sorted(topics):
            c = topics[topic].get(lane, {})
            print(f"{topic:<52} " + " ".join(f"{c.get(b, 0):>10}" for b in BUCKETS))
            for b in BUCKETS:
                tot[b] = tot.get(b, 0) + c.get(b, 0)
        n = sum(tot.values()) or 1
        print(f"{'TOTAL':<52} " + " ".join(f"{tot.get(b, 0):>10}" for b in BUCKETS))
        print(f"WORKS: {tot.get('WORKS', 0)}/{n} = {100 * tot.get('WORKS', 0) / n:.1f}%")

    if len(lanes) > 1:
        print(f"\n=== cross-lane divergences: {len(divergences)} "
              f"({len(new_div)} NOT in the baseline) ===")
        for cell, res, known in sorted(divergences):
            tag = "known" if known else "NEW"
            detail = " | ".join(f"{lane}={res[lane][0]}:{res[lane][1][:60]}" for lane in lanes)
            print(f"  [{tag}] {cell}: {detail}")

    if args.detail:
        for row in rows:
            res = measured.get(row[COL_CELL])
            if res is None or row[COL_C] == "CONTROL":
                continue
            for lane in lanes:
                bucket, actual = res[lane]
                if bucket != "WORKS":
                    print(f"  {bucket:<10} [{lane}] {row[COL_CELL]}: got {actual!r} "
                          f"want {row[COL_EXPECTED]!r}")

    for cell, why in progress:
        print(f"  PROGRESS   {cell}: {why}")
    for cell, why in regressions:
        print(f"  REGRESSION {cell}: {why}")

    if args.accept:
        (MAP / "MANIFEST.tsv").write_text(
            header + "\n" + "\n".join("\t".join(r) for r in rows) + "\n")
        print(f"\nbaseline updated ({len(progress)} progress, {len(new_div)} new "
              f"divergence) - review this diff")

    if regressions or new_div:
        print(f"\n{len(regressions)} REGRESSION(S), {len(new_div)} NEW DIVERGENCE(S)")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
