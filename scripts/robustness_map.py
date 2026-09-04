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

Two of the five lanes exist because a VALUE lane is structurally blind to whole
failure classes:

  * THE SANITIZER LANE SEES MEMORY VALIDITY, WHICH NO VALUE LANE CAN.  A cell
    that prints the right answer while double-freeing, leaking, or reading
    uninitialised memory scores WORKS on every value lane -- the program's
    stdout is the only observable they have. `asan` rebuilds the same cell with
    `gg build --sanitize` (`-fsanitize=address,undefined`), runs it under
    `detect_leaks=1`, and buckets any ASan/LSan/UBSan report as SANITIZE-FAIL,
    which is a DIFFERENT outcome from a wrong value and gets its own column.
    NB THIS LANE IS C-ONLY AND CLAIMS NO LLVM SANITIZER COVERAGE WHATSOEVER --
    it builds with `--sanitize` and NO `--backend`, so it gets the default
    backend by construction. (`--sanitize` used to be silently dropped under
    `--backend=llvm` as well, which is fixed; the flag now works there. That
    changes nothing here, because this lane never passes it. And LLVM sanitizer
    coverage is partial even now -- generated user code is not instrumented on
    that backend, only the runtime -- so pointing this lane at it would not buy
    equivalent coverage anyway.)

  * THE ggdef LANE SEES CORRECTNESS, WHICH LANE AGREEMENT CANNOT.  ggdef is the
    definitional interpreter -- the executable language definition. Where three
    production lanes agreeing only proves they share an implementation, ggdef
    adjudicates against the DEFINITION, so it can catch the case all three real
    lanes get wrong together (AGENTS.md Core #8's trap). That case is reported
    under its own heading, BOTH-LANES-WRONG-ggdef-RIGHT. ggdef implements a
    SUBSET (GGC): a cell it declines to elaborate has NO ggdef verdict, recorded
    as NO-VERDICT -- which is emphatically not "ggdef agrees". ggdef is also not
    infallible: it IMPLEMENTS the definition, it is not the definition, so a
    disagreement is a finding to triage, not an automatic verdict against the
    compiler.

Lanes:
    c         `gg build`                       (default; the CI lane)
    llvm      `gg build --backend=llvm`        NB: this lane does not pass
                                               --sanitize, so it is NOT
                                               sanitizer coverage
    selfhost  self-host driver --emit-c | cc   needs the driver built once:
                                               `gg build tests/fixtures/self_host_lowerer/driver.gg`
    asan      `gg build --sanitize`            C backend only; memory-validity,
                                               not value. Adds SANITIZE-FAIL.
    ggdef     `ggdef run`                      the definitional oracle; adds
                                               NO-VERDICT for out-of-subset cells.
                                               Needs `cargo build -p ggdef`.

Only c/llvm/selfhost participate in the cross-lane divergence gate: `asan` is the
same compiler as `c` with different cc flags (it would report a divergence for
every SANITIZE-FAIL, which is not a cross-lane semantic disagreement), and ggdef
is the oracle rather than a peer -- its disagreements get their own section.

Usage:
    python3 scripts/robustness_map.py                    # C lane: report + gate
    python3 scripts/robustness_map.py --lanes c,llvm     # two lanes + divergences
    python3 scripts/robustness_map.py --lanes c,asan     # + the sanitizer lane
    python3 scripts/robustness_map.py --lanes c,ggdef    # + the definitional oracle
    python3 scripts/robustness_map.py --lanes all        # everything
    python3 scripts/robustness_map.py --topic 06         # one topic
    python3 scripts/robustness_map.py --accept           # fold progress into baseline
"""
import argparse, concurrent.futures, os, pathlib, shutil, subprocess, sys, tempfile

# THE verdict classifier, shelled in as a module rather than reimplemented.
# One definition of "what happened when we ran this", shared with
# scripts/sanitize_sweep.sh and tests/lints.rs -- three copies of a marker
# set and an exit-code table cannot catch their own divergence, and this
# file used to hold one of the copies that was wrong.
sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import proc_guard  # noqa: E402  (path must be set first)
import verdict  # noqa: E402

ROOT = pathlib.Path(subprocess.run(["git", "rev-parse", "--show-toplevel"],
                                   capture_output=True, text=True,
                                   cwd=pathlib.Path(__file__).parent).stdout.strip())
MAP = ROOT / "tests/fixtures/robustness_map"
GG = ROOT / "target/debug/gg"
GGDEF = ROOT / "target/debug/ggdef"
DRIVER = ROOT / "tests/fixtures/self_host_lowerer/driver"
JOIN = " / "   # the manifest joins output lines with this; compare like-for-like

# Runtime sanitizer configuration for the `asan` lane. Mirrors
# `ASAN_OPTS_LEAK_CHECK` (tests/security.rs) -- deliberately the SAME options the
# `security_safe_no_leak` guard uses, so a cell that trips here trips there.
# `detect_leaks=1` is the point: a leak is invisible to every value lane, and
# `exitcode=99` makes a leak-only trip (which LSan reports at process EXIT, after
# stdout is already correct) distinguishable from the program's own exit code.
ASAN_OPTIONS = ("detect_leaks=1:halt_on_error=1:abort_on_error=0:print_summary=1"
                ":allocator_may_return_null=1:exitcode=99")
# What a sanitizer report LOOKS like on stderr is NOT decided here any more: it
# is `scripts/verdict.py`'s sanitizer axis. UBSan does NOT abort by default (it
# is a -fsanitize-recover class), so it can only be seen by READING stderr -- an
# exit-code-only check misses the entire undefined-behaviour half of the lane,
# and that reasoning now lives with the markers instead of beside a second copy
# of them.
#
# The `exitcode=99` above is passed to the classifier as an INPUT
# (`sanitizer_exitcode=`), never assumed: sanitize_sweep.sh uses 0 for the same
# runs, and a classifier that hardcodes either is wrong under the other.
SANITIZER_EXITCODE = 99

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
# Appended AFTER the divergence column, again so every existing `cut -fN` keeps
# meaning what it meant. Same "empty means never measured" rule as the lane
# columns above -- and for ggdef, "never measured" is distinct from the recorded
# verdict NO-VERDICT, which means "measured, and ggdef declined the program".
COL_ASAN, COL_GGDEF = 9, 10
NCOLS = 11
LANE_COL = {"c": COL_C, "llvm": COL_LLVM, "selfhost": COL_SELFHOST,
            "asan": COL_ASAN, "ggdef": COL_GGDEF}
ALL_LANES = ["c", "llvm", "selfhost", "asan", "ggdef"]
# Named hang census. A hang/spin/timeout is a ROW, never a shrinking integer
# (`EXPECTED_HANGS` does not exist in this script). C-lane TIMEOUT must equal
# this set on a C-lane run; a new hang is a REGRESSION, a retirement must
# shrink this tuple in the same commit. Filed: todo/t0015, todo/t0863 sixth.
HANG_CENSUS = frozenset({
    "doc_lr_meta_while_type_guard",
})
# The lanes that answer "what does this program print". Divergence is defined
# over THESE only -- see the module docstring.
VALUE_LANES = ["c", "llvm", "selfhost"]
# CRASH, TIMEOUT and UNKNOWN are NEW, and their absence was a live false-green:
#   * with no CRASH cell, a run that died on SIGSEGV fell through `returncode
#     != 0` and, on a cell whose expectation mentions a loud failure, graded
#     WORKS. A rc-139 crash published as a pass, in a round-close gate.
#   * with no TIMEOUT cell, a hung RUN graded TRAP and a hung BUILD graded
#     BUILD-FAIL -- a hang recorded as the miscompile-class signal, against
#     AGENTS.md's "every hang/spin/timeout gets root-caused into a census row,
#     never merely killed".
#   * with no UNKNOWN cell there was nowhere to put an outcome the tree cannot
#     discriminate, so `_classify_build_failure` guessed BUILD-FAIL by
#     fall-through. A guess in a baseline is worse than a gap in one.
BUCKETS = ["WORKS", "WRONG", "REJECTED", "BUILD-FAIL", "ICE", "TRAP", "CRASH",
           "TIMEOUT", "SANITIZE-FAIL", "UNKNOWN", "NO-VERDICT"]

# scripts/verdict.py's canonical labels -> this map's bucket vocabulary, which is
# deliberately COARSER. The classifier never guesses; the map records the coarser
# outcome and keeps the classifier's own words in the detail column, so an
# ambiguity is visible in the report rather than laundered into a bucket.
#
# ⚠ TRAP absorbs the run-phase rc-1 AMBIGUITY CELL (todo/t0647: the program's own
# `exit(1)` and `gorget_panic_at`'s `exit(1)` are indistinguishable from
# outside). That is a COARSENING of a stated ambiguity, not a manufactured
# discriminator: both readings are "ran, then failed loudly", which is exactly
# what this bucket has always meant here, and the detail column says so.
VERDICT_TO_BUCKET = {
    "CLEAN": None,            # caller compares stdout: WORKS or WRONG
    "TRAP": "TRAP",
    "CRASH": "CRASH",
    "TIMEOUT": "TIMEOUT",
    "RUNNER_FAIL": "UNKNOWN",  # our plumbing broke: the measurement is void
    "CORRUPT": "SANITIZE-FAIL",
    "LEAK": "SANITIZE-FAIL",
    "UB": "SANITIZE-FAIL",
    "ICE": "ICE",
    "REJECTED": "REJECTED",
    "BUILD_FAIL": "BUILD-FAIL",
    "USAGE": "UNKNOWN",
    "CHANNEL_ERROR": "TRAP",
    "FUEL": "NO-VERDICT",
    # The program chose a nonzero exit code. Not a fault, and not a clean run
    # either: the map compares STDOUT, so a deliberate `exit(7)` is adjudicated
    # exactly like any other run — it just is not a CRASH.
    "EXIT": "TRAP",
    "UNKNOWN": "UNKNOWN",
}


def _verdict(expected: str, r, actual: str, sanitized=False, timed_out=False,
             subject="program"):
    """Shared run-result adjudication: identical on every lane, so a lane can
    never disagree with another because of how its OUTCOME was read.

    ⚠ THE DEFECT THIS REPLACED, AND WHY IT MATTERS MORE THAN IT LOOKS. The old
    body read `r.returncode != 0` and nothing else, so on any cell whose
    expectation mentions a loud failure, ANY nonzero exit plus the right stdout
    prefix graded WORKS -- including **rc 139, a SIGSEGV**. This file is a
    round-close gate, so that was a segfault publishing as green.

    The rule that retires it is ratified, not invented: the toolchain exit-code
    taxonomy (docs/define-gorget/decisions.md:2092-2070) is a TOTAL enumeration,
    so `rc not in {0,1,2,101,102,103}` is off-taxonomy and can NEVER be WORKS.
    `scripts/verdict.py` owns that rule; this function only decides what the
    map's coarser bucket vocabulary calls the result.
    """
    v = verdict.findings_for("run", r.returncode, stderr=r.stderr or "",
                             stdout=r.stdout or "", timed_out=timed_out, subject=subject,
                             sanitizer_exitcode=SANITIZER_EXITCODE if sanitized else None)
    base = v.verdict.split(":", 1)[0]
    bucket = VERDICT_TO_BUCKET[base]

    if bucket is None:                              # a genuinely clean run
        if "loud failure" in expected:
            # Exited 0 where a trap was required: the check silently did not fire.
            return "WRONG", f"NO TRAP (rc=0), stdout={actual!r}"
        return ("WORKS", actual) if actual == expected.strip() else ("WRONG", actual)

    # Some cells are SUPPOSED to fail loudly -- divide-by-zero,
    # index-out-of-range, integer overflow. For those the trap IS the expected
    # behaviour, so a clean exit would be the defect; the manifest marks them by
    # describing a loud failure in the expectation rather than giving literal
    # stdout.
    #
    # ⚠ A CRASH or a TIMEOUT never satisfies that expectation. "Fails loudly"
    # means the language's own diagnostic fired, not that the process died
    # however it liked: a SIGSEGV is not a trap, and a hang is not a failure
    # mode a program can be said to have chosen. THIS is the rule that retires
    # the rc-139-grades-WORKS false-green, and it is the whole point of the
    # CRASH and TIMEOUT columns existing.
    #
    # ⚠ But run-phase rc 1 DOES. The classifier calls it ambiguous, correctly --
    # from outside, the program's own `exit(1)` and `gorget_panic_at`'s exit(1)
    # are indistinguishable (todo/t0647). The MANIFEST resolves what the process
    # alone cannot: a cell whose hand-written expectation says "a loud failure"
    # has declared its intent, and the ledger RATIFIES that reading --
    # docs/define-gorget/decisions.md:2090, "`main throws int`'s escaping int
    # KEEPS the exit-code idiom (the user chose the exit contract)". So the
    # ambiguity is resolved by an INPUT the classifier does not have, not by the
    # classifier guessing. Every other UNKNOWN stays UNKNOWN.
    ambiguous_run_rc1 = v.ambiguity is not None and v.ambiguity[0] == "run_rc1"
    detail = f"{v.verdict} rc={r.returncode}"
    if bucket in ("CRASH", "TIMEOUT") or (bucket == "UNKNOWN" and not ambiguous_run_rc1):
        return bucket, f"{detail}: {v.detail.get(v.verdict, '')[:120]}"
    if "loud failure" in expected:
        return ("WORKS", detail) if "before" in actual \
            else ("WRONG", f"{detail} but stdout={actual!r}")
    return ("TRAP" if ambiguous_run_rc1 else bucket), detail


def _classify_build_failure(err: str, rc: int = 1):
    """Split "the compiler REFUSED this program" from "the compiler ACCEPTED it and
    then failed to produce a binary". Those are opposite outcomes and must never
    share a bucket: a rejection is a diagnostic doing its job, while a BUILD-FAIL
    is the compiler having said yes and then not delivered -- a miscompile-class
    signal. The discriminator cannot be `error[` alone, because gg codes only its
    SEMANTIC diagnostics (`error[E_TypeMismatch]`); lexer and parser errors are
    uncoded (`error: expected 'case', found '='`) and used to land in BUILD-FAIL,
    which read as codegen breakage. Both end with the parse-error tally.

    ⚠ WHAT CHANGED, AND WHY IT IS NOT A LOSS OF SIGNAL. This used to END with
    `return "BUILD-FAIL", "codegen/link failure"` -- a DEFAULT SINK, so anything
    that did not match the three markers was ASSERTED to be a codegen failure.
    The split itself is right and is kept; what is gone is the guess. gg prints a
    positive marker on the delivery path (`C compiler exited with:` from the C
    backend, `Linking failed:` from LLVM), so BUILD-FAIL is now POSITIVELY
    matched, and a build failure with none of the markers is UNKNOWN instead of
    being asserted to be something it might not be.
    """
    v = verdict.findings_for("build", rc, stderr=err or "")
    base = v.verdict.split(":", 1)[0]
    bucket = VERDICT_TO_BUCKET[base] or "UNKNOWN"
    why = v.detail.get(v.verdict, "")
    return bucket, f"{base.lower().replace('_', ' ')}: {why[:120]}"


def run_gg(cell: pathlib.Path, expected: str, tmp: pathlib.Path, backend=None):
    """Build and run one cell through `gg`. Returns (bucket, actual). rc is read
    from the completed process, never off a pipeline -- a pipeline masks the real
    rc (AGENTS.md Core #15d, "never read a crash off a PIPELINE")."""
    exe = tmp / cell.stem
    cmd = [str(GG), "build", str(cell), "-o", str(exe)]
    if backend:
        cmd.append(f"--backend={backend}")
    try:
        b = proc_guard.run(cmd, timeout=300)
        if b.timed_out:
            return "TIMEOUT", "build timed out"
    except OSError as e:
        return "UNKNOWN", f"could not spawn gg: {e}"
    if b.returncode != 0:
        return _classify_build_failure(b.stderr, b.returncode)
    try:
        r = proc_guard.run([str(exe)], timeout=30)
        if r.timed_out:
            return "TIMEOUT", "run timed out"
    except OSError as e:
        return "UNKNOWN", f"could not spawn the built binary: {e}"
    return _verdict(expected, r, JOIN.join(r.stdout.strip().splitlines()))


def run_selfhost(cell: pathlib.Path, expected: str, tmp: pathlib.Path):
    """Self-host lane: driver `--emit-c` -> `cc` -> run. Mirrors
    `selfhost_step` in tests/spec_conformance.rs (find it with
    `grep -n "fn selfhost_step" tests/spec_conformance.rs`; the line number is
    deliberately not quoted -- it was cited as :505 and the function had moved to
    :497) -- same driver, same lib dir,
    same ABSOLUTE --runtime-dir (a relative one only works by cwd luck), same cc
    flags. The driver is built ONCE, out of band, and reused for every cell."""
    stem = cell.stem
    c_path, exe = tmp / f"{stem}.c", tmp / stem
    try:
        e = proc_guard.run(
            [str(DRIVER), str(cell), str(ROOT / "lib"), "--emit-c",
             f"--runtime-dir={ROOT / 'src/backend/c/runtime'}"],
            timeout=300, text=False)
        if e.timed_out:
            return "TIMEOUT", "self-host driver timed out"
    except OSError as ex:
        return "UNKNOWN", f"could not spawn the self-host driver: {ex}"
    if e.returncode != 0:
        return _classify_build_failure(e.stderr.decode("utf-8", "replace"), e.returncode)
    c_path.write_bytes(e.stdout)
    try:
        c = proc_guard.run(["cc", "-O0", "-w", "-o", str(exe), str(c_path),
                            "-lm", "-lpthread"], timeout=300)
        if c.timed_out:
            return "TIMEOUT", "cc (self-host) timed out"
    except OSError as ex:
        return "UNKNOWN", f"could not spawn cc: {ex}"
    if c.returncode != 0:
        # The self-host emitted C that a C compiler refuses. That is never a
        # "rejection" -- the frontend ACCEPTED the program and then produced
        # something unbuildable, which is a miscompile, not a diagnostic.
        return "BUILD-FAIL", "cc rejected self-host C"
    try:
        r = proc_guard.run([str(exe)], timeout=30)
        if r.timed_out:
            return "TIMEOUT", "run timed out"
    except OSError as ex:
        return "UNKNOWN", f"could not spawn the self-host binary: {ex}"
    return _verdict(expected, r, JOIN.join(r.stdout.strip().splitlines()))


def run_asan(cell: pathlib.Path, expected: str, tmp: pathlib.Path):
    """Sanitizer lane: `gg build --sanitize` -> run under `detect_leaks=1`.

    THE POINT OF THIS LANE is that the value lanes cannot see memory validity.
    A double free, a leak, or a read of uninitialised memory can all coexist with
    perfectly correct stdout -- and did: a Box `.clone()` on a dead owned param
    double-frees while printing 17, and `[b for b in s.bytes()]` prints a WRONG
    number at rc 0 with no crash at all.

    A sanitizer report is its OWN bucket, never folded into WRONG. They are
    different findings: WRONG means the program computed the wrong answer,
    SANITIZE-FAIL means the program's MEMORY BEHAVIOUR is invalid regardless of
    what it computed -- and the second is usually the more serious, because it is
    the one that turns into a crash on someone else's allocator.

    The report is read off STDERR, not off the exit code. UBSan does not abort by
    default, so a `-fsanitize=undefined` trip exits 0 with a `runtime error:` line
    and nothing else to see; an exit-code-only check is blind to it."""
    exe = tmp / cell.stem
    try:
        b = proc_guard.run([str(GG), "build", str(cell), "--sanitize", "-o", str(exe)],
                           timeout=300)
        if b.timed_out:
            return "TIMEOUT", "sanitize build timed out"
    except OSError as ex:
        return "UNKNOWN", f"could not spawn gg --sanitize: {ex}"
    if b.returncode != 0:
        return _classify_build_failure(b.stderr, b.returncode)
    env = dict(os.environ, ASAN_OPTIONS=ASAN_OPTIONS)
    try:
        r = proc_guard.run([str(exe)], timeout=120, env=env)
        if r.timed_out:
            return "TIMEOUT", "run timed out"
    except OSError as ex:
        return "UNKNOWN", f"could not spawn the sanitized binary: {ex}"
    # ⚠ This used to report the FIRST marker line. It is the MOST SEVERE finding
    # that names the defect: for a sanitized null deref the UBSan `runtime
    # error:` line PRECEDES the ASan report, so first-match under-graded it to
    # "undefined behaviour" when ASan had already called it a SEGV. The full
    # finding SET is reported too, so a run that both leaks and traps does not
    # lose the leak to the headline.
    v = verdict.findings_for("run", r.returncode, stderr=r.stderr or "",
                             stdout=r.stdout or "",
                             sanitizer_exitcode=SANITIZER_EXITCODE)
    if any(f.split(":", 1)[0] in ("CORRUPT", "LEAK", "UB") for f in v.findings):
        return "SANITIZE-FAIL", f"{'+'.join(v.findings)} rc={r.returncode}"
    return _verdict(expected, r, JOIN.join(r.stdout.strip().splitlines()),
                    sanitized=True)


def run_ggdef(cell: pathlib.Path, expected: str, tmp: pathlib.Path):
    """Definitional-oracle lane: `ggdef run`.

    ggdef's exit codes are the ratified toolchain scheme (spec/ggdef/src/main.rs):
    0 value, 1 static rejection, 2 usage, 101 trap, 103 fuel. Two of those need
    splitting apart, because they answer opposite questions:

      * exit 1 with `elaboration error` is ggdef DECLINING the program -- the
        construct is outside GGC, the definitional subset. That is NO-VERDICT.
        Scoring it REJECTED would be the worst available outcome here: it would
        read on the dashboard as "the definition rejects this program", which is
        a claim ggdef never made.
      * exit 1 with a parse error or an `error[E_Code]` IS a verdict. ggdef shares
        the PRODUCTION lexer/parser (its Cargo.toml path-deps `gorget` for exactly
        that), so a parse rejection is the same rejection gg makes; and an
        `error[E_Code]` is the ratified may-move static rejection.

    `tmp` is unused -- ggdef interprets, so there is nothing to emit."""
    del tmp
    try:
        r = proc_guard.run([str(GGDEF), "run", str(cell)], timeout=120)
        if r.timed_out:
            return "TIMEOUT", "ggdef timed out"
    except OSError as ex:
        return "UNKNOWN", f"could not spawn ggdef: {ex}"
    if r.returncode == 103:
        return "NO-VERDICT", "fuel exhausted (ggdef totality guard, not a language outcome)"
    if r.returncode == 2:
        return "NO-VERDICT", "ggdef usage error"
    if r.returncode == 1:
        if "elaboration error" in r.stderr:
            detail = r.stderr.strip().splitlines()[-1] if r.stderr.strip() else ""
            return "NO-VERDICT", f"out of GGC subset: {detail[:140]}"
        if "parse error(s)" in r.stderr:
            return "REJECTED", "rejected at parse"
        if "error[" in r.stderr:
            return "REJECTED", "rejected at check"
        return "REJECTED", "rejected (uncoded)"
    # ⚠ SUBJECT = TOOLCHAIN. `ggdef run <cell>` is a compiler invocation spelled
    # as a run: the exit code is ggdef's, and ggdef IS bound by the ratified
    # taxonomy (its own header says so). Everywhere else on this map the subject
    # is the user's compiled program, whose small-int band is the USER's exit API.
    return _verdict(expected, r, JOIN.join(r.stdout.strip().splitlines()),
                    subject="toolchain")


LANE_RUNNER = {
    "c": lambda cell, exp, tmp: run_gg(cell, exp, tmp),
    "llvm": lambda cell, exp, tmp: run_gg(cell, exp, tmp, backend="llvm"),
    "selfhost": run_selfhost,
    "asan": run_asan,
    "ggdef": run_ggdef,
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


def oracle_key(result):
    """`divergence_key`, but WORKS collapses to a single key. Used ONLY for the
    ggdef comparison. Two lanes that are both WORKS matched the SAME hand-derived
    expectation, so they agree by construction -- but their `actual` strings can
    still differ in shape (a trap-expected cell records `trapped rc=101` on one
    lane and `trapped rc=134` on another, both correctly WORKS). Feeding that to
    the strict key would report a disagreement about the exit code of a cell whose
    expectation is "a loud failure", which is not a disagreement about anything."""
    return "WORKS" if result[0] == "WORKS" else divergence_key(result)


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
                    help="fold PROGRESS rows into the baseline (never expectations); "
                         "refuses to write MANIFEST.tsv if any REGRESSION or NEW "
                         "divergence is present")
    ap.add_argument("--accept-drift", action="store_true", dest="accept_drift",
                    help="ALSO record intra-quadrant DRIFT rows (non-good -> a "
                         "DIFFERENT non-good). Separate from --accept on purpose: "
                         "a drift can be a SEVERITY ESCALATION (WRONG -> CRASH), and "
                         "recording one must be a deliberate act after triage, never "
                         "a side effect of folding progress. Same refusal rules.")
    args = ap.parse_args()
    if args.accept_drift and not args.accept:
        ap.error("--accept-drift requires --accept (it widens what --accept writes)")

    lanes = ALL_LANES if args.lanes == "all" else [l.strip() for l in args.lanes.split(",") if l.strip()]
    for lane in lanes:
        if lane not in LANE_RUNNER:
            sys.exit(f"unknown lane {lane!r}; pick from {', '.join(ALL_LANES)} or 'all'")
    if not GG.exists():
        sys.exit(f"build the compiler first: {GG} not found")
    if "selfhost" in lanes and not DRIVER.exists():
        sys.exit("the self-host lane needs its driver built once:\n"
                 f"  {GG} build tests/fixtures/self_host_lowerer/driver.gg")
    if "ggdef" in lanes and not GGDEF.exists():
        sys.exit("the ggdef lane needs the definitional interpreter built once:\n"
                 "  cargo build -p ggdef --bin ggdef")

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

    # Divergence is a question about VALUE lanes only (module docstring).
    div_lanes = [l for l in lanes if l in VALUE_LANES]
    topics, regressions, progress, divergences, new_div = {}, [], [], [], []
    drifts = []
    both_wrong_ggdef_right, ggdef_disagree = [], []
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

        keys = {lane: divergence_key(res[lane]) for lane in div_lanes}
        diverges = len(set(keys.values())) > 1
        baseline_lanes = {lane: row[LANE_COL[lane]] for lane in div_lanes}
        # A divergence the baseline already records is a KNOWN one: it stays in
        # the report (that is the point of the category) but does not gate.
        baseline_diverges = (row[COL_DIVERGE] == "DIVERGENT"
                             or len({b for b in baseline_lanes.values() if b}) > 1)
        if diverges:
            divergences.append((row[COL_CELL], res, baseline_diverges))
            if not baseline_diverges:
                new_div.append(row[COL_CELL])

        # ggdef adjudication. Two categories, and the difference matters:
        #
        #   BOTH-LANES-WRONG-ggdef-RIGHT -- every production lane measured on this
        #     run is non-WORKS and the DEFINITION gets it right. This is the case
        #     lane agreement structurally cannot find (Core #8): the real lanes can
        #     be unanimous and unanimously wrong. Measured, not hypothetical:
        #     `auto v = []` + `push(String)` prints a raw pointer on C AND LLVM
        #     while ggdef prints the string.
        #   ggdef-DISAGREES -- ggdef and a production lane reach different verdicts
        #     in any other configuration. Triage material, NOT a verdict: ggdef
        #     IMPLEMENTS the definition, it is not the definition, so it can lag a
        #     ratified decision or simply be wrong.
        #
        # NO-VERDICT rows are excluded from both -- ggdef declining a program says
        # nothing at all about that program.
        if "ggdef" in lanes and div_lanes:
            g_bucket, g_actual = res["ggdef"]
            if g_bucket != "NO-VERDICT":
                prod = {lane: res[lane] for lane in div_lanes}
                if g_bucket == "WORKS" and all(b != "WORKS" for b, _ in prod.values()):
                    both_wrong_ggdef_right.append((row[COL_CELL], prod, g_actual))
                elif any(oracle_key(r) != oracle_key(res["ggdef"])
                         for r in prod.values()):
                    ggdef_disagree.append((row[COL_CELL], prod, (g_bucket, g_actual)))

        for lane in lanes:
            bucket, actual = res[lane]
            base = row[LANE_COL[lane]]
            # A cell whose COL_EXPECTED starts with REJECTED asserts a
            # REJECTION: the documentation says the program is a compile error,
            # so REJECTED is its CORRECT state and WORKS is the regression --
            # the language got more permissive and the doc it mirrors now lies.
            #
            # The marker is COL_EXPECTED, never the C-lane baseline bucket and
            # never a `_neg` filename suffix.
            #   * Keying on COL_C made a C-lane FIX of a value-expected
            #     REJECTED cell score as a REGRESSION (the baseline recorded
            #     what the compiler printed -- the thing "don't redesign
            #     around compiler gaps" forbids). Measured: `vec_pop`
            #     (expected `30 / 2 / empty`, C=REJECTED, self-host WORKS).
            #   * Keying on `_neg` is name-matching to decide semantics, and
            #     it is measurably wrong: `vars_unary_neg` is a cell about
            #     unary MINUS whose correct state is WORKS.
            # COL_EXPECTED.startswith("REJECTED") is the column's existing
            # encoding (16 cells, not the 14 that currently also have
            # C=REJECTED -- `doc_b06_struct_eq_no_derive` and
            # `doc_b12_borrow_conflict_shared_plus_mut_neg` are C=WRONG).
            good = "REJECTED" if row[COL_EXPECTED].startswith("REJECTED") else "WORKS"
            drifted = False
            if base:                       # never measured => nothing to regress from
                if base == good and bucket != good:
                    regressions.append((row[COL_CELL], f"[{lane}] {base} -> {bucket}: {actual}"))
                elif base != good and bucket == good:
                    progress.append((row[COL_CELL], f"[{lane}] {base} -> {good}"))
                elif base != bucket:
                    # THE THIRD QUADRANT: broken before, broken after, broken
                    # DIFFERENTLY. Until this branch existed the scorer had
                    # exactly two, so BUILD-FAIL -> SANITIZE-FAIL, BUILD-FAIL ->
                    # WRONG and WRONG -> CRASH all scored as NOTHING AT ALL --
                    # and a whole class of real change was invisible to a
                    # round-close gate. (Measured on this very round: fixing the
                    # closure-shape carrier moves `hof_reduce_strings_untyped`
                    # from ICE to BUILD-FAIL, exposing a second defect
                    # underneath the first. Nothing in the old scorer could see
                    # that happen.)
                    #
                    # ⚠ REPORT-ONLY, DELIBERATELY, AND THIS IS STAGE ONE OF A
                    # RATCHET (Core #6, devbook/25). 673 cell-lanes sit at a
                    # non-good baseline today and none has ever been checked for
                    # intra-quadrant drift, so making this fatal in one step
                    # would red the map on drift nobody caused. The staging is:
                    # report -> measure the whole map -> burn the census down ->
                    # THEN promote to `regressions`. Do not promote it before
                    # the census is owned, and do not leave it report-only
                    # forever either -- a ratchet needs both directions.
                    drifts.append((row[COL_CELL], f"[{lane}] {base} -> {bucket}: {actual}"))
                    drifted = True
            if (args.accept and bucket == good and base != good) or \
               (args.accept_drift and drifted):
                # PROGRESS folds under `--accept`. DRIFT folds ONLY under the
                # separate `--accept_drift`, and the separation is the point.
                #
                # Nothing else can write a non-good bucket, so without SOME way
                # to record a drift the first DRIFT block is permanent
                # unclearable noise. But a drift is not a progress row: `WRONG ->
                # CRASH` and `TRAP -> CRASH:sig11` are SEVERITY ESCALATIONS, and
                # folding those into the baseline as a side effect of a routine
                # `--accept` would ratchet a segfault into the record while the
                # operator was reading a "6 progress rows folded" summary. The
                # flag has to be typed deliberately, and it prints what it did.
                #
                # A REGRESSION still stays in the in-memory row as the old
                # baseline; the file write below is refused when any regression
                # or new divergence is present.
                row[LANE_COL[lane]] = bucket
            topics.setdefault(row[COL_TOPIC], {}).setdefault(lane, {})
            t = topics[row[COL_TOPIC]][lane]
            t[bucket] = t.get(bucket, 0) + 1
        # Only a run that measured EVERY value lane can retire a divergence; a
        # single-lane --accept (or a `c,asan` run, which has one value lane) must
        # leave the recorded verdict alone rather than erase it.
        # Fold PROGRESS only: clear a resolved known divergence. Never write a
        # NEW DIVERGENT flag here -- those are `new_div` and refuse the file.
        #
        # ⚠ THE GUARD IS `== len(VALUE_LANES)`, NOT `> 1`, AND THE DIFFERENCE IS
        # 137 ERASED ROWS. `--lanes c,llvm --accept` has two value lanes, so it
        # passed a `> 1` test -- and then cleared DIVERGENT on every row whose
        # divergence is against the THIRD lane it never ran. Measured: 137 rows,
        # every one of them `c`/`llvm` in agreement and `selfhost` disagreeing
        # (`str_slice_colon` selfhost=WRONG, `func_closure_reassign`
        # selfhost=WORKS -- the succession-plan finding). `diverges` is computed
        # over `div_lanes` alone, so an unmeasured lane's disagreement is
        # invisible to it and reads as "resolved". That is exactly the erasure
        # the paragraph above forbids; the old condition just did not implement
        # its own comment.
        if args.accept and len(div_lanes) == len(VALUE_LANES):
            if row[COL_DIVERGE] == "DIVERGENT" and not diverges:
                row[COL_DIVERGE] = ""

    # Hang census (C lane). EXPECTED_HANGS as a shrinking integer does not
    # exist here: a hang is a named row. Both-asserts so a retirement shrinks
    # HANG_CENSUS in the same commit and a new hang cannot land silent.
    if "c" in lanes:
        measured_hangs = {
            row[COL_CELL]
            for row in selected
            if measured[row[COL_CELL]]["c"][0] == "TIMEOUT"
        }
        expected_hangs = HANG_CENSUS & {row[COL_CELL] for row in selected}
        for cell in sorted(measured_hangs - expected_hangs):
            regressions.append((cell, "[c] NEW HANG (TIMEOUT) — add a HANG_CENSUS row"))
        for cell in sorted(expected_hangs - measured_hangs):
            regressions.append(
                (cell, "[c] hang retired — remove it from HANG_CENSUS in this commit")
            )

    missing_hang_rows = HANG_CENSUS - {r[COL_CELL] for r in rows}
    if missing_hang_rows:
        sys.exit(f"HANG_CENSUS names not in MANIFEST.tsv: {sorted(missing_hang_rows)}")

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
        if lane == "asan":
            # The headline number for this lane is NOT the WORKS share -- it is
            # how many cells the sanitizer condemned. A cell can be WRONG here
            # for the same reason it is WRONG on the C lane and that is already
            # counted there; SANITIZE-FAIL is the column only this lane can fill.
            print(f"SANITIZE-FAIL: {tot.get('SANITIZE-FAIL', 0)}/{n} "
                  f"(memory-validity findings invisible to every value lane)")
        if lane == "ggdef":
            # NO-VERDICT is not a failure, it is an absence, so it must come out
            # of the denominator -- otherwise the ggdef lane's "WORKS %" reads as
            # a correctness score when it is mostly a subset-coverage score.
            adjudicated = n - tot.get("NO-VERDICT", 0)
            print(f"NO-VERDICT (outside the GGC subset): {tot.get('NO-VERDICT', 0)}/{n}")
            if adjudicated:
                print(f"WORKS among ADJUDICATED cells: {tot.get('WORKS', 0)}/{adjudicated} "
                      f"= {100 * tot.get('WORKS', 0) / adjudicated:.1f}%")

    if len(div_lanes) > 1:
        print(f"\n=== cross-lane divergences: {len(divergences)} "
              f"({len(new_div)} NOT in the baseline) ===")
        for cell, res, known in sorted(divergences):
            tag = "known" if known else "NEW"
            detail = " | ".join(f"{lane}={res[lane][0]}:{res[lane][1][:60]}" for lane in div_lanes)
            print(f"  [{tag}] {cell}: {detail}")

    if "ggdef" in lanes and div_lanes:
        print(f"\n=== BOTH-LANES-WRONG-ggdef-RIGHT: {len(both_wrong_ggdef_right)} ===")
        print("    every production lane measured is non-WORKS; the DEFINITION is right.")
        for cell, prod, g_actual in sorted(both_wrong_ggdef_right):
            detail = " | ".join(f"{lane}={b}:{a[:50]}" for lane, (b, a) in prod.items())
            print(f"  {cell}: ggdef={g_actual[:50]} vs {detail}")
        print(f"\n=== ggdef disagrees with a production lane: {len(ggdef_disagree)} ===")
        print("    triage material, not a verdict: ggdef implements the definition,")
        print("    it is not the definition, and it can lag a ratified decision.")
        for cell, prod, (g_bucket, g_actual) in sorted(ggdef_disagree):
            detail = " | ".join(f"{lane}={b}:{a[:40]}" for lane, (b, a) in prod.items())
            print(f"  {cell}: ggdef={g_bucket}:{g_actual[:40]} vs {detail}")

    if "asan" in lanes:
        san = [(row[COL_CELL], measured[row[COL_CELL]]["asan"][1])
               for row in rows
               if row[COL_CELL] in measured and row[COL_C] != "CONTROL"
               and measured[row[COL_CELL]]["asan"][0] == "SANITIZE-FAIL"]
        print(f"\n=== sanitizer findings (C lane only; the asan lane passes no "
              f"--backend): {len(san)} ===")
        for cell, detail in sorted(san):
            # A cell that is WORKS on the C lane but SANITIZE-FAIL here is the
            # headline: correct output, invalid memory. Flagged so it cannot be
            # read as "already known to be broken".
            c_bucket = measured[cell].get("c", ("?", ""))[0] if "c" in lanes else "?"
            flag = "  <-- prints the RIGHT answer" if c_bucket == "WORKS" else ""
            print(f"  {cell}: {detail}{flag}")

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

    if drifts:
        # Its own heading, because it is neither of the two verdicts the map has
        # always printed: the cell was broken and is still broken, in a
        # different way. NOT part of the exit code -- see the scoring branch.
        print(f"\n=== intra-quadrant DRIFT: {len(drifts)} "
              f"(non-good -> a DIFFERENT non-good; report-only, does not gate) ===")

    for cell, why in progress:
        print(f"  PROGRESS   {cell}: {why}")
    for cell, why in drifts:
        print(f"  DRIFT      {cell}: {why}")
    for cell, why in regressions:
        print(f"  REGRESSION {cell}: {why}")

    if args.accept:
        if regressions or new_div:
            print("\n--accept refused: MANIFEST.tsv NOT written "
                  f"({len(regressions)} REGRESSION(S), {len(new_div)} NEW DIVERGENCE(S))")
        else:
            (MAP / "MANIFEST.tsv").write_text(
                header + "\n" + "\n".join("\t".join(r) for r in rows) + "\n")
            recorded = (f", {len(drifts)} DRIFT rows recorded"
                        if args.accept_drift else
                        f" ({len(drifts)} DRIFT rows LEFT ALONE — "
                        f"pass --accept-drift after triage)" if drifts else "")
            print(f"\nbaseline updated ({len(progress)} progress rows folded"
                  f"{recorded}) - review this diff")

    if regressions or new_div:
        print(f"\n{len(regressions)} REGRESSION(S), {len(new_div)} NEW DIVERGENCE(S)")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
