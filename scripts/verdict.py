#!/usr/bin/env python3
"""THE verdict classifier: one definition of "what happened when we ran this",
consumed by every lane — bash, Python and Rust — instead of the seventeen
hand-rolled ones this file replaces.

WHY THIS FILE EXISTS
────────────────────
Seven separate measuring instruments in this tree published a GREEN over a real
fault. The worst of them are one line each:

  * `scripts/sanitize_sweep.sh` graded any unexplained nonzero exit CLEAN,
    because CLEAN was the fall-through sink of a content-only classifier — so a
    deterministic `rc 139` SIGSEGV read as a clean run.
  * `scripts/robustness_map.py` graded ANY nonzero rc `WORKS` on a cell whose
    expectation mentions a loud failure — so the same SIGSEGV read as a pass in
    a round-close gate.

Both are the same defect: a verdict reached WITHOUT a positive discriminator.
This classifier is built so that defect cannot be expressed in it.

THE THREE RULES THAT MAKE IT ABLE TO CATCH ITS OWN CLASS
────────────────────────────────────────────────────────
  1. NO DEFAULT SINK.       An outcome no rule positively matches is UNKNOWN and
                            is loud. `CLEAN` is a POSITIVE verdict — rc 0, no
                            sanitizer finding, no timeout — never a leftover.
  2. DISAGREEMENT ⇒ UNKNOWN. When two exit-axis rules both fire, the channels
                            contradict each other; report the contradiction,
                            never pick a winner.
  3. AMBIGUITY ⇒ UNKNOWN.   A rule may not fire on a tuple that is equally
                            consistent with a different verdict. The
                            under-determined tuples are ENUMERATED below with
                            the filed item that would retire each, and they
                            resolve to UNKNOWN — the tree does not have the
                            discriminator, so the classifier does not invent one.

Rule 1 catches a GAP. Rule 2 catches a CONTRADICTION. Only rule 3 catches a rule
firing CONFIDENTLY on an under-determined tuple, which is what both live
false-greens above actually were.

`--prove-exclusive` enumerates the whole observable tuple space and checks rules
2 and 3 mechanically; `--self-test` fires every label. Both are gated by
`tests/lints.rs::verdict_py_self_test_and_exclusivity_pass`.

THE EXIT-CODE AXIS IS ANCHORED ON THE RATIFIED TAXONOMY
───────────────────────────────────────────────────────
`docs/define-gorget/decisions.md:2074-2070` (ratified 2026-07-15, amended in
place 2026-08-10) is a TOTAL enumeration of the toolchain's exit codes:

    0 success · 1 static rejection · 2 usage · 101 trap + ICE
    · 102 uncaught channel error · 103 ggdef fuel

Being total is what makes it useful here: **its complement is exactly the fault
domain.** `rc ∉ {0,1,2,101,102,103}` from a gg-produced process is OFF-TAXONOMY
and can never be CLEAN — with ratified backing, not a heuristic.

The other axes are ORTHOGONAL and additional, because the ledger does not speak
to them and should not: the SANITIZER axis (LEAK / CORRUPT / UB, read off
stderr) and the RUNNER axis (TIMEOUT / RUNNER_FAIL, produced by the harness, not
by the program). A verdict is a product of (phase, exit code, sanitizer
channel, runner outcome), never an enum widened from one of them.

PHASE IS A REQUIRED INPUT
─────────────────────────
`1` means different things depending on which step produced it, and a classifier
that reads the code without the step grades an internal runtime panic as a
static rejection — a fault published as a correct outcome. So `--phase` is
mandatory and has no default.

CAPTURE THE KIND, NEVER ENUMERATE IT
────────────────────────────────────
`ERROR: AddressSanitizer: <kind>` — the kind is CAPTURED from the report. An
enumerated kind list is the same name-list blindness AGENTS.md Core #2 forbids
at IR boundaries, and it has already misclassified a sanitized SIGSEGV in this
tree (the alternation listed three kinds and ASan printed a fourth).

THE SANITIZER EXIT-CODE CONVENTION IS AN INPUT, NOT AN ASSUMPTION
─────────────────────────────────────────────────────────────────
`sanitize_sweep.sh` runs with `exitcode=0`; `robustness_map.py` runs with
`exitcode=99`. A classifier that hardcodes either is wrong under the other, so
callers pass `--sanitizer-exitcode`.

USAGE
─────
    verdict.py --phase build|run --rc N [--stderr FILE] [--stdout FILE]
               [--timed-out] [--sanitizer-exitcode N]
               [--format json|sweep|label|tsv]
    verdict.py --self-test          # fire every label, and the inverse for CLEAN
    verdict.py --prove-exclusive    # rules 2 and 3, checked over the tuple space
    verdict.py --live-test          # the same labels off REAL processes

Python consumers `import` it (`findings_for`, `classify`); bash and Rust shell
out. Shelling out rather than reimplementing keeps ONE source of truth — the
same precedent `tests/lints.rs`'s `todo_index.py` call already sets.
"""
from __future__ import annotations

import argparse
import json
import os
import re
import sys

# ── the ratified exit-code taxonomy ──────────────────────────────────────────
# docs/define-gorget/decisions.md:2074-2070. A TOTAL enumeration, which is what
# lets the complement be the fault domain.
RATIFIED_EXIT_CODES = {
    0: "success",
    1: "static rejection",
    2: "usage error",
    101: "trap or ICE",
    102: "uncaught channel error",
    103: "ggdef fuel exhaustion",
}

# ── verdict labels ───────────────────────────────────────────────────────────
# Severity order, MOST severe first. The headline is the most severe finding in
# the set — never the first one matched, because for a sanitized null deref the
# UBSan `runtime error:` line PRECEDES the ASan report and a first-match reader
# under-grades it.
#
# ⚠ The headline never REPLACES the set. `sanitize_sweep.sh` gates on per-class
# counts extracted from the comma-joined multi-label set (UBSAN_CEILING,
# TIMEOUT_CEILING, INFRA_CEILING are all 0), so collapsing a leak+UBSan run to
# one headline would make it stop counting toward UBSAN_CEILING — a silent
# ceiling regression. Consumers that gate read `findings`; consumers that
# display read `verdict`.
SEVERITY = [
    "UNKNOWN",       # loudest: we could not tell. Never maskable by anything.
    "RUNNER_FAIL",   # our own plumbing broke; the measurement is void
    # CORRUPT outranks CRASH deliberately: when ASan catches the fault it also
    # NAMES it, and the named kind is the actionable finding. A plain SIGSEGV
    # with no sanitizer report has no CORRUPT to outrank it and still headlines
    # CRASH, so nothing is lost either way — and the SET carries both.
    "CORRUPT",       # ASan: memory validity violated
    "CRASH",         # died on a signal / off-taxonomy exit
    "UB",            # UBSan: undefined behaviour (exits 0 by default!)
    "LEAK",          # LSan
    "TIMEOUT",       # deadline exceeded
    "ICE",           # compiler panic
    "BUILD_FAIL",    # accepted the program, then failed to deliver a binary
    "TRAP",          # ratified 101: a loud, CORRECT runtime failure
    "CHANNEL_ERROR", # ratified 102
    "REJECTED",      # ratified 1 on the build phase: a diagnostic doing its job
    "USAGE",         # ratified 2
    "FUEL",          # ratified 103 (ggdef totality guard)
    "EXIT",          # a USER PROGRAM chose a nonzero exit code. Not a fault.
    "CLEAN",         # positive: rc 0, no finding, no timeout
]
SEVERITY_RANK = {name: i for i, name in enumerate(SEVERITY)}

PHASES = ("build", "run")

# ⚠ WHOSE EXIT CODE IS THIS? A REQUIRED INPUT, and its absence produced a
# confident wrong answer on the first full corpus sweep.
#
# The ratified taxonomy (`decisions.md:2074-2070`) is the TOOLCHAIN's contract —
# what `gg` and `ggdef` return. It does NOT constrain a user's program: the same
# ledger entry hands the small-int band to the USER as their exit API
# (`:2073`, *"`main throws int` hands the small-int band to the USER as their
# exit API"*). So "rc off the ratified set ⇒ CRASH" is right for the toolchain
# and WRONG for a program.
#
# Measured, not reasoned: `tests/fixtures/gg_impl_exit7.gg` is a fixture whose
# entire purpose is `exit(7)` — the self-host driver must propagate it unchanged
# — and the first version of this classifier graded it `CRASH:rc7` and RED-ed a
# round-close gate over a program doing exactly what it was written to do. Same
# shape as the PHASE lesson one input earlier: the exit code alone never meant
# what it appeared to mean.
SUBJECTS = ("toolchain", "program")

# ── the sanitizer marker set ─────────────────────────────────────────────────
# The complete set: BOTH UBSan forms are here, which no single incumbent site
# had. UBSan does not abort by default, so a `-fsanitize=undefined` trip exits 0
# with only a `runtime error:` line — an exit-code-only check is blind to the
# entire undefined-behaviour half of the lane.
ASAN_ERROR_RE = re.compile(r"ERROR: AddressSanitizer: ([A-Za-z0-9_-]+)")
LSAN_MARKERS = ("ERROR: LeakSanitizer", "SUMMARY: LeakSanitizer",
                "detected memory leaks")
UBSAN_MARKERS = ("SUMMARY: UndefinedBehaviorSanitizer", "runtime error:")

# ── build-phase discriminators, every one MEASURED at HEAD ───────────────────
# Each is a POSITIVE marker printed by a known site in src/main.rs. They are the
# reason the build phase is NOT the four-way collapse it is often described as:
# link failure and uncoded lexer/parser errors both DO carry a discriminator.
#
# ⚠ NO LINE NUMBERS HERE, DELIBERATELY, AND THIS IS THE THIRD TIME OF ASKING.
# These comments carried eight `src/main.rs:NNN` cites, and every one of them was
# WRONG THE MOMENT IT WAS PUBLISHED: the same commit that created this file also
# added a `use` line to src/main.rs, shifting everything below it by one. Two
# output-review passes then certified them as correct. The ledger already ruled on
# exactly this pattern (`docs/define-gorget/decisions.md:2055-2045`): cite by
# anchor, not by line, "which is what line numbers do".
#
# The MARKER STRINGS BELOW ARE THE ANCHOR — they are literally the grep. Regenerate
# the sites, and see any that have been added since, with:
#
#     grep -n 'parse error(s) found\|error(s) found' src/main.rs
#     grep -n 'C compiler exited with\|Failed to run C compiler\|Linking failed\|for linking' src/main.rs
#
# That second command finds EIGHT sites where the old comment named four — it
# surfaced a whole freestanding-target arm the line list had never mentioned. A
# command does not rot, and it enumerates.
BUILD_ICE_MARKERS = ("panicked at", "panicked",)
# `error[E_Code]` (semantic, coded) — rendered via src/errors.rs.
BUILD_REJECT_MARKERS = (
    "error[",                    # coded semantic diagnostic
    "parse error(s) found",      # the parse/lex tally
    "error(s) found",            # the semantic tally
)
# "the compiler said yes and then failed to deliver a binary" — a
# miscompile-class signal that must NEVER share a bucket with a rejection.
BUILD_DELIVER_FAIL_MARKERS = (
    "C compiler exited with:",       # C backend, incl. the freestanding arm
    "Failed to run C compiler '",    # cc could not be spawned at all
    "Linking failed:",               # LLVM backend
    "for linking:",                  # the linker could not be spawned
)
# t0646: usage errors collapse into 1 instead of the ratified 2. Two of the 79
# `process::exit(1)` sites in src/main.rs print something recognisable; the rest
# do not, which is the ambiguity cell declared below.
BUILD_USAGE_MARKERS = ("Usage: gg", "Error reading ")

# The ratified TRAP line — `src/backend/c/runtime/panic_normal.c`'s
# `gorget_trap_at`, whose format is normative (D11).
TRAP_MARKER_RE = re.compile(r"^trap\[[A-Za-z0-9_]+\]:", re.MULTILINE)

# ── declared ambiguity cells (rule 3) ────────────────────────────────────────
# An observable tuple that is equally consistent with two or more verdicts. The
# tree does not carry a discriminator for these, so the classifier reports
# UNKNOWN and NAMES the cell — it does not manufacture one out of a fragile
# stderr substring, which is exactly the name-matching this instrument exists to
# retire. Each cell names the filed item that would retire it.
#
# Core #12 applies to the third row: `102` is in the ratified alphabet and is
# UNREACHABLE at HEAD (`E_MainThrowsNonInt` is still live and there is no
# `exit(102)` in the C runtime), so its label cannot be RED-demonstrated against
# a real gg process. It is named here as an omitted cell rather than faked.
AMBIGUITY_CELLS = {
    "build_rc1_unmarked": (
        "t0646",
        "build phase, rc 1, and none of the coded-diagnostic / parse-tally / "
        "ICE / delivery-failure / usage markers is present. Consistent with: a "
        "CLI or I/O usage error (t0646 — 79 `process::exit(1)` sites in "
        "src/main.rs, most printing a bare `{e}`), an internal compiler error "
        "path that does not say `panicked`, or a codegen failure whose message "
        "is not one of the four delivery-failure forms.",
    ),
    "run_rc1": (
        "t0647",
        "run phase, rc 1. Consistent with: the program's own `exit(1)`, or an "
        "INTERNAL runtime panic — `gorget_panic_at` "
        "(src/backend/c/runtime/panic_normal.c:5) exits 1, colliding with the "
        "static-rejection code. t0647's own text: \"a shell script can't "
        "distinguish\". It is right, and this is that shell script.",
    ),
}
# Ratified codes that exist in the alphabet but cannot be produced at HEAD.
# Named, never faked (Core #12).
UNREACHABLE_AT_HEAD = {
    102: "E_MainThrowsNonInt is still live (src/semantic/typecheck.rs) and there "
         "is no exit(102) in the C runtime; the ledger records 102 as RETIRING "
         "at E0 (decisions.md:2069-2060). The label exists and is honoured if a "
         "102 ever arrives; it has no RED demonstration against a real process.",
}


class Verdict:
    """The full finding SET plus one headline derived by severity."""

    __slots__ = ("findings", "detail", "rc", "phase", "on_taxonomy", "ambiguity")

    def __init__(self, findings, detail, rc, phase, on_taxonomy, ambiguity=None):
        self.findings = findings          # list[str], canonical labels, sorted by severity
        self.detail = detail              # dict[str, str] — per-finding evidence
        self.rc = rc
        self.phase = phase
        self.on_taxonomy = on_taxonomy
        self.ambiguity = ambiguity        # None | (cell_name, item, explanation)

    @property
    def verdict(self) -> str:
        return min(self.findings, key=lambda f: SEVERITY_RANK[_base(f)])

    def to_json(self) -> str:
        return json.dumps({
            "verdict": self.verdict,
            "findings": self.findings,
            "detail": self.detail,
            "rc": self.rc,
            "phase": self.phase,
            "on_taxonomy": self.on_taxonomy,
            "ratified_meaning": RATIFIED_EXIT_CODES.get(self.rc),
            "ambiguity": None if self.ambiguity is None else {
                "cell": self.ambiguity[0], "item": self.ambiguity[1],
                "why": self.ambiguity[2],
            },
        }, sort_keys=True)

    def to_sweep(self) -> str:
        """`scripts/sanitize_sweep.sh`'s label spelling, comma-joined.

        The spelling is part of that gate's contract — `UBSAN_CEILING` is
        extracted with `$2 ~ /(^|,)UBSAN(,|$)/` and the leak allowlist keys off
        `LEAK` — so the MAPPING lives here, in the one definition, rather than
        as a second copy of the classifier in bash.
        """
        out = []
        for f in self.findings:
            base = _base(f)
            if base == "CORRUPT":
                kind = f.split(":", 1)[1] if ":" in f else "unknown"
                out.append(f"ASAN_{kind}")
            elif base == "UB":
                out.append("UBSAN")
            elif base in ("CRASH", "EXIT"):
                out.append(f)          # CRASH:sig11 / EXIT:7 — the value captured
            else:
                out.append(base)
        return ",".join(out)


def _base(finding: str) -> str:
    return finding.split(":", 1)[0]


def _off_taxonomy_crash(rc, exit_axis, detail, subject):
    """A signal death is always a CRASH. An off-taxonomy plain exit is a crash
    only for the TOOLCHAIN.

    For the toolchain the ratified set is TOTAL, so its complement is exactly the
    fault domain — with ratified backing rather than a heuristic. For a USER
    PROGRAM the small-int band is the user's own exit API (`decisions.md:2085`),
    so a plain `exit(7)` is a deliberate outcome, not a fault: `EXIT:7`. It is
    still not CLEAN — CLEAN means rc 0 with nothing found — so nothing can hide
    behind it.

    ⚠ The 128+n band under SHELL reporting is the one place a program could
    spoof a signal by exiting 139 on purpose. The ledger already ruled on that
    pattern for the reserved codes — *"a user can still `throw 102` and spoof the
    class — same caveat as Rust's `process::exit(101)`"* — so the conventional
    reading is the ratified-consistent one, and it is taken here rather than
    treated as undecidable. The signal number is CAPTURED, never matched against
    a list of "interesting" signals.
    """
    if rc < 0:
        label = f"CRASH:sig{-rc}"
        why = (f"rc {rc} = killed by signal {-rc} (POSIX wait convention)")
    elif 128 < rc < 192:
        label = f"CRASH:sig{rc - 128}"
        why = (f"rc {rc} = 128 + signal {rc - 128} (shell convention)")
    elif subject == "toolchain":
        label = f"CRASH:rc{rc}"
        why = (f"rc {rc} is off the RATIFIED TOOLCHAIN taxonomy "
               f"{sorted(RATIFIED_EXIT_CODES)} — never CLEAN")
    else:
        label = f"EXIT:{rc}"
        why = (f"the program exited {rc} of its own accord; the small-int band "
               f"is the USER's exit API (decisions.md:2085), not the toolchain's")
    exit_axis.append(label)
    detail[label] = why


def findings_for(phase, rc, stderr="", stdout="", timed_out=False,
                 sanitizer_exitcode=None, subject=None) -> Verdict:
    """Classify one observed outcome. THE definition; everything else is a view.

    `sanitizer_exitcode` is the value the caller passed to ASAN_OPTIONS'
    `exitcode=` for this run, or None if no sanitizer was configured. It is an
    input because the tree uses two different conventions (0 and 99) and a
    classifier that assumes either is wrong under the other.
    """
    if phase not in PHASES:
        raise ValueError(f"phase must be one of {PHASES}, got {phase!r}")
    # `build` means we ran the compiler, so the subject is the toolchain; `run`
    # means we ran what it produced. Callers that break that correspondence —
    # `ggdef run <cell>` is a TOOLCHAIN invocation spelled as a run — pass
    # `subject` explicitly.
    if subject is None:
        subject = "toolchain" if phase == "build" else "program"
    if subject not in SUBJECTS:
        raise ValueError(f"subject must be one of {SUBJECTS}, got {subject!r}")
    stderr = stderr or ""
    stdout = stdout or ""
    detail = {}
    exit_axis = []      # mutually exclusive by construction; ≥2 ⇒ disagreement
    orthogonal = []     # sanitizer + runner findings, which ACCUMULATE
    ambiguity = None

    # ── runner axis ──────────────────────────────────────────────────────────
    # Three mechanisms produce a timeout — GNU `timeout(1)`'s rc 124, Python's
    # TimeoutExpired, and a Rust deadline loop — and they are ONE verdict. The
    # flag is how the two that have no exit code report it.
    if timed_out or rc == 124:
        orthogonal.append("TIMEOUT")
        detail["TIMEOUT"] = "deadline exceeded" if timed_out else "rc 124 (timeout(1))"
    # 125 = `timeout` itself failed; 126 = not executable; 127 = not found. The
    # RUNNER is broken, and a broken runner produces an EMPTY log that every
    # content-based classifier reads as CLEAN — the whole corpus silently green.
    if rc in (125, 126, 127):
        orthogonal.append("RUNNER_FAIL")
        detail["RUNNER_FAIL"] = {125: "timeout(1) failed", 126: "not executable",
                                 127: "command not found"}[rc]

    # ── sanitizer axis (orthogonal to everything; findings accumulate) ───────
    m = ASAN_ERROR_RE.search(stderr)
    if m:
        kind = m.group(1)                        # CAPTURED, never enumerated
        orthogonal.append(f"CORRUPT:{kind}")
        detail[f"CORRUPT:{kind}"] = m.group(0)
    if any(k in stderr for k in LSAN_MARKERS):
        orthogonal.append("LEAK")
        detail["LEAK"] = "LeakSanitizer report on stderr"
    if any(k in stderr for k in UBSAN_MARKERS):
        orthogonal.append("UB")
        detail["UB"] = "UndefinedBehaviorSanitizer report on stderr"

    sanitizer_fired = any(_base(f) in ("CORRUPT", "LEAK", "UB") for f in orthogonal)

    # ── exit-code axis ───────────────────────────────────────────────────────
    on_taxonomy = rc in RATIFIED_EXIT_CODES or (
        sanitizer_exitcode is not None and rc == sanitizer_exitcode)

    # When the RUNNER produced the outcome, the exit status belongs to the
    # runner, not to the program: a killed child's 137 says "we SIGKILLed it",
    # and `timeout(1)`'s 124 says "the deadline fired". Reading a program
    # verdict off either would be reading OUR signal as THEIRS.
    runner_claimed = timed_out or rc in (124, 125, 126, 127)

    if runner_claimed:
        pass
    elif sanitizer_fired and rc != 0 and rc not in RATIFIED_EXIT_CODES:
        # A sanitizer report EXPLAINS a nonzero exit that is otherwise
        # off-taxonomy — except that the process may ALSO have died on a signal,
        # which the off-taxonomy branch below still records. Fall through.
        _off_taxonomy_crash(rc, exit_axis, detail, subject)
    elif sanitizer_fired and rc != 0:
        # rc 1 (default ASan) or the configured sanitizer exit code: the
        # sanitizer channel positively accounts for the nonzero exit, so the
        # rc-1 ambiguity cell does NOT apply. This is the one case where a
        # second channel legitimately resolves an otherwise-ambiguous tuple.
        #
        # ⚠ BUT THE EXIT CODE STILL CARRIES ITS OWN RATIFIED MEANING, AND THE SET
        # MUST KEEP IT. An earlier version returned here unconditionally, so a
        # run that BOTH trapped (101 + `trap[T_X]:`) and produced an ASan report
        # yielded `['CORRUPT:kind']` with TRAP absent — the headline was right
        # (CORRUPT outranks TRAP) and no ceiling reads TRAP, so nothing went
        # green that should not have. It was still a set-completeness hole, and
        # "emit the full finding SET" is the rule that keeps `UBSAN_CEILING`
        # counting a leak+UBSan program.
        if rc == 101 and TRAP_MARKER_RE.search(stderr):
            exit_axis.append("TRAP")
            detail["TRAP"] = TRAP_MARKER_RE.search(stderr).group(0)
        elif rc == 103:
            exit_axis.append("FUEL")
            detail["FUEL"] = "ratified exit 103: ggdef fuel exhaustion"
    elif rc == 0:
        # CLEAN is a POSITIVE verdict and it is the one that must never be a
        # sink: it requires rc 0, no sanitizer finding, and no timeout. A UBSan
        # trip exits 0, so `rc == 0` alone is NOT clean.
        if not orthogonal:
            exit_axis.append("CLEAN")
            detail["CLEAN"] = "rc 0, no sanitizer finding, no timeout"
    elif rc == 2:
        exit_axis.append("USAGE")
        detail["USAGE"] = "ratified exit 2: usage error"
    elif rc == 103:
        exit_axis.append("FUEL")
        detail["FUEL"] = "ratified exit 103: ggdef fuel exhaustion"
    elif rc == 102:
        exit_axis.append("CHANNEL_ERROR")
        detail["CHANNEL_ERROR"] = "ratified exit 102: uncaught channel error"
    elif rc == 101:
        # 101 carries TWO ratified meanings and they ARE separable, positively:
        # a trap prints the normative `trap[T_Code]:` line; a Rust ICE prints
        # `panicked`. Both firing is a genuine contradiction.
        if TRAP_MARKER_RE.search(stderr):
            exit_axis.append("TRAP")
            detail["TRAP"] = TRAP_MARKER_RE.search(stderr).group(0)
        if any(k in stderr for k in BUILD_ICE_MARKERS):
            exit_axis.append("ICE")
            detail["ICE"] = "compiler panic on stderr"
        if not exit_axis:
            # rc 101 with neither marker: on-taxonomy but unattributable.
            pass
    elif rc == 1:
        if phase == "build":
            if any(k in stderr for k in BUILD_ICE_MARKERS):
                exit_axis.append("ICE")
                detail["ICE"] = "compiler panic on stderr"
            if any(k in stderr for k in BUILD_REJECT_MARKERS):
                exit_axis.append("REJECTED")
                detail["REJECTED"] = "coded diagnostic or error tally on stderr"
            if any(k in stderr for k in BUILD_DELIVER_FAIL_MARKERS):
                exit_axis.append("BUILD_FAIL")
                detail["BUILD_FAIL"] = ("the compiler accepted the program and then "
                                        "failed to deliver a binary")
            if any(k in stderr for k in BUILD_USAGE_MARKERS):
                exit_axis.append("USAGE")
                detail["USAGE"] = "usage/IO error, collapsed into rc 1 (t0646)"
            if not exit_axis:
                ambiguity = ("build_rc1_unmarked",) + AMBIGUITY_CELLS["build_rc1_unmarked"]
        else:
            # Run phase, rc 1. NO discriminator exists at HEAD (t0647). Do not
            # invent one: a program's own exit(1) and gorget_panic_at's exit(1)
            # are indistinguishable from outside.
            ambiguity = ("run_rc1",) + AMBIGUITY_CELLS["run_rc1"]
    elif sanitizer_exitcode is not None and rc == sanitizer_exitcode:
        # The sanitizer's own configured abort code. It carries no verdict of its
        # own — the SANITIZER AXIS above says what was found. If the axis found
        # nothing, the channels disagree (the sanitizer aborted and said why
        # nowhere) and that is UNKNOWN, not CLEAN.
        if not sanitizer_fired:
            ambiguity = ("sanitizer_exit_without_report", "—",
                         f"rc {rc} is the configured sanitizer exit code, but no "
                         f"sanitizer report is present on stderr: the two channels "
                         f"disagree about whether a sanitizer fired.")
    else:
        _off_taxonomy_crash(rc, exit_axis, detail, subject)

    # ── rule 2: two exit-axis rules fired ⇒ the channels contradict ──────────
    if len(exit_axis) > 1:
        return Verdict(["UNKNOWN"],
                       {"UNKNOWN": "channel disagreement: " + " + ".join(
                           f"{f} ({detail.get(f, '')})" for f in exit_axis)},
                       rc, phase, on_taxonomy,
                       ("channel_disagreement", "—",
                        "two exit-axis rules matched the same tuple"))

    # ── rule 3: a declared ambiguity cell ⇒ UNKNOWN, named ───────────────────
    if ambiguity is not None:
        findings = list(orthogonal) + ["UNKNOWN"]
        detail["UNKNOWN"] = f"ambiguous cell `{ambiguity[0]}` ({ambiguity[1]}): {ambiguity[2]}"
        return Verdict(_sorted(findings), detail, rc, phase, on_taxonomy, ambiguity)

    findings = list(orthogonal) + list(exit_axis)

    # ── rule 1: NO DEFAULT SINK ──────────────────────────────────────────────
    # Nothing positively matched. That is a GAP in this classifier, and it is
    # reported as one. It is emphatically not CLEAN.
    if not findings:
        detail["UNKNOWN"] = (
            f"no rule positively matched (phase={phase}, rc={rc}, "
            f"stderr={'non-empty' if stderr.strip() else 'empty'}). "
            f"NO DEFAULT SINK: an unclassifiable outcome is UNKNOWN, never CLEAN.")
        return Verdict(["UNKNOWN"], detail, rc, phase, on_taxonomy,
                       ("unmatched", "—", "no positive discriminator fired"))

    return Verdict(_sorted(findings), detail, rc, phase, on_taxonomy, None)


def _sorted(findings):
    seen, out = set(), []
    for f in sorted(findings, key=lambda f: SEVERITY_RANK[_base(f)]):
        if f not in seen:
            seen.add(f)
            out.append(f)
    return out


classify = findings_for   # the name Python consumers import


# ─────────────────────────── exclusivity proof ───────────────────────────────

def prove_exclusive(verbose=False) -> int:
    """Rule 2 and rule 3, checked MECHANICALLY over the observable tuple space.

    The observable tuple that any rule can read is
        (phase, rc, subset-of-markers, timed_out, sanitizer_exitcode)
    and the marker set is finite, so the space is enumerable. For every point we
    assert the classifier reaches EXACTLY ONE exit-axis verdict or UNKNOWN — an
    overlap that is not reported as UNKNOWN would be a rule firing confidently
    on an under-determined tuple, which is the defect this file exists to make
    impossible.
    """
    markers = [
        "",                                   # silence
        "error[E_TypeMismatch]: nope",        # coded semantic diagnostic
        "\n3 parse error(s) found",           # parse/lex tally
        "\n1 error(s) found",                 # semantic tally
        "thread 'main' panicked at src/x.rs", # ICE
        "C compiler exited with: exit status: 1",
        "Linking failed: exit status 1",
        "Usage: gg <file.gg>",
        "Error reading /x.gg: No such file",
        "trap[T_DivByZero]: division by zero at a.gg:1:1",
        "ERROR: AddressSanitizer: heap-use-after-free",
        "ERROR: LeakSanitizer: detected memory leaks",
        "SUMMARY: UndefinedBehaviorSanitizer: undefined-behavior",
        "x.c:1:1: runtime error: signed integer overflow",
    ]
    rcs = sorted(set(list(RATIFIED_EXIT_CODES) + [124, 125, 126, 127, 99, 134, 139, 7, -11]))
    bad = 0
    checked = 0
    import itertools
    # Every SUBSET of the marker vocabulary, so a two-channel contradiction is
    # actually reached rather than assumed unreachable.
    for k in range(len(markers) + 1):
        if k > 2:
            break            # pairs suffice to exhibit every overlap; see below
        for combo in itertools.combinations(markers, k):
            stderr = "\n".join(combo)
            for phase in PHASES:
                for rc in rcs:
                    for timed_out in (False, True):
                        for sx in (None, 0, 99):
                            checked += 1
                            v = findings_for(phase, rc, stderr=stderr,
                                             timed_out=timed_out,
                                             sanitizer_exitcode=sx)
                            exit_axis = [f for f in v.findings
                                         if _base(f) not in ("LEAK", "UB", "CORRUPT",
                                                             "TIMEOUT", "RUNNER_FAIL")]
                            if len(exit_axis) > 1:
                                bad += 1
                                print(f"OVERLAP phase={phase} rc={rc} "
                                      f"markers={combo} -> {v.findings}")
                            if "CLEAN" in v.findings and (rc != 0 or timed_out
                                                          or len(v.findings) > 1):
                                bad += 1
                                print(f"CLEAN-SINK phase={phase} rc={rc} "
                                      f"timed_out={timed_out} markers={combo} "
                                      f"-> {v.findings}")
                            if verbose:
                                print(phase, rc, combo, v.findings)
    print(f"exclusivity: {checked} tuples checked, {bad} violations")
    print("  (subsets of size 0..2 over a 14-marker vocabulary: every PAIR of "
          "channels is exercised, which is what an overlap needs)")

    # Rule 3's other half: every DECLARED ambiguity cell must actually resolve to
    # UNKNOWN. Without this, deleting rule 3 and guessing a verdict leaves the
    # overlap check green -- measured: it did -- because a guess is not an overlap.
    witnesses = {
        "build_rc1_unmarked": dict(phase="build", rc=1, stderr="opaque"),
        "run_rc1": dict(phase="run", rc=1),
    }
    missing = set(AMBIGUITY_CELLS) - set(witnesses)
    if missing:
        bad += 1
        print(f"  DECLARED-CELL-WITHOUT-WITNESS: {sorted(missing)} -- every cell in "
              f"AMBIGUITY_CELLS needs a witness tuple here, or it is prose")
    for cell, kw in witnesses.items():
        v = findings_for(**kw)
        if "UNKNOWN" not in v.findings or v.ambiguity is None or v.ambiguity[0] != cell:
            bad += 1
            print(f"  AMBIGUITY-CELL-NOT-LIVE: {cell} -> {v.findings} "
                  f"(ambiguity={v.ambiguity[0] if v.ambiguity else None})")
    print(f"ambiguity cells: {len(witnesses)} declared cells, each witnessed")
    return 1 if bad else 0


# ───────────────────────────── the self-test ─────────────────────────────────
# Q2 on this instrument: can it catch its OWN class? Every label is fired, and
# CLEAN is additionally demonstrated by the INVERSE — shown NOT to fire on each
# of the other inputs, because "make CLEAN fire" is satisfied by the very
# fall-through sink this file replaces.

SELF_TEST_CASES = [
    # (name, kwargs, expected headline, expected finding present)
    ("CLEAN", dict(phase="run", rc=0), "CLEAN", "CLEAN"),
    ("LEAK", dict(phase="run", rc=0,
                  stderr="==1==ERROR: LeakSanitizer: detected memory leaks"),
     "LEAK", "LEAK"),
    ("CORRUPT", dict(phase="run", rc=1,
                     stderr="==1==ERROR: AddressSanitizer: heap-use-after-free on address 0x1"),
     "CORRUPT:heap-use-after-free", "CORRUPT:heap-use-after-free"),
    ("CORRUPT/other-kind", dict(phase="run", rc=1,
                                stderr="==1==ERROR: AddressSanitizer: SEGV on unknown address"),
     "CORRUPT:SEGV", "CORRUPT:SEGV"),
    ("UB", dict(phase="run", rc=0,
                stderr="x.c:9:5: runtime error: signed integer overflow"),
     "UB", "UB"),
    ("CRASH", dict(phase="run", rc=139), "CRASH:sig11", "CRASH:sig11"),
    ("CRASH/negative-rc", dict(phase="run", rc=-11), "CRASH:sig11", "CRASH:sig11"),
    ("CRASH/off-taxonomy-toolchain",
     dict(phase="run", rc=7, subject="toolchain"), "CRASH:rc7", "CRASH:rc7"),
    # ⭐ THE CASE THE FIRST FULL SWEEP CAUGHT. `tests/fixtures/gg_impl_exit7.gg`
    # exists to `exit(7)` and have the driver propagate it; grading that a CRASH
    # RED-ed a round-close gate over a program doing its job.
    ("EXIT/program-chose-it", dict(phase="run", rc=7, subject="program"),
     "EXIT:7", "EXIT:7"),
    ("TIMEOUT/flag", dict(phase="run", rc=0, timed_out=True), "TIMEOUT", "TIMEOUT"),
    ("TIMEOUT/rc124", dict(phase="run", rc=124), "TIMEOUT", "TIMEOUT"),
    ("RUNNER_FAIL", dict(phase="run", rc=127), "RUNNER_FAIL", "RUNNER_FAIL"),
    ("TRAP", dict(phase="run", rc=101,
                  stderr="trap[T_DivByZero]: division by zero at a.gg:3:15"),
     "TRAP", "TRAP"),
    ("ICE", dict(phase="build", rc=101,
                 stderr="thread 'main' panicked at src/ir/lower.rs:1"),
     "ICE", "ICE"),
    ("REJECTED/coded", dict(phase="build", rc=1,
                            stderr="error[E_TypeMismatch]: type mismatch"),
     "REJECTED", "REJECTED"),
    ("REJECTED/parse-tally", dict(phase="build", rc=1,
                                  stderr="error: expected expression\n\n1 parse error(s) found"),
     "REJECTED", "REJECTED"),
    ("BUILD_FAIL", dict(phase="build", rc=1,
                        stderr="out.c:1: error: x\nC compiler exited with: exit status: 1"),
     "BUILD_FAIL", "BUILD_FAIL"),
    ("USAGE/ratified-2", dict(phase="build", rc=2), "USAGE", "USAGE"),
    ("USAGE/collapsed-1", dict(phase="build", rc=1, stderr="Usage: gg <file.gg>"),
     "USAGE", "USAGE"),
    ("FUEL", dict(phase="run", rc=103), "FUEL", "FUEL"),
    ("UNKNOWN/no-rule", dict(phase="build", rc=1, stderr="something opaque"),
     "UNKNOWN", "UNKNOWN"),
    ("UNKNOWN/run-rc1", dict(phase="run", rc=1), "UNKNOWN", "UNKNOWN"),
    # The FINAL fall-through (rule 1). See CLEAN_MUST_NOT_FIRE's note.
    ("UNKNOWN/rc101-unattributed", dict(phase="run", rc=101), "UNKNOWN", "UNKNOWN"),
    ("UNKNOWN/disagreement", dict(phase="build", rc=1,
                                  stderr="error[E_X]: no\nC compiler exited with: exit status: 1"),
     "UNKNOWN", "UNKNOWN"),
    ("UNKNOWN/sanitizer-exit-no-report",
     dict(phase="run", rc=99, sanitizer_exitcode=99), "UNKNOWN", "UNKNOWN"),
    # N3: the exit code keeps its ratified meaning even when a sanitizer also
    # fired. CORRUPT still headlines; TRAP must not vanish from the SET.
    ("set/trap+corrupt",
     dict(phase="run", rc=101,
          stderr="trap[T_Bounds]: oob at a.gg:1:1\n"
                 "==1==ERROR: AddressSanitizer: heap-buffer-overflow"),
     "CORRUPT:heap-buffer-overflow", "TRAP"),
    # Severity: MOST severe wins, and the SET is preserved.
    ("severity/leak+crash",
     dict(phase="run", rc=139,
          stderr="==1==ERROR: LeakSanitizer: detected memory leaks"),
     "CRASH:sig11", "LEAK"),
    ("severity/ub-before-asan",
     dict(phase="run", rc=1,
          stderr="a.c:1: runtime error: null pointer\n"
                 "==1==ERROR: AddressSanitizer: SEGV on unknown address"),
     "CORRUPT:SEGV", "UB"),
]

# CLEAN must NOT fire on any of these. This is the INVERSE demonstration: a
# fall-through sink passes "make CLEAN fire" and fails every row here.
CLEAN_MUST_NOT_FIRE = [
    ("leak", dict(phase="run", rc=0,
                  stderr="==1==ERROR: LeakSanitizer: detected memory leaks")),
    ("ub-at-rc0", dict(phase="run", rc=0, stderr="x.c:1: runtime error: overflow")),
    ("corrupt", dict(phase="run", rc=0,
                     stderr="==1==ERROR: AddressSanitizer: double-free")),
    ("segv", dict(phase="run", rc=139)),
    ("abort", dict(phase="run", rc=134)),
    ("off-taxonomy-toolchain", dict(phase="run", rc=7, subject="toolchain")),
    ("program-chose-a-nonzero-exit", dict(phase="run", rc=7, subject="program")),
    ("timeout-flag", dict(phase="run", rc=0, timed_out=True)),
    ("timeout-124", dict(phase="run", rc=124)),
    ("runner-fail", dict(phase="run", rc=127)),
    ("run-rc1", dict(phase="run", rc=1)),
    ("build-rc1-opaque", dict(phase="build", rc=1, stderr="mystery")),
    ("trap", dict(phase="run", rc=101, stderr="trap[T_Bounds]: oob at a.gg:1:1")),
    ("sanitizer-exit-silent", dict(phase="run", rc=99, sanitizer_exitcode=99)),
    # ⚠ THIS ROW IS THE ONE THAT REACHES THE FINAL FALL-THROUGH. rc 101 is ON the
    # ratified taxonomy but carries two meanings, and with neither the `trap[` nor
    # the `panicked` marker present nothing positively attributes it. Every other
    # row above exits through an earlier branch, so without this one a restored
    # `_labels=CLEAN` sink passes the whole self-test — measured: it did.
    ("rc101-unattributed", dict(phase="run", rc=101)),
    ("rc0-with-timeout-and-marker",
     dict(phase="build", rc=0, timed_out=True)),
]


def self_test(verbose=True) -> int:
    fails = 0
    fired = set()
    print("── every label, fired ──")
    for name, kw, want_headline, want_finding in SELF_TEST_CASES:
        v = findings_for(**kw)
        ok = v.verdict == want_headline and want_finding in v.findings
        fired.update(_base(f) for f in v.findings)
        if not ok:
            fails += 1
        if verbose or not ok:
            print(f"  {'ok ' if ok else 'FAIL'} {name:34s} -> {v.verdict:22s} "
                  f"set={v.findings}")
            if not ok:
                print(f"       wanted headline={want_headline!r} finding={want_finding!r}")

    print("── CLEAN, demonstrated by the INVERSE (it must NOT fire) ──")
    for name, kw in CLEAN_MUST_NOT_FIRE:
        v = findings_for(**kw)
        ok = "CLEAN" not in v.findings
        if not ok:
            fails += 1
        if verbose or not ok:
            print(f"  {'ok ' if ok else 'FAIL'} CLEAN absent on {name:26s} -> {v.findings}")

    missing = set(SEVERITY) - fired - {"CHANNEL_ERROR"}
    if missing:
        fails += 1
        print(f"  FAIL labels never fired by the self-test: {sorted(missing)}")

    print("── labels with NO red demonstration, named per Core #12 ──")
    for rc, why in UNREACHABLE_AT_HEAD.items():
        print(f"  {rc} ({RATIFIED_EXIT_CODES[rc]}): {why}")
    print("── declared ambiguity cells (rule 3) ──")
    for cell, (item, why) in AMBIGUITY_CELLS.items():
        print(f"  {cell} -> {item}")

    print(f"\nself-test: {len(SELF_TEST_CASES)} label cases + "
          f"{len(CLEAN_MUST_NOT_FIRE)} inverse-CLEAN cases, {fails} failures")
    return 1 if fails else 0


# ──────────────────────── the LIVE (real-process) test ───────────────────────
# The self-test above feeds SYNTHETIC stderr, which proves the rules but not
# that the rules match what a real sanitizer/kernel actually prints. This mode
# compiles and RUNS six C programs and classifies their real (rc, stderr) — and
# it reports, on the same inputs, what the two instruments this file replaces
# would have said. That comparison is the point: three of the six rows are a
# live false-GREEN in the incumbent.

LIVE_PROGRAMS = {
    "clean": '#include <stdio.h>\nint main(void){printf("ok\\n");return 0;}\n',
    "leak": '#include <stdlib.h>\n#include <stdio.h>\n'
            'int main(void){volatile char*p=malloc(64);(void)p;printf("leaked\\n");return 0;}\n',
    "uaf": '#include <stdlib.h>\n#include <stdio.h>\n'
           'int main(void){char*p=malloc(8);free(p);p[0]=1;printf("%d\\n",p[0]);return 0;}\n',
    "segv": '#include <stdio.h>\n'
            'int main(void){volatile int*p=(int*)0;printf("before\\n");fflush(stdout);'
            '*p=1;return 0;}\n',
    "ubonly": '#include <stdio.h>\n'
              'int main(void){int x=2147483647;x=x+1;printf("%d\\n",x);return 0;}\n',
    "trapish": '#include <stdio.h>\n#include <stdlib.h>\n'
               'int main(void){fprintf(stderr,"trap[T_DivByZero]: division by zero at a.gg:1:1\\n");'
               'exit(101);}\n',
}
# (name, sanitized?, expected headline base, expected finding base)
LIVE_EXPECT = [
    ("clean",   False, "CLEAN",   "CLEAN"),
    ("leak",    True,  "LEAK",    "LEAK"),
    ("uaf",     True,  "CORRUPT", "CORRUPT"),
    ("segv",    False, "CRASH",   "CRASH"),
    ("ubonly",  True,  "UB",      "UB"),
    ("trapish", False, "TRAP",    "TRAP"),
]


def _incumbent_sweep_classify(rc, log):
    """`scripts/sanitize_sweep.sh`'s `classify_log` at HEAD, transcribed. Kept
    here ONLY so the live test can show what it publishes on the same inputs —
    it is not used to classify anything."""
    if rc == 124:
        return "TIMEOUT"
    if rc in (125, 126, 127):
        return "RUNNER_FAIL"
    labels = []
    if "ERROR: AddressSanitizer: stack-overflow" in log:
        labels.append("ASAN_stack-overflow")
    elif "ERROR: AddressSanitizer" in log:
        m = re.search(r"AddressSanitizer: ([a-z-]*)", log)
        labels.append("ASAN_" + (m.group(1) if m else ""))
    if "ERROR: LeakSanitizer" in log:
        labels.append("LEAK")
    if "runtime error:" in log:
        labels.append("UBSAN")
    return ",".join(labels) if labels else "CLEAN"


def _incumbent_map_verdict(rc, expected="prints ... then a loud failure", actual="before"):
    """`scripts/robustness_map.py`'s `_verdict` at HEAD, transcribed, same
    caveat. Shows the rc-139-is-WORKS false-green on a real SIGSEGV."""
    if rc != 0:
        if "loud failure" in expected:
            return "WORKS" if "before" in actual else "WRONG"
        return "TRAP"
    if "loud failure" in expected:
        return "WRONG"
    return "WORKS"


def live_test(verbose=True) -> int:
    import shutil
    import sys as _sys
    import tempfile
    _sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
    import proc_guard  # the shared runner: even a six-line live test spawns
                       # through it, so there is no "small enough to hand-roll"
                       # exemption for the next one.
    cc = os.environ.get("CC", "cc")
    if shutil.which(cc) is None:
        print(f"live-test SKIPPED: no {cc} on PATH")
        return 0
    fails = 0
    rows = []
    with tempfile.TemporaryDirectory(prefix="gg_verdict_live_") as td:
        for name, sanitized, want_headline, want_finding in LIVE_EXPECT:
            src = os.path.join(td, name + ".c")
            exe = os.path.join(td, name)
            with open(src, "w") as fh:
                fh.write(LIVE_PROGRAMS[name])
            flags = ["-O0", "-g", "-w"]
            if sanitized:
                flags.append("-fsanitize=address,undefined")
            b = proc_guard.run([cc, *flags, "-o", exe, src], timeout=120)
            if b.returncode != 0:
                print(f"  SKIP {name}: cc refused ({b.stderr.strip()[:80]})")
                continue
            env = dict(os.environ)
            if sanitized:
                # exitcode=1 is ASan's default; pass it in explicitly, because
                # the convention is an INPUT (this tree uses 0 and 99 too).
                env["ASAN_OPTIONS"] = "detect_leaks=1:exitcode=1"
            r = proc_guard.run([exe], timeout=120, env=env)
            v = findings_for("run", r.returncode, stderr=r.stderr, stdout=r.stdout,
                             sanitizer_exitcode=1 if sanitized else None)
            ok = _base(v.verdict) == want_headline and any(
                _base(f) == want_finding for f in v.findings)
            if not ok:
                fails += 1
            rows.append((name, r.returncode, v.verdict,
                         _incumbent_sweep_classify(r.returncode, r.stderr),
                         _incumbent_map_verdict(r.returncode), ok))
    print("── LIVE: real processes, real exit codes, real sanitizer output ──")
    print(f"  {'program':10s} {'rc':>5s}  {'verdict.py':24s} "
          f"{'sanitize_sweep@HEAD':22s} {'robustness_map@HEAD':20s}")
    for name, rc, mine, sweep, mapv, ok in rows:
        flag = "ok " if ok else "FAIL"
        star = "  <-- publishes GREEN over a real fault" if (
            sweep == "CLEAN" or mapv == "WORKS") and _base(mine) != "CLEAN" else ""
        print(f"  {flag} {name:10s} {rc:5d}  {mine:24s} {sweep:22s} {mapv:20s}{star}")
    print(f"live-test: {len(rows)} real programs, {fails} failures")
    return 1 if fails else 0


# ─────────────────────────────────── CLI ─────────────────────────────────────

def _read(path):
    if not path:
        return ""
    if path == "-":
        return sys.stdin.read()
    try:
        with open(path, "rb") as fh:
            return fh.read().decode("utf-8", "replace")
    except FileNotFoundError:
        return ""


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(
        description="The one verdict classifier (see the module docstring).")
    ap.add_argument("--phase", choices=PHASES,
                    help="which step produced this exit code. REQUIRED: rc 1 "
                         "means different things on build and on run.")
    ap.add_argument("--rc", type=int, help="the process's exit status")
    ap.add_argument("--stderr", help="file holding the process's stderr ('-' = stdin)")
    ap.add_argument("--stdout", help="file holding the process's stdout")
    ap.add_argument("--stderr-text", help="the stderr text itself")
    ap.add_argument("--timed-out", action="store_true",
                    help="the runner killed it on a deadline (no exit code of "
                         "its own on two of the three mechanisms)")
    ap.add_argument("--subject", choices=SUBJECTS, default=None,
                    help="WHOSE exit code this is. Defaults from --phase "
                         "(build=toolchain, run=program). Pass it explicitly "
                         "when the two come apart — `ggdef run <cell>` is a "
                         "TOOLCHAIN invocation spelled as a run.")
    ap.add_argument("--sanitizer-exitcode", type=int, default=None,
                    help="the ASAN_OPTIONS exitcode= this run was configured "
                         "with (0 in sanitize_sweep.sh, 99 elsewhere). An "
                         "INPUT, never an assumption.")
    ap.add_argument("--format", choices=("json", "sweep", "label", "tsv"),
                    default="json")
    ap.add_argument("--self-test", action="store_true")
    ap.add_argument("--prove-exclusive", action="store_true")
    ap.add_argument("--live-test", action="store_true",
                    help="compile and RUN six real C programs and classify their "
                         "real (rc, stderr) — the synthetic self-test proves the "
                         "rules, this proves they match reality")
    ap.add_argument("--quiet", action="store_true")
    a = ap.parse_args(argv)

    if a.self_test:
        return self_test(verbose=not a.quiet)
    if a.prove_exclusive:
        return prove_exclusive()
    if a.live_test:
        return live_test(verbose=not a.quiet)

    if a.phase is None or a.rc is None:
        ap.error("--phase and --rc are required (or use --self-test / "
                 "--prove-exclusive)")

    stderr = a.stderr_text if a.stderr_text is not None else _read(a.stderr)
    v = findings_for(a.phase, a.rc, stderr=stderr, stdout=_read(a.stdout),
                     timed_out=a.timed_out, subject=a.subject,
                     sanitizer_exitcode=a.sanitizer_exitcode)
    if a.format == "json":
        print(v.to_json())
    elif a.format == "sweep":
        print(v.to_sweep())
    elif a.format == "label":
        print(v.verdict)
    else:
        print(f"{v.verdict}\t{','.join(v.findings)}\t{v.rc}\t{v.phase}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
