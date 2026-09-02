#!/usr/bin/env python3
"""Running a child with a deadline, and killing it WITH ITS WHOLE TREE.

The Python sibling of `src/proc_guard.rs`, and it exists for the same measured
reason: a plain kill reaps the direct child and leaves every grandchild alive,
spinning at ~100% CPU. On this box one such orphan burned a full core for forty
hours and skewed a round's measurements, because the test harness autoscales BOTH
its thread count and every load-adjusted deadline off `/proc/loadavg` — so a
spinner makes later runs use fewer threads and longer deadlines at the same time,
and a genuine hang quietly becomes a pass.

WHY `subprocess.run(..., timeout=N)` IS NOT ENOUGH
──────────────────────────────────────────────────
On expiry CPython does `process.kill()` — the DIRECT CHILD only, no process
group — and then `process.communicate()`, which BLOCKS on the pipes. A grandchild
that inherited the write end keeps them open, so the timeout handler itself hangs
with no deadline above it. That is the identical pair of defects the four
hand-rolled Rust runners had, in the stdlib call that looks like it handles them.

WHAT THIS DOES INSTEAD
──────────────────────
  * `start_new_session=True` makes the child a process-group (and session)
    LEADER, so `os.killpg(child.pid, SIGKILL)` reaches the whole tree.
  * On expiry it kills the GROUP first, then drains — the pipes close because
    every writer is dead, so the drain cannot block.
  * stdin is NULLED unless the caller supplies data. A child that inherits the
    parent's stdin can eat the parent's own input; measured in this repo, a
    `while read` loop driving a probe lost two thirds of its work list to a `gg`
    subprocess that consumed the pipe.

It returns the same `CompletedProcess` shape `subprocess.run` does, plus a
`timed_out` flag, so a caller can hand the outcome straight to
`scripts/verdict.py` — which is where the TIMEOUT verdict is decided. This module
reports facts; it does not decide what a run means.
"""
from __future__ import annotations

import os
import pathlib
import signal
import subprocess
import sys


class Result:
    """`subprocess.CompletedProcess` plus the one thing it cannot express."""

    __slots__ = ("args", "returncode", "stdout", "stderr", "timed_out")

    def __init__(self, args, returncode, stdout, stderr, timed_out):
        self.args = args
        self.returncode = returncode
        self.stdout = stdout
        self.stderr = stderr
        self.timed_out = timed_out


def run(cmd, timeout, text=True, env=None, cwd=None, stdin_data=None,
        capture_bytes=256 * 1024 * 1024):
    """Run `cmd` with a deadline, killing the whole process GROUP on expiry.

    `timeout` is required and has no default: every spawn in this tree owes a
    deadline, and a default would let a caller forget to choose one.
    """
    if timeout is None:
        raise ValueError("timeout is required — a spawn with no deadline is the "
                         "class this module exists to retire (see todo/t0842)")
    proc = subprocess.Popen(
        cmd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        stdin=subprocess.PIPE if stdin_data is not None else subprocess.DEVNULL,
        text=text,
        env=env,
        cwd=cwd,
        # THE LINE THAT MATTERS: the child leads its own process group, so the
        # negative-pid kill below reaches grandchildren.
        start_new_session=True,
    )
    timed_out = False
    try:
        out, err = proc.communicate(input=stdin_data, timeout=timeout)
    except subprocess.TimeoutExpired:
        timed_out = True
        _kill_group(proc.pid, proc)
        # Now that every writer is dead the pipes are closed, so this drain
        # cannot block — which is exactly what `subprocess.run`'s own timeout
        # path gets wrong when a grandchild survives.
        try:
            out, err = proc.communicate(timeout=30)
        except subprocess.TimeoutExpired:                      # pragma: no cover
            proc.kill()
            out, err = ("", "") if text else (b"", b"")
    if out is None:
        out = "" if text else b""
    if err is None:
        err = "" if text else b""
    # Bound the capture the same way the Rust side does: a miscompiled
    # infinite-printer must not be allowed to fill RAM. Truncation is marked, so
    # it can never be mistaken for the child's complete output.
    if len(out) > capture_bytes:
        out = out[:capture_bytes] + ("\n[TRUNCATED]" if text else b"\n[TRUNCATED]")
    if len(err) > capture_bytes:
        err = err[:capture_bytes] + ("\n[TRUNCATED]" if text else b"\n[TRUNCATED]")
    return Result(cmd, proc.returncode, out, err, timed_out)


def _kill_group(pid, proc=None):
    """SIGKILL a process group — but ONLY when `pid` really leads one.

    ⚠ THE GUARD IS NOT DEFENSIVE PADDING; ITS ABSENCE KILLED THIS FILE'S OWN
    SELF-TEST. `os.killpg(os.getpgid(pid))` on a process that is NOT a group
    leader signals whatever group it happens to be in — which, for anything
    spawned without `start_new_session`, is the CALLER'S OWN group. The first
    version of this helper did exactly that while cleaning up a stray process and
    SIGKILLed the interpreter running it (rc 137).

    So the group kill is conditional on the same structural fact the whole design
    rests on: `getpgid(pid) == pid` means we created that group and every member
    of it descends from the child we spawned. When it does not hold, the blast
    radius is unknown, and an unknown blast radius gets a single-pid kill.

    ⚠ THIS SENTENCE USED TO CLAIM `scripts/reap_orphans.py` APPLIED THE SAME
    REASONING, AND IT DID NOT — it carried the unguarded `killpg(getpgid(pid))`
    this guard exists to prevent, on pids that are STRANGERS rather than our own
    children. The claim was caught by an output review, not by a test, which is
    exactly what AGENTS.md Core #14 predicts of an invariant asserted only in
    prose. It is now true: `reap_orphans.kill_owned_tree` carries the same guard
    and its self-test has a NON-LEADER control that fails without it. Verify that
    before trusting this sentence again — do not inherit it.
    """
    try:
        pgid = os.getpgid(pid)
    except OSError:
        pgid = None
    if pgid == pid:
        try:
            os.killpg(pgid, signal.SIGKILL)
            return
        except OSError:
            pass
    try:
        if proc is not None:
            proc.kill()
        else:
            os.kill(pid, signal.SIGKILL)
    except OSError:
        pass


# ─────────────────────────── the Python census ───────────────────────────────
# The ENUMERATOR for the Python half of the process-runner class, and the one
# definition of it: `tests/lints.rs` shells out to `--census` rather than
# re-implementing the scan, the same precedent `todo_index.py` sets.
#
# ⚠ IT IS AN AST WALK, NOT A GREP, AND THE REASON IS MEASURED. The first version
# of the lint required `subprocess.` and `timeout=` on the SAME LINE, so it could
# not see
#     e = subprocess.run(
#         [str(DRIVER), ...],
#         capture_output=True, timeout=300)
# — which is how `run_selfhost`'s driver call was spelled at `05f72286`, before
# this change converted it: ONE OF THE EIGHT
# SITES THIS VERY CHANGE CONVERTED. The guard could not have found the thing it
# was written to find. It also skipped `#`-prefixed lines only, so a `timeout=`
# inside a docstring false-POSITIVED. An AST cannot make either mistake.
#
# TWO VERDICTS, because there are TWO CLASSES and folding them would make the
# baseline so large a real regression could hide in it:
#
#   RUNNER   -- a deadline-bearing spawn (`timeout=`), or a `Popen` (whose
#               `p.wait(timeout=5)` is the same hand-rolled runner spelled across
#               two statements, and that shape is live in this tree). These MUST
#               go through `run()`. A new one is a hard FAILURE.
#   NO-DEADLINE -- a spawn with no deadline at all. A different class: it BLOCKS
#               rather than orphaning, converting it is a per-site budget
#               decision, and it is filed as todo/t0842. Reported as a COUNT
#               RATCHET, not a hard failure -- it may only shrink.

SPAWN_ATTRS = {"run", "Popen", "call", "check_call", "check_output",
               "getoutput", "getstatusoutput"}
# Module aliases this walker recognises. ⚠ A STATED LIMIT: `import subprocess
# as X` for any other X is invisible to it. Measured 2026-08-30 -- the tree uses
# only `subprocess` and `_sp`, so the hole is theoretical today; but this
# enumerator is the witness the whole Python census rests on, and its blind spots
# belong in writing rather than in someone's head.
SPAWN_MODULES = ("subprocess", "sp", "_sp")

# Sites that legitimately spawn WITHOUT going through `run()`: file -> (COUNT,
# reason).
#
# ⚠ THE COUNT IS LOAD-BEARING, AND ITS ABSENCE WAS MEASURED. A bare
# file-allowlist exempts the FILE, so a new hand-rolled runner added to an
# already-listed file passes silently. Reproduced while RED-verifying this very
# guard: re-introducing a multi-line `subprocess.run(timeout=300)` into
# `robustness_map.py` — a file listed only for its import-time `git rev-parse` —
# did NOT fire. An exemption has to be as narrow as the thing exempted, or it is
# a hole with a comment on it.
CENSUS_ALLOW_COUNTS = {
    # This module IS the runner.
    "scripts/proc_guard.py": (3,
        "the shared runner itself, plus its self-test's deliberate "
        "`subprocess.run(timeout=)` ARM -- the control that MEASURES the stdlib "
        "leaving a grandchild alive. Routing that arm through run() would delete "
        "the comparison the self-test exists to make."),
    # The reaper's self-test plants live processes on purpose; a deadline on a
    # 600s sleeper it kills itself would be noise, and `run()` blocks.
    "scripts/reap_orphans.py": (2,
        "--self-test PLANTS long-lived sleepers and signals them itself; they "
        "are the subject, not children to be waited on. `run()` is a blocking "
        "call and cannot express them."),
    # Import-time repo-root discovery. Not a runner; a `git rev-parse` that
    # cannot hang without git itself being broken.
    "scripts/robustness_map.py": (1,
        "ONE import-time `git rev-parse --show-toplevel`. Not a runner: no child "
        "of its own, nothing to drain (todo/t0842 row). The count is 1 on "
        "purpose — every LANE spawn in this file goes through run()."),
}


def census(root=None):
    """Return [(relpath, lineno, call, has_timeout, flagged)] over tracked .py."""
    import ast
    import subprocess as _sp
    here = pathlib.Path(__file__).resolve().parent
    root = pathlib.Path(root) if root else here.parent
    out = _sp.run(["git", "ls-files", "-z", "*.py"], cwd=root,
                  capture_output=True, text=True)
    files = [f for f in out.stdout.split("\0") if f]
    rows = []
    for rel in sorted(files):
        f = root / rel
        try:
            tree = ast.parse(f.read_text(), filename=rel)
        except (OSError, SyntaxError):
            continue
        for node in ast.walk(tree):
            if not isinstance(node, ast.Call):
                continue
            fn = node.func
            name = None
            if isinstance(fn, ast.Attribute) and fn.attr in SPAWN_ATTRS:
                base = fn.value
                if isinstance(base, ast.Name) and base.id in SPAWN_MODULES:
                    name = f"subprocess.{fn.attr}"
            elif isinstance(fn, ast.Name) and fn.id in ("Popen", "check_output", "check_call"):
                name = fn.id
            if name is None:
                continue
            has_timeout = "timeout" in {k.arg for k in node.keywords if k.arg}
            is_runner = has_timeout or name.endswith("Popen")
            kind = "ALLOWED" if rel in CENSUS_ALLOW_COUNTS else (
                "RUNNER" if is_runner else "NO-DEADLINE")
            rows.append((rel, node.lineno, name, has_timeout, kind))
    return rows


# Shrink-only ratchet over the OTHER class (todo/t0842): spawns with no deadline
# at all. It may fall freely; it may not rise. Regenerate with
# `python3 scripts/proc_guard.py --census`.
NO_DEADLINE_BASELINE = 4


def print_census() -> int:
    rows = census()
    runners = [r for r in rows if r[4] == "RUNNER"]
    nodl = [r for r in rows if r[4] == "NO-DEADLINE"]
    counts = {}
    for rel, _, _, _, kind in rows:
        counts.setdefault(rel, {}).setdefault(kind, 0)
        counts[rel][kind] += 1
    print("=== python spawn census (AST over `git ls-files '*.py'`) ===")
    for rel in sorted(counts):
        kinds = ", ".join(f"{k}={v}" for k, v in sorted(counts[rel].items()))
        print(f"  {rel:42s} {kinds}")
    for rel, line, call, _, _ in runners:
        print(f"    ❌ RUNNER      {rel}:{line}  {call}")
    for rel, line, call, _, _ in nodl:
        print(f"       NO-DEADLINE {rel}:{line}  {call}   (todo/t0842)")
    print(f"total sites: {len(rows)}   runners-outside-proc_guard: {len(runners)}"
          f"   no-deadline: {len(nodl)} (baseline {NO_DEADLINE_BASELINE})")
    rc = 0
    # The allowlist is a COUNT, so a new spawn inside an exempted file is a
    # failure rather than a free ride.
    for rel, (want, why) in sorted(CENSUS_ALLOW_COUNTS.items()):
        got = sum(counts.get(rel, {}).values())
        if got != want:
            print(f"\n❌ ALLOWLIST COUNT MOVED for {rel}: {got} spawn site(s), "
                  f"expected {want}.\n   The exemption reads: {why}\n"
                  f"   A NEW site here is not covered by that reason — route it "
                  f"through proc_guard.run, or widen the entry deliberately.")
            rc = 1
    if runners:
        print("\n❌ Every deadline-bearing Python spawn goes through "
              "`proc_guard.run(cmd, timeout=N)`, which makes the child a "
              "process-group leader and kills the GROUP. `subprocess.run("
              "timeout=)` kills the direct child only and then blocks in "
              "communicate() on pipes a surviving grandchild holds open. If a "
              "site genuinely cannot, add its FILE to CENSUS_ALLOW with a reason.")
        rc = 1
    if len(nodl) > NO_DEADLINE_BASELINE:
        print(f"\n❌ NO-DEADLINE spawn count rose to {len(nodl)} (baseline "
              f"{NO_DEADLINE_BASELINE}). This ratchet only shrinks: give the new "
              f"site a deadline via proc_guard.run, or lower nothing and fix it.")
        rc = 1
    return rc


def self_test() -> int:
    """⭐ A GRANDCHILD MUST DIE WITH THE CHILD, and a plain `subprocess.run`
    leaves it alive. That contrast IS the test: both arms run the same program,
    and only the group-killing one comes back clean."""
    import pathlib
    import tempfile
    import time

    fails = 0
    with tempfile.TemporaryDirectory(prefix="gg_procguard_") as td:
        pidfile = pathlib.Path(td) / "gpid"
        script = f"sh -c 'echo $$ > {pidfile}; exec sleep 300' & exec sleep 300"

        def alive(p):
            return pathlib.Path(f"/proc/{p}").exists()

        def grandchild_pid():
            for _ in range(40):
                if pidfile.exists() and pidfile.read_text().strip():
                    return int(pidfile.read_text().strip())
                time.sleep(0.05)
            return None

        # ARM A — the stdlib. Recorded, not asserted: it documents WHY this
        # module exists, and it must not fail the gate on a platform where
        # CPython ever fixes it.
        pidfile.unlink(missing_ok=True)
        try:
            subprocess.run(["sh", "-c", script], capture_output=True, timeout=0.7)
        except subprocess.TimeoutExpired:
            pass
        g_a = grandchild_pid()
        time.sleep(0.5)
        leaked = g_a is not None and alive(g_a)
        print(f"  note subprocess.run(timeout=) left the grandchild alive: {leaked}")
        if g_a is not None and alive(g_a):
            _try_kill(g_a)

        # ARM B — this module. ASSERTED.
        pidfile.unlink(missing_ok=True)
        r = run(["sh", "-c", script], timeout=0.7)
        if not r.timed_out:
            fails += 1
            print("  FAIL the deadline did not fire")
        g_b = grandchild_pid()
        if g_b is None:
            fails += 1
            print("  FAIL the grandchild never ran; this test proves nothing")
        else:
            for _ in range(100):
                if not alive(g_b):
                    break
                time.sleep(0.05)
            if alive(g_b):
                fails += 1
                print(f"  FAIL grandchild {g_b} SURVIVED the group kill")
                _try_kill(g_b)
            else:
                print(f"  ok   grandchild {g_b} died with the process group")

        # The deadline is required, not defaulted.
        try:
            run(["true"], timeout=None)
            fails += 1
            print("  FAIL a spawn with no deadline was accepted")
        except ValueError:
            print("  ok   a spawn with no deadline is refused")

        # A normal run still behaves like subprocess.run.
        r = run(["sh", "-c", "printf hi; printf oops >&2; exit 3"], timeout=30)
        if (r.returncode, r.stdout, r.stderr, r.timed_out) != (3, "hi", "oops", False):
            fails += 1
            print(f"  FAIL ordinary run: {r.returncode} {r.stdout!r} {r.stderr!r}")
        else:
            print("  ok   an ordinary run returns rc / stdout / stderr unchanged")

    print(f"proc_guard self-test: {fails} failures")
    return 1 if fails else 0


def _try_kill(pid):
    """Clean up one stray pid. Routed through the same guarded helper, because
    the strays this self-test produces are precisely the ones whose group is the
    interpreter's own."""
    _kill_group(pid)


if __name__ == "__main__":
    if "--census" in sys.argv:
        sys.exit(print_census())
    sys.exit(self_test())
