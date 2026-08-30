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
import signal
import subprocess


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
    radius is unknown, and an unknown blast radius gets a single-pid kill — the
    same reasoning `scripts/reap_orphans.py` applies to its domain.
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
    import sys
    sys.exit(self_test())
