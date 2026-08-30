#!/usr/bin/env python3
"""Find — and, only when told to, kill — test binaries orphaned by a dead harness.

WHY, AND WHY THE OBVIOUS PREDICATE IS THE WRONG ONE
───────────────────────────────────────────────────
A fixture binary that outlives its harness burns a core forever, and this box
autoscales BOTH the thread count and every load-adjusted deadline off
`/proc/loadavg`. One spinner therefore poisons every later measurement in both
directions: fewer threads AND longer deadlines, so hangs quietly become passes.
That is not hypothetical — an orphan ran for 40 hours here and skewed a round.

Two predicates are tempting and both are WRONG:

  * **BY NAME.** `pkill -f 'integration-[0-9a-f]'` has already, in this tree,
    killed a LIVE executor's release test binary mid-gate. A name cannot tell
    two things apart that share a spelling — the same defect AGENTS.md Core #2
    forbids in the compiler, with the same failure mode.
  * **BY PARENTAGE.** `ppid == 1` is not the orphan condition. Measured here: a
    live, wanted `cargo test` sat at `ppid == 1`, so the predicate would have
    killed it. And this box is PID-namespaced — pid 1 is a `bash` and session
    leaders show **PPID 0** — so the same predicate is also BLIND to real
    orphans. Wrong in both directions at once.

THE PREDICATE IS OWNERSHIP
──────────────────────────
    REAPABLE(pid) := readlink(/proc/<pid>/exe) resolves UNDER a harness scratch
                     root, AND that root's OWNER RUN is dead
                     (owner pid gone, or the pid was recycled)

This is AGENTS.md Core #3 — *register ownership at the value's birth* — applied
to processes instead of heap values. The harness already writes the tag: every
scratch root is named `gg_..._<owner pid>[_<creation stamp>]`
(`tests/integration.rs:36903`, `:36070`, `:38315`, …).

Why that is STRUCTURALLY safe rather than merely careful: a live executor's
`target/release/deps/integration-<hash>` is **not under a scratch root at all**,
so it is ABSENT FROM THE PREDICATE'S DOMAIN — not excluded by an allowlist
somebody has to maintain. There is no spelling that can bring it back in.

And the domain is complete for the class it covers, by an invariant rather than
by hope: the SAME event (SIGKILL of the harness) both orphans the child and
prevents `TmpRootGuard::drop` from removing the directory — the code says so
itself, "Drop does NOT run on SIGKILL". An orphan implies a surviving root.

THREE BUCKETS, AND THE THIRD IS THE POINT
─────────────────────────────────────────
  (i)   tagged root, owner DEAD    → REAPABLE
  (ii)  tagged root, owner ALIVE   → LEAVE (this is where a peer agent's live
                                     binary lands, by construction)
  (iii) UNDECIDABLE                → REPORTED, never folded into zero

Bucket (iii) is load-bearing. A reaper that prints `0 orphans, rc=0` while
looking at only the families it can parse is the SAME defect as the classifier
this round replaces: a clean report over a real fault. So everything this tool
cannot decide is COUNTED AND NAMED.

WHAT THE DOMAIN CANNOT SEE (stated, not implied)
────────────────────────────────────────────────
  * `tempfile::TempDir` / `env::temp_dir()` families with NO owner tag. They
    produce `/tmp/.tmpXXXXXX`, and no glob can recover an owner from that.
    `--report` counts them so the blind spot has a number.
  * `/proc/<pid>/exe` unreadable (a different uid, or the process exiting under
    us) — UNDECIDABLE, never "absent".
  * processes outside this PID namespace — invisible, and named as such.

The reference-grade fix for all three is the Phase-2 run ledger (a directory the
harness writes at spawn and unlinks at clean exit), which needs no name parsing
at all. It is filed, not skipped.

DRY-RUN BY DEFAULT
──────────────────
Killing requires `--reap`, on the `scripts/round_cleanup.sh` precedent
("dry-runs by default, --yes applies"). `--preflight` is the loud gate: it exits
non-zero on a poisoned box so a sweep refuses to measure one, and
`GG_SKIP_PREFLIGHT=1` is the escape hatch that keeps an autonomous round from
deadlocking on a single stale orphan.

    scripts/reap_orphans.py                # report (dry run), always exit 0
    scripts/reap_orphans.py --preflight    # exit 1 if the box is poisoned
    scripts/reap_orphans.py --reap         # SIGKILL the reapable set
    scripts/reap_orphans.py --self-test    # four controls, incl. the NAME control
"""
from __future__ import annotations

import argparse
import os
import re
import signal
import sys
import time

PROC = "/proc"
# Scratch roots live under the temp dir and are named with their creator's pid.
# `gg_` is the family prefix every harness site uses.
SCRATCH_PREFIX = "gg_"
# What an UNTAGGED temp root looks like: Rust's `tempfile` and C's `mkdtemp`
# both mint `.tmpXXXXXX` / `tmpXXXXXX` with no owner in the name. This is the
# blind spot, and it is counted rather than left implicit.
UNTAGGED_TEMP_PREFIX = ".tmp"
# An owner tag is a decimal component that could be a pid. Anything larger is a
# millisecond/nanosecond creation stamp (`gg_runtime_diff_<pid>_<millis>`), which
# is why the pid_max bound is the discriminator and not the position.
PID_MAX = 4194304


def _pid_max() -> int:
    try:
        with open("/proc/sys/kernel/pid_max") as fh:
            return int(fh.read().strip())
    except OSError:
        return PID_MAX


def temp_root() -> str:
    return os.environ.get("TMPDIR", "/tmp").rstrip("/") or "/tmp"


def _clk_tck() -> int:
    try:
        return os.sysconf("SC_CLK_TCK")
    except (ValueError, OSError):
        return 100


def boot_epoch() -> float:
    with open("/proc/uptime") as fh:
        return time.time() - float(fh.read().split()[0])


def proc_start_epoch(pid: int):
    """Wall-clock start time of `pid`, or None if it is gone/unreadable.

    `/proc/<pid>/stat` field 22 is starttime in clock ticks since boot. It is
    what makes PID RECYCLING detectable: this box's pid_max is small enough that
    a long-lived scratch root can outlive a full pid wrap, and reaping on a
    recycled pid would kill an innocent process.
    """
    try:
        with open(f"{PROC}/{pid}/stat", "rb") as fh:
            raw = fh.read().decode("utf-8", "replace")
    except OSError:
        return None
    # comm can contain spaces AND parens; the fields after the last ')' are safe.
    tail = raw[raw.rfind(")") + 2:].split()
    try:
        return boot_epoch() + int(tail[19]) / _clk_tck()
    except (IndexError, ValueError):
        return None


def exe_of(pid: int):
    """The process's executable path, or a reason we could not read it.

    ⚠ `readlink` on a process whose binary was UNLINKED returns
    `/path/to/bin (deleted)`. Stripping that suffix is not cosmetic: an orphan
    whose scratch dir was partially cleaned is exactly the case this tool exists
    for, and an unstripped suffix silently fails the under-root test.
    """
    try:
        target = os.readlink(f"{PROC}/{pid}/exe")
    except FileNotFoundError:
        return None, "no /proc entry (kernel thread, or it exited)"
    except PermissionError:
        return None, "permission denied (different uid)"
    except OSError as e:
        return None, f"unreadable: {e.__class__.__name__}"
    if target.endswith(" (deleted)"):
        target = target[: -len(" (deleted)")]
    return target, None


class Root:
    """One scratch root, and what we can say about who owns it."""

    def __init__(self, path: str):
        self.path = path
        self.base = os.path.basename(path)
        self.owner_pid = None
        self.undecidable = None
        self.owner_alive = None
        self.created = None
        try:
            self.created = os.stat(path).st_mtime
        except OSError:
            pass
        pid_max = _pid_max()
        # ⚠ STRICTLY WHOLE COMPONENTS. An earlier version of this used
        # `re.findall(r"\d+", base)`, which digs digits out of the MIDDLE of a
        # word — and the self-test caught it reaping a control whose random
        # mkdtemp suffix happened to contain a `7`, i.e. inventing an owner that
        # never existed. That is the same "two things that share a spelling"
        # failure the NAME predicate has, reintroduced one level down. An owner
        # tag is a whole `_`-separated component that is ENTIRELY decimal, or
        # there is no owner tag and the root is UNDECIDABLE.
        cands = [int(t) for t in self.base.split("_")
                 if t.isdigit() and 0 < int(t) <= pid_max]
        if not cands:
            self.undecidable = ("no owner tag: no whole `_`-separated component "
                                "is a plausible pid")
            return
        # The producers all write the pid LAST or second-to-last; a creation
        # stamp (millis/nanos) is filtered out by the pid_max bound above, so the
        # last surviving candidate is the owner.
        self.owner_pid = cands[-1]
        start = proc_start_epoch(self.owner_pid)
        if start is None:
            self.owner_alive = False
            return
        # Recycled pid: a live process that started AFTER this root was created
        # is NOT the owner. 5s of slack absorbs mtime/starttime granularity.
        if self.created is not None and start > self.created + 5.0:
            self.owner_alive = False
            self.recycled = True
        else:
            self.owner_alive = True


def scan(verbose=False):
    root_dir = temp_root()
    roots = []
    untagged_temp_dirs = 0
    try:
        # scandir, not listdir+isdir: this runs as a PRE-FLIGHT before every
        # sweep, and /tmp here currently holds ~100k entries. scandir uses the
        # dirent d_type so the common case costs no extra stat(2) per entry.
        with os.scandir(root_dir) as it:
            for entry in it:
                name = entry.name
                is_untagged = name.startswith(UNTAGGED_TEMP_PREFIX)
                if not (is_untagged or name.startswith(SCRATCH_PREFIX)):
                    continue
                try:
                    if not entry.is_dir(follow_symlinks=False):
                        continue
                except OSError:
                    continue
                if is_untagged:
                    # `tempfile::TempDir` / mkdtemp produce `.tmpXXXXXX` with NO
                    # owner tag at all. Counted so the blind spot has a number
                    # instead of being an unstated absence.
                    untagged_temp_dirs += 1
                else:
                    roots.append(Root(entry.path))
    except OSError as e:
        print(f"cannot list {root_dir}: {e}", file=sys.stderr)
    roots.sort(key=lambda r: r.path)

    by_path = {r.path: r for r in roots}
    reapable, leave, undecidable = [], [], []
    seen_pids = 0
    for entry in os.listdir(PROC):
        if not entry.isdigit():
            continue
        pid = int(entry)
        if pid == os.getpid():
            continue
        seen_pids += 1
        exe, why = exe_of(pid)
        if exe is None:
            # Only a process we cannot READ is undecidable; the vast majority
            # are simply outside the domain and must not be reported at all.
            if why and "no /proc entry" not in why:
                undecidable.append((pid, None, why))
            continue
        owner_root = None
        for path, r in by_path.items():
            if exe == path or exe.startswith(path + os.sep):
                owner_root = r
                break
        if owner_root is None:
            continue                      # OUT OF DOMAIN — structurally safe
        if owner_root.undecidable:
            undecidable.append((pid, exe, f"{owner_root.base}: {owner_root.undecidable}"))
        elif owner_root.owner_alive:
            leave.append((pid, exe, owner_root))
        else:
            reapable.append((pid, exe, owner_root))
    return {
        "roots": roots,
        "reapable": reapable,
        "leave": leave,
        "undecidable": undecidable,
        "untagged_temp_dirs": untagged_temp_dirs,
        "pids_examined": seen_pids,
        "temp_root": root_dir,
    }


def report(res, preflight=False, do_reap=False) -> int:
    roots = res["roots"]
    dead_roots = [r for r in roots if not r.undecidable and r.owner_alive is False]
    print("=== orphan scan (predicate: OWNERSHIP — exe under a scratch root "
          "whose owning run is dead) ===")
    print(f"temp root:        {res['temp_root']}")
    print(f"pids examined:    {res['pids_examined']} "
          f"(this PID namespace only — processes outside it are INVISIBLE, "
          f"not absent)")
    print(f"scratch roots:    {len(roots)} tagged "
          f"({len(dead_roots)} owner-dead), "
          f"{sum(1 for r in roots if r.undecidable)} with no parsable owner tag")
    print(f"untagged .tmp dirs: {res['untagged_temp_dirs']} "
          f"(tempfile::TempDir / mkdtemp — NO owner tag exists, so this tool is "
          f"BLIND to any process living in one. This is the domain gap the "
          f"Phase-2 run ledger closes.)")
    print(f"REAPABLE:         {len(res['reapable'])}")
    for pid, exe, r in res["reapable"]:
        print(f"    pid {pid:<8} owner {r.owner_pid} dead   {exe}")
    print(f"LEAVE (owner alive): {len(res['leave'])}")
    for pid, exe, r in res["leave"]:
        print(f"    pid {pid:<8} owner {r.owner_pid} ALIVE  {exe}")
    print(f"UNDECIDABLE:      {len(res['undecidable'])}   "
          f"⚠ counted, never folded into zero")
    for pid, exe, why in res["undecidable"]:
        print(f"    pid {pid:<8} {why}   {exe or ''}")

    rc = 0
    if do_reap:
        for pid, exe, r in res["reapable"]:
            try:
                # Kill the process GROUP: the orphan is a group leader (the
                # harness spawns with process_group(0)), so its own children go
                # with it. A bare kill(pid) leaves the grandchildren spinning —
                # which is how this class survives a "cleanup" in the first place.
                try:
                    os.killpg(os.getpgid(pid), signal.SIGKILL)
                except (ProcessLookupError, PermissionError):
                    os.kill(pid, signal.SIGKILL)
                print(f"    reaped pid {pid} ({exe})")
            except OSError as e:
                print(f"    FAILED to reap pid {pid}: {e}")
                rc = 1
    elif res["reapable"]:
        print("\n(dry run — nothing was signalled. Pass --reap to act.)")

    if preflight:
        if res["reapable"] and not do_reap:
            print("\n❌ POISONED BOX: a test binary from a dead run is still "
                  "alive. Every load-adjusted deadline and the autoscaled thread "
                  "count are computed off /proc/loadavg, so this run's numbers "
                  "would be untrustworthy in BOTH directions.")
            print("   Fix:  python3 scripts/reap_orphans.py --reap")
            print("   Skip: GG_SKIP_PREFLIGHT=1 (an autonomous round must not "
                  "deadlock on one stale orphan — but the numbers are then "
                  "explicitly suspect)")
            rc = 1
        if res["undecidable"]:
            # A report, not a gate: undecidable is a KNOWN limit of the domain,
            # and failing on it would make the preflight unrunnable rather than
            # informative. It is printed above so it can never read as zero.
            print(f"\n⚠ {len(res['undecidable'])} process(es) this predicate "
                  f"cannot decide — see above. Not a gate; a stated blind spot.")
    return rc


# ─────────────────────────────── the self-test ───────────────────────────────
# Four controls. Control 3 IS the incident that motivated the ownership
# predicate, encoded as a regression test.

def self_test() -> int:
    import shutil
    import subprocess
    import tempfile

    tmp = temp_root()
    fails = 0
    made = []
    procs = []

    def sleeper(path):
        # A real executable under a real root, so /proc/<pid>/exe resolves to it.
        shutil.copy("/bin/sleep", path)
        os.chmod(path, 0o755)
        p = subprocess.Popen([path, "600"], stdout=subprocess.DEVNULL,
                             stderr=subprocess.DEVNULL, start_new_session=True)
        procs.append(p)
        return p

    try:
        # CONTROL 1 (RED) — a live process under a root whose owner pid is dead.
        dead_owner = 999_999
        while proc_start_epoch(dead_owner) is not None:
            dead_owner -= 1
        red_root = os.path.join(tmp, f"gg_selftest_reap_{dead_owner}")
        os.makedirs(red_root, exist_ok=True)
        made.append(red_root)
        # Name it after the binary the original incident was about, so a
        # name-based reaper and an ownership-based one cannot be told apart by
        # anything except the predicate itself.
        red_proc = sleeper(os.path.join(red_root, "async_select_diff"))

        # CONTROL 2 (GREEN) — same shape, but the owner is THIS live process.
        live_root = os.path.join(tmp, f"gg_selftest_reap_{os.getpid()}")
        os.makedirs(live_root, exist_ok=True)
        made.append(live_root)
        green_proc = sleeper(os.path.join(live_root, "async_select_diff"))

        # CONTROL 3 (NAME) — THE INCIDENT, encoded. An identically-named binary
        # that is NOT under a scratch root at all, standing in for a live
        # executor's `target/release/deps/integration-<hash>`. A
        # `pkill -f async_select_diff` kills it; an ownership predicate cannot
        # even SEE it, because it is absent from the domain rather than excluded
        # from it. ⚠ The prefix deliberately avoids `gg_`: putting the control
        # inside the scratch family would test something else.
        name_dir = tempfile.mkdtemp(prefix="selftest_namectl_")
        made.append(name_dir)
        outside = os.path.join(name_dir, "deps")
        os.makedirs(outside, exist_ok=True)
        name_proc = sleeper(os.path.join(outside, "async_select_diff"))

        # CONTROL 5 (UNPARSABLE TAG) — a root that IS in the scratch family but
        # whose name carries no whole-component pid. It must be UNDECIDABLE, not
        # reapable: guessing an owner out of a random suffix is how the first
        # version of this file nearly killed control 3.
        untagged_family = tempfile.mkdtemp(prefix="gg_selftest_notag_x")
        made.append(untagged_family)
        notag_proc = sleeper(os.path.join(untagged_family, "async_select_diff"))

        # CONTROL 4 (OUT OF DOMAIN) — a live process inside an UNTAGGED temp
        # family, the `tempfile::TempDir` blind spot. It must be REPORTED as
        # undecidable-or-invisible, never counted as "0 orphans, all clear".
        untagged = tempfile.mkdtemp(prefix=".tmp")
        made.append(untagged)
        oob_proc = sleeper(os.path.join(untagged, "async_select_diff"))

        time.sleep(0.4)
        res = scan()
        reaped = {pid for pid, _, _ in res["reapable"]}
        left = {pid for pid, _, _ in res["leave"]}

        def check(name, cond, why):
            nonlocal fails
            if not cond:
                fails += 1
            print(f"  {'ok  ' if cond else 'FAIL'} {name:46s} {why}")

        check("RED   orphan under a dead-owner root is FLAGGED",
              red_proc.pid in reaped, f"pid {red_proc.pid}")
        check("GREEN live-owner root is LEFT ALONE",
              green_proc.pid in left and green_proc.pid not in reaped,
              f"pid {green_proc.pid}")
        check("NAME  identically-named binary OUTSIDE the domain SURVIVES",
              name_proc.pid not in reaped,
              f"pid {name_proc.pid} — this is the pkill incident, as a test")
        check("OOD   untagged-temp process is not counted as absent",
              oob_proc.pid not in reaped and res["untagged_temp_dirs"] > 0,
              f"pid {oob_proc.pid}; {res['untagged_temp_dirs']} untagged dirs reported")
        undec = {pid for pid, _, _ in res["undecidable"]}
        check("NOTAG unparsable owner tag is UNDECIDABLE, not reapable",
              notag_proc.pid in undec and notag_proc.pid not in reaped,
              f"pid {notag_proc.pid}")
        check("preflight FAILS LOUDLY on the poisoned box",
              report(res, preflight=True) != 0, "rc != 0")

        # And the reaper actually reaps, when told to.
        rc = report(scan(), do_reap=True)
        time.sleep(0.4)
        check("RED   orphan is GONE after --reap",
              red_proc.poll() is not None, "process exited")
        check("GREEN survivor is STILL ALIVE after --reap",
              green_proc.poll() is None, "untouched")
        check("NAME  control is STILL ALIVE after --reap",
              name_proc.poll() is None, "untouched — the incident does not recur")
        check("OOD   control is STILL ALIVE after --reap",
              oob_proc.poll() is None, "untouched")
        check("NOTAG control is STILL ALIVE after --reap",
              notag_proc.poll() is None, "untouched — a guess is never a kill")
        check("--reap exits 0 when every kill succeeded", rc == 0, f"rc={rc}")
    finally:
        for p in procs:
            try:
                p.kill()
                p.wait(timeout=5)
            except Exception:                                     # noqa: BLE001
                pass
        for d in made:
            try:
                import shutil as _sh
                _sh.rmtree(d, ignore_errors=True)
            except Exception:                                     # noqa: BLE001
                pass
    print(f"\nself-test: {fails} failures")
    return 1 if fails else 0


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--reap", action="store_true",
                    help="actually SIGKILL the reapable set. Without it this is "
                         "a DRY RUN (scripts/round_cleanup.sh's precedent).")
    ap.add_argument("--preflight", action="store_true",
                    help="exit non-zero when the box is poisoned, so a sweep "
                         "refuses to measure one")
    ap.add_argument("--self-test", action="store_true")
    a = ap.parse_args(argv)
    if a.self_test:
        return self_test()
    return report(scan(), preflight=a.preflight, do_reap=a.reap)


if __name__ == "__main__":
    sys.exit(main())
