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
scratch root is named `gg_..._<owner pid>[_<creation stamp>]`. This is the WITNESS
for the whole predicate, so ENUMERATE it — the domain is exactly SCRATCH_PREFIX
below, and it is not confined to one file:

    grep -rn '"gg_' tests/

⚠ USE THAT, NOT A NARROWER PATTERN. An earlier version greped two prefixes in
`tests/integration.rs` and returned 16 — a SELECTION dressed as a census, missing
`spec_conformance.rs`, `security.rs` and `smith/main.rs` entirely, which no grep
of one file can reach. The command above returns 65. The ledger says exactly this
one line above the anchors ruling: *"derived by CENSUS … never by this list
(a cited list is a selection)"* (`docs/define-gorget/decisions.md:2040-2045`).

⊕ It replaces three line numbers that were WRONG FROM THE DAY THIS FILE WAS
COMMITTED — the same commit reshaped tests/integration.rs (net -152), so the cites
resolved against the pre-commit file. Two output-review passes certified them.

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
    scripts/reap_orphans.py --self-test    # six planted procs, 16 assertions
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
        self.recycled = False
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
            # SURFACED, not merely recorded: "the owner pid is gone" and "the
            # owner pid was REUSED by something else" are different facts, and
            # the header advertises the distinction. An earlier version set this
            # and never read it, which is a dead attribute asserting a capability
            # the report did not have.
            self.recycled = True
        else:
            self.owner_alive = True


def scan(root_filter=None):
    """Enumerate the domain. `root_filter` NARROWS it to scratch roots whose
    basename starts with that prefix.

    THIS PARAMETER IS A SAFETY BOUNDARY, NOT A CONVENIENCE. `--self-test` plants
    live processes and then exercises the KILL path, and it is wired into
    `cargo test --test lints`, which several agents run concurrently on a shared
    box. A self-test that reaps whatever an unrestricted scan happens to find is
    a box-wide SIGKILL fired by a test -- the precise incident the ownership
    predicate exists to prevent, committed by the tool that prevents it. So the
    self-test scans ONLY the uniquely-prefixed roots it created, and cannot see
    another agent's processes at all.
    """
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
                    # instead of being an unstated absence. The count survives a
                    # `root_filter`: it is a read-only statement about the
                    # domain, and nothing is ever signalled on its strength.
                    untagged_temp_dirs += 1
                elif root_filter is None or name.startswith(root_filter):
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
        "root_filter": root_filter,
    }


def kill_owned_tree(pid):
    """SIGKILL a process — its whole GROUP, but ONLY when it leads one.

    THE GUARD IS THE WHOLE POINT, AND IT MATTERS MORE HERE THAN ANYWHERE ELSE IN
    THIS TREE. `os.killpg(os.getpgid(pid))` on a process that is NOT a group
    leader signals whatever group it happens to be in — which, for anything
    spawned without `setpgid` / `start_new_session`, is some OTHER live session's
    group. `scripts/proc_guard.py` carries the same guard, but there the pids are
    OUR OWN CHILDREN; here they are STRANGERS, found by walking `/proc`. An
    unguarded group kill on a stranger is a name-matching `pkill` with extra
    steps — it signals processes nobody identified.

    `getpgid(pid) == pid` is exactly the harness's own spawn shape
    (`process_group(0)` makes the child a group LEADER), so a genuine orphan
    qualifies and its grandchildren go with it. Anything else gets a single-pid
    kill, because the blast radius of that group is unknown and an unknown blast
    radius is not ours to signal.

    Returns "group" / "pid" / None (failed) so the caller can PRINT which
    happened: an operator has to be able to see that a tree kill degraded.
    """
    try:
        pgid = os.getpgid(pid)
    except OSError:
        pgid = None
    if pgid is not None and pgid == pid:
        try:
            os.killpg(pgid, signal.SIGKILL)
            return "group"
        except OSError:
            pass
    try:
        os.kill(pid, signal.SIGKILL)
        return "pid"
    except OSError:
        return None


def report(res, preflight=False, do_reap=False) -> int:
    roots = res["roots"]
    dead_roots = [r for r in roots if not r.undecidable and r.owner_alive is False]
    print("=== orphan scan (predicate: OWNERSHIP — exe under a scratch root "
          "whose owning run is dead) ===")
    print(f"temp root:        {res['temp_root']}")
    if res.get("root_filter"):
        print(f"DOMAIN RESTRICTED to roots named {res['root_filter']}* — this is "
              f"NOT a whole-box scan and must not be read as one.")
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
        why = "RECYCLED" if r.recycled else "dead"
        print(f"    pid {pid:<8} owner {r.owner_pid} {why:<9} {exe}")
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
            how = kill_owned_tree(pid)
            if how is None:
                print(f"    FAILED to reap pid {pid}")
                rc = 1
            else:
                print(f"    reaped pid {pid} [{how} kill] ({exe})")
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
# SIX PLANTED PROCESSES, SIXTEEN ASSERTIONS.
#
# ⚠ SIX is what the harness SPAWNS AND TRACKS; control 6's stranger `sh` also
# forks a group member this file never holds a handle to, so seven processes
# exist at the high-water mark. The tracked six are what `finally` cleans up.
#
# Two of the CONTROLS are incidents this file has already caused or nearly
# caused, encoded as regression tests; a third is the incident that motivated
# the ownership predicate in the first place.
#
# ⚠ THE WHOLE SELF-TEST RUNS INSIDE A UNIQUE, PER-INVOCATION DOMAIN. It plants
# live processes and then exercises the KILL path, and it is wired into
# `cargo test --test lints`, which several agents run concurrently on this box.
# An earlier version called `report(scan(), do_reap=True)` on an UNRESTRICTED
# scan — a box-wide SIGKILL fired by a unit test, from the track whose own
# deliverable is "dry-run by default". Every `scan()` below is filtered to the
# prefix this invocation minted, so the test cannot SEE another agent's
# processes, let alone signal them.

def self_test() -> int:
    import shutil
    import subprocess
    import tempfile

    tmp = temp_root()
    fails = 0
    made = []
    procs = []
    # Unique per invocation, and deliberately NOT `_`-separable into digits: the
    # owner-tag parser takes the last whole decimal component, and a prefix like
    # `gg_st_1234_` would offer it a second candidate.
    pfx = f"gg_st{os.getpid()}x{time.time_ns()}_"

    def sleeper(path, **kw):
        # A real executable under a real root, so /proc/<pid>/exe resolves to it.
        shutil.copy("/bin/sleep", path)
        os.chmod(path, 0o755)
        p = subprocess.Popen([path, "600"], stdout=subprocess.DEVNULL,
                             stderr=subprocess.DEVNULL, start_new_session=True, **kw)
        procs.append(p)
        return p

    def check(name, cond, why):
        nonlocal fails
        if not cond:
            fails += 1
        print(f"  {'ok  ' if cond else 'FAIL'} {name:48s} {why}")

    def alive(pid):
        """RUNNING, not merely present in /proc.

        A killed process whose parent has not `wait()`ed is a ZOMBIE: its
        `/proc/<pid>` directory persists, so a bare `os.path.exists` reports it
        alive forever. Measured — it made this test fail on two controls that
        had in fact been killed correctly. The `sh` leader in control 6 stays
        alive by design, so its dead child stays unreaped by design too.
        """
        if pid is None:
            return False
        try:
            with open(f"{PROC}/{pid}/stat", "rb") as fh:
                raw = fh.read().decode("utf-8", "replace")
        except OSError:
            return False
        return raw[raw.rfind(")") + 2:].split()[0] != "Z"

    try:
        dead_owner = 999_999
        while proc_start_epoch(dead_owner) is not None:
            dead_owner -= 1

        # CONTROL 1 (RED) — a live process under a root whose owner pid is dead.
        # Named after the binary the original incident was about, so a name-based
        # reaper and an ownership-based one cannot be told apart by anything
        # except the predicate itself.
        red_root = os.path.join(tmp, f"{pfx}reap_{dead_owner}")
        os.makedirs(red_root, exist_ok=True)
        made.append(red_root)
        red_proc = sleeper(os.path.join(red_root, "async_select_diff"))

        # CONTROL 2 (GREEN) — same shape, but the owner is THIS live process.
        live_root = os.path.join(tmp, f"{pfx}reap_{os.getpid()}")
        os.makedirs(live_root, exist_ok=True)
        made.append(live_root)
        green_proc = sleeper(os.path.join(live_root, "async_select_diff"))

        # CONTROL 3 (NAME) — THE INCIDENT, encoded. An identically-named binary
        # that is NOT under a scratch root at all, standing in for a live
        # executor's `target/release/deps/integration-<hash>`. A
        # `pkill -f async_select_diff` kills it; an ownership predicate cannot
        # even SEE it, because it is absent from the domain rather than excluded
        # from it by a list somebody maintains. The prefix deliberately avoids
        # `gg_`: putting the control inside the scratch family would test
        # something else. It is asserted absent from EVERY bucket, not merely
        # from `reapable`, so the domain-filter above cannot be what excludes it.
        name_dir = tempfile.mkdtemp(prefix="selftest_namectl_")
        made.append(name_dir)
        outside = os.path.join(name_dir, "deps")
        os.makedirs(outside, exist_ok=True)
        name_proc = sleeper(os.path.join(outside, "async_select_diff"))

        # CONTROL 4 (UNPARSABLE TAG) — a root that IS in the scratch family but
        # whose name carries no whole-component pid. It must be UNDECIDABLE, not
        # reapable: guessing an owner out of a random suffix is how the first
        # version of this file flagged control 3 for reaping.
        notag_root = tempfile.mkdtemp(prefix=f"{pfx}notagx")
        made.append(notag_root)
        notag_proc = sleeper(os.path.join(notag_root, "async_select_diff"))

        # CONTROL 5 (OUT OF DOMAIN) — a live process inside an UNTAGGED temp
        # family, the `tempfile::TempDir` blind spot. It must be REPORTED as a
        # blind class, never counted as "0 orphans, all clear".
        untagged = tempfile.mkdtemp(prefix=".tmp")
        made.append(untagged)
        oob_proc = sleeper(os.path.join(untagged, "async_select_diff"))

        # ⭐ CONTROL 6 (NON-LEADER) — the control WITHOUT which this guard cannot
        # catch its own class. Every other sleeper above is spawned with
        # `start_new_session=True`, so `getpgid(pid) == pid` holds for all of
        # them and an UNGUARDED `killpg(getpgid(pid))` passes every one. This
        # control is the case that separates them:
        #
        #   a stranger `sh` leads its own group and then EXECs a sleep whose
        #   binary is OUTSIDE the domain — so the leader is not reapable — while
        #   a backgrounded child of it, in the SAME group and NOT a leader, runs
        #   a binary INSIDE a dead-owner root, so the child IS reapable.
        #
        # An unguarded group kill on the child signals its group, i.e. the
        # stranger leader too. The guard must degrade to a single-pid kill and
        # the leader must SURVIVE. That is a `pkill` with extra steps, caught.
        pg_root = os.path.join(tmp, f"{pfx}pgroup_{dead_owner}")
        os.makedirs(pg_root, exist_ok=True)
        made.append(pg_root)
        inside_bin = os.path.join(pg_root, "async_select_diff")
        shutil.copy("/bin/sleep", inside_bin)
        os.chmod(inside_bin, 0o755)
        outside_dir = tempfile.mkdtemp(prefix="selftest_leader_")
        made.append(outside_dir)
        outside_bin = os.path.join(outside_dir, "leader_sleep")
        shutil.copy("/bin/sleep", outside_bin)
        os.chmod(outside_bin, 0o755)
        pidfile = os.path.join(outside_dir, "childpid")
        leader = subprocess.Popen(
            ["sh", "-c", f"{inside_bin} 600 & echo $! > {pidfile}; exec {outside_bin} 600"],
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
            start_new_session=True)
        procs.append(leader)

        # Give every sleeper time to exec, and read control 6's child pid.
        member_pid = None
        for _ in range(60):
            if os.path.exists(pidfile) and open(pidfile).read().strip():
                member_pid = int(open(pidfile).read().strip())
                break
            time.sleep(0.05)
        time.sleep(0.4)

        res = scan(root_filter=pfx)
        reaped = {pid for pid, _, _ in res["reapable"]}
        left = {pid for pid, _, _ in res["leave"]}
        undec = {pid for pid, _, _ in res["undecidable"]}
        every = reaped | left | undec

        check("RED   orphan under a dead-owner root is FLAGGED",
              red_proc.pid in reaped, f"pid {red_proc.pid}")
        check("GREEN live-owner root is LEFT ALONE",
              green_proc.pid in left and green_proc.pid not in reaped,
              f"pid {green_proc.pid}")
        check("NAME  identically-named binary is ABSENT FROM EVERY BUCKET",
              name_proc.pid not in every,
              f"pid {name_proc.pid} — this is the pkill incident, as a test")
        check("NOTAG unparsable owner tag is UNDECIDABLE, not reapable",
              notag_proc.pid in undec and notag_proc.pid not in reaped,
              f"pid {notag_proc.pid}")
        check("OOD   untagged-temp process is out of domain, blind class counted",
              oob_proc.pid not in every and res["untagged_temp_dirs"] > 0,
              f"pid {oob_proc.pid}; {res['untagged_temp_dirs']} untagged dirs reported")
        check("PGRP  non-leader member of a STRANGER's group is FLAGGED",
              member_pid in reaped, f"pid {member_pid}")
        check("PGRP  the control really is a NON-leader (else it proves nothing)",
              member_pid is not None and os.getpgid(member_pid) != member_pid,
              f"pgid {os.getpgid(member_pid) if member_pid else '?'} != pid {member_pid}")
        check("preflight FAILS LOUDLY on the poisoned box",
              report(res, preflight=True) != 0, "rc != 0")

        # And the reaper actually reaps, when told to — still inside the domain
        # this invocation minted, never a whole-box scan.
        rc = report(scan(root_filter=pfx), do_reap=True)
        for _ in range(60):
            if not alive(red_proc.pid) and not alive(member_pid):
                break
            time.sleep(0.05)

        check("RED   orphan is GONE after --reap",
              not alive(red_proc.pid), "process exited")
        check("PGRP  non-leader member is GONE after --reap",
              not alive(member_pid), f"pid {member_pid}")
        check("PGRP  the STRANGER GROUP LEADER SURVIVED",
              alive(leader.pid),
              f"pid {leader.pid} — an unguarded killpg(getpgid(member)) kills it")
        check("GREEN survivor is STILL ALIVE after --reap",
              alive(green_proc.pid), "untouched")
        check("NAME  control is STILL ALIVE after --reap",
              alive(name_proc.pid), "untouched — the incident does not recur")
        check("OOD   control is STILL ALIVE after --reap",
              alive(oob_proc.pid), "untouched")
        check("NOTAG control is STILL ALIVE after --reap",
              alive(notag_proc.pid), "untouched — a guess is never a kill")
        check("--reap exits 0 when every kill succeeded", rc == 0, f"rc={rc}")
    finally:
        for p in procs:
            try:
                p.kill()
                p.wait(timeout=5)
            except Exception:                                     # noqa: BLE001
                pass
        # Single-pid kills only: these strays' groups are not ours to signal —
        # which is the same rule `kill_owned_tree` enforces, and the reason this
        # cleanup does not use it.
        for extra in (member_pid,):
            if extra:
                try:
                    os.kill(extra, signal.SIGKILL)
                except OSError:
                    pass
        for d in made:
            try:
                shutil.rmtree(d, ignore_errors=True)
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
