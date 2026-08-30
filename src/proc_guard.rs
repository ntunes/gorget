//! Spawning a child process with a deadline, and killing it WITH ITS WHOLE TREE.
//!
//! # Why this is one module and not five copies
//!
//! Five hand-rolled `run_with_deadline` clones existed in this repo. Exactly
//! ONE of them put the child in its own process group; the other four called a
//! plain `child.kill()`, which reaps the direct child and leaves every
//! grandchild alive. `gg run` spawns the compiled fixture, and some fixtures
//! fork their own workers, so "the direct child" is rarely the whole story: the
//! grandchild survives, reparents, and spins at ~100% CPU forever.
//!
//! That is not a hypothetical. One orphaned fixture binary burned a full core
//! for forty hours on this box. It matters more than the wasted CPU, because the
//! test harness autoscales its thread count AND every load-adjusted deadline off
//! `/proc/loadavg`: a spinner therefore makes later runs use fewer threads and
//! longer deadlines at the same time, so a genuine hang quietly becomes a pass.
//! One leaked process corrupts every subsequent measurement in both directions.
//!
//! The prose describing that defect already existed — one file away from four
//! copies that still had it. Prose does not propagate a fix; a shared function
//! does, and `tests/lints.rs::process_spawn_deadline_arm_count` stops copy six
//! from appearing.
//!
//! # The three properties a runner must have, and why each was bought
//!
//! 1. **Spawn as a process-group LEADER** (`process_group(0)`, so pgid == pid)
//!    and signal the NEGATIVE pid, so the kill reaches grandchildren.
//! 2. **Drain stdout and stderr on background threads.** A child writing more
//!    than the ~64 KiB pipe buffer blocks until the parent reads, while the
//!    parent is polling `try_wait()` waiting for it to exit: a classic deadlock,
//!    and four of the five clones also joined those threads unconditionally
//!    AFTER killing the child, so a grandchild holding the pipe write end hung
//!    the timeout handler itself, with no deadline above it.
//! 3. **CAP the drain.** A miscompiled infinite-printer will otherwise fill RAM;
//!    crossing the cap is a LOUD failure, never silently truncated output
//!    returned as if it were complete.
//!
//! # What this module deliberately does NOT do
//!
//! It does not decide what a run MEANS. Exit codes, sanitizer output and the
//! CLEAN/LEAK/CRASH/TIMEOUT verdict alphabet belong to `scripts/verdict.py`, the
//! one classifier every lane shares. This module reports facts — the output, or
//! which way the run failed — and nothing else.

use std::io::Read;
use std::process::{Child, Command, Output, Stdio};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

/// Per-stream capture cap. 256 MiB is far above any legitimate fixture's output
/// and far below "the box swaps".
pub const DEFAULT_CAPTURE_CAP: usize = 256 * 1024 * 1024;

/// How a deadline-bounded run failed. `Ok(Output)` is the only success.
///
/// This is an enum rather than a bare `TimedOut` because the two failures need
/// DIFFERENT reactions and used to be conflated: a deadline means the program
/// hung, while an overflow means it produced runaway output and the capture was
/// truncated — reporting truncated bytes as a complete result is a miscompile
/// hiding as a pass.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RunFailure {
    /// The child outlived its deadline and its whole process group was killed.
    Deadline { secs: u64 },
    /// The child crossed the per-stream capture cap and was killed.
    Overflow { cap: usize },
}

/// SIGKILL a child AND its whole process group, then reap it.
///
/// `run_with_deadline` spawns every child as a process-group *leader*
/// (`process_group(0)`, so the group id == the child's pid), so signalling the
/// NEGATIVE pid reaps the entire tree — not just the direct child.
///
/// ⚠ THE GROUP KILL IS GUARDED, AND THE GUARD IS THE DIFFERENCE BETWEEN A
/// CONTRACT AND A COMMENT. The paragraph above states a PRECONDITION, and a
/// precondition stated only in prose is one every future caller has to remember.
/// One already did not: `gg test --parallel` in `src/main.rs` spawns its workers
/// with no process group at all, so `kill(-pid)` there addresses a group the
/// worker does not lead — `ESRCH`, only the direct-child fallback fires, and the
/// worker's own grandchildren orphan exactly as before. The comment was true of
/// the helper and false at that call site (Core #14).
///
/// So the group kill now happens only when `getpgid(pid) == pid` — i.e. when the
/// child really does lead the group, which is precisely what `process_group(0)`
/// guarantees. Every present and future caller is safe without remembering
/// anything, and a caller that did NOT set the group gets a correct single-child
/// kill instead of a signal aimed at some other group. `scripts/proc_guard.py`
/// and `scripts/reap_orphans.py` carry the identical guard for the identical
/// reason.
///
/// Unix only (this box is Linux); elsewhere it degrades to a direct kill, which
/// is the best a portable API offers.
#[cfg(unix)]
pub fn kill_process_tree(child: &mut Child) {
    // `kill(2)` and `getpgid(2)` are always linked (std depends on libc), so
    // declaring the symbols here avoids a direct `libc` dependency for two calls.
    unsafe extern "C" {
        fn kill(pid: i32, sig: i32) -> i32;
        fn getpgid(pid: i32) -> i32;
    }
    const SIGKILL: i32 = 9;
    let pid = child.id() as i32;
    // Negative pid ⇒ signal the process GROUP led by `pid`. Only correct when
    // `pid` LEADS that group; otherwise it reaches whatever group the child
    // happens to be in, which may be the caller's own.
    if unsafe { getpgid(pid) } == pid {
        unsafe {
            kill(-pid, SIGKILL);
        }
    }
    // Always target the direct child too, then reap it so it does not linger as
    // a zombie. This is the whole kill when the child leads no group.
    child.kill().ok();
    child.wait().ok();
}

#[cfg(not(unix))]
pub fn kill_process_tree(child: &mut Child) {
    child.kill().ok();
    child.wait().ok();
}

/// Drain a child stream in a background thread, capturing at most `cap` bytes.
///
/// Past the cap it KEEPS READING and DISCARDS — it must not stop, or the pipe
/// fills, the child blocks on `write`, and the poll loop above deadlocks against
/// it. The `overflow` flag is what tells the caller to kill the runaway. For any
/// well-behaved child this is behaviourally identical to `read_to_end`, because
/// the flag never trips.
///
/// A signal (SIGCHLD etc.) can interrupt a blocking read; `read_to_end` retried
/// EINTR internally, so `Interrupted` is a transient retry and not
/// end-of-stream — a bare `break` there silently truncates the capture.
fn capped_drain<R: Read + Send + 'static>(
    mut reader: R,
    overflow: Arc<AtomicBool>,
    cap: usize,
) -> std::thread::JoinHandle<Vec<u8>> {
    std::thread::spawn(move || {
        let mut buf: Vec<u8> = Vec::new();
        let mut chunk = [0u8; 64 * 1024];
        loop {
            match reader.read(&mut chunk) {
                Ok(0) => break,
                Ok(n) => {
                    if buf.len() < cap {
                        let room = cap - buf.len();
                        buf.extend_from_slice(&chunk[..n.min(room)]);
                        if buf.len() >= cap {
                            overflow.store(true, Ordering::Relaxed);
                        }
                    }
                }
                Err(ref e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
                Err(_) => break,
            }
        }
        buf
    })
}

/// Run `cmd` with a deadline and the default capture cap.
pub fn run_with_deadline(cmd: &mut Command, timeout: Duration) -> Result<Output, RunFailure> {
    run_with_deadline_opts(cmd, timeout, DEFAULT_CAPTURE_CAP, None)
}

/// Run `cmd` with a deadline, an explicit capture cap, and optional stdin bytes.
///
/// `stdin_data` exists because one caller feeds a fixture on stdin. Routing that
/// through here rather than hand-rolling a sixth spawn loop is the whole point:
/// the stdin variant used to be its own copy, and it was one of the four missing
/// the process group.
///
/// `stdin` is NULLED when no data is supplied. A child that inherits the
/// harness's stdin can consume the caller's own input — measured: a `while read`
/// loop driving a probe lost two thirds of its work list to a child that ate the
/// pipe.
pub fn run_with_deadline_opts(
    cmd: &mut Command,
    timeout: Duration,
    cap: usize,
    stdin_data: Option<&[u8]>,
) -> Result<Output, RunFailure> {
    // Put the child in its OWN process group (leader pid == group id) so a
    // deadline/overflow kill can reap the whole tree via the negative pgid.
    // Without this, a grandchild orphans and spins forever.
    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        cmd.process_group(0);
    }
    let mut child = cmd
        .stdin(if stdin_data.is_some() { Stdio::piped() } else { Stdio::null() })
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| panic!("failed to spawn {cmd:?}: {e}"));

    if let Some(data) = stdin_data {
        use std::io::Write;
        if let Some(mut sink) = child.stdin.take() {
            // A child that exits without reading closes the pipe: EPIPE here is
            // the child's business, not a harness failure.
            let _ = sink.write_all(data);
        }
    }

    let stdout_handle = child.stdout.take().expect("stdout was piped");
    let stderr_handle = child.stderr.take().expect("stderr was piped");
    let overflow = Arc::new(AtomicBool::new(false));
    let stdout_thread = capped_drain(stdout_handle, overflow.clone(), cap);
    let stderr_thread = capped_drain(stderr_handle, overflow.clone(), cap);

    let deadline = std::time::Instant::now() + timeout;
    let status = loop {
        match child.try_wait() {
            Ok(Some(status)) => break status,
            Ok(None) => {
                if overflow.load(Ordering::Relaxed) {
                    kill_process_tree(&mut child);
                    return Err(RunFailure::Overflow { cap });
                }
                if std::time::Instant::now() >= deadline {
                    kill_process_tree(&mut child);
                    return Err(RunFailure::Deadline { secs: timeout.as_secs() });
                }
                std::thread::sleep(Duration::from_millis(20));
            }
            // ⚠ The ONE loop exit that used to leave the child running. `wait`
            // failing does not mean the child is gone, so bailing out here
            // without a tree kill orphans it — the exact defect this module
            // exists to close, in the module itself.
            Err(e) => {
                kill_process_tree(&mut child);
                panic!("failed to wait on child: {e}");
            }
        }
    };

    let stdout = stdout_thread.join().unwrap_or_default();
    let stderr = stderr_thread.join().unwrap_or_default();

    // Exit-at-cap race guard. A child that emitted >cap and then exited FAST can
    // win the `try_wait()` race before the poll-loop overflow branch observes the
    // flag — leaving us about to return a buffer SILENTLY TRUNCATED at the cap as
    // if it were complete output. Both drain threads have now joined, so
    // `overflow` is authoritative. Contract: >cap ⇒ loud failure, never silent
    // truncation presented as a complete result.
    if overflow.load(Ordering::Relaxed) {
        return Err(RunFailure::Overflow { cap });
    }

    Ok(Output { status, stdout, stderr })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The deadline path returns `Deadline`, not a hang and not an `Ok`.
    #[cfg(unix)]
    #[test]
    fn deadline_kills_and_reports() {
        let mut cmd = Command::new("sleep");
        cmd.arg("60");
        let start = std::time::Instant::now();
        let r = run_with_deadline(&mut cmd, Duration::from_millis(300));
        assert_eq!(r.unwrap_err(), RunFailure::Deadline { secs: 0 });
        assert!(start.elapsed() < Duration::from_secs(10), "the kill was not prompt");
    }

    /// A LIVE infinite printer trips the cap and is killed PROMPTLY — the
    /// in-loop overflow branch, not the deadline. A tiny cap so `yes` crosses it
    /// in microseconds; the generous deadline is only a backstop, so a FAILURE
    /// to trip surfaces as a slow timeout rather than a hang.
    #[cfg(unix)]
    #[test]
    fn runaway_output_trips_the_cap_promptly() {
        let mut cmd = Command::new("yes");
        let start = std::time::Instant::now();
        let r = run_with_deadline_opts(&mut cmd, Duration::from_secs(20), 8 * 1024, None);
        assert_eq!(r.unwrap_err(), RunFailure::Overflow { cap: 8 * 1024 });
        assert!(
            start.elapsed() < Duration::from_secs(10),
            "runaway kill was not prompt — the in-loop overflow branch did not fire",
        );
    }

    /// The exit-at-cap RACE: a child that emits >cap and then exits immediately
    /// must NOT come back as `Ok` with silently-truncated output. 16 KiB fits
    /// the pipe buffer, so the child never blocks and the drain reads it after
    /// exit — the post-loop re-check is the only thing that catches this.
    #[cfg(unix)]
    #[test]
    fn exit_at_cap_is_loud_not_silently_truncated() {
        let mut cmd = Command::new("head");
        cmd.arg("-c").arg("16384").arg("/dev/zero");
        let r = run_with_deadline_opts(&mut cmd, Duration::from_secs(20), 8 * 1024, None);
        assert_eq!(r.unwrap_err(), RunFailure::Overflow { cap: 8 * 1024 });
    }

    /// The DEGRADATION contract: a child that leads no group is still killed,
    /// exactly once, and the call returns promptly.
    ///
    /// ⚠ WHAT THIS TEST DOES **NOT** PIN, STATED RATHER THAN IMPLIED (Core #12).
    /// It cannot demonstrate the `getpgid(pid) == pid` guard, and the first
    /// version of it PRETENDED TO. That version asserted "the group leader
    /// survives when a non-leader member is killed" — and it passed with the
    /// guard REMOVED, because `kill(-member_pid)` addresses the group led by
    /// `member_pid`, which does not exist: the leader's group is `leader_pid`.
    /// So the unguarded call returns `ESRCH` and hits nothing. An accidentally
    /// green control (Six Questions Q6), caught by running the reversion.
    ///
    /// The guard's stray-signal half is only reachable under PID REUSE — some
    /// other process coming to lead a group whose pgid equals our child's pid —
    /// and our child is un-reaped at the moment of the call, so its pid is
    /// reserved and that cannot happen. The guard therefore buys CLARITY (the
    /// degradation is explicit instead of relying on `ESRCH`) rather than a
    /// behaviour change, and this test pins only the half that is observable.
    /// The half that MATTERS at the `gg test --parallel` call site — that a
    /// worker's grandchildren still orphan because the worker leads no group —
    /// is a live gap, filed as `todo/t0844`, not something this guard closes.
    #[cfg(unix)]
    #[test]
    fn non_leader_child_is_still_killed_directly() {
        use std::os::unix::process::CommandExt;
        let mut leader = Command::new("sleep")
            .arg("300")
            .process_group(0)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("spawn leader");
        let mut member = Command::new("sleep")
            .arg("300")
            .process_group(leader.id() as i32)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("spawn member");
        assert_ne!(
            member.id(),
            leader.id(),
            "the member must not be the group leader, or this test proves nothing",
        );

        let start = std::time::Instant::now();
        kill_process_tree(&mut member);
        assert!(
            start.elapsed() < Duration::from_secs(5),
            "killing a non-leader child blocked — the degradation path must not hang",
        );
        // Liveness via /proc, NOT `try_wait()`. `process_spawn_deadline_arm_count`
        // counts every `try_wait()` in the tree as a spawn-and-poll loop, and it
        // is RIGHT to — an exemption for "it's only a test" is how the sixth
        // hand-rolled runner gets in. The lint caught this line; the fix is the
        // test, never the lint.
        assert!(
            !std::path::Path::new(&format!("/proc/{}", member.id())).exists()
                || std::fs::read_to_string(format!("/proc/{}/stat", member.id()))
                    .map(|r| r[r.rfind(')').unwrap_or(0) + 2..].split(' ').next() == Some("Z"))
                    .unwrap_or(true),
            "the non-leader member was not killed; the direct-child fallback is \
             the WHOLE kill when the child leads no group, so it must always fire",
        );
        leader.kill().ok();
        leader.wait().ok();
    }

    /// ⭐ THE REASON THIS MODULE EXISTS: a GRANDCHILD must die with the child.
    ///
    /// The shell forks a background `sleep` (which writes its own pid down) and
    /// then execs a foreground `sleep`, so the background one is a real
    /// GRANDCHILD of this process — the exact shape `gg run` produces when a
    /// fixture forks a worker. A plain `child.kill()` reaps only the direct
    /// child and leaves the grandchild spinning; that is the defect that burned
    /// a core for forty hours here.
    ///
    /// ⚠ THIS ASSERTION HAD TO BE REWRITTEN ONCE. The first version checked only
    /// that the call RETURNED PROMPTLY, and deleting `process_group(0)` left it
    /// GREEN — because the deadline path bails out before joining the drain
    /// threads, so a surviving grandchild costs no wall time. A test that cannot
    /// fail is worse than none (Core #12), so it now looks at the grandchild
    /// itself: its pid must be GONE.
    #[cfg(unix)]
    #[test]
    fn grandchild_dies_with_the_process_group() {
        // ⚠ Plain characters only. An earlier spelling interpolated a Rust
        // ThreadId (`ThreadId(2)`) into the path, and the parentheses became a
        // SHELL SYNTAX ERROR inside the script below — the pidfile was never
        // written and the test failed for a reason unrelated to its subject.
        let pidfile = std::env::temp_dir().join(format!(
            "gg_pgkill_{}_{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_nanos())
                .unwrap_or(0),
        ));
        let _ = std::fs::remove_file(&pidfile);
        // `echo $$ > f; exec sleep` — the background subshell records its own pid
        // and then BECOMES the sleep, so the recorded pid is the live grandchild.
        let script = format!(
            "sh -c 'echo $$ > {f}; exec sleep 300' & exec sleep 300",
            f = pidfile.display()
        );
        let mut cmd = Command::new("sh");
        cmd.arg("-c").arg(&script);
        let r = run_with_deadline(&mut cmd, Duration::from_millis(700));
        assert_eq!(r.unwrap_err(), RunFailure::Deadline { secs: 0 });

        // It really existed, so the assertion below is about a kill and not
        // about a shell that never forked (Core #15e Q6 — an accidentally-green
        // control).
        let raw = std::fs::read_to_string(&pidfile)
            .expect("the grandchild never recorded its pid; this test proves nothing");
        let gpid: i32 = raw.trim().parse().expect("pidfile did not hold a pid");
        let _ = std::fs::remove_file(&pidfile);

        // Poll: reparenting and reaping are not instantaneous. `/proc/<pid>`
        // vanishing is the liveness answer on this platform; where /proc is
        // absent the check degrades to "we could not observe it", which is
        // stated rather than silently passing.
        assert!(
            std::path::Path::new("/proc/self").exists(),
            "no /proc — this assertion cannot be made on this platform",
        );
        let deadline = std::time::Instant::now() + Duration::from_secs(5);
        while std::path::Path::new(&format!("/proc/{gpid}")).exists()
            && std::time::Instant::now() < deadline
        {
            std::thread::sleep(Duration::from_millis(50));
        }
        assert!(
            !std::path::Path::new(&format!("/proc/{gpid}")).exists(),
            "grandchild pid {gpid} SURVIVED the deadline kill — the process-group \
             kill did not reach it, which is the orphan class this module exists to close",
        );
    }
}
