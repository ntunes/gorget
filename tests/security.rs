//! Security test harness for Gorget.
//!
//! Each fixture under `tests/fixtures/security/` is one adversarial program
//! designed to probe a specific memory-safety or correctness guarantee.
//! Fixtures are classified by helper:
//!
//! - [`security_safe`]       — a well-typed program that must build under
//!                             `--sanitize` and run exit-0 with expected stdout.
//! - [`security_traps`]      — a program that intentionally performs a
//!                             runtime-defined trap (e.g. division by zero)
//!                             and must panic with the Gorget-level message,
//!                             not raw C UB.
//! - [`security_rejected`]   — a program the compiler must reject at `gg check`,
//!                             with expected stderr pattern.
//! - [`security_known_unsafe`] — a program that currently reveals a real bug.
//!                             The test asserts the bug is *still* present
//!                             (so regressions can't make it worse silently
//!                             and so fixes force a reclassification).
//!
//! When a known-unsafe bug is fixed, the test will start failing — that's
//! the signal to reclassify as `security_safe` / `security_rejected` /
//! `security_traps` and relabel the fixture.
//!
//! To run just the security suite:
//!     cargo test --test security
//!
//! The full build includes `-fsanitize=address,undefined` via the
//! compiler's own `--sanitize` flag.

use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::time::Duration;

/// Sanitize builds are slower than the integration suite's, so the default
/// is higher (180 vs 120). Override with GG_BUILD_TIMEOUT_SECS for full
/// manual control. When unset, auto-scales by /proc/loadavg /
/// available_parallelism so the gate doesn't spuriously trip on shared /
/// loaded hosts.
fn build_timeout() -> Duration {
    Duration::from_secs(env_or_load_adjusted_secs("GG_BUILD_TIMEOUT_SECS", 180))
}

/// Same shape as integration.rs's helper. Linux only — falls back to
/// `base` when `/proc/loadavg` isn't readable.
fn env_or_load_adjusted_secs(var: &str, base: u64) -> u64 {
    if let Some(secs) = std::env::var(var).ok().and_then(|s| s.parse::<u64>().ok()) {
        return secs;
    }
    let load = std::fs::read_to_string("/proc/loadavg")
        .ok()
        .and_then(|s| s.split_whitespace().next()?.parse::<f64>().ok())
        .unwrap_or(0.0);
    let cpus = std::thread::available_parallelism()
        .map(|n| n.get() as f64)
        .unwrap_or(1.0);
    let ratio = (load / cpus).max(1.0);
    ((base as f64) * ratio).ceil() as u64
}

fn test_binary_timeout() -> Duration {
    Duration::from_secs(
        std::env::var("GG_TEST_TIMEOUT_SECS")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(30),
    )
}

fn gg_command(subcommand: &str) -> Command {
    let mut cmd = Command::new(env!("CARGO"));
    cmd.args(["run", "--quiet", "--", subcommand]);
    cmd
}

fn run_with_deadline(cmd: &mut Command, fixture: &str, timeout: Duration) -> std::process::Output {
    let mut child = cmd
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to execute compiled binary");

    let stdout_handle = child.stdout.take().unwrap();
    let stderr_handle = child.stderr.take().unwrap();

    let stdout_thread = std::thread::spawn(move || {
        use std::io::Read;
        let mut buf = Vec::new();
        let mut reader = stdout_handle;
        reader.read_to_end(&mut buf).ok();
        buf
    });
    let stderr_thread = std::thread::spawn(move || {
        use std::io::Read;
        let mut buf = Vec::new();
        let mut reader = stderr_handle;
        reader.read_to_end(&mut buf).ok();
        buf
    });

    let deadline = std::time::Instant::now() + timeout;
    let status = loop {
        match child.try_wait() {
            Ok(Some(status)) => break status,
            Ok(None) => {
                if std::time::Instant::now() >= deadline {
                    child.kill().ok();
                    child.wait().ok();
                    panic!("Process for {fixture} timed out after {}s", timeout.as_secs());
                }
                std::thread::sleep(Duration::from_millis(50));
            }
            Err(e) => panic!("Failed to wait on child for {fixture}: {e}"),
        }
    };

    let stdout = stdout_thread.join().unwrap_or_default();
    let stderr = stderr_thread.join().unwrap_or_default();

    std::process::Output { status, stdout, stderr }
}

fn fixture_path(name: &str) -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = manifest_dir
        .join("tests/fixtures/security")
        .join(format!("{name}.gg"));
    assert!(path.exists(), "Fixture not found: {}", path.display());
    path
}

fn cleanup(fixture: &PathBuf) {
    let stem = fixture.file_stem().unwrap().to_str().unwrap();
    let dir = fixture.parent().unwrap();
    let _ = std::fs::remove_file(dir.join(format!("{stem}.c")));
    let _ = std::fs::remove_file(dir.join(stem));
}

/// Result of a sanitize build + run. Records exactly what happened so the
/// known-unsafe helper can assert against it.
struct SanitizeOutcome {
    build_ok: bool,
    build_stderr: String,
    ran: bool,
    exit_code: Option<i32>,
    stdout: String,
    stderr: String,
}

/// Default run-time `ASAN_OPTIONS`. Leak detection is OFF here — most
/// security fixtures probe UAF/overflow/traps, not leaks, and LSan noise from
/// intentionally-abandoned allocations in trap-fixtures would be spurious.
const ASAN_OPTS_NO_LEAK: &str =
    "detect_leaks=0:halt_on_error=1:abort_on_error=0:print_summary=1:allocator_may_return_null=1";

/// Leak-detecting run-time `ASAN_OPTIONS`. `detect_leaks=1` makes
/// LeakSanitizer a hard failure and `exitcode=99` turns any ASan/LSan report
/// into a nonzero exit — so a leak is caught by BOTH the report scrape and the
/// exit code. Used by [`security_safe_no_leak`] to guard leak-class bugs that a
/// `detect_leaks=0` run (or a plain stdout check) cannot see.
const ASAN_OPTS_LEAK_CHECK: &str =
    "detect_leaks=1:halt_on_error=1:abort_on_error=0:print_summary=1:allocator_may_return_null=1:exitcode=99";

fn sanitize_build_and_run(fixture_name: &str) -> (SanitizeOutcome, PathBuf) {
    sanitize_build_and_run_with_opts(fixture_name, ASAN_OPTS_NO_LEAK)
}

/// Build the fixture with the Rust `gg` under `--sanitize`, then run it with
/// the caller-supplied `ASAN_OPTIONS`. Factored so the leak-checking guard
/// ([`security_safe_no_leak`]) shares the exact same build path but flips on
/// `detect_leaks=1` — the build MUST be the Rust compiler (the class of bug the
/// leak guard covers is a Rust-side lowering bug, not a self-host one).
fn sanitize_build_and_run_with_opts(
    fixture_name: &str,
    asan_options: &str,
) -> (SanitizeOutcome, PathBuf) {
    let fp = fixture_path(fixture_name);
    let stem = fp.file_stem().unwrap().to_str().unwrap();
    let dir = fp.parent().unwrap();
    let exe_path = dir.join(stem);

    let build = run_with_deadline(
        gg_command("build").arg("--sanitize").arg(&fp),
        fixture_name,
        build_timeout(),
    );
    let build_ok = build.status.success();
    let build_stderr = String::from_utf8_lossy(&build.stderr).into_owned();

    if !build_ok || !exe_path.exists() {
        return (
            SanitizeOutcome {
                build_ok,
                build_stderr,
                ran: false,
                exit_code: None,
                stdout: String::new(),
                stderr: String::new(),
            },
            fp,
        );
    }

    let mut run_cmd = Command::new(&exe_path);
    // Halt on first sanitizer error so a trip produces nonzero exit.
    // `allocator_may_return_null=1` lets the runtime's own null-check-after-
    // alloc paths run — otherwise ASan pre-aborts on oversized allocations
    // and masks Gorget's cleaner runtime trap.
    run_cmd.env("ASAN_OPTIONS", asan_options);
    run_cmd.env("UBSAN_OPTIONS", "halt_on_error=1:print_stacktrace=1");

    let run = run_with_deadline(&mut run_cmd, fixture_name, test_binary_timeout());
    (
        SanitizeOutcome {
            build_ok,
            build_stderr,
            ran: true,
            exit_code: run.status.code(),
            stdout: String::from_utf8_lossy(&run.stdout).into_owned(),
            stderr: String::from_utf8_lossy(&run.stderr).into_owned(),
        },
        fp,
    )
}

/// A well-typed program that must build under `--sanitize` and run exit-0
/// with the expected stdout. Any deviation (build failure, sanitizer trip,
/// wrong output, nonzero exit) fails the test.
fn security_safe(name: &str, expected_stdout: &str) {
    let (out, fp) = sanitize_build_and_run(name);
    assert!(
        out.build_ok,
        "security_safe({name}): sanitize build failed\nstderr: {}",
        out.build_stderr
    );
    assert!(out.ran, "security_safe({name}): binary did not run");
    assert_eq!(
        out.stdout.trim(),
        expected_stdout.trim(),
        "security_safe({name}): stdout mismatch\nExpected:\n{expected_stdout}\nGot:\n{}\nstderr:\n{}",
        out.stdout,
        out.stderr
    );
    assert_eq!(
        out.exit_code,
        Some(0),
        "security_safe({name}): nonzero exit {:?}\nstderr:\n{}",
        out.exit_code,
        out.stderr
    );
    cleanup(&fp);
}

/// Like [`security_safe`], but runs under `detect_leaks=1` so a memory LEAK is
/// a hard failure — not just a UAF/overflow/trap. Use for fixtures whose bug
/// class is stdout-INVISIBLE (a leaked heap buffer produces the correct output
/// yet leaks): a plain stdout check or a `detect_leaks=0` run passes it, so
/// only a LeakSanitizer-armed run guards it.
///
/// Each fixture MUST have been verified to FAIL (exit 99 / LSan report) at the
/// pre-fix baseline and PASS post-fix — else it guards nothing (Core #6).
fn security_safe_no_leak(name: &str, expected_stdout: &str) {
    let (out, fp) = sanitize_build_and_run_with_opts(name, ASAN_OPTS_LEAK_CHECK);
    assert!(
        out.build_ok,
        "security_safe_no_leak({name}): sanitize build failed\nstderr: {}",
        out.build_stderr
    );
    assert!(out.ran, "security_safe_no_leak({name}): binary did not run");
    // A leak trips LeakSanitizer → `exitcode=99` + a report on stderr. Guard on
    // BOTH so neither a stray exit code nor a scrape miss lets a leak through.
    let has_leak_report = out.stderr.contains("LeakSanitizer")
        || out.stderr.contains("AddressSanitizer")
        || out.stderr.contains("detected memory leaks")
        || out.stderr.contains("ERROR:")
        || out.stderr.contains("SUMMARY:");
    assert!(
        !has_leak_report,
        "security_safe_no_leak({name}): sanitizer report (leak/UAF) under detect_leaks=1:\n{}",
        out.stderr
    );
    assert_eq!(
        out.exit_code,
        Some(0),
        "security_safe_no_leak({name}): nonzero exit {:?} under detect_leaks=1 \
         (a leak trips exitcode=99). If this regressed, the CoW bare-assign \
         owned-String path is re-leaking.\nstderr:\n{}",
        out.exit_code,
        out.stderr
    );
    assert_eq!(
        out.stdout.trim(),
        expected_stdout.trim(),
        "security_safe_no_leak({name}): stdout mismatch\nExpected:\n{expected_stdout}\nGot:\n{}\nstderr:\n{}",
        out.stdout,
        out.stderr
    );
    cleanup(&fp);
}

/// A program that intentionally performs a runtime-defined trap (e.g.
/// division by zero, integer overflow). Must build and run, then exit
/// nonzero with the Gorget-level trap message in stderr — NOT raw C UB.
fn security_traps(name: &str, stderr_pattern: &str) {
    let (out, fp) = sanitize_build_and_run(name);
    assert!(
        out.build_ok,
        "security_traps({name}): sanitize build failed\nstderr: {}",
        out.build_stderr
    );
    assert!(out.ran, "security_traps({name}): binary did not run");
    assert_ne!(
        out.exit_code,
        Some(0),
        "security_traps({name}): expected nonzero exit (trap), got 0\nstdout:\n{}",
        out.stdout
    );
    let combined = format!("{}\n{}", out.stdout, out.stderr);
    assert!(
        combined.contains(stderr_pattern),
        "security_traps({name}): expected stderr to contain `{stderr_pattern}`, got:\nstdout:{}\nstderr:{}",
        out.stdout,
        out.stderr
    );
    cleanup(&fp);
}

/// A program the compiler must reject at `gg check` with the expected
/// stderr pattern.
fn security_rejected(name: &str, stderr_pattern: &str) {
    let fp = fixture_path(name);
    let output = run_with_deadline(gg_command("check").arg(&fp), name, build_timeout());
    assert!(
        !output.status.success(),
        "security_rejected({name}): expected `gg check` to fail, but it succeeded.\nstdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(stderr_pattern),
        "security_rejected({name}): expected stderr to contain `{stderr_pattern}`, got:\n{stderr}"
    );
}

/// Kinds of currently-present bugs we track with `security_known_unsafe`.
/// When a bug is fixed, the assertion for its kind will stop holding, the
/// test will fail, and someone must reclassify the fixture.
#[allow(dead_code)]
enum KnownBug {
    /// Compiler panics or emits C that fails to build.
    BuildFails,
    /// Binary builds under --sanitize, but the sanitizer trips at runtime.
    SanitizerTrips,
    /// Program silently produces this (wrong) output, with no trap. The most
    /// insidious class — use this for cases where the compiler "accepts" a
    /// bad program and runs it to completion.
    SilentlyProduces(&'static str),
}

/// A program that currently reveals a real bug. Asserts the bug is still
/// present. When the bug is fixed, this test will start failing — the
/// forcing function to move the fixture into `security_safe`/`_rejected`/
/// `_traps`.
fn security_known_unsafe(name: &str, bug: KnownBug, reason: &str) {
    let (out, _fp) = sanitize_build_and_run(name);
    match bug {
        KnownBug::BuildFails => {
            assert!(
                !out.build_ok,
                "security_known_unsafe({name}): expected BuildFails because `{reason}`, \
                 but build succeeded. If you fixed it, reclassify this fixture."
            );
        }
        KnownBug::SanitizerTrips => {
            assert!(
                out.build_ok,
                "security_known_unsafe({name}): expected SanitizerTrips because `{reason}`, \
                 but build failed. Did the codegen regress?\nstderr: {}",
                out.build_stderr
            );
            assert!(out.ran, "security_known_unsafe({name}): binary didn't run");
            assert_ne!(
                out.exit_code,
                Some(0),
                "security_known_unsafe({name}): expected sanitizer trip (nonzero exit) \
                 because `{reason}`, got exit 0. If you fixed it, reclassify this fixture.\n\
                 stdout: {}\nstderr: {}",
                out.stdout,
                out.stderr
            );
            let sanitizer_msg = out.stderr.contains("Sanitizer")
                || out.stderr.contains("runtime error")
                || out.stderr.contains("SEGV");
            assert!(
                sanitizer_msg,
                "security_known_unsafe({name}): expected sanitizer output in stderr \
                 because `{reason}`, got:\n{}",
                out.stderr
            );
        }
        KnownBug::SilentlyProduces(expected_stdout) => {
            assert!(
                out.build_ok,
                "security_known_unsafe({name}): expected SilentlyProduces because `{reason}`, \
                 but build failed.\nstderr: {}",
                out.build_stderr
            );
            assert!(out.ran, "security_known_unsafe({name}): binary didn't run");
            assert_eq!(
                out.exit_code,
                Some(0),
                "security_known_unsafe({name}): expected silent exit 0 because `{reason}`, \
                 got exit {:?}. Did it finally trap?\nstderr: {}",
                out.exit_code,
                out.stderr
            );
            assert_eq!(
                out.stdout.trim(),
                expected_stdout.trim(),
                "security_known_unsafe({name}): stdout changed. If you fixed the bug, \
                 reclassify this fixture. Reason: `{reason}`"
            );
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════
// Classification of the 29 adversarial fixtures (audit 2026-04-23)
// ════════════════════════════════════════════════════════════════════════════

// ── Compiler correctly rejects (the borrow checker's happy path) ──

#[test]
fn sec_01_multi_move_string() {
    security_rejected("attack_01_multi_move_string", "move");
}

#[test]
fn sec_07_return_borrow_of_local() {
    security_rejected("attack_07_return_borrow_of_local", "");
}

#[test]
fn sec_09_match_binding_escape() {
    security_rejected("attack_09_match_binding_escape", "");
}

#[test]
fn sec_16_closure_captures_string() {
    security_rejected("attack_16_closure_captures_string", "");
}

#[test]
fn sec_19_field_borrow_escape() {
    security_rejected("attack_19_field_borrow_escape", "");
}

#[test]
fn sec_93_arena_borrow_escape_push() {
    // N2 sibling of sec_19: the SAME arena borrow-escape UAF at a
    // collection-MUTATION consume position (`outer.push(arenaVec.get(0).unwrap())`).
    // RUN-confirmed UAF under ASan before the check existed.
    security_rejected("attack_93_arena_borrow_escape_push", "cannot assign arena-scoped value");
}

#[test]
fn sec_94_arena_borrow_escape_channel_send() {
    // N2 subset (a): Channel `send` ingests an arena-borrowed non-Copy element
    // into the channel's buffer; the channel OUTLIVES the arena, so the element
    // dangles at `gorget_arena_destroy`. RUN-confirmed UAF under ASan before the
    // typed `owns_buffered_elements` gate. Channel `collection_kind: None` —
    // gated on the typed flag, not a name-match of `send`.
    security_rejected("attack_94_arena_borrow_escape_channel_send", "cannot assign arena-scoped value");
}

#[test]
fn sec_95_arena_borrow_escape_heap_push() {
    // N2 subset (a) sibling: Heap `push` is the SAME element-owning-sink class as
    // Channel `send` (also `owns_buffered_elements: true`, also
    // `collection_kind: None`). RUN-confirmed UAF under ASan. Proves the gate is
    // the CLASS, not a Channel-only instance ("fix the class, not the instance").
    security_rejected("attack_95_arena_borrow_escape_heap_push", "cannot assign arena-scoped value");
}

#[test]
fn sec_96_arena_borrow_escape_ctor_arg() {
    // N2 subset (b): constructor/wrapper arg (`outer = Some(arenaVec.get(0).unwrap())`)
    // assigned to an outer binding. The ctor copies the arena-borrowed element
    // into the built value under the in-scope arena allocator, so it dangles at
    // arena destruction. RUN-confirmed UAF under ASan. Also covers positional
    // struct constructors (desugared to `Expr::StructLiteral`).
    security_rejected("attack_96_arena_borrow_escape_ctor_arg", "cannot assign arena-scoped value");
}

#[test]
fn sec_98_arena_borrow_escape_ctor_bareid() {
    // N2 subset (b) sibling: the ctor arg is a BARE arena-scoped non-Copy
    // identifier (`Some(arenaStr)` / `!arenaStr`), not a `.get()` borrow-read —
    // the same UAF class, now caught by the one producer `arena_backed_source`
    // (the arena-scoped-identifier arm). RUN-confirmed UAF under ASan.
    security_rejected("attack_98_arena_borrow_escape_ctor_bareid", "cannot assign arena-scoped value");
}

// ── Accepted well-typed programs that must run safely under --sanitize ──

#[test]
fn sec_97_arena_channel_send_inner_ok() {
    // Negative control for sec_94: the arena-escape gate must NOT over-fire when
    // the buffer-owning handle does NOT outlive the arena (Channel declared
    // INSIDE the `with`). Must build, run clean under --sanitize, and print the
    // payload. Guards against a false positive on a Copy-typed handle that the
    // arena-scoped-binding tracker must still recognize via is_buffer_owning_type.
    security_safe("attack_97_arena_channel_send_inner_ok", "payload");
}

#[test]
fn sec_03_cow_mutate_while_borrowed() {
    security_safe("attack_03_cow_mutate_while_borrowed", "alpha\ndelta");
}

/// D10(a) (decisions.md, ratified 2026-07-06): the fixture's `int &borrowed =
/// ...` decl-sigil bind is now a parse-time rejection — which matches the
/// fixture's ORIGINAL intent ("must reject"). Pre-D10 the parser silently
/// discarded the sigil, `borrowed` was a value copy, and the fixture was
/// (mis)classified safe with output "10".
#[test]
fn sec_04_cow_mutate_ref_borrow() {
    security_rejected(
        "attack_04_cow_mutate_ref_borrow",
        "local `&`-bindings are not supported",
    );
}

#[test]
fn sec_05_nested_vector_drop() {
    security_safe("attack_05_nested_vector_drop", "3\n99");
}

#[test]
fn sec_06_inline_none() {
    security_safe("attack_06_inline_none", "42\n-2\nfalse\n42");
}

#[test]
fn sec_08_closure_captures_local_ref() {
    security_safe("attack_08_closure_captures_local_ref", "123");
}

#[test]
fn sec_10_spawn_stale_shared() {
    security_safe("attack_10_spawn_stale_shared", "20000");
}

#[test]
fn sec_11_uninit_return() {
    security_safe("attack_11_uninit_return", "10\n7\n0\n1\n-1\n0");
}

#[test]
fn sec_13_vector_grow_pointer_invalidation() {
    security_safe("attack_13_vector_grow_pointer_invalidation", "alpha\nalpha");
}

#[test]
fn sec_14_dict_borrow_mutate() {
    security_safe("attack_14_dict_borrow_mutate", "v1");
}

#[test]
fn sec_15_channel_close_race() {
    security_safe("attack_15_channel_close_race", "0");
}

#[test]
fn sec_18_self_referential_mutation() {
    security_safe("attack_18_self_referential_mutation", "third\nthird");
}

#[test]
fn sec_21_vector_negative_index() {
    security_safe("attack_21_vector_negative_index", "-1\n-2\n30");
}

#[test]
fn sec_24_move_in_loop_iteration() {
    security_safe("attack_24_move_in_loop_iteration", "one\ntwo\nthree");
}

#[test]
fn sec_29_unwrap_pattern() {
    security_safe("attack_29_unwrap_pattern", "alpha");
}

#[test]
fn sec_92_static_collection_reassign_uaf() {
    // Conformance Bug 2. Storing an owned local collection into a `static`
    // (`CACHE = d`) is a consuming position; before the fix it shallow-aliased
    // the local's heap buffer, which the local's scope-exit drop freed → a
    // heap-use-after-free on the next read of the static (silent in release:
    // garbage value, exit 0). Fixed in src/ir/lowering/stmts/assigns.rs — the
    // store-to-static path now clones-or-moves the RHS, MoveZeros the moved
    // source, and drops the static's prior value. Covers Dict/Vector/Set.
    security_safe(
        "attack_92_static_collection_reassign_uaf",
        "42\n42\n10\n20\ntrue\ntrue\nfalse",
    );
}

// ── Runtime traps that must use the Gorget panic path, not C UB ──

#[test]
fn sec_25_div_zero_direct() {
    security_traps("attack_25_div_zero_direct", "division by zero");
}

#[test]
fn sec_27_signed_overflow() {
    security_traps("attack_27_signed_overflow", "overflow");
}

// ── Known-present bugs. Fixing any of these makes its test fail. ──

#[test]
fn sec_02_box_new_consuming_position() {
    // Was BuildFails. Fixed 2026-04-23 in src/ir/lowering/exprs/methods.rs —
    // Box.new now follows the push/set consuming-position rule (clone
    // multi-use, MoveZero only on last-use).
    security_safe("attack_02_box_new_double_move", "5");
}

#[test]
fn sec_17_vector_borrow_invalidate() {
    // Was SanitizerTrips. Fixed 2026-04-23 in src/ir/lowering/stmts/mod.rs —
    // VarDecl now lifts Option[Ref[T]] → Option[T] via tag-branch + clone
    // instead of emitting a wrong-sized memcpy. `v.push(...)` materializes
    // the snapshot via CoW, so `s` keeps v[0]'s value as it was at get-time.
    security_safe("attack_17_vector_borrow_invalidate", "aaa");
}

#[test]
fn sec_20_shift_out_of_range() {
    security_traps(
        "attack_20_div_by_zero",
        "shift out of range",
    );
}

#[test]
fn sec_22_weak_after_shared_dropped() {
    // Fixed 2026-04-23. Two things: (1) the original `Shared.new(v)` surface
    // syntax isn't supported in Gorget — rewrote the fixture to the idiomatic
    // `Shared[T](v)` constructor. That alone turned the link error into a
    // heap-use-after-free. (2) Root fix in src/ir/lowering/exprs/calls.rs —
    // `!x` on a refcounted type (Shared/Weak/Channel) now MoveZeros the
    // caller's slot after the call, matching the push/enum-init consuming-
    // position rule. Without (2), `drop_all(!s)` left `s` live in the caller
    // and its scope-exit drop double-freed the control block.
    security_safe("attack_22_weak_after_shared_dropped", "-1");
}

#[test]
fn sec_23_dead_div_zero_now_traps() {
    // Was SilentlyProduces("10"). Fixed 2026-04-23 in src/lir/optimize.rs —
    // integer Div/Rem/Mod and all shifts are now treated as side-effecting
    // by the LIR DCE pass, so the trap fires even when the result is dead.
    security_traps("attack_23_panic_during_init", "division by zero");
}

#[test]
fn sec_26_mod_zero_traps() {
    security_traps("attack_26_mod_zero", "division by zero");
}

#[test]
fn sec_12_vector_iter_resource() {
    // Was BuildFails. Fixed 2026-04-23 in src/lir/lower/operands.rs —
    // `resolve_field_type` and `resolve_place_type` now consult the LIR
    // struct registry for enums (flat-layout Option/Result), so Field(1)
    // on Option__Ref_T resolves as LIR::Ptr instead of falling back to I64.
    // Eliminates the Ptr-ABI debug_assert trip and the cosmetic `*(int64_t*)`
    // cast in generated C.
    security_safe("attack_12_vector_iter_resource", "a\nb\nc");
}

// ── Regression fixtures for closed findings — keep these passing. ──

#[test]
fn sec_30_option_ref_match_arm() {
    // Parallel to sec_28 (VarDecl) — this one exercises `case Some(s):` arm
    // destructuring of Option[Ref[T]]. Pre-fix: memcpy of 40 bytes from a
    // 16-byte source via &Some_0 treated as Str*. Fixed by Option[Ref[T]]
    // pattern extraction now Loading the Ref value instead of taking its
    // address (src/lir/lower/insts.rs EnumFieldLoad handler).
    security_safe("attack_30_option_ref_match_arm", "beta");
}

#[test]
fn sec_31_option_ref_if_is() {
    // Parallel to sec_30 — `if ... is Some(s):` sugar.
    security_safe("attack_31_option_ref_if_is", "alpha");
}

#[test]
fn sec_32_option_ref_resource_struct() {
    // Parallel to sec_30 — Option[Ref[S]] where S is a user struct with a
    // resource field. Exercises the Ref→struct extraction path.
    security_safe("attack_32_option_ref_resource_struct", "4");
}

#[test]
fn sec_33_integer_shift_all_counts() {
    // Regression for the shift guards: `1 << 63` must produce INT64_MIN
    // (defined via unsigned intermediate), `-8 >> 2` must produce -2
    // (arithmetic right shift), and `1 << 64` must trap with the Gorget
    // message. The program traps on the third line.
    security_traps("attack_33_integer_shift_all_counts", "shift out of range");
}

#[test]
fn sec_34_div_zero_in_expression() {
    // Regression for the DCE side-effect fix: `(x / 0) * 0 + x` has a dead
    // division (multiplied by zero then replaced by x), and DCE must NOT
    // eliminate it.
    security_traps("attack_34_div_zero_in_expression", "division by zero");
}

#[test]
fn sec_35_shared_move_chain_trivial_payload() {
    // Fixed 2026-04-23. Previously thought to be a refcount-metadata bug; the
    // real cause was that `Weak.upgrade()` registered `Option[Shared[T]]`
    // as a Named TypeId without its enum TypeDef, so match dispatch on
    // `w.upgrade()` fell through to const_bool(true) and always fired the
    // first arm. Fix at src/ir/lowering/exprs/methods.rs upgrade handler.
    security_safe("attack_35_shared_move_chain", "0");
}

#[test]
fn sec_36_match_weak_upgrade_rvalue() {
    // Regression — rvalue match on w.upgrade() must dispatch correctly.
    security_safe("attack_36_match_weak_upgrade_rvalue", "dead");
}

#[test]
fn sec_37_upgrade_still_alive() {
    // Complementary to sec_36 — Some arm must fire when Shared is alive.
    security_safe("attack_37_upgrade_still_alive", "alive");
}

// ── Round 3 attacks: broader soundness probes ────────────────────────────

#[test]
fn sec_38_fstring_no_format_injection() {
    // User-controlled %s etc. in interpolated values must be treated as
    // opaque data, not format directives. f-string lowers to
    // gorget_string_format(template, %.*s, user_data).
    security_safe(
        "attack_38_fstring_format_injection",
        "hello: %s %s %s %s %s %n",
    );
}

#[test]
fn sec_39_closure_captures_resource() {
    // Regression — closures that capture an owned resource-typed local at
    // last-use must MOVE the source into the env (MoveZero after struct init),
    // not leave a shallow alias that the source's scope-exit drop frees.
    // Fix in src/ir/lowering/closures.rs.
    security_safe("attack_39_closure_captures_resource", "3\n10");
}

#[test]
fn sec_40_integer_narrowing() {
    // `x as int8` truncates silently (300 → 44, 9999999999 → 1410065407).
    // Document the current behavior; a `checked_as` would be an upgrade.
    security_safe("attack_40_integer_narrowing", "44\n1410065407");
}

#[test]
fn sec_41_panic_during_init() {
    // Division-by-zero mid-execution traps cleanly; the partially-built
    // Vector is handled by process exit(1). No UAF under ASan.
    security_traps("attack_41_panic_during_drop", "division by zero");
}

#[test]
fn sec_42_vector_subscript_oob() {
    security_traps("attack_42_vector_subscript_oob", "index out of bounds");
}

#[test]
fn sec_43_string_slice_oob() {
    security_traps("attack_43_string_slice_negative", "byte_slice out of bounds");
}

#[test]
fn sec_45_deep_recursion_stack_overflow() {
    // Accepted as-is (2026-04-24, matches Rust / C++ / Zig): unbounded
    // recursion hits the OS guard page and SIGSEGVs. ASan surfaces this as
    // `stack-overflow`. A per-call depth counter would trap cleanly at
    // ~1-2 ns/call overhead; for now we prefer Gorget's "thin C ABI, no
    // hidden per-call cost" promise. Keeping the fixture as known_unsafe
    // documents the behavior so any future stack-guard work flips it.
    security_known_unsafe(
        "attack_45_deep_recursion",
        KnownBug::SanitizerTrips,
        "By design (matches Rust / C++ / Zig): unbounded recursion hits the \
         OS guard page → SIGSEGV. ASan surfaces `stack-overflow`. Closing \
         this means adding a per-call depth counter, which isn't free.",
    );
}

#[test]
fn sec_46_nan_equality_ieee754() {
    // Regression — simplify_cmp used to fold `x == x` as `true` even on
    // floats, which breaks IEEE-754 (NaN != NaN). Fix in
    // src/ir/transforms/optimize.rs:simplify_cmp — skip the reflexive
    // identity when type is F32/F64.
    security_safe("attack_46_float_nan", "false\ntrue\nfalse\nfalse");
}

#[test]
fn sec_47_dict_overwrite_drops_old() {
    // Dict.put on an existing key must drop the old value before storing
    // the new one. Exercises elem_drop on the overwritten cell.
    security_safe("attack_47_dict_overwrite", "1");
}

#[test]
fn sec_48_vector_remove_while_iterating() {
    // v.remove(i) inside a `while i < v.len()` loop — the classic
    // iterator-invalidation shape. Gorget's CoW system must either clone
    // the view or reflow through the mutated collection. No UAF expected.
    security_safe(
        "attack_48_vector_remove_while_iterating",
        "4\n1\n2\n4\n5",
    );
}

#[test]
fn sec_44_match_partial_option() {
    // Fixed 2026-04-24 at src/ir/lowering/exprs/mod.rs — inline `None()` in
    // a call argument now constructs Option[T]::None from expected_type
    // context instead of emitting a raw NULL constant.
    security_safe("attack_44_match_partial_option", "42\n-1");
}

#[test]
fn sec_28_cow_option_extraction_minimal() {
    // Was SanitizerTrips. Fixed 2026-04-23 together with sec_17 — the
    // minimal 4-line repro of Option[Ref[T]] → Option[T] now builds and
    // runs cleanly, printing the actual element.
    security_safe("attack_28_cow_option_extraction", "alpha");
}

// ── Round 4 attacks: arithmetic edges, collections, generics ─────────────

#[test]
fn sec_49_nested_closure() {
    // Closure capturing an enclosing function's scalar locals. Safe.
    security_safe("attack_49_nested_closure", "105");
}

#[test]
fn sec_50_int_min_div_neg_one() {
    // Regression — INT64_MIN / -1 is C UB (the result isn't representable).
    // Pre-fix: wrapping_div silently folded to INT_MIN, bypassing the runtime
    // guard. Post-fix: constant folder uses checked_div (returns None on
    // overflow so the runtime guard handles it), and the C backend's Div
    // instruction adds an explicit INT_MIN/-1 trap.
    security_traps("attack_50_int_min_div_neg_one", "integer overflow");
}

#[test]
fn sec_51_null_byte_string_equality() {
    // Strings are length-prefixed; embedded \0 doesn't terminate. Equality
    // returns correct result, len reports full byte count.
    security_safe("attack_51_null_byte_in_string", "false\n5\n5");
}

#[test]
fn sec_52_channel_send_after_close() {
    security_traps("attack_52_channel_send_after_close", "closed channel");
}

#[test]
fn sec_53_empty_collection_ops() {
    security_safe(
        "attack_53_empty_collection_ops",
        "empty-get\nempty-pop\nmissing-key",
    );
}

#[test]
fn sec_54_parse_int_overflow() {
    security_safe("attack_54_parse_int_overflow", "42\nerr-2\nerr-3");
}

#[test]
fn sec_55_dict_float_nan_key() {
    // Gorget's Dict uses bitwise equality for float keys — NaN stored with
    // `zero / zero` can be retrieved by recomputing `zero / zero` because
    // both have the same bit pattern. Semantic choice, not a memory bug.
    security_safe("attack_55_dict_float_nan_key", "real\nunreachable");
}

#[test]
fn sec_56_mutex_double_lock() {
    // Mutex.lock() on a Mutex that already has a live Guard in the
    // current scope is a deadlock waiting to happen — non-reentrant.
    // The borrow checker rejects it at compile time so it never reaches
    // the runtime.
    security_rejected("attack_56_mutex_double_lock", "already locked");
}

#[test]
fn sec_57_box_cycle_via_shared() {
    security_safe("attack_57_box_cycle_via_shared", "done");
}

#[test]
fn sec_58_string_concat_loop() {
    security_safe("attack_58_string_concat_loop", "100000");
}

#[test]
fn sec_59_generic_resource_monomorphization() {
    security_safe("attack_59_generic_resource_collision", "10\nhello");
}

// ── Round 5 attacks: sort, UTF-8, allocator edges, nested types ──────────

#[test]
fn sec_60_sort_non_transitive_comparator() {
    security_safe("attack_60_sort_non_transitive", "11");
}

#[test]
fn sec_61_dict_mutate_during_iter() {
    // `.keys()` materializes — iteration is over a snapshot. No UAF even
    // if the Dict is rehashed mid-loop.
    security_safe("attack_61_dict_mutate_during_iter", "3");
}

#[test]
fn sec_62_utf8_invalid_bytes() {
    security_safe("attack_62_utf8_invalid_bytes", "invalid");
}

#[test]
fn sec_63_huge_vector_capacity_traps() {
    // Regression — `v.reserve(huge)` used to silently fail (realloc → NULL),
    // leaving the vector with cap > 0 and data = NULL; the next push
    // segfaulted. Fix in src/backend/c/c_runtime.rs: gorget_array_reserve /
    // ensure_capacity / with_capacity now check __builtin_mul_overflow on
    // the size computation AND null-check the allocator's return, trapping
    // with `gorget: panic: allocation failed` or `array capacity overflow`.
    security_traps("attack_63_huge_vector_capacity", "allocation failed");
}

#[test]
fn sec_64_deep_option_unwrap() {
    security_safe("attack_64_deep_option_unwrap", "42\ninner-none");
}

#[test]
fn sec_65_tuple_resource_destructure() {
    security_safe("attack_65_tuple_resource_destructure", "hello\n3");
}

#[test]
fn sec_66_chained_field_mutation() {
    security_safe("attack_66_chained_field_mutation", "100");
}

#[test]
fn sec_67_tuple_return_auto_destructure() {
    security_safe("attack_67_pattern_guard_side_effects", "big\n1");
}

#[test]
fn sec_68_range_edge_cases() {
    security_safe("attack_68_range_edge_cases", "0\n0\n5");
}

#[test]
fn sec_69_negative_range() {
    security_safe("attack_69_negative_range", "-3");
}

#[test]
fn sec_70_inline_none_nested() {
    // Regression — cover several inline-None shapes so the fix doesn't
    // regress only at the top level. `level_1(None())`, `level_2(None())`,
    // `level_2(Some(None()))` all used to emit NULL-ptr-deref; now each
    // constructs the correct Option[T]::None struct from expected_type.
    security_safe("attack_70_inline_none_nested", "-1\n42\n-2\n-1\n7");
}

// ── Round 6 attacks: float casts, async panics, struct ABI, enum dispatch ─

#[test]
fn sec_71_float_to_int_saturation() {
    // Matches Rust `as i64` semantics (defined since 1.45): NaN → 0,
    // +Inf / huge → INT64_MAX. Not C UB on this platform; our cast
    // sequence is deterministic.
    security_safe(
        "attack_71_float_to_int_edge",
        "0\n9223372036854775807\n9223372036854775807\n3",
    );
}

#[test]
fn sec_72_spawn_task_panic_propagates() {
    // A panic inside a spawned task exits the whole process with a
    // clean Gorget-level message — the main task doesn't silently
    // survive while the worker dies.
    security_traps("attack_72_spawn_task_panic", "division by zero");
}

#[test]
fn sec_73_moved_self_reuse_rejected() {
    // Compiler correctly rejects `bag` use after `consume(!bag)`.
    security_rejected("attack_73_moved_self_reuse", "use of moved value");
}

#[test]
fn sec_74_hashmap_1k_keys() {
    // 1000 inserts + lookup — baseline for HashMap at moderate scale.
    // A pathological collision test would need attacker-crafted keys
    // that defeat the hash function; without knowing the hash, this
    // just confirms correctness at scale.
    security_safe("attack_74_hashmap_collision_dos", "1000\n500");
}

#[test]
fn sec_75_enum_variant_dispatch() {
    security_safe("attack_75_enum_256_variants", "0\n9\n5");
}

#[test]
fn sec_76_big_struct_return() {
    // Returning a 10-field struct by value — exercises the sret ABI.
    security_safe("attack_76_big_struct_return", "1\n10\n55");
}

#[test]
fn sec_77_dict_remove_during_iter() {
    // Collect keys first, then remove in a second pass. Safe pattern.
    security_safe("attack_77_dict_remove_during_iter", "3");
}

#[test]
fn sec_78_deep_box_drop() {
    // 1000-node Box chain. Each node's Drop recurses into next's Drop.
    // Passes — C stack handles 1000 fine. A 1M-node chain would SIGSEGV
    // via the same path as attack_45.
    security_safe("attack_78_deep_box_drop", "1000");
}

#[test]
fn sec_79_weak_clone_sanity() {
    security_safe("attack_79_weak_clone_sanity", "1\ndone");
}

#[test]
fn sec_80_char_construction_edges() {
    // `chr(0)` → 1-byte NUL String (len 1 codepoint). Gorget String is
    // length-prefixed and carries interior NULs (NOT NUL-terminated); the
    // empty-String behavior was deliberately removed in commit 11e3abb0
    // (gorget-js snag #6, codepoint_to_str(0)). `chr(1114111)` → U+10FFFF,
    // 1 codepoint (4 UTF-8 bytes). `chr(1200000)` (invalid) → U+FFFD
    // replacement, 1 codepoint. All deterministic; no invalid UTF-8 is produced.
    security_safe("attack_80_char_edge_cases", "1\n1\n1");
}

// ── Round 7 attacks: self-ref, closures-in-vector, atomic races, etc. ────

#[test]
fn sec_81_match_expr_some_arm_dropped() {
    // Fixed 2026-04-30. `lower_match_expr` was always switching to the
    // last arm's `next_test_bb` (which equals `merge_bb` when there's
    // no else), then writing `Constant::Unit` into the merge block —
    // overwriting whichever arm value the arms had just stored. The
    // sibling `lower_match_stmt_as_expr` already had the right shape:
    // gate the `switch_to` on `next_test_bb != merge_bb`, and only
    // emit a fallback assignment when there's an actual else arm.
    security_safe("attack_81_self_referential_struct", "6");
}

#[test]
fn sec_82_vector_of_closures() {
    // Fixed 2026-04-28. Three independent bugs all had to land for
    // `Vector[Callable]` to work:
    //  1. `infer_collection_element_type` resolves `Vector__Callable__…`
    //     to `FnPtr` so `gorget_array_new` is created with elem_size = 16
    //     (sizeof(GorgetClosure)) instead of 8.
    //  2. The LIR `Callable__…` Named type maps to `GorgetClosure`
    //     (16-byte struct), so `LoadRef` through the borrow returns the
    //     full closure value, not just the first 8 bytes.
    //  3. The CallExtern dispatch (used for `Vector__Callable__push`)
    //     runs `wrap_closure_call_args`, packing each closure literal
    //     into a `GorgetClosure` with a heap-alloc'd env before the
    //     runtime memcpys 16 bytes into the array slot.
    // Without (1) the array slot is too small; without (2) `.clone()`
    // on `Ref[Callable]` reinterprets fn_ptr as a memory address;
    // without (3) the slot's env is uninitialized after push.
    security_safe(
        "attack_82_vector_of_closures",
        "11\n102\n1000",
    );
}

#[test]
fn sec_83_parse_format_roundtrip() {
    security_safe(
        "attack_83_parse_format_roundtrip",
        "-9223372036854775808\n0.100000\n-0.000000",
    );
}

#[test]
fn sec_84_negative_subscript_traps() {
    // `v.get(-1)` → None (safe Option path). `v[-1]` traps via unsigned
    // bounds check (index UINT64_MAX > length). Error message prints
    // the unsigned value, which is misleading but not wrong.
    security_traps(
        "attack_84_negative_subscript",
        "index out of bounds",
    );
}

#[test]
fn sec_85_dict_struct_key_codegen() {
    // `@derive(Hashable, Equatable) struct Point` as a Dict key — the
    // key-equality wrapper now derefs and passes-by-value to match
    // `Point__eq(const void*, Point)`'s ABI, lookups by structurally-
    // equal keys hit, and the runtime drop-callback dispatch through
    // `__gorget_drop_fn` is well-typed under UBSan.
    security_safe(
        "attack_85_dict_struct_key",
        "origin-ish\norigin-ish",
    );
}

#[test]
fn sec_86_multi_arg_print() {
    // `print(evil, evil)` — the second positional arg is silently
    // ignored (print only uses the first positional; extra args would
    // need to be kwargs like terminator=/file=). User data is treated
    // as opaque %.*s payload — no format injection.
    security_safe(
        "attack_86_multi_arg_print",
        "header\n%s %n %d %%\n%s %n %d %%",
    );
}

#[test]
fn sec_87_atomic_counter_four_spawners() {
    // 4 threads × 10000 increments = 40000. No lost updates.
    security_safe("attack_87_atomic_counter_race", "40000");
}

#[test]
fn sec_88_iterator_fusion_take_after_filter_map() {
    // filter(x%7==0).map(*2).take(3) → [0, 14, 28]. Lazy — stops
    // early instead of materializing all 100 elements.
    security_safe("attack_88_iterator_fusion", "3\n0\n7000\n14000");
}

#[test]
fn sec_89_large_dict_drop() {
    // 1000-entry Dict[String, Vector[int]] drops cleanly.
    security_safe("attack_89_large_dict_drop", "1000");
}

#[test]
fn sec_90_vector_zero_capacity_then_push() {
    // reserve(0) is a no-op; push allocates on demand.
    security_safe("attack_90_vector_zero_capacity", "2\n42\n43");
}

#[test]
fn sec_91_callable_clone_outlives_source() {
    // Deep-clone path on Ref[Callable].clone(). Before the fix, .clone()
    // shallow-memcpy'd the GorgetClosure, so the cloned f shared its env
    // pointer with the Vector's slot. Vector elem_drop then UAF'd /
    // double-free'd at scope exit. The fix routes Callable through
    // gorget_closure_clone_to_owned + size-prefixed env malloc + emits
    // gorget_closure_free on FnPtr-typed locals. Each loop iteration's f
    // owns its own env; the Vector owns the slot's env independently.
    security_safe(
        "attack_91_callable_clone_outlives_source",
        "101\n101\n101",
    );
}

#[test]
fn sec_92_static_set_runtime() {
    // Gap (c) — static `Set`/`HashSet` populated at runtime. Before the GIR
    // Set arm existed, the static slot stayed a null header (`GlobalInit::
    // Zeroed`) → the first `.add` dereferenced null → SIGSEGV (exit 139) on
    // BOTH backends. This is the silent-crash class CLAUDE.md #7 flags: the
    // always-pass `*_comparison` diagnostics never see it. Under ASan the
    // fixed program must build clean and run exit-0; a regression to the
    // null-header state trips a SEGV report here instead of passing silently.
    security_safe(
        "attack_92_static_set_runtime",
        "true\nfalse\ntrue\nfalse\n4",
    );
}

// ════════════════════════════════════════════════════════════════════════════
// Round-30 Fix C — CoW bare-assign owned-String leak guards (Core #6).
//
// The removed Branch A of `lower_var_decl_assign_mode` (src/ir/lowering/stmts/
// mod.rs) `set_shared_heap`'d a `String v = <owned live heap source>` and
// `unregister`ed the source's drop, while the backend DEEP-copied the source
// into a second buffer via `gorget_string_copy_cow` — so the source's heap
// allocation LEAKED. The leaks are stdout-INVISIBLE (the program prints the
// right bytes and leaks), so only a `detect_leaks=1` run guards them.
//
// Each fixture was verified to FAIL at the pre-fix baseline (`a9b034f1`, arm
// present) under `--sanitize` + `detect_leaks=1` and to PASS post-fix:
//   owned_string   → LeakSanitizer 23B / 1 alloc  → clean
//   return_alias   → LeakSanitizer 14B / 1 alloc  → clean
//   struct_escape  → LeakSanitizer 19B / 1 alloc  → clean
//   view_source    → LeakSanitizer 32B / 2 allocs → clean
//   alias_chain    → LeakSanitizer 51B / 3 allocs → clean
// A future regression that re-adds the SharedHeap+unregister shape re-leaks and
// trips `exitcode=99` here.
// ════════════════════════════════════════════════════════════════════════════

#[test]
fn cow_bareassign_owned_string_no_leak() {
    // `String v = sb` with `sb` a heap-owned String (`a + b`), both live to
    // scope exit. Baseline leaked sb's 23-byte buffer.
    security_safe_no_leak(
        "cow_bareassign_owned_string_leak",
        "hello, cow-owned world\nhello, cow-owned world",
    );
}

#[test]
fn cow_bareassign_return_alias_no_leak() {
    // Heap-owned String bare-assigned, then RETURNED (escapes the frame).
    // Baseline leaked 14 bytes.
    security_safe_no_leak("cow_bareassign_return_alias_leak", "aaa-bbb-owned");
}

#[test]
fn cow_bareassign_struct_escape_no_leak() {
    // Heap-owned String bare-assigned, then stored into an owned struct field.
    // Baseline leaked 19 bytes.
    security_safe_no_leak(
        "cow_bareassign_struct_escape_leak",
        "field-escape-owned\nfield-escape-owned",
    );
}

#[test]
fn cow_bareassign_view_source_no_leak() {
    // Heap-owned String bare-assigned, a view (`.substring`) taken of the alias,
    // then materialized. Baseline leaked 32 bytes across 2 allocations.
    security_safe_no_leak(
        "cow_bareassign_view_source_leak",
        "view-\nview-source-owned-payload\nview-source-owned-payload",
    );
}

#[test]
fn cow_bareassign_alias_chain_no_leak() {
    // Transitive alias chain off one heap-owned String, all live to scope exit.
    // Baseline leaked 51 bytes across 3 allocations (one per hop).
    security_safe_no_leak(
        "cow_bareassign_alias_chain_leak",
        "chain-owned-tail\nchain-owned-tail\nchain-owned-tail\nchain-owned-tail",
    );
}

#[test]
fn box_resource_struct_no_leak() {
    // `Box[Money]` local, Money owning a heap `String` (Recursive drop). The
    // box local's scope-exit drop must run `Money__drop` before the box free.
    // Baseline leaked the inner heap String (22 bytes/box) — `drops.rs`'s
    // Box-inner-drop match had no `DropStrategy::Recursive` arm.
    security_safe_no_leak("box_resource_struct_leak", "box-struct-ok");
}

#[test]
fn box_resource_enum_no_leak() {
    // `Box[Msg]` local, Msg an enum with a resource-typed variant payload
    // (Recursive drop). Same Box-inner-drop hole as the struct case; the box
    // local's scope-exit drop must run the enum-dispatch `Msg__drop`.
    security_safe_no_leak("box_resource_enum_leak", "box-enum-ok");
}

// KNOWN GAP — box sibling #2 (filed 2026-07-24): `Box[user-Drop struct with a
// droppable field]` leaks the field. The user `drop` runs ("dropping") but the
// field frees (in `__gorget_dtor`) are skipped — a wrong-fn-selection shared
// with `box_inner_drop_fn`. Un-ignore + promote when the CLASS is fixed.
#[test]
#[ignore = "KNOWN GAP box sibling #2: Box[user-Drop-struct-with-field] leaks the field; \
TODO.md. Un-ignore when box_inner_drop_fn/inline both target __gorget_dtor_R."]
fn box_user_drop_struct_field_no_leak() {
    security_safe_no_leak("box_user_drop_struct_field_leak", "done\ndropping");
}

// KNOWN GAP — R1 (filed 2026-07-24, MEMORY-SAFETY): moving a `Box[T]` via `!bx`
// into a consuming `!`-param fn BAD-FREES a stack address on ALL Box[T] (accepted
// by `gg check`; ASan "attempting free on address which was not malloc()-ed",
// both backends). The `!`-param Box-consumption ABI passes `&slot` and the callee
// frees it. Un-ignore when the consumption ABI is fixed. See TODO.md.
#[test]
#[ignore = "KNOWN GAP R1: !-param Box move bad-frees a stack address (accepted \
program, ASan abort, both backends); TODO.md. Un-ignore when the !-Box consume ABI is fixed."]
fn box_move_param_bad_free_safe() {
    security_safe("box_move_param_bad_free", "consumed\ndone");
}

// ════════════════════════════════════════════════════════════════════════════
// `&`-param at an ownership boundary — return / bind / re-assign
// ════════════════════════════════════════════════════════════════════════════
//
// A `&`-param is a BORROW: the caller keeps ownership. Crossing an ownership
// boundary owes exactly ONE clone; a typed binding owes ZERO (borrow now,
// materialize on mutation). Every fixture below reported a double-free (or, for
// the String arm, an LSan leak) before the statement `return` was routed through
// the shared materialize chokepoint and the auto-deref temp started carrying its
// borrow provenance. The bug class is stdout-INVISIBLE, so these run under
// LeakSanitizer via `security_safe_no_leak`, not a plain stdout check.

#[test]
fn retborrow_return_amp_param_vector_no_leak() {
    // Arm 2: `return v` of a whole `Vector[int] &`-param, non-throws.
    security_safe_no_leak("retborrow_return_amp_param_vector", "4\n4");
}

#[test]
fn retborrow_bind_amp_param_vector_no_leak() {
    // Arm 4: bind then return. The bind is a CoW Ptr alias of the param.
    security_safe_no_leak("retborrow_bind_amp_param_vector", "4\n4");
}

#[test]
fn retborrow_reassign_amp_param_vector_no_leak() {
    // Arm 5: re-assign into an existing owned slot, then return.
    security_safe_no_leak("retborrow_reassign_amp_param_vector", "4\n4");
}

#[test]
fn retborrow_return_amp_param_string_no_leak() {
    // Arm 6: the String `&`-param return — two clones and an orphaned buffer.
    security_safe_no_leak("retborrow_return_amp_param_string", "5\n5");
}

#[test]
fn retborrow_bind_amp_param_struct_no_leak() {
    // Receiver-shape width: a user struct with a resource field.
    security_safe_no_leak("retborrow_bind_amp_param_struct", "4\n4\ntag");
}

#[test]
fn retborrow_return_amp_param_dict_no_leak() {
    // Receiver-shape width: a Dict with heap-allocated keys.
    security_safe_no_leak("retborrow_return_amp_param_dict", "3\n3");
}

#[test]
fn retborrow_bind_then_mutate_no_leak() {
    // The alias-at-bind's correctness obligation: materialize on mutation, so
    // the CALLER's value is unchanged. Two receiver shapes.
    security_safe_no_leak("retborrow_bind_then_mutate", "5\n4\n4\n4\n3\n3");
}

#[test]
fn retborrow_mutate_through_control_no_leak() {
    // CONTROL: mutating the `&`-param itself must still WRITE THROUGH — so the
    // aliasing fix cannot degenerate into "always clone".
    security_safe_no_leak("retborrow_mutate_through_control", "5\n999");
}

#[test]
fn retborrow_throws_bare_return_no_leak() {
    // CONTROL: the throws bare-`return v` leg was already clean and must stay so.
    security_safe_no_leak("retborrow_throws_bare_return", "4\n4");
}

#[test]
fn retborrow_no_over_clone_controls_no_leak() {
    // ANTI-OVER-CLONE controls: owned-local return · bare-param return ·
    // `!`-param return · bind-then-read-only.
    security_safe_no_leak("retborrow_no_over_clone_controls", "4\n4\n4\n3\n42\n4");
}

// ── DESCOPED arms: `return &v` is a VALUE-POSITION `&` ──────────────────────
// ⚖ Owner ruling: a value-position `&` must be REJECTED at check time, and the
// ruling explicitly covers `return &v`. Both fixtures below are therefore
// `security_known_unsafe` pins on a program the compiler should not accept —
// NOT accept-polarity fixtures, which would wire the wrong answer into a
// committed artifact. They fail loudly when the reject lands.

#[test]
fn retborrow_valuepos_amp_return_known_unsafe() {
    security_known_unsafe(
        "retborrow_valuepos_amp_return",
        KnownBug::SilentlyProduces("4\n4"),
        "a value-position `&` (`return &v`) is ruled to be REJECTED at check time, \
         but the compiler still accepts it and runs it to completion. The DOUBLE-FREE \
         it used to produce is closed (the return now routes through the boundary \
         chokepoint, whose pointee test covers MutPtr); the remaining defect is the \
         accept/reject polarity. Reclassify to `security_rejected` when the \
         value-position-`&` reject lands",
    );
}

/// KNOWN GAP (filed 2026-07-25 by the Track-B1 output-review). Re-assigning
/// THROUGH a `&`-param never drops the param slot's OLD pointee, so the caller's
/// original buffer leaks 64 bytes.
///
/// PRE-EXISTING and independent of the return boundary — it reproduces
/// identically before and after Track B1. It was MASKED in the combined costume
/// (`local = v; v = build(2); return local`), where the bind's double-free
/// happened to cancel the missing drop; fixing the bind correctly unmasks it.
///
/// The write-through VALUES are already correct (4 then 2) — only the drop of the
/// old value is missing, which is why this asserts the INTENDED no-leak state and
/// is `#[ignore]`d rather than pinning today's behaviour. `security_known_unsafe`
/// is the wrong harness here: it runs with `detect_leaks=0`, so a pure leak does
/// not trip it. UN-IGNORE when the write-through re-assign drops the old pointee.
#[test]
#[ignore = "known gap: write-through re-assign leaks the old pointee; asserts the INTENDED no-leak state"]
fn reassign_amp_param_leaks_old_value() {
    security_safe_no_leak("reassign_amp_param_leaks_old_value", "4\n2\n");
}

#[test]
fn retborrow_valuepos_amp_return_throws_known_unsafe() {
    security_known_unsafe(
        "retborrow_valuepos_amp_return_throws",
        KnownBug::SanitizerTrips,
        "`return &v` in a THROWS fn is ruled to be REJECTED, and is additionally \
         still memory-unsafe: the throws return keeps its own hand-rolled \
         `GirType::Ptr(inner)`-only clone, blind to the MutPtr an `&`-param is, so \
         the caller's buffer is forwarded raw and double-freed. Reclassify when \
         either the reject or the throws-leg materialize lands",
    );
}

// ════════════════════════════════════════════════════════════════════════════
// CoW aliasing-soundness durable repros (filed 2026-07-25).
//
// Every fixture below was measured at HEAD BEFORE being committed, and the
// observed pre-fix failure is recorded in its `.gg` header (Core #12: a fixture
// is not coverage until it has been seen to FAIL). Each `#[ignore]` reason names
// exactly what un-ignores it.
//
// The intent polarity is deliberate throughout: these assert the INTENDED state,
// never today's behaviour, so none of them can make a defect canonical.
// `security_known_unsafe` is NOT used here for that reason — it pins the bug as
// present, which is the opposite of what a durable repro for an unfixed defect
// should encode when the intended state is already known.
//
// LIVE (not ignored) members of the batch are the anti-regression CONTROLS: they
// are green today and go RED if a fix over-reaches. They are the other half of
// Core #12 — the axis is only covered when both the broken and the
// accidentally-working cells are pinned.
// ════════════════════════════════════════════════════════════════════════════

// ── A1: Callable-typed local bound to an `&`-param function ─────────────────

/// REGRESSION — A1 cell of the callable-indirection axis: a `Callable`-typed
/// LOCAL bound to an `&`-param function writes through on indirect call.
/// (`f = bumpint; f(&x)` — plain-local int root × LOCAL callable × `&`-arg.)
/// Pre-Track-B1: `gg check` clean, build clean, binary exit 139 (SIGSEGV);
/// under ASan, `SEGV on unknown address 0x29 ... in bumpint`. Fix wires the
/// callable's declared `param_ownerships` into `lower_call_arg` at both
/// indirect-call arms in `src/ir/lowering/exprs/calls.rs`.
#[test]
fn sound_callable_amp_param_indirect_call_safe() {
    security_safe("sound_callable_amp_param_indirect_call", "42");
}

/// KNOWN GAP — MEMORY-UNSAFE. An escaping closure that captured a local by
/// reference reads a dropped stack slot. `gg check` accepts; both backends
/// print a raw address where 1 then 2 belong.
///
/// The discriminator is why it slips through: `E_DanglingReturn` DOES fire when
/// the closure returns the captured value, but not when it merely reads it — so
/// the check appears keyed on the closure's return value rather than on what it
/// captures.
#[test]
#[ignore = "KNOWN GAP: an escaping closure reads its captured local from freed stack on both \
backends while `gg check` accepts. Asserts the INTENDED 1/2 (or a check-time reject); TODO.md."]
fn sound_closure_escaping_capture_dangles_safe() {
    security_safe("sound_closure_escaping_capture_dangles", "1\n2");
}

/// LIVE CONTROLS — the CALLEE-DISPOSITION axis. Green today and must stay so.
///
/// They guard a measured hazard: routing `exprs/calls.rs:371` through
/// `ensure_owned_at_consuming_arg` (whose Case 1 registers the clone for drops)
/// adds a caller-side drop with no compensating `MoveZero`. Under ASan that is
/// a double-free — but ONLY when the callee moves its `!` param ONWARD, because
/// `drop_if_alive` is self-nulling and makes the extra drop a no-op when the
/// callee merely drops it. That is why the pre-existing fixture over this site
/// stayed green under the same change, and why this axis needed its own net.
#[test]
fn consume_callee_moves_on_return_safe() {
    security_safe("consume_callee_moves_on_return", "4\ndone");
}

#[test]
fn consume_callee_moves_on_stash_safe() {
    security_safe("consume_callee_moves_on_stash", "1\n3\ndone");
}

/// KNOWN GAP — `&`-of-a-projection in an OPERAND position DUPLICATES the user
/// `Drop`. Measured at HEAD: prints `1 / close 9 / close 9`, while the control
/// with the sigil removed prints `1 / close 9`. `&` alone is the cause.
///
/// Same root as the array-literal costume: `reject_tainted_formation_arg` is
/// wired only at `check_expr.rs:116` and `:757`, both `CallArg` positions — so
/// the documented characterisation "literal-element positions escape 2T"
/// undercounts it. EVERY non-`CallArg` position escapes, operands included.
#[test]
#[ignore = "KNOWN GAP: `&`-of-a-place in an operand position runs the owner's Drop twice \
(control runs it once). Asserts the INTENDED single drop; TODO.md."]
fn sound_amp_operand_position_duplicate_drop_safe() {
    security_safe("sound_amp_operand_position_duplicate_drop", "1\nclose 9");
}

/// REGRESSION — A2 cell of the callable-indirection axis: a PROJECTION arg
/// through a `Callable`-typed PARAMETER writes through on indirect call.
/// (`struct-`&`-param projection root × PARAM callable × `&`-arg`.) Pre-Track-B1:
/// SEGV on both backends at runtime; the UNIT_TYPE arm in
/// `src/ir/lowering/exprs/calls.rs` (Callable-PARAMETER, `__callable_N`) chose
/// pointer-vs-value from the argument's provenance rather than the callee's
/// declared param types. Fix populates `callable_param_types` +
/// `callable_param_ownerships` for the parameter and routes each user arg
/// through `lower_call_arg`.
#[test]
fn sound_callable_amp_param_projection_safe() {
    security_safe("sound_callable_amp_param_projection", "11\n11");
}

// ── D10 exclusivity holes: accepted programs that heap-use-after-free ────────
//
// All eleven assert the INTENDED REJECT (`E_BorrowConflict`). Measured at HEAD:
// every one of them passes `gg check` and then trips AddressSanitizer with a
// heap-use-after-free — the per-fixture stack frame is recorded in each header.
// ggdef ACCEPTS these (it is a value-semantics interpreter, so the class is
// unobservable in that lane) — see the oracle note in
// `sound_excl_getchain_local.gg`; the intended reject is the ratified owner
// ruling, not ggdef's verdict.

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity hole #1, get-chain view arg): accepted program, ASan \
heap-use-after-free in gorget_string_copy_cow. Asserts the INTENDED E_BorrowConflict reject; \
TODO.md. Un-ignore when the semantic place-overlap chokepoint lands."]
fn sound_excl_getchain_local_rejected() {
    security_rejected("sound_excl_getchain_local", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity hole #3, Static root): accepted program, ASan \
heap-use-after-free; the byte-identical LOCAL spelling already rejects. Asserts the INTENDED \
E_BorrowConflict reject; TODO.md. Un-ignore when the chokepoint admits Static roots."]
fn sound_excl_getchain_static_rejected() {
    security_rejected("sound_excl_getchain_static", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity hole #1, field-path costume): accepted program, ASan \
heap-use-after-free. Asserts the INTENDED E_BorrowConflict reject; TODO.md. Un-ignore when the \
chokepoint's place resolver descends field paths through view chains."]
fn sound_excl_getchain_fieldpath_rejected() {
    security_rejected("sound_excl_getchain_fieldpath", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity, alias-provenance costume): accepted program, ASan \
heap-use-after-free. Asserts the INTENDED E_BorrowConflict reject; TODO.md. Un-ignore when the \
chokepoint resolves a CoW bind alias to its root."]
fn sound_excl_getchain_alias_provenance_rejected() {
    security_rejected("sound_excl_getchain_alias_provenance", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity hole #2, method receiver): accepted program, ASan \
heap-use-after-free. Asserts the INTENDED E_BorrowConflict reject; TODO.md. Un-ignore when the \
receiver becomes a participant (&self = writer, bare self = reader)."]
fn sound_excl_receiver_writer_rejected() {
    security_rejected("sound_excl_receiver_writer", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity hole #4, for-body &-arg writer): accepted program, ASan \
heap-use-after-free in gorget_string_borrow; the direct `v.push` spelling already rejects. \
Asserts the INTENDED E_BorrowConflict reject; TODO.md. Un-ignore when the iterable guard sees \
through a call boundary."]
fn sound_excl_forbody_amp_writer_rejected() {
    security_rejected("sound_excl_forbody_amp_writer", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity, nested writer one call deep): accepted program, ASan \
heap-use-after-free. Asserts the INTENDED E_BorrowConflict reject; TODO.md. Un-ignore when a \
writer inside a sibling arg's expression tree becomes a participant."]
fn sound_excl_nested_writer_call_rejected() {
    security_rejected("sound_excl_nested_writer_call", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity, nested writer at a CLOSURE call): accepted program, \
ASan heap-use-after-free in __Closure_0__call. Asserts the INTENDED E_BorrowConflict reject; \
TODO.md. Un-ignore when closure calls route through the chokepoint."]
fn sound_excl_nested_writer_closure_rejected() {
    security_rejected("sound_excl_nested_writer_closure", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity, nested writer in an INDEX STORE): accepted program, \
ASan heap-use-after-free. Asserts the INTENDED E_BorrowConflict reject; TODO.md. Un-ignore when \
index-store value operands route through the chokepoint."]
fn sound_excl_nested_writer_index_store_rejected() {
    security_rejected("sound_excl_nested_writer_index_store", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10(b) ADDENDUM 3 amendment, ctor arg vs sibling WRITER): accepted \
program, ASan heap-use-after-free in gorget_string_clone_to_owned — the boundary clone is \
emitted AFTER sibling evaluation. Asserts the INTENDED E_BorrowConflict reject; TODO.md + \
decisions.md. Un-ignore when aggregate init participates on the writer axis."]
fn sound_excl_nested_writer_ctor_rejected() {
    security_rejected("sound_excl_nested_writer_ctor", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10(b) ADDENDUM 3 amendment, TUPLE twin of the ctor row): accepted \
program, ASan heap-use-after-free. Asserts the INTENDED E_BorrowConflict reject; TODO.md + \
decisions.md. Un-ignore when tuple_init participates on the writer axis."]
fn sound_excl_nested_writer_tuple_rejected() {
    security_rejected("sound_excl_nested_writer_tuple", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity, operator overload + bind alias): accepted program, ASan \
heap-use-after-free in Acc__add. NOTE `a + a` runs clean and is false confidence — the ALIAS \
twin is the real shape. Asserts the INTENDED E_BorrowConflict reject; TODO.md. Un-ignore when \
operator-overload calls route through the chokepoint with receiver participation."]
fn sound_excl_overload_alias_twin_rejected() {
    security_rejected("sound_excl_overload_alias_twin", "E_BorrowConflict");
}

// ── D10 hole #5: the one with the OPPOSITE polarity ─────────────────────────

/// Owner-ruled MATERIALIZE, not reject (TODO.md): the mutation site is visible,
/// so the clone is placed lazily at the binding. Measured at HEAD: `gg check`
/// clean, ASan heap-use-after-free in `gorget_string_copy_cow`.
#[test]
#[ignore = "KNOWN GAP (D10 exclusivity hole #5, match-scrutinee element): accepted program, \
ASan heap-use-after-free. Owner-ruled MATERIALIZE (NOT reject) — asserts the INTENDED \
pre-mutation value; TODO.md. Un-ignore when the match-binding materialize lands. A fix that \
REJECTS this program has implemented the wrong ruling."]
fn sound_match_scrutinee_elem_materialize_safe() {
    security_safe("sound_match_scrutinee_elem_materialize", "alpha-heap-forced-payload");
}

// ── The `&`-in-an-owning-position class: the DROP facet ─────────────────────

/// Measured at HEAD, BOTH backends: `1 / close 9 / close 9` — the owner's user
/// `Drop` runs TWICE. ggdef REJECTS the literal form. The intended state is the
/// reject; this fixture additionally pins that if it were ever accepted, the
/// drop must run exactly ONCE (a single-owner `Drop` violation is not an
/// acceptable outcome under any reading).
#[test]
#[ignore = "KNOWN GAP (&-in-an-owning-position, DROP facet): `[&p.fd]` duplicates the owner's \
user Drop on C AND LLVM; ggdef rejects. Asserts the INTENDED single drop; TODO.md. Un-ignore \
when the owning-position reject (or the drop-taint gate at container-literal element positions) \
lands."]
fn sound_amp_array_literal_duplicate_drop_safe() {
    security_safe("sound_amp_array_literal_duplicate_drop", "1\nclose 9");
}

/// LIVE CONTROL — the unsigilled twin. Green today; goes RED if a fix changes
/// how a bare container-literal element takes ownership.
#[test]
fn sound_amp_array_literal_duplicate_drop_control_safe() {
    security_safe("sound_amp_array_literal_duplicate_drop_control", "1\nclose 9");
}

// ════════════════════════════════════════════════════════════════════════════
// Round XXII β — Option/Result combinator adapter payload-registration class
// ════════════════════════════════════════════════════════════════════════════
//
// Pre-fix (scout `/tmp/round_xxii_trackBeta_scout_79000.md`), the combinator
// adapter's extracted payload (`try_lower_option_result_combinator` at
// `src/ir/lowering/exprs/methods.rs:3795-3922`) was `set_owned` but never
// `drops.register_local(...)`. Arms whose closure only BORROWED the payload
// (map · flat_map · and_then · map_err · unwrap_or_else-Result-Error with a
// mapped-away return type) leaked 24-56B of the payload's heap bytes per
// call — no owner registered, no scope-exit `DropIfAlive` fired. Class-fix
// (Track β, 2026-08-01): route all 5 extraction sites through the shared
// `extract_enum_payload_owned` helper (Core #3 birth-registration + Core #4
// producer); pair the 4 currently-clean aliasing wrap sites with
// `mov(payload) + move_zero(payload)` to avoid a new double-free (Core #10;
// the LIR does not zero the source slot on a Move operand).
//
// Every fixture below RED-verified against the pre-fix compiler (Track β
// executor, 2026-08-01, HEAD 0aa3f5a6 with methods.rs helper reverted):
//   combinator_leak_map_money_param                → 24B / 1 alloc  → clean
//   combinator_leak_flat_map_money_param           → 32B / 1 alloc  → clean
//   combinator_leak_and_then_money_param           → 40B / 1 alloc  → clean
//   combinator_leak_map_err_money_param            → 48B / 1 alloc  → clean
//   combinator_leak_unwrap_or_else_result_money_param → 56B / 1 alloc → clean
// A future regression that removes the birth-registration OR the b1 pairing
// re-leaks the payload and trips `exitcode=99` here.

#[test]
fn combinator_leak_map_money_param_no_leak() {
    // AXIS: Option[Money].map((Money m): int) — mapped-away return type;
    // closure only borrows payload. Pre-fix: `Direct leak of 24 byte(s)
    // in 1 object(s)` via `Option__Money__clone → Money__clone →
    // gorget_array_clone` on the extracted payload.
    security_safe_no_leak("combinator_leak_map_money_param", "3");
}

#[test]
fn combinator_leak_flat_map_money_param_no_leak() {
    // AXIS: Option[Money].flat_map((Money m): Option[int]) — sibling arm
    // (closure returns Option[U]). Pre-fix: `32 byte(s) in 1 object(s)`.
    security_safe_no_leak("combinator_leak_flat_map_money_param", "4");
}

#[test]
fn combinator_leak_and_then_money_param_no_leak() {
    // AXIS: Option[Money].and_then((Money m): Option[int]) — cross-type
    // sibling. Pre-fix: `40 byte(s) in 1 object(s)`.
    security_safe_no_leak("combinator_leak_and_then_money_param", "5");
}

#[test]
fn combinator_leak_map_err_money_param_no_leak() {
    // AXIS: Result[int, Money].map_err((Money m): int) — Error-branch
    // sibling; err_val extraction had the same missing-registration hole.
    // Pre-fix: `48 byte(s) in 1 object(s)`.
    security_safe_no_leak("combinator_leak_map_err_money_param", "6");
}

#[test]
fn combinator_leak_unwrap_or_else_result_money_param_no_leak() {
    // AXIS: Result[int, Money].unwrap_or_else((Money m): int) — Error
    // branch err_val borrowed into closure; Ok branch keeps original int.
    // Pre-fix: `56 byte(s) in 1 object(s)`.
    security_safe_no_leak("combinator_leak_unwrap_or_else_result_money_param", "7");
}

// ── B5: the Option/Result combinator skips the payload's user Drop ──────────

/// Measured at HEAD under `--sanitize` + `detect_leaks=1`:
/// `512 / control-end / dropping / 512 / map-end` — the trailing `dropping` is
/// MISSING — plus `LeakSanitizer: 8192 bytes in 2 allocations`. The `is Some`
/// control leg in the same program is clean, which is what makes the map leg's
/// silence a defect rather than a design choice.
#[test]
#[ignore = "KNOWN GAP (combinator adapter): Option.map skips the payload's user Drop and leaks \
8192B; the `if o is Some` spelling of the same program is clean. Asserts the INTENDED \
drop-runs/no-leak state; TODO.md. Un-ignore when the combinator adapter owns its payload."]
fn sound_option_map_user_drop_no_leak() {
    security_safe_no_leak(
        "sound_option_map_user_drop_leak",
        "512\ncontrol-end\ndropping\n512\nmap-end\ndropping",
    );
}

// ── Anti-regression CONTROLS for the aggregate-init mover exemption ─────────

/// LIVE — D10(b) ADDENDUM 3's mover exemption. Green today; goes RED if the
/// chokepoint rebuild blanket-rejects aggregate init (a new over-rejection).
#[test]
fn sound_ctor_mover_sibling_accept_safe() {
    security_safe(
        "sound_ctor_mover_sibling_accept",
        "alpha-heap-forced-payload\nbeta-heap-forced-payload\ngamma-heap-forced-payload",
    );
}

/// The one row of the batch whose intended state is MORE permissive than
/// today's. Measured at HEAD: `gg check` REJECTS with `E_BorrowConflict` while
/// the struct and tuple spellings of the identical program accept.
#[test]
#[ignore = "KNOWN GAP (over-rejection): enum-variant construction routes through the \
CALL-shaped aliasing check, so `E.Two(v[0], !v)` rejects while `Pair(v[0], !v)` accepts. \
D10(b) ADDENDUM 3 ratifies the WIDENING; asserts the INTENDED accept. Un-ignore when enum \
ctors use the aggregate-init predicate."]
fn sound_enum_ctor_mover_sibling_accept_safe() {
    security_safe("sound_enum_ctor_mover_sibling_accept", "alpha-heap-forced-payload");
}

/// LIVE AXIS PIN — the mover-FIRST ordering must reject on the LIVENESS axis
/// (`E_UseAfterMove`), not the aliasing axis. The mover exemption above is only
/// sound because this case is already caught here; if a refactor makes this
/// fail with `E_BorrowConflict` instead, the move-tracker silently lost a case.
#[test]
fn sound_ctor_mover_first_rejected() {
    security_rejected("sound_ctor_mover_first_reject", "E_UseAfterMove");
}

/// LIVE — the intra-function sever. Green today; goes RED if a chokepoint
/// rejects on "a view of `v` exists in scope" instead of "a view of `v` is live
/// at this call".
#[test]
fn sound_getchain_sever_accept_safe() {
    security_safe("sound_getchain_sever_accept", "alpha-heap-forced-payload");
}

/// LIVE — the thin-pointer half of the `&`-of-field write-through axis, i.e.
/// the two cells that work by accident (Core #12's type case). Green today;
/// goes RED if the by-value fix "unifies" the halves by breaking this one.
#[test]
fn sound_amp_field_thinptr_control_safe() {
    security_safe("sound_amp_field_thinptr_control", "base-grown\n2\n99");
}

/// LEAK GUARD — `f(&(*box).field)` must NOT clone the whole struct to take an
/// address of. Before the Family-1 chokepoint it did, leaking the clone's
/// deep-copied `String` field (`gorget_string_clone_to_owned` <- `Holder__clone`
/// <- `main`) while ALSO discarding the callee's write.
///
/// RED-VERIFIED against the pre-fix compiler: `SUMMARY: AddressSanitizer:
/// 5 byte(s) leaked in 1 allocation(s)`, exit 99. ggdef is structurally blind to
/// this class, so ASan is the instrument (Core #13). See the fixture header for
/// why the field is literal-seeded — a heap seed makes this RED post-fix for the
/// separately-filed "re-assign through a `&`-param drops nothing" leak.
#[test]
fn sound_amp_deref_box_field_leak_safe() {
    security_safe_no_leak("sound_amp_deref_box_field_leak", "gg-payload\n9");
}

/// KNOWN GAP — assigning to a `String` element inside `for … in &coll`
/// DOUBLE-FREES. `gg check` passes, `gg build` succeeds, the binary aborts with
/// "free(): double free detected in tcache 2".
///
/// The memory-unsafe cell of a type axis that is otherwise merely wrong or
/// correct, measured on the same loop shape: `int` element -> write LOST;
/// struct and `Vector[int]` elements -> write through CORRECTLY; `String`
/// element -> double free. So the reference's former blanket claim that
/// "element write-through through a loop iterable is lost" was wrong twice —
/// false for struct/Vector, and understating heap corruption as a lost write
/// for String (Core #8 on top of Core #12).
///
/// Asserts the INTENDED write-through (`x!`) running clean under ASan. If the
/// language instead rules whole-element assignment under `&`-iteration
/// inexpressible, replace this with the check-time reject — an accepted
/// program that double-frees is wrong under either reading.
#[test]
#[ignore = "KNOWN GAP: assigning a String element inside `for e in &coll` double-frees \
(gg check passes, binary aborts). Asserts the INTENDED write-through under ASan; TODO.md. \
Un-ignore when the element-assign releases the old buffer, or replace with the reject."]
fn sound_loop_string_elem_assign_double_free() {
    security_safe("sound_loop_string_elem_assign_double_free", "x!");
}
