//! Executable structural guard: the hand-written C runtime must compile with
//! NO implicitly-declared functions.
//!
//! Background (the strptime class). glibc gates `strptime()` behind
//! `__USE_XOPEN` (`_XOPEN_SOURCE` / `_GNU_SOURCE`), NOT `__USE_MISC`, so
//! `_DEFAULT_SOURCE` alone leaves it *implicitly declared* on Linux. An implicit
//! declaration is assumed to return `int`, which truncates `strptime`'s 64-bit
//! `char*` result to 32 bits — silently corrupting `rest` inside
//! `gorget_parse_time` and producing garbage time parsing. macOS declares
//! `strptime` unconditionally, so the bug is x86_64-Linux-specific and invisible
//! to a macOS-only test pass.
//!
//! What this guard is for NOW — the original rationale has been fixed at the
//! writer site and no longer applies. It was written because the build paths
//! compiled the runtime `.c` with a blanket `-w`, which suppresses *all*
//! warnings including implicit-function-declaration (verified, gcc 12.2.0:
//! `-w` defeats BOTH `-Werror=implicit-function-declaration` AND `#pragma GCC
//! diagnostic error`, in either order). That `-w` is **gone from `src/`
//! entirely**, and both C-compile sites now carry the flag themselves:
//! `src/main.rs:1141` (C backend) and `:1443` (the LLVM path's separate
//! runtime compile). So an ordinary `gg build` now catches this class —
//! measured by disabling `_XOPEN_SOURCE` in the preamble, which reds the
//! `time_format` integration tests, not just this guard.
//!
//! This guard therefore survives as DEFENSE IN DEPTH, and that is a narrower
//! claim than it once made: it applies
//! `-Werror=implicit-function-declaration` *itself*, independently of
//! `main.rs`, so it still fires if either cc site ever drops the flag — the
//! regression that would silently return the whole class to invisibility. If
//! that argument ever stops being worth a test target, delete this file rather
//! than letting the comment drift again.
//!
//! What this guard does (per the executable-guard style in
//! `docs/devbook/25-structural-guards.md`): it emits the full self-contained C
//! for a fixture that exercises the time runtime (`gorget_parse_time` →
//! `strptime`) via `gg build <fixture> --emit-c-lir`, then recompiles that
//! emitted C with `cc -Werror=implicit-function-declaration` and NO `-w`. If any
//! hand-written runtime function is implicitly declared the recompile fails and
//! so does this test. Prose rots; this guard doesn't.
//!
//! This is a *build-time* guard at the writer site (the runtime preamble's
//! feature-test macros + the system-header gating), not an IR walk — the
//! invariant it polices ("no runtime function is implicitly declared") is a
//! property of the C compilation, so the guard lives at the C-compilation
//! boundary.

use std::path::{Path, PathBuf};
use std::process::Command;

/// The C compiler the production build would use (`$CC`, else `cc`) — mirrors
/// `src/main.rs`'s discovery so this guard exercises the same toolchain.
fn cc() -> String {
    std::env::var("CC").unwrap_or_else(|_| "cc".to_string())
}

/// The pre-built `gg` binary, supplied by cargo for this test target.
fn gg_binary() -> &'static Path {
    Path::new(env!("CARGO_BIN_EXE_gg"))
}

/// Emit the full self-contained C for `fixture` (preamble + runtime + user code)
/// via `gg build --emit-c-lir`, recompile it with
/// `-Werror=implicit-function-declaration` and NO `-w`, and assert it compiles
/// clean. A failure means some hand-written runtime function is implicitly
/// declared — the strptime-truncation class.
fn assert_no_implicit_decls(fixture: &str) {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture_path = manifest_dir.join("tests/fixtures").join(fixture);
    assert!(
        fixture_path.exists(),
        "guard fixture not found: {}",
        fixture_path.display()
    );

    // 1. Emit the full C (preamble + runtime + user code) to a file.
    let tmp_dir = manifest_dir.join("target").join("runtime_compile_guard");
    std::fs::create_dir_all(&tmp_dir).unwrap();
    let stem = fixture_path.file_stem().unwrap().to_str().unwrap();
    let emitted_c = tmp_dir.join(format!("{stem}.emitted.c"));

    let emit = Command::new(gg_binary())
        .arg("build")
        .arg(&fixture_path)
        .arg("--emit-c-lir")
        .output()
        .expect("failed to run `gg build --emit-c-lir`");
    assert!(
        emit.status.success(),
        "`gg build {} --emit-c-lir` failed:\n{}",
        fixture,
        String::from_utf8_lossy(&emit.stderr)
    );
    std::fs::write(&emitted_c, &emit.stdout).unwrap();

    // 2. The fixture must actually pull the function under guard into the
    //    emitted runtime, or the guard would silently pass on nothing. If this
    //    trips, the fixture stopped exercising the time runtime (RUNTIME_IO) and
    //    needs replacing — not deleting.
    let emitted = String::from_utf8_lossy(&emit.stdout);
    assert!(
        emitted.contains("strptime"),
        "guard fixture {fixture} no longer emits `strptime` (RUNTIME_IO) — \
         the strptime class is no longer covered; pick a fixture that calls \
         `parse_time`"
    );

    // 3. Recompile the emitted C with implicit-function-declaration promoted to
    //    an error and, critically, NO `-w` (which would defeat it). No `-Wall`
    //    either — only default warnings plus the one promotion — so this guard
    //    is about implicit decls specifically, not the broader warning posture.
    let compile = Command::new(cc())
        .arg("-O2")
        .arg("-std=c11")
        .arg("-Werror=implicit-function-declaration")
        .arg("-c")
        .arg("-o")
        .arg(tmp_dir.join(format!("{stem}.o")))
        .arg(&emitted_c)
        .arg("-lm")
        .output()
        .expect("failed to run cc on emitted runtime C");

    if !compile.status.success() {
        panic!(
            "Runtime C has an implicitly-declared function (the strptime class). \
             A missing feature-test macro in src/backend/c/runtime/runtime_preamble.c \
             leaves a libc function implicitly declared, so its return value is \
             truncated to `int`. Fix the writer site (the preamble), not the \
             caller.\n\
             Emitted C: {}\n\
             cc stderr:\n{}",
            emitted_c.display(),
            String::from_utf8_lossy(&compile.stderr)
        );
    }
}

/// `time_format.gg` calls `parse_time`, which forces the time runtime
/// (`RUNTIME_IO`, containing `gorget_parse_time` → `strptime`) into the emitted
/// C. With the `_XOPEN_SOURCE` feature-test macro in the preamble this compiles
/// clean under `-Werror=implicit-function-declaration`; without it the recompile
/// fails on `implicit declaration of function 'strptime'`.
#[test]
fn runtime_has_no_implicit_function_declarations() {
    assert_no_implicit_decls("time_format.gg");
}
