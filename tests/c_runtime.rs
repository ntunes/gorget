//! The C runtime, tested at the C level.
//!
//! Both tests here bypass the Gorget language pipeline and exercise
//! `src/backend/c/runtime/*.c` directly as a C library. Nothing in
//! `integration.rs` can do this — every test there goes source -> `gg build`
//! -> stdout, and so only reaches the runtime through whatever the compiler
//! happens to emit.
//!
//! - `str_fat_ptr_runtime` links the runtime units into a hand-written C
//!   `main()` and calls `gorget_str_*` directly: construction, comparison,
//!   slicing, UTF-8 indexing, cstr round-trips.
//! - `runtime_has_no_implicit_function_declarations` recompiles the emitted C
//!   under `-Werror=implicit-function-declaration` to police the strptime
//!   class (see its own doc comment for why it still earns its place).

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

use gorget::backend::c::c_runtime::{
    PANIC_NORMAL, RUNTIME_PREAMBLE, RUNTIME_STRING, RUNTIME_STRING_BASE_OPS,
};
use gorget::proc_guard::{RunFailure, run_with_deadline};

/// `cc` and the produced binary both go through the shared deadline runner.
///
/// They used to be bare `.output()` calls: NO deadline, so a runtime that loops
/// forever hung this target with nothing above it, and no process group, so a
/// forked grandchild would have outlived the run. Both are the class
/// `gorget::proc_guard` exists to retire; there is nothing special about this
/// file that earns an exemption.
fn run_or_panic(cmd: &mut Command, what: &str, timeout: Duration) -> std::process::Output {
    match run_with_deadline(cmd, timeout) {
        Ok(out) => out,
        Err(RunFailure::Deadline { secs }) => panic!("{what} timed out after {secs}s"),
        Err(RunFailure::Overflow { cap }) => {
            panic!("{what} produced runaway output (>{cap} bytes) — killed")
        }
    }
}

// ───────────────────────── Str runtime, called directly ─────────────────

/// Compile and run a C program that exercises the Str fat pointer runtime
/// functions directly, without going through the Gorget language pipeline.
#[test]
fn str_fat_ptr_runtime() {
    let manifest_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let tmp_dir = manifest_dir.join("target").join("str_runtime_test");
    fs::create_dir_all(&tmp_dir).unwrap();

    let c_path = tmp_dir.join("str_test.c");
    let exe_path = tmp_dir.join("str_test");

    // Build C source in the SAME unit order the real emitter uses
    // (`c_lir/emit_types.rs`: preamble → RUNTIME_STRING → … →
    // RUNTIME_STRING_BASE_OPS). `Str` and most of the functions under test are
    // defined in RUNTIME_STRING; `gorget_str_to_cstr` / `gorget_str_has_null`
    // live in RUNTIME_STRING_BASE_OPS. Assembling only the preamble compiles a
    // C file with no `Str` at all.
    let c_source = format!(
        r#"{RUNTIME_PREAMBLE}
{RUNTIME_STRING}
{RUNTIME_STRING_BASE_OPS}
{PANIC_NORMAL}
{TEST_MAIN}
"#,
        RUNTIME_PREAMBLE = RUNTIME_PREAMBLE,
        RUNTIME_STRING = RUNTIME_STRING,
        RUNTIME_STRING_BASE_OPS = RUNTIME_STRING_BASE_OPS,
        PANIC_NORMAL = PANIC_NORMAL,
        TEST_MAIN = TEST_MAIN,
    );

    fs::write(&c_path, &c_source).unwrap();

    // Compile
    let mut compile_cmd = Command::new("cc");
    compile_cmd
        .args([
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Wno-unused-function",
            // A real emitted program uses the preamble's stats counters
            // (`__gorget_map_clone_count` and friends); this synthetic
            // single-TU harness links the preamble without them, so the
            // same accommodation `-Wno-unused-function` already makes for
            // functions is owed to the variables.
            "-Wno-unused-variable",
            "-Werror",
            "-o",
        ])
        .arg(&exe_path)
        .arg(&c_path)
        .args(["-lm"]);
    let compile = run_or_panic(&mut compile_cmd, "cc", Duration::from_secs(300));

    if !compile.status.success() {
        let stderr = String::from_utf8_lossy(&compile.stderr);
        panic!("C compilation failed:\n{stderr}");
    }

    // Run
    let mut run_cmd = Command::new(&exe_path);
    let run = run_or_panic(&mut run_cmd, "str_test", Duration::from_secs(60));

    let stdout = String::from_utf8_lossy(&run.stdout);
    let stderr = String::from_utf8_lossy(&run.stderr);

    if !run.status.success() {
        panic!("str_test failed (exit {:?}):\nstdout:\n{stdout}\nstderr:\n{stderr}", run.status.code());
    }

    let expected = EXPECTED_OUTPUT.trim_end();
    let actual = stdout.trim_end();

    if actual != expected {
        // Show diff
        let expected_lines: Vec<&str> = expected.lines().collect();
        let actual_lines: Vec<&str> = actual.lines().collect();
        let max = expected_lines.len().max(actual_lines.len());
        let mut diff = String::new();
        for i in 0..max {
            let e = expected_lines.get(i).unwrap_or(&"<missing>");
            let a = actual_lines.get(i).unwrap_or(&"<missing>");
            if e != a {
                diff.push_str(&format!("line {}: expected '{}', got '{}'\n", i + 1, e, a));
            }
        }
        panic!(
            "Output mismatch:\n{diff}\n--- expected ---\n{expected}\n--- actual ---\n{actual}"
        );
    }

    // Clean up
    let _ = fs::remove_file(&c_path);
    let _ = fs::remove_file(&exe_path);
}

const TEST_MAIN: &str = r#"
int main(void) {
    // ── Construction ────────────────────────────────────────
    Str s1 = gorget_str_from_literal("hello", 5);
    printf("from_literal: data=%s len=%zu\n", s1.data, s1.len);

    Str s2 = gorget_str_from_cstr("hello");
    printf("from_cstr: data=%s len=%zu\n", s2.data, s2.len);

    // A NULL cstr yields the CANONICAL EMPTY Str (GORGET_EMPTY_STR), not a
    // null-data Str: every Str has a dereferenceable `.data`, so no consumer
    // needs a null check. Assert that invariant, not merely the len.
    Str s3 = gorget_str_from_cstr(NULL);
    printf("from_cstr_null: data=%s len=%zu\n",
           s3.data == NULL ? "NULL" : (s3.data[0] == '\0' ? "EMPTY" : "?"), s3.len);

    Str s4 = gorget_str_empty();
    printf("empty: data_is_empty=%d len=%zu\n", s4.data[0] == '\0', s4.len);

    // ── Field access ────────────────────────────────────────
    printf("byte_len: %zu\n", gorget_str_byte_len(s1));
    printf("is_empty_hello: %d\n", gorget_str_is_empty(s1));
    printf("is_empty_empty: %d\n", gorget_str_is_empty(s4));

    // ── Comparison ──────────────────────────────────────────
    Str sa = gorget_str_from_literal("abc", 3);
    Str sb = gorget_str_from_literal("abc", 3);
    Str sc = gorget_str_from_literal("abd", 3);
    Str sd = gorget_str_from_literal("ab", 2);

    printf("eq_same: %d\n", gorget_str_eq(sa, sb));
    printf("eq_diff: %d\n", gorget_str_eq(sa, sc));
    printf("eq_prefix: %d\n", gorget_str_eq(sa, sd));
    printf("eq_empty: %d\n", gorget_str_eq(s4, gorget_str_empty()));

    printf("cmp_equal: %d\n", gorget_str_cmp(sa, sb));
    printf("cmp_less: %d\n", gorget_str_cmp(sa, sc) < 0 ? 1 : 0);
    printf("cmp_greater: %d\n", gorget_str_cmp(sc, sa) > 0 ? 1 : 0);
    printf("cmp_shorter: %d\n", gorget_str_cmp(sd, sa) < 0 ? 1 : 0);
    printf("cmp_longer: %d\n", gorget_str_cmp(sa, sd) > 0 ? 1 : 0);

    // ── UTF-8 codepoint_len ─────────────────────────────────
    printf("cplen_ascii: %d\n", gorget_utf8_codepoint_len(0x41));       // 'A'
    printf("cplen_2byte: %d\n", gorget_utf8_codepoint_len(0xC3));       // é lead
    printf("cplen_3byte: %d\n", gorget_utf8_codepoint_len(0xE4));       // CJK lead
    printf("cplen_4byte: %d\n", gorget_utf8_codepoint_len(0xF0));       // emoji lead
    printf("cplen_invalid: %d\n", gorget_utf8_codepoint_len(0xFF));     // invalid

    // ── UTF-8 decode ────────────────────────────────────────
    {
        // ASCII: 'A' = 0x41
        const char* d = "A";
        size_t pos = 0;
        int64_t cp = gorget_utf8_decode(d, 1, &pos);
        printf("decode_ascii: cp=%lld pos=%zu\n", (long long)cp, pos);
    }
    {
        // café: c(1) a(1) f(1) é(2) = 5 bytes, 4 codepoints
        // é = U+00E9 = 0xC3 0xA9
        const char* d = "caf\xC3\xA9";
        size_t pos = 3;  // skip "caf", decode é
        int64_t cp = gorget_utf8_decode(d, 5, &pos);
        printf("decode_cafe: cp=%lld pos=%zu\n", (long long)cp, pos);
    }
    {
        // 🎉 = U+1F389 = 0xF0 0x9F 0x8E 0x89
        const char* d = "\xF0\x9F\x8E\x89";
        size_t pos = 0;
        int64_t cp = gorget_utf8_decode(d, 4, &pos);
        printf("decode_emoji: cp=%lld pos=%zu\n", (long long)cp, pos);
    }
    {
        // Invalid: lone continuation byte 0x80
        const char* d = "\x80";
        size_t pos = 0;
        int64_t cp = gorget_utf8_decode(d, 1, &pos);
        printf("decode_invalid: cp=%lld pos=%zu\n", (long long)cp, pos);
    }

    // ── UTF-8 validate ──────────────────────────────────────
    printf("validate_ascii: %d\n", gorget_utf8_validate("hello", 5));
    printf("validate_utf8: %d\n", gorget_utf8_validate("caf\xC3\xA9", 5));
    printf("validate_emoji: %d\n", gorget_utf8_validate("\xF0\x9F\x8E\x89", 4));
    printf("validate_invalid: %d\n", gorget_utf8_validate("\xFF\xFE", 2));
    printf("validate_trunc: %d\n", gorget_utf8_validate("\xC3", 1));  // truncated 2-byte
    printf("validate_empty: %d\n", gorget_utf8_validate("", 0));

    // ── Codepoint count ─────────────────────────────────────
    {
        Str ascii = gorget_str_from_literal("hello", 5);
        printf("cpcount_ascii: %lld\n", (long long)gorget_str_codepoint_count(ascii));
    }
    {
        // café = 5 bytes, 4 codepoints
        Str cafe = gorget_str_from_literal("caf\xC3\xA9", 5);
        printf("cpcount_cafe: %lld\n", (long long)gorget_str_codepoint_count(cafe));
    }
    {
        // 🎉 = 4 bytes, 1 codepoint
        Str emoji = gorget_str_from_literal("\xF0\x9F\x8E\x89", 4);
        printf("cpcount_emoji: %lld\n", (long long)gorget_str_codepoint_count(emoji));
    }
    {
        Str empty = gorget_str_empty();
        printf("cpcount_empty: %lld\n", (long long)gorget_str_codepoint_count(empty));
    }

    // ── Index ───────────────────────────────────────────────
    {
        // café: index 3 → é (bytes 0xC3 0xA9)
        Str cafe = gorget_str_from_literal("caf\xC3\xA9", 5);
        Str ch = gorget_str_index(cafe, 3);
        printf("index_cafe_3: len=%zu byte0=0x%02X byte1=0x%02X\n",
            ch.len, (unsigned char)ch.data[0], (unsigned char)ch.data[1]);
    }
    {
        // "hello" index 0 → 'h'
        Str hello = gorget_str_from_literal("hello", 5);
        Str ch = gorget_str_index(hello, 0);
        printf("index_hello_0: len=%zu char=%c\n", ch.len, ch.data[0]);
    }
    {
        // "hello" index -1 → 'o'
        Str hello = gorget_str_from_literal("hello", 5);
        Str ch = gorget_str_index(hello, -1);
        printf("index_hello_neg1: len=%zu char=%c\n", ch.len, ch.data[0]);
    }
    {
        // "hello" index -2 → 'l'
        Str hello = gorget_str_from_literal("hello", 5);
        Str ch = gorget_str_index(hello, -2);
        printf("index_hello_neg2: len=%zu char=%c\n", ch.len, ch.data[0]);
    }

    // ── Slice ───────────────────────────────────────────────
    {
        Str hello = gorget_str_from_literal("hello", 5);
        Str sl = gorget_str_slice(hello, 1, 4);
        // "ell"
        printf("slice_1_4: len=%zu data=%.3s\n", sl.len, sl.data);
    }
    {
        // café[1..3] → "af" (codepoints 1,2)
        Str cafe = gorget_str_from_literal("caf\xC3\xA9", 5);
        Str sl = gorget_str_slice(cafe, 1, 3);
        printf("slice_cafe_1_3: len=%zu data=%.2s\n", sl.len, sl.data);
    }
    {
        // Negative: "hello"[-3..-1] → "ll"
        Str hello = gorget_str_from_literal("hello", 5);
        Str sl = gorget_str_slice(hello, -3, -1);
        printf("slice_neg: len=%zu data=%.2s\n", sl.len, sl.data);
    }
    {
        // Full slice: "hello"[0..5] → "hello"
        Str hello = gorget_str_from_literal("hello", 5);
        Str sl = gorget_str_slice(hello, 0, 5);
        printf("slice_full: len=%zu data=%.5s\n", sl.len, sl.data);
    }
    {
        // Empty slice: "hello"[2..2] → ""
        Str hello = gorget_str_from_literal("hello", 5);
        Str sl = gorget_str_slice(hello, 2, 2);
        printf("slice_empty: len=%zu is_empty=%d\n", sl.len, gorget_str_is_empty(sl));
    }

    // ── Conversion ──────────────────────────────────────────
    {
        Str orig = gorget_str_from_literal("hello", 5);
        const char* c = gorget_str_to_cstr(orig);
        printf("to_cstr: %s\n", c);
        // Round-trip: cstr → Str
        Str back = gorget_str_from_cstr(c);
        printf("roundtrip: eq=%d\n", gorget_str_eq(orig, back));
        GORGET_FREE((void*)c, 6);
    }
    {
        Str clean = gorget_str_from_literal("hello", 5);
        printf("has_null_clean: %d\n", gorget_str_has_null(clean));

        // Str with embedded null
        Str with_null = gorget_str_from_literal("hel\0lo", 6);
        printf("has_null_embedded: %d\n", gorget_str_has_null(with_null));
    }

    // ── Edge cases ──────────────────────────────────────────
    {
        // Single byte
        Str one = gorget_str_from_literal("x", 1);
        printf("single: cpcount=%lld idx0=%c\n",
            (long long)gorget_str_codepoint_count(one), gorget_str_index(one, 0).data[0]);
    }
    {
        // All multi-byte: "é" (2 bytes) repeated
        Str multi = gorget_str_from_literal("\xC3\xA9\xC3\xA9\xC3\xA9", 6);
        printf("all_multi: bytes=%zu cps=%lld\n",
            gorget_str_byte_len(multi),
            (long long)gorget_str_codepoint_count(multi));
    }

    printf("ALL PASSED\n");
    return 0;
}
"#;

const EXPECTED_OUTPUT: &str = "\
from_literal: data=hello len=5
from_cstr: data=hello len=5
from_cstr_null: data=EMPTY len=0
empty: data_is_empty=1 len=0
byte_len: 5
is_empty_hello: 0
is_empty_empty: 1
eq_same: 1
eq_diff: 0
eq_prefix: 0
eq_empty: 1
cmp_equal: 0
cmp_less: 1
cmp_greater: 1
cmp_shorter: 1
cmp_longer: 1
cplen_ascii: 1
cplen_2byte: 2
cplen_3byte: 3
cplen_4byte: 4
cplen_invalid: 1
decode_ascii: cp=65 pos=1
decode_cafe: cp=233 pos=5
decode_emoji: cp=127881 pos=4
decode_invalid: cp=65533 pos=1
validate_ascii: 1
validate_utf8: 1
validate_emoji: 1
validate_invalid: 0
validate_trunc: 0
validate_empty: 1
cpcount_ascii: 5
cpcount_cafe: 4
cpcount_emoji: 1
cpcount_empty: 0
index_cafe_3: len=2 byte0=0xC3 byte1=0xA9
index_hello_0: len=1 char=h
index_hello_neg1: len=1 char=o
index_hello_neg2: len=1 char=l
slice_1_4: len=3 data=ell
slice_cafe_1_3: len=2 data=af
slice_neg: len=2 data=ll
slice_full: len=5 data=hello
slice_empty: len=0 is_empty=1
to_cstr: hello
roundtrip: eq=1
has_null_clean: 0
has_null_embedded: 1
single: cpcount=1 idx0=x
all_multi: bytes=6 cps=3
ALL PASSED
";


// ──────────────── No implicitly-declared runtime functions ──────────────
//
// Executable structural guard: the hand-written C runtime must compile with
// NO implicitly-declared functions.
//
// Background (the strptime class). glibc gates `strptime()` behind
// `__USE_XOPEN` (`_XOPEN_SOURCE` / `_GNU_SOURCE`), NOT `__USE_MISC`, so
// `_DEFAULT_SOURCE` alone leaves it *implicitly declared* on Linux. An implicit
// declaration is assumed to return `int`, which truncates `strptime`'s 64-bit
// `char*` result to 32 bits — silently corrupting `rest` inside
// `gorget_parse_time` and producing garbage time parsing. macOS declares
// `strptime` unconditionally, so the bug is x86_64-Linux-specific and invisible
// to a macOS-only test pass.
//
// What this guard is for NOW — the original rationale has been fixed at the
// writer site and no longer applies. It was written because the build paths
// compiled the runtime `.c` with a blanket `-w`, which suppresses *all*
// warnings including implicit-function-declaration (verified, gcc 12.2.0:
// `-w` defeats BOTH `-Werror=implicit-function-declaration` AND `#pragma GCC
// diagnostic error`, in either order). That `-w` is **gone from `src/`
// entirely**, and both C-compile sites now carry the flag themselves:
// `src/main.rs:1141` (C backend) and `:1443` (the LLVM path's separate
// runtime compile). So an ordinary `gg build` now catches this class —
// measured by disabling `_XOPEN_SOURCE` in the preamble, which reds the
// `time_format` integration tests, not just this guard.
//
// This guard therefore survives as DEFENSE IN DEPTH, and that is a narrower
// claim than it once made: it applies
// `-Werror=implicit-function-declaration` *itself*, independently of
// `main.rs`, so it still fires if either cc site ever drops the flag — the
// regression that would silently return the whole class to invisibility. If
// that argument ever stops being worth a test target, delete this file rather
// than letting the comment drift again.
//
// What this guard does (per the executable-guard style in
// `docs/devbook/25-structural-guards.md`): it emits the full self-contained C
// for a fixture that exercises the time runtime (`gorget_parse_time` →
// `strptime`) via `gg build <fixture> --emit-c-lir`, then recompiles that
// emitted C with `cc -Werror=implicit-function-declaration` and NO `-w`. If any
// hand-written runtime function is implicitly declared the recompile fails and
// so does this test. Prose rots; this guard doesn't.
//
// This is a *build-time* guard at the writer site (the runtime preamble's
// feature-test macros + the system-header gating), not an IR walk — the
// invariant it polices ("no runtime function is implicitly declared") is a
// property of the C compilation, so the guard lives at the C-compilation
// boundary.

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
