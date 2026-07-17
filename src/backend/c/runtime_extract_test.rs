//! Smoke check for the Chain-1 runtime extraction.
//!
//! Each `RUNTIME_*` / `*_RUNTIME` / `PANIC_*` / `TASK_COMMON` constant lives in
//! `src/backend/c/runtime/<name>.c` and is wired via `include_str!` in `c_runtime.rs`.
//! A missing/renamed `.c` is already an `include_str!` compile error; this test is
//! belt-and-suspenders against a `.c` being *truncated to empty* (which would compile
//! but silently drop runtime code). It asserts each extracted constant is non-empty
//! and spot-checks a couple of large ones for a sentinel substring.
//!
//! The earlier byte-identity gate against frozen `runtime_snapshot/*.snap` copies was
//! retired (the snapshots were byte-for-byte duplicates of the `.c` files, making the
//! assertion tautological — see `the r5 cleanup-snap brief (git history)`).

use super::c_runtime::*;

/// Every runtime constant extracted to a `runtime/*.c` file.
const EXTRACTED_RUNTIME_CONSTS: &[(&str, &str)] = &[
    ("ASYNC_RUNTIME", ASYNC_RUNTIME),
    ("AUDIO_RUNTIME", AUDIO_RUNTIME),
    ("BLOCKING_POOL_RUNTIME", BLOCKING_POOL_RUNTIME),
    ("BYTES_F32_RUNTIME", BYTES_F32_RUNTIME),
    ("BYTES_RUNTIME", BYTES_RUNTIME),
    ("CHANNEL_RUNTIME", CHANNEL_RUNTIME),
    ("COMPRESS_RUNTIME", COMPRESS_RUNTIME),
    ("CRYPTO_RUNTIME", CRYPTO_RUNTIME),
    ("GL_RUNTIME", GL_RUNTIME),
    ("HOT_RELOAD_RUNTIME", HOT_RELOAD_RUNTIME),
    ("IMAGE_RUNTIME", IMAGE_RUNTIME),
    ("MAIN_WAKER_RUNTIME", MAIN_WAKER_RUNTIME),
    ("METAL_RUNTIME", METAL_RUNTIME),
    ("MUTEX_RUNTIME", MUTEX_RUNTIME),
    ("PANIC_NORMAL", PANIC_NORMAL),
    ("PANIC_TEST", PANIC_TEST),
    ("PROCESS_RUNTIME", PROCESS_RUNTIME),
    ("PROCESS_SPAWN_RUNTIME", PROCESS_SPAWN_RUNTIME),
    ("REACTOR_RUNTIME", REACTOR_RUNTIME),
    ("RUNTIME_ALLOC_REPORT", RUNTIME_ALLOC_REPORT),
    ("RUNTIME_ARENA_ALLOC", RUNTIME_ARENA_ALLOC),
    ("RUNTIME_ARGS", RUNTIME_ARGS),
    ("RUNTIME_ARRAY", RUNTIME_ARRAY),
    ("RUNTIME_CHECKED_ARITH", RUNTIME_CHECKED_ARITH),
    ("RUNTIME_CLONE_STATS", RUNTIME_CLONE_STATS),
    ("RUNTIME_ENV", RUNTIME_ENV),
    ("RUNTIME_ERROR", RUNTIME_ERROR),
    ("RUNTIME_FALLBACK_ALLOC", RUNTIME_FALLBACK_ALLOC),
    ("RUNTIME_FILE", RUNTIME_FILE),
    ("RUNTIME_FIXEDBUF_ALLOC", RUNTIME_FIXEDBUF_ALLOC),
    ("RUNTIME_IO", RUNTIME_IO),
    ("RUNTIME_MAP", RUNTIME_MAP),
    ("RUNTIME_MATH", RUNTIME_MATH),
    ("RUNTIME_PARSE", RUNTIME_PARSE),
    ("RUNTIME_PATH", RUNTIME_PATH),
    ("RUNTIME_POOL_ALLOC", RUNTIME_POOL_ALLOC),
    ("RUNTIME_PREAMBLE", RUNTIME_PREAMBLE),
    ("RUNTIME_SET", RUNTIME_SET),
    ("RUNTIME_SORT", RUNTIME_SORT),
    ("RUNTIME_STRING", RUNTIME_STRING),
    ("RUNTIME_STRING_ARRAY", RUNTIME_STRING_ARRAY),
    ("RUNTIME_STRING_BASE_OPS", RUNTIME_STRING_BASE_OPS),
    ("RUNTIME_STRING_EXTENDED", RUNTIME_STRING_EXTENDED),
    ("RUNTIME_TLSF_ALLOC", RUNTIME_TLSF_ALLOC),
    ("RUNTIME_TOSTR", RUNTIME_TOSTR),
    ("RUNTIME_TRACKING_ALLOC", RUNTIME_TRACKING_ALLOC),
    ("SCHEDULER_INLINE_RUNTIME", SCHEDULER_INLINE_RUNTIME),
    ("SCHEDULER_POOL_RUNTIME", SCHEDULER_POOL_RUNTIME),
    ("SCHEDULER_SINGLE_RUNTIME", SCHEDULER_SINGLE_RUNTIME),
    ("SCHEDULER_THREAD_RUNTIME", SCHEDULER_THREAD_RUNTIME),
    ("SDL_RUNTIME", SDL_RUNTIME),
    ("SERVER_SOCKET_RUNTIME", SERVER_SOCKET_RUNTIME),
    ("SHARED_RUNTIME", SHARED_RUNTIME),
    ("SOCKET_RUNTIME", SOCKET_RUNTIME),
    ("SYNC_RUNTIME", SYNC_RUNTIME),
    ("TASK_COMMON", TASK_COMMON),
    ("TASK_GROUP_RUNTIME", TASK_GROUP_RUNTIME),
    ("THREAD_RUNTIME", THREAD_RUNTIME),
    ("TLS_SERVER_RUNTIME", TLS_SERVER_RUNTIME),
    ("TLS_SOCKET_RUNTIME", TLS_SOCKET_RUNTIME),
    ("TRACE_RUNTIME", TRACE_RUNTIME),
    ("UDP_SOCKET_RUNTIME", UDP_SOCKET_RUNTIME),
];

#[test]
fn all_extracted_runtime_constants_are_nonempty() {
    // Guards against a `runtime/*.c` file being truncated to empty (compiles fine,
    // but would silently drop runtime code).
    assert_eq!(
        EXTRACTED_RUNTIME_CONSTS.len(),
        62,
        "expected 62 extracted runtime constants",
    );
    for (name, src) in EXTRACTED_RUNTIME_CONSTS {
        assert!(
            !src.trim().is_empty(),
            "extracted runtime constant {name} is empty — its runtime/*.c file was likely truncated",
        );
    }
}

#[test]
fn key_runtime_constants_contain_expected_symbols() {
    // Spot-check a few large constants for a sentinel symbol, so a wrong/swapped
    // `.c` (right name, wrong contents) is caught beyond mere non-emptiness.
    assert!(
        RUNTIME_ARRAY.contains("gorget_array_"),
        "RUNTIME_ARRAY missing expected gorget_array_ symbols",
    );
    assert!(
        RUNTIME_MAP.contains("gorget_map_"),
        "RUNTIME_MAP missing expected gorget_map_ symbols",
    );
    assert!(
        RUNTIME_STRING.contains("gorget_str"),
        "RUNTIME_STRING missing expected gorget_str symbols",
    );
}
