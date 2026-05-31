//! Byte-identity gate for the Chain-1 runtime extraction (PURE TEXT MOVE).
//!
//! Each `RUNTIME_*` / `*_RUNTIME` / `PANIC_*` / `TASK_COMMON` constant was moved
//! from a `r#"..."#` literal in `c_runtime.rs` into `src/backend/c/runtime/<name>.c`
//! and rewired to `include_str!`. `src/backend/c/runtime_snapshot/<name>.snap` holds
//! a byte-snapshot of each constant's ORIGINAL value (captured from the pre-extraction
//! compiled binary). This test proves the move was byte-neutral: the `include_str!`
//! value MUST equal the committed snapshot, zero diff.

use super::c_runtime::*;

macro_rules! assert_snap {
    ($konst:ident, $snap:literal) => {{
        let snap = include_str!(concat!("runtime_snapshot/", $snap, ".snap"));
        assert_eq!(
            $konst, snap,
            "byte-identity FAILED for {}: include_str! value differs from the pre-extraction snapshot",
            stringify!($konst),
        );
    }};
}

#[test]
fn runtime_constants_byte_identical_to_pre_extraction_snapshot() {
    assert_snap!(ASYNC_RUNTIME, "async_runtime");
    assert_snap!(AUDIO_RUNTIME, "audio_runtime");
    assert_snap!(BLOCKING_POOL_RUNTIME, "blocking_pool_runtime");
    assert_snap!(BYTES_F32_RUNTIME, "bytes_f32_runtime");
    assert_snap!(BYTES_RUNTIME, "bytes_runtime");
    assert_snap!(CHANNEL_RUNTIME, "channel_runtime");
    assert_snap!(COMPRESS_RUNTIME, "compress_runtime");
    assert_snap!(CRYPTO_RUNTIME, "crypto_runtime");
    assert_snap!(GL_RUNTIME, "gl_runtime");
    assert_snap!(HOT_RELOAD_RUNTIME, "hot_reload_runtime");
    assert_snap!(IMAGE_RUNTIME, "image_runtime");
    assert_snap!(MAIN_WAKER_RUNTIME, "main_waker_runtime");
    assert_snap!(METAL_RUNTIME, "metal_runtime");
    assert_snap!(MUTEX_RUNTIME, "mutex_runtime");
    assert_snap!(PANIC_NORMAL, "panic_normal");
    assert_snap!(PANIC_TEST, "panic_test");
    assert_snap!(PROCESS_RUNTIME, "process_runtime");
    assert_snap!(PROCESS_SPAWN_RUNTIME, "process_spawn_runtime");
    assert_snap!(REACTOR_RUNTIME, "reactor_runtime");
    assert_snap!(RUNTIME_ALLOC_REPORT, "runtime_alloc_report");
    assert_snap!(RUNTIME_ARENA_ALLOC, "runtime_arena_alloc");
    assert_snap!(RUNTIME_ARGS, "runtime_args");
    assert_snap!(RUNTIME_ARRAY, "runtime_array");
    assert_snap!(RUNTIME_CHECKED_ARITH, "runtime_checked_arith");
    assert_snap!(RUNTIME_CLONE_STATS, "runtime_clone_stats");
    assert_snap!(RUNTIME_ENV, "runtime_env");
    assert_snap!(RUNTIME_ERROR, "runtime_error");
    assert_snap!(RUNTIME_FALLBACK_ALLOC, "runtime_fallback_alloc");
    assert_snap!(RUNTIME_FILE, "runtime_file");
    assert_snap!(RUNTIME_FIXEDBUF_ALLOC, "runtime_fixedbuf_alloc");
    assert_snap!(RUNTIME_IO, "runtime_io");
    assert_snap!(RUNTIME_MAP, "runtime_map");
    assert_snap!(RUNTIME_MATH, "runtime_math");
    assert_snap!(RUNTIME_PARSE, "runtime_parse");
    assert_snap!(RUNTIME_PATH, "runtime_path");
    assert_snap!(RUNTIME_POOL_ALLOC, "runtime_pool_alloc");
    assert_snap!(RUNTIME_PREAMBLE, "runtime_preamble");
    assert_snap!(RUNTIME_SET, "runtime_set");
    assert_snap!(RUNTIME_SORT, "runtime_sort");
    assert_snap!(RUNTIME_STRING, "runtime_string");
    assert_snap!(RUNTIME_STRING_ARRAY, "runtime_string_array");
    assert_snap!(RUNTIME_STRING_BASE_OPS, "runtime_string_base_ops");
    assert_snap!(RUNTIME_STRING_EXTENDED, "runtime_string_extended");
    assert_snap!(RUNTIME_TLSF_ALLOC, "runtime_tlsf_alloc");
    assert_snap!(RUNTIME_TOSTR, "runtime_tostr");
    assert_snap!(RUNTIME_TRACKING_ALLOC, "runtime_tracking_alloc");
    assert_snap!(SCHEDULER_INLINE_RUNTIME, "scheduler_inline_runtime");
    assert_snap!(SCHEDULER_POOL_RUNTIME, "scheduler_pool_runtime");
    assert_snap!(SCHEDULER_SINGLE_RUNTIME, "scheduler_single_runtime");
    assert_snap!(SCHEDULER_THREAD_RUNTIME, "scheduler_thread_runtime");
    assert_snap!(SDL_RUNTIME, "sdl_runtime");
    assert_snap!(SERVER_SOCKET_RUNTIME, "server_socket_runtime");
    assert_snap!(SHARED_RUNTIME, "shared_runtime");
    assert_snap!(SOCKET_RUNTIME, "socket_runtime");
    assert_snap!(SYNC_RUNTIME, "sync_runtime");
    assert_snap!(TASK_COMMON, "task_common");
    assert_snap!(TASK_GROUP_RUNTIME, "task_group_runtime");
    assert_snap!(THREAD_RUNTIME, "thread_runtime");
    assert_snap!(TLS_SERVER_RUNTIME, "tls_server_runtime");
    assert_snap!(TLS_SOCKET_RUNTIME, "tls_socket_runtime");
    assert_snap!(TRACE_RUNTIME, "trace_runtime");
    assert_snap!(UDP_SOCKET_RUNTIME, "udp_socket_runtime");
}
