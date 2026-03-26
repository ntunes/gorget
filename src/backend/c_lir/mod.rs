//! LIR → C backend.
//!
//! Thin 1:1 translation from LIR to C code. No semantic decisions —
//! all type coercions, drop calls, vtable dispatch, etc. are already
//! explicit in LIR instructions.

use crate::lir::*;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;

/// Names of structs provided by the Gorget C runtime — these should NOT
/// be re-defined by the LIR backend.
const RUNTIME_STRUCTS: &[&str] = &[
    "Str", "GorgetStringView", "GorgetString", "GorgetArray", "GorgetClosure",
    "TraitObj",
    "GorgetMap", "GorgetSet",
];

/// Structs defined by the LIR (not in the C runtime) that need their
/// original name preserved — not renamed to `__lir_sN`.
const LIR_NAMED_STRUCTS: &[&str] = &[
    "TaskHandle", "GorgetRange",
    "ExecResult", "GorgetCipherContext", "GorgetX25519KeyPair",
    "GorgetArena", "GorgetArenaCheckpoint",
    "GorgetPoolAllocator", "GorgetTlsfAllocator",
    "GorgetFixedBufferAllocator", "GorgetFallbackAllocator",
    "GorgetFile", "GorgetError",
];

/// Maps LIR struct names to their runtime C names when they differ.
fn lir_to_runtime_name(name: &str) -> Option<&'static str> {
    match name {
        "GorgetStringView" => Some("Str"),
        "ArenaCheckpoint" => Some("GorgetArenaCheckpoint"),
        "Socket" => Some("GorgetSocket"),
        "ServerSocket" => Some("GorgetServerSocket"),
        "TlsSocket" => Some("GorgetTlsSocket"),
        "TlsServerSocket" => Some("GorgetTlsServerSocket"),
        "UdpSocket" => Some("GorgetUdpSocket"),
        "UdpAddr" => Some("GorgetUdpAddr"),
        "UdpPacket" => Some("GorgetUdpPacket"),
        "Semaphore" => Some("GorgetSemaphore"),
        "WaitGroup" => Some("GorgetWaitGroup"),
        "OnceFlag" => Some("GorgetOnceFlag"),
        "CipherContext" => Some("GorgetCipherContext"),
        "BigNum" => Some("GorgetBigNum"),
        "RSAKey" => Some("GorgetRSAKey"),
        "Ed25519KeyPair" => Some("GorgetEd25519KeyPair"),
        "X25519KeyPair" => Some("GorgetX25519KeyPair"),
        "Regex" | "RegexMatch" | "Match" => {
            match name {
                "Regex" => Some("GorgetRegex"),
                "RegexMatch" | "Match" => Some("GorgetRegexMatch"),
                _ => None,
            }
        }
        "File" => Some("GorgetFile"),
        "Barrier" => Some("GorgetBarrier"),
        "CondVar" => Some("GorgetCondVar"),
        "AtomicInt" => Some("GorgetAtomicInt"),
        "AtomicBool" => Some("GorgetAtomicBool"),
        "Process" => Some("GorgetProcess"),
        // SDL types
        "SDLWindow" => Some("GorgetSDLWindow"),
        "SDLRenderer" => Some("GorgetSDLRenderer"),
        "SDLTexture" => Some("GorgetSDLTexture"),
        "SDLFont" => Some("GorgetSDLFont"),
        "SDLEvent" => Some("GorgetSDLEvent"),
        // Audio types
        "AudioChunk" => Some("GorgetAudioChunk"),
        "AudioMusic" => Some("GorgetAudioMusic"),
        // GL types — GorgetGLContext is typedef'd to int64_t in runtime
        "GLContext" => Some("GorgetGLContext"),
        _ => None,
    }
}

/// Build a mapping from StructId → C type name.
/// Runtime-provided structs use their real names; user structs use `__lir_s{id}`.
fn build_struct_names(module: &LirModule) -> HashMap<u32, String> {
    let mut map = HashMap::new();
    for (i, def) in module.structs.iter().enumerate() {
        if let Some(rt_name) = lir_to_runtime_name(&def.name) {
            map.insert(i as u32, rt_name.to_string());
        } else if RUNTIME_STRUCTS.contains(&def.name.as_str())
            || LIR_NAMED_STRUCTS.contains(&def.name.as_str()) {
            map.insert(i as u32, def.name.clone());
        } else if is_monomorphized_wrapper_type(&def.name) {
            // Channel__T, Shared__T, Weak__T, etc. — use their LIR name as C name.
            // The typedef will be emitted separately.
            map.insert(i as u32, def.name.clone());
        } else {
            map.insert(i as u32, format!("__lir_s{i}"));
        }
    }
    map
}

/// Returns true for monomorphized wrapper types that need typedefs to runtime types.
fn is_monomorphized_wrapper_type(name: &str) -> bool {
    name.starts_with("Channel__")
        || name.starts_with("Shared__")
        || name.starts_with("Weak__")
        || name.starts_with("Vector__")
        || name.starts_with("Dict__")
        || name.starts_with("Set__")
        || name.starts_with("HashMap__")
        || name.starts_with("HashSet__")
        || name.starts_with("Mutex__")
        || name.starts_with("RWLock__")
        || name.starts_with("Guard__")
        || name.starts_with("ReadGuard__")
        || name.starts_with("WriteGuard__")
        || name == "AtomicInt"
        || name == "AtomicBool"
        || name == "TaskGroup"
        || name.starts_with("Task__")
        || name.starts_with("Box__")
}

/// Generate C code from an LIR module.
pub fn generate_c(module: &LirModule) -> String {
    generate_c_inner(module, true)
}

/// Generate C code from an LIR module, optionally including the Gorget runtime.
pub fn generate_c_inner(module: &LirModule, include_runtime: bool) -> String {
    let struct_names = build_struct_names(module);
    let mut out = String::with_capacity(if include_runtime { 256 * 1024 } else { 4096 });

    if include_runtime {
        // Scan ALL call names (externs + function names + CallExtern inside bodies)
        // to determine which optional runtime modules are needed.
        let mut all_call_names: Vec<&str> = module.externs.iter().map(|e| e.name.as_str())
            .chain(module.functions.iter().map(|f| f.name.as_str()))
            .collect();
        for func in &module.functions {
            for block in &func.blocks {
                for inst in &block.insts {
                    if let Inst::CallExtern { name, .. } = inst {
                        all_call_names.push(name.as_str());
                    }
                }
            }
        }
        let has = |pred: &dyn Fn(&str) -> bool| all_call_names.iter().any(|n| pred(n));

        // Also check struct names for monomorphized types that need specific runtimes.
        let _has_struct = |name: &str| module.structs.iter().any(|s| s.name == name);

        // ── Minimal preamble (headers, allocator, scoped alloc stubs) ──
        out.push_str(crate::backend::c::c_runtime::RUNTIME_PREAMBLE);

        // ── Conditional allocators ──
        if has(&|n| n.starts_with("gorget_arena_") || n.starts_with("GorgetArena")) {
            out.push_str(crate::backend::c::c_runtime::RUNTIME_ARENA_ALLOC);
        }
        if has(&|n| n.starts_with("gorget_tracking_")) {
            out.push_str(crate::backend::c::c_runtime::RUNTIME_TRACKING_ALLOC);
        }
        if has(&|n| n.starts_with("gorget_pool_") || n.starts_with("GorgetPool")) {
            out.push_str(crate::backend::c::c_runtime::RUNTIME_POOL_ALLOC);
        }
        if has(&|n| n.starts_with("gorget_tlsf_")) {
            out.push_str(crate::backend::c::c_runtime::RUNTIME_TLSF_ALLOC);
        }
        if has(&|n| n.starts_with("gorget_fba_") || n.starts_with("gorget_fixed_buffer_")) {
            out.push_str(crate::backend::c::c_runtime::RUNTIME_FIXEDBUF_ALLOC);
        }
        if has(&|n| n.starts_with("gorget_fallback_")) {
            out.push_str(crate::backend::c::c_runtime::RUNTIME_FALLBACK_ALLOC);
        }

        // ── String types and operations ──
        out.push_str(crate::backend::c::c_runtime::RUNTIME_STRING);

        // Extended string methods (unicode tables, search, split/replace/trim/etc.)
        if has(&|n| n.starts_with("gorget_str_to_upper") || n.starts_with("gorget_str_to_lower")
            || n.starts_with("gorget_str_is_alpha") || n.starts_with("gorget_str_is_upper")
            || n.starts_with("gorget_str_is_lower") || n.starts_with("gorget_str_is_digit")
            || n.starts_with("gorget_str_is_whitespace")
            || n.starts_with("gorget_str_contains") || n.starts_with("gorget_str_starts_with")
            || n.starts_with("gorget_str_ends_with") || n.starts_with("gorget_str_find")
            || n.starts_with("gorget_memmem")
            || n.starts_with("gorget_str_trim") || n.starts_with("gorget_str_replace")
            || n.starts_with("gorget_str_repeat") || n.starts_with("gorget_str_pad")
            || n.starts_with("gorget_str_strip") || n.starts_with("gorget_str_lstrip")
            || n.starts_with("gorget_str_rstrip") || n.starts_with("gorget_str_removeprefix")
            || n.starts_with("gorget_str_removesuffix") || n.starts_with("gorget_str_index_of")
            || n.starts_with("gorget_str_count") || n.starts_with("gorget_str_center")
            || n.starts_with("gorget_str_ljust") || n.starts_with("gorget_str_rjust")
            || n.starts_with("gorget_str_zfill") || n.starts_with("gorget_str_reverse")
            || n.starts_with("gorget_str_encode_") || n.starts_with("gorget_str_decode_")
            || n.starts_with("gorget_base64_") || n.starts_with("gorget_json_escape")
            || n.starts_with("gorget_str_to_json") || n.starts_with("gorget_str_from_json")
            || n.starts_with("gorget_uint8_is_") || n.starts_with("gorget_uint8_to_")) {
            out.push_str(crate::backend::c::c_runtime::RUNTIME_STRING_EXTENDED);
        }

        // Base string operations (Str-aware concat, append, cstr conversion)
        out.push_str(crate::backend::c::c_runtime::RUNTIME_STRING_BASE_OPS);

        // ── Alloc report (test/bench mode only) ──
        let is_test_or_bench = !module.test_fns.is_empty() || !module.bench_fns.is_empty() || module.is_test_module;
        if is_test_or_bench {
            out.push_str(crate::backend::c::c_runtime::RUNTIME_ALLOC_REPORT);
        }

        // ── Panic handler ──
        if !is_test_or_bench {
            out.push_str(crate::backend::c::c_runtime::PANIC_NORMAL);
        } else {
            out.push_str(crate::backend::c::c_runtime::PANIC_TEST);
        }

        // ── Conditional core sections (formerly RUNTIME_CORE) ──
        // Use flags to track what's been emitted and enforce dependencies.
        let mut emitted_array = false;
        let mut emitted_map = false;

        // Helper macro to emit RUNTIME_ARRAY if not yet emitted
        macro_rules! ensure_array {
            ($out:expr, $flag:expr) => {
                if !$flag {
                    $out.push_str(crate::backend::c::c_runtime::RUNTIME_ARRAY);
                    $flag = true;
                }
            };
        }
        macro_rules! ensure_map {
            ($out:expr, $aflag:expr, $mflag:expr) => {
                ensure_array!($out, $aflag); // MAP depends on ARRAY
                if !$mflag {
                    $out.push_str(crate::backend::c::c_runtime::RUNTIME_MAP);
                    $mflag = true;
                }
            };
        }

        // Checked arithmetic (macros used by integer overflow checks)
        if has(&|n| n.starts_with("gorget_checked_") || n.starts_with("GORGET_CHECKED_")) {
            out.push_str(crate::backend::c::c_runtime::RUNTIME_CHECKED_ARITH);
        }

        // Collections: Array
        if has(&|n| n.starts_with("gorget_array_") || n.starts_with("Vector__")) {
            ensure_array!(out, emitted_array);
        }

        // String/Array operations (join, split, iterators — needs RUNTIME_ARRAY)
        if has(&|n| n.starts_with("gorget_str_join") || n.starts_with("gorget_str_split")
            || n.starts_with("gorget_str_bytes") || n.starts_with("gorget_str_codepoints")
            || n.starts_with("gorget_str_chars")) {
            ensure_array!(out, emitted_array);
            out.push_str(crate::backend::c::c_runtime::RUNTIME_STRING_ARRAY);
        }

        // Collections: Map (depends on Array for keys/values/items)
        if has(&|n| n.starts_with("gorget_map_") || n.starts_with("gorget_dict_")
            || n.starts_with("Dict__") || n.starts_with("HashMap__")) {
            ensure_map!(out, emitted_array, emitted_map);
        }

        // Collections: Set (depends on Map)
        if has(&|n| n.starts_with("gorget_set_") || n.starts_with("Set__") || n.starts_with("HashSet__")) {
            ensure_map!(out, emitted_array, emitted_map);
            out.push_str(crate::backend::c::c_runtime::RUNTIME_SET);
        }

        // Error handling (test/bench mode or explicit catch/throw)
        if is_test_or_bench || has(&|n| n.starts_with("gorget_catch") || n.starts_with("gorget_throw")
            || n.starts_with("gorget_cleanup_")) {
            out.push_str(crate::backend::c::c_runtime::RUNTIME_ERROR);
        }

        // File I/O (depends on Array for read_file_bytes)
        if has(&|n| n.starts_with("gorget_file_") || n == "gorget_read_file"
            || n == "gorget_write_file" || n == "gorget_append_file"
            || n == "gorget_read_file_bytes"
            || n == "File__open" || n == "File__create") {  // codegen rewrites to gorget_file_open
            ensure_array!(out, emitted_array);
            out.push_str(crate::backend::c::c_runtime::RUNTIME_FILE);
        }

        // Path functions + readdir (depends on Array for readdir)
        if has(&|n| n.starts_with("gorget_path_") || n == "gorget_is_file" || n == "gorget_is_dir"
            || n.starts_with("gorget_mkdir") || n.starts_with("gorget_readdir")
            || n == "gorget_rename" || n == "gorget_copy_file" || n == "gorget_remove"
            || n == "gorget_basename" || n == "gorget_dirname" || n == "gorget_file_size"
            || n == "gorget_file_mtime") {
            ensure_array!(out, emitted_array);
            out.push_str(crate::backend::c::c_runtime::RUNTIME_PATH);
        }

        // CLI args (gorget_args — needs RUNTIME_ARRAY; gorget_init_args is in preamble)
        if has(&|n| n == "gorget_args") {
            ensure_array!(out, emitted_array);
            out.push_str(crate::backend::c::c_runtime::RUNTIME_ARGS);
        }

        // Parsing (also detects int__parse/float__parse codegen patterns)
        if has(&|n| n.starts_with("gorget_parse_int") || n.starts_with("gorget_parse_float")
            || n.starts_with("gorget_try_parse")
            || (n.ends_with("__parse") && (n.starts_with("int") || n.starts_with("uint")
                || n == "double__parse" || n == "float__parse" || n == "bool__parse"))) {
            out.push_str(crate::backend::c::c_runtime::RUNTIME_PARSE);
        }

        // to_str conversions
        if has(&|n| n.starts_with("gorget_int_to_str") || n.starts_with("gorget_float_to_str")
            || n.starts_with("gorget_bool_to_str") || n.starts_with("gorget_codepoint_to_utf8")
            || n.starts_with("gorget_char_to_str") || n.starts_with("gorget_int_to_binary")
            || n.starts_with("gorget_int_to_hex") || n.starts_with("gorget_int_to_octal")
            || n.starts_with("gorget_int_to_float") || n.starts_with("gorget_float_to_int")
            || n == "gorget_char_chr") {
            out.push_str(crate::backend::c::c_runtime::RUNTIME_TOSTR);
        }

        // Environment
        if has(&|n| n == "gorget_getenv" || n == "gorget_setenv" || n == "gorget_getcwd"
            || n == "gorget_platform") {
            out.push_str(crate::backend::c::c_runtime::RUNTIME_ENV);
        }

        // Interactive I/O, time, datetime, random, line input (depends on Array for dt_decompose)
        if has(&|n| n.starts_with("gorget_input") || n.starts_with("gorget_rand")
            || n.starts_with("gorget_seed") || n.starts_with("gorget_sleep_ms")
            || n == "sleep" || n == "xtd_sleep" || n == "sleep_ms"  // codegen rewrites to gorget_sleep_ms
            || n.starts_with("gorget_time") || n.starts_with("gorget_format_time")
            || n.starts_with("gorget_parse_time") || n.starts_with("gorget_readline")
            || n.starts_with("gorget_dt_decompose") || n.starts_with("gorget_getchar")
            || n.starts_with("gorget_term_") || n == "gorget_is_tty") {
            ensure_array!(out, emitted_array);
            out.push_str(crate::backend::c::c_runtime::RUNTIME_IO);
        }

        // Math
        if has(&|n| n.starts_with("gorget_sqrt") || n.starts_with("gorget_pow")
            || n.starts_with("gorget_floor") || n.starts_with("gorget_ceil")
            || n.starts_with("gorget_round") || n.starts_with("gorget_abs")
            || n.starts_with("gorget_sin") || n.starts_with("gorget_cos")
            || n.starts_with("gorget_tan") || n.starts_with("gorget_log")
            || n.starts_with("gorget_exp") || n.starts_with("gorget_atan2")
            || n.starts_with("gorget_fmod") || n == "gorget_min" || n == "gorget_max"
            || n.starts_with("GORGET_PI") || n.starts_with("GORGET_E")
            || n.starts_with("GORGET_TAU") || n.starts_with("GORGET_INF")
            || n.starts_with("GORGET_NAN")) {
            out.push_str(crate::backend::c::c_runtime::RUNTIME_MATH);
        }

        // Sort comparators (depends on Array)
        if has(&|n| n.starts_with("__gorget_cmp_") || n.starts_with("gorget_array_sort")
            || n.starts_with("gorget_array_reverse") || n.starts_with("gorget_array_unique")) {
            ensure_array!(out, emitted_array);
            out.push_str(crate::backend::c::c_runtime::RUNTIME_SORT);
        }

        writeln!(out).unwrap();

        // Sync primitives (atomics, barriers, semaphores, etc.)
        let needs_sync = has(&|n| n.starts_with("gorget_atomic_int_") || n.starts_with("gorget_atomic_bool_")) || has(&|n| {
            n.starts_with("gorget_atomic_") || n.starts_with("gorget_barrier_")
            || n.starts_with("gorget_condvar_") || n.starts_with("gorget_rwlock_")
            || n.starts_with("gorget_waitgroup_") || n.starts_with("gorget_semaphore_")
            || n.starts_with("gorget_onceflag_")
            || n.starts_with("gorget_read_guard_") || n.starts_with("gorget_write_guard_")
            || n.starts_with("ReadGuard__") || n.starts_with("WriteGuard__")
        });
        if needs_sync {
            out.push_str(crate::backend::c::c_runtime::SYNC_RUNTIME);
        }

        // Async core
        let needs_async = has(&|n| {
            n.contains("channel") || n.contains("Channel")
            || n.starts_with("gorget_mutex_") || n.starts_with("gorget_guard_")
            || n.starts_with("gorget_executor_") || n == "gorget_spawn"
            || n.starts_with("__gorget_spawn_") || n.starts_with("__gorget_await_")
            || n.starts_with("gorget_task_group_") || n.starts_with("gorget_reactor_")
            || n.starts_with("Mutex__") || n.starts_with("RWLock__")
        });
        if needs_async {
            out.push_str(crate::backend::c::c_runtime::ASYNC_RUNTIME);
            out.push_str(crate::backend::c::c_runtime::TASK_COMMON);
            match module.scheduler_mode {
                crate::ir::SchedulerMode::Pool => out.push_str(crate::backend::c::c_runtime::SCHEDULER_POOL_RUNTIME),
                crate::ir::SchedulerMode::Thread => out.push_str(crate::backend::c::c_runtime::SCHEDULER_THREAD_RUNTIME),
                crate::ir::SchedulerMode::Inline => out.push_str(crate::backend::c::c_runtime::SCHEDULER_INLINE_RUNTIME),
                crate::ir::SchedulerMode::Single => out.push_str(crate::backend::c::c_runtime::SCHEDULER_SINGLE_RUNTIME),
            }
            out.push_str(crate::backend::c::c_runtime::MAIN_WAKER_RUNTIME);
            out.push_str(crate::backend::c::c_runtime::EXECUTOR_RUNTIME);
        }

        // Channels (also triggered by monomorphized Channel__T methods)
        if has(&|n| n.starts_with("gorget_channel_") || n.starts_with("Channel__")) {
            if !needs_async {
                out.push_str(crate::backend::c::c_runtime::ASYNC_RUNTIME);
            }
            out.push_str(crate::backend::c::c_runtime::CHANNEL_RUNTIME);
        }

        // Shared / Weak references (also triggered by monomorphized methods)
        if has(&|n| n.starts_with("gorget_shared_") || n.starts_with("gorget_weak_")
            || n.starts_with("Shared__") || n.starts_with("Weak__")) {
            out.push_str(crate::backend::c::c_runtime::SHARED_RUNTIME);
        }

        // Mutex / Guard (also triggered by Mutex__T monomorphized methods)
        if has(&|n| n.starts_with("gorget_mutex_") || n.starts_with("gorget_guard_")
            || n.starts_with("Mutex__") || n.starts_with("RWLock__")
            || n.starts_with("Guard__") || n.starts_with("ReadGuard__") || n.starts_with("WriteGuard__")
            || n.starts_with("gorget_rwlock_") || n.starts_with("gorget_read_guard_")
            || n.starts_with("gorget_write_guard_"))
        {
            if !needs_async {
                out.push_str(crate::backend::c::c_runtime::ASYNC_RUNTIME);
            }
            out.push_str(crate::backend::c::c_runtime::MUTEX_RUNTIME);
        }

        // Reactor (async I/O, sleep, timers)
        if has(&|n| n.starts_with("gorget_reactor_") || n.starts_with("gorget_sleep_async")) {
            out.push_str(crate::backend::c::c_runtime::REACTOR_RUNTIME);
        }

        // Blocking pool — also needed for spawned functions (blocking spawn approach)
        if has(&|n| n.starts_with("gorget_blocking_")) || !module.spawned_fns.is_empty() {
            out.push_str(crate::backend::c::c_runtime::BLOCKING_POOL_RUNTIME);
        }

        // Task groups
        if has(&|n| n.starts_with("gorget_task_group_")) {
            out.push_str(crate::backend::c::c_runtime::TASK_GROUP_RUNTIME);
        }

        // Bytes
        if has(&|n| n.starts_with("gorget_bytes_")) {
            out.push_str(crate::backend::c::c_runtime::BYTES_RUNTIME);
        }

        // Regex
        if has(&|n| n.starts_with("gorget_regex_") || n.starts_with("gorget_match_")) {
            out.push_str(crate::backend::c::c_runtime::REGEX_RUNTIME);
            // Convenience wrappers for pattern-based regex operations.
            out.push_str(r#"
static GorgetRegexMatch gorget_regex_find_pat(const char* pattern, const char* subject) {
    GorgetRegex _rx = gorget_regex_compile(pattern, NULL);
    if (!_rx.code) { GorgetRegexMatch _m; _m.start = -1; return _m; }
    GorgetRegexMatch _m = gorget_regex_find(&_rx, subject, 0);
    gorget_regex_free(&_rx);
    return _m;
}
static bool gorget_regex_is_match_pat(const char* pattern, const char* subject) {
    GorgetRegex _rx = gorget_regex_compile(pattern, NULL);
    if (!_rx.code) return false;
    bool _b = gorget_regex_is_match(&_rx, subject);
    gorget_regex_free(&_rx);
    return _b;
}
static GorgetString gorget_regex_replace_pat(const char* pattern, const char* subject, const char* replacement) {
    GorgetRegex _rx = gorget_regex_compile(pattern, NULL);
    if (!_rx.code) return gorget_string_new(subject);
    GorgetString _gs = gorget_regex_replace(&_rx, subject, replacement);
    gorget_regex_free(&_rx);
    return _gs;
}
static GorgetArray gorget_regex_split_pat(const char* pattern, const char* subject, int64_t limit) {
    GorgetRegex _rx = gorget_regex_compile(pattern, NULL);
    if (!_rx.code) { GorgetArray _a = gorget_array_new(sizeof(Str)); return _a; }
    GorgetArray _a = gorget_regex_split(&_rx, subject, limit);
    gorget_regex_free(&_rx);
    return _a;
}
"#);
        }

        // Crypto
        if has(&|n| n.starts_with("gorget_crypto_") || n.starts_with("gorget_sha") || n.starts_with("gorget_hmac") || n.starts_with("gorget_x25519") || n.starts_with("gorget_hkdf") || n.starts_with("gorget_aead")) {
            out.push_str(crate::backend::c::c_runtime::CRYPTO_RUNTIME);
        }

        // Socket (depends on Array for socket_read/read_exact)
        if has(&|n| n.starts_with("gorget_socket_") || n.starts_with("gorget_tcp_")) {
            ensure_array!(out, emitted_array);
            out.push_str(crate::backend::c::c_runtime::SOCKET_RUNTIME);
        }

        // Server socket (depends on Array)
        if has(&|n| n.starts_with("gorget_server_socket_") || n.starts_with("gorget_listener_")) {
            ensure_array!(out, emitted_array);
            out.push_str(crate::backend::c::c_runtime::SERVER_SOCKET_RUNTIME);
        }

        // UDP socket
        if has(&|n| n.starts_with("gorget_udp_")) {
            out.push_str(crate::backend::c::c_runtime::UDP_SOCKET_RUNTIME);
        }

        // TLS
        if has(&|n| n.starts_with("gorget_tls_")) {
            out.push_str(crate::backend::c::c_runtime::TLS_SOCKET_RUNTIME);
            out.push_str(crate::backend::c::c_runtime::TLS_SERVER_RUNTIME);
        }

        // Process
        if has(&|n| n.starts_with("gorget_process_") || n.starts_with("gorget_exec_") || n == "gorget_getenv" || n == "gorget_setenv") {
            out.push_str(crate::backend::c::c_runtime::PROCESS_RUNTIME);
        }

        // Process spawn (fork+exec with pipes) + signal handling (signal functions live in PROCESS_SPAWN_RUNTIME)
        if has(&|n| n.starts_with("gorget_process_spawn") || n.starts_with("gorget_process_wait")
            || n.starts_with("gorget_process_kill") || n.starts_with("gorget_process_pid")
            || n.starts_with("gorget_process_read_") || n.starts_with("gorget_process_write_")
            || n.starts_with("gorget_process_close_")
            || n.starts_with("gorget_signal_") || n == "gorget_getpid") {
            out.push_str(crate::backend::c::c_runtime::PROCESS_SPAWN_RUNTIME);
        }

        // Thread
        if has(&|n| n.starts_with("gorget_thread_") || n.starts_with("gorget_current_thread_id")
            || n.starts_with("__gorget_thread_spawn_")) || !module.thread_spawned_fns.is_empty() {
            out.push_str(crate::backend::c::c_runtime::THREAD_RUNTIME);
        }

        // Trace
        if module.trace_filename.is_some() || has(&|n| n.starts_with("gorget_trace_")) {
            out.push_str(crate::backend::c::c_runtime::TRACE_RUNTIME);
        }

        // SDL
        if has(&|n| n.starts_with("sdl_") || n.starts_with("gorget_sdl_")) {
            if has(&|n| n == "sdl_load_texture" || n == "gorget_sdl_load_texture") {
                out.push_str("#define GORGET_USE_SDL_IMAGE\n");
            }
            if has(&|n| n == "sdl_load_font" || n == "sdl_close_font" || n == "sdl_draw_text"
                || n == "sdl_render_text" || n == "sdl_text_width" || n == "sdl_text_height"
                || n.starts_with("gorget_sdl_load_font") || n.starts_with("gorget_sdl_draw_text")
                || n.starts_with("gorget_sdl_render_text")) {
                out.push_str("#define GORGET_USE_SDL_TTF\n");
            }
            out.push_str(crate::backend::c::c_runtime::SDL_RUNTIME);
        }

        // Bytes f32/f64/i64 helpers
        if has(&|n| n.starts_with("gorget_bytes_") && (n.contains("f32") || n.contains("f64") || n.contains("i64"))) {
            out.push_str(crate::backend::c::c_runtime::BYTES_F32_RUNTIME);
        }

        // OpenGL
        if has(&|n| n.starts_with("gorget_gl_")) {
            out.push_str(crate::backend::c::c_runtime::GL_RUNTIME);
        }

        // Image loading (stb_image)
        if has(&|n| n.starts_with("gorget_image_")) {
            out.push_str("\n#define STB_IMAGE_IMPLEMENTATION\n");
            out.push_str("#define STBI_NO_STDIO\n");
            out.push_str("#define STBI_ONLY_PNG\n");
            out.push_str("#define STBI_ONLY_JPEG\n");
            out.push_str("#define STBI_ONLY_TGA\n");
            out.push_str("#define STBI_ONLY_BMP\n");
            out.push_str("#define GORGET_HAS_STB_IMAGE 1\n");
            out.push_str("#pragma GCC diagnostic push\n");
            out.push_str("#pragma GCC diagnostic ignored \"-Wunused-function\"\n");
            out.push_str("#pragma GCC diagnostic ignored \"-Wunused-parameter\"\n");
            out.push_str("#pragma GCC diagnostic ignored \"-Wsign-compare\"\n");
            out.push_str("#pragma GCC diagnostic ignored \"-Wshift-negative-value\"\n");
            out.push_str(crate::backend::c::c_runtime::STB_IMAGE_SOURCE);
            out.push_str("\n#pragma GCC diagnostic pop\n");
            out.push_str(crate::backend::c::c_runtime::IMAGE_RUNTIME);
        }

        // Audio (SDL2_mixer)
        if has(&|n| n.starts_with("gorget_audio_")) {
            out.push_str(crate::backend::c::c_runtime::AUDIO_RUNTIME);
        }

        // Compression (zlib/deflate)
        if has(&|n| n.starts_with("gorget_zlib_") || n.starts_with("gorget_deflate_") || n.starts_with("gorget_crc32_")) {
            out.push_str(crate::backend::c::c_runtime::COMPRESS_RUNTIME);
        }

        // Metal (macOS Objective-C wrappers)
        if has(&|n| n.starts_with("gorget_metal_") || n.starts_with("gorget_sdl_metal_")) {
            out.push_str(crate::backend::c::c_runtime::METAL_RUNTIME);
        }

        // SQLite
        let needs_sqlite = has(&|n| n.starts_with("gorget_sqlite_") || n == "sqlite_open");
        if needs_sqlite {
            out.push_str("\n#define SQLITE_MAX_MMAP_SIZE 0\n");
            out.push_str("#define HAVE_MREMAP 0\n");
            out.push_str("#pragma GCC diagnostic push\n");
            out.push_str("#pragma GCC diagnostic ignored \"-Wunused-parameter\"\n");
            out.push_str("#pragma GCC diagnostic ignored \"-Wunused-variable\"\n");
            out.push_str("#pragma GCC diagnostic ignored \"-Wunused-function\"\n");
            out.push_str("#pragma GCC diagnostic ignored \"-Wimplicit-fallthrough\"\n");
            out.push_str("#pragma GCC diagnostic ignored \"-Wpedantic\"\n");
            out.push_str(crate::backend::c::c_runtime::SQLITE_AMALGAMATION);
            out.push_str("\n#pragma GCC diagnostic pop\n");
            out.push_str(crate::backend::c::c_runtime::SQLITE_GORGET_WRAPPERS);
        }

        // Hot-reload runtime (dlopen/file-watcher helpers)
        if module.hot_reload {
            out.push_str(crate::backend::c::c_runtime::HOT_RELOAD_RUNTIME);
        }

        // Suppress "value never read" warnings on idempotent emit-once flags.
        let _ = (emitted_array, emitted_map);

        // LIR-specific helper functions not emitted by the old C backend preamble.
        writeln!(out, "// ── LIR helpers ──").unwrap();
        if has(&|n| n == "gorget_char_chr") {
            writeln!(out, "static inline Str gorget_char_chr(int64_t code) {{ return gorget_str_from_cstr(gorget_codepoint_to_utf8(code)); }}").unwrap();
        }
        if has(&|n| n == "gorget_str_ord") {
            writeln!(out, "static inline int64_t gorget_str_ord(Str s) {{ size_t pos = 0; return (int64_t)gorget_utf8_decode(s.data, s.len, &pos); }}").unwrap();
        }
        // Default value functions for primitive types
        writeln!(out, "static inline Str gorget_str_default(void) {{ return (Str){{NULL, 0, 0, NULL}}; }}").unwrap();
        writeln!(out, "static inline int64_t int64_t__default(void) {{ return 0; }}").unwrap();
        writeln!(out, "static inline int64_t int__default(void) {{ return 0; }}").unwrap();
        writeln!(out, "static inline int8_t int8_t__default(void) {{ return 0; }}").unwrap();
        writeln!(out, "static inline int16_t int16_t__default(void) {{ return 0; }}").unwrap();
        writeln!(out, "static inline int32_t int32_t__default(void) {{ return 0; }}").unwrap();
        writeln!(out, "static inline uint8_t uint8_t__default(void) {{ return 0; }}").unwrap();
        writeln!(out, "static inline uint16_t uint16_t__default(void) {{ return 0; }}").unwrap();
        writeln!(out, "static inline uint32_t uint32_t__default(void) {{ return 0; }}").unwrap();
        writeln!(out, "static inline uint64_t uint64_t__default(void) {{ return 0; }}").unwrap();
        writeln!(out, "static inline double double__default(void) {{ return 0.0; }}").unwrap();
        writeln!(out, "static inline double float__default(void) {{ return 0.0; }}").unwrap();
        writeln!(out, "static inline bool bool__default(void) {{ return false; }}").unwrap();
        // Hash functions
        writeln!(out, "static inline int64_t __gorget_hash_int(int64_t v) {{ return (int64_t)__gorget_fnv1a(&v, sizeof(v)); }}").unwrap();
        writeln!(out, "static inline int64_t gorget_str_hash(Str s) {{ return (int64_t)__gorget_hash_str_len(s.data, s.len); }}").unwrap();
        // Signal functions — defined in the main runtime (c_runtime.rs).
        // Only emit minimal stubs when the runtime signal module is NOT included.
        writeln!(out, "#ifndef _WIN32").unwrap();
        writeln!(out, "#include <signal.h>").unwrap();
        writeln!(out, "#endif").unwrap();
        // Comparison functions for sorted()
        writeln!(out, "static int gorget_generic_compare(const void* a, const void* b) {{ return memcmp(a, b, sizeof(int64_t)); }}").unwrap();
        writeln!(out, "static int gorget_int_compare(const void* a, const void* b) {{ int64_t va = *(const int64_t*)a, vb = *(const int64_t*)b; return (va > vb) - (va < vb); }}").unwrap();
        writeln!(out, "static int gorget_float_compare(const void* a, const void* b) {{ double da = *(const double*)a, db = *(const double*)b; return (da > db) - (da < db); }}").unwrap();
        writeln!(out, "static int gorget_str_compare(const void* a, const void* b) {{ Str sa = *(const Str*)a, sb = *(const Str*)b; size_t ml = sa.len < sb.len ? sa.len : sb.len; int r = memcmp(sa.data, sb.data, ml); if (r) return r; return (sa.len > sb.len) - (sa.len < sb.len); }}").unwrap();
        writeln!(out, "static inline int64_t int64_t__one(void) {{ return 1; }}").unwrap();
        writeln!(out, "static inline int64_t int__one(void) {{ return 1; }}").unwrap();
        writeln!(out, "static inline double double__one(void) {{ return 1.0; }}").unwrap();
        writeln!(out, "static inline double float__one(void) {{ return 1.0; }}").unwrap();
        writeln!(out).unwrap();

    } else {
        // Minimal headers for standalone mode
        writeln!(out, "#include <stdint.h>").unwrap();
        writeln!(out, "#include <stdbool.h>").unwrap();
        writeln!(out, "#include <stdio.h>").unwrap();
        writeln!(out, "#include <string.h>").unwrap();
        writeln!(out, "#include <stdlib.h>").unwrap();
        writeln!(out).unwrap();
    }

    // Struct forward declarations (skip runtime-provided structs and monomorphized wrappers)
    // These structs are already defined in the C runtime, so emitting them again
    // would cause redefinition errors.
    let runtime_defined_named = &[
        "ExecResult", "GorgetCipherContext", "GorgetX25519KeyPair",
        "GorgetArena", "GorgetArenaCheckpoint",
        "GorgetPoolAllocator", "GorgetTlsfAllocator",
        "GorgetFixedBufferAllocator", "GorgetFallbackAllocator",
        "GorgetFile", "GorgetError",
        "GorgetSemaphore", "GorgetWaitGroup", "GorgetOnceFlag",
        "GorgetUdpAddr", "GorgetUdpPacket",
        "GorgetSocket", "GorgetServerSocket",
        "GorgetTlsSocket", "GorgetTlsServerSocket",
        "GorgetUdpSocket",
        "GorgetBigNum", "GorgetRSAKey", "GorgetEd25519KeyPair",
        "GorgetRegex", "GorgetRegexMatch",
    ];
    let skip_struct = |def: &StructDef| -> bool {
        RUNTIME_STRUCTS.contains(&def.name.as_str())
            || runtime_defined_named.contains(&def.name.as_str())
            || is_monomorphized_wrapper_type(&def.name)
            || lir_to_runtime_name(&def.name).is_some()
    };
    for (i, def) in module.structs.iter().enumerate() {
        if skip_struct(def) { continue; }
        let cname = &struct_names[&(i as u32)];
        writeln!(out, "typedef struct {cname} {cname};").unwrap();
    }
    // Early Task__* typedefs — these are referenced by Option__Task__*/Vector__Task__* field types
    // but their real typedef is emitted later in emit_spawn_helpers. Forward-declare them here.
    {
        let mut task_types_emitted = HashSet::new();
        for def in &module.structs {
            if def.name.starts_with("Task__") && task_types_emitted.insert(def.name.clone()) {
                writeln!(out, "typedef struct {{ void* __task; void (*__drop)(void*); }} {};", def.name).unwrap();
            }
        }
    }
    // Early Box__* typedefs — Box types appear in struct fields before their real typedef.
    // Non-trait boxes are void*, trait boxes are typedef'd to their TraitObj struct later.
    {
        let mut box_seen = HashSet::new();
        // Collect all Box__* types referenced in struct field types.
        for def in &module.structs {
            for (_, fty) in &def.fields {
                let ft = c_type_named(fty, &struct_names);
                if ft.starts_with("Box__") && box_seen.insert(ft.clone()) {
                    // Non-trait box: typedef as void*.
                    // Trait boxes will be re-typedef'd later by emit_monomorphized_typedefs.
                    writeln!(out, "typedef void* {ft};").unwrap();
                }
            }
        }
    }
    writeln!(out).unwrap();

    // Struct definitions — topologically sorted so inline struct fields are
    // defined before the structs that contain them.
    let struct_order = {
        let n = module.structs.len();
        // deps[i] = list of j where struct i depends on struct j (has Struct(j) field).
        let mut deps: Vec<Vec<usize>> = vec![Vec::new(); n];
        // dependents[j] = list of i where struct i depends on struct j.
        let mut dependents: Vec<Vec<usize>> = vec![Vec::new(); n];
        for (i, def) in module.structs.iter().enumerate() {
            for (_, fty) in &def.fields {
                if let LirType::Struct(sid) = fty {
                    let j = sid.0 as usize;
                    if j != i && j < n {
                        deps[i].push(j);
                        dependents[j].push(i);
                    }
                }
            }
        }
        // Kahn's algorithm: emit structs with no dependencies first.
        let mut in_degree: Vec<usize> = deps.iter().map(|d| d.len()).collect();
        let mut queue: std::collections::VecDeque<usize> = std::collections::VecDeque::new();
        for i in 0..n {
            if in_degree[i] == 0 {
                queue.push_back(i);
            }
        }
        let mut order = Vec::with_capacity(n);
        while let Some(k) = queue.pop_front() {
            order.push(k);
            for &i in &dependents[k] {
                in_degree[i] -= 1;
                if in_degree[i] == 0 {
                    queue.push_back(i);
                }
            }
        }
        // Any remaining nodes (cycles) — append in original order.
        if order.len() < n {
            let in_order: std::collections::HashSet<usize> = order.iter().copied().collect();
            for i in 0..n {
                if !in_order.contains(&i) {
                    order.push(i);
                }
            }
        }
        order
    };
    for &i in &struct_order {
        let def = &module.structs[i];
        if skip_struct(def) { continue; }
        let cname = &struct_names[&(i as u32)];
        let is_vtable = def.name.ends_with("_VTable");
        let is_traitobj = def.name.ends_with("_TraitObj");
        writeln!(out, "// {}", def.name).unwrap();
        writeln!(out, "struct {cname} {{").unwrap();
        if def.fields.is_empty() {
            // C doesn't allow empty structs — add a dummy byte.
            writeln!(out, "    char __pad;").unwrap();
        } else if def.is_enum && def.fields.len() > 1 {
            // Enum type: emit tag + union of variant structs.
            // Field 0 is always "tag" (I32). Fields 1+ are grouped by
            // variant prefix (e.g., IFunction_0, IFunction_1 → IFunction group).
            let (tag_name, tag_ty) = &def.fields[0];
            let tag_ty_str = c_type_named(tag_ty, &struct_names);
            writeln!(out, "    {} {};", tag_ty_str, c_field_name(tag_name)).unwrap();
            // Group remaining fields by variant name prefix
            let mut variants: Vec<(String, Vec<(&str, &LirType)>)> = Vec::new();
            for (fname, fty) in &def.fields[1..] {
                let variant_name = fname.rsplitn(2, '_').nth(1).unwrap_or(fname);
                if variants.last().map(|(n, _)| n.as_str()) == Some(variant_name) {
                    variants.last_mut().unwrap().1.push((fname.as_str(), fty));
                } else {
                    variants.push((variant_name.to_string(), vec![(fname.as_str(), fty)]));
                }
            }
            writeln!(out, "    union {{").unwrap();
            for (vname, fields) in &variants {
                if fields.len() == 1 {
                    let (fname, fty) = &fields[0];
                    let ty_str = if matches!(fty, LirType::Void) {
                        "uint8_t".to_string()
                    } else {
                        c_type_named(fty, &struct_names)
                    };
                    writeln!(out, "        {} {};  // {}", ty_str, c_field_name(fname), vname).unwrap();
                } else {
                    writeln!(out, "        struct {{  // {}", vname).unwrap();
                    for (fname, fty) in fields {
                        let ty_str = if matches!(fty, LirType::Void) {
                            "uint8_t".to_string()
                        } else {
                            c_type_named(fty, &struct_names)
                        };
                        writeln!(out, "            {} {};", ty_str, c_field_name(fname)).unwrap();
                    }
                    writeln!(out, "        }} {};", c_field_name(vname)).unwrap();
                }
            }
            writeln!(out, "    }} data;").unwrap();
        } else {
            for (fname, fty) in &def.fields {
                if is_vtable {
                    // VTable fields are function pointers.
                    // Look up the extern declaration for the Box__Trait__method to get full signature.
                    let trait_name = def.name.strip_suffix("_VTable").unwrap();
                    let ret_type = find_trait_method_return_type(module, trait_name, fname, &struct_names);
                    let box_method = format!("Box__{trait_name}__{fname}");
                    // Find impl function for better type info (extern may have Ptr where impl has Str).
                    let impl_fn_params: Option<&[LirType]> = module.functions.iter()
                        .find(|f| {
                            let prefix = format!("{trait_name}_for_");
                            let suffix = format!("__{fname}");
                            f.name.starts_with(&prefix) && f.name.ends_with(&suffix)
                        })
                        .map(|f| f.params.as_slice());
                    let extra_param_types: Vec<String> = module.externs.iter()
                        .find(|e| e.name == box_method)
                        .map(|e| e.params.iter().skip(1).enumerate() // skip self
                            .map(|(i, t)| {
                                let effective_ty = if matches!(t, LirType::Ptr) {
                                    impl_fn_params
                                        .and_then(|ps| ps.get(i + 1))
                                        .filter(|it| matches!(it, LirType::Struct(_)))
                                        .unwrap_or(t)
                                } else {
                                    t
                                };
                                c_type_named(effective_ty, &struct_names)
                            })
                            .collect())
                        .unwrap_or_default();
                    let params = if extra_param_types.is_empty() {
                        "const void*".to_string()
                    } else {
                        format!("const void*, {}", extra_param_types.join(", "))
                    };
                    writeln!(out, "    {ret_type} (*{})({});", c_field_name(fname), params).unwrap();
                } else if is_traitobj && fname == "vtable" {
                    // TraitObj vtable field should be a typed pointer to the VTable struct.
                    let trait_name = def.name.strip_suffix("_TraitObj").unwrap();
                    let vtable_cname = find_struct_cname_by_orig(module, &format!("{trait_name}_VTable"), &struct_names);
                    writeln!(out, "    const {vtable_cname}* {};", c_field_name(fname)).unwrap();
                } else {
                    // Void-typed fields are invalid in C — substitute uint8_t as a placeholder.
                    let ty_str = if matches!(fty, LirType::Void) {
                        "uint8_t".to_string()
                    } else {
                        c_type_named(fty, &struct_names)
                    };
                    writeln!(out, "    {} {};", ty_str, c_field_name(fname)).unwrap();
                }
            }
        }
        writeln!(out, "}};").unwrap();
        writeln!(out).unwrap();
    }

    // Emit monomorphized wrapper typedefs + inline wrappers AFTER struct definitions
    // so element types like __lir_s9 (Config) are already defined.
    if include_runtime {
        emit_monomorphized_typedefs(&mut out, module, &struct_names);
    }

    // Collect thread-generated names early for extern skip logic.
    let thread_generated_names: std::collections::HashSet<String> = {
        let mut s = std::collections::HashSet::new();
        for tsf in &module.thread_spawned_fns {
            let ret_c = &tsf.ret_c_type;
            s.insert(format!("Thread__{ret_c}__join"));
            s.insert(format!("Thread__{ret_c}__id"));
            s.insert(format!("__gorget_thread_spawn_{}", tsf.fn_name));
            s.insert(format!("__gorget_thread_entry_{}", tsf.fn_name));
        }
        s
    };

    // Extern declarations (skip functions already provided by included headers or runtime)
    for ext in &module.externs {
        if is_std_header_fn(&ext.name) || is_runtime_fn(&ext.name)
            || ext.name == "codepoint_to_str"
            || ext.name == "gorget_array_reversed"
            || ext.name == "gorget_array_unique"
            || ext.name == "gorget_array_zip" {
            continue;
        }
        // Skip thread-generated functions — they're emitted by emit_thread_helpers.
        if thread_generated_names.contains(&ext.name) {
            continue;
        }
        // Skip variadic externs with no named params — these are Gorget runtime
        // functions that lack proper type info in the LIR; declaring them as
        // `int32_t foo(...)` is invalid C.  They'll be resolved at link time
        // when the runtime is included.
        if ext.is_variadic && ext.params.is_empty() {
            continue;
        }
        // Skip higher-order collection methods — generated as static inline helpers.
        if parse_vector_higher_order(&ext.name).is_some()
            || parse_dict_higher_order(&ext.name).is_some()
            || parse_set_higher_order(&ext.name).is_some() {
            continue;
        }
        // Skip Option/Result combinator methods — generated as inline helpers.
        if parse_option_result_combinator(&ext.name).is_some() {
            continue;
        }
        // Skip monomorphized wrapper methods — inline wrappers emitted separately.
        if ext.name.starts_with("Channel__") || ext.name.starts_with("Shared__")
            || ext.name.starts_with("Weak__") || ext.name.starts_with("Mutex__")
            || ext.name.starts_with("RWLock__") || ext.name.starts_with("Guard__")
            || ext.name.starts_with("ReadGuard__") || ext.name.starts_with("WriteGuard__")
            || ext.name.starts_with("Box__") {
            continue;
        }
        write!(out, "{} {}(", c_type_named(&ext.return_type, &struct_names), ext.name).unwrap();
        if ext.params.is_empty() && !ext.is_variadic {
            write!(out, "void").unwrap();
        } else {
            for (i, p) in ext.params.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                // Void as a non-sole parameter is invalid C; emit void* instead
                // (typically a closure env pointer that has no captures).
                if matches!(p, LirType::Void) {
                    write!(out, "void*").unwrap();
                } else {
                    write!(out, "{}", c_type_named(p, &struct_names)).unwrap();
                }
            }
            if ext.is_variadic {
                if !ext.params.is_empty() {
                    write!(out, ", ").unwrap();
                }
                write!(out, "...").unwrap();
            }
        }
        writeln!(out, ");").unwrap();
    }
    if !module.externs.is_empty() {
        writeln!(out).unwrap();
    }

    // Generate __gorget_box_alloc_* helper functions.
    // These are monomorphized box allocators: malloc + store + return pointer.
    if include_runtime {
        let mut box_allocs: Vec<(&str, String)> = Vec::new();
        for ext in &module.externs {
            if ext.name.starts_with("__gorget_box_alloc_") && ext.params.len() == 1 {
                // Derive the C type from the function name suffix, not from the LIR param type,
                // because LIR represents Str as Ptr (void*) but the C box alloc needs the real type.
                let suffix = &ext.name["__gorget_box_alloc_".len()..];
                let param_ty = box_alloc_inner_c_type(suffix, &ext.params[0], &struct_names);
                box_allocs.push((&ext.name, param_ty));
            }
        }
        // Also scan CallExtern instructions for box allocs not in externs list.
        for func in &module.functions {
            for block in &func.blocks {
                for inst in &block.insts {
                    if let Inst::CallExtern { name, args, .. } = inst {
                        if name.starts_with("__gorget_box_alloc_") && args.len() == 1 {
                            if !box_allocs.iter().any(|(n, _)| *n == name.as_str()) {
                                let suffix = &name["__gorget_box_alloc_".len()..];
                                let param_ty = box_alloc_suffix_to_c_type(suffix);
                                box_allocs.push((name.as_str(), param_ty));
                            }
                        }
                    }
                }
            }
        }
        for (name, param_ty) in &box_allocs {
            writeln!(out, "static inline void* {name}({param_ty} val) {{ {param_ty}* p = ({param_ty}*)GORGET_ALLOC(sizeof({param_ty})); *p = val; return (void*)p; }}").unwrap();
        }
        if !box_allocs.is_empty() {
            writeln!(out).unwrap();
        }

        // Generate gorget_str_push/gorget_str_str/gorget_str_clear if called but not in runtime.
        let has_extern = |n: &str| module.externs.iter().any(|e| e.name == n)
            || module.functions.iter().flat_map(|f| f.blocks.iter())
                .flat_map(|b| b.insts.iter())
                .any(|inst| matches!(inst, Inst::CallExtern { name, .. } if name == n));
        if has_extern("gorget_str_push") {
            writeln!(out, "static inline void gorget_str_push(GorgetString* s, Str chunk) {{ gorget_string_push_char(s, chunk); }}").unwrap();
        }
        if has_extern("gorget_str_str") {
            writeln!(out, "static inline Str gorget_str_str(GorgetString* s) {{ return (Str){{ .data = s->data, .len = s->len, .cap = 0, .alloc = NULL }}; }}").unwrap();
        }
        if has_extern("gorget_str_clear") {
            writeln!(out, "static inline void gorget_str_clear(GorgetString* s) {{ s->len = 0; }}").unwrap();
        }
        if has_extern("gorget_str_push_line") {
            writeln!(out, "static inline void gorget_str_push_line(GorgetString* s, Str chunk) {{ gorget_string_push_char(s, chunk); gorget_string_push_byte(s, '\\n'); }}").unwrap();
        }
        if has_extern("gorget_str_capacity") {
            writeln!(out, "static inline int64_t gorget_str_capacity(GorgetString* s) {{ return (int64_t)s->cap; }}").unwrap();
        }
        if has_extern("gorget_str_push_char") {
            writeln!(out, "static inline void gorget_str_push_char(GorgetString* s, Str c) {{ gorget_string_push_char(s, c); }}").unwrap();
        }
        if has_extern("gorget_array_sort") {
            writeln!(out, "static size_t __gorget_sort_elem_size;").unwrap();
            writeln!(out, "static int __gorget_sort_cmp(const void* a, const void* b) {{ return memcmp(a, b, __gorget_sort_elem_size); }}").unwrap();
            writeln!(out, "static inline void gorget_array_sort(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; __gorget_sort_elem_size = a->elem_size; qsort(a->data, a->len, a->elem_size, __gorget_sort_cmp); }}").unwrap();
        }
        if has_extern("gorget_array_sorted") {
            writeln!(out, "static inline GorgetArray gorget_array_sorted(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; GorgetArray r = gorget_array_clone(a); qsort(r.data, r.len, r.elem_size, gorget_generic_compare); return r; }}").unwrap();
        }
        // gorget_array_reversed: clone + reverse (not in runtime, inlined by old backend)
        if has_extern("gorget_array_reversed") {
            writeln!(out, "static inline GorgetArray gorget_array_reversed(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; GorgetArray r = gorget_array_clone(a); gorget_array_reverse(&r); return r; }}").unwrap();
        }
        // gorget_array_unique: clone + sort + dedup (matches GIR backend semantics)
        if has_extern("gorget_array_unique") {
            writeln!(out, "static inline GorgetArray gorget_array_unique(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; GorgetArray r = gorget_array_clone(a); qsort(r.data, r.len, r.elem_size, gorget_generic_compare); gorget_array_dedup(&r); return r; }}").unwrap();
        }
        // gorget_array_zip: pair elements from two arrays into an array of tuples
        if has_extern("gorget_array_zip") {
            // Tuple struct: { _0: A, _1: B }.  We compute tuple_size from the two elem_sizes.
            // Both fields are at least 8-byte aligned in Gorget, so offset_1 = round_up(a_size, 8).
            writeln!(out, "static inline GorgetArray gorget_array_zip(void* __arr_ptr, GorgetArray __b) {{ \
                GorgetArray* __a = (GorgetArray*)__arr_ptr; \
                size_t __min = __a->len < __b.len ? __a->len : __b.len; \
                size_t __a_sz = __a->elem_size; \
                size_t __b_sz = __b.elem_size; \
                size_t __off1 = (__a_sz + 7) & ~(size_t)7; \
                size_t __tuple_sz = __off1 + ((__b_sz + 7) & ~(size_t)7); \
                GorgetArray __r = gorget_array_new(__tuple_sz); \
                for (size_t __i = 0; __i < __min; __i++) {{ \
                    char __buf[256]; memset(__buf, 0, __tuple_sz); \
                    memcpy(__buf, (char*)__a->data + __i * __a_sz, __a_sz); \
                    memcpy(__buf + __off1, (char*)__b.data + __i * __b_sz, __b_sz); \
                    gorget_array_push(&__r, __buf); \
                }} \
                return __r; }}").unwrap();
        }
        // codepoint_to_str: used by encoding/toml fixtures
        if has_extern("codepoint_to_str") {
            writeln!(out, "static inline Str codepoint_to_str(int64_t code) {{ return gorget_str_from_cstr(gorget_codepoint_to_utf8(code)); }}").unwrap();
        }

    }

    // Global declarations (split: plain globals first, then vtable globals after function forward decls)
    let has_func_addrs = |init: &LirGlobalInit| -> bool {
        fn check(init: &LirGlobalInit) -> bool {
            match init {
                LirGlobalInit::FuncAddr(_) => true,
                LirGlobalInit::Struct { fields, .. } => fields.iter().any(check),
                _ => false,
            }
        }
        check(init)
    };
    let mut deferred_globals: Vec<usize> = Vec::new();
    for (i, g) in module.globals.iter().enumerate() {
        if has_func_addrs(&g.init) {
            deferred_globals.push(i);
            continue;
        }
        let kw = if g.is_const { "const " } else { "" };
        write!(out, "{kw}{} __lir_g{i}", c_type_named(&g.ty, &struct_names)).unwrap();
        emit_global_init(&mut out, &g.init, &g.ty, &module.functions, &module.structs);
        writeln!(out, "; // {}", g.name).unwrap();
    }
    if !module.globals.is_empty() {
        writeln!(out).unwrap();
    }

    // Function forward declarations
    for func in &module.functions {
        if thread_generated_names.contains(&func.name) {
            continue;
        }
        // main() uses int main(int argc, char** argv) — must match the definition.
        if func.name == "main" {
            writeln!(out, "int main(int argc, char** argv);").unwrap();
            continue;
        }
        // For throws-int main, override Result return type to int.
        let ret_type_str = c_type_named(&func.return_type, &struct_names);
        write!(out, "{} {}(", ret_type_str, c_func_name(&func.name)).unwrap();
        if func.params.is_empty() {
            write!(out, "void").unwrap();
        } else {
            for (i, p) in func.params.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                let mut ty_str = if matches!(p, LirType::Void) { "void*".to_string() } else { c_type_named(p, &struct_names) };
                if func.const_params.get(i) == Some(&true) && matches!(p, LirType::Ptr) {
                    ty_str = format!("const {ty_str}");
                }
                write!(out, "{ty_str} __p{i}").unwrap();
            }
        }
        writeln!(out, ");").unwrap();
    }
    writeln!(out).unwrap();

    // Deferred globals (vtable constants with function pointers — must come after function forward decls)
    for &i in &deferred_globals {
        let g = &module.globals[i];
        let kw = if g.is_const { "const " } else { "" };
        write!(out, "{kw}{} __lir_g{i}", c_type_named(&g.ty, &struct_names)).unwrap();
        emit_global_init(&mut out, &g.init, &g.ty, &module.functions, &module.structs);
        writeln!(out, "; // {}", g.name).unwrap();
    }
    if !deferred_globals.is_empty() {
        writeln!(out).unwrap();
    }

    // Higher-order collection helpers (filter, map, fold, any, all, etc.)
    // Must come after function forward declarations so closure __call functions are visible.
    if include_runtime {
        emit_higher_order_collection_helpers(&mut out, module, &struct_names);
        emit_option_result_combinator_helpers(&mut out, module, &struct_names);
    }

    // Spawn/await helpers for async functions (blocking approach).
    if !module.spawned_fns.is_empty() && include_runtime {
        emit_spawn_helpers(&mut out, module);
    }

    // Thread spawn/join helpers.
    if !module.thread_spawned_fns.is_empty() && include_runtime {
        emit_thread_helpers(&mut out, module);
    }

    // Adapter functions for named functions passed as closures (FuncAddr → void* protocol).
    // When a named function is passed where a closure (void*) is expected, the call site
    // wraps it as (void*[2]){__adapt_fn, NULL}. The adapter ignores the env pointer and
    // forwards to the real function.
    {
        let mut adapter_fids: HashSet<u32> = HashSet::new();
        for func in &module.functions {
            for block in &func.blocks {
                for inst in &block.insts {
                    if let Inst::FuncAddr { func: fid, .. } = inst {
                        adapter_fids.insert(fid.0);
                    }
                }
            }
        }
        for fid_raw in &adapter_fids {
            let target = &module.functions[*fid_raw as usize];
            let ret_c = c_type_named(&target.return_type, &struct_names);
            let adapt_name = format!("__adapt_{}", c_func_name(&target.name));
            let target_name = c_func_name(&target.name);
            // Signature: ret_type __adapt_fn(void* __env, params...)
            write!(out, "{ret_c} {adapt_name}(void* __env").unwrap();
            for (i, p) in target.params.iter().enumerate() {
                let ty_str = if matches!(p, LirType::Void) { "void*".to_string() } else { c_type_named(p, &struct_names) };
                write!(out, ", {ty_str} __p{i}").unwrap();
            }
            write!(out, ") {{ ").unwrap();
            if !matches!(target.return_type, LirType::Void) {
                write!(out, "return ").unwrap();
            }
            write!(out, "{target_name}(").unwrap();
            for (i, _) in target.params.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write!(out, "__p{i}").unwrap();
            }
            writeln!(out, "); }}").unwrap();
        }
        if !adapter_fids.is_empty() {
            writeln!(out).unwrap();
        }
    }

    // Hot-reload: emit a typedef so the guest wrappers can use the original state type name.
    if module.hot_reload {
        if let Some(ref state_type) = module.hot_reload_state_type {
            // Find the LIR-mangled C name for this struct.
            if let Some((i, _)) = module.structs.iter().enumerate().find(|(_, s)| s.name == *state_type) {
                let c_name = struct_names.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"));
                if c_name != *state_type {
                    writeln!(out, "typedef {c_name} {state_type};").unwrap();
                }
            }
        }
    }

    // Forward-declare enum drop and clone functions (needed when enum A's
    // drop/clone calls B__drop/B__clone before B's definition).
    for (idx, sdef) in module.structs.iter().enumerate() {
        if module.recursive_drop_enums.contains_key(sdef.name.as_str()) {
            let c_name = struct_names.get(&(idx as u32)).cloned().unwrap_or_else(|| sdef.name.clone());
            let drop_fn = format!("{}__drop", sdef.name);
            if !module.functions.iter().any(|f| f.name == drop_fn) {
                writeln!(out, "void {drop_fn}(void*);").unwrap();
            }
            let clone_fn = format!("{}__clone", sdef.name);
            if !module.functions.iter().any(|f| f.name == clone_fn) {
                writeln!(out, "{c_name} {clone_fn}(void*);").unwrap();
            }
        }
    }
    // Emit struct drop functions for structs with Recursive drop strategy.
    // These are needed when a Recursive-drop struct appears as a field in
    // another struct — the parent's field drop calls {Name}__drop.
    emit_recursive_struct_drops(&mut out, module, &struct_names);
    emit_recursive_struct_clones(&mut out, module, &struct_names);
    emit_recursive_enum_clones(&mut out, module, &struct_names);
    emit_enum_drop_fns(&mut out, module, &struct_names);

    // Function definitions
    writeln!(out, "// ── Function Definitions ──").unwrap();
    let has_test_runner = !module.test_fns.is_empty() || !module.bench_fns.is_empty() || module.is_test_module;
    for func in &module.functions {
        if has_test_runner && func.name == "main" {
            continue;
        }
        emit_function(&mut out, func, module, &struct_names);
        writeln!(out).unwrap();
    }

    // Bench runner main — bench function bodies are lowered to LIR as __bench_N functions.
    if !module.bench_fns.is_empty() && module.functions.iter().any(|f| f.name.starts_with("__bench_")) {
        emit_bench_runner_main(&mut out, module);
    } else if !module.test_fns.is_empty() || module.is_test_module {
        emit_test_runner_main(&mut out, module);
    }

    out
}

/// Higher-order collection methods that the old C backend generates inline.
const HIGHER_ORDER_METHODS: &[&str] = &[
    "filter", "map", "flat_map", "fold", "reduce", "any", "all",
    "each", "find", "find_index", "sorted", "sort", "unique", "count",
];

/// Dict/Set methods needing inline codegen (no corresponding runtime function).
const DICT_INLINE_METHODS: &[&str] = &[
    "filter", "fold", "each", "any", "all", "map", "update", "get_or", "get_or_put",
];
const SET_INLINE_METHODS: &[&str] = &[
    "filter", "fold", "each", "any", "all", "map", "is_subset", "is_superset",
    "union", "intersection", "difference", "symmetric_difference",
];

/// Parse a monomorphized name like `Dict__Str__int64_t__filter` into
/// (key_c_type, val_c_type, method_name). Returns None if not a dict inline op.
fn parse_dict_higher_order(name: &str) -> Option<(&str, &str, &str)> {
    let rest = name.strip_prefix("Dict__")
        .or_else(|| name.strip_prefix("HashMap__"))?;
    let sep_pos = rest.rfind("__")?;
    let method = &rest[sep_pos + 2..];
    if !DICT_INLINE_METHODS.contains(&method) {
        return None;
    }
    // Remaining type part: "GorgetStringView__int64_t" → key="GorgetStringView", val="int64_t"
    let type_part = &rest[..sep_pos];
    // Find the FIRST `__` to split key from value type.
    let key_sep = type_part.find("__")?;
    let key = &type_part[..key_sep];
    let val = &type_part[key_sep + 2..];
    Some((key, val, method))
}

/// Parse a monomorphized name like `Set__int64_t__filter` into
/// (elem_c_type, method_name). Returns None if not a set inline op.
fn parse_set_higher_order(name: &str) -> Option<(&str, &str)> {
    let rest = name.strip_prefix("Set__")
        .or_else(|| name.strip_prefix("HashSet__"))?;
    let sep_pos = rest.rfind("__")?;
    let method = &rest[sep_pos + 2..];
    if !SET_INLINE_METHODS.contains(&method) {
        return None;
    }
    let elem = &rest[..sep_pos];
    Some((elem, method))
}

/// Parse a monomorphized name like `Vector__int64_t__filter` into
/// (element_c_type, method_name). Returns None if not a higher-order op.
fn parse_vector_higher_order(name: &str) -> Option<(&str, &str)> {
    // Pattern: Vector__<elem_type>__<method>
    let rest = name.strip_prefix("Vector__")?;
    // Find the LAST `__` separator — method name is after it.
    let sep_pos = rest.rfind("__")?;
    let method = &rest[sep_pos + 2..];
    if !HIGHER_ORDER_METHODS.contains(&method) {
        return None;
    }
    let elem = &rest[..sep_pos];
    Some((elem, method))
}

/// Collection helper descriptor — Vector, Dict, or Set.
enum CollHelper {
    /// (full_name, elem_c, method, closure_ty, call_fn)
    Vector(String, String, String, String, String),
    /// (full_name, key_c, val_c, method, closure_ty, call_fn)
    Dict(String, String, String, String, String, String),
    /// (full_name, elem_c, method, closure_ty, call_fn)
    Set(String, String, String, String, String),
}

/// Generate static inline C helper functions for higher-order collection operations.
/// Scans all CallExtern instructions for `Vector__T__method`, `Dict__K__V__method`,
/// and `Set__T__method` patterns and generates type-specific inline implementations.
fn emit_higher_order_collection_helpers(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    let mut helpers: Vec<CollHelper> = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();

    // Build orig name → C name map for resolving element types like Option__int64_t → __lir_s11
    let orig_to_c: HashMap<String, String> = module.structs.iter().enumerate()
        .map(|(i, def)| (def.name.clone(), sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"))))
        .collect();

    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::CallExtern { name, .. } = inst {
                    if !seen.insert(name.clone()) { continue; }
                    let ext = module.externs.iter().find(|e| e.name == *name);
                    let closure_c_type = ext.and_then(|e| e.params.last())
                        .map(|t| c_type_named(t, sn)).unwrap_or_else(|| "void*".into());
                    let call_fn_name = find_closure_call_fn(module, &closure_c_type, sn);

                    if let Some((elem_ty, method)) = parse_vector_higher_order(name) {
                        helpers.push(CollHelper::Vector(
                            name.clone(), elem_type_to_c_with_sn(elem_ty, &orig_to_c), method.to_string(),
                            closure_c_type, call_fn_name,
                        ));
                    } else if let Some((key_ty, val_ty, method)) = parse_dict_higher_order(name) {
                        helpers.push(CollHelper::Dict(
                            name.clone(), elem_type_to_c_with_sn(key_ty, &orig_to_c), elem_type_to_c_with_sn(val_ty, &orig_to_c),
                            method.to_string(), closure_c_type, call_fn_name,
                        ));
                    } else if let Some((elem_ty, method)) = parse_set_higher_order(name) {
                        helpers.push(CollHelper::Set(
                            name.clone(), elem_type_to_c_with_sn(elem_ty, &orig_to_c), method.to_string(),
                            closure_c_type, call_fn_name,
                        ));
                    } else {
                        // Not a collection higher-order op — undo insertion
                        seen.remove(name.as_str());
                    }
                }
            }
        }
    }

    if helpers.is_empty() {
        return;
    }

    writeln!(out, "/* ── Higher-order collection helpers ── */").unwrap();
    for helper in &helpers {
        match helper {
            CollHelper::Vector(full_name, elem_c, method, closure_ty, call_fn) => {
                emit_vector_helper(out, full_name, elem_c, method, closure_ty, call_fn);
            }
            CollHelper::Dict(full_name, key_c, val_c, method, closure_ty, call_fn) => {
                emit_dict_helper(out, full_name, key_c, val_c, method, closure_ty, call_fn);
            }
            CollHelper::Set(full_name, elem_c, method, closure_ty, call_fn) => {
                emit_set_helper(out, full_name, elem_c, method, closure_ty, call_fn);
            }
        }
        writeln!(out).unwrap();
    }
}

fn emit_vector_helper(out: &mut String, full_name: &str, elem_c: &str, method: &str, closure_ty: &str, call_fn: &str) {
    // Skip static helpers when we can't resolve the closure call function.
    // The inline expansion at each call site will handle it instead.
    if call_fn.contains("UNKNOWN_CLOSURE_CALL") {
        return;
    }
    match method {
        "filter" => {
            writeln!(out, "static inline GorgetArray {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    GorgetArray __result = gorget_array_new(sizeof({elem_c}));").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, __elem)) gorget_array_push(&__result, &__elem);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "map" => {
            writeln!(out, "static inline GorgetArray {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    __typeof__({call_fn}(&__fn, ({elem_c}){{0}})) __map_out;").unwrap();
            writeln!(out, "    GorgetArray __result = gorget_array_new(sizeof(__map_out));").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        __map_out = {call_fn}(&__fn, __elem);").unwrap();
            writeln!(out, "        gorget_array_push(&__result, &__map_out);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "fold" => {
            // Fold accumulator type may differ from element type. Use a macro (not inline function)
            // to let the caller's type propagate via __typeof__.
            // The inline expansion path handles all fold sites directly; this macro exists
            // only as a fallback definition to avoid linker errors on unreachable code paths.
            writeln!(out, "#define {full_name}(__arr_ptr, __acc_init, __fn) \
({{ GorgetArray __src = *(GorgetArray*)(__arr_ptr); \
__typeof__(__acc_init) __acc = (__acc_init); \
for (size_t __i = 0; __i < __src.len; __i++) {{ \
{elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i); \
__acc = {call_fn}(&(__fn), __acc, __elem); \
}} __acc; }})").unwrap();
        }
        "any" => {
            writeln!(out, "static inline bool {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, __elem)) return true;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return false;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "all" => {
            writeln!(out, "static inline bool {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        if (!{call_fn}(&__fn, __elem)) return false;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return true;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "each" => {
            writeln!(out, "static inline void {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        {call_fn}(&__fn, __elem);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "reduce" => {
            writeln!(out, "static inline {elem_c} {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    {elem_c} __acc = GORGET_ARRAY_AT({elem_c}, __src, 0);").unwrap();
            writeln!(out, "    for (size_t __i = 1; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        __acc = {call_fn}(&__fn, __acc, __elem);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __acc;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "sorted" => {
            // sorted() → clone + qsort with type-specific compare
            let cmp = compare_fn_for_elem(elem_c);
            writeln!(out, "static inline GorgetArray {full_name}(void* __arr_ptr) {{").unwrap();
            writeln!(out, "    GorgetArray __result = gorget_array_clone((GorgetArray*)__arr_ptr);").unwrap();
            writeln!(out, "    qsort(__result.data, __result.len, __result.elem_size, {cmp});").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "sort" => {
            // sort() → in-place qsort with type-specific compare
            let cmp = compare_fn_for_elem(elem_c);
            writeln!(out, "static inline void {full_name}(void* __arr_ptr) {{").unwrap();
            writeln!(out, "    GorgetArray* __a = (GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    qsort(__a->data, __a->len, __a->elem_size, {cmp});").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "unique" => {
            // unique() → clone + sort + dedup with type-specific compare
            let cmp = compare_fn_for_elem(elem_c);
            writeln!(out, "static inline GorgetArray {full_name}(void* __arr_ptr) {{").unwrap();
            writeln!(out, "    GorgetArray __result = gorget_array_clone((GorgetArray*)__arr_ptr);").unwrap();
            writeln!(out, "    qsort(__result.data, __result.len, __result.elem_size, {cmp});").unwrap();
            writeln!(out, "    gorget_array_dedup(&__result);").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "find" => {
            // find(pred) → Option[T]  (returns first matching element)
            // Use tag + payload offset that accounts for struct padding (tag is int32_t,
            // but payload may start at offset 8 due to alignment of int64_t/pointer).
            writeln!(out, "static inline void {full_name}(void* __arr_ptr, {closure_ty} __fn, void* __out) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    size_t __payload_off = (sizeof(int32_t) + (_Alignof({elem_c}) - 1)) & ~(_Alignof({elem_c}) - 1);").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, __elem)) {{ *(int32_t*)__out = 0; memcpy((char*)__out + __payload_off, &__elem, sizeof({elem_c})); return; }}").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    *(int32_t*)__out = 1;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "find_index" => {
            // find_index(pred) → int64_t (-1 if not found)
            writeln!(out, "static inline int64_t {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, __elem)) return (int64_t)__i;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return -1LL;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "flat_map" => {
            // flat_map(fn(T) → GorgetArray) → GorgetArray
            writeln!(out, "static inline GorgetArray {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    GorgetArray __result = gorget_array_new(sizeof({elem_c}));").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        GorgetArray __sub = {call_fn}(&__fn, __elem);").unwrap();
            writeln!(out, "        gorget_array_extend(&__result, &__sub);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "count" => {
            writeln!(out, "static inline int64_t {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    int64_t __count = 0;").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, __elem)) __count++;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __count;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        _ => {
            writeln!(out, "// TODO: {full_name} not yet implemented in c_lir").unwrap();
        }
    }
}

/// Emit inline C helpers for Dict higher-order and inline methods.
fn emit_dict_helper(out: &mut String, full_name: &str, key_c: &str, val_c: &str, method: &str, closure_ty: &str, call_fn: &str) {
    let iter_loop = format!(
        "for (size_t __i = 0; __i < __src.cap; __i++) {{ \
        if (__src.states[__i] != 1) continue;"
    );
    let key_read = format!("{key_c} __key = *({key_c}*)((char*)__src.keys + __i * __src.key_size);");
    let val_read = format!("{val_c} __val = *({val_c}*)((char*)__src.values + __i * __src.val_size);");
    // Dict uses ordered gorget_dict_new; if full_name starts with Dict__ or HashMap__ determines prefix
    let is_dict = full_name.starts_with("Dict__");
    let ctor_fn = if key_c == "Str" {
        if is_dict { "gorget_dict_new_str" } else { "gorget_map_new_str" }
    } else {
        if is_dict { "gorget_dict_new" } else { "gorget_map_new" }
    };
    let ctor_args = if key_c == "Str" { format!("sizeof({val_c})") } else { format!("sizeof({key_c}), sizeof({val_c})") };

    match method {
        "filter" => {
            // filter(closure(K, V) → bool) → GorgetMap
            writeln!(out, "static inline GorgetMap {full_name}(void* __map_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetMap __src = *(GorgetMap*)__map_ptr;").unwrap();
            writeln!(out, "    GorgetMap __result = {ctor_fn}({ctor_args});").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {key_read}").unwrap();
            writeln!(out, "        {val_read}").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, __key, __val)) gorget_map_put(&__result, &__key, &__val);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "fold" => {
            // fold(init, closure(acc, K, V) → acc) → acc_type
            // Use int64_t as default accumulator type (works for most cases; inline path handles others)
            writeln!(out, "static inline int64_t {full_name}(void* __map_ptr, int64_t __acc, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetMap __src = *(GorgetMap*)__map_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {key_read}").unwrap();
            writeln!(out, "        {val_read}").unwrap();
            writeln!(out, "        __acc = {call_fn}(&__fn, __acc, __key, __val);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __acc;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "each" => {
            writeln!(out, "static inline void {full_name}(void* __map_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetMap __src = *(GorgetMap*)__map_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {key_read}").unwrap();
            writeln!(out, "        {val_read}").unwrap();
            writeln!(out, "        {call_fn}(&__fn, __key, __val);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "any" => {
            writeln!(out, "static inline bool {full_name}(void* __map_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetMap __src = *(GorgetMap*)__map_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {key_read}").unwrap();
            writeln!(out, "        {val_read}").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, __key, __val)) return true;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return false;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "all" => {
            writeln!(out, "static inline bool {full_name}(void* __map_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetMap __src = *(GorgetMap*)__map_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {key_read}").unwrap();
            writeln!(out, "        {val_read}").unwrap();
            writeln!(out, "        if (!{call_fn}(&__fn, __key, __val)) return false;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return true;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "update" => {
            // update(other_map): merge other dict entries into self
            writeln!(out, "static inline void {full_name}(void* __map_ptr, GorgetMap __other) {{").unwrap();
            writeln!(out, "    GorgetMap* __dst = (GorgetMap*)__map_ptr;").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __other.cap; __i++) {{").unwrap();
            writeln!(out, "        if (__other.states[__i] != 1) continue;").unwrap();
            writeln!(out, "        void* __k = (char*)__other.keys + __i * __other.key_size;").unwrap();
            writeln!(out, "        void* __v = (char*)__other.values + __i * __other.val_size;").unwrap();
            writeln!(out, "        gorget_map_put(__dst, __k, __v);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "get_or" => {
            // get_or(key, default) → val_type
            writeln!(out, "static inline {val_c} {full_name}(void* __map_ptr, {key_c} __key, {val_c} __default) {{").unwrap();
            writeln!(out, "    {val_c}* __ptr = ({val_c}*)gorget_map_get((GorgetMap*)__map_ptr, &__key);").unwrap();
            writeln!(out, "    return __ptr ? *__ptr : __default;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "get_or_put" => {
            // get_or_put(key, default) → val_type — insert default if missing
            writeln!(out, "static inline {val_c} {full_name}(void* __map_ptr, {key_c} __key, {val_c} __default) {{").unwrap();
            writeln!(out, "    GorgetMap* __m = (GorgetMap*)__map_ptr;").unwrap();
            writeln!(out, "    {val_c}* __ptr = ({val_c}*)gorget_map_get(__m, &__key);").unwrap();
            writeln!(out, "    if (__ptr) return *__ptr;").unwrap();
            writeln!(out, "    gorget_map_put(__m, &__key, &__default);").unwrap();
            writeln!(out, "    return __default;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        _ => {
            writeln!(out, "// TODO: dict {full_name} not yet implemented in c_lir").unwrap();
        }
    }
}

/// Emit inline C helpers for Set higher-order and inline methods.
fn emit_set_helper(out: &mut String, full_name: &str, elem_c: &str, method: &str, closure_ty: &str, call_fn: &str) {
    // Set__ uses insertion order (order array), HashSet__ uses bucket order
    let is_ordered = !full_name.starts_with("HashSet__");
    let iter_loop = if is_ordered {
        format!(
            "for (size_t __j = 0; __j < __src.order_len; __j++) {{ \
            size_t __i = __src.order[__j]; \
            if (__src.states[__i] != 1) continue;"
        )
    } else {
        format!(
            "for (size_t __i = 0; __i < __src.cap; __i++) {{ \
            if (__src.states[__i] != 1) continue;"
        )
    };
    let ctor = if is_ordered { "gorget_ordered_set_new" } else { "gorget_set_new" };
    let elem_read = format!("{elem_c} __elem = *({elem_c}*)((char*)__src.keys + __i * __src.key_size);");

    match method {
        "filter" => {
            writeln!(out, "static inline GorgetSet {full_name}(void* __set_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    GorgetSet __result = {ctor}(sizeof({elem_c}));").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, __elem)) gorget_set_add(&__result, &__elem);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "fold" => {
            writeln!(out, "static inline int64_t {full_name}(void* __set_ptr, int64_t __acc, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        __acc = {call_fn}(&__fn, __acc, __elem);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __acc;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "each" => {
            writeln!(out, "static inline void {full_name}(void* __set_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        {call_fn}(&__fn, __elem);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "any" => {
            writeln!(out, "static inline bool {full_name}(void* __set_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, __elem)) return true;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return false;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "all" => {
            writeln!(out, "static inline bool {full_name}(void* __set_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        if (!{call_fn}(&__fn, __elem)) return false;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return true;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "is_subset" => {
            // is_subset(other): check every element in self is in other
            writeln!(out, "static inline bool {full_name}(void* __set_ptr, GorgetSet __other) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        if (!gorget_set_contains(&__other, &__elem)) return false;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return true;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "is_superset" => {
            // is_superset(other) = other.is_subset(self)
            writeln!(out, "static inline bool {full_name}(void* __set_ptr, GorgetSet __other) {{").unwrap();
            writeln!(out, "    GorgetSet __self = *(GorgetSet*)__set_ptr;").unwrap();
            if is_ordered {
                writeln!(out, "    for (size_t __j = 0; __j < __other.order_len; __j++) {{").unwrap();
                writeln!(out, "        size_t __i = __other.order[__j];").unwrap();
                writeln!(out, "        if (__other.states[__i] != 1) continue;").unwrap();
            } else {
                writeln!(out, "    for (size_t __i = 0; __i < __other.cap; __i++) {{").unwrap();
                writeln!(out, "        if (__other.states[__i] != 1) continue;").unwrap();
            }
            writeln!(out, "        {elem_c} __elem = *({elem_c}*)((char*)__other.keys + __i * __other.key_size);").unwrap();
            writeln!(out, "        if (!gorget_set_contains(&__self, &__elem)) return false;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return true;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "union" => {
            // union: combine all elements from self and other
            writeln!(out, "static inline GorgetSet {full_name}(void* __set_ptr, GorgetSet __other) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    GorgetSet __result = {ctor}(sizeof({elem_c}));").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        gorget_set_add(&__result, &__elem);").unwrap();
            writeln!(out, "    }}").unwrap();
            if is_ordered {
                writeln!(out, "    for (size_t __j2 = 0; __j2 < __other.order_len; __j2++) {{").unwrap();
                writeln!(out, "        size_t __i2 = __other.order[__j2];").unwrap();
                writeln!(out, "        if (__other.states[__i2] != 1) continue;").unwrap();
            } else {
                writeln!(out, "    for (size_t __i2 = 0; __i2 < __other.cap; __i2++) {{").unwrap();
                writeln!(out, "        if (__other.states[__i2] != 1) continue;").unwrap();
            }
            writeln!(out, "        {elem_c} __elem2 = *({elem_c}*)((char*)__other.keys + __i2 * __other.key_size);").unwrap();
            writeln!(out, "        gorget_set_add(&__result, &__elem2);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "intersection" => {
            // intersection: elements in both self and other
            writeln!(out, "static inline GorgetSet {full_name}(void* __set_ptr, GorgetSet __other) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    GorgetSet __result = {ctor}(sizeof({elem_c}));").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        if (gorget_set_contains(&__other, &__elem)) gorget_set_add(&__result, &__elem);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "difference" => {
            // difference: elements in self but not in other
            writeln!(out, "static inline GorgetSet {full_name}(void* __set_ptr, GorgetSet __other) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    GorgetSet __result = {ctor}(sizeof({elem_c}));").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        if (!gorget_set_contains(&__other, &__elem)) gorget_set_add(&__result, &__elem);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "symmetric_difference" => {
            // symmetric_difference: elements in self xor other
            writeln!(out, "static inline GorgetSet {full_name}(void* __set_ptr, GorgetSet __other) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    GorgetSet __result = {ctor}(sizeof({elem_c}));").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        if (!gorget_set_contains(&__other, &__elem)) gorget_set_add(&__result, &__elem);").unwrap();
            writeln!(out, "    }}").unwrap();
            if is_ordered {
                writeln!(out, "    for (size_t __j2 = 0; __j2 < __other.order_len; __j2++) {{").unwrap();
                writeln!(out, "        size_t __i2 = __other.order[__j2];").unwrap();
                writeln!(out, "        if (__other.states[__i2] != 1) continue;").unwrap();
            } else {
                writeln!(out, "    for (size_t __i2 = 0; __i2 < __other.cap; __i2++) {{").unwrap();
                writeln!(out, "        if (__other.states[__i2] != 1) continue;").unwrap();
            }
            writeln!(out, "        {elem_c} __elem2 = *({elem_c}*)((char*)__other.keys + __i2 * __other.key_size);").unwrap();
            writeln!(out, "        if (!gorget_set_contains(&__src, &__elem2)) gorget_set_add(&__result, &__elem2);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        _ => {
            writeln!(out, "// TODO: set {full_name} not yet implemented in c_lir").unwrap();
        }
    }
}

/// Find the __call function name for a closure struct type.
fn find_closure_call_fn(module: &LirModule, struct_c_name: &str, sn: &HashMap<u32, String>) -> String {
    // Map c_name back to struct def to get the original name (e.g., "__Closure_0").
    for (i, def) in module.structs.iter().enumerate() {
        let c_name = sn.get(&(i as u32)).map(|s| s.as_str()).unwrap_or(&def.name);
        if c_name == struct_c_name {
            // Look for a function named `<original_name>__call`
            let call_name = format!("{}__call", def.name);
            if module.functions.iter().any(|f| f.name == call_name) {
                return call_name;
            }
        }
    }
    // Fallback: try interpreting struct_c_name as the original name directly.
    let call_name = format!("{struct_c_name}__call");
    if module.functions.iter().any(|f| f.name == call_name) {
        return call_name;
    }
    // Last resort: return a placeholder
    format!("/* UNKNOWN_CLOSURE_CALL for {struct_c_name} */")
}

/// Look up the return type of a closure's `__call` function in LIR.
fn closure_call_return_type(module: &LirModule, call_fn_name: &str, sn: &HashMap<u32, String>) -> Option<String> {
    module.functions.iter()
        .find(|f| f.name == call_fn_name)
        .map(|f| c_type_named(&f.return_type, sn))
}

/// For a `map` combinator, determine the source enum type and the result enum type.
/// The source type comes from the function name (e.g. `Option__int64_t__map` → `Option__int64_t`).
/// The result type is an Option/Result wrapping the closure's return type.
/// If the closure returns the same element type, source == result.
/// If different (cross-type map), find the matching Option__<ret_type> struct.
fn map_combinator_types(
    name: &str, type_prefix: &str, call_fn: &str,
    module: &LirModule, sn: &HashMap<u32, String>,
) -> (String, String) {
    // Source type name = type_prefix (e.g., "Option__int64_t")
    let src_c = find_struct_c_name_by_prefix(type_prefix, module, sn)
        .unwrap_or_else(|| type_prefix.to_string());

    // Get the closure call function's return type.
    let closure_ret = closure_call_return_type(module, call_fn, sn);

    // If the closure returns the same type as the source payload, no cross-type.
    // If it returns a struct (Option/Result), the caller already does and_then, not map.
    // For map, the result wraps the closure return in the same Option/Result variant.
    if let Some(ref ret_ty) = closure_ret {
        // Extract what element type the source Option wraps.
        // E.g., "Option__int64_t" → payload is "int64_t"
        let src_payload = type_prefix.strip_prefix("Option__")
            .or_else(|| type_prefix.strip_prefix("Result__"));
        if let Some(payload) = src_payload {
            let payload_c = elem_type_to_c(payload);
            if *ret_ty != payload_c {
                // Cross-type: need Option__<ret_ty> or Result__<ret_ty> struct.
                let result_prefix = if name.starts_with("Option__") {
                    format!("Option__{}", type_name_to_monomorphized(ret_ty))
                } else {
                    // Result map keeps the error type; extract it from source struct
                    let err_part = module.structs.iter().find(|s| s.name == type_prefix)
                        .and_then(|s| s.fields.get(2))
                        .map(|(_, t)| c_type_named(t, sn));
                    if let Some(err_c) = err_part {
                        let err_m = type_name_to_monomorphized(&err_c);
                        format!("Result__{}__{err_m}", type_name_to_monomorphized(ret_ty))
                    } else {
                        format!("Result__{}", type_name_to_monomorphized(ret_ty))
                    }
                };
                let result_c = find_struct_c_name_by_prefix(&result_prefix, module, sn)
                    .unwrap_or(src_c.clone());
                return (src_c, result_c);
            }
        }
    }
    (src_c.clone(), src_c)
}

/// Compute source and result types for Result__T__E__map_err (closure transforms E → E2).
fn map_err_combinator_types(
    _name: &str, type_prefix: &str, call_fn: &str,
    module: &LirModule, sn: &HashMap<u32, String>,
) -> (String, String) {
    let src_c = find_struct_c_name_by_prefix(type_prefix, module, sn)
        .unwrap_or_else(|| type_prefix.to_string());
    let closure_ret = closure_call_return_type(module, call_fn, sn);
    if let Some(ref ret_ty) = closure_ret {
        // Result__T__E → Ok type is field[1], Error type is field[2].
        let ok_c = module.structs.iter().find(|s| s.name == type_prefix)
            .and_then(|s| s.fields.get(1))
            .map(|(_, t)| c_type_named(t, sn));
        if let Some(ok_c) = ok_c {
            let ok_m = type_name_to_monomorphized(&ok_c);
            let result_prefix = format!("Result__{ok_m}__{}", type_name_to_monomorphized(ret_ty));
            let result_c = find_struct_c_name_by_prefix(&result_prefix, module, sn)
                .unwrap_or(src_c.clone());
            return (src_c, result_c);
        }
    }
    (src_c.clone(), src_c)
}

/// Compute source and result types for and_then (closure returns the full Result/Option).
fn and_then_combinator_types(
    _name: &str, type_prefix: &str, call_fn: &str,
    module: &LirModule, sn: &HashMap<u32, String>,
) -> (String, String) {
    let src_c = find_struct_c_name_by_prefix(type_prefix, module, sn)
        .unwrap_or_else(|| type_prefix.to_string());
    let closure_ret = closure_call_return_type(module, call_fn, sn);
    if let Some(ref ret_ty) = closure_ret {
        // The closure returns the full wrapped type (e.g. Option__U, Result__U__E).
        let result_c = find_struct_c_name_by_prefix(ret_ty, module, sn)
            .unwrap_or(src_c.clone());
        return (src_c, result_c);
    }
    (src_c.clone(), src_c)
}

/// Find the C name for a struct whose original name matches a prefix.
fn find_struct_c_name_by_prefix(prefix: &str, module: &LirModule, sn: &HashMap<u32, String>) -> Option<String> {
    for (i, def) in module.structs.iter().enumerate() {
        if def.name == prefix {
            return Some(sn.get(&(i as u32)).cloned().unwrap_or_else(|| def.name.clone()));
        }
    }
    None
}

/// Map a C type name back to its monomorphized form for struct lookup.
fn type_name_to_monomorphized(c_type: &str) -> &str {
    // Struct names use mangled type names (e.g., "GorgetStringView"), but c_type_named()
    // returns C type names (e.g., "GorgetString"). Normalize to mangled form.
    match c_type {
        "GorgetString" => "GorgetStringView",
        "Str" => "GorgetStringView",
        _ => c_type,
    }
}

/// Convert a monomorphized element type name to its C type.
/// Option/Result combinator methods that the old C backend generates inline.
const OPTION_COMBINATORS: &[&str] = &[
    "map", "filter", "and_then", "or_else", "unwrap_or_else", "flat_map", "or", "flatten", "zip",
];
const RESULT_COMBINATORS: &[&str] = &[
    "map", "map_err", "and_then", "or_else", "unwrap_err", "unwrap_error",
];

/// Parse an Option/Result combinator name like `Option__int64_t__map` or
/// `Result__int64_t__Str__map` into (type_prefix, method).
/// Returns None if not a combinator.
fn parse_option_result_combinator(name: &str) -> Option<(&str, &str)> {
    if name.starts_with("Option__") {
        let rest = name.strip_prefix("Option__")?;
        let sep_pos = rest.rfind("__")?;
        let method = &rest[sep_pos + 2..];
        if OPTION_COMBINATORS.contains(&method) || RESULT_COMBINATORS.contains(&method) {
            return Some((&name[..name.len() - method.len() - 2], method));
        }
    }
    if name.starts_with("Result__") {
        let rest = name.strip_prefix("Result__")?;
        let sep_pos = rest.rfind("__")?;
        let method = &rest[sep_pos + 2..];
        if RESULT_COMBINATORS.contains(&method) || OPTION_COMBINATORS.contains(&method) {
            return Some((&name[..name.len() - method.len() - 2], method));
        }
    }
    None
}

/// For Option/Result structs, get the field names for the payload arms.
/// Returns (ok_field, err_field) — for Option: ("Some_0", "None_0"), for Result: ("Ok_0", "Error_0").
/// Falls back to ("Some_0", "None_0") if not found.
fn enum_payload_fields(type_prefix: &str, module: &LirModule) -> (String, String) {
    // Look up the struct definition by matching the type_prefix to a struct name
    for def in &module.structs {
        if def.name == type_prefix {
            // tag is field 0; payload field is field 1 (ok/some); error field is field 2 if present
            let ok_f = def.fields.get(1)
                .map(|(n, _)| c_field_name(n))
                .unwrap_or_else(|| "Some_0".to_string());
            let err_f = def.fields.get(2)
                .map(|(n, _)| c_field_name(n))
                .unwrap_or_else(|| "None_0".to_string());
            return (ok_f, err_f);
        }
    }
    ("Some_0".to_string(), "None_0".to_string())
}

/// Generate static inline C helpers for Option/Result combinator methods.
fn emit_option_result_combinator_helpers(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    // (full_name, src_c_type, result_c_type, method, closure_c_type, call_fn, ok_field, err_field)
    let mut helpers: Vec<(String, String, String, String, String, String, String, String)> = Vec::new();

    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::CallExtern { name, .. } = inst {
                    if let Some((type_prefix, method)) = parse_option_result_combinator(name) {
                        if !seen.insert(name.clone()) {
                            continue;
                        }
                        let ext = module.externs.iter().find(|e| e.name == *name);
                        let closure_c_type = ext.and_then(|e| e.params.get(1))
                            .map(|t| c_type_named(t, sn))
                            .unwrap_or_else(|| "void*".into());
                        let closure_struct_name = closure_c_type.clone();
                        let call_fn = find_closure_call_fn(module, &closure_struct_name, sn);

                        let (ok_field, err_field) = enum_payload_fields(type_prefix, module);

                        // For map/map_err/and_then, source and result types may differ.
                        let (src_c, result_c) = if method == "map" {
                            map_combinator_types(name, type_prefix, &call_fn, module, sn)
                        } else if method == "map_err" {
                            map_err_combinator_types(name, type_prefix, &call_fn, module, sn)
                        } else if method == "and_then" {
                            and_then_combinator_types(name, type_prefix, &call_fn, module, sn)
                        } else if method == "flatten" {
                            // flatten: source is Option[Option[T]], result is Option[T]
                            let src = find_struct_c_name_by_prefix(type_prefix, module, sn)
                                .unwrap_or_else(|| type_prefix.to_string());
                            let inner = module.structs.iter().find(|s| s.name == type_prefix)
                                .and_then(|s| s.fields.get(1))
                                .map(|(_, t)| c_type_named(t, sn))
                                .unwrap_or_else(|| src.clone());
                            (src, inner)
                        } else {
                            // Non-map combinators: source == result.
                            let c = find_struct_c_name_by_prefix(type_prefix, module, sn)
                                .unwrap_or_else(|| type_prefix.to_string());
                            (c.clone(), c)
                        };

                        helpers.push((name.clone(), src_c, result_c, method.to_string(), closure_c_type, call_fn, ok_field, err_field));
                    }
                }
            }
        }
    }

    if helpers.is_empty() {
        return;
    }

    writeln!(out, "/* ── Option/Result combinator helpers ── */").unwrap();
    for (full_name, src_c, result_c, method, closure_ty, call_fn, ok_field, err_field) in &helpers {
        match method.as_str() {
            "map" => {
                // map: if tag==0 (Some/Ok): apply closure to payload, wrap; else propagate
                // For map on Result, we need the result type's ok field too
                let result_ok = if full_name.starts_with("Result__") {
                    let result_prefix = full_name.rsplitn(2, "__").nth(1).unwrap_or(full_name);
                    let (rok, _) = enum_payload_fields(result_prefix, module);
                    rok
                } else {
                    ok_field.clone()
                };
                writeln!(out, "static inline {result_c} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    {result_c} __result;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{").unwrap();
                writeln!(out, "        __result.tag = 0;").unwrap();
                writeln!(out, "        __result.{result_ok} = {call_fn}(&__fn, __src.{ok_field});").unwrap();
                writeln!(out, "    }} else {{").unwrap();
                writeln!(out, "        __result.tag = 1;").unwrap();
                writeln!(out, "    }}").unwrap();
                writeln!(out, "    return __result;").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "filter" => {
                writeln!(out, "static inline {src_c} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0 && {call_fn}(&__fn, __src.{ok_field})) {{").unwrap();
                writeln!(out, "        return __src;").unwrap();
                writeln!(out, "    }}").unwrap();
                writeln!(out, "    return ({src_c}){{ .tag = 1 }};").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "and_then" => {
                writeln!(out, "static inline {result_c} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{").unwrap();
                writeln!(out, "        return {call_fn}(&__fn, __src.{ok_field});").unwrap();
                writeln!(out, "    }}").unwrap();
                writeln!(out, "    return ({result_c}){{ .tag = 1 }};").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "or_else" => {
                writeln!(out, "static inline {src_c} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{").unwrap();
                writeln!(out, "        return __src;").unwrap();
                writeln!(out, "    }}").unwrap();
                // Result or_else passes the error value; Option or_else takes no args
                if full_name.starts_with("Result__") {
                    writeln!(out, "    return {call_fn}(&__fn, __src.{err_field});").unwrap();
                } else {
                    writeln!(out, "    return {call_fn}(&__fn);").unwrap();
                }
                writeln!(out, "}}").unwrap();
            }
            "unwrap_err" | "unwrap_error" => {
                // Look up the actual error type from the struct
                let err_ty_c = module.structs.iter().find(|s| {
                    let c = sn.get(&(module.structs.iter().position(|x| std::ptr::eq(x, *s)).unwrap() as u32))
                        .cloned().unwrap_or_else(|| s.name.clone());
                    c == *src_c
                }).and_then(|s| s.fields.get(2))
                    .map(|(_, t)| c_type_named(t, sn))
                    .unwrap_or_else(|| "void*".to_string());
                writeln!(out, "static inline {err_ty_c} {full_name}(void* __res_ptr) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__res_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 1) {{").unwrap();
                writeln!(out, "        return __src.{err_field};").unwrap();
                writeln!(out, "    }}").unwrap();
                writeln!(out, "    fprintf(stderr, \"unwrap_err on Ok\\n\"); abort();").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "map_err" => {
                // Result__T__E__map_err(result*, closure) → Result__T__E2
                // if Ok: copy Ok field; if Error: apply closure to error payload, wrap in Error
                let result_err = if *result_c != *src_c {
                    // Cross-type: look up the error field name in the result struct
                    let (_, rerr) = enum_payload_fields(
                        module.structs.iter().find(|s| {
                            let cn = sn.get(&(module.structs.iter().position(|x| std::ptr::eq(x, *s)).unwrap() as u32))
                                .cloned().unwrap_or_else(|| s.name.clone());
                            cn == *result_c
                        }).map(|s| s.name.as_str()).unwrap_or(""),
                        module,
                    );
                    rerr
                } else {
                    err_field.clone()
                };
                writeln!(out, "static inline {result_c} {full_name}(void* __res_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__res_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{").unwrap();
                // Cross-type: copy the Ok value into the result struct
                if *result_c != *src_c {
                    let result_ok = {
                        let (rok, _) = enum_payload_fields(
                            module.structs.iter().find(|s| {
                                let cn = sn.get(&(module.structs.iter().position(|x| std::ptr::eq(x, *s)).unwrap() as u32))
                                    .cloned().unwrap_or_else(|| s.name.clone());
                                cn == *result_c
                            }).map(|s| s.name.as_str()).unwrap_or(""),
                            module,
                        );
                        rok
                    };
                    writeln!(out, "        {result_c} __ok_result; __ok_result.tag = 0; __ok_result.{result_ok} = __src.{ok_field};").unwrap();
                    writeln!(out, "        return __ok_result;").unwrap();
                } else {
                    writeln!(out, "        return __src;").unwrap();
                }
                writeln!(out, "    }}").unwrap();
                writeln!(out, "    {result_c} __result;").unwrap();
                writeln!(out, "    __result.tag = 1;").unwrap();
                // Use memcpy to handle Str/GorgetString layout-compatible type mismatches
                writeln!(out, "    {{ __auto_type __me_val = {call_fn}(&__fn, __src.{err_field}); memcpy(&__result.{result_err}, &__me_val, sizeof(__me_val)); }}").unwrap();
                writeln!(out, "    return __result;").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "or" => {
                // Option__T__or(opt*, other) → Option__T
                // if Some: return self; else return other
                writeln!(out, "static inline {src_c} {full_name}(void* __opt_ptr, {src_c} __other) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{ return __src; }}").unwrap();
                writeln!(out, "    return __other;").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "flatten" => {
                // Option__Option__T__flatten(opt*) → Option__T
                // if outer is Some and inner is Some: return inner; else None
                // result_c is the inner Option type
                writeln!(out, "static inline {result_c} {full_name}(void* __opt_ptr) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{ return __src.{ok_field}; }}").unwrap();
                writeln!(out, "    return ({result_c}){{ .tag = 1 }};").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "unwrap_or_else" => {
                // unwrap_or_else: if tag==0 (Some/Ok): return payload; else call closure
                // For Option: closure takes no args. For Result: closure takes error value.
                let payload_ty = module.structs.iter().find(|s| {
                    let cn = sn.get(&(module.structs.iter().position(|x| std::ptr::eq(x, *s)).unwrap() as u32))
                        .cloned().unwrap_or_else(|| s.name.clone());
                    cn == *src_c
                }).and_then(|s| s.fields.get(1))
                    .map(|(_, t)| c_type_named(t, sn))
                    .unwrap_or_else(|| "int64_t".to_string());
                writeln!(out, "static inline {payload_ty} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{ return __src.{ok_field}; }}").unwrap();
                if full_name.starts_with("Result__") {
                    writeln!(out, "    return {call_fn}(&__fn, __src.{err_field});").unwrap();
                } else {
                    writeln!(out, "    return {call_fn}(&__fn);").unwrap();
                }
                writeln!(out, "}}").unwrap();
            }
            "flat_map" => {
                // flat_map on Option: if Some, call closure (returns Option); else None
                writeln!(out, "static inline {result_c} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{ return {call_fn}(&__fn, __src.{ok_field}); }}").unwrap();
                writeln!(out, "    return ({result_c}){{ .tag = 1 }};").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "zip" => {
                // Option__T__zip(opt*, other) → Option__Tuple (not commonly used in tests, but cover it)
                writeln!(out, "// TODO: {full_name} (zip) not yet implemented").unwrap();
            }
            _ => {
                writeln!(out, "// TODO: {full_name} not yet implemented").unwrap();
            }
        }
        writeln!(out).unwrap();
    }
}

fn elem_type_to_c(elem: &str) -> String {
    elem_type_to_c_with_sn(elem, &HashMap::new())
}

fn elem_type_to_c_with_sn(elem: &str, orig_to_c: &HashMap<String, String>) -> String {
    match elem {
        "int64_t" | "int32_t" | "int16_t" | "int8_t" => elem.to_string(),
        "uint64_t" | "uint32_t" | "uint16_t" | "uint8_t" => elem.to_string(),
        "bool" => "bool".to_string(),
        "float" | "double" => elem.to_string(),
        "Str" | "GorgetStringView" => "Str".to_string(),
        _ => {
            // Try to resolve through struct name map.
            if let Some(cname) = orig_to_c.get(elem) {
                return cname.clone();
            }
            // Could be a user struct — use the name as-is.
            elem.to_string()
        }
    }
}

/// Generate blocking spawn/await helpers for each spawned function.
///
/// For each spawned function `foo`, generates:
/// - `Task__<RetType>` typedef (if not already emitted)
/// - `__SpawnCtx_foo` struct (GorgetTask base + params + result)
/// - `__spawn_run_foo()` — worker thread entry, calls the real function
/// - `__spawn_drop_foo()` — RAII cleanup (wait + free)
/// - `__gorget_spawn_foo()` — allocate ctx, init sync, submit to executor
/// - `__gorget_await_foo()` — wait, extract result, free
/// - `Task__<RetType>__drop()` — dispatch to per-fn drop via __drop pointer
fn emit_spawn_helpers(out: &mut String, module: &LirModule) {
    writeln!(out, "/* ── Spawn/await helpers (M:N executor pool) ── */").unwrap();

    // Build orig→C name map for resolving spawn param types.
    let sn = build_struct_names(module);
    let orig_to_c: HashMap<String, String> = module.structs.iter().enumerate()
        .map(|(i, def)| (def.name.clone(), sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"))))
        .collect();
    let resolve_type = |t: &str| -> String {
        orig_to_c.get(t).cloned().unwrap_or_else(|| t.to_string())
    };

    // Emit Task__T typedefs for return types not already emitted by the early Task typedef pass.
    let mut emitted_task_types: Vec<String> = Vec::new();
    // Collect already-emitted Task types from module structs (early pass).
    for def in &module.structs {
        if def.name.starts_with("Task__") {
            emitted_task_types.push(def.name.clone());
        }
    }
    for sf in &module.spawned_fns {
        let task_name = if sf.ret_c_type == "void" {
            "Task__void".to_string()
        } else {
            format!("Task__{}", sf.ret_c_type)
        };
        if !emitted_task_types.contains(&task_name) {
            writeln!(out, "typedef struct {{ void* __task; void (*__drop)(void*); }} {task_name};").unwrap();
            emitted_task_types.push(task_name);
        }
    }
    writeln!(out).unwrap();

    for sf in &module.spawned_fns {
        let fn_name = &sf.fn_name;
        let safe_fn_name = c_func_name(fn_name);
        let ret_c = &sf.ret_c_type;
        let is_void = ret_c == "void";
        let ctx_name = format!("__SpawnCtx_{fn_name}");

        // Context struct
        writeln!(out, "typedef struct {ctx_name} {{").unwrap();
        writeln!(out, "    GorgetTask base;").unwrap();
        for (param_name, param_c_type) in &sf.params {
            let resolved = resolve_type(param_c_type);
            writeln!(out, "    {resolved} __{param_name};").unwrap();
        }
        if !is_void {
            let resolved_ret = resolve_type(ret_c);
            writeln!(out, "    {resolved_ret} result;").unwrap();
        }
        writeln!(out, "}} {ctx_name};").unwrap();

        // Run function — called by worker thread
        writeln!(out, "static void __spawn_run_{fn_name}(GorgetTask* __base) {{").unwrap();
        writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)__base;").unwrap();
        let call_args: Vec<String> = sf.params.iter().enumerate().map(|(i, (name, c_type))| {
            if sf.ref_param_indices.contains(&i) {
                format!("&__ctx->__{name}")
            } else if matches!(c_type.as_str(), "GorgetArray" | "GorgetMap" | "GorgetSet") {
                // Collection params are void* in the LIR function signature but stored
                // as the actual struct in the spawn context.  Pass the address.
                format!("(void*)&__ctx->__{name}")
            } else {
                format!("__ctx->__{name}")
            }
        }).collect();
        let call_str = call_args.join(", ");
        if is_void {
            writeln!(out, "    {safe_fn_name}({call_str});").unwrap();
        } else {
            writeln!(out, "    __ctx->result = {safe_fn_name}({call_str});").unwrap();
        }
        writeln!(out, "}}").unwrap();

        // Drop helper
        writeln!(out, "static void __spawn_drop_{fn_name}(void* __ptr) {{").unwrap();
        writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)__ptr;").unwrap();
        writeln!(out, "    GORGET_SCHEDULER_WAIT(&__ctx->base);").unwrap();
        writeln!(out, "    pthread_mutex_destroy(&__ctx->base.mtx);").unwrap();
        writeln!(out, "    pthread_cond_destroy(&__ctx->base.cond);").unwrap();
        writeln!(out, "    GORGET_FREE(__ctx, sizeof({ctx_name}));").unwrap();
        writeln!(out, "}}").unwrap();

        // Spawn function — returns Task__T (matches GIR behavior).
        // When the LIR destination is a Task struct, the caller uses the struct directly.
        // When the LIR destination is void* (non-vector case), the call site wraps it.
        let task_type_name = if is_void { "Task__void".to_string() } else { format!("Task__{ret_c}") };
        let param_decls: Vec<String> = sf.params.iter().map(|(name, c_type)| {
            let resolved = resolve_type(c_type);
            format!("{resolved} {name}")
        }).collect();
        let param_decl_str = param_decls.join(", ");
        writeln!(out, "static inline {task_type_name} __gorget_spawn_{fn_name}({param_decl_str}) {{").unwrap();
        writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)GORGET_CALLOC(1, sizeof({ctx_name}));").unwrap();
        writeln!(out, "    __ctx->base.run = __spawn_run_{fn_name};").unwrap();
        writeln!(out, "    pthread_mutex_init(&__ctx->base.mtx, NULL);").unwrap();
        writeln!(out, "    pthread_cond_init(&__ctx->base.cond, NULL);").unwrap();
        for (i, (param_name, _c_type)) in sf.params.iter().enumerate() {
            // Clone refcounted params (Channel, Shared, Weak) to avoid dangling pointers.
            if let Some((_, gir_name)) = sf.clone_params.iter().find(|(idx, _)| *idx == i) {
                writeln!(out, "    __ctx->__{param_name} = {gir_name}__clone({param_name});").unwrap();
            } else {
                writeln!(out, "    __ctx->__{param_name} = {param_name};").unwrap();
            }
        }
        writeln!(out, "    GORGET_SCHEDULER_SUBMIT(&__ctx->base);").unwrap();
        writeln!(out, "    return ({task_type_name}){{.__task = __ctx, .__drop = __spawn_drop_{fn_name}}};").unwrap();
        writeln!(out, "}}").unwrap();

        // Await function — takes Task__T by value, extracts __task to get SpawnCtx.
        let resolved_ret = resolve_type(ret_c);
        if is_void {
            writeln!(out, "static inline void __gorget_await_{fn_name}({task_type_name} task) {{").unwrap();
        } else {
            writeln!(out, "static inline {resolved_ret} __gorget_await_{fn_name}({task_type_name} task) {{").unwrap();
        }
        writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)task.__task;").unwrap();
        writeln!(out, "    GORGET_SCHEDULER_WAIT(&__ctx->base);").unwrap();
        if !is_void {
            writeln!(out, "    {resolved_ret} result = __ctx->result;").unwrap();
        }
        writeln!(out, "    pthread_mutex_destroy(&__ctx->base.mtx);").unwrap();
        writeln!(out, "    pthread_cond_destroy(&__ctx->base.cond);").unwrap();
        writeln!(out, "    GORGET_FREE(__ctx, sizeof({ctx_name}));").unwrap();
        if !is_void {
            writeln!(out, "    return result;").unwrap();
        }
        writeln!(out, "}}").unwrap();
        writeln!(out).unwrap();
    }

    // Task__T__drop for each unique Task type
    let mut emitted_task_drops: Vec<String> = Vec::new();
    for sf in &module.spawned_fns {
        let task_name = if sf.ret_c_type == "void" {
            "Task__void".to_string()
        } else {
            format!("Task__{}", sf.ret_c_type)
        };
        if emitted_task_drops.contains(&task_name) {
            continue;
        }
        emitted_task_drops.push(task_name.clone());
        writeln!(out, "static inline void {task_name}__drop({task_name}* self) {{").unwrap();
        writeln!(out, "    if (self && self->__task && self->__drop) {{").unwrap();
        writeln!(out, "        self->__drop(self->__task);").unwrap();
        writeln!(out, "        self->__task = NULL;").unwrap();
        writeln!(out, "    }}").unwrap();
        writeln!(out, "}}").unwrap();
        writeln!(out, "static void (*__unused_{task_name}__drop)({task_name}*) __attribute__((unused)) = {task_name}__drop;").unwrap();
        writeln!(out).unwrap();
    }
}

fn emit_thread_helpers(out: &mut String, module: &LirModule) {
    if module.thread_spawned_fns.is_empty() {
        return;
    }
    writeln!(out, "\n/* ── Thread[T] wrappers ── */").unwrap();

    // Collect unique return types for Thread__T typedefs
    let mut emitted_thread_types: Vec<String> = Vec::new();
    for tsf in &module.thread_spawned_fns {
        let ret_c = &tsf.ret_c_type;
        let is_void = ret_c == "void";
        let thread_name = format!("Thread__{ret_c}");
        if emitted_thread_types.contains(&thread_name) {
            continue;
        }
        emitted_thread_types.push(thread_name.clone());
        let ctx_type = format!("__GorgetThread__{ret_c}");
        if is_void {
            writeln!(out, "typedef struct {{ pthread_t _thr; }} {ctx_type};").unwrap();
        } else {
            writeln!(out, "typedef struct {{ pthread_t _thr; {ret_c} _result; }} {ctx_type};").unwrap();
        }
        writeln!(out, "typedef {ctx_type}* {thread_name};").unwrap();
        // id(self) -> int64_t
        writeln!(out, "static inline int64_t {thread_name}__id({thread_name} self) {{ return (int64_t)(uintptr_t)self->_thr; }}").unwrap();
        // join(self) -> T
        if is_void {
            writeln!(out, "static inline void {thread_name}__join({thread_name} self) {{ pthread_join(self->_thr, NULL); GORGET_FREE(self, sizeof(*self)); }}").unwrap();
        } else {
            writeln!(out, "static inline {ret_c} {thread_name}__join({thread_name} self) {{ pthread_join(self->_thr, NULL); {ret_c} _r = self->_result; GORGET_FREE(self, sizeof(*self)); return _r; }}").unwrap();
        }
        writeln!(out).unwrap();
    }

    // Per-function thread entry + spawn helpers
    for tsf in &module.thread_spawned_fns {
        let fn_name = &tsf.fn_name;
        let safe_fn_name = c_func_name(fn_name);
        let ret_c = &tsf.ret_c_type;
        let is_void = ret_c == "void";
        let thread_name = format!("Thread__{ret_c}");
        let ctx_type = format!("__GorgetThread__{ret_c}");

        // Thread entry
        writeln!(out, "static void* __gorget_thread_entry_{fn_name}(void* __arg) {{").unwrap();
        writeln!(out, "    {ctx_type}* __ctx = ({ctx_type}*)__arg;").unwrap();
        if is_void {
            writeln!(out, "    {safe_fn_name}();").unwrap();
        } else {
            writeln!(out, "    __ctx->_result = {safe_fn_name}();").unwrap();
        }
        writeln!(out, "    return NULL;\n}}").unwrap();

        // Spawn function
        writeln!(out, "static inline {thread_name} __gorget_thread_spawn_{fn_name}(void) {{").unwrap();
        writeln!(out, "    {ctx_type}* __ctx = ({ctx_type}*)GORGET_CALLOC(1, sizeof({ctx_type}));").unwrap();
        writeln!(out, "    pthread_create(&__ctx->_thr, NULL, __gorget_thread_entry_{fn_name}, __ctx);").unwrap();
        writeln!(out, "    return __ctx;\n}}").unwrap();
        writeln!(out).unwrap();
    }
}

fn emit_function(out: &mut String, func: &LirFunction, module: &LirModule, sn: &HashMap<u32, String>) {
    // For main() with a Result return type (throws-int main), override to int.
    let is_throws_main = func.name == "main" && matches!(&func.return_type, LirType::Struct(sid) if {
        module.structs.get(sid.0 as usize).map_or(false, |s| s.name.starts_with("Result__"))
    });
    let ret_type_str = if is_throws_main { "int".to_string() } else { c_type_named(&func.return_type, sn) };

    // Signature — special-case main() to accept argc/argv for sys.argv support.
    if func.name == "main" {
        writeln!(out, "int main(int argc, char** argv) {{").unwrap();
        writeln!(out, "    gorget_init_args(argc, argv);").unwrap();
        if let Some(ref trace_path) = module.trace_filename {
            let escaped = trace_path.replace('\\', "\\\\").replace('"', "\\\"");
            writeln!(out, "    __gorget_trace_init(\"{escaped}\");").unwrap();
        }
    } else {
        write!(out, "{} {}(", ret_type_str, c_func_name(&func.name)).unwrap();
        if func.params.is_empty() {
            write!(out, "void").unwrap();
        } else {
            for (i, p) in func.params.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                // Void as non-sole param is invalid C — use void* (closure env ptr).
                let mut ty_str = if matches!(p, LirType::Void) { "void*".to_string() } else { c_type_named(p, sn) };
                if func.const_params.get(i) == Some(&true) && matches!(p, LirType::Ptr) {
                    ty_str = format!("const {ty_str}");
                }
                write!(out, "{ty_str} __p{i}").unwrap();
            }
        }
        writeln!(out, ") {{").unwrap();

        // Trace entry: emit call event with function name, parameter values, and depth.
        if module.trace_filename.is_some() {
            if let Some(ref display_name) = func.display_name {
                let escaped = display_name.replace('\\', "\\\\").replace('"', "\\\"");
                out.push_str("    if (__gorget_trace_fp) {\n");
                let _ = writeln!(out, "        fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"call\\\",\\\"fn\\\":\\\"{escaped}\\\",\\\"args\\\":{{\");");
                for (i, p) in func.params.iter().enumerate() {
                    let formatter = lir_trace_formatter(p, module);
                    let comma = if i == 0 { "" } else { "," };
                    let pname = func.param_names.get(i)
                        .and_then(|n| n.as_deref())
                        .unwrap_or("_");
                    let esc_name = pname.replace('\\', "\\\\").replace('"', "\\\"");
                    let _ = writeln!(out, "        fprintf(__gorget_trace_fp, \"{comma}\\\"{esc_name}\\\":\");");
                    let _ = writeln!(out, "        {formatter}(__gorget_trace_fp, __p{i});");
                }
                let _ = writeln!(out, "        fprintf(__gorget_trace_fp, \"}},\\\"depth\\\":%d}}\\n\", __gorget_trace_depth++);");
                out.push_str("    }\n");
            }
        }
    }

    // Value declarations — collect all values defined in the function.
    let mut max_val = 0u32;
    for block in &func.blocks {
        for (vid, _) in &block.params {
            if vid.0 >= max_val {
                max_val = vid.0 + 1;
            }
        }
        for inst in &block.insts {
            if let Some(dst) = inst.dst() {
                if dst.0 >= max_val {
                    max_val = dst.0 + 1;
                }
            }
        }
    }

    // Declare all values as their inferred types.
    // Build a type map from instructions (two passes for arithmetic type propagation).
    let mut val_types: Vec<Option<LirType>> = vec![None; max_val as usize];
    // Track which values originate from StrLit instructions (raw `const char*`).
    let mut str_lit_vals: Vec<bool> = vec![false; max_val as usize];
    // Track which values are raw C strings (const char*) from runtime functions.
    // Only these should be wrapped with gorget_str_from_literal when stored to Str slots.
    let mut cstr_vals: Vec<bool> = vec![false; max_val as usize];
    // Track which values are NullPtr (so we can avoid memcpy from NULL).
    let mut null_vals: Vec<bool> = vec![false; max_val as usize];
    // Track which values are FuncAddr — maps value → FuncId for adapter generation.
    let mut func_addr_targets: Vec<Option<FuncId>> = vec![None; max_val as usize];
    // Track which spawn function produced a void* value (for task_group_submit reconstruction).
    // When a spawn result is extracted to .__task (void*), we need to reconstruct Task__T
    // with the correct __drop function when passing to gorget_task_group_submit.
    let mut spawn_source_fn: Vec<Option<String>> = vec![None; max_val as usize];
    // Track the pointee type for Ptr-typed values (e.g. SlotAddr → slot type, FieldPtr → field type).
    // Used by Inst::Store to emit correct sizeof() for memcpy of aggregates.
    let mut ptr_pointee: Vec<Option<LirType>> = vec![None; max_val as usize];
    // Propagate pointee types through Ptr-typed slots (SlotStore → SlotLoad).
    let mut slot_pointee: Vec<Option<LirType>> = vec![None; func.slots.len()];
    // Override the C type name for values whose LIR type can't represent runtime structs
    // (e.g. GorgetArray, GorgetMap — not in module.structs but needed for correct C declarations).
    let mut val_c_type_override: Vec<Option<String>> = vec![None; max_val as usize];
    // Track which values came from gorget_map_get/gorget_array_get (return void* into internal storage).
    // Previously used for clone-on-read; now kept for potential future use.
    let mut _collection_get_vals: Vec<bool> = vec![false; max_val as usize];
    for block in &func.blocks {
        for (vid, ty) in &block.params {
            val_types[vid.0 as usize] = Some(ty.clone());
        }
        for inst in &block.insts {
            if let Some(ty) = infer_inst_type(inst, module, &val_types, &ptr_pointee) {
                if let Some(dst) = inst.dst() {
                    val_types[dst.0 as usize] = Some(ty);
                }
            }
            // Detect runtime struct returns that aren't in module.structs
            if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                if let Some(rt_name) = runtime_fn_return_struct(name) {
                    let in_module = module.structs.iter().any(|s| s.name == rt_name);
                    if !in_module {
                        val_c_type_override[d.0 as usize] = Some(rt_name.to_string());
                    }
                }
            }
            if let Inst::StrLit { dst, .. } = inst {
                str_lit_vals[dst.0 as usize] = true;
            }
            if let Inst::FuncAddr { dst, func } = inst {
                func_addr_targets[dst.0 as usize] = Some(*func);
            }
            // Mark CallExtern results that return const char* (not struct pointers).
            // Also override their value type to Ptr so they're declared as void* in C.
            if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                if is_cstr_returning_fn(name) {
                    cstr_vals[d.0 as usize] = true;
                    val_types[d.0 as usize] = Some(LirType::Ptr);
                }
            }
            if let Inst::NullPtr { dst } = inst {
                null_vals[dst.0 as usize] = true;
            }
            // Track collection get results — pointers into internal storage that need cloning on Load.
            if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                if name == "gorget_map_get" || name == "gorget_array_get" {
                    _collection_get_vals[d.0 as usize] = true;
                }
            }
            // Track spawn source function for void* destinations.
            // When __gorget_spawn_X returns Task__T but dst is void*, the codegen
            // extracts .__task; we record X so task_group_submit can reconstruct
            // the full Task struct with the correct __drop fn.
            if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                if name.starts_with("__gorget_spawn_") {
                    let is_task_struct = matches!(val_types.get(d.0 as usize).and_then(|t| t.as_ref()), Some(LirType::Struct(sid)) if {
                        module.structs.get(sid.0 as usize).map_or(false, |s| s.name.starts_with("Task__"))
                    });
                    if !is_task_struct {
                        let fn_suffix = name.strip_prefix("__gorget_spawn_").unwrap_or("").to_string();
                        spawn_source_fn[d.0 as usize] = Some(fn_suffix);
                    }
                }
            }
            // Track pointee types for pointer-producing instructions.
            match inst {
                Inst::SlotAddr { dst, slot } => {
                    let slot_ty = &func.slots[slot.0 as usize].ty;
                    ptr_pointee[dst.0 as usize] = Some(slot_ty.clone());
                }
                Inst::FieldPtr { dst, struct_id, field, .. } => {
                    let sdef = &module.structs[struct_id.0 as usize];
                    if (*field as usize) < sdef.fields.len() {
                        ptr_pointee[dst.0 as usize] = Some(sdef.fields[*field as usize].1.clone());
                    }
                }
                Inst::ElemPtr { dst, .. } => {
                    // Element pointer — pointee type unknown without array element type info.
                    // Leave as None; Store will fall back to sizeof(*(val)).
                    let _ = dst;
                }
                Inst::GlobalAddr { dst, .. } => {
                    // Could track global type, but globals are rarely stored into via Store.
                    let _ = dst;
                }
                // Propagate pointee types through SlotStore→SlotLoad chains.
                // When a Ptr-typed slot stores a value with known pointee, propagate
                // to subsequent loads from that slot.
                Inst::SlotStore { slot, value, .. } => {
                    if let Some(pt) = ptr_pointee.get(value.0 as usize).and_then(|p| p.clone()) {
                        if matches!(func.slots[slot.0 as usize].ty, LirType::Ptr) {
                            slot_pointee[slot.0 as usize] = Some(pt);
                        }
                    }
                }
                Inst::SlotLoad { dst, slot, .. } => {
                    if let Some(pt) = slot_pointee.get(slot.0 as usize).and_then(|p| p.clone()) {
                        ptr_pointee[dst.0 as usize] = Some(pt);
                    }
                }
                _ => {}
            }
        }
    }

    // Fix val_types for values with no inferred type (e.g. InlineC dst values).
    // For InlineC→SlotStore, use the slot's type.
    // For values passed as block parameter arguments, use the block param's type.
    for block in &func.blocks {
        let insts = &block.insts;
        for (i, inst) in insts.iter().enumerate() {
            if let Inst::InlineC { dst: Some(d), .. } = inst {
                if val_types.get(d.0 as usize) == Some(&None) {
                    if let Some(Inst::SlotStore { slot, value, .. }) = insts.get(i + 1) {
                        if *value == *d {
                            let slot_ty = func.slots[slot.0 as usize].ty.clone();
                            if !matches!(slot_ty, LirType::Ptr | LirType::Void) {
                                val_types[d.0 as usize] = Some(slot_ty);
                            }
                        }
                    }
                }
            }
        }
        // Infer or correct value types from block parameter types at jump/branch targets.
        // Block param types come from SSA slot types (deterministic). If the forward pass
        // inferred a different type (e.g., due to value numbering shifts from drop changes),
        // the block param type is authoritative.
        let infer_from_args = |target: BlockId, args: &[ValueId], val_types: &mut Vec<Option<LirType>>| {
            let target_params = &func.blocks[target.0 as usize].params;
            for (arg, (_, param_ty)) in args.iter().zip(target_params.iter()) {
                let current = val_types.get(arg.0 as usize).and_then(|t| t.as_ref());
                if current.is_none() || current != Some(param_ty) {
                    val_types[arg.0 as usize] = Some(param_ty.clone());
                }
            }
        };
        match &block.terminator {
            Term::Jump(target, args) => {
                infer_from_args(*target, args, &mut val_types);
            }
            Term::Branch { then_block, then_args, else_block, else_args, .. } => {
                infer_from_args(*then_block, then_args, &mut val_types);
                infer_from_args(*else_block, else_args, &mut val_types);
            }
            _ => {}
        }
    }

    // Fix val_types for CallExtern→SlotStore type mismatches. The extern
    // declaration's return type may be a scalar/Ptr (e.g. void*, int64_t),
    // but the GIR intended a richer type (e.g. Option[int], GorgetArray).
    // When the slot type disagrees with the inferred value type and the slot
    // type is not Ptr/Void, prefer the slot type — it comes from the GIR and
    // is more precise than the C runtime's generic signature.
    //
    // Exception: when the current type is Ptr and the slot is Str/GorgetString,
    // keep Ptr — the SlotStore handler wraps the pointer with gorget_str_from_literal.
    // Also skip cstr-returning functions which produce raw pointers handled at store time.
    for block in &func.blocks {
        let insts = &block.insts;
        for (i, inst) in insts.iter().enumerate() {
            if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                if let Some(Inst::SlotStore { slot, value, .. }) = insts.get(i + 1) {
                    if *value == *d {
                        let slot_ty = func.slots[slot.0 as usize].ty.clone();
                        if !matches!(slot_ty, LirType::Ptr | LirType::Void) {
                            let current = val_types[d.0 as usize].as_ref();
                            // Don't override Ptr values for cstr-returning functions or StrLit
                            // — their Ptr→Str conversion is handled by SlotStore wrapping.
                            let is_ptr_to_str = matches!(current, Some(LirType::Ptr))
                                && is_str_struct(&slot_ty, module);
                            let is_cstr_fn = is_cstr_returning_fn(name);
                            if !is_ptr_to_str && !is_cstr_fn && current != Some(&slot_ty) {
                                val_types[d.0 as usize] = Some(slot_ty);
                            }
                        }
                    }
                }
            }
        }
    }

    // Fix val_types for guard/shared accessor results.
    // gorget_guard_get / gorget_shared_get_ptr return void* but the actual
    // inner type can be inferred from consumers (printf format, arithmetic, etc.).
    // Default to I64 for these accessors when the consumer doesn't reveal the type.
    for block in &func.blocks {
        let insts = &block.insts;
        for (i, inst) in insts.iter().enumerate() {
            if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                let is_guard_value_accessor = matches!(name.as_str(),
                    "gorget_guard_get"
                    | "gorget_shared_get"
                    | "gorget_read_guard_get"
                    | "gorget_write_guard_get"
                );
                let is_guard_ptr_accessor = matches!(name.as_str(),
                    "gorget_guard_get_ptr"
                    | "gorget_shared_get_ptr"
                    | "gorget_read_guard_get_ptr"
                    | "gorget_write_guard_get_ptr"
                );
                // *_get_ptr functions return a raw pointer — keep as Ptr, never override to I64.
                if is_guard_ptr_accessor && matches!(val_types.get(d.0 as usize), Some(Some(LirType::Ptr)) | Some(None)) {
                    val_types[d.0 as usize] = Some(LirType::Ptr);
                }
                if is_guard_value_accessor && matches!(val_types.get(d.0 as usize), Some(Some(LirType::Ptr)) | Some(None)) {
                    // Look at the next few instructions for a consumer that reveals the type.
                    let mut inferred = None;
                    for ci in (i+1)..insts.len().min(i+10) {
                        match &insts[ci] {
                            Inst::Add { ty, lhs, .. } | Inst::Sub { ty, lhs, .. }
                            | Inst::Mul { ty, lhs, .. } | Inst::Div { ty, lhs, .. }
                            | Inst::Rem { ty, lhs, .. } if *lhs == *d => {
                                inferred = Some(ty.clone());
                                break;
                            }
                            Inst::IntCast { value, .. } if *value == *d => {
                                inferred = Some(LirType::I64);
                                break;
                            }
                            Inst::FloatCast { value, .. } if *value == *d => {
                                inferred = Some(LirType::F64);
                                break;
                            }
                            // Check SlotStore — the slot type reveals the inner type.
                            Inst::SlotStore { value, slot, .. } if *value == *d => {
                                let slot_ty = &func.slots[slot.0 as usize].ty;
                                if !matches!(slot_ty, LirType::Ptr) {
                                    inferred = Some(slot_ty.clone());
                                    break;
                                }
                            }
                            // Check printf: if the value is an arg to printf with a float format
                            Inst::CallExtern { name: call_name, args, .. }
                                if call_name == "printf" && args.len() >= 2
                                    && args[1..].contains(d) => {
                                // Check if the format string contains %f — indicates float value.
                                if let Some(fmt_val) = args.first() {
                                    // Walk backwards to find the StrLit for the format string.
                                    for si in (0..ci).rev().take(10) {
                                        if let Inst::StrLit { dst: sd, value: fmt } = &insts[si] {
                                            if *sd == *fmt_val {
                                                if fmt.contains("%f") || fmt.contains("%.") {
                                                    inferred = Some(LirType::F64);
                                                }
                                                break; // found the StrLit for this format arg
                                            }
                                        }
                                    }
                                    if inferred.is_some() { break; }
                                }
                            }
                            _ => {}
                        }
                    }
                    val_types[d.0 as usize] = Some(inferred.unwrap_or(LirType::I64));
                }
            }
        }
    }

    // Fix cross-type map combinator types. When Option__T__map is called with
    // a closure that returns U≠T, the result should be Option__U. The GIR doesn't
    // track this, so the slot and val_types have the wrong type. Fix both: val_types
    // for the value declaration and slot_overrides for the slot declaration.
    let mut slot_overrides: HashMap<u32, LirType> = HashMap::new();
    for block in &func.blocks {
        let insts = &block.insts;
        for (idx, inst) in insts.iter().enumerate() {
            if let Inst::CallExtern { dst: Some(d), name, args: call_args, .. } = inst {
                if let Some((_type_prefix, "map")) = parse_option_result_combinator(name) {
                    if call_args.len() > 1 {
                        let closure_struct = ptr_pointee.get(call_args[1].0 as usize)
                            .and_then(|t| t.as_ref())
                            .map(|t| c_type_named(t, sn));
                        let call_fn = closure_struct
                            .map(|n| find_closure_call_fn(module, &n, sn))
                            .unwrap_or_default();
                        if !call_fn.is_empty() {
                            if let Some(ret_ty_name) = closure_call_return_type(module, &call_fn, sn) {
                                let prefix = if name.starts_with("Option__") { "Option__" } else { "Result__" };
                                let target_name = format!("{prefix}{ret_ty_name}");
                                if let Some(target_sid) = module.structs.iter().position(|s| s.name == target_name) {
                                    let target_ty = LirType::Struct(StructId(target_sid as u32));
                                    val_types[d.0 as usize] = Some(target_ty.clone());
                                    // Also fix the slot that receives this value.
                                    if let Some(Inst::SlotStore { slot, value, .. }) = insts.get(idx + 1) {
                                        if *value == *d {
                                            slot_overrides.insert(slot.0, target_ty);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Infer types for untyped values from their consumers (arithmetic, comparison, etc.)
    // This handles polymorphic extern results used in typed operations without an intervening slot store.
    for block in &func.blocks {
        for inst in &block.insts {
            let consumer_ty = match inst {
                Inst::Add { ty, lhs, rhs, .. } | Inst::Sub { ty, lhs, rhs, .. }
                | Inst::Mul { ty, lhs, rhs, .. } | Inst::Div { ty, lhs, rhs, .. }
                | Inst::Rem { ty, lhs, rhs, .. } | Inst::Mod { ty, lhs, rhs, .. } => {
                    Some((ty.clone(), vec![*lhs, *rhs]))
                }
                Inst::Neg { ty, operand, .. } => Some((ty.clone(), vec![*operand])),
                Inst::BitAnd { ty, lhs, rhs, .. } | Inst::BitOr { ty, lhs, rhs, .. }
                | Inst::BitXor { ty, lhs, rhs, .. }
                | Inst::Shl { ty, lhs, rhs, .. } | Inst::Shr { ty, lhs, rhs, .. } => {
                    Some((ty.clone(), vec![*lhs, *rhs]))
                }
                Inst::BitNot { ty, operand, .. } => Some((ty.clone(), vec![*operand])),
                // SlotStore: infer from the slot's declared type.
                Inst::SlotStore { slot, value, .. } => {
                    let sty = slot_overrides.get(&slot.0).unwrap_or(&func.slots[slot.0 as usize].ty).clone();
                    if !matches!(sty, LirType::Ptr | LirType::Void) {
                        Some((sty, vec![*value]))
                    } else { None }
                }
                Inst::IntCast { value, .. } | Inst::FloatCast { value, .. }
                | Inst::IntToFloat { value, .. } | Inst::FloatToInt { value, .. } => {
                    // The source type can be inferred from the cast target for the *source* operand.
                    // But we don't know the source type from the cast alone — skip.
                    let _ = value;
                    None
                }
                _ => None,
            };
            if let Some((ty, operands)) = consumer_ty {
                for op in operands {
                    if val_types.get(op.0 as usize).and_then(|t| t.as_ref()).is_none() {
                        val_types[op.0 as usize] = Some(ty.clone());
                    }
                }
            }
            // Cmp: propagate peer type between operands.
            if let Inst::Cmp { lhs, rhs, .. } = inst {
                let lty = val_types.get(lhs.0 as usize).and_then(|t| t.as_ref()).cloned();
                let rty = val_types.get(rhs.0 as usize).and_then(|t| t.as_ref()).cloned();
                if lty.is_some() && rty.is_none() {
                    if let Some(lt) = &lty {
                        if !matches!(lt, LirType::Ptr | LirType::Void) {
                            val_types[rhs.0 as usize] = lty;
                        }
                    }
                } else if rty.is_some() && lty.is_none() {
                    if let Some(rt) = &rty {
                        if !matches!(rt, LirType::Ptr | LirType::Void) {
                            val_types[lhs.0 as usize] = rty;
                        }
                    }
                }
            }
        }
        // Also infer from block terminators: Ret(value) implies function return type.
        if let Term::Ret(val) = &block.terminator {
            if !matches!(func.return_type, LirType::Void | LirType::Ptr) {
                if val_types.get(val.0 as usize).and_then(|t| t.as_ref()).is_none() {
                    val_types[val.0 as usize] = Some(func.return_type.clone());
                }
            }
        }
    }

    // Override slot types for runtime structs that are larger than a pointer.
    // gorget_mutex_lock_to / gorget_rwlock_{read,write}_lock_to write a struct
    // (gorget_guard_t / gorget_rw_guard_t) into a slot via output pointer.
    // The LIR types them as Ptr, but the C type must be the actual struct.
    let mut slot_c_overrides: HashMap<u32, &str> = HashMap::new();
    for block in &func.blocks {
        for inst in &block.insts {
            if let Inst::CallExtern { name, args, .. } = inst {
                let guard_c_type = if name == "gorget_mutex_lock_to" { Some("gorget_guard_t") }
                    else if name == "gorget_rwlock_read_lock_to" || name == "gorget_rwlock_write_lock_to" { Some("gorget_rw_guard_t") }
                    else if name == "gorget_channel_recv_to" { Some("gorget_guard_t") }
                    else { None };
                if let Some(c_type) = guard_c_type {
                    // The output pointer arg is the last arg.
                    if let Some(out_arg) = args.last() {
                        // Trace back to SlotAddr to find the slot.
                        if let Some(Inst::SlotAddr { slot, .. }) = func.blocks.iter()
                            .flat_map(|b| b.insts.iter())
                            .find(|i| matches!(i, Inst::SlotAddr { dst, .. } if *dst == *out_arg))
                        {
                            slot_c_overrides.insert(slot.0, c_type);
                        }
                    }
                }
            }
        }
    }

    // Pre-pass: identify CallExtern results from view-eligible string methods whose
    // destination slot is Str-typed (view) AND whose value never escapes to a
    // non-Str destination (struct field, function arg, return). Views are only safe
    // when they don't outlive their source — storing a view into a struct field that
    // outlives the source causes use-after-free.
    let mut view_method_vals: Vec<bool> = vec![false; max_val as usize];
    {
        let view_eligible = |name: &str| -> bool {
            matches!(name,
                "gorget_str_trim" | "gorget_str_byte_slice" | "gorget_str_char_at"
                | "gorget_str_strip" | "gorget_str_lstrip" | "gorget_str_rstrip"
                | "gorget_str_lstrip_ws" | "gorget_str_rstrip_ws"
                | "gorget_str_removeprefix" | "gorget_str_removesuffix"
                | "gorget_str_slice"
            )
        };
        // Build set of Str slots that are only stored to and then consumed by
        // print/comparison — never loaded for reuse. A slot is safe for _view
        // only if it's never read back (SlotLoad). If it IS loaded, the value
        // could flow into a struct field, function arg, or return that outlives
        // the source string.
        let mut loaded_str_slots: std::collections::HashSet<u32> = std::collections::HashSet::new();
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::SlotLoad { slot, .. } = inst {
                    let slot_ty = &func.slots[slot.0 as usize].ty;
                    let is_str = matches!(slot_ty, LirType::Struct(sid) if sn.get(&sid.0).map_or(false, |n| n == "Str"));
                    if is_str {
                        loaded_str_slots.insert(slot.0);
                    }
                }
                // SlotAddr also escapes — pointer to the slot could be used anywhere
                if let Inst::SlotAddr { slot, .. } = inst {
                    let slot_ty = &func.slots[slot.0 as usize].ty;
                    let is_str = matches!(slot_ty, LirType::Struct(sid) if sn.get(&sid.0).map_or(false, |n| n == "Str"));
                    if is_str {
                        loaded_str_slots.insert(slot.0);
                    }
                }
            }
        }
        for block in &func.blocks {
            let insts = &block.insts;
            for (i, inst) in insts.iter().enumerate() {
                if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                    if view_eligible(name) {
                        // Check if the next instruction stores this val to a Str-typed slot.
                        if let Some(Inst::SlotStore { slot, value, .. }) = insts.get(i + 1) {
                            if *value == *d {
                                let slot_ty = &func.slots[slot.0 as usize].ty;
                                let is_str = matches!(slot_ty, LirType::Struct(sid) if sn.get(&sid.0).map_or(false, |n| n == "Str"));
                                if is_str && !loaded_str_slots.contains(&slot.0) {
                                    view_method_vals[d.0 as usize] = true;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Slot declarations (emitted after cross-type fix-ups so slot_overrides are applied).
    for (i, slot) in func.slots.iter().enumerate() {
        let effective_ty = slot_overrides.get(&(i as u32)).unwrap_or(&slot.ty);
        let ty_str = if let Some(c_override) = slot_c_overrides.get(&(i as u32)) {
            c_override.to_string()
        } else {
            let ts = c_type_named(effective_ty, sn);
            if ts == "void" { "void*".to_string() } else { ts }
        };

        write!(out, "    {ty_str} __s{i}").unwrap();
        // Zero-initialize
        if slot_c_overrides.contains_key(&(i as u32)) {
            // C type override is always a struct — use aggregate init.
            write!(out, " = {{0}}").unwrap();
        } else if effective_ty.is_scalar() {
            write!(out, " = 0").unwrap();
        } else {
            write!(out, " = {{0}}").unwrap();
        }
        writeln!(out, ";").unwrap();
    }

    for (i, ty) in val_types.iter().enumerate() {
        // Use C type override if available (for runtime structs not in module.structs).
        if let Some(Some(c_override)) = val_c_type_override.get(i) {
            writeln!(out, "    {} __v{i};", c_override).unwrap();
            continue;
        }
        // cstr_vals are const char* from runtime functions — declare as such to avoid const-discard warnings.
        if cstr_vals.get(i).copied().unwrap_or(false) {
            writeln!(out, "    const char* __v{i};").unwrap();
            continue;
        }
        match ty {
            Some(ty) => {
                let ts = c_type_named(ty, sn);
                if ts == "void" {
                    // Void-typed values are used as opaque pointers — declare as void*.
                    writeln!(out, "    void* __v{i};").unwrap();
                } else {
                    writeln!(out, "    {} __v{i};", ts).unwrap();
                }
            }
            None => {
                // No type inferred — value is referenced but type couldn't be determined.
                // Declare as void* to avoid undeclared variable errors.
                writeln!(out, "    void* __v{i};").unwrap();
            }
        }
    }

    // Block parameter move variables (for parallel copy semantics).
    // Each block param needs a temporary for parallel moves.
    for block in &func.blocks {
        for (vid, ty) in &block.params {
            writeln!(out, "    {} __bp{};", c_type_named(ty, sn), vid.0).unwrap();
        }
    }

    writeln!(out).unwrap();

    // For the main function, emit runtime call initializers for globals.
    if func.name == "main" {
        // Build original→LIR struct name map for rewriting compound literals in RuntimeCall exprs.
        let orig_to_lir: Vec<(String, String)> = module.structs.iter().enumerate()
            .filter_map(|(i, def)| {
                let lir_name = sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"));
                if lir_name != def.name {
                    Some((def.name.clone(), lir_name))
                } else {
                    None
                }
            })
            .collect();
        for (gid, g) in module.globals.iter().enumerate() {
            if let LirGlobalInit::RuntimeCall(expr) = &g.init {
                let mut rewritten = expr.clone();
                for (orig, lir_name) in &orig_to_lir {
                    rewritten = rewritten.replace(
                        &format!("({orig}){{"),
                        &format!("({lir_name}){{"),
                    );
                }
                writeln!(out, "    __lir_g{gid} = {rewritten};").unwrap();
            }
        }
    }

    // Track which slots have been registered on the cleanup stack (test functions only).
    let mut test_cleanup_pushed = std::collections::HashSet::<u32>::new();

    let tracing = module.trace_filename.is_some();
    let is_main = func.name == "main";

    // Pre-scan: collect which blocks are the "then" target of Branch terminators.
    let trace_then_blocks: std::collections::HashSet<u32> = if tracing {
        func.blocks.iter().filter_map(|b| {
            if let Term::Branch { then_block, .. } = &b.terminator {
                Some(then_block.0)
            } else {
                None
            }
        }).collect()
    } else {
        std::collections::HashSet::new()
    };

    // Blocks
    for block in &func.blocks {
        writeln!(out, "__bb{}:", block.id.0).unwrap();

        // Branch event: emitted when a "then" block is actually entered.
        if tracing && trace_then_blocks.contains(&block.id.0) {
            let _ = writeln!(out, "    if (__gorget_trace_fp) {{ fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"branch\\\",\\\"depth\\\":%d}}\\n\", __gorget_trace_depth); }}");
        }

        // Stmt_start event: emitted at the start of each block.
        if tracing {
            let _ = writeln!(out, "    if (__gorget_trace_fp) {{ fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"stmt_start\\\",\\\"depth\\\":%d}}\\n\", __gorget_trace_depth++); }}");
        }

        // Move block params from temporaries.
        for (vid, _) in &block.params {
            writeln!(out, "    __v{} = __bp{};", vid.0, vid.0).unwrap();
        }

        // Instructions
        for inst in &block.insts {
            write!(out, "    ").unwrap();
            emit_inst(out, inst, func, module, sn, &val_types, &str_lit_vals, &cstr_vals, &null_vals, &ptr_pointee, &func_addr_targets, &spawn_source_fn, &_collection_get_vals, &view_method_vals);
            writeln!(out).unwrap();

            // In test functions, register droppable user-named slots on the cleanup stack
            // so they're cleaned up if gorget_panic() calls longjmp (test assertion fails).
            if func.is_test_fn {
                if let Inst::SlotStore { slot, .. } = inst {
                    let slot_idx = slot.0;
                    if !test_cleanup_pushed.contains(&slot_idx) {
                        if func.slots[slot_idx as usize].name.is_some() {
                            if let Some(push_code) = test_cleanup_push_code_lir(slot_idx, func, module, sn) {
                                out.push_str(&push_code);
                                test_cleanup_pushed.insert(slot_idx);
                            }
                        }
                    }
                }
            }
        }

        // Stmt_end event: emitted after instructions, before the terminator.
        if tracing {
            let _ = writeln!(out, "    if (__gorget_trace_fp) {{ fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"stmt_end\\\",\\\"depth\\\":%d}}\\n\", --__gorget_trace_depth); }}");
        }

        // Trace return event: inject before each return statement for non-main functions.
        if tracing && !is_main {
            if matches!(&block.terminator, Term::Ret(_) | Term::RetVoid) {
                if let Some(ref display_name) = func.display_name {
                    let escaped = display_name.replace('\\', "\\\\").replace('"', "\\\"");
                    let _ = writeln!(out, "    if (__gorget_trace_fp) {{ fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"return\\\",\\\"fn\\\":\\\"{escaped}\\\",\\\"depth\\\":%d}}\\n\", __gorget_trace_depth--); }}");
                }
            }
        }

        // Terminator
        write!(out, "    ").unwrap();
        emit_term(out, &block.terminator, func, module, sn, &val_types);
        writeln!(out).unwrap();
    }

    writeln!(out, "}}").unwrap();
}

fn emit_inst(out: &mut String, inst: &Inst, func: &LirFunction, module: &LirModule, sn: &HashMap<u32, String>, val_types: &[Option<LirType>], str_lit_vals: &[bool], cstr_vals: &[bool], null_vals: &[bool], ptr_pointee: &[Option<LirType>], func_addr_targets: &[Option<FuncId>], spawn_source_fn: &[Option<String>], _collection_get_vals: &[bool], view_method_vals: &[bool]) {
    let v = |id: ValueId| -> String { format!("__v{}", id.0) };
    let s = |id: SlotId| -> String { format!("__s{}", id.0) };

    match inst {
        // Slot access
        Inst::SlotStore { slot, value, is_move } => {
            // Skip store to slot 0 (return slot) in void-returning functions.
            // LIR declares slot 0 as void* but unit values are int32_t — skip to avoid type mismatch.
            if slot.0 == 0 && matches!(func.return_type, LirType::Void) {
                return;
            }
            let slot_ty = &func.slots[slot.0 as usize].ty;
            let val_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
            let slot_is_str = matches!(slot_ty, LirType::Struct(sid) if sn.get(&sid.0).map_or(false, |n| n == "Str"));
            let slot_is_gs = matches!(slot_ty, LirType::Struct(sid) if sn.get(&sid.0).map_or(false, |n| n == "GorgetString"));
            let slot_is_closure = matches!(slot_ty, LirType::Struct(sid) if sn.get(&sid.0).map_or(false, |n| n == "GorgetClosure"));
            let is_str_lit_val = str_lit_vals.get(value.0 as usize).copied().unwrap_or(false);
            let is_cstr = cstr_vals.get(value.0 as usize).copied().unwrap_or(false);
            // __Closure_N struct → GorgetClosure slot: heap-alloc env + pack fn_ptr/env.
            let val_closure_name = if slot_is_closure {
                // Check direct struct type or pointee type for __Closure_N
                let check_sid = |sid: &StructId| -> Option<String> {
                    let name = sn.get(&sid.0).cloned().unwrap_or_default();
                    if name.starts_with("__Closure_") {
                        return Some(name);
                    }
                    // __lir_sN aliases — check the actual module struct name
                    module.structs.get(sid.0 as usize)
                        .filter(|sd| sd.name.starts_with("__Closure_"))
                        .map(|sd| sd.name.clone())
                };
                if let Some(LirType::Struct(val_sid)) = val_ty {
                    check_sid(val_sid)
                } else if matches!(val_ty, Some(LirType::Ptr)) {
                    // Pointer to a __Closure_N struct (e.g., from SlotAddr)
                    if let Some(Some(LirType::Struct(pt_sid))) = ptr_pointee.get(value.0 as usize) {
                        check_sid(pt_sid)
                    } else { None }
                } else { None }
            } else { None };
            if let Some(closure_struct_name) = val_closure_name {
                let call_fn = format!("{closure_struct_name}__call");
                let val_is_ptr = matches!(val_ty, Some(LirType::Ptr));
                // Use the LIR C name (e.g. __lir_s9) for the struct, not the GIR name (__Closure_0).
                let c_struct_name = if val_is_ptr {
                    if let Some(Some(LirType::Struct(pt_sid))) = ptr_pointee.get(value.0 as usize) {
                        c_type_named(&LirType::Struct(*pt_sid), sn)
                    } else { closure_struct_name.clone() }
                } else if let Some(LirType::Struct(sid)) = val_ty {
                    c_type_named(&LirType::Struct(*sid), sn)
                } else { closure_struct_name.clone() };
                if val_is_ptr {
                    write!(out, "{{ {c_struct_name}* __heap = ({c_struct_name}*)GORGET_ALLOC(sizeof({c_struct_name})); memcpy(__heap, {v}, sizeof({c_struct_name})); {s} = (GorgetClosure){{.fn_ptr = (void*){call_fn}, .env = (void*)__heap}}; }}",
                        v = v(*value), s = s(*slot)).unwrap();
                } else {
                    write!(out, "{{ {c_struct_name}* __heap = ({c_struct_name}*)GORGET_ALLOC(sizeof({c_struct_name})); *__heap = {v}; {s} = (GorgetClosure){{.fn_ptr = (void*){call_fn}, .env = (void*)__heap}}; }}",
                        v = v(*value), s = s(*slot)).unwrap();
                }
                return;
            }
            // FuncAddr → GorgetClosure slot: wrap named function with adapter.
            if slot_is_closure {
                if let Some(fid) = func_addr_targets.get(value.0 as usize).and_then(|t| *t) {
                    let adapt_name = format!("__adapt_{}", c_func_name(&module.functions[fid.0 as usize].name));
                    write!(out, "{s} = (GorgetClosure){{.fn_ptr = (void*){adapt}, .env = NULL}};",
                        s = s(*slot), adapt = adapt_name).unwrap();
                    return;
                }
            }
            // GorgetClosure slot with non-closure value (e.g. void*, int64_t, or void from another slot):
            // memcpy to avoid type mismatch. The value is always a pointer to closure data
            // (from SlotAddr, array_get, etc.), even when LIR types it as I64.
            if slot_is_closure && !matches!(val_ty, Some(LirType::Struct(_))) {
                write!(out, "memcpy(&{}, (void*){}, sizeof(GorgetClosure));", s(*slot), v(*value)).unwrap();
                return;
            }
            // Implicit Result::Ok / Option::Some wrapping: scalar or non-wrapper struct → Result/Option slot.
            if let LirType::Struct(slot_sid) = slot_ty {
                let slot_struct_name = module.structs.get(slot_sid.0 as usize).map(|sd| sd.name.as_str()).unwrap_or("");
                let is_result_slot = slot_struct_name.starts_with("Result__");
                let is_option_slot = slot_struct_name.starts_with("Option__");
                if is_result_slot || is_option_slot {
                    let val_is_same_wrapper = match val_ty {
                        Some(LirType::Struct(val_sid)) => {
                            let vn = module.structs.get(val_sid.0 as usize).map(|sd| sd.name.as_str()).unwrap_or("");
                            (is_result_slot && vn.starts_with("Result__")) || (is_option_slot && vn.starts_with("Option__"))
                        }
                        _ => false,
                    };
                    // Only wrap when val_ty is a primitive numeric/bool type (not Ptr, not struct, not void/unknown).
                    let val_is_primitive = matches!(val_ty, Some(
                        LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64
                        | LirType::U8 | LirType::U16 | LirType::U32 | LirType::U64
                        | LirType::F32 | LirType::F64 | LirType::Bool
                    ));
                    if !val_is_same_wrapper && val_is_primitive {
                        let ty_name = c_type_named(slot_ty, sn);
                        let payload_field = if is_result_slot {
                            // Find Ok field name from struct def
                            module.structs.get(slot_sid.0 as usize)
                                .and_then(|sd| sd.fields.iter().find(|(n, _)| n.starts_with("Ok")))
                                .map(|(n, _)| c_field_name(n))
                                .unwrap_or_else(|| "Ok_0".to_string())
                        } else {
                            module.structs.get(slot_sid.0 as usize)
                                .and_then(|sd| sd.fields.iter().find(|(n, _)| n.starts_with("Some")))
                                .map(|(n, _)| c_field_name(n))
                                .unwrap_or_else(|| "Some_0".to_string())
                        };
                        write!(out, "memset(&{s}, 0, sizeof({ty})); {s}.tag = 0; {s}.{f} = {val};",
                            s = s(*slot), ty = ty_name, f = payload_field, val = v(*value)).unwrap();
                        return;
                    }
                }
            }
            if slot_is_str && is_str_lit_val {
                // String literal (const char*) → Str slot: wrap with gorget_str_from_literal.
                write!(out, "{} = gorget_str_from_literal({}, strlen({}));", s(*slot), v(*value), v(*value)).unwrap();
            } else if slot_is_str && is_cstr {
                // Known const char* value (from gorget_int_to_str etc.) → Str slot: wrap.
                write!(out, "{{ const char* __cp = (const char*){}; {} = gorget_str_from_literal(__cp, __cp ? strlen(__cp) : 0); }}", v(*value), s(*slot)).unwrap();
            } else if slot_is_gs && is_str_lit_val {
                // String literal → GorgetString slot: wrap with gorget_string_new.
                write!(out, "{} = gorget_string_new({});", s(*slot), v(*value)).unwrap();
            } else if slot_is_gs && is_cstr {
                // Known const char* → GorgetString slot: wrap.
                write!(out, "{{ const char* __cp = (const char*){}; {} = gorget_string_new(__cp); }}", v(*value), s(*slot)).unwrap();
            } else if slot_ty.is_aggregate() {
                // Aggregate store: source may be a pointer (SlotAddr) or a struct value (ParamRef, Call result).
                let val_is_ptr = matches!(val_ty, Some(LirType::Ptr));
                let val_is_null = null_vals.get(value.0 as usize).copied().unwrap_or(false);
                let ty_name = c_type_named(slot_ty, sn);
                if val_is_null {
                    // NullPtr → aggregate slot: zero out (e.g. None variant of Option).
                    write!(out, "memset(&{}, 0, sizeof({}));", s(*slot), ty_name).unwrap();
                } else if val_is_ptr && (slot_is_str || slot_is_gs) {
                    if *is_move {
                        // Move: transfer ownership via memcpy. The GIR MoveZero
                        // instruction will zero the source, preventing double-free.
                        write!(out, "memcpy(&{}, {}, sizeof({}));", s(*slot), v(*value), ty_name).unwrap();
                    } else {
                        // Copy: clone to prevent double-free (source stays alive).
                        write!(out, "{} = gorget_string_clone((const GorgetString*){});", s(*slot), v(*value)).unwrap();
                    }
                } else if val_is_ptr {
                    // Value is a pointer to source data — use memcpy.
                    write!(out, "memcpy(&{}, {}, sizeof({}));", s(*slot), v(*value), ty_name).unwrap();
                } else {
                    // Value is a struct by value (e.g., from ParamRef or function return).
                    // Str and GorgetString are the same 32-byte struct (unified);
                    // cross-type assigns are direct.
                    let val_is_gs = matches!(val_ty, Some(LirType::Struct(sid)) if sn.get(&sid.0).map_or(false, |n| n == "GorgetString"));
                    let val_is_str = matches!(val_ty, Some(LirType::Struct(sid)) if sn.get(&sid.0).map_or(false, |n| n == "Str"));
                    if slot_is_str && val_is_str {
                        // Both are Str views: shallow struct copy, no clone needed (cap=0, non-owning).
                        write!(out, "{} = {};", s(*slot), v(*value)).unwrap();
                    } else if (slot_is_str && val_is_gs) || (slot_is_gs && val_is_str)
                        || ((slot_is_str || slot_is_gs) && matches!(val_ty, Some(LirType::Struct(_))))
                    {
                        // Str/GorgetString by-value → string slot: direct struct assign.
                        // The source is a C local/temporary (function return, ParamRef);
                        // transferring ownership via memcpy is correct — the source won't
                        // be double-freed (C locals have no destructors, and GIR MoveZero
                        // handles source zeroing when needed).
                        write!(out, "memcpy(&{}, &{}, sizeof({}));", s(*slot), v(*value), ty_name).unwrap();
                    } else if !matches!(val_ty, Some(LirType::Struct(_)) | None) {
                        // Scalar → single-field struct coercion (newtype wrapping).
                        if let LirType::Struct(sid) = slot_ty {
                            let sdef = &module.structs[sid.0 as usize];
                            if sdef.fields.len() == 1 {
                                write!(out, "{} = ({}){{ .{} = {} }};",
                                    s(*slot), ty_name, sdef.fields[0].0, v(*value)).unwrap();
                            } else {
                                write!(out, "{} = {};", s(*slot), v(*value)).unwrap();
                            }
                        } else {
                            write!(out, "{} = {};", s(*slot), v(*value)).unwrap();
                        }
                    } else {
                        write!(out, "{} = {};", s(*slot), v(*value)).unwrap();
                    }
                }
            } else {
                // Scalar slot — check for single-field struct → scalar unwrapping (newtype).
                if let Some(LirType::Struct(val_sid)) = val_ty {
                    let sdef = &module.structs[val_sid.0 as usize];
                    if sdef.fields.len() == 1 && !matches!(slot_ty, LirType::Struct(_)) {
                        write!(out, "{} = {}.{};", s(*slot), v(*value), sdef.fields[0].0).unwrap();
                    } else {
                        write!(out, "{} = {};", s(*slot), v(*value)).unwrap();
                    }
                } else {
                    write!(out, "{} = {};", s(*slot), v(*value)).unwrap();
                }
            }
        }
        Inst::SlotLoad { dst, slot, .. } => {
            write!(out, "{} = {};", v(*dst), s(*slot)).unwrap();
        }
        Inst::SlotAddr { dst, slot } => {
            write!(out, "{} = &{};", v(*dst), s(*slot)).unwrap();
        }

        // Constants
        Inst::IConst { dst, value, ty } => {
            write!(out, "{} = ({}){}LL;", v(*dst), c_type_named(ty, sn), value).unwrap();
        }
        Inst::FConst { dst, bits, ty } => {
            let val = f64::from_bits(*bits);
            write!(out, "{} = ({})({});", v(*dst), c_type_named(ty, sn), format_float(val)).unwrap();
        }
        Inst::BoolConst { dst, value } => {
            write!(out, "{} = {};", v(*dst), if *value { "true" } else { "false" }).unwrap();
        }
        Inst::NullPtr { dst } => {
            write!(out, "{} = NULL;", v(*dst)).unwrap();
        }
        Inst::FuncAddr { dst, func } => {
            let name = c_func_name(&module.functions[func.0 as usize].name);
            let adapt_name = format!("__adapt_{}", name);
            // Emit as a static 2-element closure array {adapter_fn, NULL} so that
            // callable dispatch ((void**)cv)[0] / ((void**)cv)[1] works correctly.
            // The adapter ignores the env pointer and forwards to the real function.
            write!(out, "{{ static void* __fa_{}[] = {{ (void*){}, NULL }}; {} = (void*)__fa_{}; }}",
                dst.0, adapt_name, v(*dst), dst.0).unwrap();
        }
        Inst::GlobalAddr { dst, global } => {
            write!(out, "{} = &__lir_g{};", v(*dst), global.0).unwrap();
        }
        Inst::StrLit { dst, value } => {
            let escaped = escape_c_string(value);
            write!(out, "{} = \"{}\";", v(*dst), escaped).unwrap();
        }
        Inst::ParamRef { dst, index, .. } => {
            if func.const_params.get(*index as usize) == Some(&true) {
                write!(out, "{} = (void*)__p{};", v(*dst), index).unwrap();
            } else {
                write!(out, "{} = __p{};", v(*dst), index).unwrap();
            }
        }

        // Arithmetic
        Inst::Add { dst, ty, lhs, rhs, overflow } => {
            if *overflow == Overflow::Trap && matches!(ty, LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8) {
                let ct = c_type_named(ty, sn);
                write!(out, "if (__builtin_add_overflow(({ct}){l}, ({ct}){r}, &{d})) {{ fprintf(stderr, \"gorget: integer overflow\\n\"); exit(1); }}",
                    d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            } else {
                let ct = c_type_named(ty, sn);
                write!(out, "{d} = ({ct}){l} + ({ct}){r};", d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            }
        }
        Inst::Sub { dst, ty, lhs, rhs, overflow } => {
            if *overflow == Overflow::Trap && matches!(ty, LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8) {
                let ct = c_type_named(ty, sn);
                write!(out, "if (__builtin_sub_overflow(({ct}){l}, ({ct}){r}, &{d})) {{ fprintf(stderr, \"gorget: integer overflow\\n\"); exit(1); }}",
                    d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            } else {
                let ct = c_type_named(ty, sn);
                write!(out, "{d} = ({ct}){l} - ({ct}){r};", d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            }
        }
        Inst::Mul { dst, ty, lhs, rhs, overflow } => {
            if *overflow == Overflow::Trap && matches!(ty, LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8) {
                let ct = c_type_named(ty, sn);
                write!(out, "if (__builtin_mul_overflow(({ct}){l}, ({ct}){r}, &{d})) {{ fprintf(stderr, \"gorget: integer overflow\\n\"); exit(1); }}",
                    d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            } else {
                let ct = c_type_named(ty, sn);
                write!(out, "{d} = ({ct}){l} * ({ct}){r};", d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            }
        }
        Inst::Div { dst, ty, lhs, rhs } => {
            if matches!(ty, LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8) {
                let ct = c_type_named(ty, sn);
                write!(out, "if (({ct}){r} == 0) {{ fprintf(stderr, \"gorget: division by zero\\n\"); exit(1); }} {d} = ({ct}){l} / ({ct}){r};",
                    d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            } else {
                write!(out, "{} = {} / {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
            }
        }
        Inst::Rem { dst, ty, lhs, rhs, .. } => {
            if matches!(ty, LirType::F32 | LirType::F64) {
                write!(out, "{} = fmod({}, {});", v(*dst), v(*lhs), v(*rhs)).unwrap();
            } else {
                let ct = c_type_named(ty, sn);
                write!(out, "{d} = ({ct}){l} % ({ct}){r};", d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            }
        }
        Inst::Mod { dst, ty, lhs, rhs, .. } => {
            if matches!(ty, LirType::F32 | LirType::F64) {
                // Python-style float modulo: fmod(a,b) + (result has different sign from b ? b : 0)
                write!(
                    out,
                    "{{ double __t = fmod({l}, {r}); {d} = __t + (__t != 0.0 && ((__t < 0) != ({r} < 0)) ? {r} : 0.0); }}",
                    d = v(*dst), l = v(*lhs), r = v(*rhs)
                ).unwrap();
            } else {
                // Python-style integer modulo
                write!(
                    out,
                    "{{ typeof({l}) __t = {l} % {r}; {d} = __t + (__t != 0 && (__t ^ {r}) < 0 ? {r} : 0); }}",
                    d = v(*dst), l = v(*lhs), r = v(*rhs)
                ).unwrap();
            }
        }
        Inst::Neg { dst, operand, .. } => {
            write!(out, "{} = -{};", v(*dst), v(*operand)).unwrap();
        }

        // Bitwise
        Inst::BitAnd { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} & {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }
        Inst::BitOr { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} | {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }
        Inst::BitXor { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} ^ {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }
        Inst::BitNot { dst, operand, .. } => {
            write!(out, "{} = ~{};", v(*dst), v(*operand)).unwrap();
        }
        Inst::Shl { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} << {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }
        Inst::Shr { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} >> {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }

        // Comparison & logic
        Inst::Cmp { dst, op, lhs, rhs } => {
            // Detect Str-typed operands for string comparison.
            let is_str = |vid: &ValueId| -> bool {
                if str_lit_vals.get(vid.0 as usize).copied().unwrap_or(false) { return true; }
                if let Some(Some(pt)) = ptr_pointee.get(vid.0 as usize) {
                    if let LirType::Struct(sid) = pt {
                        let name = sn.get(&sid.0).map(|s| s.as_str()).unwrap_or("");
                        if name == "Str" || name == "GorgetString" { return true; }
                    }
                }
                if let Some(Some(LirType::Struct(sid))) = val_types.get(vid.0 as usize) {
                    let name = sn.get(&sid.0).map(|s| s.as_str()).unwrap_or("");
                    if name == "Str" || name == "GorgetString" { return true; }
                }
                false
            };
            let lhs_str = is_str(lhs);
            let rhs_str = is_str(rhs);
            if lhs_str || rhs_str {
                // String comparison — wrap operands into Str values for gorget_str_eq/gorget_str_cmp.
                let wrap = |vid: &ValueId| -> String {
                    if str_lit_vals.get(vid.0 as usize).copied().unwrap_or(false) {
                        format!("gorget_str_from_literal({v}, strlen({v}))", v = v(*vid))
                    } else if let Some(Some(pt)) = ptr_pointee.get(vid.0 as usize) {
                        if pt.is_aggregate() {
                            // Pointer to Str slot — dereference.
                            format!("(*(Str*){v})", v = v(*vid))
                        } else {
                            v(*vid)
                        }
                    } else {
                        v(*vid)
                    }
                };
                let lhs_c = wrap(lhs);
                let rhs_c = wrap(rhs);
                match op {
                    CmpOp::Eq => write!(out, "{} = gorget_str_eq({}, {});", v(*dst), lhs_c, rhs_c).unwrap(),
                    CmpOp::Ne => write!(out, "{} = !gorget_str_eq({}, {});", v(*dst), lhs_c, rhs_c).unwrap(),
                    _ => {
                        let c_op = match op {
                            CmpOp::Lt => "<",
                            CmpOp::Le => "<=",
                            CmpOp::Gt => ">",
                            CmpOp::Ge => ">=",
                            _ => unreachable!(),
                        };
                        write!(out, "{} = gorget_str_cmp({}, {}) {} 0;", v(*dst), lhs_c, rhs_c, c_op).unwrap();
                    }
                }
            } else {
                let c_op = match op {
                    CmpOp::Eq => "==",
                    CmpOp::Ne => "!=",
                    CmpOp::Lt => "<",
                    CmpOp::Le => "<=",
                    CmpOp::Gt => ">",
                    CmpOp::Ge => ">=",
                };
                write!(out, "{} = {} {} {};", v(*dst), v(*lhs), c_op, v(*rhs)).unwrap();
            }
        }
        Inst::Not { dst, operand } => {
            write!(out, "{} = !{};", v(*dst), v(*operand)).unwrap();
        }

        // Type conversions
        Inst::IntCast { dst, value, to } => {
            // GorgetStringView → int: extract the first byte (ASCII codepoint), not cast the pointer.
            let src_is_str_ptr = ptr_pointee.get(value.0 as usize)
                .and_then(|t| t.as_ref())
                .map_or(false, |t| matches!(t, LirType::Struct(sid) if {
                    module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetStringView")
                }));
            let src_is_str_val = !src_is_str_ptr && val_types.get(value.0 as usize)
                .and_then(|t| t.as_ref())
                .map_or(false, |t| matches!(t, LirType::Struct(sid) if {
                    module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetStringView")
                }));
            if src_is_str_ptr {
                // Value is a pointer to Str — cast to Str* and extract first byte.
                write!(out, "{} = ({})((uint8_t)((Str *)({}))->data[0]);", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
            } else if src_is_str_val {
                // Value is a Str struct by value — extract first byte directly.
                write!(out, "{} = ({})((uint8_t){}.data[0]);", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
            } else {
                write!(out, "{} = ({})({});", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
            }
        }
        Inst::FloatCast { dst, value, to } => {
            write!(out, "{} = ({})({});", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
        }
        Inst::IntToFloat { dst, value, to } => {
            write!(out, "{} = ({})({});", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
        }
        Inst::FloatToInt { dst, value, to } => {
            write!(out, "{} = ({})({});", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
        }
        Inst::PtrCast { dst, value } => {
            write!(out, "{} = (void*)({});", v(*dst), v(*value)).unwrap();
        }
        Inst::Bitcast { dst, value, to } => {
            // Use memcpy for type-punning to avoid strict aliasing violations.
            write!(
                out,
                "memcpy(&{d}, &{s}, sizeof({t}));",
                d = v(*dst),
                s = v(*value),
                t = c_type_named(to, sn)
            ).unwrap();
        }

        // Memory
        Inst::Load { dst, ptr, ty } => {
            // Load from a pointer — always shallow deref.  The GIR drop elaborator
            // already determines ownership: if the loaded value needs freeing, it
            // emits a Drop/MoveZero.  Cloning resource types here would leak if no
            // corresponding Drop exists (which is the common case for collection
            // element reads — the collection owns the data).
            write!(out, "{} = *({} *)({});", v(*dst), c_type_named(ty, sn), v(*ptr)).unwrap();
        }
        Inst::Store { ptr, value } => {
            // Generic store — type is determined by context.
            let val_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
            let is_str_lit = str_lit_vals.get(value.0 as usize).copied().unwrap_or(false);
            if is_str_lit {
                // String literal → wrap into Str and store.
                write!(out, "*(Str*)({p}) = gorget_str_from_literal({val}, strlen({val}));",
                    p = v(*ptr), val = v(*value)).unwrap();
            } else if matches!(val_ty, Some(LirType::Ptr)) {
                // Source is a pointer — either an aggregate reference (memcpy) or a raw pointer value (direct store).
                let val_is_null = null_vals.get(value.0 as usize).copied().unwrap_or(false);
                let pointee = ptr_pointee.get(value.0 as usize).and_then(|t| t.as_ref());
                let dst_pointee = ptr_pointee.get(ptr.0 as usize).and_then(|t| t.as_ref());
                if val_is_null {
                    // NullPtr → zero out destination (e.g. None variant of nested Option).
                    let size_ty = pointee.or(dst_pointee);
                    if let Some(ty) = size_ty {
                        let ty_name = c_type_named(ty, sn);
                        write!(out, "memset({p}, 0, sizeof({ty_name}));", p = v(*ptr)).unwrap();
                    } else {
                        write!(out, "*(void**)({p}) = NULL;", p = v(*ptr)).unwrap();
                    }
                // If the destination field/slot itself holds a pointer (Ptr/Void), store the pointer
                // value directly — don't memcpy through it.  This happens for MutRef captures
                // (void* fields holding &outer_var).
                } else if matches!(dst_pointee, Some(LirType::Ptr) | Some(LirType::Void)) {
                    write!(out, "*(void**)({p}) = {val};", p = v(*ptr), val = v(*value)).unwrap();
                } else {
                    // Str and GorgetString are the same 32-byte struct (unified).
                    // Cross-type stores are just memcpy.
                    {
                        // Prefer destination pointee type for sizing (it's the allocation we write into).
                        let size_ty = dst_pointee.or(pointee);
                        if let Some(ty) = size_ty {
                            let ty_name = c_type_named(ty, sn);
                            write!(out, "memcpy({p}, {val}, sizeof({ty_name}));", p = v(*ptr), val = v(*value)).unwrap();
                        } else {
                            // Last resort — sizeof(*(val)) is wrong for void* but we have no type info.
                            write!(out, "memcpy({p}, {val}, sizeof(*({val})));", p = v(*ptr), val = v(*value)).unwrap();
                        }
                    }
                }
            } else if val_ty.map_or(false, |t| t.is_scalar()) {
                // Scalar — simple dereference store.
                let ty_name = c_type_named(val_ty.unwrap(), sn);
                write!(out, "*({ty_name}*)({p}) = {val};", p = v(*ptr), val = v(*value)).unwrap();
            } else {
                // Struct by value — take address for memcpy source.
                write!(out, "memcpy({p}, &{val}, sizeof({val}));", p = v(*ptr), val = v(*value)).unwrap();
            }
        }
        Inst::FieldPtr { dst, base, struct_id, field } => {
            let struct_def = &module.structs[struct_id.0 as usize];
            let sname = sn.get(&struct_id.0).map(|s| s.as_str()).unwrap_or("void");
            if (*field as usize) < struct_def.fields.len() {
                let field_name = &struct_def.fields[*field as usize].0;
                if struct_def.is_enum && *field > 0 {
                    // Enum union layout: access through data.field_name
                    // For multi-field variants (e.g., IFunction_0, IFunction_1),
                    // the variant name is a prefix (IFunction) and fields are
                    // inside the variant's anonymous struct.
                    let variant_prefix = field_name.rsplitn(2, '_').nth(1).unwrap_or(field_name);
                    // Check if this variant has multiple fields (needs struct access)
                    let variant_field_count = struct_def.fields[1..].iter()
                        .filter(|(n, _)| n.rsplitn(2, '_').nth(1).unwrap_or(n) == variant_prefix)
                        .count();
                    if variant_field_count > 1 {
                        // Multi-field variant: data.VariantName.field_name
                        write!(
                            out,
                            "{} = (void*)&(({} *)({}))->data.{}.{};",
                            v(*dst), sname, v(*base),
                            c_field_name(variant_prefix), c_field_name(field_name)
                        ).unwrap();
                    } else {
                        // Single-field variant: data.field_name (no variant struct)
                        write!(
                            out,
                            "{} = (void*)&(({} *)({}))->data.{};",
                            v(*dst), sname, v(*base), c_field_name(field_name)
                        ).unwrap();
                    }
                } else {
                    // Regular struct or field 0 (tag): direct access
                    write!(
                        out,
                        "{} = (void*)&(({} *)({}))->{};",
                        v(*dst),
                        sname,
                        v(*base),
                        c_field_name(field_name)
                    ).unwrap();
                }
            } else {
                // Fallback: field index exceeds struct definition — use byte offset.
                // This can happen for runtime-opaque structs (e.g., GorgetArray, Dict).
                write!(
                    out,
                    "{} = (void*)((char*)({}) + {} * sizeof(void*)); /* {}.{} (oob) */",
                    v(*dst),
                    v(*base),
                    field,
                    sname,
                    field
                ).unwrap();
            }
        }
        Inst::ElemPtr { dst, base, index, elem_size } => {
            write!(
                out,
                "{} = (void*)((char*)({}) + (int64_t)({}) * {});",
                v(*dst),
                v(*base),
                v(*index),
                elem_size
            ).unwrap();
        }
        Inst::Memset { ptr, byte, size } => {
            write!(out, "memset({}, (int){}, (size_t){});", v(*ptr), v(*byte), v(*size)).unwrap();
        }
        Inst::Memcpy { dst_ptr, src_ptr, size } => {
            write!(
                out,
                "memcpy({}, {}, (size_t){});",
                v(*dst_ptr),
                v(*src_ptr),
                v(*size)
            ).unwrap();
        }

        // Calls
        Inst::Call { dst, func, args } => {
            let target_func = &module.functions[func.0 as usize];
            let fname = c_func_name(&target_func.name);
            if let Some(d) = dst {
                write!(out, "{} = ", v(*d)).unwrap();
            }
            write!(out, "{}(", fname).unwrap();
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                // Closure→callable wrapping: when passing a __Closure_N to a void/Ptr param,
                // wrap in (void*)(void*[2]){(void*)__Closure_N__call, (void*)&env_struct}.
                // Skip for __Closure_N__call functions — they take the env pointer directly.
                let is_closure_call_fn = target_func.name.contains("__call");
                let param_is_void = !is_closure_call_fn && target_func.params.get(i)
                    .map_or(false, |p| matches!(p, LirType::Ptr | LirType::Void));
                let arg_closure_name = if param_is_void {
                    let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                    let check_closure = |sid: &StructId| -> Option<String> {
                        module.structs.get(sid.0 as usize)
                            .filter(|sd| sd.name.starts_with("__Closure_"))
                            .map(|sd| sd.name.clone())
                    };
                    if let Some(LirType::Struct(sid)) = arg_ty {
                        check_closure(sid)
                    } else if matches!(arg_ty, Some(LirType::Ptr)) {
                        ptr_pointee.get(a.0 as usize).and_then(|p| p.as_ref())
                            .and_then(|pt| if let LirType::Struct(sid) = pt { check_closure(sid) } else { None })
                    } else { None }
                } else { None };
                if let Some(closure_name) = arg_closure_name {
                    let call_fn = format!("{closure_name}__call");
                    let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                    if matches!(arg_ty, Some(LirType::Ptr)) {
                        // arg is already a pointer to the closure struct
                        write!(out, "(void*)(void*[2]){{(void*){call_fn}, (void*){}}}", v(*a)).unwrap();
                    } else {
                        // arg is the closure struct by value — take its address
                        write!(out, "(void*)(void*[2]){{(void*){call_fn}, (void*)&{}}}", v(*a)).unwrap();
                    }
                } else if param_is_void {
                    // Check if arg is a FuncAddr — needs adapter wrapping for closure protocol.
                    if let Some(fid) = func_addr_targets.get(a.0 as usize).and_then(|t| *t) {
                        let adapt_name = format!("__adapt_{}", c_func_name(&module.functions[fid.0 as usize].name));
                        write!(out, "(void*)(void*[2]){{(void*){adapt_name}, NULL}}").unwrap();
                    } else {
                        emit_coerced_arg(out, a, target_func.params.get(i), val_types, str_lit_vals, sn);
                    }
                } else {
                    emit_coerced_arg(out, a, target_func.params.get(i), val_types, str_lit_vals, sn);
                }
            }
            write!(out, ");").unwrap();
        }
        Inst::CallExtern { dst, name, args, .. } => {
            let original_name = if let Inst::CallExtern { original_name, .. } = inst { original_name } else { &None };
            let _emit_args = args;
            // ── __gorget_closure_call_N[__FUNC] — escaped closure dispatch via GorgetClosure ──
            if name.starts_with("__gorget_closure_call_") {
                let id_str = &name["__gorget_closure_call_".len()..];
                let id_num = id_str.split("__").next().unwrap_or(id_str);
                if id_num.parse::<u32>().is_ok() && !args.is_empty() {
                    let closure_val = args[0];
                    let actual_args = &args[1..];
                    let ret_type = dst.map(|d| {
                        val_types.get(d.0 as usize).and_then(|t| t.as_ref())
                            .map(|t| c_type_named(t, sn)).unwrap_or_else(|| "int64_t".to_string())
                    }).unwrap_or_else(|| "void".to_string());
                    let mut param_types = vec!["void*".to_string()];
                    for a in actual_args {
                        let ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref())
                            .map(|t| c_type_named(t, sn)).unwrap_or_else(|| "int64_t".to_string());
                        param_types.push(ty);
                    }
                    let cast = format!("{}(*)({})", ret_type, param_types.join(", "));
                    let cv = v(closure_val);
                    let cv_ty = val_types.get(closure_val.0 as usize).and_then(|t| t.as_ref());
                    let is_ptr = !matches!(cv_ty, Some(LirType::Struct(_)));
                    let (fp, ep) = if is_ptr {
                        (format!("((GorgetClosure*){cv})->fn_ptr"), format!("((GorgetClosure*){cv})->env"))
                    } else {
                        (format!("{cv}.fn_ptr"), format!("{cv}.env"))
                    };
                    if let Some(d) = dst {
                        write!(out, "{} = ", v(*d)).unwrap();
                    }
                    write!(out, "(({cast})({fp}))({ep}").unwrap();
                    for a in actual_args {
                        write!(out, ", {}", v(*a)).unwrap();
                    }
                    write!(out, ");").unwrap();
                    return;
                }
            }
            // ── __callable_N[__FUNC] — inline callable parameter dispatch via void*[2] ──
            // The callable param is void* pointing to [fn_ptr, env_ptr].
            // Dispatch: ((ret(*)(void*, args...))((void**)cv)[0])(((void**)cv)[1], args...)
            // Name format: __callable_N or __callable_N__FuncName (function-scoped).
            if name.starts_with("__callable_") {
                let id_str = &name["__callable_".len()..];
                let id_num = id_str.split("__").next().unwrap_or(id_str);
                if id_num.parse::<u32>().is_ok() && !args.is_empty() {
                    let closure_val = args[0];
                    let actual_args = &args[1..];
                    let ret_type = dst.map(|d| {
                        val_types.get(d.0 as usize).and_then(|t| t.as_ref())
                            .map(|t| c_type_named(t, sn)).unwrap_or_else(|| "int64_t".to_string())
                    }).unwrap_or_else(|| "void".to_string());
                    let mut param_types = vec!["void*".to_string()];
                    let mut deref_args: Vec<Option<String>> = Vec::new();
                    for a in actual_args {
                        let ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                        let pointee = ptr_pointee.get(a.0 as usize).and_then(|t| t.as_ref());
                        // If the arg is a pointer-to-aggregate, the callee expects by-value.
                        if let Some(pt) = pointee {
                            if pt.is_aggregate() {
                                param_types.push(c_type_named(pt, sn));
                                deref_args.push(Some(c_type_named(pt, sn)));
                                continue;
                            }
                        }
                        param_types.push(ty.map(|t| c_type_named(t, sn)).unwrap_or_else(|| "int64_t".to_string()));
                        deref_args.push(None);
                    }
                    let cast = format!("{}(*)({})", ret_type, param_types.join(", "));
                    let cv = v(closure_val);
                    if let Some(d) = dst {
                        write!(out, "{} = ", v(*d)).unwrap();
                    }
                    write!(out, "(({cast})((void**){cv})[0])(((void**){cv})[1]").unwrap();
                    for (i, a) in actual_args.iter().enumerate() {
                        if let Some(ref ty_name) = deref_args[i] {
                            write!(out, ", *({}*){}", ty_name, v(*a)).unwrap();
                        } else {
                            write!(out, ", {}", v(*a)).unwrap();
                        }
                    }
                    write!(out, ");").unwrap();
                    return;
                }
            }

            // ── Collection element drop loops ──
            // __gorget_array_drop_elems__ElemDropFn(addr) → for-loop calling ElemDropFn on each element
            if let Some(drop_fn) = name.strip_prefix("__gorget_array_drop_elems__") {
                if !args.is_empty() {
                    let arr = v(args[0]);
                    write!(out,
                        "for (size_t __di = 0; __di < ((GorgetArray*){arr})->len; __di++) {{ \
                         {drop_fn}(gorget_array_get((GorgetArray*){arr}, __di)); }}").unwrap();
                }
                return;
            }
            // __gorget_map_drop_vals__ValDropFn(addr) → for-loop calling ValDropFn on each value
            if let Some(drop_fn) = name.strip_prefix("__gorget_map_drop_vals__") {
                if !args.is_empty() {
                    let map = v(args[0]);
                    // Iterate occupied slots using the states array (1=occupied)
                    write!(out,
                        "for (size_t __di = 0; __di < ((GorgetMap*){map})->cap; __di++) {{ \
                         if (((GorgetMap*){map})->states[__di] == 1) {{ \
                         void* __val = (char*)((GorgetMap*){map})->values + __di * ((GorgetMap*){map})->val_size; \
                         {drop_fn}(__val); }} }}").unwrap();
                }
                return;
            }

            // ── Map key drops ──
            // __gorget_map_drop_keys__drop_fn(addr) → drop each key in the map
            if let Some(drop_fn) = name.strip_prefix("__gorget_map_drop_keys__") {
                if !args.is_empty() {
                    let map = v(args[0]);
                    write!(out,
                        "for (size_t __di = 0; __di < ((GorgetMap*){map})->cap; __di++) {{ \
                         if (((GorgetMap*){map})->states[__di] == 1) {{ \
                         void* __key = (char*)((GorgetMap*){map})->keys + __di * ((GorgetMap*){map})->key_size; \
                         {drop_fn}(__key); }} }}").unwrap();
                }
                return;
            }

            // ── Recipe-based compound element drop ──
            // __gorget_array_drop_recipe__TypeName(addr) → compound drop per element
            if let Some(type_name) = name.strip_prefix("__gorget_array_drop_recipe__") {
                if !args.is_empty() {
                    if let Some(recipe) = module.elem_drop_recipes.get(type_name) {
                        let arr = v(args[0]);
                        emit_recipe_array_drop(out, &arr, recipe, module, sn, 0);
                    }
                }
                return;
            }
            if let Some(type_name) = name.strip_prefix("__gorget_map_drop_recipe__") {
                if !args.is_empty() {
                    if let Some(recipe) = module.elem_drop_recipes.get(type_name) {
                        let map = v(args[0]);
                        emit_recipe_map_drop(out, &map, recipe, module, sn, 0);
                    }
                }
                return;
            }

            // ── DropIfAlive guard open/close ──
            // __gorget_drop_if_alive_open__SIZE(addr) → memcmp guard opening
            if let Some(size_str) = name.strip_prefix("__gorget_drop_if_alive_open__") {
                if let Ok(byte_size) = size_str.parse::<usize>() {
                    if !args.is_empty() {
                        let addr = v(args[0]);
                        write!(out, "{{ char __dia_z[{byte_size}] = {{0}}; if (memcmp({addr}, __dia_z, {byte_size}) != 0) {{").unwrap();
                    }
                }
                return;
            }
            if name == "__gorget_drop_if_alive_close" {
                write!(out, "}} }}").unwrap();
                return;
            }

            // ── Inline string codepoint helpers (synthetic GIR functions) ──
            if name == "gorget_utf8_codepoint_len_at" && args.len() == 2 {
                // gorget_utf8_codepoint_len_at(Str s, int64_t byte_pos) → int64_t
                // Expands to: gorget_utf8_codepoint_len((unsigned char)s.data[byte_pos])
                if let Some(d) = dst {
                    let s = format!("(*(Str*){})", v(args[0]));
                    write!(out, "{} = gorget_utf8_codepoint_len((unsigned char){s}.data[{}]);",
                        v(*d), v(args[1])).unwrap();
                }
                return;
            }
            if name == "gorget_str_codepoint_at" && args.len() == 2 {
                // gorget_str_codepoint_at(Str s, int64_t byte_pos) → Str
                // Expands to: (Str){ .data = s.data + byte_pos, .len = cplen }
                if let Some(d) = dst {
                    let s = format!("(*(Str*){})", v(args[0]));
                    let pos = v(args[1]);
                    write!(out, "{} = (Str){{ .data = {s}.data + {pos}, .len = (size_t)gorget_utf8_codepoint_len((unsigned char){s}.data[{pos}]), .cap = 0, .alloc = NULL }};",
                        v(*d)).unwrap();
                }
                return;
            }

            // ── Inline Option/Result helpers ────────────────────────────
            // These are pseudo-functions emitted by the GIR; they operate
            // on a pointer to an Option/Result struct.  The tag field is
            // always `int32_t` at offset 0; payload fields follow.
            if (name == "__option_is_some" || name == "__option_is_none"
                || name.ends_with("__is_some") || name.ends_with("__is_ok"))
                && !args.is_empty()
            {
                if let Some(d) = dst {
                    let op = if name.contains("is_some") || name.contains("is_ok") { "==" } else { "!=" };
                    write!(out, "{} = (*(int32_t*){}) {op} 0;", v(*d), v(args[0])).unwrap();
                }
                return;
            }
            if (name == "__option_is_none"
                || name.ends_with("__is_none") || name.ends_with("__is_err"))
                && !args.is_empty()
            {
                if let Some(d) = dst {
                    write!(out, "{} = (*(int32_t*){}) != 0;", v(*d), v(args[0])).unwrap();
                }
                return;
            }
            // ── Option/Result combinator inline expansion ──
            // map, filter, and_then, or_else are inlined at each call site
            // because the same GIR name (e.g., Option__int64_t__map) may be
            // used with different closure types (same-type and cross-type map).
            if let Some((type_prefix, method)) = parse_option_result_combinator(name) {
                if let Some(d) = dst {
                    // Source enum type from name prefix.
                    let src_ty = find_struct_c_name_by_prefix(type_prefix, module, sn)
                        .unwrap_or_else(|| type_prefix.to_string());
                    // Find the closure __call function by looking at the pointee type
                    // of the closure arg (second arg is a SlotAddr of the closure struct).
                    let call_fn = if args.len() > 1 {
                        let closure_struct = ptr_pointee.get(args[1].0 as usize)
                            .and_then(|t| t.as_ref())
                            .map(|t| c_type_named(t, sn))
                            .or_else(|| {
                                val_types.get(args[1].0 as usize)
                                    .and_then(|t| t.as_ref())
                                    .filter(|t| matches!(t, LirType::Struct(_)))
                                    .map(|t| c_type_named(t, sn))
                            });
                        closure_struct.map(|n| find_closure_call_fn(module, &n, sn))
                            .unwrap_or_default()
                    } else {
                        String::new()
                    };

                    // Determine result type for map: wraps the closure's return type.
                    // For same-type map (int→int), result == source.
                    // For cross-type map (int→str), find the Option__<ret> struct.
                    let result_ty = if method == "map" && !call_fn.is_empty() {
                        closure_call_return_type(module, &call_fn, sn)
                            .and_then(|ret_ty| {
                                if name.starts_with("Result__") {
                                    // For Result map, the error type stays the same.
                                    // Extract error type from source struct to build target name.
                                    let err_ty = module.structs.iter().find(|s| s.name == type_prefix)
                                        .and_then(|s| s.fields.get(2))
                                        .map(|(_, t)| c_type_named(t, sn));
                                    if let Some(err_c) = err_ty {
                                        let ret_m = type_name_to_monomorphized(&ret_ty);
                                        let err_m = type_name_to_monomorphized(&err_c);
                                        let target = format!("Result__{ret_m}__{err_m}");
                                        find_struct_c_name_by_prefix(&target, module, sn)
                                    } else {
                                        let ret_m = type_name_to_monomorphized(&ret_ty);
                                        let target = format!("Result__{ret_m}");
                                        find_struct_c_name_by_prefix(&target, module, sn)
                                    }
                                } else {
                                    let ret_m = type_name_to_monomorphized(&ret_ty);
                                    let target = format!("Option__{ret_m}");
                                    find_struct_c_name_by_prefix(&target, module, sn)
                                }
                            })
                            .unwrap_or_else(|| src_ty.clone())
                    } else if method == "flatten" {
                        // flatten: result type is the payload type of the outer Option
                        module.structs.iter().find(|s| s.name == type_prefix)
                            .and_then(|s| s.fields.get(1))
                            .map(|(_, t)| c_type_named(t, sn))
                            .unwrap_or_else(|| src_ty.clone())
                    } else {
                        // Non-map combinators: result type == source type
                        // (and_then's closure already returns the full Option/Result)
                        src_ty.clone()
                    };

                    let opt_ptr = v(args[0]);
                    let closure_v = if args.len() > 1 { v(args[1]) } else { String::new() };
                    let (ok_f, err_f) = enum_payload_fields(type_prefix, module);

                    match method {
                        "map" => {
                            // For map result type, also look up the result struct's ok field
                            let result_ok = if name.starts_with("Result__") {
                                // Result map result type prefix may differ from source
                                let rp = name.rsplitn(2, "__").nth(1).unwrap_or(name);
                                enum_payload_fields(rp, module).0
                            } else {
                                ok_f.clone()
                            };
                            // For Result types, the Error branch must copy the error payload
                            let err_copy = if name.starts_with("Result__") {
                                format!(" __om_r.{err_f} = __om_src.{err_f};")
                            } else { String::new() };
                            if result_ty != src_ty {
                                // Cross-type map: result type differs from source. Use block + memcpy
                                // because the destination variable may be declared as the source type.
                                write!(out, "{{ {result_ty} __om_r; {src_ty} __om_src = *({src_ty}*){opt_ptr}; \
                                    if (__om_src.tag == 0) {{ __om_r.tag = 0; __om_r.{result_ok} = {call_fn}({closure_v}, __om_src.{ok_f}); }} \
                                    else {{ __om_r.tag = 1;{err_copy} }} memcpy(&{dv}, &__om_r, sizeof({result_ty})); }}",
                                    dv = v(*d)).unwrap();
                            } else {
                                write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; {result_ty} __om_r; \
                                    if (__om_src.tag == 0) {{ __om_r.tag = 0; __om_r.{result_ok} = {call_fn}({closure_v}, __om_src.{ok_f}); }} \
                                    else {{ __om_r.tag = 1;{err_copy} }} __om_r; }});",
                                    v(*d)).unwrap();
                            }
                        }
                        "filter" => {
                            write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; {src_ty} __om_r; \
                                if (__om_src.tag == 0 && {call_fn}({closure_v}, __om_src.{ok_f})) {{ __om_r = __om_src; }} \
                                else {{ __om_r = ({src_ty}){{ .tag = 1 }}; }} __om_r; }});",
                                v(*d)).unwrap();
                        }
                        "and_then" => {
                            // For Result, copy the error payload in the else branch
                            if name.starts_with("Result__") {
                                write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; {result_ty} __om_r; \
                                    if (__om_src.tag == 0) {{ __om_r = {call_fn}({closure_v}, __om_src.{ok_f}); }} \
                                    else {{ __om_r.tag = 1; __om_r.{err_f} = __om_src.{err_f}; }} __om_r; }});",
                                    v(*d)).unwrap();
                            } else {
                                write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; {result_ty} __om_r; \
                                    if (__om_src.tag == 0) {{ __om_r = {call_fn}({closure_v}, __om_src.{ok_f}); }} \
                                    else {{ __om_r = ({result_ty}){{ .tag = 1 }}; }} __om_r; }});",
                                    v(*d)).unwrap();
                            }
                        }
                        "or_else" => {
                            // Result or_else passes error to closure; Option or_else doesn't
                            if name.starts_with("Result__") {
                                write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; {src_ty} __om_r; \
                                    if (__om_src.tag == 0) {{ __om_r = __om_src; }} \
                                    else {{ __om_r = {call_fn}({closure_v}, __om_src.{err_f}); }} __om_r; }});",
                                    v(*d)).unwrap();
                            } else {
                                write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; {src_ty} __om_r; \
                                    if (__om_src.tag == 0) {{ __om_r = __om_src; }} \
                                    else {{ __om_r = {call_fn}({closure_v}); }} __om_r; }});",
                                    v(*d)).unwrap();
                            }
                        }
                        "unwrap_err" | "unwrap_error" => {
                            write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; \
                                if (__om_src.tag != 1) {{ fprintf(stderr, \"unwrap_error on Ok\\n\"); abort(); }} \
                                __om_src.{err_f}; }});",
                                v(*d)).unwrap();
                        }
                        "map_err" => {
                            // map_err may change the error type (cross-type error mapping)
                            let map_err_result = if !call_fn.is_empty() {
                                closure_call_return_type(module, &call_fn, sn)
                                    .and_then(|ret_ty| {
                                        let ok_ty = module.structs.iter().find(|s| s.name == type_prefix)
                                            .and_then(|s| s.fields.get(1))
                                            .map(|(_, t)| c_type_named(t, sn))?;
                                        let target = format!("Result__{ok_ty}__{ret_ty}");
                                        find_struct_c_name_by_prefix(&target, module, sn)
                                    })
                            } else { None };
                            let me_result = map_err_result.as_deref().unwrap_or(&src_ty);
                            let me_err_f = if me_result != src_ty {
                                // Find the err field of the result type
                                module.structs.iter().find(|s| {
                                    let c = sn.get(&(module.structs.iter().position(|x| std::ptr::eq(x, *s)).unwrap() as u32))
                                        .cloned().unwrap_or_else(|| s.name.clone());
                                    c == *me_result
                                }).and_then(|s| s.fields.get(2))
                                    .map(|(n, _)| c_field_name(n))
                                    .unwrap_or_else(|| err_f.clone())
                            } else { err_f.clone() };
                            if me_result != src_ty {
                                write!(out, "{{ {me_result} __om_r; {src_ty} __om_src = *({src_ty}*){opt_ptr}; \
                                    if (__om_src.tag == 0) {{ __om_r.tag = 0; memcpy((char*)&__om_r + sizeof(int32_t), (char*)&__om_src + sizeof(int32_t), sizeof(__om_src.{ok_f})); }} \
                                    else {{ __om_r.tag = 1; {{ __auto_type __me_val = {call_fn}({closure_v}, __om_src.{err_f}); memcpy(&__om_r.{me_err_f}, &__me_val, sizeof(__me_val)); }} }} memcpy(&{dv}, &__om_r, sizeof({me_result})); }}",
                                    dv = v(*d)).unwrap();
                            } else {
                                write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; {src_ty} __om_r; \
                                    if (__om_src.tag == 0) {{ __om_r = __om_src; }} \
                                    else {{ __om_r.tag = 1; {{ __auto_type __me_val = {call_fn}({closure_v}, __om_src.{err_f}); memcpy(&__om_r.{err_f}, &__me_val, sizeof(__me_val)); }} }} __om_r; }});",
                                    v(*d)).unwrap();
                            }
                        }
                        "or" => {
                            // or takes a second Option value (passed as pointer)
                            let other_v = if args.len() > 1 { v(args[1]) } else { String::new() };
                            let other_is_null = args.get(1).map_or(false, |a| null_vals.get(a.0 as usize).copied().unwrap_or(false));
                            if other_is_null {
                                // "or(None)" → if self is Some, return self, else return None
                                write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; \
                                    (__om_src.tag == 0) ? __om_src : ({src_ty}){{ .tag = 1 }}; }});",
                                    v(*d)).unwrap();
                            } else {
                                write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; \
                                    (__om_src.tag == 0) ? __om_src : *({src_ty}*){other_v}; }});",
                                    v(*d)).unwrap();
                            }
                        }
                        "flatten" => {
                            write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; \
                                (__om_src.tag == 0) ? __om_src.{ok_f} : ({result_ty}){{ .tag = 1 }}; }});",
                                v(*d)).unwrap();
                        }
                        "unwrap_or_else" => {
                            // unwrap_or_else: if Some/Ok return payload, else call closure
                            if name.starts_with("Result__") {
                                write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; \
                                    (__om_src.tag == 0) ? __om_src.{ok_f} : {call_fn}({closure_v}, __om_src.{err_f}); }});",
                                    v(*d)).unwrap();
                            } else {
                                write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; \
                                    (__om_src.tag == 0) ? __om_src.{ok_f} : {call_fn}({closure_v}); }});",
                                    v(*d)).unwrap();
                            }
                        }
                        "flat_map" => {
                            // flat_map on Option: if Some, call closure; else None
                            write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; {result_ty} __om_r; \
                                if (__om_src.tag == 0) {{ __om_r = {call_fn}({closure_v}, __om_src.{ok_f}); }} \
                                else {{ __om_r = ({result_ty}){{ .tag = 1 }}; }} __om_r; }});",
                                v(*d)).unwrap();
                        }
                        _ => {
                            write!(out, "/* TODO: combinator {name} */").unwrap();
                        }
                    }
                }
                return;
            }
            // ── Newtype constructors ────────────────────────────
            // If the extern name matches a struct name, emit a compound literal
            // instead of a function call: (StructType){ ._0 = arg }.
            if let Some(d) = dst {
                let is_newtype_ctor = module.structs.iter().enumerate().any(|(i, s)| {
                    let cname = sn.get(&(i as u32)).map(|s| s.as_str()).unwrap_or(&s.name);
                    // Match by original name (the extern uses the original struct name).
                    s.name == *name && s.fields.len() == 1
                        || cname == name && s.fields.len() == 1
                });
                if is_newtype_ctor && args.len() == 1 {
                    let struct_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                    let ty_name = struct_ty.map(|t| c_type_named(t, sn))
                        .unwrap_or_else(|| name.to_string());
                    let arg_val = &args[0];
                    write!(out, "{} = ({ty_name}){{ ._0 = {} }};", v(*d), v(*arg_val)).unwrap();
                    return;
                }
            }

            // __option_unwrap / __result_unwrap / gorget_option_unwrap
            // Option__T__unwrap / Result__T__S__unwrap
            if is_option_result_unwrap(name) && !args.is_empty() {
                if let Some(d) = dst {
                    let dst_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                    // When dst_ty is Ptr/void*, try to recover the actual payload type
                    // from the struct definition. The arg points to an Option/Result struct
                    // whose payload field (index 1 = Ok/Some) gives us the real type.
                    let is_unwrap_err = name.contains("unwrap_err") || name.contains("unwrap_error");
                    let payload_field_idx: usize = if is_unwrap_err { 2 } else { 1 };
                    let recovered_ty = if matches!(dst_ty, Some(LirType::Ptr) | None) {
                        // Strategy 1: Extract type prefix from fn name
                        // Option__T__unwrap → Option__T, Result__T__S__unwrap → Result__T__S
                        let struct_prefix = if is_unwrap_err {
                            None // handled separately
                        } else {
                            name.rsplitn(2, "__").nth(1)
                        };
                        let from_name = struct_prefix.and_then(|prefix| {
                            module.structs.iter().enumerate().find(|(i, s)| {
                                let cname = sn.get(&(*i as u32)).map(|s| s.as_str()).unwrap_or(&s.name);
                                s.name.starts_with(prefix) && s.name.len() == prefix.len()
                                    || cname.starts_with(prefix) && cname.len() == prefix.len()
                            }).and_then(|(_, s)| {
                                s.fields.get(payload_field_idx).map(|(_, t)| t.clone())
                            })
                        });
                        // Strategy 2: If name-based lookup failed (e.g. __option_unwrap),
                        // look at the arg's val_type or ptr_pointee — the arg is usually a
                        // pointer (SlotAddr) to the Option/Result struct.
                        from_name.or_else(|| {
                            let arg0 = args[0].0 as usize;
                            // Try direct struct type
                            let from_val = val_types.get(arg0).and_then(|t| t.as_ref()).and_then(|arg_ty| {
                                match arg_ty {
                                    LirType::Struct(sid) => {
                                        module.structs.get(sid.0 as usize).and_then(|s| {
                                            s.fields.get(payload_field_idx).map(|(_, t)| t.clone())
                                        })
                                    }
                                    _ => None,
                                }
                            });
                            // Fall back to pointee type (arg is pointer to struct)
                            from_val.or_else(|| {
                                ptr_pointee.get(arg0).and_then(|t| t.as_ref()).and_then(|pt| {
                                    match pt {
                                        LirType::Struct(sid) => {
                                            module.structs.get(sid.0 as usize).and_then(|s| {
                                                s.fields.get(payload_field_idx).map(|(_, t)| t.clone())
                                            })
                                        }
                                        _ => None,
                                    }
                                })
                            })
                        })
                    } else {
                        None
                    };
                    // Check if the payload field is Ptr (T & reference from collection read).
                    // If so, the extracted value is a pointer — use void* regardless of dst type.
                    let payload_is_ptr = recovered_ty.as_ref().map_or(false, |t| matches!(t, LirType::Ptr))
                        || {
                            // Also check the struct definition directly when recovery didn't fire
                            let arg0_idx2 = args[0].0 as usize;
                            let check_struct = |sid: StructId| -> bool {
                                module.structs.get(sid.0 as usize)
                                    .and_then(|s| s.fields.get(payload_field_idx))
                                    .map_or(false, |(_, t)| matches!(t, LirType::Ptr))
                            };
                            val_types.get(arg0_idx2).and_then(|t| t.as_ref()).map_or(false, |t| {
                                matches!(t, LirType::Struct(sid) if check_struct(*sid))
                            }) || ptr_pointee.get(arg0_idx2).and_then(|t| t.as_ref()).map_or(false, |t| {
                                matches!(t, LirType::Struct(sid) if check_struct(*sid))
                            })
                        };
                    let effective_ty = if payload_is_ptr {
                        Some(&LirType::Ptr)
                    } else {
                        recovered_ty.as_ref().or(dst_ty)
                    };
                    let ty_name = effective_ty.map(|t| c_type_named(t, sn))
                        .unwrap_or_else(|| "int64_t".to_string());
                    // Determine field access for the payload. Use named struct field access
                    // instead of byte offsets to avoid alignment mismatches.
                    // Try to resolve struct type and field name from the arg.
                    let arg0_idx = args[0].0 as usize;
                    let arg_struct = val_types.get(arg0_idx).and_then(|t| t.as_ref()).and_then(|t| {
                        if let LirType::Struct(sid) = t { Some(*sid) } else { None }
                    }).or_else(|| {
                        ptr_pointee.get(arg0_idx).and_then(|t| t.as_ref()).and_then(|t| {
                            if let LirType::Struct(sid) = t { Some(*sid) } else { None }
                        })
                    });
                    let payload_access = arg_struct.and_then(|sid| {
                        let sdef = module.structs.get(sid.0 as usize)?;
                        let (field_name, _) = sdef.fields.get(payload_field_idx)?;
                        let struct_name = sn.get(&sid.0).map(|s| s.as_str()).unwrap_or(&sdef.name);
                        Some(format!("(({struct_name}*){})->{}", v(args[0]), c_field_name(field_name)))
                    });
                    // Fallback: byte offset (legacy path for cases where struct info unavailable)
                    let payload_align = match effective_ty {
                        Some(LirType::I8 | LirType::U8 | LirType::I16 | LirType::U16 | LirType::I32 | LirType::U32 | LirType::Bool) => 4,
                        _ => 8,
                    };
                    let payload_expr = payload_access.unwrap_or_else(|| {
                        format!("*({ty_name}*)((char*){} + {payload_align})", v(args[0]))
                    });
                    if name.contains("unwrap_or") && args.len() > 1 {
                        // Use the default arg's type if the dst type is Ptr/void* to avoid
                        // type mismatch in ternary (e.g. void* vs double).
                        let default_ty = val_types.get(args[1].0 as usize).and_then(|t| t.as_ref());
                        let needs_type_fixup = matches!(effective_ty, Some(LirType::Ptr) | None) && default_ty.is_some()
                            && !matches!(default_ty, Some(LirType::Ptr));
                        // Reverse fixup: dst is concrete struct but default is Ptr.
                        // Cast the default to the concrete type so ternary branches match.
                        let reverse_fixup = !needs_type_fixup
                            && !matches!(effective_ty, Some(LirType::Ptr) | None)
                            && matches!(default_ty, Some(LirType::Ptr) | None);
                        let ternary_ty = if needs_type_fixup {
                            c_type_named(default_ty.unwrap(), sn)
                        } else {
                            ty_name.clone()
                        };
                        if needs_type_fixup {
                            write!(out, "{{ {ternary_ty} __uw = (*(int32_t*){} == 0) ? ({ternary_ty}){payload_expr} : {}; memcpy(&{}, &__uw, sizeof(__uw)); }}",
                                v(args[0]), v(args[1]), v(*d)).unwrap();
                        } else if reverse_fixup {
                            write!(out, "{} = (*(int32_t*){} == 0) ? ({ternary_ty}){payload_expr} : *({ternary_ty}*)&{};",
                                v(*d), v(args[0]), v(args[1])).unwrap();
                        } else {
                            write!(out, "{} = (*(int32_t*){} == 0) ? ({ternary_ty}){payload_expr} : {};",
                                v(*d), v(args[0]), v(args[1])).unwrap();
                        }
                    } else {
                        if recovered_ty.is_some() && matches!(dst_ty, Some(LirType::Ptr) | None) {
                            write!(out, "{{ {ty_name} __uw = ({ty_name}){payload_expr}; memcpy(&{}, &__uw, sizeof(__uw)); }}",
                                v(*d)).unwrap();
                        } else {
                            write!(out, "{} = ({ty_name}){payload_expr};",
                                v(*d)).unwrap();
                        }
                    }
                    // Note: cloning of Recursive/Custom-drop types is handled at the
                    // GIR level (clone_fn_for_ptr in unwrap lowering), not here.
                    // The GIR emits an explicit clone call tracked for drop.
                }
                return;
            }
            // __option_expect / Result__T__S__expect — same as unwrap
            if is_option_result_expect(name) && !args.is_empty() {
                if let Some(d) = dst {
                    let dst_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                    // Recover type from arg (val_type or pointee) if dst is void*
                    let recovered_ty_e = if matches!(dst_ty, Some(LirType::Ptr) | None) {
                        let arg0 = args[0].0 as usize;
                        let from_val = val_types.get(arg0).and_then(|t| t.as_ref()).and_then(|arg_ty| {
                            match arg_ty {
                                LirType::Struct(sid) => {
                                    module.structs.get(sid.0 as usize).and_then(|s| {
                                        s.fields.get(1).map(|(_, t)| t.clone())
                                    })
                                }
                                _ => None,
                            }
                        });
                        from_val.or_else(|| {
                            ptr_pointee.get(arg0).and_then(|t| t.as_ref()).and_then(|pt| {
                                match pt {
                                    LirType::Struct(sid) => {
                                        module.structs.get(sid.0 as usize).and_then(|s| {
                                            s.fields.get(1).map(|(_, t)| t.clone())
                                        })
                                    }
                                    _ => None,
                                }
                            })
                        })
                    } else { None };
                    let effective_ty = recovered_ty_e.as_ref().or(dst_ty);
                    let ty_name = effective_ty.map(|t| c_type_named(t, sn))
                        .unwrap_or_else(|| "int64_t".to_string());
                    // Use named field access for correct layout
                    let arg0_idx_e = args[0].0 as usize;
                    let arg_struct_e = val_types.get(arg0_idx_e).and_then(|t| t.as_ref()).and_then(|t| {
                        if let LirType::Struct(sid) = t { Some(*sid) } else { None }
                    }).or_else(|| {
                        ptr_pointee.get(arg0_idx_e).and_then(|t| t.as_ref()).and_then(|t| {
                            if let LirType::Struct(sid) = t { Some(*sid) } else { None }
                        })
                    });
                    let payload_access_e = arg_struct_e.and_then(|sid| {
                        let sdef = module.structs.get(sid.0 as usize)?;
                        let (field_name, _) = sdef.fields.get(1)?; // expect always uses field 1
                        let struct_name = sn.get(&sid.0).map(|s| s.as_str()).unwrap_or(&sdef.name);
                        Some(format!("(({struct_name}*){})->{}", v(args[0]), c_field_name(field_name)))
                    });
                    let payload_align_e = match effective_ty {
                        Some(LirType::I8 | LirType::U8 | LirType::I16 | LirType::U16 | LirType::I32 | LirType::U32 | LirType::Bool) => 4,
                        _ => 8,
                    };
                    let payload_expr_e = payload_access_e.unwrap_or_else(|| {
                        format!("*({ty_name}*)((char*){} + {payload_align_e})", v(args[0]))
                    });
                    if recovered_ty_e.is_some() && matches!(dst_ty, Some(LirType::Ptr) | None) {
                        write!(out, "{{ {ty_name} __uw = ({ty_name}){payload_expr_e}; memcpy(&{}, &__uw, sizeof(__uw)); }}",
                            v(*d)).unwrap();
                    } else {
                        write!(out, "{} = ({ty_name}){payload_expr_e};",
                            v(*d)).unwrap();
                    }
                }
                return;
            }

            // ── Vector/Set/Dict constructor calls (type name without method) ──
            // Vector__int64_t(cap) → gorget_array_with_capacity(sizeof(int64_t), cap)
            // Vector__int64_t() → gorget_array_new(sizeof(int64_t))
            // Set__T(cap) / Dict__K__V(cap) → gorget_set_new(sizeof(T), cap) etc.
            if name.starts_with("Vector__") || name.starts_with("Set__") || name.starts_with("Dict__") || name.starts_with("HashMap__") || name.starts_with("HashSet__") {
                // Check if it's a constructor (name is just a type, no method suffix)
                let last_part = name.rsplit("__").next().unwrap_or("");
                if is_collection_type_constructor(last_part) {
                    emit_collection_constructor(out, name, dst, args, val_types, sn);
                    return;
                }
            }

            let is_stderr_print = name == "fprintf_stderr";
            let is_printf = name == "printf" || is_stderr_print
                || name == "gorget_string_format" || name == "gorget_string_format_alloc"
                || name == "snprintf" || name == "sprintf";
            let strip_ws_name: String;
            let view_name: String;
            let emit_name = if is_stderr_print { "fprintf" }
            else if args.len() == 1 {
                match name.as_str() {
                    "gorget_str_strip" => { strip_ws_name = "gorget_str_trim".into(); strip_ws_name.as_str() }
                    "gorget_str_lstrip" => { strip_ws_name = "gorget_str_lstrip_ws".into(); strip_ws_name.as_str() }
                    "gorget_str_rstrip" => { strip_ws_name = "gorget_str_rstrip_ws".into(); strip_ws_name.as_str() }
                    _ => name.as_str()
                }
            }
            else { name.as_str() };
            // Apply _view suffix for view-eligible string methods whose destination is Str-typed.
            let emit_name = if let Some(d) = dst {
                if view_method_vals.get(d.0 as usize).copied().unwrap_or(false) {
                    view_name = format!("{emit_name}_view");
                    view_name.as_str()
                } else { emit_name }
            } else { emit_name };

            // ── Out-parameter functions (image_load, audio_load, deflate_decompress) ──
            // These C runtime functions use void+out-param ABI but GIR calls them
            // as if they return a single Result/struct value. Rewrite to out-param form.
            if let Some(outparam_code) = try_emit_outparam_call_lir(
                emit_name, dst, args, val_types, str_lit_vals, sn, &module.structs,
            ) {
                write!(out, "{}", outparam_code).unwrap();
                return;
            }

            // ── File.create / File.open rewrite ──
            // gorget_file_create(path) → gorget_file_open(cstr, "w")
            // gorget_file_open(path) with 1 arg → gorget_file_open(cstr, "r")
            if emit_name == "gorget_file_create" && args.len() == 1 {
                let a = args[0];
                let is_str_lit = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
                let path_expr = if is_str_lit {
                    format!("{}", v(a))
                } else {
                    format!("gorget_str_to_cstr({})", v(a))
                };
                if let Some(d) = dst {
                    write!(out, "{} = gorget_file_open({}, \"w\");", v(*d), path_expr).unwrap();
                } else {
                    write!(out, "gorget_file_open({}, \"w\");", path_expr).unwrap();
                }
                return;
            }
            // gorget_file_read_all(file_ptr) → wrap result in Result<GorgetString, Str> with UTF-8 validation
            if emit_name == "gorget_file_read_all" && args.len() == 1 {
                if let Some(d) = dst {
                    let dst_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                    // If destination is a Result struct, wrap with UTF-8 validation
                    if let Some(LirType::Struct(sid)) = dst_ty {
                        let sdef = module.structs.get(sid.0 as usize);
                        let is_result = sdef.map_or(false, |s| s.name.contains("Result"));
                        if is_result {
                            let result_c = c_type_named(&LirType::Struct(*sid), sn);
                            write!(out, "{d} = ({{ GorgetString __gs = gorget_file_read_all({a}); \
                                {result_c} __wr; if (gorget_utf8_validate(__gs.data, __gs.len)) {{ \
                                __wr.tag = 0; __wr.Ok_0 = __gs; }} else {{ \
                                gorget_string_free(&__gs); __wr.tag = 1; \
                                __wr.Error_0 = gorget_str_from_literal(\"invalid UTF-8\", 13); }} __wr; }});",
                                d = v(*d), a = v(args[0])).unwrap();
                            return;
                        }
                    }
                }
            }
            if emit_name == "gorget_file_open" && args.len() == 1 {
                let a = args[0];
                let is_str_lit = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
                let path_expr = if is_str_lit {
                    format!("{}", v(a))
                } else {
                    format!("gorget_str_to_cstr({})", v(a))
                };
                if let Some(d) = dst {
                    write!(out, "{} = gorget_file_open({}, \"r\");", v(*d), path_expr).unwrap();
                } else {
                    write!(out, "gorget_file_open({}, \"r\");", path_expr).unwrap();
                }
                return;
            }

            // gorget_str_cat("", val) — str() coercion.
            // GIR represents str(int_val) as gorget_str_cat("", int_val).
            // Rewrite to gorget_int_to_str / gorget_float_to_str + wrap.
            if emit_name == "gorget_str_cat" && args.len() == 2 {
                let arg0_is_empty_str = str_lit_vals.get(args[0].0 as usize).copied().unwrap_or(false)
                    && func.blocks.iter().any(|blk| blk.insts.iter().any(|inst| {
                        matches!(inst, Inst::StrLit { dst, value } if *dst == args[0] && value.is_empty())
                    }));
                let arg1_ty = val_types.get(args[1].0 as usize).and_then(|t| t.as_ref());
                let is_arg1_int = matches!(arg1_ty, Some(LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64
                    | LirType::U8 | LirType::U16 | LirType::U32 | LirType::U64));
                let is_arg1_float = matches!(arg1_ty, Some(LirType::F32 | LirType::F64));
                let is_arg1_bool = matches!(arg1_ty, Some(LirType::Bool));
                if arg0_is_empty_str && (is_arg1_int || is_arg1_float || is_arg1_bool) {
                    let conv_fn = if is_arg1_int { "gorget_int_to_str" }
                        else if is_arg1_float { "gorget_float_to_str" }
                        else { "gorget_bool_to_str" };
                    if let Some(d) = dst {
                        // gorget_str_cat returns GorgetString — the conversion returns const char*,
                        // so wrap with gorget_string_new for GorgetString result.
                        write!(out, "{} = gorget_string_new({}({}));", v(*d), conv_fn, v(args[1])).unwrap();
                    }
                    return;
                }
            }

            // gorget_str_push / gorget_str_push_line — dispatch by arg type.
            // The GIR emits a generic `gorget_str_push(ptr, i64)` but the
            // actual runtime has type-specific variants (push_int, push_float, push_bool).
            if (emit_name == "gorget_str_push" || emit_name == "gorget_str_push_line") && args.len() == 2 {
                let arg2_ty = val_types.get(args[1].0 as usize).and_then(|t| t.as_ref());
                let is_push_line = emit_name == "gorget_str_push_line";
                let variant = match arg2_ty {
                    Some(LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64
                         | LirType::U8 | LirType::U16 | LirType::U32 | LirType::U64) =>
                        if is_push_line { Some("gorget_string_push_line_int") }
                        else { Some("gorget_string_push_int") },
                    Some(LirType::F32 | LirType::F64) =>
                        if is_push_line { Some("gorget_string_push_line_float") }
                        else { Some("gorget_string_push_float") },
                    Some(LirType::Bool) =>
                        if is_push_line { Some("gorget_string_push_line_bool") }
                        else { Some("gorget_string_push_bool") },
                    _ => None, // Str — use gorget_str_push/push_line as-is
                };
                if let Some(typed_fn) = variant {
                    write!(out, "{typed_fn}({}, {});", v(args[0]), v(args[1])).unwrap();
                    return;
                }
            }

            // time() in C requires a NULL argument.
            if name == "time" && args.is_empty() {
                if let Some(d) = dst {
                    write!(out, "{} = ", v(*d)).unwrap();
                }
                write!(out, "time(NULL);").unwrap();
                return;
            }
            // sleep(x) → gorget_sleep_ms((int64_t)(x * 1000))
            if (name == "sleep" || name == "xtd_sleep") && args.len() == 1 {
                let a = &args[0];
                let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                if matches!(arg_ty, Some(LirType::F64) | Some(LirType::F32)) {
                    write!(out, "gorget_sleep_ms((int64_t)({} * 1000));", v(*a)).unwrap();
                } else {
                    write!(out, "gorget_sleep_ms({});", v(*a)).unwrap();
                }
                return;
            }
            // ── Monomorphized parse methods ────────────────────────────
            // int8_t__parse, uint16_t__parse, double__parse, bool__parse, etc.
            // These return Option[T] but the C runtime has gorget_try_parse_int/float.
            if let Some(d) = dst {
                let dst_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                if let Some(LirType::Struct(sid)) = dst_ty {
                    let sdef = &module.structs[sid.0 as usize];
                    if sdef.name.starts_with("Option__") {
                        let opt_c = c_type_named(dst_ty.unwrap(), sn);
                        let payload_fname = sdef.fields.get(1)
                            .map(|(n, _)| c_field_name(n)).unwrap_or_else(|| "Some_0".to_string());
                        // Integer parse: int8_t__parse, uint16_t__parse, int64_t__parse, etc.
                        let is_int_parse = name.ends_with("__parse")
                            && (name.starts_with("int") || name.starts_with("uint"));
                        // Float parse: double__parse, float__parse
                        let is_float_parse = name == "double__parse" || name == "float__parse";
                        // Bool parse
                        let is_bool_parse = name == "bool__parse";
                        // Helper: coerce arg to `const char*` depending on whether it's Str, Ptr-to-Str, or already cstr
                        let coerce_arg_to_cstr = |a: ValueId| -> String {
                            let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                            let is_str_struct = matches!(arg_ty, Some(LirType::Struct(sid)) if {
                                module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetStringView" || s.name == "GorgetString")
                            });
                            let is_ptr_to_str = matches!(arg_ty, Some(LirType::Ptr)) && {
                                ptr_pointee.get(a.0 as usize).and_then(|t| t.as_ref()).map_or(false, |t| {
                                    matches!(t, LirType::Struct(sid) if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetStringView" || s.name == "GorgetString"))
                                })
                            };
                            let is_str_lit = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
                            if is_str_lit {
                                // String literals are already const char*
                                format!("(const char*){}", v(a))
                            } else if is_str_struct {
                                format!("gorget_str_to_cstr({})", v(a))
                            } else if is_ptr_to_str {
                                format!("gorget_str_to_cstr(*(Str*){})", v(a))
                            } else {
                                // Fallback: assume already const char*
                                format!("(const char*){}", v(a))
                            }
                        };
                        if is_int_parse {
                            let cast_type = if name.contains("uint8") { "uint8_t" }
                                else if name.contains("uint16") { "uint16_t" }
                                else if name.contains("uint32") { "uint32_t" }
                                else if name.contains("uint64") { "uint64_t" }
                                else if name.contains("int8") { "int8_t" }
                                else if name.contains("int16") { "int16_t" }
                                else if name.contains("int32") { "int32_t" }
                                else { "int64_t" };
                            let range_check = match cast_type {
                                "int8_t" => " && __pr.value >= -128 && __pr.value <= 127",
                                "int16_t" => " && __pr.value >= -32768 && __pr.value <= 32767",
                                "int32_t" => " && __pr.value >= -2147483648LL && __pr.value <= 2147483647LL",
                                "uint8_t" => " && __pr.value >= 0 && __pr.value <= 255",
                                "uint16_t" => " && __pr.value >= 0 && __pr.value <= 65535",
                                "uint32_t" => " && __pr.value >= 0 && __pr.value <= 4294967295LL",
                                _ => "", // int64_t / uint64_t: full-width, no check needed
                            };
                            let cstr_arg = if !args.is_empty() { coerce_arg_to_cstr(args[0]) } else { "NULL".to_string() };
                            write!(out, "{dv} = ({{ const char* __pa = {cstr_arg}; GorgetParseIntResult __pr = gorget_try_parse_int(__pa, strlen(__pa)); \
                                {opt_c} __opt; if (__pr.ok{range_check}) {{ __opt.tag = 0; __opt.{payload_fname} = ({cast_type})__pr.value; }} \
                                else {{ __opt.tag = 1; }} __opt; }});",
                                dv = v(*d)).unwrap();
                            return;
                        } else if is_float_parse {
                            let cstr_arg = if !args.is_empty() { coerce_arg_to_cstr(args[0]) } else { "NULL".to_string() };
                            write!(out, "{dv} = ({{ const char* __pa = {cstr_arg}; GorgetParseFloatResult __pr = gorget_try_parse_float(__pa, strlen(__pa)); \
                                {opt_c} __opt; if (__pr.ok) {{ __opt.tag = 0; __opt.{payload_fname} = (double)__pr.value; }} \
                                else {{ __opt.tag = 1; }} __opt; }});",
                                dv = v(*d)).unwrap();
                            return;
                        } else if is_bool_parse {
                            let cstr_arg = if !args.is_empty() { coerce_arg_to_cstr(args[0]) } else { "NULL".to_string() };
                            write!(out, "{dv} = ({{ const char* __ps = {cstr_arg}; size_t __pl = strlen(__ps); \
                                {opt_c} __opt; \
                                if (__pl == 4 && memcmp(__ps, \"true\", 4) == 0) {{ __opt.tag = 0; __opt.{payload_fname} = true; }} \
                                else if (__pl == 5 && memcmp(__ps, \"false\", 5) == 0) {{ __opt.tag = 0; __opt.{payload_fname} = false; }} \
                                else {{ __opt.tag = 1; }} __opt; }});",
                                dv = v(*d)).unwrap();
                            return;
                        }
                    }
                }
            }

            // For fprintf_stderr, skip the first arg (Null placeholder).
            let emit_args: &[ValueId] = if is_stderr_print && !args.is_empty() {
                &args[1..]
            } else {
                args
            };
            let ext_decl = module.externs.iter().find(|e| &e.name == name);
            // For spawn/inline helpers, the LIR extern declares params as
            // (ptr, i64, ...) but the generated C helper uses real types (Str etc).
            // Look up the original function to get correct types for coercion.
            let fn_params_owned: Option<Vec<LirType>> = if emit_name.starts_with("__gorget_spawn_") {
                let lookup_name = emit_name.strip_prefix("__gorget_spawn_").unwrap();
                module.functions.iter()
                    .find(|f| f.name == lookup_name)
                    .map(|f| f.params.clone())
            } else { None };
            // For Dict/Set inline methods, String ptr methods, and spawn helpers,
            // the LIR extern params use ptr for Str args. Set a flag so we can
            // coerce string literal args to Str at the call site.
            let force_str_coerce = parse_dict_higher_order(name).is_some()
                || parse_set_higher_order(name).is_some()
                || GORGET_STRING_PTR_METHODS.contains(&name.as_str());
            // For trait box method calls, determine which specific arg positions need Str coercion.
            let trait_str_arg_positions = trait_box_str_arg_positions(module, name);
            let ext_params: Option<&[LirType]> = if fn_params_owned.is_some() {
                fn_params_owned.as_deref()
            } else if let Some(e) = &ext_decl {
                Some(e.params.as_slice())
            } else {
                None
            };
            let ret_is_void = ext_decl.as_ref().map_or(false, |e| matches!(e.return_type, LirType::Void));

            // ── last_error Result wrapping ────────────────────────────
            // Runtime functions that return a raw scalar + set a thread-local error.
            // The LIR expects a Result struct. Wrap the call to construct it.
            if let Some(err_fn) = last_error_fn(emit_name) {
                if let Some(d) = dst {
                    let dst_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                    if let Some(LirType::Struct(sid)) = dst_ty {
                        let sdef = &module.structs[sid.0 as usize];
                        if sdef.name.starts_with("Result__") && sdef.fields.len() >= 3 {
                            let result_c = c_type_named(dst_ty.unwrap(), sn);
                            // Ok payload: field[1] (after tag)
                            let ok_fname = c_field_name(&sdef.fields[1].0);
                            let ok_ty_c = c_type_named(&sdef.fields[1].1, sn);
                            // Error payload: field[2]
                            let err_fname = c_field_name(&sdef.fields[2].0);
                            // Only wrap with gorget_str_from_cstr if BOTH the function
                            // is a cstr-returning category AND the Ok payload is Str.
                            let wrap_cstr = last_error_returns_cstr(emit_name) && ok_ty_c == "Str";
                            write!(out, "{dv} = ({{ {ok_ty_c} __raw = ", dv = v(*d)).unwrap();
                            if wrap_cstr { write!(out, "gorget_str_from_cstr(").unwrap(); }
                            write!(out, "{}(", emit_name).unwrap();
                            for (i, a) in emit_args.iter().enumerate() {
                                if i > 0 { write!(out, ", ").unwrap(); }
                                let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                                let is_str_lit = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
                                let needs_cstr = takes_cstr_for_str_param(emit_name, i);
                                let is_str_struct = matches!(arg_ty, Some(LirType::Struct(sid)) if {
                                    module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetStringView" || s.name == "GorgetString")
                                });
                                // Also check if arg is Ptr to GorgetStringView (e.g., SlotAddr → Str slot)
                                let is_ptr_to_str = matches!(arg_ty, Some(LirType::Ptr)) && {
                                    ptr_pointee.get(a.0 as usize).and_then(|t| t.as_ref()).map_or(false, |t| {
                                        matches!(t, LirType::Struct(sid) if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetStringView" || s.name == "GorgetString"))
                                    })
                                };
                                let needs_null_term = needs_null_terminated_cstr(emit_name);
                                if needs_cstr && is_str_lit {
                                    write!(out, "{}", v(*a)).unwrap();
                                } else if needs_cstr && needs_null_term && is_str_struct {
                                    // Str → null-terminated const char*
                                    write!(out, "gorget_str_to_cstr({})", v(*a)).unwrap();
                                } else if needs_cstr && needs_null_term && is_ptr_to_str {
                                    write!(out, "gorget_str_to_cstr(*(Str*){})", v(*a)).unwrap();
                                } else if needs_cstr && is_str_struct {
                                    // Str → const char*: extract .data field
                                    write!(out, "({}).data", v(*a)).unwrap();
                                } else if needs_cstr && is_ptr_to_str {
                                    // Ptr-to-Str → const char*: deref + extract .data
                                    write!(out, "((Str*){})->data", v(*a)).unwrap();
                                } else if runtime_arg_by_ptr(emit_name, i) && matches!(arg_ty, Some(LirType::Struct(_))) {
                                    // Struct value → pointer: take address
                                    write!(out, "&{}", v(*a)).unwrap();
                                } else if runtime_arg_by_ptr(emit_name, i) && matches!(arg_ty, Some(LirType::Ptr)) {
                                    // Already a pointer, pass directly
                                    write!(out, "{}", v(*a)).unwrap();
                                } else {
                                    let ext_param = ext_params.and_then(|p| p.get(i));
                                    emit_coerced_arg(out, a, ext_param, val_types, str_lit_vals, sn);
                                }
                            }
                            write!(out, ")").unwrap();
                            if wrap_cstr { write!(out, ")").unwrap(); }
                            write!(out, "; const char* __err = {err_fn}(); \
                                {result_c} __wr; if (__err) {{ __wr.tag = 1; __wr.{err_fname} = gorget_str_from_cstr(__err); }} \
                                else {{ __wr.tag = 0; __wr.{ok_fname} = __raw; }} __wr; }});").unwrap();
                            return;
                        }
                    }
                }
            }

            // ── Sentinel-based Option wrapping ───────────────────────
            // Runtime functions that return a scalar (int64_t) with -1 sentinel for "not found".
            // The GIR expects Option[T] — wrap: if (__raw >= 0) Some(__raw) else None.
            if let Some(d) = dst {
                let dst_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                if let Some(LirType::Struct(sid)) = dst_ty {
                    let sdef = &module.structs[sid.0 as usize];
                    if sdef.name.starts_with("Option__") {
                        // Check if the extern returns a scalar, not a struct/void*
                        let ext_ret = ext_decl.map(|e| &e.return_type);
                        let ext_ret_is_scalar = matches!(ext_ret, Some(LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8
                            | LirType::U64 | LirType::U32 | LirType::U16 | LirType::U8 | LirType::F64 | LirType::F32));
                        // Skip functions that already return Option (upgrade, recv_timeout, try_parse)
                        let skip = emit_name.ends_with("__upgrade")
                            || emit_name.ends_with("__recv_timeout")
                            || emit_name.contains("try_parse");
                        if ext_ret_is_scalar && !skip {
                            let opt_c = c_type_named(dst_ty.unwrap(), sn);
                            let payload_fname = sdef.fields.get(1)
                                .map(|(n, _)| c_field_name(n)).unwrap_or_else(|| "Some_0".to_string());
                            let payload_ty_c = sdef.fields.get(1)
                                .map(|(_, t)| c_type_named(t, sn)).unwrap_or_else(|| "int64_t".to_string());
                            let opt_void_params = collection_void_param_indices(emit_name);
                            write!(out, "{dv} = ({{ {payload_ty_c} __raw = {emit_name}(", dv = v(*d)).unwrap();
                            for (i, a) in emit_args.iter().enumerate() {
                                if i > 0 { write!(out, ", ").unwrap(); }
                                let arg_ty2 = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                                let is_str_lit2 = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
                                if opt_void_params.contains(&i) && arg_ty2.map_or(false, |t| !matches!(t, LirType::Ptr)) {
                                    let ty_name2 = c_type_named(arg_ty2.unwrap(), sn);
                                    write!(out, "&({ty_name2}){{ {} }}", v(*a)).unwrap();
                                } else if opt_void_params.contains(&i) && is_str_lit2 {
                                    write!(out, "&(Str){{ .data = {v}, .len = strlen({v}), .cap = 0, .alloc = NULL }}", v = v(*a)).unwrap();
                                } else {
                                    let ext_param = ext_params.and_then(|p| p.get(i));
                                    emit_coerced_arg(out, a, ext_param, val_types, str_lit_vals, sn);
                                }
                            }
                            // Use >= 0 sentinel for integer types, direct for others
                            if matches!(ext_ret, Some(LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8)) {
                                write!(out, "); {opt_c} __opt; if (__raw >= 0) {{ __opt.tag = 0; __opt.{payload_fname} = __raw; }} \
                                    else {{ __opt.tag = 1; }} __opt; }});").unwrap();
                            } else {
                                // For unsigned/float: always Some (this case is rare)
                                write!(out, "); {opt_c} __opt; __opt.tag = 0; __opt.{payload_fname} = __raw; __opt; }});").unwrap();
                            }
                            return;
                        }
                    }
                }
            }

            // ── Collection void* return dereference ──────────────────
            // Functions like gorget_array_get return void* — dereference
            // to the concrete element type expected by the destination.
            let void_ret = is_collection_void_return(emit_name) || needs_opt_wrapping(emit_name);
            let dst_ty_opt = dst.and_then(|d| val_types.get(d.0 as usize).and_then(|t| t.as_ref()));
            let dst_is_option_struct = void_ret && matches!(dst_ty_opt, Some(LirType::Struct(sid)) if {
                let s = module.structs.get(sid.0 as usize);
                s.map_or(false, |sd| sd.name.starts_with("Option__") || sd.name.starts_with("Result__"))
            });
            let dst_needs_deref = void_ret && !dst_is_option_struct && dst.map_or(false, |d| {
                let ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                ty.map_or(false, |t| !matches!(t, LirType::Ptr))
            });

            // When the collection function returns void* but the GIR expects Option[T],
            // we need to construct the Option from the result (NULL → None, non-null → Some(val)).
            if dst_is_option_struct {
                let d = dst.unwrap();
                let dst_ty = val_types[d.0 as usize].as_ref().unwrap();
                let struct_name = c_type_named(dst_ty, sn);
                // Find the payload type from the struct definition
                let sid = match dst_ty { LirType::Struct(s) => *s, _ => unreachable!() };
                let sdef = &module.structs[sid.0 as usize];
                // Payload field is the second field (first is "tag")
                let payload_ty = sdef.fields.get(1).map(|(_, t)| t);
                let payload_c = payload_ty.map(|t| c_type_named(t, sn)).unwrap_or_else(|| "int64_t".to_string());
                let payload_fname = sdef.fields.get(1).map(|(n, _)| c_field_name(n)).unwrap_or_else(|| "Some_0".to_string());
                // For void-returning functions that need to return a value, swap to opt variant.
                let call_name = void_to_opt_variant(emit_name);
                // Emit: { void* __tmp = call(args); if (__tmp) { dst.tag = 0; dst.payload = *(Type*)__tmp; } else { memset(&dst, 0, sizeof(StructType)); dst.tag = 1; } }
                write!(out, "{{ void* __tmp = {}(", call_name).unwrap();
                for (i, a) in emit_args.iter().enumerate() {
                    if i > 0 { write!(out, ", ").unwrap(); }
                    let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                    let is_str_lit = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
                    if runtime_arg_by_ptr(emit_name, i) && matches!(arg_ty, Some(LirType::Ptr) | Some(LirType::Struct(_))) {
                        if matches!(arg_ty, Some(LirType::Struct(_))) {
                            write!(out, "&{}", v(*a)).unwrap();
                        } else {
                            write!(out, "{}", v(*a)).unwrap();
                        }
                    } else if is_str_lit && emit_name.starts_with("gorget_str_") {
                        write!(out, "gorget_str_from_literal({}, strlen({}))", v(*a), v(*a)).unwrap();
                    } else if collection_void_param_indices(emit_name).contains(&i) && arg_ty.map_or(false, |t| !matches!(t, LirType::Ptr)) {
                        let ty_name = c_type_named(arg_ty.unwrap(), sn);
                        write!(out, "&({ty_name}){{ {} }}", v(*a)).unwrap();
                    } else if collection_void_param_indices(emit_name).contains(&i) && is_str_lit {
                        // String literal arg to void* collection param → wrap as &(Str){...}
                        write!(out, "&(Str){{ .data = {v}, .len = strlen({v}), .cap = 0, .alloc = NULL }}", v = v(*a)).unwrap();
                    } else {
                        let ext_param = ext_params.and_then(|p| p.get(i));
                        emit_coerced_arg(out, a, ext_param, val_types, str_lit_vals, sn);
                    }
                }
                // When the payload field is Ptr (i.e. Option[T &] — a borrowed reference),
                // store the pointer directly instead of dereferencing. The reference
                // borrows from the collection; no clone or drop needed.
                let payload_is_ptr = matches!(payload_ty, Some(LirType::Ptr));
                // For resource-type payloads from borrowing reads (get/first/last),
                // clone to avoid double-free. For consuming methods (pop/remove),
                // the element is already removed from the collection — no clone needed.
                let is_consuming = matches!(call_name,
                    "gorget_array_safe_pop" | "gorget_array_remove_opt"
                    | "gorget_map_remove" | "gorget_set_remove");
                let clone_fn = if payload_is_ptr || is_consuming {
                    None // Ptr payload (borrowed) or consuming method (moved out)
                } else {
                    match payload_c.as_str() {
                        "GorgetArray" => Some("gorget_array_clone"),
                        "GorgetMap" => Some("gorget_map_clone"),
                        "GorgetSet" => Some("gorget_set_clone"),
                        "GorgetString" => Some("gorget_string_clone"),
                        _ => None,
                    }
                };
                // Deep-clone placeholder: when Phase 6 collection drops are enabled,
                // struct payloads with resource fields need field-level cloning here.
                // Currently disabled — collection locals are not dropped at scope exit.
                let deep_clone_ops: Option<Vec<String>> = None;
                if payload_is_ptr {
                    // Option[T &]: store pointer directly (borrowed, not dereferenced)
                    write!(out, "); if (__tmp) {{ {dv}.tag = 0; {dv}.{payload_fname} = __tmp; }} else {{ memset(&{dv}, 0, sizeof({struct_name})); {dv}.tag = 1; }} }}", dv = v(d)).unwrap();
                } else if let Some(cfn) = clone_fn {
                    write!(out, "); if (__tmp) {{ {dv}.tag = 0; {dv}.{payload_fname} = {cfn}(({payload_c}*)__tmp); }} else {{ memset(&{dv}, 0, sizeof({struct_name})); {dv}.tag = 1; }} }}", dv = v(d)).unwrap();
                } else if let Some(ops) = deep_clone_ops {
                    write!(out, "); if (__tmp) {{ {dv}.tag = 0; {dv}.{payload_fname} = *({payload_c}*)__tmp;", dv = v(d)).unwrap();
                    for op in &ops {
                        write!(out, " {op}").unwrap();
                    }
                    write!(out, " }} else {{ memset(&{dv}, 0, sizeof({struct_name})); {dv}.tag = 1; }} }}", dv = v(d)).unwrap();
                } else {
                    write!(out, "); if (__tmp) {{ {dv}.tag = 0; {dv}.{payload_fname} = *({payload_c}*)__tmp; }} else {{ memset(&{dv}, 0, sizeof({struct_name})); {dv}.tag = 1; }} }}", dv = v(d)).unwrap();
                }
                return;
            }

            // ── __gorget_spawn_* → Task__T handling ──────────────
            // Spawn helpers now return Task__T. When LIR destination is a Task struct,
            // simple assignment works. When LIR destination is void*, extract .__task.
            let is_spawn = emit_name.starts_with("__gorget_spawn_");
            let dst_is_task_struct = is_spawn && dst.map_or(false, |d| {
                matches!(val_types.get(d.0 as usize).and_then(|t| t.as_ref()), Some(LirType::Struct(sid)) if {
                    module.structs.get(sid.0 as usize).map_or(false, |s| s.name.starts_with("Task__"))
                })
            });
            // Non-struct Task destination (void*) — extract .__task from returned struct.
            let dst_is_spawn_ptr = is_spawn && !dst_is_task_struct && dst.is_some();

            // ── Inline higher-order collection methods ─────────────
            // Vector/Dict/Set filter/map/fold/etc. must be inlined at each call
            // site to use the correct __Closure_N__call function for that site.
            // Helper: resolve __Closure_N__call for a closure arg at this specific call site.
            let resolve_call_fn = |closure_arg: Option<ValueId>| -> String {
                closure_arg.and_then(|ca| {
                    let try_from_val = val_types.get(ca.0 as usize).and_then(|t| t.as_ref()).and_then(|ty| {
                        if let LirType::Struct(sid) = ty {
                            let sdef = &module.structs[sid.0 as usize];
                            let call_name = format!("{}__call", sdef.name);
                            if module.functions.iter().any(|f| f.name == call_name) { Some(call_name) } else { None }
                        } else { None }
                    });
                    try_from_val.or_else(|| {
                        ptr_pointee.get(ca.0 as usize).and_then(|t| t.as_ref()).and_then(|ty| {
                            if let LirType::Struct(sid) = ty {
                                let sdef = &module.structs[sid.0 as usize];
                                let call_name = format!("{}__call", sdef.name);
                                if module.functions.iter().any(|f| f.name == call_name) { Some(call_name) } else { None }
                            } else { None }
                        })
                    }).or_else(|| {
                        // Check if the arg is a FuncAddr (named function as closure).
                        // Use the __adapt_* wrapper which follows the closure calling convention.
                        func_addr_targets.get(ca.0 as usize).and_then(|t| *t).map(|fid| {
                            format!("__adapt_{}", c_func_name(&module.functions[fid.0 as usize].name))
                        })
                    })
                }).unwrap_or_else(|| find_closure_call_fn(module, "void*", sn))
            };

            if let Some((elem_ty, method)) = parse_vector_higher_order(emit_name) {
                if dst.is_some() || method == "find" || method == "each" || method == "sort" {
                    let d_opt = dst;
                    let orig_to_c2: HashMap<String, String> = module.structs.iter().enumerate()
                        .map(|(i, def)| (def.name.clone(), sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"))))
                        .collect();
                    let elem_c = elem_type_to_c_with_sn(elem_ty, &orig_to_c2);
                    let closure_arg = if method == "sorted" || method == "sort" || method == "unique" { None } else { emit_args.last().copied() };
                    let call_fn = resolve_call_fn(closure_arg);
                    let dv = d_opt.map(|d| format!("__v{}", d.0)).unwrap_or_default();
                    let arr_arg = v(emit_args[0]);
                    let fn_arg = closure_arg.map(|ca| v(ca)).unwrap_or_default();
                    // If closure arg is already a pointer (Ptr type), don't add extra &
                    let closure_is_ptr = closure_arg.map_or(false, |ca| matches!(val_types.get(ca.0 as usize).and_then(|t| t.as_ref()), Some(LirType::Ptr)));
                    let fn_ref = if closure_is_ptr { fn_arg.clone() } else { format!("&{fn_arg}") };
                    match method {
                        "filter" => {
                            write!(out, "{dv} = ({{ GorgetArray __src = *(GorgetArray*){arr_arg}; \
                                GorgetArray __result = gorget_array_new(sizeof({elem_c})); \
                                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                                {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i); \
                                if ({call_fn}({fn_ref}, __elem)) gorget_array_push(&__result, &__elem); \
                                }} __result; }});").unwrap();
                        }
                        "map" => {
                            write!(out, "{dv} = ({{ GorgetArray __src = *(GorgetArray*){arr_arg}; \
                                __typeof__({call_fn}({fn_ref}, ({elem_c}){{0}})) __map_out; \
                                GorgetArray __result = gorget_array_new(sizeof(__map_out)); \
                                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                                {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i); \
                                __map_out = {call_fn}({fn_ref}, __elem); \
                                gorget_array_push(&__result, &__map_out); \
                                }} __result; }});").unwrap();
                        }
                        "fold" if emit_args.len() >= 3 => {
                            let acc_arg = v(emit_args[1]);
                            let fn_a = v(emit_args[2]);
                            let fold_closure_is_ptr = matches!(val_types.get(emit_args[2].0 as usize).and_then(|t| t.as_ref()), Some(LirType::Ptr));
                            let fn_a_ref = if fold_closure_is_ptr { fn_a.clone() } else { format!("&{fn_a}") };
                            let call_fn2 = val_types.get(emit_args[2].0 as usize).and_then(|t| t.as_ref()).and_then(|ty| {
                                if let LirType::Struct(sid) = ty {
                                    let sdef = &module.structs[sid.0 as usize];
                                    let cn = format!("{}__call", sdef.name);
                                    if module.functions.iter().any(|f| f.name == cn) { Some(cn) } else { None }
                                } else { None }
                            }).unwrap_or_else(|| call_fn.clone());
                            // Accumulator type: use destination type (fold returns accumulator, not element).
                            let acc_c = d_opt.and_then(|d| val_types.get(d.0 as usize).and_then(|t| t.as_ref()))
                                .map(|t| c_type_named(t, sn)).unwrap_or_else(|| "int64_t".to_string());
                            // Detect Str/GorgetString mismatch: if the closure returns GorgetString
                            // but the fold destination is Str, use GorgetString internally and convert at the end.
                            let closure_returns_gorget_string = {
                                // Look up the __call function and check its return type
                                module.functions.iter().find(|f| f.name == call_fn2).map_or(false, |f| {
                                    matches!(&f.return_type, LirType::Struct(sid) if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetString"))
                                })
                            };
                            let dst_is_str = acc_c == "Str";
                            let acc_is_str_lit = str_lit_vals.get(emit_args[1].0 as usize).copied().unwrap_or(false);
                            if closure_returns_gorget_string && dst_is_str {
                                // Use GorgetString as internal accumulator, convert at boundaries
                                let acc_init = if acc_is_str_lit {
                                    format!("gorget_string_new(gorget_str_from_literal({acc_arg}, strlen({acc_arg})).data)")
                                } else {
                                    format!("gorget_string_new({acc_arg}.data)")
                                };
                                write!(out, "{dv} = ({{ GorgetArray __src = *(GorgetArray*){arr_arg}; \
                                    GorgetString __acc = {acc_init}; \
                                    for (size_t __i = 0; __i < __src.len; __i++) {{ \
                                    {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i); \
                                    __acc = {call_fn2}({fn_a_ref}, (Str){{ .data = __acc.data, .len = __acc.len, .cap = 0, .alloc = NULL }}, __elem); \
                                    }} (Str){{ .data = __acc.data, .len = __acc.len, .cap = 0, .alloc = NULL }}; }});").unwrap();
                            } else {
                                write!(out, "{dv} = ({{ GorgetArray __src = *(GorgetArray*){arr_arg}; \
                                    {acc_c} __acc = {acc_arg}; \
                                    for (size_t __i = 0; __i < __src.len; __i++) {{ \
                                    {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i); \
                                    __acc = {call_fn2}({fn_a_ref}, __acc, __elem); \
                                    }} __acc; }});").unwrap();
                            }
                        }
                        "reduce" => {
                            write!(out, "{dv} = ({{ GorgetArray __src = *(GorgetArray*){arr_arg}; \
                                {elem_c} __acc = GORGET_ARRAY_AT({elem_c}, __src, 0); \
                                for (size_t __i = 1; __i < __src.len; __i++) {{ \
                                {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i); \
                                __acc = {call_fn}({fn_ref}, __acc, __elem); \
                                }} __acc; }});").unwrap();
                        }
                        "any" => {
                            write!(out, "{dv} = ({{ GorgetArray __src = *(GorgetArray*){arr_arg}; \
                                bool __any_r = false; \
                                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                                {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i); \
                                if ({call_fn}({fn_ref}, __elem)) {{ __any_r = true; break; }} \
                                }} __any_r; }});").unwrap();
                        }
                        "all" => {
                            write!(out, "{dv} = ({{ GorgetArray __src = *(GorgetArray*){arr_arg}; \
                                bool __all_r = true; \
                                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                                {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i); \
                                if (!{call_fn}({fn_ref}, __elem)) {{ __all_r = false; break; }} \
                                }} __all_r; }});").unwrap();
                        }
                        "each" => {
                            write!(out, "{{ GorgetArray __src = *(GorgetArray*){arr_arg}; \
                                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                                {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i); \
                                {call_fn}({fn_ref}, __elem); \
                                }} }}").unwrap();
                        }
                        "sorted" => {
                            let cmp = compare_fn_for_elem(&elem_c);
                            write!(out, "{dv} = ({{ GorgetArray __result = gorget_array_clone((GorgetArray*){arr_arg}); \
                                qsort(__result.data, __result.len, __result.elem_size, {cmp}); \
                                __result; }});").unwrap();
                        }
                        "sort" => {
                            let cmp = compare_fn_for_elem(&elem_c);
                            write!(out, "{{ GorgetArray* __a = (GorgetArray*){arr_arg}; \
                                qsort(__a->data, __a->len, __a->elem_size, {cmp}); }}").unwrap();
                        }
                        "unique" => {
                            let cmp = compare_fn_for_elem(&elem_c);
                            write!(out, "{dv} = ({{ GorgetArray __result = gorget_array_clone((GorgetArray*){arr_arg}); \
                                qsort(__result.data, __result.len, __result.elem_size, {cmp}); \
                                gorget_array_dedup(&__result); \
                                __result; }});").unwrap();
                        }
                        "find" => {
                            // In LIR, find returns Option struct directly (no output ptr).
                            // Option: tag=0 → Some(elem), tag=1 → None
                            let opt_ty = d_opt.and_then(|d| val_types.get(d.0 as usize).and_then(|t| t.as_ref()))
                                .map(|t| c_type_named(t, sn)).unwrap_or_else(|| "int64_t".to_string());
                            // Directly assign to the named field to handle struct padding correctly
                            // (tag is int32_t but payload may be at offset 8 due to alignment).
                            write!(out, "{dv} = ({{ GorgetArray __src = *(GorgetArray*){arr_arg}; \
                                {opt_ty} __opt; memset(&__opt, 0, sizeof(__opt)); __opt.tag = 1; \
                                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                                {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i); \
                                if ({call_fn}({fn_ref}, __elem)) {{ __opt.tag = 0; __opt.Some_0 = __elem; break; }} \
                                }} __opt; }});").unwrap();
                        }
                        "find_index" => {
                            write!(out, "{dv} = ({{ GorgetArray __src = *(GorgetArray*){arr_arg}; \
                                int64_t __idx = -1; \
                                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                                {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i); \
                                if ({call_fn}({fn_ref}, __elem)) {{ __idx = (int64_t)__i; break; }} \
                                }} __idx; }});").unwrap();
                        }
                        "flat_map" => {
                            write!(out, "{dv} = ({{ GorgetArray __src = *(GorgetArray*){arr_arg}; \
                                GorgetArray __result = gorget_array_new(sizeof({elem_c})); \
                                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                                {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i); \
                                GorgetArray __sub = {call_fn}({fn_ref}, __elem); \
                                gorget_array_extend(&__result, &__sub); \
                                }} __result; }});").unwrap();
                        }
                        "count" => {
                            write!(out, "{dv} = ({{ GorgetArray __src = *(GorgetArray*){arr_arg}; \
                                int64_t __cnt = 0; \
                                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                                {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i); \
                                if ({call_fn}({fn_ref}, __elem)) __cnt++; \
                                }} __cnt; }});").unwrap();
                        }
                        _ => {
                            // Fall through to existing helper call
                            write!(out, "{}({arr_arg}, {fn_arg})", emit_name).unwrap();
                            write!(out, ");").unwrap();
                        }
                    }
                    return;
                }
            }

            // ── Inline Dict higher-order methods ─────────────
            if let Some((key_ty, val_ty, method)) = parse_dict_higher_order(emit_name) {
                let has_closure = matches!(method, "filter" | "fold" | "each" | "any" | "all");
                if has_closure && (dst.is_some() || method == "each") {
                    let d_opt = dst;
                    let orig_to_c2: HashMap<String, String> = module.structs.iter().enumerate()
                        .map(|(i, def)| (def.name.clone(), sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"))))
                        .collect();
                    let key_c = elem_type_to_c_with_sn(key_ty, &orig_to_c2);
                    let val_c = elem_type_to_c_with_sn(val_ty, &orig_to_c2);
                    let closure_arg = emit_args.last().copied();
                    let call_fn = resolve_call_fn(closure_arg);
                    let dv = d_opt.map(|d| format!("__v{}", d.0)).unwrap_or_default();
                    let map_arg = v(emit_args[0]);
                    let fn_arg = closure_arg.map(|ca| v(ca)).unwrap_or_default();
                    let dict_closure_is_ptr = closure_arg.map_or(false, |ca| matches!(val_types.get(ca.0 as usize).and_then(|t| t.as_ref()), Some(LirType::Ptr)));
                    let dict_fn_ref = if dict_closure_is_ptr { fn_arg.clone() } else { format!("&{fn_arg}") };
                    let is_dict = emit_name.starts_with("Dict__");
                    let ctor_fn = if key_c == "Str" {
                        if is_dict { "gorget_dict_new_str" } else { "gorget_map_new_str" }
                    } else {
                        if is_dict { "gorget_dict_new" } else { "gorget_map_new" }
                    };
                    let ctor_args = if key_c == "Str" { format!("sizeof({val_c})") } else { format!("sizeof({key_c}), sizeof({val_c})") };
                    match method {
                        "filter" => {
                            write!(out, "{dv} = ({{ GorgetMap __src = *(GorgetMap*){map_arg}; \
                                GorgetMap __result = {ctor_fn}({ctor_args}); \
                                for (size_t __i = 0; __i < __src.cap; __i++) {{ \
                                if (__src.states[__i] != 1) continue; \
                                {key_c} __key = *({key_c}*)((char*)__src.keys + __i * __src.key_size); \
                                {val_c} __val = *({val_c}*)((char*)__src.values + __i * __src.val_size); \
                                if ({call_fn}({dict_fn_ref}, __key, __val)) gorget_map_put(&__result, &__key, &__val); \
                                }} __result; }});").unwrap();
                        }
                        "fold" if emit_args.len() >= 3 => {
                            let acc_arg = v(emit_args[1]);
                            let fn_a = v(emit_args[2]);
                            let dict_fold_is_ptr = matches!(val_types.get(emit_args[2].0 as usize).and_then(|t| t.as_ref()), Some(LirType::Ptr));
                            let dict_fn_a_ref = if dict_fold_is_ptr { fn_a.clone() } else { format!("&{fn_a}") };
                            let call_fn2 = resolve_call_fn(Some(emit_args[2]));
                            write!(out, "{dv} = ({{ GorgetMap __src = *(GorgetMap*){map_arg}; \
                                __typeof__({acc_arg}) __acc = {acc_arg}; \
                                for (size_t __i = 0; __i < __src.cap; __i++) {{ \
                                if (__src.states[__i] != 1) continue; \
                                {key_c} __key = *({key_c}*)((char*)__src.keys + __i * __src.key_size); \
                                {val_c} __val = *({val_c}*)((char*)__src.values + __i * __src.val_size); \
                                __acc = {call_fn2}({dict_fn_a_ref}, __acc, __key, __val); \
                                }} __acc; }});").unwrap();
                        }
                        "each" => {
                            write!(out, "{{ GorgetMap __src = *(GorgetMap*){map_arg}; \
                                for (size_t __i = 0; __i < __src.cap; __i++) {{ \
                                if (__src.states[__i] != 1) continue; \
                                {key_c} __key = *({key_c}*)((char*)__src.keys + __i * __src.key_size); \
                                {val_c} __val = *({val_c}*)((char*)__src.values + __i * __src.val_size); \
                                {call_fn}({dict_fn_ref}, __key, __val); \
                                }} }}").unwrap();
                        }
                        "any" => {
                            write!(out, "{dv} = ({{ GorgetMap __src = *(GorgetMap*){map_arg}; \
                                bool __any_r = false; \
                                for (size_t __i = 0; __i < __src.cap; __i++) {{ \
                                if (__src.states[__i] != 1) continue; \
                                {key_c} __key = *({key_c}*)((char*)__src.keys + __i * __src.key_size); \
                                {val_c} __val = *({val_c}*)((char*)__src.values + __i * __src.val_size); \
                                if ({call_fn}({dict_fn_ref}, __key, __val)) {{ __any_r = true; break; }} \
                                }} __any_r; }});").unwrap();
                        }
                        "all" => {
                            write!(out, "{dv} = ({{ GorgetMap __src = *(GorgetMap*){map_arg}; \
                                bool __all_r = true; \
                                for (size_t __i = 0; __i < __src.cap; __i++) {{ \
                                if (__src.states[__i] != 1) continue; \
                                {key_c} __key = *({key_c}*)((char*)__src.keys + __i * __src.key_size); \
                                {val_c} __val = *({val_c}*)((char*)__src.values + __i * __src.val_size); \
                                if (!{call_fn}({dict_fn_ref}, __key, __val)) {{ __all_r = false; break; }} \
                                }} __all_r; }});").unwrap();
                        }
                        _ => {
                            write!(out, "{}({map_arg}, {fn_arg});", emit_name).unwrap();
                        }
                    }
                    return;
                }
            }

            // ── Inline Set higher-order methods ─────────────
            if let Some((elem_ty, method)) = parse_set_higher_order(emit_name) {
                let has_closure = matches!(method, "filter" | "fold" | "each" | "any" | "all");
                if has_closure && (dst.is_some() || method == "each") {
                    let d_opt = dst;
                    let orig_to_c2: HashMap<String, String> = module.structs.iter().enumerate()
                        .map(|(i, def)| (def.name.clone(), sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"))))
                        .collect();
                    let elem_c = elem_type_to_c_with_sn(elem_ty, &orig_to_c2);
                    let closure_arg = emit_args.last().copied();
                    let call_fn = resolve_call_fn(closure_arg);
                    let dv = d_opt.map(|d| format!("__v{}", d.0)).unwrap_or_default();
                    let set_arg = v(emit_args[0]);
                    let fn_arg = closure_arg.map(|ca| v(ca)).unwrap_or_default();
                    let set_closure_is_ptr = closure_arg.map_or(false, |ca| matches!(val_types.get(ca.0 as usize).and_then(|t| t.as_ref()), Some(LirType::Ptr)));
                    let set_fn_ref = if set_closure_is_ptr { fn_arg.clone() } else { format!("&{fn_arg}") };
                    let set_is_ordered = !emit_name.starts_with("HashSet__");
                    let set_ctor = if set_is_ordered { "gorget_ordered_set_new" } else { "gorget_set_new" };
                    let (set_iter_var, set_iter_cond, set_idx_decl) = if set_is_ordered {
                        ("__j", "__src.order_len", "size_t __i = __src.order[__j]; if (__src.states[__i] != 1) continue; ")
                    } else {
                        ("__i", "__src.cap", "if (__src.states[__i] != 1) continue; ")
                    };
                    match method {
                        "filter" => {
                            write!(out, "{dv} = ({{ GorgetSet __src = *(GorgetSet*){set_arg}; \
                                GorgetSet __result = {set_ctor}(sizeof({elem_c})); \
                                for (size_t {set_iter_var} = 0; {set_iter_var} < {set_iter_cond}; {set_iter_var}++) {{ \
                                {set_idx_decl}\
                                {elem_c} __elem = *({elem_c}*)((char*)__src.keys + __i * __src.key_size); \
                                if ({call_fn}({set_fn_ref}, __elem)) gorget_set_add(&__result, &__elem); \
                                }} __result; }});").unwrap();
                        }
                        "fold" if emit_args.len() >= 3 => {
                            let acc_arg = v(emit_args[1]);
                            let fn_a = v(emit_args[2]);
                            let fold_closure_is_ptr2 = matches!(val_types.get(emit_args[2].0 as usize).and_then(|t| t.as_ref()), Some(LirType::Ptr));
                            let fn_a_ref2 = if fold_closure_is_ptr2 { fn_a.clone() } else { format!("&{fn_a}") };
                            let call_fn2 = resolve_call_fn(Some(emit_args[2]));
                            write!(out, "{dv} = ({{ GorgetSet __src = *(GorgetSet*){set_arg}; \
                                __typeof__({acc_arg}) __acc = {acc_arg}; \
                                for (size_t {set_iter_var} = 0; {set_iter_var} < {set_iter_cond}; {set_iter_var}++) {{ \
                                {set_idx_decl}\
                                {elem_c} __elem = *({elem_c}*)((char*)__src.keys + __i * __src.key_size); \
                                __acc = {call_fn2}({fn_a_ref2}, __acc, __elem); \
                                }} __acc; }});").unwrap();
                        }
                        "each" => {
                            write!(out, "{{ GorgetSet __src = *(GorgetSet*){set_arg}; \
                                for (size_t {set_iter_var} = 0; {set_iter_var} < {set_iter_cond}; {set_iter_var}++) {{ \
                                {set_idx_decl}\
                                {elem_c} __elem = *({elem_c}*)((char*)__src.keys + __i * __src.key_size); \
                                {call_fn}({set_fn_ref}, __elem); \
                                }} }}").unwrap();
                        }
                        "any" => {
                            write!(out, "{dv} = ({{ GorgetSet __src = *(GorgetSet*){set_arg}; \
                                bool __any_r = false; \
                                for (size_t {set_iter_var} = 0; {set_iter_var} < {set_iter_cond}; {set_iter_var}++) {{ \
                                {set_idx_decl}\
                                {elem_c} __elem = *({elem_c}*)((char*)__src.keys + __i * __src.key_size); \
                                if ({call_fn}({set_fn_ref}, __elem)) {{ __any_r = true; break; }} \
                                }} __any_r; }});").unwrap();
                        }
                        "all" => {
                            write!(out, "{dv} = ({{ GorgetSet __src = *(GorgetSet*){set_arg}; \
                                bool __all_r = true; \
                                for (size_t {set_iter_var} = 0; {set_iter_var} < {set_iter_cond}; {set_iter_var}++) {{ \
                                {set_idx_decl}\
                                {elem_c} __elem = *({elem_c}*)((char*)__src.keys + __i * __src.key_size); \
                                if (!{call_fn}({set_fn_ref}, __elem)) {{ __all_r = false; break; }} \
                                }} __all_r; }});").unwrap();
                        }
                        _ => {
                            write!(out, "{}({set_arg}, {fn_arg});", emit_name).unwrap();
                        }
                    }
                    return;
                }
            }

            // ── Nullable const char* → Option<Str> wrapping ──
            // Functions like gorget_regex_match_group return NULL for no match.
            // Wrap into Option<Str> when the destination type is a struct (Option__Str).
            if is_nullable_cstr_fn(emit_name) {
                if let Some(d) = dst {
                    let dst_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                    let is_option_struct = matches!(dst_ty, Some(LirType::Struct(sid)) if {
                        module.structs.get(sid.0 as usize).map_or(false, |s| s.name.contains("Option"))
                    });
                    if is_option_struct {
                        let opt_ty = c_type_named(dst_ty.unwrap(), sn);
                        write!(out, "{} = ({{ const char* __raw = {}(", v(*d), emit_name).unwrap();
                        for (i, a) in emit_args.iter().enumerate() {
                            if i > 0 { write!(out, ", ").unwrap(); }
                            let ext_param = ext_params.and_then(|p| p.get(i));
                            emit_coerced_arg(out, a, ext_param, val_types, str_lit_vals, sn);
                        }
                        write!(out, "); {opt_ty} __opt; if (__raw) {{ __opt.tag = 0; __opt.Some_0 = gorget_str_from_cstr(__raw); }} else {{ __opt.tag = 1; }} __opt; }});").unwrap();
                        return;
                    }
                }
            }

            // ── Channel__T__recv_timeout → Option wrapping ──
            // The wrapper returns the raw value, but the GIR expects Option<T>.
            // Wrap the call: call gorget_channel_recv_timeout directly and check return code.
            if emit_name.starts_with("Channel__") && emit_name.ends_with("__recv_timeout") {
                if let Some(d) = dst {
                    let dst_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                    let is_option_struct = matches!(dst_ty, Some(LirType::Struct(sid)) if {
                        module.structs.get(sid.0 as usize).map_or(false, |s| s.name.contains("Option"))
                    });
                    if is_option_struct {
                        let opt_ty = c_type_named(dst_ty.unwrap(), sn);
                        // Find the payload type (field 1 of Option struct)
                        let payload_ty = if let Some(LirType::Struct(sid)) = dst_ty {
                            module.structs.get(sid.0 as usize)
                                .and_then(|s| s.fields.get(1))
                                .map(|(_, t)| c_type_named(t, sn))
                        } else { None };
                        let val_c = payload_ty.unwrap_or_else(|| "int64_t".to_string());
                        // args: [channel_ptr, timeout_ms]
                        let ch_arg = v(emit_args[0]);
                        let ms_arg = v(emit_args[1]);
                        write!(out, "{dv} = ({{ {val_c} __val = {{0}}; int __rc = gorget_channel_recv_timeout(*(GorgetChannel**){ch_arg}, &__val, {ms_arg}); \
                            {opt_ty} __opt; if (__rc != 0) {{ __opt.tag = 0; __opt.Some_0 = __val; }} else {{ __opt.tag = 1; }} __opt; }});",
                            dv = v(*d)).unwrap();
                        return;
                    }
                }
            }

            // ── Sentinel-based Option wrapping (e.g. gorget_regex_find → Option<Match>) ──
            // gorget_regex_find returns GorgetRegexMatch; start==-1 means no match → None.
            if is_sentinel_option_fn(emit_name) {
                if let Some(d) = dst {
                    let dst_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                    let is_option_struct = matches!(dst_ty, Some(LirType::Struct(sid)) if {
                        module.structs.get(sid.0 as usize).map_or(false, |s| s.name.contains("Option"))
                    });
                    if is_option_struct {
                        let opt_ty = c_type_named(dst_ty.unwrap(), sn);
                        write!(out, "{} = ({{ GorgetRegexMatch __raw = {}(", v(*d), emit_name).unwrap();
                        for (i, a) in emit_args.iter().enumerate() {
                            if i > 0 { write!(out, ", ").unwrap(); }
                            let ext_param = ext_params.and_then(|p| p.get(i));
                            emit_coerced_arg(out, a, ext_param, val_types, str_lit_vals, sn);
                        }
                        write!(out, "); {opt_ty} __opt; if (__raw.start != -1) {{ __opt.tag = 0; __opt.Some_0 = __raw; }} else {{ __opt.tag = 1; }} __opt; }});").unwrap();
                        return;
                    }
                }
            }

            // ── Nullable pointer → Option wrapping (e.g. Weak__T__upgrade) ──
            if is_nullable_ptr_fn(emit_name) {
                if let Some(d) = dst {
                    let dst_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                    let is_option_struct = matches!(dst_ty, Some(LirType::Struct(sid)) if {
                        module.structs.get(sid.0 as usize).map_or(false, |s| s.name.contains("Option"))
                    });
                    if is_option_struct {
                        let opt_ty = c_type_named(dst_ty.unwrap(), sn);
                        // Find the Some payload type (field 1)
                        let inner_ty = if let Some(LirType::Struct(sid)) = dst_ty {
                            module.structs.get(sid.0 as usize)
                                .and_then(|s| s.fields.get(1))
                                .map(|(_, t)| c_type_named(t, sn))
                        } else { None };
                        let inner_c = inner_ty.unwrap_or_else(|| "void*".to_string());
                        write!(out, "{} = ({{ {inner_c} __raw = {}(", v(*d), emit_name).unwrap();
                        for (i, a) in emit_args.iter().enumerate() {
                            if i > 0 { write!(out, ", ").unwrap(); }
                            let ext_param = ext_params.and_then(|p| p.get(i));
                            emit_coerced_arg(out, a, ext_param, val_types, str_lit_vals, sn);
                        }
                        write!(out, "); {opt_ty} __opt; if (__raw) {{ __opt.tag = 0; __opt.Some_0 = __raw; }} else {{ __opt.tag = 1; }} __opt; }});").unwrap();
                        return;
                    }
                }
            }

            // ── gorget_task_group_submit(group, task) ──
            // The macro expects task to be a Task struct (with .__task, .__drop),
            // but LIR may pass a pointer (void*) containing only .__task extracted
            // from a spawn result. In that case, reconstruct the full Task__void
            // struct with the correct __drop function from the spawn source.
            if emit_name == "gorget_task_group_submit" && emit_args.len() >= 2 {
                let task_arg = emit_args[1];
                let task_ty = val_types.get(task_arg.0 as usize).and_then(|t| t.as_ref());
                let is_ptr = matches!(task_ty, Some(LirType::Ptr) | None);
                let group_arg = v(emit_args[0]);
                let task_v = v(task_arg);
                if is_ptr {
                    // Check if this void* was produced by a spawn (.__task extraction).
                    // If so, reconstruct the full Task__void struct with the correct __drop fn.
                    if let Some(Some(spawn_fn)) = spawn_source_fn.get(task_arg.0 as usize) {
                        let drop_fn = format!("__spawn_drop_{spawn_fn}");
                        // Use gorget_task_group_submit_raw directly to avoid macro comma issues
                        // with compound literals.
                        write!(out, "gorget_task_group_submit_raw(*(TaskGroup*){group_arg}, {task_v}, {drop_fn});").unwrap();
                    } else {
                        // Fallback: dereference pointer to get Task struct (legacy path)
                        write!(out, "gorget_task_group_submit(*(TaskGroup*){group_arg}, *(Task__void*){task_v});").unwrap();
                    }
                } else {
                    let task_ty_name = task_ty.map(|t| c_type_named(t, sn)).unwrap_or_else(|| "Task__void".to_string());
                    write!(out, "gorget_task_group_submit(*(TaskGroup*){group_arg}, *({task_ty_name}*){task_v});").unwrap();
                }
                return;
            }

            let mut deref_clone_extra_close = false;
            let mut deref_deep_clone_ops: Option<Vec<String>> = None;
            if dst_needs_deref {
                let d = dst.unwrap();
                let dst_ty = val_types[d.0 as usize].as_ref().unwrap();
                let ty_name = c_type_named(dst_ty, sn);
                // For resource types, clone instead of shallow-copy to prevent double-free.
                let clone_fn = match ty_name.as_str() {
                    "GorgetArray" => Some("gorget_array_clone"),
                    "GorgetMap" => Some("gorget_map_clone"),
                    "GorgetSet" => Some("gorget_set_clone"),
                    "GorgetString" => Some("gorget_string_clone"),
                    _ => None,
                };
                // Deep-clone placeholder: when Phase 6 collection drops are enabled,
                // struct element reads need field-level cloning here.
                // Currently disabled — collection locals are not dropped at scope exit.
                let deep_clone_ops: Option<Vec<String>> = None;
                if let Some(cfn) = clone_fn {
                    // Emit: dst = clone_fn((Type*)call(args));  — extra ) needed after args
                    write!(out, "{} = {}(({ty_name}*)", v(d), cfn).unwrap();
                    deref_clone_extra_close = true;
                } else if deep_clone_ops.is_some() {
                    // Shallow copy then deep-clone resource fields.
                    write!(out, "{} = *({ty_name}*)", v(d)).unwrap();
                    deref_deep_clone_ops = deep_clone_ops;
                } else {
                    // Emit: dst = *(Type*)call(args);
                    write!(out, "{} = *({ty_name}*)", v(d)).unwrap();
                }
            } else if dst_is_task_struct {
                // Spawn now returns Task__T directly — simple assignment.
                let d = dst.as_ref().unwrap();
                let dst_ty = val_types[d.0 as usize].as_ref().unwrap();
                let _task_ty_name = c_type_named(dst_ty, sn);
                let fn_name_suffix = emit_name.strip_prefix("__gorget_spawn_").unwrap_or(emit_name);
                let spawn_param_c_types: Vec<String> = module.spawned_fns.iter()
                    .find(|sf| sf.fn_name == fn_name_suffix)
                    .map(|sf| sf.params.iter().map(|(_, ct)| ct.clone()).collect())
                    .unwrap_or_default();
                write!(out, "{} = {}(", v(*d), emit_name).unwrap();
                for (i, a) in emit_args.iter().enumerate() {
                    if i > 0 { write!(out, ", ").unwrap(); }
                    let spawn_c_ty = spawn_param_c_types.get(i).map(|s| s.as_str());
                    let arg_is_ptr = matches!(val_types.get(a.0 as usize).and_then(|t| t.as_ref()), Some(LirType::Ptr));
                    if arg_is_ptr && matches!(spawn_c_ty, Some("GorgetArray" | "GorgetMap" | "GorgetSet")) {
                        write!(out, "*({}*){}", spawn_c_ty.unwrap(), v(*a)).unwrap();
                    } else {
                        let ext_param = ext_params.and_then(|p| p.get(i));
                        emit_coerced_arg(out, a, ext_param, val_types, str_lit_vals, sn);
                    }
                }
                writeln!(out, ");").unwrap();
                return;
            } else if dst_is_spawn_ptr {
                // Spawn returns Task__T but dst is void*. Extract .__task pointer.
                let d = dst.as_ref().unwrap();
                let fn_name_suffix = emit_name.strip_prefix("__gorget_spawn_").unwrap_or(emit_name);
                let spawn_param_c_types: Vec<String> = module.spawned_fns.iter()
                    .find(|sf| sf.fn_name == fn_name_suffix)
                    .map(|sf| sf.params.iter().map(|(_, ct)| ct.clone()).collect())
                    .unwrap_or_default();
                write!(out, "{} = {}(", v(*d), emit_name).unwrap();
                for (i, a) in emit_args.iter().enumerate() {
                    if i > 0 { write!(out, ", ").unwrap(); }
                    let spawn_c_ty = spawn_param_c_types.get(i).map(|s| s.as_str());
                    let arg_is_ptr = matches!(val_types.get(a.0 as usize).and_then(|t| t.as_ref()), Some(LirType::Ptr));
                    if arg_is_ptr && matches!(spawn_c_ty, Some("GorgetArray" | "GorgetMap" | "GorgetSet")) {
                        write!(out, "*({}*){}", spawn_c_ty.unwrap(), v(*a)).unwrap();
                    } else {
                        let ext_param = ext_params.and_then(|p| p.get(i));
                        emit_coerced_arg(out, a, ext_param, val_types, str_lit_vals, sn);
                    }
                }
                writeln!(out, ").__task;").unwrap();
                return;
            } else if let Some(d) = dst {
                if !ret_is_void {
                    write!(out, "{} = ", v(*d)).unwrap();
                }
            }

            // ── void* param indices for collection functions ─────────
            let void_params = collection_void_param_indices(emit_name);

            // Fix printf format strings when float args use %lld.
            // The GIR generates %lld for all numeric args, but float args need %f.
            let fmt_arg_id = if is_printf && !emit_args.is_empty() {
                emit_args.first()
            } else { None };
            let need_fmt_fix = is_printf && fmt_arg_id.map_or(false, |fid| {
                str_lit_vals.get(fid.0 as usize).copied().unwrap_or(false)
            }) && emit_args.iter().skip(1).any(|a| {
                let ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                matches!(ty, Some(LirType::F32 | LirType::F64))
                || matches!(ty, Some(LirType::Struct(sid)) if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetStringView"))
            });

            // printf(var) with a single arg triggers -Wformat-security on macOS
            // clang even if the arg is a string literal variable. Always rewrite
            // single-arg printf to printf("%s", var).
            let printf_needs_fmt_guard = is_printf && emit_args.len() == 1;

            write!(out, "{}(", emit_name).unwrap();
            if printf_needs_fmt_guard {
                write!(out, "\"%s\", (const char*)").unwrap();
            }
            if is_stderr_print {
                write!(out, "stderr").unwrap();
                if !emit_args.is_empty() {
                    write!(out, ", ").unwrap();
                }
            }
            // __gorget_await_* takes Task__T by value.
            // When the LIR arg is a Task struct, pass directly.
            // When the LIR arg is void* (non-vector case), construct a Task struct.
            let is_await = emit_name.starts_with("__gorget_await_");

            for (i, a) in emit_args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                let is_str_lit = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
                // For await helpers, coerce arg to Task__T.
                if is_await && i == 0 {
                    let is_task_struct = matches!(arg_ty, Some(LirType::Struct(sid)) if {
                        module.structs.get(sid.0 as usize).map_or(false, |s| s.name.starts_with("Task__"))
                    });
                    if is_task_struct {
                        // Already a Task struct — pass by value.
                        write!(out, "{}", v(*a)).unwrap();
                        continue;
                    } else if matches!(arg_ty, Some(LirType::Ptr)) {
                        // void* — could be:
                        // 1. Pointer to a Task struct (from SlotAddr of aggregate Task slot)
                        // 2. Raw SpawnCtx pointer (non-vector spawn, dst is void*)
                        // Check if the pointer points to a Task struct via ptr_pointee.
                        let pointee_is_task = ptr_pointee.get(a.0 as usize)
                            .and_then(|t| t.as_ref())
                            .map_or(false, |ty| matches!(ty, LirType::Struct(sid) if {
                                module.structs.get(sid.0 as usize).map_or(false, |s| s.name.starts_with("Task__"))
                            }));
                        if pointee_is_task {
                            // Dereference pointer to get Task struct value.
                            let await_fn_name = emit_name.strip_prefix("__gorget_await_").unwrap_or("");
                            let task_type = module.spawned_fns.iter()
                                .find(|sf| sf.fn_name == await_fn_name)
                                .map(|sf| if sf.ret_c_type == "void" { "Task__void".to_string() } else { format!("Task__{}", sf.ret_c_type) })
                                .unwrap_or_else(|| "Task__void".to_string());
                            write!(out, "*({task_type}*){}", v(*a)).unwrap();
                        } else {
                            // Raw SpawnCtx pointer — wrap in Task struct.
                            let await_fn_name = emit_name.strip_prefix("__gorget_await_").unwrap_or("");
                            let task_type = module.spawned_fns.iter()
                                .find(|sf| sf.fn_name == await_fn_name)
                                .map(|sf| if sf.ret_c_type == "void" { "Task__void".to_string() } else { format!("Task__{}", sf.ret_c_type) })
                                .unwrap_or_else(|| "Task__void".to_string());
                            let drop_fn = format!("__spawn_drop_{await_fn_name}");
                            write!(out, "({task_type}){{.__task = {v}, .__drop = {drop_fn}}}", v = v(*a)).unwrap();
                        }
                        continue;
                    }
                }
                // For spawn helpers, coerce void* collection args to the actual struct type.
                if is_spawn && matches!(arg_ty, Some(LirType::Ptr)) {
                    let spawn_fn_name = emit_name.strip_prefix("__gorget_spawn_").unwrap_or("");
                    let spawn_c_ty = module.spawned_fns.iter()
                        .find(|sf| sf.fn_name == spawn_fn_name)
                        .and_then(|sf| sf.params.get(i))
                        .map(|(_, ct)| ct.as_str());
                    if matches!(spawn_c_ty, Some("GorgetArray" | "GorgetMap" | "GorgetSet")) {
                        write!(out, "*({}*){}", spawn_c_ty.unwrap(), v(*a)).unwrap();
                        continue;
                    }
                }
                // Fix printf format string: replace %lld with %f for float args.
                if need_fmt_fix && i == 0 && is_str_lit {
                    // Find the StrLit instruction that defines this format arg.
                    let fmt_val = *a;
                    let mut fmt_text: Option<&str> = None;
                    'find_fmt: for blk in &func.blocks {
                        for inst2 in &blk.insts {
                            if let Inst::StrLit { dst, value } = inst2 {
                                if *dst == fmt_val {
                                    fmt_text = Some(value.as_str());
                                    break 'find_fmt;
                                }
                            }
                        }
                    }
                    if let Some(fmt) = fmt_text {
                        let arg_kinds: Vec<PrintfArgKind> = emit_args[1..].iter()
                            .map(|ea| {
                                let ty = val_types.get(ea.0 as usize).and_then(|t| t.as_ref());
                                if matches!(ty, Some(LirType::F32 | LirType::F64)) {
                                    PrintfArgKind::Float
                                } else if matches!(ty, Some(LirType::Struct(sid)) if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetStringView")) {
                                    PrintfArgKind::Str
                                } else {
                                    PrintfArgKind::Int
                                }
                            })
                            .collect();
                        let fixed = fix_printf_format(fmt, &arg_kinds);
                        let escaped = escape_c_string(&fixed);
                        write!(out, "\"{}\"", escaped).unwrap();
                    } else {
                        // Fallback: emit original value.
                        write!(out, "{}", v(*a)).unwrap();
                    }
                    continue;
                }
                // For printf, decompose GorgetStringView args into (int)len, data for %.*s format.
                if need_fmt_fix && is_printf && i > 0
                    && matches!(arg_ty, Some(LirType::Struct(sid)) if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetStringView"))
                {
                    write!(out, "(int)({v}).len, ({v}).data", v = v(*a)).unwrap();
                    continue;
                }
                // Runtime arg-by-pointer: the C prototype takes a pointer to the struct.
                // If the LIR value is a Ptr, pass directly; if Struct, take address.
                if runtime_arg_by_ptr(emit_name, i) && matches!(arg_ty, Some(LirType::Ptr) | Some(LirType::Struct(_))) {
                    if matches!(arg_ty, Some(LirType::Struct(_))) {
                        write!(out, "&{}", v(*a)).unwrap();
                    } else {
                        write!(out, "{}", v(*a)).unwrap();
                    }
                }
                // For printf, wrap bool/int args with ? "true" : "false" when
                // the corresponding format specifier is %s.
                // This handles cases where GIR types `any()`/`all()` results as i64
                // but the format expects a boolean-as-string.
                else if is_printf && i > 0 && matches!(arg_ty, Some(LirType::Bool) | Some(LirType::I64) | Some(LirType::I32)) {
                    // Check if the format specifier for this arg position is %s.
                    let fmt_is_pct_s = fmt_arg_id.and_then(|fid| {
                        let mut fmt_text: Option<&str> = None;
                        for blk in &func.blocks {
                            for inst2 in &blk.insts {
                                if let Inst::StrLit { dst, value } = inst2 {
                                    if *dst == *fid { fmt_text = Some(value.as_str()); break; }
                                }
                            }
                            if fmt_text.is_some() { break; }
                        }
                        fmt_text
                    }).map_or(false, |fmt| {
                        // Count % specifiers to find the one for arg position (i-1)
                        let arg_idx = i - 1; // skip format arg itself
                        let mut spec_idx = 0usize;
                        let mut chars = fmt.chars().peekable();
                        while let Some(ch) = chars.next() {
                            if ch == '%' {
                                if let Some(&next) = chars.peek() {
                                    if next == '%' { chars.next(); continue; }
                                    // Skip flags: - + space # 0
                                    while chars.peek().map_or(false, |c| "-+ #0".contains(*c)) {
                                        chars.next();
                                    }
                                    // Skip width: digits or * (* consumes an extra arg)
                                    if chars.peek() == Some(&'*') {
                                        chars.next();
                                        // * in width position consumes an argument
                                        if spec_idx == arg_idx { return false; }
                                        spec_idx += 1;
                                    } else {
                                        while chars.peek().map_or(false, |c| c.is_ascii_digit()) {
                                            chars.next();
                                        }
                                    }
                                    // Skip precision: . followed by digits or * (* consumes an extra arg)
                                    if chars.peek() == Some(&'.') {
                                        chars.next();
                                        if chars.peek() == Some(&'*') {
                                            chars.next();
                                            // * in precision position consumes an argument
                                            if spec_idx == arg_idx { return false; }
                                            spec_idx += 1;
                                        } else {
                                            while chars.peek().map_or(false, |c| c.is_ascii_digit()) {
                                                chars.next();
                                            }
                                        }
                                    }
                                    // Skip length modifiers: h, hh, l, ll, L, z, j, t, q
                                    while chars.peek().map_or(false, |c| "hlLzjtq".contains(*c)) {
                                        chars.next();
                                    }
                                    // Read the actual conversion letter
                                    if let Some(spec) = chars.next() {
                                        if spec_idx == arg_idx {
                                            return spec == 's';
                                        }
                                        spec_idx += 1;
                                    }
                                }
                            }
                        }
                        false
                    });
                    if fmt_is_pct_s || matches!(arg_ty, Some(LirType::Bool)) {
                        write!(out, "{} ? \"true\" : \"false\"", v(*a)).unwrap();
                    } else {
                        emit_coerced_arg(out, a, ext_params.and_then(|p| p.get(i)), val_types, str_lit_vals, sn);
                    }
                }
                // Box[Str] alloc: StrLit arg → wrap with gorget_str_from_literal.
                // Ptr arg (from a Str variable) → deref as *(Str*).
                else if name == "__gorget_box_alloc_Str" || name == "__gorget_box_alloc_GorgetStringView" {
                    if is_str_lit {
                        write!(out, "gorget_str_from_literal({}, strlen({}))", v(*a), v(*a)).unwrap();
                    } else if matches!(arg_ty, Some(LirType::Ptr)) {
                        write!(out, "*(Str*){}", v(*a)).unwrap();
                    } else {
                        write!(out, "{}", v(*a)).unwrap();
                    }
                }
                // String literal arg to a gorget_str_* or Dict/Set inline function → Str wrap.
                // Skip if ext_params says this arg is not a Str type (e.g. gorget_str_join arg 1 = GorgetArray).
                // Also wrap for any extern function whose declared param at position i is Str
                // (handles GL, SDL, and other runtime functions generically).
                // Skip functions that actually take const char* at the C level (takes_cstr_for_str_param).
                else if is_str_lit && (name.starts_with("gorget_str_") || force_str_coerce || trait_str_arg_positions.contains(&i)
                    || (ext_param_is_str(ext_params, i, &module.structs) && !takes_cstr_for_str_param(emit_name, i))
                    || runtime_fn_str_param(emit_name, i))
                    && !str_fn_non_str_arg(name, i) {
                    write!(out, "gorget_str_from_literal({}, strlen({}))", v(*a), v(*a)).unwrap();
                }
                // Ptr arg to a trait box method or runtime function that expects Str → deref as *(Str*).
                else if (trait_str_arg_positions.contains(&i) || runtime_fn_str_param(emit_name, i))
                    && matches!(arg_ty, Some(LirType::Ptr)) && !is_str_lit {
                    write!(out, "*(Str*){}", v(*a)).unwrap();
                }
                // GorgetString arg to a gorget_str_* function → coerce to Str.
                else if name.starts_with("gorget_str_") && is_gorget_string_type(arg_ty, sn)
                    && !str_fn_non_str_arg(name, i) {
                    write!(out, "(Str){{ .data = ({v}).data, .len = ({v}).len, .cap = 0, .alloc = NULL }}", v = v(*a)).unwrap();
                }
                // Ptr arg to a gorget_str_* function that expects Str by value → deref to Str.
                // Skip self-by-ptr methods (they take GorgetString*) and arg 0 of ptr methods.
                else if name.starts_with("gorget_str_") && matches!(arg_ty, Some(LirType::Ptr))
                    && !runtime_arg_by_ptr(emit_name, i)
                    && !str_fn_non_str_arg(name, i) {
                    write!(out, "*(Str*){}", v(*a)).unwrap();
                }
                // Collection void* element params — wrap concrete values with &(Type){val}.
                else if void_params.contains(&i) && arg_ty.map_or(false, |t| !matches!(t, LirType::Ptr)) {
                    let ty_name = c_type_named(arg_ty.unwrap(), sn);
                    write!(out, "&({ty_name}){{ {} }}", v(*a)).unwrap();
                }
                // String literal arg to a void* collection param → wrap as &(Str){...}.
                // This handles Dict/Set with Str keys: gorget_map_put(m, &(Str){..}, &val).
                else if void_params.contains(&i) && is_str_lit {
                    write!(out, "&(Str){{ .data = {v}, .len = strlen({v}), .cap = 0, .alloc = NULL }}", v = v(*a)).unwrap();
                }
                // Str → const char* coercion for C runtime functions that take raw strings.
                else if takes_cstr_for_str_param(emit_name, i) {
                    let is_str_struct = matches!(arg_ty, Some(LirType::Struct(sid)) if {
                        module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetStringView" || s.name == "GorgetString")
                    });
                    let is_ptr_val = matches!(arg_ty, Some(LirType::Ptr));
                    // Functions like gorget_parse_int/float need a null-terminated string.
                    // GorgetStringView slices are NOT null-terminated, so use gorget_str_to_cstr().
                    // String literals are always null-terminated and can use .data directly.
                    let needs_null_term = needs_null_terminated_cstr(emit_name);
                    if is_str_lit {
                        write!(out, "{}", v(*a)).unwrap();
                    } else if needs_null_term && is_str_struct {
                        write!(out, "gorget_str_to_cstr({})", v(*a)).unwrap();
                    } else if needs_null_term && is_ptr_val {
                        write!(out, "gorget_str_to_cstr(*(Str*){})", v(*a)).unwrap();
                    } else if is_str_struct {
                        write!(out, "({}).data", v(*a)).unwrap();
                    } else if is_ptr_val {
                        write!(out, "((Str*){})->data", v(*a)).unwrap();
                    } else {
                        write!(out, "{}", v(*a)).unwrap();
                    }
                }
                // gorget_int_to_str / gorget_float_to_str: always cast arg to expected type.
                // The LIR lowerer emits str() coercion for unknown source types, which can
                // produce void* args. macOS clang rejects implicit void*→int64_t conversion.
                // Casting is a no-op when the arg is already the correct type.
                else if emit_name == "gorget_int_to_str" || emit_name == "gorget_float_to_str" {
                    let cast_ty = if emit_name == "gorget_float_to_str" { "double" } else { "int64_t" };
                    write!(out, "({cast_ty}){}", v(*a)).unwrap();
                }
                // Use general coercion for extern params.
                else {
                    let ext_param = ext_params.and_then(|p| p.get(i));
                    emit_coerced_arg(out, a, ext_param, val_types, str_lit_vals, sn);
                }
            }
            if deref_clone_extra_close {
                write!(out, "));").unwrap();
            } else {
                write!(out, ");").unwrap();
            }
            // Deep-clone resource fields in user structs read from collections.
            if let Some(ops) = deref_deep_clone_ops {
                for op in ops {
                    write!(out, " {op}").unwrap();
                }
            }

            // Set elem_drop/val_drop on collection constructors.
            // Uses original_name to determine element type from the monomorphized name.
            if let Some(orig) = original_name.as_ref() {
                if let Some(d) = dst {
                    let dv = format!("__v{}", d.0);
                    // Vector/Array constructor: set elem_drop
                    if (name.starts_with("gorget_array_new") || name == "gorget_array_with_capacity")
                        && (orig.starts_with("Vector__") || orig.starts_with("Deque__"))
                    {
                        let raw_elem = orig.strip_prefix("Vector__")
                            .or_else(|| orig.strip_prefix("Deque__"))
                            .unwrap_or("");
                        // Strip method suffix (__new, __with_capacity, etc.):
                        // "Tracked__new" → "Tracked", "Vector__int64_t__new" → "Vector__int64_t"
                        let elem_type = raw_elem.strip_suffix("__new")
                            .or_else(|| raw_elem.strip_suffix("__with_capacity"))
                            .unwrap_or(raw_elem);
                        if let Some(drop_fn) = elem_drop_fn_for_c_type(elem_type) {
                            write!(out, " {dv}.elem_drop = (__gorget_drop_fn){drop_fn};").unwrap();
                        }
                        // Note: Custom/Recursive types do NOT get elem_drop set here
                        // because the GIR generates explicit pre-drops for set/remove.
                        // Setting elem_drop would cause double-drops.
                        if let Some(clone_fn) = elem_clone_fn_for_c_type(elem_type) {
                            write!(out, " {dv}.elem_clone = (__gorget_drop_fn){clone_fn};").unwrap();
                        } else if module.recursive_drop_structs.contains_key(elem_type)
                            || module.recursive_drop_enums.contains_key(elem_type)
                        {
                            write!(out, " {dv}.elem_clone = (__gorget_drop_fn){elem_type}__clone_inplace;").unwrap();
                        }
                    }
                    // Dict/HashMap constructor: set val_drop + val_clone
                    if (name.starts_with("gorget_dict_new") || name.starts_with("gorget_map_new"))
                        && (orig.starts_with("Dict__") || orig.starts_with("HashMap__"))
                    {
                        let prefix = if orig.starts_with("Dict__") { "Dict__" } else { "HashMap__" };
                        if let Some(rest) = orig.strip_prefix(prefix) {
                            // Strip constructor suffixes like __new, __new_str
                            let rest_stripped = rest.strip_suffix("__new_str")
                                .or_else(|| rest.strip_suffix("__new"))
                                .unwrap_or(rest);
                            if let Some(pos) = rest_stripped.find("__") {
                                let val_type = &rest_stripped[pos + 2..];
                                if let Some(drop_fn) = elem_drop_fn_for_c_type(val_type) {
                                    write!(out, " {dv}.val_drop = (__gorget_drop_fn){drop_fn};").unwrap();
                                }
                                // Note: Custom/Recursive types do NOT get val_drop set here.
                                // GIR generates explicit pre-drops for map put/remove.
                                if let Some(clone_fn) = elem_clone_fn_for_c_type(val_type) {
                                    write!(out, " {dv}.val_clone = (__gorget_drop_fn){clone_fn};").unwrap();
                                } else if module.recursive_drop_structs.contains_key(val_type)
                                    || module.recursive_drop_enums.contains_key(val_type)
                                {
                                    write!(out, " {dv}.val_clone = (__gorget_drop_fn){val_type}__clone_inplace;").unwrap();
                                }
                            }
                        }
                    }
                }
            }

            // Post-push zeroing: after gorget_array_push / gorget_map_put / gorget_set_add / gorget_heap_push,
            // if the element argument points to a collection-type value (GorgetArray, GorgetMap, GorgetSet,
            // GorgetString, GorgetClosure), zero the source to prevent double-free from shallow-copy aliasing.
            let zero_arg_indices: &[usize] = match name.as_str() {
                "gorget_array_push" | "gorget_heap_push" => &[1],
                "gorget_array_set" | "gorget_array_insert" => &[2],
                "gorget_map_put" => &[1, 2],
                "gorget_set_add" => &[1],
                "gorget_channel_send" => &[1],
                _ => &[],
            };
            for &idx in zero_arg_indices {
                if let Some(arg_val) = args.get(idx) {
                    if let Some(Some(LirType::Struct(pt_sid))) = ptr_pointee.get(arg_val.0 as usize) {
                        // Only zero direct resource types (GorgetArray, GorgetMap, etc.)
                        // after push/set/send.  User structs containing resources are
                        // handled via Custom/Recursive drop guards (memcmp zero check).
                        if is_direct_resource_type(*pt_sid, module) {
                            let struct_name = module.structs.get(pt_sid.0 as usize)
                                .map(|s| s.name.as_str())
                                .unwrap_or("");
                            if struct_name == "GorgetString" || struct_name == "GorgetStringView" {
                                // For unified str/String: zero only the cap field
                                // (demote to view) instead of zeroing the entire struct.
                                // This prevents double-free (gorget_string_free checks
                                // cap==0) while preserving data/len for subsequent reads,
                                // since str is a copy type in Gorget.
                                write!(out, "\n    ((GorgetString*){p})->cap = 0; ((GorgetString*){p})->alloc = NULL;", p = v(*arg_val)).unwrap();
                            } else {
                                let sn_name = module.structs.get(pt_sid.0 as usize)
                                    .map(|_s| c_type_named(&LirType::Struct(*pt_sid), sn))
                                    .unwrap_or_else(|| "void".to_string());
                                write!(out, "\n    memset({}, 0, sizeof({}));", v(*arg_val), sn_name).unwrap();
                            }
                        }
                    }
                }
            }
        }
        Inst::CallPtr { dst, callee, args } => {
            if let Some(d) = dst {
                write!(out, "{} = ", v(*d)).unwrap();
            }
            // Build the function pointer cast using actual arg types instead of void*
            // to avoid ABI mismatches with struct-by-value parameters.
            let ret_ty = dst.and_then(|d| val_types.get(d.0 as usize).and_then(|t| t.as_ref()))
                .map(|t| c_type_named(t, sn))
                .unwrap_or_else(|| "void".to_string());
            write!(out, "(({ret_ty}(*)(").unwrap();
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                let pointee = ptr_pointee.get(a.0 as usize).and_then(|t| t.as_ref());
                if let Some(pt) = pointee {
                    if pt.is_aggregate() {
                        // Pointer-to-aggregate: the target function likely expects by-value.
                        // Use the actual struct type.
                        write!(out, "{}", c_type_named(pt, sn)).unwrap();
                        continue;
                    }
                }
                match arg_ty {
                    Some(t) if t.is_aggregate() => write!(out, "{}", c_type_named(t, sn)).unwrap(),
                    _ => write!(out, "void*").unwrap(),
                }
            }
            write!(out, "))({}))(", v(*callee)).unwrap();
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                let pointee = ptr_pointee.get(a.0 as usize).and_then(|t| t.as_ref());
                if let Some(pt) = pointee {
                    if pt.is_aggregate() {
                        // Dereference pointer to get the struct by value.
                        write!(out, "*({}*){}", c_type_named(pt, sn), v(*a)).unwrap();
                        continue;
                    }
                }
                match arg_ty {
                    Some(t) if t.is_aggregate() => write!(out, "{}", v(*a)).unwrap(),
                    _ => write!(out, "(void*){}", v(*a)).unwrap(),
                }
            }
            write!(out, ");").unwrap();
        }

        // Runtime checks
        Inst::BoundsCheck { index, len } => {
            write!(
                out,
                "if ((uint64_t){} >= (uint64_t){}) {{ fprintf(stderr, \"index out of bounds\\n\"); abort(); }}",
                v(*index),
                v(*len)
            ).unwrap();
        }
        Inst::DivCheck { divisor } => {
            write!(
                out,
                "if ({} == 0) {{ fprintf(stderr, \"division by zero\\n\"); abort(); }}",
                v(*divisor)
            ).unwrap();
        }
        Inst::Trap { msg } => {
            write!(out, "fprintf(stderr, \"{}\"); abort();", escape_c_string(msg)).unwrap();
        }

        // Printf
        Inst::Printf { fmt, args } => {
            write!(out, "printf(\"{}\"", escape_c_string(fmt)).unwrap();
            for a in args {
                write!(out, ", {}", v(*a)).unwrap();
            }
            write!(out, ");").unwrap();
        }
        Inst::Fprintf { fd, fmt, args } => {
            // fd is a FILE* or fd int — for now treat as FILE*.
            write!(out, "fprintf((FILE*){}, \"{}\"", v(*fd), escape_c_string(fmt)).unwrap();
            for a in args {
                write!(out, ", {}", v(*a)).unwrap();
            }
            write!(out, ");").unwrap();
        }

        Inst::Nop => {
            write!(out, "/* nop */;").unwrap();
        }

        Inst::InlineC { dst, code } => {
            // Emit inline C code. For assignment patterns like `_X = expr;`,
            // rewrite `_X` to `__vN` (the SSA value name).
            if let Some(d) = dst {
                // Parse `_X = expr;` and rewrite to `__vN = expr;`
                if let Some(eq_pos) = code.find(" = ") {
                    let expr = &code[eq_pos + 3..];
                    // Rewrite local references `_N` to slot names `__sN` in the expression.
                    let rewritten = rewrite_inline_c_locals(expr, func);
                    write!(out, "{} = {};", v(*d), rewritten.trim_end_matches(';')).unwrap();
                } else {
                    write!(out, "/* inline_c: {} */;", code).unwrap();
                }
            } else {
                let rewritten = rewrite_inline_c_locals(code, func);
                write!(out, "{}", rewritten).unwrap();
            }
        }
    }
}

/// Rewrite GIR local references (`_N`) in inline C code to LIR slot names (`__sN`).
fn rewrite_inline_c_locals(code: &str, func: &LirFunction) -> String {
    // Simple regex-free approach: find `_N` patterns and replace with `__sN`.
    let mut result = String::with_capacity(code.len() + 16);
    let bytes = code.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'_' && (i == 0 || !bytes[i-1].is_ascii_alphanumeric()) {
            // Check if followed by digits
            let start = i + 1;
            let mut end = start;
            while end < bytes.len() && bytes[end].is_ascii_digit() {
                end += 1;
            }
            if end > start && (end >= bytes.len() || !bytes[end].is_ascii_alphanumeric()) {
                let num: u32 = code[start..end].parse().unwrap_or(0);
                // Map GIR local index to LIR slot if possible
                if (num as usize) < func.slots.len() {
                    result.push_str(&format!("__s{}", num));
                } else {
                    result.push('_');
                    result.push_str(&code[start..end]);
                }
                i = end;
                continue;
            }
        }
        result.push(bytes[i] as char);
        i += 1;
    }
    result
}

fn emit_term(out: &mut String, term: &Term, func: &LirFunction, module: &LirModule, sn: &HashMap<u32, String>, val_types: &[Option<LirType>]) {
    let v = |id: ValueId| -> String { format!("__v{}", id.0) };

    match term {
        Term::Ret(val) => {
            // For throws-int main, unwrap Result to exit code.
            let is_throws_main = func.name == "main" && matches!(&func.return_type, LirType::Struct(sid) if {
                module.structs.get(sid.0 as usize).map_or(false, |s| s.name.starts_with("Result__"))
            });
            if is_throws_main {
                let val_ty = val_types.get(val.0 as usize).and_then(|t| t.as_ref());
                let ty_name = c_type_named(&func.return_type, sn);
                let val_expr = if matches!(val_ty, Some(LirType::Ptr)) {
                    format!("*({ty_name}*){}", v(*val))
                } else {
                    v(*val)
                };
                write!(out, "{{ {ty_name} __res = {val_expr}; if (__res.tag == 0) {{ return 0; }} else {{ return __res.Error_0; }} }}").unwrap();
                return;
            }
            // If the function returns an aggregate but the value is a pointer, dereference.
            let val_ty = val_types.get(val.0 as usize).and_then(|t| t.as_ref());
            if func.return_type.is_aggregate() && matches!(val_ty, Some(LirType::Ptr)) {
                let ty_name = c_type_named(&func.return_type, sn);
                write!(out, "return *({ty_name}*){};", v(*val)).unwrap();
            } else {
                write!(out, "return {};", v(*val)).unwrap();
            }
        }
        Term::RetVoid => {
            write!(out, "return;").unwrap();
        }
        Term::Jump(target, args) => {
            emit_jump_args(out, *target, args, func);
            write!(out, "goto __bb{};", target.0).unwrap();
        }
        Term::Branch {
            cond,
            then_block,
            then_args,
            else_block,
            else_args,
        } => {
            writeln!(out, "if ({}) {{", v(*cond)).unwrap();
            if !then_args.is_empty() {
                write!(out, "        ").unwrap();
                emit_jump_args(out, *then_block, then_args, func);
                writeln!(out).unwrap();
            }
            writeln!(out, "        goto __bb{};", then_block.0).unwrap();
            writeln!(out, "    }} else {{").unwrap();
            if !else_args.is_empty() {
                write!(out, "        ").unwrap();
                emit_jump_args(out, *else_block, else_args, func);
                writeln!(out).unwrap();
            }
            writeln!(out, "        goto __bb{};", else_block.0).unwrap();
            write!(out, "    }}").unwrap();
        }
        Term::Switch {
            value,
            cases,
            default,
            default_args,
        } => {
            writeln!(out, "switch ((int64_t){}) {{", v(*value)).unwrap();
            for (val, block, args) in cases {
                write!(out, "        case {val}: ").unwrap();
                emit_jump_args(out, *block, args, func);
                writeln!(out, "goto __bb{};", block.0).unwrap();
            }
            write!(out, "        default: ").unwrap();
            emit_jump_args(out, *default, default_args, func);
            writeln!(out, "goto __bb{};", default.0).unwrap();
            write!(out, "    }}").unwrap();
        }
        Term::Unreachable => {
            write!(out, "__builtin_unreachable();").unwrap();
        }
    }
}

/// Emit parallel-copy assignments for block parameter passing.
/// Stores args into the target block's param temporaries (__bp{vid}).
fn emit_jump_args(out: &mut String, target: BlockId, args: &[ValueId], func: &LirFunction) {
    if args.is_empty() {
        return;
    }
    let target_block = &func.blocks[target.0 as usize];
    for (arg, (param_vid, _)) in args.iter().zip(target_block.params.iter()) {
        write!(out, "__bp{} = __v{}; ", param_vid.0, arg.0).unwrap();
    }
}

fn emit_global_init(out: &mut String, init: &LirGlobalInit, ty: &LirType, funcs: &[LirFunction], structs: &[StructDef]) {
    write!(out, " = ").unwrap();
    emit_global_init_value(out, init, ty, funcs, structs);
}

fn emit_global_init_value(out: &mut String, init: &LirGlobalInit, ty: &LirType, funcs: &[LirFunction], structs: &[StructDef]) {
    match init {
        LirGlobalInit::Zeroed => write!(out, "{{0}}").unwrap(),
        LirGlobalInit::Bytes(b) => {
            let is_float = matches!(ty, LirType::F32 | LirType::F64);
            match (b.len(), is_float) {
                (4, true) => {
                    let val = f32::from_le_bytes([b[0], b[1], b[2], b[3]]);
                    if val.is_finite() {
                        write!(out, "{val:.17e}").unwrap();
                    } else {
                        write!(out, "{val}").unwrap();
                    }
                }
                (8, true) => {
                    let val = f64::from_le_bytes([b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7]]);
                    if val.is_finite() {
                        write!(out, "{val:.17e}").unwrap();
                    } else {
                        write!(out, "{val}").unwrap();
                    }
                }
                (1, _) => write!(out, "{}", b[0] as i8).unwrap(),
                (2, _) => write!(out, "{}", i16::from_le_bytes([b[0], b[1]])).unwrap(),
                (4, _) => write!(out, "{}", i32::from_le_bytes([b[0], b[1], b[2], b[3]])).unwrap(),
                (8, _) => write!(out, "{}LL", i64::from_le_bytes([b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7]])).unwrap(),
                _ => write!(out, "{{0}} /* {} bytes */", b.len()).unwrap(),
            }
        }
        LirGlobalInit::FuncAddr(fid) => {
            let fname = funcs.get(fid.0 as usize).map(|f| f.name.as_str()).unwrap_or("__unknown_fn");
            write!(out, "(void*)&{fname}").unwrap();
        }
        LirGlobalInit::Struct { struct_id, fields } => {
            write!(out, "{{").unwrap();
            let field_types: Option<&[(String, LirType)]> = structs.get(struct_id.0 as usize)
                .map(|sd| sd.fields.as_slice());
            for (i, f) in fields.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                let ft = field_types.and_then(|fts| fts.get(i).map(|(_, t)| t)).unwrap_or(&LirType::I64);
                emit_global_init_value(out, f, ft, funcs, structs);
            }
            write!(out, "}}").unwrap();
        }
        LirGlobalInit::RuntimeCall(_) => {
            // Runtime calls are initialized separately in __gorget_init_globals.
            write!(out, "{{0}}").unwrap();
        }
    }
}

/// Map LirType to C type string.
/// Returns true if the function is provided by standard C headers
/// (stdio.h, stdlib.h, string.h) and should not be re-declared.
/// Emit a coerced argument value.
/// Handles: Ptr→Str (string literal wrapping), Ptr→Aggregate (dereference), GorgetString→Str.
fn emit_coerced_arg(
    out: &mut String,
    a: &ValueId,
    param_ty: Option<&LirType>,
    val_types: &[Option<LirType>],
    str_lit_vals: &[bool],
    sn: &HashMap<u32, String>,
) {
    let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
    let is_str_lit = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
    let param_name = param_ty.map(|t| c_type_named(t, sn));
    let arg_name = arg_ty.map(|t| c_type_named(t, sn));

    // GorgetString → Str coercion (works for both pointer and value args).
    if param_name.as_deref() == Some("Str") && arg_name.as_deref() == Some("GorgetString") {
        // GorgetString by value (from ParamRef or Call result):
        write!(out, "(Str){{ .data = ({v}).data, .len = ({v}).len, .cap = 0, .alloc = NULL }}", v = format!("__v{}", a.0)).unwrap();
        return;
    }

    // Str → const char* coercion: when the extern expects a pointer but we have a Str struct.
    if matches!(param_ty, Some(LirType::Ptr)) && (arg_name.as_deref() == Some("Str") || arg_name.as_deref() == Some("GorgetString")) {
        write!(out, "(__v{}).data", a.0).unwrap();
        return;
    }

    if param_ty.map_or(false, |t| t.is_aggregate()) && matches!(arg_ty, Some(LirType::Ptr)) {
        let ty_name = param_name.as_deref().unwrap_or("void");
        if is_str_lit && ty_name == "Str" {
            write!(out, "gorget_str_from_literal({v}, strlen({v}))", v = format!("__v{}", a.0)).unwrap();
        } else if is_str_lit && ty_name == "GorgetString" {
            // String literal → GorgetString: wrap with gorget_string_new.
            write!(out, "gorget_string_new({})", format!("__v{}", a.0)).unwrap();
        } else if ty_name == "Str" {
            // Ptr to Str (from SlotAddr of GorgetString slot?) — try coercion.
            // Check if the slot is a GorgetString.
            write!(out, "*({ty_name}*)__v{}", a.0).unwrap();
        } else {
            write!(out, "*({ty_name}*)__v{}", a.0).unwrap();
        }
    } else {
        write!(out, "__v{}", a.0).unwrap();
    }
}

/// Returns true if the LIR type is a GorgetString struct.
fn is_gorget_string_type(ty: Option<&LirType>, sn: &HashMap<u32, String>) -> bool {
    if let Some(LirType::Struct(sid)) = ty {
        let name = sn.get(&sid.0).map(|s| s.as_str()).unwrap_or("");
        name == "GorgetString"
    } else {
        false
    }
}

/// Emit drop functions for user structs with Recursive drop strategy.
/// When a struct has fields that need dropping (e.g., GorgetString), the drop
/// elaboration marks it as Recursive. When that struct appears as a field in
/// another struct, the parent's drop emits a call to `{Name}__drop`. This
/// function generates the actual `{Name}__drop` function body.
fn emit_recursive_struct_drops(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    for (idx, sdef) in module.structs.iter().enumerate() {
        let type_name = &sdef.name;

        // Check if this is a struct that needs a Recursive drop function
        let drop_info = match module.recursive_drop_structs.get(type_name.as_str()) {
            Some(info) => info,
            None => continue,
        };

        // Check if a drop function already exists (custom Drop trait impl)
        let drop_fn_name = format!("{type_name}__drop");
        if module.functions.iter().any(|f| f.name == drop_fn_name) {
            continue;
        }

        // Use the C struct name (e.g., __lir_s10) instead of the Gorget name
        let c_name = sn.get(&(idx as u32)).cloned().unwrap_or_else(|| type_name.clone());

        // Generate the drop function
        writeln!(out, "static inline void {drop_fn_name}({c_name}* self) {{").unwrap();
        for (field_name, drop_fn, field_type_name) in drop_info {
            // For collection fields, emit per-element drops before freeing.
            let is_array_free = drop_fn == "gorget_array_free";
            let is_map_free = drop_fn == "gorget_map_free";
            if is_array_free || is_map_free {
                let elem_type_name = if is_array_free {
                    field_type_name.strip_prefix("Vector__")
                        .or_else(|| field_type_name.strip_prefix("Deque__"))
                } else {
                    field_type_name.strip_prefix("Dict__")
                        .or_else(|| field_type_name.strip_prefix("HashMap__"))
                        .and_then(|rest| rest.find("__").map(|idx| &rest[idx + 2..]))
                };
                if let Some(elem_name) = elem_type_name {
                    if let Some(recipe) = module.elem_drop_recipes.get(elem_name) {
                        let addr = format!("&self->{field_name}");
                        write!(out, "    ").unwrap();
                        if is_array_free {
                            emit_recipe_array_drop(out, &addr, recipe, module, sn, 0);
                        } else {
                            emit_recipe_map_drop(out, &addr, recipe, module, sn, 0);
                        }
                        writeln!(out).unwrap();
                    }
                }
            }
            writeln!(out, "    {drop_fn}(&self->{field_name});").unwrap();
        }
        writeln!(out, "}}").unwrap();
        writeln!(out).unwrap();
    }
}

/// Emit per-type clone functions for structs with Recursive drop strategy.
/// These produce independently-owned deep copies by memcpy + per-field clone.
/// Called from collection reads (IndexLoad, Option unwrap) so extracted elements
/// don't share resource field buffers with the collection.
fn emit_recursive_struct_clones(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    for (idx, sdef) in module.structs.iter().enumerate() {
        let type_name = &sdef.name;

        let drop_info = match module.recursive_drop_structs.get(type_name.as_str()) {
            Some(info) => info,
            None => continue,
        };

        // Skip if a user-defined clone already exists
        let clone_fn_name = format!("{type_name}__clone");
        if module.functions.iter().any(|f| f.name == clone_fn_name) {
            continue;
        }

        let c_name = sn.get(&(idx as u32)).cloned().unwrap_or_else(|| type_name.clone());

        // Generate: TypeName__clone(void* __p) → T with deep-cloned resource fields
        // NOT static — the IndexLoad path emits a non-static extern declaration.
        writeln!(out, "{c_name} {clone_fn_name}(void* __p) {{").unwrap();
        writeln!(out, "    {c_name} dst = *({c_name}*)__p;").unwrap();
        for (field_name, drop_fn, _field_type_name) in drop_info {
            // Map drop function → clone function
            let clone_fn = match drop_fn.as_str() {
                "gorget_string_free" => "gorget_string_clone",
                "gorget_array_free" => "gorget_array_clone",
                "gorget_map_free" => "gorget_map_clone",
                "gorget_set_free" => "gorget_set_clone",
                other if other.ends_with("__drop") => {
                    // Recursive or Custom-drop field: call its clone function if it exists.
                    // For Recursive fields, __clone is generated by this same pass.
                    // For Custom-drop fields, use deep_clone_resource_fields inline.
                    let base = &other[..other.len() - 6]; // strip "__drop"
                    let inner_clone = format!("{base}__clone");
                    // Check if this inner type also has a Recursive clone (will be generated)
                    if module.recursive_drop_structs.contains_key(base) {
                        writeln!(out, "    dst.{field_name} = {inner_clone}(&dst.{field_name});").unwrap();
                        continue;
                    }
                    // Custom-drop field: clone resource fields inline via deep_clone_resource_fields
                    if let Some((inner_sid, _)) = module.structs.iter().enumerate()
                        .find(|(_, s)| s.name == base)
                    {
                        if let Some(ops) = deep_clone_resource_fields(
                            crate::lir::StructId(inner_sid as u32),
                            &format!("dst.{field_name}"),
                            module,
                        ) {
                            for op in ops {
                                writeln!(out, "    {op}").unwrap();
                            }
                        }
                    }
                    continue;
                }
                _ => continue, // Unknown drop — skip cloning this field
            };
            writeln!(out, "    dst.{field_name} = {clone_fn}(&dst.{field_name});").unwrap();
        }
        writeln!(out, "    return dst;").unwrap();
        writeln!(out, "}}").unwrap();
        // In-place wrapper for use as elem_clone/val_clone function pointer.
        writeln!(out, "void {clone_fn_name}_inplace(void* __p) {{ *({c_name}*)__p = {clone_fn_name}(__p); }}").unwrap();
        writeln!(out).unwrap();
    }
}

/// Emit per-type clone functions for ENUM types with Recursive drop.
/// Uses tag-based dispatch to clone the active variant's resource fields.
fn emit_recursive_enum_clones(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    for (idx, sdef) in module.structs.iter().enumerate() {
        let type_name = &sdef.name;

        let variant_info = match module.recursive_drop_enums.get(type_name.as_str()) {
            Some(info) => info,
            None => continue,
        };

        // Skip if a user-defined clone already exists
        let clone_fn_name = format!("{type_name}__clone");
        if module.functions.iter().any(|f| f.name == clone_fn_name) {
            continue;
        }

        let c_name = sn.get(&(idx as u32)).cloned().unwrap_or_else(|| type_name.clone());

        // Map drop function → clone function
        fn drop_to_clone(drop_fn: &str) -> String {
            match drop_fn {
                "gorget_string_free" => "gorget_string_clone".into(),
                "gorget_array_free" => "gorget_array_clone".into(),
                "gorget_map_free" => "gorget_map_clone".into(),
                "gorget_set_free" => "gorget_set_clone".into(),
                other if other.ends_with("__drop") => {
                    let base = &other[..other.len() - 6];
                    format!("{base}__clone")
                }
                _ => String::new(),
            }
        }

        writeln!(out, "{c_name} {clone_fn_name}(void* __p) {{").unwrap();
        writeln!(out, "    {c_name} dst = *({c_name}*)__p;").unwrap();
        writeln!(out, "    switch (dst.tag) {{").unwrap();

        // Group variant_info by variant index
        let mut by_variant: std::collections::HashMap<u32, Vec<(&str, &str, &str, &str)>> = std::collections::HashMap::new();
        for (vi, vname, field_name, drop_fn, field_type_name) in variant_info {
            by_variant.entry(*vi).or_default().push((vname, field_name, drop_fn, field_type_name));
        }

        let mut indices: Vec<u32> = by_variant.keys().copied().collect();
        indices.sort();
        for vi in indices {
            let fields = &by_variant[&vi];
            write!(out, "        case {vi}: ").unwrap();
            for (variant_name, field_name, drop_fn, _field_type_name) in fields {
                let clone_fn = drop_to_clone(drop_fn);
                if !clone_fn.is_empty() {
                    // Count how many LIR struct fields belong to this variant
                    let variant_prefix = format!("{variant_name}_");
                    let variant_field_count = sdef.fields.iter()
                        .filter(|(n, _)| n.starts_with(&variant_prefix))
                        .count();
                    let access = if sdef.is_enum && variant_field_count > 1 {
                        // Union layout, multi-field variant: data.{Variant}.{Field}
                        format!("data.{variant_name}.{field_name}")
                    } else if sdef.is_enum {
                        // Union layout, single-field variant: data.{Field}
                        format!("data.{field_name}")
                    } else {
                        // Flat layout: {Field}
                        field_name.to_string()
                    };
                    write!(out, "dst.{access} = {clone_fn}(&dst.{access}); ").unwrap();
                }
            }
            writeln!(out, "break;").unwrap();
        }

        writeln!(out, "    }}").unwrap();
        writeln!(out, "    return dst;").unwrap();
        writeln!(out, "}}").unwrap();
        writeln!(out, "void {clone_fn_name}_inplace(void* __p) {{ *({c_name}*)__p = {clone_fn_name}(__p); }}").unwrap();
        writeln!(out).unwrap();
    }
}

/// Emit drop functions for enums with resource-type variant payloads.
/// These are called explicitly from the GIR reassignment path for
/// enums that have needs_drop=true but DropStrategy::None.
fn emit_enum_drop_fns(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    use std::fmt::Write;
    for (idx, sdef) in module.structs.iter().enumerate() {
        let type_name = &sdef.name;
        let variant_info = match module.recursive_drop_enums.get(type_name.as_str()) {
            Some(info) => info,
            None => continue,
        };
        let drop_fn_name = format!("{type_name}__drop");
        // Skip if already generated by emit_recursive_struct_drops or user-defined
        if module.functions.iter().any(|f| f.name == drop_fn_name) {
            continue;
        }
        // Skip types that have a real DropStrategy (already handled)
        // We only want enums with None strategy that have resource payloads
        // These are NOT in recursive_drop_structs (that's for structs)
        if module.recursive_drop_structs.contains_key(type_name.as_str()) {
            continue;
        }
        let c_name = sn.get(&(idx as u32)).cloned().unwrap_or_else(|| type_name.clone());
        let mut by_variant: std::collections::HashMap<u32, Vec<(&str, &str, &str, &str)>> = std::collections::HashMap::new();
        for (vi, vname, field_name, drop_fn, field_type_name) in variant_info {
            by_variant.entry(*vi).or_default().push((vname, field_name, drop_fn, field_type_name));
        }
        if by_variant.is_empty() { continue; }
        writeln!(out, "void {drop_fn_name}(void* __p) {{").unwrap();
        writeln!(out, "    {c_name}* self = ({c_name}*)__p;").unwrap();
        writeln!(out, "    switch (self->tag) {{").unwrap();
        let mut indices: Vec<u32> = by_variant.keys().copied().collect();
        indices.sort();
        for vi in indices {
            let fields = &by_variant[&vi];
            write!(out, "        case {vi}: ").unwrap();
            for (variant_name, field_name, drop_fn, field_type_name) in fields {
                let variant_prefix = format!("{variant_name}_");
                let variant_field_count = sdef.fields.iter()
                    .filter(|(n, _)| n.starts_with(&variant_prefix))
                    .count();
                let access = if sdef.is_enum && variant_field_count > 1 {
                    format!("data.{variant_name}.{field_name}")
                } else if sdef.is_enum {
                    format!("data.{field_name}")
                } else {
                    field_name.to_string()
                };
                // For collection fields, emit per-element drops before freeing.
                let is_array_free = *drop_fn == "gorget_array_free";
                let is_map_free = *drop_fn == "gorget_map_free";
                if is_array_free || is_map_free {
                    let elem_type_name = if is_array_free {
                        field_type_name.strip_prefix("Vector__")
                            .or_else(|| field_type_name.strip_prefix("Deque__"))
                    } else {
                        field_type_name.strip_prefix("Dict__")
                            .or_else(|| field_type_name.strip_prefix("HashMap__"))
                            .and_then(|rest| rest.find("__").map(|idx| &rest[idx + 2..]))
                    };
                    if let Some(elem_name) = elem_type_name {
                        if let Some(recipe) = module.elem_drop_recipes.get(elem_name) {
                            let addr = format!("&self->{access}");
                            if is_array_free {
                                emit_recipe_array_drop(out, &addr, recipe, module, sn, 0);
                            } else {
                                emit_recipe_map_drop(out, &addr, recipe, module, sn, 0);
                            }
                            write!(out, " ").unwrap();
                        }
                    }
                }
                // Box fields (free): pass the pointer value directly.
                // Other drop fns (gorget_string_free, etc.): pass address of field.
                if *drop_fn == "free" {
                    write!(out, "free(self->{access}); ").unwrap();
                } else {
                    write!(out, "{drop_fn}(&self->{access}); ").unwrap();
                }
            }
            writeln!(out, "break;").unwrap();
        }
        writeln!(out, "    }}").unwrap();
        writeln!(out, "}}").unwrap();
        writeln!(out).unwrap();
    }
}

/// Emit typedefs and inline wrappers for monomorphized wrapper types
/// (Channel__T, Shared__T, Weak__T, AtomicInt, AtomicBool).
fn emit_monomorphized_typedefs(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    let mut type_seen = std::collections::HashSet::new();
    let mut method_seen = std::collections::HashSet::new();
    // Build original-name → C-name map for resolving element types in wrappers.
    let orig_to_c: HashMap<String, String> = module.structs.iter().enumerate()
        .map(|(i, def)| (def.name.clone(), sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"))))
        .collect();

    // Collect all wrapper type names from struct defs, struct_names, and spawned_fns.
    let mut type_names: Vec<String> = Vec::new();
    for def in &module.structs {
        if is_monomorphized_wrapper_type(&def.name) && type_seen.insert(def.name.clone()) {
            type_names.push(def.name.clone());
        }
    }
    for name in sn.values() {
        if is_monomorphized_wrapper_type(name) && type_seen.insert(name.clone()) {
            type_names.push(name.clone());
        }
    }
    for sf in &module.spawned_fns {
        for n in std::iter::once(&sf.ret_c_type).chain(sf.params.iter().map(|(_, t)| t)) {
            if is_monomorphized_wrapper_type(n) && type_seen.insert(n.clone()) {
                type_names.push(n.clone());
            }
        }
    }

    // Emit typedefs (skip unmonomorphized wrappers like Guard__T)
    for name in &type_names {
        if is_unmonomorphized_wrapper(name) { continue; }
        emit_wrapper_typedef(out, name, module, &orig_to_c);
    }

    // Collect all Channel/Shared/Weak/Mutex/RWLock method names from CallExtern instructions.
    let mut method_calls: Vec<String> = Vec::new();
    let is_wrapper_method = |n: &str| -> bool {
        n.starts_with("Channel__") || n.starts_with("Shared__")
        || n.starts_with("Weak__") || n.starts_with("Mutex__")
        || n.starts_with("RWLock__") || n.starts_with("Guard__")
        || n.starts_with("ReadGuard__") || n.starts_with("WriteGuard__")
        || n.starts_with("Box__")
    };
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::CallExtern { name, .. } = inst {
                    if is_wrapper_method(name) && method_seen.insert(name.clone()) {
                        method_calls.push(name.clone());
                    }
                }
            }
        }
    }
    // Also scan externs list
    for ext in &module.externs {
        if is_wrapper_method(&ext.name) && method_seen.insert(ext.name.clone()) {
            method_calls.push(ext.name.clone());
        }
    }
    // Synthesize clone method calls for refcounted types captured by spawn helpers.
    for sf in &module.spawned_fns {
        for (_idx, gir_name) in &sf.clone_params {
            let clone_name = format!("{gir_name}__clone");
            if method_seen.insert(clone_name.clone()) {
                method_calls.push(clone_name);
            }
        }
    }

    // First pass: discover types from method calls and emit all typedefs.
    // Also discover and typedef element types (e.g., Vector__int64_t inside Shared__Vector__int64_t).
    for name in &method_calls {
        let type_prefix = if let Some((tp, _)) = parse_channel_method(name) {
            Some(tp)
        } else if let Some((tp, _)) = parse_shared_method(name) {
            Some(tp)
        } else if let Some((tp, _)) = parse_weak_method(name) {
            Some(tp)
        } else if let Some((tp, _)) = parse_mutex_method(name) {
            Some(tp)
        } else if let Some((tp, _)) = parse_rwlock_method(name) {
            Some(tp)
        } else if let Some((tp, _)) = parse_box_method(name) {
            Some(tp)
        } else {
            None
        };
        if let Some(ref tp) = type_prefix {
            // Skip unmonomorphized generic wrappers (e.g. Shared__Vector__T)
            if is_unmonomorphized_wrapper(tp) { continue; }
            // Auto-discover element types that may also need typedefs.
            let elem_name = if tp.starts_with("Channel__") {
                channel_elem_type(tp).to_string()
            } else if tp.starts_with("Mutex__") {
                mutex_elem_type(tp).to_string()
            } else if tp.starts_with("RWLock__") {
                rwlock_elem_type(tp).to_string()
            } else if tp.starts_with("Box__") {
                box_elem_type(tp).to_string()
            } else if tp.starts_with("Guard__") || tp.starts_with("ReadGuard__") || tp.starts_with("WriteGuard__") {
                guard_elem_type(tp).to_string()
            } else {
                shared_elem_type(tp).to_string()
            };
            let resolved = resolve_elem_type(&elem_name, &orig_to_c);
            if is_monomorphized_wrapper_type(&resolved) && type_seen.insert(resolved.clone()) {
                emit_wrapper_typedef(out, &resolved, module, &orig_to_c);
            }
            if type_seen.insert(tp.clone()) {
                emit_wrapper_typedef(out, tp, module, &orig_to_c);
            }
        }
    }
    // Second pass: emit inline wrappers (now that all typedefs are in place).
    for name in &method_calls {
        // Extract the type prefix from whichever wrapper pattern matches.
        let tp = if let Some((tp, _)) = parse_channel_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_shared_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_weak_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_mutex_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_rwlock_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_guard_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_box_method(name) { Some(tp) }
            else { None };
        if let Some(ref tp) = tp {
            if is_unmonomorphized_wrapper(tp) { continue; }
        }
        if let Some((type_prefix, method)) = parse_channel_method(name) {
            let elem = resolve_elem_type(channel_elem_type(&type_prefix), &orig_to_c);
            emit_channel_wrapper(out, &type_prefix, method, &elem);
        } else if let Some((type_prefix, method)) = parse_shared_method(name) {
            let elem = resolve_elem_type(shared_elem_type(&type_prefix), &orig_to_c);
            emit_shared_wrapper(out, &type_prefix, method, &elem);
        } else if let Some((type_prefix, method)) = parse_weak_method(name) {
            emit_weak_wrapper(out, &type_prefix, method, &orig_to_c);
        } else if let Some((type_prefix, method)) = parse_mutex_method(name) {
            let elem = resolve_elem_type(mutex_elem_type(&type_prefix), &orig_to_c);
            emit_mutex_wrapper(out, &type_prefix, method, &elem);
        } else if let Some((type_prefix, method)) = parse_rwlock_method(name) {
            let elem = resolve_elem_type(rwlock_elem_type(&type_prefix), &orig_to_c);
            emit_rwlock_wrapper(out, &type_prefix, method, &elem);
        } else if let Some((type_prefix, method)) = parse_guard_method(name) {
            let elem = resolve_elem_type(guard_elem_type(&type_prefix), &orig_to_c);
            emit_guard_wrapper(out, &type_prefix, method, &elem);
        } else if let Some((type_prefix, method)) = parse_box_method(name) {
            let elem = resolve_elem_type(box_elem_type(&type_prefix), &orig_to_c);
            emit_box_wrapper(out, &type_prefix, method, &elem, module, &orig_to_c);
        }
    }

    writeln!(out).unwrap();
}

/// Parse `Channel__int64_t__send` → Some(("Channel__int64_t", "send"))
fn parse_channel_method(name: &str) -> Option<(String, &str)> {
    if !name.starts_with("Channel__") { return None; }
    let rest = &name["Channel__".len()..];
    // Find the last `__` that separates the type from the method.
    let method_sep = rest.rfind("__")?;
    let type_part = &rest[..method_sep];
    let method = &rest[method_sep + 2..];
    Some((format!("Channel__{type_part}"), method))
}

/// Parse `Shared__int64_t__get` → Some(("Shared__int64_t", "get"))
fn parse_shared_method(name: &str) -> Option<(String, &str)> {
    if !name.starts_with("Shared__") { return None; }
    let rest = &name["Shared__".len()..];
    let method_sep = rest.rfind("__")?;
    let type_part = &rest[..method_sep];
    let method = &rest[method_sep + 2..];
    Some((format!("Shared__{type_part}"), method))
}

/// Parse `Weak__int64_t__upgrade` → Some(("Weak__int64_t", "upgrade"))
fn parse_weak_method(name: &str) -> Option<(String, &str)> {
    if !name.starts_with("Weak__") { return None; }
    let rest = &name["Weak__".len()..];
    let method_sep = rest.rfind("__")?;
    let type_part = &rest[..method_sep];
    let method = &rest[method_sep + 2..];
    Some((format!("Weak__{type_part}"), method))
}

/// Parse `Mutex__int64_t__lock` → Some(("Mutex__int64_t", "lock"))
fn parse_mutex_method(name: &str) -> Option<(String, &str)> {
    if !name.starts_with("Mutex__") { return None; }
    let rest = &name["Mutex__".len()..];
    let method_sep = rest.rfind("__")?;
    let type_part = &rest[..method_sep];
    let method = &rest[method_sep + 2..];
    Some((format!("Mutex__{type_part}"), method))
}

/// Parse `RWLock__int64_t__read` → Some(("RWLock__int64_t", "read"))
fn parse_rwlock_method(name: &str) -> Option<(String, &str)> {
    if !name.starts_with("RWLock__") { return None; }
    let rest = &name["RWLock__".len()..];
    let method_sep = rest.rfind("__")?;
    let type_part = &rest[..method_sep];
    let method = &rest[method_sep + 2..];
    Some((format!("RWLock__{type_part}"), method))
}

/// Map a Channel wrapper type name to its C element type.
/// `Channel__int64_t` → `int64_t`, `Channel__Str` → `Str`
fn channel_elem_type(type_name: &str) -> &str {
    type_name.strip_prefix("Channel__").unwrap_or("int64_t")
}

/// Map a Shared/Weak wrapper type name to its C element type.
fn shared_elem_type(type_name: &str) -> &str {
    type_name.strip_prefix("Shared__")
        .or_else(|| type_name.strip_prefix("Weak__"))
        .unwrap_or("int64_t")
}

fn mutex_elem_type(type_name: &str) -> &str {
    type_name.strip_prefix("Mutex__").unwrap_or("int64_t")
}

fn rwlock_elem_type(type_name: &str) -> &str {
    type_name.strip_prefix("RWLock__").unwrap_or("int64_t")
}

fn guard_elem_type(type_name: &str) -> &str {
    type_name.strip_prefix("Guard__")
        .or_else(|| type_name.strip_prefix("ReadGuard__"))
        .or_else(|| type_name.strip_prefix("WriteGuard__"))
        .unwrap_or("int64_t")
}

fn parse_guard_method(name: &str) -> Option<(String, &str)> {
    let (prefix, rest) = if let Some(r) = name.strip_prefix("Guard__") {
        ("Guard__", r)
    } else if let Some(r) = name.strip_prefix("ReadGuard__") {
        ("ReadGuard__", r)
    } else if let Some(r) = name.strip_prefix("WriteGuard__") {
        ("WriteGuard__", r)
    } else {
        return None;
    };
    let method_sep = rest.rfind("__")?;
    let type_part = &rest[..method_sep];
    let method = &rest[method_sep + 2..];
    Some((format!("{prefix}{type_part}"), method))
}

fn emit_guard_wrapper(out: &mut String, type_name: &str, method: &str, elem: &str) {
    let is_read_guard = type_name.starts_with("ReadGuard__");
    let is_write_guard = type_name.starts_with("WriteGuard__");
    match method {
        "drop" => {
            let release_fn = if is_read_guard { "gorget_read_guard_release" }
                else if is_write_guard { "gorget_write_guard_release" }
                else { "gorget_guard_release" };
            writeln!(out, "static inline void {type_name}__{method}({type_name}* self) {{ {release_fn}(self); }}").unwrap();
        }
        "get" => {
            let get_fn = if is_read_guard { "gorget_read_guard_get" }
                else if is_write_guard { "gorget_write_guard_get" }
                else { "gorget_guard_get" };
            writeln!(out, "static inline {elem} {type_name}__get({type_name}* self) {{ return *({elem}*){get_fn}(self); }}").unwrap();
        }
        "get_ptr" => {
            let get_fn = if is_read_guard { "gorget_read_guard_get_ptr" }
                else if is_write_guard { "gorget_write_guard_get_ptr" }
                else { "gorget_guard_get_ptr" };
            writeln!(out, "static inline {elem}* {type_name}__get_ptr({type_name}* self) {{ return ({elem}*){get_fn}(self); }}").unwrap();
        }
        "set" => {
            let set_fn = if is_write_guard { "gorget_write_guard_set" }
                else { "gorget_guard_set" };
            writeln!(out, "static inline void {type_name}__set({type_name}* self, {elem} val) {{ {set_fn}(self, &val, sizeof({elem})); }}").unwrap();
        }
        _ => {}
    }
}

/// Emit a typedef for a monomorphized wrapper type.
fn emit_wrapper_typedef(out: &mut String, name: &str, module: &LirModule, orig_to_c: &HashMap<String, String>) {
    if name.starts_with("Channel__") {
        writeln!(out, "typedef GorgetChannel* {name};").unwrap();
    } else if name.starts_with("Shared__") || name.starts_with("Weak__") {
        writeln!(out, "typedef GorgetShared* {name};").unwrap();
    } else if name.starts_with("Vector__") {
        writeln!(out, "typedef GorgetArray {name};").unwrap();
    } else if name.starts_with("Dict__") || name.starts_with("HashMap__") {
        writeln!(out, "typedef GorgetMap {name};").unwrap();
    } else if name.starts_with("Set__") || name.starts_with("HashSet__") {
        writeln!(out, "typedef GorgetSet {name};").unwrap();
    } else if name.starts_with("Mutex__") {
        writeln!(out, "typedef GorgetMutex* {name};").unwrap();
    } else if name.starts_with("RWLock__") {
        writeln!(out, "typedef GorgetRWLock* {name};").unwrap();
    } else if name.starts_with("Guard__") {
        writeln!(out, "typedef gorget_guard_t {name};").unwrap();
    } else if name.starts_with("ReadGuard__") {
        writeln!(out, "typedef gorget_read_guard_t {name};").unwrap();
    } else if name.starts_with("WriteGuard__") {
        writeln!(out, "typedef gorget_write_guard_t {name};").unwrap();
    } else if name == "TaskGroup" {
        writeln!(out, "typedef gorget_task_group_t* TaskGroup;").unwrap();
    } else if name.starts_with("Box__") && is_trait_box(module, name) {
        // Trait object box — typedef to the TraitObj struct.
        let trait_name = name.strip_prefix("Box__").unwrap();
        let traitobj_orig = format!("{trait_name}_TraitObj");
        let traitobj_cname = orig_to_c.get(&traitobj_orig).cloned().unwrap_or(traitobj_orig);
        writeln!(out, "typedef {traitobj_cname} {name};").unwrap();
    } else if name.starts_with("Box__") {
        writeln!(out, "typedef void* {name};").unwrap();
    } else if name == "AtomicInt" {
        writeln!(out, "typedef GorgetAtomicInt* AtomicInt;").unwrap();
    } else if name == "AtomicBool" {
        writeln!(out, "typedef GorgetAtomicBool* AtomicBool;").unwrap();
    }
}

/// Resolve an element type name to its C name via the orig_to_c map.
/// Primitive types (int64_t, Str, bool, etc.) pass through unchanged.
fn resolve_elem_type(name: &str, orig_to_c: &HashMap<String, String>) -> String {
    orig_to_c.get(name).cloned().unwrap_or_else(|| name.to_string())
}

/// For Shared__Vector__T, extract the inner element type T from the type_name.
/// E.g., Shared__Vector__int64_t → int64_t, Shared__Vector__double → double.
/// If not a Shared__Vector pattern, returns `elem` unchanged (fallback).
fn shared_vector_inner_elem(type_name: &str, elem: &str) -> String {
    if let Some(rest) = type_name.strip_prefix("Shared__Vector__") {
        // rest is e.g. "int64_t", "double", "bool", "GorgetStringView"
        rest.to_string()
    } else {
        elem.to_string()
    }
}

/// Returns true if the wrapper type contains an unmonomorphized type parameter (like T, U).
fn is_unmonomorphized_wrapper(type_name: &str) -> bool {
    // Check if the element part after the wrapper prefix is a bare type variable
    for prefix in &["Shared__", "Channel__", "Mutex__", "RWLock__", "Guard__", "ReadGuard__", "WriteGuard__", "Box__", "Weak__"] {
        if let Some(rest) = type_name.strip_prefix(prefix) {
            if rest == "T" || rest == "U" || rest == "V" {
                return true;
            }
            // Also check Vector__T etc.
            if rest.starts_with("Vector__") {
                let inner = rest.strip_prefix("Vector__").unwrap_or("");
                if inner == "T" || inner == "U" || inner == "V" {
                    return true;
                }
            }
        }
    }
    false
}

fn emit_channel_wrapper(out: &mut String, type_name: &str, method: &str, elem: &str) {
    match method {
        "new" => writeln!(out, "static inline {type_name} {type_name}__new(int64_t cap) {{ return gorget_channel_new((size_t)cap, sizeof({elem})); }}").unwrap(),
        "send" => writeln!(out, "static inline void {type_name}__send({type_name}* self, {elem} val) {{ gorget_channel_send(*self, &val); }}").unwrap(),
        "recv" => writeln!(out, "static inline {elem} {type_name}__recv({type_name}* self) {{ {elem} __val; gorget_channel_recv(*self, &__val); return __val; }}").unwrap(),
        "close" => writeln!(out, "static inline void {type_name}__close({type_name}* self) {{ gorget_channel_close(*self); }}").unwrap(),
        "len" => writeln!(out, "static inline int64_t {type_name}__len({type_name}* self) {{ return gorget_channel_len(*self); }}").unwrap(),
        "capacity" => writeln!(out, "static inline int64_t {type_name}__capacity({type_name}* self) {{ return gorget_channel_capacity(*self); }}").unwrap(),
        "is_closed" => writeln!(out, "static inline bool {type_name}__is_closed({type_name}* self) {{ return gorget_channel_is_closed(*self); }}").unwrap(),
        "poll_send" => writeln!(out, "static inline bool {type_name}__poll_send({type_name}* self, {elem} val, GorgetWaker* waker) {{ return gorget_channel_poll_send(*self, &val, waker); }}").unwrap(),
        "poll_recv" => writeln!(out, "static inline bool {type_name}__poll_recv({type_name}* self, {elem}* outval, GorgetWaker* waker) {{ return gorget_channel_poll_recv(*self, outval, waker); }}").unwrap(),
        "recv_timeout" => writeln!(out, "static inline {elem} {type_name}__recv_timeout({type_name}* self, int64_t ms) {{ {elem} __val = {{0}}; gorget_channel_recv_timeout(*self, &__val, ms); return __val; }}").unwrap(),
        "clone" => writeln!(out, "static inline {type_name} {type_name}__clone({type_name} self) {{ return gorget_channel_retain(self); }}").unwrap(),
        "drop" => writeln!(out, "static inline void {type_name}__drop({type_name}* self) {{ gorget_channel_release(*self); }}").unwrap(),
        _ => {} // Unknown method — skip
    }
}

fn emit_shared_wrapper(out: &mut String, type_name: &str, method: &str, elem: &str) {
    match method {
        "new" => writeln!(out, "static inline {type_name} {type_name}__new({elem} val) {{ return gorget_shared_new(sizeof({elem}), &val); }}").unwrap(),
        "clone" => writeln!(out, "static inline {type_name} {type_name}__clone({type_name} self) {{ return gorget_shared_clone(self); }}").unwrap(),
        "drop" => writeln!(out, "static inline void {type_name}__drop({type_name}* self) {{ gorget_shared_drop(*self); }}").unwrap(),
        "get" => writeln!(out, "static inline {elem} {type_name}__get({type_name} self) {{ return *({elem}*)gorget_shared_get_ptr(self); }}").unwrap(),
        "strong_count" => writeln!(out, "static inline int64_t {type_name}__strong_count({type_name} self) {{ return gorget_shared_strong_count(self); }}").unwrap(),
        "downgrade" => {
            let weak_name = type_name.replacen("Shared__", "Weak__", 1);
            writeln!(out, "static inline {weak_name} {type_name}__downgrade({type_name} self) {{ return gorget_shared_downgrade(self); }}").unwrap();
        }
        // Shared__Vector__T extra methods: at, set_at, slen
        // For Shared[Vector[T]], the at/set_at operate on the vector's elements (type T),
        // NOT on Vector[T] itself. Use gorget_shared_array_* runtime functions.
        "at" => {
            // elem is the Vector's element type (e.g., Vector__int64_t = GorgetArray).
            // We need the *inner* element type for Shared__Vector patterns.
            let inner = shared_vector_inner_elem(type_name, elem);
            writeln!(out, "static inline {inner} {type_name}__at({type_name} self, int64_t idx) {{ return *({inner}*)gorget_shared_array_get(self, (size_t)idx); }}").unwrap();
        }
        "set_at" => {
            let inner = shared_vector_inner_elem(type_name, elem);
            writeln!(out, "static inline void {type_name}__set_at({type_name} self, int64_t idx, {inner} val) {{ gorget_shared_array_set(self, (size_t)idx, &val, sizeof({inner})); }}").unwrap();
        }
        "slen" => writeln!(out, "static inline int64_t {type_name}__slen({type_name} self) {{ return gorget_shared_array_len(self); }}").unwrap(),
        _ => {}
    }
}

fn emit_weak_wrapper(out: &mut String, type_name: &str, method: &str, _orig_to_c: &HashMap<String, String>) {
    let shared_name = type_name.replacen("Weak__", "Shared__", 1);
    match method {
        "clone" => writeln!(out, "static inline {type_name} {type_name}__clone({type_name} self) {{ return gorget_weak_clone(self); }}").unwrap(),
        "drop" => writeln!(out, "static inline void {type_name}__drop({type_name}* self) {{ gorget_weak_drop(*self); }}").unwrap(),
        "upgrade" => writeln!(out, "static inline {shared_name} {type_name}__upgrade({type_name} self) {{ return gorget_weak_upgrade(self) ? self : NULL; }}").unwrap(),
        _ => {}
    }
}

fn emit_mutex_wrapper(out: &mut String, type_name: &str, method: &str, elem: &str) {
    match method {
        "new" => writeln!(out, "static inline GorgetMutex* {type_name}__new({elem} val) {{ return gorget_mutex_new(sizeof({elem}), &val); }}").unwrap(),
        "lock" => writeln!(out, "static inline gorget_guard_t {type_name}__lock(GorgetMutex** self) {{ return gorget_mutex_lock(*self); }}").unwrap(),
        "try_lock" => writeln!(out, "static inline bool {type_name}__try_lock(GorgetMutex** self, gorget_guard_t* out) {{ return gorget_mutex_try_lock(*self, out); }}").unwrap(),
        "drop" => writeln!(out, "static inline void {type_name}__drop(GorgetMutex** self) {{ gorget_mutex_destroy(*self); }}").unwrap(),
        _ => {} // Unknown method
    }
}

fn emit_rwlock_wrapper(out: &mut String, type_name: &str, method: &str, elem: &str) {
    match method {
        "new" => writeln!(out, "static inline GorgetRWLock* {type_name}__new({elem} val) {{ return gorget_rwlock_new(sizeof({elem}), &val); }}").unwrap(),
        "read" => writeln!(out, "static inline gorget_read_guard_t {type_name}__read(GorgetRWLock** self) {{ return gorget_rwlock_read(*self); }}").unwrap(),
        "write" => writeln!(out, "static inline gorget_write_guard_t {type_name}__write(GorgetRWLock** self) {{ return gorget_rwlock_write(*self); }}").unwrap(),
        "drop" => writeln!(out, "static inline void {type_name}__drop(GorgetRWLock** self) {{ gorget_rwlock_destroy(*self); }}").unwrap(),
        _ => {}
    }
}

fn emit_box_wrapper(out: &mut String, type_name: &str, method: &str, elem: &str, module: &LirModule, orig_to_c: &HashMap<String, String>) {
    // Build struct_names map for type lookups from orig_to_c
    let sn: HashMap<u32, String> = module.structs.iter().enumerate()
        .map(|(i, def)| (i as u32, orig_to_c.get(&def.name).cloned().unwrap_or_else(|| format!("__lir_s{i}"))))
        .collect();
    match method {
        "get" => writeln!(out, "static inline {elem} {type_name}__get({type_name} self) {{ return *({elem}*)self; }}").unwrap(),
        "set" => writeln!(out, "static inline void {type_name}__set({type_name} self, {elem} val) {{ *({elem}*)self = val; }}").unwrap(),
        "drop" | "free" => writeln!(out, "static inline void {type_name}__drop({type_name} self) {{ GORGET_FREE(self, sizeof({elem})); }}").unwrap(),
        _ => {
            // Trait vtable dispatch wrapper: Box__Trait__method(self, ...) → self->vtable->method(self->data, ...)
            if is_trait_box(module, type_name) {
                let trait_name = type_name.strip_prefix("Box__").unwrap_or(elem);
                let ret_type = find_trait_method_return_type(module, trait_name, method, &sn);
                // Look up parameter list from extern declaration, but prefer impl function
                // param types when the extern has Ptr (which may actually be Str).
                let extern_name = format!("{type_name}__{method}");
                // Find an impl function: Trait_for_*__method
                let impl_params: Option<&[LirType]> = module.functions.iter()
                    .find(|f| {
                        let prefix = format!("{trait_name}_for_");
                        let suffix = format!("__{method}");
                        f.name.starts_with(&prefix) && f.name.ends_with(&suffix)
                    })
                    .map(|f| f.params.as_slice());
                let extra_params: Vec<String> = module.externs.iter()
                    .find(|e| e.name == extern_name)
                    .map(|e| {
                        // Skip first param (self) — remaining are forwarded
                        e.params.iter().skip(1).enumerate()
                            .map(|(i, t)| {
                                // If extern says Ptr but impl function says Struct(Str), use Str.
                                let effective_ty = if matches!(t, LirType::Ptr) {
                                    impl_params
                                        .and_then(|ps| ps.get(i + 1)) // +1 to skip self in impl too
                                        .filter(|it| matches!(it, LirType::Struct(_)))
                                        .unwrap_or(t)
                                } else {
                                    t
                                };
                                let ct = c_type_named(effective_ty, &sn);
                                format!("{ct} __p{}", i + 1)
                            })
                            .collect()
                    })
                    .unwrap_or_default();
                let param_decls = if extra_params.is_empty() {
                    String::new()
                } else {
                    format!(", {}", extra_params.join(", "))
                };
                let param_fwd = if extra_params.is_empty() {
                    String::new()
                } else {
                    let fwd: Vec<String> = (1..=extra_params.len()).map(|i| format!("__p{i}")).collect();
                    format!(", {}", fwd.join(", "))
                };
                if ret_type == "void" {
                    writeln!(out, "static inline void {type_name}__{method}(const {type_name}* self{param_decls}) {{ self->vtable->{method}(self->data{param_fwd}); }}").unwrap();
                } else {
                    writeln!(out, "static inline {ret_type} {type_name}__{method}(const {type_name}* self{param_decls}) {{ return self->vtable->{method}(self->data{param_fwd}); }}").unwrap();
                }
            }
        }
    }
}

fn parse_box_method(name: &str) -> Option<(String, &str)> {
    if !name.starts_with("Box__") { return None; }
    let rest = &name["Box__".len()..];
    let method_sep = rest.rfind("__")?;
    let type_part = &rest[..method_sep];
    let method = &rest[method_sep + 2..];
    Some((format!("Box__{type_part}"), method))
}

fn box_elem_type(type_name: &str) -> &str {
    type_name.strip_prefix("Box__").unwrap_or("int64_t")
}

/// Find the C return type of a trait method by looking at extern declarations first,
/// then falling back to trait impl functions.
fn find_trait_method_return_type(module: &LirModule, trait_name: &str, method: &str, sn: &HashMap<u32, String>) -> String {
    // First, check extern declarations for Box__Trait__method — these have correct return types
    // even when trait impl functions have been eliminated by DCE.
    let box_method_name = format!("Box__{trait_name}__{method}");
    for ext in &module.externs {
        if ext.name == box_method_name {
            return c_type_named(&ext.return_type, sn);
        }
    }
    // Fallback: search for trait impl functions
    let suffix = format!("__{method}");
    let prefix = format!("{trait_name}_for_");
    for func in &module.functions {
        if func.name.starts_with(&prefix) && func.name.ends_with(&suffix) {
            return c_type_named(&func.return_type, sn);
        }
    }
    // Fallback: void
    "void".to_string()
}

/// Find the C name of a struct by its original (pre-rename) name.
fn find_struct_cname_by_orig(module: &LirModule, orig_name: &str, sn: &HashMap<u32, String>) -> String {
    for (i, def) in module.structs.iter().enumerate() {
        if def.name == orig_name {
            return sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"));
        }
    }
    // Fallback
    orig_name.to_string()
}

/// Check if a Box type is a trait object box (has a corresponding _TraitObj struct).
fn is_trait_box(module: &LirModule, box_type: &str) -> bool {
    let trait_name = box_type.strip_prefix("Box__").unwrap_or(box_type);
    let traitobj_name = format!("{trait_name}_TraitObj");
    module.structs.iter().any(|d| d.name == traitobj_name)
}

/// Returns true if the function is provided by the Gorget C runtime (static inline).
fn is_runtime_fn(name: &str) -> bool {
    name.starts_with("gorget_")
        || name.starts_with("GORGET_")
        || name.starts_with("__gorget_")
}

/// Rewrite out-parameter calls for image/audio/deflate functions.
/// These C runtime functions use a void+out-param ABI but GIR treats them as single-return.
/// Returns Some(code) if the function was handled, None otherwise.
fn try_emit_outparam_call_lir(
    func_name: &str,
    dst: &Option<ValueId>,
    args: &[ValueId],
    val_types: &[Option<LirType>],
    str_lit_vals: &[bool],
    sn: &std::collections::HashMap<u32, String>,
    structs: &[StructDef],
) -> Option<String> {
    use std::fmt::Write;
    let v = |id: ValueId| format!("__v{}", id.0);
    let mut out = String::new();

    // Helper: get the C type name for the destination value's type.
    let dst_c_type = |d: &ValueId| -> String {
        val_types.get(d.0 as usize)
            .and_then(|t| t.as_ref())
            .map(|t| c_type_named(t, sn))
            .unwrap_or_else(|| "int64_t".to_string())
    };

    // Helper: coerce a string literal arg to Str, or pass a Str value as-is.
    // For Ptr args (e.g. pointer to Str slot), dereference to Str.
    let str_arg = |a: ValueId| -> String {
        let is_lit = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
        let ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
        if is_lit {
            format!("gorget_str_from_literal({v}, strlen({v}))", v = v(a))
        } else if matches!(ty, Some(LirType::Ptr)) {
            format!("*(Str*){}", v(a))
        } else {
            v(a)
        }
    };

    // Helper: get address of an array arg (pass by pointer).
    let array_addr = |a: ValueId| -> String {
        let ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
        if matches!(ty, Some(LirType::Ptr)) {
            v(a)
        } else {
            format!("&{}", v(a))
        }
    };

    // Helper: find the LIR C name for a struct by its original (GIR) name.
    let find_struct_c_name = |orig_name: &str| -> String {
        for (i, sdef) in structs.iter().enumerate() {
            if sdef.name == orig_name {
                return sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"));
            }
        }
        orig_name.to_string()
    };

    // Helper: find the Ok field name in a Result struct (LIR flattens to Ok_0, Error_0).
    // Also returns Error field name.
    let result_fields = |d: &ValueId| -> (String, String) {
        if let Some(LirType::Struct(sid)) = val_types.get(d.0 as usize).and_then(|t| t.as_ref()) {
            if let Some(sdef) = structs.get(sid.0 as usize) {
                let ok_f = sdef.fields.iter().find(|(n, _)| n.starts_with("Ok"))
                    .map(|(n, _)| c_field_name(n)).unwrap_or_else(|| "Ok_0".to_string());
                let err_f = sdef.fields.iter().find(|(n, _)| n.starts_with("Error"))
                    .map(|(n, _)| c_field_name(n)).unwrap_or_else(|| "Error_0".to_string());
                return (ok_f, err_f);
            }
        }
        ("Ok_0".to_string(), "Error_0".to_string())
    };

    match func_name {
        // gorget_image_load_rgba(Str path, int64_t* out_tag, int64_t* out_w, int64_t* out_h,
        //                       int64_t* out_ch, GorgetArray* out_data, Str* out_err)
        // Returns Result[Image, str]
        "gorget_image_load_rgba" | "image_load_rgba" => {
            let d = dst.as_ref()?;
            let c_type = dst_c_type(d);
            let path = str_arg(args[0]);
            let image_c = find_struct_c_name("Image");
            let (ok_f, err_f) = result_fields(d);
            let _ = write!(out,
                "{{ int64_t __tag = 0, __w = 0, __h = 0, __ch = 0; \
                GorgetArray __data = gorget_array_new(sizeof(uint8_t)); Str __err = {{0}}; \
                gorget_image_load_rgba({path}, &__tag, &__w, &__h, &__ch, &__data, &__err); \
                {c_type} __wr = {{0}}; __wr.tag = __tag; \
                if (__tag == 0) {{ __wr.{ok_f} = ({image_c}){{.width = __w, .height = __h, .channels = __ch, .data = __data}}; }} \
                else {{ __wr.{err_f} = __err; }} {v} = __wr; }}",
                v = v(*d));
            Some(out)
        }
        // gorget_image_load_rgba_from_memory(const GorgetArray* data, ...)
        "gorget_image_load_rgba_from_memory" => {
            let d = dst.as_ref()?;
            let c_type = dst_c_type(d);
            let data_ptr = array_addr(args[0]);
            let image_c = find_struct_c_name("Image");
            let (ok_f, err_f) = result_fields(d);
            let _ = write!(out,
                "{{ int64_t __tag = 0, __w = 0, __h = 0, __ch = 0; \
                GorgetArray __data = gorget_array_new(sizeof(uint8_t)); Str __err = {{0}}; \
                gorget_image_load_rgba_from_memory({data_ptr}, &__tag, &__w, &__h, &__ch, &__data, &__err); \
                {c_type} __wr = {{0}}; __wr.tag = __tag; \
                if (__tag == 0) {{ __wr.{ok_f} = ({image_c}){{.width = __w, .height = __h, .channels = __ch, .data = __data}}; }} \
                else {{ __wr.{err_f} = __err; }} {v} = __wr; }}",
                v = v(*d));
            Some(out)
        }
        // gorget_image_flip_vertically — extract Image fields, pass individually
        "gorget_image_flip_vertically" => {
            let d = dst.as_ref()?;
            let img = v(args[0]);
            let arg_ty = val_types.get(args[0].0 as usize).and_then(|t| t.as_ref());
            let image_c = find_struct_c_name("Image");
            // When the arg is a Ptr (void*), cast to Image* for field access.
            let (img_expr, acc) = if matches!(arg_ty, Some(LirType::Ptr)) {
                (format!("(({image_c}*){img})"), "->".to_string())
            } else {
                (img.clone(), ".".to_string())
            };
            let data_ref = format!("&{img_expr}{acc}data");
            let _ = write!(out,
                "{{ int64_t __w = 0, __h = 0, __ch = 0; \
                GorgetArray __data = gorget_array_new(sizeof(uint8_t)); \
                gorget_image_flip_vertically({img_expr}{acc}width, {img_expr}{acc}height, {img_expr}{acc}channels, {data_ref}, &__w, &__h, &__ch, &__data); \
                {v} = ({image_c}){{.width = __w, .height = __h, .channels = __ch, .data = __data}}; }}",
                v = v(*d));
            Some(out)
        }
        // gorget_audio_load_wav(Str path, ...)
        "gorget_audio_load_wav" => {
            let d = dst.as_ref()?;
            let c_type = dst_c_type(d);
            let path = str_arg(args[0]);
            let (ok_f, err_f) = result_fields(d);
            let _ = write!(out,
                "{{ int64_t __tag = 0; GorgetAudioChunk __chunk = {{0}}; Str __err = {{0}}; \
                gorget_audio_load_wav({path}, &__tag, &__chunk, &__err); \
                {c_type} __wr = {{0}}; __wr.tag = __tag; \
                if (__tag == 0) {{ __wr.{ok_f} = __chunk; }} \
                else {{ __wr.{err_f} = __err; }} {v} = __wr; }}",
                v = v(*d));
            Some(out)
        }
        // gorget_audio_load_wav_from_memory(const GorgetArray* data, ...)
        "gorget_audio_load_wav_from_memory" => {
            let d = dst.as_ref()?;
            let c_type = dst_c_type(d);
            let data_ptr = array_addr(args[0]);
            let (ok_f, err_f) = result_fields(d);
            let _ = write!(out,
                "{{ int64_t __tag = 0; GorgetAudioChunk __chunk = {{0}}; Str __err = {{0}}; \
                gorget_audio_load_wav_from_memory({data_ptr}, &__tag, &__chunk, &__err); \
                {c_type} __wr = {{0}}; __wr.tag = __tag; \
                if (__tag == 0) {{ __wr.{ok_f} = __chunk; }} \
                else {{ __wr.{err_f} = __err; }} {v} = __wr; }}",
                v = v(*d));
            Some(out)
        }
        // gorget_deflate_decompress(const GorgetArray* data, int64_t uncompressed_size, ...)
        "gorget_deflate_decompress" => {
            let d = dst.as_ref()?;
            let c_type = dst_c_type(d);
            let data_ptr = array_addr(args[0]);
            let size = v(args[1]);
            let (ok_f, err_f) = result_fields(d);
            let _ = write!(out,
                "{{ int64_t __tag = 0; GorgetArray __data = gorget_array_new(sizeof(uint8_t)); Str __err = {{0}}; \
                gorget_deflate_decompress({data_ptr}, {size}, &__tag, &__data, &__err); \
                {c_type} __wr = {{0}}; __wr.tag = __tag; \
                if (__tag == 0) {{ __wr.{ok_f} = __data; }} \
                else {{ __wr.{err_f} = __err; }} {v} = __wr; }}",
                v = v(*d));
            Some(out)
        }
        _ => None,
    }
}

/// Returns true if the extern's declared parameter at position `i` is a Str struct type.
/// Used to generically detect string literal args to any extern (GL, SDL, etc.) that
/// need wrapping with `gorget_str_from_literal()`.
fn ext_param_is_str(ext_params: Option<&[LirType]>, i: usize, structs: &[StructDef]) -> bool {
    ext_params.and_then(|p| p.get(i)).map_or(false, |ty| {
        matches!(ty, LirType::Struct(sid) if {
            structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetStringView" || s.name == "GorgetString")
        })
    })
}

/// Returns true if a runtime function takes `Str` at the given parameter position.
/// This covers GL/Metal/SDL runtime wrappers whose C definitions use `Str` by value,
/// but whose LIR extern declarations may have `Ptr` (inferred from string literal call sites).
fn runtime_fn_str_param(fn_name: &str, _param_idx: usize) -> bool {
    // GL runtime wrappers that take Str params
    matches!(fn_name,
        "gorget_gl_get_uniform_location"
        | "gorget_gl_get_attrib_location"
        | "gorget_gl_get_uniform_block_index"
        | "gorget_gl_bind_attrib_location"
        | "gorget_gl_shader_source"
        // Metal runtime wrappers that take Str params
        | "gorget_metal_create_library"
        | "gorget_metal_library_function"
        | "gorget_metal_cmd_buf_push_debug_group"
        | "gorget_metal_encoder_push_debug_group"
        | "gorget_metal_encoder_insert_debug_signpost"
    )
}

/// Returns true if the runtime function returns a raw `const char*` that needs wrapping
/// when stored to a Str/GorgetString slot. Other Ptr-returning functions return struct
/// pointers or void* that should be handled by the aggregate (memcpy) path instead.
fn is_cstr_returning_fn(name: &str) -> bool {
    matches!(
        name,
        "gorget_int_to_str"
            | "gorget_float_to_str"
            | "gorget_bool_to_str"
            | "gorget_char_to_str"
            | "gorget_bytes_to_hex"
            | "gorget_bytes_to_str"
            | "gorget_codepoint_to_utf8"
            | "gorget_format"
            | "gorget_format_time"
            | "gorget_getcwd"
            | "gorget_getenv"
            | "gorget_input"
            | "gorget_readline"
            | "gorget_memmem"
            | "gorget_path_absolute"
            | "gorget_path_basename"
            | "gorget_path_extension"
            | "gorget_path_join"
            | "gorget_path_normalize"
            | "gorget_path_parent"
            | "gorget_path_stem"
            | "gorget_platform"
            | "gorget_regex_match_text"
            | "gorget_regex_pattern_str"
            | "gorget_sdl_get_error"
            | "gorget_str_concat"
    )
}

/// For a Box__Trait__method call, return the argument positions (0-based, where 0 = self)
/// that should be coerced to Str. Returns empty vec for non-trait calls.
fn trait_box_str_arg_positions(module: &LirModule, name: &str) -> Vec<usize> {
    if !name.starts_with("Box__") { return vec![]; }
    let rest = &name["Box__".len()..];
    let sep = match rest.rfind("__") {
        Some(pos) => pos,
        None => return vec![],
    };
    let trait_name = &rest[..sep];
    let method = &rest[sep + 2..];
    // Check if there's a VTable for this trait
    if !module.structs.iter().any(|s| s.name == format!("{trait_name}_VTable")) {
        return vec![];
    }
    let str_sid = module.structs.iter().position(|s| s.name == "GorgetStringView");
    if str_sid.is_none() { return vec![]; }
    let str_sid = str_sid.unwrap();
    let prefix = format!("{trait_name}_for_");
    let suffix = format!("__{method}");
    // Find the impl function and check which params are GorgetStringView
    if let Some(f) = module.functions.iter().find(|f| f.name.starts_with(&prefix) && f.name.ends_with(&suffix)) {
        // The wrapper's arg 0 = self, impl's arg 0 = self.data (void*).
        // Wrapper args 1..N map to impl args 1..N.
        // Return positions in the *call* args (which include self at position 0).
        f.params.iter().enumerate()
            .filter(|(_, p)| matches!(p, LirType::Struct(sid) if sid.0 as usize == str_sid))
            .map(|(i, _)| i)  // i is the position in impl (0=self, 1=first arg, ...)
            .collect()
    } else {
        vec![]
    }
}

/// Functions that return a nullable `const char*` (NULL = None, non-NULL = some string).
/// These need to be wrapped into `Option<Str>` when the destination type is Option.
fn is_nullable_cstr_fn(name: &str) -> bool {
    matches!(
        name,
        "gorget_regex_match_group"
            | "gorget_regex_match_group_by_name"
            | "gorget_getenv"
    )
}

/// Functions that return a nullable pointer (NULL = None, non-NULL = Some(value)).
/// When the destination type is Option<T>, wrap into Option.
fn is_nullable_ptr_fn(name: &str) -> bool {
    // Weak__T__upgrade returns NULL when the shared value has been dropped.
    name.starts_with("Weak__") && name.ends_with("__upgrade")
}

/// Functions that return a sentinel value indicating "no result" — e.g., GorgetRegexMatch
/// with start==-1 means no match. These need wrapping into Option<T>.
fn is_sentinel_option_fn(name: &str) -> bool {
    matches!(
        name,
        "gorget_regex_find" | "gorget_regex_find_at" | "gorget_regex_find_pat"
            | "gorget_regex_fullmatch"
    )
}

/// Returns true if the collection runtime function returns `void*` (pointer to element).
/// The caller must dereference the result to the concrete element type.
fn is_collection_void_return(name: &str) -> bool {
    matches!(
        name,
        "gorget_array_get"
            | "gorget_array_safe_get"
            | "gorget_array_safe_pop"
            | "gorget_array_pop"
            | "gorget_array_first"
            | "gorget_array_last"
            | "gorget_array_remove_opt"
            | "gorget_map_get"
            | "gorget_heap_pop"
            | "gorget_heap_peek"
            // Concurrency: guard/shared/rwlock accessors return void*
            | "gorget_guard_get"
            | "gorget_guard_get_ptr"
            | "gorget_shared_get"
            | "gorget_shared_get_ptr"
            | "gorget_read_guard_get"
            | "gorget_read_guard_get_ptr"
            | "gorget_write_guard_get"
            | "gorget_write_guard_get_ptr"
            // Channel recv writes to an out-pointer (handled separately),
            // but channel_recv_timeout etc. may also return void*
            | "gorget_channel_recv"
    )
}

/// Functions that return void but need Option wrapping — included in is_collection_void_return
/// after being swapped to their opt variant.
fn needs_opt_wrapping(name: &str) -> bool {
    matches!(name, "gorget_array_remove")
}

/// For collection functions that are void but need to return a value when
/// the GIR expects Option[T], swap to the opt-returning variant.
fn void_to_opt_variant(name: &str) -> &str {
    match name {
        "gorget_array_remove" => "gorget_array_remove_opt",
        _ => name,
    }
}

/// Returns the indices of parameters that are `void*` (element/key/value pointers)
/// for collection runtime functions.  The caller must pass `&(Type){value}` for
/// these positions when the argument is a concrete value (not already a pointer).
fn collection_void_param_indices(name: &str) -> &'static [usize] {
    match name {
        "gorget_array_push" => &[1],
        "gorget_array_set" => &[2],
        "gorget_array_insert" => &[2],
        "gorget_array_contains" => &[1],
        "gorget_array_index_of" => &[1],
        "gorget_array_binary_search" => &[1],
        "gorget_array_extend" => &[1],
        "gorget_map_put" => &[1, 2],
        "gorget_map_get" | "gorget_map_contains" | "gorget_map_remove" => &[1],
        "gorget_set_add" | "gorget_set_contains" | "gorget_set_remove" => &[1],
        "gorget_heap_push" => &[1],
        // Concurrency: mutex_new(size, void*), shared_new(size, void*)
        "gorget_mutex_new" | "gorget_shared_new" | "gorget_rwlock_new" => &[1],
        // channel_send(ch, void*), guard_set(guard, void*, size)
        "gorget_channel_send" => &[1],
        "gorget_guard_set" | "gorget_write_guard_set" => &[1],
        _ => &[],
    }
}

/// Returns true if this collection runtime function takes its first arg
/// (the collection itself) by pointer.  Nearly all gorget_array_*, gorget_map_*,
/// gorget_set_* methods do, with the exception of constructors (_new).
/// Returns true if the extern name is an Option/Result unwrap helper.
fn is_option_result_unwrap(name: &str) -> bool {
    name == "__option_unwrap" || name == "__result_unwrap"
        || name == "__option_unwrap_or" || name == "__result_unwrap_or"
        || name == "gorget_option_unwrap"
        || (name.contains("Option__") && (name.ends_with("__unwrap") || name.ends_with("__unwrap_or")))
        || (name.contains("Result__") && (name.ends_with("__unwrap") || name.ends_with("__unwrap_or")))
}

/// Returns true if the extern name is an Option/Result expect helper.
fn is_option_result_expect(name: &str) -> bool {
    name == "__option_expect" || name == "__result_expect"
        || (name.contains("Option__") && name.ends_with("__expect"))
        || (name.contains("Result__") && name.ends_with("__expect"))
}

/// Returns true if the extern name is polymorphic — i.e. called with different
/// return types at different call sites. The extern declaration is unreliable
/// for type inference; use the SlotStore fix-up instead.
fn is_polymorphic_extern(name: &str) -> bool {
    is_option_result_unwrap(name) || is_option_result_expect(name)
        || is_option_result_combinator(name)
}

/// Returns true if the extern name is an Option/Result combinator helper.
fn is_option_result_combinator(name: &str) -> bool {
    (name.contains("Option__") || name.contains("Result__"))
        && (name.ends_with("__map") || name.ends_with("__filter")
            || name.ends_with("__and_then") || name.ends_with("__or_else")
            || name.ends_with("__unwrap_err") || name.ends_with("__unwrap_error") || name.ends_with("__map_err"))
}

/// String (GorgetString) methods that take self by pointer (GorgetString*).
/// These are `gorget_str_` prefixed but operate on the owned String type,
/// unlike Str view methods that take Str by value.
const GORGET_STRING_PTR_METHODS: &[&str] = &[
    "gorget_str_str",
    "gorget_str_push",
    "gorget_str_push_line",
    "gorget_str_clear",
    "gorget_str_capacity",
    "gorget_str_push_char",
    "gorget_string_free",
    "gorget_string_clone",
    "gorget_string_concat",
    "gorget_string_eq",
    "gorget_string_cstr",
    "gorget_string_append",
    "gorget_string_push_byte",
    "gorget_string_push_int",
    "gorget_string_push_float",
    "gorget_string_push_bool",
    "gorget_string_push_line",
];

fn collection_self_by_ptr(name: &str) -> bool {
    ((name.starts_with("gorget_array_") || name.starts_with("gorget_map_")
        || name.starts_with("gorget_set_") || name.starts_with("gorget_heap_")
        || name.starts_with("gorget_bytes_"))
        && !name.ends_with("_new")
        // gorget_bytes_from_str/from_hex take const char* (not GorgetArray*) as arg 0
        && name != "gorget_bytes_from_str" && name != "gorget_bytes_from_hex")
    // Dict/Set monomorphized inline methods also take self by pointer
    || parse_dict_higher_order(name).is_some()
    || parse_set_higher_order(name).is_some()
    // String (GorgetString) methods that take self by pointer
    || GORGET_STRING_PTR_METHODS.contains(&name)
}

/// Returns true if a gorget_str_* function has a non-Str parameter at index `i`.
/// Used to prevent Str wrapping for args that are actually GorgetArray, etc.
fn str_fn_non_str_arg(name: &str, i: usize) -> bool {
    // gorget_str_join(Str sep, GorgetArray parts) — arg 1 is GorgetArray
    if name == "gorget_str_join" && i == 1 { return true; }
    false
}

/// Check if a name suffix is a type name (indicating a constructor, not a method call).
fn is_collection_type_constructor(last_part: &str) -> bool {
    matches!(last_part, "int64_t" | "int32_t" | "int16_t" | "int8_t"
        | "uint64_t" | "uint32_t" | "uint16_t" | "uint8_t"
        | "double" | "float" | "bool" | "GorgetStringView" | "GorgetString"
        | "GorgetArray" | "GorgetMap" | "GorgetSet" | "void"
        | "T" | "U" | "V")
}

/// Emit a collection constructor call.
/// Vector__int64_t(cap) → gorget_array_with_capacity(sizeof(int64_t), cap)
/// Vector__int64_t() → gorget_array_new(sizeof(int64_t))
/// Return the C drop function name for a resource-type element, or None for trivial types.
fn elem_drop_fn_for_c_type(c_type: &str) -> Option<&'static str> {
    if c_type.starts_with("GorgetArray") || c_type.starts_with("Vector__") {
        Some("gorget_array_free")
    } else if c_type.starts_with("GorgetMap") || c_type.starts_with("Dict__") || c_type.starts_with("HashMap__") {
        Some("gorget_map_free")
    } else if c_type.starts_with("GorgetSet") || c_type.starts_with("Set__") || c_type.starts_with("HashSet__") {
        Some("gorget_set_free")
    } else if c_type == "GorgetString" {
        Some("gorget_string_free")
    } else {
        None
    }
}

fn elem_clone_fn_for_c_type(c_type: &str) -> Option<String> {
    if c_type.starts_with("GorgetArray") || c_type.starts_with("Vector__") {
        Some("gorget_array_clone_inplace".into())
    } else if c_type.starts_with("GorgetMap") || c_type.starts_with("Dict__") || c_type.starts_with("HashMap__") {
        Some("gorget_map_clone_inplace".into())
    } else if c_type.starts_with("GorgetSet") || c_type.starts_with("Set__") || c_type.starts_with("HashSet__") {
        Some("gorget_set_clone_inplace".into())
    } else if c_type == "GorgetString" || c_type == "GorgetStringView" {
        Some("gorget_string_clone_inplace".into())
    } else {
        // User struct/enum clone functions are set via dv.elem_clone = ...
        // at the specific call sites where we know the type has a __clone function.
        // Don't guess here — return None for unknown types.
        None
    }
}

fn emit_collection_constructor(
    out: &mut String,
    name: &str,
    dst: &Option<ValueId>,
    args: &[ValueId],
    _val_types: &[Option<LirType>],
    _sn: &HashMap<u32, String>,
) {
    use std::fmt::Write;
    let v = |vid: ValueId| format!("__v{}", vid.0);

    if let Some(d) = dst {
        write!(out, "{} = ", v(*d)).unwrap();
    }

    if name.starts_with("Vector__") || name.starts_with("GorgetArray__") {
        let elem_type = name.strip_prefix("Vector__")
            .or_else(|| name.strip_prefix("GorgetArray__"))
            .unwrap_or("int64_t");
        if args.is_empty() {
            write!(out, "gorget_array_new(sizeof({elem_type}));").unwrap();
        } else {
            write!(out, "gorget_array_with_capacity(sizeof({elem_type}), {});", v(args[0])).unwrap();
        }
        // Set elem_drop for resource-type elements
        if let Some(drop_fn) = elem_drop_fn_for_c_type(elem_type) {
            if let Some(d) = dst {
                write!(out, " {}.elem_drop = (__gorget_drop_fn){drop_fn};", v(*d)).unwrap();
            }
        }
    } else if name.starts_with("Set__") || name.starts_with("HashSet__") {
        let elem_type = name.strip_prefix("Set__")
            .or_else(|| name.strip_prefix("HashSet__"))
            .unwrap_or("int64_t");
        if args.is_empty() {
            write!(out, "gorget_set_new(sizeof({elem_type}));").unwrap();
        } else {
            write!(out, "gorget_set_with_capacity(sizeof({elem_type}), {});", v(args[0])).unwrap();
        }
    } else if name.starts_with("Dict__") || name.starts_with("HashMap__") {
        // Dict__K__V or HashMap__K__V — extract key/value types
        let prefix = if name.starts_with("Dict__") { "Dict__" } else { "HashMap__" };
        let rest = name.strip_prefix(prefix).unwrap_or("int64_t__int64_t");
        // For Dict__int64_t__Str → key=int64_t, val=Str
        // Simple heuristic: first __ part is key, rest is val
        let parts: Vec<&str> = rest.splitn(2, "__").collect();
        let (key_type, val_type) = if parts.len() == 2 {
            (parts[0], parts[1])
        } else {
            ("int64_t", "int64_t")
        };
        let fn_name = if name.starts_with("Dict__") { "gorget_dict_new" } else { "gorget_map_new" };
        write!(out, "{fn_name}(sizeof({key_type}), sizeof({val_type}));").unwrap();
        // Set val_drop for resource-type values
        if let Some(drop_fn) = elem_drop_fn_for_c_type(val_type) {
            if let Some(d) = dst {
                write!(out, " {}.val_drop = (__gorget_drop_fn){drop_fn};", v(*d)).unwrap();
            }
        }
    } else {
        // Fallback — shouldn't happen
        write!(out, "/* unknown constructor: {name} */ {{0}};").unwrap();
    }
}

/// Returns true if the runtime function expects arg at `idx` to be passed by pointer
/// (i.e. the C prototype uses `const GorgetArray*`, `const GorgetX25519KeyPair*`, etc.)
/// but LIR passes it as a Struct value. The codegen must emit `&(val)` or pass the pointer directly.
fn runtime_arg_by_ptr(name: &str, idx: usize) -> bool {
    // Self-by-pointer for collection/string methods (arg 0)
    if idx == 0 && collection_self_by_ptr(name) {
        return true;
    }
    // gorget_crypto_* functions take most struct args (GorgetArray, KeyPair) by pointer
    if name.starts_with("gorget_crypto_") {
        // All args except simple scalars are by-pointer for crypto functions
        return true;
    }
    // gorget_cipher_encrypt/decrypt(ctx*, const GorgetArray* data)
    if name == "gorget_cipher_encrypt" || name == "gorget_cipher_decrypt" { return true; }
    // gorget_bytes_concat(const GorgetArray* a, const GorgetArray* b) — all args
    if name == "gorget_bytes_concat" { return true; }
    // gorget_udp_* functions take GorgetUdpSocket* and often GorgetArray* args
    if name.starts_with("gorget_udp_") && !name.ends_with("_new") && !name.ends_with("_bind") {
        return true;
    }
    false
}

/// Maps a runtime function name to its thread-local error check function.
/// Functions in this list return a raw scalar value in C, but the GIR expects
/// them to return a Result struct. The backend must wrap the call with an
/// error check to construct the Result.
fn last_error_fn(name: &str) -> Option<&'static str> {
    if name.starts_with("gorget_udp_") {
        return Some("gorget_udp_last_error");
    }
    if name.starts_with("gorget_server_socket_") {
        return Some("gorget_server_socket_last_error");
    }
    if name.starts_with("gorget_socket_") {
        return Some("gorget_socket_last_error");
    }
    // TlsServer before Tls to avoid prefix collision
    if name.starts_with("gorget_tls_server_") {
        return Some("gorget_tls_server_last_error");
    }
    if name.starts_with("gorget_tls_") {
        return Some("gorget_tls_last_error");
    }
    if name.starts_with("gorget_regex_") {
        return Some("gorget_regex_last_error");
    }
    if name.starts_with("gorget_crypto_") {
        return Some("gorget_crypto_last_error");
    }
    if name == "gorget_process_spawn" {
        return Some("gorget_process_spawn_err");
    }
    if name == "gorget_parse_int" || name == "gorget_parse_float" {
        return Some("gorget_parse_last_error");
    }
    None
}

/// Returns true if the runtime function returns a raw `const char*` result
/// that should be wrapped with `gorget_str_from_cstr` when the Result Ok payload is Str.
fn last_error_returns_cstr(name: &str) -> bool {
    name.starts_with("gorget_socket_") || name.starts_with("gorget_tls_")
        || name.starts_with("gorget_udp_") || name.starts_with("gorget_server_socket_")
}

/// Returns true if the type is a Str or GorgetString struct.
fn is_str_struct(ty: &LirType, module: &LirModule) -> bool {
    if let LirType::Struct(sid) = ty {
        let name = &module.structs[sid.0 as usize].name;
        name == "GorgetStringView" || name == "GorgetString"
    } else {
        false
    }
}

fn is_std_header_fn(name: &str) -> bool {
    matches!(
        name,
        "printf" | "fprintf" | "sprintf" | "snprintf" | "puts" | "putchar" | "getchar"
            | "fopen" | "fclose" | "fread" | "fwrite" | "fgets" | "fputs" | "fflush"
            | "fseek" | "ftell" | "rewind" | "feof" | "ferror"
            | "malloc" | "calloc" | "realloc" | "free" | "exit" | "abort" | "atexit"
            | "atoi" | "atol" | "atof" | "strtol" | "strtod"
            | "memcpy" | "memmove" | "memset" | "memcmp"
            | "strlen" | "strcpy" | "strncpy" | "strcat" | "strncat" | "strcmp" | "strncmp"
            | "strstr" | "strchr" | "strrchr"
            | "abs" | "labs" | "llabs"
            | "getenv" | "setenv" | "unsetenv"
            | "getcwd" | "chdir" | "getpid"
            | "time" | "localtime" | "gmtime" | "strftime" | "mktime" | "difftime"
            | "clock_gettime" | "nanosleep"
            | "rand" | "srand"
            | "qsort" | "bsearch"
            // Gorget wrappers that collide with POSIX names — skip extern decls
            // because the actual calls are rewritten to runtime functions.
            | "sleep" | "xtd_sleep"
            | "mkdir" | "rename" | "remove" | "readdir"
            | "usleep"
    )
}

/// Choose the right qsort comparator based on element C type.
fn compare_fn_for_elem(elem_c: &str) -> &'static str {
    match elem_c {
        "double" | "float" => "gorget_float_compare",
        "Str" => "gorget_str_compare",
        "int64_t" => "gorget_int_compare",
        _ => "gorget_generic_compare",
    }
}

/// Map a `__gorget_box_alloc_<suffix>` suffix to the correct C type.
/// Some types (like GorgetStringView) are represented as LirType::Ptr in LIR
/// but need their real C struct type for proper sizeof/copy semantics.
fn box_alloc_suffix_to_c_type(suffix: &str) -> String {
    match suffix {
        "Str" | "GorgetStringView" => "Str".into(),
        "int64_t" => "int64_t".into(),
        "int32_t" => "int32_t".into(),
        "int16_t" => "int16_t".into(),
        "int8_t" => "int8_t".into(),
        "uint64_t" => "uint64_t".into(),
        "uint32_t" => "uint32_t".into(),
        "uint16_t" => "uint16_t".into(),
        "uint8_t" => "uint8_t".into(),
        "double" => "double".into(),
        "float" => "float".into(),
        "bool" => "bool".into(),
        _ => suffix.to_string(), // struct types use their name directly
    }
}

/// Like `box_alloc_suffix_to_c_type` but with fallback to the LIR param type.
fn box_alloc_inner_c_type(suffix: &str, lir_ty: &LirType, struct_names: &HashMap<u32, String>) -> String {
    // For GorgetStringView: LIR type is Ptr (void*) but real C type is Str
    if suffix == "Str" || suffix == "GorgetStringView" {
        return "Str".into();
    }
    // For struct types, use the suffix (which is the monomorphized struct name)
    if let LirType::Struct(_) = lir_ty {
        return c_type_named(lir_ty, struct_names);
    }
    // For primitives, the LIR type is accurate
    c_type_named(lir_ty, struct_names)
}

fn c_type_named(ty: &LirType, struct_names: &HashMap<u32, String>) -> String {
    match ty {
        LirType::I8 => "int8_t".into(),
        LirType::I16 => "int16_t".into(),
        LirType::I32 => "int32_t".into(),
        LirType::I64 => "int64_t".into(),
        LirType::U8 => "uint8_t".into(),
        LirType::U16 => "uint16_t".into(),
        LirType::U32 => "uint32_t".into(),
        LirType::U64 => "uint64_t".into(),
        LirType::F32 => "float".into(),
        LirType::F64 => "double".into(),
        LirType::Bool => "bool".into(),
        LirType::Ptr => "void*".into(),
        LirType::Struct(id) => struct_names
            .get(&id.0)
            .cloned()
            .unwrap_or_else(|| format!("__lir_s{}", id.0)),
        LirType::Void => "void".into(),
    }
}

/// Emit a test runner `main()` that calls each test function and reports results.
/// Mirrors `emit_test_runner_main` in the old C backend (`src/backend/c/mod.rs`).
fn emit_test_runner_main(out: &mut String, module: &LirModule) {
    let test_fns = &module.test_fns;
    let has_any_timeout = test_fns.iter().any(|t| t.timeout_ms.is_some());

    writeln!(out, "int main(int argc, char** argv) {{").unwrap();
    writeln!(out, "    gorget_init_args(argc, argv);").unwrap();
    if let Some(ref trace_path) = module.trace_filename {
        let escaped = trace_path.replace('\\', "\\\\").replace('"', "\\\"");
        writeln!(out, "    __gorget_trace_init(\"{escaped}\");").unwrap();
    }
    writeln!(out, "    int __test_passed = 0, __test_failed = 0, __test_skipped = 0;").unwrap();
    writeln!(out, "    struct timespec __total_start, __total_end;").unwrap();
    writeln!(out, "    clock_gettime(CLOCK_MONOTONIC, &__total_start);").unwrap();

    // Parallel support
    writeln!(out, "    int __par_id = -1, __par_total = 0;").unwrap();
    writeln!(out, "    const char* __par_id_env = getenv(\"GORGET_PARALLEL_ID\");").unwrap();
    writeln!(out, "    const char* __par_total_env = getenv(\"GORGET_PARALLEL_TOTAL\");").unwrap();
    writeln!(out, "    if (__par_id_env && __par_total_env) {{ __par_id = atoi(__par_id_env); __par_total = atoi(__par_total_env); }}").unwrap();

    // Result file support
    writeln!(out, "    const char* __results_path = getenv(\"GORGET_TEST_RESULTS\");").unwrap();
    writeln!(out, "    __gorget_snapshot_open();").unwrap();

    writeln!(out, "    int __test_total = {};", test_fns.len()).unwrap();
    writeln!(out, "    if (__par_total > 0) {{").unwrap();
    writeln!(out, "        __test_total = 0;").unwrap();
    writeln!(out, "        for (int __i = 0; __i < {}; __i++) if (__i % __par_total == __par_id) __test_total++;", test_fns.len()).unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "    printf(\"Running %d tests...\\n\", __test_total);").unwrap();

    if module.has_suite_setup {
        writeln!(out, "    __suite_setup();").unwrap();
    }

    writeln!(out, "    int __results[{}];", test_fns.len()).unwrap();
    writeln!(out, "    memset(__results, 0, sizeof(__results));").unwrap();

    for (idx, info) in test_fns.iter().enumerate() {
        let escaped = info.display_name.replace('\\', "\\\\").replace('"', "\\\"");
        let fn_name = c_func_name(&info.fn_name);

        writeln!(out, "    if (__par_total > 0 && ({idx} % __par_total != __par_id)) goto __test_done_{idx};").unwrap();

        if info.skipped {
            writeln!(out, "    printf(\"  test: {escaped} ... \");").unwrap();
            if let Some(ref reason) = info.skip_reason {
                let escaped_reason = reason.replace('\\', "\\\\").replace('"', "\\\"");
                writeln!(out, "    printf(\"SKIP ({escaped_reason})\\n\");").unwrap();
            } else {
                writeln!(out, "    printf(\"SKIP\\n\");").unwrap();
            }
            writeln!(out, "    __test_skipped++;").unwrap();
            writeln!(out, "    goto __test_done_{idx};").unwrap();
        }

        if !info.skipped {
            // Trace: test_start event
            if module.trace_filename.is_some() {
                writeln!(out, "    if (__gorget_trace_fp) fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"test_start\\\",\\\"name\\\":\\\"{escaped}\\\"}}\\n\");").unwrap();
            }
            writeln!(out, "    printf(\"  test: {escaped} ... \");").unwrap();
            writeln!(out, "    fflush(stdout);").unwrap();
            writeln!(out, "    {{").unwrap();
            writeln!(out, "        __gorget_in_test = 1;").unwrap();
            writeln!(out, "        __gorget_test_fail_msg = NULL;").unwrap();
            writeln!(out, "        __gorget_test_timed_out = 0;").unwrap();
            writeln!(out, "        __gorget_current_test = \"{escaped}\";").unwrap();
            writeln!(out, "        int __cleanup_mark = __gorget_cleanup_top;").unwrap();
            writeln!(out, "        struct timespec __t_start, __t_end;").unwrap();
            writeln!(out, "        clock_gettime(CLOCK_MONOTONIC, &__t_start);").unwrap();
            writeln!(out, "        __gorget_test_cleanup_mark = __cleanup_mark;").unwrap();

            if let Some(ms) = info.timeout_ms {
                writeln!(out, "        __gorget_set_timeout({ms}L);").unwrap();
            }

            writeln!(out, "        int __jmp_val = setjmp(__gorget_test_jmp);").unwrap();
            writeln!(out, "        if (__jmp_val == 0) {{").unwrap();
            writeln!(out, "            {fn_name}();").unwrap();
            writeln!(out, "            __gorget_cleanup_top = __cleanup_mark;").unwrap();
            writeln!(out, "        }}").unwrap();

            if info.timeout_ms.is_some() {
                writeln!(out, "        __gorget_cancel_timeout();").unwrap();
            }

            // On timeout (jmp_val==2): cleanup was NOT run by signal handler, run it now
            // On panic (jmp_val==1): gorget_panic already ran cleanup, this is a no-op
            writeln!(out, "        __gorget_cleanup_run(__cleanup_mark);").unwrap();
            writeln!(out, "        __gorget_in_test = 0;").unwrap();

            writeln!(out, "        clock_gettime(CLOCK_MONOTONIC, &__t_end);").unwrap();
            writeln!(out, "        long __t_ms = (__t_end.tv_sec - __t_start.tv_sec) * 1000 + (__t_end.tv_nsec - __t_start.tv_nsec) / 1000000;").unwrap();

            // Timeout always fails
            if has_any_timeout {
                writeln!(out, "        if (__gorget_test_timed_out) {{").unwrap();
                if let Some(ms) = info.timeout_ms {
                    writeln!(out, "            __test_failed++; __results[{idx}] = 2;").unwrap();
                    writeln!(out, "            printf(\"FAIL: timed out after {ms}ms (%ldms)\\n\", __t_ms);").unwrap();
                } else {
                    writeln!(out, "            __test_failed++; __results[{idx}] = 2;").unwrap();
                    writeln!(out, "            printf(\"FAIL: timed out (%ldms)\\n\", __t_ms);").unwrap();
                }
                writeln!(out, "        }} else").unwrap();
            }

            if info.should_panic {
                if let Some(ref msg) = info.expected_panic_msg {
                    let emsg = msg.replace('\\', "\\\\").replace('"', "\\\"");
                    writeln!(out, "        if (__gorget_test_fail_msg && strstr(__gorget_test_fail_msg, \"{emsg}\")) {{").unwrap();
                    writeln!(out, "            __test_passed++; __results[{idx}] = 1;").unwrap();
                    writeln!(out, "            printf(\"PASS (%ldms)\\n\", __t_ms);").unwrap();
                    writeln!(out, "        }} else if (__gorget_test_fail_msg) {{").unwrap();
                    writeln!(out, "            __test_failed++; __results[{idx}] = 2;").unwrap();
                    writeln!(out, "            printf(\"FAIL: expected panic containing \\\"{emsg}\\\", got: %s (%ldms)\\n\", __gorget_test_fail_msg, __t_ms);").unwrap();
                    writeln!(out, "        }} else {{").unwrap();
                    writeln!(out, "            __test_failed++; __results[{idx}] = 2;").unwrap();
                    writeln!(out, "            printf(\"FAIL: expected panic but test passed (%ldms)\\n\", __t_ms);").unwrap();
                    writeln!(out, "        }}").unwrap();
                } else {
                    writeln!(out, "        if (__gorget_test_fail_msg) {{").unwrap();
                    writeln!(out, "            __test_passed++; __results[{idx}] = 1;").unwrap();
                    writeln!(out, "            printf(\"PASS (%ldms)\\n\", __t_ms);").unwrap();
                    writeln!(out, "        }} else {{").unwrap();
                    writeln!(out, "            __test_failed++; __results[{idx}] = 2;").unwrap();
                    writeln!(out, "            printf(\"FAIL: expected panic but test passed (%ldms)\\n\", __t_ms);").unwrap();
                    writeln!(out, "        }}").unwrap();
                }
            } else {
                writeln!(out, "        if (!__gorget_test_fail_msg) {{").unwrap();
                writeln!(out, "            __test_passed++; __results[{idx}] = 1;").unwrap();
                writeln!(out, "            printf(\"PASS (%ldms)\\n\", __t_ms);").unwrap();
                writeln!(out, "        }} else {{").unwrap();
                writeln!(out, "            __test_failed++; __results[{idx}] = 2;").unwrap();
                writeln!(out, "            printf(\"FAIL: %s (%ldms)\\n\", __gorget_test_fail_msg, __t_ms);").unwrap();
                writeln!(out, "        }}").unwrap();
            }

            // Trace: test_end event with status and duration
            if module.trace_filename.is_some() {
                writeln!(out, "        if (__gorget_trace_fp) fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"test_end\\\",\\\"name\\\":\\\"{escaped}\\\",\\\"status\\\":\\\"%s\\\",\\\"duration_ms\\\":%ld}}\\n\", __results[{idx}] == 1 ? \"pass\" : __results[{idx}] == 2 ? \"fail\" : \"skip\", __t_ms);").unwrap();
            }
            writeln!(out, "    }}").unwrap();
        }

        writeln!(out, "    __test_done_{idx}:;").unwrap();
    }

    if module.has_suite_teardown {
        writeln!(out, "    __suite_teardown();").unwrap();
    }

    // Summary
    writeln!(out, "    clock_gettime(CLOCK_MONOTONIC, &__total_end);").unwrap();
    writeln!(out, "    long __total_ms = (__total_end.tv_sec - __total_start.tv_sec) * 1000 + (__total_end.tv_nsec - __total_start.tv_nsec) / 1000000;").unwrap();
    writeln!(out, "    if (__test_skipped > 0) printf(\"\\n%d passed, %d failed, %d skipped (%ldms)\\n\", __test_passed, __test_failed, __test_skipped, __total_ms);").unwrap();
    writeln!(out, "    else printf(\"\\n%d passed, %d failed (%ldms)\\n\", __test_passed, __test_failed, __total_ms);").unwrap();

    // Write results file
    writeln!(out, "    if (__results_path) {{").unwrap();
    writeln!(out, "        FILE* __rf = fopen(__results_path, \"w\");").unwrap();
    writeln!(out, "        if (__rf) {{").unwrap();
    writeln!(out, "            fprintf(__rf, \"{{\\\"results\\\":[\\n\");").unwrap();
    for (idx, info) in test_fns.iter().enumerate() {
        let escaped = info.display_name.replace('\\', "\\\\").replace('"', "\\\"");
        let comma = if idx + 1 < test_fns.len() { "," } else { "" };
        writeln!(out, "            fprintf(__rf, \"  {{\\\"name\\\":\\\"{escaped}\\\",\\\"status\\\":\\\"%s\\\"}}{comma}\\n\", __results[{idx}] == 1 ? \"pass\" : __results[{idx}] == 2 ? \"fail\" : \"skip\");").unwrap();
    }
    writeln!(out, "            fprintf(__rf, \"]}}\\n\");").unwrap();
    writeln!(out, "            fclose(__rf);").unwrap();
    writeln!(out, "        }}").unwrap();
    writeln!(out, "    }}").unwrap();

    writeln!(out, "    __gorget_snapshot_close();").unwrap();
    writeln!(out, "    return __test_failed > 0 ? 1 : 0;").unwrap();
    writeln!(out, "}}").unwrap();
}

/// Emit a bench runner `main()` that calls each benchmark function.
/// Mirrors the interpreter bench runner: warmup, auto-calibrate, timing.
fn emit_bench_runner_main(out: &mut String, module: &LirModule) {
    writeln!(out, "int main(int argc, char** argv) {{").unwrap();
    writeln!(out, "    gorget_init_args(argc, argv);").unwrap();
    writeln!(out, "    int __bench_count = {};", module.bench_fns.len()).unwrap();
    writeln!(out, "    printf(\"Running %d benchmarks...\\n\\n\", __bench_count);").unwrap();

    // Suite setup if present
    if module.has_suite_setup {
        writeln!(out, "    __suite_setup();").unwrap();
    }

    for info in &module.bench_fns {
        let escaped = info.display_name.replace('\\', "\\\\").replace('"', "\\\"");
        let fn_name = c_func_name(&info.fn_name);

        writeln!(out, "    {{").unwrap();
        // Warmup: 3 iterations
        writeln!(out, "        for (int __w = 0; __w < 3; __w++) {fn_name}();").unwrap();

        // Auto-calibrate: start at 100 iterations, double until >= 1 second
        writeln!(out, "        uint64_t __iters = 100;").unwrap();
        writeln!(out, "        uint64_t __total_ns = 0;").unwrap();
        writeln!(out, "        for (;;) {{").unwrap();
        writeln!(out, "            struct timespec __bs, __be;").unwrap();
        writeln!(out, "            clock_gettime(CLOCK_MONOTONIC, &__bs);").unwrap();
        writeln!(out, "            for (uint64_t __i = 0; __i < __iters; __i++) {fn_name}();").unwrap();
        writeln!(out, "            clock_gettime(CLOCK_MONOTONIC, &__be);").unwrap();
        writeln!(out, "            __total_ns = (uint64_t)(__be.tv_sec - __bs.tv_sec) * 1000000000ULL").unwrap();
        writeln!(out, "                       + (uint64_t)(__be.tv_nsec - __bs.tv_nsec);").unwrap();
        writeln!(out, "            if (__total_ns >= 1000000000ULL) break;").unwrap();
        writeln!(out, "            if (__total_ns < 10000000ULL) __iters *= 100;").unwrap();
        writeln!(out, "            else __iters *= 2;").unwrap();
        writeln!(out, "        }}").unwrap();

        // Format and print result
        writeln!(out, "        double __avg_ns = (double)__total_ns / (double)__iters;").unwrap();
        writeln!(out, r#"        if (__avg_ns < 1000.0) printf("  bench: {escaped} ... %llu iters, %.0f ns/iter\n", (unsigned long long)__iters, __avg_ns);"#).unwrap();
        writeln!(out, r#"        else if (__avg_ns < 1000000.0) printf("  bench: {escaped} ... %llu iters, %.2f us/iter\n", (unsigned long long)__iters, __avg_ns / 1000.0);"#).unwrap();
        writeln!(out, r#"        else if (__avg_ns < 1000000000.0) printf("  bench: {escaped} ... %llu iters, %.2f ms/iter\n", (unsigned long long)__iters, __avg_ns / 1000000.0);"#).unwrap();
        writeln!(out, r#"        else printf("  bench: {escaped} ... %llu iters, %.2f s/iter\n", (unsigned long long)__iters, __avg_ns / 1000000000.0);"#).unwrap();
        writeln!(out, "    }}").unwrap();
    }

    // Suite teardown if present
    if module.has_suite_teardown {
        writeln!(out, "    __suite_teardown();").unwrap();
    }

    writeln!(out, "    printf(\"\\n%d benchmarks complete.\\n\", __bench_count);").unwrap();
    writeln!(out, "    return 0;").unwrap();
    writeln!(out, "}}").unwrap();
}

/// Returns true if a struct IS a direct resource type (GorgetArray, GorgetMap, etc.).
/// Used for post-push zeroing where only the direct resource needs to be zeroed.
fn is_direct_resource_type(sid: crate::lir::StructId, module: &crate::lir::LirModule) -> bool {
    if let Some(sdef) = module.structs.get(sid.0 as usize) {
        return matches!(sdef.name.as_str(),
            "GorgetArray" | "GorgetMap" | "GorgetSet" | "GorgetString" | "GorgetClosure"
        );
    }
    false
}

/// Returns true if a struct (by StructId) directly is a resource type (GorgetArray, etc.)
/// or transitively contains resource-type fields that would be double-freed on shallow copy.
#[allow(dead_code)]
fn struct_contains_resources(sid: crate::lir::StructId, module: &crate::lir::LirModule) -> bool {
    if let Some(sdef) = module.structs.get(sid.0 as usize) {
        if matches!(sdef.name.as_str(),
            "GorgetArray" | "GorgetMap" | "GorgetSet" | "GorgetString" | "GorgetClosure"
        ) {
            return true;
        }
        // Check fields recursively (one level deep is sufficient for all current types).
        for (_, fty) in &sdef.fields {
            if let LirType::Struct(fsid) = fty {
                if let Some(fdef) = module.structs.get(fsid.0 as usize) {
                    if matches!(fdef.name.as_str(),
                        "GorgetArray" | "GorgetMap" | "GorgetSet" | "GorgetString" | "GorgetClosure"
                    ) {
                        return true;
                    }
                    // Two levels deep for enums containing structs containing arrays.
                    for (_, ffty) in &fdef.fields {
                        if let LirType::Struct(ffsid) = ffty {
                            if let Some(ffdef) = module.structs.get(ffsid.0 as usize) {
                                if matches!(ffdef.name.as_str(),
                                    "GorgetArray" | "GorgetMap" | "GorgetSet" | "GorgetString" | "GorgetClosure"
                                ) {
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    false
}

/// Generate deep-clone operations for resource-type fields within a struct.
#[allow(dead_code)]
/// Returns `Some(Vec<String>)` if the struct contains resource fields that need
/// individual cloning to prevent double-free on shallow copy. Each string is a
/// C statement like `{dst}.field = gorget_array_clone(&{dst}.field);`.
///
/// `dst_expr` is the C expression for the destination (e.g., `__v83.Some_0` or `__s5`).
fn deep_clone_resource_fields(
    sid: crate::lir::StructId,
    dst_expr: &str,
    module: &crate::lir::LirModule,
) -> Option<Vec<String>> {
    let sdef = module.structs.get(sid.0 as usize)?;
    // Skip direct resource types — they use gorget_array_clone etc. directly
    if matches!(sdef.name.as_str(),
        "GorgetArray" | "GorgetMap" | "GorgetSet" | "GorgetString" | "GorgetClosure"
    ) {
        return None;
    }
    // Skip enums — variants are stored in a union, can't clone all fields at once.
    // Enum element deep-clone requires match-on-tag which is handled separately.
    if sdef.is_enum {
        return None;
    }
    let mut ops = Vec::new();
    for (fname, fty) in &sdef.fields {
        if let LirType::Struct(fsid) = fty {
            if let Some(fdef) = module.structs.get(fsid.0 as usize) {
                // Skip enum fields — variant payloads can't all be cloned at once
                if fdef.is_enum { continue; }
                let clone_fn = match fdef.name.as_str() {
                    "GorgetArray" => Some("gorget_array_clone"),
                    "GorgetMap" => Some("gorget_map_clone"),
                    "GorgetSet" => Some("gorget_set_clone"),
                    "GorgetString" => Some("gorget_string_clone"),
                    _ => None,
                };
                if let Some(cfn) = clone_fn {
                    ops.push(format!("{dst_expr}.{fname} = {cfn}(&{dst_expr}.{fname});"));
                } else {
                    // Recurse one level: check if this nested struct has resource fields
                    for (ffname, ffty) in &fdef.fields {
                        if let LirType::Struct(ffsid) = ffty {
                            if let Some(ffdef) = module.structs.get(ffsid.0 as usize) {
                                if ffdef.is_enum { continue; }
                                let inner_clone = match ffdef.name.as_str() {
                                    "GorgetArray" => Some("gorget_array_clone"),
                                    "GorgetMap" => Some("gorget_map_clone"),
                                    "GorgetSet" => Some("gorget_set_clone"),
                                    "GorgetString" => Some("gorget_string_clone"),
                                    _ => None,
                                };
                                if let Some(icfn) = inner_clone {
                                    ops.push(format!("{dst_expr}.{fname}.{ffname} = {icfn}(&{dst_expr}.{fname}.{ffname});"));
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    if ops.is_empty() { None } else { Some(ops) }
}

/// Returns true if the given extern function takes a `const char*` (raw C string)
/// at the given parameter position, even though the LIR extern declaration says `Str`.
/// These are C runtime functions where the signature uses `const char*` but the GIR
/// passes Gorget's `str` type.
fn takes_cstr_for_str_param(fn_name: &str, param_idx: usize) -> bool {
    match fn_name {
        "gorget_socket_write_str" | "gorget_tls_write_str"
        | "gorget_socket_async_write_str" | "gorget_tls_async_write_str"
        | "gorget_udp_send_str" => param_idx == 1,
        // parse functions: single arg is const char* (at index 0)
        "gorget_parse_int" | "gorget_parse_float" => param_idx == 0,
        "gorget_setenv" | "gorget_file_open" | "gorget_path_join" => param_idx == 0 || param_idx == 1,
        // Functions that take const char* as first arg
        "gorget_udp_bind" | "gorget_udp_send_to"
        | "gorget_socket_connect" | "gorget_server_socket_bind"
        | "gorget_tls_connect" | "gorget_tls_server_bind"
        | "gorget_path_extension" | "gorget_path_dirname" | "gorget_path_basename"
        | "gorget_path_exists" | "gorget_path_is_file" | "gorget_path_is_dir"
        | "gorget_path_normalize"
        | "gorget_getenv" | "gorget_unsetenv"
        | "gorget_regex_compile"
        | "gorget_exec" | "gorget_exec_capture" => param_idx == 0,
        // File I/O: const char* path (and content for write/append)
        "gorget_read_file" | "gorget_read_file_bytes" | "gorget_write_file_bytes"
        | "gorget_file_exists" | "gorget_delete_file"
        | "gorget_mkdir" | "gorget_rmdir" | "gorget_is_dir"
        | "gorget_file_size" | "gorget_readdir"
        | "gorget_path_parent" | "gorget_path_stem" | "gorget_path_absolute" => param_idx == 0,
        "gorget_write_file" | "gorget_append_file" | "gorget_rename" | "gorget_copy_file" => param_idx == 0 || param_idx == 1,
        // gorget_format_time(int64_t epoch, const char* fmt) — arg 1 is cstr
        "gorget_format_time" => param_idx == 1,
        // gorget_parse_time(const char* s, const char* fmt) — both args are cstr
        "gorget_parse_time" => param_idx == 0 || param_idx == 1,
        // gorget_udp_sendto(sock*, data*, const char* host, int port) — arg 2 is cstr
        "gorget_udp_sendto" => param_idx == 2,
        // gorget_udp_join/leave_multicast(sock*, const char* group) — arg 1 is cstr
        "gorget_udp_join_multicast" | "gorget_udp_leave_multicast" => param_idx == 1,
        // Byte conversion functions that take const char*
        "gorget_bytes_from_str" | "gorget_bytes_from_hex" => param_idx == 0,
        _ => false,
    }
}

/// Returns true if a function that takes_cstr_for_str_param ALSO requires the
/// string to be null-terminated (because it uses strtoll/strtod/strcmp internally).
/// For these, we must use gorget_str_to_cstr() rather than just .data, since Str
/// slices (e.g. from byte_slice) are NOT null-terminated.
fn needs_null_terminated_cstr(fn_name: &str) -> bool {
    matches!(fn_name,
        "gorget_parse_int" | "gorget_parse_float"
        | "gorget_parse_time"
        | "gorget_file_open"
        | "gorget_read_file" | "gorget_read_file_bytes"
        | "gorget_write_file" | "gorget_write_file_bytes"
        | "gorget_path_join" | "gorget_path_extension" | "gorget_path_dirname"
        | "gorget_path_basename" | "gorget_path_exists" | "gorget_path_is_file"
        | "gorget_path_is_dir" | "gorget_path_normalize"
        | "gorget_file_exists" | "gorget_delete_file"
        | "gorget_mkdir" | "gorget_rmdir" | "gorget_is_dir"
        | "gorget_file_size" | "gorget_readdir"
        | "gorget_path_parent" | "gorget_path_stem" | "gorget_path_absolute"
        | "gorget_append_file" | "gorget_rename" | "gorget_copy_file"
        | "gorget_getenv" | "gorget_setenv" | "gorget_unsetenv"
        | "gorget_regex_compile"
        | "gorget_exec" | "gorget_exec_capture"
        | "gorget_socket_connect" | "gorget_server_socket_bind"
        | "gorget_tls_connect" | "gorget_tls_server_bind"
        | "gorget_udp_bind" | "gorget_udp_send_to" | "gorget_udp_sendto"
        | "gorget_udp_join_multicast" | "gorget_udp_leave_multicast"
        | "gorget_bytes_from_str" | "gorget_bytes_from_hex"
    )
}

/// Sanitize a field name for C.
fn c_field_name(name: &str) -> String {
    name.replace('.', "_").replace('-', "_")
}

/// C keywords and type names that cannot be used as identifiers.
const C_RESERVED: &[&str] = &[
    "auto", "break", "case", "char", "const", "continue", "default", "do",
    "double", "else", "enum", "extern", "float", "for", "goto", "if",
    "int", "long", "register", "return", "short", "signed", "sizeof",
    "static", "struct", "switch", "typedef", "union", "unsigned", "void",
    "volatile", "while", "inline", "restrict", "_Bool", "_Complex",
    "_Imaginary", "bool", "true", "false",
];

/// Escape a function name that clashes with C keywords by adding a prefix.
fn c_func_name(name: &str) -> String {
    if C_RESERVED.contains(&name) {
        format!("__gg_{name}")
    } else {
        name.to_string()
    }
}

/// Map an LIR type to the appropriate trace formatter function name.
fn lir_trace_formatter(ty: &LirType, module: &LirModule) -> &'static str {
    match ty {
        LirType::Bool => "__gorget_trace_val_bool",
        LirType::F32 | LirType::F64 => "__gorget_trace_val_float",
        LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64
        | LirType::U8 | LirType::U16 | LirType::U32 | LirType::U64 => "__gorget_trace_val_int",
        LirType::Struct(sid) => {
            if let Some(s) = module.structs.get(sid.0 as usize) {
                if s.name == "GorgetStringView" || s.name == "GorgetString" {
                    return "__gorget_trace_val_Str";
                }
            }
            "__gorget_trace_val_int" // fallback
        }
        _ => "__gorget_trace_val_int", // fallback for Ptr, Void
    }
}

/// Format a float for C source.
fn format_float(val: f64) -> String {
    if val.is_nan() {
        "NAN".into()
    } else if val.is_infinite() {
        if val > 0.0 {
            "INFINITY".into()
        } else {
            "(-INFINITY)".into()
        }
    } else {
        // Use enough precision to round-trip.
        format!("{:.17e}", val)
    }
}

/// Fix printf format strings: replace `%lld` with `%f` at positions where
/// the corresponding variadic arg is a float.
#[derive(Clone, Copy, PartialEq)]
enum PrintfArgKind { Int, Float, Str }

fn fix_printf_format(fmt: &str, arg_kinds: &[PrintfArgKind]) -> String {
    let mut result = String::with_capacity(fmt.len());
    let bytes = fmt.as_bytes();
    let mut i = 0;
    let mut arg_idx = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' {
            if i + 1 < bytes.len() && bytes[i + 1] == b'%' {
                // Literal %% — not a format specifier.
                result.push_str("%%");
                i += 2;
            } else if i + 3 < bytes.len() && bytes[i..i + 4] == *b"%lld" {
                let kind = arg_kinds.get(arg_idx).copied().unwrap_or(PrintfArgKind::Int);
                match kind {
                    PrintfArgKind::Float => result.push_str("%f"),
                    PrintfArgKind::Str => result.push_str("%.*s"),
                    PrintfArgKind::Int => result.push_str("%lld"),
                }
                arg_idx += 1;
                i += 4;
            } else {
                // Other format spec (%s, %d, %c, etc.) — copy through.
                result.push('%');
                i += 1;
                // Skip flags/width/precision
                while i < bytes.len() && !bytes[i].is_ascii_alphabetic() && bytes[i] != b'%' {
                    result.push(bytes[i] as char);
                    i += 1;
                }
                if i < bytes.len() && bytes[i].is_ascii_alphabetic() {
                    result.push(bytes[i] as char);
                    i += 1;
                }
                arg_idx += 1;
            }
        } else {
            result.push(bytes[i] as char);
            i += 1;
        }
    }
    result
}

/// Escape a string for C string literal.
fn escape_c_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 8);
    let chars: Vec<char> = s.chars().collect();
    for (ci, &c) in chars.iter().enumerate() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\0' => out.push_str("\\0"),
            c if c.is_ascii_graphic() || c == ' ' => out.push(c),
            c => {
                for byte in c.to_string().as_bytes() {
                    write!(out, "\\x{byte:02x}").unwrap();
                }
                // If the next character is a hex digit, break the string literal
                // to prevent C from consuming it as part of the \x escape.
                // e.g., \xc3\xa9 followed by 'b' would be parsed as \xc3 \xa9b (wrong).
                // Emitting "\xc3\xa9" "b" uses C string concatenation to avoid this.
                if let Some(&next) = chars.get(ci + 1) {
                    if next.is_ascii_hexdigit() {
                        out.push_str("\" \"");
                    }
                }
            }
        }
    }
    out
}

/// Infer the result type of an instruction (for variable declarations).
/// `val_types` provides already-resolved types for operands (used for arithmetic propagation).
fn infer_inst_type(inst: &Inst, module: &LirModule, val_types: &[Option<LirType>], ptr_pointee: &[Option<LirType>]) -> Option<LirType> {
    match inst {
        Inst::SlotLoad { ty, .. } => Some(ty.clone()),
        Inst::SlotAddr { .. } => Some(LirType::Ptr),
        Inst::IConst { ty, .. } => Some(ty.clone()),
        Inst::FConst { ty, .. } => Some(ty.clone()),
        Inst::BoolConst { .. } => Some(LirType::Bool),
        Inst::NullPtr { .. } => Some(LirType::Ptr),
        Inst::FuncAddr { .. } => Some(LirType::Ptr),
        Inst::GlobalAddr { .. } => Some(LirType::Ptr),
        Inst::StrLit { .. } => Some(LirType::Ptr), // simplified
        Inst::ParamRef { ty, .. } => Some(ty.clone()),

        // Arithmetic — use the explicit type field.
        Inst::Add { ty, .. } | Inst::Sub { ty, .. } | Inst::Mul { ty, .. }
        | Inst::Div { ty, .. } | Inst::Rem { ty, .. } | Inst::Mod { ty, .. }
        | Inst::Neg { ty, .. } => Some(ty.clone()),

        // Bitwise — use the explicit type field.
        Inst::BitAnd { ty, .. } | Inst::BitOr { ty, .. } | Inst::BitXor { ty, .. }
        | Inst::Shl { ty, .. } | Inst::Shr { ty, .. }
        | Inst::BitNot { ty, .. } => Some(ty.clone()),

        Inst::Cmp { .. } | Inst::Not { .. } => Some(LirType::Bool),

        Inst::IntCast { to, .. } | Inst::FloatCast { to, .. }
        | Inst::IntToFloat { to, .. } | Inst::FloatToInt { to, .. }
        | Inst::Bitcast { to, .. } => Some(to.clone()),
        Inst::PtrCast { .. } => Some(LirType::Ptr),

        Inst::Load { ty, .. } => Some(ty.clone()),
        Inst::FieldPtr { .. } | Inst::ElemPtr { .. } => Some(LirType::Ptr),

        Inst::Call { func, .. } => {
            Some(module.functions[func.0 as usize].return_type.clone())
        }
        Inst::CallExtern { name, .. } => {
            // Polymorphic externs (option/result unwrap, expect, combinators) are
            // called with different return types at different call sites. The single
            // extern declaration merges all sites, producing the wrong type.
            // For unwrap/expect/unwrap_err, try to recover from the struct definition.
            if is_polymorphic_extern(name) {
                // Try to infer from struct definition for unwrap/expect/unwrap_err.
                let is_unwrap_err = name.ends_with("__unwrap_err") || name.ends_with("__unwrap_error");
                let is_unwrap = !is_unwrap_err && (is_option_result_unwrap(name) || is_option_result_expect(name));
                if is_unwrap || is_unwrap_err {
                    // Parse struct prefix from name: Option__T__unwrap → Option__T
                    // Result__T__S__unwrap_error → Result__T__S
                    let suffix = if name.ends_with("__unwrap_error") { "__unwrap_error" }
                        else if name.ends_with("__unwrap_err") { "__unwrap_err" }
                        else if name.ends_with("__expect") { "__expect" }
                        else if name.ends_with("__unwrap_or") { "__unwrap_or" }
                        else if name.ends_with("__unwrap_or_else") { "__unwrap_or_else" }
                        else { "__unwrap" };
                    if let Some(prefix) = name.strip_suffix(suffix) {
                        let found = module.structs.iter().find(|s| s.name == prefix);
                        if let Some(sdef) = found {
                            // For unwrap/expect: payload is field 1 (Ok/Some)
                            // For unwrap_err: payload is field 2 (Error)
                            let field_idx = if is_unwrap_err { 2 } else { 1 };
                            if let Some((_, ty)) = sdef.fields.get(field_idx) {
                                return Some(ty.clone());
                            }
                        }
                    }
                }
                // Fallback: try to recover from arg type (for generic __option_unwrap etc.)
                // The arg is usually a pointer (SlotAddr) to an Option/Result struct.
                // Check both val_types (direct struct) and ptr_pointee (pointer to struct).
                if let Inst::CallExtern { args, .. } = inst {
                    if let Some(arg0) = args.first() {
                        let field_idx = if name.contains("unwrap_err") || name.contains("unwrap_error") { 2 } else { 1 };
                        // Try direct struct type
                        if let Some(Some(LirType::Struct(sid))) = val_types.get(arg0.0 as usize) {
                            if let Some(s) = module.structs.get(sid.0 as usize) {
                                if let Some((_, ty)) = s.fields.get(field_idx) {
                                    return Some(ty.clone());
                                }
                            }
                        }
                        // Try pointee type (arg is a pointer to the struct)
                        if let Some(Some(LirType::Struct(sid))) = ptr_pointee.get(arg0.0 as usize) {
                            if let Some(s) = module.structs.get(sid.0 as usize) {
                                if let Some((_, ty)) = s.fields.get(field_idx) {
                                    return Some(ty.clone());
                                }
                            }
                        }
                    }
                }
                None
            } else if let Some(rt) = runtime_fn_return_struct(name) {
                // Runtime functions that return struct types by value
                module.structs.iter().enumerate()
                    .find(|(_i, s)| s.name == rt)
                    .map(|(i, _)| LirType::Struct(StructId(i as u32)))
                    .or(Some(LirType::I64))
            } else {
                // For Shared__Vector__X__at, the extern may have wrong return type (I64
                // instead of the actual element type). Parse from the name.
                if let Some(inner) = name.strip_prefix("Shared__Vector__")
                    .and_then(|rest| rest.strip_suffix("__at"))
                {
                    return match inner {
                        "double" => Some(LirType::F64),
                        "float" => Some(LirType::F32),
                        "bool" => Some(LirType::Bool),
                        _ => Some(LirType::I64),
                    };
                }
                // For Shared__X__get, parse the element type similarly.
                if let Some(inner) = name.strip_prefix("Shared__")
                    .and_then(|rest| rest.strip_suffix("__get"))
                    .filter(|rest| !rest.contains("__"))
                {
                    return match inner {
                        "double" => Some(LirType::F64),
                        "float" => Some(LirType::F32),
                        "bool" => Some(LirType::Bool),
                        _ => Some(LirType::I64),
                    };
                }
                module.externs.iter()
                    .find(|e| &e.name == name)
                    .map(|e| e.return_type.clone())
                    .or(Some(LirType::I64))
            }
        }
        Inst::CallPtr { .. } => Some(LirType::I64), // default

        _ => None,
    }
}

/// Runtime functions that return a named struct type by value.
fn runtime_fn_return_struct(name: &str) -> Option<&'static str> {
    match name {
        "gorget_array_clone" | "gorget_array_new" | "gorget_array_with_capacity"
        | "gorget_array_sorted" | "gorget_array_reversed" | "gorget_array_unique"
        | "gorget_array_filter" | "gorget_array_map" | "gorget_array_zip"
        | "gorget_array_flat_map" | "gorget_array_flatten"
        | "gorget_str_split" | "gorget_str_chars" => Some("GorgetArray"),
        "gorget_map_new" | "gorget_map_clone" => Some("GorgetMap"),
        "gorget_set_new" | "gorget_set_clone" => Some("GorgetSet"),
        "gorget_string_new" | "gorget_string_adopt" | "gorget_string_from_concat"
        | "gorget_str_cat" | "gorget_string_format"
        | "gorget_string_format_alloc" => Some("GorgetString"),
        "gorget_file_open" => Some("GorgetFile"),
        _ => None,
    }
}

// ── Backend trait implementation ──────────────────────────────────────

/// C backend that consumes LIR.
pub struct CLirBackend;

impl super::Backend for CLirBackend {
    fn name(&self) -> &str {
        "c-lir"
    }

    fn generate(&self, module: &LirModule) -> super::CodegenOutput {
        super::CodegenOutput {
            code: generate_c(module),
            extension: "c",
        }
    }
}

/// Emit a recipe-based compound drop for array elements.
/// Generates a for-loop over the array, applying each drop action to each element.
fn emit_recipe_array_drop(
    out: &mut String,
    arr: &str,
    actions: &[ElemDropAction],
    module: &LirModule,
    sn: &HashMap<u32, String>,
    depth: usize,
) {
    let idx = format!("__di{}", if depth == 0 { String::new() } else { depth.to_string() });
    write!(out, "for (size_t {idx} = 0; {idx} < ((GorgetArray*){arr})->len; {idx}++) {{ ").unwrap();
    let elem = format!("gorget_array_get((GorgetArray*){arr}, {idx})");
    emit_drop_actions(out, &elem, actions, module, sn, depth + 1);
    write!(out, " }}").unwrap();
}

/// Emit a recipe-based compound drop for map values.
fn emit_recipe_map_drop(
    out: &mut String,
    map: &str,
    actions: &[ElemDropAction],
    module: &LirModule,
    sn: &HashMap<u32, String>,
    depth: usize,
) {
    let idx = format!("__di{}", if depth == 0 { String::new() } else { depth.to_string() });
    write!(out,
        "for (size_t {idx} = 0; {idx} < ((GorgetMap*){map})->cap; {idx}++) {{ \
         if (((GorgetMap*){map})->states[{idx}] == 1) {{ \
         void* __val{depth} = (char*)((GorgetMap*){map})->values + {idx} * ((GorgetMap*){map})->val_size; ").unwrap();
    emit_drop_actions(out, &format!("__val{depth}"), actions, module, sn, depth + 1);
    write!(out, " }} }}").unwrap();
}

/// Emit per-key drops for a map with droppable keys.
fn emit_map_key_drops(out: &mut String, map: &str, drop_fn: &str) {
    write!(out,
        "for (size_t __dki = 0; __dki < ((GorgetMap*){map})->cap; __dki++) {{ \
         if (((GorgetMap*){map})->states[__dki] == 1) {{ \
         {drop_fn}((char*)((GorgetMap*){map})->keys + __dki * ((GorgetMap*){map})->key_size); }} }}").unwrap();
}

/// Emit a sequence of drop actions on a pointer to an element.
fn emit_drop_actions(
    out: &mut String,
    elem_ptr: &str,
    actions: &[ElemDropAction],
    module: &LirModule,
    sn: &HashMap<u32, String>,
    depth: usize,
) {
    for action in actions {
        match action {
            ElemDropAction::Call(fn_name) => {
                write!(out, "{fn_name}({elem_ptr}); ").unwrap();
            }
            ElemDropAction::Field { struct_name, field_idx, actions: sub_actions } => {
                // Find the struct's C name and field offset
                if let Some(field_access) = struct_field_access_expr(elem_ptr, struct_name, *field_idx, module, sn) {
                    emit_drop_actions(out, &field_access, sub_actions, module, sn, depth);
                }
            }
            ElemDropAction::SubElems(sub_actions) => {
                // Element is itself a collection (GorgetArray*); iterate sub-elements
                emit_recipe_array_drop(out, elem_ptr, sub_actions, module, sn, depth);
            }
        }
    }
}

/// Generate a C expression to access a field of a struct pointer.
/// Returns something like "(void*)&((__lir_s5*)ptr)->field_name" or offset-based access.
fn struct_field_access_expr(
    base_ptr: &str,
    struct_name: &str,
    field_idx: u32,
    module: &LirModule,
    sn: &HashMap<u32, String>,
) -> Option<String> {
    // Find the struct definition by original name
    for (i, def) in module.structs.iter().enumerate() {
        if def.name == struct_name {
            let c_name = sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"));
            if let Some((field_name, _field_ty)) = def.fields.get(field_idx as usize) {
                return Some(format!("(void*)&(({c_name}*){base_ptr})->{field_name}"));
            }
        }
    }
    None
}

/// Collect deep-clone operations for a struct that contains resource-type fields.
/// Similar to GIR's `collect_clone_ops` but uses LIR struct info.
/// `path` is the C expression path to the struct (e.g., "dst" or "dst.Some_0").
/// Returns clone statements to be emitted after the shallow copy.
#[allow(dead_code)]
fn collect_clone_ops_lir(
    struct_id: u32,
    path: &str,
    module: &LirModule,
    sn: &HashMap<u32, String>,
) -> Vec<String> {
    let mut ops = Vec::new();
    let Some(sdef) = module.structs.get(struct_id as usize) else { return ops };
    for (fname, fty) in &sdef.fields {
        let c_fname = c_field_name(fname);
        let field_path = format!("{path}.{c_fname}");
        match fty {
            LirType::Struct(sid) => {
                let name = sn.get(&sid.0).map(|s| s.as_str()).unwrap_or("");
                match name {
                    "GorgetArray" => ops.push(format!("{field_path} = gorget_array_clone(&{field_path});")),
                    "GorgetMap" => ops.push(format!("{field_path} = gorget_map_clone(&{field_path});")),
                    "GorgetSet" => ops.push(format!("{field_path} = gorget_set_clone(&{field_path});")),
                    "GorgetString" => ops.push(format!("{field_path} = gorget_string_clone(&{field_path});")),
                    // Str (GorgetStringView) is a borrowed view (data ptr + len), not owned — no clone needed.
                    "Str" => {}
                    _ => {
                        // Recurse into nested structs.
                        let nested = collect_clone_ops_lir(sid.0, &field_path, module, sn);
                        ops.extend(nested);
                    }
                }
            }
            _ => {}
        }
    }
    ops
}

/// Generate a `__gorget_cleanup_push(...)` call for a slot in a test function.
/// Returns None if the slot's type doesn't need cleanup stack registration.
fn test_cleanup_push_code_lir(
    slot_idx: u32,
    func: &LirFunction,
    module: &LirModule,
    sn: &HashMap<u32, String>,
) -> Option<String> {
    let slot = &func.slots[slot_idx as usize];
    let slot_ty = &slot.ty;

    // Get the struct name for struct-typed slots.
    let struct_name = if let LirType::Struct(sid) = slot_ty {
        // Use original GIR name from module.structs (not the __lir_sN alias).
        module.structs.get(sid.0 as usize).map(|s| s.name.as_str())
    } else {
        None
    };

    if let Some(name) = struct_name {
        // Box types: push raw pointer (no address-of since Box is a typedef for T*).
        if name.starts_with("Box__") {
            let _c_name = sn.get(&if let LirType::Struct(sid) = slot_ty { sid.0 } else { 0 }).cloned().unwrap_or_default();
            // Check for trait object Box (Box__TraitObj).
            let inner = &name[5..];
            if module.structs.iter().any(|s| s.name == format!("{inner}_TraitObj")) {
                return Some(format!("    __gorget_cleanup_push(free, (void*)__s{slot_idx}.data);\n"));
            } else {
                return Some(format!("    __gorget_cleanup_push(free, (void*)__s{slot_idx});\n"));
            }
        }

        // GorgetString
        if name == "GorgetString" {
            return Some(format!("    __gorget_cleanup_push((__gorget_cleanup_fn)gorget_string_free, (void*)&__s{slot_idx});\n"));
        }

        // Vector/Array types
        if name.starts_with("Vector__") || name.starts_with("Deque__") || name == "GorgetArray" {
            return Some(format!("    __gorget_cleanup_push((__gorget_cleanup_fn)gorget_array_free, (void*)&__s{slot_idx});\n"));
        }

        // Dict/Map types
        if name.starts_with("Dict__") || name.starts_with("HashMap__") || name == "GorgetMap" {
            return Some(format!("    __gorget_cleanup_push((__gorget_cleanup_fn)gorget_map_free, (void*)&__s{slot_idx});\n"));
        }

        // User struct with custom drop: check if a {Name}__drop function exists.
        let drop_fn_name = format!("{name}__drop");
        if module.functions.iter().any(|f| f.name == drop_fn_name) {
            return Some(format!("    __gorget_cleanup_push((__gorget_cleanup_fn){drop_fn_name}, (void*)&__s{slot_idx});\n"));
        }
    }

    // Ptr-typed slots that are named (e.g., Box[T] lowered as raw pointer).
    // In LIR, Box[T] may also appear as a Ptr slot when the Box typedef isn't used.
    if matches!(slot_ty, LirType::Ptr) {
        // Check if the slot name suggests it's a Box (heuristic).
        // Box slots in test functions are typically registered with `free`.
        // However, we can't reliably detect this without more type info, so skip.
        // The struct-typed Box path above handles the common case.
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generate_minimal() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("main".into(), vec![], LirType::I32);
        let bb0 = func.add_block();
        let v0 = func.next_value();
        func.block_mut(bb0).insts.push(Inst::IConst {
            dst: v0,
            ty: LirType::I32,
            value: 0,
        });
        func.block_mut(bb0).terminator = Term::Ret(v0);
        module.add_function(func);

        let c = generate_c(&module);
        assert!(c.contains("int main(int argc, char** argv)"));
        assert!(c.contains("__v0 = (int32_t)0LL;"));
        assert!(c.contains("return __v0;"));
    }

    #[test]
    fn generate_arithmetic() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("add".into(), vec![LirType::I64, LirType::I64], LirType::I64);
        let bb0 = func.add_block();

        let v0 = func.next_value(); // param a
        let v1 = func.next_value(); // param b
        let v2 = func.next_value(); // result

        let s0 = func.add_slot(LirType::I64, Some("a".into()));
        let s1 = func.add_slot(LirType::I64, Some("b".into()));

        func.block_mut(bb0).insts = vec![
            Inst::SlotLoad { dst: v0, slot: s0, ty: LirType::I64 },
            Inst::SlotLoad { dst: v1, slot: s1, ty: LirType::I64 },
            Inst::Add { dst: v2, ty: LirType::I64, lhs: v0, rhs: v1, overflow: Overflow::Trap },
        ];
        func.block_mut(bb0).terminator = Term::Ret(v2);
        module.add_function(func);

        let c = generate_c(&module);
        // With Overflow::Trap, addition uses __builtin_add_overflow.
        assert!(c.contains("__builtin_add_overflow") || c.contains("__v2 = __v0 + __v1;"));
        assert!(c.contains("return __v2;"));
    }

    #[test]
    fn generate_branch() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("test".into(), vec![], LirType::I32);

        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();

        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb0).insts.push(Inst::BoolConst { dst: v0, value: true });
        func.block_mut(bb0).terminator = Term::Branch {
            cond: v0,
            then_block: bb1,
            then_args: vec![],
            else_block: bb2,
            else_args: vec![],
        };

        func.block_mut(bb1).insts.push(Inst::IConst { dst: v1, ty: LirType::I32, value: 1 });
        func.block_mut(bb1).terminator = Term::Ret(v1);

        func.block_mut(bb2).insts.push(Inst::IConst { dst: v2, ty: LirType::I32, value: 2 });
        func.block_mut(bb2).terminator = Term::Ret(v2);

        module.add_function(func);

        let c = generate_c(&module);
        assert!(c.contains("if (__v0)"));
        assert!(c.contains("goto __bb1;"));
        assert!(c.contains("goto __bb2;"));
    }

    #[test]
    fn generate_struct() {
        let mut module = LirModule::new();
        let sid = module.add_struct(StructDef {
            name: "Point".into(),
            fields: vec![("x".into(), LirType::F64), ("y".into(), LirType::F64)],
            is_enum: false,
        });

        let mut func = LirFunction::new("get_x".into(), vec![LirType::Ptr], LirType::F64);
        let bb0 = func.add_block();
        let s0 = func.add_slot(LirType::Ptr, Some("p".into()));

        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb0).insts = vec![
            Inst::SlotLoad { dst: v0, slot: s0, ty: LirType::Ptr },
            Inst::FieldPtr { dst: v1, base: v0, struct_id: sid, field: 0 },
            Inst::Load { dst: v2, ptr: v1, ty: LirType::F64 },
        ];
        func.block_mut(bb0).terminator = Term::Ret(v2);
        module.add_function(func);

        let c = generate_c(&module);
        assert!(c.contains("struct __lir_s0"));
        assert!(c.contains("double x;"));
        assert!(c.contains("((__lir_s0 *)(__v0))->x"));
    }

    #[test]
    fn escape_strings() {
        assert_eq!(escape_c_string("hello"), "hello");
        assert_eq!(escape_c_string("a\"b"), "a\\\"b");
        assert_eq!(escape_c_string("line\nend"), "line\\nend");
    }
}
