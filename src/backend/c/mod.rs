use std::fmt::Write;

pub mod c_runtime;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::ir::{Function, Global, GlobalInit, Module};
use rustc_hash::FxHashSet;

/// Output from the GIR C backend.
pub struct GirCodegenOutput {
    /// Full C code (used when not splitting host/guest).
    pub c_code: String,
    /// Host binary C code (only `Some` when `module.runtime.hot_reload == true`).
    pub host_code: Option<String>,
    /// Guest shared library C code (only `Some` when `module.runtime.hot_reload == true`).
    pub guest_code: Option<String>,
    /// True when the generated C code uses TLS (requires -lssl -lcrypto at link time).
    /// Set when TLS_SOCKET_RUNTIME or TLS_SERVER_RUNTIME is emitted.
    pub needs_tls: bool,
}

/// Options for hot-reload code generation.
pub struct HotReloadOpts {
    /// Absolute path to the source file (for the file watcher).
    pub watch_path: String,
    /// Base name of the guest shared library (without extension).
    pub guest_lib_name: String,
    /// Shell command to recompile the guest library.
    pub recompile_cmd: String,
}

/// C reserved words and type names that cannot be used as identifiers.
const C_RESERVED: &[&str] = &[
    "auto", "break", "case", "char", "const", "continue", "default", "do",
    "double", "else", "enum", "extern", "float", "for", "goto", "if",
    "inline", "int", "long", "register", "restrict", "return", "short",
    "signed", "sizeof", "static", "struct", "switch", "typedef", "union",
    "unsigned", "void", "volatile", "while", "_Bool", "_Complex", "_Imaginary",
    // Common types from stdint/stdbool
    "bool", "true", "false", "int8_t", "int16_t", "int32_t", "int64_t",
    "uint8_t", "uint16_t", "uint32_t", "uint64_t",
    // POSIX/system functions that conflict with user-defined functions
    "send", "recv", "read", "write", "open", "close", "connect", "accept",
    "bind", "listen", "select", "poll", "signal", "exit", "abort",
];

/// Mangle a function name to avoid C keyword conflicts.
fn mangle_name(name: &str) -> String {
    if name == "main" {
        return name.to_string();
    }
    if C_RESERVED.contains(&name) {
        format!("gg_{name}")
    } else {
        name.to_string()
    }
}

/// Map Gorget stdlib function names to their C runtime names.
fn map_stdlib_name(name: &str) -> &str {
    match name {
        // Conversion
        "int_to_str" => "gorget_int_to_str",
        "float_to_str" => "gorget_float_to_str",
        "bool_to_str" => "gorget_bool_to_str",
        "int_to_float" => "gorget_int_to_float",
        "ord" => "gorget_str_ord",
        "chr" => "gorget_char_chr",
        "parse_int" => "gorget_parse_int",
        "parse_float" => "gorget_parse_float",
        "print_no_newline" => "gorget_print_no_newline",
        "codepoint_to_utf8" => "gorget_codepoint_to_utf8",
        // File I/O
        "read_file" => "gorget_read_file",
        "read_file_bytes" => "gorget_read_file_bytes",
        "write_file" => "gorget_write_file",
        "append_file" => "gorget_append_file",
        "file_exists" => "gorget_file_exists",
        "delete_file" => "gorget_delete_file",
        "file_size" => "gorget_file_size",
        "is_dir" => "gorget_is_dir",
        // Filesystem
        "mkdir" => "gorget_mkdir",
        "rmdir" => "gorget_rmdir",
        "copy_file" => "gorget_copy_file",
        "readdir" => "gorget_readdir",
        "getcwd" => "gorget_getcwd",
        // Path functions
        "path_parent" => "gorget_path_parent",
        "path_basename" => "gorget_path_basename",
        "path_extension" => "gorget_path_extension",
        "path_stem" => "gorget_path_stem",
        "path_join" => "gorget_path_join",
        "path_normalize" => "gorget_path_normalize",
        "path_absolute" => "gorget_path_absolute",
        "rename" => "gorget_rename",
        // I/O
        "readline" => "gorget_readline",
        "input" => "gorget_input",
        "getchar" => "gorget_getchar",
        "term_cols" => "gorget_term_cols",
        "term_rows" => "gorget_term_rows",
        // CLI / process
        "args" => "gorget_args",
        "exec" => "gorget_exec",
        "exec_output" => "gorget_exec_output",
        "process_spawn" => "gorget_process_spawn",
        "getpid" => "gorget_getpid",
        // Process methods
        "Process__wait" => "gorget_process_wait",
        "Process__kill" => "gorget_process_kill",
        "Process__pid" => "gorget_process_pid",
        "Process__write_stdin" => "gorget_process_write_stdin",
        "Process__close_stdin" => "gorget_process_close_stdin",
        "Process__read_stdout" => "gorget_process_read_stdout",
        "Process__read_stderr" => "gorget_process_read_stderr",
        // Memory diagnostics (std.os)
        "mem_allocated" => "gorget_mem_allocated",
        "mem_freed" => "gorget_mem_freed",
        "mem_live" => "gorget_mem_live",
        "mem_alloc_count" => "gorget_mem_alloc_count",
        // Signal functions (std.signal)
        "signal_trap" => "gorget_signal_trap",
        "signal_check" => "gorget_signal_check",
        "signal_wait" => "gorget_signal_wait",
        "signal_ignore" => "gorget_signal_ignore",
        "signal_reset" => "gorget_signal_reset",
        "signal_send" => "gorget_signal_send",
        // AtomicInt methods
        "AtomicInt__new" => "gorget_atomic_int_new",
        "AtomicInt__load" => "gorget_atomic_int_load",
        "AtomicInt__store" => "gorget_atomic_int_store",
        "AtomicInt__add" => "gorget_atomic_int_add",
        "AtomicInt__sub" => "gorget_atomic_int_sub",
        "AtomicInt__compare_exchange" => "gorget_atomic_int_compare_exchange",
        "AtomicInt__free" => "gorget_atomic_int_free",
        // AtomicBool methods
        "AtomicBool__new" => "gorget_atomic_bool_new",
        "AtomicBool__load" => "gorget_atomic_bool_load",
        "AtomicBool__store" => "gorget_atomic_bool_store",
        "AtomicBool__swap" => "gorget_atomic_bool_swap",
        "AtomicBool__compare_exchange" => "gorget_atomic_bool_compare_exchange",
        "AtomicBool__free" => "gorget_atomic_bool_free",
        // Barrier methods
        "Barrier__wait" => "gorget_barrier_wait",
        // CondVar methods
        "CondVar__notify_one" => "gorget_condvar_notify_one",
        "CondVar__notify_all" => "gorget_condvar_notify_all",
        "CondVar__wait"       => "gorget_condvar_wait_guard",
        // WaitGroup methods
        "WaitGroup__new" => "gorget_waitgroup_new",
        "WaitGroup__add" => "gorget_waitgroup_add",
        "WaitGroup__done" => "gorget_waitgroup_done",
        "WaitGroup__wait" => "gorget_waitgroup_wait",
        "WaitGroup__free" => "gorget_waitgroup_free",
        // Semaphore methods
        "Semaphore__new" => "gorget_semaphore_new",
        "Semaphore__acquire" => "gorget_semaphore_acquire",
        "Semaphore__release" => "gorget_semaphore_release",
        "Semaphore__try_acquire" => "gorget_semaphore_try_acquire",
        "Semaphore__free" => "gorget_semaphore_free",
        // OnceFlag methods
        "OnceFlag__new" => "gorget_onceflag_new",
        "OnceFlag__do_once" => "gorget_onceflag_do_once",
        "OnceFlag__is_done" => "gorget_onceflag_is_done",
        // std.thread
        "current_thread_id" => "gorget_current_thread_id",
        // Environment
        "getenv" => "gorget_getenv",
        "setenv" => "gorget_setenv",
        "platform" => "gorget_platform",
        // Time
        "time" => "gorget_time",
        "time_ms" => "gorget_time_ms",
        "format_time" => "gorget_format_time",
        "parse_time" => "gorget_parse_time",
        "sleep_ms" => "gorget_sleep_ms",
        "async_sleep" => "gorget_reactor_sleep_ms",
        // Random
        "rand" => "gorget_rand",
        "rand_range" => "gorget_rand_range",
        "seed" => "gorget_seed",
        // Math
        "abs" => "gorget_abs",
        "min" => "gorget_min",
        "max" => "gorget_max",
        "sqrt" => "gorget_sqrt",
        "floor" => "gorget_floor",
        "ceil" => "gorget_ceil",
        "round" => "gorget_round",
        "log" => "gorget_log",
        "log2" => "gorget_log2",
        "log10" => "gorget_log10",
        "pow" => "gorget_pow",
        "sin" => "gorget_sin",
        "cos" => "gorget_cos",
        "tan" => "gorget_tan",
        "asin" => "gorget_asin",
        "acos" => "gorget_acos",
        "atan" => "gorget_atan",
        "atan2" => "gorget_atan2",
        // Crypto
        "crypto_sha256" => "gorget_crypto_sha256",
        "crypto_sha1" => "gorget_crypto_sha1",
        "crypto_hmac" => "gorget_crypto_hmac",
        "crypto_random_bytes" => "gorget_crypto_random_bytes",
        "crypto_aes_ctr_new" => "gorget_crypto_aes_ctr_new",
        "crypto_bn_from_bytes" => "gorget_crypto_bn_from_bytes",
        "crypto_bn_to_bytes" => "gorget_crypto_bn_to_bytes",
        "crypto_bn_mod_exp" => "gorget_crypto_bn_mod_exp",
        "crypto_rsa_load_public" => "gorget_crypto_rsa_load_public",
        "crypto_rsa_verify" => "gorget_crypto_rsa_verify",
        "crypto_ed25519_keygen" => "gorget_crypto_ed25519_keygen",
        "crypto_ed25519_sign" => "gorget_crypto_ed25519_sign",
        "crypto_ed25519_verify" => "gorget_crypto_ed25519_verify",
        "crypto_x25519_keygen" => "gorget_crypto_x25519_keygen",
        "crypto_x25519_keypair" => "gorget_crypto_x25519_keypair",
        "crypto_x25519_shared" => "gorget_crypto_x25519_shared",
        "crypto_x25519_shared_secret" => "gorget_crypto_x25519_shared_secret",
        "crypto_x25519_dh" => "gorget_crypto_x25519_dh",
        "crypto_hkdf_sha256" => "gorget_crypto_hkdf_sha256",
        "crypto_aes_gcm_encrypt" => "gorget_crypto_aes_gcm_encrypt",
        "crypto_aes_gcm_decrypt" => "gorget_crypto_aes_gcm_decrypt",
        "X25519KeyPair__public_key" => "gorget_crypto_x25519_public",
        "X25519KeyPair__private_key" => "gorget_crypto_x25519_private",
        // CipherContext methods
        "CipherContext__encrypt" => "gorget_crypto_aes_ctr_encrypt",
        "CipherContext__decrypt" => "gorget_crypto_aes_ctr_decrypt",
        // Regex free functions (compile+use+free wrappers)
        "regex_compile" => "gorget_regex_compile",
        "regex_compile_with" => "gorget_regex_compile",
        "regex_is_match" => "gorget_regex_is_match_pat",
        "regex_find" => "gorget_regex_find_pat",
        // Socket
        "socket_connect" => "gorget_socket_connect",
        "socket_listen" => "gorget_socket_listen",
        "tls_connect" => "gorget_tls_connect",
        "tls_server_bind" => "gorget_tls_server_bind",
        "TlsServerSocket__accept" => "gorget_tls_server_accept",
        "TlsServerSocket__close" => "gorget_tls_server_close",
        "TlsSocket__set_timeout" => "gorget_tls_set_timeout",
        "udp_bind" => "gorget_udp_bind",
        // Bytes
        "bytes_from_str" => "gorget_bytes_from_str",
        "bytes_to_str" => "gorget_bytes_to_str",
        "bytes_from_hex" => "gorget_bytes_from_hex",
        "bytes_to_hex" => "gorget_bytes_to_hex",
        "bytes_concat" => "gorget_bytes_concat",
        "bytes_slice" => "gorget_bytes_slice",
        "bytes_write_u16_be" => "gorget_bytes_write_u16_be",
        "bytes_read_u16_be" => "gorget_bytes_read_u16_be",
        "bytes_write_u32_be" => "gorget_bytes_write_u32_be",
        "bytes_read_u32_be" => "gorget_bytes_read_u32_be",
        "bytes_write_u16_le" => "gorget_bytes_write_u16_le",
        "bytes_read_u16_le" => "gorget_bytes_read_u16_le",
        "bytes_write_u32_le" => "gorget_bytes_write_u32_le",
        "bytes_read_u32_le" => "gorget_bytes_read_u32_le",
        // Encoding
        "base64_encode" => "gorget_base64_encode",
        "base64_decode" => "gorget_base64_decode",
        "hex_encode" => "gorget_hex_encode",
        "hex_decode" => "gorget_hex_decode",
        "url_encode" => "gorget_url_encode",
        "url_decode" => "gorget_url_decode",
        // Regex free functions (string-based, non-compiled)
        "regex_match" => "gorget_regex_match",
        "regex_find_all" => "gorget_regex_find_all",
        "regex_replace" => "gorget_regex_replace_pat",
        // Allocator methods
        "Arena__bytes_used" => "gorget_arena_bytes_used",
        "Arena__checkpoint" => "gorget_arena_checkpoint",
        "Arena__restore" => "gorget_arena_restore",
        "Arena__reset" => "gorget_arena_reset",
        "Arena__destroy" => "gorget_arena_destroy",
        "TrackingAllocator__alloc_count" => "gorget_tracking_alloc_count",
        "TrackingAllocator__free_count" => "gorget_tracking_free_count",
        "TrackingAllocator__bytes_allocated" => "gorget_tracking_bytes_allocated",
        "TrackingAllocator__bytes_freed" => "gorget_tracking_bytes_freed",
        "TrackingAllocator__current_bytes" => "gorget_tracking_current_bytes",
        "TrackingAllocator__peak_bytes" => "gorget_tracking_peak_bytes",
        "TrackingAllocator__realloc_count" => "gorget_tracking_realloc_count",
        "TrackingAllocator__reset" => "gorget_tracking_reset",
        "TrackingAllocator__report" => "gorget_tracking_report",
        "TrackingAllocator__destroy" => "gorget_tracking_destroy",
        "PoolAllocator__used_blocks" => "gorget_pool_used_blocks",
        "PoolAllocator__free_blocks" => "gorget_pool_free_blocks",
        "PoolAllocator__total_blocks" => "gorget_pool_total_blocks",
        "PoolAllocator__block_size" => "gorget_pool_block_size",
        "PoolAllocator__reset" => "gorget_pool_reset",
        "PoolAllocator__destroy" => "gorget_pool_destroy",
        "TlsfAllocator__bytes_used" => "gorget_tlsf_bytes_used",
        "TlsfAllocator__peak_bytes" => "gorget_tlsf_peak_bytes",
        "TlsfAllocator__pool_size" => "gorget_tlsf_pool_size",
        "TlsfAllocator__reset" => "gorget_tlsf_reset",
        "TlsfAllocator__destroy" => "gorget_tlsf_destroy",
        "FixedBufferAllocator__bytes_used" => "gorget_fba_bytes_used",
        "FixedBufferAllocator__capacity" => "gorget_fba_capacity",
        "FixedBufferAllocator__reset" => "gorget_fba_reset",
        "FixedBufferAllocator__destroy" => "gorget_fba_destroy",
        "FallbackAllocator__primary_count" => "gorget_fallback_primary_count",
        "FallbackAllocator__fallback_count" => "gorget_fallback_fallback_count",
        "FallbackAllocator__destroy" => "gorget_fallback_destroy",
        // UdpSocket methods
        "UdpSocket__sendto" => "gorget_udp_sendto",
        "UdpSocket__recvfrom" => "gorget_udp_recvfrom",
        "UdpSocket__join_multicast" => "gorget_udp_join_multicast",
        "UdpSocket__poll" => "gorget_udp_poll",
        "UdpSocket__set_nonblocking" => "gorget_udp_set_nonblocking",
        "UdpSocket__leave_multicast" => "gorget_udp_leave_multicast",
        "UdpSocket__set_multicast_loopback" => "gorget_udp_set_multicast_loopback",
        "UdpSocket__local_addr" => "gorget_udp_local_addr",
        "UdpSocket__close" => "gorget_udp_close",
        // ServerSocket
        "server_socket_bind" => "gorget_server_socket_bind",
        "ServerSocket__accept" => "gorget_server_socket_accept",
        "ServerSocket__close" => "gorget_server_socket_close",
        // Socket methods
        "Socket__read" => "gorget_socket_read",
        "Socket__read_exact" => "gorget_socket_read_exact",
        "Socket__write" => "gorget_socket_write",
        "Socket__write_str" => "gorget_socket_write_str",
        "Socket__read_line" => "gorget_socket_read_line",
        "Socket__set_timeout" => "gorget_socket_set_timeout",
        "Socket__close" => "gorget_socket_close",
        // TlsSocket methods
        "TlsSocket__read" => "gorget_tls_read",
        "TlsSocket__read_exact" => "gorget_tls_read_exact",
        "TlsSocket__write" => "gorget_tls_write",
        "TlsSocket__write_str" => "gorget_tls_write_str",
        "TlsSocket__read_line" => "gorget_tls_read_line",
        "TlsSocket__close" => "gorget_tls_close",
        // Regex methods
        "Regex__is_match" => "gorget_regex_is_match",
        "Regex__find" => "gorget_regex_find",
        "Regex__find_at" => "gorget_regex_find_at",
        "Regex__find_all" => "gorget_regex_find_all",
        "Regex__replace" => "gorget_regex_replace",
        "Regex__split" => "gorget_regex_split",
        "Regex__splitn" => "gorget_regex_split",
        "Regex__fullmatch" => "gorget_regex_fullmatch",
        "Regex__groups" => "gorget_regex_groups",
        "Regex__free" => "gorget_regex_free",
        "Match__group" => "gorget_regex_match_group",
        "Match__group_by_name" => "gorget_regex_match_group_by_name",
        // File methods
        "File__create" => "gorget_file_create",
        "File__open" => "gorget_file_open",
        "File__write" => "gorget_file_write_handle",
        "File__read_all" => "gorget_file_read_all",
        "File__close" => "gorget_file_close",
        // Str methods (Type__method → gorget_str_method)
        "Str__hash" => "gorget_str_hash",
        "Str__slice" => "gorget_str_slice",
        // SDL: sdl_foo → gorget_sdl_foo
        "sdl_init" => "gorget_sdl_init",
        "sdl_quit" => "gorget_sdl_quit",
        "sdl_create_window" => "gorget_sdl_create_window",
        "sdl_create_window_try" => "gorget_sdl_create_window_try",
        "sdl_window_is_null" => "gorget_sdl_window_is_null",
        "sdl_window_to_handle" => "gorget_sdl_window_to_handle",
        "sdl_get_error" => "gorget_sdl_get_error",
        "sdl_create_renderer" => "gorget_sdl_create_renderer",
        "sdl_create_renderer_try" => "gorget_sdl_create_renderer_try",
        "sdl_renderer_is_null" => "gorget_sdl_renderer_is_null",
        "sdl_destroy_window" => "gorget_sdl_destroy_window",
        "sdl_destroy_renderer" => "gorget_sdl_destroy_renderer",
        "sdl_set_draw_color" => "gorget_sdl_set_draw_color",
        "sdl_clear" => "gorget_sdl_clear",
        "sdl_present" => "gorget_sdl_present",
        "sdl_draw_rect" => "gorget_sdl_draw_rect",
        "sdl_fill_rect" => "gorget_sdl_fill_rect",
        "sdl_draw_line" => "gorget_sdl_draw_line",
        "sdl_draw_point" => "gorget_sdl_draw_point",
        "sdl_set_blend_mode" => "gorget_sdl_set_blend_mode",
        "sdl_load_texture" => "gorget_sdl_load_texture",
        "sdl_destroy_texture" => "gorget_sdl_destroy_texture",
        "sdl_render_texture" => "gorget_sdl_render_texture",
        "sdl_render_texture_sized" => "gorget_sdl_render_texture_sized",
        "sdl_set_texture_alpha" => "gorget_sdl_set_texture_alpha",
        "sdl_texture_width" => "gorget_sdl_texture_width",
        "sdl_texture_height" => "gorget_sdl_texture_height",
        "sdl_get_window_width" => "gorget_sdl_get_window_width",
        "sdl_get_window_height" => "gorget_sdl_get_window_height",
        "sdl_get_display_width" => "gorget_sdl_get_display_width",
        "sdl_get_display_height" => "gorget_sdl_get_display_height",
        "sdl_get_ticks" => "gorget_sdl_get_ticks",
        "sdl_get_performance_counter" => "gorget_sdl_get_performance_counter",
        "sdl_delay" => "gorget_sdl_delay",
        "sdl_has_event" => "gorget_sdl_has_event",
        "sdl_poll_event" => "gorget_sdl_poll_event",
        "sdl_load_font" => "gorget_sdl_load_font",
        "sdl_close_font" => "gorget_sdl_close_font",
        "sdl_draw_text" => "gorget_sdl_draw_text",
        "sdl_render_text" => "gorget_sdl_render_text",
        "sdl_text_width" => "gorget_sdl_text_width",
        "sdl_text_height" => "gorget_sdl_text_height",
        // SDL mouse capture, relative mode, text input
        "sdl_set_relative_mouse_mode" => "gorget_sdl_set_relative_mouse_mode",
        "sdl_show_cursor" => "gorget_sdl_show_cursor",
        "sdl_get_relative_mouse_state" => "gorget_sdl_get_relative_mouse_state",
        "sdl_warp_mouse_in_window" => "gorget_sdl_warp_mouse_in_window",
        "sdl_get_mouse_state" => "gorget_sdl_get_mouse_state",
        "sdl_start_text_input" => "gorget_sdl_start_text_input",
        "sdl_stop_text_input" => "gorget_sdl_stop_text_input",
        _ => name,
    }
}

/// Functions that take `const char*` arguments (need Str → .data coercion).
fn is_cstr_param_fn(name: &str) -> bool {
    matches!(name,
        "gorget_parse_int" | "gorget_parse_float"
        | "gorget_throw" | "gorget_panic"
        | "gorget_write_file" | "gorget_append_file" | "gorget_read_file"
        | "gorget_file_exists" | "gorget_delete_file" | "gorget_file_size" | "gorget_is_dir"
        | "gorget_mkdir" | "gorget_rmdir" | "gorget_rename" | "gorget_copy_file"
        | "gorget_readdir" | "gorget_getcwd"
        | "gorget_path_parent" | "gorget_path_basename" | "gorget_path_extension"
        | "gorget_path_stem" | "gorget_path_join"
        | "gorget_path_normalize" | "gorget_path_absolute"
        | "gorget_readline" | "gorget_input"
        | "gorget_exec" | "gorget_exec_output"
        | "gorget_getenv" | "gorget_setenv"
        | "gorget_format_time" | "gorget_parse_time"
        | "gorget_seed" | "gorget_sleep_ms" | "gorget_reactor_sleep_ms"
        | "gorget_server_socket_bind"
        | "gorget_socket_connect" | "gorget_socket_write_str"
        | "gorget_tls_connect" | "gorget_tls_write_str" | "gorget_tls_server_bind"
        | "gorget_udp_bind" | "gorget_udp_join_multicast" | "gorget_udp_leave_multicast"
        | "gorget_base64_encode" | "gorget_base64_decode"
        | "gorget_hex_encode" | "gorget_hex_decode"
        | "gorget_url_encode" | "gorget_url_decode"
        | "gorget_regex_match" | "gorget_regex_find_all" | "gorget_regex_replace"
        | "gorget_regex_is_match" | "gorget_regex_fullmatch"
        | "gorget_regex_compile" | "gorget_regex_escape" | "gorget_regex_parse_flags"
        | "gorget_regex_replace_all" | "gorget_regex_split" | "gorget_regex_find"
        | "gorget_regex_is_match_pat" | "gorget_regex_find_pat" | "gorget_regex_replace_pat"
        | "gorget_bytes_from_str" | "gorget_bytes_from_hex"
        | "puts" | "fputs" | "system" | "getenv"
        | "gorget_string_new"
        | "gorget_read_file_bytes"
        | "gorget_file_create" | "gorget_file_open" | "gorget_file_write_handle"
        | "gorget_sdl_create_window" | "gorget_sdl_create_window_try"
        | "gorget_sdl_load_texture" | "gorget_sdl_load_font"
        | "gorget_sdl_render_text" | "gorget_sdl_draw_text"
        | "gorget_sdl_text_width" | "gorget_sdl_text_height"
        | "gorget_hot_load"
    )
}

/// Runtime functions that take `const GorgetArray*` or `GorgetArray*` pointer arguments.
/// These need address-of (`&`) when the GIR passes array values instead of pointers.
fn takes_array_ptr_args(name: &str) -> bool {
    // gorget_bytes_* functions that take array pointers (NOT gorget_bytes_from_str/from_hex which take const char*)
    if name.starts_with("gorget_bytes_") {
        return !matches!(name, "gorget_bytes_from_str" | "gorget_bytes_from_hex");
    }
    // gorget_crypto_* functions that take array pointers (NOT those returning void/string only)
    if name.starts_with("gorget_crypto_") {
        return !matches!(name, "gorget_crypto_last_error" | "gorget_crypto_random_bytes"
            | "gorget_crypto_x25519_keygen" | "gorget_crypto_ed25519_keygen");
    }
    // Cipher functions take array pointer args
    if matches!(name, "gorget_cipher_encrypt" | "gorget_cipher_decrypt") {
        return true;
    }
    // Metal functions that take GorgetArray* (Vector[uint8]) arguments
    if matches!(name,
        "gorget_metal_create_buffer_with_data"
        | "gorget_metal_texture_upload"
        | "gorget_metal_texture_upload_mip"
        | "gorget_metal_encoder_set_vertex_bytes"
        | "gorget_metal_encoder_set_fragment_bytes"
        | "gorget_metal_create_library_from_data"
        | "gorget_metal_compute_set_bytes"
        | "gorget_metal_create_render_pipeline_mrt"
        | "gorget_metal_buffer_write"
    ) {
        return true;
    }
    // GL functions that take GorgetArray* (Vector[uint8]) arguments
    if matches!(name,
        "gorget_gl_vertex_pointer"
        | "gorget_gl_tex_coord_pointer"
        | "gorget_gl_color_pointer"
        | "gorget_gl_normal_pointer"
        | "gorget_gl_clip_plane"
        | "gorget_gl_read_pixels"
        | "gorget_gl_tex_sub_image_2d"
        | "gorget_gl_copy_tex_sub_image_2d"
        | "gorget_gl_tex_image_3d"
        | "gorget_gl_tex_sub_image_3d"
        | "gorget_gl_uniform_3fv"
        | "gorget_gl_uniform_4fv"
        | "gorget_gl_uniform_matrix3fv"
        | "gorget_gl_uniform_matrix4fv"
        | "gorget_gl_buffer_data"
        | "gorget_gl_tex_image_2d"
        | "gorget_gl_load_matrix"
        | "gorget_gl_draw_buffers"
        | "gorget_gl_buffer_storage"
        | "gorget_gl_patch_parameter_fv"
        | "gorget_gl_compressed_tex_image_2d"
        | "gorget_gl_get_tex_image"
        | "gorget_gl_get_program_binary"
        | "gorget_gl_program_binary"
    ) {
        return true;
    }
    // std.fs binary file ops
    if matches!(name, "gorget_write_file_bytes") {
        return true;
    }
    // Compression / CRC functions taking array pointer
    if matches!(name, "gorget_crc32_compute" | "gorget_deflate_decompress") {
        return true;
    }
    false
}

/// Functions that return `const char*` or `char*` (need wrapping to Str/GorgetString).
fn returns_cstr(name: &str) -> bool {
    matches!(name,
        "gorget_int_to_str" | "gorget_float_to_str" | "gorget_bool_to_str"
        | "gorget_codepoint_to_utf8"
        | "gorget_path_parent" | "gorget_path_basename" | "gorget_path_extension"
        | "gorget_path_stem" | "gorget_path_join"
        | "gorget_path_normalize" | "gorget_path_absolute"
        | "gorget_readline" | "gorget_input"
        | "gorget_getcwd" | "gorget_platform"
        | "gorget_format_time"
        | "gorget_base64_encode" | "gorget_hex_encode"
        | "gorget_bytes_to_str" | "gorget_bytes_to_hex"
        | "gorget_url_encode"
        | "getenv" | "gorget_getenv"
        | "gorget_regex_match_text" | "gorget_regex_pattern_str"
        | "gorget_sdl_get_error"
    )
}

/// Translate a GIR Module into C output (main path; hot-reload opts from `module`).
pub fn generate_c(module: &Module) -> GirCodegenOutput {
    generate_c_impl(module, None)
}

/// Translate a GIR Module into C output with explicit hot-reload opts.
pub fn generate_c_with_opts(module: &Module, hr_opts: Option<&HotReloadOpts>) -> GirCodegenOutput {
    generate_c_impl(module, hr_opts)
}

fn generate_c_impl(module: &Module, hr_opts: Option<&HotReloadOpts>) -> GirCodegenOutput {
    let mut out = String::with_capacity(4096);

    // Full runtime preamble (provides Str, GorgetString, GorgetArray, etc.)
    out.push_str(c_runtime::RUNTIME_PREAMBLE);
    // Use test panic handler if module has test functions (or is in test mode).
    if module.runtime.test_fns.is_empty() && module.runtime.bench_fns.is_empty() && !module.runtime.is_test_module {
        out.push_str(c_runtime::PANIC_NORMAL);
    } else {
        out.push_str(c_runtime::PANIC_TEST);
    }
    out.push_str(c_runtime::RUNTIME_CORE);

    // Conditionally include optional runtime sections based on functions used
    let all_call_names = collect_all_call_names(module);

    // Pre-scan: detect if any coroutine candidate has blocking or sleep yield points.
    // This determines whether BLOCKING_POOL_RUNTIME and reactor async sleep are needed.
    // Pre-scan: detect if any coroutine candidate has blocking calls.
    // This is used for diagnostic/future io_uring integration purposes.
    let _has_coroutine_blocking = module.runtime.spawned_fns.iter().any(|(fn_name, _, _)| {
        if !fn_is_coroutine_candidate(fn_name, module) { return false; }
        let Some(func) = module.find_function(fn_name) else { return false };
        func.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| {
                classify_yield(inst, func) == Some(YieldKind::Blocking)
            })
        })
    });
    if all_call_names.iter().any(|n| n.starts_with("gorget_bytes_") || n == "bytes_from_str" || n == "bytes_to_str") {
        out.push_str(c_runtime::BYTES_RUNTIME);
    }
    if all_call_names.iter().any(|n| n.starts_with("gorget_regex_") || n.starts_with("regex_") || n == "Regex") {
        out.push_str(c_runtime::REGEX_RUNTIME);
        // Wrapper functions for free-function regex calls (compile-use-free pattern)
        out.push_str(r#"
static bool gorget_regex_is_match_pat(const char* pattern, const char* subject) {
    GorgetRegex _rx = gorget_regex_compile(pattern, NULL);
    if (!_rx.code) return false;
    bool _r = gorget_regex_is_match(&_rx, subject);
    gorget_regex_free(&_rx);
    return _r;
}
static GorgetRegexMatch gorget_regex_find_pat(const char* pattern, const char* subject) {
    GorgetRegex _rx = gorget_regex_compile(pattern, NULL);
    if (!_rx.code) { GorgetRegexMatch _m; _m.start = -1; return _m; }
    GorgetRegexMatch _m = gorget_regex_find(&_rx, subject, 0);
    gorget_regex_free(&_rx);
    return _m;
}
static GorgetString gorget_regex_replace_pat(const char* pattern, const char* subject, const char* replacement) {
    GorgetRegex _rx = gorget_regex_compile(pattern, NULL);
    if (!_rx.code) return gorget_string_new(subject);
    GorgetString _gs = gorget_regex_replace(&_rx, subject, replacement);
    gorget_regex_free(&_rx);
    return _gs;
}
"#);
    }
    if all_call_names.iter().any(|n| n.starts_with("gorget_crypto_") || n.starts_with("crypto_")) {
        out.push_str(c_runtime::CRYPTO_RUNTIME);
    }
    let needs_server_socket = all_call_names.iter().any(|n| {
        n.starts_with("gorget_server_socket_") || n == "server_socket_bind" || n.starts_with("ServerSocket__")
    });
    if all_call_names.iter().any(|n| n.starts_with("gorget_socket_") || n == "socket_connect" || n == "socket_listen")
        || needs_server_socket {
        out.push_str(c_runtime::SOCKET_RUNTIME);
    }
    if needs_server_socket {
        out.push_str(c_runtime::SERVER_SOCKET_RUNTIME);
    }
    if all_call_names.iter().any(|n| n.starts_with("gorget_udp_") || n == "udp_bind") {
        out.push_str(c_runtime::UDP_SOCKET_RUNTIME);
    }
    let needs_tls_server = all_call_names.iter().any(|n| {
        n.starts_with("gorget_tls_server_") || n == "tls_server_bind" || n.starts_with("TlsServerSocket__")
    });
    let needs_tls = all_call_names.iter().any(|n| n.starts_with("gorget_tls_") || n == "tls_connect")
        || needs_tls_server;
    if needs_tls {
        out.push_str(c_runtime::TLS_SOCKET_RUNTIME);
    }
    if needs_tls_server {
        out.push_str(c_runtime::TLS_SERVER_RUNTIME);
    }
    if all_call_names.iter().any(|n| n == "gorget_exec" || n == "gorget_exec_output" || n == "exec" || n == "exec_output") {
        out.push_str(c_runtime::PROCESS_RUNTIME);
    }
    if module.runtime.has_process
        || all_call_names.iter().any(|n| n.starts_with("gorget_process_") || n == "process_spawn" || n == "getpid" || n == "gorget_getpid" || n.starts_with("Process__") || n.starts_with("gorget_signal_") || n.starts_with("signal_")) {
        out.push_str(c_runtime::PROCESS_SPAWN_RUNTIME);
    }
    if module.runtime.has_sync
        || all_call_names.iter().any(|n| n.starts_with("gorget_atomic_") || n.starts_with("gorget_barrier_") || n.starts_with("gorget_condvar_") || n.starts_with("gorget_rwlock_") || n.starts_with("AtomicInt__") || n.starts_with("AtomicBool__") || n.starts_with("Barrier__") || n.starts_with("CondVar__") || n.starts_with("RWLock__") || n.starts_with("ReadGuard__") || n.starts_with("WriteGuard__") || n.starts_with("WaitGroup__") || n.starts_with("Semaphore__") || n.starts_with("gorget_waitgroup_") || n.starts_with("gorget_semaphore_") || n.starts_with("OnceFlag__") || n.starts_with("gorget_onceflag_")) {
        out.push_str(c_runtime::SYNC_RUNTIME);
    }
    if module.runtime.has_thread
        || !module.runtime.thread_spawned_fns.is_empty()
        || all_call_names.iter().any(|n| n == "gorget_current_thread_id" || n == "current_thread_id" || n.starts_with("__gorget_thread_spawn_") || n.starts_with("Thread__")) {
        out.push_str(c_runtime::THREAD_RUNTIME);
    }
    let needs_async_runtime = module.runtime.has_async || module.runtime.has_spawn
        || all_call_names.iter().any(|n| n.contains("channel_") || n.contains("Channel")
            || n.contains("gorget_executor_") || n == "gorget_spawn" || n.contains("GorgetTask"));
    if needs_async_runtime {
        // ASYNC_RUNTIME must precede CHANNEL_RUNTIME and EXECUTOR_RUNTIME (they use GorgetWaker).
        out.push_str(c_runtime::ASYNC_RUNTIME);
    }
    if all_call_names.iter().any(|n| n.contains("gorget_executor_") || n == "gorget_spawn" || n.contains("GorgetTask"))
        || module.runtime.has_spawn {
        out.push_str(c_runtime::TASK_COMMON);
        match module.runtime.scheduler_mode {
            crate::ir::SchedulerMode::Pool => out.push_str(c_runtime::SCHEDULER_POOL_RUNTIME),
            crate::ir::SchedulerMode::Thread => out.push_str(c_runtime::SCHEDULER_THREAD_RUNTIME),
            crate::ir::SchedulerMode::Inline => out.push_str(c_runtime::SCHEDULER_INLINE_RUNTIME),
            crate::ir::SchedulerMode::Single => out.push_str(c_runtime::SCHEDULER_SINGLE_RUNTIME),
        }
    }
    let needs_channel = !module.runtime.channel_types.is_empty()
        || all_call_names.iter().any(|n| n.contains("channel_") || n.contains("Channel"));
    if needs_channel {
        out.push_str(c_runtime::CHANNEL_RUNTIME);
    }
    if needs_channel || module.runtime.has_spawn {
        // Emit Channel__T typedefs and wrapper functions (if any), plus Task__T structs.
        emit_channel_and_task_defs(&mut out, module);
    }
    let needs_shared = !module.runtime.shared_types.is_empty() || !module.runtime.weak_types.is_empty();
    if needs_shared {
        out.push_str(c_runtime::SHARED_RUNTIME);
        // Wrapper functions (emit_shared_defs, emit_weak_defs) emitted after user type definitions below.
    }
    let needs_mutex = !module.runtime.mutex_types.is_empty();
    if needs_mutex {
        // ASYNC_RUNTIME must be emitted before this (gorget_mutex may use GorgetWaker).
        if !needs_async_runtime {
            out.push_str(c_runtime::ASYNC_RUNTIME);
        }
        out.push_str(c_runtime::MUTEX_RUNTIME);
        // Wrapper functions (emit_mutex_defs) emitted after user type definitions below.
    }
    if module.runtime.has_task_group {
        if !needs_async_runtime && !needs_mutex {
            out.push_str(c_runtime::ASYNC_RUNTIME);
        }
        if !needs_mutex {
            out.push_str(c_runtime::MUTEX_RUNTIME);
        }
        out.push_str(c_runtime::TASK_GROUP_RUNTIME);
    }
    // Blocking pool (standalone pool) is emitted when explicitly requested via spawn_blocking.
    // For auto-detected blocking calls in coroutines, __gorget_blocking_enter/exit is used
    // instead (Go-style temp worker approach, already part of SCHEDULER_POOL_RUNTIME).
    if module.runtime.has_blocking_pool {
        if !needs_async_runtime {
            out.push_str(c_runtime::ASYNC_RUNTIME);
        }
        out.push_str(c_runtime::BLOCKING_POOL_RUNTIME);
    }
    // Detect std.async.async_sleep(ms) calls — mapped to gorget_reactor_sleep_ms in KNOWN_MAPPINGS.
    // Also detect legacy int-arg sleep() calls from std.async import (pre-rename path).
    let needs_reactor = all_call_names.iter().any(|n| n == "async_sleep" || n == "gorget_reactor_sleep_ms"
        || n.starts_with("gorget_socket_async_") || n == "gorget_reactor_wait_readable" || n == "gorget_reactor_wait_writable")
        || module.functions.iter().any(|f| {
            f.blocks.iter().any(|b| b.instructions.iter().any(|inst| {
                let (fname, args) = match inst {
                    Instruction::Call { func, args, .. } => (func.as_str(), args),
                    Instruction::CallExtern { func, args, .. } => (func.as_str(), args),
                    _ => return false,
                };
                if fname != "sleep" && fname != "gg_sleep" { return false; }
                args.first().map_or(false, |a| match a {
                    Operand::Constant(Constant::I64(_)) => true,
                    Operand::Copy(p) | Operand::Move(p) => {
                        let t = f.locals[p.local.0 as usize].type_id;
                        t == I64_TYPE || t == I32_TYPE
                    }
                    _ => false,
                })
            }))
        });
    if needs_reactor {
        // REACTOR_RUNTIME requires GorgetWaker (ASYNC_RUNTIME) and pthread types (EXECUTOR_RUNTIME).
        if !needs_async_runtime {
            out.push_str(c_runtime::ASYNC_RUNTIME);
        }
        if !all_call_names.iter().any(|n| n.contains("gorget_executor_") || n == "gorget_spawn") && !module.runtime.has_spawn {
            out.push_str(c_runtime::TASK_COMMON);
            out.push_str(c_runtime::SCHEDULER_POOL_RUNTIME);
        }
        out.push_str(c_runtime::REACTOR_RUNTIME);
    }
    if module.runtime.hot_reload || all_call_names.iter().any(|n| n.contains("hot_reload") || n.contains("plugin")) {
        out.push_str(c_runtime::HOT_RELOAD_RUNTIME);
    }
    if module.runtime.trace_filename.is_some() {
        out.push_str(c_runtime::TRACE_RUNTIME);
    }
    if all_call_names.iter().any(|n| n.starts_with("sdl_") || n.starts_with("gorget_sdl_")) {
        if all_call_names.iter().any(|n| *n == "sdl_load_texture" || *n == "gorget_sdl_load_texture") {
            out.push_str("#define GORGET_USE_SDL_IMAGE\n");
        }
        if all_call_names.iter().any(|n| *n == "sdl_load_font" || *n == "sdl_close_font" || *n == "sdl_draw_text" || *n == "sdl_render_text" || *n == "sdl_text_width" || *n == "sdl_text_height" || n.starts_with("gorget_sdl_load_font") || n.starts_with("gorget_sdl_draw_text") || n.starts_with("gorget_sdl_render_text")) {
            out.push_str("#define GORGET_USE_SDL_TTF\n");
        }
        out.push_str(c_runtime::SDL_RUNTIME);
    }
    // Bytes f32/f64/i64 helpers (always cheap to include)
    if all_call_names.iter().any(|n| n.starts_with("gorget_bytes_") && (n.contains("f32") || n.contains("f64") || n.contains("i64"))) {
        out.push_str(c_runtime::BYTES_F32_RUNTIME);
    }
    // OpenGL runtime
    if all_call_names.iter().any(|n| n.starts_with("gorget_gl_")) {
        out.push_str(c_runtime::GL_RUNTIME);
    }
    // Image loading runtime (stb_image)
    if all_call_names.iter().any(|n| n.starts_with("gorget_image_")) {
        out.push_str(c_runtime::IMAGE_RUNTIME);
    }
    // Audio runtime (SDL2_mixer)
    if all_call_names.iter().any(|n| n.starts_with("gorget_audio_")) {
        out.push_str(c_runtime::AUDIO_RUNTIME);
    }
    // Zlib/Deflate compression runtime
    if all_call_names.iter().any(|n| n.starts_with("gorget_zlib_") || n.starts_with("gorget_deflate_") || n.starts_with("gorget_crc32_")) {
        out.push_str(c_runtime::COMPRESS_RUNTIME);
    }
    // Metal runtime (macOS only — Objective-C wrappers)
    if all_call_names.iter().any(|n| n.starts_with("gorget_metal_") || n.starts_with("gorget_sdl_metal_")) {
        out.push_str(c_runtime::METAL_RUNTIME);
    }
    let needs_sqlite = all_call_names.iter().any(|n| n.starts_with("gorget_sqlite_") || n == "sqlite_open");
    if needs_sqlite {
        // Ensure platform APIs (mremap, etc.) and disable mmap to avoid portability issues.
        out.push_str("\n#define SQLITE_MAX_MMAP_SIZE 0\n");
        out.push_str("#define HAVE_MREMAP 0\n");
        // Suppress warnings in the amalgamation so user code stays clean.
        out.push_str("#pragma GCC diagnostic push\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wunused-parameter\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wunused-variable\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wunused-function\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wimplicit-fallthrough\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wpedantic\"\n");
        out.push_str(c_runtime::SQLITE_AMALGAMATION);
        out.push_str("\n#pragma GCC diagnostic pop\n");
        out.push_str(c_runtime::SQLITE_GORGET_WRAPPERS);
    }

    // GIR-specific helpers
    out.push_str("\nstatic int gorget_generic_compare(const void* a, const void* b) {\n    return memcmp(a, b, sizeof(int64_t));\n}\n");
    out.push_str("static int gorget_float_compare(const void* a, const void* b) {\n    double da = *(const double*)a, db = *(const double*)b;\n    return (da > db) - (da < db);\n}\n");
    out.push_str("static int gorget_str_compare(const void* a, const void* b) {\n    Str sa = *(const Str*)a, sb = *(const Str*)b;\n    size_t min_len = sa.len < sb.len ? sa.len : sb.len;\n    int c = memcmp(sa.data, sb.data, min_len);\n    if (c != 0) return c;\n    return (sa.len > sb.len) - (sa.len < sb.len);\n}\n");
    // Inline helpers for ord/chr operations
    out.push_str("static inline Str gorget_char_chr(int64_t code) { return gorget_str_from_cstr(gorget_codepoint_to_utf8(code)); }\n");
    out.push_str("static inline int64_t gorget_str_ord(Str s) { size_t pos = 0; return (int64_t)gorget_utf8_decode(s.data, s.len, &pos); }\n");
    out.push_str("static inline uint32_t gorget_str_decode_codepoint(const char* data, size_t len) {\n");
    out.push_str("    if (len == 0) return 0;\n");
    out.push_str("    uint8_t b = (uint8_t)data[0];\n");
    out.push_str("    if (b < 0x80) return b;\n");
    out.push_str("    if ((b & 0xE0) == 0xC0 && len >= 2) return ((b & 0x1F) << 6) | (data[1] & 0x3F);\n");
    out.push_str("    if ((b & 0xF0) == 0xE0 && len >= 3) return ((b & 0x0F) << 12) | ((data[1] & 0x3F) << 6) | (data[2] & 0x3F);\n");
    out.push_str("    if ((b & 0xF8) == 0xF0 && len >= 4) return ((b & 0x07) << 18) | ((data[1] & 0x3F) << 12) | ((data[2] & 0x3F) << 6) | (data[3] & 0x3F);\n");
    out.push_str("    return b;\n");
    out.push_str("}\n");
    out.push_str("static inline Str codepoint_to_str(int64_t code) { return gorget_str_from_cstr(gorget_codepoint_to_utf8(code)); }\n");
    out.push_str("static inline int64_t gorget_str_hash(Str* s) { return (int64_t)__gorget_hash_str_len(s->data, s->len); }\n");
    out.push_str("static inline int64_t __gorget_hash_int(int64_t v) { return (int64_t)__gorget_fnv1a(&v, sizeof(v)); }\n");
    out.push_str("static inline Str Str__substring(Str* s, int64_t start, int64_t end) { return gorget_str_slice(*s, start, end); }\n");
    out.push('\n');

    // Emit typedefs for collection aliases BEFORE type definitions
    // (struct fields may reference Vector__T etc.)
    emit_collection_typedefs(&mut out, module);

    // Emit Shared__T / Weak__T typedefs BEFORE type definitions so that
    // Option__Shared__T structs (emitted during type_definitions) can reference them.
    if needs_shared {
        emit_shared_weak_typedefs(&mut out, module);
    }

    // Type definitions (structs and enums), skipping unmonomorphized templates
    emit_type_definitions(&mut out, module);

    // Emit Mutex[T], RWLock[T] wrappers BEFORE Shared[T] — Shared[Mutex[T]] references Mutex__T.
    if needs_mutex {
        emit_mutex_defs(&mut out, module);
    }
    if !module.runtime.rwlock_types.is_empty() {
        emit_rwlock_defs(&mut out, module);
    }
    // Emit Shared[T] and Weak[T] wrapper functions AFTER mutex/rwlock typedefs
    // (Shared[Mutex[T]] needs Mutex__T to be declared) and after user struct typedefs
    // (Shared[Config] needs Config to be declared).
    if needs_shared {
        emit_shared_defs(&mut out, module);
        emit_weak_defs(&mut out, module);
    }
    if !module.runtime.thread_types.is_empty() || !module.runtime.thread_spawned_fns.is_empty() {
        emit_thread_defs(&mut out, module);
    }

    // Emit Channel recv_timeout implementations (needs Option__T from type_definitions)
    if !module.runtime.channel_types.is_empty() {
        emit_channel_recv_timeout_defs(&mut out, module);
    }

    // Forward declarations for all functions (before globals so vtable refs resolve)
    let skip_names = template_type_names(module);
    for func in &module.functions {
        if is_template_function(&func.name, &skip_names) {
            continue;
        }
        emit_forward_decl(&mut out, func, &module.type_registry);
    }
    out.push('\n');

    // Emit spawn/await helper functions (reference forward-declared GIR functions)
    if !module.runtime.spawned_fns.is_empty() {
        emit_spawn_helpers(&mut out, module);
    }

    // Emit thread spawn helpers (entry functions reference forward-declared GIR functions)
    emit_thread_helpers(&mut out, module);

    // Emit named-function adapters for Callable dispatch (before function definitions)
    emit_func_ref_adapters(&mut out, module);

    // Global constants
    emit_globals(&mut out, module);

    // ── Function Definitions ──
    out.push_str("// ── Function Definitions ──\n");
    let has_test_runner = !module.runtime.test_fns.is_empty() || !module.runtime.bench_fns.is_empty() || module.runtime.is_test_module;
    for func in &module.functions {
        if is_template_function(&func.name, &skip_names) {
            continue;
        }
        // Skip user main() when test/bench runner will provide main()
        if has_test_runner && func.name == "main" {
            continue;
        }
        emit_function(&mut out, func, module);
        out.push('\n');
    }

    // Test/bench runner main.
    if !module.runtime.bench_fns.is_empty() {
        emit_bench_runner_main(&mut out, module);
    } else if !module.runtime.test_fns.is_empty() || module.runtime.is_test_module {
        emit_test_runner_main(&mut out, module);
    }

    // Hot-reload: generate split host/guest sources.
    if module.runtime.hot_reload {
        let (host, guest) = generate_hot_reload_split(module, &out, hr_opts);
        return GirCodegenOutput { c_code: out, host_code: Some(host), guest_code: Some(guest), needs_tls };
    }

    GirCodegenOutput { c_code: out, host_code: None, guest_code: None, needs_tls }
}

/// Split a full compiled C string into host + guest for hot-reload mode.
///
/// - Guest: full code minus main(), plus state hash constant + exported wrappers.
/// - Host: runtime/type section only + HOT_RELOAD_RUNTIME + a dlopen-based main().
fn generate_hot_reload_split(module: &Module, full_c: &str, hr_opts: Option<&HotReloadOpts>) -> (String, String) {
    let state_type = module.runtime.hot_reload_state_type.as_deref().unwrap_or("State");
    let state_hash = module.runtime.hot_reload_state_hash;
    let has_reload = module.runtime.hot_reload_has_reload_fn;

    // ── Guest code ──
    let mut guest = String::with_capacity(full_c.len() + 1024);
    // Remove main() by finding the marker and stripping through the matching brace.
    let main_marker = "int main(int argc, char** argv) {";
    if let Some(main_pos) = full_c.find(main_marker) {
        guest.push_str(&full_c[..main_pos]);
        let after_main = &full_c[main_pos..];
        let mut depth = 0usize;
        let mut end_pos = 0;
        for (i, ch) in after_main.char_indices() {
            if ch == '{' { depth += 1; }
            if ch == '}' {
                depth -= 1;
                if depth == 0 { end_pos = i + 1; break; }
            }
        }
        let remaining = after_main[end_pos..].strip_prefix('\n').unwrap_or(&after_main[end_pos..]);
        guest.push_str(remaining);
    } else {
        guest.push_str(full_c);
    }
    // State hash constant
    guest.push_str(&format!(
        "\n// ── Hot Reload Guest Exports ──\n\
         const uint64_t GORGET_STATE_HASH = 0x{state_hash:016X}ULL;\n\n"
    ));
    // Exported init() wrapper
    guest.push_str(&format!(
        "__attribute__((visibility(\"default\")))\n\
         {state_type} gorget_guest_init(void) {{\n\
         \treturn init();\n\
         }}\n\n"
    ));
    // Exported tick() wrapper
    guest.push_str(&format!(
        "__attribute__((visibility(\"default\")))\n\
         bool gorget_guest_tick({state_type}* state) {{\n\
         \treturn tick(state);\n\
         }}\n\n"
    ));
    // Exported reload() wrapper
    if has_reload {
        guest.push_str(&format!(
            "__attribute__((visibility(\"default\")))\n\
             void gorget_guest_reload({state_type}* state) {{\n\
             \treload(state);\n\
             }}\n\n"
        ));
    }

    // ── Host code ──
    // Use the runtime/types section (before "// ── Function Definitions ──") + host main.
    let mut host = String::with_capacity(4096);
    let func_defs_marker = "// ── Function Definitions ──";
    if let Some(pos) = full_c.find(func_defs_marker) {
        host.push_str(&full_c[..pos]);
    } else {
        host.push_str(c_runtime::RUNTIME_PREAMBLE);
        host.push_str(c_runtime::PANIC_NORMAL);
        host.push_str(c_runtime::RUNTIME_CORE);
    }
    // HOT_RELOAD_RUNTIME was already emitted in the full code; don't double-emit.
    // Just emit the function-pointer typedefs and main().
    host.push_str(&format!(
        "// ── Hot Reload Host ──\n\
         typedef {state_type} (*gorget_init_fn_t)(void);\n\
         typedef bool (*gorget_tick_fn_t)({state_type}*);\n\
         typedef void (*gorget_reload_fn_t)({state_type}*);\n\n"
    ));

    let (guest_lib_name, recompile_cmd, watch_path) = if let Some(opts) = hr_opts {
        (opts.guest_lib_name.as_str(), opts.recompile_cmd.as_str(), opts.watch_path.as_str())
    } else {
        ("guest", "gg build --shared", ".")
    };
    let watch_c = format!("    const char* __watch_paths[] = {{\"{watch_path}\"}};\n    int __watch_count = 1;\n");

    host.push_str(&format!(
        r#"int main(int argc, char** argv) {{
    gorget_init_args(argc, argv);
{watch_c}
    GorgetFileWatcher __watcher = gorget_hot_watch_init(__watch_paths, __watch_count);

    if (system("{recompile_cmd}") != 0) {{
        fprintf(stderr, "[hot-reload] Initial compilation failed\n");
        return 1;
    }}
    GorgetGuestModule __guest = gorget_hot_load("./{guest_lib_name}" GORGET_DYLIB_EXT);
    if (!__guest.handle) {{
        fprintf(stderr, "[hot-reload] Failed to load guest module\n");
        return 1;
    }}

    gorget_init_fn_t __init_fn = (gorget_init_fn_t)__guest.init;
    {state_type} __state = __init_fn();
    gorget_tick_fn_t __tick_fn = (gorget_tick_fn_t)__guest.tick;
    gorget_reload_fn_t __reload_fn = (gorget_reload_fn_t)__guest.reload;
    bool __running = true;
    while (__running) {{
        if (gorget_hot_watch_check(&__watcher)) {{
            if (system("{recompile_cmd}") == 0) {{
                GorgetGuestModule __new = gorget_hot_load("./{guest_lib_name}" GORGET_DYLIB_EXT);
                if (__new.handle) {{
                    gorget_hot_unload(&__guest);
                    __guest = __new;
                    __tick_fn = (gorget_tick_fn_t)__guest.tick;
                    __reload_fn = (gorget_reload_fn_t)__guest.reload;
                    if (__reload_fn) __reload_fn(&__state);
                }}
            }}
        }}
        __running = __tick_fn(&__state);
    }}
    gorget_hot_unload(&__guest);
    return 0;
}}
"#
    ));

    (host, guest)
}

/// Emit a test runner main() with timing, @should_panic support, and suite setup/teardown.
fn emit_test_runner_main(out: &mut String, module: &Module) {
    let test_fns = &module.runtime.test_fns;
    let tracing = module.runtime.trace_filename.is_some();
    let has_any_timeout = test_fns.iter().any(|t| t.timeout_ms.is_some());
    let _ = writeln!(out, "int main(int argc, char** argv) {{");
    let _ = writeln!(out, "    gorget_init_args(argc, argv);");
    if let Some(ref trace_path) = module.runtime.trace_filename {
        let escaped = trace_path.replace('\\', "\\\\").replace('"', "\\\"");
        let _ = writeln!(out, "    __gorget_trace_init(\"{escaped}\");");
    }
    let _ = writeln!(out, "    int __test_passed = 0, __test_failed = 0, __test_skipped = 0;");
    let _ = writeln!(out, "    struct timespec __total_start, __total_end;");
    let _ = writeln!(out, "    clock_gettime(CLOCK_MONOTONIC, &__total_start);");

    // Parallel support: GORGET_PARALLEL_ID and GORGET_PARALLEL_TOTAL env vars
    let _ = writeln!(out, "    int __par_id = -1, __par_total = 0;");
    let _ = writeln!(out, "    const char* __par_id_env = getenv(\"GORGET_PARALLEL_ID\");");
    let _ = writeln!(out, "    const char* __par_total_env = getenv(\"GORGET_PARALLEL_TOTAL\");");
    let _ = writeln!(out, "    if (__par_id_env && __par_total_env) {{ __par_id = atoi(__par_id_env); __par_total = atoi(__par_total_env); }}");

    // Result file support: GORGET_TEST_RESULTS env var
    let _ = writeln!(out, "    const char* __results_path = getenv(\"GORGET_TEST_RESULTS\");");

    // Snapshot capture: open file if GORGET_SNAPSHOT_PATH is set
    let _ = writeln!(out, "    __gorget_snapshot_open();");

    // Count non-skipped, non-parallel-filtered tests for header
    let _ = writeln!(out, "    int __test_total = {};", test_fns.len());
    let _ = writeln!(out, "    if (__par_total > 0) {{");
    let _ = writeln!(out, "        __test_total = 0;");
    let _ = writeln!(out, "        for (int __i = 0; __i < {}; __i++) if (__i % __par_total == __par_id) __test_total++;", test_fns.len());
    let _ = writeln!(out, "    }}");
    let _ = writeln!(out, "    printf(\"Running %d tests...\\n\", __test_total);");

    if module.runtime.has_suite_setup {
        let _ = writeln!(out, "    __suite_setup();");
    }

    // Track results for result file: 0=skip, 1=pass, 2=fail
    let _ = writeln!(out, "    int __results[{}];", test_fns.len());
    let _ = writeln!(out, "    memset(__results, 0, sizeof(__results));");

    for (idx, info) in test_fns.iter().enumerate() {
        let escaped = info.display_name.replace('\\', "\\\\").replace('"', "\\\"");

        // Parallel: skip tests not assigned to this worker
        let _ = writeln!(out, "    if (__par_total > 0 && ({idx} % __par_total != __par_id)) goto __test_done_{idx};");

        // Skipped tests: report and continue without executing
        if info.skipped {
            let _ = writeln!(out, "    printf(\"  test: {escaped} ... \");");
            if let Some(ref reason) = info.skip_reason {
                let escaped_reason = reason.replace('\\', "\\\\").replace('"', "\\\"");
                let _ = writeln!(out, "    printf(\"SKIP ({escaped_reason})\\n\");");
            } else {
                let _ = writeln!(out, "    printf(\"SKIP\\n\");");
            }
            let _ = writeln!(out, "    __test_skipped++;");
            if tracing {
                let _ = writeln!(out, "    if (__gorget_trace_fp) fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"test_end\\\",\\\"name\\\":\\\"{escaped}\\\",\\\"status\\\":\\\"skip\\\"}}\\n\");");
            }
            let _ = writeln!(out, "    goto __test_done_{idx};");
        }

        if !info.skipped {
            let fn_name = &info.fn_name;
            let _ = writeln!(out, "    printf(\"  test: {escaped} ... \");");
            let _ = writeln!(out, "    fflush(stdout);");
            if tracing {
                let _ = writeln!(out, "    if (__gorget_trace_fp) fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"test_start\\\",\\\"name\\\":\\\"{escaped}\\\"}}\\n\");");
            }
            let _ = writeln!(out, "    {{");
            let _ = writeln!(out, "        __gorget_in_test = 1;");
            let _ = writeln!(out, "        __gorget_test_fail_msg = NULL;");
            let _ = writeln!(out, "        __gorget_test_timed_out = 0;");
            let _ = writeln!(out, "        __gorget_current_test = \"{escaped}\";");
            let _ = writeln!(out, "        int __cleanup_mark = __gorget_cleanup_top;");
            let _ = writeln!(out, "        struct timespec __t_start, __t_end;");
            let _ = writeln!(out, "        clock_gettime(CLOCK_MONOTONIC, &__t_start);");

            let _ = writeln!(out, "        __gorget_test_cleanup_mark = __cleanup_mark;");

            // Set timeout if configured
            if let Some(ms) = info.timeout_ms {
                let _ = writeln!(out, "        __gorget_set_timeout({ms}L);");
            }

            let _ = writeln!(out, "        int __jmp_val = setjmp(__gorget_test_jmp);");
            let _ = writeln!(out, "        if (__jmp_val == 0) {{");
            let _ = writeln!(out, "            {fn_name}();");
            let _ = writeln!(out, "            __gorget_cleanup_top = __cleanup_mark;");
            let _ = writeln!(out, "        }}");

            // Cancel timeout
            if info.timeout_ms.is_some() {
                let _ = writeln!(out, "        __gorget_cancel_timeout();");
            }

            // On timeout (jmp_val==2): cleanup was NOT run by signal handler, run it now
            // On panic (jmp_val==1): gorget_panic already ran cleanup, this is a no-op
            let _ = writeln!(out, "        __gorget_cleanup_run(__cleanup_mark);");
            let _ = writeln!(out, "        __gorget_in_test = 0;");

            let _ = writeln!(out, "        clock_gettime(CLOCK_MONOTONIC, &__t_end);");
            let _ = writeln!(out, "        long __t_ms = (__t_end.tv_sec - __t_start.tv_sec) * 1000 + (__t_end.tv_nsec - __t_start.tv_nsec) / 1000000;");
            if tracing {
                let _ = writeln!(out, "        int __test_trace_ok = 0;");
            }

            // Timeout always fails, regardless of @should_panic
            if has_any_timeout {
                let _ = writeln!(out, "        if (__gorget_test_timed_out) {{");
                if let Some(ms) = info.timeout_ms {
                    let _ = writeln!(out, "            __test_failed++;");
                    let _ = writeln!(out, "            __results[{idx}] = 2;");
                    let _ = writeln!(out, "            printf(\"FAIL: timed out after {ms}ms (%ldms)\\n\", __t_ms);");
                } else {
                    // No timeout on this test but another test has one — unreachable but handle gracefully
                    let _ = writeln!(out, "            __test_failed++;");
                    let _ = writeln!(out, "            __results[{idx}] = 2;");
                    let _ = writeln!(out, "            printf(\"FAIL: timed out (%ldms)\\n\", __t_ms);");
                }
                let _ = writeln!(out, "        }} else");
            }

            if info.should_panic {
                if let Some(ref msg) = info.expected_panic_msg {
                    let escaped_msg = msg.replace('\\', "\\\\").replace('"', "\\\"");
                    let _ = writeln!(out, "        if (__gorget_test_fail_msg && strstr(__gorget_test_fail_msg, \"{escaped_msg}\")) {{");
                    let _ = writeln!(out, "            __test_passed++; __results[{idx}] = 1;");
                    if tracing { let _ = writeln!(out, "            __test_trace_ok = 1;"); }
                    let _ = writeln!(out, "            printf(\"PASS (%ldms)\\n\", __t_ms);");
                    let _ = writeln!(out, "        }} else if (__gorget_test_fail_msg) {{");
                    let _ = writeln!(out, "            __test_failed++; __results[{idx}] = 2;");
                    let _ = writeln!(out, "            printf(\"FAIL: expected panic containing \\\"{escaped_msg}\\\", got: %s (%ldms)\\n\", __gorget_test_fail_msg, __t_ms);");
                    let _ = writeln!(out, "        }} else {{");
                    let _ = writeln!(out, "            __test_failed++; __results[{idx}] = 2;");
                    let _ = writeln!(out, "            printf(\"FAIL: expected panic but test passed (%ldms)\\n\", __t_ms);");
                    let _ = writeln!(out, "        }}");
                } else {
                    let _ = writeln!(out, "        if (__gorget_test_fail_msg) {{");
                    let _ = writeln!(out, "            __test_passed++; __results[{idx}] = 1;");
                    if tracing { let _ = writeln!(out, "            __test_trace_ok = 1;"); }
                    let _ = writeln!(out, "            printf(\"PASS (%ldms)\\n\", __t_ms);");
                    let _ = writeln!(out, "        }} else {{");
                    let _ = writeln!(out, "            __test_failed++; __results[{idx}] = 2;");
                    let _ = writeln!(out, "            printf(\"FAIL: expected panic but test passed (%ldms)\\n\", __t_ms);");
                    let _ = writeln!(out, "        }}");
                }
            } else {
                let _ = writeln!(out, "        if (!__gorget_test_fail_msg) {{");
                let _ = writeln!(out, "            __test_passed++; __results[{idx}] = 1;");
                if tracing { let _ = writeln!(out, "            __test_trace_ok = 1;"); }
                let _ = writeln!(out, "            printf(\"PASS (%ldms)\\n\", __t_ms);");
                let _ = writeln!(out, "        }} else {{");
                let _ = writeln!(out, "            __test_failed++; __results[{idx}] = 2;");
                let _ = writeln!(out, "            printf(\"FAIL: %s (%ldms)\\n\", __gorget_test_fail_msg, __t_ms);");
                let _ = writeln!(out, "        }}");
            }
            if tracing {
                let _ = writeln!(out, "        if (__gorget_trace_fp) fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"test_end\\\",\\\"name\\\":\\\"{escaped}\\\",\\\"status\\\":\\\"%s\\\",\\\"duration_ms\\\":%ld}}\\n\", __test_trace_ok ? \"pass\" : \"fail\", __t_ms);");
            }
            let _ = writeln!(out, "    }}");
        }

        let _ = writeln!(out, "    __test_done_{idx}:;");
    }

    if module.runtime.has_suite_teardown {
        let _ = writeln!(out, "    __suite_teardown();");
    }

    let _ = writeln!(out, "    clock_gettime(CLOCK_MONOTONIC, &__total_end);");
    let _ = writeln!(out, "    long __total_ms = (__total_end.tv_sec - __total_start.tv_sec) * 1000 + (__total_end.tv_nsec - __total_start.tv_nsec) / 1000000;");
    let _ = writeln!(out, "    if (__test_skipped > 0) printf(\"\\n%d passed, %d failed, %d skipped (%ldms)\\n\", __test_passed, __test_failed, __test_skipped, __total_ms);");
    let _ = writeln!(out, "    else printf(\"\\n%d passed, %d failed (%ldms)\\n\", __test_passed, __test_failed, __total_ms);");

    // Write results file if GORGET_TEST_RESULTS is set
    let _ = writeln!(out, "    if (__results_path) {{");
    let _ = writeln!(out, "        FILE* __rf = fopen(__results_path, \"w\");");
    let _ = writeln!(out, "        if (__rf) {{");
    let _ = writeln!(out, "            fprintf(__rf, \"{{\\\"results\\\":[\\n\");");
    for (idx, info) in test_fns.iter().enumerate() {
        let escaped = info.display_name.replace('\\', "\\\\").replace('"', "\\\"");
        let comma = if idx + 1 < test_fns.len() { "," } else { "" };
        let _ = writeln!(out, "            fprintf(__rf, \"  {{\\\"name\\\":\\\"{escaped}\\\",\\\"status\\\":\\\"%s\\\"}}{comma}\\n\", __results[{idx}] == 1 ? \"pass\" : __results[{idx}] == 2 ? \"fail\" : \"skip\");");
    }
    let _ = writeln!(out, "            fprintf(__rf, \"]}}\\n\");");
    let _ = writeln!(out, "            fclose(__rf);");
    let _ = writeln!(out, "        }}");
    let _ = writeln!(out, "    }}");

    // Close snapshot file if open
    let _ = writeln!(out, "    __gorget_snapshot_close();");

    let _ = writeln!(out, "    return __test_failed > 0 ? 1 : 0;");
    let _ = writeln!(out, "}}");
}

/// Emit a benchmark runner main() with warmup, auto-calibrated iterations, and statistics.
fn emit_bench_runner_main(out: &mut String, module: &Module) {
    let bench_fns = &module.runtime.bench_fns;
    let _ = writeln!(out, "int main(int argc, char** argv) {{");
    let _ = writeln!(out, "    gorget_init_args(argc, argv);");
    let _ = writeln!(out, "    printf(\"Running {} benchmarks...\\n\\n\");", bench_fns.len());

    if module.runtime.has_suite_setup {
        let _ = writeln!(out, "    __suite_setup();");
    }

    for info in bench_fns {
        let escaped = info.display_name.replace('\\', "\\\\").replace('"', "\\\"");
        let fn_name = &info.fn_name;

        let _ = writeln!(out, "    {{");
        let _ = writeln!(out, "        // Bench: {escaped}");
        let _ = writeln!(out, "        __gorget_in_test = 1;");
        let _ = writeln!(out, "        __gorget_test_fail_msg = NULL;");
        let _ = writeln!(out, "        int __cleanup_mark = __gorget_cleanup_top;");

        // Warmup: 3 iterations
        let _ = writeln!(out, "        for (int __w = 0; __w < 3; __w++) {{");
        let _ = writeln!(out, "            __gorget_test_cleanup_mark = __gorget_cleanup_top;");
        let _ = writeln!(out, "            if (setjmp(__gorget_test_jmp) == 0) {{");
        let _ = writeln!(out, "                {fn_name}();");
        let _ = writeln!(out, "                __gorget_cleanup_top = __cleanup_mark;");
        let _ = writeln!(out, "            }} else {{");
        let _ = writeln!(out, "                __gorget_cleanup_run(__cleanup_mark);");
        let _ = writeln!(out, "                printf(\"  bench: {escaped} ... FAIL (panic during warmup)\\n\");");
        let _ = writeln!(out, "                __gorget_in_test = 0;");
        let _ = writeln!(out, "                goto __bench_next_{};", info.fn_name);
        let _ = writeln!(out, "            }}");
        let _ = writeln!(out, "        }}");

        // Auto-calibrate: start with 100 iterations, double until >= 1 second
        let _ = writeln!(out, "        long __iters = 100;");
        let _ = writeln!(out, "        struct timespec __cal_start, __cal_end;");
        let _ = writeln!(out, "        for (;;) {{");
        let _ = writeln!(out, "            clock_gettime(CLOCK_MONOTONIC, &__cal_start);");
        let _ = writeln!(out, "            for (long __i = 0; __i < __iters; __i++) {{");
        let _ = writeln!(out, "                __gorget_test_cleanup_mark = __gorget_cleanup_top;");
        let _ = writeln!(out, "                if (setjmp(__gorget_test_jmp) == 0) {{");
        let _ = writeln!(out, "                    {fn_name}();");
        let _ = writeln!(out, "                    __gorget_cleanup_top = __cleanup_mark;");
        let _ = writeln!(out, "                }} else {{");
        let _ = writeln!(out, "                    __gorget_cleanup_run(__cleanup_mark);");
        let _ = writeln!(out, "                    printf(\"  bench: {escaped} ... FAIL (panic during measurement)\\n\");");
        let _ = writeln!(out, "                    __gorget_in_test = 0;");
        let _ = writeln!(out, "                    goto __bench_next_{};", info.fn_name);
        let _ = writeln!(out, "                }}");
        let _ = writeln!(out, "            }}");
        let _ = writeln!(out, "            clock_gettime(CLOCK_MONOTONIC, &__cal_end);");
        let _ = writeln!(out, "            long __cal_ns = (__cal_end.tv_sec - __cal_start.tv_sec) * 1000000000L + (__cal_end.tv_nsec - __cal_start.tv_nsec);");
        let _ = writeln!(out, "            if (__cal_ns >= 1000000000L) break;"); // >= 1 second
        let _ = writeln!(out, "            if (__cal_ns < 10000000L) __iters *= 100;"); // < 10ms, scale up fast
        let _ = writeln!(out, "            else __iters *= 2;");
        let _ = writeln!(out, "        }}");

        // Compute stats from the calibration run
        let _ = writeln!(out, "        long __total_ns = (__cal_end.tv_sec - __cal_start.tv_sec) * 1000000000L + (__cal_end.tv_nsec - __cal_start.tv_nsec);");
        let _ = writeln!(out, "        double __avg_ns = (double)__total_ns / (double)__iters;");

        // Format output
        let _ = writeln!(out, "        if (__avg_ns < 1000.0) {{");
        let _ = writeln!(out, "            printf(\"  bench: {escaped} ... %ld iters, %.0f ns/iter\\n\", __iters, __avg_ns);");
        let _ = writeln!(out, "        }} else if (__avg_ns < 1000000.0) {{");
        let _ = writeln!(out, "            printf(\"  bench: {escaped} ... %ld iters, %.2f us/iter\\n\", __iters, __avg_ns / 1000.0);");
        let _ = writeln!(out, "        }} else if (__avg_ns < 1000000000.0) {{");
        let _ = writeln!(out, "            printf(\"  bench: {escaped} ... %ld iters, %.2f ms/iter\\n\", __iters, __avg_ns / 1000000.0);");
        let _ = writeln!(out, "        }} else {{");
        let _ = writeln!(out, "            printf(\"  bench: {escaped} ... %ld iters, %.2f s/iter\\n\", __iters, __avg_ns / 1000000000.0);");
        let _ = writeln!(out, "        }}");

        let _ = writeln!(out, "        __gorget_in_test = 0;");
        let _ = writeln!(out, "        __bench_next_{}:;", info.fn_name);
        let _ = writeln!(out, "    }}");
    }

    if module.runtime.has_suite_teardown {
        let _ = writeln!(out, "    __suite_teardown();");
    }

    let _ = writeln!(out, "    printf(\"\\n{} benchmarks complete.\\n\");", bench_fns.len());
    let _ = writeln!(out, "    return 0;");
    let _ = writeln!(out, "}}");
}

/// Collect the names of TypeDefs that are unmonomorphized generic templates.
///
/// Two detection strategies:
/// 1. Naming convention: type name ends with `__X` where X is a single uppercase
///    ASCII letter (e.g., `Heap__T`, `Option__T`, `SparseSet__V`). These are
///    uninstantiated generic types whose type parameter was never resolved to a
///    concrete type.
/// 2. Structural: the type has at least one field whose type resolves to `void`
///    (UNIT_TYPE), indicating an unresolved type parameter that the IR erased.
fn template_type_names(module: &Module) -> Vec<String> {
    let mut names = Vec::new();
    for def in module.type_registry.type_defs() {
        // Detect by naming convention: Foo__T (single uppercase letter suffix)
        let b = def.name.as_bytes();
        let n = b.len();
        let is_unbound_by_name = n >= 3
            && b[n - 1].is_ascii_uppercase()
            && b[n - 2] == b'_'
            && b[n - 3] == b'_';
        if is_unbound_by_name {
            names.push(def.name.clone());
            continue;
        }
        // Detect by structure: field or variant field with UNIT_TYPE.
        // Skip Result__ types — they're always fully monomorphized and may
        // legitimately contain void (e.g., Result[void, int] from throws-int main).
        if !def.name.starts_with("Result__") {
            match &def.kind {
                TypeDefKind::Struct(s) => {
                    if s.fields.iter().any(|f| f.type_id == UNIT_TYPE) {
                        names.push(def.name.clone());
                    }
                }
                TypeDefKind::Enum(e) => {
                    if e.variants.iter().any(|v| v.fields.iter().any(|f| f.type_id == UNIT_TYPE)) {
                        names.push(def.name.clone());
                    }
                }
                TypeDefKind::Alias(_) => {}
            }
        }
    }
    names
}

/// Check if a function name belongs to a template type (e.g., `Container__T__get`).
fn is_template_function(func_name: &str, template_names: &[String]) -> bool {
    template_names.iter().any(|t| func_name.starts_with(&format!("{t}__")))
}

/// Return TypeDef indices in topological order for struct body emission.
///
/// Types must be emitted after all other types they embed by VALUE (not by pointer).
/// Pointer-based references only need forward declarations, which are emitted earlier.
/// This avoids "field has incomplete type" errors in C.
fn topo_sorted_body_order(
    type_defs: &[TypeDef],
    registry: &TypeRegistry,
    should_skip: &impl Fn(&str) -> bool,
) -> Vec<usize> {
    let n = type_defs.len();

    // Build a name → index map for fast lookup
    let name_to_idx: std::collections::HashMap<&str, usize> = type_defs.iter()
        .enumerate()
        .map(|(i, d)| (d.name.as_str(), i))
        .collect();

    // For each type, collect the indices of its value-type (non-pointer) dependencies
    let get_value_deps = |idx: usize| -> Vec<usize> {
        let def = &type_defs[idx];
        if should_skip(&def.name) || def.name.starts_with("Box__") {
            return vec![];
        }
        let field_types: &[StructField] = match &def.kind {
            TypeDefKind::Struct(s) => &s.fields,
            TypeDefKind::Enum(e) => {
                // Flatten variant fields into a single slice isn't trivial; collect instead
                return e.variants.iter()
                    .flat_map(|v| v.fields.iter())
                    .filter_map(|f| {
                        if let Some(GirType::Named(name)) = registry.get(f.type_id) {
                            if runtime_type_name(name).is_none() && !should_skip(name)
                                && !name.starts_with("Box__")
                            {
                                return name_to_idx.get(name.as_str()).copied();
                            }
                        }
                        None
                    })
                    .collect();
            }
            TypeDefKind::Alias(_) => return vec![],
        };
        field_types.iter()
            .filter_map(|f| {
                if let Some(GirType::Named(name)) = registry.get(f.type_id) {
                    if runtime_type_name(name).is_none() && !should_skip(name)
                        && !name.starts_with("Box__")
                    {
                        return name_to_idx.get(name.as_str()).copied();
                    }
                }
                None
            })
            .collect()
    };

    // Kahn-like multi-pass: repeatedly emit types whose value deps are already satisfied.
    // There are no true value-type cycles (that would create infinite-size types),
    // so this always terminates.
    let mut emitted = vec![false; n];
    let mut order: Vec<usize> = Vec::with_capacity(n);

    let mut made_progress = true;
    while order.len() < n && made_progress {
        made_progress = false;
        for i in 0..n {
            if emitted[i] {
                continue;
            }
            // Skipped and Box types have no ordering constraints — treat as already satisfied
            let def = &type_defs[i];
            if should_skip(&def.name) || def.name.starts_with("Box__") {
                emitted[i] = true;
                made_progress = true;
                // Don't add to `order` — these aren't emitted in the body loop
                continue;
            }
            let deps = get_value_deps(i);
            if deps.iter().all(|&d| emitted[d]) {
                emitted[i] = true;
                order.push(i);
                made_progress = true;
            }
        }
    }
    // If any remain, there is a dependency cycle — emit them in original order with a warning.
    let remaining: Vec<usize> = (0..n).filter(|i| !emitted[*i]).collect();
    if !remaining.is_empty() {
        debug_assert!(false, "BUG: topological sort found cycle among {} type definitions", remaining.len());
        for i in remaining {
            order.push(i);
        }
    }
    order
}

/// Emit Shared__T and Weak__T typedefs BEFORE type definitions.
/// These typedefs are needed early so Option__Shared__T structs can reference Shared__T.
fn emit_shared_weak_typedefs(out: &mut String, module: &Module) {
    if module.runtime.shared_types.is_empty() && module.runtime.weak_types.is_empty() {
        return;
    }
    out.push_str("\n/* ── Shared[T] / Weak[T] forward typedefs ── */\n");
    // Collect all element types that need a Shared__ typedef
    for elem_c in &module.runtime.shared_types {
        let _ = writeln!(out, "typedef GorgetShared* Shared__{elem_c};");
    }
    // Collect all element types that need a Weak__ typedef
    // (either from explicit Weak vars OR as companion for downgrade() return type)
    let mut weak_emitted: std::collections::HashSet<&str> = std::collections::HashSet::new();
    for elem_c in &module.runtime.shared_types {
        // Always emit Weak__T companion for every Shared__T (needed for downgrade() return type)
        if weak_emitted.insert(elem_c.as_str()) {
            let _ = writeln!(out, "typedef GorgetShared* Weak__{elem_c};");
        }
    }
    for elem_c in &module.runtime.weak_types {
        if weak_emitted.insert(elem_c.as_str()) {
            let _ = writeln!(out, "typedef GorgetShared* Weak__{elem_c};");
        }
    }
}

/// Emit Shared__T wrapper functions (new, clone, drop, get, strong_count, downgrade).
/// Typedefs were already emitted by emit_shared_weak_typedefs.
fn emit_shared_defs(out: &mut String, module: &Module) {
    if module.runtime.shared_types.is_empty() {
        return;
    }
    out.push_str("\n/* ── Shared[T] wrappers ── */\n");
    for elem_c in &module.runtime.shared_types {
        let shared_name = format!("Shared__{elem_c}");
        // Typedef already emitted by emit_shared_weak_typedefs — skip here.
        // Constructor: Shared__T__new(val) → allocates control block, copies val in
        let _ = writeln!(out,
            "static inline {shared_name} {shared_name}__new({elem_c} val) {{ \
             return gorget_shared_new(sizeof({elem_c}), &val); }}");
        // clone() → atomic increment of strong count, returns same pointer
        let _ = writeln!(out,
            "static inline {shared_name} {shared_name}__clone({shared_name} self) {{ \
             return gorget_shared_clone(self); }}");
        // drop() → atomic decrement; frees data+control block at zero (called by RAII Drop)
        let _ = writeln!(out,
            "static inline void {shared_name}__drop({shared_name}* self) {{ \
             gorget_shared_drop(*self); }}");
        // get() → dereferences inner value (returns copy)
        let _ = writeln!(out,
            "static inline {elem_c} {shared_name}__get({shared_name} self) {{ \
             return *({elem_c}*)gorget_shared_get_ptr(self); }}");
        // strong_count() → current number of strong refs (for debugging/testing)
        let _ = writeln!(out,
            "static inline int64_t {shared_name}__strong_count({shared_name} self) {{ \
             return gorget_shared_strong_count(self); }}");
        // Weak__T typedef was already emitted by emit_shared_weak_typedefs.
        let weak_name = format!("Weak__{elem_c}");
        // downgrade() → Weak[T] (atomic increment of weak count, returns same control block)
        let _ = writeln!(out,
            "static inline {weak_name} {shared_name}__downgrade({shared_name} self) {{ \
             return gorget_shared_downgrade(self); }}");
        // For Shared[Vector[T]]: emit element-access helpers that avoid the copy-UAF.
        // at(i) reads element i directly from the inner GorgetArray.
        // set_at(i, val) writes element i directly into the inner GorgetArray.
        // slen() returns the length of the inner GorgetArray without copying.
        if let Some(inner_elem) = elem_c.strip_prefix("Vector__") {
            let _ = writeln!(out,
                "static inline {inner_elem} {shared_name}__at({shared_name} self, int64_t i) {{ \
                 return *({inner_elem}*)gorget_shared_array_get(self, (size_t)i); }}");
            let _ = writeln!(out,
                "static inline void {shared_name}__set_at({shared_name} self, int64_t i, {inner_elem} val) {{ \
                 gorget_shared_array_set(self, (size_t)i, &val, sizeof({inner_elem})); }}");
            let _ = writeln!(out,
                "static inline int64_t {shared_name}__slen({shared_name} self) {{ \
                 return gorget_shared_array_len(self); }}");
        }
        out.push('\n');
    }
}

/// Emit Weak__T wrapper functions (clone, drop, upgrade).
/// Typedefs were already emitted by emit_shared_weak_typedefs.
fn emit_weak_defs(out: &mut String, module: &Module) {
    if module.runtime.weak_types.is_empty() {
        return;
    }
    out.push_str("\n/* ── Weak[T] wrappers ── */\n");
    for elem_c in &module.runtime.weak_types {
        let weak_name = format!("Weak__{elem_c}");
        let shared_name = format!("Shared__{elem_c}");
        // Typedef already emitted by emit_shared_weak_typedefs — skip here.
        // clone() → increment weak count
        let _ = writeln!(out,
            "static inline {weak_name} {weak_name}__clone({weak_name} self) {{ \
             return gorget_weak_clone(self); }}");
        // drop() → decrement weak count; frees control block when both counts hit 0
        let _ = writeln!(out,
            "static inline void {weak_name}__drop({weak_name}* self) {{ \
             gorget_weak_drop(*self); }}");
        // upgrade() → Option[Shared[T]]: CAS strong N→N+1; returns Some(ptr) or None
        // Option[Shared[T]] uses tag=0 for Some (first variant), tag=1 for None (second variant).
        // Member access follows the union layout: data.Some._0
        let option_name = format!("Option__{shared_name}");
        let _ = writeln!(out,
            "static inline {option_name} {weak_name}__upgrade({weak_name} self) {{ \
             {option_name} __opt; \
             if (gorget_weak_upgrade(self)) {{ \
                 __opt.tag = 0; __opt.data.Some._0 = ({shared_name})self; \
             }} else {{ \
                 __opt.tag = 1; \
             }} \
             return __opt; }}");
        out.push('\n');
    }
}

/// Emit Mutex__T and Guard__T typedef + wrapper functions.
fn emit_mutex_defs(out: &mut String, module: &Module) {
    if module.runtime.mutex_types.is_empty() {
        return;
    }
    out.push_str("\n/* ── Mutex[T] + Guard[T] wrappers ── */\n");
    for elem_c in &module.runtime.mutex_types {
        let mutex_name = format!("Mutex__{elem_c}");
        let guard_name = format!("Guard__{elem_c}");
        // Mutex__T typedef (Copy pointer)
        let _ = writeln!(out, "typedef GorgetMutex* {mutex_name};");
        // Guard__T typedef (Move value struct holding mutex ptr + data ptr)
        let _ = writeln!(out, "typedef gorget_guard_t {guard_name};");
        // Mutex__T__new(val) → allocates mutex, copies initial value in
        let _ = writeln!(out,
            "static inline {mutex_name} {mutex_name}__new({elem_c} val) {{ \
             return gorget_mutex_new(sizeof({elem_c}), &val); }}");
        // Mutex__T__lock(self) → acquires mutex, returns Guard__T (blocking)
        let _ = writeln!(out,
            "static inline {guard_name} {mutex_name}__lock({mutex_name} self) {{ \
             return gorget_mutex_lock(self); }}");
        // Guard__T__get(&self) → returns copy of inner value
        let _ = writeln!(out,
            "static inline {elem_c} {guard_name}__get({guard_name}* self) {{ \
             return *({elem_c}*)self->ptr; }}");
        // Guard__T__set(&self, val) → writes new value
        let _ = writeln!(out,
            "static inline void {guard_name}__set({guard_name}* self, {elem_c} val) {{ \
             *({elem_c}*)self->ptr = val; }}");
        // Guard__T__get_ptr(&self) → returns mutable pointer to inner value (for token wrappers)
        let _ = writeln!(out,
            "static inline {elem_c}* {guard_name}__get_ptr({guard_name}* self) {{ \
             return ({elem_c}*)self->ptr; }}");
        // Guard__T__drop(&self) → releases the mutex (called by RAII drop)
        let _ = writeln!(out,
            "static inline void {guard_name}__drop({guard_name}* self) {{ \
             gorget_guard_release(self); }}");
        out.push('\n');
    }
}

/// Emit RWLock__T + ReadGuard__T + WriteGuard__T typedef + wrapper functions.
fn emit_rwlock_defs(out: &mut String, module: &Module) {
    if module.runtime.rwlock_types.is_empty() {
        return;
    }
    out.push_str("\n/* ── RWLock[T] + ReadGuard[T] + WriteGuard[T] wrappers ── */\n");
    for elem_c in &module.runtime.rwlock_types {
        let rwlock_name = format!("RWLock__{elem_c}");
        let read_guard  = format!("ReadGuard__{elem_c}");
        let write_guard = format!("WriteGuard__{elem_c}");
        // Typedefs
        let _ = writeln!(out, "typedef GorgetRWLock* {rwlock_name};");
        let _ = writeln!(out, "typedef gorget_read_guard_t {read_guard};");
        let _ = writeln!(out, "typedef gorget_write_guard_t {write_guard};");
        // Constructor: RWLock__T__new(val)
        let _ = writeln!(out,
            "static inline {rwlock_name} {rwlock_name}__new({elem_c} val) {{ \
             return gorget_rwlock_new(sizeof({elem_c}), &val); }}");
        // read() -> ReadGuard__T
        let _ = writeln!(out,
            "static inline {read_guard} {rwlock_name}__read({rwlock_name} self) {{ \
             return gorget_rwlock_read(self); }}");
        // write() -> WriteGuard__T
        let _ = writeln!(out,
            "static inline {write_guard} {rwlock_name}__write({rwlock_name} self) {{ \
             return gorget_rwlock_write(self); }}");
        // ReadGuard__T__get(&self) -> T
        let _ = writeln!(out,
            "static inline {elem_c} {read_guard}__get({read_guard}* self) {{ \
             return *({elem_c}*)self->ptr; }}");
        // ReadGuard__T__get_ptr(&self) → returns const pointer to inner value (for auto-deref)
        let _ = writeln!(out,
            "static inline const {elem_c}* {read_guard}__get_ptr({read_guard}* self) {{ \
             return (const {elem_c}*)self->ptr; }}");
        // ReadGuard__T__drop(&self)
        let _ = writeln!(out,
            "static inline void {read_guard}__drop({read_guard}* self) {{ \
             gorget_read_guard_release(self); }}");
        // WriteGuard__T__get(&self) -> T
        let _ = writeln!(out,
            "static inline {elem_c} {write_guard}__get({write_guard}* self) {{ \
             return *({elem_c}*)self->ptr; }}");
        // WriteGuard__T__set(&self, val)
        let _ = writeln!(out,
            "static inline void {write_guard}__set({write_guard}* self, {elem_c} val) {{ \
             *({elem_c}*)self->ptr = val; }}");
        // WriteGuard__T__get_ptr(&self) → returns mutable pointer to inner value (for token wrappers)
        let _ = writeln!(out,
            "static inline {elem_c}* {write_guard}__get_ptr({write_guard}* self) {{ \
             return ({elem_c}*)self->ptr; }}");
        // WriteGuard__T__drop(&self)
        let _ = writeln!(out,
            "static inline void {write_guard}__drop({write_guard}* self) {{ \
             gorget_write_guard_release(self); }}");
        out.push('\n');
    }
}

/// Emit Channel__T__recv_timeout implementations (after Option__T is defined).
/// Only emits for channel types where recv_timeout is actually called.
fn emit_channel_recv_timeout_defs(out: &mut String, module: &Module) {
    // Scan GIR for Channel__T__recv_timeout calls to avoid emitting unused wrappers
    // (which would reference Option__T that may not be defined).
    let mut needed: Vec<&str> = Vec::new();
    for func in &module.functions {
        for bb in &func.blocks {
            for inst in &bb.instructions {
                if let Instruction::Call { func: fname, .. } = inst {
                    if fname.ends_with("__recv_timeout") {
                        if let Some(elem) = fname.strip_prefix("Channel__")
                            .and_then(|s| s.strip_suffix("__recv_timeout"))
                        {
                            if !needed.iter().any(|n| *n == elem) {
                                needed.push(elem);
                            }
                        }
                    }
                }
            }
        }
    }
    for elem_c in needed {
        let chan_name = format!("Channel__{elem_c}");
        let option_name = format!("Option__{elem_c}");
        let _ = writeln!(out,
            "{option_name} {chan_name}__recv_timeout({chan_name} self, int64_t ms) {{ \
             {option_name} __r; {elem_c} __val; \
             if (gorget_channel_recv_timeout(self, &__val, ms)) {{ __r.tag = 0; __r.data.Some._0 = __val; }} \
             else {{ __r.tag = 1; }} return __r; }}");
    }
}

/// Emit Thread__T typedef + join/id methods.
/// The internal `__GorgetThread__T` context struct is also emitted here.
/// Per-function spawn helpers (which reference GIR functions) are in emit_thread_helpers.
fn emit_thread_defs(out: &mut String, module: &Module) {
    if module.runtime.thread_types.is_empty() {
        return;
    }
    out.push_str("\n/* ── Thread[T] wrappers ── */\n");
    for elem_c in &module.runtime.thread_types {
        let thread_name = format!("Thread__{elem_c}");
        let is_void = elem_c == "void";
        // Internal context struct — use _thr/_result (not __thread: reserved GCC keyword)
        // For void return: no _result field.
        if is_void {
            let _ = writeln!(out,
                "typedef struct {{ pthread_t _thr; }} __GorgetThread__{elem_c};");
        } else {
            let _ = writeln!(out,
                "typedef struct {{ pthread_t _thr; {elem_c} _result; }} __GorgetThread__{elem_c};");
        }
        // Thread__T is a pointer to the context struct (Move; join() frees it)
        let _ = writeln!(out, "typedef __GorgetThread__{elem_c}* {thread_name};");
        // id(self) -> int64_t: return the pthread_t cast to int (takes by value like join)
        let _ = writeln!(out,
            "static inline int64_t {thread_name}__id({thread_name} self) {{ \
             return (int64_t)(uintptr_t)self->_thr; }}");
        // join(self) -> T: block until complete, extract result, free context
        if is_void {
            let _ = writeln!(out,
                "static inline void {thread_name}__join({thread_name} self) {{ \
                 pthread_join(self->_thr, NULL); \
                 GORGET_FREE(self, sizeof(*self)); }}");
        } else {
            let _ = writeln!(out,
                "static inline {elem_c} {thread_name}__join({thread_name} self) {{ \
                 pthread_join(self->_thr, NULL); \
                 {elem_c} _r = self->_result; \
                 GORGET_FREE(self, sizeof(*self)); \
                 return _r; }}");
        }
        out.push('\n');
    }
}

/// Emit per-function Thread entry and spawn helpers.
/// Called AFTER GIR function forward declarations (entry functions call GIR functions).
fn emit_thread_helpers(out: &mut String, module: &Module) {
    if module.runtime.thread_spawned_fns.is_empty() {
        return;
    }
    out.push_str("\n/* ── Thread spawn helpers ── */\n");
    for (fn_name, ret_type) in &module.runtime.thread_spawned_fns {
        let ret_c = format_type(*ret_type, &module.type_registry);
        let is_void = ret_c == "void";
        let thread_name = format!("Thread__{ret_c}");
        let ctx_type    = format!("__GorgetThread__{ret_c}");
        let mangled_fn  = mangle_name(fn_name);
        // Thread entry: calls the user function, stores result in ctx
        let _ = writeln!(out, "static void* __gorget_thread_entry_{fn_name}(void* __arg) {{");
        let _ = writeln!(out, "    {ctx_type}* __ctx = ({ctx_type}*)__arg;");
        if is_void {
            let _ = writeln!(out, "    {mangled_fn}();");
        } else {
            let _ = writeln!(out, "    __ctx->_result = {mangled_fn}();");
        }
        out.push_str("    return NULL;\n}\n");
        // Spawn function: allocate context, start thread, return Thread__T
        let _ = writeln!(out,
            "static inline {thread_name} __gorget_thread_spawn_{fn_name}(void) {{");
        let _ = writeln!(out,
            "    {ctx_type}* __ctx = ({ctx_type}*)GORGET_CALLOC(1, sizeof({ctx_type}));");
        let _ = writeln!(out,
            "    pthread_create(&__ctx->_thr, NULL, \
             __gorget_thread_entry_{fn_name}, __ctx);");
        out.push_str("    return __ctx;\n}\n");
        out.push('\n');
    }
}

/// Emit Channel__T typedef + wrapper functions and Task__T structs.
/// Called after CHANNEL_RUNTIME (which defines GorgetChannel and GorgetWaker).
fn emit_channel_and_task_defs(out: &mut String, module: &Module) {
    if module.runtime.channel_types.is_empty() && module.runtime.spawned_fns.is_empty() {
        return;
    }

    out.push_str("\n/* ── Channel wrappers ── */\n");

    // Collect all unique Task return types for Task__T structs
    let mut task_ret_c_types: Vec<String> = Vec::new();

    for elem_c in &module.runtime.channel_types {
        let chan_name = format!("Channel__{elem_c}");
        // Typedef: Channel__T = GorgetChannel* (opaque pointer wrapper)
        let _ = writeln!(out, "typedef GorgetChannel* {chan_name};");
        // Constructor: Channel__T__new(cap)
        let _ = writeln!(out,
            "static inline {chan_name} {chan_name}__new(int64_t cap) {{ \
             return gorget_channel_new((size_t)cap, sizeof({elem_c})); }}");
        // send(&self, val)
        let _ = writeln!(out,
            "static inline void {chan_name}__send({chan_name} self, {elem_c} val) {{ \
             gorget_channel_send(self, &val); }}");
        // recv(&self) → T
        let _ = writeln!(out,
            "static inline {elem_c} {chan_name}__recv({chan_name} self) {{ \
             {elem_c} __val; gorget_channel_recv(self, &__val); return __val; }}");
        // close(&self)
        let _ = writeln!(out,
            "static inline void {chan_name}__close({chan_name} self) {{ \
             gorget_channel_close(self); }}");
        // len(&self) → int
        let _ = writeln!(out,
            "static inline int64_t {chan_name}__len({chan_name} self) {{ \
             return gorget_channel_len(self); }}");
        // capacity(&self) → int
        let _ = writeln!(out,
            "static inline int64_t {chan_name}__capacity({chan_name} self) {{ \
             return gorget_channel_capacity(self); }}");
        // is_closed(&self) → bool
        let _ = writeln!(out,
            "static inline bool {chan_name}__is_closed({chan_name} self) {{ \
             return gorget_channel_is_closed(self); }}");
        // recv_timeout — defined later (after Option__T) by emit_channel_recv_timeout_defs
        // poll_send(&self, val, waker) → bool
        let _ = writeln!(out,
            "static inline bool {chan_name}__poll_send({chan_name} self, {elem_c} val, GorgetWaker* waker) {{ \
             return gorget_channel_poll_send(self, &val, waker); }}");
        // poll_recv(&self, *out, waker) → bool
        let _ = writeln!(out,
            "static inline bool {chan_name}__poll_recv({chan_name} self, {elem_c}* out, GorgetWaker* waker) {{ \
             return gorget_channel_poll_recv(self, out, waker); }}");
        // clone: retain (increment refcount)
        let _ = writeln!(out,
            "static inline {chan_name} {chan_name}__clone({chan_name} self) {{ \
             return gorget_channel_retain(self); }}");
        // drop: release (decrement refcount, auto-close+free when last ref drops)
        // Takes self by pointer to match emit_drop_code which passes &place_str.
        let _ = writeln!(out,
            "static inline void {chan_name}__drop({chan_name}* self) {{ \
             gorget_channel_release(*self); *self = NULL; }}");
        out.push('\n');
    }

    // Collect return types of spawned fns for Task__T emission
    for (_, _, ret_type) in &module.runtime.spawned_fns {
        let ret_c = format_type(*ret_type, &module.type_registry);
        let task_name = if ret_c == "void" {
            "Task__void".to_string()
        } else {
            format!("Task__{ret_c}")
        };
        if !task_ret_c_types.contains(&task_name) {
            task_ret_c_types.push(task_name);
        }
    }

    // Also scan function locals and type defs for Task__T types that may not
    // appear in spawned_fns (e.g., closure spawns, Option[Task[T]] wrapping).
    for func in &module.functions {
        for local in &func.locals {
            if let Some(crate::ir::types::GirType::Named(name)) = module.type_registry.get(local.type_id) {
                if name.starts_with("Task__") && !task_ret_c_types.contains(&name) {
                    task_ret_c_types.push(name.clone());
                }
            }
        }
    }
    for type_def in module.type_registry.type_defs() {
        if type_def.name.starts_with("Task__") && !task_ret_c_types.contains(&type_def.name) {
            task_ret_c_types.push(type_def.name.clone());
        }
    }

    if !task_ret_c_types.is_empty() {
        out.push_str("/* ── Task structs ── */\n");
        for task_name in &task_ret_c_types {
            // Task carries a void* to the __SpawnCtx and a drop function pointer
            // for RAII join-on-drop (different spawned fns have different ctx sizes).
            let _ = writeln!(out, "typedef struct {{ void* __task; void (*__drop)(void*); }} {task_name};");
        }
        out.push('\n');
    }
}

// ── Coroutine (stackless state machine) helpers ──────────────────────────────
//
// Phase 4+5 of M:N scheduling: spawned functions that internally await other
// tasks are transformed into stackless coroutines (poll functions + frame structs)
// instead of blocking worker threads with condvar_wait.  This prevents thread-pool
// deadlock when M > N tasks mutually await each other.
//
// Phase 6: sleep and blocking I/O calls are also yield points in coroutines.
// Sleep uses the reactor (gorget_reactor_sleep_async), blocking I/O is offloaded
// to the blocking thread pool (__gorget_blocking_submit).

/// Yield point classification inside a coroutine poll function.
#[derive(Debug, Clone, Copy, PartialEq)]
enum YieldKind {
    /// `__gorget_await_*` — wait for a child task to complete.
    Await,
    /// `gorget_reactor_sleep_ms` / `sleep(int)` — timer-based yield via reactor.
    Sleep,
    /// Known blocking stdlib call — offload to blocking thread pool.
    Blocking,
    /// `Mutex__*__lock` — async-aware trylock + waker yield.
    MutexLock,
    /// `RwLock__*__read` — async-aware tryrdlock + waker yield.
    RwLockRead,
    /// `RwLock__*__write` — async-aware trywrlock + waker yield.
    RwLockWrite,
    /// `Channel__*__send` — async-aware poll send + waker yield.
    ChannelSend,
    /// `Channel__*__recv` — async-aware poll recv + waker yield.
    ChannelRecv,
    /// Async socket read — explicit `async_read()` call in coroutine context.
    SocketRead,
    /// Async socket write — explicit `async_write()` call in coroutine context.
    SocketWrite,
    /// Async server socket accept — explicit `async_accept()` call in coroutine context.
    SocketAccept,
    /// Async socket connect — explicit `async_connect()` call in coroutine context.
    SocketConnect,
}

/// Known blocking function names that should be auto-offloaded in coroutines.
/// Includes both GIR names (pre-mapping) and C names (post-mapping) since
/// classify_yield runs on GIR instructions where names haven't been mapped yet.
const BLOCKING_STDLIB_CALLS: &[&str] = &[
    // File I/O (GIR names + C names)
    "read_file", "gorget_read_file",
    "write_file", "gorget_write_file",
    "append_file", "gorget_append_file",
    "gorget_file_read_all",
    "gorget_file_read_handle",
    "gorget_file_write_handle",
    "gorget_file_create",
    "gorget_file_open_read",
    "gorget_file_append",
    "readdir", "gorget_readdir",
    // Network (blocking socket ops — offloaded to blocking pool in coroutines)
    "gorget_socket_connect",
    "gorget_tls_connect",
    "gorget_socket_read_line",
    "gorget_tls_read_line",
    "gorget_socket_read_bytes",
    "gorget_tls_read_bytes",
    "gorget_server_socket_accept",
    // SQLite
    "gorget_sqlite_open",
    "gorget_sqlite_exec",
    "gorget_sqlite_query",
    // Process
    "gorget_exec", "gorget_exec_output",
    // HTTP client
    "http_get", "gorget_http_get",
    "http_post", "gorget_http_post",
    "http_put", "gorget_http_put",
    "http_delete", "gorget_http_delete",
];

/// True if a GIR call is a sleep call dispatched to the reactor.
fn is_sleep_call(func_name: &str, args: &[Operand], func: &Function) -> bool {
    if func_name == "gorget_reactor_sleep_ms" || func_name == "async_sleep" {
        return true;
    }
    if func_name == "sleep" || func_name == "gg_sleep" {
        // Only int-arg variant is reactor sleep
        if let Some(arg) = args.first() {
            return match arg {
                Operand::Constant(Constant::I64(_)) => true,
                Operand::Copy(p) | Operand::Move(p) => {
                    let t = func.locals[p.local.0 as usize].type_id;
                    t == I64_TYPE || t == I32_TYPE
                }
                _ => false,
            };
        }
    }
    false
}

/// True if a GIR call is a known blocking stdlib call.
fn is_blocking_call(func_name: &str) -> bool {
    BLOCKING_STDLIB_CALLS.contains(&func_name)
}

/// Explicit async socket read calls (Gorget `async_read` methods).
fn is_socket_read_call(func_name: &str) -> bool {
    matches!(func_name,
        "gorget_socket_async_read" | "gorget_socket_async_read_line"
        | "gorget_socket_async_read_exact"
    )
}

/// Explicit async socket write calls (Gorget `async_write` methods).
fn is_socket_write_call(func_name: &str) -> bool {
    matches!(func_name,
        "gorget_socket_async_write" | "gorget_socket_async_write_str"
    )
}

/// Explicit async accept call.
fn is_socket_accept_call(func_name: &str) -> bool {
    matches!(func_name, "gorget_socket_async_accept")
}

/// Explicit async connect call.
fn is_socket_connect_call(func_name: &str) -> bool {
    matches!(func_name, "gorget_socket_async_connect_start")
}

/// True if a GIR call is a Mutex lock call (pattern: `Mutex__*__lock`).
fn is_mutex_lock_call(func_name: &str) -> bool {
    func_name.starts_with("Mutex__") && func_name.ends_with("__lock")
}

/// True if a GIR call is a RwLock read call (pattern: `RWLock__*__read`).
fn is_rwlock_read_call(func_name: &str) -> bool {
    func_name.starts_with("RWLock__") && func_name.ends_with("__read")
}

/// True if a GIR call is a RwLock write call (pattern: `RWLock__*__write`).
fn is_rwlock_write_call(func_name: &str) -> bool {
    func_name.starts_with("RWLock__") && func_name.ends_with("__write")
}

/// True if a GIR call is a Channel send call (pattern: `Channel__*__send`).
fn is_channel_send_call(func_name: &str) -> bool {
    func_name.starts_with("Channel__") && func_name.ends_with("__send")
}

/// True if a GIR call is a Channel recv call (pattern: `Channel__*__recv`).
fn is_channel_recv_call(func_name: &str) -> bool {
    func_name.starts_with("Channel__") && func_name.ends_with("__recv")
}

/// Extract channel call info: (dst_local_id, channel_arg_str, value_arg_str_or_none, poll_fn_name).
fn extract_channel_call_info(inst: &Instruction, func: &Function, registry: &TypeRegistry, is_send: bool) -> (u32, String, Option<String>, String) {
    match inst {
        Instruction::Call { dst, func: fname, args } => {
            let d = dst.map(|id| id.0).unwrap_or(0);
            let ch = if !args.is_empty() {
                fmt_operand_poll(&args[0], func, registry)
            } else {
                "NULL".to_string()
            };
            let val = if is_send && args.len() > 1 {
                Some(fmt_operand_poll(&args[1], func, registry))
            } else {
                None
            };
            // Channel__int64_t__send → Channel__int64_t__poll_send
            let poll_fn = if is_send {
                fname.replace("__send", "__poll_send")
            } else {
                fname.replace("__recv", "__poll_recv")
            };
            (d, ch, val, poll_fn)
        }
        _ => (0, "NULL".to_string(), None, String::new()),
    }
}

/// Extract (dst_local_id, mutex_operand_str) from a Mutex lock instruction in poll context.
fn extract_lock_call_info(inst: &Instruction, func: &Function, registry: &TypeRegistry) -> (u32, String) {
    match inst {
        Instruction::Call { dst, args, .. } => {
            let d = dst.map(|id| id.0).unwrap_or(0);
            let m = if !args.is_empty() {
                fmt_operand_poll(&args[0], func, registry)
            } else {
                "NULL".to_string()
            };
            (d, m)
        }
        _ => (0, "NULL".to_string()),
    }
}



/// Extract socket call info: (dst_local_id, func_name, args_as_strings).
fn extract_socket_call_info(inst: &Instruction, func: &Function, registry: &TypeRegistry) -> (u32, String, Vec<String>) {
    match inst {
        Instruction::Call { dst, func: fname, args, .. }
        | Instruction::CallExtern { dst, func: fname, args, .. } => {
            let d = dst.map(|id| id.0).unwrap_or(0);
            let arg_strs: Vec<String> = args.iter()
                .map(|a| fmt_operand_poll(a, func, registry))
                .collect();
            (d, fname.clone(), arg_strs)
        }
        _ => (0, String::new(), vec![]),
    }
}

/// Classify an instruction as a yield point (if any).
fn classify_yield(inst: &Instruction, func: &Function) -> Option<YieldKind> {
    match inst {
        Instruction::Call { func: fname, args, .. } => {
            if fname.starts_with("__gorget_await_") {
                Some(YieldKind::Await)
            } else if is_sleep_call(fname, args, func) {
                Some(YieldKind::Sleep)
            } else if is_mutex_lock_call(fname) {
                Some(YieldKind::MutexLock)
            } else if is_rwlock_read_call(fname) {
                Some(YieldKind::RwLockRead)
            } else if is_rwlock_write_call(fname) {
                Some(YieldKind::RwLockWrite)
            } else if is_channel_send_call(fname) {
                Some(YieldKind::ChannelSend)
            } else if is_channel_recv_call(fname) {
                Some(YieldKind::ChannelRecv)
            } else if is_socket_read_call(fname) {
                Some(YieldKind::SocketRead)
            } else if is_socket_write_call(fname) {
                Some(YieldKind::SocketWrite)
            } else if is_socket_accept_call(fname) {
                Some(YieldKind::SocketAccept)
            } else if is_socket_connect_call(fname) {
                Some(YieldKind::SocketConnect)
            } else if is_blocking_call(fname) {
                Some(YieldKind::Blocking)
            } else {
                None
            }
        }
        Instruction::CallExtern { func: fname, args, .. } => {
            if is_sleep_call(fname, args, func) {
                Some(YieldKind::Sleep)
            } else if is_mutex_lock_call(fname) {
                Some(YieldKind::MutexLock)
            } else if is_rwlock_read_call(fname) {
                Some(YieldKind::RwLockRead)
            } else if is_rwlock_write_call(fname) {
                Some(YieldKind::RwLockWrite)
            } else if is_channel_send_call(fname) {
                Some(YieldKind::ChannelSend)
            } else if is_channel_recv_call(fname) {
                Some(YieldKind::ChannelRecv)
            } else if is_socket_read_call(fname) {
                Some(YieldKind::SocketRead)
            } else if is_socket_write_call(fname) {
                Some(YieldKind::SocketWrite)
            } else if is_socket_accept_call(fname) {
                Some(YieldKind::SocketAccept)
            } else if is_socket_connect_call(fname) {
                Some(YieldKind::SocketConnect)
            } else if is_blocking_call(fname) {
                Some(YieldKind::Blocking)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// True if any basic block in `fn_name` contains a true yield point (await or sleep).
/// Blocking calls are NOT yield points — they run inline with temp worker replacement.
fn fn_has_internal_await(fn_name: &str, module: &Module) -> bool {
    let Some(func) = module.find_function(fn_name) else { return false };
    func.blocks.iter().any(|bb| {
        bb.instructions.iter().any(|inst| {
            matches!(classify_yield(inst, func), Some(YieldKind::Await | YieldKind::Sleep | YieldKind::MutexLock | YieldKind::RwLockRead | YieldKind::RwLockWrite | YieldKind::ChannelSend | YieldKind::ChannelRecv | YieldKind::SocketRead | YieldKind::SocketWrite | YieldKind::SocketAccept | YieldKind::SocketConnect))
        })
    })
}
/// Compute type overrides for a GIR function's locals.
/// The GIR type system doesn't always track C-level types precisely,
/// so this pre-scan infers correct C types from instruction patterns.
fn compute_type_overrides(
    func: &Function,
    registry: &TypeRegistry,
    module: &Module,
) -> std::collections::HashMap<usize, String> {
    let mut type_overrides: std::collections::HashMap<usize, String> = std::collections::HashMap::new();
    for _pass in 0..3 {
    let prev_count = type_overrides.len();
    for block in &func.blocks {
        for inst in &block.instructions {
            match inst {
                Instruction::Call { dst: Some(dst_id), func: call_name, args: call_args }
                | Instruction::CallExtern { dst: Some(dst_id), func: call_name, args: call_args } => {
                    // GIR IR trust: if the local already has a meaningful Named type from the IR,
                    // use it instead of heuristic overrides. This prevents infer_runtime_return_type
                    // from incorrectly overriding Result/Option/struct types.
                    let dst_idx = dst_id.0 as usize;
                    let ir_type_is_named = dst_idx < func.locals.len() && {
                        let tid = func.locals[dst_idx].type_id;
                        matches!(registry.get(tid), Some(GirType::Named(_)))
                    };
                    // __result_unwrap / __option_unwrap: extract inner type from the arg's type
                    if (call_name.starts_with("__result_unwrap") || call_name.starts_with("__option_unwrap"))
                        && !call_args.is_empty()
                    {
                        let arg_type = match &call_args[0] {
                            Operand::Copy(p) | Operand::Move(p) => {
                                effective_c_type(p.local.0 as usize, func, registry, &type_overrides)
                            }
                            _ => String::new(),
                        };
                        // Strip pointer suffix for &option / &result
                        let base = arg_type.strip_suffix('*').unwrap_or(&arg_type);
                        if let Some(inner) = extract_enum_payload_type(base) {
                            let rt = runtime_type_name(&inner).unwrap_or(&inner);
                            type_overrides.insert(dst_idx, rt.to_string());
                        }
                    }
                    // Check if this is a collection method with a known scalar return type
                    // (e.g., Vector__T__len returns int64_t, not Vector__T)
                    let method_return_override = if let Some(rewrite) = try_rewrite_collection_method(call_name) {
                        if rewrite.field_access.is_some() {
                            // field_access methods (len, is_empty) return scalars
                            Some("int64_t".to_string())
                        } else {
                            None
                        }
                    } else if let Some(rt) = infer_method_return_type(call_name) {
                        if rt != "int64_t" && rt != "void" {
                            Some(rt.to_string())
                        } else {
                            None // let it fall through to the normal heuristics
                        }
                    } else {
                        None
                    };
                    if let Some(ret_type) = method_return_override {
                        type_overrides.insert(dst_idx, ret_type);
                    } else if ir_type_is_named && !type_overrides.contains_key(&dst_idx) {
                        let tid = func.locals[dst_idx].type_id;
                        let gir_c_type = format_type(tid, registry);
                        // Cross-check with callee return type: if callee has a different
                        // return type, trust the callee over the GIR local type.
                        let callee_ret = module.functions.iter()
                            .find(|f| f.name == call_name.as_str())
                            .map(|callee| format_type(callee.return_type, registry));
                        let runtime_ret = infer_runtime_return_type(call_name)
                            .or_else(|| infer_method_return_type(call_name));
                        let c_type = if let Some(ref ret) = callee_ret {
                            if ret != "int64_t" && ret != "void" && ret != &gir_c_type {
                                ret.clone()
                            } else {
                                gir_c_type
                            }
                        } else if let Some(rt) = runtime_ret {
                            if rt != "int64_t" && rt != "void" && rt != gir_c_type {
                                rt.to_string()
                            } else {
                                gir_c_type
                            }
                        } else {
                            gir_c_type
                        };
                        type_overrides.insert(dst_idx, c_type);
                    } else if is_collection_constructor(call_name) {
                        let type_name = call_name.strip_suffix("__new").unwrap_or(call_name);
                        let c_type = if let Some(alias) = collection_type_alias(type_name) {
                            alias.to_string()
                        } else if type_name == "String" {
                            "GorgetString".to_string()
                        } else if type_name == "Box" {
                            "void*".to_string()
                        } else {
                            type_name.to_string()
                        };
                        type_overrides.insert(dst_id.0 as usize, c_type);
                    } else if registry.get_type_def(call_name).is_some() {
                        type_overrides.insert(dst_id.0 as usize, call_name.to_string());
                    } else if call_name == "gorget_array_new" {
                        // gorget_array_new(sizeof(T)) → Vector__T
                        if let Some(Operand::Constant(Constant::SizeOf(elem_tid))) = call_args.first() {
                            let elem_c = format_type(*elem_tid, registry);
                            let vec_type = format!("Vector__{}", elem_c);
                            if collection_type_alias(&vec_type).is_some() {
                                type_overrides.insert(dst_id.0 as usize, vec_type);
                            } else {
                                type_overrides.insert(dst_id.0 as usize, "GorgetArray".to_string());
                            }
                        } else {
                            type_overrides.insert(dst_id.0 as usize, "GorgetArray".to_string());
                        }
                    } else if call_name == "gorget_dict_new" || call_name == "gorget_map_new"
                        || call_name == "gorget_dict_new_str" || call_name == "gorget_map_new_str" {
                        type_overrides.insert(dst_id.0 as usize, "GorgetDict".to_string());
                    } else if call_name == "gorget_set_new" {
                        type_overrides.insert(dst_id.0 as usize, "GorgetSet".to_string());
                    } else {
                        // For Dict/HashMap filter, force GorgetMap return type
                        if (call_name.starts_with("Dict__") || call_name.starts_with("HashMap__"))
                            && extract_trailing_method(call_name, "") == "filter"
                        {
                            type_overrides.insert(dst_id.0 as usize, "GorgetMap".to_string());
                        }
                        // Option/Result.map/map_err/and_then/flatten: output type depends on closure return type
                        if call_name.starts_with("Option__") || call_name.starts_with("Result__") {
                            let method = extract_trailing_method(call_name, "");
                            let needs_closure_ret = matches!(method, "map" | "map_err" | "and_then");
                            let is_flatten = method == "flatten";
                            if needs_closure_ret {
                                if let Some(closure_op) = call_args.get(1) {
                                    if let Operand::Copy(p) | Operand::Move(p) = closure_op {
                                        let closure_c_type = effective_c_type(p.local.0 as usize, func, registry, &type_overrides);
                                        let call_fn = format!("{closure_c_type}__call");
                                        if let Some(callee) = module.functions.iter().find(|f| f.name == call_fn) {
                                            let ret = format_type(callee.return_type, registry);
                                            let ret = runtime_type_name(&ret).unwrap_or(&ret).to_string();
                                            // Normalize GorgetString → Str for type parameter names
                                            // (both are `str` in Gorget; typedefs use Str)
                                            let ret = if ret == "GorgetString" { "Str".to_string() } else { ret };
                                            if call_name.starts_with("Option__") {
                                                if method == "map" {
                                                    type_overrides.insert(dst_id.0 as usize, format!("Option__{ret}"));
                                                } else if method == "and_then" {
                                                    // and_then closure returns Option<T> directly
                                                    type_overrides.insert(dst_id.0 as usize, ret);
                                                }
                                            } else {
                                                // Result — extract error type from type prefix
                                                // call_name = Result__OkType__ErrType__method
                                                let type_prefix = &call_name[..call_name.len() - method.len() - 2]; // strip __method
                                                let inner = type_prefix.strip_prefix("Result__").unwrap_or(type_prefix);
                                                // Extract error type: last type component after __
                                                let err_type = if let Some(pos) = inner.rfind("__") {
                                                    &inner[pos + 2..]
                                                } else {
                                                    inner
                                                };
                                                let ok_type = if let Some(pos) = inner.rfind("__") {
                                                    &inner[..pos]
                                                } else {
                                                    inner
                                                };
                                                if method == "map" {
                                                    type_overrides.insert(dst_id.0 as usize, format!("Result__{ret}__{err_type}"));
                                                } else if method == "map_err" {
                                                    type_overrides.insert(dst_id.0 as usize, format!("Result__{ok_type}__{ret}"));
                                                } else if method == "and_then" {
                                                    // and_then closure returns Result — extract its Ok type
                                                    // but preserve the original error type from the source Result
                                                    let new_ok = if let Some(inner_ret) = ret.strip_prefix("Result__") {
                                                        // Get the Ok type from Result__NewOk__Whatever
                                                        if let Some(pos) = inner_ret.rfind("__") {
                                                            &inner_ret[..pos]
                                                        } else {
                                                            inner_ret
                                                        }
                                                    } else {
                                                        &ret
                                                    };
                                                    type_overrides.insert(dst_id.0 as usize, format!("Result__{new_ok}__{err_type}"));
                                                }
                                            }
                                        }
                                    }
                                }
                            } else if is_flatten {
                                // Option[Option[T]].flatten() → Option[T]
                                // Extract inner type from Option__Option__X
                                let type_prefix = &call_name[..call_name.len() - "flatten".len() - 2];
                                if let Some(inner) = type_prefix.strip_prefix("Option__Option__") {
                                    type_overrides.insert(dst_id.0 as usize, format!("Option__{inner}"));
                                } else if let Some(inner) = type_prefix.strip_prefix("Result__Result__") {
                                    type_overrides.insert(dst_id.0 as usize, format!("Result__{inner}"));
                                }
                            }
                        }
                        // General inference path (for everything not already overridden)
                        if !type_overrides.contains_key(&(dst_id.0 as usize)) {
                            // Check user-defined functions FIRST (they take priority over stdlib heuristics)
                            let lookup_name = call_name.as_str();
                            let user_fn_ret = module.functions.iter()
                                .find(|f| f.name == lookup_name)
                                .map(|callee| {
                                    // FnPtr return type = escaped closure → GorgetClosure
                                    if matches!(registry.get(callee.return_type), Some(GirType::FnPtr { .. })) {
                                        "GorgetClosure".to_string()
                                    } else {
                                        format_type(callee.return_type, registry)
                                    }
                                });
                            if let Some(ref ret) = user_fn_ret {
                                // Don't override if the GIR local type is Option/Result
                                // and the callee returns a simpler type — the GIR's
                                // wrapper type is authoritative (caller expects wrapped).
                                let gir_type_name = if let Some(GirType::Named(n)) = registry.get(func.locals[dst_id.0 as usize].type_id) {
                                    Some(n.as_str())
                                } else { None };
                                let gir_is_wrapper = gir_type_name.map_or(false, |n| n.starts_with("Option__") || n.starts_with("Result__"));
                                let ret_is_wrapper = ret.starts_with("Option__") || ret.starts_with("Result__");
                                if ret != "int64_t" && ret != "void" && !(gir_is_wrapper && !ret_is_wrapper) {
                                    type_overrides.insert(dst_id.0 as usize, ret.clone());
                                }
                            } else if let Some(rt) = infer_runtime_return_type(call_name) {
                                // Float-aware dispatch for abs/min/max
                                let rt = if matches!(call_name.as_str(), "abs" | "min" | "max" | "gorget_abs" | "gorget_min" | "gorget_max")
                                    && has_float_arg_with_overrides(call_args, func, &type_overrides)
                                {
                                    "double"
                                } else {
                                    rt
                                };
                                type_overrides.insert(dst_id.0 as usize, rt.to_string());
                            } else if let Some(rt) = infer_method_return_type(call_name) {
                                type_overrides.insert(dst_id.0 as usize, rt.to_string());
                            } else {
                                // cstr-returning functions: override local to Str
                                let mapped = map_stdlib_name(call_name);
                                if returns_cstr(mapped) || returns_cstr(call_name) {
                                    type_overrides.insert(dst_id.0 as usize, "Str".to_string());
                                }
                            }
                        }
                    }
                }
                Instruction::StructInit { dst, type_name, .. }
                | Instruction::EnumInit { dst, type_name, .. } => {
                    type_overrides.insert(dst.0 as usize, type_name.clone());
                }
                // Propagate type through simple assignments
                Instruction::Assign { dst, value } => {
                    // Handle float constant assignments for type override propagation
                    // Only F64/F32 — other constants (Str, Bool) are handled by the IR type system
                    if matches!(value, Operand::Constant(Constant::F64(_)) | Operand::Constant(Constant::F32(_))) {
                        let dst_idx = dst.local.0 as usize;
                        type_overrides.entry(dst_idx).or_insert_with(|| "double".to_string());
                    }
                    if let Operand::Copy(src_place) | Operand::Move(src_place) = value {
                        let src_idx = src_place.local.0 as usize;
                        let dst_idx = dst.local.0 as usize;
                        // If source place has a Deref projection, strip pointer from type
                        let _has_deref = src_place.projections.iter().any(|p| matches!(p, Projection::Deref));
                        // Resolve Field projections: walk projections to find the actual type
                        // e.g., _x[Field(1)] on GorgetArray → resolve .len → int64_t
                        let resolve_field_projections = |base_type: &str| -> String {
                            let mut current = base_type.to_string();
                            for proj in &src_place.projections {
                                match proj {
                                    Projection::Deref => {
                                        // Handle Box__T typedef: deref gives T
                                        if let Some(inner) = current.strip_prefix("Box__") {
                                            current = inner.to_string();
                                        } else {
                                            current = current.strip_suffix('*')
                                                .unwrap_or(&current).to_string();
                                            current = current.strip_prefix("const ")
                                                .unwrap_or(&current).to_string();
                                        }
                                    }
                                    Projection::Field(idx) => {
                                        let deref = current.strip_suffix('*')
                                            .unwrap_or(&current);
                                        let deref = deref.strip_prefix("const ")
                                            .unwrap_or(deref);
                                        // Look up field type from type def
                                        let field_type = if let Some(type_def) = registry.get_type_def(deref) {
                                            if let TypeDefKind::Struct(ref sd) = type_def.kind {
                                                sd.fields.get(*idx as usize)
                                                    .map(|fld| format_type(fld.type_id, registry))
                                            } else if let TypeDefKind::Enum(ref ed) = type_def.kind {
                                                // For enum field access, resolve variant field types
                                                // Field(0) = tag (int32_t), Field(1+) = variant data
                                                if *idx == 0 {
                                                    Some("int32_t".to_string())
                                                } else {
                                                    // Accessing variant data — resolve through variant fields
                                                    ed.variants.get((*idx - 1) as usize)
                                                        .and_then(|v| v.fields.first())
                                                        .map(|f| format_type(f.type_id, registry))
                                                }
                                            } else { None }
                                        } else { None };
                                        // Fallback: runtime type fields
                                        let resolved_alias = collection_type_alias(deref)
                                            .unwrap_or(deref);
                                        let field_type = field_type.or_else(|| {
                                            match (resolved_alias, *idx) {
                                                ("GorgetArray", 0) => Some("void*".to_string()),
                                                ("GorgetArray", 1..=3) => Some("int64_t".to_string()),
                                                ("Str", 0) => Some("char*".to_string()),
                                                ("Str", 1) => Some("int64_t".to_string()),
                                                ("GorgetString", 0) => Some("char*".to_string()),
                                                ("GorgetString", 1..=2) => Some("int64_t".to_string()),
                                                _ => None,
                                            }
                                        });
                                        // Result/Option inner type extraction:
                                        // When accessing .data.Ok._0 or .data.Some._0, resolve
                                        // to the inner payload type from the type name.
                                        let field_type = field_type.or_else(|| {
                                            extract_enum_payload_type(deref)
                                        });
                                        if let Some(ft) = field_type {
                                            current = ft;
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            current
                        };
                        // Check if destination is UNIT_TYPE and lacks an override
                        let dst_is_unit = dst_idx < func.locals.len()
                            && func.locals[dst_idx].type_id == UNIT_TYPE
                            && !type_overrides.contains_key(&dst_idx);
                        if dst_is_unit {
                            // Try override first, then IR type
                            if let Some(src_type) = type_overrides.get(&src_idx).cloned() {
                                let src_type = resolve_field_projections(&src_type);
                                type_overrides.insert(dst_idx, src_type);
                            } else if src_idx < func.locals.len() {
                                let tid = func.locals[src_idx].type_id;
                                if tid != UNIT_TYPE {
                                    let formatted = format_type(tid, registry);
                                    if formatted != "void" {
                                        let resolved = resolve_field_projections(&formatted);
                                        type_overrides.insert(dst_idx, resolved);
                                    }
                                }
                            }
                        } else {
                            // Get effective source type: override first, then GIR type
                            let raw_src_type = if let Some(ovr) = type_overrides.get(&src_idx).cloned() {
                                Some(ovr)
                            } else if !src_place.projections.is_empty() && src_idx < func.locals.len() {
                                // Source has projections (Deref/Field) — resolve from GIR type
                                let formatted = format_type(func.locals[src_idx].type_id, registry);
                                if formatted != "int64_t" && formatted != "void" {
                                    Some(formatted)
                                } else { None }
                            } else { None };
                            if let Some(raw_src_type) = raw_src_type {
                                let src_type = resolve_field_projections(&raw_src_type);
                                let dst_ir_type = if dst_idx < func.locals.len() {
                                    func.locals[dst_idx].type_id
                                } else {
                                    UNIT_TYPE
                                };
                                if dst_ir_type == UNIT_TYPE {
                                    // UNIT_TYPE = placeholder, always propagate
                                    type_overrides.entry(dst_idx).or_insert(src_type);
                                } else if dst_ir_type == I64_TYPE && src_type != "int64_t" {
                                    // I64_TYPE is the GIR fallback for unknown types.
                                    // If the source has a real override (struct, collection, Str, etc.),
                                    // propagate it. This handles FieldLoad/IndexLoad chains.
                                    // Don't downgrade existing overrides.
                                    // Exception: don't propagate Option__* types to I64_TYPE
                                    // destinations — the GIR may have lowered .unwrap() as a no-op
                                    // copy because it doesn't know the intermediate is Option.
                                    if !type_overrides.contains_key(&dst_idx)
                                        && !src_type.starts_with("Option__")
                                    {
                                        type_overrides.insert(dst_idx, src_type);
                                    }
                                } else if src_type == "double" && (dst_ir_type == I64_TYPE || dst_ir_type == F64_TYPE) {
                                    // Float return from abs/min/max/etc: override int64_t → double
                                    type_overrides.insert(dst_idx, src_type);
                                } else if src_place.projections.is_empty()
                                    && (src_type.starts_with("Result__") || src_type.starts_with("Option__"))
                                {
                                    // Result/Option type override from closure-based inference:
                                    // map/and_then/map_err can change the type signature.
                                    // Propagate — this takes precedence over GIR-inferred Result/Option types
                                    // because GIR doesn't track closure return types correctly.
                                    type_overrides.insert(dst_idx, src_type);
                                } else if !src_place.projections.is_empty()
                                    && !type_overrides.contains_key(&dst_idx)
                                    && type_overrides.contains_key(&src_idx)
                                {
                                    // Source has field projections through an overridden base.
                                    type_overrides.insert(dst_idx, src_type);
                                }
                            }
                        }
                    }
                }
                // IndexLoad on a collection: override element type (or collection type for slices)
                Instruction::IndexLoad { dst, base, index } => {
                    let base_idx = base.local.0 as usize;
                    let dst_idx = dst.0 as usize;
                    let base_type = if let Some(bt) = type_overrides.get(&base_idx) {
                        bt.clone()
                    } else if base_idx < func.locals.len() {
                        format_type(func.locals[base_idx].type_id, registry)
                    } else {
                        "int64_t".to_string()
                    };
                    // Strip pointer suffix and const qualifier for ref-to-collection
                    let base_deref = base_type.strip_suffix('*').unwrap_or(&base_type);
                    let base_deref = base_deref.strip_prefix("const ").unwrap_or(base_deref);
                    // Check if index is a Range (slice operation) → result is same collection type
                    let index_is_range = match index {
                        Operand::Copy(p) | Operand::Move(p) => {
                            let idx_local = p.local.0 as usize;
                            if let Some(override_t) = type_overrides.get(&idx_local) {
                                override_t == "GorgetRange"
                            } else if idx_local < func.locals.len() {
                                matches!(registry.get(func.locals[idx_local].type_id),
                                    Some(GirType::Named(n)) if n == "GorgetRange")
                            } else {
                                false
                            }
                        }
                        _ => false,
                    };
                    if index_is_range && (base_deref.starts_with("Vector__")
                        || base_deref.starts_with("GorgetArray")
                        || base_deref.starts_with("Dict__")
                        || base_deref.starts_with("HashMap__"))
                    {
                        // Slice returns the same collection type
                        type_overrides.insert(dst_idx, base_deref.to_string());
                    } else if let Some(elem) = extract_element_type_from_collection(base_deref)
                        .or_else(|| {
                            // base_deref may be normalized (e.g. "GorgetArray"); recover element
                            // type from the raw IR type when it is a named collection.
                            if base_idx < func.locals.len() {
                                if let Some(GirType::Named(raw)) = registry.get(func.locals[base_idx].type_id) {
                                    extract_element_type_from_collection(raw.as_str())
                                } else { None }
                            } else { None }
                        })
                    {
                        type_overrides.insert(dst_idx, elem);
                    } else if base_deref == "Str" {
                        type_overrides.insert(dst_idx, "Str".to_string());
                    }
                }
                // Borrow/BorrowMut: track that dst is a pointer to the base's type
                Instruction::Borrow { dst, place } | Instruction::BorrowMut { dst, place } => {
                    let base_idx = place.local.0 as usize;
                    let base_type = if let Some(bt) = type_overrides.get(&base_idx) {
                        bt.clone()
                    } else if base_idx < func.locals.len() {
                        format_type(func.locals[base_idx].type_id, registry)
                    } else {
                        continue;
                    };
                    // Walk projections to resolve actual borrowed type
                    // e.g., &_1.items where _1 is Registry → resolved to GorgetMap
                    let mut resolved = base_type.clone();
                    for proj in &place.projections {
                        match proj {
                            Projection::Deref => {
                                // Unwrap pointer
                                if let Some(inner) = resolved.strip_prefix("Box__") {
                                    resolved = inner.to_string();
                                } else {
                                    resolved = resolved.strip_suffix('*')
                                        .unwrap_or(&resolved).to_string();
                                    resolved = resolved.strip_prefix("const ")
                                        .unwrap_or(&resolved).to_string();
                                }
                            }
                            Projection::Field(idx) => {
                                let deref = resolved.strip_suffix('*')
                                    .unwrap_or(&resolved);
                                let deref = deref.strip_prefix("const ")
                                    .unwrap_or(deref);
                                // Look up field type from type def
                                let field_type = if let Some(type_def) = registry.get_type_def(deref) {
                                    if let TypeDefKind::Struct(ref sd) = type_def.kind {
                                        sd.fields.get(*idx as usize)
                                            .map(|fld| format_type(fld.type_id, registry))
                                    } else { None }
                                } else { None };
                                // Fallback: runtime type fields
                                let resolved_alias = collection_type_alias(deref)
                                    .unwrap_or(deref);
                                let field_type = field_type.or_else(|| {
                                    match (resolved_alias, *idx) {
                                        ("GorgetArray", 0) => Some("void*".to_string()),
                                        ("GorgetArray", 1..=3) => Some("int64_t".to_string()),
                                        ("Str", 0) => Some("char*".to_string()),
                                        ("Str", 1) => Some("int64_t".to_string()),
                                        ("GorgetString", 0) => Some("char*".to_string()),
                                        ("GorgetString", 1..=2) => Some("int64_t".to_string()),
                                        _ => None,
                                    }
                                });
                                if let Some(ft) = field_type {
                                    resolved = ft;
                                }
                            }
                            _ => {}
                        }
                    }
                    // Store as pointer type for IndexLoad lookups
                    if resolved != "int64_t" && resolved != "void" {
                        type_overrides.insert(dst.0 as usize, format!("{}*", resolved));
                    }
                }
                // FieldLoad: propagate known struct field types
                Instruction::FieldLoad { dst, base, field } => {
                    let base_idx = base.local.0 as usize;
                    let base_type = if let Some(bt) = type_overrides.get(&base_idx) {
                        bt.clone()
                    } else if base_idx < func.locals.len() {
                        format_type(func.locals[base_idx].type_id, registry)
                    } else {
                        continue;
                    };
                    let base_deref = base_type.strip_suffix('*').unwrap_or(&base_type);
                    let base_deref = base_deref.strip_prefix("const ").unwrap_or(base_deref);
                    let dst_idx = dst.0 as usize;
                    // Look up field type from type def registry
                    let field_c_type = if let Some(type_def) = registry.get_type_def(base_deref) {
                        if let TypeDefKind::Struct(ref sd) = type_def.kind {
                            sd.fields.get(*field as usize)
                                .map(|fld| format_type(fld.type_id, registry))
                        } else if let TypeDefKind::Enum(ref ed) = type_def.kind {
                            // Enum field access: Field(0) = tag, Field(1+) = variant data
                            if *field == 0 {
                                Some("int32_t".to_string())
                            } else {
                                ed.variants.get(*field as usize - 1)
                                    .and_then(|v| v.fields.first())
                                    .map(|f| format_type(f.type_id, registry))
                            }
                        } else { None }
                    } else { None };
                    // Fallback: resolve known runtime type fields
                    let resolved = base_deref.strip_prefix("const ").unwrap_or(base_deref);
                    let resolved_alias = collection_type_alias(resolved).unwrap_or(resolved);
                    let field_c_type = field_c_type.or_else(|| {
                        match (resolved_alias, *field) {
                            ("Str", 0) => Some("char*".to_string()),      // data
                            ("Str", 1) => Some("int64_t".to_string()),    // len
                            ("GorgetString", 0) => Some("char*".to_string()),
                            ("GorgetString", 1) => Some("int64_t".to_string()),
                            ("GorgetString", 2) => Some("int64_t".to_string()),
                            ("GorgetArray", 0) => Some("void*".to_string()),   // data
                            ("GorgetArray", 1) => Some("int64_t".to_string()), // len
                            ("GorgetArray", 2) | ("GorgetArray", 3) => Some("int64_t".to_string()),
                            _ => None,
                        }
                    });
                    if let Some(field_c_type) = field_c_type {
                        if field_c_type != "int64_t" && field_c_type != "void" {
                            type_overrides.insert(dst_idx, field_c_type);
                        } else if field_c_type == "int64_t" && !type_overrides.contains_key(&dst_idx) {
                            // Only correct if dst has no existing override and its GIR type
                            // is a collection (clearly wrong for a scalar field like .len)
                            let gir_type = if dst_idx < func.locals.len() {
                                format_type(func.locals[dst_idx].type_id, registry)
                            } else { "int64_t".to_string() };
                            if gir_type == "GorgetArray" || gir_type.starts_with("Vector__")
                                || gir_type == "GorgetDict" || gir_type == "GorgetMap" {
                                type_overrides.insert(dst_idx, "int64_t".to_string());
                            }
                        }
                    }
                }
                // EnumFieldLoad: look up enum variant field type
                Instruction::EnumFieldLoad { dst, base, variant, field } => {
                    let base_idx = base.local.0 as usize;
                    let base_type = if let Some(bt) = type_overrides.get(&base_idx) {
                        bt.clone()
                    } else if base_idx < func.locals.len() {
                        format_type(func.locals[base_idx].type_id, registry)
                    } else {
                        continue;
                    };
                    // Strip pointer suffix for borrowed enum values
                    let base_deref = base_type.strip_suffix('*').unwrap_or(&base_type);
                    let base_deref = base_deref.strip_prefix("const ").unwrap_or(base_deref);
                    if let Some(type_def) = registry.get_type_def(base_deref) {
                        if let TypeDefKind::Enum(ref e) = type_def.kind {
                            if let Some(var) = e.variants.iter().find(|v| v.name == *variant) {
                                if let Some(fld) = var.fields.get(*field as usize) {
                                    let field_c_type = format_type(fld.type_id, registry);
                                    let dst_idx = dst.0 as usize;
                                    if field_c_type != "void" {
                                        // Always set override for enum field loads — the base
                                        // type may have been corrected by map/map_err/and_then
                                        // in a later pass, so we need to update the field type.
                                        if field_c_type != "int64_t" || type_overrides.contains_key(&dst_idx) {
                                            type_overrides.insert(dst_idx, field_c_type);
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        // Type def not found (may be a C-backend-created type name from
                        // map/map_err/and_then type override). Extract field type from type name.
                        if let Some(inner) = base_deref.strip_prefix("Result__") {
                            let (ok_type, err_type) = if let Some(pos) = inner.rfind("__") {
                                (&inner[..pos], &inner[pos + 2..])
                            } else {
                                (inner, inner)
                            };
                            let field_c_type = if variant == "Ok" { ok_type } else { err_type };
                            let field_c_type = runtime_type_name(field_c_type).unwrap_or(field_c_type);
                            type_overrides.insert(dst.0 as usize, field_c_type.to_string());
                        } else if let Some(inner) = base_deref.strip_prefix("Option__") {
                            if variant == "Some" {
                                let inner = runtime_type_name(inner).unwrap_or(inner);
                                type_overrides.insert(dst.0 as usize, inner.to_string());
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }
    // Convergence: stop when no new overrides were added
    if type_overrides.len() == prev_count { break; }
    } // end multi-pass loop
    type_overrides
}


/// True if the function can be converted to a stackless coroutine.
/// Returns false for functions with unsupported instructions / terminators.
fn fn_is_coroutine_candidate(fn_name: &str, module: &Module) -> bool {
    // Shared-token variants manage their own lock/unlock cycle — not coroutines.
    if fn_name.starts_with("__shared_token_") {
        return false;
    }
    if !fn_has_internal_await(fn_name, module) {
        return false;
    }
    let Some(func) = module.find_function(fn_name) else { return false };
    for bb in &func.blocks {
        for inst in &bb.instructions {
            match inst {
                // InlineC contains raw `_N` local references — can't rewrite
                Instruction::InlineC { .. }
                | Instruction::PushAllocator { .. }
                | Instruction::PopAllocator
                | Instruction::LoadThreadLocal { .. } => return false,
                _ => {}
            }
        }
        // Invoke (landingpad) terminator not yet supported in coroutine context
        if matches!(&bb.terminator, Some(Terminator::Invoke { .. })) {
            return false;
        }
        // Multiple awaits per BB are supported — each await gets its own
        // pre-await/resume state pair in coroutine_state_ids.
    }
    true
}

/// Format a Place for access inside a coroutine poll function (frame field: `f->_N`).
fn fmt_place_poll(place: &Place, func: &Function, registry: &TypeRegistry) -> String {
    let mut s = format!("f->_{}", place.local.0);
    let mut current_type_id = {
        let idx = place.local.0 as usize;
        if idx < func.locals.len() { Some(func.locals[idx].type_id) } else { None }
    };
    for proj in &place.projections {
        match proj {
            Projection::Field(idx) => {
                let field_name = current_type_id
                    .and_then(|tid| resolve_field_name_from_type(tid, *idx, registry));
                if let Some((name, next_type)) = field_name {
                    let _ = write!(s, ".{name}");
                    current_type_id = Some(next_type);
                } else {
                    let _ = write!(s, "._{idx}");
                    current_type_id = None;
                }
            }
            Projection::Index(local) => {
                let _ = write!(s, "[f->_{}]", local.0);
                current_type_id = None;
            }
            Projection::Deref => {
                s = format!("(*{s})");
                current_type_id = current_type_id.and_then(|tid| {
                    match registry.get(tid)? {
                        GirType::Ptr(inner) | GirType::MutPtr(inner) => Some(*inner),
                        _ => None,
                    }
                });
            }
        }
    }
    s
}

/// Format an Operand for use inside a coroutine poll function.
fn fmt_operand_poll(op: &Operand, func: &Function, registry: &TypeRegistry) -> String {
    match op {
        Operand::Copy(place) | Operand::Move(place) => fmt_place_poll(place, func, registry),
        Operand::Constant(c) => format_constant(c, func, registry),
    }
}

/// Format an Operand as Str type in poll context (wraps string literals in gorget_str_from_literal).
fn fmt_operand_poll_as_str(op: &Operand, func: &Function, registry: &TypeRegistry) -> String {
    if let Operand::Constant(Constant::Str(s)) = op {
        let escaped = escape_c_string(s);
        return format!("gorget_str_from_literal(\"{escaped}\", {})", s.len());
    }
    fmt_operand_poll(op, func, registry)
}

/// Format arguments for a function call in poll context.
fn fmt_args_poll(args: &[Operand], func: &Function, registry: &TypeRegistry) -> String {
    args.iter()
        .map(|a| fmt_operand_poll(a, func, registry))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Format arguments for a Gorget Call in poll context — wraps Constant::Str in gorget_str_from_literal.
/// Gorget functions expect `Str` type, not bare `const char*`.
#[allow(dead_code)]
fn fmt_args_poll_gorget(args: &[Operand], func: &Function, registry: &TypeRegistry) -> String {
    args.iter()
        .map(|a| fmt_operand_poll_as_str(a, func, registry))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Format arguments for a mapped C stdlib call in poll context.
/// Str-typed args are converted to `const char*` via `gorget_str_to_cstr()` since
/// C runtime functions expect null-terminated strings.
#[allow(dead_code)]
fn fmt_args_poll_cstr(args: &[Operand], func: &Function, registry: &TypeRegistry) -> String {
    args.iter()
        .map(|a| {
            if let Operand::Constant(Constant::Str(s)) = a {
                let escaped = escape_c_string(s);
                return format!("\"{escaped}\"");
            }
            let base = fmt_operand_poll(a, func, registry);
            match a {
                Operand::Copy(p) | Operand::Move(p) => {
                    let local_idx = p.local.0 as usize;
                    if local_idx < func.locals.len() {
                        let c_type = format_type(func.locals[local_idx].type_id, registry);
                        if c_type == "Str" {
                            return format!("gorget_str_to_cstr({base})");
                        }
                        if c_type == "GorgetString" {
                            return format!("{base}.data");
                        }
                    }
                    base
                }
                _ => base,
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

/// Format arguments for printf/fprintf in poll context.
/// Str-typed args are expanded to `(int)arg.len, arg.data`.
/// Bool args are expanded to ternary true/false strings.
fn fmt_printf_args_poll(args: &[Operand], func: &Function, registry: &TypeRegistry) -> String {
    let mut parts = Vec::new();
    for (i, arg) in args.iter().enumerate() {
        let arg_str = fmt_operand_poll(arg, func, registry);
        if i == 0 {
            // First arg is the format string — pass through as bare const char*
            parts.push(arg_str);
            continue;
        }
        match arg {
            Operand::Copy(p) | Operand::Move(p) => {
                let local_idx = p.local.0 as usize;
                if local_idx < func.locals.len() {
                    let c_type = format_type(func.locals[local_idx].type_id, registry);
                    if c_type == "Str" || c_type == "GorgetString" {
                        // Str/GorgetString → expand to (int)len, data for %.*s
                        parts.push(format!("(int){arg_str}.len"));
                        parts.push(format!("{arg_str}.data"));
                        continue;
                    }
                    if c_type == "bool" || c_type == "_Bool" {
                        parts.push(format!("({arg_str} ? \"true\" : \"false\")"));
                        continue;
                    }
                }
            }
            _ => {}
        }
        parts.push(arg_str);
    }
    parts.join(", ")
}

/// Emit drop code for a local variable. `place_str` is the pre-formatted C expression
/// for the place (e.g., `_5` in normal context, `f->_5` in coroutine poll context).
/// Used by both normal instruction emission and coroutine poll emission.
fn emit_drop_code(out: &mut String, place_str: &str, local_type: TypeId, registry: &TypeRegistry) {
    let type_name_str = format_type(local_type, registry);
    let gir_name = gir_type_name(local_type, registry);

    if let Some(inner_name) = type_name_str.strip_prefix("Box__") {
        let is_trait_box = registry.get_type_def(&format!("{inner_name}_TraitObj")).is_some();
        if is_trait_box {
            let _ = writeln!(out, "        free({place_str}.data);");
        } else {
            if let Some(inner_def) = registry.get_type_def(inner_name) {
                if let DropStrategy::Custom(ref fn_name) = inner_def.metadata.drop_strategy {
                    let _ = writeln!(out, "        {fn_name}({place_str});");
                }
            }
            let _ = writeln!(out, "        free({place_str});");
        }
    } else if let Some(elem_name) = gir_name.as_deref().and_then(extract_vector_elem_name) {
        if needs_drop_by_name(elem_name, registry) {
            let elem_c_type = gir_to_c_type(elem_name);
            let _ = writeln!(out, "        for (size_t __di = 0; __di < {place_str}.len; __di++) {{");
            let _ = writeln!(
                out,
                "            {elem_c_type}* __de = ({elem_c_type}*)gorget_array_get(&{place_str}, __di);"
            );
            emit_drop_for_type_via_ptr(out, "__de", elem_name, registry, "            ", 1);
            let _ = writeln!(out, "        }}");
        }
        let _ = writeln!(out, "        gorget_array_free(&{place_str});");
    } else {
        let strategy = lookup_drop_strategy(local_type, registry);
        match strategy {
            DropStrategy::None => {}
            DropStrategy::Trivial(ref fn_name) => {
                let _ = writeln!(out, "        {fn_name}(&{place_str});");
            }
            DropStrategy::Custom(ref fn_name) => {
                let _ = writeln!(out, "        if (memcmp(&{place_str}, &({type_name_str}){{0}}, sizeof({type_name_str})) != 0) {{");
                let _ = writeln!(out, "            {fn_name}(&{place_str});");
                emit_field_drops(out, place_str, local_type, registry, "            ", 0);
                out.push_str("        }\n");
            }
            DropStrategy::Recursive => {
                let _ = writeln!(out, "        if (memcmp(&{place_str}, &({type_name_str}){{0}}, sizeof({type_name_str})) != 0) {{");
                emit_field_drops(out, place_str, local_type, registry, "            ", 0);
                out.push_str("        }\n");
            }
        }
    }
}

/// Emit drop-if-alive code for a local variable. Like `emit_drop_code` but with
/// null/zero checks so already-moved values are not double-freed.
fn emit_drop_if_alive_code(out: &mut String, place_str: &str, local_type: TypeId, registry: &TypeRegistry) {
    let type_name_str = format_type(local_type, registry);
    let gir_name = gir_type_name(local_type, registry);

    if let Some(inner_name) = type_name_str.strip_prefix("Box__") {
        let is_trait_box = registry.get_type_def(&format!("{inner_name}_TraitObj")).is_some();
        if is_trait_box {
            let _ = writeln!(out, "        if ({place_str}.data != NULL) {{");
            let _ = writeln!(out, "            free({place_str}.data);");
            out.push_str("        }\n");
        } else {
            let _ = writeln!(out, "        if ({place_str} != NULL) {{");
            if let Some(inner_def) = registry.get_type_def(inner_name) {
                if let DropStrategy::Custom(ref fn_name) = inner_def.metadata.drop_strategy {
                    let _ = writeln!(out, "            {fn_name}({place_str});");
                }
            }
            let _ = writeln!(out, "            free({place_str});");
            out.push_str("        }\n");
        }
    } else if let Some(elem_name) = gir_name.as_deref().and_then(extract_vector_elem_name) {
        let _ = writeln!(out, "        if ({place_str}.data != NULL) {{");
        if needs_drop_by_name(elem_name, registry) {
            let elem_c_type = gir_to_c_type(elem_name);
            let _ = writeln!(out, "            for (size_t __di = 0; __di < {place_str}.len; __di++) {{");
            let _ = writeln!(
                out,
                "                {elem_c_type}* __de = ({elem_c_type}*)gorget_array_get(&{place_str}, __di);"
            );
            emit_drop_for_type_via_ptr(out, "__de", elem_name, registry, "                ", 1);
            let _ = writeln!(out, "            }}");
        }
        let _ = writeln!(out, "            gorget_array_free(&{place_str});");
        out.push_str("        }\n");
    } else {
        let strategy = lookup_drop_strategy(local_type, registry);
        match strategy {
            DropStrategy::None => {}
            DropStrategy::Trivial(ref fn_name) => {
                let _ = writeln!(out, "        if (memcmp(&{place_str}, &({type_name_str}){{0}}, sizeof({type_name_str})) != 0) {{");
                let _ = writeln!(out, "            {fn_name}(&{place_str});");
                out.push_str("        }\n");
            }
            DropStrategy::Custom(ref fn_name) => {
                let _ = writeln!(out, "        if (memcmp(&{place_str}, &({type_name_str}){{0}}, sizeof({type_name_str})) != 0) {{");
                let _ = writeln!(out, "            {fn_name}(&{place_str});");
                emit_field_drops(out, place_str, local_type, registry, "            ", 0);
                out.push_str("        }\n");
            }
            DropStrategy::Recursive => {
                let _ = writeln!(out, "        if (memcmp(&{place_str}, &({type_name_str}){{0}}, sizeof({type_name_str})) != 0) {{");
                emit_field_drops(out, place_str, local_type, registry, "            ", 0);
                out.push_str("        }\n");
            }
        }
    }
}

/// Emit one GIR instruction in the poll function context (all locals accessed via frame fields).
/// Returns `Some(task_local_id)` if this is an await call (caller should NOT emit the await
/// call itself — it emits the waker-check block and the post-await resume state instead).
fn emit_poll_inst(
    out: &mut String,
    inst: &Instruction,
    func: &Function,
    registry: &TypeRegistry,
    overflow_wrap: bool,
    type_overrides: &std::collections::HashMap<usize, String>,
    _module: &Module,
) -> Option<u32> {
    match inst {
        Instruction::Nop => {}

        Instruction::GlobalAssign { name, value } => {
            if !matches!(value, Operand::Constant(Constant::Unit)) {
                let val_str = fmt_operand_poll(value, func, registry);
                let _ = writeln!(out, "        {name} = {val_str};");
            }
        }

        Instruction::Assign { dst, value } => {
            if matches!(value, Operand::Constant(Constant::Unit)) {
                // Skip unit assignments (void destination)
                return None;
            }
            let dst_str = fmt_place_poll(dst, func, registry);
            if let Operand::Constant(Constant::Str(s)) = value {
                let local_type = effective_c_type(dst.local.0 as usize, func, registry, type_overrides);
                let escaped = escape_c_string(s);
                if local_type == "Str" {
                    let _ = writeln!(out, "        {dst_str} = gorget_str_from_literal(\"{escaped}\", {});", s.len());
                    return None;
                }
                if local_type == "GorgetString" {
                    let _ = writeln!(out, "        {dst_str} = gorget_string_new(\"{escaped}\");");
                    return None;
                }
            }
            // When assigning Null to an enum-typed local (e.g., Option None), emit tagged struct
            if matches!(value, Operand::Constant(Constant::Null)) {
                let local_type = func.locals[dst.local.0 as usize].type_id;
                if let Some(GirType::Named(name)) = registry.get(local_type) {
                    if let Some(type_def) = registry.get_type_def(name) {
                        if let TypeDefKind::Enum(ref e) = type_def.kind {
                            let none_tag = e.variants.iter().position(|v| v.name == "None")
                                .unwrap_or(e.variants.len() - 1);
                            let _ = writeln!(out, "        {dst_str} = ({name}){{.tag = {none_tag}}};");
                            return None;
                        }
                    }
                }
            }
            let val_str = fmt_operand_poll(value, func, registry);
            // Type coercion: GorgetString → Str via compound literal
            let dst_type = effective_c_type(dst.local.0 as usize, func, registry, type_overrides);
            if dst_type == "Str" {
                if let Operand::Copy(p) | Operand::Move(p) = value {
                    let src_type = effective_c_type(p.local.0 as usize, func, registry, type_overrides);
                    if src_type == "GorgetString" {
                        let _ = writeln!(out, "        {dst_str} = (Str){{ .data = {val_str}.data, .len = {val_str}.len }};");
                        return None;
                    }
                }
            }
            // Pointer→value coercion: dereference when assigning Ptr(T) to T
            // (e.g., `_0 = _1` where _1 is `const Quat*` and _0 is `Quat`)
            if let Operand::Copy(src_place) | Operand::Move(src_place) = value {
                if src_place.projections.is_empty() {
                    let src_idx = src_place.local.0 as usize;
                    let dst_idx = dst.local.0 as usize;
                    if src_idx < func.locals.len() && dst_idx < func.locals.len() {
                        let src_tid = func.locals[src_idx].type_id;
                        let dst_tid = func.locals[dst_idx].type_id;
                        let src_is_ptr = matches!(
                            registry.get(src_tid),
                            Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))
                        );
                        let dst_is_ptr = matches!(
                            registry.get(dst_tid),
                            Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))
                        );
                        if src_is_ptr && !dst_is_ptr && dst_tid != UNIT_TYPE {
                            let _ = writeln!(out, "        {dst_str} = (*{val_str});");
                            return None;
                        }
                    }
                }
            }
            let _ = writeln!(out, "        {dst_str} = {val_str};");
        }

        Instruction::BinOp { dst, op, type_id, lhs, rhs } => {
            if *type_id == UNIT_TYPE { return None; }
            let c_type = format_type(*type_id, registry);
            let dst_str = fmt_place_poll(&Place::local(LocalId(dst.0)), func, registry);
            let lhs_str = fmt_operand_poll(lhs, func, registry);
            let rhs_str = fmt_operand_poll(rhs, func, registry);
            if *op == BinOp::Pow {
                let _ = writeln!(out, "        {dst_str} = ({c_type})pow((double){lhs_str}, (double){rhs_str});");
            } else if matches!(op, BinOp::AddWrap | BinOp::SubWrap | BinOp::MulWrap) {
                let sym = match op { BinOp::AddWrap => "+", BinOp::SubWrap => "-", _ => "*" };
                let _ = writeln!(out, "        {dst_str} = ({c_type})((uint64_t){lhs_str} {sym} (uint64_t){rhs_str});");
            } else if matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul)
                && c_type == "int64_t" && !overflow_wrap
            {
                let builtin = match op {
                    BinOp::Add => "__builtin_add_overflow",
                    BinOp::Sub => "__builtin_sub_overflow",
                    _ => "__builtin_mul_overflow",
                };
                let _ = writeln!(out, "        if ({builtin}({lhs_str}, {rhs_str}, &{dst_str})) {{ fprintf(stderr, \"gorget: integer overflow\\n\"); exit(1); }}");
            } else if *op == BinOp::Mod && (c_type == "int64_t" || c_type == "int32_t") {
                let _ = writeln!(out, "        if ({rhs_str} == 0) {{ fprintf(stderr, \"gorget: division by zero\\n\"); exit(1); }}");
                let _ = writeln!(out, "        {{ int64_t __rem = {lhs_str} % {rhs_str}; {dst_str} = (__rem != 0 && ((__rem ^ {rhs_str}) < 0)) ? __rem + {rhs_str} : __rem; }}");
            } else if matches!(op, BinOp::Div | BinOp::Rem) && (c_type == "int64_t" || c_type == "int32_t") {
                let _ = writeln!(out, "        if ({rhs_str} == 0) {{ fprintf(stderr, \"gorget: division by zero\\n\"); exit(1); }}");
                let op_str = format_binop(*op);
                let _ = writeln!(out, "        {dst_str} = {lhs_str} {op_str} {rhs_str};");
            } else {
                let op_str = format_binop(*op);
                let _ = writeln!(out, "        {dst_str} = {lhs_str} {op_str} {rhs_str};");
            }
        }

        Instruction::UnOp { dst, op, operand, .. } => {
            let dst_str = fmt_place_poll(&Place::local(LocalId(dst.0)), func, registry);
            let val_str = fmt_operand_poll(operand, func, registry);
            let sym = match op { UnOp::Neg => "-", UnOp::Not => "!", UnOp::BitNot => "~" };
            let _ = writeln!(out, "        {dst_str} = {sym}{val_str};");
        }

        Instruction::Cmp { dst, op, lhs, rhs, .. } => {
            let dst_str = fmt_place_poll(&Place::local(LocalId(dst.0)), func, registry);
            let lhs_str = fmt_operand_poll(lhs, func, registry);
            let rhs_str = fmt_operand_poll(rhs, func, registry);
            let op_str = format_cmpop(*op);
            let _ = writeln!(out, "        {dst_str} = {lhs_str} {op_str} {rhs_str};");
        }

        Instruction::Cast { dst, target_type, value }
        | Instruction::BitCast { dst, target_type, value }
        | Instruction::PtrCast { dst, target_type, value } => {
            let dst_str = fmt_place_poll(&Place::local(LocalId(dst.0)), func, registry);
            let c_type = format_type(*target_type, registry);
            let val_str = fmt_operand_poll(value, func, registry);
            let _ = writeln!(out, "        {dst_str} = ({c_type}){val_str};");
        }

        Instruction::Call { dst, func: call_fn, args } => {
            // Await call: return the task-handle local to the caller; the caller emits
            // the waker-check block and the actual await call in the resume state.
            if call_fn.starts_with("__gorget_await_") {
                if let Some(Operand::Copy(p) | Operand::Move(p)) = args.first() {
                    return Some(p.local.0);
                }
            }
            // Option/Result unwrap: inline field access
            if (call_fn.starts_with("__result_unwrap") || call_fn.starts_with("__option_unwrap"))
                && !args.is_empty()
            {
                if let Some(dst_id) = dst {
                    let ptr = fmt_operand_poll(&args[0], func, registry);
                    let variant = if call_fn.starts_with("__result") { "Ok" } else { "Some" };
                    if call_fn.ends_with("_or") && args.len() > 1 {
                        let default_val = fmt_operand_poll(&args[1], func, registry);
                        let _ = writeln!(out, "        f->_{id} = (({ptr})->tag == 0) ? ({ptr})->data.{variant}._0 : {default_val};",
                            id = dst_id.0);
                    } else {
                        let _ = writeln!(out, "        f->_{id} = ({ptr})->data.{variant}._0;", id = dst_id.0);
                    }
                }
                return None;
            }
            // Option is_some/is_none: inline tag check
            if call_fn == "__option_is_some" || call_fn == "__option_is_none" {
                if let Some(dst_id) = dst {
                    if let Some(arg) = args.first() {
                        let ptr = fmt_operand_poll(arg, func, registry);
                        let check = if call_fn == "__option_is_some" { "== 0" } else { "!= 0" };
                        let _ = writeln!(out, "        f->_{} = ({ptr})->tag {check};", dst_id.0);
                    }
                }
                return None;
            }
            // Map GIR stdlib names to C runtime names
            let c_fn_mapped = map_stdlib_name(call_fn);
            let was_mapped = c_fn_mapped != call_fn;
            // ── Collection constructor: Vector__T__new(), Dict__K__V__new(), etc.
            if !was_mapped && is_collection_constructor(call_fn) {
                if let Some(dst_id) = dst {
                    let poll_dst = format!("f->_{}", dst_id.0);
                    if let Some(code) = emit_collection_constructor_to(call_fn, &poll_dst) {
                        out.push_str(&code);
                    } else {
                        let type_name = call_fn.strip_suffix("__new").unwrap_or(call_fn);
                        let ct = collection_type_alias(type_name).unwrap_or(type_name);
                        let _ = writeln!(out, "        {poll_dst} = ({ct}){{0}};");
                    }
                }
                return None;
            }
            // ── Inline methods in poll context: pop, sort, sorted, reversed, unique
            if !was_mapped {
                if let Some(inline) = try_inline_method(call_fn) {
                    emit_poll_inline_method(out, &inline, dst, args, func, registry, call_fn, type_overrides);
                    return None;
                }
            }
            // ── Higher-order collection methods in poll context: filter, map, fold, any, all, etc.
            if !was_mapped {
                if let Some(code) = try_emit_poll_higher_order_method(call_fn, dst, args, func, registry, type_overrides, _module) {
                    out.push_str(&code);
                    return None;
                }
            }
            // Collection method rewriting (Str__starts_with → gorget_str_starts_with, etc.)
            if !was_mapped {
                if let Some(rewrite) = try_rewrite_collection_method(call_fn) {
                    // Handle inline pseudo-functions that need special codegen
                    // Handle __INLINE_* pseudo-functions in poll context
                    if rewrite.runtime_fn.starts_with("__INLINE_") {
                        let self_str = if !args.is_empty() { fmt_operand_poll(&args[0], func, registry) } else { String::new() };
                        let is_ptr = if let Some(Operand::Copy(p) | Operand::Move(p)) = args.first() {
                            effective_c_type(p.local.0 as usize, func, registry, type_overrides).ends_with('*')
                        } else { false };
                        let self_val = if is_ptr { format!("(*{self_str})") } else { self_str.clone() };
                        let self_addr = if is_ptr { self_str.clone() } else { format!("&{self_str}") };

                        match rewrite.runtime_fn {
                            "__INLINE_MAP_GET__" => {
                                // Dict/HashMap.get(key) → Option[V] with NULL check
                                if let Some(dst_id) = dst {
                                    let option_type = type_overrides.get(&(dst_id.0 as usize))
                                        .cloned()
                                        .unwrap_or_else(|| format_type(func.locals[dst_id.0 as usize].type_id, registry));
                                    let elem_c_type = option_type.strip_prefix("Option__").unwrap_or("int64_t");
                                    let key_arg = if args.len() > 1 {
                                        let val = fmt_operand_poll(&args[1], func, registry);
                                        match &args[1] {
                                            Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => format!("&{val}"),
                                            Operand::Constant(Constant::Str(s)) => {
                                                format!("&(Str){{ .data = \"{}\", .len = {} }}", escape_c_string(s), s.len())
                                            }
                                            _ => {
                                                let arg_type = match &args[1] {
                                                    Operand::Copy(p) | Operand::Move(p) => {
                                                        let idx = p.local.0 as usize;
                                                        if idx < func.locals.len() { format_type(func.locals[idx].type_id, registry) } else { "int64_t".to_string() }
                                                    }
                                                    Operand::Constant(c) => match c {
                                                        Constant::I64(_) => "int64_t".to_string(),
                                                        Constant::F64(_) => "double".to_string(),
                                                        Constant::Bool(_) => "bool".to_string(),
                                                        _ => "int64_t".to_string(),
                                                    },
                                                };
                                                format!("&({arg_type}){{{val}}}")
                                            }
                                        }
                                    } else { "NULL".to_string() };
                                    let _ = writeln!(out,
                                        "        f->_{id} = ({{ void* __mv = gorget_map_get(&{self_val}, {key_arg}); \
                                        {option_type} __mr; if (__mv != NULL) {{ __mr = ({option_type}){{.tag = 0, .data.Some = {{*({elem_c_type}*)__mv}}}}; }} \
                                        else {{ __mr = ({option_type}){{.tag = 1}}; }} __mr; }});",
                                        id = dst_id.0);
                                }
                            }
                            "__INLINE_ARRAY_GET__" => {
                                // Vector.get(idx) → Option[T] with bounds check
                                let idx_str = if args.len() > 1 { fmt_operand_poll(&args[1], func, registry) } else { "0".to_string() };
                                if let Some(dst_id) = dst {
                                    let option_type = format_type(func.locals[dst_id.0 as usize].type_id, registry);
                                    let inner_type_str = option_type.strip_prefix("Option__").unwrap_or("");
                                    let elem_c_type =
                                        if inner_type_str.starts_with("Vector__") || inner_type_str.starts_with("List__") { "GorgetArray" }
                                        else if inner_type_str.starts_with("Dict__") || inner_type_str.starts_with("HashMap__") { "GorgetMap" }
                                        else if inner_type_str.starts_with("Set__") || inner_type_str.starts_with("HashSet__") { "GorgetSet" }
                                        else if !inner_type_str.is_empty() { inner_type_str }
                                        else { extract_collection_elem_type(call_fn) };
                                    let _ = writeln!(out, "        f->_{id} = ({{ int64_t __gi = {idx_str}; GorgetArray __gr_src = {self_val}; {option_type} __gr; \
                                        if (__gi >= 0 && (size_t)__gi < __gr_src.len) {{ __gr = ({option_type}){{.tag = 0, .data.Some = {{*({elem_c_type}*)gorget_array_get(&__gr_src, (size_t)__gi)}}}}; }} \
                                        else {{ __gr = ({option_type}){{.tag = 1}}; }} __gr; }});",
                                        id = dst_id.0);
                                }
                            }
                            "__INLINE_ARRAY_FIRST__" | "__INLINE_ARRAY_LAST__" => {
                                let is_last = rewrite.runtime_fn == "__INLINE_ARRAY_LAST__";
                                if let Some(dst_id) = dst {
                                    let option_type = format_type(func.locals[dst_id.0 as usize].type_id, registry);
                                    let inner_type_str = option_type.strip_prefix("Option__").unwrap_or("");
                                    let elem_c_type =
                                        if inner_type_str.starts_with("Vector__") || inner_type_str.starts_with("List__") { "GorgetArray" }
                                        else if inner_type_str.starts_with("Dict__") || inner_type_str.starts_with("HashMap__") { "GorgetMap" }
                                        else if inner_type_str.starts_with("Set__") || inner_type_str.starts_with("HashSet__") { "GorgetSet" }
                                        else if !inner_type_str.is_empty() { inner_type_str }
                                        else { extract_collection_elem_type(call_fn) };
                                    let idx_expr = if is_last { format!("(int64_t)({self_val}.len - 1)") } else { "0".to_string() };
                                    let _ = writeln!(out, "        f->_{id} = ({{ GorgetArray __gr_src = {self_val}; {option_type} __gr; \
                                        if (__gr_src.len > 0) {{ __gr = ({option_type}){{.tag = 0, .data.Some = {{*({elem_c_type}*)gorget_array_get(&__gr_src, {idx_expr})}}}}; }} \
                                        else {{ __gr = ({option_type}){{.tag = 1}}; }} __gr; }});",
                                        id = dst_id.0);
                                }
                            }
                            "__INLINE_ARRAY_REMOVE__" => {
                                let idx_str = if args.len() > 1 { fmt_operand_poll(&args[1], func, registry) } else { "0".to_string() };
                                if let Some(dst_id) = dst {
                                    let option_type = format_type(func.locals[dst_id.0 as usize].type_id, registry);
                                    let inner_type_str = option_type.strip_prefix("Option__").unwrap_or("");
                                    let elem_c_type =
                                        if inner_type_str.starts_with("Vector__") || inner_type_str.starts_with("List__") { "GorgetArray" }
                                        else if inner_type_str.starts_with("Dict__") || inner_type_str.starts_with("HashMap__") { "GorgetMap" }
                                        else if inner_type_str.starts_with("Set__") || inner_type_str.starts_with("HashSet__") { "GorgetSet" }
                                        else if !inner_type_str.is_empty() { inner_type_str }
                                        else { extract_collection_elem_type(call_fn) };
                                    let _ = writeln!(out, "        f->_{id} = ({{ int64_t __gi = {idx_str}; GorgetArray __gr_src = {self_val}; {option_type} __gr; \
                                        if (__gi >= 0 && (size_t)__gi < __gr_src.len) {{ {elem_c_type} __elem = *({elem_c_type}*)gorget_array_get(&__gr_src, (size_t)__gi); \
                                        gorget_array_remove({self_addr}, (size_t)__gi); \
                                        __gr = ({option_type}){{.tag = 0, .data.Some = {{__elem}}}}; }} \
                                        else {{ __gr = ({option_type}){{.tag = 1}}; }} __gr; }});",
                                        id = dst_id.0);
                                } else {
                                    let _ = writeln!(out, "        gorget_array_remove({self_addr}, {idx_str});");
                                }
                            }
                            "__INLINE_STRING_TO_STR__" => {
                                if let Some(dst_id) = dst {
                                    let _ = writeln!(out, "        f->_{id} = (Str){{ .data = {self_val}.data, .len = {self_val}.len }};", id = dst_id.0);
                                }
                            }
                            "__INLINE_STRING_CLEAR__" => {
                                if is_ptr {
                                    let _ = writeln!(out, "        {self_str}->len = 0;");
                                } else {
                                    let _ = writeln!(out, "        {self_str}.len = 0;");
                                }
                            }
                            other => {
                                // Unknown inline — emit as comment for debugging
                                let _ = writeln!(out, "        /* TODO: unhandled poll inline {other} */");
                            }
                        }
                        return None;
                    }
                    if !rewrite.runtime_fn.is_empty() {
                        // Emit collection method call in poll context
                        let mut arg_parts: Vec<String> = Vec::new();
                        for (i, arg) in args.iter().enumerate() {
                            if i == 0 {
                                // Self argument: pass by pointer or value
                                let self_str = fmt_operand_poll(arg, func, registry);
                                let is_ptr = if let Operand::Copy(p) | Operand::Move(p) = arg {
                                    let ct = effective_c_type(p.local.0 as usize, func, registry, type_overrides);
                                    ct.ends_with('*')
                                } else { false };
                                if rewrite.pass_by_ptr {
                                    // Runtime fn expects a pointer; take address if self is a value
                                    if is_ptr {
                                        arg_parts.push(self_str);
                                    } else {
                                        arg_parts.push(format!("&{self_str}"));
                                    }
                                } else {
                                    // Runtime fn expects value; deref if self is a pointer
                                    if is_ptr {
                                        arg_parts.push(format!("(*{self_str})"));
                                    } else {
                                        arg_parts.push(self_str);
                                    }
                                }
                            } else {
                                // Non-self args: if pass_by_ptr, wrap in compound literal for void* params
                                // slice takes self by ptr but remaining args as plain scalars
                                let val = fmt_operand_poll(arg, func, registry);
                                let scalar_args = rewrite.runtime_fn == "gorget_array_slice";
                                let should_ptr = rewrite.pass_by_ptr && !scalar_args;
                                if should_ptr {
                                    match arg {
                                        Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
                                            arg_parts.push(format!("&{val}"));
                                        }
                                        _ => {
                                            // Constant or projected — use compound literal
                                            let arg_type = match arg {
                                                Operand::Copy(p) | Operand::Move(p) => {
                                                    let idx = p.local.0 as usize;
                                                    if idx < func.locals.len() {
                                                        format_type(func.locals[idx].type_id, registry)
                                                    } else { "int64_t".to_string() }
                                                }
                                                Operand::Constant(c) => match c {
                                                    Constant::I64(_) => "int64_t".to_string(),
                                                    Constant::I32(_) => "int32_t".to_string(),
                                                    Constant::F64(_) => "double".to_string(),
                                                    Constant::Bool(_) => "bool".to_string(),
                                                    Constant::Str(_) => "Str".to_string(),
                                                    _ => "int64_t".to_string(),
                                                },
                                            };
                                            if let Operand::Constant(Constant::Str(s)) = arg {
                                                let escaped = escape_c_string(s);
                                                arg_parts.push(format!("&(Str){{ .data = \"{escaped}\", .len = {} }}", s.len()));
                                            } else if matches!(arg, Operand::Constant(Constant::Null)) {
                                                if let Some(elem_type) = collection_elem_type_from_name(call_fn) {
                                                    if elem_type.starts_with("Option__") {
                                                        arg_parts.push(format!("&({elem_type}){{.tag = 1}}"));
                                                    } else {
                                                        arg_parts.push(format!("&({elem_type}){{0}}"));
                                                    }
                                                } else {
                                                    arg_parts.push(format!("&({arg_type}){{{val}}}"));
                                                }
                                            } else {
                                                arg_parts.push(format!("&({arg_type}){{{val}}}"));
                                            }
                                        }
                                    }
                                } else {
                                    arg_parts.push(fmt_operand_poll_as_str(arg, func, registry));
                                }
                            }
                        }
                        // Append sizeof(element) for contains/index_of
                        if rewrite.needs_elem_size {
                            let elem_c_type = extract_collection_elem_type(call_fn);
                            arg_parts.push(format!("sizeof({elem_c_type})"));
                        }
                        let args_str = arg_parts.join(", ");
                        if let Some(ref fa) = rewrite.field_access {
                            // field_access methods (len, is_empty): emit inline
                            if let Some(dst_id) = dst {
                                let self_str = if !args.is_empty() {
                                    fmt_operand_poll(&args[0], func, registry)
                                } else { String::new() };
                                let is_ptr = if let Some(Operand::Copy(p) | Operand::Move(p)) = args.first() {
                                    let ct = effective_c_type(p.local.0 as usize, func, registry, type_overrides);
                                    ct.ends_with('*')
                                } else { false };
                                let deref = if is_ptr { format!("(*{self_str})") } else { self_str };
                                let _ = writeln!(out, "        f->_{} = {deref}.{fa};", dst_id.0);
                            }
                        } else if rewrite.has_return {
                            if let Some(dst_id) = dst {
                                let _ = writeln!(out, "        f->_{} = {}({args_str});", dst_id.0, rewrite.runtime_fn);
                            }
                        } else {
                            let _ = writeln!(out, "        {}({args_str});", rewrite.runtime_fn);
                        }
                        return None;
                    }
                }
            }
            let c_fn = c_fn_mapped;
            // Blocking calls: wrap with enter/exit to keep pool at capacity
            let is_blocking = is_blocking_call(call_fn) || is_blocking_call(c_fn);
            if is_blocking {
                out.push_str("        __gorget_blocking_enter();\n");
            }
            // Mapped C functions (or already-C-named functions) expect const char*,
            // unmapped Gorget functions expect Str.
            let needs_cstr = was_mapped || c_fn.starts_with("gorget_");
            let args_str = {
                let mut parts: Vec<String> = if needs_cstr {
                    args.iter()
                        .map(|a| {
                            if let Operand::Constant(Constant::Str(s)) = a {
                                let escaped = escape_c_string(s);
                                return format!("\"{escaped}\"");
                            }
                            let base = fmt_operand_poll(a, func, registry);
                            match a {
                                Operand::Copy(p) | Operand::Move(p) => {
                                    let local_idx = p.local.0 as usize;
                                    if local_idx < func.locals.len() {
                                        let c_type = format_type(func.locals[local_idx].type_id, registry);
                                        if c_type == "Str" {
                                            return format!("gorget_str_to_cstr({base})");
                                        }
                                        if c_type == "GorgetString" {
                                            return format!("{base}.data");
                                        }
                                    }
                                    base
                                }
                                _ => base,
                            }
                        })
                        .collect()
                } else {
                    args.iter()
                        .map(|a| fmt_operand_poll_as_str(a, func, registry))
                        .collect()
                };
                // Channel auto-deref: Channel__* wrappers expect self by value
                // (Channel__T = GorgetChannel*), but in poll frames the local is
                // stored as Channel__T* (= GorgetChannel**).  Deref arg 0.
                if c_fn.starts_with("Channel__") && !parts.is_empty() {
                    if let Some(Operand::Copy(p) | Operand::Move(p)) = args.first() {
                        let local_idx = p.local.0 as usize;
                        if local_idx < func.locals.len() {
                            let c_type = format_type(func.locals[local_idx].type_id, registry);
                            if c_type.starts_with("Channel__") && c_type.ends_with('*') {
                                parts[0] = format!("(*{})", parts[0]);
                            }
                        }
                    }
                }
                parts.join(", ")
            };
            // Result-wrapping: if destination is Result__* and function has last_error_fn,
            // emit compound-literal wrapping (same pattern as try_emit_result_wrapped_call).
            let mut emitted_result_wrap = false;
            if let Some(dst_id) = dst {
                let dst_c = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                if dst_c.starts_with("Result__") {
                    if let Some(err_fn) = last_error_fn(call_fn).or_else(|| last_error_fn(c_fn)) {
                        let id = dst_id.0;
                        let ret_cstr = returns_cstr(c_fn);
                        let raw_capture = if ret_cstr {
                            format!("gorget_str_from_cstr({c_fn}({args_str}))")
                        } else {
                            format!("{c_fn}({args_str})")
                        };
                        let _ = writeln!(out,
                            "        f->_{id} = ({{ __typeof__(f->_{id}.data.Ok._0) __raw = {raw_capture}; \
                            const char* __err = {err_fn}(); \
                            {dst_c} __wr; if (__err) {{ __wr.tag = 1; __wr.data.Error._0 = gorget_str_from_cstr(__err); }} \
                            else {{ __wr.tag = 0; __wr.data.Ok._0 = __raw; }} __wr; }});");
                        emitted_result_wrap = true;
                    }
                }
            }
            if !emitted_result_wrap {
                if let Some(dst_id) = dst {
                    let dst_str = fmt_place_poll(&Place::local(LocalId(dst_id.0)), func, registry);
                    let _ = writeln!(out, "        {dst_str} = {c_fn}({args_str});");
                } else {
                    let _ = writeln!(out, "        {c_fn}({args_str});");
                }
            }
            if is_blocking {
                out.push_str("        __gorget_blocking_exit();\n");
            }
        }

        Instruction::CallExtern { dst, func: call_fn, args } => {
            // Option/Result unwrap: inline field access
            if (call_fn.starts_with("__result_unwrap") || call_fn.starts_with("__option_unwrap"))
                && !args.is_empty()
            {
                if let Some(dst_id) = dst {
                    let ptr = fmt_operand_poll(&args[0], func, registry);
                    let variant = if call_fn.starts_with("__result") { "Ok" } else { "Some" };
                    if call_fn.ends_with("_or") && args.len() > 1 {
                        let default_val = fmt_operand_poll(&args[1], func, registry);
                        let _ = writeln!(out, "        f->_{id} = (({ptr})->tag == 0) ? ({ptr})->data.{variant}._0 : {default_val};",
                            id = dst_id.0);
                    } else {
                        let _ = writeln!(out, "        f->_{id} = ({ptr})->data.{variant}._0;", id = dst_id.0);
                    }
                }
                return None;
            }
            // Option is_some/is_none: inline tag check
            if call_fn == "__option_is_some" || call_fn == "__option_is_none" {
                if let Some(dst_id) = dst {
                    if let Some(arg) = args.first() {
                        let ptr = fmt_operand_poll(arg, func, registry);
                        let check = if call_fn == "__option_is_some" { "== 0" } else { "!= 0" };
                        let _ = writeln!(out, "        f->_{} = ({ptr})->tag {check};", dst_id.0);
                    }
                }
                return None;
            }
            // ── Collection constructor in poll context (CallExtern)
            if is_collection_constructor(call_fn) {
                if let Some(dst_id) = dst {
                    let poll_dst = format!("f->_{}", dst_id.0);
                    if let Some(code) = emit_collection_constructor_to(call_fn, &poll_dst) {
                        out.push_str(&code);
                    } else {
                        let type_name = call_fn.strip_suffix("__new").unwrap_or(call_fn);
                        let ct = collection_type_alias(type_name).unwrap_or(type_name);
                        let _ = writeln!(out, "        {poll_dst} = ({ct}){{0}};");
                    }
                }
                return None;
            }
            // ── Inline methods in poll context (CallExtern)
            if let Some(inline) = try_inline_method(call_fn) {
                emit_poll_inline_method(out, &inline, dst, args, func, registry, call_fn, type_overrides);
                return None;
            }
            // ── Higher-order collection methods in poll context (CallExtern)
            if let Some(code) = try_emit_poll_higher_order_method(call_fn, dst, args, func, registry, type_overrides, _module) {
                out.push_str(&code);
                return None;
            }
            // ── Collection method rewriting in poll context (CallExtern)
            if let Some(rewrite) = try_rewrite_collection_method(call_fn) {
                if !rewrite.runtime_fn.is_empty() {
                    let mut arg_parts: Vec<String> = Vec::new();
                    for (i, arg) in args.iter().enumerate() {
                        if i == 0 {
                            let self_str = fmt_operand_poll(arg, func, registry);
                            let is_ptr = if let Operand::Copy(p) | Operand::Move(p) = arg {
                                let ct = effective_c_type(p.local.0 as usize, func, registry, type_overrides);
                                ct.ends_with('*')
                            } else { false };
                            if rewrite.pass_by_ptr {
                                if is_ptr { arg_parts.push(self_str); }
                                else { arg_parts.push(format!("&{self_str}")); }
                            } else {
                                if is_ptr { arg_parts.push(format!("(*{self_str})")); }
                                else { arg_parts.push(self_str); }
                            }
                        } else {
                            let val = fmt_operand_poll(arg, func, registry);
                            if rewrite.pass_by_ptr {
                                match arg {
                                    Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
                                        arg_parts.push(format!("&{val}"));
                                    }
                                    _ => {
                                        let arg_type = match arg {
                                            Operand::Copy(p) | Operand::Move(p) => {
                                                let idx = p.local.0 as usize;
                                                if idx < func.locals.len() { format_type(func.locals[idx].type_id, registry) }
                                                else { "int64_t".to_string() }
                                            }
                                            Operand::Constant(c) => match c {
                                                Constant::I64(_) => "int64_t".to_string(),
                                                Constant::I32(_) => "int32_t".to_string(),
                                                Constant::F64(_) => "double".to_string(),
                                                Constant::Bool(_) => "bool".to_string(),
                                                Constant::Str(_) => "Str".to_string(),
                                                _ => "int64_t".to_string(),
                                            },
                                        };
                                        if let Operand::Constant(Constant::Str(s)) = arg {
                                            let escaped = escape_c_string(s);
                                            arg_parts.push(format!("&(Str){{ .data = \"{escaped}\", .len = {} }}", s.len()));
                                        } else {
                                            arg_parts.push(format!("&({arg_type}){{{val}}}"));
                                        }
                                    }
                                }
                            } else {
                                arg_parts.push(fmt_operand_poll_as_str(arg, func, registry));
                            }
                        }
                    }
                    if rewrite.needs_elem_size {
                        let elem_c_type = extract_collection_elem_type(call_fn);
                        arg_parts.push(format!("sizeof({elem_c_type})"));
                    }
                    let args_str = arg_parts.join(", ");
                    if let Some(ref fa) = rewrite.field_access {
                        if let Some(dst_id) = dst {
                            let self_str = if !args.is_empty() {
                                fmt_operand_poll(&args[0], func, registry)
                            } else { String::new() };
                            let is_ptr = if let Some(Operand::Copy(p) | Operand::Move(p)) = args.first() {
                                let ct = effective_c_type(p.local.0 as usize, func, registry, type_overrides);
                                ct.ends_with('*')
                            } else { false };
                            let deref = if is_ptr { format!("(*{self_str})") } else { self_str };
                            let _ = writeln!(out, "        f->_{} = {deref}.{fa};", dst_id.0);
                        }
                    } else if rewrite.has_return {
                        if let Some(dst_id) = dst {
                            let _ = writeln!(out, "        f->_{} = {}({args_str});", dst_id.0, rewrite.runtime_fn);
                        }
                    } else {
                        let _ = writeln!(out, "        {}({args_str});", rewrite.runtime_fn);
                    }
                    return None;
                }
            }
            let is_printf = call_fn == "printf" || call_fn == "fprintf" || call_fn == "sprintf"
                || call_fn == "gorget_string_format";
            let args_str = if is_printf {
                fmt_printf_args_poll(args, func, registry)
            } else {
                fmt_args_poll(args, func, registry)
            };
            // Result wrapping for extern calls (socket_connect, etc.)
            let mut emitted_result_wrap = false;
            if let Some(dst_id) = dst {
                let dst_c = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                if dst_c.starts_with("Result__") {
                    if let Some(err_fn) = last_error_fn(call_fn) {
                        let id = dst_id.0;
                        let c_fn = call_fn;
                        let ret_cstr = returns_cstr(c_fn);
                        let raw_capture = if ret_cstr {
                            format!("gorget_str_from_cstr({c_fn}({args_str}))")
                        } else {
                            format!("{c_fn}({args_str})")
                        };
                        let _ = writeln!(out,
                            "        f->_{id} = ({{ __typeof__(f->_{id}.data.Ok._0) __raw = {raw_capture}; \
                            const char* __err = {err_fn}(); \
                            {dst_c} __wr; if (__err) {{ __wr.tag = 1; __wr.data.Error._0 = gorget_str_from_cstr(__err); }} \
                            else {{ __wr.tag = 0; __wr.data.Ok._0 = __raw; }} __wr; }});");
                        emitted_result_wrap = true;
                    }
                }
            }
            if !emitted_result_wrap {
                if let Some(dst_id) = dst {
                    let dst_str = fmt_place_poll(&Place::local(LocalId(dst_id.0)), func, registry);
                    let _ = writeln!(out, "        {dst_str} = {call_fn}({args_str});");
                } else {
                    let _ = writeln!(out, "        {call_fn}({args_str});");
                }
            }
        }

        Instruction::CallIndirect { dst, callee, args } => {
            let callee_str = fmt_operand_poll(callee, func, registry);
            let args_str = fmt_args_poll(args, func, registry);
            if let Some(dst_id) = dst {
                let local_type = func.locals[dst_id.0 as usize].type_id;
                if local_type == UNIT_TYPE {
                    let _ = writeln!(out, "        {callee_str}({args_str});");
                } else {
                    let dst_str = fmt_place_poll(&Place::local(LocalId(dst_id.0)), func, registry);
                    let _ = writeln!(out, "        {dst_str} = {callee_str}({args_str});");
                }
            } else {
                let _ = writeln!(out, "        {callee_str}({args_str});");
            }
        }

        Instruction::MoveZero { place } => {
            let place_str = fmt_place_poll(place, func, registry);
            let local_type = func.locals[place.local.0 as usize].type_id;
            let type_name = format_type(local_type, registry);
            let _ = writeln!(out, "        memset(&{place_str}, 0, sizeof({type_name}));");
        }

        Instruction::Borrow { dst, place } | Instruction::BorrowMut { dst, place } => {
            let dst_str = fmt_place_poll(&Place::local(LocalId(dst.0)), func, registry);
            let place_str = fmt_place_poll(place, func, registry);
            let _ = writeln!(out, "        {dst_str} = &{place_str};");
        }

        Instruction::FieldLoad { dst, base, field } => {
            let dst_str = fmt_place_poll(&Place::local(LocalId(dst.0)), func, registry);
            let base_str = fmt_place_poll(base, func, registry);
            let base_type = func.locals[base.local.0 as usize].type_id;
            let field_name = resolve_field_name_from_type(base_type, *field, registry)
                .map(|(n, _)| n)
                .unwrap_or_else(|| format!("_{field}"));
            let _ = writeln!(out, "        {dst_str} = {base_str}.{field_name};");
        }

        Instruction::IndexLoad { dst, base, index } => {
            let dst_str = fmt_place_poll(&Place::local(LocalId(dst.0)), func, registry);
            let base_str = fmt_place_poll(base, func, registry);
            let idx_str = fmt_operand_poll(index, func, registry);
            let base_idx = base.local.0 as usize;
            let base_type = if base_idx < func.locals.len() {
                format_type(func.locals[base_idx].type_id, registry)
            } else {
                "int64_t".to_string()
            };
            // Check if index is a Range (slice)
            let index_is_range = match index {
                Operand::Copy(p) | Operand::Move(p) => {
                    let ii = p.local.0 as usize;
                    ii < func.locals.len() && matches!(
                        registry.get(func.locals[ii].type_id),
                        Some(GirType::Named(n)) if n == "GorgetRange"
                    )
                }
                _ => false,
            };
            if base_type == "Str" {
                if index_is_range {
                    let _ = writeln!(out, "        {dst_str} = gorget_str_slice({base_str}, {idx_str}.start, {idx_str}.end);");
                } else {
                    let _ = writeln!(out, "        {dst_str} = gorget_str_index({base_str}, {idx_str});");
                }
            } else if base_type.starts_with("GorgetArray") || base_type.starts_with("Vector__") {
                if index_is_range {
                    let _ = writeln!(out, "        {dst_str} = gorget_array_slice(&{base_str}, {idx_str}.start, {idx_str}.end);");
                } else {
                    // Infer element type from dst local type or base type name
                    let dst_idx = dst.0 as usize;
                    let elem_c_type = if dst_idx < func.locals.len() {
                        format_type(func.locals[dst_idx].type_id, registry)
                    } else {
                        "int64_t".to_string()
                    };
                    // For cloneable collection types, deep-clone to allow repeated access
                    if elem_c_type == "GorgetArray" || elem_c_type.starts_with("Vector__") {
                        let _ = writeln!(out, "        {dst_str} = gorget_array_clone(({elem_c_type}*)gorget_array_get(&{base_str}, {idx_str}));");
                    } else if elem_c_type == "GorgetSet" || elem_c_type.starts_with("Set__") || elem_c_type.starts_with("HashSet__") {
                        let _ = writeln!(out, "        {dst_str} = gorget_set_clone(({elem_c_type}*)gorget_array_get(&{base_str}, {idx_str}));");
                    } else if elem_c_type == "GorgetMap" || elem_c_type.starts_with("Dict__") || elem_c_type.starts_with("HashMap__") {
                        let _ = writeln!(out, "        {dst_str} = gorget_map_clone(({elem_c_type}*)gorget_array_get(&{base_str}, {idx_str}));");
                    } else {
                        let _ = writeln!(out, "        {dst_str} = *({elem_c_type}*)gorget_array_get(&{base_str}, {idx_str});");
                        // For non-cloneable move types, zero to prevent double-free
                        if dst.0 < func.locals.len() as u32 && registry.is_resource_type(func.locals[dst.0 as usize].type_id) {
                            let mut clone_ops: Vec<String> = Vec::new();
                            collect_clone_ops(&elem_c_type, &format!("f->_{}", dst.0), &mut clone_ops, registry);
                            if !clone_ops.is_empty() {
                                for op in &clone_ops {
                                    let _ = writeln!(out, "        {op}");
                                }
                            } else {
                                let _ = writeln!(out, "        memset(({elem_c_type}*)gorget_array_get(&{base_str}, {idx_str}), 0, sizeof({elem_c_type}));");
                            }
                        }
                    }
                }
            } else if base_type.starts_with("GorgetDict") || base_type.starts_with("Dict__")
                || base_type.starts_with("GorgetMap") || base_type.starts_with("HashMap__") {
                let dst_idx = dst.0 as usize;
                let val_c_type = if dst_idx < func.locals.len() {
                    format_type(func.locals[dst_idx].type_id, registry)
                } else {
                    "int64_t".to_string()
                };
                let _ = writeln!(out, "        {dst_str} = *({val_c_type}*)gorget_map_get(&{base_str}, &({idx_str}));");
            } else {
                // Plain C array or pointer indexing
                let _ = writeln!(out, "        {dst_str} = {base_str}.data[{idx_str}];");
            }
        }

        Instruction::StructInit { dst, type_name, fields } => {
            let dst_str = fmt_place_poll(&Place::local(LocalId(dst.0)), func, registry);
            let field_strs = fields.iter()
                .map(|f| fmt_operand_poll(f, func, registry))
                .collect::<Vec<_>>()
                .join(", ");
            let _ = writeln!(out, "        {dst_str} = ({type_name}){{{field_strs}}};");
        }

        Instruction::TupleInit { dst, elements } => {
            let dst_str = fmt_place_poll(&Place::local(LocalId(dst.0)), func, registry);
            let elem_strs = elements.iter()
                .map(|e| fmt_operand_poll(e, func, registry))
                .collect::<Vec<_>>()
                .join(", ");
            let _ = writeln!(out, "        {{ typeof({dst_str}) __ti = {{{elem_strs}}}; {dst_str} = __ti; }}");
        }

        Instruction::EnumInit { dst, type_name, variant, fields } => {
            let dst_str = fmt_place_poll(&Place::local(LocalId(dst.0)), func, registry);
            let type_def = registry.get_type_def(type_name);
            let (tag, variant_fields) = if let Some(def) = type_def {
                if let TypeDefKind::Enum(ref e) = def.kind {
                    let idx = e.variants.iter().position(|v| v.name == *variant).unwrap_or(0);
                    let vf: Vec<_> = e.variants[idx].fields.clone();
                    (idx, vf)
                } else { (0, vec![]) }
            } else { (0, vec![]) };
            let all_unit = !fields.is_empty() && fields.iter().all(|f| matches!(f, Operand::Constant(Constant::Unit)));
            if fields.is_empty() || all_unit {
                let _ = writeln!(out, "        {dst_str} = ({type_name}){{.tag = {tag}}};");
            } else {
                let _ = write!(out, "        {dst_str} = ({type_name}){{.tag = {tag}, .data.{variant} = {{");
                for (i, field_val) in fields.iter().enumerate() {
                    if i > 0 { out.push_str(", "); }
                    // Handle Null fields: emit tagged None struct for enum-typed fields
                    let val = if matches!(field_val, Operand::Constant(Constant::Null)) && i < variant_fields.len() {
                        let ft = &variant_fields[i].type_id;
                        if let Some(GirType::Named(fname)) = registry.get(*ft) {
                            if let Some(ftd) = registry.get_type_def(fname) {
                                if let TypeDefKind::Enum(ref e) = ftd.kind {
                                    let none_tag = e.variants.iter().position(|v| v.name == "None")
                                        .unwrap_or(e.variants.len() - 1);
                                    format!("({fname}){{.tag = {none_tag}}}")
                                } else { fmt_operand_poll(field_val, func, registry) }
                            } else { fmt_operand_poll(field_val, func, registry) }
                        } else { fmt_operand_poll(field_val, func, registry) }
                    } else if i < variant_fields.len() {
                        // Coerce string literals to Str/GorgetString
                        if let Operand::Constant(Constant::Str(s)) = field_val {
                            let field_c_type = format_type(variant_fields[i].type_id, registry);
                            if field_c_type == "Str" {
                                format!("gorget_str_from_literal(\"{}\", {})", escape_c_string(s), s.len())
                            } else if field_c_type == "GorgetString" {
                                format!("gorget_string_new(\"{}\")", escape_c_string(s))
                            } else {
                                fmt_operand_poll(field_val, func, registry)
                            }
                        } else {
                            fmt_operand_poll(field_val, func, registry)
                        }
                    } else {
                        fmt_operand_poll(field_val, func, registry)
                    };
                    let _ = write!(out, "{val}");
                }
                out.push_str("}};\n");
            }
        }

        Instruction::TagOf { dst, operand } => {
            let dst_str = fmt_place_poll(&Place::local(LocalId(dst.0)), func, registry);
            let val_str = fmt_operand_poll(operand, func, registry);
            let _ = writeln!(out, "        {dst_str} = {val_str}.tag;");
        }

        Instruction::EnumFieldLoad { dst, base, variant, field } => {
            let dst_str = fmt_place_poll(&Place::local(LocalId(dst.0)), func, registry);
            let base_str = fmt_place_poll(base, func, registry);
            let _ = writeln!(out, "        {dst_str} = {base_str}.data.{variant}._{field};");
        }

        Instruction::HeapAlloc { dst, type_id, .. } => {
            let dst_str = fmt_place_poll(&Place::local(LocalId(dst.0)), func, registry);
            let type_name = format_type(*type_id, registry);
            let _ = writeln!(out, "        {dst_str} = ({type_name}*)GORGET_ALLOC(sizeof({type_name}));");
        }

        Instruction::HeapAllocArray { dst, type_id, count, .. } => {
            let dst_str = fmt_place_poll(&Place::local(LocalId(dst.0)), func, registry);
            let type_name = format_type(*type_id, registry);
            let count_str = fmt_operand_poll(count, func, registry);
            let _ = writeln!(out, "        {dst_str} = ({type_name}*)GORGET_ALLOC(sizeof({type_name}) * (size_t)({count_str}));");
        }

        Instruction::Dealloc { ptr, .. } => {
            let ptr_str = fmt_operand_poll(ptr, func, registry);
            let _ = writeln!(out, "        GORGET_FREE({ptr_str}, 0);");
        }

        Instruction::Drop { place } => {
            let place_str = fmt_place_poll(place, func, registry);
            let local_type = func.locals[place.local.0 as usize].type_id;
            emit_drop_code(out, &place_str, local_type, registry);
        }

        Instruction::DropIfAlive { place } => {
            let place_str = fmt_place_poll(place, func, registry);
            let local_type = func.locals[place.local.0 as usize].type_id;
            emit_drop_if_alive_code(out, &place_str, local_type, registry);
        }

        _ => {
            // Unsupported — coroutine candidacy check should have caught this
            out.push_str("        /* COROUTINE_UNSUPPORTED_INST */\n");
        }
    }
    None
}

/// Assign state IDs to basic blocks.
/// Each BB gets 1 + N + S state IDs where N = number of yields and S = number of
/// "split" transitions (retry-kind yield immediately following a non-retry yield).
/// The split requires an extra intermediate state to avoid re-executing the previous
/// yield's resume code when the retry re-enters.
fn coroutine_state_ids(func: &Function) -> Vec<u32> {
    let mut ids = Vec::with_capacity(func.blocks.len());
    let mut next = 0u32;
    for bb in &func.blocks {
        ids.push(next);
        // Collect yield kinds in order (excluding Blocking which is inline).
        let yield_kinds: Vec<YieldKind> = bb.instructions.iter()
            .filter_map(|i| {
                let kind = classify_yield(i, func)?;
                if kind == YieldKind::Blocking { return None; }
                Some(kind)
            })
            .collect();
        let yield_count = yield_kinds.len() as u32;
        // Count extra states from needs_split: retry-kind yield after non-retry yield.
        let split_count = yield_kinds.windows(2)
            .filter(|pair| is_retry_yield_kind(pair[1]) && !is_retry_yield_kind(pair[0]))
            .count() as u32;
        next += 1 + yield_count + split_count;
    }
    ids
}

/// Returns true for yield kinds that retry in-place (poll pattern) rather than
/// advancing state before yielding.
fn is_retry_yield_kind(kind: YieldKind) -> bool {
    matches!(kind, YieldKind::MutexLock | YieldKind::RwLockRead | YieldKind::RwLockWrite
        | YieldKind::ChannelSend | YieldKind::ChannelRecv
        | YieldKind::SocketRead | YieldKind::SocketWrite | YieldKind::SocketAccept | YieldKind::SocketConnect)
}

/// Emit a coroutine terminator (the last thing in a state — sets next state and continues).
fn emit_poll_terminator(
    out: &mut String,
    term: &Terminator,
    func: &Function,
    registry: &TypeRegistry,
    state_ids: &[u32],
) {
    match term {
        Terminator::Return(_) => {
            // Return value is already in f->_0; signal READY.
            out.push_str("        return GORGET_POLL_READY;\n");
        }
        Terminator::Jump(target) => {
            let next = state_ids[target.0 as usize];
            let _ = writeln!(out, "        f->__state = {next}; continue;");
        }
        Terminator::Branch { cond, then_block, else_block } => {
            let cond_str = fmt_operand_poll(cond, func, registry);
            let then_id = state_ids[then_block.0 as usize];
            let else_id = state_ids[else_block.0 as usize];
            let _ = writeln!(out, "        if ({cond_str}) {{ f->__state = {then_id}; }} else {{ f->__state = {else_id}; }} continue;");
        }
        Terminator::Switch { value, cases, default } => {
            let val_str = fmt_operand_poll(value, func, registry);
            let _ = writeln!(out, "        switch ((int64_t)({val_str})) {{");
            for (c, target) in cases {
                let tid = state_ids[target.0 as usize];
                let _ = writeln!(out, "            case {c}: f->__state = {tid}; break;");
            }
            let def_id = state_ids[default.0 as usize];
            let _ = writeln!(out, "            default: f->__state = {def_id}; break;");
            out.push_str("        } continue;\n");
        }
        Terminator::Unreachable => {
            out.push_str("        __builtin_unreachable();\n");
        }
        Terminator::Invoke { .. } => {
            // Should have been filtered by fn_is_coroutine_candidate
            out.push_str("        return GORGET_POLL_READY; /* UNSUPPORTED INVOKE */\n");
        }
    }
}

/// Emit the coroutine frame struct, poll function, spawn/await/drop helpers.
/// Emit the await result call for a completed child task (non-blocking path).
fn emit_await_result_call(
    out: &mut String,
    await_inst: &Instruction,
    func: &Function,
    registry: &TypeRegistry,
) {
    if let Instruction::Call { dst, func: call_fn, args } = await_inst {
        let args_str = fmt_args_poll(args, func, registry);
        if let Some(dst_id) = dst {
            let dst_str = fmt_place_poll(&Place::local(LocalId(dst_id.0)), func, registry);
            let _ = writeln!(out, "        {dst_str} = {call_fn}({args_str});");
        } else {
            let _ = writeln!(out, "        {call_fn}({args_str});");
        }
    }
}

/// Emit the coroutine frame struct, poll function, spawn/await/drop helpers.
fn emit_coroutine(
    out: &mut String,
    fn_name: &str,
    func: &Function,
    params: &[(String, TypeId)],
    ret_type: TypeId,
    module: &Module,
) {
    let registry = &module.type_registry;
    let ret_c = format_type(ret_type, registry);
    let is_void = ret_c == "void";
    let task_name = if is_void { "Task__void".to_string() } else { format!("Task__{ret_c}") };
    let frame_name = format!("__Frame_{fn_name}");
    let state_ids = coroutine_state_ids(func);
    let mut type_overrides = compute_type_overrides(func, registry, module);

    // Parameter locals (indices 1..=N) are stored by value in the coroutine frame,
    // even though the function signature passes Move types by Ptr/MutPtr.
    // Unwrap pointer types at the GIR level to get the value type for the frame field.
    let num_params = params.len();
    for idx in 1..=num_params {
        if idx < func.locals.len() {
            let type_id = func.locals[idx].type_id;
            let inner = match registry.get(type_id) {
                Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => Some(*inner),
                _ => None,
            };
            if let Some(inner_type) = inner {
                type_overrides.insert(idx, format_type(inner_type, registry));
            }
        }
    }

    // ── 1. Frame struct ─────────────────────────────────────────────────────
    let _ = writeln!(out, "typedef struct {frame_name} {{");
    out.push_str("    GorgetTask base; /* must be first */\n");
    out.push_str("    int __state;\n");
    // All locals (including _0 = return place, _1.._N = params + user vars).
    // Type overrides already have parameter locals stripped of pointer suffix.
    for (idx, local) in func.locals.iter().enumerate() {
        let c_type = if let Some(ovr) = type_overrides.get(&idx) {
            ovr.clone()
        } else {
            format_type(local.type_id, registry)
        };
        if c_type == "void" { continue; }
        let _ = writeln!(out, "    {c_type} _{idx};");
    }
    let _ = writeln!(out, "}} {frame_name};");

    // No pre-scan thunks needed: blocking calls use the notify-and-block approach
    // (see YieldKind::Blocking in the poll function below).

    // ── 2b. Poll function ────────────────────────────────────────────────────
    let _ = writeln!(out, "static int __poll_{fn_name}(GorgetTask* __base) {{");
    let _ = writeln!(out, "    {frame_name}* f = ({frame_name}*)__base;");
    out.push_str("    for (;;) {\n");
    out.push_str("    switch (f->__state) {\n");


    for (bb_idx, bb) in func.blocks.iter().enumerate() {
        let base_state = state_ids[bb_idx];

        // Collect TRUE yield positions (await + sleep only; blocking calls are inline).
        let yield_positions: Vec<(usize, YieldKind)> = bb.instructions.iter().enumerate()
            .filter_map(|(pos, i)| {
                let kind = classify_yield(i, func)?;
                if kind == YieldKind::Blocking { return None; }
                Some((pos, kind))
            })
            .collect();

        if yield_positions.is_empty() {
            // ── No-yield state: emit all instructions + terminator ────────────
            let _ = writeln!(out, "    case {base_state}: {{");
            for inst in &bb.instructions {
                emit_poll_inst(out, inst, func, registry, module.runtime.overflow_wrap, &type_overrides, module);
            }
            if let Some(term) = &bb.terminator {
                emit_poll_terminator(out, term, func, registry, &state_ids);
            }
            out.push_str("    }\n");
        } else {
            // Process N yield points: creates N+1 states.
            let mut current_state = base_state;

            for (yield_idx, &(yield_pos, yield_kind)) in yield_positions.iter().enumerate() {
                // Determine if this retry-in-place yield follows a non-retry yield.
                // If so, we need an extra state to avoid re-executing the previous
                // yield's resume code (e.g., await free) when the retry re-enters.
                let is_retry = is_retry_yield_kind(yield_kind);
                let prev_was_non_retry = yield_idx > 0 && !is_retry_yield_kind(yield_positions[yield_idx - 1].1);
                let needs_split = is_retry && prev_was_non_retry;

                let _ = writeln!(out, "    case {current_state}: {{");

                // For resume states (not the first), emit the previous yield's resume code
                if yield_idx > 0 {
                    let (prev_pos, prev_kind) = yield_positions[yield_idx - 1];
                    let prev_inst = &bb.instructions[prev_pos];
                    match prev_kind {
                        YieldKind::Await => {
                            emit_await_result_call(out, prev_inst, func, registry);
                        }
                        YieldKind::Sleep | YieldKind::Blocking => {
                            // Sleep/blocking: no result to extract on resume — work is done
                        }
                        YieldKind::MutexLock | YieldKind::RwLockRead | YieldKind::RwLockWrite => {
                            // Guard already filled by poll_lock/poll_read/poll_write in the previous state.
                        }
                        YieldKind::ChannelSend => {}
                        YieldKind::ChannelRecv => {
                            // Recv result already written to frame local by poll_recv.
                        }
                        YieldKind::SocketRead => {
                            // Result already stored + blocking restored by yield handler.
                        }
                        YieldKind::SocketWrite => {
                            // Result already stored + blocking restored by yield handler.
                        }
                        YieldKind::SocketAccept => {
                            // Result already stored by yield handler.
                        }
                        YieldKind::SocketConnect => {
                            // Connect finished by yield handler.
                        }
                    }
                }

                // Emit instructions from after the previous yield (or BB start) to this yield
                let inst_start = if yield_idx == 0 { 0 } else { yield_positions[yield_idx - 1].0 + 1 };
                for inst in &bb.instructions[inst_start..yield_pos] {
                    emit_poll_inst(out, inst, func, registry, module.runtime.overflow_wrap, &type_overrides, module);
                }

                // If we need to split (retry yield after non-retry yield), close this
                // state and open a new one for the retry. This prevents re-executing
                // the resume code (e.g., await free) when the retry re-enters.
                if needs_split {
                    current_state += 1;
                    let _ = writeln!(out, "        f->__state = {current_state};");
                    out.push_str("    }\n");
                    out.push_str("    __attribute__((fallthrough));\n");
                    let _ = writeln!(out, "    case {current_state}: {{");
                }

                let resume_state = current_state + 1;
                // Lock/channel yield kinds manage their own state transition (only advance on success).
                if !is_retry {
                    let _ = writeln!(out, "        f->__state = {resume_state};");
                }

                match yield_kind {
                    YieldKind::Await => {
                        // Extract task local from this await call
                        let await_inst = &bb.instructions[yield_pos];
                        let task_local = if let Instruction::Call { args, .. } = await_inst {
                            args.first().and_then(|a| match a {
                                Operand::Copy(p) | Operand::Move(p) => Some(p.local.0),
                                _ => None,
                            }).unwrap_or(0)
                        } else { 0 };

                        // Waker-check: register parent waker with child, yield if not done
                        let _ = writeln!(out, "        {{");
                        let _ = writeln!(out, "            GorgetTask* __child = (GorgetTask*)f->_{task_local}.__task;");
                        out.push_str("            pthread_mutex_lock(&__child->mtx);\n");
                        out.push_str("            if (!__child->done)\n");
                        out.push_str("                __child->parent_waker = (GorgetWaker){__gorget_fiber_waker_wake, (void*)f};\n");
                        out.push_str("            int __child_done = __child->done;\n");
                        out.push_str("            pthread_mutex_unlock(&__child->mtx);\n");
                        out.push_str("            if (__child_done) continue;\n");
                        out.push_str("        }\n");
                    }
                    YieldKind::Sleep => {
                        // Extract sleep argument and emit async reactor call
                        let sleep_inst = &bb.instructions[yield_pos];
                        let sleep_arg = match sleep_inst {
                            Instruction::Call { args, func: fname, .. }
                            | Instruction::CallExtern { args, func: fname, .. } => {
                                if !args.is_empty() {
                                    let raw = fmt_operand_poll(&args[0], func, registry);
                                    if fname == "sleep" || fname == "gg_sleep" {
                                        // int-arg sleep → already in ms
                                        raw
                                    } else {
                                        raw
                                    }
                                } else {
                                    "0".to_string()
                                }
                            }
                            _ => "0".to_string(),
                        };
                        let _ = writeln!(out, "        gorget_reactor_sleep_async({sleep_arg}, (GorgetWaker){{__gorget_fiber_waker_wake, (void*)f}});");
                    }
                    YieldKind::MutexLock | YieldKind::RwLockRead | YieldKind::RwLockWrite => {
                        // Trylock: on success advance state and continue, on fail stay and yield.
                        // Waker re-enters this same case to retry.
                        let poll_fn = match yield_kind {
                            YieldKind::MutexLock => "gorget_mutex_poll_lock",
                            YieldKind::RwLockRead => "gorget_rwlock_poll_read",
                            YieldKind::RwLockWrite => "gorget_rwlock_poll_write",
                            _ => unreachable!(),
                        };
                        let (dst_local, lock_arg) = extract_lock_call_info(&bb.instructions[yield_pos], func, registry);
                        let _ = writeln!(out, "        {{");
                        let _ = writeln!(out, "            GorgetWaker __w = (GorgetWaker){{__gorget_fiber_waker_wake, (void*)f}};");
                        let _ = writeln!(out, "            int __r = {poll_fn}({lock_arg}, &f->_{dst_local}, &__w);");
                        let _ = writeln!(out, "            if (__r == GORGET_POLL_READY) {{ f->__state = {resume_state}; continue; }}");
                        out.push_str("        }\n");
                    }
                    YieldKind::ChannelSend => {
                        let (_dst, ch_arg, val_arg, poll_fn) = extract_channel_call_info(&bb.instructions[yield_pos], func, registry, true);
                        let val_str = val_arg.unwrap_or_else(|| "0".to_string());
                        let _ = writeln!(out, "        {{");
                        let _ = writeln!(out, "            GorgetWaker __w = (GorgetWaker){{__gorget_fiber_waker_wake, (void*)f}};");
                        let _ = writeln!(out, "            int __r = {poll_fn}((*{ch_arg}), {val_str}, &__w);");
                        let _ = writeln!(out, "            if (__r) {{ f->__state = {resume_state}; continue; }}");
                        out.push_str("        }\n");
                    }
                    YieldKind::ChannelRecv => {
                        let (dst_local, ch_arg, _, poll_fn) = extract_channel_call_info(&bb.instructions[yield_pos], func, registry, false);
                        let _ = writeln!(out, "        {{");
                        let _ = writeln!(out, "            GorgetWaker __w = (GorgetWaker){{__gorget_fiber_waker_wake, (void*)f}};");
                        let _ = writeln!(out, "            int __r = {poll_fn}((*{ch_arg}), &f->_{dst_local}, &__w);");
                        let _ = writeln!(out, "            if (__r) {{ f->__state = {resume_state}; continue; }}");
                        out.push_str("        }\n");
                    }
                    YieldKind::SocketRead => {
                        // Non-blocking read: set socket non-blocking, try async_read.
                        // If pending → register reactor wait_readable, return PENDING.
                        // On resume (next state) → retry the read.
                        let (dst_local, fname, args) = extract_socket_call_info(&bb.instructions[yield_pos], func, registry);
                        let sock_arg = args.first().map(|s| s.as_str()).unwrap_or("NULL");
                        let _ = writeln!(out, "        {{");
                        let _ = writeln!(out, "            GorgetWaker __w = (GorgetWaker){{__gorget_fiber_waker_wake, (void*)f}};");
                        let _ = writeln!(out, "            gorget_socket_set_nonblocking({sock_arg});");
                        if fname == "gorget_socket_read_line" {
                            // read_line returns GorgetString — use a poll loop pattern
                            // For simplicity, offload read_line to blocking pool for now
                            // (line-buffered I/O is complex to make async)
                            let _ = writeln!(out, "            f->_{dst_local} = gorget_socket_read_line({sock_arg});");
                            let _ = writeln!(out, "            gorget_socket_set_blocking({sock_arg});");
                            let _ = writeln!(out, "            f->__state = {resume_state}; continue;");
                        } else {
                            let n_arg = args.get(1).map(|s| s.as_str()).unwrap_or("4096");
                            if fname == "gorget_socket_read_exact" {
                                let _ = writeln!(out, "            f->_{dst_local} = gorget_socket_async_read({sock_arg}, {n_arg});");
                            } else {
                                let _ = writeln!(out, "            f->_{dst_local} = gorget_socket_async_read({sock_arg}, {n_arg});");
                            }
                            let _ = writeln!(out, "            if (!gorget_socket_async_read_is_pending(&f->_{dst_local})) {{");
                            let _ = writeln!(out, "                gorget_socket_set_blocking({sock_arg});");
                            let _ = writeln!(out, "                f->__state = {resume_state}; continue;");
                            let _ = writeln!(out, "            }}");
                            let _ = writeln!(out, "            gorget_reactor_wait_readable({sock_arg}->fd, __w);");
                        }
                        out.push_str("        }\n");
                    }
                    YieldKind::SocketWrite => {
                        let (dst_local, fname, args) = extract_socket_call_info(&bb.instructions[yield_pos], func, registry);
                        let sock_arg = args.first().map(|s| s.as_str()).unwrap_or("NULL");
                        let data_arg = args.get(1).map(|s| s.as_str()).unwrap_or("\"\"");
                        let _ = writeln!(out, "        {{");
                        let _ = writeln!(out, "            GorgetWaker __w = (GorgetWaker){{__gorget_fiber_waker_wake, (void*)f}};");
                        let _ = writeln!(out, "            gorget_socket_set_nonblocking({sock_arg});");
                        if fname == "gorget_socket_async_write_str" {
                            let _ = writeln!(out, "            int64_t __wr = gorget_socket_async_write_str({sock_arg}, {data_arg});");
                        } else {
                            let _ = writeln!(out, "            int64_t __wr = gorget_socket_async_write({sock_arg}, {data_arg});");
                        }
                        let _ = writeln!(out, "            if (__wr != GORGET_IO_PENDING) {{");
                        let _ = writeln!(out, "                gorget_socket_set_blocking({sock_arg});");
                        let _ = writeln!(out, "                f->_{dst_local} = __wr;");
                        let _ = writeln!(out, "                f->__state = {resume_state}; continue;");
                        let _ = writeln!(out, "            }}");
                        let _ = writeln!(out, "            gorget_reactor_wait_writable({sock_arg}->fd, __w);");
                        out.push_str("        }\n");
                    }
                    YieldKind::SocketAccept => {
                        let (dst_local, _fname, args) = extract_socket_call_info(&bb.instructions[yield_pos], func, registry);
                        let srv_arg = args.first().map(|s| s.as_str()).unwrap_or("NULL");
                        let dst_c = effective_c_type(dst_local as usize, func, registry, &type_overrides);
                        let _ = writeln!(out, "        {{");
                        let _ = writeln!(out, "            GorgetWaker __w = (GorgetWaker){{__gorget_fiber_waker_wake, (void*)f}};");
                        let _ = writeln!(out, "            gorget_server_socket_set_nonblocking({srv_arg});");
                        let _ = writeln!(out, "            GorgetSocket __acc = gorget_socket_async_accept({srv_arg});");
                        let _ = writeln!(out, "            if (__acc.fd != GORGET_IO_PENDING) {{");
                        // Wrap in Result
                        if dst_c.starts_with("Result__") {
                            let _ = writeln!(out, "                const char* __err = gorget_socket_last_error();");
                            let _ = writeln!(out, "                if (__acc.fd >= 0) {{");
                            let _ = writeln!(out, "                    f->_{dst_local} = ({dst_c}){{ .tag = 0, .data = {{ .Ok = {{ ._0 = __acc }} }} }};");
                            let _ = writeln!(out, "                }} else {{");
                            let _ = writeln!(out, "                    f->_{dst_local} = ({dst_c}){{ .tag = 1, .data = {{ .Error = {{ ._0 = gorget_str_from_cstr(__err ? __err : \"accept failed\") }} }} }};");
                            let _ = writeln!(out, "                }}");
                        } else {
                            let _ = writeln!(out, "                f->_{dst_local} = __acc;");
                        }
                        let _ = writeln!(out, "                f->__state = {resume_state}; continue;");
                        let _ = writeln!(out, "            }}");
                        let _ = writeln!(out, "            gorget_reactor_wait_readable({srv_arg}->fd, __w);");
                        out.push_str("        }\n");
                    }
                    YieldKind::SocketConnect => {
                        let (dst_local, _fname, args) = extract_socket_call_info(&bb.instructions[yield_pos], func, registry);
                        let host_arg = args.first().map(|s| s.as_str()).unwrap_or("\"\"");
                        let port_arg = args.get(1).map(|s| s.as_str()).unwrap_or("0");
                        let _ = writeln!(out, "        {{");
                        let _ = writeln!(out, "            GorgetWaker __w = (GorgetWaker){{__gorget_fiber_waker_wake, (void*)f}};");
                        let _ = writeln!(out, "            int __rc = gorget_socket_async_connect_start({host_arg}, {port_arg}, &f->_{dst_local});");
                        let _ = writeln!(out, "            if (__rc == 0) {{ f->__state = {resume_state}; continue; }}");
                        let _ = writeln!(out, "            if (__rc == GORGET_IO_PENDING) {{");
                        let _ = writeln!(out, "                gorget_reactor_wait_writable(f->_{dst_local}.fd, __w);");
                        let _ = writeln!(out, "            }} else {{");
                        let _ = writeln!(out, "                f->__state = {resume_state}; continue;"); // error — still advance
                        let _ = writeln!(out, "            }}");
                        out.push_str("        }\n");
                    }
                    YieldKind::Blocking => unreachable!("blocking calls filtered out of yield_positions"),
                }

                out.push_str("        return GORGET_POLL_PENDING;\n");
                out.push_str("    }\n");

                current_state = resume_state;
            }

            // ── Final resume state: last yield result + remaining instructions + terminator
            let (last_pos, last_kind) = *yield_positions.last().unwrap();
            let _ = writeln!(out, "    case {current_state}: {{");
            match last_kind {
                YieldKind::Await => {
                    emit_await_result_call(out, &bb.instructions[last_pos], func, registry);
                }
                YieldKind::Sleep | YieldKind::Blocking => {}
                YieldKind::MutexLock | YieldKind::RwLockRead | YieldKind::RwLockWrite => {
                    // Guard already filled by poll_lock/poll_read/poll_write in the previous state.
                }
                YieldKind::ChannelSend => {
                    // Send completed in previous state — nothing to extract.
                }
                YieldKind::ChannelRecv => {
                    // Recv result already written to frame local by poll_recv in previous state.
                }
                YieldKind::SocketRead => {
                    // Result already stored + blocking restored by yield handler.
                }
                YieldKind::SocketWrite => {
                    // Result already stored + blocking restored by yield handler.
                }
                YieldKind::SocketAccept => {
                    // Result already stored by yield handler.
                }
                YieldKind::SocketConnect => {
                    // Connect finished by yield handler.
                }
            }
            for inst in &bb.instructions[last_pos + 1..] {
                emit_poll_inst(out, inst, func, registry, module.runtime.overflow_wrap, &type_overrides, module);
            }
            if let Some(term) = &bb.terminator {
                emit_poll_terminator(out, term, func, registry, &state_ids);
            }
            out.push_str("    }\n");
        }
    }

    out.push_str("    default: return GORGET_POLL_READY;\n");
    out.push_str("    } /* switch */\n");
    out.push_str("    } /* for */\n");
    out.push_str("}\n");

    // ── 3. Drop helper ────────────────────────────────────────────────────────
    // Waits for coroutine to complete (condvar on done=1), then frees the frame.
    let param_c_types: Vec<String> = params.iter()
        .map(|(_, t)| spawn_param_c_type(*t, registry))
        .collect();
    let param_decls = params.iter().zip(&param_c_types)
        .map(|((name, _), c)| format!("{c} {name}"))
        .collect::<Vec<_>>()
        .join(", ");
    let _ = writeln!(out, "static void __spawn_drop_{fn_name}(void* __ptr) {{");
    let _ = writeln!(out, "    {frame_name}* f = ({frame_name}*)__ptr;");
    out.push_str("    GORGET_SCHEDULER_WAIT(&f->base);\n");
    out.push_str("    pthread_mutex_destroy(&f->base.mtx);\n");
    out.push_str("    pthread_cond_destroy(&f->base.cond);\n");
    let _ = writeln!(out, "    GORGET_FREE(f, sizeof({frame_name}));");
    out.push_str("}\n");

    // ── 4. Spawn function ─────────────────────────────────────────────────────
    let _ = writeln!(out, "static inline {task_name} __gorget_spawn_{fn_name}({param_decls}) {{");
    let _ = writeln!(out, "    {frame_name}* f = ({frame_name}*)GORGET_CALLOC(1, sizeof({frame_name}));");
    out.push_str("    f->base.poll = __poll_");
    out.push_str(fn_name);
    out.push_str(";\n");
    out.push_str("    pthread_mutex_init(&f->base.mtx, NULL);\n");
    out.push_str("    pthread_cond_init(&f->base.cond, NULL);\n");
    // Copy params into frame (_1.._N for params)
    for (idx, (param_name, param_type)) in params.iter().enumerate() {
        let local_idx = idx + 1; // _1 is first param
        let gir_name = gir_type_name(*param_type, registry);
        let is_refcounted = gir_name.as_ref().map_or(false, |n| {
            n.starts_with("Channel__") || n.starts_with("Shared__") || n.starts_with("Weak__")
        });
        if is_refcounted {
            let type_name = gir_name.as_ref().unwrap();
            let _ = writeln!(out, "    f->_{local_idx} = {type_name}__clone({param_name});");
        } else {
            let _ = writeln!(out, "    f->_{local_idx} = {param_name};");
        }
    }
    let submit_macro = if module.runtime.blocking_fn_names.contains(fn_name) {
        "GORGET_BLOCKING_SUBMIT"
    } else {
        "GORGET_SCHEDULER_SUBMIT"
    };
    let _ = writeln!(out, "    {submit_macro}(&f->base);");
    let _ = writeln!(out, "    return ({task_name}){{.__task = f, .__drop = __spawn_drop_{fn_name}}};");
    out.push_str("}\n");

    // ── 5. Await function ─────────────────────────────────────────────────────
    // Blocking wait (condvar): the CALLER blocks until the coroutine signals done=1.
    // Since the caller is typically the main thread (not a worker), this is safe.
    if is_void {
        let _ = writeln!(out, "static inline void __gorget_await_{fn_name}({task_name} task) {{");
    } else {
        let _ = writeln!(out, "static inline {ret_c} __gorget_await_{fn_name}({task_name} task) {{");
    }
    let _ = writeln!(out, "    {frame_name}* f = ({frame_name}*)task.__task;");
    out.push_str("    GORGET_SCHEDULER_WAIT(&f->base);\n");
    if !is_void {
        let _ = writeln!(out, "    {ret_c} result = f->_0;");
    }
    out.push_str("    pthread_mutex_destroy(&f->base.mtx);\n");
    out.push_str("    pthread_cond_destroy(&f->base.cond);\n");
    let _ = writeln!(out, "    GORGET_FREE(f, sizeof({frame_name}));");
    if !is_void {
        out.push_str("    return result;\n");
    }
    out.push_str("}\n\n");
}

/// Emit per-spawned-function context structs, executor run functions, and spawn/await helpers.
///
/// Phases 1-3 of M:N scheduling: instead of creating one OS thread per spawn, each
/// spawned task is submitted to the shared executor thread pool (bounded N workers).
/// Await uses a condvar on the task's GorgetTask.done field instead of pthread_join,
/// so 10,000 concurrent spawns use only N OS threads.
///
/// Called after GIR function forward declarations (so spawned functions are visible).
fn emit_spawn_helpers(out: &mut String, module: &Module) {
    if module.runtime.spawned_fns.is_empty() {
        return;
    }

    out.push_str("\n/* ── Spawn/await helpers (M:N executor pool) ── */\n");

    // Emit non-coroutine (blocking) helpers first so their spawn/await functions are
    // declared before any coroutine poll functions that call them.
    for (fn_name, params, ret_type) in &module.runtime.spawned_fns {
        if fn_is_coroutine_candidate(fn_name, module) { continue; }

        let ret_c = format_type(*ret_type, &module.type_registry);
        let is_void = ret_c == "void";
        let mangled_fn = mangle_name(fn_name);
        let ctx_name = format!("__SpawnCtx_{fn_name}");
        let task_name = if is_void {
            "Task__void".to_string()
        } else {
            format!("Task__{ret_c}")
        };

        // Context struct: GorgetTask base (must be first for safe pointer cast) +
        // param fields + optional result slot.
        let _ = writeln!(out, "typedef struct {ctx_name} {{");
        out.push_str("    GorgetTask base; /* must be first */\n");
        for (param_name, param_type) in params {
            // Callable params have UNIT_TYPE in GIR (void); store as void* so the
            // spawned task can call them indirectly via the __callable_N ABI.
            let param_c = spawn_param_c_type(*param_type, &module.type_registry);
            let _ = writeln!(out, "    {param_c} __{param_name};");
        }
        if !is_void {
            let _ = writeln!(out, "    {ret_c} result;");
        }
        let _ = writeln!(out, "}} {ctx_name};");

        // Executor run function — called by a worker thread from the pool.
        // The worker signals task->done after this returns (see __gorget_worker).
        // Look up actual function param types to detect auto-borrowed (MutPtr) params.
        // fn_sigs stores base types, but the function signature uses MutPtr for Move-type
        // Borrow params. We need `&` when the actual param is MutPtr wrapping the stored type.
        let actual_fn = module.functions.iter().find(|f| f.name == *fn_name);
        let call_args = params.iter()
            .enumerate()
            .map(|(i, (name, stored_type))| {
                let needs_ref = actual_fn.map_or(false, |f| {
                    f.params.get(i).map_or(false, |&actual_type| {
                        actual_type != *stored_type && matches!(
                            module.type_registry.get(actual_type),
                            Some(GirType::MutPtr(inner)) | Some(GirType::Ptr(inner))
                                if *inner == *stored_type
                        )
                    })
                });
                if needs_ref {
                    format!("&__ctx->__{name}")
                } else {
                    format!("__ctx->__{name}")
                }
            })
            .collect::<Vec<_>>()
            .join(", ");
        let _ = writeln!(out, "static void __spawn_run_{fn_name}(GorgetTask* __base) {{");
        let _ = writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)__base;");
        if is_void {
            let _ = writeln!(out, "    {mangled_fn}({call_args});");
        } else {
            let _ = writeln!(out, "    __ctx->result = {mangled_fn}({call_args});");
        }
        out.push_str("}\n");

        // Per-fn drop helper: waits for completion + destroys sync primitives + frees ctx.
        // Called via the __drop function pointer embedded in Task__T (RAII join-on-drop).
        let _ = writeln!(out, "static void __spawn_drop_{fn_name}(void* __ptr) {{");
        let _ = writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)__ptr;");
        out.push_str("    GORGET_SCHEDULER_WAIT(&__ctx->base);\n");
        out.push_str("    pthread_mutex_destroy(&__ctx->base.mtx);\n");
        out.push_str("    pthread_cond_destroy(&__ctx->base.cond);\n");
        let _ = writeln!(out, "    GORGET_FREE(__ctx, sizeof({ctx_name}));");
        out.push_str("}\n");

        // Spawn function: allocates ctx, initialises sync primitives, sets run fn,
        // submits to the executor pool — no pthread_create per spawn.
        let param_decls = params.iter()
            .map(|(name, type_id)| {
                let c = spawn_param_c_type(*type_id, &module.type_registry);
                format!("{c} {name}")
            })
            .collect::<Vec<_>>()
            .join(", ");
        let _ = writeln!(out, "static inline {task_name} __gorget_spawn_{fn_name}({param_decls}) {{");
        let _ = writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)GORGET_CALLOC(1, sizeof({ctx_name}));");
        out.push_str("    __ctx->base.run = __spawn_run_");
        out.push_str(fn_name);
        out.push_str(";\n");
        out.push_str("    pthread_mutex_init(&__ctx->base.mtx, NULL);\n");
        out.push_str("    pthread_cond_init(&__ctx->base.cond, NULL);\n");
        for (param_name, param_type) in params {
            // For ref-counted types, retain a new reference for the spawned task.
            let gir_name = gir_type_name(*param_type, &module.type_registry);
            let is_refcounted = gir_name.as_ref().map_or(false, |n|
                n.starts_with("Channel__") || n.starts_with("Shared__") || n.starts_with("Weak__"));
            if is_refcounted {
                let type_name = gir_name.as_ref().unwrap();
                let _ = writeln!(out, "    __ctx->__{param_name} = {type_name}__clone({param_name});");
            } else {
                let _ = writeln!(out, "    __ctx->__{param_name} = {param_name};");
            }
        }
        let submit_macro = if module.runtime.blocking_fn_names.contains(fn_name) {
            "GORGET_BLOCKING_SUBMIT"
        } else {
            "GORGET_SCHEDULER_SUBMIT"
        };
        let _ = writeln!(out, "    {submit_macro}(&__ctx->base);");
        let _ = writeln!(out, "    return ({task_name}){{.__task = __ctx, .__drop = __spawn_drop_{fn_name}}};");
        out.push_str("}\n");

        // Await function: waits on condvar for task completion, extracts result, frees ctx.
        // Blocking but bounded — only N OS threads are ever created, not one per spawn.
        if is_void {
            let _ = writeln!(out, "static inline void __gorget_await_{fn_name}({task_name} task) {{");
        } else {
            let _ = writeln!(out, "static inline {ret_c} __gorget_await_{fn_name}({task_name} task) {{");
        }
        let _ = writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)task.__task;");
        out.push_str("    GORGET_SCHEDULER_WAIT(&__ctx->base);\n");
        if !is_void {
            let _ = writeln!(out, "    {ret_c} result = __ctx->result;");
        }
        out.push_str("    pthread_mutex_destroy(&__ctx->base.mtx);\n");
        out.push_str("    pthread_cond_destroy(&__ctx->base.cond);\n");
        let _ = writeln!(out, "    GORGET_FREE(__ctx, sizeof({ctx_name}));");
        if !is_void {
            out.push_str("    return result;\n");
        }
        out.push_str("}\n\n");
    }

    // Emit one Task__T__drop per unique Task type.
    // Called by the RAII drop elaborator; dispatches to the per-fn drop via __drop pointer.
    // Must be emitted before coroutine helpers since poll functions may drop Task locals.
    let mut emitted_task_drops: Vec<String> = Vec::new();
    for (_, _, ret_type) in &module.runtime.spawned_fns {
        let ret_c = format_type(*ret_type, &module.type_registry);
        let task_name = if ret_c == "void" {
            "Task__void".to_string()
        } else {
            format!("Task__{ret_c}")
        };
        if emitted_task_drops.contains(&task_name) {
            continue;
        }
        emitted_task_drops.push(task_name.clone());
        let _ = writeln!(out, "static inline void {task_name}__drop({task_name}* self) {{");
        let _ = writeln!(out, "    if (self && self->__task && self->__drop) {{");
        let _ = writeln!(out, "        self->__drop(self->__task);");
        let _ = writeln!(out, "        self->__task = NULL;");
        out.push_str("    }\n}\n\n");
        // Suppress unused-function warning (drop may not be called if all tasks are awaited).
        let _ = writeln!(out, "static void (*__unused_{task_name}__drop)({task_name}*) __attribute__((unused)) = {task_name}__drop;");
    }

    // Second pass: emit coroutine (stackless state machine) helpers.
    // Done after blocking helpers + Task__T__drop so all referenced functions are declared.
    for (fn_name, params, ret_type) in &module.runtime.spawned_fns {
        if !fn_is_coroutine_candidate(fn_name, module) { continue; }
        if let Some(func) = module.find_function(fn_name) {
            emit_coroutine(out, fn_name, func, params, *ret_type, module);
        }
    }
}

/// Emit type definitions (struct typedefs and enum tagged unions).
fn emit_type_definitions(out: &mut String, module: &Module) {
    let type_defs = module.type_registry.type_defs();
    if type_defs.is_empty() {
        return;
    }
    // Identify template type names to skip (have void fields = unmonomorphized)
    let skip = template_type_names(module);
    let should_skip = |name: &str| {
        skip.iter().any(|s| s == name)
            || runtime_type_name(name).is_some()
            || matches!(name, "ExecResult")
            // Channel__T and Task__T have hand-emitted C definitions (not GIR struct defs)
            || name.starts_with("Channel__")
            || name.starts_with("Task__")
            // Shared__T, Weak__T, Mutex__T, Guard__T, TaskGroup: hand-emitted pointer typedefs
            || name.starts_with("Shared__")
            || name.starts_with("Weak__")
            || name.starts_with("Mutex__")
            || name.starts_with("Guard__")
            || name == "TaskGroup"
            // std.sync: RWLock__T, ReadGuard__T, WriteGuard__T are hand-emitted
            || name.starts_with("RWLock__")
            || name.starts_with("ReadGuard__")
            || name.starts_with("WriteGuard__")
            // std.thread: Thread__T is hand-emitted
            || name.starts_with("Thread__")
    };
    // Collect Box types for special handling (Box__T → T* pointer typedef)
    let mut box_types: Vec<(String, String)> = Vec::new(); // (box_name, inner_c_type)
    for def in type_defs {
        if let Some(inner) = def.name.strip_prefix("Box__") {
            box_types.push((def.name.clone(), inner.to_string()));
        }
    }

    // Forward declare all struct/enum types first (for mutual references),
    // and emit alias typedefs here too so Box__Alias typedefs can reference them.
    for def in type_defs {
        if should_skip(&def.name) || def.name.starts_with("Box__") {
            continue;
        }
        match &def.kind {
            TypeDefKind::Struct(_) => {
                let _ = writeln!(out, "typedef struct {name} {name};", name = def.name);
            }
            TypeDefKind::Enum(_) => {
                let _ = writeln!(out, "typedef struct {name} {name};", name = def.name);
            }
            TypeDefKind::Alias(target) => {
                // Emit alias typedef here (before Box typedefs) so that Box__Alias can use the name.
                let c_type = format_type(*target, &module.type_registry);
                let _ = writeln!(out, "typedef {c_type} {name};", name = def.name);
            }
        }
    }
    // Emit Box typedefs as pointer types
    for (box_name, inner) in &box_types {
        // Skip phantom Box[Callable[...]] types (e.g., Box__Callable__unknown) — these arise
        // from the generic collector seeing Box[Callable[sig]] declarations, but the GIR path
        // treats Box[Callable[...]] variables as direct closure structs, so no typedef is needed.
        // "Callable__unknown" (and variants) are not valid C types.
        if inner.ends_with("__unknown") || inner.starts_with("Callable__") || inner.starts_with("MutCallable__") || inner.starts_with("ConsumeCallable__") {
            continue;
        }
        // Check if the inner type is a trait (has VTable/TraitObj types).
        // Trait types don't have struct definitions, only VTable/TraitObj wrappers.
        let is_trait = type_defs.iter().any(|d| d.name == format!("{inner}_VTable") || d.name == format!("{inner}_TraitObj"));
        if is_trait {
            let _ = writeln!(out, "typedef {inner}_TraitObj {box_name};");
        } else {
            let _ = writeln!(out, "typedef {inner}* {box_name};");
        }
    }
    out.push('\n');

    // Emit full struct definitions in topological order (value deps before dependents).
    // This avoids "field has incomplete type" C errors when struct A embeds struct B by value
    // but B is defined later in the GIR type registry.
    let topo_order = topo_sorted_body_order(type_defs, &module.type_registry, &should_skip);
    for &i in &topo_order {
        let def = &type_defs[i];
        match &def.kind {
            TypeDefKind::Struct(s) => {
                let _ = writeln!(out, "struct {name} {{", name = def.name);
                for field in &s.fields {
                    let decl = format_field_decl(field.type_id, &field.name, &module.type_registry);
                    let _ = writeln!(out, "    {decl};");
                }
                out.push_str("};\n\n");
            }
            TypeDefKind::Enum(e) => {
                let _ = writeln!(out, "struct {name} {{", name = def.name);
                out.push_str("    int tag;\n");
                out.push_str("    union {\n");
                for variant in &e.variants {
                    // Skip unit variants and void-only variants (e.g., Ok in Result[void, E])
                    let has_real_fields = variant.fields.iter().any(|f| f.type_id != UNIT_TYPE);
                    if variant.fields.is_empty() || !has_real_fields {
                        continue;
                    }
                    let _ = writeln!(out, "        struct {{");
                    for field in &variant.fields {
                        let c_type = format_type(field.type_id, &module.type_registry);
                        let _ = writeln!(out, "            {c_type} {name};", name = field.name);
                    }
                    let _ = writeln!(out, "        }} {name};", name = variant.name);
                }
                out.push_str("    } data;\n");
                out.push_str("};\n\n");
            }
            TypeDefKind::Alias(target) => {
                let c_type = format_type(*target, &module.type_registry);
                let _ = writeln!(out, "typedef {c_type} {name};\n", name = def.name);
            }
        }
    }

    // Emit Box alloc/get helper functions
    for (box_name, inner) in &box_types {
        // Skip phantom Box[Callable[...]] types (same reason as typedef skip above)
        if inner.ends_with("__unknown") || inner.starts_with("Callable__") || inner.starts_with("MutCallable__") || inner.starts_with("ConsumeCallable__") {
            continue;
        }
        let is_trait = type_defs.iter().any(|d| d.name == format!("{inner}_VTable") || d.name == format!("{inner}_TraitObj"));
        if !is_trait {
            let _ = writeln!(out, "static inline {box_name} __gorget_box_alloc_{inner}({inner} val) {{");
            let _ = writeln!(out, "    {inner}* p = ({inner}*)GORGET_ALLOC(sizeof({inner}));");
            let _ = writeln!(out, "    *p = val;");
            let _ = writeln!(out, "    return p;");
            let _ = writeln!(out, "}}");
            let _ = writeln!(out, "static inline {inner} {box_name}__get({box_name} b) {{ return *b; }}");
            let _ = writeln!(out, "static inline void {box_name}__set({box_name}* b_ptr, {inner} val) {{ **b_ptr = val; }}");
        } else {
            // Trait-erased Box — emit dispatch wrappers through the vtable
            let _ = writeln!(out, "/* Box[{inner}] — trait object dispatch wrappers */");
            // Read the VTable struct to get method signatures
            let vtable_name = format!("{inner}_VTable");
            if let Some(vtable_def) = module.type_registry.get_type_def(&vtable_name) {
                if let TypeDefKind::Struct(ref s) = vtable_def.kind {
                    for field in &s.fields {
                        let method_name = &field.name;
                        // Extract function pointer type to get params and return type
                        if let Some(GirType::FnPtr { params: param_types, return_type: ret_type }) = module.type_registry.get(field.type_id) {
                            let ret_c = format_type(*ret_type, &module.type_registry);
                            // Build parameter list (skip first param which is void* self)
                            let mut param_decls = Vec::new();
                            let mut param_names = Vec::new();
                            for (i, &pt) in param_types.iter().enumerate().skip(1) {
                                let pt_c = format_type(pt, &module.type_registry);
                                param_decls.push(format!("{pt_c} __p{i}"));
                                param_names.push(format!("__p{i}"));
                            }
                            let params_str = if param_decls.is_empty() {
                                String::new()
                            } else {
                                format!(", {}", param_decls.join(", "))
                            };
                            let args_str = if param_names.is_empty() {
                                String::new()
                            } else {
                                format!(", {}", param_names.join(", "))
                            };
                            let call_expr = format!("self->vtable->{method_name}(self->data{args_str})");
                            if ret_c == "void" {
                                let _ = writeln!(out, "static inline void {box_name}__{method_name}(const {box_name}* self{params_str}) {{ {call_expr}; }}");
                            } else {
                                let _ = writeln!(out, "static inline {ret_c} {box_name}__{method_name}(const {box_name}* self{params_str}) {{ return {call_expr}; }}");
                            }
                        }
                    }
                }
            }
        }
        out.push('\n');
    }
}

/// Emit global constant/variable definitions.
fn emit_globals(out: &mut String, module: &Module) {
    for global in &module.globals {
        emit_global(out, global, &module.type_registry);
    }
    // Emit __attribute__((constructor)) for globals that need runtime initialization.
    emit_global_constructors(out, module);
    if !module.globals.is_empty() {
        out.push('\n');
    }
}

/// Emit a single `__attribute__((constructor))` function that initializes all
/// globals whose `GlobalInit` is `RuntimeCall`.
fn emit_global_constructors(out: &mut String, module: &Module) {
    let runtime_globals: Vec<_> = module.globals.iter()
        .filter(|g| matches!(&g.init, GlobalInit::RuntimeCall(_)))
        .collect();
    if runtime_globals.is_empty() {
        return;
    }
    out.push_str("__attribute__((constructor)) static void __gorget_init_globals(void) {\n");
    for g in &runtime_globals {
        if let GlobalInit::RuntimeCall(expr) = &g.init {
            let _ = writeln!(out, "    {} = {};", g.name, expr);
        }
    }
    out.push_str("}\n");
}

/// Emit a single global variable/constant.
fn emit_global(out: &mut String, global: &Global, registry: &TypeRegistry) {
    let c_type = format_type(global.type_id, registry);
    match &global.init {
        GlobalInit::Zeroed => {
            let _ = writeln!(out, "static {c_type} {name} = {{0}};", name = global.name);
        }
        GlobalInit::Struct { type_name, fields } => {
            let _ = write!(out, "static const {c_type} {name} = ({type_name}){{", name = global.name);
            for (i, (fname, init)) in fields.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                let _ = write!(out, ".{fname} = ");
                emit_global_init(out, init);
            }
            out.push_str("};\n");
        }
        GlobalInit::FnRef(fn_name) => {
            let mangled = mangle_name(fn_name);
            let _ = writeln!(out, "static const {c_type} {name} = &{mangled};", name = global.name);
        }
        GlobalInit::Bytes(bytes) => {
            let _ = write!(out, "static const uint8_t {name}[] = {{", name = global.name);
            for (i, b) in bytes.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                let _ = write!(out, "0x{b:02x}");
            }
            out.push_str("};\n");
        }
        GlobalInit::RuntimeCall(_) => {
            // Declared without initializer; initialized in __gorget_init_globals().
            let _ = writeln!(out, "static {c_type} {name};", name = global.name);
        }
    }
}

/// Emit a global initializer expression.
fn emit_global_init(out: &mut String, init: &GlobalInit) {
    match init {
        GlobalInit::Zeroed => out.push_str("{0}"),
        GlobalInit::FnRef(fn_name) => {
            let mangled = mangle_name(fn_name);
            let _ = write!(out, "&{mangled}");
        }
        GlobalInit::Struct { type_name, fields } => {
            let _ = write!(out, "({type_name}){{");
            for (i, (fname, sub_init)) in fields.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                let _ = write!(out, ".{fname} = ");
                emit_global_init(out, sub_init);
            }
            out.push('}');
        }
        GlobalInit::Bytes(_) => out.push_str("/* bytes */"),
        GlobalInit::RuntimeCall(expr) => out.push_str(expr),
    }
}

/// Collect all function call names from GIR instructions.
fn collect_all_call_names(module: &Module) -> Vec<String> {
    let mut names = Vec::new();
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.instructions {
                match inst {
                    Instruction::Call { func: f, .. } => names.push(f.clone()),
                    Instruction::CallExtern { func: f, .. } => names.push(f.clone()),
                    _ => {}
                }
            }
        }
    }
    names
}

/// Emit typedefs for collection aliases (Vector__T → GorgetArray, etc.).
fn emit_collection_typedefs(out: &mut String, module: &Module) {
    use rustc_hash::FxHashSet;
    let mut emitted: FxHashSet<String> = FxHashSet::default();

    // Scan all types in functions (locals, params, return types)
    let check_type = |type_id: TypeId, emitted: &mut FxHashSet<String>, out: &mut String| {
        if let Some(GirType::Named(name)) = module.type_registry.get(type_id) {
            if !emitted.contains(name.as_str()) {
                if let Some(c_type) = collection_type_alias(name) {
                    let _ = writeln!(out, "typedef {c_type} {name};");
                    emitted.insert(name.clone());
                }
            }
        }
    };
    for func in &module.functions {
        for local in &func.locals {
            check_type(local.type_id, &mut emitted, out);
        }
        for &param_type in &func.params {
            check_type(param_type, &mut emitted, out);
        }
        check_type(func.return_type, &mut emitted, out);
        // Scan instructions for gorget_array_new(sizeof(T)) calls to derive
        // Vector__T typedefs (since type_overrides create these names but they
        // aren't in the type registry as GirType::Named)
        for block in &func.blocks {
            for inst in &block.instructions {
                match inst {
                    Instruction::Call { func: call_name, args, .. }
                    | Instruction::CallExtern { func: call_name, args, .. } => {
                        if call_name == "gorget_array_new" {
                            if let Some(Operand::Constant(Constant::SizeOf(elem_tid))) = args.first() {
                                let elem_c = format_type(*elem_tid, &module.type_registry);
                                let vec_type = format!("Vector__{}", elem_c);
                                if !emitted.contains(&vec_type) {
                                    if let Some(c_type) = collection_type_alias(&vec_type) {
                                        let _ = writeln!(out, "typedef {c_type} {vec_type};");
                                        emitted.insert(vec_type);
                                    }
                                }
                            }
                        } else if call_name == "gorget_dict_new" {
                            // Dict constructors don't embed type info in the call;
                            // scan for StructInit instead (handled below)
                        } else if call_name == "gorget_set_new" {
                            // Similar for sets
                        }
                    }
                    Instruction::StructInit { type_name, .. }
                    | Instruction::EnumInit { type_name, .. } => {
                        if !emitted.contains(type_name.as_str()) {
                            if let Some(c_type) = collection_type_alias(type_name) {
                                let _ = writeln!(out, "typedef {c_type} {type_name};");
                                emitted.insert(type_name.clone());
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    // Also scan TypeDefs that are collection types (skipped from struct emission)
    for def in module.type_registry.type_defs() {
        if !emitted.contains(def.name.as_str()) {
            if let Some(c_type) = collection_type_alias(&def.name) {
                let _ = writeln!(out, "typedef {c_type} {name};", name = def.name);
                emitted.insert(def.name.clone());
            }
        }
    }
    // Emit Option types needed by parse methods (Option__int8_t, etc.)
    // These are simple tagged unions emitted on demand.
    let mut option_types_needed: FxHashSet<String> = FxHashSet::default();
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.instructions {
                let call_name = match inst {
                    Instruction::Call { func: f, .. } | Instruction::CallExtern { func: f, .. } => f.as_str(),
                    _ => continue,
                };
                if let Some(opt_type) = infer_method_return_type(call_name) {
                    if opt_type.starts_with("Option__") && !emitted.contains(opt_type) {
                        option_types_needed.insert(opt_type.to_string());
                    }
                }
            }
        }
    }
    // Also scan type_defs for existing Option definitions (skip emitting duplicates)
    for def in module.type_registry.type_defs() {
        if def.name.starts_with("Option__") {
            option_types_needed.remove(&def.name);
        }
    }
    for opt_name in &option_types_needed {
        if emitted.contains(opt_name.as_str()) { continue; }
        // Extract inner type from "Option__inner_type"
        let inner = &opt_name["Option__".len()..];
        let _ = writeln!(out, "typedef struct {opt_name} {opt_name};");
        let _ = writeln!(out, "struct {opt_name} {{ int tag; union {{ struct {{ {inner} _0; }} Some; }} data; }};");
        emitted.insert(opt_name.clone());
    }
    if !emitted.is_empty() {
        out.push('\n');
    }
}

/// Map a Gorget type name to its C runtime equivalent.
/// Covers collections, opaque runtime types, and stdlib types.
fn runtime_type_name(name: &str) -> Option<&'static str> {
    // Collection type aliases (monomorphized names)
    if name.starts_with("Vector__") { return Some("GorgetArray"); }
    if name.starts_with("Set__") || name.starts_with("HashSet__") { return Some("GorgetSet"); }
    if name.starts_with("Dict__") || name.starts_with("HashMap__") { return Some("GorgetMap"); }
    // Callable generics → GorgetClosure (16-byte {fn_ptr, env} struct)
    if name.starts_with("Callable__") || name.starts_with("MutCallable__") || name.starts_with("ConsumeCallable__") {
        return Some("GorgetClosure");
    }
    match name {
        // Unmonomorphized collection template names
        "Vector" | "GorgetArray" => Some("GorgetArray"),
        "Dict" | "HashMap" | "GorgetMap" | "GorgetDict" => Some("GorgetMap"),
        "Set" | "HashSet" | "GorgetSet" => Some("GorgetSet"),
        // Network types
        "Socket" => Some("GorgetSocket"),
        "ServerSocket" => Some("GorgetServerSocket"),
        "UdpSocket" => Some("GorgetUdpSocket"),
        "UdpAddr" => Some("GorgetUdpAddr"),
        "UdpPacket" => Some("GorgetUdpPacket"),
        "TlsSocket" => Some("GorgetTlsSocket"),
        "TlsServerSocket" => Some("GorgetTlsServerSocket"),
        // IO types
        "File" => Some("GorgetFile"),
        "Channel" => Some("GorgetChannel*"),
        // Concurrency types
        "Shared" => Some("GorgetShared*"),
        "Weak" => Some("GorgetShared*"),
        "Mutex" => Some("GorgetMutex*"),
        // std.sync non-generic types
        "AtomicInt" => Some("GorgetAtomicInt*"),
        "AtomicBool" => Some("GorgetAtomicBool*"),
        "Barrier" => Some("GorgetBarrier*"),
        "CondVar" => Some("GorgetCondVar*"),
        "RWLock" => Some("GorgetRWLock*"),
        "WaitGroup" => Some("GorgetWaitGroup*"),
        "Semaphore" => Some("GorgetSemaphore*"),
        "OnceFlag" => Some("GorgetOnceFlag*"),
        // std.process Process type
        "Process" => Some("GorgetProcess*"),
        // Crypto types
        "CipherContext" => Some("GorgetCipherContext"),
        "BigNum" => Some("GorgetBigNum"),
        "RSAKey" => Some("GorgetRSAKey"),
        "Ed25519KeyPair" => Some("GorgetEd25519KeyPair"),
        "X25519KeyPair" => Some("GorgetX25519KeyPair"),
        // Regex types
        "Regex" => Some("GorgetRegex"),
        "Match" | "RegexMatch" => Some("GorgetRegexMatch"),
        // SDL types
        "SDLWindow" => Some("GorgetSDLWindow"),
        "SDLRenderer" => Some("GorgetSDLRenderer"),
        "SDLTexture" => Some("GorgetSDLTexture"),
        "SDLFont" => Some("GorgetSDLFont"),
        "SDLEvent" => Some("GorgetSDLEvent"),
        // String types
        "GorgetString" => Some("GorgetString"),
        // Allocator types
        "Arena" => Some("GorgetArena*"),
        "ArenaCheckpoint" => Some("GorgetArenaCheckpoint"),
        "TrackingAllocator" => Some("GorgetTrackingAllocator*"),
        "PoolAllocator" => Some("GorgetPoolAllocator*"),
        "TlsfAllocator" => Some("GorgetTlsfAllocator*"),
        "FixedBufferAllocator" => Some("GorgetFixedBufferAllocator*"),
        "FallbackAllocator" => Some("GorgetFallbackAllocator*"),
        // Audio types
        "AudioChunk" => Some("GorgetAudioChunk"),
        "AudioMusic" => Some("GorgetAudioMusic"),
        // GL types (GorgetGLContext is int64_t — opaque handle like Metal)
        "GLContext" => Some("GorgetGLContext"),
        _ => None,
    }
}

/// Map a collection type name to its C runtime type.
fn collection_type_alias(name: &str) -> Option<&'static str> {
    if name.starts_with("Vector__") { return Some("GorgetArray"); }
    if name.starts_with("Set__") || name.starts_with("HashSet__") { return Some("GorgetSet"); }
    // Dict and HashMap both map to GorgetMap in the runtime
    if name.starts_with("Dict__") || name.starts_with("HashMap__") {
        return Some("GorgetMap");
    }
    // NOTE: Shared__T, Mutex__T, Guard__T are NOT matched here.
    // Their typedefs are emitted by emit_shared_defs/emit_mutex_defs AFTER user struct defs.
    // Runtime types with different C names
    match name {
        "Socket" => Some("GorgetSocket"),
        "ServerSocket" => Some("GorgetServerSocket"),
        "TlsSocket" => Some("GorgetTlsSocket"),
        "TlsServerSocket" => Some("GorgetTlsServerSocket"),
        "UdpSocket" => Some("GorgetUdpSocket"),
        "Channel" => Some("GorgetChannel"),
        "Shared" => Some("GorgetShared*"),
        "Mutex" => Some("GorgetMutex*"),
        "TaskGroup" => Some("gorget_task_group_t*"),
        "Regex" => Some("GorgetRegex"),
        "RegexMatch" => Some("GorgetRegexMatch"),
        _ => None,
    }
}

/// Emit a forward declaration for a function.
/// Collect all FuncRef names used across the module and emit adapter functions.
/// Adapters bridge the gap between normal function ABI and the Callable (void*, params...) ABI.
fn emit_func_ref_adapters(out: &mut String, module: &Module) {
    use std::collections::BTreeSet;
    let registry = &module.type_registry;

    // Collect all FuncRef names from all instructions across all functions
    let mut func_refs = BTreeSet::new();
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.instructions {
                collect_func_refs_from_instruction(inst, &mut func_refs);
            }
            // Check terminator operands too
            if let Some(term) = &block.terminator {
                match term {
                    Terminator::Return(op) => {
                        if let Operand::Constant(Constant::FuncRef(name)) = op {
                            func_refs.insert(name.clone());
                        }
                    }
                    Terminator::Branch { cond, .. } => {
                        if let Operand::Constant(Constant::FuncRef(name)) = cond {
                            func_refs.insert(name.clone());
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    if func_refs.is_empty() {
        return;
    }

    out.push_str("// Named-function adapters for Callable dispatch\n");
    for ref_name in &func_refs {
        // Find the target function's signature
        let target = module.functions.iter().find(|f| f.name == *ref_name);
        if let Some(target_func) = target {
            let c_name = mangle_name(&target_func.name);
            let ret_type = format_type(target_func.return_type, registry);
            let adapter_name = format!("__adapt_{c_name}");

            // Emit adapter: ret_type __adapt_func(void* __env, params...) { return func(params...); }
            let _ = write!(out, "{ret_type} {adapter_name}(void* __env");
            for (i, &param_type) in target_func.params.iter().enumerate() {
                let c_type = format_type(param_type, registry);
                let c_type = if c_type == "void" { "void*".to_string() } else { c_type };
                let _ = write!(out, ", {c_type} __p{i}");
            }
            out.push_str(") {\n");
            if ret_type == "void" {
                let _ = write!(out, "    {c_name}(");
            } else {
                let _ = write!(out, "    return {c_name}(");
            }
            for i in 0..target_func.params.len() {
                if i > 0 { out.push_str(", "); }
                let _ = write!(out, "__p{i}");
            }
            out.push_str(");\n}\n");
        }
    }
    out.push('\n');
}

/// Collect FuncRef names from an instruction's operands.
fn collect_func_refs_from_instruction(inst: &Instruction, refs: &mut std::collections::BTreeSet<String>) {
    let check_op = |op: &Operand, refs: &mut std::collections::BTreeSet<String>| {
        if let Operand::Constant(Constant::FuncRef(name)) = op {
            refs.insert(name.clone());
        }
    };
    match inst {
        Instruction::Assign { value, .. } => check_op(value, refs),
        Instruction::Call { args, .. } => {
            for arg in args { check_op(arg, refs); }
        }
        Instruction::StructInit { fields, .. } => {
            for op in fields { check_op(op, refs); }
        }
        _ => {}
    }
}

fn emit_forward_decl(out: &mut String, func: &Function, registry: &TypeRegistry) {
    if func.name == "main" {
        return; // main doesn't need a forward decl
    }

    let c_name = mangle_name(&func.name);
    // FnPtr return type = user-declared closure type (int(int)) → GorgetClosure in C.
    // (FnPtr struct field types stay as function pointers; this only affects signatures.)
    let ret_type = if matches!(registry.get(func.return_type), Some(GirType::FnPtr { .. })) {
        "GorgetClosure".to_string()
    } else {
        format_type(func.return_type, registry)
    };
    let _ = write!(out, "{ret_type} {c_name}(");

    if func.params.is_empty() {
        out.push_str("void");
    } else {
        for (i, &param_type) in func.params.iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            let c_type = format_type(param_type, registry);
            // void params are invalid in C — use void* as generic pointer
            let c_type = if c_type == "void" { "void*".to_string() } else { c_type };
            // Use _N naming so body instructions can reference them
            let _ = write!(out, "{c_type} _{}", i + 1);
        }
    }

    out.push_str(");\n");
}

/// Emit a complete function definition.
fn emit_function(out: &mut String, func: &Function, module: &Module) {
    let registry = &module.type_registry;
    let is_main = func.name == "main";

    // Function signature
    if is_main {
        out.push_str("int main(int argc, char** argv) {\n");
    } else {
        let c_name = mangle_name(&func.name);
        // FnPtr return type = user-declared closure type → GorgetClosure in C signature.
        let ret_type = if matches!(registry.get(func.return_type), Some(GirType::FnPtr { .. })) {
            "GorgetClosure".to_string()
        } else {
            format_type(func.return_type, registry)
        };
        let _ = write!(out, "{ret_type} {c_name}(");

        if func.params.is_empty() {
            out.push_str("void");
        } else {
            for (i, &param_type) in func.params.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                let c_type = format_type(param_type, registry);
                // void params are invalid in C — use void* as generic pointer
                let c_type = if c_type == "void" { "void*".to_string() } else { c_type };
                // Use _N naming to match GIR local references
                let _ = write!(out, "{c_type} _{}", i + 1);
            }
        }

        out.push_str(") {\n");
    }

    // Local variable declarations (skip _0 for void, skip params)
    let num_params = func.params.len();
    let return_is_void = func.return_type == UNIT_TYPE && !is_main;

    // Pre-scan: find locals assigned by constructors/type-defs and override their types.
    // Run multiple passes until convergence so assignment chains propagate correctly.
    let type_overrides = compute_type_overrides(func, registry, module);


    // Pre-scan: find UNIT_TYPE locals that are referenced as operands.
    // These need a declaration even without a type override.
    let mut referenced_locals: std::collections::HashSet<usize> = std::collections::HashSet::new();
    for block in &func.blocks {
        for inst in &block.instructions {
            // Collect all locals referenced as Copy/Move operands
            let mut check_operand = |op: &Operand| {
                if let Operand::Copy(p) | Operand::Move(p) = op {
                    referenced_locals.insert(p.local.0 as usize);
                }
            };
            match inst {
                Instruction::Assign { value, .. } => check_operand(value),
                Instruction::BinOp { lhs, rhs, .. } => { check_operand(lhs); check_operand(rhs); }
                Instruction::UnOp { operand, .. } => check_operand(operand),
                Instruction::Cmp { lhs, rhs, .. } => { check_operand(lhs); check_operand(rhs); }
                Instruction::Call { args, .. } | Instruction::CallExtern { args, .. }
                | Instruction::CallIndirect { args, .. } => {
                    for a in args { check_operand(a); }
                }
                Instruction::Cast { value, .. } | Instruction::BitCast { value, .. }
                | Instruction::PtrCast { value, .. } => check_operand(value),
                Instruction::IndexLoad { index, .. } => check_operand(index),
                Instruction::TagOf { operand, .. } => check_operand(operand),
                Instruction::HeapAlloc { allocator, .. } => check_operand(allocator),
                Instruction::PushAllocator { allocator } => check_operand(allocator),
                Instruction::StructInit { fields, .. } => { for f in fields { check_operand(f); } }
                Instruction::EnumInit { fields, .. } => { for f in fields { check_operand(f); } }
                Instruction::TupleInit { elements, .. } => { for e in elements { check_operand(e); } }
                _ => {}
            }
        }
        // Also check terminators for operand references
        if let Some(ref term) = block.terminator {
            match term {
                Terminator::Branch { cond, .. } => {
                    if let Operand::Copy(p) | Operand::Move(p) = cond {
                        referenced_locals.insert(p.local.0 as usize);
                    }
                }
                Terminator::Return(op) => {
                    if let Operand::Copy(p) | Operand::Move(p) = op {
                        referenced_locals.insert(p.local.0 as usize);
                    }
                }
                _ => {}
            }
        }
    }

    for (i, local) in func.locals.iter().enumerate() {
        let local_id = i;

        // Skip param locals (they're the function parameters)
        if local_id >= 1 && local_id <= num_params {
            continue;
        }

        // Skip return place for void functions
        if local_id == 0 && return_is_void {
            continue;
        }

        // Skip Unit-typed locals unless they have a type override or are referenced
        if local.type_id == UNIT_TYPE && !type_overrides.contains_key(&local_id)
            && !referenced_locals.contains(&local_id)
        {
            continue;
        }

        let c_type = if let Some(override_type) = type_overrides.get(&local_id) {
            // Normalize runtime type names (e.g. Callable__* → GorgetClosure, Vector__T → GorgetArray)
            // Preserve pointer suffix: "Vector__int64_t*" → "GorgetArray*"
            let (base, suffix) = if override_type.ends_with('*') {
                (&override_type[..override_type.len() - 1], "*")
            } else {
                (override_type.as_str(), "")
            };
            if let Some(rt) = runtime_type_name(base) {
                format!("{rt}{suffix}")
            } else {
                override_type.clone()
            }
        } else {
            // FnPtr-typed locals represent escaped closures — declare as GorgetClosure.
            // (FnPtr in struct fields remains a real function pointer; locals are different.)
            if matches!(registry.get(local.type_id), Some(GirType::FnPtr { .. })) {
                "GorgetClosure".to_string()
            } else {
                let t = format_type(local.type_id, registry);
                // UNIT_TYPE locals that are referenced need a concrete type
                if t == "void" { "int64_t".to_string() } else { t }
            }
        };
        // Zero-initialize return local (_0) to avoid uninitialized warnings
        let init = if local_id == 0 { " = {0}" } else { "" };
        if let Some(ref hint) = local.name_hint {
            let _ = writeln!(out, "    {c_type} _{local_id}{init}; /* {hint} */");
        } else {
            let _ = writeln!(out, "    {c_type} _{local_id}{init};");
        }
    }

    // Emit `goto bb0;` to start
    if is_main {
        out.push_str("    gorget_init_args(argc, argv);\n");
        // Trace init: open trace file at program start.
        if let Some(ref trace_path) = module.runtime.trace_filename {
            let escaped = trace_path.replace('\\', "\\\\").replace('"', "\\\"");
            let _ = writeln!(out, "    __gorget_trace_init(\"{escaped}\");");
        }
    } else if let Some(ref display_name) = func.display_name {
        // Trace entry: emit call event with function name, parameter values, and depth.
        if module.runtime.trace_filename.is_some() {
            let escaped = display_name.replace('\\', "\\\\").replace('"', "\\\"");
            out.push_str("    if (__gorget_trace_fp) {\n");
            let _ = writeln!(out, "        fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"call\\\",\\\"fn\\\":\\\"{escaped}\\\",\\\"args\\\":{{\");");
            for (i, &param_type) in func.params.iter().enumerate() {
                let local_idx = i + 1;
                let gorget_name = func.locals.get(local_idx)
                    .and_then(|l| l.name_hint.as_deref())
                    .unwrap_or("_");
                let formatter = trace_formatter_for_type(param_type, registry);
                let comma = if i == 0 { "" } else { "," };
                let esc_name = gorget_name.replace('\\', "\\\\").replace('"', "\\\"");
                let _ = writeln!(out, "        fprintf(__gorget_trace_fp, \"{comma}\\\"{}\\\":\");", esc_name);
                let _ = writeln!(out, "        {formatter}(__gorget_trace_fp, _{local_idx});");
            }
            let _ = writeln!(out, "        fprintf(__gorget_trace_fp, \"}},\\\"depth\\\":%d}}\\n\", __gorget_trace_depth++);");
            out.push_str("    }\n");
        }
    }
    if !func.blocks.is_empty() {
        out.push_str("    goto bb0;\n");
    }

    // Pre-scan: collect which blocks are the "then" target of Branch terminators.
    // Branch events are emitted at the start of those blocks (only when actually entered).
    let trace_then_blocks: std::collections::HashSet<u32> = if module.runtime.trace_filename.is_some() {
        func.blocks.iter().filter_map(|b| {
            if let Some(crate::ir::instructions::Terminator::Branch { then_block, .. }) = &b.terminator {
                Some(then_block.0)
            } else {
                None
            }
        }).collect()
    } else {
        std::collections::HashSet::new()
    };

    // Emit basic blocks
    let mut alloc_save_counter: usize = 0;
    let mut alloc_save_stack: Vec<usize> = Vec::new();
    // Track which test-function locals have been registered on the cleanup stack.
    // Only the FIRST assignment to each local triggers a cleanup push.
    let mut test_cleanup_pushed: FxHashSet<u32> = FxHashSet::default();
    let tracing = module.runtime.trace_filename.is_some();
    for (i, block) in func.blocks.iter().enumerate() {
        let _ = writeln!(out, "    bb{i}: ;");

        // Branch event: emitted when a "then" block is actually entered.
        if tracing && trace_then_blocks.contains(&(i as u32)) {
            let _ = writeln!(out, "    if (__gorget_trace_fp) {{ fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"branch\\\",\\\"depth\\\":%d}}\\n\", __gorget_trace_depth); }}");
        }

        // Stmt_start event: emitted at the start of each block.
        if tracing {
            let _ = writeln!(out, "    if (__gorget_trace_fp) {{ fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"stmt_start\\\",\\\"depth\\\":%d}}\\n\", __gorget_trace_depth++); }}");
        }

        for inst in &block.instructions {
            emit_instruction(out, inst, func, registry, &type_overrides, &module.functions, module, &mut alloc_save_counter, &mut alloc_save_stack, &mut test_cleanup_pushed);
        }

        // Stmt_end event: emitted after instructions, before the terminator.
        if tracing {
            let _ = writeln!(out, "    if (__gorget_trace_fp) {{ fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"stmt_end\\\",\\\"depth\\\":%d}}\\n\", --__gorget_trace_depth); }}");
        }

        if let Some(ref term) = block.terminator {
            // For traced non-main functions with a display name, inject a return event
            // just before each return statement.
            let trace_name = if tracing && !is_main {
                func.display_name.as_deref()
            } else {
                None
            };
            emit_terminator(out, term, func, registry, trace_name);
        }
    }

    out.push_str("}\n");
}

/// Map a GIR TypeId to the appropriate trace formatter function name.
fn trace_formatter_for_type(type_id: crate::ir::types::TypeId, registry: &TypeRegistry) -> &'static str {
    use crate::ir::types::*;
    match type_id {
        BOOL_TYPE => "__gorget_trace_val_bool",
        F32_TYPE | F64_TYPE => "__gorget_trace_val_float",
        _ if type_id == I8_TYPE || type_id == I16_TYPE || type_id == I32_TYPE
            || type_id == I64_TYPE || type_id == U8_TYPE || type_id == U16_TYPE
            || type_id == U32_TYPE || type_id == U64_TYPE => "__gorget_trace_val_int",
        _ => {
            if let Some(crate::ir::types::GirType::Named(name)) = registry.get(type_id) {
                if name == "Str" || name == "GorgetString" {
                    return "__gorget_trace_val_Str";
                }
            }
            "__gorget_trace_val_int" // fallback for unknown types
        }
    }
}

/// Emit a single instruction as C code.
/// Get the effective C type name for a local, considering overrides.
fn effective_c_type(
    local_idx: usize,
    func: &Function,
    registry: &TypeRegistry,
    type_overrides: &std::collections::HashMap<usize, String>,
) -> String {
    if let Some(override_type) = type_overrides.get(&local_idx) {
        // Normalize runtime type names, preserving pointer suffix.
        // e.g. "Callable__*" → "GorgetClosure", "Vector__T*" → "GorgetArray*"
        let (base, suffix) = if override_type.ends_with('*') {
            (&override_type[..override_type.len() - 1], "*")
        } else {
            (override_type.as_str(), "")
        };
        if let Some(rt) = runtime_type_name(base) {
            return format!("{rt}{suffix}");
        }
        override_type.clone()
    } else if local_idx < func.locals.len() {
        // FnPtr-typed locals are represented as GorgetClosure in C
        if matches!(registry.get(func.locals[local_idx].type_id), Some(GirType::FnPtr { .. })) {
            return "GorgetClosure".to_string();
        }
        format_type(func.locals[local_idx].type_id, registry)
    } else {
        "int64_t".to_string()
    }
}

fn emit_instruction(
    out: &mut String,
    inst: &Instruction,
    func: &Function,
    registry: &TypeRegistry,
    type_overrides: &std::collections::HashMap<usize, String>,
    all_functions: &[Function],
    module: &Module,
    alloc_save_counter: &mut usize,
    alloc_save_stack: &mut Vec<usize>,
    test_cleanup_pushed: &mut FxHashSet<u32>,
) {
    match inst {
        Instruction::Assign { dst, value } => {
            // Skip Unit assignments (void = /* unit */ is invalid C)
            // BUT if the destination is a collection type, emit a constructor
            if matches!(value, Operand::Constant(Constant::Unit)) {
                let local_type = func.locals[dst.local.0 as usize].type_id;
                let type_name = format_type(local_type, registry);
                if let Some(code) = emit_collection_constructor(&type_name, dst.local.0) {
                    out.push_str(&code);
                    return;
                }
                // Also handle String() — empty String construction
                if type_name == "GorgetString" {
                    let _ = writeln!(out, "        _{id} = gorget_string_new(\"\");", id = dst.local.0);
                    return;
                }
                return;
            }
            let dst_str = format_place_typed(dst, Some(func), registry);
            // When assigning Null to an enum-typed local (e.g., Option None), emit tagged struct
            if matches!(value, Operand::Constant(Constant::Null)) {
                let local_type = func.locals[dst.local.0 as usize].type_id;
                if let Some(GirType::Named(name)) = registry.get(local_type) {
                    if let Some(type_def) = registry.get_type_def(name) {
                        if let TypeDefKind::Enum(ref e) = type_def.kind {
                            let none_tag = e.variants.iter().position(|v| v.name == "None")
                                .unwrap_or(e.variants.len() - 1);
                            let _ = writeln!(out, "        {dst_str} = ({name}){{.tag = {none_tag}}};");
                            return;
                        }
                    }
                }
            }
            // When assigning a string literal to a Str or GorgetString destination, wrap appropriately
            // Use resolve_place_c_type to handle field projections (e.g., (*_2).de_err = "...")
            if let Operand::Constant(Constant::Str(s)) = value {
                let final_type = resolve_place_c_type(dst, func, registry, type_overrides);
                if final_type == "Str" {
                    let escaped = escape_c_string(s);
                    let _ = writeln!(out, "        {dst_str} = gorget_str_from_literal(\"{escaped}\", {});", s.len());
                    return;
                }
                if final_type == "GorgetString" {
                    let escaped = escape_c_string(s);
                    let _ = writeln!(out, "        {dst_str} = gorget_string_new(\"{escaped}\");");
                    return;
                }
            }
            let val_str = format_operand(value, func, registry);
            // Coerce GorgetString → Str when destination is Str-typed
            // Use resolve_place_c_type for field projections (e.g., (*_3).ser_out = gorget_str_cat_result)
            let dst_c_type = resolve_place_c_type(dst, func, registry, type_overrides);
            let src_c_type = match value {
                Operand::Copy(p) | Operand::Move(p) => {
                    effective_c_type(p.local.0 as usize, func, registry, type_overrides)
                }
                _ => {
                    let src_type = operand_type(value, func);
                    src_type.map(|t| format_type(t, registry)).unwrap_or_default()
                }
            };
            // Implicit unwrap: src is Option__T but dst is T (GIR lowered .unwrap() as no-op)
            if src_c_type.starts_with("Option__") && !dst_c_type.starts_with("Option__") {
                let _ = writeln!(out, "        {dst_str} = {val_str}.data.Some._0;");
                return;
            }
            // Implicit Result::Ok wrap: src is raw T but dst is Result__T__E
            // Only wrap when source IR type is genuinely non-Result (primitive/concrete)
            if dst_c_type.starts_with("Result__") && !src_c_type.starts_with("Result__") && src_c_type != "void" {
                let src_ir_is_wrapper = match value {
                    Operand::Copy(p) | Operand::Move(p) => {
                        let t = format_type(func.locals[p.local.0 as usize].type_id, registry);
                        t.starts_with("Result__")
                    }
                    _ => false,
                };
                if !src_ir_is_wrapper {
                    let _ = writeln!(out, "        {dst_str} = ({dst_c_type}){{.tag = 0, .data.Ok._0 = {val_str}}};");
                    return;
                }
            }
            // Implicit Option::Some wrap: src is raw T but dst is Option__T
            if dst_c_type.starts_with("Option__") && !src_c_type.starts_with("Option__") && src_c_type != "void" {
                let src_ir_is_wrapper = match value {
                    Operand::Copy(p) | Operand::Move(p) => {
                        let t = format_type(func.locals[p.local.0 as usize].type_id, registry);
                        t.starts_with("Option__")
                    }
                    _ => false,
                };
                if !src_ir_is_wrapper {
                    let _ = writeln!(out, "        {dst_str} = ({dst_c_type}){{.tag = 0, .data.Some._0 = {val_str}}};");
                    return;
                }
            }
            // Closure packing: __Closure_N struct → GorgetClosure (escaped closure).
            // When a closure is returned from a function (return type int(int)),
            // the GIR local has FnPtr type (declared as GorgetClosure), and the RHS is
            // a __Closure_N StructInit. Heap-allocate the env and pack into GorgetClosure.
            // Detect via the local's GIR type (FnPtr) OR override (GorgetClosure from call result).
            let dst_local_idx = dst.local.0 as usize;
            let dst_is_escaped_closure = (dst_c_type == "GorgetClosure")
                || (dst_local_idx < func.locals.len()
                    && matches!(registry.get(func.locals[dst_local_idx].type_id), Some(GirType::FnPtr { .. })));
            // FuncRef → GorgetClosure: named function assigned to an escaped Callable local.
            // Pack using the adapter that has the (void* env, params...) ABI.
            if dst_is_escaped_closure {
                if let Operand::Constant(Constant::FuncRef(fn_name)) = value {
                    let c_name = mangle_name(fn_name);
                    let _ = writeln!(out, "        {dst_str} = (GorgetClosure){{.fn_ptr = (void*)__adapt_{c_name}, .env = NULL}};");
                    return;
                }
            }
            // Callable parameter → GorgetClosure local:
            // Callable function parameters are lowered as void* in the C ABI (the caller packs
            // the closure as `(void*)(void*[2]){fn_ptr, env}`). When assigning such a parameter
            // to a GorgetClosure local, dereference the void* to extract the struct value.
            if dst_is_escaped_closure && src_c_type == "void" {
                let _ = writeln!(out, "        {dst_str} = *(GorgetClosure*){val_str};");
                return;
            }
            if dst_is_escaped_closure && src_c_type.starts_with("__Closure_") {
                let struct_name = &src_c_type;
                let call_fn_name = format!("{struct_name}__call");
                // Check for ByMutRef captures (MutPtr fields) that need boxing.
                // When the closure env is heap-allocated, pointer fields still point to
                // the caller's stack. Box them so they survive after the caller returns.
                let mut boxed_fields: Vec<(String, String)> = Vec::new(); // (field_name, inner_c_type)
                if let Some(type_def) = registry.get_type_def(struct_name) {
                    if let TypeDefKind::Struct(ref sd) = type_def.kind {
                        for field in &sd.fields {
                            if let Some(GirType::MutPtr(inner)) = registry.get(field.type_id) {
                                let inner_c = format_type(*inner, registry);
                                boxed_fields.push((field.name.clone(), inner_c));
                            }
                        }
                    }
                }
                if boxed_fields.is_empty() {
                    // No ByMutRef captures — simple heap copy
                    let _ = writeln!(out,
                        "        {dst_str} = ({{ {struct_name}* __heap = ({struct_name}*)GORGET_ALLOC(sizeof({struct_name})); *__heap = {val_str}; (GorgetClosure){{.fn_ptr = (void*){call_fn_name}, .env = (void*)__heap}}; }});");
                } else {
                    // ByMutRef captures present — heap-copy env, then box each mutable capture.
                    let _ = writeln!(out, "        {dst_str} = ({{");
                    let _ = writeln!(out, "            {struct_name}* __heap = ({struct_name}*)GORGET_ALLOC(sizeof({struct_name}));");
                    let _ = writeln!(out, "            *__heap = {val_str};");
                    for (field_name, inner_c) in &boxed_fields {
                        // Allocate a heap cell and copy the current value from the stack pointer.
                        let _ = writeln!(out, "            {{ {inner_c}* __box = ({inner_c}*)GORGET_ALLOC(sizeof({inner_c}));");
                        let _ = writeln!(out, "              *__box = *__heap->{field_name};");
                        let _ = writeln!(out, "              __heap->{field_name} = __box; }}");
                    }
                    let _ = writeln!(out, "            (GorgetClosure){{.fn_ptr = (void*){call_fn_name}, .env = (void*)__heap}};");
                    let _ = writeln!(out, "        }});");
                }
                return;
            }
            // Implicit is_none/is_some: src is Option__T but dst is bool
            // (handled above since bool doesn't start with Option__)
            if dst_c_type == "Str" && src_c_type == "GorgetString" {
                let _ = writeln!(out, "        {dst_str} = (Str){{ .data = {val_str}.data, .len = {val_str}.len }};");
            } else if dst_c_type == "GorgetString" && src_c_type == "Str" {
                let _ = writeln!(out, "        {dst_str} = gorget_string_from_str({val_str});");
            } else if dst_c_type == "Str" && src_c_type == "uint32_t" {
                // char → str coercion: encode Unicode codepoint to UTF-8 and wrap as Str
                let _ = writeln!(out, "        {dst_str} = codepoint_to_str((int64_t){val_str});");
            } else {
                let dst_type = func.locals[dst.local.0 as usize].type_id;
                let src_type = operand_type(value, func);
                // Box[Trait] ← Box[ConcreteType]: wrap in TraitObj for vtable dispatch
                if dst_c_type.starts_with("Box__") && src_c_type.starts_with("Box__") {
                    let dst_inner = &dst_c_type[5..];
                    let src_inner = &src_c_type[5..];
                    if dst_inner != src_inner && registry.get_type_def(&format!("{dst_inner}_VTable")).is_some() {
                        let vtable_inst = format!("{dst_inner}_for_{src_inner}_vtable");
                        let _ = writeln!(out, "        {dst_str} = ({dst_inner}_TraitObj){{.data = (void*){val_str}, .vtable = &{vtable_inst}}};");
                        return;
                    }
                }
                if needs_string_coercion(dst_type, src_type, registry) {
                    let _ = writeln!(out, "        {dst_str} = (Str){{ .data = {val_str}.data, .len = {val_str}.len }};");
                } else {
                    // Pointer→value coercion: dereference when assigning Ptr(T) to T
                    // (e.g., `_0 = _1` where _1 is `self` pointer and _0 is value type)
                    let need_deref = if let Operand::Copy(src_place) | Operand::Move(src_place) = value {
                        if src_place.projections.is_empty() {
                            let src_idx = src_place.local.0 as usize;
                            let dst_idx = dst.local.0 as usize;
                            if src_idx < func.locals.len() && dst_idx < func.locals.len() {
                                let src_tid = func.locals[src_idx].type_id;
                                let dst_tid = func.locals[dst_idx].type_id;
                                let s_is_ptr = matches!(registry.get(src_tid), Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_)));
                                let d_is_ptr = matches!(registry.get(dst_tid), Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_)));
                                s_is_ptr && !d_is_ptr && dst_tid != UNIT_TYPE
                            } else { false }
                        } else { false }
                    } else { false };
                    if need_deref {
                        let _ = writeln!(out, "        {dst_str} = (*{val_str});");
                    } else {
                        let _ = writeln!(out, "        {dst_str} = {val_str};");
                    }
                }
            }

            // In test functions, register droppable user-named locals on the cleanup stack
            // so they're cleaned up if gorget_panic() calls longjmp (test fails).
            // Only push once per local (first creation, not reassignments).
            if func.is_test_fn && dst.projections.is_empty() {
                let local_id = dst.local.0;
                if !test_cleanup_pushed.contains(&local_id) {
                    if let Some(local) = func.locals.get(local_id as usize) {
                        if local.name_hint.is_some() {
                            if let Some(push_code) = test_cleanup_push_code(local_id, func, registry, type_overrides) {
                                out.push_str(&push_code);
                                test_cleanup_pushed.insert(local_id);
                            }
                        }
                    }
                }
            }
        }

        Instruction::BinOp { dst, op, type_id, lhs, rhs } => {
            // Skip void-typed BinOp results (from unresolved expression types)
            if *type_id == UNIT_TYPE { return; }
            let c_type = format_type(*type_id, registry);
            let lhs_str = format_operand(lhs, func, registry);
            let rhs_str = format_operand(rhs, func, registry);
            // Check for collection/string Add requiring special handling
            let lhs_effective = match lhs {
                Operand::Copy(p) | Operand::Move(p) =>
                    effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                _ => c_type.clone(),
            };
            if *op == BinOp::Add && (lhs_effective.starts_with("GorgetArray")
                || lhs_effective.starts_with("Vector__"))
            {
                // Vector + Vector = clone lhs then extend with rhs
                let _ = writeln!(out,
                    "        _{id} = gorget_array_clone(&{lhs_str}); gorget_array_extend(&_{id}, &{rhs_str});",
                    id = dst.0);
            } else if *op == BinOp::Pow {
                // Power: use (int64_t)pow() for integer types
                let _ = writeln!(out, "        _{id} = ({c_type})pow((double){lhs_str}, (double){rhs_str});", id = dst.0);
            } else if registry.get_type_def(&lhs_effective).is_some() {
                // Struct operand — dispatch to operator overload equip method
                let trait_method = match op {
                    BinOp::Add => "add",
                    BinOp::Sub => "sub",
                    BinOp::Mul => "mul",
                    BinOp::Div => "div",
                    BinOp::Rem => "rem",
                    BinOp::Mod => "mod",
                    _ => "",
                };
                if !trait_method.is_empty() {
                    let _ = writeln!(out, "        _{id} = {lhs_effective}__{trait_method}({lhs_str}, {rhs_str});", id = dst.0);
                } else {
                    let op_str = format_binop(*op);
                    let _ = writeln!(out, "        _{id} = {lhs_str} {op_str} {rhs_str};", id = dst.0);
                }
            } else if matches!(op, BinOp::AddWrap | BinOp::SubWrap | BinOp::MulWrap) {
                // Wrapping arithmetic: cast to unsigned, operate, cast back
                let op_str = match op {
                    BinOp::AddWrap => "+",
                    BinOp::SubWrap => "-",
                    BinOp::MulWrap => "*",
                    _ => unreachable!(),
                };
                let _ = writeln!(out,
                    "        _{id} = ({c_type})((uint64_t){lhs_str} {op_str} (uint64_t){rhs_str});",
                    id = dst.0);
            } else if *op == BinOp::Mod && (c_type == "double" || c_type == "float") {
                // Float true modulo (destination is float): fmod + sign correction
                let _ = writeln!(out, "        {{ double _rem_{id} = fmod((double){lhs_str}, (double){rhs_str}); _{id} = (_rem_{id} != 0.0 && ((_rem_{id} < 0.0) != ((double){rhs_str} < 0.0))) ? _rem_{id} + (double){rhs_str} : _rem_{id}; }}", id = dst.0);
            } else if *op == BinOp::Mod && (lhs_effective == "double" || lhs_effective == "float") && (c_type == "int64_t" || c_type == "int32_t") {
                // Float operand with integer destination: true modulo
                let _ = writeln!(out, "        if ({rhs_str} == 0) {{ fprintf(stderr, \"gorget: division by zero\\n\"); exit(1); }}");
                let _ = writeln!(out, "        {{ int64_t _a_{id} = (int64_t){lhs_str}; int64_t _b_{id} = (int64_t){rhs_str}; int64_t _rem_{id} = _a_{id} % _b_{id}; _{id} = (_rem_{id} != 0 && ((_rem_{id} ^ _b_{id}) < 0)) ? _rem_{id} + _b_{id} : _rem_{id}; }}", id = dst.0);
            } else if *op == BinOp::Rem && (c_type == "double" || c_type == "float") {
                // Float remainder (destination is float): use fmod() instead of %
                let _ = writeln!(out, "        _{id} = fmod((double){lhs_str}, (double){rhs_str});", id = dst.0);
            } else if *op == BinOp::Rem && (lhs_effective == "double" || lhs_effective == "float") && (c_type == "int64_t" || c_type == "int32_t") {
                // Float operand with integer destination: cast to int first, then integer remainder
                let _ = writeln!(out, "        if ({rhs_str} == 0) {{ fprintf(stderr, \"gorget: division by zero\\n\"); exit(1); }}");
                let _ = writeln!(out, "        _{id} = (int64_t){lhs_str} % (int64_t){rhs_str};", id = dst.0);
            } else if matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul) && c_type == "int64_t" && !module.runtime.overflow_wrap {
                // Checked arithmetic: abort on overflow (only for integral types)
                // Check if either operand is a float — if so, cast and skip overflow check
                let lhs_is_float = lhs_effective == "double" || lhs_effective == "float";
                let rhs_is_float = match rhs {
                    Operand::Copy(p) | Operand::Move(p) =>
                        matches!(effective_c_type(p.local.0 as usize, func, registry, type_overrides).as_str(), "double" | "float"),
                    _ => false,
                };
                if lhs_is_float || rhs_is_float {
                    let op_str = format_binop(*op);
                    let _ = writeln!(out, "        _{id} = (int64_t)((double){lhs_str} {op_str} (double){rhs_str});", id = dst.0);
                } else {
                    let builtin = match op {
                        BinOp::Add => "__builtin_add_overflow",
                        BinOp::Sub => "__builtin_sub_overflow",
                        BinOp::Mul => "__builtin_mul_overflow",
                        _ => unreachable!(),
                    };
                    let _ = writeln!(out, "        if ({builtin}({lhs_str}, {rhs_str}, &_{id})) {{ fprintf(stderr, \"gorget: integer overflow\\n\"); exit(1); }}", id = dst.0);
                }
            } else if *op == BinOp::Mod && (c_type == "int64_t" || c_type == "int32_t") {
                // Integer true modulo with division by zero check
                let _ = writeln!(out, "        if ({rhs_str} == 0) {{ fprintf(stderr, \"gorget: division by zero\\n\"); exit(1); }}");
                let _ = writeln!(out, "        {{ int64_t _rem_{id} = {lhs_str} % {rhs_str}; _{id} = (_rem_{id} != 0 && ((_rem_{id} ^ {rhs_str}) < 0)) ? _rem_{id} + {rhs_str} : _rem_{id}; }}", id = dst.0);
            } else if matches!(op, BinOp::Div | BinOp::Rem) && (c_type == "int64_t" || c_type == "int32_t") {
                // Division by zero check
                let _ = writeln!(out, "        if ({rhs_str} == 0) {{ fprintf(stderr, \"gorget: division by zero\\n\"); exit(1); }}");
                let op_str = format_binop(*op);
                let _ = writeln!(out, "        _{id} = {lhs_str} {op_str} {rhs_str};", id = dst.0);
            } else {
                let op_str = format_binop(*op);
                let _ = writeln!(out, "        _{id} = {lhs_str} {op_str} {rhs_str};", id = dst.0);
            }
        }

        Instruction::UnOp { dst, op, type_id, operand } => {
            let c_type = format_type(*type_id, registry);
            let val_str = format_operand(operand, func, registry);
            // Check for struct operand requiring operator overload
            let val_effective = match operand {
                Operand::Copy(p) | Operand::Move(p) =>
                    effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                _ => c_type.clone(),
            };
            if *op == UnOp::Neg && registry.get_type_def(&val_effective).is_some() {
                let _ = writeln!(out, "        _{id} = {val_effective}__neg({val_str});", id = dst.0);
            } else {
                let op_str = match op {
                    UnOp::Neg => "-",
                    UnOp::Not => "!",
                    UnOp::BitNot => "~",
                };
                let _ = writeln!(out, "        _{id} = {op_str}{val_str};", id = dst.0);
            }
        }

        Instruction::Cmp { dst, op, lhs, rhs, .. } => {
            let lhs_str = format_operand(lhs, func, registry);
            let rhs_str = format_operand(rhs, func, registry);
            let operand_effective = |operand: &Operand| -> String {
                match operand {
                    Operand::Copy(p) | Operand::Move(p) =>
                        effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                    Operand::Constant(Constant::Str(_)) => "Str".to_string(),
                    _ => "int64_t".to_string(),
                }
            };
            let lhs_effective = operand_effective(lhs);
            let rhs_effective = operand_effective(rhs);
            // Wrap a string-constant operand so it becomes a Str struct, not char*
            let wrap_str_const = |operand: &Operand, formatted: &str| -> String {
                match operand {
                    Operand::Constant(Constant::Str(s)) => {
                        let escaped = escape_c_string(s);
                        format!("gorget_str_from_literal(\"{escaped}\", {})", s.len())
                    }
                    _ => formatted.to_string(),
                }
            };
            if lhs_effective == "Str" || rhs_effective == "Str" {
                // String comparison using gorget_str_eq / gorget_str_cmp
                let lhs_cmp = wrap_str_const(lhs, &lhs_str);
                let rhs_cmp = wrap_str_const(rhs, &rhs_str);
                match op {
                    CmpOp::Eq => {
                        let _ = writeln!(out, "        _{id} = gorget_str_eq({lhs_cmp}, {rhs_cmp});", id = dst.0);
                    }
                    CmpOp::Ne => {
                        let _ = writeln!(out, "        _{id} = !gorget_str_eq({lhs_cmp}, {rhs_cmp});", id = dst.0);
                    }
                    _ => {
                        let op_str = format_cmpop(*op);
                        let _ = writeln!(out, "        _{id} = gorget_str_cmp({lhs_cmp}, {rhs_cmp}) {op_str} 0;", id = dst.0);
                    }
                }
            } else if registry.get_type_def(&lhs_effective).is_some() {
                // Struct comparison — dispatch to equip method
                match op {
                    CmpOp::Eq => {
                        let _ = writeln!(out, "        _{id} = {lhs_effective}__eq({lhs_str}, {rhs_str});", id = dst.0);
                    }
                    CmpOp::Ne => {
                        let _ = writeln!(out, "        _{id} = !{lhs_effective}__eq({lhs_str}, {rhs_str});", id = dst.0);
                    }
                    _ => {
                        let op_str = format_cmpop(*op);
                        let _ = writeln!(out, "        _{id} = {lhs_effective}__cmp({lhs_str}, {rhs_str}) {op_str} 0;", id = dst.0);
                    }
                }
            } else {
                let op_str = format_cmpop(*op);
                let _ = writeln!(out, "        _{id} = {lhs_str} {op_str} {rhs_str};", id = dst.0);
            }
        }

        Instruction::Call { dst, func: func_name, args } => {
            // Check if this is an indirect call through a Callable parameter (void* typed).
            // When a function takes `Callable[sig]` the GIR lowers the param as void*,
            // and calls use the parameter name as func_name. Detect this and emit a
            // function pointer cast + indirect call.
            if try_emit_callable_indirect_call(out, func_name, dst, args, func, registry, type_overrides, module) {
                // handled
            }
            // Check if this is a collection method call (Vector__T__push, etc.)
            else if let Some(rewrite) = try_rewrite_collection_method(func_name) {
                emit_collection_method_call(out, &rewrite, dst, args, func, registry, func_name, type_overrides);
            }
            // Check for inline method (pop, sort, Option/Result methods, etc.)
            else if let Some(inline) = try_inline_method(func_name) {
                emit_inline_method(out, &inline, dst, args, func, registry, func_name, type_overrides);
            }
            // Higher-order collection methods: filter, map, fold, reduce, enumerate
            else if let Some(code) = try_emit_higher_order_method(func_name, dst, args, func, registry, type_overrides, module) {
                out.push_str(&code);
            }
            // Primitive static methods: int64_t__parse, double__parse, bool__parse, *__default
            else if let Some(code) = try_emit_primitive_static_method(func_name, dst, args, func, registry) {
                out.push_str(&code);
            }
            // File static/instance methods: File__create, File__open, File__read_all, File__write
            else if func_name.starts_with("File__") || func_name.starts_with("gorget_file_") {
                let c_func = map_stdlib_name(func_name);
                match func_name.as_str() {
                    "File__create" | "gorget_file_create" => {
                        // File.create(path) → gorget_file_open(path, "w")
                        if let Some(dst_id) = dst {
                            let path_arg = if !args.is_empty() {
                                extract_cstr_operand(&args[0], func, registry, type_overrides)
                            } else { "\"\"".to_string() };
                            let _ = writeln!(out, "        _{id} = gorget_file_open({path_arg}, \"w\");", id = dst_id.0);
                        }
                    }
                    "File__open" | "gorget_file_open" => {
                        // File.open(path) → gorget_file_open(path, "r")
                        if let Some(dst_id) = dst {
                            let path_arg = if !args.is_empty() {
                                extract_cstr_operand(&args[0], func, registry, type_overrides)
                            } else { "\"\"".to_string() };
                            let _ = writeln!(out, "        _{id} = gorget_file_open({path_arg}, \"r\");", id = dst_id.0);
                        }
                    }
                    "File__read_all" | "gorget_file_read_all" => {
                        // File.read_all(&self) → Result[String, str] with UTF-8 validation
                        if let Some(dst_id) = dst {
                            let self_arg = if !args.is_empty() {
                                let s = format_operand(&args[0], func, registry);
                                // Check if pointer
                                if let Operand::Copy(p) | Operand::Move(p) = &args[0] {
                                    let idx = p.local.0 as usize;
                                    let ct = effective_c_type(idx, func, registry, type_overrides);
                                    if ct.ends_with('*') { s } else { format!("&{s}") }
                                } else { format!("&{s}") }
                            } else { "NULL".to_string() };
                            let c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                            if c_type.starts_with("Result__") {
                                let _ = writeln!(out,
                                    "        _{id} = ({{ GorgetString __gs = gorget_file_read_all({self_arg}); \
                                    {c_type} __wr; \
                                    if (gorget_utf8_validate(__gs.data, __gs.len)) {{ __wr.tag = 0; __wr.data.Ok._0 = __gs; }} \
                                    else {{ gorget_string_free(&__gs); __wr.tag = 1; __wr.data.Error._0 = gorget_str_from_literal(\"invalid UTF-8\", 13); }} \
                                    __wr; }});",
                                    id = dst_id.0);
                            } else {
                                let _ = writeln!(out, "        _{id} = gorget_file_read_all({self_arg});", id = dst_id.0);
                            }
                        }
                    }
                    _ => {
                        // Generic file method: pass self as pointer, coerce Str args to .data
                        let mut arg_parts: Vec<String> = Vec::new();
                        for (i, arg) in args.iter().enumerate() {
                            let arg_str = format_operand(arg, func, registry);
                            if i == 0 {
                                // self → pointer
                                if let Operand::Copy(p) | Operand::Move(p) = arg {
                                    let idx = p.local.0 as usize;
                                    let ct = effective_c_type(idx, func, registry, type_overrides);
                                    if ct.ends_with('*') { arg_parts.push(arg_str); }
                                    else { arg_parts.push(format!("&{arg_str}")); }
                                } else { arg_parts.push(format!("&{arg_str}")); }
                            } else {
                                // Coerce Str to const char*
                                let is_str = if let Operand::Copy(p) | Operand::Move(p) = arg {
                                    let idx = p.local.0 as usize;
                                    let ct = effective_c_type(idx, func, registry, type_overrides);
                                    ct == "Str" || ct == "GorgetString"
                                } else { false };
                                if is_str { arg_parts.push(format!("{arg_str}.data")); }
                                else { arg_parts.push(arg_str); }
                            }
                        }
                        let args_str = arg_parts.join(", ");
                        if let Some(dst_id) = dst {
                            let _ = writeln!(out, "        _{id} = {c_func}({args_str});", id = dst_id.0);
                        } else {
                            let _ = writeln!(out, "        {c_func}({args_str});");
                        }
                    }
                }
            }
            // bytes_to_str(v) → Result[str, str] with UTF-8 validation
            else if func_name == "bytes_to_str" || func_name == "gg_bytes_to_str" {
                if let Some(dst_id) = dst {
                    if !args.is_empty() {
                        let arg_str = format_operand(&args[0], func, registry);
                        // Check if argument is already a pointer
                        let arg_ref = if let Operand::Copy(p) | Operand::Move(p) = &args[0] {
                            let idx = p.local.0 as usize;
                            let ct = effective_c_type(idx, func, registry, type_overrides);
                            if ct.ends_with('*') { arg_str } else { format!("&{arg_str}") }
                        } else { format!("&{arg_str}") };
                        let c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                        if c_type.starts_with("Result__") {
                            let _ = writeln!(out,
                                "        _{id} = ({{ const char* __bs = gorget_bytes_to_str({arg_ref}); \
                                size_t __bslen = strlen(__bs); \
                                {c_type} __wr; \
                                if (!gorget_utf8_validate(__bs, __bslen)) {{ \
                                    GORGET_FREE((void*)__bs, __bslen + 1); \
                                    __wr.tag = 1; __wr.data.Error._0 = gorget_str_from_literal(\"invalid UTF-8 in byte buffer\", 28); \
                                }} else {{ \
                                    __wr.tag = 0; __wr.data.Ok._0 = gorget_str_from_cstr(__bs); \
                                }} \
                                __wr; }});",
                                id = dst_id.0);
                        } else {
                            let _ = writeln!(out, "        _{id} = gorget_str_from_cstr(gorget_bytes_to_str({arg_ref}));", id = dst_id.0);
                        }
                    }
                }
            }
            // Special handling for Regex__ and Match__ decl_method calls.
            // These need custom arg coercion (Str → .data) and option wrapping.
            else if (func_name.starts_with("Regex__") || func_name.starts_with("Match__"))
                && !all_functions.iter().any(|f| f.name.as_str() == func_name)
            {
                let c_func = map_stdlib_name(func_name);
                // Format args: arg[0] = self pointer (pass as-is), arg[1+] coerce Str → .data
                let mut arg_parts: Vec<String> = Vec::new();
                for (i, arg) in args.iter().enumerate() {
                    let arg_str = format_operand(arg, func, registry);
                    if i == 0 {
                        arg_parts.push(arg_str); // self is already GorgetRegex*/GorgetRegexMatch*
                    } else {
                        let is_str = if let Operand::Copy(p) | Operand::Move(p) = arg {
                            let ct = effective_c_type(p.local.0 as usize, func, registry, type_overrides);
                            ct == "Str" || ct == "GorgetString"
                        } else {
                            // Constant::Str literals are already const char* in C — do NOT append .data
                            false
                        };
                        if is_str {
                            arg_parts.push(format!("{arg_str}.data"));
                        } else {
                            arg_parts.push(arg_str);
                        }
                    }
                }
                // Inject trailing args for methods with implicit parameters
                match func_name.as_str() {
                    "Regex__find" => arg_parts.push("0".to_string()),   // start_offset = 0
                    "Regex__split" => arg_parts.push("-1".to_string()),  // limit = -1 (no limit)
                    _ => {}
                }
                let args_str = arg_parts.join(", ");
                if let Some(dst_id) = dst {
                    let c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                    if c_type.starts_with("Option__") {
                        // GorgetRegexMatch sentinel: .start == -1 means no match
                        if matches!(c_func, "gorget_regex_find" | "gorget_regex_find_at" | "gorget_regex_fullmatch") {
                            let _ = writeln!(out,
                                "        _{id} = ({{ GorgetRegexMatch __raw = {c_func}({args_str}); \
                                {c_type} __opt; if (__raw.start != -1) {{ __opt.tag = 0; __opt.data.Some._0 = __raw; }} \
                                else {{ __opt.tag = 1; }} __opt; }});",
                                id = dst_id.0);
                        } else {
                            // const char* nullable → Option[str] (gorget_regex_match_group etc.)
                            let _ = writeln!(out,
                                "        _{id} = ({{ const char* __raw = {c_func}({args_str}); \
                                {c_type} __opt; if (__raw) {{ __opt.tag = 0; __opt.data.Some._0 = gorget_str_from_cstr(__raw); }} \
                                else {{ __opt.tag = 1; }} __opt; }});",
                                id = dst_id.0);
                        }
                    } else if returns_cstr(c_func) {
                        let _ = writeln!(out, "        _{id} = gorget_str_from_cstr({c_func}({args_str}));", id = dst_id.0);
                    } else {
                        let _ = writeln!(out, "        _{id} = {c_func}({args_str});", id = dst_id.0);
                    }
                } else {
                    let _ = writeln!(out, "        {c_func}({args_str});");
                }
            }
            // Stdlib functions that return Result via last_error pattern (udp_bind, socket_connect, crypto_*, etc.)
            else if let Some(code) = try_emit_result_wrapped_call(func_name, dst, args, func, registry, type_overrides) {
                out.push_str(&code);
            }
            // Image/audio/deflate out-parameter calls — C runtime uses out-param ABI
            else if let Some(code) = try_emit_outparam_call(func_name, dst, args, func, registry, type_overrides) {
                out.push_str(&code);
            }
            // sleep(arg) — dispatch based on argument type:
            //   float arg → std.time.sleep(seconds) → gorget_sleep_ms((int64_t)(arg * 1000))
            //   int arg   → std.async.sleep(ms)     → gorget_reactor_sleep_ms(arg)
            else if func_name == "sleep" || func_name == "gg_sleep" {
                let is_int_arg = !args.is_empty() && {
                    match &args[0] {
                        Operand::Constant(Constant::I64(_)) => true,
                        Operand::Constant(Constant::F64(_)) => false,
                        Operand::Copy(p) | Operand::Move(p) => {
                            let t = func.locals[p.local.0 as usize].type_id;
                            t == I64_TYPE || t == I32_TYPE
                        }
                        _ => false,
                    }
                };
                if !args.is_empty() {
                    let arg = format_operand(&args[0], func, registry);
                    if is_int_arg {
                        let _ = writeln!(out, "        gorget_reactor_sleep_ms({arg});");
                    } else {
                        let _ = writeln!(out, "        gorget_sleep_ms((int64_t)({arg} * 1000));");
                    }
                }
                if let Some(dst_id) = dst {
                    let _ = writeln!(out, "        _{id} = 0;", id = dst_id.0);
                }
            }
            // Check if this is a collection/Box/String constructor
            else if is_collection_constructor(func_name) {
                if let Some(dst_id) = dst {
                    if let Some(code) = emit_collection_constructor(func_name, dst_id.0) {
                        out.push_str(&code);
                    } else {
                        // Unknown collection type, zero-initialize as fallback
                        let type_name = func_name.strip_suffix("__new").unwrap_or(func_name);
                        let c_type = collection_type_alias(type_name).unwrap_or(type_name);
                        let _ = writeln!(out, "        _{id} = ({c_type}){{0}};", id = dst_id.0);
                    }
                }
            }
            // Check if this is a constructor call (func_name is a type name)
            else if registry.get_type_def(func_name).is_some() {
                // Constructor: emit zero-initialized struct with field values
                if let Some(dst_id) = dst {
                    let type_def = registry.get_type_def(func_name).unwrap();
                    if let TypeDefKind::Struct(ref s) = type_def.kind {
                        // Assign only — local declared in prologue with correct type
                        let _ = write!(out, "        _{id} = ({func_name}){{", id = dst_id.0);
                        for (i, (arg, field)) in args.iter().zip(s.fields.iter()).enumerate() {
                            if i > 0 { out.push_str(", "); }
                            let val = coerce_constructor_arg(arg, &field.type_id, func, registry);
                            let _ = write!(out, ".{} = {val}", field.name);
                        }
                        if args.is_empty() {
                            out.push('0');
                        }
                        out.push_str("};\n");
                    } else {
                        let _ = writeln!(out, "        _{id} = ({func_name}){{0}};", id = dst_id.0);
                    }
                }
            }
            // Free function `len(x)` — dispatch based on argument type
            else if func_name == "len" && args.len() == 1 {
                if let Some(dst_id) = dst {
                    let arg_str = format_operand(&args[0], func, registry);
                    let arg_type = match &args[0] {
                        Operand::Copy(p) | Operand::Move(p) =>
                            effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                        _ => "int64_t".to_string(),
                    };
                    let deref_arg = if arg_type.ends_with('*') {
                        format!("(*{arg_str})")
                    } else {
                        arg_str.clone()
                    };
                    if arg_type.contains("GorgetArray") || arg_type.starts_with("Vector") {
                        let _ = writeln!(out, "        _{id} = (int64_t){deref_arg}.len;", id = dst_id.0);
                    } else if arg_type == "Str" || arg_type == "Str*" || arg_type == "const Str*" {
                        let _ = writeln!(out, "        _{id} = gorget_str_codepoint_count({deref_arg});", id = dst_id.0);
                    } else if arg_type.contains("GorgetString") {
                        let _ = writeln!(out, "        _{id} = gorget_str_codepoint_count((Str){{ .data = {deref_arg}.data, .len = {deref_arg}.len }});", id = dst_id.0);
                    } else if arg_type.contains("GorgetMap") || arg_type.contains("GorgetSet") || arg_type.contains("GorgetDict") {
                        let _ = writeln!(out, "        _{id} = (int64_t){deref_arg}.count;", id = dst_id.0);
                    } else {
                        // Dispatch to equip method: TypeName__len(&arg)
                        let type_name = arg_type.trim_start_matches("const ").trim_end_matches('*');
                        let _ = writeln!(out, "        _{id} = {type_name}__len(&{arg_str});", id = dst_id.0);
                    }
                }
            }
            // Runtime functions that take GorgetArray* pointer arguments
            // Skip this path if the function is a user-defined Gorget function (it handles its own args)
            else if (takes_array_ptr_args(func_name) || takes_array_ptr_args(map_stdlib_name(func_name)))
                && !all_functions.iter().any(|f| f.name.as_str() == func_name)
            {
                let func_name_mapped = map_stdlib_name(func_name);
                let mut arg_parts: Vec<String> = Vec::new();
                for arg in args {
                    let arg_str = format_operand(arg, func, registry);
                    let needs_addr = if let Operand::Copy(p) | Operand::Move(p) = arg {
                        let idx = p.local.0 as usize;
                        let c_type = effective_c_type(idx, func, registry, type_overrides);
                        (c_type == "GorgetArray" || c_type.starts_with("Vector__"))
                            && !c_type.ends_with('*')
                    } else {
                        false
                    };
                    if needs_addr {
                        arg_parts.push(format!("&{arg_str}"));
                    } else {
                        let is_str = if let Operand::Copy(p) | Operand::Move(p) = arg {
                            let idx = p.local.0 as usize;
                            let c_type = effective_c_type(idx, func, registry, type_overrides);
                            c_type == "Str" || c_type == "GorgetString"
                        } else { false };
                        if is_str && is_cstr_param_fn(func_name_mapped) {
                            arg_parts.push(format!("{arg_str}.data"));
                        } else {
                            arg_parts.push(arg_str);
                        }
                    }
                }
                let args_str = arg_parts.join(", ");
                let ret_cstr = returns_cstr(func_name_mapped);
                if let Some(dst_id) = dst {
                    let c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                    // Result wrapping for takes_array_ptr_args in Call path
                    if c_type.starts_with("Result__") && !func_name.starts_with("__result_") {
                        let raw_expr = if ret_cstr {
                            format!("gorget_str_from_cstr({func_name_mapped}({args_str}))")
                        } else {
                            format!("{func_name_mapped}({args_str})")
                        };
                        let _ = writeln!(out,
                            "        _{id} = ({{ {c_type} __wr; \
                            if (GORGET_TRY) {{ __typeof__(_{id}.data.Ok._0) __raw = {raw_expr}; GORGET_CATCH_END; \
                            __wr.tag = 0; __wr.data.Ok._0 = __raw; }} \
                            else {{ GorgetError __err = GORGET_CATCH_ERROR(); GORGET_CATCH_END; \
                            __wr.tag = 1; __wr.data.Error._0 = gorget_str_from_cstr(__err.message); }} \
                            __wr; }});",
                            id = dst_id.0);
                    } else if ret_cstr && c_type == "Str" {
                        let _ = writeln!(out, "        _{id} = gorget_str_from_cstr({func_name_mapped}({args_str}));", id = dst_id.0);
                    } else {
                        let _ = writeln!(out, "        _{id} = {func_name_mapped}({args_str});", id = dst_id.0);
                    }
                } else {
                    let _ = writeln!(out, "        {func_name_mapped}({args_str});");
                }
            }
            else {
                // Only apply stdlib name mapping if no user-defined function exists
                let is_user_fn = all_functions.iter().any(|f| f.name.as_str() == func_name);
                let mapped_name = if is_user_fn { func_name } else { map_stdlib_name(func_name) };
                // Float-aware dispatch for abs/min/max
                let mapped_name = match mapped_name {
                    "gorget_abs" | "gorget_min" | "gorget_max" if has_float_arg_with_overrides(args, func, type_overrides) => {
                        match mapped_name {
                            "gorget_abs" => "fabs",
                            "gorget_min" => "fmin",
                            "gorget_max" => "fmax",
                            _ => mapped_name,
                        }
                    }
                    _ => mapped_name,
                };
                let c_name = mangle_name(mapped_name);
                // Format args: use cstr coercion for functions that take const char*
                let args_str = if is_cstr_param_fn(mapped_name) {
                    // format_cstr_fn_args handles locals but not GlobalRef operands.
                    // Fix up GlobalRef Str args here where we have access to the module.
                    let mut base = format_cstr_fn_args(args, func, registry);
                    for arg in args {
                        if let Operand::Constant(Constant::GlobalRef(gname)) = arg {
                            // Check if this global is Str-typed
                            let is_str_global = module.globals.iter().any(|g| {
                                g.name == *gname && format_type(g.type_id, registry) == "Str"
                            });
                            if is_str_global {
                                base = base.replace(gname.as_str(), &format!("gorget_str_to_cstr({gname})"));
                            }
                        }
                    }
                    base
                } else {
                    format_args_with_coercion(args, func, registry, type_overrides, &c_name, all_functions)
                };
                let ret_cstr = returns_cstr(mapped_name);
                // Str__slice: gorget_str_slice takes Str by value, not Str*.
                // If self is a borrow (Str*), dereference it.
                if func_name == "Str__slice" {
                    // Build args with first arg dereferenced if it's a Str pointer
                    let slice_args = if !args.is_empty() {
                        let self_str = format_operand(&args[0], func, registry);
                        let self_local_idx = match &args[0] {
                            Operand::Copy(p) | Operand::Move(p) => p.local.0 as usize,
                            _ => usize::MAX,
                        };
                        let self_type = if self_local_idx < func.locals.len() {
                            format_type(func.locals[self_local_idx].type_id, registry)
                        } else { String::new() };
                        let self_val = if self_type.ends_with("*") {
                            format!("(*{self_str})")
                        } else {
                            self_str.clone()
                        };
                        let rest: Vec<String> = args[1..].iter()
                            .map(|a| format_operand(a, func, registry))
                            .collect();
                        if rest.is_empty() {
                            self_val
                        } else {
                            format!("{self_val}, {}", rest.join(", "))
                        }
                    } else {
                        args_str.clone()
                    };
                    let _ = writeln!(out, "        {c_name}({slice_args});");
                    if let Some(dst_id) = dst {
                        if !args.is_empty() {
                            let self_str = format_operand(&args[0], func, registry);
                            let _ = writeln!(out, "        _{id} = *{self_str};", id = dst_id.0);
                        }
                    }
                } else if let Some(dst_id) = dst {
                    let local_type = func.locals[dst_id.0 as usize].type_id;
                    if local_type == UNIT_TYPE && !type_overrides.contains_key(&(dst_id.0 as usize)) {
                        let _ = writeln!(out, "        {c_name}({args_str});");
                    } else {
                        let c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                        if ret_cstr && c_type == "GorgetString" {
                            let _ = writeln!(out, "        _{id} = gorget_string_adopt((char*){c_name}({args_str}));", id = dst_id.0);
                        } else if ret_cstr && c_type == "Str" {
                            let _ = writeln!(out, "        _{id} = gorget_str_from_cstr({c_name}({args_str}));", id = dst_id.0);
                        } else if c_type.starts_with("Option__")
                            && !c_name.ends_with("__upgrade")
                            && !c_name.ends_with("__recv_timeout")
                            && {
                                // Only wrap if the called function does NOT already return Option.
                                // For user functions that return raw int, we need to wrap.
                                let callee_returns_option = all_functions.iter()
                                    .find(|f| f.name.as_str() == func_name || mangle_name(&f.name) == c_name)
                                    .map(|f| format_type(f.return_type, registry))
                                    .as_ref()
                                    .map_or(false, |r| r.starts_with("Option__"));
                                !callee_returns_option
                            } {
                            // Option wrapping for runtime functions.
                            // GorgetRegexMatch uses .start == -1 sentinel; int options use >= 0.
                            // Note: __upgrade functions already return Option directly — skip wrapping.
                            let inner_is_match = c_type == "Option__Match"
                                || c_type == "Option__GorgetRegexMatch";
                            if inner_is_match {
                                let _ = writeln!(out,
                                    "        _{id} = ({{ GorgetRegexMatch __raw = {c_name}({args_str}); \
                                    {c_type} __opt; if (__raw.start != -1) {{ __opt.tag = 0; __opt.data.Some._0 = __raw; }} \
                                    else {{ __opt.tag = 1; }} __opt; }});",
                                    id = dst_id.0);
                            } else {
                                let _ = writeln!(out,
                                    "        _{id} = ({{ __typeof__(_{id}.data.Some._0) __raw = {c_name}({args_str}); \
                                    {c_type} __opt; if (__raw >= 0) {{ __opt.tag = 0; __opt.data.Some._0 = __raw; }} \
                                    else {{ __opt.tag = 1; }} __opt; }});",
                                    id = dst_id.0);
                            }
                        } else if c_type.starts_with("Result__") && !c_name.starts_with("__result_") && {
                            // Result wrapping: only when called function does NOT return Result
                            let callee_returns_result = all_functions.iter()
                                .find(|f| f.name.as_str() == func_name || mangle_name(&f.name) == c_name)
                                .map(|f| format_type(f.return_type, registry))
                                .as_ref()
                                .map_or(false, |r| r.starts_with("Result__"));
                            !callee_returns_result
                        } {
                            // Wrap raw value in Result::Ok
                            let raw_expr = if ret_cstr {
                                format!("gorget_str_from_cstr({c_name}({args_str}))")
                            } else {
                                format!("{c_name}({args_str})")
                            };
                            let _ = writeln!(out,
                                "        _{id} = ({{ {c_type} __wr; \
                                __wr.tag = 0; __wr.data.Ok._0 = {raw_expr}; __wr; }});",
                                id = dst_id.0);
                        } else {
                            let _ = writeln!(out, "        _{id} = {c_name}({args_str});", id = dst_id.0);
                        }
                    }
                } else {
                    let _ = writeln!(out, "        {c_name}({args_str});");
                }
            }
        }

        Instruction::CallExtern { dst, func: func_name, args } => {
            // __option_is_some(&option) → option->tag == 0
            // __option_is_none(&option) → option->tag != 0
            if func_name == "__option_is_some" || func_name == "__option_is_none" {
                if let Some(dst_id) = dst {
                    if !args.is_empty() {
                        let ptr = format_operand(&args[0], func, registry);
                        let op = if func_name == "__option_is_some" { "==" } else { "!=" };
                        let _ = writeln!(out, "        _{id} = ({ptr})->tag {op} 0;", id = dst_id.0);
                    }
                }
            }
            // __option_unwrap(&option) → option->data.Some._0
            // __result_unwrap(&result) → result->data.Ok._0
            // __option_unwrap_or(&option, default) → (option->tag == 0) ? option->data.Some._0 : default
            // __result_unwrap_or(&result, default) → (result->tag == 0) ? result->data.Ok._0 : default
            else if func_name.starts_with("__option_unwrap") || func_name.starts_with("__result_unwrap") {
                if let Some(dst_id) = dst {
                    if !args.is_empty() {
                        let ptr = format_operand(&args[0], func, registry);
                        let variant = if func_name.starts_with("__result") { "Ok" } else { "Some" };
                        if func_name.ends_with("_or") && args.len() > 1 {
                            let default_val = format_operand(&args[1], func, registry);
                            let _ = writeln!(out, "        _{id} = (({ptr})->tag == 0) ? ({ptr})->data.{variant}._0 : {default_val};",
                                id = dst_id.0);
                        } else {
                            let _ = writeln!(out, "        _{id} = ({ptr})->data.{variant}._0;", id = dst_id.0);
                        }
                    }
                }
            }
            // Primitive static methods: int64_t__parse, double__parse, bool__parse, *__default
            else if let Some(code) = try_emit_primitive_static_method(func_name, dst, args, func, registry) {
                out.push_str(&code);
            }
            // Stdlib functions that return Result via last_error pattern
            else if let Some(code) = try_emit_result_wrapped_call(func_name, dst, args, func, registry, type_overrides) {
                if func_name.contains("image") || func_name.contains("audio") || func_name.contains("deflate") {
                    eprintln!("[DEBUG-CALL] Caught by try_emit_result_wrapped_call: {func_name}");
                }
                out.push_str(&code);
            }
            // sleep(arg) — same int/float dispatch as the Call variant above
            else if func_name == "sleep" || func_name == "gg_sleep" {
                let is_int_arg = !args.is_empty() && {
                    match &args[0] {
                        Operand::Constant(Constant::I64(_)) => true,
                        Operand::Constant(Constant::F64(_)) => false,
                        Operand::Copy(p) | Operand::Move(p) => {
                            let t = func.locals[p.local.0 as usize].type_id;
                            t == I64_TYPE || t == I32_TYPE
                        }
                        _ => false,
                    }
                };
                if !args.is_empty() {
                    let arg = format_operand(&args[0], func, registry);
                    if is_int_arg {
                        let _ = writeln!(out, "        gorget_reactor_sleep_ms({arg});");
                    } else {
                        let _ = writeln!(out, "        gorget_sleep_ms((int64_t)({arg} * 1000));");
                    }
                }
                if let Some(dst_id) = dst {
                    let _ = writeln!(out, "        _{id} = 0;", id = dst_id.0);
                }
            }
            // Apply same collection/method rewrites as for Call instructions.
            // CallExtern targets include auto-registered user methods.
            else if let Some(rewrite) = try_rewrite_collection_method(func_name) {
                emit_collection_method_call(out, &rewrite, dst, args, func, registry, func_name, type_overrides);
            } else if let Some(inline) = try_inline_method(func_name) {
                emit_inline_method(out, &inline, dst, args, func, registry, func_name, type_overrides);
            } else if is_collection_constructor(func_name) {
                if let Some(dst_id) = dst {
                    if let Some(code) = emit_collection_constructor(func_name, dst_id.0) {
                        out.push_str(&code);
                    } else {
                        let type_name = func_name.strip_suffix("__new").unwrap_or(func_name);
                        let c_type = collection_type_alias(type_name).unwrap_or(type_name);
                        let _ = writeln!(out, "        _{id} = ({c_type}){{0}};", id = dst_id.0);
                    }
                }
            } else if registry.get_type_def(func_name).is_some() {
                if let Some(dst_id) = dst {
                    let type_def = registry.get_type_def(func_name).unwrap();
                    if let TypeDefKind::Struct(ref s) = type_def.kind {
                        let _ = write!(out, "        _{id} = ({func_name}){{", id = dst_id.0);
                        for (i, (arg, field)) in args.iter().zip(s.fields.iter()).enumerate() {
                            if i > 0 { out.push_str(", "); }
                            let val = format_operand(arg, func, registry);
                            let _ = write!(out, ".{} = {val}", field.name);
                        }
                        if args.is_empty() { out.push('0'); }
                        out.push_str("};\n");
                    } else {
                        let _ = writeln!(out, "        _{id} = ({func_name}){{0}};", id = dst_id.0);
                    }
                }
            }
            // Direct runtime calls that need special argument marshalling
            else if func_name == "gorget_array_contains" || func_name == "gorget_array_index_of" {
                // gorget_array_contains(const GorgetArray*, const void*, size_t)
                // IR sends: (collection, element) — we need (&collection, &element, sizeof(element))
                let coll_str = if args.len() > 0 { format_operand(&args[0], func, registry) } else { "/*?*/".into() };
                let elem_str = if args.len() > 1 { format_operand(&args[1], func, registry) } else { "/*?*/".into() };
                // Determine element type for sizeof
                let elem_type = if args.len() > 1 {
                    match &args[1] {
                        Operand::Copy(p) | Operand::Move(p) => {
                            let idx = p.local.0 as usize;
                            if idx < func.locals.len() {
                                format_type(func.locals[idx].type_id, registry)
                            } else { "int64_t".to_string() }
                        }
                        Operand::Constant(c) => match c {
                            Constant::I64(_) => "int64_t".to_string(),
                            Constant::I32(_) => "int32_t".to_string(),
                            Constant::F64(_) => "double".to_string(),
                            Constant::Bool(_) => "bool".to_string(),
                            Constant::Str(_) => "Str".to_string(),
                            _ => "int64_t".to_string(),
                        },
                    }
                } else { "int64_t".to_string() };
                // Collection might be a pointer already (borrow) or a value
                let coll_ptr = is_self_pointer(args, func, registry);
                let coll_arg = if coll_ptr { coll_str.clone() } else { format!("&{coll_str}") };
                let elem_arg = format!("&({elem_type}){{{elem_str}}}");
                if let Some(dst_id) = dst {
                    let _ = writeln!(out, "        _{id} = {func_name}({coll_arg}, {elem_arg}, sizeof({elem_type}));", id = dst_id.0);
                } else {
                    let _ = writeln!(out, "        {func_name}({coll_arg}, {elem_arg}, sizeof({elem_type}));");
                }
            }
            // Synthetic GIR helpers for for-in-string loop
            else if func_name == "gorget_utf8_codepoint_len_at" && args.len() == 2 {
                // gorget_utf8_codepoint_len_at(Str s, int64_t byte_pos) → int
                // Expands to: gorget_utf8_codepoint_len((unsigned char)s.data[byte_pos])
                let s = format_operand(&args[0], func, registry);
                let pos = format_operand(&args[1], func, registry);
                if let Some(dst_id) = dst {
                    let _ = writeln!(out, "        _{id} = gorget_utf8_codepoint_len((unsigned char){s}.data[{pos}]);", id = dst_id.0);
                }
            }
            else if func_name == "gorget_str_codepoint_at" && args.len() == 2 {
                // gorget_str_codepoint_at(Str s, int64_t byte_pos) → Str
                // Expands to: (Str){ .data = s.data + byte_pos, .len = cplen }
                // where cplen is gorget_utf8_codepoint_len((unsigned char)s.data[byte_pos])
                let s = format_operand(&args[0], func, registry);
                let pos = format_operand(&args[1], func, registry);
                if let Some(dst_id) = dst {
                    let _ = writeln!(out, "        _{id} = (Str){{ .data = {s}.data + {pos}, .len = (size_t)gorget_utf8_codepoint_len((unsigned char){s}.data[{pos}]) }};", id = dst_id.0);
                }
            }
            // Image/audio out-parameter calls — C runtime uses out-param ABI
            else if let Some(code) = try_emit_outparam_call(func_name, dst, args, func, registry, type_overrides) {
                out.push_str(&code);
            }
            // Runtime functions that take GorgetArray* (pointer) arguments
            else if takes_array_ptr_args(func_name) {
                let func_name_mapped = map_stdlib_name(func_name);
                let mut arg_parts: Vec<String> = Vec::new();
                for arg in args {
                    let arg_str = format_operand(arg, func, registry);
                    // Check if this operand is an array value that needs address-of
                    let needs_addr = if let Operand::Copy(p) | Operand::Move(p) = arg {
                        let idx = p.local.0 as usize;
                        let c_type = effective_c_type(idx, func, registry, type_overrides);
                        // Array/collection value types need &
                        (c_type == "GorgetArray" || c_type.starts_with("Vector__"))
                            && !c_type.ends_with('*')
                    } else {
                        false
                    };
                    if needs_addr {
                        arg_parts.push(format!("&{arg_str}"));
                    } else {
                        // For Str args that need const char*, extract .data
                        let is_str = if let Operand::Copy(p) | Operand::Move(p) = arg {
                            let idx = p.local.0 as usize;
                            let c_type = effective_c_type(idx, func, registry, type_overrides);
                            c_type == "Str" || c_type == "GorgetString"
                        } else { false };
                        if is_str && is_cstr_param_fn(func_name_mapped) {
                            arg_parts.push(format!("{arg_str}.data"));
                        } else {
                            arg_parts.push(arg_str);
                        }
                    }
                }
                let args_str = arg_parts.join(", ");
                let ret_cstr = returns_cstr(func_name_mapped);
                if let Some(dst_id) = dst {
                    let c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                    // Result wrapping for takes_array_ptr_args path (CallExtern)
                    if c_type.starts_with("Result__") && !func_name.starts_with("__result_") {
                        let raw_expr = if ret_cstr {
                            format!("gorget_str_from_cstr({func_name_mapped}({args_str}))")
                        } else {
                            format!("{func_name_mapped}({args_str})")
                        };
                        if let Some(err_fn) = last_error_fn(func_name_mapped) {
                            let _ = writeln!(out,
                                "        _{id} = ({{ __typeof__(_{id}.data.Ok._0) __raw = {raw_expr}; \
                                const char* __err = {err_fn}(); \
                                {c_type} __wr; if (__err) {{ __wr.tag = 1; __wr.data.Error._0 = gorget_str_from_cstr(__err); }} \
                                else {{ __wr.tag = 0; __wr.data.Ok._0 = __raw; }} __wr; }});",
                                id = dst_id.0);
                        } else {
                            let _ = writeln!(out,
                                "        _{id} = ({{ {c_type} __wr; \
                                if (GORGET_TRY) {{ __typeof__(_{id}.data.Ok._0) __raw = {raw_expr}; GORGET_CATCH_END; \
                                __wr.tag = 0; __wr.data.Ok._0 = __raw; }} \
                                else {{ GorgetError __err = GORGET_CATCH_ERROR(); GORGET_CATCH_END; \
                                __wr.tag = 1; __wr.data.Error._0 = gorget_str_from_cstr(__err.message); }} \
                                __wr; }});",
                                id = dst_id.0);
                        }
                    } else if ret_cstr && c_type == "Str" {
                        let _ = writeln!(out, "        _{id} = gorget_str_from_cstr({func_name_mapped}({args_str}));", id = dst_id.0);
                    } else if c_type.starts_with("Option__") {
                        // Option wrapping: C returns raw value, wrap in Option (>= 0 → Some, else None)
                        let _ = writeln!(out,
                            "        _{id} = ({{ __typeof__(_{id}.data.Some._0) __raw = {func_name_mapped}({args_str}); \
                            {c_type} __opt; if (__raw >= 0) {{ __opt.tag = 0; __opt.data.Some._0 = __raw; }} \
                            else {{ __opt.tag = 1; }} __opt; }});",
                            id = dst_id.0);
                    } else {
                        let _ = writeln!(out, "        _{id} = {func_name_mapped}({args_str});", id = dst_id.0);
                    }
                } else {
                    let _ = writeln!(out, "        {func_name_mapped}({args_str});");
                }
            }
            else {
                // Check if this is a user-defined function (skip Option/Result wrapping for user fns)
                let is_user_fn_call = all_functions.iter().any(|f| f.name.as_str() == func_name);
                let func_name = map_stdlib_name(func_name);
                // fprintf_stderr: synthetic name → fprintf(stderr, ...)
                let is_stderr_print = func_name == "fprintf_stderr";
                let func_name = if is_stderr_print { "fprintf" } else { func_name };
                let actual_args: &[Operand] = if is_stderr_print && !args.is_empty() && matches!(args[0], Operand::Constant(Constant::Null)) {
                    &args[1..] // skip Null placeholder (legacy format)
                } else {
                    args
                };
                // Image/audio/deflate out-parameter calls in Call path (fallback)
                if let Some(code) = try_emit_outparam_call(func_name, dst, actual_args, func, registry, type_overrides) {
                    out.push_str(&code);
                } else {
                // Regular extern call
                let is_printf = func_name == "printf" || func_name == "fprintf" || func_name == "sprintf"
                    || func_name == "gorget_string_format";
                let is_str_fn = (func_name.starts_with("gorget_str_") || func_name.starts_with("gorget_string_"))
                    && func_name != "gorget_string_format";
                let is_cstr_fn = is_cstr_param_fn(func_name);
                let args = actual_args;
                let args_str = if is_stderr_print {
                    let inner = format_printf_args(args, func, registry, type_overrides);
                    format!("stderr, {inner}")
                } else if is_printf {
                    format_printf_args(args, func, registry, type_overrides)
                } else if is_str_fn {
                    format_str_fn_args(args, func, registry, type_overrides)
                } else if is_cstr_fn {
                    let mut base = format_cstr_fn_args(args, func, registry);
                    for arg in args {
                        if let Operand::Constant(Constant::GlobalRef(gname)) = arg {
                            let is_str_global = module.globals.iter().any(|g| {
                                g.name == *gname && format_type(g.type_id, registry) == "Str"
                            });
                            if is_str_global {
                                base = base.replace(gname.as_str(), &format!("gorget_str_to_cstr({gname})"));
                            }
                        }
                    }
                    base
                } else {
                    let c_name_for_coercion = mangle_name(func_name);
                    format_args_with_coercion(args, func, registry, type_overrides, &c_name_for_coercion, all_functions)
                };
                let ret_cstr = returns_cstr(func_name);
                if let Some(dst_id) = dst {
                    let local_type = func.locals[dst_id.0 as usize].type_id;
                    if local_type == UNIT_TYPE && !type_overrides.contains_key(&(dst_id.0 as usize)) {
                        let _ = writeln!(out, "        {func_name}({args_str});");
                        // Str__slice mutates self in-place and "returns" void, but
                        // callers may use the result as Str. Assign *self to dst.
                        if func_name.ends_with("__slice") && !args.is_empty() {
                            if let Operand::Copy(_p) | Operand::Move(_p) = &args[0] {
                                let self_str = format_operand(&args[0], func, registry);
                                let _ = writeln!(out, "        _{id} = *{self_str};", id = dst_id.0);
                            }
                        }
                    } else {
                        let c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                        // Result wrapping: if destination is Result__* but C function returns raw value
                        // Skip for user-defined functions that already return Result
                        if c_type.starts_with("Result__") && !func_name.starts_with("__result_") && !is_user_fn_call {
                            // Extract Ok/Error type names from Result__Ok__Err
                            let rest = &c_type["Result__".len()..];
                            let _ok_type = if let Some(pos) = rest.find("__") {
                                &rest[..pos]
                            } else { rest };
                            let _err_type = if let Some(pos) = rest.find("__") {
                                &rest[pos + 2..]
                            } else { "Str" };
                            // Determine how to capture the raw return value
                            let raw_capture = if ret_cstr {
                                format!("gorget_str_from_cstr({func_name}({args_str}))")
                            } else {
                                format!("{func_name}({args_str})")
                            };
                            if let Some(err_fn) = last_error_fn(func_name) {
                                let _ = writeln!(out,
                                    "        _{id} = ({{ __typeof__(_{id}.data.Ok._0) __raw = {raw_capture}; \
                                    const char* __err = {err_fn}(); \
                                    {c_type} __wr; if (__err) {{ __wr.tag = 1; __wr.data.Error._0 = gorget_str_from_cstr(__err); }} \
                                    else {{ __wr.tag = 0; __wr.data.Ok._0 = __raw; }} __wr; }});",
                                    id = dst_id.0);
                            } else {
                                let _ = writeln!(out,
                                    "        _{id} = ({{ {c_type} __wr; \
                                    if (GORGET_TRY) {{ __typeof__(_{id}.data.Ok._0) __raw = {raw_capture}; GORGET_CATCH_END; \
                                    __wr.tag = 0; __wr.data.Ok._0 = __raw; }} \
                                    else {{ GorgetError __err = GORGET_CATCH_ERROR(); GORGET_CATCH_END; \
                                    __wr.tag = 1; __wr.data.Error._0 = gorget_str_from_cstr(__err.message); }} \
                                    __wr; }});",
                                    id = dst_id.0);
                            }
                        } else if ret_cstr && c_type == "GorgetString" {
                            let _ = writeln!(out, "        _{id} = gorget_string_adopt((char*){func_name}({args_str}));", id = dst_id.0);
                        } else if ret_cstr && (c_type == "Str" || c_type == "int64_t") {
                            // cstr-returning function: wrap result in Str (even if local typed as int64_t)
                            let _ = writeln!(out, "        _{id} = gorget_str_from_cstr({func_name}({args_str}));", id = dst_id.0);
                        } else if c_type.starts_with("Option__") && {
                            // Option wrapping — skip if the called function already returns Option
                            let called_ret = all_functions.iter()
                                .find(|f| f.name.as_str() == func_name || mangle_name(&f.name) == func_name)
                                .map(|f| format_type(f.return_type, registry));
                            !called_ret.as_ref().map_or(false, |r| r.starts_with("Option__"))
                        } {
                            let _ = writeln!(out,
                                "        _{id} = ({{ __typeof__(_{id}.data.Some._0) __raw = {func_name}({args_str}); \
                                {c_type} __opt; if (__raw >= 0) {{ __opt.tag = 0; __opt.data.Some._0 = __raw; }} \
                                else {{ __opt.tag = 1; }} __opt; }});",
                                id = dst_id.0);
                        } else if c_type.starts_with("Result__") && !func_name.starts_with("__result_") && {
                            // Result wrapping — when called function returns raw value, not Result
                            let called_ret = all_functions.iter()
                                .find(|f| f.name.as_str() == func_name || mangle_name(&f.name) == func_name)
                                .map(|f| format_type(f.return_type, registry));
                            !called_ret.as_ref().map_or(false, |r| r.starts_with("Result__"))
                        } {
                            // Wrap raw return value as Ok(value) — the function doesn't throw
                            let raw_capture = if ret_cstr {
                                format!("gorget_str_from_cstr({func_name}({args_str}))")
                            } else {
                                format!("{func_name}({args_str})")
                            };
                            let _ = writeln!(out,
                                "        _{id} = ({{ __typeof__(_{id}.data.Ok._0) __raw = {raw_capture}; \
                                {c_type} __wr; __wr.tag = 0; __wr.data.Ok._0 = __raw; __wr; }});",
                                id = dst_id.0);
                        } else {
                            let _ = writeln!(out, "        _{id} = {func_name}({args_str});", id = dst_id.0);
                        }
                    }
                } else {
                    let _ = writeln!(out, "        {func_name}({args_str});");
                }
            } // end of outparam else
            }
        }

        // -- Aggregates (P2.0/P2.1) --
        Instruction::StructInit { dst, type_name, fields } => {
            let type_def = registry.get_type_def(type_name);
            let _ = write!(out, "        _{id} = ({type_name}){{", id = dst.0);
            if let Some(def) = type_def {
                if let TypeDefKind::Struct(ref s) = def.kind {
                    for (i, (field_val, field_def)) in fields.iter().zip(s.fields.iter()).enumerate() {
                        if i > 0 {
                            out.push_str(", ");
                        }
                        // Check if this is Null for an enum field (e.g., Option None)
                        let val = if matches!(field_val, Operand::Constant(Constant::Null)) {
                            if let Some(GirType::Named(field_type_name)) = registry.get(field_def.type_id) {
                                if let Some(field_type_def) = registry.get_type_def(field_type_name) {
                                    if let TypeDefKind::Enum(ref e) = field_type_def.kind {
                                        let none_tag = e.variants.iter().position(|v| v.name == "None")
                                            .unwrap_or(e.variants.len() - 1);
                                        format!("({field_type_name}){{.tag = {none_tag}}}")
                                    } else {
                                        format_operand(field_val, func, registry)
                                    }
                                } else {
                                    format_operand(field_val, func, registry)
                                }
                            } else {
                                format_operand(field_val, func, registry)
                            }
                        } else {
                            coerce_constructor_arg(field_val, &field_def.type_id, func, registry)
                        };
                        let _ = write!(out, ".{name} = {val}", name = field_def.name);
                    }
                }
            } else {
                // Fallback: positional
                for (i, field_val) in fields.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    let val = format_operand(field_val, func, registry);
                    let _ = write!(out, "{val}");
                }
            }
            out.push_str("};\n");
        }

        Instruction::EnumInit { dst, type_name, variant, fields } => {
            let type_def = registry.get_type_def(type_name);
            let (tag, variant_fields) = if let Some(def) = type_def {
                if let TypeDefKind::Enum(ref e) = def.kind {
                    let idx = e.variants.iter().position(|v| v.name == *variant).unwrap_or(0);
                    let vf: Vec<_> = e.variants[idx].fields.clone();
                    (idx, vf)
                } else { (0, vec![]) }
            } else { (0, vec![]) };

            let _ = write!(out, "        _{id} = ({type_name}){{.tag = {tag}", id = dst.0);
            let all_unit = !fields.is_empty() && fields.iter().all(|f| matches!(f, Operand::Constant(Constant::Unit)));
            if !fields.is_empty() && !all_unit {
                let _ = write!(out, ", .data.{variant} = {{");
                for (i, field_val) in fields.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    // Handle Null fields: emit tagged None struct for enum-typed fields
                    let val = if matches!(field_val, Operand::Constant(Constant::Null)) && i < variant_fields.len() {
                        let ft = &variant_fields[i].type_id;
                        if let Some(GirType::Named(fname)) = registry.get(*ft) {
                            if let Some(ftd) = registry.get_type_def(fname) {
                                if let TypeDefKind::Enum(ref e) = ftd.kind {
                                    let none_tag = e.variants.iter().position(|v| v.name == "None")
                                        .unwrap_or(e.variants.len() - 1);
                                    format!("({fname}){{.tag = {none_tag}}}")
                                } else { format_operand(field_val, func, registry) }
                            } else { format_operand(field_val, func, registry) }
                        } else { format_operand(field_val, func, registry) }
                    } else if i < variant_fields.len() {
                        coerce_constructor_arg(field_val, &variant_fields[i].type_id, func, registry)
                    } else {
                        format_operand(field_val, func, registry)
                    };
                    let _ = write!(out, "{val}");
                }
                out.push('}');
            }
            out.push_str("};\n");
            // Post-init zero: after moving a non-Copy local into an enum variant (e.g., Some(x)),
            // zero the source local to prevent double-free. The enum owns the data now.
            for field_op in fields.iter() {
                if let Operand::Copy(place) | Operand::Move(place) = field_op {
                    if place.projections.is_empty() {
                        let local_id = place.local.0 as usize;
                        if local_id < func.locals.len() {
                            let local_type = func.locals[local_id].type_id;
                            if let Some(gir_name) = gir_type_name(local_type, registry) {
                                if needs_drop_by_name(&gir_name, registry) {
                                    let c_type = gir_to_c_type(&gir_name);
                                    let _ = writeln!(
                                        out,
                                        "        memset(&_{local_id}, 0, sizeof({c_type}));"
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }

        Instruction::TupleInit { dst, elements } => {
            // Determine tuple type name from the local's type
            let local_type_id = if (dst.0 as usize) < func.locals.len() {
                func.locals[dst.0 as usize].type_id
            } else {
                UNIT_TYPE
            };
            let local_type = format_type(local_type_id, registry);
            // Look up field types from the TypeDef for coercion
            let field_types: Vec<TypeId> = if let Some(GirType::Named(type_name)) = registry.get(local_type_id) {
                if let Some(td) = registry.get_type_def(type_name) {
                    if let TypeDefKind::Struct(ref s) = td.kind {
                        s.fields.iter().map(|f| f.type_id).collect()
                    } else { vec![] }
                } else { vec![] }
            } else { vec![] };
            let _ = write!(out, "        _{id} = ({local_type}){{", id = dst.0);
            for (i, elem) in elements.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                let val = if let Some(&ft) = field_types.get(i) {
                    coerce_constructor_arg(elem, &ft, func, registry)
                } else {
                    format_operand(elem, func, registry)
                };
                let _ = write!(out, "._{i} = {val}");
            }
            out.push_str("};\n");
        }

        Instruction::TagOf { dst, operand } => {
            let val = format_operand(operand, func, registry);
            // Check if the operand is a pointer (e.g., enum passed by ref)
            let accessor = match operand {
                Operand::Copy(p) | Operand::Move(p) =>
                    if is_place_ptr_type(func, p, registry) { "->" } else { "." },
                _ => ".",
            };
            let _ = writeln!(out, "        _{id} = {val}{accessor}tag;", id = dst.0);
        }

        Instruction::EnumFieldLoad { dst, base, variant, field } => {
            let base_str = format_place(base, registry);
            let _c_type = format_local_type(func, dst.0 as usize, registry);
            let accessor = if is_place_ptr_type(func, base, registry) {
                "->"
            } else {
                "."
            };
            let _ = writeln!(
                out,
                "        _{id} = {base_str}{accessor}data.{variant}._{field};",
                id = dst.0
            );
        }

        Instruction::FieldLoad { dst, base, field } => {
            let base_str = format_place(base, registry);
            // Look up field name from type def
            let field_name = resolve_field_name(func, base, *field, registry);
            let _c_type = format_local_type(func, dst.0 as usize, registry);
            // Use -> for pointer types (e.g., closure __call env param)
            let accessor = if is_place_ptr_type(func, base, registry) {
                "->"
            } else {
                "."
            };
            let _ = writeln!(out, "        _{id} = {base_str}{accessor}{field_name};", id = dst.0);
        }

        Instruction::IndexLoad { dst, base, index } => {
            let base_str = format_place(base, registry);
            let idx_str = format_operand(index, func, registry);
            let base_type = effective_c_type(base.local.0 as usize, func, registry, type_overrides);
            if base_type == "Str" {
                // Check if index is a range (GorgetRange) or integer
                let idx_type = match index {
                    Operand::Copy(p) | Operand::Move(p) =>
                        effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                    _ => "int64_t".to_string(),
                };
                if idx_type == "GorgetRange" {
                    let _ = writeln!(out, "        _{id} = gorget_str_slice({base_str}, {idx_str}.start, {idx_str}.end);", id = dst.0);
                } else {
                    let _ = writeln!(out, "        _{id} = gorget_str_index({base_str}, {idx_str});", id = dst.0);
                }
            } else if base_type.starts_with("GorgetArray") || base_type.starts_with("Vector__") {
                // Array indexing: access via data pointer with elem_size
                // Infer element type from collection base type name
                let eff_type = effective_c_type(dst.0 as usize, func, registry, type_overrides);
                let ir_type = if let Some(rt) = runtime_type_name(&eff_type) {
                    rt.to_string()
                } else { eff_type };
                let c_type = if ir_type == "int64_t" {
                    // Use the raw (pre-normalization) IR type name so element type info
                    // is not lost when base_type was normalized to "GorgetArray".
                    let raw_base: String = if (base.local.0 as usize) < func.locals.len() {
                        match registry.get(func.locals[base.local.0 as usize].type_id) {
                            Some(GirType::Named(n)) => n.clone(),
                            _ => base_type.clone(),
                        }
                    } else { base_type.clone() };
                    let elem = extract_collection_elem_type(&raw_base);
                    if elem != "int64_t" || !base_type.contains("__") { elem.to_string() } else { ir_type }
                } else { ir_type };
                // Check if index is a range (slice operation)
                let idx_type = match index {
                    Operand::Copy(p) | Operand::Move(p) =>
                        effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                    _ => "int64_t".to_string(),
                };
                if idx_type == "GorgetRange" {
                    // Vector slice: v[start..end] → gorget_array_slice
                    let _ = writeln!(out, "        _{id} = gorget_array_slice(&{base_str}, {idx_str}.start, {idx_str}.end);", id = dst.0);
                } else {
                    // For cloneable collection types, deep-clone instead of move-out
                    // to allow repeated subscript access (e.g., v[0].len(); v[0][0])
                    if c_type == "GorgetArray" || c_type.starts_with("Vector__") {
                        let _ = writeln!(out, "        _{id} = gorget_array_clone(({c_type}*)gorget_array_get(&{base_str}, {idx_str}));", id = dst.0);
                    } else if c_type == "GorgetSet" || c_type.starts_with("Set__") || c_type.starts_with("HashSet__") {
                        let _ = writeln!(out, "        _{id} = gorget_set_clone(({c_type}*)gorget_array_get(&{base_str}, {idx_str}));", id = dst.0);
                    } else if c_type == "GorgetMap" || c_type.starts_with("Dict__") || c_type.starts_with("HashMap__") {
                        let _ = writeln!(out, "        _{id} = gorget_map_clone(({c_type}*)gorget_array_get(&{base_str}, {idx_str}));", id = dst.0);
                    } else {
                        let _ = writeln!(out, "        _{id} = *({c_type}*)gorget_array_get(&{base_str}, {idx_str});", id = dst.0);
                        // For non-cloneable move types (e.g. Task), zero the element in the
                        // vector after copying to prevent double-free when the vector is dropped.
                        let elem_type_id = if (dst.0 as usize) < func.locals.len() {
                            Some(func.locals[dst.0 as usize].type_id)
                        } else {
                            None
                        };
                        if let Some(tid) = elem_type_id {
                            if registry.is_resource_type(tid) {
                                // Deep clone struct fields that are collections
                                let mut clone_ops: Vec<String> = Vec::new();
                                collect_clone_ops(&c_type, &format!("_{}", dst.0), &mut clone_ops, registry);
                                if !clone_ops.is_empty() {
                                    for op in &clone_ops {
                                        let _ = writeln!(out, "        {op}");
                                    }
                                } else {
                                    let _ = writeln!(out, "        memset(({c_type}*)gorget_array_get(&{base_str}, {idx_str}), 0, sizeof({c_type}));");
                                }
                            }
                        }
                    }
                }
            } else if base_type.starts_with("GorgetDict") || base_type.starts_with("Dict__")
                || base_type.starts_with("GorgetMap") || base_type.starts_with("HashMap__") {
                // Dict indexing: use gorget_map_get
                let eff_type = effective_c_type(dst.0 as usize, func, registry, type_overrides);
                let ir_type = if let Some(rt) = runtime_type_name(&eff_type) {
                    rt.to_string()
                } else { eff_type };
                let c_type = if ir_type == "int64_t" {
                    // Try to infer value type from dict name (Dict__K__V → V)
                    if let Some(rest) = base_type.strip_prefix("Dict__").or_else(|| base_type.strip_prefix("HashMap__")) {
                        // Find second __ separator for value type
                        if let Some(pos) = rest.find("__") {
                            rest[pos+2..].to_string()
                        } else { ir_type }
                    } else { ir_type }
                } else { ir_type };
                // Determine key type for compound literal (needed for constant keys)
                let key_type_str = match index {
                    Operand::Copy(p) | Operand::Move(p) => {
                        let idx = p.local.0 as usize;
                        if idx < func.locals.len() {
                            format_type(func.locals[idx].type_id, registry)
                        } else { "int64_t".to_string() }
                    }
                    Operand::Constant(c) => match c {
                        Constant::I64(_) => "int64_t".to_string(),
                        Constant::Str(_) => "Str".to_string(),
                        _ => "int64_t".to_string(),
                    },
                };
                // Use compound literal for key to ensure it's an lvalue
                let key_arg = if idx_str.starts_with('_') {
                    format!("&{idx_str}")
                } else if let Operand::Constant(Constant::Str(s)) = index {
                    // Str compound literal must include both .data and .len
                    format!("&(Str){{ .data = \"{}\", .len = {} }}", escape_c_string(s), s.len())
                } else {
                    format!("&({key_type_str}){{{idx_str}}}")
                };
                let _ = writeln!(out, "        _{id} = *({c_type}*)gorget_map_get(&{base_str}, {key_arg});", id = dst.0);
            } else {
                let _c_type = format_local_type(func, dst.0 as usize, registry);
                let _ = writeln!(out, "        _{id} = {base_str}[{idx_str}];", id = dst.0);
            }
        }

        // -- Type conversions --
        Instruction::Cast { dst, target_type, value } => {
            let c_type = format_type(*target_type, registry);
            let val = format_operand(value, func, registry);
            let src_type_name = operand_type(value, func)
                .map(|tid| format_type(tid, registry));
            let src_is_str = matches!(src_type_name.as_deref(), Some("Str") | Some("GorgetString"));
            let dst_is_str = c_type == "Str" || c_type == "GorgetString";
            let dst_is_int = c_type == "int64_t" || c_type == "uint8_t" || c_type == "int32_t";
            let _dst_is_float = c_type == "double";
            let src_is_int = matches!(src_type_name.as_deref(), Some("int64_t") | Some("uint8_t") | Some("int32_t"));
            let src_is_float = matches!(src_type_name.as_deref(), Some("double"));
            let src_is_void = matches!(src_type_name.as_deref(), Some("void"));
            if src_is_str && dst_is_int {
                // Str → int: extract first codepoint (ASCII byte)
                let _ = writeln!(out, "        _{id} = ({c_type})((uint8_t){val}.data[0]);", id = dst.0);
            } else if (src_is_int || src_is_void) && dst_is_str {
                // int → Str: convert codepoint to UTF-8 string
                let src_val = if src_is_void { format!("0") } else { val.clone() };
                let _ = writeln!(out, "        _{id} = gorget_str_from_cstr(gorget_codepoint_to_utf8({src_val}));", id = dst.0);
            } else if src_is_float && dst_is_str {
                // float → Str: convert via gorget_float_to_str
                let _ = writeln!(out, "        _{id} = gorget_str_from_cstr(gorget_float_to_str({val}));", id = dst.0);
            } else if c_type == "void" {
                // Cast to void — just evaluate for side effects (don't assign)
                let _ = writeln!(out, "        (void){val};");
            } else {
                let _ = writeln!(out, "        _{id} = ({c_type}){val};", id = dst.0);
            }
        }

        Instruction::BitCast { dst, target_type, value } => {
            let c_type = format_type(*target_type, registry);
            let val = format_operand(value, func, registry);
            let _ = writeln!(out, "        _{id} = ({c_type}){val};", id = dst.0);
        }

        Instruction::PtrCast { dst, target_type, value } => {
            let c_type = format_type(*target_type, registry);
            let val = format_operand(value, func, registry);
            let _ = writeln!(out, "        _{id} = ({c_type}){val};", id = dst.0);
        }

        // -- Ownership (P2.6) --
        Instruction::Borrow { dst, place } => {
            let place_str = format_place_typed(place, Some(func), registry);
            let _ = writeln!(out, "        _{id} = &{place_str};", id = dst.0);
        }

        Instruction::BorrowMut { dst, place } => {
            let place_str = format_place_typed(place, Some(func), registry);
            let _ = writeln!(out, "        _{id} = &{place_str};", id = dst.0);
        }

        Instruction::Drop { place } => {
            let place_str = format_place(place, registry);
            let local_type = func.locals[place.local.0 as usize].type_id;
            emit_drop_code(out, &place_str, local_type, registry);
        }

        Instruction::DropIfAlive { place } => {
            let place_str = format_place(place, registry);
            let local_type = func.locals[place.local.0 as usize].type_id;
            emit_drop_if_alive_code(out, &place_str, local_type, registry);
        }

        Instruction::MoveZero { place } => {
            let place_str = format_place(place, registry);
            let local_type = func.locals[place.local.0 as usize].type_id;
            let type_name = format_type(local_type, registry);
            let _ = writeln!(out, "        memset(&{place_str}, 0, sizeof({type_name}));");
        }

        // -- Calls (P2.4) --
        Instruction::CallIndirect { dst, callee, args } => {
            let callee_str = format_operand(callee, func, registry);
            let args_str = format_args(args, func, registry);
            if let Some(dst_id) = dst {
                let local_type = func.locals[dst_id.0 as usize].type_id;
                if local_type == UNIT_TYPE {
                    let _ = writeln!(out, "        {callee_str}({args_str});");
                } else {
                    let _c_type = format_local_type(func, dst_id.0 as usize, registry);
                    let _ = writeln!(out, "        _{id} = {callee_str}({args_str});", id = dst_id.0);
                }
            } else {
                let _ = writeln!(out, "        {callee_str}({args_str});");
            }
        }

        // -- Heap --
        Instruction::HeapAlloc { dst, type_id, .. } => {
            let c_type = format_type(*type_id, registry);
            let _ = writeln!(out, "        {c_type}* _{id} = ({c_type}*)malloc(sizeof({c_type}));", id = dst.0);
        }

        Instruction::HeapAllocArray { dst, type_id, count, .. } => {
            let c_type = format_type(*type_id, registry);
            let count_str = format_operand(count, func, registry);
            let _ = writeln!(out, "        {c_type}* _{id} = ({c_type}*)malloc(sizeof({c_type}) * {count_str});", id = dst.0);
        }

        Instruction::Dealloc { ptr, .. } => {
            let ptr_str = format_operand(ptr, func, registry);
            let _ = writeln!(out, "        free({ptr_str});");
        }

        // -- Allocator --
        Instruction::LoadThreadLocal { dst, name } => {
            let _c_type = format_local_type(func, dst.0 as usize, registry);
            let _ = writeln!(out, "        _{id} = {name};", id = dst.0);
        }

        Instruction::PushAllocator { allocator } => {
            let alloc_str = format_operand(allocator, func, registry);
            let n = *alloc_save_counter;
            alloc_save_stack.push(n);
            *alloc_save_counter += 1;
            let _ = writeln!(out, "        GorgetAllocator* __saved_alloc_{n} = __gorget_current_alloc;");
            let _ = writeln!(out, "        __gorget_current_alloc = &{alloc_str}->__alloc;");
        }

        Instruction::PopAllocator => {
            let n = alloc_save_stack.pop().unwrap_or(0);
            let _ = writeln!(out, "        __gorget_current_alloc = __saved_alloc_{n};");
        }

        Instruction::InlineC { code } => {
            let _ = writeln!(out, "        {code}");
        }

        Instruction::GlobalAssign { name, value } => {
            if !matches!(value, Operand::Constant(Constant::Unit)) {
                let val_str = format_operand(value, func, registry);
                let _ = writeln!(out, "        {name} = {val_str};");
            }
        }

        Instruction::Nop => {
            out.push_str("        /* nop */\n");
        }
    }
}

/// Emit a terminator as C code.
fn emit_terminator(out: &mut String, term: &Terminator, func: &Function, registry: &TypeRegistry, trace_fn_name: Option<&str>) {
    match term {
        Terminator::Return(value) => {
            let is_main = func.name == "main";
            let return_is_void = func.return_type == UNIT_TYPE && !is_main;

            // Trace return event (before the actual return).
            if let Some(name) = trace_fn_name {
                let escaped = name.replace('\\', "\\\\").replace('"', "\\\"");
                let _ = writeln!(out, "        if (__gorget_trace_fp) {{ fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"return\\\",\\\"fn\\\":\\\"{escaped}\\\",\\\"depth\\\":%d}}\\n\", __gorget_trace_depth--); }}");
            }

            if return_is_void {
                out.push_str("        return;\n");
            } else if is_main {
                // For throws-int main, _0 is a Result struct — unwrap to exit code
                let ret_type_name = registry.type_name(func.return_type);
                if ret_type_name.as_deref().map_or(false, |n| n.starts_with("Result__")) {
                    out.push_str("        if (_0.tag == 0) { return 0; } else { return _0.data.Error._0; }\n");
                } else {
                    out.push_str("        return _0;\n");
                }
            } else {
                match value {
                    Operand::Constant(Constant::Unit) => {
                        out.push_str("        return;\n");
                    }
                    _ => {
                        let mut val_str = format_operand(value, func, registry);
                        // Dereference pointer→value for `return self` in equip methods:
                        // self is Ptr(T) but the function returns T (value type).
                        if let Operand::Copy(place) | Operand::Move(place) = value {
                            if place.projections.is_empty() {
                                let idx = place.local.0 as usize;
                                if idx < func.locals.len() {
                                    let operand_tid = func.locals[idx].type_id;
                                    let is_ptr = matches!(
                                        registry.get(operand_tid),
                                        Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))
                                    );
                                    let ret_is_ptr = matches!(
                                        registry.get(func.return_type),
                                        Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))
                                    );
                                    if is_ptr && !ret_is_ptr && func.return_type != UNIT_TYPE {
                                        val_str = format!("(*{val_str})");
                                    }
                                }
                            }
                        }
                        let _ = writeln!(out, "        return {val_str};");
                    }
                }
            }
        }

        Terminator::Jump(target) => {
            let _ = writeln!(out, "        goto bb{};", target.0);
        }

        Terminator::Branch { cond, then_block, else_block } => {
            let cond_str = format_operand(cond, func, registry);
            let _ = writeln!(
                out,
                "        if ({cond_str}) goto bb{}; else goto bb{};",
                then_block.0, else_block.0
            );
        }

        Terminator::Switch { value, cases, default } => {
            let val_str = format_operand(value, func, registry);
            let _ = writeln!(out, "        switch ({val_str}) {{");
            for (val, target) in cases {
                let _ = writeln!(out, "            case {val}: goto bb{};", target.0);
            }
            let _ = writeln!(out, "            default: goto bb{};", default.0);
            out.push_str("        }\n");
        }

        Terminator::Unreachable => {
            out.push_str("        __builtin_unreachable();\n");
        }

        Terminator::Invoke { .. } => {
            out.push_str("        /* unhandled invoke */\n");
        }
    }
}

/// Format a GIR type as a C type string.
fn format_type(type_id: TypeId, registry: &TypeRegistry) -> String {
    if type_id == BOOL_TYPE {
        return "bool".to_string();
    }
    if type_id == I8_TYPE {
        return "int8_t".to_string();
    }
    if type_id == I16_TYPE {
        return "int16_t".to_string();
    }
    if type_id == I32_TYPE {
        return "int32_t".to_string();
    }
    if type_id == I64_TYPE {
        return "int64_t".to_string();
    }
    if type_id == U8_TYPE {
        return "uint8_t".to_string();
    }
    if type_id == U16_TYPE {
        return "uint16_t".to_string();
    }
    if type_id == U32_TYPE {
        return "uint32_t".to_string();
    }
    if type_id == U64_TYPE {
        return "uint64_t".to_string();
    }
    if type_id == F32_TYPE {
        return "float".to_string();
    }
    if type_id == F64_TYPE {
        return "double".to_string();
    }
    if type_id == UNIT_TYPE {
        return "void".to_string();
    }

    // Non-primitive: look up in registry
    if let Some(gir_type) = registry.get(type_id) {
        match gir_type {
            GirType::Ptr(inner) => {
                if *inner == U8_TYPE {
                    return "const char*".to_string();
                }
                format!("const {}*", format_type(*inner, registry))
            }
            GirType::MutPtr(inner) => {
                format!("{}*", format_type(*inner, registry))
            }
            GirType::Named(name) => {
                // Map external/runtime types to their C equivalents
                if let Some(c_name) = runtime_type_name(name) {
                    c_name.to_string()
                } else {
                    name.clone()
                }
            }
            GirType::FnPtr { params, return_type } => {
                let ret = format_type(*return_type, registry);
                let params_str: Vec<String> = params.iter()
                    .map(|p| format_type(*p, registry))
                    .filter(|t| t != "void")  // filter out void params (UNIT_TYPE)
                    .collect();
                format!("{ret}(*)({})", params_str.join(", "))
            }
            _ => "int64_t".to_string(), // fallback
        }
    } else {
        "int64_t".to_string() // fallback
    }
}

/// Format a C type for a spawned function parameter.
///
/// Callable params have UNIT_TYPE in the GIR (the `map_ast_type` path returns UNIT_TYPE for
/// `Callable[T(Params)]` since it can't register the type on the fly). In function bodies
/// Callable params are lowered as `void*` locals, so we use the same ABI here: store as
/// `void*` in the context struct and pass `void*` to the spawned function.
fn spawn_param_c_type(type_id: TypeId, registry: &TypeRegistry) -> String {
    // FnPtr TypeId means a Callable param (fn_sigs uses map_ast_type_mut which returns FnPtr).
    // Use void* so the spawn context stores it as an opaque pointer (__callable_N ABI).
    if matches!(registry.get(type_id), Some(GirType::FnPtr { .. })) {
        return "void*".to_string();
    }
    let c = format_type(type_id, registry);
    if c == "void" {
        "void*".to_string()
    } else {
        c
    }
}

/// Format a struct field declaration as valid C.
///
/// Function pointer types need special syntax: `RetType (*name)(Params)` instead of
/// the standalone `RetType(*)(Params)` format.
fn format_field_decl(type_id: TypeId, field_name: &str, registry: &TypeRegistry) -> String {
    if let Some(GirType::FnPtr { params, return_type }) = registry.get(type_id) {
        let ret = format_type(*return_type, registry);
        let params_str: Vec<String> = params.iter()
            .map(|p| format_type(*p, registry))
            .filter(|t| t != "void")  // filter out void params (UNIT_TYPE)
            .collect();
        format!("{ret} (*{field_name})({})", params_str.join(", "))
    } else {
        let c_type = format_type(type_id, registry);
        format!("{c_type} {field_name}")
    }
}

/// Format an operand as a C expression.
/// Coerce a constructor argument to match the field's expected type.
/// Handles string literal → GorgetString/Str conversions.
fn coerce_constructor_arg(
    arg: &Operand,
    field_type: &TypeId,
    func: &Function,
    registry: &TypeRegistry,
) -> String {
    let val = format_operand(arg, func, registry);
    // String literal → coerce to concrete field type
    if let Operand::Constant(Constant::Str(s)) = arg {
        let field_c_type = format_type(*field_type, registry);
        if field_c_type == "GorgetString" {
            return format!("gorget_string_new(\"{}\")", escape_c_string(s));
        }
        if field_c_type == "Str" {
            return format!("gorget_str_from_literal(\"{}\", {})", escape_c_string(s), s.len());
        }
    }
    // Non-literal: coerce GorgetString → Str if the field expects Str
    if let Operand::Copy(place) | Operand::Move(place) = arg {
        let local_idx = place.local.0 as usize;
        if local_idx < func.locals.len() {
            let arg_c_type = format_type(func.locals[local_idx].type_id, registry);
            let field_c_type = format_type(*field_type, registry);
            if arg_c_type == "GorgetString" && field_c_type == "Str" {
                return format!("(Str){{ .data = {val}.data, .len = {val}.len }}");
            }
        }
    }
    val
}

fn format_operand(operand: &Operand, func: &Function, registry: &TypeRegistry) -> String {
    match operand {
        Operand::Copy(place) | Operand::Move(place) => format_place_typed(place, Some(func), registry),

        Operand::Constant(constant) => format_constant(constant, func, registry),
    }
}

/// Format an array operand as a pointer: adds `&` if the operand is a value type,
/// passes through unchanged if it's already a pointer (Ptr/MutPtr).
fn addr_of_array_operand(operand: &Operand, func: &Function, registry: &TypeRegistry, type_overrides: &std::collections::HashMap<usize, String>) -> String {
    let s = format_operand(operand, func, registry);
    if let Operand::Copy(p) | Operand::Move(p) = operand {
        let idx = p.local.0 as usize;
        let ct = effective_c_type(idx, func, registry, type_overrides);
        if ct.ends_with('*') {
            return s;
        }
    }
    format!("&{s}")
}

/// Extract a `const char*` from an operand: string literals pass through,
/// Str/GorgetString values get `.data` appended.
fn extract_cstr_operand(operand: &Operand, func: &Function, registry: &TypeRegistry, type_overrides: &std::collections::HashMap<usize, String>) -> String {
    let s = format_operand(operand, func, registry);
    match operand {
        Operand::Constant(Constant::Str(_)) => s, // already const char*
        Operand::Copy(p) | Operand::Move(p) => {
            let idx = p.local.0 as usize;
            let ct = effective_c_type(idx, func, registry, type_overrides);
            if ct == "Str" || ct == "GorgetString" { format!("{s}.data") } else { s }
        }
        _ => s,
    }
}

/// Format a place as a C lvalue.
fn format_place(place: &Place, registry: &TypeRegistry) -> String {
    format_place_typed(place, None, registry)
}

/// Format a place as a C lvalue, optionally using the function's locals to resolve field names.
fn format_place_typed(place: &Place, func: Option<&Function>, registry: &TypeRegistry) -> String {
    let mut s = format!("_{}", place.local.0);

    // Track current type ID through projections to resolve field names
    let mut current_type_id = func.and_then(|f| {
        let idx = place.local.0 as usize;
        if idx < f.locals.len() { Some(f.locals[idx].type_id) } else { None }
    });

    for proj in &place.projections {
        match proj {
            Projection::Field(idx) => {
                // Try to resolve field name from type registry
                let field_name = current_type_id.and_then(|type_id| {
                    resolve_field_name_from_type(type_id, *idx, registry)
                });
                if let Some((name, next_type)) = field_name {
                    let _ = write!(s, ".{name}");
                    current_type_id = Some(next_type);
                } else {
                    // Fallback: use ._N for tuples (matching GIR convention)
                    let _ = write!(s, "._{idx}");
                    current_type_id = None;
                }
            }
            Projection::Index(local) => {
                let _ = write!(s, "[_{}]", local.0);
                current_type_id = None;
            }
            Projection::Deref => {
                s = format!("(*{s})");
                // Dereference pointer type
                current_type_id = current_type_id.and_then(|tid| {
                    match registry.get(tid)? {
                        GirType::Ptr(inner) | GirType::MutPtr(inner) => Some(*inner),
                        _ => None,
                    }
                });
            }
        }
    }
    s
}

/// Resolve the final C type name of a Place, walking through projections.
/// Returns the C type of the final projected element (e.g., for `(*_2).field`, returns the field's C type).
fn resolve_place_c_type(
    place: &Place, func: &Function, registry: &TypeRegistry,
    type_overrides: &std::collections::HashMap<usize, String>,
) -> String {
    if place.projections.is_empty() {
        return effective_c_type(place.local.0 as usize, func, registry, type_overrides);
    }
    let idx = place.local.0 as usize;
    let mut current_type_id = if idx < func.locals.len() {
        Some(func.locals[idx].type_id)
    } else {
        None
    };
    for proj in &place.projections {
        match proj {
            Projection::Deref => {
                current_type_id = current_type_id.and_then(|tid| {
                    match registry.get(tid)? {
                        GirType::Ptr(inner) | GirType::MutPtr(inner) => Some(*inner),
                        _ => None,
                    }
                });
            }
            Projection::Field(field_idx) => {
                current_type_id = current_type_id.and_then(|tid| {
                    resolve_field_name_from_type(tid, *field_idx, registry)
                        .map(|(_, next_tid)| next_tid)
                });
            }
            Projection::Index(_) => {
                current_type_id = None;
            }
        }
    }
    current_type_id
        .map(|tid| format_type(tid, registry))
        .unwrap_or_else(|| effective_c_type(place.local.0 as usize, func, registry, type_overrides))
}

/// Resolve a field name and type from a type ID and field index.
fn resolve_field_name_from_type(type_id: TypeId, field_idx: u32, registry: &TypeRegistry) -> Option<(String, TypeId)> {
    // First unwrap pointer types
    let inner_id = match registry.get(type_id)? {
        GirType::Ptr(inner) | GirType::MutPtr(inner) => *inner,
        _ => type_id,
    };
    let type_name = match registry.get(inner_id)? {
        GirType::Named(name) => name.clone(),
        _ => return None,
    };
    // Check for well-known runtime types first (including collection aliases)
    let resolved_name = collection_type_alias(&type_name)
        .map(|s| s.to_string())
        .unwrap_or_else(|| type_name.clone());
    let builtin_field = match (resolved_name.as_str(), field_idx) {
        ("Str", 0) => Some(("data".to_string(), UNIT_TYPE)),
        ("Str", 1) => Some(("len".to_string(), UNIT_TYPE)),
        ("GorgetString", 0) => Some(("data".to_string(), UNIT_TYPE)),
        ("GorgetString", 1) => Some(("len".to_string(), UNIT_TYPE)),
        ("GorgetString", 2) => Some(("cap".to_string(), UNIT_TYPE)),
        ("GorgetArray", 0) => Some(("data".to_string(), UNIT_TYPE)),
        ("GorgetArray", 1) => Some(("len".to_string(), UNIT_TYPE)),
        ("GorgetArray", 2) => Some(("elem_size".to_string(), UNIT_TYPE)),
        ("GorgetArray", 3) => Some(("cap".to_string(), UNIT_TYPE)),
        ("GorgetMap" | "GorgetDict", 0) => Some(("keys".to_string(), UNIT_TYPE)),
        ("GorgetMap" | "GorgetDict", 1) => Some(("values".to_string(), UNIT_TYPE)),
        ("GorgetMap" | "GorgetDict", 2) => Some(("states".to_string(), UNIT_TYPE)),
        ("GorgetMap" | "GorgetDict", 3) => Some(("count".to_string(), UNIT_TYPE)),
        ("GorgetMap" | "GorgetDict", 4) => Some(("cap".to_string(), UNIT_TYPE)),
        ("GorgetMap" | "GorgetDict", 5) => Some(("key_size".to_string(), UNIT_TYPE)),
        ("GorgetMap" | "GorgetDict", 6) => Some(("val_size".to_string(), UNIT_TYPE)),
        ("GorgetMap" | "GorgetDict", 7) => Some(("alloc".to_string(), UNIT_TYPE)),
        _ => None,
    };
    if let Some(result) = builtin_field {
        return Some(result);
    }

    let type_def = registry.get_type_def(&type_name)?;
    if let TypeDefKind::Struct(ref s) = type_def.kind {
        let field = s.fields.get(field_idx as usize)?;
        Some((field.name.clone(), field.type_id))
    } else {
        None
    }
}

/// Resolve a field name from its index, looking up the TypeDef.
fn resolve_field_name(func: &Function, base: &Place, field_idx: u32, registry: &TypeRegistry) -> String {
    let local_type_id = func.locals[base.local.0 as usize].type_id;

    // Walk through projections to find the innermost type
    let mut current_type_id = local_type_id;
    for proj in &base.projections {
        match proj {
            Projection::Deref => {
                // Dereference pointer: Ptr(T) or MutPtr(T) → T
                if let Some(gir_type) = registry.get(current_type_id) {
                    match gir_type {
                        GirType::Ptr(inner) | GirType::MutPtr(inner) => {
                            current_type_id = *inner;
                        }
                        _ => {}
                    }
                }
            }
            Projection::Field(idx) => {
                if let Some(gir_type) = registry.get(current_type_id) {
                    if let GirType::Named(name) = gir_type {
                        if let Some(type_def) = registry.get_type_def(name) {
                            if let TypeDefKind::Struct(ref s) = type_def.kind {
                                if let Some(field) = s.fields.get(*idx as usize) {
                                    current_type_id = field.type_id;
                                }
                            }
                        }
                    }
                }
            }
            Projection::Index(_) => {}
        }
    }

    // Unwrap pointer types to get the inner named type
    let mut resolved_type_id = current_type_id;
    if let Some(gir_type) = registry.get(resolved_type_id) {
        match gir_type {
            GirType::Ptr(inner) | GirType::MutPtr(inner) => {
                resolved_type_id = *inner;
            }
            _ => {}
        }
    }

    // Now resolve the final field name
    if let Some(gir_type) = registry.get(resolved_type_id) {
        if let GirType::Named(name) = gir_type {
            // Check builtin/collection field names first
            if let Some((field_name, _)) = resolve_field_name_from_type(resolved_type_id, field_idx, registry) {
                return field_name;
            }
            if let Some(type_def) = registry.get_type_def(name) {
                if let TypeDefKind::Struct(ref s) = type_def.kind {
                    if let Some(field) = s.fields.get(field_idx as usize) {
                        return field.name.clone();
                    }
                }
            }
        }
    }

    // Fallback to positional (use _N for tuples, matching GIR convention)
    format!("_{field_idx}")
}

/// Check if a function name is a collection/Box/String constructor.
/// Distinguishes constructors from method calls:
///   "Vector__int64_t" = constructor (no method suffix after type params)
///   "Vector__int64_t__push" = method call (has method suffix)
/// Infer return type for known runtime functions when lowering loses type info.
fn infer_runtime_return_type(name: &str) -> Option<&'static str> {
    match name {
        // String methods returning GorgetArray
        "gorget_str_bytes" | "gorget_str_codepoints" | "gorget_str_chars" => Some("GorgetArray"),
        // String methods returning GorgetString
        "gorget_str_cat" | "gorget_str_to_upper" | "gorget_str_to_lower"
        | "gorget_str_replace" | "gorget_str_repeat"
        | "gorget_str_pad_left" | "gorget_str_pad_right"
        | "gorget_string_from_str" | "gorget_string_from_concat"
        | "gorget_string_format" => Some("GorgetString"),
        // String methods returning Str
        "gorget_str_from_literal" | "gorget_str_from_cstr"
        | "gorget_str_trim" | "gorget_str_strip" | "gorget_str_lstrip" | "gorget_str_rstrip"
        | "gorget_str_removeprefix" | "gorget_str_removesuffix"
        | "gorget_str_index" | "gorget_str_slice"
        | "gorget_str_byte_slice"
        | "codepoint_to_str" => Some("Str"),
        // String methods returning int64_t
        "gorget_str_codepoint_count" | "gorget_str_byte_len"
        | "gorget_str_index_of" | "gorget_str_count"
        | "gorget_str_find" => Some("int64_t"),
        // String methods returning bool
        "gorget_str_eq" | "gorget_str_contains"
        | "gorget_str_starts_with" | "gorget_str_ends_with" => Some("bool"),
        // Array functions
        "gorget_array_clone" | "gorget_array_slice" => Some("GorgetArray"),
        "gorget_array_len" | "gorget_array_index_of" => Some("int64_t"),
        // Stdlib: file I/O
        "read_file" | "gorget_read_file" => Some("GorgetString"),
        "file_exists" | "gorget_file_exists" | "is_dir" | "gorget_is_dir" => Some("bool"),
        "file_size" | "gorget_file_size" => Some("int64_t"),
        // Stdlib: path functions (return char*, wrapped to Str/GorgetString)
        "path_parent" | "gorget_path_parent"
        | "path_basename" | "gorget_path_basename"
        | "path_extension" | "gorget_path_extension"
        | "path_stem" | "gorget_path_stem"
        | "path_join" | "gorget_path_join"
        | "path_normalize" | "gorget_path_normalize"
        | "path_absolute" | "gorget_path_absolute" => Some("Str"),
        "readdir" | "gorget_readdir" => Some("GorgetArray"),
        // Stdlib: I/O
        "readline" | "gorget_readline" | "input" | "gorget_input" => Some("GorgetString"),
        "getchar" | "gorget_getchar" => Some("int64_t"),
        "term_cols" | "gorget_term_cols" | "term_rows" | "gorget_term_rows" => Some("int64_t"),
        // Stdlib: CLI / process
        "args" | "gorget_args" => Some("GorgetArray"),
        "exec" | "gorget_exec" => Some("int64_t"),
        "exec_output" | "gorget_exec_output" => Some("ExecResult"),
        // Stdlib: environment
        "getenv" | "gorget_getenv" => Some("Str"),
        "platform" | "gorget_platform" => Some("Str"),
        "getcwd" | "gorget_getcwd" => Some("GorgetString"),
        // Stdlib: time
        "time" | "gorget_time" => Some("double"),
        "time_ms" | "gorget_time_ms" => Some("int64_t"),
        "format_time" | "gorget_format_time" => Some("GorgetString"),
        "parse_time" | "gorget_parse_time" => Some("int64_t"),
        // Stdlib: random
        "rand" | "gorget_rand" => Some("int64_t"),
        "rand_range" | "gorget_rand_range" => Some("int64_t"),
        // Stdlib: math
        "abs" | "gorget_abs" => Some("int64_t"),
        "min" | "gorget_min" | "max" | "gorget_max" => Some("int64_t"),
        "sqrt" | "gorget_sqrt" | "floor" | "gorget_floor" | "ceil" | "gorget_ceil"
        | "round" | "gorget_round" | "log" | "gorget_log" | "log2" | "gorget_log2"
        | "log10" | "gorget_log10" | "pow" | "gorget_pow"
        | "sin" | "gorget_sin" | "cos" | "gorget_cos" | "tan" | "gorget_tan"
        | "asin" | "gorget_asin" | "acos" | "gorget_acos" | "atan" | "gorget_atan"
        | "atan2" | "gorget_atan2" => Some("double"),
        // Stdlib: conversion
        // parse_int/parse_float now return Result[T, str] — type comes from GIR, not here
        "ord" | "gorget_str_ord" | "gorget_char_ord" => Some("int64_t"),
        "int_to_str" | "gorget_int_to_str" | "float_to_str" | "gorget_float_to_str"
        | "bool_to_str" | "gorget_bool_to_str" => Some("Str"),
        "int_to_float" | "gorget_int_to_float" => Some("double"),
        "chr" | "gorget_char_chr" | "codepoint_to_utf8" | "gorget_codepoint_to_utf8" => Some("Str"),
        // Crypto hash functions (return GorgetArray of bytes)
        "crypto_sha256" | "gorget_crypto_sha256"
        | "crypto_sha1" | "gorget_crypto_sha1"
        | "crypto_sha512" | "gorget_crypto_sha512"
        | "crypto_md5" | "gorget_crypto_md5"
        | "gorget_cipher_encrypt" | "gorget_cipher_decrypt" => Some("GorgetArray"),
        "gorget_crypto_cipher_new" | "crypto_cipher_new" => Some("CipherContext"),
        // Regex
        "regex_match" | "gorget_regex_match" => Some("GorgetArray"),
        "regex_find_all" | "gorget_regex_find_all" => Some("GorgetArray"),
        "gorget_regex_replace_all" => Some("GorgetString"),
        "gorget_regex_split" => Some("GorgetArray"),
        "gorget_regex_compile" => Some("GorgetRegex*"),
        "gorget_regex_is_match" => Some("bool"),
        // gorget_regex_find / gorget_regex_fullmatch return GorgetRegexMatch,
        // but wrapped as Option in GIR (Option__Match) — return None to let GIR type stand.
        // gorget_regex_match_group / _by_name also return Option__Str in GIR — let GIR type stand.
        "gorget_regex_escape" => Some("GorgetString"),
        "gorget_regex_last_error" => Some("const char*"),
        // gorget_regex_match_text / pattern_str return Str (via returns_cstr wrapping)
        "gorget_regex_match_text" | "gorget_regex_pattern_str" => Some("Str"),
        "gorget_regex_capture_count" | "gorget_regex_match_start"
        | "gorget_regex_match_end" | "gorget_regex_match_group_count" => Some("int64_t"),
        "gorget_regex_group_names" | "gorget_regex_extract_names"
        | "gorget_regex_match_groups" => Some("GorgetArray"),
        // Free function wrappers (compile+use+free)
        "gorget_regex_is_match_pat" => Some("bool"),
        "gorget_regex_replace_pat" => Some("GorgetString"),
        // gorget_regex_find_pat returns Option__Match in GIR — return None to let GIR type stand
        // Allocators (return pointers in C runtime)
        "gorget_arena_new" | "Arena" => Some("GorgetArena*"),
        "gorget_tracking_new" | "TrackingAllocator" => Some("GorgetTrackingAllocator*"),
        "gorget_pool_new" | "PoolAllocator" => Some("GorgetPoolAllocator*"),
        "gorget_tlsf_new" | "TlsfAllocator" => Some("GorgetTlsfAllocator*"),
        "gorget_fba_new" | "FixedBufferAllocator" => Some("GorgetFixedBufferAllocator*"),
        "gorget_fallback_new" | "FallbackAllocator" => Some("GorgetFallbackAllocator*"),
        "Arena__bytes_used" | "gorget_arena_bytes_used" => Some("int64_t"),
        "Arena__checkpoint" | "gorget_arena_checkpoint" => Some("GorgetArenaCheckpoint"),
        "TrackingAllocator__alloc_count" | "gorget_tracking_alloc_count"
        | "TrackingAllocator__free_count" | "gorget_tracking_free_count"
        | "TrackingAllocator__bytes_allocated" | "gorget_tracking_bytes_allocated"
        | "TrackingAllocator__bytes_freed" | "gorget_tracking_bytes_freed" => Some("int64_t"),
        "TrackingAllocator__current_bytes" | "gorget_tracking_current_bytes"
        | "TrackingAllocator__peak_bytes" | "gorget_tracking_peak_bytes"
        | "TrackingAllocator__realloc_count" | "gorget_tracking_realloc_count" => Some("int64_t"),
        "PoolAllocator__used_blocks" | "gorget_pool_used_blocks"
        | "PoolAllocator__free_blocks" | "gorget_pool_free_blocks"
        | "PoolAllocator__total_blocks" | "gorget_pool_total_blocks"
        | "PoolAllocator__block_size" | "gorget_pool_block_size" => Some("int64_t"),
        "TlsfAllocator__bytes_used" | "gorget_tlsf_bytes_used"
        | "TlsfAllocator__peak_bytes" | "gorget_tlsf_peak_bytes"
        | "TlsfAllocator__pool_size" | "gorget_tlsf_pool_size" => Some("int64_t"),
        "FixedBufferAllocator__bytes_used" | "gorget_fba_bytes_used"
        | "FixedBufferAllocator__capacity" | "gorget_fba_capacity" => Some("int64_t"),
        "FallbackAllocator__primary_count" | "gorget_fallback_primary_count"
        | "FallbackAllocator__fallback_count" | "gorget_fallback_fallback_count" => Some("int64_t"),
        // Bytes
        "bytes_from_str" | "gorget_bytes_from_str"
        | "bytes_from_hex" | "gorget_bytes_from_hex"
        | "bytes_concat" | "gorget_bytes_concat"
        | "bytes_slice" | "gorget_bytes_slice" => Some("GorgetArray"),
        "bytes_to_str" | "gorget_bytes_to_str"
        | "bytes_to_hex" | "gorget_bytes_to_hex" => Some("Str"),
        "bytes_read_u16_be" | "gorget_bytes_read_u16_be"
        | "bytes_read_u32_be" | "gorget_bytes_read_u32_be"
        | "bytes_read_u16_le" | "gorget_bytes_read_u16_le"
        | "bytes_read_u32_le" | "gorget_bytes_read_u32_le" => Some("int64_t"),
        // Encoding
        "base64_encode" | "gorget_base64_encode" | "hex_encode" | "gorget_hex_encode" => Some("Str"),
        "base64_decode" | "gorget_base64_decode" | "hex_decode" | "gorget_hex_decode" => Some("GorgetArray"),
        // File
        "File__create" | "gorget_file_create" | "File__open" | "gorget_file_open" => Some("GorgetFile"),
        _ => None,
    }
}

/// Infer the C return type for a Type__method call.
fn infer_method_return_type(name: &str) -> Option<&'static str> {
    // Module-mangled function names use `___` (triple underscore) as the separator between
    // module path and function name (e.g., `std__fmt___join`).  They are NOT method calls;
    // their return types come from the GIR, not from heuristic inference.
    if name.contains("___") {
        return None;
    }
    // Primitive type parse methods: int64_t__parse → Option__int64_t, etc.
    match name {
        "int64_t__parse" | "int__parse" => return Some("Option__int64_t"),
        "int8_t__parse" | "int8__parse" => return Some("Option__int8_t"),
        "int16_t__parse" | "int16__parse" => return Some("Option__int16_t"),
        "int32_t__parse" | "int32__parse" => return Some("Option__int32_t"),
        "uint8_t__parse" | "uint8__parse" => return Some("Option__uint8_t"),
        "uint16_t__parse" | "uint16__parse" => return Some("Option__uint16_t"),
        "uint32_t__parse" | "uint32__parse" => return Some("Option__uint32_t"),
        "uint64_t__parse" | "uint64__parse" => return Some("Option__uint64_t"),
        "double__parse" | "float__parse" => return Some("Option__double"),
        "bool__parse" => return Some("Option__bool"),
        "int64_t__default" | "int__default" | "int64_t__one" | "int__one" => return Some("int64_t"),
        "double__default" | "float__default" | "double__one" | "float__one" => return Some("double"),
        "bool__default" => return Some("bool"),
        "Str__default" | "str__default" => return Some("Str"),
        _ => {}
    }
    // Str methods
    if let Some(method) = name.strip_prefix("Str__") {
        return match method {
            "bytes" | "codepoints" | "chars" | "split" => Some("GorgetArray"),
            "to_upper" | "to_lower" | "replace" | "repeat"
            | "pad_left" | "pad_right" => Some("GorgetString"),
            "trim" | "strip" | "lstrip" | "rstrip"
            | "removeprefix" | "removesuffix" | "byte_slice"
            | "slice" | "substring" => Some("Str"),
            "byte_at" => Some("uint8_t"),
            "char_at" => Some("Str"),
            "len" | "byte_len" | "count" | "find" | "hash" => Some("int64_t"),
            "index_of" => Some("Option__int64_t"),
            "is_alpha" | "is_digit" | "is_alphanumeric" | "is_whitespace"
            | "is_upper" | "is_lower" | "is_hex_digit" | "is_ascii"
            | "contains" | "starts_with" | "ends_with" | "eq" | "is_empty" => Some("bool"),
            "enumerate" => Some("GorgetArray"),
            _ => None,
        };
    }
    // GorgetString methods
    if let Some(method) = name.strip_prefix("GorgetString__") {
        return match method {
            "str" | "as_str" => Some("Str"),
            "push" | "push_str" | "push_line" | "clear" => None, // void
            "len" | "capacity" => Some("int64_t"),
            "is_empty" => Some("bool"),
            _ => None,
        };
    }
    // Collection methods returning specific types
    // Pattern: Type__method where Type is a collection
    if let Some(pos) = name.rfind("__") {
        let method = &name[pos + 2..];
        let type_prefix = &name[..pos];
        // Collection method return types
        match method {
            // Vector.get / .first / .last / .remove / .pop return Option[T]
            "get" | "first" | "last" | "remove" | "pop" if type_prefix.starts_with("Vector") => {
                if let Some(elem) = extract_element_type_from_collection(type_prefix) {
                    let option_type = format!("Option__{elem}");
                    return Some(Box::leak(option_type.into_boxed_str()));
                }
                return None;
            }
            // Dict/Set.remove returns bool
            "remove" if type_prefix.starts_with("Dict__") || type_prefix.starts_with("HashMap__")
                      || type_prefix.starts_with("Set__") || type_prefix.starts_with("HashSet__") => {
                return Some("bool");
            }
            // Dict/HashMap.get returns Option[V]
            "get" if type_prefix.starts_with("Dict__") || type_prefix.starts_with("HashMap__") => {
                if let Some(elem) = extract_element_type_from_collection(type_prefix) {
                    let option_type = format!("Option__{elem}");
                    return Some(Box::leak(option_type.into_boxed_str()));
                }
                return None;
            }
            // Methods that return the element type — extract from collection name
            "get" | "at" => {
                if let Some(elem) = extract_element_type_from_collection(type_prefix) {
                    return Some(Box::leak(elem.into_boxed_str()));
                }
                return None;
            }
            // Methods returning collections — return same collection type
            "filter" | "keys" | "values" | "items"
            | "sorted" | "reversed" | "unique"
                if !type_prefix.starts_with("Option") && !type_prefix.starts_with("Result") =>
            {
                if type_prefix.starts_with("Set__") || type_prefix.starts_with("HashSet__") {
                    return Some("GorgetSet");
                }
                if type_prefix.starts_with("Dict__") || type_prefix.starts_with("HashMap__") {
                    // filter returns same map type; keys/values/items return array
                    if method == "filter" {
                        return Some("GorgetMap");
                    }
                    return Some("GorgetArray");
                }
                return Some("GorgetArray");
            }
            // clone: returns GorgetArray for collections, but the struct type for non-collections
            "clone" => {
                let is_collection = type_prefix.starts_with("Vector__")
                    || type_prefix.starts_with("Dict__")
                    || type_prefix.starts_with("HashMap__")
                    || type_prefix.starts_with("Set__")
                    || type_prefix.starts_with("HashSet__");
                if is_collection {
                    return Some("GorgetArray");
                }
                // For non-collection types, return the type name itself
                return Some(Box::leak(type_prefix.to_string().into_boxed_str()));
            }
            "enumerate" => return Some("GorgetArray"),
            // Methods returning count/index
            "len" | "count" | "find" | "binary_search" => return Some("int64_t"),
            "index_of" => return Some("Option__int64_t"),
            // Methods returning bool
            "contains" | "is_empty" | "is_subset" | "is_superset"
            | "starts_with" | "ends_with" | "any" | "all" => return Some("bool"),
            // Methods returning Str
            // Thread.join() is NOT a string join — return type determined by GIR, not heuristic
            "join" if type_prefix.starts_with("Thread__") => return None,
            "join" if type_prefix.contains("Str") || type_prefix.contains("str") => return Some("GorgetString"),
            "join" => return Some("GorgetString"),
            "str" | "to_str" => return Some("Str"),
            // Safe indexing returns Option — need the type
            "get_safe" => return None,
            _ => {}
        }
    }
    None
}

/// Try to emit inline code for higher-order collection methods (filter, map, fold, reduce, enumerate).
/// Returns None if not a recognized higher-order method.
fn try_emit_higher_order_method(
    func_name: &str,
    dst: &Option<LocalId>,
    args: &[Operand],
    func: &Function,
    registry: &TypeRegistry,
    type_overrides: &std::collections::HashMap<usize, String>,
    module: &Module,
) -> Option<String> {
    // Parse: Vector__ElemType__method or Type__method
    let method = extract_trailing_method(func_name, "");
    if !matches!(method, "filter" | "map" | "flat_map" | "fold" | "reduce" | "enumerate" | "any" | "all" | "each" | "for_each" | "find" | "count" | "get_or_put" | "keys" | "values" | "zip") {
        return None;
    }
    // Don't treat Option/Result/Regex/Match methods as collection higher-order methods.
    if func_name.starts_with("Option__") || func_name.starts_with("Result__")
        || func_name.starts_with("Regex__") || func_name.starts_with("Match__")
    {
        return None;
    }

    // args[0] = collection ref, args[1] = closure (or init for fold), args[2] = closure (for fold)
    if args.is_empty() { return None; }

    let coll_ref = format_operand(&args[0], func, registry);
    let coll_type = match &args[0] {
        Operand::Copy(p) | Operand::Move(p) =>
            effective_c_type(p.local.0 as usize, func, registry, type_overrides),
        _ => return None,
    };

    // Determine the underlying collection type (strip const/pointer)
    let bare_type = coll_type.strip_suffix('*').unwrap_or(&coll_type);
    let bare_type = bare_type.strip_prefix("const ").unwrap_or(bare_type);

    // Detect Dict/HashMap and extract key/value types (use func_name, not bare_type,
    // because type_overrides may have collapsed Dict__K__V to GorgetMap)
    let is_dict = func_name.starts_with("Dict__") || func_name.starts_with("HashMap__")
        || bare_type.starts_with("Dict__") || bare_type.starts_with("HashMap__")
        || bare_type == "GorgetDict" || bare_type == "GorgetMap";
    // Dict__ is insertion-order preserving; HashMap__ is unordered
    let is_ordered_dict = func_name.starts_with("Dict__")
        || bare_type.starts_with("Dict__") || bare_type == "GorgetDict";

    // If the module already has a GIR-emitted function with this name, it's a user-defined
    // equip method (e.g., LinkedListIter__fold, XmlNode__find) — skip inline generation
    // and let regular function dispatch handle it.
    if module.functions.iter().any(|f| f.name == func_name) {
        return None;
    }

    let is_set = bare_type.starts_with("GorgetSet") || bare_type.starts_with("Set__") || bare_type.starts_with("HashSet__");
    let is_ordered_set = func_name.starts_with("Set__")
        || bare_type.starts_with("Set__");

    // Extract element type — first try the bare C type (e.g., "Vector__Student"),
    // then fall back to the mangled function name (e.g., "Vector__Student__map"),
    // which retains the generic parameter even when the runtime type is erased to GorgetArray.
    let elem_type = if let Some(elem) = extract_element_type_from_collection(bare_type) {
        elem
    } else if let Some(elem) = extract_element_type_from_method_name(func_name) {
        elem
    } else {
        "int64_t".to_string()
    };
    let (dict_key_type, dict_val_type) = if is_dict {
        // Extract key type from func_name (e.g., "Dict__Str__int64_t__fold" → "Str")
        let key = extract_map_key_type(func_name).unwrap_or("int64_t").to_string();
        // Extract value type: strip prefix and key to get value type
        let val = {
            let type_part = func_name.strip_prefix("Dict__")
                .or_else(|| func_name.strip_prefix("HashMap__"))
                .unwrap_or(func_name);
            // type_part = "Str__int64_t__fold" — strip key prefix and find value
            if let Some(after_key) = type_part.strip_prefix(&format!("{key}__")) {
                // after_key = "int64_t__fold" — strip trailing method name
                if let Some(pos) = after_key.rfind("__") {
                    let suffix = &after_key[pos + 2..];
                    if !suffix.is_empty() && suffix.starts_with(|c: char| c.is_lowercase()) {
                        after_key[..pos].to_string()
                    } else {
                        after_key.to_string()
                    }
                } else {
                    after_key.to_string()
                }
            } else {
                elem_type.clone()
            }
        };
        (key, val)
    } else {
        (String::new(), String::new())
    };

    // For pointer receiver, dereference to get the array value
    let coll_val = if coll_type.ends_with('*') {
        format!("(*{coll_ref})")
    } else {
        coll_ref.clone()
    };

    let dst_str = if let Some(d) = dst {
        format!("_{}", d.0)
    } else {
        // Void methods like `each` still need codegen — don't bail early
        String::new()
    };

    // Helper: extract call function name and closure operand for higher-order method args.
    // For closure structs: call_fn = "ClosureType__call", closure_ref = "&closure_local"
    // For named function refs (FuncRef): call_fn = "__adapt_funcname", closure_ref = "NULL"
    //   (the adapter has the same (void* env, params...) ABI as closure __call)
    let extract_callable = |arg: &Operand| -> (String, String, String) {
        // Check for named function reference first
        if let Operand::Constant(crate::ir::instructions::Constant::FuncRef(name)) = arg {
            let c_name = mangle_name(name);
            let adapter = format!("__adapt_{c_name}");
            let closure_ref = "NULL".to_string();
            return (adapter, closure_ref, "void*".to_string());
        }
        let closure = format_operand(arg, func, registry);
        let closure_type = match arg {
            Operand::Copy(p) | Operand::Move(p) =>
                effective_c_type(p.local.0 as usize, func, registry, type_overrides),
            _ => "void*".to_string(),
        };
        let call_fn = format!("{closure_type}__call");
        let closure_ref = format!("&{closure}");
        (call_fn, closure_ref, closure_type)
    };

    let mut out = String::new();

    match method {
        "filter" => {
            // args: [collection_ref, closure]
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _closure_type) = extract_callable(&args[1]);
            use std::fmt::Write;
            if is_dict {
                // Dict/HashMap filter: iterate entries, pass (key, value) to closure, build new map
                let dict_c_type = if bare_type.starts_with("Dict__") { "GorgetDict" } else { "GorgetMap" };
                let iter_loop = if is_ordered_dict {
                    format!("for (size_t __oi = 0; __oi < __src.order_len; __oi++) {{ \
                    size_t __i = __src.order[__oi]; \
                    if (__src.states[__i] != 1) continue;")
                } else {
                    format!("for (size_t __i = 0; __i < __src.cap; __i++) {{ \
                    if (__src.states[__i] != 1) continue;")
                };
                let ctor = if dict_key_type == "Str" {
                    if is_ordered_dict { "gorget_dict_new_str" } else { "gorget_map_new_str" }
                } else {
                    if is_ordered_dict { "gorget_dict_new" } else { "gorget_map_new" }
                };
                let ctor_args = if dict_key_type == "Str" {
                    format!("sizeof({dict_val_type})")
                } else {
                    format!("sizeof({dict_key_type}), sizeof({dict_val_type})")
                };
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetMap __src = {coll_val}; \
                    {dict_c_type} __result = {ctor}({ctor_args}); \
                    {iter_loop} \
                    {dict_key_type} __key = *({dict_key_type}*)((char*)__src.keys + __i * __src.key_size); \
                    {dict_val_type} __val = *({dict_val_type}*)((char*)__src.values + __i * __src.val_size); \
                    if ({call_fn}({closure_ref}, __key, __val)) gorget_map_put(&__result, &__key, &__val); \
                    }} __result; }});");
            } else if is_set {
                // Set filter: iterate elements, create new set
                let set_ctor = if is_ordered_set { "gorget_ordered_set_new" } else { "gorget_set_new" };
                let loop_hdr = if is_ordered_set {
                    "for (size_t __oi = 0; __oi < __src.order_len; __oi++) { \
                    size_t __i = __src.order[__oi]; \
                    if (__src.states[__i] != 1) continue;"
                } else {
                    "for (size_t __i = 0; __i < __src.cap; __i++) { \
                    if (__src.states[__i] != 1) continue;"
                };
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetSet __src = {coll_val}; \
                    GorgetSet __result = {set_ctor}(sizeof({elem_type})); \
                    {loop_hdr} \
                    {elem_type} __elem = *({elem_type}*)((char*)__src.keys + __i * __src.key_size); \
                    if ({call_fn}({closure_ref}, __elem)) gorget_set_add(&__result, &__elem); \
                    }} __result; }});");
            } else {
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                    GorgetArray __result = gorget_array_new(sizeof({elem_type})); \
                    for (size_t __i = 0; __i < __src.len; __i++) {{ \
                    {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                    if ({call_fn}({closure_ref}, __elem)) gorget_array_push(&__result, &__elem); \
                    }} __result; }});");
            }
        }
        "map" => {
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _closure_type) = extract_callable(&args[1]);
            // Determine the output element type from the destination variable's type.
            // The destination is a Vector[OutType], so extract OutType from the C type.
            // Fall back to __typeof__ with a zero-initialized element (safe for empty arrays).
            let out_elem_type = dst.and_then(|d| {
                let dst_c_type = effective_c_type(d.0 as usize, func, registry, type_overrides);
                extract_element_type_from_collection(&dst_c_type)
                    .or_else(|| extract_element_type_from_collection(dst_c_type.strip_suffix('*').unwrap_or(&dst_c_type)))
            });
            use std::fmt::Write;
            if let Some(out_type) = out_elem_type {
                // Known output type — no need to read element 0
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                    {out_type} __map_out; \
                    GorgetArray __result = gorget_array_new(sizeof({out_type})); \
                    for (size_t __i = 0; __i < __src.len; __i++) {{ \
                    {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                    __map_out = {call_fn}({closure_ref}, __elem); \
                    gorget_array_push(&__result, &__map_out); \
                    }} __result; }});");
            } else {
                // Fallback: use __typeof__ but with a zero-initialized element (safe for empty)
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                    __typeof__({call_fn}({closure_ref}, ({elem_type}){{0}})) __map_out; \
                    GorgetArray __result = gorget_array_new(sizeof(__map_out)); \
                    for (size_t __i = 0; __i < __src.len; __i++) {{ \
                    {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                    __map_out = {call_fn}({closure_ref}, __elem); \
                    gorget_array_push(&__result, &__map_out); \
                    }} __result; }});");
            };
        }
        "fold" => {
            // args: [collection_ref, init_value, closure]
            if args.len() < 3 { return None; }
            let init = if let Operand::Constant(crate::ir::instructions::Constant::Str(s)) = &args[1] {
                let escaped = escape_c_string(s);
                format!("gorget_str_from_literal(\"{escaped}\", {})", s.len())
            } else {
                format_operand(&args[1], func, registry)
            };
            let (call_fn, closure_ref, _closure_type) = extract_callable(&args[2]);
            // Determine accumulator type from the init value operand.
            // This handles float/str accumulators correctly instead of defaulting to int64_t.
            let acc_type = match &args[1] {
                Operand::Constant(crate::ir::instructions::Constant::F64(_)) => "double".to_string(),
                Operand::Constant(crate::ir::instructions::Constant::Str(_)) => "Str".to_string(),
                Operand::Constant(crate::ir::instructions::Constant::Bool(_)) => "bool".to_string(),
                Operand::Copy(p) | Operand::Move(p) => {
                    let init_type = effective_c_type(p.local.0 as usize, func, registry, type_overrides);
                    if init_type != "int64_t" { init_type } else {
                        effective_c_type(dst.unwrap().0 as usize, func, registry, type_overrides)
                    }
                }
                _ => effective_c_type(dst.unwrap().0 as usize, func, registry, type_overrides),
            };
            use std::fmt::Write;
            if is_dict {
                // Dict/HashMap fold: iterate entries, pass (acc, key, value) to closure
                let iter_loop = if is_ordered_dict {
                    format!("for (size_t __oi = 0; __oi < __src.order_len; __oi++) {{ \
                    size_t __i = __src.order[__oi]; \
                    if (__src.states[__i] != 1) continue;")
                } else {
                    format!("for (size_t __i = 0; __i < __src.cap; __i++) {{ \
                    if (__src.states[__i] != 1) continue;")
                };
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetMap __src = {coll_val}; \
                    {acc_type} __acc = {init}; \
                    {iter_loop} \
                    {dict_key_type} __key = *({dict_key_type}*)((char*)__src.keys + __i * __src.key_size); \
                    {dict_val_type} __val = *({dict_val_type}*)((char*)__src.values + __i * __src.val_size); \
                    __acc = {call_fn}({closure_ref}, __acc, __key, __val); \
                    }} __acc; }});");
            } else if is_set {
                let loop_hdr = if is_ordered_set {
                    "for (size_t __oi = 0; __oi < __src.order_len; __oi++) { \
                    size_t __i = __src.order[__oi]; \
                    if (__src.states[__i] != 1) continue;"
                } else {
                    "for (size_t __i = 0; __i < __src.cap; __i++) { \
                    if (__src.states[__i] != 1) continue;"
                };
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetSet __src = {coll_val}; \
                    {acc_type} __acc = {init}; \
                    {loop_hdr} \
                    {elem_type} __elem = *({elem_type}*)((char*)__src.keys + __i * __src.key_size); \
                    __acc = {call_fn}({closure_ref}, __acc, __elem); \
                    }} __acc; }});");
            } else {
                if acc_type == "Str" {
                    // String fold: closure returns GorgetString, so accumulate as GorgetString
                    // and coerce to Str at the end.
                    let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                        GorgetString __acc = gorget_string_new({init}.data); \
                        for (size_t __i = 0; __i < __src.len; __i++) {{ \
                        {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                        __acc = {call_fn}({closure_ref}, (Str){{ .data = __acc.data, .len = __acc.len }}, __elem); \
                        }} (Str){{ .data = __acc.data, .len = __acc.len }}; }});");
                } else {
                    let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                        {acc_type} __acc = {init}; \
                        for (size_t __i = 0; __i < __src.len; __i++) {{ \
                        {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                        __acc = {call_fn}({closure_ref}, __acc, __elem); \
                        }} __acc; }});");
                }
            }
        }
        "reduce" => {
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _closure_type) = extract_callable(&args[1]);
            use std::fmt::Write;
            let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                if (__src.len == 0) gorget_panic(\"reduce() called on empty array\"); \
                {elem_type} __acc = GORGET_ARRAY_AT({elem_type}, __src, 0); \
                for (size_t __i = 1; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                __acc = {call_fn}({closure_ref}, __acc, __elem); \
                }} __acc; }});");
        }
        "enumerate" => {
            // enumerate returns an array of (index, element) tuples
            // For now, emit as a simple array (the caller handles tuple access)
            use std::fmt::Write;
            let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                GorgetArray __result = gorget_array_new(sizeof({elem_type})); \
                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                gorget_array_push(&__result, &__elem); \
                }} __result; }});");
        }
        "zip" => {
            // zip: args[0] = &self_vec, args[1] = other_vec
            // Returns Vector[(A, B)] — iterate both to min(len), build tuple structs
            if args.len() < 2 { return None; }
            let d = match dst { Some(d) => d, None => return None };
            let other_val = format_operand(&args[1], func, registry);
            // Get the tuple type from the destination local's IR type (not C type, which may be erased)
            let dst_type_id = func.locals[d.0 as usize].type_id;
            let dst_ir_name = if let Some(crate::ir::types::GirType::Named(name)) = registry.get(dst_type_id) {
                name.clone()
            } else {
                String::new()
            };
            let tuple_type = if let Some(inner) = dst_ir_name.strip_prefix("Vector__") {
                inner.to_string()
            } else {
                format!("Tuple__{elem_type}__int64_t")
            };
            // Extract _0 and _1 types from the tuple type name (Tuple__A__B)
            let (first_type, second_type) = if let Some(rest) = tuple_type.strip_prefix("Tuple__") {
                find_tuple_type_split(rest)
                    .unwrap_or((elem_type.clone(), "int64_t".to_string()))
            } else {
                (elem_type.clone(), "int64_t".to_string())
            };
            use std::fmt::Write;
            let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __a = {coll_val}; \
                GorgetArray __b = {other_val}; \
                size_t __min = __a.len < __b.len ? __a.len : __b.len; \
                GorgetArray __result = gorget_array_new(sizeof({tuple_type})); \
                for (size_t __i = 0; __i < __min; __i++) {{ \
                {tuple_type} __t; \
                __t._0 = GORGET_ARRAY_AT({first_type}, __a, __i); \
                __t._1 = GORGET_ARRAY_AT({second_type}, __b, __i); \
                gorget_array_push(&__result, &__t); \
                }} __result; }});");
        }
        "any" => {
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _closure_type) = extract_callable(&args[1]);
            use std::fmt::Write;
            let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                bool __any_result = false; \
                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                if ({call_fn}({closure_ref}, __elem)) {{ __any_result = true; break; }} \
                }} __any_result; }});");
        }
        "all" => {
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _closure_type) = extract_callable(&args[1]);
            use std::fmt::Write;
            let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                bool __all_result = true; \
                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                if (!{call_fn}({closure_ref}, __elem)) {{ __all_result = false; break; }} \
                }} __all_result; }});");
        }
        "each" | "for_each" => {
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _closure_type) = extract_callable(&args[1]);
            use std::fmt::Write;
            let _ = writeln!(out, "        {{ GorgetArray __src = {coll_val}; \
                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                {call_fn}({closure_ref}, __elem); \
                }} }}");
        }
        "flat_map" => {
            // flat_map: closure returns a Vector, concatenate all results
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _closure_type) = extract_callable(&args[1]);
            use std::fmt::Write;
            let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                GorgetArray __result = gorget_array_new(sizeof({elem_type})); \
                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                GorgetArray __inner = {call_fn}({closure_ref}, __elem); \
                for (size_t __j = 0; __j < __inner.len; __j++) {{ \
                {elem_type} __ie = GORGET_ARRAY_AT({elem_type}, __inner, __j); \
                gorget_array_push(&__result, &__ie); \
                }} gorget_array_free(&__inner); \
                }} __result; }});");
        }
        "find" => {
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _closure_type) = extract_callable(&args[1]);
            // find returns Option[T]
            let option_type = format!("Option__{elem_type}");
            use std::fmt::Write;
            let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                {option_type} __find_result = {{ .tag = 1 }}; \
                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                if ({call_fn}({closure_ref}, __elem)) {{ __find_result.tag = 0; __find_result.data.Some._0 = __elem; break; }} \
                }} __find_result; }});");
        }
        "count" => {
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _closure_type) = extract_callable(&args[1]);
            use std::fmt::Write;
            let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                int64_t __count = 0; \
                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                if ({call_fn}({closure_ref}, __elem)) __count++; \
                }} __count; }});");
        }
        "get_or_put" => {
            // Dict/HashMap only: args[0] = map ref, args[1] = key, args[2] = default value
            if args.len() < 3 { return None; }
            let key_str = format_operand(&args[1], func, registry);
            let val_str = format_operand(&args[2], func, registry);
            // Extract value type from the map mangled name
            let val_type = &elem_type; // elem_type for maps is the value type from extract_element_type
            // Use the actual key type from the mangled name (not __typeof__ which breaks for Str keys)
            let gop_key_type = if is_dict {
                &dict_key_type
            } else {
                // Fallback: infer from func_name
                let k = extract_map_key_type(func_name).unwrap_or("int64_t");
                // Leak to get &str with 'static lifetime matching dict_key_type
                &*Box::leak(k.to_string().into_boxed_str())
            };
            // For Str keys, wrap string literal operands in gorget_str_from_literal
            let key_init = if gop_key_type == "Str" && key_str.starts_with('"') {
                let inner = &key_str[1..key_str.len()-1];
                format!("gorget_str_from_literal({key_str}, {})", inner.len())
            } else {
                key_str.clone()
            };
            use std::fmt::Write;
            let _ = writeln!(out, "        {dst_str} = ({{ GorgetMap* __gop_m = {coll_ref}; \
                {gop_key_type} __gop_k = {key_init}; \
                {val_type}* __gop_ptr = ({val_type}*)gorget_map_get(__gop_m, &__gop_k); \
                if (!__gop_ptr) {{ \
                {val_type} __gop_v = {val_str}; \
                gorget_map_put(__gop_m, &__gop_k, &__gop_v); \
                __gop_ptr = ({val_type}*)gorget_map_get(__gop_m, &__gop_k); \
                }} *__gop_ptr; }});");
        }
        "keys" if is_dict => {
            // Dict/HashMap: extract all keys into a GorgetArray
            let key_type = extract_map_key_type(func_name).unwrap_or("int64_t");
            use std::fmt::Write;
            if is_ordered_dict {
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetMap* __km = {coll_ref}; \
                    GorgetArray __keys = gorget_array_new(sizeof({key_type})); \
                    for (size_t __oi = 0; __oi < __km->order_len; __oi++) {{ \
                    size_t __i = __km->order[__oi]; \
                    {key_type}* __kp = ({key_type}*)((__km)->keys + __i * (__km)->key_size); \
                    gorget_array_push(&__keys, __kp); \
                    }} __keys; }});");
            } else {
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetMap* __km = {coll_ref}; \
                    GorgetArray __keys = gorget_array_new(sizeof({key_type})); \
                    for (size_t __i = 0; __i < __km->cap; __i++) {{ \
                    if (__km->states[__i]) {{ \
                    {key_type}* __kp = ({key_type}*)((__km)->keys + __i * (__km)->key_size); \
                    gorget_array_push(&__keys, __kp); \
                    }} }} __keys; }});");
            }
        }
        "values" if is_dict => {
            // Dict/HashMap: extract all values into a GorgetArray
            use std::fmt::Write;
            if is_ordered_dict {
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetMap* __vm = {coll_ref}; \
                    GorgetArray __vals = gorget_array_new(sizeof({elem_type})); \
                    for (size_t __oi = 0; __oi < __vm->order_len; __oi++) {{ \
                    size_t __i = __vm->order[__oi]; \
                    {elem_type}* __vp = ({elem_type}*)((__vm)->values + __i * (__vm)->val_size); \
                    gorget_array_push(&__vals, __vp); \
                    }} __vals; }});");
            } else {
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetMap* __vm = {coll_ref}; \
                    GorgetArray __vals = gorget_array_new(sizeof({elem_type})); \
                    for (size_t __i = 0; __i < __vm->cap; __i++) {{ \
                    if (__vm->states[__i]) {{ \
                    {elem_type}* __vp = ({elem_type}*)((__vm)->values + __i * (__vm)->val_size); \
                    gorget_array_push(&__vals, __vp); \
                    }} }} __vals; }});");
            }
        }
        _ => return None,
    }

    Some(out)
}

/// Extract the inner Ok/Some payload type from a Result/Option type name.
/// For `Result__X__Y`, returns X (mapped through runtime_type_name).
/// For `Option__X`, returns X (mapped through runtime_type_name).
fn extract_enum_payload_type(name: &str) -> Option<String> {
    if let Some(inner) = name.strip_prefix("Option__") {
        let rt = runtime_type_name(inner).unwrap_or(inner);
        return Some(rt.to_string());
    }
    if let Some(inner) = name.strip_prefix("Result__") {
        // Try stripping common error types from the end
        for err_suffix in &["__Str", "__int64_t", "__bool", "__double"] {
            if let Some(ok_part) = inner.strip_suffix(err_suffix) {
                let rt = runtime_type_name(ok_part).unwrap_or(ok_part);
                return Some(rt.to_string());
            }
        }
        // Fallback: split at last __ separator
        if let Some(pos) = inner.rfind("__") {
            let ok_part = &inner[..pos];
            let rt = runtime_type_name(ok_part).unwrap_or(ok_part);
            return Some(rt.to_string());
        }
    }
    None
}

/// Extract the element C type from a mangled collection name.
/// e.g., "Vector__int64_t" → "int64_t", "Vector__Str" → "Str"
fn extract_collection_elem_type(name: &str) -> &str {
    // Strip the collection prefix to get the element type
    // Name format: Vector__<elem_type> or Vector__<elem_type>__<method>
    // e.g., "Vector__int64_t" → "int64_t"
    // e.g., "Vector__int64_t__contains" → "int64_t"
    // e.g., "Vector__Vector__int64_t" → "GorgetArray" (nested collection)
    for prefix in &["Vector__", "List__", "Array__", "Set__", "HashSet__"] {
        if let Some(rest) = name.strip_prefix(prefix) {
            if rest.is_empty() { return "int64_t"; }
            // Callable types (any generic variant) are GorgetClosure at runtime
            if rest.starts_with("Callable__") || rest.starts_with("MutCallable__") || rest.starts_with("ConsumeCallable__") {
                return "GorgetClosure";
            }
            // Check if the rest starts with a collection prefix (nested collections)
            if rest.starts_with("Vector__") || rest.starts_with("List__") || rest.starts_with("Array__") {
                return "GorgetArray";
            }
            if rest.starts_with("Dict__") || rest.starts_with("HashMap__") {
                return "GorgetMap";
            }
            if rest.starts_with("Set__") || rest.starts_with("HashSet__") {
                return "GorgetSet";
            }
            // Strip method suffix: find the last "__" and check if suffix is lowercase (method)
            if let Some(pos) = rest.rfind("__") {
                let suffix = &rest[pos + 2..];
                if !suffix.is_empty() && suffix.starts_with(|c: char| c.is_lowercase()) {
                    let elem = &rest[..pos];
                    return if elem.is_empty() { "int64_t" } else { elem };
                }
            }
            return rest;
        }
    }
    "int64_t" // fallback
}

fn is_collection_constructor(name: &str) -> bool {
    // Direct runtime type constructors
    if matches!(name, "String" | "Box") {
        return true;
    }
    // For generics, check if it's a type-only name (no trailing method) or __new()
    for prefix in &["Vector__", "Dict__", "HashMap__", "Set__", "HashSet__"] {
        if name.starts_with(prefix) {
            let last = extract_trailing_method(name, prefix);
            // __new() is a constructor method
            if last == "new" {
                return true;
            }
            // It's a constructor if neither rewrite nor inline method matches
            // and the name looks like just a type (no known method suffix)
            if try_rewrite_collection_method(name).is_none() && try_inline_method(name).is_none() {
                // Heuristic: type params start with uppercase or are primitive C types
                let looks_like_type = last.starts_with(|c: char| c.is_uppercase())
                    || matches!(last, "int64_t" | "int32_t" | "int16_t" | "int8_t"
                        | "uint64_t" | "uint32_t" | "uint16_t" | "uint8_t"
                        | "double" | "float" | "bool" | "Str" | "GorgetString");
                if looks_like_type {
                    return true;
                }
            }
            return false;
        }
    }
    false
}

/// Emit a collection constructor call, returning the C code string.
/// Returns `Some(code)` if this is a known collection type, `None` otherwise.
fn emit_collection_constructor(name: &str, dst_id: u32) -> Option<String> {
    emit_collection_constructor_to(name, &format!("_{dst_id}"))
}

/// Emit a collection constructor, assigning to the given destination expression
/// (e.g. `_5` for normal context, `f->_5` for poll context).
fn emit_collection_constructor_to(name: &str, dst: &str) -> Option<String> {
    let type_name = name.strip_suffix("__new").unwrap_or(name);
    let c_type = collection_type_alias(type_name);

    match c_type {
        Some("GorgetArray") => {
            let elem_c_type = extract_collection_elem_type(name);
            Some(format!("        {dst} = gorget_array_new(sizeof({elem_c_type}));\n"))
        }
        Some("GorgetMap") => {
            let (key_type, val_type) = extract_map_kv_types(type_name);
            let val_type = runtime_type_name(val_type).unwrap_or(val_type);
            let key_type = runtime_type_name(key_type).unwrap_or(key_type);
            if key_type == "Str" {
                let ctor = if type_name.starts_with("Dict__") { "gorget_dict_new_str" } else { "gorget_map_new_str" };
                Some(format!("        {dst} = {ctor}(sizeof({val_type}));\n"))
            } else {
                let ctor = if type_name.starts_with("Dict__") { "gorget_dict_new" } else { "gorget_map_new" };
                Some(format!("        {dst} = {ctor}(sizeof({key_type}), sizeof({val_type}));\n"))
            }
        }
        Some("GorgetSet") => {
            let elem_c_type = extract_collection_elem_type(name);
            if type_name.starts_with("Set__") {
                // Ordered Set: preserves insertion order
                let elem_rt = runtime_type_name(elem_c_type).unwrap_or(elem_c_type);
                if elem_rt == "Str" {
                    Some(format!("        {dst} = gorget_ordered_set_new_str();\n"))
                } else {
                    Some(format!("        {dst} = gorget_ordered_set_new(sizeof({elem_c_type}));\n"))
                }
            } else {
                // HashSet: unordered
                Some(format!("        {dst} = gorget_set_new(sizeof({elem_c_type}));\n"))
            }
        }
        _ if type_name == "String" => {
            Some(format!("        {dst} = gorget_string_new(\"\");\n"))
        }
        _ => None,
    }
}

/// Extract key and value C types from a mangled Dict/HashMap name.
/// e.g., "Dict__Str__int64_t" → ("Str", "int64_t")
///       "HashMap__int64_t__Str" → ("int64_t", "Str")
fn extract_map_kv_types(name: &str) -> (&str, &str) {
    // Strip prefix
    for prefix in &["Dict__", "HashMap__"] {
        if let Some(rest) = name.strip_prefix(prefix) {
            // The rest is "K__V" — split on "__" but be careful with multi-word types
            // Common types: int64_t, int32_t, uint8_t, Str, GorgetString, double, bool
            if let Some(pos) = find_kv_split(rest) {
                return (&rest[..pos], &rest[pos + 2..]);
            }
        }
    }
    ("int64_t", "int64_t") // fallback
}

/// Find the split position between K and V in a "K__V" string.
/// Handles multi-part type names like "int64_t" (which contains no "__").
fn find_kv_split(s: &str) -> Option<usize> {
    // Try each possible "__" split point and check if both sides look like types
    let mut pos = 0;
    while let Some(idx) = s[pos..].find("__") {
        let real_idx = pos + idx;
        let left = &s[..real_idx];
        let right = &s[real_idx + 2..];
        if !left.is_empty() && !right.is_empty() && looks_like_c_type(left) {
            return Some(real_idx);
        }
        pos = real_idx + 2;
    }
    None
}

fn looks_like_c_type(s: &str) -> bool {
    matches!(s,
        "int64_t" | "int32_t" | "int16_t" | "int8_t"
        | "uint64_t" | "uint32_t" | "uint16_t" | "uint8_t"
        | "double" | "float" | "bool" | "Str" | "GorgetString"
    ) || s.starts_with(|c: char| c.is_uppercase())
}

/// Emit inline method in poll (coroutine) context. Uses `f->_N` frame field access
/// instead of `_N` local variable access.
fn emit_poll_inline_method(
    out: &mut String,
    method: &InlineMethod,
    dst: &Option<LocalId>,
    args: &[Operand],
    func: &Function,
    registry: &TypeRegistry,
    func_name: &str,
    type_overrides: &std::collections::HashMap<usize, String>,
) {
    use std::fmt::Write;
    let self_raw = if !args.is_empty() {
        fmt_operand_poll(&args[0], func, registry)
    } else {
        "/*no self*/".to_string()
    };
    let self_ptr = is_self_pointer(args, func, registry);
    let self_str = deref_self(&self_raw, self_ptr);

    match method {
        InlineMethod::Pop => {
            if let Some(dst_id) = dst {
                let option_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let elem_c_type = option_type.strip_prefix("Option__").unwrap_or("int64_t");
                let _ = writeln!(out,
                    "        f->_{id} = ({{ {option_type} __pr; if ({self_str}.len > 0) {{ \
                    {elem_c_type} __elem = *({elem_c_type}*)((char*){self_str}.data + {self_str}.elem_size * --{self_str}.len); \
                    __pr = ({option_type}){{.tag = 0, .data.Some = {{__elem}}}}; }} \
                    else {{ __pr = ({option_type}){{.tag = 1}}; }} __pr; }});",
                    id = dst_id.0);
            }
        }
        InlineMethod::Sort => {
            let cmp = sort_comparator_for_func_name(func_name);
            let _ = writeln!(out,
                "        qsort({self_str}.data, {self_str}.len, {self_str}.elem_size, {cmp});");
        }
        InlineMethod::Sorted => {
            if let Some(dst_id) = dst {
                let cmp = sort_comparator_for_func_name(func_name);
                let self_addr = addr_self(&self_raw, self_ptr);
                let _ = writeln!(out,
                    "        f->_{id} = gorget_array_clone({self_addr}); \
                    qsort(f->_{id}.data, f->_{id}.len, f->_{id}.elem_size, {cmp});",
                    id = dst_id.0);
            }
        }
        InlineMethod::Reversed => {
            if let Some(dst_id) = dst {
                let self_addr = addr_self(&self_raw, self_ptr);
                let _ = writeln!(out,
                    "        f->_{id} = gorget_array_clone({self_addr}); \
                    gorget_array_reverse(&f->_{id});",
                    id = dst_id.0);
            }
        }
        InlineMethod::Unique => {
            if let Some(dst_id) = dst {
                let cmp = sort_comparator_for_func_name(func_name);
                let self_addr = addr_self(&self_raw, self_ptr);
                let _ = writeln!(out,
                    "        f->_{id} = gorget_array_clone({self_addr}); \
                    qsort(f->_{id}.data, f->_{id}.len, f->_{id}.elem_size, {cmp}); \
                    gorget_array_dedup(&f->_{id});",
                    id = dst_id.0);
            }
        }
        InlineMethod::OptionUnwrap => {
            let type_name = extract_type_from_method_call(func_name);
            if let Some(dst_id) = dst {
                let _ = writeln!(out,
                    "        f->_{id} = ({{ {type_name} __opt = {self_str}; \
                    if (__opt.tag != 0) {{ fprintf(stderr, \"unwrap called on None\\n\"); exit(1); }} \
                    __opt.data.Some._0; }});",
                    id = dst_id.0);
            }
        }
        InlineMethod::OptionIsSome => {
            if let Some(dst_id) = dst {
                let _ = writeln!(out, "        f->_{id} = ({self_str}.tag == 0);", id = dst_id.0);
            }
        }
        InlineMethod::OptionIsNone => {
            if let Some(dst_id) = dst {
                let _ = writeln!(out, "        f->_{id} = ({self_str}.tag != 0);", id = dst_id.0);
            }
        }
        InlineMethod::OptionUnwrapOr => {
            if let Some(dst_id) = dst {
                let default = if args.len() > 1 {
                    fmt_operand_poll(&args[1], func, registry)
                } else { "0".to_string() };
                let _ = writeln!(out,
                    "        f->_{id} = ({self_str}.tag == 0) ? {self_str}.data.Some._0 : {default};",
                    id = dst_id.0);
            }
        }
        InlineMethod::OptionExpect => {
            let type_name = extract_type_from_method_call(func_name);
            let msg = if args.len() > 1 {
                fmt_operand_poll(&args[1], func, registry)
            } else { "\"expect failed\"".to_string() };
            if let Some(dst_id) = dst {
                if msg.starts_with('"') {
                    let _ = writeln!(out,
                        "        f->_{id} = ({{ {type_name} __opt = {self_str}; \
                        if (__opt.tag != 0) {{ fprintf(stderr, \"%s\\n\", {msg}); exit(1); }} \
                        __opt.data.Some._0; }});",
                        id = dst_id.0);
                } else {
                    let _ = writeln!(out,
                        "        f->_{id} = ({{ {type_name} __opt = {self_str}; \
                        if (__opt.tag != 0) {{ fprintf(stderr, \"%.*s\\n\", (int){msg}.len, {msg}.data); exit(1); }} \
                        __opt.data.Some._0; }});",
                        id = dst_id.0);
                }
            }
        }
        InlineMethod::OptionAndThen => {
            let type_name = extract_type_from_method_call(func_name);
            if let Some(dst_id) = dst {
                if args.len() > 1 {
                    let closure = fmt_operand_poll(&args[1], func, registry);
                    let closure_type = match &args[1] {
                        Operand::Copy(p) | Operand::Move(p) =>
                            effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                        _ => "void*".to_string(),
                    };
                    let call_fn = format!("{closure_type}__call");
                    let _ = writeln!(out,
                        "        f->_{id} = ({{ {type_name} __opt = {self_str}; {type_name} __res; \
                        if (__opt.tag == 0) {{ __res = {call_fn}(&{closure}, __opt.data.Some._0); }} \
                        else {{ __res.tag = 1; }} __res; }});",
                        id = dst_id.0);
                }
            }
        }
        InlineMethod::OptionMap => {
            let _type_name = extract_type_from_method_call(func_name);
            if let Some(dst_id) = dst {
                if args.len() > 1 {
                    let closure = fmt_operand_poll(&args[1], func, registry);
                    let closure_type = match &args[1] {
                        Operand::Copy(p) | Operand::Move(p) =>
                            effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                        _ => "void*".to_string(),
                    };
                    let call_fn = format!("{closure_type}__call");
                    let dst_c = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                    let _ = writeln!(out,
                        "        f->_{id} = ({{ {dst_c} __res; \
                        if ({self_str}.tag == 0) {{ __res.tag = 0; __res.data.Some._0 = {call_fn}(&{closure}, {self_str}.data.Some._0); }} \
                        else {{ __res.tag = 1; }} __res; }});",
                        id = dst_id.0);
                }
            }
        }
        InlineMethod::OptionFilter => {
            let type_name = extract_type_from_method_call(func_name);
            if let Some(dst_id) = dst {
                if args.len() > 1 {
                    let closure = fmt_operand_poll(&args[1], func, registry);
                    let closure_type = match &args[1] {
                        Operand::Copy(p) | Operand::Move(p) =>
                            effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                        _ => "void*".to_string(),
                    };
                    let call_fn = format!("{closure_type}__call");
                    let _ = writeln!(out,
                        "        f->_{id} = ({{ {type_name} __opt = {self_str}; {type_name} __res; \
                        if (__opt.tag == 0 && {call_fn}(&{closure}, __opt.data.Some._0)) {{ __res = __opt; }} \
                        else {{ __res.tag = 1; }} __res; }});",
                        id = dst_id.0);
                }
            }
        }
        InlineMethod::OptionOr => {
            if let Some(dst_id) = dst {
                let other = if args.len() > 1 {
                    if matches!(&args[1], Operand::Constant(Constant::Null)) {
                        let type_name = extract_type_from_method_call(func_name);
                        format!("({type_name}){{.tag = 1}}")
                    } else {
                        fmt_operand_poll(&args[1], func, registry)
                    }
                } else { format!("({{ }})")  };
                let _ = writeln!(out,
                    "        f->_{id} = ({self_str}.tag == 0) ? {self_str} : {other};",
                    id = dst_id.0);
            }
        }
        InlineMethod::OptionOrElse => {
            let type_name = extract_type_from_method_call(func_name);
            if let Some(dst_id) = dst {
                if args.len() > 1 {
                    let closure = fmt_operand_poll(&args[1], func, registry);
                    let closure_type = match &args[1] {
                        Operand::Copy(p) | Operand::Move(p) =>
                            effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                        _ => "void*".to_string(),
                    };
                    let call_fn = format!("{closure_type}__call");
                    let _ = writeln!(out,
                        "        f->_{id} = ({{ {type_name} __res; \
                        if ({self_str}.tag == 0) {{ __res = {self_str}; }} \
                        else {{ __res = {call_fn}(&{closure}); }} __res; }});",
                        id = dst_id.0);
                }
            }
        }
        InlineMethod::OptionUnwrapOrElse => {
            if let Some(dst_id) = dst {
                if args.len() > 1 {
                    let closure = fmt_operand_poll(&args[1], func, registry);
                    let closure_type = match &args[1] {
                        Operand::Copy(p) | Operand::Move(p) =>
                            effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                        _ => "void*".to_string(),
                    };
                    let call_fn = format!("{closure_type}__call");
                    let _ = writeln!(out,
                        "        f->_{id} = ({self_str}.tag == 0) ? {self_str}.data.Some._0 : {call_fn}(&{closure});",
                        id = dst_id.0);
                }
            }
        }
        InlineMethod::OptionFlatten => {
            if let Some(dst_id) = dst {
                let dst_c = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let _ = writeln!(out,
                    "        f->_{id} = ({{ {dst_c} __res; \
                    if ({self_str}.tag == 0) {{ __res = {self_str}.data.Some._0; }} \
                    else {{ __res.tag = 1; }} __res; }});",
                    id = dst_id.0);
            }
        }
        // Result methods
        InlineMethod::ResultUnwrap => {
            let rtype = extract_type_from_method_call(func_name);
            if let Some(dst_id) = dst {
                let _ = writeln!(out,
                    "        f->_{id} = ({{ {rtype} __r = {self_str}; \
                    if (__r.tag != 0) {{ fprintf(stderr, \"unwrap called on Error\\n\"); exit(1); }} \
                    __r.data.Ok._0; }});",
                    id = dst_id.0);
            }
        }
        InlineMethod::ResultExpect => {
            let type_name = extract_type_from_method_call(func_name);
            let msg = if args.len() > 1 {
                fmt_operand_poll(&args[1], func, registry)
            } else { "\"expect failed\"".to_string() };
            if let Some(dst_id) = dst {
                if msg.starts_with('"') {
                    let _ = writeln!(out,
                        "        f->_{id} = ({{ {type_name} __r = {self_str}; \
                        if (__r.tag != 0) {{ fprintf(stderr, \"%s\\n\", {msg}); exit(1); }} \
                        __r.data.Ok._0; }});",
                        id = dst_id.0);
                } else {
                    let _ = writeln!(out,
                        "        f->_{id} = ({{ {type_name} __r = {self_str}; \
                        if (__r.tag != 0) {{ fprintf(stderr, \"%.*s\\n\", (int){msg}.len, {msg}.data); exit(1); }} \
                        __r.data.Ok._0; }});",
                        id = dst_id.0);
                }
            }
        }
        InlineMethod::ResultIsOk => {
            if let Some(dst_id) = dst {
                let _ = writeln!(out, "        f->_{id} = ({self_str}.tag == 0);", id = dst_id.0);
            }
        }
        InlineMethod::ResultIsErr => {
            if let Some(dst_id) = dst {
                let _ = writeln!(out, "        f->_{id} = ({self_str}.tag != 0);", id = dst_id.0);
            }
        }
        InlineMethod::ResultUnwrapOr => {
            if let Some(dst_id) = dst {
                let default = if args.len() > 1 {
                    fmt_operand_poll(&args[1], func, registry)
                } else { "0".to_string() };
                let _ = writeln!(out,
                    "        f->_{id} = ({self_str}.tag == 0) ? {self_str}.data.Ok._0 : {default};",
                    id = dst_id.0);
            }
        }
        InlineMethod::ResultUnwrapErr => {
            let type_name = extract_type_from_method_call(func_name);
            if let Some(dst_id) = dst {
                let _ = writeln!(out,
                    "        f->_{id} = ({{ {type_name} __r = {self_str}; \
                    if (__r.tag == 0) {{ fprintf(stderr, \"unwrap_err on Ok\\n\"); exit(1); }} \
                    __r.data.Error._0; }});",
                    id = dst_id.0);
            }
        }
        InlineMethod::ResultMap => {
            if let Some(dst_id) = dst {
                let dst_c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let (closure_str, call_fn) = closure_call_info_poll(&args, func, registry, type_overrides, 1);
                let _ = writeln!(out,
                    "        f->_{id} = ({{ {dst_c_type} __rm; \
                    if ({self_str}.tag == 0) {{ __rm.tag = 0; __rm.data.Ok._0 = {call_fn}(&{closure_str}, {self_str}.data.Ok._0); }} \
                    else {{ __rm.tag = 1; __rm.data.Error._0 = {self_str}.data.Error._0; }} __rm; }});",
                    id = dst_id.0);
            }
        }
        InlineMethod::ResultAndThen => {
            if let Some(dst_id) = dst {
                let dst_c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let (closure_str, call_fn) = closure_call_info_poll(&args, func, registry, type_overrides, 1);
                let _ = writeln!(out,
                    "        f->_{id} = ({{ {dst_c_type} __rat; \
                    if ({self_str}.tag == 0) {{ \
                    __auto_type __cr = {call_fn}(&{closure_str}, {self_str}.data.Ok._0); \
                    if (__cr.tag == 0) {{ __rat.tag = 0; __rat.data.Ok._0 = __cr.data.Ok._0; }} \
                    else {{ __rat.tag = 1; __rat.data.Error._0 = __cr.data.Error._0; }} \
                    }} \
                    else {{ __rat.tag = 1; __rat.data.Error._0 = {self_str}.data.Error._0; }} __rat; }});",
                    id = dst_id.0);
            }
        }
        InlineMethod::ResultMapErr => {
            if let Some(dst_id) = dst {
                let dst_c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let (closure_str, call_fn) = closure_call_info_poll(&args, func, registry, type_overrides, 1);
                // Use __auto_type + coerce to handle GorgetString → Str mismatches
                let _ = writeln!(out,
                    "        f->_{id} = ({{ {dst_c_type} __rme; \
                    if ({self_str}.tag == 0) {{ __rme.tag = 0; __rme.data.Ok._0 = {self_str}.data.Ok._0; }} \
                    else {{ __rme.tag = 1; __auto_type __ev = {call_fn}(&{closure_str}, {self_str}.data.Error._0); \
                    memcpy(&__rme.data.Error._0, &__ev, sizeof(__rme.data.Error._0)); }} __rme; }});",
                    id = dst_id.0);
            }
        }
        InlineMethod::ResultOr => {
            if let Some(dst_id) = dst {
                let other = if args.len() > 1 { fmt_operand_poll(&args[1], func, registry) } else { self_str.clone() };
                let _ = writeln!(out,
                    "        f->_{id} = ({self_str}.tag == 0) ? {self_str} : {other};",
                    id = dst_id.0);
            }
        }
        InlineMethod::ResultOrElse => {
            if let Some(dst_id) = dst {
                let (closure_str, call_fn) = closure_call_info_poll(&args, func, registry, type_overrides, 1);
                let _ = writeln!(out,
                    "        f->_{id} = ({self_str}.tag == 0) ? {self_str} : {call_fn}(&{closure_str}, {self_str}.data.Error._0);",
                    id = dst_id.0);
            }
        }
        InlineMethod::ResultUnwrapOrElse => {
            if let Some(dst_id) = dst {
                let (closure_str, call_fn) = closure_call_info_poll(&args, func, registry, type_overrides, 1);
                let _ = writeln!(out,
                    "        f->_{id} = ({self_str}.tag == 0) ? {self_str}.data.Ok._0 : {call_fn}(&{closure_str}, {self_str}.data.Error._0);",
                    id = dst_id.0);
            }
        }
        InlineMethod::SetUnion => {
            if let Some(dst_id) = dst {
                let is_ordered = func_name.starts_with("Set__");
                let other = if args.len() > 1 { fmt_operand_poll(&args[1], func, registry) } else { "/*no other*/".to_string() };
                let iter_loop = if is_ordered {
                    format!("for (size_t __oi = 0; __oi < {other}.order_len; __oi++) {{ \
                    size_t __i = {other}.order[__oi]; \
                    if ({other}.states[__i] == 1) {{")
                } else {
                    format!("for (size_t __i = 0; __i < {other}.cap; __i++) {{ \
                    if ({other}.states[__i] == 1) {{")
                };
                let _ = writeln!(out,
                    "        f->_{id} = gorget_set_clone(&{self_str}); \
                    {iter_loop} \
                    gorget_set_add(&f->_{id}, (char*){other}.keys + __i * {other}.key_size); \
                    }} }}", id = dst_id.0);
            }
        }
        InlineMethod::SetIntersection => {
            if let Some(dst_id) = dst {
                let is_ordered = func_name.starts_with("Set__");
                let other = if args.len() > 1 { fmt_operand_poll(&args[1], func, registry) } else { "/*no other*/".to_string() };
                let ctor = if is_ordered { "gorget_ordered_set_new" } else { "gorget_set_new" };
                let iter_loop = if is_ordered {
                    format!("for (size_t __oi = 0; __oi < {self_str}.order_len; __oi++) {{ \
                    size_t __i = {self_str}.order[__oi]; \
                    if ({self_str}.states[__i] == 1) {{")
                } else {
                    format!("for (size_t __i = 0; __i < {self_str}.cap; __i++) {{ \
                    if ({self_str}.states[__i] == 1) {{")
                };
                let _ = writeln!(out,
                    "        f->_{id} = {ctor}({self_str}.key_size); \
                    {iter_loop} \
                    const void* __k = (char*){self_str}.keys + __i * {self_str}.key_size; \
                    if (gorget_set_contains(&{other}, __k)) gorget_set_add(&f->_{id}, __k); \
                    }} }}", id = dst_id.0);
            }
        }
        InlineMethod::SetDifference => {
            if let Some(dst_id) = dst {
                let is_ordered = func_name.starts_with("Set__");
                let other = if args.len() > 1 { fmt_operand_poll(&args[1], func, registry) } else { "/*no other*/".to_string() };
                let ctor = if is_ordered { "gorget_ordered_set_new" } else { "gorget_set_new" };
                let iter_loop = if is_ordered {
                    format!("for (size_t __oi = 0; __oi < {self_str}.order_len; __oi++) {{ \
                    size_t __i = {self_str}.order[__oi]; \
                    if ({self_str}.states[__i] == 1) {{")
                } else {
                    format!("for (size_t __i = 0; __i < {self_str}.cap; __i++) {{ \
                    if ({self_str}.states[__i] == 1) {{")
                };
                let _ = writeln!(out,
                    "        f->_{id} = {ctor}({self_str}.key_size); \
                    {iter_loop} \
                    const void* __k = (char*){self_str}.keys + __i * {self_str}.key_size; \
                    if (!gorget_set_contains(&{other}, __k)) gorget_set_add(&f->_{id}, __k); \
                    }} }}", id = dst_id.0);
            }
        }
        InlineMethod::SetSymmetricDifference => {
            if let Some(dst_id) = dst {
                let is_ordered = func_name.starts_with("Set__");
                let other = if args.len() > 1 { fmt_operand_poll(&args[1], func, registry) } else { "/*no other*/".to_string() };
                let ctor = if is_ordered { "gorget_ordered_set_new" } else { "gorget_set_new" };
                let iter_self = if is_ordered {
                    format!("for (size_t __oi = 0; __oi < {self_str}.order_len; __oi++) {{ \
                    size_t __i = {self_str}.order[__oi]; \
                    if ({self_str}.states[__i] == 1) {{")
                } else {
                    format!("for (size_t __i = 0; __i < {self_str}.cap; __i++) {{ \
                    if ({self_str}.states[__i] == 1) {{")
                };
                let iter_other = if is_ordered {
                    format!("for (size_t __oi = 0; __oi < {other}.order_len; __oi++) {{ \
                    size_t __i = {other}.order[__oi]; \
                    if ({other}.states[__i] == 1) {{")
                } else {
                    format!("for (size_t __i = 0; __i < {other}.cap; __i++) {{ \
                    if ({other}.states[__i] == 1) {{")
                };
                let _ = writeln!(out,
                    "        f->_{id} = {ctor}({self_str}.key_size); \
                    {iter_self} \
                    const void* __k = (char*){self_str}.keys + __i * {self_str}.key_size; \
                    if (!gorget_set_contains(&{other}, __k)) gorget_set_add(&f->_{id}, __k); \
                    }} }} \
                    {iter_other} \
                    const void* __k = (char*){other}.keys + __i * {other}.key_size; \
                    if (!gorget_set_contains(&{self_str}, __k)) gorget_set_add(&f->_{id}, __k); \
                    }} }}", id = dst_id.0);
            }
        }
        InlineMethod::SetIsSubset => {
            let other = if args.len() > 1 { fmt_operand_poll(&args[1], func, registry) } else { "/*no arg*/".to_string() };
            if let Some(dst_id) = dst {
                // is_subset doesn't need ordering — just checks containment
                let _ = writeln!(out,
                    "        f->_{id} = ({{ bool __sub = true; \
                    for (size_t __i = 0; __i < {self_str}.cap; __i++) {{ \
                    if ({self_str}.states[__i] == 1) {{ \
                    if (!gorget_set_contains(&{other}, (char*){self_str}.keys + __i * {self_str}.key_size)) {{ __sub = false; break; }} \
                    }} }} __sub; }});", id = dst_id.0);
            }
        }
        InlineMethod::SetIsSuperset => {
            let other = if args.len() > 1 { fmt_operand_poll(&args[1], func, registry) } else { "/*no arg*/".to_string() };
            if let Some(dst_id) = dst {
                let _ = writeln!(out,
                    "        f->_{id} = ({{ bool __sub = true; \
                    for (size_t __i = 0; __i < {other}.cap; __i++) {{ \
                    if ({other}.states[__i] == 1) {{ \
                    if (!gorget_set_contains(&{self_str}, (char*){other}.keys + __i * {other}.key_size)) {{ __sub = false; break; }} \
                    }} }} __sub; }});", id = dst_id.0);
            }
        }
        InlineMethod::DictKeys => {
            if let Some(dst_id) = dst {
                let is_ordered = func_name.starts_with("Dict__");
                if is_ordered {
                    let _ = writeln!(out,
                        "        f->_{id} = gorget_array_new({self_str}.key_size); \
                        for (size_t __oi = 0; __oi < {self_str}.order_len; __oi++) {{ \
                        size_t __i = {self_str}.order[__oi]; \
                        if ({self_str}.states[__i] != 1) continue; \
                        gorget_array_push(&f->_{id}, (char*){self_str}.keys + __i * {self_str}.key_size); \
                        }}", id = dst_id.0);
                } else {
                    let _ = writeln!(out,
                        "        f->_{id} = gorget_array_new({self_str}.key_size); \
                        for (size_t __i = 0; __i < {self_str}.cap; __i++) {{ \
                        if ({self_str}.states[__i] == 1) {{ \
                        gorget_array_push(&f->_{id}, (char*){self_str}.keys + __i * {self_str}.key_size); \
                        }} }}", id = dst_id.0);
                }
            }
        }
        InlineMethod::DictValues => {
            if let Some(dst_id) = dst {
                let is_ordered = func_name.starts_with("Dict__");
                if is_ordered {
                    let _ = writeln!(out,
                        "        f->_{id} = gorget_array_new({self_str}.val_size); \
                        for (size_t __oi = 0; __oi < {self_str}.order_len; __oi++) {{ \
                        size_t __i = {self_str}.order[__oi]; \
                        if ({self_str}.states[__i] != 1) continue; \
                        gorget_array_push(&f->_{id}, (char*){self_str}.values + __i * {self_str}.val_size); \
                        }}", id = dst_id.0);
                } else {
                    let _ = writeln!(out,
                        "        f->_{id} = gorget_array_new({self_str}.val_size); \
                        for (size_t __i = 0; __i < {self_str}.cap; __i++) {{ \
                        if ({self_str}.states[__i] == 1) {{ \
                        gorget_array_push(&f->_{id}, (char*){self_str}.values + __i * {self_str}.val_size); \
                        }} }}", id = dst_id.0);
                }
            }
        }
        InlineMethod::DictItems => {
            if let Some(dst_id) = dst {
                let elem_size = format!("{self_str}.key_size + {self_str}.val_size");
                let is_ordered = func_name.starts_with("Dict__");
                if is_ordered {
                    let _ = writeln!(out,
                        "        f->_{id} = gorget_array_new({elem_size}); \
                        for (size_t __oi = 0; __oi < {self_str}.order_len; __oi++) {{ \
                        size_t __i = {self_str}.order[__oi]; \
                        if ({self_str}.states[__i] != 1) continue; \
                        uint8_t __buf[{elem_size}]; \
                        memcpy(__buf, (char*){self_str}.keys + __i * {self_str}.key_size, {self_str}.key_size); \
                        memcpy(__buf + {self_str}.key_size, (char*){self_str}.values + __i * {self_str}.val_size, {self_str}.val_size); \
                        gorget_array_push(&f->_{id}, __buf); \
                        }}", id = dst_id.0);
                } else {
                    let _ = writeln!(out,
                        "        f->_{id} = gorget_array_new({elem_size}); \
                        for (size_t __i = 0; __i < {self_str}.cap; __i++) {{ \
                        if ({self_str}.states[__i] == 1) {{ \
                        uint8_t __buf[{elem_size}]; \
                        memcpy(__buf, (char*){self_str}.keys + __i * {self_str}.key_size, {self_str}.key_size); \
                        memcpy(__buf + {self_str}.key_size, (char*){self_str}.values + __i * {self_str}.val_size, {self_str}.val_size); \
                        gorget_array_push(&f->_{id}, __buf); \
                        }} }}", id = dst_id.0);
                }
            }
        }
        InlineMethod::DictUpdate => {
            let other = if args.len() > 1 { fmt_operand_poll(&args[1], func, registry) } else { "/*no arg*/".to_string() };
            let _ = writeln!(out,
                "        {{ GorgetMap __du_src = {other}; GorgetMap* __du_dst = &{self_str}; \
                for (size_t __du_i = 0; __du_i < __du_src.cap; __du_i++) {{ \
                    if (__du_src.states[__du_i] != 1) continue; \
                    void* __du_k = (char*)__du_src.keys + __du_i * __du_src.key_size; \
                    void* __du_v = (char*)__du_src.values + __du_i * __du_src.val_size; \
                    gorget_map_put(__du_dst, __du_k, __du_v); \
                }} }}");
        }
        InlineMethod::DictGetOr => {
            let key = if args.len() > 1 { fmt_operand_poll(&args[1], func, registry) } else { "0".to_string() };
            let default = if args.len() > 2 { fmt_operand_poll(&args[2], func, registry) } else { "0".to_string() };
            if let Some(dst_id) = dst {
                let val_type = collection_element_c_type(func, dst_id.0 as usize, registry, func_name, type_overrides);
                let key_type = if let Some(rest) = func_name.strip_prefix("Dict__")
                    .or_else(|| func_name.strip_prefix("HashMap__")) {
                    if let Some(pos) = rest.find("__") { &rest[..pos] } else { "int64_t" }
                } else { "int64_t" };
                let key_ref = if key.starts_with("f->_") {
                    if key_type == "Str" {
                        format!("&(Str){{ .data = {key}.data, .len = {key}.len }}")
                    } else {
                        format!("&{key}")
                    }
                } else if key_type == "Str" {
                    let s = key.trim_matches('"');
                    format!("&(Str){{ .data = {key}, .len = {} }}", s.len())
                } else {
                    format!("&({key_type}){{{key}}}")
                };
                let _ = writeln!(out,
                    "        f->_{id} = ({{ {val_type}* __gop = ({val_type}*)gorget_map_get(&{self_str}, {key_ref}); \
                    __gop ? *__gop : {default}; }});",
                    id = dst_id.0);
            }
        }
    }
}

/// Emit higher-order collection methods (filter, map, fold, any, all, etc.)
/// in coroutine poll context, using frame field access (`f->_N`).
fn try_emit_poll_higher_order_method(
    func_name: &str,
    dst: &Option<LocalId>,
    args: &[Operand],
    func: &Function,
    registry: &TypeRegistry,
    type_overrides: &std::collections::HashMap<usize, String>,
    module: &Module,
) -> Option<String> {
    let method = extract_trailing_method(func_name, "");
    if !matches!(method, "filter" | "map" | "flat_map" | "fold" | "reduce"
        | "enumerate" | "any" | "all" | "each" | "for_each" | "find" | "count"
        | "get_or_put" | "keys" | "values" | "zip") {
        return None;
    }
    if func_name.starts_with("Option__") || func_name.starts_with("Result__")
        || func_name.starts_with("Regex__") || func_name.starts_with("Match__")
    {
        return None;
    }
    if args.is_empty() { return None; }

    // If the module already has a GIR-emitted function with this name, skip inline generation
    if module.functions.iter().any(|f| f.name == func_name) {
        return None;
    }

    let coll_ref = fmt_operand_poll(&args[0], func, registry);
    let coll_type = match &args[0] {
        Operand::Copy(p) | Operand::Move(p) =>
            effective_c_type(p.local.0 as usize, func, registry, type_overrides),
        _ => return None,
    };

    let bare_type = coll_type.strip_suffix('*').unwrap_or(&coll_type);
    let bare_type = bare_type.strip_prefix("const ").unwrap_or(bare_type);

    let is_dict = func_name.starts_with("Dict__") || func_name.starts_with("HashMap__")
        || bare_type.starts_with("Dict__") || bare_type.starts_with("HashMap__")
        || bare_type == "GorgetDict" || bare_type == "GorgetMap";
    let is_ordered_dict = func_name.starts_with("Dict__")
        || bare_type.starts_with("Dict__") || bare_type == "GorgetDict";
    let is_set = bare_type.starts_with("GorgetSet") || bare_type.starts_with("Set__")
        || bare_type.starts_with("HashSet__");
    let is_ordered_set = func_name.starts_with("Set__")
        || bare_type.starts_with("Set__");

    let elem_type = if let Some(elem) = extract_element_type_from_collection(bare_type) {
        elem
    } else if let Some(elem) = extract_element_type_from_method_name(func_name) {
        elem
    } else {
        "int64_t".to_string()
    };

    let (dict_key_type, dict_val_type) = if is_dict {
        let key = extract_map_key_type(func_name).unwrap_or("int64_t").to_string();
        let val = {
            let type_part = func_name.strip_prefix("Dict__")
                .or_else(|| func_name.strip_prefix("HashMap__"))
                .unwrap_or(func_name);
            if let Some(after_key) = type_part.strip_prefix(&format!("{key}__")) {
                if let Some(pos) = after_key.rfind("__") {
                    let suffix = &after_key[pos + 2..];
                    if !suffix.is_empty() && suffix.starts_with(|c: char| c.is_lowercase()) {
                        after_key[..pos].to_string()
                    } else {
                        after_key.to_string()
                    }
                } else {
                    after_key.to_string()
                }
            } else {
                elem_type.clone()
            }
        };
        (key, val)
    } else {
        (String::new(), String::new())
    };

    let coll_val = if coll_type.ends_with('*') {
        format!("(*{coll_ref})")
    } else {
        coll_ref.clone()
    };

    let dst_str = if let Some(d) = dst {
        format!("f->_{}", d.0)
    } else {
        String::new()
    };

    let extract_callable_poll = |arg: &Operand| -> (String, String, String) {
        if let Operand::Constant(crate::ir::instructions::Constant::FuncRef(name)) = arg {
            let c_name = mangle_name(name);
            let adapter = format!("__adapt_{c_name}");
            return (adapter, "NULL".to_string(), "void*".to_string());
        }
        let closure = fmt_operand_poll(arg, func, registry);
        let closure_type = match arg {
            Operand::Copy(p) | Operand::Move(p) =>
                effective_c_type(p.local.0 as usize, func, registry, type_overrides),
            _ => "void*".to_string(),
        };
        let call_fn = format!("{closure_type}__call");
        let closure_ref = format!("&{closure}");
        (call_fn, closure_ref, closure_type)
    };

    let mut out = String::new();
    use std::fmt::Write;

    match method {
        "filter" => {
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _) = extract_callable_poll(&args[1]);
            if is_dict {
                let dict_c_type = if bare_type.starts_with("Dict__") { "GorgetDict" } else { "GorgetMap" };
                let iter_loop = if is_ordered_dict {
                    format!("for (size_t __oi = 0; __oi < __src.order_len; __oi++) {{ \
                    size_t __i = __src.order[__oi]; \
                    if (__src.states[__i] != 1) continue;")
                } else {
                    format!("for (size_t __i = 0; __i < __src.cap; __i++) {{ \
                    if (__src.states[__i] != 1) continue;")
                };
                let ctor = if dict_key_type == "Str" {
                    if is_ordered_dict { "gorget_dict_new_str" } else { "gorget_map_new_str" }
                } else {
                    if is_ordered_dict { "gorget_dict_new" } else { "gorget_map_new" }
                };
                let ctor_args = if dict_key_type == "Str" {
                    format!("sizeof({dict_val_type})")
                } else {
                    format!("sizeof({dict_key_type}), sizeof({dict_val_type})")
                };
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetMap __src = {coll_val}; \
                    {dict_c_type} __result = {ctor}({ctor_args}); \
                    {iter_loop} \
                    {dict_key_type} __key = *({dict_key_type}*)((char*)__src.keys + __i * __src.key_size); \
                    {dict_val_type} __val = *({dict_val_type}*)((char*)__src.values + __i * __src.val_size); \
                    if ({call_fn}({closure_ref}, __key, __val)) gorget_map_put(&__result, &__key, &__val); \
                    }} __result; }});");
            } else if is_set {
                let set_ctor = if is_ordered_set { "gorget_ordered_set_new" } else { "gorget_set_new" };
                let loop_hdr = if is_ordered_set {
                    "for (size_t __oi = 0; __oi < __src.order_len; __oi++) { \
                    size_t __i = __src.order[__oi]; \
                    if (__src.states[__i] != 1) continue;"
                } else {
                    "for (size_t __i = 0; __i < __src.cap; __i++) { \
                    if (__src.states[__i] != 1) continue;"
                };
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetSet __src = {coll_val}; \
                    GorgetSet __result = {set_ctor}(sizeof({elem_type})); \
                    {loop_hdr} \
                    {elem_type} __elem = *({elem_type}*)((char*)__src.keys + __i * __src.key_size); \
                    if ({call_fn}({closure_ref}, __elem)) gorget_set_add(&__result, &__elem); \
                    }} __result; }});");
            } else {
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                    GorgetArray __result = gorget_array_new(sizeof({elem_type})); \
                    for (size_t __i = 0; __i < __src.len; __i++) {{ \
                    {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                    if ({call_fn}({closure_ref}, __elem)) gorget_array_push(&__result, &__elem); \
                    }} __result; }});");
            }
        }
        "map" => {
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _) = extract_callable_poll(&args[1]);
            // Same fix as non-poll: determine output element type from destination
            let out_elem_type = dst.and_then(|d| {
                let dst_c_type = effective_c_type(d.0 as usize, func, registry, type_overrides);
                extract_element_type_from_collection(&dst_c_type)
                    .or_else(|| extract_element_type_from_collection(dst_c_type.strip_suffix('*').unwrap_or(&dst_c_type)))
            });
            if let Some(out_type) = out_elem_type {
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                    {out_type} __map_out; \
                    GorgetArray __result = gorget_array_new(sizeof({out_type})); \
                    for (size_t __i = 0; __i < __src.len; __i++) {{ \
                    {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                    __map_out = {call_fn}({closure_ref}, __elem); \
                    gorget_array_push(&__result, &__map_out); \
                    }} __result; }});");
            } else {
                // Fallback: use __typeof__ with zero-initialized element (safe for empty)
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                    __typeof__({call_fn}({closure_ref}, ({elem_type}){{0}})) __map_out; \
                    GorgetArray __result = gorget_array_new(sizeof(__map_out)); \
                    for (size_t __i = 0; __i < __src.len; __i++) {{ \
                    {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                    __map_out = {call_fn}({closure_ref}, __elem); \
                    gorget_array_push(&__result, &__map_out); \
                    }} __result; }});");
            }
        }
        "fold" => {
            if args.len() < 3 { return None; }
            let init_val = if let Operand::Constant(Constant::Str(s)) = &args[1] {
                let escaped = escape_c_string(s);
                format!("gorget_str_from_literal(\"{escaped}\", {})", s.len())
            } else {
                fmt_operand_poll(&args[1], func, registry)
            };
            let (call_fn, closure_ref, _) = extract_callable_poll(&args[2]);
            if is_dict {
                let iter_loop = if is_ordered_dict {
                    format!("for (size_t __oi = 0; __oi < __src.order_len; __oi++) {{ \
                    size_t __i = __src.order[__oi]; \
                    if (__src.states[__i] != 1) continue;")
                } else {
                    format!("for (size_t __i = 0; __i < __src.cap; __i++) {{ \
                    if (__src.states[__i] != 1) continue;")
                };
                let acc_type = match &args[1] {
                    Operand::Copy(p) | Operand::Move(p) =>
                        effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                    Operand::Constant(Constant::I64(_)) => "int64_t".to_string(),
                    Operand::Constant(Constant::F64(_)) => "double".to_string(),
                    Operand::Constant(Constant::Str(_)) => "Str".to_string(),
                    _ => "int64_t".to_string(),
                };
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetMap __src = {coll_val}; \
                    {acc_type} __acc = {init_val}; \
                    {iter_loop} \
                    {dict_key_type} __key = *({dict_key_type}*)((char*)__src.keys + __i * __src.key_size); \
                    {dict_val_type} __val = *({dict_val_type}*)((char*)__src.values + __i * __src.val_size); \
                    __acc = {call_fn}({closure_ref}, __acc, __key, __val); \
                    }} __acc; }});");
            } else if is_set {
                let acc_type = match &args[1] {
                    Operand::Copy(p) | Operand::Move(p) =>
                        effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                    Operand::Constant(Constant::I64(_)) => "int64_t".to_string(),
                    Operand::Constant(Constant::F64(_)) => "double".to_string(),
                    Operand::Constant(Constant::Str(_)) => "Str".to_string(),
                    _ => "int64_t".to_string(),
                };
                let loop_hdr = if is_ordered_set {
                    "for (size_t __oi = 0; __oi < __src.order_len; __oi++) { \
                    size_t __i = __src.order[__oi]; \
                    if (__src.states[__i] != 1) continue;"
                } else {
                    "for (size_t __i = 0; __i < __src.cap; __i++) { \
                    if (__src.states[__i] != 1) continue;"
                };
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetSet __src = {coll_val}; \
                    {acc_type} __acc = {init_val}; \
                    {loop_hdr} \
                    {elem_type} __elem = *({elem_type}*)((char*)__src.keys + __i * __src.key_size); \
                    __acc = {call_fn}({closure_ref}, __acc, __elem); \
                    }} __acc; }});");
            } else {
                let acc_type = match &args[1] {
                    Operand::Copy(p) | Operand::Move(p) =>
                        effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                    Operand::Constant(Constant::I64(_)) => "int64_t".to_string(),
                    Operand::Constant(Constant::F64(_)) => "double".to_string(),
                    Operand::Constant(Constant::Str(_)) => "Str".to_string(),
                    _ => "int64_t".to_string(),
                };
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                    {acc_type} __acc = {init_val}; \
                    for (size_t __i = 0; __i < __src.len; __i++) {{ \
                    {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                    __acc = {call_fn}({closure_ref}, __acc, __elem); \
                    }} __acc; }});");
            }
        }
        "any" => {
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _) = extract_callable_poll(&args[1]);
            if is_set {
                let loop_hdr = if is_ordered_set {
                    "for (size_t __oi = 0; __oi < __src.order_len; __oi++) { \
                    size_t __i = __src.order[__oi]; \
                    if (__src.states[__i] != 1) continue;"
                } else {
                    "for (size_t __i = 0; __i < __src.cap; __i++) { \
                    if (__src.states[__i] != 1) continue;"
                };
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetSet __src = {coll_val}; bool __any_result = false; \
                    {loop_hdr} \
                    {elem_type} __elem = *({elem_type}*)((char*)__src.keys + __i * __src.key_size); \
                    if ({call_fn}({closure_ref}, __elem)) {{ __any_result = true; break; }} \
                    }} __any_result; }});");
            } else {
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; bool __any_result = false; \
                    for (size_t __i = 0; __i < __src.len; __i++) {{ \
                    {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                    if ({call_fn}({closure_ref}, __elem)) {{ __any_result = true; break; }} \
                    }} __any_result; }});");
            }
        }
        "all" => {
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _) = extract_callable_poll(&args[1]);
            if is_set {
                let loop_hdr = if is_ordered_set {
                    "for (size_t __oi = 0; __oi < __src.order_len; __oi++) { \
                    size_t __i = __src.order[__oi]; \
                    if (__src.states[__i] != 1) continue;"
                } else {
                    "for (size_t __i = 0; __i < __src.cap; __i++) { \
                    if (__src.states[__i] != 1) continue;"
                };
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetSet __src = {coll_val}; bool __all_result = true; \
                    {loop_hdr} \
                    {elem_type} __elem = *({elem_type}*)((char*)__src.keys + __i * __src.key_size); \
                    if (!{call_fn}({closure_ref}, __elem)) {{ __all_result = false; break; }} \
                    }} __all_result; }});");
            } else {
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; bool __all_result = true; \
                    for (size_t __i = 0; __i < __src.len; __i++) {{ \
                    {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                    if (!{call_fn}({closure_ref}, __elem)) {{ __all_result = false; break; }} \
                    }} __all_result; }});");
            }
        }
        "each" | "for_each" => {
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _) = extract_callable_poll(&args[1]);
            let _ = writeln!(out, "        {{ GorgetArray __src = {coll_val}; \
                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                {call_fn}({closure_ref}, __elem); \
                }} }}");
        }
        "reduce" => {
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _) = extract_callable_poll(&args[1]);
            let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                {elem_type} __acc = GORGET_ARRAY_AT({elem_type}, __src, 0); \
                for (size_t __i = 1; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                __acc = {call_fn}({closure_ref}, __acc, __elem); \
                }} __acc; }});");
        }
        "enumerate" => {
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _) = extract_callable_poll(&args[1]);
            let _ = writeln!(out, "        {{ GorgetArray __src = {coll_val}; \
                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                {call_fn}({closure_ref}, (int64_t)__i, __elem); \
                }} }}");
        }
        "find" => {
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _) = extract_callable_poll(&args[1]);
            if let Some(dst_id) = dst {
                let opt_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let _ = writeln!(out, "        f->_{id} = ({{ GorgetArray __src = {coll_val}; \
                    {opt_type} __fr; __fr.tag = 1; \
                    for (size_t __i = 0; __i < __src.len; __i++) {{ \
                    {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                    if ({call_fn}({closure_ref}, __elem)) {{ __fr.tag = 0; __fr.data.Some._0 = __elem; break; }} \
                    }} __fr; }});",
                    id = dst_id.0);
            }
        }
        "count" => {
            if args.len() < 2 { return None; }
            let (call_fn, closure_ref, _) = extract_callable_poll(&args[1]);
            let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; int64_t __cnt = 0; \
                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                if ({call_fn}({closure_ref}, __elem)) __cnt++; \
                }} __cnt; }});");
        }
        "keys" | "values" | "items" | "get_or_put" | "flat_map" | "zip" => {
            // These are less common in coroutines; return None to fall through to generic call
            return None;
        }
        _ => return None,
    }

    Some(out)
}

/// Inline method emit types for methods that don't map to a single runtime function.
enum InlineMethod {
    /// Pop: *(T*)(arr.data + arr.elem_size * --arr.len)
    Pop,
    /// Sort: qsort with type-specific comparator (in-place)
    Sort,
    /// Sorted: clone + sort (returns new array)
    Sorted,
    /// Reversed: clone + reverse (returns new array)
    Reversed,
    /// Unique: clone + sort + dedup (returns new array with duplicates removed)
    Unique,
    /// Option methods
    OptionUnwrap,
    OptionIsSome,
    OptionIsNone,
    OptionExpect,
    OptionUnwrapOr,
    /// Result methods
    ResultUnwrap,
    ResultExpect,
    ResultIsOk,
    ResultIsErr,
    ResultUnwrapOr,
    /// Dict keys/values/items
    DictKeys,
    DictValues,
    DictItems,
    /// Set operations
    SetUnion,
    SetIntersection,
    SetDifference,
    SetSymmetricDifference,
    /// Dict update/get_or
    DictUpdate,
    DictGetOr,
    /// Set is_subset / is_superset
    SetIsSubset,
    SetIsSuperset,
    /// Option combinators
    OptionAndThen,
    OptionOrElse,
    OptionUnwrapOrElse,
    OptionMap,
    OptionFilter,
    OptionOr,
    OptionFlatten,
    /// Result combinators
    ResultMap,
    ResultAndThen,
    ResultMapErr,
    ResultUnwrapErr,
    ResultUnwrapOrElse,
    ResultOr,
    ResultOrElse,
}

/// Represents a rewritten collection method call.
struct CollectionMethodCall {
    runtime_fn: &'static str,
    pass_by_ptr: bool,
    has_return: bool,
    needs_deref_cast: bool,
    field_access: Option<&'static str>,
    /// Whether to append `sizeof(element)` as an extra argument.
    needs_elem_size: bool,
    /// Whitespace variant for Str strip methods (used when no chars arg given).
    ws_variant: Option<&'static str>,
}

impl Default for CollectionMethodCall {
    fn default() -> Self {
        Self {
            runtime_fn: "",
            pass_by_ptr: false,
            has_return: false,
            needs_deref_cast: false,
            field_access: None,
            needs_elem_size: false,
            ws_variant: None,
        }
    }
}

/// Try to rewrite a collection method call (Vector__T__method, Dict__K__V__method, etc.)
/// to the corresponding runtime function.
fn try_rewrite_collection_method(func_name: &str) -> Option<CollectionMethodCall> {
    // Extract method name from the end: "Vector__int64_t__push" → "push"
    // Pattern: one or more Type__...__ segments followed by the method name.
    // Vector/Set have one type param, Dict/HashMap have two.

    // Try Vector (GorgetArray) patterns
    if func_name.starts_with("Vector__") || func_name.starts_with("GorgetArray__") {
        let method = extract_trailing_method(func_name, "Vector__");
        return match method {
            "push" => Some(CollectionMethodCall {
                runtime_fn: "gorget_array_push", pass_by_ptr: true,
                has_return: false, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "get" => Some(CollectionMethodCall {
                runtime_fn: "__INLINE_ARRAY_GET__", pass_by_ptr: false,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "set" => Some(CollectionMethodCall {
                runtime_fn: "gorget_array_set", pass_by_ptr: true,
                has_return: false, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "len" => Some(CollectionMethodCall {
                runtime_fn: "", pass_by_ptr: false,
                has_return: true, needs_deref_cast: false, field_access: Some("len"),
                ..Default::default()
            }),
            "contains" => Some(CollectionMethodCall {
                runtime_fn: "gorget_array_contains", pass_by_ptr: true,
                has_return: true, needs_deref_cast: false, field_access: None,
                needs_elem_size: true, ..Default::default()
            }),
            "index_of" => Some(CollectionMethodCall {
                runtime_fn: "gorget_array_index_of", pass_by_ptr: true,
                has_return: true, needs_deref_cast: false, field_access: None,
                needs_elem_size: false, ..Default::default()
            }),
            "reserve" => Some(CollectionMethodCall {
                runtime_fn: "gorget_array_reserve", pass_by_ptr: false,
                has_return: false, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "extend" => Some(CollectionMethodCall {
                runtime_fn: "gorget_array_extend", pass_by_ptr: true,
                has_return: false, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "clone" => Some(CollectionMethodCall {
                runtime_fn: "gorget_array_clone", pass_by_ptr: true,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "slice" => Some(CollectionMethodCall {
                runtime_fn: "gorget_array_slice", pass_by_ptr: true,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "remove" => Some(CollectionMethodCall {
                runtime_fn: "__INLINE_ARRAY_REMOVE__", pass_by_ptr: false,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "clear" => Some(CollectionMethodCall {
                runtime_fn: "gorget_array_clear", pass_by_ptr: false,
                has_return: false, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "insert" => Some(CollectionMethodCall {
                runtime_fn: "gorget_array_insert", pass_by_ptr: true,
                has_return: false, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "reverse" => Some(CollectionMethodCall {
                runtime_fn: "gorget_array_reverse", pass_by_ptr: false,
                has_return: false, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "is_empty" => Some(CollectionMethodCall {
                runtime_fn: "", pass_by_ptr: false,
                has_return: true, needs_deref_cast: false, field_access: Some("len == 0"),
                ..Default::default()
            }),
            "first" => Some(CollectionMethodCall {
                runtime_fn: "__INLINE_ARRAY_FIRST__", pass_by_ptr: false,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "last" => Some(CollectionMethodCall {
                runtime_fn: "__INLINE_ARRAY_LAST__", pass_by_ptr: false,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "binary_search" => Some(CollectionMethodCall {
                runtime_fn: "gorget_array_binary_search", pass_by_ptr: true,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            _ => None,
        };
    }

    // Try Set / HashSet (GorgetSet) patterns
    if func_name.starts_with("Set__") || func_name.starts_with("HashSet__") {
        let prefix = if func_name.starts_with("HashSet__") { "HashSet__" } else { "Set__" };
        let method = extract_trailing_method(func_name, prefix);
        return match method {
            "add" => Some(CollectionMethodCall {
                runtime_fn: "gorget_set_add", pass_by_ptr: true,
                has_return: false, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "contains" => Some(CollectionMethodCall {
                runtime_fn: "gorget_set_contains", pass_by_ptr: true,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "remove" => Some(CollectionMethodCall {
                runtime_fn: "gorget_set_remove", pass_by_ptr: true,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "clear" => Some(CollectionMethodCall {
                runtime_fn: "gorget_set_clear", pass_by_ptr: true,
                has_return: false, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "len" => Some(CollectionMethodCall {
                runtime_fn: "gorget_set_len", pass_by_ptr: true,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "is_empty" => Some(CollectionMethodCall {
                runtime_fn: "", pass_by_ptr: false,
                has_return: true, needs_deref_cast: false, field_access: Some("count == 0"),
                ..Default::default()
            }),
            _ => None,
        };
    }

    // Try Dict (ordered map) patterns
    if func_name.starts_with("Dict__") {
        let method = extract_trailing_method(func_name, "Dict__");
        return match method {
            "put" | "set" | "insert" => Some(CollectionMethodCall {
                runtime_fn: "gorget_map_put", pass_by_ptr: true,
                has_return: false, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "get" => Some(CollectionMethodCall {
                runtime_fn: "__INLINE_MAP_GET__", pass_by_ptr: true,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "contains" | "has" => Some(CollectionMethodCall {
                runtime_fn: "gorget_map_contains", pass_by_ptr: true,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "remove" => Some(CollectionMethodCall {
                runtime_fn: "gorget_map_remove", pass_by_ptr: true,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "len" => Some(CollectionMethodCall {
                runtime_fn: "gorget_map_len", pass_by_ptr: true,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "clear" => Some(CollectionMethodCall {
                runtime_fn: "gorget_map_clear", pass_by_ptr: true,
                has_return: false, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "is_empty" => Some(CollectionMethodCall {
                runtime_fn: "", pass_by_ptr: false,
                has_return: true, needs_deref_cast: false, field_access: Some("count == 0"),
                ..Default::default()
            }),
            _ => None,
        };
    }

    // Try Str method patterns
    if func_name.starts_with("Str__") {
        let method = &func_name[4..]; // Skip "Str_" (get "__method")
        // Str methods: receiver is Str (passed as const Str* in lowering)
        let simple = |runtime_fn| Some(CollectionMethodCall {
            runtime_fn, pass_by_ptr: false, has_return: true,
            needs_deref_cast: false, field_access: None,
            ..Default::default()
        });
        let void_fn = |runtime_fn| Some(CollectionMethodCall {
            runtime_fn, pass_by_ptr: false, has_return: false,
            needs_deref_cast: false, field_access: None,
            ..Default::default()
        });
        return match method {
            "_bytes" => simple("gorget_str_bytes"),
            "_codepoints" => simple("gorget_str_codepoints"),
            "_chars" => simple("gorget_str_chars"),
            "_trim" => simple("gorget_str_trim"),
            "_strip" => Some(CollectionMethodCall {
                runtime_fn: "gorget_str_strip", pass_by_ptr: false, has_return: true,
                needs_deref_cast: false, field_access: None,
                ws_variant: Some("gorget_str_trim"), ..Default::default()
            }),
            "_lstrip" => Some(CollectionMethodCall {
                runtime_fn: "gorget_str_lstrip", pass_by_ptr: false, has_return: true,
                needs_deref_cast: false, field_access: None,
                ws_variant: Some("gorget_str_lstrip_ws"), ..Default::default()
            }),
            "_rstrip" => Some(CollectionMethodCall {
                runtime_fn: "gorget_str_rstrip", pass_by_ptr: false, has_return: true,
                needs_deref_cast: false, field_access: None,
                ws_variant: Some("gorget_str_rstrip_ws"), ..Default::default()
            }),
            "_removeprefix" => simple("gorget_str_removeprefix"),
            "_removesuffix" => simple("gorget_str_removesuffix"),
            "_byte_slice" => simple("gorget_str_byte_slice"),
            "_byte_at" => simple("gorget_str_byte_at"),
            "_char_at" => simple("gorget_str_char_at"),
            "_is_alpha" => simple("gorget_str_is_alpha"),
            "_is_digit" => simple("gorget_str_is_digit"),
            "_is_alphanumeric" => simple("gorget_str_is_alphanumeric"),
            "_is_whitespace" => simple("gorget_str_is_whitespace"),
            "_is_upper" => simple("gorget_str_is_upper"),
            "_is_lower" => simple("gorget_str_is_lower"),
            "_is_hex_digit" => simple("gorget_str_is_hex_digit"),
            "_is_ascii" => simple("gorget_str_is_ascii"),
            "_to_upper" => simple("gorget_str_to_upper"),
            "_to_lower" => simple("gorget_str_to_lower"),
            "_replace" => simple("gorget_str_replace"),
            "_repeat" => simple("gorget_str_repeat"),
            "_pad_left" => simple("gorget_str_pad_left"),
            "_pad_right" => simple("gorget_str_pad_right"),
            "_len" => simple("gorget_str_codepoint_count"),
            "_byte_len" => simple("gorget_str_byte_len"),
            "_index_of" => simple("gorget_str_index_of"),
            "_count" => simple("gorget_str_count"),
            "_find" => simple("gorget_str_find"),
            "_contains" => simple("gorget_str_contains"),
            "_starts_with" => simple("gorget_str_starts_with"),
            "_ends_with" => simple("gorget_str_ends_with"),
            "_split" => simple("gorget_str_split"),
            "_join" => simple("gorget_str_join"),
            "_is_empty" => Some(CollectionMethodCall {
                runtime_fn: "", pass_by_ptr: false,
                has_return: true, needs_deref_cast: false, field_access: Some("len == 0"),
                ..Default::default()
            }),
            _ => { let _ = void_fn; None },
        };
    }

    // GorgetString (owned String) methods
    if func_name.starts_with("GorgetString__") {
        let method = &func_name[14..]; // Skip "GorgetString__"
        let simple = |runtime_fn| Some(CollectionMethodCall {
            runtime_fn, pass_by_ptr: false, has_return: true,
            needs_deref_cast: false, field_access: None,
            ..Default::default()
        });
        let void_fn = |runtime_fn| Some(CollectionMethodCall {
            runtime_fn, pass_by_ptr: false, has_return: false,
            needs_deref_cast: false, field_access: None,
            ..Default::default()
        });
        return match method {
            "len" => Some(CollectionMethodCall {
                runtime_fn: "", pass_by_ptr: false,
                has_return: true, needs_deref_cast: false, field_access: Some("len"),
                ..Default::default()
            }),
            "str" => Some(CollectionMethodCall {
                // GorgetString→Str view: emit inline coercion
                runtime_fn: "__INLINE_STRING_TO_STR__", pass_by_ptr: false,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "push" => void_fn("gorget_string_append_str"),
            "push_char" => void_fn("gorget_string_push_char"),
            "clear" => Some(CollectionMethodCall {
                runtime_fn: "__INLINE_STRING_CLEAR__", pass_by_ptr: false,
                has_return: false, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "push_line" => void_fn("gorget_string_push_line"),
            "push_str" => void_fn("gorget_string_append_str"),
            "capacity" => Some(CollectionMethodCall {
                runtime_fn: "", pass_by_ptr: false,
                has_return: true, needs_deref_cast: false, field_access: Some("cap"),
                ..Default::default()
            }),
            "is_empty" => Some(CollectionMethodCall {
                runtime_fn: "", pass_by_ptr: false,
                has_return: true, needs_deref_cast: false, field_access: Some("len == 0"),
                ..Default::default()
            }),
            "to_upper" => simple("gorget_str_to_upper"),
            "to_lower" => simple("gorget_str_to_lower"),
            // Read-only methods that delegate to gorget_str_* (GorgetString→Str coercion
            // is handled in emit_collection_method_call when runtime_fn starts with "gorget_str_")
            "contains" => simple("gorget_str_contains"),
            "starts_with" => simple("gorget_str_starts_with"),
            "ends_with" => simple("gorget_str_ends_with"),
            "replace" => simple("gorget_str_replace"),
            "find" => simple("gorget_str_find"),
            "index_of" => simple("gorget_str_index_of"),
            "count" => simple("gorget_str_count"),
            "split" => simple("gorget_str_split"),
            "join" => simple("gorget_str_join"),
            "repeat" => simple("gorget_str_repeat"),
            "pad_left" => simple("gorget_str_pad_left"),
            "pad_right" => simple("gorget_str_pad_right"),
            "trim" => simple("gorget_str_trim"),
            "removeprefix" => simple("gorget_str_removeprefix"),
            "removesuffix" => simple("gorget_str_removesuffix"),
            "byte_slice" => simple("gorget_str_byte_slice"),
            "byte_at" => simple("gorget_str_byte_at"),
            "char_at" => simple("gorget_str_char_at"),
            "is_alpha" => simple("gorget_str_is_alpha"),
            "is_digit" => simple("gorget_str_is_digit"),
            "is_alphanumeric" => simple("gorget_str_is_alphanumeric"),
            "is_whitespace" => simple("gorget_str_is_whitespace"),
            "is_upper" => simple("gorget_str_is_upper"),
            "is_lower" => simple("gorget_str_is_lower"),
            "is_hex_digit" => simple("gorget_str_is_hex_digit"),
            "is_ascii" => simple("gorget_str_is_ascii"),
            "bytes" => simple("gorget_str_bytes"),
            "codepoints" => simple("gorget_str_codepoints"),
            "chars" => simple("gorget_str_chars"),
            "byte_len" => simple("gorget_str_byte_len"),
            "strip" => Some(CollectionMethodCall {
                runtime_fn: "gorget_str_strip", pass_by_ptr: false, has_return: true,
                needs_deref_cast: false, field_access: None,
                ws_variant: Some("gorget_str_trim"), ..Default::default()
            }),
            "lstrip" => Some(CollectionMethodCall {
                runtime_fn: "gorget_str_lstrip", pass_by_ptr: false, has_return: true,
                needs_deref_cast: false, field_access: None,
                ws_variant: Some("gorget_str_lstrip_ws"), ..Default::default()
            }),
            "rstrip" => Some(CollectionMethodCall {
                runtime_fn: "gorget_str_rstrip", pass_by_ptr: false, has_return: true,
                needs_deref_cast: false, field_access: None,
                ws_variant: Some("gorget_str_rstrip_ws"), ..Default::default()
            }),
            _ => None,
        };
    }

    // uint8_t (byte) methods
    if func_name.starts_with("uint8_t__") {
        let method = &func_name["uint8_t__".len()..];
        let simple_bool = |runtime_fn| Some(CollectionMethodCall {
            runtime_fn, pass_by_ptr: false, has_return: true,
            needs_deref_cast: false, field_access: None,
            ..Default::default()
        });
        return match method {
            "is_alpha" => simple_bool("gorget_uint8_is_alpha"),
            "is_digit" => simple_bool("gorget_uint8_is_digit"),
            "is_alphanumeric" => simple_bool("gorget_uint8_is_alphanumeric"),
            "is_whitespace" => simple_bool("gorget_uint8_is_whitespace"),
            "is_upper" => simple_bool("gorget_uint8_is_upper"),
            "is_lower" => simple_bool("gorget_uint8_is_lower"),
            "is_hex_digit" => simple_bool("gorget_uint8_is_hex_digit"),
            "is_ascii" => simple_bool("gorget_uint8_is_ascii"),
            "to_upper" => simple_bool("gorget_uint8_to_upper"),
            "to_lower" => simple_bool("gorget_uint8_to_lower"),
            _ => None,
        };
    }

    // Try HashMap (unordered map) patterns
    if func_name.starts_with("HashMap__") {
        let method = extract_trailing_method(func_name, "HashMap__");
        return match method {
            "put" => Some(CollectionMethodCall {
                runtime_fn: "gorget_map_put", pass_by_ptr: true,
                has_return: false, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "get" => Some(CollectionMethodCall {
                runtime_fn: "__INLINE_MAP_GET__", pass_by_ptr: true,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "contains" | "has" => Some(CollectionMethodCall {
                runtime_fn: "gorget_map_contains", pass_by_ptr: true,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "remove" => Some(CollectionMethodCall {
                runtime_fn: "gorget_map_remove", pass_by_ptr: true,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "len" => Some(CollectionMethodCall {
                runtime_fn: "gorget_map_len", pass_by_ptr: true,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "clear" => Some(CollectionMethodCall {
                runtime_fn: "gorget_map_clear", pass_by_ptr: true,
                has_return: false, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "is_empty" => Some(CollectionMethodCall {
                runtime_fn: "", pass_by_ptr: false,
                has_return: true, needs_deref_cast: false, field_access: Some("count == 0"),
                ..Default::default()
            }),
            _ => None,
        };
    }

    None
}

/// Extract the trailing method name from a mangled collection method.
/// e.g. "Vector__int64_t__push" with prefix "Vector__" → "push"
/// e.g. "Dict__Str__int64_t__put" with prefix "Dict__" → "put"
fn extract_trailing_method<'a>(name: &'a str, _prefix: &str) -> &'a str {
    // The method name is after the last "__"
    if let Some(pos) = name.rfind("__") {
        &name[pos + 2..]
    } else {
        name
    }
}

/// Try to match a method call to an inline emit pattern.
/// Handles methods that don't map to a single runtime function call.
/// Emit code for primitive type static methods (int.parse, float.parse, int.default, etc.)
/// Returns Some(code) if this is a recognized primitive static method, None otherwise.
/// Emit an indirect call through a Callable parameter.
/// Handles two patterns:
/// 1. `__callable_N` — GIR-lowered callable param calls (N is the local ID)
/// 2. Direct parameter name matching (legacy)
/// Returns true if the call was handled, false if it should fall through to other handlers.
fn try_emit_callable_indirect_call(
    out: &mut String,
    func_name: &str,
    dst: &Option<LocalId>,
    args: &[Operand],
    func: &Function,
    registry: &TypeRegistry,
    type_overrides: &std::collections::HashMap<usize, String>,
    module: &Module,
) -> bool {
    // Pattern 0: __gorget_closure_call_N — escaped GorgetClosure returned from a function.
    // args[0] = the GorgetClosure local, args[1..] = actual function arguments.
    // Emits: ((ret_t(*)(void*, arg_types...))(closure.fn_ptr))(closure.env, args...)
    if let Some(id_str) = func_name.strip_prefix("__gorget_closure_call_") {
        if id_str.parse::<u32>().is_ok() {
            if args.is_empty() { return false; }
            let closure_str = format_operand(&args[0], func, registry);
            let mut arg_c_types = Vec::new();
            let mut arg_strs = Vec::new();
            for (i, arg) in args.iter().enumerate() {
                if i == 0 { continue; } // skip the closure itself
                let arg_str = format_operand(arg, func, registry);
                let arg_type = match arg {
                    Operand::Copy(p) | Operand::Move(p) => {
                        let t = effective_c_type(p.local.0 as usize, func, registry, type_overrides);
                        if t == "void" { "int64_t".to_string() } else { t }
                    }
                    Operand::Constant(c) => match c {
                        Constant::I64(_) | Constant::I32(_) => "int64_t".to_string(),
                        Constant::F64(_) => "double".to_string(),
                        Constant::Bool(_) => "bool".to_string(),
                        Constant::Str(_) => "const char*".to_string(),
                        _ => "int64_t".to_string(),
                    },
                };
                arg_c_types.push(arg_type);
                arg_strs.push(arg_str);
            }
            let ret_type = if let Some(dst_id) = dst {
                effective_c_type(dst_id.0 as usize, func, registry, type_overrides)
            } else {
                "void".to_string()
            };
            let mut cast_params = vec!["void*".to_string()];
            cast_params.extend_from_slice(&arg_c_types);
            let cast = format!("{ret_type}(*)({})", cast_params.join(", "));
            let mut all_args = vec![format!("{closure_str}.env")];
            all_args.extend_from_slice(&arg_strs);
            let call_expr = format!("(({cast})({closure_str}.fn_ptr))({})", all_args.join(", "));
            if let Some(dst_id) = dst {
                let _ = writeln!(out, "        _{} = {call_expr};", dst_id.0);
            } else {
                let _ = writeln!(out, "        {call_expr};");
            }
            return true;
        }
    }
    // Pattern 1: __callable_N convention from GIR lowering
    if let Some(id_str) = func_name.strip_prefix("__callable_") {
        if let Ok(param_idx) = id_str.parse::<u32>() {
            return emit_indirect_callable(out, param_idx as usize, dst, args, func, registry, type_overrides);
        }
    }
    // Pattern 2: Direct parameter name (legacy path)
    let is_module_fn = module.functions.iter().any(|f| f.name == func_name)
        || module.externs.iter().any(|e| e.name == func_name);
    if is_module_fn {
        return false;
    }
    if func_name.contains("__") {
        return false;
    }
    let param_count = func.params.len();
    if param_count == 0 || func.locals.len() <= param_count {
        return false;
    }
    let callable_param = func.locals[1..=param_count].iter().enumerate()
        .find(|(_, local)| local.name_hint.as_deref() == Some(func_name))
        .map(|(i, _)| i + 1); // +1 because _0 is return slot
    if let Some(param_idx) = callable_param {
        return emit_indirect_callable(out, param_idx, dst, args, func, registry, type_overrides);
    }
    false
}

/// Emit a function pointer cast + indirect call through local _{param_idx}.
fn emit_indirect_callable(
    out: &mut String,
    param_idx: usize,
    dst: &Option<LocalId>,
    args: &[Operand],
    func: &Function,
    registry: &TypeRegistry,
    type_overrides: &std::collections::HashMap<usize, String>,
) -> bool {
    // The first arg in the GIR call is the callable local itself (env pointer).
    // Remaining args are the actual function arguments.
    // Build arg types: first is void* (env), rest from operands
    let mut arg_c_types = Vec::new();
    let mut arg_strs = Vec::new();
    for (i, arg) in args.iter().enumerate() {
        let arg_str = format_operand(arg, func, registry);
        let arg_type = if i == 0 {
            // First arg is the callable env pointer — always void*
            "void*".to_string()
        } else {
            match arg {
                Operand::Copy(p) | Operand::Move(p) => {
                    let t = effective_c_type(p.local.0 as usize, func, registry, type_overrides);
                    // void means UNIT_TYPE (unresolved) — fallback to int64_t
                    if t == "void" { "int64_t".to_string() } else { t }
                }
                Operand::Constant(c) => match c {
                    Constant::I64(_) | Constant::I32(_) => "int64_t".to_string(),
                    Constant::F64(_) => "double".to_string(),
                    Constant::Bool(_) => "bool".to_string(),
                    Constant::Str(_) => "Str".to_string(),
                    _ => "int64_t".to_string(),
                },
            }
        };
        arg_c_types.push(arg_type);
        arg_strs.push(arg_str);
    }
    // Prefer the FnPtr return type recorded on the callable local (more precise).
    // Fall back to the destination local's type if FnPtr info is absent.
    let ret_type = func.locals.get(param_idx)
        .and_then(|local| match registry.get(local.type_id) {
            Some(GirType::FnPtr { return_type, .. }) => {
                let t = format_type(*return_type, registry);
                if t != "void" { Some(t) } else { None }
            }
            _ => None,
        })
        .unwrap_or_else(|| if let Some(dst_id) = dst {
            effective_c_type(dst_id.0 as usize, func, registry, type_overrides)
        } else {
            "void".to_string()
        });
    // _N is a void* pointing to [fn_ptr, env_ptr] pair.
    // Extract: fn = ((void**)_N)[0], env = ((void**)_N)[1]
    // Then call: ((ret_t(*)(void*, args...))fn)(env, args...)
    // Replace the first arg (which was _N itself) with env
    arg_strs[0] = format!("((void**)_{param_idx})[1]");
    let cast = format!("{ret_type}(*)({})", arg_c_types.join(", "));
    let fn_expr = format!("((void**)_{param_idx})[0]");
    let call_expr = format!("(({cast}){fn_expr})({})", arg_strs.join(", "));
    if let Some(dst_id) = dst {
        let _ = writeln!(out, "        _{id} = {call_expr};", id = dst_id.0);
    } else {
        let _ = writeln!(out, "        {call_expr};");
    }
    true
}

fn try_emit_primitive_static_method(
    func_name: &str,
    dst: &Option<LocalId>,
    args: &[Operand],
    func: &Function,
    registry: &TypeRegistry,
) -> Option<String> {
    let mut out = String::new();
    // Helper: extract (data_expr, len_expr) from a Str arg.
    // gorget_try_parse_int/float take (const char* s, size_t len) so they work
    // correctly on non-null-terminated substrings produced by byte_slice().
    let extract_str_data_len = |args: &[Operand], func: &Function, registry: &TypeRegistry| -> (String, String) {
        if args.is_empty() { return ("\"\"".to_string(), "0".to_string()); }
        match &args[0] {
            Operand::Constant(Constant::Str(lit)) => {
                let data = format_operand(&args[0], func, registry);
                let len = lit.len().to_string();
                (data, len)
            }
            Operand::Copy(p) | Operand::Move(p) => {
                let s = format_operand(&args[0], func, registry);
                let idx = p.local.0 as usize;
                let c_type = format_type(func.locals[idx].type_id, registry);
                if c_type == "Str" || c_type == "GorgetString" {
                    (format!("{s}.data"), format!("{s}.len"))
                } else {
                    // Raw const char* — use strlen
                    (s.clone(), format!("strlen({s})"))
                }
            }
            _ => {
                let s = format_operand(&args[0], func, registry);
                (s.clone(), format!("strlen({s})"))
            }
        }
    };
    match func_name {
        "int64_t__parse" | "int__parse"
        | "int8_t__parse" | "int8__parse"
        | "int16_t__parse" | "int16__parse"
        | "int32_t__parse" | "int32__parse"
        | "uint8_t__parse" | "uint8__parse"
        | "uint16_t__parse" | "uint16__parse"
        | "uint32_t__parse" | "uint32__parse"
        | "uint64_t__parse" | "uint64__parse" => {
            if let Some(dst_id) = dst {
                let (data_str, len_str) = extract_str_data_len(args, func, registry);
                // Determine the C cast type from the function name
                // Check uint variants BEFORE int variants (uint contains int as substring)
                let cast_type = if func_name.contains("uint8") { "uint8_t" }
                    else if func_name.contains("uint16") { "uint16_t" }
                    else if func_name.contains("uint32") { "uint32_t" }
                    else if func_name.contains("uint64") { "uint64_t" }
                    else if func_name.contains("int8") { "int8_t" }
                    else if func_name.contains("int16") { "int16_t" }
                    else if func_name.contains("int32") { "int32_t" }
                    else { "int64_t" };
                // Always use Option__cast_type since the GIR local type doesn't know about Option
                let opt_type = format!("Option__{cast_type}");
                let _ = writeln!(out,
                    "        _{id} = ({{ GorgetParseIntResult __pr = gorget_try_parse_int({data_str}, {len_str}); \
                    {opt_type} __opt; \
                    if (__pr.ok) {{ __opt.tag = 0; __opt.data.Some._0 = ({cast_type})__pr.value; }} \
                    else {{ __opt.tag = 1; }} \
                    __opt; }});",
                    id = dst_id.0);
            }
            Some(out)
        }
        "double__parse" | "float__parse" => {
            if let Some(dst_id) = dst {
                let (data_str, len_str) = extract_str_data_len(args, func, registry);
                let _ = writeln!(out,
                    "        _{id} = ({{ GorgetParseFloatResult __pr = gorget_try_parse_float({data_str}, {len_str}); \
                    Option__double __opt; \
                    if (__pr.ok) {{ __opt.tag = 0; __opt.data.Some._0 = (double)__pr.value; }} \
                    else {{ __opt.tag = 1; }} \
                    __opt; }});",
                    id = dst_id.0);
            }
            Some(out)
        }
        "bool__parse" => {
            if let Some(dst_id) = dst {
                let (data_str, len_str) = extract_str_data_len(args, func, registry);
                // bool parse: "true"→true, "false"→false, else None.
                // Use strncmp+length check to handle non-null-terminated Str slices.
                let _ = writeln!(out,
                    "        _{id} = ({{ Option__bool __opt; size_t __plen = (size_t){len_str}; \
                    if (__plen == 4 && strncmp({data_str}, \"true\", 4) == 0) {{ __opt.tag = 0; __opt.data.Some._0 = 1; }} \
                    else if (__plen == 5 && strncmp({data_str}, \"false\", 5) == 0) {{ __opt.tag = 0; __opt.data.Some._0 = 0; }} \
                    else {{ __opt.tag = 1; }} \
                    __opt; }});",
                    id = dst_id.0);
            }
            Some(out)
        }
        "int64_t__default" | "int__default" => {
            if let Some(dst_id) = dst {
                let _ = writeln!(out, "        _{id} = 0;", id = dst_id.0);
            }
            Some(out)
        }
        "int64_t__one" | "int__one" => {
            if let Some(dst_id) = dst {
                let _ = writeln!(out, "        _{id} = 1;", id = dst_id.0);
            }
            Some(out)
        }
        "double__default" | "float__default" => {
            if let Some(dst_id) = dst {
                let _ = writeln!(out, "        _{id} = 0.0;", id = dst_id.0);
            }
            Some(out)
        }
        "double__one" | "float__one" => {
            if let Some(dst_id) = dst {
                let _ = writeln!(out, "        _{id} = 1.0;", id = dst_id.0);
            }
            Some(out)
        }
        "bool__default" => {
            if let Some(dst_id) = dst {
                let _ = writeln!(out, "        _{id} = false;", id = dst_id.0);
            }
            Some(out)
        }
        "Str__default" | "str__default" => {
            if let Some(dst_id) = dst {
                let _ = writeln!(out, "        _{id} = (Str){{ .data = \"\", .len = 0 }};", id = dst_id.0);
            }
            Some(out)
        }
        _ => None,
    }
}

/// Return the `*_last_error()` companion function for C runtime functions that
/// use the error-last-error pattern (instead of GORGET_TRY/longjmp).
fn last_error_fn(func_name: &str) -> Option<&'static str> {
    // UDP
    if func_name.starts_with("gorget_udp_") || func_name == "udp_bind"
        || func_name.starts_with("UdpSocket__") {
        return Some("gorget_udp_last_error");
    }
    // ServerSocket (check before Socket to avoid prefix collision with "Socket__")
    if func_name.starts_with("gorget_server_socket_") || func_name == "server_socket_bind"
        || func_name.starts_with("ServerSocket__") {
        return Some("gorget_server_socket_last_error");
    }
    // Socket
    if func_name.starts_with("gorget_socket_") || func_name == "socket_connect"
        || func_name.starts_with("Socket__") {
        return Some("gorget_socket_last_error");
    }
    // TlsServerSocket (check before TlsSocket — "gorget_tls_server_" starts with "gorget_tls_")
    if func_name.starts_with("gorget_tls_server_") || func_name == "tls_server_bind"
        || func_name.starts_with("TlsServerSocket__") {
        return Some("gorget_tls_server_last_error");
    }
    // TLS
    if func_name.starts_with("gorget_tls_") || func_name == "tls_connect"
        || func_name.starts_with("TlsSocket__") {
        return Some("gorget_tls_last_error");
    }
    // Regex
    if func_name.starts_with("gorget_regex_") || func_name.starts_with("regex_compile") {
        return Some("gorget_regex_last_error");
    }
    // Crypto
    if func_name.starts_with("gorget_crypto_") || func_name.starts_with("crypto_") {
        return Some("gorget_crypto_last_error");
    }
    // Process spawn
    if func_name == "gorget_process_spawn" || func_name == "process_spawn" {
        return Some("gorget_process_spawn_err");
    }
    // Parse functions (std.conv)
    if func_name == "gorget_parse_int" || func_name == "parse_int"
        || func_name == "gorget_parse_float" || func_name == "parse_float" {
        return Some("gorget_parse_last_error");
    }
    None
}

/// Emit a function call with last_error-based Result wrapping.
/// Used for stdlib functions that store errors in a thread-local instead of throwing.
/// Returns Some(code) if handled, None otherwise.
fn try_emit_result_wrapped_call(
    func_name: &str,
    dst: &Option<LocalId>,
    args: &[Operand],
    func: &Function,
    registry: &TypeRegistry,
    type_overrides: &std::collections::HashMap<usize, String>,
) -> Option<String> {
    let dst_id = dst.as_ref()?;
    let c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
    if !c_type.starts_with("Result__") {
        return None;
    }

    // Determine the C function name and error-check function
    let c_func = map_stdlib_name(func_name);
    let err_fn = last_error_fn(func_name).or_else(|| last_error_fn(c_func))?;

    let mut out = String::new();

    // Build args: for equip methods, first arg is `self` which needs `&`
    let is_method = func_name.contains("__") && !func_name.starts_with("gorget_")
        && !func_name.starts_with("crypto_") && !func_name.starts_with("regex_")
        && !func_name.starts_with("socket_") && !func_name.starts_with("tls_")
        && !func_name.starts_with("udp_");

    let mut arg_parts: Vec<String> = Vec::new();
    for (i, arg) in args.iter().enumerate() {
        let arg_str = format_operand(arg, func, registry);
        if i == 0 && is_method {
            // Self argument: pass as pointer
            let is_ptr = if let Operand::Copy(p) | Operand::Move(p) = arg {
                let idx = p.local.0 as usize;
                let ct = effective_c_type(idx, func, registry, type_overrides);
                ct.ends_with('*')
            } else { false };
            if is_ptr {
                arg_parts.push(arg_str);
            } else {
                arg_parts.push(format!("&{arg_str}"));
            }
        } else {
            // Check if arg is a struct value that needs & (C runtime funcs take pointers)
            let needs_addr = if let Operand::Copy(p) | Operand::Move(p) = arg {
                let idx = p.local.0 as usize;
                let ct = effective_c_type(idx, func, registry, type_overrides);
                !ct.ends_with('*') && (
                    ct == "GorgetArray" || ct.starts_with("Vector__")
                    || ct.starts_with("Gorget") // GorgetX25519KeyPair, GorgetCipherContext, etc.
                )
            } else { false };
            // Check if arg is Str and function wants const char*
            let is_str = if let Operand::Copy(p) | Operand::Move(p) = arg {
                let idx = p.local.0 as usize;
                let ct = effective_c_type(idx, func, registry, type_overrides);
                ct == "Str" || ct == "GorgetString"
            } else { false };
            if needs_addr {
                arg_parts.push(format!("&{arg_str}"));
            } else if is_str {
                // C runtime functions in this handler take const char* — use gorget_str_to_cstr
                // to produce a properly null-terminated copy (Str slices are not null-terminated).
                // GorgetString: .data IS null-terminated (GorgetString guarantees this).
                let is_gorget_string = if let Operand::Copy(p) | Operand::Move(p) = arg {
                    effective_c_type(p.local.0 as usize, func, registry, type_overrides) == "GorgetString"
                } else { false };
                if is_gorget_string {
                    arg_parts.push(format!("{arg_str}.data"));
                } else {
                    arg_parts.push(format!("gorget_str_to_cstr({arg_str})"));
                }
            } else {
                arg_parts.push(arg_str);
            }
        }
    }

    // Special case: regex_compile takes an extra NULL flags argument
    if c_func == "gorget_regex_compile" && func_name != "regex_compile_with"
        && func_name != "gorget_regex_compile_with" {
        arg_parts.push("NULL".to_string());
    }

    let args_str = arg_parts.join(", ");

    // Emit: raw = call(); err = err_fn(); err ? Error(err) : Ok(raw)
    let ret_cstr = returns_cstr(c_func);
    let raw_capture = if ret_cstr {
        format!("gorget_str_from_cstr({c_func}({args_str}))")
    } else {
        format!("{c_func}({args_str})")
    };

    let _ = writeln!(out,
        "        _{id} = ({{ __typeof__(_{id}.data.Ok._0) __raw = {raw_capture}; \
        const char* __err = {err_fn}(); \
        {c_type} __wr; if (__err) {{ __wr.tag = 1; __wr.data.Error._0 = gorget_str_from_cstr(__err); }} \
        else {{ __wr.tag = 0; __wr.data.Ok._0 = __raw; }} __wr; }});",
        id = dst_id.0);

    Some(out)
}

/// Emit out-parameter calls for image/audio/deflate functions that use out-param ABI.
/// The C runtime signatures use pointer out-params instead of returning structs.
fn try_emit_outparam_call(
    func_name: &str,
    dst: &Option<LocalId>,
    args: &[Operand],
    func: &Function,
    registry: &TypeRegistry,
    type_overrides: &std::collections::HashMap<usize, String>,
) -> Option<String> {
    let c_name = map_stdlib_name(func_name);
    let mut out = String::new();

    match c_name {
        // gorget_image_load_rgba(Str path, int64_t* out_tag, int64_t* out_w, int64_t* out_h,
        //                       int64_t* out_ch, GorgetArray* out_data, Str* out_err)
        // Returns Result[Image, str] where Image = {width, height, channels, data}
        "gorget_image_load_rgba" | "image_load_rgba" => {
            let dst_id = dst.as_ref()?;
            let path = format_operand(&args[0], func, registry);
            let c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
            let _ = writeln!(out,
                "        _{id} = ({{ int64_t __tag = 0, __w = 0, __h = 0, __ch = 0; \
                GorgetArray __data = gorget_array_new(sizeof(uint8_t)); Str __err = {{0}}; \
                gorget_image_load_rgba({path}, &__tag, &__w, &__h, &__ch, &__data, &__err); \
                {c_type} __wr; if (__tag == 0) {{ __wr.tag = 0; __wr.data.Ok._0 = (Image){{.width = __w, .height = __h, .channels = __ch, .data = __data}}; }} \
                else {{ __wr.tag = 1; __wr.data.Error._0 = __err; }} __wr; }});",
                id = dst_id.0);
            Some(out)
        }
        // gorget_image_load_rgba_from_memory(const GorgetArray* data, int64_t* out_tag,
        //   int64_t* out_w, int64_t* out_h, int64_t* out_ch, GorgetArray* out_data, Str* out_err)
        "gorget_image_load_rgba_from_memory" => {
            let dst_id = dst.as_ref()?;
            let data_ptr = addr_of_array_operand(&args[0], func, registry, type_overrides);
            let c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
            let _ = writeln!(out,
                "        _{id} = ({{ int64_t __tag = 0, __w = 0, __h = 0, __ch = 0; \
                GorgetArray __data = gorget_array_new(sizeof(uint8_t)); Str __err = {{0}}; \
                gorget_image_load_rgba_from_memory({data_ptr}, &__tag, &__w, &__h, &__ch, &__data, &__err); \
                {c_type} __wr; if (__tag == 0) {{ __wr.tag = 0; __wr.data.Ok._0 = (Image){{.width = __w, .height = __h, .channels = __ch, .data = __data}}; }} \
                else {{ __wr.tag = 1; __wr.data.Error._0 = __err; }} __wr; }});",
                id = dst_id.0);
            Some(out)
        }
        // gorget_image_flip_vertically(int64_t w, int64_t h, int64_t ch, const GorgetArray* in_data,
        //   int64_t* out_w, int64_t* out_h, int64_t* out_ch, GorgetArray* out_data)
        // Input: Image struct (width, height, channels, data), Output: Image struct
        "gorget_image_flip_vertically" => {
            let dst_id = dst.as_ref()?;
            let img = format_operand(&args[0], func, registry);
            // Check if the operand is a pointer type — use -> instead of .
            let acc = if let Operand::Copy(p) | Operand::Move(p) = &args[0] {
                let ct = effective_c_type(p.local.0 as usize, func, registry, type_overrides);
                if ct.ends_with('*') { "->" } else { "." }
            } else { "." };
            let data_ref = if acc == "->" {
                format!("&{img}->data")
            } else {
                format!("&{img}.data")
            };
            let _ = writeln!(out,
                "        _{id} = ({{ int64_t __w = 0, __h = 0, __ch = 0; \
                GorgetArray __data = gorget_array_new(sizeof(uint8_t)); \
                gorget_image_flip_vertically({img}{acc}width, {img}{acc}height, {img}{acc}channels, {data_ref}, &__w, &__h, &__ch, &__data); \
                (Image){{.width = __w, .height = __h, .channels = __ch, .data = __data}}; }});",
                id = dst_id.0);
            Some(out)
        }
        // gorget_audio_load_wav(Str path, int64_t* out_tag, GorgetAudioChunk* out_chunk, Str* out_err)
        "gorget_audio_load_wav" => {
            let dst_id = dst.as_ref()?;
            let path = format_operand(&args[0], func, registry);
            let c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
            let _ = writeln!(out,
                "        _{id} = ({{ int64_t __tag = 0; GorgetAudioChunk __chunk = {{0}}; Str __err = {{0}}; \
                gorget_audio_load_wav({path}, &__tag, &__chunk, &__err); \
                {c_type} __wr; if (__tag == 0) {{ __wr.tag = 0; __wr.data.Ok._0 = __chunk; }} \
                else {{ __wr.tag = 1; __wr.data.Error._0 = __err; }} __wr; }});",
                id = dst_id.0);
            Some(out)
        }
        // gorget_audio_load_wav_from_memory(const GorgetArray* data, int64_t* out_tag, GorgetAudioChunk* out_chunk, Str* out_err)
        "gorget_audio_load_wav_from_memory" => {
            let dst_id = dst.as_ref()?;
            let data_ptr = addr_of_array_operand(&args[0], func, registry, type_overrides);
            let c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
            let _ = writeln!(out,
                "        _{id} = ({{ int64_t __tag = 0; GorgetAudioChunk __chunk = {{0}}; Str __err = {{0}}; \
                gorget_audio_load_wav_from_memory({data_ptr}, &__tag, &__chunk, &__err); \
                {c_type} __wr; if (__tag == 0) {{ __wr.tag = 0; __wr.data.Ok._0 = __chunk; }} \
                else {{ __wr.tag = 1; __wr.data.Error._0 = __err; }} __wr; }});",
                id = dst_id.0);
            Some(out)
        }
        // gorget_deflate_decompress(const GorgetArray* data, int64_t uncompressed_size,
        //   int64_t* out_tag, GorgetArray* out_data, Str* out_err)
        "gorget_deflate_decompress" => {
            let dst_id = dst.as_ref()?;
            let data_ptr = addr_of_array_operand(&args[0], func, registry, type_overrides);
            let size_arg = format_operand(&args[1], func, registry);
            let c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
            let _ = writeln!(out,
                "        _{id} = ({{ int64_t __tag = 0; GorgetArray __data = gorget_array_new(sizeof(uint8_t)); Str __err = {{0}}; \
                gorget_deflate_decompress({data_ptr}, {size_arg}, &__tag, &__data, &__err); \
                {c_type} __wr; if (__tag == 0) {{ __wr.tag = 0; __wr.data.Ok._0 = __data; }} \
                else {{ __wr.tag = 1; __wr.data.Error._0 = __err; }} __wr; }});",
                id = dst_id.0);
            Some(out)
        }
        _ => None,
    }
}

fn try_inline_method(func_name: &str) -> Option<InlineMethod> {
    // Vector pop/sort
    if func_name.starts_with("Vector__") || func_name.starts_with("GorgetArray__") {
        let method = extract_trailing_method(func_name, "Vector__");
        return match method {
            "pop" => Some(InlineMethod::Pop),
            "sort" => Some(InlineMethod::Sort),
            "sorted" => Some(InlineMethod::Sorted),
            "reversed" => Some(InlineMethod::Reversed),
            "unique" => Some(InlineMethod::Unique),
            _ => None,
        };
    }
    // Set operations
    if func_name.starts_with("Set__") || func_name.starts_with("HashSet__") {
        let set_prefix = if func_name.starts_with("HashSet__") { "HashSet__" } else { "Set__" };
        let method = extract_trailing_method(func_name, set_prefix);
        return match method {
            "union" => Some(InlineMethod::SetUnion),
            "intersection" => Some(InlineMethod::SetIntersection),
            "difference" => Some(InlineMethod::SetDifference),
            "symmetric_difference" => Some(InlineMethod::SetSymmetricDifference),
            "is_subset" => Some(InlineMethod::SetIsSubset),
            "is_superset" => Some(InlineMethod::SetIsSuperset),
            _ => None,
        };
    }
    // Dict/HashMap keys/values/items
    if func_name.starts_with("Dict__") || func_name.starts_with("HashMap__") {
        let method = extract_trailing_method(func_name, "Dict__");
        return match method {
            "keys" => Some(InlineMethod::DictKeys),
            "values" => Some(InlineMethod::DictValues),
            "items" => Some(InlineMethod::DictItems),
            "update" => Some(InlineMethod::DictUpdate),
            "get_or" => Some(InlineMethod::DictGetOr),
            _ => None,
        };
    }
    // Option methods
    if func_name.starts_with("Option__") {
        let method = extract_trailing_method(func_name, "Option__");
        return match method {
            "unwrap" => Some(InlineMethod::OptionUnwrap),
            "is_some" => Some(InlineMethod::OptionIsSome),
            "is_none" => Some(InlineMethod::OptionIsNone),
            "expect" => Some(InlineMethod::OptionExpect),
            "unwrap_or" => Some(InlineMethod::OptionUnwrapOr),
            "and_then" => Some(InlineMethod::OptionAndThen),
            "or_else" => Some(InlineMethod::OptionOrElse),
            "or" => Some(InlineMethod::OptionOr),
            "unwrap_or_else" => Some(InlineMethod::OptionUnwrapOrElse),
            "map" => Some(InlineMethod::OptionMap),
            "filter" => Some(InlineMethod::OptionFilter),
            "flatten" => Some(InlineMethod::OptionFlatten),
            _ => None,
        };
    }
    // Result methods
    if func_name.starts_with("Result__") {
        let method = extract_trailing_method(func_name, "Result__");
        return match method {
            "unwrap" => Some(InlineMethod::ResultUnwrap),
            "expect" => Some(InlineMethod::ResultExpect),
            "is_ok" => Some(InlineMethod::ResultIsOk),
            "is_err" | "is_error" => Some(InlineMethod::ResultIsErr),
            "unwrap_or" => Some(InlineMethod::ResultUnwrapOr),
            "unwrap_err" => Some(InlineMethod::ResultUnwrapErr),
            "map" => Some(InlineMethod::ResultMap),
            "and_then" => Some(InlineMethod::ResultAndThen),
            "map_err" => Some(InlineMethod::ResultMapErr),
            "or" => Some(InlineMethod::ResultOr),
            "or_else" => Some(InlineMethod::ResultOrElse),
            "unwrap_or_else" => Some(InlineMethod::ResultUnwrapOrElse),
            _ => None,
        };
    }
    None
}

/// Extract the Option/Result type name from a mangled method call.
/// e.g. "Option__int64_t__unwrap" → "Option__int64_t"
/// e.g. "Result__int64_t__Str__unwrap" → "Result__int64_t__Str"
fn extract_type_from_method_call(func_name: &str) -> &str {
    if let Some(pos) = func_name.rfind("__") {
        &func_name[..pos]
    } else {
        func_name
    }
}

/// Select the qsort comparator function based on the element type in the mangled name.
/// E.g., "Vector__double__sort" → "gorget_float_compare", "Vector__Str__sort" → "gorget_str_compare".
fn sort_comparator_for_func_name(func_name: &str) -> &'static str {
    if func_name.contains("__double__") || func_name.contains("__float__") {
        "gorget_float_compare"
    } else if func_name.contains("__Str__") {
        "gorget_str_compare"
    } else {
        "gorget_generic_compare"
    }
}

/// Emit inline C for methods that don't map to a runtime function.
fn emit_inline_method(
    out: &mut String,
    method: &InlineMethod,
    dst: &Option<LocalId>,
    args: &[Operand],
    func: &Function,
    registry: &TypeRegistry,
    func_name: &str,
    type_overrides: &std::collections::HashMap<usize, String>,
) {
    let self_raw = if !args.is_empty() {
        format_operand(&args[0], func, registry)
    } else {
        "/*no self*/".to_string()
    };
    let self_ptr = is_self_pointer(args, func, registry);
    // For inline methods that access struct fields, dereference pointers
    let self_str = deref_self(&self_raw, self_ptr);

    match method {
        InlineMethod::Pop => {
            // pop: returns Option[T] — Some(last_elem) if non-empty, None if empty
            if let Some(dst_id) = dst {
                let option_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                // Extract the element type from the Option type name (e.g. Option__int64_t → int64_t)
                let elem_c_type = option_type.strip_prefix("Option__")
                    .unwrap_or("int64_t");
                let _ = writeln!(out,
                    "        _{id} = ({{ {option_type} __pr; if ({self_str}.len > 0) {{ \
                    {elem_c_type} __elem = *({elem_c_type}*)((char*){self_str}.data + {self_str}.elem_size * --{self_str}.len); \
                    __pr = ({option_type}){{.tag = 0, .data.Some = {{__elem}}}}; }} \
                    else {{ __pr = ({option_type}){{.tag = 1}}; }} __pr; }});",
                    id = dst_id.0);
            }
        }
        InlineMethod::Sort => {
            let cmp = sort_comparator_for_func_name(func_name);
            let _ = writeln!(out,
                "        qsort({self_str}.data, {self_str}.len, {self_str}.elem_size, {cmp});");
        }
        InlineMethod::Sorted => {
            // sorted: clone + sort (returns new array)
            if let Some(dst_id) = dst {
                let cmp = sort_comparator_for_func_name(func_name);
                let self_addr = addr_self(&self_raw, self_ptr);
                let _c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let _ = writeln!(out,
                    "        _{id} = gorget_array_clone({self_addr}); \
                    qsort(_{id}.data, _{id}.len, _{id}.elem_size, {cmp});",
                    id = dst_id.0);
            }
        }
        InlineMethod::Reversed => {
            // reversed: clone + reverse (returns new array, original unchanged)
            if let Some(dst_id) = dst {
                let self_addr = addr_self(&self_raw, self_ptr);
                let _ = writeln!(out,
                    "        _{id} = gorget_array_clone({self_addr}); \
                    gorget_array_reverse(&_{id});",
                    id = dst_id.0);
            }
        }
        InlineMethod::Unique => {
            // unique: clone + sort + dedup (returns new array with duplicates removed)
            if let Some(dst_id) = dst {
                let cmp = sort_comparator_for_func_name(func_name);
                let self_addr = addr_self(&self_raw, self_ptr);
                let _ = writeln!(out,
                    "        _{id} = gorget_array_clone({self_addr}); \
                    qsort(_{id}.data, _{id}.len, _{id}.elem_size, {cmp}); \
                    gorget_array_dedup(&_{id});",
                    id = dst_id.0);
            }
        }
        InlineMethod::OptionUnwrap => {
            let type_name = extract_type_from_method_call(func_name);
            if let Some(dst_id) = dst {
                let _c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let _ = writeln!(out,
                    "        _{id} = ({{ {type_name} __opt = {self_str}; \
                    if (__opt.tag != 0) {{ fprintf(stderr, \"unwrap called on None\\n\"); exit(1); }} \
                    __opt.data.Some._0; }});",
                    id = dst_id.0);
            }
        }
        InlineMethod::OptionExpect => {
            let type_name = extract_type_from_method_call(func_name);
            let is_const_str = args.len() > 1 && matches!(&args[1], Operand::Constant(Constant::Str(_)));
            let msg = if args.len() > 1 {
                format_operand(&args[1], func, registry)
            } else {
                "\"expect failed\"".to_string()
            };
            if let Some(dst_id) = dst {
                let _c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                if is_const_str || msg.starts_with('"') {
                    // Plain C string — use %s
                    let _ = writeln!(out,
                        "        _{id} = ({{ {type_name} __opt = {self_str}; \
                        if (__opt.tag != 0) {{ fprintf(stderr, \"%s\\n\", {msg}); exit(1); }} \
                        __opt.data.Some._0; }});",
                        id = dst_id.0);
                } else {
                    // Str value — use %.*s with .len/.data
                    let _ = writeln!(out,
                        "        _{id} = ({{ {type_name} __opt = {self_str}; \
                        if (__opt.tag != 0) {{ fprintf(stderr, \"%.*s\\n\", (int){msg}.len, {msg}.data); exit(1); }} \
                        __opt.data.Some._0; }});",
                        id = dst_id.0);
                }
            }
        }
        InlineMethod::OptionIsSome => {
            if let Some(dst_id) = dst {
                let _ = writeln!(out,
                    "        _{id} = ({self_str}.tag == 0);", id = dst_id.0);
            }
        }
        InlineMethod::OptionIsNone => {
            if let Some(dst_id) = dst {
                let _ = writeln!(out,
                    "        _{id} = ({self_str}.tag != 0);", id = dst_id.0);
            }
        }
        InlineMethod::OptionUnwrapOr => {
            if let Some(dst_id) = dst {
                let default = if args.len() > 1 {
                    format_operand(&args[1], func, registry)
                } else { "0".to_string() };
                let _c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let _ = writeln!(out,
                    "        _{id} = ({self_str}.tag == 0) ? {self_str}.data.Some._0 : {default};",
                    id = dst_id.0);
            }
        }
        InlineMethod::ResultUnwrap => {
            let type_name = extract_type_from_method_call(func_name);
            if let Some(dst_id) = dst {
                let _c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let _ = writeln!(out,
                    "        _{id} = ({{ {type_name} __res = {self_str}; \
                    if (__res.tag != 0) {{ fprintf(stderr, \"unwrap called on Err\\n\"); exit(1); }} \
                    __res.data.Ok._0; }});",
                    id = dst_id.0);
            }
        }
        InlineMethod::ResultExpect => {
            let type_name = extract_type_from_method_call(func_name);
            let is_const_str = args.len() > 1 && matches!(&args[1], Operand::Constant(Constant::Str(_)));
            let msg = if args.len() > 1 {
                format_operand(&args[1], func, registry)
            } else {
                "\"expect failed\"".to_string()
            };
            if let Some(dst_id) = dst {
                let _c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                if is_const_str || msg.starts_with('"') {
                    let _ = writeln!(out,
                        "        _{id} = ({{ {type_name} __res = {self_str}; \
                        if (__res.tag != 0) {{ fprintf(stderr, \"%s\\n\", {msg}); exit(1); }} \
                        __res.data.Ok._0; }});",
                        id = dst_id.0);
                } else {
                    let _ = writeln!(out,
                        "        _{id} = ({{ {type_name} __res = {self_str}; \
                        if (__res.tag != 0) {{ fprintf(stderr, \"%.*s\\n\", (int){msg}.len, {msg}.data); exit(1); }} \
                        __res.data.Ok._0; }});",
                        id = dst_id.0);
                }
            }
        }
        InlineMethod::ResultIsOk => {
            if let Some(dst_id) = dst {
                let _ = writeln!(out,
                    "        _{id} = ({self_str}.tag == 0);", id = dst_id.0);
            }
        }
        InlineMethod::ResultIsErr => {
            if let Some(dst_id) = dst {
                let _ = writeln!(out,
                    "        _{id} = ({self_str}.tag != 0);", id = dst_id.0);
            }
        }
        InlineMethod::ResultUnwrapOr => {
            if let Some(dst_id) = dst {
                let default = if args.len() > 1 {
                    format_operand(&args[1], func, registry)
                } else { "0".to_string() };
                let _c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let _ = writeln!(out,
                    "        _{id} = ({self_str}.tag == 0) ? {self_str}.data.Ok._0 : {default};",
                    id = dst_id.0);
            }
        }
        InlineMethod::DictKeys => {
            // Iterate map entries and collect keys into an array
            // Dict__ uses insertion-order (order/order_len); HashMap__ uses bucket scan
            if let Some(dst_id) = dst {
                let is_ordered = func_name.starts_with("Dict__");
                if is_ordered {
                    let _ = writeln!(out,
                        "        _{id} = gorget_array_new({self_str}.key_size); \
                        for (size_t __oi = 0; __oi < {self_str}.order_len; __oi++) {{ \
                        size_t __i = {self_str}.order[__oi]; \
                        if ({self_str}.states[__i] != 1) continue; \
                        gorget_array_push(&_{id}, (char*){self_str}.keys + __i * {self_str}.key_size); \
                        }}",
                        id = dst_id.0);
                } else {
                    let _ = writeln!(out,
                        "        _{id} = gorget_array_new({self_str}.key_size); \
                        for (size_t __i = 0; __i < {self_str}.cap; __i++) {{ \
                        if ({self_str}.states[__i] == 1) {{ \
                        gorget_array_push(&_{id}, (char*){self_str}.keys + __i * {self_str}.key_size); \
                        }} }}",
                        id = dst_id.0);
                }
            }
        }
        InlineMethod::DictValues => {
            if let Some(dst_id) = dst {
                let is_ordered = func_name.starts_with("Dict__");
                if is_ordered {
                    let _ = writeln!(out,
                        "        _{id} = gorget_array_new({self_str}.val_size); \
                        for (size_t __oi = 0; __oi < {self_str}.order_len; __oi++) {{ \
                        size_t __i = {self_str}.order[__oi]; \
                        if ({self_str}.states[__i] != 1) continue; \
                        gorget_array_push(&_{id}, (char*){self_str}.values + __i * {self_str}.val_size); \
                        }}",
                        id = dst_id.0);
                } else {
                    let _ = writeln!(out,
                        "        _{id} = gorget_array_new({self_str}.val_size); \
                        for (size_t __i = 0; __i < {self_str}.cap; __i++) {{ \
                        if ({self_str}.states[__i] == 1) {{ \
                        gorget_array_push(&_{id}, (char*){self_str}.values + __i * {self_str}.val_size); \
                        }} }}",
                        id = dst_id.0);
                }
            }
        }
        InlineMethod::DictItems => {
            // Items returns array of key-value pairs packed into a buffer
            if let Some(dst_id) = dst {
                let elem_size = format!("{self_str}.key_size + {self_str}.val_size");
                let is_ordered = func_name.starts_with("Dict__");
                if is_ordered {
                    let _ = writeln!(out,
                        "        _{id} = gorget_array_new({elem_size}); \
                        for (size_t __oi = 0; __oi < {self_str}.order_len; __oi++) {{ \
                        size_t __i = {self_str}.order[__oi]; \
                        if ({self_str}.states[__i] != 1) continue; \
                        uint8_t __buf[{elem_size}]; \
                        memcpy(__buf, (char*){self_str}.keys + __i * {self_str}.key_size, {self_str}.key_size); \
                        memcpy(__buf + {self_str}.key_size, (char*){self_str}.values + __i * {self_str}.val_size, {self_str}.val_size); \
                        gorget_array_push(&_{id}, __buf); \
                        }}",
                        id = dst_id.0);
                } else {
                    let _ = writeln!(out,
                        "        _{id} = gorget_array_new({elem_size}); \
                        for (size_t __i = 0; __i < {self_str}.cap; __i++) {{ \
                        if ({self_str}.states[__i] == 1) {{ \
                        uint8_t __buf[{elem_size}]; \
                        memcpy(__buf, (char*){self_str}.keys + __i * {self_str}.key_size, {self_str}.key_size); \
                        memcpy(__buf + {self_str}.key_size, (char*){self_str}.values + __i * {self_str}.val_size, {self_str}.val_size); \
                        gorget_array_push(&_{id}, __buf); \
                        }} }}",
                        id = dst_id.0);
                }
            }
        }
        InlineMethod::SetUnion => {
            if let Some(dst_id) = dst {
                let is_ordered = func_name.starts_with("Set__");
                let other = if args.len() > 1 {
                    format_operand(&args[1], func, registry)
                } else { "/*no other*/".to_string() };
                let _c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let iter_loop = if is_ordered {
                    format!("for (size_t __oi = 0; __oi < {other}.order_len; __oi++) {{ \
                    size_t __i = {other}.order[__oi]; \
                    if ({other}.states[__i] == 1) {{")
                } else {
                    format!("for (size_t __i = 0; __i < {other}.cap; __i++) {{ \
                    if ({other}.states[__i] == 1) {{")
                };
                let _ = writeln!(out,
                    "        _{id} = gorget_set_clone(&{self_str}); \
                    {iter_loop} \
                    gorget_set_add(&_{id}, (char*){other}.keys + __i * {other}.key_size); \
                    }} }}",
                    id = dst_id.0);
            }
        }
        InlineMethod::SetIntersection => {
            if let Some(dst_id) = dst {
                let is_ordered = func_name.starts_with("Set__");
                let other = if args.len() > 1 {
                    format_operand(&args[1], func, registry)
                } else { "/*no other*/".to_string() };
                let _c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let ctor = if is_ordered { "gorget_ordered_set_new" } else { "gorget_set_new" };
                let iter_loop = if is_ordered {
                    format!("for (size_t __oi = 0; __oi < {self_str}.order_len; __oi++) {{ \
                    size_t __i = {self_str}.order[__oi]; \
                    if ({self_str}.states[__i] == 1) {{")
                } else {
                    format!("for (size_t __i = 0; __i < {self_str}.cap; __i++) {{ \
                    if ({self_str}.states[__i] == 1) {{")
                };
                let _ = writeln!(out,
                    "        _{id} = {ctor}({self_str}.key_size); \
                    {iter_loop} \
                    const void* __k = (char*){self_str}.keys + __i * {self_str}.key_size; \
                    if (gorget_set_contains(&{other}, __k)) gorget_set_add(&_{id}, __k); \
                    }} }}",
                    id = dst_id.0);
            }
        }
        InlineMethod::SetDifference => {
            if let Some(dst_id) = dst {
                let is_ordered = func_name.starts_with("Set__");
                let other = if args.len() > 1 {
                    format_operand(&args[1], func, registry)
                } else { "/*no other*/".to_string() };
                let _c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let ctor = if is_ordered { "gorget_ordered_set_new" } else { "gorget_set_new" };
                let iter_loop = if is_ordered {
                    format!("for (size_t __oi = 0; __oi < {self_str}.order_len; __oi++) {{ \
                    size_t __i = {self_str}.order[__oi]; \
                    if ({self_str}.states[__i] == 1) {{")
                } else {
                    format!("for (size_t __i = 0; __i < {self_str}.cap; __i++) {{ \
                    if ({self_str}.states[__i] == 1) {{")
                };
                let _ = writeln!(out,
                    "        _{id} = {ctor}({self_str}.key_size); \
                    {iter_loop} \
                    const void* __k = (char*){self_str}.keys + __i * {self_str}.key_size; \
                    if (!gorget_set_contains(&{other}, __k)) gorget_set_add(&_{id}, __k); \
                    }} }}",
                    id = dst_id.0);
            }
        }
        InlineMethod::SetSymmetricDifference => {
            if let Some(dst_id) = dst {
                let is_ordered = func_name.starts_with("Set__");
                let other = if args.len() > 1 {
                    format_operand(&args[1], func, registry)
                } else { "/*no other*/".to_string() };
                let _c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let ctor = if is_ordered { "gorget_ordered_set_new" } else { "gorget_set_new" };
                let iter_self = if is_ordered {
                    format!("for (size_t __oi = 0; __oi < {self_str}.order_len; __oi++) {{ \
                    size_t __i = {self_str}.order[__oi]; \
                    if ({self_str}.states[__i] == 1) {{")
                } else {
                    format!("for (size_t __i = 0; __i < {self_str}.cap; __i++) {{ \
                    if ({self_str}.states[__i] == 1) {{")
                };
                let iter_other = if is_ordered {
                    format!("for (size_t __oi = 0; __oi < {other}.order_len; __oi++) {{ \
                    size_t __i = {other}.order[__oi]; \
                    if ({other}.states[__i] == 1) {{")
                } else {
                    format!("for (size_t __i = 0; __i < {other}.cap; __i++) {{ \
                    if ({other}.states[__i] == 1) {{")
                };
                let _ = writeln!(out,
                    "        _{id} = {ctor}({self_str}.key_size); \
                    {iter_self} \
                    const void* __k = (char*){self_str}.keys + __i * {self_str}.key_size; \
                    if (!gorget_set_contains(&{other}, __k)) gorget_set_add(&_{id}, __k); \
                    }} }} \
                    {iter_other} \
                    const void* __k = (char*){other}.keys + __i * {other}.key_size; \
                    if (!gorget_set_contains(&{self_str}, __k)) gorget_set_add(&_{id}, __k); \
                    }} }}",
                    id = dst_id.0);
            }
        }
        // -- Dict update/get_or --
        InlineMethod::DictUpdate => {
            // update(other): merge other dict into self
            let other = if args.len() > 1 { format_operand(&args[1], func, registry) } else { "/*no arg*/".to_string() };
            let _ = writeln!(out,
                "        {{ GorgetMap __du_src = {other}; GorgetMap* __du_dst = &{self_str}; \
                for (size_t __du_i = 0; __du_i < __du_src.cap; __du_i++) {{ \
                    if (__du_src.states[__du_i] != 1) continue; \
                    void* __du_k = (char*)__du_src.keys + __du_i * __du_src.key_size; \
                    void* __du_v = (char*)__du_src.values + __du_i * __du_src.val_size; \
                    gorget_map_put(__du_dst, __du_k, __du_v); \
                }} }}");
        }
        InlineMethod::DictGetOr => {
            // get_or(key, default): return *get_ptr(key) or default
            let key = if args.len() > 1 { format_operand(&args[1], func, registry) } else { "0".to_string() };
            let default = if args.len() > 2 { format_operand(&args[2], func, registry) } else { "0".to_string() };
            if let Some(dst_id) = dst {
                let val_type = collection_element_c_type(func, dst_id.0 as usize, registry, func_name, type_overrides);
                // Need to take address of key — use compound literal if it's a direct value
                // Infer key type from function name
                let key_type = if let Some(rest) = func_name.strip_prefix("Dict__")
                    .or_else(|| func_name.strip_prefix("HashMap__")) {
                    if let Some(pos) = rest.find("__") {
                        &rest[..pos]
                    } else { "int64_t" }
                } else { "int64_t" };
                let key_ref = if key.starts_with('_') {
                    if key_type == "Str" {
                        format!("&(Str){{ .data = {key}.data, .len = {key}.len }}")
                    } else {
                        format!("&{key}")
                    }
                } else if key_type == "Str" {
                    // String literal key: wrap in gorget_str_from_literal
                    let s = key.trim_matches('"');
                    format!("&(Str){{ .data = {key}, .len = {} }}", s.len())
                } else {
                    format!("&({key_type}){{{key}}}")
                };
                // Wrap default in Str literal if val_type is Str and default is a C string literal
                let default_expr = if val_type == "Str" && default.starts_with('"') {
                    let s = default.trim_matches('"');
                    format!("((Str){{ .data = {default}, .len = {} }})", s.len())
                } else {
                    default.clone()
                };
                let _ = writeln!(out,
                    "        _{id} = ({{ {val_type}* __gop = ({val_type}*)gorget_map_get(&{self_str}, {key_ref}); \
                    __gop ? *__gop : {default_expr}; }});",
                    id = dst_id.0);
            }
        }

        // -- Set is_subset --
        InlineMethod::SetIsSubset => {
            let other = if args.len() > 1 { format_operand(&args[1], func, registry) } else { "/*no arg*/".to_string() };
            if let Some(dst_id) = dst {
                let elem_type = extract_collection_elem_type(func_name);
                let _ = writeln!(out,
                    "        _{id} = ({{ GorgetSet __ssub_self = {self_str}; GorgetSet __ssub_other = {other}; \
                    bool __ssub_result = true; \
                    for (size_t __ssub_i = 0; __ssub_i < __ssub_self.cap; __ssub_i++) {{ \
                        if (__ssub_self.states[__ssub_i] != 1) continue; \
                        {elem_type} __ssub_elem = *({elem_type}*)((char*)__ssub_self.keys + __ssub_i * __ssub_self.key_size); \
                        if (!gorget_set_contains(&__ssub_other, &__ssub_elem)) {{ __ssub_result = false; break; }} \
                    }} \
                    __ssub_result; }});",
                    id = dst_id.0);
            }
        }

        InlineMethod::SetIsSuperset => {
            // is_superset(other) = other.is_subset(self) — iterate other, check in self
            let other = if args.len() > 1 { format_operand(&args[1], func, registry) } else { "/*no arg*/".to_string() };
            if let Some(dst_id) = dst {
                let elem_type = extract_collection_elem_type(func_name);
                let _ = writeln!(out,
                    "        _{id} = ({{ GorgetSet __ssub_self = {other}; GorgetSet __ssub_other = {self_str}; \
                    bool __ssub_result = true; \
                    for (size_t __ssub_i = 0; __ssub_i < __ssub_self.cap; __ssub_i++) {{ \
                        if (__ssub_self.states[__ssub_i] != 1) continue; \
                        {elem_type} __ssub_elem = *({elem_type}*)((char*)__ssub_self.keys + __ssub_i * __ssub_self.key_size); \
                        if (!gorget_set_contains(&__ssub_other, &__ssub_elem)) {{ __ssub_result = false; break; }} \
                    }} \
                    __ssub_result; }});",
                    id = dst_id.0);
            }
        }

        // -- Option combinators --
        InlineMethod::OptionAndThen => {
            // and_then(f): if Some, return f(value); else None
            if let Some(dst_id) = dst {
                let (closure_str, call_fn) = closure_call_info(&args, func, registry, type_overrides, 1);
                let _ = writeln!(out,
                    "        _{id} = ({{ __typeof__({self_str}) __oat; \
                    if ({self_str}.tag == 0) {{ __oat = {call_fn}(&{closure_str}, {self_str}.data.Some._0); }} \
                    else {{ __oat.tag = 1; }} __oat; }});",
                    id = dst_id.0);
            }
        }
        InlineMethod::OptionOrElse => {
            // or_else(f): if Some, return self; else call f()
            if let Some(dst_id) = dst {
                let (closure_str, call_fn) = closure_call_info(&args, func, registry, type_overrides, 1);
                let _ = writeln!(out,
                    "        _{id} = ({self_str}.tag == 0) ? {self_str} : {call_fn}(&{closure_str});",
                    id = dst_id.0);
            }
        }
        InlineMethod::OptionUnwrapOrElse => {
            // unwrap_or_else(f): if Some, return value; else call f()
            if let Some(dst_id) = dst {
                let (closure_str, call_fn) = closure_call_info(&args, func, registry, type_overrides, 1);
                let _ = writeln!(out,
                    "        _{id} = ({self_str}.tag == 0) ? {self_str}.data.Some._0 : {call_fn}(&{closure_str});",
                    id = dst_id.0);
            }
        }
        InlineMethod::OptionMap => {
            // map(f): if Some, return Some(f(value)); else None
            // The type override pre-scan computes the correct output Option type
            // from the closure's return type (e.g., Option__Str for int→Str closure).
            if let Some(dst_id) = dst {
                let dst_c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let (closure_str, call_fn) = closure_call_info(&args, func, registry, type_overrides, 1);
                let _ = writeln!(out,
                    "        _{id} = ({{ {dst_c_type} __om; \
                    if ({self_str}.tag == 0) {{ __om.tag = 0; __om.data.Some._0 = {call_fn}(&{closure_str}, {self_str}.data.Some._0); }} \
                    else {{ __om.tag = 1; }} __om; }});",
                    id = dst_id.0);
            }
        }
        InlineMethod::OptionFilter => {
            // filter(f): if Some and f(value), return self; else None
            // Use __typeof__ on self (not dst) since dst may have wrong GIR type
            if let Some(dst_id) = dst {
                let (closure_str, call_fn) = closure_call_info(&args, func, registry, type_overrides, 1);
                let _ = writeln!(out,
                    "        _{id} = ({{ __typeof__({self_str}) __of; \
                    if ({self_str}.tag == 0 && {call_fn}(&{closure_str}, {self_str}.data.Some._0)) {{ __of = {self_str}; }} \
                    else {{ __of.tag = 1; }} __of; }});",
                    id = dst_id.0);
            }
        }
        InlineMethod::OptionOr => {
            // or(other): if Some, return self; else return other
            if let Some(dst_id) = dst {
                let other = if args.len() > 1 {
                    if matches!(&args[1], Operand::Constant(Constant::Null)) {
                        // None → emit proper tagged struct
                        let type_name = extract_type_from_method_call(func_name);
                        format!("({type_name}){{.tag = 1}}")
                    } else {
                        format_operand(&args[1], func, registry)
                    }
                } else { self_str.clone() };
                let _ = writeln!(out,
                    "        _{id} = ({self_str}.tag == 0) ? {self_str} : {other};",
                    id = dst_id.0);
            }
        }
        InlineMethod::OptionFlatten => {
            // flatten: Option[Option[T]] → Option[T]
            // If outer is None, return None. If inner is None, return None. Else return inner.
            if let Some(dst_id) = dst {
                let _ = writeln!(out,
                    "        _{id} = ({self_str}.tag == 0) ? {self_str}.data.Some._0 : \
                    ({{ __typeof__({self_str}.data.Some._0) __fl_none; __fl_none.tag = 1; __fl_none; }});",
                    id = dst_id.0);
            }
        }

        // -- Result combinators --
        InlineMethod::ResultMap => {
            // map(f): if Ok, return Ok(f(value)); else return Err as-is
            // Use destination type because map may change the Ok type
            if let Some(dst_id) = dst {
                let dst_c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let (closure_str, call_fn) = closure_call_info(&args, func, registry, type_overrides, 1);
                let _ = writeln!(out,
                    "        _{id} = ({{ {dst_c_type} __rm; \
                    if ({self_str}.tag == 0) {{ __rm.tag = 0; __rm.data.Ok._0 = {call_fn}(&{closure_str}, {self_str}.data.Ok._0); }} \
                    else {{ __rm.tag = 1; __rm.data.Error._0 = {self_str}.data.Error._0; }} __rm; }});",
                    id = dst_id.0);
            }
        }
        InlineMethod::ResultAndThen => {
            // and_then(f): if Ok, return f(value); else return Err as-is
            // Use destination type because and_then may change the Result type.
            // The closure may return a Result with a mismatched error type (GIR doesn't
            // constrain it), so we extract Ok/Error tags and Ok._0 from the closure result
            // and construct the destination Result manually, preserving the original error.
            if let Some(dst_id) = dst {
                let dst_c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let (closure_str, call_fn) = closure_call_info(&args, func, registry, type_overrides, 1);
                let _ = writeln!(out,
                    "        _{id} = ({{ {dst_c_type} __rat; \
                    if ({self_str}.tag == 0) {{ \
                    __auto_type __cr = {call_fn}(&{closure_str}, {self_str}.data.Ok._0); \
                    if (__cr.tag == 0) {{ __rat.tag = 0; __rat.data.Ok._0 = __cr.data.Ok._0; }} \
                    else {{ __rat.tag = 1; __rat.data.Error._0 = __cr.data.Error._0; }} \
                    }} \
                    else {{ __rat.tag = 1; __rat.data.Error._0 = {self_str}.data.Error._0; }} __rat; }});",
                    id = dst_id.0);
            }
        }
        InlineMethod::ResultMapErr => {
            // map_err(f): if Err, return Err(f(error)); else return Ok as-is
            // Use __auto_type + memcpy to handle GorgetString → Str mismatches
            if let Some(dst_id) = dst {
                let dst_c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let (closure_str, call_fn) = closure_call_info(&args, func, registry, type_overrides, 1);
                let _ = writeln!(out,
                    "        _{id} = ({{ {dst_c_type} __rme; \
                    if ({self_str}.tag == 0) {{ __rme.tag = 0; __rme.data.Ok._0 = {self_str}.data.Ok._0; }} \
                    else {{ __rme.tag = 1; __auto_type __ev = {call_fn}(&{closure_str}, {self_str}.data.Error._0); \
                    memcpy(&__rme.data.Error._0, &__ev, sizeof(__rme.data.Error._0)); }} __rme; }});",
                    id = dst_id.0);
            }
        }
        InlineMethod::ResultUnwrapErr => {
            // unwrap_err: assert is_err, return error value
            if let Some(dst_id) = dst {
                let _result_type = extract_type_from_method_call(func_name);
                let _ = writeln!(out,
                    "        if ({self_str}.tag == 0) {{ fprintf(stderr, \"gorget: panic: unwrap_err on Ok\\n\"); exit(1); }}");
                let _ = writeln!(out,
                    "        _{id} = {self_str}.data.Error._0;",
                    id = dst_id.0);
            }
        }
        InlineMethod::ResultOr => {
            // or(other): if Ok, return self; else return other
            if let Some(dst_id) = dst {
                let other = if args.len() > 1 { format_operand(&args[1], func, registry) } else { self_str.clone() };
                let _ = writeln!(out,
                    "        _{id} = ({self_str}.tag == 0) ? {self_str} : {other};",
                    id = dst_id.0);
            }
        }
        InlineMethod::ResultOrElse => {
            // or_else(f): if Ok, return self; else call f(err) which returns a new Result
            if let Some(dst_id) = dst {
                let (closure_str, call_fn) = closure_call_info(&args, func, registry, type_overrides, 1);
                let _ = writeln!(out,
                    "        _{id} = ({self_str}.tag == 0) ? {self_str} : {call_fn}(&{closure_str}, {self_str}.data.Error._0);",
                    id = dst_id.0);
            }
        }
        InlineMethod::ResultUnwrapOrElse => {
            // unwrap_or_else(f): if Ok, return value; else call f(err)
            if let Some(dst_id) = dst {
                let (closure_str, call_fn) = closure_call_info(&args, func, registry, type_overrides, 1);
                let _ = writeln!(out,
                    "        _{id} = ({self_str}.tag == 0) ? {self_str}.data.Ok._0 : {call_fn}(&{closure_str}, {self_str}.data.Error._0);",
                    id = dst_id.0);
            }
        }
    }
}

/// Extract closure operand name and call function name from args at given index.
fn closure_call_info(
    args: &[Operand],
    func: &Function,
    registry: &TypeRegistry,
    type_overrides: &std::collections::HashMap<usize, String>,
    arg_idx: usize,
) -> (String, String) {
    let closure_str = if args.len() > arg_idx {
        format_operand(&args[arg_idx], func, registry)
    } else {
        "/*no closure*/".to_string()
    };
    let closure_type = if args.len() > arg_idx {
        match &args[arg_idx] {
            Operand::Copy(p) | Operand::Move(p) =>
                effective_c_type(p.local.0 as usize, func, registry, type_overrides),
            _ => "void".to_string(),
        }
    } else {
        "void".to_string()
    };
    let call_fn = format!("{closure_type}__call");
    (closure_str, call_fn)
}

/// Poll-context version of closure_call_info — uses fmt_operand_poll for frame field access.
fn closure_call_info_poll(
    args: &[Operand],
    func: &Function,
    registry: &TypeRegistry,
    type_overrides: &std::collections::HashMap<usize, String>,
    arg_idx: usize,
) -> (String, String) {
    let closure_str = if args.len() > arg_idx {
        fmt_operand_poll(&args[arg_idx], func, registry)
    } else {
        "/*no closure*/".to_string()
    };
    let closure_type = if args.len() > arg_idx {
        match &args[arg_idx] {
            Operand::Copy(p) | Operand::Move(p) =>
                effective_c_type(p.local.0 as usize, func, registry, type_overrides),
            _ => "void".to_string(),
        }
    } else {
        "void".to_string()
    };
    let call_fn = format!("{closure_type}__call");
    (closure_str, call_fn)
}

/// Check if the self argument is a pointer type (borrow/reference).
fn is_self_pointer(args: &[Operand], func: &Function, registry: &TypeRegistry) -> bool {
    if let Some(arg0) = args.first() {
        if let Operand::Copy(place) | Operand::Move(place) = arg0 {
            let local_idx = place.local.0 as usize;
            if local_idx < func.locals.len() {
                let type_id = func.locals[local_idx].type_id;
                if let Some(GirType::Ptr(_)) = registry.get(type_id) {
                    return true;
                }
                // Also check C type format
                let c_type = format_type(type_id, registry);
                if c_type.ends_with('*') {
                    return true;
                }
            }
        }
    }
    false
}

fn has_float_arg_with_overrides(args: &[Operand], func: &Function, type_overrides: &std::collections::HashMap<usize, String>) -> bool {
    args.iter().any(|arg| match arg {
        Operand::Copy(place) | Operand::Move(place) => {
            let idx = place.local.0 as usize;
            let ir_float = idx < func.locals.len() && (func.locals[idx].type_id == F64_TYPE || func.locals[idx].type_id == F32_TYPE);
            let override_float = type_overrides.get(&idx).map(|t| t == "double" || t == "float").unwrap_or(false);
            ir_float || override_float
        }
        Operand::Constant(Constant::F64(_)) | Operand::Constant(Constant::F32(_)) => true,
        _ => false,
    })
}

/// Deref a self expression: if it's a pointer, dereference it; otherwise return as-is.
fn deref_self(self_str: &str, is_ptr: bool) -> String {
    if is_ptr {
        format!("(*{self_str})")
    } else {
        self_str.to_string()
    }
}

/// Format self for passing to a function: if it's a pointer, pass directly; otherwise take &.
fn addr_self(self_str: &str, is_ptr: bool) -> String {
    if is_ptr {
        self_str.to_string()
    } else {
        format!("&{self_str}")
    }
}

/// Emit a rewritten collection method call.
fn emit_collection_method_call(
    out: &mut String,
    rewrite: &CollectionMethodCall,
    dst: &Option<LocalId>,
    args: &[Operand],
    func: &Function,
    registry: &TypeRegistry,
    original_name: &str,
    type_overrides: &std::collections::HashMap<usize, String>,
) {
    let self_ptr = is_self_pointer(args, func, registry);
    let self_str = if !args.is_empty() {
        format_operand(&args[0], func, registry)
    } else {
        "/*no self*/".to_string()
    };
    let _self_deref = deref_self(&self_str, self_ptr);

    // Inline special methods
    if rewrite.runtime_fn == "__INLINE_STRING_TO_STR__" {
        let dereffed = deref_self(&self_str, self_ptr);
        if let Some(dst_id) = dst {
            let _ = writeln!(out, "        _{id} = (Str){{ .data = {dereffed}.data, .len = {dereffed}.len }};", id = dst_id.0);
        }
        return;
    }
    if rewrite.runtime_fn == "__INLINE_ARRAY_GET__" {
        // Vector.get(idx) → Option[T] with bounds check (with deep clone for droppable elements)
        let self_val = deref_self(&self_str, self_ptr);
        let idx_str = if args.len() > 1 {
            format_operand(&args[1], func, registry)
        } else {
            "0".to_string()
        };
        if let Some(dst_id) = dst {
            // Get the Option type from the destination local's type (matches GIR registration)
            let option_type = format_type(func.locals[dst_id.0 as usize].type_id, registry);
            // Element C type: strip "Option__" prefix, then resolve to C type
            let inner_type_str = option_type.strip_prefix("Option__").unwrap_or("");
            let elem_c_type: &str =
                if inner_type_str.starts_with("Vector__") || inner_type_str.starts_with("List__") {
                    "GorgetArray"
                } else if inner_type_str.starts_with("Dict__") || inner_type_str.starts_with("HashMap__") {
                    "GorgetMap"
                } else if inner_type_str.starts_with("Set__") || inner_type_str.starts_with("HashSet__") {
                    "GorgetSet"
                } else if !inner_type_str.is_empty() {
                    inner_type_str
                } else {
                    extract_collection_elem_type(original_name)
                };
            if elem_c_type == "GorgetArray" {
                // Collection element: deep clone via gorget_array_clone to prevent double-free
                let _ = writeln!(out, "        _{id} = ({{ int64_t __gi = {idx_str}; GorgetArray __gr_src = {self_val}; {option_type} __gr; \
                    if (__gi >= 0 && (size_t)__gi < __gr_src.len) {{ GorgetArray __elem = gorget_array_clone((GorgetArray*)gorget_array_get(&__gr_src, (size_t)__gi)); \
                    __gr = ({option_type}){{.tag = 0, .data.Some = {{__elem}}}}; }} \
                    else {{ __gr = ({option_type}){{.tag = 1}}; }} __gr; }});",
                    id = dst_id.0);
            } else if elem_c_type == "GorgetSet" {
                // Set element: deep clone via gorget_set_clone
                let _ = writeln!(out, "        _{id} = ({{ int64_t __gi = {idx_str}; GorgetArray __gr_src = {self_val}; {option_type} __gr; \
                    if (__gi >= 0 && (size_t)__gi < __gr_src.len) {{ GorgetSet __elem = gorget_set_clone((GorgetSet*)gorget_array_get(&__gr_src, (size_t)__gi)); \
                    __gr = ({option_type}){{.tag = 0, .data.Some = {{__elem}}}}; }} \
                    else {{ __gr = ({option_type}){{.tag = 1}}; }} __gr; }});",
                    id = dst_id.0);
            } else {
                // Struct or primitive element: collect field clone operations
                let mut clone_ops: Vec<String> = Vec::new();
                collect_clone_ops(elem_c_type, "__elem", &mut clone_ops, registry);
                if clone_ops.is_empty() && needs_drop_by_name(elem_c_type, registry) {
                    // Move-out: element type has drop glue but no cloneable fields (e.g. Task).
                    // Copy the value out and zero the source slot to prevent double-free.
                    let _ = writeln!(out, "        _{id} = ({{ int64_t __gi = {idx_str}; GorgetArray __gr_src = {self_val}; {option_type} __gr; \
                        if (__gi >= 0 && (size_t)__gi < __gr_src.len) {{ {elem_c_type} __elem = *({elem_c_type}*)gorget_array_get(&__gr_src, (size_t)__gi); \
                        memset(gorget_array_get(&__gr_src, (size_t)__gi), 0, sizeof({elem_c_type})); \
                        __gr = ({option_type}){{.tag = 0, .data.Some = {{__elem}}}}; }} \
                        else {{ __gr = ({option_type}){{.tag = 1}}; }} __gr; }});",
                        id = dst_id.0);
                } else if clone_ops.is_empty() {
                    // Simple shallow copy (no droppable fields)
                    let _ = writeln!(out, "        _{id} = ({{ int64_t __gi = {idx_str}; GorgetArray __gr_src = {self_val}; {option_type} __gr; \
                        if (__gi >= 0 && (size_t)__gi < __gr_src.len) {{ __gr = ({option_type}){{.tag = 0, .data.Some = {{*({elem_c_type}*)gorget_array_get(&__gr_src, (size_t)__gi)}}}}; }} \
                        else {{ __gr = ({option_type}){{.tag = 1}}; }} __gr; }});",
                        id = dst_id.0);
                } else {
                    // Deep clone: copy struct value, then clone GorgetArray fields
                    let clone_stmts = clone_ops.join(" ");
                    let _ = writeln!(out, "        _{id} = ({{ int64_t __gi = {idx_str}; GorgetArray __gr_src = {self_val}; {option_type} __gr; \
                        if (__gi >= 0 && (size_t)__gi < __gr_src.len) {{ {elem_c_type} __elem = *({elem_c_type}*)gorget_array_get(&__gr_src, (size_t)__gi); {clone_stmts} \
                        __gr = ({option_type}){{.tag = 0, .data.Some = {{__elem}}}}; }} \
                        else {{ __gr = ({option_type}){{.tag = 1}}; }} __gr; }});",
                        id = dst_id.0);
                }
            }
        }
        return;
    }
    if rewrite.runtime_fn == "__INLINE_MAP_GET__" {
        // Dict/HashMap.get(key) → Option[V] with NULL check
        let self_val = deref_self(&self_str, self_ptr);
        if let Some(dst_id) = dst {
            let option_type = if let Some(ov) = type_overrides.get(&(dst_id.0 as usize)) {
                ov.clone()
            } else {
                format_type(func.locals[dst_id.0 as usize].type_id, registry)
            };
            let elem_c_type = option_type.strip_prefix("Option__").unwrap_or("int64_t");
            // Build key argument — pass by pointer for gorget_map_get
            let key_arg = if args.len() > 1 {
                let val = format_operand(&args[1], func, registry);
                match &args[1] {
                    Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => {
                        format!("&{val}")
                    }
                    Operand::Constant(Constant::Str(s)) => {
                        format!("&(Str){{ .data = \"{}\", .len = {} }}",
                            escape_c_string(s), s.len())
                    }
                    _ => {
                        // Determine type for compound literal
                        let arg_type = match &args[1] {
                            Operand::Copy(p) | Operand::Move(p) => {
                                let idx = p.local.0 as usize;
                                if idx < func.locals.len() {
                                    format_type(func.locals[idx].type_id, registry)
                                } else { "int64_t".to_string() }
                            }
                            Operand::Constant(c) => match c {
                                Constant::I64(_) => "int64_t".to_string(),
                                Constant::F64(_) => "double".to_string(),
                                Constant::Bool(_) => "bool".to_string(),
                                _ => "int64_t".to_string(),
                            },
                        };
                        format!("&({arg_type}){{{val}}}")
                    }
                }
            } else {
                "NULL".to_string()
            };
            // gorget_map_get returns void* (NULL if key not found)
            let _ = writeln!(out,
                "        _{id} = ({{ void* __mv = gorget_map_get(&{self_val}, {key_arg}); \
                {option_type} __mr; if (__mv != NULL) {{ __mr = ({option_type}){{.tag = 0, .data.Some = {{*({elem_c_type}*)__mv}}}}; }} \
                else {{ __mr = ({option_type}){{.tag = 1}}; }} __mr; }});",
                id = dst_id.0);
        }
        return;
    }
    if rewrite.runtime_fn == "__INLINE_ARRAY_FIRST__" || rewrite.runtime_fn == "__INLINE_ARRAY_LAST__" {
        // Vector.first() / .last() → Option[T] with bounds check, same pattern as __INLINE_ARRAY_GET__
        let self_val = deref_self(&self_str, self_ptr);
        let is_last = rewrite.runtime_fn == "__INLINE_ARRAY_LAST__";
        if let Some(dst_id) = dst {
            let option_type = format_type(func.locals[dst_id.0 as usize].type_id, registry);
            let inner_type_str = option_type.strip_prefix("Option__").unwrap_or("");
            let elem_c_type: &str =
                if inner_type_str.starts_with("Vector__") || inner_type_str.starts_with("List__") {
                    "GorgetArray"
                } else if inner_type_str.starts_with("Dict__") || inner_type_str.starts_with("HashMap__") {
                    "GorgetMap"
                } else if inner_type_str.starts_with("Set__") || inner_type_str.starts_with("HashSet__") {
                    "GorgetSet"
                } else if !inner_type_str.is_empty() {
                    inner_type_str
                } else {
                    extract_collection_elem_type(original_name)
                };
            // index expression: 0 for first, (len-1) for last
            let idx_expr = if is_last {
                format!("(int64_t)({self_val}.len - 1)")
            } else {
                "0".to_string()
            };
            let mut clone_ops: Vec<String> = Vec::new();
            collect_clone_ops(elem_c_type, "__elem", &mut clone_ops, registry);
            if elem_c_type == "GorgetArray" {
                let _ = writeln!(out, "        _{id} = ({{ GorgetArray __gr_src = {self_val}; {option_type} __gr; \
                    if (__gr_src.len > 0) {{ GorgetArray __elem = gorget_array_clone((GorgetArray*)gorget_array_get(&__gr_src, (size_t)({idx_expr}))); \
                    __gr = ({option_type}){{.tag = 0, .data.Some = {{__elem}}}}; }} \
                    else {{ __gr = ({option_type}){{.tag = 1}}; }} __gr; }});",
                    id = dst_id.0);
            } else if clone_ops.is_empty() && needs_drop_by_name(elem_c_type, registry) {
                // Move-out for types with drop glue but no cloneable fields (e.g. Task)
                let _ = writeln!(out, "        _{id} = ({{ GorgetArray __gr_src = {self_val}; {option_type} __gr; \
                    if (__gr_src.len > 0) {{ {elem_c_type} __elem = *({elem_c_type}*)gorget_array_get(&__gr_src, (size_t)({idx_expr})); \
                    memset(gorget_array_get(&__gr_src, (size_t)({idx_expr})), 0, sizeof({elem_c_type})); \
                    __gr = ({option_type}){{.tag = 0, .data.Some = {{__elem}}}}; }} \
                    else {{ __gr = ({option_type}){{.tag = 1}}; }} __gr; }});",
                    id = dst_id.0);
            } else if clone_ops.is_empty() {
                let _ = writeln!(out, "        _{id} = ({{ GorgetArray __gr_src = {self_val}; {option_type} __gr; \
                    if (__gr_src.len > 0) {{ __gr = ({option_type}){{.tag = 0, .data.Some = {{*({elem_c_type}*)gorget_array_get(&__gr_src, (size_t)({idx_expr}))}}}}; }} \
                    else {{ __gr = ({option_type}){{.tag = 1}}; }} __gr; }});",
                    id = dst_id.0);
            } else {
                let clone_stmts = clone_ops.join(" ");
                let _ = writeln!(out, "        _{id} = ({{ GorgetArray __gr_src = {self_val}; {option_type} __gr; \
                    if (__gr_src.len > 0) {{ {elem_c_type} __elem = *({elem_c_type}*)gorget_array_get(&__gr_src, (size_t)({idx_expr})); {clone_stmts} \
                    __gr = ({option_type}){{.tag = 0, .data.Some = {{__elem}}}}; }} \
                    else {{ __gr = ({option_type}){{.tag = 1}}; }} __gr; }});",
                    id = dst_id.0);
            }
        }
        return;
    }
    if rewrite.runtime_fn == "__INLINE_ARRAY_REMOVE__" {
        // Vector.remove(idx) → Option[T]: bounds-check, extract element, shift array
        let arr_ref = addr_self(&self_str, self_ptr);
        let self_val = deref_self(&self_str, self_ptr);
        let idx_str = if args.len() > 1 {
            format_operand(&args[1], func, registry)
        } else {
            "0".to_string()
        };
        if let Some(dst_id) = dst {
            let option_type = format_type(func.locals[dst_id.0 as usize].type_id, registry);
            let inner_type_str = option_type.strip_prefix("Option__").unwrap_or("");
            let elem_c_type: &str =
                if inner_type_str.starts_with("Vector__") || inner_type_str.starts_with("List__") {
                    "GorgetArray"
                } else if inner_type_str.starts_with("Dict__") || inner_type_str.starts_with("HashMap__") {
                    "GorgetMap"
                } else if inner_type_str.starts_with("Set__") || inner_type_str.starts_with("HashSet__") {
                    "GorgetSet"
                } else if !inner_type_str.is_empty() {
                    inner_type_str
                } else {
                    extract_collection_elem_type(original_name)
                };
            // Bounds check, extract element, then remove (shift) — element must be read before removal
            let _ = writeln!(out, "        _{id} = ({{ int64_t __gi = {idx_str}; GorgetArray __gr_src = {self_val}; {option_type} __gr; \
                if (__gi >= 0 && (size_t)__gi < __gr_src.len) {{ {elem_c_type} __elem = *({elem_c_type}*)gorget_array_get(&__gr_src, (size_t)__gi); \
                gorget_array_remove({arr_ref}, (size_t)__gi); \
                __gr = ({option_type}){{.tag = 0, .data.Some = {{__elem}}}}; }} \
                else {{ __gr = ({option_type}){{.tag = 1}}; }} __gr; }});",
                id = dst_id.0);
        } else {
            let _ = writeln!(out, "        gorget_array_remove({arr_ref}, {idx_str});");
        }
        return;
    }
    if rewrite.runtime_fn == "__INLINE_STRING_CLEAR__" {
        if self_ptr {
            let _ = writeln!(out, "        {self_str}->len = 0;");
        } else {
            let _ = writeln!(out, "        {self_str}.len = 0;");
        }
        return;
    }

    // GorgetString .push() overload dispatch: choose runtime fn based on arg type
    if (rewrite.runtime_fn == "gorget_string_append_str"
        || rewrite.runtime_fn == "gorget_string_push_line")
        && args.len() > 1
    {
        let arg_c_type = match &args[1] {
            Operand::Copy(p) | Operand::Move(p) => {
                let idx = p.local.0 as usize;
                if idx < func.locals.len() {
                    format_type(func.locals[idx].type_id, registry)
                } else { String::new() }
            }
            Operand::Constant(c) => match c {
                Constant::I64(_) | Constant::I32(_) => "int64_t".to_string(),
                Constant::F64(_) => "double".to_string(),
                Constant::Bool(_) => "bool".to_string(),
                Constant::Str(_) => "Str".to_string(),
                _ => String::new(),
            },
        };
        let is_push_line = rewrite.runtime_fn == "gorget_string_push_line";
        let self_ref = addr_self(&self_str, self_ptr);
        let arg_val = format_operand(&args[1], func, registry);
        let suffix = if is_push_line { "_line" } else { "" };
        match arg_c_type.as_str() {
            "int64_t" | "int32_t" => {
                let _ = writeln!(out, "        gorget_string_push{suffix}_int({self_ref}, {arg_val});");
            }
            "double" => {
                let _ = writeln!(out, "        gorget_string_push{suffix}_float({self_ref}, {arg_val});");
            }
            "bool" => {
                let _ = writeln!(out, "        gorget_string_push{suffix}_bool({self_ref}, {arg_val});");
            }
            "uint32_t" => {
                let _ = writeln!(out, "        gorget_string_push{suffix}_char({self_ref}, {arg_val});");
            }
            "GorgetString" => {
                // String argument — coerce to Str view
                if is_push_line {
                    let _ = writeln!(out, "        gorget_string_append_str({self_ref}, (Str){{ .data = {arg_val}.data, .len = {arg_val}.len }});");
                    let _ = writeln!(out, "        gorget_string_push_byte({self_ref}, '\\n');");
                } else {
                    let _ = writeln!(out, "        gorget_string_append_str({self_ref}, (Str){{ .data = {arg_val}.data, .len = {arg_val}.len }});");
                }
            }
            _ => {
                // Default: Str argument
                let coerced = if let Operand::Constant(Constant::Str(s)) = &args[1] {
                    format!("gorget_str_from_literal(\"{}\", {})", escape_c_string(s), s.len())
                } else {
                    arg_val
                };
                if is_push_line {
                    let _ = writeln!(out, "        gorget_string_append_str({self_ref}, {coerced});");
                    let _ = writeln!(out, "        gorget_string_push_byte({self_ref}, '\\n');");
                } else {
                    let _ = writeln!(out, "        gorget_string_append_str({self_ref}, {coerced});");
                }
            }
        }
        return;
    }

    // Field access (len, is_empty) — no function call needed
    if let Some(field) = rewrite.field_access {
        if let Some(dst_id) = dst {
            let _c_type = format_local_type(func, dst_id.0 as usize, registry);
            let access = if self_ptr { "->" } else { "." };
            let _ = writeln!(out, "        _{id} = {self_str}{access}{field};", id = dst_id.0);
        }
        return;
    }

    // Build argument list for the runtime function
    let mut call_args = Vec::new();
    // Str and uint8 methods take self by value; dereference if self is a pointer
    let is_str_method = rewrite.runtime_fn.starts_with("gorget_str_");
    let is_uint8_method = rewrite.runtime_fn.starts_with("gorget_uint8_");
    if is_uint8_method {
        let dereffed = deref_self(&self_str, self_ptr);
        call_args.push(dereffed);
    } else if is_str_method {
        // Check if self is a GorgetString — if so, coerce to Str
        let self_c_type = if !args.is_empty() {
            match &args[0] {
                Operand::Copy(p) | Operand::Move(p) => {
                    let idx = p.local.0 as usize;
                    if idx < func.locals.len() {
                        format_type(func.locals[idx].type_id, registry)
                    } else { String::new() }
                }
                _ => String::new(),
            }
        } else { String::new() };
        let dereffed = deref_self(&self_str, self_ptr);
        if self_c_type == "GorgetString" || self_c_type.contains("GorgetString") {
            // GorgetString → Str coercion: (Str){.data = gs.data, .len = gs.len}
            call_args.push(format!("(Str){{ .data = {dereffed}.data, .len = {dereffed}.len }}"));
        } else {
            call_args.push(dereffed);
        }
    } else {
        // Other collections: pass self by pointer (take & if not already a pointer)
        call_args.push(addr_self(&self_str, self_ptr));
    }

    // For set/insert: first extra arg is index (plain value), second is element (by pointer)
    let is_index_plus_elem = rewrite.runtime_fn == "gorget_array_set"
        || rewrite.runtime_fn == "gorget_array_insert";
    // slice takes self by ptr but remaining args (start, end) as plain int64_t
    let scalar_args = rewrite.runtime_fn == "gorget_array_slice";

    // Remaining args
    for (arg_idx, arg) in args.iter().skip(1).enumerate() {
        let val = format_operand(arg, func, registry);
        // For set/insert: arg[0] = index (value), arg[1] = element (pointer)
        let should_ptr = if scalar_args { false } else if is_index_plus_elem { arg_idx == 1 } else { rewrite.pass_by_ptr };
        if should_ptr {
            // For void*-generic functions, pass element by pointer.
            // Use compound literal for constants/rvalues: &(type){val}
            match arg {
                Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
                    // Local variable — can take address directly
                    call_args.push(format!("&{val}"));
                }
                _ => {
                    // Constant or projected place — use compound literal
                    let arg_type = match arg {
                        Operand::Copy(p) | Operand::Move(p) => {
                            let idx = p.local.0 as usize;
                            if idx < func.locals.len() {
                                format_type(func.locals[idx].type_id, registry)
                            } else {
                                "int64_t".to_string()
                            }
                        }
                        Operand::Constant(c) => match c {
                            Constant::I64(_) => "int64_t".to_string(),
                            Constant::I32(_) => "int32_t".to_string(),
                            Constant::F64(_) => "double".to_string(),
                            Constant::Bool(_) => "bool".to_string(),
                            Constant::Str(_) => "Str".to_string(),
                            _ => "int64_t".to_string(),
                        },
                    };
                    // For Str, use gorget_str_from_literal to set both .data and .len
                    if let Operand::Constant(Constant::Str(s)) = arg {
                        call_args.push(format!("&(Str){{ .data = \"{}\", .len = {} }}",
                            escape_c_string(s), s.len()));
                    } else if matches!(arg, Operand::Constant(Constant::Null)) {
                        // Null pushed into a collection — infer element type from collection name
                        // e.g. Vector__Option__int64_t__push → element is Option__int64_t
                        if let Some(elem_type) = collection_elem_type_from_name(original_name) {
                            if elem_type.starts_with("Option__") {
                                // None value: emit tagged struct with .tag = 1 (None tag)
                                call_args.push(format!("&({elem_type}){{.tag = 1}}"));
                            } else {
                                call_args.push(format!("&({elem_type}){{0}}"));
                            }
                        } else {
                            call_args.push(format!("&({arg_type}){{{val}}}"));
                        }
                    } else {
                        call_args.push(format!("&({arg_type}){{{val}}}"));
                    }
                }
            }
        } else {
            // Wrap string literal arguments in gorget_str_from_literal (needed for all methods that take Str)
            if let Operand::Constant(Constant::Str(s)) = arg {
                call_args.push(format!("gorget_str_from_literal(\"{}\", {})",
                    escape_c_string(s), s.len()));
            } else if is_str_method {
                // gorget_str_* functions expect Str args — coerce GorgetString if needed
                let arg_c_type = match arg {
                    Operand::Copy(p) | Operand::Move(p) => {
                        let idx = p.local.0 as usize;
                        if idx < func.locals.len() {
                            format_type(func.locals[idx].type_id, registry)
                        } else { String::new() }
                    }
                    _ => String::new(),
                };
                if arg_c_type == "GorgetString" || arg_c_type.contains("GorgetString") {
                    call_args.push(format!("(Str){{ .data = {val}.data, .len = {val}.len }}"));
                } else {
                    call_args.push(val);
                }
            } else {
                call_args.push(val);
            }
        }
    }
    // For contains/index_of: append sizeof(element) derived from the original func name
    if rewrite.needs_elem_size {
        let elem_c_type = extract_collection_elem_type(original_name);
        call_args.push(format!("sizeof({elem_c_type})"));
    }

    let args_str = call_args.join(", ");

    // Choose runtime function — use ws_variant if no extra args beyond self
    let runtime_fn = if let Some(ws_fn) = rewrite.ws_variant {
        // ws_variant is for strip/lstrip/rstrip: use ws version when only self arg
        if args.len() <= 1 { ws_fn } else { rewrite.runtime_fn }
    } else {
        rewrite.runtime_fn
    };

    // Determine if this method returns a scalar (len, contains, etc.) vs a collection element.
    // Scalar methods should never be Option-wrapped.
    let method_for_wrap = extract_trailing_method(original_name, "");
    let is_scalar_method = matches!(method_for_wrap,
        "len" | "count" | "capacity" | "is_empty" | "contains" | "has"
        | "remove" | "binary_search" | "hash" | "index_of");

    if rewrite.has_return {
        if let Some(dst_id) = dst {
            let c_type = if is_scalar_method {
                // Scalar methods: use the IR type directly, don't infer from collection name
                effective_c_type(dst_id.0 as usize, func, registry, type_overrides)
            } else {
                collection_element_c_type(func, dst_id.0 as usize, registry, original_name, type_overrides)
            };
            if rewrite.needs_deref_cast {
                // get() returns void* — cast and deref: *(T*)gorget_array_get(...)
                let _ = writeln!(
                    out,
                    "        _{id} = *({c_type}*){runtime_fn}({args_str});",
                    id = dst_id.0,
                );
            } else if c_type.starts_with("Option__") {
                // Option wrapping for runtime functions that return raw int (index_of, find, etc.)
                let _ = writeln!(out,
                    "        _{id} = ({{ __typeof__(_{id}.data.Some._0) __raw = {runtime_fn}({args_str}); \
                    {c_type} __opt; if (__raw >= 0) {{ __opt.tag = 0; __opt.data.Some._0 = __raw; }} \
                    else {{ __opt.tag = 1; }} __opt; }});",
                    id = dst_id.0);
            } else {
                let _ = writeln!(
                    out,
                    "        _{id} = {runtime_fn}({args_str});",
                    id = dst_id.0,
                );
            }
        } else {
            let _ = writeln!(out, "        {runtime_fn}({args_str});");
        }
    } else {
        // Pre-drop for gorget_array_set: drop old element before overwriting
        if runtime_fn == "gorget_array_set" && call_args.len() >= 2 {
            let elem_name = extract_collection_elem_type_full(original_name);
            if needs_drop_by_name(&elem_name, registry) {
                let arr_ptr = &call_args[0].clone();
                let idx_arg = &call_args[1].clone();
                let elem_c_type = gir_to_c_type(&elem_name).to_string();
                let _ = writeln!(out, "        {{");
                let _ = writeln!(
                    out,
                    "            {elem_c_type}* __old = ({elem_c_type}*)gorget_array_get({arr_ptr}, {idx_arg});"
                );
                // "__old" is a fresh pointer, not inside any outer loop → depth 0
                emit_drop_for_type_via_ptr(out, "__old", &elem_name, registry, "            ", 0);
                let _ = writeln!(out, "        }}");
            }
        }
        let _ = writeln!(out, "        {runtime_fn}({args_str});");
        // Post-call zero: after consuming an element/value into a collection, zero the source
        // local to prevent double-free. The collection owns the data now; the source's
        // DropIfAlive/Drop will be a no-op once its data pointer is set to NULL.
        // For push/set_add: element is args[1].
        // For insert/set/map_put: element is args[2] (args[1] is index or key).
        let consuming_arg_idx = match runtime_fn {
            "gorget_array_push" | "gorget_set_add" => Some(1usize),
            "gorget_array_insert" | "gorget_array_set" | "gorget_map_put" => Some(2usize),
            _ => None,
        };
        if let Some(idx) = consuming_arg_idx {
            if let Some(arg) = args.get(idx) {
                if let Operand::Copy(place) | Operand::Move(place) = arg {
                    if place.projections.is_empty() {
                        let local_id = place.local.0 as usize;
                        if local_id < func.locals.len() {
                            let local_type = func.locals[local_id].type_id;
                            if let Some(gir_name) = gir_type_name(local_type, registry) {
                                if needs_drop_by_name(&gir_name, registry) {
                                    let c_type = gir_to_c_type(&gir_name);
                                    let _ = writeln!(
                                        out,
                                        "        memset(&_{local_id}, 0, sizeof({c_type}));"
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Look up a type's DropStrategy from the registry.
fn lookup_drop_strategy(type_id: TypeId, registry: &TypeRegistry) -> DropStrategy {
    if let Some(GirType::Named(name)) = registry.get(type_id) {
        if let Some(type_def) = registry.get_type_def(name) {
            return type_def.metadata.drop_strategy.clone();
        }
    }
    DropStrategy::None
}

/// Generate the `__gorget_cleanup_push(...)` line for a given local in a test function.
/// Returns None if the type doesn't need cleanup stack registration.
fn test_cleanup_push_code(
    local_idx: u32,
    func: &Function,
    registry: &TypeRegistry,
    type_overrides: &std::collections::HashMap<usize, String>,
) -> Option<String> {
    let id = local_idx;
    let c_type = effective_c_type(local_idx as usize, func, registry, type_overrides);

    // Box types: push raw pointer (no address-of)
    if c_type.starts_with("Box__") {
        let inner = &c_type[5..];
        let is_trait_box = registry.get_type_def(&format!("{inner}_TraitObj")).is_some();
        if is_trait_box {
            return Some(format!("        __gorget_cleanup_push(free, (void*)_{id}.data);\n"));
        } else {
            return Some(format!("        __gorget_cleanup_push(free, (void*)_{id});\n"));
        }
    }

    // GorgetString
    if c_type == "GorgetString" {
        return Some(format!("        __gorget_cleanup_push((__gorget_cleanup_fn)gorget_string_free, (void*)&_{id});\n"));
    }

    // Vector/Array type
    if let Some(type_id) = func.locals.get(local_idx as usize).map(|l| l.type_id) {
        if let Some(gir_name) = gir_type_name(type_id, registry) {
            if extract_vector_elem_name(&gir_name).is_some() {
                return Some(format!("        __gorget_cleanup_push((__gorget_cleanup_fn)gorget_array_free, (void*)&_{id});\n"));
            }
        }
    }

    // Named struct: use its drop strategy
    if let Some(local) = func.locals.get(local_idx as usize) {
        match lookup_drop_strategy(local.type_id, registry) {
            DropStrategy::Custom(fn_name) => {
                return Some(format!("        __gorget_cleanup_push((__gorget_cleanup_fn){fn_name}, (void*)&_{id});\n"));
            }
            DropStrategy::Trivial(fn_name) => {
                return Some(format!("        __gorget_cleanup_push((__gorget_cleanup_fn){fn_name}, (void*)&_{id});\n"));
            }
            _ => {}
        }
    }

    None
}

/// Emit drops for individual fields of a struct that have their own drop strategies.
/// Used after Custom drops (to clean up droppable fields) and for Recursive drops
/// (structs with no Drop impl but containing droppable fields).
fn emit_field_drops(
    out: &mut String,
    parent_expr: &str,
    type_id: TypeId,
    registry: &TypeRegistry,
    indent: &str,
    depth: usize,
) {
    let type_name = if let Some(GirType::Named(name)) = registry.get(type_id) {
        name.clone()
    } else {
        return;
    };

    let type_def = if let Some(td) = registry.get_type_def(&type_name) {
        td.clone()
    } else {
        return;
    };

    if let TypeDefKind::Struct(ref sdef) = type_def.kind {
        for field in &sdef.fields {
            let Some(field_type_name) = gir_type_name(field.type_id, registry) else { continue };
            if needs_drop_by_name(&field_type_name, registry) {
                // Take address of struct field, then use pointer-based emit to handle
                // all collection and custom-drop types uniformly (including Vector prefix fast path).
                emit_drop_for_type_via_ptr(
                    out,
                    &format!("&{parent_expr}.{}", field.name),
                    &field_type_name,
                    registry,
                    indent,
                    depth,
                );
            }
        }
    }
}

/// Get the GIR type name for a TypeId (e.g., "Vector__Tracked" instead of "GorgetArray").
fn gir_type_name(type_id: TypeId, registry: &TypeRegistry) -> Option<String> {
    if let Some(GirType::Named(name)) = registry.get(type_id) {
        Some(name.clone())
    } else {
        None
    }
}

/// Check whether a GIR type name requires drop/cleanup.
fn needs_drop_by_name(type_name: &str, registry: &TypeRegistry) -> bool {
    if type_name.starts_with("Vector__")
        || type_name.starts_with("List__")
        || type_name.starts_with("Dict__")
        || type_name.starts_with("HashMap__")
        || type_name.starts_with("Set__")
        || type_name.starts_with("HashSet__")
        || matches!(type_name, "GorgetArray" | "GorgetMap" | "GorgetSet" | "GorgetString")
    {
        return true;
    }
    registry
        .get_type_def(type_name)
        .map(|td| !matches!(td.metadata.drop_strategy, DropStrategy::None))
        .unwrap_or(false)
}

/// Map a GIR type name to its C type name (for casts and declarations).
fn gir_to_c_type(type_name: &str) -> &str {
    if type_name.starts_with("Vector__") || type_name.starts_with("List__") || type_name == "GorgetArray" {
        "GorgetArray"
    } else if type_name.starts_with("Dict__") || type_name.starts_with("HashMap__") {
        "GorgetMap"
    } else if type_name.starts_with("Set__") || type_name.starts_with("HashSet__") || type_name == "GorgetSet" {
        "GorgetSet"
    } else {
        type_name
    }
}

/// Extract the element GIR type name from a Vector/List type ("Vector__X" → Some("X")).
fn extract_vector_elem_name(type_name: &str) -> Option<&str> {
    for prefix in ["Vector__", "List__", "Array__"] {
        if let Some(rest) = type_name.strip_prefix(prefix) {
            if !rest.is_empty() {
                return Some(rest);
            }
        }
    }
    None
}

/// Emit drop code for a value accessed via pointer `ptr_expr` of GIR type `type_name`.
/// For Vector types: iterate elements + gorget_array_free.
/// For structs: dispatch on drop strategy.
fn emit_drop_for_type_via_ptr(
    out: &mut String,
    ptr_expr: &str,
    type_name: &str,
    registry: &TypeRegistry,
    indent: &str,
    depth: usize,
) {
    // Vector/List type: per-element drops + free
    if let Some(elem_name) = extract_vector_elem_name(type_name) {
        if needs_drop_by_name(elem_name, registry) {
            let elem_c_type = gir_to_c_type(elem_name);
            // Use numbered loop variables to avoid shadowing in nested drop loops.
            // ({ptr_expr}) parenthesizes address-of exprs like "&struct.field" so
            // "(&struct.field)->len" is valid C (vs "&struct.field->len" which is not).
            let di = format!("__di{depth}");
            let de = format!("__de{depth}");
            let _ = writeln!(out, "{indent}for (size_t {di} = 0; {di} < ({ptr_expr})->len; {di}++) {{");
            let _ = writeln!(
                out,
                "{indent}    {elem_c_type}* {de} = ({elem_c_type}*)gorget_array_get(({ptr_expr}), {di});"
            );
            emit_drop_for_type_via_ptr(out, &de, elem_name, registry, &format!("{indent}    "), depth + 1);
            let _ = writeln!(out, "{indent}}}");
        }
        let _ = writeln!(out, "{indent}gorget_array_free({ptr_expr});");
        return;
    }
    // Struct/enum: dispatch on drop strategy
    let Some(td) = registry.get_type_def(type_name) else { return };
    match td.metadata.drop_strategy.clone() {
        DropStrategy::None => {}
        DropStrategy::Trivial(fn_name) => {
            let _ = writeln!(out, "{indent}{fn_name}({ptr_expr});");
        }
        DropStrategy::Custom(fn_name) => {
            let _ = writeln!(out, "{indent}{fn_name}({ptr_expr});");
            emit_field_drops_via_ptr(out, ptr_expr, type_name, registry, indent, depth);
        }
        DropStrategy::Recursive => {
            emit_field_drops_via_ptr(out, ptr_expr, type_name, registry, indent, depth);
        }
    }
}

/// Emit drops for struct fields accessible via pointer (`->` notation).
fn emit_field_drops_via_ptr(
    out: &mut String,
    ptr_expr: &str,
    type_name: &str,
    registry: &TypeRegistry,
    indent: &str,
    depth: usize,
) {
    let Some(td) = registry.get_type_def(type_name) else { return };
    let kind = td.kind.clone();
    let TypeDefKind::Struct(ref sdef) = kind else { return };
    for field in &sdef.fields {
        let Some(field_type_name) = gir_type_name(field.type_id, registry) else { continue };
        if needs_drop_by_name(&field_type_name, registry) {
            // Parenthesize ptr_expr to avoid "&&expr->field" when ptr_expr itself starts with &
            emit_drop_for_type_via_ptr(
                out,
                &format!("&({ptr_expr})->{}", field.name),
                &field_type_name,
                registry,
                indent,
                depth,
            );
        }
    }
}

/// Collect clone operations for struct fields that are GorgetArrays (or contain them).
/// `path` is the C expression for the value (dot-notation), e.g., "__elem".
/// Pushes "path.field = gorget_array_clone(&path.field);" or recurses for nested structs.
fn collect_clone_ops(type_name: &str, path: &str, ops: &mut Vec<String>, registry: &TypeRegistry) {
    let Some(td) = registry.get_type_def(type_name) else { return };
    let kind = td.kind.clone();
    let TypeDefKind::Struct(ref sdef) = kind else { return };
    for field in &sdef.fields {
        let Some(field_type_name) = gir_type_name(field.type_id, registry) else { continue };
        let field_path = format!("{path}.{}", field.name);
        if field_type_name.starts_with("Vector__")
            || field_type_name.starts_with("List__")
            || field_type_name == "GorgetArray"
        {
            ops.push(format!("{field_path} = gorget_array_clone(&{field_path});"));
        } else if needs_drop_by_name(&field_type_name, registry) {
            collect_clone_ops(&field_type_name, &field_path, ops, registry);
        }
    }
}

/// Like extract_collection_elem_type but preserves full GIR names for nested collections.
/// e.g., "Vector__Vector__Container__set" → "Vector__Container"
fn extract_collection_elem_type_full(name: &str) -> String {
    for prefix in &["Vector__", "List__", "Array__", "Set__", "HashSet__"] {
        if let Some(rest) = name.strip_prefix(*prefix) {
            if rest.is_empty() {
                return "int64_t".to_string();
            }
            // Strip method suffix: rightmost "__" where suffix is all ASCII lowercase letters
            if let Some(pos) = rest.rfind("__") {
                let suffix = &rest[pos + 2..];
                if !suffix.is_empty() && suffix.chars().all(|c| c.is_ascii_lowercase()) {
                    let elem = &rest[..pos];
                    if !elem.is_empty() {
                        return elem.to_string();
                    }
                }
            }
            return rest.to_string();
        }
    }
    "int64_t".to_string()
}

/// Format a constant as a C literal.
fn format_constant(constant: &Constant, _func: &Function, _registry: &TypeRegistry) -> String {
    match constant {
        Constant::Bool(b) => {
            if *b { "true".to_string() } else { "false".to_string() }
        }
        Constant::I8(n) => format!("(int8_t){n}"),
        Constant::I16(n) => format!("(int16_t){n}"),
        Constant::I32(n) => {
            if *n == i32::MIN {
                "(-2147483647 - 1)".to_string()
            } else {
                format!("{n}")
            }
        }
        Constant::I64(n) => {
            // INT64_MIN (-9223372036854775808) can't be written as a literal in C
            // because 9223372036854775808 overflows long long before negation.
            if *n == i64::MIN {
                "(-9223372036854775807LL - 1LL)".to_string()
            } else {
                format!("{n}LL")
            }
        }
        Constant::U8(n) => format!("(uint8_t){n}"),
        Constant::U16(n) => format!("(uint16_t){n}"),
        Constant::U32(n) => format!("{n}u"),
        Constant::U64(n) => format!("{n}ULL"),
        Constant::F32(n) => format_float(*n as f64),
        Constant::F64(n) => format_float(*n),
        Constant::Str(s) => format!("\"{}\"", escape_c_string(s)),
        Constant::Null => "NULL".to_string(),
        Constant::Unit => "0 /* unit */".to_string(),
        Constant::SizeOf(type_id) => {
            let c_type = format_type(*type_id, _registry);
            format!("sizeof({c_type})")
        }
        Constant::FuncRef(name) => {
            // Named function reference — emit as a [adapter_fn_ptr, NULL] pair for Callable dispatch.
            // The adapter has the (void* env, params...) ABI matching closure __call functions.
            let c_name = mangle_name(name);
            format!("(void*)(void*[2]){{(void*)__adapt_{c_name}, NULL}}")
        }
        Constant::GlobalRef(name) => {
            // Reference to a module-level static variable — emit the variable name directly.
            name.clone()
        }
        Constant::GlobalRefPtr(name) => {
            // Pointer to a module-level static variable — emit &name.
            format!("&{name}")
        }
    }
}

/// Format a float with a guaranteed decimal point.
fn format_float(n: f64) -> String {
    if n.is_infinite() {
        return if n > 0.0 { "(1.0/0.0)".to_string() } else { "(-1.0/0.0)".to_string() };
    }
    if n.is_nan() {
        return "(0.0/0.0)".to_string();
    }
    let s = format!("{n}");
    if s.contains('.') || s.contains('e') || s.contains('E') {
        s
    } else {
        format!("{s}.0")
    }
}

/// Escape a string for C string literal context.
fn escape_c_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\0' => out.push_str("\\0"),
            c => out.push(c),
        }
    }
    out
}

/// Format function call arguments.
fn format_args(args: &[Operand], func: &Function, registry: &TypeRegistry) -> String {
    let empty = std::collections::HashMap::new();
    format_args_inner(args, func, registry, FormatArgsMode::Normal, &empty)
}

/// Format function call arguments for printf (needs special Str→.data handling).
fn format_printf_args(args: &[Operand], func: &Function, registry: &TypeRegistry, type_overrides: &std::collections::HashMap<usize, String>) -> String {
    format_args_inner(args, func, registry, FormatArgsMode::Printf, type_overrides)
}

/// Format function call arguments for gorget_str_* (wraps string literals in Str).
fn format_str_fn_args(args: &[Operand], func: &Function, registry: &TypeRegistry, type_overrides: &std::collections::HashMap<usize, String>) -> String {
    format_args_inner(args, func, registry, FormatArgsMode::StrFn, type_overrides)
}

/// Format function call arguments for C functions that take const char* (extracts .data from Str).
fn format_cstr_fn_args(args: &[Operand], func: &Function, registry: &TypeRegistry) -> String {
    let empty = std::collections::HashMap::new();
    format_args_inner(args, func, registry, FormatArgsMode::CstrFn, &empty)
}

#[derive(PartialEq)]
enum FormatArgsMode { Normal, Printf, StrFn, CstrFn }

fn format_args_inner(args: &[Operand], func: &Function, registry: &TypeRegistry, mode: FormatArgsMode, type_overrides: &std::collections::HashMap<usize, String>) -> String {
    // Collect which argument positions have been overridden to float/str types
    // so we can patch the format string
    let mut float_override_positions: Vec<usize> = Vec::new();
    let mut str_override_positions: Vec<usize> = Vec::new();
    let mut parts = Vec::new();
    for (i, arg) in args.iter().enumerate() {
        let mut arg_str = format_operand(arg, func, registry);

        match arg {
            Operand::Copy(place) | Operand::Move(place) => {
                let local_idx = place.local.0 as usize;
                if local_idx < func.locals.len() {
                    let local_type = func.locals[local_idx].type_id;
                    // Check if this local has a type override to float
                    let is_float_override = type_overrides.get(&local_idx)
                        .map(|t| t == "double" || t == "float")
                        .unwrap_or(false);
                    let is_float_ir = local_type == F64_TYPE || local_type == F32_TYPE;
                    // Bool args in non-first position: "true"/"false" conversion
                    if i > 0 && local_type == BOOL_TYPE {
                        arg_str = format!("({arg_str} ? \"true\" : \"false\")");
                    }
                    // Cast int64_t/uint64_t args to long long for %lld/%llu printf
                    // BUT skip if the local has been overridden to double/float or to Str/GorgetString
                    let is_str_override = type_overrides.get(&local_idx)
                        .map(|t| t == "Str" || t == "GorgetString")
                        .unwrap_or(false);
                    if mode == FormatArgsMode::Printf && i > 0 && !is_float_override && !is_float_ir && !is_str_override {
                        if local_type == I64_TYPE || local_type == I32_TYPE || local_type == I16_TYPE || local_type == I8_TYPE {
                            arg_str = format!("(long long){arg_str}");
                        } else if local_type == U64_TYPE || local_type == U32_TYPE || local_type == U16_TYPE || local_type == U8_TYPE {
                            arg_str = format!("(unsigned long long){arg_str}");
                        }
                    }
                    if mode == FormatArgsMode::Printf && i > 0 && (is_float_override || is_float_ir) {
                        float_override_positions.push(i);
                    }
                    // int/float → Str coercion for gorget_str_* functions (string interpolation)
                    // Skip if type_overrides says this local is already Str (e.g. cstr-returning fn)
                    if mode == FormatArgsMode::StrFn {
                        let override_is_str = type_overrides.get(&local_idx)
                            .map_or(false, |t| t == "Str" || t == "GorgetString");
                        if !override_is_str {
                            let is_int = local_type == I64_TYPE || local_type == I32_TYPE
                                || local_type == I16_TYPE || local_type == I8_TYPE
                                || local_type == U64_TYPE || local_type == U32_TYPE
                                || local_type == U16_TYPE || local_type == U8_TYPE;
                            let is_float = local_type == F64_TYPE || local_type == F32_TYPE;
                            if is_int {
                                arg_str = format!("gorget_str_from_cstr(gorget_int_to_str({arg_str}))");
                            } else if is_float {
                                arg_str = format!("gorget_str_from_cstr(gorget_float_to_str({arg_str}))");
                            } else if local_type == BOOL_TYPE {
                                arg_str = format!("gorget_str_from_cstr(gorget_bool_to_str({arg_str}))");
                            }
                        }
                    }
                    // Dereference Str*/const Str* pointers for str/cstr functions
                    // gorget_str_* functions expect Str by value, not Str*
                    if mode == FormatArgsMode::StrFn || mode == FormatArgsMode::CstrFn {
                        let is_str_ptr = match registry.get(local_type) {
                            Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => {
                                matches!(registry.get(*inner), Some(GirType::Named(n)) if n == "Str" || n == "GorgetString")
                                    || format_type(*inner, registry) == "Str"
                            }
                            _ => false,
                        };
                        if is_str_ptr {
                            arg_str = format!("(*{arg_str})");
                        }
                    }
                    // Determine effective type name: type_override > GIR Named type
                    let eff_type_name: Option<&str> = type_overrides.get(&local_idx)
                        .map(|s| s.as_str())
                        .or_else(|| {
                            if let Some(GirType::Named(name)) = registry.get(local_type) {
                                Some(name.as_str())
                            } else { None }
                        });
                    if let Some(type_name) = eff_type_name {
                        match type_name {
                            // Str args in printf: expand to (int)len, data for %.*s
                            "Str" if mode == FormatArgsMode::Printf && i > 0 => {
                                str_override_positions.push(i);
                                parts.push(format!("(int){arg_str}.len"));
                                arg_str = format!("{arg_str}.data");
                            }
                            // GorgetString args in printf: expand to (int)len, data for %.*s
                            "GorgetString" if mode == FormatArgsMode::Printf && i > 0 => {
                                str_override_positions.push(i);
                                parts.push(format!("(int){arg_str}.len"));
                                arg_str = format!("{arg_str}.data");
                            }
                            // GorgetString → Str coercion for gorget_str_* functions
                            "GorgetString" if mode == FormatArgsMode::StrFn => {
                                arg_str = format!("(Str){{ .data = {arg_str}.data, .len = {arg_str}.len }}");
                            }
                            // Str/GorgetString → const char* for C functions
                            "Str" if mode == FormatArgsMode::CstrFn => {
                                arg_str = format!("gorget_str_to_cstr({arg_str})");
                            }
                            "GorgetString" if mode == FormatArgsMode::CstrFn => {
                                arg_str = format!("{arg_str}.data");
                            }
                            _ => {}
                        }
                    }
                }
            }
            Operand::Constant(Constant::Bool(b)) => {
                // Bool constants in printf: must use string literal not C `true`/`false`
                if i > 0 && (mode == FormatArgsMode::Printf || mode == FormatArgsMode::Normal) {
                    arg_str = if *b { "\"true\"".to_string() } else { "\"false\"".to_string() };
                }
            }
            Operand::Constant(Constant::Str(s)) => {
                if mode == FormatArgsMode::CstrFn {
                    // CstrFn mode: emit raw C string literal (const char*)
                    let escaped = escape_c_string(s);
                    arg_str = format!("\"{escaped}\"");
                } else if mode == FormatArgsMode::StrFn {
                    // StrFn mode: wrap string literals in gorget_str_from_literal
                    let escaped = escape_c_string(s);
                    arg_str = format!("gorget_str_from_literal(\"{escaped}\", {})", s.len());
                }
            }
            Operand::Constant(Constant::GlobalRef(_)) => {
                // GlobalRef args handled at CstrFn call sites where module is available
            }
            _ => {}
        }

        parts.push(arg_str);
    }
    // Patch format string to replace %lld/%s with correct specifiers for overridden arguments
    let needs_patching = !float_override_positions.is_empty() || !str_override_positions.is_empty();
    if needs_patching && !parts.is_empty() {
        let fmt = &parts[0];
        // The format string is a C string literal like "%lld\n" or "\"%lld\\n\""
        let new_fmt = fmt.clone();
        // Each %lld in the format string corresponds to a printf argument position
        // Replace the Nth %lld where N matches float_override_positions
        let mut spec_idx = 0usize; // 1-based arg position counter
        let mut result = String::new();
        let mut chars = new_fmt.chars().peekable();
        while let Some(ch) = chars.next() {
            if ch == '%' {
                // Collect the full printf format specifier: %[flags][width][.precision][length]conversion
                let mut spec = String::from('%');
                while let Some(&next) = chars.peek() {
                    spec.push(next);
                    chars.next();
                    // Length modifiers (h, hh, l, ll, L, z, j, t, q) are NOT terminal
                    // Conversion specifiers (d,i,u,f,e,g,x,o,s,c,p,n,a,%) ARE terminal
                    let is_conversion = matches!(next, 'd' | 'i' | 'u' | 'f' | 'e' | 'g' | 'x' | 'X' | 'o' | 's' | 'c' | 'p' | 'n' | 'a' | 'A' | '%');
                    if is_conversion {
                        break;
                    }
                    // Also break on '*' for width/precision
                    if next == '*' {
                        break;
                    }
                }
                if spec == "%%" {
                    result.push_str(&spec);
                } else if spec.ends_with('*') {
                    // %.*s — consumes two args (len, data), skip both
                    // Read the actual type char (e.g., 's')
                    if let Some(&type_char) = chars.peek() {
                        spec.push(type_char);
                        chars.next();
                    }
                    spec_idx += 1;
                    result.push_str(&spec);
                } else {
                    spec_idx += 1;
                    if str_override_positions.contains(&spec_idx) && (spec.contains("lld") || spec.contains("llu") || spec == "%s") {
                        result.push_str("%.*s");
                    } else if float_override_positions.contains(&spec_idx) && (spec.contains("lld") || spec.contains("llu")) {
                        result.push_str("%f");
                    } else {
                        result.push_str(&spec);
                    }
                }
            } else {
                result.push(ch);
            }
        }
        parts[0] = result;
    }
    parts.join(", ")
}

/// Format arguments for user function calls with type-override-aware coercion.
/// Only coerces GorgetString → Str when the target function parameter expects Str.
fn format_args_with_coercion(
    args: &[Operand], func: &Function, registry: &TypeRegistry,
    type_overrides: &std::collections::HashMap<usize, String>,
    target_name: &str, all_functions: &[Function],
) -> String {
    // Look up the target function's parameter types
    let target_params: Option<&[TypeId]> = all_functions.iter()
        .find(|f| mangle_name(&f.name) == target_name)
        .map(|f| f.params.as_slice());

    let mut parts = Vec::new();
    for (arg_idx, arg) in args.iter().enumerate() {
        let arg_str = format_operand(arg, func, registry);
        match arg {
            Operand::Copy(place) | Operand::Move(place) => {
                let local_idx = place.local.0 as usize;
                let eff_type = effective_c_type(local_idx, func, registry, type_overrides);
                if eff_type == "GorgetString" {
                    // Only coerce GorgetString → Str if the target param expects Str
                    let target_expects_str = target_params
                        .and_then(|params| params.get(arg_idx))
                        .map(|&tid| format_type(tid, registry) == "Str")
                        .unwrap_or(true); // default to coerce for unknown targets
                    if target_expects_str {
                        parts.push(format!("(Str){{ .data = {arg_str}.data, .len = {arg_str}.len }}"));
                    } else {
                        parts.push(arg_str);
                    }
                } else {
                    // Check if target expects void* but arg is a closure struct
                    let target_param_type = target_params
                        .and_then(|params| params.get(arg_idx))
                        .map(|&tid| format_type(tid, registry));
                    let target_is_void_ptr = matches!(
                        target_param_type.as_deref(),
                        Some("const void*") | Some("void*") | Some("void")
                    );
                    if target_is_void_ptr
                        && eff_type.starts_with("__Closure_")
                    {
                        // Pass [fn_ptr, env_ptr] pair for Callable dispatch
                        let closure_type = &eff_type;
                        let call_fn = format!("{closure_type}__call");
                        parts.push(format!("(void*)(void*[2]){{(void*){call_fn}, (void*)&{arg_str}}}"));
                    } else {
                        // Pointer→value coercion: dereference when passing self (Ptr(T))
                        // to a function parameter expecting T (value type).
                        let local_idx = place.local.0 as usize;
                        let local_type_id = if local_idx < func.locals.len() {
                            Some(func.locals[local_idx].type_id)
                        } else {
                            None
                        };
                        let is_ptr = local_type_id.map_or(false, |tid| {
                            matches!(registry.get(tid), Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_)))
                        });
                        let target_is_ptr = target_params
                            .and_then(|params| params.get(arg_idx))
                            .map_or(false, |&tid| {
                                matches!(registry.get(tid), Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_)))
                            });
                        // Auto-deref pointer args: when the target function's param
                        // types are known, deref if the param is a value type.
                        // When unknown (inline wrappers not in all_functions), deref
                        // arg 0 ONLY for Channel types — their wrappers take self by
                        // value (Channel__T = GorgetChannel*), so MutPtr(Channel__T)
                        // needs one deref. Other types (Guard, Shared, RwLock) take
                        // self by pointer, so their MutPtr refs should stay as-is.
                        let should_deref = if target_params.is_some() {
                            !target_is_ptr
                        } else if arg_idx == 0 {
                            // Only auto-deref self for channel-pointer types
                            let inner_type_name = local_type_id.and_then(|tid| {
                                match registry.get(tid)? {
                                    GirType::Ptr(inner) | GirType::MutPtr(inner) => {
                                        Some(format_type(*inner, registry))
                                    }
                                    _ => None,
                                }
                            });
                            inner_type_name.map_or(false, |n| n.starts_with("Channel__"))
                        } else {
                            false
                        };
                        // Take address when target expects a pointer but arg is a value type.
                        // This happens when the IR dereferences a borrow and then passes
                        // the resulting value to a function that still expects a pointer param.
                        let should_addr = target_params.is_some() && target_is_ptr && !is_ptr;
                        if is_ptr && should_deref && place.projections.is_empty() {
                            parts.push(format!("(*{arg_str})"));
                        } else if should_addr {
                            parts.push(format!("&{arg_str}"));
                        } else {
                            parts.push(arg_str);
                        }
                    }
                }
            }
            Operand::Constant(Constant::Str(s)) => {
                let escaped = escape_c_string(s);
                // Check if target expects const char* (runtime C functions)
                let is_cstr_target = matches!(target_name,
                    "gorget_parse_int" | "gorget_parse_float" | "gorget_throw" | "gorget_panic"
                    | "puts" | "fputs" | "system" | "getenv" | "gorget_string_new");
                if is_cstr_target {
                    parts.push(format!("\"{}\"", escaped));
                } else {
                    // Check if target param expects GorgetString
                    let target_expects_owned = target_params
                        .and_then(|params| params.get(arg_idx))
                        .map(|&tid| format_type(tid, registry) == "GorgetString")
                        .unwrap_or(false);
                    if target_expects_owned {
                        parts.push(format!("gorget_string_new(\"{}\")", escaped));
                    } else {
                        parts.push(format!("gorget_str_from_literal(\"{escaped}\", {})", s.len()));
                    }
                }
            }
            _ => parts.push(arg_str),
        }
    }
    parts.join(", ")
}

/// Look up a local's type and format it.
fn format_local_type(func: &Function, local_idx: usize, registry: &TypeRegistry) -> String {
    if local_idx < func.locals.len() {
        format_type(func.locals[local_idx].type_id, registry)
    } else {
        "int64_t".to_string()
    }
}

/// Extract the element type for a collection method call by parsing the mangled name.
/// E.g., "Vector__Str__get" → "Str", "Dict__Str__int64_t__keys" → "Str"
/// Extract element type from a collection type name (e.g., "Vector__Str" → "Str", "Dict__Str__int64_t" → "int64_t")
fn extract_element_type_from_collection(type_name: &str) -> Option<String> {
    // Shared__T: strip prefix and recurse to get element type of the inner collection
    if let Some(rest) = type_name.strip_prefix("Shared__") {
        return extract_element_type_from_collection(rest);
    }
    // Vector__T or GorgetArray (with embedded type hint)
    if let Some(rest) = type_name.strip_prefix("Vector__") {
        return Some(rest.to_string());
    }
    // Set__T or HashSet__T
    if let Some(rest) = type_name.strip_prefix("Set__")
        .or_else(|| type_name.strip_prefix("HashSet__")) {
        return Some(rest.to_string());
    }
    // Dict__K__V → value type V
    if let Some(rest) = type_name.strip_prefix("Dict__") {
        // Find the split between K and V — take everything after first __
        if let Some(pos) = rest.find("__") {
            return Some(rest[pos + 2..].to_string());
        }
    }
    if let Some(rest) = type_name.strip_prefix("Map__") {
        if let Some(pos) = rest.find("__") {
            return Some(rest[pos + 2..].to_string());
        }
    }
    None
}

/// Split a two-element tuple type's inner portion into its component types.
/// E.g., "int64_t__Str" → Some(("int64_t", "Str")),
///       "Str__int64_t" → Some(("Str", "int64_t")),
///       "int64_t__int64_t" → Some(("int64_t", "int64_t"))
/// Handles multi-segment type names like "int64_t" by trying each __ split position.
fn find_tuple_type_split(rest: &str) -> Option<(String, String)> {
    // Try each __ position as a split point.
    // A valid split produces two non-empty type names.
    let mut pos = 0;
    while let Some(idx) = rest[pos..].find("__") {
        let split = pos + idx;
        let first = &rest[..split];
        let second = &rest[split + 2..];
        if !first.is_empty() && !second.is_empty() {
            // Heuristic: a valid C type name either starts with a letter/underscore
            // or is a known primitive. Accept the first valid split.
            return Some((first.to_string(), second.to_string()));
        }
        pos = split + 2;
    }
    None
}

/// Extract the element type from a mangled higher-order method name.
/// E.g., "Vector__Student__map" → "Student", "Vector__int64_t__filter" → "int64_t"
/// Used as fallback when the collection's runtime C type is erased (e.g., GorgetArray).
fn extract_element_type_from_method_name(func_name: &str) -> Option<String> {
    let rest = func_name.strip_prefix("Vector__")
        .or_else(|| func_name.strip_prefix("Set__"))
        .or_else(|| func_name.strip_prefix("HashSet__"))?;
    // rest = "ElemType__method" — find the last __ to separate elem from method
    if let Some(pos) = rest.rfind("__") {
        let method = &rest[pos + 2..];
        // method should start with a lowercase letter
        if method.chars().next().map(|c| c.is_lowercase()).unwrap_or(false) {
            let elem = &rest[..pos];
            if !elem.is_empty() {
                return Some(elem.to_string());
            }
        }
    }
    None
}

/// Extract the key type from a mangled Dict/HashMap name.
/// e.g., "Dict__Str__int64_t__keys" → "Str", "Dict__Str__int64_t" → "Str"
fn extract_map_key_type(name: &str) -> Option<&str> {
    let rest = name.strip_prefix("Dict__")
        .or_else(|| name.strip_prefix("HashMap__"))
        .or_else(|| name.strip_prefix("Map__"))?;
    // Key is everything before the first __ separator
    if let Some(pos) = rest.find("__") {
        Some(&rest[..pos])
    } else {
        Some(rest)
    }
}

fn collection_elem_type_from_name(original_name: &str) -> Option<&str> {
    // Try Vector__T__method
    if let Some(rest) = original_name.strip_prefix("Vector__") {
        if let Some(pos) = rest.rfind("__") {
            return Some(&rest[..pos]);
        }
    }
    // Try Dict__K__V__method or HashMap__K__V__method → extract V
    for prefix in &["Dict__", "HashMap__"] {
        if let Some(rest) = original_name.strip_prefix(prefix) {
            // Strip trailing method: find last `__method` where method is lowercase
            let type_part = if let Some(pos) = rest.rfind("__") {
                let suffix = &rest[pos + 2..];
                if !suffix.is_empty() && suffix.starts_with(|c: char| c.is_lowercase()) {
                    &rest[..pos]
                } else {
                    rest
                }
            } else {
                rest
            };
            // Now type_part = K__V — find first __ to split key from value
            if let Some(sep) = type_part.find("__") {
                return Some(&type_part[sep + 2..]);
            }
        }
    }
    None
}

/// Format a collection element type for casts. Uses the mangled collection name
/// to determine the element type when the destination local has wrong IR type.
fn collection_element_c_type(func: &Function, local_idx: usize, registry: &TypeRegistry, original_name: &str,
    type_overrides: &std::collections::HashMap<usize, String>) -> String {
    // Check type_overrides first (most reliable), then GIR type
    let eff_type = effective_c_type(local_idx, func, registry, type_overrides);
    // Map collection aliases through runtime_type_name (Vector__T → GorgetArray)
    let ir_type = if let Some(rt) = runtime_type_name(&eff_type) {
        rt.to_string()
    } else { eff_type };
    // If IR type is int64_t, try to infer from collection name
    if ir_type == "int64_t" {
        if let Some(elem) = collection_elem_type_from_name(original_name) {
            // Map nested collection names to runtime types
            if let Some(rt) = runtime_type_name(elem) {
                return rt.to_string();
            }
            return elem.to_string();
        }
    }
    ir_type
}

/// Check if a place resolves to a pointer type after applying projections.
/// A local with Ptr type is a pointer, but if there's a Deref projection,
/// the result is the pointed-to value (not a pointer anymore).
fn is_place_ptr_type(func: &Function, place: &Place, registry: &TypeRegistry) -> bool {
    let local_idx = place.local.0 as usize;
    if local_idx >= func.locals.len() {
        return false;
    }
    let mut is_ptr = matches!(
        registry.get(func.locals[local_idx].type_id),
        Some(GirType::Ptr(_) | GirType::MutPtr(_))
    );
    // Each Deref "unwraps" one pointer level; Field/Index don't change ptr-ness
    for proj in &place.projections {
        match proj {
            Projection::Deref => { is_ptr = false; }
            _ => {}
        }
    }
    is_ptr
}

/// Format a binary operator as its C symbol.
fn format_binop(op: BinOp) -> &'static str {
    match op {
        BinOp::Add => "+",
        BinOp::Sub => "-",
        BinOp::Mul => "*",
        BinOp::Div => "/",
        BinOp::Rem => "%",
        BinOp::Mod => "%", // fallback only — actual modulo uses inline code
        BinOp::Pow => "pow_placeholder",
        BinOp::BitAnd => "&",
        BinOp::BitOr => "|",
        BinOp::BitXor => "^",
        BinOp::Shl => "<<",
        BinOp::Shr => ">>",
        BinOp::AddWrap => "+",
        BinOp::SubWrap => "-",
        BinOp::MulWrap => "*",
    }
}

/// Get the TypeId of an operand from its source local.
fn operand_type(op: &Operand, func: &Function) -> Option<TypeId> {
    match op {
        Operand::Copy(place) | Operand::Move(place) => {
            let idx = place.local.0 as usize;
            if idx < func.locals.len() {
                Some(func.locals[idx].type_id)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Check if we need to coerce GorgetString → Str (or vice versa).
fn needs_string_coercion(dst_type: TypeId, src_type: Option<TypeId>, registry: &TypeRegistry) -> bool {
    let Some(src) = src_type else { return false };
    if dst_type == src { return false; }
    let dst_name = match registry.get(dst_type) {
        Some(GirType::Named(n)) => n.as_str(),
        _ => return false,
    };
    let src_name = match registry.get(src) {
        Some(GirType::Named(n)) => n.as_str(),
        _ => return false,
    };
    // GorgetString → Str coercion
    dst_name == "Str" && src_name == "GorgetString"
}

/// Format a comparison operator as its C symbol.
fn format_cmpop(op: CmpOp) -> &'static str {
    match op {
        CmpOp::Eq => "==",
        CmpOp::Ne => "!=",
        CmpOp::Lt => "<",
        CmpOp::Le => "<=",
        CmpOp::Gt => ">",
        CmpOp::Ge => ">=",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::builder::FunctionBuilder;

    #[test]
    fn emit_empty_main() {
        let mut module = Module::new();
        let mut b = FunctionBuilder::new("main", I32_TYPE, &[]);
        b.assign(Place::local(LocalId(0)), FunctionBuilder::const_i32(0));
        b.ret(FunctionBuilder::copy(LocalId(0)));
        module.functions.push(b.build());

        let c_code = generate_c(&module).c_code;
        assert!(c_code.contains("int main(int argc, char** argv)"));
        assert!(c_code.contains("return _0;"));
    }

    #[test]
    fn emit_arithmetic() {
        let mut module = Module::new();
        let mut b = FunctionBuilder::new(
            "add",
            I64_TYPE,
            &[(I64_TYPE, Some("a")), (I64_TYPE, Some("b"))],
        );
        let sum = b.bin_op(
            BinOp::Add,
            I64_TYPE,
            FunctionBuilder::copy(LocalId(1)),
            FunctionBuilder::copy(LocalId(2)),
        );
        b.assign(Place::local(LocalId(0)), FunctionBuilder::copy(sum));
        b.ret(FunctionBuilder::copy(LocalId(0)));
        module.functions.push(b.build());

        let c_code = generate_c(&module).c_code;
        assert!(c_code.contains("int64_t add(int64_t _1, int64_t _2)"));
        assert!(c_code.contains("+"));
        assert!(c_code.contains("return _0;"));
    }

    #[test]
    fn emit_basic_blocks() {
        let mut module = Module::new();
        let mut b = FunctionBuilder::new(
            "f",
            I64_TYPE,
            &[(I64_TYPE, Some("x"))],
        );

        let then_bb = b.new_block();
        let else_bb = b.new_block();
        let merge_bb = b.new_block();

        let cond = b.cmp(
            CmpOp::Gt,
            I64_TYPE,
            FunctionBuilder::copy(LocalId(1)),
            FunctionBuilder::const_i64(0),
        );
        b.branch(FunctionBuilder::copy(cond), then_bb, else_bb);

        b.switch_to(then_bb);
        b.assign(Place::local(LocalId(0)), FunctionBuilder::copy(LocalId(1)));
        b.jump(merge_bb);

        b.switch_to(else_bb);
        let neg = b.un_op(UnOp::Neg, I64_TYPE, FunctionBuilder::copy(LocalId(1)));
        b.assign(Place::local(LocalId(0)), FunctionBuilder::copy(neg));
        b.jump(merge_bb);

        b.switch_to(merge_bb);
        b.ret(FunctionBuilder::copy(LocalId(0)));

        module.functions.push(b.build());

        let c_code = generate_c(&module).c_code;
        assert!(c_code.contains("bb0:"));
        assert!(c_code.contains("bb1:"));
        assert!(c_code.contains("bb2:"));
        assert!(c_code.contains("bb3:"));
        assert!(c_code.contains("goto bb"));
        assert!(c_code.contains("if ("));
    }

    #[test]
    fn emit_struct_typedef() {
        let mut module = Module::new();
        module.type_registry.add_type_def(TypeDef {
            name: "Point".into(),
            kind: TypeDefKind::Struct(StructDef {
                fields: vec![
                    StructField { name: "x".into(), type_id: F64_TYPE },
                    StructField { name: "y".into(), type_id: F64_TYPE },
                ],
            }),
            metadata: TypeMetadata::default(),
        });
        let point_id = module.type_registry.insert(GirType::Named("Point".into()));

        // Add a main function that uses the struct
        let mut b = FunctionBuilder::new("main", I32_TYPE, &[]);
        let _ = b.struct_init("Point", point_id, vec![
            FunctionBuilder::const_f64(1.0),
            FunctionBuilder::const_f64(2.0),
        ]);
        b.assign(Place::local(LocalId(0)), FunctionBuilder::const_i32(0));
        b.ret(FunctionBuilder::copy(LocalId(0)));
        module.functions.push(b.build());

        let c_code = generate_c(&module).c_code;
        assert!(c_code.contains("typedef struct Point Point;"));
        assert!(c_code.contains("struct Point {"));
        assert!(c_code.contains("double x;"));
        assert!(c_code.contains("double y;"));
        assert!(c_code.contains("_1 = (Point){.x = 1.0, .y = 2.0}"));
    }

    #[test]
    fn emit_enum_typedef() {
        let mut module = Module::new();
        module.type_registry.add_type_def(TypeDef {
            name: "Shape".into(),
            kind: TypeDefKind::Enum(EnumDef {
                variants: vec![
                    EnumVariant {
                        name: "Circle".into(),
                        fields: vec![StructField { name: "_0".into(), type_id: F64_TYPE }],
                    },
                    EnumVariant {
                        name: "Rect".into(),
                        fields: vec![
                            StructField { name: "_0".into(), type_id: F64_TYPE },
                            StructField { name: "_1".into(), type_id: F64_TYPE },
                        ],
                    },
                    EnumVariant { name: "Empty".into(), fields: vec![] },
                ],
            }),
            metadata: TypeMetadata::default(),
        });

        // Need a function for valid module
        let mut b = FunctionBuilder::new("main", I32_TYPE, &[]);
        b.assign(Place::local(LocalId(0)), FunctionBuilder::const_i32(0));
        b.ret(FunctionBuilder::copy(LocalId(0)));
        module.functions.push(b.build());

        let c_code = generate_c(&module).c_code;
        assert!(c_code.contains("typedef struct Shape Shape;"));
        assert!(c_code.contains("struct Shape {"));
        assert!(c_code.contains("int tag;"));
        assert!(c_code.contains("union {"));
        assert!(c_code.contains("} Circle;"));
        assert!(c_code.contains("} Rect;"));
    }

    #[test]
    fn emit_global_constant() {
        let mut module = Module::new();
        module.globals.push(Global {
            name: "my_vtable".into(),
            type_id: I64_TYPE,
            init: GlobalInit::Zeroed,
        });

        let mut b = FunctionBuilder::new("main", I32_TYPE, &[]);
        b.assign(Place::local(LocalId(0)), FunctionBuilder::const_i32(0));
        b.ret(FunctionBuilder::copy(LocalId(0)));
        module.functions.push(b.build());

        let c_code = generate_c(&module).c_code;
        assert!(c_code.contains("static int64_t my_vtable = {0};"));
    }
}
