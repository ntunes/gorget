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
    /// Host binary C code (only `Some` when `module.hot_reload == true`).
    pub host_code: Option<String>,
    /// Guest shared library C code (only `Some` when `module.hot_reload == true`).
    pub guest_code: Option<String>,
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
        "char_to_str" => "gorget_char_to_str",
        "bool_to_str" => "gorget_bool_to_str",
        "ord" => "gorget_char_ord",
        "chr" => "gorget_char_chr",
        "parse_int" => "gorget_parse_int",
        "parse_float" => "gorget_parse_float",
        "print_no_newline" => "gorget_print_no_newline",
        "codepoint_to_utf8" => "gorget_codepoint_to_utf8",
        // File I/O
        "read_file" => "gorget_read_file",
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
        // AtomicInt methods
        "AtomicInt__load" => "gorget_atomic_int_load",
        "AtomicInt__store" => "gorget_atomic_int_store",
        "AtomicInt__add" => "gorget_atomic_int_add",
        "AtomicInt__sub" => "gorget_atomic_int_sub",
        "AtomicInt__compare_exchange" => "gorget_atomic_int_compare_exchange",
        // AtomicBool methods
        "AtomicBool__load" => "gorget_atomic_bool_load",
        "AtomicBool__store" => "gorget_atomic_bool_store",
        "AtomicBool__swap" => "gorget_atomic_bool_swap",
        "AtomicBool__compare_exchange" => "gorget_atomic_bool_compare_exchange",
        // Barrier methods
        "Barrier__wait" => "gorget_barrier_wait",
        // CondVar methods
        "CondVar__notify_one" => "gorget_condvar_notify_one",
        "CondVar__notify_all" => "gorget_condvar_notify_all",
        "CondVar__wait"       => "gorget_condvar_wait_guard",
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
        "Str__char_at" => "gorget_str_char_at",
        "Str__hash" => "gorget_str_hash",
        // SDL: sdl_foo → gorget_sdl_foo
        "sdl_init" => "gorget_sdl_init",
        "sdl_quit" => "gorget_sdl_quit",
        "sdl_create_window" => "gorget_sdl_create_window",
        "sdl_create_window_try" => "gorget_sdl_create_window_try",
        "sdl_window_is_null" => "gorget_sdl_window_is_null",
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
        | "gorget_readline" | "gorget_input"
        | "gorget_exec" | "gorget_exec_output"
        | "gorget_getenv" | "gorget_setenv"
        | "gorget_format_time" | "gorget_parse_time"
        | "gorget_seed" | "gorget_sleep_ms" | "gorget_reactor_sleep_ms"
        | "gorget_socket_connect" | "gorget_socket_write_str"
        | "gorget_tls_connect" | "gorget_tls_write_str"
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
    false
}

/// Functions that return `const char*` or `char*` (need wrapping to Str/GorgetString).
fn returns_cstr(name: &str) -> bool {
    matches!(name,
        "gorget_int_to_str" | "gorget_float_to_str" | "gorget_char_to_str"
        | "gorget_bool_to_str" | "gorget_codepoint_to_utf8"
        | "gorget_path_parent" | "gorget_path_basename" | "gorget_path_extension"
        | "gorget_path_stem" | "gorget_path_join"
        | "gorget_readline" | "gorget_input"
        | "gorget_getcwd" | "gorget_platform"
        | "gorget_format_time"
        | "gorget_base64_encode" | "gorget_hex_encode"
        | "gorget_bytes_to_str" | "gorget_bytes_to_hex"
        | "gorget_url_encode"
        | "getenv" | "gorget_getenv"
        | "gorget_regex_match_text" | "gorget_regex_pattern_str"
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
    if module.test_fns.is_empty() && !module.is_test_module {
        out.push_str(c_runtime::PANIC_NORMAL);
    } else {
        out.push_str(c_runtime::PANIC_TEST);
    }
    out.push_str(c_runtime::RUNTIME_CORE);

    // Conditionally include optional runtime sections based on functions used
    let all_call_names = collect_all_call_names(module);
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
    if all_call_names.iter().any(|n| n.starts_with("gorget_socket_") || n == "socket_connect" || n == "socket_listen") {
        out.push_str(c_runtime::SOCKET_RUNTIME);
    }
    if all_call_names.iter().any(|n| n.starts_with("gorget_udp_") || n == "udp_bind") {
        out.push_str(c_runtime::UDP_SOCKET_RUNTIME);
    }
    if all_call_names.iter().any(|n| n.starts_with("gorget_tls_") || n == "tls_connect") {
        out.push_str(c_runtime::TLS_SOCKET_RUNTIME);
    }
    if all_call_names.iter().any(|n| n == "gorget_exec" || n == "gorget_exec_output" || n == "exec" || n == "exec_output") {
        out.push_str(c_runtime::PROCESS_RUNTIME);
    }
    if module.has_process
        || all_call_names.iter().any(|n| n.starts_with("gorget_process_") || n == "process_spawn" || n == "getpid" || n == "gorget_getpid" || n.starts_with("Process__")) {
        out.push_str(c_runtime::PROCESS_SPAWN_RUNTIME);
    }
    if module.has_sync
        || all_call_names.iter().any(|n| n.starts_with("gorget_atomic_") || n.starts_with("gorget_barrier_") || n.starts_with("gorget_condvar_") || n.starts_with("gorget_rwlock_") || n.starts_with("AtomicInt__") || n.starts_with("AtomicBool__") || n.starts_with("Barrier__") || n.starts_with("CondVar__") || n.starts_with("RWLock__") || n.starts_with("ReadGuard__") || n.starts_with("WriteGuard__")) {
        out.push_str(c_runtime::SYNC_RUNTIME);
    }
    if module.has_thread
        || !module.thread_spawned_fns.is_empty()
        || all_call_names.iter().any(|n| n == "gorget_current_thread_id" || n == "current_thread_id" || n.starts_with("__gorget_thread_spawn_") || n.starts_with("Thread__")) {
        out.push_str(c_runtime::THREAD_RUNTIME);
    }
    let needs_async_runtime = module.has_async || module.has_spawn
        || all_call_names.iter().any(|n| n.contains("channel_") || n.contains("Channel")
            || n.contains("gorget_executor_") || n == "gorget_spawn" || n.contains("GorgetTask"));
    if needs_async_runtime {
        // ASYNC_RUNTIME must precede CHANNEL_RUNTIME and EXECUTOR_RUNTIME (they use GorgetWaker).
        out.push_str(c_runtime::ASYNC_RUNTIME);
    }
    if all_call_names.iter().any(|n| n.contains("gorget_executor_") || n == "gorget_spawn" || n.contains("GorgetTask"))
        || module.has_spawn {
        out.push_str(c_runtime::EXECUTOR_RUNTIME);
    }
    let needs_channel = !module.channel_types.is_empty()
        || all_call_names.iter().any(|n| n.contains("channel_") || n.contains("Channel"));
    if needs_channel {
        out.push_str(c_runtime::CHANNEL_RUNTIME);
    }
    if needs_channel || module.has_spawn {
        // Emit Channel__T typedefs and wrapper functions (if any), plus Task__T structs.
        emit_channel_and_task_defs(&mut out, module);
    }
    let needs_shared = !module.shared_types.is_empty() || !module.weak_types.is_empty();
    if needs_shared {
        out.push_str(c_runtime::SHARED_RUNTIME);
        // Wrapper functions (emit_shared_defs, emit_weak_defs) emitted after user type definitions below.
    }
    let needs_mutex = !module.mutex_types.is_empty();
    if needs_mutex {
        // ASYNC_RUNTIME must be emitted before this (gorget_mutex may use GorgetWaker).
        if !needs_async_runtime {
            out.push_str(c_runtime::ASYNC_RUNTIME);
        }
        out.push_str(c_runtime::MUTEX_RUNTIME);
        // Wrapper functions (emit_mutex_defs) emitted after user type definitions below.
    }
    if module.has_task_group {
        if !needs_async_runtime && !needs_mutex {
            out.push_str(c_runtime::ASYNC_RUNTIME);
        }
        if !needs_mutex {
            out.push_str(c_runtime::MUTEX_RUNTIME);
        }
        out.push_str(c_runtime::TASK_GROUP_RUNTIME);
    }
    // Detect std.async.async_sleep(ms) calls — mapped to gorget_reactor_sleep_ms in KNOWN_MAPPINGS.
    // Also detect legacy int-arg sleep() calls from std.async import (pre-rename path).
    let needs_reactor = all_call_names.iter().any(|n| n == "async_sleep" || n == "gorget_reactor_sleep_ms")
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
        if !all_call_names.iter().any(|n| n.contains("gorget_executor_") || n == "gorget_spawn") && !module.has_spawn {
            out.push_str(c_runtime::EXECUTOR_RUNTIME);
        }
        out.push_str(c_runtime::REACTOR_RUNTIME);
    }
    if module.hot_reload || all_call_names.iter().any(|n| n.contains("hot_reload") || n.contains("plugin")) {
        out.push_str(c_runtime::HOT_RELOAD_RUNTIME);
    }
    if module.trace_filename.is_some() {
        out.push_str(c_runtime::TRACE_RUNTIME);
    }
    if all_call_names.iter().any(|n| n.starts_with("sdl_") || n.starts_with("gorget_sdl_")) {
        out.push_str(c_runtime::SDL_RUNTIME);
    }

    // GIR-specific helpers
    out.push_str("\nstatic int gorget_generic_compare(const void* a, const void* b) {\n    return memcmp(a, b, sizeof(int64_t));\n}\n");
    // Inline helpers for char operations (ord/chr)
    out.push_str("static inline int64_t gorget_char_ord(uint32_t c) { return (int64_t)c; }\n");
    out.push_str("static inline Str gorget_char_chr(int64_t code) { return gorget_str_from_cstr(gorget_codepoint_to_utf8(code)); }\n");
    out.push_str("static inline uint32_t gorget_str_decode_codepoint(const char* data, size_t len) {\n");
    out.push_str("    if (len == 0) return 0;\n");
    out.push_str("    uint8_t b = (uint8_t)data[0];\n");
    out.push_str("    if (b < 0x80) return b;\n");
    out.push_str("    if ((b & 0xE0) == 0xC0 && len >= 2) return ((b & 0x1F) << 6) | (data[1] & 0x3F);\n");
    out.push_str("    if ((b & 0xF0) == 0xE0 && len >= 3) return ((b & 0x0F) << 12) | ((data[1] & 0x3F) << 6) | (data[2] & 0x3F);\n");
    out.push_str("    if ((b & 0xF8) == 0xF0 && len >= 4) return ((b & 0x07) << 18) | ((data[1] & 0x3F) << 12) | ((data[2] & 0x3F) << 6) | (data[3] & 0x3F);\n");
    out.push_str("    return b;\n");
    out.push_str("}\n");
    out.push_str("static inline uint32_t gorget_str_char_at(Str* s, int64_t idx) { return (uint32_t)gorget_str_byte_at(*s, idx); }\n");
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

    // Emit Shared[T], Weak[T], and Mutex[T] wrapper functions AFTER user struct typedefs
    // so that inner types like 'Config' are already declared when used in wrapper sigs.
    if needs_shared {
        emit_shared_defs(&mut out, module);
        emit_weak_defs(&mut out, module);
    }
    if needs_mutex {
        emit_mutex_defs(&mut out, module);
    }
    if !module.rwlock_types.is_empty() {
        emit_rwlock_defs(&mut out, module);
    }
    if !module.thread_types.is_empty() || !module.thread_spawned_fns.is_empty() {
        emit_thread_defs(&mut out, module);
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
    if !module.spawned_fns.is_empty() {
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
    let has_test_runner = !module.test_fns.is_empty() || module.is_test_module;
    for func in &module.functions {
        if is_template_function(&func.name, &skip_names) {
            continue;
        }
        // Skip user main() when test runner will provide main()
        if has_test_runner && func.name == "main" {
            continue;
        }
        emit_function(&mut out, func, module);
        out.push('\n');
    }

    // Test runner main (if test functions were registered, or forced by test_mode).
    if !module.test_fns.is_empty() || module.is_test_module {
        emit_test_runner_main(&mut out, module);
    }

    // Hot-reload: generate split host/guest sources.
    if module.hot_reload {
        let (host, guest) = generate_hot_reload_split(module, &out, hr_opts);
        return GirCodegenOutput { c_code: out, host_code: Some(host), guest_code: Some(guest) };
    }

    GirCodegenOutput { c_code: out, host_code: None, guest_code: None }
}

/// Split a full compiled C string into host + guest for hot-reload mode.
///
/// - Guest: full code minus main(), plus state hash constant + exported wrappers.
/// - Host: runtime/type section only + HOT_RELOAD_RUNTIME + a dlopen-based main().
fn generate_hot_reload_split(module: &Module, full_c: &str, hr_opts: Option<&HotReloadOpts>) -> (String, String) {
    let state_type = module.hot_reload_state_type.as_deref().unwrap_or("State");
    let state_hash = module.hot_reload_state_hash;
    let has_reload = module.hot_reload_has_reload_fn;

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
    let test_fns = &module.test_fns;
    let registry = &module.type_registry;
    let tracing = module.trace_filename.is_some();
    let _ = writeln!(out, "int main(int argc, char** argv) {{");
    let _ = writeln!(out, "    gorget_init_args(argc, argv);");
    if let Some(ref trace_path) = module.trace_filename {
        let escaped = trace_path.replace('\\', "\\\\").replace('"', "\\\"");
        let _ = writeln!(out, "    __gorget_trace_init(\"{escaped}\");");
    }
    let _ = writeln!(out, "    int __test_passed = 0, __test_failed = 0;");
    let _ = writeln!(out, "    struct timespec __total_start, __total_end;");
    let _ = writeln!(out, "    clock_gettime(CLOCK_MONOTONIC, &__total_start);");
    let _ = writeln!(out, "    printf(\"Running {} tests...\\n\");", test_fns.len());

    if module.has_suite_setup {
        let _ = writeln!(out, "    __suite_setup();");
    }

    for info in test_fns {
        let fn_name = &info.fn_name;
        let escaped = info.display_name.replace('\\', "\\\\").replace('"', "\\\"");
        let _ = writeln!(out, "    printf(\"  test: {escaped} ... \");");
        let _ = writeln!(out, "    fflush(stdout);");
        if tracing {
            let _ = writeln!(out, "    if (__gorget_trace_fp) fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"test_start\\\",\\\"name\\\":\\\"{escaped}\\\"}}\\n\");");
        }
        let _ = writeln!(out, "    {{");
        let _ = writeln!(out, "        __gorget_in_test = 1;");
        let _ = writeln!(out, "        __gorget_test_fail_msg = NULL;");
        let _ = writeln!(out, "        int __cleanup_mark = __gorget_cleanup_top;");
        let _ = writeln!(out, "        struct timespec __t_start, __t_end;");
        let _ = writeln!(out, "        clock_gettime(CLOCK_MONOTONIC, &__t_start);");

        // Declare and initialize with-bindings in the test runner's stack frame.
        // This ensures they're alive (and dropped) even when the test panics via longjmp.
        for (wb_idx, wb) in info.with_bindings.iter().enumerate() {
            let c_type = format_type(wb.type_id, registry);
            let init_fn = &wb.init_fn_name;
            let _ = writeln!(out, "        {c_type} __wb_{wb_idx} = {init_fn}();");
        }

        // Build the argument list: &__wb_0, &__wb_1, ...
        let wb_args: String = (0..info.with_bindings.len())
            .map(|i| format!("&__wb_{i}"))
            .collect::<Vec<_>>()
            .join(", ");

        // Tell gorget_panic() where to start running cleanup on panic.
        // gorget_panic() calls __gorget_cleanup_run(__gorget_test_cleanup_mark) BEFORE
        // longjmp, while the test function's stack frame is still valid.
        // This ensures stack-allocated test-body locals are dropped with valid pointers.
        let _ = writeln!(out, "        __gorget_test_cleanup_mark = __cleanup_mark;");
        let _ = writeln!(out, "        if (setjmp(__gorget_test_jmp) == 0) {{");
        let _ = writeln!(out, "            {fn_name}({wb_args});");
        // On normal exit: reset cleanup stack WITHOUT running (entries already dropped by test fn).
        let _ = writeln!(out, "            __gorget_cleanup_top = __cleanup_mark;");
        let _ = writeln!(out, "        }}");
        // On panic (longjmp): gorget_panic already ran cleanup. This is now a no-op.
        let _ = writeln!(out, "        __gorget_cleanup_run(__cleanup_mark);");
        let _ = writeln!(out, "        __gorget_in_test = 0;");

        // Drop with-bindings in LIFO order (reverse of initialization).
        for wb_idx in (0..info.with_bindings.len()).rev() {
            let wb = &info.with_bindings[wb_idx];
            if let Some(type_name) = gir_type_name(wb.type_id, registry) {
                if needs_drop_by_name(&type_name, registry) {
                    emit_drop_for_type_via_ptr(
                        out,
                        &format!("&__wb_{wb_idx}"),
                        &type_name,
                        registry,
                        "        ",
                        0,
                    );
                }
            }
        }

        let _ = writeln!(out, "        clock_gettime(CLOCK_MONOTONIC, &__t_end);");
        let _ = writeln!(out, "        long __t_ms = (__t_end.tv_sec - __t_start.tv_sec) * 1000 + (__t_end.tv_nsec - __t_start.tv_nsec) / 1000000;");
        if tracing {
            let _ = writeln!(out, "        int __test_trace_ok = 0;");
        }

        if info.should_panic {
            if let Some(ref msg) = info.expected_panic_msg {
                let escaped_msg = msg.replace('\\', "\\\\").replace('"', "\\\"");
                let _ = writeln!(out, "        if (__gorget_test_fail_msg && strstr(__gorget_test_fail_msg, \"{escaped_msg}\")) {{");
                let _ = writeln!(out, "            __test_passed++;");
                if tracing { let _ = writeln!(out, "            __test_trace_ok = 1;"); }
                let _ = writeln!(out, "            printf(\"PASS (%ldms)\\n\", __t_ms);");
                let _ = writeln!(out, "        }} else if (__gorget_test_fail_msg) {{");
                let _ = writeln!(out, "            __test_failed++;");
                let _ = writeln!(out, "            printf(\"FAIL: expected panic containing \\\"{escaped_msg}\\\", got: %s (%ldms)\\n\", __gorget_test_fail_msg, __t_ms);");
                let _ = writeln!(out, "        }} else {{");
                let _ = writeln!(out, "            __test_failed++;");
                let _ = writeln!(out, "            printf(\"FAIL: expected panic but test passed (%ldms)\\n\", __t_ms);");
                let _ = writeln!(out, "        }}");
            } else {
                let _ = writeln!(out, "        if (__gorget_test_fail_msg) {{");
                let _ = writeln!(out, "            __test_passed++;");
                if tracing { let _ = writeln!(out, "            __test_trace_ok = 1;"); }
                let _ = writeln!(out, "            printf(\"PASS (%ldms)\\n\", __t_ms);");
                let _ = writeln!(out, "        }} else {{");
                let _ = writeln!(out, "            __test_failed++;");
                let _ = writeln!(out, "            printf(\"FAIL: expected panic but test passed (%ldms)\\n\", __t_ms);");
                let _ = writeln!(out, "        }}");
            }
        } else {
            let _ = writeln!(out, "        if (!__gorget_test_fail_msg) {{");
            let _ = writeln!(out, "            __test_passed++;");
            if tracing { let _ = writeln!(out, "            __test_trace_ok = 1;"); }
            let _ = writeln!(out, "            printf(\"PASS (%ldms)\\n\", __t_ms);");
            let _ = writeln!(out, "        }} else {{");
            let _ = writeln!(out, "            __test_failed++;");
            let _ = writeln!(out, "            printf(\"FAIL: %s (%ldms)\\n\", __gorget_test_fail_msg, __t_ms);");
            let _ = writeln!(out, "        }}");
        }
        if tracing {
            let _ = writeln!(out, "        if (__gorget_trace_fp) fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"test_end\\\",\\\"name\\\":\\\"{escaped}\\\",\\\"status\\\":\\\"%s\\\",\\\"duration_ms\\\":%ld}}\\n\", __test_trace_ok ? \"pass\" : \"fail\", __t_ms);");
        }
        let _ = writeln!(out, "    }}");
    }

    if module.has_suite_teardown {
        let _ = writeln!(out, "    __suite_teardown();");
    }

    let _ = writeln!(out, "    clock_gettime(CLOCK_MONOTONIC, &__total_end);");
    let _ = writeln!(out, "    long __total_ms = (__total_end.tv_sec - __total_start.tv_sec) * 1000 + (__total_end.tv_nsec - __total_start.tv_nsec) / 1000000;");
    let _ = writeln!(out, "    printf(\"\\n%d passed, %d failed (%ldms)\\n\", __test_passed, __test_failed, __total_ms);");
    let _ = writeln!(out, "    return __test_failed > 0 ? 1 : 0;");
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
        // Detect by structure: field or variant field with UNIT_TYPE
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
    if module.shared_types.is_empty() && module.weak_types.is_empty() {
        return;
    }
    out.push_str("\n/* ── Shared[T] / Weak[T] forward typedefs ── */\n");
    // Collect all element types that need a Shared__ typedef
    for elem_c in &module.shared_types {
        let _ = writeln!(out, "typedef GorgetShared* Shared__{elem_c};");
    }
    // Collect all element types that need a Weak__ typedef
    // (either from explicit Weak vars OR as companion for downgrade() return type)
    let mut weak_emitted: std::collections::HashSet<&str> = std::collections::HashSet::new();
    for elem_c in &module.shared_types {
        // Always emit Weak__T companion for every Shared__T (needed for downgrade() return type)
        if weak_emitted.insert(elem_c.as_str()) {
            let _ = writeln!(out, "typedef GorgetShared* Weak__{elem_c};");
        }
    }
    for elem_c in &module.weak_types {
        if weak_emitted.insert(elem_c.as_str()) {
            let _ = writeln!(out, "typedef GorgetShared* Weak__{elem_c};");
        }
    }
}

/// Emit Shared__T wrapper functions (new, clone, drop, get, strong_count, downgrade).
/// Typedefs were already emitted by emit_shared_weak_typedefs.
fn emit_shared_defs(out: &mut String, module: &Module) {
    if module.shared_types.is_empty() {
        return;
    }
    out.push_str("\n/* ── Shared[T] wrappers ── */\n");
    for elem_c in &module.shared_types {
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
    if module.weak_types.is_empty() {
        return;
    }
    out.push_str("\n/* ── Weak[T] wrappers ── */\n");
    for elem_c in &module.weak_types {
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
    if module.mutex_types.is_empty() {
        return;
    }
    out.push_str("\n/* ── Mutex[T] + Guard[T] wrappers ── */\n");
    for elem_c in &module.mutex_types {
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
        // Guard__T__drop(&self) → releases the mutex (called by RAII drop)
        let _ = writeln!(out,
            "static inline void {guard_name}__drop({guard_name}* self) {{ \
             gorget_guard_release(self); }}");
        out.push('\n');
    }
}

/// Emit RWLock__T + ReadGuard__T + WriteGuard__T typedef + wrapper functions.
fn emit_rwlock_defs(out: &mut String, module: &Module) {
    if module.rwlock_types.is_empty() {
        return;
    }
    out.push_str("\n/* ── RWLock[T] + ReadGuard[T] + WriteGuard[T] wrappers ── */\n");
    for elem_c in &module.rwlock_types {
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
        // WriteGuard__T__drop(&self)
        let _ = writeln!(out,
            "static inline void {write_guard}__drop({write_guard}* self) {{ \
             gorget_write_guard_release(self); }}");
        out.push('\n');
    }
}

/// Emit Thread__T typedef + join/id methods.
/// The internal `__GorgetThread__T` context struct is also emitted here.
/// Per-function spawn helpers (which reference GIR functions) are in emit_thread_helpers.
fn emit_thread_defs(out: &mut String, module: &Module) {
    if module.thread_types.is_empty() {
        return;
    }
    out.push_str("\n/* ── Thread[T] wrappers ── */\n");
    for elem_c in &module.thread_types {
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
    if module.thread_spawned_fns.is_empty() {
        return;
    }
    out.push_str("\n/* ── Thread spawn helpers ── */\n");
    for (fn_name, ret_type) in &module.thread_spawned_fns {
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
    if module.channel_types.is_empty() && module.spawned_fns.is_empty() {
        return;
    }

    out.push_str("\n/* ── Channel wrappers ── */\n");

    // Collect all unique Task return types for Task__T structs
    let mut task_ret_c_types: Vec<String> = Vec::new();

    for elem_c in &module.channel_types {
        let chan_name = format!("Channel__{elem_c}");
        // Typedef: Channel__T = GorgetChannel* (opaque pointer wrapper)
        let _ = writeln!(out, "typedef GorgetChannel* {chan_name};");
        // Constructor: Channel__T__new(cap)
        let _ = writeln!(out,
            "static inline {chan_name} {chan_name}__new(int64_t cap) {{ \
             return gorget_channel_new((size_t)cap, sizeof({elem_c})); }}");
        // send(&self, val)
        let _ = writeln!(out,
            "static inline void {chan_name}__send({chan_name}* self, {elem_c} val) {{ \
             gorget_channel_send(*self, &val); }}");
        // recv(&self) → T
        let _ = writeln!(out,
            "static inline {elem_c} {chan_name}__recv({chan_name}* self) {{ \
             {elem_c} __val; gorget_channel_recv(*self, &__val); return __val; }}");
        // close(&self)
        let _ = writeln!(out,
            "static inline void {chan_name}__close({chan_name}* self) {{ \
             gorget_channel_close(*self); }}");
        // poll_recv(&self, *out, waker) → bool
        let _ = writeln!(out,
            "static inline bool {chan_name}__poll_recv({chan_name}* self, {elem_c}* out, GorgetWaker* waker) {{ \
             return gorget_channel_poll_recv(*self, out, waker); }}");
        // clone: retain (increment refcount)
        let _ = writeln!(out,
            "static inline {chan_name} {chan_name}__clone({chan_name} self) {{ \
             return gorget_channel_retain(self); }}");
        // drop: release (decrement refcount, auto-close+free when last ref drops)
        let _ = writeln!(out,
            "static inline void {chan_name}__drop({chan_name}* self) {{ \
             gorget_channel_release(*self); }}");
        out.push('\n');
    }

    // Collect return types of spawned fns for Task__T emission
    for (_, _, ret_type) in &module.spawned_fns {
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

/// Emit per-spawned-function context structs, thread functions, and spawn/await helpers.
/// Called after GIR function forward declarations (so spawned functions are visible).
fn emit_spawn_helpers(out: &mut String, module: &Module) {
    if module.spawned_fns.is_empty() {
        return;
    }

    out.push_str("\n/* ── Spawn/await helpers ── */\n");

    for (fn_name, params, ret_type) in &module.spawned_fns {
        let ret_c = format_type(*ret_type, &module.type_registry);
        let is_void = ret_c == "void";
        let mangled_fn = mangle_name(fn_name);
        let ctx_name = format!("__SpawnCtx_{fn_name}");
        let task_name = if is_void {
            "Task__void".to_string()
        } else {
            format!("Task__{ret_c}")
        };

        // Context struct: { pthread_t thread; param_types params; ret_type result; }
        let _ = writeln!(out, "typedef struct {ctx_name} {{");
        out.push_str("    pthread_t thread;\n");
        for (param_name, param_type) in params {
            let param_c = format_type(*param_type, &module.type_registry);
            let _ = writeln!(out, "    {param_c} __{param_name};");
        }
        if !is_void {
            let _ = writeln!(out, "    {ret_c} result;");
        }
        let _ = writeln!(out, "}} {ctx_name};");

        // Thread function
        let _ = writeln!(out, "static void* __spawn_thread_{fn_name}(void* __arg) {{");
        let _ = writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)__arg;");
        let call_args = params.iter()
            .map(|(name, _)| format!("__ctx->__{name}"))
            .collect::<Vec<_>>()
            .join(", ");
        if is_void {
            let _ = writeln!(out, "    {mangled_fn}({call_args});");
        } else {
            let _ = writeln!(out, "    __ctx->result = {mangled_fn}({call_args});");
        }
        out.push_str("    return NULL;\n}\n");

        // Per-fn drop helper: joins thread + frees the specific __SpawnCtx type.
        // Called via the __drop function pointer embedded in Task__T.
        let _ = writeln!(out, "static void __spawn_drop_{fn_name}(void* __ptr) {{");
        let _ = writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)__ptr;");
        out.push_str("    pthread_join(__ctx->thread, NULL);\n");
        let _ = writeln!(out, "    GORGET_FREE(__ctx, sizeof({ctx_name}));");
        out.push_str("}\n");

        // Spawn function: allocates ctx, fills args, creates thread, returns Task
        let param_decls = params.iter()
            .map(|(name, type_id)| {
                let c = format_type(*type_id, &module.type_registry);
                format!("{c} {name}")
            })
            .collect::<Vec<_>>()
            .join(", ");
        let _ = writeln!(out, "static inline {task_name} __gorget_spawn_{fn_name}({param_decls}) {{");
        let _ = writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)GORGET_CALLOC(1, sizeof({ctx_name}));");
        for (param_name, param_type) in params {
            // For ref-counted types, emit a retain/clone call to increment the
            // refcount — the spawned thread gets its own reference.
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
        let _ = writeln!(out, "    pthread_create(&__ctx->thread, NULL, __spawn_thread_{fn_name}, __ctx);");
        let _ = writeln!(out, "    return ({task_name}){{.__task = __ctx, .__drop = __spawn_drop_{fn_name}}};");
        out.push_str("}\n");

        // Await function: joins thread, extracts result, frees ctx
        if is_void {
            let _ = writeln!(out, "static inline void __gorget_await_{fn_name}({task_name} task) {{");
        } else {
            let _ = writeln!(out, "static inline {ret_c} __gorget_await_{fn_name}({task_name} task) {{");
        }
        let _ = writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)task.__task;");
        out.push_str("    pthread_join(__ctx->thread, NULL);\n");
        if !is_void {
            let _ = writeln!(out, "    {ret_c} result = __ctx->result;");
        }
        let _ = writeln!(out, "    GORGET_FREE(__ctx, sizeof({ctx_name}));");
        if !is_void {
            out.push_str("    return result;\n");
        }
        out.push_str("}\n\n");
    }

    // Emit one Task__T__drop per unique Task type.
    // Called by the RAII drop elaborator; dispatches to the per-fn drop via __drop pointer.
    let mut emitted_task_drops: Vec<String> = Vec::new();
    for (_, _, ret_type) in &module.spawned_fns {
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
        // Use __attribute__((unused)) via a variable reference.
        let _ = writeln!(out, "static void (*__unused_{task_name}__drop)({task_name}*) __attribute__((unused)) = {task_name}__drop;");
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
                    if variant.fields.is_empty() {
                        // Unit variant — no data
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
    match name {
        // Unmonomorphized collection template names
        "Vector" | "GorgetArray" => Some("GorgetArray"),
        "Dict" | "HashMap" | "GorgetMap" | "GorgetDict" => Some("GorgetMap"),
        "Set" | "HashSet" | "GorgetSet" => Some("GorgetSet"),
        // Network types
        "Socket" => Some("GorgetSocket"),
        "UdpSocket" => Some("GorgetUdpSocket"),
        "UdpAddr" => Some("GorgetUdpAddr"),
        "UdpPacket" => Some("GorgetUdpPacket"),
        "TlsSocket" => Some("GorgetTlsSocket"),
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
        "TrackingAllocator" => Some("GorgetTrackingAllocator*"),
        "PoolAllocator" => Some("GorgetPoolAllocator*"),
        "TlsfAllocator" => Some("GorgetTlsfAllocator*"),
        _ => None,
    }
}

/// Map a collection type name to its C runtime type.
fn collection_type_alias(name: &str) -> Option<&'static str> {
    if name.starts_with("Vector__") { return Some("GorgetArray"); }
    if name.starts_with("Set__") { return Some("GorgetSet"); }
    // Dict and HashMap both map to GorgetMap in the runtime
    if name.starts_with("Dict__") || name.starts_with("HashMap__") {
        return Some("GorgetMap");
    }
    // NOTE: Shared__T, Mutex__T, Guard__T are NOT matched here.
    // Their typedefs are emitted by emit_shared_defs/emit_mutex_defs AFTER user struct defs.
    // Runtime types with different C names
    match name {
        "Socket" => Some("GorgetSocket"),
        "TlsSocket" => Some("GorgetTlsSocket"),
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
                                if ret != "int64_t" && ret != "void" {
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
                    } else if let Some(elem) = extract_element_type_from_collection(base_deref) {
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
            override_type.clone()
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
        if let Some(ref hint) = local.name_hint {
            let _ = writeln!(out, "    {c_type} _{local_id}; /* {hint} */");
        } else {
            let _ = writeln!(out, "    {c_type} _{local_id};");
        }
    }

    // Emit `goto bb0;` to start
    if is_main {
        out.push_str("    gorget_init_args(argc, argv);\n");
        // Trace init: open trace file at program start.
        if let Some(ref trace_path) = module.trace_filename {
            let escaped = trace_path.replace('\\', "\\\\").replace('"', "\\\"");
            let _ = writeln!(out, "    __gorget_trace_init(\"{escaped}\");");
        }
    } else if let Some(ref display_name) = func.display_name {
        // Trace entry: emit call event with function name, parameter values, and depth.
        if module.trace_filename.is_some() {
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
    let trace_then_blocks: std::collections::HashSet<u32> = if module.trace_filename.is_some() {
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
    let tracing = module.trace_filename.is_some();
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
            || type_id == U32_TYPE || type_id == U64_TYPE || type_id == CHAR_TYPE => "__gorget_trace_val_int",
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
        override_type.clone()
    } else if local_idx < func.locals.len() {
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
            // Closure packing: __Closure_N struct → GorgetClosure (escaped closure).
            // When a closure is returned from a function (return type int(int)),
            // the GIR local has FnPtr type (declared as GorgetClosure), and the RHS is
            // a __Closure_N StructInit. Heap-allocate the env and pack into GorgetClosure.
            // Detect via the local's GIR type (FnPtr) OR override (GorgetClosure from call result).
            let dst_local_idx = dst.local.0 as usize;
            let dst_is_escaped_closure = (dst_c_type == "GorgetClosure")
                || (dst_local_idx < func.locals.len()
                    && matches!(registry.get(func.locals[dst_local_idx].type_id), Some(GirType::FnPtr { .. })));
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
                    let _ = writeln!(out, "        {dst_str} = {val_str};");
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
            } else if *op == BinOp::Rem && (c_type == "double" || c_type == "float") {
                // Float modulo (destination is float): use fmod() instead of %
                let _ = writeln!(out, "        _{id} = fmod((double){lhs_str}, (double){rhs_str});", id = dst.0);
            } else if *op == BinOp::Rem && (lhs_effective == "double" || lhs_effective == "float") && (c_type == "int64_t" || c_type == "int32_t") {
                // Float operand with integer destination: cast to int first, then integer modulo
                let _ = writeln!(out, "        if ({rhs_str} == 0) {{ fprintf(stderr, \"gorget: division by zero\\n\"); exit(1); }}");
                let _ = writeln!(out, "        _{id} = (int64_t){lhs_str} % (int64_t){rhs_str};", id = dst.0);
            } else if matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul) && c_type == "int64_t" && !module.overflow_wrap {
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
            let lhs_effective = match lhs {
                Operand::Copy(p) | Operand::Move(p) =>
                    effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                _ => "int64_t".to_string(),
            };
            if lhs_effective == "Str" {
                // String comparison using gorget_str_eq
                match op {
                    CmpOp::Eq => {
                        let _ = writeln!(out, "        _{id} = gorget_str_eq({lhs_str}, {rhs_str});", id = dst.0);
                    }
                    CmpOp::Ne => {
                        let _ = writeln!(out, "        _{id} = !gorget_str_eq({lhs_str}, {rhs_str});", id = dst.0);
                    }
                    _ => {
                        let op_str = format_cmpop(*op);
                        let _ = writeln!(out, "        _{id} = gorget_str_cmp({lhs_str}, {rhs_str}) {op_str} 0;", id = dst.0);
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
                    format_cstr_fn_args(args, func, registry)
                } else {
                    format_args_with_coercion(args, func, registry, type_overrides, &c_name, all_functions)
                };
                let ret_cstr = returns_cstr(mapped_name);
                if let Some(dst_id) = dst {
                    let local_type = func.locals[dst_id.0 as usize].type_id;
                    if local_type == UNIT_TYPE && !type_overrides.contains_key(&(dst_id.0 as usize)) {
                        let _ = writeln!(out, "        {c_name}({args_str});");
                    } else {
                        let c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                        if ret_cstr && c_type == "GorgetString" {
                            let _ = writeln!(out, "        _{id} = gorget_string_adopt((char*){c_name}({args_str}));", id = dst_id.0);
                        } else if ret_cstr && c_type == "Str" {
                            let _ = writeln!(out, "        _{id} = gorget_str_from_cstr({c_name}({args_str}));", id = dst_id.0);
                        } else if c_type.starts_with("Option__") && !is_user_fn
                            && !c_name.ends_with("__upgrade") {
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
                    format_str_fn_args(args, func, registry)
                } else if is_cstr_fn {
                    format_cstr_fn_args(args, func, registry)
                } else {
                    format_str_fn_args(args, func, registry)
                };
                let ret_cstr = returns_cstr(func_name);
                if let Some(dst_id) = dst {
                    let local_type = func.locals[dst_id.0 as usize].type_id;
                    if local_type == UNIT_TYPE && !type_overrides.contains_key(&(dst_id.0 as usize)) {
                        let _ = writeln!(out, "        {func_name}({args_str});");
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
                        } else if ret_cstr && c_type == "Str" {
                            let _ = writeln!(out, "        _{id} = gorget_str_from_cstr({func_name}({args_str}));", id = dst_id.0);
                        } else if c_type.starts_with("Option__") && !is_user_fn_call {
                            // Option wrapping for stdlib functions — user functions already return Option
                            let _ = writeln!(out,
                                "        _{id} = ({{ __typeof__(_{id}.data.Some._0) __raw = {func_name}({args_str}); \
                                {c_type} __opt; if (__raw >= 0) {{ __opt.tag = 0; __opt.data.Some._0 = __raw; }} \
                                else {{ __opt.tag = 1; }} __opt; }});",
                                id = dst_id.0);
                        } else {
                            let _ = writeln!(out, "        _{id} = {func_name}({args_str});", id = dst_id.0);
                        }
                    }
                } else {
                    let _ = writeln!(out, "        {func_name}({args_str});");
                }
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
            if !fields.is_empty() {
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
                    // Try to extract element type from collection type name
                    let elem = extract_collection_elem_type(&base_type);
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
                    let _ = writeln!(out, "        _{id} = *({c_type}*)gorget_array_get(&{base_str}, {idx_str});", id = dst.0);
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
            let _ = writeln!(out, "        _{id} = ({c_type}){val};", id = dst.0);
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
            // Look up drop strategy from TypeDef
            let local_type = func.locals[place.local.0 as usize].type_id;
            let type_name_str = format_type(local_type, registry);
            // Get the full GIR name (e.g., "Vector__Tracked" instead of "GorgetArray")
            let gir_name = gir_type_name(local_type, registry);

            // Special handling for Box types: call inner Drop (if any) then free
            if let Some(inner_name) = type_name_str.strip_prefix("Box__") {
                // Check if this Box wraps a trait object (struct) vs concrete type (pointer)
                let is_trait_box = registry.get_type_def(&format!("{inner_name}_TraitObj")).is_some();
                if is_trait_box {
                    // Trait object Box: free the heap data via .data field
                    let _ = writeln!(out, "        free({place_str}.data);");
                } else {
                    // Concrete Box: call inner Drop (if any) then free the pointer
                    if let Some(inner_def) = registry.get_type_def(inner_name) {
                        if let DropStrategy::Custom(ref fn_name) = inner_def.metadata.drop_strategy {
                            let _ = writeln!(out, "        {fn_name}({place_str});");
                        }
                    }
                    let _ = writeln!(out, "        free({place_str});");
                }
            } else if let Some(elem_name) = gir_name.as_deref().and_then(extract_vector_elem_name) {
                // Vector type: per-element drops (if needed) then free the array
                if needs_drop_by_name(elem_name, registry) {
                    let elem_c_type = gir_to_c_type(elem_name);
                    let _ = writeln!(out, "        for (size_t __di = 0; __di < {place_str}.len; __di++) {{");
                    let _ = writeln!(
                        out,
                        "            {elem_c_type}* __de = ({elem_c_type}*)gorget_array_get(&{place_str}, __di);"
                    );
                    // Outer loop uses __di/__de (unnumbered); recursive call starts at depth 1
                    emit_drop_for_type_via_ptr(out, "__de", elem_name, registry, "            ", 1);
                    let _ = writeln!(out, "        }}");
                }
                let _ = writeln!(out, "        gorget_array_free(&{place_str});");
            } else {
                let strategy = lookup_drop_strategy(local_type, registry);
                match strategy {
                    DropStrategy::None => {
                        // No-op
                    }
                    DropStrategy::Trivial(ref fn_name) => {
                        let _ = writeln!(out, "        {fn_name}(&{place_str});");
                    }
                    DropStrategy::Custom(ref fn_name) => {
                        // Zero-check: if the struct was synthetically zeroed after being
                        // moved into a collection, skip the drop — nothing to free.
                        let _ = writeln!(out, "        if (memcmp(&{place_str}, &({type_name_str}){{0}}, sizeof({type_name_str})) != 0) {{");
                        let _ = writeln!(out, "            {fn_name}(&{place_str});");
                        // After custom drop, also drop fields that have their own drops
                        emit_field_drops(out, &place_str, local_type, registry, "            ", 0);
                        out.push_str("        }\n");
                    }
                    DropStrategy::Recursive => {
                        // Zero-check: same rationale as Custom above.
                        let _ = writeln!(out, "        if (memcmp(&{place_str}, &({type_name_str}){{0}}, sizeof({type_name_str})) != 0) {{");
                        emit_field_drops(out, &place_str, local_type, registry, "            ", 0);
                        out.push_str("        }\n");
                    }
                }
            }
        }

        Instruction::DropIfAlive { place } => {
            let place_str = format_place(place, registry);
            let local_type = func.locals[place.local.0 as usize].type_id;
            let type_name = format_type(local_type, registry);
            // Get the full GIR name (e.g., "Vector__Tracked" instead of "GorgetArray")
            let gir_name = gir_type_name(local_type, registry);

            // Special handling for Box types: null-check, call inner Drop, free
            if let Some(inner_name) = type_name.strip_prefix("Box__") {
                let is_trait_box = registry.get_type_def(&format!("{inner_name}_TraitObj")).is_some();
                if is_trait_box {
                    // Trait object Box: check .data for null, then free
                    let _ = writeln!(out, "        if ({place_str}.data != NULL) {{");
                    let _ = writeln!(out, "            free({place_str}.data);");
                    out.push_str("        }\n");
                } else {
                    // Concrete Box: null-check the pointer, call inner Drop, free
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
                // Vector type: check .data (null = zeroed/moved), then per-element drops + free
                let _ = writeln!(out, "        if ({place_str}.data != NULL) {{");
                if needs_drop_by_name(elem_name, registry) {
                    let elem_c_type = gir_to_c_type(elem_name);
                    let _ = writeln!(out, "            for (size_t __di = 0; __di < {place_str}.len; __di++) {{");
                    let _ = writeln!(
                        out,
                        "                {elem_c_type}* __de = ({elem_c_type}*)gorget_array_get(&{place_str}, __di);"
                    );
                    // Outer loop uses __di/__de (unnumbered); recursive call starts at depth 1
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
                        let _ = writeln!(out, "        if (memcmp(&{place_str}, &({type_name}){{0}}, sizeof({type_name})) != 0) {{");
                        let _ = writeln!(out, "            {fn_name}(&{place_str});");
                        out.push_str("        }\n");
                    }
                    DropStrategy::Custom(ref fn_name) => {
                        let _ = writeln!(out, "        if (memcmp(&{place_str}, &({type_name}){{0}}, sizeof({type_name})) != 0) {{");
                        let _ = writeln!(out, "            {fn_name}(&{place_str});");
                        emit_field_drops(out, &place_str, local_type, registry, "            ", 0);
                        out.push_str("        }\n");
                    }
                    DropStrategy::Recursive => {
                        let _ = writeln!(out, "        if (memcmp(&{place_str}, &({type_name}){{0}}, sizeof({type_name})) != 0) {{");
                        emit_field_drops(out, &place_str, local_type, registry, "            ", 0);
                        out.push_str("        }\n");
                    }
                }
            }
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
                // For main, return _0
                out.push_str("        return _0;\n");
            } else {
                match value {
                    Operand::Constant(Constant::Unit) => {
                        out.push_str("        return;\n");
                    }
                    _ => {
                        let val_str = format_operand(value, func, registry);
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
    if type_id == CHAR_TYPE {
        return "uint32_t".to_string();
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
        | "path_join" | "gorget_path_join" => Some("Str"),
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
        "ord" | "gorget_char_ord" => Some("int64_t"),
        "int_to_str" | "gorget_int_to_str" | "float_to_str" | "gorget_float_to_str"
        | "char_to_str" | "gorget_char_to_str"
        | "bool_to_str" | "gorget_bool_to_str" => Some("Str"),
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
        "Arena__bytes_used" | "gorget_arena_bytes_used" => Some("int64_t"),
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
        "int64_t__default" | "int__default" => return Some("int64_t"),
        "double__default" | "float__default" => return Some("double"),
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
            "char_at" => Some("uint32_t"),
            "len" | "byte_len" | "count" | "find" | "hash" => Some("int64_t"),
            "index_of" => Some("Option__int64_t"),
            "contains" | "starts_with" | "ends_with" | "eq" => Some("bool"),
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
            // Vector.get returns Option[T] (bounds-checked safe access)
            "get" if type_prefix.starts_with("Vector") => {
                if let Some(elem) = extract_element_type_from_collection(type_prefix) {
                    let option_type = format!("Option__{elem}");
                    return Some(Box::leak(option_type.into_boxed_str()));
                }
                return None;
            }
            // Methods that return the element type — extract from collection name
            "pop" | "first" | "last" | "remove" | "get" => {
                if let Some(elem) = extract_element_type_from_collection(type_prefix) {
                    return Some(Box::leak(elem.into_boxed_str()));
                }
                return None;
            }
            // Methods returning collections — return same collection type
            "filter" | "keys" | "values" | "items"
            | "sorted" | "reversed" | "unique" | "flatten"
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
    if !matches!(method, "filter" | "map" | "fold" | "reduce" | "enumerate" | "any" | "all" | "each" | "find" | "count" | "get_or_put" | "keys" | "values") {
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
        return Some(String::new()); // void call — no result needed
    };

    let mut out = String::new();

    match method {
        "filter" => {
            // args: [collection_ref, closure]
            if args.len() < 2 { return None; }
            let closure = format_operand(&args[1], func, registry);
            let closure_type = match &args[1] {
                Operand::Copy(p) | Operand::Move(p) =>
                    effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                _ => "void*".to_string(),
            };
            let call_fn = format!("{closure_type}__call");
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
                    if ({call_fn}(&{closure}, __key, __val)) gorget_map_put(&__result, &__key, &__val); \
                    }} __result; }});");
            } else if is_set {
                // Set filter: iterate by hash-table states, create new set
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetSet __src = {coll_val}; \
                    GorgetSet __result = gorget_set_new(sizeof({elem_type})); \
                    for (size_t __i = 0; __i < __src.cap; __i++) {{ \
                    if (__src.states[__i] != 1) continue; \
                    {elem_type} __elem = *({elem_type}*)((char*)__src.keys + __i * __src.key_size); \
                    if ({call_fn}(&{closure}, __elem)) gorget_set_add(&__result, &__elem); \
                    }} __result; }});");
            } else {
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                    GorgetArray __result = gorget_array_new(sizeof({elem_type})); \
                    for (size_t __i = 0; __i < __src.len; __i++) {{ \
                    {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                    if ({call_fn}(&{closure}, __elem)) gorget_array_push(&__result, &__elem); \
                    }} __result; }});");
            }
        }
        "map" => {
            if args.len() < 2 { return None; }
            let closure = format_operand(&args[1], func, registry);
            let closure_type = match &args[1] {
                Operand::Copy(p) | Operand::Move(p) =>
                    effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                _ => "void*".to_string(),
            };
            let call_fn = format!("{closure_type}__call");
            // For map, output type might differ from input. Use __typeof__ to infer.
            use std::fmt::Write;
            let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                {elem_type} __first_elem = GORGET_ARRAY_AT({elem_type}, __src, 0); \
                __typeof__({call_fn}(&{closure}, __first_elem)) __map_out; \
                GorgetArray __result = gorget_array_new(sizeof(__map_out)); \
                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                __map_out = {call_fn}(&{closure}, __elem); \
                gorget_array_push(&__result, &__map_out); \
                }} __result; }});");
        }
        "fold" => {
            // args: [collection_ref, init_value, closure]
            if args.len() < 3 { return None; }
            let init = format_operand(&args[1], func, registry);
            let closure = format_operand(&args[2], func, registry);
            let closure_type = match &args[2] {
                Operand::Copy(p) | Operand::Move(p) =>
                    effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                _ => "void*".to_string(),
            };
            let call_fn = format!("{closure_type}__call");
            let acc_type = effective_c_type(dst.unwrap().0 as usize, func, registry, type_overrides);
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
                    __acc = {call_fn}(&{closure}, __acc, __key, __val); \
                    }} __acc; }});");
            } else if is_set {
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetSet __src = {coll_val}; \
                    {acc_type} __acc = {init}; \
                    for (size_t __i = 0; __i < __src.cap; __i++) {{ \
                    if (__src.states[__i] != 1) continue; \
                    {elem_type} __elem = *({elem_type}*)((char*)__src.keys + __i * __src.key_size); \
                    __acc = {call_fn}(&{closure}, __acc, __elem); \
                    }} __acc; }});");
            } else {
                let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                    {acc_type} __acc = {init}; \
                    for (size_t __i = 0; __i < __src.len; __i++) {{ \
                    {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                    __acc = {call_fn}(&{closure}, __acc, __elem); \
                    }} __acc; }});");
            }
        }
        "reduce" => {
            if args.len() < 2 { return None; }
            let closure = format_operand(&args[1], func, registry);
            let closure_type = match &args[1] {
                Operand::Copy(p) | Operand::Move(p) =>
                    effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                _ => "void*".to_string(),
            };
            let call_fn = format!("{closure_type}__call");
            use std::fmt::Write;
            let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                if (__src.len == 0) gorget_panic(\"reduce() called on empty array\"); \
                {elem_type} __acc = GORGET_ARRAY_AT({elem_type}, __src, 0); \
                for (size_t __i = 1; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                __acc = {call_fn}(&{closure}, __acc, __elem); \
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
        "any" => {
            if args.len() < 2 { return None; }
            let closure = format_operand(&args[1], func, registry);
            let closure_type = match &args[1] {
                Operand::Copy(p) | Operand::Move(p) =>
                    effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                _ => "void*".to_string(),
            };
            let call_fn = format!("{closure_type}__call");
            use std::fmt::Write;
            let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                bool __any_result = false; \
                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                if ({call_fn}(&{closure}, __elem)) {{ __any_result = true; break; }} \
                }} __any_result; }});");
        }
        "all" => {
            if args.len() < 2 { return None; }
            let closure = format_operand(&args[1], func, registry);
            let closure_type = match &args[1] {
                Operand::Copy(p) | Operand::Move(p) =>
                    effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                _ => "void*".to_string(),
            };
            let call_fn = format!("{closure_type}__call");
            use std::fmt::Write;
            let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                bool __all_result = true; \
                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                if (!{call_fn}(&{closure}, __elem)) {{ __all_result = false; break; }} \
                }} __all_result; }});");
        }
        "each" => {
            if args.len() < 2 { return None; }
            let closure = format_operand(&args[1], func, registry);
            let closure_type = match &args[1] {
                Operand::Copy(p) | Operand::Move(p) =>
                    effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                _ => "void*".to_string(),
            };
            let call_fn = format!("{closure_type}__call");
            use std::fmt::Write;
            let _ = writeln!(out, "        {{ GorgetArray __src = {coll_val}; \
                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                {call_fn}(&{closure}, __elem); \
                }} }}");
        }
        "find" => {
            if args.len() < 2 { return None; }
            let closure = format_operand(&args[1], func, registry);
            let closure_type = match &args[1] {
                Operand::Copy(p) | Operand::Move(p) =>
                    effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                _ => "void*".to_string(),
            };
            let call_fn = format!("{closure_type}__call");
            // find returns Option[T]
            let option_type = format!("Option__{elem_type}");
            use std::fmt::Write;
            let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                {option_type} __find_result = {{ .tag = 1 }}; \
                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                if ({call_fn}(&{closure}, __elem)) {{ __find_result.tag = 0; __find_result.data.Some._0 = __elem; break; }} \
                }} __find_result; }});");
        }
        "count" => {
            if args.len() < 2 { return None; }
            let closure = format_operand(&args[1], func, registry);
            let closure_type = match &args[1] {
                Operand::Copy(p) | Operand::Move(p) =>
                    effective_c_type(p.local.0 as usize, func, registry, type_overrides),
                _ => "void*".to_string(),
            };
            let call_fn = format!("{closure_type}__call");
            use std::fmt::Write;
            let _ = writeln!(out, "        {dst_str} = ({{ GorgetArray __src = {coll_val}; \
                int64_t __count = 0; \
                for (size_t __i = 0; __i < __src.len; __i++) {{ \
                {elem_type} __elem = GORGET_ARRAY_AT({elem_type}, __src, __i); \
                if ({call_fn}(&{closure}, __elem)) __count++; \
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
    let type_name = name.strip_suffix("__new").unwrap_or(name);
    let c_type = collection_type_alias(type_name);

    match c_type {
        Some("GorgetArray") => {
            let elem_c_type = extract_collection_elem_type(name);
            Some(format!("        _{dst_id} = gorget_array_new(sizeof({elem_c_type}));\n"))
        }
        Some("GorgetMap") => {
            // Extract key and value types from Dict__K__V or HashMap__K__V
            let (key_type, val_type) = extract_map_kv_types(type_name);
            // Use Str-aware constructors for Str keys (content-based hash/compare)
            if key_type == "Str" {
                let ctor = if type_name.starts_with("Dict__") { "gorget_dict_new_str" } else { "gorget_map_new_str" };
                Some(format!("        _{dst_id} = {ctor}(sizeof({val_type}));\n"))
            } else {
                let ctor = if type_name.starts_with("Dict__") { "gorget_dict_new" } else { "gorget_map_new" };
                Some(format!("        _{dst_id} = {ctor}(sizeof({key_type}), sizeof({val_type}));\n"))
            }
        }
        Some("GorgetSet") => {
            let elem_c_type = extract_collection_elem_type(name);
            Some(format!("        _{dst_id} = gorget_set_new(sizeof({elem_c_type}));\n"))
        }
        _ if type_name == "String" => {
            Some(format!("        _{dst_id} = gorget_string_new(\"\");\n"))
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

/// Inline method emit types for methods that don't map to a single runtime function.
enum InlineMethod {
    /// Pop: *(T*)(arr.data + arr.elem_size * --arr.len)
    Pop,
    /// Sort: qsort with type-specific comparator (in-place)
    Sort,
    /// Sorted: clone + sort (returns new array)
    Sorted,
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
    /// Char methods
    CharClassify(&'static str), // C function name (isalpha, isdigit, etc.)
    CharToUpper,
    CharToLower,
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
                runtime_fn: "gorget_array_slice", pass_by_ptr: false,
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
            _ => None,
        };
    }

    // Try Set (GorgetSet) patterns
    if func_name.starts_with("Set__") {
        let method = extract_trailing_method(func_name, "Set__");
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
                runtime_fn: "gorget_set_clear", pass_by_ptr: false,
                has_return: false, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "len" => Some(CollectionMethodCall {
                runtime_fn: "gorget_set_len", pass_by_ptr: false,
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
            "put" | "set" => Some(CollectionMethodCall {
                runtime_fn: "gorget_map_put", pass_by_ptr: true,
                has_return: false, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "get" => Some(CollectionMethodCall {
                runtime_fn: "gorget_map_get", pass_by_ptr: true,
                has_return: true, needs_deref_cast: true, field_access: None,
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
                runtime_fn: "gorget_map_len", pass_by_ptr: false,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "clear" => Some(CollectionMethodCall {
                runtime_fn: "gorget_map_clear", pass_by_ptr: false,
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
                runtime_fn: "gorget_map_get", pass_by_ptr: true,
                has_return: true, needs_deref_cast: true, field_access: None,
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
                runtime_fn: "gorget_map_len", pass_by_ptr: false,
                has_return: true, needs_deref_cast: false, field_access: None,
                ..Default::default()
            }),
            "clear" => Some(CollectionMethodCall {
                runtime_fn: "gorget_map_clear", pass_by_ptr: false,
                has_return: false, needs_deref_cast: false, field_access: None,
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
    let ret_type = if let Some(dst_id) = dst {
        effective_c_type(dst_id.0 as usize, func, registry, type_overrides)
    } else {
        "void".to_string()
    };
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
    // Helper: extract const char* from arg (handles Str vs literal)
    let extract_cstr_arg = |args: &[Operand], func: &Function, registry: &TypeRegistry| -> String {
        if args.is_empty() { return "\"\"".to_string(); }
        match &args[0] {
            Operand::Constant(Constant::Str(_)) => {
                // String literal — already a const char* in C
                format_operand(&args[0], func, registry)
            }
            Operand::Copy(p) | Operand::Move(p) => {
                let s = format_operand(&args[0], func, registry);
                // Check if it's a Str or a raw string
                let idx = p.local.0 as usize;
                let c_type = format_type(func.locals[idx].type_id, registry);
                if c_type == "Str" || c_type == "GorgetString" {
                    format!("{s}.data")
                } else {
                    s
                }
            }
            _ => format_operand(&args[0], func, registry),
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
                let arg_str = extract_cstr_arg(args, func, registry);
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
                    "        _{id} = ({{ GorgetParseIntResult __pr = gorget_try_parse_int({arg_str}); \
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
                let arg_str = extract_cstr_arg(args, func, registry);
                let _ = writeln!(out,
                    "        _{id} = ({{ GorgetParseFloatResult __pr = gorget_try_parse_float({arg_str}); \
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
                let arg_str = extract_cstr_arg(args, func, registry);
                // bool parse: "true"→true, "false"→false, else None
                let _ = writeln!(out,
                    "        _{id} = ({{ Option__bool __opt; \
                    if (strcmp({arg_str}, \"true\") == 0) {{ __opt.tag = 0; __opt.data.Some._0 = 1; }} \
                    else if (strcmp({arg_str}, \"false\") == 0) {{ __opt.tag = 0; __opt.data.Some._0 = 0; }} \
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
        "double__default" | "float__default" => {
            if let Some(dst_id) = dst {
                let _ = writeln!(out, "        _{id} = 0.0;", id = dst_id.0);
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
    // Socket
    if func_name.starts_with("gorget_socket_") || func_name == "socket_connect"
        || func_name.starts_with("Socket__") {
        return Some("gorget_socket_last_error");
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

fn try_inline_method(func_name: &str) -> Option<InlineMethod> {
    // Vector pop/sort
    if func_name.starts_with("Vector__") || func_name.starts_with("GorgetArray__") {
        let method = extract_trailing_method(func_name, "Vector__");
        return match method {
            "pop" => Some(InlineMethod::Pop),
            "sort" => Some(InlineMethod::Sort),
            "sorted" => Some(InlineMethod::Sorted),
            _ => None,
        };
    }
    // Set operations
    if func_name.starts_with("Set__") || func_name.starts_with("HashSet__") {
        let method = extract_trailing_method(func_name, "Set__");
        return match method {
            "union" => Some(InlineMethod::SetUnion),
            "intersection" => Some(InlineMethod::SetIntersection),
            "difference" => Some(InlineMethod::SetDifference),
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
    // Char methods
    if func_name.starts_with("char__") {
        let method = &func_name[6..]; // strip "char__"
        return match method {
            "is_alpha" => Some(InlineMethod::CharClassify("isalpha")),
            "is_digit" => Some(InlineMethod::CharClassify("isdigit")),
            "is_alphanumeric" => Some(InlineMethod::CharClassify("isalnum")),
            "is_whitespace" => Some(InlineMethod::CharClassify("isspace")),
            "is_hex_digit" => Some(InlineMethod::CharClassify("isxdigit")),
            "is_upper" => Some(InlineMethod::CharClassify("isupper")),
            "is_lower" => Some(InlineMethod::CharClassify("islower")),
            "is_ascii" => Some(InlineMethod::CharClassify("isascii")),
            "to_upper" => Some(InlineMethod::CharToUpper),
            "to_lower" => Some(InlineMethod::CharToLower),
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
            "is_err" => Some(InlineMethod::ResultIsErr),
            "unwrap_or" => Some(InlineMethod::ResultUnwrapOr),
            "unwrap_err" => Some(InlineMethod::ResultUnwrapErr),
            "map" => Some(InlineMethod::ResultMap),
            "and_then" => Some(InlineMethod::ResultAndThen),
            "map_err" => Some(InlineMethod::ResultMapErr),
            "or" => Some(InlineMethod::ResultOr),
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
            // pop: *(T*)(arr.data + arr.elem_size * --arr.len)
            if let Some(dst_id) = dst {
                let c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let _ = writeln!(out,
                    "        _{id} = *({c_type}*)((char*){self_str}.data + {self_str}.elem_size * --{self_str}.len);",
                    id = dst_id.0);
            }
        }
        InlineMethod::Sort => {
            let _ = writeln!(out,
                "        qsort({self_str}.data, {self_str}.len, {self_str}.elem_size, gorget_generic_compare);");
        }
        InlineMethod::Sorted => {
            // sorted: clone + sort (returns new array)
            if let Some(dst_id) = dst {
                let self_addr = addr_self(&self_raw, self_ptr);
                let _c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let _ = writeln!(out,
                    "        _{id} = gorget_array_clone({self_addr}); \
                    qsort(_{id}.data, _{id}.len, _{id}.elem_size, gorget_generic_compare);",
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
                let other = if args.len() > 1 {
                    format_operand(&args[1], func, registry)
                } else { "/*no other*/".to_string() };
                let _c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let _ = writeln!(out,
                    "        _{id} = gorget_set_clone(&{self_str}); \
                    for (size_t __i = 0; __i < {other}.cap; __i++) {{ \
                    if ({other}.states[__i] == 1) {{ \
                    gorget_set_add(&_{id}, (char*){other}.keys + __i * {other}.key_size); \
                    }} }}",
                    id = dst_id.0);
            }
        }
        InlineMethod::SetIntersection => {
            if let Some(dst_id) = dst {
                let other = if args.len() > 1 {
                    format_operand(&args[1], func, registry)
                } else { "/*no other*/".to_string() };
                let _c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let _ = writeln!(out,
                    "        _{id} = gorget_set_new({self_str}.key_size); \
                    for (size_t __i = 0; __i < {self_str}.cap; __i++) {{ \
                    if ({self_str}.states[__i] == 1) {{ \
                    const void* __k = (char*){self_str}.keys + __i * {self_str}.key_size; \
                    if (gorget_set_contains(&{other}, __k)) gorget_set_add(&_{id}, __k); \
                    }} }}",
                    id = dst_id.0);
            }
        }
        InlineMethod::SetDifference => {
            if let Some(dst_id) = dst {
                let other = if args.len() > 1 {
                    format_operand(&args[1], func, registry)
                } else { "/*no other*/".to_string() };
                let _c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let _ = writeln!(out,
                    "        _{id} = gorget_set_new({self_str}.key_size); \
                    for (size_t __i = 0; __i < {self_str}.cap; __i++) {{ \
                    if ({self_str}.states[__i] == 1) {{ \
                    const void* __k = (char*){self_str}.keys + __i * {self_str}.key_size; \
                    if (!gorget_set_contains(&{other}, __k)) gorget_set_add(&_{id}, __k); \
                    }} }}",
                    id = dst_id.0);
            }
        }
        InlineMethod::CharClassify(c_func) => {
            // char method (is_alpha, is_digit, etc.) → C ctype function
            if let Some(dst_id) = dst {
                let _ = writeln!(out,
                    "        _{id} = {c_func}((int){self_str});",
                    id = dst_id.0);
            }
        }
        InlineMethod::CharToUpper => {
            if let Some(dst_id) = dst {
                let _ = writeln!(out,
                    "        _{id} = (uint32_t)toupper((int){self_str});",
                    id = dst_id.0);
            }
        }
        InlineMethod::CharToLower => {
            if let Some(dst_id) = dst {
                let _ = writeln!(out,
                    "        _{id} = (uint32_t)tolower((int){self_str});",
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
                let _ = writeln!(out,
                    "        _{id} = ({{ {val_type}* __gop = ({val_type}*)gorget_map_get(&{self_str}, {key_ref}); \
                    __gop ? *__gop : {default}; }});",
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
                let other = if args.len() > 1 { format_operand(&args[1], func, registry) } else { self_str.clone() };
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
                    else {{ __rat.tag = 1; __rat.data.Error._0 = {self_str}.data.Error._0; }} \
                    }} \
                    else {{ __rat.tag = 1; __rat.data.Error._0 = {self_str}.data.Error._0; }} __rat; }});",
                    id = dst_id.0);
            }
        }
        InlineMethod::ResultMapErr => {
            // map_err(f): if Err, return Err(f(error)); else return Ok as-is
            // Use destination type because map_err may change the Error type
            if let Some(dst_id) = dst {
                let dst_c_type = effective_c_type(dst_id.0 as usize, func, registry, type_overrides);
                let (closure_str, call_fn) = closure_call_info(&args, func, registry, type_overrides, 1);
                let _ = writeln!(out,
                    "        _{id} = ({{ {dst_c_type} __rme; \
                    if ({self_str}.tag == 0) {{ __rme.tag = 0; __rme.data.Ok._0 = {self_str}.data.Ok._0; }} \
                    else {{ __rme.tag = 1; __rme.data.Error._0 = {call_fn}(&{closure_str}, {self_str}.data.Error._0); }} __rme; }});",
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
                if clone_ops.is_empty() {
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
    if rewrite.runtime_fn == "__INLINE_ARRAY_REMOVE__" {
        // Vector.remove(idx) → get element at idx, then remove, return element
        let arr_ref = addr_self(&self_str, self_ptr);
        let idx_str = if args.len() > 1 {
            format_operand(&args[1], func, registry)
        } else {
            "0".to_string()
        };
        if let Some(dst_id) = dst {
            let c_type = collection_element_c_type(func, dst_id.0 as usize, registry, original_name, type_overrides);
            let _ = writeln!(out, "        _{id} = *({c_type}*)gorget_array_get({arr_ref}, {idx_str});",
                id = dst_id.0);
            let _ = writeln!(out, "        gorget_array_remove({arr_ref}, {idx_str});");
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
                Constant::Char(_) => "uint32_t".to_string(),
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
                    let _ = writeln!(out, "        gorget_string_push_char({self_ref}, '\\n');");
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
                    let _ = writeln!(out, "        gorget_string_push_char({self_ref}, '\\n');");
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
    // Str methods take self by value; dereference if self is a pointer
    let is_str_method = rewrite.runtime_fn.starts_with("gorget_str_");
    if is_str_method {
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

    // Remaining args
    for (arg_idx, arg) in args.iter().skip(1).enumerate() {
        let val = format_operand(arg, func, registry);
        // For set/insert: arg[0] = index (value), arg[1] = element (pointer)
        let should_ptr = if is_index_plus_elem { arg_idx == 1 } else { rewrite.pass_by_ptr };
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
                            Constant::Char(_) => "uint32_t".to_string(),
                            _ => "int64_t".to_string(),
                        },
                    };
                    // For Str, use gorget_str_from_literal to set both .data and .len
                    if let Operand::Constant(Constant::Str(s)) = arg {
                        call_args.push(format!("&(Str){{ .data = \"{}\", .len = {} }}",
                            escape_c_string(s), s.len()));
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

    if rewrite.has_return {
        if let Some(dst_id) = dst {
            let c_type = collection_element_c_type(func, dst_id.0 as usize, registry, original_name, type_overrides);
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
        Constant::I32(n) => format!("{n}"),
        Constant::I64(n) => {
            if *n > i32::MAX as i64 || *n < i32::MIN as i64 {
                format!("{n}LL")
            } else {
                format!("{n}")
            }
        }
        Constant::U8(n) => format!("(uint8_t){n}"),
        Constant::U16(n) => format!("(uint16_t){n}"),
        Constant::U32(n) => format!("{n}u"),
        Constant::Char(n) => format!("{n}u"),
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
fn format_str_fn_args(args: &[Operand], func: &Function, registry: &TypeRegistry) -> String {
    let empty = std::collections::HashMap::new();
    format_args_inner(args, func, registry, FormatArgsMode::StrFn, &empty)
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
                        parts.push(arg_str);
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
