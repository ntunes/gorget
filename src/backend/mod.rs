pub mod c;
pub mod c_lir;

use crate::lir::LirModule;

/// Map Gorget stdlib function names to their C runtime names.
///
/// This is the canonical mapping shared by both the GIR C backend and the LIR
/// lowerer. Returns `None` for names that are not stdlib functions.
pub fn map_stdlib_name(name: &str) -> Option<&'static str> {
    let mapped = match name {
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
        "regex_compile" | "regex_compile_with" => "gorget_regex_compile",
        "regex_is_match" => "gorget_regex_is_match_pat",
        "regex_find" => "gorget_regex_find_pat",
        "regex_match" => "gorget_regex_match",
        "regex_find_all" => "gorget_regex_find_all",
        "regex_replace" => "gorget_regex_replace_pat",
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
        // Regex methods
        "Regex__is_match" => "gorget_regex_is_match",
        "Regex__find" => "gorget_regex_find",
        "Regex__find_at" => "gorget_regex_find_at",
        "Regex__find_all" => "gorget_regex_find_all",
        "Regex__replace" => "gorget_regex_replace",
        "Regex__split" | "Regex__splitn" => "gorget_regex_split",
        "Regex__fullmatch" => "gorget_regex_fullmatch",
        "Regex__groups" => "gorget_regex_groups",
        "Regex__free" => "gorget_regex_free",
        "Match__group" => "gorget_regex_match_group",
        "Match__group_by_name" => "gorget_regex_match_group_by_name",
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
        "ServerSocket__local_port" => "gorget_server_socket_local_port",
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
        // File methods
        "File__create" => "gorget_file_create",
        "File__open" => "gorget_file_open",
        "File__write" => "gorget_file_write_handle",
        "File__read_all" => "gorget_file_read_all",
        "File__close" => "gorget_file_close",
        // Str methods
        "Str__hash" => "gorget_str_hash",
        "Str__slice" => "gorget_str_slice",
        // Bare Str method names emitted by GIR without type prefix
        "chars" => "gorget_str_chars",
        "bytes" => "gorget_str_bytes",
        // SDL
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
        "sdl_set_relative_mouse_mode" => "gorget_sdl_set_relative_mouse_mode",
        "sdl_show_cursor" => "gorget_sdl_show_cursor",
        "sdl_get_relative_mouse_state" => "gorget_sdl_get_relative_mouse_state",
        "sdl_warp_mouse_in_window" => "gorget_sdl_warp_mouse_in_window",
        "sdl_get_mouse_state" => "gorget_sdl_get_mouse_state",
        "sdl_start_text_input" => "gorget_sdl_start_text_input",
        "sdl_stop_text_input" => "gorget_sdl_stop_text_input",
        _ => return None,
    };
    Some(mapped)
}

/// Output from a backend code generation pass.
pub struct CodegenOutput {
    /// The generated source code (C, LLVM IR, WASM text, etc.).
    pub code: String,
    /// File extension for the output (e.g., "c", "ll", "wat").
    pub extension: &'static str,
}

/// Trait for LIR-consuming backends.
///
/// All backends consume an optimized `LirModule` and produce source code
/// that can be compiled to a binary by an external toolchain.
pub trait Backend {
    /// Human-readable backend name (e.g., "c-lir", "llvm").
    fn name(&self) -> &str;

    /// Generate source code from an LIR module.
    fn generate(&self, module: &LirModule) -> CodegenOutput;
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

/// Split a full compiled C string into host + guest for hot-reload mode.
///
/// - Guest: full code minus main(), plus state hash constant + exported wrappers.
/// - Host: runtime/type section only + a dlopen-based main().
pub fn generate_hot_reload_split(
    full_c: &str,
    state_type: &str,
    state_hash: u64,
    has_reload: bool,
    hr_opts: Option<&HotReloadOpts>,
) -> (String, String) {
    // ── Guest code ──
    let mut guest = String::with_capacity(full_c.len() + 1024);
    // Remove main() by finding a line starting with "int main(" and ending with ") {"
    // then bracket-match to remove the body.
    let main_pos = full_c.find("\nint main(").map(|p| p + 1)
        .or_else(|| if full_c.starts_with("int main(") { Some(0) } else { None });
    if let Some(main_pos) = main_pos {
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
        host.push_str(c::c_runtime::RUNTIME_PREAMBLE);
        host.push_str(c::c_runtime::PANIC_NORMAL);
        host.push_str(c::c_runtime::RUNTIME_CORE);
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
