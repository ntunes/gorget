/// Everything before the panic helper — includes, types, string helpers.
pub const RUNTIME_PREAMBLE: &str = include_str!("runtime/runtime_preamble.c");

/// Arena allocator — bump allocator with overflow blocks.
pub const RUNTIME_ARENA_ALLOC: &str = include_str!("runtime/runtime_arena_alloc.c");

/// Tracking allocator — wraps another allocator with statistics.
pub const RUNTIME_TRACKING_ALLOC: &str = include_str!("runtime/runtime_tracking_alloc.c");

/// Pool allocator — fixed-size block allocation with free-list reuse.
pub const RUNTIME_POOL_ALLOC: &str = include_str!("runtime/runtime_pool_alloc.c");

/// TLSF allocator — O(1) alloc/free with low fragmentation.
pub const RUNTIME_TLSF_ALLOC: &str = include_str!("runtime/runtime_tlsf_alloc.c");

/// Fixed-buffer allocator — bump allocator over a fixed buffer.
pub const RUNTIME_FIXEDBUF_ALLOC: &str = include_str!("runtime/runtime_fixedbuf_alloc.c");

/// Fallback allocator — tries primary, falls back to secondary.
pub const RUNTIME_FALLBACK_ALLOC: &str = include_str!("runtime/runtime_fallback_alloc.c");

/// String types, operations, and UTF-8 utilities.
pub const RUNTIME_STRING: &str = include_str!("runtime/runtime_string.c");

/// Unicode case mapping tables, upper/lower/alpha/whitespace, search, and string methods.
pub const RUNTIME_STRING_EXTENDED: &str = include_str!("runtime/runtime_string_extended.c");

/// Base string operations — concatenation, conversion, cstr interop.
pub const RUNTIME_STRING_BASE_OPS: &str = include_str!("runtime/runtime_string_base_ops.c");

/// Alloc-report — atexit allocation summary (test/bench mode only).
pub const RUNTIME_ALLOC_REPORT: &str = include_str!("runtime/runtime_alloc_report.c");

/// Clone-stats — atexit runtime clone/alloc counter report for `--clone-stats`.
/// Single-line parseable format so scripts can diff values across runs. Includes
/// peak RSS (on Linux via /proc/self/status VmHWM) so no external `time -v` is
/// needed to capture the memory footprint.
pub const RUNTIME_CLONE_STATS: &str = include_str!("runtime/runtime_clone_stats.c");

/// Normal panic handler — exits the process.
pub const PANIC_NORMAL: &str = include_str!("runtime/panic_normal.c");

/// Test panic handler — uses setjmp/longjmp to recover from assertion failures.
pub const PANIC_TEST: &str = include_str!("runtime/panic_test.c");

/// Async runtime constants (polling protocol).
pub const ASYNC_RUNTIME: &str = include_str!("runtime/async_runtime.c");

/// GorgetTask struct + poll constants, shared by all scheduler backends.
pub const TASK_COMMON: &str = include_str!("runtime/task_common.c");

/// M:N thread-pool executor with work-stealing (default scheduler).
pub const SCHEDULER_POOL_RUNTIME: &str = include_str!("runtime/scheduler_pool_runtime.c");

/// 1:1 OS thread per spawn.
pub const SCHEDULER_THREAD_RUNTIME: &str = include_str!("runtime/scheduler_thread_runtime.c");

/// Synchronous inline execution (no threads).
pub const SCHEDULER_INLINE_RUNTIME: &str = include_str!("runtime/scheduler_inline_runtime.c");

/// N:1 cooperative event loop (single-threaded).
pub const SCHEDULER_SINGLE_RUNTIME: &str = include_str!("runtime/scheduler_single_runtime.c");

/// Legacy alias — kept for backward compat with any code referencing EXECUTOR_RUNTIME.
pub const EXECUTOR_RUNTIME: &str = "";

/// Main-thread waker for event-driven async main loop.
/// Only emitted when `has_spawn` is true (needs pthreads).
pub const MAIN_WAKER_RUNTIME: &str = include_str!("runtime/main_waker_runtime.c");

/// Bounded MPMC channel runtime (mutex + condvar ring buffer) with async waker support.
pub const CHANNEL_RUNTIME: &str = include_str!("runtime/channel_runtime.c");

/// Shared[T] / Weak[T] — atomically ref-counted shared data (Arc/Weak-like).
/// V2: full atomic strong + weak ref-counts with proper RAII drop.
/// - strong count: number of Shared[T] handles alive
/// - weak count: number of Weak[T] handles alive, plus 1 (held collectively by all strongs)
///   The collective strong-held weak ref keeps the control block alive until the last strong drops.
/// - downgrade(): Shared→Weak (increments weak)
/// - upgrade(): Weak→Option[Shared[T]] (CAS strong: if >0 bump, else None)
pub const SHARED_RUNTIME: &str = include_str!("runtime/shared_runtime.c");

/// Mutex[T] — async-compatible mutex returning a Guard[T] RAII handle.
/// Blocking lock via `gorget_mutex_lock` (uses pthread_mutex_lock).
/// Async poll variant `gorget_mutex_poll_lock` supports waker-based notification.
pub const MUTEX_RUNTIME: &str = include_str!("runtime/mutex_runtime.c");

/// I/O Reactor: timerfd (Linux) / kqueue (macOS) backed sleep that uses the GorgetWaker protocol.
/// gorget_reactor_sleep_ms() is emitted when std.async.sleep(ms) is called.
/// Requires: ASYNC_RUNTIME and EXECUTOR_RUNTIME (for __GorgetWorkerWakerCtx pattern).
pub const REACTOR_RUNTIME: &str = include_str!("runtime/reactor_runtime.c");

/// Blocking thread pool: expandable pool for offloading blocking I/O from async worker threads.
/// Starts with 0 threads, grows on demand up to GORGET_BLOCKING_POOL_MAX, idle threads exit
/// after 10 seconds. Work items carry a GorgetWaker that fires on completion.
pub const BLOCKING_POOL_RUNTIME: &str = include_str!("runtime/blocking_pool_runtime.c");

/// TaskGroup — structured concurrency: all spawned children complete before join() returns.
pub const TASK_GROUP_RUNTIME: &str = include_str!("runtime/task_group_runtime.c");

/// Checked arithmetic macros.
pub const RUNTIME_CHECKED_ARITH: &str = include_str!("runtime/runtime_checked_arith.c");

/// GorgetArray — dynamic array (Vector).
pub const RUNTIME_ARRAY: &str = include_str!("runtime/runtime_array.c");

/// String/Array operations — join, split, iterators (needs RUNTIME_ARRAY).
pub const RUNTIME_STRING_ARRAY: &str = include_str!("runtime/runtime_string_array.c");

/// GorgetMap — open-addressing hash map.
pub const RUNTIME_MAP: &str = include_str!("runtime/runtime_map.c");

/// GorgetSet — thin wrapper over GorgetMap.
pub const RUNTIME_SET: &str = include_str!("runtime/runtime_set.c");

/// Error handling (setjmp/longjmp for test recovery).
pub const RUNTIME_ERROR: &str = include_str!("runtime/runtime_error.c");

/// File I/O operations.
pub const RUNTIME_FILE: &str = include_str!("runtime/runtime_file.c");

/// Path functions, directory operations, readdir.
pub const RUNTIME_PATH: &str = include_str!("runtime/runtime_path.c");

/// gorget_args() — returns CLI args as GorgetArray (needs RUNTIME_ARRAY).
pub const RUNTIME_ARGS: &str = include_str!("runtime/runtime_args.c");

/// Parsing functions (parse_int, parse_float, try_parse).
pub const RUNTIME_PARSE: &str = include_str!("runtime/runtime_parse.c");

/// Type-to-string conversions (int_to_str, float_to_str, etc.).
pub const RUNTIME_TOSTR: &str = include_str!("runtime/runtime_tostr.c");

/// Environment access (getenv, setenv, getcwd, platform).
pub const RUNTIME_ENV: &str = include_str!("runtime/runtime_env.c");

/// Interactive I/O, random, time, datetime, line input.
pub const RUNTIME_IO: &str = include_str!("runtime/runtime_io.c");

/// Math functions and constants.
pub const RUNTIME_MATH: &str = include_str!("runtime/runtime_math.c");

/// Array sort comparators.
pub const RUNTIME_SORT: &str = include_str!("runtime/runtime_sort.c");

/// Trace runtime emitted only when `directive trace` or `--trace` is active.
pub const TRACE_RUNTIME: &str = include_str!("runtime/trace_runtime.c");

/// C runtime for xtd.sdl — SDL2 graphics bindings.
pub const SDL_RUNTIME: &str = include_str!("runtime/sdl_runtime.c");

/// C runtime for std.process — process execution (exec, exec_output).
pub const PROCESS_RUNTIME: &str = include_str!("runtime/process_runtime.c");


/// C runtime for xtd.http — HTTP requests via libcurl.

// ── xtd.crypto runtime ──────────────────────────────────────
pub const CRYPTO_RUNTIME: &str = include_str!("runtime/crypto_runtime.c");

// ── std.net.socket runtime ───────────────────────────────────
pub const SOCKET_RUNTIME: &str = include_str!("runtime/socket_runtime.c");

// ── std.net.socket server runtime ───────────────────────────
pub const SERVER_SOCKET_RUNTIME: &str = include_str!("runtime/server_socket_runtime.c");

// ── std.net.udp runtime ─────────────────────────────────────
pub const UDP_SOCKET_RUNTIME: &str = include_str!("runtime/udp_socket_runtime.c");

// ── std.net.tls runtime ─────────────────────────────────────
pub const TLS_SOCKET_RUNTIME: &str = include_str!("runtime/tls_socket_runtime.c");

// ── TLS Server Socket runtime ───────────────────────────────
pub const TLS_SERVER_RUNTIME: &str = include_str!("runtime/tls_server_runtime.c");

// ── std.bytes runtime ───────────────────────────────────────
pub const BYTES_RUNTIME: &str = include_str!("runtime/bytes_runtime.c");

pub const HOT_RELOAD_RUNTIME: &str = include_str!("runtime/hot_reload_runtime.c");

/// std.sync — AtomicInt, AtomicBool, Barrier, RWLock[T] + ReadGuard[T]/WriteGuard[T].
pub const SYNC_RUNTIME: &str = include_str!("runtime/sync_runtime.c");

/// std.thread — Thread[T], thread_spawn, current_thread_id.
/// Per-function spawn/join C helpers are emitted by emit_thread_helpers() in the backend.
pub const THREAD_RUNTIME: &str = include_str!("runtime/thread_runtime.c");

/// Enhanced std.process runtime — adds Process type with fork+exec+pipes.
pub const PROCESS_SPAWN_RUNTIME: &str = include_str!("runtime/process_spawn_runtime.c");

// ─── SQLite ──────────────────────────────────────────────────
// The amalgamation is embedded at compile time via include_str! so programs
// that use xtd.sqlite need no external -lsqlite3 flag.  The gorget wrapper
// functions come after the amalgamation so they can call sqlite3_* symbols.

/// Full SQLite3 amalgamation source (~8.7 MB of C code).
/// Emitted with GCC diagnostic suppressors to keep -Wall -Wextra clean.
// ─── bytes_read/write_f32_le ──────────────────────────────────
// Required for BSP/MD3 binary parsing and vertex buffer packing.
// Gorget float is f64; these convert to/from IEEE 754 single-precision.

pub const BYTES_F32_RUNTIME: &str = include_str!("runtime/bytes_f32_runtime.c");

// ─── OpenGL 2.1 Runtime ──────────────────────────────────────
// Thin wrappers around OpenGL calls. Uses SDL2 for context management.
// Gorget float is f64; all GL calls cast to GLfloat/GLdouble as needed.

pub const GL_RUNTIME: &str = include_str!("runtime/gl_runtime.c");

// ─── Image Loading (stb_image) ───────────────────────────────
// Embeds stb_image.h (public domain) for loading PNG/JPG/TGA/BMP.

pub const IMAGE_RUNTIME: &str = include_str!("runtime/image_runtime.c");

// ─── Audio (SDL2_mixer) ──────────────────────────────────────

pub const AUDIO_RUNTIME: &str = include_str!("runtime/audio_runtime.c");

// ─── Zlib/Deflate (miniz) ────────────────────────────────────
// Embeds miniz for PK3/ZIP decompression.

pub const COMPRESS_RUNTIME: &str = include_str!("runtime/compress_runtime.c");

// ─── Metal Runtime (macOS) ────────────────────────────────────
// Objective-C wrappers around Apple Metal API. Compiled with -x objective-c.
// All Metal objects are passed as int64_t (opaque handles, pointer-sized).
// Objects from new* methods are returned +1 (caller owns).
// Objects from non-new methods are retained before returning.
// gorget_metal_release() does [(id)handle release].

pub const METAL_RUNTIME: &str = include_str!("runtime/metal_runtime.c");

/// stb_image.h (public domain) — full image loading for PNG/JPEG/TGA/BMP.
/// Embedded directly so every platform gets JPEG support without needing
/// stb_image.h installed on the system include path.
pub const STB_IMAGE_SOURCE: &str = include_str!("stb_image.h");

pub const SQLITE_AMALGAMATION: &str = include_str!("sqlite3/sqlite3.c");

/// Gorget-callable thin wrappers around the SQLite3 C API.
/// Must be emitted AFTER both RUNTIME_PREAMBLE (for `Str`, `int64_t`, etc.)
/// and SQLITE_AMALGAMATION (for `sqlite3*`, `SQLITE_ROW`, etc.).
pub const SQLITE_GORGET_WRAPPERS: &str = include_str!("sqlite3/gorget_sqlite.c");
