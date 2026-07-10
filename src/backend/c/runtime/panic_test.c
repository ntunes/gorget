
// ── Cleanup stack (survives longjmp) ─────────────────────────
typedef void (*__gorget_cleanup_fn)(void*);
typedef struct { __gorget_cleanup_fn fn; void* ptr; } __gorget_cleanup_entry;
static _Thread_local __gorget_cleanup_entry __gorget_cleanup[256];
static _Thread_local int __gorget_cleanup_top = 0;

static inline void __gorget_cleanup_push(__gorget_cleanup_fn fn, void* ptr) {
    if (__gorget_cleanup_top < 256)
        __gorget_cleanup[__gorget_cleanup_top++] =
            (__gorget_cleanup_entry){fn, ptr};
}

static inline void __gorget_cleanup_run(int mark) {
    for (int i = __gorget_cleanup_top - 1; i >= mark; i--)
        __gorget_cleanup[i].fn(__gorget_cleanup[i].ptr);
    __gorget_cleanup_top = mark;
}

// ── Panic helper (test mode) ─────────────────────────────────
static jmp_buf __gorget_test_jmp;
static const char* __gorget_test_fail_msg = NULL;
static volatile int __gorget_in_test = 0;
// Set by the test runner to the cleanup stack mark before calling each test.
// gorget_panic() runs cleanup entries from this mark before longjmp,
// while the test function's stack frame is still valid.
static volatile int __gorget_test_cleanup_mark = 0;

// Fail-message copy buffer. gorget_panic_at / gorget_trap_at longjmp back to
// the test runner, abandoning the trapping call's stack frame — but `msg` /
// `detail` frequently point INTO that frame (the runtime's snprintf'd bounds
// details) or into test-body heap values that __gorget_cleanup_run frees
// (user `panic(str)` marshals `(const char*)str.data`). Copy into a static
// buffer BEFORE cleanup + longjmp so __gorget_test_fail_msg never dangles.
// _Thread_local matches the cleanup-stack convention above. Overlong
// messages truncate — acceptable for a test-failure line.
static _Thread_local char __gorget_test_fail_buf[256];
static inline const char* __gorget_test_msg_copy(const char* msg) {
    // Never return NULL: a NULL __gorget_test_fail_msg means "test passed".
    if (!msg) msg = "";
    snprintf(__gorget_test_fail_buf, sizeof(__gorget_test_fail_buf), "%s", msg);
    return __gorget_test_fail_buf;
}

static inline void gorget_panic_at(const char* file, int line, int col, const char* msg) {
    if (__gorget_in_test) {
        // Copy first: msg may point into this test's stack frame or into a
        // heap value the cleanup run below is about to free.
        __gorget_test_fail_msg = __gorget_test_msg_copy(msg);
        // Run test-body cleanup while stack frame is still valid (locals not yet abandoned).
        __gorget_cleanup_run(__gorget_test_cleanup_mark);
        longjmp(__gorget_test_jmp, 1);
    }
    fprintf(stderr, "%s:%d:%d: %s\n", file, line, col, msg);
    exit(1);
}
static inline void gorget_panic(const char* msg) {
    gorget_panic_at("<unknown>", 0, 0, msg);
}

// ── Trap helper (test mode) ──────────────────────────────────
// D11 normalization. In test mode capture the BARE `detail` (NOT the
// `trap[...]`-wrapped line) into __gorget_test_fail_msg and longjmp, exactly
// as gorget_panic_at captures `msg` — so the #[test] harness's `FAIL: %s` and
// @should_panic `strstr` see the same message the runtime-normal path would
// print. Outside test mode, emit the normative trap line + exit 101.
// Argument order is (code, detail, file, line, col) — code-first.
static inline void gorget_trap_at(const char* code, const char* detail, const char* file, int line, int col) {
    if (__gorget_in_test) {
        // Copy first (see __gorget_test_msg_copy): the D11 bounds/offset
        // details are snprintf'd into `char __gg_detail[96]` stack buffers in
        // the trapping frame, which longjmp abandons — storing the raw
        // pointer left __gorget_test_fail_msg dangling (garbage FAIL text,
        // nondeterministic @should_panic matching).
        __gorget_test_fail_msg = __gorget_test_msg_copy(detail);
        __gorget_cleanup_run(__gorget_test_cleanup_mark);
        longjmp(__gorget_test_jmp, 1);
    }
    fprintf(stderr, "trap[%s]: %s at %s:%d:%d\n", code, detail, file, line, col);
    exit(101);
}
static inline void gorget_trap(const char* code, const char* detail) {
    gorget_trap_at(code, detail, "<unknown>", 0, 0);
}

// ── Formatted trap helpers ───────────────────────────────────
// One-line replacement for the copy-pasted producer pattern
//     char __gg_detail[96]; snprintf(__gg_detail, ..., ...); gorget_trap(code, __gg_detail);
// The formatted detail lives in a local buffer; safe in test mode because
// gorget_trap_at copies it (above) before longjmp. Mirrored in panic_normal.c
// (exactly one of the two panic runtimes is emitted per binary).
static inline void gorget_trap_fmt(const char* code, const char* fmt, ...) {
    char __gg_detail[256];
    va_list __gg_ap;
    va_start(__gg_ap, fmt);
    vsnprintf(__gg_detail, sizeof(__gg_detail), fmt, __gg_ap);
    va_end(__gg_ap);
    gorget_trap(code, __gg_detail);
}
// Span-carrying variant. Argument order deviates from gorget_trap_at
// (code, detail, file, line, col) because varargs must trail: span first,
// then the format.
static inline void gorget_trap_at_fmt(const char* code, const char* file, int line, int col, const char* fmt, ...) {
    char __gg_detail[256];
    va_list __gg_ap;
    va_start(__gg_ap, fmt);
    vsnprintf(__gg_detail, sizeof(__gg_detail), fmt, __gg_ap);
    va_end(__gg_ap);
    gorget_trap_at(code, __gg_detail, file, line, col);
}

// ── Timeout support (SIGALRM + setitimer) ────────────────────
#include <signal.h>
#include <sys/time.h>
static volatile int __gorget_test_timed_out = 0;
static void __gorget_timeout_handler(int sig) {
    (void)sig;
    __gorget_test_timed_out = 1;
    __gorget_test_fail_msg = "test timed out";
    // Do NOT run cleanup here (signal handler context). The post-setjmp code handles it.
    longjmp(__gorget_test_jmp, 2);
}
static inline void __gorget_set_timeout(long ms) {
    __gorget_test_timed_out = 0;
    struct sigaction sa = {0};
    sa.sa_handler = __gorget_timeout_handler;
    sigaction(SIGALRM, &sa, NULL);
    struct itimerval tv = {{0,0}, {ms / 1000, (ms % 1000) * 1000}};
    setitimer(ITIMER_REAL, &tv, NULL);
}
static inline void __gorget_cancel_timeout(void) {
    struct itimerval off = {{0,0}, {0,0}};
    setitimer(ITIMER_REAL, &off, NULL);
    signal(SIGALRM, SIG_DFL);
}

// ── Test Output Capture (start/end — toggling is test-runner only) ──
static void __gorget_capture_start(void) {
    __gorget_capturing = 1;
    __gorget_capture_len = 0;
}

static const char *__gorget_capture_end(size_t *out_len) {
    __gorget_capturing = 0;
    if (__gorget_capture_buf && __gorget_capture_len > 0)
        __gorget_capture_buf[__gorget_capture_len] = '\0';
    *out_len = __gorget_capture_len;
    return __gorget_capture_buf ? __gorget_capture_buf : "";
}

// ── Snapshot Capture ──
static FILE* __gorget_snapshot_file = NULL;
static int __gorget_snapshot_first = 1;
static const char* __gorget_current_test = "";

static void __gorget_snapshot_open(void) {
    const char* path = getenv("GORGET_SNAPSHOT_PATH");
    if (path) __gorget_snapshot_file = fopen(path, "w");
    if (__gorget_snapshot_file) fprintf(__gorget_snapshot_file, "[\n");
}

static void __gorget_snapshot_begin(const char* test_name, const char* point_name) {
    if (!__gorget_snapshot_file) return;
    if (!__gorget_snapshot_first) fprintf(__gorget_snapshot_file, ",\n");
    __gorget_snapshot_first = 0;
    fprintf(__gorget_snapshot_file, "{\"test\":\"%s\",\"point\":\"%s\",\"value\":", test_name, point_name);
}

static void __gorget_snapshot_end(void) {
    if (!__gorget_snapshot_file) return;
    fprintf(__gorget_snapshot_file, "}");
}

static void __gorget_snapshot_close(void) {
    if (__gorget_snapshot_file) {
        fprintf(__gorget_snapshot_file, "\n]\n");
        fclose(__gorget_snapshot_file);
        __gorget_snapshot_file = NULL;
    }
}

static void __gorget_snapshot_write_int(const char* point, long long v) {
    __gorget_snapshot_begin(__gorget_current_test, point);
    if (__gorget_snapshot_file) fprintf(__gorget_snapshot_file, "%lld", v);
    __gorget_snapshot_end();
}

static void __gorget_snapshot_write_float(const char* point, double v) {
    __gorget_snapshot_begin(__gorget_current_test, point);
    if (__gorget_snapshot_file) fprintf(__gorget_snapshot_file, "%.17g", v);
    __gorget_snapshot_end();
}

static void __gorget_snapshot_write_bool(const char* point, int v) {
    __gorget_snapshot_begin(__gorget_current_test, point);
    if (__gorget_snapshot_file) fprintf(__gorget_snapshot_file, "%s", v ? "true" : "false");
    __gorget_snapshot_end();
}

static void __gorget_snapshot_write_str(const char* point, Str s) {
    __gorget_snapshot_begin(__gorget_current_test, point);
    if (__gorget_snapshot_file) {
        const char* d = (const char*)s.data;
        fputc('"', __gorget_snapshot_file);
        for (size_t i = 0; i < s.len; i++) {
            char c = d[i];
            if (c == '"' || c == '\\') { fputc('\\', __gorget_snapshot_file); fputc(c, __gorget_snapshot_file); }
            else if (c == '\n') { fputc('\\', __gorget_snapshot_file); fputc('n', __gorget_snapshot_file); }
            else if (c == '\r') { fputc('\\', __gorget_snapshot_file); fputc('r', __gorget_snapshot_file); }
            else if (c == '\t') { fputc('\\', __gorget_snapshot_file); fputc('t', __gorget_snapshot_file); }
            else fputc(c, __gorget_snapshot_file);
        }
        fputc('"', __gorget_snapshot_file);
    }
    __gorget_snapshot_end();
}

static void __gorget_snapshot_write_null(const char* point) {
    __gorget_snapshot_begin(__gorget_current_test, point);
    if (__gorget_snapshot_file) fprintf(__gorget_snapshot_file, "null");
    __gorget_snapshot_end();
}
