
// ── Panic helper ─────────────────────────────────────────────
static inline void gorget_panic_at(const char* file, int line, int col, const char* msg) {
    fprintf(stderr, "%s:%d:%d: %s\n", file, line, col, msg);
    exit(1);
}
static inline void gorget_panic(const char* msg) {
    gorget_panic_at("<unknown>", 0, 0, msg);
}

// ── Trap helper (D11 normalization) ──────────────────────────
// The normative `trap[<T_code>]: <detail> at <file>:<line>:<col>` line + exit
// 101. The `T_` code is DATA passed from the compiler's `TrapKind::code()`
// (src/trap.rs) — there is no C-side name table. Argument order is
// (code, detail, file, line, col) — deliberately code-first, NOT the
// file-first order of `gorget_panic_at`.
static inline void gorget_trap_at(const char* code, const char* detail, const char* file, int line, int col) {
    fprintf(stderr, "trap[%s]: %s at %s:%d:%d\n", code, detail, file, line, col);
    exit(101);
}
// Span-less form for emit sites that lack a source location (runtime-internal
// callers such as gorget_assert_fail_values). Symmetric to gorget_panic.
static inline void gorget_trap(const char* code, const char* detail) {
    gorget_trap_at(code, detail, "<unknown>", 0, 0);
}
