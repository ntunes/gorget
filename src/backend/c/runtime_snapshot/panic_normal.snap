
// ── Panic helper ─────────────────────────────────────────────
static inline void gorget_panic_at(const char* file, int line, int col, const char* msg) {
    fprintf(stderr, "%s:%d:%d: %s\n", file, line, col, msg);
    exit(1);
}
static inline void gorget_panic(const char* msg) {
    gorget_panic_at("<unknown>", 0, 0, msg);
}
