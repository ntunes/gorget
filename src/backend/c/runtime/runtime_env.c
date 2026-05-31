
// ── getenv / setenv / getcwd / platform ──────────────────────
static inline const char* gorget_getenv(const char* name) {
    const char* val = getenv(name);
    return val ? val : "";
}

static inline void gorget_setenv(const char* name, const char* value) {
    setenv(name, value, 1);
}

static inline const char* gorget_getcwd(void) {
    char buf[4096];
    if (getcwd(buf, sizeof(buf)) == NULL) { char* r = (char*)GORGET_ALLOC(1); r[0]='\0'; return r; }
    size_t len = strlen(buf);
    char* out = (char*)GORGET_ALLOC(len + 1);
    memcpy(out, buf, len + 1);
    return out;
}

static inline const char* gorget_platform(void) {
#if defined(__APPLE__)
    return "macos";
#elif defined(__linux__)
    return "linux";
#elif defined(_WIN32)
    return "windows";
#elif defined(__FreeBSD__)
    return "freebsd";
#else
    return "unknown";
#endif
}

static inline _Noreturn void gorget_exit(int64_t code) {
    exit((int)code);
}

