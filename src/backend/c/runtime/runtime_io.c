
// ── Interactive I/O ─────────────────────────────────────────
// Portable splitmix64 PRNG — deterministic across all platforms.
static uint64_t __gorget_rng_state = 0;
static inline void gorget_seed(int64_t seed) { __gorget_rng_state = (uint64_t)seed; }
static inline int64_t gorget_rand(void) {
    __gorget_rng_state += UINT64_C(0x9e3779b97f4a7c15);
    uint64_t z = __gorget_rng_state;
    z = (z ^ (z >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
    z = (z ^ (z >> 27)) * UINT64_C(0x94d049bb133111eb);
    z = z ^ (z >> 31);
    return (int64_t)(z & INT64_MAX);
}
static inline int64_t gorget_rand_range(int64_t lo, int64_t hi) {
    if (lo >= hi) return lo;
    return lo + (int64_t)((uint64_t)gorget_rand() % (uint64_t)(hi - lo));
}
static inline int64_t gorget_getchar(void) {
    unsigned char c;
    ssize_t n = read(STDIN_FILENO, &c, 1);
    return n <= 0 ? -1 : (int64_t)c;
}
static inline void gorget_sleep_ms(int64_t ms) { usleep((useconds_t)(ms * 1000)); }
static inline int64_t gorget_time(void) { return (int64_t)time(NULL); }
static inline int64_t gorget_time_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (int64_t)ts.tv_sec * 1000 + (int64_t)(ts.tv_nsec / 1000000);
}
static inline const char* gorget_format_time(int64_t epoch, const char* fmt) {
    time_t t = (time_t)epoch;
    struct tm tm_buf;
    localtime_r(&t, &tm_buf);
    char buf[256];
    size_t n = strftime(buf, sizeof(buf), fmt, &tm_buf);
    char* out = (char*)GORGET_ALLOC(n + 1);
    memcpy(out, buf, n + 1);
    return out;
}
static inline int64_t gorget_parse_time(const char* s, const char* fmt) {
    struct tm tm_buf;
    memset(&tm_buf, 0, sizeof(tm_buf));
    tm_buf.tm_isdst = -1;
    char* rest = strptime(s, fmt, &tm_buf);
    if (rest == NULL) return INT64_C(-1);
    return (int64_t)mktime(&tm_buf);
}
static inline int64_t gorget_term_cols(void) { struct winsize ws; if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1) return 80; return (int64_t)ws.ws_col; }
static inline int64_t gorget_term_rows(void) { struct winsize ws; if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1) return 24; return (int64_t)ws.ws_row; }
static inline bool gorget_is_tty(void) { return isatty(STDOUT_FILENO) != 0; }

// ── DateTime decomposition ────────────────────────────────────
// Returns a GorgetArray of 7 int64_t elements:
// [year, month(1-12), day(1-31), hour, minute, second, offset_sec]
static inline GorgetArray gorget_dt_decompose(int64_t epoch, bool utc) {
    GorgetArray arr = gorget_array_new(sizeof(int64_t));
    time_t t = (time_t)epoch;
    struct tm tm_buf;
    memset(&tm_buf, 0, sizeof(tm_buf));
    if (utc) {
        gmtime_r(&t, &tm_buf);
    } else {
        localtime_r(&t, &tm_buf);
    }
    int64_t fields[7];
    fields[0] = (int64_t)(tm_buf.tm_year + 1900);
    fields[1] = (int64_t)(tm_buf.tm_mon + 1);
    fields[2] = (int64_t)tm_buf.tm_mday;
    fields[3] = (int64_t)tm_buf.tm_hour;
    fields[4] = (int64_t)tm_buf.tm_min;
    fields[5] = (int64_t)tm_buf.tm_sec;
#ifdef __linux__
    fields[6] = utc ? INT64_C(0) : (int64_t)tm_buf.tm_gmtoff;
#elif defined(__APPLE__)
    fields[6] = utc ? INT64_C(0) : (int64_t)tm_buf.tm_gmtoff;
#else
    fields[6] = INT64_C(0);
#endif
    for (int _i = 0; _i < 7; _i++) {
        gorget_array_push(&arr, &fields[_i]);
    }
    return arr;
}

// ── Line input ───────────────────────────────────────────────
static inline const char* gorget_readline(void) {
    char buf[4096];
    if (fgets(buf, sizeof(buf), stdin) == NULL) { char* r = (char*)GORGET_ALLOC(1); r[0]='\0'; return r; }
    size_t len = strlen(buf);
    if (len > 0 && buf[len - 1] == '\n') buf[--len] = '\0';
    char* out = (char*)GORGET_ALLOC(len + 1);
    memcpy(out, buf, len + 1);
    return out;
}

static inline const char* gorget_input(const char* prompt) {
    fputs(prompt, stdout);
    fflush(stdout);
    return gorget_readline();
}

