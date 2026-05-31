
// ── parse_int / parse_float — Result-returning (thread-local error) ──────────
static __thread const char* __gorget_parse_last_error = NULL;
static const char* gorget_parse_last_error(void) {
    const char* e = __gorget_parse_last_error;
    __gorget_parse_last_error = NULL;
    return e;
}

static inline int64_t gorget_parse_int(const char* s) {
    char* endptr;
    errno = 0;
    long long result = strtoll(s, &endptr, 10);
    if (endptr == s || (*endptr != '\0' && !isspace((unsigned char)*endptr)) || errno == ERANGE) {
        size_t n = strlen(s) + 40;
        char* msg = (char*)GORGET_ALLOC(n);
        snprintf(msg, n, "invalid integer: '%s'", s);
        __gorget_parse_last_error = msg;
        return 0;
    }
    return (int64_t)result;
}

static inline double gorget_parse_float(const char* s) {
    char* endptr;
    errno = 0;
    double result = strtod(s, &endptr);
    if (endptr == s || (*endptr != '\0' && !isspace((unsigned char)*endptr)) || errno == ERANGE) {
        size_t n = strlen(s) + 40;
        char* msg = (char*)GORGET_ALLOC(n);
        snprintf(msg, n, "invalid float: '%s'", s);
        __gorget_parse_last_error = msg;
        return 0.0;
    }
    return result;
}

// ── try_parse (fallible) ─────────────────────────────────────
// len: number of characters to treat as the string.  This matters when the
// Str pointer points into the interior of a larger allocation (e.g. the
// result of byte_slice): the bytes after len are not part of the value.
typedef struct { int64_t value; bool ok; } GorgetParseIntResult;
static inline GorgetParseIntResult gorget_try_parse_int(const char* s, size_t len) {
    char* endptr;
    errno = 0;
    long long result = strtoll(s, &endptr, 10);
    if (endptr == s || (size_t)(endptr - s) != len || errno == ERANGE) {
        return (GorgetParseIntResult){0, false};
    }
    return (GorgetParseIntResult){(int64_t)result, true};
}

/* Length-bounded variant: parses exactly `n` characters, no null-terminator required. */
static inline GorgetParseIntResult gorget_try_parse_int_n(const char* s, size_t n) {
    if (n == 0) return (GorgetParseIntResult){0, false};
    long long result = 0;
    int sign = 1;
    size_t i = 0;
    if (s[i] == '-') { sign = -1; i++; }
    else if (s[i] == '+') { i++; }
    if (i >= n) return (GorgetParseIntResult){0, false};
    size_t start = i;
    for (; i < n; i++) {
        if (s[i] < '0' || s[i] > '9') return (GorgetParseIntResult){0, false};
        result = result * 10 + (s[i] - '0');
    }
    if (i == start) return (GorgetParseIntResult){0, false};
    return (GorgetParseIntResult){(int64_t)(sign * result), true};
}

typedef struct { double value; bool ok; } GorgetParseFloatResult;
static inline GorgetParseFloatResult gorget_try_parse_float(const char* s, size_t len) {
    char* endptr;
    errno = 0;
    double result = strtod(s, &endptr);
    if (endptr == s || (size_t)(endptr - s) != len || errno == ERANGE) {
        return (GorgetParseFloatResult){0.0, false};
    }
    return (GorgetParseFloatResult){result, true};
}

