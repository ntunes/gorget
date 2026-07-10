
// ── to_str conversions ──────────────────────────────────────
static inline Str gorget_int_to_str(int64_t n) {
    char buf[32];
    snprintf(buf, sizeof(buf), "%" PRId64, n);
    size_t len = strlen(buf);
    char* out = (char*)GORGET_ALLOC(len + 1);
    memcpy(out, buf, len + 1);
    return gorget_string_adopt(out);
}

static inline Str gorget_float_to_str(double x) {
    char buf[64];
    snprintf(buf, sizeof(buf), "%g", x);
    size_t len = strlen(buf);
    char* out = (char*)GORGET_ALLOC(len + 1);
    memcpy(out, buf, len + 1);
    return gorget_string_adopt(out);
}

static inline Str gorget_bool_to_str(bool b) {
    const char* s = b ? "true" : "false";
    size_t len = b ? 4 : 5;
    char* out = (char*)GORGET_ALLOC(len + 1);
    memcpy(out, s, len + 1);
    return gorget_string_adopt(out);
}

// debug() for String: produce a quoted, escaped representation.
// Escapes: " \\ \n \r \t \0; bytes < 0x20 or 0x7F as \xNN.
static inline Str gorget_string_debug(Str s) {
    size_t slen = s.len;
    const unsigned char* src = (const unsigned char*)s.data;
    size_t cap = slen + 2; /* surrounding quotes */
    for (size_t i = 0; i < slen; ++i) {
        unsigned char c = src[i];
        if (c == '"' || c == '\\' || c == '\n' || c == '\r' || c == '\t' || c == '\0') cap += 1;
        else if (c < 0x20 || c == 0x7F) cap += 3; /* \xNN - 1 raw char */
    }
    char* out = (char*)GORGET_ALLOC(cap + 1);
    size_t j = 0;
    out[j++] = '"';
    for (size_t i = 0; i < slen; ++i) {
        unsigned char c = src[i];
        switch (c) {
            case '"':  out[j++] = '\\'; out[j++] = '"'; break;
            case '\\': out[j++] = '\\'; out[j++] = '\\'; break;
            case '\n': out[j++] = '\\'; out[j++] = 'n'; break;
            case '\r': out[j++] = '\\'; out[j++] = 'r'; break;
            case '\t': out[j++] = '\\'; out[j++] = 't'; break;
            case '\0': out[j++] = '\\'; out[j++] = '0'; break;
            default:
                if (c < 0x20 || c == 0x7F) {
                    static const char hex[] = "0123456789abcdef";
                    out[j++] = '\\';
                    out[j++] = 'x';
                    out[j++] = hex[(c >> 4) & 0xF];
                    out[j++] = hex[c & 0xF];
                } else {
                    out[j++] = (char)c;
                }
                break;
        }
    }
    out[j++] = '"';
    out[j] = '\0';
    return gorget_string_adopt(out);
}

static inline double gorget_int_to_float(int64_t n) { return (double)n; }

// Bit-reinterpret a double as its IEEE-754 u64 payload. The self-host lowerer
// uses this to carry a float constant's exact bits through IFConst (which
// stores them as an int64_t and memcpy's them back at C-emit) — `(int64_t)v`
// would TRUNCATE the value instead of preserving the bit pattern.
static inline int64_t gorget_float_to_bits(double x) {
    int64_t bits;
    memcpy(&bits, &x, sizeof(bits));
    return bits;
}

static inline Str gorget_char_to_str(char c) {
    char* out = (char*)GORGET_ALLOC(2);
    out[0] = c;
    out[1] = '\0';
    return gorget_string_adopt(out);
}

static inline Str gorget_codepoint_to_utf8(int64_t cp) {
    // Encode into a small stack buffer with an EXPLICIT length, then
    // copy via str_alloc_copy. Going through gorget_string_adopt would
    // use strlen() to derive the length, which truncates at the first
    // NUL byte — wrong for U+0000 (returns empty) and any codepoint
    // whose UTF-8 form contains an embedded NUL (none exist for valid
    // codepoints, but U+0000 is the obvious case).
    char buf[4];
    size_t len;
    if (cp < 0 || cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF)) {
        buf[0] = (char)0xEF; buf[1] = (char)0xBF; buf[2] = (char)0xBD;
        len = 3;
    } else if (cp <= 0x7F) {
        buf[0] = (char)cp;
        len = 1;
    } else if (cp <= 0x7FF) {
        buf[0] = (char)(0xC0 | (cp >> 6));
        buf[1] = (char)(0x80 | (cp & 0x3F));
        len = 2;
    } else if (cp <= 0xFFFF) {
        buf[0] = (char)(0xE0 | (cp >> 12));
        buf[1] = (char)(0x80 | ((cp >> 6) & 0x3F));
        buf[2] = (char)(0x80 | (cp & 0x3F));
        len = 3;
    } else {
        buf[0] = (char)(0xF0 | (cp >> 18));
        buf[1] = (char)(0x80 | ((cp >> 12) & 0x3F));
        buf[2] = (char)(0x80 | ((cp >> 6) & 0x3F));
        buf[3] = (char)(0x80 | (cp & 0x3F));
        len = 4;
    }
    return str_alloc_copy(buf, len, __gorget_current_alloc);
}


// ── Assert failure with pre-formatted value strings ─────────
// D11: `code` is the T_AssertFailed trap code (typed data from src/trap.rs).
// Routes through gorget_trap (span-less — this runtime helper has no source
// location) → trap[T_AssertFailed]: <detail> at <unknown>:0:0 + exit 101, or
// the test-mode longjmp capturing the bare detail.
static inline void gorget_assert_fail_values(const char* code, const char* op, Str left, Str right) {
    gorget_trap(code, gorget_format("assertion failed: left %s right\n  left:  %.*s\n  right: %.*s",
            op, (int)left.len, (const char*)left.data, (int)right.len, (const char*)right.data));
}

