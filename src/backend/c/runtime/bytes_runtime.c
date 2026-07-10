
// ── Byte Buffer Helpers (std.bytes) ─────────────────────────

// Convert a str to Vector[uint8]
static inline GorgetArray gorget_bytes_from_str(const char* s) {
    GorgetArray arr = gorget_array_new(sizeof(uint8_t));
    if (s) {
        size_t len = strlen(s);
        for (size_t i = 0; i < len; i++) {
            uint8_t b = (uint8_t)s[i];
            gorget_array_push(&arr, &b);
        }
    }
    return arr;
}

// Convert Vector[uint8] to a Str. Uses length-aware copy so embedded NUL
// bytes survive into the resulting Str (gorget_string_adopt would strlen
// the buffer and truncate at the first '\0').
static inline Str gorget_bytes_to_str(const GorgetArray* arr) {
    size_t len = arr->len;
    return str_alloc_copy((const char*)arr->data, len, __gorget_current_alloc);
}

// Validate that a Vector[uint8] contains well-formed UTF-8.
// Returns 1 (true) if valid, 0 (false) if the byte sequence is invalid UTF-8.
static inline int gorget_bytes_utf8_valid(const GorgetArray* arr) {
    const uint8_t* p = (const uint8_t*)arr->data;
    size_t len = arr->len;
    size_t i = 0;
    while (i < len) {
        uint8_t b = p[i];
        int seq; // expected number of continuation bytes
        uint32_t cp; // codepoint accumulator for range checks
        if (b < 0x80) {
            // ASCII — single byte
            i++; continue;
        } else if (b < 0xC2) {
            // 0x80-0xBF: stray continuation; 0xC0-0xC1: overlong 2-byte
            return 0;
        } else if (b < 0xE0) {
            seq = 1; cp = b & 0x1F;
        } else if (b < 0xF0) {
            seq = 2; cp = b & 0x0F;
        } else if (b <= 0xF4) {
            seq = 3; cp = b & 0x07;
        } else {
            // 0xF5+ are invalid in UTF-8
            return 0;
        }
        i++;
        for (int j = 0; j < seq; j++, i++) {
            if (i >= len) return 0; // truncated sequence
            uint8_t c = p[i];
            if ((c & 0xC0) != 0x80) return 0; // not a continuation byte
            cp = (cp << 6) | (c & 0x3F);
        }
        // Range checks: reject surrogates and codepoints > U+10FFFF,
        // and catch overlong 3-byte sequences (cp < 0x800).
        if (seq == 2 && cp < 0x800) return 0;
        if (seq == 3 && (cp < 0x10000 || cp > 0x10FFFF)) return 0;
        if (cp >= 0xD800 && cp <= 0xDFFF) return 0; // UTF-16 surrogates
    }
    return 1;
}

// Convert a hex string to Vector[uint8]
static inline GorgetArray gorget_bytes_from_hex(const char* hex) {
    GorgetArray arr = gorget_array_new(sizeof(uint8_t));
    if (!hex) return arr;
    size_t len = strlen(hex);
    for (size_t i = 0; i + 1 < len; i += 2) {
        char buf[3] = { hex[i], hex[i+1], '\0' };
        uint8_t b = (uint8_t)strtoul(buf, NULL, 16);
        gorget_array_push(&arr, &b);
    }
    return arr;
}

// Write a big-endian uint32 at offset into Vector[uint8]
static inline void gorget_bytes_write_u32_be(GorgetArray* arr, int64_t offset, int64_t value) {
    if (offset < 0 || (size_t)(offset + 4) > arr->len) {
        char __gg_detail[96];
        snprintf(__gg_detail, sizeof(__gg_detail), "bytes_write_u32_be: offset %lld out of bounds (len %zu)", (long long)offset, arr->len);
        gorget_trap(GG_T_BOUNDS, __gg_detail);
    }
    uint8_t* p = (uint8_t*)arr->data + offset;
    uint32_t v = (uint32_t)value;
    p[0] = (v >> 24) & 0xFF;
    p[1] = (v >> 16) & 0xFF;
    p[2] = (v >> 8) & 0xFF;
    p[3] = v & 0xFF;
}

// Read a big-endian uint32 from offset in Vector[uint8]
static inline int64_t gorget_bytes_read_u32_be(const GorgetArray* arr, int64_t offset) {
    if (offset < 0 || (size_t)(offset + 4) > arr->len) {
        char __gg_detail[96];
        snprintf(__gg_detail, sizeof(__gg_detail), "bytes_read_u32_be: offset %lld out of bounds (len %zu)", (long long)offset, arr->len);
        gorget_trap(GG_T_BOUNDS, __gg_detail);
    }
    const uint8_t* p = (const uint8_t*)arr->data + offset;
    return (int64_t)(((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) | ((uint32_t)p[2] << 8) | (uint32_t)p[3]);
}

// Concatenate two Vector[uint8]
static inline GorgetArray gorget_bytes_concat(const GorgetArray* a, const GorgetArray* b) {
    GorgetArray arr = gorget_array_new(sizeof(uint8_t));
    size_t total = a->len + b->len;
    if (total > 0) {
        arr.data = GORGET_ALLOC(total * sizeof(uint8_t));
        arr.cap = total;
        if (a->len > 0) memcpy(arr.data, a->data, a->len);
        if (b->len > 0) memcpy((uint8_t*)arr.data + a->len, b->data, b->len);
        arr.len = total;
    }
    return arr;
}

// Slice a Vector[uint8] from start to end (exclusive)
static inline GorgetArray gorget_bytes_slice(const GorgetArray* arr, int64_t start, int64_t end) {
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    if (start < 0) start = 0;
    if (end > (int64_t)arr->len) end = (int64_t)arr->len;
    if (start >= end) return out;
    size_t count = (size_t)(end - start);
    out.data = GORGET_ALLOC(count * sizeof(uint8_t));
    out.cap = count;
    memcpy(out.data, (uint8_t*)arr->data + start, count);
    out.len = count;
    return out;
}

// Generate random bytes using /dev/urandom
static inline GorgetArray gorget_random_bytes(int64_t n) {
    GorgetArray arr = gorget_array_new(sizeof(uint8_t));
    if (n <= 0) return arr;
    arr.data = GORGET_ALLOC((size_t)n);
    arr.cap = (size_t)n;
    arr.len = (size_t)n;
    FILE* f = fopen("/dev/urandom", "rb");
    if (!f) {
        fprintf(stderr, "gorget: panic: cannot open /dev/urandom\n");
        exit(1);
    }
    if (fread(arr.data, 1, (size_t)n, f) != (size_t)n) {
        fprintf(stderr, "gorget: panic: short read from /dev/urandom\n");
        fclose(f);
        exit(1);
    }
    fclose(f);
    return arr;
}

// Write a big-endian uint16 at offset
static inline void gorget_bytes_write_u16_be(GorgetArray* arr, int64_t offset, int64_t value) {
    if (offset < 0 || (size_t)(offset + 2) > arr->len) {
        char __gg_detail[96];
        snprintf(__gg_detail, sizeof(__gg_detail), "bytes_write_u16_be: offset %lld out of bounds (len %zu)", (long long)offset, arr->len);
        gorget_trap(GG_T_BOUNDS, __gg_detail);
    }
    uint8_t* p = (uint8_t*)arr->data + offset;
    uint16_t v = (uint16_t)value;
    p[0] = (v >> 8) & 0xFF;
    p[1] = v & 0xFF;
}

// Read a big-endian uint16 from offset
static inline int64_t gorget_bytes_read_u16_be(const GorgetArray* arr, int64_t offset) {
    if (offset < 0 || (size_t)(offset + 2) > arr->len) {
        char __gg_detail[96];
        snprintf(__gg_detail, sizeof(__gg_detail), "bytes_read_u16_be: offset %lld out of bounds (len %zu)", (long long)offset, arr->len);
        gorget_trap(GG_T_BOUNDS, __gg_detail);
    }
    const uint8_t* p = (const uint8_t*)arr->data + offset;
    return (int64_t)(((uint16_t)p[0] << 8) | (uint16_t)p[1]);
}

// Write a little-endian uint32 at offset into Vector[uint8]
static inline void gorget_bytes_write_u32_le(GorgetArray* arr, int64_t offset, int64_t value) {
    if (offset < 0 || (size_t)(offset + 4) > arr->len) {
        char __gg_detail[96];
        snprintf(__gg_detail, sizeof(__gg_detail), "bytes_write_u32_le: offset %lld out of bounds (len %zu)", (long long)offset, arr->len);
        gorget_trap(GG_T_BOUNDS, __gg_detail);
    }
    uint8_t* p = (uint8_t*)arr->data + offset;
    uint32_t v = (uint32_t)value;
    p[0] = v & 0xFF;
    p[1] = (v >> 8) & 0xFF;
    p[2] = (v >> 16) & 0xFF;
    p[3] = (v >> 24) & 0xFF;
}

// Read a little-endian uint32 from offset in Vector[uint8]
static inline int64_t gorget_bytes_read_u32_le(const GorgetArray* arr, int64_t offset) {
    if (offset < 0 || (size_t)(offset + 4) > arr->len) {
        char __gg_detail[96];
        snprintf(__gg_detail, sizeof(__gg_detail), "bytes_read_u32_le: offset %lld out of bounds (len %zu)", (long long)offset, arr->len);
        gorget_trap(GG_T_BOUNDS, __gg_detail);
    }
    const uint8_t* p = (const uint8_t*)arr->data + offset;
    return (int64_t)((uint32_t)p[0] | ((uint32_t)p[1] << 8) | ((uint32_t)p[2] << 16) | ((uint32_t)p[3] << 24));
}

// Write a little-endian uint16 at offset
static inline void gorget_bytes_write_u16_le(GorgetArray* arr, int64_t offset, int64_t value) {
    if (offset < 0 || (size_t)(offset + 2) > arr->len) {
        char __gg_detail[96];
        snprintf(__gg_detail, sizeof(__gg_detail), "bytes_write_u16_le: offset %lld out of bounds (len %zu)", (long long)offset, arr->len);
        gorget_trap(GG_T_BOUNDS, __gg_detail);
    }
    uint8_t* p = (uint8_t*)arr->data + offset;
    uint16_t v = (uint16_t)value;
    p[0] = v & 0xFF;
    p[1] = (v >> 8) & 0xFF;
}

// Read a little-endian uint16 from offset
static inline int64_t gorget_bytes_read_u16_le(const GorgetArray* arr, int64_t offset) {
    if (offset < 0 || (size_t)(offset + 2) > arr->len) {
        char __gg_detail[96];
        snprintf(__gg_detail, sizeof(__gg_detail), "bytes_read_u16_le: offset %lld out of bounds (len %zu)", (long long)offset, arr->len);
        gorget_trap(GG_T_BOUNDS, __gg_detail);
    }
    const uint8_t* p = (const uint8_t*)arr->data + offset;
    return (int64_t)((uint16_t)p[0] | ((uint16_t)p[1] << 8));
}

// Convert Vector[uint8] to hex string
static inline Str gorget_bytes_to_hex(const GorgetArray* arr) {
    size_t len = arr->len;
    char* s = (char*)GORGET_ALLOC(len * 2 + 1);
    for (size_t i = 0; i < len; i++) {
        sprintf(s + i * 2, "%02x", ((uint8_t*)arr->data)[i]);
    }
    s[len * 2] = '\0';
    return gorget_string_adopt(s);
}

