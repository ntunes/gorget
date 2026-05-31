
// ── String join (needs GorgetArray) ─────────────────────────
static inline GorgetString gorget_str_join(Str sep, GorgetArray parts) {
    GorgetAllocator* al = __gorget_current_alloc;
    if (parts.len == 0) return GORGET_EMPTY_STR;
    // Elements are Str structs
    size_t total = 0;
    for (size_t i = 0; i < parts.len; i++) {
        Str* part = (Str*)((char*)parts.data + i * parts.elem_size);
        total += part->len;
    }
    total += sep.len * (parts.len - 1);
    size_t cap = total + 1;
    char* result = (char*)al->alloc(al->ctx, cap);
    if (!result) { fprintf(stderr, "gorget: panic: out of memory\n"); exit(1); }
    char* dst = result;
    for (size_t i = 0; i < parts.len; i++) {
        if (i > 0 && sep.len > 0) {
            memcpy(dst, sep.data, sep.len);
            dst += sep.len;
        }
        Str* part = (Str*)((char*)parts.data + i * parts.elem_size);
        if (part->len > 0) memcpy(dst, part->data, part->len);
        dst += part->len;
    }
    *dst = '\0';
    return (Str){ .data = result, .cap = cap, .len = total, .alloc = al };
}

// ── String iterator methods (.bytes, .codepoints, .chars) ────
static inline GorgetArray gorget_str_bytes(Str s) {
    GorgetArray arr = gorget_array_new(sizeof(uint8_t));
    const char* d = (const char*)s.data;
    for (size_t i = 0; i < s.len; i++) {
        uint8_t b = (uint8_t)d[i];
        gorget_array_push(&arr, &b);
    }
    return arr;
}

static inline GorgetArray gorget_str_codepoints(Str s) {
    GorgetArray arr = gorget_array_new(sizeof(int64_t));
    const char* d = (const char*)s.data;
    size_t pos = 0;
    while (pos < s.len) {
        int64_t cp = gorget_utf8_decode(d, s.len, &pos);
        gorget_array_push(&arr, &cp);
    }
    return arr;
}

static inline GorgetArray gorget_str_chars(Str s) {
    GorgetArray arr = gorget_array_new(sizeof(Str));
    arr.elem_drop = (__gorget_drop_fn)gorget_string_free;
    arr.elem_clone = (__gorget_drop_fn)gorget_string_clone_inplace;
    const char* d = (const char*)s.data;
    size_t pos = 0;
    while (pos < s.len) {
        int cplen = gorget_utf8_codepoint_len((unsigned char)d[pos]);
        if (pos + (size_t)cplen > s.len) break;
        Str ch = gorget_str_own_region(d + pos, (size_t)cplen);
        gorget_array_push(&arr, &ch);
        pos += (size_t)cplen;
    }
    return arr;
}

