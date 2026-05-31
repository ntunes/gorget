
// ── Str-aware String operations ───────────────────────

// Str concatenation — both args are 32-byte Str structs by value.
static inline GorgetString gorget_str_cat(Str a, Str b) {
    __gorget_str_cat_count++;
    size_t la = a.len;
    size_t lb = b.len;
    if (la + lb == 0) return GORGET_EMPTY_STR;
    GorgetAllocator* al = __gorget_current_alloc;
    size_t total = la + lb;
    size_t cap = total + 1;
    char* data = (char*)al->alloc(al->ctx, cap);
    if (la > 0) memcpy(data, a.data, la);
    if (lb > 0) memcpy(data + la, b.data, lb);
    data[total] = '\0';
    return (Str){ .data = data, .cap = cap, .len = total, .alloc = al };
}

// Str-aware append to a String builder.
static inline void gorget_string_append_str(GorgetString* s, Str rhs) {
    size_t rlen = rhs.len;
    if (rlen == 0) return;
    if (s->cap == 0) {
        // View or empty — materialize into a fresh owned buffer.
        GorgetAllocator* a = __gorget_current_alloc;
        size_t new_len = s->len + rlen;
        size_t new_cap = (new_len + 1) * 2;
        char* data = (char*)a->alloc(a->ctx, new_cap);
        if (s->len > 0 && s->data) memcpy(data, s->data, s->len);
        memcpy(data + s->len, rhs.data, rlen);
        data[new_len] = '\0';
        *s = (Str){ .data = data, .cap = new_cap, .len = new_len, .alloc = a };
        return;
    }
    size_t new_len = s->len + rlen;
    if (new_len + 1 > s->cap) {
        size_t new_cap = (new_len + 1) * 2;
        s->data = s->alloc->realloc(s->alloc->ctx, s->data, s->cap, new_cap);
        s->cap = new_cap;
    }
    memcpy((char*)s->data + s->len, rhs.data, rlen);
    ((char*)s->data)[new_len] = '\0';
    s->len = new_len;
}

// Create a String from a Str (deep clone — always an owned copy).
static inline GorgetString gorget_string_from_str(Str s) {
    if (s.len == 0) return GORGET_EMPTY_STR;
    return str_alloc_copy((const char*)s.data, s.len, __gorget_current_alloc);
}

// ── cstr ↔ Str conversion ───────────────────────────────────
// Str's data pointer is NUL-terminated for owned strings (we +1 the cap in
// str_alloc_*) and for literal views (they point at C string literals).
// Views from slice/char_at/... are NOT NUL-terminated — callers of cstr
// should not be passing those (this plan keeps slice/char_at allocating).
static inline const char* gorget_str_to_cstr(Str s) {
    if (!s.data) return "";
    // Owned strings (cap>0) and literal views are NUL-terminated at data[len].
    // Mid-string views (from slice/trim/char_at) may NOT be NUL-terminated.
    if (s.cap > 0 || s.len == 0 || s.data[s.len] == '\0') return s.data;
    // Non-NUL-terminated view — allocate a NUL-terminated copy for C FFI.
    char* buf = (char*)__gorget_current_alloc->alloc(__gorget_current_alloc->ctx, s.len + 1);
    memcpy(buf, s.data, s.len);
    buf[s.len] = '\0';
    return buf;
}

// Check for embedded null bytes.
static inline bool gorget_str_has_null(Str s) {
    if (s.len == 0) return false;
    return memchr(s.data, '\0', s.len) != NULL;
}

