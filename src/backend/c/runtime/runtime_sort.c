
// ── Array sort comparators ───────────────────────────────────
static int __gorget_cmp_i64(const void* a, const void* b) {
    int64_t va = *(const int64_t*)a, vb = *(const int64_t*)b;
    return (va > vb) - (va < vb);
}
static int __gorget_cmp_f64(const void* a, const void* b) {
    double va = *(const double*)a, vb = *(const double*)b;
    return (va > vb) - (va < vb);
}
static int __gorget_cmp_str(const void* a, const void* b) {
    const char* sa = *(const char* const*)a;
    const char* sb = *(const char* const*)b;
    return strcmp(sa, sb);
}
static int __gorget_cmp_Str(const void* a, const void* b) {
    Str sa = *(const Str*)a;
    Str sb = *(const Str*)b;
    size_t min_len = sa.len < sb.len ? sa.len : sb.len;
    int cmp = min_len > 0 ? memcmp(sa.data, sb.data, min_len) : 0;
    if (cmp != 0) return cmp;
    return (sa.len > sb.len) - (sa.len < sb.len);
}
static int __gorget_cmp_char(const void* a, const void* b) {
    char ca = *(const char*)a, cb = *(const char*)b;
    return (ca > cb) - (ca < cb);
}
static int __gorget_cmp_bool(const void* a, const void* b) {
    bool ba = *(const bool*)a, bb = *(const bool*)b;
    return (int)ba - (int)bb;
}

static inline void gorget_array_reverse(GorgetArray* arr) {
    if (arr->len <= 1) return;
    char tmp[arr->elem_size];
    for (size_t i = 0, j = arr->len - 1; i < j; i++, j--) {
        char* a = (char*)arr->data + i * arr->elem_size;
        char* b = (char*)arr->data + j * arr->elem_size;
        memcpy(tmp, a, arr->elem_size);
        memcpy(a, b, arr->elem_size);
        memcpy(b, tmp, arr->elem_size);
    }
}

