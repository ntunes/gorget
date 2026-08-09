
// ── GorgetArray ──────────────────────────────────────────────

static inline GorgetArray gorget_array_new(size_t elem_size) {
    __gorget_array_new_count++;
    return (GorgetArray){NULL, 0, 0, elem_size, __gorget_current_alloc, NULL, NULL, NULL};
}

// Array constructor with element drop function for resource-type elements.
static inline GorgetArray gorget_array_new_drop(size_t elem_size, __gorget_drop_fn drop) {
    __gorget_array_new_count++;
    return (GorgetArray){NULL, 0, 0, elem_size, __gorget_current_alloc, drop, NULL, NULL};
}

static inline void gorget_array_push(GorgetArray* arr, const void* elem) {
    if (arr->len >= arr->cap) {
        size_t old_cap = arr->cap;
        size_t new_cap = old_cap == 0 ? 8 : old_cap * 2;
        arr->data = arr->alloc->realloc(arr->alloc->ctx, arr->data, old_cap * arr->elem_size, new_cap * arr->elem_size);
        arr->cap = new_cap;
    }
    memcpy((char*)arr->data + arr->len * arr->elem_size, elem, arr->elem_size);
    arr->len++;
    // CoW ownership boundary: materialize views/borrows to owned copies.
    // Push takes ownership — the collection must independently own its elements.
    if (arr->elem_materialize) {
        arr->elem_materialize((char*)arr->data + (arr->len - 1) * arr->elem_size);
    }
}

static inline void* gorget_array_get(const GorgetArray* arr, size_t index) {
    if (index >= arr->len) {
        gorget_trap_fmt(GG_T_BOUNDS, "index out of bounds: index %zu, length %zu", index, arr->len);
    }
    return (char*)arr->data + index * arr->elem_size;
}

// Location-aware Vector element read (D11 Layer B). The compiler reroutes the
// `v[i]` READ `gorget_array_get` CallExtern to this at the C / LLVM emit
// boundary (src/backend/c_lir/emit_call_extern.rs, src/backend/llvm/mod.rs),
// threading `TrapKind::Bounds.code()` + the resolved `file:line:col`. On OOB it
// emits the normative `trap[<code>]: <detail> at file:line:col` + exit 101 via
// the shared `gorget_trap_at`; the in-bounds fast path is identical to
// `gorget_array_get` (returns the element `void*` — the downstream shared LIR
// `Inst::Load` derefs). `index` is `int64_t` (the LIR passes I64) so a negative
// subscript reports its real value rather than a wrapped `size_t`.
static inline void* gorget_array_get_at(const GorgetArray* arr, int64_t index, const char* code, const char* file, int line, int col) {
    if (index < 0 || (size_t)index >= arr->len) {
        gorget_trap_at_fmt(code, file, line, col, "index out of bounds: index %" PRId64 ", length %zu", index, arr->len);
    }
    return (char*)arr->data + (size_t)index * arr->elem_size;
}

// Non-panicking bounds-checked get — returns NULL for out-of-bounds.
// Used by the LIR backend for Vector.get() → Option[T].
static inline void* gorget_array_safe_get(const GorgetArray* arr, int64_t index) {
    if (index < 0 || (size_t)index >= arr->len) return NULL;
    return (char*)arr->data + (size_t)index * arr->elem_size;
}

static inline size_t gorget_array_len(const GorgetArray* arr) {
    return arr->len;
}

static inline size_t gorget_array_capacity(const GorgetArray* arr) {
    return arr->cap;
}

// Ownership boundary: materialize all view elements to owned copies.
// Called at return boundaries so the caller gets independently-owned data.
// No-op for arrays without elem_materialize (trivial element types).
static inline void gorget_array_materialize_all(GorgetArray* arr) {
    if (arr->elem_materialize && arr->data) {
        for (size_t i = 0; i < arr->len; i++) {
            arr->elem_materialize((char*)arr->data + i * arr->elem_size);
        }
    }
}

// Ownership boundary for Dict returns: materialize any string-view keys to
// owned copies. `key_materialize` is set only on string-keyed maps, so the
// outer guard is what skips non-string-keyed dicts; inside the loop we just
// dispatch the hook (cap==0-only clone for strings, NULL / no-op otherwise).
static inline void gorget_map_materialize_keys(GorgetMap* m) {
    if (m->key_materialize && m->keys) {
        for (size_t i = 0; i < m->cap; i++) {
            if (m->states[i] == 1) {
                m->key_materialize((char*)m->keys + i * m->key_size);
            }
        }
    }
}

static inline void gorget_array_set(GorgetArray* arr, size_t index, const void* elem) {
    if (index >= arr->len) {
        gorget_trap_fmt(GG_T_BOUNDS, "index out of bounds: index %zu, length %zu", index, arr->len);
    }
    void* slot = (char*)arr->data + index * arr->elem_size;
    // Drop old element before overwriting (prevents resource leak).
    if (arr->elem_drop) {
        arr->elem_drop(slot);
    }
    memcpy(slot, elem, arr->elem_size);
}

static inline void gorget_array_remove(GorgetArray* arr, size_t index) {
    if (index >= arr->len) {
        gorget_trap_fmt(GG_T_BOUNDS, "index out of bounds: index %zu, length %zu", index, arr->len);
    }
    if (arr->elem_drop) {
        arr->elem_drop((char*)arr->data + index * arr->elem_size);
    }
    if (index + 1 < arr->len) {
        memmove((char*)arr->data + index * arr->elem_size,
                (char*)arr->data + (index + 1) * arr->elem_size,
                (arr->len - index - 1) * arr->elem_size);
    }
    arr->len--;
}

// Remove element at index and return pointer to a static copy of it.
// The returned pointer is valid until the next gorget_array_remove_opt call.
static inline void* gorget_array_remove_opt(GorgetArray* arr, size_t index) {
    static _Thread_local char __remove_buf[4096];
    static _Thread_local char* __remove_heap = NULL;
    static _Thread_local size_t __remove_heap_cap = 0;
    if (index >= arr->len) return NULL;
    void* elem = (char*)arr->data + index * arr->elem_size;
    char* buf;
    if (arr->elem_size <= sizeof(__remove_buf)) {
        buf = __remove_buf;
    } else {
        if (arr->elem_size > __remove_heap_cap) {
            free(__remove_heap);
            __remove_heap = (char*)malloc(arr->elem_size);
            __remove_heap_cap = arr->elem_size;
        }
        buf = __remove_heap;
    }
    memcpy(buf, elem, arr->elem_size);
    if (index + 1 < arr->len) {
        memmove((char*)arr->data + index * arr->elem_size,
                (char*)arr->data + (index + 1) * arr->elem_size,
                (arr->len - index - 1) * arr->elem_size);
    }
    arr->len--;
    return buf;
}

static inline bool gorget_array_is_empty(const GorgetArray* arr) {
    return arr->len == 0;
}

// Pop the last element and return a pointer to it (caller must deref before next mutation).
static inline void* gorget_array_pop(GorgetArray* arr) {
    if (arr->len == 0) {
        fprintf(stderr, "gorget: panic: pop from empty array\n");
        exit(1);
    }
    arr->len--;
    return (char*)arr->data + arr->len * arr->elem_size;
}

// Swap-remove: O(1) alternative to `remove(index)` that doesn't shift —
// moves the last element into the hole. Order-destroying. Panics on
// out-of-bounds.
static inline void gorget_array_swap_remove(GorgetArray* arr, size_t index) {
    if (index >= arr->len) {
        gorget_trap_fmt(GG_T_BOUNDS, "swap_remove index out of bounds: index %zu, length %zu", index, arr->len);
    }
    void* slot = (char*)arr->data + index * arr->elem_size;
    if (arr->elem_drop) {
        arr->elem_drop(slot);
    }
    size_t last = arr->len - 1;
    if (index != last) {
        memcpy(slot, (char*)arr->data + last * arr->elem_size, arr->elem_size);
    }
    arr->len--;
}

// Swap two elements by index. Panics on out-of-bounds.
static inline void gorget_array_swap(GorgetArray* arr, size_t i, size_t j) {
    if (i >= arr->len || j >= arr->len) {
        gorget_trap(GG_T_BOUNDS, "swap index out of bounds");
    }
    if (i == j) return;
    static _Thread_local char __swap_buf[4096];
    static _Thread_local char* __swap_heap = NULL;
    static _Thread_local size_t __swap_heap_cap = 0;
    char* buf;
    if (arr->elem_size <= sizeof(__swap_buf)) {
        buf = __swap_buf;
    } else {
        if (arr->elem_size > __swap_heap_cap) {
            free(__swap_heap);
            __swap_heap = (char*)malloc(arr->elem_size);
            __swap_heap_cap = arr->elem_size;
        }
        buf = __swap_heap;
    }
    void* a = (char*)arr->data + i * arr->elem_size;
    void* b = (char*)arr->data + j * arr->elem_size;
    memcpy(buf, a, arr->elem_size);
    memcpy(a, b, arr->elem_size);
    memcpy(b, buf, arr->elem_size);
}

static inline void gorget_array_ensure_capacity(GorgetArray* arr, size_t needed, size_t elem_size);

// Fill an existing array with `n` copies of `val_src`. Callers supply
// the size of the value (must match elem_size). Drops existing
// elements. Used as the low-level building block for `Vector.fill(n, v)`.
static inline void gorget_array_fill(GorgetArray* arr, size_t n, const void* val_src) {
    if (arr->elem_drop) {
        for (size_t i = 0; i < arr->len; i++) {
            arr->elem_drop((char*)arr->data + i * arr->elem_size);
        }
    }
    gorget_array_ensure_capacity(arr, n, arr->elem_size);
    for (size_t i = 0; i < n; i++) {
        memcpy((char*)arr->data + i * arr->elem_size, val_src, arr->elem_size);
    }
    arr->len = n;
}

// Pop the last element, returning NULL on empty (safe for Option wrapping).
static inline void* gorget_array_safe_pop(GorgetArray* arr) {
    if (arr->len == 0) return NULL;
    arr->len--;
    return (char*)arr->data + arr->len * arr->elem_size;
}

// Return a pointer to the first element.
static inline void* gorget_array_first(const GorgetArray* arr) {
    if (arr->len == 0) return NULL;
    return arr->data;
}

// Return a pointer to the last element.
static inline void* gorget_array_last(const GorgetArray* arr) {
    if (arr->len == 0) return NULL;
    return (char*)arr->data + (arr->len - 1) * arr->elem_size;
}

static inline void gorget_array_clear(GorgetArray* arr) {
    if (arr->elem_drop && arr->data) {
        for (size_t i = 0; i < arr->len; i++) {
            arr->elem_drop((char*)arr->data + i * arr->elem_size);
        }
    }
    arr->len = 0;
}

// Takes void* (not GorgetArray*) for the same reason as gorget_string_free:
// keeps __gorget_drop_fn callback dispatch well-typed under UBSan. Direct
// callers pass &arr / &nested_array, both decay to void*.
static inline void gorget_array_free(void* p) {
    GorgetArray* arr = (GorgetArray*)p;
    __gorget_array_free_count++;
    if (arr->elem_drop && arr->data) {
        for (size_t i = 0; i < arr->len; i++) {
            arr->elem_drop((char*)arr->data + i * arr->elem_size);
        }
    }
    if (arr->data) {
        // Owned data requires a live allocator. A NULL alloc here would deref
        // through it on dealloc — silent corruption that the cap=0 view check
        // doesn't catch (cap may be nonzero on a partially-constructed array).
        if (!arr->alloc) { fprintf(stderr, "gorget: panic: gorget_array_free invariant: data=%p but alloc=NULL (cap=%zu, len=%zu)\n", arr->data, arr->cap, arr->len); exit(1); }
        arr->alloc->dealloc(arr->alloc->ctx, arr->data, arr->cap * arr->elem_size);
    }
    arr->data = NULL;
    arr->len = 0;
    arr->cap = 0;
}

static inline bool gorget_array_contains(const GorgetArray* a, const void* needle, size_t elem_size) {
    // String elements: use content-based comparison (len + data bytes).
    // memcmp on the full GorgetString struct would compare cap/alloc fields
    // which differ between views (cap=0) and owned strings (cap>0).
    bool is_str = (a->elem_drop == (__gorget_drop_fn)gorget_string_free);
    for (size_t i = 0; i < a->len; i++) {
        void* slot = (char*)a->data + i * a->elem_size;
        if (is_str) {
            const Str* sa = (const Str*)slot;
            const Str* sb = (const Str*)needle;
            if (sa->len == sb->len && (sa->len == 0 || memcmp(sa->data, sb->data, sa->len) == 0))
                return true;
        } else {
            if (memcmp(slot, needle, elem_size) == 0) return true;
        }
    }
    return false;
}

// Allocation-size sanity:
// 1. `cap * elem_size` must not overflow size_t — otherwise the allocator
//    receives a truncated size, returns a buffer smaller than the user
//    requested, and subsequent pushes walk past the end.
// 2. `realloc` / `alloc` may return NULL on OOM or on size > platform limit.
//    Silently assigning NULL to `arr->data` then returning leaves the Vector
//    with cap>0 but a null buffer — the next push segfaults.
// Both must trap with a clean Gorget-level message.
static inline void gorget_array_reserve(GorgetArray* arr, size_t new_cap) {
    if (new_cap > arr->cap) {
        size_t new_size;
        if (__builtin_mul_overflow(new_cap, arr->elem_size, &new_size)) {
            fprintf(stderr, "gorget: panic: array capacity overflow\n"); exit(1);
        }
        void* new_data = arr->alloc->realloc(arr->alloc->ctx, arr->data, arr->cap * arr->elem_size, new_size);
        if (new_data == NULL) {
            fprintf(stderr, "gorget: panic: allocation failed\n"); exit(1);
        }
        arr->data = new_data;
        arr->cap = new_cap;
    }
}

static inline void gorget_array_ensure_capacity(GorgetArray* arr, size_t needed, size_t elem_size) {
    arr->elem_size = elem_size;
    if (needed > arr->cap) {
        size_t new_size;
        if (__builtin_mul_overflow(needed, elem_size, &new_size)) {
            fprintf(stderr, "gorget: panic: array capacity overflow\n"); exit(1);
        }
        void* new_data = arr->alloc->realloc(arr->alloc->ctx, arr->data, arr->cap * elem_size, new_size);
        if (new_data == NULL) {
            fprintf(stderr, "gorget: panic: allocation failed\n"); exit(1);
        }
        arr->data = new_data;
        arr->cap = needed;
    }
}

static inline GorgetArray gorget_array_with_capacity(size_t elem_size, size_t capacity) {
    GorgetAllocator* a = __gorget_current_alloc;
    GorgetArray arr = {NULL, 0, 0, elem_size, a, NULL, NULL, NULL};
    if (capacity > 0) {
        size_t total_size;
        if (__builtin_mul_overflow(capacity, elem_size, &total_size)) {
            fprintf(stderr, "gorget: panic: array capacity overflow\n"); exit(1);
        }
        arr.data = a->alloc(a->ctx, total_size);
        if (arr.data == NULL) {
            fprintf(stderr, "gorget: panic: allocation failed\n"); exit(1);
        }
        arr.cap = capacity;
    }
    return arr;
}

static inline int64_t gorget_array_index_of(const GorgetArray* a, const void* needle) {
    for (size_t i = 0; i < a->len; i++) {
        if (memcmp((char*)a->data + i * a->elem_size, needle, a->elem_size) == 0) return (int64_t)i;
    }
    return -1;
}

static inline int64_t gorget_array_binary_search(const GorgetArray* a, const void* key) {
    if (a->len == 0) return -1;
    size_t lo = 0, hi = a->len;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        int cmp = memcmp((char*)a->data + mid * a->elem_size, key, a->elem_size);
        if (cmp == 0) return (int64_t)mid;
        if (cmp < 0) lo = mid + 1;
        else hi = mid;
    }
    return -1;
}

static inline void gorget_array_dedup(GorgetArray* arr) {
    if (arr->len <= 1) return;
    size_t write = 1;
    for (size_t read = 1; read < arr->len; read++) {
        char* prev = (char*)arr->data + (write - 1) * arr->elem_size;
        char* curr = (char*)arr->data + read * arr->elem_size;
        if (memcmp(prev, curr, arr->elem_size) != 0) {
            if (write != read) {
                memcpy((char*)arr->data + write * arr->elem_size, curr, arr->elem_size);
            }
            write++;
        }
    }
    arr->len = write;
}

static inline void gorget_array_insert(GorgetArray* arr, size_t index, const void* elem) {
    if (index > arr->len) {
        gorget_trap_fmt(GG_T_BOUNDS, "insert index out of bounds: index %zu, length %zu", index, arr->len);
    }
    if (arr->len >= arr->cap) {
        size_t old_cap = arr->cap;
        size_t new_cap = old_cap == 0 ? 8 : old_cap * 2;
        arr->data = arr->alloc->realloc(arr->alloc->ctx, arr->data, old_cap * arr->elem_size, new_cap * arr->elem_size);
        arr->cap = new_cap;
    }
    if (index < arr->len) {
        memmove((char*)arr->data + (index + 1) * arr->elem_size,
                (char*)arr->data + index * arr->elem_size,
                (arr->len - index) * arr->elem_size);
    }
    memcpy((char*)arr->data + index * arr->elem_size, elem, arr->elem_size);
    arr->len++;
}

static inline void gorget_array_extend(GorgetArray* dst, const GorgetArray* src) {
    if (src->len == 0) return;
    size_t old_len = dst->len;
    size_t needed = old_len + src->len;
    if (needed > dst->cap) {
        size_t old_cap = dst->cap;
        size_t new_cap = old_cap == 0 ? 8 : old_cap;
        while (new_cap < needed) new_cap *= 2;
        dst->data = dst->alloc->realloc(dst->alloc->ctx, dst->data, old_cap * dst->elem_size, new_cap * dst->elem_size);
        dst->cap = new_cap;
    }
    memcpy((char*)dst->data + old_len * dst->elem_size, src->data, src->len * src->elem_size);
    dst->len = needed;
    // Deep-clone resource-type elements in the newly extended range so the
    // append is independent of `src` (mirror gorget_array_clone / slice).
    // Trivial elements leave elem_clone NULL and stay memcpy-only.
    if (dst->elem_clone) {
        for (size_t i = old_len; i < needed; i++) {
            dst->elem_clone((char*)dst->data + i * dst->elem_size);
        }
    }
}

static inline GorgetArray gorget_array_clone(const GorgetArray* src) {
    __gorget_array_clone_count++;
    GorgetArray dst = gorget_array_new(src->elem_size);
    dst.elem_drop = src->elem_drop;
    dst.elem_clone = src->elem_clone;
    dst.elem_materialize = src->elem_materialize;
    if (src->len > 0) {
        gorget_array_reserve(&dst, src->len);
        memcpy(dst.data, src->data, src->len * src->elem_size);
        dst.len = src->len;
        // Deep-clone resource-type elements so the copy is independent.
        if (dst.elem_clone) {
            for (size_t i = 0; i < dst.len; i++) {
                dst.elem_clone((char*)dst.data + i * dst.elem_size);
            }
        }
    }
    return dst;
}

// In-place clone wrappers for use as elem_clone/val_clone function pointers.
// These take void* and clone the resource in place.
static inline void gorget_array_clone_inplace(void* p) {
    GorgetArray* a = (GorgetArray*)p;
    *a = gorget_array_clone(a);
}
static inline void gorget_string_clone_inplace(void* p) {
    GorgetString* s = (GorgetString*)p;
    *s = gorget_string_clone_to_owned(s);
}

static inline GorgetArray gorget_array_slice(const GorgetArray* arr, int64_t start, int64_t end) {
    // D22 (ratified 2026-07-06): Python-style CLAMP semantics for
    // `v[a:b]` / `v[a..b]` slicing. Negative starts clamp to 0; ends past
    // the length clamp to the length; and `start > end` yields an EMPTY
    // slice (never a fault). This mirrors the String slice runtime
    // (`gorget_str_slice` at runtime_string.c:717) and ggdef's evaluator
    // (`spec/ggdef/src/eval.rs:1225-1227`). Runtime-negative variables
    // (`v[some_int:b]` where `some_int` goes negative at runtime) also
    // land here — the surface language rejects only NEGATIVE-LITERAL
    // indices at parse-time (D22 negatives-deferred + §10.9 as the
    // fallback for a genuinely-negative runtime value; kept as clamp here
    // because D22 chose clamp over §10.9 for the slice-value case).
    int64_t alen = (int64_t)arr->len;
    if (start < 0) { start = 0; }
    if (end < 0) { end = 0; }
    if (start > alen) { start = alen; }
    if (end > alen) { end = alen; }
    if (start > end) { start = end; }
    size_t slice_len = (size_t)(end - start);
    GorgetAllocator* a = __gorget_current_alloc;
    GorgetArray result = {NULL, 0, 0, arr->elem_size, a, arr->elem_drop, arr->elem_clone, arr->elem_materialize};
    if (slice_len > 0) {
        result.data = a->alloc(a->ctx, slice_len * arr->elem_size);
        memcpy(result.data, (char*)arr->data + (size_t)start * arr->elem_size, slice_len * arr->elem_size);
        result.len = slice_len;
        result.cap = slice_len;
        // Deep-clone resource-type elements so the slice is independent
        if (result.elem_clone) {
            for (size_t i = 0; i < slice_len; i++) {
                result.elem_clone((char*)result.data + i * result.elem_size);
            }
        }
    }
    return result;
}

static inline GorgetArray gorget_str_split_impl(Str s, Str delim, int64_t limit) {
    GorgetArray arr = gorget_array_new(sizeof(Str));
    // Elements are owned Strings — set drop/clone so the array manages them.
    arr.elem_drop = (__gorget_drop_fn)gorget_string_free;
    arr.elem_clone = (__gorget_drop_fn)gorget_string_clone_inplace;
    const char* sd = (const char*)s.data;
    if (delim.len == 0) {
        // Split into individual codepoints
        size_t pos = 0;
        int64_t count = 0;
        while (pos < s.len) {
            if (limit > 0 && count >= limit - 1) {
                Str sv = gorget_str_own_region(sd + pos, s.len - pos);
                gorget_array_push(&arr, &sv);
                break;
            }
            size_t cp_len = (size_t)gorget_utf8_codepoint_len((unsigned char)sd[pos]);
            Str sv = gorget_str_own_region(sd + pos, cp_len);
            gorget_array_push(&arr, &sv);
            pos += cp_len;
            count++;
        }
        return arr;
    }
    const char* dd = (const char*)delim.data;
    const char* p = sd;
    size_t remaining = s.len;
    int64_t count = 0;
    while (1) {
        if (limit > 0 && count >= limit - 1) {
            // Last part: take everything remaining
            Str sv = gorget_str_own_region(p, remaining);
            gorget_array_push(&arr, &sv);
            break;
        }
        const char* found = gorget_memmem(p, remaining, dd, delim.len);
        if (!found) {
            Str sv = gorget_str_own_region(p, remaining);
            gorget_array_push(&arr, &sv);
            break;
        }
        size_t chunk = (size_t)(found - p);
        Str sv = gorget_str_own_region(p, chunk);
        gorget_array_push(&arr, &sv);
        size_t skip = chunk + delim.len;
        p += skip;
        remaining -= skip;
        count++;
    }
    return arr;
}

// 2-arg version (backward compat): split all
static inline GorgetArray gorget_str_split(Str s, Str delim) {
    return gorget_str_split_impl(s, delim, 0);
}

// 3-arg version: split with limit
static inline GorgetArray gorget_str_splitn(Str s, Str delim, int64_t limit) {
    return gorget_str_split_impl(s, delim, limit);
}

static inline GorgetArray gorget_str_lines(Str s) {
    GorgetArray arr = gorget_array_new(sizeof(Str));
    arr.elem_drop = (__gorget_drop_fn)gorget_string_free;
    arr.elem_clone = (__gorget_drop_fn)gorget_string_clone_inplace;
    const char* sd = (const char*)s.data;
    const char* p = sd;
    size_t remaining = s.len;
    while (remaining > 0) {
        // Find next \n or \r
        size_t i = 0;
        while (i < remaining && p[i] != '\n' && p[i] != '\r') i++;
        Str sv = gorget_str_own_region(p, i);
        gorget_array_push(&arr, &sv);
        if (i >= remaining) break;
        // Skip the line ending: \r\n (2 chars) or \n or \r (1 char)
        if (p[i] == '\r' && i + 1 < remaining && p[i + 1] == '\n') {
            i += 2;
        } else {
            i += 1;
        }
        p += i;
        remaining -= i;
    }
    return arr;
}

// ── GORGET_ARRAY_AT macro ────────────────────────────────────
#define GORGET_ARRAY_AT(type, arr, i) (*(type*)gorget_array_get(&(arr), (i)))

