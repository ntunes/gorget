
// ── Shared[T] / Weak[T] (atomic ref-counted shared data, Arc/Weak pattern) ──
// Marker for mutex_runtime.c (emitted AFTER this file): `gorget_shared_mutex_drop`
// (the leak-free `Shared[Mutex[T]]` carrier drop) needs both GorgetShared and
// GorgetMutex, so it lives in mutex_runtime.c guarded by this define — present
// only when shared_runtime.c is also part of the build.
#define GORGET_SHARED_RUNTIME 1
typedef struct GorgetShared {
    volatile int64_t strong;     // atomic strong ref count
    volatile int64_t weak;       // atomic weak ref count (+1 while any strong exists)
    void*            data;
    size_t           data_size;
} GorgetShared;

static inline GorgetShared* gorget_shared_new(size_t size, void* init_data) {
    GorgetShared* s = (GorgetShared*)GORGET_ALLOC(sizeof(GorgetShared));
    s->strong = 1;
    s->weak = 1;  // collective weak ref held by all strongs
    s->data = GORGET_ALLOC(size);
    s->data_size = size;
    memcpy(s->data, init_data, size);
    return s;
}

// Clone a Shared handle — atomically increments strong count, returns same pointer.
static inline GorgetShared* gorget_shared_clone(GorgetShared* s) {
    __atomic_fetch_add(&s->strong, 1, __ATOMIC_SEQ_CST);
    return s;
}

// Drop a Shared handle — decrements strong; frees data + releases collective weak at zero.
static inline void gorget_shared_drop(GorgetShared* s) {
    if (__atomic_sub_fetch(&s->strong, 1, __ATOMIC_SEQ_CST) == 0) {
        // Last strong ref — free inner data
        GORGET_FREE(s->data, s->data_size);
        s->data = NULL;
        // Release the collective weak ref that all strongs held
        if (__atomic_sub_fetch(&s->weak, 1, __ATOMIC_SEQ_CST) == 0) {
            GORGET_FREE(s, sizeof(GorgetShared));
        }
    }
}

// NOTE: `gorget_shared_mutex_drop` (the leak-free carrier drop for the
// round-16 `shared int x` model) is defined in mutex_runtime.c, not here:
// it must free the inner `GorgetMutex*` stored in the carrier's `data`, and
// `GorgetMutex` / `gorget_mutex_free` are only declared once mutex_runtime.c
// is included (which is AFTER this file). `GorgetShared` is in scope there
// because this file is included first.

static inline void* gorget_shared_get_ptr(GorgetShared* s) {
    return s->data;
}

static inline int64_t gorget_shared_strong_count(GorgetShared* s) {
    return __atomic_load_n(&s->strong, __ATOMIC_SEQ_CST);
}

// Downgrade Shared → Weak: atomically increments weak count, returns same control block.
static inline GorgetShared* gorget_shared_downgrade(GorgetShared* s) {
    __atomic_fetch_add(&s->weak, 1, __ATOMIC_SEQ_CST);
    return s;
}

// ── Weak[T] operations ──

// Clone a Weak handle — increments weak count.
static inline GorgetShared* gorget_weak_clone(GorgetShared* w) {
    __atomic_fetch_add(&w->weak, 1, __ATOMIC_SEQ_CST);
    return w;
}

// Drop a Weak handle — decrements weak; frees control block when both counts hit 0.
static inline void gorget_weak_drop(GorgetShared* w) {
    if (__atomic_sub_fetch(&w->weak, 1, __ATOMIC_SEQ_CST) == 0) {
        GORGET_FREE(w, sizeof(GorgetShared));
    }
}

// Upgrade Weak → Shared: CAS strong from N→N+1 (fails if already 0). Returns 1 on success.
static inline int gorget_weak_upgrade(GorgetShared* w) {
    int64_t cur = __atomic_load_n(&w->strong, __ATOMIC_SEQ_CST);
    while (cur > 0) {
        if (__atomic_compare_exchange_n(&w->strong, &cur, cur + 1,
                                        0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)) {
            return 1;  // success — caller now owns a new strong ref
        }
        // CAS reloaded cur on failure — retry
    }
    return 0;  // dead — strong already 0, data freed
}

// ── Shared[Vector[T]] element access (no-copy read/write via shared backing buffer) ──
// These operate directly on the GorgetArray inside the shared control block,
// avoiding the UAF that occurs when get() copies the GorgetArray struct.
static inline void* gorget_shared_array_get(GorgetShared* s, size_t index) {
    GorgetArray* arr = (GorgetArray*)s->data;
    if (index >= (size_t)arr->len) {
        gorget_trap_fmt(GG_T_BOUNDS, "shared array index out of bounds: %zu >= %lld", index, (long long)arr->len);
    }
    return (char*)arr->data + index * arr->elem_size;
}
static inline void gorget_shared_array_set(GorgetShared* s, size_t index, const void* elem, size_t elem_size) {
    GorgetArray* arr = (GorgetArray*)s->data;
    if (index >= (size_t)arr->len) {
        gorget_trap_fmt(GG_T_BOUNDS, "shared array index out of bounds: %zu >= %lld", index, (long long)arr->len);
    }
    memcpy((char*)arr->data + index * elem_size, elem, elem_size);
}
static inline int64_t gorget_shared_array_len(GorgetShared* s) {
    GorgetArray* arr = (GorgetArray*)s->data;
    return arr->len;
}
