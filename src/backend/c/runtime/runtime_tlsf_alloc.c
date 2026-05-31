
// ── TLSF (Two-Level Segregate Fit) Allocator ─────────────────
// O(1) worst-case alloc/free with low fragmentation.
// Algorithm based on the TLSF spec (http://www.gii.upv.es/tlsf/),
// informed by mattconte/tlsf (BSD license). Implemented from scratch
// for Gorget's parent-allocator overflow model and sentinel blocks.
//
// Each memory region (main pool + overflow) ends with a sentinel
// block (size=0, marked used) that prevents coalescing across
// region boundaries without needing explicit pool_end checks.

#define TLSF_ALIGN_SIZE   16u
#define TLSF_ALIGN_MASK   (TLSF_ALIGN_SIZE - 1u)
#define TLSF_SL_INDEX_LOG2 4
#define TLSF_SL_INDEX_COUNT (1u << TLSF_SL_INDEX_LOG2)
#define TLSF_FL_INDEX_MAX  30
#define TLSF_FL_INDEX_SHIFT (TLSF_SL_INDEX_LOG2 + 1)
#define TLSF_FL_INDEX_COUNT (TLSF_FL_INDEX_MAX - TLSF_FL_INDEX_SHIFT + 1)
#define TLSF_BLOCK_FREE_BIT   1u
#define TLSF_BLOCK_PREV_FREE_BIT 2u
#define TLSF_BLOCK_FLAG_BITS  3u
// Minimum block body: must hold two free-list pointers (next_free, prev_free).
#define TLSF_BLOCK_OVERHEAD  (sizeof(GorgetTlsfBlockHeader))
#define TLSF_MIN_BLOCK_BODY  (sizeof(GorgetTlsfBlockHeader*) * 2)
// Minimum usable region: header + min body + sentinel header
#define TLSF_MIN_REGION_SIZE (2 * TLSF_BLOCK_OVERHEAD + TLSF_MIN_BLOCK_BODY)

typedef struct GorgetTlsfBlockHeader {
    struct GorgetTlsfBlockHeader* prev_phys;   // physically previous block (for backward coalescing)
    size_t                        size_and_flags; // block body size | flag bits in low 2 bits
    // --- free blocks only: payload area reused for free-list pointers ---
    struct GorgetTlsfBlockHeader* next_free;
    struct GorgetTlsfBlockHeader* prev_free;
} GorgetTlsfBlockHeader;

// Overflow region — tracks additional pools allocated from parent when the main pool is exhausted.
typedef struct GorgetTlsfOverflow {
    void*  ptr;
    size_t size;
    struct GorgetTlsfOverflow* next;
} GorgetTlsfOverflow;

typedef struct GorgetTlsfAllocator {
    GorgetAllocator    __alloc;
    GorgetAllocator*   parent;
    uint32_t           fl_bitmap;
    uint32_t           sl_bitmap[TLSF_FL_INDEX_COUNT];
    GorgetTlsfBlockHeader* blocks[TLSF_FL_INDEX_COUNT][TLSF_SL_INDEX_COUNT];
    void*              pool_start;
    size_t             pool_size;       // original main pool size (never inflated)
    int64_t            bytes_used;
    int64_t            peak_bytes;
    GorgetTlsfOverflow* overflow;
} GorgetTlsfAllocator;

// ── Bit helpers ──────────────────────────────────────────────
static inline int __gorget_tlsf_fls(unsigned int x) {
    return x ? (int)(31 - (unsigned)__builtin_clz(x)) : -1;
}
static inline int __gorget_tlsf_ffs(unsigned int x) {
    return x ? (int)__builtin_ctz(x) : -1;
}

// ── Block accessors ──────────────────────────────────────────
static inline size_t __gorget_tlsf_block_size(GorgetTlsfBlockHeader* b) {
    return b->size_and_flags & ~(size_t)TLSF_BLOCK_FLAG_BITS;
}
static inline void __gorget_tlsf_block_set_size(GorgetTlsfBlockHeader* b, size_t s) {
    b->size_and_flags = s | (b->size_and_flags & TLSF_BLOCK_FLAG_BITS);
}
static inline int __gorget_tlsf_block_is_free(GorgetTlsfBlockHeader* b) {
    return (int)(b->size_and_flags & TLSF_BLOCK_FREE_BIT);
}
static inline void __gorget_tlsf_block_set_free(GorgetTlsfBlockHeader* b) {
    b->size_and_flags |= TLSF_BLOCK_FREE_BIT;
}
static inline void __gorget_tlsf_block_set_used(GorgetTlsfBlockHeader* b) {
    b->size_and_flags &= ~(size_t)TLSF_BLOCK_FREE_BIT;
}
static inline int __gorget_tlsf_block_is_prev_free(GorgetTlsfBlockHeader* b) {
    return (int)(b->size_and_flags & TLSF_BLOCK_PREV_FREE_BIT);
}
static inline void __gorget_tlsf_block_set_prev_free(GorgetTlsfBlockHeader* b) {
    b->size_and_flags |= TLSF_BLOCK_PREV_FREE_BIT;
}
static inline void __gorget_tlsf_block_set_prev_used(GorgetTlsfBlockHeader* b) {
    b->size_and_flags &= ~(size_t)TLSF_BLOCK_PREV_FREE_BIT;
}
static inline void* __gorget_tlsf_block_payload(GorgetTlsfBlockHeader* b) {
    return (char*)b + TLSF_BLOCK_OVERHEAD;
}
static inline GorgetTlsfBlockHeader* __gorget_tlsf_block_from_payload(void* p) {
    return (GorgetTlsfBlockHeader*)((char*)p - TLSF_BLOCK_OVERHEAD);
}
static inline GorgetTlsfBlockHeader* __gorget_tlsf_block_next(GorgetTlsfBlockHeader* b) {
    return (GorgetTlsfBlockHeader*)((char*)__gorget_tlsf_block_payload(b) + __gorget_tlsf_block_size(b));
}
// Sentinel check: a block with size==0 that is not free is a sentinel.
static inline int __gorget_tlsf_block_is_sentinel(GorgetTlsfBlockHeader* b) {
    return __gorget_tlsf_block_size(b) == 0 && !__gorget_tlsf_block_is_free(b);
}

// ── Mapping: size → (fl, sl) ─────────────────────────────────
static inline void __gorget_tlsf_mapping_insert(size_t size, int* fl, int* sl) {
    if (size < (1u << TLSF_FL_INDEX_SHIFT)) {
        *fl = 0;
        *sl = (int)(size / (TLSF_ALIGN_SIZE));
    } else {
        int t = __gorget_tlsf_fls((unsigned int)size);
        *sl = (int)((size >> (t - TLSF_SL_INDEX_LOG2)) ^ (1u << TLSF_SL_INDEX_LOG2));
        *fl = t - TLSF_FL_INDEX_SHIFT + 1;
    }
}
static inline void __gorget_tlsf_mapping_search(size_t size, int* fl, int* sl) {
    // Round up to next size class
    size_t round = size + (1u << (__gorget_tlsf_fls((unsigned int)size) - TLSF_SL_INDEX_LOG2)) - 1u;
    __gorget_tlsf_mapping_insert(round, fl, sl);
}

// ── Free-list management ─────────────────────────────────────
static void __gorget_tlsf_remove_free_block(GorgetTlsfAllocator* t, GorgetTlsfBlockHeader* b, int fl, int sl) {
    GorgetTlsfBlockHeader* prev = b->prev_free;
    GorgetTlsfBlockHeader* next = b->next_free;
    if (next) next->prev_free = prev;
    if (prev) prev->next_free = next;
    if (t->blocks[fl][sl] == b) {
        t->blocks[fl][sl] = next;
        if (!next) {
            t->sl_bitmap[fl] &= ~(1u << (unsigned)sl);
            if (!t->sl_bitmap[fl]) {
                t->fl_bitmap &= ~(1u << (unsigned)fl);
            }
        }
    }
}

static void __gorget_tlsf_insert_free_block(GorgetTlsfAllocator* t, GorgetTlsfBlockHeader* b, int fl, int sl) {
    GorgetTlsfBlockHeader* head = t->blocks[fl][sl];
    b->next_free = head;
    b->prev_free = NULL;
    if (head) head->prev_free = b;
    t->blocks[fl][sl] = b;
    t->fl_bitmap |= 1u << (unsigned)fl;
    t->sl_bitmap[fl] |= 1u << (unsigned)sl;
}

// ── Coalescing ───────────────────────────────────────────────
// Sentinels (size=0, used) at region boundaries stop coalescing naturally.
static GorgetTlsfBlockHeader* __gorget_tlsf_merge_prev(GorgetTlsfAllocator* t, GorgetTlsfBlockHeader* b) {
    if (!__gorget_tlsf_block_is_prev_free(b)) return b;
    GorgetTlsfBlockHeader* prev = b->prev_phys;
    int fl, sl;
    __gorget_tlsf_mapping_insert(__gorget_tlsf_block_size(prev), &fl, &sl);
    __gorget_tlsf_remove_free_block(t, prev, fl, sl);
    size_t new_size = __gorget_tlsf_block_size(prev) + TLSF_BLOCK_OVERHEAD + __gorget_tlsf_block_size(b);
    __gorget_tlsf_block_set_size(prev, new_size);
    GorgetTlsfBlockHeader* next = __gorget_tlsf_block_next(prev);
    next->prev_phys = prev;
    return prev;
}

static GorgetTlsfBlockHeader* __gorget_tlsf_merge_next(GorgetTlsfAllocator* t, GorgetTlsfBlockHeader* b) {
    GorgetTlsfBlockHeader* next = __gorget_tlsf_block_next(b);
    // Sentinel blocks (size=0, used) stop coalescing at region boundaries
    if (!__gorget_tlsf_block_is_free(next)) return b;
    int fl, sl;
    __gorget_tlsf_mapping_insert(__gorget_tlsf_block_size(next), &fl, &sl);
    __gorget_tlsf_remove_free_block(t, next, fl, sl);
    size_t new_size = __gorget_tlsf_block_size(b) + TLSF_BLOCK_OVERHEAD + __gorget_tlsf_block_size(next);
    __gorget_tlsf_block_set_size(b, new_size);
    GorgetTlsfBlockHeader* nn = __gorget_tlsf_block_next(b);
    nn->prev_phys = b;
    return b;
}

// ── Split ────────────────────────────────────────────────────
static void __gorget_tlsf_block_split(GorgetTlsfAllocator* t, GorgetTlsfBlockHeader* b, size_t size) {
    size_t cur = __gorget_tlsf_block_size(b);
    // Need space for: remainder header + min body
    if (cur - size < TLSF_BLOCK_OVERHEAD + TLSF_MIN_BLOCK_BODY) return;
    size_t remain = cur - size - TLSF_BLOCK_OVERHEAD;
    GorgetTlsfBlockHeader* rest = (GorgetTlsfBlockHeader*)((char*)__gorget_tlsf_block_payload(b) + size);
    rest->size_and_flags = 0;
    __gorget_tlsf_block_set_size(rest, remain);
    rest->prev_phys = b;
    __gorget_tlsf_block_set_size(b, size);
    __gorget_tlsf_block_set_free(rest);
    // Update next block's prev_phys (could be sentinel — safe)
    GorgetTlsfBlockHeader* nn = __gorget_tlsf_block_next(rest);
    nn->prev_phys = rest;
    __gorget_tlsf_block_set_prev_free(nn);
    int fl, sl;
    __gorget_tlsf_mapping_insert(remain, &fl, &sl);
    __gorget_tlsf_insert_free_block(t, rest, fl, sl);
}

// ── Find suitable block ──────────────────────────────────────
static GorgetTlsfBlockHeader* __gorget_tlsf_find_suitable(GorgetTlsfAllocator* t, int* fl, int* sl) {
    unsigned int sl_map = t->sl_bitmap[*fl] & (~0u << (unsigned)*sl);
    if (!sl_map) {
        unsigned int fl_map = t->fl_bitmap & (~0u << ((unsigned)*fl + 1u));
        if (!fl_map) return NULL;
        *fl = __gorget_tlsf_ffs(fl_map);
        sl_map = t->sl_bitmap[*fl];
    }
    *sl = __gorget_tlsf_ffs(sl_map);
    return t->blocks[*fl][*sl];
}

// ── Region init: create free block + sentinel from raw memory ─
static void __gorget_tlsf_region_init(GorgetTlsfAllocator* t, void* pool, size_t pool_size) {
    uintptr_t start = (uintptr_t)pool;
    uintptr_t aligned_start = (start + TLSF_ALIGN_MASK) & ~(uintptr_t)TLSF_ALIGN_MASK;
    size_t adj = (size_t)(aligned_start - start);
    if (adj >= pool_size) return;
    size_t usable = pool_size - adj;
    // Need: block header + body + sentinel header
    if (usable < TLSF_MIN_REGION_SIZE) return;
    // Reserve space for sentinel at end
    size_t body_size = usable - 2 * TLSF_BLOCK_OVERHEAD;
    body_size &= ~(size_t)TLSF_ALIGN_MASK;
    if (body_size < TLSF_MIN_BLOCK_BODY) return;

    // Create main free block
    GorgetTlsfBlockHeader* b = (GorgetTlsfBlockHeader*)aligned_start;
    b->prev_phys = NULL;
    b->size_and_flags = 0;
    __gorget_tlsf_block_set_size(b, body_size);
    __gorget_tlsf_block_set_free(b);
    __gorget_tlsf_block_set_prev_used(b);
    int fl, sl;
    __gorget_tlsf_mapping_insert(body_size, &fl, &sl);
    __gorget_tlsf_insert_free_block(t, b, fl, sl);

    // Create sentinel block at end (size=0, used, not free)
    GorgetTlsfBlockHeader* sentinel = __gorget_tlsf_block_next(b);
    sentinel->prev_phys = b;
    sentinel->size_and_flags = 0;  // size=0, used (free bit not set)
    __gorget_tlsf_block_set_prev_free(sentinel); // preceding block is free
}

// ── Vtable: alloc ────────────────────────────────────────────
static void* __gorget_tlsf_alloc(void* ctx, size_t size) {
    GorgetTlsfAllocator* t = (GorgetTlsfAllocator*)ctx;
    size = (size + TLSF_ALIGN_MASK) & ~(size_t)TLSF_ALIGN_MASK;
    if (size < TLSF_MIN_BLOCK_BODY) size = TLSF_MIN_BLOCK_BODY;
    // Oversized: delegate to parent
    if (size > t->pool_size) {
        return t->parent->alloc(t->parent->ctx, size);
    }
    int fl, sl;
    __gorget_tlsf_mapping_search(size, &fl, &sl);
    GorgetTlsfBlockHeader* b = __gorget_tlsf_find_suitable(t, &fl, &sl);
    if (!b) {
        // Pool exhausted — allocate overflow region from parent, init with sentinel
        size_t overflow_size = t->pool_size;
        if (overflow_size < size + TLSF_MIN_REGION_SIZE) overflow_size = size + TLSF_MIN_REGION_SIZE;
        void* new_pool = t->parent->alloc(t->parent->ctx, overflow_size);
        if (!new_pool) { fprintf(stderr, "gorget: panic: TLSF overflow allocation failed\\n"); exit(1); }
        GorgetTlsfOverflow* node = (GorgetTlsfOverflow*)t->parent->alloc(t->parent->ctx, sizeof(GorgetTlsfOverflow));
        if (!node) { fprintf(stderr, "gorget: panic: TLSF overflow tracking failed\\n"); exit(1); }
        node->ptr = new_pool;
        node->size = overflow_size;
        node->next = t->overflow;
        t->overflow = node;
        __gorget_tlsf_region_init(t, new_pool, overflow_size);
        // Retry
        __gorget_tlsf_mapping_search(size, &fl, &sl);
        b = __gorget_tlsf_find_suitable(t, &fl, &sl);
        if (!b) { fprintf(stderr, "gorget: panic: TLSF alloc failed after overflow\\n"); exit(1); }
    }
    __gorget_tlsf_remove_free_block(t, b, fl, sl);
    __gorget_tlsf_block_split(t, b, size);
    __gorget_tlsf_block_set_used(b);
    // Mark next block as prev-used (could be sentinel — safe)
    GorgetTlsfBlockHeader* next = __gorget_tlsf_block_next(b);
    __gorget_tlsf_block_set_prev_used(next);
    t->bytes_used += (int64_t)__gorget_tlsf_block_size(b);
    if (t->bytes_used > t->peak_bytes) t->peak_bytes = t->bytes_used;
    return __gorget_tlsf_block_payload(b);
}

// ── Vtable: dealloc ──────────────────────────────────────────
static void __gorget_tlsf_dealloc(void* ctx, void* ptr, size_t size) {
    GorgetTlsfAllocator* t = (GorgetTlsfAllocator*)ctx;
    if (!ptr) return;
    // Oversized: was delegated to parent
    if (size > t->pool_size) {
        t->parent->dealloc(t->parent->ctx, ptr, size);
        return;
    }
    GorgetTlsfBlockHeader* b = __gorget_tlsf_block_from_payload(ptr);
    t->bytes_used -= (int64_t)__gorget_tlsf_block_size(b);
    __gorget_tlsf_block_set_free(b);
    b = __gorget_tlsf_merge_next(t, b);
    b = __gorget_tlsf_merge_prev(t, b);
    // Mark next block as prev-free (could be sentinel — safe)
    GorgetTlsfBlockHeader* next = __gorget_tlsf_block_next(b);
    __gorget_tlsf_block_set_prev_free(next);
    int fl, sl;
    __gorget_tlsf_mapping_insert(__gorget_tlsf_block_size(b), &fl, &sl);
    __gorget_tlsf_insert_free_block(t, b, fl, sl);
}

// ── Vtable: realloc ──────────────────────────────────────────
static void* __gorget_tlsf_realloc(void* ctx, void* ptr, size_t old_size, size_t new_size) {
    GorgetTlsfAllocator* t = (GorgetTlsfAllocator*)ctx;
    if (!ptr) return __gorget_tlsf_alloc(ctx, new_size);
    if (new_size == 0) { __gorget_tlsf_dealloc(ctx, ptr, old_size); return NULL; }
    // Oversized: was delegated to parent
    if (old_size > t->pool_size) {
        return t->parent->realloc(t->parent->ctx, ptr, old_size, new_size);
    }
    GorgetTlsfBlockHeader* b = __gorget_tlsf_block_from_payload(ptr);
    size_t cur = __gorget_tlsf_block_size(b);
    size_t aligned_new = (new_size + TLSF_ALIGN_MASK) & ~(size_t)TLSF_ALIGN_MASK;
    if (aligned_new < TLSF_MIN_BLOCK_BODY) aligned_new = TLSF_MIN_BLOCK_BODY;
    if (aligned_new <= cur) {
        __gorget_tlsf_block_split(t, b, aligned_new);
        t->bytes_used -= (int64_t)(cur - __gorget_tlsf_block_size(b));
        return ptr;
    }
    // Try expanding into next block if free (sentinel is never free → safe)
    GorgetTlsfBlockHeader* next = __gorget_tlsf_block_next(b);
    if (__gorget_tlsf_block_is_free(next)) {
        size_t combined = cur + TLSF_BLOCK_OVERHEAD + __gorget_tlsf_block_size(next);
        if (combined >= aligned_new) {
            int fl, sl;
            __gorget_tlsf_mapping_insert(__gorget_tlsf_block_size(next), &fl, &sl);
            __gorget_tlsf_remove_free_block(t, next, fl, sl);
            __gorget_tlsf_block_set_size(b, combined);
            GorgetTlsfBlockHeader* nn = __gorget_tlsf_block_next(b);
            nn->prev_phys = b;
            __gorget_tlsf_block_split(t, b, aligned_new);
            t->bytes_used += (int64_t)(__gorget_tlsf_block_size(b) - cur);
            if (t->bytes_used > t->peak_bytes) t->peak_bytes = t->bytes_used;
            return ptr;
        }
    }
    // Fallback: alloc, copy, free
    void* new_ptr = __gorget_tlsf_alloc(ctx, new_size);
    if (!new_ptr) return NULL;
    memcpy(new_ptr, ptr, cur < new_size ? cur : new_size);
    __gorget_tlsf_dealloc(ctx, ptr, old_size);
    return new_ptr;
}

// ── Public API ───────────────────────────────────────────────
static GorgetTlsfAllocator* gorget_tlsf_new(size_t pool_size) {
    if (pool_size == 0) pool_size = 65536;
    GorgetAllocator* pa = __gorget_current_alloc;
    GorgetTlsfAllocator* t = (GorgetTlsfAllocator*)pa->alloc(pa->ctx, sizeof(GorgetTlsfAllocator));
    if (!t) { fprintf(stderr, "gorget: panic: TLSF allocator allocation failed\\n"); exit(1); }
    memset(t, 0, sizeof(GorgetTlsfAllocator));
    t->parent = pa;
    t->pool_start = pa->alloc(pa->ctx, pool_size);
    if (!t->pool_start) { fprintf(stderr, "gorget: panic: TLSF pool allocation failed\\n"); exit(1); }
    t->pool_size = pool_size;
    t->bytes_used = 0;
    t->peak_bytes = 0;
    t->overflow = NULL;
    __gorget_tlsf_region_init(t, t->pool_start, pool_size);
    t->__alloc.alloc   = __gorget_tlsf_alloc;
    t->__alloc.realloc = __gorget_tlsf_realloc;
    t->__alloc.dealloc = __gorget_tlsf_dealloc;
    t->__alloc.ctx     = t;
    return t;
}

static int64_t gorget_tlsf_bytes_used(GorgetTlsfAllocator* t)  { return t->bytes_used; }
static int64_t gorget_tlsf_peak_bytes(GorgetTlsfAllocator* t)  { return t->peak_bytes; }
static int64_t gorget_tlsf_pool_size(GorgetTlsfAllocator* t)   { return (int64_t)t->pool_size; }

static void gorget_tlsf_reset(GorgetTlsfAllocator* t) {
    // Free overflow regions
    GorgetTlsfOverflow* node = t->overflow;
    while (node) {
        GorgetTlsfOverflow* next = node->next;
        t->parent->dealloc(t->parent->ctx, node->ptr, node->size);
        t->parent->dealloc(t->parent->ctx, node, sizeof(GorgetTlsfOverflow));
        node = next;
    }
    t->overflow = NULL;
    // Zero bitmaps + block matrix, reinit main pool
    t->fl_bitmap = 0;
    memset(t->sl_bitmap, 0, sizeof(t->sl_bitmap));
    memset(t->blocks, 0, sizeof(t->blocks));
    t->bytes_used = 0;
    t->peak_bytes = 0;
    __gorget_tlsf_region_init(t, t->pool_start, t->pool_size);
}

static void gorget_tlsf_destroy(GorgetTlsfAllocator* t) {
    if (!t) return;
    GorgetAllocator* pa = t->parent;
    // Free overflow regions
    GorgetTlsfOverflow* node = t->overflow;
    while (node) {
        GorgetTlsfOverflow* next = node->next;
        pa->dealloc(pa->ctx, node->ptr, node->size);
        pa->dealloc(pa->ctx, node, sizeof(GorgetTlsfOverflow));
        node = next;
    }
    pa->dealloc(pa->ctx, t->pool_start, t->pool_size);
    pa->dealloc(pa->ctx, t, sizeof(GorgetTlsfAllocator));
}

static void gorget_tlsf_free(GorgetTlsfAllocator** pp) {
    gorget_tlsf_destroy(*pp);
    *pp = NULL;
}

