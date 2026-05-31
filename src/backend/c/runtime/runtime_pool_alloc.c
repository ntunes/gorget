
// ── Pool Allocator ───────────────────────────────────────────
typedef struct GorgetPoolOverflow {
    void* ptr;
    struct GorgetPoolOverflow* next;
} GorgetPoolOverflow;

typedef struct GorgetPoolAllocator {
    GorgetAllocator      __alloc;       // vtable (first field — pointer-castable)
    GorgetAllocator*     parent;        // allocator active at construction time
    void*                slab;          // contiguous pre-allocated block storage
    void*                free_list;     // intrusive linked list head
    size_t               block_size;    // fixed block size (>= sizeof(void*))
    int64_t              initial_count; // blocks in the slab (for reset/destroy)
    int64_t              total_blocks;  // initial + overflow
    int64_t              used_blocks;   // currently allocated
    GorgetPoolOverflow*  overflow;      // overflow blocks (for cleanup)
} GorgetPoolAllocator;

static void* __gorget_pool_alloc(void* ctx, size_t size) {
    GorgetPoolAllocator* p = (GorgetPoolAllocator*)ctx;
    if (size > p->block_size) {
        // Oversized — delegate to parent
        return p->parent->alloc(p->parent->ctx, size);
    }
    if (p->free_list) {
        // Pop from free list
        void* block = p->free_list;
        p->free_list = *(void**)block;
        p->used_blocks++;
        return block;
    }
    // Free list empty — allocate overflow block from parent
    void* block = p->parent->alloc(p->parent->ctx, p->block_size);
    if (!block) return NULL;
    GorgetPoolOverflow* node = (GorgetPoolOverflow*)p->parent->alloc(p->parent->ctx, sizeof(GorgetPoolOverflow));
    if (!node) { p->parent->dealloc(p->parent->ctx, block, p->block_size); return NULL; }
    node->ptr = block;
    node->next = p->overflow;
    p->overflow = node;
    p->total_blocks++;
    p->used_blocks++;
    return block;
}

static void* __gorget_pool_realloc(void* ctx, void* ptr, size_t old_size, size_t new_size) {
    GorgetPoolAllocator* p = (GorgetPoolAllocator*)ctx;
    // realloc(NULL, 0, n) acts like alloc(n)
    if (!ptr) return __gorget_pool_alloc(ctx, new_size);
    // Both fit in a block — no-op, return same pointer
    if (old_size <= p->block_size && new_size <= p->block_size) return ptr;
    // Old was parent-allocated (oversized) — delegate
    if (old_size > p->block_size) {
        return p->parent->realloc(p->parent->ctx, ptr, old_size, new_size);
    }
    // Outgrows block — alloc from parent, copy, return block to free list
    void* new_ptr = p->parent->alloc(p->parent->ctx, new_size);
    if (!new_ptr) return NULL;
    memcpy(new_ptr, ptr, old_size < new_size ? old_size : new_size);
    // Return pool block to free list
    *(void**)ptr = p->free_list;
    p->free_list = ptr;
    p->used_blocks--;
    return new_ptr;
}

static void __gorget_pool_dealloc(void* ctx, void* ptr, size_t size) {
    GorgetPoolAllocator* p = (GorgetPoolAllocator*)ctx;
    if (size > p->block_size) {
        // Oversized — delegate to parent
        p->parent->dealloc(p->parent->ctx, ptr, size);
        return;
    }
    // Push to free list
    *(void**)ptr = p->free_list;
    p->free_list = ptr;
    p->used_blocks--;
}

static GorgetPoolAllocator* gorget_pool_new(size_t block_size, int64_t initial_count) {
    if (block_size < sizeof(void*)) block_size = sizeof(void*);
    GorgetAllocator* pa = __gorget_current_alloc;
    GorgetPoolAllocator* p = (GorgetPoolAllocator*)pa->alloc(pa->ctx, sizeof(GorgetPoolAllocator));
    if (!p) { fprintf(stderr, "gorget: panic: pool allocator allocation failed\\n"); exit(1); }
    p->parent = pa;
    p->block_size = block_size;
    p->initial_count = initial_count;
    p->total_blocks = initial_count;
    p->used_blocks = 0;
    p->overflow = NULL;
    // Allocate slab
    size_t slab_size = block_size * (size_t)initial_count;
    p->slab = pa->alloc(pa->ctx, slab_size);
    if (!p->slab && initial_count > 0) { fprintf(stderr, "gorget: panic: pool slab allocation failed\\n"); exit(1); }
    // Build intrusive free list through the slab
    p->free_list = NULL;
    char* base = (char*)p->slab;
    for (int64_t i = initial_count - 1; i >= 0; i--) {
        void* block = base + (size_t)i * block_size;
        *(void**)block = p->free_list;
        p->free_list = block;
    }
    p->__alloc.alloc   = __gorget_pool_alloc;
    p->__alloc.realloc = __gorget_pool_realloc;
    p->__alloc.dealloc = __gorget_pool_dealloc;
    p->__alloc.ctx     = p;
    return p;
}

static int64_t gorget_pool_used_blocks(GorgetPoolAllocator* p) { return p->used_blocks; }
static int64_t gorget_pool_free_blocks(GorgetPoolAllocator* p) { return p->total_blocks - p->used_blocks; }
static int64_t gorget_pool_total_blocks(GorgetPoolAllocator* p) { return p->total_blocks; }
static int64_t gorget_pool_block_size(GorgetPoolAllocator* p) { return (int64_t)p->block_size; }

static void gorget_pool_reset(GorgetPoolAllocator* p) {
    // Free overflow blocks via parent
    GorgetPoolOverflow* node = p->overflow;
    while (node) {
        GorgetPoolOverflow* next = node->next;
        p->parent->dealloc(p->parent->ctx, node->ptr, p->block_size);
        p->parent->dealloc(p->parent->ctx, node, sizeof(GorgetPoolOverflow));
        node = next;
    }
    p->overflow = NULL;
    p->total_blocks = p->initial_count;
    p->used_blocks = 0;
    // Rebuild free list from slab
    p->free_list = NULL;
    char* base = (char*)p->slab;
    for (int64_t i = p->initial_count - 1; i >= 0; i--) {
        void* block = base + (size_t)i * p->block_size;
        *(void**)block = p->free_list;
        p->free_list = block;
    }
}

static void gorget_pool_destroy(GorgetPoolAllocator* p) {
    if (!p) return;
    GorgetAllocator* pa = p->parent;
    // Free overflow chain
    GorgetPoolOverflow* node = p->overflow;
    while (node) {
        GorgetPoolOverflow* next = node->next;
        pa->dealloc(pa->ctx, node->ptr, p->block_size);
        pa->dealloc(pa->ctx, node, sizeof(GorgetPoolOverflow));
        node = next;
    }
    // Free slab + struct
    pa->dealloc(pa->ctx, p->slab, p->block_size * (size_t)p->initial_count);
    pa->dealloc(pa->ctx, p, sizeof(GorgetPoolAllocator));
}

static void gorget_pool_free(GorgetPoolAllocator** pp) {
    gorget_pool_destroy(*pp);
    *pp = NULL;
}

