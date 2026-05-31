
// ── Arena Allocator ──────────────────────────────────────────
typedef struct GorgetArenaBlock {
    void*  data;
    size_t capacity;
    size_t used;
    struct GorgetArenaBlock* prev;  // linked list of overflow blocks
} GorgetArenaBlock;

typedef struct GorgetArena {
    GorgetAllocator   __alloc;       // vtable (must be first for pointer cast)
    GorgetAllocator*  parent_alloc;  // allocator active at arena construction time
    GorgetArenaBlock  primary;       // inline first block (no separate allocation)
    GorgetArenaBlock* current;       // points to primary or latest overflow block
} GorgetArena;

static void* __gorget_arena_alloc(void* ctx, size_t size) {
    GorgetArena* arena = (GorgetArena*)ctx;
    // 16-byte alignment
    size_t aligned = (size + 15u) & ~(size_t)15u;
    GorgetArenaBlock* blk = arena->current;
    if (blk->used + aligned <= blk->capacity) {
        void* ptr = (char*)blk->data + blk->used;
        blk->used += aligned;
        return ptr;
    }
    // Overflow: allocate a new block via parent allocator (geometric doubling)
    size_t new_cap = blk->capacity * 2;
    if (new_cap < aligned) new_cap = aligned;
    GorgetAllocator* pa = arena->parent_alloc;
    GorgetArenaBlock* nb = (GorgetArenaBlock*)pa->alloc(pa->ctx, sizeof(GorgetArenaBlock));
    if (!nb) { fprintf(stderr, "gorget: panic: arena overflow block allocation failed\n"); exit(1); }
    nb->data = pa->alloc(pa->ctx, new_cap);
    if (!nb->data) { fprintf(stderr, "gorget: panic: arena overflow block allocation failed\n"); exit(1); }
    nb->capacity = new_cap;
    nb->used = aligned;
    nb->prev = blk;
    arena->current = nb;
    return nb->data;
}

static void* __gorget_arena_realloc(void* ctx, void* ptr, size_t old_size, size_t new_size) {
    // Bump allocator: allocate new, memcpy, old space wasted
    void* new_ptr = __gorget_arena_alloc(ctx, new_size);
    if (ptr && old_size > 0) memcpy(new_ptr, ptr, old_size < new_size ? old_size : new_size);
    return new_ptr;
}

static void __gorget_arena_dealloc(void* ctx, void* ptr, size_t size) {
    (void)ctx; (void)ptr; (void)size;  // no-op for bump allocator
}

static GorgetArena* gorget_arena_new(size_t capacity) {
    if (capacity == 0) capacity = 4096;
    GorgetAllocator* pa = __gorget_current_alloc;  // capture parent
    GorgetArena* arena = (GorgetArena*)pa->alloc(pa->ctx, sizeof(GorgetArena));
    if (!arena) { fprintf(stderr, "gorget: panic: arena allocation failed\n"); exit(1); }
    arena->primary.data = pa->alloc(pa->ctx, capacity);
    if (!arena->primary.data) { fprintf(stderr, "gorget: panic: arena allocation failed\n"); exit(1); }
    arena->primary.capacity = capacity;
    arena->primary.used = 0;
    arena->primary.prev = NULL;
    arena->current = &arena->primary;
    arena->parent_alloc = pa;
    arena->__alloc.alloc   = __gorget_arena_alloc;
    arena->__alloc.realloc = __gorget_arena_realloc;
    arena->__alloc.dealloc = __gorget_arena_dealloc;
    arena->__alloc.ctx     = arena;
    return arena;
}

static int64_t gorget_arena_bytes_used(GorgetArena* arena) {
    int64_t total = 0;
    GorgetArenaBlock* blk = arena->current;
    while (blk) {
        total += (int64_t)blk->used;
        blk = blk->prev;
    }
    return total;
}

static void gorget_arena_reset(GorgetArena* arena) {
    // Free overflow blocks via parent allocator, keep primary
    GorgetArenaBlock* blk = arena->current;
    GorgetAllocator* pa = arena->parent_alloc;
    while (blk != &arena->primary) {
        GorgetArenaBlock* prev = blk->prev;
        pa->dealloc(pa->ctx, blk->data, blk->capacity);
        pa->dealloc(pa->ctx, blk, sizeof(GorgetArenaBlock));
        blk = prev;
    }
    arena->primary.used = 0;
    arena->current = &arena->primary;
}

static void gorget_arena_destroy(GorgetArena* arena) {
    if (!arena) return;
    GorgetAllocator* pa = arena->parent_alloc;
    // Free overflow chain
    GorgetArenaBlock* blk = arena->current;
    while (blk != &arena->primary) {
        GorgetArenaBlock* prev = blk->prev;
        pa->dealloc(pa->ctx, blk->data, blk->capacity);
        pa->dealloc(pa->ctx, blk, sizeof(GorgetArenaBlock));
        blk = prev;
    }
    // Free primary buffer + arena struct
    pa->dealloc(pa->ctx, arena->primary.data, arena->primary.capacity);
    pa->dealloc(pa->ctx, arena, sizeof(GorgetArena));
}

static void gorget_arena_free(GorgetArena** p) {
    gorget_arena_destroy(*p);
    *p = NULL;
}

// Arena checkpoint: captures position in the overflow block chain.
typedef struct GorgetArenaCheckpoint {
    GorgetArenaBlock* block;
    size_t used;
} GorgetArenaCheckpoint;

static GorgetArenaCheckpoint gorget_arena_checkpoint(GorgetArena* arena) {
    return (GorgetArenaCheckpoint){ .block = arena->current, .used = arena->current->used };
}

static void gorget_arena_restore(GorgetArena* arena, GorgetArenaCheckpoint cp) {
    // Free any overflow blocks allocated after the checkpoint
    GorgetAllocator* pa = arena->parent_alloc;
    GorgetArenaBlock* blk = arena->current;
    while (blk != cp.block) {
        GorgetArenaBlock* prev = blk->prev;
        pa->dealloc(pa->ctx, blk->data, blk->capacity);
        pa->dealloc(pa->ctx, blk, sizeof(GorgetArenaBlock));
        blk = prev;
    }
    arena->current = cp.block;
    cp.block->used = cp.used;
}

