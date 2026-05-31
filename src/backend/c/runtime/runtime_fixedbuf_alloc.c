
// ── GorgetFixedBufferAllocator ────────────────────────────────
// Bump allocator over a fixed buffer.  The buffer is allocated
// from the parent allocator in one shot at construction time;
// no further heap calls are made.  Individual deallocs are
// no-ops — use reset() or destroy() for bulk free.

typedef struct GorgetFixedBufferAllocator {
    GorgetAllocator   __alloc;      // vtable (must be first field)
    GorgetAllocator*  parent_alloc; // allocator active at construction
    uint8_t*          buf;          // backing buffer (allocated inline)
    size_t            capacity;
    size_t            used;
} GorgetFixedBufferAllocator;

static void* __gorget_fba_alloc(void* ctx, size_t size) {
    GorgetFixedBufferAllocator* fba = (GorgetFixedBufferAllocator*)ctx;
    size_t aligned = (size + 7) & ~(size_t)7;
    if (fba->used + aligned > fba->capacity) return NULL;
    void* ptr = fba->buf + fba->used;
    fba->used += aligned;
    return ptr;
}

static void* __gorget_fba_realloc(void* ctx, void* ptr, size_t old_size, size_t new_size) {
    GorgetFixedBufferAllocator* fba = (GorgetFixedBufferAllocator*)ctx;
    size_t old_aligned = (old_size + 7) & ~(size_t)7;
    size_t new_aligned = (new_size + 7) & ~(size_t)7;
    // If ptr is the most-recent allocation, try to extend in place
    if (ptr && (uint8_t*)ptr + old_aligned == fba->buf + fba->used) {
        if (new_aligned <= old_aligned) { return ptr; }
        size_t extra = new_aligned - old_aligned;
        if (fba->used + extra <= fba->capacity) {
            fba->used += extra;
            return ptr;
        }
    }
    // Otherwise bump-allocate fresh and copy
    void* new_ptr = __gorget_fba_alloc(ctx, new_size);
    if (new_ptr && ptr) {
        size_t copy_sz = old_size < new_size ? old_size : new_size;
        memcpy(new_ptr, ptr, copy_sz);
    }
    return new_ptr;
}

static void __gorget_fba_dealloc(void* ctx, void* ptr, size_t size) {
    (void)ctx; (void)ptr; (void)size;
    // No-op: bump allocators do not free individual allocations
}

static GorgetFixedBufferAllocator* gorget_fba_new(size_t capacity) {
    GorgetAllocator* pa = __gorget_current_alloc;
    // Allocate struct + buffer in one contiguous block
    GorgetFixedBufferAllocator* fba = (GorgetFixedBufferAllocator*)pa->alloc(
        pa->ctx, sizeof(GorgetFixedBufferAllocator) + capacity);
    if (!fba) return NULL;
    fba->parent_alloc    = pa;
    fba->buf             = (uint8_t*)(fba + 1);
    fba->capacity        = capacity;
    fba->used            = 0;
    fba->__alloc.alloc   = __gorget_fba_alloc;
    fba->__alloc.realloc = __gorget_fba_realloc;
    fba->__alloc.dealloc = __gorget_fba_dealloc;
    fba->__alloc.ctx     = fba;
    return fba;
}

static int64_t gorget_fba_bytes_used(GorgetFixedBufferAllocator* fba) {
    return (int64_t)fba->used;
}
static int64_t gorget_fba_capacity(GorgetFixedBufferAllocator* fba) {
    return (int64_t)fba->capacity;
}

static void gorget_fba_reset(GorgetFixedBufferAllocator* fba) {
    fba->used = 0;
}

static void gorget_fba_destroy(GorgetFixedBufferAllocator* fba) {
    GorgetAllocator* pa = fba->parent_alloc;
    pa->dealloc(pa->ctx, fba, sizeof(GorgetFixedBufferAllocator) + fba->capacity);
}

static void gorget_fba_free(GorgetFixedBufferAllocator** pp) {
    gorget_fba_destroy(*pp);
    *pp = NULL;
}

