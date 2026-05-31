
// ── GorgetFallbackAllocator ───────────────────────────────────
// Combinator: tries primary first; if primary returns NULL, falls
// back to secondary.  Individual deallocs are no-ops — intended
// for use with bulk-free primary allocators (FixedBufferAllocator,
// Arena) paired with an unlimited secondary (global malloc).

typedef struct GorgetFallbackAllocator {
    GorgetAllocator   __alloc;        // vtable (must be first field)
    GorgetAllocator*  parent_alloc;   // allocator active at construction
    GorgetAllocator*  primary;
    GorgetAllocator*  secondary;
    int64_t           primary_count;
    int64_t           fallback_count;
} GorgetFallbackAllocator;

static void* __gorget_fallback_alloc(void* ctx, size_t size) {
    GorgetFallbackAllocator* f = (GorgetFallbackAllocator*)ctx;
    void* ptr = f->primary->alloc(f->primary->ctx, size);
    if (ptr) { f->primary_count++; return ptr; }
    ptr = f->secondary->alloc(f->secondary->ctx, size);
    if (ptr) f->fallback_count++;
    return ptr;
}

static void* __gorget_fallback_realloc(void* ctx, void* ptr, size_t old_size, size_t new_size) {
    GorgetFallbackAllocator* f = (GorgetFallbackAllocator*)ctx;
    void* new_ptr = f->primary->realloc(f->primary->ctx, ptr, old_size, new_size);
    if (new_ptr) return new_ptr;
    // Fall back: allocate from secondary and copy
    new_ptr = f->secondary->alloc(f->secondary->ctx, new_size);
    if (new_ptr) {
        if (ptr) memcpy(new_ptr, ptr, old_size < new_size ? old_size : new_size);
        f->fallback_count++;
    }
    return new_ptr;
}

static void __gorget_fallback_dealloc(void* ctx, void* ptr, size_t size) {
    (void)ctx; (void)ptr; (void)size;
    // No-op: FallbackAllocator is designed for bulk-free primary allocators.
    // Individual frees are skipped; destroy() on the primary/secondary handles cleanup.
}

static GorgetFallbackAllocator* gorget_fallback_new(void* primary, void* secondary) {
    GorgetAllocator* pa = __gorget_current_alloc;
    GorgetFallbackAllocator* f = (GorgetFallbackAllocator*)pa->alloc(
        pa->ctx, sizeof(GorgetFallbackAllocator));
    if (!f) return NULL;
    f->parent_alloc    = pa;
    f->primary         = (GorgetAllocator*)primary;
    f->secondary       = (GorgetAllocator*)secondary;
    f->primary_count   = 0;
    f->fallback_count  = 0;
    f->__alloc.alloc   = __gorget_fallback_alloc;
    f->__alloc.realloc = __gorget_fallback_realloc;
    f->__alloc.dealloc = __gorget_fallback_dealloc;
    f->__alloc.ctx     = f;
    return f;
}

static int64_t gorget_fallback_primary_count(GorgetFallbackAllocator* f) {
    return f->primary_count;
}
static int64_t gorget_fallback_fallback_count(GorgetFallbackAllocator* f) {
    return f->fallback_count;
}

static void gorget_fallback_destroy(GorgetFallbackAllocator* f) {
    GorgetAllocator* pa = f->parent_alloc;
    pa->dealloc(pa->ctx, f, sizeof(GorgetFallbackAllocator));
}

static void gorget_fallback_free(GorgetFallbackAllocator** pp) {
    gorget_fallback_destroy(*pp);
    *pp = NULL;
}

