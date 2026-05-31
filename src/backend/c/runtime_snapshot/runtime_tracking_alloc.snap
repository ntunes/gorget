
// ── Tracking Allocator ───────────────────────────────────────
typedef struct GorgetTrackingAllocator {
    GorgetAllocator  __alloc;       // vtable (must be first for pointer cast)
    GorgetAllocator* inner;         // wrapped allocator
    int64_t alloc_count;
    int64_t free_count;
    int64_t realloc_count;
    int64_t bytes_allocated;
    int64_t bytes_freed;
    int64_t current_bytes;          // allocated - freed
    int64_t peak_bytes;
} GorgetTrackingAllocator;

static void* __gorget_tracking_alloc(void* ctx, size_t size) {
    GorgetTrackingAllocator* t = (GorgetTrackingAllocator*)ctx;
    void* ptr = t->inner->alloc(t->inner->ctx, size);
    if (ptr) {
        t->alloc_count++;
        t->bytes_allocated += (int64_t)size;
        t->current_bytes += (int64_t)size;
        if (t->current_bytes > t->peak_bytes) t->peak_bytes = t->current_bytes;
    }
    return ptr;
}

static void* __gorget_tracking_realloc(void* ctx, void* ptr, size_t old_size, size_t new_size) {
    GorgetTrackingAllocator* t = (GorgetTrackingAllocator*)ctx;
    void* new_ptr = t->inner->realloc(t->inner->ctx, ptr, old_size, new_size);
    if (new_ptr) {
        t->realloc_count++;
        /* Treat realloc as free(old) + alloc(new) so all counters
           accurately reflect logical memory activity.
           When ptr == NULL it is an initial alloc routed via realloc
           (e.g. first Vector push); count it as an alloc_count hit. */
        if (ptr != NULL && old_size > 0) {
            t->free_count++;
            t->bytes_freed += (int64_t)old_size;
        } else {
            t->alloc_count++;
        }
        t->bytes_allocated += (int64_t)new_size;
        t->current_bytes += ((int64_t)new_size - (int64_t)old_size);
        if (t->current_bytes > t->peak_bytes) t->peak_bytes = t->current_bytes;
    }
    return new_ptr;
}

static void __gorget_tracking_dealloc(void* ctx, void* ptr, size_t size) {
    GorgetTrackingAllocator* t = (GorgetTrackingAllocator*)ctx;
    t->inner->dealloc(t->inner->ctx, ptr, size);
    t->free_count++;
    t->bytes_freed += (int64_t)size;
    t->current_bytes -= (int64_t)size;
}

static GorgetTrackingAllocator* gorget_tracking_new(void) {
    GorgetAllocator* pa = __gorget_current_alloc;
    GorgetTrackingAllocator* t = (GorgetTrackingAllocator*)pa->alloc(pa->ctx, sizeof(GorgetTrackingAllocator));
    if (!t) { fprintf(stderr, "gorget: panic: tracking allocator allocation failed\n"); exit(1); }
    t->inner = pa;
    t->alloc_count = 0;
    t->free_count = 0;
    t->realloc_count = 0;
    t->bytes_allocated = 0;
    t->bytes_freed = 0;
    t->current_bytes = 0;
    t->peak_bytes = 0;
    t->__alloc.alloc   = __gorget_tracking_alloc;
    t->__alloc.realloc = __gorget_tracking_realloc;
    t->__alloc.dealloc = __gorget_tracking_dealloc;
    t->__alloc.ctx     = t;
    return t;
}

static int64_t gorget_tracking_alloc_count(GorgetTrackingAllocator* t) { return t->alloc_count; }
static int64_t gorget_tracking_free_count(GorgetTrackingAllocator* t) { return t->free_count; }
static int64_t gorget_tracking_bytes_allocated(GorgetTrackingAllocator* t) { return t->bytes_allocated; }
static int64_t gorget_tracking_bytes_freed(GorgetTrackingAllocator* t) { return t->bytes_freed; }
static int64_t gorget_tracking_current_bytes(GorgetTrackingAllocator* t) { return t->current_bytes; }
static int64_t gorget_tracking_peak_bytes(GorgetTrackingAllocator* t) { return t->peak_bytes; }
static int64_t gorget_tracking_realloc_count(GorgetTrackingAllocator* t) { return t->realloc_count; }

static void gorget_tracking_reset(GorgetTrackingAllocator* t) {
    t->alloc_count = 0;
    t->free_count = 0;
    t->realloc_count = 0;
    t->bytes_allocated = 0;
    t->bytes_freed = 0;
    t->current_bytes = 0;
    t->peak_bytes = 0;
}

static void gorget_tracking_report(GorgetTrackingAllocator* t) {
    fprintf(stderr, "── TrackingAllocator Report ──\\n");
    fprintf(stderr, "  allocs:     %lld\\n", (long long)t->alloc_count);
    fprintf(stderr, "  frees:      %lld\\n", (long long)t->free_count);
    fprintf(stderr, "  reallocs:   %lld\\n", (long long)t->realloc_count);
    fprintf(stderr, "  bytes in:   %lld\\n", (long long)t->bytes_allocated);
    fprintf(stderr, "  bytes out:  %lld\\n", (long long)t->bytes_freed);
    fprintf(stderr, "  current:    %lld\\n", (long long)t->current_bytes);
    fprintf(stderr, "  peak:       %lld\\n", (long long)t->peak_bytes);
    fprintf(stderr, "─────────────────────────────\\n");
}

static void gorget_tracking_destroy(GorgetTrackingAllocator* t) {
    if (!t) return;
    if (t->current_bytes > 0) {
        fprintf(stderr, "gorget: warning: TrackingAllocator destroyed with %lld bytes still allocated "
                "(%lld allocs, %lld frees)\n",
                (long long)t->current_bytes, (long long)t->alloc_count, (long long)t->free_count);
    }
    GorgetAllocator* pa = t->inner;
    pa->dealloc(pa->ctx, t, sizeof(GorgetTrackingAllocator));
}

static void gorget_tracking_free(GorgetTrackingAllocator** p) {
    gorget_tracking_destroy(*p);
    *p = NULL;
}

