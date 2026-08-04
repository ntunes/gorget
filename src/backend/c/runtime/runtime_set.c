
// ── GorgetSet (thin wrapper over GorgetMap) ───────────────────

static inline GorgetSet gorget_set_new(size_t elem_size) {
    return gorget_map_new(elem_size, 0);
}

static inline GorgetSet gorget_set_new_str(void) {
    return gorget_map_new_str(0);
}

// Ordered Set: preserves insertion order (like Dict vs HashMap)
static inline GorgetSet gorget_ordered_set_new(size_t elem_size) {
    return gorget_dict_new(elem_size, 0);
}

static inline GorgetSet gorget_ordered_set_new_str(void) {
    return gorget_dict_new_str(0);
}

static inline void gorget_set_add(GorgetSet* s, const void* elem) {
    gorget_map_put(s, elem, NULL);
}

static inline bool gorget_set_contains(const GorgetSet* s, const void* elem) {
    return gorget_map_contains(s, elem);
}

static inline bool gorget_set_remove(GorgetSet* s, const void* elem) {
    return gorget_map_remove(s, elem);
}

static inline void gorget_set_clear(GorgetSet* s) {
    gorget_map_clear(s);
}

static inline size_t gorget_set_len(const GorgetSet* s) {
    return gorget_map_len(s);
}

static inline bool gorget_set_is_empty(const GorgetSet* s) { return gorget_map_is_empty(s); }

static inline void gorget_set_free(GorgetSet* s) {
    gorget_map_free(s);
}

static inline GorgetSet gorget_set_clone(const GorgetSet* src) {
    __gorget_set_clone_count++;
    GorgetAllocator* a = __gorget_current_alloc;
    GorgetSet dst;
    memset(&dst, 0, sizeof(dst));
    dst.key_size = src->key_size;
    dst.val_size = 0;
    dst.alloc = a;
    dst.hash_fn = src->hash_fn;
    dst.eq_fn = src->eq_fn;
    dst.key_drop = src->key_drop;
    // D39 DENSE MODE (dormant in A.2a): clone entries + indices; skip the
    // legacy sparse-buckets path entirely. `dst` was memset above, so all
    // legacy fields (keys/values/states/cap/order/tombstones) stay zero.
    if (src->entries_keys) {
        dst.count = src->count;
        dst.entries_cap = src->entries_cap;
        dst.entries_len = src->entries_len;
        if (src->entries_cap > 0) {
            dst.entries_keys = a->alloc(a->ctx, src->entries_cap * src->key_size);
            memcpy(dst.entries_keys, src->entries_keys, src->entries_len * src->key_size);
            size_t indices_cap = 2 * src->entries_cap;
            if (src->indices) {
                dst.indices = (int32_t*)a->alloc(a->ctx, indices_cap * sizeof(int32_t));
                memcpy(dst.indices, src->indices, indices_cap * sizeof(int32_t));
            }
            // Deep-clone resource-typed keys so the copy is independent.
            if (dst.key_drop) {
                for (size_t i = 0; i < dst.entries_len; i++) {
                    Str* key = (Str*)((char*)dst.entries_keys + i * dst.key_size);
                    if (key->cap > 0 && key->data) {
                        Str cloned = gorget_string_clone(key);
                        *key = cloned;
                    }
                }
            }
        }
        return dst;
    }
    dst.count = src->count;
    dst.cap = src->cap;
    dst.tombstones = src->tombstones;
    // Clone order array if present (ordered Set)
    if (src->order != NULL && src->order_len > 0) {
        dst.order = (size_t*)a->alloc(a->ctx, src->cap * sizeof(size_t));
        memcpy(dst.order, src->order, src->cap * sizeof(size_t));
        dst.order_len = src->order_len;
    } else {
        dst.order = NULL;
        dst.order_len = 0;
    }
    if (src->cap == 0) {
        dst.keys = NULL; dst.values = NULL; dst.states = NULL;
        return dst;
    }
    dst.keys = a->alloc(a->ctx, src->cap * src->key_size);
    memcpy(dst.keys, src->keys, src->cap * src->key_size);
    dst.values = NULL;
    dst.states = (uint8_t*)a->alloc(a->ctx, src->cap);
    memcpy(dst.states, src->states, src->cap);
    // Deep-clone resource-type keys (e.g., owned strings) so the copy is independent.
    // Without this, both src and dst share key buffers → double-free on drop.
    if (dst.key_drop) {
        for (size_t i = 0; i < dst.cap; i++) {
            if (dst.states[i] == 1) {
                Str* key = (Str*)((char*)dst.keys + i * dst.key_size);
                if (key->cap > 0 && key->data) {
                    Str cloned = gorget_string_clone(key);
                    *key = cloned;
                }
            }
        }
    }
    return dst;
}

// Set → GorgetArray (ordered iteration via gorget_map_keys since Set is a Map with val_size=0)
static inline GorgetArray gorget_set_to_array(const GorgetSet* s) {
    return gorget_map_keys(s);
}

