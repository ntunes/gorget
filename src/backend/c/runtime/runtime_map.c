
// ── GorgetMap (open-addressing hash map) ─────────────────────

// Str key hash/eq for GorgetMap — content-based (32-byte Str struct).
static inline uint64_t __gorget_str_key_hash(const void* key) {
    const Str* s = (const Str*)key;
    if (s->len == 0) return 0;
    return __gorget_hash_str_len((const char*)s->data, s->len);
}
static inline bool __gorget_str_key_eq(const void* a, const void* b) {
    const Str* sa = (const Str*)a;
    const Str* sb = (const Str*)b;
    if (sa->len != sb->len) return false;
    if (sa->len == 0) return true;
    if (sa->data == sb->data) return true;  // aliased views
    return memcmp(sa->data, sb->data, sa->len) == 0;
}

// Hash/eq dispatch: use custom functions if set, otherwise default
// Default byte-wise key equality. Fast-paths the dominant 8-byte (int/ptr) and
// 4-byte key widths with a direct word compare, skipping the libc memcmp call
// (which showed up as ~7% of the int-Dict hot path via bcmp). Reads exactly
// key_size bytes — same contract as memcmp.
static inline bool __gorget_key_eq_default(const void* a, const void* b, size_t key_size) {
    if (key_size == 8) {
        uint64_t x, y; memcpy(&x, a, 8); memcpy(&y, b, 8); return x == y;
    }
    if (key_size == 4) {
        uint32_t x, y; memcpy(&x, a, 4); memcpy(&y, b, 4); return x == y;
    }
    return memcmp(a, b, key_size) == 0;
}
#define __GORGET_MAP_HASH(m, key) ((m)->hash_fn ? (m)->hash_fn(key) : __gorget_fnv1a(key, (m)->key_size))
#define __GORGET_MAP_EQ(m, idx, key) ((m)->eq_fn ? (m)->eq_fn((const char*)(m)->keys + (idx) * (m)->key_size, key) : __gorget_key_eq_default((const char*)(m)->keys + (idx) * (m)->key_size, key, (m)->key_size))

static inline void __gorget_map_grow(GorgetMap* m) {
    GorgetAllocator* a = m->alloc;
    size_t old_cap = m->cap;
    void* old_keys = m->keys;
    void* old_values = m->values;
    uint8_t* old_states = m->states;
    size_t* old_order = m->order;
    size_t old_order_len = m->order_len;

    // Capacity is ALWAYS a power of two: gorget_dict_new/gorget_map_new start at
    // 16 (or 0→16 here) and grow strictly by doubling. gorget_map_reserve grows
    // by repeated doubling too. This invariant lets every probe use the bitmask
    // `idx & (cap-1)` instead of `idx % cap` — a 1-cycle AND vs a ~20-cycle divide.
    size_t new_cap = old_cap == 0 ? 16 : old_cap * 2;
    size_t new_mask = new_cap - 1;  // valid because new_cap is power-of-two
    m->keys = GORGET_CALLOC(new_cap, m->key_size);
    m->values = m->val_size > 0 ? GORGET_CALLOC(new_cap, m->val_size) : NULL;
    m->states = (uint8_t*)GORGET_CALLOC(new_cap, 1);
    m->order = old_order ? (size_t*)GORGET_CALLOC(new_cap, sizeof(size_t)) : NULL;
    m->cap = new_cap;
    m->count = 0;
    m->order_len = 0;
    m->tombstones = 0;

    // Reinsert existing elements
    if (old_order) {
        // Ordered mode: reinsert in insertion order to preserve ordering
        for (size_t oi = 0; oi < old_order_len; oi++) {
            size_t i = old_order[oi];
            if (old_states[i] != 1) continue;
            const void* key = (const char*)old_keys + i * m->key_size;
            uint64_t h = __GORGET_MAP_HASH(m, key);
            size_t idx = (size_t)(h & new_mask);
            while (m->states[idx] != 0) {
                idx = (idx + 1) & new_mask;
            }
            memcpy((char*)m->keys + idx * m->key_size, key, m->key_size);
            if (m->val_size > 0) {
                const void* val = (const char*)old_values + i * m->val_size;
                memcpy((char*)m->values + idx * m->val_size, val, m->val_size);
            }
            m->states[idx] = 1;
            m->order[m->order_len++] = idx;
            m->count++;
        }
    } else {
        // Unordered mode: bucket scan to reinsert all occupied slots
        for (size_t i = 0; i < old_cap; i++) {
            if (old_states[i] != 1) continue;
            const void* key = (const char*)old_keys + i * m->key_size;
            uint64_t h = __GORGET_MAP_HASH(m, key);
            size_t idx = (size_t)(h & new_mask);
            while (m->states[idx] != 0) {
                idx = (idx + 1) & new_mask;
            }
            memcpy((char*)m->keys + idx * m->key_size, key, m->key_size);
            if (m->val_size > 0) {
                const void* val = (const char*)old_values + i * m->val_size;
                memcpy((char*)m->values + idx * m->val_size, val, m->val_size);
            }
            m->states[idx] = 1;
            m->count++;
        }
    }

    a->dealloc(a->ctx, old_keys, old_cap * m->key_size);
    if (old_values) a->dealloc(a->ctx, old_values, old_cap * m->val_size);
    a->dealloc(a->ctx, old_states, old_cap);
    if (old_order) a->dealloc(a->ctx, old_order, old_cap * sizeof(size_t));
}

static inline GorgetMap gorget_map_new(size_t key_size, size_t val_size) {
    // Field order: { keys, cap, values, states, count, key_size, val_size, alloc, order, order_len, tombstones, hash_fn, eq_fn, val_drop, val_clone, key_drop, key_clone, val_materialize, key_materialize }
    return (GorgetMap){NULL, 0, NULL, NULL, 0, key_size, val_size, __gorget_current_alloc, NULL, 0, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};
}

// Pre-allocate hash table to hold at least `new_cap` entries without growing.
static inline void gorget_map_reserve(GorgetMap* m, int64_t new_cap) {
    if (new_cap <= 0 || (size_t)new_cap <= m->cap) return;
    while (m->cap < (size_t)new_cap) __gorget_map_grow(m);
}

static inline void gorget_set_reserve(GorgetSet* s, int64_t new_cap) {
    gorget_map_reserve(s, new_cap);
}

// Ordered Dict: pre-allocates order array so put() tracks insertion order
static inline GorgetMap gorget_dict_new(size_t key_size, size_t val_size) {
    GorgetAllocator* a = __gorget_current_alloc;
    size_t init_cap = 16;
    GorgetMap m;
    m.keys = GORGET_CALLOC(init_cap, key_size);
    m.values = val_size > 0 ? GORGET_CALLOC(init_cap, val_size) : NULL;
    m.states = (uint8_t*)GORGET_CALLOC(init_cap, 1);
    m.count = 0;
    m.cap = init_cap;
    m.key_size = key_size;
    m.val_size = val_size;
    m.alloc = a;
    m.order = (size_t*)GORGET_CALLOC(init_cap, sizeof(size_t));
    m.order_len = 0;
    m.tombstones = 0;
    m.hash_fn = NULL;
    m.eq_fn = NULL;
    m.val_drop = NULL;
    m.val_clone = NULL;
    m.key_drop = NULL;
    m.key_clone = NULL;
    m.val_materialize = NULL;
    m.key_materialize = NULL;
    return m;
}

// Str-key variants: content-based hash/eq instead of byte-based
static inline GorgetMap gorget_map_new_str(size_t val_size) {
    GorgetMap m = gorget_map_new(sizeof(Str), val_size);
    m.hash_fn = __gorget_str_key_hash;
    m.eq_fn = __gorget_str_key_eq;
    m.key_drop = (__gorget_drop_fn)gorget_string_free;
    m.key_clone = (__gorget_drop_fn)gorget_string_clone_inplace;
    m.key_materialize = (__gorget_drop_fn)gorget_string_materialize_inplace;
    return m;
}
static inline GorgetMap gorget_dict_new_str(size_t val_size) {
    GorgetMap m = gorget_dict_new(sizeof(Str), val_size);
    m.hash_fn = __gorget_str_key_hash;
    m.eq_fn = __gorget_str_key_eq;
    m.key_drop = (__gorget_drop_fn)gorget_string_free;
    m.key_clone = (__gorget_drop_fn)gorget_string_clone_inplace;
    m.key_materialize = (__gorget_drop_fn)gorget_string_materialize_inplace;
    return m;
}

// CoW materialize after memcpy of a key/value into the map's slot.
//
// Symmetric with `gorget_array_push`'s elem_materialize: for owned resource
// data (cap>0 strings, owned arrays/dicts/sets/user structs) the compiler
// guarantees independence at the call site — clone-before-put for borrows,
// MoveZero after for owned temps / last-use. So this hook is cap==0-only for
// strings (clones static/stack literals into owned copies) and NULL for other
// resource types (ownership transferred via the C backend post-call zero).
// `*_clone` (full always-clone) is kept separately for dict.clone() and the
// gorget_map_put_cloned helper used by filter/map/update when deliberately
// duplicating entries from another map.
static inline void __gorget_map_materialize_key(GorgetMap* m, size_t idx) {
    if (m->key_materialize) {
        m->key_materialize((char*)m->keys + idx * m->key_size);
    }
}
static inline void __gorget_map_materialize_value(GorgetMap* m, size_t idx) {
    if (m->val_materialize) {
        m->val_materialize((char*)m->values + idx * m->val_size);
    }
}

static inline void gorget_map_put(GorgetMap* m, const void* key, const void* value) {
    // Ordered mode (order != NULL): count tombstones in load factor to force grow,
    // and never reuse tombstone slots. This ensures stale order-array entries
    // pointing to tombstoned slots are correctly skipped during iteration.
    if (m->order) {
        if (m->cap == 0 || (m->count + m->tombstones) * 4 >= m->cap * 3) {
            __gorget_map_grow(m);
        }
        size_t mask = m->cap - 1;  // cap is power-of-two (see __gorget_map_grow)
        uint64_t h = __GORGET_MAP_HASH(m, key);
        size_t idx = (size_t)(h & mask);
        for (size_t __probes = 0; __probes < m->cap; __probes++) {
            if (m->states[idx] == 0) {
                memcpy((char*)m->keys + idx * m->key_size, key, m->key_size);
                __gorget_map_materialize_key(m, idx);
                if (m->val_size > 0 && value != NULL) {
                    memcpy((char*)m->values + idx * m->val_size, value, m->val_size);
                    __gorget_map_materialize_value(m, idx);
                }
                m->states[idx] = 1;
                m->count++;
                m->order[m->order_len++] = idx;
                return;
            }
            if (m->states[idx] == 1 && __GORGET_MAP_EQ(m, idx, key)) {
                if (m->val_size > 0 && value != NULL) {
                    if (m->val_drop) {
                        m->val_drop((char*)m->values + idx * m->val_size);
                    }
                    memcpy((char*)m->values + idx * m->val_size, value, m->val_size);
                    __gorget_map_materialize_value(m, idx);
                }
                return;
            }
            idx = (idx + 1) & mask;
        }
        return;
    }
    // Unordered mode (HashMap/Set): reuse tombstones for efficiency
    if (m->cap == 0 || m->count * 4 >= m->cap * 3) {
        __gorget_map_grow(m);
    }
    size_t mask = m->cap - 1;  // cap is power-of-two (see __gorget_map_grow)
    uint64_t h = __GORGET_MAP_HASH(m, key);
    size_t idx = (size_t)(h & mask);
    size_t first_tombstone = (size_t)-1;
    for (size_t __probes = 0; __probes < m->cap; __probes++) {
        if (m->states[idx] == 0) {
            size_t target = first_tombstone != (size_t)-1 ? first_tombstone : idx;
            memcpy((char*)m->keys + target * m->key_size, key, m->key_size);
            __gorget_map_materialize_key(m, target);
            if (m->val_size > 0 && value != NULL) {
                memcpy((char*)m->values + target * m->val_size, value, m->val_size);
                __gorget_map_materialize_value(m, target);
            }
            m->states[target] = 1;
            m->count++;
            return;
        }
        if (m->states[idx] == 2 && first_tombstone == (size_t)-1) {
            first_tombstone = idx;
        }
        if (m->states[idx] == 1 && __GORGET_MAP_EQ(m, idx, key)) {
            if (m->val_size > 0 && value != NULL) {
                if (m->val_drop) {
                    m->val_drop((char*)m->values + idx * m->val_size);
                }
                memcpy((char*)m->values + idx * m->val_size, value, m->val_size);
                __gorget_map_materialize_value(m, idx);
            }
            return;
        }
        idx = (idx + 1) & mask;
    }
    if (first_tombstone != (size_t)-1) {
        memcpy((char*)m->keys + first_tombstone * m->key_size, key, m->key_size);
        __gorget_map_materialize_key(m, first_tombstone);
        if (m->val_size > 0 && value != NULL) {
            memcpy((char*)m->values + first_tombstone * m->val_size, value, m->val_size);
            __gorget_map_materialize_value(m, first_tombstone);
        }
        m->states[first_tombstone] = 1;
        m->count++;
    }
}

// Put with full deep-clone of key/value via key_clone/val_clone hooks. Used by
// inline helpers (filter/map/update/etc.) that deliberately insert pointers
// borrowed from another map's slots: the caller does not own the source, so
// we must materialize independent copies after the raw memcpy done by
// gorget_map_put. gorget_map_put is the no-clone ownership-transfer variant
// used at Dict[k]=v and .put() call sites (where the compiler manages
// ownership). Skips the second clone for static literals (cap==0) — those
// were already cloned by gorget_string_materialize_inplace via key_materialize.
static inline void gorget_map_put_cloned(GorgetMap* m, const void* key, const void* value) {
    gorget_map_put(m, key, value);
    size_t mask = m->cap - 1;  // cap is power-of-two (see __gorget_map_grow)
    uint64_t h = __GORGET_MAP_HASH(m, key);
    size_t idx = (size_t)(h & mask);
    for (size_t __probes = 0; __probes < m->cap; __probes++) {
        if (m->states[idx] == 1 && __GORGET_MAP_EQ(m, idx, key)) {
            if (m->key_clone) {
                // For strings, key_materialize handles cap==0 views; owned (cap>0)
                // strings still need a deep clone here to avoid buffer aliasing.
                void* kp = (char*)m->keys + idx * m->key_size;
                if (m->key_materialize == (__gorget_drop_fn)gorget_string_materialize_inplace) {
                    Str* ks = (Str*)kp;
                    if (ks->cap > 0 && ks->len > 0) {
                        *ks = gorget_string_clone_to_owned(ks);
                    }
                } else {
                    m->key_clone(kp);
                }
            }
            if (m->val_size > 0 && m->val_clone) {
                void* vp = (char*)m->values + idx * m->val_size;
                if (m->val_materialize == (__gorget_drop_fn)gorget_string_materialize_inplace) {
                    Str* vs = (Str*)vp;
                    if (vs->cap > 0 && vs->len > 0) {
                        *vs = gorget_string_clone_to_owned(vs);
                    }
                } else {
                    m->val_clone(vp);
                }
            }
            return;
        }
        idx = (idx + 1) & mask;
    }
}

static inline void* gorget_map_get(const GorgetMap* m, const void* key) {
    if (m->cap == 0) return NULL;
    size_t mask = m->cap - 1;  // cap is power-of-two (see __gorget_map_grow)
    uint64_t h = __GORGET_MAP_HASH(m, key);
    size_t idx = (size_t)(h & mask);
    for (size_t __probes = 0; __probes < m->cap; __probes++) {
        if (m->states[idx] == 0) return NULL;
        if (m->states[idx] == 1 && __GORGET_MAP_EQ(m, idx, key)) {
            if (m->val_size == 0) return (void*)1;  // Set mode: non-NULL means present
            return (char*)m->values + idx * m->val_size;
        }
        idx = (idx + 1) & mask;
    }
    return NULL;
}

static inline bool gorget_map_contains(const GorgetMap* m, const void* key) {
    return gorget_map_get(m, key) != NULL;
}

static inline size_t gorget_map_len(const GorgetMap* m) {
    return m->count;
}

static inline bool gorget_map_remove(GorgetMap* m, const void* key) {
    if (m->cap == 0) return false;
    size_t mask = m->cap - 1;  // cap is power-of-two (see __gorget_map_grow)
    uint64_t h = __GORGET_MAP_HASH(m, key);
    size_t idx = (size_t)(h & mask);
    for (size_t __probes = 0; __probes < m->cap; __probes++) {
        if (m->states[idx] == 0) return false;
        if (m->states[idx] == 1 && __GORGET_MAP_EQ(m, idx, key)) {
            // Drop the value and key before tombstoning the slot.
            if (m->val_drop && m->values)
                m->val_drop((char*)m->values + idx * m->val_size);
            if (m->key_drop && m->keys)
                m->key_drop((char*)m->keys + idx * m->key_size);
            m->states[idx] = 2;  // tombstone
            m->count--;
            m->tombstones++;
            return true;
        }
        idx = (idx + 1) & mask;
    }
    return false;
}

// Remove the entry at `key` and return a pointer to a thread-local copy of the
// removed value. Returns NULL when the key is absent. The caller owns the
// returned value: the map drops its key (but NOT its value — ownership transfers
// to the caller). Pointer validity: until the next gorget_map_remove_opt call
// on this thread.
static inline void* gorget_map_remove_opt(GorgetMap* m, const void* key) {
    static _Thread_local char __map_remove_buf[4096];
    static _Thread_local char* __map_remove_heap = NULL;
    static _Thread_local size_t __map_remove_heap_cap = 0;
    if (m->cap == 0) return NULL;
    size_t mask = m->cap - 1;  // cap is power-of-two (see __gorget_map_grow)
    uint64_t h = __GORGET_MAP_HASH(m, key);
    size_t idx = (size_t)(h & mask);
    for (size_t __probes = 0; __probes < m->cap; __probes++) {
        if (m->states[idx] == 0) return NULL;
        if (m->states[idx] == 1 && __GORGET_MAP_EQ(m, idx, key)) {
            char* buf;
            if (m->val_size <= sizeof(__map_remove_buf)) {
                buf = __map_remove_buf;
            } else {
                if (m->val_size > __map_remove_heap_cap) {
                    free(__map_remove_heap);
                    __map_remove_heap = (char*)malloc(m->val_size);
                    __map_remove_heap_cap = m->val_size;
                }
                buf = __map_remove_heap;
            }
            if (m->val_size > 0 && m->values) {
                memcpy(buf, (char*)m->values + idx * m->val_size, m->val_size);
            }
            // Drop the key; transfer value ownership to caller (no val_drop).
            if (m->key_drop && m->keys)
                m->key_drop((char*)m->keys + idx * m->key_size);
            m->states[idx] = 2;  // tombstone
            m->count--;
            m->tombstones++;
            return m->val_size > 0 ? (void*)buf : (void*)1;
        }
        idx = (idx + 1) & mask;
    }
    return NULL;
}

static inline void gorget_map_clear(GorgetMap* m) {
    if (m->states) memset(m->states, 0, m->cap);
    m->count = 0;
    m->order_len = 0;
    m->tombstones = 0;
}

static inline void gorget_map_free(GorgetMap* m) {
    if (!m->alloc) return;
    if (m->states) {
        for (size_t i = 0; i < m->cap; i++) {
            if (m->states[i] == 1) {
                if (m->val_drop && m->values)
                    m->val_drop((char*)m->values + i * m->val_size);
                if (m->key_drop && m->keys)
                    m->key_drop((char*)m->keys + i * m->key_size);
            }
        }
    }
    GorgetAllocator* a = m->alloc;
    if (m->keys) a->dealloc(a->ctx, m->keys, m->cap * m->key_size);
    if (m->values) a->dealloc(a->ctx, m->values, m->cap * m->val_size);
    if (m->states) a->dealloc(a->ctx, m->states, m->cap);
    if (m->order) a->dealloc(a->ctx, m->order, m->cap * sizeof(size_t));
    m->keys = NULL;
    m->values = NULL;
    m->states = NULL;
    m->order = NULL;
    m->count = 0;
    m->cap = 0;
    m->order_len = 0;
    m->tombstones = 0;
}

static inline bool gorget_map_is_empty(const GorgetMap* m) { return m->count == 0; }

static inline GorgetMap gorget_map_clone(const GorgetMap* src) {
    __gorget_map_clone_count++;
    GorgetAllocator* a = __gorget_current_alloc;
    GorgetMap dst;
    memset(&dst, 0, sizeof(dst));
    dst.key_size = src->key_size;
    dst.val_size = src->val_size;
    dst.hash_fn = src->hash_fn;
    dst.eq_fn = src->eq_fn;
    dst.val_drop = src->val_drop;
    dst.val_clone = src->val_clone;
    dst.key_drop = src->key_drop;
    dst.key_clone = src->key_clone;
    dst.val_materialize = src->val_materialize;
    dst.key_materialize = src->key_materialize;
    dst.alloc = a;
    if (src->cap > 0) {
        dst.keys = a->alloc(a->ctx, src->cap * src->key_size);
        dst.values = a->alloc(a->ctx, src->cap * src->val_size);
        dst.states = a->alloc(a->ctx, src->cap);
        memcpy(dst.keys, src->keys, src->cap * src->key_size);
        memcpy(dst.values, src->values, src->cap * src->val_size);
        memcpy(dst.states, src->states, src->cap);
        dst.cap = src->cap;
        dst.count = src->count;
        dst.tombstones = src->tombstones;
        // Preserve "ordered Dict" status even when src is empty (order_len == 0).
        // gorget_dict_new allocates `order` up front so put() takes the ordered
        // branch; cloning must propagate that, otherwise the clone silently
        // degrades to an unordered HashMap and subsequent puts iterate in hash
        // bucket order instead of insertion order. (gorget-js snag #4.)
        if (src->order) {
            dst.order = a->alloc(a->ctx, src->cap * sizeof(size_t));
            if (src->order_len > 0) {
                memcpy(dst.order, src->order, src->order_len * sizeof(size_t));
            }
            dst.order_len = src->order_len;
        }
        // Deep-clone resource-type values so the copy is independent.
        if (dst.val_clone) {
            for (size_t i = 0; i < dst.cap; i++) {
                if (dst.states[i] == 1) {
                    dst.val_clone((char*)dst.values + i * dst.val_size);
                }
            }
        }
        // Deep-clone resource-type keys so the copy is independent.
        if (dst.key_clone) {
            for (size_t i = 0; i < dst.cap; i++) {
                if (dst.states[i] == 1) {
                    dst.key_clone((char*)dst.keys + i * dst.key_size);
                }
            }
        } else if (dst.key_drop) {
            // Legacy fallback: string keys without key_clone
            for (size_t i = 0; i < dst.cap; i++) {
                if (dst.states[i] == 1) {
                    Str* key = (Str*)((char*)dst.keys + i * dst.key_size);
                    if (key->len > 0) {
                        *key = gorget_string_clone(key);
                    }
                }
            }
        }
    }
    return dst;
}

static inline void gorget_map_clone_inplace(void* p) {
    GorgetMap* m = (GorgetMap*)p;
    *m = gorget_map_clone(m);
}
static inline void gorget_set_clone_inplace(void* p) {
    GorgetMap* s = (GorgetMap*)p;  // GorgetSet == GorgetMap
    *s = gorget_map_clone(s);
}

// Ordered iteration: keys → GorgetArray
static inline GorgetArray gorget_map_keys(const GorgetMap* m) {
    GorgetArray result = gorget_array_new(m->key_size);
    if (m->key_drop) {
        result.elem_drop = m->key_drop;
    }
    // Propagate the element clone hook too: the result owns deep-cloned
    // resource keys (below), so a subsequent `k.clone()` must per-element
    // clone them. Without this, gorget_array_clone does a shallow buffer
    // copy while still propagating elem_drop → double-free. (Mirrors the
    // elem_drop guard above; covers Set→array via gorget_set_to_array.)
    if (m->key_clone) {
        result.elem_clone = m->key_clone;
    }
    if (m->order != NULL) {
        for (size_t oi = 0; oi < m->order_len; oi++) {
            size_t i = m->order[oi];
            if (m->states[i] != 1) continue;
            gorget_array_push(&result, (char*)m->keys + i * m->key_size);
            // Clone the key so the result owns an independent copy
            if (m->key_drop) {
                Str* key = (Str*)gorget_array_get(&result, result.len - 1);
                if (key->len > 0) *key = gorget_string_clone(key);
            }
        }
    } else {
        for (size_t i = 0; i < m->cap; i++) {
            if (m->states[i] == 1) {
                gorget_array_push(&result, (char*)m->keys + i * m->key_size);
                if (m->key_drop) {
                    Str* key = (Str*)gorget_array_get(&result, result.len - 1);
                    if (key->len > 0) *key = gorget_string_clone(key);
                }
            }
        }
    }
    return result;
}

// Ordered iteration: values → GorgetArray
static inline GorgetArray gorget_map_values(const GorgetMap* m) {
    GorgetArray result = gorget_array_new(m->val_size);
    if (m->order != NULL) {
        for (size_t oi = 0; oi < m->order_len; oi++) {
            size_t i = m->order[oi];
            if (m->states[i] != 1) continue;
            gorget_array_push(&result, (char*)m->values + i * m->val_size);
        }
    } else {
        for (size_t i = 0; i < m->cap; i++) {
            if (m->states[i] == 1) {
                gorget_array_push(&result, (char*)m->values + i * m->val_size);
            }
        }
    }
    return result;
}

// Ordered iteration: items (key+value pairs) → GorgetArray
// Each element is key_size + val_size bytes: key followed by value.
static inline GorgetArray gorget_map_items(const GorgetMap* m) {
    size_t item_size = m->key_size + m->val_size;
    GorgetArray result = gorget_array_new(item_size);
    // Allocate a temporary buffer for assembling each item
    char* tmp = (char*)alloca(item_size);
    if (m->order != NULL) {
        for (size_t oi = 0; oi < m->order_len; oi++) {
            size_t i = m->order[oi];
            if (m->states[i] != 1) continue;
            memcpy(tmp, (char*)m->keys + i * m->key_size, m->key_size);
            memcpy(tmp + m->key_size, (char*)m->values + i * m->val_size, m->val_size);
            gorget_array_push(&result, tmp);
        }
    } else {
        for (size_t i = 0; i < m->cap; i++) {
            if (m->states[i] == 1) {
                memcpy(tmp, (char*)m->keys + i * m->key_size, m->key_size);
                memcpy(tmp + m->key_size, (char*)m->values + i * m->val_size, m->val_size);
                gorget_array_push(&result, tmp);
            }
        }
    }
    return result;
}

// ── Dict/Set iteration accessors ──────────────────────────────
// Used by GIR lowering to iterate over map/set entries without
// InlineC. All accessors are trivial field reads that the C
// compiler will inline to zero overhead.

static inline int64_t gorget_map_iter_cap(const void* m) {
    return (int64_t)((const GorgetMap*)m)->cap;
}
static inline int64_t gorget_map_iter_state(const void* m, int64_t idx) {
    return (int64_t)((const GorgetMap*)m)->states[(size_t)idx];
}
static inline int64_t gorget_map_iter_order_len(const void* m) {
    return (int64_t)((const GorgetMap*)m)->order_len;
}
static inline int64_t gorget_map_iter_order(const void* m, int64_t idx) {
    return (int64_t)((const GorgetMap*)m)->order[(size_t)idx];
}
static inline void gorget_map_iter_key(const void* m, int64_t idx, void* out) {
    const GorgetMap* mm = (const GorgetMap*)m;
    memcpy(out, (const char*)mm->keys + (size_t)idx * mm->key_size, mm->key_size);
    // Resource-typed keys (String, Vector, …) need an independent owned
    // copy at the caller — the memcpy above produces a shallow alias of
    // the map's storage. `key_clone` is the in-place clone wrapper
    // installed at map construction (NULL for trivially-copyable keys
    // like int / bool / cstr).
    if (mm->key_clone) {
        mm->key_clone(out);
    }
}
static inline void gorget_map_iter_value(const void* m, int64_t idx, void* out) {
    const GorgetMap* mm = (const GorgetMap*)m;
    memcpy(out, (const char*)mm->values + (size_t)idx * mm->val_size, mm->val_size);
    // Same rationale as `gorget_map_iter_key`: clone resource-typed values.
    if (mm->val_clone) {
        mm->val_clone(out);
    }
}

// ── Dict/Set drain (consuming iteration) ──────────────────────
// Move K (and V for Dict) out of the bucket at physical index
// `phys_idx` and tombstone-mark the slot. Caller takes ownership;
// no key_drop / val_drop is called here. The map's normal drop
// path (`gorget_map_free`) checks `states[i] == 1` and skips
// tombstones, so no double-free for drained entries.
//
// Returns 1 if a value was extracted (slot was occupied), 0 if the
// slot was empty or already tombstoned. Drainable callers can use
// the return to skip empty slots without a separate state lookup.
//
// `m` is `const void*` to match Gorget's `Ref[Dict[K, V]]` /
// `Ref[Set[T]]` extern-parameter convention (mirrors
// `gorget_map_iter_key` which has the same shape and likewise
// writes through the const pointer). The function casts to a
// non-const GorgetMap pointer internally to flip `states[idx]`
// and tweak `count` / `tombstones`. Drain reuses the same `order`
// array as the iter accessors for insertion-order traversal.
static inline int64_t gorget_map_drain_entry(const void* m, int64_t phys_idx, void* out_key, void* out_val) {
    GorgetMap* mm = (GorgetMap*)m;  // cast away const — see comment above
    size_t idx = (size_t)phys_idx;
    if (mm->states[idx] != 1) return 0;  // already drained or never occupied
    memcpy(out_key, (const char*)mm->keys + idx * mm->key_size, mm->key_size);
    if (mm->val_size > 0 && mm->values && out_val) {
        memcpy(out_val, (const char*)mm->values + idx * mm->val_size, mm->val_size);
    }
    mm->states[idx] = 2;  // tombstone
    if (mm->count > 0) mm->count--;
    mm->tombstones++;
    return 1;
}

// Set drain — thin alias for `gorget_map_drain_entry` with NULL
// value-out. GorgetSet == GorgetMap with val_size==0.
static inline int64_t gorget_set_drain_entry(const void* s, int64_t phys_idx, void* out_key) {
    return gorget_map_drain_entry(s, phys_idx, out_key, NULL);
}

