
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
// Dense-mode counterpart of __GORGET_MAP_EQ: `entries_i` addresses the packed
// `entries_keys` array rather than the sparse `keys` bucket table.
#define __GORGET_MAP_EQ_DENSE(m, entries_i, key) ((m)->eq_fn ? (m)->eq_fn((const char*)(m)->entries_keys + (entries_i) * (m)->key_size, key) : __gorget_key_eq_default((const char*)(m)->entries_keys + (entries_i) * (m)->key_size, key, (m)->key_size))

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

// D39 dense-mode grow (coupled): doubles entries_cap AND rehashes indices in
// one step. Invariant: `indices_cap == 2 * entries_cap` at all times (50% load
// factor, no separate indices_cap field). Initial grow bumps 0→16 entries /
// 0→32 indices; subsequent grows double both. Rehash rebuilds indices from
// entries[0..entries_len] since the mask changed — tombstones (-2 sentinels
// in the old indices array) are naturally dropped by the rebuild, since we
// re-hash only from live entries.
//
// LIVE after A.2c activation — ctors allocate entries_keys, so this path is
// on the hot allocation road for every Dict/OrderedSet grow.
static inline void __gorget_map_dense_grow(GorgetMap* m) {
    GorgetAllocator* a = m->alloc;
    size_t old_cap = m->entries_cap;
    size_t old_indices_cap = 2 * old_cap;
    size_t new_cap = old_cap == 0 ? 16 : old_cap * 2;
    size_t new_indices_cap = 2 * new_cap;
    size_t new_mask = new_indices_cap - 1;  // power-of-two: 32/64/128/...

    // Realloc entries_keys (copy live prefix, drop old)
    void* new_keys = a->alloc(a->ctx, new_cap * m->key_size);
    if (m->entries_keys && m->entries_len > 0) {
        memcpy(new_keys, m->entries_keys, m->entries_len * m->key_size);
    }
    if (m->entries_keys) a->dealloc(a->ctx, m->entries_keys, old_cap * m->key_size);
    m->entries_keys = new_keys;

    // Realloc entries_values (only when val_size > 0 — Sets skip this)
    if (m->val_size > 0) {
        void* new_values = a->alloc(a->ctx, new_cap * m->val_size);
        if (m->entries_values && m->entries_len > 0) {
            memcpy(new_values, m->entries_values, m->entries_len * m->val_size);
        }
        if (m->entries_values) a->dealloc(a->ctx, m->entries_values, old_cap * m->val_size);
        m->entries_values = new_values;
    }

    // Realloc + rehash indices from scratch (mask changed → old positions invalid)
    if (m->indices) a->dealloc(a->ctx, m->indices, old_indices_cap * sizeof(int32_t));
    m->indices = (int32_t*)a->alloc(a->ctx, new_indices_cap * sizeof(int32_t));
    for (size_t s = 0; s < new_indices_cap; s++) m->indices[s] = -1;
    for (size_t i = 0; i < m->entries_len; i++) {
        const void* key = (const char*)m->entries_keys + i * m->key_size;
        uint64_t h = __GORGET_MAP_HASH(m, key);
        size_t idx = (size_t)(h & new_mask);
        while (m->indices[idx] >= 0) {
            idx = (idx + 1) & new_mask;
        }
        m->indices[idx] = (int32_t)i;
    }

    m->entries_cap = new_cap;
}

static inline GorgetMap gorget_map_new(size_t key_size, size_t val_size) {
    // Field order: { keys, cap, values, states, count, key_size, val_size, alloc, order, order_len, tombstones, hash_fn, eq_fn, val_drop, val_clone, key_drop, key_clone, val_materialize, key_materialize }
    return (GorgetMap){NULL, 0, NULL, NULL, 0, key_size, val_size, __gorget_current_alloc, NULL, 0, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};
}

// Pre-allocate hash table to hold at least `new_cap` entries without growing.
static inline void gorget_map_reserve(GorgetMap* m, int64_t new_cap) {
    if (new_cap <= 0) return;
    // Dense mode: grow entries_cap (coupled indices doubling handled by dense_grow).
    if (m->entries_keys) {
        if ((size_t)new_cap <= m->entries_cap) return;
        while (m->entries_cap < (size_t)new_cap) __gorget_map_dense_grow(m);
        return;
    }
    // Legacy mode.
    if ((size_t)new_cap <= m->cap) return;
    while (m->cap < (size_t)new_cap) __gorget_map_grow(m);
}

static inline void gorget_set_reserve(GorgetSet* s, int64_t new_cap) {
    gorget_map_reserve(s, new_cap);
}

// Ordered Dict (D39 dense-index-map layout — ATOMIC ACTIVATION in Phase A.2c).
//
// Dict / OrderedSet allocate the dense entries[] + indices[] pair up front so
// every put()/get()/remove() takes the dense branch that A.2a landed in the
// runtime and A.2b landed in the C-emit + SH mirror. Insertion order is
// preserved by the packed entries[] append (no sidecar `order` array), and
// remove()'s in-place memmove keeps entries[] contiguous (entries_len == count
// invariant is obvious). indices[] uses a two-sentinel protocol per
// runtime_preamble.c:383 — -1 empty (end-of-probe-chain), -2 tombstone (keep
// probing; put may reuse); rehash on grow compacts tombstones away. HashMap /
// HashSet stay legacy (open-addressing + states[] tombstones — their reason to
// exist); the two-way discriminator at runtime is `entries_keys != NULL`
// (dense) vs `states != NULL` (legacy).
//
// Initial sizing: entries_cap=16, indices_cap=32 (=2·entries_cap, 50% load
// factor, no separate indices_cap field). __gorget_map_dense_grow doubles
// both in lockstep.
static inline GorgetMap gorget_dict_new(size_t key_size, size_t val_size) {
    GorgetAllocator* a = __gorget_current_alloc;
    size_t init_entries_cap = 16;
    size_t init_indices_cap = 2 * init_entries_cap;  // 32; 50% load factor
    GorgetMap m;
    // Defensive zero-fill: gorget_dict_new uses field-by-field assignment on an
    // automatic variable, which is NOT zero-initialised in C. Legacy fields
    // (keys / values / states / order / cap / order_len) MUST stay NULL/0 so
    // the runtime's dense/legacy discriminator (`entries_keys != NULL`) routes
    // every Dict/Set op through the dense branch, and the legacy free-branch
    // never fires on a Dict/Set (it would ULLONG_MAX·key_size the NULL keys ptr).
    memset(&m, 0, sizeof(m));
    m.key_size = key_size;
    m.val_size = val_size;
    m.alloc = a;
    // Dense allocation — ATOMIC ACTIVATION MOMENT of D39 Phase A.
    m.entries_keys = a->alloc(a->ctx, init_entries_cap * key_size);
    m.entries_values = val_size > 0 ? a->alloc(a->ctx, init_entries_cap * val_size) : NULL;
    m.entries_cap = init_entries_cap;
    m.entries_len = 0;
    m.indices = (int32_t*)a->alloc(a->ctx, init_indices_cap * sizeof(int32_t));
    for (size_t i = 0; i < init_indices_cap; i++) m.indices[i] = -1;  // empty sentinel
    m.count = 0;
    m.tombstones = 0;  // dense has no tombstones; kept 0 for legacy-branch guards
    // Function-pointer sidecars — populated by _str variants + register_* hooks.
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
// idx semantics is caller-dictated: hash-slot-index in legacy mode, packed
// entries-index in dense mode. The mode branch picks the right base array.
static inline void __gorget_map_materialize_key(GorgetMap* m, size_t idx) {
    if (m->key_materialize) {
        if (m->entries_keys) {
            m->key_materialize((char*)m->entries_keys + idx * m->key_size);
        } else {
            m->key_materialize((char*)m->keys + idx * m->key_size);
        }
    }
}
static inline void __gorget_map_materialize_value(GorgetMap* m, size_t idx) {
    if (m->val_materialize) {
        if (m->entries_keys) {
            m->val_materialize((char*)m->entries_values + idx * m->val_size);
        } else {
            m->val_materialize((char*)m->values + idx * m->val_size);
        }
    }
}

static inline void gorget_map_put(GorgetMap* m, const void* key, const void* value) {
    // D39 DENSE MODE (LIVE after A.2c activation): append to entries[entries_len]
    // on miss, overwrite in place on hit. Grow when entries_len == entries_cap.
    // Insertion order is naturally preserved by the append; no separate order
    // array. Tombstone protocol: -1 empty (end of probe chain), -2 tombstone
    // (removed — keep probing, may reuse for insert). See gorget_map_remove.
    if (m->entries_keys) {
        if (m->entries_cap == 0 || m->entries_len >= m->entries_cap) {
            __gorget_map_dense_grow(m);
        }
        size_t indices_cap = 2 * m->entries_cap;
        size_t mask = indices_cap - 1;  // power-of-two
        uint64_t h = __GORGET_MAP_HASH(m, key);
        size_t idx = (size_t)(h & mask);
        size_t first_tombstone = (size_t)-1;
        for (;;) {
            int32_t ei = m->indices[idx];
            if (ei == -1) {
                // End of probe chain: insert. Prefer reusing the earliest
                // tombstone we passed so probe chains stay short.
                size_t insert_slot = first_tombstone != (size_t)-1 ? first_tombstone : idx;
                size_t new_i = m->entries_len;
                memcpy((char*)m->entries_keys + new_i * m->key_size, key, m->key_size);
                __gorget_map_materialize_key(m, new_i);
                if (m->val_size > 0 && value != NULL) {
                    memcpy((char*)m->entries_values + new_i * m->val_size, value, m->val_size);
                    __gorget_map_materialize_value(m, new_i);
                }
                m->indices[insert_slot] = (int32_t)new_i;
                m->entries_len++;
                m->count++;
                return;
            }
            if (ei == -2) {
                // Tombstone: remember earliest for possible reuse; keep probing
                // in case the key exists further down the chain.
                if (first_tombstone == (size_t)-1) first_tombstone = idx;
                idx = (idx + 1) & mask;
                continue;
            }
            if (ei >= 0 && __GORGET_MAP_EQ_DENSE(m, (size_t)ei, key)) {
                // Hit: overwrite value in place; key stays.
                if (m->val_size > 0 && value != NULL) {
                    if (m->val_drop) {
                        m->val_drop((char*)m->entries_values + (size_t)ei * m->val_size);
                    }
                    memcpy((char*)m->entries_values + (size_t)ei * m->val_size, value, m->val_size);
                    __gorget_map_materialize_value(m, (size_t)ei);
                }
                return;
            }
            idx = (idx + 1) & mask;
        }
    }
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
    // D39 DENSE MODE (LIVE after A.2c activation): re-probe indices to locate
    // the entries index of the just-inserted key, then deep-clone key/value in
    // place. Skip past -2 tombstones (put may reuse a tombstone slot, so the
    // insertion slot's original index_cap position can look like a tombstone
    // when there's a later live entry).
    if (m->entries_keys) {
        size_t indices_cap = 2 * m->entries_cap;
        size_t mask = indices_cap - 1;
        uint64_t h = __GORGET_MAP_HASH(m, key);
        size_t idx = (size_t)(h & mask);
        for (;;) {
            int32_t ei = m->indices[idx];
            if (ei == -1) return;  // shouldn't happen — we just put
            if (ei == -2) { idx = (idx + 1) & mask; continue; }
            if (ei >= 0 && __GORGET_MAP_EQ_DENSE(m, (size_t)ei, key)) {
                size_t entries_i = (size_t)ei;
                if (m->key_clone) {
                    void* kp = (char*)m->entries_keys + entries_i * m->key_size;
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
                    void* vp = (char*)m->entries_values + entries_i * m->val_size;
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
    // D39 DENSE MODE (LIVE after A.2c activation): -1 empty ends the probe
    // chain, -2 tombstone keeps probing (the key may exist further down).
    if (m->entries_keys) {
        if (m->entries_cap == 0) return NULL;
        size_t indices_cap = 2 * m->entries_cap;
        size_t mask = indices_cap - 1;
        uint64_t h = __GORGET_MAP_HASH(m, key);
        size_t idx = (size_t)(h & mask);
        for (;;) {
            int32_t ei = m->indices[idx];
            if (ei == -1) return NULL;
            if (ei == -2) { idx = (idx + 1) & mask; continue; }
            if (ei >= 0 && __GORGET_MAP_EQ_DENSE(m, (size_t)ei, key)) {
                if (m->val_size == 0) return (void*)1;
                return (char*)m->entries_values + (size_t)ei * m->val_size;
            }
            idx = (idx + 1) & mask;
        }
    }
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
    // D39 DENSE MODE (LIVE after A.2c activation): locate entries index i;
    // memmove entries[i+1..entries_len] down by 1; decrement any indices[j] > i;
    // MARK the freed indices slot as TOMBSTONE (-2), not empty (-1), so that
    // any probe chain that ran PAST this slot when the removed key was live
    // still continues probing to find its target. Writing -1 here would
    // silently truncate every such chain (Bug #2 from A.2c executor report:
    // dict_tombstone_stress observed 22 entries live where 21 expected). O(n)
    // but order-preserving. Rehash on grow drops tombstones (compacts).
    if (m->entries_keys) {
        if (m->entries_cap == 0) return false;
        size_t indices_cap = 2 * m->entries_cap;
        size_t mask = indices_cap - 1;
        uint64_t h = __GORGET_MAP_HASH(m, key);
        size_t idx = (size_t)(h & mask);
        for (;;) {
            int32_t ei = m->indices[idx];
            if (ei == -1) return false;
            if (ei == -2) { idx = (idx + 1) & mask; continue; }
            if (ei >= 0 && __GORGET_MAP_EQ_DENSE(m, (size_t)ei, key)) {
                size_t entries_i = (size_t)ei;
                if (m->val_drop && m->entries_values) {
                    m->val_drop((char*)m->entries_values + entries_i * m->val_size);
                }
                if (m->key_drop) {
                    m->key_drop((char*)m->entries_keys + entries_i * m->key_size);
                }
                size_t tail = m->entries_len - entries_i - 1;
                if (tail > 0) {
                    memmove((char*)m->entries_keys + entries_i * m->key_size,
                            (char*)m->entries_keys + (entries_i + 1) * m->key_size,
                            tail * m->key_size);
                    if (m->entries_values) {
                        memmove((char*)m->entries_values + entries_i * m->val_size,
                                (char*)m->entries_values + (entries_i + 1) * m->val_size,
                                tail * m->val_size);
                    }
                }
                m->entries_len--;
                m->count--;
                m->indices[idx] = -2;  // tombstone (protocol per runtime_preamble.c:383)
                for (size_t j = 0; j < indices_cap; j++) {
                    if (m->indices[j] > (int32_t)entries_i) m->indices[j]--;
                }
                return true;
            }
            idx = (idx + 1) & mask;
        }
    }
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
    // D39 DENSE MODE (LIVE after A.2c activation): mirror gorget_map_remove but
    // transfer the value bytes to the TLS buffer for Option[V] return before
    // memmove. Tombstone (-2) protocol matches gorget_map_remove.
    if (m->entries_keys) {
        if (m->entries_cap == 0) return NULL;
        size_t indices_cap = 2 * m->entries_cap;
        size_t mask = indices_cap - 1;
        uint64_t h = __GORGET_MAP_HASH(m, key);
        size_t idx = (size_t)(h & mask);
        for (;;) {
            int32_t ei = m->indices[idx];
            if (ei == -1) return NULL;
            if (ei == -2) { idx = (idx + 1) & mask; continue; }
            if (ei >= 0 && __GORGET_MAP_EQ_DENSE(m, (size_t)ei, key)) {
                size_t entries_i = (size_t)ei;
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
                if (m->val_size > 0 && m->entries_values) {
                    memcpy(buf, (char*)m->entries_values + entries_i * m->val_size, m->val_size);
                }
                // Drop key; value ownership transfers to caller (no val_drop).
                if (m->key_drop) {
                    m->key_drop((char*)m->entries_keys + entries_i * m->key_size);
                }
                size_t tail = m->entries_len - entries_i - 1;
                if (tail > 0) {
                    memmove((char*)m->entries_keys + entries_i * m->key_size,
                            (char*)m->entries_keys + (entries_i + 1) * m->key_size,
                            tail * m->key_size);
                    if (m->entries_values) {
                        memmove((char*)m->entries_values + entries_i * m->val_size,
                                (char*)m->entries_values + (entries_i + 1) * m->val_size,
                                tail * m->val_size);
                    }
                }
                m->entries_len--;
                m->count--;
                m->indices[idx] = -2;  // tombstone (protocol per runtime_preamble.c:383)
                for (size_t j = 0; j < indices_cap; j++) {
                    if (m->indices[j] > (int32_t)entries_i) m->indices[j]--;
                }
                return m->val_size > 0 ? (void*)buf : (void*)1;
            }
            idx = (idx + 1) & mask;
        }
    }
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

// D39 Phase A.3: O(1) `swap_remove` for the dense-index-map layout.
//
// swap_remove locates the entries[] slot for `key`; if the slot is the LAST
// live entry, drops-and-pops; otherwise moves the last entry's key+value bytes
// into the freed slot, rewrites the SWAPPED entry's indices[] slot to point
// at its new position, tombstones the removed key's indices[] slot (-2, same
// protocol as `gorget_map_remove`), and decrements entries_len + count.
//
// Order-DESTROYING (unlike `gorget_map_remove`, which is O(n) shift). Callers
// opt in at the method name (`Dict.swap_remove` / `Set.swap_remove`).
//
// Returns true iff the key was present.
static inline bool gorget_map_swap_remove(GorgetMap* m, const void* key) {
    if (m->entries_keys) {
        if (m->entries_cap == 0) return false;
        size_t indices_cap = 2 * m->entries_cap;
        size_t mask = indices_cap - 1;
        uint64_t h = __GORGET_MAP_HASH(m, key);
        size_t idx = (size_t)(h & mask);
        for (;;) {
            int32_t ei = m->indices[idx];
            if (ei == -1) return false;
            if (ei == -2) { idx = (idx + 1) & mask; continue; }
            if (ei >= 0 && __GORGET_MAP_EQ_DENSE(m, (size_t)ei, key)) {
                size_t entries_i = (size_t)ei;
                size_t last_i = m->entries_len - 1;
                // Drop the removed slot's value and key.
                if (m->val_drop && m->entries_values) {
                    m->val_drop((char*)m->entries_values + entries_i * m->val_size);
                }
                if (m->key_drop) {
                    m->key_drop((char*)m->entries_keys + entries_i * m->key_size);
                }
                if (entries_i != last_i) {
                    // Swap the LAST entry's key+value bytes into the freed slot.
                    memcpy((char*)m->entries_keys + entries_i * m->key_size,
                           (char*)m->entries_keys + last_i * m->key_size,
                           m->key_size);
                    if (m->entries_values) {
                        memcpy((char*)m->entries_values + entries_i * m->val_size,
                               (char*)m->entries_values + last_i * m->val_size,
                               m->val_size);
                    }
                    // Rewrite the SWAPPED entry's indices[] slot to point at
                    // its new position (entries_i). Walk the probe chain for
                    // the swapped key; the slot currently holds `last_i`.
                    for (size_t j = 0; j < indices_cap; j++) {
                        if (m->indices[j] == (int32_t)last_i) {
                            m->indices[j] = (int32_t)entries_i;
                            break;
                        }
                    }
                }
                m->entries_len--;
                m->count--;
                // Tombstone (-2) so probe chains that ran PAST this slot when
                // the removed key was live still continue to their target.
                // Same protocol as gorget_map_remove.
                m->indices[idx] = -2;
                return true;
            }
            idx = (idx + 1) & mask;
        }
    }
    // Legacy sparse-buckets path (present when dense mode never activated for
    // this map — e.g. a HashMap ctor). Delegates to gorget_map_remove: the
    // sparse path is unordered so remove and swap_remove are semantically
    // equivalent (both tombstone the slot).
    return gorget_map_remove(m, key);
}

// D39 Phase A.3: O(1) swap_remove_opt — mirrors gorget_map_swap_remove but
// transfers the value bytes to a thread-local buffer for Option[V] return
// before the swap. Ownership of the returned value transfers to the caller
// (no val_drop on the removed slot). Pointer validity: until the next
// gorget_map_swap_remove_opt / gorget_map_remove_opt call on this thread.
static inline void* gorget_map_swap_remove_opt(GorgetMap* m, const void* key) {
    static _Thread_local char __map_swap_remove_buf[4096];
    static _Thread_local char* __map_swap_remove_heap = NULL;
    static _Thread_local size_t __map_swap_remove_heap_cap = 0;
    if (m->entries_keys) {
        if (m->entries_cap == 0) return NULL;
        size_t indices_cap = 2 * m->entries_cap;
        size_t mask = indices_cap - 1;
        uint64_t h = __GORGET_MAP_HASH(m, key);
        size_t idx = (size_t)(h & mask);
        for (;;) {
            int32_t ei = m->indices[idx];
            if (ei == -1) return NULL;
            if (ei == -2) { idx = (idx + 1) & mask; continue; }
            if (ei >= 0 && __GORGET_MAP_EQ_DENSE(m, (size_t)ei, key)) {
                size_t entries_i = (size_t)ei;
                size_t last_i = m->entries_len - 1;
                char* buf;
                if (m->val_size <= sizeof(__map_swap_remove_buf)) {
                    buf = __map_swap_remove_buf;
                } else {
                    if (m->val_size > __map_swap_remove_heap_cap) {
                        free(__map_swap_remove_heap);
                        __map_swap_remove_heap = (char*)malloc(m->val_size);
                        __map_swap_remove_heap_cap = m->val_size;
                    }
                    buf = __map_swap_remove_heap;
                }
                // Transfer value to TLS buffer BEFORE the swap overwrites it.
                if (m->val_size > 0 && m->entries_values) {
                    memcpy(buf, (char*)m->entries_values + entries_i * m->val_size, m->val_size);
                }
                // Drop key; value ownership transfers to caller.
                if (m->key_drop) {
                    m->key_drop((char*)m->entries_keys + entries_i * m->key_size);
                }
                if (entries_i != last_i) {
                    memcpy((char*)m->entries_keys + entries_i * m->key_size,
                           (char*)m->entries_keys + last_i * m->key_size,
                           m->key_size);
                    if (m->entries_values) {
                        memcpy((char*)m->entries_values + entries_i * m->val_size,
                               (char*)m->entries_values + last_i * m->val_size,
                               m->val_size);
                    }
                    for (size_t j = 0; j < indices_cap; j++) {
                        if (m->indices[j] == (int32_t)last_i) {
                            m->indices[j] = (int32_t)entries_i;
                            break;
                        }
                    }
                }
                m->entries_len--;
                m->count--;
                m->indices[idx] = -2;  // tombstone
                return m->val_size > 0 ? (void*)buf : (void*)1;
            }
            idx = (idx + 1) & mask;
        }
    }
    // Legacy sparse-buckets path: delegates to gorget_map_remove_opt (sparse
    // is unordered → semantically equivalent).
    return gorget_map_remove_opt(m, key);
}

static inline void gorget_map_clear(GorgetMap* m) {
    // D39 DENSE MODE (dormant in A.2a): drop each live entry, reset entries_len
    // and count, and re-init indices to -1. Retain allocations.
    if (m->entries_keys) {
        for (size_t i = 0; i < m->entries_len; i++) {
            if (m->val_drop && m->entries_values) {
                m->val_drop((char*)m->entries_values + i * m->val_size);
            }
            if (m->key_drop) {
                m->key_drop((char*)m->entries_keys + i * m->key_size);
            }
        }
        m->entries_len = 0;
        m->count = 0;
        if (m->indices) {
            size_t indices_cap = 2 * m->entries_cap;
            for (size_t s = 0; s < indices_cap; s++) m->indices[s] = -1;
        }
        return;
    }
    if (m->states) memset(m->states, 0, m->cap);
    m->count = 0;
    m->order_len = 0;
    m->tombstones = 0;
}

static inline void gorget_map_free(GorgetMap* m) {
    if (!m->alloc) return;
    // D39 DENSE MODE (dormant in A.2a): drop entries[0..entries_len), free the
    // three dense arrays, null them.
    if (m->entries_keys) {
        for (size_t i = 0; i < m->entries_len; i++) {
            if (m->val_drop && m->entries_values) {
                m->val_drop((char*)m->entries_values + i * m->val_size);
            }
            if (m->key_drop) {
                m->key_drop((char*)m->entries_keys + i * m->key_size);
            }
        }
        GorgetAllocator* a = m->alloc;
        a->dealloc(a->ctx, m->entries_keys, m->entries_cap * m->key_size);
        if (m->entries_values) a->dealloc(a->ctx, m->entries_values, m->entries_cap * m->val_size);
        if (m->indices) a->dealloc(a->ctx, m->indices, 2 * m->entries_cap * sizeof(int32_t));
        m->entries_keys = NULL;
        m->entries_values = NULL;
        m->indices = NULL;
        m->entries_len = 0;
        m->entries_cap = 0;
        m->count = 0;
        return;
    }
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
    // D39 DENSE MODE (dormant in A.2a): clone entries + indices; skip the
    // legacy sparse-buckets path entirely. `dst` was memset above, so all
    // legacy fields stay zero. Preserves order via the packed entries array.
    if (src->entries_keys) {
        dst.entries_cap = src->entries_cap;
        dst.entries_len = src->entries_len;
        dst.count = src->count;
        if (src->entries_cap > 0) {
            dst.entries_keys = a->alloc(a->ctx, src->entries_cap * src->key_size);
            memcpy(dst.entries_keys, src->entries_keys, src->entries_len * src->key_size);
            if (src->val_size > 0 && src->entries_values) {
                dst.entries_values = a->alloc(a->ctx, src->entries_cap * src->val_size);
                memcpy(dst.entries_values, src->entries_values, src->entries_len * src->val_size);
            }
            size_t indices_cap = 2 * src->entries_cap;
            if (src->indices) {
                dst.indices = (int32_t*)a->alloc(a->ctx, indices_cap * sizeof(int32_t));
                memcpy(dst.indices, src->indices, indices_cap * sizeof(int32_t));
            }
            // Deep-clone resource-typed values so the copy is independent.
            if (dst.val_clone && dst.entries_values) {
                for (size_t i = 0; i < dst.entries_len; i++) {
                    dst.val_clone((char*)dst.entries_values + i * dst.val_size);
                }
            }
            // Deep-clone resource-typed keys so the copy is independent.
            if (dst.key_clone) {
                for (size_t i = 0; i < dst.entries_len; i++) {
                    dst.key_clone((char*)dst.entries_keys + i * dst.key_size);
                }
            } else if (dst.key_drop) {
                for (size_t i = 0; i < dst.entries_len; i++) {
                    Str* key = (Str*)((char*)dst.entries_keys + i * dst.key_size);
                    if (key->len > 0) *key = gorget_string_clone(key);
                }
            }
        }
        return dst;
    }
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
    // D39 DENSE MODE (dormant in A.2a): entries[] is packed and already in
    // insertion order — walk it directly.
    if (m->entries_keys) {
        for (size_t i = 0; i < m->entries_len; i++) {
            gorget_array_push(&result, (char*)m->entries_keys + i * m->key_size);
            if (m->key_drop) {
                Str* key = (Str*)gorget_array_get(&result, result.len - 1);
                if (key->len > 0) *key = gorget_string_clone(key);
            }
        }
        return result;
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
    // D39 DENSE MODE (dormant in A.2a).
    if (m->entries_keys) {
        for (size_t i = 0; i < m->entries_len; i++) {
            gorget_array_push(&result, (char*)m->entries_values + i * m->val_size);
        }
        return result;
    }
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
    // D39 DENSE MODE (dormant in A.2a).
    if (m->entries_keys) {
        for (size_t i = 0; i < m->entries_len; i++) {
            memcpy(tmp, (char*)m->entries_keys + i * m->key_size, m->key_size);
            memcpy(tmp + m->key_size, (char*)m->entries_values + i * m->val_size, m->val_size);
            gorget_array_push(&result, tmp);
        }
        return result;
    }
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

// Iterator accessors — dormant dense-mode branches (A.2a).
//
// Dense mode collapses the "iterate the sparse bucket table skipping empty
// slots via `order`" pattern to a straight [0..entries_len) walk over the
// packed entries arrays. Under dense: `iter_cap` and `iter_order_len` both
// return entries_len; `iter_state` is always 1 (all in-range indices are
// live); `iter_order` is identity; key/value reads pull from
// entries_keys/entries_values.
static inline int64_t gorget_map_iter_cap(const void* m) {
    const GorgetMap* mm = (const GorgetMap*)m;
    if (mm->entries_keys) return (int64_t)mm->entries_len;
    return (int64_t)mm->cap;
}
static inline int64_t gorget_map_iter_state(const void* m, int64_t idx) {
    const GorgetMap* mm = (const GorgetMap*)m;
    if (mm->entries_keys) return 1;
    return (int64_t)mm->states[(size_t)idx];
}
static inline int64_t gorget_map_iter_order_len(const void* m) {
    const GorgetMap* mm = (const GorgetMap*)m;
    if (mm->entries_keys) return (int64_t)mm->entries_len;
    return (int64_t)mm->order_len;
}
static inline int64_t gorget_map_iter_order(const void* m, int64_t idx) {
    const GorgetMap* mm = (const GorgetMap*)m;
    if (mm->entries_keys) return idx;
    return (int64_t)mm->order[(size_t)idx];
}
static inline void gorget_map_iter_key(const void* m, int64_t idx, void* out) {
    const GorgetMap* mm = (const GorgetMap*)m;
    if (mm->entries_keys) {
        memcpy(out, (const char*)mm->entries_keys + (size_t)idx * mm->key_size, mm->key_size);
    } else {
        memcpy(out, (const char*)mm->keys + (size_t)idx * mm->key_size, mm->key_size);
    }
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
    if (mm->entries_keys) {
        memcpy(out, (const char*)mm->entries_values + (size_t)idx * mm->val_size, mm->val_size);
    } else {
        memcpy(out, (const char*)mm->values + (size_t)idx * mm->val_size, mm->val_size);
    }
    // Same rationale as `gorget_map_iter_key`: clone resource-typed values.
    if (mm->val_clone) {
        mm->val_clone(out);
    }
}

// ── Dict/Set drain (consuming iteration) ──────────────────────
// Move K (and V for Dict) out of the bucket at physical index
// `phys_idx` and mark the slot vacated so the map's drop path
// doesn't double-drop. Caller takes ownership of the moved-out
// bytes; no key_drop / val_drop is called here.
//
// Legacy sparse path: flip `states[idx] = 2` (tombstone) so
// `gorget_map_free` skips the slot.
//
// Dense path: `entries[0..entries_len]` is a packed live array
// with no per-slot state discriminator. Instead of tracking a
// per-slot dead marker or shrinking `entries_len` mid-iteration
// (which would break the drain iterator's `n = iter_order_len`
// cursor loop — see `lib/std/iter.gg::DictDrain::next`), we
// **memset the slot to zero after transfer**. All Gorget owned
// types treat all-zero bytes as the empty/default state whose
// drop is a no-op: `Str`/`GorgetString` (`cap == 0` → view →
// free is no-op), `GorgetArray` (`data == NULL` → skips both
// the per-element drop loop and the dealloc), `GorgetMap`
// (`alloc == NULL` → free short-circuits), `GorgetClosure`
// (`env == NULL` → free is no-op), and user structs recursively
// via their zero'd fields. So `gorget_map_free` can iterate
// `entries[0..entries_len]` unchanged; its per-slot key_drop
// and val_drop calls on the memset-zeroed drained slots are
// safe no-ops. This preserves the iterator's contract (n stays
// stable across all `next()` calls) and needs no new field.
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
    if (mm->entries_keys) {
        if (idx >= mm->entries_len) return 0;
        memcpy(out_key, (const char*)mm->entries_keys + idx * mm->key_size, mm->key_size);
        if (mm->val_size > 0 && mm->entries_values && out_val) {
            memcpy(out_val, (const char*)mm->entries_values + idx * mm->val_size, mm->val_size);
        }
        // VACATE the slot so gorget_map_free's per-slot drop-hook loop
        // treats it as dead. See rationale in the block comment above.
        memset((char*)mm->entries_keys + idx * mm->key_size, 0, mm->key_size);
        if (mm->val_size > 0 && mm->entries_values) {
            memset((char*)mm->entries_values + idx * mm->val_size, 0, mm->val_size);
        }
        if (mm->count > 0) mm->count--;
        return 1;
    }
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

