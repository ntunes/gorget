
// ── String (32-byte fat struct with generic view sentinel) ────
// Str is a 4-field struct, 32 bytes total:
//
//     struct Str {
//         void*            data;   // offset 0  — UTF-8 bytes (not NUL-terminated by contract)
//         size_t           cap;    // offset 8  — allocated bytes for data buffer (0 = VIEW)
//         size_t           len;    // offset 16 — byte length (authoritative)
//         GorgetAllocator* alloc;  // offset 24 — allocator for owned buffer (NULL for views)
//     };
//
// Invariants:
//   * cap == 0 ⟺ view  (borrowed buffer — may point into .rodata or another Str)
//   * cap >  0 ⟺ owned (we free `data` via `alloc->dealloc(data, cap)` at drop time)
//   * len is authoritative regardless of owned/view
//
// String literals are views into .rodata:
//   static const Str __slit_N = { .data = "hello", .cap = 0, .len = 5, .alloc = NULL };
//
// Field order `{data, cap, len, alloc}` puts `cap` at offset +8, matching the
// generic view-discriminator prefix shared with GorgetArray / GorgetMap / GorgetSet.

typedef struct {
    char*            data;   // char* instead of void* so .data is directly usable as const char*
    size_t           cap;
    size_t           len;
    GorgetAllocator* alloc;
} Str;

typedef Str GorgetString;
// StringHeader typedef removed — no longer needed. Str IS the header.

// Legacy accessor macros — no longer used by any runtime or emit code.
// Kept as documentation of the compat layer used during migration.
// #define STR_LEN(s)  ((s).len)
// #define STR_CAP(s)  ((s).cap)
// #define STR_HDR(s)  (&(s))

// Generic view discriminator shared across resource types: the field at
// offset +8 is "cap"; 0 means the resource is a view, nonzero means owned.
static inline bool gorget_is_view(const void* resource) {
    return ((const size_t*)resource)[1] == 0;
}

// Internal view type — for temporary read-only access, never stored in Gorget types.
typedef struct { const char* data; size_t len; } StrView;
static inline StrView str_view(Str s) {
    return (StrView){ (const char*)s.data, s.len };
}
static inline StrView str_view_raw(const char* data, size_t len) {
    return (StrView){ data, len };
}

// Global empty string — static view into an empty rodata literal.
// Drop is a no-op because cap == 0.
static const Str GORGET_EMPTY_STR = { .data = "", .cap = 0, .len = 0, .alloc = NULL };

// Stack-allocated string literal — compound literal yielding a Str struct (C99).
// Zero allocation; data points at the C string literal in .rodata. cap = 0 = view.
#define GORGET_SLIT(lit) \
    ((Str){ .data = (char*)(lit), .cap = 0, .len = sizeof(lit) - 1, .alloc = NULL })

// ── Allocation helpers ─────────────────────────────────────────
// Under 32-byte layout we allocate just the data bytes. The Str struct lives
// on the caller's stack (passed by value). Compared to the thin-pointer design
// this saves a 24-byte StringHeader allocation per owned string.

static inline Str str_alloc_with(size_t cap, GorgetAllocator* a) {
    char* data = (char*)a->alloc(a->ctx, cap);
    if (cap > 0) data[0] = '\0';
    return (Str){ .data = data, .cap = cap, .len = 0, .alloc = a };
}

static inline Str str_alloc_copy(const char* src, size_t len, GorgetAllocator* a) {
    size_t cap = len + 1;
    char* data = (char*)a->alloc(a->ctx, cap);
    if (len > 0 && src) memcpy(data, src, len);
    data[len] = '\0';
    return (Str){ .data = data, .cap = cap, .len = len, .alloc = a };
}

// Adopt a raw char* buffer into a proper Str. Frees the raw buffer.
static inline Str str_adopt_buf(char* buf, size_t len, size_t buf_cap, GorgetAllocator* a) {
    if (len == 0) {
        if (buf) a->dealloc(a->ctx, buf, buf_cap);
        return GORGET_EMPTY_STR;
    }
    Str result = str_alloc_copy(buf, len, a);
    a->dealloc(a->ctx, buf, buf_cap);
    return result;
}

// ── Core string operations ─────────────────────────────────────

static inline GorgetString gorget_string_new(const char* s) {
    __gorget_string_new_count++;
    if (s == NULL || s[0] == '\0') return GORGET_EMPTY_STR;
    return str_alloc_copy(s, strlen(s), __gorget_current_alloc);
}

// Adopt a freshly-allocated char* into a Str. Allocates a new buffer, copies
// data, frees original. Caller must have allocated with GORGET_ALLOC or similar.
static inline GorgetString gorget_string_adopt(char* s) {
    if (s == NULL) return GORGET_EMPTY_STR;
    size_t len = strlen(s);
    if (len == 0) { GORGET_FREE(s, 1); return GORGET_EMPTY_STR; }
    Str result = str_alloc_copy(s, len, __gorget_current_alloc);
    GORGET_FREE(s, len + 1);
    return result;
}

// Drop an owned string. Views (cap == 0) are no-ops — they borrow their buffer
// from .rodata, another Str, or an external source. Owned strings free `data`
// via the allocator that created them, then zero the struct so double-free is safe.
// Takes void* (not GorgetString*) so the cast at __gorget_drop_fn callback
// sites is well-typed under -fsanitize=undefined: the callee's parameter type
// must match the function-pointer type used at the call site, not just the
// underlying ABI. Direct callers pass &gs / &owned_string; both decay to void*.
static inline void gorget_string_free(void* p) {
    GorgetString* s = (GorgetString*)p;
    if (s->cap == 0) { *s = (Str){0}; return; }  // view — nothing to free
    // Owned (cap > 0) requires a live allocator. A NULL alloc here means
    // either (a) a CoW-demoted string left a stale alloc cleared without
    // resetting cap to 0, or (b) the value was never properly initialized.
    // Both are silent corruption without this check.
    if (!s->alloc) { fprintf(stderr, "gorget: panic: gorget_string_free invariant: cap=%zu but alloc=NULL\n", s->cap); exit(1); }
    __gorget_string_free_count++;
    s->alloc->dealloc(s->alloc->ctx, s->data, s->cap);
    *s = (Str){0};
}

// Closure env allocator. Returns a pointer to the env data; the 8 bytes
// preceding the returned pointer hold the env size, so `gorget_closure_free`
// and `gorget_closure_clone_to_owned` can recover the size without keeping
// it on the GorgetClosure struct (which stays at 16 bytes — fn_ptr + env).
//
// Layout: [size_t size | env_data...]
//                       ^-- returned pointer
static inline void* __gorget_closure_env_alloc(size_t env_size) {
    void* hdr = malloc(sizeof(size_t) + env_size);
    if (!hdr) return NULL;
    *(size_t*)hdr = env_size;
    return (char*)hdr + sizeof(size_t);
}

// Drop a closure value. The env pointer is heap-alloc'd by
// `wrap_closure_args_at_void_elem` (LIR) via `__gorget_closure_env_alloc`,
// so the GorgetClosure value owns it and must free on drop. NULL env
// (FuncRef wrap with adapter) is a no-op. Function pointers are weak
// references to .text and are never freed. Same void* signature pattern as
// gorget_string_free for typed __gorget_drop_fn callback dispatch under
// UBSan. Direct callers pass &closure; decays to void*.
static inline void gorget_closure_free(void* p) {
    GorgetClosure* c = (GorgetClosure*)p;
    if (c->env) {
        // Walk back to the size-prefix header and free the original allocation.
        free((char*)c->env - sizeof(size_t));
    }
    c->fn_ptr = NULL;
    c->env = NULL;
}

// Deep-clone a closure: copy fn_ptr as-is, allocate a fresh env via
// `__gorget_closure_env_alloc` (reading the size from the source's prefix
// header), and memcpy the env contents. Result is independently owned —
// the clone's drop frees its own env without affecting the source.
static inline GorgetClosure gorget_closure_clone_to_owned(const GorgetClosure* src) {
    __gorget_closure_clone_count++;
    GorgetClosure dst;
    dst.fn_ptr = src->fn_ptr;
    if (src->env) {
        size_t env_size = ((size_t*)src->env)[-1];
        dst.env = __gorget_closure_env_alloc(env_size);
        memcpy(dst.env, src->env, env_size);
    } else {
        dst.env = NULL;
    }
    return dst;
}

// In-place clone for collection elem_clone slots: read the closure at p,
// deep-clone it, write the deep copy back to p.
static inline void gorget_closure_clone_inplace(void* p) {
    GorgetClosure* c = (GorgetClosure*)p;
    *c = gorget_closure_clone_to_owned(c);
}

// Clone: produces an independently-owned copy. Under 32-byte this is
// view-aware — if src is already a view, we allocate a fresh owned buffer
// with the same contents. If src is owned, same thing (fresh buffer).
// Either way the result is cap > 0.
static inline GorgetString gorget_string_clone(const GorgetString* src) {
    if (src->len == 0) return GORGET_EMPTY_STR;
    return str_alloc_copy((const char*)src->data, src->len, __gorget_current_alloc);
}

// Same semantic as gorget_string_clone under 32-byte — kept as a distinct
// entry point because the GIR lowering emits this name for explicit .clone()
// calls and boundary materialization. Always produces an independently-owned
// (cap>0) buffer. Callers that hold a view (cap=0) into ephemeral storage
// (a substring of a heap buffer that may be dropped) depend on this contract
// to escape the aliasing — collapsing the cap=0 path to a shallow struct
// copy would dangle the result. The zero-cost path for cap=0 rodata-view
// globals lives elsewhere (GIR clone-on-access elides the call entirely
// when the global is a known literal-view; see `clone_resource_global_ref`).
static inline GorgetString gorget_string_clone_to_owned(const GorgetString* src) {
    __gorget_string_clone_count++;
    if (src->len == 0) return GORGET_EMPTY_STR;
    return str_alloc_copy((const char*)src->data, src->len, __gorget_current_alloc);
}

// Copy-on-write aware copy: views (cap=0) are struct-copied (zero alloc),
// owned strings (cap>0) are deep-cloned (allocates new buffer).
// Used by the C backend for `String t = s` assignment Copy path.
static inline GorgetString gorget_string_copy_cow(const GorgetString* src) {
    if (src->cap == 0) return *src;  // view: 32-byte struct copy, no alloc
    if (src->len == 0) return GORGET_EMPTY_STR;
    __gorget_string_cow_count++;
    return str_alloc_copy((const char*)src->data, src->len, __gorget_current_alloc);
}

// Borrow: shallow struct copy. The caller promises not to drop the borrow;
// the compiler emits no free for Ref locals. If a borrow escapes to storage
// the compiler materializes (clones) it at the ownership boundary.
static inline GorgetString gorget_string_borrow(const GorgetString* src) {
    return *src;
}

// Borrow as a TRUE view — shallow struct copy with cap FORCED to 0 so the
// result is drop-safe (gorget_string_free no-ops cap=0) even when stored in a
// drop-tracked VALUE slot that shares the source's heap buffer. Used by the
// lazy loop-carried element borrow: `s` holds this view until the
// flag-guarded in-place materialize deep-clones it (cap>0) on the first
// mutating use. Distinct from gorget_string_borrow (which copies cap as-is —
// fine for a Ref local the compiler never frees, but a double-free when the
// borrow lives in a freed value slot).
static inline GorgetString gorget_string_borrow_view(const GorgetString* src) {
    if (src->len == 0) return GORGET_EMPTY_STR;
    return (Str){ .data = src->data, .cap = 0, .len = src->len, .alloc = NULL };
}

// Materialize an in-slot Str value. If it's a view (cap == 0), clone it into
// an owned buffer so the enclosing container can safely outlive the original
// source. If already owned, no-op.
//
// Called by the elem_materialize / val_materialize / key_materialize hooks
// inside gorget_array_push / gorget_map_put after the caller memcpys the
// value into the slot. This is the runtime side of lazy CoW: views crossing
// into owning storage get upgraded on-the-fly.
static inline void gorget_string_materialize_inplace(void* p) {
    Str* s = (Str*)p;
    if (s->cap == 0 && s->len > 0) {
        *s = str_alloc_copy((const char*)s->data, s->len, __gorget_current_alloc);
    }
}

static inline GorgetString gorget_string_concat(const GorgetString* a, const GorgetString* b) {
    size_t la = a->len;
    size_t lb = b->len;
    if (la == 0 && lb == 0) return GORGET_EMPTY_STR;
    if (la == 0) return gorget_string_clone(b);
    if (lb == 0) return gorget_string_clone(a);
    GorgetAllocator* al = __gorget_current_alloc;
    size_t total = la + lb;
    size_t cap = total + 1;
    char* data = (char*)al->alloc(al->ctx, cap);
    memcpy(data, a->data, la);
    memcpy((char*)data + la, b->data, lb);
    data[total] = '\0';
    return (Str){ .data = data, .cap = cap, .len = total, .alloc = al };
}

static inline bool gorget_string_eq(const GorgetString* a, const GorgetString* b) {
    if (a->len != b->len) return false;
    if (a->len == 0) return true;
    if (a->data == b->data) return true;  // same buffer (aliased views)
    return memcmp(a->data, b->data, a->len) == 0;
}

// Return a NUL-terminated C string for FFI. Owned Strs have a '\0' byte at
// data[len] thanks to the +1 in cap (str_alloc_copy / str_alloc_with).
// Views into C string literals are naturally NUL-terminated too. Views into
// the middle of another buffer (from slice/char_at/...) are NOT NUL-terminated
// at len — but this plan keeps those functions allocating, so gorget_string_cstr
// only sees allocation-backed strings in practice.
static inline const char* gorget_string_cstr(const GorgetString* s) {
    return s->data ? (const char*)s->data : "";
}

static inline GorgetString gorget_string_format(const char* fmt, ...) {
    GorgetAllocator* a = __gorget_current_alloc;
    // Single-pass fast path: format into a stack scratch buffer, then copy
    // exactly `len+1` bytes into one right-sized heap allocation. vsnprintf
    // returns the length it WOULD have written, so we still learn the true
    // length even if it didn't fit — only then do we re-format. This avoids
    // the probe-pass vsnprintf(NULL, 0, ...) for the common short-string case
    // (f-string keys/values), halving the formatting work.
    char scratch[256];
    va_list args1, args2;
    va_start(args1, fmt);
    va_copy(args2, args1);
    int len = vsnprintf(scratch, sizeof(scratch), fmt, args1);
    va_end(args1);
    if (len < 0) { va_end(args2); fprintf(stderr, "gorget: panic: format error\n"); exit(1); }
    size_t cap = (size_t)len + 1;
    char* data = (char*)a->alloc(a->ctx, cap);
    if (cap <= sizeof(scratch)) {
        // Fully formatted in scratch — just copy out (includes '\0').
        memcpy(data, scratch, cap);
    } else {
        // Output was truncated; re-format directly into the right-sized buffer.
        vsnprintf(data, cap, fmt, args2);
    }
    va_end(args2);
    return (Str){ .data = data, .cap = cap, .len = (size_t)len, .alloc = a };
}

static inline const char* gorget_format(const char* fmt, ...) {
    // Single-pass fast path (see gorget_string_format): format into a stack
    // scratch buffer, then copy out into one right-sized allocation, falling
    // back to a re-format only when the result didn't fit in scratch.
    char scratch[256];
    va_list args1, args2;
    va_start(args1, fmt);
    va_copy(args2, args1);
    int len = vsnprintf(scratch, sizeof(scratch), fmt, args1);
    va_end(args1);
    if (len < 0) { va_end(args2); fprintf(stderr, "gorget: panic: format error\n"); exit(1); }
    size_t cap = (size_t)len + 1;
    char* data = (char*)GORGET_ALLOC(cap);
    if (cap <= sizeof(scratch)) {
        memcpy(data, scratch, cap);
    } else {
        vsnprintf(data, cap, fmt, args2);
    }
    va_end(args2);
    return data;
}

static inline GorgetString gorget_string_from_concat(const char* a, const char* b) {
    size_t la = strlen(a), lb = strlen(b);
    if (la + lb == 0) return GORGET_EMPTY_STR;
    GorgetAllocator* al = __gorget_current_alloc;
    size_t cap = la + lb + 1;
    char* data = (char*)al->alloc(al->ctx, cap);
    memcpy(data, a, la);
    memcpy(data + la, b, lb + 1);  // includes trailing '\0'
    return (Str){ .data = data, .cap = cap, .len = la + lb, .alloc = al };
}

// Append the NUL-terminated C string `rhs` to `*s` in place.
// If `*s` is a view or uninitialized, allocates a fresh owned buffer first
// (CoW-on-mutation semantics for string builders).
static inline void gorget_string_append(GorgetString* s, const char* rhs) {
    size_t rlen = strlen(rhs);
    if (rlen == 0) return;
    if (s->cap == 0) {
        // View or empty — materialize into a freshly-owned buffer.
        GorgetAllocator* a = __gorget_current_alloc;
        size_t new_len = s->len + rlen;
        size_t new_cap = (new_len + 1) * 2;
        char* data = (char*)a->alloc(a->ctx, new_cap);
        if (s->len > 0 && s->data) memcpy(data, s->data, s->len);
        memcpy(data + s->len, rhs, rlen + 1);  // includes trailing '\0'
        *s = (Str){ .data = data, .cap = new_cap, .len = new_len, .alloc = a };
        return;
    }
    size_t new_len = s->len + rlen;
    if (new_len + 1 > s->cap) {
        size_t new_cap = (new_len + 1) * 2;
        s->data = s->alloc->realloc(s->alloc->ctx, s->data, s->cap, new_cap);
        s->cap = new_cap;
    }
    memcpy((char*)s->data + s->len, rhs, rlen + 1);  // includes trailing '\0'
    s->len = new_len;
}

static inline void gorget_string_push_byte(GorgetString* s, char c) {
    if (s->cap == 0) {
        // View or empty — materialize into a fresh owned buffer with room to grow.
        GorgetAllocator* a = __gorget_current_alloc;
        size_t new_cap = (s->len + 2) * 2;
        if (new_cap < 16) new_cap = 16;
        char* data = (char*)a->alloc(a->ctx, new_cap);
        if (s->len > 0 && s->data) memcpy(data, s->data, s->len);
        data[s->len] = c;
        data[s->len + 1] = '\0';
        *s = (Str){ .data = data, .cap = new_cap, .len = s->len + 1, .alloc = a };
        return;
    }
    if (s->len + 2 > s->cap) {
        size_t new_cap = (s->len + 2) * 2;
        s->data = s->alloc->realloc(s->alloc->ctx, s->data, s->cap, new_cap);
        s->cap = new_cap;
    }
    ((char*)s->data)[s->len] = c;
    s->len++;
    ((char*)s->data)[s->len] = '\0';
}

static inline GorgetString gorget_string_with_capacity(int64_t cap) {
    size_t c = (size_t)(cap > 0 ? cap : 16);
    return str_alloc_with(c, __gorget_current_alloc);
}

static inline void gorget_string_push_int(GorgetString* s, int64_t n) {
    char buf[32];
    snprintf(buf, sizeof(buf), "%" PRId64, n);
    gorget_string_append(s, buf);
}

static inline void gorget_string_push_float(GorgetString* s, double d) {
    char buf[64];
    snprintf(buf, sizeof(buf), "%g", d);
    gorget_string_append(s, buf);
}

static inline void gorget_string_push_bool(GorgetString* s, bool b) {
    gorget_string_append(s, b ? "true" : "false");
}

static inline void gorget_string_push_line(GorgetString* s, const char* rhs) {
    gorget_string_append(s, rhs);
    gorget_string_push_byte(s, '\n');
}

static inline void gorget_string_push_line_int(GorgetString* s, int64_t n) {
    gorget_string_push_int(s, n);
    gorget_string_push_byte(s, '\n');
}

static inline void gorget_string_push_line_float(GorgetString* s, double d) {
    gorget_string_push_float(s, d);
    gorget_string_push_byte(s, '\n');
}

static inline void gorget_string_push_line_bool(GorgetString* s, bool b) {
    gorget_string_push_bool(s, b);
    gorget_string_push_byte(s, '\n');
}

static inline void gorget_string_push_line_char(GorgetString* s, char c) {
    gorget_string_push_byte(s, c);
    gorget_string_push_byte(s, '\n');
}

// ── String concatenation ────────────────────────────────────
static inline const char* gorget_str_concat(const char* a, const char* b) {
    size_t la = strlen(a), lb = strlen(b);
    char* out = (char*)GORGET_ALLOC(la + lb + 1);
    memcpy(out, a, la);
    memcpy(out + la, b, lb + 1);
    return out;
}

// ── Str view functions ──────────────────────────────────────
// Forward declarations — defined in the panic handler section.
static void gorget_panic(const char* msg);
static void gorget_panic_at(const char* file, int line, int col, const char* msg);
// Trap normalization (D11): forward-declared here because runtime files
// concatenate with runtime_string.c BEFORE panic_normal.c, and gorget_tostr.c
// (gorget_assert_fail_values) calls gorget_trap before its definition.
static void gorget_trap(const char* code, const char* detail);
static void gorget_trap_at(const char* code, const char* detail, const char* file, int line, int col);

// Build a Str from a literal C string. Allocation helper used by the C
// backend when a string value must be materialized at runtime from a raw
// const char* (e.g., gorget_string_adopt fallbacks). String literals that
// the LIR StrLit path emits go through static `__slit_N` structs instead,
// which is free (no allocation, view into .rodata).
static inline Str gorget_str_from_literal(const char* data, size_t len) {
    if (len == 0) return GORGET_EMPTY_STR;
    return str_alloc_copy(data, len, __gorget_current_alloc);
}

// Push a Str (1+ bytes) onto a String builder — user-facing push_char.
// `c` is passed by value (32-byte struct copy).
static inline void gorget_string_push_char(GorgetString* s, Str c) {
    size_t clen = c.len;
    if (clen == 0) return;
    if (s->cap == 0) {
        GorgetAllocator* a = __gorget_current_alloc;
        size_t new_len = s->len + clen;
        size_t new_cap = (new_len + 1) * 2;
        if (new_cap < 16) new_cap = 16;
        char* data = (char*)a->alloc(a->ctx, new_cap);
        if (s->len > 0 && s->data) memcpy(data, s->data, s->len);
        memcpy(data + s->len, c.data, clen);
        data[new_len] = '\0';
        *s = (Str){ .data = data, .cap = new_cap, .len = new_len, .alloc = a };
        return;
    }
    if (s->len + clen + 1 > s->cap) {
        size_t new_cap = (s->len + clen + 1) * 2;
        s->data = s->alloc->realloc(s->alloc->ctx, s->data, s->cap, new_cap);
        s->cap = new_cap;
    }
    memcpy((char*)s->data + s->len, c.data, clen);
    s->len += clen;
    ((char*)s->data)[s->len] = '\0';
}

static inline Str gorget_str_from_cstr(const char* s) {
    if (s == NULL || s[0] == '\0') return GORGET_EMPTY_STR;
    return str_alloc_copy(s, strlen(s), __gorget_current_alloc);
}

static inline Str gorget_str_empty(void) {
    return GORGET_EMPTY_STR;
}

static inline Str gorget_int_to_binary(long long val, long long alt) {
    // Max 64 bits + "0b" prefix + null
    char buf[67];
    int pos = 66;
    buf[pos] = '\0';
    unsigned long long uval = (unsigned long long)val;
    if (uval == 0) {
        buf[--pos] = '0';
    } else {
        while (uval > 0) {
            buf[--pos] = '0' + (char)(uval & 1);
            uval >>= 1;
        }
    }
    if (alt) {
        buf[--pos] = 'b';
        buf[--pos] = '0';
    }
    size_t len = 66 - (size_t)pos;
    return str_alloc_copy(buf + pos, len, __gorget_current_alloc);
}

// ── Str field access + comparison ───────────────────────────
static inline size_t gorget_str_byte_len(Str s) {
    return s.len;
}
static inline bool gorget_str_is_empty(Str s) {
    return s.len == 0;
}

static inline bool gorget_str_eq(Str a, Str b) {
    if (a.len != b.len) return false;
    if (a.len == 0) return true;
    if (a.data == b.data) return true;
    return memcmp(a.data, b.data, a.len) == 0;
}

static inline int gorget_str_cmp(Str a, Str b) {
    size_t la = a.len;
    size_t lb = b.len;
    size_t min_len = la < lb ? la : lb;
    int r = min_len > 0 ? memcmp(a.data, b.data, min_len) : 0;
    if (r != 0) return r;
    if (la < lb) return -1;
    if (la > lb) return 1;
    return 0;
}

// ── UTF-8 utilities ─────────────────────────────────────────
// Byte count for a UTF-8 codepoint from its first byte (1-4, or 1 for invalid)
static inline int gorget_utf8_codepoint_len(unsigned char b) {
    if (b < 0x80) return 1;
    if ((b & 0xE0) == 0xC0) return 2;
    if ((b & 0xF0) == 0xE0) return 3;
    if ((b & 0xF8) == 0xF0) return 4;
    return 1;  // invalid lead byte — treat as single byte to advance
}

// Decode one codepoint starting at data[*pos]. Advances *pos past it.
// Returns the Unicode codepoint value, or 0xFFFD (replacement char) on error.
static inline int64_t gorget_utf8_decode(const char* data, size_t len, size_t* pos) {
    if (*pos >= len) return 0xFFFD;
    unsigned char b0 = (unsigned char)data[*pos];

    if (b0 < 0x80) {
        (*pos)++;
        return (int64_t)b0;
    }

    int expected;
    int64_t cp;
    if ((b0 & 0xE0) == 0xC0) {
        expected = 2; cp = b0 & 0x1F;
    } else if ((b0 & 0xF0) == 0xE0) {
        expected = 3; cp = b0 & 0x0F;
    } else if ((b0 & 0xF8) == 0xF0) {
        expected = 4; cp = b0 & 0x07;
    } else {
        // Invalid lead byte
        (*pos)++;
        return 0xFFFD;
    }

    if (*pos + (size_t)expected > len) {
        // Truncated sequence
        (*pos)++;
        return 0xFFFD;
    }

    for (int i = 1; i < expected; i++) {
        unsigned char cb = (unsigned char)data[*pos + (size_t)i];
        if ((cb & 0xC0) != 0x80) {
            // Invalid continuation byte
            (*pos)++;
            return 0xFFFD;
        }
        cp = (cp << 6) | (cb & 0x3F);
    }

    *pos += (size_t)expected;

    // Reject overlong encodings
    if (expected == 2 && cp < 0x80) return 0xFFFD;
    if (expected == 3 && cp < 0x800) return 0xFFFD;
    if (expected == 4 && cp < 0x10000) return 0xFFFD;

    // Reject surrogates and values > U+10FFFF
    if (cp >= 0xD800 && cp <= 0xDFFF) return 0xFFFD;
    if (cp > 0x10FFFF) return 0xFFFD;

    return cp;
}

// Validate entire byte sequence as valid UTF-8.
static inline bool gorget_utf8_validate(const char* data, size_t len) {
    size_t pos = 0;
    while (pos < len) {
        size_t saved = pos;
        int64_t cp = gorget_utf8_decode(data, len, &pos);
        if (cp == 0xFFFD && (pos - saved != 1 || (unsigned char)data[saved] != 0xFD)) {
            if (saved + 3 <= len &&
                (unsigned char)data[saved] == 0xEF &&
                (unsigned char)data[saved + 1] == 0xBF &&
                (unsigned char)data[saved + 2] == 0xBD) {
                continue;  // Legitimate U+FFFD in input
            }
            return false;
        }
    }
    return true;
}

// Validate String contents as UTF-8 (convenience wrapper for codegen).
static inline bool gorget_string_is_valid_utf8(const GorgetString* s) {
    if (s->len == 0) return true;
    return gorget_utf8_validate((const char*)s->data, s->len);
}

// ── Codepoint-level Str operations ──────────────────────────
// Count codepoints. A UTF-8 codepoint is encoded as one lead byte
// (top two bits != 0b10) followed by 0–3 continuation bytes
// (top two bits == 0b10). So the codepoint count is simply the number
// of NON-continuation bytes: count = len - (#continuation bytes).
//
// We count continuation bytes word-at-a-time (SWAR): a byte is a
// continuation byte iff (b & 0xC0) == 0x80, i.e. bit7 set AND bit6 clear.
// For a 64-bit word w: ((w >> 7) & ~(w >> 6) & 0x0101...01) marks each
// continuation byte's low bit; popcount gives the per-word tally. This is
// ~8× fewer iterations than the per-byte walk and removes the per-byte
// function call, while producing the identical count for any valid or
// invalid UTF-8 byte sequence (each leading/invalid byte is counted once).
static inline int64_t gorget_str_codepoint_count(Str s) {
    if (s.len == 0) return 0;
    const unsigned char* d = (const unsigned char*)s.data;
    size_t len = s.len;
    size_t cont = 0; // number of continuation bytes (0b10xxxxxx)

    size_t i = 0;
    // Align to 8 bytes so the word loads below are well-defined.
    while (i < len && ((uintptr_t)(d + i) & 7u) != 0) {
        if ((d[i] & 0xC0) == 0x80) cont++;
        i++;
    }
    // Word-at-a-time core.
    while (i + 8 <= len) {
        uint64_t w;
        memcpy(&w, d + i, 8);
        // Mark bytes with bit7 set and bit6 clear (continuation bytes).
        uint64_t marks = (w >> 7) & ~(w >> 6) & 0x0101010101010101ULL;
        cont += (size_t)__builtin_popcountll(marks);
        i += 8;
    }
    // Tail.
    while (i < len) {
        if ((d[i] & 0xC0) == 0x80) cont++;
        i++;
    }
    return (int64_t)(len - cont);
}

// Forward declarations
static inline Str gorget_str_own_region(const char* data, size_t len);
static inline Str gorget_str_view_region(const char* data, size_t len);

// Return owned copy of ith codepoint (0-based). Supports negative indexing.
static inline Str gorget_str_index(Str s, int64_t idx) {
    int64_t cp_count = gorget_str_codepoint_count(s);
    if (idx < 0) idx += cp_count;
    if (idx < 0 || idx >= cp_count) {
        gorget_panic("str index out of bounds");
    }
    const char* d = (const char*)s.data;
    size_t byte_off = 0;
    int64_t i = 0;
    while (i < idx) {
        byte_off += (size_t)gorget_utf8_codepoint_len((unsigned char)d[byte_off]);
        i++;
    }
    int cplen = gorget_utf8_codepoint_len((unsigned char)d[byte_off]);
    return gorget_str_view_region(d + byte_off, (size_t)cplen);
}

// Return owned copy of codepoint range [start, end). Supports negative indices.
static inline Str gorget_str_slice(Str s, int64_t start, int64_t end) {
    int64_t cp_count = gorget_str_codepoint_count(s);
    if (start < 0) start += cp_count;
    if (end < 0) end += cp_count;
    if (start < 0) start = 0;
    if (end > cp_count) end = cp_count;
    if (start >= end) return GORGET_EMPTY_STR;
    if (start > cp_count || end < 0) {
        gorget_panic("str slice out of bounds");
    }
    const char* d = (const char*)s.data;
    // Walk to start byte offset
    size_t start_byte = 0;
    for (int64_t i = 0; i < start; i++) {
        start_byte += (size_t)gorget_utf8_codepoint_len((unsigned char)d[start_byte]);
    }
    // Walk from start to end byte offset
    size_t end_byte = start_byte;
    for (int64_t i = start; i < end; i++) {
        end_byte += (size_t)gorget_utf8_codepoint_len((unsigned char)d[end_byte]);
    }
    return gorget_str_view_region(d + start_byte, end_byte - start_byte);
}

// Clone a region of memory into an owned Str.
static inline Str gorget_str_own_region(const char* data, size_t len) {
    if (len == 0) return GORGET_EMPTY_STR;
    return str_alloc_copy(data, len, __gorget_current_alloc);
}

// Return a zero-allocation view into an existing buffer. cap=0 marks it as a view;
// gorget_string_free is a no-op on views. The data pointer borrows from the source.
// NOT NUL-terminated at data[len] (points into the middle of another string).
static inline Str gorget_str_view_region(const char* data, size_t len) {
    if (len == 0) return GORGET_EMPTY_STR;
    return (Str){ .data = (char*)data, .cap = 0, .len = len, .alloc = NULL };
}

// Borrow region — returns StrView (internal use only, never stored).
static inline StrView gorget_str_borrow_region_view(const char* data, size_t len) {
    return str_view_raw(data, len);
}

// Return a byte-level owned copy of s[start..end].
static inline Str gorget_str_byte_slice(Str s, int64_t start, int64_t end) {
    if (start < 0 || end < 0 || (size_t)start > s.len || (size_t)end > s.len || start > end) {
        fprintf(stderr, "gorget: panic: string byte_slice out of bounds: [%" PRId64 "..%" PRId64 "], byte length %zu\n", start, end, s.len);
        exit(1);
    }
    return gorget_str_view_region((const char*)s.data + start, (size_t)(end - start));
}

// Return a view of the character at the given byte index.
static inline Str gorget_str_char_at(Str s, int64_t index) {
    if (index < 0 || (size_t)index >= s.len) {
        return GORGET_EMPTY_STR;
    }
    return gorget_str_view_region((const char*)s.data + index, 1);
}

// Return the byte at index (byte-level). Bounds-checked against byte length.
static inline uint8_t gorget_str_byte_at(Str s, int64_t index) {
    if (index < 0 || (size_t)index >= s.len) {
        fprintf(stderr, "gorget: panic: string byte index out of bounds: index %" PRId64 ", byte length %zu\n", index, s.len);
        exit(1);
    }
    return (uint8_t)((const char*)s.data)[index];
}

// ── UTF-8 encode helper ─────────────────────────────────────
// Encode a Unicode codepoint to UTF-8. Writes 1-4 bytes to out[]. Returns bytes written.
static inline int gorget_utf8_encode(int64_t cp, char* out) {
    if (cp < 0x80) {
        out[0] = (char)cp;
        return 1;
    }
    if (cp < 0x800) {
        out[0] = (char)(0xC0 | (cp >> 6));
        out[1] = (char)(0x80 | (cp & 0x3F));
        return 2;
    }
    if (cp < 0x10000) {
        out[0] = (char)(0xE0 | (cp >> 12));
        out[1] = (char)(0x80 | ((cp >> 6) & 0x3F));
        out[2] = (char)(0x80 | (cp & 0x3F));
        return 3;
    }
    if (cp <= 0x10FFFF) {
        out[0] = (char)(0xF0 | (cp >> 18));
        out[1] = (char)(0x80 | ((cp >> 12) & 0x3F));
        out[2] = (char)(0x80 | ((cp >> 6) & 0x3F));
        out[3] = (char)(0x80 | (cp & 0x3F));
        return 4;
    }
    // Invalid codepoint — emit U+FFFD
    out[0] = (char)0xEF; out[1] = (char)0xBF; out[2] = (char)0xBD;
    return 3;
}

// ── Str-native search primitive ─────────────────────────────
// memmem implementation (always available, no _GNU_SOURCE dependency).
static inline const char* gorget_memmem(const char* h, size_t hlen, const char* n, size_t nlen) {
    if (nlen == 0) return h;
    if (nlen > hlen) return NULL;
    const char first = n[0];
    const char* end = h + hlen - nlen + 1;
    for (const char* p = h; p < end; p++) {
        p = (const char*)memchr(p, first, (size_t)(end - p));
        if (!p) return NULL;
        if (memcmp(p, n, nlen) == 0) return p;
    }
    return NULL;
}

// In-place append with explicit (cstr, len) for the rhs side. extern
// "C" coerces `String rhs` to `cstr`, dropping the .len field; the
// self-host emits cstr+len as separate args and we read both directly
// — no strlen() walk needed. Used by lir_codegen.gg::sb_push to break
// the O(N²) `out = out + ...` pattern in code generators.
//
// Returns int (always 0) instead of void because the self-host emits
// `__vN = call(...)` for extern calls and chokes on void-return.
static inline int64_t gorget_string_append_buf(GorgetString* s, const char* data, int64_t len) {
    if (len <= 0 || !data) return 0;
    size_t n = (size_t)len;
    if (s->cap == 0) {
        GorgetAllocator* a = __gorget_current_alloc;
        size_t new_len = s->len + n;
        size_t new_cap = (new_len + 1) * 2;
        if (new_cap < 64) new_cap = 64;
        char* d = (char*)a->alloc(a->ctx, new_cap);
        if (s->len > 0 && s->data) memcpy(d, s->data, s->len);
        memcpy(d + s->len, data, n);
        d[new_len] = '\0';
        *s = (Str){ .data = d, .cap = new_cap, .len = new_len, .alloc = a };
        return 0;
    }
    size_t new_len = s->len + n;
    if (new_len + 1 > s->cap) {
        size_t new_cap = (new_len + 1) * 2;
        s->data = (char*)s->alloc->realloc(s->alloc->ctx, s->data, s->cap, new_cap);
        s->cap = new_cap;
    }
    memcpy((char*)s->data + s->len, data, n);
    s->len = new_len;
    ((char*)s->data)[s->len] = '\0';
    return 0;
}

// ── In-place append with both args by pointer ───────────────
// Companion to gorget_string_append_str — same semantics but takes
// the rhs string by pointer too. This sidesteps the extern "C" ABI
// coercion that turns `String rhs` into `const char*` (cstr) at call
// sites: by declaring the second arg as a pointer in Gorget too,
// the compiler keeps it as Str* and we read both data + len directly.
static inline void gorget_string_append_str_ptr(GorgetString* s, const Str* rhs) {
    if (!rhs || rhs->len == 0 || !rhs->data) return;
    size_t n = rhs->len;
    if (s->cap == 0) {
        GorgetAllocator* a = __gorget_current_alloc;
        size_t new_len = s->len + n;
        size_t new_cap = (new_len + 1) * 2;
        if (new_cap < 64) new_cap = 64;
        char* d = (char*)a->alloc(a->ctx, new_cap);
        if (s->len > 0 && s->data) memcpy(d, s->data, s->len);
        memcpy(d + s->len, rhs->data, n);
        d[new_len] = '\0';
        *s = (Str){ .data = d, .cap = new_cap, .len = new_len, .alloc = a };
        return;
    }
    size_t new_len = s->len + n;
    if (new_len + 1 > s->cap) {
        size_t new_cap = (new_len + 1) * 2;
        s->data = (char*)s->alloc->realloc(s->alloc->ctx, s->data, s->cap, new_cap);
        s->cap = new_cap;
    }
    memcpy((char*)s->data + s->len, rhs->data, n);
    s->len = new_len;
    ((char*)s->data)[s->len] = '\0';
}

// ── Debug stderr writer ─────────────────────────────────────
// Line-buffered unlike the default stdout path (which is block-buffered
// when piped to a file). Used by self-host sources for real-time progress
// markers without pulling in std.io's File handles (which drag in the
// entire std.io → IoError → derive trait graph that the self-host can't
// yet lower cleanly).
static inline int gorget_eprint_debug(Str msg) {
    if (msg.len > 0 && msg.data) fwrite(msg.data, 1, msg.len, stderr);
    fputc('\n', stderr);
    fflush(stderr);
    return 0;
}



