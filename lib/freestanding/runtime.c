/*
 * Gorget freestanding runtime — minimal allocator and compiler support.
 * No libc dependency. Provides the absolute minimum for Gorget programs
 * to run on bare metal (UEFI, custom kernels, embedded).
 */

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* MSVC/PE toolchain: the compiler references _fltused when floating-point
 * code is present. In freestanding there's no CRT to provide it. */
int _fltused = 0;

/* ── libc stubs for freestanding ────────────────────────────────── */

/* stderr/stdout — not real file pointers, just markers for fwrite stub */
static void* stderr = (void*)1;

/* exit() halts the CPU in freestanding */
_Noreturn static void exit(int code) {
    (void)code;
#if defined(__x86_64__) || defined(_M_X64)
    for (;;) __asm__ volatile("hlt");
#elif defined(__aarch64__)
    for (;;) __asm__ volatile("wfi");
#else
    for (;;) {}
#endif
}

/* Forward declaration — defined later after __gorget_putchar is declared. */
static int fprintf(void* stream, const char* fmt, ...);

/* ── Compiler builtins (no libc) ────────────────────────────────── */

void* memset(void* s, int c, size_t n) {
    uint8_t* p = (uint8_t*)s;
    for (size_t i = 0; i < n; i++) p[i] = (uint8_t)c;
    return s;
}

void* memcpy(void* dest, const void* src, size_t n) {
    uint8_t* d = (uint8_t*)dest;
    const uint8_t* s = (const uint8_t*)src;
    for (size_t i = 0; i < n; i++) d[i] = s[i];
    return dest;
}

int memcmp(const void* a, const void* b, size_t n) {
    const uint8_t* pa = (const uint8_t*)a;
    const uint8_t* pb = (const uint8_t*)b;
    for (size_t i = 0; i < n; i++) {
        if (pa[i] != pb[i]) return pa[i] < pb[i] ? -1 : 1;
    }
    return 0;
}

size_t strlen(const char* s) {
    size_t n = 0;
    while (s[n]) n++;
    return n;
}

/* ── Halt (used by panic) ───────────────────────────────────────── */

static void __gorget_halt(void) {
#if defined(__x86_64__) || defined(_M_X64)
    for (;;) __asm__ volatile("hlt");
#elif defined(__aarch64__)
    for (;;) __asm__ volatile("wfi");
#else
    for (;;) {}
#endif
}

/* ── Console output (set by UEFI stub) ──────────────────────────── */

/* Function pointer for text output — the UEFI stub sets this to ConOut. */
typedef void (*__gorget_putchar_fn)(int ch);
static __gorget_putchar_fn __gorget_putchar = NULL;

void __gorget_set_putchar(__gorget_putchar_fn fn) {
    __gorget_putchar = fn;
}

static void __gorget_puts(const char* s) {
    if (!__gorget_putchar) return;
    while (*s) {
        if (*s == '\n') __gorget_putchar('\r');
        __gorget_putchar(*s++);
    }
}

/* ── fprintf implementation (deferred until __gorget_puts is available) ── */

static int fprintf(void* stream, const char* fmt, ...) {
    (void)stream;
    /* Print the format string literally — covers constant overflow messages. */
    __gorget_puts(fmt ? fmt : "");
    return 0;
}

/* ── Panic ──────────────────────────────────────────────────────── */

void gorget_panic(const char* msg) {
    __gorget_puts("gorget: panic: ");
    __gorget_puts(msg);
    __gorget_puts("\n");
    __gorget_halt();
}

/* ── Bump allocator ─────────────────────────────────────────────── */

#ifndef GORGET_HEAP_SIZE
#define GORGET_HEAP_SIZE (64 * 1024 * 1024)  /* 64 MB */
#endif

static _Alignas(16) char __gorget_heap[GORGET_HEAP_SIZE];
static size_t __gorget_heap_offset = 0;

static void* __gorget_bump_alloc(void* ctx, size_t size) {
    (void)ctx;
    size = (size + 15) & ~(size_t)15;  /* 16-byte alignment */
    if (__gorget_heap_offset + size > GORGET_HEAP_SIZE) {
        gorget_panic("out of memory");
    }
    void* p = __gorget_heap + __gorget_heap_offset;
    __gorget_heap_offset += size;
    return p;
}

static void __gorget_bump_free(void* ctx, void* ptr, size_t size) {
    (void)ctx; (void)ptr; (void)size;
    /* Bump allocator: no-op free. Arena reclaimed on reboot. */
}

static void* __gorget_bump_realloc(void* ctx, void* ptr, size_t old_size, size_t new_size) {
    void* p = __gorget_bump_alloc(ctx, new_size);
    if (ptr && old_size > 0) {
        size_t copy = old_size < new_size ? old_size : new_size;
        memcpy(p, ptr, copy);
    }
    return p;
}

/* ── Gorget allocator interface ─────────────────────────────────── */

typedef struct {
    void* (*alloc)(void* ctx, size_t size);
    void* (*realloc)(void* ctx, void* ptr, size_t old_size, size_t new_size);
    void  (*dealloc)(void* ctx, void* ptr, size_t size);
    void* ctx;
} GorgetAllocator;

static GorgetAllocator __gorget_bump_allocator = {
    __gorget_bump_alloc,
    __gorget_bump_realloc,
    __gorget_bump_free,
    NULL,
};

static GorgetAllocator* __gorget_current_alloc = &__gorget_bump_allocator;

/* Macros expected by Gorget-emitted C code */
#define GORGET_ALLOC(size) \
    __gorget_current_alloc->alloc(__gorget_current_alloc->ctx, (size))
#define GORGET_REALLOC(ptr, old_size, new_size) \
    __gorget_current_alloc->realloc(__gorget_current_alloc->ctx, (ptr), (old_size), (new_size))
#define GORGET_FREE(ptr, size) \
    __gorget_current_alloc->dealloc(__gorget_current_alloc->ctx, (ptr), (size))
#define GORGET_CALLOC(count, elem_size) \
    ({ void* __p = GORGET_ALLOC((count) * (elem_size)); memset(__p, 0, (count) * (elem_size)); __p; })

/* Allocation counters (no-op in freestanding — kept for API compat) */
static size_t __gorget_total_allocated = 0;
static size_t __gorget_total_freed = 0;
static size_t __gorget_alloc_count = 0;
static size_t __gorget_free_count = 0;

/* ── String type (same layout as hosted runtime) ────────────────── */

typedef struct {
    char*            data;
    size_t           cap;
    size_t           len;
    GorgetAllocator* alloc;
} Str;

typedef Str GorgetString;

#define STR_LIT(lit) \
    ((Str){ .data = (char*)(lit), .cap = 0, .len = sizeof(lit) - 1, .alloc = NULL })

static inline GorgetString gorget_string_new(const char* s) {
    GorgetAllocator* a = __gorget_current_alloc;
    if (s == NULL) return (GorgetString){NULL, 0, 0, a};
    size_t len = strlen(s);
    size_t cap = len + 1;
    char* data = (char*)a->alloc(a->ctx, cap);
    memcpy(data, s, len + 1);
    return (GorgetString){data, cap, len, a};
}

static inline void gorget_string_free(GorgetString* s) {
    if (s->cap > 0 && s->data) {
        GORGET_FREE(s->data, s->cap);
    }
    s->data = NULL; s->cap = 0; s->len = 0;
}

static inline GorgetString gorget_string_clone_to_owned(const GorgetString* src) {
    if (!src->data || src->len == 0) return gorget_string_new("");
    GorgetAllocator* a = __gorget_current_alloc;
    size_t cap = src->len + 1;
    char* data = (char*)a->alloc(a->ctx, cap);
    memcpy(data, src->data, src->len);
    data[src->len] = '\0';
    return (GorgetString){data, cap, src->len, a};
}

/* ── Array type (same layout as hosted runtime) ─────────────────── */

typedef void (*__gorget_drop_fn)(void*);

typedef struct {
    void*            data;
    size_t           cap;
    size_t           len;
    size_t           elem_size;
    GorgetAllocator* alloc;
    __gorget_drop_fn elem_drop;
    __gorget_drop_fn elem_clone;
    __gorget_drop_fn elem_materialize;
} GorgetArray;

static inline GorgetArray gorget_array_new(size_t elem_size) {
    return (GorgetArray){NULL, 0, 0, elem_size, __gorget_current_alloc, NULL, NULL, NULL};
}

static inline void gorget_array_push(GorgetArray* arr, const void* elem) {
    if (arr->len >= arr->cap) {
        size_t old_cap = arr->cap;
        size_t new_cap = old_cap == 0 ? 8 : old_cap * 2;
        arr->data = arr->alloc->realloc(
            arr->alloc->ctx, arr->data,
            old_cap * arr->elem_size, new_cap * arr->elem_size);
        arr->cap = new_cap;
    }
    memcpy((char*)arr->data + arr->len * arr->elem_size, elem, arr->elem_size);
    arr->len++;
    if (arr->elem_materialize) {
        arr->elem_materialize((char*)arr->data + (arr->len - 1) * arr->elem_size);
    }
}

static inline void* gorget_array_get(const GorgetArray* arr, size_t index) {
    if (index >= arr->len) {
        gorget_panic("index out of bounds");
    }
    return (char*)arr->data + index * arr->elem_size;
}

static inline size_t gorget_array_len(const GorgetArray* arr) {
    return arr->len;
}

static inline void gorget_array_free(GorgetArray* arr) {
    if (arr->elem_drop && arr->data) {
        for (size_t i = 0; i < arr->len; i++) {
            arr->elem_drop((char*)arr->data + i * arr->elem_size);
        }
    }
    if (arr->cap > 0 && arr->data) {
        GORGET_FREE(arr->data, arr->cap * arr->elem_size);
    }
    arr->data = NULL; arr->cap = 0; arr->len = 0;
}

/* ── Minimal printf for freestanding ────────────────────────────── */

/* Only supports %d / %lld / %s / %f (enough for print() lowering). */
static void __gorget_print_int(int64_t n) {
    if (!__gorget_putchar) return;
    if (n < 0) { __gorget_putchar('-'); n = -n; }
    if (n == 0) { __gorget_putchar('0'); return; }
    char buf[20];
    int i = 0;
    while (n > 0) { buf[i++] = '0' + (n % 10); n /= 10; }
    while (i > 0) __gorget_putchar(buf[--i]);
}

static void __gorget_print_str(const char* s) {
    __gorget_puts(s ? s : "(null)");
}

/* ── Framebuffer API (defined by uefi_stub.c) ───────────────────── */

/* These are defined in uefi_stub.c and called by Gorget code. */
extern void gorget_fb_plot(int64_t x, int64_t y, int64_t color);
extern int64_t gorget_fb_width(void);
extern int64_t gorget_fb_height(void);

/* ── Stub functions for Gorget runtime API compat ───────────────── */

static inline int64_t gorget_mem_live_bytes(void) {
    return (int64_t)__gorget_heap_offset;
}

static inline int64_t gorget_mem_alloc_count(void) {
    return (int64_t)__gorget_alloc_count;
}

/* ── Stubs for hosted-runtime APIs not available in freestanding ── */

static inline void gorget_init_args(int argc, char** argv) {
    (void)argc; (void)argv;
}

/* printf stub: the hosted runtime uses printf for print().
 * In freestanding, print() maps to a simplified output path. */
static inline int printf(const char* fmt, ...) {
    (void)fmt;
    return 0;  /* no-op in freestanding */
}

/* fwrite stub for print() */
static inline size_t fwrite(const void* ptr, size_t size, size_t nmemb, void* stream) {
    (void)stream;
    if (__gorget_putchar) {
        const char* p = (const char*)ptr;
        for (size_t i = 0; i < size * nmemb; i++) __gorget_putchar(p[i]);
    }
    return size * nmemb;
}

static void* stdout = (void*)0;
static void* stdin = (void*)0;

/* snprintf stub for int_to_str / float_to_str */
int snprintf(char* buf, size_t n, const char* fmt, ...);
/* Full snprintf is complex — we only need %lld and %g for freestanding.
 * For now, provide a minimal version. */
static inline void __gorget_int_to_buf(char* buf, size_t n, int64_t val) {
    if (n == 0) return;
    int neg = val < 0;
    if (neg) val = -val;
    char tmp[20];
    int i = 0;
    if (val == 0) tmp[i++] = '0';
    else while (val > 0) { tmp[i++] = '0' + (val % 10); val /= 10; }
    size_t pos = 0;
    if (neg && pos < n - 1) buf[pos++] = '-';
    while (i > 0 && pos < n - 1) buf[pos++] = tmp[--i];
    buf[pos] = '\0';
}
