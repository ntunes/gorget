
// ── Error Handling (setjmp/longjmp) ─────────────────────────
typedef struct {
    char message[256];
    int code;
} GorgetError;

static _Thread_local jmp_buf __gorget_jmp_stack[64];
static _Thread_local int __gorget_jmp_top = -1;
static _Thread_local GorgetError __gorget_last_error;

#define GORGET_TRY (__gorget_jmp_top >= 63 ? (fprintf(stderr, "gorget: try stack overflow\n"), exit(1), 0) : (__gorget_jmp_top++, setjmp(__gorget_jmp_stack[__gorget_jmp_top]) == 0))
#define GORGET_CATCH_END (__gorget_jmp_top--)

static inline void gorget_throw(const char* msg, int code) {
    strncpy(__gorget_last_error.message, msg, 255);
    __gorget_last_error.message[255] = '\0';
    __gorget_last_error.code = code;
    if (__gorget_jmp_top >= 0) {
        longjmp(__gorget_jmp_stack[__gorget_jmp_top], 1);
    } else {
        fprintf(stderr, "Unhandled error: %s\n", msg);
        exit(1);
    }
}

#define GORGET_THROW(msg, code) gorget_throw(msg, code)
#define GORGET_CATCH_ERROR() (__gorget_last_error)

