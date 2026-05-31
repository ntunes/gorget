
// ── Checked Arithmetic ──────────────────────────────────────
// Inline helpers call __builtin_*_overflow (integer-only).
// _Generic macros dispatch: integer types → checked, double → plain.
// Both branches are valid C, so Clang type-checks them safely.
static inline int64_t gorget_checked_add_i64(int64_t a, int64_t b) {
    int64_t res;
    if (__builtin_add_overflow(a, b, &res)) gorget_panic("integer overflow");
    return res;
}
static inline int64_t gorget_checked_sub_i64(int64_t a, int64_t b) {
    int64_t res;
    if (__builtin_sub_overflow(a, b, &res)) gorget_panic("integer overflow");
    return res;
}
static inline int64_t gorget_checked_mul_i64(int64_t a, int64_t b) {
    int64_t res;
    if (__builtin_mul_overflow(a, b, &res)) gorget_panic("integer overflow");
    return res;
}

#define GORGET_CHECKED_ADD(a, b) _Generic((a), \
    long long: gorget_checked_add_i64((a), (b)), \
    long: gorget_checked_add_i64((a), (b)), \
    default: ((a) + (b)))

#define GORGET_CHECKED_SUB(a, b) _Generic((a), \
    long long: gorget_checked_sub_i64((a), (b)), \
    long: gorget_checked_sub_i64((a), (b)), \
    default: ((a) - (b)))

#define GORGET_CHECKED_MUL(a, b) _Generic((a), \
    long long: gorget_checked_mul_i64((a), (b)), \
    long: gorget_checked_mul_i64((a), (b)), \
    default: ((a) * (b)))

#define GORGET_CHECKED_ADD_ASSIGN(t, v) \
    ((t) = _Generic((t), \
        long long: gorget_checked_add_i64((t), (v)), \
        long: gorget_checked_add_i64((t), (v)), \
        default: ((t) + (v))))

#define GORGET_CHECKED_SUB_ASSIGN(t, v) \
    ((t) = _Generic((t), \
        long long: gorget_checked_sub_i64((t), (v)), \
        long: gorget_checked_sub_i64((t), (v)), \
        default: ((t) - (v))))

#define GORGET_CHECKED_MUL_ASSIGN(t, v) \
    ((t) = _Generic((t), \
        long long: gorget_checked_mul_i64((t), (v)), \
        long: gorget_checked_mul_i64((t), (v)), \
        default: ((t) * (v))))

