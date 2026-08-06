
// ── Math functions ───────────────────────────────────────────
static inline int64_t gorget_abs(int64_t x) { return x < 0 ? -x : x; }
static inline int64_t gorget_min(int64_t a, int64_t b) { return a < b ? a : b; }
static inline int64_t gorget_max(int64_t a, int64_t b) { return a > b ? a : b; }
static inline double gorget_sqrt(double x) { return sqrt(x); }
static inline double gorget_pow(double base, double exp) { return pow(base, exp); }
static inline float  gorget_powf(float base, float exp) { return powf(base, exp); }

// ── D28 integer `**` — checked exponentiation ────────────────
// One helper per width: trap `T_Overflow` on overflow OR on negative
// exponent (domain-fault per amendment R3 — a fallible `**!` deferred).
// All widths use exponentiation-by-squaring with a `__builtin_mul_overflow`
// check on each multiply so any overflow — mid-loop or final — traps.
// Unsigned variants use the same shape; negative exponent is impossible
// (unsigned exp cannot be negative), so the sign check is signed-only.
static inline int64_t gorget_pow_checked_i64(int64_t base, int64_t exp) {
    if (exp < 0) gorget_trap("T_Overflow", "negative exponent in `**`");
    int64_t result = 1;
    int64_t b = base;
    int64_t e = exp;
    while (e > 0) {
        if (e & 1) {
            int64_t r;
            if (__builtin_mul_overflow(result, b, &r)) gorget_trap("T_Overflow", "integer `**` overflow");
            result = r;
        }
        e >>= 1;
        if (e > 0) {
            int64_t bb;
            if (__builtin_mul_overflow(b, b, &bb)) gorget_trap("T_Overflow", "integer `**` overflow");
            b = bb;
        }
    }
    return result;
}
static inline int32_t gorget_pow_checked_i32(int32_t base, int32_t exp) {
    if (exp < 0) gorget_trap("T_Overflow", "negative exponent in `**`");
    int32_t result = 1;
    int32_t b = base;
    int32_t e = exp;
    while (e > 0) {
        if (e & 1) {
            int32_t r;
            if (__builtin_mul_overflow(result, b, &r)) gorget_trap("T_Overflow", "integer `**` overflow");
            result = r;
        }
        e >>= 1;
        if (e > 0) {
            int32_t bb;
            if (__builtin_mul_overflow(b, b, &bb)) gorget_trap("T_Overflow", "integer `**` overflow");
            b = bb;
        }
    }
    return result;
}
static inline int16_t gorget_pow_checked_i16(int16_t base, int16_t exp) {
    if (exp < 0) gorget_trap("T_Overflow", "negative exponent in `**`");
    int16_t result = 1;
    int16_t b = base;
    int16_t e = exp;
    while (e > 0) {
        if (e & 1) {
            int16_t r;
            if (__builtin_mul_overflow(result, b, &r)) gorget_trap("T_Overflow", "integer `**` overflow");
            result = r;
        }
        e >>= 1;
        if (e > 0) {
            int16_t bb;
            if (__builtin_mul_overflow(b, b, &bb)) gorget_trap("T_Overflow", "integer `**` overflow");
            b = bb;
        }
    }
    return result;
}
static inline int8_t gorget_pow_checked_i8(int8_t base, int8_t exp) {
    if (exp < 0) gorget_trap("T_Overflow", "negative exponent in `**`");
    int8_t result = 1;
    int8_t b = base;
    int8_t e = exp;
    while (e > 0) {
        if (e & 1) {
            int8_t r;
            if (__builtin_mul_overflow(result, b, &r)) gorget_trap("T_Overflow", "integer `**` overflow");
            result = r;
        }
        e >>= 1;
        if (e > 0) {
            int8_t bb;
            if (__builtin_mul_overflow(b, b, &bb)) gorget_trap("T_Overflow", "integer `**` overflow");
            b = bb;
        }
    }
    return result;
}
static inline uint64_t gorget_pow_checked_u64(uint64_t base, uint64_t exp) {
    uint64_t result = 1;
    uint64_t b = base;
    uint64_t e = exp;
    while (e > 0) {
        if (e & 1) {
            uint64_t r;
            if (__builtin_mul_overflow(result, b, &r)) gorget_trap("T_Overflow", "integer `**` overflow");
            result = r;
        }
        e >>= 1;
        if (e > 0) {
            uint64_t bb;
            if (__builtin_mul_overflow(b, b, &bb)) gorget_trap("T_Overflow", "integer `**` overflow");
            b = bb;
        }
    }
    return result;
}
static inline uint32_t gorget_pow_checked_u32(uint32_t base, uint32_t exp) {
    uint32_t result = 1;
    uint32_t b = base;
    uint32_t e = exp;
    while (e > 0) {
        if (e & 1) {
            uint32_t r;
            if (__builtin_mul_overflow(result, b, &r)) gorget_trap("T_Overflow", "integer `**` overflow");
            result = r;
        }
        e >>= 1;
        if (e > 0) {
            uint32_t bb;
            if (__builtin_mul_overflow(b, b, &bb)) gorget_trap("T_Overflow", "integer `**` overflow");
            b = bb;
        }
    }
    return result;
}
static inline uint16_t gorget_pow_checked_u16(uint16_t base, uint16_t exp) {
    uint16_t result = 1;
    uint16_t b = base;
    uint16_t e = exp;
    while (e > 0) {
        if (e & 1) {
            uint16_t r;
            if (__builtin_mul_overflow(result, b, &r)) gorget_trap("T_Overflow", "integer `**` overflow");
            result = r;
        }
        e >>= 1;
        if (e > 0) {
            uint16_t bb;
            if (__builtin_mul_overflow(b, b, &bb)) gorget_trap("T_Overflow", "integer `**` overflow");
            b = bb;
        }
    }
    return result;
}
static inline uint8_t gorget_pow_checked_u8(uint8_t base, uint8_t exp) {
    uint8_t result = 1;
    uint8_t b = base;
    uint8_t e = exp;
    while (e > 0) {
        if (e & 1) {
            uint8_t r;
            if (__builtin_mul_overflow(result, b, &r)) gorget_trap("T_Overflow", "integer `**` overflow");
            result = r;
        }
        e >>= 1;
        if (e > 0) {
            uint8_t bb;
            if (__builtin_mul_overflow(b, b, &bb)) gorget_trap("T_Overflow", "integer `**` overflow");
            b = bb;
        }
    }
    return result;
}
static inline double gorget_floor(double x) { return floor(x); }
static inline double gorget_ceil(double x) { return ceil(x); }
static inline double gorget_round(double x) { return round(x); }
static inline double gorget_log(double x) { return log(x); }
static inline double gorget_log2(double x) { return log2(x); }
static inline double gorget_log10(double x) { return log10(x); }
static inline double gorget_sin(double x) { return sin(x); }
static inline double gorget_cos(double x) { return cos(x); }
static inline double gorget_tan(double x) { return tan(x); }
static inline double gorget_asin(double x) { return asin(x); }
static inline double gorget_acos(double x) { return acos(x); }
static inline double gorget_atan(double x) { return atan(x); }
static inline double gorget_atan2(double y, double x) { return atan2(y, x); }
static inline double gorget_fabs(double x) { return fabs(x); }
static inline double gorget_fmin(double a, double b) { return fmin(a, b); }
static inline double gorget_fmax(double a, double b) { return fmax(a, b); }

// ── Math constants ──────────────────────────────────────────
static const double GORGET_PI = 3.141592653589793;
static const double GORGET_E = 2.718281828459045;
static const double GORGET_TAU = 6.283185307179586;
#define GORGET_INFINITY INFINITY
#define GORGET_NAN NAN
static inline double gorget_math_infinity(void) { return INFINITY; }
static inline double gorget_math_nan(void) { return NAN; }

