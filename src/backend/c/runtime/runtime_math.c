
// ── Math functions ───────────────────────────────────────────
static inline int64_t gorget_abs(int64_t x) { return x < 0 ? -x : x; }
static inline int64_t gorget_min(int64_t a, int64_t b) { return a < b ? a : b; }
static inline int64_t gorget_max(int64_t a, int64_t b) { return a > b ? a : b; }
static inline double gorget_sqrt(double x) { return sqrt(x); }
static inline double gorget_pow(double base, double exp) { return pow(base, exp); }
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

