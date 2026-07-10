
// Write a 32-bit IEEE 754 float (from f64) in little-endian at offset
static inline void gorget_bytes_write_f32_le(GorgetArray* arr, int64_t offset, double value) {
    if (offset < 0 || (size_t)(offset + 4) > arr->len) {
        gorget_trap_fmt(GG_T_BOUNDS, "bytes_write_f32_le: offset %lld out of bounds (len %zu)", (long long)offset, arr->len);
    }
    float fv = (float)value;
    uint8_t* p = (uint8_t*)arr->data + offset;
    memcpy(p, &fv, 4);
}

// Read a signed 32-bit integer from little-endian bytes, sign-extending to i64
static inline int64_t gorget_bytes_read_i32_le(const GorgetArray* arr, int64_t offset) {
    if (offset < 0 || (size_t)(offset + 4) > arr->len) {
        gorget_trap_fmt(GG_T_BOUNDS, "bytes_read_i32_le: offset %lld out of bounds (len %zu)", (long long)offset, arr->len);
    }
    int32_t v;
    const uint8_t* p = (const uint8_t*)arr->data + offset;
    memcpy(&v, p, 4);
    return (int64_t)v;  // sign-extends i32 → i64
}

// Write a signed 32-bit integer to little-endian bytes
static inline void gorget_bytes_write_i32_le(GorgetArray* arr, int64_t offset, int64_t val) {
    if (offset < 0 || (size_t)(offset + 4) > arr->len) {
        gorget_trap_fmt(GG_T_BOUNDS, "bytes_write_i32_le: offset %lld out of bounds (len %zu)", (long long)offset, arr->len);
    }
    int32_t v = (int32_t)val;
    uint8_t* p = (uint8_t*)arr->data + offset;
    memcpy(p, &v, 4);
}

// Read a 32-bit IEEE 754 float from little-endian at offset, return as f64
static inline double gorget_bytes_read_f32_le(const GorgetArray* arr, int64_t offset) {
    if (offset < 0 || (size_t)(offset + 4) > arr->len) {
        gorget_trap_fmt(GG_T_BOUNDS, "bytes_read_f32_le: offset %lld out of bounds (len %zu)", (long long)offset, arr->len);
    }
    float fv;
    const uint8_t* p = (const uint8_t*)arr->data + offset;
    memcpy(&fv, p, 4);
    return (double)fv;
}

// Write a 64-bit IEEE 754 float in little-endian at offset
static inline void gorget_bytes_write_f64_le(GorgetArray* arr, int64_t offset, double value) {
    if (offset < 0 || (size_t)(offset + 8) > arr->len) {
        gorget_trap_fmt(GG_T_BOUNDS, "bytes_write_f64_le: offset %lld out of bounds (len %zu)", (long long)offset, arr->len);
    }
    uint8_t* p = (uint8_t*)arr->data + offset;
    memcpy(p, &value, 8);
}

// Read a 64-bit IEEE 754 float from little-endian at offset
static inline double gorget_bytes_read_f64_le(const GorgetArray* arr, int64_t offset) {
    if (offset < 0 || (size_t)(offset + 8) > arr->len) {
        gorget_trap_fmt(GG_T_BOUNDS, "bytes_read_f64_le: offset %lld out of bounds (len %zu)", (long long)offset, arr->len);
    }
    double dv;
    const uint8_t* p = (const uint8_t*)arr->data + offset;
    memcpy(&dv, p, 8);
    return dv;
}

// Write a little-endian int64 at offset
static inline void gorget_bytes_write_i64_le(GorgetArray* arr, int64_t offset, int64_t value) {
    if (offset < 0 || (size_t)(offset + 8) > arr->len) {
        gorget_trap_fmt(GG_T_BOUNDS, "bytes_write_i64_le: offset %lld out of bounds (len %zu)", (long long)offset, arr->len);
    }
    uint8_t* p = (uint8_t*)arr->data + offset;
    memcpy(p, &value, 8);
}

// Read a little-endian int64 from offset
static inline int64_t gorget_bytes_read_i64_le(const GorgetArray* arr, int64_t offset) {
    if (offset < 0 || (size_t)(offset + 8) > arr->len) {
        gorget_trap_fmt(GG_T_BOUNDS, "bytes_read_i64_le: offset %lld out of bounds (len %zu)", (long long)offset, arr->len);
    }
    int64_t iv;
    const uint8_t* p = (const uint8_t*)arr->data + offset;
    memcpy(&iv, p, 8);
    return iv;
}
