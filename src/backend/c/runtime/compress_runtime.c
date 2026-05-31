
#ifdef __has_include
#if __has_include("miniz.h")
#include "miniz.h"
#define GORGET_HAS_MINIZ 1
#endif
#endif

#ifndef GORGET_HAS_MINIZ
// Minimal zlib-compatible uncompress using the system zlib if available,
// otherwise a stub that returns an error.
#ifdef __has_include
#if __has_include(<zlib.h>)
#include <zlib.h>
#define GORGET_HAS_ZLIB 1
#endif
#endif
#endif

static inline void gorget_zlib_decompress(const GorgetArray* data, int64_t uncompressed_size, int64_t* out_tag, GorgetArray* out_data, Str* out_err) {
#if defined(GORGET_HAS_MINIZ) || defined(GORGET_HAS_ZLIB)
    if (!data || !data->data || data->len == 0) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("empty input");
        return;
    }
    unsigned long dest_len = (unsigned long)uncompressed_size;
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    gorget_array_ensure_capacity(&out, (size_t)dest_len, sizeof(uint8_t));
    int rc = uncompress((unsigned char*)out.data, &dest_len, (const unsigned char*)data->data, (unsigned long)data->len);
    if (rc != 0) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("zlib decompress failed");
        gorget_array_free(&out);
        return;
    }
    out.len = (size_t)dest_len;
    *out_tag = 0;
    *out_data = out;
#else
    *out_tag = 1;
    *out_err = gorget_str_from_cstr("no zlib/miniz available");
#endif
}

static inline void gorget_zlib_compress(const GorgetArray* data, int64_t* out_tag, GorgetArray* out_data, Str* out_err) {
#if defined(GORGET_HAS_MINIZ) || defined(GORGET_HAS_ZLIB)
    if (!data || !data->data || data->len == 0) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("empty input");
        return;
    }
    unsigned long dest_len = compressBound((unsigned long)data->len);
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    gorget_array_ensure_capacity(&out, (size_t)dest_len, sizeof(uint8_t));
    int rc = compress((unsigned char*)out.data, &dest_len, (const unsigned char*)data->data, (unsigned long)data->len);
    if (rc != 0) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("zlib compress failed");
        gorget_array_free(&out);
        return;
    }
    out.len = (size_t)dest_len;
    *out_tag = 0;
    *out_data = out;
#else
    *out_tag = 1;
    *out_err = gorget_str_from_cstr("no zlib/miniz available");
#endif
}

// Compress with specific level (0=none, 1=fastest, 9=best)
static inline void gorget_zlib_compress_level(const GorgetArray* data, int64_t level, int64_t* out_tag, GorgetArray* out_data, Str* out_err) {
#if defined(GORGET_HAS_MINIZ) || defined(GORGET_HAS_ZLIB)
    if (!data || !data->data || data->len == 0) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("empty input");
        return;
    }
    unsigned long dest_len = compressBound((unsigned long)data->len);
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    gorget_array_ensure_capacity(&out, (size_t)dest_len, sizeof(uint8_t));
    int rc = compress2((unsigned char*)out.data, &dest_len, (const unsigned char*)data->data, (unsigned long)data->len, (int)level);
    if (rc != 0) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("zlib compress failed");
        gorget_array_free(&out);
        return;
    }
    out.len = (size_t)dest_len;
    *out_tag = 0;
    *out_data = out;
#else
    *out_tag = 1;
    *out_err = gorget_str_from_cstr("no zlib/miniz available");
#endif
}

// Raw deflate compress (no zlib header)
static inline void gorget_deflate_compress(const GorgetArray* data, int64_t* out_tag, GorgetArray* out_data, Str* out_err) {
#if defined(GORGET_HAS_MINIZ)
    if (!data || !data->data || data->len == 0) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("empty input");
        return;
    }
    mz_ulong dest_len = mz_compressBound((mz_ulong)data->len);
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    gorget_array_ensure_capacity(&out, (size_t)dest_len, sizeof(uint8_t));
    mz_stream stream;
    memset(&stream, 0, sizeof(stream));
    if (mz_deflateInit2(&stream, MZ_DEFAULT_LEVEL, MZ_DEFLATED, -MZ_DEFAULT_WINDOW_BITS, 9, MZ_DEFAULT_STRATEGY) != MZ_OK) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("deflate init failed");
        gorget_array_free(&out);
        return;
    }
    stream.next_in = (const unsigned char*)data->data;
    stream.avail_in = (mz_uint32)data->len;
    stream.next_out = (unsigned char*)out.data;
    stream.avail_out = (mz_uint32)dest_len;
    int rc = mz_deflate(&stream, MZ_FINISH);
    mz_deflateEnd(&stream);
    if (rc != MZ_STREAM_END) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("deflate compress failed");
        gorget_array_free(&out);
        return;
    }
    out.len = stream.total_out;
    *out_tag = 0;
    *out_data = out;
#elif defined(GORGET_HAS_ZLIB)
    if (!data || !data->data || data->len == 0) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("empty input");
        return;
    }
    uLongf dest_len = compressBound((uLong)data->len);
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    gorget_array_ensure_capacity(&out, (size_t)dest_len, sizeof(uint8_t));
    z_stream stream;
    memset(&stream, 0, sizeof(stream));
    if (deflateInit2(&stream, Z_DEFAULT_COMPRESSION, Z_DEFLATED, -MAX_WBITS, 8, Z_DEFAULT_STRATEGY) != Z_OK) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("deflate init failed");
        gorget_array_free(&out);
        return;
    }
    stream.next_in = (Bytef*)data->data;
    stream.avail_in = (uInt)data->len;
    stream.next_out = (Bytef*)out.data;
    stream.avail_out = (uInt)dest_len;
    int rc = deflate(&stream, Z_FINISH);
    deflateEnd(&stream);
    if (rc != Z_STREAM_END) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("deflate compress failed");
        gorget_array_free(&out);
        return;
    }
    out.len = stream.total_out;
    *out_tag = 0;
    *out_data = out;
#else
    *out_tag = 1;
    *out_err = gorget_str_from_cstr("no zlib/miniz available");
#endif
}

// Raw deflate decompress (no zlib header)
static inline void gorget_deflate_decompress(const GorgetArray* data, int64_t uncompressed_size, int64_t* out_tag, GorgetArray* out_data, Str* out_err) {
#if defined(GORGET_HAS_MINIZ)
    if (!data || !data->data || data->len == 0) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("empty input");
        return;
    }
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    gorget_array_ensure_capacity(&out, (size_t)uncompressed_size, sizeof(uint8_t));
    mz_stream stream;
    memset(&stream, 0, sizeof(stream));
    if (mz_inflateInit2(&stream, -MZ_DEFAULT_WINDOW_BITS) != MZ_OK) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("inflate init failed");
        gorget_array_free(&out);
        return;
    }
    stream.next_in = (const unsigned char*)data->data;
    stream.avail_in = (mz_uint32)data->len;
    stream.next_out = (unsigned char*)out.data;
    stream.avail_out = (mz_uint32)uncompressed_size;
    int rc = mz_inflate(&stream, MZ_FINISH);
    mz_inflateEnd(&stream);
    if (rc != MZ_STREAM_END) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("deflate decompress failed");
        gorget_array_free(&out);
        return;
    }
    out.len = stream.total_out;
    *out_tag = 0;
    *out_data = out;
#elif defined(GORGET_HAS_ZLIB)
    if (!data || !data->data || data->len == 0) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("empty input");
        return;
    }
    GorgetArray out = gorget_array_new(sizeof(uint8_t));
    gorget_array_ensure_capacity(&out, (size_t)uncompressed_size, sizeof(uint8_t));
    z_stream stream;
    memset(&stream, 0, sizeof(stream));
    if (inflateInit2(&stream, -MAX_WBITS) != Z_OK) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("inflate init failed");
        gorget_array_free(&out);
        return;
    }
    stream.next_in = (Bytef*)data->data;
    stream.avail_in = (uInt)data->len;
    stream.next_out = (Bytef*)out.data;
    stream.avail_out = (uInt)uncompressed_size;
    int rc = inflate(&stream, Z_FINISH);
    inflateEnd(&stream);
    if (rc != Z_STREAM_END) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("deflate decompress failed");
        gorget_array_free(&out);
        return;
    }
    out.len = stream.total_out;
    *out_tag = 0;
    *out_data = out;
#else
    *out_tag = 1;
    *out_err = gorget_str_from_cstr("no zlib/miniz available");
#endif
}

// CRC32 (for ZIP/PK3 verification)
static inline int64_t gorget_crc32_compute(const GorgetArray* data) {
    if (!data || !data->data || data->len == 0) return 0;
#if defined(GORGET_HAS_MINIZ) || defined(GORGET_HAS_ZLIB)
    return (int64_t)crc32(crc32(0, NULL, 0), (const unsigned char*)data->data, (unsigned int)data->len);
#else
    // Minimal CRC32 fallback
    uint32_t crc = 0xFFFFFFFF;
    const uint8_t* p = (const uint8_t*)data->data;
    for (size_t i = 0; i < data->len; i++) {
        crc ^= p[i];
        for (int j = 0; j < 8; j++) {
            crc = (crc >> 1) ^ (0xEDB88320 & (-(crc & 1)));
        }
    }
    return (int64_t)(crc ^ 0xFFFFFFFF);
#endif
}
