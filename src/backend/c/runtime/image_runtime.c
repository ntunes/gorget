
// stb_image.h is embedded above (STB_IMAGE_SOURCE) with full
// PNG/JPEG/TGA/BMP support.  The defines below were set before it.

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wunused-parameter"

// Load image from file path
static inline void gorget_image_load(Str path, int64_t* out_tag, int64_t* out_width, int64_t* out_height, int64_t* out_channels, GorgetArray* out_data, Str* out_err) {
    char cpath[4096];
    size_t n = path.len < 4095 ? path.len : 4095;
    memcpy(cpath, path.data, n);
    cpath[n] = '\0';

    FILE* f = fopen(cpath, "rb");
    if (!f) {
        *out_tag = 1; // Error
        *out_err = gorget_str_from_cstr("could not open file");
        return;
    }
    fseek(f, 0, SEEK_END);
    long flen = ftell(f);
    fseek(f, 0, SEEK_SET);
    unsigned char* fbuf = (unsigned char*)malloc(flen);
    fread(fbuf, 1, flen, f);
    fclose(f);

    int w, h, ch;
    unsigned char* pixels = stbi_load_from_memory(fbuf, (int)flen, &w, &h, &ch, 0);
    free(fbuf);
    if (!pixels) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr(stbi_failure_reason());
        return;
    }
    int data_size = w * h * ch;
    GorgetArray arr = gorget_array_new(sizeof(uint8_t));
    gorget_array_ensure_capacity(&arr, data_size, sizeof(uint8_t));
    memcpy(arr.data, pixels, data_size);
    arr.len = data_size;
    stbi_image_free(pixels);

    *out_tag = 0; // Ok
    *out_width = w;
    *out_height = h;
    *out_channels = ch;
    *out_data = arr;
}

// Load image forced to RGBA
static inline void gorget_image_load_rgba(Str path, int64_t* out_tag, int64_t* out_width, int64_t* out_height, int64_t* out_channels, GorgetArray* out_data, Str* out_err) {
    char cpath[4096];
    size_t n = path.len < 4095 ? path.len : 4095;
    memcpy(cpath, path.data, n);
    cpath[n] = '\0';

    FILE* f = fopen(cpath, "rb");
    if (!f) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("could not open file");
        return;
    }
    fseek(f, 0, SEEK_END);
    long flen = ftell(f);
    fseek(f, 0, SEEK_SET);
    unsigned char* fbuf = (unsigned char*)malloc(flen);
    fread(fbuf, 1, flen, f);
    fclose(f);

    int w, h, ch;
    unsigned char* pixels = stbi_load_from_memory(fbuf, (int)flen, &w, &h, &ch, 4);
    free(fbuf);
    if (!pixels) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr(stbi_failure_reason());
        return;
    }
    int data_size = w * h * 4;
    GorgetArray arr = gorget_array_new(sizeof(uint8_t));
    gorget_array_ensure_capacity(&arr, data_size, sizeof(uint8_t));
    memcpy(arr.data, pixels, data_size);
    arr.len = data_size;
    stbi_image_free(pixels);

    *out_tag = 0;
    *out_width = w;
    *out_height = h;
    *out_channels = 4;
    *out_data = arr;
}

// Load image from memory buffer
static inline void gorget_image_load_from_memory(const GorgetArray* data, int64_t* out_tag, int64_t* out_width, int64_t* out_height, int64_t* out_channels, GorgetArray* out_data, Str* out_err) {
    if (!data || !data->data || data->len == 0) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("empty input data");
        return;
    }
    int w, h, ch;
    unsigned char* pixels = stbi_load_from_memory((const unsigned char*)data->data, (int)data->len, &w, &h, &ch, 0);
    if (!pixels) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr(stbi_failure_reason());
        return;
    }
    int data_size = w * h * ch;
    GorgetArray arr = gorget_array_new(sizeof(uint8_t));
    gorget_array_ensure_capacity(&arr, data_size, sizeof(uint8_t));
    memcpy(arr.data, pixels, data_size);
    arr.len = data_size;
    stbi_image_free(pixels);

    *out_tag = 0;
    *out_width = w;
    *out_height = h;
    *out_channels = ch;
    *out_data = arr;
}

// Flip image vertically (for GL texture upload)
static inline void gorget_image_flip_vertically(int64_t width, int64_t height, int64_t channels, const GorgetArray* in_data, int64_t* out_width, int64_t* out_height, int64_t* out_channels, GorgetArray* out_data) {
    int w = (int)width, h = (int)height, ch = (int)channels;
    int row_bytes = w * ch;
    GorgetArray arr = gorget_array_new(sizeof(uint8_t));
    int total = w * h * ch;
    gorget_array_ensure_capacity(&arr, total, sizeof(uint8_t));
    arr.len = total;
    const uint8_t* src = (const uint8_t*)in_data->data;
    uint8_t* dst = (uint8_t*)arr.data;
    for (int y = 0; y < h; y++) {
        memcpy(dst + y * row_bytes, src + (h - 1 - y) * row_bytes, row_bytes);
    }
    *out_width = width;
    *out_height = height;
    *out_channels = channels;
    *out_data = arr;
}

// ── Image Info (header-only query) ──────────────────────────

#ifdef GORGET_HAS_STB_IMAGE
#ifndef GORGET_HAS_STB_IMAGE_INFO
#define GORGET_HAS_STB_IMAGE_INFO 1
#endif
#endif

static inline void gorget_image_info(Str path, int64_t* out_tag, int64_t* out_width, int64_t* out_height, int64_t* out_channels, GorgetArray* out_data, Str* out_err) {
    char cpath[4096];
    size_t n = path.len < 4095 ? path.len : 4095;
    memcpy(cpath, path.data, n);
    cpath[n] = '\0';

    FILE* f = fopen(cpath, "rb");
    if (!f) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("could not open file");
        return;
    }
    fseek(f, 0, SEEK_END);
    long flen = ftell(f);
    fseek(f, 0, SEEK_SET);
    unsigned char* fbuf = (unsigned char*)malloc(flen);
    fread(fbuf, 1, flen, f);
    fclose(f);

#ifdef GORGET_HAS_STB_IMAGE_INFO
    int w, h, ch;
    if (!stbi_info_from_memory(fbuf, (int)flen, &w, &h, &ch)) {
        free(fbuf);
        *out_tag = 1;
        *out_err = gorget_str_from_cstr(stbi_failure_reason());
        return;
    }
    free(fbuf);
    *out_tag = 0;
    *out_width = w;
    *out_height = h;
    *out_channels = ch;
    *out_data = gorget_array_new(sizeof(uint8_t)); // empty data for info-only
#else
    // Fallback: do full decode
    int w, h, ch;
    unsigned char* pixels = stbi_load_from_memory(fbuf, (int)flen, &w, &h, &ch, 0);
    free(fbuf);
    if (!pixels) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("could not read image info");
        return;
    }
    stbi_image_free(pixels);
    *out_tag = 0;
    *out_width = w;
    *out_height = h;
    *out_channels = ch;
    *out_data = gorget_array_new(sizeof(uint8_t));
#endif
}

static inline void gorget_image_info_from_memory(const GorgetArray* data, int64_t* out_tag, int64_t* out_width, int64_t* out_height, int64_t* out_channels, GorgetArray* out_data, Str* out_err) {
    if (!data || !data->data || data->len == 0) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("empty input data");
        return;
    }
#ifdef GORGET_HAS_STB_IMAGE_INFO
    int w, h, ch;
    if (!stbi_info_from_memory((const unsigned char*)data->data, (int)data->len, &w, &h, &ch)) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr(stbi_failure_reason());
        return;
    }
    *out_tag = 0;
    *out_width = w;
    *out_height = h;
    *out_channels = ch;
    *out_data = gorget_array_new(sizeof(uint8_t));
#else
    int w, h, ch;
    unsigned char* pixels = stbi_load_from_memory((const unsigned char*)data->data, (int)data->len, &w, &h, &ch, 0);
    if (!pixels) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("could not read image info");
        return;
    }
    stbi_image_free(pixels);
    *out_tag = 0;
    *out_width = w;
    *out_height = h;
    *out_channels = ch;
    *out_data = gorget_array_new(sizeof(uint8_t));
#endif
}

// Load from memory forced RGBA
static inline void gorget_image_load_rgba_from_memory(const GorgetArray* data, int64_t* out_tag, int64_t* out_width, int64_t* out_height, int64_t* out_channels, GorgetArray* out_data, Str* out_err) {
    if (!data || !data->data || data->len == 0) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("empty input data");
        return;
    }
    int w, h, ch;
    unsigned char* pixels = stbi_load_from_memory((const unsigned char*)data->data, (int)data->len, &w, &h, &ch, 4);
    if (!pixels) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr(stbi_failure_reason());
        return;
    }
    int data_size = w * h * 4;
    GorgetArray arr = gorget_array_new(sizeof(uint8_t));
    gorget_array_ensure_capacity(&arr, data_size, sizeof(uint8_t));
    memcpy(arr.data, pixels, data_size);
    arr.len = data_size;
    stbi_image_free(pixels);

    *out_tag = 0;
    *out_width = w;
    *out_height = h;
    *out_channels = 4;
    *out_data = arr;
}

// ── Image Resize (bilinear) ─────────────────────────────────

#ifdef GORGET_HAS_STB_IMAGE
#ifndef STB_IMAGE_RESIZE_IMPLEMENTATION
// Try system stb_image_resize2.h or stb_image_resize.h
#ifdef __has_include
#if __has_include("stb_image_resize2.h")
#define STB_IMAGE_RESIZE_IMPLEMENTATION
#include "stb_image_resize2.h"
#define GORGET_HAS_STB_RESIZE 1
#elif __has_include("stb_image_resize.h")
#define STB_IMAGE_RESIZE_IMPLEMENTATION
#include "stb_image_resize.h"
#define GORGET_HAS_STB_RESIZE_V1 1
#endif
#endif
#endif
#endif

static inline void gorget_image_resize(int64_t width, int64_t height, int64_t channels, const GorgetArray* in_data, int64_t new_width, int64_t new_height,
    int64_t* out_tag, int64_t* out_width, int64_t* out_height, int64_t* out_channels, GorgetArray* out_data, Str* out_err) {

    int w = (int)width, h = (int)height, ch = (int)channels;
    int nw = (int)new_width, nh = (int)new_height;

    if (nw <= 0 || nh <= 0) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("invalid resize dimensions");
        return;
    }

    int out_size = nw * nh * ch;
    GorgetArray arr = gorget_array_new(sizeof(uint8_t));
    gorget_array_ensure_capacity(&arr, out_size, sizeof(uint8_t));
    arr.len = out_size;

#if defined(GORGET_HAS_STB_RESIZE)
    stbir_resize_uint8_linear((const unsigned char*)in_data->data, w, h, w * ch, (unsigned char*)arr.data, nw, nh, nw * ch, (stbir_pixel_layout)ch);
#elif defined(GORGET_HAS_STB_RESIZE_V1)
    stbir_resize_uint8((const unsigned char*)in_data->data, w, h, w * ch, (unsigned char*)arr.data, nw, nh, nw * ch, ch);
#else
    // Manual bilinear resize fallback
    const unsigned char* src = (const unsigned char*)in_data->data;
    unsigned char* dst = (unsigned char*)arr.data;
    for (int y = 0; y < nh; y++) {
        float fy = (float)y * (float)(h - 1) / (float)(nh > 1 ? nh - 1 : 1);
        int y0 = (int)fy;
        int y1 = y0 + 1 < h ? y0 + 1 : y0;
        float yw = fy - y0;
        for (int x = 0; x < nw; x++) {
            float fx = (float)x * (float)(w - 1) / (float)(nw > 1 ? nw - 1 : 1);
            int x0 = (int)fx;
            int x1 = x0 + 1 < w ? x0 + 1 : x0;
            float xw = fx - x0;
            for (int c = 0; c < ch; c++) {
                float v = src[(y0 * w + x0) * ch + c] * (1 - xw) * (1 - yw) +
                          src[(y0 * w + x1) * ch + c] * xw * (1 - yw) +
                          src[(y1 * w + x0) * ch + c] * (1 - xw) * yw +
                          src[(y1 * w + x1) * ch + c] * xw * yw;
                dst[(y * nw + x) * ch + c] = (unsigned char)(v + 0.5f);
            }
        }
    }
#endif

    *out_tag = 0;
    *out_width = nw;
    *out_height = nh;
    *out_channels = ch;
    *out_data = arr;
}

// ── Image Write (stb_image_write fallback) ──────────────────

#ifdef __has_include
#if __has_include("stb_image_write.h")
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"
#define GORGET_HAS_STB_WRITE 1
#endif
#endif

#ifndef GORGET_HAS_STB_WRITE
// Minimal TGA writer fallback
static int gorget_write_tga(const char* path, int w, int h, int ch, const unsigned char* data) {
    FILE* f = fopen(path, "wb");
    if (!f) return 0;
    unsigned char hdr[18] = {0};
    hdr[2] = 2; // uncompressed true-color
    hdr[12] = w & 0xFF; hdr[13] = (w >> 8) & 0xFF;
    hdr[14] = h & 0xFF; hdr[15] = (h >> 8) & 0xFF;
    hdr[16] = ch * 8;
    fwrite(hdr, 1, 18, f);
    // Write in BGR order (TGA format)
    for (int i = 0; i < w * h; i++) {
        unsigned char bgr[4];
        bgr[0] = data[i * ch + 2];
        bgr[1] = data[i * ch + 1];
        bgr[2] = data[i * ch + 0];
        if (ch >= 4) bgr[3] = data[i * ch + 3];
        fwrite(bgr, 1, ch, f);
    }
    fclose(f);
    return 1;
}
#endif

static inline void gorget_image_write_png(Str path, int64_t width, int64_t height, int64_t channels, const GorgetArray* data,
    int64_t* out_tag, int64_t* out_val, Str* out_err) {
    char cpath[4096];
    size_t n = path.len < 4095 ? path.len : 4095;
    memcpy(cpath, path.data, n);
    cpath[n] = '\0';
#ifdef GORGET_HAS_STB_WRITE
    int ok = stbi_write_png(cpath, (int)width, (int)height, (int)channels, data->data, (int)(width * channels));
    if (!ok) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("PNG write failed");
        return;
    }
    *out_tag = 0;
    *out_val = 1;
#else
    // Fallback: write as TGA
    int ok = gorget_write_tga(cpath, (int)width, (int)height, (int)channels, (const unsigned char*)data->data);
    if (!ok) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("image write failed (no stb_image_write, TGA fallback failed)");
        return;
    }
    *out_tag = 0;
    *out_val = 1;
#endif
}

static inline void gorget_image_write_jpg(Str path, int64_t width, int64_t height, int64_t channels, const GorgetArray* data, int64_t quality,
    int64_t* out_tag, int64_t* out_val, Str* out_err) {
    char cpath[4096];
    size_t n = path.len < 4095 ? path.len : 4095;
    memcpy(cpath, path.data, n);
    cpath[n] = '\0';
#ifdef GORGET_HAS_STB_WRITE
    int ok = stbi_write_jpg(cpath, (int)width, (int)height, (int)channels, data->data, (int)quality);
    if (!ok) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("JPG write failed");
        return;
    }
    *out_tag = 0;
    *out_val = 1;
#else
    *out_tag = 1;
    *out_err = gorget_str_from_cstr("JPG write not available (no stb_image_write)");
#endif
}

// Callback for stbi_write_*_to_func
#ifdef GORGET_HAS_STB_WRITE
static void gorget_stb_write_callback(void* context, void* data, int size) {
    GorgetArray* arr = (GorgetArray*)context;
    size_t old_len = arr->len;
    gorget_array_ensure_capacity(arr, old_len + size, sizeof(uint8_t));
    memcpy((uint8_t*)arr->data + old_len, data, size);
    arr->len = old_len + size;
}
#endif

static inline void gorget_image_encode_png(int64_t width, int64_t height, int64_t channels, const GorgetArray* data,
    int64_t* out_tag, GorgetArray* out_data, Str* out_err) {
#ifdef GORGET_HAS_STB_WRITE
    GorgetArray arr = gorget_array_new(sizeof(uint8_t));
    int ok = stbi_write_png_to_func(gorget_stb_write_callback, &arr, (int)width, (int)height, (int)channels, data->data, (int)(width * channels));
    if (!ok) {
        gorget_array_free(&arr);
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("PNG encode failed");
        return;
    }
    *out_tag = 0;
    *out_data = arr;
#else
    *out_tag = 1;
    *out_err = gorget_str_from_cstr("PNG encode not available (no stb_image_write)");
#endif
}
