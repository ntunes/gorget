
// ── GorgetFile ──────────────────────────────────────────────
typedef struct {
    FILE* handle;
    bool  owned;   // false for stdin/stdout/stderr
} GorgetFile;

static inline GorgetFile gorget_file_open(const char* path, const char* mode) {
    FILE* f = fopen(path, mode);
    if (!f) { fprintf(stderr, "Error: cannot open '%s'\n", path); exit(1); }
    return (GorgetFile){f, true};
}

static inline void gorget_file_close(GorgetFile* f) {
    if (f->handle && f->owned) { fclose(f->handle); f->handle = NULL; }
}

static inline GorgetString gorget_file_read_all(GorgetFile* f) {
    fseek(f->handle, 0, SEEK_END);
    long len = ftell(f->handle);
    fseek(f->handle, 0, SEEK_SET);
    if (len <= 0) return GORGET_EMPTY_STR;
    char* buf = (char*)GORGET_ALLOC((size_t)len + 1);
    size_t n = fread(buf, 1, (size_t)len, f->handle);
    buf[n] = '\0';
    // Use fread's reported byte count directly — going through strlen would
    // silently truncate at the first embedded NUL byte (e.g., UTF-16 source,
    // binary blobs read as text). Mirrors gorget_bytes_to_str's NUL-preserving
    // shape (commit a6b77b2b).
    Str result = str_alloc_copy(buf, n, __gorget_current_alloc);
    GORGET_FREE(buf, (size_t)len + 1);
    return result;
}

static inline void gorget_file_write(GorgetFile* f, const char* s) {
    if (s) fputs(s, f->handle);
}

// Flush the C stdio buffer attached to this file. Returns 0 on success,
// -errno on failure (mirroring gorget_file_write_bytes_buf's convention).
// Callers in Gorget land wrap via `File.flush() -> Result[void, IoError]`.
static inline int64_t gorget_file_flush(GorgetFile* f) {
    if (!f->handle) return 0;
    if (fflush(f->handle) == 0) return 0;
    return -errno;
}

// ── File byte-oriented writer ───────────────────────────────
//
// Writes up to `buf->len` bytes from `buf->data` to the file.
// Returns the number of bytes actually written (>= 0) on success, or
// a negative errno on failure so the Gorget-side Writer wrapper can
// map it to the appropriate `IoError` variant.
//
// Short writes are legitimate (sockets/pipes). The caller's
// `write_all` helper loops until the full buffer is written.
static inline int64_t gorget_file_write_bytes_buf(GorgetFile* f, const GorgetArray* buf) {
    if (!f || !f->handle || !buf) return 0;
    if (!buf->data || buf->len == 0) return 0;
    clearerr(f->handle);
    size_t n = fwrite(buf->data, 1, (size_t)buf->len, f->handle);
    if (n == 0 && ferror(f->handle)) {
        int e = errno;
        if (e == 0) e = 5; // EIO
        return -(int64_t)e;
    }
    return (int64_t)n;
}

// ── File byte-oriented reader ───────────────────────────────
//
// Reads up to `max_bytes` bytes from the file and appends them to
// `buf`. Returns the number of bytes actually read (>= 0), or a
// negative errno on failure. A return of 0 means EOF.
static inline int64_t gorget_file_read_bytes_buf(GorgetFile* f, GorgetArray* buf, int64_t max_bytes) {
    if (!f || !f->handle || !buf || max_bytes <= 0) return 0;
    size_t old_len = (size_t)buf->len;
    gorget_array_ensure_capacity(buf, old_len + (size_t)max_bytes, 1);
    clearerr(f->handle);
    size_t n = fread((uint8_t*)buf->data + old_len, 1, (size_t)max_bytes, f->handle);
    if (n == 0) {
        if (ferror(f->handle)) {
            int e = errno;
            if (e == 0) e = 5; // EIO
            return -(int64_t)e;
        }
        return 0; // EOF
    }
    buf->len = (int64_t)(old_len + n);
    return (int64_t)n;
}

// ── stdout / stderr / stdin handles ─────────────────────────
//
// Return a non-owning GorgetFile that wraps the C stdio stream.
// `owned=false` means `gorget_file_close` will leave the underlying
// FILE* alone — we never fclose stdout/stderr/stdin.
static inline GorgetFile gorget_stdout_handle(void) {
    return (GorgetFile){stdout, false};
}

static inline GorgetFile gorget_stderr_handle(void) {
    return (GorgetFile){stderr, false};
}

static inline GorgetFile gorget_stdin_handle(void) {
    return (GorgetFile){stdin, false};
}

// ── File.open — typed Result[File, IoError] ─────────────────
//
// Opens a file and writes its handle into `*out`. Returns 0 on
// success, or a negative errno on failure. Gorget-side wrapper
// surfaces the result as `Result[File, IoError]`.
static inline int64_t gorget_file_open_try(const char* path, const char* mode, GorgetFile* out) {
    if (!out) return -22; // EINVAL
    out->handle = NULL;
    out->owned = false;
    if (!path || !mode) return -22;
    FILE* f = fopen(path, mode);
    if (!f) {
        int e = errno;
        if (e == 0) e = 5; // EIO
        return -(int64_t)e;
    }
    out->handle = f;
    out->owned = true;
    return 0;
}

// Free functions
static inline GorgetString gorget_read_file(const char* path) {
    GorgetFile f = gorget_file_open(path, "r");
    GorgetString content = gorget_file_read_all(&f);
    gorget_file_close(&f);
    return content;
}

static inline GorgetArray gorget_read_file_bytes(const char* path) {
    FILE* f = fopen(path, "rb");
    if (!f) {
        fprintf(stderr, "gorget: panic: read_file_bytes: cannot open '%s'\n", path);
        exit(1);
    }
    fseek(f, 0, SEEK_END);
    long len = ftell(f);
    fseek(f, 0, SEEK_SET);
    GorgetArray arr = gorget_array_new(1); // element_size = 1 (uint8)
    if (len > 0) {
        uint8_t* buf = (uint8_t*)GORGET_ALLOC((size_t)len);
        size_t read = fread(buf, 1, (size_t)len, f);
        arr.data = buf;
        arr.len = read;
        arr.cap = (size_t)len;
    }
    fclose(f);
    return arr;
}

static inline void gorget_write_file_bytes(const char* path, const GorgetArray* data) {
    FILE* f = fopen(path, "wb");
    if (!f) {
        fprintf(stderr, "gorget: panic: write_file_bytes: cannot open '%s'\n", path);
        exit(1);
    }
    if (data && data->data && data->len > 0) {
        fwrite(data->data, 1, data->len, f);
    }
    fclose(f);
}

static inline void gorget_write_file(const char* path, const char* content) {
    GorgetFile f = gorget_file_open(path, "w");
    gorget_file_write(&f, content);
    gorget_file_close(&f);
}

static inline void gorget_append_file(const char* path, const char* content) {
    GorgetFile f = gorget_file_open(path, "a");
    gorget_file_write(&f, content);
    gorget_file_close(&f);
}

static inline bool gorget_file_exists(const char* path) {
    FILE* f = fopen(path, "r");
    if (f) { fclose(f); return true; }
    return false;
}

static inline bool gorget_delete_file(const char* path) {
    return remove(path) == 0;
}

static inline bool gorget_mkdir(const char* path) {
    return mkdir(path, 0755) == 0;
}

static inline bool gorget_rmdir(const char* path) {
    return rmdir(path) == 0;
}

static inline bool gorget_rename(const char* old_path, const char* new_path) {
    return rename(old_path, new_path) == 0;
}

static inline bool gorget_copy_file(const char* src, const char* dst) {
    FILE* in = fopen(src, "rb");
    if (!in) return false;
    FILE* out = fopen(dst, "wb");
    if (!out) { fclose(in); return false; }
    char buf[4096];
    size_t n;
    while ((n = fread(buf, 1, sizeof(buf), in)) > 0) {
        if (fwrite(buf, 1, n, out) != n) { fclose(in); fclose(out); return false; }
    }
    fclose(in);
    fclose(out);
    return true;
}

static inline int64_t gorget_file_size(const char* path) {
    struct stat st;
    if (stat(path, &st) != 0) return -1;
    return (int64_t)st.st_size;
}

static inline bool gorget_is_dir(const char* path) {
    struct stat st;
    if (stat(path, &st) != 0) return false;
    return S_ISDIR(st.st_mode);
}

