
// ── Path functions ───────────────────────────────────────────
static inline const char* gorget_path_parent(const char* path) {
    if (path == NULL || path[0] == '\0') { char* r = (char*)GORGET_ALLOC(2); r[0]='.'; r[1]='\0'; return r; }
    size_t len = strlen(path);
    // Strip trailing slashes
    while (len > 1 && path[len - 1] == '/') len--;
    // Find last slash
    size_t i = len;
    while (i > 0 && path[i - 1] != '/') i--;
    if (i == 0) { char* r = (char*)GORGET_ALLOC(2); r[0]='.'; r[1]='\0'; return r; }
    if (i == 1) { char* r = (char*)GORGET_ALLOC(2); r[0]='/'; r[1]='\0'; return r; }
    // Return prefix up to (but not including) the trailing slash
    char* result = (char*)GORGET_ALLOC(i);
    memcpy(result, path, i - 1);
    result[i - 1] = '\0';
    return result;
}

static inline const char* gorget_path_basename(const char* path) {
    if (path == NULL || path[0] == '\0') { char* r = (char*)GORGET_ALLOC(1); r[0]='\0'; return r; }
    size_t len = strlen(path);
    // Strip trailing slashes
    while (len > 1 && path[len - 1] == '/') len--;
    // Find last slash
    size_t i = len;
    while (i > 0 && path[i - 1] != '/') i--;
    size_t blen = len - i;
    char* result = (char*)GORGET_ALLOC(blen + 1);
    memcpy(result, path + i, blen);
    result[blen] = '\0';
    return result;
}

static inline const char* gorget_path_extension(const char* path) {
    char* base = (char*)gorget_path_basename(path);
    size_t blen = strlen(base);
    if (blen == 0) { GORGET_FREE(base, blen+1); char* r = (char*)GORGET_ALLOC(1); r[0]='\0'; return r; }
    // Find last dot, skipping leading dot for hidden files
    size_t start = (base[0] == '.') ? 1 : 0;
    const char* dot = NULL;
    for (size_t i = start; i < blen; i++) {
        if (base[i] == '.') dot = base + i;
    }
    if (dot == NULL) { GORGET_FREE(base, blen+1); char* r = (char*)GORGET_ALLOC(1); r[0]='\0'; return r; }
    // Return everything after the dot (not including the dot)
    size_t elen = blen - (size_t)(dot + 1 - base);
    char* result = (char*)GORGET_ALLOC(elen + 1);
    memcpy(result, dot + 1, elen);
    result[elen] = '\0';
    GORGET_FREE(base, blen+1);
    return result;
}

static inline const char* gorget_path_stem(const char* path) {
    char* base = (char*)gorget_path_basename(path);
    size_t blen = strlen(base);
    if (blen == 0) { GORGET_FREE(base, 1); char* r = (char*)GORGET_ALLOC(1); r[0]='\0'; return r; }
    // Find last dot, skipping leading dot for hidden files
    size_t start = (base[0] == '.') ? 1 : 0;
    const char* dot = NULL;
    for (size_t i = start; i < blen; i++) {
        if (base[i] == '.') dot = base + i;
    }
    if (dot == NULL) return base;  // transfer ownership
    size_t slen = (size_t)(dot - base);
    char* result = (char*)GORGET_ALLOC(slen + 1);
    memcpy(result, base, slen);
    result[slen] = '\0';
    GORGET_FREE(base, blen+1);
    return result;
}

static inline const char* gorget_path_join(const char* a, const char* b) {
    if (a == NULL || a[0] == '\0') { const char* s = b ? b : ""; size_t l = strlen(s); char* r = (char*)GORGET_ALLOC(l+1); memcpy(r,s,l+1); return r; }
    if (b == NULL || b[0] == '\0') { size_t l = strlen(a); char* r = (char*)GORGET_ALLOC(l+1); memcpy(r,a,l+1); return r; }
    size_t alen = strlen(a);
    size_t blen = strlen(b);
    // Strip trailing slash from a
    while (alen > 1 && a[alen - 1] == '/') alen--;
    // Strip leading slash from b
    size_t bstart = 0;
    while (bstart < blen && b[bstart] == '/') bstart++;
    size_t rlen = alen + 1 + (blen - bstart);
    char* result = (char*)GORGET_ALLOC(rlen + 1);
    memcpy(result, a, alen);
    result[alen] = '/';
    memcpy(result + alen + 1, b + bstart, blen - bstart);
    result[rlen] = '\0';
    return result;
}

static inline const char* gorget_path_normalize(const char* path) {
    if (path == NULL || path[0] == '\0') { char* r = (char*)GORGET_ALLOC(2); r[0]='.'; r[1]='\0'; return r; }
    int absolute = (path[0] == '/');
    size_t len = strlen(path);
    // Copy path and replace '/' with '\0' so components become null-terminated strings in place
    char* tmp = (char*)GORGET_ALLOC(len + 1);
    memcpy(tmp, path, len + 1);
    for (size_t i = 0; i < len; i++) {
        if (tmp[i] == '/') tmp[i] = '\0';
    }
    // Walk null-terminated component tokens, build a stack of kept components
    // Max components bounded by len
    const char** stack = (const char**)GORGET_ALLOC((len + 2) * sizeof(const char*));
    size_t depth = 0;
    char* p = tmp;
    char* end_p = tmp + len;
    while (p < end_p) {
        if (*p == '\0') { p++; continue; }
        size_t clen = strlen(p);
        if (clen == 1 && p[0] == '.') {
            // skip
        } else if (clen == 2 && p[0] == '.' && p[1] == '.') {
            if (depth > 0) depth--;
        } else {
            stack[depth++] = p;
        }
        p += clen + 1;
    }
    // Reconstruct
    if (depth == 0) {
        GORGET_FREE(stack, (len + 2) * sizeof(const char*));
        GORGET_FREE(tmp, len + 1);
        if (absolute) { char* r = (char*)GORGET_ALLOC(2); r[0]='/'; r[1]='\0'; return r; }
        char* r = (char*)GORGET_ALLOC(2); r[0]='.'; r[1]='\0'; return r;
    }
    // Compute output length: optional leading '/', components joined by '/'
    size_t outlen = (absolute ? 1 : 0) + (depth - 1);  // leading slash + separating slashes
    for (size_t i = 0; i < depth; i++) outlen += strlen(stack[i]);
    char* result = (char*)GORGET_ALLOC(outlen + 1);
    size_t pos = 0;
    if (absolute) result[pos++] = '/';
    for (size_t i = 0; i < depth; i++) {
        if (i > 0) result[pos++] = '/';
        size_t clen = strlen(stack[i]);
        memcpy(result + pos, stack[i], clen);
        pos += clen;
    }
    result[pos] = '\0';
    GORGET_FREE(stack, (len + 2) * sizeof(const char*));
    GORGET_FREE(tmp, len + 1);
    return result;
}

static inline const char* gorget_path_absolute(const char* path) {
    if (path == NULL || path[0] == '\0') {
        char cwd[4096]; if (!getcwd(cwd, sizeof(cwd))) cwd[0]='\0';
        size_t l = strlen(cwd); char* r = (char*)GORGET_ALLOC(l+1); memcpy(r,cwd,l+1); return r;
    }
    if (path[0] == '/') return gorget_path_normalize(path);
    char cwd[4096]; if (!getcwd(cwd, sizeof(cwd))) cwd[0]='\0';
    size_t cwdlen = strlen(cwd);
    size_t pathlen = strlen(path);
    // cwd + '/' + path
    char* combined = (char*)GORGET_ALLOC(cwdlen + 1 + pathlen + 1);
    memcpy(combined, cwd, cwdlen);
    combined[cwdlen] = '/';
    memcpy(combined + cwdlen + 1, path, pathlen + 1);
    const char* result = gorget_path_normalize(combined);
    GORGET_FREE(combined, cwdlen + 1 + pathlen + 1);
    return result;
}

// ── readdir ─────────────────────────────────────────────────
static inline GorgetArray gorget_readdir(const char* path) {
    GorgetArray arr = gorget_array_new(sizeof(Str));
    arr.elem_drop = (__gorget_drop_fn)gorget_string_free;
    arr.elem_clone = (__gorget_drop_fn)gorget_string_clone_inplace;
    DIR* d = opendir(path);
    if (!d) { fprintf(stderr, "Error: cannot open directory '%s'\n", path); exit(1); }
    struct dirent* ent;
    while ((ent = readdir(d)) != NULL) {
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0) continue;
        size_t nlen = strlen(ent->d_name);
        char* name = (char*)GORGET_ALLOC(nlen + 1);
        memcpy(name, ent->d_name, nlen + 1);
        Str s = gorget_str_own_region(name, nlen);
        gorget_array_push(&arr, &s);
    }
    closedir(d);
    return arr;
}

