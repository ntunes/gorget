
// ── Gorget Trace Runtime ──────────────────────────────────────
static FILE* __gorget_trace_fp = NULL;
static int __gorget_trace_depth = 0;

static void __gorget_trace_close(void) {
    if (__gorget_trace_fp && __gorget_trace_fp != stderr) fclose(__gorget_trace_fp);
    __gorget_trace_fp = NULL;
}

static void __gorget_trace_init(const char* path) {
    __gorget_trace_fp = fopen(path, "w");
    if (!__gorget_trace_fp) __gorget_trace_fp = stderr;
    atexit(__gorget_trace_close);
}

static void __gorget_trace_json_str(FILE* fp, const char* s) {
    while (*s) {
        switch (*s) {
            case '"':  fputs("\\\"", fp); break;
            case '\\': fputs("\\\\", fp); break;
            case '\n': fputs("\\n", fp); break;
            default:   fputc(*s, fp);
        }
        s++;
    }
}

static void __gorget_trace_val_int(FILE* fp, int64_t v) { fprintf(fp, "%" PRId64, v); }
static void __gorget_trace_val_float(FILE* fp, double v) { fprintf(fp, "%g", v); }
static void __gorget_trace_val_bool(FILE* fp, bool v) { fprintf(fp, v ? "true" : "false"); }
static void __gorget_trace_val_str(FILE* fp, const char* v) {
    fputc('"', fp);
    if (v) {
        for (const char* p = v; *p; p++) {
            if (*p == '"') fputs("\\\"", fp);
            else if (*p == '\\') fputs("\\\\", fp);
            else if (*p == '\n') fputs("\\n", fp);
            else if (*p == '\t') fputs("\\t", fp);
            else fputc(*p, fp);
        }
    }
    fputc('"', fp);
}
static void __gorget_trace_val_Str(FILE* fp, Str v) { __gorget_trace_val_str(fp, v.data ? (const char*)v.data : ""); }
static void __gorget_trace_val_char(FILE* fp, char v) { fprintf(fp, "'%c'", v); }
static void __gorget_trace_val_void(FILE* fp) { (void)fp; }

// ── Envelope emitters ─────────────────────────────────────────
// Used by the LLVM backend in lieu of inline fprintf — keeps the
// generated LLVM IR small. The C backend emits the same JSON shape
// directly and doesn't call these helpers; both paths keep depth in
// sync because they share `__gorget_trace_depth`.
static void __gorget_trace_emit_stmt_start(void) {
    if (__gorget_trace_fp) {
        fprintf(__gorget_trace_fp, "{\"type\":\"stmt_start\",\"depth\":%d}\n", __gorget_trace_depth++);
    }
}
static void __gorget_trace_emit_stmt_end(void) {
    if (__gorget_trace_fp) {
        fprintf(__gorget_trace_fp, "{\"type\":\"stmt_end\",\"depth\":%d}\n", --__gorget_trace_depth);
    }
}
static void __gorget_trace_emit_branch(void) {
    if (__gorget_trace_fp) {
        fprintf(__gorget_trace_fp, "{\"type\":\"branch\",\"depth\":%d}\n", __gorget_trace_depth);
    }
}
static void __gorget_trace_emit_call_begin(const char* fn_name) {
    if (__gorget_trace_fp) {
        fprintf(__gorget_trace_fp, "{\"type\":\"call\",\"fn\":\"%s\",\"args\":{", fn_name);
    }
}
// Emits ',"name":' (or '"name":' when first==1).
static void __gorget_trace_emit_arg_name(const char* name, int first) {
    if (__gorget_trace_fp) {
        fprintf(__gorget_trace_fp, "%s\"%s\":", first ? "" : ",", name);
    }
}
// Bundled arg-name + value emitters — keeps the LLVM-side IR small (one
// call per arg vs. name-then-value pair). Format:
//   first  ? '"name":<value>'
//   else  ',"name":<value>'
static void __gorget_trace_emit_arg_int(const char* name, int first, int64_t val) {
    if (__gorget_trace_fp) {
        __gorget_trace_emit_arg_name(name, first);
        __gorget_trace_val_int(__gorget_trace_fp, val);
    }
}
static void __gorget_trace_emit_arg_float(const char* name, int first, double val) {
    if (__gorget_trace_fp) {
        __gorget_trace_emit_arg_name(name, first);
        __gorget_trace_val_float(__gorget_trace_fp, val);
    }
}
static void __gorget_trace_emit_arg_bool(const char* name, int first, int val) {
    if (__gorget_trace_fp) {
        __gorget_trace_emit_arg_name(name, first);
        __gorget_trace_val_bool(__gorget_trace_fp, val ? true : false);
    }
}
static void __gorget_trace_emit_arg_str(const char* name, int first, const Str* val) {
    if (__gorget_trace_fp) {
        __gorget_trace_emit_arg_name(name, first);
        __gorget_trace_val_Str(__gorget_trace_fp, val ? *val : (Str){0});
    }
}
static void __gorget_trace_emit_call_end(void) {
    if (__gorget_trace_fp) {
        fprintf(__gorget_trace_fp, "},\"depth\":%d}\n", __gorget_trace_depth++);
    }
}
static void __gorget_trace_emit_return(const char* fn_name) {
    if (__gorget_trace_fp) {
        fprintf(__gorget_trace_fp, "{\"type\":\"return\",\"fn\":\"%s\",\"depth\":%d}\n", fn_name, __gorget_trace_depth--);
    }
}

