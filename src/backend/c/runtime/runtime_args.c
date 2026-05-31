
// ── CLI args (gorget_args) ──────────────────────────────────
static inline GorgetArray gorget_args(void) {
    GorgetArray arr = gorget_array_new(sizeof(Str));
    arr.elem_drop = (__gorget_drop_fn)gorget_string_free;
    arr.elem_clone = (__gorget_drop_fn)gorget_string_clone_inplace;
    for (int i = 0; i < gorget_argc; i++) {
        Str s = gorget_str_from_cstr(gorget_argv[i]);
        gorget_array_push(&arr, &s);
    }
    return arr;
}

