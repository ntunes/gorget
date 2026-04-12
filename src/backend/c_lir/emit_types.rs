//! Type definition emission, drop/clone functions, higher-order collection helpers,
//! Option/Result combinator helpers, spawn/thread helpers, and runtime module selection.

use super::*;

/// Higher-order collection methods that the old C backend generates inline.
pub(super) const HIGHER_ORDER_METHODS: &[&str] = &[
    "filter", "map", "flat_map", "fold", "reduce", "any", "all",
    "each", "find", "find_index", "sorted", "sort", "unique", "count",
];

/// Dict/Set methods needing inline codegen (no corresponding runtime function).
pub(super) const DICT_INLINE_METHODS: &[&str] = &[
    "filter", "fold", "each", "any", "all", "map", "update", "get_or", "get_or_put",
];
pub(super) const SET_INLINE_METHODS: &[&str] = &[
    "filter", "fold", "each", "any", "all", "map", "is_subset", "is_superset",
    "union", "intersection", "difference", "symmetric_difference",
];

/// Parse a monomorphized name like `Dict__Str__int64_t__filter` into
/// (key_c_type, val_c_type, method_name). Returns None if not a dict inline op.
pub(super) fn parse_dict_higher_order(name: &str) -> Option<(&str, &str, &str)> {
    let rest = name.strip_prefix("Dict__")
        .or_else(|| name.strip_prefix("HashMap__"))?;
    let sep_pos = rest.rfind("__")?;
    let method = &rest[sep_pos + 2..];
    if !DICT_INLINE_METHODS.contains(&method) {
        return None;
    }
    // Remaining type part: "GorgetString__int64_t" → key="GorgetString", val="int64_t"
    let type_part = &rest[..sep_pos];
    // Find the FIRST `__` to split key from value type.
    let key_sep = type_part.find("__")?;
    let key = &type_part[..key_sep];
    let val = &type_part[key_sep + 2..];
    Some((key, val, method))
}

/// Parse a monomorphized name like `Set__int64_t__filter` into
/// (elem_c_type, method_name). Returns None if not a set inline op.
pub(super) fn parse_set_higher_order(name: &str) -> Option<(&str, &str)> {
    let rest = name.strip_prefix("Set__")
        .or_else(|| name.strip_prefix("HashSet__"))?;
    let sep_pos = rest.rfind("__")?;
    let method = &rest[sep_pos + 2..];
    if !SET_INLINE_METHODS.contains(&method) {
        return None;
    }
    let elem = &rest[..sep_pos];
    Some((elem, method))
}

/// Parse a monomorphized name like `Vector__int64_t__filter` into
/// (element_c_type, method_name). Returns None if not a higher-order op.
pub(super) fn parse_vector_higher_order(name: &str) -> Option<(&str, &str)> {
    // Pattern: Vector__<elem_type>__<method>
    let rest = name.strip_prefix("Vector__")?;
    // Find the LAST `__` separator — method name is after it.
    let sep_pos = rest.rfind("__")?;
    let method = &rest[sep_pos + 2..];
    if !HIGHER_ORDER_METHODS.contains(&method) {
        return None;
    }
    let elem = &rest[..sep_pos];
    Some((elem, method))
}
/// Collection helper descriptor — Vector, Dict, or Set.
pub(super) enum CollHelper {
    /// (full_name, elem_c, method, closure_ty, call_fn)
    Vector(String, String, String, String, String),
    /// (full_name, key_c, val_c, method, closure_ty, call_fn)
    Dict(String, String, String, String, String, String),
    /// (full_name, elem_c, method, closure_ty, call_fn)
    Set(String, String, String, String, String),
}
/// Generate static inline C helper functions for higher-order collection operations.
/// Scans all CallExtern instructions for `Vector__T__method`, `Dict__K__V__method`,
/// and `Set__T__method` patterns and generates type-specific inline implementations.
pub(super) fn emit_higher_order_collection_helpers(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    let mut helpers: Vec<CollHelper> = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();

    // Build orig name → C name map for resolving element types like Option__int64_t → __lir_s11
    let orig_to_c: HashMap<String, String> = module.structs.iter().enumerate()
        .map(|(i, def)| (def.name.clone(), sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"))))
        .collect();

    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::CallExtern { name, .. } = inst {
                    if !seen.insert(name.clone()) { continue; }
                    let ext = module.externs.iter().find(|e| e.name == *name);
                    let closure_c_type = ext.and_then(|e| e.params.last())
                        .map(|t| c_type_named(t, sn)).unwrap_or_else(|| "void*".into());
                    let call_fn_name = find_closure_call_fn(module, &closure_c_type, sn);

                    if let Some((elem_ty, method)) = parse_vector_higher_order(name) {
                        helpers.push(CollHelper::Vector(
                            name.clone(), elem_type_to_c_with_sn(elem_ty, &orig_to_c), method.to_string(),
                            closure_c_type, call_fn_name,
                        ));
                    } else if let Some((key_ty, val_ty, method)) = parse_dict_higher_order(name) {
                        helpers.push(CollHelper::Dict(
                            name.clone(), elem_type_to_c_with_sn(key_ty, &orig_to_c), elem_type_to_c_with_sn(val_ty, &orig_to_c),
                            method.to_string(), closure_c_type, call_fn_name,
                        ));
                    } else if let Some((elem_ty, method)) = parse_set_higher_order(name) {
                        helpers.push(CollHelper::Set(
                            name.clone(), elem_type_to_c_with_sn(elem_ty, &orig_to_c), method.to_string(),
                            closure_c_type, call_fn_name,
                        ));
                    } else {
                        // Not a collection higher-order op — undo insertion
                        seen.remove(name.as_str());
                    }
                }
            }
        }
    }

    if helpers.is_empty() {
        return;
    }

    writeln!(out, "/* ── Higher-order collection helpers ── */").unwrap();
    for helper in &helpers {
        match helper {
            CollHelper::Vector(full_name, elem_c, method, closure_ty, call_fn) => {
                emit_vector_helper(out, full_name, elem_c, method, closure_ty, call_fn, module, sn);
            }
            CollHelper::Dict(full_name, key_c, val_c, method, closure_ty, call_fn) => {
                emit_dict_helper(out, full_name, key_c, val_c, method, closure_ty, call_fn, module);
            }
            CollHelper::Set(full_name, elem_c, method, closure_ty, call_fn) => {
                emit_set_helper(out, full_name, elem_c, method, closure_ty, call_fn, module);
            }
        }
        writeln!(out).unwrap();
    }
}

/// Resolve the accumulator type for a fold by looking up the closure's return type.
fn resolve_fold_acc_type(call_fn: &str, module: &LirModule, sn: &HashMap<u32, String>) -> Option<String> {
    closure_call_return_type(module, call_fn, sn)
}

pub(super) fn emit_vector_helper(out: &mut String, full_name: &str, elem_c: &str, method: &str, closure_ty: &str, call_fn: &str, module: &LirModule, sn: &HashMap<u32, String>) {
    // Skip closure-dependent helpers when we can't resolve the closure call function.
    // Methods that don't use closures (sort, sorted, unique, count) always emit.
    let closure_free = matches!(method, "sort" | "sorted" | "unique" | "count");
    if !closure_free && call_fn.contains("UNKNOWN_CLOSURE_CALL") {
        return;
    }
    // Determine which closure params need & prefix (Ptr ABI for resource types)
    let needs_ref = closure_params_need_ref(module, call_fn);
    let er = if needs_ref.first().copied().unwrap_or(false) { "&" } else { "" };
    match method {
        "filter" => {
            writeln!(out, "static inline GorgetArray {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    GorgetArray __result = gorget_array_new(sizeof({elem_c}));").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, {er}__elem)) gorget_array_push(&__result, &__elem);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "map" => {
            writeln!(out, "static inline GorgetArray {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    __typeof__({call_fn}(&__fn, {er}({elem_c}){{0}})) __map_out;").unwrap();
            writeln!(out, "    GorgetArray __result = gorget_array_new(sizeof(__map_out));").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        __map_out = {call_fn}(&__fn, {er}__elem);").unwrap();
            writeln!(out, "        gorget_array_push(&__result, &__map_out);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "fold" => {
            // Use the closure's return type for the accumulator — resolve from closure call fn.
            // Falls back to int64_t for cross-type folds where closure returns a different type.
            let acc_c = resolve_fold_acc_type(call_fn, module, sn).unwrap_or_else(|| "int64_t".into());
            let ar = if needs_ref.first().copied().unwrap_or(false) { "&" } else { "" };
            let er2 = if needs_ref.get(1).copied().unwrap_or(false) { "&" } else { "" };
            writeln!(out, "static inline {acc_c} {full_name}(void* __arr_ptr, {acc_c} __acc_init, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    {acc_c} __acc = __acc_init;").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        __acc = {call_fn}(&__fn, {ar}__acc, {er2}__elem);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __acc;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "any" => {
            writeln!(out, "static inline bool {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, {er}__elem)) return true;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return false;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "all" => {
            writeln!(out, "static inline bool {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        if (!{call_fn}(&__fn, {er}__elem)) return false;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return true;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "each" => {
            writeln!(out, "static inline void {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        {call_fn}(&__fn, {er}__elem);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "reduce" => {
            let ar = if needs_ref.first().copied().unwrap_or(false) { "&" } else { "" };
            let er2 = if needs_ref.get(1).copied().unwrap_or(false) { "&" } else { "" };
            writeln!(out, "static inline {elem_c} {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    {elem_c} __acc = GORGET_ARRAY_AT({elem_c}, __src, 0);").unwrap();
            writeln!(out, "    for (size_t __i = 1; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        __acc = {call_fn}(&__fn, {ar}__acc, {er2}__elem);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __acc;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "sorted" => {
            // sorted() → clone + qsort with type-specific compare
            let cmp = compare_fn_for_elem(elem_c);
            writeln!(out, "static inline GorgetArray {full_name}(void* __arr_ptr) {{").unwrap();
            writeln!(out, "    GorgetArray __result = gorget_array_clone((GorgetArray*)__arr_ptr);").unwrap();
            writeln!(out, "    qsort(__result.data, __result.len, __result.elem_size, {cmp});").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "sort" => {
            // sort() → in-place qsort with type-specific compare
            let cmp = compare_fn_for_elem(elem_c);
            writeln!(out, "static inline void {full_name}(void* __arr_ptr) {{").unwrap();
            writeln!(out, "    GorgetArray* __a = (GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    qsort(__a->data, __a->len, __a->elem_size, {cmp});").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "unique" => {
            // unique() → clone + sort + dedup with type-specific compare
            let cmp = compare_fn_for_elem(elem_c);
            writeln!(out, "static inline GorgetArray {full_name}(void* __arr_ptr) {{").unwrap();
            writeln!(out, "    GorgetArray __result = gorget_array_clone((GorgetArray*)__arr_ptr);").unwrap();
            writeln!(out, "    qsort(__result.data, __result.len, __result.elem_size, {cmp});").unwrap();
            writeln!(out, "    gorget_array_dedup(&__result);").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "find" => {
            // find(pred) → Option[T]  (returns first matching element)
            writeln!(out, "static inline void {full_name}(void* __arr_ptr, {closure_ty} __fn, void* __out) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    size_t __payload_off = (sizeof(int32_t) + (_Alignof({elem_c}) - 1)) & ~(_Alignof({elem_c}) - 1);").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, {er}__elem)) {{ *(int32_t*)__out = 0; memcpy((char*)__out + __payload_off, &__elem, sizeof({elem_c})); return; }}").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    *(int32_t*)__out = 1;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "find_index" => {
            // find_index(pred) → int64_t (-1 if not found)
            writeln!(out, "static inline int64_t {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, {er}__elem)) return (int64_t)__i;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return -1LL;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "flat_map" => {
            // flat_map(fn(T) → GorgetArray) → GorgetArray
            writeln!(out, "static inline GorgetArray {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    GorgetArray __result = gorget_array_new(sizeof({elem_c}));").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        GorgetArray __sub = {call_fn}(&__fn, {er}__elem);").unwrap();
            writeln!(out, "        gorget_array_extend(&__result, &__sub);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "count" => {
            writeln!(out, "static inline int64_t {full_name}(void* __arr_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetArray __src = *(GorgetArray*)__arr_ptr;").unwrap();
            writeln!(out, "    int64_t __count = 0;").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __src.len; __i++) {{").unwrap();
            writeln!(out, "        {elem_c} __elem = GORGET_ARRAY_AT({elem_c}, __src, __i);").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, {er}__elem)) __count++;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __count;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        _ => {
            writeln!(out, "// TODO: {full_name} not yet implemented in c_lir").unwrap();
        }
    }
}

/// Emit inline C helpers for Dict higher-order and inline methods.
pub(super) fn emit_dict_helper(out: &mut String, full_name: &str, key_c: &str, val_c: &str, method: &str, closure_ty: &str, call_fn: &str, module: &LirModule) {
    let iter_loop = format!(
        "for (size_t __i = 0; __i < __src.cap; __i++) {{ \
        if (__src.states[__i] != 1) continue;"
    );
    let key_read = format!("{key_c} __key = *({key_c}*)((char*)__src.keys + __i * __src.key_size);");
    let val_read = format!("{val_c} __val = *({val_c}*)((char*)__src.values + __i * __src.val_size);");
    // Dict uses ordered gorget_dict_new; if full_name starts with Dict__ or HashMap__ determines prefix
    let is_dict = full_name.starts_with("Dict__");
    let ctor_fn = if key_c == "Str" {
        if is_dict { "gorget_dict_new_str" } else { "gorget_map_new_str" }
    } else {
        if is_dict { "gorget_dict_new" } else { "gorget_map_new" }
    };
    let ctor_args = if key_c == "Str" { format!("sizeof({val_c})") } else { format!("sizeof({key_c}), sizeof({val_c})") };

    // Determine which closure params need & prefix (Ptr ABI for resource types)
    let needs_ref = closure_params_need_ref(module, call_fn);
    let kr = if needs_ref.first().copied().unwrap_or(false) { "&" } else { "" };
    let vr = if needs_ref.get(1).copied().unwrap_or(false) { "&" } else { "" };
    match method {
        "filter" => {
            // filter(closure(K, V) → bool) → GorgetMap.
            // __key/__val shallow-copy slots of __src → use put_cloned to avoid aliasing.
            writeln!(out, "static inline GorgetMap {full_name}(void* __map_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetMap __src = *(GorgetMap*)__map_ptr;").unwrap();
            writeln!(out, "    GorgetMap __result = {ctor_fn}({ctor_args});").unwrap();
            if val_c == "Str" || val_c == "GorgetString" {
                writeln!(out, "    __result.val_drop = (__gorget_drop_fn)gorget_string_free;").unwrap();
                writeln!(out, "    __result.val_clone = (__gorget_drop_fn)gorget_string_clone_inplace;").unwrap();
                writeln!(out, "    __result.val_materialize = (__gorget_drop_fn)gorget_string_materialize_inplace;").unwrap();
            }
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {key_read}").unwrap();
            writeln!(out, "        {val_read}").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, {kr}__key, {vr}__val)) gorget_map_put_cloned(&__result, &__key, &__val);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "fold" => {
            // fold(init, closure(acc, K, V) → acc) → acc_type
            let ar = if needs_ref.first().copied().unwrap_or(false) { "&" } else { "" };
            let kr2 = if needs_ref.get(1).copied().unwrap_or(false) { "&" } else { "" };
            let vr2 = if needs_ref.get(2).copied().unwrap_or(false) { "&" } else { "" };
            writeln!(out, "static inline int64_t {full_name}(void* __map_ptr, int64_t __acc, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetMap __src = *(GorgetMap*)__map_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {key_read}").unwrap();
            writeln!(out, "        {val_read}").unwrap();
            writeln!(out, "        __acc = {call_fn}(&__fn, {ar}__acc, {kr2}__key, {vr2}__val);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __acc;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "each" => {
            writeln!(out, "static inline void {full_name}(void* __map_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetMap __src = *(GorgetMap*)__map_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {key_read}").unwrap();
            writeln!(out, "        {val_read}").unwrap();
            writeln!(out, "        {call_fn}(&__fn, {kr}__key, {vr}__val);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "any" => {
            writeln!(out, "static inline bool {full_name}(void* __map_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetMap __src = *(GorgetMap*)__map_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {key_read}").unwrap();
            writeln!(out, "        {val_read}").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, {kr}__key, {vr}__val)) return true;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return false;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "all" => {
            writeln!(out, "static inline bool {full_name}(void* __map_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetMap __src = *(GorgetMap*)__map_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {key_read}").unwrap();
            writeln!(out, "        {val_read}").unwrap();
            writeln!(out, "        if (!{call_fn}(&__fn, {kr}__key, {vr}__val)) return false;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return true;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "update" => {
            // update(other_map): merge other dict entries into self.
            // __k/__v point into __other's storage — use put_cloned so the new slot
            // gets independent key/value copies via key_clone / val_clone.
            writeln!(out, "static inline void {full_name}(void* __map_ptr, GorgetMap __other) {{").unwrap();
            writeln!(out, "    GorgetMap* __dst = (GorgetMap*)__map_ptr;").unwrap();
            writeln!(out, "    for (size_t __i = 0; __i < __other.cap; __i++) {{").unwrap();
            writeln!(out, "        if (__other.states[__i] != 1) continue;").unwrap();
            writeln!(out, "        void* __k = (char*)__other.keys + __i * __other.key_size;").unwrap();
            writeln!(out, "        void* __v = (char*)__other.values + __i * __other.val_size;").unwrap();
            writeln!(out, "        gorget_map_put_cloned(__dst, __k, __v);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "get_or" => {
            // get_or(key, default) → val_type
            // For String values, clone to prevent aliasing the map's internal storage.
            // This matches dict.get() which clones String values into Option payloads.
            let val_is_str = val_c == "Str" || val_c == "GorgetString";
            let deref = if val_is_str { "gorget_string_clone_to_owned(__ptr)" } else { "*__ptr" };
            writeln!(out, "static inline {val_c} {full_name}(void* __map_ptr, {key_c} __key, {val_c} __default) {{").unwrap();
            writeln!(out, "    {val_c}* __ptr = ({val_c}*)gorget_map_get((GorgetMap*)__map_ptr, &__key);").unwrap();
            writeln!(out, "    return __ptr ? {deref} : __default;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "get_or_put" => {
            // get_or_put(key, default) → val_type — insert default if missing
            let val_is_str = val_c == "Str" || val_c == "GorgetString";
            let deref = if val_is_str { "gorget_string_clone_to_owned(__ptr)" } else { "*__ptr" };
            writeln!(out, "static inline {val_c} {full_name}(void* __map_ptr, {key_c} __key, {val_c} __default) {{").unwrap();
            writeln!(out, "    GorgetMap* __m = (GorgetMap*)__map_ptr;").unwrap();
            writeln!(out, "    {val_c}* __ptr = ({val_c}*)gorget_map_get(__m, &__key);").unwrap();
            writeln!(out, "    if (__ptr) return {deref};").unwrap();
            writeln!(out, "    gorget_map_put(__m, &__key, &__default);").unwrap();
            writeln!(out, "    return __default;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        _ => {
            writeln!(out, "// TODO: dict {full_name} not yet implemented in c_lir").unwrap();
        }
    }
}

/// Emit inline C helpers for Set higher-order and inline methods.
pub(super) fn emit_set_helper(out: &mut String, full_name: &str, elem_c: &str, method: &str, closure_ty: &str, call_fn: &str, module: &LirModule) {
    // Set__ uses insertion order (order array), HashSet__ uses bucket order
    let is_ordered = !full_name.starts_with("HashSet__");
    let iter_loop = if is_ordered {
        format!(
            "for (size_t __j = 0; __j < __src.order_len; __j++) {{ \
            size_t __i = __src.order[__j]; \
            if (__src.states[__i] != 1) continue;"
        )
    } else {
        format!(
            "for (size_t __i = 0; __i < __src.cap; __i++) {{ \
            if (__src.states[__i] != 1) continue;"
        )
    };
    // Use the _str variants when the element type is String so __result has
    // content-based hash/eq (otherwise pointer-compare would fail to dedupe
    // semantically equal strings from different allocations).
    let elem_is_str = elem_c == "Str" || elem_c == "GorgetString";
    let (ctor, ctor_args) = match (is_ordered, elem_is_str) {
        (true, true)  => ("gorget_ordered_set_new_str", String::new()),
        (true, false) => ("gorget_ordered_set_new", format!("sizeof({elem_c})")),
        (false, true) => ("gorget_set_new_str", String::new()),
        (false, false)=> ("gorget_set_new", format!("sizeof({elem_c})")),
    };
    let elem_read = format!("{elem_c} __elem = *({elem_c}*)((char*)__src.keys + __i * __src.key_size);");

    // Determine which closure params need & prefix (Ptr ABI for resource types)
    let needs_ref = closure_params_need_ref(module, call_fn);
    let er = if needs_ref.first().copied().unwrap_or(false) { "&" } else { "" };
    match method {
        "filter" => {
            writeln!(out, "static inline GorgetSet {full_name}(void* __set_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    GorgetSet __result = {ctor}({ctor_args});").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, {er}__elem)) gorget_map_put_cloned(&__result, &__elem, NULL);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "fold" => {
            let ar = if needs_ref.first().copied().unwrap_or(false) { "&" } else { "" };
            let er2 = if needs_ref.get(1).copied().unwrap_or(false) { "&" } else { "" };
            writeln!(out, "static inline int64_t {full_name}(void* __set_ptr, int64_t __acc, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        __acc = {call_fn}(&__fn, {ar}__acc, {er2}__elem);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __acc;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "each" => {
            writeln!(out, "static inline void {full_name}(void* __set_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        {call_fn}(&__fn, {er}__elem);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "any" => {
            writeln!(out, "static inline bool {full_name}(void* __set_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        if ({call_fn}(&__fn, {er}__elem)) return true;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return false;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "all" => {
            writeln!(out, "static inline bool {full_name}(void* __set_ptr, {closure_ty} __fn) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        if (!{call_fn}(&__fn, {er}__elem)) return false;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return true;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "is_subset" => {
            // is_subset(other): check every element in self is in other
            writeln!(out, "static inline bool {full_name}(void* __set_ptr, GorgetSet __other) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        if (!gorget_set_contains(&__other, &__elem)) return false;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return true;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "is_superset" => {
            // is_superset(other) = other.is_subset(self)
            writeln!(out, "static inline bool {full_name}(void* __set_ptr, GorgetSet __other) {{").unwrap();
            writeln!(out, "    GorgetSet __self = *(GorgetSet*)__set_ptr;").unwrap();
            if is_ordered {
                writeln!(out, "    for (size_t __j = 0; __j < __other.order_len; __j++) {{").unwrap();
                writeln!(out, "        size_t __i = __other.order[__j];").unwrap();
                writeln!(out, "        if (__other.states[__i] != 1) continue;").unwrap();
            } else {
                writeln!(out, "    for (size_t __i = 0; __i < __other.cap; __i++) {{").unwrap();
                writeln!(out, "        if (__other.states[__i] != 1) continue;").unwrap();
            }
            writeln!(out, "        {elem_c} __elem = *({elem_c}*)((char*)__other.keys + __i * __other.key_size);").unwrap();
            writeln!(out, "        if (!gorget_set_contains(&__self, &__elem)) return false;").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return true;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "union" => {
            // union: combine all elements from self and other
            writeln!(out, "static inline GorgetSet {full_name}(void* __set_ptr, GorgetSet __other) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    GorgetSet __result = {ctor}({ctor_args});").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        gorget_map_put_cloned(&__result, &__elem, NULL);").unwrap();
            writeln!(out, "    }}").unwrap();
            if is_ordered {
                writeln!(out, "    for (size_t __j2 = 0; __j2 < __other.order_len; __j2++) {{").unwrap();
                writeln!(out, "        size_t __i2 = __other.order[__j2];").unwrap();
                writeln!(out, "        if (__other.states[__i2] != 1) continue;").unwrap();
            } else {
                writeln!(out, "    for (size_t __i2 = 0; __i2 < __other.cap; __i2++) {{").unwrap();
                writeln!(out, "        if (__other.states[__i2] != 1) continue;").unwrap();
            }
            writeln!(out, "        {elem_c} __elem2 = *({elem_c}*)((char*)__other.keys + __i2 * __other.key_size);").unwrap();
            writeln!(out, "        gorget_map_put_cloned(&__result, &__elem2, NULL);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "intersection" => {
            // intersection: elements in both self and other
            writeln!(out, "static inline GorgetSet {full_name}(void* __set_ptr, GorgetSet __other) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    GorgetSet __result = {ctor}({ctor_args});").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        if (gorget_set_contains(&__other, &__elem)) gorget_map_put_cloned(&__result, &__elem, NULL);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "difference" => {
            // difference: elements in self but not in other
            writeln!(out, "static inline GorgetSet {full_name}(void* __set_ptr, GorgetSet __other) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    GorgetSet __result = {ctor}({ctor_args});").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        if (!gorget_set_contains(&__other, &__elem)) gorget_map_put_cloned(&__result, &__elem, NULL);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        "symmetric_difference" => {
            // symmetric_difference: elements in self xor other
            writeln!(out, "static inline GorgetSet {full_name}(void* __set_ptr, GorgetSet __other) {{").unwrap();
            writeln!(out, "    GorgetSet __src = *(GorgetSet*)__set_ptr;").unwrap();
            writeln!(out, "    GorgetSet __result = {ctor}({ctor_args});").unwrap();
            writeln!(out, "    {iter_loop}").unwrap();
            writeln!(out, "        {elem_read}").unwrap();
            writeln!(out, "        if (!gorget_set_contains(&__other, &__elem)) gorget_map_put_cloned(&__result, &__elem, NULL);").unwrap();
            writeln!(out, "    }}").unwrap();
            if is_ordered {
                writeln!(out, "    for (size_t __j2 = 0; __j2 < __other.order_len; __j2++) {{").unwrap();
                writeln!(out, "        size_t __i2 = __other.order[__j2];").unwrap();
                writeln!(out, "        if (__other.states[__i2] != 1) continue;").unwrap();
            } else {
                writeln!(out, "    for (size_t __i2 = 0; __i2 < __other.cap; __i2++) {{").unwrap();
                writeln!(out, "        if (__other.states[__i2] != 1) continue;").unwrap();
            }
            writeln!(out, "        {elem_c} __elem2 = *({elem_c}*)((char*)__other.keys + __i2 * __other.key_size);").unwrap();
            writeln!(out, "        if (!gorget_set_contains(&__src, &__elem2)) gorget_map_put_cloned(&__result, &__elem2, NULL);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return __result;").unwrap();
            writeln!(out, "}}").unwrap();
        }
        _ => {
            writeln!(out, "// TODO: set {full_name} not yet implemented in c_lir").unwrap();
        }
    }
}
/// Find the __call function name for a closure struct type.
pub(super) fn find_closure_call_fn(module: &LirModule, struct_c_name: &str, sn: &HashMap<u32, String>) -> String {
    // Map c_name back to struct def to get the original name (e.g., "__Closure_0").
    for (i, def) in module.structs.iter().enumerate() {
        let c_name = sn.get(&(i as u32)).map(|s| s.as_str()).unwrap_or(&def.name);
        if c_name == struct_c_name {
            // Look for a function named `<original_name>__call`
            let call_name = format!("{}__call", def.name);
            if module.functions.iter().any(|f| f.name == call_name) {
                return call_name;
            }
        }
    }
    // Fallback: try interpreting struct_c_name as the original name directly.
    let call_name = format!("{struct_c_name}__call");
    if module.functions.iter().any(|f| f.name == call_name) {
        return call_name;
    }
    // Last resort: return a placeholder
    format!("/* UNKNOWN_CLOSURE_CALL for {struct_c_name} */")
}

/// Look up the return type of a closure's `__call` function in LIR.
pub(super) fn closure_call_return_type(module: &LirModule, call_fn_name: &str, sn: &HashMap<u32, String>) -> Option<String> {
    module.functions.iter()
        .find(|f| f.name == call_fn_name)
        .map(|f| c_type_named(&f.return_type, sn))
}

/// Check which closure params (skipping env pointer) are passed by pointer.
/// Returns a vec of bools — true means the template should use `&` prefix for that arg.
pub(super) fn closure_params_need_ref(module: &LirModule, call_fn: &str) -> Vec<bool> {
    if let Some(func) = module.functions.iter().find(|f| f.name == call_fn) {
        // Params: [0]=env_ptr, [1..]=closure params → skip env
        func.params.iter().skip(1)
            .map(|t| matches!(t, LirType::PtrTo(_) | LirType::Ptr))
            .collect()
    } else {
        Vec::new()
    }
}

/// For a `map` combinator, determine the source enum type and the result enum type.
/// The source type comes from the function name (e.g. `Option__int64_t__map` → `Option__int64_t`).
/// The result type is an Option/Result wrapping the closure's return type.
/// If the closure returns the same element type, source == result.
/// If different (cross-type map), find the matching Option__<ret_type> struct.
pub(super) fn map_combinator_types(
    name: &str, type_prefix: &str, call_fn: &str,
    module: &LirModule, sn: &HashMap<u32, String>,
) -> (String, String) {
    // Source type name = type_prefix (e.g., "Option__int64_t")
    let src_c = find_struct_c_name_by_prefix(type_prefix, module, sn)
        .unwrap_or_else(|| type_prefix.to_string());

    // Get the closure call function's return type.
    let closure_ret = closure_call_return_type(module, call_fn, sn);

    // If the closure returns the same type as the source payload, no cross-type.
    // If it returns a struct (Option/Result), the caller already does and_then, not map.
    // For map, the result wraps the closure return in the same Option/Result variant.
    if let Some(ref ret_ty) = closure_ret {
        // Extract what element type the source Option wraps.
        // E.g., "Option__int64_t" → payload is "int64_t"
        let src_payload = type_prefix.strip_prefix("Option__")
            .or_else(|| type_prefix.strip_prefix("Result__"));
        if let Some(payload) = src_payload {
            let payload_c = elem_type_to_c(payload);
            if *ret_ty != payload_c {
                // Cross-type: need Option__<ret_ty> or Result__<ret_ty> struct.
                let result_prefix = if name.starts_with("Option__") {
                    format!("Option__{}", type_name_to_monomorphized(ret_ty))
                } else {
                    // Result map keeps the error type; extract it from source struct
                    let err_part = module.structs.iter().find(|s| s.name == type_prefix)
                        .and_then(|s| s.fields.get(2))
                        .map(|(_, t)| c_type_named(t, sn));
                    if let Some(err_c) = err_part {
                        let err_m = type_name_to_monomorphized(&err_c);
                        format!("Result__{}__{err_m}", type_name_to_monomorphized(ret_ty))
                    } else {
                        format!("Result__{}", type_name_to_monomorphized(ret_ty))
                    }
                };
                let result_c = find_struct_c_name_by_prefix(&result_prefix, module, sn)
                    .unwrap_or(src_c.clone());
                return (src_c, result_c);
            }
        }
    }
    (src_c.clone(), src_c)
}

/// Compute source and result types for Result__T__E__map_err (closure transforms E → E2).
pub(super) fn map_err_combinator_types(
    _name: &str, type_prefix: &str, call_fn: &str,
    module: &LirModule, sn: &HashMap<u32, String>,
) -> (String, String) {
    let src_c = find_struct_c_name_by_prefix(type_prefix, module, sn)
        .unwrap_or_else(|| type_prefix.to_string());
    let closure_ret = closure_call_return_type(module, call_fn, sn);
    if let Some(ref ret_ty) = closure_ret {
        // Result__T__E → Ok type is field[1], Error type is field[2].
        let ok_c = module.structs.iter().find(|s| s.name == type_prefix)
            .and_then(|s| s.fields.get(1))
            .map(|(_, t)| c_type_named(t, sn));
        if let Some(ok_c) = ok_c {
            let ok_m = type_name_to_monomorphized(&ok_c);
            let result_prefix = format!("Result__{ok_m}__{}", type_name_to_monomorphized(ret_ty));
            let result_c = find_struct_c_name_by_prefix(&result_prefix, module, sn)
                .unwrap_or(src_c.clone());
            return (src_c, result_c);
        }
    }
    (src_c.clone(), src_c)
}

/// Compute source and result types for and_then (closure returns the full Result/Option).
pub(super) fn and_then_combinator_types(
    _name: &str, type_prefix: &str, call_fn: &str,
    module: &LirModule, sn: &HashMap<u32, String>,
) -> (String, String) {
    let src_c = find_struct_c_name_by_prefix(type_prefix, module, sn)
        .unwrap_or_else(|| type_prefix.to_string());
    let closure_ret = closure_call_return_type(module, call_fn, sn);
    if let Some(ref ret_ty) = closure_ret {
        // The closure returns the full wrapped type (e.g. Option__U, Result__U__E).
        let result_c = find_struct_c_name_by_prefix(ret_ty, module, sn)
            .unwrap_or(src_c.clone());
        return (src_c, result_c);
    }
    (src_c.clone(), src_c)
}

/// Find the C name for a struct whose original name matches a prefix.
pub(super) fn find_struct_c_name_by_prefix(prefix: &str, module: &LirModule, sn: &HashMap<u32, String>) -> Option<String> {
    for (i, def) in module.structs.iter().enumerate() {
        if def.name == prefix {
            return Some(sn.get(&(i as u32)).cloned().unwrap_or_else(|| def.name.clone()));
        }
    }
    None
}

/// Map a C type name back to its monomorphized form for struct lookup.
pub(super) fn type_name_to_monomorphized(c_type: &str) -> &str {
    // Normalize C type names to monomorphized struct names.
    match c_type {
        "Str" => "GorgetString",
        _ => c_type,
    }
}

/// Convert a monomorphized element type name to its C type.
/// Option/Result combinator methods that the old C backend generates inline.
pub(super) const OPTION_COMBINATORS: &[&str] = &[
    "map", "filter", "and_then", "or_else", "unwrap_or_else", "flat_map", "or", "flatten", "zip",
];
pub(super) const RESULT_COMBINATORS: &[&str] = &[
    "map", "map_err", "and_then", "or_else", "unwrap_err", "unwrap_error",
];

/// Parse an Option/Result combinator name like `Option__int64_t__map` or
/// Returns None if not a combinator.
pub(super) fn parse_option_result_combinator(name: &str) -> Option<(&str, &str)> {
    if name.starts_with("Option__") {
        let rest = name.strip_prefix("Option__")?;
        let sep_pos = rest.rfind("__")?;
        let method = &rest[sep_pos + 2..];
        if OPTION_COMBINATORS.contains(&method) || RESULT_COMBINATORS.contains(&method) {
            return Some((&name[..name.len() - method.len() - 2], method));
        }
    }
    if name.starts_with("Result__") {
        let rest = name.strip_prefix("Result__")?;
        let sep_pos = rest.rfind("__")?;
        let method = &rest[sep_pos + 2..];
        if RESULT_COMBINATORS.contains(&method) || OPTION_COMBINATORS.contains(&method) {
            return Some((&name[..name.len() - method.len() - 2], method));
        }
    }
    None
}

/// For Option/Result structs, get the field names for the payload arms.
/// Returns (ok_field, err_field) — for Option: ("Some_0", "None_0"), for Result: ("Ok_0", "Error_0").
/// Falls back to ("Some_0", "None_0") if not found.
pub(super) fn enum_payload_fields(type_prefix: &str, module: &LirModule) -> (String, String) {
    // Look up the struct definition by matching the type_prefix to a struct name
    for def in &module.structs {
        if def.name == type_prefix {
            // tag is field 0; payload field is field 1 (ok/some); error field is field 2 if present
            let ok_f = def.fields.get(1)
                .map(|(n, _)| c_field_name(n))
                .unwrap_or_else(|| "Some_0".to_string());
            let err_f = def.fields.get(2)
                .map(|(n, _)| c_field_name(n))
                .unwrap_or_else(|| "None_0".to_string());
            return (ok_f, err_f);
        }
    }
    ("Some_0".to_string(), "None_0".to_string())
}
/// Generate static inline C helpers for Option/Result combinator methods.
pub(super) fn emit_option_result_combinator_helpers(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    // (full_name, src_c_type, result_c_type, method, closure_c_type, call_fn, ok_field, err_field)
    let mut helpers: Vec<(String, String, String, String, String, String, String, String)> = Vec::new();

    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::CallExtern { name, .. } = inst {
                    if let Some((type_prefix, method)) = parse_option_result_combinator(name) {
                        if !seen.insert(name.clone()) {
                            continue;
                        }
                        let ext = module.externs.iter().find(|e| e.name == *name);
                        let closure_c_type = ext.and_then(|e| e.params.get(1))
                            .map(|t| c_type_named(t, sn))
                            .unwrap_or_else(|| "void*".into());
                        let closure_struct_name = closure_c_type.clone();
                        let call_fn = find_closure_call_fn(module, &closure_struct_name, sn);

                        let (ok_field, err_field) = enum_payload_fields(type_prefix, module);

                        // For map/map_err/and_then, source and result types may differ.
                        let (src_c, result_c) = if method == "map" {
                            map_combinator_types(name, type_prefix, &call_fn, module, sn)
                        } else if method == "map_err" {
                            map_err_combinator_types(name, type_prefix, &call_fn, module, sn)
                        } else if method == "and_then" {
                            and_then_combinator_types(name, type_prefix, &call_fn, module, sn)
                        } else if method == "flatten" {
                            // flatten: source is Option[Option[T]], result is Option[T]
                            let src = find_struct_c_name_by_prefix(type_prefix, module, sn)
                                .unwrap_or_else(|| type_prefix.to_string());
                            let inner = module.structs.iter().find(|s| s.name == type_prefix)
                                .and_then(|s| s.fields.get(1))
                                .map(|(_, t)| c_type_named(t, sn))
                                .unwrap_or_else(|| src.clone());
                            (src, inner)
                        } else {
                            // Non-map combinators: source == result.
                            let c = find_struct_c_name_by_prefix(type_prefix, module, sn)
                                .unwrap_or_else(|| type_prefix.to_string());
                            (c.clone(), c)
                        };

                        helpers.push((name.clone(), src_c, result_c, method.to_string(), closure_c_type, call_fn, ok_field, err_field));
                    }
                }
            }
        }
    }

    if helpers.is_empty() {
        return;
    }

    writeln!(out, "/* ── Option/Result combinator helpers ── */").unwrap();
    for (full_name, src_c, result_c, method, closure_ty, call_fn, ok_field, err_field) in &helpers {
        // Determine if closure params need & prefix (Ptr ABI for resource types)
        let comb_needs_ref = closure_params_need_ref(module, call_fn);
        let cr = if comb_needs_ref.first().copied().unwrap_or(false) { "&" } else { "" };
        match method.as_str() {
            "map" => {
                // map: if tag==0 (Some/Ok): apply closure to payload, wrap; else propagate
                // For map on Result, we need the result type's ok field too
                let result_ok = if full_name.starts_with("Result__") {
                    let result_prefix = full_name.rsplitn(2, "__").nth(1).unwrap_or(full_name);
                    let (rok, _) = enum_payload_fields(result_prefix, module);
                    rok
                } else {
                    ok_field.clone()
                };
                writeln!(out, "static inline {result_c} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    {result_c} __result;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{").unwrap();
                writeln!(out, "        __result.tag = 0;").unwrap();
                writeln!(out, "        __result.{result_ok} = {call_fn}(&__fn, {cr}__src.{ok_field});").unwrap();
                writeln!(out, "    }} else {{").unwrap();
                writeln!(out, "        __result.tag = 1;").unwrap();
                writeln!(out, "    }}").unwrap();
                writeln!(out, "    return __result;").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "filter" => {
                writeln!(out, "static inline {src_c} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0 && {call_fn}(&__fn, {cr}__src.{ok_field})) {{").unwrap();
                writeln!(out, "        return __src;").unwrap();
                writeln!(out, "    }}").unwrap();
                writeln!(out, "    return ({src_c}){{ .tag = 1 }};").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "and_then" => {
                writeln!(out, "static inline {result_c} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{").unwrap();
                writeln!(out, "        return {call_fn}(&__fn, {cr}__src.{ok_field});").unwrap();
                writeln!(out, "    }}").unwrap();
                writeln!(out, "    return ({result_c}){{ .tag = 1 }};").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "or_else" => {
                writeln!(out, "static inline {src_c} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{").unwrap();
                writeln!(out, "        return __src;").unwrap();
                writeln!(out, "    }}").unwrap();
                // Result or_else passes the error value; Option or_else takes no args
                if full_name.starts_with("Result__") {
                    writeln!(out, "    return {call_fn}(&__fn, {cr}__src.{err_field});").unwrap();
                } else {
                    writeln!(out, "    return {call_fn}(&__fn);").unwrap();
                }
                writeln!(out, "}}").unwrap();
            }
            "unwrap_err" | "unwrap_error" => {
                // Look up the actual error type from the struct
                let err_ty_c = module.structs.iter().find(|s| {
                    let c = sn.get(&(module.structs.iter().position(|x| std::ptr::eq(x, *s)).unwrap() as u32))
                        .cloned().unwrap_or_else(|| s.name.clone());
                    c == *src_c
                }).and_then(|s| s.fields.get(2))
                    .map(|(_, t)| c_type_named(t, sn))
                    .unwrap_or_else(|| "void*".to_string());
                writeln!(out, "static inline {err_ty_c} {full_name}(void* __res_ptr) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__res_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 1) {{").unwrap();
                writeln!(out, "        return __src.{err_field};").unwrap();
                writeln!(out, "    }}").unwrap();
                writeln!(out, "    fprintf(stderr, \"unwrap_err on Ok\\n\"); abort();").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "map_err" => {
                // Result__T__E__map_err(result*, closure) → Result__T__E2
                // if Ok: copy Ok field; if Error: apply closure to error payload, wrap in Error
                let result_err = if *result_c != *src_c {
                    // Cross-type: look up the error field name in the result struct
                    let (_, rerr) = enum_payload_fields(
                        module.structs.iter().find(|s| {
                            let cn = sn.get(&(module.structs.iter().position(|x| std::ptr::eq(x, *s)).unwrap() as u32))
                                .cloned().unwrap_or_else(|| s.name.clone());
                            cn == *result_c
                        }).map(|s| s.name.as_str()).unwrap_or(""),
                        module,
                    );
                    rerr
                } else {
                    err_field.clone()
                };
                writeln!(out, "static inline {result_c} {full_name}(void* __res_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__res_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{").unwrap();
                // Cross-type: copy the Ok value into the result struct
                if *result_c != *src_c {
                    let result_ok = {
                        let (rok, _) = enum_payload_fields(
                            module.structs.iter().find(|s| {
                                let cn = sn.get(&(module.structs.iter().position(|x| std::ptr::eq(x, *s)).unwrap() as u32))
                                    .cloned().unwrap_or_else(|| s.name.clone());
                                cn == *result_c
                            }).map(|s| s.name.as_str()).unwrap_or(""),
                            module,
                        );
                        rok
                    };
                    writeln!(out, "        {result_c} __ok_result; __ok_result.tag = 0; __ok_result.{result_ok} = __src.{ok_field};").unwrap();
                    writeln!(out, "        return __ok_result;").unwrap();
                } else {
                    writeln!(out, "        return __src;").unwrap();
                }
                writeln!(out, "    }}").unwrap();
                writeln!(out, "    {result_c} __result;").unwrap();
                writeln!(out, "    __result.tag = 1;").unwrap();
                // Use memcpy to handle Str/GorgetString layout-compatible type mismatches
                writeln!(out, "    {{ __auto_type __me_val = {call_fn}(&__fn, {cr}__src.{err_field}); memcpy(&__result.{result_err}, &__me_val, sizeof(__me_val)); }}").unwrap();
                writeln!(out, "    return __result;").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "or" => {
                // Option__T__or(opt*, other) → Option__T
                // if Some: return self; else return other
                writeln!(out, "static inline {src_c} {full_name}(void* __opt_ptr, {src_c} __other) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{ return __src; }}").unwrap();
                writeln!(out, "    return __other;").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "flatten" => {
                // Option__Option__T__flatten(opt*) → Option__T
                // if outer is Some and inner is Some: return inner; else None
                // result_c is the inner Option type
                writeln!(out, "static inline {result_c} {full_name}(void* __opt_ptr) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{ return __src.{ok_field}; }}").unwrap();
                writeln!(out, "    return ({result_c}){{ .tag = 1 }};").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "unwrap_or_else" => {
                // unwrap_or_else: if tag==0 (Some/Ok): return payload; else call closure
                // For Option: closure takes no args. For Result: closure takes error value.
                let payload_ty = module.structs.iter().find(|s| {
                    let cn = sn.get(&(module.structs.iter().position(|x| std::ptr::eq(x, *s)).unwrap() as u32))
                        .cloned().unwrap_or_else(|| s.name.clone());
                    cn == *src_c
                }).and_then(|s| s.fields.get(1))
                    .map(|(_, t)| c_type_named(t, sn))
                    .unwrap_or_else(|| "int64_t".to_string());
                writeln!(out, "static inline {payload_ty} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{ return __src.{ok_field}; }}").unwrap();
                if full_name.starts_with("Result__") {
                    writeln!(out, "    return {call_fn}(&__fn, {cr}__src.{err_field});").unwrap();
                } else {
                    writeln!(out, "    return {call_fn}(&__fn);").unwrap();
                }
                writeln!(out, "}}").unwrap();
            }
            "flat_map" => {
                writeln!(out, "static inline {result_c} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{ return {call_fn}(&__fn, {cr}__src.{ok_field}); }}").unwrap();
                writeln!(out, "    return ({result_c}){{ .tag = 1 }};").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "zip" => {
                // Option__T__zip(opt*, other) → Option__Tuple (not commonly used in tests, but cover it)
                writeln!(out, "// TODO: {full_name} (zip) not yet implemented").unwrap();
            }
            _ => {
                writeln!(out, "// TODO: {full_name} not yet implemented").unwrap();
            }
        }
        writeln!(out).unwrap();
    }
}
pub(super) fn elem_type_to_c(elem: &str) -> String {
    elem_type_to_c_with_sn(elem, &HashMap::new())
}

pub(super) fn elem_type_to_c_with_sn(elem: &str, orig_to_c: &HashMap<String, String>) -> String {
    match elem {
        "int64_t" | "int32_t" | "int16_t" | "int8_t" => elem.to_string(),
        "uint64_t" | "uint32_t" | "uint16_t" | "uint8_t" => elem.to_string(),
        "bool" => "bool".to_string(),
        "float" | "double" => elem.to_string(),
        "Str" | "GorgetString" => "Str".to_string(),
        _ => {
            // Try to resolve through struct name map.
            if let Some(cname) = orig_to_c.get(elem) {
                return cname.clone();
            }
            // Could be a user struct — use the name as-is.
            elem.to_string()
        }
    }
}
/// Generate blocking spawn/await helpers for each spawned function.
///
/// For each spawned function `foo`, generates:
/// - `Task__<RetType>` typedef (if not already emitted)
/// - `__SpawnCtx_foo` struct (GorgetTask base + params + result)
/// - `__spawn_run_foo()` — worker thread entry, calls the real function
/// - `__spawn_drop_foo()` — RAII cleanup (wait + free)
/// - `__gorget_spawn_foo()` — allocate ctx, init sync, submit to executor
/// - `__gorget_await_foo()` — wait, extract result, free
/// - `Task__<RetType>__drop()` — dispatch to per-fn drop via __drop pointer
pub(super) fn emit_spawn_helpers(out: &mut String, module: &LirModule) {
    writeln!(out, "/* ── Spawn/await helpers (M:N executor pool) ── */").unwrap();

    // Build orig→C name map for resolving spawn param types.
    let sn = build_struct_names(module);
    let orig_to_c: HashMap<String, String> = module.structs.iter().enumerate()
        .map(|(i, def)| (def.name.clone(), sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"))))
        .collect();
    let resolve_type = |t: &str| -> String {
        orig_to_c.get(t).cloned().unwrap_or_else(|| t.to_string())
    };

    // Emit Task__T typedefs for return types not already emitted by the early Task typedef pass.
    let mut emitted_task_types: Vec<String> = Vec::new();
    // Collect already-emitted Task types from module structs (early pass).
    for def in &module.structs {
        if def.name.starts_with("Task__") {
            emitted_task_types.push(def.name.clone());
        }
    }
    for sf in &module.spawned_fns {
        let task_name = if sf.ret_c_type == "void" {
            "Task__void".to_string()
        } else {
            format!("Task__{}", sf.ret_c_type)
        };
        if !emitted_task_types.contains(&task_name) {
            writeln!(out, "typedef struct {{ void* __task; void (*__drop)(void*); }} {task_name};").unwrap();
            emitted_task_types.push(task_name);
        }
    }
    writeln!(out).unwrap();

    for sf in &module.spawned_fns {
        let fn_name = &sf.fn_name;
        let safe_fn_name = c_func_name(fn_name);
        let ret_c = &sf.ret_c_type;
        let is_void = ret_c == "void";
        let ctx_name = format!("__SpawnCtx_{fn_name}");

        // Context struct
        writeln!(out, "typedef struct {ctx_name} {{").unwrap();
        writeln!(out, "    GorgetTask base;").unwrap();
        for (param_name, param_c_type) in &sf.params {
            let resolved = resolve_type(param_c_type);
            writeln!(out, "    {resolved} __{param_name};").unwrap();
        }
        if !is_void {
            let resolved_ret = resolve_type(ret_c);
            writeln!(out, "    {resolved_ret} result;").unwrap();
        }
        writeln!(out, "}} {ctx_name};").unwrap();

        // Run function — called by worker thread
        writeln!(out, "static void __spawn_run_{fn_name}(GorgetTask* __base) {{").unwrap();
        writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)__base;").unwrap();
        let call_args: Vec<String> = sf.params.iter().enumerate().map(|(i, (name, c_type))| {
            if sf.ref_param_indices.contains(&i) {
                format!("&__ctx->__{name}")
            } else if matches!(c_type.as_str(), "GorgetArray" | "GorgetMap" | "GorgetSet") {
                // Collection resource params are void* in the LIR function signature
                // but stored as the actual struct in the spawn context.
                format!("(void*)&__ctx->__{name}")
            } else if matches!(c_type.as_str(), "Str" | "GorgetString") {
                // String params: check if the target function takes void* (Ptr) or Str (by value).
                // Find the target function's param type.
                let target_fn = module.functions.iter().find(|f| f.name == sf.fn_name);
                let target_param_is_ptr = target_fn
                    .and_then(|f| f.params.get(i))
                    .map_or(true, |p| p.is_ptr()); // default to Ptr if unknown
                if target_param_is_ptr {
                    format!("(void*)&__ctx->__{name}")
                } else {
                    format!("__ctx->__{name}")
                }
            } else {
                format!("__ctx->__{name}")
            }
        }).collect();
        let call_str = call_args.join(", ");
        if is_void {
            writeln!(out, "    {safe_fn_name}({call_str});").unwrap();
        } else {
            writeln!(out, "    __ctx->result = {safe_fn_name}({call_str});").unwrap();
        }
        writeln!(out, "}}").unwrap();

        // Drop helper
        writeln!(out, "static void __spawn_drop_{fn_name}(void* __ptr) {{").unwrap();
        writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)__ptr;").unwrap();
        writeln!(out, "    GORGET_SCHEDULER_WAIT(&__ctx->base);").unwrap();
        writeln!(out, "    pthread_mutex_destroy(&__ctx->base.mtx);").unwrap();
        writeln!(out, "    pthread_cond_destroy(&__ctx->base.cond);").unwrap();
        writeln!(out, "    GORGET_FREE(__ctx, sizeof({ctx_name}));").unwrap();
        writeln!(out, "}}").unwrap();

        // Spawn function — returns Task__T (matches GIR behavior).
        // When the LIR destination is a Task struct, the caller uses the struct directly.
        // When the LIR destination is void* (non-vector case), the call site wraps it.
        let task_type_name = if is_void { "Task__void".to_string() } else { format!("Task__{ret_c}") };
        let param_decls: Vec<String> = sf.params.iter().map(|(name, c_type)| {
            let resolved = resolve_type(c_type);
            format!("{resolved} {name}")
        }).collect();
        let param_decl_str = param_decls.join(", ");
        writeln!(out, "static inline {task_type_name} __gorget_spawn_{fn_name}({param_decl_str}) {{").unwrap();
        writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)GORGET_CALLOC(1, sizeof({ctx_name}));").unwrap();
        writeln!(out, "    __ctx->base.run = __spawn_run_{fn_name};").unwrap();
        writeln!(out, "    pthread_mutex_init(&__ctx->base.mtx, NULL);").unwrap();
        writeln!(out, "    pthread_cond_init(&__ctx->base.cond, NULL);").unwrap();
        for (i, (param_name, _c_type)) in sf.params.iter().enumerate() {
            // Clone refcounted params (Channel, Shared, Weak) to avoid dangling pointers.
            if let Some((_, gir_name)) = sf.clone_params.iter().find(|(idx, _)| *idx == i) {
                writeln!(out, "    __ctx->__{param_name} = {gir_name}__clone({param_name});").unwrap();
            } else {
                writeln!(out, "    __ctx->__{param_name} = {param_name};").unwrap();
            }
        }
        writeln!(out, "    GORGET_SCHEDULER_SUBMIT(&__ctx->base);").unwrap();
        writeln!(out, "    return ({task_type_name}){{.__task = __ctx, .__drop = __spawn_drop_{fn_name}}};").unwrap();
        writeln!(out, "}}").unwrap();

        // Await function — takes Task__T by value, extracts __task to get SpawnCtx.
        let resolved_ret = resolve_type(ret_c);
        if is_void {
            writeln!(out, "static inline void __gorget_await_{fn_name}({task_type_name} task) {{").unwrap();
        } else {
            writeln!(out, "static inline {resolved_ret} __gorget_await_{fn_name}({task_type_name} task) {{").unwrap();
        }
        writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)task.__task;").unwrap();
        writeln!(out, "    GORGET_SCHEDULER_WAIT(&__ctx->base);").unwrap();
        if !is_void {
            writeln!(out, "    {resolved_ret} result = __ctx->result;").unwrap();
        }
        writeln!(out, "    pthread_mutex_destroy(&__ctx->base.mtx);").unwrap();
        writeln!(out, "    pthread_cond_destroy(&__ctx->base.cond);").unwrap();
        writeln!(out, "    GORGET_FREE(__ctx, sizeof({ctx_name}));").unwrap();
        if !is_void {
            writeln!(out, "    return result;").unwrap();
        }
        writeln!(out, "}}").unwrap();
        writeln!(out).unwrap();
    }

    // Task__T__drop for each unique Task type
    let mut emitted_task_drops: Vec<String> = Vec::new();
    for sf in &module.spawned_fns {
        let task_name = if sf.ret_c_type == "void" {
            "Task__void".to_string()
        } else {
            format!("Task__{}", sf.ret_c_type)
        };
        if emitted_task_drops.contains(&task_name) {
            continue;
        }
        emitted_task_drops.push(task_name.clone());
        writeln!(out, "static inline void {task_name}__drop({task_name}* self) {{").unwrap();
        writeln!(out, "    if (self && self->__task && self->__drop) {{").unwrap();
        writeln!(out, "        self->__drop(self->__task);").unwrap();
        writeln!(out, "        self->__task = NULL;").unwrap();
        writeln!(out, "    }}").unwrap();
        writeln!(out, "}}").unwrap();
        writeln!(out, "static void (*__unused_{task_name}__drop)({task_name}*) __attribute__((unused)) = {task_name}__drop;").unwrap();
        writeln!(out).unwrap();
    }
}
pub(super) fn emit_thread_helpers(out: &mut String, module: &LirModule) {
    if module.thread_spawned_fns.is_empty() {
        return;
    }
    writeln!(out, "\n/* ── Thread[T] wrappers ── */").unwrap();

    // Collect unique return types for Thread__T typedefs
    let mut emitted_thread_types: Vec<String> = Vec::new();
    for tsf in &module.thread_spawned_fns {
        let ret_c = &tsf.ret_c_type;
        let is_void = ret_c == "void";
        let thread_name = format!("Thread__{ret_c}");
        if emitted_thread_types.contains(&thread_name) {
            continue;
        }
        emitted_thread_types.push(thread_name.clone());
        let ctx_type = format!("__GorgetThread__{ret_c}");
        if is_void {
            writeln!(out, "typedef struct {{ pthread_t _thr; }} {ctx_type};").unwrap();
        } else {
            writeln!(out, "typedef struct {{ pthread_t _thr; {ret_c} _result; }} {ctx_type};").unwrap();
        }
        writeln!(out, "typedef {ctx_type}* {thread_name};").unwrap();
        // id(self) -> int64_t
        writeln!(out, "static inline int64_t {thread_name}__id({thread_name} self) {{ return (int64_t)(uintptr_t)self->_thr; }}").unwrap();
        // join(self) -> T
        if is_void {
            writeln!(out, "static inline void {thread_name}__join({thread_name} self) {{ pthread_join(self->_thr, NULL); GORGET_FREE(self, sizeof(*self)); }}").unwrap();
        } else {
            writeln!(out, "static inline {ret_c} {thread_name}__join({thread_name} self) {{ pthread_join(self->_thr, NULL); {ret_c} _r = self->_result; GORGET_FREE(self, sizeof(*self)); return _r; }}").unwrap();
        }
        writeln!(out).unwrap();
    }

    // Per-function thread entry + spawn helpers
    for tsf in &module.thread_spawned_fns {
        let fn_name = &tsf.fn_name;
        let safe_fn_name = c_func_name(fn_name);
        let ret_c = &tsf.ret_c_type;
        let is_void = ret_c == "void";
        let thread_name = format!("Thread__{ret_c}");
        let ctx_type = format!("__GorgetThread__{ret_c}");

        // Thread entry
        writeln!(out, "static void* __gorget_thread_entry_{fn_name}(void* __arg) {{").unwrap();
        writeln!(out, "    {ctx_type}* __ctx = ({ctx_type}*)__arg;").unwrap();
        if is_void {
            writeln!(out, "    {safe_fn_name}();").unwrap();
        } else {
            writeln!(out, "    __ctx->_result = {safe_fn_name}();").unwrap();
        }
        writeln!(out, "    return NULL;\n}}").unwrap();

        // Spawn function
        writeln!(out, "static inline {thread_name} __gorget_thread_spawn_{fn_name}(void) {{").unwrap();
        writeln!(out, "    {ctx_type}* __ctx = ({ctx_type}*)GORGET_CALLOC(1, sizeof({ctx_type}));").unwrap();
        writeln!(out, "    pthread_create(&__ctx->_thr, NULL, __gorget_thread_entry_{fn_name}, __ctx);").unwrap();
        writeln!(out, "    return __ctx;\n}}").unwrap();
        writeln!(out).unwrap();
    }
}
/// Rewrite GIR local references (`_N`) in inline C code to LIR slot names (`__sN`).
pub(super) fn rewrite_inline_c_locals(code: &str, func: &LirFunction) -> String {
    // Simple regex-free approach: find `_N` patterns and replace with `__sN`.
    let mut result = String::with_capacity(code.len() + 16);
    let bytes = code.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'_' && (i == 0 || !bytes[i-1].is_ascii_alphanumeric()) {
            // Check if followed by digits
            let start = i + 1;
            let mut end = start;
            while end < bytes.len() && bytes[end].is_ascii_digit() {
                end += 1;
            }
            if end > start && (end >= bytes.len() || !bytes[end].is_ascii_alphanumeric()) {
                let num: u32 = code[start..end].parse().unwrap_or(0);
                // Map GIR local index to LIR slot if possible
                if (num as usize) < func.slots.len() {
                    result.push_str(&format!("__s{}", num));
                } else {
                    result.push('_');
                    result.push_str(&code[start..end]);
                }
                i = end;
                continue;
            }
        }
        result.push(bytes[i] as char);
        i += 1;
    }
    result
}
pub(super) fn emit_global_init(out: &mut String, init: &LirGlobalInit, ty: &LirType, funcs: &[LirFunction], structs: &[StructDef]) {
    write!(out, " = ").unwrap();
    emit_global_init_value(out, init, ty, funcs, structs);
}

pub(super) fn emit_global_init_value(out: &mut String, init: &LirGlobalInit, ty: &LirType, funcs: &[LirFunction], structs: &[StructDef]) {
    match init {
        LirGlobalInit::Zeroed => write!(out, "{{0}}").unwrap(),
        LirGlobalInit::Bytes(b) => {
            let is_float = matches!(ty, LirType::F32 | LirType::F64);
            match (b.len(), is_float) {
                (4, true) => {
                    let val = f32::from_le_bytes([b[0], b[1], b[2], b[3]]);
                    if val.is_finite() {
                        write!(out, "{val:.17e}").unwrap();
                    } else {
                        write!(out, "{val}").unwrap();
                    }
                }
                (8, true) => {
                    let val = f64::from_le_bytes([b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7]]);
                    if val.is_finite() {
                        write!(out, "{val:.17e}").unwrap();
                    } else {
                        write!(out, "{val}").unwrap();
                    }
                }
                (1, _) => write!(out, "{}", b[0] as i8).unwrap(),
                (2, _) => write!(out, "{}", i16::from_le_bytes([b[0], b[1]])).unwrap(),
                (4, _) => write!(out, "{}", i32::from_le_bytes([b[0], b[1], b[2], b[3]])).unwrap(),
                (8, _) => write!(out, "{}LL", i64::from_le_bytes([b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7]])).unwrap(),
                _ => write!(out, "{{0}} /* {} bytes */", b.len()).unwrap(),
            }
        }
        LirGlobalInit::FuncAddr(fid) => {
            let fname = funcs.get(fid.0 as usize).map(|f| f.name.as_str()).unwrap_or("__unknown_fn");
            write!(out, "(void*)&{fname}").unwrap();
        }
        LirGlobalInit::Struct { struct_id, fields } => {
            write!(out, "{{").unwrap();
            let field_types: Option<&[(String, LirType)]> = structs.get(struct_id.0 as usize)
                .map(|sd| sd.fields.as_slice());
            for (i, f) in fields.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                let ft = field_types.and_then(|fts| fts.get(i).map(|(_, t)| t)).unwrap_or(&LirType::I64);
                emit_global_init_value(out, f, ft, funcs, structs);
            }
            write!(out, "}}").unwrap();
        }
        LirGlobalInit::RuntimeCall(_) => {
            // Runtime calls are initialized separately in __gorget_init_globals.
            write!(out, "{{0}}").unwrap();
        }
    }
}
/// Map LirType to C type string.
/// Returns true if the function is provided by standard C headers
/// (stdio.h, stdlib.h, string.h) and should not be re-declared.
/// Emit a coerced argument value.
/// Emit an argument with explicit ABI marshalling. Returns true if handled,
/// false if the caller should fall back to existing logic.
pub(super) fn emit_abi_arg(
    out: &mut String,
    val: &str,
    abi: crate::ir::abi::AbiKind,
    arg_ty: Option<&LirType>,
    is_str_lit: bool,
) -> bool {
    use crate::ir::abi::AbiKind;
    let is_ptr = arg_ty.map_or(false, |t| t.is_ptr());
    let is_struct = arg_ty.map_or(false, |t| t.is_aggregate());
    match abi {
        AbiKind::CStr => {
            // Under 32-byte Str, extract .data for const char* params.
            if is_str_lit || is_struct {
                write!(out, "(const char*){val}.data").unwrap();
            } else if is_ptr {
                write!(out, "({val} ? gorget_str_to_cstr(*(Str*){val}) : NULL)").unwrap();
            } else {
                write!(out, "{val}").unwrap();
            }
            true
        }
        AbiKind::BytePtr => {
            if is_str_lit || is_struct {
                write!(out, "(const char*){val}.data").unwrap();
            } else if is_ptr {
                // Ptr(Str) → deref to get Str, then .data for char*
                write!(out, "({val} ? (const char*)((Str*){val})->data : NULL)").unwrap();
            } else {
                write!(out, "{val}").unwrap();
            }
            true
        }
        AbiKind::GorgetString => {
            if is_str_lit {
                write!(out, "gorget_str_from_literal({val}, strlen({val}))").unwrap();
            } else if is_ptr {
                write!(out, "*(Str*){val}").unwrap();
            } else {
                write!(out, "{val}").unwrap();
            }
            true
        }
        AbiKind::Ptr => {
            // Callee expects a pointer. If arg is a struct, take its address.
            if is_struct {
                write!(out, "&{val}").unwrap();
            } else {
                write!(out, "{val}").unwrap();
            }
            true
        }
        AbiKind::Opaque | AbiKind::Scalar => {
            write!(out, "{val}").unwrap();
            true
        }
        AbiKind::Auto => false, // fall back to existing logic
    }
}

/// Handles: Ptr→Str (string literal wrapping), Ptr→Aggregate (dereference), GorgetString→Str.
pub(super) fn emit_coerced_arg(
    out: &mut String,
    a: &ValueId,
    param_ty: Option<&LirType>,
    val_types: &[Option<LirType>],
    str_lit_vals: &[bool],
    sn: &HashMap<u32, String>,
) {
    let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
    let is_str_lit = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
    let param_name = param_ty.map(|t| c_type_named(t, sn));
    let arg_name = arg_ty.map(|t| c_type_named(t, sn));

    // GorgetString ↔ Str coercion — both are the same 32-byte struct, identity.
    if (param_name.as_deref() == Some("Str") && arg_name.as_deref() == Some("GorgetString"))
        || (param_name.as_deref() == Some("GorgetString") && arg_name.as_deref() == Some("Str"))
    {
        write!(out, "{}", format!("__v{}", a.0)).unwrap();
        return;
    }

    // Str struct → char* param: extract .data (for printf format strings, legacy C FFI).
    // The Str's data pointer is valid and NUL-terminated for owned/literal strings.
    if matches!(param_ty, Some(LirType::Ptr))
        && (arg_name.as_deref() == Some("Str") || arg_name.as_deref() == Some("GorgetString"))
    {
        write!(out, "(const char*)__v{}.data", a.0).unwrap();
        return;
    }
    // PtrTo(Str) → void* param: pass the pointer directly (it's already void*).
    if param_ty.map_or(false, |t| t.is_ptr()) && arg_ty.map_or(false, |t| matches!(t, LirType::PtrTo(_))) {
        write!(out, "__v{}", a.0).unwrap();
        return;
    }

    if param_ty.map_or(false, |t| t.is_aggregate()) && arg_ty.map_or(false, |t| t.is_ptr()) {
        let ty_name = param_name.as_deref().unwrap_or("void");
        if is_str_lit && ty_name == "Str" {
            write!(out, "gorget_str_from_literal({v}, strlen({v}))", v = format!("__v{}", a.0)).unwrap();
        } else if is_str_lit && ty_name == "GorgetString" {
            // String literal → GorgetString: wrap with gorget_string_new.
            write!(out, "gorget_string_new({})", format!("__v{}", a.0)).unwrap();
        } else if ty_name == "Str" {
            // Ptr to Str (from SlotAddr of GorgetString slot?) — try coercion.
            write!(out, "*({ty_name}*)__v{}", a.0).unwrap();
        } else {
            write!(out, "*({ty_name}*)__v{}", a.0).unwrap();
        }
    }
    // Str struct arg → unknown callee (no param_ty info): extract .data for const char*.
    // This is the catch-all for runtime functions like gorget_file_open, gorget_file_write,
    // gorget_socket_write_str, etc. that take const char* but receive Str structs.
    else if param_ty.is_none() && (arg_name.as_deref() == Some("Str") || arg_name.as_deref() == Some("GorgetString")) {
        write!(out, "(const char*)__v{}.data", a.0).unwrap();
    }
    else {
        write!(out, "__v{}", a.0).unwrap();
    }
}
/// Returns true if the LIR type is a GorgetString struct.
pub(super) fn is_gorget_string_type(ty: Option<&LirType>, sn: &HashMap<u32, String>) -> bool {
    if let Some(LirType::Struct(sid)) = ty {
        let name = sn.get(&sid.0).map(|s| s.as_str()).unwrap_or("");
        name == "GorgetString"
    } else {
        false
    }
}
/// Emit inline tag-checked clones for Option fields containing resources.
/// Drop-side intentionally does nothing — Option types have DropStrategy::None
/// to avoid double-free with match/unwrap paths, and struct-field drops rely
/// on that. Clone-side deep-copies because cloning is always safe.
fn emit_option_field_clones(
    out: &mut String,
    sdef: &crate::lir::StructDef,
    already_handled: &std::collections::HashSet<String>,
    module: &crate::lir::LirModule,
) {
    for (fname, fty) in &sdef.fields {
        if already_handled.contains(fname) { continue; }
        if let crate::lir::LirType::Struct(fsid) = fty {
            if let Some(fdef) = module.structs.get(fsid.0 as usize) {
                if fdef.enum_kind == crate::lir::EnumKind::Option {
                    for (vfname, vfty) in &fdef.fields {
                        if vfname == "tag" { continue; }
                        if let crate::lir::LirType::Struct(vfsid) = vfty {
                            if let Some(vfdef) = module.structs.get(vfsid.0 as usize) {
                                let clone_fn = match vfdef.name.as_str() {
                                    "GorgetString" => Some("gorget_string_clone_to_owned"),
                                    "GorgetArray"  => Some("gorget_array_clone"),
                                    "GorgetMap"    => Some("gorget_map_clone"),
                                    "GorgetSet"    => Some("gorget_set_clone"),
                                    _ => None,
                                };
                                if let Some(cfn) = clone_fn {
                                    writeln!(out, "    if (dst.{fname}.tag != 0) {{ dst.{fname}.{vfname} = {cfn}(&dst.{fname}.{vfname}); }}").unwrap();
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// When a struct has fields that need dropping (e.g., GorgetString), the drop
/// elaboration marks it as Recursive. When that struct appears as a field in
/// another struct, the parent's drop emits a call to `{Name}__drop`. This
/// function generates the actual `{Name}__drop` function body.
pub(super) fn emit_recursive_struct_drops(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    for (idx, sdef) in module.structs.iter().enumerate() {
        let type_name = &sdef.name;

        // Check if this is a struct that needs a Recursive drop function
        let drop_info = match module.recursive_drop_structs.get(type_name.as_str()) {
            Some(info) => info,
            None => continue,
        };

        // Check if a drop function already exists (custom Drop trait impl)
        let drop_fn_name = format!("{type_name}__drop");
        if module.functions.iter().any(|f| f.name == drop_fn_name) {
            continue;
        }

        // Use the C struct name (e.g., __lir_s10) instead of the Gorget name
        let c_name = sn.get(&(idx as u32)).cloned().unwrap_or_else(|| type_name.clone());

        // Generate the drop function.
        // NOTE: Option/Result fields are intentionally NOT dropped here — they
        // have DropStrategy::None to avoid double-free with match/unwrap paths.
        // The clone function DOES deep-copy them to prevent CoW aliasing.
        writeln!(out, "static inline void {drop_fn_name}({c_name}* self) {{").unwrap();
        for (field_name, drop_fn, _field_type_name) in drop_info {
            if drop_fn.starts_with("__clone_only:") { continue; }
            writeln!(out, "    {drop_fn}(&self->{field_name});").unwrap();
        }
        writeln!(out, "}}").unwrap();
        writeln!(out).unwrap();
    }
}
/// Emit per-type clone functions for structs with Recursive drop strategy.
/// These produce independently-owned deep copies by memcpy + per-field clone.
/// Called from collection reads (IndexLoad, Option unwrap) so extracted elements
/// don't share resource field buffers with the collection.
pub(super) fn emit_recursive_struct_clones(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    for (idx, sdef) in module.structs.iter().enumerate() {
        let type_name = &sdef.name;

        let drop_info = match module.recursive_drop_structs.get(type_name.as_str()) {
            Some(info) => info,
            None => continue,
        };

        // Skip if a user-defined clone already exists
        let clone_fn_name = format!("{type_name}__clone");
        if module.functions.iter().any(|f| f.name == clone_fn_name) {
            continue;
        }

        let c_name = sn.get(&(idx as u32)).cloned().unwrap_or_else(|| type_name.clone());

        // Generate: TypeName__clone(void* __p) → T with deep-cloned resource fields
        // NOT static — the IndexLoad path emits a non-static extern declaration.
        // Null-safe: return zero struct if __p is null (from uninitialized Ptr locals).
        writeln!(out, "{c_name} {clone_fn_name}(void* __p) {{").unwrap();
        writeln!(out, "    if (!__p) {{ {c_name} z = {{0}}; return z; }}").unwrap();
        writeln!(out, "    {c_name} dst = *({c_name}*)__p;").unwrap();
        for (field_name, drop_fn, _field_type_name) in drop_info {
            // Handle clone-only entries (Option/Result fields)
            if let Some(clone_name) = drop_fn.strip_prefix("__clone_only:") {
                writeln!(out, "    dst.{field_name} = {clone_name}(&dst.{field_name});").unwrap();
                continue;
            }
            // Map drop function → clone function
            let clone_fn = match drop_fn.as_str() {
                // Clone to owned: CoW materializations must produce independently-owned
                // copies. The MoveZero gap that required view-preserving clones is fixed.
                "gorget_string_free" => "gorget_string_clone_to_owned",
                "gorget_array_free" => "gorget_array_clone",
                "gorget_map_free" => "gorget_map_clone",
                "gorget_set_free" => "gorget_set_clone",
                other if other.ends_with("__drop") => {
                    // Recursive or Custom-drop field: call its clone function if it exists.
                    // For Recursive fields, __clone is generated by this same pass.
                    // For Custom-drop fields, use deep_clone_resource_fields inline.
                    let base = &other[..other.len() - 6]; // strip "__drop"
                    let inner_clone = format!("{base}__clone");
                    // Check if this inner type also has a Recursive clone (will be generated)
                    if module.recursive_drop_structs.contains_key(base)
                        || module.recursive_drop_enums.contains_key(base)
                    {
                        writeln!(out, "    dst.{field_name} = {inner_clone}(&dst.{field_name});").unwrap();
                        continue;
                    }
                    // Custom-drop field: clone resource fields inline via deep_clone_resource_fields
                    if let Some((inner_sid, _)) = module.structs.iter().enumerate()
                        .find(|(_, s)| s.name == base)
                    {
                        if let Some(ops) = deep_clone_resource_fields(
                            crate::lir::StructId(inner_sid as u32),
                            &format!("dst.{field_name}"),
                            module,
                        ) {
                            for op in ops {
                                writeln!(out, "    {op}").unwrap();
                            }
                        }
                    }
                    continue;
                }
                _ => continue, // Unknown drop — skip cloning this field
            };
            writeln!(out, "    dst.{field_name} = {clone_fn}(&dst.{field_name});").unwrap();
        }
        // Inline clone for Option fields containing resources (mirrors drop logic).
        {
            let already_cloned: std::collections::HashSet<String> = drop_info.iter()
                .map(|(f, _, _)| f.clone())
                .collect();
            emit_option_field_clones(out, &module.structs[idx], &already_cloned, module);
        }
        writeln!(out, "    return dst;").unwrap();
        writeln!(out, "}}").unwrap();
        // In-place wrapper for use as elem_clone/val_clone function pointer.
        writeln!(out, "void {clone_fn_name}_inplace(void* __p) {{ *({c_name}*)__p = {clone_fn_name}(__p); }}").unwrap();
        writeln!(out).unwrap();
    }
}
/// Emit per-type clone functions for ENUM types with Recursive drop.
/// Uses tag-based dispatch to clone the active variant's resource fields.
pub(super) fn emit_recursive_enum_clones(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    for (idx, sdef) in module.structs.iter().enumerate() {
        let type_name = &sdef.name;

        let variant_info = match module.recursive_drop_enums.get(type_name.as_str()) {
            Some(info) => info,
            None => continue,
        };

        // Skip if a user-defined clone already exists
        let clone_fn_name = format!("{type_name}__clone");
        if module.functions.iter().any(|f| f.name == clone_fn_name) {
            continue;
        }

        let c_name = sn.get(&(idx as u32)).cloned().unwrap_or_else(|| type_name.clone());

        // Map drop function → clone function
        fn drop_to_clone(drop_fn: &str) -> String {
            match drop_fn {
                // Always use clone_to_owned: enum clones must independently own all
                // string data because gorget_string_free is called on drop.
                "gorget_string_free" => "gorget_string_clone_to_owned".into(),
                "gorget_array_free" => "gorget_array_clone".into(),
                "gorget_map_free" => "gorget_map_clone".into(),
                "gorget_set_free" => "gorget_set_clone".into(),
                "free" => "__gorget_box_clone".into(),
                other if other.ends_with("__drop") => {
                    let base = &other[..other.len() - 6];
                    format!("{base}__clone")
                }
                _ => String::new(),
            }
        }

        writeln!(out, "{c_name} {clone_fn_name}(void* __p) {{").unwrap();
        writeln!(out, "    if (!__p) {{ {c_name} z = {{0}}; return z; }}").unwrap();
        writeln!(out, "    {c_name} dst = *({c_name}*)__p;").unwrap();
        writeln!(out, "    switch (dst.tag) {{").unwrap();

        // Group variant_info by variant index
        let mut by_variant: std::collections::HashMap<u32, Vec<(&str, &str, &str, &str)>> = std::collections::HashMap::new();
        for (vi, vname, field_name, drop_fn, field_type_name) in variant_info {
            by_variant.entry(*vi).or_default().push((vname, field_name, drop_fn, field_type_name));
        }

        let mut indices: Vec<u32> = by_variant.keys().copied().collect();
        indices.sort();
        for vi in indices {
            let fields = &by_variant[&vi];
            write!(out, "        case {vi}: ").unwrap();
            for (variant_name, field_name, drop_fn, _field_type_name) in fields {
                // Handle clone-only entries (Option/Result fields)
                let clone_fn = if let Some(clone_name) = drop_fn.strip_prefix("__clone_only:") {
                    clone_name.to_string()
                } else {
                    drop_to_clone(drop_fn)
                };
                // Only emit clone call if the function is a known runtime clone OR
                // will be generated (exists in recursive_drop_structs/enums).
                // Handle types like Task with Trivial drop but no clone are left
                // as shallow copies (from the initial `dst = *(Type*)__p`).
                let clone_exists = matches!(clone_fn.as_str(),
                    "gorget_string_clone_to_owned" | "gorget_array_clone"
                    | "gorget_map_clone" | "gorget_set_clone")
                    || clone_fn.ends_with("__clone") && {
                        let base = &clone_fn[..clone_fn.len() - 7];
                        module.recursive_drop_structs.contains_key(base)
                            || module.recursive_drop_enums.contains_key(base)
                            || module.functions.iter().any(|f| f.name == clone_fn)
                    };
                if !clone_fn.is_empty() && (clone_fn == "__gorget_box_clone" || clone_exists) {
                    let variant_prefix = format!("{variant_name}_");
                    let variant_field_count = sdef.fields.iter()
                        .filter(|(n, _)| n.starts_with(&variant_prefix))
                        .count();
                    let access = if sdef.is_union_layout && variant_field_count > 1 {
                        format!("data.{variant_name}.{field_name}")
                    } else if sdef.is_union_layout {
                        format!("data.{field_name}")
                    } else {
                        field_name.to_string()
                    };
                    if clone_fn == "__gorget_box_clone" {
                        // Box: alloc new box, copy content, deep-clone content
                        let inner_type = _field_type_name.strip_prefix("Box__").unwrap_or(_field_type_name);
                        let inner_clone = format!("{inner_type}__clone_inplace");
                        let has_inner_clone = module.recursive_drop_structs.contains_key(inner_type)
                            || module.recursive_drop_enums.contains_key(inner_type);
                        let alloc_fn = format!("__gorget_box_alloc_{inner_type}");
                        let inner_c_name = module.structs.iter().enumerate()
                            .find(|(_, s)| s.name == inner_type)
                            .and_then(|(i, _)| sn.get(&(i as u32)).cloned())
                            .unwrap_or_else(|| inner_type.to_string());
                        write!(out, "dst.{access} = {alloc_fn}(*({inner_c_name}*)dst.{access}); ").unwrap();
                        if has_inner_clone {
                            write!(out, "{inner_clone}(dst.{access}); ").unwrap();
                        }
                    } else {
                        write!(out, "dst.{access} = {clone_fn}(&dst.{access}); ").unwrap();
                    }
                }
            }
            writeln!(out, "break;").unwrap();
        }

        writeln!(out, "    }}").unwrap();
        writeln!(out, "    return dst;").unwrap();
        writeln!(out, "}}").unwrap();
        writeln!(out, "void {clone_fn_name}_inplace(void* __p) {{ *({c_name}*)__p = {clone_fn_name}(__p); }}").unwrap();
        writeln!(out).unwrap();
    }
}
/// Emit drop functions for enums with resource-type variant payloads.
/// These are called explicitly from the GIR reassignment path for
/// enums that have needs_drop=true but DropStrategy::None.
pub(super) fn emit_enum_drop_fns(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    use std::fmt::Write;
    for (idx, sdef) in module.structs.iter().enumerate() {
        let type_name = &sdef.name;
        let variant_info = match module.recursive_drop_enums.get(type_name.as_str()) {
            Some(info) => info,
            None => continue,
        };
        let drop_fn_name = format!("{type_name}__drop");
        // Skip if already generated by emit_recursive_struct_drops or user-defined
        if module.functions.iter().any(|f| f.name == drop_fn_name) {
            continue;
        }
        // Skip types that have a real DropStrategy (already handled)
        // We only want enums with None strategy that have resource payloads
        // These are NOT in recursive_drop_structs (that's for structs)
        if module.recursive_drop_structs.contains_key(type_name.as_str()) {
            continue;
        }
        let c_name = sn.get(&(idx as u32)).cloned().unwrap_or_else(|| type_name.clone());
        let mut by_variant: std::collections::HashMap<u32, Vec<(&str, &str, &str, &str)>> = std::collections::HashMap::new();
        for (vi, vname, field_name, drop_fn, field_type_name) in variant_info {
            by_variant.entry(*vi).or_default().push((vname, field_name, drop_fn, field_type_name));
        }
        if by_variant.is_empty() { continue; }
        writeln!(out, "void {drop_fn_name}(void* __p) {{").unwrap();
        writeln!(out, "    {c_name}* self = ({c_name}*)__p;").unwrap();
        writeln!(out, "    switch (self->tag) {{").unwrap();
        let mut indices: Vec<u32> = by_variant.keys().copied().collect();
        indices.sort();
        for vi in indices {
            let fields = &by_variant[&vi];
            write!(out, "        case {vi}: ").unwrap();
            for (variant_name, field_name, drop_fn, _field_type_name) in fields {
                if drop_fn.starts_with("__clone_only:") { continue; }
                let variant_prefix = format!("{variant_name}_");
                let variant_field_count = sdef.fields.iter()
                    .filter(|(n, _)| n.starts_with(&variant_prefix))
                    .count();
                let access = if sdef.is_union_layout && variant_field_count > 1 {
                    format!("data.{variant_name}.{field_name}")
                } else if sdef.is_union_layout {
                    format!("data.{field_name}")
                } else {
                    field_name.to_string()
                };
                // Self-cleaning: gorget_array_free/gorget_map_free drop elements.
                // Box fields (free): pass the pointer value directly.
                // Other drop fns (gorget_string_free, etc.): pass address of field.
                if *drop_fn == "free" {
                    write!(out, "free(self->{access}); ").unwrap();
                } else {
                    write!(out, "{drop_fn}(&self->{access}); ").unwrap();
                }
            }
            writeln!(out, "break;").unwrap();
        }
        writeln!(out, "    }}").unwrap();
        writeln!(out, "}}").unwrap();
        writeln!(out).unwrap();
    }
}
/// Emit unified drop/clone functions from type_drop_fns.
/// Generates Type__drop(void*) for every type with droppable fields.
/// Skips types that already have a drop function from the old generators or user code.
pub(super) fn emit_type_drop_fns(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    use std::fmt::Write;
    for (idx, sdef) in module.structs.iter().enumerate() {
        let type_name = &sdef.name;
        let info = match module.type_drop_fns.get(type_name.as_str()) {
            Some(i) => i,
            None => continue,
        };

        let c_name = sn.get(&(idx as u32)).cloned().unwrap_or_else(|| type_name.clone());

        // --- Drop function ---
        // Skip if the exact function name already exists. For mangled names
        // (__gorget_dtor_*), always generate — the old generators don't produce them.
        let already_has_drop = module.functions.iter().any(|f| f.name == info.drop_fn_name)
            || (!info.drop_fn_name.starts_with("__gorget_dtor_") && (
                module.recursive_drop_structs.contains_key(type_name.as_str())
                || module.recursive_drop_enums.contains_key(type_name.as_str())
            ));
        if !already_has_drop {
            if let Some(ref variants) = info.enum_variants {
                // Enum drop: switch on tag
                writeln!(out, "void {}(void* __p) {{", info.drop_fn_name).unwrap();
                writeln!(out, "    {c_name}* self = ({c_name}*)__p;").unwrap();
                if let Some(ref user_fn) = info.user_drop_fn {
                    writeln!(out, "    {user_fn}(__p);").unwrap();
                }
                writeln!(out, "    switch (self->tag) {{").unwrap();
                let mut by_variant: std::collections::HashMap<u32, Vec<(&str, &str, &str, &str)>> = std::collections::HashMap::new();
                for (vi, vname, field_name, drop_fn, ftn) in variants {
                    by_variant.entry(*vi).or_default().push((vname, field_name, drop_fn, ftn));
                }
                let mut indices: Vec<u32> = by_variant.keys().copied().collect();
                indices.sort();
                for vi in indices {
                    let fields = &by_variant[&vi];
                    write!(out, "        case {vi}: ").unwrap();
                    for (variant_name, field_name, drop_fn, _ftn) in fields {
                        let variant_prefix = format!("{variant_name}_");
                        let variant_field_count = sdef.fields.iter()
                            .filter(|(n, _)| n.starts_with(&variant_prefix))
                            .count();
                        let access = if sdef.is_union_layout && variant_field_count > 1 {
                            format!("data.{variant_name}.{field_name}")
                        } else if sdef.is_union_layout {
                            format!("data.{field_name}")
                        } else {
                            field_name.to_string()
                        };
                        if *drop_fn == "free" {
                            write!(out, "free(self->{access}); ").unwrap();
                        } else {
                            write!(out, "{drop_fn}(&self->{access}); ").unwrap();
                        }
                    }
                    writeln!(out, "break;").unwrap();
                }
                writeln!(out, "    }}").unwrap();
                writeln!(out, "}}").unwrap();
                writeln!(out).unwrap();
            } else {
                // Struct drop: call per-field drops
                writeln!(out, "void {}(void* __p) {{", info.drop_fn_name).unwrap();
                writeln!(out, "    {c_name}* self = ({c_name}*)__p;").unwrap();
                if let Some(ref user_fn) = info.user_drop_fn {
                    writeln!(out, "    {user_fn}(__p);").unwrap();
                }
                for (field_name, drop_fn, _ftn) in &info.field_drops {
                    if drop_fn == "free" {
                        writeln!(out, "    free(self->{field_name});").unwrap();
                    } else {
                        writeln!(out, "    {drop_fn}(&self->{field_name});").unwrap();
                    }
                }
                writeln!(out, "}}").unwrap();
                writeln!(out).unwrap();
            }
        }

        // --- Clone function ---
        let clone_fn_name = format!("{type_name}__clone");
        let already_has_clone = module.functions.iter().any(|f| f.name == clone_fn_name)
            || module.recursive_drop_structs.contains_key(type_name.as_str())
            || module.recursive_drop_enums.contains_key(type_name.as_str());
        if !already_has_clone {
            fn drop_to_clone_fn(drop_fn: &str) -> Option<String> {
                match drop_fn {
                    "gorget_string_free" => Some("gorget_string_clone_to_owned".into()),
                    "gorget_array_free" => Some("gorget_array_clone".into()),
                    "gorget_map_free" => Some("gorget_map_clone".into()),
                    "gorget_set_free" => Some("gorget_set_clone".into()),
                    other if other.ends_with("__drop") => {
                        let base = &other[..other.len() - 6];
                        Some(format!("{base}__clone"))
                    }
                    other if other.starts_with("__gorget_dtor_") => {
                        let base = &other["__gorget_dtor_".len()..];
                        Some(format!("{base}__clone"))
                    }
                    _ => None,
                }
            }

            if let Some(ref variants) = info.enum_variants {
                // Enum clone (null-safe)
                writeln!(out, "{c_name} {clone_fn_name}(void* __p) {{").unwrap();
                writeln!(out, "    if (!__p) {{ {c_name} z = {{0}}; return z; }}").unwrap();
                writeln!(out, "    {c_name} dst = *({c_name}*)__p;").unwrap();
                writeln!(out, "    switch (dst.tag) {{").unwrap();
                let mut by_variant: std::collections::HashMap<u32, Vec<(&str, &str, &str, &str)>> = std::collections::HashMap::new();
                for (vi, vname, fname, dfn, ftn) in variants {
                    by_variant.entry(*vi).or_default().push((vname, fname, dfn, ftn));
                }
                let mut indices: Vec<u32> = by_variant.keys().copied().collect();
                indices.sort();
                for vi in indices {
                    let fields = &by_variant[&vi];
                    write!(out, "        case {vi}: ").unwrap();
                    for (variant_name, field_name, drop_fn, _ftn) in fields {
                        if let Some(cfn) = drop_to_clone_fn(drop_fn) {
                            let variant_prefix = format!("{variant_name}_");
                            let variant_field_count = sdef.fields.iter()
                                .filter(|(n, _)| n.starts_with(&variant_prefix))
                                .count();
                            let access = if sdef.is_union_layout && variant_field_count > 1 {
                                format!("data.{variant_name}.{field_name}")
                            } else if sdef.is_union_layout {
                                format!("data.{field_name}")
                            } else {
                                field_name.to_string()
                            };
                            write!(out, "dst.{access} = {cfn}(&dst.{access}); ").unwrap();
                        }
                    }
                    writeln!(out, "break;").unwrap();
                }
                writeln!(out, "    }}").unwrap();
                writeln!(out, "    return dst;").unwrap();
                writeln!(out, "}}").unwrap();
                writeln!(out, "void {clone_fn_name}_inplace(void* __p) {{ *({c_name}*)__p = {clone_fn_name}(__p); }}").unwrap();
                writeln!(out).unwrap();
            } else {
                // Struct clone (null-safe)
                writeln!(out, "{c_name} {clone_fn_name}(void* __p) {{").unwrap();
                writeln!(out, "    if (!__p) {{ {c_name} z = {{0}}; return z; }}").unwrap();
                writeln!(out, "    {c_name} dst = *({c_name}*)__p;").unwrap();
                for (field_name, drop_fn, _ftn) in &info.field_drops {
                    if let Some(cfn) = drop_to_clone_fn(drop_fn) {
                        writeln!(out, "    dst.{field_name} = {cfn}(&dst.{field_name});").unwrap();
                    }
                }
                writeln!(out, "    return dst;").unwrap();
                writeln!(out, "}}").unwrap();
                writeln!(out, "void {clone_fn_name}_inplace(void* __p) {{ *({c_name}*)__p = {clone_fn_name}(__p); }}").unwrap();
                writeln!(out).unwrap();
            }
        }
    }
}
/// Emit typedefs and inline wrappers for monomorphized wrapper types
/// (Channel__T, Shared__T, Weak__T, AtomicInt, AtomicBool).
pub(super) fn emit_monomorphized_typedefs(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    let mut type_seen = std::collections::HashSet::new();
    let mut method_seen = std::collections::HashSet::new();
    // Build original-name → C-name map for resolving element types in wrappers.
    let orig_to_c: HashMap<String, String> = module.structs.iter().enumerate()
        .map(|(i, def)| (def.name.clone(), sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"))))
        .collect();

    // Collect all wrapper type names from struct defs, struct_names, and spawned_fns.
    let mut type_names: Vec<String> = Vec::new();
    for def in &module.structs {
        if is_monomorphized_wrapper_type(&def.name) && type_seen.insert(def.name.clone()) {
            type_names.push(def.name.clone());
        }
    }
    for name in sn.values() {
        if is_monomorphized_wrapper_type(name) && type_seen.insert(name.clone()) {
            type_names.push(name.clone());
        }
    }
    for sf in &module.spawned_fns {
        for n in std::iter::once(&sf.ret_c_type).chain(sf.params.iter().map(|(_, t)| t)) {
            if is_monomorphized_wrapper_type(n) && type_seen.insert(n.clone()) {
                type_names.push(n.clone());
            }
        }
    }

    // Emit typedefs (skip unmonomorphized wrappers like Guard__T)
    for name in &type_names {
        if is_unmonomorphized_wrapper(name) { continue; }
        emit_wrapper_typedef(out, name, module, &orig_to_c);
    }

    // Collect all Channel/Shared/Weak/Mutex/RWLock method names from CallExtern instructions.
    let mut method_calls: Vec<String> = Vec::new();
    let is_wrapper_method = |n: &str| -> bool {
        n.starts_with("Channel__") || n.starts_with("Shared__")
        || n.starts_with("Weak__") || n.starts_with("Mutex__")
        || n.starts_with("RWLock__") || n.starts_with("Guard__")
        || n.starts_with("ReadGuard__") || n.starts_with("WriteGuard__")
        || n.starts_with("Box__")
    };
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::CallExtern { name, .. } = inst {
                    if is_wrapper_method(name) && method_seen.insert(name.clone()) {
                        method_calls.push(name.clone());
                    }
                }
            }
        }
    }
    // Also scan externs list
    for ext in &module.externs {
        if is_wrapper_method(&ext.name) && method_seen.insert(ext.name.clone()) {
            method_calls.push(ext.name.clone());
        }
    }
    // Synthesize clone method calls for refcounted types captured by spawn helpers.
    for sf in &module.spawned_fns {
        for (_idx, gir_name) in &sf.clone_params {
            let clone_name = format!("{gir_name}__clone");
            if method_seen.insert(clone_name.clone()) {
                method_calls.push(clone_name);
            }
        }
    }

    // First pass: discover types from method calls and emit all typedefs.
    // Also discover and typedef element types (e.g., Vector__int64_t inside Shared__Vector__int64_t).
    for name in &method_calls {
        let type_prefix = if let Some((tp, _)) = parse_channel_method(name) {
            Some(tp)
        } else if let Some((tp, _)) = parse_shared_method(name) {
            Some(tp)
        } else if let Some((tp, _)) = parse_weak_method(name) {
            Some(tp)
        } else if let Some((tp, _)) = parse_mutex_method(name) {
            Some(tp)
        } else if let Some((tp, _)) = parse_rwlock_method(name) {
            Some(tp)
        } else if let Some((tp, _)) = parse_box_method(name) {
            Some(tp)
        } else {
            None
        };
        if let Some(ref tp) = type_prefix {
            // Skip unmonomorphized generic wrappers (e.g. Shared__Vector__T)
            if is_unmonomorphized_wrapper(tp) { continue; }
            // Auto-discover element types that may also need typedefs.
            let elem_name = if tp.starts_with("Channel__") {
                channel_elem_type(tp).to_string()
            } else if tp.starts_with("Mutex__") {
                mutex_elem_type(tp).to_string()
            } else if tp.starts_with("RWLock__") {
                rwlock_elem_type(tp).to_string()
            } else if tp.starts_with("Box__") {
                box_elem_type(tp).to_string()
            } else if tp.starts_with("Guard__") || tp.starts_with("ReadGuard__") || tp.starts_with("WriteGuard__") {
                guard_elem_type(tp).to_string()
            } else {
                shared_elem_type(tp).to_string()
            };
            let resolved = resolve_elem_type(&elem_name, &orig_to_c);
            if is_monomorphized_wrapper_type(&resolved) && type_seen.insert(resolved.clone()) {
                emit_wrapper_typedef(out, &resolved, module, &orig_to_c);
            }
            if type_seen.insert(tp.clone()) {
                emit_wrapper_typedef(out, tp, module, &orig_to_c);
            }
        }
    }
    // Second pass: emit inline wrappers (now that all typedefs are in place).
    for name in &method_calls {
        // Extract the type prefix from whichever wrapper pattern matches.
        let tp = if let Some((tp, _)) = parse_channel_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_shared_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_weak_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_mutex_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_rwlock_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_guard_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_box_method(name) { Some(tp) }
            else { None };
        if let Some(ref tp) = tp {
            if is_unmonomorphized_wrapper(tp) { continue; }
        }
        if let Some((type_prefix, method)) = parse_channel_method(name) {
            let elem = resolve_elem_type(channel_elem_type(&type_prefix), &orig_to_c);
            emit_channel_wrapper(out, &type_prefix, method, &elem);
        } else if let Some((type_prefix, method)) = parse_shared_method(name) {
            let elem = resolve_elem_type(shared_elem_type(&type_prefix), &orig_to_c);
            emit_shared_wrapper(out, &type_prefix, method, &elem);
        } else if let Some((type_prefix, method)) = parse_weak_method(name) {
            emit_weak_wrapper(out, &type_prefix, method, &orig_to_c);
        } else if let Some((type_prefix, method)) = parse_mutex_method(name) {
            let elem = resolve_elem_type(mutex_elem_type(&type_prefix), &orig_to_c);
            emit_mutex_wrapper(out, &type_prefix, method, &elem);
        } else if let Some((type_prefix, method)) = parse_rwlock_method(name) {
            let elem = resolve_elem_type(rwlock_elem_type(&type_prefix), &orig_to_c);
            emit_rwlock_wrapper(out, &type_prefix, method, &elem);
        } else if let Some((type_prefix, method)) = parse_guard_method(name) {
            let elem = resolve_elem_type(guard_elem_type(&type_prefix), &orig_to_c);
            emit_guard_wrapper(out, &type_prefix, method, &elem);
        } else if let Some((type_prefix, method)) = parse_box_method(name) {
            let elem = resolve_elem_type(box_elem_type(&type_prefix), &orig_to_c);
            emit_box_wrapper(out, &type_prefix, method, &elem, module, &orig_to_c);
        }
    }

    writeln!(out).unwrap();
}

/// Scan call names in the LIR module and conditionally include C runtime modules,
/// LIR helper functions, box allocators, and inline shim functions.
///
/// This covers everything that depends on `include_runtime == true`:
/// - Conditional runtime section inclusion (preamble, allocators, collections, async, etc.)
/// - LIR helpers (default value functions, comparators, hash functions)
/// - `__gorget_box_alloc_*` monomorphized box allocators
/// - Inline shims for str/array operations not provided by the C runtime
pub(super) fn emit_runtime_modules(out: &mut String, module: &LirModule, _struct_names: &HashMap<u32, String>) {
    // Scan ALL call names (externs + function names + CallExtern inside bodies)
    // to determine which optional runtime modules are needed.
    let mut all_call_names: Vec<&str> = module.externs.iter().map(|e| e.name.as_str())
        .chain(module.functions.iter().map(|f| f.name.as_str()))
        .collect();
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::CallExtern { name, .. } = inst {
                    all_call_names.push(name.as_str());
                }
            }
        }
    }
    let has = |pred: &dyn Fn(&str) -> bool| all_call_names.iter().any(|n| pred(n));

    // Also check struct names for monomorphized types that need specific runtimes.
    let _has_struct = |name: &str| module.structs.iter().any(|s| s.name == name);

    // ── Minimal preamble (headers, allocator, scoped alloc stubs) ──
    out.push_str(crate::backend::c::c_runtime::RUNTIME_PREAMBLE);

    // ── Conditional allocators ──
    if has(&|n| n.starts_with("gorget_arena_") || n.starts_with("GorgetArena")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_ARENA_ALLOC);
    }
    if has(&|n| n.starts_with("gorget_tracking_")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_TRACKING_ALLOC);
    }
    if has(&|n| n.starts_with("gorget_pool_") || n.starts_with("GorgetPool")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_POOL_ALLOC);
    }
    if has(&|n| n.starts_with("gorget_tlsf_")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_TLSF_ALLOC);
    }
    if has(&|n| n.starts_with("gorget_fba_") || n.starts_with("gorget_fixed_buffer_")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_FIXEDBUF_ALLOC);
    }
    if has(&|n| n.starts_with("gorget_fallback_")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_FALLBACK_ALLOC);
    }

    // ── String types and operations ──
    out.push_str(crate::backend::c::c_runtime::RUNTIME_STRING);

    // Extended string methods (unicode tables, search, split/replace/trim/etc.)
    if has(&|n| n.starts_with("gorget_str_to_upper") || n.starts_with("gorget_str_to_lower")
        || n.starts_with("gorget_str_is_alpha") || n.starts_with("gorget_str_is_upper")
        || n.starts_with("gorget_str_is_lower") || n.starts_with("gorget_str_is_digit")
        || n.starts_with("gorget_str_is_whitespace")
        || n.starts_with("gorget_str_contains") || n.starts_with("gorget_str_starts_with")
        || n.starts_with("gorget_str_ends_with") || n.starts_with("gorget_str_find")
        || n.starts_with("gorget_memmem")
        || n.starts_with("gorget_str_trim") || n.starts_with("gorget_str_replace")
        || n.starts_with("gorget_str_repeat") || n.starts_with("gorget_str_pad")
        || n.starts_with("gorget_str_strip") || n.starts_with("gorget_str_lstrip")
        || n.starts_with("gorget_str_rstrip") || n.starts_with("gorget_str_removeprefix")
        || n.starts_with("gorget_str_removesuffix") || n.starts_with("gorget_str_index_of")
        || n.starts_with("gorget_str_count") || n.starts_with("gorget_str_center")
        || n.starts_with("gorget_str_ljust") || n.starts_with("gorget_str_rjust")
        || n.starts_with("gorget_str_zfill") || n.starts_with("gorget_str_reverse")
        || n.starts_with("gorget_str_encode_") || n.starts_with("gorget_str_decode_")
        || n.starts_with("gorget_base64_") || n.starts_with("gorget_json_escape")
        || n.starts_with("gorget_str_to_json") || n.starts_with("gorget_str_from_json")
        || n.starts_with("gorget_uint8_is_") || n.starts_with("gorget_uint8_to_")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_STRING_EXTENDED);
    }

    // Base string operations (Str-aware concat, append, cstr conversion)
    out.push_str(crate::backend::c::c_runtime::RUNTIME_STRING_BASE_OPS);

    // ── Alloc report (test/bench mode only) ──
    let is_test_or_bench = !module.test_fns.is_empty() || !module.bench_fns.is_empty() || module.is_test_module;
    if is_test_or_bench {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_ALLOC_REPORT);
    }

    // ── Panic handler ──
    if !is_test_or_bench {
        out.push_str(crate::backend::c::c_runtime::PANIC_NORMAL);
    } else {
        out.push_str(crate::backend::c::c_runtime::PANIC_TEST);
    }

    // ── Conditional core sections (formerly RUNTIME_CORE) ──
    // Use flags to track what's been emitted and enforce dependencies.
    let mut emitted_array = false;
    let mut emitted_map = false;

    // Helper macro to emit RUNTIME_ARRAY if not yet emitted
    macro_rules! ensure_array {
        ($out:expr, $flag:expr) => {
            if !$flag {
                $out.push_str(crate::backend::c::c_runtime::RUNTIME_ARRAY);
                $flag = true;
            }
        };
    }
    macro_rules! ensure_map {
        ($out:expr, $aflag:expr, $mflag:expr) => {
            ensure_array!($out, $aflag); // MAP depends on ARRAY
            if !$mflag {
                $out.push_str(crate::backend::c::c_runtime::RUNTIME_MAP);
                $mflag = true;
            }
        };
    }

    // Checked arithmetic (macros used by integer overflow checks)
    if has(&|n| n.starts_with("gorget_checked_") || n.starts_with("GORGET_CHECKED_")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_CHECKED_ARITH);
    }

    // Collections: Array
    if has(&|n| n.starts_with("gorget_array_") || n.starts_with("Vector__")) {
        ensure_array!(out, emitted_array);
    }

    // String/Array operations (join, split, iterators — needs RUNTIME_ARRAY)
    if has(&|n| n.starts_with("gorget_str_join") || n.starts_with("gorget_str_split")
        || n.starts_with("gorget_str_bytes") || n.starts_with("gorget_str_codepoints")
        || n.starts_with("gorget_str_chars")) {
        ensure_array!(out, emitted_array);
        out.push_str(crate::backend::c::c_runtime::RUNTIME_STRING_ARRAY);
    }

    // Collections: Map (depends on Array for keys/values/items)
    if has(&|n| n.starts_with("gorget_map_") || n.starts_with("gorget_dict_")
        || n.starts_with("Dict__") || n.starts_with("HashMap__")) {
        ensure_map!(out, emitted_array, emitted_map);
    }

    // Collections: Set (depends on Map)
    if has(&|n| n.starts_with("gorget_set_") || n.starts_with("Set__") || n.starts_with("HashSet__")) {
        ensure_map!(out, emitted_array, emitted_map);
        out.push_str(crate::backend::c::c_runtime::RUNTIME_SET);
    }

    // Error handling (test/bench mode or explicit catch/throw)
    if is_test_or_bench || has(&|n| n.starts_with("gorget_catch") || n.starts_with("gorget_throw")
        || n.starts_with("gorget_cleanup_")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_ERROR);
    }

    // File I/O (depends on Array for read_file_bytes)
    if has(&|n| n.starts_with("gorget_file_") || n == "gorget_read_file"
        || n == "gorget_write_file" || n == "gorget_append_file"
        || n == "gorget_read_file_bytes"
        || n == "File__open" || n == "File__create") {  // codegen rewrites to gorget_file_open
        ensure_array!(out, emitted_array);
        out.push_str(crate::backend::c::c_runtime::RUNTIME_FILE);
    }

    // Path functions + readdir (depends on Array for readdir)
    if has(&|n| n.starts_with("gorget_path_") || n == "gorget_is_file" || n == "gorget_is_dir"
        || n.starts_with("gorget_mkdir") || n.starts_with("gorget_readdir")
        || n == "gorget_rename" || n == "gorget_copy_file" || n == "gorget_remove"
        || n == "gorget_basename" || n == "gorget_dirname" || n == "gorget_file_size"
        || n == "gorget_file_mtime") {
        ensure_array!(out, emitted_array);
        out.push_str(crate::backend::c::c_runtime::RUNTIME_PATH);
    }

    // CLI args (gorget_args — needs RUNTIME_ARRAY; gorget_init_args is in preamble)
    if has(&|n| n == "gorget_args") {
        ensure_array!(out, emitted_array);
        out.push_str(crate::backend::c::c_runtime::RUNTIME_ARGS);
    }

    // Parsing (also detects int__parse/float__parse codegen patterns)
    if has(&|n| n.starts_with("gorget_parse_int") || n.starts_with("gorget_parse_float")
        || n.starts_with("gorget_try_parse")
        || (n.ends_with("__parse") && (n.starts_with("int") || n.starts_with("uint")
            || n == "double__parse" || n == "float__parse" || n == "bool__parse"))) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_PARSE);
    }

    // to_str conversions
    if has(&|n| n.starts_with("gorget_int_to_str") || n.starts_with("gorget_float_to_str")
        || n.starts_with("gorget_bool_to_str") || n.starts_with("gorget_codepoint_to_utf8")
        || n.starts_with("gorget_char_to_str") || n.starts_with("gorget_int_to_binary")
        || n.starts_with("gorget_int_to_hex") || n.starts_with("gorget_int_to_octal")
        || n.starts_with("gorget_int_to_float") || n.starts_with("gorget_float_to_int")
        || n == "gorget_char_chr") {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_TOSTR);
    }

    // Environment
    if has(&|n| n == "gorget_getenv" || n == "gorget_setenv" || n == "gorget_getcwd"
        || n == "gorget_platform") {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_ENV);
    }

    // Interactive I/O, time, datetime, random, line input (depends on Array for dt_decompose)
    if has(&|n| n.starts_with("gorget_input") || n.starts_with("gorget_rand")
        || n.starts_with("gorget_seed") || n.starts_with("gorget_sleep_ms")
        || n == "sleep_ms"
        || n.starts_with("gorget_time") || n.starts_with("gorget_format_time")
        || n.starts_with("gorget_parse_time") || n.starts_with("gorget_readline")
        || n.starts_with("gorget_dt_decompose") || n.starts_with("gorget_getchar")
        || n.starts_with("gorget_term_") || n == "gorget_is_tty") {
        ensure_array!(out, emitted_array);
        out.push_str(crate::backend::c::c_runtime::RUNTIME_IO);
    }

    // Math
    if has(&|n| n.starts_with("gorget_sqrt") || n.starts_with("gorget_pow")
        || n.starts_with("gorget_floor") || n.starts_with("gorget_ceil")
        || n.starts_with("gorget_round") || n.starts_with("gorget_abs")
        || n.starts_with("gorget_sin") || n.starts_with("gorget_cos")
        || n.starts_with("gorget_tan") || n.starts_with("gorget_log")
        || n.starts_with("gorget_exp") || n.starts_with("gorget_atan2")
        || n.starts_with("gorget_fmod") || n == "gorget_min" || n == "gorget_max"
        || n.starts_with("GORGET_PI") || n.starts_with("GORGET_E")
        || n.starts_with("GORGET_TAU") || n.starts_with("GORGET_INF")
        || n.starts_with("GORGET_NAN")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_MATH);
    }

    // Sort comparators (depends on Array)
    if has(&|n| n.starts_with("__gorget_cmp_") || n.starts_with("gorget_array_sort")
        || n.starts_with("gorget_array_reverse") || n.starts_with("gorget_array_unique")) {
        ensure_array!(out, emitted_array);
        out.push_str(crate::backend::c::c_runtime::RUNTIME_SORT);
    }

    writeln!(out).unwrap();

    // Sync primitives (atomics, barriers, semaphores, etc.)
    let needs_sync = has(&|n| n.starts_with("gorget_atomic_int_") || n.starts_with("gorget_atomic_bool_")) || has(&|n| {
        n.starts_with("gorget_atomic_") || n.starts_with("gorget_barrier_")
        || n.starts_with("gorget_condvar_") || n.starts_with("gorget_rwlock_")
        || n.starts_with("gorget_waitgroup_") || n.starts_with("gorget_semaphore_")
        || n.starts_with("gorget_onceflag_")
        || n.starts_with("gorget_read_guard_") || n.starts_with("gorget_write_guard_")
        || n.starts_with("ReadGuard__") || n.starts_with("WriteGuard__")
    });
    if needs_sync {
        out.push_str(crate::backend::c::c_runtime::SYNC_RUNTIME);
    }

    // Async core
    let needs_async = has(&|n| {
        n.contains("channel") || n.contains("Channel")
        || n.starts_with("gorget_mutex_") || n.starts_with("gorget_guard_")
        || n.starts_with("gorget_executor_") || n == "gorget_spawn"
        || n.starts_with("__gorget_spawn_") || n.starts_with("__gorget_await_")
        || n.starts_with("gorget_task_group_") || n.starts_with("gorget_reactor_")
        || n.starts_with("Mutex__") || n.starts_with("RWLock__")
    });
    if needs_async {
        out.push_str(crate::backend::c::c_runtime::ASYNC_RUNTIME);
        out.push_str(crate::backend::c::c_runtime::TASK_COMMON);
        match module.scheduler_mode {
            crate::ir::SchedulerMode::Pool => out.push_str(crate::backend::c::c_runtime::SCHEDULER_POOL_RUNTIME),
            crate::ir::SchedulerMode::Thread => out.push_str(crate::backend::c::c_runtime::SCHEDULER_THREAD_RUNTIME),
            crate::ir::SchedulerMode::Inline => out.push_str(crate::backend::c::c_runtime::SCHEDULER_INLINE_RUNTIME),
            crate::ir::SchedulerMode::Single => out.push_str(crate::backend::c::c_runtime::SCHEDULER_SINGLE_RUNTIME),
        }
        out.push_str(crate::backend::c::c_runtime::MAIN_WAKER_RUNTIME);
        out.push_str(crate::backend::c::c_runtime::EXECUTOR_RUNTIME);
    }

    // Channels (also triggered by monomorphized Channel__T methods)
    if has(&|n| n.starts_with("gorget_channel_") || n.starts_with("Channel__")) {
        if !needs_async {
            out.push_str(crate::backend::c::c_runtime::ASYNC_RUNTIME);
        }
        out.push_str(crate::backend::c::c_runtime::CHANNEL_RUNTIME);
    }

    // Shared / Weak references (also triggered by monomorphized methods)
    if has(&|n| n.starts_with("gorget_shared_") || n.starts_with("gorget_weak_")
        || n.starts_with("Shared__") || n.starts_with("Weak__")) {
        out.push_str(crate::backend::c::c_runtime::SHARED_RUNTIME);
    }

    // Mutex / Guard (also triggered by Mutex__T monomorphized methods)
    if has(&|n| n.starts_with("gorget_mutex_") || n.starts_with("gorget_guard_")
        || n.starts_with("Mutex__") || n.starts_with("RWLock__")
        || n.starts_with("Guard__") || n.starts_with("ReadGuard__") || n.starts_with("WriteGuard__")
        || n.starts_with("gorget_rwlock_") || n.starts_with("gorget_read_guard_")
        || n.starts_with("gorget_write_guard_"))
    {
        if !needs_async {
            out.push_str(crate::backend::c::c_runtime::ASYNC_RUNTIME);
        }
        out.push_str(crate::backend::c::c_runtime::MUTEX_RUNTIME);
    }

    // Reactor (async I/O, sleep, timers)
    if has(&|n| n.starts_with("gorget_reactor_") || n.starts_with("gorget_sleep_async")) {
        out.push_str(crate::backend::c::c_runtime::REACTOR_RUNTIME);
    }

    // Blocking pool — also needed for spawned functions (blocking spawn approach)
    if has(&|n| n.starts_with("gorget_blocking_")) || !module.spawned_fns.is_empty() {
        out.push_str(crate::backend::c::c_runtime::BLOCKING_POOL_RUNTIME);
    }

    // Task groups
    if has(&|n| n.starts_with("gorget_task_group_")) {
        out.push_str(crate::backend::c::c_runtime::TASK_GROUP_RUNTIME);
    }

    // Bytes
    if has(&|n| n.starts_with("gorget_bytes_")) {
        out.push_str(crate::backend::c::c_runtime::BYTES_RUNTIME);
    }

    // Regex
    if has(&|n| n.starts_with("gorget_regex_") || n.starts_with("gorget_match_")) {
        out.push_str(crate::backend::c::c_runtime::REGEX_RUNTIME);
        // Forward-declare gorget_array_new for regex_split_pat.
        out.push_str("static inline GorgetArray gorget_array_new(size_t elem_size);\n");
        // Convenience wrappers for pattern-based regex operations.
        out.push_str(r#"
static GorgetRegexMatch gorget_regex_find_pat(const char* pattern, const char* subject) {
    GorgetRegex _rx = gorget_regex_compile(pattern, NULL);
    if (!_rx.code) { GorgetRegexMatch _m; _m.start = -1; return _m; }
    GorgetRegexMatch _m = gorget_regex_find(&_rx, subject, 0);
    gorget_regex_free(&_rx);
    return _m;
}
static bool gorget_regex_is_match_pat(const char* pattern, const char* subject) {
    GorgetRegex _rx = gorget_regex_compile(pattern, NULL);
    if (!_rx.code) return false;
    bool _b = gorget_regex_is_match(&_rx, subject);
    gorget_regex_free(&_rx);
    return _b;
}
static GorgetString gorget_regex_replace_pat(const char* pattern, const char* subject, const char* replacement) {
    GorgetRegex _rx = gorget_regex_compile(pattern, NULL);
    if (!_rx.code) return gorget_string_new(subject);
    GorgetString _gs = gorget_regex_replace(&_rx, subject, replacement);
    gorget_regex_free(&_rx);
    return _gs;
}
static GorgetArray gorget_regex_split_pat(const char* pattern, const char* subject, int64_t limit) {
    GorgetRegex _rx = gorget_regex_compile(pattern, NULL);
    if (!_rx.code) { GorgetArray _a = gorget_array_new(sizeof(Str)); return _a; }
    GorgetArray _a = gorget_regex_split(&_rx, subject, limit);
    gorget_regex_free(&_rx);
    return _a;
}
"#);
    }

    // Crypto
    if has(&|n| n.starts_with("gorget_crypto_") || n.starts_with("gorget_sha") || n.starts_with("gorget_hmac") || n.starts_with("gorget_x25519") || n.starts_with("gorget_hkdf") || n.starts_with("gorget_aead")) {
        out.push_str(crate::backend::c::c_runtime::CRYPTO_RUNTIME);
    }

    // Socket (depends on Array for socket_read/read_exact)
    if has(&|n| n.starts_with("gorget_socket_") || n.starts_with("gorget_tcp_")) {
        ensure_array!(out, emitted_array);
        out.push_str(crate::backend::c::c_runtime::SOCKET_RUNTIME);
    }

    // Server socket (depends on Array)
    if has(&|n| n.starts_with("gorget_server_socket_") || n.starts_with("gorget_listener_")) {
        ensure_array!(out, emitted_array);
        out.push_str(crate::backend::c::c_runtime::SERVER_SOCKET_RUNTIME);
    }

    // UDP socket
    if has(&|n| n.starts_with("gorget_udp_")) {
        out.push_str(crate::backend::c::c_runtime::UDP_SOCKET_RUNTIME);
    }

    // TLS
    if has(&|n| n.starts_with("gorget_tls_")) {
        out.push_str(crate::backend::c::c_runtime::TLS_SOCKET_RUNTIME);
        out.push_str(crate::backend::c::c_runtime::TLS_SERVER_RUNTIME);
    }

    // Process spawn (fork+exec with pipes) + signal handling (signal functions live in PROCESS_SPAWN_RUNTIME)
    let needs_spawn = has(&|n| n.starts_with("gorget_process_spawn") || n.starts_with("gorget_process_wait")
        || n.starts_with("gorget_process_kill") || n.starts_with("gorget_process_pid")
        || n.starts_with("gorget_process_read_") || n.starts_with("gorget_process_write_")
        || n.starts_with("gorget_process_close_")
        || n.starts_with("gorget_signal_") || n == "gorget_getpid");

    // Process — also needed when spawn is used (ExecResult typedef lives here)
    if needs_spawn || has(&|n| n.starts_with("gorget_process_") || n.starts_with("gorget_exec_") || n == "gorget_getenv" || n == "gorget_setenv") {
        out.push_str(crate::backend::c::c_runtime::PROCESS_RUNTIME);
    }

    if needs_spawn {
        ensure_array!(out, emitted_array); // gorget_process_spawn uses gorget_array_get
        out.push_str(crate::backend::c::c_runtime::PROCESS_SPAWN_RUNTIME);
    }

    // Thread
    if has(&|n| n.starts_with("gorget_thread_") || n.starts_with("gorget_current_thread_id")
        || n.starts_with("__gorget_thread_spawn_")) || !module.thread_spawned_fns.is_empty() {
        out.push_str(crate::backend::c::c_runtime::THREAD_RUNTIME);
    }

    // Trace
    if module.trace_filename.is_some() || has(&|n| n.starts_with("gorget_trace_")) {
        out.push_str(crate::backend::c::c_runtime::TRACE_RUNTIME);
    }

    // SDL
    if has(&|n| n.starts_with("sdl_") || n.starts_with("gorget_sdl_")) {
        if has(&|n| n == "sdl_load_texture" || n == "gorget_sdl_load_texture") {
            out.push_str("#define GORGET_USE_SDL_IMAGE\n");
        }
        if has(&|n| n == "sdl_load_font" || n == "sdl_close_font" || n == "sdl_draw_text"
            || n == "sdl_render_text" || n == "sdl_text_width" || n == "sdl_text_height"
            || n.starts_with("gorget_sdl_load_font") || n.starts_with("gorget_sdl_draw_text")
            || n.starts_with("gorget_sdl_render_text")) {
            out.push_str("#define GORGET_USE_SDL_TTF\n");
        }
        out.push_str(crate::backend::c::c_runtime::SDL_RUNTIME);
    }

    // Bytes f32/f64/i64 helpers
    if has(&|n| n.starts_with("gorget_bytes_") && (n.contains("f32") || n.contains("f64") || n.contains("i64"))) {
        out.push_str(crate::backend::c::c_runtime::BYTES_F32_RUNTIME);
    }

    // OpenGL
    if has(&|n| n.starts_with("gorget_gl_")) {
        out.push_str(crate::backend::c::c_runtime::GL_RUNTIME);
    }

    // Image loading (stb_image)
    if has(&|n| n.starts_with("gorget_image_")) {
        out.push_str("\n#define STB_IMAGE_IMPLEMENTATION\n");
        out.push_str("#define STBI_NO_STDIO\n");
        out.push_str("#define STBI_ONLY_PNG\n");
        out.push_str("#define STBI_ONLY_JPEG\n");
        out.push_str("#define STBI_ONLY_TGA\n");
        out.push_str("#define STBI_ONLY_BMP\n");
        out.push_str("#define GORGET_HAS_STB_IMAGE 1\n");
        out.push_str("#pragma GCC diagnostic push\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wunused-function\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wunused-parameter\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wsign-compare\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wshift-negative-value\"\n");
        out.push_str(crate::backend::c::c_runtime::STB_IMAGE_SOURCE);
        out.push_str("\n#pragma GCC diagnostic pop\n");
        out.push_str(crate::backend::c::c_runtime::IMAGE_RUNTIME);
    }

    // Audio (SDL2_mixer)
    if has(&|n| n.starts_with("gorget_audio_")) {
        out.push_str(crate::backend::c::c_runtime::AUDIO_RUNTIME);
    }

    // Compression (zlib/deflate)
    if has(&|n| n.starts_with("gorget_zlib_") || n.starts_with("gorget_deflate_") || n.starts_with("gorget_crc32_")) {
        out.push_str(crate::backend::c::c_runtime::COMPRESS_RUNTIME);
    }

    // Metal (macOS Objective-C wrappers)
    if has(&|n| n.starts_with("gorget_metal_") || n.starts_with("gorget_sdl_metal_")) {
        out.push_str(crate::backend::c::c_runtime::METAL_RUNTIME);
    }

    // SQLite
    let needs_sqlite = has(&|n| n.starts_with("gorget_sqlite_") || n == "sqlite_open");
    if needs_sqlite {
        out.push_str("\n#define SQLITE_MAX_MMAP_SIZE 0\n");
        out.push_str("#define HAVE_MREMAP 0\n");
        out.push_str("#pragma GCC diagnostic push\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wunused-parameter\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wunused-variable\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wunused-function\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wimplicit-fallthrough\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wpedantic\"\n");
        out.push_str(crate::backend::c::c_runtime::SQLITE_AMALGAMATION);
        out.push_str("\n#pragma GCC diagnostic pop\n");
        out.push_str(crate::backend::c::c_runtime::SQLITE_GORGET_WRAPPERS);
    }

    // Hot-reload runtime (dlopen/file-watcher helpers)
    if module.hot_reload {
        out.push_str(crate::backend::c::c_runtime::HOT_RELOAD_RUNTIME);
    }

    // Suppress "value never read" warnings on idempotent emit-once flags.
    let _ = (emitted_array, emitted_map);

    emit_lir_helpers(out, module);
}

/// LIR-specific helper functions: char operations, hash, default values,
/// comparison functions for sorted(), etc. Called from both emit_runtime_modules
/// and generate_llvm_wrappers.
pub(super) fn emit_lir_helpers(out: &mut String, module: &LirModule) {
    let has = |pred: &dyn Fn(&str) -> bool| -> bool {
        module.externs.iter().any(|e| pred(&e.name))
            || module.functions.iter().flat_map(|f| f.blocks.iter())
                .flat_map(|b| b.insts.iter())
                .any(|inst| matches!(inst, Inst::CallExtern { name, .. } if pred(name)))
    };

    writeln!(out, "// ── LIR helpers ──").unwrap();
    if has(&|n| n == "gorget_char_chr") {
        writeln!(out, "static inline Str gorget_char_chr(int64_t code) {{ return gorget_codepoint_to_utf8(code); }}").unwrap();
    }
    if has(&|n| n == "gorget_str_ord") {
        writeln!(out, "static inline int64_t gorget_str_ord(Str s) {{ size_t pos = 0; return (int64_t)gorget_utf8_decode((const char*)s.data, s.len, &pos); }}").unwrap();
    }
    // Default value functions for primitive types
    writeln!(out, "static inline Str gorget_str_default(void) {{ return GORGET_EMPTY_STR; }}").unwrap();
    writeln!(out, "static inline int64_t int64_t__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline int64_t int__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline int8_t int8_t__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline int16_t int16_t__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline int32_t int32_t__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline uint8_t uint8_t__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline uint16_t uint16_t__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline uint32_t uint32_t__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline uint64_t uint64_t__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline double double__default(void) {{ return 0.0; }}").unwrap();
    writeln!(out, "static inline double float__default(void) {{ return 0.0; }}").unwrap();
    writeln!(out, "static inline bool bool__default(void) {{ return false; }}").unwrap();
    // Hash functions
    writeln!(out, "static inline int64_t __gorget_hash_int(int64_t v) {{ return (int64_t)__gorget_fnv1a(&v, sizeof(v)); }}").unwrap();
    writeln!(out, "static inline int64_t gorget_str_hash(Str s) {{ return (int64_t)__gorget_hash_str_len((const char*)s.data, s.len); }}").unwrap();
    // Signal functions — defined in the main runtime (c_runtime.rs).
    // Only emit minimal stubs when the runtime signal module is NOT included.
    writeln!(out, "#ifndef _WIN32").unwrap();
    writeln!(out, "#include <signal.h>").unwrap();
    writeln!(out, "#endif").unwrap();
    // Comparison functions for sorted()
    writeln!(out, "static int gorget_generic_compare(const void* a, const void* b) {{ return memcmp(a, b, sizeof(int64_t)); }}").unwrap();
    writeln!(out, "static int gorget_int_compare(const void* a, const void* b) {{ int64_t va = *(const int64_t*)a, vb = *(const int64_t*)b; return (va > vb) - (va < vb); }}").unwrap();
    writeln!(out, "static int gorget_float_compare(const void* a, const void* b) {{ double da = *(const double*)a, db = *(const double*)b; return (da > db) - (da < db); }}").unwrap();
    writeln!(out, "static int gorget_str_compare(const void* a, const void* b) {{ Str sa = *(const Str*)a, sb = *(const Str*)b; size_t la = sa.len, lb = sb.len; size_t ml = la < lb ? la : lb; int r = ml > 0 ? memcmp(sa.data, sb.data, ml) : 0; if (r) return r; return (la > lb) - (la < lb); }}").unwrap();
    writeln!(out, "static inline int64_t int64_t__one(void) {{ return 1; }}").unwrap();
    writeln!(out, "static inline int64_t int__one(void) {{ return 1; }}").unwrap();
    writeln!(out, "static inline double double__one(void) {{ return 1.0; }}").unwrap();
    writeln!(out, "static inline double float__one(void) {{ return 1.0; }}").unwrap();

    // UTF-8 codepoint helpers (normally in emit_runtime_modules)
    if has(&|n| n == "gorget_utf8_codepoint_len_at") {
        writeln!(out, "static inline int64_t gorget_utf8_codepoint_len_at(Str s, int64_t byte_pos) {{ \
            if (byte_pos < 0 || byte_pos >= (int64_t)s.len) return 0; \
            return (int64_t)gorget_utf8_codepoint_len(((const unsigned char*)s.data)[byte_pos]); }}").unwrap();
    }
    if has(&|n| n == "gorget_str_codepoint_at") {
        writeln!(out, "static inline Str gorget_str_codepoint_at(Str s, int64_t byte_pos) {{ \
            if (byte_pos < 0 || byte_pos >= (int64_t)s.len) return GORGET_EMPTY_STR; \
            int cplen = gorget_utf8_codepoint_len(((const unsigned char*)s.data)[byte_pos]); \
            if (byte_pos + cplen > (int64_t)s.len) cplen = (int)(s.len - (size_t)byte_pos); \
            return gorget_str_own_region((const char*)s.data + byte_pos, (size_t)cplen); }}").unwrap();
    }
    // gorget_signal_ignore is already in the C runtime — no duplicate emission needed.

    // gorget_task_group_submit is a MACRO in the runtime, not a function.
    // The LLVM/C backend calls it as a function with (TaskGroup*, Task__T) args.
    // Every `Task__T` has layout { void* __task, void(*__drop)(void*) }, so we
    // emit a replacement that receives the task by address (cast to void*) and
    // reads the two fields through that pointer. This is struct-type-agnostic:
    // it works uniformly for Task__void / Task__int / Task__String / etc.,
    // avoiding C's nominal-type rejection when a concrete Task__T is passed
    // where the function signature nominally wants __TaskHandle.
    if has(&|n| n == "gorget_task_group_submit") {
        writeln!(out, "#undef gorget_task_group_submit").unwrap();
        writeln!(out, "#define gorget_task_group_submit(g, task) do {{ \\").unwrap();
        writeln!(out, "    gorget_task_group_submit_raw((g), (task).__task, (task).__drop); \\").unwrap();
        writeln!(out, "    (task).__task = NULL; \\").unwrap();
        writeln!(out, "}} while(0)").unwrap();
    }

    writeln!(out).unwrap();
}

/// Emit `__gorget_box_alloc_*` monomorphized box allocators and inline shim
/// functions for str/array operations that supplement the C runtime.
pub(super) fn emit_runtime_helpers(out: &mut String, module: &LirModule, struct_names: &HashMap<u32, String>) {
    // Generate __gorget_box_alloc_* helper functions.
    // These are monomorphized box allocators: malloc + store + return pointer.
    let mut box_allocs: Vec<(&str, String)> = Vec::new();
    for ext in &module.externs {
        if ext.name.starts_with("__gorget_box_alloc_") && ext.params.len() == 1 {
            // Derive the C type from the function name suffix, not from the LIR param type,
            // because LIR represents Str as Ptr (void*) but the C box alloc needs the real type.
            let suffix = &ext.name["__gorget_box_alloc_".len()..];
            let param_ty = box_alloc_inner_c_type(suffix, &ext.params[0], struct_names);
            box_allocs.push((&ext.name, param_ty));
        }
    }
    // Also scan CallExtern instructions for box allocs not in externs list.
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::CallExtern { name, args, .. } = inst {
                    if name.starts_with("__gorget_box_alloc_") && args.len() == 1 {
                        if !box_allocs.iter().any(|(n, _)| *n == name.as_str()) {
                            let suffix = &name["__gorget_box_alloc_".len()..];
                            let param_ty = box_alloc_suffix_to_c_type(suffix);
                            box_allocs.push((name.as_str(), param_ty));
                        }
                    }
                }
            }
        }
    }
    for (name, param_ty) in &box_allocs {
        writeln!(out, "static inline void* {name}({param_ty} val) {{ {param_ty}* p = ({param_ty}*)GORGET_ALLOC(sizeof({param_ty})); *p = val; return (void*)p; }}").unwrap();
    }
    if !box_allocs.is_empty() {
        writeln!(out).unwrap();
    }

    // Generate gorget_str_push/gorget_str_str/gorget_str_clear if called but not in runtime.
    let has_extern = |n: &str| module.externs.iter().any(|e| e.name == n)
        || module.functions.iter().flat_map(|f| f.blocks.iter())
            .flat_map(|b| b.insts.iter())
            .any(|inst| matches!(inst, Inst::CallExtern { name, .. } if name == n));
    if has_extern("gorget_str_push") {
        writeln!(out, "static inline void gorget_str_push(GorgetString* s, Str chunk) {{ gorget_string_push_char(s, chunk); }}").unwrap();
    }
    if has_extern("gorget_str_str") {
        // gorget_str_str: extract the immutable string from a builder.
        // Must clone because the builder and the result are separate owned strings.
        writeln!(out, "static inline Str gorget_str_str(GorgetString* s) {{ return gorget_string_clone_to_owned(s); }}").unwrap();
    }
    if has_extern("gorget_str_clear") {
        // Reset the len to 0 but keep the owned buffer for reuse. For views, len=0 is fine —
        // they'll re-materialize on next push/append.
        writeln!(out, "static inline void gorget_str_clear(GorgetString* s) {{ s->len = 0; if (s->cap > 0 && s->data) ((char*)s->data)[0] = '\\0'; }}").unwrap();
    }
    if has_extern("gorget_str_push_line") {
        writeln!(out, "static inline void gorget_str_push_line(GorgetString* s, Str chunk) {{ gorget_string_push_char(s, chunk); gorget_string_push_byte(s, '\\n'); }}").unwrap();
    }
    if has_extern("gorget_str_capacity") {
        writeln!(out, "static inline int64_t gorget_str_capacity(GorgetString* s) {{ return (int64_t)s->cap; }}").unwrap();
    }
    if has_extern("gorget_str_push_char") {
        writeln!(out, "static inline void gorget_str_push_char(GorgetString* s, Str c) {{ gorget_string_push_char(s, c); }}").unwrap();
    }
    if has_extern("gorget_array_sort") {
        // Thread-local to prevent data races when two threads sort concurrently.
        writeln!(out, "static _Thread_local size_t __gorget_sort_elem_size;").unwrap();
        writeln!(out, "static int __gorget_sort_cmp(const void* a, const void* b) {{ return memcmp(a, b, __gorget_sort_elem_size); }}").unwrap();
        writeln!(out, "static inline void gorget_array_sort(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; __gorget_sort_elem_size = a->elem_size; qsort(a->data, a->len, a->elem_size, __gorget_sort_cmp); }}").unwrap();
    }
    if has_extern("gorget_array_sorted") {
        writeln!(out, "static inline GorgetArray gorget_array_sorted(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; GorgetArray r = gorget_array_clone(a); qsort(r.data, r.len, r.elem_size, gorget_generic_compare); return r; }}").unwrap();
    }
    // gorget_array_reversed: clone + reverse (not in runtime, inlined by old backend)
    if has_extern("gorget_array_reversed") {
        writeln!(out, "static inline GorgetArray gorget_array_reversed(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; GorgetArray r = gorget_array_clone(a); gorget_array_reverse(&r); return r; }}").unwrap();
    }
    // gorget_array_unique: clone + sort + dedup (matches GIR backend semantics)
    if has_extern("gorget_array_unique") {
        writeln!(out, "static inline GorgetArray gorget_array_unique(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; GorgetArray r = gorget_array_clone(a); qsort(r.data, r.len, r.elem_size, gorget_generic_compare); gorget_array_dedup(&r); return r; }}").unwrap();
    }
    // gorget_array_zip: pair elements from two arrays into an array of tuples
    if has_extern("gorget_array_zip") {
        // Tuple struct: { _0: A, _1: B }.  We compute tuple_size from the two elem_sizes.
        // Both fields are at least 8-byte aligned in Gorget, so offset_1 = round_up(a_size, 8).
        writeln!(out, "static inline GorgetArray gorget_array_zip(void* __arr_ptr, GorgetArray __b) {{ \
            GorgetArray* __a = (GorgetArray*)__arr_ptr; \
            size_t __min = __a->len < __b.len ? __a->len : __b.len; \
            size_t __a_sz = __a->elem_size; \
            size_t __b_sz = __b.elem_size; \
            size_t __off1 = (__a_sz + 7) & ~(size_t)7; \
            size_t __tuple_sz = __off1 + ((__b_sz + 7) & ~(size_t)7); \
            GorgetArray __r = gorget_array_new(__tuple_sz); \
            char __sbuf[256]; \
            char* __buf = __tuple_sz <= sizeof(__sbuf) ? __sbuf : (char*)malloc(__tuple_sz); \
            for (size_t __i = 0; __i < __min; __i++) {{ \
                memset(__buf, 0, __tuple_sz); \
                memcpy(__buf, (char*)__a->data + __i * __a_sz, __a_sz); \
                memcpy(__buf + __off1, (char*)__b.data + __i * __b_sz, __b_sz); \
                gorget_array_push(&__r, __buf); \
            }} \
            if (__buf != __sbuf) free(__buf); \
            return __r; }}").unwrap();
    }
    // codepoint_to_str: used by encoding/toml fixtures
    if has_extern("codepoint_to_str") {
        writeln!(out, "static inline Str codepoint_to_str(int64_t code) {{ return gorget_codepoint_to_utf8(code); }}").unwrap();
    }
    // NOTE: int64_t__parse, double__parse etc. are monomorphized parse methods.
    // They're too complex to emit as inline C here due to GorgetParseIntResult types
    // and Option struct name mismatches. They remain as link errors for now.
}
