//! Helper functions used across C LIR emission.

use super::*;

/// Parse `Channel__int64_t__send` → Some(("Channel__int64_t", "send"))
pub(super) fn parse_channel_method(name: &str) -> Option<(String, &str)> {
    if !name.starts_with("Channel__") { return None; }
    let rest = &name["Channel__".len()..];
    // Find the last `__` that separates the type from the method.
    let method_sep = rest.rfind("__")?;
    let type_part = &rest[..method_sep];
    let method = &rest[method_sep + 2..];
    Some((format!("Channel__{type_part}"), method))
}

/// Parse `Shared__int64_t__get` → Some(("Shared__int64_t", "get"))
pub(super) fn parse_shared_method(name: &str) -> Option<(String, &str)> {
    if !name.starts_with("Shared__") { return None; }
    let rest = &name["Shared__".len()..];
    let method_sep = rest.rfind("__")?;
    let type_part = &rest[..method_sep];
    let method = &rest[method_sep + 2..];
    Some((format!("Shared__{type_part}"), method))
}

/// Parse `Weak__int64_t__upgrade` → Some(("Weak__int64_t", "upgrade"))
pub(super) fn parse_weak_method(name: &str) -> Option<(String, &str)> {
    if !name.starts_with("Weak__") { return None; }
    let rest = &name["Weak__".len()..];
    let method_sep = rest.rfind("__")?;
    let type_part = &rest[..method_sep];
    let method = &rest[method_sep + 2..];
    Some((format!("Weak__{type_part}"), method))
}

/// Parse `Mutex__int64_t__lock` → Some(("Mutex__int64_t", "lock"))
pub(super) fn parse_mutex_method(name: &str) -> Option<(String, &str)> {
    if !name.starts_with("Mutex__") { return None; }
    let rest = &name["Mutex__".len()..];
    let method_sep = rest.rfind("__")?;
    let type_part = &rest[..method_sep];
    let method = &rest[method_sep + 2..];
    Some((format!("Mutex__{type_part}"), method))
}

/// Parse `RWLock__int64_t__read` → Some(("RWLock__int64_t", "read"))
pub(super) fn parse_rwlock_method(name: &str) -> Option<(String, &str)> {
    if !name.starts_with("RWLock__") { return None; }
    let rest = &name["RWLock__".len()..];
    let method_sep = rest.rfind("__")?;
    let type_part = &rest[..method_sep];
    let method = &rest[method_sep + 2..];
    Some((format!("RWLock__{type_part}"), method))
}

/// Map a Channel wrapper type name to its C element type.
/// `Channel__int64_t` → `int64_t`, `Channel__Str` → `Str`
pub(super) fn channel_elem_type(type_name: &str) -> &str {
    type_name.strip_prefix("Channel__").unwrap_or("int64_t")
}

/// Map a Shared/Weak wrapper type name to its C element type.
pub(super) fn shared_elem_type(type_name: &str) -> &str {
    type_name.strip_prefix("Shared__")
        .or_else(|| type_name.strip_prefix("Weak__"))
        .unwrap_or("int64_t")
}

pub(super) fn mutex_elem_type(type_name: &str) -> &str {
    type_name.strip_prefix("Mutex__").unwrap_or("int64_t")
}

pub(super) fn rwlock_elem_type(type_name: &str) -> &str {
    type_name.strip_prefix("RWLock__").unwrap_or("int64_t")
}

pub(super) fn guard_elem_type(type_name: &str) -> &str {
    type_name.strip_prefix("Guard__")
        .or_else(|| type_name.strip_prefix("ReadGuard__"))
        .or_else(|| type_name.strip_prefix("WriteGuard__"))
        .unwrap_or("int64_t")
}

pub(super) fn parse_guard_method(name: &str) -> Option<(String, &str)> {
    let (prefix, rest) = if let Some(r) = name.strip_prefix("Guard__") {
        ("Guard__", r)
    } else if let Some(r) = name.strip_prefix("ReadGuard__") {
        ("ReadGuard__", r)
    } else if let Some(r) = name.strip_prefix("WriteGuard__") {
        ("WriteGuard__", r)
    } else {
        return None;
    };
    let method_sep = rest.rfind("__")?;
    let type_part = &rest[..method_sep];
    let method = &rest[method_sep + 2..];
    Some((format!("{prefix}{type_part}"), method))
}
pub(super) fn emit_guard_wrapper(out: &mut String, type_name: &str, method: &str, elem: &str) {
    let is_read_guard = type_name.starts_with("ReadGuard__");
    let is_write_guard = type_name.starts_with("WriteGuard__");
    match method {
        "drop" => {
            let release_fn = if is_read_guard { "gorget_read_guard_release" }
                else if is_write_guard { "gorget_write_guard_release" }
                else { "gorget_guard_release" };
            writeln!(out, "static inline void {type_name}__{method}({type_name}* self) {{ {release_fn}(self); }}").unwrap();
        }
        "get" => {
            let get_fn = if is_read_guard { "gorget_read_guard_get" }
                else if is_write_guard { "gorget_write_guard_get" }
                else { "gorget_guard_get" };
            writeln!(out, "static inline {elem} {type_name}__get({type_name}* self) {{ return *({elem}*){get_fn}(self); }}").unwrap();
        }
        "get_ptr" => {
            let get_fn = if is_read_guard { "gorget_read_guard_get_ptr" }
                else if is_write_guard { "gorget_write_guard_get_ptr" }
                else { "gorget_guard_get_ptr" };
            writeln!(out, "static inline {elem}* {type_name}__get_ptr({type_name}* self) {{ return ({elem}*){get_fn}(self); }}").unwrap();
        }
        "set" => {
            let set_fn = if is_write_guard { "gorget_write_guard_set" }
                else { "gorget_guard_set" };
            writeln!(out, "static inline void {type_name}__set({type_name}* self, {elem} val) {{ {set_fn}(self, &val, sizeof({elem})); }}").unwrap();
        }
        _ => {}
    }
}

/// Emit a typedef for a monomorphized wrapper type.
pub(super) fn emit_wrapper_typedef(out: &mut String, name: &str, module: &LirModule, orig_to_c: &HashMap<String, String>) {
    // SSoT routing: consult `compiler/data/resources.gg`. Two cases land
    // here without any prefix-arm fallback:
    //   (a) collection rows (Vector/Deque/Heap/Dict/Set) — gated on
    //       `collection_kind`; the table's `runtime_name` doubles as the
    //       C typedef target for these struct-by-value resources.
    //   (b) ref-counted-handle + guard rows
    //       (Channel/Shared/Weak/Mutex/RWLock/Guard/ReadGuard/WriteGuard)
    //       — the table's `c_typedef_name` field overrides `runtime_name`
    //       when the C typedef target diverges (e.g. `runtime_name="Mutex"`
    //       but C typedef target `GorgetMutex*`).
    //
    // Box__ stays below — its emission branches on the LIR `is_trait_box`
    // flag (trait box → `<Trait>_TraitObj`, regular box → `void*`), which
    // can't be encoded as a static schema field today.
    if let Some(meta) = crate::resources::table().lookup(name) {
        use crate::resource_schema::CollectionKind;
        let is_collection = matches!(meta.collection_kind,
            CollectionKind::Vector | CollectionKind::Deque | CollectionKind::Heap
            | CollectionKind::Dict
            | CollectionKind::OrderedSet | CollectionKind::HashSet);
        if is_collection {
            writeln!(out, "typedef {} {name};", meta.runtime_name).unwrap();
            return;
        }
        // Ref-counted-handle / guard rows: c_typedef_name carries the C
        // typedef target. Only fires when Some — None falls through to
        // the legacy arms below (Box__, TaskGroup, AtomicInt/Bool).
        if let Some(target) = meta.c_typedef_name.as_deref() {
            writeln!(out, "typedef {target} {name};").unwrap();
            return;
        }
    }
    if name == "TaskGroup" {
        writeln!(out, "typedef gorget_task_group_t* TaskGroup;").unwrap();
    } else if name.starts_with("Box__") {
        // Read the typed `is_trait_box` flag set at registration time
        // (commit e5de1616). Trait-object Box typedef'd to <Trait>_TraitObj
        // (16 bytes: data + vtable); concrete Box typedef'd to void* (8 bytes).
        // Can't migrate via c_typedef_name today: the trait-box branch's
        // target is computed from the trait name, not a static field.
        let is_trait = module.structs.iter()
            .find(|s| s.name == name)
            .map_or(false, |s| s.is_trait_box);
        if is_trait {
            let trait_name = name.strip_prefix("Box__").unwrap();
            let traitobj_orig = format!("{trait_name}_TraitObj");
            let traitobj_cname = orig_to_c.get(&traitobj_orig).cloned().unwrap_or(traitobj_orig);
            writeln!(out, "typedef {traitobj_cname} {name};").unwrap();
        } else {
            writeln!(out, "typedef void* {name};").unwrap();
        }
    } else if name == "AtomicInt" {
        writeln!(out, "typedef GorgetAtomicInt* AtomicInt;").unwrap();
    } else if name == "AtomicBool" {
        writeln!(out, "typedef GorgetAtomicBool* AtomicBool;").unwrap();
    }
}

/// Resolve an element type name to its C name via the orig_to_c map.
/// Primitive types (int64_t, Str, bool, etc.) pass through unchanged.
pub(super) fn resolve_elem_type(name: &str, orig_to_c: &HashMap<String, String>) -> String {
    orig_to_c.get(name).cloned().unwrap_or_else(|| name.to_string())
}

/// For Shared__Vector__T, extract the inner element type T from the type_name.
/// E.g., Shared__Vector__int64_t → int64_t, Shared__Vector__double → double.
/// If not a Shared__Vector pattern, returns `elem` unchanged (fallback).
pub(super) fn shared_vector_inner_elem(type_name: &str, elem: &str) -> String {
    if let Some(rest) = type_name.strip_prefix("Shared__Vector__") {
        // rest is e.g. "int64_t", "double", "bool", "GorgetString"
        rest.to_string()
    } else {
        elem.to_string()
    }
}

/// Returns true if the wrapper type contains an unmonomorphized type parameter (like T, U).
pub(super) fn is_unmonomorphized_wrapper(type_name: &str) -> bool {
    // Check if the element part after the wrapper prefix is a bare type variable
    for prefix in &["Shared__", "Channel__", "Mutex__", "RWLock__", "Guard__", "ReadGuard__", "WriteGuard__", "Box__", "Weak__"] {
        if let Some(rest) = type_name.strip_prefix(prefix) {
            if rest == "T" || rest == "U" || rest == "V" {
                return true;
            }
            // Also check Vector__T etc.
            if rest.starts_with("Vector__") {
                // vector-only-by-design: `is_unmonomorphized_wrapper` guards
                // against un-monomorphized `Shared[Vector[T]]`-style inner-bare-
                // T wrappers reaching code-emit. Historical scout for the sibling
                // Deque case (Round XXVII Track B Site 3) found the wrapper
                // path is already blocked upstream for `Shared[Deque[T]]`, so
                // no live repro exists here — the Vector arm was retained as a
                // defensive fossil for the specific Shared__Vector path.
                let inner = rest.strip_prefix("Vector__").unwrap_or("");
                if inner == "T" || inner == "U" || inner == "V" {
                    return true;
                }
            }
        }
    }
    false
}

pub(super) fn emit_channel_wrapper(out: &mut String, type_name: &str, method: &str, elem: &str) {
    match method {
        "new" => writeln!(out, "static inline {type_name} {type_name}__new(int64_t cap) {{ return gorget_channel_new((size_t)cap, sizeof({elem})); }}").unwrap(),
        "send" => writeln!(out, "static inline void {type_name}__send({type_name}* self, {elem} val) {{ gorget_channel_send(*self, &val); }}").unwrap(),
        "recv" => writeln!(out, "static inline {elem} {type_name}__recv({type_name}* self) {{ {elem} __val; gorget_channel_recv(*self, &__val); return __val; }}").unwrap(),
        "close" => writeln!(out, "static inline void {type_name}__close({type_name}* self) {{ gorget_channel_close(*self); }}").unwrap(),
        "len" => writeln!(out, "static inline int64_t {type_name}__len({type_name}* self) {{ return gorget_channel_len(*self); }}").unwrap(),
        "capacity" => writeln!(out, "static inline int64_t {type_name}__capacity({type_name}* self) {{ return gorget_channel_capacity(*self); }}").unwrap(),
        "is_closed" => writeln!(out, "static inline bool {type_name}__is_closed({type_name}* self) {{ return gorget_channel_is_closed(*self); }}").unwrap(),
        "poll_send" => writeln!(out, "static inline bool {type_name}__poll_send({type_name}* self, {elem} val, GorgetWaker* waker) {{ return gorget_channel_poll_send(*self, &val, waker); }}").unwrap(),
        "poll_recv" => writeln!(out, "static inline bool {type_name}__poll_recv({type_name}* self, {elem}* outval, GorgetWaker* waker) {{ return gorget_channel_poll_recv(*self, outval, waker); }}").unwrap(),
        "recv_timeout" => writeln!(out, "static inline {elem} {type_name}__recv_timeout({type_name}* self, int64_t ms) {{ {elem} __val = {{0}}; gorget_channel_recv_timeout(*self, &__val, ms); return __val; }}").unwrap(),
        "clone" => writeln!(out, "static inline {type_name} {type_name}__clone({type_name} self) {{ return gorget_channel_retain(self); }}").unwrap(),
        "drop" => writeln!(out, "static inline void {type_name}__drop({type_name}* self) {{ gorget_channel_release(*self); }}").unwrap(),
        _ => {} // Unknown method — skip
    }
}

pub(super) fn emit_shared_wrapper(out: &mut String, type_name: &str, method: &str, elem: &str) {
    match method {
        "new" => writeln!(out, "static inline {type_name} {type_name}__new({elem} val) {{ return gorget_shared_new(sizeof({elem}), &val); }}").unwrap(),
        "clone" => writeln!(out, "static inline {type_name} {type_name}__clone({type_name} self) {{ return gorget_shared_clone(self); }}").unwrap(),
        "drop" => writeln!(out, "static inline void {type_name}__drop({type_name}* self) {{ gorget_shared_drop(*self); }}").unwrap(),
        "get" => writeln!(out, "static inline {elem} {type_name}__get({type_name} self) {{ return *({elem}*)gorget_shared_get_ptr(self); }}").unwrap(),
        "strong_count" => writeln!(out, "static inline int64_t {type_name}__strong_count({type_name} self) {{ return gorget_shared_strong_count(self); }}").unwrap(),
        "downgrade" => {
            let weak_name = type_name.replacen("Shared__", "Weak__", 1);
            writeln!(out, "static inline {weak_name} {type_name}__downgrade({type_name} self) {{ return gorget_shared_downgrade(self); }}").unwrap();
        }
        // Shared__Vector__T extra methods: at, set_at, slen
        // For Shared[Vector[T]], the at/set_at operate on the vector's elements (type T),
        // NOT on Vector[T] itself. Use gorget_shared_array_* runtime functions.
        "at" => {
            // elem is the Vector's element type (e.g., Vector__int64_t = GorgetArray).
            // We need the *inner* element type for Shared__Vector patterns.
            let inner = shared_vector_inner_elem(type_name, elem);
            writeln!(out, "static inline {inner} {type_name}__at({type_name} self, int64_t idx) {{ return *({inner}*)gorget_shared_array_get(self, (size_t)idx); }}").unwrap();
        }
        "set_at" => {
            let inner = shared_vector_inner_elem(type_name, elem);
            writeln!(out, "static inline void {type_name}__set_at({type_name} self, int64_t idx, {inner} val) {{ gorget_shared_array_set(self, (size_t)idx, &val, sizeof({inner})); }}").unwrap();
        }
        "slen" => writeln!(out, "static inline int64_t {type_name}__slen({type_name} self) {{ return gorget_shared_array_len(self); }}").unwrap(),
        _ => {}
    }
}

pub(super) fn emit_weak_wrapper(out: &mut String, type_name: &str, method: &str, _orig_to_c: &HashMap<String, String>) {
    let shared_name = type_name.replacen("Weak__", "Shared__", 1);
    match method {
        "clone" => writeln!(out, "static inline {type_name} {type_name}__clone({type_name} self) {{ return gorget_weak_clone(self); }}").unwrap(),
        "drop" => writeln!(out, "static inline void {type_name}__drop({type_name}* self) {{ gorget_weak_drop(*self); }}").unwrap(),
        "upgrade" => writeln!(out, "static inline {shared_name} {type_name}__upgrade({type_name} self) {{ return gorget_weak_upgrade(self) ? self : NULL; }}").unwrap(),
        _ => {}
    }
}

pub(super) fn emit_mutex_wrapper(out: &mut String, type_name: &str, method: &str, elem: &str) {
    match method {
        "new" => writeln!(out, "static inline GorgetMutex* {type_name}__new({elem} val) {{ return gorget_mutex_new(sizeof({elem}), &val); }}").unwrap(),
        "lock" => writeln!(out, "static inline gorget_guard_t {type_name}__lock(GorgetMutex** self) {{ return gorget_mutex_lock(*self); }}").unwrap(),
        "try_lock" => writeln!(out, "static inline bool {type_name}__try_lock(GorgetMutex** self, gorget_guard_t* out) {{ return gorget_mutex_try_lock(*self, out); }}").unwrap(),
        "drop" => writeln!(out, "static inline void {type_name}__drop(GorgetMutex** self) {{ gorget_mutex_free(*self); }}").unwrap(),
        _ => {} // Unknown method
    }
}

pub(super) fn emit_rwlock_wrapper(out: &mut String, type_name: &str, method: &str, elem: &str) {
    match method {
        "new" => writeln!(out, "static inline GorgetRWLock* {type_name}__new({elem} val) {{ return gorget_rwlock_new(sizeof({elem}), &val); }}").unwrap(),
        "read" => writeln!(out, "static inline gorget_read_guard_t {type_name}__read(GorgetRWLock** self) {{ return gorget_rwlock_read(*self); }}").unwrap(),
        "write" => writeln!(out, "static inline gorget_write_guard_t {type_name}__write(GorgetRWLock** self) {{ return gorget_rwlock_write(*self); }}").unwrap(),
        "drop" => writeln!(out, "static inline void {type_name}__drop(GorgetRWLock** self) {{ gorget_rwlock_free(*self); }}").unwrap(),
        _ => {}
    }
}

pub(super) fn emit_box_wrapper(out: &mut String, type_name: &str, method: &str, elem: &str, _module: &LirModule, _orig_to_c: &HashMap<String, String>) {
    // Trait-object dispatch wrappers (previously a `_ =>` branch here)
    // have been retired: Step 7 moved that to `__gg_synth_trait_*`
    // helpers synthesized via `bir::synth::get_or_emit_trait_helper`.
    // See `docs/devbook/16-bir.md`.
    match method {
        "get" => writeln!(out, "static inline {elem} {type_name}__get({type_name} self) {{ return *({elem}*)self; }}").unwrap(),
        "set" => writeln!(out, "static inline void {type_name}__set({type_name} self, {elem} val) {{ *({elem}*)self = val; }}").unwrap(),
        // D36: `Box[T]` auto-deref projects the receiver through this helper
        // so the equipped-on-T method dispatches against a `T*`. Mirrors
        // `emit_guard_get_ptr`'s role for `Guard[T]` (`shared.rs`).
        "get_ptr" => writeln!(out, "static inline {elem}* {type_name}__get_ptr({type_name} self) {{ return ({elem}*)self; }}").unwrap(),
        "drop" | "free" => writeln!(out, "static inline void {type_name}__drop({type_name} self) {{ GORGET_FREE(self, sizeof({elem})); }}").unwrap(),
        _ => {}
    }
}

pub(super) fn parse_box_method(name: &str) -> Option<(String, &str)> {
    if !name.starts_with("Box__") { return None; }
    let rest = &name["Box__".len()..];
    let method_sep = rest.rfind("__")?;
    let type_part = &rest[..method_sep];
    let method = &rest[method_sep + 2..];
    Some((format!("Box__{type_part}"), method))
}

pub(super) fn box_elem_type(type_name: &str) -> &str {
    type_name.strip_prefix("Box__").unwrap_or("int64_t")
}
/// Find the C return type of a trait method by looking at extern declarations first,
/// then falling back to trait impl functions.
pub(super) fn find_trait_method_return_type(module: &LirModule, trait_name: &str, method: &str, sn: &HashMap<u32, String>) -> String {
    // First, check extern declarations for Box__Trait__method — these have correct return types
    // even when trait impl functions have been eliminated by DCE.
    let box_method_name = format!("Box__{trait_name}__{method}");
    for ext in &module.externs {
        if ext.name == box_method_name {
            return c_type_named(&ext.return_type, sn);
        }
    }
    // Fallback: search for trait impl functions
    let suffix = format!("__{method}");
    let prefix = format!("{trait_name}_for_");
    for func in &module.functions {
        if func.name.starts_with(&prefix) && func.name.ends_with(&suffix) {
            return c_type_named(&func.return_type, sn);
        }
    }
    // Fallback: void
    "void".to_string()
}

/// Find the C name of a struct by its original (pre-rename) name.
pub(super) fn find_struct_cname_by_orig(module: &LirModule, orig_name: &str, sn: &HashMap<u32, String>) -> String {
    for (i, def) in module.structs.iter().enumerate() {
        if def.name == orig_name {
            return sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"));
        }
    }
    // Fallback
    orig_name.to_string()
}

// `struct_contains_resource`, `is_user_hashable_key`, `hashable_key_fn_names`
// were lifted to `crate::lir::queries` so the LLVM backend can consume them
// without reaching into `c_lir`. Use the `queries::*` paths at call sites;
// this module re-exports through the `pub use` at the top of `helpers.rs` to
// avoid churn at internal sites.
pub use crate::lir::queries::{
    struct_contains_resource,
    is_user_hashable_key,
    hashable_key_fn_names,
};

/// Returns true if the function is provided by the Gorget C runtime (static inline).
pub(super) fn is_runtime_fn(name: &str) -> bool {
    name.starts_with("gorget_")
        || name.starts_with("GORGET_")
        || name.starts_with("__gorget_")
}
/// Rewrite out-parameter calls for image/audio/deflate functions.
/// These C runtime functions use a void+out-param ABI but GIR treats them as single-return.
/// Returns Some(code) if the function was handled, None otherwise.
pub(super) fn try_emit_outparam_call_lir(
    func_name: &str,
    dst: &Option<ValueId>,
    args: &[ValueId],
    val_types: &[Option<LirType>],
    func: &LirFunction,
    sn: &std::collections::HashMap<u32, String>,
    structs: &[StructDef],
) -> Option<String> {
    use std::fmt::Write;
    let v = |id: ValueId| format!("__v{}", id.0);
    let mut out = String::new();

    // Helper: get the C type name for the destination value's type.
    let dst_c_type = |d: &ValueId| -> String {
        val_types.get(d.0 as usize)
            .and_then(|t| t.as_ref())
            .map(|t| c_type_named(t, sn))
            .unwrap_or_else(|| "int64_t".to_string())
    };

    // Helper: coerce a string literal arg to Str, or pass a Str value as-is.
    // For Ptr args (e.g. pointer to Str slot), dereference to Str.
    let str_arg = |a: ValueId| -> String {
        let is_lit = matches!(
            func.value_origins.get(a.0 as usize).and_then(|o| o.as_ref()),
            Some(ValueOrigin::StrLit)
        );
        let ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
        if is_lit {
            format!("gorget_str_from_literal({v}, strlen({v}))", v = v(a))
        } else if matches!(ty, Some(LirType::Ptr)) {
            format!("*(Str*){}", v(a))
        } else {
            v(a)
        }
    };

    // Helper: get address of an array arg (pass by pointer).
    let array_addr = |a: ValueId| -> String {
        let ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
        if matches!(ty, Some(LirType::Ptr)) {
            v(a)
        } else {
            format!("&{}", v(a))
        }
    };

    // Helper: find the LIR C name for a struct by its original (GIR) name.
    let find_struct_c_name = |orig_name: &str| -> String {
        for (i, sdef) in structs.iter().enumerate() {
            if sdef.name == orig_name {
                return sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"));
            }
        }
        orig_name.to_string()
    };

    // Helper: find the Ok field name in a Result struct (LIR flattens to Ok_0, Error_0).
    // Also returns Error field name.
    let result_fields = |d: &ValueId| -> (String, String) {
        if let Some(LirType::Struct(sid)) = val_types.get(d.0 as usize).and_then(|t| t.as_ref()) {
            if let Some(sdef) = structs.get(sid.0 as usize) {
                let ok_f = sdef.fields.iter().find(|(n, _)| n.starts_with("Ok"))
                    .map(|(n, _)| c_field_name(n)).unwrap_or_else(|| "Ok_0".to_string());
                let err_f = sdef.fields.iter().find(|(n, _)| n.starts_with("Error"))
                    .map(|(n, _)| c_field_name(n)).unwrap_or_else(|| "Error_0".to_string());
                return (ok_f, err_f);
            }
        }
        ("Ok_0".to_string(), "Error_0".to_string())
    };

    match func_name {
        // gorget_image_load_rgba(Str path, int64_t* out_tag, int64_t* out_w, int64_t* out_h,
        //                       int64_t* out_ch, GorgetArray* out_data, Str* out_err)
        // Returns Result[Image, str]
        "gorget_image_load_rgba" | "image_load_rgba" => {
            let d = dst.as_ref()?;
            let c_type = dst_c_type(d);
            let path = str_arg(args[0]);
            let image_c = find_struct_c_name("Image");
            let (ok_f, err_f) = result_fields(d);
            let _ = write!(out,
                "{{ int64_t __tag = 0, __w = 0, __h = 0, __ch = 0; \
                GorgetArray __data = gorget_array_new(sizeof(uint8_t)); Str __err = {{0}}; \
                gorget_image_load_rgba({path}, &__tag, &__w, &__h, &__ch, &__data, &__err); \
                {c_type} __wr = {{0}}; __wr.tag = __tag; \
                if (__tag == 0) {{ __wr.{ok_f} = ({image_c}){{.width = __w, .height = __h, .channels = __ch, .data = __data}}; }} \
                else {{ __wr.{err_f} = __err; }} {v} = __wr; }}",
                v = v(*d));
            Some(out)
        }
        // gorget_image_load_rgba_from_memory(const GorgetArray* data, ...)
        "gorget_image_load_rgba_from_memory" => {
            let d = dst.as_ref()?;
            let c_type = dst_c_type(d);
            let data_ptr = array_addr(args[0]);
            let image_c = find_struct_c_name("Image");
            let (ok_f, err_f) = result_fields(d);
            let _ = write!(out,
                "{{ int64_t __tag = 0, __w = 0, __h = 0, __ch = 0; \
                GorgetArray __data = gorget_array_new(sizeof(uint8_t)); Str __err = {{0}}; \
                gorget_image_load_rgba_from_memory({data_ptr}, &__tag, &__w, &__h, &__ch, &__data, &__err); \
                {c_type} __wr = {{0}}; __wr.tag = __tag; \
                if (__tag == 0) {{ __wr.{ok_f} = ({image_c}){{.width = __w, .height = __h, .channels = __ch, .data = __data}}; }} \
                else {{ __wr.{err_f} = __err; }} {v} = __wr; }}",
                v = v(*d));
            Some(out)
        }
        // gorget_image_flip_vertically — extract Image fields, pass individually
        "gorget_image_flip_vertically" => {
            let d = dst.as_ref()?;
            let img = v(args[0]);
            let arg_ty = val_types.get(args[0].0 as usize).and_then(|t| t.as_ref());
            let image_c = find_struct_c_name("Image");
            // When the arg is a Ptr (void*), cast to Image* for field access.
            let (img_expr, acc) = if matches!(arg_ty, Some(LirType::Ptr)) {
                (format!("(({image_c}*){img})"), "->".to_string())
            } else {
                (img.clone(), ".".to_string())
            };
            let data_ref = format!("&{img_expr}{acc}data");
            let _ = write!(out,
                "{{ int64_t __w = 0, __h = 0, __ch = 0; \
                GorgetArray __data = gorget_array_new(sizeof(uint8_t)); \
                gorget_image_flip_vertically({img_expr}{acc}width, {img_expr}{acc}height, {img_expr}{acc}channels, {data_ref}, &__w, &__h, &__ch, &__data); \
                {v} = ({image_c}){{.width = __w, .height = __h, .channels = __ch, .data = __data}}; }}",
                v = v(*d));
            Some(out)
        }
        // gorget_audio_load_wav(Str path, ...)
        "gorget_audio_load_wav" => {
            let d = dst.as_ref()?;
            let c_type = dst_c_type(d);
            let path = str_arg(args[0]);
            let (ok_f, err_f) = result_fields(d);
            let _ = write!(out,
                "{{ int64_t __tag = 0; GorgetAudioChunk __chunk = {{0}}; Str __err = {{0}}; \
                gorget_audio_load_wav({path}, &__tag, &__chunk, &__err); \
                {c_type} __wr = {{0}}; __wr.tag = __tag; \
                if (__tag == 0) {{ __wr.{ok_f} = __chunk; }} \
                else {{ __wr.{err_f} = __err; }} {v} = __wr; }}",
                v = v(*d));
            Some(out)
        }
        // gorget_audio_load_wav_from_memory(const GorgetArray* data, ...)
        "gorget_audio_load_wav_from_memory" => {
            let d = dst.as_ref()?;
            let c_type = dst_c_type(d);
            let data_ptr = array_addr(args[0]);
            let (ok_f, err_f) = result_fields(d);
            let _ = write!(out,
                "{{ int64_t __tag = 0; GorgetAudioChunk __chunk = {{0}}; Str __err = {{0}}; \
                gorget_audio_load_wav_from_memory({data_ptr}, &__tag, &__chunk, &__err); \
                {c_type} __wr = {{0}}; __wr.tag = __tag; \
                if (__tag == 0) {{ __wr.{ok_f} = __chunk; }} \
                else {{ __wr.{err_f} = __err; }} {v} = __wr; }}",
                v = v(*d));
            Some(out)
        }
        // gorget_deflate_decompress(const GorgetArray* data, int64_t uncompressed_size, ...)
        "gorget_deflate_decompress" => {
            let d = dst.as_ref()?;
            let c_type = dst_c_type(d);
            let data_ptr = array_addr(args[0]);
            let size = v(args[1]);
            let (ok_f, err_f) = result_fields(d);
            let _ = write!(out,
                "{{ int64_t __tag = 0; GorgetArray __data = gorget_array_new(sizeof(uint8_t)); Str __err = {{0}}; \
                gorget_deflate_decompress({data_ptr}, {size}, &__tag, &__data, &__err); \
                {c_type} __wr = {{0}}; __wr.tag = __tag; \
                if (__tag == 0) {{ __wr.{ok_f} = __data; }} \
                else {{ __wr.{err_f} = __err; }} {v} = __wr; }}",
                v = v(*d));
            Some(out)
        }
        _ => None,
    }
}
/// Returns true if the extern's declared parameter at position `i` is a Str struct type.
/// Used to generically detect string literal args to any extern (GL, SDL, etc.) that
/// need wrapping with `gorget_str_from_literal()`.
pub(super) fn ext_param_is_str(ext_params: Option<&[LirType]>, i: usize, module: &LirModule) -> bool {
    ext_params.and_then(|p| p.get(i)).map_or(false, |ty| is_str_struct(ty, module))
}

/// Returns true if the runtime function returns a raw `const char*` that needs wrapping
/// when stored to a Str/GorgetString slot. Other Ptr-returning functions return struct
/// pointers or void* that should be handled by the aggregate (memcpy) path instead.
/// For a Box__Trait__method call, return the argument positions (0-based, where 0 = self)
/// that should be coerced to Str. Returns empty vec for non-trait calls.
pub(super) fn trait_box_str_arg_positions(module: &LirModule, name: &str) -> Vec<usize> {
    if !name.starts_with("Box__") { return vec![]; }
    let rest = &name["Box__".len()..];
    let sep = match rest.rfind("__") {
        Some(pos) => pos,
        None => return vec![],
    };
    let trait_name = &rest[..sep];
    let method = &rest[sep + 2..];
    // Check if there's a VTable for this trait
    if !module.structs.iter().any(|s| s.name == format!("{trait_name}_VTable")) {
        return vec![];
    }
    let str_sid = module.structs.iter().position(|s| s.name == "GorgetString");
    if str_sid.is_none() { return vec![]; }
    let str_sid = str_sid.unwrap();
    let prefix = format!("{trait_name}_for_");
    let suffix = format!("__{method}");
    // Find the impl function and check which params are GorgetString
    if let Some(f) = module.functions.iter().find(|f| f.name.starts_with(&prefix) && f.name.ends_with(&suffix)) {
        // The wrapper's arg 0 = self, impl's arg 0 = self.data (void*).
        // Wrapper args 1..N map to impl args 1..N.
        // Return positions in the *call* args (which include self at position 0).
        f.params.iter().enumerate()
            .filter(|(_, p)| {
                matches!(p, LirType::Struct(sid) if sid.0 as usize == str_sid)
                || matches!(p, LirType::PtrTo(sid) if sid.0 as usize == str_sid)
            })
            .map(|(i, _)| i)  // i is the position in impl (0=self, 1=first arg, ...)
            .collect()
    } else {
        vec![]
    }
}
/// Functions that return a nullable `const char*` (NULL = None, non-NULL = some string).
/// These need to be wrapped into `Option<Str>` when the destination type is Option.
#[allow(dead_code)]
pub(super) fn is_nullable_cstr_fn(name: &str) -> bool {
    matches!(name, "gorget_getenv")
}

/// Functions that return a nullable pointer (NULL = None, non-NULL = Some(value)).
/// When the destination type is Option<T>, wrap into Option.
pub(super) fn is_nullable_ptr_fn(name: &str) -> bool {
    // Weak__T__upgrade returns NULL when the shared value has been dropped.
    name.starts_with("Weak__") && name.ends_with("__upgrade")
}

/// Functions that return a sentinel value indicating "no result". Empty now —
/// the old gorget_regex_find family routed through this when xtd.regex was a
/// PCRE2 wrapper. The pure-Gorget engine handles its own Option wrapping.
pub(super) fn is_sentinel_option_fn(_name: &str) -> bool {
    false
}
/// Returns true if the collection runtime function returns `void*` (pointer to element).
/// The caller must dereference the result to the concrete element type.
pub(super) fn is_collection_void_return(name: &str) -> bool {
    // NOTE: gorget_array_safe_get, gorget_array_safe_pop, and gorget_array_remove_opt
    // are no longer here — their Option wrapping is now generated at GIR level
    // (null-check + enum_init), so the C backend sees a truthful Ptr return.
    matches!(
        name,
        "gorget_array_get"
            | "gorget_array_pop"
            | "gorget_array_first"
            | "gorget_array_last"
            | "gorget_map_get"
            | "gorget_heap_pop"
            | "gorget_heap_peek"
            // Concurrency: guard/shared/rwlock accessors return void*
            | "gorget_guard_get"
            | "gorget_guard_get_ptr"
            | "gorget_shared_get"
            | "gorget_shared_get_ptr"
            | "gorget_read_guard_get"
            | "gorget_read_guard_get_ptr"
            | "gorget_write_guard_get"
            | "gorget_write_guard_get_ptr"
            // Channel recv writes to an out-pointer (handled separately),
            // but channel_recv_timeout etc. may also return void*
            | "gorget_channel_recv"
    )
}

/// Functions that return void but need Option wrapping — included in is_collection_void_return
/// after being swapped to their opt variant.
/// NOTE: gorget_array_remove no longer needs this — the LIR now maps directly to
/// gorget_array_remove_opt, and Option wrapping is done at GIR level.
pub(super) fn needs_opt_wrapping(_name: &str) -> bool {
    false
}

/// For collection functions that are void but need to return a value when
/// the GIR expects Option[T], swap to the opt-returning variant.
/// NOTE: No longer needed — the LIR maps remove directly to gorget_array_remove_opt.
#[allow(dead_code)]
pub(super) fn void_to_opt_variant(name: &str) -> &str {
    name
}

/// Returns the indices of parameters that are `void*` (element/key/value pointers)
/// for collection runtime functions.  The caller must pass `&(Type){value}` for
/// these positions when the argument is a concrete value (not already a pointer).
/// DEPRECATED: Legacy fallback for VoidElem arg positions. All CallExtern
/// instructions now carry arg_abis from RuntimeFn::resolve_lir_sig. This whitelist
/// serves only as a safety net for edge cases where arg_abis is empty.
pub(super) fn collection_void_param_indices(name: &str) -> &'static [usize] {
    match name {
        "gorget_array_push" => &[1],
        "gorget_array_set" => &[2],
        "gorget_array_insert" => &[2],
        "gorget_array_contains" => &[1],
        "gorget_array_index_of" => &[1],
        "gorget_array_binary_search" => &[1],
        "gorget_array_extend" => &[1],
        "gorget_array_fill" => &[2],      // arg 2: element value (by void*)
        "gorget_map_put" => &[1, 2],
        "gorget_map_get" | "gorget_map_contains" | "gorget_map_remove" | "gorget_map_remove_opt" => &[1],
        "gorget_set_add" | "gorget_set_contains" | "gorget_set_remove" => &[1],
        "gorget_heap_push" => &[1],
        // Dict/Set iteration output-parameter: arg 2 is void* out buffer
        "gorget_map_iter_key" | "gorget_map_iter_value" => &[2],
        // Dict/Set drain: args 2/3 are void* out buffers (K then V)
        "gorget_map_drain_entry" => &[2, 3],
        "gorget_set_drain_entry" => &[2],
        // Concurrency: mutex_new(size, void*), shared_new(size, void*)
        "gorget_mutex_new" | "gorget_shared_new" | "gorget_rwlock_new" => &[1],
        // channel_send(ch, void*), guard_set(guard, void*, size)
        "gorget_channel_send" => &[1],
        "gorget_guard_set" | "gorget_write_guard_set" => &[1],
        _ => &[],
    }
}
/// Returns true if this collection runtime function takes its first arg
/// (the collection itself) by pointer.  Nearly all gorget_array_*, gorget_map_*,
/// gorget_set_* methods do, with the exception of constructors (_new).
/// Returns true if the extern name is an Option/Result unwrap helper.
#[allow(dead_code)]
pub(super) fn is_option_result_unwrap(name: &str) -> bool {
    name == "__option_unwrap" || name == "__result_unwrap"
        || name == "__option_unwrap_or" || name == "__result_unwrap_or"
        || name == "__result_unwrap_error"
        || name == "gorget_option_unwrap"
        || (name.contains("Option__") && (name.ends_with("__unwrap") || name.ends_with("__unwrap_or")))
        || (name.contains("Result__") && (name.ends_with("__unwrap") || name.ends_with("__unwrap_or")))
}

/// Returns true if the extern name is an Option/Result expect helper.
#[allow(dead_code)]
pub(super) fn is_option_result_expect(name: &str) -> bool {
    name == "__option_expect" || name == "__result_expect"
        || (name.contains("Option__") && name.ends_with("__expect"))
        || (name.contains("Result__") && name.ends_with("__expect"))
}

/// Returns true if the extern name is polymorphic — i.e. called with different
/// return types at different call sites. The extern declaration is unreliable
/// for type inference; use the SlotStore fix-up instead.
pub(super) fn is_polymorphic_extern(name: &str) -> bool {
    is_option_result_unwrap(name) || is_option_result_expect(name)
        || is_option_result_combinator(name)
}

/// Returns true if the extern name is an Option/Result combinator helper.
pub(super) fn is_option_result_combinator(name: &str) -> bool {
    (name.contains("Option__") || name.contains("Result__"))
        && (name.ends_with("__map") || name.ends_with("__filter")
            || name.ends_with("__and_then") || name.ends_with("__or_else")
            || name.ends_with("__unwrap_err") || name.ends_with("__unwrap_error") || name.ends_with("__map_err"))
}
/// Returns true if the runtime function takes self (arg 0) by pointer.
/// LEGACY: only needed for unmapped GIR names (gorget_str_push etc.) that bypass
/// RuntimeFn::resolve_lir_sig. Tagged functions use arg_abis directly.
/// DEPRECATED: Legacy fallback for self-by-ptr detection. All CallExtern
/// instructions now carry arg_abis from RuntimeFn::resolve_lir_sig. This whitelist
/// serves only as a safety net for unmapped GIR names.
fn legacy_self_by_ptr(name: &str) -> bool {
    // gorget_str_push/push_line/push_char/clear — GIR names that don't always get remapped
    matches!(name, "gorget_str_push" | "gorget_str_push_line" | "gorget_str_push_char"
        | "gorget_str_clear" | "gorget_str_capacity" | "gorget_str_str"
        | "gorget_array_extend")
    || ((name.starts_with("gorget_array_") || name.starts_with("gorget_map_")
        || name.starts_with("gorget_set_") || name.starts_with("gorget_heap_")
        || name.starts_with("gorget_bytes_"))
        && !name.ends_with("_new"))
}

/// Returns true if a gorget_str_* function has a non-Str parameter at index `i`.
/// Used to prevent Str wrapping for args that are actually GorgetArray, etc.
pub(super) fn str_fn_non_str_arg(name: &str, i: usize) -> bool {
    // gorget_str_join(Str sep, GorgetArray parts) — arg 1 is GorgetArray
    if name == "gorget_str_join" && i == 1 { return true; }
    false
}

/// Check if a name suffix is a type name (indicating a constructor, not a method call).
pub(super) fn is_collection_type_constructor(last_part: &str) -> bool {
    matches!(last_part, "int64_t" | "int32_t" | "int16_t" | "int8_t"
        | "uint64_t" | "uint32_t" | "uint16_t" | "uint8_t"
        | "double" | "float" | "bool" | "GorgetString"
        | "GorgetArray" | "GorgetMap" | "GorgetSet" | "void"
        | "T" | "U" | "V")
}
/// Emit a collection constructor call.
/// Vector__int64_t(cap) → gorget_array_with_capacity(sizeof(int64_t), cap)
/// Vector__int64_t() → gorget_array_new(sizeof(int64_t))
/// Return the C drop function name for a resource-type element, or None for trivial types.
///
/// Phase A residual #2 (closes 2026-05-05): reads `StructDef.elem_drop_fn`
/// via `LirModule::struct_def_by_name`, which transparently resolves alias
/// names (`Vector__int64_t`, `Dict__K__V`, …) to the runtime singleton's
/// StructDef. The previous name-prefix fallback retired here — every
/// collection alias is registered in `module.struct_aliases` at LIR
/// lowering time, so the resolved StructDef carries the typed metadata
/// uniformly. CLAUDE.md "no name matching" applied to the LIR/c_lir
/// boundary.
pub(super) fn elem_drop_fn_for_c_type(c_type: &str, module: &crate::lir::LirModule) -> Option<String> {
    let sd = module.struct_def_by_name(c_type)?;
    if let Some(ref f) = sd.elem_drop_fn {
        return Some(f.clone());
    }
    // c_runtime_alias path: when the StructDef itself doesn't carry the
    // metadata directly but aliases another runtime struct via the C
    // typedef path. Mostly subsumed by `struct_def_by_name` once the
    // alias map is populated; kept defensive for paths where the
    // alias entry is absent but the StructDef carries `c_runtime_alias`.
    if let Some(ref rt) = sd.c_runtime_alias {
        return module.struct_def_by_name(rt)
            .and_then(|s| s.elem_drop_fn.clone());
    }
    None
}

// `elem_clone_fn_for_c_type` is gone — its only callers (Vector / Dict
// post-call wiring in `emit_call_extern.rs`) were retired now that
// `infer_collection_elem_fns` (in `lir/lower/insts.rs`) emits the
// equivalent FieldStore insts via `super::types::elem_clone_fn_for_type`
// at the LIR layer. The LIR-level helper covers the same type set.

/// Item 7e Phase 4 consumer: emit a collection constructor.
///
/// Two-tier read strategy, in priority order:
/// 1. **Typed read (SSoT)** — if the destination `ValueId`'s
///    `val_types[d]` is `LirType::Resource { kind, params }`, render
///    element / key / value C-type names from `params` directly. This
///    is the layering-discipline-aligned path: the writer (LIR lowering)
///    sets typed params; we read them.
/// 2. **Name-parse fallback** — when val_types isn't populated as
///    `Resource` (legacy `CallExtern` ctor path that wasn't promoted
///    to `Inst::CollectionCtor`), fall back to the original
///    `strip_prefix("Vector__")` parse. The fallback retires as more
///    of the lowering pipeline emits typed Resource operands.
///
/// The fallback path is deliberately preserved (not deleted) so this
/// commit lands as a writer-aware enhancement, not a regression risk.
/// Per CLAUDE.md scope discipline, removing it requires confirming
/// every call site flows through the typed path first.
pub(super) fn emit_collection_constructor(
    out: &mut String,
    name: &str,
    dst: &Option<ValueId>,
    args: &[ValueId],
    val_types: &[Option<LirType>],
    sn: &HashMap<u32, String>,
    module: &crate::lir::LirModule,
) {
    use std::fmt::Write;
    let v = |vid: ValueId| format!("__v{}", vid.0);

    if let Some(d) = dst {
        write!(out, "{} = ", v(*d)).unwrap();
    }

    // Tier 1: typed-Resource read. Pull (kind, params) off the dst
    // value's inferred LIR type; render param[i] to a C type name.
    let typed = dst.and_then(|d| val_types.get(d.0 as usize).and_then(|t| t.as_ref()))
        .and_then(|t| match t {
            LirType::Resource { kind, params } => Some((*kind, params.clone())),
            _ => None,
        });

    if let Some((kind, params)) = typed {
        use crate::lir::ResourceKind;
        match kind {
            ResourceKind::GorgetArray => {
                let elem_type = params.first()
                    .map(|p| c_type_named(p, sn))
                    .unwrap_or_else(|| "int64_t".to_string());
                if args.is_empty() {
                    write!(out, "gorget_array_new(sizeof({elem_type}));").unwrap();
                } else {
                    write!(out, "gorget_array_with_capacity(sizeof({elem_type}), {});", v(args[0])).unwrap();
                }
                if let Some(drop_fn) = elem_drop_fn_for_c_type(&elem_type, module) {
                    if let Some(d) = dst {
                        write!(out, " {}.elem_drop = (__gorget_drop_fn){drop_fn};", v(*d)).unwrap();
                    }
                }
                return;
            }
            ResourceKind::GorgetSet => {
                let elem_type = params.first()
                    .map(|p| c_type_named(p, sn))
                    .unwrap_or_else(|| "int64_t".to_string());
                if args.is_empty() {
                    write!(out, "gorget_set_new(sizeof({elem_type}));").unwrap();
                } else {
                    write!(out, "gorget_set_with_capacity(sizeof({elem_type}), {});", v(args[0])).unwrap();
                }
                if is_user_hashable_key(&elem_type, module) {
                    if let Some(d) = dst {
                        write!(out, " {}.hash_fn = (__gorget_hash_fn)__gorget_ktable_hash__{elem_type};", v(*d)).unwrap();
                        write!(out, " {}.eq_fn = (__gorget_eq_fn)__gorget_ktable_eq__{elem_type};", v(*d)).unwrap();
                    }
                }
                return;
            }
            ResourceKind::GorgetMap => {
                let key_type = params.first()
                    .map(|p| c_type_named(p, sn))
                    .unwrap_or_else(|| "int64_t".to_string());
                let val_type = params.get(1)
                    .map(|p| c_type_named(p, sn))
                    .unwrap_or_else(|| "int64_t".to_string());
                let fn_name = if name.starts_with("Dict__") { "gorget_dict_new" } else { "gorget_map_new" };
                write!(out, "{fn_name}(sizeof({key_type}), sizeof({val_type}));").unwrap();
                if let Some(drop_fn) = elem_drop_fn_for_c_type(&val_type, module) {
                    if let Some(d) = dst {
                        write!(out, " {}.val_drop = (__gorget_drop_fn){drop_fn};", v(*d)).unwrap();
                    }
                }
                if is_user_hashable_key(&key_type, module) {
                    if let Some(d) = dst {
                        write!(out, " {}.hash_fn = (__gorget_hash_fn)__gorget_ktable_hash__{key_type};", v(*d)).unwrap();
                        write!(out, " {}.eq_fn = (__gorget_eq_fn)__gorget_ktable_eq__{key_type};", v(*d)).unwrap();
                    }
                }
                return;
            }
            // String / Closure / RefCounted don't reach this constructor path.
            _ => {}
        }
    }

    // Tier 2: name-parse fallback (legacy path; retire incrementally
    // as more lowering sites emit typed Resource operands).
    if name.starts_with("Vector__") || name.starts_with("Deque__") || name.starts_with("GorgetArray__") {
        let elem_type = name.strip_prefix("Vector__")
            .or_else(|| name.strip_prefix("Deque__"))
            .or_else(|| name.strip_prefix("GorgetArray__"))
            .unwrap_or("int64_t");
        if args.is_empty() {
            write!(out, "gorget_array_new(sizeof({elem_type}));").unwrap();
        } else {
            write!(out, "gorget_array_with_capacity(sizeof({elem_type}), {});", v(args[0])).unwrap();
        }
        // Set elem_drop for resource-type elements
        if let Some(drop_fn) = elem_drop_fn_for_c_type(elem_type, module) {
            if let Some(d) = dst {
                write!(out, " {}.elem_drop = (__gorget_drop_fn){drop_fn};", v(*d)).unwrap();
            }
        }
    } else if name.starts_with("Set__") || name.starts_with("HashSet__") {
        let elem_type = name.strip_prefix("Set__")
            .or_else(|| name.strip_prefix("HashSet__"))
            .unwrap_or("int64_t");
        if args.is_empty() {
            write!(out, "gorget_set_new(sizeof({elem_type}));").unwrap();
        } else {
            write!(out, "gorget_set_with_capacity(sizeof({elem_type}), {});", v(args[0])).unwrap();
        }
        // User-type keys with Hashable+Equatable impls: wire the runtime
        // hash_fn / eq_fn through the synthetic `__gorget_ktable_*` bridges
        // emitted in `generate_c_inner_impl`. Without this, Set falls back
        // to byte-FNV / memcmp — correct for POD structs, wrong for any key
        // with a pointer field (String, Vector, etc.).
        if is_user_hashable_key(elem_type, module) {
            if let Some(d) = dst {
                write!(out, " {}.hash_fn = (__gorget_hash_fn)__gorget_ktable_hash__{elem_type};", v(*d)).unwrap();
                write!(out, " {}.eq_fn = (__gorget_eq_fn)__gorget_ktable_eq__{elem_type};", v(*d)).unwrap();
            }
        }
    } else if name.starts_with("Dict__") || name.starts_with("HashMap__") {
        // Dict__K__V or HashMap__K__V — extract key/value types
        let prefix = if name.starts_with("Dict__") { "Dict__" } else { "HashMap__" };
        let rest = name.strip_prefix(prefix).unwrap_or("int64_t__int64_t");
        // For Dict__int64_t__Str → key=int64_t, val=Str
        // Simple heuristic: first __ part is key, rest is val
        let parts: Vec<&str> = rest.splitn(2, "__").collect();
        let (key_type, val_type) = if parts.len() == 2 {
            (parts[0], parts[1])
        } else {
            ("int64_t", "int64_t")
        };
        let fn_name = if name.starts_with("Dict__") { "gorget_dict_new" } else { "gorget_map_new" };
        write!(out, "{fn_name}(sizeof({key_type}), sizeof({val_type}));").unwrap();
        // Set val_drop for resource-type values
        if let Some(drop_fn) = elem_drop_fn_for_c_type(val_type, module) {
            if let Some(d) = dst {
                write!(out, " {}.val_drop = (__gorget_drop_fn){drop_fn};", v(*d)).unwrap();
            }
        }
        // Wire the Hashable/Equatable bridges for user-type keys (see
        // comment on Set path above).
        if is_user_hashable_key(key_type, module) {
            if let Some(d) = dst {
                write!(out, " {}.hash_fn = (__gorget_hash_fn)__gorget_ktable_hash__{key_type};", v(*d)).unwrap();
                write!(out, " {}.eq_fn = (__gorget_eq_fn)__gorget_ktable_eq__{key_type};", v(*d)).unwrap();
            }
        }
    } else {
        // Fallback — shouldn't happen
        write!(out, "/* unknown constructor: {name} */ {{0}};").unwrap();
    }
}

// `is_user_hashable_key` and `hashable_key_fn_names` were lifted to
// `crate::lir::queries` and are re-exported at the top of this module.

/// Maps a runtime function name to its thread-local error check function.
/// Functions in this list return a raw scalar value in C, but the GIR expects
/// them to return a Result struct. The backend must wrap the call with an
/// error check to construct the Result.
#[allow(dead_code)]
pub(super) fn last_error_fn(name: &str) -> Option<&'static str> {
    if name.starts_with("gorget_udp_") {
        return Some("gorget_udp_last_error");
    }
    if name.starts_with("gorget_server_socket_") {
        return Some("gorget_server_socket_last_error");
    }
    if name.starts_with("gorget_socket_") {
        return Some("gorget_socket_last_error");
    }
    // TlsServer before Tls to avoid prefix collision
    if name.starts_with("gorget_tls_server_") {
        return Some("gorget_tls_server_last_error");
    }
    if name.starts_with("gorget_tls_") {
        return Some("gorget_tls_last_error");
    }
    if name.starts_with("gorget_crypto_") {
        return Some("gorget_crypto_last_error");
    }
    if name == "gorget_process_spawn" {
        return Some("gorget_process_spawn_err");
    }
    if name == "gorget_parse_int" || name == "gorget_parse_float" {
        return Some("gorget_parse_last_error");
    }
    None
}
/// Returns true if the given struct ID refers to a string struct.
pub(super) fn is_str_struct_id(sid: &StructId, module: &LirModule) -> bool {
    module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetString")
}

/// Returns true if the type is a Str or GorgetString struct.
pub(super) fn is_str_struct(ty: &LirType, module: &LirModule) -> bool {
    matches!(ty, LirType::Struct(sid) if is_str_struct_id(sid, module))
}

/// True if `ty` is a `PtrTo` pointing at a string struct.
pub(super) fn is_str_ptr(ty: &LirType, module: &LirModule) -> bool {
    matches!(ty, LirType::PtrTo(sid) if is_str_struct_id(sid, module))
}

/// Optional-ref variant of `is_str_ptr`.
pub(super) fn is_str_ptr_opt(ty: Option<&LirType>, module: &LirModule) -> bool {
    ty.map_or(false, |t| is_str_ptr(t, module))
}
pub(super) fn is_std_header_fn(name: &str) -> bool {
    matches!(
        name,
        "printf" | "fprintf" | "sprintf" | "snprintf" | "puts" | "putchar" | "getchar"
            | "fopen" | "fclose" | "fread" | "fwrite" | "fgets" | "fputs" | "fflush"
            | "fseek" | "ftell" | "rewind" | "feof" | "ferror"
            | "malloc" | "calloc" | "realloc" | "free" | "exit" | "abort" | "atexit"
            | "atoi" | "atol" | "atof" | "strtol" | "strtod"
            | "memcpy" | "memmove" | "memset" | "memcmp"
            | "strlen" | "strcpy" | "strncpy" | "strcat" | "strncat" | "strcmp" | "strncmp"
            | "strstr" | "strchr" | "strrchr"
            | "abs" | "labs" | "llabs"
            | "getenv" | "setenv" | "unsetenv"
            | "getcwd" | "chdir" | "getpid"
            | "time" | "localtime" | "gmtime" | "strftime" | "mktime" | "difftime"
            | "clock_gettime" | "nanosleep"
            | "rand" | "srand"
            | "qsort" | "bsearch"
            // Gorget wrappers that collide with POSIX names — skip extern decls.
            | "sleep"
            | "mkdir" | "rename" | "remove" | "readdir"
            | "usleep"
    )
}
// `compare_fn_for_elem` was removed — element-type qsort dispatch now
// happens at LIR emission time via `map_monomorphized_to_runtime`,
// which routes `Vector__T__sort` to typed stubs like
// `gorget_array_sort_int` / `_float` / `_str` / `_generic`.
/// Map a `__gorget_box_alloc_<suffix>` suffix to the correct C type.
/// Some types (like GorgetString) are represented as LirType::Ptr in LIR
/// but need their real C struct type for proper sizeof/copy semantics.
pub(super) fn box_alloc_suffix_to_c_type(suffix: &str) -> String {
    match suffix {
        "Str" | "GorgetString" => "Str".into(),
        "int64_t" => "int64_t".into(),
        "int32_t" => "int32_t".into(),
        "int16_t" => "int16_t".into(),
        "int8_t" => "int8_t".into(),
        "uint64_t" => "uint64_t".into(),
        "uint32_t" => "uint32_t".into(),
        "uint16_t" => "uint16_t".into(),
        "uint8_t" => "uint8_t".into(),
        "double" => "double".into(),
        "float" => "float".into(),
        "bool" => "bool".into(),
        _ => suffix.to_string(), // struct types use their name directly
    }
}

/// Like `box_alloc_suffix_to_c_type` but with fallback to the LIR param type.
pub(super) fn box_alloc_inner_c_type(suffix: &str, lir_ty: &LirType, struct_names: &HashMap<u32, String>) -> String {
    // For GorgetString: LIR type is Ptr (void*) but real C type is Str
    if suffix == "Str" || suffix == "GorgetString" {
        return "Str".into();
    }
    // For struct types, use the suffix (which is the monomorphized struct name)
    if let LirType::Struct(_) = lir_ty {
        return c_type_named(lir_ty, struct_names);
    }
    // For primitives, the LIR type is accurate
    c_type_named(lir_ty, struct_names)
}
// `struct_contains_resource` was lifted to `crate::lir::queries` and is
// re-exported at the top of this module.

pub(super) fn c_type_named(ty: &LirType, struct_names: &HashMap<u32, String>) -> String {
    match ty {
        LirType::I8 => "int8_t".into(),
        LirType::I16 => "int16_t".into(),
        LirType::I32 => "int32_t".into(),
        LirType::I64 => "int64_t".into(),
        LirType::U8 => "uint8_t".into(),
        LirType::U16 => "uint16_t".into(),
        LirType::U32 => "uint32_t".into(),
        LirType::U64 => "uint64_t".into(),
        LirType::F32 => "float".into(),
        LirType::F64 => "double".into(),
        LirType::Bool => "bool".into(),
        LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef => "void*".into(),
        LirType::Struct(id) => struct_names
            .get(&id.0)
            .cloned()
            .unwrap_or_else(|| format!("__lir_s{}", id.0)),
        // Item 7e (Phase 1): Resource is ABI-equivalent to its runtime struct
        // form. At this layer we render the C runtime struct name; the typed
        // `params` field is consumed by lowering / consumer migration paths,
        // not by this name-formatter.
        LirType::Resource { kind, .. } => match kind {
            crate::lir::ResourceKind::GorgetString => "GorgetString".into(),
            crate::lir::ResourceKind::GorgetArray => "GorgetArray".into(),
            crate::lir::ResourceKind::GorgetMap => "GorgetMap".into(),
            crate::lir::ResourceKind::GorgetSet => "GorgetSet".into(),
            crate::lir::ResourceKind::GorgetClosure => "GorgetClosure".into(),
            crate::lir::ResourceKind::RefCounted => "void*".into(),
        },
        LirType::Void => "void".into(),
    }
}
/// Emit a test runner `main()` that calls each test function and reports results.
/// Mirrors `emit_test_runner_main` in the old C backend (`src/backend/c/mod.rs`).
pub(super) fn emit_test_runner_main(out: &mut String, module: &LirModule) {
    let test_fns = &module.test_fns;
    let has_any_timeout = test_fns.iter().any(|t| t.timeout_ms.is_some());

    writeln!(out, "int main(int argc, char** argv) {{").unwrap();
    writeln!(out, "    gorget_init_args(argc, argv);").unwrap();
    if let Some(ref trace_path) = module.trace_filename {
        let escaped = trace_path.replace('\\', "\\\\").replace('"', "\\\"");
        writeln!(out, "    __gorget_trace_init(\"{escaped}\");").unwrap();
    }
    writeln!(out, "    int __test_passed = 0, __test_failed = 0, __test_skipped = 0;").unwrap();
    writeln!(out, "    int __nocapture = (getenv(\"GORGET_TEST_NOCAPTURE\") != NULL);").unwrap();
    writeln!(out, "    struct timespec __total_start, __total_end;").unwrap();
    writeln!(out, "    clock_gettime(CLOCK_MONOTONIC, &__total_start);").unwrap();

    // Parallel support
    writeln!(out, "    int __par_id = -1, __par_total = 0;").unwrap();
    writeln!(out, "    const char* __par_id_env = getenv(\"GORGET_PARALLEL_ID\");").unwrap();
    writeln!(out, "    const char* __par_total_env = getenv(\"GORGET_PARALLEL_TOTAL\");").unwrap();
    writeln!(out, "    if (__par_id_env && __par_total_env) {{ __par_id = atoi(__par_id_env); __par_total = atoi(__par_total_env); }}").unwrap();

    // Result file support
    writeln!(out, "    const char* __results_path = getenv(\"GORGET_TEST_RESULTS\");").unwrap();
    writeln!(out, "    __gorget_snapshot_open();").unwrap();

    writeln!(out, "    int __test_total = {};", test_fns.len()).unwrap();
    writeln!(out, "    if (__par_total > 0) {{").unwrap();
    writeln!(out, "        __test_total = 0;").unwrap();
    writeln!(out, "        for (int __i = 0; __i < {}; __i++) if (__i % __par_total == __par_id) __test_total++;", test_fns.len()).unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "    printf(\"Running %d tests...\\n\", __test_total);").unwrap();

    if module.has_suite_setup {
        writeln!(out, "    __suite_setup();").unwrap();
    }

    writeln!(out, "    int __results[{}];", test_fns.len()).unwrap();
    writeln!(out, "    memset(__results, 0, sizeof(__results));").unwrap();

    for (idx, info) in test_fns.iter().enumerate() {
        let escaped = info.display_name.replace('\\', "\\\\").replace('"', "\\\"");
        let fn_name = c_func_name(&info.fn_name);

        writeln!(out, "    if (__par_total > 0 && ({idx} % __par_total != __par_id)) goto __test_done_{idx};").unwrap();

        if info.skipped {
            writeln!(out, "    printf(\"  test: {escaped} ... \");").unwrap();
            if let Some(ref reason) = info.skip_reason {
                let escaped_reason = reason.replace('\\', "\\\\").replace('"', "\\\"");
                writeln!(out, "    printf(\"SKIP ({escaped_reason})\\n\");").unwrap();
            } else {
                writeln!(out, "    printf(\"SKIP\\n\");").unwrap();
            }
            writeln!(out, "    __test_skipped++;").unwrap();
            writeln!(out, "    goto __test_done_{idx};").unwrap();
        }

        if !info.skipped {
            // Trace: test_start event
            if module.trace_filename.is_some() {
                writeln!(out, "    if (__gorget_trace_fp) fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"test_start\\\",\\\"name\\\":\\\"{escaped}\\\"}}\\n\");").unwrap();
            }
            writeln!(out, "    printf(\"  test: {escaped} ... \");").unwrap();
            writeln!(out, "    fflush(stdout);").unwrap();
            writeln!(out, "    {{").unwrap();
            writeln!(out, "        __gorget_in_test = 1;").unwrap();
            writeln!(out, "        __gorget_test_fail_msg = NULL;").unwrap();
            writeln!(out, "        __gorget_test_timed_out = 0;").unwrap();
            writeln!(out, "        __gorget_current_test = \"{escaped}\";").unwrap();
            writeln!(out, "        int __cleanup_mark = __gorget_cleanup_top;").unwrap();
            writeln!(out, "        struct timespec __t_start, __t_end;").unwrap();
            writeln!(out, "        clock_gettime(CLOCK_MONOTONIC, &__t_start);").unwrap();
            writeln!(out, "        __gorget_test_cleanup_mark = __cleanup_mark;").unwrap();

            if let Some(ms) = info.timeout_ms {
                writeln!(out, "        __gorget_set_timeout({ms}L);").unwrap();
            }

            writeln!(out, "        if (!__nocapture) __gorget_capture_start();").unwrap();
            writeln!(out, "        int __jmp_val = setjmp(__gorget_test_jmp);").unwrap();
            writeln!(out, "        if (__jmp_val == 0) {{").unwrap();
            writeln!(out, "            {fn_name}();").unwrap();
            writeln!(out, "            __gorget_cleanup_top = __cleanup_mark;").unwrap();
            writeln!(out, "        }}").unwrap();

            if info.timeout_ms.is_some() {
                writeln!(out, "        __gorget_cancel_timeout();").unwrap();
            }

            // On timeout (jmp_val==2): cleanup was NOT run by signal handler, run it now
            // On panic (jmp_val==1): gorget_panic already ran cleanup, this is a no-op
            writeln!(out, "        __gorget_cleanup_run(__cleanup_mark);").unwrap();
            writeln!(out, "        __gorget_in_test = 0;").unwrap();
            writeln!(out, "        size_t __cap_len = 0;").unwrap();
            writeln!(out, "        const char *__cap_buf = __gorget_capture_end(&__cap_len);").unwrap();

            writeln!(out, "        clock_gettime(CLOCK_MONOTONIC, &__t_end);").unwrap();
            writeln!(out, "        long __t_ms = (__t_end.tv_sec - __t_start.tv_sec) * 1000 + (__t_end.tv_nsec - __t_start.tv_nsec) / 1000000;").unwrap();

            // Timeout always fails
            if has_any_timeout {
                writeln!(out, "        if (__gorget_test_timed_out) {{").unwrap();
                if let Some(ms) = info.timeout_ms {
                    writeln!(out, "            __test_failed++; __results[{idx}] = 2;").unwrap();
                    writeln!(out, "            printf(\"FAIL: timed out after {ms}ms (%ldms)\\n\", __t_ms);").unwrap();
                } else {
                    writeln!(out, "            __test_failed++; __results[{idx}] = 2;").unwrap();
                    writeln!(out, "            printf(\"FAIL: timed out (%ldms)\\n\", __t_ms);").unwrap();
                }
                writeln!(out, "        }} else").unwrap();
            }

            if info.should_panic {
                if let Some(ref msg) = info.expected_panic_msg {
                    let emsg = msg.replace('\\', "\\\\").replace('"', "\\\"");
                    writeln!(out, "        if (__gorget_test_fail_msg && strstr(__gorget_test_fail_msg, \"{emsg}\")) {{").unwrap();
                    writeln!(out, "            __test_passed++; __results[{idx}] = 1;").unwrap();
                    writeln!(out, "            printf(\"PASS (%ldms)\\n\", __t_ms);").unwrap();
                    writeln!(out, "        }} else if (__gorget_test_fail_msg) {{").unwrap();
                    writeln!(out, "            __test_failed++; __results[{idx}] = 2;").unwrap();
                    writeln!(out, "            printf(\"FAIL: expected panic containing \\\"{emsg}\\\", got: %s (%ldms)\\n\", __gorget_test_fail_msg, __t_ms);").unwrap();
                    writeln!(out, "        }} else {{").unwrap();
                    writeln!(out, "            __test_failed++; __results[{idx}] = 2;").unwrap();
                    writeln!(out, "            printf(\"FAIL: expected panic but test passed (%ldms)\\n\", __t_ms);").unwrap();
                    writeln!(out, "        }}").unwrap();
                } else {
                    writeln!(out, "        if (__gorget_test_fail_msg) {{").unwrap();
                    writeln!(out, "            __test_passed++; __results[{idx}] = 1;").unwrap();
                    writeln!(out, "            printf(\"PASS (%ldms)\\n\", __t_ms);").unwrap();
                    writeln!(out, "        }} else {{").unwrap();
                    writeln!(out, "            __test_failed++; __results[{idx}] = 2;").unwrap();
                    writeln!(out, "            printf(\"FAIL: expected panic but test passed (%ldms)\\n\", __t_ms);").unwrap();
                    writeln!(out, "        }}").unwrap();
                }
            } else {
                writeln!(out, "        if (!__gorget_test_fail_msg) {{").unwrap();
                writeln!(out, "            __test_passed++; __results[{idx}] = 1;").unwrap();
                writeln!(out, "            printf(\"PASS (%ldms)\\n\", __t_ms);").unwrap();
                writeln!(out, "        }} else {{").unwrap();
                writeln!(out, "            __test_failed++; __results[{idx}] = 2;").unwrap();
                writeln!(out, "            printf(\"FAIL: %s (%ldms)\\n\", __gorget_test_fail_msg, __t_ms);").unwrap();
                writeln!(out, "        }}").unwrap();
            }

            // Dump captured output on failure.
            writeln!(out, "        if (__results[{idx}] == 2 && __cap_len > 0) {{").unwrap();
            writeln!(out, "            printf(\"    --- captured output ---\\n\");").unwrap();
            writeln!(out, "            fwrite(__cap_buf, 1, __cap_len, stdout);").unwrap();
            writeln!(out, "            if (__cap_len > 0 && __cap_buf[__cap_len - 1] != '\\n') printf(\"\\n\");").unwrap();
            writeln!(out, "            printf(\"    ---\\n\");").unwrap();
            writeln!(out, "        }}").unwrap();

            // Trace: test_end event with status and duration
            if module.trace_filename.is_some() {
                writeln!(out, "        if (__gorget_trace_fp) fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"test_end\\\",\\\"name\\\":\\\"{escaped}\\\",\\\"status\\\":\\\"%s\\\",\\\"duration_ms\\\":%ld}}\\n\", __results[{idx}] == 1 ? \"pass\" : __results[{idx}] == 2 ? \"fail\" : \"skip\", __t_ms);").unwrap();
            }
            writeln!(out, "    }}").unwrap();
        }

        writeln!(out, "    __test_done_{idx}:;").unwrap();
    }

    if module.has_suite_teardown {
        writeln!(out, "    __suite_teardown();").unwrap();
    }

    // Summary
    writeln!(out, "    clock_gettime(CLOCK_MONOTONIC, &__total_end);").unwrap();
    writeln!(out, "    long __total_ms = (__total_end.tv_sec - __total_start.tv_sec) * 1000 + (__total_end.tv_nsec - __total_start.tv_nsec) / 1000000;").unwrap();
    writeln!(out, "    if (__test_skipped > 0) printf(\"\\n%d passed, %d failed, %d skipped (%ldms)\\n\", __test_passed, __test_failed, __test_skipped, __total_ms);").unwrap();
    writeln!(out, "    else printf(\"\\n%d passed, %d failed (%ldms)\\n\", __test_passed, __test_failed, __total_ms);").unwrap();

    // Write results file
    writeln!(out, "    if (__results_path) {{").unwrap();
    writeln!(out, "        FILE* __rf = fopen(__results_path, \"w\");").unwrap();
    writeln!(out, "        if (__rf) {{").unwrap();
    writeln!(out, "            fprintf(__rf, \"{{\\\"results\\\":[\\n\");").unwrap();
    for (idx, info) in test_fns.iter().enumerate() {
        let escaped = info.display_name.replace('\\', "\\\\").replace('"', "\\\"");
        let comma = if idx + 1 < test_fns.len() { "," } else { "" };
        writeln!(out, "            fprintf(__rf, \"  {{\\\"name\\\":\\\"{escaped}\\\",\\\"status\\\":\\\"%s\\\"}}{comma}\\n\", __results[{idx}] == 1 ? \"pass\" : __results[{idx}] == 2 ? \"fail\" : \"skip\");").unwrap();
    }
    writeln!(out, "            fprintf(__rf, \"]}}\\n\");").unwrap();
    writeln!(out, "            fclose(__rf);").unwrap();
    writeln!(out, "        }}").unwrap();
    writeln!(out, "    }}").unwrap();

    writeln!(out, "    __gorget_snapshot_close();").unwrap();
    writeln!(out, "    return __test_failed > 0 ? 1 : 0;").unwrap();
    writeln!(out, "}}").unwrap();
}

/// Emit a bench runner `main()` that calls each benchmark function,
/// with warmup, auto-calibration, and timing.
pub(super) fn emit_bench_runner_main(out: &mut String, module: &LirModule) {
    writeln!(out, "int main(int argc, char** argv) {{").unwrap();
    writeln!(out, "    gorget_init_args(argc, argv);").unwrap();
    writeln!(out, "    int __bench_count = {};", module.bench_fns.len()).unwrap();
    writeln!(out, "    printf(\"Running %d benchmarks...\\n\\n\", __bench_count);").unwrap();

    // Suite setup if present
    if module.has_suite_setup {
        writeln!(out, "    __suite_setup();").unwrap();
    }

    for info in &module.bench_fns {
        let escaped = info.display_name.replace('\\', "\\\\").replace('"', "\\\"");
        let fn_name = c_func_name(&info.fn_name);

        writeln!(out, "    {{").unwrap();
        // Warmup: 3 iterations
        writeln!(out, "        for (int __w = 0; __w < 3; __w++) {fn_name}();").unwrap();

        // Auto-calibrate: start at 100 iterations, double until >= 1 second
        writeln!(out, "        uint64_t __iters = 100;").unwrap();
        writeln!(out, "        uint64_t __total_ns = 0;").unwrap();
        writeln!(out, "        for (;;) {{").unwrap();
        writeln!(out, "            struct timespec __bs, __be;").unwrap();
        writeln!(out, "            clock_gettime(CLOCK_MONOTONIC, &__bs);").unwrap();
        writeln!(out, "            for (uint64_t __i = 0; __i < __iters; __i++) {fn_name}();").unwrap();
        writeln!(out, "            clock_gettime(CLOCK_MONOTONIC, &__be);").unwrap();
        writeln!(out, "            __total_ns = (uint64_t)(__be.tv_sec - __bs.tv_sec) * 1000000000ULL").unwrap();
        writeln!(out, "                       + (uint64_t)(__be.tv_nsec - __bs.tv_nsec);").unwrap();
        writeln!(out, "            if (__total_ns >= 1000000000ULL) break;").unwrap();
        writeln!(out, "            if (__total_ns < 10000000ULL) __iters *= 100;").unwrap();
        writeln!(out, "            else __iters *= 2;").unwrap();
        writeln!(out, "        }}").unwrap();

        // Format and print result
        writeln!(out, "        double __avg_ns = (double)__total_ns / (double)__iters;").unwrap();
        writeln!(out, r#"        if (__avg_ns < 1000.0) printf("  bench: {escaped} ... %llu iters, %.0f ns/iter\n", (unsigned long long)__iters, __avg_ns);"#).unwrap();
        writeln!(out, r#"        else if (__avg_ns < 1000000.0) printf("  bench: {escaped} ... %llu iters, %.2f us/iter\n", (unsigned long long)__iters, __avg_ns / 1000.0);"#).unwrap();
        writeln!(out, r#"        else if (__avg_ns < 1000000000.0) printf("  bench: {escaped} ... %llu iters, %.2f ms/iter\n", (unsigned long long)__iters, __avg_ns / 1000000.0);"#).unwrap();
        writeln!(out, r#"        else printf("  bench: {escaped} ... %llu iters, %.2f s/iter\n", (unsigned long long)__iters, __avg_ns / 1000000000.0);"#).unwrap();
        writeln!(out, "    }}").unwrap();
    }

    // Suite teardown if present
    if module.has_suite_teardown {
        writeln!(out, "    __suite_teardown();").unwrap();
    }

    writeln!(out, "    printf(\"\\n%d benchmarks complete.\\n\", __bench_count);").unwrap();
    writeln!(out, "    return 0;").unwrap();
    writeln!(out, "}}").unwrap();
}
/// Returns true if a struct (by StructId) directly is a resource type (GorgetArray, etc.)
/// or transitively contains resource-type fields that would be double-freed on shallow copy.
#[allow(dead_code)]
pub(super) fn struct_contains_resources(sid: crate::lir::StructId, module: &crate::lir::LirModule) -> bool {
    if let Some(sdef) = module.structs.get(sid.0 as usize) {
        if matches!(sdef.name.as_str(),
            "GorgetArray" | "GorgetMap" | "GorgetSet" | "GorgetString" | "GorgetClosure"
        ) {
            return true;
        }
        // Check fields recursively (one level deep is sufficient for all current types).
        for (_, fty) in &sdef.fields {
            if let LirType::Struct(fsid) = fty {
                if let Some(fdef) = module.structs.get(fsid.0 as usize) {
                    if matches!(fdef.name.as_str(),
                        "GorgetArray" | "GorgetMap" | "GorgetSet" | "GorgetString" | "GorgetClosure"
                    ) {
                        return true;
                    }
                    // Two levels deep for enums containing structs containing arrays.
                    for (_, ffty) in &fdef.fields {
                        if let LirType::Struct(ffsid) = ffty {
                            if let Some(ffdef) = module.structs.get(ffsid.0 as usize) {
                                if matches!(ffdef.name.as_str(),
                                    "GorgetArray" | "GorgetMap" | "GorgetSet" | "GorgetString" | "GorgetClosure"
                                ) {
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    false
}

/// Generate deep-clone operations for resource-type fields within a struct.
#[allow(dead_code)]
/// Returns `Some(Vec<String>)` if the struct contains resource fields that need
/// individual cloning to prevent double-free on shallow copy. Each string is a
/// C statement like `{dst}.field = gorget_array_clone(&{dst}.field);`.
///
/// `dst_expr` is the C expression for the destination (e.g., `__v83.Some_0` or `__s5`).
pub(super) fn deep_clone_resource_fields(
    sid: crate::lir::StructId,
    dst_expr: &str,
    module: &crate::lir::LirModule,
) -> Option<Vec<String>> {
    let sdef = module.structs.get(sid.0 as usize)?;
    // Skip direct resource types — they use gorget_array_clone etc. directly
    if matches!(sdef.name.as_str(),
        "GorgetArray" | "GorgetMap" | "GorgetSet" | "GorgetString" | "GorgetClosure"
    ) {
        return None;
    }
    // Skip enums — variants are stored in a union, can't clone all fields at once.
    // Enum element deep-clone requires match-on-tag which is handled separately.
    if sdef.is_enum() {
        return None;
    }
    let mut ops = Vec::new();
    for (fname, fty) in &sdef.fields {
        if let LirType::Struct(fsid) = fty {
            if let Some(fdef) = module.structs.get(fsid.0 as usize) {
                if fdef.is_enum() {
                    // Enum fields: handle Option/Result types that wrap resources.
                    // Option__GorgetString layout: { tag, Some_0: Str }
                    // Only clone the payload when tag != 0 (Some variant).
                    if fdef.enum_kind == crate::lir::EnumKind::Option {
                        for (vfname, vfty) in &fdef.fields {
                            if vfname == "tag" { continue; }
                            if let LirType::Struct(vfsid) = vfty {
                                if let Some(vfdef) = module.structs.get(vfsid.0 as usize) {
                                    let clone_fn = match vfdef.name.as_str() {
                                        "GorgetArray" => Some("gorget_array_clone"),
                                        "GorgetMap" => Some("gorget_map_clone"),
                                        "GorgetSet" => Some("gorget_set_clone"),
                                        "GorgetString" => Some("gorget_string_clone_to_owned"),
                                        _ => None,
                                    };
                                    if let Some(cfn) = clone_fn {
                                        ops.push(format!(
                                            "if ({dst_expr}.{fname}.tag != 0) {{ {dst_expr}.{fname}.{vfname} = {cfn}(&{dst_expr}.{fname}.{vfname}); }}"
                                        ));
                                    }
                                }
                            }
                        }
                    }
                    continue;
                }
                let clone_fn = match fdef.name.as_str() {
                    "GorgetArray" => Some("gorget_array_clone"),
                    "GorgetMap" => Some("gorget_map_clone"),
                    "GorgetSet" => Some("gorget_set_clone"),
                    "GorgetString" => Some("gorget_string_clone_to_owned"),
                    _ => None,
                };
                if let Some(cfn) = clone_fn {
                    ops.push(format!("{dst_expr}.{fname} = {cfn}(&{dst_expr}.{fname});"));
                } else {
                    // Recurse one level: check if this nested struct has resource fields
                    for (ffname, ffty) in &fdef.fields {
                        if let LirType::Struct(ffsid) = ffty {
                            if let Some(ffdef) = module.structs.get(ffsid.0 as usize) {
                                if ffdef.is_enum() {
                                    // Handle nested Option fields too
                                    if ffdef.enum_kind == crate::lir::EnumKind::Option {
                                        for (vfname, vfty) in &ffdef.fields {
                                            if vfname == "tag" { continue; }
                                            if let LirType::Struct(vfsid) = vfty {
                                                if let Some(vfdef) = module.structs.get(vfsid.0 as usize) {
                                                    let inner_clone = match vfdef.name.as_str() {
                                                        "GorgetArray" => Some("gorget_array_clone"),
                                                        "GorgetMap" => Some("gorget_map_clone"),
                                                        "GorgetSet" => Some("gorget_set_clone"),
                                                        "GorgetString" => Some("gorget_string_clone_to_owned"),
                                                        _ => None,
                                                    };
                                                    if let Some(icfn) = inner_clone {
                                                        ops.push(format!(
                                                            "if ({dst_expr}.{fname}.{ffname}.tag != 0) {{ {dst_expr}.{fname}.{ffname}.{vfname} = {icfn}(&{dst_expr}.{fname}.{ffname}.{vfname}); }}"
                                                        ));
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    continue;
                                }
                                let inner_clone = match ffdef.name.as_str() {
                                    "GorgetArray" => Some("gorget_array_clone"),
                                    "GorgetMap" => Some("gorget_map_clone"),
                                    "GorgetSet" => Some("gorget_set_clone"),
                                    "GorgetString" => Some("gorget_string_clone_to_owned"),
                                    _ => None,
                                };
                                if let Some(icfn) = inner_clone {
                                    ops.push(format!("{dst_expr}.{fname}.{ffname} = {icfn}(&{dst_expr}.{fname}.{ffname});"));
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    if ops.is_empty() { None } else { Some(ops) }
}
/// Fallback whitelist for C runtime functions where the signature uses `const char*`
/// but the GIR passes Gorget's `str` type. Only needed for Declaration-body methods
/// (name-mapped at LIR level, no .gg declaration) and internal runtime functions.
/// Functions declared as `extern "C"` in .gg files get ABI tags automatically.
/// Resolve the effective ABI kind for an extern param: explicit tag takes priority,
/// then structural runtime_arg_by_ptr fallback, then Auto.
pub(super) fn resolve_param_abi(
    ext_decl: Option<&LirExtern>,
    _fn_name: &str,
    param_idx: usize,
) -> crate::ir::abi::AbiKind {
    use crate::ir::abi::AbiKind;
    // Explicit tag from module declaration (extern "C" blocks, T* syntax, etc.)
    if let Some(abi) = ext_decl.and_then(|e| e.param_abis.get(param_idx)).copied() {
        if abi != AbiKind::Auto {
            return abi;
        }
    }
    // Legacy fallback for unmapped GIR names that bypass RuntimeFn::resolve_lir_sig.
    if param_idx == 0 && legacy_self_by_ptr(_fn_name) {
        return AbiKind::Ptr;
    }
    AbiKind::Auto
}

/// Sanitize a field name for C.
pub(super) fn c_field_name(name: &str) -> String {
    name.replace('.', "_").replace('-', "_")
}

/// C keywords and type names that cannot be used as identifiers.
pub(super) const C_RESERVED: &[&str] = &[
    "auto", "break", "case", "char", "const", "continue", "default", "do",
    "double", "else", "enum", "extern", "float", "for", "goto", "if",
    "int", "long", "register", "return", "short", "signed", "sizeof",
    "static", "struct", "switch", "typedef", "union", "unsigned", "void",
    "volatile", "while", "inline", "restrict", "_Bool", "_Complex",
    "_Imaginary", "bool", "true", "false",
];

/// Escape a function name that clashes with C keywords by adding a prefix.
pub(super) fn c_func_name(name: &str) -> String {
    if C_RESERVED.contains(&name) {
        format!("__gg_{name}")
    } else {
        name.to_string()
    }
}
/// Map an LIR type to the appropriate trace formatter function name.
pub(super) fn lir_trace_formatter(ty: &LirType, module: &LirModule) -> &'static str {
    match ty {
        LirType::Bool => "__gorget_trace_val_bool",
        LirType::F32 | LirType::F64 => "__gorget_trace_val_float",
        LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64
        | LirType::U8 | LirType::U16 | LirType::U32 | LirType::U64 => "__gorget_trace_val_int",
        LirType::Struct(sid) => {
            if is_str_struct_id(sid, module) {
                return "__gorget_trace_val_Str";
            }
            "__gorget_trace_val_int" // fallback
        }
        _ => "__gorget_trace_val_int", // fallback for Ptr, Void
    }
}

/// Format a float for C source.
pub(super) fn format_float(val: f64) -> String {
    if val.is_nan() {
        "NAN".into()
    } else if val.is_infinite() {
        if val > 0.0 {
            "INFINITY".into()
        } else {
            "(-INFINITY)".into()
        }
    } else {
        // Use enough precision to round-trip.
        format!("{:.17e}", val)
    }
}

// Printf format rewriting (fix_printf_format, PrintfArgKind) has been moved to
// LIR lowering in src/lir/lower/calls.rs. The C backend no longer does format
// string rewriting — all float/bool/string format fixes happen before codegen.

/// True iff `init` is the canonical lowering of a module-level `String FOO =
/// "literal"` declaration AND the target slot is a `GorgetString` (a.k.a.
/// `Str`) struct. Such inits would otherwise emit a runtime
/// `gorget_str_from_literal` call at `main()` prologue — heap-allocating via
/// `str_alloc_copy` — even though the data is a compile-time constant. We
/// detect this exact shape and reroute the emit to a static rodata-view
/// initializer (cap=0), making the global zero-alloc, zero-free, and zero-
/// cost to read. Both C and LLVM backends consult this predicate.
pub(crate) fn is_str_literal_view_init(
    name: &str,
    args: &[crate::lir::LirGlobalInitArg],
    ty: &crate::lir::LirType,
    structs: &[crate::lir::StructDef],
) -> bool {
    use crate::lir::{LirGlobalInitArg, LirType};
    if name != "gorget_str_from_literal" || args.len() != 2 {
        return false;
    }
    if !matches!(&args[0], LirGlobalInitArg::StrLit(_)) {
        return false;
    }
    if !matches!(&args[1], LirGlobalInitArg::Int(_)) {
        return false;
    }
    let sid = match ty {
        LirType::Struct(sid) => sid,
        _ => return false,
    };
    let struct_name = match structs.get(sid.0 as usize) {
        Some(sd) => sd.name.as_str(),
        None => return false,
    };
    matches!(struct_name, "GorgetString" | "Str")
}

/// Escape a string for C string literal.
pub(crate) fn escape_c_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 8);
    let chars: Vec<char> = s.chars().collect();
    for (ci, &c) in chars.iter().enumerate() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\0' => out.push_str("\\0"),
            c if c.is_ascii_graphic() || c == ' ' => out.push(c),
            c => {
                for byte in c.to_string().as_bytes() {
                    write!(out, "\\x{byte:02x}").unwrap();
                }
                // If the next character is a hex digit, break the string literal
                // to prevent C from consuming it as part of the \x escape.
                // e.g., \xc3\xa9 followed by 'b' would be parsed as \xc3 \xa9b (wrong).
                // Emitting "\xc3\xa9" "b" uses C string concatenation to avoid this.
                if let Some(&next) = chars.get(ci + 1) {
                    if next.is_ascii_hexdigit() {
                        out.push_str("\" \"");
                    }
                }
            }
        }
    }
    out
}
/// Infer the result type of an instruction (for variable declarations).
/// `val_types` provides already-resolved types for operands (used for arithmetic propagation).
pub(super) fn infer_inst_type(inst: &Inst, module: &LirModule, val_types: &[Option<LirType>], ptr_pointee: &[Option<LirType>], func: &LirFunction) -> Option<LirType> {
    match inst {
        Inst::SlotLoad { ty, .. } => Some(ty.clone()),
        Inst::SlotAddr { slot, .. } => {
            // Preserve PtrTo type from the slot for correct string deref downstream.
            let slot_ty = &func.slots[slot.0 as usize].ty;
            if let LirType::PtrTo(sid) = slot_ty {
                Some(LirType::PtrTo(*sid))
            } else {
                Some(LirType::Ptr)
            }
        }
        Inst::IConst { ty, .. } => Some(ty.clone()),
        Inst::FConst { ty, .. } => Some(ty.clone()),
        Inst::BoolConst { .. } => Some(LirType::Bool),
        Inst::NullPtr { .. } => Some(LirType::Ptr),
        Inst::FuncAddr { .. } => Some(LirType::FuncRef),
        Inst::GlobalAddr { .. } => Some(LirType::Ptr),
        Inst::StrLit { .. } => {
            // Under 32-byte Str, StrLit produces a Str struct value (not a raw pointer).
            // Find the GorgetString struct id so the value gets declared as `Str __vN`
            // rather than `void* __vN`. Fall back to Ptr if the struct isn't registered.
            module.structs.iter().enumerate()
                .find(|(_, s)| s.name == "GorgetString")
                .map(|(idx, _)| LirType::Struct(crate::lir::StructId(idx as u32)))
                .or(Some(LirType::Ptr))
        }
        Inst::ParamRef { ty, .. } => Some(ty.clone()),

        // Arithmetic — use the explicit type field.
        Inst::Add { ty, .. } | Inst::Sub { ty, .. } | Inst::Mul { ty, .. }
        | Inst::Div { ty, .. } | Inst::Rem { ty, .. } | Inst::Mod { ty, .. }
        | Inst::Neg { ty, .. } => Some(ty.clone()),

        // Bitwise — use the explicit type field.
        Inst::BitAnd { ty, .. } | Inst::BitOr { ty, .. } | Inst::BitXor { ty, .. }
        | Inst::Shl { ty, .. } | Inst::Shr { ty, .. }
        | Inst::BitNot { ty, .. } => Some(ty.clone()),

        Inst::Cmp { .. } | Inst::Not { .. } => Some(LirType::Bool),

        Inst::IntCast { to, .. } | Inst::FloatCast { to, .. }
        | Inst::IntToFloat { to, .. } | Inst::FloatToInt { to, .. }
        | Inst::Bitcast { to, .. } => Some(to.clone()),
        Inst::PtrCast { .. } => Some(LirType::Ptr),

        Inst::Load { ty, ptr, .. } => {
            // Trust the declared type; when the LIR lowerer emitted an
            // under-specified `Load.ty = Void`, fall back to the pointer's
            // pointee type from `ptr_pointee`. This keeps val_types usable
            // downstream when the LIR lowerer didn't propagate a concrete
            // field/element type onto the Load instruction itself.
            if matches!(ty, LirType::Void) {
                if let Some(Some(pt)) = ptr_pointee.get(ptr.0 as usize) {
                    return Some(pt.clone());
                }
            }
            Some(ty.clone())
        }
        Inst::FieldPtr { .. } | Inst::ElemPtr { .. } => Some(LirType::Ptr),

        Inst::Call { func, .. } => {
            Some(module.functions[func.0 as usize].return_type.clone())
        }
        Inst::CallExtern { name, .. } => {
            // (Previously: float/int/bool CallExtern return-type override
            // lived here. LIR Tier 3c in src/lir/lower/insts.rs rewrites
            // `float(x)` / `int(x)` / `bool(x)` / `int(string)` into
            // primitive cast instructions before BIR lowering, so no
            // CallExtern with these names survives into the backend.)
            // Polymorphic externs (option/result unwrap, expect, combinators) are
            // called with different return types at different call sites. The single
            // extern declaration merges all sites, producing the wrong type.
            // For unwrap/expect/unwrap_err, try to recover from the struct definition.
            if is_polymorphic_extern(name) {
                // Try to infer from struct definition for unwrap/expect/unwrap_err.
                let is_unwrap_err = name.ends_with("__unwrap_err") || name.ends_with("__unwrap_error");
                let is_unwrap = !is_unwrap_err && (is_option_result_unwrap(name) || is_option_result_expect(name));
                if is_unwrap || is_unwrap_err {
                    // Parse struct prefix from name: Option__T__unwrap → Option__T
                    // Result__T__S__unwrap_error → Result__T__S
                    let suffix = if name.ends_with("__unwrap_error") { "__unwrap_error" }
                        else if name.ends_with("__unwrap_err") { "__unwrap_err" }
                        else if name.ends_with("__expect") { "__expect" }
                        else if name.ends_with("__unwrap_or") { "__unwrap_or" }
                        else if name.ends_with("__unwrap_or_else") { "__unwrap_or_else" }
                        else { "__unwrap" };
                    if let Some(prefix) = name.strip_suffix(suffix) {
                        let found = module.structs.iter().find(|s| s.name == prefix);
                        if let Some(sdef) = found {
                            // For unwrap/expect: payload is field 1 (Ok/Some)
                            // For unwrap_err: payload is field 2 (Error)
                            let field_idx = if is_unwrap_err { 2 } else { 1 };
                            if let Some((_, ty)) = sdef.fields.get(field_idx) {
                                return Some(ty.clone());
                            }
                        }
                    }
                }
                // Fallback: try to recover from arg type (for generic __option_unwrap etc.)
                // The arg is usually a pointer (SlotAddr) to an Option/Result struct.
                // Check both val_types (direct struct) and ptr_pointee (pointer to struct).
                if let Inst::CallExtern { args, .. } = inst {
                    if let Some(arg0) = args.first() {
                        let field_idx = if name.contains("unwrap_err") || name.contains("unwrap_error") { 2 } else { 1 };
                        // Try direct struct type
                        if let Some(Some(LirType::Struct(sid))) = val_types.get(arg0.0 as usize) {
                            if let Some(s) = module.structs.get(sid.0 as usize) {
                                if let Some((_, ty)) = s.fields.get(field_idx) {
                                    return Some(ty.clone());
                                }
                            }
                        }
                        // Try pointee type (arg is a pointer to the struct)
                        if let Some(Some(LirType::Struct(sid))) = ptr_pointee.get(arg0.0 as usize) {
                            if let Some(s) = module.structs.get(sid.0 as usize) {
                                if let Some((_, ty)) = s.fields.get(field_idx) {
                                    return Some(ty.clone());
                                }
                            }
                        }
                    }
                }
                None
            } else if let Some(rt) = runtime_fn_return_struct(name) {
                // Runtime functions that return struct types by value
                module.structs.iter().enumerate()
                    .find(|(_i, s)| s.name == rt)
                    .map(|(i, _)| LirType::Struct(StructId(i as u32)))
                    .or(Some(LirType::I64))
            } else {
                // For Shared__Vector__X__at, the extern may have wrong return type (I64
                // instead of the actual element type). Parse from the name.
                if let Some(inner) = name.strip_prefix("Shared__Vector__")
                    .and_then(|rest| rest.strip_suffix("__at"))
                {
                    return match inner {
                        "double" => Some(LirType::F64),
                        "float" => Some(LirType::F32),
                        "bool" => Some(LirType::Bool),
                        _ => Some(LirType::I64),
                    };
                }
                // For Shared__X__get, parse the element type similarly.
                if let Some(inner) = name.strip_prefix("Shared__")
                    .and_then(|rest| rest.strip_suffix("__get"))
                    .filter(|rest| !rest.contains("__"))
                {
                    return match inner {
                        "double" => Some(LirType::F64),
                        "float" => Some(LirType::F32),
                        "bool" => Some(LirType::Bool),
                        _ => Some(LirType::I64),
                    };
                }
                module.externs.iter()
                    .find(|e| &e.name == name)
                    .map(|e| e.return_type.clone())
                    .or(Some(LirType::I64))
            }
        }
        Inst::CallPtr { ret_ty, .. } => {
            if *ret_ty != LirType::Void {
                Some(ret_ty.clone())
            } else {
                None
            }
        }
        Inst::CallByRef { ret_ty, .. } => {
            if *ret_ty != LirType::Void {
                Some(ret_ty.clone())
            } else {
                None
            }
        }
        Inst::CallClosure { ret_ty, .. } => {
            if *ret_ty != LirType::Void { Some(ret_ty.clone()) } else { None }
        }

        _ => None,
    }
}

/// Runtime functions that return a named struct type by value.
pub(super) fn runtime_fn_return_struct(name: &str) -> Option<&'static str> {
    match name {
        "gorget_array_clone" | "gorget_array_new" | "gorget_array_with_capacity"
        | "gorget_array_sorted" | "gorget_array_reversed" | "gorget_array_unique"
        | "gorget_array_filter" | "gorget_array_map" | "gorget_array_zip"
        | "gorget_array_flat_map" | "gorget_array_flatten"
        | "gorget_str_split" | "gorget_str_chars" => Some("GorgetArray"),
        "gorget_map_new" | "gorget_map_clone" => Some("GorgetMap"),
        "gorget_set_new" | "gorget_set_clone" => Some("GorgetSet"),
        "gorget_string_new" | "gorget_string_adopt" | "gorget_string_from_concat"
        | "gorget_str_cat" | "gorget_string_format"
        | "gorget_string_format_alloc" => Some("GorgetString"),
        "gorget_file_open" => Some("GorgetFile"),
        _ => None,
    }
}

/// Collect deep-clone operations for a struct that contains resource-type fields.
/// Similar to GIR's `collect_clone_ops` but uses LIR struct info.
/// `path` is the C expression path to the struct (e.g., "dst" or "dst.Some_0").
/// Returns clone statements to be emitted after the shallow copy.
#[allow(dead_code)]
pub(super) fn collect_clone_ops_lir(
    struct_id: u32,
    path: &str,
    module: &LirModule,
    sn: &HashMap<u32, String>,
) -> Vec<String> {
    let mut ops = Vec::new();
    let Some(sdef) = module.structs.get(struct_id as usize) else { return ops };
    for (fname, fty) in &sdef.fields {
        let c_fname = c_field_name(fname);
        let field_path = format!("{path}.{c_fname}");
        match fty {
            LirType::Struct(sid) => {
                let name = sn.get(&sid.0).map(|s| s.as_str()).unwrap_or("");
                match name {
                    "GorgetArray" => ops.push(format!("{field_path} = gorget_array_clone(&{field_path});")),
                    "GorgetMap" => ops.push(format!("{field_path} = gorget_map_clone(&{field_path});")),
                    "GorgetSet" => ops.push(format!("{field_path} = gorget_set_clone(&{field_path});")),
                    "GorgetString" => ops.push(format!("{field_path} = gorget_string_clone(&{field_path});")),
                    // Str (GorgetString) is a borrowed view (data ptr + len), not owned — no clone needed.
                    "Str" => {}
                    _ => {
                        // Recurse into nested structs.
                        let nested = collect_clone_ops_lir(sid.0, &field_path, module, sn);
                        ops.extend(nested);
                    }
                }
            }
            _ => {}
        }
    }
    ops
}

/// Generate a `__gorget_cleanup_push(...)` call for a slot in a test function.
/// Returns None if the slot's type doesn't need cleanup stack registration.
pub(super) fn test_cleanup_push_code_lir(
    slot_idx: u32,
    func: &LirFunction,
    module: &LirModule,
    _sn: &HashMap<u32, String>,
) -> Option<String> {
    let slot = &func.slots[slot_idx as usize];
    let slot_ty = &slot.ty;

    // Get the struct name for struct-typed slots.
    let struct_name = if let LirType::Struct(sid) = slot_ty {
        // Use original GIR name from module.structs (not the __lir_sN alias).
        module.structs.get(sid.0 as usize).map(|s| s.name.as_str())
    } else {
        None
    };

    if let Some(name) = struct_name {
        // Box types: push raw pointer (no address-of since Box is a typedef for T*).
        // Concrete and trait-object boxes both clean up via free(slot) post the
        // thin-pointer redesign (commit 7034597d) — no longer need to discriminate.
        if name.starts_with("Box__") {
            return Some(format!("    __gorget_cleanup_push(free, (void*)__s{slot_idx});\n"));
        }

        // Resource types: read `elem_drop_fn` from the typed StructDef (alias-
        // aware lookup handles Vector__T → GorgetArray etc.). Replaces three
        // parallel name-prefix matches against `gorget_*_free` strings; the
        // runtime singletons (GorgetString/Array/Map/Set/Closure) and their
        // monomorphized aliases all carry the same canonical drop fn here.
        if let Some(sd) = module.struct_def_by_name(name) {
            if let Some(ref drop_fn) = sd.elem_drop_fn {
                return Some(format!("    __gorget_cleanup_push((__gorget_cleanup_fn){drop_fn}, (void*)&__s{slot_idx});\n"));
            }
        }

        // User struct with custom drop: check if a {Name}__drop function exists.
        let drop_fn_name = format!("{name}__drop");
        if module.functions.iter().any(|f| f.name == drop_fn_name) {
            return Some(format!("    __gorget_cleanup_push((__gorget_cleanup_fn){drop_fn_name}, (void*)&__s{slot_idx});\n"));
        }
    }

    // Ptr-typed slots that are named (e.g., Box[T] lowered as raw pointer).
    // In LIR, Box[T] may also appear as a Ptr slot when the Box typedef isn't used.
    if matches!(slot_ty, LirType::Ptr) {
        // Check if the slot name suggests it's a Box (heuristic).
        // Box slots in test functions are typically registered with `free`.
        // However, we can't reliably detect this without more type info, so skip.
        // The struct-typed Box path above handles the common case.
    }

    None
}
