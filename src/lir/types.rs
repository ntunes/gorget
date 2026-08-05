//! Type helpers and struct registry for LIR.

use super::{EnumKind, LirType, StructDef, StructId};
use std::collections::HashMap;

/// Registry mapping struct names to their StructIds.
/// Used during GIR→LIR lowering to look up or create struct definitions.
pub struct StructRegistry {
    name_to_id: HashMap<String, StructId>,
}

impl StructRegistry {
    pub fn new() -> Self {
        Self {
            name_to_id: HashMap::new(),
        }
    }

    /// Register a struct definition. Returns `None` if the name was new,
    /// or `Some(existing_id)` if it was already registered.
    pub fn register(&mut self, name: &str, id: StructId) -> Option<StructId> {
        self.name_to_id.insert(name.to_string(), id)
    }

    /// Look up a struct by name.
    pub fn lookup(&self, name: &str) -> Option<StructId> {
        self.name_to_id.get(name).copied()
    }

    /// Number of registered structs.
    pub fn len(&self) -> usize {
        self.name_to_id.len()
    }

    pub fn is_empty(&self) -> bool {
        self.name_to_id.is_empty()
    }
}

impl Default for StructRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Well-known struct layouts for Gorget runtime types.
pub fn builtin_struct_defs() -> Vec<StructDef> {
    vec![
        // GorgetString — 32-byte fat struct { data, cap, len, alloc }.
        // Field order matches the generic view-discriminator prefix: cap at offset +8
        // marks a view (cap == 0) vs owned (cap > 0). C layout: 4 × 8 = 32 bytes.
        StructDef {
            name: "GorgetString".into(),
            fields: vec![
                ("data".into(), LirType::Ptr),
                ("cap".into(), LirType::I64),
                ("len".into(), LirType::I64),
                ("alloc".into(), LirType::Ptr),
            ],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: Some(32), computed_c_align: Some(8),
            elem_drop_fn: Some("gorget_string_free".into()),
            elem_clone_fn: Some("gorget_string_clone_inplace".into()),
            materialize_fn: Some("gorget_string_materialize_inplace".into()),
            c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        },
        // GorgetArray — dynamic array (Vector[T] backing).
        // C layout: { data, cap, len, elem_size, alloc, elem_drop, elem_clone, elem_materialize } — 8 × 8 = 64 bytes.
        // Field order: cap at offset +8 matches the generic view-discriminator prefix.
        // LIR only models 4 fields; the extra 4 are runtime-internal.
        StructDef {
            name: "GorgetArray".into(),
            fields: vec![
                ("data".into(), LirType::Ptr),
                ("cap".into(), LirType::I64),
                ("len".into(), LirType::I64),
                ("elem_size".into(), LirType::I64),
            ],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: Some(64), computed_c_align: Some(8),
            elem_drop_fn: Some("gorget_array_free".into()),
            elem_clone_fn: Some("gorget_array_clone_inplace".into()),
            materialize_fn: None,
            c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        },
        // Closure — function pointer + environment. 2 × 8 = 16 bytes.
        StructDef {
            name: "GorgetClosure".into(),
            fields: vec![
                ("fn_ptr".into(), LirType::Ptr),
                ("env".into(), LirType::Ptr),
            ],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: Some(16), computed_c_align: Some(8),
            elem_drop_fn: Some("gorget_closure_free".into()),
            elem_clone_fn: Some("gorget_closure_clone_inplace".into()),
            materialize_fn: None,
            c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        },
        // Trait object — data pointer + vtable pointer. 2 × 8 = 16 bytes.
        StructDef {
            name: "TraitObj".into(),
            fields: vec![
                ("data".into(), LirType::Ptr),
                ("vtable".into(), LirType::Ptr),
            ],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: Some(16), computed_c_align: Some(8),
            elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        },
        // Task handle — task pointer + drop function. 2 × 8 = 16 bytes.
        StructDef {
            name: "TaskHandle".into(),
            fields: vec![
                ("task_ptr".into(), LirType::Ptr),
                ("drop_fn".into(), LirType::Ptr),
            ],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: Some(16), computed_c_align: Some(8),
            elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        },
        // GorgetMap — hash map backing Dict[K,V] and HashMap[K,V].
        // C layout: 24 fields × 8 = 192 bytes (see runtime_preamble.c GorgetMap
        // — 13 core layout/data fields + 6 runtime hook fn-ptrs + 5 D39
        // dense-mode fields appended at struct END). cap at offset +8 matches
        // the generic view-discriminator prefix shared with Str and GorgetArray.
        // All 24 fields are modeled explicitly here so FieldPtr, pointee
        // inference, and the LLVM struct-type layout all agree with the C ABI.
        // Field indexes: LLVM `SetCollectionBridge` hardcodes 11=hash_fn,
        // 12=eq_fn; BIR dense scaffolds hardcode 19=entries_keys,
        // 20=entries_values, 21=entries_len — preserving these positions is
        // load-bearing (DD#7 lock: new fields APPEND, never insert).
        StructDef {
            name: "GorgetMap".into(),
            fields: vec![
                // 0-12: core layout/data fields
                ("keys".into(), LirType::Ptr),
                ("cap".into(), LirType::I64),
                ("values".into(), LirType::Ptr),
                ("states".into(), LirType::Ptr),
                ("count".into(), LirType::I64),
                ("key_size".into(), LirType::I64),
                ("val_size".into(), LirType::I64),
                ("alloc".into(), LirType::Ptr),
                ("order".into(), LirType::Ptr),
                ("order_len".into(), LirType::I64),
                ("tombstones".into(), LirType::I64),
                ("hash_fn".into(), LirType::Ptr),
                ("eq_fn".into(), LirType::Ptr),
                // 13-18: runtime hook function pointers
                ("val_drop".into(), LirType::Ptr),
                ("val_clone".into(), LirType::Ptr),
                ("key_drop".into(), LirType::Ptr),
                ("key_clone".into(), LirType::Ptr),
                ("val_materialize".into(), LirType::Ptr),
                ("key_materialize".into(), LirType::Ptr),
                // 19-23: D39 dense-mode fields (NULL/0 in legacy mode)
                ("entries_keys".into(), LirType::Ptr),
                ("entries_values".into(), LirType::Ptr),
                ("entries_len".into(), LirType::I64),
                ("entries_cap".into(), LirType::I64),
                ("indices".into(), LirType::Ptr),
            ],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: Some(192), computed_c_align: Some(8),
            elem_drop_fn: Some("gorget_map_free".into()),
            elem_clone_fn: Some("gorget_map_clone_inplace".into()),
            materialize_fn: None,
            c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        },
        // GorgetSet — typedef alias for GorgetMap, backs Set[T] and HashSet[T].
        // Same C layout as GorgetMap: 24 fields × 8 = 192 bytes.
        StructDef {
            name: "GorgetSet".into(),
            fields: vec![
                // 0-12: core layout/data fields
                ("keys".into(), LirType::Ptr),
                ("cap".into(), LirType::I64),
                ("values".into(), LirType::Ptr),
                ("states".into(), LirType::Ptr),
                ("count".into(), LirType::I64),
                ("key_size".into(), LirType::I64),
                ("val_size".into(), LirType::I64),
                ("alloc".into(), LirType::Ptr),
                ("order".into(), LirType::Ptr),
                ("order_len".into(), LirType::I64),
                ("tombstones".into(), LirType::I64),
                ("hash_fn".into(), LirType::Ptr),
                ("eq_fn".into(), LirType::Ptr),
                // 13-18: runtime hook function pointers (Set: only key_drop/key_clone used)
                ("val_drop".into(), LirType::Ptr),
                ("val_clone".into(), LirType::Ptr),
                ("key_drop".into(), LirType::Ptr),
                ("key_clone".into(), LirType::Ptr),
                ("val_materialize".into(), LirType::Ptr),
                ("key_materialize".into(), LirType::Ptr),
                // 19-23: D39 dense-mode fields (NULL/0 in legacy mode)
                ("entries_keys".into(), LirType::Ptr),
                ("entries_values".into(), LirType::Ptr),
                ("entries_len".into(), LirType::I64),
                ("entries_cap".into(), LirType::I64),
                ("indices".into(), LirType::Ptr),
            ],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: Some(192), computed_c_align: Some(8),
            elem_drop_fn: Some("gorget_set_free".into()),
            elem_clone_fn: Some("gorget_set_clone_inplace".into()),
            materialize_fn: None,
            c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        },
        // GorgetRange — range iterator. 3 × 8 = 24 bytes.
        StructDef {
            name: "GorgetRange".into(),
            fields: vec![
                ("start".into(), LirType::I64),
                ("end".into(), LirType::I64),
                ("step".into(), LirType::I64),
            ],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
                      },
    ]
}

/// Size of a scalar LIR type in bytes. Returns `None` for aggregates and void.
pub fn scalar_size(ty: &LirType) -> Option<u32> {
    match ty {
        LirType::I8 | LirType::U8 | LirType::Bool => Some(1),
        LirType::I16 | LirType::U16 => Some(2),
        LirType::I32 | LirType::U32 | LirType::F32 => Some(4),
        LirType::I64 | LirType::U64 | LirType::F64 | LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef => Some(8),
        LirType::Struct(_) | LirType::Void => None,
        // Item 7e (Phase 1): only `RefCounted` resources are scalar
        // (pointer-shaped); the aggregate ones (GorgetArray/Map/Set/String/
        // Closure) are address-only just like `Struct(_)`.
        LirType::Resource { kind, .. } => match kind {
            crate::lir::ResourceKind::RefCounted => Some(8),
            _ => None,
        },
    }
}

// ── Value Type Derivation ──────────────────────────────────────────────────

use super::{LirFunction, LirModule, Inst, ValueId};

/// Derive the LIR type of a single instruction's result.
/// Returns `None` for instructions that don't produce a value.
///
/// `pointee_types` is consulted for `Inst::Load` when the load's declared
/// `ty` is `Void` (i.e. the lowerer didn't propagate a concrete pointee
/// type onto the instruction). This requires `compute_module_pointee_types`
/// to run BEFORE `compute_module_value_types` so the pointee table is
/// populated. Pass an empty slice if pointee info isn't available yet —
/// the function then degrades to the previous "trust the instruction's
/// declared type" behaviour.
fn infer_inst_type(
    inst: &Inst,
    func: &LirFunction,
    module: &LirModule,
    val_types: &[Option<LirType>],
    pointee_types: &[Option<LirType>],
) -> Option<LirType> {
    match inst {
        Inst::SlotLoad { ty, .. } | Inst::ParamRef { ty, .. } => {
            if *ty == LirType::Void { Some(LirType::Ptr) } else { Some(ty.clone()) }
        }
        Inst::SlotAddr { slot, .. } => {
            let slot_ty = &func.slots[slot.0 as usize].ty;
            match slot_ty {
                LirType::Struct(sid) | LirType::PtrTo(sid) => Some(LirType::PtrTo(*sid)),
                _ => Some(LirType::Ptr),
            }
        }
        Inst::IConst { ty, .. } | Inst::FConst { ty, .. } => Some(ty.clone()),
        Inst::BoolConst { .. } => Some(LirType::Bool),
        Inst::NullPtr { .. } | Inst::GlobalAddr { .. } => Some(LirType::Ptr),
        // Tier E §8.6: function addresses carry the typed `FuncRef` variant.
        Inst::FuncAddr { .. } | Inst::NamedFuncAddr { .. } => Some(LirType::FuncRef),
        Inst::StrLit { .. } => {
            module.structs.iter().enumerate()
                .find(|(_, s)| s.name == "GorgetString")
                .map(|(i, _)| LirType::Struct(StructId(i as u32)))
                .or(Some(LirType::Ptr))
        }

        // Arithmetic, bitwise — explicit type field.
        Inst::Add { ty, .. } | Inst::Sub { ty, .. } | Inst::Mul { ty, .. }
        | Inst::Div { ty, .. } | Inst::Rem { ty, .. } | Inst::Mod { ty, .. }
        | Inst::Neg { ty, .. }
        | Inst::BitAnd { ty, .. } | Inst::BitOr { ty, .. } | Inst::BitXor { ty, .. }
        | Inst::Shl { ty, .. } | Inst::Shr { ty, .. }
        | Inst::BitNot { ty, .. } => Some(ty.clone()),

        Inst::Cmp { .. } | Inst::Not { .. } | Inst::FaultCheck { .. } => Some(LirType::Bool),

        Inst::IntCast { to, .. } | Inst::FloatCast { to, .. }
        | Inst::IntToFloat { to, .. } | Inst::FloatToInt { to, .. }
        | Inst::Bitcast { to, .. } => Some(to.clone()),
        Inst::PtrCast { value, .. } => {
            // Preserve PtrTo through casts.
            match val_types.get(value.0 as usize).and_then(|t| t.as_ref()) {
                Some(LirType::PtrTo(sid)) => Some(LirType::PtrTo(*sid)),
                _ => Some(LirType::Ptr),
            }
        }

        Inst::Load { ty, ptr, .. } => {
            // When the lowerer emitted an under-specified `Load { ty: Void }`,
            // fall back to the pointer's pointee type from `pointee_types`.
            // Mirrors the C backend's local fallback, now consolidated upstream.
            if matches!(ty, LirType::Void) {
                if let Some(Some(pt)) = pointee_types.get(ptr.0 as usize) {
                    return Some(pt.clone());
                }
            }
            Some(ty.clone())
        }
        Inst::FieldPtr { .. } | Inst::ElemPtr { .. } => Some(LirType::Ptr),

        Inst::Call { func: fid, .. } => {
            Some(module.functions[fid.0 as usize].return_type.clone())
        }
        Inst::CallExtern { name, args, .. } => {
            infer_call_extern_type(name, args, module, val_types)
        }
        Inst::CallPtr { dst, ret_ty, .. } => {
            if dst.is_some() && !matches!(ret_ty, LirType::Void) {
                Some(ret_ty.clone())
            } else {
                None
            }
        }
        Inst::CallByRef { dst, ret_ty, .. } => {
            if dst.is_some() && !matches!(ret_ty, LirType::Void) {
                Some(ret_ty.clone())
            } else {
                None
            }
        }
        Inst::CallClosure { ret_ty, .. } => {
            if *ret_ty != LirType::Void { Some(ret_ty.clone()) } else { None }
        }
        Inst::InlineC { dst, .. } => {
            if dst.is_some() { Some(LirType::I64) } else { None }
        }
        // Item 7e Phase 2: typed collection constructor result.
        //
        // The instruction already carries typed `kind` + `elem_or_key` + `val`
        // (the `ElemMeta`s populated at lowering by `parse_collection_ctor_info`
        // and `elem_type_to_meta`). Lift those fields into a fully-typed
        // `LirType::Resource { kind, params }` so downstream consumers
        // (sort/sorted/unique dispatch, emit_collection_constructor, validation)
        // read element / key / value types from the typed operand rather than
        // re-parsing the mangled callee name.
        //
        // The mapping:
        //   CollectionCtorKind::Vector / Deque        → ResourceKind::GorgetArray, [elem]
        //   CollectionCtorKind::Set / HashSet         → ResourceKind::GorgetSet,   [elem]
        //   CollectionCtorKind::Dict / HashMap        → ResourceKind::GorgetMap,   [key, val]
        Inst::CollectionCtor { kind, elem_or_key, val, .. } => {
            use crate::lir::{CollectionCtorKind, ResourceKind};
            let resource_kind = match kind {
                CollectionCtorKind::Vector | CollectionCtorKind::Deque => ResourceKind::GorgetArray,
                CollectionCtorKind::Set    | CollectionCtorKind::HashSet => ResourceKind::GorgetSet,
                CollectionCtorKind::Dict   | CollectionCtorKind::HashMap => ResourceKind::GorgetMap,
            };
            let mut params = Vec::with_capacity(2);
            params.push(elem_meta_to_lir_type(elem_or_key, module));
            if let Some(v) = val {
                params.push(elem_meta_to_lir_type(v, module));
            }
            Some(LirType::Resource { kind: resource_kind, params })
        }
        _ => None,
    }
}

/// Convert an `ElemMeta` into the corresponding `LirType` operand.
///
/// Item 7e: writer-site helper invoked from `infer_inst_type` so the
/// `Inst::CollectionCtor`'s typed `ElemMeta` fields lift into the
/// parametric `LirType::Resource` form. UserType folds into a `Struct(sid)`
/// — the elem-type is a user struct/enum, identified by its registered
/// LIR StructId.
fn elem_meta_to_lir_type(meta: &crate::lir::ElemMeta, module: &LirModule) -> LirType {
    use crate::lir::{ElemMeta, ResourceKind};
    match meta {
        ElemMeta::Primitive(ty) => ty.clone(),
        ElemMeta::Resource(kind) => {
            // Build a Resource with the correct arity. For Phase 2 we
            // cannot reach into the original GIR name from here, so we
            // populate params with the kind's pointer-shaped placeholder
            // (Ptr) — which is the right ABI placeholder for nested
            // resources whose element type wasn't propagated at this
            // depth. Downstream consumers that need the nested element
            // type today still fall back to name parsing; closing that
            // gap is item 7e Phase 4's consumer-migration work.
            let arity = LirType::expected_resource_arity(*kind);
            let params = vec![LirType::Ptr; arity];
            LirType::Resource { kind: *kind, params }
        }
        ElemMeta::UserType(sid) => {
            // Read the registered struct's ABI shape — if it's a runtime
            // resource alias (Callable* aliased to GorgetClosure), prefer
            // the alias's Resource form for consistency with the kind-
            // routing axis.
            if let Some(sd) = module.structs.get(sid.0 as usize) {
                if sd.c_runtime_alias.as_deref() == Some("GorgetClosure") {
                    return LirType::Resource { kind: ResourceKind::GorgetClosure, params: vec![] };
                }
            }
            LirType::Struct(*sid)
        }
    }
}

/// Infer the return type of a CallExtern from its name and module metadata.
fn infer_call_extern_type(
    name: &str,
    args: &[ValueId],
    module: &LirModule,
    val_types: &[Option<LirType>],
) -> Option<LirType> {
    // Builtin type casts
    match name {
        "float" => return Some(LirType::F64),
        "int" => return Some(LirType::I64),
        "bool" => return Some(LirType::Bool),
        // libc allocators — BIR synthesis (e.g. `__gg_synth_sort_impl`)
        // emits raw `Inst::CallExtern { name: "malloc", ... }` without
        // registering the matching `LirExtern` in `module.externs`. Without
        // this special-case the fallback at the bottom returns `LirType::I64`
        // for malloc, downstream `free(aux)` arg-emit picks `i64 %v8` against
        // the preamble's `declare void @free(ptr)`, and llc rejects it.
        "malloc" | "calloc" | "realloc" => return Some(LirType::Ptr),
        _ => {}
    }
    // Tag checks always return bool
    if name == "__option_is_some" || name == "__option_is_none"
        || name.ends_with("__is_some") || name.ends_with("__is_none")
        || name.ends_with("__is_ok") || name.ends_with("__is_err")
    {
        return Some(LirType::Bool);
    }

    // unwrap_error: error payload type from the Result struct (last field)
    let is_unwrap_err = name == "__result_unwrap_error"
        || name.ends_with("__unwrap_error") || name.ends_with("__unwrap_err");
    if is_unwrap_err && !args.is_empty() {
        if let Some(ty) = extract_payload_type(args[0], 2, module, val_types) {
            return Some(ty);
        }
    }

    // unwrap/expect/unwrap_or: payload type from Option/Result struct
    let is_unwrap = name == "__option_unwrap" || name == "__result_unwrap"
        || name.ends_with("__unwrap") || name.ends_with("__expect")
        || name.ends_with("__unwrap_or") || name.ends_with("__unwrap_or_else");
    if is_unwrap && !args.is_empty() {
        if let Some(ty) = extract_payload_type(args[0], 1, module, val_types) {
            return Some(ty);
        }
        // Try parsing struct name from the extern name for typed variants
        let suffixes = [
            "__unwrap_error", "__unwrap_err", "__expect",
            "__unwrap_or_else", "__unwrap_or", "__unwrap",
        ];
        for suffix in &suffixes {
            if let Some(prefix) = name.strip_suffix(suffix) {
                let field_idx = if suffix.contains("err") { 2 } else { 1 };
                if let Some(sdef) = module.structs.iter().find(|s| s.name == prefix) {
                    if let Some((_, ty)) = sdef.fields.get(field_idx) {
                        return Some(if *ty == LirType::Void { LirType::Ptr } else { ty.clone() });
                    }
                }
                break;
            }
        }
    }

    // String-returning helpers
    if name == "gorget_str_default" || name == "gorget_str_str" {
        return find_struct_type("GorgetString", module);
    }

    // Parse methods return Option__T by value
    let is_parse = name.ends_with("__parse") && (name.starts_with("int") || name.starts_with("uint")
        || name == "double__parse" || name == "float__parse" || name == "bool__parse");
    if is_parse {
        if let Some(ext) = module.externs.iter().find(|e| e.name == name) {
            return Some(ext.return_type.clone());
        }
    }

    // Runtime functions that return struct types by value
    let rt_struct = match name {
        "gorget_array_clone" | "gorget_array_new" | "gorget_array_with_capacity"
        | "gorget_array_sorted" | "gorget_array_reversed" | "gorget_array_unique"
        | "gorget_array_filter" | "gorget_array_map" | "gorget_array_zip"
        | "gorget_array_flat_map" | "gorget_array_flatten" | "gorget_array_slice"
        | "gorget_array_concat" | "gorget_str_split" | "gorget_str_chars"
        | "gorget_str_bytes" | "gorget_str_codepoints" | "gorget_str_lines"
        | "gorget_str_splitn"
        | "gorget_map_items" | "gorget_map_keys" | "gorget_map_values"
        | "gorget_set_to_array"
        | "gorget_bytes_concat" | "gorget_bytes_from_hex" | "gorget_bytes_from_str"
        | "gorget_bytes_slice" => Some("GorgetArray"),
        "gorget_map_new" | "gorget_map_clone" | "gorget_map_new_like" | "gorget_map_new_str"
        | "gorget_dict_new" | "gorget_dict_new_str" => Some("GorgetMap"),
        "gorget_set_new" | "gorget_set_clone" | "gorget_set_new_like" | "gorget_set_new_str"
        | "gorget_ordered_set_new" | "gorget_ordered_set_new_str" => Some("GorgetSet"),
        "gorget_string_new" | "gorget_string_adopt" | "gorget_string_from_concat"
        | "gorget_string_borrow_view"
        | "gorget_str_cat" | "gorget_string_format"
        | "gorget_string_format_alloc"
        | "gorget_string_clone"
        | "gorget_string_clone_to_owned" | "gorget_string_concat"
        | "gorget_string_debug" | "gorget_string_from_str"
        | "gorget_int_to_str" | "gorget_float_to_str" | "gorget_char_to_str"
        | "gorget_bool_to_str" | "gorget_char_chr" | "gorget_codepoint_to_utf8"
        | "gorget_str_replace" | "gorget_str_replacen" | "gorget_str_repeat"
        | "gorget_str_join" | "gorget_str_reverse"
        | "gorget_str_pad_left" | "gorget_str_pad_right"
        | "gorget_str_lstrip" | "gorget_str_rstrip" | "gorget_str_strip"
        | "gorget_str_lstrip_ws" | "gorget_str_rstrip_ws" | "gorget_str_trim"
        | "gorget_str_to_lower" | "gorget_str_to_upper"
        | "gorget_str_substr" | "gorget_str_index"
        | "gorget_str_byte_slice" | "gorget_str_char_at" | "gorget_str_codepoint_at"
        | "gorget_str_empty" | "gorget_str_slice"
        | "gorget_str_from_bool" | "gorget_str_from_cstr" | "gorget_str_from_float"
        | "gorget_str_from_int" | "gorget_str_from_literal"
        | "gorget_str_removeprefix" | "gorget_str_removesuffix"
        | "gorget_process_read_stdout" | "gorget_process_read_stderr"
        | "gorget_bytes_to_hex" | "gorget_bytes_to_str" | "gorget_read_file" => Some("GorgetString"),
        "gorget_file_open" => Some("GorgetFile"),
        _ => None,
    };
    if let Some(sname) = rt_struct {
        if let Some(ty) = find_struct_type(sname, module) {
            return Some(ty);
        }
    }

    // Shared__Vector__X__at, Shared__X__get: element type from name
    if let Some(inner) = name.strip_prefix("Shared__Vector__")
        .and_then(|rest| rest.strip_suffix("__at"))
    {
        // Check if the element type is a struct before assuming scalar
        if let Some(ty) = find_struct_type(inner, module) {
            return Some(ty);
        }
        return Some(scalar_from_name(inner));
    }
    if let Some(inner) = name.strip_prefix("Shared__")
        .and_then(|rest| rest.strip_suffix("__get"))
        .filter(|rest| !rest.contains("__"))
    {
        // Check if the inner type is a struct (not just a scalar)
        if let Some(ty) = find_struct_type(inner, module) {
            return Some(ty);
        }
        return Some(scalar_from_name(inner));
    }

    // Polymorphic externs (Option/Result map/filter/and_then/or_else/map_err)
    // have merged return types in the extern declaration that don't match
    // specific call sites. The return type depends on the closure argument,
    // which the shared inference can't resolve — return None so backend
    // fixup phases handle them from context.
    let is_combinator = (name.contains("Option__") || name.contains("Result__"))
        && (name.ends_with("__map") || name.ends_with("__filter")
            || name.ends_with("__and_then") || name.ends_with("__or_else")
            || name.ends_with("__map_err"));
    if is_combinator {
        return None;
    }

    // Fallback: extern declaration
    module.externs.iter()
        .find(|e| e.name == *name)
        .map(|e| e.return_type.clone())
        .or(Some(LirType::I64))
}

/// Test-only re-export of [`infer_call_extern_type`]. Used by
/// `runtime::tests::struct_returns_are_recognized_by_infer` to assert that
/// every struct-returning RuntimeFn is recognized by the name-based
/// inference path. Not for production callers.
#[cfg(test)]
pub fn infer_call_extern_type_for_test(
    name: &str,
    args: &[ValueId],
    module: &LirModule,
    val_types: &[Option<LirType>],
) -> Option<LirType> {
    infer_call_extern_type(name, args, module, val_types)
}

/// Extract payload type from an Option/Result struct argument.
fn extract_payload_type(
    arg: ValueId,
    field_idx: usize,
    module: &LirModule,
    val_types: &[Option<LirType>],
) -> Option<LirType> {
    let arg_ty = val_types.get(arg.0 as usize).and_then(|t| t.as_ref());
    let sid = match arg_ty {
        Some(LirType::Struct(sid)) | Some(LirType::PtrTo(sid)) => *sid,
        _ => return None,
    };
    let sdef = module.structs.get(sid.0 as usize)?;
    let (_, ty) = sdef.fields.get(field_idx)?;
    Some(if *ty == LirType::Void { LirType::Ptr } else { ty.clone() })
}

/// Find a struct type by name in the module.
fn find_struct_type(name: &str, module: &LirModule) -> Option<LirType> {
    module.structs.iter().enumerate()
        .find(|(_, s)| s.name == name)
        .map(|(i, _)| LirType::Struct(StructId(i as u32)))
}

/// Map a mangled scalar name to LirType.
fn scalar_from_name(name: &str) -> LirType {
    match name {
        "double" => LirType::F64,
        "float" => LirType::F32,
        "bool" => LirType::Bool,
        _ => LirType::I64,
    }
}

/// Compute per-value type metadata for a function.
///
/// Single-pass derivation from instructions — replaces the duplicated
/// type-reconstruction loops that each backend maintained independently.
/// Call this after SSA + optimization, before backend emission.
pub fn compute_value_types(func: &mut LirFunction, module: &LirModule) {
    let n = func.value_count() as usize;
    let mut vt: Vec<Option<LirType>> = vec![None; n];

    // Block parameters carry explicit types.
    for block in &func.blocks {
        for (vid, ty) in &block.params {
            if (vid.0 as usize) < n {
                vt[vid.0 as usize] = Some(ty.clone());
            }
        }
    }

    // Instructions: derive from fields. Reads `func.pointee_types` for the
    // `Inst::Load { ty: Void }` fallback (matches C backend behaviour).
    // Callers should run `compute_pointee_types` BEFORE this pass; otherwise
    // pointee info is empty and the fallback degrades to "trust the
    // declared ty" (same as the previous behaviour).
    let pt = &func.pointee_types;
    for block in &func.blocks {
        for inst in &block.insts {
            if let Some(dst) = inst.dst() {
                if (dst.0 as usize) < n {
                    let ty = infer_inst_type(inst, func, module, &vt, pt);
                    if ty.is_some() {
                        vt[dst.0 as usize] = ty;
                    }
                }
            }
        }
    }

    apply_callextern_slotstore_override(func, module, &mut vt);

    func.value_types = vt;
}

/// CallExtern→SlotStore slot-type override.
///
/// When a `CallExtern` result is immediately stored into a slot whose type is
/// more specific than the extern's declared return type (e.g. extern returns
/// `void*` / `int64_t` but the GIR-typed slot is `Option[int]` / `GorgetArray`),
/// prefer the slot type. The slot type comes from GIR via SSA construction and
/// is authoritative; the extern's declared C return type is generic.
///
/// Excludes the case where the current inferred type is `Ptr` and the slot's
/// raw type is `GorgetString` (Struct or PtrTo): in that path the consumer
/// (`SlotStore`) wraps the pointer with `gorget_str_from_literal`, so the
/// value-side type must stay `Ptr`. Also skips `Ptr`/`Void` slot types —
/// they are no more specific than the inferred type.
///
/// Layering rule 4 (resolve-once write-through): this used to live as a fixup
/// in the C backend (`backend/c_lir/mod.rs`), now consolidated upstream so
/// `func.value_types` is the single source of truth.
fn apply_callextern_slotstore_override(
    func: &LirFunction,
    module: &LirModule,
    vt: &mut [Option<LirType>],
) {
    let n = vt.len();
    let str_struct_id: Option<StructId> = module.structs.iter().enumerate()
        .find(|(_, s)| s.name == "GorgetString")
        .map(|(i, _)| StructId(i as u32));
    let is_str_raw = |ty: &LirType| -> bool {
        match (ty, str_struct_id) {
            (LirType::Struct(sid), Some(s)) => *sid == s,
            (LirType::PtrTo(sid), Some(s)) => *sid == s,
            _ => false,
        }
    };
    let normed = |ty: LirType| -> LirType {
        if matches!(ty, LirType::PtrTo(_)) { LirType::Ptr } else { ty }
    };
    for block in &func.blocks {
        let insts = &block.insts;
        for (i, inst) in insts.iter().enumerate() {
            let Inst::CallExtern { dst: Some(d), .. } = inst else { continue; };
            let Some(Inst::SlotStore { slot, value, .. }) = insts.get(i + 1) else { continue; };
            if *value != *d { continue; }
            let raw_slot_ty = func.slots[slot.0 as usize].ty.clone();
            let slot_ty = normed(raw_slot_ty.clone());
            if matches!(slot_ty, LirType::Ptr | LirType::Void) { continue; }
            let didx = d.0 as usize;
            if didx >= n { continue; }
            let current = vt[didx].as_ref();
            let is_ptr_to_str = matches!(current, Some(LirType::Ptr)) && is_str_raw(&raw_slot_ty);
            if is_ptr_to_str { continue; }
            if current != Some(&slot_ty) {
                vt[didx] = Some(slot_ty);
            }
        }
    }
}

/// Compute value types for all functions in a module.
pub fn compute_module_value_types(module: &mut LirModule) {
    for i in 0..module.functions.len() {
        compute_function_value_types_at(module, i);
    }
}

/// Insert `Inst::SetCollectionBridge` after each typed `Inst::CollectionCtor`
/// whose key type is a user `@derive(Hashable, Equatable)` struct. The
/// IR-level distinction between user-keyed and primitive-keyed collections
/// is preserved as an explicit instruction so backends compile it 1:1
/// instead of scanning string-typed metadata.
///
/// Walks each function's blocks and inserts the new inst immediately
/// after the matching `Inst::CollectionCtor`. The collection's `ValueId`
/// is the same as the ctor's `dst`. The pass is idempotent — guards on
/// the user-hashable check (vector/deque/primitive-key cases trivially
/// short-circuit).
///
/// Must run after LIR construction (so the typed CollectionCtor insts exist)
/// and before BIR lowering (so the bridge instruction is in place when BIR
/// lowers CollectionCtor → CallExtern).
pub fn wire_collection_bridges(module: &mut LirModule) {
    use crate::lir::{CollectionCtorKind, ElemMeta, Inst};
    use crate::lir::queries;

    // Two passes — first scan, then mutate. Mutating mid-iteration
    // would invalidate the index into `block.insts`.
    let mut to_insert: Vec<(usize /*func*/, usize /*block*/, usize /*after_idx*/, Inst)> =
        Vec::new();
    for (fi, func) in module.functions.iter().enumerate() {
        for (bi, block) in func.blocks.iter().enumerate() {
            for (ii, inst) in block.insts.iter().enumerate() {
                let Inst::CollectionCtor { dst, kind, elem_or_key, str_keyed, .. } = inst
                else { continue; };
                // Only map-like / set-like collections need bridges; vectors
                // / deques don't dispatch by key.
                let is_set = match kind {
                    CollectionCtorKind::Vector | CollectionCtorKind::Deque => continue,
                    CollectionCtorKind::Set | CollectionCtorKind::HashSet => true,
                    CollectionCtorKind::Dict | CollectionCtorKind::HashMap => false,
                };
                // `_str` runtime variants pre-wire the String key bridges;
                // we MUST NOT emit user-side bridges for those.
                if *str_keyed { continue; }
                // Only `UserType` keys need bridges. Primitive (int/bool/…)
                // and Resource (GorgetString / nested collection) keys use
                // their own runtime paths.
                let key_struct = match elem_or_key {
                    ElemMeta::UserType(sid) => *sid,
                    _ => continue,
                };
                // Verify the user type carries hash + eq impls. If not,
                // that's a Hashable-derive error caught upstream — skip
                // silently here (no bridge to wire).
                let key_name = module.structs.get(key_struct.0 as usize)
                    .map(|s| s.name.as_str())
                    .unwrap_or("");
                if !queries::is_user_hashable_key(key_name, module) { continue; }
                to_insert.push((fi, bi, ii + 1, Inst::SetCollectionBridge {
                    collection: *dst,
                    is_set,
                    key_struct,
                }));
            }
        }
    }
    // Insert in reverse so earlier indices stay valid. Sort by
    // (func, block, idx) descending — `Inst` doesn't `Ord`.
    to_insert.sort_by(|a, b| {
        let ka = (a.0, a.1, a.2);
        let kb = (b.0, b.1, b.2);
        kb.cmp(&ka)
    });
    for (fi, bi, idx, inst) in to_insert {
        // SetCollectionBridge is a typed-derived synthesis (no source
        // span); keep the parallel-array invariant by inserting a `None`
        // span at the same index.
        module.functions[fi].blocks[bi].insert_inst(idx, inst, None);
    }
}

/// Populate `func.pointee_types` for every function in the module.
///
/// For each pointer-typed value, records the type the pointer addresses
/// when it can be derived from a canonical pointer-producing instruction:
///
/// - `SlotAddr { dst, slot }`            → the slot's declared type
/// - `FieldPtr { dst, struct_id, field }`→ that field's declared type
/// - `GlobalAddr { dst, global }`        → the global's declared type
/// - `PtrCast` / `Bitcast`               → propagate from source
/// - `SlotStore` of a known-pointee value into a `Ptr`/`PtrTo(_)` slot,
///   followed by `SlotLoad`              → propagate slot→load
/// - block-arg → block-param at fixed point (loops + diamonds)
///
/// `ElemPtr` is left `None` (no element-type info on the instruction);
/// call returns are left `None` (callee-dependent).
///
/// Both backends consume this to disambiguate value-vs-pointer ABIs
/// at call sites — replacing the per-backend inline scans that were
/// doing the same lookup ad hoc (the LLVM backend's `Inst::CallClosure`
/// arg-handling, the C backend's drop-target inference).
pub fn compute_module_pointee_types(module: &mut LirModule) {
    for i in 0..module.functions.len() {
        compute_function_pointee_types_at(module, i);
    }
}

pub fn compute_function_pointee_types_at(module: &mut LirModule, fi: usize) {
    use crate::lir::{Inst, BlockId, ValueId, Term};
    let n = module.functions[fi].value_count() as usize;
    let mut pt: Vec<Option<LirType>> = vec![None; n];
    // `slot_pointee[slot.0]` = the inferred pointee for whatever pointer
    // value last got stored into that slot. Used to propagate through
    // SlotStore→SlotLoad chains where the slot itself is `Ptr`/`PtrTo(_)`.
    let mut slot_pointee: Vec<Option<LirType>> = vec![None; module.functions[fi].slots.len()];

    for block in &module.functions[fi].blocks {
        for inst in &block.insts {
            match inst {
                Inst::SlotAddr { dst, slot } => {
                    let slot_ty = &module.functions[fi].slots[slot.0 as usize].ty;
                    if (dst.0 as usize) < n {
                        pt[dst.0 as usize] = Some(slot_ty.clone());
                    }
                }
                Inst::FieldPtr { dst, struct_id, field, .. } => {
                    let sdef = &module.structs[struct_id.0 as usize];
                    if (*field as usize) < sdef.fields.len() && (dst.0 as usize) < n {
                        pt[dst.0 as usize] = Some(sdef.fields[*field as usize].1.clone());
                    }
                }
                Inst::GlobalAddr { dst, global } => {
                    if let Some(g) = module.globals.get(global.0 as usize) {
                        if (dst.0 as usize) < n {
                            pt[dst.0 as usize] = Some(g.ty.clone());
                        }
                    }
                }
                Inst::PtrCast { dst, value } | Inst::Bitcast { dst, value, .. } => {
                    if let Some(p) = pt.get(value.0 as usize).and_then(|p| p.clone()) {
                        if (dst.0 as usize) < n {
                            pt[dst.0 as usize] = Some(p);
                        }
                    }
                }
                Inst::SlotStore { slot, value, .. } => {
                    if let Some(p) = pt.get(value.0 as usize).and_then(|p| p.clone()) {
                        let slot_ty = &module.functions[fi].slots[slot.0 as usize].ty;
                        if matches!(slot_ty, LirType::Ptr | LirType::PtrTo(_)) {
                            slot_pointee[slot.0 as usize] = Some(p);
                        }
                    }
                }
                Inst::SlotLoad { dst, slot, .. } => {
                    if let Some(p) = slot_pointee.get(slot.0 as usize).and_then(|p| p.clone()) {
                        if (dst.0 as usize) < n {
                            pt[dst.0 as usize] = Some(p);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    // Block-arg → block-param fixed-point. A pointer flowing through a
    // `Jump`/`Branch`/`Switch` should propagate its pointee to the
    // matching block param. Iterate until stable; loops and diamonds can
    // require multiple passes. Cap at 16 to stay bounded on pathological
    // CFGs (in practice 1-3 passes suffice).
    let mut changed = true;
    let mut iters = 0;
    while changed && iters < 16 {
        changed = false;
        iters += 1;
        for block in &module.functions[fi].blocks {
            let pairs: Vec<(BlockId, Vec<ValueId>)> = match &block.terminator {
                Term::Jump(target, args) => vec![(*target, args.clone())],
                Term::Branch { then_block, then_args, else_block, else_args, .. } => {
                    vec![
                        (*then_block, then_args.clone()),
                        (*else_block, else_args.clone()),
                    ]
                }
                Term::Switch { cases, default, default_args, .. } => {
                    let mut v: Vec<(BlockId, Vec<ValueId>)> = cases.iter()
                        .map(|(_, b, a)| (*b, a.clone()))
                        .collect();
                    v.push((*default, default_args.clone()));
                    v
                }
                _ => continue,
            };
            for (target_id, args) in pairs {
                let target_block = match module.functions[fi].blocks.get(target_id.0 as usize) {
                    Some(b) => b,
                    None => continue,
                };
                for (i, arg) in args.iter().enumerate() {
                    let param_vid = match target_block.params.get(i) {
                        Some((vid, _)) => *vid,
                        None => continue,
                    };
                    let param_idx = param_vid.0 as usize;
                    if param_idx >= n || pt[param_idx].is_some() { continue; }
                    if let Some(p) = pt.get(arg.0 as usize).and_then(|p| p.clone()) {
                        pt[param_idx] = Some(p);
                        changed = true;
                    }
                }
            }
        }
    }

    module.functions[fi].pointee_types = pt;
}

/// Populate `func.value_origins` for every function in the module.
///
/// Phase D6 (`docs/devbook/14-lir-ssa.md`): the LIR-side per-value
/// provenance. Each value gets at most one `ValueOrigin` tag, set at the
/// instruction that produced it. Backends read this via typed match instead
/// of reconstructing origin information from instruction shapes.
///
/// The mapping (one site per origin variant):
/// - `Inst::StrLit { dst }`                                 → `StrLit`
/// - `Inst::NullPtr { dst }`                                → `NullPtr`
/// - `Inst::FuncAddr { dst, func }`                         → `FuncAddr(func)`
/// - `Inst::CallExtern { dst, name, .. }` with CStr ret ABI → `CStr { from_extern: true }`
/// - `Inst::CallExtern { dst, name = "__gorget_spawn_X", .. }` with non-Task return
///                                                          → `SpawnSource("X")`
///
/// All other producers leave the origin as `None`.
pub fn compute_module_value_origins(module: &mut LirModule) {
    for i in 0..module.functions.len() {
        compute_function_value_origins_at(module, i);
    }
}

pub fn compute_function_value_origins_at(module: &mut LirModule, fi: usize) {
    use crate::lir::{Inst, ValueOrigin};
    let n = module.functions[fi].value_count() as usize;
    let mut vo: Vec<Option<ValueOrigin>> = vec![None; n];

    // First pass: per-value type info we need for the spawn-source decision
    // (whether the spawn return type is the full Task struct or got reshaped
    // to void*). The C backend's logic checks `val_types[d]` against
    // "starts_with Task__"; mirror that here using the LIR's already-computed
    // value_types when available, falling back to the per-instruction call
    // return types if not yet populated.
    let val_types: &[Option<LirType>] = &module.functions[fi].value_types;

    for block in &module.functions[fi].blocks {
        for inst in &block.insts {
            match inst {
                Inst::StrLit { dst, .. } => {
                    if (dst.0 as usize) < n {
                        vo[dst.0 as usize] = Some(ValueOrigin::StrLit);
                    }
                }
                Inst::NullPtr { dst } => {
                    if (dst.0 as usize) < n {
                        vo[dst.0 as usize] = Some(ValueOrigin::NullPtr);
                    }
                }
                Inst::FuncAddr { dst, func } => {
                    if (dst.0 as usize) < n {
                        vo[dst.0 as usize] = Some(ValueOrigin::FuncAddr(*func));
                    }
                }
                Inst::CallExtern { dst: Some(d), name, .. } => {
                    let ret_abi = module.externs.iter()
                        .find(|e| &e.name == name)
                        .map(|e| e.return_abi)
                        .unwrap_or_default();
                    if ret_abi == crate::ir::abi::AbiKind::CStr {
                        if (d.0 as usize) < n {
                            vo[d.0 as usize] = Some(ValueOrigin::CStr { from_extern: true });
                        }
                        continue;
                    }
                    if name.starts_with("__gorget_spawn_") {
                        // Only tag when the return got reshaped away from
                        // the full `Task__T` struct (the codegen extracts
                        // .__task and downstream loses the spawn-fn link).
                        let is_task_struct = matches!(
                            val_types.get(d.0 as usize).and_then(|t| t.as_ref()),
                            Some(LirType::Struct(sid)) if {
                                module.structs.get(sid.0 as usize)
                                    .map_or(false, |s| s.name.starts_with("Task__"))
                            }
                        );
                        if !is_task_struct {
                            let suffix = name.strip_prefix("__gorget_spawn_")
                                .unwrap_or("").to_string();
                            if (d.0 as usize) < n {
                                vo[d.0 as usize] = Some(ValueOrigin::SpawnSource(suffix));
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }

    module.functions[fi].value_origins = vo;
}

/// Compute value types for a single function at index `i` in `module.functions`.
///
/// Split out from `compute_module_value_types` so BIR synthesis can populate
/// `value_types` for newly-appended synth functions without re-scanning the
/// whole module.
pub fn compute_function_value_types_at(module: &mut LirModule, i: usize) {
    let n = module.functions[i].value_count() as usize;
    let mut vt: Vec<Option<LirType>> = vec![None; n];

    // Block parameters carry explicit types.
    for block in &module.functions[i].blocks {
        for (vid, ty) in &block.params {
            if (vid.0 as usize) < n {
                vt[vid.0 as usize] = Some(ty.clone());
            }
        }
    }

    // Instructions: derive from fields. Reads `pointee_types` for the
    // `Inst::Load { ty: Void }` fallback. See note on `compute_value_types`.
    for block in &module.functions[i].blocks {
        for inst in &block.insts {
            if let Some(dst) = inst.dst() {
                if (dst.0 as usize) < n {
                    let pt = &module.functions[i].pointee_types;
                    let ty = infer_inst_type(inst, &module.functions[i], module, &vt, pt);
                    if ty.is_some() {
                        vt[dst.0 as usize] = ty;
                    }
                }
            }
        }
    }

    apply_callextern_slotstore_override(&module.functions[i], module, &mut vt);

    module.functions[i].value_types = vt;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn struct_registry() {
        let mut reg = StructRegistry::new();
        assert!(reg.is_empty());

        assert!(reg.register("GorgetString", StructId(0)).is_none());
        assert_eq!(reg.lookup("GorgetString"), Some(StructId(0)));
        assert_eq!(reg.len(), 1);

        // Re-registering returns old ID
        assert_eq!(reg.register("GorgetString", StructId(5)), Some(StructId(0)));
    }

    #[test]
    fn builtin_structs() {
        let defs = builtin_struct_defs();
        assert!(defs.len() >= 8);
        assert_eq!(defs[0].name, "GorgetString");
        assert_eq!(defs[0].fields.len(), 4);
        assert_eq!(defs[1].name, "GorgetArray");
        assert_eq!(defs[1].fields.len(), 4);
    }

    #[test]
    fn scalar_sizes() {
        assert_eq!(scalar_size(&LirType::I8), Some(1));
        assert_eq!(scalar_size(&LirType::I16), Some(2));
        assert_eq!(scalar_size(&LirType::I32), Some(4));
        assert_eq!(scalar_size(&LirType::I64), Some(8));
        assert_eq!(scalar_size(&LirType::F32), Some(4));
        assert_eq!(scalar_size(&LirType::F64), Some(8));
        assert_eq!(scalar_size(&LirType::Ptr), Some(8));
        assert_eq!(scalar_size(&LirType::Bool), Some(1));
        assert_eq!(scalar_size(&LirType::Struct(StructId(0))), None);
        assert_eq!(scalar_size(&LirType::Void), None);
    }
}
