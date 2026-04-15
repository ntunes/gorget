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
            computed_c_size: Some(32),
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
            computed_c_size: Some(64),
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
            computed_c_size: Some(16),
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
            computed_c_size: Some(16),
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
            computed_c_size: Some(16),
        },
        // GorgetMap — hash map backing Dict[K,V] and HashMap[K,V].
        // C layout: 20 fields × 8 = 160 bytes. cap at offset +8 matches the
        // generic view-discriminator prefix shared with Str and GorgetArray.
        // LIR models 13 fields; the extra 7 are runtime-internal function pointers.
        StructDef {
            name: "GorgetMap".into(),
            fields: vec![
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
            ],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: Some(152),
        },
        // GorgetSet — typedef alias for GorgetMap, backs Set[T] and HashSet[T].
        // Same C layout as GorgetMap: 160 bytes.
        StructDef {
            name: "GorgetSet".into(),
            fields: vec![
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
            ],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: Some(152),
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
            computed_c_size: None,
                      },
    ]
}

/// Size of a scalar LIR type in bytes. Returns `None` for aggregates and void.
pub fn scalar_size(ty: &LirType) -> Option<u32> {
    match ty {
        LirType::I8 | LirType::U8 | LirType::Bool => Some(1),
        LirType::I16 | LirType::U16 => Some(2),
        LirType::I32 | LirType::U32 | LirType::F32 => Some(4),
        LirType::I64 | LirType::U64 | LirType::F64 | LirType::Ptr | LirType::PtrTo(_) => Some(8),
        LirType::Struct(_) | LirType::Void => None,
    }
}

// ── Value Type Derivation ──────────────────────────────────────────────────

use super::{LirFunction, LirModule, Inst, ValueId};

/// Derive the LIR type of a single instruction's result.
/// Returns `None` for instructions that don't produce a value.
fn infer_inst_type(
    inst: &Inst,
    func: &LirFunction,
    module: &LirModule,
    val_types: &[Option<LirType>],
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
        Inst::NullPtr { .. } | Inst::FuncAddr { .. } | Inst::GlobalAddr { .. } => Some(LirType::Ptr),
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

        Inst::Cmp { .. } | Inst::Not { .. } => Some(LirType::Bool),

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

        Inst::Load { ty, .. } => Some(ty.clone()),
        Inst::FieldPtr { .. } | Inst::ElemPtr { .. } => Some(LirType::Ptr),

        Inst::Call { func: fid, .. } => {
            Some(module.functions[fid.0 as usize].return_type.clone())
        }
        Inst::CallExtern { name, args, .. } => {
            infer_call_extern_type(name, args, module, val_types)
        }
        Inst::CallPtr { dst, .. } => {
            if dst.is_some() { Some(LirType::I64) } else { None }
        }
        Inst::CallClosure { ret_ty, .. } => {
            if *ret_ty != LirType::Void { Some(ret_ty.clone()) } else { None }
        }
        Inst::InlineC { dst, .. } => {
            if dst.is_some() { Some(LirType::I64) } else { None }
        }
        _ => None,
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
        | "gorget_array_concat" | "gorget_str_split" | "gorget_str_chars" => Some("GorgetArray"),
        "gorget_map_new" | "gorget_map_clone" => Some("GorgetMap"),
        "gorget_set_new" | "gorget_set_clone" => Some("GorgetSet"),
        "gorget_string_new" | "gorget_string_adopt" | "gorget_string_from_concat"
        | "gorget_str_cat" | "gorget_string_format"
        | "gorget_string_format_alloc"
        | "gorget_string_clone"
        | "gorget_int_to_str" | "gorget_float_to_str" | "gorget_char_to_str"
        | "gorget_str_replace" | "gorget_str_repeat" | "gorget_str_join"
        | "gorget_str_reverse" | "gorget_str_pad_left" | "gorget_str_pad_right"
        | "gorget_str_lstrip" | "gorget_str_rstrip" | "gorget_str_strip"
        | "gorget_str_to_lower" | "gorget_str_to_upper"
        | "gorget_str_substr" | "gorget_str_index" => Some("GorgetString"),
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

    // Instructions: derive from fields.
    for block in &func.blocks {
        for inst in &block.insts {
            if let Some(dst) = inst.dst() {
                if (dst.0 as usize) < n {
                    let ty = infer_inst_type(inst, func, module, &vt);
                    if ty.is_some() {
                        vt[dst.0 as usize] = ty;
                    }
                }
            }
        }
    }

    func.value_types = vt;
}

/// Compute value types for all functions in a module.
pub fn compute_module_value_types(module: &mut LirModule) {
    // We need to borrow functions mutably one at a time while reading module metadata.
    // Split the module to avoid double-borrow.
    for i in 0..module.functions.len() {
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

        // Instructions: derive from fields.
        for block in &module.functions[i].blocks {
            for inst in &block.insts {
                if let Some(dst) = inst.dst() {
                    if (dst.0 as usize) < n {
                        let ty = infer_inst_type(inst, &module.functions[i], module, &vt);
                        if ty.is_some() {
                            vt[dst.0 as usize] = ty;
                        }
                    }
                }
            }
        }

        module.functions[i].value_types = vt;
    }
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
