//! LIR → LLVM IR backend.
//!
//! Generates LLVM IR textual format (.ll) from LIR. The mapping is nearly 1:1
//! since LIR is already SSA with block parameters (phi-equivalent).

use crate::lir::*;
use std::collections::HashMap;
use std::fmt::Write;

/// LLVM IR backend.
pub struct LlvmBackend;

/// Returns true if an aggregate return type needs sret convention.
/// Small structs (≤16 bytes on aarch64) are returned in registers.
fn needs_sret(ty: &LirType, structs: &[StructDef]) -> bool {
    ty.is_aggregate() && !is_small_aggregate(ty, structs)
}

/// Returns true if a struct type is small enough to be returned in registers
/// (≤16 bytes on aarch64, ≤8 bytes on x86-64). If true, DO NOT use sret convention.
fn is_small_aggregate(ty: &LirType, structs: &[StructDef]) -> bool {
    if let LirType::Struct(sid) = ty {
        let sdef = &structs[sid.0 as usize];
        // Known opaque types with fixed C layout sizes.
        if sdef.name == "TaskGroup" { return true; }       // ptr = 8 bytes
        if sdef.name.starts_with("Task__") { return true; } // {ptr,ptr} = 16 bytes
        if sdef.name == "File" || sdef.name == "GorgetFile" { return true; } // {ptr,i64} = 16 bytes
        // Use computed_c_size if available, otherwise estimate from fields.
        let size = if let Some(cs) = sdef.computed_c_size {
            cs
        } else {
            // Rough estimate: sum field sizes with 8-byte alignment.
            let mut size: usize = 0;
            for (_, fty) in &sdef.fields {
                let fsz = match fty {
                    LirType::I8 | LirType::U8 | LirType::Bool => 1,
                    LirType::I16 | LirType::U16 => 2,
                    LirType::I32 | LirType::U32 => 4,
                    LirType::I64 | LirType::U64 | LirType::F64 | LirType::Ptr | LirType::PtrTo(_) => 8,
                    LirType::F32 => 4,
                    LirType::Struct(_) => 64, // conservatively large
                    _ => 8,
                };
                // Align to field size (simplified)
                let align = if fsz > 8 { 8 } else { fsz };
                size = (size + align - 1) & !(align - 1);
                size += fsz;
            }
            size
        };
        size <= 16
    } else {
        false
    }
}

impl super::Backend for LlvmBackend {
    fn name(&self) -> &str {
        "llvm"
    }

    fn generate(&self, module: &LirModule) -> super::CodegenOutput {
        super::CodegenOutput {
            code: generate_llvm_ir(module),
            extension: "ll",
        }
    }
}

// ── Type Mapping ───────────────────────────────────────────────────────────

/// Map LirType to LLVM IR type string.
fn llvm_type(ty: &LirType) -> &'static str {
    match ty {
        LirType::I8 | LirType::U8 => "i8",
        LirType::I16 | LirType::U16 => "i16",
        LirType::I32 | LirType::U32 => "i32",
        LirType::I64 | LirType::U64 => "i64",
        LirType::F32 => "float",
        LirType::F64 => "double",
        LirType::Bool => "i1",
        LirType::Ptr | LirType::PtrTo(_) => "ptr",
        LirType::Struct(_) | LirType::Void => unreachable!("use llvm_type_full for these"),
    }
}

/// Map LirType to LLVM IR type string, handling Struct and Void.
fn llvm_type_full(ty: &LirType, snames: &HashMap<u32, String>) -> String {
    match ty {
        LirType::Struct(sid) => {
            let name = &snames[&sid.0];
            // Box types are opaque pointers (void* in C)
            if name.starts_with("Box__") {
                "ptr".to_string()
            } else {
                format!("%{name}")
            }
        }
        LirType::Void => "void".to_string(),
        other => llvm_type(other).to_string(),
    }
}

/// Map LirType to LLVM IR type for use as a function argument or parameter.
/// Void is invalid as an argument type in LLVM IR — use ptr instead (closure env).
fn llvm_arg_type(ty: &LirType, snames: &HashMap<u32, String>) -> String {
    match ty {
        LirType::Void => "ptr".to_string(),
        _ => llvm_type_full(ty, snames),
    }
}

/// C reserved keywords — function names clashing with these are prefixed with `__gg_`.
/// Must match the C backend's `c_func_name()` in helpers.rs.
const C_RESERVED: &[&str] = &[
    "auto", "break", "case", "char", "const", "continue", "default", "do",
    "double", "else", "enum", "extern", "float", "for", "goto", "if",
    "int", "long", "register", "return", "short", "signed", "sizeof",
    "static", "struct", "switch", "typedef", "union", "unsigned", "void",
    "volatile", "while", "inline", "restrict", "_Bool", "_Complex",
    "_Imaginary", "bool", "true", "false",
];

/// Escape a function name that clashes with C keywords.
/// The LLVM backend must match the C backend's escaping since the binary
/// links against C-compiled runtime code that references these mangled names.
fn c_func_name(name: &str) -> String {
    if C_RESERVED.contains(&name) {
        format!("__gg_{name}")
    } else {
        name.to_string()
    }
}

/// Parse a monomorphized name like `Vector__int64_t__map` into (elem_c_name, method).
/// Returns None if not a vector higher-order operation.
fn parse_vector_hof(name: &str) -> Option<(&str, &str)> {
    let rest = name.strip_prefix("Vector__")?;
    let sep = rest.rfind("__")?;
    let method = &rest[sep + 2..];
    match method {
        "filter" | "map" | "each" | "any" | "all" | "fold" | "reduce"
        | "find" | "find_index" | "flat_map" | "count" => {}
        _ => return None,
    }
    let elem = &rest[..sep];
    Some((elem, method))
}

/// Map a C element type name (from Vector__<elem>__method) to an LLVM type string and size.
fn elem_c_to_llvm(elem: &str, module: &LirModule, snames: &HashMap<u32, String>) -> (String, usize) {
    match elem {
        "int64_t" => ("i64".to_string(), 8),
        "int32_t" => ("i32".to_string(), 4),
        "int16_t" => ("i16".to_string(), 2),
        "int8_t" => ("i8".to_string(), 1),
        "uint64_t" => ("i64".to_string(), 8),
        "uint32_t" => ("i32".to_string(), 4),
        "uint16_t" => ("i16".to_string(), 2),
        "uint8_t" => ("i8".to_string(), 1),
        "double" => ("double".to_string(), 8),
        "float" => ("float".to_string(), 4),
        "bool" => ("i1".to_string(), 1),
        "GorgetString" | "Str" => ("%GorgetString".to_string(), 32),
        _ => {
            // Look up as a struct name (e.g., Option__int64_t, user struct, etc.)
            for (i, def) in module.structs.iter().enumerate() {
                if def.name == elem {
                    let sid = StructId(i as u32);
                    let llvm_ty = llvm_type_full(&LirType::Struct(sid), snames);
                    let size = sizeof_lir_type(&LirType::Struct(sid), &module.structs, snames);
                    return (llvm_ty, size);
                }
            }
            // Fallback: assume pointer-sized (8 bytes)
            ("i64".to_string(), 8)
        }
    }
}

/// Resolve the __Closure_N__call function for a closure argument at a specific call site.
/// Returns (call_fn_name, return_type, params_are_ptr) or None.
fn resolve_closure_call_fn(
    closure_arg: ValueId,
    val_types: &[Option<LirType>],
    module: &LirModule,
) -> Option<(String, LirType, Vec<bool>)> {
    // Strategy 1: look at val_types[closure_arg] for Struct(sid) or PtrTo(sid)
    let ty = val_types.get(closure_arg.0 as usize).and_then(|t| t.as_ref());
    let sid = match ty {
        Some(LirType::Struct(sid)) => Some(*sid),
        Some(LirType::PtrTo(sid)) => Some(*sid),
        _ => None,
    };
    if let Some(sid) = sid {
        let sdef = &module.structs[sid.0 as usize];
        let call_name = format!("{}__call", sdef.name);
        if let Some(func) = module.functions.iter().find(|f| f.name == call_name) {
            let params_are_ptr = func.params.iter().skip(1)
                .map(|t| matches!(t, LirType::PtrTo(_) | LirType::Ptr))
                .collect();
            return Some((call_name, func.return_type.clone(), params_are_ptr));
        }
    }
    None
}

/// Trace a Ptr-typed value back to the FuncAddr instruction that produced it,
/// following through SlotStore → SlotLoad chains. Returns the referenced FuncId if found.
fn trace_funcaddr(func: &crate::lir::LirFunction, val_id: u32) -> Option<crate::lir::FuncId> {
    use crate::lir::Inst;
    // Direct FuncAddr
    for block in &func.blocks {
        for inst in &block.insts {
            if let Inst::FuncAddr { dst, func: fid } = inst {
                if dst.0 == val_id { return Some(*fid); }
            }
        }
    }
    // Through slot: find SlotLoad that produces val_id → get slot → find SlotStore
    let mut slot_id = None;
    'find_load: for block in &func.blocks {
        for inst in &block.insts {
            if let Inst::SlotLoad { dst, slot, .. } = inst {
                if dst.0 == val_id { slot_id = Some(slot.0); break 'find_load; }
            }
        }
    }
    if let Some(sid) = slot_id {
        // Find the LAST SlotStore into that slot (dominating the load)
        let mut stored_val = None;
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::SlotStore { slot, value, .. } = inst {
                    if slot.0 == sid { stored_val = Some(value.0); }
                }
            }
        }
        if let Some(sv) = stored_val {
            // Recurse (limited depth — avoid infinite loops)
            return trace_funcaddr(func, sv);
        }
    }
    None
}

/// Check if a LirType is PtrTo(GorgetString).
/// Infer the payload type of an Option/Result struct from its struct definition.
/// The first field is always the tag (i32), the second is the payload.
fn infer_option_payload_type(
    arg: &ValueId,
    val_types: &[Option<LirType>],
    module: &LirModule,
    snames: &HashMap<u32, String>,
) -> String {
    // Try to get the struct id from the value's PtrTo type
    let val_ty = val_types.get(arg.0 as usize).and_then(|t| t.as_ref());
    if let Some(LirType::PtrTo(sid)) = val_ty {
        if let Some(def) = module.structs.get(sid.0 as usize) {
            // Field 1 is the payload (field 0 is tag)
            if def.fields.len() >= 2 {
                let payload = &def.fields[1].1;
                return llvm_arg_type(payload, snames);
            }
        }
    }
    // Fallback: try to find the struct from any SlotAddr/SlotLoad that produced this value
    // Default to i64
    "i64".to_string()
}

/// Compute the byte offset of the payload field in `{ i32 tag, PayloadType payload }`.
/// LLVM inserts padding between tag (4 bytes) and payload to satisfy payload's alignment:
///   - ≤4-byte-aligned types (bool, i8, i16, i32, float): offset = 4
///   - ≥8-byte-aligned types (i64, double, ptr, structs): offset = 8
fn option_payload_offset(payload_ty_str: &str) -> u64 {
    match payload_ty_str {
        "i1" | "i8" | "i16" | "i32" | "float" => 4,
        _ => 8, // i64, double, ptr, %StructName, etc. — all 8-byte aligned
    }
}

/// Compute the payload offset in an Option struct from a LirType.
fn lir_payload_offset(fty: &LirType) -> u64 {
    match fty {
        LirType::Bool | LirType::I8 | LirType::U8
        | LirType::I16 | LirType::U16
        | LirType::I32 | LirType::U32
        | LirType::F32 => 4,
        _ => 8,
    }
}

fn is_ptr_to_gorget_string(ty: &LirType, snames: &HashMap<u32, String>) -> bool {
    if let LirType::PtrTo(sid) = ty {
        return snames.get(&sid.0).map_or(false, |n| n == "GorgetString");
    }
    false
}

/// Whether this LirType is signed (for selecting sdiv vs udiv, etc.).
fn is_signed(ty: &LirType) -> bool {
    matches!(ty, LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64)
}

/// Bit width of an integer type.
fn int_bits(ty: &LirType) -> u32 {
    match ty {
        LirType::I8 | LirType::U8 | LirType::Bool => 8,
        LirType::I16 | LirType::U16 => 16,
        LirType::I32 | LirType::U32 => 32,
        LirType::I64 | LirType::U64 => 64,
        _ => 64,
    }
}

// ── String Escaping ────────────────────────────────────────────────────────

/// Escape a string for LLVM IR constant data (byte-level, not C-style).
fn llvm_escape_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() * 2);
    for byte in s.as_bytes() {
        match byte {
            b'\\' => out.push_str("\\5C"),
            b'"' => out.push_str("\\22"),
            b'\n' => out.push_str("\\0A"),
            b'\r' => out.push_str("\\0D"),
            b'\t' => out.push_str("\\09"),
            0 => out.push_str("\\00"),
            0x20..=0x7E => out.push(*byte as char),
            _ => write!(out, "\\{byte:02X}").unwrap(),
        }
    }
    out
}



// ── Struct Names ───────────────────────────────────────────────────────────

fn build_struct_names(module: &LirModule) -> HashMap<u32, String> {
    let mut map = HashMap::new();
    for (i, def) in module.structs.iter().enumerate() {
        // Sanitize name: replace non-alphanumeric (except _) with _
        let sanitized: String = def.name.chars()
            .map(|c| if c.is_alphanumeric() || c == '_' { c } else { '_' })
            .collect();
        map.insert(i as u32, sanitized);
    }
    map
}

// ── Value Type Inference ───────────────────────────────────────────────────

/// Infer the LirType produced by an instruction.
fn infer_inst_type(inst: &Inst, module: &LirModule, _val_types: &[Option<LirType>], func: Option<&LirFunction>) -> Option<LirType> {
    match inst {
        Inst::SlotLoad { ty, .. } | Inst::ParamRef { ty, .. } => {
            // Void types produce ptr in our codegen (closure env)
            if *ty == LirType::Void { Some(LirType::Ptr) } else { Some(ty.clone()) }
        }
        Inst::SlotAddr { slot, .. } => {
            // Carry struct type info as PtrTo for Option/Result unwrap inference
            if let Some(f) = func {
                if let Some(s) = f.slots.get(slot.0 as usize) {
                    if let LirType::Struct(sid) = &s.ty {
                        return Some(LirType::PtrTo(*sid));
                    }
                }
            }
            Some(LirType::Ptr)
        }
        Inst::IConst { ty, .. } | Inst::FConst { ty, .. } => Some(ty.clone()),
        Inst::BoolConst { .. } => Some(LirType::Bool),
        Inst::NullPtr { .. } | Inst::FuncAddr { .. } | Inst::GlobalAddr { .. } => Some(LirType::Ptr),
        Inst::StrLit { .. } => {
            // StrLit returns ptr to GorgetString alloca — find GorgetString struct id
            let gs_id = module.structs.iter().position(|s| s.name == "GorgetString")
                .map(|i| StructId(i as u32));
            Some(gs_id.map_or(LirType::Ptr, LirType::PtrTo))
        }

        Inst::Add { ty, .. } | Inst::Sub { ty, .. } | Inst::Mul { ty, .. }
        | Inst::Div { ty, .. } | Inst::Rem { ty, .. } | Inst::Mod { ty, .. }
        | Inst::Neg { ty, .. } => Some(ty.clone()),

        Inst::BitAnd { ty, .. } | Inst::BitOr { ty, .. } | Inst::BitXor { ty, .. }
        | Inst::Shl { ty, .. } | Inst::Shr { ty, .. }
        | Inst::BitNot { ty, .. } => Some(ty.clone()),

        Inst::Cmp { .. } | Inst::Not { .. } => Some(LirType::Bool),

        Inst::IntCast { to, .. } | Inst::FloatCast { to, .. }
        | Inst::IntToFloat { to, .. } | Inst::FloatToInt { to, .. }
        | Inst::Bitcast { to, .. } => Some(to.clone()),
        Inst::PtrCast { value, .. } => {
            // Preserve PtrTo type info through pointer casts
            let src_ty = _val_types.get(value.0 as usize).and_then(|t| t.as_ref());
            if let Some(LirType::PtrTo(sid)) = src_ty {
                Some(LirType::PtrTo(*sid))
            } else {
                Some(LirType::Ptr)
            }
        }

        Inst::Load { ty, .. } => Some(ty.clone()),
        Inst::FieldPtr { .. } | Inst::ElemPtr { .. } => Some(LirType::Ptr),

        Inst::Call { func, .. } => {
            Some(module.functions[func.0 as usize].return_type.clone())
        }
        Inst::CallExtern { name, args, .. } => {
            // Tag checks always return bool
            let is_tag = name == "__option_is_some" || name == "__option_is_none"
                || name.ends_with("__is_some") || name.ends_with("__is_none")
                || name.ends_with("__is_ok") || name.ends_with("__is_err");
            if is_tag { return Some(LirType::Bool); }

            // unwrap_error: error payload type from the Result struct (last field)
            let is_unwrap_err = name == "__result_unwrap_error"
                || name.ends_with("__unwrap_error") || name.ends_with("__unwrap_err");
            if is_unwrap_err && !args.is_empty() {
                let arg_ty = _val_types.get(args[0].0 as usize).and_then(|t| t.as_ref());
                if let Some(LirType::PtrTo(sid)) = arg_ty {
                    if let Some(def) = module.structs.get(sid.0 as usize) {
                        if let Some((_, err_ty)) = def.fields.last() {
                            return Some(if *err_ty == LirType::Void { LirType::Ptr } else { err_ty.clone() });
                        }
                    }
                }
            }

            // Inline-handled default() methods and string-returning stubs: return PtrTo(GorgetString)
            if name == "gorget_str_default" || name == "gorget_str_str" {
                let gs_sid = module.structs.iter().position(|s| s.name == "GorgetString").map(|i| StructId(i as u32));
                return Some(gs_sid.map_or(LirType::Ptr, LirType::PtrTo));
            }

            // Parse methods: return PtrTo(Option__*) (alloca in our inline handler)
            let is_parse = name.ends_with("__parse") && (name.starts_with("int") || name.starts_with("uint")
                || name == "double__parse" || name == "float__parse" || name == "bool__parse");
            if is_parse {
                // Find the Option struct from the extern's declared return type
                let ext = module.externs.iter().find(|e| e.name == *name);
                if let Some(ext) = ext {
                    if let LirType::Struct(sid) = &ext.return_type {
                        return Some(LirType::PtrTo(*sid));
                    }
                }
                return Some(LirType::Ptr);
            }

            // Unwrap: payload type from the Option/Result struct
            let is_unwrap = name == "__option_unwrap" || name == "__result_unwrap"
                || name.ends_with("__unwrap") || name.ends_with("__expect");
            let is_unwrap_or = name == "__option_unwrap_or" || name == "__result_unwrap_or"
                || name.ends_with("__unwrap_or");
            if (is_unwrap || is_unwrap_or) && !args.is_empty() {
                let arg_ty = _val_types.get(args[0].0 as usize).and_then(|t| t.as_ref());
                if let Some(LirType::PtrTo(sid)) = arg_ty {
                    if let Some(def) = module.structs.get(sid.0 as usize) {
                        if def.fields.len() >= 2 {
                            let payload = &def.fields[1].1;
                            return Some(if *payload == LirType::Void { LirType::Ptr } else { payload.clone() });
                        }
                    }
                }
                return Some(LirType::I64); // fallback
            }

            // Check module.externs first
            let from_externs = module.externs.iter()
                .find(|e| e.name == *name)
                .map(|e| e.return_type.clone());
            if from_externs.is_some() {
                return from_externs;
            }
            // Infer return type from name patterns for externs not registered in module.externs
            // (e.g., gorget_array_clone/extend added directly in insts.rs)
            let arr_sid = module.structs.iter().position(|s| s.name == "GorgetArray").map(|i| StructId(i as u32));
            let str_sid = module.structs.iter().position(|s| s.name == "GorgetString").map(|i| StructId(i as u32));
            let map_sid = module.structs.iter().position(|s| s.name == "GorgetMap").map(|i| StructId(i as u32));
            let set_sid = module.structs.iter().position(|s| s.name == "GorgetSet").map(|i| StructId(i as u32));
            if name.contains("gorget_array_clone") || name.contains("gorget_array_slice") || name.contains("gorget_array_concat") {
                return arr_sid.map(LirType::Struct);
            }
            if name.contains("gorget_str_slice") || name.contains("gorget_str_substr")
                || name.contains("gorget_str_trim") || name.contains("gorget_str_to_lower")
                || name.contains("gorget_str_to_upper") || name.contains("gorget_string_clone")
                || name.contains("gorget_int_to_str") || name.contains("gorget_float_to_str")
                || name.contains("gorget_char_to_str") || name.contains("gorget_str_replace")
                || name.contains("gorget_str_repeat") || name.contains("gorget_str_join")
                || name.contains("gorget_str_reverse") || name.contains("gorget_str_pad")
                || name.contains("gorget_str_lstrip") || name.contains("gorget_str_rstrip")
                || name.contains("gorget_str_strip") || name.contains("gorget_str_cat")
                || name == "gorget_str_index" {
                return str_sid.map(LirType::Struct);
            }
            if name.contains("gorget_map_clone") {
                return map_sid.map(LirType::Struct);
            }
            if name.contains("gorget_set_clone") {
                return set_sid.map(LirType::Struct);
            }
            None
        }
        Inst::CallPtr { dst, .. } => {
            // CallPtr return type is hard to infer without more context.
            // Default to i64 if there's a destination.
            if dst.is_some() { Some(LirType::I64) } else { None }
        }

        _ => None,
    }
}

// ── Enum Layout ────────────────────────────────────────────────────────────

/// For enum structs, compute the union payload size in bytes.
/// Field 0 is the tag (i32). Fields 1+ are variant payloads grouped by prefix.
/// E.g., Rectangle_0, Rectangle_1 belong to the Rectangle variant — their
/// sizes must be summed. The max across all variants gives the union size.
fn enum_payload_size(def: &StructDef, structs: &[StructDef], snames: &HashMap<u32, String>) -> usize {
    // Group fields by variant prefix (everything before the last _N suffix).
    let mut variant_sizes: HashMap<&str, usize> = HashMap::new();
    for (fname, fty) in def.fields.iter().skip(1) {
        let prefix = fname.rsplitn(2, '_').nth(1).unwrap_or(fname);
        let fsz = sizeof_lir_type(fty, structs, snames);
        let entry = variant_sizes.entry(prefix).or_insert(0);
        // Align field within variant
        let align = fsz.min(8).max(1);
        *entry = (*entry + align - 1) & !(align - 1);
        *entry += fsz;
    }
    variant_sizes.values().copied().max().unwrap_or(0)
}

/// Compute the size of a LirType in bytes (for memcpy/memset sizes, enum payloads).
fn sizeof_lir_type(ty: &LirType, structs: &[StructDef], snames: &HashMap<u32, String>) -> usize {
    match ty {
        LirType::I8 | LirType::U8 | LirType::Bool => 1,
        LirType::I16 | LirType::U16 => 2,
        LirType::I32 | LirType::U32 => 4,
        LirType::I64 | LirType::U64 | LirType::F64 | LirType::Ptr | LirType::PtrTo(_) => 8,
        LirType::F32 => 4,
        LirType::Struct(sid) => {
            if let Some(def) = structs.get(sid.0 as usize) {
                // Box types are opaque pointers — always 8 bytes
                if def.name.starts_with("Box__") {
                    return 8;
                }
                // Known opaque types with fixed C sizes
                if def.name == "TaskGroup" { return 8; }
                if def.name.starts_with("Task__") { return 16; }
                if def.name == "File" || def.name == "GorgetFile" { return 16; }
                if let Some(sz) = def.computed_c_size {
                    return sz;
                }
                // Rough estimate: sum of field sizes with alignment
                let mut total = 0usize;
                for (_, fty) in &def.fields {
                    let fsz = sizeof_lir_type(fty, structs, snames);
                    let align = fsz.min(8);
                    total = (total + align - 1) & !(align - 1);
                    total += fsz;
                }
                // Align total to 8
                (total + 7) & !7
            } else {
                8
            }
        }
        LirType::Void => 0,
    }
}

// ── Main Entry ─────────────────────────────────────────────────────────────

/// Generate LLVM IR from an LIR module.
pub fn generate_llvm_ir(module: &LirModule) -> String {
    let snames = build_struct_names(module);
    let mut out = String::with_capacity(64 * 1024);

    // Module header
    if let Some(ref name) = module.source_filename {
        writeln!(out, "source_filename = \"{name}\"").unwrap();
    }
    // Target datalayout and triple are set at compile time based on host.
    #[cfg(target_arch = "x86_64")]
    {
        writeln!(out, "target datalayout = \"e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128\"").unwrap();
        writeln!(out, "target triple = \"x86_64-unknown-linux-gnu\"").unwrap();
    }
    #[cfg(target_arch = "aarch64")]
    {
        writeln!(out, "target datalayout = \"e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128\"").unwrap();
        writeln!(out, "target triple = \"aarch64-unknown-linux-gnu\"").unwrap();
    }
    #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
    {
        // Fallback: omit target info and let LLVM infer
    }
    writeln!(out).unwrap();

    // Struct type definitions
    emit_struct_types(&mut out, module, &snames);

    // String literal globals (collected from all functions)
    let mut str_globals = StrGlobals::new();
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::StrLit { value, .. } = inst {
                    str_globals.intern(value);
                }
                if let Inst::Printf { fmt, .. } | Inst::Fprintf { fmt, .. } = inst {
                    str_globals.intern(fmt);
                }
                if let Inst::Trap { msg, .. } = inst {
                    str_globals.intern(msg);
                }
                if let Inst::BoundsCheck { .. } = inst {
                    str_globals.intern("index out of bounds: index %lld, len %lld\n");
                }
                if let Inst::DivCheck { .. } = inst {
                    str_globals.intern("division by zero\n");
                }
                if let Inst::Div { ty, .. } | Inst::Rem { ty, .. } = inst {
                    if ty.is_integer() {
                        str_globals.intern("gorget: division by zero\n");
                    }
                }
                if let Inst::Add { overflow: Overflow::Trap, ty, .. }
                    | Inst::Sub { overflow: Overflow::Trap, ty, .. }
                    | Inst::Mul { overflow: Overflow::Trap, ty, .. } = inst {
                    if ty.is_integer() {
                        str_globals.intern("gorget: integer overflow\n");
                    }
                }
            }
        }
    }
    // Track how many strings are pre-interned so we can detect late additions.
    let pre_intern_count = str_globals.strings.len();
    emit_string_globals(&mut out, &str_globals);

    // Global variables
    emit_globals(&mut out, module, &snames);

    // Extern function declarations
    emit_extern_declarations(&mut out, module, &snames);

    // Intrinsic declarations
    emit_intrinsic_declarations(&mut out, module, &snames);

    // No forward declarations needed — LLVM handles out-of-order function references.

    // Function definitions (may intern new strings via format fix)
    for func in &module.functions {
        emit_function(&mut out, func, module, &snames, &mut str_globals);
    }

    // Emit any strings that were intern'd during function emission (e.g., fixed printf formats).
    // In LLVM IR text format, constant definitions may appear after their first use.
    if str_globals.strings.len() > pre_intern_count {
        writeln!(out, "; Late-added string globals (format fix)").unwrap();
        for i in pre_intern_count..str_globals.strings.len() {
            let s = &str_globals.strings[i];
            let escaped = llvm_escape_string(s);
            let byte_len = s.len() + 1;
            writeln!(out, "@.str.{i} = private unnamed_addr constant [{byte_len} x i8] c\"{escaped}\\00\"").unwrap();
        }
        writeln!(out).unwrap();
    }

    out
}

// ── String Global Interning ────────────────────────────────────────────────

struct StrGlobals {
    /// string content → global index
    map: HashMap<String, usize>,
    /// ordered list of unique strings
    strings: Vec<String>,
}

impl StrGlobals {
    fn new() -> Self {
        Self { map: HashMap::new(), strings: Vec::new() }
    }

    fn intern(&mut self, s: &str) -> usize {
        if let Some(&idx) = self.map.get(s) {
            return idx;
        }
        let idx = self.strings.len();
        self.map.insert(s.to_string(), idx);
        self.strings.push(s.to_string());
        idx
    }

    fn get_index(&self, s: &str) -> usize {
        self.map[s]
    }

}

fn emit_string_globals(out: &mut String, sg: &StrGlobals) {
    for (i, s) in sg.strings.iter().enumerate() {
        let escaped = llvm_escape_string(s);
        let byte_len = s.len() + 1; // +1 for null terminator
        writeln!(out, "@.str.{i} = private unnamed_addr constant [{byte_len} x i8] c\"{escaped}\\00\"").unwrap();
    }
    if !sg.strings.is_empty() {
        writeln!(out).unwrap();
    }
}

// ── Struct Types ───────────────────────────────────────────────────────────

fn emit_struct_types(out: &mut String, module: &LirModule, snames: &HashMap<u32, String>) {
    for (i, def) in module.structs.iter().enumerate() {
        let name = &snames[&(i as u32)];
        if def.is_union_layout {
            // Enum: { i32 tag, [payload_bytes x i8] }
            let payload = enum_payload_size(def, &module.structs, snames);
            if payload > 0 {
                writeln!(out, "%{name} = type {{ i32, [{payload} x i8] }}").unwrap();
            } else {
                writeln!(out, "%{name} = type {{ i32 }}").unwrap();
            }
        } else if def.fields.is_empty() {
            // Known opaque types whose C layout is fixed:
            // - TaskGroup: typedef gorget_task_group_t* → ptr (8 bytes)
            // - Task__T:   { void* __task, void(*__drop)(void*) } = 16 bytes
            // - File:      GorgetFile { FILE*, bool } padded to 16 bytes
            if name == "TaskGroup" {
                writeln!(out, "%{name} = type {{ ptr }}").unwrap();
            } else if name.starts_with("Task__") {
                writeln!(out, "%{name} = type {{ ptr, ptr }}").unwrap();
            } else if name == "File" || name == "GorgetFile" {
                // { FILE* handle, bool owned } — bool padded to i64 for C ABI (16 bytes total)
                writeln!(out, "%{name} = type {{ ptr, i64 }}").unwrap();
            } else {
                // Other empty structs — use single byte padding
                writeln!(out, "%{name} = type {{ i8 }}").unwrap();
            }
        } else {
            let mut fields: Vec<String> = def.fields.iter()
                .map(|(_, fty)| {
                    if *fty == LirType::Void { "i8".to_string() }
                    else { llvm_type_full(fty, snames) }
                })
                .collect();
            // If computed_c_size is larger than the LLVM struct size, add padding bytes
            // to match the C ABI size. This happens for runtime structs like
            // GorgetArray (4 LIR fields = 32B, C has 7 fields = 56B).
            if let Some(c_size) = def.computed_c_size {
                // Calculate aligned size matching LLVM's layout rules
                let mut llvm_size = 0usize;
                for (_, fty) in &def.fields {
                    let fsz = sizeof_lir_type(fty, &module.structs, snames);
                    let align = fsz.min(8).max(1);
                    llvm_size = (llvm_size + align - 1) & !(align - 1);
                    llvm_size += fsz;
                }
                // Align total to 8 bytes (struct alignment)
                llvm_size = (llvm_size + 7) & !7;
                if c_size > llvm_size {
                    let pad = c_size - llvm_size;
                    fields.push(format!("[{pad} x i8]"));
                }
            }
            writeln!(out, "%{name} = type {{ {} }}", fields.join(", ")).unwrap();
        }
    }
    if !module.structs.is_empty() {
        writeln!(out).unwrap();
    }
}

// ── Global Variables ───────────────────────────────────────────────────────

fn emit_globals(out: &mut String, module: &LirModule, snames: &HashMap<u32, String>) {
    for (i, global) in module.globals.iter().enumerate() {
        let ty = llvm_type_full(&global.ty, snames);
        let linkage = if global.is_const { "private constant" } else { "internal global" };
        match &global.init {
            LirGlobalInit::Zeroed => {
                writeln!(out, "@__lir_g{i} = {linkage} {ty} zeroinitializer ; {}", global.name).unwrap();
            }
            LirGlobalInit::Bytes(data) => {
                // Emit as array of i8
                let len = data.len();
                let vals: Vec<String> = data.iter().map(|b| format!("i8 {b}")).collect();
                let vals_str = vals.join(", ");
                writeln!(out, "@__lir_g{i} = {linkage} [{len} x i8] [{vals_str}] ; {}", global.name).unwrap();
            }
            LirGlobalInit::FuncAddr(fid) => {
                let fname = &module.functions[fid.0 as usize].name;
                writeln!(out, "@__lir_g{i} = {linkage} ptr @{fname} ; {}", global.name).unwrap();
            }
            LirGlobalInit::Struct { struct_id, fields } => {
                let sdef = &module.structs[struct_id.0 as usize];
                // Use named struct only if field count matches; otherwise anonymous
                let use_named = fields.len() == sdef.fields.len() && !sdef.fields.is_empty();
                let sty = if use_named {
                    format!("%{}", snames[&struct_id.0])
                } else {
                    let ftypes: Vec<&str> = fields.iter().map(|f| match f {
                        LirGlobalInit::FuncAddr(_) => "ptr",
                        _ => "i64",
                    }).collect();
                    format!("{{ {} }}", ftypes.join(", "))
                };
                let field_vals: Vec<String> = fields.iter().enumerate().map(|(fi, init)| {
                    let fty = if use_named && fi < sdef.fields.len() {
                        llvm_type_full(&sdef.fields[fi].1, snames)
                    } else {
                        match init {
                            LirGlobalInit::FuncAddr(_) => "ptr".to_string(),
                            _ => "i64".to_string(),
                        }
                    };
                    match init {
                        LirGlobalInit::Zeroed => format!("{fty} zeroinitializer"),
                        LirGlobalInit::FuncAddr(fid) => {
                            let fname = c_func_name(&module.functions[fid.0 as usize].name);
                            format!("ptr @{fname}")
                        }
                        LirGlobalInit::Bytes(data) if data.len() <= 8 => {
                            let mut val = 0i64;
                            for (bi, &b) in data.iter().enumerate() {
                                val |= (b as i64) << (bi * 8);
                            }
                            format!("{fty} {val}")
                        }
                        _ => format!("{fty} zeroinitializer"),
                    }
                }).collect();
                writeln!(out, "@__lir_g{i} = {linkage} {sty} {{ {} }} ; {}",
                    field_vals.join(", "), global.name).unwrap();
            }
            LirGlobalInit::RuntimeCall(_expr) => {
                // Runtime-initialized globals need a constructor. For now, zero-init
                // and we'll add @llvm.global_ctors later.
                writeln!(out, "@__lir_g{i} = {linkage} {ty} zeroinitializer ; {} (runtime init)", global.name).unwrap();
            }
        }
    }
    if !module.globals.is_empty() {
        writeln!(out).unwrap();
    }
}

// ── Extern Declarations ───────────────────────────────────────────────────

/// Names that are declared as libc builtins — skip if they also appear in module externs.
const LIBC_BUILTINS: &[&str] = &[
    "printf", "fprintf", "abort", "memset", "memcpy", "exit", "malloc", "free", "realloc", "calloc",
    "gorget_panic", "gorget_init_args",
    // Printf-like functions — we call them with (ptr, ...) signature, skip extern declaration
    "gorget_string_format", "gorget_string_format_alloc", "fprintf_stderr",
    // gorget_bool_to_str — declared with sret in libc section
    "gorget_bool_to_str",
    // Other builtins we declare explicitly
    "free",
    // String comparison — declared with correct ABI (gorget_str_cmp returns int, not int64_t)
    "gorget_str_eq", "gorget_str_cmp",
    // Always declared in preamble (inline expansion support)
    "gorget_string_clone_to_owned",
];

fn emit_extern_declarations(out: &mut String, module: &LirModule, snames: &HashMap<u32, String>) {
    // Also declare libc functions we use directly
    writeln!(out, "; -- libc --").unwrap();
    writeln!(out, "declare i32 @printf(ptr, ...)").unwrap();
    writeln!(out, "declare i32 @fprintf(ptr, ptr, ...)").unwrap();
    writeln!(out, "declare void @abort() noreturn").unwrap();
    writeln!(out, "declare ptr @memset(ptr, i32, i64)").unwrap();
    writeln!(out, "declare ptr @memcpy(ptr, ptr, i64)").unwrap();
    writeln!(out, "declare void @gorget_init_args(i32, ptr)").unwrap();
    writeln!(out, "declare void @gorget_panic(ptr)").unwrap();
    // String comparison — declared with precise C ABI return types.
    // gorget_str_eq returns bool (i1), gorget_str_cmp returns int (i32, NOT i64!).
    // On aarch64, 'mov w0, -1' zero-extends to x0=0xFFFFFFFF; must call as i32 + sext.
    // Listed in LIBC_BUILTINS so module.externs never re-declares them with wrong types.
    writeln!(out, "declare i1 @gorget_str_eq(ptr, ptr)").unwrap();
    writeln!(out, "declare i32 @gorget_str_cmp(ptr, ptr)").unwrap();
    // Collection constructors and HOF helpers — always declare so inline HOF expansion works
    // even when the function is not in module.externs (e.g. flat_map inlining needs extend).
    let hof_decls: &[(&str, &str)] = &[
        ("gorget_array_with_capacity", "declare void @gorget_array_with_capacity(ptr sret(%GorgetArray), i64, i64)"),
        ("gorget_array_new",   "declare void @gorget_array_new(ptr sret(%GorgetArray), i64)"),
        ("gorget_array_push",  "declare void @gorget_array_push(ptr, ptr)"),
        ("gorget_array_extend","declare void @gorget_array_extend(ptr, ptr)"),
        ("gorget_set_new",     "declare void @gorget_set_new(ptr sret(%GorgetSet), i64)"),
        ("gorget_set_new_str", "declare void @gorget_set_new_str(ptr sret(%GorgetSet))"),
    ];
    for (fn_name, decl) in hof_decls {
        if !module.externs.iter().any(|e| e.name == *fn_name) {
            writeln!(out, "{decl}").unwrap();
        }
    }
    writeln!(out, "declare void @gorget_string_format(ptr sret(%GorgetString), ptr, ...)").unwrap();
    writeln!(out, "declare void @gorget_string_format_alloc(ptr sret(%GorgetString), ptr, ...)").unwrap();
    // gorget_bool_to_str returns GorgetString by value → sret
    writeln!(out, "declare void @gorget_bool_to_str(ptr sret(%GorgetString), i1)").unwrap();
    writeln!(out, "declare ptr @malloc(i64)").unwrap();
    writeln!(out, "declare void @free(ptr)").unwrap();
    // String push variants for gorget_str_push type dispatch
    writeln!(out, "declare void @gorget_string_push_int(ptr, i64)").unwrap();
    writeln!(out, "declare void @gorget_string_push_float(ptr, double)").unwrap();
    writeln!(out, "declare void @gorget_string_push_bool(ptr, i1)").unwrap();
    writeln!(out, "declare void @gorget_string_push_char(ptr, ptr)").unwrap();
    writeln!(out, "declare void @gorget_string_push_line_int(ptr, i64)").unwrap();
    writeln!(out, "declare void @gorget_string_push_line_float(ptr, double)").unwrap();
    writeln!(out, "declare void @gorget_string_push_line_bool(ptr, i1)").unwrap();
    writeln!(out, "declare void @gorget_string_push_line(ptr, ptr)").unwrap();
    writeln!(out, "declare void @exit(i32) noreturn").unwrap();
    // gorget_task_group_submit_raw: the real function behind the gorget_task_group_submit macro.
    // gorget_task_group_submit is a C macro — replaced inline with calls to _raw.
    writeln!(out, "declare void @gorget_task_group_submit_raw(ptr, ptr, ptr)").unwrap();
    // Parse helpers
    // gorget_try_parse_int/float take (const char* s, i64 len) and return
    // a 16-byte struct {value, ok}. C pads bool to 8 bytes for alignment,
    // so use {i64, i64} / {double, i64} to match the C ABI on aarch64.
    writeln!(out, "declare {{i64, i64}} @gorget_try_parse_int(ptr, i64)").unwrap();
    writeln!(out, "declare {{double, i64}} @gorget_try_parse_float(ptr, i64)").unwrap();
    writeln!(out, "declare ptr @gorget_str_to_cstr(ptr)").unwrap();
    // CStr → GorgetString wrapping (for extern "C" return values)
    if !module.externs.iter().any(|e| e.name == "gorget_str_from_cstr") {
        writeln!(out, "declare void @gorget_str_from_cstr(ptr sret(%GorgetString), ptr)").unwrap();
    }
    // gorget_string_clone_to_owned: used by gorget_str_str inline expansion
    writeln!(out, "declare void @gorget_string_clone_to_owned(ptr sret(%GorgetString), ptr)").unwrap();
    // Runtime constructors — used for static global initialization
    // (LirGlobalInit::RuntimeCall). Only declare if not already in module.externs
    // or already declared above in hof_decls.
    let has_runtime_init = module.globals.iter().any(|g| matches!(&g.init, crate::lir::LirGlobalInit::RuntimeCall(_)));
    if has_runtime_init {
        let existing_externs: std::collections::HashSet<&str> = module.externs.iter().map(|e| e.name.as_str()).collect();
        let hof_names: std::collections::HashSet<&str> = hof_decls.iter().map(|(n, _)| *n).collect();
        let runtime_init_fns: &[(&str, &str)] = &[
            ("gorget_array_new",      "declare void @gorget_array_new(ptr sret(%GorgetArray), i64)"),
            ("gorget_array_extend",   "declare void @gorget_array_extend(ptr, ptr)"),
            ("gorget_map_new",        "declare void @gorget_map_new(ptr sret(%GorgetMap), i64, i64)"),
            ("gorget_map_new_str",    "declare void @gorget_map_new_str(ptr sret(%GorgetMap), i64)"),
            ("gorget_dict_new",       "declare void @gorget_dict_new(ptr sret(%GorgetMap), i64, i64)"),
            ("gorget_dict_new_str",   "declare void @gorget_dict_new_str(ptr sret(%GorgetMap), i64)"),
            ("gorget_set_new",        "declare void @gorget_set_new(ptr sret(%GorgetSet), i64)"),
            ("gorget_set_new_str",    "declare void @gorget_set_new_str(ptr sret(%GorgetSet))"),
        ];
        for (fn_name, decl) in runtime_init_fns {
            if !existing_externs.contains(*fn_name) && !hof_names.contains(*fn_name) {
                writeln!(out, "{decl}").unwrap();
            }
        }
        // Also declare any other functions referenced in RuntimeCall expressions
        // that aren't already covered (e.g. gorget_mutex_new, gorget_atomic_int_new).
        let known_init_fns: std::collections::HashSet<&str> = runtime_init_fns.iter().map(|(n, _)| *n).collect();
        for global in &module.globals {
            if let crate::lir::LirGlobalInit::RuntimeCall(expr) = &global.init {
                if let Some(paren) = expr.find('(') {
                    let fn_name = &expr[..paren];
                    if !existing_externs.contains(fn_name) && !hof_names.contains(fn_name)
                        && !known_init_fns.contains(fn_name) {
                        // Parse arg count from expression to determine declaration
                        let args_str = &expr[paren+1..];
                        let args_str = args_str.strip_suffix(')').unwrap_or(args_str);
                        let n_args = if args_str.trim().is_empty() { 0 }
                            else { args_str.split(',').count() };
                        let params: Vec<&str> = (0..n_args).map(|_| "i64").collect();
                        writeln!(out, "declare i64 @{fn_name}({})", params.join(", ")).unwrap();
                    }
                }
            }
        }
    }
    writeln!(out, "@stderr = external global ptr").unwrap();
    writeln!(out).unwrap();

    // Collect names of functions defined in this module (skip forward declarations)
    let defined_fns: std::collections::HashSet<&str> = module.functions.iter()
        .map(|f| f.name.as_str())
        .collect();

    // Module externs
    writeln!(out, "; -- runtime externs --").unwrap();
    // Seed seen set with preamble-declared function names to avoid redefinition.
    // Only include hof_decls that were actually emitted (i.e., not in module.externs).
    let mut seen: std::collections::HashSet<String> = hof_decls.iter()
        .filter(|(n, _)| !module.externs.iter().any(|e| e.name == *n))
        .map(|(n, _)| n.to_string())
        .collect();
    // Add other preamble declarations
    for name in &[
        "gorget_str_eq", "gorget_str_cmp", "gorget_string_format", "gorget_string_format_alloc",
        "gorget_bool_to_str", "malloc", "free", "gorget_string_push_int", "gorget_string_push_float",
        "gorget_string_push_bool", "gorget_string_push_char", "gorget_string_push_line_int",
        "gorget_string_push_line_float", "gorget_string_push_line_bool", "gorget_string_push_line",
        "exit", "gorget_task_group_submit_raw", "gorget_try_parse_int", "gorget_try_parse_float",
        "gorget_str_to_cstr", "gorget_string_clone_to_owned",
    ] {
        seen.insert(name.to_string());
    }
    if !module.externs.iter().any(|e| e.name == "gorget_str_from_cstr") {
        seen.insert("gorget_str_from_cstr".to_string());
    }
    // Also add runtime_init_fns if they were declared
    if has_runtime_init {
        for (fn_name, _) in &[
            ("gorget_map_new", ""), ("gorget_map_new_str", ""),
            ("gorget_dict_new", ""), ("gorget_dict_new_str", ""),
        ] {
            seen.insert(fn_name.to_string());
        }
    }
    for ext in &module.externs {
        if !seen.insert(ext.name.clone()) {
            continue;
        }
        // Skip libc builtins (already declared above)
        if LIBC_BUILTINS.contains(&ext.name.as_str()) {
            continue;
        }
        // Skip functions defined in this module
        if defined_fns.contains(ext.name.as_str()) {
            continue;
        }
        // Skip inline-expanded names
        if ext.name.starts_with("__callable_") || ext.name.starts_with("__gorget_closure_call_")
            || ext.name.starts_with("__gorget_drop_if_alive_") {
            continue;
        }
        // Skip monomorphized parse methods (handled inline)
        if ext.name.ends_with("__parse") && (ext.name.starts_with("int") || ext.name.starts_with("uint")
            || ext.name == "double__parse" || ext.name == "float__parse" || ext.name == "bool__parse") {
            continue;
        }
        // Skip default() methods and wrong-arity stubs handled inline in LLVM backend
        if ext.name == "gorget_str_default" || ext.name == "gorget_str_str" || ext.name == "gorget_str_clear" {
            continue;
        }
        // gorget_task_group_submit is a C macro — expanded inline via gorget_task_group_submit_raw.
        if ext.name == "gorget_task_group_submit" {
            continue;
        }
        // gorget_regex_find_at is a C macro alias for gorget_regex_find
        if ext.name == "gorget_regex_find_at" {
            // Emit declaration with the real function name
            let params: Vec<String> = ext.params.iter().map(|p| llvm_arg_type(p, snames)).collect();
            let ret = llvm_type_full(&ext.return_type, snames);
            if needs_sret(&ext.return_type, &module.structs) {
                writeln!(out, "declare void @gorget_regex_find(ptr sret({ret}), {})", params.join(", ")).unwrap();
            } else {
                writeln!(out, "declare {ret} @gorget_regex_find({})", params.join(", ")).unwrap();
            }
            continue;
        }
        // gorget_file_open with 1 arg means "open for reading".
        // The real gorget_file_open takes 2 args — redirect to __gorget_file_open_r wrapper.
        // Skip the default declaration; emit the wrapper declaration instead.
        if ext.name == "gorget_file_open" && ext.params.len() == 1 {
            let ret_ty = llvm_type_full(&ext.return_type, snames);
            let param_ty = llvm_type_full(&ext.params[0], snames);
            writeln!(out, "declare {ret_ty} @__gorget_file_open_r({param_ty})").unwrap();
            continue; // Do NOT emit the 1-arg gorget_file_open declaration
        }
        // gorget_file_read_all: C function returns GorgetString but LIR expects Result<Str,Str>.
        // Redirect to __gorget_file_read_all_r which wraps the return in a proper Result struct.
        if ext.name == "gorget_file_read_all" {
            let ret_ty = llvm_type_full(&ext.return_type, snames);
            let param_ty = ext.params.first().map(|p| llvm_type_full(p, snames)).unwrap_or_else(|| "ptr".to_string());
            writeln!(out, "declare void @__gorget_file_read_all_r(ptr sret({ret_ty}), {param_ty})").unwrap();
            continue; // Do NOT emit the direct gorget_file_read_all declaration
        }
        let params: Vec<String> = ext.params.iter()
            .map(|p| {
                // Void params are invalid in LLVM — replace with ptr (typically closure env)
                if *p == LirType::Void { return "ptr".to_string(); }
                // Aggregate params: small structs (≤16 bytes) pass in registers (aarch64 ABI),
                // large structs (>16 bytes) pass by indirect reference (ptr).
                if p.is_aggregate() {
                    if is_small_aggregate(p, &module.structs) {
                        return llvm_type_full(p, snames);
                    }
                    return "ptr".to_string();
                }
                llvm_type_full(p, snames)
            })
            .collect();
        let variadic = if ext.is_variadic {
            if params.is_empty() { "...".to_string() } else { ", ...".to_string() }
        } else {
            String::new()
        };
        // CStr return ABI: function returns const char*, not a struct.
        // Declare as returning ptr, NOT sret.
        if ext.return_abi == crate::ir::abi::AbiKind::CStr {
            writeln!(out, "declare ptr @{}({}{})", ext.name, params.join(", "), variadic).unwrap();
            continue;
        }
        if needs_sret(&ext.return_type, &module.structs) {
            let ret_ty = llvm_type_full(&ext.return_type, snames);
            let sret_params = if params.is_empty() {
                format!("ptr sret({ret_ty}){variadic}")
            } else {
                format!("ptr sret({ret_ty}), {}{variadic}", params.join(", "))
            };
            writeln!(out, "declare void @{}({sret_params})", ext.name).unwrap();
        } else {
            let ret = llvm_type_full(&ext.return_type, snames);
            writeln!(out, "declare {ret} @{}({}{})", ext.name, params.join(", "), variadic).unwrap();
        }
    }

    // Auto-declare any CallExtern targets not yet declared
    // (some runtime functions are called without explicit extern declarations)
    // Build a lookup of defined functions by name for return type inference
    let fn_by_name: HashMap<&str, &LirFunction> = module.functions.iter()
        .map(|f| (f.name.as_str(), f))
        .collect();
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::CallExtern { name, args, dst, .. } = inst {
                    // Redirect no-arg strip variants to their whitespace-only counterparts.
                    // LIR emits gorget_str_strip(s) for strip() with no args, but the C runtime
                    // only has gorget_str_strip(s, chars). The 0-arg version is gorget_str_trim.
                    // This redirect must happen BEFORE the seen.contains check so we use the
                    // effective name for deduplication (1-arg and 2-arg versions are separate fns).
                    let name: String = if args.len() == 1 {
                        match name.as_str() {
                            "gorget_str_strip" => "gorget_str_trim".to_string(),
                            "gorget_str_lstrip" => "gorget_str_lstrip_ws".to_string(),
                            "gorget_str_rstrip" => "gorget_str_rstrip_ws".to_string(),
                            _ => name.clone(),
                        }
                    } else { name.clone() };
                    if seen.contains(name.as_str()) || LIBC_BUILTINS.contains(&name.as_str())
                        || defined_fns.contains(name.as_str()) {
                        continue;
                    }
                    // Skip inline-expanded names — no extern declaration needed.
                    if name.starts_with("__callable_") || name.starts_with("__gorget_closure_call_")
                        || name.starts_with("__gorget_drop_if_alive_")
                        || name == "__gorget_drop_flag_open" || name == "__gorget_drop_flag_close" {
                        continue;
                    }
                    // Skip monomorphized parse methods
                    if name.ends_with("__parse") && (name.starts_with("int") || name.starts_with("uint")
                        || name == "double__parse" || name == "float__parse" || name == "bool__parse") {
                        continue;
                    }
                    // Skip inline-handled default() methods and wrong-arity stubs
                    if name == "gorget_str_default" || name == "gorget_str_str" || name == "gorget_str_clear" {
                        continue;
                    }
                    seen.insert(name.clone());

                    // Try to find the function defined in this module for signature
                    if let Some(target_fn) = fn_by_name.get(name.as_str()) {
                        let params: Vec<String> = target_fn.params.iter()
                            .map(|p| llvm_arg_type(p, snames))
                            .collect();
                        if needs_sret(&target_fn.return_type, &module.structs) {
                            let ret_ty = llvm_type_full(&target_fn.return_type, snames);
                            let sret_params = if params.is_empty() {
                                format!("ptr sret({ret_ty})")
                            } else {
                                format!("ptr sret({ret_ty}), {}", params.join(", "))
                            };
                            writeln!(out, "declare void @{name}({sret_params})").unwrap();
                        } else {
                            let ret = llvm_type_full(&target_fn.return_type, snames);
                            writeln!(out, "declare {ret} @{name}({})", params.join(", ")).unwrap();
                        }
                    } else {
                        // Truly unknown — infer from args, defaulting to ptr params.
                        // For return type, check if the function name matches a runtime
                        // pattern that returns an aggregate (GorgetArray, GorgetString, etc.)
                        let params: Vec<String> = args.iter().map(|_| "ptr".to_string()).collect();
                        let returns_array = name.contains("gorget_array_clone")
                            || name.contains("gorget_array_slice")
                            || name.contains("gorget_array_concat");
                        let returns_string = name.contains("gorget_str_slice")
                            || name.contains("gorget_str_substr")
                            || name.contains("gorget_str_trim")
                            || name.contains("gorget_str_to_lower")
                            || name.contains("gorget_str_to_upper")
                            || name.contains("gorget_str_replace")
                            || name.contains("gorget_str_repeat")
                            || name.contains("gorget_str_join")
                            || name.contains("gorget_str_reverse")
                            || name.contains("gorget_str_pad")
                            || name.contains("gorget_str_lstrip")
                            || name.contains("gorget_str_rstrip")
                            || name.contains("gorget_str_strip")
                            || name.contains("gorget_string_clone")
                            || name.contains("gorget_int_to_str")
                            || name.contains("gorget_float_to_str")
                            || name.contains("gorget_char_to_str");
                        let returns_map = name.contains("gorget_map_clone");
                        let returns_set = name.contains("gorget_set_clone");
                        if dst.is_none() {
                            writeln!(out, "declare void @{name}({})", params.join(", ")).unwrap();
                        } else if returns_array {
                            let sret_params = if params.is_empty() {
                                "ptr sret(%GorgetArray)".to_string()
                            } else {
                                format!("ptr sret(%GorgetArray), {}", params.join(", "))
                            };
                            writeln!(out, "declare void @{name}({sret_params})").unwrap();
                        } else if returns_string {
                            let sret_params = if params.is_empty() {
                                "ptr sret(%GorgetString)".to_string()
                            } else {
                                format!("ptr sret(%GorgetString), {}", params.join(", "))
                            };
                            writeln!(out, "declare void @{name}({sret_params})").unwrap();
                        } else if returns_map {
                            let sret_params = if params.is_empty() {
                                "ptr sret(%GorgetMap)".to_string()
                            } else {
                                format!("ptr sret(%GorgetMap), {}", params.join(", "))
                            };
                            writeln!(out, "declare void @{name}({sret_params})").unwrap();
                        } else if returns_set {
                            let sret_params = if params.is_empty() {
                                "ptr sret(%GorgetSet)".to_string()
                            } else {
                                format!("ptr sret(%GorgetSet), {}", params.join(", "))
                            };
                            writeln!(out, "declare void @{name}({sret_params})").unwrap();
                        } else {
                            // Default: return ptr if has dst (most runtime fns return pointers)
                            let ret = if dst.is_some() { "ptr" } else { "void" };
                            writeln!(out, "declare {ret} @{name}({})", params.join(", ")).unwrap();
                        }
                    }
                }
            }
        }
    }
    writeln!(out).unwrap();
}

// ── Global Runtime Init ───────────────────────────────────────────────────

/// Emit LLVM IR to initialize a RuntimeCall global at the start of main().
/// The C expression (e.g. "gorget_array_new(sizeof(int64_t))") is parsed to
/// extract the function name and i64 sizeof arguments.
fn emit_global_runtime_init(out: &mut String, gid: usize, expr: &str, snames: &HashMap<u32, String>, module: &LirModule) {
    // Parse: "func_name(arg1, arg2, ...)" where args are sizeof(TYPE) or numbers.
    let (fn_name, args_str) = if let Some(paren) = expr.find('(') {
        let name = &expr[..paren];
        // Strip outer parentheses: "func(a, b)" → "a, b"
        let rest = &expr[paren+1..];
        let args = rest.strip_suffix(')').unwrap_or(rest);
        (name, args)
    } else {
        return; // Can't parse, skip
    };

    // Parse sizeof(TYPE) arguments → i64 constants.
    let args: Vec<u64> = args_str.split(',')
        .map(|a| {
            let a = a.trim();
            if let Some(inner) = a.strip_prefix("sizeof(").and_then(|s| s.strip_suffix(')')) {
                c_sizeof_name(inner) as u64
            } else if let Ok(n) = a.parse::<u64>() {
                n
            } else {
                8 // fallback
            }
        })
        .collect();

    // Determine return type from function name
    let (ret_struct_name, ret_llvm) = if fn_name.contains("map") || fn_name.contains("dict") || fn_name.contains("set") {
        ("GorgetMap", "%GorgetMap")
    } else if fn_name.contains("array") {
        ("GorgetArray", "%GorgetArray")
    } else if fn_name.contains("string") {
        ("GorgetString", "%GorgetString")
    } else {
        // Unknown — just call it with raw i64 return
        let arg_strs: Vec<String> = args.iter().map(|a| format!("i64 {a}")).collect();
        writeln!(out, "  %__ginit_{gid}_raw = call i64 @{fn_name}({})", arg_strs.join(", ")).unwrap();
        writeln!(out, "  store i64 %__ginit_{gid}_raw, ptr @__lir_g{gid}").unwrap();
        return;
    };

    // Emit sret call → memcpy to global
    let sz = sizeof_struct_by_name(ret_struct_name, module, snames);
    let arg_strs: Vec<String> = args.iter().map(|a| format!("i64 {a}")).collect();
    writeln!(out, "  %__ginit_{gid} = alloca {ret_llvm}").unwrap();
    let sret_arg = format!("ptr sret({ret_llvm}) %__ginit_{gid}");
    let all_args = if arg_strs.is_empty() {
        sret_arg
    } else {
        format!("{sret_arg}, {}", arg_strs.join(", "))
    };
    writeln!(out, "  call void @{fn_name}({all_args})").unwrap();
    writeln!(out, "  call ptr @memcpy(ptr @__lir_g{gid}, ptr %__ginit_{gid}, i64 {sz})").unwrap();
}

/// Return the C sizeof for a type name string.
fn c_sizeof_name(name: &str) -> usize {
    match name {
        "int64_t" | "uint64_t" | "double" | "int64" => 8,
        "int32_t" | "uint32_t" | "float" => 4,
        "int16_t" | "uint16_t" => 2,
        "int8_t" | "uint8_t" | "bool" | "char" => 1,
        "Str" | "GorgetString" => 32,
        _ => 8, // default
    }
}

/// Return sizeof for a struct by name (from module structs or known constants).
fn sizeof_struct_by_name(name: &str, module: &LirModule, snames: &HashMap<u32, String>) -> usize {
    if let Some(pos) = module.structs.iter().position(|s| s.name == name) {
        let sid = crate::lir::StructId(pos as u32);
        return sizeof_lir_type(&LirType::Struct(sid), &module.structs, snames);
    }
    // Fallback known sizes
    match name {
        "GorgetString" => 32,
        "GorgetArray" => 64,
        "GorgetMap" | "GorgetSet" => 160,
        _ => 64,
    }
}

// ── Intrinsic Declarations ────────────────────────────────────────────────

fn emit_intrinsic_declarations(out: &mut String, module: &LirModule, snames: &HashMap<u32, String>) {
    // Check if we need overflow intrinsics
    let mut need_sadd_i64 = false;
    let mut need_ssub_i64 = false;
    let mut need_smul_i64 = false;
    let mut need_sadd_i32 = false;
    let mut need_ssub_i32 = false;
    let mut need_smul_i32 = false;

    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                match inst {
                    Inst::Add { ty, overflow: Overflow::Trap, .. } => {
                        match ty { LirType::I64 => need_sadd_i64 = true, LirType::I32 => need_sadd_i32 = true, _ => {} }
                    }
                    Inst::Sub { ty, overflow: Overflow::Trap, .. } => {
                        match ty { LirType::I64 => need_ssub_i64 = true, LirType::I32 => need_ssub_i32 = true, _ => {} }
                    }
                    Inst::Mul { ty, overflow: Overflow::Trap, .. } => {
                        match ty { LirType::I64 => need_smul_i64 = true, LirType::I32 => need_smul_i32 = true, _ => {} }
                    }
                    _ => {}
                }
            }
        }
    }

    writeln!(out, "; -- intrinsics --").unwrap();
    writeln!(out, "declare void @llvm.trap() noreturn nounwind").unwrap();
    if need_sadd_i64 { writeln!(out, "declare {{ i64, i1 }} @llvm.sadd.with.overflow.i64(i64, i64)").unwrap(); }
    if need_ssub_i64 { writeln!(out, "declare {{ i64, i1 }} @llvm.ssub.with.overflow.i64(i64, i64)").unwrap(); }
    if need_smul_i64 { writeln!(out, "declare {{ i64, i1 }} @llvm.smul.with.overflow.i64(i64, i64)").unwrap(); }
    if need_sadd_i32 { writeln!(out, "declare {{ i32, i1 }} @llvm.sadd.with.overflow.i32(i32, i32)").unwrap(); }
    if need_ssub_i32 { writeln!(out, "declare {{ i32, i1 }} @llvm.ssub.with.overflow.i32(i32, i32)").unwrap(); }
    if need_smul_i32 { writeln!(out, "declare {{ i32, i1 }} @llvm.smul.with.overflow.i32(i32, i32)").unwrap(); }

    // Adapter function definitions for FuncAddr → callable wrapping.
    // Each adapter ignores the closure env pointer and forwards to the real function.
    // ABI: non-sret: ret_ty @__adapt_fn(ptr env, params...)
    //      sret:     void   @__adapt_fn(ptr sret(T) out, ptr env, params...)
    let mut adapter_fids = std::collections::HashSet::new();
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::FuncAddr { func: fid, .. } = inst {
                    adapter_fids.insert(fid.0);
                }
            }
        }
    }
    for fid_raw in &adapter_fids {
        let target = &module.functions[*fid_raw as usize];
        let safe_name = c_func_name(&target.name);
        let adapt_name = format!("__adapt_{safe_name}");
        let target_uses_sret = needs_sret(&target.return_type, &module.structs);
        let ret_llvm = llvm_type_full(&target.return_type, snames);

        // Build parameter list (excluding sret, which we handle separately)
        let mut param_names: Vec<String> = Vec::new();
        let mut param_decls: Vec<String> = Vec::new();
        if target_uses_sret {
            param_decls.push(format!("ptr sret({ret_llvm}) %a.out"));
        }
        param_decls.push("ptr %a.env".to_string()); // env (ignored)
        for (i, p) in target.params.iter().enumerate() {
            let ty = llvm_arg_type(p, snames);
            param_decls.push(format!("{ty} %a.p{i}"));
            param_names.push(format!("{ty} %a.p{i}"));
        }

        // Forward call to target
        let fwd_args = if target_uses_sret {
            let mut a = vec![format!("ptr sret({ret_llvm}) %a.out")];
            a.extend(param_names.iter().cloned());
            a.join(", ")
        } else {
            param_names.join(", ")
        };

        if target_uses_sret {
            writeln!(out, "define void @{adapt_name}({}) {{", param_decls.join(", ")).unwrap();
            writeln!(out, "  call void @{safe_name}({fwd_args})").unwrap();
            writeln!(out, "  ret void").unwrap();
        } else if target.return_type == LirType::Void {
            writeln!(out, "define void @{adapt_name}({}) {{", param_decls.join(", ")).unwrap();
            writeln!(out, "  call void @{safe_name}({fwd_args})").unwrap();
            writeln!(out, "  ret void").unwrap();
        } else {
            writeln!(out, "define {ret_llvm} @{adapt_name}({}) {{", param_decls.join(", ")).unwrap();
            writeln!(out, "  %a.r = call {ret_llvm} @{safe_name}({fwd_args})").unwrap();
            writeln!(out, "  ret {ret_llvm} %a.r").unwrap();
        }
        writeln!(out, "}}").unwrap();
    }
    writeln!(out).unwrap();
}

// ── Function Emission ──────────────────────────────────────────────────────

fn emit_function(
    out: &mut String,
    func: &LirFunction,
    module: &LirModule,
    snames: &HashMap<u32, String>,
    str_globals: &mut StrGlobals,
) {
    let ret = llvm_type_full(&func.return_type, snames);
    let params: Vec<String> = func.params.iter().enumerate()
        .map(|(i, p)| {
            let ty = if *p == LirType::Void { "ptr".to_string() } else { llvm_type_full(p, snames) };
            format!("{ty} %p{i}")
        })
        .collect();

    let is_main = func.name == "main";
    let has_sret = !is_main && needs_sret(&func.return_type, &module.structs);
    if is_main {
        writeln!(out, "define i32 @main(i32 %argc, ptr %argv) {{").unwrap();
    } else if has_sret {
        // Aggregate return: sret convention (hidden first parameter)
        let sret_params = if params.is_empty() {
            format!("ptr sret({ret}) %sret.out")
        } else {
            format!("ptr sret({ret}) %sret.out, {}", params.join(", "))
        };
        writeln!(out, "define void @{}({sret_params}) {{", c_func_name(&func.name)).unwrap();
    } else {
        writeln!(out, "define {} @{}({}) {{", ret, c_func_name(&func.name), params.join(", ")).unwrap();
    }

    // Build value type map for this function
    let val_count = func.value_count() as usize;
    let mut val_types: Vec<Option<LirType>> = vec![None; val_count];

    // Infer types from params
    for (i, p) in func.params.iter().enumerate() {
        // ParamRef instructions map to specific ValueIds, we'll fill those in below
        let _ = (i, p);
    }

    // First pass: infer types from all instructions.
    // Aggregate returns from Call/CallExtern are stored via temp alloca,
    // making them pointers in LLVM — override their type to Ptr.
    for block in &func.blocks {
        for (vid, vty) in &block.params {
            if (vid.0 as usize) < val_types.len() {
                val_types[vid.0 as usize] = Some(vty.clone());
            }
        }
        for inst in &block.insts {
            if let Some(dst) = inst.dst() {
                if (dst.0 as usize) < val_types.len() {
                    let ty = infer_inst_type(inst, module, &val_types, Some(func));
                    // Aggregates are always represented as pointers in our LLVM codegen:
                    // - Call/CallExtern results stored via temp alloca
                    // - Load on aggregate uses pointer alias (no actual load)
                    // - ParamRef for aggregate uses alloca
                    // Use PtrTo(sid) instead of plain Ptr to preserve struct identity
                    // for downstream Store→memcpy detection.
                    let ty = match &ty {
                        Some(LirType::Struct(sid)) => Some(LirType::PtrTo(*sid)),
                        _ => ty,
                    };
                    val_types[dst.0 as usize] = ty;
                }
            }
        }

        // Sentinel-based Option wrapping type override: when a CallExtern returns scalar
        // but the result is stored into an Option slot, the emitter will construct an
        // Option alloca (ptr) instead. Override the value type so SlotStore uses memcpy.
        for inst in &block.insts {
            if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                let vid = d.0 as usize;
                let is_scalar = val_types.get(vid).and_then(|t| t.as_ref())
                    .map_or(false, |t| t.is_integer() || t.is_float());
                if !is_scalar { continue; }
                let skip = name.ends_with("__upgrade")
                    || name.ends_with("__recv_timeout")
                    || name.contains("try_parse");
                if skip { continue; }
                // Scan block for SlotStore of this value into an Option slot
                for next in &block.insts {
                    if let Inst::SlotStore { slot, value, .. } = next {
                        if value.0 == d.0 {
                            if let LirType::Struct(sid) = &func.slots[slot.0 as usize].ty {
                                if module.structs.get(sid.0 as usize)
                                    .map_or(false, |s| s.name.starts_with("Option__")) {
                                    val_types[vid] = Some(LirType::PtrTo(*sid));
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Guard/shared value accessor type override: gorget_guard_get / gorget_shared_get
    // return void* but the value needs to be loaded as the actual inner type.
    // Infer from downstream consumers (arithmetic, slot store, printf format).
    for block in &func.blocks {
        let insts = &block.insts;
        for (i, inst) in insts.iter().enumerate() {
            if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                let is_guard_value = matches!(name.as_str(),
                    "gorget_guard_get" | "gorget_shared_get"
                    | "gorget_read_guard_get" | "gorget_write_guard_get"
                );
                if !is_guard_value { continue; }
                let vid = d.0 as usize;
                if !matches!(val_types.get(vid), Some(Some(LirType::Ptr)) | Some(None)) { continue; }
                let mut inferred = None;
                for ci in (i+1)..insts.len().min(i+10) {
                    match &insts[ci] {
                        Inst::Add { ty, lhs, .. } | Inst::Sub { ty, lhs, .. }
                        | Inst::Mul { ty, lhs, .. } | Inst::Div { ty, lhs, .. }
                        | Inst::Rem { ty, lhs, .. } if *lhs == *d => {
                            inferred = Some(ty.clone()); break;
                        }
                        Inst::IntCast { value, .. } if *value == *d => {
                            inferred = Some(LirType::I64); break;
                        }
                        Inst::FloatCast { value, .. } if *value == *d => {
                            inferred = Some(LirType::F64); break;
                        }
                        Inst::SlotStore { value, slot, .. } if *value == *d => {
                            let slot_ty = &func.slots[slot.0 as usize].ty;
                            if !slot_ty.is_ptr() {
                                inferred = Some(slot_ty.clone()); break;
                            }
                        }
                        _ => {}
                    }
                }
                val_types[vid] = Some(inferred.unwrap_or(LirType::I64));
            }
        }
    }

    // Entry block: emit allocas for non-promoted slots
    writeln!(out, "entry.prelude:").unwrap();

    // For main(), call gorget_init_args to set up argc/argv
    if is_main {
        writeln!(out, "  call void @gorget_init_args(i32 %argc, ptr %argv)").unwrap();
        // Emit runtime initializers for static globals that require constructor calls.
        // The C LIR backend emits these at the start of main(); LLVM does the same.
        for (gid, global) in module.globals.iter().enumerate() {
            if let crate::lir::LirGlobalInit::RuntimeCall(expr) = &global.init {
                emit_global_runtime_init(out, gid, expr, snames, module);
            }
        }
    }
    for (i, slot) in func.slots.iter().enumerate() {
        if slot.ty == LirType::Void {
            // Void slots hold closure env pointers — allocate space for a ptr.
            writeln!(out, "  %s{i} = alloca ptr ; void slot").unwrap();
        } else {
            let ty = llvm_type_full(&slot.ty, snames);
            let name = slot.name.as_deref().unwrap_or("slot");
            writeln!(out, "  %s{i} = alloca {ty} ; {name}").unwrap();
            // Zero-initialize resource-type slots. gorget_string_free / gorget_array_free
            // check cap/len before deallocating — uninitialized memory has garbage cap
            // values that cause crashes. C backends get away with this because debug
            // builds zero stack memory, but LLVM alloca contents are undefined.
            let needs_zero = match &slot.ty {
                LirType::Struct(sid) => {
                    let sname = snames.get(&sid.0).map(|s| s.as_str()).unwrap_or("");
                    sname == "GorgetString" || sname == "GorgetArray" || sname == "GorgetMap"
                        || sname == "GorgetSet" || sname.starts_with("Result__")
                        || sname.starts_with("Option__")
                }
                _ => false,
            };
            if needs_zero {
                let sz = sizeof_lir_type(&slot.ty, &module.structs, snames);
                writeln!(out, "  call ptr @memset(ptr %s{i}, i32 0, i64 {sz})").unwrap();
            }
        }
    }

    // Allocas for StrLit materializations
    for block in &func.blocks {
        for inst in &block.insts {
            if let Inst::StrLit { dst, .. } = inst {
                // We'll store the Str struct in an alloca and return ptr to it
                writeln!(out, "  %strlit.{} = alloca %GorgetString", dst.0).unwrap();
            }
        }
    }

    writeln!(out, "  br label %bb0").unwrap();
    writeln!(out).unwrap();

    // Emit blocks
    // First, collect predecessor info for phi nodes
    let pred_map = build_predecessor_map(func);

    // Pre-compute exit labels for each block. Only instructions that create
    // LLVM sub-blocks (overflow/bounds/div checks) change the exit label.
    // Other trap_counter increments (printf uid, ext_uid, etc.) are just for
    // unique naming and don't create labels.
    //
    // We use a separate label_counter that tracks only label-creating
    // instructions, and a separate total_counter that mirrors all trap_counter
    // increments (so the label indices match the emission).
    let block_exit_labels: HashMap<BlockId, String> = {
        let mut labels = HashMap::new();
        for block in &func.blocks {
            let bid = block.id.0;
            let mut label = format!("bb{bid}");
            let mut counter = 0u32; // mirrors trap_counter exactly
            let mut df_stack: Vec<u32> = Vec::new(); // drop flag open/close nesting
            for inst in &block.insts {
                match inst {
                    Inst::Add { overflow: Overflow::Trap, ty, .. }
                    | Inst::Sub { overflow: Overflow::Trap, ty, .. }
                    | Inst::Mul { overflow: Overflow::Trap, ty, .. } if ty.is_integer() => {
                        label = format!("ov.{bid}.{counter}.ok");
                        counter += 1;
                    }
                    Inst::Div { ty, .. } if ty.is_integer() => {
                        label = format!("divz.{bid}.{counter}.ok");
                        counter += 1;
                    }
                    Inst::Rem { ty, .. } if ty.is_integer() => {
                        label = format!("remz.{bid}.{counter}.ok");
                        counter += 1;
                    }
                    Inst::BoundsCheck { .. } => {
                        label = format!("bc.{bid}.{counter}.ok");
                        counter += 1;
                    }
                    Inst::DivCheck { .. } => {
                        label = format!("dc.{bid}.{counter}.ok");
                        counter += 1;
                    }
                    // CallExtern paths that increment trap_counter but DON'T create labels.
                    // MUST mirror every trap_counter += 1 in the emit_inst CallExtern handler.
                    Inst::CallExtern { name, args, dst, .. } => {
                        let is_drop_guard = name.starts_with("__gorget_drop_if_alive_open__")
                            || name == "__gorget_drop_if_alive_close";
                        // Drop flag guards (V3) — emit conditional branches.
                        let is_drop_flag_open = name == "__gorget_drop_flag_open";
                        let is_drop_flag_close = name == "__gorget_drop_flag_close";
                        let is_callable = name.starts_with("__callable_");
                        let is_closure_call = name.starts_with("__gorget_closure_call_");
                        let is_map_get = name == "gorget_map_get" || name == "gorget_map_safe_get";
                        let is_void_ret = matches!(name.as_str(),
                            "gorget_guard_get" | "gorget_shared_get"
                            | "gorget_read_guard_get" | "gorget_write_guard_get");
                        let is_bool_to_str = name == "gorget_bool_to_str";
                        let is_printf_like = name == "printf" || name == "gorget_string_format"
                            || name == "gorget_string_format_alloc" || name == "fprintf_stderr";
                        let is_tag = name == "__option_is_some" || name == "__option_is_none"
                            || name.ends_with("__is_some") || name.ends_with("__is_none")
                            || name.ends_with("__is_ok") || name.ends_with("__is_err");
                        let is_unwrap = name == "__option_unwrap" || name == "__result_unwrap"
                            || name.ends_with("__unwrap") || name.ends_with("__expect");
                        let is_unwrap_or = name == "__option_unwrap_or" || name == "__result_unwrap_or"
                            || name.ends_with("__unwrap_or");
                        let is_str_push = name == "gorget_str_push" || name == "gorget_str_push_line";
                        let is_str_clear = name == "gorget_str_clear";
                        let is_str_push_line_direct = name == "gorget_string_push_line";

                        // Detect Vector HOF calls that create inline loops (labels)
                        let is_vector_hof = parse_vector_hof(name).is_some();
                        let vector_hof_needs_inline = if is_vector_hof {
                            let (_, method) = parse_vector_hof(name).unwrap();
                            match method {
                                "each" => true,
                                _ => dst.is_some(),
                            }
                        } else { false };

                        // Count ALL counter increments to stay in sync with emission
                        if is_drop_guard { /* no counter */ }
                        else if is_drop_flag_open {
                            // Creates body + join labels; counter used as uid.
                            df_stack.push(counter);
                            counter += 1;
                        }
                        else if is_drop_flag_close {
                            if let Some(uid) = df_stack.pop() {
                                label = format!("df.{bid}.{uid}.join");
                            }
                        }
                        else if is_callable && !args.is_empty() { counter += 1; }
                        else if is_closure_call && !args.is_empty() { counter += 1; }
                        else if vector_hof_needs_inline && !args.is_empty() {
                            label = format!("hof.{bid}.{counter}.done");
                            counter += 1;
                        }
                        else if is_str_clear {
                            // gorget_str_clear emits a conditional branch → changes exit label
                            label = format!("scl.done.{counter}");
                            counter += 1;
                        }
                        else if is_map_get && dst.is_some() { counter += 1; }
                        else if is_void_ret && dst.is_some() { counter += 1; }
                        else if is_str_push && args.len() == 2 {
                            // gorget_str_push_line with Str arg: emission increments counter
                            // to generate the %psl.data.{uid} temporary. Other cases: no counter.
                            let is_push_line = name == "gorget_str_push_line";
                            if is_push_line {
                                let arg2_ty = val_types.get(args[1].0 as usize).and_then(|t| t.as_ref());
                                let arg2_is_str = arg2_ty.map_or(false, |t| t.is_ptr() || matches!(t, LirType::Struct(_) | LirType::PtrTo(_)));
                                if arg2_is_str { counter += 1; }
                            }
                        }
                        else if is_str_push_line_direct && !args.is_empty() { counter += 1; }
                        else if is_bool_to_str { counter += 1; }
                        else if is_printf_like && !args.is_empty() { counter += 1; }
                        else if is_tag && !args.is_empty() { counter += 1; }
                        else if is_unwrap && !args.is_empty() { counter += 1; }
                        else if is_unwrap_or && args.len() >= 2 { counter += 1; }
                        else { counter += 1; }
                    }
                    _ => {}
                }
            }
            labels.insert(block.id, label);
        }
        labels
    };

    for block in &func.blocks {
        let bid = block.id.0;
        writeln!(out, "bb{bid}:").unwrap();

        // Phi nodes for block parameters
        for (pi, (param_val, param_ty)) in block.params.iter().enumerate() {
            let ty = llvm_type_full(param_ty, snames);
            let mut phi_entries = Vec::new();
            if let Some(preds) = pred_map.get(&block.id) {
                for &pred_id in preds {
                    let pred_block = &func.blocks[pred_id.0 as usize];
                    let args = get_branch_args_for_target(&pred_block.terminator, block.id);
                    // Use the actual exit label (may differ from bb{N} due to overflow checks)
                    let pred_label = block_exit_labels.get(&pred_id)
                        .cloned()
                        .unwrap_or_else(|| format!("bb{}", pred_id.0));
                    if pi < args.len() {
                        phi_entries.push(format!("[ %v{}, %{pred_label} ]", args[pi].0));
                    } else {
                        phi_entries.push(format!("[ undef, %{pred_label} ]"));
                    }
                }
            }
            if phi_entries.is_empty() {
                // Unreachable block param — just set to undef
                writeln!(out, "  %v{} = add {ty} 0, 0 ; dead phi", param_val.0).unwrap();
            } else {
                writeln!(out, "  %v{} = phi {ty} {}", param_val.0, phi_entries.join(", ")).unwrap();
            }
        }

        // Instructions
        // Track the "current label" — overflow/bounds checks emit internal sub-blocks,
        // changing the effective predecessor label for phi nodes in successor blocks.
        let mut trap_counter = 0u32;
        let mut current_label = format!("bb{bid}");
        let mut df_stack: Vec<u32> = Vec::new(); // drop flag open/close nesting
        for inst in &block.insts {
            emit_inst(out, inst, func, module, snames, str_globals, &val_types, bid, &mut trap_counter, &mut current_label, &mut df_stack);

        }

        // Terminator
        emit_term(out, &block.terminator, func, module, snames, &val_types);
    }

    writeln!(out, "}}\n").unwrap();
}

// ── Predecessor Map ────────────────────────────────────────────────────────

fn build_predecessor_map(func: &LirFunction) -> HashMap<BlockId, Vec<BlockId>> {
    let mut map: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
    for block in &func.blocks {
        for succ in block.terminator.successors() {
            map.entry(succ).or_default().push(block.id);
        }
    }
    map
}

/// Get the arguments that a terminator passes to a specific target block.
fn get_branch_args_for_target(term: &Term, target: BlockId) -> Vec<ValueId> {
    match term {
        Term::Jump(tgt, args) if *tgt == target => args.clone(),
        Term::Branch { then_block, then_args, else_block, else_args, .. } => {
            if *then_block == target {
                then_args.clone()
            } else if *else_block == target {
                else_args.clone()
            } else {
                vec![]
            }
        }
        Term::Switch { cases, default, default_args, .. } => {
            for (_, blk, args) in cases {
                if *blk == target {
                    return args.clone();
                }
            }
            if *default == target {
                default_args.clone()
            } else {
                vec![]
            }
        }
        _ => vec![],
    }
}

// ── Instruction Emission ───────────────────────────────────────────────────

fn emit_inst(
    out: &mut String,
    inst: &Inst,
    func: &LirFunction,
    module: &LirModule,
    snames: &HashMap<u32, String>,
    str_globals: &mut StrGlobals,
    val_types: &[Option<LirType>],
    block_id: u32,
    trap_counter: &mut u32,
    current_label: &mut String,
    df_stack: &mut Vec<u32>,
) {
    match inst {
        // ── Slot Access ─────────────────────────────────────────────
        Inst::SlotStore { slot, value, .. } => {
            let slot_ty = &func.slots[slot.0 as usize].ty;
            if *slot_ty == LirType::Void {
                // Void slots are used for closure env pointers — store the pointer.
                let val_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
                if val_ty.map_or(false, |t| t.is_ptr()) {
                    writeln!(out, "  store ptr %v{}, ptr %s{}", value.0, slot.0).unwrap();
                } else {
                    writeln!(out, "  ; void slot store skipped").unwrap();
                }
            } else if slot_ty.is_aggregate() {
                // __Closure_N → GorgetClosure: heap-alloc env + pack fn_ptr/env.
                let slot_is_closure = matches!(slot_ty, LirType::Struct(sid) if {
                    snames.get(&sid.0).map_or(false, |n| n == "GorgetClosure")
                });
                let val_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
                let val_closure_name = if slot_is_closure {
                    let check_sid = |sid: &StructId| -> Option<String> {
                        module.structs.get(sid.0 as usize)
                            .filter(|sd| sd.name.starts_with("__Closure_"))
                            .map(|sd| sd.name.clone())
                    };
                    match val_ty {
                        Some(LirType::Struct(sid)) | Some(LirType::PtrTo(sid)) => check_sid(sid),
                        _ => None,
                    }
                } else { None };
                if let Some(closure_name) = val_closure_name {
                    // Escaped closure: heap-alloc env, pack GorgetClosure struct.
                    let call_fn = format!("{closure_name}__call");
                    let env_sid = match val_ty {
                        Some(LirType::Struct(sid)) | Some(LirType::PtrTo(sid)) => *sid,
                        _ => unreachable!(),
                    };
                    let env_size = sizeof_lir_type(&LirType::Struct(env_sid), &module.structs, snames);
                    let uid = format!("esc.{}.{}", slot.0, value.0);
                    // Heap-allocate env
                    writeln!(out, "  %{uid}.heap = call ptr @malloc(i64 {env_size})").unwrap();
                    // Copy env data to heap
                    writeln!(out, "  call ptr @memcpy(ptr %{uid}.heap, ptr %v{}, i64 {env_size})", value.0).unwrap();
                    // Store fn_ptr to GorgetClosure.fn_ptr (field 0)
                    writeln!(out, "  %{uid}.fpgep = getelementptr %GorgetClosure, ptr %s{}, i32 0, i32 0", slot.0).unwrap();
                    writeln!(out, "  store ptr @{call_fn}, ptr %{uid}.fpgep").unwrap();
                    // Store env_ptr to GorgetClosure.env (field 1)
                    writeln!(out, "  %{uid}.envgep = getelementptr %GorgetClosure, ptr %s{}, i32 0, i32 1", slot.0).unwrap();
                    writeln!(out, "  store ptr %{uid}.heap, ptr %{uid}.envgep").unwrap();
                } else {
                // Aggregate slot — value may be a pointer (SlotAddr/FieldPtr) or
                // aggregate by value (Call return). Check val_types.
                // Assume ptr if type is unknown (None) — aggregate values are always ptrs in our model
                let is_ptr_val = val_ty.map_or(true, |t| t.is_ptr());
                // Check if the source is a NullPtr — use memset(0) instead of memcpy from null.
                let value_is_null = func.blocks.iter().any(|b| {
                    b.insts.iter().any(|i| matches!(i, Inst::NullPtr { dst } if dst.0 == value.0))
                });
                if is_ptr_val {
                    // Value is a pointer — memcpy from it (or memset if null)
                    let sz = sizeof_lir_type(slot_ty, &module.structs, snames);
                    if value_is_null {
                        writeln!(out, "  call ptr @memset(ptr %s{}, i32 0, i64 {sz})", slot.0).unwrap();
                    } else {
                        writeln!(out, "  call ptr @memcpy(ptr %s{}, ptr %v{}, i64 {sz})", slot.0, value.0).unwrap();
                    }
                } else if val_ty.map_or(false, |t| t.is_integer() || t.is_float() || matches!(t, LirType::Bool)) {
                    // Scalar value stored into aggregate slot — type mismatch from extern
                    // that returns aggregate but was declared as scalar. Store the scalar
                    // at the slot's address using the value's actual type.
                    let vty = llvm_type_full(val_ty.unwrap(), snames);
                    writeln!(out, "  store {vty} %v{}, ptr %s{}", value.0, slot.0).unwrap();
                } else {
                    // Value is an aggregate by value — store it directly
                    let sty = llvm_type_full(slot_ty, snames);
                    writeln!(out, "  store {sty} %v{}, ptr %s{}", value.0, slot.0).unwrap();
                }
                } // else (not closure escape)
            } else {
                let sty = llvm_type_full(slot_ty, snames);
                let val_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
                // If slot is integer but value is ptr (e.g. Option alloca stored via auto type
                // inference choosing payload type), use memcpy to treat the slot as raw storage
                // for the Option struct that the ptr points to.
                let slot_is_int = matches!(slot_ty, LirType::I64 | LirType::U64 | LirType::I32 | LirType::U32 | LirType::I16 | LirType::U16 | LirType::I8 | LirType::U8);
                let val_is_ptr = val_ty.map_or(false, |t| matches!(t, LirType::Ptr | LirType::PtrTo(_)));
                if slot_is_int && val_is_ptr {
                    // LIR typed this slot as a scalar but a ptr was stored — common when
                    // `auto` inference picks the payload type instead of the Option struct.
                    // Store the ptr value directly (LLVM accepts store ptr, ptr for opaque ptrs).
                    writeln!(out, "  store ptr %v{}, ptr %s{}", value.0, slot.0).unwrap();
                } else {
                    writeln!(out, "  store {sty} %v{}, ptr %s{}", value.0, slot.0).unwrap();
                }
            }
        }
        Inst::SlotLoad { dst, slot, ty } => {
            if *ty == LirType::Void {
                // Void slots hold closure env pointers — load the stored pointer.
                writeln!(out, "  %v{} = load ptr, ptr %s{} ; void slot load", dst.0, slot.0).unwrap();
            } else {
                let lty = llvm_type_full(ty, snames);
                writeln!(out, "  %v{} = load {lty}, ptr %s{}", dst.0, slot.0).unwrap();
            }
        }
        Inst::SlotAddr { dst, slot } => {
            // SlotAddr just returns the alloca pointer. Use a no-op bitcast (ptr->ptr).
            // Actually LLVM doesn't need this; we can use getelementptr with 0 index.
            writeln!(out, "  %v{} = getelementptr i8, ptr %s{}, i32 0", dst.0, slot.0).unwrap();
        }

        // ── Constants ───────────────────────────────────────────────
        Inst::IConst { dst, ty, value } => {
            let lty = llvm_type(ty);
            writeln!(out, "  %v{} = add {lty} 0, {value}", dst.0).unwrap();
        }
        Inst::FConst { dst, ty, bits } => {
            let lty = llvm_type(ty);
            // LLVM accepts hex float constants
            let hex = format!("0x{bits:016X}");
            writeln!(out, "  %v{} = fadd {lty} 0.0, {hex}", dst.0).unwrap();
        }
        Inst::BoolConst { dst, value } => {
            let v = if *value { 1 } else { 0 };
            writeln!(out, "  %v{} = add i1 0, {v}", dst.0).unwrap();
        }
        Inst::NullPtr { dst } => {
            writeln!(out, "  %v{} = inttoptr i64 0 to ptr", dst.0).unwrap();
        }
        Inst::FuncAddr { dst, func: fid } => {
            // Named function passed as closure — create a [fn_ptr, env_ptr=null] array
            // so callable dispatch (load from [0] and [1]) works correctly.
            // The adapter function (defined by the C wrapper glue) ignores the env pointer.
            let target = &module.functions[fid.0 as usize];
            let adapt_name = format!("__adapt_{}", c_func_name(&target.name));
            let fa = format!("fa.{}", dst.0);
            writeln!(out, "  %{fa} = alloca [2 x ptr]").unwrap();
            writeln!(out, "  %{fa}.0 = getelementptr [2 x ptr], ptr %{fa}, i32 0, i32 0").unwrap();
            writeln!(out, "  store ptr @{adapt_name}, ptr %{fa}.0").unwrap();
            writeln!(out, "  %{fa}.1 = getelementptr [2 x ptr], ptr %{fa}, i32 0, i32 1").unwrap();
            writeln!(out, "  store ptr null, ptr %{fa}.1").unwrap();
            writeln!(out, "  %v{} = bitcast ptr %{fa} to ptr", dst.0).unwrap();
        }
        Inst::GlobalAddr { dst, global } => {
            writeln!(out, "  %v{} = bitcast ptr @__lir_g{} to ptr", dst.0, global.0).unwrap();
        }
        Inst::StrLit { dst, value } => {
            // Build a GorgetString struct in the alloca
            let idx = str_globals.get_index(value);
            let byte_len = value.len();
            // Store data pointer
            let fp = format!("strlit.{}.fp", dst.0);
            let fl = format!("strlit.{}.fl", dst.0);
            let fc = format!("strlit.{}.fc", dst.0);
            let fa = format!("strlit.{}.fa", dst.0);
            writeln!(out, "  %{fp} = getelementptr %GorgetString, ptr %strlit.{}, i32 0, i32 0", dst.0).unwrap();
            writeln!(out, "  store ptr @.str.{idx}, ptr %{fp}").unwrap();
            // Store capacity = 0 (view/literal) — field 1 (cap at offset +8)
            writeln!(out, "  %{fc} = getelementptr %GorgetString, ptr %strlit.{}, i32 0, i32 1", dst.0).unwrap();
            writeln!(out, "  store i64 0, ptr %{fc}").unwrap();
            // Store length — field 2
            writeln!(out, "  %{fl} = getelementptr %GorgetString, ptr %strlit.{}, i32 0, i32 2", dst.0).unwrap();
            writeln!(out, "  store i64 {byte_len}, ptr %{fl}").unwrap();
            // Store alloc = null
            writeln!(out, "  %{fa} = getelementptr %GorgetString, ptr %strlit.{}, i32 0, i32 3", dst.0).unwrap();
            writeln!(out, "  store ptr null, ptr %{fa}").unwrap();
            // The value is the pointer to the alloca
            writeln!(out, "  %v{} = bitcast ptr %strlit.{} to ptr", dst.0, dst.0).unwrap();
        }
        Inst::ParamRef { dst, index, ty } => {
            let lty = llvm_type_full(ty, snames);
            // Parameters are named %pN — just alias them
            // LLVM doesn't allow direct aliasing, so use a dummy add/bitcast
            match ty {
                LirType::Ptr | LirType::PtrTo(_) => {
                    writeln!(out, "  %v{} = bitcast ptr %p{index} to ptr", dst.0).unwrap();
                }
                LirType::F32 => {
                    writeln!(out, "  %v{} = fadd float 0.0, %p{index}", dst.0).unwrap();
                }
                LirType::F64 => {
                    writeln!(out, "  %v{} = fadd double 0.0, %p{index}", dst.0).unwrap();
                }
                LirType::Bool => {
                    writeln!(out, "  %v{} = add i1 0, %p{index}", dst.0).unwrap();
                }
                LirType::Struct(_) => {
                    // Aggregate passed by value — not typical, just forward
                    writeln!(out, "  %v{} = alloca {lty}", dst.0).unwrap();
                    writeln!(out, "  store {lty} %p{index}, ptr %v{}", dst.0).unwrap();
                }
                LirType::Void => {
                    // Void param (closure env) — treat as ptr
                    writeln!(out, "  %v{} = bitcast ptr %p{index} to ptr", dst.0).unwrap();
                }
                _ => {
                    writeln!(out, "  %v{} = add {lty} 0, %p{index}", dst.0).unwrap();
                }
            }
        }

        // ── Arithmetic ──────────────────────────────────────────────
        Inst::Add { dst, ty, lhs, rhs, overflow } => {
            let lty = llvm_type(ty);
            if *overflow == Overflow::Trap && ty.is_integer() {
                emit_overflow_check(out, "add", dst, lhs, rhs, ty, snames, block_id, trap_counter, current_label, str_globals, val_types);
            } else if ty.is_float() {
                writeln!(out, "  %v{} = fadd {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            } else {
                writeln!(out, "  %v{} = add {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            }
        }
        Inst::Sub { dst, ty, lhs, rhs, overflow } => {
            let lty = llvm_type(ty);
            if *overflow == Overflow::Trap && ty.is_integer() {
                emit_overflow_check(out, "sub", dst, lhs, rhs, ty, snames, block_id, trap_counter, current_label, str_globals, val_types);
            } else if ty.is_float() {
                writeln!(out, "  %v{} = fsub {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            } else {
                writeln!(out, "  %v{} = sub {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            }
        }
        Inst::Mul { dst, ty, lhs, rhs, overflow } => {
            let lty = llvm_type(ty);
            if *overflow == Overflow::Trap && ty.is_integer() {
                emit_overflow_check(out, "mul", dst, lhs, rhs, ty, snames, block_id, trap_counter, current_label, str_globals, val_types);
            } else if ty.is_float() {
                writeln!(out, "  %v{} = fmul {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            } else {
                writeln!(out, "  %v{} = mul {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            }
        }
        Inst::Div { dst, ty, lhs, rhs } => {
            let lty = llvm_type(ty);
            if ty.is_float() {
                writeln!(out, "  %v{} = fdiv {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            } else {
                // Integer division: check for zero divisor
                let uid = *trap_counter;
                *trap_counter += 1;
                let cmp = format!("divz.{block_id}.{uid}.cmp");
                let trap_label = format!("divz.{block_id}.{uid}.trap");
                let ok_label = format!("divz.{block_id}.{uid}.ok");
                writeln!(out, "  %{cmp} = icmp eq {lty} %v{}, 0", rhs.0).unwrap();
                writeln!(out, "  br i1 %{cmp}, label %{trap_label}, label %{ok_label}").unwrap();
                writeln!(out, "{trap_label}:").unwrap();
                let panic_msg_idx = str_globals.get_index("gorget: division by zero\n");
                let stderr_name = format!("divz.{block_id}.{uid}.stderr");
                writeln!(out, "  %{stderr_name} = load ptr, ptr @stderr").unwrap();
                writeln!(out, "  call i32 (ptr, ptr, ...) @fprintf(ptr %{stderr_name}, ptr @.str.{panic_msg_idx})").unwrap();
                writeln!(out, "  call void @exit(i32 1)").unwrap();
                writeln!(out, "  unreachable").unwrap();
                writeln!(out, "{ok_label}:").unwrap();
                *current_label = ok_label;
                let op = if is_signed(ty) { "sdiv" } else { "udiv" };
                writeln!(out, "  %v{} = {op} {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            }
        }
        Inst::Rem { dst, ty, lhs, rhs } => {
            let lty = llvm_type(ty);
            if ty.is_float() {
                writeln!(out, "  %v{} = frem {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            } else {
                // Integer remainder: check for zero divisor
                let uid = *trap_counter;
                *trap_counter += 1;
                let cmp = format!("remz.{block_id}.{uid}.cmp");
                let trap_label = format!("remz.{block_id}.{uid}.trap");
                let ok_label = format!("remz.{block_id}.{uid}.ok");
                writeln!(out, "  %{cmp} = icmp eq {lty} %v{}, 0", rhs.0).unwrap();
                writeln!(out, "  br i1 %{cmp}, label %{trap_label}, label %{ok_label}").unwrap();
                writeln!(out, "{trap_label}:").unwrap();
                let rem_panic_idx = str_globals.get_index("gorget: division by zero\n");
                let stderr_name = format!("remz.{block_id}.{uid}.stderr");
                writeln!(out, "  %{stderr_name} = load ptr, ptr @stderr").unwrap();
                writeln!(out, "  call i32 (ptr, ptr, ...) @fprintf(ptr %{stderr_name}, ptr @.str.{rem_panic_idx})").unwrap();
                writeln!(out, "  call void @exit(i32 1)").unwrap();
                writeln!(out, "  unreachable").unwrap();
                writeln!(out, "{ok_label}:").unwrap();
                *current_label = ok_label;
                let op = if is_signed(ty) { "srem" } else { "urem" };
                writeln!(out, "  %v{} = {op} {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            }
        }
        Inst::Mod { dst, ty, lhs, rhs } => {
            // Python-style modulo: ((a % b) + b) % b
            let lty = llvm_type(ty);
            if ty.is_float() {
                let tmp1 = format!("mod.{}.1", dst.0);
                let tmp2 = format!("mod.{}.2", dst.0);
                writeln!(out, "  %{tmp1} = frem {lty} %v{}, %v{}", lhs.0, rhs.0).unwrap();
                writeln!(out, "  %{tmp2} = fadd {lty} %{tmp1}, %v{}", rhs.0).unwrap();
                writeln!(out, "  %v{} = frem {lty} %{tmp2}, %v{}", dst.0, rhs.0).unwrap();
            } else {
                let rem_op = if is_signed(ty) { "srem" } else { "urem" };
                let tmp1 = format!("mod.{}.1", dst.0);
                let tmp2 = format!("mod.{}.2", dst.0);
                writeln!(out, "  %{tmp1} = {rem_op} {lty} %v{}, %v{}", lhs.0, rhs.0).unwrap();
                writeln!(out, "  %{tmp2} = add {lty} %{tmp1}, %v{}", rhs.0).unwrap();
                writeln!(out, "  %v{} = {rem_op} {lty} %{tmp2}, %v{}", dst.0, rhs.0).unwrap();
            }
        }
        Inst::Neg { dst, ty, operand } => {
            let lty = llvm_type(ty);
            if ty.is_float() {
                writeln!(out, "  %v{} = fneg {lty} %v{}", dst.0, operand.0).unwrap();
            } else {
                writeln!(out, "  %v{} = sub {lty} 0, %v{}", dst.0, operand.0).unwrap();
            }
        }

        // ── Bitwise ─────────────────────────────────────────────────
        Inst::BitAnd { dst, ty, lhs, rhs } => {
            let lty = llvm_type(ty);
            writeln!(out, "  %v{} = and {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
        }
        Inst::BitOr { dst, ty, lhs, rhs } => {
            let lty = llvm_type(ty);
            writeln!(out, "  %v{} = or {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
        }
        Inst::BitXor { dst, ty, lhs, rhs } => {
            let lty = llvm_type(ty);
            writeln!(out, "  %v{} = xor {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
        }
        Inst::BitNot { dst, ty, operand } => {
            let lty = llvm_type(ty);
            writeln!(out, "  %v{} = xor {lty} %v{}, -1", dst.0, operand.0).unwrap();
        }
        Inst::Shl { dst, ty, lhs, rhs } => {
            let lty = llvm_type(ty);
            writeln!(out, "  %v{} = shl {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
        }
        Inst::Shr { dst, ty, lhs, rhs } => {
            let lty = llvm_type(ty);
            if is_signed(ty) {
                writeln!(out, "  %v{} = ashr {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            } else {
                writeln!(out, "  %v{} = lshr {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            }
        }

        // ── Comparison & Logic ──────────────────────────────────────
        Inst::Cmp { dst, op, lhs, rhs } => {
            // Detect Str-typed operands for string comparison via gorget_str_eq.
            let is_str_val = |vid: &ValueId| -> bool {
                match val_types.get(vid.0 as usize).and_then(|t| t.as_ref()) {
                    Some(LirType::PtrTo(sid)) | Some(LirType::Struct(sid)) => {
                        snames.get(&sid.0).map_or(false, |n| n == "GorgetString" || n == "Str")
                    }
                    _ => false,
                }
            };
            let lhs_str = is_str_val(lhs);
            let rhs_str = is_str_val(rhs);
            // Use gorget_str_eq for string content comparison (not pointer equality).
            // Require BOTH operands to be strings — if only one is a string, this is a
            // null-pointer check (e.g. gorget_array_safe_pop result vs null constant).
            // Calling gorget_str_eq with a null arg would dereference address 0 → crash.
            if (lhs_str && rhs_str) && !matches!(op, CmpOp::Lt | CmpOp::Le | CmpOp::Gt | CmpOp::Ge) {
                // Call gorget_str_eq(Str a, Str b) → bool
                // Both args are ptrs to GorgetString — pass as-is (Str is >16 bytes → indirect)
                let result = format!("strcmpres.{}.{}", dst.0, lhs.0);
                writeln!(out, "  %{result} = call i1 @gorget_str_eq(ptr %v{}, ptr %v{})", lhs.0, rhs.0).unwrap();
                if matches!(op, CmpOp::Eq) {
                    writeln!(out, "  %v{} = add i1 0, %{result}", dst.0).unwrap();
                } else {
                    // Ne
                    writeln!(out, "  %v{} = xor i1 %{result}, 1", dst.0).unwrap();
                }
            } else if lhs_str && rhs_str {
                // Ordering comparison: gorget_str_cmp returns i32 (-1, 0, 1)
                // Only when BOTH operands are strings — see note above about null checks.
                let cmp_result = format!("strcmpord.{}.{}", dst.0, lhs.0);
                let cmp_ext = format!("strcmpext.{}.{}", dst.0, lhs.0);
                writeln!(out, "  %{cmp_result} = call i32 @gorget_str_cmp(ptr %v{}, ptr %v{})", lhs.0, rhs.0).unwrap();
                writeln!(out, "  %{cmp_ext} = sext i32 %{cmp_result} to i64").unwrap();
                let icmp_op = match op {
                    CmpOp::Lt => "slt", CmpOp::Le => "sle",
                    CmpOp::Gt => "sgt", CmpOp::Ge => "sge",
                    _ => unreachable!(),
                };
                writeln!(out, "  %v{} = icmp {icmp_op} i64 %{cmp_ext}, 0", dst.0).unwrap();
            } else {
            // Determine operand type from val_types
            let lhs_ty = val_types.get(lhs.0 as usize).and_then(|t| t.as_ref());
            let rhs_ty = val_types.get(rhs.0 as usize).and_then(|t| t.as_ref());
            let is_float_cmp = lhs_ty.map_or(false, |t| t.is_float());
            let is_ptr_cmp = lhs_ty.map_or(false, |t| t.is_ptr());
            let is_signed_cmp = lhs_ty.map_or(true, |t| is_signed(t));
            let lty = lhs_ty.map_or("i64", |t| {
                if t.is_ptr() { "ptr" } else { llvm_type(t) }
            });

            // If RHS has different integer width than LHS, emit trunc/zext to match
            let rhs_name = if !is_float_cmp && !is_ptr_cmp {
                let lhs_bits = lhs_ty.map_or(64, int_bits);
                let rhs_bits = rhs_ty.map_or(64, int_bits);
                if rhs_bits != lhs_bits && rhs_bits > 0 && lhs_bits > 0 {
                    let rhs_ty_str = rhs_ty.map_or("i64", |t| llvm_type(t));
                    let cast_name = format!("cmp.cast.{}.{}", dst.0, rhs.0);
                    if rhs_bits > lhs_bits {
                        writeln!(out, "  %{cast_name} = trunc {rhs_ty_str} %v{} to {lty}", rhs.0).unwrap();
                    } else {
                        let ext = if is_signed_cmp { "sext" } else { "zext" };
                        writeln!(out, "  %{cast_name} = {ext} {rhs_ty_str} %v{} to {lty}", rhs.0).unwrap();
                    }
                    format!("%{cast_name}")
                } else {
                    format!("%v{}", rhs.0)
                }
            } else {
                format!("%v{}", rhs.0)
            };

            if is_float_cmp {
                let fcmp_op = match op {
                    CmpOp::Eq => "oeq",
                    CmpOp::Ne => "une",
                    CmpOp::Lt => "olt",
                    CmpOp::Le => "ole",
                    CmpOp::Gt => "ogt",
                    CmpOp::Ge => "oge",
                };
                writeln!(out, "  %v{} = fcmp {fcmp_op} {lty} %v{}, {rhs_name}", dst.0, lhs.0).unwrap();
            } else if is_ptr_cmp {
                let icmp_op = match op {
                    CmpOp::Eq => "eq",
                    CmpOp::Ne => "ne",
                    _ => "eq",
                };
                writeln!(out, "  %v{} = icmp {icmp_op} ptr %v{}, {rhs_name}", dst.0, lhs.0).unwrap();
            } else {
                let icmp_op = match op {
                    CmpOp::Eq => "eq",
                    CmpOp::Ne => "ne",
                    CmpOp::Lt => if is_signed_cmp { "slt" } else { "ult" },
                    CmpOp::Le => if is_signed_cmp { "sle" } else { "ule" },
                    CmpOp::Gt => if is_signed_cmp { "sgt" } else { "ugt" },
                    CmpOp::Ge => if is_signed_cmp { "sge" } else { "uge" },
                };
                writeln!(out, "  %v{} = icmp {icmp_op} {lty} %v{}, {rhs_name}", dst.0, lhs.0).unwrap();
            }
            } // close string comparison else
        }
        Inst::Not { dst, operand } => {
            writeln!(out, "  %v{} = xor i1 %v{}, 1", dst.0, operand.0).unwrap();
        }

        // ── Type Conversions ────────────────────────────────────────
        Inst::IntCast { dst, value, to } => {
            let src_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
            let to_ty = llvm_type(to);
            let src_ty_str = src_ty.map_or("i64", |t| llvm_type(t));

            // IntCast to float type → use sitofp/uitofp to convert value (not bitcast)
            if to.is_float() {
                if src_ty.map_or(false, |t| t.is_integer()) {
                    // Integer → Float: convert value
                    let op = if src_ty.map_or(true, is_signed) { "sitofp" } else { "uitofp" };
                    writeln!(out, "  %v{} = {op} {src_ty_str} %v{} to {to_ty}", dst.0, value.0).unwrap();
                } else {
                    writeln!(out, "  %v{} = fadd {to_ty} 0.0, %v{}", dst.0, value.0).unwrap();
                }
            } else {
                let src_bits = src_ty.map_or(64, int_bits);
                let to_bits = int_bits(to);
                if src_ty.map_or(false, |t| t.is_float()) {
                    // Float → Integer: convert value
                    let op = if is_signed(to) { "fptosi" } else { "fptoui" };
                    writeln!(out, "  %v{} = {op} {src_ty_str} %v{} to {to_ty}", dst.0, value.0).unwrap();
                } else if src_bits == to_bits {
                    writeln!(out, "  %v{} = add {to_ty} 0, %v{}", dst.0, value.0).unwrap();
                } else if to_bits > src_bits {
                    let ext = if src_ty.map_or(true, is_signed) { "sext" } else { "zext" };
                    writeln!(out, "  %v{} = {ext} {src_ty_str} %v{} to {to_ty}", dst.0, value.0).unwrap();
                } else {
                    writeln!(out, "  %v{} = trunc {src_ty_str} %v{} to {to_ty}", dst.0, value.0).unwrap();
                }
            }
        }
        Inst::FloatCast { dst, value, to } => {
            let src_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
            let to_ty = llvm_type(to);
            let src_ty_str = src_ty.map_or("double", |t| llvm_type(t));
            if matches!(to, LirType::F64) {
                writeln!(out, "  %v{} = fpext {src_ty_str} %v{} to {to_ty}", dst.0, value.0).unwrap();
            } else {
                writeln!(out, "  %v{} = fptrunc {src_ty_str} %v{} to {to_ty}", dst.0, value.0).unwrap();
            }
        }
        Inst::IntToFloat { dst, value, to } => {
            let src_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
            let to_ty = llvm_type(to);
            let src_ty_str = src_ty.map_or("i64", |t| llvm_type(t));
            let op = if src_ty.map_or(true, is_signed) { "sitofp" } else { "uitofp" };
            writeln!(out, "  %v{} = {op} {src_ty_str} %v{} to {to_ty}", dst.0, value.0).unwrap();
        }
        Inst::FloatToInt { dst, value, to } => {
            let src_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
            let to_ty = llvm_type(to);
            let src_ty_str = src_ty.map_or("double", |t| llvm_type(t));
            let op = if is_signed(to) { "fptosi" } else { "fptoui" };
            writeln!(out, "  %v{} = {op} {src_ty_str} %v{} to {to_ty}", dst.0, value.0).unwrap();
        }
        Inst::PtrCast { dst, value } => {
            writeln!(out, "  %v{} = bitcast ptr %v{} to ptr", dst.0, value.0).unwrap();
        }
        Inst::Bitcast { dst, value, to } => {
            let src_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
            let to_ty = llvm_type(to);
            let src_ty_str = src_ty.map_or("i64", |t| llvm_type(t));
            writeln!(out, "  %v{} = bitcast {src_ty_str} %v{} to {to_ty}", dst.0, value.0).unwrap();
        }

        // ── Memory ──────────────────────────────────────────────────
        Inst::Load { dst, ptr, ty } => {
            if ty.is_aggregate() {
                // For aggregates, don't load — just alias the pointer.
                // Our codegen model keeps aggregates as pointers throughout.
                writeln!(out, "  %v{} = getelementptr i8, ptr %v{}, i32 0", dst.0, ptr.0).unwrap();
            } else {
                let lty = llvm_type_full(ty, snames);
                writeln!(out, "  %v{} = load {lty}, ptr %v{}", dst.0, ptr.0).unwrap();
            }
        }
        Inst::Store { ptr, value } => {
            // Infer the type of the value being stored
            let val_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
            // Check if the source value is a NullPtr — if so, intent is zero-fill, not memcpy from null.
            let value_is_null = func.blocks.iter().any(|b| {
                b.insts.iter().any(|i| matches!(i, Inst::NullPtr { dst } if dst.0 == value.0))
            });
            if let Some(LirType::PtrTo(sid)) = val_ty {
                // Source is a typed pointer to an aggregate — memcpy the struct contents
                let sz = sizeof_lir_type(&LirType::Struct(*sid), &module.structs, snames);
                writeln!(out, "  call ptr @memcpy(ptr %v{}, ptr %v{}, i64 {sz})", ptr.0, value.0).unwrap();
            } else if matches!(val_ty, Some(LirType::Ptr)) {
                // Source is an opaque ptr (e.g. void* from gorget_array_safe_pop/safe_get).
                // If the destination is a FieldPtr to an aggregate field, the ptr is actually
                // a pointer to that struct's data → emit memcpy instead of store ptr.
                let dest_field_ty = func.blocks.iter().find_map(|b| {
                    b.insts.iter().find_map(|i| {
                        if let Inst::FieldPtr { dst: d, struct_id, field, .. } = i {
                            if d.0 == ptr.0 {
                                let sdef = &module.structs[struct_id.0 as usize];
                                return sdef.fields.get(*field as usize).map(|(_, t)| t.clone());
                            }
                        }
                        None
                    })
                });
                match dest_field_ty {
                    Some(LirType::Struct(sid)) => {
                        // void* points to an inline struct — memcpy/memset the whole struct.
                        // If the source is a NullPtr, zero the destination instead of reading from null.
                        let sz = sizeof_lir_type(&LirType::Struct(sid), &module.structs, snames);
                        if value_is_null {
                            writeln!(out, "  call ptr @memset(ptr %v{}, i32 0, i64 {sz})", ptr.0).unwrap();
                        } else {
                            writeln!(out, "  call ptr @memcpy(ptr %v{}, ptr %v{}, i64 {sz})", ptr.0, value.0).unwrap();
                        }
                    }
                    Some(scalar_ty) if !scalar_ty.is_ptr() => {
                        // void* points to a scalar (bool, int, float) — load then store
                        let ty_str = llvm_type_full(&scalar_ty, snames);
                        let tmp = format!("voidderef.{}.{}", ptr.0, value.0);
                        writeln!(out, "  %{tmp} = load {ty_str}, ptr %v{}", value.0).unwrap();
                        writeln!(out, "  store {ty_str} %{tmp}, ptr %v{}", ptr.0).unwrap();
                    }
                    _ => {
                        // Unknown or ptr field type — plain ptr store
                        writeln!(out, "  store ptr %v{}, ptr %v{}", value.0, ptr.0).unwrap();
                    }
                }
            } else {
                let ty_str = val_ty.map(|t| llvm_type_full(t, snames))
                    .unwrap_or_else(|| "i64".to_string());
                writeln!(out, "  store {ty_str} %v{}, ptr %v{}", value.0, ptr.0).unwrap();
            }
        }
        Inst::FieldPtr { dst, base, struct_id, field } => {
            let sname = &snames[&struct_id.0];
            let sdef = &module.structs[struct_id.0 as usize];
            if sdef.is_union_layout && *field > 0 {
                // For union-layout enums: { i32 tag, [N x i8] payload }.
                // All variant fields share the payload as a union. The field name
                // encodes the variant (e.g., Triangle_0, Triangle_1). The suffix
                // number is the field's position within that variant.
                let payload_ptr = format!("fptr.{}.payload", dst.0);
                writeln!(out, "  %{payload_ptr} = getelementptr %{sname}, ptr %v{}, i32 0, i32 1", base.0).unwrap();
                // Determine the byte offset within the payload from the variant field suffix.
                // E.g., Triangle_0 → offset 0, Triangle_1 → offset 8.
                let field_name = &sdef.fields[*field as usize].0;
                let variant_field_idx = field_name.rsplit('_').next()
                    .and_then(|s| s.parse::<u32>().ok())
                    .unwrap_or(0);
                if variant_field_idx == 0 {
                    writeln!(out, "  %v{} = bitcast ptr %{payload_ptr} to ptr", dst.0).unwrap();
                } else {
                    // Compute byte offset: accumulate sizes of preceding variant fields.
                    // Find the variant prefix (e.g., "Triangle") and sum sizes of fields 0..idx.
                    let prefix = field_name.rsplitn(2, '_').nth(1).unwrap_or(field_name);
                    let mut byte_offset = 0usize;
                    for f in &sdef.fields[1..] {
                        if f.0.starts_with(prefix) {
                            let f_idx = f.0.rsplit('_').next()
                                .and_then(|s| s.parse::<u32>().ok()).unwrap_or(0);
                            if f_idx < variant_field_idx {
                                let fsz = match &f.1 {
                                    LirType::I8 | LirType::U8 | LirType::Bool => 1,
                                    LirType::I16 | LirType::U16 => 2,
                                    LirType::I32 | LirType::U32 | LirType::F32 => 4,
                                    LirType::I64 | LirType::U64 | LirType::F64 | LirType::Ptr | LirType::PtrTo(_) => 8,
                                    LirType::Struct(sid) => module.structs[sid.0 as usize].computed_c_size.unwrap_or(8),
                                    _ => 8,
                                };
                                // Align to field's natural alignment
                                let align = fsz.min(8).max(1);
                                byte_offset = (byte_offset + align - 1) & !(align - 1);
                                byte_offset += fsz;
                            }
                        }
                    }
                    // Align the final offset
                    let target_field_ty = &sdef.fields[*field as usize].1;
                    let target_align = match target_field_ty {
                        LirType::I8 | LirType::U8 | LirType::Bool => 1,
                        LirType::I16 | LirType::U16 => 2,
                        LirType::I32 | LirType::U32 | LirType::F32 => 4,
                        _ => 8,
                    };
                    byte_offset = (byte_offset + target_align - 1) & !(target_align - 1);
                    writeln!(out, "  %v{} = getelementptr i8, ptr %{payload_ptr}, i64 {byte_offset}", dst.0).unwrap();
                }
            } else {
                writeln!(out, "  %v{} = getelementptr %{sname}, ptr %v{}, i32 0, i32 {field}", dst.0, base.0).unwrap();
            }
        }
        Inst::ElemPtr { dst, base, index, elem_size } => {
            // base + index * elem_size, computed as byte offset
            let offset = format!("eptr.{}.off", dst.0);
            writeln!(out, "  %{offset} = mul i64 %v{}, {elem_size}", index.0).unwrap();
            writeln!(out, "  %v{} = getelementptr i8, ptr %v{}, i64 %{offset}", dst.0, base.0).unwrap();
        }
        Inst::Memset { ptr, byte, size } => {
            // Use memset intrinsic
            writeln!(out, "  call ptr @memset(ptr %v{}, i32 %v{}, i64 %v{})", ptr.0, byte.0, size.0).unwrap();
        }
        Inst::Memcpy { dst_ptr, src_ptr, size } => {
            writeln!(out, "  call ptr @memcpy(ptr %v{}, ptr %v{}, i64 %v{})", dst_ptr.0, src_ptr.0, size.0).unwrap();
        }

        // ── Calls ───────────────────────────────────────────────────
        Inst::Call { dst, func: fid, args } => {
            let target = &module.functions[fid.0 as usize];
            let ret_ty = llvm_type_full(&target.return_type, snames);
            let is_closure_call_fn = target.name.contains("__call");
            // For aggregate params: if value is ptr but param is aggregate, load first
            let mut load_lines = Vec::new();
            let arg_strs: Vec<String> = args.iter().enumerate().map(|(i, a)| {
                let param_ty = if i < target.params.len() { Some(&target.params[i]) } else { None };
                let actual_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                let is_ptr_val = actual_ty.map_or(false, |t| t.is_ptr());
                let is_agg_param = param_ty.map_or(false, |t| t.is_aggregate());
                let param_is_void_or_ptr = !is_closure_call_fn && param_ty
                    .map_or(false, |p| p.is_ptr() || matches!(p, LirType::Void));

                // Closure→callable wrapping: when passing a __Closure_N struct to a
                // void/ptr parameter, wrap in [fn_ptr, env_ptr] array on the stack.
                if param_is_void_or_ptr {
                    let closure_name = {
                        let check_closure = |sid: &StructId| -> Option<String> {
                            module.structs.get(sid.0 as usize)
                                .filter(|sd| sd.name.starts_with("__Closure_"))
                                .map(|sd| sd.name.clone())
                        };
                        match actual_ty {
                            Some(LirType::Struct(sid)) => check_closure(sid),
                            Some(LirType::PtrTo(sid)) => check_closure(sid),
                            _ => None,
                        }
                    };
                    if let Some(cname) = closure_name {
                        let call_fn = format!("{cname}__call");
                        let pfx = format!("cw.{}.{i}", a.0);
                        load_lines.push(format!("  %{pfx} = alloca [2 x ptr]"));
                        load_lines.push(format!("  %{pfx}.0 = getelementptr [2 x ptr], ptr %{pfx}, i32 0, i32 0"));
                        load_lines.push(format!("  store ptr @{call_fn}, ptr %{pfx}.0"));
                        load_lines.push(format!("  %{pfx}.1 = getelementptr [2 x ptr], ptr %{pfx}, i32 0, i32 1"));
                        load_lines.push(format!("  store ptr %v{}, ptr %{pfx}.1", a.0));
                        return format!("ptr %{pfx}");
                    }
                }

                if is_agg_param && is_ptr_val {
                    let pty = llvm_arg_type(param_ty.unwrap(), snames);
                    let load_name = format!("arg.load.{}.{i}", a.0);
                    load_lines.push(format!("  %{load_name} = load {pty}, ptr %v{}", a.0));
                    format!("{pty} %{load_name}")
                } else {
                    let pty = param_ty.map(|t| llvm_arg_type(t, snames))
                        .unwrap_or_else(|| actual_ty.map(|t| llvm_arg_type(t, snames))
                            .unwrap_or_else(|| "i64".to_string()));
                    format!("{pty} %v{}", a.0)
                }
            }).collect();
            for line in &load_lines {
                writeln!(out, "{line}").unwrap();
            }
            let call_name = c_func_name(&target.name);
            if let Some(d) = dst {
                if target.return_type == LirType::Void {
                    writeln!(out, "  call void @{}({})", call_name, arg_strs.join(", ")).unwrap();
                } else if needs_sret(&target.return_type, &module.structs) {
                    // Large aggregate return: sret convention
                    writeln!(out, "  %v{} = alloca {ret_ty}", d.0).unwrap();
                    let sret_args = if arg_strs.is_empty() {
                        format!("ptr sret({ret_ty}) %v{}", d.0)
                    } else {
                        format!("ptr sret({ret_ty}) %v{}, {}", d.0, arg_strs.join(", "))
                    };
                    writeln!(out, "  call void @{}({sret_args})", call_name).unwrap();
                } else if target.return_type.is_aggregate() {
                    // Small aggregate: returned in registers, store to alloca
                    writeln!(out, "  %v{}.ret = call {ret_ty} @{}({})", d.0, call_name, arg_strs.join(", ")).unwrap();
                    writeln!(out, "  %v{} = alloca {ret_ty}", d.0).unwrap();
                    writeln!(out, "  store {ret_ty} %v{}.ret, ptr %v{}", d.0, d.0).unwrap();
                } else {
                    writeln!(out, "  %v{} = call {ret_ty} @{}({})", d.0, call_name, arg_strs.join(", ")).unwrap();
                }
            } else {
                writeln!(out, "  call {ret_ty} @{}({})", call_name, arg_strs.join(", ")).unwrap();
            }
        }
        Inst::CallExtern { dst, name, args, .. } => {
            // Special case: printf/fprintf with GorgetString arg → extract .data field
            // Drop-if-alive guards — no-op in LLVM (drops happen unconditionally)
            if name.starts_with("__gorget_drop_if_alive_open__") || name == "__gorget_drop_if_alive_close" {
                writeln!(out, "  ; {name} (no-op in LLVM)").unwrap();
                return;
            }

            // ── Drop flag guards (V3) ──────────────────────────────────────
            // __gorget_drop_flag_open(bool_val) → conditional branch: if true, execute drops
            // __gorget_drop_flag_close → end of conditional drop block
            if name == "__gorget_drop_flag_open" {
                let uid = *trap_counter;
                *trap_counter += 1;
                let flag_val = args.first().map(|a| a.0).unwrap_or(0);
                let body_label = format!("df.{block_id}.{uid}.body");
                let join_label = format!("df.{block_id}.{uid}.join");
                writeln!(out, "  br i1 %v{flag_val}, label %{body_label}, label %{join_label}").unwrap();
                writeln!(out, "{body_label}:").unwrap();
                *current_label = body_label;
                df_stack.push(uid);
                return;
            }
            if name == "__gorget_drop_flag_close" {
                if let Some(uid) = df_stack.pop() {
                    let join_label = format!("df.{block_id}.{uid}.join");
                    writeln!(out, "  br label %{join_label}").unwrap();
                    writeln!(out, "{join_label}:").unwrap();
                    *current_label = join_label;
                }
                return;
            }

            // ── __callable_N[__FUNC] — inline callable parameter dispatch via void*[2] ──
            // The callable param is a pointer to [fn_ptr, env_ptr].
            // In LLVM IR: load fn_ptr, load env_ptr, indirect call.
            if name.starts_with("__callable_") {
                let id_str = &name["__callable_".len()..];
                let id_num = id_str.split("__").next().unwrap_or(id_str);
                if id_num.parse::<u32>().is_ok() && !args.is_empty() {
                    let uid = *trap_counter;
                    *trap_counter += 1;
                    let closure_val = args[0];
                    let actual_args = &args[1..];
                    let pfx = format!("callable.{block_id}.{uid}");
                    // Load fn_ptr from callable[0]
                    writeln!(out, "  %{pfx}.fnp = load ptr, ptr %v{}", closure_val.0).unwrap();
                    // Load env_ptr from callable[1]
                    writeln!(out, "  %{pfx}.envgep = getelementptr ptr, ptr %v{}, i32 1", closure_val.0).unwrap();
                    writeln!(out, "  %{pfx}.env = load ptr, ptr %{pfx}.envgep").unwrap();
                    // Build arg types + values for indirect call
                    let mut call_arg_strs = vec![format!("ptr %{pfx}.env")];
                    for a in actual_args {
                        let pty = val_types.get(a.0 as usize)
                            .and_then(|t| t.as_ref())
                            .map(|t| llvm_arg_type(t, snames))
                            .unwrap_or_else(|| "i64".to_string());
                        call_arg_strs.push(format!("{pty} %v{}", a.0));
                    }
                    let joined_args = call_arg_strs.join(", ");
                    // Detect sret/small-agg return convention from dst LirType.
                    // val_types stores large aggregates as PtrTo(sid); map back to Struct(sid).
                    let dst_lir_ty = dst.and_then(|d| {
                        val_types.get(d.0 as usize).and_then(|t| t.as_ref()).cloned()
                    });
                    let (call_is_sret, call_is_small_agg, call_struct_sid) = match &dst_lir_ty {
                        Some(LirType::PtrTo(sid)) => {
                            let as_struct = LirType::Struct(*sid);
                            (needs_sret(&as_struct, &module.structs),
                             is_small_aggregate(&as_struct, &module.structs),
                             Some(*sid))
                        }
                        _ => (false, false, None),
                    };
                    if let Some(d) = dst {
                        if call_is_sret {
                            let sid = call_struct_sid.unwrap();
                            let struct_llvm = format!("%{}", snames.get(&sid.0).unwrap_or(&"unknown".to_string()));
                            // Use %v{d.0} as the sret slot directly — allocate it, then pass as sret arg.
                            writeln!(out, "  %v{} = alloca {struct_llvm}", d.0).unwrap();
                            writeln!(out, "  call void %{pfx}.fnp(ptr sret({struct_llvm}) %v{}, {joined_args})", d.0).unwrap();
                        } else if call_is_small_agg {
                            let sid = call_struct_sid.unwrap();
                            let struct_llvm = format!("%{}", snames.get(&sid.0).unwrap_or(&"unknown".to_string()));
                            writeln!(out, "  %{pfx}.ret = call {struct_llvm} %{pfx}.fnp({joined_args})").unwrap();
                            writeln!(out, "  %v{} = alloca {struct_llvm}", d.0).unwrap();
                            writeln!(out, "  store {struct_llvm} %{pfx}.ret, ptr %v{}", d.0).unwrap();
                        } else {
                            let ret_type = dst_lir_ty.as_ref()
                                .map(|t| llvm_arg_type(t, snames))
                                .unwrap_or_else(|| "i64".to_string());
                            writeln!(out, "  %v{} = call {ret_type} %{pfx}.fnp({joined_args})", d.0).unwrap();
                        }
                    } else {
                        writeln!(out, "  call void %{pfx}.fnp({joined_args})").unwrap();
                    }
                    return;
                }
            }

            // ── __gorget_closure_call_N — escaped closure dispatch via GorgetClosure struct ──
            // GorgetClosure = { fn_ptr: ptr, env: ptr }
            if name.starts_with("__gorget_closure_call_") {
                let id_str = &name["__gorget_closure_call_".len()..];
                let id_num = id_str.split("__").next().unwrap_or(id_str);
                if id_num.parse::<u32>().is_ok() && !args.is_empty() {
                    let uid = *trap_counter;
                    *trap_counter += 1;
                    let closure_val = args[0];
                    let actual_args = &args[1..];
                    let pfx = format!("closurecall.{block_id}.{uid}");
                    // GorgetClosure.fn_ptr is field 0, GorgetClosure.env is field 1
                    writeln!(out, "  %{pfx}.fpgep = getelementptr %GorgetClosure, ptr %v{}, i32 0, i32 0", closure_val.0).unwrap();
                    writeln!(out, "  %{pfx}.fnp = load ptr, ptr %{pfx}.fpgep").unwrap();
                    writeln!(out, "  %{pfx}.envgep = getelementptr %GorgetClosure, ptr %v{}, i32 0, i32 1", closure_val.0).unwrap();
                    writeln!(out, "  %{pfx}.env = load ptr, ptr %{pfx}.envgep").unwrap();
                    let mut call_arg_strs = vec![format!("ptr %{pfx}.env")];
                    for a in actual_args {
                        let pty = val_types.get(a.0 as usize)
                            .and_then(|t| t.as_ref())
                            .map(|t| llvm_arg_type(t, snames))
                            .unwrap_or_else(|| "i64".to_string());
                        call_arg_strs.push(format!("{pty} %v{}", a.0));
                    }
                    let joined_args = call_arg_strs.join(", ");
                    // Detect sret/small-agg return convention from dst LirType.
                    let dst_lir_ty = dst.and_then(|d| {
                        val_types.get(d.0 as usize).and_then(|t| t.as_ref()).cloned()
                    });
                    let (call_is_sret, call_is_small_agg, call_struct_sid) = match &dst_lir_ty {
                        Some(LirType::PtrTo(sid)) => {
                            let as_struct = LirType::Struct(*sid);
                            (needs_sret(&as_struct, &module.structs),
                             is_small_aggregate(&as_struct, &module.structs),
                             Some(*sid))
                        }
                        _ => (false, false, None),
                    };
                    if let Some(d) = dst {
                        if call_is_sret {
                            let sid = call_struct_sid.unwrap();
                            let struct_llvm = format!("%{}", snames.get(&sid.0).unwrap_or(&"unknown".to_string()));
                            writeln!(out, "  %v{} = alloca {struct_llvm}", d.0).unwrap();
                            writeln!(out, "  call void %{pfx}.fnp(ptr sret({struct_llvm}) %v{}, {joined_args})", d.0).unwrap();
                        } else if call_is_small_agg {
                            let sid = call_struct_sid.unwrap();
                            let struct_llvm = format!("%{}", snames.get(&sid.0).unwrap_or(&"unknown".to_string()));
                            writeln!(out, "  %{pfx}.ret = call {struct_llvm} %{pfx}.fnp({joined_args})").unwrap();
                            writeln!(out, "  %v{} = alloca {struct_llvm}", d.0).unwrap();
                            writeln!(out, "  store {struct_llvm} %{pfx}.ret, ptr %v{}", d.0).unwrap();
                        } else {
                            let ret_type = dst_lir_ty.as_ref()
                                .map(|t| llvm_arg_type(t, snames))
                                .unwrap_or_else(|| "i64".to_string());
                            writeln!(out, "  %v{} = call {ret_type} %{pfx}.fnp({joined_args})", d.0).unwrap();
                        }
                    } else {
                        writeln!(out, "  call void %{pfx}.fnp({joined_args})").unwrap();
                    }
                    return;
                }
            }

            // ── Newtype constructors ──────────────────────────────
            // If the extern name matches a struct name with exactly 1 field,
            // inline the construction: alloca + store field 0.
            if let Some(d) = dst {
                let newtype_sid = module.structs.iter().enumerate().find_map(|(i, s)| {
                    if (s.name == *name || snames.get(&(i as u32)).map_or(false, |n| n == name))
                        && s.fields.len() == 1 && !s.name.starts_with("Option__") && !s.name.starts_with("Result__") {
                        Some(StructId(i as u32))
                    } else { None }
                });
                if let Some(sid) = newtype_sid {
                    if args.len() == 1 {
                        let struct_ty = format!("%{}", snames.get(&sid.0).unwrap_or(&name.to_string()));
                        let field_ty = llvm_type_full(&module.structs[sid.0 as usize].fields[0].1, snames);
                        let arg_ty = val_types.get(args[0].0 as usize).and_then(|t| t.as_ref());
                        writeln!(out, "  %v{} = alloca {struct_ty}", d.0).unwrap();
                        let fptr = format!("nt.{block_id}.{}.fp", d.0);
                        writeln!(out, "  %{fptr} = getelementptr {struct_ty}, ptr %v{}, i32 0, i32 0", d.0).unwrap();
                        if arg_ty.map_or(false, |t| t.is_ptr()) && !module.structs[sid.0 as usize].fields[0].1.is_ptr() {
                            // Arg is ptr (from alloca), field is scalar — load then store
                            let loaded = format!("nt.{block_id}.{}.ld", d.0);
                            writeln!(out, "  %{loaded} = load {field_ty}, ptr %v{}", args[0].0).unwrap();
                            writeln!(out, "  store {field_ty} %{loaded}, ptr %{fptr}").unwrap();
                        } else {
                            writeln!(out, "  store {field_ty} %v{}, ptr %{fptr}", args[0].0).unwrap();
                        }
                        return;
                    }
                }
            }

            // ── Collection constructors (Vector__T, Set__T, Dict__K__V) ──
            // Vector__int64_t() → gorget_array_new(sizeof(int64_t))
            // Vector__int64_t(cap) → gorget_array_with_capacity(sizeof(int64_t), cap)
            if let Some(d) = dst {
                let is_collection_ctor = (name.starts_with("Vector__") || name.starts_with("Set__")
                    || name.starts_with("Dict__") || name.starts_with("HashMap__") || name.starts_with("HashSet__"))
                    && !name.contains("__map") && !name.contains("__filter") && !name.contains("__get")
                    && !name.contains("__put") && !name.contains("__push") && !name.contains("__len")
                    && !name.contains("__pop") && !name.contains("__remove") && !name.contains("__contains")
                    && !name.contains("__clone") && !name.contains("__free") && !name.contains("__drop")
                    && !name.contains("__keys") && !name.contains("__values") && !name.contains("__items")
                    && !name.contains("__clear") && !name.contains("__is_empty") && !name.contains("__new")
                    && !name.contains("__each") && !name.contains("__any") && !name.contains("__all")
                    && !name.contains("__fold") && !name.contains("__reduce") && !name.contains("__find")
                    && !name.contains("__count") && !name.contains("__sort") && !name.contains("__reverse")
                    && !name.contains("__set") && !name.contains("__insert") && !name.contains("__add")
                    && !name.contains("__extend") && !name.contains("__slice") && !name.contains("__concat")
                    && !name.contains("__to_array") && !name.contains("__dedup") && !name.contains("__reserve")
                    && !name.contains("__capacity") && !name.contains("__index_of") && !name.contains("__binary_search")
                    && !name.contains("__unique") && !name.contains("__flat_map") && !name.contains("__sorted")
                    && !name.contains("__union") && !name.contains("__intersection") && !name.contains("__difference")
                    && !name.contains("__symmetric_difference") && !name.contains("__update") && !name.contains("__from")
                    && !name.contains("__is_subset") && !name.contains("__is_superset")
                    && !name.contains("__is_disjoint") && !name.contains("__equal")
                    // Constructors have 0 or 1 arg (optional capacity). Methods have ≥2 args.
                    && args.len() <= 1;
                if is_collection_ctor {
                    // Determine element size from type name
                    let elem_size: i64 = if name.contains("int64_t") || name.contains("double") || name.contains("GorgetString") { 8 }
                        else if name.contains("int32_t") { 4 }
                        else if name.contains("bool") { 1 }
                        else { 8 }; // default
                    // For GorgetString elements, use 32 (Str struct size)
                    let elem_size: i64 = if name.contains("GorgetString") { 32 } else { elem_size };
                    let ret_ty = if name.starts_with("Vector__") { "%GorgetArray" }
                        else if name.starts_with("Set__") || name.starts_with("HashSet__") { "%GorgetSet" }
                        else { "%GorgetMap" };
                    writeln!(out, "  %v{} = alloca {ret_ty}", d.0).unwrap();
                    if args.is_empty() {
                        writeln!(out, "  call void @gorget_array_new(ptr sret({ret_ty}) %v{}, i64 {elem_size})", d.0).unwrap();
                    } else {
                        writeln!(out, "  call void @gorget_array_with_capacity(ptr sret({ret_ty}) %v{}, i64 {elem_size}, i64 %v{})", d.0, args[0].0).unwrap();
                    }
                    return;
                }
            }

            // ── Inline Vector higher-order methods (map/filter/each/any/all/etc.) ──
            // These must be inlined at each call site because the C wrapper functions
            // hardcode a single closure — calling with a different closure at a
            // different site would invoke the wrong function.
            if let Some((elem_c, method)) = parse_vector_hof(name) {
                let has_dst = dst.is_some();
                let needs_inline = match method {
                    "each" => true,
                    "filter" | "map" | "flat_map" | "fold" | "reduce"
                    | "any" | "all" | "find" | "find_index" | "count" => has_dst,
                    _ => false,
                };
                if needs_inline && !args.is_empty() {
                    let uid = *trap_counter;
                    *trap_counter += 1;
                    let pfx = format!("hof.{block_id}.{uid}");
                    let arr_arg = args[0];
                    let (elem_llvm_ty, elem_size) = elem_c_to_llvm(elem_c, module, snames);
                    let elem_is_aggregate = elem_llvm_ty.starts_with('%');

                    // Resolve closure call function
                    let closure_arg = if method == "fold" && args.len() >= 3 {
                        Some(args[2])
                    } else if args.len() >= 2 {
                        Some(*args.last().unwrap())
                    } else {
                        None
                    };
                    let closure_info = closure_arg.and_then(|ca| resolve_closure_call_fn(ca, val_types, module));

                    // Determine the closure call function name and return type
                    let (call_fn, ret_ty, params_are_ptr) = match &closure_info {
                        Some((name, rty, pap)) => (name.clone(), rty.clone(), pap.clone()),
                        None => {
                            // Can't resolve closure — fall through to generic handler
                            // (This shouldn't happen for well-formed HOF calls)
                            writeln!(out, "  ; WARNING: could not resolve closure for {name}").unwrap();
                            // Fall through below will handle it
                            ("".to_string(), LirType::Void, vec![])
                        }
                    };

                    if closure_info.is_some() {
                        let closure_val = closure_arg.unwrap();
                        // Check if the closure param takes the element by pointer
                        let elem_by_ptr = params_are_ptr.first().copied().unwrap_or(false);
                        // aarch64 ABI: aggregates ≤16 bytes pass in registers (by value);
                        // larger ones pass by pointer. When elem_is_aggregate, only pass ptr
                        // for large structs — small structs must be loaded and passed by value.
                        let elem_pass_by_ptr = elem_by_ptr || (elem_is_aggregate && elem_size > 16);

                        match method {
                            "map" => {
                                let d = dst.unwrap();
                                let ret_llvm = llvm_type_full(&ret_ty, snames);
                                let ret_is_agg = ret_llvm.starts_with('%');
                                let ret_size = sizeof_lir_type(&ret_ty, &module.structs, snames);
                                // Determine if call_fn uses sret for aggregate return
                                let map_ret_sret = ret_is_agg && !is_small_aggregate(&ret_ty, &module.structs);
                                let map_ret_small_agg = ret_is_agg && is_small_aggregate(&ret_ty, &module.structs);

                                // Alloca for result array
                                writeln!(out, "  %v{} = alloca %GorgetArray", d.0).unwrap();
                                writeln!(out, "  call void @gorget_array_new(ptr sret(%GorgetArray) %v{}, i64 {ret_size})", d.0).unwrap();
                                // Loop
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.check:").unwrap();
                                writeln!(out, "  %{pfx}.i = phi i64 [0, %{current_label}], [%{pfx}.next, %{pfx}.body]").unwrap();
                                writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                                writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len").unwrap();
                                writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                writeln!(out, "{pfx}.body:").unwrap();
                                // Load element
                                writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                // Call closure with element
                                if elem_by_ptr {
                                    // Pass element by pointer
                                    if map_ret_sret {
                                        writeln!(out, "  %{pfx}.tmp = alloca {ret_llvm}").unwrap();
                                        writeln!(out, "  call void @{call_fn}(ptr sret({ret_llvm}) %{pfx}.tmp, ptr %v{}, ptr %{pfx}.ep)", closure_val.0).unwrap();
                                        writeln!(out, "  call void @gorget_array_push(ptr %v{}, ptr %{pfx}.tmp)", d.0).unwrap();
                                    } else if map_ret_small_agg {
                                        writeln!(out, "  %{pfx}.out.v = call {ret_llvm} @{call_fn}(ptr %v{}, ptr %{pfx}.ep)", closure_val.0).unwrap();
                                        writeln!(out, "  %{pfx}.tmp = alloca {ret_llvm}").unwrap();
                                        writeln!(out, "  store {ret_llvm} %{pfx}.out.v, ptr %{pfx}.tmp").unwrap();
                                        writeln!(out, "  call void @gorget_array_push(ptr %v{}, ptr %{pfx}.tmp)", d.0).unwrap();
                                    } else {
                                        let ret_ll = llvm_type_full(&ret_ty, snames);
                                        writeln!(out, "  %{pfx}.out = call {ret_ll} @{call_fn}(ptr %v{}, ptr %{pfx}.ep)", closure_val.0).unwrap();
                                        writeln!(out, "  %{pfx}.tmp = alloca {ret_ll}").unwrap();
                                        writeln!(out, "  store {ret_ll} %{pfx}.out, ptr %{pfx}.tmp").unwrap();
                                        writeln!(out, "  call void @gorget_array_push(ptr %v{}, ptr %{pfx}.tmp)", d.0).unwrap();
                                    }
                                } else if elem_pass_by_ptr {
                                    // Large aggregate — pass pointer to it
                                    if map_ret_sret {
                                        writeln!(out, "  %{pfx}.tmp = alloca {ret_llvm}").unwrap();
                                        writeln!(out, "  call void @{call_fn}(ptr sret({ret_llvm}) %{pfx}.tmp, ptr %v{}, ptr %{pfx}.ep)", closure_val.0).unwrap();
                                        writeln!(out, "  call void @gorget_array_push(ptr %v{}, ptr %{pfx}.tmp)", d.0).unwrap();
                                    } else if map_ret_small_agg {
                                        writeln!(out, "  %{pfx}.out.v = call {ret_llvm} @{call_fn}(ptr %v{}, ptr %{pfx}.ep)", closure_val.0).unwrap();
                                        writeln!(out, "  %{pfx}.tmp = alloca {ret_llvm}").unwrap();
                                        writeln!(out, "  store {ret_llvm} %{pfx}.out.v, ptr %{pfx}.tmp").unwrap();
                                        writeln!(out, "  call void @gorget_array_push(ptr %v{}, ptr %{pfx}.tmp)", d.0).unwrap();
                                    } else {
                                        let ret_ll = llvm_type_full(&ret_ty, snames);
                                        writeln!(out, "  %{pfx}.out = call {ret_ll} @{call_fn}(ptr %v{}, ptr %{pfx}.ep)", closure_val.0).unwrap();
                                        writeln!(out, "  %{pfx}.tmp = alloca {ret_ll}").unwrap();
                                        writeln!(out, "  store {ret_ll} %{pfx}.out, ptr %{pfx}.tmp").unwrap();
                                        writeln!(out, "  call void @gorget_array_push(ptr %v{}, ptr %{pfx}.tmp)", d.0).unwrap();
                                    }
                                } else {
                                    // Scalar or small aggregate — load and pass by value
                                    writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                    if map_ret_sret {
                                        writeln!(out, "  %{pfx}.tmp = alloca {ret_llvm}").unwrap();
                                        writeln!(out, "  call void @{call_fn}(ptr sret({ret_llvm}) %{pfx}.tmp, ptr %v{}, {elem_llvm_ty} %{pfx}.elem)", closure_val.0).unwrap();
                                        writeln!(out, "  call void @gorget_array_push(ptr %v{}, ptr %{pfx}.tmp)", d.0).unwrap();
                                    } else if map_ret_small_agg {
                                        writeln!(out, "  %{pfx}.out.v = call {ret_llvm} @{call_fn}(ptr %v{}, {elem_llvm_ty} %{pfx}.elem)", closure_val.0).unwrap();
                                        writeln!(out, "  %{pfx}.tmp = alloca {ret_llvm}").unwrap();
                                        writeln!(out, "  store {ret_llvm} %{pfx}.out.v, ptr %{pfx}.tmp").unwrap();
                                        writeln!(out, "  call void @gorget_array_push(ptr %v{}, ptr %{pfx}.tmp)", d.0).unwrap();
                                    } else {
                                        let ret_ll = llvm_type_full(&ret_ty, snames);
                                        writeln!(out, "  %{pfx}.out = call {ret_ll} @{call_fn}(ptr %v{}, {elem_llvm_ty} %{pfx}.elem)", closure_val.0).unwrap();
                                        writeln!(out, "  %{pfx}.tmp = alloca {ret_ll}").unwrap();
                                        writeln!(out, "  store {ret_ll} %{pfx}.out, ptr %{pfx}.tmp").unwrap();
                                        writeln!(out, "  call void @gorget_array_push(ptr %v{}, ptr %{pfx}.tmp)", d.0).unwrap();
                                    }
                                }
                                // Increment
                                writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.done:").unwrap();
                                *current_label = format!("{pfx}.done");
                                return;
                            }
                            "filter" => {
                                let d = dst.unwrap();
                                // Alloca for result array
                                writeln!(out, "  %v{} = alloca %GorgetArray", d.0).unwrap();
                                writeln!(out, "  call void @gorget_array_new(ptr sret(%GorgetArray) %v{}, i64 {elem_size})", d.0).unwrap();
                                // Loop
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.check:").unwrap();
                                writeln!(out, "  %{pfx}.i = phi i64 [0, %{current_label}], [%{pfx}.next, %{pfx}.skip]").unwrap();
                                writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                                writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len").unwrap();
                                writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                writeln!(out, "{pfx}.body:").unwrap();
                                // Load element pointer
                                writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                // Call closure with element → bool
                                if elem_pass_by_ptr {
                                    writeln!(out, "  %{pfx}.keep = call i1 @{call_fn}(ptr %v{}, ptr %{pfx}.ep)", closure_val.0).unwrap();
                                } else {
                                    writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                    writeln!(out, "  %{pfx}.keep = call i1 @{call_fn}(ptr %v{}, {elem_llvm_ty} %{pfx}.elem)", closure_val.0).unwrap();
                                }
                                // Conditional push
                                writeln!(out, "  br i1 %{pfx}.keep, label %{pfx}.push, label %{pfx}.skip").unwrap();
                                writeln!(out, "{pfx}.push:").unwrap();
                                writeln!(out, "  call void @gorget_array_push(ptr %v{}, ptr %{pfx}.ep)", d.0).unwrap();
                                writeln!(out, "  br label %{pfx}.skip").unwrap();
                                writeln!(out, "{pfx}.skip:").unwrap();
                                // Increment
                                writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.done:").unwrap();
                                *current_label = format!("{pfx}.done");
                                return;
                            }
                            "each" => {
                                // Loop without collecting results
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.check:").unwrap();
                                writeln!(out, "  %{pfx}.i = phi i64 [0, %{current_label}], [%{pfx}.next, %{pfx}.body]").unwrap();
                                writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                                writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len").unwrap();
                                writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                writeln!(out, "{pfx}.body:").unwrap();
                                writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                if elem_pass_by_ptr {
                                    writeln!(out, "  call void @{call_fn}(ptr %v{}, ptr %{pfx}.ep)", closure_val.0).unwrap();
                                } else {
                                    writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                    writeln!(out, "  call void @{call_fn}(ptr %v{}, {elem_llvm_ty} %{pfx}.elem)", closure_val.0).unwrap();
                                }
                                writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.done:").unwrap();
                                *current_label = format!("{pfx}.done");
                                return;
                            }
                            "any" => {
                                let d = dst.unwrap();
                                // any: return true if any element matches
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.check:").unwrap();
                                writeln!(out, "  %{pfx}.i = phi i64 [0, %{current_label}], [%{pfx}.next, %{pfx}.cont]").unwrap();
                                writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                                writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len").unwrap();
                                writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                writeln!(out, "{pfx}.body:").unwrap();
                                writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                if elem_pass_by_ptr {
                                    writeln!(out, "  %{pfx}.pred = call i1 @{call_fn}(ptr %v{}, ptr %{pfx}.ep)", closure_val.0).unwrap();
                                } else {
                                    writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                    writeln!(out, "  %{pfx}.pred = call i1 @{call_fn}(ptr %v{}, {elem_llvm_ty} %{pfx}.elem)", closure_val.0).unwrap();
                                }
                                writeln!(out, "  br i1 %{pfx}.pred, label %{pfx}.found, label %{pfx}.cont").unwrap();
                                writeln!(out, "{pfx}.cont:").unwrap();
                                writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.found:").unwrap();
                                writeln!(out, "  br label %{pfx}.done").unwrap();
                                writeln!(out, "{pfx}.done:").unwrap();
                                // Use i64 (0/1) to match C ABI bool convention
                                writeln!(out, "  %{pfx}.b = phi i1 [true, %{pfx}.found], [false, %{pfx}.check]").unwrap();
                                writeln!(out, "  %v{} = zext i1 %{pfx}.b to i64", d.0).unwrap();
                                *current_label = format!("{pfx}.done");
                                return;
                            }
                            "all" => {
                                let d = dst.unwrap();
                                // all: return false if any element doesn't match
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.check:").unwrap();
                                writeln!(out, "  %{pfx}.i = phi i64 [0, %{current_label}], [%{pfx}.next, %{pfx}.cont]").unwrap();
                                writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                                writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len").unwrap();
                                writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                writeln!(out, "{pfx}.body:").unwrap();
                                writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                if elem_pass_by_ptr {
                                    writeln!(out, "  %{pfx}.pred = call i1 @{call_fn}(ptr %v{}, ptr %{pfx}.ep)", closure_val.0).unwrap();
                                } else {
                                    writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                    writeln!(out, "  %{pfx}.pred = call i1 @{call_fn}(ptr %v{}, {elem_llvm_ty} %{pfx}.elem)", closure_val.0).unwrap();
                                }
                                writeln!(out, "  br i1 %{pfx}.pred, label %{pfx}.cont, label %{pfx}.fail").unwrap();
                                writeln!(out, "{pfx}.cont:").unwrap();
                                writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.fail:").unwrap();
                                writeln!(out, "  br label %{pfx}.done").unwrap();
                                writeln!(out, "{pfx}.done:").unwrap();
                                // Use i64 (0/1) to match C ABI bool convention
                                writeln!(out, "  %{pfx}.b = phi i1 [false, %{pfx}.fail], [true, %{pfx}.check]").unwrap();
                                writeln!(out, "  %v{} = zext i1 %{pfx}.b to i64", d.0).unwrap();
                                *current_label = format!("{pfx}.done");
                                return;
                            }
                            "fold" if args.len() >= 3 => {
                                let d = dst.unwrap();
                                let acc_arg = args[1];
                                let acc_ty = val_types.get(d.0 as usize)
                                    .and_then(|t| t.as_ref())
                                    .cloned()
                                    .unwrap_or(LirType::I64);
                                let acc_llvm = llvm_type_full(&acc_ty, snames);
                                let acc_is_agg = acc_llvm.starts_with('%');
                                // fold: accumulate over array
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.check:").unwrap();
                                writeln!(out, "  %{pfx}.i = phi i64 [0, %{current_label}], [%{pfx}.next, %{pfx}.body]").unwrap();
                                if acc_is_agg {
                                    writeln!(out, "  %{pfx}.acc = phi ptr [%v{}, %{current_label}], [%{pfx}.accnew, %{pfx}.body]", acc_arg.0).unwrap();
                                } else {
                                    writeln!(out, "  %{pfx}.acc = phi {acc_llvm} [%v{}, %{current_label}], [%{pfx}.accnew, %{pfx}.body]", acc_arg.0).unwrap();
                                }
                                writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                                writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len").unwrap();
                                writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                writeln!(out, "{pfx}.body:").unwrap();
                                writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                // fold closure takes (env, acc, elem)
                                let fold_needs_ref = params_are_ptr.clone();
                                let acc_ref = fold_needs_ref.first().copied().unwrap_or(false);
                                let elem_ref = fold_needs_ref.get(1).copied().unwrap_or(false);
                                // Build call: the return is the new accumulator
                                let acc_param = if acc_is_agg || acc_ref {
                                    format!("ptr %{pfx}.acc")
                                } else {
                                    format!("{acc_llvm} %{pfx}.acc")
                                };
                                let elem_param = if elem_pass_by_ptr || elem_ref {
                                    format!("ptr %{pfx}.ep")
                                } else {
                                    writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                    format!("{elem_llvm_ty} %{pfx}.elem")
                                };
                                // Use the closure's actual return type to determine sret convention.
                                // acc_ty may be LirType::Ptr (e.g. for String), but the closure
                                // function may still use sret for its struct return — use ret_ty.
                                let fold_ret_llvm = llvm_type_full(&ret_ty, snames);
                                let fold_ret_sret = needs_sret(&ret_ty, &module.structs);
                                let fold_ret_small = ret_ty.is_aggregate() && is_small_aggregate(&ret_ty, &module.structs);
                                if fold_ret_sret {
                                    writeln!(out, "  %{pfx}.accnew = alloca {fold_ret_llvm}").unwrap();
                                    writeln!(out, "  call void @{call_fn}(ptr sret({fold_ret_llvm}) %{pfx}.accnew, ptr %v{}, {acc_param}, {elem_param})", closure_val.0).unwrap();
                                } else if fold_ret_small {
                                    writeln!(out, "  %{pfx}.accret = call {fold_ret_llvm} @{call_fn}(ptr %v{}, {acc_param}, {elem_param})", closure_val.0).unwrap();
                                    writeln!(out, "  %{pfx}.accnew = alloca {fold_ret_llvm}").unwrap();
                                    writeln!(out, "  store {fold_ret_llvm} %{pfx}.accret, ptr %{pfx}.accnew").unwrap();
                                } else {
                                    writeln!(out, "  %{pfx}.accnew = call {acc_llvm} @{call_fn}(ptr %v{}, {acc_param}, {elem_param})", closure_val.0).unwrap();
                                }
                                writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.done:").unwrap();
                                // Result is the final accumulator (comes from check block only)
                                if acc_is_agg || fold_ret_sret || fold_ret_small {
                                    writeln!(out, "  %v{} = phi ptr [%{pfx}.acc, %{pfx}.check]", d.0).unwrap();
                                } else {
                                    writeln!(out, "  %v{} = phi {acc_llvm} [%{pfx}.acc, %{pfx}.check]", d.0).unwrap();
                                }
                                *current_label = format!("{pfx}.done");
                                return;
                            }
                            "reduce" => {
                                let d = dst.unwrap();
                                let red_needs_ref = params_are_ptr.clone();
                                let acc_ref = red_needs_ref.first().copied().unwrap_or(false);
                                let elem_ref = red_needs_ref.get(1).copied().unwrap_or(false);
                                // reduce: like fold but init with first element
                                // Load first element as initial accumulator
                                writeln!(out, "  %{pfx}.datap0 = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                if elem_pass_by_ptr || acc_ref {
                                    // Large aggregate or ptr-typed acc: acc is a pointer
                                    writeln!(out, "  %{pfx}.acc0 = getelementptr i8, ptr %{pfx}.datap0, i64 0").unwrap();
                                } else {
                                    writeln!(out, "  %{pfx}.acc0 = load {elem_llvm_ty}, ptr %{pfx}.datap0").unwrap();
                                }
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.check:").unwrap();
                                writeln!(out, "  %{pfx}.i = phi i64 [1, %{current_label}], [%{pfx}.next, %{pfx}.body]").unwrap();
                                if elem_pass_by_ptr || acc_ref {
                                    writeln!(out, "  %{pfx}.acc = phi ptr [%{pfx}.acc0, %{current_label}], [%{pfx}.accnew, %{pfx}.body]").unwrap();
                                } else {
                                    writeln!(out, "  %{pfx}.acc = phi {elem_llvm_ty} [%{pfx}.acc0, %{current_label}], [%{pfx}.accnew, %{pfx}.body]").unwrap();
                                }
                                writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                                writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len").unwrap();
                                writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                writeln!(out, "{pfx}.body:").unwrap();
                                writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                let acc_param = if elem_pass_by_ptr || acc_ref {
                                    format!("ptr %{pfx}.acc")
                                } else {
                                    format!("{elem_llvm_ty} %{pfx}.acc")
                                };
                                let elem_param = if elem_pass_by_ptr || elem_ref {
                                    format!("ptr %{pfx}.ep")
                                } else {
                                    writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                    format!("{elem_llvm_ty} %{pfx}.elem")
                                };
                                if elem_pass_by_ptr {
                                    // Large aggregate acc/elem: use sret or ptr return
                                    let ret_sret = elem_size > 16;
                                    if ret_sret {
                                        writeln!(out, "  %{pfx}.accnew = alloca {elem_llvm_ty}").unwrap();
                                        writeln!(out, "  call void @{call_fn}(ptr sret({elem_llvm_ty}) %{pfx}.accnew, ptr %v{}, {acc_param}, {elem_param})", closure_val.0).unwrap();
                                    } else {
                                        writeln!(out, "  %{pfx}.accnew = call ptr @{call_fn}(ptr %v{}, {acc_param}, {elem_param})", closure_val.0).unwrap();
                                    }
                                } else {
                                    writeln!(out, "  %{pfx}.accnew = call {elem_llvm_ty} @{call_fn}(ptr %v{}, {acc_param}, {elem_param})", closure_val.0).unwrap();
                                }
                                writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.done:").unwrap();
                                if elem_pass_by_ptr || acc_ref {
                                    writeln!(out, "  %v{} = phi ptr [%{pfx}.acc, %{pfx}.check]", d.0).unwrap();
                                } else {
                                    writeln!(out, "  %v{} = phi {elem_llvm_ty} [%{pfx}.acc, %{pfx}.check]", d.0).unwrap();
                                }
                                *current_label = format!("{pfx}.done");
                                return;
                            }
                            "find" => {
                                let d = dst.unwrap();
                                // find: return Option — alloca, set tag=1 (None), loop and set tag=0+payload on match
                                let find_pay_off = option_payload_offset(&elem_llvm_ty) as usize;
                                writeln!(out, "  %v{} = alloca i8, i64 {}", d.0, find_pay_off + elem_size.max(1)).unwrap();
                                // Init to None (tag=1)
                                writeln!(out, "  %{pfx}.tagp = getelementptr i8, ptr %v{}, i32 0", d.0).unwrap();
                                writeln!(out, "  store i32 1, ptr %{pfx}.tagp").unwrap();
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.check:").unwrap();
                                writeln!(out, "  %{pfx}.i = phi i64 [0, %{current_label}], [%{pfx}.next, %{pfx}.cont]").unwrap();
                                writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                                writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len").unwrap();
                                writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                writeln!(out, "{pfx}.body:").unwrap();
                                writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                if elem_pass_by_ptr {
                                    writeln!(out, "  %{pfx}.pred = call i1 @{call_fn}(ptr %v{}, ptr %{pfx}.ep)", closure_val.0).unwrap();
                                } else {
                                    writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                    writeln!(out, "  %{pfx}.pred = call i1 @{call_fn}(ptr %v{}, {elem_llvm_ty} %{pfx}.elem)", closure_val.0).unwrap();
                                }
                                writeln!(out, "  br i1 %{pfx}.pred, label %{pfx}.found, label %{pfx}.cont").unwrap();
                                writeln!(out, "{pfx}.found:").unwrap();
                                // Set tag=0 (Some) and copy element to payload at the correct offset
                                writeln!(out, "  store i32 0, ptr %{pfx}.tagp").unwrap();
                                writeln!(out, "  %{pfx}.payp = getelementptr i8, ptr %v{}, i64 {find_pay_off}", d.0).unwrap();
                                writeln!(out, "  call ptr @memcpy(ptr %{pfx}.payp, ptr %{pfx}.ep, i64 {elem_size})").unwrap();
                                writeln!(out, "  br label %{pfx}.done").unwrap();
                                writeln!(out, "{pfx}.cont:").unwrap();
                                writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.done:").unwrap();
                                *current_label = format!("{pfx}.done");
                                return;
                            }
                            "find_index" => {
                                let d = dst.unwrap();
                                // find_index: return i64 (-1 if not found)
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.check:").unwrap();
                                writeln!(out, "  %{pfx}.i = phi i64 [0, %{current_label}], [%{pfx}.next, %{pfx}.cont]").unwrap();
                                writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                                writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len").unwrap();
                                writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                writeln!(out, "{pfx}.body:").unwrap();
                                writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                if elem_pass_by_ptr {
                                    writeln!(out, "  %{pfx}.pred = call i1 @{call_fn}(ptr %v{}, ptr %{pfx}.ep)", closure_val.0).unwrap();
                                } else {
                                    writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                    writeln!(out, "  %{pfx}.pred = call i1 @{call_fn}(ptr %v{}, {elem_llvm_ty} %{pfx}.elem)", closure_val.0).unwrap();
                                }
                                writeln!(out, "  br i1 %{pfx}.pred, label %{pfx}.found, label %{pfx}.cont").unwrap();
                                writeln!(out, "{pfx}.found:").unwrap();
                                writeln!(out, "  br label %{pfx}.done").unwrap();
                                writeln!(out, "{pfx}.cont:").unwrap();
                                writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.done:").unwrap();
                                writeln!(out, "  %v{} = phi i64 [%{pfx}.i, %{pfx}.found], [-1, %{pfx}.check]", d.0).unwrap();
                                *current_label = format!("{pfx}.done");
                                return;
                            }
                            "count" => {
                                let d = dst.unwrap();
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.check:").unwrap();
                                writeln!(out, "  %{pfx}.i = phi i64 [0, %{current_label}], [%{pfx}.next, %{pfx}.body]").unwrap();
                                writeln!(out, "  %{pfx}.cnt = phi i64 [0, %{current_label}], [%{pfx}.cntnew, %{pfx}.body]").unwrap();
                                writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                                writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len").unwrap();
                                writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                writeln!(out, "{pfx}.body:").unwrap();
                                writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                if elem_pass_by_ptr {
                                    writeln!(out, "  %{pfx}.pred = call i1 @{call_fn}(ptr %v{}, ptr %{pfx}.ep)", closure_val.0).unwrap();
                                } else {
                                    writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                    writeln!(out, "  %{pfx}.pred = call i1 @{call_fn}(ptr %v{}, {elem_llvm_ty} %{pfx}.elem)", closure_val.0).unwrap();
                                }
                                writeln!(out, "  %{pfx}.inc = zext i1 %{pfx}.pred to i64").unwrap();
                                writeln!(out, "  %{pfx}.cntnew = add i64 %{pfx}.cnt, %{pfx}.inc").unwrap();
                                writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.done:").unwrap();
                                writeln!(out, "  %v{} = phi i64 [%{pfx}.cnt, %{pfx}.check]", d.0).unwrap();
                                *current_label = format!("{pfx}.done");
                                return;
                            }
                            "flat_map" => {
                                let d = dst.unwrap();
                                // flat_map: map to arrays and extend
                                writeln!(out, "  %v{} = alloca %GorgetArray", d.0).unwrap();
                                writeln!(out, "  call void @gorget_array_new(ptr sret(%GorgetArray) %v{}, i64 {elem_size})", d.0).unwrap();
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.check:").unwrap();
                                writeln!(out, "  %{pfx}.i = phi i64 [0, %{current_label}], [%{pfx}.next, %{pfx}.body]").unwrap();
                                writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                                writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len").unwrap();
                                writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                writeln!(out, "{pfx}.body:").unwrap();
                                writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                // Call closure → returns GorgetArray
                                writeln!(out, "  %{pfx}.sub = alloca %GorgetArray").unwrap();
                                if elem_pass_by_ptr {
                                    writeln!(out, "  call void @{call_fn}(ptr sret(%GorgetArray) %{pfx}.sub, ptr %v{}, ptr %{pfx}.ep)", closure_val.0).unwrap();
                                } else {
                                    writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                    writeln!(out, "  call void @{call_fn}(ptr sret(%GorgetArray) %{pfx}.sub, ptr %v{}, {elem_llvm_ty} %{pfx}.elem)", closure_val.0).unwrap();
                                }
                                writeln!(out, "  call void @gorget_array_extend(ptr %v{}, ptr %{pfx}.sub)", d.0).unwrap();
                                writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                writeln!(out, "  br label %{pfx}.check").unwrap();
                                writeln!(out, "{pfx}.done:").unwrap();
                                *current_label = format!("{pfx}.done");
                                return;
                            }
                            _ => {} // Fall through for unsupported methods
                        }
                    }

                    // ── Generic callable dispatch for named-function callables ──
                    // When resolve_closure_call_fn returns None but the closure arg is Ptr,
                    // the callable is a [fn_ptr, env_ptr] pair created by FuncAddr.
                    // Trace back to the adapter function to get the return type and params.
                    if closure_info.is_none() {
                        if let Some(ca) = closure_arg {
                            let ca_ty = val_types.get(ca.0 as usize).and_then(|t| t.as_ref());
                            if matches!(ca_ty, Some(LirType::Ptr)) {
                                // Resolve adapter function via FuncAddr trace
                                let adapt_fid = trace_funcaddr(func, ca.0);
                                let adapt_fn = adapt_fid.and_then(|fid| module.functions.get(fid.0 as usize));
                                // Determine return type: use adapter if available, else method default
                                let (gen_ret_ty, gen_ret_sret, gen_ret_small_agg) = match adapt_fn {
                                    Some(f) if f.return_type != LirType::Void => {
                                        let sret = needs_sret(&f.return_type, &module.structs);
                                        let small_agg = f.return_type.is_aggregate() && is_small_aggregate(&f.return_type, &module.structs);
                                        (f.return_type.clone(), sret, small_agg)
                                    }
                                    _ => {
                                        // Default based on method
                                        let ret = match method {
                                            "each" => LirType::Void,
                                            "any" | "all" | "filter" | "find" => LirType::Bool,
                                            "count" | "find_index" => LirType::I64,
                                            "flat_map" | "map" => {
                                                // These need GorgetArray return (sret)
                                                // Find GorgetArray struct id
                                                if let Some(i) = module.structs.iter().position(|s| s.name == "GorgetArray") {
                                                    LirType::Struct(StructId(i as u32))
                                                } else { LirType::Ptr }
                                            }
                                            "fold" | "reduce" => LirType::I64, // best guess
                                            _ => LirType::I64,
                                        };
                                        let sret = needs_sret(&ret, &module.structs);
                                        let small_agg = ret.is_aggregate() && is_small_aggregate(&ret, &module.structs);
                                        (ret, sret, small_agg)
                                    }
                                };
                                let gen_ret_llvm = llvm_type_full(&gen_ret_ty, snames);
                                // Determine element passing convention
                                let elem_pass_by_ptr_gen = elem_is_aggregate && elem_size > 16;
                                // Load fn_ptr and env_ptr from the callable [fn_ptr, env_ptr]
                                writeln!(out, "  %{pfx}.gfnp = load ptr, ptr %v{}", ca.0).unwrap();
                                writeln!(out, "  %{pfx}.genvgep = getelementptr ptr, ptr %v{}, i32 1", ca.0).unwrap();
                                writeln!(out, "  %{pfx}.genv = load ptr, ptr %{pfx}.genvgep").unwrap();

                                match method {
                                    "each" => {
                                        writeln!(out, "  br label %{pfx}.check").unwrap();
                                        writeln!(out, "{pfx}.check:").unwrap();
                                        writeln!(out, "  %{pfx}.i = phi i64 [0, %{current_label}], [%{pfx}.next, %{pfx}.body]").unwrap();
                                        writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                        writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                                        writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len").unwrap();
                                        writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                        writeln!(out, "{pfx}.body:").unwrap();
                                        writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                        writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                        writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                        if elem_pass_by_ptr_gen {
                                            writeln!(out, "  call void %{pfx}.gfnp(ptr %{pfx}.genv, ptr %{pfx}.ep)").unwrap();
                                        } else {
                                            writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                            writeln!(out, "  call void %{pfx}.gfnp(ptr %{pfx}.genv, {elem_llvm_ty} %{pfx}.elem)").unwrap();
                                        }
                                        writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                        writeln!(out, "  br label %{pfx}.check").unwrap();
                                        writeln!(out, "{pfx}.done:").unwrap();
                                        *current_label = format!("{pfx}.done");
                                        return;
                                    }
                                    "any" | "all" => {
                                        let d = dst.unwrap();
                                        let (init_val, early_val, final_val, pred_cmp) = if method == "any" {
                                            ("false", "true", "false", "eq")
                                        } else {
                                            ("true", "false", "true", "ne")
                                        };
                                        writeln!(out, "  br label %{pfx}.check").unwrap();
                                        writeln!(out, "{pfx}.check:").unwrap();
                                        writeln!(out, "  %{pfx}.i = phi i64 [0, %{current_label}], [%{pfx}.next, %{pfx}.body]").unwrap();
                                        writeln!(out, "  %{pfx}.acc = phi i1 [{init_val}, %{current_label}], [%{pfx}.accnew, %{pfx}.body]").unwrap();
                                        writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                        writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                                        writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len").unwrap();
                                        writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                        writeln!(out, "{pfx}.body:").unwrap();
                                        writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                        writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                        writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                        if elem_pass_by_ptr_gen {
                                            writeln!(out, "  %{pfx}.pred = call i1 %{pfx}.gfnp(ptr %{pfx}.genv, ptr %{pfx}.ep)").unwrap();
                                        } else {
                                            writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                            writeln!(out, "  %{pfx}.pred = call i1 %{pfx}.gfnp(ptr %{pfx}.genv, {elem_llvm_ty} %{pfx}.elem)").unwrap();
                                        }
                                        writeln!(out, "  %{pfx}.short = icmp {pred_cmp} i1 %{pfx}.pred, {early_val}").unwrap();
                                        writeln!(out, "  %{pfx}.accnew = select i1 %{pfx}.short, i1 {early_val}, i1 %{pfx}.acc").unwrap();
                                        writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                        writeln!(out, "  br label %{pfx}.check").unwrap();
                                        writeln!(out, "{pfx}.done:").unwrap();
                                        // Extend to i64 to match C ABI bool convention (same as typed any/all)
                                        writeln!(out, "  %{pfx}.b = phi i1 [%{pfx}.acc, %{pfx}.check]").unwrap();
                                        writeln!(out, "  %v{} = zext i1 %{pfx}.b to i64", d.0).unwrap();
                                        *current_label = format!("{pfx}.done");
                                        return;
                                    }
                                    "filter" => {
                                        let d = dst.unwrap();
                                        writeln!(out, "  %v{} = alloca %GorgetArray", d.0).unwrap();
                                        writeln!(out, "  call void @gorget_array_new(ptr sret(%GorgetArray) %v{}, i64 {elem_size})", d.0).unwrap();
                                        writeln!(out, "  br label %{pfx}.check").unwrap();
                                        writeln!(out, "{pfx}.check:").unwrap();
                                        // phi backedge: %pfx.skip is the actual loop back-edge predecessor
                                        writeln!(out, "  %{pfx}.i = phi i64 [0, %{current_label}], [%{pfx}.next, %{pfx}.skip]").unwrap();
                                        writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                        writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                                        writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len").unwrap();
                                        writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                        writeln!(out, "{pfx}.body:").unwrap();
                                        writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                        writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                        writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                        if elem_pass_by_ptr_gen {
                                            writeln!(out, "  %{pfx}.pred = call i1 %{pfx}.gfnp(ptr %{pfx}.genv, ptr %{pfx}.ep)").unwrap();
                                        } else {
                                            writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                            writeln!(out, "  %{pfx}.pred = call i1 %{pfx}.gfnp(ptr %{pfx}.genv, {elem_llvm_ty} %{pfx}.elem)").unwrap();
                                        };
                                        writeln!(out, "  br i1 %{pfx}.pred, label %{pfx}.push, label %{pfx}.skip").unwrap();
                                        writeln!(out, "{pfx}.push:").unwrap();
                                        writeln!(out, "  call void @gorget_array_push(ptr %v{}, ptr %{pfx}.ep)", d.0).unwrap();
                                        writeln!(out, "  br label %{pfx}.skip").unwrap();
                                        writeln!(out, "{pfx}.skip:").unwrap();
                                        writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                        writeln!(out, "  br label %{pfx}.check").unwrap();
                                        writeln!(out, "{pfx}.done:").unwrap();
                                        *current_label = format!("{pfx}.done");
                                        return;
                                    }
                                    "map" => {
                                        let d = dst.unwrap();
                                        // For map with generic callable, use adapter return type
                                        let (map_ret_llvm, map_elem_size, map_sret, map_small_agg) = if gen_ret_sret {
                                            (gen_ret_llvm.clone(), sizeof_lir_type(&gen_ret_ty, &module.structs, snames), true, false)
                                        } else if gen_ret_small_agg {
                                            (gen_ret_llvm.clone(), sizeof_lir_type(&gen_ret_ty, &module.structs, snames), false, true)
                                        } else {
                                            let sz = sizeof_lir_type(&gen_ret_ty, &module.structs, snames);
                                            (gen_ret_llvm.clone(), sz, false, false)
                                        };
                                        writeln!(out, "  %v{} = alloca %GorgetArray", d.0).unwrap();
                                        writeln!(out, "  call void @gorget_array_new(ptr sret(%GorgetArray) %v{}, i64 {map_elem_size})", d.0).unwrap();
                                        writeln!(out, "  br label %{pfx}.check").unwrap();
                                        writeln!(out, "{pfx}.check:").unwrap();
                                        writeln!(out, "  %{pfx}.i = phi i64 [0, %{current_label}], [%{pfx}.next, %{pfx}.body]").unwrap();
                                        writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                        writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                                        writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len").unwrap();
                                        writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                        writeln!(out, "{pfx}.body:").unwrap();
                                        writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                        writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                        writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                        if map_sret {
                                            writeln!(out, "  %{pfx}.tmp = alloca {map_ret_llvm}").unwrap();
                                            if elem_pass_by_ptr_gen {
                                                writeln!(out, "  call void %{pfx}.gfnp(ptr sret({map_ret_llvm}) %{pfx}.tmp, ptr %{pfx}.genv, ptr %{pfx}.ep)").unwrap();
                                            } else {
                                                writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                                writeln!(out, "  call void %{pfx}.gfnp(ptr sret({map_ret_llvm}) %{pfx}.tmp, ptr %{pfx}.genv, {elem_llvm_ty} %{pfx}.elem)").unwrap();
                                            }
                                            writeln!(out, "  call void @gorget_array_push(ptr %v{}, ptr %{pfx}.tmp)", d.0).unwrap();
                                        } else if map_small_agg {
                                            if elem_pass_by_ptr_gen {
                                                writeln!(out, "  %{pfx}.out.v = call {map_ret_llvm} %{pfx}.gfnp(ptr %{pfx}.genv, ptr %{pfx}.ep)").unwrap();
                                            } else {
                                                writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                                writeln!(out, "  %{pfx}.out.v = call {map_ret_llvm} %{pfx}.gfnp(ptr %{pfx}.genv, {elem_llvm_ty} %{pfx}.elem)").unwrap();
                                            }
                                            writeln!(out, "  %{pfx}.tmp = alloca {map_ret_llvm}").unwrap();
                                            writeln!(out, "  store {map_ret_llvm} %{pfx}.out.v, ptr %{pfx}.tmp").unwrap();
                                            writeln!(out, "  call void @gorget_array_push(ptr %v{}, ptr %{pfx}.tmp)", d.0).unwrap();
                                        } else {
                                            if elem_pass_by_ptr_gen {
                                                writeln!(out, "  %{pfx}.out = call {map_ret_llvm} %{pfx}.gfnp(ptr %{pfx}.genv, ptr %{pfx}.ep)").unwrap();
                                            } else {
                                                writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                                writeln!(out, "  %{pfx}.out = call {map_ret_llvm} %{pfx}.gfnp(ptr %{pfx}.genv, {elem_llvm_ty} %{pfx}.elem)").unwrap();
                                            }
                                            writeln!(out, "  %{pfx}.tmp = alloca {map_ret_llvm}").unwrap();
                                            writeln!(out, "  store {map_ret_llvm} %{pfx}.out, ptr %{pfx}.tmp").unwrap();
                                            writeln!(out, "  call void @gorget_array_push(ptr %v{}, ptr %{pfx}.tmp)", d.0).unwrap();
                                        }
                                        writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                        writeln!(out, "  br label %{pfx}.check").unwrap();
                                        writeln!(out, "{pfx}.done:").unwrap();
                                        *current_label = format!("{pfx}.done");
                                        return;
                                    }
                                    "flat_map" => {
                                        let d = dst.unwrap();
                                        writeln!(out, "  %v{} = alloca %GorgetArray", d.0).unwrap();
                                        writeln!(out, "  call void @gorget_array_new(ptr sret(%GorgetArray) %v{}, i64 {elem_size})", d.0).unwrap();
                                        writeln!(out, "  br label %{pfx}.check").unwrap();
                                        writeln!(out, "{pfx}.check:").unwrap();
                                        writeln!(out, "  %{pfx}.i = phi i64 [0, %{current_label}], [%{pfx}.next, %{pfx}.body]").unwrap();
                                        writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                        writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                                        writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len").unwrap();
                                        writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                        writeln!(out, "{pfx}.body:").unwrap();
                                        writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                        writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                        writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                        writeln!(out, "  %{pfx}.sub = alloca %GorgetArray").unwrap();
                                        if elem_pass_by_ptr_gen {
                                            writeln!(out, "  call void %{pfx}.gfnp(ptr sret(%GorgetArray) %{pfx}.sub, ptr %{pfx}.genv, ptr %{pfx}.ep)").unwrap();
                                        } else {
                                            writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                            writeln!(out, "  call void %{pfx}.gfnp(ptr sret(%GorgetArray) %{pfx}.sub, ptr %{pfx}.genv, {elem_llvm_ty} %{pfx}.elem)").unwrap();
                                        }
                                        writeln!(out, "  call void @gorget_array_extend(ptr %v{}, ptr %{pfx}.sub)", d.0).unwrap();
                                        writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                        writeln!(out, "  br label %{pfx}.check").unwrap();
                                        writeln!(out, "{pfx}.done:").unwrap();
                                        *current_label = format!("{pfx}.done");
                                        return;
                                    }
                                    "fold" => {
                                        if args.len() >= 3 {
                                            let d = dst.unwrap();
                                            let acc_arg = args[1];
                                            let acc_ty = val_types.get(acc_arg.0 as usize).and_then(|t| t.as_ref())
                                                .cloned().unwrap_or(LirType::I64);
                                            let acc_llvm = llvm_arg_type(&acc_ty, snames);
                                            let acc_is_agg = acc_llvm.starts_with('%');
                                            let fold_ret_sret = needs_sret(&gen_ret_ty, &module.structs);
                                            let fold_ret_small = gen_ret_ty.is_aggregate() && is_small_aggregate(&gen_ret_ty, &module.structs);
                                            writeln!(out, "  br label %{pfx}.check").unwrap();
                                            writeln!(out, "{pfx}.check:").unwrap();
                                            writeln!(out, "  %{pfx}.i = phi i64 [0, %{current_label}], [%{pfx}.next, %{pfx}.body]").unwrap();
                                            // phi value operands must NOT include type — type is the phi's own declared type
                                            if acc_is_agg {
                                                writeln!(out, "  %{pfx}.acc = phi ptr [%v{}, %{current_label}], [%{pfx}.accnew, %{pfx}.body]", acc_arg.0).unwrap();
                                            } else {
                                                writeln!(out, "  %{pfx}.acc = phi {acc_llvm} [%v{}, %{current_label}], [%{pfx}.accnew, %{pfx}.body]", acc_arg.0).unwrap();
                                            }
                                            writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                            writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                                            writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len").unwrap();
                                            writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                            writeln!(out, "{pfx}.body:").unwrap();
                                            writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                            writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                            writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                            let acc_param = if acc_is_agg { format!("ptr %{pfx}.acc") } else { format!("{acc_llvm} %{pfx}.acc") };
                                            let elem_param = if elem_pass_by_ptr_gen { format!("ptr %{pfx}.ep") } else {
                                                writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                                format!("{elem_llvm_ty} %{pfx}.elem")
                                            };
                                            if fold_ret_sret {
                                                writeln!(out, "  %{pfx}.accnew = alloca {gen_ret_llvm}").unwrap();
                                                writeln!(out, "  call void %{pfx}.gfnp(ptr sret({gen_ret_llvm}) %{pfx}.accnew, ptr %{pfx}.genv, {acc_param}, {elem_param})").unwrap();
                                            } else if fold_ret_small {
                                                writeln!(out, "  %{pfx}.accret = call {gen_ret_llvm} %{pfx}.gfnp(ptr %{pfx}.genv, {acc_param}, {elem_param})").unwrap();
                                                writeln!(out, "  %{pfx}.accnew = alloca {gen_ret_llvm}").unwrap();
                                                writeln!(out, "  store {gen_ret_llvm} %{pfx}.accret, ptr %{pfx}.accnew").unwrap();
                                            } else {
                                                writeln!(out, "  %{pfx}.accnew = call {acc_llvm} %{pfx}.gfnp(ptr %{pfx}.genv, {acc_param}, {elem_param})").unwrap();
                                            }
                                            writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                            writeln!(out, "  br label %{pfx}.check").unwrap();
                                            writeln!(out, "{pfx}.done:").unwrap();
                                            if acc_is_agg || fold_ret_sret || fold_ret_small {
                                                writeln!(out, "  %v{} = phi ptr [%{pfx}.acc, %{pfx}.check]", d.0).unwrap();
                                            } else {
                                                writeln!(out, "  %v{} = phi {acc_llvm} [%{pfx}.acc, %{pfx}.check]", d.0).unwrap();
                                            }
                                            *current_label = format!("{pfx}.done");
                                            return;
                                        }
                                    }
                                    "reduce" => {
                                        let d = dst.unwrap();
                                        let ret_llvm = gen_ret_llvm.clone();
                                        writeln!(out, "  %{pfx}.datap0 = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                        writeln!(out, "  %{pfx}.lenp0 = getelementptr %GorgetArray, ptr %v{}, i32 0, i32 2", arr_arg.0).unwrap();
                                        writeln!(out, "  %{pfx}.len0 = load i64, ptr %{pfx}.lenp0").unwrap();
                                        writeln!(out, "  %{pfx}.ep0 = getelementptr i8, ptr %{pfx}.datap0, i64 0").unwrap();
                                        writeln!(out, "  %{pfx}.init = load {elem_llvm_ty}, ptr %{pfx}.ep0").unwrap();
                                        writeln!(out, "  br label %{pfx}.check").unwrap();
                                        writeln!(out, "{pfx}.check:").unwrap();
                                        writeln!(out, "  %{pfx}.i = phi i64 [1, %{current_label}], [%{pfx}.next, %{pfx}.body]").unwrap();
                                        writeln!(out, "  %{pfx}.acc = phi {elem_llvm_ty} [%{pfx}.init, %{current_label}], [%{pfx}.accnew, %{pfx}.body]").unwrap();
                                        writeln!(out, "  %{pfx}.cmp = icmp ult i64 %{pfx}.i, %{pfx}.len0").unwrap();
                                        writeln!(out, "  br i1 %{pfx}.cmp, label %{pfx}.body, label %{pfx}.done").unwrap();
                                        writeln!(out, "{pfx}.body:").unwrap();
                                        writeln!(out, "  %{pfx}.datap = load ptr, ptr %v{}", arr_arg.0).unwrap();
                                        writeln!(out, "  %{pfx}.offset = mul i64 %{pfx}.i, {elem_size}").unwrap();
                                        writeln!(out, "  %{pfx}.ep = getelementptr i8, ptr %{pfx}.datap, i64 %{pfx}.offset").unwrap();
                                        writeln!(out, "  %{pfx}.elem = load {elem_llvm_ty}, ptr %{pfx}.ep").unwrap();
                                        writeln!(out, "  %{pfx}.accnew = call {ret_llvm} %{pfx}.gfnp(ptr %{pfx}.genv, {elem_llvm_ty} %{pfx}.acc, {elem_llvm_ty} %{pfx}.elem)").unwrap();
                                        writeln!(out, "  %{pfx}.next = add i64 %{pfx}.i, 1").unwrap();
                                        writeln!(out, "  br label %{pfx}.check").unwrap();
                                        writeln!(out, "{pfx}.done:").unwrap();
                                        writeln!(out, "  %v{} = phi {ret_llvm} [%{pfx}.acc, %{pfx}.check]", d.0).unwrap();
                                        *current_label = format!("{pfx}.done");
                                        return;
                                    }
                                    _ => {} // Fall through for unsupported methods
                                }
                            }
                        }
                    }
                    // If closure resolution failed or method not handled, fall through
                }
            }

            // ── gorget_str_push — type-dispatch to push_int/push_float/push_bool ──
            if (name == "gorget_str_push" || name == "gorget_str_push_line") && args.len() == 2 {
                let arg2_ty = val_types.get(args[1].0 as usize).and_then(|t| t.as_ref());
                let is_push_line = name == "gorget_str_push_line";
                let variant = match arg2_ty {
                    Some(LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64
                         | LirType::U8 | LirType::U16 | LirType::U32 | LirType::U64) =>
                        if is_push_line { Some("gorget_string_push_line_int") }
                        else { Some("gorget_string_push_int") },
                    Some(LirType::F32 | LirType::F64) =>
                        if is_push_line { Some("gorget_string_push_line_float") }
                        else { Some("gorget_string_push_float") },
                    Some(LirType::Bool) =>
                        if is_push_line { Some("gorget_string_push_line_bool") }
                        else { Some("gorget_string_push_bool") },
                    _ => None, // Str — use gorget_str_push/gorget_string_push_char
                };
                if let Some(typed_fn) = variant {
                    let arg1_ty = val_types.get(args[1].0 as usize)
                        .and_then(|t| t.as_ref())
                        .map(|t| llvm_arg_type(t, snames))
                        .unwrap_or_else(|| "i64".to_string());
                    writeln!(out, "  call void @{typed_fn}(ptr %v{}, {arg1_ty} %v{})", args[0].0, args[1].0).unwrap();
                    return;
                }
                // Str arg: call gorget_string_push_char / gorget_string_push_line
                let arg2_is_str = arg2_ty.map_or(false, |t| t.is_ptr() || matches!(t, LirType::Struct(_) | LirType::PtrTo(_)));
                if arg2_is_str {
                    if is_push_line {
                        // gorget_string_push_line takes (GorgetString*, const char*).
                        // The 2nd arg is a GorgetString* — extract the .data pointer (field 0).
                        let uid = *trap_counter; *trap_counter += 1;
                        writeln!(out, "  %psl.data.{uid} = load ptr, ptr %v{}", args[1].0).unwrap();
                        writeln!(out, "  call void @gorget_string_push_line(ptr %v{}, ptr %psl.data.{uid})", args[0].0).unwrap();
                    } else {
                        // gorget_string_push_char takes (GorgetString*, Str c) — Str is 32 bytes,
                        // so on aarch64 it's passed by hidden pointer. Pass GorgetString* directly.
                        writeln!(out, "  call void @gorget_string_push_char(ptr %v{}, ptr %v{})", args[0].0, args[1].0).unwrap();
                    }
                    return;
                }
            }

            // ── gorget_file_open (1-arg) → __gorget_file_open_r ─────────────────
            // LIR uses 1-arg gorget_file_open for "open for reading".
            // The real C function takes 2 args; __gorget_file_open_r is a wrapper adding "r".
            if name == "gorget_file_open" && args.len() == 1 {
                *trap_counter += 1; // must match pre-pass 'else { counter += 1; }'
                // args[0] is a GorgetString pointer (PtrTo GorgetString from StrLit/SlotAddr).
                // __gorget_file_open_r expects const char* — extract the data field.
                // The CStr ABI tag in the LIR extern marks this arg as needing cstr extraction.
                // We emit a load of the .data ptr (offset 0) from the GorgetString, then call
                // gorget_str_to_cstr for null-termination safety.
                let arg0 = args[0].0;
                let cstr_name = format!("fopenr.{block_id}.{}", *trap_counter - 1);
                // Use gorget_str_to_cstr: ensures null termination even for views.
                writeln!(out, "  %{cstr_name} = call ptr @gorget_str_to_cstr(ptr %v{arg0})").unwrap();
                if let Some(d) = dst {
                    // Use the extern's declared return type (not val_types — val_types[d]
                    // may be PtrTo(File) from an earlier SlotAddr instruction that was
                    // computed before the call, preventing the correct Struct(File) type).
                    let ext_ret = module.externs.iter()
                        .find(|e| e.name == "gorget_file_open")
                        .map(|e| &e.return_type);
                    if let Some(ret_ty) = ext_ret {
                        if is_small_aggregate(ret_ty, &module.structs) {
                            let agg_ty = llvm_type_full(ret_ty, snames);
                            writeln!(out, "  %v{}.ret = call {agg_ty} @__gorget_file_open_r(ptr %{cstr_name})", d.0).unwrap();
                            writeln!(out, "  %v{} = alloca {agg_ty}", d.0).unwrap();
                            writeln!(out, "  store {agg_ty} %v{}.ret, ptr %v{}", d.0, d.0).unwrap();
                            return;
                        }
                    }
                    writeln!(out, "  %v{}.ret = call ptr @__gorget_file_open_r(ptr %{cstr_name})", d.0).unwrap();
                    writeln!(out, "  %v{} = alloca ptr", d.0).unwrap();
                    writeln!(out, "  store ptr %v{}.ret, ptr %v{}", d.0, d.0).unwrap();
                } else {
                    writeln!(out, "  call void @__gorget_file_open_r(ptr %{cstr_name})").unwrap();
                }
                return;
            }

            // ── gorget_task_group_submit ──────────────────────────────────────────
            // gorget_task_group_submit is a C macro, not a real function.
            // Expand inline: extract __task (offset 0) and __drop (offset 8) from Task struct,
            // load the actual TaskGroup pointer, then call gorget_task_group_submit_raw.
            // Finally zero __task to prevent double-free (matches macro: task.__task = NULL).
            if name == "gorget_task_group_submit" && args.len() >= 2 {
                let uid = *trap_counter; *trap_counter += 1;
                let pfx = format!("tgs.{block_id}.{uid}");
                // args[0] = ptr to TaskGroup storage (TaskGroup is a ptr typedef, load it)
                // args[1] = ptr to Task struct storage ({void*__task, void(*__drop)(void*)})
                let tg = args[0].0;
                let task = args[1].0;
                writeln!(out, "  %{pfx}.tg = load ptr, ptr %v{tg}").unwrap();
                writeln!(out, "  %{pfx}.ctx = load ptr, ptr %v{task}").unwrap();
                writeln!(out, "  %{pfx}.dropgep = getelementptr i8, ptr %v{task}, i64 8").unwrap();
                writeln!(out, "  %{pfx}.drop = load ptr, ptr %{pfx}.dropgep").unwrap();
                writeln!(out, "  call void @gorget_task_group_submit_raw(ptr %{pfx}.tg, ptr %{pfx}.ctx, ptr %{pfx}.drop)").unwrap();
                writeln!(out, "  store ptr null, ptr %v{task}").unwrap();
                return;
            }

            // gorget_str_default / String.default() — returns empty GorgetString (all zeros).
            // The C runtime inline version is not exported; generate inline instead.
            if name == "gorget_str_default" {
                *trap_counter += 1; // must match pre-pass 'else { counter += 1; }'
                if let Some(d) = dst {
                    writeln!(out, "  %v{} = alloca %GorgetString", d.0).unwrap();
                    writeln!(out, "  call ptr @memset(ptr %v{}, i32 0, i64 32)", d.0).unwrap();
                }
                return;
            }

            // gorget_str_str(GorgetString*) → clone string builder contents.
            // LIR extern arity is wrong (says 2 Str args, only 1 is passed).
            // Redirect to gorget_string_clone_to_owned which has the correct signature.
            if name == "gorget_str_str" && !args.is_empty() {
                *trap_counter += 1; // must match pre-pass 'else { counter += 1; }'
                if let Some(d) = dst {
                    writeln!(out, "  %v{} = alloca %GorgetString", d.0).unwrap();
                    writeln!(out, "  call void @gorget_string_clone_to_owned(ptr sret(%GorgetString) %v{}, ptr %v{})", d.0, args[0].0).unwrap();
                }
                return;
            }

            // gorget_string_push_line(GorgetString* s, const char* rhs) — rhs is const char*,
            // but LIR passes a GorgetString* (ptr to struct). Extract .data (field 0) first.
            if name == "gorget_string_push_line" && args.len() >= 2 {
                let uid = *trap_counter; *trap_counter += 1;
                let sb = args[0].0;
                let str_arg = args[1].0;
                writeln!(out, "  %psl.data.{uid} = load ptr, ptr %v{str_arg}").unwrap();
                writeln!(out, "  call void @gorget_string_push_line(ptr %v{sb}, ptr %psl.data.{uid})").unwrap();
                return;
            }

            // gorget_str_clear(GorgetString*) — static inline in C backend only, not in runtime.
            // Equivalent: s->len = 0; if (s->cap > 0 && s->data) ((char*)s->data)[0] = '\0';
            // GorgetString layout: { ptr data @0, i64 cap @8, i64 len @16, ptr alloc @24 }
            if name == "gorget_str_clear" && !args.is_empty() {
                let uid = *trap_counter; *trap_counter += 1;
                let sb = args[0].0;
                // Zero the len field (offset 16)
                writeln!(out, "  %scl.len.{uid} = getelementptr %GorgetString, ptr %v{sb}, i32 0, i32 2").unwrap();
                writeln!(out, "  store i64 0, ptr %scl.len.{uid}").unwrap();
                // Load cap (offset 8) and data (offset 0) to conditionally zero first byte
                writeln!(out, "  %scl.cap.{uid} = getelementptr %GorgetString, ptr %v{sb}, i32 0, i32 1").unwrap();
                writeln!(out, "  %scl.capv.{uid} = load i64, ptr %scl.cap.{uid}").unwrap();
                writeln!(out, "  %scl.hascap.{uid} = icmp sgt i64 %scl.capv.{uid}, 0").unwrap();
                writeln!(out, "  %scl.dp.{uid} = getelementptr %GorgetString, ptr %v{sb}, i32 0, i32 0").unwrap();
                writeln!(out, "  %scl.datav.{uid} = load ptr, ptr %scl.dp.{uid}").unwrap();
                writeln!(out, "  %scl.notnull.{uid} = icmp ne ptr %scl.datav.{uid}, null").unwrap();
                writeln!(out, "  %scl.cond.{uid} = and i1 %scl.hascap.{uid}, %scl.notnull.{uid}").unwrap();
                writeln!(out, "  br i1 %scl.cond.{uid}, label %scl.zero.{uid}, label %scl.done.{uid}").unwrap();
                writeln!(out, "scl.zero.{uid}:").unwrap();
                writeln!(out, "  store i8 0, ptr %scl.datav.{uid}").unwrap();
                writeln!(out, "  br label %scl.done.{uid}").unwrap();
                writeln!(out, "scl.done.{uid}:").unwrap();
                return;
            }

            // gorget_bool_to_str returns GorgetString via sret
            if name == "gorget_bool_to_str" && !args.is_empty() {
                // Always increment trap_counter (must match pre-pass which always increments)
                let uid = *trap_counter; *trap_counter += 1;
                if let Some(d) = dst {
                    let arg_ty = val_types.get(args[0].0 as usize).and_then(|t| t.as_ref());
                    let is_bool = matches!(arg_ty, Some(LirType::Bool));
                    writeln!(out, "  %v{} = alloca %GorgetString", d.0).unwrap();
                    if is_bool {
                        writeln!(out, "  call void @gorget_bool_to_str(ptr sret(%GorgetString) %v{}, i1 %v{})", d.0, args[0].0).unwrap();
                    } else {
                        // Truncate i64/i32 → i1 for bool arg
                        writeln!(out, "  %bts.{uid} = trunc i64 %v{} to i1", args[0].0).unwrap();
                        writeln!(out, "  call void @gorget_bool_to_str(ptr sret(%GorgetString) %v{}, i1 %bts.{uid})", d.0).unwrap();
                    }
                }
                return;
            }

            // ── gorget_map_get / gorget_map_safe_get — wrap result in Option ──
            // C runtime returns void* (NULL if missing, ptr-to-value if found).
            // Detect Option wrapping by scanning ahead: if the next Memcpy from this
            // result uses size > 8, the downstream code expects an Option struct.
            if (name == "gorget_map_get" || name == "gorget_map_safe_get") && !args.is_empty() {
                if let Some(d) = dst {
                    // Detect Option wrapping: scan for SlotStore of this result into
                    // a slot whose type is an Option struct (name starts with "Option__").
                    let needs_option_wrap = {
                        let block = &func.blocks[block_id as usize];
                        block.insts.iter().any(|next_inst| {
                            if let Inst::SlotStore { slot, value, .. } = next_inst {
                                if value.0 == d.0 {
                                    let slot_ty = &func.slots[slot.0 as usize].ty;
                                    if let LirType::Struct(sid) = slot_ty {
                                        return module.structs.get(sid.0 as usize)
                                            .map_or(false, |s| s.name.starts_with("Option__") || s.name.starts_with("Result__"));
                                    }
                                }
                            }
                            false
                        })
                    };
                    if !needs_option_wrap {
                        // dict[key] direct access — fall through to generic handler
                    } else {
                    let uid = *trap_counter;
                    *trap_counter += 1;
                    let pfx = format!("mapget.{block_id}.{uid}");
                    // Find the Option struct by scanning slots
                    let opt_struct_id = func.blocks[block_id as usize].insts.iter().find_map(|i| {
                        if let Inst::SlotStore { slot, value, .. } = i {
                            if value.0 == d.0 {
                                if let LirType::Struct(sid) = &func.slots[slot.0 as usize].ty {
                                    return Some(*sid);
                                }
                            }
                        }
                        None
                    });
                    let dst_ty: Option<&LirType> = opt_struct_id.map(|sid| &func.slots.iter()
                        .find(|s| matches!(&s.ty, LirType::Struct(s2) if *s2 == sid))
                        .unwrap().ty);
                    // Determine payload size and offset from the destination Option struct
                    let (payload_size, payload_off) = match dst_ty {
                        Some(LirType::PtrTo(sid)) | Some(LirType::Struct(sid)) => {
                            let sdef = &module.structs[sid.0 as usize];
                            sdef.fields.get(1).map(|(_, fty)| {
                                let sz = match fty {
                                    LirType::I64 | LirType::U64 | LirType::F64 | LirType::Ptr | LirType::PtrTo(_) => 8usize,
                                    LirType::I32 | LirType::U32 | LirType::F32 => 4,
                                    LirType::I16 | LirType::U16 => 2,
                                    LirType::I8 | LirType::U8 | LirType::Bool => 1,
                                    LirType::Struct(inner) => module.structs[inner.0 as usize].computed_c_size.unwrap_or(64),
                                    _ => 8,
                                };
                                let off = lir_payload_offset(fty) as usize;
                                (sz, off)
                            }).unwrap_or((8, 8))
                        }
                        _ => (8, 8),
                    };
                    // Build args — spill scalars to alloca (gorget_map_get takes ptr to key)
                    let mut spills = Vec::new();
                    let arg_strs: Vec<String> = args.iter().enumerate().map(|(i, a)| {
                        let aty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                        if aty.map_or(false, |t| t.is_ptr()) {
                            format!("ptr %v{}", a.0)
                        } else {
                            // Spill scalar to alloca
                            let alty = aty.map(|t| llvm_arg_type(t, snames)).unwrap_or_else(|| "i64".to_string());
                            let spill_name = format!("{pfx}.spill.{i}");
                            spills.push(format!("  %{spill_name} = alloca {alty}"));
                            spills.push(format!("  store {alty} %v{}, ptr %{spill_name}", a.0));
                            format!("ptr %{spill_name}")
                        }
                    }).collect();
                    for s in &spills {
                        writeln!(out, "{s}").unwrap();
                    }
                    // Call gorget_map_get → ptr (or NULL)
                    writeln!(out, "  %{pfx}.raw = call ptr @{name}({})", arg_strs.join(", ")).unwrap();
                    writeln!(out, "  %{pfx}.isnull = icmp eq ptr %{pfx}.raw, null").unwrap();
                    // Branchless Option construction (avoids splitting basic blocks,
                    // which would break phi node predecessor lists).
                    // Alloca must hold tag (4) + padding + payload at the correct alignment offset.
                    let alloca_size = payload_off + payload_size;
                    writeln!(out, "  %v{} = alloca i8, i64 {alloca_size}", d.0).unwrap();
                    // tag: 0 for Some, 1 for None
                    writeln!(out, "  %{pfx}.tag = select i1 %{pfx}.isnull, i32 1, i32 0").unwrap();
                    writeln!(out, "  store i32 %{pfx}.tag, ptr %v{}", d.0).unwrap();
                    // payload: copy from raw ptr if non-null, else copy from Option alloca
                    // itself (harmless self-read since tag=1/None means payload is ignored).
                    writeln!(out, "  %{pfx}.payptr = getelementptr i8, ptr %v{}, i64 {payload_off}", d.0).unwrap();
                    writeln!(out, "  %{pfx}.src = select i1 %{pfx}.isnull, ptr %{pfx}.payptr, ptr %{pfx}.raw").unwrap();
                    writeln!(out, "  call ptr @memcpy(ptr %{pfx}.payptr, ptr %{pfx}.src, i64 {payload_size})").unwrap();
                    return;
                    } // else (is_option_dst)
                }
            }

            // ── Collection void* return dereference ──
            // ── Box__T__get → dereference void* to the boxed value ──
            if name.contains("__get") && name.starts_with("Box__") && !args.is_empty() {
                if let Some(d) = dst {
                    let uid = *trap_counter;
                    *trap_counter += 1;
                    let pfx = format!("boxget.{block_id}.{uid}");
                    // Box__T__get returns void* → load the scalar
                    let arg_strs: Vec<String> = args.iter().map(|a| format!("ptr %v{}", a.0)).collect();
                    writeln!(out, "  %{pfx}.raw = call ptr @{name}({})", arg_strs.join(", ")).unwrap();
                    // Determine the element type from the return type context
                    let elem_ty = if name.contains("int64") || name.contains("int__") { "i64" }
                        else if name.contains("double") || name.contains("float__") { "double" }
                        else { "i64" }; // default
                    writeln!(out, "  %v{} = load {elem_ty}, ptr %{pfx}.raw", d.0).unwrap();
                }
                return;
            }

            // Functions like gorget_guard_get, gorget_shared_get etc. return void*.
            // The type inference pre-pass determined the actual inner type from
            // downstream consumers. If the type is scalar, load through the void*.
            {
                let is_void_return_fn = matches!(name.as_str(),
                    "gorget_guard_get" | "gorget_shared_get"
                    | "gorget_read_guard_get" | "gorget_write_guard_get"
                );
                if is_void_return_fn {
                    if let Some(d) = dst {
                        let inferred_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                        let is_scalar = inferred_ty.map_or(false, |t| {
                            t.is_integer() || t.is_float() || matches!(t, LirType::Bool)
                        });
                        if is_scalar {
                            let uid = *trap_counter;
                            *trap_counter += 1;
                            let pfx = format!("voidret.{block_id}.{uid}");
                            let load_ty = llvm_type_full(inferred_ty.unwrap(), snames);
                            let arg_strs: Vec<String> = args.iter().map(|a| {
                                let aty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                                let ty_str = aty.map(|t| llvm_arg_type(t, snames)).unwrap_or_else(|| "ptr".to_string());
                                format!("{ty_str} %v{}", a.0)
                            }).collect();
                            writeln!(out, "  %{pfx}.raw = call ptr @{name}({})", arg_strs.join(", ")).unwrap();
                            writeln!(out, "  %v{} = load {load_ty}, ptr %{pfx}.raw", d.0).unwrap();
                            return;
                        }
                    }
                }
            }

            // Inline expansion of Option/Result helpers
            // Option/Result is_some/is_none/is_ok/is_err — tag check
            let is_tag_check = name == "__option_is_some" || name == "__option_is_none"
                || name.ends_with("__is_some") || name.ends_with("__is_none")
                || name.ends_with("__is_ok") || name.ends_with("__is_err");
            if is_tag_check && !args.is_empty() {
                let uid = *trap_counter;
                *trap_counter += 1;
                let tag_ptr = format!("opt.{block_id}.{uid}.tagptr");
                let tag_val = format!("opt.{block_id}.{uid}.tag");
                writeln!(out, "  %{tag_ptr} = getelementptr i8, ptr %v{}, i32 0", args[0].0).unwrap();
                writeln!(out, "  %{tag_val} = load i32, ptr %{tag_ptr}").unwrap();
                if let Some(d) = dst {
                    let is_positive = name.contains("is_some") || name.contains("is_ok");
                    if is_positive {
                        // is_some/is_ok: tag == 0 (Some=tag0, Ok=tag0)
                        writeln!(out, "  %v{} = icmp eq i32 %{tag_val}, 0", d.0).unwrap();
                    } else {
                        // is_none/is_err: tag != 0
                        writeln!(out, "  %v{} = icmp ne i32 %{tag_val}, 0", d.0).unwrap();
                    }
                }
                return;
            }

            // Result unwrap_error — return error payload from Result struct
            if (name == "__result_unwrap_error" || name.ends_with("__unwrap_error")
                || name.ends_with("__unwrap_err")) && !args.is_empty() {
                if let Some(d) = dst {
                    let uid = *trap_counter;
                    *trap_counter += 1;
                    // Find the Result struct and get the error field offset
                    let arg_ty = val_types.get(args[0].0 as usize).and_then(|t| t.as_ref());
                    let (err_offset, err_ty_str) = match arg_ty {
                        Some(LirType::PtrTo(sid)) | Some(LirType::Struct(sid)) => {
                            let sdef = &module.structs[sid.0 as usize];
                            // Error field is the last field (field 2+ in Result structs)
                            // Compute offset from struct layout
                            if sdef.fields.len() >= 3 {
                                let err_field_ty = &sdef.fields.last().unwrap().1;
                                let ty_str = llvm_type_full(err_field_ty, snames);
                                // Error offset: after tag(4) + padding(4) + ok_payload
                                // For Result__int64_t__GorgetString: tag(4)+pad(4)+ok(8) = 16
                                // For Result__GorgetString__GorgetString: tag(4)+pad(4)+ok(32) = 40
                                // For Result__void__int64_t: tag(4)+pad(4)+void(0?)+err = 8
                                let ok_size: usize = sdef.fields.get(1).map(|(_, fty)| match fty {
                                    LirType::I8 | LirType::U8 | LirType::Bool => 1,
                                    LirType::I16 | LirType::U16 => 2,
                                    LirType::I32 | LirType::U32 => 4,
                                    LirType::I64 | LirType::U64 | LirType::F64 | LirType::Ptr | LirType::PtrTo(_) => 8,
                                    LirType::F32 => 4,
                                    LirType::Void => 0,
                                    // Aggregate payload: compute actual size (must match LLVM struct layout)
                                    LirType::Struct(sid) => sizeof_lir_type(&LirType::Struct(*sid), &module.structs, snames),
                                }).unwrap_or(8);
                                let offset = 8 + ok_size; // tag(4) + pad to 8 + ok payload
                                // Align error field to its own alignment (typically 8)
                                let err_align = 8usize;
                                let offset = (offset + err_align - 1) & !(err_align - 1);
                                (offset, ty_str)
                            } else {
                                (8, "i64".to_string())
                            }
                        }
                        _ => (8, "i64".to_string()),
                    };
                    let pfx = format!("unwraperr.{block_id}.{uid}");
                    writeln!(out, "  %{pfx}.ptr = getelementptr i8, ptr %v{}, i64 {err_offset}", args[0].0).unwrap();
                    if err_ty_str.starts_with('%') {
                        // Aggregate payload: return pointer to inline struct (no load)
                        writeln!(out, "  %v{} = getelementptr i8, ptr %v{}, i64 {err_offset}", d.0, args[0].0).unwrap();
                    } else if err_ty_str == "ptr" {
                        writeln!(out, "  %v{} = load ptr, ptr %{pfx}.ptr", d.0).unwrap();
                    } else {
                        writeln!(out, "  %v{} = load {err_ty_str}, ptr %{pfx}.ptr", d.0).unwrap();
                    }
                }
                return;
            }

            // ── Monomorphized parse methods: int64_t__parse(Str) → Option[int64_t] ──
            // These call gorget_try_parse_int/float and wrap the result in Option.
            let is_int_parse = name.ends_with("__parse")
                && (name.starts_with("int") || name.starts_with("uint"));
            let is_float_parse = name == "double__parse" || name == "float__parse";
            if (is_int_parse || is_float_parse) && !args.is_empty() {
                if let Some(d) = dst {
                    let uid = *trap_counter;
                    *trap_counter += 1;
                    let pfx = format!("parse.{block_id}.{uid}");
                    // The Str arg has .data at field 0 and .len at field 2.
                    // gorget_try_parse_int/float takes (const char* s, size_t len).
                    writeln!(out, "  %{pfx}.datap = getelementptr %GorgetString, ptr %v{}, i32 0, i32 0", args[0].0).unwrap();
                    writeln!(out, "  %{pfx}.data = load ptr, ptr %{pfx}.datap").unwrap();
                    writeln!(out, "  %{pfx}.lenp = getelementptr %GorgetString, ptr %v{}, i32 0, i32 2", args[0].0).unwrap();
                    writeln!(out, "  %{pfx}.len = load i64, ptr %{pfx}.lenp").unwrap();
                    // Call → returns {value, ok} struct in registers.
                    // C struct has bool padded to 8 bytes → use i64 for ok field.
                    let (val_ty, ret_ty, try_fn) = if is_float_parse {
                        ("double", "{double, i64}", "gorget_try_parse_float")
                    } else {
                        ("i64", "{i64, i64}", "gorget_try_parse_int")
                    };
                    writeln!(out, "  %{pfx}.result = call {ret_ty} @{try_fn}(ptr %{pfx}.data, i64 %{pfx}.len)").unwrap();
                    writeln!(out, "  %{pfx}.val = extractvalue {ret_ty} %{pfx}.result, 0").unwrap();
                    writeln!(out, "  %{pfx}.ok_raw = extractvalue {ret_ty} %{pfx}.result, 1").unwrap();
                    writeln!(out, "  %{pfx}.parse_ok = icmp ne i64 %{pfx}.ok_raw, 0").unwrap();
                    // Narrow int types: determine target LLVM type, range, and payload offset.
                    // payload offset: 4 for ≤4-byte payloads, 8 for ≥8-byte (i64/f64).
                    // Narrow types also need range checking before accepting the parse.
                    struct NarrowInfo { llvm_ty: &'static str, pay_off: u64, min: i64, max: i64 }
                    let narrow: Option<NarrowInfo> = if !is_float_parse {
                        if name.starts_with("int8") {
                            Some(NarrowInfo { llvm_ty: "i8",  pay_off: 4, min: -128,       max: 127        })
                        } else if name.starts_with("uint8") {
                            Some(NarrowInfo { llvm_ty: "i8",  pay_off: 4, min: 0,           max: 255        })
                        } else if name.starts_with("int16") {
                            Some(NarrowInfo { llvm_ty: "i16", pay_off: 4, min: -32768,      max: 32767      })
                        } else if name.starts_with("uint16") {
                            Some(NarrowInfo { llvm_ty: "i16", pay_off: 4, min: 0,           max: 65535      })
                        } else if name.starts_with("int32") {
                            Some(NarrowInfo { llvm_ty: "i32", pay_off: 4, min: -2147483648, max: 2147483647 })
                        } else if name.starts_with("uint32") {
                            Some(NarrowInfo { llvm_ty: "i32", pay_off: 4, min: 0,           max: 4294967295 })
                        } else {
                            None // int64, uint64 — full range, offset 8
                        }
                    } else { None };
                    // Combine parse success with range check
                    if let Some(ref n) = narrow {
                        writeln!(out, "  %{pfx}.lo = icmp sge i64 %{pfx}.val, {}", n.min).unwrap();
                        writeln!(out, "  %{pfx}.hi = icmp sle i64 %{pfx}.val, {}", n.max).unwrap();
                        writeln!(out, "  %{pfx}.in_range = and i1 %{pfx}.lo, %{pfx}.hi").unwrap();
                        writeln!(out, "  %{pfx}.is_ok = and i1 %{pfx}.parse_ok, %{pfx}.in_range").unwrap();
                    } else {
                        writeln!(out, "  %{pfx}.is_ok = add i1 %{pfx}.parse_ok, 0").unwrap();
                    }
                    // Construct Option result
                    writeln!(out, "  %v{} = alloca i8, i64 16", d.0).unwrap();
                    writeln!(out, "  %{pfx}.tag = select i1 %{pfx}.is_ok, i32 0, i32 1").unwrap();
                    writeln!(out, "  store i32 %{pfx}.tag, ptr %v{}", d.0).unwrap();
                    // Store payload at correct offset with correct type
                    if let Some(ref n) = narrow {
                        // Truncate i64 to narrow type and store at offset 4
                        writeln!(out, "  %{pfx}.trunc = trunc i64 %{pfx}.val to {}", n.llvm_ty).unwrap();
                        writeln!(out, "  %{pfx}.pay = getelementptr i8, ptr %v{}, i64 {}", d.0, n.pay_off).unwrap();
                        writeln!(out, "  store {} %{pfx}.trunc, ptr %{pfx}.pay", n.llvm_ty).unwrap();
                    } else if is_float_parse {
                        // float/double: payload at offset 8
                        writeln!(out, "  %{pfx}.pay = getelementptr i8, ptr %v{}, i64 8", d.0).unwrap();
                        writeln!(out, "  store {val_ty} %{pfx}.val, ptr %{pfx}.pay").unwrap();
                    } else {
                        // int64/uint64: payload at offset 8
                        writeln!(out, "  %{pfx}.pay = getelementptr i8, ptr %v{}, i64 8", d.0).unwrap();
                        writeln!(out, "  store {val_ty} %{pfx}.val, ptr %{pfx}.pay").unwrap();
                    }
                }
                return;
            }

            // Option/Result unwrap — return payload from offset 8
            let is_unwrap = name == "__option_unwrap" || name == "__result_unwrap"
                || name.ends_with("__unwrap") || name.ends_with("__expect");
            if is_unwrap && !args.is_empty() {
                if let Some(d) = dst {
                    let uid = *trap_counter;
                    *trap_counter += 1;
                    // Determine payload type from the Option/Result struct
                    let payload_ty = infer_option_payload_type(
                        &args[0], val_types, module, snames
                    );
                    let payload_ptr = format!("unwrap.{block_id}.{uid}.ptr");
                    let payload_off = option_payload_offset(&payload_ty);
                    writeln!(out, "  %{payload_ptr} = getelementptr i8, ptr %v{}, i64 {payload_off}", args[0].0).unwrap();
                    if payload_ty.starts_with('%') {
                        // Aggregate payload — the struct is inline at payload offset,
                        // return a pointer to it (no load needed)
                        writeln!(out, "  %v{} = getelementptr i8, ptr %v{}, i64 {payload_off}", d.0, args[0].0).unwrap();
                    } else if payload_ty == "ptr" {
                        writeln!(out, "  %v{} = load ptr, ptr %{payload_ptr}", d.0).unwrap();
                    } else {
                        writeln!(out, "  %v{} = load {payload_ty}, ptr %{payload_ptr}", d.0).unwrap();
                    }
                }
                return;
            }

            // Option/Result unwrap_or — return payload if Some/Ok, else default
            let is_unwrap_or = name == "__option_unwrap_or" || name == "__result_unwrap_or"
                || name.ends_with("__unwrap_or");
            if is_unwrap_or && args.len() >= 2 {
                if let Some(d) = dst {
                    let uid = *trap_counter;
                    *trap_counter += 1;
                    let payload_ty = infer_option_payload_type(
                        &args[0], val_types, module, snames
                    );
                    let tag_ptr = format!("unwrapor.{block_id}.{uid}.tagptr");
                    let tag_val = format!("unwrapor.{block_id}.{uid}.tag");
                    let payload_ptr = format!("unwrapor.{block_id}.{uid}.pptr");
                    let payload_val = format!("unwrapor.{block_id}.{uid}.pval");
                    let cmp = format!("unwrapor.{block_id}.{uid}.cmp");
                    writeln!(out, "  %{tag_ptr} = getelementptr i8, ptr %v{}, i32 0", args[0].0).unwrap();
                    writeln!(out, "  %{tag_val} = load i32, ptr %{tag_ptr}").unwrap();
                    writeln!(out, "  %{cmp} = icmp eq i32 %{tag_val}, 0").unwrap();
                    let payload_off = option_payload_offset(&payload_ty);
                    writeln!(out, "  %{payload_ptr} = getelementptr i8, ptr %v{}, i64 {payload_off}", args[0].0).unwrap();
                    if payload_ty.starts_with('%') {
                        // Aggregate payload: select between pointer to inline payload vs default
                        writeln!(out, "  %v{} = select i1 %{cmp}, ptr %{payload_ptr}, ptr %v{}", d.0, args[1].0).unwrap();
                    } else if payload_ty == "ptr" {
                        writeln!(out, "  %{payload_val} = load ptr, ptr %{payload_ptr}").unwrap();
                        writeln!(out, "  %v{} = select i1 %{cmp}, ptr %{payload_val}, ptr %v{}", d.0, args[1].0).unwrap();
                    } else {
                        writeln!(out, "  %{payload_val} = load {payload_ty}, ptr %{payload_ptr}").unwrap();
                        writeln!(out, "  %v{} = select i1 %{cmp}, {payload_ty} %{payload_val}, {payload_ty} %v{}", d.0, args[1].0).unwrap();
                    }
                }
                return;
            }

            // Printf-like functions: extract .data from first GorgetString arg, pass rest by value
            let is_printf_like = name == "printf" || name == "gorget_string_format"
                || name == "gorget_string_format_alloc" || name == "fprintf_stderr";
            if is_printf_like && !args.is_empty() {
                let uid = *trap_counter;
                *trap_counter += 1;

                // fprintf_stderr has args[0]=fd (skip), args[1]=fmt GorgetString
                // printf/gorget_string_format have args[0]=fmt GorgetString
                let (fmt_arg_idx, extra_start) = if name == "fprintf_stderr" && args.len() >= 2 {
                    (1, 2)
                } else {
                    (0, 1)
                };

                // Check if any extra arg is float or string but format uses %lld.
                // GIR may generate %lld for f-string interpolation when the actual type
                // is float (→ %f) or String (→ %.*s). Fix the format string.
                let has_float_arg = args[extra_start..].iter().any(|a| {
                    val_types.get(a.0 as usize).and_then(|t| t.as_ref())
                        .map_or(false, |t| matches!(t, LirType::F32 | LirType::F64))
                });
                let has_str_arg = args[extra_start..].iter().any(|a| {
                    match val_types.get(a.0 as usize).and_then(|t| t.as_ref()) {
                        Some(LirType::PtrTo(sid)) | Some(LirType::Struct(sid)) => {
                            snames.get(&sid.0).map_or(false, |n| n == "GorgetString" || n == "Str")
                        }
                        _ => false,
                    }
                });
                let fmt_strlit = if has_float_arg || has_str_arg {
                    func.blocks.iter().flat_map(|b| b.insts.iter()).find_map(|inst| {
                        if let Inst::StrLit { dst, value } = inst {
                            if dst.0 == args[fmt_arg_idx].0 && value.contains("%lld") {
                                return Some(value.clone());
                            }
                        }
                        None
                    })
                } else { None };

                // Extract .data from the GorgetString format arg
                let str_data = format!("printf.{block_id}.{uid}.data");
                let str_val = format!("printf.{block_id}.{uid}.val");
                if let Some(orig_fmt) = fmt_strlit {
                    // Fix %lld → %f (float) or %lld → %.*s (string) based on arg types.
                    // Process each %lld occurrence, replacing with the correct format
                    // for the corresponding argument's type.
                    let mut fixed_fmt = orig_fmt.clone();
                    let mut arg_idx = 0;
                    let mut result = String::new();
                    let mut chars = fixed_fmt.chars().peekable();
                    while let Some(c) = chars.next() {
                        if c == '%' {
                            let mut spec = String::from('%');
                            // Collect format specifier.
                            // Length modifiers (l, ll, h, hh, j, z, t, L) are NOT conversion
                            // characters — don't stop on them. Only stop on conversion chars
                            // (d, i, u, o, x, X, f, F, e, E, g, G, c, s, p, n, %) or '*'.
                            for nc in chars.by_ref() {
                                spec.push(nc);
                                match nc {
                                    // Length modifiers — keep collecting
                                    'l' | 'h' | 'j' | 'z' | 't' | 'L' | 'q' => {}
                                    // Conversion characters — done
                                    'd' | 'i' | 'u' | 'o' | 'x' | 'X' | 'f' | 'F'
                                    | 'e' | 'E' | 'g' | 'G' | 'c' | 's' | 'p' | 'n'
                                    | '%' | '*' => { break; }
                                    _ => { break; }
                                }
                            }
                            if spec == "%lld" {
                                let arg = args.get(extra_start + arg_idx);
                                let is_str = arg.map_or(false, |a| {
                                    match val_types.get(a.0 as usize).and_then(|t| t.as_ref()) {
                                        Some(LirType::PtrTo(sid)) | Some(LirType::Struct(sid)) => {
                                            snames.get(&sid.0).map_or(false, |n| n == "GorgetString" || n == "Str")
                                        }
                                        _ => false,
                                    }
                                });
                                let is_float = arg.map_or(false, |a| {
                                    val_types.get(a.0 as usize).and_then(|t| t.as_ref())
                                        .map_or(false, |t| matches!(t, LirType::F32 | LirType::F64))
                                });
                                if is_str {
                                    result.push_str("%.*s");
                                } else if is_float {
                                    result.push_str("%f");
                                } else {
                                    result.push_str(&spec);
                                }
                                arg_idx += 1;
                            } else {
                                if spec != "%%" && spec != "%*" { arg_idx += 1; }
                                result.push_str(&spec);
                            }
                        } else {
                            result.push(c);
                        }
                    }
                    fixed_fmt = result;
                    // intern (not get_index) — the fixed format may not have been pre-interned
                    // if it was first produced here. Late additions will be emitted after
                    // function bodies in the module (see late_str_globals handling).
                    let fixed_idx = str_globals.intern(&fixed_fmt);
                    let fixed_len = fixed_fmt.len();
                    let pfx = format!("fmtfix.{block_id}.{uid}");
                    writeln!(out, "  %{pfx} = alloca %GorgetString").unwrap();
                    writeln!(out, "  %{pfx}.fp = getelementptr %GorgetString, ptr %{pfx}, i32 0, i32 0").unwrap();
                    writeln!(out, "  store ptr @.str.{fixed_idx}, ptr %{pfx}.fp").unwrap();
                    // cap = 0 (view/literal) — field 1 (cap at offset +8)
                    writeln!(out, "  %{pfx}.fc = getelementptr %GorgetString, ptr %{pfx}, i32 0, i32 1").unwrap();
                    writeln!(out, "  store i64 0, ptr %{pfx}.fc").unwrap();
                    // len — field 2
                    writeln!(out, "  %{pfx}.fl = getelementptr %GorgetString, ptr %{pfx}, i32 0, i32 2").unwrap();
                    writeln!(out, "  store i64 {fixed_len}, ptr %{pfx}.fl").unwrap();
                    writeln!(out, "  %{pfx}.fa = getelementptr %GorgetString, ptr %{pfx}, i32 0, i32 3").unwrap();
                    writeln!(out, "  store ptr null, ptr %{pfx}.fa").unwrap();
                    writeln!(out, "  %{str_data} = getelementptr %GorgetString, ptr %{pfx}, i32 0, i32 0").unwrap();
                } else {
                    writeln!(out, "  %{str_data} = getelementptr %GorgetString, ptr %v{}, i32 0, i32 0", args[fmt_arg_idx].0).unwrap();
                }
                writeln!(out, "  %{str_val} = load ptr, ptr %{str_data}").unwrap();
                // Build remaining args with their types.
                // GorgetString args need expansion to (i32 len, ptr data) for %.*s.
                let mut extra_args: Vec<String> = Vec::new();
                let mut printf_spills = Vec::new();
                for (ai, a) in args[extra_start..].iter().enumerate() {
                    let aty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                    let is_str = match aty {
                        Some(LirType::PtrTo(sid)) | Some(LirType::Struct(sid)) => {
                            snames.get(&sid.0).map_or(false, |n| n == "GorgetString" || n == "Str")
                        }
                        _ => false,
                    };
                    if is_str {
                        // Expand GorgetString → (i32 len, ptr data) for %.*s
                        let sn = format!("printf.{block_id}.{uid}.s{ai}");
                        printf_spills.push(format!("  %{sn}.lenp = getelementptr %GorgetString, ptr %v{}, i32 0, i32 2", a.0));
                        printf_spills.push(format!("  %{sn}.len = load i64, ptr %{sn}.lenp"));
                        printf_spills.push(format!("  %{sn}.leni = trunc i64 %{sn}.len to i32"));
                        printf_spills.push(format!("  %{sn}.datap = getelementptr %GorgetString, ptr %v{}, i32 0, i32 0", a.0));
                        printf_spills.push(format!("  %{sn}.data = load ptr, ptr %{sn}.datap"));
                        extra_args.push(format!("i32 %{sn}.leni"));
                        extra_args.push(format!("ptr %{sn}.data"));
                    } else {
                        let pty = aty.map(|t| llvm_arg_type(t, snames))
                            .unwrap_or_else(|| "i64".to_string());
                        extra_args.push(format!("{pty} %v{}", a.0));
                    }
                }
                for s in &printf_spills { writeln!(out, "{s}").unwrap(); }
                let all_args = if extra_args.is_empty() {
                    format!("ptr %{str_val}")
                } else {
                    format!("ptr %{str_val}, {}", extra_args.join(", "))
                };
                // Determine return type and actual function name
                if name == "fprintf_stderr" {
                    // fprintf_stderr → fprintf(stderr, fmt, ...)
                    let se = format!("fstderr.{block_id}.{uid}");
                    writeln!(out, "  %{se} = load ptr, ptr @stderr").unwrap();
                    let se_args = format!("ptr %{se}, {all_args}");
                    if let Some(d) = dst {
                        writeln!(out, "  %v{} = call i32 (ptr, ptr, ...) @fprintf({se_args})", d.0).unwrap();
                    } else {
                        writeln!(out, "  call i32 (ptr, ptr, ...) @fprintf({se_args})").unwrap();
                    }
                } else {
                    let (call_name, call_ret) = if name == "printf" {
                        ("printf", "i32")
                    } else {
                        // gorget_string_format etc. return GorgetString
                        (name.as_str(), "%GorgetString")
                    };
                    if let Some(d) = dst {
                        if call_ret == "%GorgetString" {
                            writeln!(out, "  %v{} = alloca %GorgetString", d.0).unwrap();
                            let sret_all = format!("ptr sret(%GorgetString) %v{}, {all_args}", d.0);
                            writeln!(out, "  call void @{call_name}({sret_all})").unwrap();
                        } else {
                            writeln!(out, "  %v{} = call {call_ret} (ptr, ...) @{call_name}({all_args})", d.0).unwrap();
                        }
                    } else {
                        writeln!(out, "  call {call_ret} (ptr, ...) @{call_name}({all_args})").unwrap();
                    }
                }
                return;
            }

            // ── Sentinel-based Option wrapping ──────────────────────
            // Runtime functions that return a scalar (int64_t) with -1 sentinel for "not found".
            // The GIR expects Option[T] — wrap: if (raw >= 0) Some(raw) else None.
            // Uses branchless select to avoid splitting basic blocks (which breaks phi nodes).
            if let Some(d) = dst {
                // Check if the result will be stored into an Option slot
                let opt_slot_info = func.blocks[block_id as usize].insts.iter().find_map(|next_inst| {
                    if let Inst::SlotStore { slot, value, .. } = next_inst {
                        if value.0 == d.0 {
                            let slot_ty = &func.slots[slot.0 as usize].ty;
                            if let LirType::Struct(sid) = slot_ty {
                                let sdef = &module.structs[sid.0 as usize];
                                if sdef.name.starts_with("Option__") {
                                    return Some(*sid);
                                }
                            }
                        }
                    }
                    None
                });
                let ext_decl = module.externs.iter().find(|e| e.name == *name);
                let ext_ret_is_scalar = ext_decl.map_or(false, |e| matches!(e.return_type,
                    LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8
                    | LirType::U64 | LirType::U32 | LirType::U16 | LirType::U8
                    | LirType::F64 | LirType::F32));
                // Skip functions that already return Option natively
                let skip = name.ends_with("__upgrade")
                    || name.ends_with("__recv_timeout")
                    || name.contains("try_parse");
                if let Some(sid) = opt_slot_info {
                    if ext_ret_is_scalar && !skip {
                        let uid = *trap_counter;
                        *trap_counter += 1;
                        let pfx = format!("optwrap.{block_id}.{uid}");
                        let opt_ty = format!("%{}", snames.get(&sid.0).unwrap_or(&"Option".to_string()));
                        let ext_ret = ext_decl.map(|e| &e.return_type).unwrap_or(&LirType::I64);
                        let raw_ty = llvm_type_full(ext_ret, snames);

                        // Build args — match the generic extern handler's coercion rules:
                        // aggregate expected + ptr actual → pass as ptr
                        // ptr expected + scalar actual → spill to alloca
                        let ext_params = ext_decl.map(|e| &e.params);
                        let mut spill_lines = Vec::new();
                        let arg_strs: Vec<String> = args.iter().enumerate().map(|(i, a)| {
                            let expected = ext_params.and_then(|p| p.get(i));
                            let actual = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                            let expects_ptr = expected.map_or(false, |t| t.is_ptr());
                            let expects_agg = expected.map_or(false, |t| t.is_aggregate());
                            let is_ptr = actual.map_or(false, |t| t.is_ptr());
                            if expects_agg && is_ptr {
                                // Aggregate params are declared as ptr in extern (C ABI)
                                format!("ptr %v{}", a.0)
                            } else if expects_ptr && !is_ptr && actual.is_some() {
                                let alty = llvm_arg_type(actual.unwrap(), snames);
                                let sn = format!("{pfx}.spill.{i}");
                                spill_lines.push(format!("  %{sn} = alloca {alty}"));
                                spill_lines.push(format!("  store {alty} %v{}, ptr %{sn}", a.0));
                                format!("ptr %{sn}")
                            } else {
                                let pty = if let Some(ety) = expected {
                                    llvm_arg_type(ety, snames)
                                } else {
                                    actual.map(|t| llvm_arg_type(t, snames))
                                        .unwrap_or_else(|| "i64".to_string())
                                };
                                format!("{pty} %v{}", a.0)
                            }
                        }).collect();
                        for s in &spill_lines { writeln!(out, "{s}").unwrap(); }

                        // Call the function → raw scalar
                        writeln!(out, "  %{pfx}.raw = call {raw_ty} @{name}({})", arg_strs.join(", ")).unwrap();

                        // Alloca the Option struct
                        writeln!(out, "  %v{} = alloca {opt_ty}", d.0).unwrap();

                        // Branchless Option construction (same pattern as gorget_map_get):
                        // tag = select(raw >= 0, 0, 1); store tag; store payload.
                        let is_signed = matches!(ext_ret, LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8);
                        if is_signed {
                            writeln!(out, "  %{pfx}.isneg = icmp slt {raw_ty} %{pfx}.raw, 0").unwrap();
                            writeln!(out, "  %{pfx}.tag = select i1 %{pfx}.isneg, i32 1, i32 0").unwrap();
                        } else {
                            // Unsigned/float: always Some (tag=0)
                            writeln!(out, "  %{pfx}.tag = add i32 0, 0").unwrap();
                        }
                        writeln!(out, "  %{pfx}.tagp = getelementptr {opt_ty}, ptr %v{}, i32 0, i32 0", d.0).unwrap();
                        writeln!(out, "  store i32 %{pfx}.tag, ptr %{pfx}.tagp").unwrap();
                        writeln!(out, "  %{pfx}.payp = getelementptr {opt_ty}, ptr %v{}, i32 0, i32 1", d.0).unwrap();
                        writeln!(out, "  store {raw_ty} %{pfx}.raw, ptr %{pfx}.payp").unwrap();
                        return;
                    }
                }
            }

            // Redirect no-arg strip variants: gorget_str_strip(s) → gorget_str_trim(s), etc.
            // Same logic as C backend (emit_call_extern.rs line 708).
            let name: &str = if args.len() == 1 {
                match name.as_str() {
                    "gorget_str_strip" => "gorget_str_trim",
                    "gorget_str_lstrip" => "gorget_str_lstrip_ws",
                    "gorget_str_rstrip" => "gorget_str_rstrip_ws",
                    _ => name.as_str(),
                }
            } else { name.as_str() };
            // gorget_regex_find_at is a C macro alias for gorget_regex_find
            let name: &str = if name == "gorget_regex_find_at" { "gorget_regex_find" } else { name };

            // gorget_str_cmp: C returns int (32-bit). On aarch64, 'mov w0, -1' zero-extends
            // x0 to 0xFFFFFFFF (not 0xFFFFFFFFFFFFFFFF). Must call as i32 and sext to i64
            // so the LIR I64 result has the correct sign.
            if name == "gorget_str_cmp" {
                if let Some(d) = dst {
                    let uid = *trap_counter;
                    *trap_counter += 1;
                    let pfx = format!("strcmp32.{block_id}.{uid}");
                    let arg_strs: Vec<String> = args.iter().map(|a| format!("ptr %v{}", a.0)).collect();
                    writeln!(out, "  %{pfx}.raw = call i32 @gorget_str_cmp({})", arg_strs.join(", ")).unwrap();
                    writeln!(out, "  %v{} = sext i32 %{pfx}.raw to i64", d.0).unwrap();
                } else {
                    let arg_strs: Vec<String> = args.iter().map(|a| format!("ptr %v{}", a.0)).collect();
                    writeln!(out, "  call i32 @gorget_str_cmp({})", arg_strs.join(", ")).unwrap();
                }
                return;
            }

            // Look up the extern declaration
            let ext = module.externs.iter().find(|e| e.name == *name);
            if let Some(ext) = ext {
                // CStr return ABI: function returns const char*, wrap to GorgetString
                if ext.return_abi == crate::ir::abi::AbiKind::CStr {
                    if let Some(d) = dst {
                        let uid = *trap_counter;
                        *trap_counter += 1;
                        let pfx = format!("cstr_ret.{block_id}.{uid}");
                        // Build args — CStr params need .data extraction from GorgetString
                        let mut spill_lines2 = Vec::new();
                        let arg_strs2: Vec<String> = args.iter().enumerate().map(|(i, a)| {
                            let expected = ext.params.get(i);
                            let actual = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                            let is_ptr_val = actual.map_or(false, |t| t.is_ptr());
                            let is_str_val = match actual {
                                Some(LirType::PtrTo(sid)) => snames.get(&sid.0).map_or(false, |n| n == "GorgetString"),
                                _ => false,
                            };
                            let param_abi = ext.param_abis.get(i).copied().unwrap_or_default();
                            // If param ABI is CStr or if extern expects Ptr and value is GorgetString
                            if param_abi == crate::ir::abi::AbiKind::CStr || (expected.map_or(false, |t| t.is_ptr()) && is_str_val) {
                                // Extract .data from GorgetString
                                let sn = format!("{pfx}.cstr.{i}");
                                spill_lines2.push(format!("  %{sn} = load ptr, ptr %v{}", a.0));
                                format!("ptr %{sn}")
                            } else if is_ptr_val {
                                format!("ptr %v{}", a.0)
                            } else {
                                let pty = actual.map(|t| llvm_arg_type(t, snames)).unwrap_or_else(|| "i64".to_string());
                                format!("{pty} %v{}", a.0)
                            }
                        }).collect();
                        for s in &spill_lines2 { writeln!(out, "{s}").unwrap(); }
                        // Call → returns const char*
                        writeln!(out, "  %{pfx}.raw = call ptr @{}({})", name, arg_strs2.join(", ")).unwrap();
                        // Wrap const char* into GorgetString via gorget_str_from_cstr (sret)
                        writeln!(out, "  %v{} = alloca %GorgetString", d.0).unwrap();
                        writeln!(out, "  call void @gorget_str_from_cstr(ptr sret(%GorgetString) %v{}, ptr %{pfx}.raw)", d.0).unwrap();
                    }
                    return;
                }

                // gorget_file_read_all: C function returns GorgetString but LIR expects Result.
                // Redirect to C wrapper __gorget_file_read_all_r that returns the proper Result.
                if name == "gorget_file_read_all" && !args.is_empty() {
                    *trap_counter += 1;
                    let ret_ty = llvm_type_full(&ext.return_type, snames);
                    if let Some(d) = dst {
                        writeln!(out, "  %v{} = alloca {ret_ty}", d.0).unwrap();
                        writeln!(out, "  call void @__gorget_file_read_all_r(ptr sret({ret_ty}) %v{}, ptr %v{})", d.0, args[0].0).unwrap();
                    }
                    return;
                }

                let actual_ret = ext.return_type.clone();
                let ret_ty = llvm_type_full(&actual_ret, snames);
                // For each arg, handle type mismatches:
                // - extern expects ptr but value is scalar → spill to alloca
                // - extern expects aggregate but value is ptr → load struct from ptr
                let ext_uid = *trap_counter;
                *trap_counter += 1;
                let mut spill_lines = Vec::new();
                let arg_strs: Vec<String> = args.iter().enumerate().map(|(i, a)| {
                    let expected_ty = if i < ext.params.len() {
                        Some(&ext.params[i])
                    } else {
                        None
                    };
                    let actual_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                    let expects_ptr = expected_ty.map_or(false, |t| t.is_ptr());
                    let is_ptr = actual_ty.map_or(false, |t| t.is_ptr());

                    let expects_agg = expected_ty.map_or(false, |t| t.is_aggregate());

                    // Detect GorgetString value passed to const char* param:
                    // Use param_abis (CStr annotation) or known function list.
                    let param_abi = ext.param_abis.get(i).copied().unwrap_or_default();
                    let is_str_to_cstr = (param_abi == crate::ir::abi::AbiKind::CStr
                        || (expects_ptr && match (name, i) {
                            ("gorget_assert_fail_values", 0) | ("gorget_panic", 0) => true,
                            // gorget_string_push_line takes (GorgetString*, const char*) — extract data from arg 1
                            ("gorget_string_push_line", 1) => true,
                            _ => false,
                        }))
                        && match actual_ty {
                        Some(LirType::PtrTo(sid)) if snames.get(&sid.0).map_or(false, |n| n == "GorgetString") => {
                            true
                        }
                        _ => false,
                    };

                    let expects_small_agg = expected_ty.map_or(false, |t| {
                        t.is_aggregate() && is_small_aggregate(t, &module.structs)
                    });
                    if expects_small_agg && is_ptr {
                        // Small aggregate (≤16 bytes): load from ptr and pass by value
                        let agg_ty = llvm_type_full(expected_ty.unwrap(), snames);
                        let load_name = format!("aggload.{block_id}.{ext_uid}.{i}");
                        spill_lines.push(format!("  %{load_name} = load {agg_ty}, ptr %v{}", a.0));
                        format!("{agg_ty} %{load_name}")
                    } else if expects_agg && is_ptr {
                        // Large aggregate (>16 bytes): pass pointer (indirect C ABI).
                        format!("ptr %v{}", a.0)
                    } else if is_str_to_cstr {
                        // GorgetString → const char*: load .data pointer from field 0
                        let cstr_name = format!("cstr.{block_id}.{ext_uid}.{i}");
                        spill_lines.push(format!("  %{cstr_name} = load ptr, ptr %v{}", a.0));
                        format!("ptr %{cstr_name}")
                    } else if expects_ptr && !is_ptr && actual_ty.is_some() {
                        // Spill scalar to alloca and pass pointer
                        let spill_ty = llvm_arg_type(actual_ty.unwrap(), snames);
                        let spill_name = format!("spill.{block_id}.{ext_uid}.{i}");
                        spill_lines.push(format!("  %{spill_name} = alloca {spill_ty}"));
                        spill_lines.push(format!("  store {spill_ty} %v{}, ptr %{spill_name}", a.0));
                        format!("ptr %{spill_name}")
                    } else {
                        let pty = if let Some(ety) = expected_ty {
                            llvm_arg_type(ety, snames)
                        } else {
                            actual_ty.map(|t| llvm_arg_type(t, snames))
                                .unwrap_or_else(|| "i64".to_string())
                        };
                        format!("{pty} %v{}", a.0)
                    }
                }).collect();
                // Emit spill allocas before the call
                for line in &spill_lines {
                    writeln!(out, "{line}").unwrap();
                }
                let variadic = if ext.is_variadic { ", ..." } else { "" };
                let _fn_ty = format!("{ret_ty} ({}{})",
                    ext.params.iter().map(|p| llvm_type_full(p, snames)).collect::<Vec<_>>().join(", "),
                    variadic,
                );
                if let Some(d) = dst {
                    if ext.return_type == LirType::Void {
                        writeln!(out, "  call void @{name}({})", arg_strs.join(", ")).unwrap();
                    } else if needs_sret(&ext.return_type, &module.structs) {
                        // Large aggregate return: sret convention
                        writeln!(out, "  %v{} = alloca {ret_ty}", d.0).unwrap();
                        let sret_args = if arg_strs.is_empty() {
                            format!("ptr sret({ret_ty}) %v{}", d.0)
                        } else {
                            format!("ptr sret({ret_ty}) %v{}, {}", d.0, arg_strs.join(", "))
                        };
                        writeln!(out, "  call void @{name}({sret_args})").unwrap();
                    } else if ext.return_type.is_aggregate() {
                        // Small aggregate: returned in registers
                        writeln!(out, "  %v{}.ret = call {ret_ty} @{name}({})", d.0, arg_strs.join(", ")).unwrap();
                        writeln!(out, "  %v{} = alloca {ret_ty}", d.0).unwrap();
                        writeln!(out, "  store {ret_ty} %v{}.ret, ptr %v{}", d.0, d.0).unwrap();
                    } else {
                        writeln!(out, "  %v{} = call {ret_ty} @{name}({})", d.0, arg_strs.join(", ")).unwrap();
                    }
                } else {
                    writeln!(out, "  call {ret_ty} @{name}({})", arg_strs.join(", ")).unwrap();
                }
            } else {
                // Unknown extern — emit as call with inferred types
                let _ext_uid = *trap_counter;
                *trap_counter += 1;
                let arg_strs: Vec<String> = args.iter().map(|a| {
                    let pty = val_types.get(a.0 as usize)
                        .and_then(|t| t.as_ref())
                        .map(|t| llvm_arg_type(t, snames))
                        .unwrap_or_else(|| "i64".to_string());
                    format!("{pty} %v{}", a.0)
                }).collect();
                // Check if this function was auto-declared with sret (aggregate return)
                let returns_array = name.contains("gorget_array_clone")
                    || name.contains("gorget_array_slice")
                    || name.contains("gorget_array_concat");
                let returns_string = name.contains("gorget_str_slice")
                    || name.contains("gorget_str_substr")
                    || name.contains("gorget_str_trim")
                    || name.contains("gorget_str_to_lower")
                    || name.contains("gorget_str_to_upper")
                    || name.contains("gorget_str_replace")
                    || name.contains("gorget_str_repeat")
                    || name.contains("gorget_str_join")
                    || name.contains("gorget_str_reverse")
                    || name.contains("gorget_str_pad")
                    || name.contains("gorget_str_lstrip")
                    || name.contains("gorget_str_rstrip")
                    || name.contains("gorget_str_strip")
                    || name.contains("gorget_string_clone")
                    || name.contains("gorget_int_to_str")
                    || name.contains("gorget_float_to_str")
                    || name.contains("gorget_char_to_str");
                let returns_map = name.contains("gorget_map_clone");
                let returns_set = name.contains("gorget_set_clone");
                let sret_ty = if returns_array { Some("%GorgetArray") }
                    else if returns_string { Some("%GorgetString") }
                    else if returns_map { Some("%GorgetMap") }
                    else if returns_set { Some("%GorgetSet") }
                    else { None };
                if let Some(d) = dst {
                    if let Some(sret) = sret_ty {
                        // Aggregate return via sret
                        writeln!(out, "  %v{} = alloca {sret}", d.0).unwrap();
                        let sret_args = if arg_strs.is_empty() {
                            format!("ptr sret({sret}) %v{}", d.0)
                        } else {
                            format!("ptr sret({sret}) %v{}, {}", d.0, arg_strs.join(", "))
                        };
                        writeln!(out, "  call void @{name}({sret_args})").unwrap();
                    } else {
                        // Default to ptr return (most runtime fns return pointers)
                        writeln!(out, "  %v{} = call ptr @{name}({})", d.0, arg_strs.join(", ")).unwrap();
                    }
                } else {
                    writeln!(out, "  call void @{name}({})", arg_strs.join(", ")).unwrap();
                }
            }
        }
        Inst::CallPtr { dst, callee, args } => {
            // Indirect call through function pointer
            let arg_strs: Vec<String> = args.iter().map(|a| {
                let pty = val_types.get(a.0 as usize)
                    .and_then(|t| t.as_ref())
                    .map(|t| llvm_arg_type(t, snames))
                    .unwrap_or_else(|| "i64".to_string());
                format!("{pty} %v{}", a.0)
            }).collect();
            let ret_ty = if dst.is_some() { "i64" } else { "void" };
            let param_tys: Vec<String> = args.iter().map(|a| {
                val_types.get(a.0 as usize)
                    .and_then(|t| t.as_ref())
                    .map(|t| llvm_arg_type(t, snames))
                    .unwrap_or_else(|| "i64".to_string())
            }).collect();
            let _fn_ty = format!("{ret_ty} ({})", param_tys.join(", "));
            if let Some(d) = dst {
                writeln!(out, "  %v{} = call {ret_ty} %v{}({})", d.0, callee.0, arg_strs.join(", ")).unwrap();
            } else {
                writeln!(out, "  call {ret_ty} %v{}({})", callee.0, arg_strs.join(", ")).unwrap();
            }
        }

        // ── Runtime Checks ──────────────────────────────────────────
        Inst::BoundsCheck { index, len } => {
            let trap_id = *trap_counter;
            *trap_counter += 1;
            let cmp_name = format!("bc.{block_id}.{trap_id}.cmp");
            let trap_label = format!("bc.{block_id}.{trap_id}.trap");
            let ok_label = format!("bc.{block_id}.{trap_id}.ok");
            writeln!(out, "  %{cmp_name} = icmp uge i64 %v{}, %v{}", index.0, len.0).unwrap();
            writeln!(out, "  br i1 %{cmp_name}, label %{trap_label}, label %{ok_label}").unwrap();
            writeln!(out, "{trap_label}:").unwrap();
            let fmt_idx = str_globals.get_index("index out of bounds: index %lld, len %lld\n");
            let se = format!("bc.{block_id}.{trap_id}.stderr");
            writeln!(out, "  %{se} = load ptr, ptr @stderr").unwrap();
            writeln!(out, "  call i32 (ptr, ptr, ...) @fprintf(ptr %{se}, ptr @.str.{fmt_idx}, i64 %v{}, i64 %v{})", index.0, len.0).unwrap();
            writeln!(out, "  call void @exit(i32 1)").unwrap();
            writeln!(out, "  unreachable").unwrap();
            writeln!(out, "{ok_label}:").unwrap();
            *current_label = ok_label;
        }
        Inst::DivCheck { divisor } => {
            let trap_id = *trap_counter;
            *trap_counter += 1;
            let cmp_name = format!("dc.{block_id}.{trap_id}.cmp");
            let trap_label = format!("dc.{block_id}.{trap_id}.trap");
            let ok_label = format!("dc.{block_id}.{trap_id}.ok");
            writeln!(out, "  %{cmp_name} = icmp eq i64 %v{}, 0", divisor.0).unwrap();
            writeln!(out, "  br i1 %{cmp_name}, label %{trap_label}, label %{ok_label}").unwrap();
            writeln!(out, "{trap_label}:").unwrap();
            let fmt_idx = str_globals.get_index("division by zero\n");
            let se = format!("dc.{block_id}.{trap_id}.stderr");
            writeln!(out, "  %{se} = load ptr, ptr @stderr").unwrap();
            writeln!(out, "  call i32 (ptr, ptr, ...) @fprintf(ptr %{se}, ptr @.str.{fmt_idx})").unwrap();
            writeln!(out, "  call void @exit(i32 1)").unwrap();
            writeln!(out, "  unreachable").unwrap();
            writeln!(out, "{ok_label}:").unwrap();
            *current_label = ok_label;
        }
        Inst::Trap { msg } => {
            let fmt_idx = str_globals.get_index(msg);
            let se = format!("trap.{block_id}.stderr");
            writeln!(out, "  %{se} = load ptr, ptr @stderr").unwrap();
            writeln!(out, "  call i32 (ptr, ptr, ...) @fprintf(ptr %{se}, ptr @.str.{fmt_idx})").unwrap();
            writeln!(out, "  call void @exit(i32 1)").unwrap();
            writeln!(out, "  unreachable").unwrap();
        }

        // ── Printf / Fprintf ────────────────────────────────────────
        Inst::Printf { fmt, args } => {
            let fmt_idx = str_globals.get_index(fmt);
            let arg_strs: Vec<String> = args.iter().enumerate().map(|(ai, a)| {
                let pty = val_types.get(a.0 as usize)
                    .and_then(|t| t.as_ref());
                // PtrTo(Str) args: extract .data field so printf gets the char*
                if pty.map_or(false, |t| is_ptr_to_gorget_string(t, snames)) {
                    let data_ptr = format!("pf.{block_id}.{}.{ai}.dp", trap_counter);
                    let data_val = format!("pf.{block_id}.{}.{ai}.dv", trap_counter);
                    writeln!(out, "  %{data_ptr} = getelementptr %GorgetString, ptr %v{}, i32 0, i32 0", a.0).unwrap();
                    writeln!(out, "  %{data_val} = load ptr, ptr %{data_ptr}").unwrap();
                    format!("ptr %{data_val}")
                } else {
                    let ty_str = pty.map(|t| llvm_arg_type(t, snames))
                        .unwrap_or_else(|| "i64".to_string());
                    format!("{ty_str} %v{}", a.0)
                }
            }).collect();
            let all_args = if arg_strs.is_empty() {
                format!("ptr @.str.{fmt_idx}")
            } else {
                format!("ptr @.str.{fmt_idx}, {}", arg_strs.join(", "))
            };
            writeln!(out, "  call i32 (ptr, ...) @printf({all_args})").unwrap();
        }
        Inst::Fprintf { fd, fmt, args } => {
            let fmt_idx = str_globals.get_index(fmt);
            let arg_strs: Vec<String> = args.iter().enumerate().map(|(ai, a)| {
                let pty = val_types.get(a.0 as usize)
                    .and_then(|t| t.as_ref());
                if pty.map_or(false, |t| is_ptr_to_gorget_string(t, snames)) {
                    let data_ptr = format!("fpf.{block_id}.{}.{ai}.dp", trap_counter);
                    let data_val = format!("fpf.{block_id}.{}.{ai}.dv", trap_counter);
                    writeln!(out, "  %{data_ptr} = getelementptr %GorgetString, ptr %v{}, i32 0, i32 0", a.0).unwrap();
                    writeln!(out, "  %{data_val} = load ptr, ptr %{data_ptr}").unwrap();
                    format!("ptr %{data_val}")
                } else {
                    let ty_str = pty.map(|t| llvm_arg_type(t, snames))
                        .unwrap_or_else(|| "i64".to_string());
                    format!("{ty_str} %v{}", a.0)
                }
            }).collect();
            let all_args = if arg_strs.is_empty() {
                format!("ptr %v{}, ptr @.str.{fmt_idx}", fd.0)
            } else {
                format!("ptr %v{}, ptr @.str.{fmt_idx}, {}", fd.0, arg_strs.join(", "))
            };
            writeln!(out, "  call i32 (ptr, ptr, ...) @fprintf({all_args})").unwrap();
        }

        // ── InlineC (legacy — all known sites now use proper CallExtern) ──
        Inst::InlineC { dst, code } => {
            if code.contains("exit(") || code.contains("abort()") || code.contains("gorget_panic") {
                writeln!(out, "  call void @exit(i32 1) ; InlineC fatal").unwrap();
                writeln!(out, "  unreachable").unwrap();
            } else if let Some(d) = dst {
                writeln!(out, "  %v{} = add i64 0, 0 ; InlineC skipped: {}", d.0, code.chars().take(60).collect::<String>()).unwrap();
            } else {
                writeln!(out, "  ; InlineC skipped").unwrap();
            }
        }

        // ── MoveSlot (consumed by drop elaboration) ────────────────
        Inst::MoveSlot { .. } => {}

        // ── Nop ─────────────────────────────────────────────────────
        Inst::Nop => {
            // nothing
        }
    }
}

// ── Overflow Check Emission ────────────────────────────────────────────────

fn emit_overflow_check(
    out: &mut String,
    op: &str,  // "add", "sub", "mul"
    dst: &ValueId,
    lhs: &ValueId,
    rhs: &ValueId,
    ty: &LirType,
    _snames: &HashMap<u32, String>,
    block_id: u32,
    trap_counter: &mut u32,
    current_label: &mut String,
    str_globals: &StrGlobals,
    val_types: &[Option<LirType>],
) {
    let lty = llvm_type(ty);
    let bits = int_bits(ty);
    let signed_prefix = if is_signed(ty) { "s" } else { "u" };
    let intrinsic = format!("@llvm.{signed_prefix}{op}.with.overflow.i{bits}");

    let trap_id = *trap_counter;
    *trap_counter += 1;

    let result = format!("ov.{block_id}.{trap_id}.result");
    let val = format!("ov.{block_id}.{trap_id}.val");
    let flag = format!("ov.{block_id}.{trap_id}.flag");
    let trap_label = format!("ov.{block_id}.{trap_id}.trap");
    let ok_label = format!("ov.{block_id}.{trap_id}.ok");

    // For sub-64-bit types (i8, i16, i32), LIR constants are always emitted as i64,
    // but some operands may already be the narrower type (e.g., from gorget_str_byte_at).
    // Only emit trunc when the actual operand type is wider than the target type.
    let needs_trunc = |vid: u32| -> bool {
        if bits >= 64 { return false; }
        match val_types.get(vid as usize).and_then(|t| t.as_ref()) {
            Some(t) => int_bits(t) > bits,
            None => true, // unknown — assume i64, truncate to be safe
        }
    };
    let lhs_str = if needs_trunc(lhs.0) {
        let name = format!("ov.{block_id}.{trap_id}.lhs");
        writeln!(out, "  %{name} = trunc i64 %v{} to {lty}", lhs.0).unwrap();
        format!("%{name}")
    } else {
        format!("%v{}", lhs.0)
    };
    let rhs_str = if needs_trunc(rhs.0) {
        let name = format!("ov.{block_id}.{trap_id}.rhs");
        writeln!(out, "  %{name} = trunc i64 %v{} to {lty}", rhs.0).unwrap();
        format!("%{name}")
    } else {
        format!("%v{}", rhs.0)
    };

    writeln!(out, "  %{result} = call {{ {lty}, i1 }} {intrinsic}({lty} {lhs_str}, {lty} {rhs_str})").unwrap();
    writeln!(out, "  %{val} = extractvalue {{ {lty}, i1 }} %{result}, 0").unwrap();
    writeln!(out, "  %{flag} = extractvalue {{ {lty}, i1 }} %{result}, 1").unwrap();
    writeln!(out, "  br i1 %{flag}, label %{trap_label}, label %{ok_label}").unwrap();
    writeln!(out, "{trap_label}:").unwrap();
    // Match C backend: fprintf(stderr, "gorget: integer overflow\n"); exit(1);
    let ov_se = format!("ov.{block_id}.{trap_id}.stderr");
    writeln!(out, "  %{ov_se} = load ptr, ptr @stderr").unwrap();
    let ov_msg_idx = str_globals.get_index("gorget: integer overflow\n");
    writeln!(out, "  call i32 (ptr, ptr, ...) @fprintf(ptr %{ov_se}, ptr @.str.{ov_msg_idx})").unwrap();
    writeln!(out, "  call void @exit(i32 1)").unwrap();
    writeln!(out, "  unreachable").unwrap();
    writeln!(out, "{ok_label}:").unwrap();
    writeln!(out, "  %v{} = add {lty} 0, %{val}", dst.0).unwrap();

    // Update current_label — execution continues from the ok block
    *current_label = ok_label;
}

// ── Terminator Emission ────────────────────────────────────────────────────

fn emit_term(
    out: &mut String,
    term: &Term,
    func: &LirFunction,
    _module: &LirModule,
    snames: &HashMap<u32, String>,
    val_types: &[Option<LirType>],
) {
    match term {
        Term::Ret(val) => {
            let ret_ty = llvm_type_full(&func.return_type, snames);
            let is_main = func.name == "main";
            if is_main {
                // main() always returns i32. LIR return value might be i64 or ptr to Result.
                let val_ty = val_types.get(val.0 as usize).and_then(|t| t.as_ref());
                if matches!(val_ty, Some(LirType::I64 | LirType::U64)) {
                    writeln!(out, "  %main.ret.{} = trunc i64 %v{} to i32", val.0, val.0).unwrap();
                    writeln!(out, "  ret i32 %main.ret.{}", val.0).unwrap();
                } else {
                    writeln!(out, "  ret i32 0 ; main implicit return").unwrap();
                }
            } else if needs_sret(&func.return_type, &_module.structs) {
                // Large aggregate: sret convention — memcpy result into %sret.out, then ret void
                let sz = sizeof_lir_type(&func.return_type, &_module.structs, snames);
                writeln!(out, "  call ptr @memcpy(ptr %sret.out, ptr %v{}, i64 {sz})", val.0).unwrap();
                writeln!(out, "  ret void").unwrap();
            } else if func.return_type.is_aggregate() {
                // Small aggregate: load from pointer and return by value
                writeln!(out, "  %retval.{} = load {ret_ty}, ptr %v{}", val.0, val.0).unwrap();
                writeln!(out, "  ret {ret_ty} %retval.{}", val.0).unwrap();
            } else {
                // Check for type mismatch between value type and function return type.
                let val_ty = val_types.get(val.0 as usize).and_then(|t| t.as_ref());
                let needs_bitcast = func.return_type.is_float()
                    && val_ty.map_or(false, |t| t.is_integer());
                let needs_trunc_bool = func.return_type == LirType::Bool
                    && val_ty.map_or(false, |t| *t != LirType::Bool && t.is_integer());
                let needs_trunc_i32 = matches!(func.return_type, LirType::I32 | LirType::U32)
                    && val_ty.map_or(false, |t| matches!(t, LirType::I64 | LirType::U64));
                if needs_bitcast {
                    // e.g. function returns double, value is i64
                    let val_ty_str = val_ty.map(|t| llvm_type(t)).unwrap_or("i64");
                    writeln!(out, "  %ret.bc.{} = bitcast {val_ty_str} %v{} to {ret_ty}", val.0, val.0).unwrap();
                    writeln!(out, "  ret {ret_ty} %ret.bc.{}", val.0).unwrap();
                } else if needs_trunc_bool {
                    // e.g. function returns i1, value is i64 (bool vector element)
                    let val_ty_str = val_ty.map(|t| llvm_type(t)).unwrap_or("i64");
                    writeln!(out, "  %ret.tb.{} = trunc {val_ty_str} %v{} to i1", val.0, val.0).unwrap();
                    writeln!(out, "  ret i1 %ret.tb.{}", val.0).unwrap();
                } else if needs_trunc_i32 {
                    // e.g. function returns i32, value is i64
                    writeln!(out, "  %ret.t32.{} = trunc i64 %v{} to i32", val.0, val.0).unwrap();
                    writeln!(out, "  ret i32 %ret.t32.{}", val.0).unwrap();
                } else {
                    writeln!(out, "  ret {ret_ty} %v{}", val.0).unwrap();
                }
            }
        }
        Term::RetVoid => {
            writeln!(out, "  ret void").unwrap();
        }
        Term::Jump(target, _args) => {
            // Args are consumed by phi nodes in the target block
            writeln!(out, "  br label %bb{}", target.0).unwrap();
        }
        Term::Branch { cond, then_block, then_args: _, else_block, else_args: _, .. } => {
            writeln!(out, "  br i1 %v{}, label %bb{}, label %bb{}", cond.0, then_block.0, else_block.0).unwrap();
        }
        Term::Switch { value, cases, default, default_args: _ } => {
            let val_ty = val_types.get(value.0 as usize)
                .and_then(|t| t.as_ref())
                .map(|t| llvm_type(t))
                .unwrap_or("i64");
            write!(out, "  switch {val_ty} %v{}, label %bb{} [\n", value.0, default.0).unwrap();
            for (case_val, target, _args) in cases {
                writeln!(out, "    {val_ty} {case_val}, label %bb{}", target.0).unwrap();
            }
            writeln!(out, "  ]").unwrap();
        }
        Term::Unreachable => {
            writeln!(out, "  unreachable").unwrap();
        }
    }
}
