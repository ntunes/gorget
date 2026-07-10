//! LIR → LLVM IR backend.
//!
//! Generates LLVM IR textual format (.ll) from LIR. The mapping is nearly 1:1
//! since LIR is already SSA with block parameters (phi-equivalent).

use crate::lir::*;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;

/// LLVM IR backend.
pub struct LlvmBackend;

/// Returns true if an aggregate return type needs sret convention.
/// Small structs (≤16 bytes on aarch64) are returned in registers.
fn needs_sret(ty: &LirType, structs: &[StructDef]) -> bool {
    ty.is_aggregate() && !is_small_aggregate(ty, structs)
}

/// Shared `is_small_aggregate` lives in `src/lir/lower/types.rs` so the
/// GIR→LIR pass and the backends all agree on the register-vs-memory
/// threshold. Importing it here preserves the many local call sites.
use crate::lir::lower::types::is_small_aggregate;

impl super::Backend for LlvmBackend {
    fn name(&self) -> &str {
        "llvm"
    }

    fn generate(&self, module: &crate::bir::BirModule) -> super::CodegenOutput {
        // Backends consume BIR (guaranteed by the newtype); the LLVM emitter
        // works on the underlying LirModule for its 1:1 translation.
        super::CodegenOutput {
            code: generate_llvm_ir(module.as_lir()),
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
        LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef => "ptr",
        // Item 7e (Phase 1): Resource lowers to its runtime struct form.
        // Scalar pointer-kind resources (RefCounted) lower to `ptr`; the
        // aggregate-shaped ones flow through `llvm_type_full`.
        LirType::Resource { kind, .. } => match kind {
            crate::lir::ResourceKind::RefCounted => "ptr",
            _ => unreachable!("non-pointer Resource — use llvm_type_full"),
        },
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
        // Item 7e (Phase 1): Resource maps to a named LLVM struct
        // (`%GorgetArray`, `%GorgetMap`, etc.) for aggregate-shaped resource
        // kinds, and to `ptr` for the pointer-shaped ones (RefCounted).
        LirType::Resource { kind, .. } => match kind {
            crate::lir::ResourceKind::GorgetString => "%GorgetString".to_string(),
            crate::lir::ResourceKind::GorgetArray => "%GorgetArray".to_string(),
            crate::lir::ResourceKind::GorgetMap => "%GorgetMap".to_string(),
            crate::lir::ResourceKind::GorgetSet => "%GorgetSet".to_string(),
            crate::lir::ResourceKind::GorgetClosure => "%GorgetClosure".to_string(),
            crate::lir::ResourceKind::RefCounted => "ptr".to_string(),
        },
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

/// Parameter type for a C-ABI function *declaration*.
///
/// Gorget `bool` lowers to LLVM `i1`, but a C `_Bool` argument must be passed
/// `zeroext` per the x86-64 psABI: the callee reads the argument byte and the
/// caller must guarantee the high bits are clear. An `i1` produced by `icmp`
/// or `xor` lives in a register with *undefined* high bits, so without
/// `zeroext` the C runtime reads garbage — e.g. `not (x == 0)` then flips
/// nondeterministically with register allocation (it surfaced as a phantom
/// leak in `leak_string_heavy` after unrelated codegen churn). llc applies the
/// declaration's parameter attribute to direct call sites, so annotating the
/// declaration is sufficient; this mirrors what clang emits for every `bool`
/// parameter. Keep new C-function declarations that take a `bool` routed
/// through here (or spell `i1 zeroext` by hand) — a bare `i1` param is the bug.
fn llvm_c_param_type(ty: &LirType, snames: &HashMap<u32, String>) -> String {
    match ty {
        LirType::Bool => "i1 zeroext".to_string(),
        _ => llvm_arg_type(ty, snames),
    }
}

/// On x86_64 SysV, large aggregates (>16 bytes) passed by value go on the
/// outgoing stack frame as a memory-class copy. On aarch64 AAPCS64 they are
/// instead passed via an implicit pointer in a register, which is what the
/// bare `ptr` IR emission already produces. So we emit `byval(...)` only on
/// x86_64; on aarch64 (and other targets) the existing `ptr` matches the
/// platform ABI. The annotation must be present on both the call site and
/// the extern declaration — they are kept in sync via this single helper.
///
/// `GG_LLVM_FORCE_X86_64_ABI=1` forces the x86_64 path on a non-x86_64 host —
/// dev-time affordance for inspecting the IR shape that an x86_64 build
/// would emit (e.g. cross-target llc verification from an aarch64 box).
fn large_agg_byval_attr(ty: &LirType, snames: &HashMap<u32, String>) -> String {
    let on_x86_64 = cfg!(target_arch = "x86_64")
        || std::env::var_os("GG_LLVM_FORCE_X86_64_ABI").is_some();
    if on_x86_64 {
        format!("byval({}) align 8 ", llvm_type_full(ty, snames))
    } else {
        String::new()
    }
}

/// Reverse-lookup a struct id by its sanitized name (the form stored in
/// `snames`). Linear scan, but `snames` is small (~tens of entries per
/// module) so this is cheap.
fn struct_sid_by_name(snames: &HashMap<u32, String>, name: &str) -> Option<u32> {
    snames.iter().find_map(|(k, v)| if v == name { Some(*k) } else { None })
}

/// Byval attribute for a Str/GorgetString-by-value param. Resolves the
/// `GorgetString` struct id once and formats the x86_64 `byval(...) align 8 `
/// prefix (with a trailing space ready to splice before `%vN`). Empty on
/// non-x86_64 — `large_agg_byval_attr` handles the platform check. Returns
/// empty if `GorgetString` isn't registered (defensive — the runtime always
/// emits it, but bail rather than crash if a future refactor changes that).
fn gorget_string_byval_attr(snames: &HashMap<u32, String>) -> String {
    struct_sid_by_name(snames, "GorgetString")
        .map(|sid| large_agg_byval_attr(&LirType::Struct(StructId(sid)), snames))
        .unwrap_or_default()
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

// `user_key_type_from_original` and `emit_user_key_bridge_wiring` were
// retired when `Inst::SetCollectionBridge` landed. The user-key-type
// extraction now lives in `lir::types::wire_collection_bridges` (run
// once per module); the wiring itself is emitted inline by the
// `SetCollectionBridge` arm in `emit_inst`.

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

/// Parse a Dict/HashMap/Set HOF name like `Dict__int64_t__GorgetString__fold` → ("int64_t", "GorgetString", "fold").
/// Returns (key_c, val_c, method) or None.
/// Parse a Dict/HashMap HOF name like `Dict__int64_t__int64_t__fold` →
/// ("int64_t", "int64_t", "fold"). `filter` is excluded — migrated to
/// `Inst::HofExpand` and handled by the BIR expansion, not LLVM's
/// inline dispatch.
fn parse_dict_hof(name: &str) -> Option<(&str, &str, &str)> {
    let prefix = if name.starts_with("Dict__") { "Dict__" }
        else if name.starts_with("HashMap__") { "HashMap__" }
        else { return None; };
    let rest = name.strip_prefix(prefix)?;
    let sep = rest.rfind("__")?;
    let method = &rest[sep + 2..];
    match method {
        "fold" | "each" | "any" | "all" => {}
        _ => return None,
    }
    let kv = &rest[..sep];
    // Split key__val — first __ separator
    let kv_sep = kv.find("__")?;
    let key = &kv[..kv_sep];
    let val = &kv[kv_sep + 2..];
    Some((key, val, method))
}

/// Parse a Set/HashSet HOF name like `Set__int64_t__fold` → ("int64_t", "fold").
/// Sets only have keys (no values), so the closure takes (acc, key) or (key).
/// `filter` is excluded — migrated to `Inst::HofExpand` and handled by
/// the BIR expansion, not LLVM's inline dispatch.
fn parse_set_hof(name: &str) -> Option<(&str, &str)> {
    let prefix = if name.starts_with("Set__") { "Set__" }
        else if name.starts_with("HashSet__") { "HashSet__" }
        else { return None; };
    let rest = name.strip_prefix(prefix)?;
    let sep = rest.rfind("__")?;
    let method = &rest[sep + 2..];
    match method {
        "fold" | "each" | "any" | "all" => {}
        _ => return None,
    }
    let elem = &rest[..sep];
    Some((elem, method))
}

/// Parse an Option/Result combinator name like `Option__int64_t__map` → ("Option__int64_t", "map").
/// Returns None if the name is not a recognised Option/Result combinator.
fn parse_option_result_combinator(name: &str) -> Option<(&str, &str)> {
    const OPT_COMB: &[&str] = &[
        "map", "filter", "and_then", "or_else", "unwrap_or_else", "flat_map", "or", "flatten", "zip",
    ];
    const RES_COMB: &[&str] = &[
        "map", "map_err", "and_then", "or_else", "unwrap_err", "unwrap_error",
    ];
    if name.starts_with("Option__") {
        let rest = name.strip_prefix("Option__")?;
        let sep = rest.rfind("__")?;
        let method = &rest[sep + 2..];
        if OPT_COMB.contains(&method) || RES_COMB.contains(&method) {
            return Some((&name[..name.len() - method.len() - 2], method));
        }
    }
    if name.starts_with("Result__") {
        let rest = name.strip_prefix("Result__")?;
        let sep = rest.rfind("__")?;
        let method = &rest[sep + 2..];
        if RES_COMB.contains(&method) || OPT_COMB.contains(&method) {
            return Some((&name[..name.len() - method.len() - 2], method));
        }
    }
    None
}

/// Resolve the StructId for a type prefix (e.g., "Option__int64_t") → StructId.
fn find_struct_by_prefix(prefix: &str, module: &LirModule) -> Option<StructId> {
    module.structs.iter().enumerate()
        .find(|(_, d)| d.name == prefix)
        .map(|(i, _)| StructId(i as u32))
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

/// Emit an inter-field / trailing struct padding member of `pad` bytes.
///
/// **Must use an integer scalar (`iN`) for power-of-two sizes, NOT `[N x i8]`.**
/// On x86-64 SysV, LLVM 18's aggregate-ABI classifier treats a struct field of
/// array type `[N x i8]` as forcing the whole struct toward the MEMORY class —
/// so a 16-byte enum like `Option__Box__Expr` emitted as `{ i32, [4 x i8], ptr }`
/// is *returned via sret* (hidden pointer in RDI), while the C runtime declares
/// the same struct as `{ int32_t; void* }` and returns it in registers (RAX:RDX).
/// That ABI mismatch makes the C callee read the sret slot pointer as its `__p`
/// argument → dereference a stack address → SIGABRT/SIGSEGV (only at `llc -O0`;
/// `-O2` happens to optimize the bad reload away). Padding with `i32`/`i64`/…
/// keeps each eightbyte INTEGER-classed, matching the C natural-padding layout.
/// Non-power-of-two gaps (rare; e.g. a 3-byte hole) fall back to `[N x i8]`.
fn llvm_struct_padding(pad: usize) -> String {
    match pad {
        1 => "i8".to_string(),
        2 => "i16".to_string(),
        4 => "i32".to_string(),
        8 => "i64".to_string(),
        _ => format!("[{pad} x i8]"),
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

/// Emit an LLVM int-width coercion when `src` and `dst` differ in bit
/// width. Picks `trunc` (narrowing) / `sext` (widening signed) / `zext`
/// (widening unsigned) based on `dst`'s signedness, and writes the cast
/// instruction to `lines` (caller decides whether to flush inline before
/// the consuming instruction or via a spill list). Returns the new
/// SSA name to reference in place of `%v{src_id}`. Returns `None` when
/// no coercion is needed (widths match) or types aren't both integers.
///
/// Centralises the trunc/sext/zext sequence used at `Inst::Call` /
/// `Inst::CallExtern` / `Term::Ret` / `Inst::CallClosure` widening
/// sites — all of which used to copy-paste the same op-picking logic.
fn emit_int_coerce(
    lines: &mut Vec<String>,
    cast_name: &str,
    src_id: u32,
    src: &LirType,
    dst: &LirType,
) -> Option<String> {
    if !src.is_integer() || !dst.is_integer() {
        return None;
    }
    let sb = int_bits(src);
    let db = int_bits(dst);
    if sb == db || sb == 0 || db == 0 {
        return None;
    }
    let from_ty = llvm_type(src);
    let to_ty = llvm_type(dst);
    let op = if sb > db {
        "trunc"
    } else if is_signed(dst) {
        "sext"
    } else {
        "zext"
    };
    lines.push(format!("  %{cast_name} = {op} {from_ty} %v{src_id} to {to_ty}"));
    Some(format!("%{cast_name}"))
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

/// Resolve a span to the panic-site (file, line, col) triple emitted into
/// runtime panic messages. None (synthetic instruction or absent file
/// info) returns the conventional `<unknown>:0:0` fallback. The filename
/// is returned raw — the LLVM-side caller may either bake it into a
/// pre-formatted message string (escaped via `llvm_escape_string` at the
/// interning site) or pass it as an argument to `@gorget_panic_at` via a
/// dedicated `@.str.N` global.
///
/// Mirrors `src/backend/c_lir/mod.rs::resolve_panic_loc`. The C-side
/// version pre-applies `escape_c_string` since its callers interpolate
/// directly into emitted C source; the LLVM backend escapes lazily so
/// the same filename can flow through both the baked-message and
/// runtime-arg paths without double-escaping.
fn resolve_panic_loc(
    span: Option<crate::span::Span>,
    file_infos: &[crate::span::FileInfo],
) -> (String, u32, u32) {
    if let Some(s) = span {
        if let Some((file, line, col)) = crate::span::offset_to_location(file_infos, s.start) {
            return (file.to_string(), line, col);
        }
    }
    ("<unknown>".to_string(), 0, 0)
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
#[allow(dead_code)]
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
        Inst::NullPtr { .. } | Inst::GlobalAddr { .. } => Some(LirType::Ptr),
        Inst::FuncAddr { .. } | Inst::NamedFuncAddr { .. } => Some(LirType::FuncRef),
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
        Inst::CallPtr { dst, ret_ty, .. } | Inst::CallByRef { dst, ret_ty, .. } => {
            // Return type is carried explicitly; fall back to i64 when the
            // legacy `Void` sentinel is present (older lowering paths).
            if dst.is_some() {
                if matches!(ret_ty, LirType::Void) {
                    Some(LirType::I64)
                } else {
                    Some(ret_ty.clone())
                }
            } else { None }
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
        LirType::I64 | LirType::U64 | LirType::F64 | LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef => 8,
        LirType::F32 => 4,
        LirType::Struct(sid) => {
            if let Some(def) = structs.get(sid.0 as usize) {
                // Trait-box layout: typed flag set at registration. Wins
                // over `computed_c_size` only when the registered size
                // (8) disagrees with the runtime-required {data, vtable}
                // layout (16 bytes). Held here defensively; current
                // registration emits 16 for trait boxes already.
                if def.is_trait_box {
                    return 16;
                }
                // 0-field opaque handle (TaskGroup, Socket, AtomicInt, …):
                // its gorget-visible decl has no fields, so `compute_struct_sizes`
                // leaves `computed_c_size == Some(0)`, but the runtime layout is
                // pointer-shaped (8). `emit_struct_types` (~`:969`) already emits
                // the 8-byte `opaque_runtime_size` for these via its
                // `def.fields.is_empty()` branch — honoring `computed_c_size`(0)
                // here would disagree, yielding `memcpy(_, _, i64 0)` and an
                // uninitialized handle (→ SIGSEGV). Typed (field-count) guard;
                // shares the `opaque_runtime_size` source of truth with
                // `emit_struct_types` (keep the two in sync — see that fn).
                if def.fields.is_empty() && def.computed_c_size == Some(0) {
                    if let Some(sz) = crate::lir::lower::types::opaque_runtime_size(&def.name) {
                        return sz;
                    }
                }
                // Typed read: `computed_c_size` is the canonical source of
                // truth, set at registration for runtime singletons
                // (GorgetArray = 64, GorgetMap = 152, GorgetClosure = 16,
                // …) and populated by `compute_struct_sizes()` for
                // user-defined structs / Box / Task / Guard at the end of
                // LIR lowering. Replaces a former unconditional
                // `opaque_runtime_size` short-circuit that fired for every
                // monomorphized prefix-matched name.
                if let Some(sz) = def.computed_c_size {
                    return sz;
                }
                // Pre-`compute_struct_sizes` fallback (and for the few
                // singletons whose registration leaves `computed_c_size`
                // None — e.g. GorgetRange).
                if let Some(sz) = crate::lir::lower::types::opaque_runtime_size(&def.name) {
                    return sz;
                }
                // Union-layout enums: LLVM type is { i32, i32, [N x i8] } =
                // 8 (tag + pad) + N (payload). Matches C ABI union layout.
                if def.is_union_layout {
                    let payload = enum_payload_size(def, structs, snames);
                    return 8 + payload;
                }
                // Sum fields with C alignment rules
                let mut total = 0usize;
                for (_, fty) in &def.fields {
                    let fsz = sizeof_lir_type(fty, structs, snames);
                    let align = crate::lir::lower::types::c_alignof_lir_type(fty, structs);
                    total = (total + align - 1) & !(align - 1);
                    total += fsz;
                }
                // Align total to struct's own alignment
                let struct_align = def.computed_c_align.unwrap_or(8);
                (total + struct_align - 1) & !(struct_align - 1)
            } else {
                8
            }
        }
        // Item 7e (Phase 1): resource sizes are fixed by the C runtime
        // ABI — defer to the runtime singleton size table.
        LirType::Resource { kind, .. } => match kind {
            crate::lir::ResourceKind::GorgetString => 32,
            crate::lir::ResourceKind::GorgetArray => 64,
            crate::lir::ResourceKind::GorgetMap => 152,
            // GorgetSet aliases GorgetMap layout.
            crate::lir::ResourceKind::GorgetSet => 152,
            crate::lir::ResourceKind::GorgetClosure => 16,
            // RefCounted handles are pointer-shaped.
            crate::lir::ResourceKind::RefCounted => 8,
        },
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
                // Trap / BoundsCheck / DivCheck / overflow arms used to
                // pre-intern shared `gorget: <msg>` constants here; the
                // panic-location pass (stack-traces phase 3) bakes the
                // per-site `file:line:col:` prefix into each message, so
                // the strings differ per call site and are interned lazily
                // in `emit_inst` instead. Late-added globals are emitted
                // after the function bodies — see `pre_intern_count` below.
            }
        }
    }
    // Pre-intern any `gorget_str_from_literal` StrLit args from module-level
    // globals. These are emitted as cap=0 rodata-view static initializers
    // (no runtime ctor call), so we need their `@.str.N` index resolved
    // before `emit_globals` runs.
    for g in &module.globals {
        // Recurse through struct / static-array-view globals so nested string-
        // literal views (R34 Track A) get their `@.str.N` interned too.
        intern_const_strs(&g.init, &g.ty, module, &mut str_globals);
    }

    // Track how many strings are pre-interned so we can detect late additions.
    let pre_intern_count = str_globals.strings.len();
    emit_string_globals(&mut out, &str_globals);

    // Global variables
    emit_globals(&mut out, module, &snames, &str_globals);

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
        // Opaque-runtime overrides — these structs cross the LLVM↔C ABI
        // boundary, and the gorget-visible struct (e.g. `struct File: int handle`,
        // 8 bytes) is a single-field "cover" for a wider C runtime struct (e.g.
        // `GorgetFile { FILE* handle; bool owned; }`, 16 bytes). LLVM must
        // declare the *runtime* layout, otherwise an `alloca %File` is short by
        // 8 bytes and the runtime's `gorget_file_open_try(... GorgetFile* out)`
        // store of `out->owned` walks off the alloca → SIGSEGV. Emit the
        // override here, before the field-driven path below sees the gorget
        // declaration. `def.fields.is_empty()` branch below catches the
        // truly-opaque (zero-field) form for completeness.
        if !def.is_union_layout && (name == "File" || name == "GorgetFile") {
            writeln!(out, "%{name} = type {{ ptr, i64 }}").unwrap();
            continue;
        }
        if def.is_union_layout {
            // Enum: { i32 tag, i32 pad, [payload_bytes x i8] }
            // Padding matches C ABI: union payload at 8-byte-aligned offset.
            let payload = enum_payload_size(def, &module.structs, snames);
            if payload > 0 {
                writeln!(out, "%{name} = type {{ i32, i32, [{payload} x i8] }}").unwrap();
            } else {
                writeln!(out, "%{name} = type {{ i32 }}").unwrap();
            }
        } else if def.fields.is_empty() {
            // Known opaque types whose C runtime layout is fixed. The shared
            // table lives in src/lir/lower/types.rs so all backends agree.
            // SHARED SOURCE OF TRUTH: `sizeof_lir_type` (~`:732`) mirrors this
            // 0-field/`opaque_runtime_size` branch for its byte-size; keep the
            // two in sync so the type decl (here) and the `memcpy` size (there)
            // never disagree on an opaque handle's layout. (A genuinely-empty
            // USER struct with no `opaque_runtime_size` entry still pads to
            // `{i8}`=1 here but `sizeof`s to 0 there — a pre-existing latent
            // inconsistency the field-count guard does not address.)
            if let Some(layout) = crate::lir::lower::types::opaque_runtime_layout(name) {
                // Structs that cross C ABI boundaries by value need their real
                // field types declared so AArch64's HFA / register-return rules
                // fire correctly (vs. the `[N x i8]` opaque-blob layout, which
                // forces sret/memory return and mismatches the C runtime ABI).
                let parts: Vec<String> = layout.iter().map(|t| llvm_type_full(t, snames)).collect();
                writeln!(out, "%{name} = type {{ {} }}", parts.join(", ")).unwrap();
            } else if let Some(sz) = crate::lir::lower::types::opaque_runtime_size(name) {
                // Specific layouts whose internal shape matters for GEP emission.
                if name.starts_with("Task__") {
                    writeln!(out, "%{name} = type {{ ptr, ptr }}").unwrap();
                } else if name == "File" || name == "GorgetFile" {
                    writeln!(out, "%{name} = type {{ ptr, i64 }}").unwrap();
                } else if name.starts_with("Box__")
                    && module.structs.iter().find(|s| s.name == *name)
                        .map_or(false, |s| s.is_trait_box)
                {
                    // Box[Trait] is C-typedef'd to <Trait>_TraitObj (16 bytes:
                    // { data ptr, vtable ptr }) — the opaque-runtime-size table
                    // says 8 (correct for Box[Concrete]), but trait boxes need
                    // the 16-byte TraitObj layout so memcpy/sizeof/by-value
                    // ABI all match the C runtime. Read the typed `is_trait_box`
                    // flag set at registration time (commit e5de1616).
                    writeln!(out, "%{name} = type {{ ptr, ptr }}").unwrap();
                } else if sz == 8 {
                    // Pointer-sized handle (TaskGroup, Socket, AtomicInt, …).
                    writeln!(out, "%{name} = type {{ ptr }}").unwrap();
                } else {
                    // Opaque byte buffer matching the C struct size. The LLVM
                    // backend never needs to GEP into these — they are passed
                    // around by pointer or memcpy'd as whole blobs.
                    writeln!(out, "%{name} = type {{ [{sz} x i8] }}").unwrap();
                }
            } else {
                // Other empty structs — use single byte padding
                writeln!(out, "%{name} = type {{ i8 }}").unwrap();
            }
        } else {
            // Emit struct fields with explicit inter-field padding to match C ABI.
            // C inserts padding between fields for alignment; LLVM named structs
            // may not when aggregate fields have lower apparent alignment than their
            // C alignment (e.g., %Json = {i32, i32, [N x i8]} has LLVM align 4
            // but C align 8 due to int64_t inside the union).
            // VTable structs carry function-pointer fields regardless of what the
            // LIR struct def says (the GIR types them as closures for call-site
            // abstraction, but at the ABI level they are bare `ptr`s — matching
            // the C backend's override in src/backend/c_lir/mod.rs).
            let is_vtable = name.ends_with("_VTable");
            let mut fields: Vec<String> = Vec::new();
            let mut offset = 0usize;
            for (_, fty) in &def.fields {
                let (field_llvm, fsz, c_align) = if is_vtable {
                    ("ptr".to_string(), 8usize, 8usize)
                } else if *fty == LirType::Void {
                    ("i8".to_string(), 1usize, 1usize)
                } else {
                    (llvm_type_full(fty, snames),
                     sizeof_lir_type(fty, &module.structs, snames),
                     crate::lir::lower::types::c_alignof_lir_type(fty, &module.structs))
                };
                let aligned_offset = (offset + c_align - 1) & !(c_align - 1);
                if aligned_offset > offset {
                    let pad = aligned_offset - offset;
                    // Integer-typed padding (not `[N x i8]`) so x86-64 SysV
                    // keeps each eightbyte INTEGER-classed and matches the C
                    // runtime's register-return ABI. See `llvm_struct_padding`.
                    fields.push(llvm_struct_padding(pad));
                    offset = aligned_offset;
                }
                fields.push(field_llvm);
                offset += fsz;
            }
            // Trailing padding to match C size (for runtime structs with hidden fields).
            // Skip for VTables — we've remapped closure fields to ptr, so the LIR-tracked
            // c_size (counting closures) no longer matches our bare-ptr emission.
            if !is_vtable {
                if let Some(c_size) = def.computed_c_size {
                    if c_size > offset {
                        let pad = c_size - offset;
                        fields.push(llvm_struct_padding(pad));
                    }
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

/// R34 Track A: render a const `LirGlobalInit` to an LLVM constant EXPRESSION
/// (type-prefixed, e.g. `%GorgetArray { … }`, `i64 5`, `%RuntimeFn { … }`).
/// RECURSIVE — nested structs, string views, and nested `StaticArrayView`s all
/// resolve here. `StaticArrayView`s need a NAMED backing constant (LLVM can't
/// take the address of an inline array in a constant initializer), so any such
/// definitions are appended to `aux` (emitted just before the referencing
/// global) and `aux_ctr` hands out unique `@.arrback.N` names.
///
/// The scalar / FuncAddr / str-view spellings are kept byte-identical to the
/// former inline `Struct`/`Extern` global arms so existing struct globals
/// (`Point`, `Vec3`, module string literals) emit unchanged.
fn llvm_const_value(
    init: &LirGlobalInit,
    ty: &LirType,
    module: &LirModule,
    snames: &HashMap<u32, String>,
    str_globals: &StrGlobals,
    aux: &mut String,
    aux_ctr: &mut usize,
) -> String {
    match init {
        LirGlobalInit::Zeroed => format!("{} zeroinitializer", llvm_type_full(ty, snames)),
        LirGlobalInit::FuncAddr(fid) => {
            let fname = c_func_name(&module.functions[fid.0 as usize].name);
            format!("ptr @{fname}")
        }
        LirGlobalInit::BoxDropAddr(inner) => format!("ptr @Box__{inner}__drop"),
        LirGlobalInit::Bytes(data) if data.len() <= 8 => llvm_scalar_bytes_const(data, ty),
        LirGlobalInit::Bytes(_) => format!("{} zeroinitializer", llvm_type_full(ty, snames)),
        LirGlobalInit::Extern { name, args } => {
            // Module-level / nested string literal → `%GorgetString` view into
            // the interned `@.str.N` rodata (cap=0, no alloc, no free).
            if crate::backend::c_lir::helpers::is_str_literal_view_init(name, args, ty, &module.structs) {
                if let (LirGlobalInitArg::StrLit(text), LirGlobalInitArg::Int(len)) = (&args[0], &args[1]) {
                    let idx = str_globals.get_index(text);
                    return format!("%GorgetString {{ ptr @.str.{idx}, i64 0, i64 {len}, ptr null }}");
                }
            }
            format!("{} zeroinitializer", llvm_type_full(ty, snames))
        }
        LirGlobalInit::Struct { struct_id, fields } => {
            llvm_struct_const(*struct_id, fields, module, snames, str_globals, aux, aux_ctr)
        }
        LirGlobalInit::StaticArrayView { elem_ty, elems } => {
            let elem_llvm = llvm_type_full(elem_ty, snames);
            let elem_size = sizeof_lir_type(elem_ty, &module.structs, snames);
            let data_ptr = if elems.is_empty() {
                // An empty array needs no backing constant — a zero-length view
                // never dereferences `.data`.
                "ptr null".to_string()
            } else {
                let n = *aux_ctr;
                *aux_ctr += 1;
                let elem_consts: Vec<String> = elems
                    .iter()
                    .map(|e| llvm_const_value(e, elem_ty, module, snames, str_globals, aux, aux_ctr))
                    .collect();
                writeln!(
                    aux,
                    "@.arrback.{n} = private constant [{} x {elem_llvm}] [ {} ]",
                    elems.len(),
                    elem_consts.join(", ")
                )
                .unwrap();
                format!("ptr @.arrback.{n}")
            };
            // %GorgetArray = { ptr data, i64 cap, i64 len, i64 elem_size, [32 x i8] rest }
            // cap = 0 marks the buffer non-owning; the trailing 32 bytes (alloc
            // + elem_drop/clone/materialize fn ptrs) are all NULL for a view.
            format!(
                "%GorgetArray {{ {data_ptr}, i64 0, i64 {}, i64 {elem_size}, [32 x i8] zeroinitializer }}",
                elems.len()
            )
        }
    }
}

/// Render a scalar `Bytes` field to an LLVM typed constant, matching the former
/// inline `Struct` field emitter (float bit-patterns; little-endian integer).
fn llvm_scalar_bytes_const(data: &[u8], ty: &LirType) -> String {
    match ty {
        LirType::F64 if data.len() == 8 => {
            let bits = u64::from_le_bytes([
                data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
            ]);
            format!("double 0x{bits:016X}")
        }
        LirType::F32 if data.len() == 4 => {
            let bits = f32::from_le_bytes([data[0], data[1], data[2], data[3]]) as f64;
            format!("float 0x{:016X}", bits.to_bits())
        }
        _ => {
            let mut val = 0i64;
            for (bi, &b) in data.iter().enumerate() {
                val |= (b as i64) << (bi * 8);
            }
            format!("{} {val}", llvm_type(ty))
        }
    }
}

/// Render a `Struct`/flat-enum const, mirroring `emit_struct_types`' field +
/// inter-field + trailing padding layout EXACTLY (same size/align helpers), so
/// the constant's field sequence matches the declared `%Name = type { … }`. A
/// mismatch is a hard LLVM verifier error (loud build failure, never silent
/// miscompile).
fn llvm_struct_const(
    struct_id: crate::lir::StructId,
    field_values: &[LirGlobalInit],
    module: &LirModule,
    snames: &HashMap<u32, String>,
    str_globals: &StrGlobals,
    aux: &mut String,
    aux_ctr: &mut usize,
) -> String {
    let def = &module.structs[struct_id.0 as usize];
    let name = &snames[&struct_id.0];
    let is_vtable = name.ends_with("_VTable");
    let mut parts: Vec<String> = Vec::new();
    let mut offset = 0usize;
    for (i, (_, fty)) in def.fields.iter().enumerate() {
        let (field_ty, fsz, c_align) = if is_vtable {
            (LirType::Ptr, 8usize, 8usize)
        } else if *fty == LirType::Void {
            (LirType::I8, 1usize, 1usize)
        } else {
            (
                fty.clone(),
                sizeof_lir_type(fty, &module.structs, snames),
                crate::lir::lower::types::c_alignof_lir_type(fty, &module.structs),
            )
        };
        let aligned_offset = (offset + c_align - 1) & !(c_align - 1);
        if aligned_offset > offset {
            parts.push(format!("{} zeroinitializer", llvm_struct_padding(aligned_offset - offset)));
            offset = aligned_offset;
        }
        let val = match field_values.get(i) {
            Some(v) => llvm_const_value(v, &field_ty, module, snames, str_globals, aux, aux_ctr),
            None => format!("{} zeroinitializer", llvm_type_full(&field_ty, snames)),
        };
        parts.push(val);
        offset += fsz;
    }
    if !is_vtable {
        if let Some(c_size) = def.computed_c_size {
            if c_size > offset {
                parts.push(format!("{} zeroinitializer", llvm_struct_padding(c_size - offset)));
            }
        }
    }
    format!("%{name} {{ {} }}", parts.join(", "))
}

/// Recursively intern every string-literal-view `StrLit` reachable from a
/// const global init, so `@.str.N` indices resolve before `emit_globals`.
fn intern_const_strs(init: &LirGlobalInit, ty: &LirType, module: &LirModule, str_globals: &mut StrGlobals) {
    match init {
        LirGlobalInit::Extern { name, args } => {
            if crate::backend::c_lir::helpers::is_str_literal_view_init(name, args, ty, &module.structs) {
                if let LirGlobalInitArg::StrLit(text) = &args[0] {
                    str_globals.intern(text);
                }
            }
        }
        LirGlobalInit::Struct { struct_id, fields } => {
            let def = module.structs.get(struct_id.0 as usize);
            for (i, f) in fields.iter().enumerate() {
                let fty = def
                    .and_then(|d| d.fields.get(i).map(|(_, t)| t.clone()))
                    .unwrap_or(LirType::I64);
                intern_const_strs(f, &fty, module, str_globals);
            }
        }
        LirGlobalInit::StaticArrayView { elem_ty, elems } => {
            for e in elems {
                intern_const_strs(e, elem_ty, module, str_globals);
            }
        }
        _ => {}
    }
}

fn emit_globals(out: &mut String, module: &LirModule, snames: &HashMap<u32, String>, str_globals: &StrGlobals) {
    let mut aux_ctr = 0usize;
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
            LirGlobalInit::BoxDropAddr(inner) => {
                writeln!(out, "@__lir_g{i} = {linkage} ptr @Box__{inner}__drop ; {}", global.name).unwrap();
            }
            LirGlobalInit::Struct { struct_id, fields } => {
                let sdef = &module.structs[struct_id.0 as usize];
                // Use named struct only if field count matches; otherwise anonymous
                let use_named = fields.len() == sdef.fields.len() && !sdef.fields.is_empty();
                if use_named {
                    // R34 Track A: route the well-formed case through the
                    // recursive const emitter — it mirrors `emit_struct_types`'
                    // ABI padding and recurses into nested structs / string
                    // views / static-array-view fields (the former inline arm
                    // zero-inited those). Byte-identical for existing padding-
                    // free scalar struct globals.
                    let mut aux = String::new();
                    let cv = llvm_const_value(
                        &global.init, &global.ty, module, snames, str_globals, &mut aux, &mut aux_ctr,
                    );
                    out.push_str(&aux); // backing constants precede the global that references them
                    writeln!(out, "@__lir_g{i} = {linkage} {cv} ; {}", global.name).unwrap();
                    continue;
                }
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
                    let field_lir = if use_named && fi < sdef.fields.len() {
                        Some(&sdef.fields[fi].1)
                    } else {
                        None
                    };
                    let fty = if let Some(t) = field_lir {
                        llvm_type_full(t, snames)
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
                        // Trait-object vtable drop slot: the C wrapper glue
                        // (generate_llvm_wrappers → emit_box_drop_wrappers)
                        // defines `Box__<inner>__drop`; declared below in
                        // `emit_box_drop_decls`.
                        LirGlobalInit::BoxDropAddr(inner) => {
                            format!("ptr @Box__{inner}__drop")
                        }
                        LirGlobalInit::Bytes(data) if data.len() <= 8 => {
                            // Bytes targeting a float field: use LLVM's hex
                            // bit-pattern syntax. `double 1.5` is invalid
                            // syntax at module scope; `double 0x3FF8000000000000`
                            // is. (Compile-time `Struct`-with-float-fields
                            // shows up after the compound-literal lift: e.g.
                            // `static Vec3 v = Vec3(1.0, 2.0, 3.0)` lowers to
                            // a `Struct` with three `Bytes(f64.to_le_bytes())`
                            // fields.)
                            match field_lir {
                                Some(LirType::F64) if data.len() == 8 => {
                                    let bits = u64::from_le_bytes([
                                        data[0], data[1], data[2], data[3],
                                        data[4], data[5], data[6], data[7],
                                    ]);
                                    format!("double 0x{bits:016X}")
                                }
                                Some(LirType::F32) if data.len() == 4 => {
                                    // f32 globals are emitted with i64-shaped
                                    // hex constants — LLVM accepts a `double`-
                                    // shaped hex even for `float` fields when
                                    // it's representable exactly. Use the
                                    // double-precision representation of the
                                    // f32 bit-pattern.
                                    let bits = f32::from_le_bytes([
                                        data[0], data[1], data[2], data[3],
                                    ]) as f64;
                                    format!("float 0x{:016X}", bits.to_bits())
                                }
                                _ => {
                                    let mut val = 0i64;
                                    for (bi, &b) in data.iter().enumerate() {
                                        val |= (b as i64) << (bi * 8);
                                    }
                                    format!("{fty} {val}")
                                }
                            }
                        }
                        _ => format!("{fty} zeroinitializer"),
                    }
                }).collect();
                writeln!(out, "@__lir_g{i} = {linkage} {sty} {{ {} }} ; {}",
                    field_vals.join(", "), global.name).unwrap();
            }
            LirGlobalInit::Extern { name, args } => {
                // Module-level string literal: `String FOO = "literal"` lowers
                // to a `gorget_str_from_literal(StrLit, Int)` ctor call. Emit
                // it as a static `%GorgetString` initializer that points at
                // the interned `@.str.N` rodata buffer (cap=0 view, no alloc,
                // no free). Mirrors the C backend — see
                // `c_lir::helpers::is_str_literal_view_init`.
                if crate::backend::c_lir::helpers::is_str_literal_view_init(
                    name, args, &global.ty, &module.structs,
                ) {
                    if let (LirGlobalInitArg::StrLit(text), LirGlobalInitArg::Int(len)) = (&args[0], &args[1]) {
                        let idx = str_globals.get_index(text);
                        writeln!(
                            out,
                            "@__lir_g{i} = {linkage} %GorgetString {{ ptr @.str.{idx}, i64 0, i64 {len}, ptr null }} ; {}",
                            global.name
                        ).unwrap();
                        continue;
                    }
                }
                // Runtime-initialized globals are populated by a call
                // emitted at main()'s prologue (`emit_global_runtime_init`).
                // The declaration zero-inits the slot.
                writeln!(out, "@__lir_g{i} = {linkage} {ty} zeroinitializer ; {} (runtime init)", global.name).unwrap();
            }
            LirGlobalInit::StaticArrayView { .. } => {
                // R34 Track A: a `cap = 0` %GorgetArray view over a named
                // backing constant (`@.arrback.N`, emitted first). Recursive —
                // struct/string/nested-array elements resolve via
                // `llvm_const_value`.
                let mut aux = String::new();
                let cv = llvm_const_value(
                    &global.init, &global.ty, module, snames, str_globals, &mut aux, &mut aux_ctr,
                );
                out.push_str(&aux);
                writeln!(out, "@__lir_g{i} = {linkage} {cv} ; {}", global.name).unwrap();
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
    "gorget_panic", "gorget_panic_at", "gorget_trap", "gorget_trap_at", "gorget_init_args",
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
    // `gorget_panic_at(file, line, col, msg)` is the location-aware panic
    // entry point. Compiler-emit `gorget_panic` call sites are rewritten
    // to `gorget_panic_at` in the CallExtern handler so the runtime
    // message carries `file:line:col`. Mirrors the C-backend declaration
    // at `src/backend/c/c_runtime.rs` (PANIC_NORMAL / test variant).
    writeln!(out, "declare void @gorget_panic_at(ptr, i32, i32, ptr)").unwrap();
    // Trap normalization (D11): `gorget_trap_at(code, detail, file, line, col)`
    // emits the normative `trap[<code>]: <detail> at file:line:col` + exit 101.
    // The arg order is code-first (NOT gorget_panic_at's file-first order).
    // `gorget_trap` is the span-less form; both are also runtime-defined in
    // panic_normal.c / panic_test.c.
    writeln!(out, "declare void @gorget_trap(ptr, ptr)").unwrap();
    writeln!(out, "declare void @gorget_trap_at(ptr, ptr, ptr, i32, i32)").unwrap();
    // Trace runtime — declared unconditionally; the linker drops unused
    // declares. Provides the envelope emitters used at function entry,
    // block entry/exit, and branch/return points when `directive trace`
    // or `--trace` is active. Defined in `c_runtime::TRACE_RUNTIME`,
    // included in the LLVM build's runtime .c when `module.trace_filename`
    // is set (`src/main.rs::compile_llvm_pipeline`).
    if module.trace_filename.is_some() {
        writeln!(out, "declare void @__gorget_trace_init(ptr)").unwrap();
        writeln!(out, "declare void @__gorget_trace_emit_call_begin(ptr)").unwrap();
        writeln!(out, "declare void @__gorget_trace_emit_call_end()").unwrap();
        writeln!(out, "declare void @__gorget_trace_emit_arg_int(ptr, i32, i64)").unwrap();
        writeln!(out, "declare void @__gorget_trace_emit_arg_float(ptr, i32, double)").unwrap();
        writeln!(out, "declare void @__gorget_trace_emit_arg_bool(ptr, i32, i32)").unwrap();
        writeln!(out, "declare void @__gorget_trace_emit_arg_str(ptr, i32, ptr)").unwrap();
        writeln!(out, "declare void @__gorget_trace_emit_stmt_start()").unwrap();
        writeln!(out, "declare void @__gorget_trace_emit_stmt_end()").unwrap();
        writeln!(out, "declare void @__gorget_trace_emit_branch()").unwrap();
        writeln!(out, "declare void @__gorget_trace_emit_return(ptr)").unwrap();
    }
    // String comparison — declared with precise C ABI return types.
    // gorget_str_eq returns bool (i1), gorget_str_cmp returns int (i32, NOT i64!).
    // On aarch64, 'mov w0, -1' zero-extends to x0=0xFFFFFFFF; must call as i32 + sext.
    // Listed in LIBC_BUILTINS so module.externs never re-declares them with wrong types.
    // Both take `Str` by value (32-byte struct); on x86_64 SysV that's memory
    // class — annotate the declaration to match the C ABI gcc/clang compile.
    let str_byval_attr = gorget_string_byval_attr(&snames);
    let str_byval = str_byval_attr.trim_end();
    let str_param = if str_byval.is_empty() { "ptr".to_string() } else { format!("ptr {str_byval}") };
    writeln!(out, "declare i1 @gorget_str_eq({sp}, {sp})", sp = str_param).unwrap();
    writeln!(out, "declare i32 @gorget_str_cmp({sp}, {sp})", sp = str_param).unwrap();
    // Runtime collection free/clone_inplace/materialize_inplace — always declared because
    // NamedFuncAddr instructions reference them for elem_drop/elem_clone fields.
    writeln!(out, "declare void @gorget_string_free(ptr)").unwrap();
    writeln!(out, "declare void @gorget_array_free(ptr)").unwrap();
    writeln!(out, "declare void @gorget_map_free(ptr)").unwrap();
    writeln!(out, "declare void @gorget_set_free(ptr)").unwrap();
    writeln!(out, "declare void @gorget_array_clone_inplace(ptr)").unwrap();
    writeln!(out, "declare void @gorget_map_clone_inplace(ptr)").unwrap();
    writeln!(out, "declare void @gorget_set_clone_inplace(ptr)").unwrap();
    writeln!(out, "declare void @gorget_string_clone_inplace(ptr)").unwrap();
    writeln!(out, "declare void @gorget_string_materialize_inplace(ptr)").unwrap();
    // Closure resource ops — landed with the Vector[Callable] deep-clone work
    // (commit 58396fca). Like the string/collection inplace pairs, these are
    // referenced through `NamedFuncAddr` for elem_drop / elem_clone slots, so
    // the LLVM IR needs the bare declare even when no LirExtern carries them.
    writeln!(out, "declare void @gorget_closure_free(ptr)").unwrap();
    writeln!(out, "declare void @gorget_closure_clone_inplace(ptr)").unwrap();
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
    writeln!(out, "declare void @gorget_bool_to_str(ptr sret(%GorgetString), i1 zeroext)").unwrap();
    writeln!(out, "declare ptr @malloc(i64)").unwrap();
    writeln!(out, "declare void @free(ptr)").unwrap();
    // String push variants for gorget_str_push type dispatch
    writeln!(out, "declare void @gorget_string_push_int(ptr, i64)").unwrap();
    writeln!(out, "declare void @gorget_string_push_float(ptr, double)").unwrap();
    writeln!(out, "declare void @gorget_string_push_bool(ptr, i1 zeroext)").unwrap();
    // gorget_string_push_char(GorgetString* s, Str c) — second arg is Str by value.
    writeln!(out, "declare void @gorget_string_push_char(ptr, {sp})", sp = str_param).unwrap();
    writeln!(out, "declare void @gorget_string_push_line_int(ptr, i64)").unwrap();
    writeln!(out, "declare void @gorget_string_push_line_float(ptr, double)").unwrap();
    writeln!(out, "declare void @gorget_string_push_line_bool(ptr, i1 zeroext)").unwrap();
    writeln!(out, "declare void @gorget_string_push_line(ptr, ptr)").unwrap();
    writeln!(out, "declare void @exit(i32) noreturn").unwrap();
    // Dict/Map/Set HOF helpers for filter inline expansion
    writeln!(out, "declare void @gorget_map_put_cloned(ptr, ptr, ptr)").unwrap();
    writeln!(out, "declare void @gorget_set_add(ptr, ptr)").unwrap();
    // gorget_task_group_submit_raw: the real function behind the gorget_task_group_submit macro.
    // gorget_task_group_submit is a C macro — replaced inline with calls to _raw.
    writeln!(out, "declare void @gorget_task_group_submit_raw(ptr, ptr, ptr)").unwrap();
    // Parse helpers
    // gorget_try_parse_int/float take (const char* s, i64 len) and return
    // a 16-byte struct {value, ok}. C pads bool to 8 bytes for alignment,
    // so use {i64, i64} / {double, i64} to match the C ABI on aarch64.
    writeln!(out, "declare {{i64, i64}} @gorget_try_parse_int(ptr, i64)").unwrap();
    writeln!(out, "declare {{double, i64}} @gorget_try_parse_float(ptr, i64)").unwrap();
    // gorget_str_to_cstr takes Str by value (32 bytes) — see str_byval handling above.
    writeln!(out, "declare ptr @gorget_str_to_cstr({sp})", sp = str_param).unwrap();
    // CStr → GorgetString wrapping (for extern "C" return values)
    if !module.externs.iter().any(|e| e.name == "gorget_str_from_cstr") {
        writeln!(out, "declare void @gorget_str_from_cstr(ptr sret(%GorgetString), ptr)").unwrap();
    }
    // gorget_string_clone_to_owned: used by gorget_str_str inline expansion
    writeln!(out, "declare void @gorget_string_clone_to_owned(ptr sret(%GorgetString), ptr)").unwrap();
    // gorget_string_copy_cow: CoW-aware copy used by non-move SlotStore of Ptr → String slot.
    // Views (cap=0) are struct-copied (zero alloc); owned strings (cap>0) are deep-cloned.
    // Mirrors the C backend's Copy-semantic SlotStore path at src/backend/c_lir/mod.rs.
    writeln!(out, "declare void @gorget_string_copy_cow(ptr sret(%GorgetString), ptr)").unwrap();
    // gorget_channel_recv_timeout: used by the Channel__T__recv_timeout intercept
    // in emit_inst (inlines the Option-wrapping). Declare once here unconditionally
    // — if the call site isn't reached the dead declaration is harmless.
    if !module.externs.iter().any(|e| e.name == "gorget_channel_recv_timeout") {
        writeln!(out, "declare i32 @gorget_channel_recv_timeout(ptr, ptr, i64)").unwrap();
    }
    // Names of functions DEFINED in this module (skip forward declarations).
    // Hoisted above the `has_runtime_init` block so the runtime-init extern-decl
    // guard (~`:1410`) can skip locally-defined synthesized fns (e.g.
    // `__gg_static_init_*`) — those get a real `define`, so a `declare` here
    // would be an `llc: invalid redefinition`. Also reused by the
    // `module.externs` loop (~`:1481`, `:1650`). Single source of truth.
    let defined_fns: std::collections::HashSet<&str> = module.functions.iter()
        .map(|f| f.name.as_str())
        .collect();
    // Runtime constructors — used for static global initialization
    // (LirGlobalInit::Extern). Only declare if not already in module.externs
    // or already declared above in hof_decls.
    let has_runtime_init = module.globals.iter().any(|g| matches!(&g.init, crate::lir::LirGlobalInit::Extern { .. }));
    // Track exactly which runtime-init ctors we emitted here, so the
    // `module.externs` loop below (seen-set seeding) can skip precisely
    // those — never more, never fewer. This is the single source of truth
    // for which runtime-init decls exist. The old code hardcoded a 4-name
    // subset ({map,dict}_new[_str]) into the seen-block, which BOTH
    // double-counted (those four were already emitted unconditionally only
    // when absent from externs) and silently MISSED set_new/ordered_set_new
    // — a `static Dict` init that ALSO appears in module.externs (a real
    // `Dict()`/`.put` call references it) hit BOTH skip guards and got
    // declared nowhere → `llc: use of undefined value '@gorget_dict_new'`.
    let mut runtime_init_seen: std::collections::HashSet<&'static str> = std::collections::HashSet::new();
    if has_runtime_init {
        let hof_names: std::collections::HashSet<&str> = hof_decls.iter().map(|(n, _)| *n).collect();
        let runtime_init_fns: &[(&'static str, &'static str)] = &[
            ("gorget_array_new",      "declare void @gorget_array_new(ptr sret(%GorgetArray), i64)"),
            ("gorget_array_extend",   "declare void @gorget_array_extend(ptr, ptr)"),
            ("gorget_map_new",        "declare void @gorget_map_new(ptr sret(%GorgetMap), i64, i64)"),
            ("gorget_map_new_str",    "declare void @gorget_map_new_str(ptr sret(%GorgetMap), i64)"),
            ("gorget_dict_new",       "declare void @gorget_dict_new(ptr sret(%GorgetMap), i64, i64)"),
            ("gorget_dict_new_str",   "declare void @gorget_dict_new_str(ptr sret(%GorgetMap), i64)"),
            ("gorget_set_new",        "declare void @gorget_set_new(ptr sret(%GorgetSet), i64)"),
            ("gorget_set_new_str",    "declare void @gorget_set_new_str(ptr sret(%GorgetSet))"),
            // Ordered Set ctors (`Set[T]()` / `Set[String]()`) — used by
            // static `Set` inits (`eval_static_init` Set arm) and any
            // `Set()` constructor. Mirror the unordered set_new signatures.
            ("gorget_ordered_set_new",     "declare void @gorget_ordered_set_new(ptr sret(%GorgetSet), i64)"),
            ("gorget_ordered_set_new_str", "declare void @gorget_ordered_set_new_str(ptr sret(%GorgetSet))"),
            // String-from-literal: takes (const char* data, size_t len) and
            // returns a 32-byte GorgetString via sret. Used by
            // `static String s = "..."` inits.
            ("gorget_str_from_literal", "declare void @gorget_str_from_literal(ptr sret(%GorgetString), ptr, i64)"),
        ];
        // Emit each runtime-init decl REGARDLESS of `existing_externs` (the
        // canonical hardcoded decl is the single source of truth — these are
        // de-staticified to link-visible symbols, see `src/main.rs`). Keep the
        // `!hof_names` exclusion: gorget_array_new/array_extend/set_new/
        // set_new_str live in BOTH hof_decls and runtime_init_fns, and
        // hof_decls already emitted them above → emitting here too = double-decl.
        for (fn_name, decl) in runtime_init_fns {
            if !hof_names.contains(*fn_name) {
                writeln!(out, "{decl}").unwrap();
                runtime_init_seen.insert(*fn_name);
            }
        }
        let existing_externs: std::collections::HashSet<&str> = module.externs.iter().map(|e| e.name.as_str()).collect();
        // Declare any other extern referenced in `Extern { name, args }`
        // (gorget_mutex_new, gorget_atomic_int_new, gorget_str_from_literal,
        // …). Default signature: `i64 fn(...args)` — backends downstream
        // override return types via `ext.return_type` when there's a
        // matching LirExtern.
        let known_init_fns: std::collections::HashSet<&str> = runtime_init_fns.iter().map(|(n, _)| *n).collect();
        for global in &module.globals {
            if let crate::lir::LirGlobalInit::Extern { name, args } = &global.init {
                if !existing_externs.contains(name.as_str())
                    && !hof_names.contains(name.as_str())
                    && !known_init_fns.contains(name.as_str())
                    // A LOCALLY-DEFINED synthesized init (`__gg_static_init_*`,
                    // for a static with a literal/aggregate initializer) gets a
                    // real `define` later — declaring it here too is an
                    // `llc: invalid redefinition`. Typed guard (is it in
                    // `module.functions`?), not a name test on `__gg_static_init_`.
                    && !defined_fns.contains(name.as_str())
                {
                    let n_args = args.len();
                    let params: Vec<&str> = (0..n_args).map(|_| "i64").collect();
                    // Return type is the GLOBAL's own type (the single source of
                    // truth for the scalar init's ABI), NOT a hardcoded i64.
                    // A `float`-returning runtime ctor (`gorget_math_infinity`,
                    // `gorget_math_nan`) declared as `i64` is read from the wrong
                    // register on x86_64 SysV (rax vs xmm0) → garbage 0.0; a
                    // pointer-sized integer handle keeps `i64`. Aggregate globals
                    // never reach here (sret-routed via known_init_fns above).
                    let ret = llvm_type_full(&global.ty, snames);
                    writeln!(out, "declare {ret} @{name}({})", params.join(", ")).unwrap();
                }
            }
        }
    }
    writeln!(out, "@stderr = external global ptr").unwrap();
    writeln!(out).unwrap();

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
        // Runtime free/clone/materialize — declared in preamble for NamedFuncAddr references
        "gorget_string_free", "gorget_array_free", "gorget_map_free", "gorget_set_free",
        "gorget_array_clone_inplace", "gorget_map_clone_inplace", "gorget_set_clone_inplace",
        "gorget_string_clone_inplace", "gorget_string_materialize_inplace",
        "gorget_closure_free", "gorget_closure_clone_inplace",
        // HOF helpers declared in preamble
        "gorget_map_put_cloned", "gorget_set_add",
    ] {
        seen.insert(name.to_string());
    }
    if !module.externs.iter().any(|e| e.name == "gorget_str_from_cstr") {
        seen.insert("gorget_str_from_cstr".to_string());
    }
    // Skip every runtime-init ctor we actually emitted above — driven off
    // `runtime_init_seen` (the single source of truth), NOT a hardcoded
    // subset. The old 4-name list ({map,dict}_new[_str]) silently missed
    // set_new/ordered_set_new and, combined with the now-removed
    // `existing_externs` guard on the emit loop, let a ctor that was BOTH a
    // static-init AND a module.externs reference get skipped in both places
    // → `llc: undefined value`. (hof-owned names — array_new/array_extend/
    // set_new/set_new_str — are NOT in `runtime_init_seen`; they are already
    // seeded via the `hof_decls` filter at the top of this block.)
    for fn_name in &runtime_init_seen {
        seen.insert(fn_name.to_string());
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
        if ext.name.starts_with("__callable_") || ext.name.starts_with("__gorget_closure_call_")  {
            // These are now CallClosure instructions, but ensure_extern may still register them.
            continue;
        }
        // Skip Option/Result combinator methods (inlined at each call site)
        if parse_option_result_combinator(&ext.name).is_some() {
            continue;
        }
        // Skip Dict/Set HOF methods (inlined at each call site)
        if parse_dict_hof(&ext.name).is_some() || parse_set_hof(&ext.name).is_some() {
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
        // (Removed: gorget_regex_find_at alias — xtd.regex is now pure Gorget,
        // no PCRE2 externs.)
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
        // __gorget_box_alloc_<inner>: the C runtime defines these as
        // `void* __gorget_box_alloc_<inner>(<inner> val)` — the inner type
        // is passed BY VALUE regardless of how the LIR registered the
        // single param (typically Ptr because the SSA operand holds a
        // pointer to the inner). On x86_64 SysV the C ABI puts >16-byte
        // aggregates as memory-class on the outgoing stack frame; without
        // a byval(...) override, llc emits a pointer-in-register and the
        // C side reads garbage. Resolve the suffix → struct id and treat
        // the param as the actual inner type for ABI purposes.
        //
        // Primitive inners (`__gorget_box_alloc_int64_t`, `_double`, etc.)
        // don't match a struct in `snames`, so this lookup returns None and
        // the standard scalar-spill path at the call site (`expects_ptr &&
        // !is_ptr`) handles them. Only struct inners need the override.
        let box_alloc_inner: Option<u32> = ext.name
            .strip_prefix("__gorget_box_alloc_")
            .and_then(|suffix| struct_sid_by_name(snames, suffix));
        let params: Vec<String> = ext.params.iter().enumerate()
            .map(|(i, p)| {
                // Void params are invalid in LLVM — replace with ptr (typically closure env)
                if *p == LirType::Void { return "ptr".to_string(); }
                // Box allocator: override the LIR param type with the suffix-derived
                // struct type so the byval/small-agg branches below see the real ABI.
                let p_owned;
                let p: &LirType = if let (0, Some(sid)) = (i, box_alloc_inner) {
                    p_owned = LirType::Struct(StructId(sid));
                    &p_owned
                } else { p };
                // Honor AbiKind::Ptr from the extern declaration (set by the
                // GIR lowerer when the Gorget extern uses `T*` pointer
                // syntax). The LIR keeps the type as the inner struct (for
                // type-system consistency), but the actual C ABI takes a
                // pointer-to-struct. Without this override the LLVM declares
                // the param as `%Struct` by-value while the runtime expects
                // `ptr`, mismatching x0 register layout on the call.
                let param_abi = ext.param_abis.get(i).copied().unwrap_or_default();
                if param_abi == crate::ir::abi::AbiKind::Ptr && p.is_aggregate() {
                    return "ptr".to_string();
                }
                // CStr: the C runtime takes a bare `const char*`, NOT a 32-byte
                // Str-by-value. The matching call site already marshals via
                // gorget_str_to_cstr → a raw `ptr` (`is_str_to_cstr`). Without
                // this branch a CStr-tagged aggregate param falls through to the
                // is_aggregate() byval branch below, so on x86_64 SysV the declare
                // byval's 32 bytes while the call passes a pointer → the runtime
                // reads garbage/empty. Bare `ptr` on both arches (aarch64 already
                // emitted bare ptr via the empty byval attr — why this was x86_64-only).
                if param_abi == crate::ir::abi::AbiKind::CStr {
                    return "ptr".to_string();
                }
                // AbiKind::GorgetString or ByValue: the C side takes a Str
                // (32-byte aggregate) by value, even though the LIR type may
                // be Ptr (the SSA operand is the address of a strlit struct).
                // Without forcing the byval treatment here, x86_64 SysV
                // emits a bare pointer-in-register and the C side reads the
                // struct from the wrong location. For Gorget extern blocks,
                // `string_abi=GorgetString` is set on every String param, so
                // this is the path most user-defined externs take.
                let force_str_byval = matches!(param_abi,
                    crate::ir::abi::AbiKind::GorgetString | crate::ir::abi::AbiKind::ByValue
                );
                if force_str_byval && struct_sid_by_name(snames, "GorgetString").is_some() {
                    // On x86_64 the helper produces `byval(%GorgetString) align 8 `;
                    // on other targets it returns "" and we emit bare `ptr` (matches AAPCS64).
                    let attr = gorget_string_byval_attr(snames);
                    return format!("ptr {attr}").trim_end().to_string();
                }
                // Aggregate params: small structs (≤16 bytes) pass in registers (aarch64 ABI),
                // large structs (>16 bytes) pass by indirect reference (ptr).
                // On x86_64 SysV the C ABI for large aggregates by value is a
                // memory-class copy on the outgoing stack frame, *not* a
                // register pointer — annotate with `byval` so llc lowers it to
                // the right convention. aarch64 stays bare `ptr` (AAPCS64
                // implicit-pointer-in-register matches the existing emission).
                if p.is_aggregate() {
                    if is_small_aggregate(p, &module.structs) {
                        return llvm_type_full(p, snames);
                    }
                    return format!("ptr {}", large_agg_byval_attr(p, snames)).trim_end().to_string();
                }
                // Scalar params: `bool` must carry `zeroext` at the C-ABI
                // boundary (see llvm_c_param_type) — a bare `i1` reads garbage
                // upper bits on the C side.
                llvm_c_param_type(p, snames)
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
                    if name.starts_with("__callable_") || name.starts_with("__gorget_closure_call_")  {
                        // These are now CallClosure instructions, but ensure_extern may still register them.
                        continue;
                    }
                    // Skip Option/Result combinator methods (inlined at each call site)
                    if parse_option_result_combinator(&name).is_some() {
                        continue;
                    }
                    // Skip Dict/Set HOF methods (inlined at each call site)
                    if parse_dict_hof(&name).is_some() || parse_set_hof(&name).is_some() {
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
                    } else if matches!(name.as_str(),
                        "gorget_str_trim" | "gorget_str_lstrip_ws" | "gorget_str_rstrip_ws"
                    ) {
                        // No-arg strip family: the LIR registers `gorget_str_strip` /
                        // `gorget_str_lstrip` / `gorget_str_rstrip` (2-arg sigs from
                        // RuntimeFn::resolve_lir_sig) but the codegen renames to the `_ws`
                        // variants for 1-arg calls. The renamed-target name has no
                        // module.externs entry, so we land here. The C runtime defines
                        // these as `Str f(Str s)` — 1 Str arg by value (byval on x86_64)
                        // and Str return via sret (32-byte aggregate).
                        let str_attr = gorget_string_byval_attr(snames);
                        writeln!(out, "declare void @{name}(ptr sret(%GorgetString), ptr {str_attr})").unwrap();
                    } else {
                        // Truly unknown — no LirExtern declared and not a locally-defined
                        // function. By design every CallExtern should have a matching
                        // LirExtern registered via ensure_extern (or be in LIBC_BUILTINS /
                        // the preamble). Anything reaching here is a forgotten registration
                        // upstream; declare a minimal void/ptr signature and move on.
                        let params: Vec<String> = args.iter().map(|_| "ptr".to_string()).collect();
                        let ret = if dst.is_some() { "ptr" } else { "void" };
                        writeln!(out, "declare {ret} @{name}({})", params.join(", ")).unwrap();
                    }
                }
            }
        }
    }

    // Forward-declare the synthetic `__gorget_ktable_hash__T` /
    // `__gorget_ktable_eq__T` bridges referenced by `SetCollectionBridge`
    // insts. Bodies live in the linked C runtime (emitted by
    // `c_lir::emit_hashable_key_bridges`); LLVM only needs the
    // address-of declarations for the `store ptr @bridge, ...` wiring.
    // Hash signature: `uint64_t (const void*)`. Eq: `bool (const void*,
    // const void*)`.
    let mut bridge_keys: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::SetCollectionBridge { key_struct, .. } = inst {
                    let name = &module.structs[key_struct.0 as usize].name;
                    bridge_keys.insert(name.clone());
                }
            }
        }
    }
    for key in &bridge_keys {
        writeln!(out, "declare i64 @__gorget_ktable_hash__{key}(ptr)").unwrap();
        writeln!(out, "declare i1 @__gorget_ktable_eq__{key}(ptr, ptr)").unwrap();
    }

    // Forward-declare the `Box__<inner>__drop` wrappers referenced by
    // trait-object vtable drop slots (`LirGlobalInit::BoxDropAddr`). Bodies
    // live in the linked C wrapper glue (`generate_llvm_wrappers` →
    // `emit_box_drop_wrappers`); LLVM only needs the address-of declaration
    // for the `ptr @Box__<inner>__drop` vtable field. Skip names already in
    // `module.externs` so we never double-declare.
    let mut box_drop_inners: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    fn collect_box_drops(init: &LirGlobalInit, acc: &mut std::collections::BTreeSet<String>) {
        match init {
            LirGlobalInit::BoxDropAddr(inner) => { acc.insert(inner.clone()); }
            LirGlobalInit::Struct { fields, .. } => {
                for f in fields { collect_box_drops(f, acc); }
            }
            _ => {}
        }
    }
    for g in &module.globals {
        collect_box_drops(&g.init, &mut box_drop_inners);
    }
    for inner in &box_drop_inners {
        let name = format!("Box__{inner}__drop");
        if module.externs.iter().any(|e| e.name == name) {
            continue;
        }
        writeln!(out, "declare void @{name}(ptr)").unwrap();
    }

    writeln!(out).unwrap();
}

// ── Global Runtime Init ───────────────────────────────────────────────────
//
// `LirGlobalInit::Extern { name, args }` is consumed directly here — no
// string parsing of C expressions, no name extraction. The `args` slice
// is iterated and each `LirGlobalInitArg` rendered to its LLVM IR form
// via `emit_global_init_arg_llvm`. Sizeof / AddrOfInline / StrLit each
// have their own materialization (constant fold, alloca-store, intern
// into the module-level `@.str.N` table respectively).

/// Pick the C-runtime arg-emitter helper name for a parameter type.
/// Mirrors `c_lir::helpers::lir_trace_formatter` but routes through the
/// bundled `__gorget_trace_emit_arg_*` helpers (defined in TRACE_RUNTIME)
/// so each LLVM call site is a single `call void @...(name, first, val)`
/// rather than a name-then-value pair.
///
/// Returns `(helper_name, llvm_arg_ty)`. `llvm_arg_ty` is the LLVM type
/// the LLVM backend should pass the parameter as (e.g. `i64` for ints,
/// `double` for floats, `ptr` for Str). Bool params come in as `i1`,
/// the helper takes `int`, so the caller must `zext` first — caller
/// inspects the type tag if the returned ty is `i32` to know to zext.
fn trace_arg_emitter(ty: &LirType, snames: &HashMap<u32, String>) -> Option<(&'static str, &'static str)> {
    match ty {
        LirType::Bool => Some(("__gorget_trace_emit_arg_bool", "i32")),
        LirType::F32 | LirType::F64 => Some(("__gorget_trace_emit_arg_float", "double")),
        LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64
        | LirType::U8 | LirType::U16 | LirType::U32 | LirType::U64 => {
            Some(("__gorget_trace_emit_arg_int", "i64"))
        }
        LirType::Struct(sid) | LirType::PtrTo(sid) => {
            let sname = snames.get(&sid.0).map(|s| s.as_str()).unwrap_or("");
            if sname == "GorgetString" || sname == "Str" {
                Some(("__gorget_trace_emit_arg_str", "ptr"))
            } else {
                // Non-Str aggregates aren't traceable as plain values;
                // fall back to int (mirrors the C backend).
                Some(("__gorget_trace_emit_arg_int", "i64"))
            }
        }
        // Ptr / Void / FnPtr — fall back to int rendering.
        _ => Some(("__gorget_trace_emit_arg_int", "i64")),
    }
}

/// Return the `n`-th `%`-prefixed format specifier in a printf format
/// string (skipping `%%`), or `None` if the format has fewer specs.
/// Used by the guard-value type-inference pass to pick `F64` for args
/// at `%f`/`%g`/`%e` positions.
fn nth_printf_spec(fmt: &str, n: usize) -> Option<String> {
    let mut chars = fmt.chars().peekable();
    let mut idx = 0usize;
    while let Some(c) = chars.next() {
        if c != '%' { continue; }
        if chars.peek() == Some(&'%') { chars.next(); continue; }
        // Consume flags / width / precision / length, then return on the
        // conversion char.
        let mut spec = String::from('%');
        while let Some(&nc) = chars.peek() {
            spec.push(nc);
            chars.next();
            if nc.is_alphabetic() { break; }
        }
        if idx == n { return Some(spec); }
        idx += 1;
    }
    None
}

// `parse_compound_literal`, `parse_compound_literal_field`,
// `RuntimeInitArg`, `parse_runtime_init_arg`, and `split_top_level_args`
// were all removed when `GlobalInit::RuntimeCall(String)` got replaced
// with the typed `Extern { name, args }` shape. All consumed in
// `emit_global_init_arg_llvm` directly off `LirGlobalInitArg`.

fn emit_global_runtime_init(
    out: &mut String,
    gid: usize,
    fn_name: &str,
    args: &[crate::lir::LirGlobalInitArg],
    global_ty: &LirType,
    snames: &HashMap<u32, String>,
    module: &LirModule,
    str_globals: &mut StrGlobals,
) {
    // Render each arg into LLVM IR. `AddrOfInline` allocates a temporary,
    // stores the value, and passes its pointer; `StrLit` interns into the
    // module-level `@.str.X` table and passes the address; everything
    // else flows as an immediate operand.
    let mut arg_strs: Vec<String> = Vec::new();
    for (i, arg) in args.iter().enumerate() {
        emit_global_init_arg_llvm(out, gid, i, arg, snames, module, str_globals, &mut arg_strs);
    }

    // A LOCALLY-DEFINED init (`__gg_static_init_*`, synthesized for a static
    // with a literal/aggregate initializer) carries a TYPED return signature in
    // `module.functions` — drive its ABI off that, not the name allow-list
    // below. Use the SAME `needs_sret` predicate the `define` site uses
    // (~`:2250`) so caller and callee always agree; aggregate returns are sret
    // (alloca + `call void @f(ptr sret(T), …)` + memcpy into the global),
    // scalars are a direct `call T @f(…)` + store. Keeps the allow-list as the
    // fallback for real runtime ctors (`gorget_array_new` …), which are NOT in
    // `module.functions`.
    if let Some(target_fn) = module.functions.iter().find(|f| f.name == fn_name) {
        let sym = c_func_name(fn_name);
        if needs_sret(&target_fn.return_type, &module.structs) {
            let ret_llvm = llvm_type_full(&target_fn.return_type, snames);
            let sz = sizeof_lir_type(&target_fn.return_type, &module.structs, snames);
            writeln!(out, "  %__ginit_{gid} = alloca {ret_llvm}").unwrap();
            let sret_arg = format!("ptr sret({ret_llvm}) %__ginit_{gid}");
            let all_args = if arg_strs.is_empty() {
                sret_arg
            } else {
                format!("{sret_arg}, {}", arg_strs.join(", "))
            };
            writeln!(out, "  call void @{sym}({all_args})").unwrap();
            writeln!(out, "  call ptr @memcpy(ptr @__lir_g{gid}, ptr %__ginit_{gid}, i64 {sz})").unwrap();
        } else {
            let ret_llvm = llvm_type_full(&target_fn.return_type, snames);
            writeln!(out, "  %__ginit_{gid}_raw = call {ret_llvm} @{sym}({})", arg_strs.join(", ")).unwrap();
            writeln!(out, "  store {ret_llvm} %__ginit_{gid}_raw, ptr @__lir_g{gid}").unwrap();
        }
        return;
    }

    // Determine return type from function name. Aggregate-returning ctors
    // (`GorgetMap` / `GorgetArray` / `GorgetSet` / `GorgetString`) need
    // sret + memcpy; scalar-returning ones (`gorget_atomic_int_new`,
    // `gorget_stdout_handle`, `gorget_mutex_new`, …) call as `i64` and
    // store the result directly. Heuristic: explicit allow-list for the
    // canonical names, then suffix-based fallback for monomorphized
    // wrappers (`Mutex__T__new` etc.).
    let (ret_struct_name, ret_llvm) = if matches!(fn_name,
        "gorget_dict_new" | "gorget_dict_new_str"
        | "gorget_map_new"  | "gorget_map_new_str"
        | "gorget_set_new"  | "gorget_set_new_str"
        // Ordered Set ctors return GorgetSet (a `typedef GorgetMap` in C —
        // same 152-byte layout). Without this arm a `static Set` init falls
        // to the scalar `i64` path below → the 152-byte handle is truncated
        // to a corrupt pointer-sized value → segfault on first `.add`.
        | "gorget_ordered_set_new" | "gorget_ordered_set_new_str"
    ) {
        ("GorgetMap", "%GorgetMap")
    } else if matches!(fn_name,
        "gorget_array_new" | "gorget_array_with_capacity"
    ) {
        ("GorgetArray", "%GorgetArray")
    } else if matches!(fn_name,
        "gorget_str_from_literal" | "gorget_string_clone" | "gorget_int_to_str" | "gorget_float_to_str"
    ) {
        ("GorgetString", "%GorgetString")
    } else {
        // Scalar return (handle, atomic, mutex, float constant, …). The
        // return type is the GLOBAL's own type — NOT a hardcoded i64. A
        // `float`-returning runtime ctor (`gorget_math_infinity` /
        // `gorget_math_nan`) called as `i64` reads rax instead of xmm0 on
        // x86_64 SysV → garbage 0.0 (the `static_init_imported`/`math_*`
        // bug). Pointer-sized integer handles keep `i64` since their global
        // type is I64/Ptr. Aggregate globals never reach here (sret-routed).
        let ret_llvm = llvm_arg_type(global_ty, snames);
        writeln!(out, "  %__ginit_{gid}_raw = call {ret_llvm} @{fn_name}({})", arg_strs.join(", ")).unwrap();
        writeln!(out, "  store {ret_llvm} %__ginit_{gid}_raw, ptr @__lir_g{gid}").unwrap();
        return;
    };

    // sret-returning ctor: alloca + memcpy into the global slot.
    let sz = sizeof_struct_by_name(ret_struct_name, module, snames);
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

/// Render one `LirGlobalInitArg` into LLVM IR. Pushes the typed-arg
/// fragment (e.g. `"i64 8"`, `"ptr %__ginit_0_a1"`, `"ptr @.str.N"`) into
/// `arg_strs`. `AddrOfInline` requires writing prelude lines (alloca +
/// store), `StrLit` interns into the module-level string table.
fn emit_global_init_arg_llvm(
    out: &mut String,
    gid: usize,
    i: usize,
    arg: &crate::lir::LirGlobalInitArg,
    snames: &HashMap<u32, String>,
    module: &LirModule,
    str_globals: &mut StrGlobals,
    arg_strs: &mut Vec<String>,
) {
    use crate::lir::LirGlobalInitArg;
    match arg {
        LirGlobalInitArg::Int(n) => arg_strs.push(format!("i64 {n}")),
        LirGlobalInitArg::Float(x) => arg_strs.push(format!("double 0x{:016X}", x.to_bits())),
        LirGlobalInitArg::Bool(b) => arg_strs.push(format!("i64 {}", if *b { 1 } else { 0 })),
        LirGlobalInitArg::Sizeof(t) => {
            // A user-struct VALUE in a static collection (`static Dict[int,
            // Point]`) spells its element size as `Sizeof("Point")`.
            // `c_sizeof_name` only knows primitives + runtime handle structs,
            // so a user struct hits its `_ => 8` default → the value slot
            // truncates to one field (C is correct: it emits literal
            // `sizeof(Point)` resolved at cc-time). Compute the real size from
            // `module.structs` when the name resolves there; otherwise fall to
            // `c_sizeof_name` (primitives, and collection handle structs —
            // `"GorgetMap"`/`"GorgetSet"` → 152, `"GorgetArray"` → 64 — owned
            // by Bug 1). The `t ∈ module.structs` gate routes ONLY user
            // structs here; no GorgetMap/Set exclusion is needed.
            let sz = if module.structs.iter().any(|s| s.name == *t) {
                sizeof_struct_by_name(t, module, snames)
            } else {
                c_sizeof_name(t)
            };
            arg_strs.push(format!("i64 {sz}"));
        }
        LirGlobalInitArg::StrLit(s) => {
            // Intern into the module-level `@.str.N` table — it's already
            // emitted at module-scope by `emit_string_globals`. Pass the
            // address; the matching `Int(len)` arg from the producer
            // carries the length.
            let idx = str_globals.intern(s);
            arg_strs.push(format!("ptr @.str.{idx}"));
        }
        LirGlobalInitArg::AddrOfInline { c_type, value } => {
            let ty = c_type_to_llvm(c_type);
            let tmp = format!("__ginit_{gid}_a{i}");
            writeln!(out, "  %{tmp} = alloca {ty}").unwrap();
            let val_str = match value.as_ref() {
                LirGlobalInitArg::Int(n) => format!("{n}"),
                LirGlobalInitArg::Float(f) => format!("0x{:016X}", f.to_bits()),
                LirGlobalInitArg::Bool(b) => format!("{}", if *b { 1 } else { 0 }),
                _ => "0".to_string(),
            };
            writeln!(out, "  store {ty} {val_str}, ptr %{tmp}").unwrap();
            arg_strs.push(format!("ptr %{tmp}"));
        }
    }
}

/// Map a C type name to its LLVM type.
fn c_type_to_llvm(c_type: &str) -> &'static str {
    match c_type {
        "int8_t" | "uint8_t" | "char" | "signed char" | "unsigned char" | "bool" => "i8",
        "int16_t" | "uint16_t" | "short" | "unsigned short" => "i16",
        "int32_t" | "uint32_t" | "int" | "unsigned int" => "i32",
        "int64_t" | "uint64_t" | "long" | "long long" | "size_t" | "ssize_t" => "i64",
        "float" => "float",
        "double" => "double",
        _ => "i64",
    }
}

/// Return the C sizeof for a type name string.
fn c_sizeof_name(name: &str) -> usize {
    match name {
        "int64_t" | "uint64_t" | "double" | "int64" => 8,
        "int32_t" | "uint32_t" | "float" => 4,
        "int16_t" | "uint16_t" => 2,
        "int8_t" | "uint8_t" | "bool" | "char" => 1,
        "Str" | "GorgetString" => 32,
        // Runtime collection handle structs — element-size operands of a
        // static collection constructor (`gorget_dict_new(sizeof(GorgetArray))`
        // for `Dict[int, Vector[int]]`). The GIR routes the surface type
        // (`Vector`/`Set`/…) to these handle-struct names via
        // `collection_arg_sizeof_c_type`; without these arms the `_ => 8`
        // default truncates the handle to size 8 → bounds panic.
        "GorgetArray" => 64,
        // 152 is the authoritative GorgetMap struct size (19 × 8-byte fields,
        // `runtime_preamble.c:328-349`; GorgetSet is `typedef GorgetMap`), used
        // everywhere on the C side (`src/lir/types.rs:150`
        // `computed_c_size: Some(152)`). NOT 160 — `sizeof_struct_by_name`'s
        // 160 (above) is a separate pre-existing over-size to be cleaned up.
        "GorgetMap" | "GorgetSet" => 152,
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
    // Track all (signed_prefix, op, bits) triples used across the module so we
    // declare only the intrinsics we actually reference (LLVM is picky about
    // `declare` signatures and rejects unused duplicates).
    let mut overflow_intrinsics: std::collections::HashSet<(char, &'static str, u32)> =
        std::collections::HashSet::new();
    // (signed_prefix, dst_int_bits, src_float_bits) for fptosi.sat / fptoui.sat.
    // These give Rust `as`-style saturation (NaN→0, out-of-range→clamp) — a raw
    // `fptosi`/`fptoui` returns poison out of range, which is UB.
    let mut fp_sat_intrinsics: std::collections::HashSet<(char, u32, u32)> =
        std::collections::HashSet::new();
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                match inst {
                    Inst::Add { ty, overflow: Overflow::Trap, .. }
                    | Inst::Sub { ty, overflow: Overflow::Trap, .. }
                    | Inst::Mul { ty, overflow: Overflow::Trap, .. } => {
                        let op = match inst {
                            Inst::Add { .. } => "add",
                            Inst::Sub { .. } => "sub",
                            Inst::Mul { .. } => "mul",
                            _ => unreachable!(),
                        };
                        if !ty.is_integer() { continue; }
                        let bits = int_bits(ty);
                        let signed = if is_signed(ty) { 's' } else { 'u' };
                        overflow_intrinsics.insert((signed, op, bits));
                    }
                    // Fault-catch checked Add/Sub/Mul use the same with-overflow
                    // intrinsic as the trap form (error-model.md §11.2); declare
                    // it. Div/Rem use an `icmp`, no intrinsic.
                    Inst::FaultCheck { op, ty, .. } => {
                        if let Some(builtin) = op.overflow_builtin() {
                            if ty.is_integer() {
                                let bits = int_bits(ty);
                                let signed = if is_signed(ty) { 's' } else { 'u' };
                                overflow_intrinsics.insert((signed, builtin, bits));
                            }
                        }
                    }
                    Inst::FloatToInt { value, to, .. } => {
                        if !to.is_integer() { continue; }
                        let dst_bits = int_bits(to);
                        let signed = if is_signed(to) { 's' } else { 'u' };
                        // Source float width: look up the function-local value_types;
                        // default to f64 (the common case — Gorget's `float` is F64).
                        let src_bits = match func.value_types.get(value.0 as usize)
                            .and_then(|t| t.as_ref())
                        {
                            Some(LirType::F32) => 32,
                            _ => 64,
                        };
                        fp_sat_intrinsics.insert((signed, dst_bits, src_bits));
                    }
                    _ => {}
                }
            }
        }
    }

    writeln!(out, "; -- intrinsics --").unwrap();
    writeln!(out, "declare void @llvm.trap() noreturn nounwind").unwrap();
    writeln!(out, "declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)").unwrap();
    writeln!(out, "declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)").unwrap();
    let mut sorted_intrinsics: Vec<_> = overflow_intrinsics.into_iter().collect();
    sorted_intrinsics.sort();
    for (signed, op, bits) in sorted_intrinsics {
        writeln!(out,
            "declare {{ i{bits}, i1 }} @llvm.{signed}{op}.with.overflow.i{bits}(i{bits}, i{bits})"
        ).unwrap();
    }
    let mut sorted_fp_sat: Vec<_> = fp_sat_intrinsics.into_iter().collect();
    sorted_fp_sat.sort();
    for (signed, dst_bits, src_bits) in sorted_fp_sat {
        let src_ty = if src_bits == 32 { "float" } else { "double" };
        let kind = if signed == 's' { "fptosi" } else { "fptoui" };
        writeln!(out,
            "declare i{dst_bits} @llvm.{kind}.sat.i{dst_bits}.f{src_bits}({src_ty})"
        ).unwrap();
    }

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
                if let Inst::ClosurePack { call_func, needs_adapter: true, .. } = inst {
                    adapter_fids.insert(call_func.0);
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
        // Cross-frame fault (Inc-2.1b): a PARTICIPATING fn has synthesized trailing
        // `MutPtr<i32>` fault-slot param(s) that are NOT part of its callable type.
        // The adapter is invoked through the user-arity callable ABI, so it must
        // declare ONLY the user params and pass `null` for the trailing slot(s) —
        // forwarding a phantom slot arg writes a fault tag through a wild pointer
        // (memory corruption / UB call-signature mismatch). `null` makes the callee's
        // fault arm panic inline = panic-by-default for an indirectly-invoked fault.
        // Typed count off the LIR function, never name/shape-matched (devbook/24
        // rule 2). Mirrors the C adapter (src/backend/c_lir/mod.rs).
        let user_param_count = target.params.len().saturating_sub(target.fault_slot_param_count);
        for (i, p) in target.params.iter().take(user_param_count).enumerate() {
            let ty = llvm_arg_type(p, snames);
            param_decls.push(format!("{ty} %a.p{i}"));
            param_names.push(format!("{ty} %a.p{i}"));
        }
        // Append `null` for each synthesized trailing fault-slot param (panic-by-default).
        for p in target.params.iter().skip(user_param_count) {
            let ty = llvm_arg_type(p, snames);
            param_names.push(format!("{ty} null"));
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

    // Scan NamedFuncAddr instructions for user-type function references. These
    // need declarations (or definitions for __clone_inplace wrappers) since
    // NamedFuncAddr just emits `bitcast ptr @NAME to ptr` without declaring.
    let mut clone_inplace_seen = std::collections::HashSet::new();
    let mut named_addr_fns = std::collections::HashSet::new();
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::NamedFuncAddr { name, .. } = inst {
                    if name.ends_with("__clone_inplace") && !name.starts_with("gorget_") {
                        clone_inplace_seen.insert(name.clone());
                    }
                    named_addr_fns.insert(name.clone());
                }
            }
        }
    }
    // Emit declarations for NamedFuncAddr targets not defined locally.
    // The wrapper generator below handles `__clone_inplace`; here we cover
    // `T__drop` (elem_drop on user types, resolved at link time from C runtime).
    // Compute the set of function names already referenced via CallExtern —
    // those get declarations emitted by the CallExtern scan loop further down
    // (seen set). NamedFuncAddr uses must share declarations with CallExtern
    // to avoid duplicate `declare`s.
    let call_extern_names: std::collections::HashSet<String> = module.functions.iter()
        .flat_map(|f| f.blocks.iter().flat_map(|b| b.insts.iter()))
        .filter_map(|i| match i {
            Inst::CallExtern { name, .. } => Some(name.clone()),
            _ => None,
        })
        .collect();
    for name in &named_addr_fns {
        if name.starts_with("gorget_") { continue; }
        if name.ends_with("__clone_inplace") { continue; } // wrapper below
        let defined = module.functions.iter().any(|f| f.name == *name)
            || module.externs.iter().any(|e| e.name == *name)
            || call_extern_names.contains(name);
        if !defined {
            let safe = c_func_name(name);
            writeln!(out, "declare void @{safe}(ptr)").unwrap();
        }
    }
    for name in &clone_inplace_seen {
        let clone_fn = name.strip_suffix("_inplace").unwrap_or(name);
        // Find the struct for this clone function to determine the type
        let type_name = clone_fn.strip_suffix("__clone").unwrap_or(clone_fn);
        let struct_sid = module.structs.iter().enumerate()
            .find(|(_, s)| s.name == type_name)
            .map(|(i, _)| StructId(i as u32));
        if let Some(sid) = struct_sid {
            let ty = llvm_type_full(&LirType::Struct(sid), snames);
            let sz = sizeof_lir_type(&LirType::Struct(sid), &module.structs, snames);
            let safe_name = c_func_name(name);
            let safe_clone = c_func_name(clone_fn);
            // Declare T__clone if not already defined in this module. The wrapper
            // references it, so we need the declaration for LLVM/llc to resolve.
            // T__clone is defined in the C runtime amalgamation (via CliArg__clone
            // etc. in c_runtime.rs) and resolved at link time.
            let clone_declared = module.functions.iter().any(|f| f.name == clone_fn)
                || module.externs.iter().any(|e| e.name == clone_fn);
            if !clone_declared {
                writeln!(out, "declare void @{safe_clone}(ptr sret({ty}), ptr)").unwrap();
            }
            writeln!(out, "define linkonce_odr void @{safe_name}(ptr %p) {{").unwrap();
            writeln!(out, "  %tmp = alloca {ty}").unwrap();
            writeln!(out, "  call void @{safe_clone}(ptr sret({ty}) %tmp, ptr %p)").unwrap();
            writeln!(out, "  call void @llvm.memcpy.p0.p0.i64(ptr %p, ptr %tmp, i64 {sz}, i1 false)").unwrap();
            writeln!(out, "  ret void").unwrap();
            writeln!(out, "}}").unwrap();
        } else {
            // Unknown type — emit a declare as fallback
            let safe_name = c_func_name(name);
            writeln!(out, "declare void @{safe_name}(ptr)").unwrap();
        }
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
    // Spawn wrappers cross the C runtime / LLVM IR boundary, so their
    // >16-byte aggregate params need a ptr-passing override (AArch64 PCS
    // rule B.4 vs LLVM's default split-across-8-regs lowering). The
    // disambiguation lives in `lir::queries::is_spawn_wrapper` so the
    // naming patterns are tracked in one place.
    let is_spawn_wrapper = crate::lir::queries::is_spawn_wrapper(func);
    let params: Vec<String> = func.params.iter().enumerate()
        .map(|(i, p)| {
            let ty = if *p == LirType::Void { "ptr".to_string() } else { llvm_type_full(p, snames) };
            if is_spawn_wrapper && p.is_aggregate() && !is_small_aggregate(p, &module.structs) {
                // x86_64 SysV: a >16-byte by-value aggregate is MEMORY class (a stack
                // copy), so the C run-fn passes the Str/struct BY VALUE. Without byval the
                // wrapper-def would take a register pointer while its body memcpy's the
                // stack-passed bytes as if `%pN` were an address -> SEGV. byval on x86_64
                // (via the x86_64-gated large_agg_byval_attr); "" on aarch64 -> bare
                // `ptr %pN`, byte-identical to before (AAPCS64 passes by implicit ptr).
                let byval = large_agg_byval_attr(p, snames);
                return format!("ptr {byval}%p{i}").trim_end().to_string();
            }
            format!("{ty} %p{i}")
        })
        .collect();

    let is_main = func.name == "main";
    let has_sret = !is_main && needs_sret(&func.return_type, &module.structs);
    if is_main {
        // @main runs the program body on thread 0 (the host stack) — the
        // intended plain-main behavior, matching the C backend now that the
        // 64MB-pthread main runner (Fix B) was reverted (src/backend/c_lir/mod.rs
        // emit_function). Deep user recursion overflowing the OS stack is
        // shared with C/Rust (TCO is the eventual cure); the stack_guard_*
        // integration tests skip_under_llvm() since they probe the C runner.
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

    // Start from shared value_types (computed once after LIR optimization).
    // The shared computation already uses PtrTo for struct pointers, matching
    // the LLVM backend's convention.  Struct(sid) values need PtrTo override
    // since aggregates are always represented as pointers in LLVM codegen.
    let val_count = func.value_count() as usize;
    let mut val_types: Vec<Option<LirType>> = func.value_types.iter()
        .map(|t| match t.as_ref() {
            Some(LirType::Struct(sid)) => Some(LirType::PtrTo(*sid)),
            other => other.cloned(),
        })
        .collect();
    val_types.resize(val_count, None);

    // Fix Call/CallClosure return types: when the target function returns an aggregate,
    // the emitter stores it via alloca making the value a pointer. Override to PtrTo.
    for block in &func.blocks {
        for inst in &block.insts {
            match inst {
                Inst::Call { dst: Some(d), func: fid, .. } => {
                    let target = &module.functions[fid.0 as usize];
                    if target.return_type.is_aggregate() {
                        if let LirType::Struct(sid) = &target.return_type {
                            val_types[d.0 as usize] = Some(LirType::PtrTo(*sid));
                        }
                    }
                }
                Inst::CallClosure { dst: Some(d), ret_ty, .. } => {
                    if ret_ty.is_aggregate() {
                        if let LirType::Struct(sid) = ret_ty {
                            val_types[d.0 as usize] = Some(LirType::PtrTo(*sid));
                        }
                    }
                }
                _ => {}
            }
        }
    }

    // Fix Option/Result combinator return types: these produce alloca'd structs (PtrTo).
    for block in &func.blocks {
        for inst in &block.insts {
            if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                if let Some((type_prefix, method)) = parse_option_result_combinator(name) {
                    // Result type is the Option/Result struct
                    let result_sid = match method {
                        "flatten" => {
                            // flatten result is the inner Option type (payload of outer)
                            find_struct_by_prefix(type_prefix, module).and_then(|sid| {
                                let sdef = &module.structs[sid.0 as usize];
                                sdef.fields.get(1).and_then(|(_, t)| match t {
                                    LirType::Struct(inner) => Some(*inner),
                                    _ => None,
                                })
                            }).or_else(|| find_struct_by_prefix(type_prefix, module))
                        }
                        "unwrap_or_else" => {
                            // unwrap_or_else returns the payload, not the Option — use arg type
                            find_struct_by_prefix(type_prefix, module).and_then(|sid| {
                                let sdef = &module.structs[sid.0 as usize];
                                sdef.fields.get(1).and_then(|(_, t)| match t {
                                    LirType::Struct(inner) => Some(*inner),
                                    _ => None,
                                })
                            })
                        }
                        _ => find_struct_by_prefix(type_prefix, module),
                    };
                    if let Some(sid) = result_sid {
                        val_types[d.0 as usize] = Some(LirType::PtrTo(sid));
                    }
                }
            }
        }
    }

    // Parse method type override: int8_t__parse / double__parse etc. are declared
    // `i64` in LIR but the LLVM inline handler (emit_inst) materializes the result
    // as an alloca'd Option struct — so the value IS a pointer. Override val_types
    // to PtrTo(Option__T) so SlotStore takes the memcpy path (not scalar store).
    // The target struct is inferred from the slot's type at consumer sites when
    // the extern's declared return_type is a scalar.
    for block in &func.blocks {
        let insts = &block.insts;
        for (i, inst) in insts.iter().enumerate() {
            if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                let is_parse = name.ends_with("__parse")
                    && (name.starts_with("int") || name.starts_with("uint")
                        || name == "double__parse" || name == "float__parse"
                        || name == "bool__parse");
                if !is_parse { continue; }
                let vid = d.0 as usize;
                // Look at the extern declaration. If it returns a struct, use PtrTo(sid).
                // Otherwise search downstream SlotStore for a struct slot type.
                let mut target_sid: Option<StructId> = module.externs.iter()
                    .find(|e| e.name == *name)
                    .and_then(|e| match &e.return_type {
                        LirType::Struct(sid) => Some(*sid),
                        _ => None,
                    });
                if target_sid.is_none() {
                    for ci in (i+1)..insts.len().min(i+10) {
                        if let Inst::SlotStore { value, slot, .. } = &insts[ci] {
                            if *value == *d {
                                if let LirType::Struct(sid) = &func.slots[slot.0 as usize].ty {
                                    target_sid = Some(*sid);
                                    break;
                                }
                            }
                        }
                    }
                }
                if let Some(sid) = target_sid {
                    val_types[vid] = Some(LirType::PtrTo(sid));
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
                                // gorget_guard_get returns a pointer to the inner type;
                                // for struct slots we must treat the call result as
                                // PtrTo(struct) so SlotStore does memcpy-from-ptr, not
                                // a struct-by-value store (LLVM rejects type mismatch).
                                inferred = Some(match slot_ty {
                                    LirType::Struct(sid) => LirType::PtrTo(*sid),
                                    other => other.clone(),
                                });
                                break;
                            }
                        }
                        // Printf-as-consumer: a printf-like extern with a static
                        // `%f`/`%g`/`%e` spec at our position means the runtime
                        // expects a double in the matching FP register; loading
                        // as i64 puts the bits in x{n} and printf reads stale
                        // garbage off v0 → silent flake (~63% wrong on shared_float).
                        // Mirrors what the C backend gets via `*(double*)v` at the
                        // call site.
                        Inst::CallExtern { name, args: pargs, .. }
                            if matches!(name.as_str(),
                                "printf" | "fprintf_stderr" | "snprintf" | "sprintf"
                                | "gorget_string_format" | "gorget_string_format_alloc")
                                && pargs.len() >= 2
                                && pargs[1..].iter().position(|a| *a == *d).is_some() =>
                        {
                            // Walk back to find the format-string str_lit. The
                            // format is the first arg; we want the spec at our
                            // position in the variadic tail.
                            let pos_in_varargs = pargs[1..].iter().position(|a| *a == *d).unwrap();
                            let fmt_arg = pargs[0];
                            let fmt = insts[..ci].iter().find_map(|s| match s {
                                Inst::StrLit { dst: sd, value } if *sd == fmt_arg => Some(value.as_str()),
                                _ => None,
                            });
                            if let Some(fmt_str) = fmt {
                                if let Some(spec) = nth_printf_spec(fmt_str, pos_in_varargs) {
                                    let last = spec.chars().last().unwrap_or(' ');
                                    if matches!(last, 'f' | 'g' | 'e' | 'F' | 'G' | 'E' | 'a' | 'A') {
                                        inferred = Some(LirType::F64);
                                        break;
                                    }
                                }
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
            if let crate::lir::LirGlobalInit::Extern { name, args } = &global.init {
                // Module-level string literals are initialized as cap=0
                // rodata views by `emit_globals`. Skip the runtime ctor.
                if crate::backend::c_lir::helpers::is_str_literal_view_init(
                    name, args, &global.ty, &module.structs,
                ) {
                    continue;
                }
                emit_global_runtime_init(out, gid, name, args, &global.ty, snames, module, str_globals);
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
                    let is_runtime_resource = matches!(sname,
                        "GorgetString" | "GorgetArray" | "GorgetMap" | "GorgetSet");
                    // Read typed `enum_kind` (set at LIR struct registration
                    // from GIR's `enum_category`) instead of name-prefix
                    // matching to detect Option/Result slots.
                    let kind = module.structs.get(sid.0 as usize)
                        .map(|s| s.enum_kind).unwrap_or(crate::lir::EnumKind::NotEnum);
                    let is_opt_or_result = matches!(kind,
                        crate::lir::EnumKind::Option | crate::lir::EnumKind::Result);
                    is_runtime_resource || is_opt_or_result
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

    // Trace runtime: at main()'s prologue, open the trace file. Mirrors
    // `c_lir/mod.rs:924-927` which emits the same call into the C `main`.
    let tracing = module.trace_filename.is_some();
    if tracing {
        if is_main {
            if let Some(path) = &module.trace_filename {
                let path_idx = str_globals.intern(path);
                writeln!(out, "  call void @__gorget_trace_init(ptr @.str.{path_idx})").unwrap();
            }
        }
        // Function-entry call event — non-main only, mirroring
        // `c_lir/mod.rs:947-966` which gates the emission on the
        // `else` arm of `if func.name == "main"`. Display name comes from
        // `func.display_name` (Gorget-level name, not C-mangled).
        if !is_main {
        if let Some(display_name) = &func.display_name {
            let name_idx = str_globals.intern(display_name);
            writeln!(out, "  call void @__gorget_trace_emit_call_begin(ptr @.str.{name_idx})").unwrap();
            for (pi, pty) in func.params.iter().enumerate() {
                let pname = func.param_names.get(pi)
                    .and_then(|n| n.as_deref())
                    .unwrap_or("_");
                let pname_idx = str_globals.intern(pname);
                let first = if pi == 0 { 1 } else { 0 };
                let (helper, arg_ty) = match trace_arg_emitter(pty, snames) {
                    Some(p) => p,
                    None => continue,
                };
                // Bool params come in as i1 — the helper takes i32, so zext.
                let val_str = match (pty, arg_ty) {
                    (LirType::Bool, "i32") => {
                        let z = format!("trace.argz.{pi}");
                        writeln!(out, "  %{z} = zext i1 %p{pi} to i32").unwrap();
                        format!("%{z}")
                    }
                    _ => format!("%p{pi}"),
                };
                writeln!(out,
                    "  call void @{helper}(ptr @.str.{pname_idx}, i32 {first}, {arg_ty} {val_str})").unwrap();
            }
            writeln!(out, "  call void @__gorget_trace_emit_call_end()").unwrap();
        }
        }
    }

    // NOTE: `br label %bb0` (the entry-block terminator) is NOT emitted here.
    // It is emitted AFTER the block loop below, so the per-instruction temp
    // allocas hoisted out of the body can land in the entry block *before* the
    // terminator. (An `alloca` after a block terminator is invalid IR; LLVM
    // never reclaims non-entry-block allocas across loop iterations, so a hot
    // loop's per-iteration body allocas otherwise pile up onto the stack →
    // overflow on the ~660K-line self-compile.)

    // Emit blocks
    // First, collect predecessor info for phi nodes
    let pred_map = build_predecessor_map(func);

    // Trace: pre-collect which blocks are the `then` target of a Branch.
    // Those get an extra `branch` event before `stmt_start`. Mirrors the
    // C backend's `trace_then_blocks` (c_lir/mod.rs:1682).
    let trace_then_blocks: std::collections::HashSet<u32> = if tracing {
        func.blocks.iter().filter_map(|b| {
            if let Term::Branch { then_block, .. } = &b.terminator {
                Some(then_block.0)
            } else {
                None
            }
        }).collect()
    } else {
        std::collections::HashSet::new()
    };

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
                        // div0 guard always bumps + sets exit. For SIGNED, the
                        // `TYPE_MIN/-1` overflow guard (emit_div_overflow_trap,
                        // error-model.md §11 (E)) bumps again and moves the exit
                        // to its own `ok` block — mirror BOTH counter bumps and
                        // the final label exactly (layering: this pre-pass twins
                        // the emit).
                        counter += 1;
                        if is_signed(ty) {
                            label = format!("ovfdiv.{bid}.{counter}.ok");
                            counter += 1;
                        } else {
                            label = format!("divz.{bid}.{}.ok", counter - 1);
                        }
                    }
                    Inst::Rem { ty, .. } if ty.is_integer() => {
                        counter += 1;
                        if is_signed(ty) {
                            label = format!("ovfrem.{bid}.{counter}.ok");
                            counter += 1;
                        } else {
                            label = format!("remz.{bid}.{}.ok", counter - 1);
                        }
                    }
                    Inst::Mod { ty, .. } if ty.is_integer() => {
                        // Mirror the emit's SINGLE counter bump (one `uid`, reused
                        // for the div0 AND signed-overflow labels). Signed exits at
                        // the merge `done` block; unsigned exits at the div0 `ok`.
                        let uid = counter;
                        counter += 1;
                        if is_signed(ty) {
                            label = format!("modov.{bid}.{uid}.done");
                        } else {
                            label = format!("modz.{bid}.{uid}.ok");
                        }
                    }
                    Inst::BoundsCheck { .. } => {
                        label = format!("bc.{bid}.{counter}.ok");
                        counter += 1;
                    }
                    Inst::DivCheck { .. } => {
                        label = format!("dc.{bid}.{counter}.ok");
                        counter += 1;
                    }
                    // D11 shift range-check (owner ruling 2026-07-10): every
                    // Shl/Shr now emits a guard that splits the block + bumps
                    // trap_counter, exiting at `sh{l,r}.{bid}.{uid}.ok`. This
                    // pre-pass TWINS that emit exactly (layering: same counter,
                    // same exit label) — omitting it desyncs phi predecessor
                    // labels (an `llc: use of undefined value %ov.*.ok`).
                    Inst::Shl { .. } => {
                        label = format!("shl.{bid}.{counter}.ok");
                        counter += 1;
                    }
                    Inst::Shr { .. } => {
                        label = format!("shr.{bid}.{counter}.ok");
                        counter += 1;
                    }
                    // DropGuardOpen (Bool) creates body + join labels.
                    Inst::DropGuardOpen { kind: DropGuardKind::Bool, .. } => {
                        df_stack.push(counter);
                        counter += 1;
                    }
                    Inst::DropGuardClose => {
                        if let Some(uid) = df_stack.pop() {
                            label = format!("dg.{bid}.{uid}.join");
                        }
                    }
                    Inst::DropGuardOpen { kind: DropGuardKind::NonZero { .. }, .. } => { /* no counter */ }
                    // CallExtern paths that increment trap_counter but DON'T create labels.
                    // MUST mirror every trap_counter += 1 in the emit_inst CallExtern handler.
                    Inst::CallClosure { .. } => { counter += 1; }
                    Inst::CallExtern { name, args, dst, .. } => {
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
                        // Channel__T__recv_timeout intercept emits some/none/done branches.
                        // MUST mirror the counter bump in this pre-pass.
                        let is_recv_timeout_inline = name.starts_with("Channel__")
                            && name.ends_with("__recv_timeout") && args.len() >= 2 && dst.is_some();

                        // Detect Vector HOF calls that create inline loops (labels).
                        // All core HOFs (each/any/all/map/filter/fold/reduce/
                        // count/flat_map/find/find_index) are lowered upstream
                        // via `Inst::HofExpand`. The backend only inlines the
                        // generic opaque-callable fallback (Ptr-typed closure
                        // args the intercept can't resolve). Mirror the
                        // emission gate exactly: counter bumps iff the closure
                        // arg is Ptr-typed.
                        let is_vector_hof = parse_vector_hof(name).is_some();
                        let vector_hof_needs_inline = if is_vector_hof {
                            let (_, method) = parse_vector_hof(name).unwrap();
                            let closure_arg_peek = if method == "fold" && args.len() >= 3 {
                                Some(args[2])
                            } else if args.len() >= 2 {
                                Some(*args.last().unwrap())
                            } else {
                                None
                            };
                            closure_arg_peek.is_some_and(|ca| {
                                matches!(val_types.get(ca.0 as usize).and_then(|t| t.as_ref()), Some(LirType::Ptr))
                            })
                        } else { false };

                        // Detect Dict/Set HOF calls that create inline loops
                        let dict_hof_needs_inline = if parse_dict_hof(name).is_some() {
                            let (_, _, method) = parse_dict_hof(name).unwrap();
                            matches!(method, "filter" | "map") && dst.is_some()
                        } else if parse_set_hof(name).is_some() {
                            let (_, method) = parse_set_hof(name).unwrap();
                            matches!(method, "filter" | "map") && dst.is_some()
                        } else { false };

                        // Detect Option/Result combinator calls that generate branches
                        let is_opt_combinator = parse_option_result_combinator(name).is_some()
                            && dst.is_some() && !args.is_empty();
                        let opt_combinator_has_branch = if is_opt_combinator {
                            let (_, method) = parse_option_result_combinator(name).unwrap();
                            // These methods need if/then/else branches:
                            matches!(method, "map" | "filter" | "and_then" | "or_else"
                                | "map_err" | "flat_map" | "unwrap_or_else" | "flatten")
                        } else { false };

                        // Count ALL counter increments to stay in sync with emission
                        if is_opt_combinator {
                            if opt_combinator_has_branch {
                                // Branch-based combinators: emit labels and change exit label
                                label = format!("comb.{bid}.{counter}.done");
                            }
                            counter += 1;
                        }
                        else if vector_hof_needs_inline && !args.is_empty() {
                            label = format!("hof.{bid}.{counter}.done");
                            counter += 1;
                        }
                        else if dict_hof_needs_inline && !args.is_empty() {
                            label = format!("dhof.{bid}.{counter}.done");
                            counter += 1;
                        }
                        else if is_str_clear {
                            // gorget_str_clear emits a conditional branch → changes exit label
                            label = format!("scl.done.{counter}");
                            counter += 1;
                        }
                        else if is_recv_timeout_inline {
                            label = format!("recvtmo.{bid}.{counter}.done");
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

    // Buffer the block body separately so per-instruction temp `alloca`s can be
    // hoisted to the entry block afterwards. Every body emitter
    // (`emit_inst`/`emit_branch_arg_casts`/`emit_term` and the phi/trace
    // writeln!s) takes `out: &mut String` first, so feeding it `&mut body_buf`
    // threads through with no per-site rewrite. `body_out` is a distinct name
    // (not a shadow of `out`) so the entry-block `out` stays writable after the
    // loop for the hoist assembly.
    let mut body_buf = String::new();
    let body_out = &mut body_buf;

    for block in &func.blocks {
        let bid = block.id.0;
        writeln!(body_out, "bb{bid}:").unwrap();

        // Phi nodes for block parameters.
        // Aggregates flow as pointers in this backend — emit `phi ptr` (not
        // `phi <struct>`) so consumers (memcpy, FieldPtr, etc.) get the
        // pointer they expect. emit_branch_arg_casts spills any predecessor
        // that has the struct value to a slot and passes its pointer.
        for (pi, (param_val, param_ty)) in block.params.iter().enumerate() {
            let phi_as_ptr = param_ty.is_aggregate();
            let ty = if phi_as_ptr { "ptr".to_string() } else { llvm_type_full(param_ty, snames) };
            let mut phi_entries = Vec::new();
            if let Some(preds) = pred_map.get(&block.id) {
                for &pred_id in preds {
                    let pred_block = &func.blocks[pred_id.0 as usize];
                    let args = get_branch_args_for_target(&pred_block.terminator, block.id);
                    // If the predecessor is a same-target `Term::Branch`
                    // whose two arms diverge at this arg index, the
                    // terminator emit lowered the branch into selects
                    // named `%br.sel.<pred>.<target>.<pi>` followed by an
                    // unconditional jump. Reference that select here so
                    // the phi sees the clamp instead of just the THEN arg.
                    let same_target_with_diverging_args = matches!(
                        &pred_block.terminator,
                        Term::Branch { then_block, else_block, then_args, else_args, .. }
                            if then_block == else_block
                                && pi < then_args.len()
                                && pi < else_args.len()
                                && then_args[pi] != else_args[pi]
                    );
                    // Use the actual exit label (may differ from bb{N} due to overflow checks)
                    let pred_label = block_exit_labels.get(&pred_id)
                        .cloned()
                        .unwrap_or_else(|| format!("bb{}", pred_id.0));
                    if same_target_with_diverging_args {
                        phi_entries.push(format!(
                            "[ %br.sel.{}.{}.{pi}, %{pred_label} ]",
                            pred_id.0, block.id.0
                        ));
                    } else if pi < args.len() {
                        // If emit_branch_arg_casts widened/narrowed/spilled this arg, use the cast.
                        let arg = args[pi];
                        let actual = val_types.get(arg.0 as usize).and_then(|t| t.as_ref());
                        let needs_int_cast = param_ty.is_integer() && {
                            actual.map(int_bits).unwrap_or(64) != int_bits(param_ty)
                        };
                        let needs_agg_spill = phi_as_ptr
                            && actual.map_or(false, |t| !matches!(t, LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef));
                        if needs_int_cast || needs_agg_spill {
                            phi_entries.push(format!(
                                "[ %br.cast.{}.{}.{pi}, %{pred_label} ]",
                                pred_id.0, block.id.0
                            ));
                        } else {
                            phi_entries.push(format!("[ %v{}, %{pred_label} ]", arg.0));
                        }
                    } else {
                        phi_entries.push(format!("[ undef, %{pred_label} ]"));
                    }
                }
            }
            if phi_entries.is_empty() {
                // Unreachable block param — just set to undef
                writeln!(body_out, "  %v{} = add {ty} 0, 0 ; dead phi", param_val.0).unwrap();
            } else {
                writeln!(body_out, "  %v{} = phi {ty} {}", param_val.0, phi_entries.join(", ")).unwrap();
            }
        }

        // Trace: branch + stmt_start events. `branch` fires when this block
        // is the `then` arm of a conditional terminator; `stmt_start` fires
        // at every block entry. Both emit AFTER phi nodes (LLVM requires
        // phis to be the leading instructions of a block) and before user
        // instructions. Mirrors `c_lir/mod.rs:1712-1720`.
        if tracing && trace_then_blocks.contains(&bid) {
            writeln!(body_out, "  call void @__gorget_trace_emit_branch()").unwrap();
        }
        if tracing {
            writeln!(body_out, "  call void @__gorget_trace_emit_stmt_start()").unwrap();
        }

        // Instructions
        // Track the "current label" — overflow/bounds checks emit internal sub-blocks,
        // changing the effective predecessor label for phi nodes in successor blocks.
        let mut trap_counter = 0u32;
        let mut current_label = format!("bb{bid}");
        let mut df_stack: Vec<u32> = Vec::new(); // drop flag open/close nesting
        for (inst_idx, inst) in block.insts.iter().enumerate() {
            // Resolve panic location per-instruction. Synthetic instructions
            // (no span entry) and missing file infos fall back to
            // `<unknown>:0:0`. Mirrors the C backend's pattern at
            // src/backend/c_lir/mod.rs `resolve_panic_loc`.
            let span = block.span_map.get(inst_idx).copied().flatten();
            let loc = resolve_panic_loc(span, &module.file_infos);
            emit_inst(body_out, inst, func, module, snames, str_globals, &val_types, bid, &mut trap_counter, &mut current_label, &mut df_stack, &loc);

        }

        // Trace: stmt_end + return events. `stmt_end` fires at the end of
        // every block; `return` fires before each Ret terminator (non-main
        // only — main exits via the implicit `ret i32 0` we emit). Mirrors
        // `c_lir/mod.rs:1750-1763`.
        if tracing {
            writeln!(body_out, "  call void @__gorget_trace_emit_stmt_end()").unwrap();
            if !is_main && matches!(&block.terminator, Term::Ret(_) | Term::RetVoid) {
                if let Some(display_name) = &func.display_name {
                    let name_idx = str_globals.intern(display_name);
                    writeln!(body_out, "  call void @__gorget_trace_emit_return(ptr @.str.{name_idx})").unwrap();
                }
            }
        }

        // Terminator — pre-emit any int-width casts needed for branch args
        // whose types don't match the target block's params.
        emit_branch_arg_casts(body_out, &block.terminator, func, bid, &val_types, snames);
        emit_term(body_out, &block.terminator, func, module, snames, &val_types, bid);
    }

    // ── Hoist per-instruction temp allocas to the entry block ──────────────
    //
    // LLVM only reclaims allocas that live in the function's *entry* block; an
    // alloca emitted into a loop-body basic block is allocated afresh on every
    // iteration and never freed until the call returns. The self-host driver's
    // hot fixpoint loops (e.g. `coal_compute_live_blocks`) therefore piled
    // millions of per-instruction temp allocas onto one frame → stack overflow
    // on its own ~660K-line source.
    //
    // Fix (enumeration-free): every body alloca DEFINITION is a single,
    // statically-sized, uniquely-named line `^\s*%… = alloca …`. Move each such
    // line out of `body_buf` into the entry block (which dominates all blocks,
    // so every hoist is SSA-valid); leave the follower lines (store / memset /
    // getelementptr / select / memcpy / call that USE the pointer) in place.
    let mut hoisted: Vec<&str> = Vec::new();
    let mut remaining_body = String::with_capacity(body_buf.len());
    for line in body_buf.lines() {
        let trimmed = line.trim_start();
        let is_alloca_def = trimmed.starts_with('%')
            && trimmed.contains(" = alloca ");
        if is_alloca_def {
            // SAFETY: a RUNTIME-sized alloca (`= alloca <ty>, i32/i64 %<reg>`)
            // depends on a register computed earlier in the body — hoisting it
            // above its size operand would be wrong. None are emitted today
            // (the only comma forms are constant-sized, e.g. `alloca i8, i64
            // 16`); guard so a future one is left in place rather than silently
            // mis-hoisted. Detect the `, i32/i64 %` runtime-operand shape.
            let is_runtime_sized = trimmed.contains(", i64 %")
                || trimmed.contains(", i32 %");
            debug_assert!(
                !is_runtime_sized,
                "runtime-sized alloca cannot be hoisted to the entry block: {line}"
            );
            if is_runtime_sized {
                remaining_body.push_str(line);
                remaining_body.push('\n');
                continue;
            }
            hoisted.push(line);
        } else {
            remaining_body.push_str(line);
            remaining_body.push('\n');
        }
    }

    // Anti-regression ratchet: every body alloca must have been hoisted. A
    // future emit arm that introduces a body alloca trips this debug assertion
    // instead of the next SIGSEGV.
    debug_assert!(
        !remaining_body.lines().any(|l| {
            let t = l.trim_start();
            t.starts_with('%') && t.contains(" = alloca ")
        }),
        "body buffer still contains an `= alloca` line after the entry-block hoist"
    );

    // Assemble the entry block: the streamed prelude already in `out` (slot /
    // StrLit allocas, trace prologue), then the hoisted body allocas, then the
    // entry terminator (relocated here from above so allocas precede it — an
    // alloca after a terminator is invalid IR), then the rewritten body.
    for line in hoisted {
        out.push_str(line);
        out.push('\n');
    }
    writeln!(out, "  br label %bb0").unwrap();
    writeln!(out).unwrap();
    out.push_str(&remaining_body);

    writeln!(out, "}}\n").unwrap();
}

// ── Predecessor Map ────────────────────────────────────────────────────────

fn build_predecessor_map(func: &LirFunction) -> HashMap<BlockId, Vec<BlockId>> {
    let mut map: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
    for block in &func.blocks {
        // Dedupe successors per terminator: a `Term::Branch` with
        // `then_block == else_block` lists the same successor twice. The
        // phi-emission below would then add two entries from this pred to
        // the target's phi node, which LLVM only accepts when both values
        // are identical. The merge-sort BIR synth uses exactly this pattern
        // (`compute_right_bb` clamps `right = min(right_raw, n)` by
        // branching to `merge_loop_bb` on both arms with different args),
        // and the duplicate-pred phi silently kept the THEN value for
        // both, dropping the clamp → write past the malloc'd merge buffer
        // → heap corruption that surfaces as a glibc malloc assertion when
        // the program later allocates anything (e.g. `int_to_str`).
        // `Term::Branch` is rewritten to select-then-jump in the
        // terminator emit; pred_map only reports each pred once so phi
        // emission produces one consolidated entry.
        let mut seen: HashSet<BlockId> = HashSet::new();
        for succ in block.terminator.successors() {
            if seen.insert(succ) {
                map.entry(succ).or_default().push(block.id);
            }
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
    loc: &(String, u32, u32),
) {
    // BIR lowering normally rewrites CallRuntime → CallExtern. Per-function
    // debug emit (`Backend::emit_function`) bypasses BIR and may hand us a
    // CallRuntime; synthesize a CallExtern locally so the rest of this
    // dispatcher (including its many name-based scans) sees a uniform shape.
    let _synthesized_callextern;
    let inst = if let Inst::CallRuntime { dst, callee, args, arg_abis } = inst {
        _synthesized_callextern = Inst::CallExtern {
            dst: *dst,
            name: callee.c_name().to_string(),
            args: args.clone(),
            arg_abis: arg_abis.clone(),
        };
        &_synthesized_callextern
    } else {
        inst
    };

    match inst {
        // ── Slot Access ─────────────────────────────────────────────
        Inst::SlotStore { slot, value, is_move } => {
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
                // Aggregate slot — value may be a pointer (SlotAddr/FieldPtr) or
                // aggregate by value (Call return). Check val_types.
                let val_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
                // Assume ptr if type is unknown (None) — aggregate values are always ptrs in our model.
                // Also treat scalar types as ptr if the value was produced by a Call/CallClosure
                // returning an aggregate (the emitter stores small-agg returns via alloca,
                // making the value a pointer even though type inference says scalar).
                let val_is_from_agg_call = if !val_ty.map_or(true, |t| t.is_ptr()) {
                    func.blocks.iter().any(|b| b.insts.iter().any(|inst| {
                        match inst {
                            Inst::Call { dst: Some(d), func: fid, .. } if d.0 == value.0 => {
                                let target = &module.functions[fid.0 as usize];
                                target.return_type.is_aggregate()
                            }
                            Inst::CallClosure { dst: Some(d), ret_ty, .. } if d.0 == value.0 => {
                                ret_ty.is_aggregate()
                            }
                            _ => false,
                        }
                    }))
                } else { false };
                let is_ptr_val = val_ty.map_or(true, |t| t.is_ptr()) || val_is_from_agg_call;
                // Check if the source is a NullPtr — use memset(0) instead of memcpy from null.
                let value_is_null = func.blocks.iter().any(|b| {
                    b.insts.iter().any(|i| matches!(i, Inst::NullPtr { dst } if dst.0 == value.0))
                });
                // CoW-aware string Copy: when storing a Ptr into a String slot with Copy
                // semantics (is_move=false), deep-clone through gorget_string_copy_cow.
                // Views (cap=0) become 32-byte struct copies; owned strings are deep-cloned.
                // Mirrors the C backend's path in src/backend/c_lir/mod.rs — without this,
                // shallow memcpy aliases the source buffer and elem_drop/explicit drops
                // conflict into double-frees.
                //
                // Discriminator: read the LIR-level `func.value_types[value]` (which
                // mirrors what the C backend sees), NOT the LLVM-converted `val_types`.
                // The LLVM val_types pre-pass blanket-converts every aggregate `Struct`
                // to `PtrTo`, so it can't distinguish:
                //   • LIR `Ptr`/`PtrTo(_)` — a real pointer/borrow → needs CoW.
                //   • LIR `Struct(_)` (LLVM-converted to `PtrTo`) — a freshly-returned
                //     owned struct from a CallExtern → MUST stay memcpy (CoW would
                //     leak the fresh allocation).
                let slot_is_string = matches!(slot_ty, LirType::Struct(sid)
                    if snames.get(&sid.0).map_or(false, |n| n == "GorgetString" || n == "Str"));
                let lir_val_ty = func.value_types.get(value.0 as usize).and_then(|t| t.as_ref());
                let val_ty_is_ptr = matches!(lir_val_ty, Some(LirType::Ptr) | Some(LirType::PtrTo(_)));
                let value_is_strlit = func.blocks.iter().any(|b| {
                    b.insts.iter().any(|i| matches!(i, Inst::StrLit { dst, .. } if dst.0 == value.0))
                });
                if val_ty_is_ptr && slot_is_string && !*is_move && !value_is_null && !value_is_strlit {
                    writeln!(out, "  call void @gorget_string_copy_cow(ptr sret(%GorgetString) %s{}, ptr %v{})",
                        slot.0, value.0).unwrap();
                } else if is_ptr_val {
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
            } else {
                let sty = llvm_type_full(slot_ty, snames);
                let val_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
                // If slot is integer but value is ptr (e.g. Option alloca stored via auto type
                // inference choosing payload type), use memcpy to treat the slot as raw storage
                // for the Option struct that the ptr points to.
                let slot_is_int = matches!(slot_ty, LirType::I64 | LirType::U64 | LirType::I32 | LirType::U32 | LirType::I16 | LirType::U16 | LirType::I8 | LirType::U8);
                let val_is_ptr = val_ty.map_or(false, |t| matches!(t, LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef));
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
            } else if ty.is_aggregate() {
                // Aggregate types: return a pointer to the slot data (not a by-value load).
                // Downstream code expects ptr and will memcpy or field-access through it.
                writeln!(out, "  %v{} = getelementptr i8, ptr %s{}, i32 0", dst.0, slot.0).unwrap();
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

        // Canonical ops — must have been expanded away by bir::lower before reaching here.
        // The BirModule newtype + validator guarantee this, but keep an explicit arm so
        // the pattern match stays exhaustive.
        Inst::SizeOf { .. }
        | Inst::EnumInit { .. }
        | Inst::EnumCheck { .. }
        | Inst::EnumExtract { .. }
        | Inst::StructInit { .. }
        | Inst::CowClone { .. }
        | Inst::TraitCall { .. }
        | Inst::HofExpand { .. }
        | Inst::AddressOf { .. }
        | Inst::BoxAlloc { .. } => {
            unreachable!("canonical LIR op survived BIR lowering — validator should have rejected it");
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
        Inst::NamedFuncAddr { dst, name } => {
            let cname = c_func_name(name);
            writeln!(out, "  %v{} = bitcast ptr @{cname} to ptr", dst.0).unwrap();
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
                LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef => {
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
                    // For spawn wrappers we declared the param as plain `ptr`
                    // (PCS-compliant ABI for >16-byte composites called from
                    // the C runtime — see the function-def emitter). The
                    // IR-level type is then ptr; alias on entry instead of
                    // attempting `store %S, ptr` which would be ill-typed.
                    let is_spawn_wrapper_fn = crate::lir::queries::is_spawn_wrapper(func);
                    let large_agg = ty.is_aggregate() && !is_small_aggregate(ty, &module.structs);
                    if is_spawn_wrapper_fn && large_agg {
                        writeln!(out, "  %v{} = bitcast ptr %p{index} to ptr", dst.0).unwrap();
                    } else {
                        writeln!(out, "  %v{} = alloca {lty}", dst.0).unwrap();
                        writeln!(out, "  store {lty} %p{index}, ptr %v{}", dst.0).unwrap();
                    }
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
                emit_overflow_check(out, "add", dst, lhs, rhs, ty, snames, block_id, trap_counter, current_label, str_globals, val_types, loc);
            } else if ty.is_float() {
                writeln!(out, "  %v{} = fadd {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            } else {
                writeln!(out, "  %v{} = add {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            }
        }
        Inst::Sub { dst, ty, lhs, rhs, overflow } => {
            let lty = llvm_type(ty);
            if *overflow == Overflow::Trap && ty.is_integer() {
                emit_overflow_check(out, "sub", dst, lhs, rhs, ty, snames, block_id, trap_counter, current_label, str_globals, val_types, loc);
            } else if ty.is_float() {
                writeln!(out, "  %v{} = fsub {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            } else {
                writeln!(out, "  %v{} = sub {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            }
        }
        Inst::Mul { dst, ty, lhs, rhs, overflow } => {
            let lty = llvm_type(ty);
            if *overflow == Overflow::Trap && ty.is_integer() {
                emit_overflow_check(out, "mul", dst, lhs, rhs, ty, snames, block_id, trap_counter, current_label, str_globals, val_types, loc);
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
                // D11: normalize div-by-zero to `trap[T_DivByZero]` + exit 101
                // via gorget_trap_at — byte-identical with the C backend.
                let dz_code_idx = str_globals.intern(crate::trap::TrapKind::DivByZero.code());
                let dz_detail_idx = str_globals.intern("division by zero");
                let dz_file_idx = str_globals.intern(&loc.0);
                writeln!(out, "  call void @gorget_trap_at(ptr @.str.{dz_code_idx}, ptr @.str.{dz_detail_idx}, ptr @.str.{dz_file_idx}, i32 {}, i32 {})", loc.1, loc.2).unwrap();
                writeln!(out, "  unreachable").unwrap();
                writeln!(out, "{ok_label}:").unwrap();
                *current_label = ok_label;
                // Signed `TYPE_MIN/-1` overflow is UB (C-Div traps it; LLVM
                // bare `sdiv` was silent UB) — TRAP it unconditionally, like
                // div0 (error-model.md §11 (E)). Unsigned never overflows.
                if is_signed(ty) {
                    emit_div_overflow_trap(out, "div", dst, lhs, rhs, ty, block_id, trap_counter, current_label, str_globals, loc);
                    let op = "sdiv";
                    writeln!(out, "  %v{} = {op} {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
                } else {
                    writeln!(out, "  %v{} = udiv {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
                }
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
                // D11: normalize rem-by-zero to `trap[T_DivByZero]` + exit 101.
                let dz_code_idx = str_globals.intern(crate::trap::TrapKind::DivByZero.code());
                let dz_detail_idx = str_globals.intern("division by zero");
                let dz_file_idx = str_globals.intern(&loc.0);
                writeln!(out, "  call void @gorget_trap_at(ptr @.str.{dz_code_idx}, ptr @.str.{dz_detail_idx}, ptr @.str.{dz_file_idx}, i32 {}, i32 {})", loc.1, loc.2).unwrap();
                writeln!(out, "  unreachable").unwrap();
                writeln!(out, "{ok_label}:").unwrap();
                *current_label = ok_label;
                // Signed `TYPE_MIN % -1` overflow is UB (C-Div traps `/`; LLVM
                // bare `srem` was silent UB) — TRAP it unconditionally, like
                // div0 (error-model.md §11 (E)). Unsigned never overflows.
                if is_signed(ty) {
                    emit_div_overflow_trap(out, "rem", dst, lhs, rhs, ty, block_id, trap_counter, current_label, str_globals, loc);
                    writeln!(out, "  %v{} = srem {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
                } else {
                    writeln!(out, "  %v{} = urem {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
                }
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
                // Integer modulo: guard div0, AND (signed only) `TYPE_MIN % -1`.
                // Mirror C-Mod (`c_lir/mod.rs`): div0 traps; the signed
                // overflow case produces 0 (the Euclidean result is genuinely 0,
                // unlike Div/Rem which overflow and must trap). The `srem` is
                // UB at INT_MIN/-1 (LLVM LangRef), so we BRANCH around it — a
                // `select` would still execute the poison `srem` on that path.
                let rem_op = if is_signed(ty) { "srem" } else { "urem" };
                let uid = *trap_counter;
                *trap_counter += 1;
                // div0 check (shared shape with Div/Rem).
                let zcmp = format!("modz.{block_id}.{uid}.cmp");
                let ztrap = format!("modz.{block_id}.{uid}.trap");
                let zok = format!("modz.{block_id}.{uid}.ok");
                writeln!(out, "  %{zcmp} = icmp eq {lty} %v{}, 0", rhs.0).unwrap();
                writeln!(out, "  br i1 %{zcmp}, label %{ztrap}, label %{zok}").unwrap();
                writeln!(out, "{ztrap}:").unwrap();
                // D11: normalize mod-by-zero to `trap[T_DivByZero]` + exit 101.
                let dz_code_idx = str_globals.intern(crate::trap::TrapKind::DivByZero.code());
                let dz_detail_idx = str_globals.intern("division by zero");
                let dz_file_idx = str_globals.intern(&loc.0);
                writeln!(out, "  call void @gorget_trap_at(ptr @.str.{dz_code_idx}, ptr @.str.{dz_detail_idx}, ptr @.str.{dz_file_idx}, i32 {}, i32 {})", loc.1, loc.2).unwrap();
                writeln!(out, "  unreachable").unwrap();
                writeln!(out, "{zok}:").unwrap();
                *current_label = zok;
                if is_signed(ty) {
                    // Signed `TYPE_MIN % -1` → 0 (NOT a trap; the Euclidean
                    // result is genuinely 0). Branch around the UB `srem`.
                    let bits = int_bits(ty);
                    let tmin = format!("-{}", 1u128 << (bits - 1)); // INT_MIN as decimal literal
                    let lmin = format!("modov.{block_id}.{uid}.lmin");
                    let rneg1 = format!("modov.{block_id}.{uid}.rneg1");
                    let ovf = format!("modov.{block_id}.{uid}.flag");
                    let ovlabel = format!("modov.{block_id}.{uid}.zero");
                    let normlabel = format!("modov.{block_id}.{uid}.norm");
                    let donelabel = format!("modov.{block_id}.{uid}.done");
                    writeln!(out, "  %{lmin} = icmp eq {lty} %v{}, {tmin}", lhs.0).unwrap();
                    writeln!(out, "  %{rneg1} = icmp eq {lty} %v{}, -1", rhs.0).unwrap();
                    writeln!(out, "  %{ovf} = and i1 %{lmin}, %{rneg1}").unwrap();
                    writeln!(out, "  br i1 %{ovf}, label %{ovlabel}, label %{normlabel}").unwrap();
                    // Normal path: the Euclidean ((a % b) + b) % b.
                    writeln!(out, "{normlabel}:").unwrap();
                    let tmp1 = format!("mod.{}.1", dst.0);
                    let tmp2 = format!("mod.{}.2", dst.0);
                    let normres = format!("mod.{}.norm", dst.0);
                    writeln!(out, "  %{tmp1} = {rem_op} {lty} %v{}, %v{}", lhs.0, rhs.0).unwrap();
                    writeln!(out, "  %{tmp2} = add {lty} %{tmp1}, %v{}", rhs.0).unwrap();
                    writeln!(out, "  %{normres} = {rem_op} {lty} %{tmp2}, %v{}", rhs.0).unwrap();
                    writeln!(out, "  br label %{donelabel}").unwrap();
                    // Overflow path: result is 0.
                    writeln!(out, "{ovlabel}:").unwrap();
                    writeln!(out, "  br label %{donelabel}").unwrap();
                    // Merge.
                    writeln!(out, "{donelabel}:").unwrap();
                    writeln!(out, "  %v{} = phi {lty} [%{normres}, %{normlabel}], [0, %{ovlabel}]", dst.0).unwrap();
                    *current_label = donelabel;
                } else {
                    // Unsigned: no TYPE_MIN issue once div0 is guarded.
                    let tmp1 = format!("mod.{}.1", dst.0);
                    let tmp2 = format!("mod.{}.2", dst.0);
                    writeln!(out, "  %{tmp1} = {rem_op} {lty} %v{}, %v{}", lhs.0, rhs.0).unwrap();
                    writeln!(out, "  %{tmp2} = add {lty} %{tmp1}, %v{}", rhs.0).unwrap();
                    writeln!(out, "  %v{} = {rem_op} {lty} %{tmp2}, %v{}", dst.0, rhs.0).unwrap();
                }
            }
        }
        // Fault-catch checked arithmetic: produce the `i1` FLAG `%v{dst}` (true
        // iff the op would fault) WITHOUT a trap and WITHOUT committing the
        // arithmetic result. The result is computed only on the no-fault
        // continuation path the `Term::Branch` falls through to (so for Div/Rem
        // no division ever executes here). No inline labels are emitted and
        // `trap_counter`/`current_label` are NOT touched, so the
        // `block_exit_labels` pre-pass leaves this block's exit at `bb{N}`,
        // correctly the `Term::Branch` (shared LIR shape, error-model.md §11.2).
        Inst::FaultCheck { dst, op, ty, lhs, rhs } => {
            let lty = llvm_type(ty);
            let bits = int_bits(ty);
            let signed = is_signed(ty);
            // Operands may be wider (i64 constants) or narrower than `ty`; coerce.
            let adjust = |out: &mut String, vid: u32, tag: &str| -> String {
                let actual = val_types.get(vid as usize).and_then(|t| t.as_ref()).cloned();
                let actual_bits = actual.as_ref().map(int_bits).unwrap_or(64);
                if actual_bits == bits {
                    return format!("%v{vid}");
                }
                let from_ty = actual.as_ref().map(llvm_type).unwrap_or("i64");
                let name = format!("fc.{}.{tag}", dst.0);
                if actual_bits > bits {
                    writeln!(out, "  %{name} = trunc {from_ty} %v{vid} to {lty}").unwrap();
                } else {
                    let ext = if signed { "sext" } else { "zext" };
                    writeln!(out, "  %{name} = {ext} {from_ty} %v{vid} to {lty}").unwrap();
                }
                format!("%{name}")
            };
            let lhs_s = adjust(out, lhs.0, "lhs");
            let rhs_s = adjust(out, rhs.0, "rhs");
            match op.overflow_builtin() {
                Some(builtin) => {
                    let sp = if signed { "s" } else { "u" };
                    let intrinsic = format!("@llvm.{sp}{builtin}.with.overflow.i{bits}");
                    let res = format!("fc.{}.res", dst.0);
                    writeln!(out, "  %{res} = call {{ {lty}, i1 }} {intrinsic}({lty} {lhs_s}, {lty} {rhs_s})").unwrap();
                    writeln!(out, "  %v{} = extractvalue {{ {lty}, i1 }} %{res}, 1", dst.0).unwrap();
                }
                None if matches!(op, FaultOp::DivOverflow) => {
                    // Signed `TYPE_MIN/-1` overflow of a Div/Rem — its OWN
                    // condition (split out of div0, error-model.md §11 (C)):
                    // flag = (lhs == TYPE_MIN && rhs == -1). Unsigned never
                    // overflows this way → constant false.
                    if signed {
                        let tmin = format!("-{}", 1u128 << (bits - 1)); // INT_MIN as decimal literal
                        let lmin = format!("fc.{}.lmin", dst.0);
                        let rneg1 = format!("fc.{}.rneg1", dst.0);
                        writeln!(out, "  %{lmin} = icmp eq {lty} {lhs_s}, {tmin}").unwrap();
                        writeln!(out, "  %{rneg1} = icmp eq {lty} {rhs_s}, -1").unwrap();
                        writeln!(out, "  %v{} = and i1 %{lmin}, %{rneg1}", dst.0).unwrap();
                    } else {
                        writeln!(out, "  %v{} = add i1 0, 0", dst.0).unwrap();
                    }
                }
                None => {
                    // Div / Rem div-by-zero ONLY: flag = (rhs == 0).
                    writeln!(out, "  %v{} = icmp eq {lty} {rhs_s}, 0", dst.0).unwrap();
                }
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
            // D11 + owner ruling 2026-07-10: guard shift-count out of range
            // (>= bit width, or negative via the unsigned comparison). LLVM
            // `shl` by an out-of-range count is poison/UB — the C backend
            // already traps it, so ADD the check here so both backends agree on
            // `x << 64` and both normalize to `trap[T_Overflow]` + exit 101.
            let bits = int_bits(ty);
            let uid = *trap_counter;
            *trap_counter += 1;
            let cmp = format!("shl.{block_id}.{uid}.cmp");
            let trap_label = format!("shl.{block_id}.{uid}.trap");
            let ok_label = format!("shl.{block_id}.{uid}.ok");
            writeln!(out, "  %{cmp} = icmp uge {lty} %v{}, {bits}", rhs.0).unwrap();
            writeln!(out, "  br i1 %{cmp}, label %{trap_label}, label %{ok_label}").unwrap();
            writeln!(out, "{trap_label}:").unwrap();
            let sh_code_idx = str_globals.intern(crate::trap::TrapKind::Overflow.code());
            let sh_detail_idx = str_globals.intern("shift out of range");
            let sh_file_idx = str_globals.intern(&loc.0);
            writeln!(out, "  call void @gorget_trap_at(ptr @.str.{sh_code_idx}, ptr @.str.{sh_detail_idx}, ptr @.str.{sh_file_idx}, i32 {}, i32 {})", loc.1, loc.2).unwrap();
            writeln!(out, "  unreachable").unwrap();
            writeln!(out, "{ok_label}:").unwrap();
            *current_label = ok_label;
            writeln!(out, "  %v{} = shl {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
        }
        Inst::Shr { dst, ty, lhs, rhs } => {
            let lty = llvm_type(ty);
            // D11: same out-of-range shift-count guard as Shl (owner ruling
            // 2026-07-10 → T_Overflow), matching the C backend.
            let bits = int_bits(ty);
            let uid = *trap_counter;
            *trap_counter += 1;
            let cmp = format!("shr.{block_id}.{uid}.cmp");
            let trap_label = format!("shr.{block_id}.{uid}.trap");
            let ok_label = format!("shr.{block_id}.{uid}.ok");
            writeln!(out, "  %{cmp} = icmp uge {lty} %v{}, {bits}", rhs.0).unwrap();
            writeln!(out, "  br i1 %{cmp}, label %{trap_label}, label %{ok_label}").unwrap();
            writeln!(out, "{trap_label}:").unwrap();
            let sh_code_idx = str_globals.intern(crate::trap::TrapKind::Overflow.code());
            let sh_detail_idx = str_globals.intern("shift out of range");
            let sh_file_idx = str_globals.intern(&loc.0);
            writeln!(out, "  call void @gorget_trap_at(ptr @.str.{sh_code_idx}, ptr @.str.{sh_detail_idx}, ptr @.str.{sh_file_idx}, i32 {}, i32 {})", loc.1, loc.2).unwrap();
            writeln!(out, "  unreachable").unwrap();
            writeln!(out, "{ok_label}:").unwrap();
            *current_label = ok_label;
            if is_signed(ty) {
                writeln!(out, "  %v{} = ashr {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            } else {
                writeln!(out, "  %v{} = lshr {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            }
        }

        // ── Comparison & Logic (purely scalar — string comparisons are lowered
        // to CallExtern(gorget_str_eq/gorget_str_cmp) by the LIR lowerer) ────
        Inst::Cmp { dst, op, lhs, rhs } => {
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
        }
        Inst::Not { dst, operand } => {
            writeln!(out, "  %v{} = xor i1 %v{}, 1", dst.0, operand.0).unwrap();
        }

        // ── Type Conversions ────────────────────────────────────────
        Inst::IntCast { dst, value, to } => {
            let src_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
            let to_ty = llvm_type(to);
            let src_ty_str = src_ty.map_or("i64", |t| llvm_type(t));
            let src_is_ptr = src_ty.map_or(false, |t| matches!(t, LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef));
            let to_is_ptr = matches!(to, LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef);

            // IntCast to float type → use sitofp/uitofp to convert value (not bitcast)
            if to.is_float() {
                if src_ty.map_or(false, |t| t.is_integer()) {
                    // Integer → Float: convert value
                    let op = if src_ty.map_or(true, is_signed) { "sitofp" } else { "uitofp" };
                    writeln!(out, "  %v{} = {op} {src_ty_str} %v{} to {to_ty}", dst.0, value.0).unwrap();
                } else {
                    writeln!(out, "  %v{} = fadd {to_ty} 0.0, %v{}", dst.0, value.0).unwrap();
                }
            } else if to_is_ptr && src_is_ptr {
                // Ptr → Ptr identity cast: `add ptr 0, ...` is invalid in LLVM.
                // Emit a no-op getelementptr instead.
                writeln!(out, "  %v{} = getelementptr i8, ptr %v{}, i32 0", dst.0, value.0).unwrap();
            } else if to_is_ptr {
                // Integer → Ptr
                writeln!(out, "  %v{} = inttoptr {src_ty_str} %v{} to ptr", dst.0, value.0).unwrap();
            } else if src_is_ptr {
                // Ptr → Integer
                writeln!(out, "  %v{} = ptrtoint ptr %v{} to {to_ty}", dst.0, value.0).unwrap();
            } else {
                let src_bits = src_ty.map_or(64, int_bits);
                let to_bits = int_bits(to);
                if src_ty.map_or(false, |t| t.is_float()) {
                    // Float → Integer: convert value
                    let op = if is_signed(to) { "fptosi" } else { "fptoui" };
                    writeln!(out, "  %v{} = {op} {src_ty_str} %v{} to {to_ty}", dst.0, value.0).unwrap();
                } else if matches!(to, LirType::Bool) {
                    // int → bool is truthiness (nonzero→true), NOT bit-0 truncation. Matches C's (bool)(x).
                    writeln!(out, "  %v{} = icmp ne {src_ty_str} %v{}, 0", dst.0, value.0).unwrap();
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
            // Rust `as`-style saturation via LLVM's fptosi.sat / fptoui.sat:
            // NaN → 0, out-of-range → clamped to TYPE_MIN/TYPE_MAX. A raw
            // `fptosi` / `fptoui` returns poison out of range (UB), which
            // the x86_64 lowering surfaces as INT_MIN — platform-dependent
            // garbage. The saturating intrinsic matches the C backend's
            // handwritten ternary.
            let src_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
            let to_ty = llvm_type(to);
            let src_ty_str = src_ty.map_or("double", |t| llvm_type(t));
            let src_bits = match src_ty { Some(LirType::F32) => 32, _ => 64 };
            let dst_bits = int_bits(to);
            let kind = if is_signed(to) { "fptosi" } else { "fptoui" };
            writeln!(
                out,
                "  %v{} = call {to_ty} @llvm.{kind}.sat.i{dst_bits}.f{src_bits}({src_ty_str} %v{})",
                dst.0, value.0,
            ).unwrap();
        }
        Inst::PtrCast { dst, value } => {
            writeln!(out, "  %v{} = bitcast ptr %v{} to ptr", dst.0, value.0).unwrap();
        }
        Inst::Bitcast { dst, value, to } => {
            let src_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
            let to_ty = llvm_type(to);
            let src_ty_str = src_ty.map_or("i64", |t| llvm_type(t));
            let src_is_ptr = src_ty.map_or(false, |t| t.is_ptr());
            let dst_is_ptr = matches!(to,
                LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef);
            // LLVM's `bitcast` rejects pointer↔int conversions; those need
            // explicit `ptrtoint` / `inttoptr` opcodes. The C backend papers
            // over this with `(int64_t)p` / `(void*)i`. Same-kind casts (ptr→ptr,
            // int→int with equal width) keep using `bitcast`.
            let opcode = match (src_is_ptr, dst_is_ptr) {
                (true, false) => "ptrtoint",
                (false, true) => "inttoptr",
                _ => "bitcast",
            };
            writeln!(out, "  %v{} = {opcode} {src_ty_str} %v{} to {to_ty}", dst.0, value.0).unwrap();
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
            // If the destination came from a FieldPtr, the field's declared type
            // tells us how many bytes to write. A struct field of type `Ptr`
            // wants an 8-byte pointer store regardless of whether the value is
            // a `PtrTo(SomeStruct)` (which is still pointer-typed at the wire).
            // Without this guard, a `StructInit` for a struct like
            // `DictIter { source: ptr; cursor: i64 }` lowers the
            // `source <- &map` field-store into a 152-byte memcpy of the
            // GorgetMap contents, blowing past the 16-byte slot and corrupting
            // the caller's stack.
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
            if let Some(LirType::PtrTo(sid)) = val_ty {
                if matches!(&dest_field_ty, Some(LirType::Ptr) | Some(LirType::PtrTo(_))) {
                    // Pointer-typed field: store the pointer value, don't deep-copy.
                    writeln!(out, "  store ptr %v{}, ptr %v{}", value.0, ptr.0).unwrap();
                } else {
                    // Source is a typed pointer to an aggregate — memcpy the struct contents.
                    let sz = sizeof_lir_type(&LirType::Struct(*sid), &module.structs, snames);
                    writeln!(out, "  call ptr @memcpy(ptr %v{}, ptr %v{}, i64 {sz})", ptr.0, value.0).unwrap();
                }
            } else if matches!(val_ty, Some(LirType::Ptr) | Some(LirType::FuncRef)) {
                // Source is an opaque ptr (e.g. void* from gorget_array_safe_pop/safe_get)
                // or a typed FuncRef (Tier E §8.6). Mirrors c_lir's path — without admitting
                // FuncRef here, FuncAddr→GorgetClosure-field stores collapse to an 8-byte
                // pointer write, leaving `env` uninitialized → SIGSEGV.
                //
                // NULL-ZERO SIZING (move-out field zero): a drop-elaboration move-out
                // zeroes the moved-from struct/enum field via a `store <Null>` into a
                // pointer that is a `Cast`/byte-`getelementptr` result (NOT an
                // `Inst::FieldPtr` dst), so the `dest_field_ty` FieldPtr-scan returns
                // `None` and the fallback would emit an 8-byte `store ptr null` — only
                // the enum tag, leaving the heap String pointer at offset 8 live →
                // double-free. The C oracle (`c_lir/mod.rs:2729-2737`) sizes this from
                // the canonical `ptr_pointee[ptr]` (`func.pointee_types`, recomputed
                // post-optimization). Mirror it: when the value is Null and the
                // canonical pointee of `ptr` is a `Struct(sid)`, `memset` the full
                // struct so the two backends are byte-size-identical. Match
                // `Struct(sid)` ONLY — genuine `Ptr`/`PtrTo` pointer fields stay on the
                // 8-byte path (matching `PtrTo` here would over-zero and diverge from C).
                let null_zero_struct_sid: Option<crate::lir::StructId> = if value_is_null {
                    func.pointee_types.get(ptr.0 as usize)
                        .and_then(|p| p.as_ref())
                        .and_then(|pt| match pt {
                            LirType::Struct(sid) => Some(*sid),
                            _ => None,
                        })
                } else {
                    None
                };
                if let Some(sid) = null_zero_struct_sid {
                    let sz = sizeof_lir_type(&LirType::Struct(sid), &module.structs, snames);
                    writeln!(out, "  call ptr @memset(ptr %v{}, i32 0, i64 {sz})", ptr.0).unwrap();
                } else {
                // If the destination is a FieldPtr to an aggregate field, the ptr is actually
                // a pointer to that struct's data → emit memcpy instead of store ptr.
                match dest_field_ty.clone() {
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
                }
            } else {
                // Generic-iterator-erasure widening (paired with CallClosure's
                // widened ret-ty): when the source value is a CallClosure
                // result whose LIR `ret_ty` was typed-erased narrower than
                // the destination field, the call already emitted as the
                // wider int. Use the destination field type so the store
                // matches the call's actual return width.
                let value_from_call_closure = func.blocks.iter().any(|b| {
                    b.insts.iter().any(|i| matches!(i, Inst::CallClosure { dst: Some(d), .. } if d.0 == value.0))
                });
                let widened_via_callclosure = if value_from_call_closure {
                    match (val_ty, dest_field_ty.as_ref()) {
                        (Some(vt), Some(ft))
                            if (vt.is_integer() || matches!(vt, LirType::Bool))
                                && ft.is_integer()
                                && int_bits(ft) > int_bits(vt) => Some(ft.clone()),
                        _ => None,
                    }
                } else {
                    None
                };
                let ty_str = widened_via_callclosure
                    .as_ref()
                    .map(|t| llvm_type_full(t, snames))
                    .or_else(|| val_ty.map(|t| llvm_type_full(t, snames)))
                    .unwrap_or_else(|| "i64".to_string());
                writeln!(out, "  store {ty_str} %v{}, ptr %v{}", value.0, ptr.0).unwrap();
            }
        }
        Inst::FieldPtr { dst, base, struct_id, field } => {
            let sname = &snames[&struct_id.0];
            let sdef = &module.structs[struct_id.0 as usize];
            if sdef.is_union_layout && *field > 0 {
                // For union-layout enums: { i32 tag, i32 pad, [N x i8] payload }.
                // All variant fields share the payload as a union. The field name
                // encodes the variant (e.g., Triangle_0, Triangle_1). The suffix
                // number is the field's position within that variant.
                let payload_ptr = format!("fptr.{}.payload", dst.0);
                // Field 0 = tag, field 1 = padding, field 2 = payload array
                writeln!(out, "  %{payload_ptr} = getelementptr %{sname}, ptr %v{}, i32 0, i32 2", base.0).unwrap();
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
                    // Find the variant name (e.g., "Triangle") and sum sizes of fields 0..idx.
                    // Each field is named `<Variant>_<idx>`; derive the owning variant by
                    // stripping the trailing `_<idx>` and compare for EXACT equality. A
                    // starts_with test here would wrongly group a variant whose name is a
                    // prefix of another (GICall ⊂ GICallExtern, GIDeref ⊂ GIDerefStore),
                    // counting the longer variant's fields into the shorter's offsets and
                    // writing payload past the struct. Mirrors the C backend's exact
                    // variant grouping (src/backend/c_lir/mod.rs).
                    let prefix = field_name.rsplitn(2, '_').nth(1).unwrap_or(field_name);
                    let mut byte_offset = 0usize;
                    for f in &sdef.fields[1..] {
                        let f_variant = f.0.rsplitn(2, '_').nth(1).unwrap_or(f.0.as_str());
                        if f_variant == prefix {
                            let f_idx = f.0.rsplit('_').next()
                                .and_then(|s| s.parse::<u32>().ok()).unwrap_or(0);
                            if f_idx < variant_field_idx {
                                let fsz = match &f.1 {
                                    LirType::I8 | LirType::U8 | LirType::Bool => 1,
                                    LirType::I16 | LirType::U16 => 2,
                                    LirType::I32 | LirType::U32 | LirType::F32 => 4,
                                    LirType::I64 | LirType::U64 | LirType::F64 | LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef => 8,
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
            } else if (*field as usize) < sdef.fields.len() {
                // Compute byte offset accounting for C alignment padding.
                // Can't use LLVM struct GEP indices because the LLVM struct
                // may have extra padding fields inserted by emit_struct_types.
                //
                // VTable note: emit_struct_types collapses closure-typed fields
                // in `*_VTable` structs to bare `ptr` (matching the C backend).
                // FieldPtr offsets must follow that override — every field is
                // exactly 8 bytes — otherwise method lookup uses the wrong slot.
                let is_vtable = sname.ends_with("_VTable");
                let mut byte_offset = 0usize;
                for fi in 0..(*field as usize) {
                    let (fsz, fa) = if is_vtable {
                        (8usize, 8usize)
                    } else {
                        let fty = &sdef.fields[fi].1;
                        (
                            sizeof_lir_type(fty, &module.structs, snames),
                            crate::lir::lower::types::c_alignof_lir_type(fty, &module.structs),
                        )
                    };
                    byte_offset = (byte_offset + fa - 1) & !(fa - 1);
                    byte_offset += fsz;
                }
                // Align to target field's alignment
                let target_align = if is_vtable {
                    8
                } else {
                    let target_fty = &sdef.fields[*field as usize].1;
                    crate::lir::lower::types::c_alignof_lir_type(target_fty, &module.structs)
                };
                byte_offset = (byte_offset + target_align - 1) & !(target_align - 1);
                if byte_offset == 0 {
                    writeln!(out, "  %v{} = bitcast ptr %v{} to ptr", dst.0, base.0).unwrap();
                } else {
                    writeln!(out, "  %v{} = getelementptr i8, ptr %v{}, i64 {byte_offset}", dst.0, base.0).unwrap();
                }
            } else {
                // Opaque or empty struct — field index is a raw byte offset
                let byte_offset = (*field as usize) * 8;
                if byte_offset == 0 {
                    writeln!(out, "  %v{} = bitcast ptr %v{} to ptr", dst.0, base.0).unwrap();
                } else {
                    writeln!(out, "  %v{} = getelementptr i8, ptr %v{}, i64 {byte_offset}", dst.0, base.0).unwrap();
                }
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
            // Closure→callable wrapping is now done in LIR (ClosurePack).
            // For aggregate params: if value is ptr but param is aggregate, load first
            let mut load_lines = Vec::new();
            let arg_strs: Vec<String> = args.iter().enumerate().map(|(i, a)| {
                let param_ty = if i < target.params.len() { Some(&target.params[i]) } else { None };
                let actual_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                let is_ptr_val = actual_ty.map_or(false, |t| t.is_ptr());
                let is_agg_param = param_ty.map_or(false, |t| t.is_aggregate());

                if is_agg_param && is_ptr_val {
                    let pty = llvm_arg_type(param_ty.unwrap(), snames);
                    let load_name = format!("arg.load.{}.{i}", a.0);
                    load_lines.push(format!("  %{load_name} = load {pty}, ptr %v{}", a.0));
                    format!("{pty} %{load_name}")
                } else {
                    let pty = param_ty.map(|t| llvm_arg_type(t, snames))
                        .unwrap_or_else(|| actual_ty.map(|t| llvm_arg_type(t, snames))
                            .unwrap_or_else(|| "i64".to_string()));
                    // Width mismatch (e.g. byte-literal `IConst` typed I64 flowing
                    // into an i8 param). LLVM verifies arg widths strictly; the C
                    // backend gets implicit C int promotions for free.
                    if let (Some(p), Some(av)) = (param_ty, actual_ty) {
                        let cast = format!("call.coerce.{}.{i}", a.0);
                        if let Some(coerced) = emit_int_coerce(&mut load_lines, &cast, a.0, av, p) {
                            return format!("{pty} {coerced}");
                        }
                    }
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
            // ── Drop guards are now Inst::DropGuardOpen/Close (not CallExtern) ──

            // ── Closure dispatch is now Inst::CallClosure (not CallExtern) ──

            // ── Panic with source location ──────────────────────────
            // Compiler-emit `gorget_panic(msg)` is rewritten to
            // `gorget_panic_at(file, line, col, msg)` so the runtime
            // message carries the call site location. The arg (Str by
            // value or ptr to Str struct) is converted to `const char*`
            // via `gorget_str_to_cstr`, matching the C backend's path in
            // `src/backend/c_lir/emit_call_extern.rs`. Runtime-internal
            // panics keep the 1-arg `gorget_panic` (the runtime wrapper
            // degrades to `<unknown>:0:0`) until per-runtime-fn span
            // plumbing lands in a later phase.
            if name == "gorget_panic" && args.len() == 1 {
                let a = args[0];
                let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                let is_str_struct = matches!(arg_ty,
                    Some(LirType::PtrTo(sid)) | Some(LirType::Struct(sid))
                    if snames.get(&sid.0).map_or(false, |n| n == "GorgetString" || n == "Str"));
                // Intern the file path as a string global; line/col are i32 constants.
                let file_idx = str_globals.intern(&loc.0);
                let msg_arg = if is_str_struct {
                    // GorgetString → const char* via gorget_str_to_cstr.
                    let uid = *trap_counter;
                    *trap_counter += 1;
                    let cstr_name = format!("gp.{block_id}.{uid}.cstr");
                    let str_attr_call = gorget_string_byval_attr(snames);
                    writeln!(out, "  %{cstr_name} = call ptr @gorget_str_to_cstr(ptr {str_attr_call}%v{})", a.0).unwrap();
                    format!("%{cstr_name}")
                } else {
                    // Already an opaque pointer (e.g. const char* literal).
                    format!("%v{}", a.0)
                };
                writeln!(out, "  call void @gorget_panic_at(ptr @.str.{file_idx}, i32 {}, i32 {}, ptr {msg_arg})",
                    loc.1, loc.2).unwrap();
                return;
            }

            // ── Trap with source location (D11) ─────────────────────
            // Compiler-emit `gorget_trap(code, detail)` is rewritten to
            // `gorget_trap_at(code, detail, file, line, col)` threading the
            // call-site span — the SAME machinery as the gorget_panic rewrite
            // above. Both args are marshalled to `const char*` (a str literal
            // is already an opaque ptr; a GorgetString/Str goes through
            // gorget_str_to_cstr).
            if name == "gorget_trap" && args.len() == 2 {
                let file_idx = str_globals.intern(&loc.0);
                let mut marshal_cstr = |a: ValueId| -> String {
                    let aty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                    let is_str_struct = matches!(aty,
                        Some(LirType::PtrTo(sid)) | Some(LirType::Struct(sid))
                        if snames.get(&sid.0).map_or(false, |n| n == "GorgetString" || n == "Str"));
                    if is_str_struct {
                        let uid = *trap_counter;
                        *trap_counter += 1;
                        let cstr_name = format!("gt.{block_id}.{uid}.cstr");
                        let str_attr_call = gorget_string_byval_attr(snames);
                        writeln!(out, "  %{cstr_name} = call ptr @gorget_str_to_cstr(ptr {str_attr_call}%v{})", a.0).unwrap();
                        format!("%{cstr_name}")
                    } else {
                        format!("%v{}", a.0)
                    }
                };
                let code_arg = marshal_cstr(args[0]);
                let detail_arg = marshal_cstr(args[1]);
                writeln!(out, "  call void @gorget_trap_at(ptr {code_arg}, ptr {detail_arg}, ptr @.str.{file_idx}, i32 {}, i32 {})",
                    loc.1, loc.2).unwrap();
                return;
            }

            // ── Newtype constructors ──────────────────────────────
            // If the extern name matches a struct name with exactly 1 field,
            // inline the construction: alloca + store field 0.
            if let Some(d) = dst {
                let newtype_sid = module.structs.iter().enumerate().find_map(|(i, s)| {
                    // Newtype: 1-field struct that is not an Option/Result
                    // (those are 2-3 field flat enums whose name happens to
                    // match a generated extern; read the typed `enum_kind`
                    // instead of name-prefix matching).
                    let is_enum = matches!(s.enum_kind,
                        crate::lir::EnumKind::Option | crate::lir::EnumKind::Result);
                    if (s.name == *name || snames.get(&(i as u32)).map_or(false, |n| n == name))
                        && s.fields.len() == 1 && !is_enum {
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
                // Read typed `struct_aliases` (Phase A residual #2) — a
                // monomorphized collection alias name is registered when its
                // alias-target StructDef is registered. Replaces five
                // `name.starts_with("Vector__"|"Set__"|"Dict__"|"HashMap__"|
                // "HashSet__")` arms with one typed map lookup.
                let is_collection_ctor = module.struct_aliases.contains_key(name)
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
                    // Resolve the alias target's name to pick the runtime
                    // C type (GorgetArray/GorgetMap/GorgetSet) — typed via
                    // struct_aliases (Phase A residual #2).
                    let ret_ty = module.struct_aliases.get(name)
                        .and_then(|sid| module.structs.get(sid.0 as usize))
                        .map(|s| match s.name.as_str() {
                            "GorgetArray" => "%GorgetArray",
                            "GorgetMap" => "%GorgetMap",
                            "GorgetSet" => "%GorgetSet",
                            _ => "%GorgetMap",
                        })
                        .unwrap_or("%GorgetMap");
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
                // All Vector HOFs (each/any/all/map/filter/fold/reduce/count/
                // flat_map/find/find_index) are lowered upstream via
                // `Inst::HofExpand` (scalar + aggregate via closure_call_sigs).
                // The generic callable dispatch fallback below handles opaque
                // callable parameters (Ptr-typed `[fn_ptr, env_ptr]` pairs)
                // where the HofExpand intercept couldn't resolve the sig.
                let closure_arg_peek = if method == "fold" && args.len() >= 3 {
                    Some(args[2])
                } else if args.len() >= 2 {
                    Some(*args.last().unwrap())
                } else {
                    None
                };
                let opaque_callable = closure_arg_peek.is_some_and(|ca| {
                    matches!(val_types.get(ca.0 as usize).and_then(|t| t.as_ref()), Some(LirType::Ptr))
                });
                let needs_inline = opaque_callable;
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

                    // With find/find_index migrated to HofExpand, the
                    // concrete-closure branch is empty — the block is only
                    // entered for opaque Ptr-typed callables, which take
                    // the generic fallback below. Keep `closure_info` in
                    // scope for the fallback's `is_none()` check.
                    let _ = &closure_info;

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
                                        let (init_val, early_val, pred_cmp) = if method == "any" {
                                            ("false", "true", "eq")
                                        } else {
                                            ("true", "false", "ne")
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
                        // gorget_string_push_char takes (GorgetString*, Str c) — Str is
                        // 32 bytes. On aarch64 it's passed by hidden pointer (matches
                        // bare `ptr`). On x86_64 SysV it's memory class on the stack —
                        // emit `byval` so llc lowers it to bytes-on-stack.
                        let str_attr_psc = gorget_string_byval_attr(snames);
                        writeln!(out, "  call void @gorget_string_push_char(ptr %v{}, ptr {}%v{})", args[0].0, str_attr_psc, args[1].0).unwrap();
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
                // gorget_str_to_cstr takes Str by value — annotate with byval on x86_64
                // so the C runtime gets the bytes via memory class on the stack.
                let str_attr_local = gorget_string_byval_attr(snames);
                writeln!(out, "  %{cstr_name} = call ptr @gorget_str_to_cstr(ptr {}%v{arg0})", str_attr_local).unwrap();
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
                        // Truncate i64/i32 → i1 for bool arg. Name must be
                        // function-unique — `trap_counter` resets per block,
                        // so qualify with block id to avoid SSA collisions
                        // when two blocks each emit at counter=0.
                        writeln!(out, "  %bts.{block_id}.{uid} = trunc i64 %v{} to i1", args[0].0).unwrap();
                        writeln!(out, "  call void @gorget_bool_to_str(ptr sret(%GorgetString) %v{}, i1 %bts.{block_id}.{uid})", d.0).unwrap();
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
                                        // Read typed `enum_kind` (Phase A) — set at LIR
                                        // struct registration from GIR's `enum_category`.
                                        return module.structs.get(sid.0 as usize)
                                            .map_or(false, |s| matches!(s.enum_kind,
                                                crate::lir::EnumKind::Option | crate::lir::EnumKind::Result));
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
                                    LirType::I64 | LirType::U64 | LirType::F64 | LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef => 8usize,
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
            // ── Channel__T__recv_timeout → Option wrapping ──
            // The C runtime `gorget_channel_recv_timeout(GorgetChannel*, void* out, int64_t)`
            // returns `int` (1 = received, 0 = timeout) and writes the value through `out`.
            // The Gorget signature wraps that into Option[T]. The auto-generated
            // `Channel__T__recv_timeout` helper in helpers.rs returns the bare T (loses
            // the timeout signal), which mismatches what the LIR call site (and integer
            // calling convention) expects when its declared return type is the 16-byte
            // Option struct. C backend intercepts at emit_call_extern.rs and inlines;
            // mirror that here.
            if name.starts_with("Channel__") && name.ends_with("__recv_timeout") && args.len() >= 2 {
                if let Some(d) = dst {
                    let uid = *trap_counter; *trap_counter += 1;
                    let pfx = format!("recvtmo.{block_id}.{uid}");
                    // Locate the Option struct type from the destination slot or val_types.
                    let opt_sid = func.blocks.iter().flat_map(|b| b.insts.iter()).find_map(|i| {
                        if let Inst::SlotStore { slot, value, .. } = i {
                            if value.0 == d.0 {
                                if let LirType::Struct(sid) = &func.slots[slot.0 as usize].ty {
                                    if module.structs.get(sid.0 as usize)
                                        .map_or(false, |s| s.enum_kind == crate::lir::EnumKind::Option)
                                    {
                                        return Some(*sid);
                                    }
                                }
                            }
                        }
                        None
                    });
                    if let Some(sid) = opt_sid {
                        let opt_def = &module.structs[sid.0 as usize];
                        let opt_ty = format!("%{}", snames.get(&sid.0).cloned().unwrap_or_else(|| opt_def.name.clone()));
                        let payload_ty = opt_def.fields.get(1).map(|(_, t)| t.clone()).unwrap_or(LirType::I64);
                        let payload_llvm = llvm_type_full(&payload_ty, snames);
                        let payload_off = lir_payload_offset(&payload_ty);
                        let opt_size = sizeof_lir_type(&LirType::Struct(sid), &module.structs, snames);
                        // alloca for Option result + scratch for the value
                        writeln!(out, "  %v{} = alloca {opt_ty}", d.0).unwrap();
                        writeln!(out, "  call ptr @memset(ptr %v{}, i32 0, i64 {opt_size})", d.0).unwrap();
                        writeln!(out, "  %{pfx}.val = alloca {payload_llvm}").unwrap();
                        // Channel arg: deref the channel handle pointer to get GorgetChannel*
                        writeln!(out, "  %{pfx}.ch = load ptr, ptr %v{}", args[0].0).unwrap();
                        writeln!(out, "  %{pfx}.rc = call i32 @gorget_channel_recv_timeout(ptr %{pfx}.ch, ptr %{pfx}.val, i64 %v{})", args[1].0).unwrap();
                        writeln!(out, "  %{pfx}.ok = icmp ne i32 %{pfx}.rc, 0").unwrap();
                        writeln!(out, "  br i1 %{pfx}.ok, label %{pfx}.some, label %{pfx}.none").unwrap();
                        writeln!(out, "{pfx}.some:").unwrap();
                        writeln!(out, "  store i32 0, ptr %v{}", d.0).unwrap();
                        writeln!(out, "  %{pfx}.payp = getelementptr i8, ptr %v{}, i64 {payload_off}", d.0).unwrap();
                        let payload_size = sizeof_lir_type(&payload_ty, &module.structs, snames);
                        writeln!(out, "  call ptr @memcpy(ptr %{pfx}.payp, ptr %{pfx}.val, i64 {payload_size})").unwrap();
                        writeln!(out, "  br label %{pfx}.done").unwrap();
                        writeln!(out, "{pfx}.none:").unwrap();
                        writeln!(out, "  store i32 1, ptr %v{}", d.0).unwrap();
                        writeln!(out, "  br label %{pfx}.done").unwrap();
                        writeln!(out, "{pfx}.done:").unwrap();
                        *current_label = format!("{pfx}.done");
                        // Ensure gorget_channel_recv_timeout is declared
                        writeln!(out, "; gorget_channel_recv_timeout is declared via runtime").unwrap();
                        return;
                    }
                }
            }

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

            // Dict HOF variants (`filter`, `fold`, `each`, `any`, `all`)
            // are migrated to `Inst::HofExpand` — the LIR intercept
            // generates loop blocks + `CallClosure` upstream, so LLVM
            // sees primitive IR, not the monomorphized `Dict__K__V__method`
            // extern call. `map` is not implemented.

            // Set HOF variants (`filter`, `fold`, `each`, `any`, `all`)
            // are migrated to `Inst::HofExpand` — the LIR intercept
            // generates loop blocks + `CallClosure` upstream, so LLVM
            // sees primitive IR, not the monomorphized `Set__T__method`
            // extern call. `map` is not implemented.

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
                                    LirType::I64 | LirType::U64 | LirType::F64 | LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef => 8,
                                    LirType::F32 => 4,
                                    LirType::Void => 0,
                                    // Aggregate payload: compute actual size (must match LLVM struct layout)
                                    LirType::Struct(sid) => sizeof_lir_type(&LirType::Struct(*sid), &module.structs, snames),
                                    // Item 7e (Phase 1): resource payloads route through the
                                    // common sizeof for consistency with the surrounding ABI.
                                    LirType::Resource { .. } => sizeof_lir_type(fty, &module.structs, snames),
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

            // ── Option/Result combinator inline expansion ──
            // map, filter, and_then, or_else, or, flatten, unwrap_or_else, flat_map, map_err
            if let Some((type_prefix, method)) = parse_option_result_combinator(name) {
                if let Some(d) = dst {
                    let uid = *trap_counter;
                    *trap_counter += 1;
                    let pfx = format!("comb.{block_id}.{uid}");

                    // Resolve source Option/Result struct
                    let src_sid = find_struct_by_prefix(type_prefix, module);
                    let src_sdef = src_sid.map(|s| &module.structs[s.0 as usize]);
                    // Read typed `enum_kind` (Phase A) — set at LIR struct
                    // registration. Replaces five downstream
                    // `name.starts_with("Result__")` probes.
                    let is_result = src_sdef
                        .map_or(false, |s| s.enum_kind == crate::lir::EnumKind::Result);

                    // Get payload type from field 1 (ok/Some) and field 2 (err/None) if present
                    let payload_ty = src_sdef.and_then(|d| d.fields.get(1))
                        .map(|(_, t)| t.clone()).unwrap_or(LirType::I64);
                    let payload_llvm = llvm_type_full(&payload_ty, snames);
                    let payload_off = lir_payload_offset(&payload_ty);
                    let _payload_is_agg = payload_llvm.starts_with('%');

                    let err_ty = src_sdef.and_then(|d| d.fields.get(2))
                        .map(|(_, t)| t.clone());
                    let _err_llvm = err_ty.as_ref().map(|t| llvm_type_full(t, snames))
                        .unwrap_or_else(|| "i64".to_string());
                    let err_off = err_ty.as_ref().map(|_t| {
                        let ok_size = sizeof_lir_type(&payload_ty, &module.structs, snames);
                        let raw = 8 + ok_size; // tag(4) + pad(4) + ok payload
                        (raw + 7) & !7 // align to 8
                    }).unwrap_or(8);

                    // Resolve closure (args[1]) if method needs one
                    let needs_closure = matches!(method, "map" | "filter" | "and_then"
                        | "or_else" | "unwrap_or_else" | "flat_map" | "map_err");
                    let closure_info = if needs_closure && args.len() >= 2 {
                        resolve_closure_call_fn(args[1], val_types, module)
                    } else { None };

                    // Determine the result struct size for alloca.
                    // For most combinators, result type == source type.
                    // For map/map_err/and_then/filter/flat_map, read from the typed
                    // LirExtern.combinator_result_struct_id (set by the LIR post-pass).
                    let result_sid = match method {
                        "map" | "map_err" | "and_then" | "filter" | "flat_map" => {
                            module.externs.iter().find(|e| e.name == *name)
                                .and_then(|e| e.combinator_result_struct_id)
                                .or(src_sid)
                        }
                        "flatten" => {
                            // Flatten: result is the inner Option/Result (payload type)
                            match &payload_ty {
                                LirType::Struct(inner_sid) => Some(*inner_sid),
                                _ => src_sid,
                            }
                        }
                        _ => src_sid,
                    };
                    let result_size = result_sid.map(|s| {
                        sizeof_lir_type(&LirType::Struct(s), &module.structs, snames)
                    }).unwrap_or(16);
                    let _result_llvm = result_sid.map(|s| llvm_type_full(&LirType::Struct(s), snames))
                        .unwrap_or_else(|| "%Option__int64_t".to_string());

                    // Result payload type (may differ from source for cross-type map)
                    let result_payload_ty = result_sid.and_then(|s| module.structs[s.0 as usize].fields.get(1))
                        .map(|(_, t)| t.clone()).unwrap_or(LirType::I64);
                    let _result_payload_llvm = llvm_type_full(&result_payload_ty, snames);
                    let result_payload_off = lir_payload_offset(&result_payload_ty);

                    // Alloca result
                    writeln!(out, "  %v{} = alloca i8, i64 {result_size}", d.0).unwrap();

                    match method {
                        "or" => {
                            // or(self, other): if self is Some/Ok, return self; else return other.
                            // args[0] = self ptr, args[1] = other ptr/value
                            //
                            // When the user writes `x.or(None)`, the LIR represents the second
                            // argument as a raw null Ptr (no Option-typed slot is allocated).
                            // Without special-casing, the `else` branch would memcpy from NULL
                            // and segfault. Detect it and materialize a None Option in a fresh
                            // alloca so the select+memcpy below stays uniform.
                            let arg1_is_null = args.len() > 1 && func.blocks.iter().any(|b| {
                                b.insts.iter().any(|i| matches!(i, Inst::NullPtr { dst } if dst.0 == args[1].0))
                            });
                            if arg1_is_null {
                                writeln!(out, "  %{pfx}.none_alloca = alloca i8, i64 {result_size}").unwrap();
                                writeln!(out, "  call void @llvm.memset.p0.i64(ptr %{pfx}.none_alloca, i8 0, i64 {result_size}, i1 false)").unwrap();
                                writeln!(out, "  %{pfx}.none_tagp = getelementptr i8, ptr %{pfx}.none_alloca, i32 0").unwrap();
                                writeln!(out, "  store i32 1, ptr %{pfx}.none_tagp ; None").unwrap();
                            }
                            writeln!(out, "  %{pfx}.tagp = getelementptr i8, ptr %v{}, i32 0", args[0].0).unwrap();
                            writeln!(out, "  %{pfx}.tag = load i32, ptr %{pfx}.tagp").unwrap();
                            writeln!(out, "  %{pfx}.is_some = icmp eq i32 %{pfx}.tag, 0").unwrap();
                            // Select source: if Some → self, else → other
                            let other_ptr = if arg1_is_null {
                                format!("%{pfx}.none_alloca")
                            } else if args.len() > 1 {
                                format!("%v{}", args[1].0)
                            } else {
                                format!("%v{}", args[0].0) // shouldn't happen
                            };
                            writeln!(out, "  %{pfx}.src = select i1 %{pfx}.is_some, ptr %v{}, ptr {other_ptr}", args[0].0).unwrap();
                            writeln!(out, "  call void @llvm.memcpy.p0.p0.i64(ptr %v{}, ptr %{pfx}.src, i64 {result_size}, i1 false)", d.0).unwrap();
                        }
                        "flatten" => {
                            // flatten(self): if self is Some(inner), return inner; else return None/Error
                            // The payload IS the inner Option/Result struct.
                            writeln!(out, "  %{pfx}.tagp = getelementptr i8, ptr %v{}, i32 0", args[0].0).unwrap();
                            writeln!(out, "  %{pfx}.tag = load i32, ptr %{pfx}.tagp").unwrap();
                            writeln!(out, "  %{pfx}.is_some = icmp eq i32 %{pfx}.tag, 0").unwrap();
                            // If Some, copy the inner Option from payload offset
                            writeln!(out, "  %{pfx}.inner = getelementptr i8, ptr %v{}, i64 {payload_off}", args[0].0).unwrap();
                            // If None, store tag=1 (None) into result
                            // Use a branch to handle this
                            let then_l = format!("{pfx}.some");
                            let else_l = format!("{pfx}.none");
                            let done_l = format!("{pfx}.done");
                            writeln!(out, "  br i1 %{pfx}.is_some, label %{then_l}, label %{else_l}").unwrap();
                            writeln!(out, "{then_l}:").unwrap();
                            writeln!(out, "  call void @llvm.memcpy.p0.p0.i64(ptr %v{}, ptr %{pfx}.inner, i64 {result_size}, i1 false)", d.0).unwrap();
                            writeln!(out, "  br label %{done_l}").unwrap();
                            writeln!(out, "{else_l}:").unwrap();
                            // Zero the result and set tag = 1
                            writeln!(out, "  call void @llvm.memset.p0.i64(ptr %v{}, i8 0, i64 {result_size}, i1 false)", d.0).unwrap();
                            writeln!(out, "  store i32 1, ptr %v{}", d.0).unwrap();
                            writeln!(out, "  br label %{done_l}").unwrap();
                            writeln!(out, "{done_l}:").unwrap();
                            *current_label = done_l;
                        }
                        "map" | "filter" | "and_then" | "or_else" | "flat_map"
                        | "unwrap_or_else" | "map_err" => {
                            // All branch-based combinators:
                            // 1. Load tag
                            // 2. Branch on tag
                            // 3. Call closure in appropriate branch
                            // 4. Store result
                            writeln!(out, "  %{pfx}.tagp = getelementptr i8, ptr %v{}, i32 0", args[0].0).unwrap();
                            writeln!(out, "  %{pfx}.tag = load i32, ptr %{pfx}.tagp").unwrap();
                            writeln!(out, "  %{pfx}.is_some = icmp eq i32 %{pfx}.tag, 0").unwrap();

                            let then_l = format!("{pfx}.then");
                            let else_l = format!("{pfx}.else");
                            let done_l = format!("{pfx}.done");
                            writeln!(out, "  br i1 %{pfx}.is_some, label %{then_l}, label %{else_l}").unwrap();

                            if let Some((ref call_fn, ref ret_ty, ref params_are_ptr)) = closure_info {
                                let ret_llvm_ty = llvm_type_full(ret_ty, snames);
                                let ret_is_agg = ret_llvm_ty.starts_with('%');
                                let ret_sret = ret_is_agg && !is_small_aggregate(ret_ty, &module.structs);
                                let ret_small_agg = ret_is_agg && is_small_aggregate(ret_ty, &module.structs);
                                let param_by_ptr = params_are_ptr.first().copied().unwrap_or(false);

                                // Helper closure: emit a closure call with one argument at a given pointer.
                                // Returns the name of the value/alloca holding the result.
                                let emit_closure_call = |out: &mut String, label: &str, arg_ptr: &str| -> String {
                                    if ret_sret {
                                        writeln!(out, "  %{pfx}.{label}.tmp = alloca {ret_llvm_ty}").unwrap();
                                        if param_by_ptr {
                                            writeln!(out, "  call void @{call_fn}(ptr sret({ret_llvm_ty}) %{pfx}.{label}.tmp, ptr %v{}, ptr {arg_ptr})", args[1].0).unwrap();
                                        } else {
                                            writeln!(out, "  %{pfx}.{label}.arg = load {payload_llvm}, ptr {arg_ptr}").unwrap();
                                            writeln!(out, "  call void @{call_fn}(ptr sret({ret_llvm_ty}) %{pfx}.{label}.tmp, ptr %v{}, {payload_llvm} %{pfx}.{label}.arg)", args[1].0).unwrap();
                                        }
                                        format!("%{pfx}.{label}.tmp")
                                    } else if ret_small_agg {
                                        if param_by_ptr {
                                            writeln!(out, "  %{pfx}.{label}.rv = call {ret_llvm_ty} @{call_fn}(ptr %v{}, ptr {arg_ptr})", args[1].0).unwrap();
                                        } else {
                                            writeln!(out, "  %{pfx}.{label}.arg = load {payload_llvm}, ptr {arg_ptr}").unwrap();
                                            writeln!(out, "  %{pfx}.{label}.rv = call {ret_llvm_ty} @{call_fn}(ptr %v{}, {payload_llvm} %{pfx}.{label}.arg)", args[1].0).unwrap();
                                        }
                                        writeln!(out, "  %{pfx}.{label}.tmp = alloca {ret_llvm_ty}").unwrap();
                                        writeln!(out, "  store {ret_llvm_ty} %{pfx}.{label}.rv, ptr %{pfx}.{label}.tmp").unwrap();
                                        format!("%{pfx}.{label}.tmp")
                                    } else {
                                        if param_by_ptr {
                                            writeln!(out, "  %{pfx}.{label}.rv = call {ret_llvm_ty} @{call_fn}(ptr %v{}, ptr {arg_ptr})", args[1].0).unwrap();
                                        } else {
                                            writeln!(out, "  %{pfx}.{label}.arg = load {payload_llvm}, ptr {arg_ptr}").unwrap();
                                            writeln!(out, "  %{pfx}.{label}.rv = call {ret_llvm_ty} @{call_fn}(ptr %v{}, {payload_llvm} %{pfx}.{label}.arg)", args[1].0).unwrap();
                                        }
                                        format!("%{pfx}.{label}.rv")
                                    }
                                };

                                // Helper closure: emit a closure call with err payload.
                                let emit_err_closure_call = |out: &mut String, label: &str, arg_ptr: &str| -> String {
                                    let err_param_llvm = err_ty.as_ref().map(|t| llvm_type_full(t, snames))
                                        .unwrap_or_else(|| "i64".to_string());
                                    let err_param_is_agg = err_param_llvm.starts_with('%');
                                    let err_param_by_ptr = params_are_ptr.first().copied().unwrap_or(false);
                                    if ret_sret {
                                        writeln!(out, "  %{pfx}.{label}.tmp = alloca {ret_llvm_ty}").unwrap();
                                        if err_param_by_ptr || err_param_is_agg {
                                            writeln!(out, "  call void @{call_fn}(ptr sret({ret_llvm_ty}) %{pfx}.{label}.tmp, ptr %v{}, ptr {arg_ptr})", args[1].0).unwrap();
                                        } else {
                                            writeln!(out, "  %{pfx}.{label}.earg = load {err_param_llvm}, ptr {arg_ptr}").unwrap();
                                            writeln!(out, "  call void @{call_fn}(ptr sret({ret_llvm_ty}) %{pfx}.{label}.tmp, ptr %v{}, {err_param_llvm} %{pfx}.{label}.earg)", args[1].0).unwrap();
                                        }
                                        format!("%{pfx}.{label}.tmp")
                                    } else if ret_small_agg {
                                        if err_param_by_ptr || err_param_is_agg {
                                            writeln!(out, "  %{pfx}.{label}.rv = call {ret_llvm_ty} @{call_fn}(ptr %v{}, ptr {arg_ptr})", args[1].0).unwrap();
                                        } else {
                                            writeln!(out, "  %{pfx}.{label}.earg = load {err_param_llvm}, ptr {arg_ptr}").unwrap();
                                            writeln!(out, "  %{pfx}.{label}.rv = call {ret_llvm_ty} @{call_fn}(ptr %v{}, {err_param_llvm} %{pfx}.{label}.earg)", args[1].0).unwrap();
                                        }
                                        writeln!(out, "  %{pfx}.{label}.tmp = alloca {ret_llvm_ty}").unwrap();
                                        writeln!(out, "  store {ret_llvm_ty} %{pfx}.{label}.rv, ptr %{pfx}.{label}.tmp").unwrap();
                                        format!("%{pfx}.{label}.tmp")
                                    } else {
                                        if err_param_by_ptr || err_param_is_agg {
                                            writeln!(out, "  %{pfx}.{label}.rv = call {ret_llvm_ty} @{call_fn}(ptr %v{}, ptr {arg_ptr})", args[1].0).unwrap();
                                        } else {
                                            writeln!(out, "  %{pfx}.{label}.earg = load {err_param_llvm}, ptr {arg_ptr}").unwrap();
                                            writeln!(out, "  %{pfx}.{label}.rv = call {ret_llvm_ty} @{call_fn}(ptr %v{}, {err_param_llvm} %{pfx}.{label}.earg)", args[1].0).unwrap();
                                        }
                                        format!("%{pfx}.{label}.rv")
                                    }
                                };

                                // Helper closure: emit a closure call with no payload arg (Option or_else with no err).
                                let emit_closure_call_no_arg = |out: &mut String, label: &str| -> String {
                                    if ret_sret {
                                        writeln!(out, "  %{pfx}.{label}.tmp = alloca {ret_llvm_ty}").unwrap();
                                        writeln!(out, "  call void @{call_fn}(ptr sret({ret_llvm_ty}) %{pfx}.{label}.tmp, ptr %v{})", args[1].0).unwrap();
                                        format!("%{pfx}.{label}.tmp")
                                    } else if ret_small_agg {
                                        writeln!(out, "  %{pfx}.{label}.rv = call {ret_llvm_ty} @{call_fn}(ptr %v{})", args[1].0).unwrap();
                                        writeln!(out, "  %{pfx}.{label}.tmp = alloca {ret_llvm_ty}").unwrap();
                                        writeln!(out, "  store {ret_llvm_ty} %{pfx}.{label}.rv, ptr %{pfx}.{label}.tmp").unwrap();
                                        format!("%{pfx}.{label}.tmp")
                                    } else {
                                        writeln!(out, "  %{pfx}.{label}.rv = call {ret_llvm_ty} @{call_fn}(ptr %v{})", args[1].0).unwrap();
                                        format!("%{pfx}.{label}.rv")
                                    }
                                };

                                match method {
                                    "map" => {
                                        // Some branch: call closure on payload, wrap result
                                        writeln!(out, "{then_l}:").unwrap();
                                        let ok_ptr = format!("%{pfx}.ok_ptr");
                                        writeln!(out, "  {ok_ptr} = getelementptr i8, ptr %v{}, i64 {payload_off}", args[0].0).unwrap();
                                        let call_result = emit_closure_call(out, "map", &ok_ptr);
                                        // Store tag=0 and payload into result
                                        writeln!(out, "  store i32 0, ptr %v{}", d.0).unwrap();
                                        let rp = format!("%{pfx}.rpay");
                                        writeln!(out, "  {rp} = getelementptr i8, ptr %v{}, i64 {result_payload_off}", d.0).unwrap();
                                        if ret_is_agg {
                                            let ret_size = sizeof_lir_type(ret_ty, &module.structs, snames);
                                            writeln!(out, "  call void @llvm.memcpy.p0.p0.i64(ptr {rp}, ptr {call_result}, i64 {ret_size}, i1 false)").unwrap();
                                        } else {
                                            writeln!(out, "  store {ret_llvm_ty} {call_result}, ptr {rp}").unwrap();
                                        }
                                        // Copy error payload for Result types
                                        if is_result {
                                            // Not needed — on the Some/Ok branch there's no error to copy
                                        }
                                        writeln!(out, "  br label %{done_l}").unwrap();

                                        // None branch: tag=1, copy error for Result
                                        writeln!(out, "{else_l}:").unwrap();
                                        writeln!(out, "  store i32 1, ptr %v{}", d.0).unwrap();
                                        if is_result && err_ty.is_some() {
                                            // Copy error payload from source
                                            let src_err = format!("%{pfx}.src_err");
                                            let dst_err = format!("%{pfx}.dst_err");
                                            // Compute result error offset (may differ from source)
                                            let result_err_off = result_sid.and_then(|s| {
                                                let rdef = &module.structs[s.0 as usize];
                                                rdef.fields.get(1).map(|(_, rpay)| {
                                                    let rpay_size = sizeof_lir_type(rpay, &module.structs, snames);
                                                    let raw = 8 + rpay_size;
                                                    (raw + 7) & !7
                                                })
                                            }).unwrap_or(err_off);
                                            let err_size = err_ty.as_ref().map(|t| sizeof_lir_type(t, &module.structs, snames)).unwrap_or(8);
                                            writeln!(out, "  {src_err} = getelementptr i8, ptr %v{}, i64 {err_off}", args[0].0).unwrap();
                                            writeln!(out, "  {dst_err} = getelementptr i8, ptr %v{}, i64 {result_err_off}", d.0).unwrap();
                                            writeln!(out, "  call void @llvm.memcpy.p0.p0.i64(ptr {dst_err}, ptr {src_err}, i64 {err_size}, i1 false)").unwrap();
                                        }
                                        writeln!(out, "  br label %{done_l}").unwrap();

                                        writeln!(out, "{done_l}:").unwrap();
                                        *current_label = done_l;
                                    }
                                    "filter" => {
                                        // Some branch: call predicate closure on payload
                                        writeln!(out, "{then_l}:").unwrap();
                                        let ok_ptr = format!("%{pfx}.ok_ptr");
                                        writeln!(out, "  {ok_ptr} = getelementptr i8, ptr %v{}, i64 {payload_off}", args[0].0).unwrap();
                                        // Predicate returns bool (i1)
                                        if param_by_ptr {
                                            writeln!(out, "  %{pfx}.pred = call i1 @{call_fn}(ptr %v{}, ptr {ok_ptr})", args[1].0).unwrap();
                                        } else {
                                            writeln!(out, "  %{pfx}.farg = load {payload_llvm}, ptr {ok_ptr}").unwrap();
                                            writeln!(out, "  %{pfx}.pred = call i1 @{call_fn}(ptr %v{}, {payload_llvm} %{pfx}.farg)", args[1].0).unwrap();
                                        }
                                        // If predicate true: copy source to result; else: tag=1 (None)
                                        let keep_l = format!("{pfx}.keep");
                                        let drop_l = format!("{pfx}.drop");
                                        writeln!(out, "  br i1 %{pfx}.pred, label %{keep_l}, label %{drop_l}").unwrap();
                                        writeln!(out, "{keep_l}:").unwrap();
                                        writeln!(out, "  call void @llvm.memcpy.p0.p0.i64(ptr %v{}, ptr %v{}, i64 {result_size}, i1 false)", d.0, args[0].0).unwrap();
                                        writeln!(out, "  br label %{done_l}").unwrap();
                                        writeln!(out, "{drop_l}:").unwrap();
                                        writeln!(out, "  call void @llvm.memset.p0.i64(ptr %v{}, i8 0, i64 {result_size}, i1 false)", d.0).unwrap();
                                        writeln!(out, "  store i32 1, ptr %v{}", d.0).unwrap();
                                        writeln!(out, "  br label %{done_l}").unwrap();

                                        // None branch: tag=1
                                        writeln!(out, "{else_l}:").unwrap();
                                        writeln!(out, "  call void @llvm.memset.p0.i64(ptr %v{}, i8 0, i64 {result_size}, i1 false)", d.0).unwrap();
                                        writeln!(out, "  store i32 1, ptr %v{}", d.0).unwrap();
                                        writeln!(out, "  br label %{done_l}").unwrap();

                                        writeln!(out, "{done_l}:").unwrap();
                                        *current_label = done_l;
                                    }
                                    "and_then" | "flat_map" => {
                                        // Some branch: call closure on payload, result IS the full Option/Result
                                        writeln!(out, "{then_l}:").unwrap();
                                        let ok_ptr = format!("%{pfx}.ok_ptr");
                                        writeln!(out, "  {ok_ptr} = getelementptr i8, ptr %v{}, i64 {payload_off}", args[0].0).unwrap();
                                        let call_result = emit_closure_call(out, "at", &ok_ptr);
                                        // Copy closure result to dest (it's already a full Option/Result)
                                        writeln!(out, "  call void @llvm.memcpy.p0.p0.i64(ptr %v{}, ptr {call_result}, i64 {result_size}, i1 false)", d.0).unwrap();
                                        writeln!(out, "  br label %{done_l}").unwrap();

                                        // None/Error branch: propagate None/Error
                                        writeln!(out, "{else_l}:").unwrap();
                                        writeln!(out, "  call void @llvm.memset.p0.i64(ptr %v{}, i8 0, i64 {result_size}, i1 false)", d.0).unwrap();
                                        writeln!(out, "  store i32 1, ptr %v{}", d.0).unwrap();
                                        if is_result && err_ty.is_some() {
                                            let err_size = err_ty.as_ref().map(|t| sizeof_lir_type(t, &module.structs, snames)).unwrap_or(8);
                                            let result_err_off = result_sid.and_then(|s| {
                                                let rdef = &module.structs[s.0 as usize];
                                                rdef.fields.get(1).map(|(_, rpay)| {
                                                    let rpay_size = sizeof_lir_type(rpay, &module.structs, snames);
                                                    let raw = 8 + rpay_size;
                                                    (raw + 7) & !7
                                                })
                                            }).unwrap_or(err_off);
                                            writeln!(out, "  %{pfx}.src_err2 = getelementptr i8, ptr %v{}, i64 {err_off}", args[0].0).unwrap();
                                            writeln!(out, "  %{pfx}.dst_err2 = getelementptr i8, ptr %v{}, i64 {result_err_off}", d.0).unwrap();
                                            writeln!(out, "  call void @llvm.memcpy.p0.p0.i64(ptr %{pfx}.dst_err2, ptr %{pfx}.src_err2, i64 {err_size}, i1 false)").unwrap();
                                        }
                                        writeln!(out, "  br label %{done_l}").unwrap();

                                        writeln!(out, "{done_l}:").unwrap();
                                        *current_label = done_l;
                                    }
                                    "or_else" => {
                                        // Some/Ok branch: copy self to result
                                        writeln!(out, "{then_l}:").unwrap();
                                        writeln!(out, "  call void @llvm.memcpy.p0.p0.i64(ptr %v{}, ptr %v{}, i64 {result_size}, i1 false)", d.0, args[0].0).unwrap();
                                        writeln!(out, "  br label %{done_l}").unwrap();

                                        // None/Error branch: call closure
                                        writeln!(out, "{else_l}:").unwrap();
                                        if is_result && err_ty.is_some() {
                                            // Result or_else: pass error payload to closure
                                            let err_ptr_name = format!("%{pfx}.err_ptr");
                                            writeln!(out, "  {err_ptr_name} = getelementptr i8, ptr %v{}, i64 {err_off}", args[0].0).unwrap();
                                            let call_result = emit_err_closure_call(out, "oe", &err_ptr_name);
                                            writeln!(out, "  call void @llvm.memcpy.p0.p0.i64(ptr %v{}, ptr {call_result}, i64 {result_size}, i1 false)", d.0).unwrap();
                                        } else {
                                            // Option or_else: closure takes no arg
                                            let call_result = emit_closure_call_no_arg(out, "oe");
                                            if ret_is_agg {
                                                writeln!(out, "  call void @llvm.memcpy.p0.p0.i64(ptr %v{}, ptr {call_result}, i64 {result_size}, i1 false)", d.0).unwrap();
                                            } else {
                                                // Scalar — must construct Option struct
                                                writeln!(out, "  store i32 0, ptr %v{}", d.0).unwrap();
                                                let rp = format!("%{pfx}.oe_rpay");
                                                writeln!(out, "  {rp} = getelementptr i8, ptr %v{}, i64 {result_payload_off}", d.0).unwrap();
                                                writeln!(out, "  store {ret_llvm_ty} {call_result}, ptr {rp}").unwrap();
                                            }
                                        }
                                        writeln!(out, "  br label %{done_l}").unwrap();

                                        writeln!(out, "{done_l}:").unwrap();
                                        *current_label = done_l;
                                    }
                                    "unwrap_or_else" => {
                                        // Some/Ok branch: load payload
                                        writeln!(out, "{then_l}:").unwrap();
                                        let ok_ptr = format!("%{pfx}.ok_ptr");
                                        writeln!(out, "  {ok_ptr} = getelementptr i8, ptr %v{}, i64 {payload_off}", args[0].0).unwrap();
                                        // Copy payload to result (unwrap_or_else returns the payload, not Option)
                                        let pay_size = sizeof_lir_type(&payload_ty, &module.structs, snames);
                                        writeln!(out, "  call void @llvm.memcpy.p0.p0.i64(ptr %v{}, ptr {ok_ptr}, i64 {pay_size}, i1 false)", d.0).unwrap();
                                        writeln!(out, "  br label %{done_l}").unwrap();

                                        // None/Error branch: call closure
                                        writeln!(out, "{else_l}:").unwrap();
                                        if is_result && err_ty.is_some() {
                                            let err_ptr_name = format!("%{pfx}.err_ptr2");
                                            writeln!(out, "  {err_ptr_name} = getelementptr i8, ptr %v{}, i64 {err_off}", args[0].0).unwrap();
                                            let call_result = emit_err_closure_call(out, "uoe", &err_ptr_name);
                                            if ret_is_agg {
                                                writeln!(out, "  call void @llvm.memcpy.p0.p0.i64(ptr %v{}, ptr {call_result}, i64 {pay_size}, i1 false)", d.0).unwrap();
                                            } else {
                                                writeln!(out, "  store {ret_llvm_ty} {call_result}, ptr %v{}", d.0).unwrap();
                                            }
                                        } else {
                                            let call_result = emit_closure_call_no_arg(out, "uoe");
                                            if ret_is_agg {
                                                writeln!(out, "  call void @llvm.memcpy.p0.p0.i64(ptr %v{}, ptr {call_result}, i64 {pay_size}, i1 false)", d.0).unwrap();
                                            } else {
                                                writeln!(out, "  store {ret_llvm_ty} {call_result}, ptr %v{}", d.0).unwrap();
                                            }
                                        }
                                        writeln!(out, "  br label %{done_l}").unwrap();

                                        writeln!(out, "{done_l}:").unwrap();
                                        *current_label = done_l;
                                    }
                                    "map_err" => {
                                        // Ok branch: copy self to result
                                        writeln!(out, "{then_l}:").unwrap();
                                        writeln!(out, "  call void @llvm.memcpy.p0.p0.i64(ptr %v{}, ptr %v{}, i64 {result_size}, i1 false)", d.0, args[0].0).unwrap();
                                        writeln!(out, "  br label %{done_l}").unwrap();

                                        // Error branch: call closure on error, wrap in new Result
                                        writeln!(out, "{else_l}:").unwrap();
                                        writeln!(out, "  store i32 1, ptr %v{}", d.0).unwrap();
                                        if err_ty.is_some() {
                                            let err_ptr_name = format!("%{pfx}.me_err_ptr");
                                            writeln!(out, "  {err_ptr_name} = getelementptr i8, ptr %v{}, i64 {err_off}", args[0].0).unwrap();
                                            let call_result = emit_err_closure_call(out, "me", &err_ptr_name);
                                            let result_err_off = result_sid.and_then(|s| {
                                                let rdef = &module.structs[s.0 as usize];
                                                rdef.fields.get(1).map(|(_, rpay)| {
                                                    let rpay_size = sizeof_lir_type(rpay, &module.structs, snames);
                                                    let raw = 8 + rpay_size;
                                                    (raw + 7) & !7
                                                })
                                            }).unwrap_or(err_off);
                                            let new_err_size = sizeof_lir_type(ret_ty, &module.structs, snames);
                                            writeln!(out, "  %{pfx}.me_dst = getelementptr i8, ptr %v{}, i64 {result_err_off}", d.0).unwrap();
                                            if ret_is_agg {
                                                writeln!(out, "  call void @llvm.memcpy.p0.p0.i64(ptr %{pfx}.me_dst, ptr {call_result}, i64 {new_err_size}, i1 false)").unwrap();
                                            } else {
                                                writeln!(out, "  store {ret_llvm_ty} {call_result}, ptr %{pfx}.me_dst").unwrap();
                                            }
                                        }
                                        writeln!(out, "  br label %{done_l}").unwrap();

                                        writeln!(out, "{done_l}:").unwrap();
                                        *current_label = done_l;
                                    }
                                    _ => unreachable!("unhandled combinator method: {method}"),
                                }
                            } else {
                                // Closure not resolved — emit a warning and zero the result
                                writeln!(out, "{then_l}:").unwrap();
                                writeln!(out, "  ; WARNING: could not resolve closure for combinator {name}").unwrap();
                                writeln!(out, "  br label %{done_l}").unwrap();
                                writeln!(out, "{else_l}:").unwrap();
                                writeln!(out, "  br label %{done_l}").unwrap();
                                writeln!(out, "{done_l}:").unwrap();
                                *current_label = done_l;
                                writeln!(out, "  call void @llvm.memset.p0.i64(ptr %v{}, i8 0, i64 {result_size}, i1 false)", d.0).unwrap();
                            }
                        }
                        _ => {
                            writeln!(out, "  ; TODO: combinator {method} for {name}").unwrap();
                            writeln!(out, "  call void @llvm.memset.p0.i64(ptr %v{}, i8 0, i64 {result_size}, i1 false)", d.0).unwrap();
                        }
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
                            // Restate the VARARGS function type on the call. Without
                            // the `(ptr, ptr, ...)` signature LLVM lowers this as a
                            // call to a fixed-arity prototype and omits the x86_64
                            // SysV vararg vector-register count in %al → variadic
                            // `%f`/double args are read as 0.0 by an -O2 runtime
                            // (`Vec2(dx=0.0…)` / struct-derive float-field bug).
                            // Matches the printf/fprintf siblings below; the sret
                            // first param keeps the 32-byte GorgetString return ABI.
                            writeln!(out, "  call void (ptr, ptr, ...) @{call_name}({sret_all})").unwrap();
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
                                if sdef.enum_kind == crate::lir::EnumKind::Option {
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
                                // Aggregate params are declared as ptr in extern (C ABI).
                                // For large aggregates (>16 bytes) on x86_64,
                                // attach `byval(...)` so the call matches the
                                // SysV memory-class C convention.
                                let attr = if expected.map_or(false, |t| !is_small_aggregate(t, &module.structs)) {
                                    large_agg_byval_attr(expected.unwrap(), snames)
                                } else {
                                    String::new()
                                };
                                format!("ptr {}%v{}", attr, a.0)
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
            // The LIR registers the extern under the ORIGINAL name (gorget_str_strip)
            // with the 2-arg RuntimeFn signature, so the lookup must use the ORIGINAL
            // name — that gives us the proper ABI tags (GorgetString → byval) for the
            // single Str arg. The call instruction itself uses the renamed symbol.
            let original_name: &str = name.as_str();
            let name: &str = if args.len() == 1 {
                match name.as_str() {
                    "gorget_str_strip" => "gorget_str_trim",
                    "gorget_str_lstrip" => "gorget_str_lstrip_ws",
                    "gorget_str_rstrip" => "gorget_str_rstrip_ws",
                    _ => name.as_str(),
                }
            } else { name.as_str() };
            // (Removed gorget_regex_find_at alias — xtd.regex is pure Gorget now.)
            let lookup_name: &str = original_name;

            // gorget_str_cmp: C returns int (32-bit). On aarch64, 'mov w0, -1' zero-extends
            // x0 to 0xFFFFFFFF (not 0xFFFFFFFFFFFFFFFF). Must call as i32 and sext to i64
            // so the LIR I64 result has the correct sign.
            if name == "gorget_str_cmp" {
                // Both args are Str-by-value (32-byte struct). On x86_64 SysV
                // these go via memory class — annotate with `byval` so llc
                // copies the bytes onto the outgoing stack frame; bare `ptr`
                // would put pointers in registers and the C side would read
                // garbage.
                let str_attr = gorget_string_byval_attr(snames);
                if let Some(d) = dst {
                    let uid = *trap_counter;
                    *trap_counter += 1;
                    let pfx = format!("strcmp32.{block_id}.{uid}");
                    let arg_strs: Vec<String> = args.iter().map(|a| format!("ptr {}%v{}", str_attr, a.0)).collect();
                    writeln!(out, "  %{pfx}.raw = call i32 @gorget_str_cmp({})", arg_strs.join(", ")).unwrap();
                    writeln!(out, "  %v{} = sext i32 %{pfx}.raw to i64", d.0).unwrap();
                } else {
                    let arg_strs: Vec<String> = args.iter().map(|a| format!("ptr {}%v{}", str_attr, a.0)).collect();
                    writeln!(out, "  call i32 @gorget_str_cmp({})", arg_strs.join(", ")).unwrap();
                }
                return;
            }

            // Look up the extern declaration (use the original LIR name, before
            // any C-symbol rename above — externs are registered under the LIR name).
            let ext = module.externs.iter().find(|e| e.name == lookup_name);
            if let Some(ext) = ext {
                // CStr return ABI: function returns const char*. Two consumer
                // patterns coexist in our LIR:
                //   • "Implicit-String": LIR feeds the result straight into a
                //     `slot_store s_str, v` (treats it as a wrapped GorgetString,
                //     e.g. `String b = path_basename(p)`). Here we MUST wrap at
                //     the call site, otherwise the slot_store memcpys 8 bytes
                //     of cstr ptr into a 32-byte struct slot.
                //   • "Explicit-cstr": LIR null-checks the raw cstr (`cmp.ne v, null`)
                //     and explicitly wraps via `gorget_str_from_cstr(v)` in the
                //     non-null branch (the lifts.rs nullable-cstr → Option/Result
                //     path). Here we must NOT wrap, otherwise `%v` is the alloca
                //     pointer (never null) and the cmp always picks the wrong
                //     branch, plus the LIR's explicit wrap reads the alloca's
                //     bytes as a cstr → garbage error message.
                //
                // Disambiguate by looking ahead at how `dst` is consumed:
                if ext.return_abi == crate::ir::abi::AbiKind::CStr {
                    if let Some(d) = dst {
                        let uid = *trap_counter;
                        *trap_counter += 1;
                        let pfx = format!("cstr_ret.{block_id}.{uid}");
                        // Lookahead: is `d` consumed by `cmp.ne d, null` or
                        // `gorget_str_from_cstr(d)` anywhere in the function?
                        // If so, downstream code expects the raw cstr.
                        let consumed_as_cstr = func.blocks.iter().any(|b| {
                            b.insts.iter().any(|i| match i {
                                Inst::Cmp { lhs, rhs, op: CmpOp::Eq | CmpOp::Ne, .. } => {
                                    let other = if lhs.0 == d.0 { Some(*rhs) }
                                                else if rhs.0 == d.0 { Some(*lhs) }
                                                else { None };
                                    other.map_or(false, |o| {
                                        b.insts.iter().any(|j| matches!(j, Inst::NullPtr { dst } if dst.0 == o.0))
                                            || func.blocks.iter().any(|b2| b2.insts.iter().any(|j| matches!(j, Inst::NullPtr { dst } if dst.0 == o.0)))
                                    })
                                }
                                Inst::CallExtern { name: n, args: a, .. } => {
                                    n == "gorget_str_from_cstr" && a.iter().any(|x| x.0 == d.0)
                                }
                                _ => false,
                            })
                        });
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
                        if consumed_as_cstr {
                            // Bind %v{d} to the raw cstr ptr — caller does its own null-check / explicit wrap.
                            writeln!(out, "  %v{} = call ptr @{}({})", d.0, name, arg_strs2.join(", ")).unwrap();
                        } else {
                            // Implicit-String consumer — wrap to GorgetString via sret alloca.
                            writeln!(out, "  %{pfx}.raw = call ptr @{}({})", name, arg_strs2.join(", ")).unwrap();
                            writeln!(out, "  %v{} = alloca %GorgetString", d.0).unwrap();
                            writeln!(out, "  call void @gorget_str_from_cstr(ptr sret(%GorgetString) %v{}, ptr %{pfx}.raw)", d.0).unwrap();
                        }
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
                // __gorget_box_alloc_<inner>: same override as the extern declaration
                // emitter — the C-side takes the inner type by value, regardless of
                // how the LIR registered the param (typically Ptr because the SSA
                // operand holds a pointer to the inner). Without this, the call site
                // would lower the second-arg branch as plain `ptr` instead of the
                // byval/small-agg ABI required on x86_64 SysV. Primitive inners
                // (int64_t/double/etc.) miss the snames lookup and fall through to
                // the scalar-spill path below — same as the decl emitter.
                let box_alloc_inner_call: Option<LirType> = name
                    .strip_prefix("__gorget_box_alloc_")
                    .and_then(|suffix| struct_sid_by_name(snames, suffix))
                    .map(|sid| LirType::Struct(StructId(sid)));
                let arg_strs: Vec<String> = args.iter().enumerate().map(|(i, a)| {
                    let expected_ty = if let (0, Some(inner)) = (i, &box_alloc_inner_call) {
                        Some(inner)
                    } else if i < ext.params.len() {
                        Some(&ext.params[i])
                    } else {
                        None
                    };
                    let actual_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                    let expects_ptr = expected_ty.map_or(false, |t| t.is_ptr());
                    let is_ptr = actual_ty.map_or(false, |t| t.is_ptr());

                    let expects_agg = expected_ty.map_or(false, |t| t.is_aggregate());

                    // Detect GorgetString value passed to const char* param: trust the
                    // CStr ABI tag on the extern's param_abis (registered by
                    // RuntimeFn::resolve_lir_sig in src/lir/runtime.rs).
                    let param_abi = ext.param_abis.get(i).copied().unwrap_or_default();
                    let _ = expects_ptr; // kept to preserve surrounding structure
                    let is_str_to_cstr = param_abi == crate::ir::abi::AbiKind::CStr
                        && match actual_ty {
                        Some(LirType::PtrTo(sid)) if snames.get(&sid.0).map_or(false, |n| n == "GorgetString") => {
                            true
                        }
                        Some(LirType::Ptr) => true, // raw ptr to a Str struct (e.g. address_of strlit)
                        _ => false,
                    };

                    let expects_small_agg = expected_ty.map_or(false, |t| {
                        t.is_aggregate() && is_small_aggregate(t, &module.structs)
                    });
                    // CStr param ABI takes precedence over the aggregate-passing
                    // branches: even though the LIR declares the param as a Str
                    // struct, the runtime expects `const char*`. The
                    // `expects_agg && is_ptr` arm would otherwise just pass the
                    // 32-byte struct address through, and the runtime would
                    // read the {data, cap, len, alloc} header as if it were the
                    // C string content — garbled output for write_stdin etc.
                    if is_str_to_cstr {
                        let cstr_name = format!("cstr.{block_id}.{ext_uid}.{i}");
                        // gorget_str_to_cstr takes Str by value — see large_agg_byval_attr.
                        let str_attr_call = gorget_string_byval_attr(snames);
                        spill_lines.push(format!("  %{cstr_name} = call ptr @gorget_str_to_cstr(ptr {}%v{})", str_attr_call, a.0));
                        format!("ptr %{cstr_name}")
                    } else if matches!(param_abi,
                        crate::ir::abi::AbiKind::GorgetString | crate::ir::abi::AbiKind::ByValue
                    ) && is_ptr {
                        // Param ABI says "Str by value" but the LIR type is Ptr (the
                        // SSA operand is the address of a strlit struct). Force byval
                        // so x86_64 SysV emits a memory-class stack copy instead of
                        // a bare pointer-in-register.
                        let attr = gorget_string_byval_attr(snames);
                        format!("ptr {}%v{}", attr, a.0)
                    } else if param_abi == crate::ir::abi::AbiKind::Ptr && expects_agg && is_ptr {
                        // The extern declares the param as a pointer (`T*` in
                        // Gorget extern syntax). LIR keeps the type as the
                        // inner struct, but the call site must pass `ptr`
                        // unchanged — caller's slot already has the struct
                        // bytes there, ptr is the correct ABI for the
                        // runtime fn that takes `T*`.
                        format!("ptr %v{}", a.0)
                    } else if expects_small_agg && is_ptr {
                        // Small aggregate (≤16 bytes): load from ptr and pass by value
                        let agg_ty = llvm_type_full(expected_ty.unwrap(), snames);
                        let load_name = format!("aggload.{block_id}.{ext_uid}.{i}");
                        spill_lines.push(format!("  %{load_name} = load {agg_ty}, ptr %v{}", a.0));
                        format!("{agg_ty} %{load_name}")
                    } else if expects_agg && is_ptr {
                        // Large aggregate (>16 bytes): pass pointer (indirect C ABI).
                        // x86_64 SysV needs `byval(...)` so llc lowers it to a
                        // memory-class stack copy at the call site — matches
                        // the C-side `Str s` parameter that gcc/clang compile
                        // as stack-passed bytes. aarch64 leaves it bare `ptr`
                        // (AAPCS64 implicit-pointer convention).
                        let attr = large_agg_byval_attr(expected_ty.unwrap(), snames);
                        format!("ptr {}%v{}", attr, a.0)
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
                        // Width mismatch (e.g. `gorget_str_byte_at` returns i8
                        // flowing into a `gorget_char_chr(i64)` param). LLVM
                        // verifies arg widths strictly.
                        if let (Some(p), Some(av)) = (expected_ty, actual_ty) {
                            let cast = format!("ext.coerce.{block_id}.{ext_uid}.{i}");
                            if let Some(coerced) = emit_int_coerce(&mut spill_lines, &cast, a.0, av, p) {
                                return format!("{pty} {coerced}");
                            }
                        }
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

            // Bridge wiring for user-keyed Dict/Set is now an explicit
            // `Inst::SetCollectionBridge` emitted by the LIR pass
            // `wire_collection_bridges`. It compiles inline in the
            // SetCollectionBridge arm above. No post-call hook needed.
        }
        Inst::CallPtr { dst, callee, args, ret_ty: call_ret_ty } => {
            // Indirect call through function pointer. Honor the LIR-declared
            // return type — for aggregate returns the underlying function uses
            // sret convention, so we allocate an out slot and pass it as the
            // first arg. (Legacy emission paths that left ret_ty=Void still
            // get scalar i64 by default when dst is set.)
            let arg_strs: Vec<String> = args.iter().map(|a| {
                let pty = val_types.get(a.0 as usize)
                    .and_then(|t| t.as_ref())
                    .map(|t| llvm_arg_type(t, snames))
                    .unwrap_or_else(|| "i64".to_string());
                format!("{pty} %v{}", a.0)
            }).collect();

            if let Some(d) = dst {
                if !matches!(call_ret_ty, LirType::Void) && needs_sret(call_ret_ty, &module.structs) {
                    let ret_ty = llvm_type_full(call_ret_ty, snames);
                    writeln!(out, "  %v{} = alloca {ret_ty}", d.0).unwrap();
                    let prepended = if arg_strs.is_empty() {
                        format!("ptr sret({ret_ty}) %v{}", d.0)
                    } else {
                        format!("ptr sret({ret_ty}) %v{}, {}", d.0, arg_strs.join(", "))
                    };
                    writeln!(out, "  call void %v{}({prepended})", callee.0).unwrap();
                } else if !matches!(call_ret_ty, LirType::Void) && call_ret_ty.is_aggregate() {
                    // Small aggregate: returned in registers, stored to alloca
                    let ret_ty = llvm_type_full(call_ret_ty, snames);
                    writeln!(out, "  %v{}.ret = call {ret_ty} %v{}({})", d.0, callee.0, arg_strs.join(", ")).unwrap();
                    writeln!(out, "  %v{} = alloca {ret_ty}", d.0).unwrap();
                    writeln!(out, "  store {ret_ty} %v{}.ret, ptr %v{}", d.0, d.0).unwrap();
                } else {
                    let ret_ty = if matches!(call_ret_ty, LirType::Void) {
                        "i64".to_string() // legacy fallback when ret_ty wasn't plumbed
                    } else {
                        llvm_type_full(call_ret_ty, snames)
                    };
                    writeln!(out, "  %v{} = call {ret_ty} %v{}({})", d.0, callee.0, arg_strs.join(", ")).unwrap();
                }
            } else {
                let ret_ty = if matches!(call_ret_ty, LirType::Void) {
                    "void".to_string()
                } else {
                    llvm_type_full(call_ret_ty, snames)
                };
                writeln!(out, "  call {ret_ty} %v{}({})", callee.0, arg_strs.join(", ")).unwrap();
            }
        }
        // Tier E §8.6: typed indirect call through a `LirType::FuncRef`.
        // Identical lowering to `CallPtr` (FuncRef → ptr at the LLVM ABI).
        // The IR-level distinction exists for a future WASM backend that
        // would emit `call_indirect <table-index>` here.
        Inst::CallByRef { dst, fref, args, ret_ty: call_ret_ty } => {
            let arg_strs: Vec<String> = args.iter().map(|a| {
                let pty = val_types.get(a.0 as usize)
                    .and_then(|t| t.as_ref())
                    .map(|t| llvm_arg_type(t, snames))
                    .unwrap_or_else(|| "i64".to_string());
                format!("{pty} %v{}", a.0)
            }).collect();

            if let Some(d) = dst {
                if !matches!(call_ret_ty, LirType::Void) && needs_sret(call_ret_ty, &module.structs) {
                    let ret_ty = llvm_type_full(call_ret_ty, snames);
                    writeln!(out, "  %v{} = alloca {ret_ty}", d.0).unwrap();
                    let prepended = if arg_strs.is_empty() {
                        format!("ptr sret({ret_ty}) %v{}", d.0)
                    } else {
                        format!("ptr sret({ret_ty}) %v{}, {}", d.0, arg_strs.join(", "))
                    };
                    writeln!(out, "  call void %v{}({prepended})", fref.0).unwrap();
                } else if !matches!(call_ret_ty, LirType::Void) && call_ret_ty.is_aggregate() {
                    let ret_ty = llvm_type_full(call_ret_ty, snames);
                    writeln!(out, "  %v{}.ret = call {ret_ty} %v{}({})", d.0, fref.0, arg_strs.join(", ")).unwrap();
                    writeln!(out, "  %v{} = alloca {ret_ty}", d.0).unwrap();
                    writeln!(out, "  store {ret_ty} %v{}.ret, ptr %v{}", d.0, d.0).unwrap();
                } else {
                    let ret_ty = if matches!(call_ret_ty, LirType::Void) {
                        "i64".to_string()
                    } else {
                        llvm_type_full(call_ret_ty, snames)
                    };
                    writeln!(out, "  %v{} = call {ret_ty} %v{}({})", d.0, fref.0, arg_strs.join(", ")).unwrap();
                }
            } else {
                let ret_ty = if matches!(call_ret_ty, LirType::Void) {
                    "void".to_string()
                } else {
                    llvm_type_full(call_ret_ty, snames)
                };
                writeln!(out, "  call {ret_ty} %v{}({})", fref.0, arg_strs.join(", ")).unwrap();
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
            // Bake the panic location prefix into the fprintf format. The
            // index/len ints are still passed at runtime.
            let bc_fmt = format!("{}:{}:{}: index out of bounds: index %lld, len %lld\n", loc.0, loc.1, loc.2);
            let fmt_idx = str_globals.intern(&bc_fmt);
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
            let dc_fmt = format!("{}:{}:{}: division by zero\n", loc.0, loc.1, loc.2);
            let fmt_idx = str_globals.intern(&dc_fmt);
            let se = format!("dc.{block_id}.{trap_id}.stderr");
            writeln!(out, "  %{se} = load ptr, ptr @stderr").unwrap();
            writeln!(out, "  call i32 (ptr, ptr, ...) @fprintf(ptr %{se}, ptr @.str.{fmt_idx})").unwrap();
            writeln!(out, "  call void @exit(i32 1)").unwrap();
            writeln!(out, "  unreachable").unwrap();
            writeln!(out, "{ok_label}:").unwrap();
            *current_label = ok_label;
        }
        Inst::Trap { msg } => {
            // Bake `file:line:col:` prefix in front of the trap message.
            // Synthetic instructions and missing file infos fall back to
            // `<unknown>:0:0` via `resolve_panic_loc`.
            let trap_msg = format!("{}:{}:{}: {}", loc.0, loc.1, loc.2, msg);
            let fmt_idx = str_globals.intern(&trap_msg);
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

        // ── ClosurePack ─────────────────────────────────────────────
        Inst::ClosurePack { slot, env_ptr, call_func, needs_adapter } => {
            let raw_name = &module.functions[call_func.0 as usize].name;
            let fn_name = if *needs_adapter {
                format!("__adapt_{}", c_func_name(raw_name))
            } else {
                raw_name.clone()
            };
            let uid = format!("cp.{}.{}", slot.0, env_ptr.0);
            // Store fn_ptr to GorgetClosure.fn_ptr (field 0)
            writeln!(out, "  %{uid}.fpgep = getelementptr %GorgetClosure, ptr %s{}, i32 0, i32 0", slot.0).unwrap();
            writeln!(out, "  store ptr @{fn_name}, ptr %{uid}.fpgep").unwrap();
            // Store env_ptr to GorgetClosure.env (field 1)
            writeln!(out, "  %{uid}.envgep = getelementptr %GorgetClosure, ptr %s{}, i32 0, i32 1", slot.0).unwrap();
            writeln!(out, "  store ptr %v{}, ptr %{uid}.envgep", env_ptr.0).unwrap();
        }

        // ── MoveSlot (consumed by drop elaboration) ────────────────
        Inst::MoveSlot { .. } => {}

        Inst::CallClosure { dst, kind, closure, args, ret_ty, arg_abis } => {
            let uid = *trap_counter;
            *trap_counter += 1;
            let pfx = format!("cc.{block_id}.{uid}");
            // Load fn_ptr and env from the closure.
            match kind {
                ClosureDispatchKind::CallableParam => {
                    // void*[2]: fn_ptr at [0], env at [1]
                    writeln!(out, "  %{pfx}.fnp = load ptr, ptr %v{}", closure.0).unwrap();
                    writeln!(out, "  %{pfx}.envgep = getelementptr ptr, ptr %v{}, i32 1", closure.0).unwrap();
                    writeln!(out, "  %{pfx}.env = load ptr, ptr %{pfx}.envgep").unwrap();
                }
                ClosureDispatchKind::EscapedClosure => {
                    // GorgetClosure struct: fn_ptr field 0, env field 1
                    writeln!(out, "  %{pfx}.fpgep = getelementptr %GorgetClosure, ptr %v{}, i32 0, i32 0", closure.0).unwrap();
                    writeln!(out, "  %{pfx}.fnp = load ptr, ptr %{pfx}.fpgep").unwrap();
                    writeln!(out, "  %{pfx}.envgep = getelementptr %GorgetClosure, ptr %v{}, i32 0, i32 1", closure.0).unwrap();
                    writeln!(out, "  %{pfx}.env = load ptr, ptr %{pfx}.envgep").unwrap();
                }
            }
            // Build arg list: env first, then user args.
            //
            // Closure ABI per-arg must match `__Closure_N__call` (see
            // `ir/lowering/closures.rs::resolve_param_type`) and the
            // `__adapt_*` shim (above in this file), which use by-pointer
            // (`ptr`) for resource-containing aggregates (String / Vector /
            // Dict / Set / HashMap, or any user struct transitively holding
            // one of those) and by-value for plain non-resource aggregates
            // like `Option[int]`. ByValue promotion (LIR `arg_abis`) is
            // honoured only when the pointee is non-resource; resource
            // aggregates always travel by pointer to match the callee.
            // Mismatching these is silent on AAPCS64 and SIGSEGVs on
            // x86-64 SysV — see `backend/c_lir/mod.rs` CallClosure.
            let mut call_arg_strs = vec![format!("ptr %{pfx}.env")];
            for (ai, a) in args.iter().enumerate() {
                let vt = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                let abi = arg_abis.get(ai).copied().unwrap_or_default();
                // BIR's expand_filter / each / map / fold emit `arg_abis = Scalar`
                // when the closure declared the param as a struct-by-value
                // (`AbiKind::ByValue` from abi_from_param_ty), then BIR's
                // `Inst::Load { ty: aggregate }` aliases the elem pointer
                // (LLVM keeps aggregates as ptrs). The closure's __call body
                // still declares `param: %Struct` — passing `ptr` here is an
                // ABI mismatch.
                //
                // The closure-emit path always declares aggregate params as
                // struct-by-value (LIR func.params carries the LIR `Struct`
                // type), regardless of whether the struct contains resources.
                // The call site must therefore load + pass by value to match,
                // even for resource-containing aggregates like `Result[int,
                // String]`. The comment elsewhere about "resource aggregates
                // travel by pointer" applies to bare-fn callees and
                // `__adapt_*` shims, not to `__Closure_N__call` bodies.
                // For tuple-by-value > 16 B (e.g. `((int, int, int))` 24-byte
                // destructure), `Inst::CallClosure` lowering at insts.rs:3287
                // only promotes Auto→ByValue for `is_small_aggregate` types
                // (≤16 B). The closure body's declared param type, however, is
                // always the bare struct (`resolve_param_type` returns the
                // base for non-resource Borrows). Caller-side `Auto` then
                // falls through to passing `ptr`, while the callee reads it as
                // struct value — first slot becomes the pointer's address.
                //
                // The shared `compute_module_pointee_types` pass populates
                // `func.pointee_types` for every pointer-typed value via the
                // canonical pointer-producing instructions (`SlotAddr` /
                // `FieldPtr` / `GlobalAddr`) and propagates through
                // `SlotStore`→`SlotLoad` chains, casts, and block params.
                // For Auto-abi'd callable args we look up the pointee — when
                // it's a non-resource `Struct`, that means a locally-built
                // tuple/struct value and the closure body declared the param
                // by-value, so the call site must load + pass by value. A
                // `Counter&`-style param-of-Ptr has no pointee tracked
                // (params don't go through SlotAddr/FieldPtr) so the
                // disambiguation falls out naturally.
                let auto_struct_pointee: Option<crate::lir::StructId> = func
                    .pointee_types.get(a.0 as usize)
                    .and_then(|p| p.as_ref())
                    .and_then(|pt| match pt {
                        LirType::Struct(sid) => Some(*sid),
                        _ => None,
                    });
                let by_value_sid = if matches!(abi,
                    crate::ir::abi::AbiKind::ByValue | crate::ir::abi::AbiKind::Scalar)
                {
                    match vt {
                        Some(LirType::PtrTo(sid)) => Some(*sid),
                        _ => None,
                    }
                } else if matches!(abi, crate::ir::abi::AbiKind::Auto) {
                    // Non-small-aggregate `Auto` only widens when the
                    // pointer's pointee is a non-resource struct (i.e. a
                    // locally-built tuple/struct value, not a borrow).
                    auto_struct_pointee.and_then(|sid| {
                        if crate::lir::queries::struct_contains_resource(sid, module) {
                            None
                        } else {
                            Some(sid)
                        }
                    })
                } else {
                    None
                };
                if let Some(sid) = by_value_sid {
                    let s_ty = LirType::Struct(sid);
                    let struct_ty_str = llvm_type_full(&s_ty, snames);
                    let tmp = format!("{pfx}.arg{ai}");
                    writeln!(out, "  %{tmp} = load {struct_ty_str}, ptr %v{}", a.0).unwrap();
                    call_arg_strs.push(format!("{struct_ty_str} %{tmp}"));
                } else {
                    let pty = vt
                        .map(|t| llvm_arg_type(t, snames))
                        .unwrap_or_else(|| "i64".to_string());
                    call_arg_strs.push(format!("{pty} %v{}", a.0));
                }
            }
            let joined_args = call_arg_strs.join(", ");
            if let Some(d) = dst {
                // Generic-iterator-erasure workaround: when the closure's
                // declared signature was type-erased to `GorgetClosure` (the
                // typechecker collapses `F: Callable[U(T)]` and forgets the
                // signature, so `_NN: bool` shows up for what should be U=i64
                // — see `MapIter[..., U=int64_t, F]::next` where `cb(!x)`
                // typechecks as `bool` because GorgetClosure's first cached
                // signature wins). The actual `__adapt_*` shim returns the
                // real type. If the call result is immediately consumed by a
                // `Store` into a wider-int struct field (BIR-expanded from
                // `EnumInit` like `Some(cb(!x))`), widen the call's return
                // type to the field type so we read the full register value
                // off `x0` instead of truncating to `i1`/`i8`. The C backend
                // gets lucky here — `bool` is 1 byte so values <256 survive;
                // LLVM's strict `i1` truncates to bit 0 and silently zeroes
                // anything not divisible by 2.
                let ret_is_intlike = ret_ty.is_integer() || matches!(ret_ty, LirType::Bool);
                let widened_ret_ty: Option<LirType> = if ret_is_intlike
                    && int_bits(ret_ty) < 64
                {
                    let mut found: Option<LirType> = None;
                    'outer: for b in &func.blocks {
                        for inst in &b.insts {
                            if let Inst::Store { ptr: sptr, value: sval } = inst {
                                if sval.0 == d.0 {
                                    // Trace ptr back to its FieldPtr to find dst field type.
                                    for b2 in &func.blocks {
                                        for i2 in &b2.insts {
                                            if let Inst::FieldPtr { dst: fd, struct_id, field, .. } = i2 {
                                                if fd.0 == sptr.0 {
                                                    let sdef = &module.structs[struct_id.0 as usize];
                                                    if let Some((_, ft)) = sdef.fields.get(*field as usize) {
                                                        if ft.is_integer() && int_bits(ft) > int_bits(ret_ty) {
                                                            found = Some(ft.clone());
                                                            break 'outer;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    found
                } else {
                    None
                };
                let effective_ret_ty = widened_ret_ty.as_ref().unwrap_or(ret_ty);
                let ret_type_str = llvm_arg_type(effective_ret_ty, snames);
                if needs_sret(effective_ret_ty, &module.structs) {
                    // Large aggregate: sret convention
                    writeln!(out, "  %v{} = alloca {ret_type_str}", d.0).unwrap();
                    let sret_args = format!("ptr sret({ret_type_str}) %v{}, {joined_args}", d.0);
                    writeln!(out, "  call void %{pfx}.fnp({sret_args})").unwrap();
                } else if effective_ret_ty.is_aggregate() {
                    // Small aggregate: returned in registers, store to alloca
                    writeln!(out, "  %v{}.ret = call {ret_type_str} %{pfx}.fnp({joined_args})", d.0).unwrap();
                    writeln!(out, "  %v{} = alloca {ret_type_str}", d.0).unwrap();
                    writeln!(out, "  store {ret_type_str} %v{}.ret, ptr %v{}", d.0, d.0).unwrap();
                } else {
                    writeln!(out, "  %v{} = call {ret_type_str} %{pfx}.fnp({joined_args})", d.0).unwrap();
                }
            } else {
                writeln!(out, "  call void %{pfx}.fnp({joined_args})").unwrap();
            }
        }
        Inst::DropGuardOpen { kind, value } => {
            match kind {
                DropGuardKind::Bool => {
                    let uid = *trap_counter;
                    *trap_counter += 1;
                    let body_label = format!("dg.{block_id}.{uid}.body");
                    let join_label = format!("dg.{block_id}.{uid}.join");
                    writeln!(out, "  br i1 %v{}, label %{body_label}, label %{join_label}", value.0).unwrap();
                    writeln!(out, "{body_label}:").unwrap();
                    *current_label = body_label;
                    df_stack.push(uid);
                }
                DropGuardKind::NonZero { .. } => {
                    // NonZero guards are eliminated by drop_elab (replaced with Bool).
                    // If one reaches the LLVM backend, emit a no-op comment.
                    writeln!(out, "  ; drop_guard_open.nonzero (no-op in LLVM)").unwrap();
                }
            }
        }
        Inst::DropGuardClose => {
            if let Some(uid) = df_stack.pop() {
                let join_label = format!("dg.{block_id}.{uid}.join");
                writeln!(out, "  br label %{join_label}").unwrap();
                writeln!(out, "{join_label}:").unwrap();
                *current_label = join_label.clone();
            }
        }

        // ── Bridge wiring ──────────────────────────────────────────
        Inst::SetCollectionBridge { collection, is_set: _, key_struct } => {
            // GorgetMap layout: hash_fn at field 11, eq_fn at field 12.
            // GorgetSet aliases GorgetMap, so the same offsets apply.
            // Bridge symbols (`__gorget_ktable_hash__T` /
            // `__gorget_ktable_eq__T`) are forward-declared at module
            // prologue from a scan over `SetCollectionBridge` insts.
            let key_name = &module.structs[key_struct.0 as usize].name;
            writeln!(out, "  %v{0}.hash_fp = getelementptr %GorgetMap, ptr %v{0}, i32 0, i32 11", collection.0).unwrap();
            writeln!(out, "  store ptr @__gorget_ktable_hash__{key_name}, ptr %v{0}.hash_fp", collection.0).unwrap();
            writeln!(out, "  %v{0}.eq_fp = getelementptr %GorgetMap, ptr %v{0}, i32 0, i32 12", collection.0).unwrap();
            writeln!(out, "  store ptr @__gorget_ktable_eq__{key_name}, ptr %v{0}.eq_fp", collection.0).unwrap();
        }

        // ── Nop ─────────────────────────────────────────────────────
        Inst::Nop => {
            // nothing
        }

        // ── Should never reach here — normalized to CallExtern at top ──
        Inst::CallRuntime { .. } => {
            unreachable!("CallRuntime should have been normalized to CallExtern \
                at the top of emit_inst — see the shadowing rebind above.");
        }
        Inst::CollectionCtor { .. } => {
            unreachable!("CollectionCtor reached LLVM backend; BIR lowering \
                should have expanded it.");
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
    str_globals: &mut StrGlobals,
    val_types: &[Option<LirType>],
    loc: &(String, u32, u32),
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
    // Emit trunc when the operand is wider than target, or sext/zext when narrower.
    let signed = is_signed(ty);
    let adjust_operand = |out: &mut String, vid: u32, tag: &str| -> String {
        let actual = val_types.get(vid as usize).and_then(|t| t.as_ref()).cloned();
        let actual_bits = actual.as_ref().map(int_bits).unwrap_or(64);
        if actual_bits == bits {
            return format!("%v{vid}");
        }
        let from_ty = actual.as_ref().map(llvm_type).unwrap_or("i64");
        let name = format!("ov.{block_id}.{trap_id}.{tag}");
        if actual_bits > bits {
            writeln!(out, "  %{name} = trunc {from_ty} %v{vid} to {lty}").unwrap();
        } else {
            let op = if signed { "sext" } else { "zext" };
            writeln!(out, "  %{name} = {op} {from_ty} %v{vid} to {lty}").unwrap();
        }
        format!("%{name}")
    };
    let lhs_str = adjust_operand(out, lhs.0, "lhs");
    let rhs_str = adjust_operand(out, rhs.0, "rhs");

    writeln!(out, "  %{result} = call {{ {lty}, i1 }} {intrinsic}({lty} {lhs_str}, {lty} {rhs_str})").unwrap();
    writeln!(out, "  %{val} = extractvalue {{ {lty}, i1 }} %{result}, 0").unwrap();
    writeln!(out, "  %{flag} = extractvalue {{ {lty}, i1 }} %{result}, 1").unwrap();
    writeln!(out, "  br i1 %{flag}, label %{trap_label}, label %{ok_label}").unwrap();
    writeln!(out, "{trap_label}:").unwrap();
    // Trap normalization (D11): emit the normative `trap[T_Overflow]: integer
    // overflow at file:line:col` + exit 101 via the shared `gorget_trap_at`
    // runtime entry — byte-identical with the C backend. The `T_` code is DATA
    // from `TrapKind::code()` (src/trap.rs), interned as a string global; there
    // is no LLVM-side name table. Arg order is (code, detail, file, line, col).
    let ov_code_idx = str_globals.intern(crate::trap::TrapKind::Overflow.code());
    let ov_detail_idx = str_globals.intern("integer overflow");
    let ov_file_idx = str_globals.intern(&loc.0);
    writeln!(out, "  call void @gorget_trap_at(ptr @.str.{ov_code_idx}, ptr @.str.{ov_detail_idx}, ptr @.str.{ov_file_idx}, i32 {}, i32 {})", loc.1, loc.2).unwrap();
    writeln!(out, "  unreachable").unwrap();
    writeln!(out, "{ok_label}:").unwrap();
    writeln!(out, "  %v{} = add {lty} 0, %{val}", dst.0).unwrap();

    // Update current_label — execution continues from the ok block
    *current_label = ok_label;
}

/// Emit the signed `TYPE_MIN / -1` (or `TYPE_MIN % -1`) overflow trap for a
/// plain Div/Rem (error-model.md §11 (E)). Mirrors C-Div's unconditional guard:
/// `if (lhs == TYPE_MIN && rhs == -1) panic("integer overflow")`. Branches to a
/// trap block on overflow, else falls through to a fresh ok block (updating
/// `current_label`) where the bare `sdiv`/`srem` is then emitted (statically
/// safe there). Called ONLY for signed integer types after the div0 guard.
fn emit_div_overflow_trap(
    out: &mut String,
    op: &str, // "div" or "rem" (label prefix only)
    dst: &ValueId,
    lhs: &ValueId,
    rhs: &ValueId,
    ty: &LirType,
    block_id: u32,
    trap_counter: &mut u32,
    current_label: &mut String,
    str_globals: &mut StrGlobals,
    loc: &(String, u32, u32),
) {
    let lty = llvm_type(ty);
    let bits = int_bits(ty);
    let tmin = format!("-{}", 1u128 << (bits - 1)); // INT_MIN as decimal literal

    let uid = *trap_counter;
    *trap_counter += 1;
    let lmin = format!("ovf{op}.{block_id}.{uid}.lmin");
    let rneg1 = format!("ovf{op}.{block_id}.{uid}.rneg1");
    let flag = format!("ovf{op}.{block_id}.{uid}.flag");
    let trap_label = format!("ovf{op}.{block_id}.{uid}.trap");
    let ok_label = format!("ovf{op}.{block_id}.{uid}.ok");

    writeln!(out, "  %{lmin} = icmp eq {lty} %v{}, {tmin}", lhs.0).unwrap();
    writeln!(out, "  %{rneg1} = icmp eq {lty} %v{}, -1", rhs.0).unwrap();
    writeln!(out, "  %{flag} = and i1 %{lmin}, %{rneg1}").unwrap();
    writeln!(out, "  br i1 %{flag}, label %{trap_label}, label %{ok_label}").unwrap();
    writeln!(out, "{trap_label}:").unwrap();
    // D11: signed TYPE_MIN/-1 overflow normalizes to T_Overflow, same as the
    // checked-arith overflow path — byte-identical with the C backend.
    let ov_code_idx = str_globals.intern(crate::trap::TrapKind::Overflow.code());
    let ov_detail_idx = str_globals.intern("integer overflow");
    let ov_file_idx = str_globals.intern(&loc.0);
    writeln!(out, "  call void @gorget_trap_at(ptr @.str.{ov_code_idx}, ptr @.str.{ov_detail_idx}, ptr @.str.{ov_file_idx}, i32 {}, i32 {})", loc.1, loc.2).unwrap();
    writeln!(out, "  unreachable").unwrap();
    writeln!(out, "{ok_label}:").unwrap();
    *current_label = ok_label;
    let _ = dst;
}

// ── Terminator Emission ────────────────────────────────────────────────────

/// Emit integer-width conversions for branch args whose type doesn't match
/// the target block's param type. Returns a map {(target_bid, arg_idx) -> cast name}
/// that the phi emitter consults when building phi entries.
fn emit_branch_arg_casts(
    out: &mut String,
    term: &Term,
    func: &LirFunction,
    pred_bid: u32,
    val_types: &[Option<LirType>],
    snames: &HashMap<u32, String>,
) {
    let targets: Vec<(BlockId, Vec<ValueId>)> = match term {
        Term::Jump(tgt, args) => vec![(*tgt, args.clone())],
        Term::Branch { then_block, then_args, else_block, else_args, .. } => vec![
            (*then_block, then_args.clone()),
            (*else_block, else_args.clone()),
        ],
        Term::Switch { cases, default, default_args, .. } => {
            let mut ts: Vec<_> = cases.iter().map(|(_, b, a)| (*b, a.clone())).collect();
            ts.push((*default, default_args.clone()));
            ts
        }
        _ => Vec::new(),
    };
    for (tgt, args) in targets {
        let target_block = &func.blocks[tgt.0 as usize];
        for (ai, arg) in args.iter().enumerate() {
            let Some((_, param_ty)) = target_block.params.get(ai) else { continue; };
            let actual = val_types.get(arg.0 as usize).and_then(|t| t.as_ref());

            // Aggregate-typed param flows as ptr in this backend. Spill any
            // predecessor that has the struct value into a fresh alloca and
            // pass its pointer, so phi receives a homogeneous ptr stream.
            if param_ty.is_aggregate() {
                let actual_is_ptr = actual.map_or(false, |t| matches!(t, LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef));
                if !actual_is_ptr {
                    let agg_ty = llvm_type_full(param_ty, snames);
                    writeln!(out, "  %br.cast.{pred_bid}.{}.{ai} = alloca {agg_ty}",
                        tgt.0).unwrap();
                    writeln!(out, "  store {agg_ty} %v{}, ptr %br.cast.{pred_bid}.{}.{ai}",
                        arg.0, tgt.0).unwrap();
                }
                continue;
            }

            if !param_ty.is_integer() { continue; }
            let actual_bits = actual.map(int_bits).unwrap_or(64);
            let param_bits = int_bits(param_ty);
            if actual_bits == param_bits { continue; }
            let from_ty = actual.map(llvm_type).unwrap_or("i64");
            let to_ty = llvm_type(param_ty);
            let op = if actual_bits > param_bits { "trunc" }
                else if is_signed(param_ty) { "sext" } else { "zext" };
            writeln!(out, "  %br.cast.{pred_bid}.{}.{ai} = {op} {from_ty} %v{} to {to_ty}",
                tgt.0, arg.0).unwrap();
        }
    }
}

fn emit_term(
    out: &mut String,
    term: &Term,
    func: &LirFunction,
    _module: &LirModule,
    snames: &HashMap<u32, String>,
    val_types: &[Option<LirType>],
    block_id: u32,
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
                    // General narrow↔wide integer mismatch (e.g. IConst typed
                    // i32 returned from an i64 function).
                    let mut lines: Vec<String> = Vec::new();
                    let cast = format!("ret.coerce.{}", val.0);
                    let coerced = val_ty.and_then(|av| {
                        emit_int_coerce(&mut lines, &cast, val.0, av, &func.return_type)
                    });
                    for line in &lines { writeln!(out, "{line}").unwrap(); }
                    if let Some(name) = coerced {
                        writeln!(out, "  ret {ret_ty} {name}").unwrap();
                    } else {
                        writeln!(out, "  ret {ret_ty} %v{}", val.0).unwrap();
                    }
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
        Term::Branch { cond, then_block, then_args, else_block, else_args, .. } => {
            // Same-target branch with diverging block args: lower as `select +
            // unconditional jump`. The target's phi reads from the merged
            // values via `%br.sel.<pred>.<target>.<i>` (see phi emission).
            // Without this, both arms collapse into one edge and LLVM picks
            // the THEN args silently — dropping any clamp/compute the ELSE
            // arm performed. Surfaced in the BIR-synth merge-sort's
            // `right = min(right_raw, n)` clamp (compute_right_bb branches
            // to merge_loop_bb on both arms with different right values).
            if then_block == else_block && then_args != else_args {
                let target = then_block.0;
                let pred = block_id;
                for (pi, (t_arg, e_arg)) in then_args.iter().zip(else_args.iter()).enumerate() {
                    let ty = val_types
                        .get(t_arg.0 as usize)
                        .and_then(|x| x.as_ref())
                        .map(|t| llvm_arg_type(t, snames))
                        .unwrap_or_else(|| "i64".to_string());
                    writeln!(
                        out,
                        "  %br.sel.{pred}.{target}.{pi} = select i1 %v{}, {ty} %v{}, {ty} %v{}",
                        cond.0, t_arg.0, e_arg.0
                    ).unwrap();
                }
                writeln!(out, "  br label %bb{target}").unwrap();
            } else {
                writeln!(
                    out,
                    "  br i1 %v{}, label %bb{}, label %bb{}",
                    cond.0, then_block.0, else_block.0
                ).unwrap();
            }
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
