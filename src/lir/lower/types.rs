//! Type mapping, size calculation, and enum helpers for GIR → LIR lowering.

use super::*;

/// Extract the element sizeof from a monomorphized collection constructor name.
/// E.g., `Vector__int64_t__new` → sizeof(int64_t) = 8.
/// Returns the size in bytes, or None if the name doesn't match.
pub(super) fn elem_size_from_monomorphized(name: &str, structs: &[StructDef]) -> Option<usize> {
    // Extract the type portion between the collection prefix and the method name.
    let type_str = if let Some(rest) = name.strip_prefix("Vector__") {
        rest.strip_suffix("__new")?
    } else if let Some(rest) = name.strip_prefix("Set__") {
        rest.strip_suffix("__new")?
    } else if let Some(rest) = name.strip_prefix("HashSet__") {
        rest.strip_suffix("__new")?
    } else if let Some(rest) = name.strip_prefix("Heap__") {
        rest.strip_suffix("__new")?
    } else {
        // Dict/HashMap constructors are handled by dict_elem_sizes_from_monomorphized.
        return None;
    };
    Some(c_sizeof_with_structs(type_str, structs))
}

/// Extract the inner-type sizeof from a monomorphized concurrency constructor or
/// guard set call. Works for Mutex__T__new, Shared__T__new, RWLock__T__new,
/// Channel__T__new, Guard__T__set, WriteGuard__T__set.
pub(super) fn concurrency_elem_size(name: &str, structs: &[StructDef]) -> Option<usize> {
    // Try each prefix; the type sits between the prefix and the __method suffix.
    for prefix in &["Mutex__", "Shared__", "RWLock__", "Channel__", "Guard__", "WriteGuard__"] {
        if let Some(rest) = name.strip_prefix(prefix) {
            // Find the last `__method` segment.
            if let Some(idx) = rest.rfind("__") {
                let type_str = &rest[..idx];
                return Some(c_sizeof_with_structs(type_str, structs));
            }
        }
    }
    None
}

/// Extract key and value sizeof from a monomorphized Dict constructor name.
/// E.g., `Dict__Str__int64_t__new` → (sizeof(Str), sizeof(int64_t)) = (16, 8).
pub(super) fn dict_elem_sizes_from_monomorphized(name: &str, structs: &[StructDef]) -> (usize, usize) {
    // Dict__K__V__new or HashMap__K__V__new
    let rest = name.strip_prefix("Dict__")
        .or_else(|| name.strip_prefix("HashMap__"))
        .and_then(|r| r.strip_suffix("__new"));
    if let Some(types) = rest {
        // Split on `__` to find key and value type names.
        // For simple types: "Str__int64_t" → key=Str, val=int64_t
        // For complex types: "int64_t__Str" → key=int64_t, val=Str
        // Heuristic: try splitting at each `__` boundary and pick the first valid split.
        if let Some(idx) = types.find("__") {
            let key = &types[..idx];
            let val = &types[idx + 2..];
            return (c_sizeof_with_structs(key, structs), c_sizeof_with_structs(val, structs));
        }
    }
    (8, 8) // fallback
}

/// Extract the key type name from a monomorphized Dict/HashMap name.
/// E.g., `Dict__GorgetString__int64_t__new` → Some("GorgetString").
pub(super) fn dict_key_type_from_monomorphized(name: &str) -> Option<String> {
    let rest = name.strip_prefix("Dict__")
        .or_else(|| name.strip_prefix("HashMap__"))
        .and_then(|r| r.strip_suffix("__new"))?;
    let idx = rest.find("__")?;
    Some(rest[..idx].to_string())
}

/// Extract the element type from a monomorphized Set/HashSet name.
/// E.g., `Set__GorgetString__new` → Some("GorgetString").
pub(super) fn set_elem_type_from_monomorphized(name: &str) -> Option<String> {
    let rest = name.strip_prefix("Set__")
        .or_else(|| name.strip_prefix("HashSet__"))
        .and_then(|r| r.strip_suffix("__new"))?;
    Some(rest.to_string())
}

/// Determine the elem_drop function name for a collection element type.
/// Returns None for trivially-droppable types (int, float, bool, ptr).
pub(super) fn elem_drop_fn_for_type(elem_type: &str) -> Option<String> {
    if elem_type.starts_with("GorgetArray") || elem_type.starts_with("Vector__") {
        Some("gorget_array_free".into())
    } else if elem_type.starts_with("GorgetMap") || elem_type.starts_with("Dict__") || elem_type.starts_with("HashMap__") {
        Some("gorget_map_free".into())
    } else if elem_type.starts_with("GorgetSet") || elem_type.starts_with("Set__") || elem_type.starts_with("HashSet__") {
        Some("gorget_set_free".into())
    } else if elem_type == "GorgetString" {
        Some("gorget_string_free".into())
    } else {
        None
    }
}

/// Determine the elem_clone function name for a collection element type.
pub(super) fn elem_clone_fn_for_type(elem_type: &str) -> Option<String> {
    if elem_type.starts_with("GorgetArray") || elem_type.starts_with("Vector__") {
        Some("gorget_array_clone_inplace".into())
    } else if elem_type.starts_with("GorgetMap") || elem_type.starts_with("Dict__") || elem_type.starts_with("HashMap__") {
        Some("gorget_map_clone_inplace".into())
    } else if elem_type.starts_with("GorgetSet") || elem_type.starts_with("Set__") || elem_type.starts_with("HashSet__") {
        Some("gorget_set_clone_inplace".into())
    } else if elem_type == "GorgetString" {
        Some("gorget_string_clone_inplace".into())
    } else {
        None
    }
}

/// Return the sizeof of an LIR type in bytes (best-effort for 64-bit targets).
pub(super) fn lir_type_sizeof(ty: &LirType) -> usize {
    match ty {
        LirType::I8 | LirType::U8 | LirType::Bool => 1,
        LirType::I16 | LirType::U16 => 2,
        LirType::I32 | LirType::U32 | LirType::F32 => 4,
        LirType::I64 | LirType::U64 | LirType::F64 => 8,
        LirType::Ptr | LirType::PtrTo(_) => 8,
        LirType::Struct(_) => 8, // conservative; struct sizeof varies
        LirType::Void => 0,
    }
}

/// Map a GIR C type name to its sizeof in bytes.
/// `structs` is used to compute sizes of user-defined struct types.
pub(super) fn c_sizeof_with_structs(type_name: &str, structs: &[StructDef]) -> usize {
    match type_name {
        "int64_t" | "uint64_t" | "double" => 8,
        "int32_t" | "uint32_t" | "float" => 4,
        "int16_t" | "uint16_t" => 2,
        "int8_t" | "uint8_t" | "bool" => 1,
        // GorgetString is a 32-byte fat struct { data, cap, len, alloc }
        "GorgetString" | "String" | "Str" => 32,
        _ => {
            // Runtime collection structs.
            if type_name.starts_with("Vector__") || type_name == "GorgetArray" {
                return 64; // {data, len, cap, elem_size, alloc, elem_drop, elem_clone, elem_materialize}
            }
            if type_name.starts_with("Dict__") || type_name.starts_with("HashMap__") || type_name == "GorgetMap" {
                return 152;
            }
            // GorgetSet aliases GorgetMap (same struct)
            if type_name.starts_with("Set__") || type_name.starts_with("HashSet__") || type_name == "GorgetSet" {
                return 152;
            }
            // GorgetClosure / Callable = {fn_ptr, env} = 16 bytes
            if type_name == "GorgetClosure" || type_name.starts_with("Callable__") || type_name.starts_with("Callable_") {
                return 16;
            }
            // Task__T = { void* __task; void (*__drop)(void*); } = 16 bytes
            if type_name.starts_with("Task__") {
                return 16;
            }
            // Tuple__T1__T2__... — sum of field sizes with 8-byte alignment per field
            if let Some(rest) = type_name.strip_prefix("Tuple__") {
                return c_sizeof_tuple_fields(rest, structs);
            }
            // Option__T — tag(4) + padding(4) + payload
            if let Some(inner) = type_name.strip_prefix("Option__") {
                let payload = c_sizeof_with_structs(inner, structs);
                // struct { int32_t tag; <pad to 8>; T payload; }
                return 8 + std::cmp::max(payload, 8);
            }
            // User-defined struct — prefer cached size, fall back to computation.
            if let Some(sd) = structs.iter().find(|s| s.name == type_name) {
                return sd.computed_c_size.unwrap_or_else(|| c_sizeof_struct_def(sd, structs));
            }
            // Pointer/opaque types default to 8
            8
        }
    }
}

/// Compute the size of a struct from its LIR StructDef.
/// For union-layout enums (`is_union_layout == true`), uses union layout:
///   sizeof = align8(tag) + max(variant_size), aligned to 8
/// For regular structs, sums fields sequentially with alignment.
pub fn c_sizeof_struct_def(sd: &StructDef, structs: &[StructDef]) -> usize {
    if sd.is_union_layout && sd.fields.len() > 1 {
        // Union layout: tag (field 0) + union of variant groups.
        // tag is always I32 = 4 bytes, padded to 8 for union alignment.
        let tag_size = 8usize;

        // Group remaining fields by variant prefix (split on last '_').
        let mut max_variant_size = 0usize;
        let mut current_prefix = String::new();
        let mut current_variant_size = 0usize;

        for (name, ty) in &sd.fields[1..] {
            let prefix = name.rsplitn(2, '_').nth(1).unwrap_or(name).to_string();
            if prefix != current_prefix {
                // Finish previous variant group.
                if !current_prefix.is_empty() {
                    let aligned = (current_variant_size + 7) / 8 * 8;
                    max_variant_size = std::cmp::max(max_variant_size, aligned);
                }
                current_prefix = prefix;
                current_variant_size = 0;
            }
            let field_sz = c_sizeof_lir_type(ty, structs);
            let align = std::cmp::min(field_sz, 8);
            if align > 0 {
                current_variant_size = (current_variant_size + align - 1) / align * align;
            }
            current_variant_size += field_sz;
        }
        // Finish last variant group.
        if !current_prefix.is_empty() {
            let aligned = (current_variant_size + 7) / 8 * 8;
            max_variant_size = std::cmp::max(max_variant_size, aligned);
        }

        let total = tag_size + max_variant_size;
        // Align total to 8 bytes.
        (total + 7) / 8 * 8
    } else {
        // Regular struct: sum fields sequentially.
        let mut total = 0usize;
        let mut max_align = 1usize;
        for (_name, ty) in &sd.fields {
            let field_sz = c_sizeof_lir_type(ty, structs);
            let align = std::cmp::min(field_sz, 8).max(1);
            max_align = std::cmp::max(max_align, align);
            total = (total + align - 1) / align * align;
            total += field_sz;
        }
        // Align total to the struct's max field alignment.
        if max_align > 0 {
            total = (total + max_align - 1) / max_align * max_align;
        }
        total
    }
}

/// Known sizes for opaque runtime C structs that have no LIR fields
/// (declared as `struct X: pass` in Gorget, backed by a C typedef).
///
/// Shared by size-of calculations, LLVM struct emission, and aggregate-return
/// ABI decisions. Keeping the table in one place means both backends reach
/// the same layout; adding a new runtime struct requires one edit here.
pub fn opaque_runtime_size(name: &str) -> Option<usize> {
    let sz = match name {
        // Core collections (layouts match c_runtime.rs typedefs).
        "GorgetString" | "Str" => 32,
        "GorgetArray" => 64,
        "GorgetMap" | "GorgetSet" => 152,
        "GorgetClosure" => 16,
        "GorgetRange" => 24,
        // Concurrency opaque handles — pointer-sized.
        "AtomicInt" | "AtomicBool" | "Mutex" | "Shared" | "RWLock" | "Barrier"
        | "CondVar" | "WaitGroup" | "Semaphore" | "OnceFlag" | "TaskGroup"
        | "Weak" | "Executor" | "BlockingPool" | "FileWatcher" | "Reactor"
        | "GuestModule" | "Thread" => 8,
        // Channel[T] is a pointer to GorgetChannel.
        "Channel" => 8,
        // Task__T and per-type Thread__T: {task_ptr, drop_fn} = 16.
        _ if name.starts_with("Task__") => 16,
        _ if name.starts_with("Thread__") => 8,
        // Per-type concurrency typedefs alias to opaque pointers.
        _ if name.starts_with("Mutex__") || name.starts_with("Shared__")
            || name.starts_with("RWLock__") || name.starts_with("Channel__")
            || name.starts_with("Weak__") => 8,
        // Sockets / files / process — runtime uses fixed layouts.
        "Socket" | "ServerSocket" | "UdpSocket" => 8,
        "TlsSocket" | "TlsServerSocket" => 24,  // fd + SSL_CTX* + SSL*/ctx
        "UdpAddr" => 40,   // {Str host, int64_t port} = 32 + 8
        "UdpPacket" => 104,// {GorgetArray data, UdpAddr sender} = 64 + 40
        "File" | "GorgetFile" => 16,
        "Process" => 48,   // {pid, stdin/out/err fds, status, owned}
        "ExecResult" => 48, // {int status, Vector[uint8] stdout, stderr}
        // Regex — see c_runtime.rs GorgetRegexMatch.
        "Regex" => 16,       // {pcre2_code*, const char* pattern}
        "Match" | "RegexMatch" => 56, // {text, start, end, groups, count, names, names_len}
        // Allocators — each has its own fixed layout.
        "Arena" => 64,       // fields vary; treat as 64-byte allocator handle
        "ArenaCheckpoint" => 16,
        "PoolAllocator" | "TlsfAllocator" | "FixedBufferAllocator"
        | "FallbackAllocator" | "TrackingAllocator" => 8,
        // Crypto.
        "CipherContext" => 8,
        "BigNum" | "RSAKey" => 8,
        "Ed25519KeyPair" | "X25519KeyPair" => 64,
        // SDL.
        "SDLWindow" | "SDLRenderer" | "SDLTexture" | "SDLFont" | "SDLEvent" => 8,
        // Audio.
        "AudioChunk" => 16,
        // Box[T] types are 1-ptr.
        _ if name.starts_with("Box__") => 8,
        // Guard types: { owner: ptr, data: ptr } = 16.
        _ if name.starts_with("Guard__") || name.starts_with("ReadGuard__")
            || name.starts_with("WriteGuard__") => 16,
        _ => return None,
    };
    Some(sz)
}

/// Compute sizeof for an LirType.
pub(super) fn c_sizeof_lir_type(ty: &LirType, structs: &[StructDef]) -> usize {
    match ty {
        LirType::I8 | LirType::U8 | LirType::Bool => 1,
        LirType::I16 | LirType::U16 => 2,
        LirType::I32 | LirType::U32 => 4,
        LirType::I64 | LirType::U64 | LirType::F64 | LirType::Ptr | LirType::PtrTo(_) => 8,
        LirType::F32 => 4,
        LirType::Struct(sid) => {
            if let Some(sd) = structs.get(sid.0 as usize) {
                if let Some(sz) = opaque_runtime_size(&sd.name) {
                    return sz;
                }
                c_sizeof_struct_def(sd, structs)
            } else {
                8
            }
        }
        LirType::Void => 0,
    }
}

/// Compute the C alignment of an LirType (in bytes).
/// Scalars align to their natural size (capped at 8). Aggregates align to
/// the max of their field alignments. Used by the LLVM backend to insert
/// inter-field padding matching the C ABI.
pub fn c_alignof_lir_type(ty: &LirType, structs: &[StructDef]) -> usize {
    match ty {
        LirType::I8 | LirType::U8 | LirType::Bool => 1,
        LirType::I16 | LirType::U16 => 2,
        LirType::I32 | LirType::U32 | LirType::F32 => 4,
        LirType::I64 | LirType::U64 | LirType::F64 | LirType::Ptr | LirType::PtrTo(_) => 8,
        LirType::Struct(sid) => {
            if let Some(sd) = structs.get(sid.0 as usize) {
                if let Some(a) = sd.computed_c_align {
                    return a;
                }
                // Union-layout enums always align to 8 (payload contains i64/ptr)
                if sd.is_union_layout { return 8; }
                // Recursive: max of field alignments
                sd.fields.iter()
                    .map(|(_, fty)| c_alignof_lir_type(fty, structs))
                    .max()
                    .unwrap_or(1)
                    .min(8)
            } else {
                8
            }
        }
        LirType::Void => 1,
    }
}

/// Compute the size of a tuple from its mangled field types.
/// `Tuple__int64_t__Str` → fields are [int64_t, Str] → 8 + 32 = 40.
/// Fields are split on `__` but multi-word types like `int64_t` contain `_`
/// (not `__`), so we split on `__` and rejoin single-underscore segments.
pub(super) fn c_sizeof_tuple_fields(fields_str: &str, structs: &[StructDef]) -> usize {
    let mut total = 0usize;
    // Split on __ delimiter.  Type names use single _ (int64_t, uint8_t).
    for part in fields_str.split("__") {
        if part.is_empty() { continue; }
        let field_sz = c_sizeof_with_structs(part, structs);
        // Align each field to its natural alignment (max 8).
        let align = std::cmp::min(field_sz, 8);
        if align > 0 {
            total = (total + align - 1) / align * align;
        }
        total += field_sz;
    }
    // Align total to 8 bytes (struct padding).
    let align = 8;
    total = (total + align - 1) / align * align;
    total
}

pub(super) fn lower_global_init(init: &ir::GlobalInit, func_index: &std::collections::HashMap<String, FuncId>, target_ty: &LirType, struct_reg: &StructRegistry) -> LirGlobalInit {
    match init {
        ir::GlobalInit::Zeroed => LirGlobalInit::Zeroed,
        ir::GlobalInit::Bytes(b) => LirGlobalInit::Bytes(b.clone()),
        ir::GlobalInit::FnRef(name) => {
            if let Some(fid) = func_index.get(name) {
                LirGlobalInit::FuncAddr(*fid)
            } else {
                LirGlobalInit::Zeroed
            }
        }
        ir::GlobalInit::Struct { type_name, fields } => {
            // Resolve struct_id from the GIR type_name. Falls back to the
            // target type's struct if the registry doesn't have it yet,
            // then StructId(0) if neither is available (trivial structs).
            let struct_id = struct_reg.lookup(type_name)
                .or_else(|| match target_ty {
                    LirType::Struct(sid) | LirType::PtrTo(sid) => Some(*sid),
                    _ => None,
                })
                .unwrap_or(StructId(0));
            LirGlobalInit::Struct {
                struct_id,
                fields: fields.iter().map(|(_, f)| lower_global_init(f, func_index, &LirType::I64, struct_reg)).collect(),
            }
        }
        ir::GlobalInit::RuntimeCall(expr) => {
            // Try to parse the expression as a numeric constant.
            // For float-typed globals, always parse as f64 to avoid
            // integer-like values (e.g. "800" from 800.0) being stored
            // as i64 bits instead of f64 bits.
            if target_ty.is_float() {
                if let Ok(v) = expr.parse::<f64>() {
                    return LirGlobalInit::Bytes(v.to_le_bytes().to_vec());
                }
            }
            if let Ok(v) = expr.parse::<i64>() {
                LirGlobalInit::Bytes(v.to_le_bytes().to_vec())
            } else if let Ok(v) = expr.parse::<f64>() {
                LirGlobalInit::Bytes(v.to_le_bytes().to_vec())
            } else {
                // Complex runtime call — remap function names to runtime equivalents.
                let mut remapped = expr.clone();
                if let Some(paren_pos) = remapped.find('(') {
                    let func_name = &remapped[..paren_pos];
                    if let Some(mapped) = map_monomorphized_to_runtime(func_name) {
                        // Concurrency constructors need sizeof + address-of injected:
                        // Mutex__T__new(val) → gorget_mutex_new(sizeof(T), &(T){val})
                        if matches!(mapped.as_str(), "gorget_mutex_new" | "gorget_shared_new" | "gorget_rwlock_new") {
                            let args_str = &remapped[paren_pos + 1..remapped.len() - 1]; // strip parens
                            let _elem_size = concurrency_elem_size(func_name, &[]).unwrap_or(8);
                            // Use a compound literal with the element type for proper alignment
                            let elem_type = func_name
                                .strip_prefix("Mutex__").or_else(|| func_name.strip_prefix("Shared__"))
                                .or_else(|| func_name.strip_prefix("RWLock__"))
                                .and_then(|r| r.rsplit_once("__").map(|(t, _)| t))
                                .unwrap_or("int64_t");
                            remapped = format!("{mapped}(sizeof({elem_type}), &({elem_type}){{{args_str}}})");
                        } else {
                            remapped = format!("{mapped}{}", &remapped[paren_pos..]);
                        }
                    }
                }
                LirGlobalInit::RuntimeCall(remapped)
            }
        }
    }
}

/// Top-level entry point: lower a GIR module to LIR.
pub fn lower_module(gir: &ir::Module) -> LirModule {
    let ctx = LoweringContext::new(gir);
    ctx.lower()
}

/// Convert a GIR TypeId to its C type name (for spawn metadata).
/// Note: returns "GorgetString" for GorgetString so Task type names match the
/// mangling used by Task[str] user annotations (Task__GorgetString, not Task__GorgetString).
pub(super) fn gir_type_to_c(type_id: gir_types::TypeId, registry: &TypeRegistry) -> String {
    use gir_types::*;
    if type_id == BOOL_TYPE { return "bool".into(); }
    if type_id == I8_TYPE { return "int8_t".into(); }
    if type_id == I16_TYPE { return "int16_t".into(); }
    if type_id == I32_TYPE { return "int32_t".into(); }
    if type_id == I64_TYPE { return "int64_t".into(); }
    if type_id == U8_TYPE { return "uint8_t".into(); }
    if type_id == U16_TYPE { return "uint16_t".into(); }
    if type_id == U32_TYPE { return "uint32_t".into(); }
    if type_id == U64_TYPE { return "uint64_t".into(); }
    if type_id == F32_TYPE { return "float".into(); }
    if type_id == F64_TYPE { return "double".into(); }
    if type_id == UNIT_TYPE { return "void".into(); }
    if let Some(gir_type) = registry.get(type_id) {
        match gir_type {
            GirType::Ptr(inner) if *inner == U8_TYPE => "const char*".into(),
            GirType::Ptr(inner) => format!("const {}*", gir_type_to_c(*inner, registry)),
            GirType::MutPtr(inner) => format!("{}*", gir_type_to_c(*inner, registry)),
            GirType::Named(name) => {
                // Map collection instantiations to runtime struct names.
                if let Some(rt) = collection_runtime_type(name) {
                    rt.into()
                } else if is_opaque_pointer_type(name) {
                    // Opaque types are lowered to Ptr (void*) in LIR.
                    "void*".into()
                } else if let Some(rt) = opaque_runtime_type_name(name) {
                    rt.into()
                } else {
                    name.clone()
                }
            }
            GirType::FnPtr { .. } => "void*".into(),
            _ => format!("int64_t"), // fallback
        }
    } else {
        "int64_t".into()
    }
}

/// Convert a GIR TypeId to C type for spawn context fields.
/// Callable params (FnPtr) become void*; void becomes void*.
pub(super) fn spawn_param_c_type(type_id: gir_types::TypeId, registry: &TypeRegistry) -> String {
    if matches!(registry.get(type_id), Some(GirType::FnPtr { .. })) {
        return "void*".into();
    }
    let c = gir_type_to_c(type_id, registry);
    if c == "void" { "void*".into() } else { c }
}
