//! LIR → LLVM IR backend.
//!
//! Generates LLVM IR textual format (.ll) from LIR. The mapping is nearly 1:1
//! since LIR is already SSA with block parameters (phi-equivalent).

use crate::lir::*;
use std::collections::HashMap;
use std::fmt::Write;

/// LLVM IR backend.
pub struct LlvmBackend;

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
        LirType::Struct(sid) => format!("%{}", snames[&sid.0]),
        LirType::Void => "void".to_string(),
        other => llvm_type(other).to_string(),
    }
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
fn infer_inst_type(inst: &Inst, module: &LirModule, _val_types: &[Option<LirType>]) -> Option<LirType> {
    match inst {
        Inst::SlotLoad { ty, .. } | Inst::ParamRef { ty, .. } => Some(ty.clone()),
        Inst::SlotAddr { .. } => Some(LirType::Ptr),
        Inst::IConst { ty, .. } | Inst::FConst { ty, .. } => Some(ty.clone()),
        Inst::BoolConst { .. } => Some(LirType::Bool),
        Inst::NullPtr { .. } | Inst::FuncAddr { .. } | Inst::GlobalAddr { .. } => Some(LirType::Ptr),
        Inst::StrLit { .. } => Some(LirType::Ptr), // materialized as alloca returning ptr

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
        Inst::PtrCast { .. } => Some(LirType::Ptr),

        Inst::Load { ty, .. } => Some(ty.clone()),
        Inst::FieldPtr { .. } | Inst::ElemPtr { .. } => Some(LirType::Ptr),

        Inst::Call { func, .. } => {
            Some(module.functions[func.0 as usize].return_type.clone())
        }
        Inst::CallExtern { name, .. } => {
            module.externs.iter()
                .find(|e| e.name == *name)
                .map(|e| e.return_type.clone())
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
/// Field 0 is the tag (i32). Fields 1+ are variant payloads.
fn enum_payload_size(def: &StructDef, structs: &[StructDef], snames: &HashMap<u32, String>) -> usize {
    let mut max_size = 0usize;
    for (_, fty) in def.fields.iter().skip(1) {
        let sz = sizeof_lir_type(fty, structs, snames);
        if sz > max_size {
            max_size = sz;
        }
    }
    max_size
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
            }
        }
    }
    emit_string_globals(&mut out, &str_globals);

    // Global variables
    emit_globals(&mut out, module, &snames);

    // Extern function declarations
    emit_extern_declarations(&mut out, module, &snames);

    // Intrinsic declarations
    emit_intrinsic_declarations(&mut out, module);

    // No forward declarations needed — LLVM handles out-of-order function references.

    // Function definitions
    for func in &module.functions {
        emit_function(&mut out, func, module, &snames, &str_globals);
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
        if def.is_enum {
            // Enum: { i32 tag, [payload_bytes x i8] }
            let payload = enum_payload_size(def, &module.structs, snames);
            if payload > 0 {
                writeln!(out, "%{name} = type {{ i32, [{payload} x i8] }}").unwrap();
            } else {
                writeln!(out, "%{name} = type {{ i32 }}").unwrap();
            }
        } else if def.fields.is_empty() {
            // Empty struct — use single byte padding
            writeln!(out, "%{name} = type {{ i8 }}").unwrap();
        } else {
            let fields: Vec<String> = def.fields.iter()
                .map(|(_, fty)| llvm_type_full(fty, snames))
                .collect();
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
                let sty = format!("%{}", snames[&struct_id.0]);
                let sdef = &module.structs[struct_id.0 as usize];
                let field_vals: Vec<String> = fields.iter().enumerate().map(|(fi, init)| {
                    let fty = if fi < sdef.fields.len() {
                        llvm_type_full(&sdef.fields[fi].1, snames)
                    } else {
                        "i64".to_string()
                    };
                    match init {
                        LirGlobalInit::Zeroed => format!("{fty} zeroinitializer"),
                        LirGlobalInit::FuncAddr(fid) => {
                            let fname = &module.functions[fid.0 as usize].name;
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
const LIBC_BUILTINS: &[&str] = &["printf", "fprintf", "abort", "memset", "memcpy", "exit", "malloc", "free", "realloc", "calloc"];

fn emit_extern_declarations(out: &mut String, module: &LirModule, snames: &HashMap<u32, String>) {
    // Also declare libc functions we use directly
    writeln!(out, "; -- libc --").unwrap();
    writeln!(out, "declare i32 @printf(ptr, ...)").unwrap();
    writeln!(out, "declare i32 @fprintf(ptr, ptr, ...)").unwrap();
    writeln!(out, "declare void @abort() noreturn").unwrap();
    writeln!(out, "declare ptr @memset(ptr, i32, i64)").unwrap();
    writeln!(out, "declare ptr @memcpy(ptr, ptr, i64)").unwrap();
    writeln!(out, "declare void @gorget_init_args(i32, ptr)").unwrap();
    writeln!(out).unwrap();

    // Collect names of functions defined in this module (skip forward declarations)
    let defined_fns: std::collections::HashSet<&str> = module.functions.iter()
        .map(|f| f.name.as_str())
        .collect();

    // Module externs
    writeln!(out, "; -- runtime externs --").unwrap();
    let mut seen = std::collections::HashSet::new();
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
        let ret = llvm_type_full(&ext.return_type, snames);
        let params: Vec<String> = ext.params.iter()
            .map(|p| llvm_type_full(p, snames))
            .collect();
        let variadic = if ext.is_variadic {
            if params.is_empty() { "...".to_string() } else { format!(", ...") }
        } else {
            String::new()
        };
        writeln!(out, "declare {ret} @{}({}{})", ext.name, params.join(", "), variadic).unwrap();
    }

    // Auto-declare any CallExtern targets not yet declared
    // (some runtime functions are called without explicit extern declarations)
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::CallExtern { name, args, dst, .. } = inst {
                    if seen.contains(name.as_str()) || LIBC_BUILTINS.contains(&name.as_str())
                        || defined_fns.contains(name.as_str()) {
                        continue;
                    }
                    seen.insert(name.clone());
                    // Infer signature: all args as ptr, return void or i64
                    let params: Vec<&str> = args.iter().map(|_| "ptr").collect();
                    let ret = if dst.is_some() { "i64" } else { "void" };
                    writeln!(out, "declare {ret} @{name}({})", params.join(", ")).unwrap();
                }
            }
        }
    }
    writeln!(out).unwrap();
}

// ── Intrinsic Declarations ────────────────────────────────────────────────

fn emit_intrinsic_declarations(out: &mut String, module: &LirModule) {
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
    writeln!(out).unwrap();
}

// ── Function Emission ──────────────────────────────────────────────────────

fn emit_function(
    out: &mut String,
    func: &LirFunction,
    module: &LirModule,
    snames: &HashMap<u32, String>,
    str_globals: &StrGlobals,
) {
    let ret = llvm_type_full(&func.return_type, snames);
    let params: Vec<String> = func.params.iter().enumerate()
        .map(|(i, p)| format!("{} %p{i}", llvm_type_full(p, snames)))
        .collect();

    let is_main = func.name == "main";
    if is_main {
        // main needs C ABI signature: (i32, ptr) for argc/argv
        writeln!(out, "define i32 @main(i32 %argc, ptr %argv) {{").unwrap();
    } else {
        writeln!(out, "define {} @{}({}) {{", ret, &func.name, params.join(", ")).unwrap();
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
                    let ty = infer_inst_type(inst, module, &val_types);
                    // Aggregates are always represented as pointers in our LLVM codegen:
                    // - Call/CallExtern results stored via temp alloca
                    // - Load on aggregate uses pointer alias (no actual load)
                    // - ParamRef for aggregate uses alloca
                    let ty = match &ty {
                        Some(t) if t.is_aggregate() => Some(LirType::Ptr),
                        _ => ty,
                    };
                    val_types[dst.0 as usize] = ty;
                }
            }
        }
    }

    // Entry block: emit allocas for non-promoted slots
    writeln!(out, "entry.prelude:").unwrap();

    // For main(), call gorget_init_args to set up argc/argv
    if is_main {
        writeln!(out, "  call void @gorget_init_args(i32 %argc, ptr %argv)").unwrap();
    }
    for (i, slot) in func.slots.iter().enumerate() {
        if slot.ty == LirType::Void {
            // Void slots don't need allocation — use i8 as placeholder
            writeln!(out, "  %s{i} = alloca i8 ; void slot").unwrap();
        } else {
            let ty = llvm_type_full(&slot.ty, snames);
            let name = slot.name.as_deref().unwrap_or("slot");
            writeln!(out, "  %s{i} = alloca {ty} ; {name}").unwrap();
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

    // Pre-compute exit labels for each block: if a block contains overflow/bounds/div checks,
    // the actual exit point is the last "ok" label, not the original bb{N}.
    // Must exactly mirror the trap_counter increments in emit_inst.
    let block_exit_labels: HashMap<BlockId, String> = {
        let mut labels = HashMap::new();
        for block in &func.blocks {
            let bid = block.id.0;
            let mut label = format!("bb{bid}");
            let mut counter = 0u32;
            for inst in &block.insts {
                match inst {
                    Inst::Add { overflow: Overflow::Trap, ty, .. }
                    | Inst::Sub { overflow: Overflow::Trap, ty, .. }
                    | Inst::Mul { overflow: Overflow::Trap, ty, .. } if ty.is_integer() => {
                        label = format!("ov.{bid}.{counter}.ok");
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
                    // CallExtern inline expansions that increment trap_counter
                    Inst::CallExtern { name, args, .. } => {
                        // Printf string extraction
                        if name == "printf" && !args.is_empty() {
                            counter += 1;
                        }
                        // Option/Result tag checks
                        let is_tag = name == "__option_is_some" || name == "__option_is_none"
                            || name.ends_with("__is_some") || name.ends_with("__is_none")
                            || name.ends_with("__is_ok") || name.ends_with("__is_err");
                        if is_tag && !args.is_empty() {
                            counter += 1;
                        }
                        // Option/Result unwrap
                        let is_unwrap = name == "__option_unwrap" || name == "__result_unwrap"
                            || name.ends_with("__unwrap") || name.ends_with("__expect");
                        if is_unwrap && !args.is_empty() {
                            counter += 1;
                        }
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
        for inst in &block.insts {
            emit_inst(out, inst, func, module, snames, str_globals, &val_types, bid, &mut trap_counter, &mut current_label);
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
    str_globals: &StrGlobals,
    val_types: &[Option<LirType>],
    block_id: u32,
    trap_counter: &mut u32,
    current_label: &mut String,
) {
    match inst {
        // ── Slot Access ─────────────────────────────────────────────
        Inst::SlotStore { slot, value, .. } => {
            let slot_ty = &func.slots[slot.0 as usize].ty;
            if *slot_ty == LirType::Void {
                writeln!(out, "  ; void slot store skipped").unwrap();
            } else if slot_ty.is_aggregate() {
                // Aggregate slot — value may be a pointer (SlotAddr/FieldPtr) or
                // aggregate by value (Call return). Check val_types.
                let val_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
                let is_ptr_val = val_ty.map_or(false, |t| t.is_ptr());
                if is_ptr_val {
                    // Value is a pointer — memcpy from it
                    let sz = sizeof_lir_type(slot_ty, &module.structs, snames);
                    writeln!(out, "  call ptr @memcpy(ptr %s{}, ptr %v{}, i64 {sz})", slot.0, value.0).unwrap();
                } else {
                    // Value is an aggregate by value — store it directly
                    let sty = llvm_type_full(slot_ty, snames);
                    writeln!(out, "  store {sty} %v{}, ptr %s{}", value.0, slot.0).unwrap();
                }
            } else {
                let sty = llvm_type_full(slot_ty, snames);
                writeln!(out, "  store {sty} %v{}, ptr %s{}", value.0, slot.0).unwrap();
            }
        }
        Inst::SlotLoad { dst, slot, ty } => {
            if *ty == LirType::Void {
                writeln!(out, "  ; void slot load skipped (v{})", dst.0).unwrap();
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
            let fname = &module.functions[fid.0 as usize].name;
            writeln!(out, "  %v{} = bitcast ptr @{fname} to ptr", dst.0).unwrap();
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
            // Store length
            writeln!(out, "  %{fl} = getelementptr %GorgetString, ptr %strlit.{}, i32 0, i32 1", dst.0).unwrap();
            writeln!(out, "  store i64 {byte_len}, ptr %{fl}").unwrap();
            // Store capacity = 0 (view/literal)
            writeln!(out, "  %{fc} = getelementptr %GorgetString, ptr %strlit.{}, i32 0, i32 2", dst.0).unwrap();
            writeln!(out, "  store i64 0, ptr %{fc}").unwrap();
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
                _ => {
                    writeln!(out, "  %v{} = add {lty} 0, %p{index}", dst.0).unwrap();
                }
            }
        }

        // ── Arithmetic ──────────────────────────────────────────────
        Inst::Add { dst, ty, lhs, rhs, overflow } => {
            let lty = llvm_type(ty);
            if *overflow == Overflow::Trap && ty.is_integer() {
                emit_overflow_check(out, "add", dst, lhs, rhs, ty, snames, block_id, trap_counter, current_label);
            } else if ty.is_float() {
                writeln!(out, "  %v{} = fadd {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            } else {
                writeln!(out, "  %v{} = add {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            }
        }
        Inst::Sub { dst, ty, lhs, rhs, overflow } => {
            let lty = llvm_type(ty);
            if *overflow == Overflow::Trap && ty.is_integer() {
                emit_overflow_check(out, "sub", dst, lhs, rhs, ty, snames, block_id, trap_counter, current_label);
            } else if ty.is_float() {
                writeln!(out, "  %v{} = fsub {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            } else {
                writeln!(out, "  %v{} = sub {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            }
        }
        Inst::Mul { dst, ty, lhs, rhs, overflow } => {
            let lty = llvm_type(ty);
            if *overflow == Overflow::Trap && ty.is_integer() {
                emit_overflow_check(out, "mul", dst, lhs, rhs, ty, snames, block_id, trap_counter, current_label);
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
            } else if is_signed(ty) {
                writeln!(out, "  %v{} = sdiv {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            } else {
                writeln!(out, "  %v{} = udiv {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            }
        }
        Inst::Rem { dst, ty, lhs, rhs } => {
            let lty = llvm_type(ty);
            if ty.is_float() {
                writeln!(out, "  %v{} = frem {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            } else if is_signed(ty) {
                writeln!(out, "  %v{} = srem {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            } else {
                writeln!(out, "  %v{} = urem {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
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
            // Determine operand type from val_types
            let lhs_ty = val_types.get(lhs.0 as usize).and_then(|t| t.as_ref());
            let is_float_cmp = lhs_ty.map_or(false, |t| t.is_float());
            let is_ptr_cmp = lhs_ty.map_or(false, |t| t.is_ptr());
            let is_signed_cmp = lhs_ty.map_or(true, |t| is_signed(t));
            let lty = lhs_ty.map_or("i64", |t| {
                if t.is_ptr() { "ptr" } else { llvm_type(t) }
            });

            if is_float_cmp {
                let fcmp_op = match op {
                    CmpOp::Eq => "oeq",
                    CmpOp::Ne => "une",
                    CmpOp::Lt => "olt",
                    CmpOp::Le => "ole",
                    CmpOp::Gt => "ogt",
                    CmpOp::Ge => "oge",
                };
                writeln!(out, "  %v{} = fcmp {fcmp_op} {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            } else if is_ptr_cmp {
                let icmp_op = match op {
                    CmpOp::Eq => "eq",
                    CmpOp::Ne => "ne",
                    _ => "eq", // ptr comparisons other than eq/ne are rare
                };
                writeln!(out, "  %v{} = icmp {icmp_op} ptr %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            } else {
                let icmp_op = match op {
                    CmpOp::Eq => "eq",
                    CmpOp::Ne => "ne",
                    CmpOp::Lt => if is_signed_cmp { "slt" } else { "ult" },
                    CmpOp::Le => if is_signed_cmp { "sle" } else { "ule" },
                    CmpOp::Gt => if is_signed_cmp { "sgt" } else { "ugt" },
                    CmpOp::Ge => if is_signed_cmp { "sge" } else { "uge" },
                };
                writeln!(out, "  %v{} = icmp {icmp_op} {lty} %v{}, %v{}", dst.0, lhs.0, rhs.0).unwrap();
            }
        }
        Inst::Not { dst, operand } => {
            writeln!(out, "  %v{} = xor i1 %v{}, 1", dst.0, operand.0).unwrap();
        }

        // ── Type Conversions ────────────────────────────────────────
        Inst::IntCast { dst, value, to } => {
            let src_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
            let to_ty = llvm_type(to);
            let src_bits = src_ty.map_or(64, int_bits);
            let to_bits = int_bits(to);
            let src_ty_str = src_ty.map_or("i64", |t| llvm_type(t));

            if src_bits == to_bits {
                // Same size — no-op
                writeln!(out, "  %v{} = add {to_ty} 0, %v{}", dst.0, value.0).unwrap();
            } else if to_bits > src_bits {
                // Widening
                let ext = if src_ty.map_or(true, is_signed) { "sext" } else { "zext" };
                writeln!(out, "  %v{} = {ext} {src_ty_str} %v{} to {to_ty}", dst.0, value.0).unwrap();
            } else {
                // Narrowing
                writeln!(out, "  %v{} = trunc {src_ty_str} %v{} to {to_ty}", dst.0, value.0).unwrap();
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
            let val_ty = val_types.get(value.0 as usize)
                .and_then(|t| t.as_ref())
                .map(|t| llvm_type_full(t, snames))
                .unwrap_or_else(|| "i64".to_string());
            writeln!(out, "  store {val_ty} %v{}, ptr %v{}", value.0, ptr.0).unwrap();
        }
        Inst::FieldPtr { dst, base, struct_id, field } => {
            let sname = &snames[&struct_id.0];
            let sdef = &module.structs[struct_id.0 as usize];
            if sdef.is_enum && *field > 0 {
                // For enum, field 0 is tag (i32), field 1+ goes into the union payload byte array.
                // Access via GEP into the [N x i8] payload at byte offset.
                // First get pointer to the payload array (field index 1)
                let payload_ptr = format!("fptr.{}.payload", dst.0);
                writeln!(out, "  %{payload_ptr} = getelementptr %{sname}, ptr %v{}, i32 0, i32 1", base.0).unwrap();
                // The payload pointer is already the right base — the caller will
                // cast/load from it at the right offset. For now, return the payload ptr.
                writeln!(out, "  %v{} = bitcast ptr %{payload_ptr} to ptr", dst.0).unwrap();
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
            let arg_strs: Vec<String> = args.iter().enumerate().map(|(i, a)| {
                let pty = if i < target.params.len() {
                    llvm_type_full(&target.params[i], snames)
                } else {
                    // Variadic or mismatch — try to infer
                    val_types.get(a.0 as usize)
                        .and_then(|t| t.as_ref())
                        .map(|t| llvm_type_full(t, snames))
                        .unwrap_or_else(|| "i64".to_string())
                };
                format!("{pty} %v{}", a.0)
            }).collect();
            if let Some(d) = dst {
                writeln!(out, "  %v{} = call {ret_ty} @{}({})", d.0, target.name, arg_strs.join(", ")).unwrap();
            } else {
                writeln!(out, "  call {ret_ty} @{}({})", target.name, arg_strs.join(", ")).unwrap();
            }
        }
        Inst::CallExtern { dst, name, args, .. } => {
            // Special case: printf/fprintf with GorgetString arg → extract .data field
            // Drop-if-alive guards — no-op in LLVM (drops happen unconditionally)
            if name.starts_with("__gorget_drop_if_alive_open__") || name == "__gorget_drop_if_alive_close" {
                writeln!(out, "  ; {name} (no-op in LLVM)").unwrap();
                return;
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
                        // is_some/is_ok: tag != 0 (Some=1, Ok=1)
                        writeln!(out, "  %v{} = icmp ne i32 %{tag_val}, 0", d.0).unwrap();
                    } else {
                        // is_none/is_err: tag == 0 for None, tag == 2 for Err
                        if name.contains("is_err") {
                            writeln!(out, "  %v{} = icmp eq i32 %{tag_val}, 2", d.0).unwrap();
                        } else {
                            writeln!(out, "  %v{} = icmp eq i32 %{tag_val}, 0", d.0).unwrap();
                        }
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
                    let payload_ptr = format!("unwrap.{block_id}.{uid}.ptr");
                    writeln!(out, "  %{payload_ptr} = getelementptr i8, ptr %v{}, i64 8", args[0].0).unwrap();
                    // For aggregate payloads, return the pointer; for scalars, load i64
                    writeln!(out, "  %v{} = load i64, ptr %{payload_ptr}", d.0).unwrap();
                }
                return;
            }

            if name == "printf" && !args.is_empty() {
                // First arg is ptr to GorgetString — extract .data (field 0)
                let uid = *trap_counter;
                *trap_counter += 1;
                let str_data = format!("printf.{block_id}.{uid}.data");
                let str_val = format!("printf.{block_id}.{uid}.val");
                writeln!(out, "  %{str_data} = getelementptr %GorgetString, ptr %v{}, i32 0, i32 0", args[0].0).unwrap();
                writeln!(out, "  %{str_val} = load ptr, ptr %{str_data}").unwrap();
                // Build remaining args with their types
                let extra_args: Vec<String> = args[1..].iter().map(|a| {
                    let pty = val_types.get(a.0 as usize)
                        .and_then(|t| t.as_ref())
                        .map(|t| llvm_type_full(t, snames))
                        .unwrap_or_else(|| "i64".to_string());
                    format!("{pty} %v{}", a.0)
                }).collect();
                let all_args = if extra_args.is_empty() {
                    format!("ptr %{str_val}")
                } else {
                    format!("ptr %{str_val}, {}", extra_args.join(", "))
                };
                if let Some(d) = dst {
                    writeln!(out, "  %v{} = call i32 (ptr, ...) @printf({all_args})", d.0).unwrap();
                } else {
                    writeln!(out, "  call i32 (ptr, ...) @printf({all_args})").unwrap();
                }
                return;
            }

            // Look up the extern declaration
            let ext = module.externs.iter().find(|e| e.name == *name);
            if let Some(ext) = ext {
                let ret_ty = llvm_type_full(&ext.return_type, snames);
                // For each arg, if extern expects ptr but value is scalar, spill to alloca
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

                    if expects_ptr && !is_ptr && actual_ty.is_some() {
                        // Spill scalar to alloca and pass pointer
                        let spill_ty = llvm_type_full(actual_ty.unwrap(), snames);
                        let spill_name = format!("spill.{}.{i}", a.0);
                        spill_lines.push(format!("  %{spill_name} = alloca {spill_ty}"));
                        spill_lines.push(format!("  store {spill_ty} %v{}, ptr %{spill_name}", a.0));
                        format!("ptr %{spill_name}")
                    } else {
                        let pty = if let Some(ety) = expected_ty {
                            llvm_type_full(ety, snames)
                        } else {
                            actual_ty.map(|t| llvm_type_full(t, snames))
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
                    writeln!(out, "  %v{} = call {ret_ty} @{name}({})", d.0, arg_strs.join(", ")).unwrap();
                } else {
                    writeln!(out, "  call {ret_ty} @{name}({})", arg_strs.join(", ")).unwrap();
                }
            } else {
                // Unknown extern — emit as call with inferred types
                let arg_strs: Vec<String> = args.iter().map(|a| {
                    let pty = val_types.get(a.0 as usize)
                        .and_then(|t| t.as_ref())
                        .map(|t| llvm_type_full(t, snames))
                        .unwrap_or_else(|| "i64".to_string());
                    format!("{pty} %v{}", a.0)
                }).collect();
                if let Some(d) = dst {
                    writeln!(out, "  %v{} = call i64 @{name}({})", d.0, arg_strs.join(", ")).unwrap();
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
                    .map(|t| llvm_type_full(t, snames))
                    .unwrap_or_else(|| "i64".to_string());
                format!("{pty} %v{}", a.0)
            }).collect();
            let ret_ty = if dst.is_some() { "i64" } else { "void" };
            let param_tys: Vec<String> = args.iter().map(|a| {
                val_types.get(a.0 as usize)
                    .and_then(|t| t.as_ref())
                    .map(|t| llvm_type_full(t, snames))
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
            writeln!(out, "  call i32 (ptr, ...) @printf(ptr @.str.{fmt_idx}, i64 %v{}, i64 %v{})", index.0, len.0).unwrap();
            writeln!(out, "  call void @abort()").unwrap();
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
            writeln!(out, "  call i32 (ptr, ...) @printf(ptr @.str.{fmt_idx})").unwrap();
            writeln!(out, "  call void @abort()").unwrap();
            writeln!(out, "  unreachable").unwrap();
            writeln!(out, "{ok_label}:").unwrap();
            *current_label = ok_label;
        }
        Inst::Trap { msg } => {
            let fmt_idx = str_globals.get_index(msg);
            writeln!(out, "  call i32 (ptr, ...) @printf(ptr @.str.{fmt_idx})").unwrap();
            writeln!(out, "  call void @abort()").unwrap();
            writeln!(out, "  unreachable").unwrap();
        }

        // ── Printf / Fprintf ────────────────────────────────────────
        Inst::Printf { fmt, args } => {
            let fmt_idx = str_globals.get_index(fmt);
            let arg_strs: Vec<String> = args.iter().map(|a| {
                let pty = val_types.get(a.0 as usize)
                    .and_then(|t| t.as_ref())
                    .map(|t| llvm_type_full(t, snames))
                    .unwrap_or_else(|| "i64".to_string());
                format!("{pty} %v{}", a.0)
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
            let arg_strs: Vec<String> = args.iter().map(|a| {
                let pty = val_types.get(a.0 as usize)
                    .and_then(|t| t.as_ref())
                    .map(|t| llvm_type_full(t, snames))
                    .unwrap_or_else(|| "i64".to_string());
                format!("{pty} %v{}", a.0)
            }).collect();
            let all_args = if arg_strs.is_empty() {
                format!("ptr %v{}, ptr @.str.{fmt_idx}", fd.0)
            } else {
                format!("ptr %v{}, ptr @.str.{fmt_idx}, {}", fd.0, arg_strs.join(", "))
            };
            writeln!(out, "  call i32 (ptr, ptr, ...) @fprintf({all_args})").unwrap();
        }

        // ── InlineC (C-backend only — skip) ─────────────────────────
        Inst::InlineC { dst, .. } => {
            if let Some(d) = dst {
                writeln!(out, "  %v{} = add i64 0, 0 ; InlineC skipped", d.0).unwrap();
            } else {
                writeln!(out, "  ; InlineC skipped").unwrap();
            }
        }

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

    writeln!(out, "  %{result} = call {{ {lty}, i1 }} {intrinsic}({lty} %v{}, {lty} %v{})", lhs.0, rhs.0).unwrap();
    writeln!(out, "  %{val} = extractvalue {{ {lty}, i1 }} %{result}, 0").unwrap();
    writeln!(out, "  %{flag} = extractvalue {{ {lty}, i1 }} %{result}, 1").unwrap();
    writeln!(out, "  br i1 %{flag}, label %{trap_label}, label %{ok_label}").unwrap();
    writeln!(out, "{trap_label}:").unwrap();
    writeln!(out, "  call void @llvm.trap()").unwrap();
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
            // If function returns aggregate but value is a pointer, load from it
            let val_ty = val_types.get(val.0 as usize).and_then(|t| t.as_ref());
            let is_ptr_val = val_ty.map_or(false, |t| t.is_ptr());
            let is_agg_ret = func.return_type.is_aggregate();
            if is_agg_ret && is_ptr_val {
                writeln!(out, "  %ret.load.{} = load {ret_ty}, ptr %v{}", val.0, val.0).unwrap();
                writeln!(out, "  ret {ret_ty} %ret.load.{}", val.0).unwrap();
            } else {
                writeln!(out, "  ret {ret_ty} %v{}", val.0).unwrap();
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
