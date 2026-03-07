//! LIR → C backend.
//!
//! Thin 1:1 translation from LIR to C code. No semantic decisions —
//! all type coercions, drop calls, vtable dispatch, etc. are already
//! explicit in LIR instructions.

use crate::lir::*;
use std::collections::HashMap;
use std::fmt::Write;

/// Names of structs provided by the Gorget C runtime — these should NOT
/// be re-defined by the LIR backend.
const RUNTIME_STRUCTS: &[&str] = &[
    "Str", "GorgetString", "GorgetArray", "GorgetClosure",
    "TraitObj", "TaskHandle", "GorgetRange",
];

/// Build a mapping from StructId → C type name.
/// Runtime-provided structs use their real names; user structs use `__lir_s{id}`.
fn build_struct_names(module: &LirModule) -> HashMap<u32, String> {
    let mut map = HashMap::new();
    for (i, def) in module.structs.iter().enumerate() {
        if RUNTIME_STRUCTS.contains(&def.name.as_str()) {
            map.insert(i as u32, def.name.clone());
        } else {
            map.insert(i as u32, format!("__lir_s{i}"));
        }
    }
    map
}

/// Generate C code from an LIR module.
pub fn generate_c(module: &LirModule) -> String {
    generate_c_inner(module, true)
}

/// Generate C code from an LIR module, optionally including the Gorget runtime.
pub fn generate_c_inner(module: &LirModule, include_runtime: bool) -> String {
    let struct_names = build_struct_names(module);
    let mut out = String::with_capacity(if include_runtime { 256 * 1024 } else { 4096 });

    if include_runtime {
        // Include the full Gorget runtime (provides Str, GorgetString, collections, etc.)
        out.push_str(crate::backend::c::c_runtime::RUNTIME_PREAMBLE);
        out.push_str(crate::backend::c::c_runtime::PANIC_NORMAL);
        out.push_str(crate::backend::c::c_runtime::RUNTIME_CORE);
        writeln!(out).unwrap();
    } else {
        // Minimal headers for standalone mode
        writeln!(out, "#include <stdint.h>").unwrap();
        writeln!(out, "#include <stdbool.h>").unwrap();
        writeln!(out, "#include <stdio.h>").unwrap();
        writeln!(out, "#include <string.h>").unwrap();
        writeln!(out, "#include <stdlib.h>").unwrap();
        writeln!(out).unwrap();
    }

    // Struct forward declarations (skip runtime-provided structs)
    for (i, def) in module.structs.iter().enumerate() {
        if def.fields.is_empty() || RUNTIME_STRUCTS.contains(&def.name.as_str()) {
            continue;
        }
        let cname = &struct_names[&(i as u32)];
        writeln!(out, "typedef struct {cname} {cname};").unwrap();
    }
    writeln!(out).unwrap();

    // Struct definitions (skip runtime-provided structs)
    for (i, def) in module.structs.iter().enumerate() {
        if def.fields.is_empty() || RUNTIME_STRUCTS.contains(&def.name.as_str()) {
            continue;
        }
        let cname = &struct_names[&(i as u32)];
        writeln!(out, "// {}", def.name).unwrap();
        writeln!(out, "struct {cname} {{").unwrap();
        for (fname, fty) in &def.fields {
            writeln!(out, "    {} {};", c_type_named(fty, &struct_names), c_field_name(fname)).unwrap();
        }
        writeln!(out, "}};").unwrap();
        writeln!(out).unwrap();
    }

    // Extern declarations (skip functions already provided by included headers)
    for ext in &module.externs {
        if is_std_header_fn(&ext.name) {
            continue;
        }
        // Skip variadic externs with no named params — these are Gorget runtime
        // functions that lack proper type info in the LIR; declaring them as
        // `int32_t foo(...)` is invalid C.  They'll be resolved at link time
        // when the runtime is included.
        if ext.is_variadic && ext.params.is_empty() {
            continue;
        }
        write!(out, "{} {}(", c_type_named(&ext.return_type, &struct_names), ext.name).unwrap();
        if ext.params.is_empty() && !ext.is_variadic {
            write!(out, "void").unwrap();
        } else {
            for (i, p) in ext.params.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}", c_type_named(p, &struct_names)).unwrap();
            }
            if ext.is_variadic {
                if !ext.params.is_empty() {
                    write!(out, ", ").unwrap();
                }
                write!(out, "...").unwrap();
            }
        }
        writeln!(out, ");").unwrap();
    }
    if !module.externs.is_empty() {
        writeln!(out).unwrap();
    }

    // Global declarations
    for (i, g) in module.globals.iter().enumerate() {
        let kw = if g.is_const { "const " } else { "" };
        write!(out, "{kw}{} __lir_g{i}", c_type_named(&g.ty, &struct_names)).unwrap();
        emit_global_init(&mut out, &g.init);
        writeln!(out, "; // {}", g.name).unwrap();
    }
    if !module.globals.is_empty() {
        writeln!(out).unwrap();
    }

    // Function forward declarations
    for func in &module.functions {
        write!(out, "{} {}(", c_type_named(&func.return_type, &struct_names), c_func_name(&func.name)).unwrap();
        if func.params.is_empty() {
            write!(out, "void").unwrap();
        } else {
            for (i, p) in func.params.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{} __p{i}", c_type_named(p, &struct_names)).unwrap();
            }
        }
        writeln!(out, ");").unwrap();
    }
    writeln!(out).unwrap();

    // Function definitions
    for func in &module.functions {
        emit_function(&mut out, func, module, &struct_names);
        writeln!(out).unwrap();
    }

    out
}

fn emit_function(out: &mut String, func: &LirFunction, module: &LirModule, sn: &HashMap<u32, String>) {
    // Signature
    write!(out, "{} {}(", c_type_named(&func.return_type, sn), c_func_name(&func.name)).unwrap();
    if func.params.is_empty() {
        write!(out, "void").unwrap();
    } else {
        for (i, p) in func.params.iter().enumerate() {
            if i > 0 {
                write!(out, ", ").unwrap();
            }
            write!(out, "{} __p{i}", c_type_named(p, sn)).unwrap();
        }
    }
    writeln!(out, ") {{").unwrap();

    // Slot declarations
    for (i, slot) in func.slots.iter().enumerate() {
        let ty_str = c_type_named(&slot.ty, sn);
        if ty_str == "void" {
            // Type couldn't be resolved — skip (will cause errors if used)
            writeln!(out, "    // __s{i}: void (unresolved type)").unwrap();
            continue;
        }
        write!(out, "    {ty_str} __s{i}").unwrap();
        // Zero-initialize
        if slot.ty.is_scalar() {
            write!(out, " = 0").unwrap();
        } else {
            write!(out, " = {{0}}").unwrap();
        }
        writeln!(out, ";").unwrap();
    }

    // Value declarations — collect all values defined in the function.
    let mut max_val = 0u32;
    for block in &func.blocks {
        for (vid, _) in &block.params {
            if vid.0 >= max_val {
                max_val = vid.0 + 1;
            }
        }
        for inst in &block.insts {
            if let Some(dst) = inst.dst() {
                if dst.0 >= max_val {
                    max_val = dst.0 + 1;
                }
            }
        }
    }

    // Declare all values as their inferred types.
    // Build a type map from instructions (two passes for arithmetic type propagation).
    let mut val_types: Vec<Option<LirType>> = vec![None; max_val as usize];
    for block in &func.blocks {
        for (vid, ty) in &block.params {
            val_types[vid.0 as usize] = Some(ty.clone());
        }
        for inst in &block.insts {
            if let Some(ty) = infer_inst_type(inst, module, &val_types) {
                if let Some(dst) = inst.dst() {
                    val_types[dst.0 as usize] = Some(ty);
                }
            }
        }
    }

    for (i, ty) in val_types.iter().enumerate() {
        if let Some(ty) = ty {
            let ts = c_type_named(ty, sn);
            if ts == "void" {
                writeln!(out, "    // __v{i}: void (unresolved type)").unwrap();
            } else {
                writeln!(out, "    {} __v{i};", ts).unwrap();
            }
        }
    }

    // Block parameter move variables (for parallel copy semantics).
    // Each block param needs a temporary for parallel moves.
    for block in &func.blocks {
        for (vid, ty) in &block.params {
            writeln!(out, "    {} __bp{};", c_type_named(ty, sn), vid.0).unwrap();
        }
    }

    writeln!(out).unwrap();

    // Blocks
    for block in &func.blocks {
        writeln!(out, "__bb{}:", block.id.0).unwrap();

        // Move block params from temporaries.
        for (vid, _) in &block.params {
            writeln!(out, "    __v{} = __bp{};", vid.0, vid.0).unwrap();
        }

        // Instructions
        for inst in &block.insts {
            write!(out, "    ").unwrap();
            emit_inst(out, inst, func, module, sn);
            writeln!(out).unwrap();
        }

        // Terminator
        write!(out, "    ").unwrap();
        emit_term(out, &block.terminator, func);
        writeln!(out).unwrap();
    }

    writeln!(out, "}}").unwrap();
}

fn emit_inst(out: &mut String, inst: &Inst, func: &LirFunction, module: &LirModule, sn: &HashMap<u32, String>) {
    let v = |id: ValueId| -> String { format!("__v{}", id.0) };
    let s = |id: SlotId| -> String { format!("__s{}", id.0) };

    match inst {
        // Slot access
        Inst::SlotStore { slot, value } => {
            let slot_ty = &func.slots[slot.0 as usize].ty;
            if slot_ty.is_aggregate() {
                // Aggregate store: value is a pointer to source data. Use memcpy.
                let ty_name = c_type_named(slot_ty, sn);
                write!(out, "memcpy(&{}, {}, sizeof({}));", s(*slot), v(*value), ty_name).unwrap();
            } else {
                write!(out, "{} = {};", s(*slot), v(*value)).unwrap();
            }
        }
        Inst::SlotLoad { dst, slot, .. } => {
            write!(out, "{} = {};", v(*dst), s(*slot)).unwrap();
        }
        Inst::SlotAddr { dst, slot } => {
            write!(out, "{} = &{};", v(*dst), s(*slot)).unwrap();
        }

        // Constants
        Inst::IConst { dst, value, ty } => {
            write!(out, "{} = ({}){}LL;", v(*dst), c_type_named(ty, sn), value).unwrap();
        }
        Inst::FConst { dst, bits, ty } => {
            let val = f64::from_bits(*bits);
            write!(out, "{} = ({})({});", v(*dst), c_type_named(ty, sn), format_float(val)).unwrap();
        }
        Inst::BoolConst { dst, value } => {
            write!(out, "{} = {};", v(*dst), if *value { "true" } else { "false" }).unwrap();
        }
        Inst::NullPtr { dst } => {
            write!(out, "{} = NULL;", v(*dst)).unwrap();
        }
        Inst::FuncAddr { dst, func } => {
            let name = c_func_name(&module.functions[func.0 as usize].name);
            write!(out, "{} = (void*)&{};", v(*dst), name).unwrap();
        }
        Inst::GlobalAddr { dst, global } => {
            write!(out, "{} = &__lir_g{};", v(*dst), global.0).unwrap();
        }
        Inst::StrLit { dst, value } => {
            let escaped = escape_c_string(value);
            write!(out, "{} = \"{}\";", v(*dst), escaped).unwrap();
        }
        Inst::ParamRef { dst, index, .. } => {
            write!(out, "{} = __p{};", v(*dst), index).unwrap();
        }

        // Arithmetic
        Inst::Add { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} + {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }
        Inst::Sub { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} - {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }
        Inst::Mul { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} * {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }
        Inst::Div { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} / {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }
        Inst::Rem { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} % {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }
        Inst::Mod { dst, lhs, rhs, .. } => {
            // Python-style modulo: ((a % b) + b) % b
            write!(
                out,
                "{{ typeof({l}) __t = {l} % {r}; {d} = __t + (__t != 0 && (__t ^ {r}) < 0 ? {r} : 0); }}",
                d = v(*dst),
                l = v(*lhs),
                r = v(*rhs)
            ).unwrap();
        }
        Inst::Neg { dst, operand, .. } => {
            write!(out, "{} = -{};", v(*dst), v(*operand)).unwrap();
        }

        // Bitwise
        Inst::BitAnd { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} & {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }
        Inst::BitOr { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} | {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }
        Inst::BitXor { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} ^ {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }
        Inst::BitNot { dst, operand, .. } => {
            write!(out, "{} = ~{};", v(*dst), v(*operand)).unwrap();
        }
        Inst::Shl { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} << {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }
        Inst::Shr { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} >> {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }

        // Comparison & logic
        Inst::Cmp { dst, op, lhs, rhs } => {
            let c_op = match op {
                CmpOp::Eq => "==",
                CmpOp::Ne => "!=",
                CmpOp::Lt => "<",
                CmpOp::Le => "<=",
                CmpOp::Gt => ">",
                CmpOp::Ge => ">=",
            };
            write!(out, "{} = {} {} {};", v(*dst), v(*lhs), c_op, v(*rhs)).unwrap();
        }
        Inst::Not { dst, operand } => {
            write!(out, "{} = !{};", v(*dst), v(*operand)).unwrap();
        }

        // Type conversions
        Inst::IntCast { dst, value, to } => {
            write!(out, "{} = ({})({});", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
        }
        Inst::FloatCast { dst, value, to } => {
            write!(out, "{} = ({})({});", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
        }
        Inst::IntToFloat { dst, value, to } => {
            write!(out, "{} = ({})({});", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
        }
        Inst::FloatToInt { dst, value, to } => {
            write!(out, "{} = ({})({});", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
        }
        Inst::PtrCast { dst, value } => {
            write!(out, "{} = (void*)({});", v(*dst), v(*value)).unwrap();
        }
        Inst::Bitcast { dst, value, to } => {
            // Use memcpy for type-punning to avoid strict aliasing violations.
            write!(
                out,
                "memcpy(&{d}, &{s}, sizeof({t}));",
                d = v(*dst),
                s = v(*value),
                t = c_type_named(to, sn)
            ).unwrap();
        }

        // Memory
        Inst::Load { dst, ptr, ty } => {
            write!(out, "{} = *({} *)({});", v(*dst), c_type_named(ty, sn), v(*ptr)).unwrap();
        }
        Inst::Store { ptr, value } => {
            // Generic store — type is determined by context.
            // Use memcpy as a safe fallback that handles any type size.
            write!(out, "memcpy({p}, &{val}, sizeof({val}));", p = v(*ptr), val = v(*value)).unwrap();
        }
        Inst::FieldPtr { dst, base, struct_id, field } => {
            let field_name = &module.structs[struct_id.0 as usize].fields[*field as usize].0;
            write!(
                out,
                "{} = (void*)&(({} *)({}))->{};",
                v(*dst),
                sn.get(&struct_id.0).map(|s| s.as_str()).unwrap_or("void"),
                v(*base),
                c_field_name(field_name)
            ).unwrap();
        }
        Inst::ElemPtr { dst, base, index, elem_size } => {
            write!(
                out,
                "{} = (void*)((char*)({}) + (int64_t)({}) * {});",
                v(*dst),
                v(*base),
                v(*index),
                elem_size
            ).unwrap();
        }
        Inst::Memset { ptr, byte, size } => {
            write!(out, "memset({}, (int){}, (size_t){});", v(*ptr), v(*byte), v(*size)).unwrap();
        }
        Inst::Memcpy { dst_ptr, src_ptr, size } => {
            write!(
                out,
                "memcpy({}, {}, (size_t){});",
                v(*dst_ptr),
                v(*src_ptr),
                v(*size)
            ).unwrap();
        }

        // Calls
        Inst::Call { dst, func, args } => {
            let fname = c_func_name(&module.functions[func.0 as usize].name);
            if let Some(d) = dst {
                write!(out, "{} = ", v(*d)).unwrap();
            }
            write!(out, "{}(", fname).unwrap();
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}", v(*a)).unwrap();
            }
            write!(out, ");").unwrap();
        }
        Inst::CallExtern { dst, name, args } => {
            if let Some(d) = dst {
                write!(out, "{} = ", v(*d)).unwrap();
            }
            write!(out, "{}(", name).unwrap();
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}", v(*a)).unwrap();
            }
            write!(out, ");").unwrap();
        }
        Inst::CallPtr { dst, callee, args } => {
            if let Some(d) = dst {
                write!(out, "{} = ", v(*d)).unwrap();
            }
            write!(out, "((void*(*)(").unwrap();
            for (i, _) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "void*").unwrap();
            }
            write!(out, "))({}))(", v(*callee)).unwrap();
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "(void*){}", v(*a)).unwrap();
            }
            write!(out, ");").unwrap();
        }

        // Runtime checks
        Inst::BoundsCheck { index, len } => {
            write!(
                out,
                "if ((uint64_t){} >= (uint64_t){}) {{ fprintf(stderr, \"index out of bounds\\n\"); abort(); }}",
                v(*index),
                v(*len)
            ).unwrap();
        }
        Inst::DivCheck { divisor } => {
            write!(
                out,
                "if ({} == 0) {{ fprintf(stderr, \"division by zero\\n\"); abort(); }}",
                v(*divisor)
            ).unwrap();
        }
        Inst::Trap { msg } => {
            write!(out, "fprintf(stderr, \"{}\"); abort();", escape_c_string(msg)).unwrap();
        }

        // Printf
        Inst::Printf { fmt, args } => {
            write!(out, "printf(\"{}\"", escape_c_string(fmt)).unwrap();
            for a in args {
                write!(out, ", {}", v(*a)).unwrap();
            }
            write!(out, ");").unwrap();
        }
        Inst::Fprintf { fd, fmt, args } => {
            // fd is a FILE* or fd int — for now treat as FILE*.
            write!(out, "fprintf((FILE*){}, \"{}\"", v(*fd), escape_c_string(fmt)).unwrap();
            for a in args {
                write!(out, ", {}", v(*a)).unwrap();
            }
            write!(out, ");").unwrap();
        }

        Inst::Nop => {
            write!(out, "/* nop */;").unwrap();
        }
    }
}

fn emit_term(out: &mut String, term: &Term, func: &LirFunction) {
    let v = |id: ValueId| -> String { format!("__v{}", id.0) };

    match term {
        Term::Ret(val) => {
            write!(out, "return {};", v(*val)).unwrap();
        }
        Term::RetVoid => {
            write!(out, "return;").unwrap();
        }
        Term::Jump(target, args) => {
            emit_jump_args(out, *target, args, func);
            write!(out, "goto __bb{};", target.0).unwrap();
        }
        Term::Branch {
            cond,
            then_block,
            then_args,
            else_block,
            else_args,
        } => {
            writeln!(out, "if ({}) {{", v(*cond)).unwrap();
            if !then_args.is_empty() {
                write!(out, "        ").unwrap();
                emit_jump_args(out, *then_block, then_args, func);
                writeln!(out).unwrap();
            }
            writeln!(out, "        goto __bb{};", then_block.0).unwrap();
            writeln!(out, "    }} else {{").unwrap();
            if !else_args.is_empty() {
                write!(out, "        ").unwrap();
                emit_jump_args(out, *else_block, else_args, func);
                writeln!(out).unwrap();
            }
            writeln!(out, "        goto __bb{};", else_block.0).unwrap();
            write!(out, "    }}").unwrap();
        }
        Term::Switch {
            value,
            cases,
            default,
            default_args,
        } => {
            writeln!(out, "switch ((int64_t){}) {{", v(*value)).unwrap();
            for (val, block, args) in cases {
                write!(out, "        case {val}: ").unwrap();
                emit_jump_args(out, *block, args, func);
                writeln!(out, "goto __bb{};", block.0).unwrap();
            }
            write!(out, "        default: ").unwrap();
            emit_jump_args(out, *default, default_args, func);
            writeln!(out, "goto __bb{};", default.0).unwrap();
            write!(out, "    }}").unwrap();
        }
        Term::Unreachable => {
            write!(out, "__builtin_unreachable();").unwrap();
        }
    }
}

/// Emit parallel-copy assignments for block parameter passing.
/// Stores args into the target block's param temporaries (__bp{vid}).
fn emit_jump_args(out: &mut String, target: BlockId, args: &[ValueId], func: &LirFunction) {
    if args.is_empty() {
        return;
    }
    let target_block = &func.blocks[target.0 as usize];
    for (arg, (param_vid, _)) in args.iter().zip(target_block.params.iter()) {
        write!(out, "__bp{} = __v{}; ", param_vid.0, arg.0).unwrap();
    }
}

fn emit_global_init(out: &mut String, init: &LirGlobalInit) {
    match init {
        LirGlobalInit::Zeroed => write!(out, " = {{0}}").unwrap(),
        LirGlobalInit::Bytes(b) => write!(out, " /* {} bytes */", b.len()).unwrap(),
        LirGlobalInit::FuncAddr(_) => write!(out, " = NULL /* func addr */").unwrap(),
        LirGlobalInit::Struct { fields, .. } => {
            write!(out, " = {{").unwrap();
            for (i, f) in fields.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                emit_global_init(out, f);
            }
            write!(out, "}}").unwrap();
        }
    }
}

/// Map LirType to C type string.
/// Returns true if the function is provided by standard C headers
/// (stdio.h, stdlib.h, string.h) and should not be re-declared.
fn is_std_header_fn(name: &str) -> bool {
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
    )
}

fn c_type_named(ty: &LirType, struct_names: &HashMap<u32, String>) -> String {
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
        LirType::Ptr => "void*".into(),
        LirType::Struct(id) => struct_names
            .get(&id.0)
            .cloned()
            .unwrap_or_else(|| format!("__lir_s{}", id.0)),
        LirType::Void => "void".into(),
    }
}

/// Sanitize a field name for C.
fn c_field_name(name: &str) -> String {
    name.replace('.', "_").replace('-', "_")
}

/// C keywords and type names that cannot be used as identifiers.
const C_RESERVED: &[&str] = &[
    "auto", "break", "case", "char", "const", "continue", "default", "do",
    "double", "else", "enum", "extern", "float", "for", "goto", "if",
    "int", "long", "register", "return", "short", "signed", "sizeof",
    "static", "struct", "switch", "typedef", "union", "unsigned", "void",
    "volatile", "while", "inline", "restrict", "_Bool", "_Complex",
    "_Imaginary", "bool", "true", "false",
];

/// Escape a function name that clashes with C keywords by adding a prefix.
fn c_func_name(name: &str) -> String {
    if C_RESERVED.contains(&name) {
        format!("__gg_{name}")
    } else {
        name.to_string()
    }
}

/// Format a float for C source.
fn format_float(val: f64) -> String {
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

/// Escape a string for C string literal.
fn escape_c_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 8);
    for c in s.chars() {
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
            }
        }
    }
    out
}

/// Infer the result type of an instruction (for variable declarations).
/// `val_types` provides already-resolved types for operands (used for arithmetic propagation).
fn infer_inst_type(inst: &Inst, module: &LirModule, _val_types: &[Option<LirType>]) -> Option<LirType> {
    match inst {
        Inst::SlotLoad { ty, .. } => Some(ty.clone()),
        Inst::SlotAddr { .. } => Some(LirType::Ptr),
        Inst::IConst { ty, .. } => Some(ty.clone()),
        Inst::FConst { ty, .. } => Some(ty.clone()),
        Inst::BoolConst { .. } => Some(LirType::Bool),
        Inst::NullPtr { .. } => Some(LirType::Ptr),
        Inst::FuncAddr { .. } => Some(LirType::Ptr),
        Inst::GlobalAddr { .. } => Some(LirType::Ptr),
        Inst::StrLit { .. } => Some(LirType::Ptr), // simplified
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

        Inst::Load { ty, .. } => Some(ty.clone()),
        Inst::FieldPtr { .. } | Inst::ElemPtr { .. } => Some(LirType::Ptr),

        Inst::Call { func, .. } => {
            Some(module.functions[func.0 as usize].return_type.clone())
        }
        Inst::CallExtern { name, .. } => {
            module.externs.iter()
                .find(|e| &e.name == name)
                .map(|e| e.return_type.clone())
                .or(Some(LirType::I64))
        }
        Inst::CallPtr { .. } => Some(LirType::I64), // default

        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generate_minimal() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("main".into(), vec![], LirType::I32);
        let bb0 = func.add_block();
        let v0 = func.next_value();
        func.block_mut(bb0).insts.push(Inst::IConst {
            dst: v0,
            ty: LirType::I32,
            value: 0,
        });
        func.block_mut(bb0).terminator = Term::Ret(v0);
        module.add_function(func);

        let c = generate_c(&module);
        assert!(c.contains("int32_t main(void)"));
        assert!(c.contains("__v0 = (int32_t)0LL;"));
        assert!(c.contains("return __v0;"));
    }

    #[test]
    fn generate_arithmetic() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("add".into(), vec![LirType::I64, LirType::I64], LirType::I64);
        let bb0 = func.add_block();

        let v0 = func.next_value(); // param a
        let v1 = func.next_value(); // param b
        let v2 = func.next_value(); // result

        let s0 = func.add_slot(LirType::I64, Some("a".into()));
        let s1 = func.add_slot(LirType::I64, Some("b".into()));

        func.block_mut(bb0).insts = vec![
            Inst::SlotLoad { dst: v0, slot: s0, ty: LirType::I64 },
            Inst::SlotLoad { dst: v1, slot: s1, ty: LirType::I64 },
            Inst::Add { dst: v2, ty: LirType::I64, lhs: v0, rhs: v1, overflow: Overflow::Trap },
        ];
        func.block_mut(bb0).terminator = Term::Ret(v2);
        module.add_function(func);

        let c = generate_c(&module);
        assert!(c.contains("__v2 = __v0 + __v1;"));
        assert!(c.contains("return __v2;"));
    }

    #[test]
    fn generate_branch() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("test".into(), vec![], LirType::I32);

        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();

        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb0).insts.push(Inst::BoolConst { dst: v0, value: true });
        func.block_mut(bb0).terminator = Term::Branch {
            cond: v0,
            then_block: bb1,
            then_args: vec![],
            else_block: bb2,
            else_args: vec![],
        };

        func.block_mut(bb1).insts.push(Inst::IConst { dst: v1, ty: LirType::I32, value: 1 });
        func.block_mut(bb1).terminator = Term::Ret(v1);

        func.block_mut(bb2).insts.push(Inst::IConst { dst: v2, ty: LirType::I32, value: 2 });
        func.block_mut(bb2).terminator = Term::Ret(v2);

        module.add_function(func);

        let c = generate_c(&module);
        assert!(c.contains("if (__v0)"));
        assert!(c.contains("goto __bb1;"));
        assert!(c.contains("goto __bb2;"));
    }

    #[test]
    fn generate_struct() {
        let mut module = LirModule::new();
        let sid = module.add_struct(StructDef {
            name: "Point".into(),
            fields: vec![("x".into(), LirType::F64), ("y".into(), LirType::F64)],
        });

        let mut func = LirFunction::new("get_x".into(), vec![LirType::Ptr], LirType::F64);
        let bb0 = func.add_block();
        let s0 = func.add_slot(LirType::Ptr, Some("p".into()));

        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb0).insts = vec![
            Inst::SlotLoad { dst: v0, slot: s0, ty: LirType::Ptr },
            Inst::FieldPtr { dst: v1, base: v0, struct_id: sid, field: 0 },
            Inst::Load { dst: v2, ptr: v1, ty: LirType::F64 },
        ];
        func.block_mut(bb0).terminator = Term::Ret(v2);
        module.add_function(func);

        let c = generate_c(&module);
        assert!(c.contains("struct __lir_s0"));
        assert!(c.contains("double x;"));
        assert!(c.contains("((__lir_s0 *)(__v0))->x"));
    }

    #[test]
    fn escape_strings() {
        assert_eq!(escape_c_string("hello"), "hello");
        assert_eq!(escape_c_string("a\"b"), "a\\\"b");
        assert_eq!(escape_c_string("line\nend"), "line\\nend");
    }
}
