use std::fmt::Write;

use super::instructions::*;
use super::types::*;
use super::{BasicBlock, ExternDecl, Function, Global, GlobalInit, Module};

/// Serialize a GIR module to the text format.
pub fn print_module(module: &Module) -> String {
    let mut out = String::new();

    // Summary statistics
    let total_blocks: usize = module.functions.iter().map(|f| f.blocks.len()).sum();
    let total_insts: usize = module.functions.iter()
        .flat_map(|f| f.blocks.iter())
        .map(|b| b.instructions.len())
        .sum();
    let total_locals: usize = module.functions.iter().map(|f| f.locals.len()).sum();
    writeln!(out, "; GIR module: {} functions, {} blocks, {} instructions, {} locals",
        module.functions.len(), total_blocks, total_insts, total_locals).unwrap();
    writeln!(out, "; {} type defs, {} externs, {} globals",
        module.type_registry.type_defs().len(),
        module.externs.len(),
        module.globals.len()).unwrap();
    writeln!(out).unwrap();

    // Type definitions
    if !module.type_registry.type_defs().is_empty() {
        writeln!(out, "; -- Type definitions --").unwrap();
        writeln!(out).unwrap();
        for def in module.type_registry.type_defs() {
            print_type_def(&mut out, def, &module.type_registry);
            writeln!(out).unwrap();
        }
    }

    // Globals
    if !module.globals.is_empty() {
        writeln!(out, "; -- Globals --").unwrap();
        writeln!(out).unwrap();
        for global in &module.globals {
            print_global(&mut out, global, &module.type_registry);
        }
        writeln!(out).unwrap();
    }

    // Extern declarations
    if !module.externs.is_empty() {
        writeln!(out, "; -- Extern declarations --").unwrap();
        writeln!(out).unwrap();
        for ext in &module.externs {
            print_extern(&mut out, ext, &module.type_registry);
        }
        writeln!(out).unwrap();
    }

    // Functions
    if !module.functions.is_empty() {
        writeln!(out, "; -- Functions --").unwrap();
        writeln!(out).unwrap();
        for (i, func) in module.functions.iter().enumerate() {
            print_function(&mut out, func, &module.type_registry);
            if i + 1 < module.functions.len() {
                writeln!(out).unwrap();
            }
        }
    }

    out
}

fn print_type_def(out: &mut String, def: &TypeDef, reg: &TypeRegistry) {
    match &def.kind {
        TypeDefKind::Struct(s) => {
            write!(out, "type {} = struct {{ ", def.name).unwrap();
            for (i, field) in s.fields.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}: {}", field.name, format_type(field.type_id, reg)).unwrap();
            }
            write!(out, " }}").unwrap();
        }
        TypeDefKind::Enum(e) => {
            write!(out, "type {} = enum {{ ", def.name).unwrap();
            for (i, variant) in e.variants.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{} {{ ", variant.name).unwrap();
                for (j, field) in variant.fields.iter().enumerate() {
                    if j > 0 {
                        write!(out, ", ").unwrap();
                    }
                    write!(out, "{}: {}", field.name, format_type(field.type_id, reg)).unwrap();
                }
                write!(out, " }}").unwrap();
            }
            write!(out, " }}").unwrap();
        }
        TypeDefKind::Alias(target) => {
            write!(out, "type {} = alias {}", def.name, format_type(*target, reg)).unwrap();
        }
    }

    // Metadata
    let m = &def.metadata;
    let size_str = m.size.map_or("?".into(), |s| s.to_string());
    let align_str = m.align.map_or("?".into(), |a| a.to_string());
    let drop_str = match &m.drop_strategy {
        DropStrategy::None => "None".into(),
        DropStrategy::Trivial(f) => format!("Trivial(@{})", f),
        DropStrategy::Recursive => "Recursive".into(),
        DropStrategy::Custom(f) => format!("Custom(@{})", f),
    };
    let copy_str = match m.copy_semantics {
        CopySemantics::Trivial => "Copy",
        CopySemantics::Resource => "Move",
    };
    writeln!(
        out,
        " [size: {}, align: {}, drop: {}, copy: {}]",
        size_str, align_str, drop_str, copy_str
    )
    .unwrap();
}

fn print_global(out: &mut String, global: &Global, reg: &TypeRegistry) {
    write!(
        out,
        "global @{}: {} = ",
        global.name,
        format_type(global.type_id, reg)
    )
    .unwrap();
    print_global_init(out, &global.init);
    writeln!(out).unwrap();
}

fn print_global_init(out: &mut String, init: &GlobalInit) {
    match init {
        GlobalInit::Zeroed => write!(out, "zeroed").unwrap(),
        GlobalInit::Struct { type_name, fields } => {
            write!(out, "{} {{ ", type_name).unwrap();
            for (i, (name, val)) in fields.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}: ", name).unwrap();
                print_global_init(out, val);
            }
            write!(out, " }}").unwrap();
        }
        GlobalInit::FnRef(name) => write!(out, "@{}", name).unwrap(),
        GlobalInit::BoxDropRef(inner) => write!(out, "@Box__{}__drop", inner).unwrap(),
        GlobalInit::Bytes(bytes) => write!(out, "bytes[{}]", bytes.len()).unwrap(),
        GlobalInit::Extern { name, args } => {
            write!(out, "extern {name}(").unwrap();
            for (i, arg) in args.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                print_global_init_arg(out, arg);
            }
            write!(out, ")").unwrap();
        }
        GlobalInit::StaticArrayView { elem_type_name, elems } => {
            write!(out, "static_view[{elem_type_name}; {}]{{ ", elems.len()).unwrap();
            for (i, e) in elems.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                print_global_init(out, e);
            }
            write!(out, " }}").unwrap();
        }
    }
}

fn print_global_init_arg(out: &mut String, arg: &crate::ir::GlobalInitArg) {
    use crate::ir::GlobalInitArg;
    match arg {
        GlobalInitArg::Int(n) => write!(out, "{n}").unwrap(),
        GlobalInitArg::Float(x) => write!(out, "{x}").unwrap(),
        GlobalInitArg::Bool(b) => write!(out, "{}", if *b { "true" } else { "false" }).unwrap(),
        GlobalInitArg::Sizeof(t) => write!(out, "sizeof({t})").unwrap(),
        GlobalInitArg::StrLit(s) => write!(out, "{:?}", s).unwrap(),
        GlobalInitArg::AddrOfInline { c_type, value } => {
            write!(out, "&({c_type}){{").unwrap();
            print_global_init_arg(out, value);
            write!(out, "}}").unwrap();
        }
    }
}

fn print_extern(out: &mut String, ext: &ExternDecl, reg: &TypeRegistry) {
    write!(out, "extern fn @{}(", ext.name).unwrap();
    for (i, param) in ext.params.iter().enumerate() {
        if i > 0 {
            write!(out, ", ").unwrap();
        }
        write!(out, "{}", format_type(*param, reg)).unwrap();
    }
    if ext.is_variadic {
        if !ext.params.is_empty() {
            write!(out, ", ").unwrap();
        }
        write!(out, "...").unwrap();
    }
    writeln!(out, ") -> {}", format_type(ext.return_type, reg)).unwrap();
}

fn print_function(out: &mut String, func: &Function, reg: &TypeRegistry) {
    // Signature
    write!(out, "fn @{}(", func.name).unwrap();
    for (i, param_ty) in func.params.iter().enumerate() {
        if i > 0 {
            write!(out, ", ").unwrap();
        }
        write!(out, "{}", format_type(*param_ty, reg)).unwrap();
    }
    writeln!(out, ") -> {} {{", format_type(func.return_type, reg)).unwrap();

    // Locals
    writeln!(out, "    locals:").unwrap();
    for (i, local) in func.locals.iter().enumerate() {
        write!(out, "        _{}: {}", i, format_type(local.type_id, reg)).unwrap();
        if let Some(ref hint) = local.name_hint {
            write!(out, "         ; {}", hint).unwrap();
        }
        writeln!(out).unwrap();
    }
    writeln!(out).unwrap();

    // Blocks
    for (i, block) in func.blocks.iter().enumerate() {
        print_block(out, i, block, reg);
        if i + 1 < func.blocks.len() {
            writeln!(out).unwrap();
        }
    }

    writeln!(out, "}}").unwrap();
}

fn print_block(out: &mut String, index: usize, block: &BasicBlock, reg: &TypeRegistry) {
    writeln!(out, "    bb{}:", index).unwrap();

    for inst in &block.instructions {
        write!(out, "        ").unwrap();
        print_instruction(out, inst, reg);
        writeln!(out).unwrap();
    }

    if let Some(ref term) = block.terminator {
        write!(out, "        ").unwrap();
        print_terminator(out, term, reg);
        writeln!(out).unwrap();
    }
}

fn print_instruction(out: &mut String, inst: &Instruction, reg: &TypeRegistry) {
    match inst {
        Instruction::Assign { mode, dst, value } => {
            let mode_str = match mode {
                AssignMode::Copy => "",
                AssignMode::Move => "[Mv] ",
                AssignMode::Clone => "[Cl] ",
                AssignMode::Borrow => "[Bw] ",
            };
            write!(out, "{}{} = {}", mode_str, format_place(dst), format_operand(value, reg)).unwrap();
        }
        Instruction::FieldLoad { dst, base, field, .. } => {
            write!(out, "_{} = field_load {}, {}", dst.0, format_place(base), field).unwrap();
        }
        Instruction::IndexLoad { dst, base, index, .. } => {
            write!(
                out,
                "_{} = index_load {}, {}",
                dst.0,
                format_place(base),
                format_operand(index, reg)
            )
            .unwrap();
        }
        Instruction::HeapAlloc {
            dst,
            type_id,
            allocator,
        } => {
            write!(
                out,
                "_{} = heap_alloc {}, {}",
                dst.0,
                format_type(*type_id, reg),
                format_operand(allocator, reg)
            )
            .unwrap();
        }
        Instruction::HeapAllocArray {
            dst,
            type_id,
            count,
            allocator,
        } => {
            write!(
                out,
                "_{} = heap_alloc_array {}, {}, {}",
                dst.0,
                format_type(*type_id, reg),
                format_operand(count, reg),
                format_operand(allocator, reg)
            )
            .unwrap();
        }
        Instruction::LoadRef { dst, src } => {
            writeln!(out, "    _{} = LoadRef {}", dst.0, format_place(src)).unwrap();
        }
        Instruction::StoreRef { dst, value } => {
            writeln!(out, "    StoreRef {} = {}", format_place(dst), format_operand(value, reg)).unwrap();
        }
        Instruction::Dealloc { ptr, allocator } => {
            write!(
                out,
                "dealloc {}, {}",
                format_operand(ptr, reg),
                format_operand(allocator, reg)
            )
            .unwrap();
        }
        Instruction::BinOp {
            dst,
            op,
            type_id,
            lhs,
            rhs,
        } => {
            write!(
                out,
                "_{} = {} {} {}, {}",
                dst.0,
                format_binop(*op),
                format_type(*type_id, reg),
                format_operand(lhs, reg),
                format_operand(rhs, reg)
            )
            .unwrap();
        }
        Instruction::FaultableBinOp {
            dst,
            op,
            type_id,
            lhs,
            rhs,
            overflow_handler,
            divzero_handler,
        } => {
            let ovf = overflow_handler.map(|b| format!(" !overflow->bb{}", b.0)).unwrap_or_default();
            let dz = divzero_handler.map(|b| format!(" !divzero->bb{}", b.0)).unwrap_or_default();
            write!(
                out,
                "_{} = {} {} {}, {}{}{}",
                dst.0,
                format_binop(*op),
                format_type(*type_id, reg),
                format_operand(lhs, reg),
                format_operand(rhs, reg),
                ovf,
                dz,
            )
            .unwrap();
        }
        Instruction::FaultableIndexLoad { dst, base, index, fault_handler, .. } => {
            write!(
                out,
                "_{} = index_load {}, {} !bounds->bb{}",
                dst.0,
                format_place(base),
                format_operand(index, reg),
                fault_handler.0,
            )
            .unwrap();
        }
        Instruction::UnOp {
            dst,
            op,
            type_id,
            operand,
        } => {
            write!(
                out,
                "_{} = {} {} {}",
                dst.0,
                format_unop(*op),
                format_type(*type_id, reg),
                format_operand(operand, reg)
            )
            .unwrap();
        }
        Instruction::Cmp {
            dst,
            op,
            type_id,
            lhs,
            rhs,
        } => {
            write!(
                out,
                "_{} = {} {} {}, {}",
                dst.0,
                format_cmpop(*op),
                format_type(*type_id, reg),
                format_operand(lhs, reg),
                format_operand(rhs, reg)
            )
            .unwrap();
        }
        Instruction::Cast {
            dst,
            target_type,
            value,
        } => {
            write!(
                out,
                "_{} = cast {} {}",
                dst.0,
                format_type(*target_type, reg),
                format_operand(value, reg)
            )
            .unwrap();
        }
        Instruction::BitCast {
            dst,
            target_type,
            value,
        } => {
            write!(
                out,
                "_{} = bitcast {} {}",
                dst.0,
                format_type(*target_type, reg),
                format_operand(value, reg)
            )
            .unwrap();
        }
        Instruction::PtrCast {
            dst,
            target_type,
            value,
        } => {
            write!(
                out,
                "_{} = ptr_cast {} {}",
                dst.0,
                format_type(*target_type, reg),
                format_operand(value, reg)
            )
            .unwrap();
        }
        Instruction::StructInit {
            dst,
            type_name,
            fields,
        } => {
            write!(out, "_{} = struct_init {} {{ ", dst.0, type_name).unwrap();
            for (i, f) in fields.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}", format_operand(f, reg)).unwrap();
            }
            write!(out, " }}").unwrap();
        }
        Instruction::EnumInit {
            dst,
            type_name,
            variant,
            fields,
        } => {
            write!(out, "_{} = enum_init {}::{} {{ ", dst.0, type_name, variant).unwrap();
            for (i, f) in fields.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}", format_operand(f, reg)).unwrap();
            }
            write!(out, " }}").unwrap();
        }
        Instruction::TupleInit { dst, elements } => {
            write!(out, "_{} = tuple_init (", dst.0).unwrap();
            for (i, e) in elements.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}", format_operand(e, reg)).unwrap();
            }
            write!(out, ")").unwrap();
        }
        Instruction::TagOf { dst, operand } => {
            write!(out, "_{} = tag_of {}", dst.0, format_operand(operand, reg)).unwrap();
        }
        Instruction::EnumFieldLoad { dst, base, variant, field, mode } => {
            let mode_str = match mode {
                crate::ir::instructions::EnumFieldLoadMode::Move => "",
                crate::ir::instructions::EnumFieldLoadMode::Borrow => " borrow",
            };
            write!(
                out,
                "_{} = enum_field_load{} {}, {}, {}",
                dst.0,
                mode_str,
                format_place(base),
                variant,
                field
            )
            .unwrap();
        }
        Instruction::Call { dst, func, args, .. } => {
            if let Some(d) = dst {
                write!(out, "_{} = ", d.0).unwrap();
            }
            write!(out, "call @{}(", func).unwrap();
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}", format_operand(a, reg)).unwrap();
            }
            write!(out, ")").unwrap();
        }
        Instruction::FaultableCall { dst, func, args, fault_slot, overflow_handler, divzero_handler, bounds_handler } => {
            if let Some(d) = dst {
                write!(out, "_{} = ", d.0).unwrap();
            }
            write!(out, "fault_call @{}(", func).unwrap();
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}", format_operand(a, reg)).unwrap();
            }
            write!(out, ") fault_slot={}", format_place(fault_slot)).unwrap();
            if let Some(h) = overflow_handler {
                write!(out, " overflow->bb{}", h.0).unwrap();
            }
            if let Some(h) = divzero_handler {
                write!(out, " divzero->bb{}", h.0).unwrap();
            }
            if let Some(h) = bounds_handler {
                write!(out, " bounds->bb{}", h.0).unwrap();
            }
        }
        Instruction::CallIndirect { dst, callee, args } => {
            if let Some(d) = dst {
                write!(out, "_{} = ", d.0).unwrap();
            }
            write!(out, "call_indirect {}(", format_operand(callee, reg)).unwrap();
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}", format_operand(a, reg)).unwrap();
            }
            write!(out, ")").unwrap();
        }
        Instruction::CallExtern { dst, func, args } => {
            if let Some(d) = dst {
                write!(out, "_{} = ", d.0).unwrap();
            }
            write!(out, "call_extern @{}(", func).unwrap();
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}", format_operand(a, reg)).unwrap();
            }
            write!(out, ")").unwrap();
        }
        Instruction::MoveZero { place } => {
            write!(out, "move_zero {}", format_place(place)).unwrap();
        }
        Instruction::Borrow { dst, place } => {
            write!(out, "_{} = borrow {}", dst.0, format_place(place)).unwrap();
        }
        Instruction::BorrowMut { dst, place } => {
            write!(out, "_{} = borrow_mut {}", dst.0, format_place(place)).unwrap();
        }
        Instruction::Drop { place } => {
            write!(out, "drop {}", format_place(place)).unwrap();
        }
        Instruction::DropIfAlive { place } => {
            write!(out, "drop_if_alive {}", format_place(place)).unwrap();
        }
        Instruction::LoadThreadLocal { dst, name } => {
            write!(out, "_{} = load_thread_local @{}", dst.0, name).unwrap();
        }
        Instruction::PushAllocator { allocator } => {
            write!(out, "push_allocator {}", format_operand(allocator, reg)).unwrap();
        }
        Instruction::PopAllocator => {
            write!(out, "pop_allocator").unwrap();
        }
        Instruction::InlineC { code } => {
            write!(out, "inline_c {code:?}").unwrap();
        }
        Instruction::GlobalAssign { name, value } => {
            write!(out, "global_assign {name} = {}", format_operand(value, reg)).unwrap();
        }
        Instruction::Nop => {
            write!(out, "nop").unwrap();
        }
    }
}

fn print_terminator(out: &mut String, term: &Terminator, reg: &TypeRegistry) {
    match term {
        Terminator::Return(value) => {
            write!(out, "return {}", format_operand(value, reg)).unwrap();
        }
        Terminator::Jump(target) => {
            write!(out, "jump bb{}", target.0).unwrap();
        }
        Terminator::Branch {
            cond,
            then_block,
            else_block,
        } => {
            write!(
                out,
                "br {}, bb{}, bb{}",
                format_operand(cond, reg),
                then_block.0,
                else_block.0
            )
            .unwrap();
        }
        Terminator::Switch {
            value,
            cases,
            default,
        } => {
            write!(out, "switch {}, [", format_operand(value, reg)).unwrap();
            for (i, (val, block)) in cases.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}: bb{}", val, block.0).unwrap();
            }
            write!(out, "], default: bb{}", default.0).unwrap();
        }
        Terminator::Invoke {
            func,
            args,
            dst,
            normal,
            error,
        } => {
            if let Some(d) = dst {
                write!(out, "_{} = ", d.0).unwrap();
            }
            write!(out, "invoke @{}(", func).unwrap();
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                write!(out, "{}", format_operand(a, reg)).unwrap();
            }
            write!(out, ") -> bb{}, bb{}", normal.0, error.0).unwrap();
        }
        Terminator::Unreachable => {
            write!(out, "unreachable").unwrap();
        }
    }
}

fn format_type(type_id: TypeId, reg: &TypeRegistry) -> String {
    match reg.get(type_id) {
        Some(GirType::Bool) => "bool".into(),
        Some(GirType::I8) => "i8".into(),
        Some(GirType::I16) => "i16".into(),
        Some(GirType::I32) => "i32".into(),
        Some(GirType::I64) => "i64".into(),
        Some(GirType::U8) => "u8".into(),
        Some(GirType::U16) => "u16".into(),
        Some(GirType::U32) => "u32".into(),
        Some(GirType::U64) => "u64".into(),
        Some(GirType::F32) => "f32".into(),
        Some(GirType::F64) => "f64".into(),
        Some(GirType::Unit) => "unit".into(),
        Some(GirType::Ptr(inner)) => format!("*{}", format_type(*inner, reg)),
        Some(GirType::MutPtr(inner)) => format!("*mut {}", format_type(*inner, reg)),
        Some(GirType::FnPtr {
            params,
            return_type,
            ..
        }) => {
            let params_str: Vec<String> = params.iter().map(|p| format_type(*p, reg)).collect();
            format!("fn({}) -> {}", params_str.join(", "), format_type(*return_type, reg))
        }
        Some(GirType::Named(name)) => name.clone(),
        None => format!("?TypeId({})", type_id.0),
    }
}

fn format_operand(op: &Operand, _reg: &TypeRegistry) -> String {
    match op {
        Operand::Copy(place) => format!("copy {}", format_place(place)),
        Operand::Move(place) => format!("move {}", format_place(place)),
        Operand::Constant(c) => format!("const {}", format_constant(c)),
    }
}

fn format_place(place: &Place) -> String {
    let mut s = format!("_{}", place.local.0);
    for proj in &place.projections {
        match proj {
            Projection::Field(idx) => write!(s, ".{}", idx).unwrap(),
            Projection::Index(local) => write!(s, "[_{}]", local.0).unwrap(),
            Projection::Deref => write!(s, ".*").unwrap(),
        }
    }
    s
}

fn format_constant(c: &Constant) -> String {
    match c {
        Constant::Bool(b) => format!("{}", b),
        Constant::I8(n) => format!("{}i8", n),
        Constant::I16(n) => format!("{}i16", n),
        Constant::I32(n) => format!("{}i32", n),
        Constant::I64(n) => format!("{}i64", n),
        Constant::U8(n) => format!("{}u8", n),
        Constant::U16(n) => format!("{}u16", n),
        Constant::U32(n) => format!("{}u32", n),
        Constant::U64(n) => format!("{}u64", n),
        Constant::F32(n) => format!("{}f32", n),
        Constant::F64(n) => format!("{}f64", n),
        Constant::Str(s) => format!("\"{}\"", s),
        Constant::Null => "null".into(),
        Constant::Unit => "unit".into(),
        Constant::SizeOf(type_id) => format!("sizeof(Type{})", type_id.0),
        Constant::FuncRef(name) => format!("@{}", name),
        Constant::GlobalRef(name) => format!("global:{}", name),
        Constant::GlobalRefPtr(name) => format!("&global:{}", name),
    }
}

fn format_binop(op: BinOp) -> &'static str {
    match op {
        BinOp::Add => "add",
        BinOp::Sub => "sub",
        BinOp::Mul => "mul",
        BinOp::Div => "div",
        BinOp::Rem => "rem",
        BinOp::Mod => "mod",
        BinOp::Pow => "pow",
        BinOp::BitAnd => "bit_and",
        BinOp::BitOr => "bit_or",
        BinOp::BitXor => "bit_xor",
        BinOp::Shl => "shl",
        BinOp::Shr => "shr",
        BinOp::AddWrap => "add_wrap",
        BinOp::SubWrap => "sub_wrap",
        BinOp::MulWrap => "mul_wrap",
    }
}

fn format_unop(op: UnOp) -> &'static str {
    match op {
        UnOp::Neg => "neg",
        UnOp::Not => "not",
        UnOp::BitNot => "bit_not",
    }
}

fn format_cmpop(op: CmpOp) -> &'static str {
    match op {
        CmpOp::Eq => "cmp_eq",
        CmpOp::Ne => "cmp_ne",
        CmpOp::Lt => "cmp_lt",
        CmpOp::Le => "cmp_le",
        CmpOp::Gt => "cmp_gt",
        CmpOp::Ge => "cmp_ge",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::builder::FunctionBuilder;

    #[test]
    fn print_empty_function() {
        let mut module = Module::new();
        let mut b = FunctionBuilder::new("main", I32_TYPE, &[]);
        b.ret(FunctionBuilder::const_i32(0));
        module.functions.push(b.build());

        let text = print_module(&module);
        assert!(text.contains("fn @main() -> i32 {"));
        assert!(text.contains("_0: i32"));
        assert!(text.contains("return const 0i32"));
        assert!(text.contains("}"));
    }

    #[test]
    fn print_type_defs() {
        let mut module = Module::new();

        // Struct
        module.type_registry.add_type_def(TypeDef {
            name: "Point".into(),
            kind: TypeDefKind::Struct(StructDef {
                fields: vec![
                    StructField { name: "x".into(), type_id: F64_TYPE },
                    StructField { name: "y".into(), type_id: F64_TYPE },
                ],
            }),
            metadata: TypeMetadata {
                size: Some(16),
                align: Some(8),
                drop_strategy: DropStrategy::None,
                copy_semantics: CopySemantics::Trivial,
                ..Default::default()
            },
        });

        // Enum
        module.type_registry.add_type_def(TypeDef {
            name: "Option__int".into(),
            kind: TypeDefKind::Enum(EnumDef {
                variants: vec![
                    EnumVariant {
                        name: "Some".into(),
                        fields: vec![StructField {
                            name: "_0".into(),
                            type_id: I64_TYPE,
                        }],
                    },
                    EnumVariant {
                        name: "None".into(),
                        fields: vec![],
                    },
                ],
            }),
            metadata: TypeMetadata::default(),
        });

        let text = print_module(&module);
        assert!(text.contains("type Point = struct { x: f64, y: f64 }"));
        assert!(text.contains("[size: 16, align: 8, drop: None, copy: Copy]"));
        assert!(text.contains("type Option__int = enum { Some { _0: i64 }, None {  } }"));
    }

    #[test]
    fn print_instructions() {
        let mut module = Module::new();
        let mut b = FunctionBuilder::new(
            "compute",
            I64_TYPE,
            &[(I64_TYPE, Some("x"))],
        );

        // Various instructions
        let sum = b.bin_op(
            BinOp::Add,
            I64_TYPE,
            FunctionBuilder::copy(LocalId(1)),
            FunctionBuilder::const_i64(10),
        );
        let _neg = b.un_op(UnOp::Neg, I64_TYPE, FunctionBuilder::copy(sum));
        let _result = b.call("helper", vec![FunctionBuilder::copy(LocalId(1))], I64_TYPE);
        b.call_void("print_int", vec![FunctionBuilder::copy(LocalId(1))]);
        b.drop(Place::local(LocalId(1)));
        b.ret(FunctionBuilder::copy(LocalId(0)));

        module.functions.push(b.build());

        let text = print_module(&module);
        assert!(text.contains("add i64 copy _1, const 10i64"));
        assert!(text.contains("neg i64"));
        assert!(text.contains("call @helper("));
        assert!(text.contains("call @print_int("));
        assert!(text.contains("drop _1"));
        assert!(text.contains("return copy _0"));
    }

    #[test]
    fn print_full_module() {
        let mut module = Module::new();

        // Type def
        let point_type_id = module.type_registry.insert(GirType::Named("Point".into()));
        module.type_registry.add_type_def(TypeDef {
            name: "Point".into(),
            kind: TypeDefKind::Struct(StructDef {
                fields: vec![
                    StructField { name: "x".into(), type_id: F64_TYPE },
                    StructField { name: "y".into(), type_id: F64_TYPE },
                ],
            }),
            metadata: TypeMetadata {
                size: Some(16),
                align: Some(8),
                drop_strategy: DropStrategy::None,
                copy_semantics: CopySemantics::Trivial,
                ..Default::default()
            },
        });

        // Global
        module.globals.push(Global {
            name: "origin".into(),
            type_id: point_type_id,
            init: GlobalInit::Zeroed,
        });

        // Extern
        module.externs.push(ExternDecl {
            name: "printf".into(),
            params: vec![],
            return_type: I32_TYPE,
            is_variadic: true,
            param_abis: vec![],
            returns_borrowed: false,
        });

        // Function
        let mut b = FunctionBuilder::new("main", I32_TYPE, &[]);
        let p = b.struct_init("Point", point_type_id, vec![
            FunctionBuilder::const_f64(1.0),
            FunctionBuilder::const_f64(2.5),
        ]);
        let _x = b.field_load(Place::local(p), 0, F64_TYPE);
        b.assign(Place::local(LocalId(0)), FunctionBuilder::const_i32(0));
        b.ret(FunctionBuilder::copy(LocalId(0)));
        module.functions.push(b.build());

        let text = print_module(&module);
        // Verify all sections present
        assert!(text.contains("; -- Type definitions --"));
        assert!(text.contains("; -- Globals --"));
        assert!(text.contains("; -- Extern declarations --"));
        assert!(text.contains("; -- Functions --"));
        assert!(text.contains("type Point = struct"));
        assert!(text.contains("global @origin:"));
        assert!(text.contains("extern fn @printf(...) -> i32"));
        assert!(text.contains("fn @main() -> i32 {"));
        assert!(text.contains("struct_init Point"));
        assert!(text.contains("field_load"));
    }
}
