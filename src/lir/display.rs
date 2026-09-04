//! Human-readable LIR dump (for `--dump-lir` output).

use super::*;
use std::fmt;

/// Display wrapper for a full LIR module.
pub struct DisplayModule<'a>(pub &'a LirModule);

impl<'a> fmt::Display for DisplayModule<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let module = self.0;

        if let Some(ref src) = module.source_filename {
            writeln!(f, "; source: {src}")?;
        }
        writeln!(f)?;

        // Struct definitions
        for (i, def) in module.structs.iter().enumerate() {
            write!(f, "struct.{i} = type {}", def.name)?;
            writeln!(f, " {{")?;
            for (fname, fty) in &def.fields {
                writeln!(f, "    {fname}: {fty},")?;
            }
            writeln!(f, "}}")?;
            writeln!(f)?;
        }

        // Extern declarations
        for ext in &module.externs {
            write!(f, "declare {} @{}(", ext.return_type, ext.name)?;
            for (i, p) in ext.params.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{p}")?;
            }
            if ext.is_variadic {
                if !ext.params.is_empty() {
                    write!(f, ", ")?;
                }
                write!(f, "...")?;
            }
            writeln!(f, ")")?;
        }
        if !module.externs.is_empty() {
            writeln!(f)?;
        }

        // Globals
        for (i, g) in module.globals.iter().enumerate() {
            let kw = if g.is_const { "const" } else { "global" };
            write!(f, "global.{i} = {kw} {} @{}", g.ty, g.name)?;
            write_global_init(f, &g.init)?;
            writeln!(f)?;
        }
        if !module.globals.is_empty() {
            writeln!(f)?;
        }

        // Functions
        for func in &module.functions {
            write_function(f, func, module)?;
            writeln!(f)?;
        }

        Ok(())
    }
}

fn write_global_init(f: &mut fmt::Formatter<'_>, init: &LirGlobalInit) -> fmt::Result {
    match init {
        LirGlobalInit::Zeroed => write!(f, " = zeroed"),
        LirGlobalInit::Bytes(bytes) => write!(f, " = bytes[{}]", bytes.len()),
        LirGlobalInit::FuncAddr(fid) => write!(f, " = {fid}"),
        LirGlobalInit::BoxDropAddr(inner) => write!(f, " = &Box__{inner}__drop"),
        LirGlobalInit::Extern { name, args } => {
            write!(f, " = extern {name}(")?;
            for (i, arg) in args.iter().enumerate() {
                if i > 0 { write!(f, ", ")?; }
                write_global_init_arg(f, arg)?;
            }
            write!(f, ")")
        }
        LirGlobalInit::Struct { struct_id, fields } => {
            write!(f, " = {struct_id} {{")?;
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    write!(f, ",")?;
                }
                write_global_init(f, field)?;
            }
            write!(f, " }}")
        }
        LirGlobalInit::StaticArrayView { elem_ty, elems } => {
            write!(f, " = static_view[{elem_ty:?}; {}]{{", elems.len())?;
            for (i, e) in elems.iter().enumerate() {
                if i > 0 { write!(f, ",")?; }
                write_global_init(f, e)?;
            }
            write!(f, " }}")
        }
    }
}

fn write_global_init_arg(f: &mut fmt::Formatter<'_>, arg: &crate::lir::LirGlobalInitArg) -> fmt::Result {
    use crate::lir::LirGlobalInitArg;
    match arg {
        LirGlobalInitArg::Int(n) => write!(f, "{n}"),
        LirGlobalInitArg::Float(x) => write!(f, "{x}"),
        LirGlobalInitArg::Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
        LirGlobalInitArg::Sizeof(t) => write!(f, "sizeof({t})"),
        LirGlobalInitArg::StrLit(s) => write!(f, "{:?}", s),
        LirGlobalInitArg::AddrOfInline { c_type, value } => {
            write!(f, "&({c_type}){{")?;
            write_global_init_arg(f, value)?;
            write!(f, "}}")
        }
    }
}

fn write_function(
    f: &mut fmt::Formatter<'_>,
    func: &LirFunction,
    _module: &LirModule,
) -> fmt::Result {
    // Signature
    write!(f, "fn @{}(", func.name)?;
    for (i, p) in func.params.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write!(f, "{p}")?;
    }
    writeln!(f, ") -> {} {{", func.return_type)?;

    // Slots
    for (i, slot) in func.slots.iter().enumerate() {
        write!(f, "    s{i}: {}", slot.ty)?;
        if let Some(ref name) = slot.name {
            write!(f, "  ; {name}")?;
        }
        writeln!(f)?;
    }
    if !func.slots.is_empty() {
        writeln!(f)?;
    }

    // Blocks
    for block in &func.blocks {
        write!(f, "  {}(", block.id)?;
        for (i, (vid, ty)) in block.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{vid}: {ty}")?;
        }
        writeln!(f, "):")?;

        for inst in &block.insts {
            write!(f, "    ")?;
            write_inst(f, inst)?;
            writeln!(f)?;
        }

        write!(f, "    ")?;
        write_term(f, &block.terminator)?;
        writeln!(f)?;
    }

    writeln!(f, "}}")
}

fn write_inst(f: &mut fmt::Formatter<'_>, inst: &Inst) -> fmt::Result {
    match inst {
        // Slot access
        Inst::SlotStore { slot, value, .. } => write!(f, "slot_store {slot}, {value}"),
        Inst::SlotLoad { dst, slot, ty } => write!(f, "{dst}: {ty} = slot_load {slot}"),
        Inst::SlotAddr { dst, slot } => write!(f, "{dst}: ptr = slot_addr {slot}"),

        // Constants
        Inst::IConst { dst, ty, value } => write!(f, "{dst}: {ty} = iconst {value}"),
        Inst::FConst { dst, ty, bits } => {
            let val = f64::from_bits(*bits);
            write!(f, "{dst}: {ty} = fconst {val}")
        }
        Inst::BoolConst { dst, value } => write!(f, "{dst}: bool = bconst {value}"),
        Inst::SizeOf { dst, ty } => write!(f, "{dst}: i64 = sizeof {ty}"),
        Inst::EnumInit { target, struct_id, variant_tag, fields } => {
            write!(f, "enum_init {target}, struct{}, tag={variant_tag}, fields=[", struct_id.0)?;
            for (i, (idx, val)) in fields.iter().enumerate() {
                if i > 0 { write!(f, ", ")?; }
                write!(f, "{idx}={val}")?;
            }
            write!(f, "]")
        }
        Inst::EnumCheck { dst, value, struct_id, variant_tag } => {
            write!(f, "{dst}: bool = enum_check {value}, struct{}, tag={variant_tag}", struct_id.0)
        }
        Inst::EnumExtract { dst, value, struct_id, payload_field, ty } => {
            write!(f, "{dst}: {ty} = enum_extract {value}, struct{}, field={payload_field}", struct_id.0)
        }
        Inst::StructInit { target, struct_id, fields } => {
            write!(f, "struct_init {target}, struct{}, fields=[", struct_id.0)?;
            for (i, (idx, val)) in fields.iter().enumerate() {
                if i > 0 { write!(f, ", ")?; }
                write!(f, "{idx}={val}")?;
            }
            write!(f, "]")
        }
        Inst::CowClone { dst, src, ty } => {
            write!(f, "{dst}: {ty} = cow_clone {src}")
        }
        Inst::TraitCall { dst, object, trait_obj_struct, method_idx, args, ret_ty, .. } => {
            if let Some(d) = dst {
                write!(f, "{d}: {ret_ty} = trait_call {object}, {trait_obj_struct}.method[{method_idx}](")?;
            } else {
                write!(f, "trait_call {object}, {trait_obj_struct}.method[{method_idx}](")?;
            }
            for (i, a) in args.iter().enumerate() {
                if i > 0 { write!(f, ", ")?; }
                write!(f, "{a}")?;
            }
            write!(f, ")")
        }
        Inst::HofExpand { coll, hof_op, closure, init, dst, .. } => {
            if let Some(d) = dst {
                write!(f, "{d} = hof_expand.{hof_op:?} {coll}, closure={closure}")?;
            } else {
                write!(f, "hof_expand.{hof_op:?} {coll}, closure={closure}")?;
            }
            if let Some(i) = init { write!(f, ", init={i}")?; }
            Ok(())
        }
        Inst::AddressOf { dst, value, ty } => {
            write!(f, "{dst}: ptr = address_of {value}: {ty}")
        }
        Inst::BoxAlloc { dst, inner_ty, value } => {
            write!(f, "{dst}: ptr = box_alloc {value}: {inner_ty}")
        }
        Inst::NullPtr { dst } => write!(f, "{dst}: ptr = null"),
        Inst::FuncAddr { dst, func } => write!(f, "{dst}: funcref = func_addr {func}"),
        Inst::NamedFuncAddr { dst, name } => write!(f, "{dst}: funcref = named_func_addr @{name}"),
        Inst::GlobalAddr { dst, global } => write!(f, "{dst}: ptr = global_addr {global}"),
        Inst::StrLit { dst, value } => write!(f, "{dst}: struct.Str = str_lit {value:?}"),
        Inst::ParamRef { dst, index, ty } => write!(f, "{dst}: {ty} = param {index}"),

        // Arithmetic
        Inst::Add { dst, ty, lhs, rhs, overflow } => {
            let op = match overflow {
                Overflow::Trap => "add",
                Overflow::Wrap => "add.wrap",
            };
            write!(f, "{dst}: {ty} = {op} {lhs}, {rhs}")
        }
        Inst::Sub { dst, ty, lhs, rhs, overflow } => {
            let op = match overflow {
                Overflow::Trap => "sub",
                Overflow::Wrap => "sub.wrap",
            };
            write!(f, "{dst}: {ty} = {op} {lhs}, {rhs}")
        }
        Inst::Mul { dst, ty, lhs, rhs, overflow } => {
            let op = match overflow {
                Overflow::Trap => "mul",
                Overflow::Wrap => "mul.wrap",
            };
            write!(f, "{dst}: {ty} = {op} {lhs}, {rhs}")
        }
        Inst::Div { dst, ty, lhs, rhs } => write!(f, "{dst}: {ty} = div {lhs}, {rhs}"),
        Inst::Rem { dst, ty, lhs, rhs } => write!(f, "{dst}: {ty} = rem {lhs}, {rhs}"),
        Inst::Mod { dst, ty, lhs, rhs } => write!(f, "{dst}: {ty} = mod {lhs}, {rhs}"),
        Inst::FaultCheck { dst, op, ty, lhs, rhs } => write!(f, "{dst}: bool = fault_check.{op:?}.{ty} {lhs}, {rhs}"),
        Inst::Neg { dst, ty, operand } => write!(f, "{dst}: {ty} = neg {operand}"),

        // Bitwise
        Inst::BitAnd { dst, ty, lhs, rhs } => write!(f, "{dst}: {ty} = band {lhs}, {rhs}"),
        Inst::BitOr { dst, ty, lhs, rhs } => write!(f, "{dst}: {ty} = bor {lhs}, {rhs}"),
        Inst::BitXor { dst, ty, lhs, rhs } => write!(f, "{dst}: {ty} = bxor {lhs}, {rhs}"),
        Inst::BitNot { dst, ty, operand } => write!(f, "{dst}: {ty} = bnot {operand}"),
        Inst::Shl { dst, ty, lhs, rhs } => write!(f, "{dst}: {ty} = shl {lhs}, {rhs}"),
        Inst::Shr { dst, ty, lhs, rhs } => write!(f, "{dst}: {ty} = shr {lhs}, {rhs}"),

        // Comparison & logic
        Inst::Cmp { dst, op, lhs, rhs } => write!(f, "{dst}: bool = cmp.{op} {lhs}, {rhs}"),
        Inst::Not { dst, operand } => write!(f, "{dst}: bool = not {operand}"),

        // Conversions
        Inst::IntCast { dst, value, to } => write!(f, "{dst}: {to} = int_cast {value}"),
        Inst::FloatCast { dst, value, to } => write!(f, "{dst}: {to} = float_cast {value}"),
        Inst::IntToFloat { dst, value, to } => write!(f, "{dst}: {to} = int_to_float {value}"),
        Inst::FloatToInt { dst, value, to } => write!(f, "{dst}: {to} = float_to_int {value}"),
        Inst::PtrCast { dst, value } => write!(f, "{dst}: ptr = ptr_cast {value}"),
        Inst::Bitcast { dst, value, to } => write!(f, "{dst}: {to} = bitcast {value}"),

        // Memory
        Inst::Load { dst, ptr, ty } => write!(f, "{dst}: {ty} = load {ptr}"),
        Inst::Store { ptr, value } => write!(f, "store {ptr}, {value}"),
        Inst::FieldPtr { dst, base, struct_id, field } => {
            write!(f, "{dst}: ptr = field_ptr {base}, {struct_id}.{field}")
        }
        Inst::ElemPtr { dst, base, index, elem_size } => {
            write!(f, "{dst}: ptr = elem_ptr {base}, {index}, size={elem_size}")
        }
        Inst::Memset { ptr, byte, size } => write!(f, "memset {ptr}, {byte}, {size}"),
        Inst::Memcpy { dst_ptr, src_ptr, size } => {
            write!(f, "memcpy {dst_ptr}, {src_ptr}, {size}")
        }

        // Calls
        Inst::Call { dst, func, args } => {
            if let Some(d) = dst {
                write!(f, "{d} = ")?;
            }
            write!(f, "call {func}(")?;
            write_value_list(f, args)?;
            write!(f, ")")
        }
        Inst::CallExtern { dst, name, args, .. } => {
            if let Some(d) = dst {
                write!(f, "{d} = ")?;
            }
            write!(f, "call_extern @{name}(")?;
            write_value_list(f, args)?;
            write!(f, ")")
        }
        Inst::CallRuntime { dst, callee, args, .. } => {
            if let Some(d) = dst {
                write!(f, "{d} = ")?;
            }
            write!(f, "call_runtime {:?}(", callee)?;
            write_value_list(f, args)?;
            write!(f, ")")
        }
        Inst::CollectionCtor { dst, kind, elem_or_key, val, args, with_capacity, str_keyed, .. } => {
            write!(f, "{dst} = collection_ctor {:?}<{:?}", kind, elem_or_key)?;
            if let Some(v) = val {
                write!(f, ", {:?}", v)?;
            }
            write!(f, ">(")?;
            write_value_list(f, args)?;
            write!(f, ")")?;
            if *with_capacity {
                write!(f, " with_capacity")?;
            }
            if *str_keyed {
                write!(f, " str_keyed")?;
            }
            Ok(())
        }
        Inst::CallPtr { dst, callee, args, ret_ty: _ } => {
            if let Some(d) = dst {
                write!(f, "{d} = ")?;
            }
            write!(f, "call_ptr {callee}(")?;
            write_value_list(f, args)?;
            write!(f, ")")
        }
        Inst::CallByRef { dst, fref, args, ret_ty: _ } => {
            if let Some(d) = dst {
                write!(f, "{d} = ")?;
            }
            write!(f, "call_by_ref {fref}(")?;
            write_value_list(f, args)?;
            write!(f, ")")
        }
        Inst::CallClosure { dst, kind, closure, args, .. } => {
            if let Some(d) = dst {
                write!(f, "{d} = ")?;
            }
            let k = match kind {
                ClosureDispatchKind::CallableParam => "callable",
                ClosureDispatchKind::EscapedClosure => "closure",
            };
            write!(f, "call_{k} {closure}(")?;
            write_value_list(f, args)?;
            write!(f, ")")
        }

        // Drop guards
        Inst::DropGuardOpen { kind, value } => match kind {
            DropGuardKind::Bool => write!(f, "drop_guard_open.bool {value}"),
            DropGuardKind::NonZero { size } => write!(f, "drop_guard_open.nonzero {value}, size={size}"),
        },
        Inst::DropGuardClose => write!(f, "drop_guard_close"),

        // Runtime checks
        Inst::BoundsCheck { index, len } => write!(f, "bounds_check {index}, {len}"),
        Inst::DivCheck { divisor } => write!(f, "div_check {divisor}"),
        Inst::Trap { msg } => write!(f, "trap {msg:?}"),

        // Printf
        Inst::Printf { fmt: format, args } => {
            write!(f, "printf {format:?}, [")?;
            write_value_list(f, args)?;
            write!(f, "]")
        }
        Inst::Fprintf { fd, fmt: format, args } => {
            write!(f, "fprintf {fd}, {format:?}, [")?;
            write_value_list(f, args)?;
            write!(f, "]")
        }

        Inst::ClosurePack { slot, env_ptr, call_func, needs_adapter } => {
            let adapt = if *needs_adapter { ", adapt" } else { "" };
            write!(f, "closure_pack {slot}, env={env_ptr}, call={call_func}{adapt}")
        }
        Inst::MoveSlot { slot } => write!(f, "move_slot {slot}"),
        Inst::SetCollectionBridge { collection, is_set, key_struct } => {
            let kind = if *is_set { "set" } else { "dict" };
            write!(f, "set_collection_bridge {collection}, kind={kind}, key={key_struct}")
        }
        Inst::Nop => write!(f, "nop"),
        Inst::InlineC { dst, code } => {
            if let Some(d) = dst {
                write!(f, "{d} = inline_c \"{}\"", code.replace('"', "\\\""))
            } else {
                write!(f, "inline_c \"{}\"", code.replace('"', "\\\""))
            }
        }
    }
}

fn write_term(f: &mut fmt::Formatter<'_>, term: &Term) -> fmt::Result {
    match term {
        Term::Ret(v) => write!(f, "ret {v}"),
        Term::RetVoid => write!(f, "ret void"),
        Term::Jump(target, args) => {
            write!(f, "jmp {target}")?;
            if !args.is_empty() {
                write!(f, "(")?;
                write_value_list(f, args)?;
                write!(f, ")")?;
            }
            Ok(())
        }
        Term::Branch {
            cond,
            then_block,
            then_args,
            else_block,
            else_args,
        } => {
            write!(f, "br {cond}, {then_block}")?;
            if !then_args.is_empty() {
                write!(f, "(")?;
                write_value_list(f, then_args)?;
                write!(f, ")")?;
            }
            write!(f, ", {else_block}")?;
            if !else_args.is_empty() {
                write!(f, "(")?;
                write_value_list(f, else_args)?;
                write!(f, ")")?;
            }
            Ok(())
        }
        Term::Switch {
            value,
            cases,
            default,
            default_args,
        } => {
            write!(f, "switch {value}")?;
            for (val, block, args) in cases {
                write!(f, ", {val} => {block}")?;
                if !args.is_empty() {
                    write!(f, "(")?;
                    write_value_list(f, args)?;
                    write!(f, ")")?;
                }
            }
            write!(f, ", default => {default}")?;
            if !default_args.is_empty() {
                write!(f, "(")?;
                write_value_list(f, default_args)?;
                write!(f, ")")?;
            }
            Ok(())
        }
        Term::Unreachable => write!(f, "unreachable"),
    }
}

fn write_value_list(f: &mut fmt::Formatter<'_>, values: &[ValueId]) -> fmt::Result {
    for (i, v) in values.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write!(f, "{v}")?;
    }
    Ok(())
}

/// Format an LIR module as a string.
pub fn dump_module(module: &LirModule) -> String {
    format!("{}", DisplayModule(module))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dump_minimal_module() {
        let mut module = LirModule::new();
        module.source_filename = Some("test.gg".into());

        let str_id = module.add_struct(StructDef {
            name: "GorgetString".into(),
            fields: vec![("data".into(), LirType::Ptr), ("len".into(), LirType::I64)],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false, closure_call_fn: None,
                      });

        module.add_extern(LirExtern {
            name: "puts".into(),
            params: vec![LirType::Ptr],
            return_type: LirType::I32,
            is_variadic: false,
            param_abis: vec![],
            return_abi: Default::default(),
            combinator_result_struct_id: None,
        });

        let mut func = LirFunction::new("main".into(), vec![], LirType::I32);
        let _slot = func.add_slot(LirType::Struct(str_id), Some("msg".into()));
        let bb0 = func.add_block();

        let v0 = func.next_value();
        func.block_mut(bb0).insts.push(Inst::IConst {
            dst: v0,
            ty: LirType::I32,
            value: 0,
        });
        func.block_mut(bb0).terminator = Term::Ret(v0);

        module.add_function(func);

        let output = dump_module(&module);
        assert!(output.contains("; source: test.gg"));
        assert!(output.contains("struct.0 = type GorgetString"));
        assert!(output.contains("data: ptr"));
        assert!(output.contains("declare i32 @puts(ptr)"));
        assert!(output.contains("fn @main() -> i32"));
        assert!(output.contains("s0: struct.0  ; msg"));
        assert!(output.contains("v0: i32 = iconst 0"));
        assert!(output.contains("ret v0"));
    }

    #[test]
    fn dump_branch_and_args() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("test".into(), vec![], LirType::I64);

        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();
        let bb_merge = func.add_block();

        let v_cond = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();
        let v_param = func.next_value();

        func.block_mut(bb0).insts.push(Inst::BoolConst {
            dst: v_cond,
            value: true,
        });
        func.block_mut(bb0).terminator = Term::Branch {
            cond: v_cond,
            then_block: bb1,
            then_args: vec![],
            else_block: bb2,
            else_args: vec![],
        };

        func.block_mut(bb1).insts.push(Inst::IConst {
            dst: v1,
            ty: LirType::I64,
            value: 10,
        });
        func.block_mut(bb1).terminator = Term::Jump(bb_merge, vec![v1]);

        func.block_mut(bb2).insts.push(Inst::IConst {
            dst: v2,
            ty: LirType::I64,
            value: 20,
        });
        func.block_mut(bb2).terminator = Term::Jump(bb_merge, vec![v2]);

        func.block_mut(bb_merge).params.push((v_param, LirType::I64));
        func.block_mut(bb_merge).terminator = Term::Ret(v_param);

        module.add_function(func);

        let output = dump_module(&module);
        assert!(output.contains("br v0, bb1, bb2"));
        assert!(output.contains("jmp bb3(v1)"));
        assert!(output.contains("jmp bb3(v2)"));
        assert!(output.contains("bb3(v3: i64):"));
        assert!(output.contains("ret v3"));
    }
}
