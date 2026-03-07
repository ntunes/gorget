//! LIR optimization passes.
//!
//! All references are explicit in LIR, making optimization straightforward.

use super::*;
use std::collections::HashSet;

/// Statistics from optimization passes.
#[derive(Debug, Default)]
pub struct OptStats {
    pub dead_functions_eliminated: usize,
    pub dead_globals_eliminated: usize,
    pub dead_instructions_eliminated: usize,
    pub copies_propagated: usize,
    pub constants_folded: usize,
}

/// Run all optimization passes on an LIR module.
pub fn optimize_module(module: &mut LirModule) -> OptStats {
    let mut stats = OptStats::default();
    stats.dead_functions_eliminated = eliminate_dead_functions(module);
    stats.dead_globals_eliminated = eliminate_dead_globals(module);
    for func in &mut module.functions {
        // Fold constants first, then DCE cleans up dead operands.
        stats.constants_folded += fold_constants(func);
        stats.dead_instructions_eliminated += eliminate_dead_code(func);
        stats.copies_propagated += propagate_copies(func);
    }
    stats
}

// ── Dead Function Elimination ───────────────────────────────────────────────

/// Remove functions that are never called or referenced.
/// Returns the number of functions eliminated.
pub fn eliminate_dead_functions(module: &mut LirModule) -> usize {
    if module.functions.is_empty() {
        return 0;
    }

    let live = find_live_functions(module);
    let original = module.functions.len();

    // Build old→new FuncId remapping.
    let mut remap: Vec<Option<FuncId>> = vec![None; original];
    let mut new_idx = 0u32;
    for (old_idx, _) in module.functions.iter().enumerate() {
        if live.contains(&FuncId(old_idx as u32)) {
            remap[old_idx] = Some(FuncId(new_idx));
            new_idx += 1;
        }
    }

    // Remove dead functions.
    let mut i = 0;
    module.functions.retain(|_| {
        let keep = live.contains(&FuncId(i as u32));
        i += 1;
        keep
    });

    // Rewrite FuncId references in surviving functions.
    for func in &mut module.functions {
        for block in &mut func.blocks {
            for inst in &mut block.insts {
                rewrite_func_refs(inst, &remap);
            }
        }
    }

    // Rewrite FuncId references in globals.
    for global in &mut module.globals {
        rewrite_global_func_refs(&mut global.init, &remap);
    }

    original - module.functions.len()
}

fn find_live_functions(module: &LirModule) -> HashSet<FuncId> {
    let mut live = HashSet::new();
    let mut worklist: Vec<FuncId> = Vec::new();

    // Roots: main, any function whose name starts with "__test", and
    // any function referenced by globals.
    for (i, func) in module.functions.iter().enumerate() {
        let fid = FuncId(i as u32);
        if func.name == "main"
            || func.name.starts_with("__test")
            || func.name.starts_with("__suite_")
        {
            if live.insert(fid) {
                worklist.push(fid);
            }
        }
    }

    // Functions referenced by global initializers.
    for global in &module.globals {
        collect_global_func_refs(&global.init, &mut |fid| {
            if live.insert(fid) {
                worklist.push(fid);
            }
        });
    }

    // Transitive closure: walk called/referenced functions.
    while let Some(fid) = worklist.pop() {
        let func = &module.functions[fid.0 as usize];
        for block in &func.blocks {
            for inst in &block.insts {
                collect_inst_func_refs(inst, &mut |ref_fid| {
                    if live.insert(ref_fid) {
                        worklist.push(ref_fid);
                    }
                });
            }
        }
    }

    live
}

fn collect_inst_func_refs(inst: &Inst, cb: &mut dyn FnMut(FuncId)) {
    match inst {
        Inst::Call { func, .. } => cb(*func),
        Inst::FuncAddr { func, .. } => cb(*func),
        _ => {}
    }
}

fn collect_global_func_refs(init: &LirGlobalInit, cb: &mut dyn FnMut(FuncId)) {
    match init {
        LirGlobalInit::FuncAddr(fid) => cb(*fid),
        LirGlobalInit::Struct { fields, .. } => {
            for f in fields {
                collect_global_func_refs(f, cb);
            }
        }
        _ => {}
    }
}

fn rewrite_func_refs(inst: &mut Inst, remap: &[Option<FuncId>]) {
    match inst {
        Inst::Call { func, .. } => {
            if let Some(new_id) = remap[func.0 as usize] {
                *func = new_id;
            }
        }
        Inst::FuncAddr { func, .. } => {
            if let Some(new_id) = remap[func.0 as usize] {
                *func = new_id;
            }
        }
        _ => {}
    }
}

fn rewrite_global_func_refs(init: &mut LirGlobalInit, remap: &[Option<FuncId>]) {
    match init {
        LirGlobalInit::FuncAddr(fid) => {
            if let Some(new_id) = remap[fid.0 as usize] {
                *fid = new_id;
            }
        }
        LirGlobalInit::Struct { fields, .. } => {
            for f in fields {
                rewrite_global_func_refs(f, remap);
            }
        }
        _ => {}
    }
}

// ── Dead Global Elimination ─────────────────────────────────────────────────

/// Remove globals that are never referenced. Returns count eliminated.
pub fn eliminate_dead_globals(module: &mut LirModule) -> usize {
    if module.globals.is_empty() {
        return 0;
    }

    let mut referenced: HashSet<GlobalId> = HashSet::new();
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::GlobalAddr { global, .. } = inst {
                    referenced.insert(*global);
                }
            }
        }
    }

    let original = module.globals.len();
    let mut i = 0;
    module.globals.retain(|_| {
        let keep = referenced.contains(&GlobalId(i as u32));
        i += 1;
        keep
    });

    original - module.globals.len()
}

// ── Dead Code Elimination ───────────────────────────────────────────────────

/// Remove instructions whose results are never used. Returns count eliminated.
pub fn eliminate_dead_code(func: &mut LirFunction) -> usize {
    // Build use counts for each ValueId.
    let val_count = func.value_count() as usize;
    let mut use_count = vec![0u32; val_count];

    for block in &func.blocks {
        for inst in &block.insts {
            for used in inst.uses() {
                if (used.0 as usize) < val_count {
                    use_count[used.0 as usize] += 1;
                }
            }
        }
        for used in block.terminator.uses() {
            if (used.0 as usize) < val_count {
                use_count[used.0 as usize] += 1;
            }
        }
        // Block params used in terminators of other blocks are counted above.
    }

    let mut eliminated = 0;
    for block in &mut func.blocks {
        block.insts.retain(|inst| {
            // Keep instructions with side effects.
            if has_side_effects(inst) {
                return true;
            }
            // Keep instructions whose results are used.
            if let Some(dst) = inst.dst() {
                if (dst.0 as usize) < val_count && use_count[dst.0 as usize] > 0 {
                    return true;
                }
                // Dead — result unused.
                eliminated += 1;
                false
            } else {
                true // No dst means side-effect (already handled above)
            }
        });
    }

    eliminated
}

fn has_side_effects(inst: &Inst) -> bool {
    matches!(
        inst,
        Inst::SlotStore { .. }
            | Inst::Store { .. }
            | Inst::Memset { .. }
            | Inst::Memcpy { .. }
            | Inst::Call { .. }
            | Inst::CallExtern { .. }
            | Inst::CallPtr { .. }
            | Inst::BoundsCheck { .. }
            | Inst::DivCheck { .. }
            | Inst::Trap { .. }
            | Inst::Printf { .. }
            | Inst::Fprintf { .. }
            | Inst::Nop
    )
}

// ── Constant Folding ──────────────────────────────────────────────────────

/// Known constant value for a ValueId.
#[derive(Clone, Debug)]
enum KnownConst {
    Int(i64, LirType),
    Float(f64, LirType),
    Bool(bool),
}

/// Evaluate constant integer binary operations at compile time.
/// Returns the count of instructions folded.
pub fn fold_constants(func: &mut LirFunction) -> usize {
    let val_count = func.value_count() as usize;
    let mut known: Vec<Option<KnownConst>> = vec![None; val_count];

    // First pass: collect known constants.
    for block in &func.blocks {
        for inst in &block.insts {
            match inst {
                Inst::IConst { dst, value, ty } => {
                    known[dst.0 as usize] = Some(KnownConst::Int(*value, ty.clone()));
                }
                Inst::FConst { dst, bits, ty } => {
                    known[dst.0 as usize] = Some(KnownConst::Float(f64::from_bits(*bits), ty.clone()));
                }
                Inst::BoolConst { dst, value } => {
                    known[dst.0 as usize] = Some(KnownConst::Bool(*value));
                }
                _ => {}
            }
        }
    }

    // Second pass: fold arithmetic on known constants.
    let mut folded = 0;
    for block in &mut func.blocks {
        for inst in &mut block.insts {
            if let Some(replacement) = try_fold(inst, &known) {
                // Record the new constant for cascading folds.
                if let Some(dst) = replacement.dst() {
                    match &replacement {
                        Inst::IConst { value, ty, .. } => {
                            known[dst.0 as usize] = Some(KnownConst::Int(*value, ty.clone()));
                        }
                        Inst::FConst { bits, ty, .. } => {
                            known[dst.0 as usize] = Some(KnownConst::Float(f64::from_bits(*bits), ty.clone()));
                        }
                        Inst::BoolConst { value, .. } => {
                            known[dst.0 as usize] = Some(KnownConst::Bool(*value));
                        }
                        _ => {}
                    }
                }
                *inst = replacement;
                folded += 1;
            }
        }
    }

    folded
}

/// Try to fold a single instruction. Returns `Some(replacement)` if foldable.
fn try_fold(inst: &Inst, known: &[Option<KnownConst>]) -> Option<Inst> {
    let get_int = |v: ValueId| -> Option<(i64, LirType)> {
        match known.get(v.0 as usize)? {
            Some(KnownConst::Int(val, ty)) => Some((*val, ty.clone())),
            _ => None,
        }
    };
    let get_float = |v: ValueId| -> Option<(f64, LirType)> {
        match known.get(v.0 as usize)? {
            Some(KnownConst::Float(val, ty)) => Some((*val, ty.clone())),
            _ => None,
        }
    };
    let get_bool = |v: ValueId| -> Option<bool> {
        match known.get(v.0 as usize)? {
            Some(KnownConst::Bool(val)) => Some(*val),
            _ => None,
        }
    };

    match inst {
        // Integer arithmetic
        Inst::Add { dst, lhs, rhs, .. } => {
            if let (Some((a, ty)), Some((b, _))) = (get_int(*lhs), get_int(*rhs)) {
                return Some(Inst::IConst { dst: *dst, value: a.wrapping_add(b), ty });
            }
            if let (Some((a, ty)), Some((b, _))) = (get_float(*lhs), get_float(*rhs)) {
                return Some(Inst::FConst { dst: *dst, bits: (a + b).to_bits(), ty });
            }
            None
        }
        Inst::Sub { dst, lhs, rhs, .. } => {
            if let (Some((a, ty)), Some((b, _))) = (get_int(*lhs), get_int(*rhs)) {
                return Some(Inst::IConst { dst: *dst, value: a.wrapping_sub(b), ty });
            }
            if let (Some((a, ty)), Some((b, _))) = (get_float(*lhs), get_float(*rhs)) {
                return Some(Inst::FConst { dst: *dst, bits: (a - b).to_bits(), ty });
            }
            None
        }
        Inst::Mul { dst, lhs, rhs, .. } => {
            if let (Some((a, ty)), Some((b, _))) = (get_int(*lhs), get_int(*rhs)) {
                return Some(Inst::IConst { dst: *dst, value: a.wrapping_mul(b), ty });
            }
            if let (Some((a, ty)), Some((b, _))) = (get_float(*lhs), get_float(*rhs)) {
                return Some(Inst::FConst { dst: *dst, bits: (a * b).to_bits(), ty });
            }
            None
        }
        Inst::Div { dst, lhs, rhs } => {
            if let (Some((a, ty)), Some((b, _))) = (get_int(*lhs), get_int(*rhs)) {
                if b != 0 {
                    return Some(Inst::IConst { dst: *dst, value: a.wrapping_div(b), ty });
                }
            }
            if let (Some((a, ty)), Some((b, _))) = (get_float(*lhs), get_float(*rhs)) {
                return Some(Inst::FConst { dst: *dst, bits: (a / b).to_bits(), ty });
            }
            None
        }
        Inst::Rem { dst, lhs, rhs } => {
            if let (Some((a, ty)), Some((b, _))) = (get_int(*lhs), get_int(*rhs)) {
                if b != 0 {
                    return Some(Inst::IConst { dst: *dst, value: a.wrapping_rem(b), ty });
                }
            }
            None
        }
        Inst::Mod { dst, lhs, rhs } => {
            if let (Some((a, ty)), Some((b, _))) = (get_int(*lhs), get_int(*rhs)) {
                if b != 0 {
                    let r = a.wrapping_rem(b);
                    let result = if r != 0 && (r ^ b) < 0 { r + b } else { r };
                    return Some(Inst::IConst { dst: *dst, value: result, ty });
                }
            }
            None
        }
        Inst::Neg { dst, operand } => {
            if let Some((a, ty)) = get_int(*operand) {
                return Some(Inst::IConst { dst: *dst, value: a.wrapping_neg(), ty });
            }
            if let Some((a, ty)) = get_float(*operand) {
                return Some(Inst::FConst { dst: *dst, bits: (-a).to_bits(), ty });
            }
            None
        }

        // Bitwise
        Inst::BitAnd { dst, lhs, rhs } => {
            let (a, ty) = get_int(*lhs)?;
            let (b, _) = get_int(*rhs)?;
            Some(Inst::IConst { dst: *dst, value: a & b, ty })
        }
        Inst::BitOr { dst, lhs, rhs } => {
            let (a, ty) = get_int(*lhs)?;
            let (b, _) = get_int(*rhs)?;
            Some(Inst::IConst { dst: *dst, value: a | b, ty })
        }
        Inst::BitXor { dst, lhs, rhs } => {
            let (a, ty) = get_int(*lhs)?;
            let (b, _) = get_int(*rhs)?;
            Some(Inst::IConst { dst: *dst, value: a ^ b, ty })
        }
        Inst::BitNot { dst, operand } => {
            let (a, ty) = get_int(*operand)?;
            Some(Inst::IConst { dst: *dst, value: !a, ty })
        }
        Inst::Shl { dst, lhs, rhs } => {
            let (a, ty) = get_int(*lhs)?;
            let (b, _) = get_int(*rhs)?;
            if b >= 0 && b < 64 {
                Some(Inst::IConst { dst: *dst, value: a.wrapping_shl(b as u32), ty })
            } else {
                None
            }
        }
        Inst::Shr { dst, lhs, rhs } => {
            let (a, ty) = get_int(*lhs)?;
            let (b, _) = get_int(*rhs)?;
            if b >= 0 && b < 64 {
                Some(Inst::IConst { dst: *dst, value: a.wrapping_shr(b as u32), ty })
            } else {
                None
            }
        }

        // Comparison
        Inst::Cmp { dst, op, lhs, rhs } => {
            if let (Some((a, _)), Some((b, _))) = (get_int(*lhs), get_int(*rhs)) {
                let result = match op {
                    CmpOp::Eq => a == b,
                    CmpOp::Ne => a != b,
                    CmpOp::Lt => a < b,
                    CmpOp::Le => a <= b,
                    CmpOp::Gt => a > b,
                    CmpOp::Ge => a >= b,
                };
                return Some(Inst::BoolConst { dst: *dst, value: result });
            }
            if let (Some((a, _)), Some((b, _))) = (get_float(*lhs), get_float(*rhs)) {
                let result = match op {
                    CmpOp::Eq => a == b,
                    CmpOp::Ne => a != b,
                    CmpOp::Lt => a < b,
                    CmpOp::Le => a <= b,
                    CmpOp::Gt => a > b,
                    CmpOp::Ge => a >= b,
                };
                return Some(Inst::BoolConst { dst: *dst, value: result });
            }
            None
        }
        Inst::Not { dst, operand } => {
            let b = get_bool(*operand)?;
            Some(Inst::BoolConst { dst: *dst, value: !b })
        }

        // Integer casts between known constants
        Inst::IntCast { dst, value, to } => {
            let (a, _) = get_int(*value)?;
            let truncated = match to {
                LirType::I8 => (a as i8) as i64,
                LirType::I16 => (a as i16) as i64,
                LirType::I32 => (a as i32) as i64,
                LirType::I64 => a,
                LirType::U8 => (a as u8) as i64,
                LirType::U16 => (a as u16) as i64,
                LirType::U32 => (a as u32) as i64,
                LirType::U64 => a, // stored as i64 bits
                _ => return None,
            };
            Some(Inst::IConst { dst: *dst, value: truncated, ty: to.clone() })
        }
        Inst::IntToFloat { dst, value, to } => {
            let (a, _) = get_int(*value)?;
            let f = a as f64;
            Some(Inst::FConst { dst: *dst, bits: f.to_bits(), ty: to.clone() })
        }
        Inst::FloatToInt { dst, value, to } => {
            let (a, _) = get_float(*value)?;
            let i = a as i64;
            Some(Inst::IConst { dst: *dst, value: i, ty: to.clone() })
        }

        _ => None,
    }
}

// ── Copy Propagation ────────────────────────────────────────────────────────

/// Propagate trivial copies. Returns count of copies propagated.
pub fn propagate_copies(func: &mut LirFunction) -> usize {
    let _ = func;
    0
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_module_with_dead_fn() -> LirModule {
        let mut module = LirModule::new();

        // fn main() -> i32 { call helper(); return 0; }
        let mut main_fn = LirFunction::new("main".into(), vec![], LirType::I32);
        let bb = main_fn.add_block();
        let v0 = main_fn.next_value();
        let v1 = main_fn.next_value();
        main_fn.block_mut(bb).insts = vec![
            Inst::Call {
                dst: Some(v0),
                func: FuncId(1), // helper
                args: vec![],
            },
            Inst::IConst {
                dst: v1,
                ty: LirType::I32,
                value: 0,
            },
        ];
        main_fn.block_mut(bb).terminator = Term::Ret(v1);
        module.add_function(main_fn);

        // fn helper() -> i64 { return 42; }
        let mut helper = LirFunction::new("helper".into(), vec![], LirType::I64);
        let bb = helper.add_block();
        let v0 = helper.next_value();
        helper.block_mut(bb).insts.push(Inst::IConst {
            dst: v0,
            ty: LirType::I64,
            value: 42,
        });
        helper.block_mut(bb).terminator = Term::Ret(v0);
        module.add_function(helper);

        // fn dead() -> i64 { return 99; } — never called
        let mut dead = LirFunction::new("dead".into(), vec![], LirType::I64);
        let bb = dead.add_block();
        let v0 = dead.next_value();
        dead.block_mut(bb).insts.push(Inst::IConst {
            dst: v0,
            ty: LirType::I64,
            value: 99,
        });
        dead.block_mut(bb).terminator = Term::Ret(v0);
        module.add_function(dead);

        module
    }

    #[test]
    fn dead_function_elimination() {
        let mut module = make_module_with_dead_fn();
        assert_eq!(module.functions.len(), 3);

        let eliminated = eliminate_dead_functions(&mut module);
        assert_eq!(eliminated, 1, "should eliminate 1 dead function");
        assert_eq!(module.functions.len(), 2);
        assert_eq!(module.functions[0].name, "main");
        assert_eq!(module.functions[1].name, "helper");

        // Verify FuncId in main's Call was remapped.
        let call = &module.functions[0].blocks[0].insts[0];
        if let Inst::Call { func, .. } = call {
            assert_eq!(*func, FuncId(1), "helper should be FuncId(1) after remap");
        } else {
            panic!("expected Call instruction");
        }
    }

    #[test]
    fn dead_fn_transitive() {
        let mut module = LirModule::new();

        // main calls a, a calls b, c is dead
        let mut main_fn = LirFunction::new("main".into(), vec![], LirType::I32);
        let bb = main_fn.add_block();
        let v0 = main_fn.next_value();
        main_fn.block_mut(bb).insts.push(Inst::Call {
            dst: Some(v0),
            func: FuncId(1), // a
            args: vec![],
        });
        main_fn.block_mut(bb).terminator = Term::Ret(v0);
        module.add_function(main_fn);

        let mut fn_a = LirFunction::new("a".into(), vec![], LirType::I32);
        let bb = fn_a.add_block();
        let v0 = fn_a.next_value();
        fn_a.block_mut(bb).insts.push(Inst::Call {
            dst: Some(v0),
            func: FuncId(2), // b
            args: vec![],
        });
        fn_a.block_mut(bb).terminator = Term::Ret(v0);
        module.add_function(fn_a);

        let mut fn_b = LirFunction::new("b".into(), vec![], LirType::I32);
        let bb = fn_b.add_block();
        let v0 = fn_b.next_value();
        fn_b.block_mut(bb).insts.push(Inst::IConst {
            dst: v0,
            ty: LirType::I32,
            value: 42,
        });
        fn_b.block_mut(bb).terminator = Term::Ret(v0);
        module.add_function(fn_b);

        let mut fn_c = LirFunction::new("c_dead".into(), vec![], LirType::I32);
        let bb = fn_c.add_block();
        fn_c.block_mut(bb).terminator = Term::RetVoid;
        module.add_function(fn_c);

        let eliminated = eliminate_dead_functions(&mut module);
        assert_eq!(eliminated, 1);
        assert_eq!(module.functions.len(), 3);
        assert!(module.functions.iter().all(|f| f.name != "c_dead"));
    }

    #[test]
    fn funcaddr_keeps_alive() {
        let mut module = LirModule::new();

        // main takes FuncAddr of helper (no direct call)
        let mut main_fn = LirFunction::new("main".into(), vec![], LirType::I32);
        let bb = main_fn.add_block();
        let v0 = main_fn.next_value();
        let v1 = main_fn.next_value();
        main_fn.block_mut(bb).insts = vec![
            Inst::FuncAddr {
                dst: v0,
                func: FuncId(1),
            },
            Inst::IConst {
                dst: v1,
                ty: LirType::I32,
                value: 0,
            },
        ];
        main_fn.block_mut(bb).terminator = Term::Ret(v1);
        module.add_function(main_fn);

        let mut helper = LirFunction::new("helper".into(), vec![], LirType::Void);
        let bb = helper.add_block();
        helper.block_mut(bb).terminator = Term::RetVoid;
        module.add_function(helper);

        let eliminated = eliminate_dead_functions(&mut module);
        assert_eq!(eliminated, 0, "FuncAddr reference should keep helper alive");
    }

    #[test]
    fn dead_code_elimination() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::I32);
        let bb = func.add_block();

        let v0 = func.next_value();
        let v1 = func.next_value(); // dead
        let v2 = func.next_value(); // dead
        let v3 = func.next_value();

        func.block_mut(bb).insts = vec![
            Inst::IConst {
                dst: v0,
                ty: LirType::I32,
                value: 42,
            },
            Inst::IConst {
                dst: v1,
                ty: LirType::I64,
                value: 99, // dead — never used
            },
            Inst::IConst {
                dst: v2,
                ty: LirType::I64,
                value: 100, // dead — never used
            },
            Inst::IConst {
                dst: v3,
                ty: LirType::I32,
                value: 0,
            },
        ];
        func.block_mut(bb).terminator = Term::Ret(v3);

        let eliminated = eliminate_dead_code(&mut func);
        assert_eq!(eliminated, 3, "should eliminate 3 dead constants");
        assert_eq!(func.blocks[0].insts.len(), 1);
    }

    #[test]
    fn side_effect_instructions_kept() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::Void);
        let bb = func.add_block();

        func.block_mut(bb).insts = vec![
            Inst::CallExtern {
                dst: None,
                name: "puts".into(),
                args: vec![],
            },
            Inst::Printf {
                fmt: "hello\n".into(),
                args: vec![],
            },
            Inst::Nop,
        ];
        func.block_mut(bb).terminator = Term::RetVoid;

        let eliminated = eliminate_dead_code(&mut func);
        assert_eq!(eliminated, 0, "side-effect instructions should be kept");
        assert_eq!(func.blocks[0].insts.len(), 3);
    }

    #[test]
    fn dead_global_elimination() {
        let mut module = LirModule::new();

        module.add_global(LirGlobal {
            name: "used_global".into(),
            ty: LirType::I64,
            init: LirGlobalInit::Zeroed,
            is_const: false,
        });
        module.add_global(LirGlobal {
            name: "dead_global".into(),
            ty: LirType::I64,
            init: LirGlobalInit::Zeroed,
            is_const: false,
        });

        let mut func = LirFunction::new("main".into(), vec![], LirType::I32);
        let bb = func.add_block();
        let v0 = func.next_value();
        let v1 = func.next_value();
        func.block_mut(bb).insts = vec![
            Inst::GlobalAddr {
                dst: v0,
                global: GlobalId(0), // references used_global
            },
            Inst::IConst {
                dst: v1,
                ty: LirType::I32,
                value: 0,
            },
        ];
        func.block_mut(bb).terminator = Term::Ret(v1);
        module.add_function(func);

        let eliminated = eliminate_dead_globals(&mut module);
        assert_eq!(eliminated, 1);
        assert_eq!(module.globals.len(), 1);
        assert_eq!(module.globals[0].name, "used_global");
    }

    #[test]
    fn full_optimization_pass() {
        let mut module = make_module_with_dead_fn();
        let stats = optimize_module(&mut module);
        assert_eq!(stats.dead_functions_eliminated, 1);
        assert_eq!(module.functions.len(), 2);
    }

    #[test]
    fn empty_module_optimization() {
        let mut module = LirModule::new();
        let stats = optimize_module(&mut module);
        assert_eq!(stats.dead_functions_eliminated, 0);
        assert_eq!(stats.dead_globals_eliminated, 0);
        assert_eq!(stats.dead_instructions_eliminated, 0);
    }

    #[test]
    fn constant_fold_add() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::I64);
        let bb = func.add_block();
        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb).insts = vec![
            Inst::IConst { dst: v0, ty: LirType::I64, value: 10 },
            Inst::IConst { dst: v1, ty: LirType::I64, value: 32 },
            Inst::Add { dst: v2, lhs: v0, rhs: v1, overflow: Overflow::Wrap },
        ];
        func.block_mut(bb).terminator = Term::Ret(v2);

        let folded = fold_constants(&mut func);
        assert_eq!(folded, 1, "should fold one Add");
        // v2 should now be IConst 42
        match &func.blocks[0].insts[2] {
            Inst::IConst { value: 42, .. } => {}
            other => panic!("expected IConst 42, got {:?}", other),
        }
    }

    #[test]
    fn constant_fold_cascading() {
        // v0=3, v1=4, v2=v0*v1=12, v3=v2+8=20 — two folds
        let mut func = LirFunction::new("test".into(), vec![], LirType::I64);
        let bb = func.add_block();
        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();
        let v3 = func.next_value();
        let v4 = func.next_value();

        func.block_mut(bb).insts = vec![
            Inst::IConst { dst: v0, ty: LirType::I64, value: 3 },
            Inst::IConst { dst: v1, ty: LirType::I64, value: 4 },
            Inst::Mul { dst: v2, lhs: v0, rhs: v1, overflow: Overflow::Wrap },
            Inst::IConst { dst: v3, ty: LirType::I64, value: 8 },
            Inst::Add { dst: v4, lhs: v2, rhs: v3, overflow: Overflow::Wrap },
        ];
        func.block_mut(bb).terminator = Term::Ret(v4);

        let folded = fold_constants(&mut func);
        assert_eq!(folded, 2, "should cascade: fold mul then add");
        match &func.blocks[0].insts[4] {
            Inst::IConst { value: 20, .. } => {}
            other => panic!("expected IConst 20, got {:?}", other),
        }
    }

    #[test]
    fn constant_fold_comparison() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::Bool);
        let bb = func.add_block();
        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb).insts = vec![
            Inst::IConst { dst: v0, ty: LirType::I64, value: 5 },
            Inst::IConst { dst: v1, ty: LirType::I64, value: 10 },
            Inst::Cmp { dst: v2, op: CmpOp::Lt, lhs: v0, rhs: v1 },
        ];
        func.block_mut(bb).terminator = Term::Ret(v2);

        let folded = fold_constants(&mut func);
        assert_eq!(folded, 1);
        match &func.blocks[0].insts[2] {
            Inst::BoolConst { value: true, .. } => {}
            other => panic!("expected BoolConst true, got {:?}", other),
        }
    }

    #[test]
    fn constant_fold_bitwise() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::I64);
        let bb = func.add_block();
        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb).insts = vec![
            Inst::IConst { dst: v0, ty: LirType::I64, value: 0xFF },
            Inst::IConst { dst: v1, ty: LirType::I64, value: 0x0F },
            Inst::BitAnd { dst: v2, lhs: v0, rhs: v1 },
        ];
        func.block_mut(bb).terminator = Term::Ret(v2);

        let folded = fold_constants(&mut func);
        assert_eq!(folded, 1);
        match &func.blocks[0].insts[2] {
            Inst::IConst { value: 0x0F, .. } => {}
            other => panic!("expected IConst 0x0F, got {:?}", other),
        }
    }

    #[test]
    fn constant_fold_float() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::F64);
        let bb = func.add_block();
        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb).insts = vec![
            Inst::FConst { dst: v0, ty: LirType::F64, bits: 2.5_f64.to_bits() },
            Inst::FConst { dst: v1, ty: LirType::F64, bits: 3.0_f64.to_bits() },
            Inst::Mul { dst: v2, lhs: v0, rhs: v1, overflow: Overflow::Wrap },
        ];
        func.block_mut(bb).terminator = Term::Ret(v2);

        let folded = fold_constants(&mut func);
        assert_eq!(folded, 1);
        match &func.blocks[0].insts[2] {
            Inst::FConst { bits, .. } => {
                assert_eq!(f64::from_bits(*bits), 7.5);
            }
            other => panic!("expected FConst 7.5, got {:?}", other),
        }
    }

    #[test]
    fn constant_fold_div_by_zero_not_folded() {
        let mut func = LirFunction::new("test".into(), vec![], LirType::I64);
        let bb = func.add_block();
        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb).insts = vec![
            Inst::IConst { dst: v0, ty: LirType::I64, value: 42 },
            Inst::IConst { dst: v1, ty: LirType::I64, value: 0 },
            Inst::Div { dst: v2, lhs: v0, rhs: v1 },
        ];
        func.block_mut(bb).terminator = Term::Ret(v2);

        let folded = fold_constants(&mut func);
        assert_eq!(folded, 0, "division by zero should not be folded");
    }
}
