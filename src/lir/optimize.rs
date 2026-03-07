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
}

/// Run all optimization passes on an LIR module.
pub fn optimize_module(module: &mut LirModule) -> OptStats {
    let mut stats = OptStats::default();
    stats.dead_functions_eliminated = eliminate_dead_functions(module);
    stats.dead_globals_eliminated = eliminate_dead_globals(module);
    for func in &mut module.functions {
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

// ── Copy Propagation ────────────────────────────────────────────────────────

/// Propagate trivial copies (SlotLoad → SlotStore chains where the slot is
/// only written once). Returns count of copies propagated.
pub fn propagate_copies(func: &mut LirFunction) -> usize {
    // In post-SSA form, each value is defined exactly once.
    // Find values that are simple copies of other values:
    // - SlotLoad that loads from a slot that was stored exactly once
    // For now, implement the simpler "identity" propagation:
    // if v1 = v0 (via load-of-store), replace all uses of v1 with v0.

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
}
