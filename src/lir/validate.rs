//! LIR invariant checking.
//!
//! Validates structural properties of LIR modules:
//! - Every value use is dominated by its definition
//! - Block parameter counts match jump argument counts
//! - Block IDs are sequential and in-range
//! - No duplicate value definitions
//! - Terminators reference valid blocks

use super::*;
use std::collections::{HashMap, HashSet};

/// Validation error with context.
#[derive(Debug, Clone)]
pub struct LirError {
    pub func: String,
    pub block: Option<BlockId>,
    pub message: String,
}

impl std::fmt::Display for LirError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "in @{}", self.func)?;
        if let Some(bb) = self.block {
            write!(f, " {bb}")?;
        }
        write!(f, ": {}", self.message)
    }
}

/// Validate a full LIR module. Returns a list of errors (empty = valid).
pub fn validate_module(module: &LirModule) -> Vec<LirError> {
    let mut errors = Vec::new();

    // Validate struct references
    for func in &module.functions {
        validate_function(func, module, &mut errors);
    }

    errors
}

fn validate_function(func: &LirFunction, module: &LirModule, errors: &mut Vec<LirError>) {
    let num_blocks = func.blocks.len();
    let num_slots = func.slots.len();

    // Check block IDs are sequential
    for (i, block) in func.blocks.iter().enumerate() {
        if block.id.0 as usize != i {
            errors.push(LirError {
                func: func.name.clone(),
                block: Some(block.id),
                message: format!("block id {} at position {i}", block.id),
            });
        }
    }

    // Collect all defined values and check for duplicates
    let mut defined: HashMap<ValueId, BlockId> = HashMap::new();

    // Block params define values
    for block in &func.blocks {
        for (vid, _) in &block.params {
            if let Some(prev_block) = defined.insert(*vid, block.id) {
                errors.push(LirError {
                    func: func.name.clone(),
                    block: Some(block.id),
                    message: format!(
                        "duplicate definition of {vid} (also defined in {prev_block})"
                    ),
                });
            }
        }

        // Instructions define values
        for inst in &block.insts {
            if let Some(dst) = inst.dst() {
                if let Some(prev_block) = defined.insert(dst, block.id) {
                    errors.push(LirError {
                        func: func.name.clone(),
                        block: Some(block.id),
                        message: format!(
                            "duplicate definition of {dst} (also defined in {prev_block})"
                        ),
                    });
                }
            }
        }
    }

    // Check each block
    for block in &func.blocks {
        // Validate slot references
        for inst in &block.insts {
            check_slot_refs(inst, num_slots, func, block.id, errors);
            check_struct_refs(inst, module, func, block.id, errors);
        }

        // Validate terminator
        validate_terminator(
            &block.terminator,
            func,
            block.id,
            num_blocks,
            errors,
        );
    }
}

fn check_slot_refs(
    inst: &Inst,
    num_slots: usize,
    func: &LirFunction,
    block: BlockId,
    errors: &mut Vec<LirError>,
) {
    let slot = match inst {
        Inst::SlotStore { slot, .. }
        | Inst::SlotLoad { slot, .. }
        | Inst::SlotAddr { slot, .. } => Some(*slot),
        _ => None,
    };

    if let Some(s) = slot {
        if s.0 as usize >= num_slots {
            errors.push(LirError {
                func: func.name.clone(),
                block: Some(block),
                message: format!("slot {s} out of range (function has {num_slots} slots)"),
            });
        }
    }
}

fn check_struct_refs(
    inst: &Inst,
    module: &LirModule,
    func: &LirFunction,
    block: BlockId,
    errors: &mut Vec<LirError>,
) {
    let struct_id = match inst {
        Inst::FieldPtr { struct_id, field, .. } => {
            // Also check field index
            if let Some(def) = module.structs.get(struct_id.0 as usize) {
                if *field as usize >= def.fields.len() {
                    errors.push(LirError {
                        func: func.name.clone(),
                        block: Some(block),
                        message: format!(
                            "field index {field} out of range for {} (has {} fields)",
                            def.name,
                            def.fields.len()
                        ),
                    });
                }
            }
            Some(*struct_id)
        }
        _ => None,
    };

    if let Some(sid) = struct_id {
        if sid.0 as usize >= module.structs.len() {
            errors.push(LirError {
                func: func.name.clone(),
                block: Some(block),
                message: format!(
                    "{sid} out of range (module has {} structs)",
                    module.structs.len()
                ),
            });
        }
    }
}

fn validate_terminator(
    term: &Term,
    func: &LirFunction,
    block: BlockId,
    num_blocks: usize,
    errors: &mut Vec<LirError>,
) {
    // Check all successor blocks are valid and arg counts match
    let check_target = |target: BlockId, args: &[ValueId], errors: &mut Vec<LirError>| {
        if target.0 as usize >= num_blocks {
            errors.push(LirError {
                func: func.name.clone(),
                block: Some(block),
                message: format!("terminator references invalid {target}"),
            });
        } else {
            let target_params = func.blocks[target.0 as usize].params.len();
            if args.len() != target_params {
                errors.push(LirError {
                    func: func.name.clone(),
                    block: Some(block),
                    message: format!(
                        "jump to {target} provides {} args but block expects {target_params}",
                        args.len()
                    ),
                });
            }
        }
    };

    match term {
        Term::Ret(_) | Term::RetVoid | Term::Unreachable => {}
        Term::Jump(target, args) => check_target(*target, args, errors),
        Term::Branch {
            then_block,
            then_args,
            else_block,
            else_args,
            ..
        } => {
            check_target(*then_block, then_args, errors);
            check_target(*else_block, else_args, errors);
        }
        Term::Switch {
            cases,
            default,
            default_args,
            ..
        } => {
            for (_, target, args) in cases {
                check_target(*target, args, errors);
            }
            check_target(*default, default_args, errors);
        }
    }
}

/// Validate SSA dominance: every value use is dominated by its definition.
/// Uses Cooper-Harvey-Kennedy iterative dominator algorithm.
pub fn validate_ssa_dominance(func: &LirFunction) -> Vec<LirError> {
    let mut errors = Vec::new();
    let n = func.blocks.len();
    if n == 0 { return errors; }

    // Build predecessor lists
    let mut preds: Vec<Vec<usize>> = vec![vec![]; n];
    for block in &func.blocks {
        let idx = block.id.0 as usize;
        for succ in block.terminator.successors() {
            let s = succ.0 as usize;
            if s < n {
                preds[s].push(idx);
            }
        }
    }

    // Compute RPO (reverse postorder)
    let rpo = compute_rpo(n, &func.blocks);

    // Iterative dominator computation (Cooper-Harvey-Kennedy 2001)
    // idom[i] = immediate dominator of block i (in RPO numbering)
    let mut rpo_order: Vec<usize> = vec![usize::MAX; n]; // block_idx → RPO position
    for (pos, &block_idx) in rpo.iter().enumerate() {
        rpo_order[block_idx] = pos;
    }

    let mut idom: Vec<usize> = vec![usize::MAX; n];
    idom[rpo[0]] = rpo[0]; // entry dominates itself

    let intersect = |mut a: usize, mut b: usize, idom: &[usize], rpo_order: &[usize]| -> usize {
        while a != b {
            while rpo_order[a] > rpo_order[b] { a = idom[a]; }
            while rpo_order[b] > rpo_order[a] { b = idom[b]; }
        }
        a
    };

    let mut changed = true;
    while changed {
        changed = false;
        for &b in &rpo[1..] { // skip entry
            let mut new_idom = usize::MAX;
            for &p in &preds[b] {
                if idom[p] != usize::MAX {
                    new_idom = if new_idom == usize::MAX { p } else { intersect(new_idom, p, &idom, &rpo_order) };
                }
            }
            if new_idom != usize::MAX && idom[b] != new_idom {
                idom[b] = new_idom;
                changed = true;
            }
        }
    }

    // Build dominance set for each block: which blocks dominate block b?
    // A block d dominates b if d is on the path from entry to b via idom chain.
    let dominates = |d: usize, b: usize| -> bool {
        let mut cur = b;
        loop {
            if cur == d { return true; }
            if idom[cur] == cur || idom[cur] == usize::MAX { return false; }
            cur = idom[cur];
        }
    };

    // Collect where each value is defined
    let mut def_block: HashMap<ValueId, usize> = HashMap::new();
    for block in &func.blocks {
        let idx = block.id.0 as usize;
        for (vid, _) in &block.params {
            def_block.insert(*vid, idx);
        }
        for inst in &block.insts {
            if let Some(dst) = inst.dst() {
                def_block.insert(dst, idx);
            }
        }
    }

    // Check: every use must be in a block dominated by the definition block.
    // For uses within the same block as the definition, verify instruction ordering.
    for block in &func.blocks {
        let use_idx = block.id.0 as usize;
        // Track which values have been defined so far in this block (for intra-block ordering)
        let mut defined_before: HashSet<ValueId> = HashSet::new();
        for (vid, _) in &block.params {
            defined_before.insert(*vid);
        }

        for inst in &block.insts {
            for used in inst.uses() {
                match def_block.get(&used) {
                    None => {
                        errors.push(LirError {
                            func: func.name.clone(),
                            block: Some(block.id),
                            message: format!("use of undefined value {used}"),
                        });
                    }
                    Some(&def_b) => {
                        if def_b == use_idx {
                            // Same block: must be defined before this instruction
                            if !defined_before.contains(&used) {
                                errors.push(LirError {
                                    func: func.name.clone(),
                                    block: Some(block.id),
                                    message: format!("use of {used} before its definition in same block"),
                                });
                            }
                        } else if !dominates(def_b, use_idx) {
                            errors.push(LirError {
                                func: func.name.clone(),
                                block: Some(block.id),
                                message: format!("{used} defined in bb{def_b} which does not dominate bb{use_idx}"),
                            });
                        }
                    }
                }
            }
            if let Some(dst) = inst.dst() {
                defined_before.insert(dst);
            }
        }

        // Check terminator uses
        for used in block.terminator.uses() {
            match def_block.get(&used) {
                None => {
                    errors.push(LirError {
                        func: func.name.clone(),
                        block: Some(block.id),
                        message: format!("use of undefined value {used} in terminator"),
                    });
                }
                Some(&def_b) => {
                    if def_b == use_idx {
                        if !defined_before.contains(&used) {
                            errors.push(LirError {
                                func: func.name.clone(),
                                block: Some(block.id),
                                message: format!("use of {used} before its definition in terminator"),
                            });
                        }
                    } else if !dominates(def_b, use_idx) {
                        errors.push(LirError {
                            func: func.name.clone(),
                            block: Some(block.id),
                            message: format!("{used} defined in bb{def_b} which does not dominate bb{use_idx} (terminator)"),
                        });
                    }
                }
            }
        }
    }

    errors
}

/// Compute reverse postorder of blocks via DFS from block 0.
fn compute_rpo(n: usize, blocks: &[Block]) -> Vec<usize> {
    let mut visited = vec![false; n];
    let mut postorder = Vec::with_capacity(n);

    fn dfs(b: usize, blocks: &[Block], visited: &mut Vec<bool>, postorder: &mut Vec<usize>) {
        if b >= blocks.len() || visited[b] { return; }
        visited[b] = true;
        for succ in blocks[b].terminator.successors() {
            dfs(succ.0 as usize, blocks, visited, postorder);
        }
        postorder.push(b);
    }

    dfs(0, blocks, &mut visited, &mut postorder);
    postorder.reverse();
    postorder
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_valid_module() -> LirModule {
        let mut module = LirModule::new();
        module.add_struct(StructDef {
            name: "Point".into(),
            fields: vec![("x".into(), LirType::F64), ("y".into(), LirType::F64)],
            is_enum: false,
            computed_c_size: None,
                      });

        let mut func = LirFunction::new("main".into(), vec![], LirType::I32);
        let bb = func.add_block();
        let v0 = func.next_value();
        func.block_mut(bb).insts.push(Inst::IConst {
            dst: v0,
            ty: LirType::I32,
            value: 0,
        });
        func.block_mut(bb).terminator = Term::Ret(v0);
        module.add_function(func);
        module
    }

    #[test]
    fn valid_module_passes() {
        let module = make_valid_module();
        let errors = validate_module(&module);
        assert!(errors.is_empty(), "errors: {errors:?}");
    }

    #[test]
    fn detects_invalid_block_target() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("bad".into(), vec![], LirType::Void);
        let bb = func.add_block();
        func.block_mut(bb).terminator = Term::Jump(BlockId(99), vec![]);
        module.add_function(func);

        let errors = validate_module(&module);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("invalid bb99"));
    }

    #[test]
    fn detects_param_count_mismatch() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("bad".into(), vec![], LirType::Void);

        let bb0 = func.add_block();
        let bb1 = func.add_block();

        // bb1 expects one param but jump provides none
        let v_param = func.next_value();
        func.block_mut(bb1).params.push((v_param, LirType::I64));
        func.block_mut(bb1).terminator = Term::Ret(v_param);

        func.block_mut(bb0).terminator = Term::Jump(bb1, vec![]); // 0 args, expects 1

        module.add_function(func);

        let errors = validate_module(&module);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("provides 0 args but block expects 1"));
    }

    #[test]
    fn detects_duplicate_value_def() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("bad".into(), vec![], LirType::I32);
        let bb = func.add_block();

        // Define v0 twice
        func.block_mut(bb).insts.push(Inst::IConst {
            dst: ValueId(0),
            ty: LirType::I32,
            value: 1,
        });
        func.block_mut(bb).insts.push(Inst::IConst {
            dst: ValueId(0), // duplicate!
            ty: LirType::I32,
            value: 2,
        });
        func.block_mut(bb).terminator = Term::Ret(ValueId(0));
        // Manually set next_value to avoid assertion
        module.add_function(func);

        let errors = validate_module(&module);
        assert!(errors.iter().any(|e| e.message.contains("duplicate definition")));
    }

    #[test]
    fn detects_slot_out_of_range() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("bad".into(), vec![], LirType::Void);
        let bb = func.add_block();

        func.block_mut(bb).insts.push(Inst::SlotStore {
            slot: SlotId(5), // no slots exist
            value: ValueId(0),
            is_move: false,
        });
        func.block_mut(bb).terminator = Term::RetVoid;
        module.add_function(func);

        let errors = validate_module(&module);
        assert!(errors.iter().any(|e| e.message.contains("slot s5 out of range")));
    }

    #[test]
    fn detects_struct_out_of_range() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("bad".into(), vec![], LirType::Void);
        let bb = func.add_block();

        func.block_mut(bb).insts.push(Inst::FieldPtr {
            dst: ValueId(0),
            base: ValueId(1),
            struct_id: StructId(99), // doesn't exist
            field: 0,
        });
        func.block_mut(bb).terminator = Term::RetVoid;
        module.add_function(func);

        let errors = validate_module(&module);
        assert!(errors.iter().any(|e| e.message.contains("struct.99 out of range")));
    }

    #[test]
    fn detects_field_out_of_range() {
        let mut module = LirModule::new();
        let sid = module.add_struct(StructDef {
            name: "Tiny".into(),
            fields: vec![("x".into(), LirType::I32)],
            is_enum: false,
            computed_c_size: None,
                      });

        let mut func = LirFunction::new("bad".into(), vec![], LirType::Void);
        let bb = func.add_block();

        func.block_mut(bb).insts.push(Inst::FieldPtr {
            dst: ValueId(0),
            base: ValueId(1),
            struct_id: sid,
            field: 5, // only 1 field
        });
        func.block_mut(bb).terminator = Term::RetVoid;
        module.add_function(func);

        let errors = validate_module(&module);
        assert!(errors.iter().any(|e| e.message.contains("field index 5 out of range")));
    }

    #[test]
    fn ssa_dominance_detects_undefined() {
        let mut func = LirFunction::new("bad".into(), vec![], LirType::I64);
        let bb = func.add_block();

        // Use v99 which is never defined
        func.block_mut(bb).insts.push(Inst::Neg {
            dst: ValueId(0),
            ty: LirType::I64,
            operand: ValueId(99),
        });
        func.block_mut(bb).terminator = Term::Ret(ValueId(0));

        let errors = validate_ssa_dominance(&func);
        assert!(errors.iter().any(|e| e.message.contains("undefined value v99")));
    }

    #[test]
    fn ssa_dominance_passes_valid() {
        let mut func = LirFunction::new("ok".into(), vec![], LirType::I64);
        let bb = func.add_block();

        let v0 = func.next_value();
        let v1 = func.next_value();
        func.block_mut(bb).insts.push(Inst::IConst {
            dst: v0,
            ty: LirType::I64,
            value: 42,
        });
        func.block_mut(bb).insts.push(Inst::Neg {
            dst: v1,
            ty: LirType::I64,
            operand: v0,
        });
        func.block_mut(bb).terminator = Term::Ret(v1);

        let errors = validate_ssa_dominance(&func);
        assert!(errors.is_empty(), "errors: {errors:?}");
    }

    #[test]
    fn ssa_dominance_detects_use_before_def_same_block() {
        let mut func = LirFunction::new("bad".into(), vec![], LirType::I64);
        let bb = func.add_block();

        let v0 = func.next_value();
        let v1 = func.next_value();
        // Use v1 before it's defined
        func.block_mut(bb).insts.push(Inst::Neg {
            dst: v0,
            ty: LirType::I64,
            operand: v1,
        });
        func.block_mut(bb).insts.push(Inst::IConst {
            dst: v1,
            ty: LirType::I64,
            value: 42,
        });
        func.block_mut(bb).terminator = Term::Ret(v0);

        let errors = validate_ssa_dominance(&func);
        assert!(errors.iter().any(|e| e.message.contains("before its definition")),
            "expected use-before-def error, got: {errors:?}");
    }

    #[test]
    fn ssa_dominance_passes_diamond_cfg() {
        // bb0 → bb1, bb2 → bb3 (diamond)
        let mut func = LirFunction::new("diamond".into(), vec![], LirType::I64);
        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();
        let bb3 = func.add_block();

        let v_cond = func.next_value();
        let v_one = func.next_value();
        let v_two = func.next_value();
        let v_result = func.next_value();

        // bb0: define cond, branch
        func.block_mut(bb0).insts.push(Inst::BoolConst { dst: v_cond, value: true });
        func.block_mut(bb0).terminator = Term::Branch {
            cond: v_cond,
            then_block: bb1, then_args: vec![],
            else_block: bb2, else_args: vec![],
        };

        // bb1: define v_one, jump to bb3
        func.block_mut(bb1).insts.push(Inst::IConst { dst: v_one, ty: LirType::I64, value: 1 });
        func.block_mut(bb1).terminator = Term::Jump(bb3, vec![v_one]);

        // bb2: define v_two, jump to bb3
        func.block_mut(bb2).insts.push(Inst::IConst { dst: v_two, ty: LirType::I64, value: 2 });
        func.block_mut(bb2).terminator = Term::Jump(bb3, vec![v_two]);

        // bb3: receive via block param, return
        func.block_mut(bb3).params.push((v_result, LirType::I64));
        func.block_mut(bb3).terminator = Term::Ret(v_result);

        let errors = validate_ssa_dominance(&func);
        assert!(errors.is_empty(), "errors: {errors:?}");
    }
}
