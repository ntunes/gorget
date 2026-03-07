//! GIR optimization passes — backend-agnostic transforms on the IR.
//!
//! These run after lowering and before backend emission. All passes
//! operate on `Function` in-place and preserve GIR semantics.

use std::collections::{HashSet, VecDeque};

use crate::ir::{BasicBlock, Function};
use crate::ir::instructions::{BinOp, CmpOp, Constant, Instruction, Operand, Place, Terminator, UnOp};
use crate::ir::types::LocalId;

/// Run all optimization passes on every function in the module.
pub fn optimize_module(module: &mut crate::ir::Module) {
    for func in &mut module.functions {
        constant_fold(func);
        fold_constant_branches(func);
        elide_dead_drops(func);
        thread_jumps(func);
        eliminate_dead_blocks(func);
        eliminate_unused_locals(func);
    }
}

// ── Constant Folding ──────────────────────────────────────────────────

/// Evaluate BinOp, UnOp, and Cmp instructions with constant operands
/// at compile time, replacing them with simple Assign of the result.
fn constant_fold(func: &mut Function) {
    for bb in &mut func.blocks {
        for inst in &mut bb.instructions {
            let folded = match inst {
                Instruction::BinOp { dst, op, lhs, rhs, .. } => {
                    fold_binop(*dst, *op, lhs, rhs)
                }
                Instruction::UnOp { dst, op, operand, .. } => {
                    fold_unop(*dst, *op, operand)
                }
                Instruction::Cmp { dst, op, lhs, rhs, .. } => {
                    fold_cmp(*dst, *op, lhs, rhs)
                }
                _ => None,
            };
            if let Some(new_inst) = folded {
                *inst = new_inst;
            }
        }
    }
}

fn fold_binop(dst: LocalId, op: BinOp, lhs: &Operand, rhs: &Operand) -> Option<Instruction> {
    let (l, r) = match (lhs, rhs) {
        (Operand::Constant(l), Operand::Constant(r)) => (l, r),
        _ => return None,
    };
    let result = match (l, r) {
        (Constant::I64(a), Constant::I64(b)) => fold_binop_i64(*a, op, *b)?,
        (Constant::F64(a), Constant::F64(b)) => fold_binop_f64(*a, op, *b)?,
        (Constant::Bool(a), Constant::Bool(b)) => fold_binop_bool(*a, op, *b)?,
        _ => return None,
    };
    Some(Instruction::Assign {
        dst: Place::local(dst),
        value: Operand::Constant(result),
    })
}

fn fold_binop_i64(a: i64, op: BinOp, b: i64) -> Option<Constant> {
    Some(Constant::I64(match op {
        BinOp::Add => a.checked_add(b)?,
        BinOp::Sub => a.checked_sub(b)?,
        BinOp::Mul => a.checked_mul(b)?,
        BinOp::Div => { if b == 0 { return None; } a.checked_div(b)? }
        BinOp::Rem | BinOp::Mod => { if b == 0 { return None; } a.checked_rem(b)? }
        BinOp::BitAnd => a & b,
        BinOp::BitOr => a | b,
        BinOp::BitXor => a ^ b,
        BinOp::Shl => { if b < 0 || b >= 64 { return None; } a << b }
        BinOp::Shr => { if b < 0 || b >= 64 { return None; } a >> b }
        BinOp::AddWrap => a.wrapping_add(b),
        BinOp::SubWrap => a.wrapping_sub(b),
        BinOp::MulWrap => a.wrapping_mul(b),
        BinOp::Pow => {
            if b < 0 || b > 63 { return None; }
            i64::checked_pow(a, b as u32)?
        }
    }))
}

fn fold_binop_f64(a: f64, op: BinOp, b: f64) -> Option<Constant> {
    Some(Constant::F64(match op {
        BinOp::Add => a + b,
        BinOp::Sub => a - b,
        BinOp::Mul => a * b,
        BinOp::Div => { if b == 0.0 { return None; } a / b }
        BinOp::Rem | BinOp::Mod => { if b == 0.0 { return None; } a % b }
        BinOp::Pow => a.powf(b),
        _ => return None, // bitwise ops don't apply to floats
    }))
}

fn fold_binop_bool(a: bool, op: BinOp, b: bool) -> Option<Constant> {
    Some(Constant::Bool(match op {
        BinOp::BitAnd => a & b,
        BinOp::BitOr => a | b,
        BinOp::BitXor => a ^ b,
        _ => return None,
    }))
}

fn fold_unop(dst: LocalId, op: UnOp, operand: &Operand) -> Option<Instruction> {
    let c = match operand {
        Operand::Constant(c) => c,
        _ => return None,
    };
    let result = match (op, c) {
        (UnOp::Neg, Constant::I64(a)) => Constant::I64(a.checked_neg()?),
        (UnOp::Neg, Constant::F64(a)) => Constant::F64(-a),
        (UnOp::Not, Constant::Bool(a)) => Constant::Bool(!a),
        (UnOp::BitNot, Constant::I64(a)) => Constant::I64(!a),
        _ => return None,
    };
    Some(Instruction::Assign {
        dst: Place::local(dst),
        value: Operand::Constant(result),
    })
}

fn fold_cmp(dst: LocalId, op: CmpOp, lhs: &Operand, rhs: &Operand) -> Option<Instruction> {
    let (l, r) = match (lhs, rhs) {
        (Operand::Constant(l), Operand::Constant(r)) => (l, r),
        _ => return None,
    };
    let result = match (l, r) {
        (Constant::I64(a), Constant::I64(b)) => cmp_ord(a.cmp(b), op),
        (Constant::F64(a), Constant::F64(b)) => cmp_ord(a.partial_cmp(b)?, op),
        (Constant::Bool(a), Constant::Bool(b)) => match op {
            CmpOp::Eq => a == b,
            CmpOp::Ne => a != b,
            _ => return None,
        },
        (Constant::Str(a), Constant::Str(b)) => cmp_ord(a.cmp(b), op),
        _ => return None,
    };
    Some(Instruction::Assign {
        dst: Place::local(dst),
        value: Operand::Constant(Constant::Bool(result)),
    })
}

fn cmp_ord(ord: std::cmp::Ordering, op: CmpOp) -> bool {
    match op {
        CmpOp::Eq => ord == std::cmp::Ordering::Equal,
        CmpOp::Ne => ord != std::cmp::Ordering::Equal,
        CmpOp::Lt => ord == std::cmp::Ordering::Less,
        CmpOp::Le => ord != std::cmp::Ordering::Greater,
        CmpOp::Gt => ord == std::cmp::Ordering::Greater,
        CmpOp::Ge => ord != std::cmp::Ordering::Less,
    }
}

/// Replace Branch terminators with constant conditions by Jump, and
/// simplify branches where both targets are the same block.
fn fold_constant_branches(func: &mut Function) {
    for bb in &mut func.blocks {
        let folded = match &bb.terminator {
            Some(Terminator::Branch { cond: Operand::Constant(Constant::Bool(true)), then_block, .. }) => {
                Some(Terminator::Jump(*then_block))
            }
            Some(Terminator::Branch { cond: Operand::Constant(Constant::Bool(false)), else_block, .. }) => {
                Some(Terminator::Jump(*else_block))
            }
            // Both targets identical — condition is dead code
            Some(Terminator::Branch { then_block, else_block, .. }) if then_block == else_block => {
                Some(Terminator::Jump(*then_block))
            }
            Some(Terminator::Switch { value: Operand::Constant(Constant::I64(v)), cases, default, .. }) => {
                let target = cases.iter()
                    .find(|(cv, _)| *cv == *v)
                    .map(|(_, b)| *b)
                    .unwrap_or(*default);
                Some(Terminator::Jump(target))
            }
            _ => None,
        };
        if let Some(new_term) = folded {
            bb.terminator = Some(new_term);
        }
    }
}

// ── Drop Elision ──────────────────────────────────────────────────────

/// Remove DropIfAlive when a MoveZero for the same local appears earlier
/// in the SAME basic block and no intervening assignment re-initializes it.
/// This is safe because within a single BB, instructions execute sequentially.
fn elide_dead_drops(func: &mut Function) {
    for bb in &mut func.blocks {
        // Track locals that have been MoveZero'd in this block (and not re-assigned)
        let mut moved_in_block: HashSet<u32> = HashSet::new();
        let mut elide_indices: Vec<usize> = Vec::new();

        for (i, inst) in bb.instructions.iter().enumerate() {
            match inst {
                Instruction::MoveZero { place } if place.projections.is_empty() => {
                    moved_in_block.insert(place.local.0);
                }
                // Any write to a local un-marks it (re-initialization)
                Instruction::Assign { dst, .. } if dst.projections.is_empty() => {
                    moved_in_block.remove(&dst.local.0);
                }
                Instruction::DropIfAlive { place }
                    if place.projections.is_empty()
                        && moved_in_block.contains(&place.local.0) =>
                {
                    elide_indices.push(i);
                }
                _ => {}
            }
        }

        if !elide_indices.is_empty() {
            let elide_set: HashSet<usize> = elide_indices.into_iter().collect();
            let mut idx = 0;
            bb.instructions.retain(|_| {
                let keep = !elide_set.contains(&idx);
                idx += 1;
                keep
            });
        }
    }
}

// ── Jump Threading ────────────────────────────────────────────────────

/// Thread jumps through empty blocks: if a block has no instructions and its
/// terminator is `Jump(target)`, redirect predecessors to `target` directly.
///
/// This cleans up the CFG after constant branch folding turns `Branch` into
/// `Jump`, leaving the unused branch target as an empty trampoline block.
/// Runs before dead block elimination so that the threaded blocks become
/// unreachable and get removed.
fn thread_jumps(func: &mut Function) {
    if func.blocks.is_empty() {
        return;
    }

    let n = func.blocks.len();

    // Build a forwarding table: for each block, where does it ultimately jump?
    // Follow chains (bb1 → bb2 → bb3) but cap depth to avoid infinite loops
    // in malformed IR (blocks that form a cycle of empty jumps).
    let mut forward = vec![None; n];
    for (i, block) in func.blocks.iter().enumerate() {
        if !block.instructions.is_empty() {
            continue;
        }
        if let Some(Terminator::Jump(target)) = &block.terminator {
            forward[i] = Some(target.0);
        }
    }

    // Resolve chains: if bb1 → bb2 and bb2 → bb3, then bb1 → bb3
    let mut resolved = vec![0u32; n];
    for i in 0..n {
        let mut target = i as u32;
        let mut depth = 0;
        while let Some(next) = forward[target as usize] {
            if next == target || depth > n { break; } // cycle guard
            target = next;
            depth += 1;
        }
        resolved[i] = target;
    }

    // Check if any remapping actually changed
    let has_changes = resolved.iter().enumerate().any(|(i, &t)| t != i as u32);
    if !has_changes {
        return;
    }

    // Don't remap block 0 (entry block) — it can't be skipped
    resolved[0] = 0;

    // Rewrite all terminator block references through the forwarding table
    for bb in &mut func.blocks {
        if let Some(ref mut term) = bb.terminator {
            remap_terminator_targets(term, &resolved);
        }
    }
}

/// Remap block targets in a terminator using a forwarding table.
fn remap_terminator_targets(term: &mut Terminator, forward: &[u32]) {
    match term {
        Terminator::Jump(target) => {
            target.0 = forward[target.0 as usize];
        }
        Terminator::Branch { then_block, else_block, .. } => {
            then_block.0 = forward[then_block.0 as usize];
            else_block.0 = forward[else_block.0 as usize];
        }
        Terminator::Switch { cases, default, .. } => {
            for (_, target) in cases.iter_mut() {
                target.0 = forward[target.0 as usize];
            }
            default.0 = forward[default.0 as usize];
        }
        Terminator::Invoke { normal, error, .. } => {
            normal.0 = forward[normal.0 as usize];
            error.0 = forward[error.0 as usize];
        }
        Terminator::Return(_) | Terminator::Unreachable => {}
    }
}

// ── Dead Block Elimination ─────────────────────────────────────────────

/// Remove basic blocks unreachable from the entry block (BB0).
/// Renumbers remaining block IDs and updates all terminators.
fn eliminate_dead_blocks(func: &mut Function) {
    if func.blocks.is_empty() {
        return;
    }

    // BFS from entry block to find all reachable blocks
    let n = func.blocks.len();
    let mut reachable = vec![false; n];
    let mut queue = VecDeque::new();
    reachable[0] = true;
    queue.push_back(0usize);

    while let Some(bb_idx) = queue.pop_front() {
        for succ in successors(&func.blocks[bb_idx]) {
            let s = succ as usize;
            if s < n && !reachable[s] {
                reachable[s] = true;
                queue.push_back(s);
            }
        }
    }

    // Count unreachable blocks — skip transform if all reachable
    let dead_count = reachable.iter().filter(|r| !**r).count();
    if dead_count == 0 {
        return;
    }

    // Build old→new index mapping (only for reachable blocks)
    let mut remap = vec![0u32; n];
    let mut new_idx = 0u32;
    for (old_idx, is_reachable) in reachable.iter().enumerate() {
        if *is_reachable {
            remap[old_idx] = new_idx;
            new_idx += 1;
        }
    }

    // Filter to reachable blocks and remap terminators
    let mut new_blocks = Vec::with_capacity(new_idx as usize);
    for (old_idx, block) in func.blocks.drain(..).enumerate() {
        if !reachable[old_idx] {
            continue;
        }
        let mut block = block;
        if let Some(ref mut term) = block.terminator {
            remap_terminator(term, &remap);
        }
        new_blocks.push(block);
    }
    func.blocks = new_blocks;
}

/// Collect successor block IDs from a basic block's terminator.
fn successors(bb: &BasicBlock) -> Vec<u32> {
    match &bb.terminator {
        Some(Terminator::Jump(target)) => vec![target.0],
        Some(Terminator::Branch { then_block, else_block, .. }) => {
            vec![then_block.0, else_block.0]
        }
        Some(Terminator::Switch { cases, default, .. }) => {
            let mut succs: Vec<u32> = cases.iter().map(|(_, b)| b.0).collect();
            succs.push(default.0);
            succs
        }
        Some(Terminator::Invoke { normal, error, .. }) => {
            vec![normal.0, error.0]
        }
        Some(Terminator::Return(_)) | Some(Terminator::Unreachable) | None => vec![],
    }
}

/// Remap block IDs in a terminator using the old→new mapping.
fn remap_terminator(term: &mut Terminator, remap: &[u32]) {
    match term {
        Terminator::Jump(target) => {
            target.0 = remap[target.0 as usize];
        }
        Terminator::Branch { then_block, else_block, .. } => {
            then_block.0 = remap[then_block.0 as usize];
            else_block.0 = remap[else_block.0 as usize];
        }
        Terminator::Switch { cases, default, .. } => {
            for (_, target) in cases.iter_mut() {
                target.0 = remap[target.0 as usize];
            }
            default.0 = remap[default.0 as usize];
        }
        Terminator::Invoke { normal, error, .. } => {
            normal.0 = remap[normal.0 as usize];
            error.0 = remap[error.0 as usize];
        }
        Terminator::Return(_) | Terminator::Unreachable => {}
    }
}

// ── Unused Local Elimination ───────────────────────────────────────────

/// Remove locals that are never referenced by any instruction or terminator.
/// Renumbers remaining local IDs. Skips _0 (return place) and params.
fn eliminate_unused_locals(func: &mut Function) {
    if func.locals.len() <= 1 {
        return;
    }

    let n = func.locals.len();
    let mut referenced = vec![false; n];

    // _0 (return place) and params are always referenced
    referenced[0] = true;
    let n_params = func.params.len();
    for i in 1..=n_params.min(n - 1) {
        referenced[i] = true;
    }

    // Scan all instructions and terminators for local references
    for bb in &func.blocks {
        for inst in &bb.instructions {
            mark_instruction_locals(inst, &mut referenced);
        }
        if let Some(ref term) = bb.terminator {
            mark_terminator_locals(term, &mut referenced);
        }
    }

    let dead_count = referenced.iter().filter(|r| !**r).count();
    if dead_count == 0 {
        return;
    }

    // Build old→new local ID mapping
    let mut remap = vec![0u32; n];
    let mut new_idx = 0u32;
    for (old_idx, is_ref) in referenced.iter().enumerate() {
        if *is_ref {
            remap[old_idx] = new_idx;
            new_idx += 1;
        }
    }

    // Filter locals
    let mut new_locals = Vec::with_capacity(new_idx as usize);
    for (old_idx, local) in func.locals.drain(..).enumerate() {
        if referenced[old_idx] {
            new_locals.push(local);
        }
    }
    func.locals = new_locals;

    // Params are TypeIds, not LocalIds — they don't need remapping.

    // Remap all local references in instructions and terminators
    for bb in &mut func.blocks {
        for inst in &mut bb.instructions {
            remap_instruction_locals(inst, &remap);
        }
        if let Some(ref mut term) = bb.terminator {
            remap_terminator_locals(term, &remap);
        }
    }
}

/// Mark all locals referenced by an instruction.
fn mark_instruction_locals(inst: &Instruction, referenced: &mut [bool]) {
    fn mark_operand(op: &Operand, referenced: &mut [bool]) {
        match op {
            Operand::Copy(p) | Operand::Move(p) => mark_place(p, referenced),
            Operand::Constant(_) => {}
        }
    }
    fn mark_place(p: &Place, referenced: &mut [bool]) {
        let idx = p.local.0 as usize;
        if idx < referenced.len() {
            referenced[idx] = true;
        }
        for proj in &p.projections {
            if let crate::ir::instructions::Projection::Index(local) = proj {
                let i = local.0 as usize;
                if i < referenced.len() {
                    referenced[i] = true;
                }
            }
        }
    }
    fn mark_local(id: u32, referenced: &mut [bool]) {
        let idx = id as usize;
        if idx < referenced.len() {
            referenced[idx] = true;
        }
    }

    match inst {
        Instruction::Nop => {}
        Instruction::Assign { dst, value } => {
            mark_place(dst, referenced);
            mark_operand(value, referenced);
        }
        Instruction::BinOp { dst, lhs, rhs, .. } => {
            mark_local(dst.0, referenced);
            mark_operand(lhs, referenced);
            mark_operand(rhs, referenced);
        }
        Instruction::UnOp { dst, operand, .. } => {
            mark_local(dst.0, referenced);
            mark_operand(operand, referenced);
        }
        Instruction::Cmp { dst, lhs, rhs, .. } => {
            mark_local(dst.0, referenced);
            mark_operand(lhs, referenced);
            mark_operand(rhs, referenced);
        }
        Instruction::Cast { dst, value, .. }
        | Instruction::BitCast { dst, value, .. }
        | Instruction::PtrCast { dst, value, .. } => {
            mark_local(dst.0, referenced);
            mark_operand(value, referenced);
        }
        Instruction::Call { dst, args, .. } | Instruction::CallExtern { dst, args, .. } => {
            if let Some(d) = dst { mark_local(d.0, referenced); }
            for a in args { mark_operand(a, referenced); }
        }
        Instruction::CallIndirect { dst, callee, args } => {
            if let Some(d) = dst { mark_local(d.0, referenced); }
            mark_operand(callee, referenced);
            for a in args { mark_operand(a, referenced); }
        }
        Instruction::MoveZero { place } => mark_place(place, referenced),
        Instruction::Borrow { dst, place } | Instruction::BorrowMut { dst, place } => {
            mark_local(dst.0, referenced);
            mark_place(place, referenced);
        }
        Instruction::FieldLoad { dst, base, .. } => {
            mark_local(dst.0, referenced);
            mark_place(base, referenced);
        }
        Instruction::IndexLoad { dst, base, index } => {
            mark_local(dst.0, referenced);
            mark_place(base, referenced);
            mark_operand(index, referenced);
        }
        Instruction::StructInit { dst, fields, .. } => {
            mark_local(dst.0, referenced);
            for f in fields { mark_operand(f, referenced); }
        }
        Instruction::TupleInit { dst, elements } => {
            mark_local(dst.0, referenced);
            for e in elements { mark_operand(e, referenced); }
        }
        Instruction::EnumInit { dst, fields, .. } => {
            mark_local(dst.0, referenced);
            for f in fields { mark_operand(f, referenced); }
        }
        Instruction::TagOf { dst, operand } => {
            mark_local(dst.0, referenced);
            mark_operand(operand, referenced);
        }
        Instruction::EnumFieldLoad { dst, base, .. } => {
            mark_local(dst.0, referenced);
            mark_place(base, referenced);
        }
        Instruction::HeapAlloc { dst, allocator, .. } => {
            mark_local(dst.0, referenced);
            mark_operand(allocator, referenced);
        }
        Instruction::HeapAllocArray { dst, count, allocator, .. } => {
            mark_local(dst.0, referenced);
            mark_operand(count, referenced);
            mark_operand(allocator, referenced);
        }
        Instruction::Dealloc { ptr, .. } => mark_operand(ptr, referenced),
        Instruction::Drop { place } | Instruction::DropIfAlive { place } => {
            mark_place(place, referenced);
        }
        Instruction::InlineC { .. } => {
            // InlineC may reference any local via _N — conservatively mark all
            for i in 0..referenced.len() {
                referenced[i] = true;
            }
        }
        Instruction::PushAllocator { allocator } => mark_operand(allocator, referenced),
        Instruction::PopAllocator => {}
        Instruction::LoadThreadLocal { dst, .. } => mark_local(dst.0, referenced),
    }
}

/// Mark all locals referenced by a terminator.
fn mark_terminator_locals(term: &Terminator, referenced: &mut [bool]) {
    fn mark_operand(op: &Operand, referenced: &mut [bool]) {
        if let Operand::Copy(p) | Operand::Move(p) = op {
            let idx = p.local.0 as usize;
            if idx < referenced.len() {
                referenced[idx] = true;
            }
        }
    }
    match term {
        Terminator::Return(op) => mark_operand(op, referenced),
        Terminator::Branch { cond, .. } => mark_operand(cond, referenced),
        Terminator::Switch { value, .. } => mark_operand(value, referenced),
        Terminator::Invoke { args, dst, .. } => {
            if let Some(d) = dst {
                let idx = d.0 as usize;
                if idx < referenced.len() { referenced[idx] = true; }
            }
            for a in args { mark_operand(a, referenced); }
        }
        Terminator::Jump(_) | Terminator::Unreachable => {}
    }
}

/// Remap local IDs in an instruction.
fn remap_instruction_locals(inst: &mut Instruction, remap: &[u32]) {
    fn remap_operand(op: &mut Operand, remap: &[u32]) {
        match op {
            Operand::Copy(p) | Operand::Move(p) => remap_place(p, remap),
            Operand::Constant(_) => {}
        }
    }
    fn remap_place(p: &mut Place, remap: &[u32]) {
        p.local.0 = remap[p.local.0 as usize];
        for proj in &mut p.projections {
            if let crate::ir::instructions::Projection::Index(local) = proj {
                local.0 = remap[local.0 as usize];
            }
        }
    }
    fn remap_local(id: &mut crate::ir::types::LocalId, remap: &[u32]) {
        id.0 = remap[id.0 as usize];
    }

    match inst {
        Instruction::Nop => {}
        Instruction::Assign { dst, value } => {
            remap_place(dst, remap);
            remap_operand(value, remap);
        }
        Instruction::BinOp { dst, lhs, rhs, .. } => {
            remap_local(dst, remap);
            remap_operand(lhs, remap);
            remap_operand(rhs, remap);
        }
        Instruction::UnOp { dst, operand, .. } => {
            remap_local(dst, remap);
            remap_operand(operand, remap);
        }
        Instruction::Cmp { dst, lhs, rhs, .. } => {
            remap_local(dst, remap);
            remap_operand(lhs, remap);
            remap_operand(rhs, remap);
        }
        Instruction::Cast { dst, value, .. }
        | Instruction::BitCast { dst, value, .. }
        | Instruction::PtrCast { dst, value, .. } => {
            remap_local(dst, remap);
            remap_operand(value, remap);
        }
        Instruction::Call { dst, args, .. } | Instruction::CallExtern { dst, args, .. } => {
            if let Some(d) = dst { remap_local(d, remap); }
            for a in args { remap_operand(a, remap); }
        }
        Instruction::CallIndirect { dst, callee, args } => {
            if let Some(d) = dst { remap_local(d, remap); }
            remap_operand(callee, remap);
            for a in args { remap_operand(a, remap); }
        }
        Instruction::MoveZero { place } => remap_place(place, remap),
        Instruction::Borrow { dst, place } | Instruction::BorrowMut { dst, place } => {
            remap_local(dst, remap);
            remap_place(place, remap);
        }
        Instruction::FieldLoad { dst, base, .. } => {
            remap_local(dst, remap);
            remap_place(base, remap);
        }
        Instruction::IndexLoad { dst, base, index } => {
            remap_local(dst, remap);
            remap_place(base, remap);
            remap_operand(index, remap);
        }
        Instruction::StructInit { dst, fields, .. } => {
            remap_local(dst, remap);
            for f in fields { remap_operand(f, remap); }
        }
        Instruction::TupleInit { dst, elements } => {
            remap_local(dst, remap);
            for e in elements { remap_operand(e, remap); }
        }
        Instruction::EnumInit { dst, fields, .. } => {
            remap_local(dst, remap);
            for f in fields { remap_operand(f, remap); }
        }
        Instruction::TagOf { dst, operand } => {
            remap_local(dst, remap);
            remap_operand(operand, remap);
        }
        Instruction::EnumFieldLoad { dst, base, .. } => {
            remap_local(dst, remap);
            remap_place(base, remap);
        }
        Instruction::HeapAlloc { dst, allocator, .. } => {
            remap_local(dst, remap);
            remap_operand(allocator, remap);
        }
        Instruction::HeapAllocArray { dst, count, allocator, .. } => {
            remap_local(dst, remap);
            remap_operand(count, remap);
            remap_operand(allocator, remap);
        }
        Instruction::Dealloc { ptr, .. } => remap_operand(ptr, remap),
        Instruction::Drop { place } | Instruction::DropIfAlive { place } => {
            remap_place(place, remap);
        }
        Instruction::InlineC { .. } => {} // InlineC uses raw strings — can't remap
        Instruction::PushAllocator { allocator } => remap_operand(allocator, remap),
        Instruction::PopAllocator => {}
        Instruction::LoadThreadLocal { dst, .. } => remap_local(dst, remap),
    }
}

/// Remap local IDs in a terminator.
fn remap_terminator_locals(term: &mut Terminator, remap: &[u32]) {
    fn remap_operand(op: &mut Operand, remap: &[u32]) {
        if let Operand::Copy(p) | Operand::Move(p) = op {
            p.local.0 = remap[p.local.0 as usize];
            for proj in &mut p.projections {
                if let crate::ir::instructions::Projection::Index(local) = proj {
                    local.0 = remap[local.0 as usize];
                }
            }
        }
    }
    match term {
        Terminator::Return(op) => remap_operand(op, remap),
        Terminator::Branch { cond, .. } => remap_operand(cond, remap),
        Terminator::Switch { value, .. } => remap_operand(value, remap),
        Terminator::Invoke { args, dst, .. } => {
            if let Some(d) = dst { d.0 = remap[d.0 as usize]; }
            for a in args { remap_operand(a, remap); }
        }
        Terminator::Jump(_) | Terminator::Unreachable => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Function, BasicBlock, Local};
    use crate::ir::instructions::*;
    use crate::ir::types::*;

    fn make_func(blocks: Vec<BasicBlock>, locals: Vec<Local>, params: Vec<TypeId>) -> Function {
        Function {
            name: "test".into(),
            params,
            return_type: I64_TYPE,
            locals,
            blocks,
            is_test_fn: false,
            display_name: None,
            def_span: None,
        }
    }

    fn local(ty: TypeId) -> Local {
        Local { type_id: ty, name_hint: None }
    }

    fn bb(instructions: Vec<Instruction>, terminator: Terminator) -> BasicBlock {
        let span_map = vec![None; instructions.len()];
        BasicBlock { instructions, terminator: Some(terminator), span_map, terminator_span: None }
    }

    // ── Constant Folding Tests ──────────────────────────────────────

    fn ret(op: Operand) -> Terminator { Terminator::Return(op) }
    fn ret_local(id: u32) -> Terminator { ret(Operand::Copy(Place::local(LocalId(id)))) }
    fn ret_i64(v: i64) -> Terminator { ret(Operand::Constant(Constant::I64(v))) }

    #[test]
    fn fold_add_i64() {
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(1), op: BinOp::Add, type_id: I64_TYPE,
            lhs: Operand::Constant(Constant::I64(3)), rhs: Operand::Constant(Constant::I64(4)),
        }], ret_local(1))], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        constant_fold(&mut f);
        assert!(matches!(&f.blocks[0].instructions[0],
            Instruction::Assign { value: Operand::Constant(Constant::I64(7)), .. }));
    }

    #[test]
    fn fold_div_by_zero_unchanged() {
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(1), op: BinOp::Div, type_id: I64_TYPE,
            lhs: Operand::Constant(Constant::I64(10)), rhs: Operand::Constant(Constant::I64(0)),
        }], ret_i64(0))], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        constant_fold(&mut f);
        assert!(matches!(&f.blocks[0].instructions[0], Instruction::BinOp { .. }));
    }

    #[test]
    fn fold_f64_mul() {
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(1), op: BinOp::Mul, type_id: F64_TYPE,
            lhs: Operand::Constant(Constant::F64(2.5)), rhs: Operand::Constant(Constant::F64(4.0)),
        }], ret_local(1))], vec![local(F64_TYPE), local(F64_TYPE)], vec![]);
        constant_fold(&mut f);
        match &f.blocks[0].instructions[0] {
            Instruction::Assign { value: Operand::Constant(Constant::F64(v)), .. } => {
                assert!((v - 10.0).abs() < 1e-10);
            }
            other => panic!("Expected folded f64, got {:?}", other),
        }
    }

    #[test]
    fn fold_unop_neg() {
        let mut f = make_func(vec![bb(vec![Instruction::UnOp {
            dst: LocalId(1), op: UnOp::Neg, type_id: I64_TYPE,
            operand: Operand::Constant(Constant::I64(42)),
        }], ret_local(1))], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        constant_fold(&mut f);
        assert!(matches!(&f.blocks[0].instructions[0],
            Instruction::Assign { value: Operand::Constant(Constant::I64(-42)), .. }));
    }

    #[test]
    fn fold_cmp_lt() {
        let mut f = make_func(vec![bb(vec![Instruction::Cmp {
            dst: LocalId(1), op: CmpOp::Lt, type_id: I64_TYPE,
            lhs: Operand::Constant(Constant::I64(3)), rhs: Operand::Constant(Constant::I64(5)),
        }], ret_local(1))], vec![local(BOOL_TYPE), local(BOOL_TYPE)], vec![]);
        constant_fold(&mut f);
        assert!(matches!(&f.blocks[0].instructions[0],
            Instruction::Assign { value: Operand::Constant(Constant::Bool(true)), .. }));
    }

    #[test]
    fn fold_non_constant_unchanged() {
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(2), op: BinOp::Add, type_id: I64_TYPE,
            lhs: Operand::Copy(Place::local(LocalId(1))), rhs: Operand::Constant(Constant::I64(1)),
        }], ret_local(2))], vec![local(I64_TYPE), local(I64_TYPE), local(I64_TYPE)], vec![I64_TYPE]);
        constant_fold(&mut f);
        assert!(matches!(&f.blocks[0].instructions[0], Instruction::BinOp { .. }));
    }

    // ── Constant Branch Folding Tests ───────────────────────────────

    #[test]
    fn fold_true_branch() {
        let mut f = make_func(vec![
            bb(vec![], Terminator::Branch {
                cond: Operand::Constant(Constant::Bool(true)),
                then_block: BlockId(1), else_block: BlockId(2),
            }),
            bb(vec![], ret_i64(1)),
            bb(vec![], ret_i64(2)),
        ], vec![local(I64_TYPE)], vec![]);
        fold_constant_branches(&mut f);
        assert!(matches!(f.blocks[0].terminator, Some(Terminator::Jump(BlockId(1)))));
    }

    #[test]
    fn fold_switch_constant() {
        let mut f = make_func(vec![
            bb(vec![], Terminator::Switch {
                value: Operand::Constant(Constant::I64(2)),
                cases: vec![(1, BlockId(1)), (2, BlockId(2)), (3, BlockId(3))],
                default: BlockId(4),
            }),
            bb(vec![], ret_i64(0)),
            bb(vec![], ret_i64(0)),
            bb(vec![], ret_i64(0)),
            bb(vec![], ret_i64(0)),
        ], vec![local(I64_TYPE)], vec![]);
        fold_constant_branches(&mut f);
        assert!(matches!(f.blocks[0].terminator, Some(Terminator::Jump(BlockId(2)))));
    }

    #[test]
    fn fold_identity_branch() {
        // Branch where both targets are the same → Jump
        let mut f = make_func(vec![
            bb(vec![], Terminator::Branch {
                cond: Operand::Copy(Place::local(LocalId(1))),
                then_block: BlockId(1), else_block: BlockId(1),
            }),
            bb(vec![], ret_i64(0)),
        ], vec![local(I64_TYPE), local(BOOL_TYPE)], vec![BOOL_TYPE]);
        fold_constant_branches(&mut f);
        assert!(matches!(f.blocks[0].terminator, Some(Terminator::Jump(BlockId(1)))));
    }

    // ── Drop Elision Tests ──────────────────────────────────────────

    #[test]
    fn elide_drop_after_move_zero() {
        let mut f = make_func(vec![bb(vec![
            Instruction::Assign { dst: Place::local(LocalId(1)), value: Operand::Constant(Constant::I64(42)) },
            Instruction::MoveZero { place: Place::local(LocalId(1)) },
            Instruction::DropIfAlive { place: Place::local(LocalId(1)) },
        ], ret_i64(0))], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        elide_dead_drops(&mut f);
        assert_eq!(f.blocks[0].instructions.len(), 2);
    }

    #[test]
    fn no_elide_drop_after_reassign() {
        let mut f = make_func(vec![bb(vec![
            Instruction::MoveZero { place: Place::local(LocalId(1)) },
            Instruction::Assign { dst: Place::local(LocalId(1)), value: Operand::Constant(Constant::I64(99)) },
            Instruction::DropIfAlive { place: Place::local(LocalId(1)) },
        ], ret_i64(0))], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        elide_dead_drops(&mut f);
        assert_eq!(f.blocks[0].instructions.len(), 3);
    }

    #[test]
    fn no_elide_cross_block_drop() {
        let mut f = make_func(vec![
            bb(vec![Instruction::MoveZero { place: Place::local(LocalId(1)) }],
               Terminator::Jump(BlockId(1))),
            bb(vec![Instruction::DropIfAlive { place: Place::local(LocalId(1)) }],
               ret_i64(0)),
        ], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        elide_dead_drops(&mut f);
        assert_eq!(f.blocks[1].instructions.len(), 1);
    }

    // ── Jump Threading Tests ──────────────────────────────────────────

    #[test]
    fn thread_simple_jump() {
        // bb0 → bb1 (empty) → bb2. After threading, bb0 → bb2.
        let mut f = make_func(vec![
            bb(vec![], Terminator::Jump(BlockId(1))),
            bb(vec![], Terminator::Jump(BlockId(2))),  // empty trampoline
            bb(vec![], ret_i64(42)),
        ], vec![local(I64_TYPE)], vec![]);
        thread_jumps(&mut f);
        assert!(matches!(f.blocks[0].terminator, Some(Terminator::Jump(BlockId(2)))));
    }

    #[test]
    fn thread_chain() {
        // bb0 → bb1 → bb2 → bb3. All intermediates empty.
        let mut f = make_func(vec![
            bb(vec![], Terminator::Jump(BlockId(1))),
            bb(vec![], Terminator::Jump(BlockId(2))),
            bb(vec![], Terminator::Jump(BlockId(3))),
            bb(vec![], ret_i64(0)),
        ], vec![local(I64_TYPE)], vec![]);
        thread_jumps(&mut f);
        assert!(matches!(f.blocks[0].terminator, Some(Terminator::Jump(BlockId(3)))));
    }

    #[test]
    fn thread_branch_targets() {
        // bb0 branches to bb1 (empty→bb3) and bb2 (empty→bb3).
        let mut f = make_func(vec![
            bb(vec![], Terminator::Branch {
                cond: Operand::Copy(Place::local(LocalId(1))),
                then_block: BlockId(1), else_block: BlockId(2),
            }),
            bb(vec![], Terminator::Jump(BlockId(3))),  // empty → bb3
            bb(vec![], Terminator::Jump(BlockId(3))),  // empty → bb3
            bb(vec![], ret_i64(0)),
        ], vec![local(I64_TYPE), local(BOOL_TYPE)], vec![BOOL_TYPE]);
        thread_jumps(&mut f);
        match &f.blocks[0].terminator {
            Some(Terminator::Branch { then_block, else_block, .. }) => {
                assert_eq!(then_block.0, 3);
                assert_eq!(else_block.0, 3);
            }
            other => panic!("Expected Branch, got {:?}", other),
        }
    }

    #[test]
    fn no_thread_non_empty_block() {
        // bb1 has instructions — should NOT be threaded
        let mut f = make_func(vec![
            bb(vec![], Terminator::Jump(BlockId(1))),
            bb(vec![Instruction::Assign {
                dst: Place::local(LocalId(1)),
                value: Operand::Constant(Constant::I64(5)),
            }], Terminator::Jump(BlockId(2))),
            bb(vec![], ret_local(1)),
        ], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        thread_jumps(&mut f);
        // bb0 should still go to bb1 (not threaded past it)
        assert!(matches!(f.blocks[0].terminator, Some(Terminator::Jump(BlockId(1)))));
    }

    #[test]
    fn fold_wrapping_ops() {
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(1), op: BinOp::AddWrap, type_id: I64_TYPE,
            lhs: Operand::Constant(Constant::I64(i64::MAX)), rhs: Operand::Constant(Constant::I64(1)),
        }], ret_local(1))], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        constant_fold(&mut f);
        match &f.blocks[0].instructions[0] {
            Instruction::Assign { value: Operand::Constant(Constant::I64(v)), .. } => {
                assert_eq!(*v, i64::MIN);
            }
            other => panic!("Expected folded wrapping add, got {:?}", other),
        }
    }

    #[test]
    fn fold_overflow_unchanged() {
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(1), op: BinOp::Add, type_id: I64_TYPE,
            lhs: Operand::Constant(Constant::I64(i64::MAX)), rhs: Operand::Constant(Constant::I64(1)),
        }], ret_local(1))], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        constant_fold(&mut f);
        assert!(matches!(&f.blocks[0].instructions[0], Instruction::BinOp { .. }));
    }
}
