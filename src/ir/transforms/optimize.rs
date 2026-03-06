//! GIR optimization passes — backend-agnostic transforms on the IR.
//!
//! These run after lowering and before backend emission. All passes
//! operate on `Function` in-place and preserve GIR semantics.

use std::collections::VecDeque;

use crate::ir::{BasicBlock, Function};
use crate::ir::instructions::{Instruction, Operand, Place, Terminator};

/// Run all optimization passes on every function in the module.
pub fn optimize_module(module: &mut crate::ir::Module) {
    for func in &mut module.functions {
        eliminate_dead_blocks(func);
        eliminate_unused_locals(func);
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
