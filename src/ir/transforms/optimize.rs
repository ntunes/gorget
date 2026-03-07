//! GIR optimization passes — backend-agnostic transforms on the IR.
//!
//! These run after lowering and before backend emission. All passes
//! operate on `Function` in-place and preserve GIR semantics.

use std::collections::{HashSet, VecDeque};

use crate::ir::{BasicBlock, Function};
use crate::ir::instructions::{BinOp, CmpOp, Constant, Instruction, Operand, Place, Projection, Terminator, UnOp};
use crate::ir::types::{BlockId, LocalId, TypeId};

/// Run all optimization passes on every function in the module.
pub fn optimize_module(module: &mut crate::ir::Module) {
    for func in &mut module.functions {
        // Phase 1: simplify values
        propagate_constants(func);
        constant_fold(func);
        // Re-propagate after folding: constant_fold can turn computed
        // instructions (e.g., UnOp(Not, false)) into Assign(true),
        // creating new propagation opportunities.
        propagate_constants(func);
        constant_fold(func);
        simplify_algebraic(func);
        simplify_cmp(func);
        eliminate_common_subexpressions(func);
        reduce_strength(func);
        fold_constant_branches(func);
        eliminate_self_assigns(func);
        // Phase 2: eliminate dead code
        eliminate_nops(func);
        elide_dead_drops(func);
        eliminate_dead_stores(func);
        // Phase 3: simplify CFG
        thread_jumps(func);
        merge_blocks(func);
        eliminate_dead_blocks(func);
        eliminate_unused_locals(func);
    }
}

// ── Constant Propagation ─────────────────────────────────────────────

/// Within each basic block, substitute `Copy(local)` operands with known
/// constant values when the local was previously assigned a constant and
/// not reassigned.  This enables constant folding to fire on patterns like:
///
///     _1 = 42
///     _2 = BinOp(Add, Copy(_1), Copy(_1))  →  _2 = BinOp(Add, 42, 42)
///
/// Only propagates through simple `Assign { dst: local, value: Constant }`.
/// Invalidates on any reassignment to the local.
fn propagate_constants(func: &mut Function) {
    for bb in &mut func.blocks {
        let mut known: std::collections::HashMap<u32, Constant> = std::collections::HashMap::new();

        for inst in &mut bb.instructions {
            // First: substitute known constants into operands
            substitute_operands(inst, &known);

            // Then: track/invalidate based on this instruction's writes
            match inst {
                Instruction::Assign { dst, value } if dst.projections.is_empty() => {
                    if let Operand::Constant(c) = value {
                        // Only propagate simple scalar constants — strings, function
                        // refs, and other complex values have ABI implications that
                        // break when substituted into instruction operands.
                        if is_propagatable_constant(c) {
                            known.insert(dst.local.0, c.clone());
                        } else {
                            known.remove(&dst.local.0);
                        }
                    } else {
                        known.remove(&dst.local.0);
                    }
                }
                // Calls can modify any local through borrows/pointers —
                // invalidate all tracked constants conservatively.
                Instruction::Call { dst, .. }
                | Instruction::CallExtern { dst, .. } => {
                    known.clear();
                    // Re-track the dst if it's a constant (it's a fresh write)
                    // — but Call results are never constants, so just clear.
                    let _ = dst;
                }
                Instruction::CallIndirect { .. } => {
                    known.clear();
                }
                _ => {
                    // Any other write to a local invalidates it
                    if let Some(written) = instruction_dst(inst) {
                        known.remove(&written);
                    }
                }
            }
        }

        // Also substitute into the terminator
        if !known.is_empty() {
            if let Some(ref mut term) = bb.terminator {
                substitute_terminator_operands(term, &known);
            }
        }
    }
}

/// Lightweight re-propagation that only substitutes constants into terminators.
/// Used after constant folding to enable branch folding for cases where
/// folding created new constant assignments (e.g., UnOp(Not, false) → true).
/// Check if a constant is safe to propagate into operand positions.
/// Strings, function refs, and global refs have complex ABI (e.g., Str struct
/// vs const char*) and must not be substituted.
fn is_propagatable_constant(c: &Constant) -> bool {
    matches!(c,
        Constant::Bool(_) |
        Constant::I8(_) | Constant::I16(_) | Constant::I32(_) | Constant::I64(_) |
        Constant::U8(_) | Constant::U16(_) | Constant::U32(_) | Constant::U64(_) |
        Constant::F32(_) | Constant::F64(_)
    )
}

/// Replace `Copy(local)` operands with known constant values.
fn substitute_operands(inst: &mut Instruction, known: &std::collections::HashMap<u32, Constant>) {
    let sub = |op: &mut Operand| {
        if let Operand::Copy(p) = &*op {
            if p.projections.is_empty() {
                if let Some(c) = known.get(&p.local.0) {
                    *op = Operand::Constant(c.clone());
                }
            }
        }
    };

    match inst {
        Instruction::Assign { value, .. } => sub(value),
        Instruction::BinOp { lhs, rhs, .. } | Instruction::Cmp { lhs, rhs, .. } => {
            sub(lhs);
            sub(rhs);
        }
        Instruction::UnOp { operand, .. }
        | Instruction::Cast { value: operand, .. }
        | Instruction::BitCast { value: operand, .. }
        | Instruction::PtrCast { value: operand, .. }
        | Instruction::TagOf { operand, .. } => {
            sub(operand);
        }
        Instruction::IndexLoad { index, .. } => sub(index),
        Instruction::Call { args, .. } | Instruction::CallExtern { args, .. } => {
            for a in args { sub(a); }
        }
        Instruction::CallIndirect { callee, args, .. } => {
            sub(callee);
            for a in args { sub(a); }
        }
        Instruction::StructInit { fields, .. } | Instruction::EnumInit { fields, .. } => {
            for f in fields { sub(f); }
        }
        Instruction::TupleInit { elements, .. } => {
            for e in elements { sub(e); }
        }
        Instruction::HeapAlloc { allocator, .. } => sub(allocator),
        Instruction::HeapAllocArray { count, allocator, .. } => {
            sub(count);
            sub(allocator);
        }
        Instruction::Dealloc { ptr, allocator } => {
            sub(ptr);
            sub(allocator);
        }
        Instruction::PushAllocator { allocator } => sub(allocator),
        // Instructions without operands that can be substituted
        _ => {}
    }
}

/// Replace `Copy(local)` operands in terminators with known constant values.
fn substitute_terminator_operands(term: &mut Terminator, known: &std::collections::HashMap<u32, Constant>) {
    let sub = |op: &mut Operand| {
        if let Operand::Copy(p) = &*op {
            if p.projections.is_empty() {
                if let Some(c) = known.get(&p.local.0) {
                    *op = Operand::Constant(c.clone());
                }
            }
        }
    };
    match term {
        Terminator::Return(op) => sub(op),
        Terminator::Branch { cond, .. } => sub(cond),
        Terminator::Switch { value, .. } => sub(value),
        Terminator::Invoke { args, .. } => {
            for a in args { sub(a); }
        }
        Terminator::Jump(_) | Terminator::Unreachable => {}
    }
}

// ── Constant Folding ──────────────────────────────────────────────────

/// Evaluate BinOp, UnOp, Cmp, and Cast instructions with constant operands
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
                Instruction::Cast { dst, target_type, value } => {
                    fold_cast(*dst, *target_type, value)
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
        BinOp::Rem => { if b == 0 { return None; } a.checked_rem(b)? }
        BinOp::Mod => {
            // Mathematical modulo: result has sign of divisor.
            // ((a % b) + b) % b
            if b == 0 { return None; }
            let r = a.checked_rem(b)?;
            ((r.wrapping_add(b)).checked_rem(b))?
        }
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
        BinOp::Rem => { if b == 0.0 { return None; } a % b }
        BinOp::Mod => { if b == 0.0 { return None; } ((a % b) + b) % b }
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

fn fold_cast(dst: LocalId, target: TypeId, value: &Operand) -> Option<Instruction> {
    use crate::ir::types::*;
    let c = match value {
        Operand::Constant(c) => c,
        _ => return None,
    };
    // Extract source as i64 or f64 for numeric casting.
    let result = match target {
        I64_TYPE => match c {
            Constant::I8(v) => Constant::I64(*v as i64),
            Constant::I16(v) => Constant::I64(*v as i64),
            Constant::I32(v) => Constant::I64(*v as i64),
            Constant::U8(v) => Constant::I64(*v as i64),
            Constant::U16(v) => Constant::I64(*v as i64),
            Constant::U32(v) => Constant::I64(*v as i64),
            Constant::U64(v) => Constant::I64(*v as i64),
            Constant::F32(v) => Constant::I64(*v as i64),
            Constant::F64(v) => Constant::I64(*v as i64),
            Constant::Bool(v) => Constant::I64(if *v { 1 } else { 0 }),
            Constant::I64(_) => return None, // identity cast
            _ => return None,
        },
        I32_TYPE => match c {
            Constant::I8(v) => Constant::I32(*v as i32),
            Constant::I16(v) => Constant::I32(*v as i32),
            Constant::I64(v) => Constant::I32(*v as i32),
            Constant::U8(v) => Constant::I32(*v as i32),
            Constant::U16(v) => Constant::I32(*v as i32),
            Constant::U32(v) => Constant::I32(*v as i32),
            Constant::F32(v) => Constant::I32(*v as i32),
            Constant::F64(v) => Constant::I32(*v as i32),
            Constant::Bool(v) => Constant::I32(if *v { 1 } else { 0 }),
            Constant::I32(_) => return None,
            _ => return None,
        },
        I16_TYPE => match c {
            Constant::I8(v) => Constant::I16(*v as i16),
            Constant::I32(v) => Constant::I16(*v as i16),
            Constant::I64(v) => Constant::I16(*v as i16),
            Constant::U8(v) => Constant::I16(*v as i16),
            Constant::I16(_) => return None,
            _ => return None,
        },
        I8_TYPE => match c {
            Constant::I16(v) => Constant::I8(*v as i8),
            Constant::I32(v) => Constant::I8(*v as i8),
            Constant::I64(v) => Constant::I8(*v as i8),
            Constant::U8(v) => Constant::I8(*v as u8 as i8),
            Constant::I8(_) => return None,
            _ => return None,
        },
        U8_TYPE => match c {
            Constant::I8(v) => Constant::U8(*v as u8),
            Constant::I16(v) => Constant::U8(*v as u8),
            Constant::I32(v) => Constant::U8(*v as u8),
            Constant::I64(v) => Constant::U8(*v as u8),
            Constant::U16(v) => Constant::U8(*v as u8),
            Constant::U32(v) => Constant::U8(*v as u8),
            Constant::U64(v) => Constant::U8(*v as u8),
            Constant::U8(_) => return None,
            _ => return None,
        },
        U16_TYPE => match c {
            Constant::U8(v) => Constant::U16(*v as u16),
            Constant::I8(v) => Constant::U16(*v as u16),
            Constant::I16(v) => Constant::U16(*v as u16),
            Constant::I32(v) => Constant::U16(*v as u16),
            Constant::I64(v) => Constant::U16(*v as u16),
            Constant::U16(_) => return None,
            _ => return None,
        },
        U32_TYPE => match c {
            Constant::U8(v) => Constant::U32(*v as u32),
            Constant::U16(v) => Constant::U32(*v as u32),
            Constant::I8(v) => Constant::U32(*v as u32),
            Constant::I16(v) => Constant::U32(*v as u32),
            Constant::I32(v) => Constant::U32(*v as u32),
            Constant::I64(v) => Constant::U32(*v as u32),
            Constant::U32(_) => return None,
            _ => return None,
        },
        U64_TYPE => match c {
            Constant::U8(v) => Constant::U64(*v as u64),
            Constant::U16(v) => Constant::U64(*v as u64),
            Constant::U32(v) => Constant::U64(*v as u64),
            Constant::I8(v) => Constant::U64(*v as u64),
            Constant::I16(v) => Constant::U64(*v as u64),
            Constant::I32(v) => Constant::U64(*v as u64),
            Constant::I64(v) => Constant::U64(*v as u64),
            Constant::U64(_) => return None,
            _ => return None,
        },
        F64_TYPE => match c {
            Constant::I8(v) => Constant::F64(*v as f64),
            Constant::I16(v) => Constant::F64(*v as f64),
            Constant::I32(v) => Constant::F64(*v as f64),
            Constant::I64(v) => Constant::F64(*v as f64),
            Constant::U8(v) => Constant::F64(*v as f64),
            Constant::U16(v) => Constant::F64(*v as f64),
            Constant::U32(v) => Constant::F64(*v as f64),
            Constant::U64(v) => Constant::F64(*v as f64),
            Constant::F32(v) => Constant::F64(*v as f64),
            Constant::F64(_) => return None,
            _ => return None,
        },
        F32_TYPE => match c {
            Constant::I8(v) => Constant::F32(*v as f32),
            Constant::I16(v) => Constant::F32(*v as f32),
            Constant::I32(v) => Constant::F32(*v as f32),
            Constant::I64(v) => Constant::F32(*v as f32),
            Constant::U8(v) => Constant::F32(*v as f32),
            Constant::U16(v) => Constant::F32(*v as f32),
            Constant::U32(v) => Constant::F32(*v as f32),
            Constant::U64(v) => Constant::F32(*v as f32),
            Constant::F64(v) => Constant::F32(*v as f32),
            Constant::F32(_) => return None,
            _ => return None,
        },
        BOOL_TYPE => match c {
            Constant::I64(v) => Constant::Bool(*v != 0),
            Constant::I32(v) => Constant::Bool(*v != 0),
            Constant::U8(v) => Constant::Bool(*v != 0),
            Constant::Bool(_) => return None,
            _ => return None,
        },
        _ => return None, // non-primitive target type
    };
    Some(Instruction::Assign {
        dst: Place::local(dst),
        value: Operand::Constant(result),
    })
}

// ── Self-assign Elimination ──────────────────────────────────────────

/// Eliminate `_N = Copy(_N)` instructions (assign a local to itself).
/// These can arise from lowering or after other optimizations.
fn eliminate_self_assigns(func: &mut Function) {
    for bb in &mut func.blocks {
        for inst in &mut bb.instructions {
            let is_self = matches!(inst,
                Instruction::Assign { dst, value: Operand::Copy(src) }
                    if dst.projections.is_empty()
                    && src.projections.is_empty()
                    && dst.local == src.local
            );
            if is_self {
                *inst = Instruction::Nop;
            }
        }
    }
}

// ── Algebraic Simplification ──────────────────────────────────────────

/// Simplify BinOp instructions with identity/absorbing elements:
///   x + 0, x - 0, x | 0, x ^ 0, x << 0, x >> 0 → x
///   x * 1, x / 1                                 → x
///   0 + x, 0 | x, 0 ^ x                          → x  (commutative)
///   1 * x                                         → x  (commutative)
///   x * 0, 0 * x                                  → 0  (absorbing)
///   x & 0, 0 & x                                  → 0  (absorbing)
///   x - x                                         → 0
///   x ^ x                                         → 0
fn simplify_algebraic(func: &mut Function) {
    for bb in &mut func.blocks {
        for inst in &mut bb.instructions {
            let simplified = match inst {
                Instruction::BinOp { dst, op, lhs, rhs, .. } => {
                    simplify_binop(*dst, *op, lhs, rhs)
                }
                _ => None,
            };
            if let Some(new_inst) = simplified {
                *inst = new_inst;
            }
        }
    }
}

/// Simplify Cmp instructions with identical operands:
///   x == x → true,  x != x → false
///   x <  x → false, x >  x → false
///   x <= x → true,  x >= x → true
fn simplify_cmp(func: &mut Function) {
    for bb in &mut func.blocks {
        for inst in &mut bb.instructions {
            let simplified = match inst {
                Instruction::Cmp { dst, op, lhs, rhs, .. } => {
                    // Only fire when both operands are the same simple local
                    if let (Operand::Copy(lp), Operand::Copy(rp)) = (lhs, rhs) {
                        if lp.local == rp.local && lp.projections.is_empty() && rp.projections.is_empty() {
                            let result = match op {
                                CmpOp::Eq | CmpOp::Le | CmpOp::Ge => true,
                                CmpOp::Ne | CmpOp::Lt | CmpOp::Gt => false,
                            };
                            Some(Instruction::Assign {
                                dst: Place::local(*dst),
                                value: Operand::Constant(Constant::Bool(result)),
                            })
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            };
            if let Some(new_inst) = simplified {
                *inst = new_inst;
            }
        }
    }
}

fn simplify_binop(dst: LocalId, op: BinOp, lhs: &Operand, rhs: &Operand) -> Option<Instruction> {
    let assign_op = |op: Operand| -> Instruction {
        Instruction::Assign { dst: Place::local(dst), value: op }
    };
    let zero_i64 = || Operand::Constant(Constant::I64(0));

    // Check for x op x patterns (same local, no projections)
    if let (Operand::Copy(lp), Operand::Copy(rp)) = (lhs, rhs) {
        if lp.local == rp.local && lp.projections.is_empty() && rp.projections.is_empty() {
            match op {
                BinOp::Sub | BinOp::SubWrap | BinOp::BitXor => {
                    return Some(assign_op(zero_i64()));
                }
                _ => {}
            }
        }
    }

    // Extract constant value from either side
    let lhs_i64 = match lhs { Operand::Constant(Constant::I64(v)) => Some(*v), _ => None };
    let rhs_i64 = match rhs { Operand::Constant(Constant::I64(v)) => Some(*v), _ => None };

    match op {
        // Additive identity: x + 0 → x, 0 + x → x
        BinOp::Add | BinOp::AddWrap => {
            if rhs_i64 == Some(0) { return Some(assign_op(lhs.clone())); }
            if lhs_i64 == Some(0) { return Some(assign_op(rhs.clone())); }
        }
        // Subtractive identity: x - 0 → x
        BinOp::Sub | BinOp::SubWrap => {
            if rhs_i64 == Some(0) { return Some(assign_op(lhs.clone())); }
        }
        // Multiplicative identity/absorbing: x * 1 → x, 1 * x → x, x * 0 → 0, 0 * x → 0
        BinOp::Mul | BinOp::MulWrap => {
            if rhs_i64 == Some(1) { return Some(assign_op(lhs.clone())); }
            if lhs_i64 == Some(1) { return Some(assign_op(rhs.clone())); }
            if rhs_i64 == Some(0) { return Some(assign_op(zero_i64())); }
            if lhs_i64 == Some(0) { return Some(assign_op(zero_i64())); }
        }
        // Division identity: x / 1 → x
        BinOp::Div => {
            if rhs_i64 == Some(1) { return Some(assign_op(lhs.clone())); }
        }
        // Remainder: x % 1 → 0
        BinOp::Rem | BinOp::Mod => {
            if rhs_i64 == Some(1) { return Some(assign_op(zero_i64())); }
        }
        // Bitwise AND absorbing: x & 0 → 0, 0 & x → 0
        BinOp::BitAnd => {
            if rhs_i64 == Some(0) { return Some(assign_op(zero_i64())); }
            if lhs_i64 == Some(0) { return Some(assign_op(zero_i64())); }
        }
        // Bitwise OR identity: x | 0 → x, 0 | x → x
        BinOp::BitOr => {
            if rhs_i64 == Some(0) { return Some(assign_op(lhs.clone())); }
            if lhs_i64 == Some(0) { return Some(assign_op(rhs.clone())); }
        }
        // Bitwise XOR identity: x ^ 0 → x, 0 ^ x → x
        BinOp::BitXor => {
            if rhs_i64 == Some(0) { return Some(assign_op(lhs.clone())); }
            if lhs_i64 == Some(0) { return Some(assign_op(rhs.clone())); }
        }
        // Shift identity: x << 0 → x, x >> 0 → x
        BinOp::Shl | BinOp::Shr => {
            if rhs_i64 == Some(0) { return Some(assign_op(lhs.clone())); }
        }
        // Power: x ** 1 → x, x ** 0 → 1
        BinOp::Pow => {
            if rhs_i64 == Some(1) { return Some(assign_op(lhs.clone())); }
            if rhs_i64 == Some(0) { return Some(assign_op(Operand::Constant(Constant::I64(1)))); }
        }
    }

    None
}

// ── Common Subexpression Elimination ─────────────────────────────────

/// Within each basic block, if the same pure computation appears twice with
/// the same operands, replace the second with a copy of the first's result.
///
/// Currently handles BinOp and Cmp instructions (the most common patterns).
/// Invalidates all tracked expressions on Call/CallExtern/CallIndirect
/// (which may modify state through borrows) and on assignment to any operand
/// local (which changes the expression's meaning).
fn eliminate_common_subexpressions(func: &mut Function) {
    for bb in &mut func.blocks {
        // Map from (op_kind, operand1, operand2) → destination local
        let mut known: std::collections::HashMap<CseKey, LocalId> = std::collections::HashMap::new();

        for inst in &mut bb.instructions {
            // Check if this instruction matches a known CSE key
            if let Some(key) = cse_key(inst) {
                if let Some(&prev_dst) = known.get(&key) {
                    // Replace with Copy of previous result
                    let dst = cse_dst(inst).unwrap();
                    *inst = Instruction::Assign {
                        dst: Place::local(dst),
                        value: Operand::Copy(Place::local(prev_dst)),
                    };
                    continue;
                } else {
                    // Record this expression
                    if let Some(dst) = cse_dst(inst) {
                        known.insert(key, dst);
                    }
                }
            }

            // Invalidate on calls (may modify locals through borrows)
            match inst {
                Instruction::Call { .. } | Instruction::CallExtern { .. }
                | Instruction::CallIndirect { .. } => {
                    known.clear();
                }
                Instruction::Assign { dst, .. } if dst.projections.is_empty() => {
                    known.retain(|k, _| !k.reads_local(dst.local.0));
                }
                _ => {
                    // If this instruction writes to a local, invalidate any expression
                    // that reads that local
                    if let Some(written) = instruction_dst(inst) {
                        known.retain(|k, _| !k.reads_local(written));
                    }
                }
            }
        }
    }
}

/// A key identifying a pure computation for CSE purposes.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum CseKey {
    BinOp { op: BinOp, lhs: CseOperand, rhs: CseOperand },
    UnOp { op: UnOp, operand: CseOperand },
    Cmp { op: CmpOp, lhs: CseOperand, rhs: CseOperand },
}

impl CseKey {
    fn reads_local(&self, local: u32) -> bool {
        match self {
            CseKey::BinOp { lhs, rhs, .. } | CseKey::Cmp { lhs, rhs, .. } => {
                lhs.is_local(local) || rhs.is_local(local)
            }
            CseKey::UnOp { operand, .. } => operand.is_local(local),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum CseOperand {
    Local(u32),
    Constant(ConstantKey),
}

impl CseOperand {
    fn is_local(&self, id: u32) -> bool {
        matches!(self, CseOperand::Local(l) if *l == id)
    }
}

/// Hashable representation of a Constant for CSE.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ConstantKey {
    Bool(bool),
    I64(i64),
    U64(u64),
    // F64 is not Eq/Hash — skip float CSE
}

fn operand_to_cse(op: &Operand) -> Option<CseOperand> {
    match op {
        Operand::Copy(p) if p.projections.is_empty() => Some(CseOperand::Local(p.local.0)),
        Operand::Constant(Constant::Bool(b)) => Some(CseOperand::Constant(ConstantKey::Bool(*b))),
        Operand::Constant(Constant::I64(v)) => Some(CseOperand::Constant(ConstantKey::I64(*v))),
        Operand::Constant(Constant::U64(v)) => Some(CseOperand::Constant(ConstantKey::U64(*v))),
        _ => None,
    }
}

fn cse_key(inst: &Instruction) -> Option<CseKey> {
    match inst {
        Instruction::BinOp { op, lhs, rhs, .. } => {
            let l = operand_to_cse(lhs)?;
            let r = operand_to_cse(rhs)?;
            Some(CseKey::BinOp { op: *op, lhs: l, rhs: r })
        }
        Instruction::UnOp { op, operand, .. } => {
            let o = operand_to_cse(operand)?;
            Some(CseKey::UnOp { op: *op, operand: o })
        }
        Instruction::Cmp { op, lhs, rhs, .. } => {
            let l = operand_to_cse(lhs)?;
            let r = operand_to_cse(rhs)?;
            Some(CseKey::Cmp { op: *op, lhs: l, rhs: r })
        }
        _ => None,
    }
}

fn cse_dst(inst: &Instruction) -> Option<LocalId> {
    match inst {
        Instruction::BinOp { dst, .. } | Instruction::UnOp { dst, .. }
        | Instruction::Cmp { dst, .. } => Some(*dst),
        _ => None,
    }
}

// ── Strength Reduction ────────────────────────────────────────────────

/// Replace expensive operations with cheaper equivalents:
///   x * 2^n → x << n   (multiplication by power of 2)
///   x / 2^n → x >> n   (unsigned division by power of 2, only for positive constants)
fn reduce_strength(func: &mut Function) {
    for bb in &mut func.blocks {
        for inst in &mut bb.instructions {
            let reduced = match inst {
                // Only reduce MulWrap → Shl. Regular Mul has overflow checking
                // that shift doesn't provide (shift wraps silently).
                Instruction::BinOp { dst, op: BinOp::MulWrap, type_id, lhs, rhs } => {
                    reduce_mul_pow2(*dst, *type_id, lhs, rhs)
                }
                Instruction::BinOp { dst, op: BinOp::Div, type_id, lhs, rhs } => {
                    reduce_div_pow2(*dst, *type_id, lhs, rhs)
                }
                _ => None,
            };
            if let Some(new_inst) = reduced {
                *inst = new_inst;
            }
        }
    }
}

/// x * 2^n → x << n (commutative: check both sides)
fn reduce_mul_pow2(dst: LocalId, type_id: TypeId, lhs: &Operand, rhs: &Operand) -> Option<Instruction> {
    // Don't reduce x*0 or x*1 — algebraic simplification handles those
    if let Some(shift) = pow2_shift(rhs) {
        return Some(Instruction::BinOp {
            dst, op: BinOp::Shl, type_id,
            lhs: lhs.clone(),
            rhs: Operand::Constant(Constant::I64(shift)),
        });
    }
    if let Some(shift) = pow2_shift(lhs) {
        return Some(Instruction::BinOp {
            dst, op: BinOp::Shl, type_id,
            lhs: rhs.clone(),
            rhs: Operand::Constant(Constant::I64(shift)),
        });
    }
    None
}

/// x / 2^n → x >> n (only when divisor is positive power of 2)
/// Note: this is only correct for non-negative x. For signed integers,
/// `(-7) / 4 = -1` but `(-7) >> 2 = -2` (arithmetic shift rounds toward
/// negative infinity). We only apply this when the shift amount is safe
/// and both operands are i64 (Gorget's default integer type).
fn reduce_div_pow2(dst: LocalId, type_id: TypeId, lhs: &Operand, rhs: &Operand) -> Option<Instruction> {
    // Only reduce for constant divisors that are powers of 2 ≥ 2
    // Skip: division semantics differ for negative dividends, so only
    // apply when we can prove the dividend is non-negative (constant ≥ 0).
    if let Some(shift) = pow2_shift(rhs) {
        if let Operand::Constant(Constant::I64(v)) = lhs {
            if *v >= 0 {
                return Some(Instruction::BinOp {
                    dst, op: BinOp::Shr, type_id,
                    lhs: lhs.clone(),
                    rhs: Operand::Constant(Constant::I64(shift)),
                });
            }
        }
    }
    None
}

/// If the operand is a constant power of 2 (≥ 2), return the shift amount.
fn pow2_shift(op: &Operand) -> Option<i64> {
    if let Operand::Constant(Constant::I64(v)) = op {
        let v = *v;
        if v >= 2 && v.count_ones() == 1 {
            return Some(v.trailing_zeros() as i64);
        }
    }
    None
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
            // Switch with no cases → Jump to default
            Some(Terminator::Switch { cases, default, .. }) if cases.is_empty() => {
                Some(Terminator::Jump(*default))
            }
            // Switch where all targets (cases + default) are the same → Jump
            Some(Terminator::Switch { cases, default, .. })
                if !cases.is_empty() && cases.iter().all(|(_, b)| b == default) =>
            {
                Some(Terminator::Jump(*default))
            }
            _ => None,
        };
        if let Some(new_term) = folded {
            bb.terminator = Some(new_term);
        }
    }
}

// ── Nop Elimination ──────────────────────────────────────────────────

/// Remove `Instruction::Nop` entries from all basic blocks.
fn eliminate_nops(func: &mut Function) {
    for bb in &mut func.blocks {
        if !bb.instructions.iter().any(|inst| matches!(inst, Instruction::Nop)) {
            continue;
        }
        // Build keep mask, then filter both instructions and span_map in sync
        let keep: Vec<bool> = bb.instructions.iter()
            .map(|inst| !matches!(inst, Instruction::Nop))
            .collect();

        let mut new_insts = Vec::with_capacity(bb.instructions.len());
        let mut new_spans = Vec::with_capacity(bb.span_map.len());
        for (i, (inst, k)) in bb.instructions.drain(..).zip(keep.iter()).enumerate() {
            if *k {
                new_insts.push(inst);
                if i < bb.span_map.len() {
                    new_spans.push(bb.span_map[i]);
                }
            }
        }
        bb.instructions = new_insts;
        bb.span_map = new_spans;
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

// ── Dead Store Elimination ────────────────────────────────────────────

/// Within each basic block, remove assignments to simple locals (no projections)
/// that are overwritten before being read.  An assignment `_N = expr` is dead if
/// _N is reassigned later in the same block with no intervening read of _N.
///
/// Only operates on `Assign { dst: Place::local(_), value: Constant|Copy }` — we
/// don't remove Calls/BinOps/etc. that might have side effects.
fn eliminate_dead_stores(func: &mut Function) {
    for bb in &mut func.blocks {
        // last_store[local_id] = instruction index of the last Assign to that local
        let mut last_store: std::collections::HashMap<u32, usize> = std::collections::HashMap::new();
        let mut dead_indices: Vec<usize> = Vec::new();

        for (i, inst) in bb.instructions.iter().enumerate() {
            // First: check if this instruction READS any locals
            let reads = collect_read_locals(inst);
            // Any read clears the "pending dead store" for that local
            for r in &reads {
                last_store.remove(r);
            }

            // Then: check if this instruction WRITES a simple local
            match inst {
                Instruction::Assign { dst, value } if dst.projections.is_empty() => {
                    // Only remove stores of constants or copies (no side effects)
                    let is_pure = matches!(value, Operand::Constant(_) | Operand::Copy(_));
                    if is_pure {
                        if let Some(prev_idx) = last_store.insert(dst.local.0, i) {
                            dead_indices.push(prev_idx);
                        }
                    } else {
                        // Move operand — might have side effect (drop of moved value)
                        last_store.insert(dst.local.0, i);
                    }
                }
                // Non-Assign writes (BinOp dst, Call dst, etc.) clear tracking
                _ => {
                    if let Some(written) = instruction_dst(inst) {
                        last_store.remove(&written);
                    }
                }
            }
        }

        // Also check terminator for reads — don't remove stores read by terminators
        if let Some(ref term) = bb.terminator {
            let term_reads = collect_terminator_read_locals(term);
            for r in &term_reads {
                // If this local's last store is pending as dead, un-mark it
                if let Some(idx) = last_store.get(r) {
                    dead_indices.retain(|&d| d != *idx);
                }
            }
        }

        if !dead_indices.is_empty() {
            let dead_set: HashSet<usize> = dead_indices.into_iter().collect();
            let mut idx = 0;
            bb.instructions.retain(|_| {
                let keep = !dead_set.contains(&idx);
                idx += 1;
                keep
            });
        }
    }
}

/// Get the destination local of an instruction (if it writes to a simple local).
fn instruction_dst(inst: &Instruction) -> Option<u32> {
    match inst {
        Instruction::BinOp { dst, .. }
        | Instruction::UnOp { dst, .. }
        | Instruction::Cmp { dst, .. }
        | Instruction::Cast { dst, .. }
        | Instruction::BitCast { dst, .. }
        | Instruction::PtrCast { dst, .. }
        | Instruction::FieldLoad { dst, .. }
        | Instruction::IndexLoad { dst, .. }
        | Instruction::HeapAlloc { dst, .. }
        | Instruction::HeapAllocArray { dst, .. }
        | Instruction::StructInit { dst, .. }
        | Instruction::EnumInit { dst, .. }
        | Instruction::TupleInit { dst, .. }
        | Instruction::TagOf { dst, .. }
        | Instruction::EnumFieldLoad { dst, .. }
        | Instruction::Borrow { dst, .. }
        | Instruction::BorrowMut { dst, .. }
        | Instruction::LoadThreadLocal { dst, .. } => Some(dst.0),
        Instruction::Call { dst: Some(d), .. }
        | Instruction::CallIndirect { dst: Some(d), .. }
        | Instruction::CallExtern { dst: Some(d), .. } => Some(d.0),
        _ => None,
    }
}

/// Collect all locals READ by an instruction (operands, not destinations).
fn collect_read_locals(inst: &Instruction) -> Vec<u32> {
    let mut reads = Vec::new();

    match inst {
        Instruction::Assign { dst, value } => {
            if !dst.projections.is_empty() {
                push_place_reads(&mut reads, dst);
            }
            push_operand_reads(&mut reads, value);
        }
        Instruction::BinOp { lhs, rhs, .. } | Instruction::Cmp { lhs, rhs, .. } => {
            push_operand_reads(&mut reads, lhs);
            push_operand_reads(&mut reads, rhs);
        }
        Instruction::UnOp { operand, .. }
        | Instruction::Cast { value: operand, .. }
        | Instruction::BitCast { value: operand, .. }
        | Instruction::PtrCast { value: operand, .. }
        | Instruction::TagOf { operand, .. } => {
            push_operand_reads(&mut reads, operand);
        }
        Instruction::FieldLoad { base, .. } | Instruction::EnumFieldLoad { base, .. } => {
            push_place_reads(&mut reads, base);
        }
        Instruction::IndexLoad { base, index, .. } => {
            push_place_reads(&mut reads, base);
            push_operand_reads(&mut reads, index);
        }
        Instruction::Call { args, .. } | Instruction::CallExtern { args, .. } => {
            for a in args { push_operand_reads(&mut reads, a); }
        }
        Instruction::CallIndirect { callee, args, .. } => {
            push_operand_reads(&mut reads, callee);
            for a in args { push_operand_reads(&mut reads, a); }
        }
        Instruction::StructInit { fields, .. } | Instruction::EnumInit { fields, .. } => {
            for f in fields { push_operand_reads(&mut reads, f); }
        }
        Instruction::TupleInit { elements, .. } => {
            for e in elements { push_operand_reads(&mut reads, e); }
        }
        Instruction::MoveZero { place } | Instruction::Drop { place }
        | Instruction::DropIfAlive { place } | Instruction::Borrow { place, .. }
        | Instruction::BorrowMut { place, .. } => {
            push_place_reads(&mut reads, place);
        }
        Instruction::HeapAlloc { allocator, .. } => { push_operand_reads(&mut reads, allocator); }
        Instruction::HeapAllocArray { count, allocator, .. } => {
            push_operand_reads(&mut reads, count);
            push_operand_reads(&mut reads, allocator);
        }
        Instruction::Dealloc { ptr, allocator } => {
            push_operand_reads(&mut reads, ptr);
            push_operand_reads(&mut reads, allocator);
        }
        Instruction::PushAllocator { allocator } => { push_operand_reads(&mut reads, allocator); }
        Instruction::PopAllocator | Instruction::Nop
        | Instruction::InlineC { .. } | Instruction::LoadThreadLocal { .. } => {}
    }
    reads
}

fn push_operand_reads(reads: &mut Vec<u32>, op: &Operand) {
    match op {
        Operand::Copy(p) | Operand::Move(p) => push_place_reads(reads, p),
        Operand::Constant(_) => {}
    }
}

fn push_place_reads(reads: &mut Vec<u32>, p: &Place) {
    reads.push(p.local.0);
    for proj in &p.projections {
        if let Projection::Index(id) = proj {
            reads.push(id.0);
        }
    }
}

/// Collect all locals read by a terminator.
fn collect_terminator_read_locals(term: &Terminator) -> Vec<u32> {
    let mut reads = Vec::new();
    let add_op = |op: &Operand, reads: &mut Vec<u32>| {
        match op {
            Operand::Copy(p) | Operand::Move(p) => {
                reads.push(p.local.0);
            }
            Operand::Constant(_) => {}
        }
    };
    match term {
        Terminator::Return(op) => add_op(op, &mut reads),
        Terminator::Branch { cond, .. } => add_op(cond, &mut reads),
        Terminator::Switch { value, .. } => add_op(value, &mut reads),
        Terminator::Invoke { args, .. } => {
            for a in args { add_op(a, &mut reads); }
        }
        Terminator::Jump(_) | Terminator::Unreachable => {}
    }
    reads
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

// ── Block Merging ─────────────────────────────────────────────────────

/// Merge a block into its unique predecessor when:
/// - The predecessor ends with `Jump(target)`
/// - The target has exactly one predecessor (that block)
///
/// This reduces block count and can enable further optimizations within the
/// merged block. Runs after jump threading (which creates merge opportunities)
/// and before dead block elimination (which renumbers).
fn merge_blocks(func: &mut Function) {
    if func.blocks.len() <= 1 {
        return;
    }

    // Count predecessors for each block.
    let n = func.blocks.len();
    let mut pred_count = vec![0u32; n];
    // Block 0 has an implicit entry predecessor.
    pred_count[0] = 1;
    for bb in &func.blocks {
        for succ in successors(bb) {
            if (succ as usize) < n {
                pred_count[succ as usize] += 1;
            }
        }
    }

    // Iteratively merge until no more progress.
    let mut changed = true;
    while changed {
        changed = false;
        for i in 0..func.blocks.len() {
            // Check: does this block jump to a block with exactly one predecessor?
            let target = match &func.blocks[i].terminator {
                Some(Terminator::Jump(BlockId(t))) => *t as usize,
                _ => continue,
            };
            if target >= func.blocks.len() || target == i {
                continue;
            }
            if pred_count[target] != 1 {
                continue;
            }

            // Merge: append target's instructions and take its terminator.
            // We need to drain target first to avoid double-borrow.
            let target_insts = std::mem::take(&mut func.blocks[target].instructions);
            let target_term = func.blocks[target].terminator.take();

            func.blocks[i].instructions.extend(target_insts);
            func.blocks[i].terminator = target_term;

            // The merged block inherits target's successors — update pred_count.
            // target is now dead (no terminator, no instructions) but we don't
            // remove it here — dead block elimination handles that.
            pred_count[target] = 0;
            changed = true;
        }
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
        Instruction::Dealloc { ptr, allocator } => {
            mark_operand(ptr, referenced);
            mark_operand(allocator, referenced);
        }
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
        Instruction::Dealloc { ptr, allocator } => {
            remap_operand(ptr, remap);
            remap_operand(allocator, remap);
        }
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

    #[test]
    fn fold_switch_empty_cases() {
        // Switch with no cases → Jump to default
        let mut f = make_func(vec![
            bb(vec![], Terminator::Switch {
                value: Operand::Copy(Place::local(LocalId(1))),
                cases: vec![],
                default: BlockId(1),
            }),
            bb(vec![], ret_i64(0)),
        ], vec![local(I64_TYPE), local(I64_TYPE)], vec![I64_TYPE]);
        fold_constant_branches(&mut f);
        assert!(matches!(f.blocks[0].terminator, Some(Terminator::Jump(BlockId(1)))));
    }

    #[test]
    fn fold_switch_all_same_target() {
        // Switch where all cases + default go to bb2 → Jump(bb2)
        let mut f = make_func(vec![
            bb(vec![], Terminator::Switch {
                value: Operand::Copy(Place::local(LocalId(1))),
                cases: vec![(1, BlockId(2)), (2, BlockId(2)), (3, BlockId(2))],
                default: BlockId(2),
            }),
            bb(vec![], ret_i64(0)),
            bb(vec![], ret_i64(42)),
        ], vec![local(I64_TYPE), local(I64_TYPE)], vec![I64_TYPE]);
        fold_constant_branches(&mut f);
        assert!(matches!(f.blocks[0].terminator, Some(Terminator::Jump(BlockId(2)))));
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

    // ── Block Merging Tests ──────────────────────────────────────────

    #[test]
    fn merge_simple_chain() {
        // bb0: assign _1=42, Jump(bb1)
        // bb1: return _1
        // → merged into: bb0: assign _1=42, return _1
        let mut f = make_func(vec![
            bb(vec![Instruction::Assign {
                dst: Place::local(LocalId(1)),
                value: Operand::Constant(Constant::I64(42)),
            }], Terminator::Jump(BlockId(1))),
            bb(vec![], ret_local(1)),
        ], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        merge_blocks(&mut f);
        // bb0 should now contain the assign + return
        assert_eq!(f.blocks[0].instructions.len(), 1);
        assert!(matches!(f.blocks[0].terminator, Some(Terminator::Return(_))));
    }

    #[test]
    fn merge_chain_of_three() {
        // bb0: Jump(bb1), bb1: assign _1=10, Jump(bb2), bb2: return _1
        let mut f = make_func(vec![
            bb(vec![], Terminator::Jump(BlockId(1))),
            bb(vec![Instruction::Assign {
                dst: Place::local(LocalId(1)),
                value: Operand::Constant(Constant::I64(10)),
            }], Terminator::Jump(BlockId(2))),
            bb(vec![], ret_local(1)),
        ], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        merge_blocks(&mut f);
        // bb0 should absorb bb1 and bb2
        assert_eq!(f.blocks[0].instructions.len(), 1);
        assert!(matches!(f.blocks[0].terminator, Some(Terminator::Return(_))));
    }

    #[test]
    fn no_merge_multiple_predecessors() {
        // bb0: Branch(cond, bb1, bb1) — bb1 has 2 predecessors, don't merge
        let mut f = make_func(vec![
            bb(vec![], Terminator::Branch {
                cond: Operand::Constant(Constant::Bool(true)),
                then_block: BlockId(1),
                else_block: BlockId(1),
            }),
            bb(vec![], ret_i64(0)),
        ], vec![local(I64_TYPE)], vec![]);
        let orig_term = f.blocks[0].terminator.clone();
        merge_blocks(&mut f);
        // bb0 should keep its Branch (bb1 has 2 predecessors from the Branch)
        assert_eq!(format!("{:?}", f.blocks[0].terminator), format!("{:?}", orig_term));
    }

    // ── Constant Propagation Tests ─────────────────────────────────

    #[test]
    fn propagate_constant_into_binop() {
        // _1 = 10; _2 = BinOp(Add, Copy(_1), Copy(_1))
        // → after propagation: _2 = BinOp(Add, 10, 10)
        // → after folding: _2 = 20
        let mut f = make_func(vec![bb(vec![
            Instruction::Assign {
                dst: Place::local(LocalId(1)),
                value: Operand::Constant(Constant::I64(10)),
            },
            Instruction::BinOp {
                dst: LocalId(2),
                op: BinOp::Add,
                type_id: I64_TYPE,
                lhs: Operand::Copy(Place::local(LocalId(1))),
                rhs: Operand::Copy(Place::local(LocalId(1))),
            },
        ], ret_local(2))], vec![local(I64_TYPE), local(I64_TYPE), local(I64_TYPE)], vec![]);
        propagate_constants(&mut f);
        // After propagation, the BinOp should have constant operands
        match &f.blocks[0].instructions[1] {
            Instruction::BinOp { lhs: Operand::Constant(Constant::I64(10)),
                                 rhs: Operand::Constant(Constant::I64(10)), .. } => {}
            other => panic!("Expected constant operands, got {:?}", other),
        }
        // After folding, it should become an Assign
        constant_fold(&mut f);
        match &f.blocks[0].instructions[1] {
            Instruction::Assign { value: Operand::Constant(Constant::I64(20)), .. } => {}
            other => panic!("Expected folded to 20, got {:?}", other),
        }
    }

    #[test]
    fn propagate_invalidated_by_reassign() {
        // _1 = 10; _1 = Copy(_2); _3 = BinOp(Add, Copy(_1), 1)
        // → _1's constant should NOT propagate into the BinOp
        let mut f = make_func(vec![bb(vec![
            Instruction::Assign {
                dst: Place::local(LocalId(1)),
                value: Operand::Constant(Constant::I64(10)),
            },
            Instruction::Assign {
                dst: Place::local(LocalId(1)),
                value: Operand::Copy(Place::local(LocalId(2))),
            },
            Instruction::BinOp {
                dst: LocalId(3),
                op: BinOp::Add,
                type_id: I64_TYPE,
                lhs: Operand::Copy(Place::local(LocalId(1))),
                rhs: Operand::Constant(Constant::I64(1)),
            },
        ], ret_local(3))], vec![local(I64_TYPE), local(I64_TYPE), local(I64_TYPE), local(I64_TYPE)], vec![]);
        propagate_constants(&mut f);
        // _1 was reassigned from _2 (not constant), so BinOp's lhs should stay Copy(_1)
        match &f.blocks[0].instructions[2] {
            Instruction::BinOp { lhs: Operand::Copy(p), .. } if p.local == LocalId(1) => {}
            other => panic!("Expected Copy(_1), got {:?}", other),
        }
    }

    #[test]
    fn propagate_constant_into_branch_terminator() {
        // _1 = true; br _1, bb1, bb2 → br const true, bb1, bb2
        let mut f = make_func(vec![
            bb(vec![
                Instruction::Assign {
                    dst: Place::local(LocalId(1)),
                    value: Operand::Constant(Constant::Bool(true)),
                },
            ], Terminator::Branch {
                cond: Operand::Copy(Place::local(LocalId(1))),
                then_block: BlockId(1),
                else_block: BlockId(2),
            }),
            bb(vec![], ret_i64(1)),
            bb(vec![], ret_i64(2)),
        ], vec![local(I64_TYPE), local(BOOL_TYPE)], vec![]);
        propagate_constants(&mut f);
        match &f.blocks[0].terminator {
            Some(Terminator::Branch { cond: Operand::Constant(Constant::Bool(true)), .. }) => {}
            other => panic!("Expected constant true in branch, got {:?}", other),
        }
    }

    #[test]
    fn propagate_constant_into_return_terminator() {
        // _1 = 42; return _1 → return const 42
        let mut f = make_func(vec![bb(vec![
            Instruction::Assign {
                dst: Place::local(LocalId(1)),
                value: Operand::Constant(Constant::I64(42)),
            },
        ], ret_local(1))], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        propagate_constants(&mut f);
        match &f.blocks[0].terminator {
            Some(Terminator::Return(Operand::Constant(Constant::I64(42)))) => {}
            other => panic!("Expected return 42, got {:?}", other),
        }
    }

    // ── Dead Store Elimination Tests ────────────────────────────────

    #[test]
    fn elide_dead_store_overwritten() {
        // _1 = 42; _1 = 99; return _1 → first assign removed
        let mut f = make_func(vec![bb(vec![
            Instruction::Assign {
                dst: Place::local(LocalId(1)),
                value: Operand::Constant(Constant::I64(42)),
            },
            Instruction::Assign {
                dst: Place::local(LocalId(1)),
                value: Operand::Constant(Constant::I64(99)),
            },
        ], ret_local(1))], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        eliminate_dead_stores(&mut f);
        assert_eq!(f.blocks[0].instructions.len(), 1);
        match &f.blocks[0].instructions[0] {
            Instruction::Assign { value: Operand::Constant(Constant::I64(99)), .. } => {}
            other => panic!("Expected assign 99, got {:?}", other),
        }
    }

    #[test]
    fn no_elide_store_read_between() {
        // _1 = 42; _2 = Copy(_1); _1 = 99 → keep both assigns
        let mut f = make_func(vec![bb(vec![
            Instruction::Assign {
                dst: Place::local(LocalId(1)),
                value: Operand::Constant(Constant::I64(42)),
            },
            Instruction::Assign {
                dst: Place::local(LocalId(2)),
                value: Operand::Copy(Place::local(LocalId(1))),
            },
            Instruction::Assign {
                dst: Place::local(LocalId(1)),
                value: Operand::Constant(Constant::I64(99)),
            },
        ], ret_local(1))], vec![local(I64_TYPE), local(I64_TYPE), local(I64_TYPE)], vec![]);
        eliminate_dead_stores(&mut f);
        assert_eq!(f.blocks[0].instructions.len(), 3);
    }

    #[test]
    fn no_elide_store_read_by_terminator() {
        // _1 = 42; return _1 → keep (read by terminator)
        let mut f = make_func(vec![bb(vec![
            Instruction::Assign {
                dst: Place::local(LocalId(1)),
                value: Operand::Constant(Constant::I64(42)),
            },
        ], ret_local(1))], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        eliminate_dead_stores(&mut f);
        assert_eq!(f.blocks[0].instructions.len(), 1);
    }

    // ── Block Merging Tests ──────────────────────────────────────────

    // ── Algebraic Simplification Tests ──────────────────────────────

    #[test]
    fn simplify_add_zero_rhs() {
        // _2 = BinOp(Add, Copy(_1), 0) → _2 = Copy(_1)
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(2), op: BinOp::Add, type_id: I64_TYPE,
            lhs: Operand::Copy(Place::local(LocalId(1))),
            rhs: Operand::Constant(Constant::I64(0)),
        }], ret_local(2))], vec![local(I64_TYPE), local(I64_TYPE), local(I64_TYPE)], vec![I64_TYPE]);
        simplify_algebraic(&mut f);
        match &f.blocks[0].instructions[0] {
            Instruction::Assign { value: Operand::Copy(p), .. } if p.local == LocalId(1) => {}
            other => panic!("Expected Copy(_1), got {:?}", other),
        }
    }

    #[test]
    fn simplify_mul_zero() {
        // _2 = BinOp(Mul, Copy(_1), 0) → _2 = 0
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(2), op: BinOp::Mul, type_id: I64_TYPE,
            lhs: Operand::Copy(Place::local(LocalId(1))),
            rhs: Operand::Constant(Constant::I64(0)),
        }], ret_local(2))], vec![local(I64_TYPE), local(I64_TYPE), local(I64_TYPE)], vec![I64_TYPE]);
        simplify_algebraic(&mut f);
        match &f.blocks[0].instructions[0] {
            Instruction::Assign { value: Operand::Constant(Constant::I64(0)), .. } => {}
            other => panic!("Expected 0, got {:?}", other),
        }
    }

    #[test]
    fn simplify_mul_one() {
        // _2 = BinOp(Mul, Copy(_1), 1) → _2 = Copy(_1)
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(2), op: BinOp::Mul, type_id: I64_TYPE,
            lhs: Operand::Copy(Place::local(LocalId(1))),
            rhs: Operand::Constant(Constant::I64(1)),
        }], ret_local(2))], vec![local(I64_TYPE), local(I64_TYPE), local(I64_TYPE)], vec![I64_TYPE]);
        simplify_algebraic(&mut f);
        match &f.blocks[0].instructions[0] {
            Instruction::Assign { value: Operand::Copy(p), .. } if p.local == LocalId(1) => {}
            other => panic!("Expected Copy(_1), got {:?}", other),
        }
    }

    #[test]
    fn simplify_sub_self() {
        // _2 = BinOp(Sub, Copy(_1), Copy(_1)) → _2 = 0
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(2), op: BinOp::Sub, type_id: I64_TYPE,
            lhs: Operand::Copy(Place::local(LocalId(1))),
            rhs: Operand::Copy(Place::local(LocalId(1))),
        }], ret_local(2))], vec![local(I64_TYPE), local(I64_TYPE), local(I64_TYPE)], vec![I64_TYPE]);
        simplify_algebraic(&mut f);
        match &f.blocks[0].instructions[0] {
            Instruction::Assign { value: Operand::Constant(Constant::I64(0)), .. } => {}
            other => panic!("Expected 0, got {:?}", other),
        }
    }

    #[test]
    fn simplify_xor_self() {
        // _2 = BinOp(BitXor, Copy(_1), Copy(_1)) → _2 = 0
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(2), op: BinOp::BitXor, type_id: I64_TYPE,
            lhs: Operand::Copy(Place::local(LocalId(1))),
            rhs: Operand::Copy(Place::local(LocalId(1))),
        }], ret_local(2))], vec![local(I64_TYPE), local(I64_TYPE), local(I64_TYPE)], vec![I64_TYPE]);
        simplify_algebraic(&mut f);
        match &f.blocks[0].instructions[0] {
            Instruction::Assign { value: Operand::Constant(Constant::I64(0)), .. } => {}
            other => panic!("Expected 0, got {:?}", other),
        }
    }

    #[test]
    fn simplify_pow_zero() {
        // _2 = BinOp(Pow, Copy(_1), 0) → _2 = 1
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(2), op: BinOp::Pow, type_id: I64_TYPE,
            lhs: Operand::Copy(Place::local(LocalId(1))),
            rhs: Operand::Constant(Constant::I64(0)),
        }], ret_local(2))], vec![local(I64_TYPE), local(I64_TYPE), local(I64_TYPE)], vec![I64_TYPE]);
        simplify_algebraic(&mut f);
        match &f.blocks[0].instructions[0] {
            Instruction::Assign { value: Operand::Constant(Constant::I64(1)), .. } => {}
            other => panic!("Expected 1, got {:?}", other),
        }
    }

    // ── Comparison Simplification Tests ─────────────────────────────

    #[test]
    fn simplify_cmp_eq_self() {
        // _2 = Cmp(Eq, Copy(_1), Copy(_1)) → _2 = true
        let mut f = make_func(vec![bb(vec![Instruction::Cmp {
            dst: LocalId(2), op: CmpOp::Eq, type_id: I64_TYPE,
            lhs: Operand::Copy(Place::local(LocalId(1))),
            rhs: Operand::Copy(Place::local(LocalId(1))),
        }], ret_local(2))], vec![local(BOOL_TYPE), local(I64_TYPE), local(BOOL_TYPE)], vec![I64_TYPE]);
        simplify_cmp(&mut f);
        match &f.blocks[0].instructions[0] {
            Instruction::Assign { value: Operand::Constant(Constant::Bool(true)), .. } => {}
            other => panic!("Expected true, got {:?}", other),
        }
    }

    #[test]
    fn simplify_cmp_lt_self() {
        // _2 = Cmp(Lt, Copy(_1), Copy(_1)) → _2 = false
        let mut f = make_func(vec![bb(vec![Instruction::Cmp {
            dst: LocalId(2), op: CmpOp::Lt, type_id: I64_TYPE,
            lhs: Operand::Copy(Place::local(LocalId(1))),
            rhs: Operand::Copy(Place::local(LocalId(1))),
        }], ret_local(2))], vec![local(BOOL_TYPE), local(I64_TYPE), local(BOOL_TYPE)], vec![I64_TYPE]);
        simplify_cmp(&mut f);
        match &f.blocks[0].instructions[0] {
            Instruction::Assign { value: Operand::Constant(Constant::Bool(false)), .. } => {}
            other => panic!("Expected false, got {:?}", other),
        }
    }

    #[test]
    fn no_simplify_cmp_different_locals() {
        // _3 = Cmp(Eq, Copy(_1), Copy(_2)) → unchanged
        let mut f = make_func(vec![bb(vec![Instruction::Cmp {
            dst: LocalId(3), op: CmpOp::Eq, type_id: I64_TYPE,
            lhs: Operand::Copy(Place::local(LocalId(1))),
            rhs: Operand::Copy(Place::local(LocalId(2))),
        }], ret_local(3))], vec![local(BOOL_TYPE), local(I64_TYPE), local(I64_TYPE), local(BOOL_TYPE)], vec![I64_TYPE, I64_TYPE]);
        simplify_cmp(&mut f);
        assert!(matches!(&f.blocks[0].instructions[0], Instruction::Cmp { .. }));
    }

    // ── Common Subexpression Elimination Tests ──────────────────────

    #[test]
    fn cse_duplicate_binop() {
        // _2 = BinOp(Add, Copy(_1), 10)
        // _3 = BinOp(Add, Copy(_1), 10)  → _3 = Copy(_2)
        let mut f = make_func(vec![bb(vec![
            Instruction::BinOp {
                dst: LocalId(2), op: BinOp::Add, type_id: I64_TYPE,
                lhs: Operand::Copy(Place::local(LocalId(1))),
                rhs: Operand::Constant(Constant::I64(10)),
            },
            Instruction::BinOp {
                dst: LocalId(3), op: BinOp::Add, type_id: I64_TYPE,
                lhs: Operand::Copy(Place::local(LocalId(1))),
                rhs: Operand::Constant(Constant::I64(10)),
            },
        ], ret_local(3))], vec![local(I64_TYPE); 4], vec![I64_TYPE]);
        eliminate_common_subexpressions(&mut f);
        match &f.blocks[0].instructions[1] {
            Instruction::Assign { value: Operand::Copy(p), .. } if p.local == LocalId(2) => {}
            other => panic!("Expected Copy(_2), got {:?}", other),
        }
    }

    #[test]
    fn cse_invalidated_by_reassign() {
        // _2 = BinOp(Add, Copy(_1), 10)
        // _1 = 99  ← invalidates the expression
        // _3 = BinOp(Add, Copy(_1), 10)  → NOT eliminated
        let mut f = make_func(vec![bb(vec![
            Instruction::BinOp {
                dst: LocalId(2), op: BinOp::Add, type_id: I64_TYPE,
                lhs: Operand::Copy(Place::local(LocalId(1))),
                rhs: Operand::Constant(Constant::I64(10)),
            },
            Instruction::Assign {
                dst: Place::local(LocalId(1)),
                value: Operand::Constant(Constant::I64(99)),
            },
            Instruction::BinOp {
                dst: LocalId(3), op: BinOp::Add, type_id: I64_TYPE,
                lhs: Operand::Copy(Place::local(LocalId(1))),
                rhs: Operand::Constant(Constant::I64(10)),
            },
        ], ret_local(3))], vec![local(I64_TYPE); 4], vec![I64_TYPE]);
        eliminate_common_subexpressions(&mut f);
        // Should remain as BinOp (not eliminated)
        assert!(matches!(&f.blocks[0].instructions[2], Instruction::BinOp { .. }));
    }

    #[test]
    fn cse_different_ops_not_eliminated() {
        // _2 = BinOp(Add, Copy(_1), 10)
        // _3 = BinOp(Mul, Copy(_1), 10)  → different op, not eliminated
        let mut f = make_func(vec![bb(vec![
            Instruction::BinOp {
                dst: LocalId(2), op: BinOp::Add, type_id: I64_TYPE,
                lhs: Operand::Copy(Place::local(LocalId(1))),
                rhs: Operand::Constant(Constant::I64(10)),
            },
            Instruction::BinOp {
                dst: LocalId(3), op: BinOp::Mul, type_id: I64_TYPE,
                lhs: Operand::Copy(Place::local(LocalId(1))),
                rhs: Operand::Constant(Constant::I64(10)),
            },
        ], ret_local(3))], vec![local(I64_TYPE); 4], vec![I64_TYPE]);
        eliminate_common_subexpressions(&mut f);
        assert!(matches!(&f.blocks[0].instructions[1], Instruction::BinOp { op: BinOp::Mul, .. }));
    }

    // ── Strength Reduction Tests ──────────────────────────────────

    #[test]
    fn reduce_mulwrap_by_4() {
        // _2 = MulWrap(Copy(_1), 4) → _2 = Shl(Copy(_1), 2)
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(2), op: BinOp::MulWrap, type_id: I64_TYPE,
            lhs: Operand::Copy(Place::local(LocalId(1))),
            rhs: Operand::Constant(Constant::I64(4)),
        }], ret_local(2))], vec![local(I64_TYPE), local(I64_TYPE), local(I64_TYPE)], vec![I64_TYPE]);
        reduce_strength(&mut f);
        match &f.blocks[0].instructions[0] {
            Instruction::BinOp { op: BinOp::Shl, rhs: Operand::Constant(Constant::I64(2)), .. } => {}
            other => panic!("Expected Shl by 2, got {:?}", other),
        }
    }

    #[test]
    fn reduce_mulwrap_by_2_commutative() {
        // _2 = MulWrap(2, Copy(_1)) → _2 = Shl(Copy(_1), 1)
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(2), op: BinOp::MulWrap, type_id: I64_TYPE,
            lhs: Operand::Constant(Constant::I64(2)),
            rhs: Operand::Copy(Place::local(LocalId(1))),
        }], ret_local(2))], vec![local(I64_TYPE), local(I64_TYPE), local(I64_TYPE)], vec![I64_TYPE]);
        reduce_strength(&mut f);
        match &f.blocks[0].instructions[0] {
            Instruction::BinOp { op: BinOp::Shl, lhs: Operand::Copy(p), rhs: Operand::Constant(Constant::I64(1)), .. }
                if p.local == LocalId(1) => {}
            other => panic!("Expected Shl(Copy(_1), 1), got {:?}", other),
        }
    }

    #[test]
    fn no_reduce_mul_checked() {
        // _2 = Mul(Copy(_1), 4) → unchanged (checked mul must not become shift)
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(2), op: BinOp::Mul, type_id: I64_TYPE,
            lhs: Operand::Copy(Place::local(LocalId(1))),
            rhs: Operand::Constant(Constant::I64(4)),
        }], ret_local(2))], vec![local(I64_TYPE), local(I64_TYPE), local(I64_TYPE)], vec![I64_TYPE]);
        reduce_strength(&mut f);
        match &f.blocks[0].instructions[0] {
            Instruction::BinOp { op: BinOp::Mul, .. } => {}
            other => panic!("Expected unchanged Mul, got {:?}", other),
        }
    }

    #[test]
    fn no_reduce_mulwrap_non_power_of_2() {
        // _2 = MulWrap(Copy(_1), 3) → unchanged (3 is not a power of 2)
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(2), op: BinOp::MulWrap, type_id: I64_TYPE,
            lhs: Operand::Copy(Place::local(LocalId(1))),
            rhs: Operand::Constant(Constant::I64(3)),
        }], ret_local(2))], vec![local(I64_TYPE), local(I64_TYPE), local(I64_TYPE)], vec![I64_TYPE]);
        reduce_strength(&mut f);
        match &f.blocks[0].instructions[0] {
            Instruction::BinOp { op: BinOp::MulWrap, .. } => {}
            other => panic!("Expected unchanged MulWrap, got {:?}", other),
        }
    }

    #[test]
    fn reduce_div_by_8_positive() {
        // _1 = Div(16, 8) → Shr(16, 3) — positive constant dividend
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(1), op: BinOp::Div, type_id: I64_TYPE,
            lhs: Operand::Constant(Constant::I64(16)),
            rhs: Operand::Constant(Constant::I64(8)),
        }], ret_local(1))], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        reduce_strength(&mut f);
        match &f.blocks[0].instructions[0] {
            Instruction::BinOp { op: BinOp::Shr, rhs: Operand::Constant(Constant::I64(3)), .. } => {}
            other => panic!("Expected Shr by 3, got {:?}", other),
        }
    }

    #[test]
    fn no_reduce_div_negative_dividend() {
        // _2 = Div(Copy(_1), 4) → unchanged (dividend sign unknown)
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(2), op: BinOp::Div, type_id: I64_TYPE,
            lhs: Operand::Copy(Place::local(LocalId(1))),
            rhs: Operand::Constant(Constant::I64(4)),
        }], ret_local(2))], vec![local(I64_TYPE), local(I64_TYPE), local(I64_TYPE)], vec![I64_TYPE]);
        reduce_strength(&mut f);
        match &f.blocks[0].instructions[0] {
            Instruction::BinOp { op: BinOp::Div, .. } => {}
            other => panic!("Expected unchanged Div, got {:?}", other),
        }
    }

    #[test]
    fn no_simplify_non_identity() {
        // _2 = BinOp(Add, Copy(_1), 5) → unchanged
        let mut f = make_func(vec![bb(vec![Instruction::BinOp {
            dst: LocalId(2), op: BinOp::Add, type_id: I64_TYPE,
            lhs: Operand::Copy(Place::local(LocalId(1))),
            rhs: Operand::Constant(Constant::I64(5)),
        }], ret_local(2))], vec![local(I64_TYPE), local(I64_TYPE), local(I64_TYPE)], vec![I64_TYPE]);
        simplify_algebraic(&mut f);
        assert!(matches!(&f.blocks[0].instructions[0], Instruction::BinOp { .. }));
    }

    #[test]
    fn no_merge_self_loop() {
        // bb0: Jump(bb0) — self loop, don't merge with self
        let mut f = make_func(vec![
            bb(vec![], Terminator::Jump(BlockId(0))),
        ], vec![local(I64_TYPE)], vec![]);
        merge_blocks(&mut f);
        assert!(matches!(f.blocks[0].terminator, Some(Terminator::Jump(BlockId(0)))));
    }

    // ── Cast Folding Tests ────────────────────────────────────────────

    #[test]
    fn fold_cast_i32_to_i64() {
        let mut f = make_func(vec![bb(vec![Instruction::Cast {
            dst: LocalId(1), target_type: I64_TYPE,
            value: Operand::Constant(Constant::I32(42)),
        }], ret_local(1))], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        constant_fold(&mut f);
        assert!(matches!(&f.blocks[0].instructions[0],
            Instruction::Assign { value: Operand::Constant(Constant::I64(42)), .. }));
    }

    #[test]
    fn fold_cast_i64_to_f64() {
        let mut f = make_func(vec![bb(vec![Instruction::Cast {
            dst: LocalId(1), target_type: F64_TYPE,
            value: Operand::Constant(Constant::I64(7)),
        }], ret_local(1))], vec![local(F64_TYPE), local(F64_TYPE)], vec![]);
        constant_fold(&mut f);
        assert!(matches!(&f.blocks[0].instructions[0],
            Instruction::Assign { value: Operand::Constant(Constant::F64(v)), .. } if *v == 7.0));
    }

    #[test]
    fn fold_cast_f64_to_i32() {
        let mut f = make_func(vec![bb(vec![Instruction::Cast {
            dst: LocalId(1), target_type: I32_TYPE,
            value: Operand::Constant(Constant::F64(3.9)),
        }], ret_local(1))], vec![local(I32_TYPE), local(I32_TYPE)], vec![]);
        constant_fold(&mut f);
        assert!(matches!(&f.blocks[0].instructions[0],
            Instruction::Assign { value: Operand::Constant(Constant::I32(3)), .. }));
    }

    #[test]
    fn fold_cast_bool_to_i64() {
        let mut f = make_func(vec![bb(vec![Instruction::Cast {
            dst: LocalId(1), target_type: I64_TYPE,
            value: Operand::Constant(Constant::Bool(true)),
        }], ret_local(1))], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        constant_fold(&mut f);
        assert!(matches!(&f.blocks[0].instructions[0],
            Instruction::Assign { value: Operand::Constant(Constant::I64(1)), .. }));
    }

    #[test]
    fn fold_cast_identity_unchanged() {
        // Cast i64 → i64 should NOT fire (identity cast)
        let mut f = make_func(vec![bb(vec![Instruction::Cast {
            dst: LocalId(1), target_type: I64_TYPE,
            value: Operand::Constant(Constant::I64(99)),
        }], ret_local(1))], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        constant_fold(&mut f);
        assert!(matches!(&f.blocks[0].instructions[0], Instruction::Cast { .. }));
    }

    #[test]
    fn fold_cast_u8_to_u32() {
        let mut f = make_func(vec![bb(vec![Instruction::Cast {
            dst: LocalId(1), target_type: U32_TYPE,
            value: Operand::Constant(Constant::U8(255)),
        }], ret_local(1))], vec![local(U32_TYPE), local(U32_TYPE)], vec![]);
        constant_fold(&mut f);
        assert!(matches!(&f.blocks[0].instructions[0],
            Instruction::Assign { value: Operand::Constant(Constant::U32(255)), .. }));
    }

    // ── Self-assign Elimination Tests ─────────────────────────────────

    #[test]
    fn self_assign_eliminated() {
        let mut f = make_func(vec![bb(vec![
            Instruction::Assign {
                dst: Place::local(LocalId(1)),
                value: Operand::Copy(Place::local(LocalId(1))),
            },
        ], ret_local(1))], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        eliminate_self_assigns(&mut f);
        assert!(matches!(&f.blocks[0].instructions[0], Instruction::Nop));
    }

    #[test]
    fn non_self_assign_preserved() {
        let mut f = make_func(vec![bb(vec![
            Instruction::Assign {
                dst: Place::local(LocalId(1)),
                value: Operand::Copy(Place::local(LocalId(2))),
            },
        ], ret_local(1))], vec![local(I64_TYPE), local(I64_TYPE), local(I64_TYPE)], vec![]);
        eliminate_self_assigns(&mut f);
        assert!(matches!(&f.blocks[0].instructions[0], Instruction::Assign { .. }));
    }

    #[test]
    fn self_assign_with_projection_preserved() {
        // _1.0 = Copy(_1) is NOT a self-assign (different place)
        let mut f = make_func(vec![bb(vec![
            Instruction::Assign {
                dst: Place::field(LocalId(1), 0),
                value: Operand::Copy(Place::local(LocalId(1))),
            },
        ], ret_local(1))], vec![local(I64_TYPE), local(I64_TYPE)], vec![]);
        eliminate_self_assigns(&mut f);
        assert!(matches!(&f.blocks[0].instructions[0], Instruction::Assign { .. }));
    }
}
