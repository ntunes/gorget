//! LIR — Low-Level Intermediate Representation.
//!
//! SSA-form IR between GIR and backends. All implicit operations (drop glue,
//! vtable dispatch, closures, coercions) are explicit here.

pub mod display;
pub mod drop_elab;
mod integration;
pub mod lower;
pub mod optimize;
pub mod ssa;
pub mod types;
pub mod validate;

use std::collections::{HashMap, HashSet};
use std::fmt;

// ── Identity types ──────────────────────────────────────────────────────────

/// SSA value identifier. Unique within a function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueId(pub u32);

/// Stack slot identifier. Unique within a function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SlotId(pub u32);

/// Basic block identifier. Unique within a function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(pub u32);

/// Struct definition identifier. Unique within a module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructId(pub u32);

/// Function identifier. Unique within a module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncId(pub u32);

/// Global variable identifier. Unique within a module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalId(pub u32);

impl fmt::Display for ValueId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

impl fmt::Display for SlotId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "s{}", self.0)
    }
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bb{}", self.0)
    }
}

impl fmt::Display for StructId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "struct.{}", self.0)
    }
}

impl fmt::Display for FuncId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn.{}", self.0)
    }
}

impl fmt::Display for GlobalId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "global.{}", self.0)
    }
}

// ── Types ───────────────────────────────────────────────────────────────────

/// Concrete machine type — no generics, no ownership qualifiers.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LirType {
    // Scalars (SSA values)
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    /// Opaque pointer (like LLVM's `ptr`). Pointed-to type carried by load/store.
    Ptr,
    /// Typed pointer — known to point at a specific struct (e.g. `*GorgetString`).
    /// Semantically identical to `Ptr` at runtime (8 bytes, scalar), but carries
    /// the pointee identity so the C backend can emit correct dereferences.
    PtrTo(StructId),

    // Aggregates (address-only — live in stack slots)
    Struct(StructId),

    // Special
    Void,
}

impl LirType {
    /// True if this type can be an SSA value (fits in a register).
    pub fn is_scalar(&self) -> bool {
        !matches!(self, LirType::Struct(_) | LirType::Void)
    }

    /// True if this is an aggregate that must live in a stack slot.
    pub fn is_aggregate(&self) -> bool {
        matches!(self, LirType::Struct(_))
    }

    /// True if this is an integer type (signed or unsigned).
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            LirType::I8
                | LirType::I16
                | LirType::I32
                | LirType::I64
                | LirType::U8
                | LirType::U16
                | LirType::U32
                | LirType::U64
        )
    }

    /// True if this is a floating-point type.
    pub fn is_float(&self) -> bool {
        matches!(self, LirType::F32 | LirType::F64)
    }

    /// True if this is any pointer type (`Ptr` or `PtrTo`).
    pub fn is_ptr(&self) -> bool {
        matches!(self, LirType::Ptr | LirType::PtrTo(_))
    }

    /// If this is a `PtrTo(sid)`, return the pointee struct id.
    pub fn pointee_struct(&self) -> Option<StructId> {
        if let LirType::PtrTo(sid) = self {
            Some(*sid)
        } else {
            None
        }
    }
}

impl fmt::Display for LirType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LirType::I8 => write!(f, "i8"),
            LirType::I16 => write!(f, "i16"),
            LirType::I32 => write!(f, "i32"),
            LirType::I64 => write!(f, "i64"),
            LirType::U8 => write!(f, "u8"),
            LirType::U16 => write!(f, "u16"),
            LirType::U32 => write!(f, "u32"),
            LirType::U64 => write!(f, "u64"),
            LirType::F32 => write!(f, "f32"),
            LirType::F64 => write!(f, "f64"),
            LirType::Bool => write!(f, "bool"),
            LirType::Ptr => write!(f, "ptr"),
            LirType::PtrTo(id) => write!(f, "ptr.{}", id.0),
            LirType::Struct(id) => write!(f, "{id}"),
            LirType::Void => write!(f, "void"),
        }
    }
}

// ── Overflow semantics ──────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Overflow {
    /// Default: abort on overflow.
    Trap,
    /// Wrapping (modular) arithmetic.
    Wrap,
}

// ── Comparison operators ────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl fmt::Display for CmpOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CmpOp::Eq => write!(f, "eq"),
            CmpOp::Ne => write!(f, "ne"),
            CmpOp::Lt => write!(f, "lt"),
            CmpOp::Le => write!(f, "le"),
            CmpOp::Gt => write!(f, "gt"),
            CmpOp::Ge => write!(f, "ge"),
        }
    }
}

// ── Closure dispatch kind ──────────────────────────────────────────────────

/// How a closure value is laid out in memory for `CallClosure`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClosureDispatchKind {
    /// Callable parameter: `void*[2]` layout (fn_ptr at `[0]`, env at `[1]`).
    /// Originally `__callable_N`.
    CallableParam,
    /// Escaped closure: `GorgetClosure` struct (fn_ptr field 0, env field 1).
    /// Originally `__gorget_closure_call_N`.
    EscapedClosure,
}

// ── Drop guard kind ───────────────────────────────────────────────────────

/// Condition kind for conditional drop guard blocks.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DropGuardKind {
    /// V3 bool flag: guard fires when the bool value is true.
    Bool,
    /// V2 memcmp: guard fires when memory at the value address is non-zero
    /// for `size` bytes.
    NonZero { size: u32 },
}

// ── Instructions ────────────────────────────────────────────────────────────

/// A single LIR instruction. Each produces at most one value (`dst`).
#[derive(Debug, Clone)]
pub enum Inst {
    // ── Slot Access (pre-SSA, lowered by SSA construction) ──────────
    /// Store a value into a stack slot.
    /// `is_move`: when true, the source is being moved (ownership transfer) —
    /// the C backend can use memcpy instead of clone for resource types.
    SlotStore { slot: SlotId, value: ValueId, is_move: bool },
    /// Load a value from a stack slot.
    SlotLoad { dst: ValueId, slot: SlotId, ty: LirType },
    /// Get the address of a stack slot (for aggregates).
    SlotAddr { dst: ValueId, slot: SlotId },

    // ── Constants ───────────────────────────────────────────────────
    IConst { dst: ValueId, ty: LirType, value: i64 },
    FConst { dst: ValueId, ty: LirType, bits: u64 },
    BoolConst { dst: ValueId, value: bool },
    NullPtr { dst: ValueId },
    /// Canonical-op: compile-time size-of a type (bytes).
    ///
    /// Emitted by GIR→LIR lowering in place of eager `IConst { value: sizeof(T) }`
    /// for sites that want to surface "this integer is a sizeof." `lower_lir_to_bir`
    /// resolves each `SizeOf { dst, ty }` into `IConst { dst, ty: I64, value: N }`
    /// by consulting the shared `opaque_runtime_size` / `c_sizeof_lir_type` tables,
    /// so BIR (and therefore backends) never see this instruction.
    ///
    /// Step 3 of the BIR lift plan — see `docs/internals/lir-backend-lift-plan.md`.
    SizeOf { dst: ValueId, ty: LirType },

    /// Canonical-op: initialize an enum variant at the given target address.
    ///
    /// Writes the tag field (`variant_tag`) and, when `payload` is present,
    /// stores that value into the per-variant payload field. `variant_idx`
    /// identifies which payload field to use (field offset = `1 + variant_idx`
    /// for the default flat layout). BIR lowering expands this to the explicit
    /// `FieldPtr`/`Store`/`Memcpy` sequence.
    ///
    /// Step 4 of the BIR lift plan.
    EnumInit {
        target: ValueId,
        struct_id: StructId,
        variant_tag: u32,
        variant_idx: u32,
        payload: Option<ValueId>,
    },

    /// Canonical-op: test whether an enum at the given address holds a specific variant.
    ///
    /// Produces a bool `dst` that is true iff the enum's tag equals `variant_tag`.
    /// BIR lowering expands into `FieldPtr` (tag), `Load`, `IConst`, `Cmp`.
    ///
    /// Step 4 of the BIR lift plan.
    EnumCheck {
        dst: ValueId,
        value: ValueId,
        struct_id: StructId,
        variant_tag: u32,
    },

    /// Canonical-op: load the payload of a specific enum variant.
    ///
    /// Produces `dst` of type `ty` holding the contents of the `payload_field`
    /// slot on the enum at `value`. Callers are responsible for only emitting
    /// this on a value that has been checked with `EnumCheck` (or is known
    /// statically to hold that variant). BIR lowering expands to `FieldPtr`
    /// plus `Load` (or a `Memcpy` into a temp slot, for aggregate payloads).
    ///
    /// Step 4 of the BIR lift plan.
    EnumExtract {
        dst: ValueId,
        value: ValueId,
        struct_id: StructId,
        payload_field: u32,
        ty: LirType,
    },
    FuncAddr { dst: ValueId, func: FuncId },
    /// Address of a function by name (module or extern). Produces a Ptr.
    /// Used to store function pointers in collection structs (elem_drop, elem_clone, etc.).
    NamedFuncAddr { dst: ValueId, name: String },
    GlobalAddr { dst: ValueId, global: GlobalId },
    /// String literal → materialized as Str struct (data ptr + len).
    StrLit { dst: ValueId, value: String },
    /// Reference a function parameter by index.
    ParamRef { dst: ValueId, index: u32, ty: LirType },

    // ── Arithmetic ──────────────────────────────────────────────────
    Add { dst: ValueId, ty: LirType, lhs: ValueId, rhs: ValueId, overflow: Overflow },
    Sub { dst: ValueId, ty: LirType, lhs: ValueId, rhs: ValueId, overflow: Overflow },
    Mul { dst: ValueId, ty: LirType, lhs: ValueId, rhs: ValueId, overflow: Overflow },
    Div { dst: ValueId, ty: LirType, lhs: ValueId, rhs: ValueId },
    Rem { dst: ValueId, ty: LirType, lhs: ValueId, rhs: ValueId },
    Mod { dst: ValueId, ty: LirType, lhs: ValueId, rhs: ValueId },
    Neg { dst: ValueId, ty: LirType, operand: ValueId },

    // ── Bitwise ─────────────────────────────────────────────────────
    BitAnd { dst: ValueId, ty: LirType, lhs: ValueId, rhs: ValueId },
    BitOr { dst: ValueId, ty: LirType, lhs: ValueId, rhs: ValueId },
    BitXor { dst: ValueId, ty: LirType, lhs: ValueId, rhs: ValueId },
    BitNot { dst: ValueId, ty: LirType, operand: ValueId },
    Shl { dst: ValueId, ty: LirType, lhs: ValueId, rhs: ValueId },
    Shr { dst: ValueId, ty: LirType, lhs: ValueId, rhs: ValueId },

    // ── Comparison & Logic ──────────────────────────────────────────
    Cmp { dst: ValueId, op: CmpOp, lhs: ValueId, rhs: ValueId },
    Not { dst: ValueId, operand: ValueId },

    // ── Type Conversions (ALL coercions are explicit) ───────────────
    /// Integer widening/narrowing.
    IntCast { dst: ValueId, value: ValueId, to: LirType },
    /// Float precision change (f32 ↔ f64).
    FloatCast { dst: ValueId, value: ValueId, to: LirType },
    /// Integer → float.
    IntToFloat { dst: ValueId, value: ValueId, to: LirType },
    /// Float → integer.
    FloatToInt { dst: ValueId, value: ValueId, to: LirType },
    /// Pointer reinterpret cast.
    PtrCast { dst: ValueId, value: ValueId },
    /// Same-size reinterpret.
    Bitcast { dst: ValueId, value: ValueId, to: LirType },

    // ── Memory ──────────────────────────────────────────────────────
    /// Load a value from a pointer.
    Load { dst: ValueId, ptr: ValueId, ty: LirType },
    /// Store a value to a pointer.
    Store { ptr: ValueId, value: ValueId },
    /// Get pointer to a struct field: `base + offsetof(struct, field)`.
    FieldPtr { dst: ValueId, base: ValueId, struct_id: StructId, field: u32 },
    /// Get pointer to an array element: `base + index * elem_size`.
    ElemPtr { dst: ValueId, base: ValueId, index: ValueId, elem_size: u32 },
    /// memset.
    Memset { ptr: ValueId, byte: ValueId, size: ValueId },
    /// memcpy.
    Memcpy { dst_ptr: ValueId, src_ptr: ValueId, size: ValueId },

    // ── Calls ───────────────────────────────────────────────────────
    /// Direct call to a known function.
    Call { dst: Option<ValueId>, func: FuncId, args: Vec<ValueId> },
    /// Call to an external (C) function by name.
    /// `original_name` preserves the pre-mapping GIR name (e.g., "Vector__GorgetArray__push")
    /// so the C backend can determine element types for drop function assignment.
    CallExtern { dst: Option<ValueId>, name: String, args: Vec<ValueId>, original_name: Option<String>, arg_abis: Vec<crate::ir::abi::AbiKind> },
    /// Indirect call through a function pointer.
    CallPtr { dst: Option<ValueId>, callee: ValueId, args: Vec<ValueId> },
    /// Indirect call through a closure (fn_ptr + env dispatch).
    /// `kind` distinguishes void*[2] (CallableParam) from GorgetClosure struct (EscapedClosure).
    /// `arg_abis` carries per-arg ABI decisions (deref for non-resource aggregates).
    /// `ret_ty` is explicit so backends don't need to re-derive it.
    CallClosure {
        dst: Option<ValueId>,
        kind: ClosureDispatchKind,
        closure: ValueId,
        args: Vec<ValueId>,
        arg_abis: Vec<crate::ir::abi::AbiKind>,
        ret_ty: LirType,
    },

    // ── Runtime Checks ──────────────────────────────────────────────
    /// Trap if `index >= len`.
    BoundsCheck { index: ValueId, len: ValueId },
    /// Trap if `divisor == 0`.
    DivCheck { divisor: ValueId },
    /// Unconditional abort with message.
    Trap { msg: String },

    // ── Printf (pragmatic high-level instruction) ───────────────────
    /// Backend lowers to platform-appropriate printf.
    Printf { fmt: String, args: Vec<ValueId> },
    /// fprintf to a file descriptor.
    Fprintf { fd: ValueId, fmt: String, args: Vec<ValueId> },

    // ── Backend-specific escape hatch ─────────────────────────────────
    /// Inline C code passthrough. Used for collection field access patterns
    /// that the GIR generates as raw C (e.g., `_x = (int64_t)_y.cap`).
    InlineC { dst: Option<ValueId>, code: String },

    // ── Closures ─────────────────────────────────────────────────────
    /// Pack a closure env + call function into a GorgetClosure slot.
    ///
    /// `env_ptr` is a heap-allocated pointer to the captured environment
    /// (the lowerer emits the malloc + memcpy before this instruction).
    /// `call_func` is the function to call through the closure.
    /// `needs_adapter`: when true, backends emit an `__adapt_` wrapper around
    /// `call_func` (bare function ref → callable coercion). When false,
    /// `call_func` is already a `__Closure_N__call` that takes env directly.
    ///
    /// Semantically: `slot = GorgetClosure { fn_ptr = call_func, env = env_ptr }`.
    ClosurePack { slot: SlotId, env_ptr: ValueId, call_func: FuncId, needs_adapter: bool },

    // ── Drop Guards ──────────────────────────────────────────────────
    /// Open a conditional drop guard block.
    /// Instructions between DropGuardOpen and DropGuardClose are executed only
    /// if the guard condition is true (bool flag or non-zero memory).
    DropGuardOpen { kind: DropGuardKind, value: ValueId },
    /// Close the nearest open drop guard block.
    DropGuardClose,

    // ── Ownership ────────────────────────────────────────────────────
    /// Marks a slot as moved (ownership transferred).  No runtime effect —
    /// pure dataflow annotation consumed by the drop elaboration pass.
    MoveSlot { slot: SlotId },

    // ── No-op (source mapping placeholder) ──────────────────────────
    Nop,
}

impl Inst {
    /// Return the destination ValueId if this instruction defines one.
    pub fn dst(&self) -> Option<ValueId> {
        match self {
            Inst::SlotStore { .. } | Inst::Store { .. } | Inst::Memset { .. }
            | Inst::Memcpy { .. } | Inst::BoundsCheck { .. } | Inst::DivCheck { .. }
            | Inst::Trap { .. } | Inst::Printf { .. } | Inst::Fprintf { .. }
            | Inst::ClosurePack { .. } | Inst::MoveSlot { .. }
            | Inst::DropGuardOpen { .. } | Inst::DropGuardClose | Inst::Nop
            | Inst::EnumInit { .. } => None,
            Inst::InlineC { dst, .. } => *dst,

            Inst::SlotLoad { dst, .. }
            | Inst::SlotAddr { dst, .. }
            | Inst::IConst { dst, .. }
            | Inst::FConst { dst, .. }
            | Inst::BoolConst { dst, .. }
            | Inst::NullPtr { dst }
            | Inst::SizeOf { dst, .. }
            | Inst::EnumCheck { dst, .. }
            | Inst::EnumExtract { dst, .. }
            | Inst::FuncAddr { dst, .. }
            | Inst::NamedFuncAddr { dst, .. }
            | Inst::GlobalAddr { dst, .. }
            | Inst::StrLit { dst, .. }
            | Inst::ParamRef { dst, .. }
            | Inst::Add { dst, .. }
            | Inst::Sub { dst, .. }
            | Inst::Mul { dst, .. }
            | Inst::Div { dst, .. }
            | Inst::Rem { dst, .. }
            | Inst::Mod { dst, .. }
            | Inst::Neg { dst, .. }
            | Inst::BitAnd { dst, .. }
            | Inst::BitOr { dst, .. }
            | Inst::BitXor { dst, .. }
            | Inst::BitNot { dst, .. }
            | Inst::Shl { dst, .. }
            | Inst::Shr { dst, .. }
            | Inst::Cmp { dst, .. }
            | Inst::Not { dst, .. }
            | Inst::IntCast { dst, .. }
            | Inst::FloatCast { dst, .. }
            | Inst::IntToFloat { dst, .. }
            | Inst::FloatToInt { dst, .. }
            | Inst::PtrCast { dst, .. }
            | Inst::Bitcast { dst, .. }
            | Inst::Load { dst, .. }
            | Inst::FieldPtr { dst, .. }
            | Inst::ElemPtr { dst, .. } => Some(*dst),

            Inst::Call { dst, .. }
            | Inst::CallExtern { dst, .. }
            | Inst::CallPtr { dst, .. }
            | Inst::CallClosure { dst, .. } => *dst,
        }
    }

    /// Return all ValueIds used (read) by this instruction.
    pub fn uses(&self) -> Vec<ValueId> {
        match self {
            Inst::SlotStore { value, .. } => vec![*value],
            Inst::ClosurePack { env_ptr, .. } => vec![*env_ptr],
            Inst::SlotLoad { .. } | Inst::SlotAddr { .. } => vec![],
            Inst::IConst { .. } | Inst::FConst { .. } | Inst::BoolConst { .. }
            | Inst::NullPtr { .. } | Inst::FuncAddr { .. } | Inst::NamedFuncAddr { .. }
            | Inst::GlobalAddr { .. }
            | Inst::StrLit { .. } | Inst::ParamRef { .. } | Inst::MoveSlot { .. }
            | Inst::SizeOf { .. }
            | Inst::Nop | Inst::InlineC { .. } => vec![],

            Inst::Add { lhs, rhs, .. }
            | Inst::Sub { lhs, rhs, .. }
            | Inst::Mul { lhs, rhs, .. }
            | Inst::Div { lhs, rhs, .. }
            | Inst::Rem { lhs, rhs, .. }
            | Inst::Mod { lhs, rhs, .. }
            | Inst::BitAnd { lhs, rhs, .. }
            | Inst::BitOr { lhs, rhs, .. }
            | Inst::BitXor { lhs, rhs, .. }
            | Inst::Shl { lhs, rhs, .. }
            | Inst::Shr { lhs, rhs, .. }
            | Inst::Cmp { lhs, rhs, .. } => vec![*lhs, *rhs],

            Inst::Neg { operand, .. }
            | Inst::BitNot { operand, .. }
            | Inst::Not { operand, .. } => vec![*operand],

            Inst::IntCast { value, .. }
            | Inst::FloatCast { value, .. }
            | Inst::IntToFloat { value, .. }
            | Inst::FloatToInt { value, .. }
            | Inst::PtrCast { value, .. }
            | Inst::Bitcast { value, .. } => vec![*value],

            Inst::Load { ptr, .. } => vec![*ptr],
            Inst::Store { ptr, value } => vec![*ptr, *value],
            Inst::FieldPtr { base, .. } => vec![*base],
            Inst::ElemPtr { base, index, .. } => vec![*base, *index],
            Inst::Memset { ptr, byte, size } => vec![*ptr, *byte, *size],
            Inst::Memcpy { dst_ptr, src_ptr, size } => vec![*dst_ptr, *src_ptr, *size],

            Inst::Call { args, .. } => args.clone(),
            Inst::CallExtern { args, .. } => args.clone(),
            Inst::CallPtr { callee, args, .. } => {
                let mut v = vec![*callee];
                v.extend(args);
                v
            }
            Inst::CallClosure { closure, args, .. } => {
                let mut v = vec![*closure];
                v.extend(args);
                v
            }

            Inst::DropGuardOpen { value, .. } => vec![*value],
            Inst::DropGuardClose => vec![],

            Inst::BoundsCheck { index, len } => vec![*index, *len],
            Inst::DivCheck { divisor } => vec![*divisor],
            Inst::Trap { .. } => vec![],
            Inst::Printf { args, .. } => args.clone(),
            Inst::Fprintf { fd, args, .. } => {
                let mut v = vec![*fd];
                v.extend(args);
                v
            }

            Inst::EnumInit { target, payload, .. } => {
                let mut v = vec![*target];
                if let Some(p) = payload { v.push(*p); }
                v
            }
            Inst::EnumCheck { value, .. } => vec![*value],
            Inst::EnumExtract { value, .. } => vec![*value],
        }
    }
}

// ── Terminators ─────────────────────────────────────────────────────────────

/// Block terminator — transfers control flow.
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// Return a value.
    Ret(ValueId),
    /// Return void.
    RetVoid,
    /// Unconditional jump with block arguments.
    Jump(BlockId, Vec<ValueId>),
    /// Conditional branch.
    Branch {
        cond: ValueId,
        then_block: BlockId,
        then_args: Vec<ValueId>,
        else_block: BlockId,
        else_args: Vec<ValueId>,
    },
    /// Multi-way switch on integer value.
    Switch {
        value: ValueId,
        cases: Vec<(i64, BlockId, Vec<ValueId>)>,
        default: BlockId,
        default_args: Vec<ValueId>,
    },
    /// Unreachable (after a trap or noreturn call).
    Unreachable,
}

impl Term {
    /// Return all ValueIds used by this terminator.
    pub fn uses(&self) -> Vec<ValueId> {
        match self {
            Term::Ret(v) => vec![*v],
            Term::RetVoid | Term::Unreachable => vec![],
            Term::Jump(_, args) => args.clone(),
            Term::Branch { cond, then_args, else_args, .. } => {
                let mut v = vec![*cond];
                v.extend(then_args);
                v.extend(else_args);
                v
            }
            Term::Switch { value, cases, default_args, .. } => {
                let mut v = vec![*value];
                for (_, _, args) in cases {
                    v.extend(args);
                }
                v.extend(default_args);
                v
            }
        }
    }

    /// Return all successor block IDs.
    pub fn successors(&self) -> Vec<BlockId> {
        match self {
            Term::Ret(_) | Term::RetVoid | Term::Unreachable => vec![],
            Term::Jump(target, _) => vec![*target],
            Term::Branch { then_block, else_block, .. } => vec![*then_block, *else_block],
            Term::Switch { cases, default, .. } => {
                let mut targets: Vec<BlockId> = cases.iter().map(|(_, b, _)| *b).collect();
                targets.push(*default);
                targets
            }
        }
    }
}

// ── Blocks ──────────────────────────────────────────────────────────────────

/// A basic block with optional parameters (populated by SSA construction).
#[derive(Debug, Clone)]
pub struct Block {
    pub id: BlockId,
    /// Block parameters — empty pre-SSA, populated by SSA construction at merge points.
    pub params: Vec<(ValueId, LirType)>,
    pub insts: Vec<Inst>,
    pub terminator: Term,
}

// ── Slots ───────────────────────────────────────────────────────────────────

/// A named memory slot — the pre-SSA representation of a local variable.
/// SSA construction promotes scalar slots to SSA values + block parameters.
/// Aggregate slots remain as stack allocations.
#[derive(Debug, Clone)]
pub struct Slot {
    pub ty: LirType,
    pub name: Option<String>,
}

// ── Functions ───────────────────────────────────────────────────────────────

/// An LIR function.
#[derive(Debug, Clone)]
pub struct LirFunction {
    pub name: String,
    pub params: Vec<LirType>,
    pub return_type: LirType,
    /// Stack slots for local variables.
    pub slots: Vec<Slot>,
    pub blocks: Vec<Block>,
    /// Next ValueId to allocate.
    next_value: u32,
    /// Whether this function is a test function (needs cleanup stack registration).
    pub is_test_fn: bool,
    /// Human-readable Gorget function name for trace output (e.g. "add", "Point.distance").
    /// None for compiler-generated functions (closures, vtable methods, etc.).
    pub display_name: Option<String>,
    /// Original Gorget parameter names for trace output.
    pub param_names: Vec<Option<String>>,
    /// Which pointer params are const (came from `GirType::Ptr`, i.e. bare borrow, not `&`/`!`).
    pub const_params: Vec<bool>,
    /// Values that are Ptr to GorgetString.
    /// The C backend uses this to deref Ptr(Str) args in printf, CmpOp, and CallExtern.
    pub str_ptr_values: rustc_hash::FxHashSet<ValueId>,
    /// Per-value type metadata, indexed by `ValueId.0`.
    /// Computed once after SSA + optimization; both backends read this
    /// instead of reconstructing types from instructions.
    pub value_types: Vec<Option<LirType>>,
}

impl LirFunction {
    pub fn new(name: String, params: Vec<LirType>, return_type: LirType) -> Self {
        Self {
            name,
            params,
            return_type,
            slots: Vec::new(),
            blocks: Vec::new(),
            next_value: 0,
            is_test_fn: false,
            display_name: None,
            param_names: Vec::new(),
            const_params: Vec::new(),
            str_ptr_values: rustc_hash::FxHashSet::default(),
            value_types: Vec::new(),
        }
    }

    /// Allocate a fresh ValueId.
    pub fn next_value(&mut self) -> ValueId {
        let id = ValueId(self.next_value);
        self.next_value += 1;
        id
    }

    /// The total number of values allocated.
    pub fn value_count(&self) -> u32 {
        self.next_value
    }

    /// Raw access to the ValueId counter — for passes (e.g. BIR lowering)
    /// that need to allocate values while holding a mutable borrow of
    /// `self.blocks`, which would conflict with `self.next_value()`.
    pub fn next_value_raw(&self) -> u32 {
        self.next_value
    }

    /// Write back the ValueId counter after a pass has manually allocated values.
    pub fn set_next_value_raw(&mut self, next: u32) {
        self.next_value = next;
    }

    /// Add a stack slot, returning its SlotId.
    pub fn add_slot(&mut self, ty: LirType, name: Option<String>) -> SlotId {
        let id = SlotId(self.slots.len() as u32);
        self.slots.push(Slot { ty, name });
        id
    }

    /// Add a block, returning its BlockId.
    pub fn add_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len() as u32);
        self.blocks.push(Block {
            id,
            params: Vec::new(),
            insts: Vec::new(),
            terminator: Term::Unreachable, // placeholder
        });
        id
    }

    /// Get a mutable reference to a block.
    pub fn block_mut(&mut self, id: BlockId) -> &mut Block {
        &mut self.blocks[id.0 as usize]
    }

    /// Get an immutable reference to a block.
    pub fn block(&self, id: BlockId) -> &Block {
        &self.blocks[id.0 as usize]
    }
}

// ── Struct Definitions ──────────────────────────────────────────────────────

/// Distinguishes enum kinds for clone/drop code generation.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum EnumKind {
    /// Not an enum — regular struct.
    #[default]
    NotEnum,
    /// Option-like enum: `{tag, Some_0}`. Tag 0 = None, tag != 0 = has payload.
    Option,
    /// Result-like enum: `{tag, Ok_0, Error_0}`. Two payload variants.
    Result,
    /// General user-defined enum with arbitrary variants.
    General,
}

/// A struct type definition (covers structs, enums, tuples, runtime types).
#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<(String, LirType)>,
    /// What kind of enum this struct represents (if any).
    pub enum_kind: EnumKind,
    /// True for large enums (>4 fields) that use C union layout.  Small enums
    /// (Option, Result) use flat struct layout.
    /// The C backend checks this for field access patterns and struct emission.
    pub is_union_layout: bool,
    /// Cached C sizeof for this struct (in bytes). Computed after type lowering
    /// via `compute_struct_sizes()`. Avoids repeated string-based size lookups.
    pub computed_c_size: Option<usize>,
    /// Cached C alignment for this struct (in bytes). Max of field alignments,
    /// capped at 8. Used by the LLVM backend for inter-field padding.
    pub computed_c_align: Option<usize>,
}

impl StructDef {
    /// Create a regular (non-enum) struct definition.
    pub fn new(name: String, fields: Vec<(String, LirType)>) -> Self {
        Self { name, fields, enum_kind: EnumKind::NotEnum, is_union_layout: false, computed_c_size: None, computed_c_align: None }
    }
    /// Create an enum struct definition with the given kind.
    pub fn new_enum(name: String, fields: Vec<(String, LirType)>, kind: EnumKind) -> Self {
        Self { name, fields, enum_kind: kind, is_union_layout: false, computed_c_size: None, computed_c_align: None }
    }
    /// True when the type originated from any enum definition.
    pub fn is_enum(&self) -> bool {
        self.enum_kind != EnumKind::NotEnum
    }
}

// ── Globals ─────────────────────────────────────────────────────────────────

/// A global variable.
#[derive(Debug, Clone)]
pub struct LirGlobal {
    pub name: String,
    pub ty: LirType,
    pub init: LirGlobalInit,
    pub is_const: bool,
}

/// Global variable initializer.
#[derive(Debug, Clone)]
pub enum LirGlobalInit {
    /// Zero-initialized.
    Zeroed,
    /// Raw byte data.
    Bytes(Vec<u8>),
    /// Address of a function.
    FuncAddr(FuncId),
    /// Struct aggregate initializer.
    Struct {
        struct_id: StructId,
        fields: Vec<LirGlobalInit>,
    },
    /// Runtime call expression — must be evaluated before main.
    /// The string is a C expression like `gorget_dict_new_str(sizeof(int64_t))`.
    RuntimeCall(String),
}

// ── Externs ─────────────────────────────────────────────────────────────────

/// An external function declaration.
#[derive(Debug, Clone)]
pub struct LirExtern {
    pub name: String,
    pub params: Vec<LirType>,
    pub return_type: LirType,
    pub is_variadic: bool,
    /// Per-parameter ABI marshalling kind. Empty = all Auto.
    pub param_abis: Vec<crate::ir::abi::AbiKind>,
    /// Return value ABI marshalling kind. Auto = no conversion.
    pub return_abi: crate::ir::abi::AbiKind,
}

// ── Module ──────────────────────────────────────────────────────────────────

/// Metadata for a spawned (async) function.
#[derive(Debug, Clone)]
pub struct SpawnedFn {
    /// Name of the function to spawn (e.g., "compute").
    pub fn_name: String,
    /// Parameter names and their C type names (e.g., [("data", "GorgetArray")]).
    pub params: Vec<(String, String)>,
    /// C type name for the return type (e.g., "int64_t"), or "void".
    pub ret_c_type: String,
    /// Whether any parameter is passed by mutable reference (&) in the actual function.
    pub ref_param_indices: Vec<usize>,
    /// Indices of parameters that are refcounted and need cloning when captured into spawn context.
    /// Each entry is (param_index, original_gir_type_name) e.g. (0, "Channel__int64_t").
    pub clone_params: Vec<(usize, String)>,
}

/// Metadata for a test function, mirrored from GIR's TestFnInfo.
#[derive(Debug, Clone)]
pub struct LirTestFn {
    pub fn_name: String,
    pub display_name: String,
    pub should_panic: bool,
    pub expected_panic_msg: Option<String>,
    pub skipped: bool,
    pub skip_reason: Option<String>,
    pub timeout_ms: Option<u64>,
}

/// Metadata for a benchmark function, mirrored from GIR's BenchFnInfo.
#[derive(Debug, Clone)]
pub struct LirBenchFn {
    pub fn_name: String,
    pub display_name: String,
}

/// Metadata for a thread-spawned function (std.thread).
#[derive(Debug, Clone)]
pub struct ThreadSpawnedFn {
    /// Name of the function to spawn.
    pub fn_name: String,
    /// C type name for the return type (e.g., "int64_t"), or "void".
    pub ret_c_type: String,
}

pub struct LirModule {
    pub structs: Vec<StructDef>,
    pub globals: Vec<LirGlobal>,
    pub functions: Vec<LirFunction>,
    pub externs: Vec<LirExtern>,
    pub source_filename: Option<String>,
    /// Spawned functions metadata for generating spawn/await helpers.
    pub spawned_fns: Vec<SpawnedFn>,
    /// Thread-spawned functions metadata for generating thread spawn/join helpers.
    pub thread_spawned_fns: Vec<ThreadSpawnedFn>,
    /// Test functions (for test harness generation).
    pub test_fns: Vec<LirTestFn>,
    /// Benchmark functions (for bench harness generation).
    pub bench_fns: Vec<LirBenchFn>,
    /// Whether a suite_setup function exists.
    pub has_suite_setup: bool,
    /// Whether a suite_teardown function exists.
    pub has_suite_teardown: bool,
    /// Scheduler mode (pool, thread, inline, single).
    pub scheduler_mode: crate::ir::SchedulerMode,
    /// Trace output filename (set by --trace flag).
    pub trace_filename: Option<String>,
    /// Whether this is a test module (affects panic handler and test runner).
    pub is_test_module: bool,
    /// Whether this module uses hot-reload mode.
    pub hot_reload: bool,
    /// The state type name for hot-reload (defaults to "State").
    pub hot_reload_state_type: Option<String>,
    /// Hash of the state type layout for hot-reload ABI compatibility.
    pub hot_reload_state_hash: u64,
    /// Whether the module defines a `reload()` function.
    pub hot_reload_has_reload_fn: bool,
    /// Recursive drop structs: type_name → Vec<(field_name, drop_fn_name)>.
    /// Populated during LIR lowering for structs that have `Recursive` drop strategy
    /// but no user-defined `{Name}__drop` function.
    pub recursive_drop_structs: HashMap<String, Vec<(String, String, String)>>,
    /// Recursive drop enums: type_name → Vec<(variant_index, variant_name, field_name, drop_fn_name, field_type_name)>.
    /// Used for tag-based clone/drop dispatch on enum types with resource variant payloads.
    pub recursive_drop_enums: HashMap<String, Vec<(u32, String, String, String, String)>>,
    /// Types whose `{Name}__drop` name collides with a user-defined method.
    /// When dropping fields of these types, the backend must inline sub-field drops
    /// instead of calling `{Name}__drop`.
    pub drop_collision_types: HashSet<String>,
    /// Unified drop function info for all types with droppable fields.
    /// Maps type name → drop function specification. The C backend generates
    /// one `Type__drop(void*)` per entry. Scope-exit emits a single call.
    pub type_drop_fns: HashMap<String, TypeDropInfo>,
    /// Target environment: "native" (default), "freestanding".
    /// Affects which runtime is emitted by the C backend.
    pub target: String,
}

/// Specification for a generated `Type__drop` function.
#[derive(Debug, Clone)]
pub struct TypeDropInfo {
    /// The C function name (usually "Type__drop", mangled for collisions).
    pub drop_fn_name: String,
    /// Struct fields to drop: (field_name, drop_fn_name, field_type_name).
    pub field_drops: Vec<(String, String, String)>,
    /// For Custom-drop types: user's drop function to call BEFORE field drops.
    pub user_drop_fn: Option<String>,
    /// For enum types: variant dispatch (tag, variant_name, field_name, drop_fn, field_type_name).
    pub enum_variants: Option<Vec<(u32, String, String, String, String)>>,
}

impl LirModule {
    pub fn new() -> Self {
        Self {
            structs: Vec::new(),
            globals: Vec::new(),
            functions: Vec::new(),
            externs: Vec::new(),
            source_filename: None,
            spawned_fns: Vec::new(),
            thread_spawned_fns: Vec::new(),
            test_fns: Vec::new(),
            bench_fns: Vec::new(),
            has_suite_setup: false,
            has_suite_teardown: false,
            scheduler_mode: crate::ir::SchedulerMode::Pool,
            trace_filename: None,
            is_test_module: false,
            hot_reload: false,
            hot_reload_state_type: None,
            hot_reload_state_hash: 0,
            hot_reload_has_reload_fn: false,
            recursive_drop_structs: HashMap::new(),
            recursive_drop_enums: HashMap::new(),
            drop_collision_types: HashSet::new(),
            type_drop_fns: HashMap::new(),
            target: "native".to_string(),
        }
    }

    /// Add a struct definition, returning its StructId.
    pub fn add_struct(&mut self, def: StructDef) -> StructId {
        let id = StructId(self.structs.len() as u32);
        self.structs.push(def);
        id
    }

    /// Add a global variable, returning its GlobalId.
    pub fn add_global(&mut self, global: LirGlobal) -> GlobalId {
        let id = GlobalId(self.globals.len() as u32);
        self.globals.push(global);
        id
    }

    /// Add a function, returning its FuncId.
    pub fn add_function(&mut self, func: LirFunction) -> FuncId {
        let id = FuncId(self.functions.len() as u32);
        self.functions.push(func);
        id
    }

    /// Add an extern declaration.
    pub fn add_extern(&mut self, ext: LirExtern) {
        self.externs.push(ext);
    }

    /// Compute and cache the C sizeof for every struct definition.
    /// Call once after all struct types have been registered and fields populated.
    /// Uses `c_sizeof_struct_def` for proper enum union layout handling.
    pub fn compute_struct_sizes(&mut self) {
        // Need to compute sizes in dependency order. Since structs can reference
        // other structs via fields, we iterate until all sizes are computed.
        // In practice, most structs only reference primitives or already-sized types.
        let max_iters = self.structs.len() + 1;
        for _ in 0..max_iters {
            let mut progress = false;
            for i in 0..self.structs.len() {
                if self.structs[i].computed_c_size.is_some() {
                    continue;
                }
                // Try to compute — may fail if a referenced struct is not yet sized.
                // c_sizeof_struct_def reads other structs' sizes from their fields,
                // not from computed_c_size, so this always works.
                let size = lower::types::c_sizeof_struct_def(&self.structs[i], &self.structs);
                let align = lower::types::c_alignof_lir_type(&LirType::Struct(StructId(i as u32)), &self.structs);
                self.structs[i].computed_c_size = Some(size);
                self.structs[i].computed_c_align = Some(align);
                progress = true;
            }
            if !progress { break; }
        }
    }

    /// Look up a struct definition by StructId.
    pub fn struct_def(&self, id: StructId) -> &StructDef {
        &self.structs[id.0 as usize]
    }

    /// Look up a function by FuncId.
    pub fn function(&self, id: FuncId) -> &LirFunction {
        &self.functions[id.0 as usize]
    }

    /// Look up a function mutably by FuncId.
    pub fn function_mut(&mut self, id: FuncId) -> &mut LirFunction {
        &mut self.functions[id.0 as usize]
    }

    /// Find a function by name.
    pub fn find_function(&self, name: &str) -> Option<FuncId> {
        self.functions
            .iter()
            .position(|f| f.name == name)
            .map(|i| FuncId(i as u32))
    }
}

impl Default for LirModule {
    fn default() -> Self {
        Self::new()
    }
}

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_minimal_function() {
        let mut module = LirModule::new();

        // fn main() -> i32
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

        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].blocks.len(), 1);
        assert_eq!(module.functions[0].blocks[0].insts.len(), 1);
    }

    #[test]
    fn build_branch() {
        let mut func = LirFunction::new("test_branch".into(), vec![LirType::Bool], LirType::I64);

        let bb0 = func.add_block();
        let bb_then = func.add_block();
        let bb_else = func.add_block();

        let v_cond = func.next_value();
        let v_one = func.next_value();
        let v_two = func.next_value();

        func.block_mut(bb0).insts.push(Inst::BoolConst {
            dst: v_cond,
            value: true,
        });
        func.block_mut(bb0).terminator = Term::Branch {
            cond: v_cond,
            then_block: bb_then,
            then_args: vec![],
            else_block: bb_else,
            else_args: vec![],
        };

        func.block_mut(bb_then).insts.push(Inst::IConst {
            dst: v_one,
            ty: LirType::I64,
            value: 1,
        });
        func.block_mut(bb_then).terminator = Term::Ret(v_one);

        func.block_mut(bb_else).insts.push(Inst::IConst {
            dst: v_two,
            ty: LirType::I64,
            value: 2,
        });
        func.block_mut(bb_else).terminator = Term::Ret(v_two);

        assert_eq!(func.blocks.len(), 3);
        assert_eq!(
            func.block(bb0).terminator.successors(),
            vec![bb_then, bb_else]
        );
    }

    #[test]
    fn slot_operations() {
        let mut func = LirFunction::new("test_slots".into(), vec![], LirType::I64);

        let slot = func.add_slot(LirType::I64, Some("x".into()));
        assert_eq!(slot, SlotId(0));

        let bb0 = func.add_block();
        let v0 = func.next_value();
        let v1 = func.next_value();

        func.block_mut(bb0).insts.push(Inst::IConst {
            dst: v0,
            ty: LirType::I64,
            value: 42,
        });
        func.block_mut(bb0).insts.push(Inst::SlotStore {
            slot,
            value: v0,
            is_move: false,
        });
        func.block_mut(bb0).insts.push(Inst::SlotLoad {
            dst: v1,
            slot,
            ty: LirType::I64,
        });
        func.block_mut(bb0).terminator = Term::Ret(v1);

        assert_eq!(func.slots.len(), 1);
        assert_eq!(func.slots[0].ty, LirType::I64);
        assert_eq!(func.value_count(), 2);
    }

    #[test]
    fn struct_def_and_field_ptr() {
        let mut module = LirModule::new();

        let point_id = module.add_struct(StructDef {
            name: "Point".into(),
            fields: vec![
                ("x".into(), LirType::F64),
                ("y".into(), LirType::F64),
            ],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None,
                      });

        let mut func = LirFunction::new("get_x".into(), vec![LirType::Ptr], LirType::F64);
        let bb0 = func.add_block();

        let v_ptr = func.next_value();
        let v_field = func.next_value();
        let v_val = func.next_value();

        // Simulate: param ptr is v_ptr (slot 0)
        let slot = func.add_slot(LirType::Ptr, Some("p".into()));
        func.block_mut(bb0).insts.push(Inst::SlotLoad {
            dst: v_ptr,
            slot,
            ty: LirType::Ptr,
        });
        func.block_mut(bb0).insts.push(Inst::FieldPtr {
            dst: v_field,
            base: v_ptr,
            struct_id: point_id,
            field: 0,
        });
        func.block_mut(bb0).insts.push(Inst::Load {
            dst: v_val,
            ptr: v_field,
            ty: LirType::F64,
        });
        func.block_mut(bb0).terminator = Term::Ret(v_val);

        module.add_function(func);

        assert_eq!(module.structs.len(), 1);
        assert_eq!(module.struct_def(point_id).fields.len(), 2);
    }

    #[test]
    fn inst_dst_and_uses() {
        let v0 = ValueId(0);
        let v1 = ValueId(1);
        let v2 = ValueId(2);

        let add = Inst::Add {
            dst: v2,
            ty: LirType::I64,
            lhs: v0,
            rhs: v1,
            overflow: Overflow::Trap,
        };
        assert_eq!(add.dst(), Some(v2));
        assert_eq!(add.uses(), vec![v0, v1]);

        let store = Inst::SlotStore {
            slot: SlotId(0),
            value: v0,
            is_move: false,
        };
        assert_eq!(store.dst(), None);
        assert_eq!(store.uses(), vec![v0]);

        let nop = Inst::Nop;
        assert_eq!(nop.dst(), None);
        assert!(nop.uses().is_empty());
    }

    #[test]
    fn type_classification() {
        assert!(LirType::I64.is_scalar());
        assert!(LirType::Ptr.is_scalar());
        assert!(LirType::Bool.is_scalar());
        assert!(!LirType::Struct(StructId(0)).is_scalar());
        assert!(!LirType::Void.is_scalar());

        assert!(LirType::Struct(StructId(0)).is_aggregate());
        assert!(!LirType::I64.is_aggregate());

        assert!(LirType::I32.is_integer());
        assert!(LirType::U64.is_integer());
        assert!(!LirType::F64.is_integer());

        assert!(LirType::F64.is_float());
        assert!(!LirType::I64.is_float());
    }

    #[test]
    fn term_successors() {
        let ret = Term::Ret(ValueId(0));
        assert!(ret.successors().is_empty());

        let jump = Term::Jump(BlockId(1), vec![]);
        assert_eq!(jump.successors(), vec![BlockId(1)]);

        let branch = Term::Branch {
            cond: ValueId(0),
            then_block: BlockId(1),
            then_args: vec![],
            else_block: BlockId(2),
            else_args: vec![],
        };
        assert_eq!(branch.successors(), vec![BlockId(1), BlockId(2)]);

        let switch = Term::Switch {
            value: ValueId(0),
            cases: vec![(0, BlockId(1), vec![]), (1, BlockId(2), vec![])],
            default: BlockId(3),
            default_args: vec![],
        };
        assert_eq!(
            switch.successors(),
            vec![BlockId(1), BlockId(2), BlockId(3)]
        );
    }

    #[test]
    fn module_lookup() {
        let mut module = LirModule::new();
        let f1 = module.add_function(LirFunction::new("foo".into(), vec![], LirType::Void));
        let f2 = module.add_function(LirFunction::new("bar".into(), vec![], LirType::I32));

        assert_eq!(module.find_function("foo"), Some(f1));
        assert_eq!(module.find_function("bar"), Some(f2));
        assert_eq!(module.find_function("baz"), None);
    }

    #[test]
    fn global_init_variants() {
        let mut module = LirModule::new();

        module.add_global(LirGlobal {
            name: "counter".into(),
            ty: LirType::I64,
            init: LirGlobalInit::Zeroed,
            is_const: false,
        });

        let fid = module.add_function(LirFunction::new("handler".into(), vec![], LirType::Void));
        module.add_global(LirGlobal {
            name: "callback".into(),
            ty: LirType::Ptr,
            init: LirGlobalInit::FuncAddr(fid),
            is_const: true,
        });

        assert_eq!(module.globals.len(), 2);
        assert!(!module.globals[0].is_const);
        assert!(module.globals[1].is_const);
    }
}
