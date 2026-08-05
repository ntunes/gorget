use super::types::{BlockId, LocalId, TypeId};

/// A place is an addressable memory location.
#[derive(Debug, Clone, PartialEq)]
pub struct Place {
    pub local: LocalId,
    pub projections: Vec<Projection>,
}

impl Place {
    /// A simple place with no projections.
    pub fn local(id: LocalId) -> Self {
        Self {
            local: id,
            projections: Vec::new(),
        }
    }

    /// A place with a field projection.
    pub fn field(id: LocalId, field: u32) -> Self {
        Self {
            local: id,
            projections: vec![Projection::Field(field)],
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Projection {
    /// `.N` (field index).
    Field(u32),
    /// `[_N]` (dynamic index).
    Index(LocalId),
    /// `*` (pointer dereference).
    Deref,
}

/// An operand (right-hand side of an instruction).
#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    /// Read from place (for Copy types).
    Copy(Place),
    /// Move from place (for Move types).
    Move(Place),
    /// A compile-time constant.
    Constant(Constant),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Bool(bool),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    Str(String),
    Null,
    Unit,
    /// sizeof(type) — emitted as `sizeof(C_type_name)` by the C backend.
    SizeOf(TypeId),
    /// Reference to a named function (for passing functions as Callable arguments).
    FuncRef(String),
    /// Reference to a named global variable (module-level static).
    /// Emitted as the variable name directly in C.
    GlobalRef(String),
    /// Pointer to a named global variable (&global).
    /// Emitted as `&variable_name` in C.
    GlobalRefPtr(String),
}

/// How a value is read at a use site (the *unified* read-mode discipline,
/// per `docs/devbook/13-ownership-in-ir.md`, the unified read-mode vocabulary).
///
/// Replaces the four previously-parallel per-instruction read mode enums
/// (`AssignMode`, `FieldLoadMode`, `IndexLoad.borrow: bool`, `ArgOwnership`)
/// with a single canonical vocabulary. Existing instructions keep their
/// shape — typed views (e.g. `AssignMode = ReadMode`, `IndexLoad.read`)
/// alias into this enum so the validator and the readers share one rule.
///
/// The four cases are mechanical:
/// * `Copy` — bitwise copy. Validator: source must be a trivial type.
/// * `Move` — consume the source (last-use, source becomes dead after).
/// * `Clone` — deep clone via the type's clone fn (Phase A metadata).
/// * `Borrow` — destination is a reference / view; source stays live.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReadMode {
    /// Bitwise copy — trivial types (int, bool, float, simple structs).
    /// Validator: source type MUST have `CopySemantics::Trivial`.
    Copy,
    /// Transfer ownership — source is consumed (zeroed after copy).
    /// Used for temp→variable assignments where the temp is no longer needed.
    /// Validator: source must own AND be at last use.
    Move,
    /// Deep clone — creates an independent copy of resource-type data.
    /// Used for variable→variable assignments where both must remain valid.
    /// Validator: source's type must expose a clone fn (Phase A metadata).
    Clone,
    /// Borrow / view — source stays alive, destination is a reference.
    /// Used when a Ptr(T) value is assigned without dereferencing, when an
    /// `IndexLoad` returns a zero-copy view, etc.
    Borrow,
}

/// Typed view of [`ReadMode`] for the `Assign` instruction (`dst = mode value`).
///
/// Kept as a type alias rather than a wrapper struct because every existing
/// emission and consumer site already uses `AssignMode::{Copy,Move,Clone,Borrow}`
/// with no behaviour distinct from the unified vocabulary; the alias preserves
/// the migration-friendly name while folding the semantics into one source of
/// truth. See `docs/devbook/13-ownership-in-ir.md`, the unified read-mode vocabulary.
pub type AssignMode = ReadMode;

/// Move vs Borrow mode for `EnumFieldLoad` (Snag #34 family).
///
/// `Move` zeros the source's payload field at LIR for resource-type
/// payloads (preventing shallow-copy double-free); used by
/// `emit_pattern_bindings` when the binding takes ownership of the
/// extracted field.
///
/// `Borrow` skips the source-zero step; used by `lower_pattern_condition`
/// to inspect a nested constructor's tag/payload without destroying the
/// source. The subsequent `emit_pattern_bindings` then re-reads from the
/// same (un-zeroed) source. Without this split, the condition test's
/// destructive read zeros the payload and the binding sees zeros — Snag
/// #34's "Dict[String, NonCopyEnum] silently drops mutations" surface
/// symptom.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EnumFieldLoadMode {
    Move,
    Borrow,
}

/// Instructions that don't transfer control flow.
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // -- Memory --
    Assign {
        mode: AssignMode,
        dst: Place,
        value: Operand,
    },
    /// Assign to a module-level static variable (global).
    GlobalAssign {
        name: String,
        value: Operand,
    },
    FieldLoad {
        dst: LocalId,
        base: Place,
        field: u32,
    },
    IndexLoad {
        dst: LocalId,
        base: Place,
        index: Operand,
        /// How the element flows out of the collection. Typed view of
        /// [`ReadMode`] (Phase D5 / `docs/devbook/13-ownership-in-ir.md`):
        /// * `Borrow` — zero-copy view (e.g. `gorget_string_borrow`),
        ///   used by for-loop iteration over string-typed elements.
        /// * `Clone` — deep clone via the element type's clone fn
        ///   (the default). Used everywhere else.
        /// Other variants (`Copy`, `Move`) are reserved; the LIR
        /// currently only routes Borrow vs Clone for collection reads.
        read: ReadMode,
    },

    /// Load a value by dereferencing a Ptr-typed local.
    /// Replaces the implicit auto-deref for unique-borrow params (& or !).
    /// The source local holds a Ptr(T); the result is the T value.
    LoadRef {
        dst: LocalId,
        src: Place,
    },

    /// Store a value through a Ptr-typed local (write-back).
    /// Replaces the implicit Deref-projection write in mutable captures.
    StoreRef {
        dst: Place,
        value: Operand,
    },

    HeapAlloc {
        dst: LocalId,
        type_id: TypeId,
        allocator: Operand,
    },
    HeapAllocArray {
        dst: LocalId,
        type_id: TypeId,
        count: Operand,
        allocator: Operand,
    },
    Dealloc {
        ptr: Operand,
        allocator: Operand,
    },

    // -- Arithmetic / logic --
    BinOp {
        dst: LocalId,
        op: BinOp,
        type_id: TypeId,
        lhs: Operand,
        rhs: Operand,
    },
    /// A faultable arithmetic op inside a fault-`catch` (error-model.md §11.2):
    /// identical to `BinOp` (`op` is one of Add/Sub/Mul/Div/Rem on an integer
    /// type), but on a fault (overflow / div-by-zero) it BRANCHES to
    /// `fault_handler` (a GIR block in the SAME function) instead of panicking.
    /// GIR→LIR lowering splits the block at this inst: emit `Inst::FaultCheck`
    /// producing a flag, terminate with `Term::Branch { flag → handler,
    /// !flag → continuation }`, then compute `dst = lhs op rhs` in the
    /// continuation. A SEPARATE variant (not a field on `BinOp`) so every
    /// existing BinOp site — optimizer, liveness — is untouched and the
    /// fault op is forced through the one shared lowering arm.
    FaultableBinOp {
        dst: LocalId,
        op: BinOp,
        type_id: TypeId,
        lhs: Operand,
        rhs: Operand,
        /// Where an Add/Sub/Mul overflow (or a Div/Rem signed `TYPE_MIN/-1`
        /// overflow) branches. `None` = that fault category is not caught here
        /// (it panics by default). A Div/Rem op may set both this and
        /// `divzero_handler`; Add/Sub/Mul set only this.
        overflow_handler: Option<BlockId>,
        /// Where a Div/Rem divide-by-zero branches. `None` = not caught (panic).
        /// Always `None` for Add/Sub/Mul (they cannot divide by zero).
        divzero_handler: Option<BlockId>,
    },
    /// A faultable array element-READ inside a fault-`catch` (error-model.md
    /// §11, Increment 2 `Fault.Bounds`): identical to `IndexLoad` on an array,
    /// but an out-of-bounds index BRANCHES to `fault_handler` (a GIR block in
    /// the SAME function) instead of panicking. GIR→LIR lowering calls the
    /// non-panicking `gorget_array_safe_get` (returns NULL on OOB), tests the
    /// pointer for NULL, terminates with `Term::Branch { null → handler, else
    /// → continuation }`, then materializes the element in the continuation by
    /// SHARING the `IndexLoad` clone/move-zero/str-ptr logic (NULL is never
    /// deref'd — branch-before-deref, unwind-free). A SEPARATE variant (not a
    /// field on `IndexLoad`) so every existing IndexLoad site — optimizer,
    /// liveness, validate — is untouched and the fault read is forced through
    /// the one shared lowering arm. Array element reads only (the sole path
    /// with a runtime bounds check); dict/string/range index OUT.
    FaultableIndexLoad {
        dst: LocalId,
        base: Place,
        index: Operand,
        read: ReadMode,
        fault_handler: BlockId,
    },
    UnOp {
        dst: LocalId,
        op: UnOp,
        type_id: TypeId,
        operand: Operand,
    },
    Cmp {
        dst: LocalId,
        op: CmpOp,
        type_id: TypeId,
        lhs: Operand,
        rhs: Operand,
    },

    // -- Type conversion --
    Cast {
        dst: LocalId,
        target_type: TypeId,
        value: Operand,
    },
    BitCast {
        dst: LocalId,
        target_type: TypeId,
        value: Operand,
    },
    PtrCast {
        dst: LocalId,
        target_type: TypeId,
        value: Operand,
    },

    // -- Aggregates --
    StructInit {
        dst: LocalId,
        type_name: String,
        fields: Vec<Operand>,
    },
    EnumInit {
        dst: LocalId,
        type_name: String,
        variant: String,
        fields: Vec<Operand>,
    },
    TupleInit {
        dst: LocalId,
        elements: Vec<Operand>,
    },
    TagOf {
        dst: LocalId,
        operand: Operand,
    },
    /// Load a field from an enum variant's data (union access).
    /// C: `type _N = base.data.{variant}._{field};`
    ///
    /// `mode = Move` (default for pattern bindings): zeros the source field
    /// after extraction for resource-type payloads (string / collection thin
    /// pointers), preventing shallow-copy double-free when the caller
    /// subsequently drops either the extracted local or the original enum.
    ///
    /// `mode = Borrow` (pattern-condition tests on nested constructors): does
    /// NOT zero the source field. Required when the same scrutinee will be
    /// read again by `emit_pattern_bindings` after the condition check
    /// completes — without Borrow, the test's destructive read zeros the
    /// source and the subsequent binding sees zeros (Snag #34 family). The
    /// extracted local is a *view* of the source's payload bytes; the caller
    /// must not drop it independently.
    EnumFieldLoad {
        dst: LocalId,
        base: Place,
        variant: String,
        field: u32,
        mode: EnumFieldLoadMode,
    },

    // -- Calls --
    Call {
        dst: Option<LocalId>,
        func: String,
        args: Vec<Operand>,
        /// G3 MaterializeReason: `Some(_)` iff this Call is a clone the compiler
        /// emitted at an ownership boundary (or an explicit `.clone()`), naming
        /// WHICH boundary demanded it. `None` for every ordinary call. This is
        /// the typed field that lets the clone-reason validator identify clones
        /// WITHOUT name-matching the callee (devbook/24 rule 2). Set via
        /// `builder.call_clone`; ordinary `builder.call` leaves it `None`.
        /// GIR-only — dropped (`..`) at GIR→LIR lowering; the backend never
        /// sees it.
        reason: Option<crate::ir::ImplicitCloneReason>,
    },
    /// Reserved for future dynamic dispatch (function pointers, closures).
    /// Currently not emitted by the lowering layer.
    CallIndirect {
        dst: Option<LocalId>,
        callee: Operand,
        args: Vec<Operand>,
    },
    CallExtern {
        dst: Option<LocalId>,
        func: String,
        args: Vec<Operand>,
    },
    /// A direct call to a user function that PARTICIPATES in cross-frame fault
    /// propagation (error-model.md §11, Increment 2.1a/2.1c — arithmetic faults
    /// `Fault.Overflow` + `Fault.DivByZero`), emitted ONLY at a call site inside
    /// an active fault-`catch` scope. Identical to `Call` for
    /// ownership/liveness/optimizer purposes (`dst`/`func`/`args` behave exactly
    /// like `Call`'s), but the callee writes a per-category fault TAG into the
    /// hidden trailing `MutPtr<i32>` slot (`fault_slot`, passed as the LAST element
    /// of `args`) on a fault instead of panicking, and the caller reads the tag
    /// VALUE and DISPATCHES to the matching per-category handler (a GIR block in
    /// the SAME function) AFTER the call. GIR→LIR lowering emits `Inst::Call`, then
    /// loads the slot and, by tag VALUE, branches: `0` → continuation,
    /// `OVERFLOW_TAG` → `overflow_handler`, `DIVZERO_TAG` → `divzero_handler`. The
    /// continuation reads the result. Branch-BEFORE-read ⇒ the sentinel return
    /// value is never consumed on a fault path (mirrors `FaultableIndexLoad`'s
    /// branch-before-deref). The per-category handlers are ALWAYS `Some` at this
    /// instruction (resolved at the call-site gate to the user's catch entry OR
    /// the scope's panic block, so an uncaught-by-this-scope category re-panics
    /// automatically — uniform across both backends, no LIR-level conditional).
    /// A SEPARATE variant (not a field on `Call`) so every existing `Call` site —
    /// optimizer, liveness, validate — is untouched and the fault routing is
    /// forced through the one shared lowering arm. (Bounds adds a third
    /// `bounds_handler` category in 2.1d.)
    FaultableCall {
        dst: Option<LocalId>,
        func: String,
        /// User args FOLLOWED by the hidden trailing fault-slot operand
        /// (`&slot` / `BorrowMut` of the caller's `i32` slot). The callee's
        /// synthesized trailing `MutPtr<i32>` param receives it.
        args: Vec<Operand>,
        /// The caller's `i32` fault slot place — loaded AFTER the call; its tag
        /// VALUE selects the per-category handler. Same place the trailing
        /// `&slot` arg in `args` borrows.
        fault_slot: Place,
        /// GIR block to dispatch to when the slot holds the `Overflow` tag. The
        /// user's `Fault.Overflow` catch entry if this scope catches it, else the
        /// scope's `div_overflow_panic` block (re-panic). Always `Some` for an
        /// emitted catching `FaultableCall`. `block_map`-remapped at GIR→LIR.
        overflow_handler: Option<BlockId>,
        /// GIR block to dispatch to when the slot holds the `DivByZero` tag. The
        /// user's `Fault.DivByZero` catch entry if this scope catches it, else the
        /// scope's `div_zero_panic` block (re-panic). Always `Some` for an emitted
        /// catching `FaultableCall`. `block_map`-remapped at GIR→LIR.
        divzero_handler: Option<BlockId>,
        /// GIR block to dispatch to when the slot holds the `Bounds` tag. The
        /// user's `Fault.Bounds` catch entry if this scope catches it, else the
        /// scope's `bounds_panic` block (re-panic). Always `Some` for an emitted
        /// catching `FaultableCall`. `block_map`-remapped at GIR→LIR (2.1d).
        bounds_handler: Option<BlockId>,
    },

    // -- Ownership --
    MoveZero {
        place: Place,
    },
    Borrow {
        dst: LocalId,
        place: Place,
    },
    BorrowMut {
        dst: LocalId,
        place: Place,
    },
    Drop {
        place: Place,
    },
    DropIfAlive {
        place: Place,
    },

    // -- Allocator --
    LoadThreadLocal {
        dst: LocalId,
        name: String,
    },
    PushAllocator {
        allocator: Operand,
    },
    PopAllocator,

    // -- Inline C code (for patterns that can't be expressed in GIR) --
    InlineC {
        code: String,
    },

    // -- No-op (for source mapping) --
    Nop,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Mod,
    Pow,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    /// Wrapping (modular) addition: cast to unsigned, add, cast back.
    AddWrap,
    /// Wrapping (modular) subtraction.
    SubWrap,
    /// Wrapping (modular) multiplication.
    MulWrap,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOp {
    Neg,
    Not,
    BitNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

/// Terminators end a basic block and transfer control.
#[derive(Debug, Clone, PartialEq)]
pub enum Terminator {
    Return(Operand),
    Jump(BlockId),
    Branch {
        cond: Operand,
        then_block: BlockId,
        else_block: BlockId,
    },
    Switch {
        value: Operand,
        cases: Vec<(i64, BlockId)>,
        default: BlockId,
    },
    Invoke {
        func: String,
        args: Vec<Operand>,
        dst: Option<LocalId>,
        normal: BlockId,
        error: BlockId,
    },
    Unreachable,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::types::{BOOL_TYPE, I64_TYPE};

    #[test]
    fn place_projections() {
        let p = Place {
            local: LocalId(1),
            projections: vec![
                Projection::Field(0),
                Projection::Deref,
                Projection::Index(LocalId(3)),
            ],
        };
        assert_eq!(p.local, LocalId(1));
        assert_eq!(p.projections.len(), 3);
        assert_eq!(p.projections[0], Projection::Field(0));
        assert_eq!(p.projections[1], Projection::Deref);
        assert_eq!(p.projections[2], Projection::Index(LocalId(3)));
    }

    #[test]
    fn operand_variants() {
        let copy = Operand::Copy(Place::local(LocalId(1)));
        let mov = Operand::Move(Place::local(LocalId(2)));
        let c_i64 = Operand::Constant(Constant::I64(42));
        let c_str = Operand::Constant(Constant::Str("hello".into()));
        let c_bool = Operand::Constant(Constant::Bool(true));

        assert!(matches!(copy, Operand::Copy(_)));
        assert!(matches!(mov, Operand::Move(_)));
        assert!(matches!(c_i64, Operand::Constant(Constant::I64(42))));
        assert!(matches!(c_str, Operand::Constant(Constant::Str(_))));
        assert!(matches!(c_bool, Operand::Constant(Constant::Bool(true))));
    }

    #[test]
    fn instruction_variants() {
        // One of each major category — just verify they construct without panic.
        let _assign = Instruction::Assign { mode: crate::ir::instructions::AssignMode::Copy, dst: Place::local(LocalId(1)),
            value: Operand::Constant(Constant::I64(10)),
        };
        let _binop = Instruction::BinOp {
            dst: LocalId(2),
            op: BinOp::Add,
            type_id: I64_TYPE,
            lhs: Operand::Copy(Place::local(LocalId(1))),
            rhs: Operand::Constant(Constant::I64(5)),
        };
        let _call = Instruction::Call {
            dst: Some(LocalId(3)),
            func: "foo".into(),
            args: vec![Operand::Copy(Place::local(LocalId(1)))],
            reason: None,
        };
        let _drop = Instruction::Drop {
            place: Place::local(LocalId(1)),
        };
        let _struct_init = Instruction::StructInit {
            dst: LocalId(4),
            type_name: "Point".into(),
            fields: vec![
                Operand::Constant(Constant::F64(1.0)),
                Operand::Constant(Constant::F64(2.0)),
            ],
        };
        let _cmp = Instruction::Cmp {
            dst: LocalId(5),
            op: CmpOp::Lt,
            type_id: I64_TYPE,
            lhs: Operand::Copy(Place::local(LocalId(1))),
            rhs: Operand::Constant(Constant::I64(100)),
        };
        let _branch = Terminator::Branch {
            cond: Operand::Copy(Place::local(LocalId(5))),
            then_block: BlockId(1),
            else_block: BlockId(2),
        };
        let _ret = Terminator::Return(Operand::Constant(Constant::I32(0)));
        let _switch = Terminator::Switch {
            value: Operand::Copy(Place::local(LocalId(1))),
            cases: vec![(0, BlockId(1)), (1, BlockId(2))],
            default: BlockId(3),
        };
        let _unop = Instruction::UnOp {
            dst: LocalId(6),
            op: UnOp::Neg,
            type_id: I64_TYPE,
            operand: Operand::Copy(Place::local(LocalId(1))),
        };
        let _borrow = Instruction::Borrow {
            dst: LocalId(7),
            place: Place::local(LocalId(1)),
        };
        // If we got here, all variants construct fine.
        assert_eq!(BOOL_TYPE.0, 0); // use BOOL_TYPE to avoid unused warning
    }
}
