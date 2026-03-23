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

/// How an assignment transfers ownership.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AssignMode {
    /// Bitwise copy — trivial types (int, bool, float, simple structs).
    Copy,
    /// Transfer ownership — source is consumed (zeroed after copy).
    /// Used for temp→variable assignments where the temp is no longer needed.
    Move,
    /// Deep clone — creates an independent copy of resource-type data.
    /// Used for variable→variable assignments where both must remain valid.
    Clone,
    /// Borrow as Ptr — source stays alive, destination is a reference.
    /// Used when a Ptr(T) value is assigned without dereferencing.
    Borrow,
}

/// How a field load handles ownership of the extracted value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FieldLoadMode {
    /// Bitwise copy — trivial field type (int, bool, simple struct).
    Copy,
    /// Consuming extraction — copy value + zero source field.
    /// Used for tuple/enum destructuring of resource-type fields.
    MoveZeroSource,
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
        mode: FieldLoadMode,
        dst: LocalId,
        base: Place,
        field: u32,
    },
    IndexLoad {
        dst: LocalId,
        base: Place,
        index: Operand,
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
    EnumFieldLoad {
        dst: LocalId,
        base: Place,
        variant: String,
        field: u32,
    },

    // -- Calls --
    Call {
        dst: Option<LocalId>,
        func: String,
        args: Vec<Operand>,
    },
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
