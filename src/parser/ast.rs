use crate::span::{Span, Spanned};

// ══════════════════════════════════════════════════════════════
// Top-Level
// ══════════════════════════════════════════════════════════════

/// A complete source file.
#[derive(Debug, Clone)]
pub struct Module {
    pub items: Vec<Spanned<Item>>,
    pub span: Span,
}

/// A top-level item in a module.
#[derive(Debug, Clone)]
pub enum Item {
    Function(FunctionDef),
    Struct(StructDef),
    Enum(EnumDef),
    Trait(TraitDef),
    Equip(EquipBlock),
    Import(ImportStmt),
    TypeAlias(TypeAlias),
    Newtype(NewtypeDef),
    ConstDecl(ConstDecl),
    StaticDecl(StaticDecl),
    ExternBlock(ExternBlock),
    Directive(Directive),
    Test(TestDef),
    SuiteSetup(SuiteSetup),
    SuiteTeardown(SuiteTeardown),
}

// ══════════════════════════════════════════════════════════════
// Directives
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct Directive {
    pub name: String,
    pub value: Option<String>,
    pub span: Span,
}

// ══════════════════════════════════════════════════════════════
// Test Definitions
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct TestDef {
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: Spanned<String>,
    pub with_bindings: Vec<WithBinding>,
    pub body: Block,
    pub doc_comment: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SuiteSetup {
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SuiteTeardown {
    pub body: Block,
    pub span: Span,
}

// ══════════════════════════════════════════════════════════════
// Functions
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub attributes: Vec<Spanned<Attribute>>,
    pub visibility: Visibility,
    pub qualifiers: FunctionQualifiers,
    pub return_type: Spanned<Type>,
    pub name: Spanned<String>,
    pub generic_params: Option<Spanned<GenericParams>>,
    pub params: Vec<Spanned<Param>>,
    pub throws: Option<Spanned<Type>>,
    pub where_clause: Option<Spanned<WhereClause>>,
    pub body: FunctionBody,
    pub doc_comment: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone, Default)]
pub struct FunctionQualifiers {
    pub is_async: bool,
    pub is_const: bool,
    pub is_static: bool,
    pub is_unsafe: bool,
}

#[derive(Debug, Clone)]
pub enum FunctionBody {
    Block(Block),
    Expression(Box<Spanned<Expr>>),
    Declaration,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub type_: Spanned<Type>,
    pub ownership: Ownership,
    pub name: Spanned<String>,
    pub default: Option<Spanned<Expr>>,
    pub is_live: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ownership {
    Borrow,
    MutableBorrow,
    Move,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Private,
    Public,
}

// ══════════════════════════════════════════════════════════════
// Structs
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct StructDef {
    pub attributes: Vec<Spanned<Attribute>>,
    pub visibility: Visibility,
    pub name: Spanned<String>,
    pub generic_params: Option<Spanned<GenericParams>>,
    pub fields: Vec<Spanned<FieldDef>>,
    pub doc_comment: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldDef {
    pub visibility: Visibility,
    pub type_: Spanned<Type>,
    pub name: Spanned<String>,
}

// ══════════════════════════════════════════════════════════════
// Enums
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub attributes: Vec<Spanned<Attribute>>,
    pub visibility: Visibility,
    pub name: Spanned<String>,
    pub generic_params: Option<Spanned<GenericParams>>,
    pub variants: Vec<Spanned<Variant>>,
    pub doc_comment: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub name: Spanned<String>,
    pub fields: VariantFields,
}

#[derive(Debug, Clone)]
pub enum VariantFields {
    Unit,
    Tuple(Vec<Spanned<Type>>),
}

// ══════════════════════════════════════════════════════════════
// Traits
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct TraitDef {
    pub attributes: Vec<Spanned<Attribute>>,
    pub visibility: Visibility,
    pub name: Spanned<String>,
    pub generic_params: Option<Spanned<GenericParams>>,
    pub extends: Vec<Spanned<TraitBound>>,
    pub items: Vec<Spanned<TraitItem>>,
    pub doc_comment: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TraitItem {
    Method(FunctionDef),
    AssociatedType(AssociatedTypeDef),
}

#[derive(Debug, Clone)]
pub struct AssociatedTypeDef {
    pub name: Spanned<String>,
    pub bounds: Vec<Spanned<TraitBound>>,
    pub default: Option<Spanned<Type>>,
    pub span: Span,
}

// ══════════════════════════════════════════════════════════════
// Equip Blocks
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct EquipBlock {
    pub generic_params: Option<Spanned<GenericParams>>,
    pub trait_: Option<EquipTrait>,
    pub type_: Spanned<Type>,
    pub via_field: Option<Spanned<String>>,
    pub where_clause: Option<Spanned<WhereClause>>,
    pub items: Vec<Spanned<FunctionDef>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EquipTrait {
    pub trait_name: Spanned<Type>,
    pub span: Span,
}

// ══════════════════════════════════════════════════════════════
// Imports
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub enum ImportStmt {
    /// `import std.io`
    Simple {
        path: Vec<Spanned<String>>,
        span: Span,
    },
    /// `import std.sync.{Arc, Mutex}`
    Grouped {
        path: Vec<Spanned<String>>,
        names: Vec<Spanned<String>>,
        span: Span,
    },
    /// `from std.fmt import Displayable, format`
    From {
        path: Vec<Spanned<String>>,
        names: Vec<Spanned<String>>,
        span: Span,
    },
}

// ══════════════════════════════════════════════════════════════
// Type Aliases & Newtypes
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub name: Spanned<String>,
    pub generic_params: Option<Spanned<GenericParams>>,
    pub type_: Spanned<Type>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct NewtypeDef {
    pub name: Spanned<String>,
    pub inner_type: Spanned<Type>,
    pub span: Span,
}

// ══════════════════════════════════════════════════════════════
// Generics & Bounds
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct GenericParams {
    pub params: Vec<Spanned<GenericParam>>,
}

#[derive(Debug, Clone)]
pub enum GenericParam {
    Type(Spanned<String>),
    Lifetime(Spanned<String>),
    Const {
        type_: Spanned<Type>,
        name: Spanned<String>,
    },
}

#[derive(Debug, Clone)]
pub struct WhereClause {
    pub bounds: Vec<Spanned<WhereBound>>,
}

#[derive(Debug, Clone)]
pub struct WhereBound {
    pub type_name: Spanned<String>,
    pub bounds: Vec<Spanned<TraitBound>>,
}

#[derive(Debug, Clone)]
pub struct TraitBound {
    pub name: Spanned<String>,
    pub generic_args: Option<Vec<Spanned<Type>>>,
    pub assoc_type_bindings: Vec<AssocTypeBinding>,
}

#[derive(Debug, Clone)]
pub struct AssocTypeBinding {
    pub name: Spanned<String>,
    pub type_: Spanned<Type>,
}

// ══════════════════════════════════════════════════════════════
// Types
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub enum Type {
    /// int, float, bool, char, str, String, void, etc.
    Primitive(PrimitiveType),

    /// Named type with optional generic args: `Vector[int]`
    Named {
        name: Spanned<String>,
        generic_args: Vec<Spanned<Type>>,
    },

    /// Fixed array: `int[5]`
    Array {
        element: Box<Spanned<Type>>,
        size: Box<Spanned<Expr>>,
    },

    /// Slice: `int[]`
    Slice {
        element: Box<Spanned<Type>>,
    },

    /// Tuple: `(int, String)`
    Tuple(Vec<Spanned<Type>>),

    /// Function type: `int(int, int)`
    Function {
        return_type: Box<Spanned<Type>>,
        params: Vec<Spanned<Type>>,
    },

    /// Self type
    SelfType,

    /// auto (inferred)
    Inferred,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float,
    Float32,
    Float64,
    Bool,
    Char,
    Str,
    StringType,
    Void,
}

// ══════════════════════════════════════════════════════════════
// Expressions
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub enum Expr {
    // ── Literals ──
    IntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    CharLiteral(char),
    StringLiteral(crate::lexer::token::StringLit),
    NoneLiteral,

    // ── Identifiers ──
    Identifier(String),
    SelfExpr,

    /// Qualified path: `Point.origin`, `List.Nil`
    Path {
        segments: Vec<Spanned<String>>,
    },

    // ── Unary ops ──
    UnaryOp {
        op: UnaryOp,
        operand: Box<Spanned<Expr>>,
    },

    // ── Binary ops ──
    BinaryOp {
        left: Box<Spanned<Expr>>,
        op: BinaryOp,
        right: Box<Spanned<Expr>>,
    },

    // ── Function call ──
    Call {
        callee: Box<Spanned<Expr>>,
        generic_args: Option<Vec<Spanned<Type>>>,
        args: Vec<Spanned<CallArg>>,
    },

    // ── Method call ──
    MethodCall {
        receiver: Box<Spanned<Expr>>,
        method: Spanned<String>,
        generic_args: Option<Vec<Spanned<Type>>>,
        args: Vec<Spanned<CallArg>>,
    },

    // ── Field access ──
    FieldAccess {
        object: Box<Spanned<Expr>>,
        field: Spanned<String>,
    },

    // ── Tuple field access: .0, .1 ──
    TupleFieldAccess {
        object: Box<Spanned<Expr>>,
        index: usize,
    },

    // ── Index ──
    Index {
        object: Box<Spanned<Expr>>,
        index: Box<Spanned<Expr>>,
    },

    // ── Range ──
    Range {
        start: Option<Box<Spanned<Expr>>>,
        end: Option<Box<Spanned<Expr>>>,
        inclusive: bool,
    },

    // ── Optional chaining (?.) ──
    OptionalChain {
        object: Box<Spanned<Expr>>,
        field: Spanned<String>,
    },

    // ── Nil coalescing (??) ──
    NilCoalescing {
        lhs: Box<Spanned<Expr>>,
        rhs: Box<Spanned<Expr>>,
    },

    // ── Try/early return (?) ──
    Try {
        expr: Box<Spanned<Expr>>,
    },

    // ── Move (!) ──
    Move {
        expr: Box<Spanned<Expr>>,
    },

    // ── Mutable borrow (&) ──
    MutableBorrow {
        expr: Box<Spanned<Expr>>,
    },

    // ── Dereference (*) ──
    Deref {
        expr: Box<Spanned<Expr>>,
    },

    // ── If expression ──
    If {
        condition: Box<Spanned<Expr>>,
        then_branch: Box<Spanned<Expr>>,
        elif_branches: Vec<(Spanned<Expr>, Spanned<Expr>)>,
        else_branch: Option<Box<Spanned<Expr>>>,
    },

    // ── Match expression ──
    Match {
        scrutinee: Box<Spanned<Expr>>,
        arms: Vec<MatchArm>,
        else_arm: Option<Box<Spanned<Expr>>>,
    },

    // ── Block ──
    Block(Block),

    // ── Do expression ──
    Do {
        body: Block,
    },

    // ── Closure ──
    Closure {
        is_move: bool,
        is_async: bool,
        params: Vec<Spanned<ClosureParam>>,
        body: Box<Spanned<Expr>>,
    },

    // ── Implicit 'it' closure ──
    ImplicitClosure {
        body: Box<Spanned<Expr>>,
    },

    // ── Comprehensions ──
    ListComprehension {
        expr: Box<Spanned<Expr>>,
        variable: Spanned<Pattern>,
        ownership: Ownership,
        iterable: Box<Spanned<Expr>>,
        condition: Option<Box<Spanned<Expr>>>,
    },

    DictComprehension {
        key: Box<Spanned<Expr>>,
        value: Box<Spanned<Expr>>,
        variables: Vec<Spanned<String>>,
        iterable: Box<Spanned<Expr>>,
        condition: Option<Box<Spanned<Expr>>>,
    },

    SetComprehension {
        expr: Box<Spanned<Expr>>,
        variable: Spanned<String>,
        iterable: Box<Spanned<Expr>>,
        condition: Option<Box<Spanned<Expr>>>,
    },

    // ── Collection literals ──
    ArrayLiteral(Vec<Spanned<Expr>>),
    TupleLiteral(Vec<Spanned<Expr>>),

    // ── Struct construction ──
    StructLiteral {
        name: Spanned<String>,
        args: Vec<Spanned<Expr>>,
    },

    // ── Cast ──
    As {
        expr: Box<Spanned<Expr>>,
        type_: Spanned<Type>,
    },

    // ── Await ──
    Await {
        expr: Box<Spanned<Expr>>,
    },

    // ── Spawn ──
    Spawn {
        expr: Box<Spanned<Expr>>,
    },

    // ── Try capture ──
    TryCapture {
        expr: Box<Spanned<Expr>>,
    },

    // ── Is pattern test ──
    Is {
        expr: Box<Spanned<Expr>>,
        negated: bool,
        pattern: Spanned<Pattern>,
    },

    // ── It (implicit closure parameter) ──
    It,
}

#[derive(Debug, Clone)]
pub struct ClosureParam {
    pub type_: Option<Spanned<Type>>,
    pub ownership: Ownership,
    pub name: Spanned<String>,
}

#[derive(Debug, Clone)]
pub struct CallArg {
    pub name: Option<Spanned<String>>,
    pub ownership: Ownership,
    pub value: Spanned<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
    Deref,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    AddWrap,
    SubWrap,
    MulWrap,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Eq,
    Neq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    And,
    Or,
    In,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Spanned<Pattern>,
    pub guard: Option<Spanned<Expr>>,
    pub body: Spanned<Expr>,
    pub span: Span,
}

// ══════════════════════════════════════════════════════════════
// Patterns
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub enum Pattern {
    /// Wildcard: _
    Wildcard,

    /// Literal: 42, "hello", true
    Literal(Box<Spanned<Expr>>),

    /// Variable binding: x
    Binding(String),

    /// Enum/struct destructure: Some(x), Point(x, y)
    Constructor {
        path: Vec<Spanned<String>>,
        fields: Vec<Spanned<Pattern>>,
    },

    /// Tuple destructure: (x, y, z)
    Tuple(Vec<Spanned<Pattern>>),

    /// Or pattern: 200 | 201 | 204
    Or(Vec<Spanned<Pattern>>),

    /// Rest pattern: ..
    Rest,
}

// ══════════════════════════════════════════════════════════════
// Statements
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub enum Stmt {
    /// Variable declaration: int x = 5
    VarDecl {
        is_const: bool,
        is_mutable: bool,
        type_: Spanned<Type>,
        pattern: Spanned<Pattern>,
        value: Spanned<Expr>,
    },

    /// Expression statement
    Expr(Spanned<Expr>),

    /// Assignment: x = expr
    Assign {
        target: Spanned<Expr>,
        value: Spanned<Expr>,
    },

    /// Compound assignment: x += expr
    CompoundAssign {
        target: Spanned<Expr>,
        op: BinaryOp,
        value: Spanned<Expr>,
    },

    /// return [expr]
    Return(Option<Spanned<Expr>>),

    /// throw expr
    Throw(Spanned<Expr>),

    /// break [expr]
    Break(Option<Spanned<Expr>>),

    /// continue
    Continue,

    /// pass
    Pass,

    /// for pattern in [&|!] expr: block [else: block]
    For {
        pattern: Spanned<Pattern>,
        ownership: Ownership,
        iterable: Spanned<Expr>,
        body: Block,
        else_body: Option<Block>,
    },

    /// while condition: block [else: block]
    While {
        condition: Spanned<Expr>,
        body: Block,
        else_body: Option<Block>,
    },

    /// loop: block
    Loop {
        body: Block,
    },

    /// if/elif/else statement
    If {
        condition: Spanned<Expr>,
        then_body: Block,
        elif_branches: Vec<(Spanned<Expr>, Block)>,
        else_body: Option<Block>,
    },

    /// match/case/else statement
    Match {
        scrutinee: Spanned<Expr>,
        arms: Vec<MatchArm>,
        else_arm: Option<Block>,
    },

    /// with expr as name: block
    With {
        bindings: Vec<WithBinding>,
        body: Block,
    },

    /// unsafe: block
    Unsafe {
        body: Block,
    },

    /// assert condition [, message]
    Assert {
        condition: Spanned<Expr>,
        message: Option<Spanned<Expr>>,
    },

    /// Nested item definition
    Item(Box<Item>),
}

#[derive(Debug, Clone)]
pub struct WithBinding {
    pub expr: Spanned<Expr>,
    pub name: Spanned<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Spanned<Stmt>>,
    pub span: Span,
}

// ══════════════════════════════════════════════════════════════
// Other Top-Level Items
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: Spanned<String>,
    pub args: Vec<AttributeArg>,
}

#[derive(Debug, Clone)]
pub enum AttributeArg {
    Identifier(String),
    StringLiteral(String),
    KeyValue(String, String),
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub visibility: Visibility,
    pub type_: Spanned<Type>,
    pub name: Spanned<String>,
    pub value: Spanned<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StaticDecl {
    pub visibility: Visibility,
    pub type_: Spanned<Type>,
    pub name: Spanned<String>,
    pub value: Spanned<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ExternBlock {
    pub abi: Option<Spanned<String>>,
    pub items: Vec<Spanned<FunctionDef>>,
    pub span: Span,
}
