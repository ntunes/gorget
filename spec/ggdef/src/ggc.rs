//! Gorget Core (GGC) — the small, desugared core that `ggdef` evaluates
//! (RFC §2.1). The production surface AST elaborates *into* this (see
//! `elaborate`); `eval` walks only GGC, never the surface AST.
//!
//! This is the Increment-A subset: int64/bool/float64 scalars, `String`,
//! `Vector`, structs, tuples; the three mode tags; and the operations the
//! first ~20 `cow_*` fixtures use. The full sized-int matrix, `Dict`/`Set`,
//! enums/`match`, `equip`, ranges and closures are Increment B/C.

use gorget::span::Span;

/// A whole GGC program: the functions plus the struct field layouts needed to
/// construct/read struct values by name.
#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>,
    pub structs: Vec<StructDef>,
}

/// A struct's field names, in declaration order (the ctor is positional).
#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<String>,
}

/// A GGC function.
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub body: Block,
    pub span: Span,
}

/// A function parameter with its elaboration-resolved mode tag. GGC never
/// re-infers a mode (RFC §2.1).
#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub mode: Mode,
    pub span: Span,
}

/// The three mode tags (RFC §2.1/§2.2). `Borrow` is a non-owning view that
/// **materialises on first write**; `WriteThrough` aliases the owner; `Move`
/// transfers and kills the source.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    Borrow,
    WriteThrough,
    Move,
}

pub type Block = Vec<Stmt>;

/// A statement. Each carries its source span so the evaluator can stamp trace
/// events with provenance without threading spans through every expression.
#[derive(Debug, Clone)]
pub enum Stmt {
    /// `let name = <source>` — a fresh binding in the current scope.
    Bind { name: String, source: Source, span: Span },
    /// `place = <source>` — write to an existing place.
    Assign { target: Expr, value: Source, span: Span },
    /// A bare expression evaluated for effect (e.g. a mutating method call).
    Expr { expr: Expr, span: Span },
    /// The `print` output effect (RFC §2.1): formats + a trailing newline.
    Print { expr: Expr, span: Span },
    If { cond: Expr, then_: Block, else_: Block, span: Span },
    While { cond: Expr, body: Block, span: Span },
    Loop { body: Block, span: Span },
    Return { value: Option<Expr>, span: Span },
    Break { span: Span },
    Continue { span: Span },
}

/// A value-or-alias operand with its ownership discipline. This is how the
/// closed set of copy/move/borrow decisions from §2.2 crosses into GGC: each
/// consuming/binding position tags its operand once, at elaboration.
#[derive(Debug, Clone)]
pub enum Source {
    /// Deep-copy a live place (an implicit-copy position). Inner is a place.
    Copy(Expr),
    /// Move a place, killing it. Inner is a place.
    Move(Expr),
    /// A bare-param / for-var view: materialise-on-write. Inner is a place.
    BorrowView(Expr),
    /// A `&` write-through alias. Inner is a place.
    WriteThrough(Expr),
    /// A fresh temp value (constructor/call/binop/literal), moved into the slot.
    Value(Expr),
}

/// An f-string part: literal text or an interpolated expression.
#[derive(Debug, Clone)]
pub enum FPart {
    Lit(String),
    Interp(Expr),
}

/// What a `Construct` builds.
#[derive(Debug, Clone)]
pub enum ConstructKind {
    Struct(String),
    Vector,
    Tuple,
}

/// The builtin collection/string methods the Increment-A fixtures use.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinMethod {
    /// `v.push(x)` — append (a collection-put, so the arg is an owning copy/move).
    Push,
    /// `v.set(i, x)` — replace element `i`.
    Set,
    /// `v.len()` — element count (a read).
    Len,
}

/// A GGC expression.
#[derive(Debug, Clone)]
pub enum Expr {
    // ── Literals ──
    Int(i64),
    Bool(bool),
    Float(f64),
    Str(String),
    FString(Vec<FPart>),

    // ── Places ──
    Local(String),
    Field(Box<Expr>, String),
    TupleField(Box<Expr>, usize),
    Index(Box<Expr>, Box<Expr>),

    // ── Pure operators ──
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),

    // ── Value producers ──
    Call { func: String, args: Vec<Source> },
    Construct { kind: ConstructKind, args: Vec<Source> },
    Method { recv: Box<Expr>, method: BuiltinMethod, args: Vec<Source> },
    /// An explicit `.clone()` deep copy of a place (emits `ExplicitClone`).
    Clone(Box<Expr>),
}

impl Expr {
    /// Whether this expression denotes a *place* (an assignable/aliasable
    /// storage location) rather than a fresh value. Drives the copy-vs-view
    /// decision at binding and call-arg positions.
    pub fn is_place(&self) -> bool {
        matches!(
            self,
            Expr::Local(_) | Expr::Field(..) | Expr::TupleField(..) | Expr::Index(..)
        )
    }

    /// A human-readable path for this place, for trace provenance
    /// (`a`, `a.text`, `v[0]`). Best-effort for non-place shapes.
    pub fn place_str(&self) -> String {
        match self {
            Expr::Local(n) => n.clone(),
            Expr::Field(o, f) => format!("{}.{}", o.place_str(), f),
            Expr::TupleField(o, i) => format!("{}.{}", o.place_str(), i),
            Expr::Index(o, i) => match &**i {
                Expr::Int(n) => format!("{}[{}]", o.place_str(), n),
                _ => format!("{}[?]", o.place_str()),
            },
            Expr::Clone(inner) => inner.place_str(),
            _ => "<temp>".to_string(),
        }
    }
}

/// The binary operators the A subset needs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Neq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    And,
    Or,
}

/// The unary operators the A subset needs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
}

/// A runtime value. Values are *plain data* (RFC §2.7) — no `Rc`/`RefCell`,
/// no interior mutability — copied deeply whenever value semantics demands it.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Unit,
    Int(i64),
    Bool(bool),
    Float(f64),
    Str(String),
    Vector(Vec<Value>),
    Tuple(Vec<Value>),
    Struct { name: String, fields: Vec<(String, Value)> },
}

impl Value {
    /// The name of this value's kind, for error/trap messages.
    pub fn kind_name(&self) -> &'static str {
        match self {
            Value::Unit => "unit",
            Value::Int(_) => "int",
            Value::Bool(_) => "bool",
            Value::Float(_) => "float",
            Value::Str(_) => "String",
            Value::Vector(_) => "Vector",
            Value::Tuple(_) => "tuple",
            Value::Struct { .. } => "struct",
        }
    }
}
