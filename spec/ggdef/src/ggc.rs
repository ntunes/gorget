//! Gorget Core (GGC) — the small, desugared core that `ggdef` evaluates
//! (RFC §2.1). The production surface AST elaborates *into* this (see
//! `elaborate`); `eval` walks only GGC, never the surface AST.
//!
//! This is the Increment-B1 subset: it extends Increment A (int64/bool/float64
//! scalars, `String`, `Vector`, structs, tuples; the three mode tags; and the
//! first ~20 `cow_*` fixtures' operations) with the **non-equip** phase-0
//! surface:
//!   * Option/Result (ordinary enums) + `.get()`/`.unwrap()`/`.unwrap_or()`;
//!   * user payload enums + `match` + pattern bindings (Borrow-mode; a new
//!     `Proj::Payload` at the eval layer);
//!   * `Dict`/`Set` (insertion-ordered);
//!   * ranges/string slices `s[a..b]`; named-arg construction;
//!   * the full corpus builtin-method set (`push`/`set`/`len`/`get`/`unwrap`/
//!     `unwrap_or`/`pop`/`clear`/`fill`/`add`/`trim`/`substring`);
//!   * sized-int `as`-cast saturation (unit-tested only);
//!   * by-value closures + the `std.conv.int_to_str` shim.
//!
//! `equip`/`Drop`/receiver-type-inference/D4-rejections stay Increment B2.

use gorget::span::Span;

/// A whole GGC program: the functions plus the struct field layouts, enum
/// variant arities, and closure definitions needed to construct/read values.
#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>,
    pub structs: Vec<StructDef>,
    pub enums: Vec<EnumDef>,
    pub closures: Vec<ClosureDef>,
    /// `equip T with Drop` custom-drop functions: `(type-name, drop-fn-name)`.
    /// The evaluator runs the named function (self moved in) at each scope-exit
    /// drop of a value of that type (RFC §2.2 D4: custom drops run in reverse
    /// declaration order). `eval::Ctx` resolves the fn-name to its index.
    pub drop_fns: Vec<(String, String)>,
    /// D29 (visible error propagation): the FIRST bare-fallible-mark violation
    /// the elaborator saw, if any — a fallible call whose outcome is neither
    /// marked (`f()!`), captured (`Result[T,E] r = f()`), nor handled. Recorded
    /// as TYPED metadata during elaboration (where callee fallibility + capture
    /// context live) and surfaced by `run` as `Outcome::IllFormed` + the ratified
    /// `E_MissingFallibleMark` reject code BEFORE any evaluation — so the ggdef
    /// conformance lane AFFIRMS the reject on the CODE axis (a `MATCH`), not a
    /// generic `FrontendError` (a `GGDEF-SKIP`). `None` when the program is
    /// D29-clean. Deliberately on `Program`, NOT surfaced as an `ElabError`: a
    /// ratified static rejection carries its `E_` code, exactly like the may-move
    /// gate's `check_liveness` → `IllFormed` + `reject_code` pathway.
    pub d29_reject: Option<D29Reject>,
}

/// A recorded D29 bare-fallible-mark violation: the (impl-defined) human message
/// and the offending call's span. The reject CODE is the constant
/// `E_MissingFallibleMark` (the sole D29 code; the reason — bare / redundant-on-
/// capture / result-arms-on-peeled — rides the message, never the code, per the
/// one-code-per-family doctrine). Mirrors `liveness::LivenessError` for the
/// throws/Result-mark axis.
#[derive(Debug, Clone)]
pub struct D29Reject {
    pub message: String,
    pub span: Span,
}

impl D29Reject {
    /// The ratified registry code — the conformance-compared axis (pin 3).
    pub const CODE: &'static str = "E_MissingFallibleMark";
}

/// A struct's field names, in declaration order (the ctor is positional).
#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<String>,
}

/// A user enum's variants, each with its payload arity (positional).
#[derive(Debug, Clone)]
pub struct EnumDef {
    pub name: String,
    pub variants: Vec<(String, usize)>,
}

/// A closure definition (RFC §2.1: "a closure value = code ref + environment
/// record"). Elaboration lifts every surface closure into `Program.closures`
/// and refers to it by index; the environment (`captured`) is built at the
/// creation site from `captures` (D5: capture-by-value at creation).
#[derive(Debug, Clone)]
pub struct ClosureDef {
    pub params: Vec<Param>,
    /// Free variables captured by value at the closure's creation site.
    pub captures: Vec<String>,
    /// The closure body is a single expression (surface closures always have
    /// expression bodies; a block body is `Expr::Block` on the surface).
    pub body: Expr,
    pub span: Span,
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
    /// `with <source> as name:` — a scoped resource bind (RFC §2.6 sugar). The
    /// resource is bound in a FRESH scope; at block exit it drops (custom drop
    /// included) AFTER the body's own locals, in reverse declaration order. It
    /// is deliberately NOT inlined as a plain `Bind` — that would drop-time the
    /// resource at the enclosing function's exit rather than the block's.
    With { name: String, source: Source, body: Block, span: Span },
    /// The `print` output effect (RFC §2.1): formats + a trailing newline.
    Print { expr: Expr, span: Span },
    If { cond: Expr, then_: Block, else_: Block, span: Span },
    While { cond: Expr, body: Block, span: Span },
    Loop { body: Block, span: Span },
    /// `match <scrutinee>:` in statement position — each arm body is a block.
    Match { scrutinee: Expr, arms: Vec<StmtArm>, else_arm: Option<Block>, span: Span },
    Return { value: Option<Expr>, span: Span },
    Break { span: Span },
    Continue { span: Span },
    /// `assert cond` / `assert cond, msg` — Traps `T_AssertFailed` when `cond`
    /// is false. The message (if present) is evaluated ONLY on failure.
    Assert { cond: Expr, message: Option<Expr>, span: Span },
}

/// A `match`-statement arm: a pattern and a statement block body.
#[derive(Debug, Clone)]
pub struct StmtArm {
    pub pattern: Pattern,
    pub body: Block,
}

/// A `match`-expression arm: a pattern and a value-producing expression body.
#[derive(Debug, Clone)]
pub struct ExprArm {
    pub pattern: Pattern,
    pub body: Expr,
}

/// A GGC pattern. Bindings introduced by a pattern are Borrow-mode views into
/// the scrutinee (RFC §2.2), reached via `Proj::Payload` at the eval layer.
#[derive(Debug, Clone)]
pub enum Pattern {
    /// `_` — matches anything, binds nothing.
    Wildcard,
    /// A literal to compare by value (`case 0:`, `case "seven":`).
    Literal(Box<Expr>),
    /// A bare name — matches anything, binds the whole scrutinee.
    Binding(String),
    /// An enum-variant destructure (`Token.Ident(s)`, `Some(x)`). Matches an
    /// `Enum` value with the same variant + arity; sub-patterns bind payloads.
    Variant { variant: String, fields: Vec<Pattern> },
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
    Dict,
    Set,
}

/// The builtin collection/string methods the phase-0 fixtures use. Several are
/// overloaded across receiver types (e.g. `.get`/`.set`/`.len` on Vector vs
/// Dict vs String) and dispatch on the runtime receiver value at eval time.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinMethod {
    /// `v.push(x)` — append (a collection-put: the arg is an owning copy/move).
    Push,
    /// `v.set(i, x)` (Vector) / `d.set(k, v)` (Dict) — replace/insert.
    Set,
    /// `v.len()` — element/codepoint count (a read).
    Len,
    /// `v.get(i)` (Vector) / `d.get(k)` (Dict) — read → `Option`.
    Get,
    /// `o.unwrap()` — extract Some/Ok payload; Trap on None/Error.
    Unwrap,
    /// `r.unwrap_error()` — the dual of `unwrap()`: extract the `Error` payload;
    /// Trap (`T_UnwrapErrorOnOk`) on an `Ok` receiver.
    UnwrapError,
    /// `o.unwrap_or(default)` — payload or the default.
    UnwrapOr,
    /// `v.pop()` — remove+return last → `Option` (mutates).
    Pop,
    /// `v.clear()` — remove all elements (mutates).
    Clear,
    /// `v.fill(n, x)` — replace contents with `n` copies of `x` (mutates).
    Fill,
    /// `s.add(x)` — Set insert if absent (mutates).
    Add,
    /// `s.trim()` — String, trailing/leading whitespace removed (→ new String).
    Trim,
    /// `s.substring(a, b)` — String codepoint slice `[a, b)` (→ new String).
    Substring,
    // ── Option / Result combinators (Increment B3). ─────────────────────────
    // All READ-ONLY on the receiver in the phase-0 semantic model — they
    // produce a fresh `Option[U]` / `Result[U, E]` / payload, never mutate
    // the receiver in place. Adjudication axis: VALUE semantics on the
    // returned value AND on the preserved receiver. Structurally BLIND to
    // memory-invalidation (Core #13: SIGSEGV cells are not adjudicable via
    // ggdef; the C+LLVM ASan lane is the adjudicator there).
    /// `o.map(f)` — apply f to the payload on the success arm; None/Error
    /// arms pass through unchanged.
    Map,
    /// `o.filter(pred)` — Option only: keep the Some payload only if pred
    /// returns true. On Result, elaboration is IllFormed (the phase-0 model
    /// keeps filter Option-only).
    Filter,
    /// `o.or_else(f)` — call f() on None/Error, else pass through the
    /// success arm unchanged. Closure receives no args on Option; on Result
    /// it receives the Error payload.
    OrElse,
    /// `o.and_then(f)` — apply f to the payload; f RETURNS the wrapped
    /// Option/Result (no auto-wrap). None/Error passes through.
    AndThen,
    /// `o.flat_map(f)` — Option-only alias of `and_then` at the phase-0
    /// semantic level. On Result, elaboration is `error[E_NoMethodFound]`
    /// (Round XXV Track B — mirrors `src/ir/lowering/builtins.rs:1425-1429`
    /// asserting `RESULT.flat_map` is unregistered). Uses `.and_then()`
    /// on Result instead.
    FlatMap,
    /// `o.unwrap_or_else(f)` — payload on success, else the value f()
    /// returns. On Result f receives the Error payload; on Option it takes
    /// no args.
    UnwrapOrElse,
    /// `r.map_err(f)` — Result only: apply f to the Error payload; Ok
    /// passes through. On Option, elaboration is IllFormed.
    MapErr,
}

/// The target of an `as`-cast (unit-tested only — no phase-0 corpus fixture).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CastTarget {
    /// A sized integer: `(bits, signed)`. `int`==(64,true), `byte`==(8,false).
    Int { bits: u32, signed: bool },
    Float32,
    Float64,
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
    /// The unit value `()`. Elaboration's ONLY producer is the throws→Result
    /// desugar's `Ok(())` for a bare/fall-off `return` in a `void … throws E`
    /// function (there is no unit literal on the surface).
    Unit,

    // ── Places ──
    Local(String),
    Field(Box<Expr>, String),
    TupleField(Box<Expr>, usize),
    Index(Box<Expr>, Box<Expr>),
    /// `s[a..b]` codepoint/element slice (`start`/`end` optional, open ends).
    Slice { object: Box<Expr>, start: Option<Box<Expr>>, end: Option<Box<Expr>>, inclusive: bool },

    // ── Pure operators ──
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),
    /// `expr as T` — sized-int / float cast (saturating float→int).
    Cast { expr: Box<Expr>, target: CastTarget },

    // ── Value producers ──
    Call { func: String, args: Vec<Source> },
    /// A call of a first-class closure value (`f()` where `f` is a local).
    /// `consumes_callee` is set at elaboration when the callee's declared type
    /// is a single-owner `ConsumeCallable` (D5 kind axis): calling it CONSUMES
    /// the callee value, so a second call reads a moved-out slot → `IllFormed`
    /// (mirrors production `check_move` on a ConsumeCallable call). A plain
    /// `Callable` / `MutCallable` is reusable, so the flag is `false`.
    CallValue { callee: Box<Expr>, args: Vec<Source>, consumes_callee: bool },
    Construct { kind: ConstructKind, args: Vec<Source> },
    /// An enum variant value (`Some(x)`, `None`, `Ok(v)`, `Token.Ident(s)`).
    EnumConstruct { type_name: String, variant: String, args: Vec<Source> },
    Method { recv: Box<Expr>, method: BuiltinMethod, args: Vec<Source> },
    /// A closure value: index into `Program.closures`. Captures resolve at eval.
    Closure(usize),
    /// A `match` in expression position — each arm body yields a value.
    Match { scrutinee: Box<Expr>, arms: Vec<ExprArm>, else_arm: Option<Box<Expr>>, span: Span },
    /// `panic(msg)` — an uncatchable trap (`T_Panic`) carrying a user message.
    /// Noreturn: evaluation unwinds and never yields a value.
    Panic(Box<Expr>),
    /// The `std.conv.int_to_str` shim intrinsic.
    IntToStr(Box<Expr>),
    /// An explicit `.clone()` deep copy of a place (emits `ExplicitClone`).
    Clone(Box<Expr>),
    /// Error-propagation of a `Result` value — the elaboration home of the
    /// implicit `throws`-call auto-propagate (RFC §2.6 row 1, "throws/Result").
    /// The inner expression MUST evaluate to a `Result`: `Ok(x)` yields `x`;
    /// `Error(e)` performs an early function-level `return Error(e)` from the
    /// enclosing `throws` function (the `?`-operator semantics). Elaboration
    /// only emits this inside a `throws` function; a stray one reaching the top
    /// frame is `IllFormed` (defensive).
    Propagate(Box<Expr>),
}

impl Expr {
    /// Whether this expression denotes a *place* (an assignable/aliasable
    /// storage location) rather than a fresh value. Drives the copy-vs-view
    /// decision at binding and call-arg positions.
    pub fn is_place(&self) -> bool {
        match self {
            Expr::Local(_) | Expr::Field(..) | Expr::TupleField(..) | Expr::Index(..) => true,
            // Auto-borrow-from-get: `coll.get(i).unwrap()` denotes the PLACE
            // `coll[i]`. `.get()` yields `Option[Ref[T]]` and `.unwrap()` the
            // `Ref[T]` borrow (the ratified get→Ref semantics), so a mutating
            // method on the chain writes THROUGH to the stored element and a read
            // reads it. A place iff the underlying collection is itself a place.
            Expr::Method { recv, method: BuiltinMethod::Unwrap, args } if args.is_empty() => {
                matches!(&**recv, Expr::Method { recv: coll, method: BuiltinMethod::Get, .. } if coll.is_place())
            }
            _ => false,
        }
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

/// The binary operators the subset needs.
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

/// The unary operators the subset needs.
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
    /// An enum value (Option/Result/user enums are all ordinary enums).
    Enum { type_name: String, variant: String, payload: Vec<Value> },
    /// An insertion-ordered map (Dict). Keys compared by value equality.
    Dict(Vec<(Value, Value)>),
    /// An insertion-ordered set (Set). Elements compared by value equality.
    Set(Vec<Value>),
    /// A first-class closure: a code ref (`def`) + its captured environment.
    Closure { def: usize, captured: Vec<(String, Value)> },
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
            Value::Enum { .. } => "enum",
            Value::Dict(_) => "Dict",
            Value::Set(_) => "Set",
            Value::Closure { .. } => "closure",
        }
    }
}
