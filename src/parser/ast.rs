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

impl Module {
    /// Return a flat list of all items, recursively unwrapping `Item::Module` wrappers.
    ///
    /// After `merge_modules()`, non-entry imported modules are wrapped in `Item::Module`.
    /// Most compiler passes want a flat view of all items across all modules — this method
    /// provides that without allocating a new AST.
    pub fn all_items(&self) -> Vec<&Item> {
        let mut result = Vec::new();
        Self::collect_flat(&self.items, &mut result);
        result
    }

    fn collect_flat<'a>(items: &'a [Spanned<Item>], out: &mut Vec<&'a Item>) {
        for item in items {
            if let Item::Module { items: inner, .. } = &item.node {
                Self::collect_flat(inner, out);
            } else {
                out.push(&item.node);
            }
        }
    }
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
    Bench(BenchDef),
    SuiteSetup(SuiteSetup),
    SuiteTeardown(SuiteTeardown),
    MetaConst(MetaConst),
    MetaType(MetaType),
    MetaTypeFunc(MetaTypeFunc),
    MetaAssert(MetaAssert),
    MetaLog(MetaLog),
    MetaIf(MetaIf),
    /// A file-based module's items, wrapped during merge to preserve module identity.
    /// Created by the loader when merging multi-file programs.
    Module {
        /// Logical import path, e.g. `["xtd", "csv"]`.
        path: Vec<String>,
        items: Vec<Spanned<Item>>,
    },
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

#[derive(Debug, Clone)]
pub struct BenchDef {
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: Spanned<String>,
    pub body: Block,
    pub doc_comment: Option<String>,
    pub span: Span,
}

// ══════════════════════════════════════════════════════════════
// Functions
// ══════════════════════════════════════════════════════════════

/// A function's declared fallibility (D29). One typed axis with three states —
/// NOT a `Option<Type>` + sentinel: the `!:` inferred spelling must never be a
/// magic `Type::Named("!inferred")` (layering rule 2: no name-matched sentinels).
#[derive(Debug, Clone, Default)]
pub enum ThrowsSpec {
    /// No `throws` clause and no bare `!` — the function is infallible.
    #[default]
    No,
    /// Bare `!` before the body (`int f()!:`) — A31's inferred-error-set
    /// spelling. Parses so the grammar locks now; the checker teaching-rejects
    /// it until A31 lands. Carries the `!` span for the diagnostic.
    Inferred(Span),
    /// Explicit `throws E` error contract.
    Explicit(Spanned<Type>),
}

impl ThrowsSpec {
    /// True when the function declares fallibility (either `throws E` or the
    /// inferred `!`). Replaces the old `throws.is_some()`.
    pub fn declares_throws(&self) -> bool {
        !matches!(self, ThrowsSpec::No)
    }

    /// The explicit `throws E` type, if the fallibility is spelled with a
    /// concrete contract. `None` for both `No` and `Inferred`. Replaces the old
    /// `throws.as_ref()` where the caller wants the error type.
    pub fn explicit_type(&self) -> Option<&Spanned<Type>> {
        match self {
            ThrowsSpec::Explicit(t) => Some(t),
            _ => None,
        }
    }

    /// Mutable access to the explicit `throws E` type (for the resolve/meta
    /// substitution passes that rewrite type names in place).
    pub fn explicit_type_mut(&mut self) -> Option<&mut Spanned<Type>> {
        match self {
            ThrowsSpec::Explicit(t) => Some(t),
            _ => None,
        }
    }

    /// True for the bare `!` inferred spelling (A31 reservation).
    pub fn is_inferred(&self) -> bool {
        matches!(self, ThrowsSpec::Inferred(_))
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub attributes: Vec<Spanned<Attribute>>,
    pub visibility: Visibility,
    pub qualifiers: FunctionQualifiers,
    pub return_type: Spanned<Type>,
    pub name: Spanned<String>,
    pub generic_params: Option<Spanned<GenericParams>>,
    pub params: Vec<Spanned<Param>>,
    pub throws: ThrowsSpec,
    pub body: FunctionBody,
    pub doc_comment: Option<String>,
    pub span: Span,
    /// Per-parameter ABI marshalling for extern functions. Empty = all Auto.
    pub param_abis: Vec<crate::ir::abi::AbiKind>,
    /// ABI language tag for inline extern declarations: `extern "C" int foo() = "symbol"`.
    /// Determines string param marshalling (Some("C") → String params become CStr).
    pub extern_abi: Option<String>,
    /// `extern borrowed T f(...)`: the FFI returns a non-owned pointer
    /// (e.g. SDL_GetError's internal buffer). Callers must clone at the
    /// ownership boundary; the IR layer is expected to insert that clone.
    /// Only meaningful for extern functions; ignored otherwise.
    pub returns_borrowed: bool,
}

#[derive(Debug, Clone, Default)]
pub struct FunctionQualifiers {
    pub is_async: bool,
    pub is_const: bool,
    pub is_static: bool,
    pub is_unsafe: bool,
    /// True for `blocking` extern functions — the call may block the thread.
    /// Used by the shared_async transform to release/reacquire locks.
    pub is_blocking: bool,
    /// True for `noreturn` extern functions — the call never returns control
    /// (e.g., `exit`, `abort`). Type system treats the call as `Never`; the IR
    /// terminates the basic block with `unreachable` after the call.
    pub is_noreturn: bool,
}

#[derive(Debug, Clone)]
pub enum FunctionBody {
    Block(Block),
    Expression(Box<Spanned<Expr>>),
    Declaration,
    /// Extern binding: body is a C symbol name, e.g. `extern int abs(int x) = "abs"`
    Extern(String),
}

#[derive(Debug, Clone)]
pub struct Param {
    pub type_: Spanned<Type>,
    pub ownership: Ownership,
    pub name: Spanned<String>,
    pub default: Option<Spanned<Expr>>,
    /// True for params declared `meta name` — carry no runtime value; operator token only.
    pub is_meta_op: bool,
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
    /// Also supports glob: `from xtd.log import LogLevel.*`
    /// Also supports aliasing: `from std.math import sin as msin`
    /// Also supports module-level wildcard: `from std.math import *`
    /// Glob names are in `glob_types`; they import the type + all its variants bare.
    From {
        path: Vec<Spanned<String>>,
        names: Vec<ImportName>,
        /// Type names imported with `.*` — bring type + all variants into scope.
        glob_types: Vec<Spanned<String>>,
        /// True for module-level wildcard `from X import *` — bind all
        /// public names from the module into the current scope.
        wildcard: bool,
        span: Span,
    },
}

/// One name in a `from X import ...` list, optionally aliased with `as Z`.
/// `name` is the source-module name; `alias` is the local rebinding (when present).
#[derive(Debug, Clone)]
pub struct ImportName {
    pub name: Spanned<String>,
    pub alias: Option<Spanned<String>>,
}

impl ImportName {
    /// The name the import is bound under in the importing scope.
    /// Equals `alias` when aliased, `name` otherwise.
    pub fn local_name(&self) -> &Spanned<String> {
        self.alias.as_ref().unwrap_or(&self.name)
    }
}

impl ImportStmt {
    pub fn span(&self) -> Span {
        match self {
            ImportStmt::Simple { span, .. }
            | ImportStmt::Grouped { span, .. }
            | ImportStmt::From { span, .. } => *span,
        }
    }
}

// ══════════════════════════════════════════════════════════════
// Type Aliases & Newtypes
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub name: Spanned<String>,
    pub generic_params: Option<Spanned<GenericParams>>,
    pub type_: Spanned<Type>,
    pub visibility: Visibility,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct NewtypeDef {
    pub name: Spanned<String>,
    pub inner_type: Spanned<Type>,
    pub visibility: Visibility,
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
    Type {
        name: Spanned<String>,
        bounds: Vec<Spanned<TraitBound>>,
    },
    Const {
        type_: Spanned<Type>,
        name: Spanned<String>,
    },
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

    /// Function type: `int(int, int)` or `int(&MyStruct, int)`
    Function {
        return_type: Box<Spanned<Type>>,
        params: Vec<Spanned<Type>>,
        param_ownerships: Vec<Ownership>,
    },

    /// Borrowed reference: `Type &`
    Ref(Box<Spanned<Type>>),

    /// Owned/moved value: `Type !`
    Owned(Box<Spanned<Type>>),

    /// Pointer type: `T*` — only valid in extern "C" context.
    /// Means "pass as const T* in C" (take address of struct value).
    Pointer(Box<Spanned<Type>>),

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
    CStr,
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
    /// String literal with optional pre-parsed interpolation expressions.
    /// The first field is the lexer-emitted literal (text + segment list).
    /// The second is one parsed `Expr` per `StringSegment::Interpolation`,
    /// in order — populated by the parser for `Format`-kind strings so that
    /// semantic passes (resolution, typecheck, method-mangling rewriter)
    /// process the interpolation expressions naturally. Empty for non-format
    /// strings and for synthetic literals constructed during lowering.
    StringLiteral(crate::lexer::token::StringLiteral, Vec<Spanned<Expr>>),
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

    // ── Default operator (??) ──
    DefaultOp {
        lhs: Box<Spanned<Expr>>,
        rhs: Box<Spanned<Expr>>,
    },

    // ── Rethrow (inline error transform) ──
    /// `expr rethrow (Type name): transform_expr`  (binding form)
    /// `expr rethrow transform_expr`               (bare form)
    Rethrow {
        expr: Box<Spanned<Expr>>,
        error_binding: Option<(Spanned<Type>, Spanned<String>)>,
        transform: Box<Spanned<Expr>>,
    },

    // ── Catch (error recovery) ──
    /// `expr catch (name): recovery_expr`
    Catch {
        expr: Box<Spanned<Expr>>,
        error_binding: Spanned<String>,
        recovery: Box<Spanned<Expr>>,
    },

    // ── Fault catch (local, unwind-free recovery from a faultable op) ──
    /// A NEW form, distinct from the `Result` `catch` above (error-model.md
    /// §11). Recovers an overflow/div-by-zero fault from the faultable ops
    /// emitted DIRECTLY into `expr`'s own basic blocks (not through any call).
    /// Two spellings, distinguished by `pattern`:
    ///   - pattern form `(expr) catch Fault.Overflow: handler` — catches that
    ///     one variant; no value is bound;
    ///   - binding form `(expr) catch f: handler` — binds the constructed
    ///     `Fault` value to `f` (the handler typically `match f`es it).
    FaultCatch {
        expr: Box<Spanned<Expr>>,
        pattern: FaultCatchPattern,
        handler: Box<Spanned<Expr>>,
    },

    // ── Move (!) ──
    Move {
        expr: Box<Spanned<Expr>>,
    },

    // ── D29: postfix error-propagation mark (`call()!`) ──
    // Marks a fallible-call site as activating its error channel (propagate /
    // `catch` / `rethrow`). Semantically the identity on the success value; the
    // node exists so the fallibility is VISIBLE at the site and the checker can
    // enforce the mandatory mark (D29 / `decisions.md` 2026-07-17 amendment).
    Propagate {
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
    DictLiteral(Vec<(Spanned<Expr>, Spanned<Expr>)>),

    // ── Struct construction ──
    StructLiteral {
        name: Spanned<String>,
        generic_args: Option<Vec<Spanned<Type>>>,
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
        /// `spawn unchecked <call_expr>` — opt out of the borrow
        /// checker's spawn-capture safety checks. The programmer
        /// guarantees manual synchronization. Grep-able escape hatch.
        unchecked: bool,
    },
    /// `spawn blocking <call_expr>` — run on the expandable blocking pool.
    SpawnBlocking {
        expr: Box<Spanned<Expr>>,
        /// Same opt-out as `Expr::Spawn::unchecked`.
        unchecked: bool,
    },

    // ── Is pattern test ──
    Is {
        expr: Box<Spanned<Expr>>,
        negated: bool,
        pattern: Spanned<Pattern>,
    },

    // ── It (implicit closure parameter) ──
    It,

    // ── Dot-shorthand variant: .Red(), .Blue(42) ──
    /// Resolved to `EnumType.Variant(args)` using the expected type from context.
    DotShorthand {
        variant: Spanned<String>,
        args: Vec<Spanned<CallArg>>,
    },

    /// `a meta[op_name] b` — compile-time operator placeholder in a generic body.
    /// Substituted with a real `BinaryOp` during monomorphization when the `meta op`
    /// param is instantiated with a concrete operator token.
    MetaOpInfix {
        left:    Box<Spanned<Expr>>,
        op_name: String,
        right:   Box<Spanned<Expr>>,
    },

    /// `meta +` / `meta -` etc. at a call site — passes an operator token to a
    /// `meta op` parameter.  Has no runtime value; filtered out before GIR lowering.
    MetaOpToken(BinaryOp),
}

/// How an [`Expr::FaultCatch`] selects which fault(s) it intercepts.
#[derive(Debug, Clone, PartialEq)]
pub enum FaultCatchPattern {
    /// Pattern form `catch Fault.Overflow:` — catches exactly the named
    /// variant; faults of other variants in the wrapped expression panic.
    /// `qualifier` is the enum prefix (`Fault`) — validated at typecheck so a
    /// wrong qualifier (`Bogus.Overflow`) is rejected rather than silently
    /// accepted; `variant` is the variant name (e.g. `"Overflow"`).
    Variant {
        qualifier: Spanned<String>,
        variant: Spanned<String>,
    },
    /// Binding form `catch f:` — catches ANY fault in the wrapped expression,
    /// constructs the corresponding `Fault` value, and binds it to the name.
    Binding(Spanned<String>),
}

#[derive(Debug, Clone)]
pub struct ClosureParam {
    pub type_: Option<Spanned<Type>>,
    pub ownership: Ownership,
    pub name: Spanned<String>,
    /// Source-level tuple destructuring: `((T1 x, T2 y))`. When Some, `name` is
    /// a compiler-synthesised `__dp_N` placeholder; the closure body's first
    /// `destructure.as_ref().unwrap().len()` statements are the desugared
    /// `T1 x = __dp_N._0; T2 y = __dp_N._1; ...` prelude. The formatter prints
    /// the destructure pattern in source form and skips the prelude statements.
    pub destructure: Option<Vec<DestructureBinding>>,
}

/// One binding inside a `((T1 x, T2 y))` closure-param tuple destructure.
#[derive(Debug, Clone)]
pub struct DestructureBinding {
    pub type_: Spanned<Type>,
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
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
    // D26 fallible arithmetic (Round XXXIII Batch C1) — each `+!` / `-!` / etc
    // produces `Result[T, ArithError]` on integer operands; a plain trap becomes
    // a value in the ONE error channel (D23), auto-propagates via D29 disposition.
    AddFallible,
    SubFallible,
    MulFallible,
    DivFallible,
    RemFallible,
    ShlFallible,
    ShrFallible,
}

impl BinaryOp {
    /// True for the D26 fallible arithmetic operators (`+!` / `-!` / `*!` /
    /// `/!` / `%!` / `<<!` / `>>!`). Typed check — never a name/string match
    /// (layering rule 2). Called from typecheck (auto-infer body walk,
    /// integer-only reject) and lowering (build FaultableBinOp with
    /// ArithError handler).
    pub fn is_fallible_arith(self) -> bool {
        matches!(
            self,
            BinaryOp::AddFallible
                | BinaryOp::SubFallible
                | BinaryOp::MulFallible
                | BinaryOp::DivFallible
                | BinaryOp::RemFallible
                | BinaryOp::ShlFallible
                | BinaryOp::ShrFallible,
        )
    }
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Spanned<Pattern>,
    pub guard: Option<Spanned<Expr>>,
    pub body: Spanned<Expr>,
    pub span: Span,
}

/// A single item inside a `match` arm list.
/// Most items are concrete arms; a `MetaFor` item is a compile-time loop that
/// generates arms at monomorphization time and is expanded by the meta eval pass.
#[derive(Debug, Clone)]
pub enum MatchItem {
    Arm(MatchArm),
    MetaFor {
        vars: Vec<Spanned<String>>,
        range: Spanned<Expr>,
        arm_template: MatchArm,
        span: Span,
    },
}

impl MatchItem {
    pub fn arm(&self) -> Option<&MatchArm> {
        match self { MatchItem::Arm(a) => Some(a), _ => None }
    }
    pub fn arm_mut(&mut self) -> Option<&mut MatchArm> {
        match self { MatchItem::Arm(a) => Some(a), _ => None }
    }
}

// ══════════════════════════════════════════════════════════════
// Select (channel multiplexing)
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub enum SelectOp {
    Recv {
        type_: Spanned<Type>,
        name: Spanned<String>,
        channel: Spanned<Expr>,
    },
    Send {
        channel: Spanned<Expr>,
        value: Spanned<Expr>,
    },
}

#[derive(Debug, Clone)]
pub struct SelectArm {
    pub op: SelectOp,
    pub body: Block,
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

    /// Dot-shorthand pattern: .Red(), .Blue(n)
    /// Resolved to `EnumType.Variant(fields)` using the scrutinee type from context.
    DotShorthand {
        variant: Spanned<String>,
        fields: Vec<Spanned<Pattern>>,
    },
}

// ══════════════════════════════════════════════════════════════
// Shared Bindings
// ══════════════════════════════════════════════════════════════

/// Synchronization strategy for `shared` bindings.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SharedKind {
    /// Not shared — plain binding.
    None,
    /// `shared` — compiler picks sync strategy via CFA.
    Auto,
    /// `shared(rwlock)` — user override: ARC + RwLock.
    RwLock,
    /// `shared(atomic)` — user override: ARC + Atomic (scalars only).
    Atomic,
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
        shared: SharedKind,
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

    /// on error: block
    OnError {
        body: Block,
    },

    /// break (takes no value — loops are not expressions; D19)
    Break,

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
        arms: Vec<MatchItem>,
        else_arm: Option<Block>,
    },

    /// select/case statement for channel multiplexing
    Select {
        arms: Vec<SelectArm>,
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

    /// Named scope block: `identifier:\n    body`.
    /// Mid-function drop zone — variables created inside are dropped at block exit.
    /// Thread safety follows from `Task[T]` RAII: tasks spawned inside are joined
    /// (dropped) before the scope exits, so outer borrows remain valid.
    NamedScope {
        name: Spanned<String>,
        body: Block,
    },

    /// assert condition [, message]
    Assert {
        condition: Spanned<Expr>,
        message: Option<Spanned<Expr>>,
    },

    /// assert return <condition> [, message] — postcondition checked at every return site.
    /// `return` in the condition binds to the return value (represented as `Expr::Identifier("__return__")`).
    AssertReturn {
        condition: Spanned<Expr>,
        message: Option<Spanned<Expr>>,
    },

    /// snapshot "name" expr — capture a serialized value for snapshot testing.
    /// Only valid inside test blocks. The expression type must be a primitive or implement Serializable.
    Snapshot {
        name: Spanned<String>,
        value: Spanned<Expr>,
    },

    /// Nested item definition
    Item(Box<Item>),

    /// meta if <expr>: <stmts> [elif <expr>: <stmts>]* [else: <stmts>]
    /// Delayed compile-time conditional inside generic function/method bodies.
    /// Evaluated at monomorphization time when type parameters are concrete.
    MetaIf {
        condition: Spanned<Expr>,
        then_body: Block,
        elif_branches: Vec<(Spanned<Expr>, Block)>,
        else_body: Option<Block>,
        span: Span,
    },

    /// meta for <name> in <range_expr>: <stmts>
    /// meta for <name>, <name> in <expr>: <stmts>  (multi-var destructuring)
    /// Compile-time loop unrolling inside generic function/method bodies.
    /// Evaluated at monomorphization time when type parameters are concrete.
    /// `vars.len() == 1`: single binding (integer ranges, field_names, …).
    /// `vars.len() >= 2`: positional destructure — each list item must itself be a list.
    MetaFor {
        vars: Vec<Spanned<String>>,
        range: Spanned<Expr>,
        body: Block,
        span: Span,
    },

    /// meta match <expr>: case <val>: <block> ... [else: <block>]
    /// Compile-time match on a meta value inside a generic function body.
    /// Evaluated at monomorphization time when type parameters are concrete.
    MetaMatch {
        scrutinee: Spanned<Expr>,
        arms: Vec<(Spanned<Expr>, Block)>,  // (case value expr, body)
        else_arm: Option<Block>,
        span: Span,
    },

    /// meta while <condition>: <block>
    /// Compile-time while loop inside generic function/method bodies.
    /// Evaluated at monomorphization time when type parameters are concrete.
    MetaWhile {
        condition: Spanned<Expr>,
        body: Block,
        span: Span,
    },

    /// `meta const <name> = <expr>`
    /// Compile-time constant binding inside a generic function/method body.
    /// Evaluated at monomorphization time; value is available to subsequent
    /// statements in the same block via the delayed meta environment.
    MetaConst {
        name:  Spanned<String>,
        value: Spanned<Expr>,
        span:  Span,
    },

    /// `meta log <expr> [, <expr> ...]`
    /// Compile-time diagnostic: evaluates each expression and prints to stderr.
    /// Evaluated at monomorphization time in generic bodies; at Phase 0 at file scope.
    MetaLog {
        args: Vec<Spanned<Expr>>,
        span: Span,
    },
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

// ══════════════════════════════════════════════════════════════
// Meta (Compile-Time) Declarations
// ══════════════════════════════════════════════════════════════

/// `meta int X = 1024` — compile-time constant.
#[derive(Debug, Clone)]
pub struct MetaConst {
    pub type_: Spanned<Type>,
    pub name: Spanned<String>,
    pub value: Spanned<Expr>,
    pub span: Span,
}

/// The right-hand side of a `meta type` declaration.
#[derive(Debug, Clone)]
pub enum MetaTypeRhs {
    /// `meta type Num = int`
    Plain(Spanned<Type>),
    /// `meta type Map = Dict if feature("ordered") else HashMap`
    Conditional {
        then_type: Spanned<Type>,
        condition: Spanned<Expr>,
        else_type: Spanned<Type>,
    },
    /// `meta type Word = sized_int(arch_word_bits())`
    Call {
        callee: Spanned<String>,
        args: Vec<Spanned<Expr>>,
    },
}

/// `meta type Vec = Vector[int]` — compile-time type alias.
#[derive(Debug, Clone)]
pub struct MetaType {
    pub name: Spanned<String>,
    pub rhs: MetaTypeRhs,
    pub span: Span,
}

/// `meta type sized_int(int bits): ...` — compile-time type function.
#[derive(Debug, Clone)]
pub struct MetaTypeFunc {
    pub name: Spanned<String>,
    pub params: Vec<Spanned<Param>>,
    pub body: Block,
    pub span: Span,
}

/// `meta assert X > 0, "msg"` — compile-time assertion.
#[derive(Debug, Clone)]
pub struct MetaAssert {
    pub condition: Spanned<Expr>,
    pub message: Option<Spanned<Expr>>,
    pub span: Span,
}

/// `meta log <expr> [, <expr> ...]`
/// Compile-time diagnostic: evaluates each expression and prints to stderr during compilation.
#[derive(Debug, Clone)]
pub struct MetaLog {
    pub args: Vec<Spanned<Expr>>,
    pub span: Span,
}

/// `meta if <expr>: <block> [elif <expr>: <block>]* [else: <block>]`
/// Conditional compilation — only the taken branch is emitted.
#[derive(Debug, Clone)]
pub struct MetaIf {
    pub condition: Spanned<Expr>,
    pub then_items: Vec<Spanned<Item>>,
    pub elif_branches: Vec<(Spanned<Expr>, Vec<Spanned<Item>>)>,
    pub else_items: Option<Vec<Spanned<Item>>>,
    pub span: Span,
}
