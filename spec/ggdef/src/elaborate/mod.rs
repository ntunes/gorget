//! Elaboration: production surface AST → GGC (RFC §2.4), for the Increment-A
//! subset only.
//!
//! This is the spec-owned front half. It shares the production **lexer +
//! parser + AST** (that is the only dependency; the import ratchet forbids
//! `ir`/`semantic`/`lir`/`bir`/`backend`). It performs the desugarings and
//! mode-tag resolution the A fixtures need — and NOTHING it cannot do
//! faithfully: any surface construct outside the A subset is a hard
//! `ElabError`, never a silent approximation (per the brief's stop-and-report
//! rule and "Don't redesign around compiler gaps").
//!
//! A-subset desugarings implemented here:
//!   * collection literals `[a, b, c]` → `Vector` construct with owning elems;
//!   * f-strings → `FString` parts (literal / interpolation);
//!   * `for pat in coll:` → an index `while` loop with a `Borrow`-view var;
//!   * `elif` chains → nested `if`;
//!   * `x += e` → `x = x + e`;
//!   * `print(e)` → the `Print` output effect;
//!   * method calls (`push`/`set`/`len`/`clone`) → GGC `Method`/`Clone`;
//!   * mode tags from syntax (`bare`/`&`/`!`) at every binding/arg position;
//!   * `from std.collections import ...` → a parse-and-DISCARD no-op (the
//!     imported types are prelude-available in A; the full shim list is B).

use std::collections::HashSet;

use gorget::lexer::token::{StringKind, StringSegment};
use gorget::parser::ast;
use gorget::span::{Span, Spanned};

use crate::ggc::{
    BinOp, BuiltinMethod, ConstructKind, Expr, FPart, Function, Param, Program, Source, Stmt,
    StructDef, UnOp,
};

/// A faithful-elaboration failure: a surface construct outside the A subset,
/// or a shape the elaborator cannot lower without inventing semantics.
#[derive(Debug, Clone)]
pub struct ElabError {
    pub message: String,
    pub span: Span,
}

impl ElabError {
    fn new(message: impl Into<String>, span: Span) -> Self {
        ElabError { message: message.into(), span }
    }
}

type ElabResult<T> = Result<T, ElabError>;

/// Elaborate a parsed surface module into a GGC program.
pub fn elaborate(module: &ast::Module) -> ElabResult<Program> {
    let mut el = Elaborator::default();
    let items = module.all_items();

    // Pass 1: collect struct layouts and function names (the tiny resolver).
    for item in &items {
        match item {
            ast::Item::Struct(sd) => {
                let name = sd.name.node.clone();
                let fields = sd.fields.iter().map(|f| f.node.name.node.clone()).collect();
                el.structs.push(StructDef { name: name.clone(), fields });
                el.struct_names.insert(name);
            }
            ast::Item::Function(fd) => {
                el.func_names.insert(fd.name.node.clone());
            }
            // Imports are a discard no-op in A (types are prelude-available).
            ast::Item::Import(_) => {}
            // Nested modules are already flattened by `all_items`.
            ast::Item::Module { .. } => {}
            other => {
                return Err(ElabError::new(
                    format!("item kind {} is outside the Increment-A subset", item_kind(other)),
                    item_span(other),
                ));
            }
        }
    }

    // Pass 2: elaborate each function body.
    let mut functions = Vec::new();
    for item in &items {
        if let ast::Item::Function(fd) = item {
            functions.push(el.elaborate_function(fd)?);
        }
    }

    Ok(Program { functions, structs: el.structs })
}

#[derive(Default)]
struct Elaborator {
    structs: Vec<StructDef>,
    struct_names: HashSet<String>,
    func_names: HashSet<String>,
    gensym: usize,
}

impl Elaborator {
    fn fresh(&mut self, hint: &str) -> String {
        let n = self.gensym;
        self.gensym += 1;
        format!("__{hint}_{n}")
    }

    fn elaborate_function(&mut self, fd: &ast::FunctionDef) -> ElabResult<Function> {
        let mut params = Vec::with_capacity(fd.params.len());
        for p in &fd.params {
            if p.node.is_meta_op {
                return Err(ElabError::new("`meta op` params are phase 2", p.span));
            }
            params.push(Param {
                name: p.node.name.node.clone(),
                mode: mode_of(p.node.ownership),
                span: p.span,
            });
        }
        let body = match &fd.body {
            ast::FunctionBody::Block(block) => self.elaborate_block(block)?,
            ast::FunctionBody::Expression(e) => {
                // Expression-body function: evaluate and return the value.
                vec![Stmt::Return { value: Some(self.elaborate_expr(e)?), span: e.span }]
            }
            ast::FunctionBody::Declaration | ast::FunctionBody::Extern(_) => {
                return Err(ElabError::new(
                    "extern / declaration-only functions are out of spec v1",
                    fd.span,
                ));
            }
        };
        Ok(Function { name: fd.name.node.clone(), params, body, span: fd.span })
    }

    fn elaborate_block(&mut self, block: &ast::Block) -> ElabResult<Vec<Stmt>> {
        let mut out = Vec::new();
        for stmt in &block.stmts {
            out.extend(self.elaborate_stmt(stmt)?);
        }
        Ok(out)
    }

    /// One surface statement may lower to several GGC statements (the for-loop
    /// desugar), so this returns a vector.
    fn elaborate_stmt(&mut self, stmt: &Spanned<ast::Stmt>) -> ElabResult<Vec<Stmt>> {
        let span = stmt.span;
        match &stmt.node {
            ast::Stmt::VarDecl { pattern, value, .. } => {
                let name = binding_name(pattern)?;
                let source = self.bind_source(value)?;
                Ok(vec![Stmt::Bind { name, source, span }])
            }

            ast::Stmt::Assign { target, value } => {
                let target_expr = self.elaborate_expr(target)?;
                let value_src = self.owning_source_from_expr(value)?;
                Ok(vec![Stmt::Assign { target: target_expr, value: value_src, span }])
            }

            ast::Stmt::CompoundAssign { target, op, value } => {
                // `x op= e`  →  `x = x op e`
                let target_expr = self.elaborate_expr(target)?;
                let lhs = self.elaborate_expr(target)?;
                let rhs = self.elaborate_expr(value)?;
                let combined = Expr::Binary(map_binop(*op, span)?, Box::new(lhs), Box::new(rhs));
                Ok(vec![Stmt::Assign { target: target_expr, value: Source::Value(combined), span }])
            }

            ast::Stmt::Expr(e) => {
                // `print(...)` is the output effect, not an ordinary call.
                if let Some(arg) = as_print_call(e) {
                    let expr = self.elaborate_expr(arg)?;
                    return Ok(vec![Stmt::Print { expr, span }]);
                }
                Ok(vec![Stmt::Expr { expr: self.elaborate_expr(e)?, span }])
            }

            ast::Stmt::Return(opt) => {
                let value = match opt {
                    Some(e) => Some(self.elaborate_expr(e)?),
                    None => None,
                };
                Ok(vec![Stmt::Return { value, span }])
            }

            ast::Stmt::Break(None) => Ok(vec![Stmt::Break { span }]),
            ast::Stmt::Break(Some(_)) => {
                Err(ElabError::new("`break <value>` is outside the Increment-A subset", span))
            }
            ast::Stmt::Continue => Ok(vec![Stmt::Continue { span }]),
            ast::Stmt::Pass => Ok(vec![]),

            ast::Stmt::If { condition, then_body, elif_branches, else_body } => {
                Ok(vec![self.build_if(condition, then_body, elif_branches, else_body.as_ref())?])
            }

            ast::Stmt::While { condition, body, else_body } => {
                if else_body.is_some() {
                    return Err(ElabError::new("`while ... else` is outside the A subset", span));
                }
                let cond = self.elaborate_expr(condition)?;
                let body = self.elaborate_block(body)?;
                Ok(vec![Stmt::While { cond, body, span }])
            }

            ast::Stmt::Loop { body } => {
                Ok(vec![Stmt::Loop { body: self.elaborate_block(body)?, span }])
            }

            ast::Stmt::For { pattern, ownership, iterable, body, else_body } => {
                self.desugar_for(pattern, *ownership, iterable, body, else_body.as_ref(), span)
            }

            other => Err(ElabError::new(
                format!("statement `{}` is outside the Increment-A subset", stmt_kind(other)),
                span,
            )),
        }
    }

    fn build_if(
        &mut self,
        condition: &Spanned<ast::Expr>,
        then_body: &ast::Block,
        elifs: &[(Spanned<ast::Expr>, ast::Block)],
        else_body: Option<&ast::Block>,
    ) -> ElabResult<Stmt> {
        let cond = self.elaborate_expr(condition)?;
        let then_ = self.elaborate_block(then_body)?;
        let else_ = match elifs.split_first() {
            Some(((elif_cond, elif_body), rest)) => {
                vec![self.build_if(elif_cond, elif_body, rest, else_body)?]
            }
            None => match else_body {
                Some(b) => self.elaborate_block(b)?,
                None => Vec::new(),
            },
        };
        Ok(Stmt::If { cond, then_, else_, span: condition.span })
    }

    /// `for var in coll:` → an index `while` with a `Borrow`-view element var.
    fn desugar_for(
        &mut self,
        pattern: &Spanned<ast::Pattern>,
        ownership: ast::Ownership,
        iterable: &Spanned<ast::Expr>,
        body: &ast::Block,
        else_body: Option<&ast::Block>,
        span: Span,
    ) -> ElabResult<Vec<Stmt>> {
        if else_body.is_some() {
            return Err(ElabError::new("`for ... else` is outside the A subset", span));
        }
        if ownership != ast::Ownership::Borrow {
            // `for x in &coll` / `for x in !coll` (write-through / draining) is B.
            return Err(ElabError::new("`for &`/`for !` iteration is Increment B", span));
        }
        let var = binding_name(pattern)?;
        let coll = self.fresh("coll");
        let idx = self.fresh("i");

        // `__coll = <iterable owning source>`
        let coll_src = self.owning_source_from_expr(iterable)?;
        // `__i = 0`
        let idx_bind = Stmt::Bind {
            name: idx.clone(),
            source: Source::Value(Expr::Int(0)),
            span,
        };
        // condition: `__i < __coll.len()`
        let cond = Expr::Binary(
            BinOp::Lt,
            Box::new(Expr::Local(idx.clone())),
            Box::new(Expr::Method {
                recv: Box::new(Expr::Local(coll.clone())),
                method: BuiltinMethod::Len,
                args: Vec::new(),
            }),
        );
        // loop body: bind the element (Borrow view), run body, `__i += 1`.
        let mut while_body = Vec::new();
        while_body.push(Stmt::Bind {
            name: var,
            source: Source::BorrowView(Expr::Index(
                Box::new(Expr::Local(coll.clone())),
                Box::new(Expr::Local(idx.clone())),
            )),
            span,
        });
        while_body.extend(self.elaborate_block(body)?);
        while_body.push(Stmt::Assign {
            target: Expr::Local(idx.clone()),
            value: Source::Value(Expr::Binary(
                BinOp::Add,
                Box::new(Expr::Local(idx.clone())),
                Box::new(Expr::Int(1)),
            )),
            span,
        });

        Ok(vec![
            Stmt::Bind { name: coll, source: coll_src, span },
            idx_bind,
            Stmt::While { cond, body: while_body, span },
        ])
    }

    // ── Source classification (the copy/move/borrow decision) ──────────────

    /// The RHS of a `let` binding (an implicit-copy position, but `&` makes a
    /// write-through alias and `!` a move).
    fn bind_source(&mut self, value: &Spanned<ast::Expr>) -> ElabResult<Source> {
        match &value.node {
            ast::Expr::Move { expr } => Ok(Source::Move(self.elaborate_expr(expr)?)),
            ast::Expr::MutableBorrow { expr } => Ok(Source::WriteThrough(self.elaborate_expr(expr)?)),
            _ if is_clone_call(&value.node) => Ok(Source::Value(self.elaborate_expr(value)?)),
            _ if ast_is_place(&value.node) => Ok(Source::Copy(self.elaborate_expr(value)?)),
            _ => Ok(Source::Value(self.elaborate_expr(value)?)),
        }
    }

    /// A value in an OWNING position from a bare expression (assign RHS, array
    /// element). No write-through alias is permitted here.
    fn owning_source_from_expr(&mut self, value: &Spanned<ast::Expr>) -> ElabResult<Source> {
        match &value.node {
            ast::Expr::Move { expr } => Ok(Source::Move(self.elaborate_expr(expr)?)),
            ast::Expr::MutableBorrow { .. } => {
                Err(ElabError::new("`&`-alias in an owning position is not valid", value.span))
            }
            _ if is_clone_call(&value.node) => Ok(Source::Value(self.elaborate_expr(value)?)),
            _ if ast_is_place(&value.node) => Ok(Source::Copy(self.elaborate_expr(value)?)),
            _ => Ok(Source::Value(self.elaborate_expr(value)?)),
        }
    }

    /// A value in an OWNING position from a call-arg (collection put, struct/
    /// enum field init): the sigil rides `CallArg.ownership`.
    fn owning_source_from_arg(&mut self, arg: &ast::CallArg) -> ElabResult<Source> {
        match arg.ownership {
            ast::Ownership::Move => Ok(Source::Move(self.elaborate_expr(&arg.value)?)),
            ast::Ownership::MutableBorrow => {
                Err(ElabError::new("`&`-alias into an owning position is not valid", arg.value.span))
            }
            ast::Ownership::Borrow => {
                if is_clone_call(&arg.value.node) {
                    Ok(Source::Value(self.elaborate_expr(&arg.value)?))
                } else if ast_is_place(&arg.value.node) {
                    Ok(Source::Copy(self.elaborate_expr(&arg.value)?))
                } else {
                    Ok(Source::Value(self.elaborate_expr(&arg.value)?))
                }
            }
        }
    }

    /// A function-call argument: bare params are BORROW **views**, so a bare
    /// place becomes a view (not a copy). `&`/`!` ride `CallArg.ownership`.
    fn call_arg_source(&mut self, arg: &ast::CallArg) -> ElabResult<Source> {
        match arg.ownership {
            ast::Ownership::Move => Ok(Source::Move(self.elaborate_expr(&arg.value)?)),
            ast::Ownership::MutableBorrow => Ok(Source::WriteThrough(self.elaborate_expr(&arg.value)?)),
            ast::Ownership::Borrow => {
                if ast_is_place(&arg.value.node) {
                    Ok(Source::BorrowView(self.elaborate_expr(&arg.value)?))
                } else {
                    Ok(Source::Value(self.elaborate_expr(&arg.value)?))
                }
            }
        }
    }

    // ── Expressions ────────────────────────────────────────────────────────

    fn elaborate_expr(&mut self, expr: &Spanned<ast::Expr>) -> ElabResult<Expr> {
        let span = expr.span;
        match &expr.node {
            ast::Expr::IntLiteral(i) => Ok(Expr::Int(*i)),
            ast::Expr::FloatLiteral(f) => Ok(Expr::Float(*f)),
            ast::Expr::BoolLiteral(b) => Ok(Expr::Bool(*b)),
            ast::Expr::StringLiteral(lit, interps) => self.elaborate_string(lit, interps, span),

            ast::Expr::Identifier(name) => Ok(Expr::Local(name.clone())),

            ast::Expr::FieldAccess { object, field } => {
                Ok(Expr::Field(Box::new(self.elaborate_expr(object)?), field.node.clone()))
            }
            ast::Expr::TupleFieldAccess { object, index } => {
                Ok(Expr::TupleField(Box::new(self.elaborate_expr(object)?), *index))
            }
            ast::Expr::Index { object, index } => Ok(Expr::Index(
                Box::new(self.elaborate_expr(object)?),
                Box::new(self.elaborate_expr(index)?),
            )),

            ast::Expr::BinaryOp { left, op, right } => Ok(Expr::Binary(
                map_binop(*op, span)?,
                Box::new(self.elaborate_expr(left)?),
                Box::new(self.elaborate_expr(right)?),
            )),
            ast::Expr::UnaryOp { op, operand } => {
                Ok(Expr::Unary(map_unop(*op, span)?, Box::new(self.elaborate_expr(operand)?)))
            }

            ast::Expr::Move { expr } => {
                // Bare `!x` in a read position (e.g. `print(!x)`): read the
                // moved value. Faithful move-kill is applied only at binding /
                // owning positions, which route through the `Source` helpers.
                self.elaborate_expr(expr)
            }

            ast::Expr::Call { callee, generic_args, args } => {
                self.elaborate_call(callee, generic_args.is_some(), args, span)
            }

            ast::Expr::MethodCall { receiver, method, args, .. } => {
                self.elaborate_method(receiver, &method.node, args, span)
            }

            ast::Expr::ArrayLiteral(elems) => {
                let mut out = Vec::with_capacity(elems.len());
                for e in elems {
                    out.push(self.owning_source_from_expr(e)?);
                }
                Ok(Expr::Construct { kind: ConstructKind::Vector, args: out })
            }
            ast::Expr::TupleLiteral(elems) => {
                let mut out = Vec::with_capacity(elems.len());
                for e in elems {
                    out.push(self.owning_source_from_expr(e)?);
                }
                Ok(Expr::Construct { kind: ConstructKind::Tuple, args: out })
            }
            ast::Expr::StructLiteral { name, args, .. } => {
                let mut out = Vec::with_capacity(args.len());
                for e in args {
                    out.push(self.owning_source_from_expr(e)?);
                }
                Ok(Expr::Construct { kind: ConstructKind::Struct(name.node.clone()), args: out })
            }

            other => Err(ElabError::new(
                format!("expression `{}` is outside the Increment-A subset", expr_kind(other)),
                span,
            )),
        }
    }

    fn elaborate_string(
        &mut self,
        lit: &gorget::lexer::token::StringLiteral,
        interps: &[Spanned<ast::Expr>],
        span: Span,
    ) -> ElabResult<Expr> {
        match lit.kind {
            StringKind::Format => {
                let mut parts = Vec::new();
                let mut next_interp = 0usize;
                for seg in &lit.segments {
                    match seg {
                        StringSegment::Literal(s) => parts.push(FPart::Lit(s.clone())),
                        StringSegment::Interpolation(_, _) => {
                            let e = interps.get(next_interp).ok_or_else(|| {
                                ElabError::new("f-string interpolation without a parsed expr", span)
                            })?;
                            next_interp += 1;
                            parts.push(FPart::Interp(self.elaborate_expr(e)?));
                        }
                    }
                }
                Ok(Expr::FString(parts))
            }
            StringKind::Normal | StringKind::Raw | StringKind::MultiLine => {
                Ok(Expr::Str(lit.as_plain_text()))
            }
            StringKind::Byte | StringKind::CStr => {
                Err(ElabError::new("byte / cstr literals are outside the A subset", span))
            }
        }
    }

    fn elaborate_call(
        &mut self,
        callee: &Spanned<ast::Expr>,
        has_generic_args: bool,
        args: &[Spanned<ast::CallArg>],
        span: Span,
    ) -> ElabResult<Expr> {
        let ast::Expr::Identifier(name) = &callee.node else {
            return Err(ElabError::new("only named callees are supported in A", callee.span));
        };
        if name == "print" {
            return Err(ElabError::new("`print` may only appear as a statement in A", span));
        }
        // `Vector[T]()` — an empty (or literal-seeded) collection construct.
        if name == "Vector" && has_generic_args {
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                out.push(self.owning_source_from_arg(&a.node)?);
            }
            return Ok(Expr::Construct { kind: ConstructKind::Vector, args: out });
        }
        // Struct construction: `Res("x")`, `Person("Alice", 30)`.
        if self.struct_names.contains(name) {
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                out.push(self.owning_source_from_arg(&a.node)?);
            }
            return Ok(Expr::Construct { kind: ConstructKind::Struct(name.clone()), args: out });
        }
        // Ordinary function call.
        if self.func_names.contains(name) {
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                out.push(self.call_arg_source(&a.node)?);
            }
            return Ok(Expr::Call { func: name.clone(), args: out });
        }
        Err(ElabError::new(
            format!("unresolved callee `{name}` (unknown function/struct; may need Increment B)"),
            callee.span,
        ))
    }

    fn elaborate_method(
        &mut self,
        receiver: &Spanned<ast::Expr>,
        method: &str,
        args: &[Spanned<ast::CallArg>],
        span: Span,
    ) -> ElabResult<Expr> {
        let recv = Box::new(self.elaborate_expr(receiver)?);
        match method {
            "push" => {
                if args.len() != 1 {
                    return Err(ElabError::new("`.push` takes 1 arg", span));
                }
                Ok(Expr::Method {
                    recv,
                    method: BuiltinMethod::Push,
                    args: vec![self.owning_source_from_arg(&args[0].node)?],
                })
            }
            "set" => {
                if args.len() != 2 {
                    return Err(ElabError::new("`.set` takes 2 args", span));
                }
                Ok(Expr::Method {
                    recv,
                    method: BuiltinMethod::Set,
                    args: vec![
                        self.owning_source_from_arg(&args[0].node)?,
                        self.owning_source_from_arg(&args[1].node)?,
                    ],
                })
            }
            "len" => {
                if !args.is_empty() {
                    return Err(ElabError::new("`.len` takes no args", span));
                }
                Ok(Expr::Method { recv, method: BuiltinMethod::Len, args: Vec::new() })
            }
            "clone" => {
                if !args.is_empty() {
                    return Err(ElabError::new("`.clone` takes no args", span));
                }
                Ok(Expr::Clone(recv))
            }
            other => Err(ElabError::new(
                format!("method `.{other}()` is outside the Increment-A subset (may need Increment B)"),
                span,
            )),
        }
    }
}

// ── Small helpers ──────────────────────────────────────────────────────────

fn mode_of(o: ast::Ownership) -> crate::ggc::Mode {
    match o {
        ast::Ownership::Borrow => crate::ggc::Mode::Borrow,
        ast::Ownership::MutableBorrow => crate::ggc::Mode::WriteThrough,
        ast::Ownership::Move => crate::ggc::Mode::Move,
    }
}

/// Whether a surface expression denotes a place (a named storage location),
/// so binding/arg positions can pick copy/view rather than fresh-temp.
fn ast_is_place(e: &ast::Expr) -> bool {
    match e {
        ast::Expr::Identifier(_) | ast::Expr::SelfExpr => true,
        ast::Expr::FieldAccess { object, .. }
        | ast::Expr::TupleFieldAccess { object, .. }
        | ast::Expr::Index { object, .. } => ast_is_place(&object.node),
        _ => false,
    }
}

fn is_clone_call(e: &ast::Expr) -> bool {
    matches!(e, ast::Expr::MethodCall { method, args, .. } if method.node == "clone" && args.is_empty())
}

/// If `e` is a `print(arg)` call, return the single argument expression.
fn as_print_call(e: &Spanned<ast::Expr>) -> Option<&Spanned<ast::Expr>> {
    if let ast::Expr::Call { callee, args, .. } = &e.node {
        if let ast::Expr::Identifier(name) = &callee.node {
            if name == "print" && args.len() == 1 {
                return Some(&args[0].node.value);
            }
        }
    }
    None
}

fn binding_name(pattern: &Spanned<ast::Pattern>) -> ElabResult<String> {
    match &pattern.node {
        ast::Pattern::Binding(name) => Ok(name.clone()),
        _ => Err(ElabError::new("only simple bindings are supported in Increment A", pattern.span)),
    }
}

fn map_binop(op: ast::BinaryOp, span: Span) -> ElabResult<BinOp> {
    use ast::BinaryOp as B;
    Ok(match op {
        B::Add => BinOp::Add,
        B::Sub => BinOp::Sub,
        B::Mul => BinOp::Mul,
        B::Div => BinOp::Div,
        B::Rem | B::Mod => BinOp::Rem,
        B::Eq => BinOp::Eq,
        B::Neq => BinOp::Neq,
        B::Lt => BinOp::Lt,
        B::Gt => BinOp::Gt,
        B::LtEq => BinOp::LtEq,
        B::GtEq => BinOp::GtEq,
        B::And => BinOp::And,
        B::Or => BinOp::Or,
        other => {
            return Err(ElabError::new(
                format!("operator {other:?} is outside the Increment-A subset"),
                span,
            ));
        }
    })
}

fn map_unop(op: ast::UnaryOp, span: Span) -> ElabResult<UnOp> {
    match op {
        ast::UnaryOp::Neg => Ok(UnOp::Neg),
        ast::UnaryOp::Not => Ok(UnOp::Not),
        ast::UnaryOp::BitNot => {
            Err(ElabError::new("bitwise-not is outside the Increment-A subset", span))
        }
    }
}

// ── Diagnostic labels (for stop-and-report clarity) ────────────────────────

fn item_kind(item: &ast::Item) -> &'static str {
    match item {
        ast::Item::Function(_) => "function",
        ast::Item::Struct(_) => "struct",
        ast::Item::Enum(_) => "enum",
        ast::Item::Trait(_) => "trait",
        ast::Item::Equip(_) => "equip",
        ast::Item::Import(_) => "import",
        ast::Item::TypeAlias(_) => "type-alias",
        ast::Item::Newtype(_) => "newtype",
        ast::Item::ConstDecl(_) => "const",
        ast::Item::StaticDecl(_) => "static",
        _ => "other",
    }
}

fn item_span(item: &ast::Item) -> Span {
    match item {
        ast::Item::Function(f) => f.span,
        ast::Item::Struct(s) => s.span,
        ast::Item::Enum(e) => e.span,
        ast::Item::Trait(t) => t.span,
        ast::Item::Equip(e) => e.span,
        ast::Item::Import(i) => i.span(),
        ast::Item::Newtype(n) => n.span,
        ast::Item::ConstDecl(c) => c.span,
        ast::Item::StaticDecl(s) => s.span,
        _ => Span::dummy(),
    }
}

fn stmt_kind(s: &ast::Stmt) -> &'static str {
    match s {
        ast::Stmt::Throw(_) => "throw",
        ast::Stmt::OnError { .. } => "on-error",
        ast::Stmt::Match { .. } => "match",
        ast::Stmt::Select { .. } => "select",
        ast::Stmt::With { .. } => "with",
        ast::Stmt::Unsafe { .. } => "unsafe",
        ast::Stmt::NamedScope { .. } => "named-scope",
        ast::Stmt::Assert { .. } => "assert",
        _ => "unsupported",
    }
}

fn expr_kind(e: &ast::Expr) -> &'static str {
    match e {
        ast::Expr::Match { .. } => "match",
        ast::Expr::If { .. } => "if-expr",
        ast::Expr::Closure { .. } => "closure",
        ast::Expr::Range { .. } => "range",
        ast::Expr::NoneLiteral => "None",
        ast::Expr::DotShorthand { .. } => "dot-shorthand",
        ast::Expr::As { .. } => "as-cast",
        _ => "unsupported",
    }
}
