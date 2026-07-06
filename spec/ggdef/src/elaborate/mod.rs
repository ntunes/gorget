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
    BinOp, BuiltinMethod, CastTarget, ClosureDef, ConstructKind, EnumDef, Expr, ExprArm, FPart,
    Function, Param, Pattern, Program, Source, Stmt, StmtArm, StructDef, UnOp,
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

    // Pass 1: collect struct layouts, enum variant arities, and function names
    // (the tiny resolver).
    for item in &items {
        match item {
            ast::Item::Struct(sd) => {
                let name = sd.name.node.clone();
                let fields = sd.fields.iter().map(|f| f.node.name.node.clone()).collect();
                el.structs.push(StructDef { name: name.clone(), fields });
                el.struct_names.insert(name);
            }
            ast::Item::Enum(ed) => {
                let name = ed.name.node.clone();
                let variants = ed
                    .variants
                    .iter()
                    .map(|v| {
                        let arity = match &v.node.fields {
                            ast::VariantFields::Unit => 0,
                            ast::VariantFields::Tuple(ts) => ts.len(),
                        };
                        (v.node.name.node.clone(), arity)
                    })
                    .collect();
                el.enums.push(EnumDef { name, variants });
            }
            ast::Item::Function(fd) => {
                el.func_names.insert(fd.name.node.clone());
            }
            // Imports are a discard no-op (types are prelude-available; the
            // `std.conv.int_to_str` shim is handled at the call site).
            ast::Item::Import(_) => {}
            // Nested modules are already flattened by `all_items`.
            ast::Item::Module { .. } => {}
            other => {
                return Err(ElabError::new(
                    format!("item kind {} is outside the phase-0 subset", item_kind(other)),
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

    Ok(Program { functions, structs: el.structs, enums: el.enums, closures: el.closures })
}

#[derive(Default)]
struct Elaborator {
    structs: Vec<StructDef>,
    struct_names: HashSet<String>,
    enums: Vec<EnumDef>,
    func_names: HashSet<String>,
    closures: Vec<ClosureDef>,
    /// Local names bound anywhere in the CURRENT function (params + var decls +
    /// for-vars). Used to distinguish a closure-value call (`f()`) from an
    /// unknown-function error, and to compute closure capture sets.
    local_names: HashSet<String>,
    gensym: usize,
}

impl Elaborator {
    fn fresh(&mut self, hint: &str) -> String {
        let n = self.gensym;
        self.gensym += 1;
        format!("__{hint}_{n}")
    }

    fn elaborate_function(&mut self, fd: &ast::FunctionDef) -> ElabResult<Function> {
        // Reset + populate the per-function local-name set (params + all bound
        // names) so closure detection and capture computation are scoped.
        self.local_names.clear();
        for p in &fd.params {
            self.local_names.insert(p.node.name.node.clone());
        }
        if let ast::FunctionBody::Block(block) = &fd.body {
            collect_bound_names(block, &mut self.local_names);
        }

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

            ast::Stmt::Match { scrutinee, arms, else_arm } => {
                Ok(vec![self.elaborate_match_stmt(scrutinee, arms, else_arm.as_ref(), span)?])
            }

            other => Err(ElabError::new(
                format!("statement `{}` is outside the phase-0 subset", stmt_kind(other)),
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
            return Err(ElabError::new("`for ... else` is outside the phase-0 subset", span));
        }
        if ownership != ast::Ownership::Borrow {
            // `for x in &coll` / `for x in !coll` (write-through / draining) is B2.
            return Err(ElabError::new("`for &`/`for !` iteration is Increment B2", span));
        }
        let var = binding_name(pattern)?;

        // `for i in a..b:` → a numeric `while` loop (the loop variable is a
        // fresh int per iteration, not a Borrow view of an element).
        if let ast::Expr::Range { start, end, inclusive } = &iterable.node {
            let start_e = match start {
                Some(e) => self.elaborate_expr(e)?,
                None => Expr::Int(0),
            };
            let end_e = match end {
                Some(e) => self.elaborate_expr(e)?,
                None => return Err(ElabError::new("`for` over an open-ended range is unsupported", span)),
            };
            let cmp = if *inclusive { BinOp::LtEq } else { BinOp::Lt };
            let mut while_body = self.elaborate_block(body)?;
            while_body.push(Stmt::Assign {
                target: Expr::Local(var.clone()),
                value: Source::Value(Expr::Binary(
                    BinOp::Add,
                    Box::new(Expr::Local(var.clone())),
                    Box::new(Expr::Int(1)),
                )),
                span,
            });
            return Ok(vec![
                Stmt::Bind { name: var.clone(), source: Source::Value(start_e), span },
                Stmt::While {
                    cond: Expr::Binary(cmp, Box::new(Expr::Local(var)), Box::new(end_e)),
                    body: while_body,
                    span,
                },
            ]);
        }

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
    /// Reject named arguments at positions that bind POSITIONALLY (ordinary
    /// calls, enum/collection constructors, closure-value calls). Silently
    /// dropping the name would mis-bind (RFC discipline: never silently
    /// mis-evaluate). Struct construction reorders named args and does not
    /// come through here; call-side named-arg REORDER is an Increment-B2
    /// deliverable.
    fn reject_named_args(&self, args: &[gorget::span::Spanned<ast::CallArg>], what: &str) -> ElabResult<()> {
        for a in args {
            if let Some(n) = &a.node.name {
                return Err(ElabError::new(
                    format!("named argument `{}` is not supported at a {what} in ggdef Increment B1 (positional binding would silently mis-bind; named-arg reorder for calls is Increment B2)", n.node),
                    a.span,
                ));
            }
        }
        Ok(())
    }

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
            ast::Expr::Index { object, index } => {
                // `s[a..b]` / `v[a..b]` → a `Slice`; `x[i]` → an `Index`.
                if let ast::Expr::Range { start, end, inclusive } = &index.node {
                    let object = Box::new(self.elaborate_expr(object)?);
                    let start = self.opt_expr(start.as_deref())?;
                    let end = self.opt_expr(end.as_deref())?;
                    Ok(Expr::Slice { object, start, end, inclusive: *inclusive })
                } else {
                    Ok(Expr::Index(
                        Box::new(self.elaborate_expr(object)?),
                        Box::new(self.elaborate_expr(index)?),
                    ))
                }
            }

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

            ast::Expr::NoneLiteral => Ok(enum_construct("Option", "None", Vec::new())),

            ast::Expr::As { expr, type_ } => {
                let inner = self.elaborate_expr(expr)?;
                let target = cast_target(&type_.node, type_.span)?;
                Ok(Expr::Cast { expr: Box::new(inner), target })
            }

            ast::Expr::Closure { is_async, params, body, .. } => {
                self.elaborate_closure(*is_async, params, body, span)
            }

            ast::Expr::Match { scrutinee, arms, else_arm } => {
                let scrut = self.elaborate_expr(scrutinee)?;
                let mut ggc_arms = Vec::with_capacity(arms.len());
                for arm in arms {
                    if arm.guard.is_some() {
                        return Err(ElabError::new("match guards are outside the phase-0 subset", arm.span));
                    }
                    ggc_arms.push(ExprArm {
                        pattern: self.elaborate_pattern(&arm.pattern)?,
                        body: self.elaborate_expr(&arm.body)?,
                    });
                }
                let else_arm = match else_arm {
                    Some(e) => Some(Box::new(self.elaborate_expr(e)?)),
                    None => None,
                };
                Ok(Expr::Match { scrutinee: Box::new(scrut), arms: ggc_arms, else_arm, span })
            }

            other => Err(ElabError::new(
                format!("expression `{}` is outside the phase-0 subset", expr_kind(other)),
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
        _span: Span,
    ) -> ElabResult<Expr> {
        // `None()` — the callee is a `NoneLiteral`, not an identifier.
        if matches!(callee.node, ast::Expr::NoneLiteral) {
            return Ok(enum_construct("Option", "None", Vec::new()));
        }
        let ast::Expr::Identifier(name) = &callee.node else {
            return Err(ElabError::new("only named callees are supported in phase 0", callee.span));
        };
        // `print(...)` in expression position (e.g. a closure body). In
        // statement position it is lowered to `Stmt::Print` upstream.
        if name == "print" {
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                out.push(self.call_arg_source(&a.node)?);
            }
            return Ok(Expr::Call { func: "print".to_string(), args: out });
        }
        // Prelude enum constructors: `Some(x)`, `None()`, `Ok(v)`, `Error(e)`.
        if let Some(type_name) = prelude_enum_of(name) {
            self.reject_named_args(args, "prelude enum constructor")?;
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                out.push(self.owning_source_from_arg(&a.node)?);
            }
            return Ok(Expr::EnumConstruct { type_name: type_name.to_string(), variant: name.clone(), args: out });
        }
        // The `std.conv.int_to_str` shim intrinsic (§2.6 shim list).
        if name == "int_to_str" && args.len() == 1 {
            return Ok(Expr::IntToStr(Box::new(self.elaborate_expr(&args[0].node.value)?)));
        }
        // Collection constructors: `Vector[T]()`, `Dict[K,V]()`, `Set[T]()`.
        if has_generic_args {
            if let Some(kind) = collection_ctor_kind(name) {
                self.reject_named_args(args, "collection constructor")?;
                let mut out = Vec::with_capacity(args.len());
                for a in args {
                    out.push(self.owning_source_from_arg(&a.node)?);
                }
                return Ok(Expr::Construct { kind, args: out });
            }
        }
        // User-enum variant constructor spelled bare (rare): `Variant(args)`.
        if let Some(type_name) = self.user_enum_of_variant(name) {
            self.reject_named_args(args, "enum variant constructor")?;
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                out.push(self.owning_source_from_arg(&a.node)?);
            }
            return Ok(Expr::EnumConstruct { type_name, variant: name.clone(), args: out });
        }
        // Struct construction: `Res("x")`, `Person("Alice", 30)`, `Point(x=1, y=2)`.
        if self.struct_names.contains(name) {
            let out = self.struct_ctor_args(name, args)?;
            return Ok(Expr::Construct { kind: ConstructKind::Struct(name.clone()), args: out });
        }
        // A first-class closure value stored in a local: `f()`, `grow()`.
        if self.local_names.contains(name) && !self.func_names.contains(name) {
            self.reject_named_args(args, "closure-value call")?;
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                out.push(self.call_arg_source(&a.node)?);
            }
            return Ok(Expr::CallValue { callee: Box::new(Expr::Local(name.clone())), args: out });
        }
        // Ordinary function call.
        if self.func_names.contains(name) {
            self.reject_named_args(args, "function call")?;
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                out.push(self.call_arg_source(&a.node)?);
            }
            return Ok(Expr::Call { func: name.clone(), args: out });
        }
        Err(ElabError::new(
            format!("unresolved callee `{name}` (unknown function/struct/enum; may need Increment B2)"),
            callee.span,
        ))
    }

    /// Elaborate struct-construction args, honouring named args (`Point(x=1,
    /// y=2)`): reorder to the struct's declaration order.
    fn struct_ctor_args(
        &mut self,
        struct_name: &str,
        args: &[Spanned<ast::CallArg>],
    ) -> ElabResult<Vec<Source>> {
        let any_named = args.iter().any(|a| a.node.name.is_some());
        if !any_named {
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                out.push(self.owning_source_from_arg(&a.node)?);
            }
            return Ok(out);
        }
        let field_order: Vec<String> = self
            .structs
            .iter()
            .find(|s| s.name == struct_name)
            .map(|s| s.fields.clone())
            .unwrap_or_default();
        let mut by_name: Vec<(String, Source)> = Vec::with_capacity(args.len());
        for a in args {
            let name = a
                .node
                .name
                .as_ref()
                .ok_or_else(|| ElabError::new("mixed positional/named struct args are unsupported", a.span))?
                .node
                .clone();
            by_name.push((name, self.owning_source_from_arg(&a.node)?));
        }
        let mut out = Vec::with_capacity(field_order.len());
        for f in &field_order {
            let pos = by_name
                .iter()
                .position(|(n, _)| n == f)
                .ok_or_else(|| ElabError::new(format!("missing field `{f}` in `{struct_name}(...)`"), args[0].span))?;
            out.push(by_name.remove(pos).1);
        }
        Ok(out)
    }

    /// The enum a user-declared variant belongs to (for bare-spelled ctors).
    fn user_enum_of_variant(&self, variant: &str) -> Option<String> {
        self.enums
            .iter()
            .find(|e| e.variants.iter().any(|(v, _)| v == variant))
            .map(|e| e.name.clone())
    }

    /// The arity of `variant` in enum `type_name`, if that enum exists.
    fn enum_variant_arity(&self, type_name: &str, variant: &str) -> Option<usize> {
        self.enums
            .iter()
            .find(|e| e.name == type_name)?
            .variants
            .iter()
            .find(|(v, _)| v == variant)
            .map(|(_, a)| *a)
    }

    fn elaborate_method(
        &mut self,
        receiver: &Spanned<ast::Expr>,
        method: &str,
        args: &[Spanned<ast::CallArg>],
        span: Span,
    ) -> ElabResult<Expr> {
        // Enum-variant construction is parsed as a method call on the type name
        // (`Token.Ident("x")` → MethodCall{ recv: Token, method: Ident }).
        if let ast::Expr::Identifier(type_name) = &receiver.node {
            if let Some(arity) = self.enum_variant_arity(type_name, method) {
                if arity != args.len() {
                    return Err(ElabError::new(
                        format!("variant `{type_name}.{method}` expects {arity} field(s), got {}", args.len()),
                        span,
                    ));
                }
                let mut out = Vec::with_capacity(args.len());
                for a in args {
                    out.push(self.owning_source_from_arg(&a.node)?);
                }
                return Ok(Expr::EnumConstruct {
                    type_name: type_name.clone(),
                    variant: method.to_string(),
                    args: out,
                });
            }
        }

        let recv = Box::new(self.elaborate_expr(receiver)?);
        // `(method, expected-arg-count)` for the fixed-arity builtins.
        let (bm, argn): (BuiltinMethod, Option<usize>) = match method {
            "push" => (BuiltinMethod::Push, Some(1)),
            "set" | "put" => (BuiltinMethod::Set, Some(2)),
            "len" => (BuiltinMethod::Len, Some(0)),
            "get" => (BuiltinMethod::Get, Some(1)),
            "unwrap" => (BuiltinMethod::Unwrap, Some(0)),
            "unwrap_or" => (BuiltinMethod::UnwrapOr, Some(1)),
            "pop" => (BuiltinMethod::Pop, Some(0)),
            "clear" => (BuiltinMethod::Clear, Some(0)),
            "fill" => (BuiltinMethod::Fill, Some(2)),
            "add" => (BuiltinMethod::Add, Some(1)),
            "trim" => (BuiltinMethod::Trim, Some(0)),
            "substring" => (BuiltinMethod::Substring, Some(2)),
            "clone" => {
                if !args.is_empty() {
                    return Err(ElabError::new("`.clone` takes no args", span));
                }
                return Ok(Expr::Clone(recv));
            }
            other => {
                return Err(ElabError::new(
                    format!("method `.{other}()` is outside the phase-0 subset (may need Increment B2)"),
                    span,
                ));
            }
        };
        if let Some(n) = argn {
            if args.len() != n {
                return Err(ElabError::new(format!("`.{method}` takes {n} arg(s)"), span));
            }
        }
        let mut out = Vec::with_capacity(args.len());
        for a in args {
            out.push(self.owning_source_from_arg(&a.node)?);
        }
        Ok(Expr::Method { recv, method: bm, args: out })
    }

    /// Elaborate a `match` in statement position (arm bodies are blocks).
    fn elaborate_match_stmt(
        &mut self,
        scrutinee: &Spanned<ast::Expr>,
        arms: &[ast::MatchItem],
        else_arm: Option<&ast::Block>,
        span: Span,
    ) -> ElabResult<Stmt> {
        let scrut = self.elaborate_expr(scrutinee)?;
        let mut out_arms = Vec::with_capacity(arms.len());
        for item in arms {
            let arm = item
                .arm()
                .ok_or_else(|| ElabError::new("`meta for` match arms are phase 2", span))?;
            if arm.guard.is_some() {
                return Err(ElabError::new("match guards are outside the phase-0 subset", arm.span));
            }
            out_arms.push(StmtArm {
                pattern: self.elaborate_pattern(&arm.pattern)?,
                body: self.arm_body_block(&arm.body)?,
            });
        }
        let else_ = match else_arm {
            Some(b) => Some(self.elaborate_block(b)?),
            None => None,
        };
        Ok(Stmt::Match { scrutinee: scrut, arms: out_arms, else_arm: else_, span })
    }

    /// A statement-match arm body: a block, or a single expression treated as
    /// a statement (so `case 0: print("x")` lowers to a `Print`).
    fn arm_body_block(&mut self, body: &Spanned<ast::Expr>) -> ElabResult<Vec<Stmt>> {
        if let ast::Expr::Block(b) = &body.node {
            return self.elaborate_block(b);
        }
        if let Some(arg) = as_print_call(body) {
            return Ok(vec![Stmt::Print { expr: self.elaborate_expr(arg)?, span: body.span }]);
        }
        Ok(vec![Stmt::Expr { expr: self.elaborate_expr(body)?, span: body.span }])
    }

    fn elaborate_pattern(&mut self, pat: &Spanned<ast::Pattern>) -> ElabResult<Pattern> {
        match &pat.node {
            ast::Pattern::Wildcard => Ok(Pattern::Wildcard),
            ast::Pattern::Binding(name) => Ok(Pattern::Binding(name.clone())),
            ast::Pattern::Literal(e) => Ok(Pattern::Literal(Box::new(self.elaborate_expr(e)?))),
            ast::Pattern::Constructor { path, fields } => {
                let variant = path
                    .last()
                    .ok_or_else(|| ElabError::new("empty constructor path", pat.span))?
                    .node
                    .clone();
                let mut fs = Vec::with_capacity(fields.len());
                for f in fields {
                    fs.push(self.elaborate_pattern(f)?);
                }
                Ok(Pattern::Variant { variant, fields: fs })
            }
            ast::Pattern::DotShorthand { variant, fields } => {
                let mut fs = Vec::with_capacity(fields.len());
                for f in fields {
                    fs.push(self.elaborate_pattern(f)?);
                }
                Ok(Pattern::Variant { variant: variant.node.clone(), fields: fs })
            }
            _ => Err(ElabError::new("pattern shape is outside the phase-0 subset", pat.span)),
        }
    }

    /// Elaborate a bare (by-value) closure into `Program.closures`, computing
    /// its capture set (free enclosing-locals referenced in the body).
    fn elaborate_closure(
        &mut self,
        is_async: bool,
        params: &[Spanned<ast::ClosureParam>],
        body: &Spanned<ast::Expr>,
        span: Span,
    ) -> ElabResult<Expr> {
        if is_async {
            return Err(ElabError::new("async closures are phase 3", span));
        }
        let mut cparams = Vec::with_capacity(params.len());
        for p in params {
            if p.node.destructure.is_some() {
                return Err(ElabError::new("closure param destructuring is outside phase 0", p.span));
            }
            cparams.push(Param {
                name: p.node.name.node.clone(),
                mode: mode_of(p.node.ownership),
                span: p.span,
            });
        }
        let cbody = self.elaborate_expr(body)?;
        // Capture set: enclosing locals referenced in the body, minus the
        // closure's own params. Deterministic order (sorted).
        let mut used = HashSet::new();
        collect_expr_locals(&cbody, &self.closures, &mut used);
        let cparam_names: HashSet<String> = cparams.iter().map(|p| p.name.clone()).collect();
        let mut captures: Vec<String> = self
            .local_names
            .iter()
            .filter(|n| used.contains(*n) && !cparam_names.contains(*n))
            .cloned()
            .collect();
        captures.sort();
        let id = self.closures.len();
        self.closures.push(ClosureDef { params: cparams, captures, body: cbody, span });
        Ok(Expr::Closure(id))
    }

    /// Elaborate an optional sub-expression (range endpoints), boxing it.
    fn opt_expr(&mut self, e: Option<&Spanned<ast::Expr>>) -> ElabResult<Option<Box<Expr>>> {
        match e {
            Some(e) => Ok(Some(Box::new(self.elaborate_expr(e)?))),
            None => Ok(None),
        }
    }
}

// ── Small helpers ──────────────────────────────────────────────────────────

/// Build an `EnumConstruct` expression.
fn enum_construct(type_name: &str, variant: &str, args: Vec<Source>) -> Expr {
    Expr::EnumConstruct { type_name: type_name.to_string(), variant: variant.to_string(), args }
}

/// The prelude enum a bare constructor name belongs to (`Some`/`None` →
/// `Option`, `Ok`/`Error` → `Result`).
fn prelude_enum_of(name: &str) -> Option<&'static str> {
    match name {
        "Some" | "None" => Some("Option"),
        "Ok" | "Error" => Some("Result"),
        _ => None,
    }
}

/// The construct kind for a generic collection constructor call.
fn collection_ctor_kind(name: &str) -> Option<ConstructKind> {
    match name {
        "Vector" => Some(ConstructKind::Vector),
        "Dict" | "HashMap" => Some(ConstructKind::Dict),
        "Set" | "HashSet" => Some(ConstructKind::Set),
        _ => None,
    }
}

/// Map a surface type to an `as`-cast target (unit-tested only).
fn cast_target(ty: &ast::Type, span: Span) -> ElabResult<CastTarget> {
    use gorget::parser::ast::PrimitiveType as P;
    let ast::Type::Primitive(p) = ty else {
        return Err(ElabError::new("`as`-cast target must be a numeric primitive", span));
    };
    Ok(match p {
        P::Int8 => CastTarget::Int { bits: 8, signed: true },
        P::Int16 => CastTarget::Int { bits: 16, signed: true },
        P::Int32 => CastTarget::Int { bits: 32, signed: true },
        P::Int | P::Int64 => CastTarget::Int { bits: 64, signed: true },
        P::Uint8 => CastTarget::Int { bits: 8, signed: false },
        P::Uint16 => CastTarget::Int { bits: 16, signed: false },
        P::Uint32 => CastTarget::Int { bits: 32, signed: false },
        P::Uint | P::Uint64 => CastTarget::Int { bits: 64, signed: false },
        P::Float32 => CastTarget::Float32,
        P::Float | P::Float64 => CastTarget::Float64,
        _ => return Err(ElabError::new("`as`-cast target must be a numeric primitive", span)),
    })
}

/// Pre-pass: collect every name bound (var decls, for-vars) in a block, for the
/// per-function local-name set. Recurses through nested blocks.
fn collect_bound_names(block: &ast::Block, out: &mut HashSet<String>) {
    for stmt in &block.stmts {
        collect_stmt_bound_names(&stmt.node, out);
    }
}

fn collect_stmt_bound_names(stmt: &ast::Stmt, out: &mut HashSet<String>) {
    match stmt {
        ast::Stmt::VarDecl { pattern, .. } => pattern_names(&pattern.node, out),
        ast::Stmt::If { then_body, elif_branches, else_body, .. } => {
            collect_bound_names(then_body, out);
            for (_, b) in elif_branches {
                collect_bound_names(b, out);
            }
            if let Some(b) = else_body {
                collect_bound_names(b, out);
            }
        }
        ast::Stmt::While { body, .. } | ast::Stmt::Loop { body } => collect_bound_names(body, out),
        ast::Stmt::For { pattern, body, .. } => {
            pattern_names(&pattern.node, out);
            collect_bound_names(body, out);
        }
        ast::Stmt::Match { arms, else_arm, .. } => {
            for item in arms {
                if let Some(arm) = item.arm() {
                    if let ast::Expr::Block(b) = &arm.body.node {
                        collect_bound_names(b, out);
                    }
                }
            }
            if let Some(b) = else_arm {
                collect_bound_names(b, out);
            }
        }
        _ => {}
    }
}

fn pattern_names(pat: &ast::Pattern, out: &mut HashSet<String>) {
    match pat {
        ast::Pattern::Binding(n) => {
            out.insert(n.clone());
        }
        ast::Pattern::Tuple(ps) => {
            for p in ps {
                pattern_names(&p.node, out);
            }
        }
        ast::Pattern::Constructor { fields, .. } | ast::Pattern::DotShorthand { fields, .. } => {
            for f in fields {
                pattern_names(&f.node, out);
            }
        }
        _ => {}
    }
}

/// Collect the local names referenced in a GGC expression (for closure capture
/// sets). Recurses through sub-expressions and sources; a nested closure
/// contributes its own (already-computed) capture names transitively.
fn collect_expr_locals(e: &Expr, closures: &[ClosureDef], out: &mut HashSet<String>) {
    match e {
        Expr::Local(n) => {
            out.insert(n.clone());
        }
        Expr::Int(_) | Expr::Bool(_) | Expr::Float(_) | Expr::Str(_) => {}
        Expr::FString(parts) => {
            for p in parts {
                if let FPart::Interp(e) = p {
                    collect_expr_locals(e, closures, out);
                }
            }
        }
        Expr::Field(o, _) | Expr::TupleField(o, _) => collect_expr_locals(o, closures, out),
        Expr::Index(o, i) => {
            collect_expr_locals(o, closures, out);
            collect_expr_locals(i, closures, out);
        }
        Expr::Slice { object, start, end, .. } => {
            collect_expr_locals(object, closures, out);
            if let Some(s) = start {
                collect_expr_locals(s, closures, out);
            }
            if let Some(en) = end {
                collect_expr_locals(en, closures, out);
            }
        }
        Expr::Binary(_, l, r) => {
            collect_expr_locals(l, closures, out);
            collect_expr_locals(r, closures, out);
        }
        Expr::Unary(_, e) | Expr::Cast { expr: e, .. } | Expr::IntToStr(e) | Expr::Clone(e) => {
            collect_expr_locals(e, closures, out)
        }
        Expr::Call { args, .. } | Expr::Construct { args, .. } | Expr::EnumConstruct { args, .. } => {
            for a in args {
                collect_source_locals(a, closures, out);
            }
        }
        Expr::CallValue { callee, args } => {
            collect_expr_locals(callee, closures, out);
            for a in args {
                collect_source_locals(a, closures, out);
            }
        }
        Expr::Method { recv, args, .. } => {
            collect_expr_locals(recv, closures, out);
            for a in args {
                collect_source_locals(a, closures, out);
            }
        }
        Expr::Closure(id) => {
            for c in &closures[*id].captures {
                out.insert(c.clone());
            }
        }
        Expr::Match { scrutinee, arms, else_arm, .. } => {
            collect_expr_locals(scrutinee, closures, out);
            for a in arms {
                collect_expr_locals(&a.body, closures, out);
            }
            if let Some(e) = else_arm {
                collect_expr_locals(e, closures, out);
            }
        }
    }
}

fn collect_source_locals(s: &Source, closures: &[ClosureDef], out: &mut HashSet<String>) {
    match s {
        Source::Copy(e)
        | Source::Move(e)
        | Source::BorrowView(e)
        | Source::WriteThrough(e)
        | Source::Value(e) => collect_expr_locals(e, closures, out),
    }
}

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
        ast::Expr::FieldAccess { object, .. } | ast::Expr::TupleFieldAccess { object, .. } => {
            ast_is_place(&object.node)
        }
        // `x[i]` is a place, but `x[a..b]` (a slice) is a fresh value.
        ast::Expr::Index { object, index } => {
            !matches!(index.node, ast::Expr::Range { .. }) && ast_is_place(&object.node)
        }
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
