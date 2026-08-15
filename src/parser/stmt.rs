use crate::lexer::token::{Keyword, Token};
use crate::span::{Span, Spanned};

use super::ast::*;
use super::Parser;
use crate::errors::ParseError;

/// Outcome of the speculative var-decl name probe in `parse_decl_or_expr_stmt`.
/// `Reserved` carries a keyword found in binding-name position (immediately
/// before `=`) so the non-speculative caller can raise a clear diagnostic —
/// `try_parse` can only signal failure via `None`, not return a `ParseError`.
enum DeclName {
    Name {
        is_mutable: bool,
        type_: Spanned<Type>,
        name: Spanned<String>,
    },
    Reserved {
        span: Span,
        kw: Keyword,
    },
    /// A type-first declaration with no `=` initializer (`int x`). Carries the
    /// name's span so the non-speculative caller can raise a clear "requires an
    /// initializer" diagnostic instead of letting `int x` fall through to
    /// expression parsing (where `x` would resolve as an undefined name).
    MissingInit {
        span: Span,
    },
    /// D10(a): a `&` decl-sigil on a local binding (`Vector[int] &r = a`) —
    /// the decl-sigil form of a local `&`-bind, rejected in v1 (one exclusive
    /// writer per place). Carries the sigil's span for the diagnostic.
    BorrowSigil {
        span: Span,
    },
}

fn make_var_decl(
    is_const: bool,
    is_mutable: bool,
    shared: SharedKind,
    type_: Spanned<Type>,
    pattern: Spanned<Pattern>,
    value: Spanned<Expr>,
    start: Span,
) -> Spanned<Stmt> {
    let end = value.span;
    Spanned::new(
        Stmt::VarDecl {
            is_const,
            is_mutable,
            shared,
            type_,
            pattern,
            value,
        },
        start.merge(end),
    )
}

impl Parser {
    /// Parse a statement.
    pub fn parse_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        match self.peek().clone() {
            // Compile-time meta statements (delayed evaluation in generic bodies)
            Token::Keyword(Keyword::Meta) => self.parse_meta_stmt(),

            // Explicit control flow keywords
            Token::Keyword(Keyword::Return) => self.parse_return_stmt(),
            Token::Keyword(Keyword::Throw) => self.parse_throw_stmt(),
            // "on error:" — both `on` and `error` are contextual identifiers
            // (`on` is commonly used in user code; lowercase `error` is not the `Error` keyword)
            Token::Identifier(ref s) if s == "on"
                && matches!(self.peek_ahead(1), Token::Identifier(e) if e == "error") =>
            {
                self.parse_on_error_stmt()
            }
            Token::Keyword(Keyword::Assert) => self.parse_assert_stmt(),
            Token::Keyword(Keyword::Snapshot) => self.parse_snapshot_stmt(),
            Token::Keyword(Keyword::Break) => self.parse_break_stmt(),
            Token::Keyword(Keyword::Continue) => self.parse_simple_stmt(Stmt::Continue),
            Token::Keyword(Keyword::Pass) => self.parse_simple_stmt(Stmt::Pass),

            // Control flow statements
            Token::Keyword(Keyword::If) => self.parse_if_stmt(),
            Token::Keyword(Keyword::For) => self.parse_for_stmt(),
            Token::Keyword(Keyword::While) => self.parse_while_stmt(),
            Token::Keyword(Keyword::Loop) => self.parse_loop_stmt(),
            Token::Keyword(Keyword::Match) => self.parse_match_stmt(),
            Token::Keyword(Keyword::Select) => self.parse_select_stmt(),
            Token::Keyword(Keyword::With) => self.parse_with_stmt(),
            Token::Keyword(Keyword::Unsafe) => self.parse_unsafe_stmt(),

            // shared — shared variable declaration with automatic synchronization
            Token::Keyword(Keyword::Shared) => self.parse_shared_var_decl(),

            // const — could be variable declaration
            Token::Keyword(Keyword::Const) => self.parse_const_var_decl(),

            // auto — type-inferred variable declaration
            Token::Keyword(Keyword::Auto) => self.parse_auto_var_decl(),

            // mutable — prefix for mutable variable declaration
            Token::Keyword(Keyword::Mutable) => self.parse_decl_or_expr_stmt(),

            // Named scope: `identifier: \n    body` — mid-function drop zone.
            _ if self.check_identifier_colon_block() => self.parse_named_scope(),

            // Type keyword starting a declaration or expression
            _ if self.is_type_start() => self.parse_decl_or_expr_stmt(),

            // Expression statement (or assignment)
            _ => self.parse_expr_or_assign_stmt(),
        }
    }

    /// Parse a keyword-only statement (e.g., `continue`, `pass`): advance, consume newline.
    fn parse_simple_stmt(&mut self, stmt: Stmt) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.advance();
        let end = self.previous_span();
        self.consume_newline();
        Ok(Spanned::new(stmt, start.merge(end)))
    }

    pub(super) fn parse_return_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Return)?;

        let value = if !self.check(&Token::Newline) && !self.check(&Token::Dedent) {
            let first = self.parse_expr()?;
            if self.check(&Token::Comma) {
                let elem_start = first.span;
                let mut elements = vec![first];
                while self.match_token(&Token::Comma) {
                    elements.push(self.parse_expr()?);
                }
                let end = elements.last().unwrap().span;
                Some(Spanned::new(Expr::TupleLiteral(elements), elem_start.merge(end)))
            } else {
                Some(first)
            }
        } else {
            None
        };

        let end = self.previous_span();
        self.consume_newline();
        Ok(Spanned::new(Stmt::Return(value), start.merge(end)))
    }

    pub(super) fn parse_throw_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Throw)?;
        let value = self.parse_expr()?;
        let end = value.span;
        self.consume_newline();
        Ok(Spanned::new(Stmt::Throw(value), start.merge(end)))
    }

    fn parse_on_error_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.advance(); // consume "on" (contextual identifier)
        self.advance(); // consume "error" (contextual identifier)
        if self.check(&Token::Colon) {
            // Block form: on error:\n    stmts
            let body = self.parse_block(start.start)?;
            let end = self.previous_span();
            Ok(Spanned::new(Stmt::OnError { body }, start.merge(end)))
        } else {
            // Inline form: on error stmt
            let stmt = self.parse_stmt()?;
            let end = stmt.span;
            // `on error <stmt>` — note the inline form takes NO colon.
            let body = Block {
                stmts: vec![stmt],
                span: start.merge(end),
                layout: SuiteLayout::Inline,
                header_start: start.start,
            };
            Ok(Spanned::new(Stmt::OnError { body }, start.merge(end)))
        }
    }

    fn parse_break_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Break)?;

        // `break <expr>` (loop-as-expression) was removed from the v1 surface
        // (D19). Recognize the shape and reject with a teaching error rather
        // than falling through to a generic "expected newline".
        if !self.check(&Token::Newline) && !self.check(&Token::Dedent) && self.is_expr_start() {
            return Err(ParseError {
                kind: crate::errors::ParseErrorKind::BreakWithValue,
                span: self.peek_span(),
            });
        }

        let end = self.previous_span();
        self.consume_newline();
        Ok(Spanned::new(Stmt::Break, start.merge(end)))
    }

    fn parse_assert_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Assert)?;

        // `assert return <expr>` — postcondition
        if self.check(&Token::Keyword(Keyword::Return)) {
            let ret_span = self.peek_span();
            self.advance(); // consume `return`
            // Build a synthetic identifier for the return value
            let return_ident = Spanned::new(
                Expr::Identifier("__return__".to_string()),
                ret_span,
            );
            // Parse the rest as an infix continuation (e.g., `>= 0`)
            let condition = self.parse_expr_with_lhs(return_ident)?;

            let message = if self.match_token(&Token::Comma) {
                Some(self.parse_expr()?)
            } else {
                None
            };

            let end = self.previous_span();
            self.consume_newline();
            return Ok(Spanned::new(Stmt::AssertReturn { condition, message }, start.merge(end)));
        }

        let condition = self.parse_expr()?;

        let message = if self.match_token(&Token::Comma) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        let end = self.previous_span();
        self.consume_newline();
        Ok(Spanned::new(Stmt::Assert { condition, message }, start.merge(end)))
    }

    fn parse_snapshot_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Snapshot)?;

        let name = self.expect_plain_string()?;
        let value = self.parse_expr()?;
        let end = self.previous_span();
        self.consume_newline();
        Ok(Spanned::new(Stmt::Snapshot { name, value }, start.merge(end)))
    }

    fn parse_if_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::If)?;
        let condition = self.parse_expr()?;
        let then_body = self.parse_block_or_inline_stmt(start.start)?;

        let mut elif_branches = Vec::new();
        let mut else_body = None;

        while self.match_elif() {
            // ⚠ The clause keyword's span, captured BEFORE the condition is
            // parsed: after `parse_expr` a `previous_span()` would name the
            // CONDITION's last token, which a wrapped condition puts on a
            // later line.
            let clause = self.previous_span();
            let elif_cond = self.parse_expr()?;
            let elif_body = self.parse_block_or_inline_stmt(clause.start)?;
            elif_branches.push((elif_cond, elif_body));
        }

        if self.match_keyword(Keyword::Else) {
            let clause = self.previous_span();
            else_body = Some(self.parse_block_or_inline_stmt(clause.start)?);
        }

        let end = self.previous_span();
        Ok(Spanned::new(
            Stmt::If {
                condition,
                then_body,
                elif_branches,
                else_body,
            },
            start.merge(end),
        ))
    }

    fn parse_for_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::For)?;

        let pattern = self.parse_bare_pattern()?;
        self.expect_keyword(Keyword::In)?;

        // Check for ownership modifier on iterable
        let ownership = self.parse_ownership_modifier();

        let iterable = self.parse_expr()?;
        let body = self.parse_block(start.start)?;

        let else_body = if self.match_keyword(Keyword::Else) {
            let clause = self.previous_span();
            Some(self.parse_block(clause.start)?)
        } else {
            None
        };

        let end = self.previous_span();
        Ok(Spanned::new(
            Stmt::For {
                pattern,
                ownership,
                iterable,
                body,
                else_body,
            },
            start.merge(end),
        ))
    }

    fn parse_while_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::While)?;
        let condition = self.parse_expr()?;
        let body = self.parse_block(start.start)?;

        let else_body = if self.match_keyword(Keyword::Else) {
            let clause = self.previous_span();
            Some(self.parse_block(clause.start)?)
        } else {
            None
        };

        let end = self.previous_span();
        Ok(Spanned::new(
            Stmt::While {
                condition,
                body,
                else_body,
            },
            start.merge(end),
        ))
    }

    fn parse_loop_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Loop)?;
        let body = self.parse_block(start.start)?;
        let end = self.previous_span();
        Ok(Spanned::new(Stmt::Loop { body }, start.merge(end)))
    }

    fn parse_match_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Match)?;
        let scrutinee = self.parse_expr()?;
        self.expect_block_start()?;

        let mut arms = Vec::new();
        let mut else_arm = None;

        while !self.check(&Token::Dedent) && !self.at_end() && !self.at_error_limit() {
            if self.check(&Token::Newline) {
                self.advance();
                continue;
            }

            if self.match_keyword(Keyword::Else) {
                let clause = self.previous_span();
                match self.parse_block(clause.start) {
                    Ok(block) => else_arm = Some(block),
                    Err(e) => {
                        self.errors.push(e);
                        self.synchronize_with_progress();
                    }
                }
                continue;
            }

            // meta for <vars> in <range>: <single case arm>
            if self.check(&Token::Keyword(Keyword::Meta)) {
                match self.parse_meta_for_match_item() {
                    Ok(item) => arms.push(item),
                    Err(e) => {
                        self.errors.push(e);
                        self.synchronize_with_progress();
                    }
                }
                continue;
            }

            let saved_pos = self.pos;
            let arm_start = self.peek_span();
            match self.parse_match_arm_inner(arm_start) {
                Ok(arm) => arms.push(arm),
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize_with_progress();
                    if self.pos == saved_pos {
                        self.advance();
                    }
                }
            }
        }

        self.expect(&Token::Dedent)?;
        let end = self.previous_span();

        Ok(Spanned::new(
            Stmt::Match {
                scrutinee,
                arms,
                else_arm,
            },
            start.merge(end),
        ))
    }

    fn parse_match_arm_inner(&mut self, arm_start: Span) -> Result<MatchItem, ParseError> {
        self.expect_keyword(Keyword::Case)?;
        let pattern = self.parse_pattern()?;

        let guard = if self.match_keyword(Keyword::If) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.expect(&Token::Colon)?;
        let body = self.parse_arm_body(arm_start)?;

        let arm_end = body.span;
        Ok(MatchItem::Arm(MatchArm {
            pattern,
            guard,
            body,
            span: arm_start.merge(arm_end),
        }))
    }

    fn parse_select_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Select)?;
        self.expect_block_start()?;

        let mut arms = Vec::new();
        let mut else_arm = None;

        while !self.check(&Token::Dedent) && !self.at_end() {
            if self.check(&Token::Newline) {
                self.advance();
                continue;
            }

            if self.match_keyword(Keyword::Else) {
                let clause = self.previous_span();
                else_arm = Some(self.parse_block(clause.start)?);
                continue;
            }

            let arm_start = self.peek_span();
            self.expect_keyword(Keyword::Case)?;

            let op = self.parse_select_op()?;

            self.expect(&Token::Colon)?;
            // `arm_start` is the `case` keyword — the arm's own first line.
            let body = self.parse_block_body(arm_start, arm_start.start)?;

            let arm_end = body.span;
            arms.push(SelectArm {
                op,
                body,
                span: arm_start.merge(arm_end),
            });
        }

        self.expect(&Token::Dedent)?;
        let end = self.previous_span();

        Ok(Spanned::new(
            Stmt::Select { arms, else_arm },
            start.merge(end),
        ))
    }

    fn parse_select_op(&mut self) -> Result<SelectOp, ParseError> {
        // Try recv: Type name = expr.recv()
        if let Some(recv_op) = self.try_parse(|p| {
            let type_ = p.parse_type().ok()?;
            let name_span = p.peek_span();
            let name = p.expect_identifier().ok()?;
            let name = Spanned::new(name.node, name_span);
            p.match_token(&Token::Eq).then_some(())?;
            let channel = p.parse_expr().ok()?;
            // Validate it's a .recv() call
            match &channel.node {
                Expr::MethodCall { method, args, .. } if method.node == "recv" && args.is_empty() => {}
                _ => return None,
            }
            // Extract receiver from the MethodCall
            if let Expr::MethodCall { receiver, .. } = channel.node {
                Some(SelectOp::Recv { type_, name, channel: *receiver })
            } else {
                None
            }
        }) {
            return Ok(recv_op);
        }

        // Otherwise: expr.send(value)
        let expr = self.parse_expr()?;
        match expr.node {
            Expr::MethodCall { receiver, method, args, .. } if method.node == "send" && args.len() == 1 => {
                let value = args.into_iter().next().expect("len was 1").node.value;
                Ok(SelectOp::Send { channel: *receiver, value })
            }
            _ => Err(self.error_at(expr.span, "expected channel.recv() or channel.send()")),
        }
    }

    fn parse_with_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::With)?;

        let mut bindings = Vec::new();
        loop {
            let bind_start = self.peek_span();
            let full_expr = self.parse_expr()?;
            let bind_end = self.previous_span();
            let binding = self.decompose_with_binding(full_expr, bind_start.merge(bind_end))?;
            bindings.push(binding);

            if !self.match_token(&Token::Comma) {
                break;
            }
        }

        let body = self.parse_block(start.start)?;
        let end = self.previous_span();

        Ok(Spanned::new(
            Stmt::With { bindings, body },
            start.merge(end),
        ))
    }

    /// Decompose a with-binding expression into a `WithBinding`.
    /// Supports both `expr as name` and bare `name` forms.
    /// The bare form (`with x:`) uses the identifier as both expression and name.
    fn decompose_with_binding(
        &self,
        full_expr: Spanned<Expr>,
        span: Span,
    ) -> Result<WithBinding, ParseError> {
        // `expr as name` form
        if let Expr::As { expr, type_ } = &full_expr.node {
            if let Type::Named { name, generic_args } = &type_.node {
                if generic_args.is_empty() {
                    return Ok(WithBinding {
                        expr: *expr.clone(),
                        name: name.clone(),
                        explicit_as: true,
                        span,
                    });
                }
            }
            return Err(self.error_at(span, "expected 'as <name>' in with-binding"));
        }
        // Bare `name` form — use identifier as both expr and name.
        // The duplicated span is incidental, NOT the record of which form was
        // written: `explicit_as` carries that (Layering rules 2/4).
        if let Expr::Identifier(name) = &full_expr.node {
            return Ok(WithBinding {
                name: Spanned::new(name.clone(), full_expr.span),
                expr: full_expr,
                explicit_as: false,
                span,
            });
        }
        Err(self.error_at(span, "expected '<expr> as <name>' or bare identifier in with-binding"))
    }

    fn parse_unsafe_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Unsafe)?;
        let body = self.parse_block(start.start)?;
        let end = self.previous_span();
        Ok(Spanned::new(Stmt::Unsafe { body }, start.merge(end)))
    }

    fn parse_const_var_decl(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Const)?;

        let type_ = self.parse_type()?;
        let pattern = self.parse_binding_pattern()?;

        self.expect(&Token::Eq)?;
        let value = self.parse_expr()?;
        self.consume_newline();
        Ok(make_var_decl(true, false, SharedKind::None, type_, pattern, value, start))
    }

    /// Parse a `shared` variable declaration.
    /// Syntax: `shared [type] name = expr` or `shared(rwlock|atomic) [type] name = expr`
    fn parse_shared_var_decl(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Shared)?;

        // Parse optional override: shared(rwlock) or shared(atomic)
        let shared_kind = if self.match_token(&Token::LParen) {
            let override_name = self.expect_identifier()?;
            let kind = match override_name.node.as_str() {
                "rwlock" => SharedKind::RwLock,
                "atomic" => SharedKind::Atomic,
                other => {
                    return Err(ParseError {
                        kind: crate::errors::ParseErrorKind::UnexpectedToken {
                            expected: "`rwlock` or `atomic`".to_string(),
                            got: format!("`{other}`"),
                        },
                        span: override_name.span,
                    });
                }
            };
            self.expect(&Token::RParen)?;
            kind
        } else {
            SharedKind::Auto
        };

        let type_ = self.parse_type()?;
        let pattern = self.parse_binding_pattern()?;

        self.expect(&Token::Eq)?;
        let value = self.parse_expr()?;
        self.consume_newline();
        Ok(make_var_decl(false, false, shared_kind, type_, pattern, value, start))
    }

    fn parse_auto_var_decl(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Auto)?;

        let type_ = Spanned::new(Type::Inferred, start);
        let first_pat = self.parse_binding_pattern()?;
        let pattern = if self.check(&Token::Comma) {
            let pat_start = first_pat.span;
            let mut pats = vec![first_pat];
            while self.match_token(&Token::Comma) {
                pats.push(self.parse_binding_pattern()?);
            }
            let end = pats.last().unwrap().span;
            Spanned::new(Pattern::Tuple(pats), pat_start.merge(end))
        } else {
            first_pat
        };
        self.expect(&Token::Eq)?;
        let value = self.parse_expr()?;
        self.consume_newline();
        Ok(make_var_decl(false, false, SharedKind::None, type_, pattern, value, start))
    }

    /// Try to parse a declaration (type name = expr) or fall back to expression statement.
    /// Handles: `type name = expr`, `mutable type name = expr`, `mutable auto name = expr`,
    /// and ownership modifiers (`type &name = expr`, `type mutable name = expr`, etc.).
    fn parse_decl_or_expr_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();

        // Defensive clear: no promotable-error stash should be live at the
        // entry to a fresh var-decl-or-expr attempt. See
        // `pending_speculative_error` on `Parser` for the promotion protocol.
        let _ = self.take_promotable_error();

        // Speculatively try: [mutable] type [ownership] name =
        //
        // The binding name must be a plain identifier. A reserved keyword in
        // this position (after a successfully-parsed type + optional ownership
        // sigil) followed immediately by `=` is never a valid program — the
        // only legal continuations are a binding name or an expression-tail
        // operator (`.`, `(`, `[`, `as`, `and`, …). So `<type> [sigil] <kw> =`
        // is unambiguously a misuse of a keyword as a variable name; we carry a
        // typed `Reserved` signal out of the speculative closure (its position
        // stays advanced on `Some`) and raise a clear error in the caller.
        //
        // This deliberately reverts commit 089b8e48, which special-cased the
        // `it` keyword as an acceptable binding name. That acceptance bound a
        // local named `it`, but every *read* of `it` parses to `Expr::It`
        // (the implicit-closure parameter), so the binding was unreadable —
        // `int it = 42; print(it)` printed garbage with only an unused-variable
        // warning. Rejecting the declaration outright makes such programs a hard
        // parse error, which is correct. Implicit-`it` closures are unaffected:
        // they never go through this var-decl path.
        match self.try_parse(|p| {
            let has_mutable_prefix = if p.check_keyword(Keyword::Mutable) {
                p.advance();
                true
            } else {
                false
            };

            let type_ = if has_mutable_prefix && p.check_keyword(Keyword::Auto) {
                let auto_start = p.peek_span();
                p.advance();
                Spanned::new(Type::Inferred, auto_start)
            } else {
                // D35 (Advisory A1, 2026-07-28): `parse_type` can fail with
                // `FunctionTypeParamSigilBeforeType` on `Callable[void(&int)] cb`
                // in local-decl position — a shape that's unambiguously a
                // function type (`Callable[...]` opens the generic), so falling
                // back to expression parsing after backtrack surfaces the
                // fallback's generic `expected expression, found 'void'` and
                // BURIES the D35 teaching diagnostic that names the
                // replacement. Stash D35 so the caller can PROMOTE it in the
                // None arm below; other parse-type failures still drop
                // silently (the fallback expression parse may succeed).
                match p.parse_type() {
                    Ok(t) => t,
                    Err(err) => {
                        if matches!(
                            err.kind,
                            crate::errors::ParseErrorKind::FunctionTypeParamSigilBeforeType { .. }
                        ) {
                            p.stash_promotable_error(err);
                        }
                        return None;
                    }
                }
            };

            // D10(a) (decisions.md, ratified 2026-07-06): a `&` sigil between
            // the type and the name (`Vector[int] &r = a`) is the decl-sigil
            // form of a local `&`-bind — rejected. Historically the sigil was
            // silently DISCARDED here (only `mutable` set `is_mutable`; `&`,
            // `!`, `move` were consumed and dropped, so `T &r = a` bound a
            // plain value copy while reading as a reference decl). Track the
            // `&` span so the caller can raise the teaching diagnostic; the
            // no-`=` shapes keep their existing diagnosis.
            let mut amp_span: Option<Span> = None;
            let is_mutable = if !has_mutable_prefix
                && (p.check(&Token::Ampersand)
                    || p.check(&Token::Bang)
                    || p.check_keyword(Keyword::Mutable)
                    || p.check_keyword(Keyword::Move))
            {
                let ownership_tok = p.advance();
                if matches!(ownership_tok.node, Token::Ampersand) {
                    amp_span = Some(ownership_tok.span);
                }
                matches!(ownership_tok.node, Token::Keyword(Keyword::Mutable))
            } else {
                has_mutable_prefix
            };

            match p.peek() {
                Token::Identifier(_) => {
                    let name = p.expect_identifier().ok()?;
                    if p.match_token(&Token::Eq) {
                        if let Some(span) = amp_span {
                            Some(DeclName::BorrowSigil { span })
                        } else {
                            Some(DeclName::Name { is_mutable, type_, name })
                        }
                    } else {
                        // `<type> <name>` with no `=`. There is no expression
                        // statement of this shape (Gorget has no juxtaposition),
                        // so this is unambiguously a declaration missing its
                        // initializer. Carry the name span out so the caller can
                        // raise a clear diagnostic rather than falling through to
                        // expression parsing (where `name` reads as undefined).
                        Some(DeclName::MissingInit { span: name.span })
                    }
                }
                // A keyword immediately followed by `=` is a keyword used as a
                // variable name. The `peek_ahead(1) == Eq` guard is load-bearing:
                // a type-path followed by an infix keyword (`x as float`, `a and
                // b`, `v is Some(p)`, `k in d`) is a valid expression statement
                // and must fall through to `parse_expr_or_assign_stmt` via `None`.
                Token::Keyword(kw) if *p.peek_ahead(1) == Token::Eq => {
                    let kw = *kw;
                    let span = p.peek_span();
                    Some(DeclName::Reserved { span, kw })
                }
                // Anything else (operator, `(`, `.`, `[`, a keyword NOT followed
                // by `=`, …): not a var-decl. Fall through unchanged.
                _ => None,
            }
        }) {
            Some(DeclName::Name { is_mutable, type_, name }) => {
                // Clear any leftover stash: this path succeeded via the
                // var-decl branch, so a stashed D35 from a sibling probe
                // (unreachable in practice — same closure, single call —
                // but keep the invariant tight) must not leak forward.
                let _ = self.take_promotable_error();
                let value = self.parse_expr()?;
                self.consume_newline();
                let pattern = Spanned::new(Pattern::Binding(name.node), name.span);
                Ok(make_var_decl(false, is_mutable, SharedKind::None, type_, pattern, value, start))
            }
            Some(DeclName::Reserved { span, kw }) => Err(self.error_at(
                span,
                &format!(
                    "`{}` is a reserved keyword and cannot be used as a variable name",
                    kw.as_name()
                ),
            )),
            Some(DeclName::BorrowSigil { span }) => Err(ParseError {
                kind: crate::errors::ParseErrorKind::LocalBorrowBindSigil,
                span,
            }),
            Some(DeclName::MissingInit { span }) => Err(self.error_missing_init(span)),
            None => {
                // D35 (Advisory A1, 2026-07-28): the speculative type parse
                // may have stashed `FunctionTypeParamSigilBeforeType`. We
                // cannot promote it unconditionally — a shape like
                // `cb(&y)` in stmt position triggers `parse_type` on `cb`
                // (a `Named` type-start), which sees `cb(...)`, treats it
                // as a function-type context, then hits `&y` and emits
                // D35. That shape is a legitimate function-CALL expression
                // and the fallback expression parse succeeds. So the rule
                // is: PROMOTE D35 only if the fallback ALSO fails —
                // meaning the shape really was a function-type-decl and
                // the D35 teaching diagnostic is the right story to tell.
                let stashed = self.take_promotable_error();
                match self.parse_expr_or_assign_stmt() {
                    Ok(stmt) => Ok(stmt),
                    Err(fallback_err) => Err(stashed.unwrap_or(fallback_err)),
                }
            }
        }
    }

    /// Parse an expression statement, assignment, or compound assignment.
    fn parse_expr_or_assign_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        let expr = self.parse_expr()?;

        // Check for assignment
        if self.match_token(&Token::Eq) {
            let value = self.parse_expr()?;
            let end = value.span;
            self.consume_newline();
            return Ok(Spanned::new(
                Stmt::Assign {
                    target: expr,
                    value,
                },
                start.merge(end),
            ));
        }

        // D26: compound-fallible-assign forms (`+!=` etc) are v1-EXCLUDED per
        // `decisions.md:945`. Reject at parse-time with a precise span before
        // trying to parse as compound-assign. The lexer already produces a
        // distinct token per glyph so the reject fires at the operator's exact
        // span (not the surrounding expression).
        if matches!(
            self.peek(),
            Token::PlusBangEq | Token::MinusBangEq | Token::StarBangEq
            | Token::SlashBangEq | Token::PercentBangEq
            | Token::LtLtBangEq | Token::GtGtBangEq
        ) {
            let bad_span = self.peek_span();
            let glyph = match self.peek() {
                Token::PlusBangEq => "+!=",
                Token::MinusBangEq => "-!=",
                Token::StarBangEq => "*!=",
                Token::SlashBangEq => "/!=",
                Token::PercentBangEq => "%!=",
                Token::LtLtBangEq => "<<!=",
                Token::GtGtBangEq => ">>!=",
                _ => unreachable!(),
            };
            return Err(ParseError {
                kind: crate::errors::ParseErrorKind::CompoundFallibleAssignExcluded {
                    op: glyph.to_string(),
                },
                span: bad_span,
            });
        }

        // Check for compound assignment
        let compound_op = match self.peek() {
            Token::PlusEq => Some(BinaryOp::Add),
            Token::MinusEq => Some(BinaryOp::Sub),
            Token::StarEq => Some(BinaryOp::Mul),
            Token::StarStarEq => Some(BinaryOp::Pow),
            Token::SlashEq => Some(BinaryOp::Div),
            Token::PercentEq => Some(BinaryOp::Rem),
            Token::PlusPercentEq => Some(BinaryOp::AddWrap),
            Token::MinusPercentEq => Some(BinaryOp::SubWrap),
            Token::StarPercentEq => Some(BinaryOp::MulWrap),
            Token::AmpersandEq => Some(BinaryOp::BitAnd),
            Token::PipeEq => Some(BinaryOp::BitOr),
            Token::CaretEq => Some(BinaryOp::BitXor),
            Token::LtLtEq => Some(BinaryOp::Shl),
            Token::GtGtEq => Some(BinaryOp::Shr),
            _ => None,
        };

        if let Some(op) = compound_op {
            self.advance();
            let value = self.parse_expr()?;
            let end = value.span;
            self.consume_newline();
            return Ok(Spanned::new(
                Stmt::CompoundAssign {
                    target: expr,
                    op,
                    value,
                },
                start.merge(end),
            ));
        }

        // Expression statement
        let end = expr.span;
        self.consume_newline();
        Ok(Spanned::new(Stmt::Expr(expr), start.merge(end)))
    }

    /// Parse a `meta if`, `meta for`, or `meta match` statement (delayed compile-time evaluation).
    fn parse_meta_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Meta)?;

        match self.peek().clone() {
            Token::Keyword(Keyword::If) => self.parse_meta_if_stmt(start),
            Token::Keyword(Keyword::For) => self.parse_meta_for_stmt(start),
            Token::Keyword(Keyword::Match) => self.parse_meta_match_stmt(start),
            Token::Keyword(Keyword::While) => self.parse_meta_while_stmt(start),
            Token::Keyword(Keyword::Const) => self.parse_meta_const_stmt(start),
            Token::Identifier(ref s) if s == "log" => self.parse_meta_log_stmt(start),
            _ => Err(ParseError {
                kind: crate::errors::ParseErrorKind::UnexpectedToken {
                    expected: "`if`, `for`, `match`, `while`, `const`, or `log` after `meta` in function body".to_string(),
                    got: format!("{:?}", self.peek()),
                },
                span: self.peek_span(),
            }),
        }
    }

    fn parse_meta_log_stmt(&mut self, start: Span) -> Result<Spanned<Stmt>, ParseError> {
        self.advance(); // consume `log` identifier
        let mut args = vec![self.parse_expr()?];
        while self.match_token(&Token::Comma) {
            args.push(self.parse_expr()?);
        }
        let end = self.previous_span();
        self.consume_newline();
        let span = start.merge(end);
        Ok(Spanned::new(Stmt::MetaLog { args, span }, span))
    }

    fn parse_meta_if_stmt(&mut self, start: Span) -> Result<Spanned<Stmt>, ParseError> {
        self.expect_keyword(Keyword::If)?;
        let condition = self.parse_expr()?;
        let then_body = self.parse_block(start.start)?;

        let mut elif_branches = Vec::new();
        let mut else_body = None;

        while self.match_elif() {
            // ⚠ Captured BEFORE the condition — see `parse_if_stmt`.
            let clause = self.previous_span();
            let elif_cond = self.parse_expr()?;
            let elif_body = self.parse_block(clause.start)?;
            elif_branches.push((elif_cond, elif_body));
        }

        if self.match_keyword(Keyword::Else) {
            let clause = self.previous_span();
            else_body = Some(self.parse_block(clause.start)?);
        }

        let end = self.previous_span();
        let span = start.merge(end);
        Ok(Spanned::new(
            Stmt::MetaIf {
                condition,
                then_body,
                elif_branches,
                else_body,
                span,
            },
            span,
        ))
    }

    /// Parse `var, var2 in range` after `meta for` — shared between statement and match-item forms.
    fn parse_meta_for_vars(&mut self) -> Result<(Vec<Spanned<String>>, Spanned<Expr>), ParseError> {
        self.expect_keyword(Keyword::For)?;
        let var_span = self.peek_span();
        let first = self.expect_identifier()?;
        let mut vars = vec![Spanned::new(first.node, var_span)];
        while self.match_token(&Token::Comma) {
            vars.push(self.expect_identifier()?);
        }
        self.expect_keyword(Keyword::In)?;
        let range = self.parse_expr()?;
        Ok((vars, range))
    }

    fn parse_meta_for_stmt(&mut self, start: Span) -> Result<Spanned<Stmt>, ParseError> {
        let (vars, range) = self.parse_meta_for_vars()?;
        let body = self.parse_block(start.start)?;
        let end = self.previous_span();
        let span = start.merge(end);
        Ok(Spanned::new(
            Stmt::MetaFor {
                vars,
                range,
                body,
                span,
            },
            span,
        ))
    }

    /// Parse `meta for <vars> in <range>: <single case arm>` inside a match arm list.
    /// Produces a `MatchItem::MetaFor` that is expanded at monomorphization time.
    fn parse_meta_for_match_item(&mut self) -> Result<MatchItem, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Meta)?;
        let (vars, range) = self.parse_meta_for_vars()?;

        // Parse `:` newline indent — then exactly one case arm as the template
        self.expect_block_start()?;

        // Skip blank lines
        while self.check(&Token::Newline) { self.advance(); }

        let arm_start = self.peek_span();
        self.expect_keyword(Keyword::Case)?;
        let pattern = self.parse_pattern()?;

        let guard = if self.match_keyword(Keyword::If) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.expect(&Token::Colon)?;
        let body = self.parse_arm_body(arm_start)?;

        let arm_end = body.span;
        let arm_template = MatchArm { pattern, guard, body, span: arm_start.merge(arm_end) };

        // Skip blank lines before dedent
        while self.check(&Token::Newline) { self.advance(); }
        self.expect(&Token::Dedent)?;
        let end = self.previous_span();

        Ok(MatchItem::MetaFor { vars, range, arm_template, span: start.merge(end) })
    }

    fn parse_meta_match_stmt(&mut self, start: Span) -> Result<Spanned<Stmt>, ParseError> {
        self.expect_keyword(Keyword::Match)?;
        let scrutinee = self.parse_expr()?;
        self.expect_block_start()?;

        let mut arms: Vec<(Spanned<Expr>, Block)> = Vec::new();
        let mut else_arm: Option<Block> = None;

        while !self.check(&Token::Dedent) && !self.at_end() {
            if self.check(&Token::Newline) {
                self.advance();
                continue;
            }

            if self.match_keyword(Keyword::Else) {
                // The `else` keyword was just consumed — its span is the
                // clause's first line.
                let kw = self.previous_span();
                let clause_start = self.peek_span();
                self.expect(&Token::Colon)?;
                let body = self.parse_meta_match_arm_body(clause_start, kw.start)?;
                else_arm = Some(body);
                continue;
            }

            // ⚠ Captured BEFORE the case EXPRESSION is parsed: a wrapped case
            // expression would leave `previous_span()` on a continuation line.
            let case_kw = self.peek_span();
            self.expect_keyword(Keyword::Case)?;
            let case_expr = self.parse_expr()?;
            let clause_start = self.peek_span();
            self.expect(&Token::Colon)?;
            let body = self.parse_meta_match_arm_body(clause_start, case_kw.start)?;
            arms.push((case_expr, body));
        }

        self.expect(&Token::Dedent)?;
        let end = self.previous_span();
        let span = start.merge(end);
        Ok(Spanned::new(
            Stmt::MetaMatch {
                scrutinee,
                arms,
                else_arm,
                span,
            },
            span,
        ))
    }

    /// Parse a `meta match` arm body: either a single inline statement or a
    /// newline-indented block.
    ///
    /// `start` is the arm's own COLON, captured by the caller before consuming
    /// it — the same anchor `parse_block` records for every other suite. It
    /// puts the resulting `Block.span.start` on the arm's own source line,
    /// which is what a walk-back from the block (for the blank line above a
    /// clause header, or for the comments that lead it) needs. Anchoring at
    /// the NEWLINE token instead put it on the line BELOW, one line past
    /// everything such a walk is looking for.
    ///
    /// `header_start` is the arm's own FIRST line — the `case` keyword (which
    /// the caller must capture BEFORE parsing the case EXPRESSION, since a
    /// wrapped case expression puts `previous_span()` on a later line) or the
    /// `else`. `start`, the colon, cannot stand in for it.
    fn parse_meta_match_arm_body(
        &mut self,
        start: Span,
        header_start: usize,
    ) -> Result<Block, ParseError> {
        if self.check(&Token::Newline) {
            self.parse_block_body(start, header_start)
        } else {
            // Single inline statement
            let stmt_start = self.peek_span();
            let stmt = self.parse_stmt()?;
            let span = stmt.span;
            Ok(Block {
                stmts: vec![stmt],
                span: stmt_start.merge(span),
                layout: SuiteLayout::Inline,
                header_start,
            })
        }
    }

    fn parse_meta_while_stmt(&mut self, start: Span) -> Result<Spanned<Stmt>, ParseError> {
        self.expect_keyword(Keyword::While)?;
        let condition = self.parse_expr()?;
        // The COLON, captured before it is consumed. This site used to take
        // `peek_span()` AFTER the colon, which put `Block.span.start` on the
        // BODY's own line — one line below everything the blank/lookback logic
        // that reads it walks back for. `header_start` is separately the `meta`
        // keyword, since a header-INDENT question wants the header's FIRST line
        // and the colon's line is its LAST.
        let block_start = self.peek_span();
        self.expect(&Token::Colon)?;
        let body = self.parse_block_body(block_start, start.start)?;
        let end = self.previous_span();
        let span = start.merge(end);
        Ok(Spanned::new(
            Stmt::MetaWhile { condition, body, span },
            span,
        ))
    }

    fn parse_meta_const_stmt(&mut self, start: Span) -> Result<Spanned<Stmt>, ParseError> {
        self.expect_keyword(Keyword::Const)?;
        let name = self.expect_identifier()?;
        self.expect(&Token::Eq)?;
        let value = self.parse_expr()?;
        let end = self.previous_span();
        self.consume_newline();
        let span = start.merge(end);
        Ok(Spanned::new(Stmt::MetaConst { name, value, span }, span))
    }

    /// Parse a named scope block: `identifier:\n    stmts`.
    fn parse_named_scope(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        let name = self.expect_identifier()?;
        let body = self.parse_block(start.start)?;
        let end = self.previous_span();
        Ok(Spanned::new(
            Stmt::NamedScope { name, body },
            start.merge(end),
        ))
    }

    /// Parse a simple binding pattern for variable declarations.
    /// Supports: name, (a, b), StructName(a, b, ..) — delegates to `parse_pattern()`
    /// for tuple and constructor cases.
    fn parse_binding_pattern(&mut self) -> Result<Spanned<Pattern>, ParseError> {
        // Fast path: plain identifier without constructor parens → Binding
        if let Token::Identifier(_) = self.peek() {
            if !matches!(self.peek_ahead(1), Token::LParen | Token::Dot) {
                let name = self.expect_identifier()?;
                return Ok(Spanned::new(Pattern::Binding(name.node), name.span));
            }
        }
        self.parse_pattern()
    }
}
