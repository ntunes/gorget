use crate::lexer::token::{Keyword, Token};
use crate::span::Spanned;

use super::ast::*;
use super::Parser;
use crate::errors::ParseError;

impl Parser {
    /// Parse a statement.
    pub fn parse_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();

        match self.peek().clone() {
            // Explicit control flow keywords
            Token::Keyword(Keyword::Return) => self.parse_return_stmt(),
            Token::Keyword(Keyword::Throw) => self.parse_throw_stmt(),
            Token::Keyword(Keyword::Assert) => self.parse_assert_stmt(),
            Token::Keyword(Keyword::Break) => self.parse_break_stmt(),
            Token::Keyword(Keyword::Continue) => {
                self.advance();
                let end = self.previous_span();
                self.consume_newline();
                Ok(Spanned::new(Stmt::Continue, start.merge(end)))
            }
            Token::Keyword(Keyword::Pass) => {
                self.advance();
                let end = self.previous_span();
                self.consume_newline();
                Ok(Spanned::new(Stmt::Pass, start.merge(end)))
            }

            // Control flow statements
            Token::Keyword(Keyword::If) => self.parse_if_stmt(),
            Token::Keyword(Keyword::For) => self.parse_for_stmt(),
            Token::Keyword(Keyword::While) => self.parse_while_stmt(),
            Token::Keyword(Keyword::Loop) => self.parse_loop_stmt(),
            Token::Keyword(Keyword::Match) => self.parse_match_stmt(),
            Token::Keyword(Keyword::With) => self.parse_with_stmt(),
            Token::Keyword(Keyword::Unsafe) => self.parse_unsafe_stmt(),

            // const — could be variable declaration
            Token::Keyword(Keyword::Const) => self.parse_const_var_decl(),

            // auto — type-inferred variable declaration
            Token::Keyword(Keyword::Auto) => self.parse_auto_var_decl(),

            // mutable — prefix for mutable variable declaration under immutable-by-default
            Token::Keyword(Keyword::Mutable) => self.parse_decl_or_expr_stmt(),

            // Type keyword starting a declaration or expression
            _ if self.is_type_start() => self.parse_decl_or_expr_stmt(),

            // Expression statement (or assignment)
            _ => self.parse_expr_or_assign_stmt(),
        }
    }

    fn parse_return_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Return)?;

        let value = if !self.check(&Token::Newline) && !self.check(&Token::Dedent) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        let end = self.previous_span();
        self.consume_newline();
        Ok(Spanned::new(Stmt::Return(value), start.merge(end)))
    }

    fn parse_throw_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Throw)?;
        let value = self.parse_expr()?;
        let end = value.span;
        self.consume_newline();
        Ok(Spanned::new(Stmt::Throw(value), start.merge(end)))
    }

    fn parse_break_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Break)?;

        let value = if !self.check(&Token::Newline) && !self.check(&Token::Dedent) && self.is_expr_start() {
            Some(self.parse_expr()?)
        } else {
            None
        };

        let end = self.previous_span();
        self.consume_newline();
        Ok(Spanned::new(Stmt::Break(value), start.merge(end)))
    }

    fn parse_assert_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Assert)?;
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

    fn parse_if_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::If)?;
        let condition = self.parse_expr()?;
        let then_body = self.parse_block()?;

        let mut elif_branches = Vec::new();
        let mut else_body = None;

        while self.match_keyword(Keyword::Elif) {
            let elif_cond = self.parse_expr()?;
            let elif_body = self.parse_block()?;
            elif_branches.push((elif_cond, elif_body));
        }

        if self.match_keyword(Keyword::Else) {
            else_body = Some(self.parse_block()?);
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

        let pattern = self.parse_pattern()?;
        self.expect_keyword(Keyword::In)?;

        // Check for ownership modifier on iterable
        let ownership = self.parse_ownership_modifier();

        let iterable = self.parse_expr()?;
        let body = self.parse_block()?;

        let else_body = if self.match_keyword(Keyword::Else) {
            Some(self.parse_block()?)
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
        let body = self.parse_block()?;

        let else_body = if self.match_keyword(Keyword::Else) {
            Some(self.parse_block()?)
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
        let body = self.parse_block()?;
        let end = self.previous_span();
        Ok(Spanned::new(Stmt::Loop { body }, start.merge(end)))
    }

    fn parse_match_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Match)?;
        let scrutinee = self.parse_expr()?;
        self.expect(&Token::Colon)?;
        self.expect(&Token::Newline)?;
        self.expect(&Token::Indent)?;

        let mut arms = Vec::new();
        let mut else_arm = None;

        while !self.check(&Token::Dedent) && !self.at_end() {
            if self.check(&Token::Newline) {
                self.advance();
                continue;
            }

            if self.match_keyword(Keyword::Else) {
                else_arm = Some(self.parse_block()?);
                continue;
            }

            let arm_start = self.peek_span();
            self.expect_keyword(Keyword::Case)?;
            let pattern = self.parse_pattern()?;

            let guard = if self.match_keyword(Keyword::If) {
                Some(self.parse_expr()?)
            } else {
                None
            };

            // Check if this is a single-expression arm or a block arm
            self.expect(&Token::Colon)?;
            let body = if self.check(&Token::Newline) {
                // Block arm
                self.advance();
                self.expect(&Token::Indent)?;
                let mut stmts = Vec::new();
                while !self.check(&Token::Dedent) && !self.at_end() {
                    match self.parse_stmt() {
                        Ok(stmt) => stmts.push(stmt),
                        Err(e) => {
                            self.errors.push(e);
                            self.synchronize();
                        }
                    }
                }
                self.expect(&Token::Dedent)?;
                let end = self.previous_span();
                Spanned::new(
                    Expr::Block(Block {
                        stmts,
                        span: arm_start.merge(end),
                    }),
                    arm_start.merge(end),
                )
            } else {
                // Expression arm on same line
                let expr = self.parse_expr()?;
                self.consume_newline();
                expr
            };

            let arm_end = body.span;
            arms.push(MatchArm {
                pattern: pattern.clone(),
                guard,
                body,
                span: arm_start.merge(arm_end),
            });
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

    fn parse_with_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::With)?;

        let mut bindings = Vec::new();
        loop {
            let bind_start = self.peek_span();
            let full_expr = self.parse_expr()?;
            let bind_end = self.previous_span();
            let binding = self.decompose_as_binding(full_expr, bind_start.merge(bind_end))?;
            bindings.push(binding);

            if !self.match_token(&Token::Comma) {
                break;
            }
        }

        let body = self.parse_block()?;
        let end = self.previous_span();

        Ok(Spanned::new(
            Stmt::With { bindings, body },
            start.merge(end),
        ))
    }

    fn parse_unsafe_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Unsafe)?;
        let body = self.parse_block()?;
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
        let end = value.span;
        self.consume_newline();

        Ok(Spanned::new(
            Stmt::VarDecl {
                is_const: true,
                is_mutable: false,
                type_,
                pattern,
                value,
            },
            start.merge(end),
        ))
    }

    fn parse_auto_var_decl(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Auto)?;

        let type_ = Spanned::new(Type::Inferred, start);
        let pattern = self.parse_binding_pattern()?;
        self.expect(&Token::Eq)?;
        let value = self.parse_expr()?;
        let end = value.span;
        self.consume_newline();

        Ok(Spanned::new(
            Stmt::VarDecl {
                is_const: false,
                is_mutable: false,
                type_,
                pattern,
                value,
            },
            start.merge(end),
        ))
    }

    /// Try to parse a declaration (type name = expr) or fall back to expression statement.
    fn parse_decl_or_expr_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek_span();
        let saved_pos = self.pos;

        // Check for `mutable type name = expr` or `mutable auto name = expr` prefix syntax
        if self.check_keyword(Keyword::Mutable) {
            let mutable_pos = self.pos;
            self.advance(); // consume `mutable`

            // Try `mutable auto name = expr`
            if self.check_keyword(Keyword::Auto) {
                let auto_start = self.peek_span();
                self.advance(); // consume `auto`
                let type_ = Spanned::new(Type::Inferred, auto_start);
                if let Token::Identifier(_) = self.peek() {
                    let name = self.expect_identifier()?;
                    if self.match_token(&Token::Eq) {
                        let value = self.parse_expr()?;
                        let end = value.span;
                        self.consume_newline();
                        return Ok(Spanned::new(
                            Stmt::VarDecl {
                                is_const: false,
                                is_mutable: true,
                                type_,
                                pattern: Spanned::new(Pattern::Binding(name.node), name.span),
                                value,
                            },
                            start.merge(end),
                        ));
                    }
                }
                // Not a declaration — backtrack past `mutable`
                self.pos = mutable_pos;
            } else {
                // Try `mutable type name = expr`
                match self.parse_type() {
                    Ok(type_) => {
                        if let Token::Identifier(_) = self.peek() {
                            let name = self.expect_identifier()?;
                            if self.match_token(&Token::Eq) {
                                let value = self.parse_expr()?;
                                let end = value.span;
                                self.consume_newline();
                                return Ok(Spanned::new(
                                    Stmt::VarDecl {
                                        is_const: false,
                                        is_mutable: true,
                                        type_,
                                        pattern: Spanned::new(Pattern::Binding(name.node), name.span),
                                        value,
                                    },
                                    start.merge(end),
                                ));
                            }
                        }
                        // Not a declaration — backtrack past `mutable`
                        self.pos = mutable_pos;
                    }
                    Err(_) => {
                        // Not a type after `mutable` — backtrack
                        self.pos = mutable_pos;
                    }
                }
            }
        }

        // Try to parse as a type followed by a name
        match self.parse_type() {
            Ok(type_) => {
                // Check if the next token is an identifier (variable name)
                if let Token::Identifier(_) = self.peek() {
                    let name = self.expect_identifier()?;

                    // Check what follows the name
                    if self.match_token(&Token::Eq) {
                        // Variable declaration: type name = expr
                        let value = self.parse_expr()?;
                        let end = value.span;
                        self.consume_newline();
                        return Ok(Spanned::new(
                            Stmt::VarDecl {
                                is_const: false,
                                is_mutable: false,
                                type_,
                                pattern: Spanned::new(Pattern::Binding(name.node), name.span),
                                value,
                            },
                            start.merge(end),
                        ));
                    }

                    // Not a declaration — backtrack
                    self.pos = saved_pos;
                    return self.parse_expr_or_assign_stmt();
                }

                // Check for ownership modifier: type &name, type !name, type mutable name, type moving name
                if self.check(&Token::Ampersand) || self.check(&Token::Bang)
                    || self.check_keyword(Keyword::Mutable) || self.check_keyword(Keyword::Moving) {
                    let ownership_tok = self.advance();
                    let is_mutable = matches!(ownership_tok.node, Token::Keyword(Keyword::Mutable));
                    if let Token::Identifier(_) = self.peek() {
                        let name = self.expect_identifier()?;
                        if self.match_token(&Token::Eq) {
                            let value = self.parse_expr()?;
                            let end = value.span;
                            self.consume_newline();
                            return Ok(Spanned::new(
                                Stmt::VarDecl {
                                    is_const: false,
                                    is_mutable,
                                    type_,
                                    pattern: Spanned::new(Pattern::Binding(name.node), name.span),
                                    value,
                                },
                                start.merge(end),
                            ));
                        }
                    }
                    // Not a declaration — backtrack
                    self.pos = saved_pos;
                    return self.parse_expr_or_assign_stmt();
                }

                // Not followed by identifier — backtrack, parse as expression
                self.pos = saved_pos;
                self.parse_expr_or_assign_stmt()
            }
            Err(_) => {
                // Not a type — backtrack and parse as expression
                self.pos = saved_pos;
                self.parse_expr_or_assign_stmt()
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

        // Check for compound assignment
        let compound_op = match self.peek() {
            Token::PlusEq => Some(BinaryOp::Add),
            Token::MinusEq => Some(BinaryOp::Sub),
            Token::StarEq => Some(BinaryOp::Mul),
            Token::SlashEq => Some(BinaryOp::Div),
            Token::PercentEq => Some(BinaryOp::Mod),
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

    /// Parse a simple binding pattern for variable declarations.
    /// Supports: name, (a, b), StructName(a, b, ..)
    fn parse_binding_pattern(&mut self) -> Result<Spanned<Pattern>, ParseError> {
        let start = self.peek_span();

        if self.match_token(&Token::LParen) {
            // Tuple destructuring: (a, b, c)
            let mut patterns = Vec::new();
            while !self.check(&Token::RParen) && !self.at_end() {
                patterns.push(self.parse_pattern()?);
                if !self.check(&Token::RParen) {
                    self.expect(&Token::Comma)?;
                }
            }
            self.expect(&Token::RParen)?;
            let end = self.previous_span();
            Ok(Spanned::new(Pattern::Tuple(patterns), start.merge(end)))
        } else if let Token::Identifier(_) = self.peek() {
            let name = self.expect_identifier()?;

            // Check for struct destructuring: Name(a, b)
            if self.check(&Token::LParen) {
                self.advance();
                let mut fields = Vec::new();
                while !self.check(&Token::RParen) && !self.at_end() {
                    fields.push(self.parse_pattern()?);
                    if !self.check(&Token::RParen) {
                        self.expect(&Token::Comma)?;
                    }
                }
                self.expect(&Token::RParen)?;
                let end = self.previous_span();
                Ok(Spanned::new(
                    Pattern::Constructor {
                        path: vec![name],
                        fields,
                    },
                    start.merge(end),
                ))
            } else {
                Ok(Spanned::new(Pattern::Binding(name.node), name.span))
            }
        } else {
            self.parse_pattern()
        }
    }
}
