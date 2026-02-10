use crate::lexer::token::{Keyword, Token};
use crate::span::{Span, Spanned};

use super::ast::*;
use super::Parser;
use crate::errors::ParseError;

/// Recursively check whether an expression contains `Expr::It`.
/// Returns `false` if `it` only appears inside a nested closure (where it
/// would be bound by that closure instead).
fn contains_it(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::It => true,

        // Stop recursion at closure boundaries — `it` inside a nested
        // closure belongs to that closure, not an outer implicit one.
        Expr::Closure { .. } | Expr::ImplicitClosure { .. } => false,

        // Unary
        Expr::UnaryOp { operand, .. } => contains_it(operand),
        Expr::Try { expr } | Expr::Move { expr } | Expr::MutableBorrow { expr }
        | Expr::Deref { expr } | Expr::Await { expr } | Expr::Spawn { expr }
        | Expr::TryCapture { expr } => contains_it(expr),

        // Binary
        Expr::BinaryOp { left, right, .. } => contains_it(left) || contains_it(right),
        Expr::NilCoalescing { lhs, rhs } => contains_it(lhs) || contains_it(rhs),

        // Access
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. }
        | Expr::OptionalChain { object, .. } => contains_it(object),
        Expr::Index { object, index } => contains_it(object) || contains_it(index),

        // Calls
        Expr::Call { callee, args, .. } => {
            contains_it(callee) || args.iter().any(|a| contains_it(&a.node.value))
        }
        Expr::MethodCall { receiver, args, .. } => {
            contains_it(receiver) || args.iter().any(|a| contains_it(&a.node.value))
        }

        // If
        Expr::If { condition, then_branch, elif_branches, else_branch } => {
            contains_it(condition)
                || contains_it(then_branch)
                || elif_branches.iter().any(|(c, b)| contains_it(c) || contains_it(b))
                || else_branch.as_ref().is_some_and(|b| contains_it(b))
        }

        // Match
        Expr::Match { scrutinee, arms, else_arm } => {
            contains_it(scrutinee)
                || arms.iter().any(|arm| {
                    arm.guard.as_ref().is_some_and(|g| contains_it(g))
                        || contains_it(&arm.body)
                })
                || else_arm.as_ref().is_some_and(|b| contains_it(b))
        }

        // Cast
        Expr::As { expr, .. } => contains_it(expr),

        // Is
        Expr::Is { expr, .. } => contains_it(expr),

        // Range
        Expr::Range { start, end, .. } => {
            start.as_ref().is_some_and(|s| contains_it(s))
                || end.as_ref().is_some_and(|e| contains_it(e))
        }

        // Collections
        Expr::ArrayLiteral(elems) | Expr::TupleLiteral(elems) => {
            elems.iter().any(contains_it)
        }
        Expr::StructLiteral { args, .. } => args.iter().any(contains_it),

        // Comprehensions — these introduce their own bindings, but `it`
        // would still refer to the outer implicit closure if present.
        Expr::ListComprehension { expr, iterable, condition, .. } => {
            contains_it(expr)
                || contains_it(iterable)
                || condition.as_ref().is_some_and(|c| contains_it(c))
        }
        Expr::DictComprehension { key, value, iterable, condition, .. } => {
            contains_it(key)
                || contains_it(value)
                || contains_it(iterable)
                || condition.as_ref().is_some_and(|c| contains_it(c))
        }
        Expr::SetComprehension { expr, iterable, condition, .. } => {
            contains_it(expr)
                || contains_it(iterable)
                || condition.as_ref().is_some_and(|c| contains_it(c))
        }

        // Block / Do — walk statements for expressions
        Expr::Block(block) | Expr::Do { body: block } => block_contains_it(block),

        // Leaves — no sub-expressions
        Expr::IntLiteral(_) | Expr::FloatLiteral(_) | Expr::BoolLiteral(_)
        | Expr::CharLiteral(_) | Expr::StringLiteral(_) | Expr::NoneLiteral
        | Expr::Identifier(_) | Expr::SelfExpr | Expr::Path { .. } => false,
    }
}

/// Check whether any statement in a block contains `Expr::It`.
fn block_contains_it(block: &Block) -> bool {
    block.stmts.iter().any(|stmt| stmt_contains_it(&stmt.node))
}

fn stmt_contains_it(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Expr(e) => contains_it(e),
        Stmt::VarDecl { value, .. } => contains_it(value),
        Stmt::Assign { target, value } => contains_it(target) || contains_it(value),
        Stmt::CompoundAssign { target, value, .. } => contains_it(target) || contains_it(value),
        Stmt::Return(Some(e)) | Stmt::Throw(e) | Stmt::Break(Some(e)) => contains_it(e),
        Stmt::Return(None) | Stmt::Break(None) | Stmt::Continue | Stmt::Pass => false,
        Stmt::For { iterable, body, else_body, .. } => {
            contains_it(iterable)
                || block_contains_it(body)
                || else_body.as_ref().is_some_and(block_contains_it)
        }
        Stmt::While { condition, body, else_body } => {
            contains_it(condition)
                || block_contains_it(body)
                || else_body.as_ref().is_some_and(block_contains_it)
        }
        Stmt::Loop { body } | Stmt::Unsafe { body } => block_contains_it(body),
        Stmt::If { condition, then_body, elif_branches, else_body } => {
            contains_it(condition)
                || block_contains_it(then_body)
                || elif_branches.iter().any(|(c, b)| contains_it(c) || block_contains_it(b))
                || else_body.as_ref().is_some_and(block_contains_it)
        }
        Stmt::Match { scrutinee, arms, else_arm } => {
            contains_it(scrutinee)
                || arms.iter().any(|arm| {
                    arm.guard.as_ref().is_some_and(|g| contains_it(g))
                        || contains_it(&arm.body)
                })
                || else_arm.as_ref().is_some_and(block_contains_it)
        }
        Stmt::With { bindings, body } => {
            bindings.iter().any(|b| contains_it(&b.expr))
                || block_contains_it(body)
        }
        Stmt::Item(_) => false,
    }
}

/// Binding power (precedence) for operators.
/// Higher = tighter binding. Left-assoc: (left_bp, left_bp + 1).
/// Right-assoc: (right_bp + 1, right_bp).
#[derive(Debug, Clone, Copy)]
struct InfixBP {
    left: u8,
    right: u8,
    op: InfixOp,
}

#[derive(Debug, Clone, Copy)]
enum InfixOp {
    Binary(BinaryOp),
    NilCoalescing,
    Is,
    IsNot,
    As,
}

impl Parser {
    /// Parse an expression.
    pub fn parse_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        self.parse_expr_bp(0)
    }

    /// Parse an expression with minimum binding power (Pratt parser).
    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Spanned<Expr>, ParseError> {
        let mut lhs = self.parse_prefix()?;

        loop {
            // Check for postfix operators
            if let Some(bp) = self.postfix_bp() {
                if bp < min_bp {
                    break;
                }
                lhs = self.parse_postfix(lhs)?;
                continue;
            }

            // Check for infix operators
            if let Some(ibp) = self.infix_bp() {
                if ibp.left < min_bp {
                    break;
                }
                lhs = self.parse_infix(lhs, ibp)?;
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    // ── Prefix Parsing ────────────────────────────────────────

    fn parse_prefix(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.peek_span();

        match self.peek().clone() {
            // Literals
            Token::IntLiteral(n) => {
                self.advance();
                Ok(Spanned::new(Expr::IntLiteral(n), start))
            }
            Token::FloatLiteral(n) => {
                self.advance();
                Ok(Spanned::new(Expr::FloatLiteral(n), start))
            }
            Token::BoolLiteral(b) => {
                self.advance();
                Ok(Spanned::new(Expr::BoolLiteral(b), start))
            }
            Token::CharLiteral(c) => {
                self.advance();
                Ok(Spanned::new(Expr::CharLiteral(c), start))
            }
            Token::StringLiteral(s) => {
                self.advance();
                Ok(Spanned::new(Expr::StringLiteral(s), start))
            }
            Token::Keyword(Keyword::True) => {
                self.advance();
                Ok(Spanned::new(Expr::BoolLiteral(true), start))
            }
            Token::Keyword(Keyword::False) => {
                self.advance();
                Ok(Spanned::new(Expr::BoolLiteral(false), start))
            }
            Token::Keyword(Keyword::None) => {
                self.advance();
                Ok(Spanned::new(Expr::NoneLiteral, start))
            }
            Token::Keyword(Keyword::SelfLower) => {
                self.advance();
                Ok(Spanned::new(Expr::SelfExpr, start))
            }
            Token::Keyword(Keyword::It) => {
                self.advance();
                Ok(Spanned::new(Expr::It, start))
            }

            // Unary not
            Token::Keyword(Keyword::Not) => {
                self.advance();
                let operand = self.parse_expr_bp(14)?; // high precedence
                let end = operand.span;
                Ok(Spanned::new(
                    Expr::UnaryOp {
                        op: UnaryOp::Not,
                        operand: Box::new(operand),
                    },
                    start.merge(end),
                ))
            }

            // Unary negation
            Token::Minus => {
                self.advance();
                let operand = self.parse_expr_bp(23)?;
                let end = operand.span;
                Ok(Spanned::new(
                    Expr::UnaryOp {
                        op: UnaryOp::Neg,
                        operand: Box::new(operand),
                    },
                    start.merge(end),
                ))
            }

            // Dereference
            Token::Star => {
                self.advance();
                let operand = self.parse_expr_bp(23)?;
                let end = operand.span;
                Ok(Spanned::new(
                    Expr::Deref {
                        expr: Box::new(operand),
                    },
                    start.merge(end),
                ))
            }

            // Move (! or moving keyword)
            Token::Bang | Token::Keyword(Keyword::Moving) => {
                self.advance();
                // Check for move closure: !(params): or moving (params):
                if self.check(&Token::LParen) {
                    return self.parse_closure(true, false, start);
                }
                let operand = self.parse_expr_bp(23)?;
                let end = operand.span;
                Ok(Spanned::new(
                    Expr::Move {
                        expr: Box::new(operand),
                    },
                    start.merge(end),
                ))
            }

            // Mutable borrow (& or mutable keyword)
            Token::Ampersand | Token::Keyword(Keyword::Mutable) => {
                self.advance();
                let operand = self.parse_expr_bp(23)?;
                let end = operand.span;
                Ok(Spanned::new(
                    Expr::MutableBorrow {
                        expr: Box::new(operand),
                    },
                    start.merge(end),
                ))
            }

            // Await
            Token::Keyword(Keyword::Await) => {
                self.advance();
                let operand = self.parse_expr_bp(2)?;
                let end = operand.span;
                Ok(Spanned::new(
                    Expr::Await {
                        expr: Box::new(operand),
                    },
                    start.merge(end),
                ))
            }

            // Try capture
            Token::Keyword(Keyword::Try) => {
                self.advance();
                let operand = self.parse_expr_bp(2)?;
                let end = operand.span;
                Ok(Spanned::new(
                    Expr::TryCapture {
                        expr: Box::new(operand),
                    },
                    start.merge(end),
                ))
            }

            // Spawn
            Token::Keyword(Keyword::Spawn) => {
                self.advance();
                let operand = self.parse_expr_bp(2)?;
                let end = operand.span;
                Ok(Spanned::new(
                    Expr::Spawn {
                        expr: Box::new(operand),
                    },
                    start.merge(end),
                ))
            }

            // If expression
            Token::Keyword(Keyword::If) => self.parse_if_expr(),

            // Match expression
            Token::Keyword(Keyword::Match) => self.parse_match_expr(),

            // Do expression
            Token::Keyword(Keyword::Do) => {
                self.advance();
                let body = self.parse_block()?;
                let end = self.previous_span();
                Ok(Spanned::new(Expr::Do { body }, start.merge(end)))
            }

            // Parenthesized expression, tuple, or closure
            Token::LParen => self.parse_paren_expr(),

            // Array literal or comprehension
            Token::LBracket => self.parse_array_or_comprehension(),

            // Dict/set literal or comprehension
            Token::LBrace => self.parse_dict_or_set(),

            // Async closure
            Token::Keyword(Keyword::Async) if matches!(self.peek_ahead(1), Token::LParen) => {
                self.advance();
                self.parse_closure(false, true, start)
            }

            // Identifiers and paths
            Token::Identifier(_) => self.parse_identifier_expr(),

            // Named enum/type constructors: Some, Ok, Error
            Token::Keyword(Keyword::Some | Keyword::Ok | Keyword::Error) => {
                let name_str = format!("{}", self.peek()).trim_matches('\'').to_string();
                self.advance();
                let name = Spanned::new(name_str, start);

                if self.check(&Token::LParen) {
                    // Constructor call: Some(value), Ok(value), Error(msg)
                    self.advance();
                    let mut args = Vec::new();
                    while !self.check(&Token::RParen) && !self.at_end() {
                        let arg = self.parse_call_arg()?;
                        args.push(arg);
                        if !self.check(&Token::RParen) {
                            self.expect(&Token::Comma)?;
                        }
                    }
                    self.expect(&Token::RParen)?;
                    let end = self.previous_span();
                    Ok(Spanned::new(
                        Expr::Call {
                            callee: Box::new(Spanned::new(
                                Expr::Identifier(name.node.clone()),
                                name.span,
                            )),
                            generic_args: None,
                            args,
                        },
                        start.merge(end),
                    ))
                } else {
                    Ok(Spanned::new(Expr::Identifier(name.node), start))
                }
            }

            // Smart pointer constructors
            Token::Keyword(
                Keyword::Box
                | Keyword::Rc
                | Keyword::Arc
                | Keyword::Weak
                | Keyword::Cell
                | Keyword::RefCell
                | Keyword::Mutex
                | Keyword::RwLock,
            ) => {
                let name_str = format!("{}", self.peek()).trim_matches('\'').to_string();
                self.advance();
                let end = self.previous_span();
                Ok(Spanned::new(
                    Expr::Identifier(name_str),
                    start.merge(end),
                ))
            }

            // Keyword used as type name in expression context
            Token::Keyword(Keyword::SelfUpper) => {
                self.advance();
                Ok(Spanned::new(Expr::Identifier("Self".to_string()), start))
            }

            _ => Err(self.error_unexpected("expression")),
        }
    }

    // ── Infix Parsing ─────────────────────────────────────────

    fn infix_bp(&self) -> Option<InfixBP> {
        Some(match self.peek() {
            // Assignment operators are handled as statements, not expressions
            Token::Eq | Token::PlusEq | Token::MinusEq | Token::StarEq | Token::SlashEq
            | Token::PercentEq => {
                return None;
            }

            // Nil coalescing
            Token::DoubleQuestion => InfixBP {
                left: 3,
                right: 4,
                op: InfixOp::NilCoalescing,
            },

            // or
            Token::Keyword(Keyword::Or) => InfixBP {
                left: 5,
                right: 6,
                op: InfixOp::Binary(BinaryOp::Or),
            },

            // and
            Token::Keyword(Keyword::And) => InfixBP {
                left: 7,
                right: 8,
                op: InfixOp::Binary(BinaryOp::And),
            },

            // is / is not
            Token::Keyword(Keyword::Is) => {
                // Check for "is not"
                if matches!(self.peek_ahead(1), Token::Keyword(Keyword::Not)) {
                    InfixBP {
                        left: 9,
                        right: 10,
                        op: InfixOp::IsNot,
                    }
                } else {
                    InfixBP {
                        left: 9,
                        right: 10,
                        op: InfixOp::Is,
                    }
                }
            }

            // Comparison
            Token::EqEq => InfixBP {
                left: 11,
                right: 12,
                op: InfixOp::Binary(BinaryOp::Eq),
            },
            Token::BangEq => InfixBP {
                left: 11,
                right: 12,
                op: InfixOp::Binary(BinaryOp::Neq),
            },
            Token::Lt => InfixBP {
                left: 13,
                right: 14,
                op: InfixOp::Binary(BinaryOp::Lt),
            },
            Token::Gt => InfixBP {
                left: 13,
                right: 14,
                op: InfixOp::Binary(BinaryOp::Gt),
            },
            Token::LtEq => InfixBP {
                left: 13,
                right: 14,
                op: InfixOp::Binary(BinaryOp::LtEq),
            },
            Token::GtEq => InfixBP {
                left: 13,
                right: 14,
                op: InfixOp::Binary(BinaryOp::GtEq),
            },

            // in
            Token::Keyword(Keyword::In) => InfixBP {
                left: 15,
                right: 16,
                op: InfixOp::Binary(BinaryOp::In),
            },

            // Range
            Token::DotDot | Token::DotDotEq => return None, // handled specially

            // Additive
            Token::Plus => InfixBP {
                left: 19,
                right: 20,
                op: InfixOp::Binary(BinaryOp::Add),
            },
            Token::Minus => InfixBP {
                left: 19,
                right: 20,
                op: InfixOp::Binary(BinaryOp::Sub),
            },

            // Multiplicative
            Token::Star => InfixBP {
                left: 21,
                right: 22,
                op: InfixOp::Binary(BinaryOp::Mul),
            },
            Token::Slash => InfixBP {
                left: 21,
                right: 22,
                op: InfixOp::Binary(BinaryOp::Div),
            },
            Token::Percent => InfixBP {
                left: 21,
                right: 22,
                op: InfixOp::Binary(BinaryOp::Mod),
            },

            // as (cast)
            Token::Keyword(Keyword::As) => InfixBP {
                left: 23,
                right: 24,
                op: InfixOp::As,
            },

            _ => return None,
        })
    }

    fn parse_infix(
        &mut self,
        lhs: Spanned<Expr>,
        ibp: InfixBP,
    ) -> Result<Spanned<Expr>, ParseError> {
        let start = lhs.span;

        match ibp.op {
            InfixOp::Binary(op) => {
                self.advance(); // consume operator
                let rhs = self.parse_expr_bp(ibp.right)?;
                let end = rhs.span;
                Ok(Spanned::new(
                    Expr::BinaryOp {
                        left: Box::new(lhs),
                        op,
                        right: Box::new(rhs),
                    },
                    start.merge(end),
                ))
            }
            InfixOp::NilCoalescing => {
                self.advance();
                let rhs = self.parse_expr_bp(ibp.right)?;
                let end = rhs.span;
                Ok(Spanned::new(
                    Expr::NilCoalescing {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    start.merge(end),
                ))
            }
            InfixOp::Is => {
                self.advance(); // consume 'is'
                let pattern = self.parse_pattern()?;
                let end = pattern.span;
                Ok(Spanned::new(
                    Expr::Is {
                        expr: Box::new(lhs),
                        negated: false,
                        pattern,
                    },
                    start.merge(end),
                ))
            }
            InfixOp::IsNot => {
                self.advance(); // consume 'is'
                self.advance(); // consume 'not'
                let pattern = self.parse_pattern()?;
                let end = pattern.span;
                Ok(Spanned::new(
                    Expr::Is {
                        expr: Box::new(lhs),
                        negated: true,
                        pattern,
                    },
                    start.merge(end),
                ))
            }
            InfixOp::As => {
                self.advance(); // consume 'as'
                let type_ = self.parse_type()?;
                let end = type_.span;
                Ok(Spanned::new(
                    Expr::As {
                        expr: Box::new(lhs),
                        type_,
                    },
                    start.merge(end),
                ))
            }
        }
    }

    // ── Postfix Parsing ───────────────────────────────────────

    fn postfix_bp(&self) -> Option<u8> {
        match self.peek() {
            // Field access, method call
            Token::Dot => Some(25),
            // Optional chaining
            Token::QuestionDot => Some(25),
            // Index
            Token::LBracket => Some(25),
            // Function call
            Token::LParen => Some(25),
            // Try operator
            Token::Question => Some(24),
            // Range operators
            Token::DotDot | Token::DotDotEq => Some(17),
            _ => None,
        }
    }

    fn parse_postfix(&mut self, lhs: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        let start = lhs.span;

        match self.peek().clone() {
            Token::Dot => {
                self.advance();
                // Check for tuple field access: .0, .1
                if let Token::IntLiteral(n) = self.peek() {
                    let idx = *n as usize;
                    self.advance();
                    let end = self.previous_span();
                    return Ok(Spanned::new(
                        Expr::TupleFieldAccess {
                            object: Box::new(lhs),
                            index: idx,
                        },
                        start.merge(end),
                    ));
                }

                let field = self.expect_name()?;

                // Check for method call: expr.method(args) or expr.method[T](args)
                if self.check(&Token::LBracket) || self.check(&Token::LParen) {
                    let generic_args = if self.check(&Token::LBracket) {
                        Some(self.parse_generic_type_args()?)
                    } else {
                        None
                    };
                    self.expect(&Token::LParen)?;
                    let args = self.parse_call_args()?;
                    self.expect(&Token::RParen)?;
                    let end = self.previous_span();
                    Ok(Spanned::new(
                        Expr::MethodCall {
                            receiver: Box::new(lhs),
                            method: field,
                            generic_args,
                            args,
                        },
                        start.merge(end),
                    ))
                } else {
                    let end = field.span;
                    Ok(Spanned::new(
                        Expr::FieldAccess {
                            object: Box::new(lhs),
                            field,
                        },
                        start.merge(end),
                    ))
                }
            }

            Token::QuestionDot => {
                self.advance();
                let field = self.expect_name()?;
                let end = field.span;
                Ok(Spanned::new(
                    Expr::OptionalChain {
                        object: Box::new(lhs),
                        field,
                    },
                    start.merge(end),
                ))
            }

            Token::LBracket => {
                // Ambiguity: expr[...] could be indexing OR generic call expr[T](args).
                // Use save/restore backtracking: try parsing as generic type args.
                // If the next token after ] is (, it's a generic call. Otherwise, restore
                // and parse as index.
                let saved_pos = self.pos;
                if let Ok(type_args) = self.parse_generic_type_args() {
                    if self.check(&Token::LParen) {
                        self.advance(); // skip (
                        let args = self.parse_call_args()?;
                        self.expect(&Token::RParen)?;
                        let end = self.previous_span();
                        return Ok(Spanned::new(
                            Expr::Call {
                                callee: Box::new(lhs),
                                generic_args: Some(type_args),
                                args,
                            },
                            start.merge(end),
                        ));
                    }
                }
                // Not a generic call — backtrack, parse as index
                self.pos = saved_pos;
                self.advance(); // skip [
                let index = self.parse_expr()?;
                self.expect(&Token::RBracket)?;
                let end = self.previous_span();
                Ok(Spanned::new(
                    Expr::Index {
                        object: Box::new(lhs),
                        index: Box::new(index),
                    },
                    start.merge(end),
                ))
            }

            Token::LParen => {
                self.advance();
                let args = self.parse_call_args()?;
                self.expect(&Token::RParen)?;
                let end = self.previous_span();
                Ok(Spanned::new(
                    Expr::Call {
                        callee: Box::new(lhs),
                        generic_args: None,
                        args,
                    },
                    start.merge(end),
                ))
            }

            Token::Question => {
                self.advance();
                let end = self.previous_span();
                Ok(Spanned::new(
                    Expr::Try {
                        expr: Box::new(lhs),
                    },
                    start.merge(end),
                ))
            }

            Token::DotDot => {
                self.advance();
                let end_expr = if self.is_expr_start() {
                    Some(Box::new(self.parse_expr_bp(18)?))
                } else {
                    None
                };
                let end = self.previous_span();
                Ok(Spanned::new(
                    Expr::Range {
                        start: Some(Box::new(lhs)),
                        end: end_expr,
                        inclusive: false,
                    },
                    start.merge(end),
                ))
            }

            Token::DotDotEq => {
                self.advance();
                let end_expr = self.parse_expr_bp(18)?;
                let end = end_expr.span;
                Ok(Spanned::new(
                    Expr::Range {
                        start: Some(Box::new(lhs)),
                        end: Some(Box::new(end_expr)),
                        inclusive: true,
                    },
                    start.merge(end),
                ))
            }

            _ => unreachable!(),
        }
    }

    // ── Compound Expressions ──────────────────────────────────

    fn parse_identifier_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.peek_span();
        let name = self.expect_identifier()?;

        // Check for qualified path: Name.member
        // But NOT method call (that's handled by postfix)
        Ok(Spanned::new(Expr::Identifier(name.node), start))
    }

    fn parse_paren_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.peek_span();

        // Try to determine if this is a closure, tuple, or parenthesized expr.
        // Heuristic: save position and try closure first, then backtrack.

        // Quick check: empty parens -> closure or unit tuple
        if matches!(self.peek_ahead(1), Token::RParen) {
            // () — check if followed by : (closure) or not (unit/call)
            if matches!(self.peek_ahead(2), Token::Colon) {
                return self.parse_closure(false, false, start);
            }
        }

        // Check if this looks like a closure: (type name, ...) or (name):
        if self.looks_like_closure() {
            return self.parse_closure(false, false, start);
        }

        // Parse as parenthesized expression or tuple
        self.advance(); // skip (
        if self.check(&Token::RParen) {
            self.advance();
            let end = self.previous_span();
            return Ok(Spanned::new(
                Expr::TupleLiteral(Vec::new()),
                start.merge(end),
            ));
        }

        let first = self.parse_expr()?;

        if self.match_token(&Token::Comma) {
            // Tuple
            let mut items = vec![first];
            while !self.check(&Token::RParen) && !self.at_end() {
                items.push(self.parse_expr()?);
                if !self.check(&Token::RParen) {
                    self.expect(&Token::Comma)?;
                }
            }
            self.expect(&Token::RParen)?;
            let end = self.previous_span();
            Ok(Spanned::new(Expr::TupleLiteral(items), start.merge(end)))
        } else {
            // Parenthesized expression
            self.expect(&Token::RParen)?;
            Ok(first)
        }
    }

    /// Check if the current position looks like a closure: (params):
    fn looks_like_closure(&self) -> bool {
        // Scan ahead through the parens to find matching ), then check for :
        let mut depth = 0;
        let mut i = self.pos;
        loop {
            match self.tokens.get(i).map(|t| &t.node) {
                Some(Token::LParen) => depth += 1,
                Some(Token::RParen) => {
                    depth -= 1;
                    if depth == 0 {
                        // Check if : follows )
                        return matches!(
                            self.tokens.get(i + 1).map(|t| &t.node),
                            Some(Token::Colon)
                        );
                    }
                }
                Some(Token::Eof) | None => return false,
                _ => {}
            }
            i += 1;
        }
    }

    fn parse_closure(
        &mut self,
        is_move: bool,
        is_async: bool,
        start: Span,
    ) -> Result<Spanned<Expr>, ParseError> {
        self.expect(&Token::LParen)?;

        let mut params = Vec::new();
        while !self.check(&Token::RParen) && !self.at_end() {
            let param_start = self.peek_span();

            // Try to parse typed parameter: Type name
            // Or just a name
            let (type_, ownership, name) = if self.is_type_start() {
                // Could be typed parameter
                let saved_pos = self.pos;
                match self.parse_type() {
                    Ok(ty) => {
                        let ownership = self.parse_ownership_modifier();

                        if let Token::Identifier(_) = self.peek() {
                            let n = self.expect_identifier()?;
                            (Some(ty), ownership, n)
                        } else {
                            // Not a typed param — backtrack, treat as untyped
                            self.pos = saved_pos;
                            let n = self.expect_identifier()?;
                            (None, Ownership::Borrow, n)
                        }
                    }
                    Err(_) => {
                        self.pos = saved_pos;
                        let n = self.expect_identifier()?;
                        (None, Ownership::Borrow, n)
                    }
                }
            } else {
                let n = self.expect_identifier()?;
                (None, Ownership::Borrow, n)
            };

            let param_end = self.previous_span();
            params.push(Spanned::new(
                ClosureParam {
                    type_,
                    ownership,
                    name,
                },
                param_start.merge(param_end),
            ));

            if !self.check(&Token::RParen) {
                self.expect(&Token::Comma)?;
            }
        }

        self.expect(&Token::RParen)?;
        self.expect(&Token::Colon)?;

        // Body: either single expression on same line, or indented block
        let body = if self.check(&Token::Newline) {
            // Multi-line closure body
            self.advance(); // consume newline
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
                    span: start.merge(end),
                }),
                start.merge(end),
            )
        } else {
            // Single expression
            self.parse_expr()?
        };

        let end = body.span;
        Ok(Spanned::new(
            Expr::Closure {
                is_move,
                is_async,
                params,
                body: Box::new(body),
            },
            start.merge(end),
        ))
    }

    fn parse_array_or_comprehension(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.peek_span();
        self.advance(); // skip [

        if self.check(&Token::RBracket) {
            self.advance();
            let end = self.previous_span();
            return Ok(Spanned::new(
                Expr::ArrayLiteral(Vec::new()),
                start.merge(end),
            ));
        }

        // Parse first expression
        let first = self.parse_expr()?;

        // Check for comprehension: [expr for x in iter]
        if self.check_keyword(Keyword::For) {
            return self.parse_list_comprehension(first, start);
        }

        // Regular array literal
        let mut items = vec![first];
        while self.match_token(&Token::Comma) {
            if self.check(&Token::RBracket) {
                break; // trailing comma
            }
            items.push(self.parse_expr()?);
        }
        self.expect(&Token::RBracket)?;
        let end = self.previous_span();

        Ok(Spanned::new(Expr::ArrayLiteral(items), start.merge(end)))
    }

    fn parse_list_comprehension(
        &mut self,
        expr: Spanned<Expr>,
        start: Span,
    ) -> Result<Spanned<Expr>, ParseError> {
        self.expect_keyword(Keyword::For)?;
        let variable = self.parse_pattern()?;

        let ownership = self.parse_ownership_modifier();

        self.expect_keyword(Keyword::In)?;
        let iterable = self.parse_expr()?;

        let condition = if self.match_keyword(Keyword::If) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };

        self.expect(&Token::RBracket)?;
        let end = self.previous_span();

        Ok(Spanned::new(
            Expr::ListComprehension {
                expr: Box::new(expr),
                variable,
                ownership,
                iterable: Box::new(iterable),
                condition,
            },
            start.merge(end),
        ))
    }

    fn parse_dict_or_set(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.peek_span();
        self.advance(); // skip {

        if self.check(&Token::RBrace) {
            self.advance();
            let end = self.previous_span();
            // Empty braces — could be empty dict or set. Default to empty array for now.
            return Ok(Spanned::new(
                Expr::ArrayLiteral(Vec::new()),
                start.merge(end),
            ));
        }

        let first = self.parse_expr()?;

        // Check for dict: {key: value, ...}
        if self.match_token(&Token::Colon) {
            let value = self.parse_expr()?;

            // Check for dict comprehension: {k: v for k, v in iter}
            if self.check_keyword(Keyword::For) {
                self.expect_keyword(Keyword::For)?;
                let mut vars = vec![self.expect_identifier()?];
                while self.match_token(&Token::Comma) {
                    vars.push(self.expect_identifier()?);
                }
                self.expect_keyword(Keyword::In)?;
                let iterable = self.parse_expr()?;
                let condition = if self.match_keyword(Keyword::If) {
                    Some(Box::new(self.parse_expr()?))
                } else {
                    None
                };
                self.expect(&Token::RBrace)?;
                let end = self.previous_span();
                return Ok(Spanned::new(
                    Expr::DictComprehension {
                        key: Box::new(first),
                        value: Box::new(value),
                        variables: vars,
                        iterable: Box::new(iterable),
                        condition,
                    },
                    start.merge(end),
                ));
            }

            // Regular dict literal — just parse remaining key:value pairs
            // For now, represent as array of tuples
            let mut pairs = vec![Spanned::new(
                Expr::TupleLiteral(vec![first, value]),
                start,
            )];
            while self.match_token(&Token::Comma) {
                if self.check(&Token::RBrace) {
                    break;
                }
                let k = self.parse_expr()?;
                self.expect(&Token::Colon)?;
                let v = self.parse_expr()?;
                let span = k.span.merge(v.span);
                pairs.push(Spanned::new(Expr::TupleLiteral(vec![k, v]), span));
            }
            self.expect(&Token::RBrace)?;
            let end = self.previous_span();
            return Ok(Spanned::new(Expr::ArrayLiteral(pairs), start.merge(end)));
        }

        // Check for set comprehension: {expr for x in iter}
        if self.check_keyword(Keyword::For) {
            self.expect_keyword(Keyword::For)?;
            let variable = self.expect_identifier()?;
            self.expect_keyword(Keyword::In)?;
            let iterable = self.parse_expr()?;
            let condition = if self.match_keyword(Keyword::If) {
                Some(Box::new(self.parse_expr()?))
            } else {
                None
            };
            self.expect(&Token::RBrace)?;
            let end = self.previous_span();
            return Ok(Spanned::new(
                Expr::SetComprehension {
                    expr: Box::new(first),
                    variable,
                    iterable: Box::new(iterable),
                    condition,
                },
                start.merge(end),
            ));
        }

        // Set literal: {a, b, c}
        let mut items = vec![first];
        while self.match_token(&Token::Comma) {
            if self.check(&Token::RBrace) {
                break;
            }
            items.push(self.parse_expr()?);
        }
        self.expect(&Token::RBrace)?;
        let end = self.previous_span();
        Ok(Spanned::new(Expr::ArrayLiteral(items), start.merge(end)))
    }

    fn parse_if_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::If)?;
        let condition = self.parse_expr()?;
        self.expect(&Token::Colon)?;

        let then_branch = self.parse_expr()?;

        let mut elif_branches = Vec::new();
        let mut else_branch = None;

        while self.match_keyword(Keyword::Elif) {
            let elif_cond = self.parse_expr()?;
            self.expect(&Token::Colon)?;
            let elif_body = self.parse_expr()?;
            elif_branches.push((elif_cond, elif_body));
        }

        if self.match_keyword(Keyword::Else) {
            self.expect(&Token::Colon)?;
            else_branch = Some(Box::new(self.parse_expr()?));
        }

        let end = self.previous_span();
        Ok(Spanned::new(
            Expr::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                elif_branches,
                else_branch,
            },
            start.merge(end),
        ))
    }

    fn parse_match_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Match)?;
        let scrutinee = self.parse_expr()?;
        self.expect(&Token::Colon)?;

        // Match expression arms on same or next lines
        // In expression context, arms use: case Pattern: expr
        let mut arms = Vec::new();
        let mut else_arm = None;

        self.expect(&Token::Newline)?;
        self.expect(&Token::Indent)?;

        while !self.check(&Token::Dedent) && !self.at_end() {
            if self.check(&Token::Newline) {
                self.advance();
                continue;
            }

            if self.match_keyword(Keyword::Else) {
                self.expect(&Token::Colon)?;
                else_arm = Some(Box::new(self.parse_expr()?));
                self.consume_newline();
                continue;
            }

            self.expect_keyword(Keyword::Case)?;
            let pattern = self.parse_pattern()?;

            let guard = if self.match_keyword(Keyword::If) {
                Some(self.parse_expr()?)
            } else {
                None
            };

            self.expect(&Token::Colon)?;
            let body = self.parse_expr()?;
            let arm_end = body.span;
            self.consume_newline();

            arms.push(MatchArm {
                pattern: pattern.clone(),
                guard,
                body,
                span: pattern.span.merge(arm_end),
            });
        }

        self.expect(&Token::Dedent)?;
        let end = self.previous_span();

        Ok(Spanned::new(
            Expr::Match {
                scrutinee: Box::new(scrutinee),
                arms,
                else_arm,
            },
            start.merge(end),
        ))
    }

    // ── Call Arguments ────────────────────────────────────────

    /// Parse generic type arguments: `[T1, T2, ...]`
    /// Expects the opening `[` to be the current token.
    fn parse_generic_type_args(&mut self) -> Result<Vec<Spanned<Type>>, ParseError> {
        self.expect(&Token::LBracket)?;
        let mut args = Vec::new();
        while !self.check(&Token::RBracket) && !self.at_end() {
            args.push(self.parse_type()?);
            if !self.check(&Token::RBracket) {
                self.expect(&Token::Comma)?;
            }
        }
        self.expect(&Token::RBracket)?;
        Ok(args)
    }

    fn parse_call_args(&mut self) -> Result<Vec<Spanned<CallArg>>, ParseError> {
        let mut args = Vec::new();
        if self.check(&Token::RParen) {
            return Ok(args);
        }

        loop {
            args.push(self.parse_call_arg()?);
            if !self.match_token(&Token::Comma) {
                break;
            }
        }
        Ok(args)
    }

    fn parse_call_arg(&mut self) -> Result<Spanned<CallArg>, ParseError> {
        let start = self.peek_span();

        // Check for ownership modifiers
        let ownership = self.parse_ownership_modifier();

        // Check for named argument: name = value
        let name = if matches!(self.peek(), Token::Identifier(_))
            && matches!(self.peek_ahead(1), Token::Eq)
            && !matches!(self.peek_ahead(1), Token::EqEq)
        {
            let n = self.expect_identifier()?;
            self.advance(); // skip =
            Some(n)
        } else {
            None
        };

        self.call_arg_depth += 1;
        let value = self.parse_expr()?;
        self.call_arg_depth -= 1;

        // Auto-wrap: if the argument expression contains `it`, wrap it in
        // an ImplicitClosure so downstream passes treat it as a lambda.
        // Only wrap at the outermost call-arg level (depth 0) to prevent
        // double-wrapping when `it` appears inside nested calls like
        // `and_then(Some(it + 1))`.
        let value = if self.call_arg_depth == 0 && contains_it(&value) {
            let span = value.span;
            Spanned::new(Expr::ImplicitClosure { body: Box::new(value) }, span)
        } else {
            value
        };

        let end = value.span;

        Ok(Spanned::new(
            CallArg {
                name,
                ownership,
                value,
            },
            start.merge(end),
        ))
    }

    // ── Helpers ───────────────────────────────────────────────

    /// Check if the current token can start an expression.
    pub fn is_expr_start(&self) -> bool {
        matches!(
            self.peek(),
            Token::IntLiteral(_)
                | Token::FloatLiteral(_)
                | Token::StringLiteral(_)
                | Token::CharLiteral(_)
                | Token::BoolLiteral(_)
                | Token::Identifier(_)
                | Token::LParen
                | Token::LBracket
                | Token::LBrace
                | Token::Minus
                | Token::Star
                | Token::Bang
                | Token::Ampersand
                | Token::Keyword(Keyword::True)
                | Token::Keyword(Keyword::False)
                | Token::Keyword(Keyword::None)
                | Token::Keyword(Keyword::Some)
                | Token::Keyword(Keyword::Ok)
                | Token::Keyword(Keyword::Error)
                | Token::Keyword(Keyword::Not)
                | Token::Keyword(Keyword::If)
                | Token::Keyword(Keyword::Match)
                | Token::Keyword(Keyword::Do)
                | Token::Keyword(Keyword::Await)
                | Token::Keyword(Keyword::Try)
                | Token::Keyword(Keyword::Spawn)
                | Token::Keyword(Keyword::SelfLower)
                | Token::Keyword(Keyword::It)
                | Token::Keyword(Keyword::Moving)
                | Token::Keyword(Keyword::Mutable)
        )
    }

    /// Check if the current token can start a type.
    pub fn is_type_start(&self) -> bool {
        matches!(
            self.peek(),
            Token::Keyword(
                Keyword::Int
                    | Keyword::Int8
                    | Keyword::Int16
                    | Keyword::Int32
                    | Keyword::Int64
                    | Keyword::Uint
                    | Keyword::Uint8
                    | Keyword::Uint16
                    | Keyword::Uint32
                    | Keyword::Uint64
                    | Keyword::Float
                    | Keyword::Float32
                    | Keyword::Float64
                    | Keyword::Bool
                    | Keyword::Char
                    | Keyword::Str
                    | Keyword::StringType
                    | Keyword::Void
                    | Keyword::Auto
                    | Keyword::SelfUpper
                    | Keyword::Dynamic
                    | Keyword::Box
                    | Keyword::Rc
                    | Keyword::Arc
                    | Keyword::Weak
                    | Keyword::Cell
                    | Keyword::RefCell
                    | Keyword::Mutex
                    | Keyword::RwLock
            ) | Token::Identifier(_)
                | Token::LParen
        )
    }
}
