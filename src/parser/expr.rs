use crate::lexer::token::{Keyword, Token};
use crate::span::{Span, Spanned};

use super::ast::*;
use super::Parser;
use crate::errors::{ParseError, ParseErrorKind};

/// Maximum AST-tree nesting depth for a single expression. A deeper expression
/// overflows the lowering recursion (SIGSEGV on the gg compiler's own stack);
/// the parser rejects it first with a clean teaching error. The limit is
/// gg-specific: rustc's `recursion_limit` is also 128, which is 2.46× over the
/// deepest expression in any real fixture (52) and comfortably under the
/// sized-stack-widened crash wall. See `ExprDepthGuard`.
pub(crate) const MAX_EXPR_DEPTH: usize = 128;

/// RAII guard that increments `call_arg_depth` on creation and decrements on drop,
/// ensuring the counter stays consistent even if parsing returns early.
struct CallArgGuard<'a> {
    parser: &'a mut Parser,
}

impl<'a> CallArgGuard<'a> {
    fn new(parser: &'a mut Parser) -> Self {
        parser.call_arg_depth += 1;
        CallArgGuard { parser }
    }
}

impl Drop for CallArgGuard<'_> {
    fn drop(&mut self) {
        self.parser.call_arg_depth -= 1;
    }
}

/// RAII guard for the AST-tree expression depth. Increments `expr_depth` on
/// creation and decrements on drop (even on an early `?` return). Construction
/// returns `Err(ExpressionTooDeep)` when the bumped depth exceeds
/// `MAX_EXPR_DEPTH`, so a pathologically nested expression (deep parens / unary)
/// is rejected before lowering recurses. Mirrors `CallArgGuard`; all parsing in
/// the guarded scope goes through `guard.parser`.
struct ExprDepthGuard<'a> {
    parser: &'a mut Parser,
}

impl<'a> ExprDepthGuard<'a> {
    fn new(parser: &'a mut Parser) -> Result<Self, ParseError> {
        parser.expr_depth += 1;
        if parser.expr_depth > MAX_EXPR_DEPTH {
            // Decrement back out so the counter is consistent for the error
            // unwind, then report. (Drop won't run — we never built the guard.)
            let depth = parser.expr_depth;
            parser.expr_depth -= 1;
            return Err(parser.error_expr_too_deep(depth));
        }
        Ok(ExprDepthGuard { parser })
    }
}

impl Drop for ExprDepthGuard<'_> {
    fn drop(&mut self) {
        self.parser.expr_depth -= 1;
    }
}

/// Map a token to a `BinaryOp` if it is a recognised binary operator token.
/// Used to parse `meta +`, `meta -`, `meta *`, etc.
fn binary_op_from_token(tok: &Token) -> Option<BinaryOp> {
    Some(match tok {
        Token::Plus     => BinaryOp::Add,
        Token::Minus    => BinaryOp::Sub,
        Token::Star     => BinaryOp::Mul,
        Token::StarStar => BinaryOp::Pow,
        Token::Slash    => BinaryOp::Div,
        Token::EqEq     => BinaryOp::Eq,
        Token::BangEq   => BinaryOp::Neq,
        Token::Lt       => BinaryOp::Lt,
        Token::Gt       => BinaryOp::Gt,
        Token::LtEq     => BinaryOp::LtEq,
        Token::GtEq     => BinaryOp::GtEq,
        _ => return None,
    })
}

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
        Expr::Move { expr } | Expr::Propagate { expr } | Expr::MutableBorrow { expr }
        | Expr::Deref { expr } | Expr::Await { expr }
        | Expr::Spawn { expr, .. } | Expr::SpawnBlocking { expr, .. } => contains_it(expr),

        // Binary
        Expr::BinaryOp { left, right, .. } => contains_it(left) || contains_it(right),
        Expr::DefaultOp { lhs, rhs } => contains_it(lhs) || contains_it(rhs),

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
                    pattern_contains_it(&arm.pattern.node)
                        || arm.guard.as_ref().is_some_and(|g| contains_it(g))
                        || contains_it(&arm.body)
                })
                || else_arm.as_ref().is_some_and(|b| contains_it(b))
        }

        // Cast
        Expr::As { expr, .. } => contains_it(expr),

        // Is — walk pattern too (Pattern::Literal contains expressions)
        Expr::Is { expr, pattern, .. } => {
            contains_it(expr) || pattern_contains_it(&pattern.node)
        }

        // Range
        Expr::Range { start, end, .. } => {
            start.as_ref().is_some_and(|s| contains_it(s))
                || end.as_ref().is_some_and(|e| contains_it(e))
        }

        // Collections
        Expr::ArrayLiteral(elems) | Expr::TupleLiteral(elems) => {
            elems.iter().any(contains_it)
        }
        Expr::DictLiteral(pairs) => {
            pairs.iter().any(|(k, v)| contains_it(k) || contains_it(v))
        }
        Expr::StructLiteral { args, .. } => args.iter().any(contains_it),
        // generic_args covered by ..

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

        // Dot-shorthand: .Variant(args)
        Expr::DotShorthand { args, .. } => args.iter().any(|a| contains_it(&a.node.value)),

        // Meta op: recurse into operands
        Expr::MetaOpInfix { left, right, .. } => contains_it(left) || contains_it(right),
        Expr::MetaOpToken(_) => false,

        // Rethrow / Catch
        Expr::Rethrow { expr, transform, .. } => contains_it(expr) || contains_it(transform),
        Expr::Catch { expr, recovery, .. } => contains_it(expr) || contains_it(recovery),

        // Leaves — no sub-expressions
        Expr::IntLiteral(_) | Expr::FloatLiteral(_) | Expr::BoolLiteral(_)
        | Expr::StringLiteral(_, _) | Expr::NoneLiteral
        | Expr::Identifier(_) | Expr::SelfExpr | Expr::Path { .. } => false,
    }
}

/// Recursively check whether a pattern contains `Expr::It` (via `Pattern::Literal`).
fn pattern_contains_it(pat: &Pattern) -> bool {
    match pat {
        Pattern::Literal(expr) => contains_it(expr),
        Pattern::Constructor { fields, .. }
        | Pattern::Tuple(fields)
        | Pattern::Or(fields)
        | Pattern::DotShorthand { fields, .. } => {
            fields.iter().any(|f| pattern_contains_it(&f.node))
        }
        Pattern::Wildcard | Pattern::Binding(_) | Pattern::Rest => false,
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
        Stmt::Return(Some(e)) | Stmt::Throw(e) => contains_it(e),
        Stmt::Return(None) | Stmt::Break | Stmt::Continue | Stmt::Pass => false,
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
                || arms.iter().filter_map(|i| i.arm()).any(|arm| {
                    pattern_contains_it(&arm.pattern.node)
                        || arm.guard.as_ref().is_some_and(|g| contains_it(g))
                        || contains_it(&arm.body)
                })
                || else_arm.as_ref().is_some_and(block_contains_it)
        }
        Stmt::Select { arms, else_arm } => {
            arms.iter().any(|arm| {
                (match &arm.op {
                    SelectOp::Recv { channel, .. } => contains_it(channel),
                    SelectOp::Send { channel, value } => contains_it(channel) || contains_it(value),
                }) || block_contains_it(&arm.body)
            }) || else_arm.as_ref().is_some_and(block_contains_it)
        }
        Stmt::With { bindings, body } => {
            bindings.iter().any(|b| contains_it(&b.expr))
                || block_contains_it(body)
        }
        Stmt::Assert { condition, message } | Stmt::AssertReturn { condition, message } => {
            contains_it(condition) || message.as_ref().is_some_and(contains_it)
        }
        Stmt::Snapshot { value, .. } => contains_it(value),
        Stmt::Item(_) => false,
        Stmt::MetaIf { condition, then_body, elif_branches, else_body, .. } => {
            contains_it(condition)
                || block_contains_it(then_body)
                || elif_branches.iter().any(|(c, b)| contains_it(c) || block_contains_it(b))
                || else_body.as_ref().is_some_and(block_contains_it)
        }
        Stmt::MetaFor { range, body, .. } => {
            contains_it(range) || block_contains_it(body)
        }
        Stmt::MetaMatch { scrutinee, arms, else_arm, .. } => {
            contains_it(scrutinee)
                || arms.iter().any(|(c, b)| contains_it(c) || block_contains_it(b))
                || else_arm.as_ref().is_some_and(block_contains_it)
        }
        Stmt::MetaWhile { condition, body, .. } => {
            contains_it(condition) || block_contains_it(body)
        }
        Stmt::MetaConst { value, .. } => contains_it(value),
        Stmt::MetaLog { args, .. } => args.iter().any(contains_it),
        Stmt::NamedScope { body, .. } => block_contains_it(body),
        Stmt::OnError { body } => block_contains_it(body),
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
    DefaultOp,
    Is,
    IsNot,
    As,
    /// `a meta[op_name] b` — placeholder infix op for meta op templates.
    MetaOp,
    /// `expr rethrow (Type name): transform` — inline error transform.
    Rethrow,
    /// `expr catch (name): recovery` — error recovery.
    Catch,
}

/// Pre-parse each interpolation segment of a format-kind string literal
/// into a `Spanned<Expr>`. The returned vector has one entry per
/// `StringSegment::Interpolation` in declaration order; literal segments
/// are skipped. Empty for non-format strings.
///
/// Errors during sub-expression parsing fall back to a literal string
/// fragment carrying the original text — IR-lowering's old re-parse path
/// remains available as a backstop. The semantic-pass benefit (resolution,
/// typecheck, method-mangling) only requires successful parses.
///
/// Span keys must be unique across the module: typecheck uses
/// `method.span.start` to index `inferred_method_targs` and Pass 4.5 sync.
/// Each segment gets a fresh synthetic base offset from the parser's
/// per-instance counter (`Parser::next_interp_offset`) so no two parsed
/// segments in the same parse produce the same span keys, AND span values
/// are deterministic per parse (a process-global atomic would produce
/// non-deterministic values across parallel test runs that share the
/// counter, breaking exact-match comparisons in resolver_comparison etc.).
/// Diagnostics inside f-string segments point at synthetic offsets rather
/// than real source positions — acceptable trade for correctness; the
/// lexer would need to record per-segment source offsets to fix that.
fn parse_format_string_interp_exprs(
    parser: &mut Parser,
    lit: &crate::lexer::token::StringLiteral,
    span: Span,
) -> Vec<Spanned<Expr>> {
    use crate::lexer::token::{StringKind, StringSegment};
    if lit.kind != StringKind::Format {
        return Vec::new();
    }
    let mut out = Vec::new();
    for seg in &lit.segments {
        if let StringSegment::Interpolation(text, _) = seg {
            let base = parser.next_interp_offset;
            parser.next_interp_offset = base + (1usize << 20);
            let parsed = match Parser::new_with_offset(text, base).parse_expr() {
                Ok(e) => e,
                Err(_) => Spanned::new(
                    Expr::StringLiteral(
                        crate::lexer::token::StringLiteral {
                            kind: StringKind::Normal,
                            segments: vec![StringSegment::Literal(text.clone())],
                        },
                        Vec::new(),
                    ),
                    span,
                ),
            };
            out.push(parsed);
        }
    }
    out
}

impl Parser {
    /// Parse an expression.
    pub fn parse_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        self.parse_expr_bp(0)
    }

    /// Parse an expression starting from a given LHS (for `assert return` where `return` is the LHS).
    pub fn parse_expr_with_lhs(&mut self, lhs: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        self.parse_expr_bp_with_lhs(0, lhs)
    }

    /// Parse an expression with minimum binding power (Pratt parser).
    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Spanned<Expr>, ParseError> {
        let lhs = self.parse_prefix()?;
        self.parse_expr_bp_with_lhs(min_bp, lhs)
    }

    /// Pratt parser infix/postfix loop from a given LHS.
    fn parse_expr_bp_with_lhs(&mut self, min_bp: u8, mut lhs: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        // Each infix/postfix node built here extends the LEFT SPINE by one tree
        // level. A flat operator chain (`a + a + … + a`) is parsed iteratively —
        // the parser's call stack stays ~2 deep — so a recursion-depth counter
        // (or the `parse_prefix` guard) would miss it; this accumulated count is
        // the complementary check. We compose it with `self.expr_depth` (the
        // prefix nesting we're already inside, e.g. deep parens around a chain)
        // so the two guards add up rather than each having its own slack. See
        // `MAX_EXPR_DEPTH`.
        let mut spine_depth = self.expr_depth;

        loop {
            // Check for postfix operators
            if let Some(bp) = self.postfix_bp() {
                if bp < min_bp {
                    break;
                }
                spine_depth += 1;
                if spine_depth > MAX_EXPR_DEPTH {
                    return Err(self.error_expr_too_deep(spine_depth));
                }
                lhs = self.parse_postfix(lhs)?;
                continue;
            }

            // Check for infix operators
            if let Some(ibp) = self.infix_bp() {
                if ibp.left < min_bp {
                    break;
                }
                spine_depth += 1;
                if spine_depth > MAX_EXPR_DEPTH {
                    return Err(self.error_expr_too_deep(spine_depth));
                }
                lhs = self.parse_infix(lhs, ibp)?;
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    // ── Prefix Parsing ────────────────────────────────────────

    /// Parse a prefix expression (atom / unary / parenthesised group).
    ///
    /// Thin wrapper that tracks AST-tree depth via `ExprDepthGuard`: each prefix
    /// entry is one tree level, so nested parens/unary (`((((…))))`, `!!!…`)
    /// accumulate depth here. The guard rejects an over-deep nest at parse time
    /// (clean error) instead of letting it SIGSEGV in lowering. The flat
    /// `a + a + …` operator chain is parsed iteratively (parser stack stays ~2
    /// deep), so it's caught by the complementary left-spine check in
    /// `parse_expr_bp_with_lhs`, not here.
    fn parse_prefix(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let guard = ExprDepthGuard::new(self)?;
        guard.parser.parse_prefix_inner()
    }

    fn parse_prefix_inner(&mut self) -> Result<Spanned<Expr>, ParseError> {
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
            Token::StringLiteral(s) => {
                self.advance();
                // For format-kind strings, parse each interpolation segment's
                // expression text up-front so it participates in name resolution,
                // typechecking, and the method-mangling rewriter alongside other
                // expressions. Without this, IR-lowering would re-parse the text
                // and lower a fresh AST that bypassed every semantic pass — the
                // root cause of `f"{v.iter().any(p)}"` link-failing against an
                // un-mangled symbol.
                let interp_exprs = parse_format_string_interp_exprs(self, &s, start);
                Ok(Spanned::new(Expr::StringLiteral(s, interp_exprs), start))
            }
            Token::Keyword(Keyword::True) => {
                self.advance();
                Ok(Spanned::new(Expr::BoolLiteral(true), start))
            }
            Token::Keyword(Keyword::False) => {
                self.advance();
                Ok(Spanned::new(Expr::BoolLiteral(false), start))
            }
            Token::Keyword(Keyword::SelfLower) => {
                self.advance();
                Ok(Spanned::new(Expr::SelfExpr, start))
            }
            Token::Keyword(Keyword::It) => {
                self.advance();
                Ok(Spanned::new(Expr::It, start))
            }

            // Divergent expressions: `throw expr` and `return [expr]` are
            // accepted in expression position so they can flow into things
            // like `?? throw err()`, `if cond: return 0 else: value`, and
            // single-line catch / match arm forms. Wraps the resulting
            // statement in a synthetic `Expr::Block` so downstream lowering
            // — which already handles Block-as-expr — emits the early-exit
            // terminator. The block's value is irrelevant: the typecheck
            // pass treats divergent expressions as compatible with any
            // expected type, and the LIR's `set_terminator` no-op rule
            // (Cluster B fix, 2026-05-12) prevents post-divergent assigns
            // / jumps from clobbering the early exit. Gorget-js critique
            // item #2 (2026-05-13).
            Token::Keyword(Keyword::Throw) => {
                let throw_stmt = self.parse_throw_stmt()?;
                let span = throw_stmt.span;
                Ok(Spanned::new(
                    Expr::Block(Block { stmts: vec![throw_stmt], span }),
                    span,
                ))
            }
            Token::Keyword(Keyword::Return) => {
                let return_stmt = self.parse_return_stmt()?;
                let span = return_stmt.span;
                Ok(Spanned::new(
                    Expr::Block(Block { stmts: vec![return_stmt], span }),
                    span,
                ))
            }

            // Unary not
            Token::Keyword(Keyword::Not) => {
                self.advance();
                let operand = self.parse_expr_bp(20)?; // high precedence
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
                // D28 amendment R1 (docs/define-gorget/decisions.md:1197):
                // `-x ** 2` is REJECT (JS/TC39 guardrail). Detect at parse
                // time via the token stream: if the token right after `-` is
                // `(`, the user parenthesized (`-(x ** 2)` accepts) — the
                // parse_paren_expr call peels the parens but the token check
                // records the shape. Otherwise, after parsing the operand at
                // bp 33 (Pow's right bp), if the top-level operand turns out
                // to be a `Pow`, `-` and `**` were unparenthesized siblings
                // → emit E_AmbiguousUnaryMinusPow.
                let after_minus_lparen = matches!(self.peek(), Token::LParen);
                let operand = self.parse_expr_bp(33)?;
                let end = operand.span;
                if !after_minus_lparen {
                    if let Expr::BinaryOp { op: BinaryOp::Pow, .. } = &operand.node {
                        self.errors.push(ParseError {
                            kind: ParseErrorKind::AmbiguousUnaryMinusPow,
                            span: start.merge(end),
                        });
                    }
                }
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
                let operand = self.parse_expr_bp(33)?;
                let end = operand.span;
                Ok(Spanned::new(
                    Expr::Deref {
                        expr: Box::new(operand),
                    },
                    start.merge(end),
                ))
            }

            // Move expression (D27 accept-both: `!` retired glyph and `^`
            // canonical glyph both produce `Expr::Move`. Round A2 minimal:
            // parser accepts both so docs can teach `^` as canonical while
            // `gg fmt` still normalizes to `!` — Round B does the fmt swap.)
            Token::Bang | Token::Caret => {
                self.advance();
                // Check for move closure: !(params): body / ^(params): body
                if self.check(&Token::LParen) {
                    return self.parse_closure(true, false, start);
                }
                let operand = self.parse_expr_bp(33)?;
                let end = operand.span;
                Ok(Spanned::new(
                    Expr::Move {
                        expr: Box::new(operand),
                    },
                    start.merge(end),
                ))
            }

            // Move closure only (move keyword): move (params): body
            Token::Keyword(Keyword::Move) => {
                self.advance();
                if self.check(&Token::LParen) {
                    return self.parse_closure(true, false, start);
                }
                Err(self.error_at(start,
                    "use `!` for move expressions (e.g. `!x`). \
                     The `move` keyword is only valid for closures: `move (params): body`",
                ))
            }

            // Bitwise NOT
            Token::Tilde => {
                self.advance();
                let operand = self.parse_expr_bp(33)?;
                let end = operand.span;
                Ok(Spanned::new(
                    Expr::UnaryOp {
                        op: UnaryOp::BitNot,
                        operand: Box::new(operand),
                    },
                    start.merge(end),
                ))
            }

            // Mutable borrow (& prefix)
            Token::Ampersand => {
                self.advance();
                let operand = self.parse_expr_bp(33)?;
                let end = operand.span;
                Ok(Spanned::new(
                    Expr::MutableBorrow {
                        expr: Box::new(operand),
                    },
                    start.merge(end),
                ))
            }

            // Spawn / Spawn blocking — optionally preceded by `unchecked`.
            // Accepted shapes: `spawn foo()`, `spawn blocking foo()`,
            // `spawn unchecked foo()`, `spawn blocking unchecked foo()`,
            // `spawn unchecked blocking foo()`.
            Token::Keyword(Keyword::Spawn) => {
                self.advance();
                let mut is_blocking = self.match_keyword(Keyword::Blocking);
                let mut unchecked = self.match_keyword(Keyword::Unchecked);
                if !is_blocking {
                    // Allow `unchecked` before `blocking` as well.
                    is_blocking = self.match_keyword(Keyword::Blocking);
                }
                if !unchecked {
                    unchecked = self.match_keyword(Keyword::Unchecked);
                }
                let operand = self.parse_expr_bp(2)?;
                let end = operand.span;
                let expr_node = if is_blocking {
                    Expr::SpawnBlocking { expr: Box::new(operand), unchecked }
                } else {
                    Expr::Spawn { expr: Box::new(operand), unchecked }
                };
                Ok(Spanned::new(expr_node, start.merge(end)))
            }

            // Prefix await
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

            // Keywords used as identifiers in expression position.
            // Postfix parsing handles `(args)` to form Call, `.method()` for static methods, etc.
            Token::Keyword(
                kw @ (Keyword::StringType
                | Keyword::SelfUpper
                | Keyword::Int | Keyword::Int8 | Keyword::Int16 | Keyword::Int32 | Keyword::Int64
                | Keyword::Uint | Keyword::Uint8 | Keyword::Uint16 | Keyword::Uint32 | Keyword::Uint64
                | Keyword::Float | Keyword::Float32 | Keyword::Float64
                | Keyword::Bool),
            ) => {
                let name = kw.as_name().to_string();
                self.advance();
                let end = self.previous_span();
                Ok(Spanned::new(
                    Expr::Identifier(name),
                    start.merge(end),
                ))
            }

            // Dot-shorthand variant: .Red() or .Blue(42)
            Token::Dot => {
                self.advance();
                let variant = self.expect_identifier()?;
                if self.match_token(&Token::LParen) {
                    let args = self.parse_call_args()?;
                    self.expect(&Token::RParen)?;
                    let end = self.previous_span();
                    Ok(Spanned::new(
                        Expr::DotShorthand { variant, args },
                        start.merge(end),
                    ))
                } else {
                    let end = variant.span;
                    Ok(Spanned::new(
                        Expr::DotShorthand { variant, args: Vec::new() },
                        start.merge(end),
                    ))
                }
            }

            // `meta +` / `meta -` / `meta *` etc. — operator token for meta op params
            Token::Keyword(Keyword::Meta) => {
                if let Some(op) = binary_op_from_token(self.peek_ahead(1)) {
                    self.advance(); // consume `meta`
                    self.advance(); // consume operator token
                    let end = self.previous_span();
                    Ok(Spanned::new(Expr::MetaOpToken(op), start.merge(end)))
                } else {
                    Err(self.error_unexpected(
                        "operator after `meta` (e.g. `meta +`, `meta <`)",
                    ))
                }
            }

            _ => Err(self.error_unexpected("expression")),
        }
    }

    // ── Infix Parsing ─────────────────────────────────────────

    fn infix_bp(&self) -> Option<InfixBP> {
        Some(match self.peek() {
            // Assignment operators are handled as statements, not expressions.
            // D26: compound fallible-assign forms (`+!=` etc) are v1-EXCLUDED
            // per `decisions.md:945`; treated as assignment tokens here so
            // expression parsing stops — the actual reject fires in the
            // stmt-parser via E_CompoundFallibleAssignExcluded.
            Token::Eq | Token::PlusEq | Token::MinusEq | Token::StarEq | Token::SlashEq
            | Token::PercentEq | Token::PlusPercentEq | Token::MinusPercentEq
            | Token::StarPercentEq | Token::StarStarEq | Token::AmpersandEq | Token::PipeEq
            | Token::CaretEq | Token::LtLtEq | Token::GtGtEq
            | Token::PlusBangEq | Token::MinusBangEq | Token::StarBangEq
            | Token::SlashBangEq | Token::PercentBangEq
            | Token::LtLtBangEq | Token::GtGtBangEq => {
                return None;
            }

            // rethrow — inline error transform (lowest precedence)
            Token::Keyword(Keyword::Rethrow) => InfixBP {
                left: 1,
                right: 2,
                op: InfixOp::Rethrow,
            },

            // catch — error recovery (same precedence as rethrow)
            Token::Keyword(Keyword::Catch) => InfixBP {
                left: 1,
                right: 2,
                op: InfixOp::Catch,
            },

            // Default operator
            Token::DoubleQuestion => InfixBP {
                left: 3,
                right: 4,
                op: InfixOp::DefaultOp,
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

            // Bitwise OR
            Token::Pipe => InfixBP {
                left: 17,
                right: 18,
                op: InfixOp::Binary(BinaryOp::BitOr),
            },

            // Bitwise XOR
            Token::Caret => InfixBP {
                left: 19,
                right: 20,
                op: InfixOp::Binary(BinaryOp::BitXor),
            },

            // Bitwise AND
            Token::Ampersand => InfixBP {
                left: 21,
                right: 22,
                op: InfixOp::Binary(BinaryOp::BitAnd),
            },

            // Range
            Token::DotDot | Token::DotDotEq => return None, // handled specially

            // Shifts
            Token::LtLt => InfixBP {
                left: 25,
                right: 26,
                op: InfixOp::Binary(BinaryOp::Shl),
            },
            Token::GtGt => InfixBP {
                left: 25,
                right: 26,
                op: InfixOp::Binary(BinaryOp::Shr),
            },
            // D26 fallible shifts (same precedence as base).
            Token::LtLtBang => InfixBP {
                left: 25,
                right: 26,
                op: InfixOp::Binary(BinaryOp::ShlFallible),
            },
            Token::GtGtBang => InfixBP {
                left: 25,
                right: 26,
                op: InfixOp::Binary(BinaryOp::ShrFallible),
            },

            // Additive
            Token::Plus => InfixBP {
                left: 27,
                right: 28,
                op: InfixOp::Binary(BinaryOp::Add),
            },
            Token::Minus => InfixBP {
                left: 27,
                right: 28,
                op: InfixOp::Binary(BinaryOp::Sub),
            },
            Token::PlusPercent => InfixBP {
                left: 27,
                right: 28,
                op: InfixOp::Binary(BinaryOp::AddWrap),
            },
            Token::MinusPercent => InfixBP {
                left: 27,
                right: 28,
                op: InfixOp::Binary(BinaryOp::SubWrap),
            },
            // D26 fallible additive (same precedence as base).
            Token::PlusBang => InfixBP {
                left: 27,
                right: 28,
                op: InfixOp::Binary(BinaryOp::AddFallible),
            },
            Token::MinusBang => InfixBP {
                left: 27,
                right: 28,
                op: InfixOp::Binary(BinaryOp::SubFallible),
            },

            // Multiplicative
            Token::Star => InfixBP {
                left: 29,
                right: 30,
                op: InfixOp::Binary(BinaryOp::Mul),
            },
            Token::StarPercent => InfixBP {
                left: 29,
                right: 30,
                op: InfixOp::Binary(BinaryOp::MulWrap),
            },
            Token::Slash => InfixBP {
                left: 29,
                right: 30,
                op: InfixOp::Binary(BinaryOp::Div),
            },
            Token::Percent => InfixBP {
                left: 29,
                right: 30,
                op: InfixOp::Binary(BinaryOp::Rem),
            },
            // D26 fallible multiplicative (same precedence as base).
            Token::StarBang => InfixBP {
                left: 29,
                right: 30,
                op: InfixOp::Binary(BinaryOp::MulFallible),
            },
            Token::SlashBang => InfixBP {
                left: 29,
                right: 30,
                op: InfixOp::Binary(BinaryOp::DivFallible),
            },
            Token::PercentBang => InfixBP {
                left: 29,
                right: 30,
                op: InfixOp::Binary(BinaryOp::RemFallible),
            },
            // as (cast)
            Token::Keyword(Keyword::As) => InfixBP {
                left: 31,
                right: 32,
                op: InfixOp::As,
            },

            // Power `**` — right-associative (amendment R2), binds tighter than
            // unary prefix (33) so `2 ** -1` parses `2 ** (-1)` where the unary
            // minus applies to the operand. R1's unparenthesized `-x ** 2`
            // reject is a typecheck-time shape check, not a parser gate.
            // Right-assoc via `(left, left-1)` pattern → `2 ** 3 ** 2` parses
            // as `2 ** (3 ** 2)` = 512.
            Token::StarStar => InfixBP {
                left: 34,
                right: 33,
                op: InfixOp::Binary(BinaryOp::Pow),
            },

            // meta[op_name] — compile-time operator placeholder (same precedence as addition)
            Token::Keyword(Keyword::Meta)
                if matches!(self.peek_ahead(1), Token::LBracket) =>
            {
                InfixBP {
                    left: 27,
                    right: 28,
                    op: InfixOp::MetaOp,
                }
            }

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
            InfixOp::DefaultOp => {
                self.advance();
                let rhs = self.parse_expr_bp(ibp.right)?;
                let end = rhs.span;
                Ok(Spanned::new(
                    Expr::DefaultOp {
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
            InfixOp::MetaOp => {
                self.advance(); // consume `meta`
                self.expect(&Token::LBracket)?;
                let op_name = self.expect_identifier()?.node;
                self.expect(&Token::RBracket)?;
                let rhs = self.parse_expr_bp(ibp.right)?;
                let end = rhs.span;
                Ok(Spanned::new(
                    Expr::MetaOpInfix {
                        left: Box::new(lhs),
                        op_name,
                        right: Box::new(rhs),
                    },
                    start.merge(end),
                ))
            }
            InfixOp::Rethrow => {
                self.advance(); // consume `rethrow`
                // Check for binding form: rethrow (Type name): expr
                // vs bare form: rethrow expr
                let (error_binding, transform) = if self.check(&Token::LParen) {
                    // Binding form: (Type name): transform
                    self.expect(&Token::LParen)?;
                    let error_type = self.parse_type()?;
                    let error_name = self.expect_identifier()?;
                    self.expect(&Token::RParen)?;
                    self.expect(&Token::Colon)?;
                    let transform = self.parse_body_or_expr(start)?;
                    (Some((error_type, error_name)), transform)
                } else {
                    // Bare form: rethrow expr
                    let transform = self.parse_expr_bp(2)?;
                    (None, transform)
                };
                let end = transform.span;
                Ok(Spanned::new(
                    Expr::Rethrow {
                        expr: Box::new(lhs),
                        error_binding,
                        transform: Box::new(transform),
                    },
                    start.merge(end),
                ))
            }
            InfixOp::Catch => {
                self.advance(); // consume `catch`
                // ONE production, three cases distinguished by the token after
                // `catch` (LL(1), error-model.md §1.5):
                //   `(`                 → the EXISTING Result `catch (name):`
                //                          (welded to Result[T,E] — untouched);
                //   `Ident . Ident`     → fault PATTERN `catch Fault.Overflow:`;
                //   `Ident` (no dot)    → fault BINDING `catch f: <body>`.
                if matches!(self.peek(), Token::LParen) {
                    // ── Existing Result-catch path (unchanged contract) ──
                    self.advance(); // consume `(`
                    // The name can be `_` (wildcard) — gorget-js critique #1,
                    // 2026-05-13. Stored as binding name "_"; not a valid
                    // expression-position identifier, so the recovery body can't
                    // reference it (the right wildcard semantic without changing
                    // the AST shape). Mirrors match arms accepting `_`.
                    let error_name = if matches!(self.peek(), Token::Underscore) {
                        let span = self.peek_span();
                        self.advance();
                        Spanned::new(String::from("_"), span)
                    } else {
                        self.expect_identifier()?
                    };
                    self.expect(&Token::RParen)?;
                    self.expect(&Token::Colon)?;
                    let recovery = self.parse_body_or_expr(start)?;
                    let end = recovery.span;
                    return Ok(Spanned::new(
                        Expr::Catch {
                            expr: Box::new(lhs),
                            error_binding: error_name,
                            recovery: Box::new(recovery),
                        },
                        start.merge(end),
                    ));
                }
                // ── D25: the fault-catch form (`catch Fault.X:` or
                // `catch f: match f: …`) was REMOVED. Reject with a teaching
                // diagnostic pointing at the D26 fallible-arithmetic operators
                // (`+!`/`-!`/`*!`/`/!`/`%!`/`<<!`/`>>!`) as the recovery path.
                let reject_span = self.peek_span();
                let ident_span = self.expect_identifier()?.span;
                let mut end = ident_span;
                if matches!(self.peek(), Token::Dot) {
                    self.advance(); // consume `.`
                    end = self.expect_identifier()?.span;
                }
                Err(ParseError {
                    kind: ParseErrorKind::FaultCatchRemoved,
                    span: reject_span.merge(end),
                })
            }
        }
    }

    // ── Postfix Parsing ───────────────────────────────────────

    fn postfix_bp(&self) -> Option<u8> {
        match self.peek() {
            // Field access, method call
            Token::Dot => Some(35),
            // Optional chaining
            Token::QuestionDot => Some(35),
            // Index
            Token::LBracket => Some(35),
            // Function call
            Token::LParen => Some(35),
            // D29: postfix error-propagation `expr!`. Same tight bp as
            // `.`/call/index so `f()!.m()!` chains left-to-right and
            // `f()! catch …` nests as Catch(Propagate(..)). Note `!=` lexes as
            // BangEq (never Bang) under maximal munch, so `a()!=b` is NOT seen
            // here — it stays a not-equal comparison.
            Token::Bang => Some(35),
            // Range operators
            Token::DotDot | Token::DotDotEq => Some(23),
            _ => None,
        }
    }

    fn parse_postfix(&mut self, lhs: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        let start = lhs.span;

        match *self.peek() {
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

                // Postfix .await() → Expr::Await
                if field.node == "await" {
                    self.expect(&Token::LParen)?;
                    self.expect(&Token::RParen)?;
                    let end = self.previous_span();
                    return Ok(Spanned::new(
                        Expr::Await {
                            expr: Box::new(lhs),
                        },
                        start.merge(end),
                    ));
                }

                // Check for method call: expr.method(args) or expr.method[T](args)
                if self.check(&Token::LBracket) || self.check(&Token::LParen) {
                    // Ambiguity: expr.field[...] could be expr.field[T](args) (generic method)
                    // or field access followed by indexing. Try parsing as generic args;
                    // backtrack if it's not followed by `(`.
                    let generic_args = if self.check(&Token::LBracket) {
                        match self.try_parse(|p| {
                            let type_args = p.parse_generic_type_args().ok()?;
                            p.check(&Token::LParen).then_some(type_args)
                        }) {
                            Some(args) => Some(args),
                            None => {
                                // Not a generic method call — return FieldAccess;
                                // the next iteration will handle [
                                let end = field.span;
                                return Ok(Spanned::new(
                                    Expr::FieldAccess {
                                        object: Box::new(lhs),
                                        field,
                                    },
                                    start.merge(end),
                                ));
                            }
                        }
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
                // Try parsing as generic type args followed by `(`.
                if let Some(type_args) = self.try_parse(|p| {
                    let type_args = p.parse_generic_type_args().ok()?;
                    p.check(&Token::LParen).then_some(type_args)
                }) {
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
                // Not a generic call — parse as index
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

            // D29: postfix error-propagation `expr!`.
            Token::Bang => {
                self.advance();
                let end = self.previous_span();
                Ok(Spanned::new(
                    Expr::Propagate {
                        expr: Box::new(lhs),
                    },
                    start.merge(end),
                ))
            }

            Token::DotDot => {
                self.advance();
                let end_expr = if self.is_expr_start() {
                    Some(Box::new(self.parse_expr_bp(24)?))
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
                let end_expr = self.parse_expr_bp(24)?;
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

            _ => unreachable!("unhandled postfix operator"),
        }
    }

    // ── Compound Expressions ──────────────────────────────────

    fn parse_identifier_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.peek_span();
        let name = self.expect_identifier()?;

        // `None` is a prelude variant (Option.None). For historical
        // reasons the AST has a dedicated `Expr::NoneLiteral` node
        // with special IR-lowering support — keep emitting it even
        // though `None` is no longer a lexer keyword.
        if name.node == "None" {
            return Ok(Spanned::new(Expr::NoneLiteral, start));
        }

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
            match self.kinds.get(i) {
                Some(Token::LParen) => depth += 1,
                Some(Token::RParen) => {
                    depth -= 1;
                    if depth == 0 {
                        // Check if : follows )
                        return matches!(
                            self.kinds.get(i + 1),
                            Some(Token::Colon)
                        );
                    }
                }
                // Tokens that cannot appear in closure parameter lists —
                // if found, this is a parenthesized expression, not a closure.
                Some(Token::Keyword(Keyword::And))
                | Some(Token::Keyword(Keyword::Or))
                | Some(Token::Keyword(Keyword::Not))
                // `as` / `is` are infix-only operators on values, never
                // type-list separators. `(x as bool):` (if-condition that
                // IS the cast) and `(x is Some(k)):` (if-condition that
                // IS the pattern test) used to misfire as closure-param
                // parsing producing `expected ',', found 'as'/'is'`.
                | Some(Token::Keyword(Keyword::As))
                | Some(Token::Keyword(Keyword::Is))
                | Some(Token::EqEq)
                | Some(Token::BangEq)
                | Some(Token::LtEq)
                | Some(Token::GtEq)
                | Some(Token::Lt)
                | Some(Token::Gt)
                // Arithmetic / bitwise / shift / range operators that are
                // never sigil characters and never appear in closure
                // param lists. `+`, `/`, `%`, `|`, `^`, `<<`, `>>`, `..`
                // — and their wrapping variants. (`-`, `*`, `&`, `!` are
                // deliberately omitted because they double as unary /
                // sigil tokens in param-type contexts: `-1` default,
                // `*T` pointer, `&T` borrow, `!T` move.)
                | Some(Token::Plus)
                | Some(Token::Slash)
                | Some(Token::Percent)
                | Some(Token::Pipe)
                | Some(Token::Caret)
                | Some(Token::LtLt)
                | Some(Token::GtGt)
                | Some(Token::DotDot)
                | Some(Token::DotDotEq)
                | Some(Token::PlusPercent)
                | Some(Token::MinusPercent)
                | Some(Token::StarPercent)
                // Optional / default-value operators on expressions.
                | Some(Token::Question)
                | Some(Token::DoubleQuestion)
                | Some(Token::QuestionDot)
                // Member access. Closure param types are simple-or-
                // generic (`int`, `Dict[K, V]`, `Vector[T]`) — never
                // dotted. A `.` inside the parens proves this is an
                // expression. Without this, the bare paren-wrapped
                // method-call shape `(d.contains(x))` followed by a
                // trailing `:` (e.g., the if-statement's colon) is
                // misclassified as a closure and the parser tries to
                // consume `d` as a type-name + `.` as something it can't
                // parse, producing `expected ',', found '.'`.
                | Some(Token::Dot) => return false,
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
        // Per-param destructuring metadata. We accumulate it here and stamp it onto the
        // matching `ClosureParam.destructure` field after each push, then later use it
        // to prepend `T name = __dp_<i>._<j>` decls to the body.
        let mut destructure_bindings: Vec<(usize, usize, Spanned<Type>, Ownership, Spanned<String>)> =
            Vec::new();
        while !self.check(&Token::RParen) && !self.at_end() {
            let param_start = self.peek_span();

            // First: try tuple-destructuring pattern `(Type Name, Type Name, ...)`.
            // Only applicable when the param slot opens with `(` AND the inner shape is at least
            // two `Type Name` pairs (or one `Type Name,` followed by another). This disambiguates
            // from `((Tuple) name)` — a tuple-typed param with a trailing name — which is parsed
            // by the existing typed-param branch below.
            let destructure: Option<Vec<(Spanned<Type>, Ownership, Spanned<String>)>> =
                if self.check(&Token::LParen) {
                    self.try_parse(|p| {
                        p.expect(&Token::LParen).ok()?;
                        let mut bindings = Vec::new();
                        loop {
                            let ty = p.parse_type().ok()?;
                            let ownership = p.parse_ownership_modifier();
                            if !matches!(p.peek(), Token::Identifier(_)) {
                                return None;
                            }
                            let n = p.expect_identifier().ok()?;
                            bindings.push((ty, ownership, n));
                            if p.check(&Token::Comma) {
                                p.advance();
                                continue;
                            }
                            break;
                        }
                        p.expect(&Token::RParen).ok()?;
                        // Must be at end of param (next is `,` or closing `)`).
                        if !p.check(&Token::Comma) && !p.check(&Token::RParen) {
                            return None;
                        }
                        // Need at least 2 bindings — single-binding `((T x))` is just
                        // a typed param wrapped in extra parens; reject so the existing
                        // typed-param branch can handle it.
                        if bindings.len() < 2 {
                            return None;
                        }
                        Some(bindings)
                    })
                } else {
                    None
                };

            let (type_, ownership, name, destructure_meta) = if let Some(bindings) = destructure {
                // Synthesize tuple-typed param. Name uses `__dp_` prefix (compiler-internal,
                // unreachable from user code since identifiers can't contain `__`).
                let param_idx = params.len();
                let synth_name = format!("__dp_{}", param_idx);
                let pattern_span = param_start.merge(self.previous_span());
                let tuple_types: Vec<Spanned<Type>> =
                    bindings.iter().map(|(ty, _, _)| ty.clone()).collect();
                let tuple_ty = Spanned::new(Type::Tuple(tuple_types), pattern_span);
                let meta: Vec<DestructureBinding> = bindings
                    .iter()
                    .map(|(ty, own, n)| DestructureBinding {
                        type_: ty.clone(),
                        ownership: *own,
                        name: n.clone(),
                    })
                    .collect();
                for (binding_idx, (ty, own, n)) in bindings.into_iter().enumerate() {
                    destructure_bindings.push((param_idx, binding_idx, ty, own, n));
                }
                (
                    Some(tuple_ty),
                    Ownership::Borrow,
                    Spanned::new(synth_name, pattern_span),
                    Some(meta),
                )
            } else if self.is_type_start() {
                // type-first: Could be typed parameter `Type name`
                if let Some(result) = self.try_parse(|p| {
                    let ty = p.parse_type().ok()?;
                    let ownership = p.parse_ownership_modifier();
                    matches!(p.peek(), Token::Identifier(_)).then(|| {
                        let n = p.expect_identifier().unwrap(); // safe: just checked
                        (Some(ty), ownership, n, None)
                    })
                }) {
                    result
                } else {
                    // Not a typed param — treat as untyped
                    let n = self.expect_identifier()?;
                    (None, Ownership::Borrow, n, None)
                }
            } else {
                let n = self.expect_identifier()?;
                (None, Ownership::Borrow, n, None)
            };

            let param_end = self.previous_span();
            params.push(Spanned::new(
                ClosureParam {
                    type_,
                    ownership,
                    name,
                    destructure: destructure_meta,
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
            let block = self.parse_block_body(start)?;
            let span = block.span;
            Spanned::new(Expr::Block(block), span)
        } else {
            // Single expression
            self.parse_expr()?
        };

        // If we have destructure bindings, prepend `T name = __dp_<i>.<j>` stmts to body.
        let body = if destructure_bindings.is_empty() {
            body
        } else {
            let body_span = body.span;
            // Normalize body to a Block. For expression-body closures, wrap the expression
            // in a `return` — block-bodied closure lowering requires an explicit terminator
            // (lowered as `Expr::Block` doesn't auto-return the last expression).
            let mut block = match body.node {
                Expr::Block(b) => b,
                other => Block {
                    stmts: vec![Spanned::new(
                        Stmt::Return(Some(Spanned::new(other, body_span))),
                        body_span,
                    )],
                    span: body_span,
                },
            };
            // Build prelude stmts. Iterate in declaration order.
            let mut prelude: Vec<Spanned<Stmt>> = Vec::with_capacity(destructure_bindings.len());
            for (param_idx, binding_idx, ty, own, n) in destructure_bindings {
                let synth_name = format!("__dp_{}", param_idx);
                let span = n.span;
                let object = Spanned::new(Expr::Identifier(synth_name), span);
                let value = Spanned::new(
                    Expr::TupleFieldAccess {
                        object: Box::new(object),
                        index: binding_idx,
                    },
                    span,
                );
                let pattern = Spanned::new(Pattern::Binding(n.node.clone()), span);
                let stmt = Stmt::VarDecl {
                    is_const: false,
                    is_mutable: matches!(own, Ownership::MutableBorrow),
                    shared: SharedKind::None,
                    type_: ty,
                    pattern,
                    value,
                };
                prelude.push(Spanned::new(stmt, span));
            }
            // Prepend prelude before the existing block stmts.
            prelude.extend(block.stmts.drain(..));
            block.stmts = prelude;
            Spanned::new(Expr::Block(block), body_span)
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

    fn parse_optional_comprehension_filter(
        &mut self,
    ) -> Result<Option<Box<Spanned<Expr>>>, ParseError> {
        if self.match_keyword(Keyword::If) {
            Ok(Some(Box::new(self.parse_expr()?)))
        } else {
            Ok(None)
        }
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

        let condition = self.parse_optional_comprehension_filter()?;

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
            return Ok(Spanned::new(
                Expr::DictLiteral(Vec::new()),
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
                let condition = self.parse_optional_comprehension_filter()?;
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

            // Regular dict literal: {k: v, k: v, ...}
            let mut pairs = vec![(first, value)];
            while self.match_token(&Token::Comma) {
                if self.check(&Token::RBrace) {
                    break;
                }
                let k = self.parse_expr()?;
                self.expect(&Token::Colon)?;
                let v = self.parse_expr()?;
                pairs.push((k, v));
            }
            self.expect(&Token::RBrace)?;
            let end = self.previous_span();
            return Ok(Spanned::new(Expr::DictLiteral(pairs), start.merge(end)));
        }

        // Check for set comprehension: {expr for x in iter}
        if self.check_keyword(Keyword::For) {
            self.expect_keyword(Keyword::For)?;
            let variable = self.expect_identifier()?;
            self.expect_keyword(Keyword::In)?;
            let iterable = self.parse_expr()?;
            let condition = self.parse_optional_comprehension_filter()?;
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
        // Reuses ArrayLiteral — semantic analysis distinguishes set vs array by context.
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

        while self.match_elif() {
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

        // Match expression arms accept the same body shape as match-statement
        // arms (`parse_arm_body`): either a single inline expression
        // (`case P: expr`) or an indented block (`case P:\n    stmts...\n    tail`)
        // wrapped as `Expr::Block`. The IR lowering's `lower_block_expr` walks
        // the block's last stmt as the value (or routes a trailing diverging
        // `return`/`throw` through normally), so a multi-statement arm body
        // with an early-exit tail composes correctly with the match's value.
        // Snag #11 (2026-05-06).
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
                let else_start = self.peek_span();
                self.expect(&Token::Colon)?;
                let body = self.parse_arm_body(else_start)?;
                else_arm = Some(Box::new(body));
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

            self.expect(&Token::Colon)?;
            let pattern_span = pattern.span;
            let body = self.parse_arm_body(arm_start)?;
            let arm_end = body.span;

            arms.push(MatchArm {
                pattern,
                guard,
                body,
                span: pattern_span.merge(arm_end),
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
            // Trailing comma: `foo(a, b,)` — stop before `)`
            if self.check(&Token::RParen) {
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
        {
            let n = self.expect_identifier()?;
            self.advance(); // skip =
            Some(n)
        } else {
            None
        };

        let value = {
            let guard = CallArgGuard::new(self);
            let v = guard.parser.parse_expr()?;
            v
        };

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
                | Token::BoolLiteral(_)
                | Token::Identifier(_)
                | Token::LParen
                | Token::LBracket
                | Token::LBrace
                | Token::Minus
                | Token::Star
                | Token::Bang
                | Token::Ampersand
                | Token::Tilde
                | Token::Keyword(Keyword::True)
                | Token::Keyword(Keyword::False)
                | Token::Keyword(Keyword::Not)
                | Token::Keyword(Keyword::If)
                | Token::Keyword(Keyword::Match)
                | Token::Keyword(Keyword::Do)
                | Token::Keyword(Keyword::Await)
                | Token::Keyword(Keyword::Spawn)
                | Token::Keyword(Keyword::Async)
                | Token::Keyword(Keyword::SelfLower)
                | Token::Keyword(Keyword::It)
                | Token::Keyword(Keyword::Move)
                | Token::Keyword(Keyword::Int)
                | Token::Keyword(Keyword::Int8)
                | Token::Keyword(Keyword::Int16)
                | Token::Keyword(Keyword::Int32)
                | Token::Keyword(Keyword::Int64)
                | Token::Keyword(Keyword::Uint)
                | Token::Keyword(Keyword::Uint8)
                | Token::Keyword(Keyword::Uint16)
                | Token::Keyword(Keyword::Uint32)
                | Token::Keyword(Keyword::Uint64)
                | Token::Keyword(Keyword::Float)
                | Token::Keyword(Keyword::Float32)
                | Token::Keyword(Keyword::Float64)
                | Token::Keyword(Keyword::Bool)
                | Token::Keyword(Keyword::StringType)
                | Token::Dot
        )
    }

    /// Check if the current token can start a type.
    pub fn is_type_start(&self) -> bool {
        match self.peek() {
            Token::Keyword(kw) => kw.is_type_keyword(),
            Token::Identifier(_) | Token::LParen => true,
            _ => false,
        }
    }
}
