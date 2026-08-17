use crate::lexer::token::{Keyword, Token};
use crate::span::{Span, Spanned};

use super::ast::*;
use super::Parser;
use crate::errors::ParseError;

impl Parser {
    /// Parse a bare (possibly comma-separated) pattern.
    /// Used in `for` loops: `for x, y in items:` → `Pattern::Tuple([x, y])`.
    /// Terminates naturally when the next token is `in` or another non-pattern token.
    pub fn parse_bare_pattern(&mut self) -> Result<Spanned<Pattern>, ParseError> {
        let first = self.parse_pattern()?;
        if self.check(&Token::Comma) {
            let start = first.span;
            let mut pats = vec![first];
            while self.match_token(&Token::Comma) {
                pats.push(self.parse_pattern()?);
            }
            let end = pats.last().unwrap().span;
            Ok(Spanned::new(Pattern::Tuple(pats), start.merge(end)))
        } else {
            Ok(first)
        }
    }

    /// Parse a pattern (for match arms, destructuring, is-expressions).
    pub fn parse_pattern(&mut self) -> Result<Spanned<Pattern>, ParseError> {
        let mut pattern = self.parse_single_pattern()?;

        // Check for or-pattern: pat1 | pat2 | pat3
        if self.check(&Token::Pipe) {
            let mut alternatives = vec![pattern];
            while self.match_token(&Token::Pipe) {
                alternatives.push(self.parse_single_pattern()?);
            }
            let start = alternatives.first().unwrap().span;
            let end = alternatives.last().unwrap().span;
            pattern = Spanned::new(Pattern::Or(alternatives), start.merge(end));
        }

        Ok(pattern)
    }

    fn parse_single_pattern(&mut self) -> Result<Spanned<Pattern>, ParseError> {
        let start = self.peek_span();

        match self.peek().clone() {
            // Wildcard
            Token::Underscore => {
                self.advance();
                Ok(Spanned::new(Pattern::Wildcard, start))
            }

            // Rest pattern
            Token::DotDot => {
                self.advance();
                Ok(Spanned::new(Pattern::Rest, start))
            }

            // Negative literal
            Token::Minus => {
                self.advance();
                let inner = self.parse_prefix_for_pattern()?;
                let end = inner.span;
                Ok(Spanned::new(
                    Pattern::Literal(Box::new(Spanned::new(
                        Expr::UnaryOp {
                            op: UnaryOp::Neg,
                            operand: Box::new(inner),
                        },
                        start.merge(end),
                    ))),
                    start.merge(end),
                ))
            }

            // Integer literal
            Token::IntLiteral(n) => {
                self.advance();
                Ok(Spanned::new(
                    Pattern::Literal(Box::new(Spanned::new(Expr::IntLiteral(n), start))),
                    start,
                ))
            }

            // Float literal
            Token::FloatLiteral(n) => {
                self.advance();
                Ok(Spanned::new(
                    Pattern::Literal(Box::new(Spanned::new(Expr::FloatLiteral(n), start))),
                    start,
                ))
            }

            // String literal
            Token::StringLiteral(s) => {
                self.advance();
                Ok(Spanned::new(
                    Pattern::Literal(Box::new(Spanned::new(Expr::StringLiteral(s, Vec::new()), start))),
                    start,
                ))
            }

            // Boolean literals
            Token::Keyword(Keyword::True) => {
                self.advance();
                Ok(Spanned::new(
                    Pattern::Literal(Box::new(Spanned::new(Expr::BoolLiteral(true), start))),
                    start,
                ))
            }
            Token::Keyword(Keyword::False) => {
                self.advance();
                Ok(Spanned::new(
                    Pattern::Literal(Box::new(Spanned::new(Expr::BoolLiteral(false), start))),
                    start,
                ))
            }

            // Type-name keywords used as patterns in `T is <type>` expressions.
            // These are lexed as keywords (not identifiers) so need explicit handling.
            Token::Keyword(kw) if kw.is_type_keyword() => {
                let name_str = kw.as_name().to_string();
                self.advance();
                Ok(Spanned::new(Pattern::Binding(name_str), start))
            }

            Token::Identifier(_) => {
                let name = self.expect_identifier()?;
                // `None` is a prelude variant — emit the same
                // `Pattern::Literal(NoneLiteral)` shape the old
                // keyword-based branch produced, so downstream
                // pattern resolution / IR lowering doesn't have to
                // change.
                if name.node == "None" && !self.check(&Token::LParen) {
                    return Ok(Spanned::new(
                        Pattern::Literal(Box::new(Spanned::new(Expr::NoneLiteral, name.span))),
                        name.span,
                    ));
                }
                self.parse_constructor_or_binding(name, start)
            }

            // Tuple pattern: (a, b, c)
            Token::LParen => {
                self.advance();
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
            }

            // Dot-shorthand pattern: .Red() or .Blue(n)
            Token::Dot => {
                self.advance();
                let variant = self.expect_identifier()?;
                if self.match_token(&Token::LParen) {
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
                        Pattern::DotShorthand { variant, fields, paren_spelled: true },
                        start.merge(end),
                    ))
                } else {
                    let end = variant.span;
                    Ok(Spanned::new(
                        Pattern::DotShorthand {
                            variant,
                            fields: Vec::new(),
                            paren_spelled: false,
                        },
                        start.merge(end),
                    ))
                }
            }

            _ => Err(self.error_unexpected("pattern")),
        }
    }

    fn parse_constructor_or_binding(
        &mut self,
        name: Spanned<String>,
        start: Span,
    ) -> Result<Spanned<Pattern>, ParseError> {
        // Build a potentially qualified path: Name.Variant
        let mut path = vec![name];
        while self.match_token(&Token::Dot) {
            path.push(self.expect_identifier()?);
        }

        // Check for constructor pattern: Name(fields)
        if self.match_token(&Token::LParen) {
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
                Pattern::Constructor { path, fields, paren_spelled: true },
                start.merge(end),
            ))
        } else if path.len() == 1 {
            // Single identifier — binding or unit variant
            let name = path.into_iter().next().expect("len was 1");
            // Uppercase-starting = unit variant, lowercase = binding
            // But we'll let semantic analysis handle this distinction
            Ok(Spanned::new(Pattern::Binding(name.node), name.span))
        } else {
            // Qualified path without parens — unit variant: List.Nil
            let end = path.last().unwrap().span;
            Ok(Spanned::new(
                Pattern::Constructor {
                    path,
                    fields: Vec::new(),
                    paren_spelled: false,
                },
                start.merge(end),
            ))
        }
    }

    /// Parse a prefix expression that can appear in a pattern (for negative literals).
    fn parse_prefix_for_pattern(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.peek_span();
        match self.peek().clone() {
            Token::IntLiteral(n) => {
                self.advance();
                Ok(Spanned::new(Expr::IntLiteral(n), start))
            }
            Token::FloatLiteral(n) => {
                self.advance();
                Ok(Spanned::new(Expr::FloatLiteral(n), start))
            }
            _ => Err(self.error_unexpected("numeric literal")),
        }
    }
}
