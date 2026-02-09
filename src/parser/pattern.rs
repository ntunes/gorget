use crate::lexer::token::{Keyword, Token};
use crate::span::{Span, Spanned};

use super::ast::*;
use super::Parser;
use crate::errors::ParseError;

impl Parser {
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
                    Pattern::Literal(Box::new(Spanned::new(Expr::StringLiteral(s), start))),
                    start,
                ))
            }

            // Char literal
            Token::CharLiteral(c) => {
                self.advance();
                Ok(Spanned::new(
                    Pattern::Literal(Box::new(Spanned::new(Expr::CharLiteral(c), start))),
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

            // None
            Token::Keyword(Keyword::None) => {
                self.advance();
                Ok(Spanned::new(
                    Pattern::Literal(Box::new(Spanned::new(Expr::NoneLiteral, start))),
                    start,
                ))
            }

            // Named patterns: Some, Ok, Error, or identifiers
            Token::Keyword(Keyword::Some | Keyword::Ok | Keyword::Error) => {
                let name_str = format!("{}", self.peek()).trim_matches('\'').to_string();
                self.advance();
                self.parse_constructor_or_binding(Spanned::new(name_str, start), start)
            }

            Token::Identifier(_) => {
                let name = self.expect_identifier()?;
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
                Pattern::Constructor { path, fields },
                start.merge(end),
            ))
        } else if path.len() == 1 {
            // Single identifier — binding or unit variant
            let name = path.into_iter().next().unwrap();
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
