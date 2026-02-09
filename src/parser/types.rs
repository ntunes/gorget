use crate::lexer::token::{Keyword, Token};
use crate::span::{Span, Spanned};

use super::ast::*;
use super::Parser;
use crate::errors::ParseError;

impl Parser {
    /// Parse a type expression.
    pub fn parse_type(&mut self) -> Result<Spanned<Type>, ParseError> {
        let start = self.peek_span();

        // Handle prefix modifiers
        if self.match_keyword(Keyword::Dynamic) {
            let trait_ = self.parse_type()?;
            let end = self.previous_span();
            return Ok(Spanned::new(
                Type::Dynamic {
                    trait_: Box::new(trait_),
                },
                start.merge(end),
            ));
        }

        let base = self.parse_base_type()?;
        self.parse_type_postfix(base, start)
    }

    fn parse_base_type(&mut self) -> Result<Spanned<Type>, ParseError> {
        let start = self.peek_span();

        match self.peek().clone() {
            // Primitive types
            Token::Keyword(kw) => {
                let prim = match kw {
                    Keyword::Int => Some(PrimitiveType::Int),
                    Keyword::Int8 => Some(PrimitiveType::Int8),
                    Keyword::Int16 => Some(PrimitiveType::Int16),
                    Keyword::Int32 => Some(PrimitiveType::Int32),
                    Keyword::Int64 => Some(PrimitiveType::Int64),
                    Keyword::Uint => Some(PrimitiveType::Uint),
                    Keyword::Uint8 => Some(PrimitiveType::Uint8),
                    Keyword::Uint16 => Some(PrimitiveType::Uint16),
                    Keyword::Uint32 => Some(PrimitiveType::Uint32),
                    Keyword::Uint64 => Some(PrimitiveType::Uint64),
                    Keyword::Float => Some(PrimitiveType::Float),
                    Keyword::Float32 => Some(PrimitiveType::Float32),
                    Keyword::Float64 => Some(PrimitiveType::Float64),
                    Keyword::Bool => Some(PrimitiveType::Bool),
                    Keyword::Char => Some(PrimitiveType::Char),
                    Keyword::Str => Some(PrimitiveType::Str),
                    Keyword::StringType => Some(PrimitiveType::StringType),
                    Keyword::Void => Some(PrimitiveType::Void),
                    _ => None,
                };

                if let Some(p) = prim {
                    self.advance();
                    let end = self.previous_span();
                    return Ok(Spanned::new(Type::Primitive(p), start.merge(end)));
                }

                // Self type
                if kw == Keyword::SelfUpper {
                    self.advance();
                    let end = self.previous_span();
                    return Ok(Spanned::new(Type::SelfType, start.merge(end)));
                }

                // auto
                if kw == Keyword::Auto {
                    self.advance();
                    let end = self.previous_span();
                    return Ok(Spanned::new(Type::Inferred, start.merge(end)));
                }

                // Smart pointer types — parse as named types
                if matches!(
                    kw,
                    Keyword::Box
                        | Keyword::Rc
                        | Keyword::Arc
                        | Keyword::Weak
                        | Keyword::Cell
                        | Keyword::RefCell
                        | Keyword::Mutex
                        | Keyword::RwLock
                ) {
                    return self.parse_named_type();
                }

                Err(self.error_unexpected("type"))
            }

            // Named type (user-defined): Vector, HashMap, etc.
            Token::Identifier(_) => self.parse_named_type(),

            // Tuple type: (int, String)
            Token::LParen => self.parse_tuple_type(),

            _ => Err(self.error_unexpected("type")),
        }
    }

    fn parse_named_type(&mut self) -> Result<Spanned<Type>, ParseError> {
        let start = self.peek_span();
        let name = self.expect_name()?;

        let mut generic_args = Vec::new();
        if self.match_token(&Token::LBracket) {
            while !self.check(&Token::RBracket) && !self.at_end() {
                generic_args.push(self.parse_type()?);
                if !self.check(&Token::RBracket) {
                    self.expect(&Token::Comma)?;
                }
            }
            self.expect(&Token::RBracket)?;
        }

        let end = self.previous_span();
        Ok(Spanned::new(
            Type::Named { name, generic_args },
            start.merge(end),
        ))
    }

    fn parse_tuple_type(&mut self) -> Result<Spanned<Type>, ParseError> {
        let start = self.peek_span();
        self.expect(&Token::LParen)?;

        let mut types = Vec::new();
        while !self.check(&Token::RParen) && !self.at_end() {
            types.push(self.parse_type()?);
            if !self.check(&Token::RParen) {
                self.expect(&Token::Comma)?;
            }
        }
        self.expect(&Token::RParen)?;
        let end = self.previous_span();

        Ok(Spanned::new(Type::Tuple(types), start.merge(end)))
    }

    fn parse_type_postfix(
        &mut self,
        base: Spanned<Type>,
        start: Span,
    ) -> Result<Spanned<Type>, ParseError> {
        // Check for array/slice suffix: int[5] or int[]
        // Only applies to primitive types — for named types, [] was already parsed as generics
        if matches!(base.node, Type::Primitive(_)) && self.check(&Token::LBracket) {
            self.advance(); // [
            // Check for empty brackets → slice type
            if self.check(&Token::RBracket) {
                self.advance(); // ]
                let end = self.previous_span();
                return Ok(Spanned::new(
                    Type::Slice {
                        element: Box::new(base),
                    },
                    start.merge(end),
                ));
            }
            let size = self.parse_expr()?;
            self.expect(&Token::RBracket)?;
            let end = self.previous_span();
            return Ok(Spanned::new(
                Type::Array {
                    element: Box::new(base),
                    size: Box::new(size),
                },
                start.merge(end),
            ));
        }

        // Check for function type suffix: int(int, int)
        // A type followed by ( is a function type
        if self.check(&Token::LParen) && self.is_function_type_context(&base.node) {
            self.advance(); // (
            let mut params = Vec::new();
            while !self.check(&Token::RParen) && !self.at_end() {
                params.push(self.parse_type()?);
                if !self.check(&Token::RParen) {
                    self.expect(&Token::Comma)?;
                }
            }
            self.expect(&Token::RParen)?;
            let end = self.previous_span();
            return Ok(Spanned::new(
                Type::Function {
                    return_type: Box::new(base),
                    params,
                },
                start.merge(end),
            ));
        }

        Ok(base)
    }

    /// Check if a base type followed by ( should be parsed as a function type.
    /// This is true only when we're in a type position (parameter type, return type, etc.),
    /// not in expression position where ( would be a call.
    fn is_function_type_context(&self, base: &Type) -> bool {
        // Function types use return_type(param_types)
        // We only treat this as function type for primitive return types
        // or if explicitly in a type context (handled by the caller)
        matches!(base, Type::Primitive(_))
    }
}
