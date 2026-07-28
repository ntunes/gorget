use crate::lexer::token::{Keyword, Token};
use crate::span::{Span, Spanned};

use super::ast::*;
use super::Parser;
use crate::errors::ParseError;

impl Parser {
    /// Parse a type expression.
    pub fn parse_type(&mut self) -> Result<Spanned<Type>, ParseError> {
        let start = self.peek_span();

        let base = self.parse_base_type()?;
        self.parse_type_postfix(base, start)
    }

    /// Parse a type with optional trailing ownership suffix (`&` or `!`).
    /// Used in generic args and return types — NOT in param parsing
    /// (params use separate `parse_ownership_modifier()` for the `Param.ownership` field).
    pub fn parse_type_with_ownership(&mut self) -> Result<Spanned<Type>, ParseError> {
        let ty = self.parse_type()?;
        let start = ty.span;
        if self.check(&Token::Ampersand) {
            self.advance();
            let end = self.previous_span();
            Ok(Spanned::new(Type::Ref(Box::new(ty)), start.merge(end)))
        } else if self.check(&Token::Bang) {
            self.advance();
            let end = self.previous_span();
            Ok(Spanned::new(Type::Owned(Box::new(ty)), start.merge(end)))
        } else {
            Ok(ty)
        }
    }

    pub(crate) fn parse_base_type(&mut self) -> Result<Spanned<Type>, ParseError> {
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

                // Smart pointer / concurrency types are now regular identifiers.
                // They go through Token::Identifier branch below.

                Err(self.error_unexpected("type"))
            }

            // Named type (user-defined): Vector, HashMap, etc.
            // `cstr` is a contextual type — only valid inside extern "C" contexts.
            Token::Identifier(ref name) if name == "cstr" => {
                if self.in_extern_c {
                    self.advance();
                    let end = self.previous_span();
                    Ok(Spanned::new(Type::Primitive(PrimitiveType::CStr), start.merge(end)))
                } else {
                    Err(self.error_unexpected("type (`cstr` is only valid inside `extern \"C\"` declarations)"))
                }
            }
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
                generic_args.push(self.parse_type_with_ownership()?);
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

    pub(crate) fn parse_type_postfix(
        &mut self,
        base: Spanned<Type>,
        start: Span,
    ) -> Result<Spanned<Type>, ParseError> {
        // T* — pointer type, only valid inside extern "C" context.
        // Means "pass as const T* in C" (take address of struct value).
        if self.check(&Token::Star) {
            if self.in_extern_c {
                self.advance();
                let end = self.previous_span();
                return Ok(Spanned::new(
                    Type::Pointer(Box::new(base)),
                    start.merge(end),
                ));
            } else {
                // * outside extern "C" — don't consume, let it be parsed as multiplication
            }
        }

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

        // Check for function type suffix: int(int, int) or int(MyStruct &, int)
        // A type followed by ( is a function type.
        //
        // D35 (docs/define-gorget/decisions.md, ratified 2026-07-26): an
        // unnamed parameter's sigil goes AFTER the type — `Callable[void(int &)]`
        // — mirroring the named-parameter rule (`void modify(Message &msg)`).
        // The pre-D35 spelling with the sigil BEFORE the type is REJECTED with
        // a diagnostic naming the replacement; retirement is a hard break, per
        // D35's ratification.
        if self.check(&Token::LParen) && self.is_function_type_context(&base.node) {
            self.advance(); // (
            let mut params = Vec::new();
            let mut param_ownerships = Vec::new();
            while !self.check(&Token::RParen) && !self.at_end() {
                // Pre-D35 spelling: sigil BEFORE the type. Return `Err` with a
                // diagnostic that names the replacement — non-speculative parses
                // (function-def return type, generic arg, top-level decl type)
                // propagate it to the user. Speculative parses (`try_parse` in
                // `parse_decl_or_expr_stmt`) map `Err` to `None` and backtrack,
                // which is correct — we should not push to `self.errors` here
                // because that error would then leak into a fallback expression
                // parse. Retirement is a hard break, per D35's ratification.
                if self.check(&Token::Ampersand) || self.check(&Token::Bang) {
                    let sigil = if self.check(&Token::Ampersand) { '&' } else { '!' };
                    let span = self.peek_span();
                    return Err(ParseError {
                        kind: crate::errors::ParseErrorKind::FunctionTypeParamSigilBeforeType { sigil },
                        span,
                    });
                }
                let param_ty = self.parse_type()?;
                // D35: sigil AFTER the type — routed through
                // `parse_ownership_modifier` (D32's whitelist mechanism).
                let ownership = self.parse_ownership_modifier();
                params.push(param_ty);
                param_ownerships.push(ownership);
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
                    param_ownerships,
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
        // This is safe because parse_type() is only called in type positions,
        // never in expression positions where ( would be a call.
        // Named types are needed for generic callable params: Callable[T(int)]
        matches!(base, Type::Primitive(_) | Type::Named { .. })
    }
}
