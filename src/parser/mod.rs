pub mod ast;
pub mod expr;
pub mod pattern;
pub mod stmt;
pub mod types;

use crate::errors::ParseError;
use crate::lexer::token::{Keyword, Token};
use crate::lexer::Lexer;
use crate::span::{Span, Spanned};
use ast::*;
/// Recursive descent parser for Vyper source code.
pub struct Parser {
    tokens: Vec<Spanned<Token>>,
    pos: usize,
    pub errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(source: &str) -> Self {
        let lexer = Lexer::new(source);
        let mut tokens: Vec<Spanned<Token>> = lexer.collect();
        // Ensure we always have an EOF sentinel
        let eof_pos = tokens.last().map(|t| t.span.end).unwrap_or(0);
        tokens.push(Spanned::new(Token::Eof, Span::new(eof_pos, eof_pos)));
        Self {
            tokens,
            pos: 0,
            errors: Vec::new(),
        }
    }

    // ── Token Management ──────────────────────────────────────

    pub fn peek(&self) -> &Token {
        self.tokens
            .get(self.pos)
            .map(|s| &s.node)
            .unwrap_or(&Token::Eof)
    }

    pub fn peek_span(&self) -> Span {
        self.tokens
            .get(self.pos)
            .map(|s| s.span)
            .unwrap_or(Span::dummy())
    }

    /// Look ahead n tokens (0 = current).
    pub fn peek_ahead(&self, n: usize) -> &Token {
        self.tokens
            .get(self.pos + n)
            .map(|s| &s.node)
            .unwrap_or(&Token::Eof)
    }

    pub fn advance(&mut self) -> Spanned<Token> {
        let tok = self.tokens.get(self.pos).cloned().unwrap_or(Spanned::new(
            Token::Eof,
            Span::dummy(),
        ));
        self.pos += 1;
        tok
    }

    pub fn check(&self, token: &Token) -> bool {
        self.peek() == token
    }

    pub fn check_keyword(&self, kw: Keyword) -> bool {
        matches!(self.peek(), Token::Keyword(k) if *k == kw)
    }

    pub fn match_token(&mut self, token: &Token) -> bool {
        if self.check(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn match_keyword(&mut self, kw: Keyword) -> bool {
        if self.check_keyword(kw) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn expect(&mut self, token: &Token) -> Result<Spanned<Token>, ParseError> {
        if self.check(token) {
            Ok(self.advance())
        } else {
            Err(self.error_unexpected(&format!("{token}")))
        }
    }

    pub fn expect_keyword(&mut self, kw: Keyword) -> Result<Spanned<Token>, ParseError> {
        if self.check_keyword(kw) {
            Ok(self.advance())
        } else {
            Err(self.error_unexpected(&format!("{kw}")))
        }
    }

    pub fn expect_identifier(&mut self) -> Result<Spanned<String>, ParseError> {
        match self.peek().clone() {
            Token::Identifier(name) => {
                let span = self.peek_span();
                self.advance();
                Ok(Spanned::new(name, span))
            }
            _ => Err(self.error_unexpected("identifier")),
        }
    }

    /// Expect an identifier, but also accept keywords that can be used as names
    /// in certain positions (e.g., field names).
    pub fn expect_name(&mut self) -> Result<Spanned<String>, ParseError> {
        match self.peek().clone() {
            Token::Identifier(name) => {
                let span = self.peek_span();
                self.advance();
                Ok(Spanned::new(name, span))
            }
            Token::Keyword(kw) => {
                let span = self.peek_span();
                self.advance();
                Ok(Spanned::new(format!("{kw}").trim_matches('\'').to_string(), span))
            }
            _ => Err(self.error_unexpected("identifier")),
        }
    }

    pub fn at_end(&self) -> bool {
        matches!(self.peek(), Token::Eof)
    }

    pub fn previous_span(&self) -> Span {
        if self.pos > 0 {
            self.tokens[self.pos - 1].span
        } else {
            Span::dummy()
        }
    }

    // ── Error Handling ────────────────────────────────────────

    pub fn error_unexpected(&self, expected: &str) -> ParseError {
        ParseError {
            kind: crate::errors::ParseErrorKind::UnexpectedToken {
                expected: expected.to_string(),
                got: format!("{}", self.peek()),
            },
            span: self.peek_span(),
        }
    }

    pub fn error_at(&self, span: Span, msg: &str) -> ParseError {
        ParseError {
            kind: crate::errors::ParseErrorKind::UnexpectedToken {
                expected: msg.to_string(),
                got: String::new(),
            },
            span,
        }
    }

    /// Skip tokens until we find a synchronization point.
    pub fn synchronize(&mut self) {
        loop {
            match self.peek() {
                Token::Newline => {
                    self.advance();
                    return;
                }
                Token::Dedent | Token::Eof => return,
                Token::Keyword(
                    Keyword::If
                    | Keyword::For
                    | Keyword::While
                    | Keyword::Return
                    | Keyword::Match
                    | Keyword::Struct
                    | Keyword::Enum
                    | Keyword::Trait
                    | Keyword::Implement
                    | Keyword::Import
                    | Keyword::From,
                ) => return,
                _ => {
                    self.advance();
                }
            }
        }
    }

    // ── Block Parsing ─────────────────────────────────────────

    /// Parse a block: COLON NEWLINE INDENT stmts DEDENT
    pub fn parse_block(&mut self) -> Result<Block, ParseError> {
        let start = self.peek_span();
        self.expect(&Token::Colon)?;
        self.expect(&Token::Newline)?;
        self.expect(&Token::Indent)?;

        let mut stmts = Vec::new();
        while !self.check(&Token::Dedent) && !self.at_end() {
            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => {
                    self.errors.push(e);
                    let pos_before = self.pos;
                    self.synchronize();
                    if self.pos == pos_before {
                        self.advance();
                    }
                }
            }
        }

        self.expect(&Token::Dedent)?;
        let end = self.previous_span();

        Ok(Block {
            stmts,
            span: start.merge(end),
        })
    }

    /// Consume a newline if present (used after statements).
    pub fn consume_newline(&mut self) {
        self.match_token(&Token::Newline);
    }

    // ── Top-Level Parsing ─────────────────────────────────────

    /// Parse a complete module (top-level items).
    pub fn parse_module(&mut self) -> Module {
        let start = self.peek_span();
        let mut items = Vec::new();

        while !self.at_end() {

            // Skip stray newlines at top level
            if self.check(&Token::Newline) {
                self.advance();
                continue;
            }

            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(e) => {
                    self.errors.push(e);
                    let pos_before = self.pos;
                    self.synchronize();
                    // Guarantee forward progress to prevent infinite loops
                    if self.pos == pos_before {
                        self.advance();
                    }
                }
            }
        }
        let end = self.previous_span();
        Module {
            items,
            span: start.merge(end),
        }
    }

    /// Parse a top-level item.
    pub fn parse_item(&mut self) -> Result<Spanned<Item>, ParseError> {
        let start = self.peek_span();

        // Collect doc comments
        let mut doc_comment = None;
        while matches!(self.peek(), Token::DocComment(_)) {
            if let Token::DocComment(comment) = self.advance().node {
                let dc = doc_comment.get_or_insert_with(String::new);
                if !dc.is_empty() {
                    dc.push('\n');
                }
                dc.push_str(&comment);
            }
        }

        // Collect attributes
        let mut attributes = Vec::new();
        while self.check(&Token::At) {
            attributes.push(self.parse_attribute()?);
        }

        // Parse visibility
        let visibility = if self.match_keyword(Keyword::Public) {
            Visibility::Public
        } else {
            Visibility::Private
        };

        // Determine item kind
        match self.peek() {
            Token::Keyword(Keyword::Struct) => {
                let def = self.parse_struct_def(attributes, visibility, doc_comment)?;
                let span = start.merge(def.span);
                Ok(Spanned::new(Item::Struct(def), span))
            }
            Token::Keyword(Keyword::Enum) => {
                let def = self.parse_enum_def(attributes, visibility, doc_comment)?;
                let span = start.merge(def.span);
                Ok(Spanned::new(Item::Enum(def), span))
            }
            Token::Keyword(Keyword::Trait) => {
                let def = self.parse_trait_def(attributes, visibility, doc_comment)?;
                let span = start.merge(def.span);
                Ok(Spanned::new(Item::Trait(def), span))
            }
            Token::Keyword(Keyword::Implement) => {
                let block = self.parse_impl_block()?;
                let span = start.merge(block.span);
                Ok(Spanned::new(Item::Implement(block), span))
            }
            Token::Keyword(Keyword::Import) => {
                let stmt = self.parse_import()?;
                let span = start.merge(match &stmt {
                    ImportStmt::Simple { span, .. }
                    | ImportStmt::Grouped { span, .. }
                    | ImportStmt::From { span, .. } => *span,
                });
                Ok(Spanned::new(Item::Import(stmt), span))
            }
            Token::Keyword(Keyword::From) => {
                let stmt = self.parse_from_import()?;
                let span = start.merge(match &stmt {
                    ImportStmt::Simple { span, .. }
                    | ImportStmt::Grouped { span, .. }
                    | ImportStmt::From { span, .. } => *span,
                });
                Ok(Spanned::new(Item::Import(stmt), span))
            }
            Token::Keyword(Keyword::Type) => {
                let alias = self.parse_type_alias()?;
                let span = start.merge(alias.span);
                Ok(Spanned::new(Item::TypeAlias(alias), span))
            }
            Token::Keyword(Keyword::Newtype) => {
                let nt = self.parse_newtype()?;
                let span = start.merge(nt.span);
                Ok(Spanned::new(Item::Newtype(nt), span))
            }
            Token::Keyword(Keyword::Extern) => {
                let ext = self.parse_extern_block()?;
                let span = start.merge(ext.span);
                Ok(Spanned::new(Item::ExternBlock(ext), span))
            }
            Token::Keyword(Keyword::Static) => {
                let decl = self.parse_static_decl(visibility)?;
                let span = start.merge(decl.span);
                Ok(Spanned::new(Item::StaticDecl(decl), span))
            }
            Token::Keyword(Keyword::Const) => {
                // Could be const function or const declaration
                // Peek ahead: if after const we see a type followed by identifier( → function
                // If after const we see a type followed by identifier = → const decl
                let decl = self.parse_const_item(attributes, visibility, doc_comment)?;
                let span = start.merge(match &decl {
                    Item::ConstDecl(d) => d.span,
                    Item::Function(f) => f.span,
                    _ => unreachable!(),
                });
                Ok(Spanned::new(decl, span))
            }
            // Function definition (starts with return type or qualifiers)
            _ => {
                let func = self.parse_function_def(attributes, visibility, doc_comment)?;
                let span = start.merge(func.span);
                Ok(Spanned::new(Item::Function(func), span))
            }
        }
    }

    // ── Attributes ────────────────────────────────────────────

    fn parse_attribute(&mut self) -> Result<Spanned<Attribute>, ParseError> {
        let start = self.peek_span();
        self.expect(&Token::At)?;
        let name = self.expect_identifier()?;

        let mut args = Vec::new();
        if self.match_token(&Token::LParen) {
            while !self.check(&Token::RParen) && !self.at_end() {
                match self.peek() {
                    Token::Identifier(_) => {
                        let ident = self.expect_identifier()?;
                        if self.match_token(&Token::Eq) {
                            // key = "value"
                            if let Token::StringLiteral(s) = self.peek().clone() {
                                self.advance();
                                let val = s
                                    .segments
                                    .iter()
                                    .filter_map(|seg| {
                                        if let crate::lexer::token::StringSegment::Literal(l) = seg
                                        {
                                            Some(l.as_str())
                                        } else {
                                            None
                                        }
                                    })
                                    .collect();
                                args.push(AttributeArg::KeyValue(ident.node, val));
                            } else {
                                let val_ident = self.expect_identifier()?;
                                args.push(AttributeArg::KeyValue(ident.node, val_ident.node));
                            }
                        } else {
                            args.push(AttributeArg::Identifier(ident.node));
                        }
                    }
                    Token::StringLiteral(_) => {
                        if let Token::StringLiteral(s) = self.advance().node {
                            let val: String = s
                                .segments
                                .iter()
                                .filter_map(|seg| {
                                    if let crate::lexer::token::StringSegment::Literal(l) = seg {
                                        Some(l.as_str())
                                    } else {
                                        None
                                    }
                                })
                                .collect();
                            args.push(AttributeArg::StringLiteral(val));
                        }
                    }
                    _ => {
                        return Err(self.error_unexpected("attribute argument"));
                    }
                }
                if !self.check(&Token::RParen) {
                    self.expect(&Token::Comma)?;
                }
            }
            self.expect(&Token::RParen)?;
        }

        let end = self.previous_span();
        self.consume_newline();

        Ok(Spanned::new(Attribute { name, args }, start.merge(end)))
    }

    // ── Struct ────────────────────────────────────────────────

    fn parse_struct_def(
        &mut self,
        attributes: Vec<Spanned<Attribute>>,
        visibility: Visibility,
        doc_comment: Option<String>,
    ) -> Result<StructDef, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Struct)?;
        let name = self.expect_identifier()?;

        let generic_params = if self.check(&Token::LBracket) {
            Some(self.parse_generic_params()?)
        } else {
            None
        };

        self.expect(&Token::Colon)?;
        self.expect(&Token::Newline)?;
        self.expect(&Token::Indent)?;

        let mut fields = Vec::new();
        while !self.check(&Token::Dedent) && !self.at_end() {
            let field_start = self.peek_span();
            let field_vis = if self.match_keyword(Keyword::Public) {
                Visibility::Public
            } else {
                Visibility::Private
            };
            let type_ = self.parse_type()?;
            let field_name = self.expect_identifier()?;
            let field_end = self.previous_span();
            self.consume_newline();

            fields.push(Spanned::new(
                FieldDef {
                    visibility: field_vis,
                    type_,
                    name: field_name,
                },
                field_start.merge(field_end),
            ));
        }

        self.expect(&Token::Dedent)?;
        let end = self.previous_span();

        Ok(StructDef {
            attributes,
            visibility,
            name,
            generic_params,
            fields,
            doc_comment,
            span: start.merge(end),
        })
    }

    // ── Enum ──────────────────────────────────────────────────

    fn parse_enum_def(
        &mut self,
        attributes: Vec<Spanned<Attribute>>,
        visibility: Visibility,
        doc_comment: Option<String>,
    ) -> Result<EnumDef, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Enum)?;
        let name = self.expect_identifier()?;

        let generic_params = if self.check(&Token::LBracket) {
            Some(self.parse_generic_params()?)
        } else {
            None
        };

        self.expect(&Token::Colon)?;
        self.expect(&Token::Newline)?;
        self.expect(&Token::Indent)?;

        let mut variants = Vec::new();
        while !self.check(&Token::Dedent) && !self.at_end() {
            let var_start = self.peek_span();
            let var_name = self.expect_name()?;

            let fields = if self.match_token(&Token::LParen) {
                let mut types = Vec::new();
                while !self.check(&Token::RParen) && !self.at_end() {
                    types.push(self.parse_type()?);
                    if !self.check(&Token::RParen) {
                        self.expect(&Token::Comma)?;
                    }
                }
                self.expect(&Token::RParen)?;
                VariantFields::Tuple(types)
            } else {
                VariantFields::Unit
            };

            let var_end = self.previous_span();
            self.consume_newline();

            variants.push(Spanned::new(
                Variant {
                    name: var_name,
                    fields,
                },
                var_start.merge(var_end),
            ));
        }

        self.expect(&Token::Dedent)?;
        let end = self.previous_span();

        Ok(EnumDef {
            attributes,
            visibility,
            name,
            generic_params,
            variants,
            doc_comment,
            span: start.merge(end),
        })
    }

    // ── Trait ─────────────────────────────────────────────────

    fn parse_trait_def(
        &mut self,
        attributes: Vec<Spanned<Attribute>>,
        visibility: Visibility,
        doc_comment: Option<String>,
    ) -> Result<TraitDef, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Trait)?;
        let name = self.expect_identifier()?;

        let generic_params = if self.check(&Token::LBracket) {
            Some(self.parse_generic_params()?)
        } else {
            None
        };

        let extends = if self.match_keyword(Keyword::Extends) {
            self.parse_trait_bound_list()?
        } else {
            Vec::new()
        };

        self.expect(&Token::Colon)?;
        self.expect(&Token::Newline)?;
        self.expect(&Token::Indent)?;

        let mut items = Vec::new();
        while !self.check(&Token::Dedent) && !self.at_end() {
            // Skip doc comments within trait body
            let mut method_doc = None;
            while matches!(self.peek(), Token::DocComment(_)) {
                if let Token::DocComment(comment) = self.advance().node {
                    let dc = method_doc.get_or_insert_with(String::new);
                    if !dc.is_empty() {
                        dc.push('\n');
                    }
                    dc.push_str(&comment);
                }
            }

            if self.check(&Token::Newline) {
                self.advance();
                continue;
            }

            // Associated type: type Item
            if self.check_keyword(Keyword::Type) {
                let assoc = self.parse_associated_type()?;
                items.push(Spanned::new(TraitItem::AssociatedType(assoc.node), assoc.span));
            } else {
                // Method
                let func = self.parse_function_def(Vec::new(), Visibility::Private, method_doc)?;
                let span = func.span;
                items.push(Spanned::new(TraitItem::Method(func), span));
            }
        }

        self.expect(&Token::Dedent)?;
        let end = self.previous_span();

        Ok(TraitDef {
            attributes,
            visibility,
            name,
            generic_params,
            extends,
            items,
            doc_comment,
            span: start.merge(end),
        })
    }

    fn parse_associated_type(&mut self) -> Result<Spanned<AssociatedTypeDef>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Type)?;
        let name = self.expect_identifier()?;

        let default = if self.match_token(&Token::Eq) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.consume_newline();
        let end = self.previous_span();

        Ok(Spanned::new(
            AssociatedTypeDef {
                name,
                bounds: Vec::new(),
                default,
                span: start.merge(end),
            },
            start.merge(end),
        ))
    }

    // ── Implement Block ───────────────────────────────────────

    fn parse_impl_block(&mut self) -> Result<ImplBlock, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Implement)?;

        let generic_params = if self.check(&Token::LBracket) {
            Some(self.parse_generic_params()?)
        } else {
            None
        };

        // Parse first type (could be trait name or self type)
        let first_type = self.parse_type()?;

        // Check for "for" to determine if this is a trait impl
        let (trait_, type_) = if self.match_keyword(Keyword::For) {
            let self_type = self.parse_type()?;
            let trait_span = first_type.span;
            (
                Some(ImplTrait {
                    trait_name: first_type,
                    span: trait_span,
                }),
                self_type,
            )
        } else {
            (None, first_type)
        };

        let where_clause = if self.check_keyword(Keyword::Where) {
            Some(self.parse_where_clause()?)
        } else {
            None
        };

        self.expect(&Token::Colon)?;
        self.expect(&Token::Newline)?;
        self.expect(&Token::Indent)?;

        let mut items = Vec::new();
        while !self.check(&Token::Dedent) && !self.at_end() {
            let mut method_doc = None;
            while matches!(self.peek(), Token::DocComment(_)) {
                if let Token::DocComment(comment) = self.advance().node {
                    let dc = method_doc.get_or_insert_with(String::new);
                    if !dc.is_empty() {
                        dc.push('\n');
                    }
                    dc.push_str(&comment);
                }
            }

            if self.check(&Token::Newline) {
                self.advance();
                continue;
            }

            // Collect attributes for methods
            let mut attrs = Vec::new();
            while self.check(&Token::At) {
                attrs.push(self.parse_attribute()?);
            }

            let vis = if self.match_keyword(Keyword::Public) {
                Visibility::Public
            } else {
                Visibility::Private
            };

            let func = self.parse_function_def(attrs, vis, method_doc)?;
            let span = func.span;
            items.push(Spanned::new(func, span));
        }

        self.expect(&Token::Dedent)?;
        let end = self.previous_span();

        Ok(ImplBlock {
            generic_params,
            trait_,
            type_,
            where_clause,
            items,
            span: start.merge(end),
        })
    }

    // ── Import ────────────────────────────────────────────────

    fn parse_import(&mut self) -> Result<ImportStmt, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Import)?;

        let mut path = vec![self.expect_name()?];
        while self.match_token(&Token::Dot) {
            // Check for grouped import: import std.sync.{Arc, Mutex}
            if self.check(&Token::LBrace) {
                self.advance(); // skip {
                let mut names = Vec::new();
                while !self.check(&Token::RBrace) && !self.at_end() {
                    names.push(self.expect_name()?);
                    if !self.check(&Token::RBrace) {
                        self.expect(&Token::Comma)?;
                    }
                }
                self.expect(&Token::RBrace)?;
                let end = self.previous_span();
                self.consume_newline();
                return Ok(ImportStmt::Grouped {
                    path,
                    names,
                    span: start.merge(end),
                });
            }
            path.push(self.expect_name()?);
        }

        let end = self.previous_span();
        self.consume_newline();

        Ok(ImportStmt::Simple {
            path,
            span: start.merge(end),
        })
    }

    fn parse_from_import(&mut self) -> Result<ImportStmt, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::From)?;

        let mut path = vec![self.expect_name()?];
        while self.match_token(&Token::Dot) {
            path.push(self.expect_name()?);
        }

        self.expect_keyword(Keyword::Import)?;

        let mut names = vec![self.expect_name()?];
        while self.match_token(&Token::Comma) {
            names.push(self.expect_name()?);
        }

        let end = self.previous_span();
        self.consume_newline();

        Ok(ImportStmt::From {
            path,
            names,
            span: start.merge(end),
        })
    }

    // ── Type Alias ────────────────────────────────────────────

    fn parse_type_alias(&mut self) -> Result<TypeAlias, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Type)?;
        let name = self.expect_identifier()?;

        let generic_params = if self.check(&Token::LBracket) {
            Some(self.parse_generic_params()?)
        } else {
            None
        };

        self.expect(&Token::Eq)?;
        let type_ = self.parse_type()?;
        let end = self.previous_span();
        self.consume_newline();

        Ok(TypeAlias {
            name,
            generic_params,
            type_,
            span: start.merge(end),
        })
    }

    // ── Newtype ───────────────────────────────────────────────

    fn parse_newtype(&mut self) -> Result<NewtypeDef, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Newtype)?;
        let name = self.expect_identifier()?;
        self.expect(&Token::LParen)?;
        let inner_type = self.parse_type()?;
        self.expect(&Token::RParen)?;
        let end = self.previous_span();
        self.consume_newline();

        Ok(NewtypeDef {
            name,
            inner_type,
            span: start.merge(end),
        })
    }

    // ── Extern ────────────────────────────────────────────────

    fn parse_extern_block(&mut self) -> Result<ExternBlock, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Extern)?;

        let abi = if let Token::StringLiteral(_) = self.peek() {
            if let Token::StringLiteral(s) = self.advance().node {
                let val: String = s
                    .segments
                    .iter()
                    .filter_map(|seg| {
                        if let crate::lexer::token::StringSegment::Literal(l) = seg {
                            Some(l.as_str())
                        } else {
                            None
                        }
                    })
                    .collect();
                Some(Spanned::new(val, self.previous_span()))
            } else {
                None
            }
        } else {
            None
        };

        self.expect(&Token::Colon)?;
        self.expect(&Token::Newline)?;
        self.expect(&Token::Indent)?;

        let mut items = Vec::new();
        while !self.check(&Token::Dedent) && !self.at_end() {
            if self.check(&Token::Newline) {
                self.advance();
                continue;
            }
            let func = self.parse_function_def(Vec::new(), Visibility::Private, None)?;
            let span = func.span;
            items.push(Spanned::new(func, span));
        }

        self.expect(&Token::Dedent)?;
        let end = self.previous_span();

        Ok(ExternBlock {
            abi,
            items,
            span: start.merge(end),
        })
    }

    // ── Static Declaration ────────────────────────────────────

    fn parse_static_decl(&mut self, visibility: Visibility) -> Result<StaticDecl, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Static)?;
        let type_ = self.parse_type()?;
        let name = self.expect_identifier()?;
        self.expect(&Token::Eq)?;
        let value = self.parse_expr()?;
        let end = self.previous_span();
        self.consume_newline();

        Ok(StaticDecl {
            visibility,
            type_,
            name,
            value,
            span: start.merge(end),
        })
    }

    // ── Const Item (declaration or function) ──────────────────

    fn parse_const_item(
        &mut self,
        attributes: Vec<Spanned<Attribute>>,
        visibility: Visibility,
        doc_comment: Option<String>,
    ) -> Result<Item, ParseError> {
        // const int X = 5  →  ConstDecl
        // const int factorial(int n):  →  FunctionDef with is_const
        let start = self.peek_span();
        self.expect_keyword(Keyword::Const)?;

        let type_ = self.parse_type()?;
        let name = self.expect_identifier()?;

        if self.check(&Token::LParen) {
            // It's a const function
            let func = self.parse_function_def_after_name(
                attributes,
                visibility,
                FunctionQualifiers {
                    is_const: true,
                    ..Default::default()
                },
                type_,
                name,
                doc_comment,
                start,
            )?;
            Ok(Item::Function(func))
        } else {
            // It's a const declaration
            self.expect(&Token::Eq)?;
            let value = self.parse_expr()?;
            let end = self.previous_span();
            self.consume_newline();

            Ok(Item::ConstDecl(ConstDecl {
                visibility,
                type_,
                name,
                value,
                span: start.merge(end),
            }))
        }
    }

    // ── Function Definition ───────────────────────────────────

    pub fn parse_function_def(
        &mut self,
        attributes: Vec<Spanned<Attribute>>,
        visibility: Visibility,
        doc_comment: Option<String>,
    ) -> Result<FunctionDef, ParseError> {
        let start = self.peek_span();

        let mut qualifiers = FunctionQualifiers::default();

        // Parse qualifiers
        loop {
            if self.match_keyword(Keyword::Async) {
                qualifiers.is_async = true;
            } else if self.match_keyword(Keyword::Const) {
                qualifiers.is_const = true;
            } else if self.match_keyword(Keyword::Static) {
                qualifiers.is_static = true;
            } else if self.match_keyword(Keyword::Unsafe) {
                qualifiers.is_unsafe = true;
            } else {
                break;
            }
        }

        let return_type = self.parse_type()?;
        let name = self.expect_identifier()?;

        self.parse_function_def_after_name(
            attributes,
            visibility,
            qualifiers,
            return_type,
            name,
            doc_comment,
            start,
        )
    }

    fn parse_function_def_after_name(
        &mut self,
        attributes: Vec<Spanned<Attribute>>,
        visibility: Visibility,
        qualifiers: FunctionQualifiers,
        return_type: Spanned<Type>,
        name: Spanned<String>,
        doc_comment: Option<String>,
        start: Span,
    ) -> Result<FunctionDef, ParseError> {
        let generic_params = if self.check(&Token::LBracket) {
            // Need to disambiguate: is this generic params on the function name,
            // or array indexing? In a function definition context, [ after name is generics.
            Some(self.parse_generic_params()?)
        } else {
            None
        };

        // Parse parameters
        self.expect(&Token::LParen)?;
        let params = self.parse_param_list()?;
        self.expect(&Token::RParen)?;

        // Parse throws clause
        let throws = if self.match_keyword(Keyword::Throws) {
            Some(self.parse_type()?)
        } else {
            None
        };

        // Parse where clause
        let where_clause = if self.check_keyword(Keyword::Where) {
            Some(self.parse_where_clause()?)
        } else {
            None
        };

        // Parse body
        let body = if self.match_token(&Token::Eq) {
            // Expression body: int double(int x) = x * 2
            let expr = self.parse_expr()?;
            self.consume_newline();
            FunctionBody::Expression(Box::new(expr))
        } else if self.check(&Token::Colon) {
            FunctionBody::Block(self.parse_block()?)
        } else {
            // Declaration only (trait method without body)
            self.consume_newline();
            FunctionBody::Declaration
        };

        let end = self.previous_span();

        Ok(FunctionDef {
            attributes,
            visibility,
            qualifiers,
            return_type,
            name,
            generic_params,
            params,
            throws,
            where_clause,
            body,
            doc_comment,
            span: start.merge(end),
        })
    }

    fn parse_param_list(&mut self) -> Result<Vec<Spanned<Param>>, ParseError> {
        let mut params = Vec::new();

        if self.check(&Token::RParen) {
            return Ok(params);
        }

        loop {
            let param = self.parse_param()?;
            params.push(param);
            if !self.match_token(&Token::Comma) {
                break;
            }
        }

        Ok(params)
    }

    fn parse_param(&mut self) -> Result<Spanned<Param>, ParseError> {
        let start = self.peek_span();

        let is_live = self.match_keyword(Keyword::Live);

        // Handle self parameter: self, &self, !self
        if self.check_keyword(Keyword::SelfLower) {
            let name_tok = self.advance();
            return Ok(Spanned::new(
                Param {
                    type_: Spanned::new(Type::SelfType, name_tok.span),
                    ownership: Ownership::Borrow,
                    name: Spanned::new("self".to_string(), name_tok.span),
                    default: None,
                    is_live,
                },
                start.merge(name_tok.span),
            ));
        }
        if self.check(&Token::Ampersand)
            && matches!(self.peek_ahead(1), Token::Keyword(Keyword::SelfLower))
        {
            self.advance(); // skip &
            let name_tok = self.advance(); // self
            return Ok(Spanned::new(
                Param {
                    type_: Spanned::new(Type::SelfType, name_tok.span),
                    ownership: Ownership::MutableBorrow,
                    name: Spanned::new("self".to_string(), name_tok.span),
                    default: None,
                    is_live,
                },
                start.merge(name_tok.span),
            ));
        }
        if self.check(&Token::Bang)
            && matches!(self.peek_ahead(1), Token::Keyword(Keyword::SelfLower))
        {
            self.advance(); // skip !
            let name_tok = self.advance(); // self
            return Ok(Spanned::new(
                Param {
                    type_: Spanned::new(Type::SelfType, name_tok.span),
                    ownership: Ownership::Move,
                    name: Spanned::new("self".to_string(), name_tok.span),
                    default: None,
                    is_live,
                },
                start.merge(name_tok.span),
            ));
        }

        let type_ = self.parse_type()?;

        // Check for ownership modifier on the name
        let ownership = if self.check(&Token::Ampersand) {
            self.advance();
            Ownership::MutableBorrow
        } else if self.check(&Token::Bang) {
            self.advance();
            Ownership::Move
        } else {
            Ownership::Borrow
        };

        let name = self.expect_identifier()?;

        // Default value
        let default = if self.match_token(&Token::Eq) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        let end = self.previous_span();

        Ok(Spanned::new(
            Param {
                type_,
                ownership,
                name,
                default,
                is_live,
            },
            start.merge(end),
        ))
    }

    // ── Generics ──────────────────────────────────────────────

    pub fn parse_generic_params(&mut self) -> Result<Spanned<GenericParams>, ParseError> {
        let start = self.peek_span();
        self.expect(&Token::LBracket)?;

        let mut params = Vec::new();
        while !self.check(&Token::RBracket) && !self.at_end() {
            let param_start = self.peek_span();

            if self.match_keyword(Keyword::Life) {
                let name = self.expect_identifier()?;
                let end = self.previous_span();
                params.push(Spanned::new(
                    GenericParam::Lifetime(name),
                    param_start.merge(end),
                ));
            } else if self.match_keyword(Keyword::Const) {
                let type_ = self.parse_type()?;
                let name = self.expect_identifier()?;
                let end = self.previous_span();
                params.push(Spanned::new(
                    GenericParam::Const { type_, name },
                    param_start.merge(end),
                ));
            } else {
                let name = self.expect_identifier()?;
                let end = self.previous_span();
                params.push(Spanned::new(
                    GenericParam::Type(name),
                    param_start.merge(end),
                ));
            }

            if !self.check(&Token::RBracket) {
                self.expect(&Token::Comma)?;
            }
        }

        self.expect(&Token::RBracket)?;
        let end = self.previous_span();

        Ok(Spanned::new(GenericParams { params }, start.merge(end)))
    }

    pub fn parse_where_clause(&mut self) -> Result<Spanned<WhereClause>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Where)?;

        let mut bounds = Vec::new();
        loop {
            let bound_start = self.peek_span();
            let type_name = self.expect_identifier()?;
            self.expect_keyword(Keyword::Is)?;
            let trait_bounds = self.parse_trait_bound_list()?;
            let bound_end = self.previous_span();

            bounds.push(Spanned::new(
                WhereBound {
                    type_name,
                    bounds: trait_bounds,
                },
                bound_start.merge(bound_end),
            ));

            if !self.match_token(&Token::Comma) {
                break;
            }
        }

        let end = self.previous_span();
        Ok(Spanned::new(WhereClause { bounds }, start.merge(end)))
    }

    pub fn parse_trait_bound_list(&mut self) -> Result<Vec<Spanned<TraitBound>>, ParseError> {
        let mut bounds = Vec::new();
        bounds.push(self.parse_single_trait_bound()?);
        while self.match_token(&Token::Plus) {
            bounds.push(self.parse_single_trait_bound()?);
        }
        Ok(bounds)
    }

    fn parse_single_trait_bound(&mut self) -> Result<Spanned<TraitBound>, ParseError> {
        let start = self.peek_span();
        let name = self.expect_identifier()?;

        let mut generic_args = None;
        let mut assoc_type_bindings = Vec::new();

        if self.match_token(&Token::LBracket) {
            // Could be generic args or associated type bindings
            let mut args = Vec::new();
            while !self.check(&Token::RBracket) && !self.at_end() {
                // Check for associated type binding: Item = T
                if matches!(self.peek(), Token::Identifier(_))
                    && matches!(self.peek_ahead(1), Token::Eq)
                {
                    let assoc_name = self.expect_identifier()?;
                    self.expect(&Token::Eq)?;
                    let assoc_type = self.parse_type()?;
                    assoc_type_bindings.push(AssocTypeBinding {
                        name: assoc_name,
                        type_: assoc_type,
                    });
                } else {
                    args.push(self.parse_type()?);
                }
                if !self.check(&Token::RBracket) {
                    self.expect(&Token::Comma)?;
                }
            }
            self.expect(&Token::RBracket)?;
            if !args.is_empty() {
                generic_args = Some(args);
            }
        }

        let end = self.previous_span();
        Ok(Spanned::new(
            TraitBound {
                name,
                generic_args,
                assoc_type_bindings,
            },
            start.merge(end),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: parse source, return module. Panics if there are errors.
    fn parse(source: &str) -> Module {
        let mut parser = Parser::new(source);
        let module = parser.parse_module();
        if !parser.errors.is_empty() {
            for e in &parser.errors {
                eprintln!("Parse error: {:?}", e);
            }
            panic!("Parser produced {} error(s)", parser.errors.len());
        }
        module
    }

    /// Helper: parse source, expect errors.
    fn parse_with_errors(source: &str) -> (Module, Vec<crate::errors::ParseError>) {
        let mut parser = Parser::new(source);
        let module = parser.parse_module();
        (module, parser.errors)
    }

    // ── Import ──────────────────────────────────────────────────

    #[test]
    fn test_from_import() {
        let module = parse("from std.fmt import Displayable\n");
        assert_eq!(module.items.len(), 1);
        assert!(matches!(&module.items[0].node, Item::Import(ImportStmt::From { path, names, .. })
            if path.len() == 2 && names.len() == 1));
    }

    #[test]
    fn test_simple_import() {
        let module = parse("import std.io\n");
        assert_eq!(module.items.len(), 1);
        assert!(matches!(&module.items[0].node, Item::Import(ImportStmt::Simple { path, .. })
            if path.len() == 2));
    }

    #[test]
    fn test_grouped_import() {
        let module = parse("import std.sync.{Arc, Mutex}\n");
        assert_eq!(module.items.len(), 1);
        assert!(matches!(&module.items[0].node, Item::Import(ImportStmt::Grouped { path, names, .. })
            if path.len() == 2 && names.len() == 2));
    }

    // ── Struct ──────────────────────────────────────────────────

    #[test]
    fn test_struct_def() {
        let module = parse("struct Point:\n    float x\n    float y\n");
        assert_eq!(module.items.len(), 1);
        if let Item::Struct(ref s) = module.items[0].node {
            assert_eq!(s.name.node, "Point");
            assert_eq!(s.fields.len(), 2);
            assert_eq!(s.fields[0].node.name.node, "x");
            assert_eq!(s.fields[1].node.name.node, "y");
        } else {
            panic!("Expected struct");
        }
    }

    #[test]
    fn test_generic_struct() {
        let module = parse("struct Pair[A, B]:\n    A first\n    B second\n");
        assert_eq!(module.items.len(), 1);
        if let Item::Struct(ref s) = module.items[0].node {
            assert!(s.generic_params.is_some());
            let gp = s.generic_params.as_ref().unwrap();
            assert_eq!(gp.node.params.len(), 2);
        } else {
            panic!("Expected struct");
        }
    }

    // ── Enum ────────────────────────────────────────────────────

    #[test]
    fn test_enum_def() {
        let module = parse("enum Color:\n    Red\n    Green\n    Blue\n    Custom(uint8, uint8, uint8)\n");
        assert_eq!(module.items.len(), 1);
        if let Item::Enum(ref e) = module.items[0].node {
            assert_eq!(e.name.node, "Color");
            assert_eq!(e.variants.len(), 4);
            assert!(matches!(e.variants[0].node.fields, VariantFields::Unit));
            assert!(matches!(&e.variants[3].node.fields, VariantFields::Tuple(types) if types.len() == 3));
        } else {
            panic!("Expected enum");
        }
    }

    // ── Function ────────────────────────────────────────────────

    #[test]
    fn test_function_with_block() {
        let module = parse("int add(int a, int b):\n    return a + b\n");
        assert_eq!(module.items.len(), 1);
        if let Item::Function(ref f) = module.items[0].node {
            assert_eq!(f.name.node, "add");
            assert_eq!(f.params.len(), 2);
            assert!(matches!(f.body, FunctionBody::Block(_)));
        } else {
            panic!("Expected function");
        }
    }

    #[test]
    fn test_function_expression_body() {
        let module = parse("int double(int x) = x * 2\n");
        assert_eq!(module.items.len(), 1);
        if let Item::Function(ref f) = module.items[0].node {
            assert_eq!(f.name.node, "double");
            assert!(matches!(f.body, FunctionBody::Expression(_)));
        } else {
            panic!("Expected function");
        }
    }

    #[test]
    fn test_void_function() {
        let module = parse("void main():\n    pass\n");
        assert_eq!(module.items.len(), 1);
        if let Item::Function(ref f) = module.items[0].node {
            assert_eq!(f.name.node, "main");
            assert!(matches!(f.return_type.node, Type::Primitive(PrimitiveType::Void)));
        } else {
            panic!("Expected function");
        }
    }

    #[test]
    fn test_function_with_throws() {
        let module = parse("int parse_int(String s) throws ValueError:\n    pass\n");
        assert_eq!(module.items.len(), 1);
        if let Item::Function(ref f) = module.items[0].node {
            assert!(f.throws.is_some());
        } else {
            panic!("Expected function");
        }
    }

    #[test]
    fn test_async_function() {
        let module = parse("async int fetch():\n    pass\n");
        assert_eq!(module.items.len(), 1);
        if let Item::Function(ref f) = module.items[0].node {
            assert!(f.qualifiers.is_async);
        } else {
            panic!("Expected function");
        }
    }

    // ── Implement Block ─────────────────────────────────────────

    #[test]
    fn test_trait_impl() {
        let module = parse("implement Displayable for Point:\n    String to_string(self):\n        return \"point\"\n");
        assert_eq!(module.items.len(), 1);
        if let Item::Implement(ref imp) = module.items[0].node {
            assert!(imp.trait_.is_some());
            assert_eq!(imp.items.len(), 1);
        } else {
            panic!("Expected implement block");
        }
    }

    #[test]
    fn test_inherent_impl() {
        let module = parse("implement Point:\n    float distance(self):\n        return 0.0\n");
        assert_eq!(module.items.len(), 1);
        if let Item::Implement(ref imp) = module.items[0].node {
            assert!(imp.trait_.is_none());
            assert_eq!(imp.items.len(), 1);
        } else {
            panic!("Expected implement block");
        }
    }

    #[test]
    fn test_self_param_variants() {
        // Test bare self (immutable borrow)
        let module = parse("implement Foo:\n    void a(self):\n        pass\n");
        if let Item::Implement(ref imp) = module.items[0].node {
            let param = &imp.items[0].node.params[0].node;
            assert_eq!(param.ownership, Ownership::Borrow);
        } else {
            panic!();
        }

        // Test &self (mutable borrow)
        let module = parse("implement Foo:\n    void b(&self):\n        pass\n");
        if let Item::Implement(ref imp) = module.items[0].node {
            let param = &imp.items[0].node.params[0].node;
            assert_eq!(param.ownership, Ownership::MutableBorrow);
        } else {
            panic!();
        }

        // Test !self (move)
        let module = parse("implement Foo:\n    void c(!self):\n        pass\n");
        if let Item::Implement(ref imp) = module.items[0].node {
            let param = &imp.items[0].node.params[0].node;
            assert_eq!(param.ownership, Ownership::Move);
        } else {
            panic!();
        }
    }

    // ── Trait ───────────────────────────────────────────────────

    #[test]
    fn test_trait_def() {
        let module = parse("trait Drawable:\n    void draw(self)\n");
        assert_eq!(module.items.len(), 1);
        if let Item::Trait(ref t) = module.items[0].node {
            assert_eq!(t.name.node, "Drawable");
            assert_eq!(t.items.len(), 1);
        } else {
            panic!("Expected trait");
        }
    }

    #[test]
    fn test_trait_extends() {
        let module = parse("trait Drawable extends Displayable:\n    void draw(self)\n");
        if let Item::Trait(ref t) = module.items[0].node {
            assert_eq!(t.extends.len(), 1);
        } else {
            panic!("Expected trait");
        }
    }

    // ── Statements ──────────────────────────────────────────────

    #[test]
    fn test_var_decl() {
        let module = parse("void main():\n    int x = 5\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                assert_eq!(block.stmts.len(), 1);
                assert!(matches!(&block.stmts[0].node, Stmt::VarDecl { is_const: false, .. }));
            } else {
                panic!("Expected block body");
            }
        } else {
            panic!("Expected function");
        }
    }

    #[test]
    fn test_const_var_decl() {
        let module = parse("void main():\n    const int y = 10\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                assert!(matches!(&block.stmts[0].node, Stmt::VarDecl { is_const: true, .. }));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_auto_var_decl() {
        let module = parse("void main():\n    auto name = \"vyper\"\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                if let Stmt::VarDecl { ref type_, .. } = block.stmts[0].node {
                    assert!(matches!(type_.node, Type::Inferred));
                } else {
                    panic!();
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_if_elif_else() {
        let module = parse("void main():\n    if x > 0:\n        pass\n    elif x < 0:\n        pass\n    else:\n        pass\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                if let Stmt::If { ref elif_branches, ref else_body, .. } = block.stmts[0].node {
                    assert_eq!(elif_branches.len(), 1);
                    assert!(else_body.is_some());
                } else {
                    panic!("Expected if statement");
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_for_loop() {
        let module = parse("void main():\n    for i in 0..10:\n        pass\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                assert!(matches!(&block.stmts[0].node, Stmt::For { .. }));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_while_loop() {
        let module = parse("void main():\n    while x > 0:\n        x = x - 1\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                assert!(matches!(&block.stmts[0].node, Stmt::While { .. }));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_compound_assignment() {
        let module = parse("void main():\n    x += 1\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                assert!(matches!(&block.stmts[0].node, Stmt::CompoundAssign { op: BinaryOp::Add, .. }));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_return_stmt() {
        let module = parse("int foo():\n    return 42\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                assert!(matches!(&block.stmts[0].node, Stmt::Return(Some(_))));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    // ── Expressions ─────────────────────────────────────────────

    #[test]
    fn test_binary_expr() {
        let module = parse("int foo() = 1 + 2 * 3\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Expression(ref expr) = f.body {
                // Should be Add(1, Mul(2, 3))
                if let Expr::BinaryOp { ref op, ref right, .. } = expr.node {
                    assert_eq!(*op, BinaryOp::Add);
                    assert!(matches!(&right.node, Expr::BinaryOp { op: BinaryOp::Mul, .. }));
                } else {
                    panic!("Expected binary op");
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_method_call() {
        let module = parse("void main():\n    x.foo(1, 2)\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                if let Stmt::Expr(ref expr) = block.stmts[0].node {
                    assert!(matches!(&expr.node, Expr::MethodCall { .. }));
                } else {
                    panic!();
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_field_access() {
        let module = parse("void main():\n    x.y\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                if let Stmt::Expr(ref expr) = block.stmts[0].node {
                    assert!(matches!(&expr.node, Expr::FieldAccess { .. }));
                } else {
                    panic!();
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_list_comprehension() {
        let module = parse("void main():\n    auto squares = [x * x for x in 0..10]\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                if let Stmt::VarDecl { ref value, .. } = block.stmts[0].node {
                    assert!(matches!(&value.node, Expr::ListComprehension { .. }));
                } else {
                    panic!();
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_optional_chaining() {
        let module = parse("void main():\n    a?.b\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                if let Stmt::Expr(ref expr) = block.stmts[0].node {
                    assert!(matches!(&expr.node, Expr::OptionalChain { .. }));
                } else {
                    panic!();
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_nil_coalescing() {
        let module = parse("void main():\n    auto x = a ?? b\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                if let Stmt::VarDecl { ref value, .. } = block.stmts[0].node {
                    assert!(matches!(&value.node, Expr::NilCoalescing { .. }));
                } else {
                    panic!();
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    // ── Type Alias & Newtype ────────────────────────────────────

    #[test]
    fn test_type_alias() {
        let module = parse("type StringList = Vector[String]\n");
        assert_eq!(module.items.len(), 1);
        assert!(matches!(&module.items[0].node, Item::TypeAlias(_)));
    }

    #[test]
    fn test_newtype() {
        let module = parse("newtype UserId(int)\n");
        assert_eq!(module.items.len(), 1);
        assert!(matches!(&module.items[0].node, Item::Newtype(_)));
    }

    // ── Attributes ──────────────────────────────────────────────

    #[test]
    fn test_attribute() {
        let module = parse("@derive(Debuggable)\nstruct Point:\n    int x\n");
        if let Item::Struct(ref s) = module.items[0].node {
            assert_eq!(s.attributes.len(), 1);
            assert_eq!(s.attributes[0].node.name.node, "derive");
        } else {
            panic!();
        }
    }

    // ── Visibility ──────────────────────────────────────────────

    #[test]
    fn test_public_visibility() {
        let module = parse("public int add(int a, int b) = a + b\n");
        if let Item::Function(ref f) = module.items[0].node {
            assert_eq!(f.visibility, Visibility::Public);
        } else {
            panic!();
        }
    }

    // ── Generic Functions ───────────────────────────────────────

    #[test]
    fn test_generic_function_with_where() {
        let module = parse("void print_all[T](Vector[T] items) where T is Displayable:\n    pass\n");
        if let Item::Function(ref f) = module.items[0].node {
            assert!(f.generic_params.is_some());
            assert!(f.where_clause.is_some());
        } else {
            panic!();
        }
    }

    // ── Complex Programs ────────────────────────────────────────

    #[test]
    fn test_basics_vy() {
        let source = std::fs::read_to_string("examples/basics.vy")
            .expect("Could not read examples/basics.vy");
        let module = parse(&source);
        // 7 items: import, struct, enum, implement, add, double, main
        assert_eq!(module.items.len(), 7);
    }

    // ── Error Recovery ──────────────────────────────────────────

    #[test]
    fn test_error_recovery() {
        // Parser should recover from errors and continue parsing
        let (module, errors) = parse_with_errors("struct Point:\n    float x\n\n!@#$%\n\nstruct Size:\n    int w\n");
        assert!(!errors.is_empty());
        // Should still parse both structs despite the error line
        assert!(module.items.len() >= 1);
    }

    #[test]
    fn test_match_stmt() {
        let module = parse(
            "void main():\n    match x:\n        case 1: print(\"one\")\n        case 2: print(\"two\")\n        else:\n            print(\"other\")\n"
        );
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                if let Stmt::Match { ref arms, ref else_arm, .. } = block.stmts[0].node {
                    assert_eq!(arms.len(), 2);
                    assert!(else_arm.is_some());
                } else {
                    panic!("Expected match stmt");
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_generic_type_in_decl() {
        let module = parse("void main():\n    Vector[int] items = [1, 2, 3]\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                if let Stmt::VarDecl { ref type_, .. } = block.stmts[0].node {
                    assert!(matches!(type_.node, Type::Named { .. }));
                } else {
                    panic!();
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_comprehensive_vy() {
        let source = std::fs::read_to_string("examples/comprehensive.vy")
            .expect("Could not read examples/comprehensive.vy");
        let module = parse(&source);
        // Should have many items: imports, type aliases, newtypes, structs, enums,
        // traits, impl blocks, functions, attributed struct
        assert!(module.items.len() >= 15);
    }

    #[test]
    fn test_enum_with_keyword_variants() {
        let module = parse("enum Option[T]:\n    Some(T)\n    None\n");
        if let Item::Enum(ref e) = module.items[0].node {
            assert_eq!(e.variants.len(), 2);
            assert_eq!(e.variants[0].node.name.node, "Some");
            assert_eq!(e.variants[1].node.name.node, "None");
        } else {
            panic!();
        }
    }

    #[test]
    fn test_loop_stmt() {
        let module = parse("void main():\n    loop:\n        break\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                assert!(matches!(&block.stmts[0].node, Stmt::Loop { .. }));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_pass_stmt() {
        let module = parse("void main():\n    pass\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                assert!(matches!(&block.stmts[0].node, Stmt::Pass));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_multiple_items() {
        let module = parse(
            "struct A:\n    int x\n\nstruct B:\n    int y\n\nint add(int a, int b) = a + b\n"
        );
        assert_eq!(module.items.len(), 3);
    }

    #[test]
    fn test_nested_blocks() {
        let module = parse(
            "void main():\n    if true:\n        if true:\n            pass\n"
        );
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                if let Stmt::If { ref then_body, .. } = block.stmts[0].node {
                    assert!(matches!(&then_body.stmts[0].node, Stmt::If { .. }));
                } else {
                    panic!();
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_string_interpolation_in_expr() {
        let module = parse("void main():\n    print(\"hello {name}\")\n");
        if let Item::Function(ref f) = module.items[0].node {
            if let FunctionBody::Block(ref block) = f.body {
                if let Stmt::Expr(ref expr) = block.stmts[0].node {
                    assert!(matches!(&expr.node, Expr::Call { .. }));
                } else {
                    panic!();
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn test_doc_comments() {
        let module = parse("#/ Documentation for the function\nint foo() = 42\n");
        if let Item::Function(ref f) = module.items[0].node {
            assert!(f.doc_comment.is_some());
        } else {
            panic!();
        }
    }

    #[test]
    fn test_empty_module() {
        let module = parse("");
        assert_eq!(module.items.len(), 0);
    }

    #[test]
    fn test_module_with_only_comments() {
        let module = parse("# This is a comment\n# Another comment\n");
        assert_eq!(module.items.len(), 0);
    }
}
