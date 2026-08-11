pub mod ast;
pub mod expr;
pub mod pattern;
pub mod stmt;
pub mod types;
pub mod visitor;

use crate::errors::{ParseError, ParseWarning};
use crate::lexer::token::{Keyword, Token};
use crate::lexer::Lexer;
use crate::span::{Span, Spanned};
use ast::*;

fn make_self_param(
    start: Span,
    name_span: Span,
    ownership: Ownership,
) -> Spanned<Param> {
    Spanned::new(
        Param {
            type_: Spanned::new(Type::SelfType, name_span),
            ownership,
            name: Spanned::new("self".to_string(), name_span),
            default: None,
            is_meta_op: false,
            // THE receiver chokepoint: all three receiver spellings
            // (`self` / `&self` / `^self`) route through here, and nothing
            // else does. Downstream consumers read `Param::is_receiver`
            // instead of re-deriving from the type or the name.
            is_receiver: true,
        },
        start.merge(name_span),
    )
}

/// Maximum number of errors before the parser stops trying to recover.
const MAX_ERRORS: usize = 10;

/// Recursive descent parser for Gorget source code.
pub struct Parser {
    kinds: Vec<Token>,
    spans: Vec<Span>,
    pos: usize,
    pub errors: Vec<ParseError>,
    pub warnings: Vec<ParseWarning>,
    /// Nesting depth for call-arg parsing. Used to auto-wrap `it` only at the
    /// outermost call-arg level and prevent double-wrapping in nested calls.
    call_arg_depth: usize,
    /// AST-tree nesting depth for the current expression. Bumped on each
    /// `parse_prefix` entry (parens/unary/atoms) and checked against
    /// `MAX_EXPR_DEPTH` both there and on the accumulated left-spine inside the
    /// Pratt precedence loop. A pathologically deep expression overflows the
    /// lowering recursion (SIGSEGV); this guard rejects it at parse time with a
    /// clean teaching error (à la clang `-fbracket-depth` / rustc
    /// `recursion_limit`). See `ExprDepthGuard` / `MAX_EXPR_DEPTH` in `expr.rs`.
    expr_depth: usize,
    /// Comments extracted from the token stream, for use by the formatter.
    pub comments: Vec<Spanned<String>>,
    /// True when parsing inside an `extern "C":` block or `extern "C"` inline declaration.
    /// Controls whether `cstr` is accepted as a type.
    in_extern_c: bool,
    /// Next synthetic base offset for f-string interpolation segment
    /// sub-parsing. Starts at 1<<40 (well above any plausible source-file
    /// size, ~1 TiB) and bumps by 1<<20 per segment. Per-Parser instead
    /// of process-global so span values are deterministic per parse —
    /// otherwise concurrent fixture parses on the same atomic produce
    /// different span values across test runs. See
    /// `parse_format_string_interp_exprs` in `expr.rs`.
    pub(crate) next_interp_offset: usize,
    /// A `ParseError` from inside a `try_parse` speculation that the
    /// caller should PROMOTE to the user instead of the fallback path's
    /// diagnostic. `try_parse` only signals failure via `None` (position
    /// restored) and drops the closure's error; some rejects — like D35's
    /// `FunctionTypeParamSigilBeforeType`, which unambiguously identifies
    /// the shape and carries a teaching diagnostic — must survive the
    /// backtrack so the user does not see the fallback's generic
    /// `expected expression, found 'void'`. The producer stashes the
    /// error here (via `stash_promotable_error`); the caller consumes it
    /// via `take_promotable_error()` after the fallback fails, and clears
    /// it on success paths so a stashed error from one call site never
    /// leaks into a later parse.
    pub(crate) pending_speculative_error: Option<ParseError>,
}

impl Parser {
    pub fn new(source: &str) -> Self {
        Self::new_with_offset(source, 0)
    }

    pub fn new_with_offset(source: &str, base_offset: usize) -> Self {
        // Synthetic f-string interp range: high-bit base at `1 << 40` (so
        // synthetic offsets stay distinguishable from real source offsets),
        // plus `base_offset << 20` per-module shift so each module's
        // synthetic range is disjoint. Without the shift, every module's
        // first f-string interp token shares span `1 << 40`, the resolver's
        // `resolution_map[span_start]` collides last-write-wins, and
        // `lower_call` emits the wrong mangled symbol at the interp site.
        let interp_base = (1usize << 40).wrapping_add(base_offset.wrapping_shl(20));
        // Single lexing pass: drive the iterator to collect tokens, then take
        // the accumulated lex errors from the SAME lexer (never a second
        // re-lex — that would be the sidecar the one-source-of-truth mandate
        // forbids). `by_ref()` keeps `lexer` alive so `lexer.errors` is still
        // reachable after tokenizing. Lex errors are converted to `ParseError`
        // and become the parser's initial error set, so every consumer that
        // already drains `parser.errors` rejects malformed tokens.
        let mut lexer = Lexer::new_with_offset(source, base_offset);
        let all_tokens: Vec<Spanned<Token>> = lexer.by_ref().collect();
        let lex_errors: Vec<ParseError> = std::mem::take(&mut lexer.errors)
            .into_iter()
            .map(ParseError::from)
            .collect();

        // Partition: Comment tokens go to side-table, everything else split into parallel arrays
        let mut kinds = Vec::new();
        let mut spans = Vec::new();
        let mut comments = Vec::new();
        for tok in all_tokens {
            if let Token::Comment(ref text) = tok.node {
                comments.push(Spanned::new(text.clone(), tok.span));
            } else {
                kinds.push(tok.node);
                spans.push(tok.span);
            }
        }

        // Ensure we always have an EOF sentinel
        let eof_pos = spans.last().map(|s| s.end).unwrap_or(0);
        kinds.push(Token::Eof);
        spans.push(Span::new(eof_pos, eof_pos));
        Self {
            kinds,
            spans,
            pos: 0,
            errors: lex_errors,
            warnings: Vec::new(),
            call_arg_depth: 0,
            expr_depth: 0,
            in_extern_c: false,
            comments,
            next_interp_offset: interp_base,
            pending_speculative_error: None,
        }
    }

    /// Stash a `ParseError` from inside a `try_parse` speculation so the
    /// caller can PROMOTE it over the fallback path's generic diagnostic.
    /// See `pending_speculative_error` docs. Callers should stash only
    /// errors whose diagnostic is more actionable than the fallback's —
    /// e.g. D35's `FunctionTypeParamSigilBeforeType`, which unambiguously
    /// identifies the shape and names the replacement spelling.
    pub(crate) fn stash_promotable_error(&mut self, err: ParseError) {
        self.pending_speculative_error = Some(err);
    }

    /// Take and clear any stashed speculative error. Callers that reach a
    /// non-fallback success path should also clear the stash so a stray
    /// error does not leak into later parsing.
    pub(crate) fn take_promotable_error(&mut self) -> Option<ParseError> {
        self.pending_speculative_error.take()
    }

    /// Returns `true` when the current position starts a named scope block:
    /// `Identifier Colon Newline Indent`.
    pub fn check_identifier_colon_block(&self) -> bool {
        matches!(self.peek_ahead(0), Token::Identifier(_))
            && matches!(self.peek_ahead(1), Token::Colon)
            && matches!(self.peek_ahead(2), Token::Newline)
            && matches!(self.peek_ahead(3), Token::Indent)
    }

    // ── Token Management ──────────────────────────────────────

    pub fn peek(&self) -> &Token {
        self.kinds.get(self.pos).unwrap_or(&Token::Eof)
    }

    pub fn peek_span(&self) -> Span {
        self.spans.get(self.pos).copied().unwrap_or(Span::dummy())
    }

    /// Look ahead n tokens (0 = current).
    pub fn peek_ahead(&self, n: usize) -> &Token {
        self.kinds.get(self.pos + n).unwrap_or(&Token::Eof)
    }

    pub fn advance(&mut self) -> Spanned<Token> {
        let kind = self.kinds.get(self.pos).cloned().unwrap_or(Token::Eof);
        let span = self.spans.get(self.pos).copied().unwrap_or(Span::dummy());
        self.pos += 1;
        Spanned::new(kind, span)
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

    /// Match `elif` or `else if` (treated identically).
    pub fn match_elif(&mut self) -> bool {
        if self.check_keyword(Keyword::Elif) {
            self.advance();
            true
        } else if self.check_keyword(Keyword::Else)
            && matches!(self.peek_ahead(1), Token::Keyword(Keyword::If))
        {
            self.advance(); // consume `else`
            self.advance(); // consume `if`
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
        if let Token::Identifier(sym) = self.peek() {
            let name = sym.as_str().to_string();
            let span = self.peek_span();
            self.advance();
            Ok(Spanned::new(name, span))
        } else {
            Err(self.error_unexpected("identifier"))
        }
    }

    /// Expect an identifier, but also accept keywords that can be used as names
    /// in certain positions (e.g., field names).
    pub fn expect_name(&mut self) -> Result<Spanned<String>, ParseError> {
        match self.peek() {
            Token::Identifier(sym) => {
                let name = sym.as_str().to_string();
                let span = self.peek_span();
                self.advance();
                Ok(Spanned::new(name, span))
            }
            Token::Keyword(kw) => {
                let name = kw.as_name().to_string();
                let span = self.peek_span();
                self.advance();
                Ok(Spanned::new(name, span))
            }
            _ => Err(self.error_unexpected("identifier")),
        }
    }

    /// Parse an ownership modifier: `&` → MutableBorrow, `!` or `^` → Move
    /// (D27 accept-both this round; canonical glyph is `^` per docs; `!` is
    /// the retired-but-still-accepted glyph — Round B does the fmt sweep and
    /// then rejects `!`), else Borrow.
    pub fn parse_ownership_modifier(&mut self) -> Ownership {
        if self.check(&Token::Ampersand) {
            self.advance();
            Ownership::MutableBorrow
        } else if self.check(&Token::Bang) || self.check(&Token::Caret) {
            self.advance();
            Ownership::Move
        } else {
            Ownership::Borrow
        }
    }

    /// Parse an optional `private` or `public` visibility modifier.
    /// Defaults to `Public` if neither is present.
    ///
    /// Returns `(visibility, explicit)` — `explicit` records whether a keyword
    /// was actually WRITTEN. The value alone cannot answer that (`public Foo`
    /// and a bare `Foo` both yield `Public`), and the formatter needs the
    /// distinction so it neither deletes an author's `public` nor invents one.
    pub fn parse_visibility_modifier(&mut self) -> (Visibility, bool) {
        if self.match_keyword(Keyword::Private) {
            (Visibility::Private, true)
        } else if self.match_keyword(Keyword::Public) {
            (Visibility::Public, true)
        } else {
            (Visibility::Public, false)
        }
    }

    pub fn at_end(&self) -> bool {
        matches!(self.peek(), Token::Eof)
    }

    pub fn previous_span(&self) -> Span {
        if self.pos > 0 {
            self.spans[self.pos - 1]
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

    /// Build the `ExpressionTooDeep` parse error for the current position.
    /// `depth` is the offending nesting depth; the limit is `MAX_EXPR_DEPTH`.
    pub fn error_expr_too_deep(&self, depth: usize) -> ParseError {
        ParseError {
            kind: crate::errors::ParseErrorKind::ExpressionTooDeep {
                depth,
                limit: crate::parser::expr::MAX_EXPR_DEPTH,
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

    /// Build the `MissingInitializer` parse error for a `Type name` declaration
    /// that has no `=` initializer. `span` is the binding name's span.
    pub fn error_missing_init(&self, span: Span) -> ParseError {
        ParseError {
            kind: crate::errors::ParseErrorKind::MissingInitializer,
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
                    | Keyword::Equip
                    | Keyword::Import
                    | Keyword::From,
                ) => return,
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// Expect the `: NEWLINE INDENT` sequence that begins an indented block.
    pub fn expect_block_start(&mut self) -> Result<(), ParseError> {
        self.expect(&Token::Colon)?;
        self.expect(&Token::Newline)?;
        self.expect(&Token::Indent)?;
        Ok(())
    }

    /// Synchronize and guarantee forward progress. If `synchronize()` didn't
    /// advance past any tokens, force one step forward to prevent infinite loops.
    pub fn synchronize_with_progress(&mut self) {
        let pos_before = self.pos;
        self.synchronize();
        if self.pos == pos_before {
            self.advance();
        }
    }

    /// Skip tokens until we reach the top level (balanced INDENT/DEDENT).
    /// Used after a failed top-level item to skip its entire body.
    fn synchronize_to_top_level(&mut self) {
        let mut depth: usize = 0;
        loop {
            match self.peek() {
                Token::Eof => return,
                Token::Indent => {
                    depth += 1;
                    self.advance();
                }
                Token::Dedent => {
                    if depth == 0 {
                        return;
                    }
                    depth -= 1;
                    self.advance();
                    if depth == 0 {
                        if self.check(&Token::Newline) {
                            self.advance();
                        }
                        return;
                    }
                }
                Token::Newline if depth == 0 => {
                    self.advance();
                    return;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// Returns `true` when the error limit has been reached.
    fn at_error_limit(&self) -> bool {
        self.errors.len() >= MAX_ERRORS
    }

    // ── Block Parsing ─────────────────────────────────────────

    /// Parse a block: COLON NEWLINE INDENT stmts DEDENT
    pub fn parse_block(&mut self) -> Result<Block, ParseError> {
        let start = self.peek_span();
        self.expect(&Token::Colon)?;
        self.parse_block_body(start)
    }

    /// Parse `NEWLINE INDENT stmt* DEDENT`, returning a Block.
    /// The colon (or other introducer) must already be consumed by the caller.
    pub fn parse_block_body(&mut self, start: Span) -> Result<Block, ParseError> {
        self.expect(&Token::Newline)?;
        self.expect(&Token::Indent)?;

        let mut stmts = Vec::new();
        while !self.check(&Token::Dedent) && !self.at_end() {
            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize_with_progress();
                }
            }
        }

        self.expect(&Token::Dedent)?;
        let end = self.previous_span();

        Ok(Block {
            stmts,
            span: start.merge(end),
            // The ONE `NextLine` writer: this function IS the indented-suite
            // grammar (`NEWLINE INDENT stmt* DEDENT`). Every other `Block`
            // construction in the parser is an inline or synthesized form.
            layout: SuiteLayout::NextLine,
        })
    }

    /// Consume a newline if present (used after statements).
    pub fn consume_newline(&mut self) {
        self.match_token(&Token::Newline);
    }

    /// Parse `: <body>` where `<body>` is either an indented block OR a
    /// single statement on the same line ("if-one-liner"). Used by
    /// `if` / `elif` / `else` statement-form bodies — mirrors Python's
    /// `if x: stmt` shape. The colon must be the current token; advance
    /// past it and dispatch on the next token (Newline → indented block,
    /// anything else → single inline statement).
    pub fn parse_block_or_inline_stmt(&mut self) -> Result<Block, ParseError> {
        let start = self.peek_span();
        self.expect(&Token::Colon)?;
        if self.check(&Token::Newline) {
            return self.parse_block_body(start);
        }
        // One-liner: parse a single statement at the post-colon position.
        // The wrapper Block has one stmt and a span covering the colon
        // through the stmt's end. `parse_stmt` consumes its own trailing
        // newline so the outer `if`/`elif`/`else` chain composes cleanly.
        let stmt = self.parse_stmt()?;
        let end = stmt.span;
        Ok(Block {
            stmts: vec![stmt],
            span: start.merge(end),
            layout: SuiteLayout::Inline,
        })
    }

    /// Parse a body that is either an indented block (→ `Expr::Do`) or a
    /// single expression on the same line. Used by rethrow and catch.
    pub fn parse_body_or_expr(&mut self, start: Span) -> Result<Spanned<Expr>, ParseError> {
        if self.check(&Token::Newline) {
            let block = self.parse_block_body(start)?;
            let span = block.span;
            // SYNTHESIZED `Do` — `catch (e):` / `rethrow (e):` take an
            // indented suite directly; the author wrote no `do`.
            Ok(Spanned::new(
                Expr::Do {
                    body: block,
                    author_spelled: false,
                },
                span,
            ))
        } else {
            self.parse_expr()
        }
    }

    /// Parse a match-arm body: indented block (→ `Expr::Block`) or inline
    /// expression (consumed newline). Used by match arms and meta-for match items.
    pub fn parse_arm_body(&mut self, start: Span) -> Result<Spanned<Expr>, ParseError> {
        if self.check(&Token::Newline) {
            let block = self.parse_block_body(start)?;
            let span = block.span;
            Ok(Spanned::new(Expr::Block(block), span))
        } else {
            let expr = self.parse_expr()?;
            self.consume_newline();
            Ok(expr)
        }
    }

    // ── Top-Level Parsing ─────────────────────────────────────

    /// Parse a complete module (top-level items).
    pub fn parse_module(&mut self) -> Module {
        let start = self.peek_span();
        let mut items = Vec::new();

        while !self.at_end() && !self.at_error_limit() {

            // Skip stray newlines at top level
            if self.check(&Token::Newline) {
                self.advance();
                continue;
            }

            match self.parse_item() {
                Ok(item) => {
                    items.push(item);
                }
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize_to_top_level();
                    // Guarantee progress
                    if !self.at_end() && !matches!(self.peek(), Token::Newline | Token::Dedent) {
                        // Still stuck — force one token forward
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

        // Check for directive before collecting doc comments/attributes
        if matches!(self.peek(), Token::Keyword(Keyword::Directive)) {
            let d = self.parse_directive()?;
            let span = start.merge(d.span);
            return Ok(Spanned::new(Item::Directive(d), span));
        }

        // Collect doc comments
        let doc_comment = self.collect_doc_comment();

        // Collect attributes
        let mut attributes = Vec::new();
        while self.check(&Token::At) {
            attributes.push(self.parse_attribute()?);
        }

        // Parse visibility — public by default; `private` makes the item module-private.
        // Exception: `static` declarations are private by default (use `public static` to export).
        // The `public` keyword is accepted for explicitness on other items.
        let (visibility, explicit_visibility) = if self.match_keyword(Keyword::Private) {
            (Visibility::Private, true)
        } else if self.match_keyword(Keyword::Public) {
            (Visibility::Public, true)
        } else {
            (Visibility::Public, false)
        };

        // Determine item kind
        match self.peek() {
            Token::Keyword(Keyword::Struct) => {
                let def =
                    self.parse_struct_def(attributes, visibility, explicit_visibility, doc_comment)?;
                let span = start.merge(def.span);
                Ok(Spanned::new(Item::Struct(def), span))
            }
            Token::Keyword(Keyword::Enum) => {
                let def =
                    self.parse_enum_def(attributes, visibility, explicit_visibility, doc_comment)?;
                let span = start.merge(def.span);
                Ok(Spanned::new(Item::Enum(def), span))
            }
            Token::Keyword(Keyword::Trait) => {
                let def =
                    self.parse_trait_def(attributes, visibility, explicit_visibility, doc_comment)?;
                let span = start.merge(def.span);
                Ok(Spanned::new(Item::Trait(def), span))
            }
            Token::Keyword(Keyword::Equip) => {
                let block = self.parse_equip_block()?;
                let span = start.merge(block.span);
                Ok(Spanned::new(Item::Equip(block), span))
            }
            Token::Keyword(Keyword::Import) => {
                let stmt = self.parse_import()?;
                let span = start.merge(stmt.span());
                Ok(Spanned::new(Item::Import(stmt), span))
            }
            Token::Keyword(Keyword::From) => {
                let stmt = self.parse_from_import()?;
                let span = start.merge(stmt.span());
                Ok(Spanned::new(Item::Import(stmt), span))
            }
            Token::Keyword(Keyword::Type) => {
                let alias = self.parse_type_alias(visibility, explicit_visibility)?;
                let span = start.merge(alias.span);
                Ok(Spanned::new(Item::TypeAlias(alias), span))
            }
            Token::Keyword(Keyword::Newtype) => {
                let nt = self.parse_newtype(visibility, explicit_visibility)?;
                let span = start.merge(nt.span);
                Ok(Spanned::new(Item::Newtype(nt), span))
            }
            Token::Keyword(Keyword::Extern) => {
                // Disambiguate: `extern "C":` (block) vs `extern "C" int foo()` (inline) vs `extern int foo()` (no abi).
                // Block form: extern + string literal + colon/newline → ExternBlock
                // Inline form: extern + optional string literal + type → FunctionDef
                let is_block = matches!(self.peek_ahead(1), Token::StringLiteral(_))
                    && matches!(self.peek_ahead(2), Token::Colon | Token::Newline);
                if is_block {
                    let ext = self.parse_extern_block()?;
                    let span = start.merge(ext.span);
                    Ok(Spanned::new(Item::ExternBlock(ext), span))
                } else {
                    let func = self.parse_function_def(
                        attributes,
                        visibility,
                        explicit_visibility,
                        doc_comment,
                    )?;
                    let span = start.merge(func.span);
                    Ok(Spanned::new(Item::Function(func), span))
                }
            }
            Token::Keyword(Keyword::Static) => {
                // Static declarations are private by default
                let vis = if explicit_visibility { visibility } else { Visibility::Private };
                let decl = self.parse_static_decl(vis, explicit_visibility)?;
                let span = start.merge(decl.span);
                Ok(Spanned::new(Item::StaticDecl(decl), span))
            }
            Token::Keyword(Keyword::Const) => {
                // Could be const function or const declaration
                // Peek ahead: if after const we see a type followed by identifier( → function
                // If after const we see a type followed by identifier = → const decl
                let decl =
                    self.parse_const_item(attributes, visibility, explicit_visibility, doc_comment)?;
                let span = start.merge(match &decl {
                    Item::ConstDecl(d) => d.span,
                    Item::Function(f) => f.span,
                    _ => unreachable!("parse_const_item returned unexpected item kind"),
                });
                Ok(Spanned::new(decl, span))
            }
            Token::Keyword(Keyword::Test) => {
                let def = self.parse_test_def(attributes, doc_comment)?;
                let span = start.merge(def.span);
                Ok(Spanned::new(Item::Test(def), span))
            }
            Token::Keyword(Keyword::Bench) => {
                let def = self.parse_bench_def(attributes, doc_comment)?;
                let span = start.merge(def.span);
                Ok(Spanned::new(Item::Bench(def), span))
            }
            Token::Keyword(Keyword::Suite) => {
                let (item, item_span) = self.parse_suite_block()?;
                let span = start.merge(item_span);
                Ok(Spanned::new(item, span))
            }
            Token::Keyword(Keyword::Meta) => {
                return self.parse_meta_item();
            }
            // Function definition (starts with return type).
            // Exception: `TypeName varname = expr` is a module-level variable declaration.
            _ => {
                if self.looks_like_module_var_decl() {
                    // Module-level var decls are implicitly static — private by default
                    let vis = if explicit_visibility { visibility } else { Visibility::Private };
                    let decl = self.parse_module_var_decl(vis, explicit_visibility)?;
                    let span = start.merge(decl.span);
                    Ok(Spanned::new(Item::StaticDecl(decl), span))
                } else {
                    let func = self.parse_function_def(
                        attributes,
                        visibility,
                        explicit_visibility,
                        doc_comment,
                    )?;
                    let span = start.merge(func.span);
                    Ok(Spanned::new(Item::Function(func), span))
                }
            }
        }
    }

    // ── Directive ─────────────────────────────────────────────

    fn parse_directive(&mut self) -> Result<Directive, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Directive)?;

        // Parse directive name (may be hyphenated: strip-asserts)
        let first = self.expect_identifier()?;
        let mut name = first.node;
        while self.check(&Token::Minus) {
            self.advance(); // consume '-'
            let part = self.expect_identifier()?;
            name.push('-');
            name.push_str(&part.node);
        }

        // Parse optional =value
        let value = if self.match_token(&Token::Eq) {
            let val = self.expect_identifier()?;
            Some(val.node)
        } else {
            None
        };

        let end = self.previous_span();
        self.consume_newline();

        Ok(Directive {
            name,
            value,
            span: start.merge(end),
        })
    }

    // ── Meta (Compile-Time) Items ─────────────────────────────

    /// Dispatch `meta` to the appropriate sub-parser.
    fn parse_meta_item(&mut self) -> Result<Spanned<Item>, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Meta)?;

        match self.peek() {
            Token::Keyword(Keyword::Type) => self.parse_meta_type(start),
            Token::Keyword(Keyword::Assert) => self.parse_meta_assert(start),
            Token::Keyword(Keyword::If) => self.parse_meta_if(start),
            Token::Identifier(s) if s == "log" => self.parse_meta_log_item(start),
            _ => self.parse_meta_const(start),
        }
    }

    /// `meta <type> <name> = <expr>`
    fn parse_meta_const(&mut self, start: Span) -> Result<Spanned<Item>, ParseError> {
        let type_ = self.parse_type()?;
        let name = self.expect_identifier()?;
        self.expect(&Token::Eq)?;
        let value = self.parse_expr()?;
        let end = self.previous_span();
        self.consume_newline();
        let span = start.merge(end);
        Ok(Spanned::new(
            Item::MetaConst(MetaConst {
                type_,
                name,
                value,
                span,
            }),
            span,
        ))
    }

    /// `meta type <name> = <type>` or `meta type <name>(<params>): <block>`
    fn parse_meta_type(&mut self, start: Span) -> Result<Spanned<Item>, ParseError> {
        self.expect_keyword(Keyword::Type)?;
        let name = self.expect_identifier()?;

        if self.check(&Token::LParen) {
            // MetaTypeFunc: meta type name(params): block
            self.advance(); // consume (
            let params = self.parse_param_list()?;
            self.expect(&Token::RParen)?;
            let body = self.parse_block()?;
            let end = self.previous_span();
            let span = start.merge(end);
            Ok(Spanned::new(
                Item::MetaTypeFunc(MetaTypeFunc {
                    name,
                    params,
                    body,
                    span,
                }),
                span,
            ))
        } else {
            // MetaType: meta type name = <rhs>
            self.expect(&Token::Eq)?;
            let rhs = self.parse_meta_type_rhs()?;
            let end = self.previous_span();
            self.consume_newline();
            let span = start.merge(end);
            Ok(Spanned::new(
                Item::MetaType(MetaType {
                    name,
                    rhs,
                    span,
                }),
                span,
            ))
        }
    }

    /// Parse the RHS of a `meta type Name = <rhs>` declaration.
    ///
    /// Handles three forms:
    /// - Plain:       `meta type Num = int`
    /// - Conditional: `meta type Map = Dict if feature("ordered") else HashMap`
    /// - Call:        `meta type Word = sized_int(arch_word_bits())`
    fn parse_meta_type_rhs(&mut self) -> Result<MetaTypeRhs, ParseError> {
        let start = self.peek_span();
        let base = self.parse_base_type()?;

        // Type function call: bare named type followed by (
        // Must intercept before parse_type_postfix, which treats Named(...) + ( as a function type.
        if let Type::Named { ref name, ref generic_args } = base.node {
            if generic_args.is_empty() && self.check(&Token::LParen) {
                let callee = name.clone();
                self.advance(); // consume (
                let mut args = Vec::new();
                while !self.check(&Token::RParen) && !self.at_end() {
                    args.push(self.parse_expr()?);
                    if !self.check(&Token::RParen) {
                        self.expect(&Token::Comma)?;
                    }
                }
                self.expect(&Token::RParen)?;
                return Ok(MetaTypeRhs::Call { callee, args });
            }
        }

        // Complete the type with postfix modifiers (array/function type suffixes)
        let full_type = self.parse_type_postfix(base, start)?;

        // Conditional type: <type> if <cond> else <type>
        if self.check_keyword(Keyword::If) {
            self.advance();
            let condition = self.parse_expr()?;
            self.expect_keyword(Keyword::Else)?;
            let else_type = self.parse_type()?;
            return Ok(MetaTypeRhs::Conditional {
                then_type: full_type,
                condition,
                else_type,
            });
        }

        Ok(MetaTypeRhs::Plain(full_type))
    }

    /// `meta assert <expr> [, <msg>]`
    fn parse_meta_assert(&mut self, start: Span) -> Result<Spanned<Item>, ParseError> {
        self.expect_keyword(Keyword::Assert)?;
        let condition = self.parse_expr()?;
        let message = if self.match_token(&Token::Comma) {
            Some(self.parse_expr()?)
        } else {
            None
        };
        let end = self.previous_span();
        self.consume_newline();
        let span = start.merge(end);
        Ok(Spanned::new(
            Item::MetaAssert(MetaAssert {
                condition,
                message,
                span,
            }),
            span,
        ))
    }

    /// `meta log <expr> [, <expr> ...]`
    fn parse_meta_log_item(&mut self, start: Span) -> Result<Spanned<Item>, ParseError> {
        self.advance(); // consume `log` identifier
        let mut args = vec![self.parse_expr()?];
        while self.match_token(&Token::Comma) {
            args.push(self.parse_expr()?);
        }
        let end = self.previous_span();
        self.consume_newline();
        let span = start.merge(end);
        Ok(Spanned::new(Item::MetaLog(MetaLog { args, span }), span))
    }

    /// `meta if <expr>: <items> [elif <expr>: <items>]* [else: <items>]`
    fn parse_meta_if(&mut self, start: Span) -> Result<Spanned<Item>, ParseError> {
        self.expect_keyword(Keyword::If)?;
        let condition = self.parse_expr()?;
        let then_items = self.parse_meta_block()?;

        let mut elif_branches = Vec::new();
        while self.match_elif() {
            let elif_cond = self.parse_expr()?;
            let elif_items = self.parse_meta_block()?;
            elif_branches.push((elif_cond, elif_items));
        }

        let else_branch = if self.match_keyword(Keyword::Else) {
            // The clause header's own position, recorded where it is known —
            // the formatter cannot recover it, since `else` has no expression
            // and the first item starts on the next line. Captured together
            // with the items so the pair cannot come apart.
            let kw = self.previous_span();
            Some((kw, self.parse_meta_block()?))
        } else {
            None
        };

        let end = self.previous_span();
        let span = start.merge(end);
        Ok(Spanned::new(
            Item::MetaIf(MetaIf {
                condition,
                then_items,
                elif_branches,
                else_branch,
                span,
            }),
            span,
        ))
    }

    /// Parse `: NEWLINE INDENT item* DEDENT` — a block of items (not statements).
    fn parse_meta_block(&mut self) -> Result<Vec<Spanned<Item>>, ParseError> {
        self.expect_block_start()?;

        let mut items = Vec::new();
        while !self.check(&Token::Dedent) && !self.at_end() {
            // Skip stray newlines
            if self.match_token(&Token::Newline) {
                continue;
            }
            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize_with_progress();
                }
            }
        }

        self.expect(&Token::Dedent)?;
        Ok(items)
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
                            if let Token::StringLiteral(s) = self.peek() {
                                let text = s.as_plain_text();
                                let val_span = self.peek_span();
                                self.advance();
                                args.push(AttributeArg::KeyValue(
                                    ident.node,
                                    AttributeArgValue::Str(Spanned::new(text, val_span)),
                                ));
                            } else {
                                let val_ident = self.expect_identifier()?;
                                args.push(AttributeArg::KeyValue(
                                    ident.node,
                                    AttributeArgValue::Ident(val_ident),
                                ));
                            }
                        } else {
                            args.push(AttributeArg::Identifier(ident.node));
                        }
                    }
                    Token::StringLiteral(_) => {
                        let lit_span = self.peek_span();
                        if let Token::StringLiteral(s) = self.advance().node {
                            args.push(AttributeArg::StringLiteral(Spanned::new(
                                s.as_plain_text(),
                                lit_span,
                            )));
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
        explicit_visibility: bool,
        doc_comment: Option<String>,
    ) -> Result<StructDef, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Struct)?;
        let name = self.expect_identifier()?;

        let generic_params = self.try_parse_generic_params()?;

        self.expect_block_start()?;

        let mut fields = Vec::new();
        // Allow `pass` for empty struct bodies (opaque types)
        if self.match_keyword(Keyword::Pass) {
            self.consume_newline();
        }
        while !self.check(&Token::Dedent) && !self.at_end() && !self.at_error_limit() {
            if self.check(&Token::Newline) {
                self.advance();
                continue;
            }
            let saved_pos = self.pos;
            let field_start = self.peek_span();
            let (field_vis, field_vis_explicit) = self.parse_visibility_modifier();
            match self.parse_type().and_then(|type_| {
                let field_name = self.expect_identifier()?;
                let field_end = self.previous_span();
                self.consume_newline();
                Ok(Spanned::new(
                    FieldDef {
                        visibility: field_vis,
                        explicit_visibility: field_vis_explicit,
                        type_,
                        name: field_name,
                    },
                    field_start.merge(field_end),
                ))
            }) {
                Ok(field) => fields.push(field),
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize_with_progress();
                    // Ensure progress even if synchronize landed on same spot
                    if self.pos == saved_pos {
                        self.advance();
                    }
                }
            }
        }

        self.expect(&Token::Dedent)?;
        let end = self.previous_span();

        Ok(StructDef {
            attributes,
            visibility,
            explicit_visibility,
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
        explicit_visibility: bool,
        doc_comment: Option<String>,
    ) -> Result<EnumDef, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Enum)?;
        let name = self.expect_identifier()?;

        let generic_params = self.try_parse_generic_params()?;

        self.expect_block_start()?;

        let mut variants = Vec::new();
        while !self.check(&Token::Dedent) && !self.at_end() && !self.at_error_limit() {
            if self.check(&Token::Newline) {
                self.advance();
                continue;
            }
            let saved_pos = self.pos;
            let var_start = self.peek_span();
            match self.parse_enum_variant_inner(var_start) {
                Ok(variant) => variants.push(variant),
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

        Ok(EnumDef {
            attributes,
            visibility,
            explicit_visibility,
            name,
            generic_params,
            variants,
            doc_comment,
            span: start.merge(end),
        })
    }

    fn parse_enum_variant_inner(&mut self, var_start: Span) -> Result<Spanned<Variant>, ParseError> {
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

        Ok(Spanned::new(
            Variant {
                name: var_name,
                fields,
            },
            var_start.merge(var_end),
        ))
    }

    // ── Trait ─────────────────────────────────────────────────

    fn parse_trait_def(
        &mut self,
        attributes: Vec<Spanned<Attribute>>,
        visibility: Visibility,
        explicit_visibility: bool,
        doc_comment: Option<String>,
    ) -> Result<TraitDef, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Trait)?;
        let name = self.expect_identifier()?;

        let generic_params = self.try_parse_generic_params()?;

        let extends = if self.match_keyword(Keyword::Extends) {
            self.parse_trait_bound_list()?
        } else {
            Vec::new()
        };

        self.expect_block_start()?;

        let mut items = Vec::new();
        while !self.check(&Token::Dedent) && !self.at_end() && !self.at_error_limit() {
            // Skip doc comments within trait body
            let method_doc = self.collect_doc_comment();

            if self.check(&Token::Newline) {
                self.advance();
                continue;
            }

            let saved_pos = self.pos;
            let result = if self.check_keyword(Keyword::Type) {
                self.parse_associated_type().map(|assoc| {
                    Spanned::new(TraitItem::AssociatedType(assoc.node), assoc.span)
                })
            } else {
                self.parse_function_def(Vec::new(), Visibility::Public, false, method_doc).map(|func| {
                    let span = func.span;
                    Spanned::new(TraitItem::Method(func), span)
                })
            };
            match result {
                Ok(item) => items.push(item),
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

        Ok(TraitDef {
            attributes,
            visibility,
            explicit_visibility,
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

        // Optional bounds: `type Iter: Iterator[T] & Clone`
        let bounds = if self.match_token(&Token::Colon) {
            self.parse_trait_bound_list()?
        } else {
            Vec::new()
        };

        // Optional default: `type Item = T`
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
                bounds,
                default,
                span: start.merge(end),
            },
            start.merge(end),
        ))
    }

    // ── Equip Block ────────────────────────────────────────────

    fn parse_equip_block(&mut self) -> Result<EquipBlock, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Equip)?;

        let generic_params = self.try_parse_generic_params()?;

        // Parse self type (always comes first in equip syntax)
        let self_type = self.parse_type()?;

        // Check for "with Trait" to determine if this is a trait impl
        let trait_ = if self.match_keyword(Keyword::With) {
            let trait_name = self.parse_type()?;
            let trait_span = trait_name.span;
            Some(EquipTrait {
                trait_name,
                span: trait_span,
            })
        } else {
            None
        };

        let via_field = if self.match_keyword(Keyword::Via) {
            Some(self.expect_identifier()?)
        } else {
            None
        };

        let items = if self.check(&Token::Colon) {
            self.advance();
            self.expect(&Token::Newline)?;
            self.expect(&Token::Indent)?;

            let mut items = Vec::new();
            while !self.check(&Token::Dedent) && !self.at_end() && !self.at_error_limit() {
                let method_doc = self.collect_doc_comment();

                if self.check(&Token::Newline) {
                    self.advance();
                    continue;
                }

                // `pass` means no methods in this equip block
                if self.match_keyword(Keyword::Pass) {
                    if self.check(&Token::Newline) {
                        self.advance();
                    }
                    continue;
                }

                let saved_pos = self.pos;

                // Collect attributes for methods
                let mut attrs = Vec::new();
                let attr_ok = loop {
                    if !self.check(&Token::At) {
                        break true;
                    }
                    match self.parse_attribute() {
                        Ok(attr) => attrs.push(attr),
                        Err(e) => {
                            self.errors.push(e);
                            break false;
                        }
                    }
                };

                if attr_ok {
                    let (vis, vis_explicit) = self.parse_visibility_modifier();
                    match self.parse_function_def(attrs, vis, vis_explicit, method_doc) {
                        Ok(func) => {
                            let span = func.span;
                            items.push(Spanned::new(func, span));
                            continue;
                        }
                        Err(e) => {
                            self.errors.push(e);
                        }
                    }
                }

                self.synchronize_with_progress();
                if self.pos == saved_pos {
                    self.advance();
                }
            }

            self.expect(&Token::Dedent)?;
            items
        } else {
            // No colon — blank equip block; default implementations come from the trait.
            self.consume_newline();
            Vec::new()
        };

        let end = self.previous_span();

        Ok(EquipBlock {
            generic_params,
            trait_,
            type_: self_type,
            via_field,
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

        // Parse import names, detecting:
        //  - bare module wildcard `*` (must be the only item: `from X import *`)
        //  - `EnumName.*` glob syntax
        //  - `Y as Z` alias syntax
        let mut names = Vec::new();
        let mut glob_types = Vec::new();
        let mut wildcard = false;

        // Bare `*` as first (and only) token after `import` → module wildcard.
        if self.match_token(&Token::Star) {
            wildcard = true;
            let end = self.previous_span();
            self.consume_newline();
            return Ok(ImportStmt::From {
                path,
                names,
                glob_types,
                wildcard,
                span: start.merge(end),
            });
        }

        let first_name = self.expect_name()?;
        if self.match_token(&Token::Dot) && self.match_token(&Token::Star) {
            glob_types.push(first_name);
        } else {
            let alias = if self.match_keyword(Keyword::As) {
                Some(self.expect_name()?)
            } else {
                None
            };
            names.push(ImportName { name: first_name, alias });
        }

        while self.match_token(&Token::Comma) {
            let name = self.expect_name()?;
            if self.match_token(&Token::Dot) && self.match_token(&Token::Star) {
                glob_types.push(name);
            } else {
                let alias = if self.match_keyword(Keyword::As) {
                    Some(self.expect_name()?)
                } else {
                    None
                };
                names.push(ImportName { name, alias });
            }
        }

        let end = self.previous_span();
        self.consume_newline();

        Ok(ImportStmt::From {
            path,
            names,
            glob_types,
            wildcard,
            span: start.merge(end),
        })
    }

    // ── Type Alias ────────────────────────────────────────────

    fn parse_type_alias(
        &mut self,
        visibility: Visibility,
        explicit_visibility: bool,
    ) -> Result<TypeAlias, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Type)?;
        let name = self.expect_identifier()?;

        let generic_params = self.try_parse_generic_params()?;

        self.expect(&Token::Eq)?;
        let type_ = self.parse_type()?;
        let end = self.previous_span();
        self.consume_newline();

        Ok(TypeAlias {
            name,
            generic_params,
            type_,
            visibility,
            explicit_visibility,
            span: start.merge(end),
        })
    }

    // ── Newtype ───────────────────────────────────────────────

    fn parse_newtype(
        &mut self,
        visibility: Visibility,
        explicit_visibility: bool,
    ) -> Result<NewtypeDef, ParseError> {
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
            visibility,
            explicit_visibility,
            span: start.merge(end),
        })
    }

    // ── Extern ────────────────────────────────────────────────

    fn parse_extern_block(&mut self) -> Result<ExternBlock, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Extern)?;

        let abi = if let Token::StringLiteral(_) = self.peek() {
            if let Token::StringLiteral(s) = self.advance().node {
                Some(Spanned::new(s.as_plain_text(), self.previous_span()))
            } else {
                None
            }
        } else {
            None
        };

        self.expect_block_start()?;

        let is_c_abi = abi.as_ref().map_or(false, |a| a.node == "C");
        let prev_extern_c = self.in_extern_c;
        if is_c_abi { self.in_extern_c = true; }

        let mut items = Vec::new();
        while !self.check(&Token::Dedent) && !self.at_end() {
            if self.check(&Token::Newline) {
                self.advance();
                continue;
            }
            let func = self.parse_function_def(Vec::new(), Visibility::Public, false, None)?;
            let span = func.span;
            items.push(Spanned::new(func, span));
        }

        self.in_extern_c = prev_extern_c;
        self.expect(&Token::Dedent)?;
        let end = self.previous_span();

        Ok(ExternBlock {
            abi,
            items,
            span: start.merge(end),
        })
    }

    // ── Module-level variable (type-first, no keyword) ────────

    /// Returns true if the current token sequence looks like `Type Name =` (module-level var decl).
    /// Scans past the type (including optional `[...]` generic args), then the name, checks for `=`.
    fn looks_like_module_var_decl(&self) -> bool {
        let mut i = 0;
        // First token: identifier (type name) or type keyword
        match self.peek_ahead(i) {
            Token::Identifier(_) => i += 1,
            Token::Keyword(kw) if kw.is_type_keyword() => i += 1,
            _ => return false,
        }
        // Optional generic args: [...]
        if matches!(self.peek_ahead(i), Token::LBracket) {
            i += 1;
            let mut depth = 1usize;
            loop {
                match self.peek_ahead(i) {
                    Token::LBracket => { depth += 1; i += 1; }
                    Token::RBracket => {
                        depth -= 1;
                        i += 1;
                        if depth == 0 { break; }
                    }
                    Token::Eof => return false,
                    _ => { i += 1; }
                }
            }
        }
        // Must be followed by identifier (variable name)
        if !matches!(self.peek_ahead(i), Token::Identifier(_)) {
            return false;
        }
        i += 1;
        // `=` → variable decl; `(` → function def
        matches!(self.peek_ahead(i), Token::Eq)
    }

    /// Parse a module-level variable declaration without a `static` keyword:
    /// `TypeName [generic_args] name = expr`
    fn parse_module_var_decl(
        &mut self,
        visibility: Visibility,
        explicit_visibility: bool,
    ) -> Result<StaticDecl, ParseError> {
        let start = self.peek_span();
        // No `static` keyword was consumed — this is the implicit form.
        self.parse_static_decl_body(start, visibility, explicit_visibility, false)
    }

    // ── Static Declaration ────────────────────────────────────

    fn parse_static_decl(
        &mut self,
        visibility: Visibility,
        explicit_visibility: bool,
    ) -> Result<StaticDecl, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Static)?;
        // The keyword was consumed HERE — that is the one place that knows.
        self.parse_static_decl_body(start, visibility, explicit_visibility, true)
    }

    /// Shared body for module-level and `static` variable declarations:
    /// `type name = expr`
    fn parse_static_decl_body(
        &mut self,
        start: Span,
        visibility: Visibility,
        explicit_visibility: bool,
        explicit_static_kw: bool,
    ) -> Result<StaticDecl, ParseError> {
        let type_ = self.parse_type()?;
        let name = self.expect_identifier()?;
        self.expect(&Token::Eq)?;
        let value = self.parse_expr()?;
        let end = self.previous_span();
        self.consume_newline();
        Ok(StaticDecl {
            visibility,
            explicit_visibility,
            explicit_static_kw,
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
        explicit_visibility: bool,
        doc_comment: Option<String>,
    ) -> Result<Item, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Const)?;

        // Type-first: const int X = 5 or const int factorial(int n):
        let type_ = self.parse_type()?;
        let name = self.expect_identifier()?;

        if self.check(&Token::LParen) {
            // It's a const function
            let func = self.finish_function_def(
                attributes,
                visibility,
                explicit_visibility,
                FunctionQualifiers {
                    is_const: true,
                    ..Default::default()
                },
                type_,
                name,
                doc_comment,
                start,
                false,
                None,
                false,
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
                explicit_visibility,
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
        explicit_visibility: bool,
        doc_comment: Option<String>,
    ) -> Result<FunctionDef, ParseError> {
        let start = self.peek_span();

        // Check for `extern` qualifier (extern function binding)
        // Supports optional ABI tag: `extern "C" int foo() = "symbol"`
        let is_extern = self.match_keyword(Keyword::Extern);
        let extern_abi = if is_extern {
            if let Token::StringLiteral(_) = self.peek() {
                let abi_span = self.peek_span();
                if let Token::StringLiteral(s) = self.advance().node {
                    Some(Spanned::new(s.as_plain_text(), abi_span))
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        // Set extern "C" context for type parsing (enables cstr type)
        let prev_extern_c = self.in_extern_c;
        if extern_abi.as_ref().map(|a| a.node.as_str()) == Some("C") { self.in_extern_c = true; }

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
            } else if self.match_keyword(Keyword::Blocking) {
                qualifiers.is_blocking = true;
            } else if self.match_keyword(Keyword::Noreturn) {
                qualifiers.is_noreturn = true;
            } else {
                break;
            }
        }

        // `extern borrowed T f(...)` — the FFI return value is a non-owned
        // pointer; the caller must clone at the ownership boundary. Only
        // recognised here when `is_extern` is set; everywhere else `borrowed`
        // remains a regular identifier (no keyword reservation breakage).
        let returns_borrowed = if is_extern {
            if let Token::Identifier(sym) = self.peek() {
                if sym.as_str() == "borrowed" {
                    self.advance();
                    true
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        };

        // Parse return type and function name (type-first: `ReturnType name(params):`)
        // Parse potentially comma-separated return types (bare tuple syntax).
        // e.g. `str, int, bool f(...)` desugars to `(str, int, bool) f(...)`.
        let first_type = self.parse_type()?;
        let return_type = if self.check(&Token::Comma) {
            let start = first_type.span;
            let mut types = vec![first_type];
            while self.match_token(&Token::Comma) {
                types.push(self.parse_type()?);
            }
            let end = types.last().unwrap().span;
            Spanned::new(Type::Tuple(types), start.merge(end))
        } else {
            first_type
        };
        // Use expect_name() to allow keywords as function/method names
        // (e.g., `from` in `equip Celsius with From[float]`).
        let name = self.expect_name()?;

        let result = self.finish_function_def(
            attributes, visibility, explicit_visibility, qualifiers, return_type, name, doc_comment, start, is_extern, extern_abi, returns_borrowed,
        );
        self.in_extern_c = prev_extern_c;
        result
    }

    /// Shared suffix for function definition parsing. Handles generic params,
    /// param list, throws, where clause, body, and FunctionDef construction.
    fn finish_function_def(
        &mut self,
        attributes: Vec<Spanned<Attribute>>,
        visibility: Visibility,
        explicit_visibility: bool,
        qualifiers: FunctionQualifiers,
        return_type: Spanned<Type>,
        name: Spanned<String>,
        doc_comment: Option<String>,
        start: Span,
        is_extern: bool,
        extern_abi: Option<Spanned<String>>,
        returns_borrowed: bool,
    ) -> Result<FunctionDef, ParseError> {
        let generic_params = self.try_parse_generic_params()?;

        // Parse parameters
        self.expect(&Token::LParen)?;
        let params = self.parse_param_list()?;
        self.expect(&Token::RParen)?;

        // Parse throws clause (D29).
        //   `throws E` — explicit error contract (the v1 spelling; unchanged).
        //   `!`        — bare `!` before the body (`int f()!:`) is A31's
        //                inferred-error-set signature spelling. It PARSES so the
        //                grammar locks now; the checker teaching-rejects it until
        //                A31 lands. Carried as `ThrowsSpec::Inferred` — a typed
        //                axis, NOT a `Type::Named("!inferred")` sentinel.
        //   `! E`      — sigil+type does NOT exist (cancelled 2026-07-16); reject.
        let throws = if self.match_keyword(Keyword::Throws) {
            ThrowsSpec::Explicit(self.parse_type()?)
        } else if self.match_token(&Token::Bang) {
            let bang_span = self.previous_span();
            if self.check(&Token::Colon) || self.check(&Token::Eq) || self.check(&Token::Newline) {
                ThrowsSpec::Inferred(bang_span)
            } else {
                return Err(self.error_at(
                    self.peek_span(),
                    "`! Type` is not a signature form — write `throws E` for an explicit error contract (bare `!` is reserved for A31 inferred error sets)",
                ));
            }
        } else {
            ThrowsSpec::No
        };



        // Parse body
        let body = if is_extern {
            // Extern function: expect `= "c_symbol_name"`
            self.expect(&Token::Eq)?;
            let sym_span = self.peek_span();
            if let Token::StringLiteral(s) = self.advance().node {
                self.consume_newline();
                FunctionBody::Extern(Spanned::new(s.as_plain_text(), sym_span))
            } else {
                return Err(self.error_unexpected("string literal for extern symbol"));
            }
        } else if self.match_token(&Token::Colon) {
            if self.check(&Token::Newline) {
                let start = self.previous_span();
                FunctionBody::Block(self.parse_block_body(start)?)
            } else {
                // Same line → expression body
                let expr = self.parse_expr()?;
                self.consume_newline();
                FunctionBody::Expression(Box::new(expr))
            }
        } else {
            self.consume_newline();
            FunctionBody::Declaration
        };

        let end = self.previous_span();

        Ok(FunctionDef {
            attributes,
            visibility,
            explicit_visibility,
            qualifiers,
            return_type,
            name,
            generic_params,
            params,
            throws,
            body,
            doc_comment,
            span: start.merge(end),
            param_abis: vec![],
            extern_abi,
            returns_borrowed,
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
            // Trailing comma: `f(int a, int b,)` — stop before `)`
            if self.check(&Token::RParen) {
                break;
            }
        }

        Ok(params)
    }

    fn parse_param(&mut self) -> Result<Spanned<Param>, ParseError> {
        let start = self.peek_span();

        // meta op parameter: `meta name` — no type, carries only an operator token at call site.
        if self.check_keyword(Keyword::Meta) {
            let meta_span = self.peek_span();
            self.advance(); // consume `meta`
            let name = self.expect_identifier()?;
            let end = self.previous_span();
            return Ok(Spanned::new(
                Param {
                    type_: Spanned::new(Type::Primitive(PrimitiveType::Void), meta_span),
                    ownership: Ownership::Borrow,
                    name,
                    default: None,
                    is_meta_op: true,
                    is_receiver: false,
                },
                start.merge(end),
            ));
        }

        // Handle self parameter: self, &self, !self
        if self.check_keyword(Keyword::SelfLower) {
            let name_tok = self.advance();
            return Ok(make_self_param(start, name_tok.span, Ownership::Borrow));
        }
        if self.check(&Token::Ampersand)
            && matches!(self.peek_ahead(1), Token::Keyword(Keyword::SelfLower))
        {
            self.advance(); // skip &
            let name_tok = self.advance(); // self
            return Ok(make_self_param(start, name_tok.span, Ownership::MutableBorrow));
        }
        // D27 accept-both: `!self` (retired) and `^self` (canonical) both mean
        // `Ownership::Move` on the self-face.
        if (self.check(&Token::Bang) || self.check(&Token::Caret))
            && matches!(self.peek_ahead(1), Token::Keyword(Keyword::SelfLower))
        {
            self.advance(); // skip ! or ^
            let name_tok = self.advance(); // self
            return Ok(make_self_param(start, name_tok.span, Ownership::Move));
        }

        // Type-first: type [&|!]name
        let type_ = self.parse_type()?;
        let ownership = self.parse_ownership_modifier();
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
                is_meta_op: false,
                // A regular param — including a `Self`-TYPED one such as
                // `int get(Self a)`, which is NOT the receiver and keeps
                // its user-written name.
                is_receiver: false,
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

            if self.match_keyword(Keyword::Const) {
                let type_ = self.parse_type()?;
                let name = self.expect_identifier()?;
                let end = self.previous_span();
                params.push(Spanned::new(
                    GenericParam::Const { type_, name },
                    param_start.merge(end),
                ));
            } else {
                // Try to parse inline trait bounds: `Trait [& Trait]* ParamName`
                // Use try_parse to speculatively attempt this form.
                let inline_bounds = self.try_parse(|p| {
                    // Parse one or more trait bounds separated by `&`
                    let first = p.parse_single_trait_bound().ok()?;
                    let mut bounds = vec![first];
                    while p.match_token(&Token::Ampersand) {
                        bounds.push(p.parse_single_trait_bound().ok()?);
                    }
                    // Next must be an identifier (the param name) — not `,` or `]`
                    if matches!(p.peek(), Token::Identifier(_)) {
                        Some(bounds)
                    } else {
                        None
                    }
                });
                let bounds = inline_bounds.unwrap_or_default();
                let name = self.expect_identifier()?;
                let end = self.previous_span();
                params.push(Spanned::new(
                    GenericParam::Type { name, bounds },
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

    /// Speculatively attempt a parse. If the closure returns `None`, the parser
    /// position is restored to where it was before the attempt.
    ///
    /// Note: only `pos` is saved/restored — `errors` are retained even on
    /// backtrack. This is intentional: speculative paths rarely push errors,
    /// and any that do are acceptable diagnostics.
    fn try_parse<F, T>(&mut self, f: F) -> Option<T>
    where
        F: FnOnce(&mut Self) -> Option<T>,
    {
        let saved_pos = self.pos;
        match f(self) {
            Some(result) => Some(result),
            None => {
                self.pos = saved_pos;
                None
            }
        }
    }

    /// Parse optional generic parameters (`[T, U]`). Returns `None` if not present.
    fn try_parse_generic_params(&mut self) -> Result<Option<Spanned<GenericParams>>, ParseError> {
        if self.check(&Token::LBracket) {
            Ok(Some(self.parse_generic_params()?))
        } else {
            Ok(None)
        }
    }

    /// Collect consecutive doc comments into a single string, or `None` if no doc comments present.
    fn collect_doc_comment(&mut self) -> Option<String> {
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
        doc_comment
    }


    // ── Test Definition ────────────────────────────────────────

    fn parse_test_def(
        &mut self,
        attributes: Vec<Spanned<Attribute>>,
        doc_comment: Option<String>,
    ) -> Result<TestDef, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Test)?;

        let name = self.expect_plain_string()?;

        let body = self.parse_block()?;
        let end = self.previous_span();

        Ok(TestDef {
            attributes,
            name,
            body,
            doc_comment,
            span: start.merge(end),
        })
    }

    // ── Bench Block ──────────────────────────────────────────

    fn parse_bench_def(
        &mut self,
        attributes: Vec<Spanned<Attribute>>,
        doc_comment: Option<String>,
    ) -> Result<BenchDef, ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Bench)?;

        let name = self.expect_plain_string()?;

        let body = self.parse_block()?;
        let end = self.previous_span();

        Ok(BenchDef {
            attributes,
            name,
            body,
            doc_comment,
            span: start.merge(end),
        })
    }

    // ── Suite Block ──────────────────────────────────────────

    fn parse_suite_block(&mut self) -> Result<(Item, Span), ParseError> {
        let start = self.peek_span();
        self.expect_keyword(Keyword::Suite)?;

        let ident = self.expect_identifier()?;
        match ident.node.as_str() {
            "setup" => {
                let body = self.parse_block()?;
                let end = self.previous_span();
                let span = start.merge(end);
                Ok((Item::SuiteSetup(SuiteSetup { body, span }), span))
            }
            "teardown" => {
                let body = self.parse_block()?;
                let end = self.previous_span();
                let span = start.merge(end);
                Ok((Item::SuiteTeardown(SuiteTeardown { body, span }), span))
            }
            _ => Err(self.error_at(ident.span, "expected 'setup' or 'teardown' after 'suite'")),
        }
    }

    /// Consume a string literal that must be plain (no interpolations).
    fn expect_plain_string(&mut self) -> Result<Spanned<String>, ParseError> {
        if let Token::StringLiteral(s) = self.peek() {
            if s.has_interpolation() {
                return Err(self.error_at(self.peek_span(), "test name must be a plain string (no interpolations)"));
            }
            let text = s.as_plain_text();
            let span = self.peek_span();
            self.advance();
            Ok(Spanned::new(text, span))
        } else {
            Err(self.error_unexpected("string literal"))
        }
    }

    pub fn parse_trait_bound_list(&mut self) -> Result<Vec<Spanned<TraitBound>>, ParseError> {
        let mut bounds = Vec::new();
        bounds.push(self.parse_single_trait_bound()?);
        while self.match_token(&Token::Ampersand) {
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
mod tests;
