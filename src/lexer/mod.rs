pub mod token;

use crate::errors::{LexError, LexErrorKind};
use crate::span::{Span, Spanned};
use logos::Logos;
use std::collections::VecDeque;
use token::{Keyword, RawToken, StringKind, StringLiteral, StringSegment, Token};

/// Indentation-aware lexer for Gorget source code.
///
/// Wraps the logos-based raw tokenizer and emits synthetic INDENT/DEDENT/NEWLINE
/// tokens based on indentation changes. Also handles string literals (including
/// interpolation), char literals, comments, and doc comments.
pub struct Lexer<'src> {
    source: &'src str,
    /// Current byte offset in source.
    pos: usize,
    /// Base offset added to all spans for cross-module uniqueness.
    base_offset: usize,
    /// Stack of indentation levels (in spaces). Starts with [0].
    indent_stack: Vec<usize>,
    /// Nesting depth of ( [ { — suppresses NEWLINE/INDENT/DEDENT when > 0.
    bracket_depth: usize,
    /// Buffered tokens to emit before scanning the next line.
    pending: VecDeque<Spanned<Token>>,
    /// Whether we've finished scanning all input.
    finished: bool,
    /// Whether we need to emit a NEWLINE before the next line's tokens.
    need_newline: bool,
    /// Collected errors.
    pub errors: Vec<LexError>,
}

/// What kind of literal we're parsing escape sequences for. Encodes the
/// DELIMITER concern only — which quote/brace escapes are legal in this
/// context. Byte-ness (the `\xHH` range) is a SEPARATE, orthogonal axis
/// (`EscapeByteness`) so that byte literals keep their `\'`/`\\` delimiter
/// escapes while widening only the hex range.
enum EscapeContext {
    /// String literal: allows `\"`, `\{`, `\}`.
    String,
    /// Single-quoted literal: allows `\'`.
    SingleQuoted,
}

/// The byte-ness of an escape context — controls the `\xHH` hex-escape range,
/// ORTHOGONAL to `EscapeContext`'s delimiter concern (Layering rule 2: typed
/// metadata, chosen from context, never a name/shape heuristic).
#[derive(Clone, Copy, PartialEq, Eq)]
enum EscapeByteness {
    /// UTF-8 text context (`"..."`, `f"..."`, `"""..."""`, `c"..."`, `'x'`,
    /// and — for now — `b"..."`): `\xHH` encodes only ASCII `0x00-0x7F`; a
    /// value `> 0x7F` is an error directing the user to `\u{00HH}`. Because
    /// `parse_escape` returns a `char`, a byte `>= 0x80` cannot round-trip as
    /// a raw byte here anyway (`\xFF` -> U+00FF is two UTF-8 bytes).
    Ascii,
    /// Byte-literal context (`b'\xFF'`): `\xHH` encodes the full byte range
    /// `0x00-0xFF`. `scan_byte_literal` immediately narrows the returned
    /// `char` scalar (`ch as u32 <= 255`) to a `u8`, so U+00FF -> 255 is the
    /// intended raw byte with no UTF-8 buffer path.
    Byte,
}

/// Result of parsing a single escape sequence.
enum EscapeResult {
    /// Successfully resolved escape character.
    Char(char),
    /// EOF encountered after backslash.
    Eof,
}

/// Classify a byte as a string prefix, given the byte that follows it.
/// Returns `Some(true)` for prefixes that start a string literal (route to `scan_string_literal`),
/// `Some(false)` for `b'` which starts a byte literal (route to `scan_byte_literal`),
/// or `None` if the byte is not a string/byte-literal prefix.
fn string_prefix_kind(byte: u8, next: u8) -> Option<bool> {
    match byte {
        b'r' | b'c' if next == b'"' => Some(true),
        b'b' if next == b'"' => Some(true),
        b'b' if next == b'\'' => Some(false), // byte literal
        b'f' if next == b'"' || next == b'\'' => Some(true),
        _ => None,
    }
}

/// Returns true if `bytes[i]` starts a string/byte-literal prefix sequence.
fn is_string_prefix(bytes: &[u8], i: usize) -> bool {
    i + 1 < bytes.len() && string_prefix_kind(bytes[i], bytes[i + 1]).is_some()
}

/// Split a `FloatLiteral` slice into its two tuple-index halves when it is
/// actually nested tuple-field access (`D.D`, no exponent).
///
/// Returns `(left, right, dot_byte_offset)` where both halves are pure
/// digit/underscore runs. Returns `None` for exponent-bearing floats (`1.0e5`),
/// which must stay floats — the caller only invokes this when the float follows
/// a `Dot`, and `expr.1.0e5` is not meaningful tuple access. Splitting only the
/// digit-pair case keeps every real float literal untouched.
fn split_tuple_float(slice: &str) -> Option<(&str, &str, usize)> {
    let dot = slice.find('.')?;
    let left = &slice[..dot];
    let right = &slice[dot + 1..];
    let is_index = |s: &str| !s.is_empty() && s.bytes().all(|b| b.is_ascii_digit() || b == b'_');
    if is_index(left) && is_index(right) {
        Some((left, right, dot))
    } else {
        None
    }
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self::new_with_offset(source, 0)
    }

    pub fn new_with_offset(source: &'src str, base_offset: usize) -> Self {
        Self {
            source,
            pos: 0,
            base_offset,
            indent_stack: vec![0],
            bracket_depth: 0,
            pending: VecDeque::new(),
            finished: false,
            need_newline: false,
            errors: Vec::new(),
        }
    }

    /// Create a `Span` with the base offset applied.
    fn span(&self, start: usize, end: usize) -> Span {
        Span::new(self.base_offset + start, self.base_offset + end)
    }

    /// The byte offset just past the end of this lexer's source.
    pub fn end_offset(&self) -> usize {
        self.base_offset + self.source.len()
    }

    /// Peek at the character at byte offset `pos`.
    fn char_at(&self, byte_pos: usize) -> Option<char> {
        self.source[byte_pos..].chars().next()
    }

    /// Scan the next line from `self.pos`, processing indentation,
    /// comments, strings, and emitting tokens into `self.pending`.
    fn scan_next_line(&mut self) {
        if self.pos >= self.source.len() {
            self.emit_eof();
            return;
        }

        // Skip blank lines and comment-only lines
        loop {
            if self.pos >= self.source.len() {
                self.emit_eof();
                return;
            }

            let line_start = self.pos;
            let indent_spaces = self.count_leading_spaces();
            let after_indent = self.pos;

            // Check what follows the indentation
            match self.char_at(self.pos) {
                None => {
                    // End of file
                    self.emit_eof();
                    return;
                }
                Some('\n') => {
                    // Blank line — skip
                    self.pos += 1;
                    continue;
                }
                Some('#') => {
                    // Check for doc comment
                    let is_doc = self.pos + 1 < self.source.len()
                        && self.source.as_bytes()[self.pos + 1] == b'/';
                    self.emit_comment_token(line_start, is_doc);
                    continue;
                }
                Some('.') if self.bracket_depth == 0 && self.need_newline => {
                    // Leading-dot continuation — do NOT emit NEWLINE or change indent
                    // Just tokenize this line as continuation of previous expression
                    self.tokenize_line_content(after_indent);
                    return;
                }
                _ => {
                    // Non-blank, non-comment line — process indentation
                    if self.bracket_depth == 0 {
                        self.process_indentation(indent_spaces, line_start);
                    }
                    self.tokenize_line_content(after_indent);
                    return;
                }
            }
        }
    }

    /// Count leading spaces from current position. Advances `self.pos`.
    fn count_leading_spaces(&mut self) -> usize {
        let start = self.pos;
        while self.pos < self.source.len() {
            match self.source.as_bytes()[self.pos] {
                b' ' => self.pos += 1,
                b'\t' => {
                    self.errors.push(LexError {
                        kind: LexErrorKind::TabCharacter,
                        span: self.span(self.pos, self.pos + 1),
                    });
                    self.pos += 1;
                }
                _ => break,
            }
        }
        self.pos - start
    }

    /// Emit a pending NEWLINE token if one is needed.
    fn emit_pending_newline(&mut self, line_start: usize) {
        if self.need_newline {
            self.pending.push_back(Spanned::new(
                Token::Newline,
                self.span(line_start, line_start),
            ));
        }
    }

    /// Process indentation changes, emitting INDENT/DEDENT/NEWLINE tokens.
    fn process_indentation(&mut self, spaces: usize, line_start: usize) {
        let current = *self.indent_stack.last().unwrap();

        if spaces == current {
            // Same indentation level
            self.emit_pending_newline(line_start);
        } else if spaces > current {
            // Indentation increased — any amount is valid (Python-style)
            self.emit_pending_newline(line_start);
            self.indent_stack.push(spaces);
            self.pending.push_back(Spanned::new(
                Token::Indent,
                self.span(line_start, line_start + spaces),
            ));
        } else {
            // Indentation decreased — emit DEDENT(s)
            self.emit_pending_newline(line_start);
            while *self.indent_stack.last().unwrap() > spaces {
                self.indent_stack.pop();
                self.pending.push_back(Spanned::new(
                    Token::Dedent,
                    self.span(line_start, line_start + spaces),
                ));
            }
            if *self.indent_stack.last().unwrap() != spaces {
                self.errors.push(LexError {
                    kind: LexErrorKind::IndentationMismatch { got: spaces },
                    span: self.span(line_start, line_start + spaces),
                });
            }
        }
        self.need_newline = true;
    }

    /// Emit EOF: flush remaining DEDENTs and the EOF token.
    fn emit_eof(&mut self) {
        if self.finished {
            return;
        }
        self.finished = true;

        let eof_pos = self.source.len();

        if self.need_newline {
            self.pending.push_back(Spanned::new(
                Token::Newline,
                self.span(eof_pos, eof_pos),
            ));
        }

        // Pop all indentation levels back to 0
        while self.indent_stack.len() > 1 {
            self.indent_stack.pop();
            self.pending.push_back(Spanned::new(
                Token::Dedent,
                self.span(eof_pos, eof_pos),
            ));
        }

        self.pending
            .push_back(Spanned::new(Token::Eof, self.span(eof_pos, eof_pos)));
    }

    /// Tokenize the content of a line (after indentation has been handled).
    /// Scans character-by-character, handling strings and chars directly,
    /// and feeding non-string segments to logos.
    fn tokenize_line_content(&mut self, start_pos: usize) {
        let bytes = self.source.as_bytes();
        let mut i = start_pos;

        while i < bytes.len() {
            // Skip whitespace
            while i < bytes.len() && (bytes[i] == b' ' || bytes[i] == b'\t') {
                i += 1;
            }
            if i >= bytes.len() || bytes[i] == b'\n' {
                break;
            }

            match bytes[i] {
                // Comment — emit as token then stop processing line
                b'#' => {
                    let comment_start = i;
                    while i < bytes.len() && bytes[i] != b'\n' {
                        i += 1;
                    }
                    let content = self.source[comment_start..i].to_string();
                    self.pending.push_back(Spanned::new(
                        Token::Comment(content),
                        self.span(comment_start, i),
                    ));
                    break;
                }

                // String literals
                b'"' => {
                    let (tok, new_end) = self.scan_string_literal(i);
                    let span = self.span(i, new_end);
                    self.pending.push_back(Spanned::new(tok, span));
                    i = new_end;
                }

                // Prefixed string/byte literals: r"...", b"...", b'X', c"...", f"...", f'...'
                b'r' | b'b' | b'c' | b'f' if i + 1 < bytes.len()
                    && string_prefix_kind(bytes[i], bytes[i + 1]).is_some() =>
                {
                    let is_string = string_prefix_kind(bytes[i], bytes[i + 1]).unwrap();
                    let (tok, new_end) = if is_string {
                        self.scan_string_literal(i)
                    } else {
                        self.scan_byte_literal(i)
                    };
                    let span = self.span(i, new_end);
                    self.pending.push_back(Spanned::new(tok, span));
                    i = new_end;
                }

                // Single-quoted string/char literal
                b'\'' => {
                    let (tok, new_end) = self.scan_char_literal(i);
                    let span = self.span(i, new_end);
                    self.pending.push_back(Spanned::new(tok, span));
                    i = new_end;
                }

                // Everything else — find the next string/char/comment/newline boundary
                // and feed that segment to logos
                _ => {
                    let seg_start = i;
                    while i < bytes.len()
                        && bytes[i] != b'\n'
                        && bytes[i] != b'"'
                        && bytes[i] != b'\''
                        && bytes[i] != b'#'
                        && !is_string_prefix(bytes, i)
                    {
                        i += 1;
                    }
                    // Feed this segment to logos
                    let segment = &self.source[seg_start..i];
                    let mut lex = RawToken::lexer(segment);
                    while let Some(raw_result) = lex.next() {
                        let slice = lex.slice();
                        let span_start = seg_start + lex.span().start;
                        let span_end = seg_start + lex.span().end;
                        let span = self.span(span_start, span_end);
                        match raw_result {
                            Ok(raw) => {
                                // Tuple-index disambiguation: a `FloatLiteral` lexed
                                // immediately after a `Dot` is never a real float — it is
                                // nested tuple-field access (`tup.1.0` → `tup . 1 . 0`).
                                // `D.D` greedily matches the FloatLiteral regex, so split it
                                // back into `Int Dot Int`. This composes across the Pratt
                                // postfix loop: after splitting, the trailing `Int` lets a
                                // following `.N` chain (`a.1.2.3`) keep disambiguating.
                                if raw == RawToken::FloatLiteral
                                    && matches!(
                                        self.pending.back().map(|t| &t.node),
                                        Some(Token::Dot)
                                    )
                                {
                                    if let Some((left, right, dot_off)) =
                                        split_tuple_float(slice)
                                    {
                                        let left_span =
                                            self.span(span_start, span_start + dot_off);
                                        let dot_span = self.span(
                                            span_start + dot_off,
                                            span_start + dot_off + 1,
                                        );
                                        let right_span =
                                            self.span(span_start + dot_off + 1, span_end);
                                        let left_tok =
                                            self.parse_int_literal(left, 0, 10, left_span);
                                        let right_tok =
                                            self.parse_int_literal(right, 0, 10, right_span);
                                        self.pending
                                            .push_back(Spanned::new(left_tok, left_span));
                                        self.pending
                                            .push_back(Spanned::new(Token::Dot, dot_span));
                                        self.pending
                                            .push_back(Spanned::new(right_tok, right_span));
                                        continue;
                                    }
                                }
                                if let Some(tok) = self.convert_raw_token(raw, slice, span) {
                                    match &tok {
                                        Token::LParen | Token::LBracket | Token::LBrace => {
                                            self.bracket_depth += 1;
                                        }
                                        Token::RParen | Token::RBracket | Token::RBrace => {
                                            self.bracket_depth = self.bracket_depth.saturating_sub(1);
                                        }
                                        _ => {}
                                    }
                                    self.pending.push_back(Spanned::new(tok, span));
                                }
                            }
                            Err(()) => {
                                let ch = self.source[span_start..].chars().next();
                                if let Some(c) = ch {
                                    self.errors.push(LexError {
                                        kind: LexErrorKind::InvalidCharacter(c),
                                        span,
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }

        // Advance past the line
        self.pos = i;
        if self.pos < self.source.len() && bytes[self.pos] == b'\n' {
            self.pos += 1;
        }
    }

    /// Parse an integer literal with the given radix and prefix length.
    fn parse_int_literal(&mut self, slice: &str, prefix_len: usize, radix: u32, span: Span) -> Token {
        let clean: String = slice[prefix_len..].chars().filter(|c| *c != '_').collect();
        match i64::from_str_radix(&clean, radix) {
            Ok(n) => Token::IntLiteral(n),
            Err(_) => {
                self.errors.push(LexError {
                    kind: LexErrorKind::InvalidNumericLiteral(slice.to_string()),
                    span,
                });
                Token::Error
            }
        }
    }

    /// Convert a raw token + its slice into a final Token.
    fn convert_raw_token(&mut self, raw: RawToken, slice: &str, span: Span) -> Option<Token> {
        Some(match raw {
            RawToken::IntLiteral => self.parse_int_literal(slice, 0, 10, span),
            RawToken::HexLiteral => self.parse_int_literal(slice, 2, 16, span),
            RawToken::OctalLiteral => self.parse_int_literal(slice, 2, 8, span),
            RawToken::BinaryLiteral => self.parse_int_literal(slice, 2, 2, span),
            RawToken::FloatLiteral => {
                let clean: String = slice.chars().filter(|c| *c != '_').collect();
                match clean.parse::<f64>() {
                    Ok(n) => Token::FloatLiteral(n),
                    Err(_) => {
                        self.errors.push(LexError {
                            kind: LexErrorKind::InvalidNumericLiteral(slice.to_string()),
                            span,
                        });
                        Token::Error
                    }
                }
            }
            RawToken::Identifier | RawToken::Underscore => {
                if let Some(kw) = Keyword::from_str(slice) {
                    Token::Keyword(kw)
                } else if slice == "_" {
                    Token::Underscore
                } else {
                    Token::Identifier(crate::intern::intern(slice))
                }
            }
            RawToken::Newline => {
                // Newlines within a line content shouldn't happen
                // (we split by lines), but handle gracefully
                return None;
            }
            // Operators — direct mapping
            RawToken::Plus => Token::Plus,
            RawToken::Minus => Token::Minus,
            RawToken::Star => Token::Star,
            RawToken::Slash => Token::Slash,
            RawToken::Percent => Token::Percent,
            RawToken::Eq => Token::Eq,
            RawToken::Lt => Token::Lt,
            RawToken::Gt => Token::Gt,
            RawToken::Bang => Token::Bang,
            RawToken::Ampersand => Token::Ampersand,
            RawToken::Pipe => Token::Pipe,
            RawToken::Dot => Token::Dot,
            RawToken::Question => Token::Question,
            RawToken::At => Token::At,
            RawToken::LParen => Token::LParen,
            RawToken::RParen => Token::RParen,
            RawToken::LBracket => Token::LBracket,
            RawToken::RBracket => Token::RBracket,
            RawToken::LBrace => Token::LBrace,
            RawToken::RBrace => Token::RBrace,
            RawToken::Colon => Token::Colon,
            RawToken::Comma => Token::Comma,
            RawToken::EqEq => Token::EqEq,
            RawToken::BangEq => Token::BangEq,
            RawToken::LtEq => Token::LtEq,
            RawToken::GtEq => Token::GtEq,
            RawToken::PlusEq => Token::PlusEq,
            RawToken::Arrow => Token::Arrow,
            RawToken::MinusEq => Token::MinusEq,
            RawToken::StarEq => Token::StarEq,
            RawToken::StarStar => Token::StarStar,
            RawToken::StarStarEq => Token::StarStarEq,
            RawToken::SlashEq => Token::SlashEq,
            RawToken::PercentEq => Token::PercentEq,
            RawToken::PlusPercent => Token::PlusPercent,
            RawToken::MinusPercent => Token::MinusPercent,
            RawToken::StarPercent => Token::StarPercent,
            RawToken::PlusPercentEq => Token::PlusPercentEq,
            RawToken::MinusPercentEq => Token::MinusPercentEq,
            RawToken::StarPercentEq => Token::StarPercentEq,
            RawToken::PlusBang => Token::PlusBang,
            RawToken::MinusBang => Token::MinusBang,
            RawToken::StarBang => Token::StarBang,
            RawToken::SlashBang => Token::SlashBang,
            RawToken::PercentBang => Token::PercentBang,
            RawToken::LtLtBang => Token::LtLtBang,
            RawToken::GtGtBang => Token::GtGtBang,
            RawToken::PlusBangEq => Token::PlusBangEq,
            RawToken::MinusBangEq => Token::MinusBangEq,
            RawToken::StarBangEq => Token::StarBangEq,
            RawToken::SlashBangEq => Token::SlashBangEq,
            RawToken::PercentBangEq => Token::PercentBangEq,
            RawToken::LtLtBangEq => Token::LtLtBangEq,
            RawToken::GtGtBangEq => Token::GtGtBangEq,
            RawToken::LtLt => Token::LtLt,
            RawToken::GtGt => Token::GtGt,
            RawToken::LtLtEq => Token::LtLtEq,
            RawToken::GtGtEq => Token::GtGtEq,
            RawToken::AmpersandEq => Token::AmpersandEq,
            RawToken::PipeEq => Token::PipeEq,
            RawToken::CaretEq => Token::CaretEq,
            RawToken::Caret => Token::Caret,
            RawToken::Tilde => Token::Tilde,
            RawToken::DotDot => Token::DotDot,
            RawToken::DotDotEq => Token::DotDotEq,
            RawToken::QuestionDot => Token::QuestionDot,
            RawToken::DoubleQuestion => Token::DoubleQuestion,
        })
    }

    /// Parse a single escape sequence at `bytes[*i]` (the byte after `\`).
    /// Advances `*i` past the consumed bytes. Pushes errors on invalid sequences.
    fn parse_escape(
        &mut self,
        bytes: &[u8],
        i: &mut usize,
        context: EscapeContext,
        byteness: EscapeByteness,
    ) -> EscapeResult {
        if *i >= bytes.len() {
            return EscapeResult::Eof;
        }
        let backslash_pos = *i - 1;
        let ch = match bytes[*i] {
            b'n' => { *i += 1; '\n' }
            b't' => { *i += 1; '\t' }
            b'r' => { *i += 1; '\r' }
            b'\\' => { *i += 1; '\\' }
            b'0' => { *i += 1; '\0' }
            b'"' if matches!(context, EscapeContext::String) => { *i += 1; '"' }
            b'\'' if matches!(context, EscapeContext::SingleQuoted) => { *i += 1; '\'' }
            b'{' if matches!(context, EscapeContext::String) => { *i += 1; '{' }
            b'}' if matches!(context, EscapeContext::String) => { *i += 1; '}' }
            b'u' if *i + 1 < bytes.len() && bytes[*i + 1] == b'{' => {
                *i += 2; // skip u{
                let hex_start = *i;
                while *i < bytes.len() && bytes[*i] != b'}' {
                    *i += 1;
                }
                let hex = &self.source[hex_start..*i];
                if *i < bytes.len() {
                    *i += 1; // skip }
                }
                u32::from_str_radix(hex, 16)
                    .ok()
                    .and_then(char::from_u32)
                    .unwrap_or_else(|| {
                        self.errors.push(LexError {
                            kind: LexErrorKind::InvalidEscapeSequence(
                                format!("\\u{{{hex}}}"),
                            ),
                            span: self.span(backslash_pos, *i),
                        });
                        '\u{FFFD}'
                    })
            }
            // \uXXXX — 4-hex-digit BMP codepoint (JS/Rust/Java shape).
            // Supplementary planes (codepoints > U+FFFF) require \u{...} form;
            // surrogate-pair encoding via \uD8xx\uDCxx is not supported.
            b'u' => {
                *i += 1; // skip u
                let hex_start = *i;
                let hex_end = (*i + 4).min(bytes.len());
                let mut all_hex = true;
                let mut count = 0;
                while *i < hex_end && bytes[*i].is_ascii_hexdigit() {
                    *i += 1;
                    count += 1;
                }
                if count != 4 {
                    all_hex = false;
                }
                if !all_hex {
                    self.errors.push(LexError {
                        kind: LexErrorKind::InvalidEscapeSequence(
                            "\\u requires 4 hex digits (e.g. \\u0041) or \\u{...} form"
                                .to_string(),
                        ),
                        span: self.span(backslash_pos, *i),
                    });
                    '\u{FFFD}'
                } else {
                    let hex = &self.source[hex_start..*i];
                    u32::from_str_radix(hex, 16)
                        .ok()
                        .and_then(char::from_u32)
                        .unwrap_or_else(|| {
                            // Lone surrogate (U+D800..U+DFFF) — invalid in UTF-8.
                            self.errors.push(LexError {
                                kind: LexErrorKind::InvalidEscapeSequence(
                                    format!("\\u{hex} (lone surrogate not allowed)"),
                                ),
                                span: self.span(backslash_pos, *i),
                            });
                            '\u{FFFD}'
                        })
                }
            }
            // \xHH — hex byte escape (Rust model). Exactly 2 hex digits.
            // In UTF-8 text contexts (`EscapeByteness::Ascii`) the value must
            // be ASCII (`<= 0x7F`); `> 0x7F` is rejected with a pointer to the
            // `\u{00HH}` form (a raw byte >= 0x80 cannot round-trip through the
            // returned `char`). In byte-literal context (`EscapeByteness::Byte`)
            // the full `0x00-0xFF` range is accepted. `\u{...}` / `\uXXXX` are
            // SEPARATE arms above and stay untouched.
            b'x' => {
                *i += 1; // skip x
                let hex_start = *i;
                let hex_end = (*i + 2).min(bytes.len());
                while *i < hex_end && bytes[*i].is_ascii_hexdigit() {
                    *i += 1;
                }
                if *i - hex_start != 2 {
                    self.errors.push(LexError {
                        kind: LexErrorKind::InvalidEscapeSequence(
                            "\\x requires exactly 2 hex digits (e.g. \\x41)".to_string(),
                        ),
                        span: self.span(backslash_pos, *i),
                    });
                    '\u{FFFD}'
                } else {
                    let hex = &self.source[hex_start..*i];
                    // Two hex digits always parse to 0..=255.
                    let val = u32::from_str_radix(hex, 16).unwrap();
                    if byteness == EscapeByteness::Ascii && val > 0x7F {
                        self.errors.push(LexError {
                            kind: LexErrorKind::InvalidEscapeSequence(
                                format!(
                                    "\\x{hex}: \\x only encodes ASCII (<=0x7F); use \\u{{00{hex}}} for U+00{hex}"
                                ),
                            ),
                            span: self.span(backslash_pos, *i),
                        });
                        '\u{FFFD}'
                    } else {
                        // val <= 0xFF is always a valid scalar (U+0000..U+00FF).
                        char::from_u32(val).unwrap()
                    }
                }
            }
            other => {
                self.errors.push(LexError {
                    kind: LexErrorKind::InvalidEscapeSequence(
                        format!("\\{}", other as char),
                    ),
                    span: self.span(backslash_pos, *i + 1),
                });
                *i += 1;
                other as char
            }
        };
        EscapeResult::Char(ch)
    }

    /// Scan a string literal starting at `pos`. Returns (Token, end_position).
    fn scan_string_literal(&mut self, pos: usize) -> (Token, usize) {
        let bytes = self.source.as_bytes();
        let mut i = pos;

        // Detect prefix
        let kind = if i < bytes.len() && bytes[i] == b'r' {
            i += 1; // skip 'r'
            StringKind::Raw
        } else if i < bytes.len() && bytes[i] == b'b' {
            i += 1; // skip 'b'
            StringKind::Byte
        } else if i < bytes.len() && bytes[i] == b'c' {
            i += 1; // skip 'c'
            StringKind::CStr
        } else if i < bytes.len() && bytes[i] == b'f' {
            i += 1; // skip 'f'
            StringKind::Format
        } else {
            StringKind::Normal
        };

        // Expect opening quote (either " or ')
        if i >= bytes.len() || (bytes[i] != b'"' && bytes[i] != b'\'') {
            return (Token::Error, i);
        }
        let quote_char = bytes[i];

        // Check for triple-quote (only for double-quoted Normal strings)
        let triple = quote_char == b'"'
            && i + 2 < bytes.len()
            && bytes[i + 1] == b'"'
            && bytes[i + 2] == b'"';
        let actual_kind = if triple && kind == StringKind::Normal {
            StringKind::MultiLine
        } else {
            kind
        };

        if triple {
            i += 3; // skip """
        } else {
            i += 1; // skip opening quote
        }

        let mut segments: Vec<StringSegment> = Vec::new();
        let mut current_literal = String::new();

        loop {
            if i >= bytes.len() {
                self.errors.push(LexError {
                    kind: LexErrorKind::UnterminatedString,
                    span: self.span(pos, i),
                });
                break;
            }

            if triple {
                if i + 2 < bytes.len()
                    && bytes[i] == b'"'
                    && bytes[i + 1] == b'"'
                    && bytes[i + 2] == b'"'
                {
                    i += 3;
                    break;
                }
            } else if bytes[i] == quote_char {
                i += 1;
                break;
            }

            if bytes[i] == b'\n' && !triple {
                self.errors.push(LexError {
                    kind: LexErrorKind::UnterminatedString,
                    span: self.span(pos, i),
                });
                break;
            }

            // Escape sequences (all non-Raw kinds)
            if bytes[i] == b'\\' && actual_kind != StringKind::Raw {
                i += 1; // skip backslash
                // All string-buffer kinds (Normal / Format / MultiLine / CStr,
                // and byte string `b"..."`) decode into a UTF-8 `String`
                // buffer, so `\xHH` is ASCII-only here. TODO(byte-string):
                // when `b"..."` gains a real `Vec[u8]` value type (today it is
                // typed as a plain `string_id`, `typecheck.rs`), route
                // `StringKind::Byte` through `EscapeByteness::Byte` for the
                // full `0x00-0xFF` range — it needs a byte-buffer path first.
                match self.parse_escape(bytes, &mut i, EscapeContext::String, EscapeByteness::Ascii) {
                    EscapeResult::Char(c) => current_literal.push(c),
                    EscapeResult::Eof => {
                        self.errors.push(LexError {
                            kind: LexErrorKind::UnterminatedString,
                            span: self.span(pos, i),
                        });
                        break;
                    }
                }
                continue;
            }

            // String interpolation: {expr} — only for Format strings
            if bytes[i] == b'{' && actual_kind == StringKind::Format {
                // Check for escaped brace: {{
                if i + 1 < bytes.len() && bytes[i + 1] == b'{' {
                    current_literal.push('{');
                    i += 2;
                    continue;
                }

                // Flush current literal
                if !current_literal.is_empty() {
                    segments.push(StringSegment::Literal(
                        std::mem::take(&mut current_literal),
                    ));
                }

                // Scan interpolation expression
                i += 1; // skip {
                let expr_start = i;
                let mut brace_depth = 1;
                while i < bytes.len() && brace_depth > 0 {
                    match bytes[i] {
                        b'{' => brace_depth += 1,
                        b'}' => brace_depth -= 1,
                        b'"' | b'\'' => {
                            skip_quoted_string(bytes, &mut i);
                            continue;
                        }
                        _ => {}
                    }
                    if brace_depth > 0 {
                        i += 1;
                    }
                }

                if brace_depth > 0 {
                    self.errors.push(LexError {
                        kind: LexErrorKind::UnterminatedInterpolation,
                        span: self.span(expr_start - 1, i),
                    });
                } else {
                    let full_text = self.source[expr_start..i].to_string();
                    let (expr_text, fmt_spec) = split_interpolation_spec(&full_text);
                    segments.push(StringSegment::Interpolation(expr_text, fmt_spec));
                    i += 1; // skip closing }
                }
                continue;
            }

            // Escaped brace: }} — only for Format strings
            if bytes[i] == b'}' && actual_kind == StringKind::Format
                && i + 1 < bytes.len()
                && bytes[i + 1] == b'}'
            {
                current_literal.push('}');
                i += 2;
                continue;
            }

            // Regular character
            let ch = self.source[i..].chars().next().unwrap();
            current_literal.push(ch);
            i += ch.len_utf8();
        }

        if !current_literal.is_empty() {
            segments.push(StringSegment::Literal(current_literal));
        }

        let token = Token::StringLiteral(StringLiteral {
            kind: actual_kind,
            segments,
        });
        (token, i)
    }

    /// Scan a single-quoted literal starting at `pos`. Returns (Token, end_position).
    ///
    /// All single-quoted literals → `Token::StringLiteral(Normal)` (raw str, no interpolation).
    /// `'x'`, `'\n'`, `'hello'`, `''` all produce StringLiteral.
    ///
    /// Note: `f'...'` is routed to `scan_string_literal` instead, and `b'...'` is routed
    /// to `scan_byte_literal`, so this function only sees plain single-quoted strings.
    fn scan_char_literal(&mut self, pos: usize) -> (Token, usize) {
        let bytes = self.source.as_bytes();
        let mut i = pos + 1; // skip opening '

        if i >= bytes.len() || bytes[i] == b'\n' {
            self.errors.push(LexError {
                kind: LexErrorKind::UnterminatedCharLiteral,
                span: self.span(pos, i),
            });
            return (Token::Error, i);
        }

        // Multi-char (or empty `''`) single-quoted string → StringLiteral::Normal (raw str).
        let mut literal = String::new();

        loop {
            if i >= bytes.len() || bytes[i] == b'\n' {
                self.errors.push(LexError {
                    kind: LexErrorKind::UnterminatedString,
                    span: self.span(pos, i),
                });
                break;
            }

            if bytes[i] == b'\'' {
                i += 1; // consume closing '
                break;
            }

            // Single-quoted `'…'` is UTF-8 text: `\xHH` is ASCII-only.
            match self.parse_one_quoted_char(bytes, i, pos, EscapeByteness::Ascii) {
                Ok((ch, new_i)) => {
                    literal.push(ch);
                    i = new_i;
                }
                Err(new_i) => {
                    i = new_i;
                    break;
                }
            }
        }

        let token = Token::StringLiteral(StringLiteral {
            kind: StringKind::Normal,
            segments: vec![StringSegment::Literal(literal)],
        });
        (token, i)
    }

    /// Parse one character (literal or escaped) from a single-quoted context.
    /// Returns `Ok((char, new_pos))` on success, or `Err(new_pos)` on EOF/unterminated.
    ///
    /// Shared by `scan_char_literal` (`'x'`, UTF-8 text — `EscapeByteness::Ascii`)
    /// and `scan_byte_literal` (`b'\xFF'`, raw byte — `EscapeByteness::Byte`).
    /// Both use `EscapeContext::SingleQuoted` so `\'`/`\\` stay legal in each;
    /// byte-ness is threaded ORTHOGONALLY so `b'\''` keeps working while only
    /// the `\xHH` range widens for byte literals.
    fn parse_one_quoted_char(
        &mut self,
        bytes: &[u8],
        i: usize,
        literal_start: usize,
        byteness: EscapeByteness,
    ) -> Result<(char, usize), usize> {
        if bytes[i] == b'\\' {
            let mut j = i + 1;
            match self.parse_escape(bytes, &mut j, EscapeContext::SingleQuoted, byteness) {
                EscapeResult::Char(c) => Ok((c, j)),
                EscapeResult::Eof => {
                    self.errors.push(LexError {
                        kind: LexErrorKind::UnterminatedString,
                        span: self.span(literal_start, j),
                    });
                    Err(j)
                }
            }
        } else {
            let ch = self.source[i..].chars().next().unwrap();
            Ok((ch, i + ch.len_utf8()))
        }
    }

    /// Scan a byte literal `b'A'`, `b'\n'` etc. starting at `pos` (the `b`).
    /// Emits `Token::IntLiteral(byte_value)`.
    fn scan_byte_literal(&mut self, pos: usize) -> (Token, usize) {
        let bytes = self.source.as_bytes();
        // pos points at 'b', pos+1 is '\''
        let i = pos + 2; // skip b and opening '

        if i >= bytes.len() || bytes[i] == b'\n' {
            self.errors.push(LexError {
                kind: LexErrorKind::UnterminatedCharLiteral,
                span: self.span(pos, i),
            });
            return (Token::Error, i);
        }

        // Parse one char or escape sequence. Byte literals take the full
        // `\xHH` range (`0x00-0xFF`); the scalar is narrowed to `u8` below.
        let (ch, after) = match self.parse_one_quoted_char(bytes, i, pos, EscapeByteness::Byte) {
            Ok(result) => result,
            Err(end) => return (Token::Error, end),
        };

        // Validate byte range
        let byte_val = if ch as u32 > u8::MAX as u32 {
            self.errors.push(LexError {
                kind: LexErrorKind::InvalidEscapeSequence("byte literal: escape value > 255".to_string()),
                span: self.span(pos, after),
            });
            return (Token::Error, after);
        } else {
            ch as u8
        };

        // Expect closing '
        if after < bytes.len() && bytes[after] == b'\'' {
            (Token::IntLiteral(byte_val as i64), after + 1)
        } else {
            self.errors.push(LexError {
                kind: LexErrorKind::UnterminatedCharLiteral,
                span: self.span(pos, after),
            });
            (Token::Error, after)
        }
    }

    /// Scan a comment to EOL, emit as a token, and skip the trailing newline.
    fn emit_comment_token(&mut self, start: usize, is_doc: bool) {
        let content = self.scan_to_eol();
        let token = if is_doc { Token::DocComment(content) } else { Token::Comment(content) };
        self.pending.push_back(Spanned::new(token, self.span(start, self.pos)));
        if self.pos < self.source.len() {
            self.pos += 1; // skip \n
        }
    }

    /// Skip to end of line (or EOF) without collecting content.
    fn skip_to_eol(&mut self) {
        while self.pos < self.source.len() && self.source.as_bytes()[self.pos] != b'\n' {
            self.pos += 1;
        }
    }

    /// Scan to end of line, returning the content (including the start position).
    fn scan_to_eol(&mut self) -> String {
        let start = self.pos;
        self.skip_to_eol();
        self.source[start..self.pos].to_string()
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        // Return buffered tokens first
        if let Some(tok) = self.pending.pop_front() {
            if tok.node == Token::Eof {
                return None;
            }
            return Some(tok);
        }

        if self.finished {
            return None;
        }

        // Scan next line to fill the buffer
        self.scan_next_line();

        // Return first token from buffer
        if let Some(tok) = self.pending.pop_front() {
            if tok.node == Token::Eof {
                return None;
            }
            Some(tok)
        } else {
            None
        }
    }
}

/// Advance `i` past a quoted string (single or double), handling backslash escapes.
/// On entry, `bytes[i]` must be the opening quote character.
/// On exit, `i` points one past the closing quote (or at end of input if unclosed).
fn skip_quoted_string(bytes: &[u8], i: &mut usize) {
    let quote = bytes[*i];
    *i += 1;
    while *i < bytes.len() && bytes[*i] != quote {
        if bytes[*i] == b'\\' {
            *i += 1;
        }
        *i += 1;
    }
    // Advance past closing quote
    if *i < bytes.len() {
        *i += 1;
    }
}

/// Split an interpolation text like `"x:.2f"` into `("x".to_string(), Some(".2f".to_string()))`.
/// Only splits on the LAST `:` at bracket/paren depth 0, outside quotes.
/// This avoids false splits on dict literals `{a: b}`, slice `x[1:3]`, or ternary expressions.
fn split_interpolation_spec(text: &str) -> (String, Option<String>) {
    let bytes = text.as_bytes();
    let mut last_colon_at_depth_0: Option<usize> = None;
    let mut paren_depth: usize = 0;
    let mut bracket_depth: usize = 0;
    let mut brace_depth: usize = 0;
    let mut i = 0;

    while i < bytes.len() {
        match bytes[i] {
            b'(' => paren_depth += 1,
            b')' => paren_depth = paren_depth.saturating_sub(1),
            b'[' => bracket_depth += 1,
            b']' => bracket_depth = bracket_depth.saturating_sub(1),
            b'{' => brace_depth += 1,
            b'}' => brace_depth = brace_depth.saturating_sub(1),
            b'"' | b'\'' => {
                skip_quoted_string(bytes, &mut i);
                continue;
            }
            b':' if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 => {
                last_colon_at_depth_0 = Some(i);
            }
            _ => {}
        }
        i += 1;
    }

    if let Some(pos) = last_colon_at_depth_0 {
        let spec = &text[pos + 1..];
        if !spec.is_empty() {
            return (text[..pos].to_string(), Some(spec.to_string()));
        }
    }

    (text.to_string(), None)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(source: &str) -> Vec<Token> {
        Lexer::new(source).map(|s| s.node).collect()
    }

    #[test]
    fn test_simple_variable() {
        let tokens = lex("int x = 5\n");
        assert_eq!(
            tokens,
            vec![
                Token::Keyword(Keyword::Int),
                Token::Identifier(crate::intern::intern("x")),
                Token::Eq,
                Token::IntLiteral(5),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_indentation() {
        let tokens = lex("if x:\n    y = 1\n");
        assert_eq!(
            tokens,
            vec![
                Token::Keyword(Keyword::If),
                Token::Identifier(crate::intern::intern("x")),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier(crate::intern::intern("y")),
                Token::Eq,
                Token::IntLiteral(1),
                Token::Newline,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn test_nested_indentation() {
        let tokens = lex("if x:\n    if y:\n        z = 1\n");
        assert_eq!(
            tokens,
            vec![
                Token::Keyword(Keyword::If),
                Token::Identifier(crate::intern::intern("x")),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Keyword(Keyword::If),
                Token::Identifier(crate::intern::intern("y")),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier(crate::intern::intern("z")),
                Token::Eq,
                Token::IntLiteral(1),
                Token::Newline,
                Token::Dedent,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn test_brackets_suppress_newlines() {
        let tokens = lex("f(a,\n  b)\n");
        assert_eq!(
            tokens,
            vec![
                Token::Identifier(crate::intern::intern("f")),
                Token::LParen,
                Token::Identifier(crate::intern::intern("a")),
                Token::Comma,
                Token::Identifier(crate::intern::intern("b")),
                Token::RParen,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_string_literal() {
        let tokens = lex("\"hello\"\n");
        assert_eq!(
            tokens,
            vec![
                Token::StringLiteral(StringLiteral {
                    kind: StringKind::Normal,
                    segments: vec![StringSegment::Literal("hello".to_string())],
                }),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_string_no_interpolation() {
        // Plain "..." strings do NOT interpolate — {name} is a literal brace group.
        let tokens = lex("\"Hello, {name}!\"\n");
        assert_eq!(
            tokens,
            vec![
                Token::StringLiteral(StringLiteral {
                    kind: StringKind::Normal,
                    segments: vec![StringSegment::Literal("Hello, {name}!".to_string())],
                }),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_fstring_interpolation() {
        // f"..." strings DO interpolate.
        let tokens = lex("f\"Hello, {name}!\"\n");
        assert_eq!(
            tokens,
            vec![
                Token::StringLiteral(StringLiteral {
                    kind: StringKind::Format,
                    segments: vec![
                        StringSegment::Literal("Hello, ".to_string()),
                        StringSegment::Interpolation("name".to_string(), None),
                        StringSegment::Literal("!".to_string()),
                    ],
                }),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_keywords() {
        let tokens = lex("struct enum trait equip\n");
        assert_eq!(
            tokens,
            vec![
                Token::Keyword(Keyword::Struct),
                Token::Keyword(Keyword::Enum),
                Token::Keyword(Keyword::Trait),
                Token::Keyword(Keyword::Equip),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_operators() {
        let tokens = lex("a + b == c\n");
        assert_eq!(
            tokens,
            vec![
                Token::Identifier(crate::intern::intern("a")),
                Token::Plus,
                Token::Identifier(crate::intern::intern("b")),
                Token::EqEq,
                Token::Identifier(crate::intern::intern("c")),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_function_definition() {
        let tokens = lex("int add(int a, int b):\n    return a + b\n");
        assert_eq!(
            tokens,
            vec![
                Token::Keyword(Keyword::Int),
                Token::Identifier(crate::intern::intern("add")),
                Token::LParen,
                Token::Keyword(Keyword::Int),
                Token::Identifier(crate::intern::intern("a")),
                Token::Comma,
                Token::Keyword(Keyword::Int),
                Token::Identifier(crate::intern::intern("b")),
                Token::RParen,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Keyword(Keyword::Return),
                Token::Identifier(crate::intern::intern("a")),
                Token::Plus,
                Token::Identifier(crate::intern::intern("b")),
                Token::Newline,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn test_comments_emitted() {
        let tokens = lex("x = 5  # comment\n");
        assert_eq!(
            tokens,
            vec![
                Token::Identifier(crate::intern::intern("x")),
                Token::Eq,
                Token::IntLiteral(5),
                Token::Comment("# comment".to_string()),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_blank_lines_skipped() {
        let tokens = lex("x = 1\n\n\ny = 2\n");
        assert_eq!(
            tokens,
            vec![
                Token::Identifier(crate::intern::intern("x")),
                Token::Eq,
                Token::IntLiteral(1),
                Token::Newline,
                Token::Identifier(crate::intern::intern("y")),
                Token::Eq,
                Token::IntLiteral(2),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_doc_comment() {
        let tokens = lex("#/ This is a doc comment\nint x = 5\n");
        assert_eq!(
            tokens,
            vec![
                Token::DocComment("#/ This is a doc comment".to_string()),
                Token::Keyword(Keyword::Int),
                Token::Identifier(crate::intern::intern("x")),
                Token::Eq,
                Token::IntLiteral(5),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_char_literal() {
        // Single-quoted 'A' is now a StringLiteral (1-codepoint str)
        let tokens = lex("'A'\n");
        assert!(matches!(&tokens[0], Token::StringLiteral(s) if s.segments.len() == 1));
    }

    #[test]
    fn test_byte_literal() {
        // b'A' == 65
        let tokens = lex("b'A'\n");
        assert_eq!(tokens, vec![Token::IntLiteral(65), Token::Newline,]);
    }

    #[test]
    fn test_float_literal() {
        let tokens = lex("3.14\n");
        assert_eq!(
            tokens,
            vec![Token::FloatLiteral(3.14), Token::Newline,]
        );
    }

    #[test]
    fn test_float_literal_not_after_dot_stays_float() {
        // Bare floats, operator-led floats, and exponent floats must NOT be split.
        assert_eq!(
            lex("x + 1.0\n"),
            vec![
                Token::Identifier(crate::intern::intern("x")),
                Token::Plus,
                Token::FloatLiteral(1.0),
                Token::Newline,
            ]
        );
        assert_eq!(
            lex("6.022e23\n"),
            vec![Token::FloatLiteral(6.022e23), Token::Newline,]
        );
        assert_eq!(
            lex("1_000.5\n"),
            vec![Token::FloatLiteral(1000.5), Token::Newline,]
        );
    }

    #[test]
    fn test_tuple_index_nested_splits_float() {
        // `a.1.0` lexes the `1.0` as a FloatLiteral; after a Dot it must split to Int Dot Int.
        assert_eq!(
            lex("a.1.0\n"),
            vec![
                Token::Identifier(crate::intern::intern("a")),
                Token::Dot,
                Token::IntLiteral(1),
                Token::Dot,
                Token::IntLiteral(0),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_tuple_index_leading_zero() {
        // `a.0.1` — the f64-reconstruction trap (0.1f64 loses the split). Lexer-side split is exact.
        assert_eq!(
            lex("a.0.1\n"),
            vec![
                Token::Identifier(crate::intern::intern("a")),
                Token::Dot,
                Token::IntLiteral(0),
                Token::Dot,
                Token::IntLiteral(1),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_tuple_index_three_deep_composes() {
        // `a.1.2.3` lexes as `a . 1.2 . 3`; the split must compose so the chain is fully Int Dot Int...
        assert_eq!(
            lex("a.1.2.3\n"),
            vec![
                Token::Identifier(crate::intern::intern("a")),
                Token::Dot,
                Token::IntLiteral(1),
                Token::Dot,
                Token::IntLiteral(2),
                Token::Dot,
                Token::IntLiteral(3),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_tuple_index_then_operator() {
        // `t.0 + 1`: `.0` is a lone Int already (no float), `+ 1` stays separate.
        assert_eq!(
            lex("t.0 + 1\n"),
            vec![
                Token::Identifier(crate::intern::intern("t")),
                Token::Dot,
                Token::IntLiteral(0),
                Token::Plus,
                Token::IntLiteral(1),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_float_method_call_not_split() {
        // `1.0.mod(3)`: leading `1.0` is the float (no preceding Dot), `.mod` is a normal method.
        let toks = lex("1.0.mod(3)\n");
        assert_eq!(toks[0], Token::FloatLiteral(1.0));
        assert_eq!(toks[1], Token::Dot);
        assert_eq!(toks[2], Token::Identifier(crate::intern::intern("mod")));
    }

    #[test]
    fn test_hex_literal() {
        let tokens = lex("0xFF\n");
        assert_eq!(
            tokens,
            vec![Token::IntLiteral(255), Token::Newline,]
        );
    }

    #[test]
    fn test_dedent_to_lower_level() {
        let tokens = lex("if x:\n    if y:\n        a = 1\n    b = 2\nc = 3\n");
        assert_eq!(
            tokens,
            vec![
                Token::Keyword(Keyword::If),
                Token::Identifier(crate::intern::intern("x")),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Keyword(Keyword::If),
                Token::Identifier(crate::intern::intern("y")),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier(crate::intern::intern("a")),
                Token::Eq,
                Token::IntLiteral(1),
                Token::Newline,
                Token::Dedent,
                Token::Identifier(crate::intern::intern("b")),
                Token::Eq,
                Token::IntLiteral(2),
                Token::Newline,
                Token::Dedent,
                Token::Identifier(crate::intern::intern("c")),
                Token::Eq,
                Token::IntLiteral(3),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_leading_dot_continuation() {
        let tokens = lex("x = foo\n    .bar()\n    .baz()\n");
        assert_eq!(
            tokens,
            vec![
                Token::Identifier(crate::intern::intern("x")),
                Token::Eq,
                Token::Identifier(crate::intern::intern("foo")),
                Token::Dot,
                Token::Identifier(crate::intern::intern("bar")),
                Token::LParen,
                Token::RParen,
                Token::Dot,
                Token::Identifier(crate::intern::intern("baz")),
                Token::LParen,
                Token::RParen,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_raw_string() {
        let tokens = lex("r\"no \\escape\"\n");
        assert_eq!(
            tokens,
            vec![
                Token::StringLiteral(StringLiteral {
                    kind: StringKind::Raw,
                    segments: vec![StringSegment::Literal("no \\escape".to_string())],
                }),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_normal_string_literal_braces() {
        // In Normal strings, { and } are literal characters — no escape needed.
        let tokens = lex("\"{{literal}}\"\n");
        assert_eq!(
            tokens,
            vec![
                Token::StringLiteral(StringLiteral {
                    kind: StringKind::Normal,
                    segments: vec![StringSegment::Literal("{{literal}}".to_string())],
                }),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_fstring_escaped_braces() {
        // In Format strings, {{ → { and }} → } (escape sequences).
        let tokens = lex("f\"{{literal}}\"\n");
        assert_eq!(
            tokens,
            vec![
                Token::StringLiteral(StringLiteral {
                    kind: StringKind::Format,
                    segments: vec![StringSegment::Literal("{literal}".to_string())],
                }),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_optional_chaining_and_default_op() {
        let tokens = lex("a?.b ?? c\n");
        assert_eq!(
            tokens,
            vec![
                Token::Identifier(crate::intern::intern("a")),
                Token::QuestionDot,
                Token::Identifier(crate::intern::intern("b")),
                Token::DoubleQuestion,
                Token::Identifier(crate::intern::intern("c")),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_range_operators() {
        let tokens = lex("0..10\n0..=10\n");
        assert_eq!(
            tokens,
            vec![
                Token::IntLiteral(0),
                Token::DotDot,
                Token::IntLiteral(10),
                Token::Newline,
                Token::IntLiteral(0),
                Token::DotDotEq,
                Token::IntLiteral(10),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_compound_assignment() {
        let tokens = lex("x += 1\n");
        assert_eq!(
            tokens,
            vec![
                Token::Identifier(crate::intern::intern("x")),
                Token::PlusEq,
                Token::IntLiteral(1),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_ownership_operators() {
        let tokens = lex("f(&name)\nf(!name)\n");
        assert_eq!(
            tokens,
            vec![
                Token::Identifier(crate::intern::intern("f")),
                Token::LParen,
                Token::Ampersand,
                Token::Identifier(crate::intern::intern("name")),
                Token::RParen,
                Token::Newline,
                Token::Identifier(crate::intern::intern("f")),
                Token::LParen,
                Token::Bang,
                Token::Identifier(crate::intern::intern("name")),
                Token::RParen,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_attribute() {
        let tokens = lex("@derive(Debuggable)\n");
        assert_eq!(
            tokens,
            vec![
                Token::At,
                Token::Identifier(crate::intern::intern("derive")),
                Token::LParen,
                Token::Identifier(crate::intern::intern("Debuggable")),
                Token::RParen,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_match_with_pipe() {
        let tokens = lex("case 200 | 201:\n");
        assert_eq!(
            tokens,
            vec![
                Token::Keyword(Keyword::Case),
                Token::IntLiteral(200),
                Token::Pipe,
                Token::IntLiteral(201),
                Token::Colon,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_multiline_brackets() {
        let tokens = lex("[1,\n    2,\n    3]\n");
        assert_eq!(
            tokens,
            vec![
                Token::LBracket,
                Token::IntLiteral(1),
                Token::Comma,
                Token::IntLiteral(2),
                Token::Comma,
                Token::IntLiteral(3),
                Token::RBracket,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_complete_program() {
        let source = "\
void main():
    int x = 42
    if x > 0:
        print(\"positive\")
";
        let tokens = lex(source);
        assert_eq!(
            tokens,
            vec![
                Token::Keyword(Keyword::Void),
                Token::Identifier(crate::intern::intern("main")),
                Token::LParen,
                Token::RParen,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Keyword(Keyword::Int),
                Token::Identifier(crate::intern::intern("x")),
                Token::Eq,
                Token::IntLiteral(42),
                Token::Newline,
                Token::Keyword(Keyword::If),
                Token::Identifier(crate::intern::intern("x")),
                Token::Gt,
                Token::IntLiteral(0),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier(crate::intern::intern("print")),
                Token::LParen,
                Token::StringLiteral(StringLiteral {
                    kind: StringKind::Normal,
                    segments: vec![StringSegment::Literal("positive".to_string())],
                }),
                Token::RParen,
                Token::Newline,
                Token::Dedent,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn test_whole_line_comment() {
        let tokens = lex("# whole line comment\nx = 1\n");
        assert_eq!(
            tokens,
            vec![
                Token::Comment("# whole line comment".to_string()),
                Token::Identifier(crate::intern::intern("x")),
                Token::Eq,
                Token::IntLiteral(1),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_wrapping_operators() {
        let tokens = lex("a +% b -% c *% d\n");
        assert_eq!(
            tokens,
            vec![
                Token::Identifier(crate::intern::intern("a")),
                Token::PlusPercent,
                Token::Identifier(crate::intern::intern("b")),
                Token::MinusPercent,
                Token::Identifier(crate::intern::intern("c")),
                Token::StarPercent,
                Token::Identifier(crate::intern::intern("d")),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_meta_keyword() {
        let tokens = lex("meta int X = 1024\n");
        assert_eq!(
            tokens,
            vec![
                Token::Keyword(Keyword::Meta),
                Token::Keyword(Keyword::Int),
                Token::Identifier(crate::intern::intern("X")),
                Token::Eq,
                Token::IntLiteral(1024),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_wrapping_compound_assignment() {
        let tokens = lex("x +%= 1\ny -%= 2\nz *%= 3\n");
        assert_eq!(
            tokens,
            vec![
                Token::Identifier(crate::intern::intern("x")),
                Token::PlusPercentEq,
                Token::IntLiteral(1),
                Token::Newline,
                Token::Identifier(crate::intern::intern("y")),
                Token::MinusPercentEq,
                Token::IntLiteral(2),
                Token::Newline,
                Token::Identifier(crate::intern::intern("z")),
                Token::StarPercentEq,
                Token::IntLiteral(3),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn cstr_literal() {
        let tokens = lex(r#"c"hello""#);
        assert_eq!(tokens.len(), 2); // StringLiteral + Newline
        match &tokens[0] {
            Token::StringLiteral(s) => {
                assert_eq!(s.kind, StringKind::CStr);
                assert_eq!(s.segments, vec![StringSegment::Literal("hello".to_string())]);
            }
            other => panic!("expected StringLiteral, got {:?}", other),
        }
    }

    #[test]
    fn cstr_no_interpolation() {
        let tokens = lex(r#"c"{x}""#);
        assert_eq!(tokens.len(), 2);
        match &tokens[0] {
            Token::StringLiteral(s) => {
                assert_eq!(s.kind, StringKind::CStr);
                // {x} should be literal text, not interpolation
                assert_eq!(s.segments, vec![StringSegment::Literal("{x}".to_string())]);
            }
            other => panic!("expected StringLiteral, got {:?}", other),
        }
    }

    #[test]
    fn cstr_escape_sequences() {
        let tokens = lex(r#"c"hello\nworld""#);
        assert_eq!(tokens.len(), 2);
        match &tokens[0] {
            Token::StringLiteral(s) => {
                assert_eq!(s.kind, StringKind::CStr);
                assert_eq!(s.segments, vec![StringSegment::Literal("hello\nworld".to_string())]);
            }
            other => panic!("expected StringLiteral, got {:?}", other),
        }
    }

    #[test]
    fn cstr_is_identifier() {
        let tokens = lex("cstr");
        assert!(matches!(&tokens[0], Token::Identifier(_)),
            "expected identifier, got {:?}", tokens[0]);
    }
}
