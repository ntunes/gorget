pub mod token;

use crate::errors::{LexError, LexErrorKind};
use crate::span::{Span, Spanned};
use logos::Logos;
use std::collections::VecDeque;
use token::{Keyword, RawToken, StringKind, StringLit, StringSegment, Token};

/// Indentation-aware lexer for Gorget source code.
///
/// Wraps the logos-based raw tokenizer and emits synthetic INDENT/DEDENT/NEWLINE
/// tokens based on indentation changes. Also handles string literals (including
/// interpolation), char literals, comments, and doc comments.
pub struct Lexer<'src> {
    source: &'src str,
    /// Current byte offset in source.
    pos: usize,
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

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            pos: 0,
            indent_stack: vec![0],
            bracket_depth: 0,
            pending: VecDeque::new(),
            finished: false,
            need_newline: false,
            errors: Vec::new(),
        }
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
                    if self.pos + 1 < self.source.len()
                        && self.source.as_bytes()[self.pos + 1] == b'/'
                    {
                        // Doc comment — extract content and emit
                        let content = self.scan_to_eol();
                        self.pending.push_back(Spanned::new(
                            Token::DocComment(content),
                            Span::new(line_start, self.pos),
                        ));
                        if self.pos < self.source.len() {
                            self.pos += 1; // skip \n
                        }
                        continue;
                    }
                    // Regular comment — emit as token
                    let content = self.scan_to_eol();
                    self.pending.push_back(Spanned::new(
                        Token::Comment(content),
                        Span::new(line_start, self.pos),
                    ));
                    if self.pos < self.source.len() {
                        self.pos += 1; // skip \n
                    }
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
                        span: Span::new(self.pos, self.pos + 1),
                    });
                    self.pos += 1;
                }
                _ => break,
            }
        }
        self.pos - start
    }

    /// Process indentation changes, emitting INDENT/DEDENT/NEWLINE tokens.
    fn process_indentation(&mut self, spaces: usize, line_start: usize) {
        let current = *self.indent_stack.last().unwrap();

        if spaces == current {
            // Same indentation level
            if self.need_newline {
                self.pending.push_back(Spanned::new(
                    Token::Newline,
                    Span::new(line_start, line_start),
                ));
            }
        } else if spaces > current {
            // Indentation increased — any amount is valid (Python-style)
            if self.need_newline {
                self.pending.push_back(Spanned::new(
                    Token::Newline,
                    Span::new(line_start, line_start),
                ));
            }
            self.indent_stack.push(spaces);
            self.pending.push_back(Spanned::new(
                Token::Indent,
                Span::new(line_start, line_start + spaces),
            ));
        } else {
            // Indentation decreased — emit DEDENT(s)
            if self.need_newline {
                self.pending.push_back(Spanned::new(
                    Token::Newline,
                    Span::new(line_start, line_start),
                ));
            }
            while *self.indent_stack.last().unwrap() > spaces {
                self.indent_stack.pop();
                self.pending.push_back(Spanned::new(
                    Token::Dedent,
                    Span::new(line_start, line_start + spaces),
                ));
            }
            if *self.indent_stack.last().unwrap() != spaces {
                self.errors.push(LexError {
                    kind: LexErrorKind::IndentationMismatch { got: spaces },
                    span: Span::new(line_start, line_start + spaces),
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
                Span::new(eof_pos, eof_pos),
            ));
        }

        // Pop all indentation levels back to 0
        while self.indent_stack.len() > 1 {
            self.indent_stack.pop();
            self.pending.push_back(Spanned::new(
                Token::Dedent,
                Span::new(eof_pos, eof_pos),
            ));
        }

        self.pending
            .push_back(Spanned::new(Token::Eof, Span::new(eof_pos, eof_pos)));
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
                        Span::new(comment_start, i),
                    ));
                    break;
                }

                // String literals
                b'"' => {
                    let (tok, new_end) = self.scan_string_literal(i);
                    let span = Span::new(i, new_end);
                    self.pending.push_back(Spanned::new(tok, span));
                    i = new_end;
                }

                // Raw string r"..." or byte string b"..."
                b'r' | b'b' if i + 1 < bytes.len() && bytes[i + 1] == b'"' => {
                    let (tok, new_end) = self.scan_string_literal(i);
                    let span = Span::new(i, new_end);
                    self.pending.push_back(Spanned::new(tok, span));
                    i = new_end;
                }

                // Char literal
                b'\'' => {
                    let (tok, new_end) = self.scan_char_literal(i);
                    let span = Span::new(i, new_end);
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
                        && !(bytes[i] == b'r'
                            && i + 1 < bytes.len()
                            && bytes[i + 1] == b'"')
                        && !(bytes[i] == b'b'
                            && i + 1 < bytes.len()
                            && bytes[i + 1] == b'"')
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
                        let span = Span::new(span_start, span_end);
                        match raw_result {
                            Ok(raw) => {
                                if let Some(tok) = self.convert_raw_token(raw, slice, span) {
                                    match &tok {
                                        Token::LParen | Token::LBracket | Token::LBrace => {
                                            self.bracket_depth += 1;
                                        }
                                        Token::RParen | Token::RBracket | Token::RBrace => {
                                            if self.bracket_depth > 0 {
                                                self.bracket_depth -= 1;
                                            }
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

    /// Convert a raw token + its slice into a final Token.
    fn convert_raw_token(&mut self, raw: RawToken, slice: &str, span: Span) -> Option<Token> {
        Some(match raw {
            RawToken::IntLiteral => {
                let clean: String = slice.chars().filter(|c| *c != '_').collect();
                match clean.parse::<i64>() {
                    Ok(n) => Token::IntLiteral(n),
                    Err(_) => {
                        self.errors.push(LexError {
                            kind: LexErrorKind::InvalidNumericLiteral(slice.to_string()),
                            span,
                        });
                        Token::Error(format!("invalid integer: {slice}"))
                    }
                }
            }
            RawToken::HexLiteral => {
                let clean: String = slice[2..].chars().filter(|c| *c != '_').collect();
                match i64::from_str_radix(&clean, 16) {
                    Ok(n) => Token::IntLiteral(n),
                    Err(_) => {
                        self.errors.push(LexError {
                            kind: LexErrorKind::InvalidNumericLiteral(slice.to_string()),
                            span,
                        });
                        Token::Error(format!("invalid hex: {slice}"))
                    }
                }
            }
            RawToken::OctalLiteral => {
                let clean: String = slice[2..].chars().filter(|c| *c != '_').collect();
                match i64::from_str_radix(&clean, 8) {
                    Ok(n) => Token::IntLiteral(n),
                    Err(_) => {
                        self.errors.push(LexError {
                            kind: LexErrorKind::InvalidNumericLiteral(slice.to_string()),
                            span,
                        });
                        Token::Error(format!("invalid octal: {slice}"))
                    }
                }
            }
            RawToken::BinaryLiteral => {
                let clean: String = slice[2..].chars().filter(|c| *c != '_').collect();
                match i64::from_str_radix(&clean, 2) {
                    Ok(n) => Token::IntLiteral(n),
                    Err(_) => {
                        self.errors.push(LexError {
                            kind: LexErrorKind::InvalidNumericLiteral(slice.to_string()),
                            span,
                        });
                        Token::Error(format!("invalid binary: {slice}"))
                    }
                }
            }
            RawToken::FloatLiteral => {
                let clean: String = slice.chars().filter(|c| *c != '_').collect();
                match clean.parse::<f64>() {
                    Ok(n) => Token::FloatLiteral(n),
                    Err(_) => {
                        self.errors.push(LexError {
                            kind: LexErrorKind::InvalidNumericLiteral(slice.to_string()),
                            span,
                        });
                        Token::Error(format!("invalid float: {slice}"))
                    }
                }
            }
            RawToken::Identifier | RawToken::Underscore => {
                if let Some(kw) = Keyword::from_str(slice) {
                    Token::Keyword(kw)
                } else if slice == "_" {
                    Token::Underscore
                } else {
                    Token::Identifier(slice.to_string())
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
            RawToken::MinusEq => Token::MinusEq,
            RawToken::StarEq => Token::StarEq,
            RawToken::SlashEq => Token::SlashEq,
            RawToken::PercentEq => Token::PercentEq,
            RawToken::PlusPercent => Token::PlusPercent,
            RawToken::MinusPercent => Token::MinusPercent,
            RawToken::StarPercent => Token::StarPercent,
            RawToken::PlusPercentEq => Token::PlusPercentEq,
            RawToken::MinusPercentEq => Token::MinusPercentEq,
            RawToken::StarPercentEq => Token::StarPercentEq,
            RawToken::DotDot => Token::DotDot,
            RawToken::DotDotEq => Token::DotDotEq,
            RawToken::QuestionDot => Token::QuestionDot,
            RawToken::DoubleQuestion => Token::DoubleQuestion,
        })
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
        } else {
            StringKind::Normal
        };

        // Expect opening quote
        if i >= bytes.len() || bytes[i] != b'"' {
            return (Token::Error("expected string".to_string()), i);
        }

        // Check for triple-quote
        let triple = i + 2 < bytes.len() && bytes[i + 1] == b'"' && bytes[i + 2] == b'"';
        let actual_kind = if triple && kind == StringKind::Normal {
            StringKind::MultiLine
        } else {
            kind
        };

        if triple {
            i += 3; // skip """
        } else {
            i += 1; // skip "
        }

        let mut segments: Vec<StringSegment> = Vec::new();
        let mut current_literal = String::new();

        loop {
            if i >= bytes.len() {
                self.errors.push(LexError {
                    kind: LexErrorKind::UnterminatedString,
                    span: Span::new(pos, i),
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
            } else if bytes[i] == b'"' {
                i += 1;
                break;
            }

            if bytes[i] == b'\n' && !triple {
                self.errors.push(LexError {
                    kind: LexErrorKind::UnterminatedString,
                    span: Span::new(pos, i),
                });
                break;
            }

            // Escape sequences
            if bytes[i] == b'\\' && actual_kind != StringKind::Raw {
                i += 1;
                if i >= bytes.len() {
                    self.errors.push(LexError {
                        kind: LexErrorKind::UnterminatedString,
                        span: Span::new(pos, i),
                    });
                    break;
                }
                match bytes[i] {
                    b'n' => {
                        current_literal.push('\n');
                        i += 1;
                    }
                    b't' => {
                        current_literal.push('\t');
                        i += 1;
                    }
                    b'r' => {
                        current_literal.push('\r');
                        i += 1;
                    }
                    b'\\' => {
                        current_literal.push('\\');
                        i += 1;
                    }
                    b'"' => {
                        current_literal.push('"');
                        i += 1;
                    }
                    b'0' => {
                        current_literal.push('\0');
                        i += 1;
                    }
                    b'{' => {
                        current_literal.push('{');
                        i += 1;
                    }
                    b'}' => {
                        current_literal.push('}');
                        i += 1;
                    }
                    b'u' if i + 1 < bytes.len() && bytes[i + 1] == b'{' => {
                        i += 2; // skip u{
                        let hex_start = i;
                        while i < bytes.len() && bytes[i] != b'}' {
                            i += 1;
                        }
                        let hex = &self.source[hex_start..i];
                        if i < bytes.len() {
                            i += 1; // skip }
                        }
                        if let Ok(code) = u32::from_str_radix(hex, 16) {
                            if let Some(c) = char::from_u32(code) {
                                current_literal.push(c);
                            } else {
                                self.errors.push(LexError {
                                    kind: LexErrorKind::InvalidEscapeSequence(
                                        format!("\\u{{{hex}}}"),
                                    ),
                                    span: Span::new(hex_start - 3, i),
                                });
                            }
                        }
                    }
                    other => {
                        self.errors.push(LexError {
                            kind: LexErrorKind::InvalidEscapeSequence(
                                format!("\\{}", other as char),
                            ),
                            span: Span::new(i - 1, i + 1),
                        });
                        current_literal.push(other as char);
                        i += 1;
                    }
                }
                continue;
            }

            // String interpolation: {expr}
            if bytes[i] == b'{' && actual_kind != StringKind::Raw {
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
                        b'"' => {
                            // Skip nested string
                            i += 1;
                            while i < bytes.len() && bytes[i] != b'"' {
                                if bytes[i] == b'\\' {
                                    i += 1;
                                }
                                i += 1;
                            }
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
                        span: Span::new(expr_start - 1, i),
                    });
                } else {
                    let expr_text = self.source[expr_start..i].to_string();
                    segments.push(StringSegment::Interpolation(expr_text));
                    i += 1; // skip closing }
                }
                continue;
            }

            // Escaped brace: }}
            if bytes[i] == b'}'
                && actual_kind != StringKind::Raw
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

        let token = Token::StringLiteral(StringLit {
            kind: actual_kind,
            segments,
        });
        (token, i)
    }

    /// Scan a char literal starting at `pos`. Returns (Token, end_position).
    fn scan_char_literal(&mut self, pos: usize) -> (Token, usize) {
        let bytes = self.source.as_bytes();
        let mut i = pos + 1; // skip opening '

        if i >= bytes.len() || bytes[i] == b'\n' {
            self.errors.push(LexError {
                kind: LexErrorKind::UnterminatedCharLiteral,
                span: Span::new(pos, i),
            });
            return (Token::Error("unterminated char".to_string()), i);
        }

        let ch = if bytes[i] == b'\\' {
            i += 1;
            if i >= bytes.len() {
                self.errors.push(LexError {
                    kind: LexErrorKind::UnterminatedCharLiteral,
                    span: Span::new(pos, i),
                });
                return (Token::Error("unterminated char".to_string()), i);
            }
            let escaped = match bytes[i] {
                b'n' => '\n',
                b't' => '\t',
                b'r' => '\r',
                b'\\' => '\\',
                b'\'' => '\'',
                b'0' => '\0',
                b'u' if i + 1 < bytes.len() && bytes[i + 1] == b'{' => {
                    i += 2; // skip u{
                    let hex_start = i;
                    while i < bytes.len() && bytes[i] != b'}' {
                        i += 1;
                    }
                    let hex = &self.source[hex_start..i];
                    if i < bytes.len() {
                        i += 1; // skip }
                    }
                    match u32::from_str_radix(hex, 16) {
                        Ok(code) => char::from_u32(code).unwrap_or('\u{FFFD}'),
                        Err(_) => {
                            self.errors.push(LexError {
                                kind: LexErrorKind::InvalidEscapeSequence(
                                    format!("\\u{{{hex}}}"),
                                ),
                                span: Span::new(pos, i),
                            });
                            '\u{FFFD}'
                        }
                    }
                }
                other => {
                    self.errors.push(LexError {
                        kind: LexErrorKind::InvalidEscapeSequence(
                            format!("\\{}", other as char),
                        ),
                        span: Span::new(i - 1, i + 1),
                    });
                    other as char
                }
            };
            i += if bytes.get(i.wrapping_sub(1)) == Some(&b'u') {
                0
            } else {
                1
            };
            escaped
        } else {
            let c = self.source[i..].chars().next().unwrap();
            i += c.len_utf8();
            c
        };

        // Expect closing quote
        if i < bytes.len() && bytes[i] == b'\'' {
            i += 1;
            (Token::CharLiteral(ch), i)
        } else {
            self.errors.push(LexError {
                kind: LexErrorKind::UnterminatedCharLiteral,
                span: Span::new(pos, i),
            });
            (Token::CharLiteral(ch), i)
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
                Token::Identifier("x".to_string()),
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
                Token::Identifier("x".to_string()),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier("y".to_string()),
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
                Token::Identifier("x".to_string()),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Keyword(Keyword::If),
                Token::Identifier("y".to_string()),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier("z".to_string()),
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
                Token::Identifier("f".to_string()),
                Token::LParen,
                Token::Identifier("a".to_string()),
                Token::Comma,
                Token::Identifier("b".to_string()),
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
                Token::StringLiteral(StringLit {
                    kind: StringKind::Normal,
                    segments: vec![StringSegment::Literal("hello".to_string())],
                }),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_string_interpolation() {
        let tokens = lex("\"Hello, {name}!\"\n");
        assert_eq!(
            tokens,
            vec![
                Token::StringLiteral(StringLit {
                    kind: StringKind::Normal,
                    segments: vec![
                        StringSegment::Literal("Hello, ".to_string()),
                        StringSegment::Interpolation("name".to_string()),
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
                Token::Identifier("a".to_string()),
                Token::Plus,
                Token::Identifier("b".to_string()),
                Token::EqEq,
                Token::Identifier("c".to_string()),
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
                Token::Identifier("add".to_string()),
                Token::LParen,
                Token::Keyword(Keyword::Int),
                Token::Identifier("a".to_string()),
                Token::Comma,
                Token::Keyword(Keyword::Int),
                Token::Identifier("b".to_string()),
                Token::RParen,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Keyword(Keyword::Return),
                Token::Identifier("a".to_string()),
                Token::Plus,
                Token::Identifier("b".to_string()),
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
                Token::Identifier("x".to_string()),
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
                Token::Identifier("x".to_string()),
                Token::Eq,
                Token::IntLiteral(1),
                Token::Newline,
                Token::Identifier("y".to_string()),
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
                Token::Identifier("x".to_string()),
                Token::Eq,
                Token::IntLiteral(5),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_char_literal() {
        let tokens = lex("'A'\n");
        assert_eq!(
            tokens,
            vec![Token::CharLiteral('A'), Token::Newline,]
        );
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
                Token::Identifier("x".to_string()),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Keyword(Keyword::If),
                Token::Identifier("y".to_string()),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier("a".to_string()),
                Token::Eq,
                Token::IntLiteral(1),
                Token::Newline,
                Token::Dedent,
                Token::Identifier("b".to_string()),
                Token::Eq,
                Token::IntLiteral(2),
                Token::Newline,
                Token::Dedent,
                Token::Identifier("c".to_string()),
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
                Token::Identifier("x".to_string()),
                Token::Eq,
                Token::Identifier("foo".to_string()),
                Token::Dot,
                Token::Identifier("bar".to_string()),
                Token::LParen,
                Token::RParen,
                Token::Dot,
                Token::Identifier("baz".to_string()),
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
                Token::StringLiteral(StringLit {
                    kind: StringKind::Raw,
                    segments: vec![StringSegment::Literal("no \\escape".to_string())],
                }),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_escaped_braces_in_string() {
        let tokens = lex("\"{{literal}}\"\n");
        assert_eq!(
            tokens,
            vec![
                Token::StringLiteral(StringLit {
                    kind: StringKind::Normal,
                    segments: vec![StringSegment::Literal("{literal}".to_string())],
                }),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn test_optional_chaining_and_nil_coalescing() {
        let tokens = lex("a?.b ?? c\n");
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::QuestionDot,
                Token::Identifier("b".to_string()),
                Token::DoubleQuestion,
                Token::Identifier("c".to_string()),
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
                Token::Identifier("x".to_string()),
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
                Token::Identifier("f".to_string()),
                Token::LParen,
                Token::Ampersand,
                Token::Identifier("name".to_string()),
                Token::RParen,
                Token::Newline,
                Token::Identifier("f".to_string()),
                Token::LParen,
                Token::Bang,
                Token::Identifier("name".to_string()),
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
                Token::Identifier("derive".to_string()),
                Token::LParen,
                Token::Identifier("Debuggable".to_string()),
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
                Token::Identifier("main".to_string()),
                Token::LParen,
                Token::RParen,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Keyword(Keyword::Int),
                Token::Identifier("x".to_string()),
                Token::Eq,
                Token::IntLiteral(42),
                Token::Newline,
                Token::Keyword(Keyword::If),
                Token::Identifier("x".to_string()),
                Token::Gt,
                Token::IntLiteral(0),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier("print".to_string()),
                Token::LParen,
                Token::StringLiteral(StringLit {
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
                Token::Identifier("x".to_string()),
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
                Token::Identifier("a".to_string()),
                Token::PlusPercent,
                Token::Identifier("b".to_string()),
                Token::MinusPercent,
                Token::Identifier("c".to_string()),
                Token::StarPercent,
                Token::Identifier("d".to_string()),
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
                Token::Identifier("x".to_string()),
                Token::PlusPercentEq,
                Token::IntLiteral(1),
                Token::Newline,
                Token::Identifier("y".to_string()),
                Token::MinusPercentEq,
                Token::IntLiteral(2),
                Token::Newline,
                Token::Identifier("z".to_string()),
                Token::StarPercentEq,
                Token::IntLiteral(3),
                Token::Newline,
            ]
        );
    }
}
