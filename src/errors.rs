use crate::span::Span;
use codespan_reporting::diagnostic::{self, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

/// Lex-time error.
#[derive(Debug, Clone)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum LexErrorKind {
    UnterminatedString,
    UnterminatedCharLiteral,
    InvalidEscapeSequence(String),
    InvalidIndentation {
        got: usize,
    },
    IndentationMismatch {
        got: usize,
    },
    InvalidCharacter(char),
    InvalidNumericLiteral(String),
    UnterminatedInterpolation,
    TabCharacter,
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            LexErrorKind::UnterminatedString => write!(f, "unterminated string literal"),
            LexErrorKind::UnterminatedCharLiteral => write!(f, "unterminated character literal"),
            LexErrorKind::InvalidEscapeSequence(s) => write!(f, "invalid escape sequence: {s}"),
            LexErrorKind::InvalidIndentation { got } => {
                write!(f, "indentation must be a multiple of 4 spaces, got {got}")
            }
            LexErrorKind::IndentationMismatch { got } => {
                write!(f, "dedent does not match any outer indentation level (got {got} spaces)")
            }
            LexErrorKind::InvalidCharacter(c) => write!(f, "unexpected character: '{c}'"),
            LexErrorKind::InvalidNumericLiteral(s) => write!(f, "invalid numeric literal: {s}"),
            LexErrorKind::UnterminatedInterpolation => {
                write!(f, "unterminated string interpolation")
            }
            LexErrorKind::TabCharacter => write!(f, "tab characters are not allowed; use spaces"),
        }
    }
}

/// Parse-time error.
#[derive(Debug, Clone)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    UnexpectedToken {
        expected: String,
        got: String,
    },
    UnexpectedEof,
    ExpectedBlock,
    ExpectedExpression,
    ExpectedType,
    ExpectedPattern,
    ExpectedIdentifier,
    InvalidAssignmentTarget,
    PositionalAfterNamedArg,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ParseErrorKind::UnexpectedToken { expected, got } => {
                write!(f, "expected {expected}, found {got}")
            }
            ParseErrorKind::UnexpectedEof => write!(f, "unexpected end of file"),
            ParseErrorKind::ExpectedBlock => write!(f, "expected indented block"),
            ParseErrorKind::ExpectedExpression => write!(f, "expected expression"),
            ParseErrorKind::ExpectedType => write!(f, "expected type"),
            ParseErrorKind::ExpectedPattern => write!(f, "expected pattern"),
            ParseErrorKind::ExpectedIdentifier => write!(f, "expected identifier"),
            ParseErrorKind::InvalidAssignmentTarget => write!(f, "invalid assignment target"),
            ParseErrorKind::PositionalAfterNamedArg => {
                write!(f, "positional argument after named argument")
            }
        }
    }
}

/// Renders compiler diagnostics to stderr.
pub struct ErrorReporter {
    files: SimpleFiles<String, String>,
    file_id: usize,
}

impl ErrorReporter {
    pub fn new(filename: String, source: String) -> Self {
        let mut files = SimpleFiles::new();
        let file_id = files.add(filename, source);
        Self { files, file_id }
    }

    pub fn report_lex_error(&self, err: &LexError) {
        let diag = diagnostic::Diagnostic::error()
            .with_message(err.to_string())
            .with_labels(vec![Label::primary(
                self.file_id,
                err.span.start..err.span.end,
            )]);
        self.emit(&diag);
    }

    pub fn report_parse_error(&self, err: &ParseError) {
        let diag = diagnostic::Diagnostic::error()
            .with_message(err.to_string())
            .with_labels(vec![Label::primary(
                self.file_id,
                err.span.start..err.span.end,
            )]);
        self.emit(&diag);
    }

    pub fn report_semantic_error(&self, err: &crate::semantic::errors::SemanticError) {
        use crate::semantic::errors::SemanticErrorKind;

        let mut labels = vec![Label::primary(
            self.file_id,
            err.span.start..err.span.end,
        )];

        // Add secondary labels for errors that reference other locations.
        match &err.kind {
            SemanticErrorKind::DuplicateDefinition { original, .. } => {
                labels.push(
                    Label::secondary(self.file_id, original.start..original.end)
                        .with_message("originally defined here"),
                );
            }
            SemanticErrorKind::UseAfterMove { moved_at, .. } => {
                labels.push(
                    Label::secondary(self.file_id, moved_at.start..moved_at.end)
                        .with_message("value moved here"),
                );
            }
            SemanticErrorKind::DoubleMove { first_move, .. } => {
                labels.push(
                    Label::secondary(self.file_id, first_move.start..first_move.end)
                        .with_message("first move here"),
                );
            }
            _ => {}
        }

        let diag = diagnostic::Diagnostic::error()
            .with_message(err.to_string())
            .with_labels(labels);
        self.emit(&diag);
    }

    fn emit(&self, diag: &diagnostic::Diagnostic<usize>) {
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = term::Config::default();
        let _ = term::emit(&mut writer.lock(), &config, &self.files, diag);
    }
}
