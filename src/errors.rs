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
            SemanticErrorKind::UseAfterSourceMoved { moved_at, source_name, .. } => {
                labels.push(
                    Label::secondary(self.file_id, moved_at.start..moved_at.end)
                        .with_message(format!("`{source_name}` moved here")),
                );
            }
            SemanticErrorKind::DanglingReturn { local_declared_at, local_name, .. } => {
                if let Some(decl_span) = local_declared_at {
                    labels.push(
                        Label::secondary(self.file_id, decl_span.start..decl_span.end)
                            .with_message(format!("`{local_name}` declared here — will be dropped when function returns")),
                    );
                }
            }
            SemanticErrorKind::TemporaryBorrow { temp_at, callee, .. } => {
                if let Some(temp_span) = temp_at {
                    labels.push(
                        Label::secondary(self.file_id, temp_span.start..temp_span.end)
                            .with_message(format!("temporary from `{callee}()` created here — dropped at end of statement")),
                    );
                }
            }
            _ => {}
        }

        let diag = diagnostic::Diagnostic::error()
            .with_message(err.to_string())
            .with_labels(labels);
        self.emit(&diag);
    }

    pub fn report_semantic_warning(&self, warn: &crate::semantic::errors::SemanticWarning) {
        use crate::semantic::errors::SemanticWarningKind;
        let mut labels = vec![Label::primary(
            self.file_id,
            warn.span.start..warn.span.end,
        )];
        let mut notes = Vec::new();

        // Add secondary labels for multi-span warnings.
        if let SemanticWarningKind::StaleSharedCondition {
            derivation_span, await_span, shared_name, ..
        } = &warn.kind {
            if let Some(ds) = derivation_span {
                labels.push(
                    Label::secondary(self.file_id, ds.start..ds.end)
                        .with_message(format!("read from shared `{shared_name}` here")),
                );
            }
            if let Some(aws) = await_span {
                labels.push(
                    Label::secondary(self.file_id, aws.start..aws.end)
                        .with_message("token released at this await — value may have changed"),
                );
            }
            notes.push(format!("re-read `{shared_name}` after the await, or use `with {shared_name}:` to auto-refresh across await points"));
        }

        let mut diag = diagnostic::Diagnostic::warning()
            .with_message(warn.to_string())
            .with_labels(labels);
        if !notes.is_empty() {
            diag = diag.with_notes(notes);
        }
        self.emit(&diag);
    }

    fn emit(&self, diag: &diagnostic::Diagnostic<usize>) {
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = term::Config::default();
        let _ = term::emit(&mut writer.lock(), &config, &self.files, diag);
    }
}
