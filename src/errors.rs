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

/// Parse-time warning (non-fatal).
#[derive(Debug, Clone)]
pub struct ParseWarning {
    pub kind: ParseWarningKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ParseWarningKind {}

impl std::fmt::Display for ParseWarning {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {}
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
    /// A single expression nests deeper than `MAX_EXPR_DEPTH` (parens, unary, or
    /// a flat operator chain). Rejected at parse time so the compiler emits a
    /// clean teaching error instead of overflowing the lowering recursion stack
    /// (SIGSEGV). See `MAX_EXPR_DEPTH` / `ExprDepthGuard` in `src/parser/expr.rs`.
    ExpressionTooDeep {
        depth: usize,
        limit: usize,
    },
    /// A type-first declaration (`int x`) with no `=` initializer. Gorget has no
    /// uninitialized-variable form, so every declaration must bind a value. Caught
    /// at the declaration site (rather than letting `int x` fall through to
    /// expression parsing, where `x` would resolve as an undefined name).
    MissingInitializer,
    /// D10(a) (docs/define-gorget/decisions.md, ratified 2026-07-06): a
    /// `&` decl-sigil on a local binding (`Vector[int] &r = a`) — the
    /// decl-sigil form of a local `&`-bind, rejected in v1. Historically the
    /// sigil was silently discarded (the binding was a plain value copy that
    /// READ as a reference decl); the `= &expr` init form is rejected by the
    /// typechecker as `E_LocalBorrowBind`.
    LocalBorrowBindSigil,
    /// `break <expr>` — break takes no value. Loop-as-expression was removed
    /// from the v1 surface (D19, 2026-07-06): the form was a half-wired stub
    /// (unparseable in assignment position, value silently discarded at
    /// lowering). Rejected at parse with a teaching message.
    BreakWithValue,
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
            ParseErrorKind::ExpressionTooDeep { depth, limit } => {
                write!(
                    f,
                    "expression nesting too deep (depth {depth} exceeds the limit of {limit}); \
                     help: break the expression into intermediate variables (let bindings)"
                )
            }
            ParseErrorKind::MissingInitializer => {
                write!(
                    f,
                    "variable declaration requires an initializer; \
                     help: write `Type name = value` (Gorget has no uninitialized-variable form)"
                )
            }
            ParseErrorKind::LocalBorrowBindSigil => {
                write!(
                    f,
                    "local `&`-bindings are not supported: a `Type &name = ...` \
                     declaration would alias a second writable path to the value \
                     (a place has one exclusive writer); \
                     help: pass the borrow at a call site (`f(&name)`) or mutate \
                     the place directly (`name.push(..)`, `name.field = value`)"
                )
            }
            ParseErrorKind::BreakWithValue => {
                write!(
                    f,
                    "break takes no value; loops are not expressions; \
                     help: assign to a variable declared before the loop, then `break`"
                )
            }
        }
    }
}

/// Renders compiler diagnostics to stderr.
pub struct ErrorReporter {
    files: SimpleFiles<String, String>,
    file_id: usize,
    /// For multi-file projects: sorted list of (base_offset, source_len, file_id).
    /// Used to map global spans back to file-local offsets.
    file_ranges: Vec<(usize, usize, usize)>,
}

impl ErrorReporter {
    pub fn new(filename: String, source: String) -> Self {
        let mut files = SimpleFiles::new();
        let file_id = files.add(filename, source);
        Self { files, file_id, file_ranges: Vec::new() }
    }

    /// Create a reporter for multi-file projects.
    /// `file_infos` is a list of (filename, source, base_offset) for each module,
    /// in the order they were loaded.  Spans in the AST use global byte offsets;
    /// this constructor lets the reporter map them back to file-local positions.
    pub fn new_multi(file_infos: Vec<(String, String, usize)>) -> Self {
        let mut files = SimpleFiles::new();
        let mut file_ranges = Vec::new();
        let mut first_file_id = 0;
        for (i, (name, source, base_offset)) in file_infos.iter().enumerate() {
            let len = source.len();
            let fid = files.add(name.clone(), source.clone());
            if i == 0 {
                first_file_id = fid;
            }
            file_ranges.push((*base_offset, len, fid));
        }
        // Sort by base_offset for binary search
        file_ranges.sort_by_key(|(off, _, _)| *off);
        Self { files, file_id: first_file_id, file_ranges }
    }

    /// Map a global byte offset to (file_id, file-local offset).
    fn resolve_offset(&self, global: usize) -> (usize, usize) {
        if self.file_ranges.is_empty() {
            return (self.file_id, global);
        }
        // Binary search: find the last file_range whose base_offset <= global
        let idx = match self.file_ranges.binary_search_by_key(&global, |(off, _, _)| *off) {
            Ok(i) => i,
            Err(i) => i.saturating_sub(1),
        };
        let (base, _len, fid) = self.file_ranges[idx];
        (fid, global.saturating_sub(base))
    }

    /// Check whether a span belongs to the entry file (file_id 0, i.e., the user's source).
    pub fn is_entry_file(&self, span: Span) -> bool {
        let (fid, _) = self.resolve_offset(span.start);
        fid == self.file_id
    }

    /// Create a primary label for a span, resolving to the correct file.
    fn primary_label(&self, span: Span) -> Label<usize> {
        let (fid, local_start) = self.resolve_offset(span.start);
        let local_end = local_start + (span.end - span.start);
        Label::primary(fid, local_start..local_end)
    }

    /// Create a secondary label for a span, resolving to the correct file.
    fn secondary_label(&self, span: Span) -> Label<usize> {
        let (fid, local_start) = self.resolve_offset(span.start);
        let local_end = local_start + (span.end - span.start);
        Label::secondary(fid, local_start..local_end)
    }

    pub fn report_lex_error(&self, err: &LexError) {
        let diag = diagnostic::Diagnostic::error()
            .with_message(err.to_string())
            .with_labels(vec![self.primary_label(err.span)]);
        self.emit(&diag);
    }

    pub fn report_parse_error(&self, err: &ParseError) {
        let diag = diagnostic::Diagnostic::error()
            .with_message(err.to_string())
            .with_labels(vec![self.primary_label(err.span)]);
        self.emit(&diag);
    }

    pub fn report_parse_warning(&self, warn: &ParseWarning) {
        let diag = diagnostic::Diagnostic::warning()
            .with_message(warn.to_string())
            .with_labels(vec![self.primary_label(warn.span)]);
        self.emit(&diag);
    }

    pub fn report_semantic_error(&self, err: &crate::semantic::errors::SemanticError) {
        use crate::semantic::errors::SemanticErrorKind;

        let mut labels = vec![self.primary_label(err.span)];

        // Add secondary labels for errors that reference other locations.
        match &err.kind {
            SemanticErrorKind::DuplicateDefinition { original, .. } => {
                labels.push(
                    self.secondary_label(*original)
                        .with_message("originally defined here"),
                );
            }
            SemanticErrorKind::UseAfterMove { moved_at, .. } => {
                labels.push(
                    self.secondary_label(*moved_at)
                        .with_message("value moved here"),
                );
            }
            SemanticErrorKind::DoubleMove { first_move, .. } => {
                labels.push(
                    self.secondary_label(*first_move)
                        .with_message("first move here"),
                );
            }
            SemanticErrorKind::UseAfterSourceMoved { moved_at, source_name, .. } => {
                labels.push(
                    self.secondary_label(*moved_at)
                        .with_message(format!("`{source_name}` moved here")),
                );
            }
            SemanticErrorKind::DanglingReturn { local_declared_at, local_name, .. } => {
                if let Some(decl_span) = local_declared_at {
                    labels.push(
                        self.secondary_label(*decl_span)
                            .with_message(format!("`{local_name}` declared here — will be dropped when function returns")),
                    );
                }
            }
            SemanticErrorKind::TemporaryBorrow { temp_at, callee, .. } => {
                if let Some(temp_span) = temp_at {
                    labels.push(
                        self.secondary_label(*temp_span)
                            .with_message(format!("temporary from `{callee}()` created here — dropped at end of statement")),
                    );
                }
            }
            _ => {}
        }

        let diag = diagnostic::Diagnostic::error()
            .with_code(err.kind.code())
            .with_message(err.to_string())
            .with_labels(labels);
        self.emit(&diag);
    }

    pub fn report_semantic_warning(&self, warn: &crate::semantic::errors::SemanticWarning) {
        use crate::semantic::errors::SemanticWarningKind;
        let mut labels = vec![self.primary_label(warn.span)];
        let mut notes = Vec::new();

        // Add secondary labels for multi-span warnings.
        if let SemanticWarningKind::StaleSharedCondition {
            derivation_span, await_span, shared_name, ..
        } = &warn.kind {
            if let Some(ds) = derivation_span {
                labels.push(
                    self.secondary_label(*ds)
                        .with_message(format!("read from shared `{shared_name}` here")),
                );
            }
            if let Some(aws) = await_span {
                labels.push(
                    self.secondary_label(*aws)
                        .with_message("token released at this await — value may have changed"),
                );
            }
            notes.push(format!("re-read `{shared_name}` after the await, or use `with {shared_name}:` to auto-refresh across await points"));
        }

        if let SemanticWarningKind::WithCheckThenAct {
            shared_names, condition_span, yield_span,
        } = &warn.kind {
            let names = shared_names.iter().map(|n| format!("`{n}`")).collect::<Vec<_>>().join(", ");
            labels.push(
                self.secondary_label(*condition_span)
                    .with_message(format!("condition reads shared {names} here")),
            );
            labels.push(
                self.secondary_label(*yield_span)
                    .with_message("yield point releases the lock — another task may invalidate the condition"),
            );
            notes.push(format!("move the yield before the branch, or re-check {names} after the yield"));
        }

        if let SemanticWarningKind::StaleSharedWriteBack {
            source_shared_name, derivation_span, yield_span, ..
        } = &warn.kind {
            if let Some(ds) = derivation_span {
                labels.push(
                    self.secondary_label(*ds)
                        .with_message(format!("value derived from shared `{source_shared_name}` here")),
                );
            }
            if let Some(ys) = yield_span {
                labels.push(
                    self.secondary_label(*ys)
                        .with_message("yield point here — value is now stale"),
                );
            }
            notes.push(format!("re-read `{source_shared_name}` after the yield and recompute before writing back"));
        }

        if let SemanticWarningKind::SharedIteratorInvalidation {
            shared_name, iterable_span, yield_span,
        } = &warn.kind {
            labels.push(
                self.secondary_label(*iterable_span)
                    .with_message(format!("iterating over shared `{shared_name}` here")),
            );
            labels.push(
                self.secondary_label(*yield_span)
                    .with_message("yield point releases the lock — collection may change between iterations"),
            );
            notes.push(format!("collect into a local copy before iterating, or move the yield outside the loop"));
        }

        if let SemanticWarningKind::SpawnWithTrackedBinding {
            shared_name, ..
        } = &warn.kind {
            notes.push(format!("`{shared_name}` is managed by a `with` block — the spawned task won't hold the lock"));
            notes.push("copy the value before spawning, or pass the underlying shared variable with `&`".to_string());
        }

        if let SemanticWarningKind::UnusedVariable { name } = &warn.kind {
            notes.push(format!("prefix the variable name with `_` to suppress: `_{name}`"));
        }

        if let SemanticWarningKind::UnusedImport { name } = &warn.kind {
            notes.push(format!("remove the import of `{name}` or use it in code"));
        }

        if let SemanticWarningKind::UncheckedUnwrap { .. } = &warn.kind {
            notes.push("check with `.is_some()` or use `match` to handle the None/Error case".to_string());
        }

        if let SemanticWarningKind::CouldBeConst { name } = &warn.kind {
            notes.push(format!("declare as `const {name} = ...` if the value is known at compile time"));
        }

        if let SemanticWarningKind::NeedlessMutableBorrow { name } = &warn.kind {
            notes.push(format!("if the function only reads from `{name}`, pass it as a bare parameter instead"));
        }

        if let SemanticWarningKind::DeadBareParamWrite { name, param_span } = &warn.kind {
            labels.push(
                self.secondary_label(*param_span)
                    .with_message(format!("`{name}` is declared as a bare (read-only borrow) parameter here")),
            );
            notes.push("if a private scratch copy is intended, read the copy after the write — or bind an explicit local instead".to_string());
        }

        let mut diag = diagnostic::Diagnostic::warning()
            .with_message(warn.to_string())
            .with_labels(labels);
        if !notes.is_empty() {
            diag = diag.with_notes(notes);
        }
        self.emit(&diag);
    }

    /// Resolve a span to (filename, line_number, column_number) for structured output.
    pub fn span_location(&self, span: Span) -> (String, usize, usize) {
        use codespan_reporting::files::Files;
        let (fid, local_offset) = self.resolve_offset(span.start);
        let name = self.files.name(fid).map(|n| n.clone()).unwrap_or_default();
        if let Ok(loc) = self.files.location(fid, local_offset) {
            (name, loc.line_number, loc.column_number)
        } else {
            (name, 0, 0)
        }
    }

    /// Emit a warning with a primary span and a note string.
    pub fn emit_warning_with_note(&self, span: Span, message: &str, note: &str) {
        let label = self.primary_label(span);
        let diag = diagnostic::Diagnostic::warning()
            .with_message(message)
            .with_labels(vec![label])
            .with_notes(vec![note.to_string()]);
        self.emit(&diag);
    }

    fn emit(&self, diag: &diagnostic::Diagnostic<usize>) {
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = term::Config::default();
        let _ = term::emit(&mut writer.lock(), &config, &self.files, diag);
    }
}
