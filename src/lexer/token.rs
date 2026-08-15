use crate::intern::Symbol;
use logos::Logos;
use std::fmt;

/// Raw tokens produced by the logos lexer (before indentation processing).
/// Whitespace at line starts is handled separately by the indentation layer.
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t]+")]
pub enum RawToken {
    // ── Numeric Literals ─────────────────────────────────────
    #[regex(r"0x[0-9a-fA-F][0-9a-fA-F_]*")]
    HexLiteral,

    #[regex(r"0o[0-7][0-7_]*")]
    OctalLiteral,

    #[regex(r"0b[01][01_]*")]
    BinaryLiteral,

    #[regex(r"[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9]+)?")]
    FloatLiteral,

    #[regex(r"[0-9][0-9_]*")]
    IntLiteral,

    // ── Multi-char operators (longest match first) ───────────
    #[token("..=")]
    DotDotEq,
    #[token("..")]
    DotDot,
    #[token("?.")]
    QuestionDot,
    #[token("??")]
    DoubleQuestion,
    #[token("==")]
    EqEq,
    #[token("!=")]
    BangEq,
    #[token("<=")]
    LtEq,
    #[token(">=")]
    GtEq,
    #[token("+=")]
    PlusEq,
    #[token("->")]
    Arrow,
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    StarEq,
    #[token("**")]
    StarStar,
    #[token("**=")]
    StarStarEq,
    #[token("/=")]
    SlashEq,
    #[token("%=")]
    PercentEq,
    #[token("+%=")]
    PlusPercentEq,
    #[token("-%=")]
    MinusPercentEq,
    #[token("*%=")]
    StarPercentEq,
    #[token("+%")]
    PlusPercent,
    #[token("-%")]
    MinusPercent,
    #[token("*%")]
    StarPercent,
    // D26 fallible arithmetic operators (Round XXXIII Batch C1). Each `!`-suffixed
    // form converts a would-be-trap into `Result[T, ArithError]`; auto-propagates
    // via D29 disposition. Precedence mirrors the plain operator.
    // Compound-fallible-assign forms (`+!=`, etc.) are v1-EXCLUDED per amendment
    // `decisions.md:945`; the reject-tokens below give them a distinct diagnostic
    // span instead of a maximal-munch ambiguity trap (they never reach parser).
    #[token("+!=")]
    PlusBangEq,
    #[token("-!=")]
    MinusBangEq,
    #[token("*!=")]
    StarBangEq,
    #[token("/!=")]
    SlashBangEq,
    #[token("%!=")]
    PercentBangEq,
    #[token("<<!=")]
    LtLtBangEq,
    #[token(">>!=")]
    GtGtBangEq,
    #[token("<<!")]
    LtLtBang,
    #[token(">>!")]
    GtGtBang,
    #[token("+!")]
    PlusBang,
    #[token("-!")]
    MinusBang,
    #[token("*!")]
    StarBang,
    #[token("/!")]
    SlashBang,
    #[token("%!")]
    PercentBang,
    #[token("<<=")]
    LtLtEq,
    #[token(">>=")]
    GtGtEq,
    #[token("<<")]
    LtLt,
    #[token(">>")]
    GtGt,
    #[token("&=")]
    AmpersandEq,
    #[token("|=")]
    PipeEq,
    #[token("^=")]
    CaretEq,

    // ── Single-char operators & delimiters ────────────────────
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("=")]
    Eq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("?")]
    Question,
    #[token("!")]
    Bang,
    #[token("&")]
    Ampersand,
    #[token("@")]
    At,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("~")]
    Tilde,
    #[token("_")]
    Underscore,

    // ── Newline ──────────────────────────────────────────────
    #[token("\n")]
    Newline,

    // ── Identifier (catch-all after keywords) ────────────────
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", priority = 1)]
    Identifier,
}

impl fmt::Display for RawToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RawToken::IntLiteral => write!(f, "integer"),
            RawToken::HexLiteral => write!(f, "hex integer"),
            RawToken::OctalLiteral => write!(f, "octal integer"),
            RawToken::BinaryLiteral => write!(f, "binary integer"),
            RawToken::FloatLiteral => write!(f, "float"),
            RawToken::DotDotEq => write!(f, "'..='"),
            RawToken::DotDot => write!(f, "'..'"),
            RawToken::QuestionDot => write!(f, "'?.'"),
            RawToken::DoubleQuestion => write!(f, "'??'"),
            RawToken::EqEq => write!(f, "'=='"),
            RawToken::BangEq => write!(f, "'!='"),
            RawToken::LtEq => write!(f, "'<='"),
            RawToken::GtEq => write!(f, "'>='"),
            RawToken::PlusEq => write!(f, "'+='"),
            RawToken::Arrow => write!(f, "'->'"),
            RawToken::MinusEq => write!(f, "'-='"),
            RawToken::StarEq => write!(f, "'*='"),
            RawToken::StarStar => write!(f, "'**'"),
            RawToken::StarStarEq => write!(f, "'**='"),
            RawToken::SlashEq => write!(f, "'/='"),
            RawToken::PercentEq => write!(f, "'%='"),
            RawToken::PlusPercent => write!(f, "'+%'"),
            RawToken::MinusPercent => write!(f, "'-%'"),
            RawToken::StarPercent => write!(f, "'*%'"),
            RawToken::PlusPercentEq => write!(f, "'+%='"),
            RawToken::MinusPercentEq => write!(f, "'-%='"),
            RawToken::StarPercentEq => write!(f, "'*%='"),
            RawToken::PlusBang => write!(f, "'+!'"),
            RawToken::MinusBang => write!(f, "'-!'"),
            RawToken::StarBang => write!(f, "'*!'"),
            RawToken::SlashBang => write!(f, "'/!'"),
            RawToken::PercentBang => write!(f, "'%!'"),
            RawToken::LtLtBang => write!(f, "'<<!'"),
            RawToken::GtGtBang => write!(f, "'>>!'"),
            RawToken::PlusBangEq => write!(f, "'+!='"),
            RawToken::MinusBangEq => write!(f, "'-!='"),
            RawToken::StarBangEq => write!(f, "'*!='"),
            RawToken::SlashBangEq => write!(f, "'/!='"),
            RawToken::PercentBangEq => write!(f, "'%!='"),
            RawToken::LtLtBangEq => write!(f, "'<<!='"),
            RawToken::GtGtBangEq => write!(f, "'>>!='"),
            RawToken::Plus => write!(f, "'+'"),
            RawToken::Minus => write!(f, "'-'"),
            RawToken::Star => write!(f, "'*'"),
            RawToken::Slash => write!(f, "'/'"),
            RawToken::Percent => write!(f, "'%'"),
            RawToken::Eq => write!(f, "'='"),
            RawToken::Lt => write!(f, "'<'"),
            RawToken::Gt => write!(f, "'>'"),
            RawToken::LParen => write!(f, "'('"),
            RawToken::RParen => write!(f, "')'"),
            RawToken::LBracket => write!(f, "'['"),
            RawToken::RBracket => write!(f, "']'"),
            RawToken::LBrace => write!(f, "'{{'"),
            RawToken::RBrace => write!(f, "'}}'"),
            RawToken::Colon => write!(f, "':'"),
            RawToken::Comma => write!(f, "','"),
            RawToken::Dot => write!(f, "'.'"),
            RawToken::Question => write!(f, "'?'"),
            RawToken::Bang => write!(f, "'!'"),
            RawToken::Ampersand => write!(f, "'&'"),
            RawToken::At => write!(f, "'@'"),
            RawToken::Pipe => write!(f, "'|'"),
            RawToken::Caret => write!(f, "'^'"),
            RawToken::Tilde => write!(f, "'~'"),
            RawToken::LtLt => write!(f, "'<<'"),
            RawToken::GtGt => write!(f, "'>>'"),
            RawToken::LtLtEq => write!(f, "'<<='"),
            RawToken::GtGtEq => write!(f, "'>>='"),
            RawToken::AmpersandEq => write!(f, "'&='"),
            RawToken::PipeEq => write!(f, "'|='"),
            RawToken::CaretEq => write!(f, "'^='"),
            RawToken::Underscore => write!(f, "'_'"),
            RawToken::Newline => write!(f, "newline"),
            RawToken::Identifier => write!(f, "identifier"),
        }
    }
}

/// Keywords recognized from identifiers after lexing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Keyword {
    // Type keywords
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float,
    Float32,
    Float64,
    Bool,
    StringType,
    Void,
    Auto,

    // Declaration keywords
    Const,
    Struct,
    Enum,
    Trait,
    Equip,
    Public,
    Private,
    Static,
    Type,
    Newtype,

    // Control flow
    If,
    Elif,
    Else,
    For,
    While,
    Loop,
    In,
    Match,
    Case,
    Break,
    Continue,
    Return,
    Do,
    Pass,

    // Logical operators
    And,
    Or,
    Not,
    Is,

    // Literals
    True,
    False,
    // Note: Option / Result variant constructors (None, Some, Ok, Error)
    // used to be keywords here. They're now plain identifiers registered
    // as prelude variants in `semantic::resolve` — that lets `Error`
    // etc. be used as trait names while still resolving correctly in
    // pattern / expression position via the prelude scope.

    // Error handling
    Throw,
    Throws,
    Rethrow,

    // Imports
    Import,
    From,

    // Directives
    Directive,

    // Resource/scope
    With,
    As,
    Via,

    // Generics/type constraints
    Where,
    Extends,

    // Concurrency
    Async,
    Await,
    Spawn,
    Blocking,
    Unchecked,
    Select,
    Shared,

    // Safety
    Unsafe,
    Extern,
    /// `noreturn` — extern function never returns control to caller (calls like
    /// `_Noreturn` C functions: `exit`, `abort`). Lets the typechecker treat
    /// such calls as Never-typed and the IR terminate the basic block.
    Noreturn,

    // Self
    SelfLower,
    SelfUpper,

    // Smart pointer / concurrency types — demoted to identifiers.
    // Box, Rc, Arc, Weak, Cell, RefCell, Mutex, RwLock are regular identifiers.

    // Error recovery
    Catch,

    // Ownership keywords
    Move,
    Mutable,

    // Testing
    Test,
    Suite,
    Bench,
    Snapshot,

    // Compile-time
    Meta,

    // Special identifiers
    It,
    Assert,
}

impl Keyword {
    /// Returns true if this keyword names a type (primitives, smart pointers, etc.).
    /// Single source of truth used by `is_type_start()`, `looks_like_module_var_decl()`,
    /// and pattern-matching type guards.
    pub fn is_type_keyword(&self) -> bool {
        matches!(
            self,
            Keyword::Int
                | Keyword::Int8
                | Keyword::Int16
                | Keyword::Int32
                | Keyword::Int64
                | Keyword::Uint
                | Keyword::Uint8
                | Keyword::Uint16
                | Keyword::Uint32
                | Keyword::Uint64
                | Keyword::Float
                | Keyword::Float32
                | Keyword::Float64
                | Keyword::Bool
                | Keyword::StringType
                | Keyword::Void
                | Keyword::Auto
                | Keyword::SelfUpper
        )
    }

    /// Return the keyword's source-level name (e.g., `"Some"`, `"int"`, `"String"`).
    pub fn as_name(&self) -> &'static str {
        match self {
            Keyword::Int => "int",
            Keyword::Int8 => "int8",
            Keyword::Int16 => "int16",
            Keyword::Int32 => "int32",
            Keyword::Int64 => "int64",
            Keyword::Uint => "uint",
            Keyword::Uint8 => "uint8",
            Keyword::Uint16 => "uint16",
            Keyword::Uint32 => "uint32",
            Keyword::Uint64 => "uint64",
            Keyword::Float => "float",
            Keyword::Float32 => "float32",
            Keyword::Float64 => "float64",
            Keyword::Bool => "bool",
            Keyword::StringType => "String",
            Keyword::Void => "void",
            Keyword::Auto => "auto",
            Keyword::Const => "const",
            Keyword::Struct => "struct",
            Keyword::Enum => "enum",
            Keyword::Trait => "trait",
            Keyword::Equip => "equip",
            Keyword::Public => "public",
            Keyword::Private => "private",
            Keyword::Static => "static",
            Keyword::Type => "type",
            Keyword::Newtype => "newtype",
            Keyword::If => "if",
            Keyword::Elif => "elif",
            Keyword::Else => "else",
            Keyword::For => "for",
            Keyword::While => "while",
            Keyword::Loop => "loop",
            Keyword::In => "in",
            Keyword::Match => "match",
            Keyword::Case => "case",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
            Keyword::Return => "return",
            Keyword::Do => "do",
            Keyword::Pass => "pass",
            Keyword::And => "and",
            Keyword::Or => "or",
            Keyword::Not => "not",
            Keyword::Is => "is",
            Keyword::True => "true",
            Keyword::False => "false",
            Keyword::Throw => "throw",
            Keyword::Throws => "throws",
            Keyword::Rethrow => "rethrow",
            Keyword::Import => "import",
            Keyword::From => "from",
            Keyword::Directive => "directive",
            Keyword::With => "with",
            Keyword::As => "as",
            Keyword::Via => "via",
            Keyword::Where => "where",
            Keyword::Extends => "extends",
            Keyword::Async => "async",
            Keyword::Await => "await",
            Keyword::Spawn => "spawn",
            Keyword::Blocking => "blocking",
            Keyword::Unchecked => "unchecked",
            Keyword::Select => "select",
            Keyword::Shared => "shared",
            Keyword::Unsafe => "unsafe",
            Keyword::Extern => "extern",
            Keyword::Noreturn => "noreturn",
            Keyword::SelfLower => "self",
            Keyword::SelfUpper => "Self",
            Keyword::Catch => "catch",
            Keyword::Move => "move",
            Keyword::Mutable => "mutable",
            Keyword::Test => "test",
            Keyword::Suite => "suite",
            Keyword::Bench => "bench",
            Keyword::Snapshot => "snapshot",
            Keyword::Meta => "meta",
            Keyword::It => "it",
            Keyword::Assert => "assert",
        }
    }

    /// Try to match a string to a keyword. Returns None for regular identifiers.
    pub fn from_str(s: &str) -> Option<Keyword> {
        match s {
            "int" => Some(Keyword::Int),
            "int8" => Some(Keyword::Int8),
            "int16" => Some(Keyword::Int16),
            "int32" => Some(Keyword::Int32),
            "int64" => Some(Keyword::Int64),
            "uint" => Some(Keyword::Uint),
            "uint8" => Some(Keyword::Uint8),
            "uint16" => Some(Keyword::Uint16),
            "uint32" => Some(Keyword::Uint32),
            "uint64" => Some(Keyword::Uint64),
            "float" => Some(Keyword::Float),
            "float32" => Some(Keyword::Float32),
            "float64" => Some(Keyword::Float64),
            "bool" => Some(Keyword::Bool),
            "byte" => Some(Keyword::Uint8), // `byte` is a user-facing alias for `uint8`
            "String" => Some(Keyword::StringType),
            "void" => Some(Keyword::Void),
            "auto" => Some(Keyword::Auto),
            "const" => Some(Keyword::Const),
            "struct" => Some(Keyword::Struct),
            "enum" => Some(Keyword::Enum),
            "trait" => Some(Keyword::Trait),
            "equip" => Some(Keyword::Equip),
            "public" => Some(Keyword::Public),
            "private" => Some(Keyword::Private),
            "static" => Some(Keyword::Static),
            "type" => Some(Keyword::Type),
            "newtype" => Some(Keyword::Newtype),
            "if" => Some(Keyword::If),
            "elif" => Some(Keyword::Elif),
            "else" => Some(Keyword::Else),
            "for" => Some(Keyword::For),
            "while" => Some(Keyword::While),
            "loop" => Some(Keyword::Loop),
            "in" => Some(Keyword::In),
            "match" => Some(Keyword::Match),
            "case" => Some(Keyword::Case),
            "break" => Some(Keyword::Break),
            "continue" => Some(Keyword::Continue),
            "return" => Some(Keyword::Return),
            "do" => Some(Keyword::Do),
            "pass" => Some(Keyword::Pass),
            "and" => Some(Keyword::And),
            "or" => Some(Keyword::Or),
            "not" => Some(Keyword::Not),
            "is" => Some(Keyword::Is),
            "true" => Some(Keyword::True),
            "false" => Some(Keyword::False),
            // `None` / `Some` / `Ok` / `Error` are NOT keywords — they
            // lex as identifiers and resolve via the prelude scope
            // (semantic::resolve registers them as variants).
            "throw" => Some(Keyword::Throw),
            "throws" => Some(Keyword::Throws),
            "rethrow" => Some(Keyword::Rethrow),
            "import" => Some(Keyword::Import),
            "from" => Some(Keyword::From),
            "directive" => Some(Keyword::Directive),
            "with" => Some(Keyword::With),
            "as" => Some(Keyword::As),
            "via" => Some(Keyword::Via),
            "where" => Some(Keyword::Where),
            "extends" => Some(Keyword::Extends),
            "async" => Some(Keyword::Async),
            "await" => Some(Keyword::Await),
            "spawn" => Some(Keyword::Spawn),
            "blocking" => Some(Keyword::Blocking),
            "unchecked" => Some(Keyword::Unchecked),
            "select" => Some(Keyword::Select),
            "shared" => Some(Keyword::Shared),
            "unsafe" => Some(Keyword::Unsafe),
            "extern" => Some(Keyword::Extern),
            "noreturn" => Some(Keyword::Noreturn),
            "self" => Some(Keyword::SelfLower),
            "Self" => Some(Keyword::SelfUpper),
            "catch" => Some(Keyword::Catch),
            "move" => Some(Keyword::Move),
            "mutable" => Some(Keyword::Mutable),
            "test" => Some(Keyword::Test),
            "suite" => Some(Keyword::Suite),
            "bench" => Some(Keyword::Bench),
            "snapshot" => Some(Keyword::Snapshot),
            "meta" => Some(Keyword::Meta),
            // "mod" removed as keyword — now .mod() method on ints
            "it" => Some(Keyword::It),
            "assert" => Some(Keyword::Assert),
            _ => None,
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}'", self.as_name())
    }
}

/// Where a comment sits on its own source line.
///
/// The lexer has TWO comment producers and they ARE the two placements — a
/// comment-only line is scanned by `emit_comment_token`, a comment after code
/// by the `#` arm of `tokenize_line_content` — so the placement is recorded at
/// the producer rather than re-derived downstream by scanning back for a
/// newline (Layering rule 2).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentPlacement {
    /// Nothing but whitespace precedes the `#` on its source line.
    OwnLine,
    /// Code precedes the `#` on its source line.
    Trailing,
}

/// A comment token's payload: the text plus the LEXICAL attachment facts the
/// lexer knows for free and every downstream reader would otherwise have to
/// reconstruct from the source bytes.
///
/// `Token::Comment`'s span is normalized by BOTH producers to start at the `#`
/// itself, so `span.start` means one thing on one axis. (It used to mean the
/// LINE start for a comment-only line and the `#` for a trailing one — one
/// axis, two meanings.)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CommentToken {
    /// The comment text, `#` included, to end of line.
    pub text: String,
    /// Byte offset of the comment's own line start (base-offset applied, like
    /// every span position). `line_start == previous_comment.span.end + 1`
    /// is exactly "directly below, nothing in between" — a comment token's
    /// span ends AT the `\n`, so the next line begins one byte later. That is
    /// how `CommentTable::build` decides run membership without re-reading the
    /// source.
    pub line_start: usize,
    /// CHARACTER column of the `#` on its line. Char, not byte: a trailing
    /// comment's line may hold non-ASCII code before the `#`, and the column
    /// is a display quantity.
    pub hash_col: usize,
    pub placement: CommentPlacement,
}

/// The final token type emitted by the indentation-aware lexer.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords and identifiers
    Keyword(Keyword),
    Identifier(Symbol),

    // Literals
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(StringLiteral),
    BoolLiteral(bool),

    // Operators (single-char)
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Eq,
    Lt,
    Gt,
    Bang,
    Ampersand,
    Pipe,
    Caret,
    Tilde,
    Dot,
    Question,
    At,
    Underscore,

    // Operators (multi-char)
    EqEq,
    BangEq,
    LtEq,
    GtEq,
    LtLt,
    GtGt,
    LtLtEq,
    GtGtEq,
    AmpersandEq,
    PipeEq,
    CaretEq,
    PlusEq,
    Arrow,
    MinusEq,
    StarEq,
    StarStar,
    StarStarEq,
    SlashEq,
    PercentEq,
    PlusPercent,
    MinusPercent,
    StarPercent,
    PlusPercentEq,
    MinusPercentEq,
    StarPercentEq,
    // D26 fallible arithmetic operators (Round XXXIII Batch C1).
    PlusBang,
    MinusBang,
    StarBang,
    SlashBang,
    PercentBang,
    LtLtBang,
    GtGtBang,
    // D26 compound-fallible-assign reject tokens (v1-EXCLUDED per
    // `decisions.md:945`) — kept as distinct tokens so lexer/parser can
    // produce E_CompoundFallibleAssignExcluded with a precise span.
    PlusBangEq,
    MinusBangEq,
    StarBangEq,
    SlashBangEq,
    PercentBangEq,
    LtLtBangEq,
    GtGtBangEq,
    DotDot,
    DotDotEq,
    QuestionDot,
    DoubleQuestion,

    // Delimiters
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Colon,
    Comma,

    // Indentation
    Indent,
    Dedent,
    Newline,

    // Documentation
    DocComment(String),

    // Comments
    Comment(CommentToken),

    // End of file
    Eof,

    // Error recovery (details are in Lexer::errors, not in the token)
    Error,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Keyword(kw) => write!(f, "{kw}"),
            Token::Identifier(sym) => write!(f, "identifier '{sym}'"),
            Token::IntLiteral(n) => write!(f, "integer {n}"),
            Token::FloatLiteral(n) => write!(f, "float {n}"),
            Token::StringLiteral(_) => write!(f, "string"),
            Token::BoolLiteral(b) => write!(f, "{b}"),
            Token::Plus => write!(f, "'+'"),
            Token::Minus => write!(f, "'-'"),
            Token::Star => write!(f, "'*'"),
            Token::Slash => write!(f, "'/'"),
            Token::Percent => write!(f, "'%'"),
            Token::Eq => write!(f, "'='"),
            Token::Lt => write!(f, "'<'"),
            Token::Gt => write!(f, "'>'"),
            Token::Bang => write!(f, "'!'"),
            Token::Ampersand => write!(f, "'&'"),
            Token::Pipe => write!(f, "'|'"),
            Token::Caret => write!(f, "'^'"),
            Token::Tilde => write!(f, "'~'"),
            Token::Dot => write!(f, "'.'"),
            Token::Question => write!(f, "'?'"),
            Token::At => write!(f, "'@'"),
            Token::Underscore => write!(f, "'_'"),
            Token::EqEq => write!(f, "'=='"),
            Token::BangEq => write!(f, "'!='"),
            Token::LtEq => write!(f, "'<='"),
            Token::GtEq => write!(f, "'>='"),
            Token::LtLt => write!(f, "'<<'"),
            Token::GtGt => write!(f, "'>>'"),
            Token::LtLtEq => write!(f, "'<<='"),
            Token::GtGtEq => write!(f, "'>>='"),
            Token::AmpersandEq => write!(f, "'&='"),
            Token::PipeEq => write!(f, "'|='"),
            Token::CaretEq => write!(f, "'^='"),
            Token::PlusEq => write!(f, "'+='"),
            Token::Arrow => write!(f, "'->'"),
            Token::MinusEq => write!(f, "'-='"),
            Token::StarEq => write!(f, "'*='"),
            Token::StarStar => write!(f, "'**'"),
            Token::StarStarEq => write!(f, "'**='"),
            Token::SlashEq => write!(f, "'/='"),
            Token::PercentEq => write!(f, "'%='"),
            Token::PlusPercent => write!(f, "'+%'"),
            Token::MinusPercent => write!(f, "'-%'"),
            Token::StarPercent => write!(f, "'*%'"),
            Token::PlusPercentEq => write!(f, "'+%='"),
            Token::MinusPercentEq => write!(f, "'-%='"),
            Token::StarPercentEq => write!(f, "'*%='"),
            Token::PlusBang => write!(f, "'+!'"),
            Token::MinusBang => write!(f, "'-!'"),
            Token::StarBang => write!(f, "'*!'"),
            Token::SlashBang => write!(f, "'/!'"),
            Token::PercentBang => write!(f, "'%!'"),
            Token::LtLtBang => write!(f, "'<<!'"),
            Token::GtGtBang => write!(f, "'>>!'"),
            Token::PlusBangEq => write!(f, "'+!='"),
            Token::MinusBangEq => write!(f, "'-!='"),
            Token::StarBangEq => write!(f, "'*!='"),
            Token::SlashBangEq => write!(f, "'/!='"),
            Token::PercentBangEq => write!(f, "'%!='"),
            Token::LtLtBangEq => write!(f, "'<<!='"),
            Token::GtGtBangEq => write!(f, "'>>!='"),
            Token::DotDot => write!(f, "'..'"),
            Token::DotDotEq => write!(f, "'..='"),
            Token::QuestionDot => write!(f, "'?.'"),
            Token::DoubleQuestion => write!(f, "'??'"),
            Token::LParen => write!(f, "'('"),
            Token::RParen => write!(f, "')'"),
            Token::LBracket => write!(f, "'['"),
            Token::RBracket => write!(f, "']'"),
            Token::LBrace => write!(f, "'{{'"),
            Token::RBrace => write!(f, "'}}'"),
            Token::Colon => write!(f, "':'"),
            Token::Comma => write!(f, "','"),
            Token::Indent => write!(f, "INDENT"),
            Token::Dedent => write!(f, "DEDENT"),
            Token::Newline => write!(f, "NEWLINE"),
            Token::DocComment(_) => write!(f, "doc comment"),
            Token::Comment(_) => write!(f, "comment"),
            Token::Eof => write!(f, "end of file"),
            Token::Error => write!(f, "error"),
        }
    }
}

/// A processed string literal with possible interpolation segments.
#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub kind: StringKind,
    pub segments: Vec<StringSegment>,
}

impl StringLiteral {
    /// Extract the plain text content, ignoring any interpolation segments.
    pub fn as_plain_text(&self) -> String {
        self.segments
            .iter()
            .filter_map(|seg| {
                if let StringSegment::Literal(l) = seg {
                    Some(l.as_str())
                } else {
                    None
                }
            })
            .collect()
    }

    /// Returns true if the string contains any interpolation segments.
    pub fn has_interpolation(&self) -> bool {
        self.segments
            .iter()
            .any(|seg| matches!(seg, StringSegment::Interpolation(_, _)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringKind {
    Normal,
    Format, // f"..." or f'...' — formatted/interpolating string (opt-in)
    Raw,
    MultiLine,
    Byte,
    CStr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringSegment {
    Literal(String),
    /// (expression_text, optional_format_spec)
    /// e.g., `{x:.2f}` → Interpolation("x", Some(".2f"))
    Interpolation(String, Option<String>),
}
