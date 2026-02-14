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
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    StarEq,
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
            RawToken::MinusEq => write!(f, "'-='"),
            RawToken::StarEq => write!(f, "'*='"),
            RawToken::SlashEq => write!(f, "'/='"),
            RawToken::PercentEq => write!(f, "'%='"),
            RawToken::PlusPercent => write!(f, "'+%'"),
            RawToken::MinusPercent => write!(f, "'-%'"),
            RawToken::StarPercent => write!(f, "'*%'"),
            RawToken::PlusPercentEq => write!(f, "'+%='"),
            RawToken::MinusPercentEq => write!(f, "'-%='"),
            RawToken::StarPercentEq => write!(f, "'*%='"),
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
    Char,
    Str,
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
    None,
    Some,
    Ok,
    Error,

    // Error handling
    Throw,
    Throws,
    Try,

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
    Live,
    Life,

    // Concurrency
    Async,
    Await,
    Spawn,

    // Safety
    Unsafe,
    Extern,

    // Self
    SelfLower,
    SelfUpper,

    // Smart pointer types
    Box,
    Rc,
    Arc,
    Weak,
    Cell,
    RefCell,
    Mutex,
    RwLock,

    // Ownership keywords
    Moving,
    Mutable,

    // Testing
    Test,
    Suite,

    // Special identifiers
    It,
    Panic,
    Assert,
}

impl Keyword {
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
            "char" => Some(Keyword::Char),
            "str" => Some(Keyword::Str),
            "String" => Some(Keyword::StringType),
            "void" => Some(Keyword::Void),
            "auto" => Some(Keyword::Auto),
            "const" => Some(Keyword::Const),
            "struct" => Some(Keyword::Struct),
            "enum" => Some(Keyword::Enum),
            "trait" => Some(Keyword::Trait),
            "equip" => Some(Keyword::Equip),
            "public" => Some(Keyword::Public),
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
            "None" => Some(Keyword::None),
            "Some" => Some(Keyword::Some),
            "Ok" => Some(Keyword::Ok),
            "Error" => Some(Keyword::Error),
            "throw" => Some(Keyword::Throw),
            "throws" => Some(Keyword::Throws),
            "try" => Some(Keyword::Try),
            "import" => Some(Keyword::Import),
            "from" => Some(Keyword::From),
            "directive" => Some(Keyword::Directive),
            "with" => Some(Keyword::With),
            "as" => Some(Keyword::As),
            "via" => Some(Keyword::Via),
            "where" => Some(Keyword::Where),
            "extends" => Some(Keyword::Extends),
            "live" => Some(Keyword::Live),
            "life" => Some(Keyword::Life),
            "async" => Some(Keyword::Async),
            "await" => Some(Keyword::Await),
            "spawn" => Some(Keyword::Spawn),
            "unsafe" => Some(Keyword::Unsafe),
            "extern" => Some(Keyword::Extern),
            "self" => Some(Keyword::SelfLower),
            "Self" => Some(Keyword::SelfUpper),
            "Box" => Some(Keyword::Box),
            "Rc" => Some(Keyword::Rc),
            "Arc" => Some(Keyword::Arc),
            "Weak" => Some(Keyword::Weak),
            "Cell" => Some(Keyword::Cell),
            "RefCell" => Some(Keyword::RefCell),
            "Mutex" => Some(Keyword::Mutex),
            "RwLock" => Some(Keyword::RwLock),
            "moving" => Some(Keyword::Moving),
            "mutable" => Some(Keyword::Mutable),
            "test" => Some(Keyword::Test),
            "suite" => Some(Keyword::Suite),
            "it" => Some(Keyword::It),
            "panic" => Some(Keyword::Panic),
            "assert" => Some(Keyword::Assert),
            _ => Option::None,
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
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
            Keyword::Char => "char",
            Keyword::Str => "str",
            Keyword::StringType => "String",
            Keyword::Void => "void",
            Keyword::Auto => "auto",
            Keyword::Const => "const",
            Keyword::Struct => "struct",
            Keyword::Enum => "enum",
            Keyword::Trait => "trait",
            Keyword::Equip => "equip",
            Keyword::Public => "public",
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
            Keyword::None => "None",
            Keyword::Some => "Some",
            Keyword::Ok => "Ok",
            Keyword::Error => "Error",
            Keyword::Throw => "throw",
            Keyword::Throws => "throws",
            Keyword::Try => "try",
            Keyword::Import => "import",
            Keyword::From => "from",
            Keyword::Directive => "directive",
            Keyword::With => "with",
            Keyword::As => "as",
            Keyword::Via => "via",
            Keyword::Where => "where",
            Keyword::Extends => "extends",
            Keyword::Live => "live",
            Keyword::Life => "life",
            Keyword::Async => "async",
            Keyword::Await => "await",
            Keyword::Spawn => "spawn",
            Keyword::Unsafe => "unsafe",
            Keyword::Extern => "extern",
            Keyword::SelfLower => "self",
            Keyword::SelfUpper => "Self",
            Keyword::Box => "Box",
            Keyword::Rc => "Rc",
            Keyword::Arc => "Arc",
            Keyword::Weak => "Weak",
            Keyword::Cell => "Cell",
            Keyword::RefCell => "RefCell",
            Keyword::Mutex => "Mutex",
            Keyword::RwLock => "RwLock",
            Keyword::Moving => "moving",
            Keyword::Mutable => "mutable",
            Keyword::Test => "test",
            Keyword::Suite => "suite",
            Keyword::It => "it",
            Keyword::Panic => "panic",
            Keyword::Assert => "assert",
        };
        write!(f, "'{s}'")
    }
}

/// The final token type emitted by the indentation-aware lexer.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords and identifiers
    Keyword(Keyword),
    Identifier(String),

    // Literals
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(StringLit),
    CharLiteral(char),
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
    MinusEq,
    StarEq,
    SlashEq,
    PercentEq,
    PlusPercent,
    MinusPercent,
    StarPercent,
    PlusPercentEq,
    MinusPercentEq,
    StarPercentEq,
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
    Comment(String),

    // End of file
    Eof,

    // Error recovery
    Error(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Keyword(kw) => write!(f, "{kw}"),
            Token::Identifier(name) => write!(f, "identifier '{name}'"),
            Token::IntLiteral(n) => write!(f, "integer {n}"),
            Token::FloatLiteral(n) => write!(f, "float {n}"),
            Token::StringLiteral(_) => write!(f, "string"),
            Token::CharLiteral(c) => write!(f, "char '{c}'"),
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
            Token::MinusEq => write!(f, "'-='"),
            Token::StarEq => write!(f, "'*='"),
            Token::SlashEq => write!(f, "'/='"),
            Token::PercentEq => write!(f, "'%='"),
            Token::PlusPercent => write!(f, "'+%'"),
            Token::MinusPercent => write!(f, "'-%'"),
            Token::StarPercent => write!(f, "'*%'"),
            Token::PlusPercentEq => write!(f, "'+%='"),
            Token::MinusPercentEq => write!(f, "'-%='"),
            Token::StarPercentEq => write!(f, "'*%='"),
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
            Token::Error(msg) => write!(f, "error: {msg}"),
        }
    }
}

/// A processed string literal with possible interpolation segments.
#[derive(Debug, Clone, PartialEq)]
pub struct StringLit {
    pub kind: StringKind,
    pub segments: Vec<StringSegment>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringKind {
    Normal,
    Raw,
    MultiLine,
    Byte,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringSegment {
    Literal(String),
    Interpolation(String),
}
