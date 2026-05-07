//! Typed runtime function table.
//!
//! Single source of truth for the Gorget runtime API at the IR level.
//! Replaces the string-keyed `runtime_extern_sig()` lookup with an enum
//! whose variants are the only legal way to name a runtime function in
//! `Inst::CallRuntime` (added in A1 — see
//! `docs/internals/lir-correctness-roadmap.md`).
//!
//! Each variant has:
//! * a stable C symbol name (`c_name`),
//! * a const-buildable signature (`signature`),
//! * an `AbiKind` per parameter (resolved alongside the type at call sites).
//!
//! Variants and their `REGISTRY` entries are kept in lockstep by the
//! `runtime_table!` macro — adding a runtime function means appending one
//! line. Drift between the enum's `as usize` ordinal and `REGISTRY`'s index
//! is structurally impossible.

use std::sync::OnceLock;

use rustc_hash::FxHashMap;

use crate::ir::abi::AbiKind;
use crate::lir::types::StructRegistry;
use crate::lir::LirType;

// ── Type abstraction ────────────────────────────────────────────────────────

/// Abstract runtime-API type. Const-buildable; resolves to a concrete `LirType`
/// at call sites via [`CRuntimeType::to_lir_type`] using the per-module
/// `StructRegistry` (which holds the runtime-allocated `StructId` for each
/// well-known struct).
///
/// Kept separate from `LirType` because:
/// 1. The static signature table must be const, but `LirType::Struct(StructId)`
///    requires runtime-allocated IDs.
/// 2. The runtime API surface is narrower than the full LirType — only a
///    handful of named structs flow across it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CRuntimeType {
    Void,
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    /// `size_t` — passed as `i64` in Gorget. Kept distinct from `I64` for clarity.
    Size,
    /// Opaque `void*`.
    Ptr,
    /// Pointer to a single element of unspecified type. Same wire format as
    /// `Ptr`, but flags the collection-element ABI. The backend-side ABI
    /// marshalling reads `AbiKind::VoidElem`; this type tag makes the intent
    /// visible at the LIR level too.
    VoidElem,
    /// `const char*` (null-terminated). Same wire format as `Ptr`.
    CStr,
    /// Gorget `Str` struct (32 bytes — fat string handle: `{data, cap, len, alloc}`).
    Str,
    /// `GorgetArray` struct (64 bytes).
    Array,
    /// `GorgetMap` struct.
    Map,
    /// `GorgetSet` struct.
    Set,
    /// `GorgetRegex` struct (16 bytes — pcre2 handle + pattern pointer).
    Regex,
    /// `GorgetRegexMatch` struct (56 bytes — capture vector + offsets).
    Match,
}

impl CRuntimeType {
    /// Resolve to a concrete `LirType` using the module's struct registry.
    /// Mirrors the existing fallback behavior in `runtime_extern_sig()`:
    /// if a named struct hasn't been registered yet (e.g. the module doesn't
    /// touch regex), the type degrades to `LirType::Ptr`.
    pub fn to_lir_type(self, sr: &StructRegistry) -> LirType {
        let lookup = |name: &str| {
            sr.lookup(name).map(LirType::Struct).unwrap_or(LirType::Ptr)
        };
        match self {
            CRuntimeType::Void => LirType::Void,
            CRuntimeType::Bool => LirType::Bool,
            CRuntimeType::I8 => LirType::I8,
            CRuntimeType::I16 => LirType::I16,
            CRuntimeType::I32 => LirType::I32,
            CRuntimeType::I64 | CRuntimeType::Size => LirType::I64,
            CRuntimeType::U8 => LirType::U8,
            CRuntimeType::U16 => LirType::U16,
            CRuntimeType::U32 => LirType::U32,
            CRuntimeType::U64 => LirType::U64,
            CRuntimeType::F32 => LirType::F32,
            CRuntimeType::F64 => LirType::F64,
            CRuntimeType::Ptr | CRuntimeType::VoidElem | CRuntimeType::CStr => LirType::Ptr,
            CRuntimeType::Str => lookup("GorgetString"),
            CRuntimeType::Array => lookup("GorgetArray"),
            CRuntimeType::Map => lookup("GorgetMap"),
            CRuntimeType::Set => lookup("GorgetSet"),
            CRuntimeType::Regex => lookup("Regex"),
            CRuntimeType::Match => sr
                .lookup("Match")
                .or_else(|| sr.lookup("RegexMatch"))
                .map(LirType::Struct)
                .unwrap_or(LirType::Ptr),
        }
    }

    /// If this type is a named struct, return the canonical struct name.
    pub fn struct_name(self) -> Option<&'static str> {
        match self {
            CRuntimeType::Str => Some("GorgetString"),
            CRuntimeType::Array => Some("GorgetArray"),
            CRuntimeType::Map => Some("GorgetMap"),
            CRuntimeType::Set => Some("GorgetSet"),
            CRuntimeType::Regex => Some("Regex"),
            CRuntimeType::Match => Some("Match"),
            _ => None,
        }
    }
}

// ── Side-effects classification ─────────────────────────────────────────────

/// Coarse-grained side-effects classification. Used by the optimizer to
/// decide which runtime calls are eligible for CSE / DCE / reordering.
///
/// Today the optimizer reads a hand-curated whitelist; once every variant
/// has an accurate tag, the whitelist deletes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SideEffects {
    /// No reads, no writes — pure mathematical function. Eligible for CSE,
    /// DCE if dst unused, and constant folding when args are constants.
    Pure,
    /// Reads from heap or global state but doesn't mutate. Eligible for CSE
    /// across non-mutating calls, but cannot be hoisted past a `Mutates` call.
    ReadOnly,
    /// May allocate (extends the heap / arena).
    Allocates,
    /// Mutates one or more arguments.
    Mutates,
    /// Performs I/O (file, stdin/stdout, sockets).
    Io,
    /// May abort the process or unwind. Treated as a control-flow boundary.
    Aborts,
    /// Touches concurrency primitives (locks, channels, atomics, threads).
    Concurrent,
    /// Catch-all for variants whose effects haven't been classified yet.
    /// Optimizer treats `Unknown` as `Mutates` — the safe over-approximation.
    Unknown,
}

impl SideEffects {
    /// True if this call may mutate observable state (heap, args, globals,
    /// I/O, panics). The optimizer uses this to gate CSE / hoisting.
    pub fn may_mutate(self) -> bool {
        !matches!(self, SideEffects::Pure | SideEffects::ReadOnly)
    }
}

// ── Signatures ──────────────────────────────────────────────────────────────

/// Const-buildable signature for a runtime function.
#[derive(Debug)]
pub struct RuntimeSig {
    pub params: &'static [(CRuntimeType, AbiKind)],
    pub ret: CRuntimeType,
    pub side_effects: SideEffects,
    /// True iff the runtime fn ALWAYS returns a fresh, independently
    /// heap-allocated buffer that does not alias any input. Distinct from
    /// `SideEffects::Allocates` (which is a coarse may-allocate tag covering
    /// view-returners that build cap=0 views without owning their data).
    /// Read by IR lowering's CoW machinery to decide whether the result of a
    /// call can skip the self-referential reassignment clone guard and the
    /// return-clone-elision check (see `is_fresh_string` in
    /// `src/ir/lowering/context.rs`). Replaces the deprecated
    /// `is_fresh_allocating_extern` name list.
    pub returns_fresh: bool,
}

/// Runtime-resolved signature in `LirType` terms.
#[derive(Debug, Clone)]
pub struct ResolvedSig {
    pub params: Vec<LirType>,
    pub ret: LirType,
    pub param_abis: Vec<AbiKind>,
}

const fn sig(
    params: &'static [(CRuntimeType, AbiKind)],
    ret: CRuntimeType,
    fx: SideEffects,
) -> RuntimeSig {
    RuntimeSig { params, ret, side_effects: fx, returns_fresh: false }
}

/// Like [`sig`] but tags `returns_fresh: true` — for runtime fns that always
/// return an independently heap-allocated buffer (no aliasing into inputs).
const fn sig_fresh(
    params: &'static [(CRuntimeType, AbiKind)],
    ret: CRuntimeType,
    fx: SideEffects,
) -> RuntimeSig {
    RuntimeSig { params, ret, side_effects: fx, returns_fresh: true }
}

struct Entry {
    name: &'static str,
    sig: RuntimeSig,
}

// ── Variant + registry definition (lockstep) ────────────────────────────────

/// `runtime_table!` declares the `RuntimeFn` enum and the parallel
/// `REGISTRY` slice in one macro invocation. Each `$variant => $name, $sig;`
/// line emits both an enum variant and a registry entry, guaranteeing the
/// `as usize` ordinal matches the registry index. Adding a function means
/// appending one line.
macro_rules! runtime_table {
    ( $( $variant:ident => $name:literal, $sig:expr; )* ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[repr(u16)]
        #[rustfmt::skip]
        pub enum RuntimeFn {
            $( $variant, )*
        }

        const REGISTRY: &[Entry] = &[
            $( Entry { name: $name, sig: $sig }, )*
        ];
    };
}

// Local aliases to keep the table readable.
use AbiKind as A;
use CRuntimeType as T;
use SideEffects as F;

#[rustfmt::skip]
runtime_table! {
    // ── __gorget_* allocator hooks ────────────────────────────────────────
    LoAlloc         => "__gorget_alloc",          sig(&[(T::Ptr, A::Opaque)], T::Ptr, F::Allocates);
    LoAllocArray    => "__gorget_alloc_array",    sig(&[(T::I64, A::Scalar), (T::Ptr, A::Opaque)], T::Ptr, F::Allocates);
    LoDealloc       => "__gorget_dealloc",        sig(&[(T::Ptr, A::Opaque), (T::Ptr, A::Opaque)], T::Void, F::Mutates);
    LoPopAllocator  => "__gorget_pop_allocator",  sig(&[], T::Void, F::Mutates);
    LoPushAllocator => "__gorget_push_allocator", sig(&[(T::Ptr, A::Opaque)], T::Void, F::Mutates);

    // ── libc primitives ───────────────────────────────────────────────────
    Free   => "free",   sig(&[(T::Ptr, A::Opaque)], T::Void, F::Mutates);
    Malloc => "malloc", sig(&[(T::I64, A::Scalar)], T::Ptr, F::Allocates);
    Memcmp => "memcmp", sig(&[(T::Ptr, A::Opaque), (T::Ptr, A::Opaque), (T::I64, A::Scalar)], T::I32, F::ReadOnly);
    Memset => "memset", sig(&[(T::Ptr, A::Opaque), (T::I32, A::Scalar), (T::I64, A::Scalar)], T::Ptr, F::Mutates);

    // ── Math ──────────────────────────────────────────────────────────────
    Abs    => "gorget_abs",    sig(&[(T::I64, A::Scalar)], T::I64, F::Pure);
    Acos   => "gorget_acos",   sig(&[(T::F64, A::Scalar)], T::F64, F::Pure);
    Asin   => "gorget_asin",   sig(&[(T::F64, A::Scalar)], T::F64, F::Pure);
    Atan   => "gorget_atan",   sig(&[(T::F64, A::Scalar)], T::F64, F::Pure);
    Atan2  => "gorget_atan2",  sig(&[(T::F64, A::Scalar), (T::F64, A::Scalar)], T::F64, F::Pure);
    Ceil   => "gorget_ceil",   sig(&[(T::F64, A::Scalar)], T::F64, F::Pure);
    Cos    => "gorget_cos",    sig(&[(T::F64, A::Scalar)], T::F64, F::Pure);
    Fabs   => "gorget_fabs",   sig(&[(T::F64, A::Scalar)], T::F64, F::Pure);
    Floor  => "gorget_floor",  sig(&[(T::F64, A::Scalar)], T::F64, F::Pure);
    Fmax   => "gorget_fmax",   sig(&[(T::F64, A::Scalar), (T::F64, A::Scalar)], T::F64, F::Pure);
    Fmin   => "gorget_fmin",   sig(&[(T::F64, A::Scalar), (T::F64, A::Scalar)], T::F64, F::Pure);
    Log    => "gorget_log",    sig(&[(T::F64, A::Scalar)], T::F64, F::Pure);
    Log10  => "gorget_log10",  sig(&[(T::F64, A::Scalar)], T::F64, F::Pure);
    Log2   => "gorget_log2",   sig(&[(T::F64, A::Scalar)], T::F64, F::Pure);
    Min    => "gorget_min",    sig(&[(T::I64, A::Scalar), (T::I64, A::Scalar)], T::I64, F::Pure);
    Max    => "gorget_max",    sig(&[(T::I64, A::Scalar), (T::I64, A::Scalar)], T::I64, F::Pure);
    Pow    => "gorget_pow",    sig(&[(T::F64, A::Scalar), (T::F64, A::Scalar)], T::F64, F::Pure);
    Round  => "gorget_round",  sig(&[(T::F64, A::Scalar)], T::F64, F::Pure);
    Sin    => "gorget_sin",    sig(&[(T::F64, A::Scalar)], T::F64, F::Pure);
    Sqrt   => "gorget_sqrt",   sig(&[(T::F64, A::Scalar)], T::F64, F::Pure);
    Tan    => "gorget_tan",    sig(&[(T::F64, A::Scalar)], T::F64, F::Pure);

    // ── Conversion ────────────────────────────────────────────────────────
    BoolToStr        => "gorget_bool_to_str",        sig_fresh(&[(T::Bool, A::Scalar)], T::Str, F::Allocates);
    CharChr          => "gorget_char_chr",           sig(&[(T::I64, A::Scalar)], T::Str, F::Allocates);
    CodepointToUtf8  => "gorget_codepoint_to_utf8",  sig(&[(T::I64, A::Scalar)], T::Str, F::Allocates);
    FloatToStr       => "gorget_float_to_str",       sig_fresh(&[(T::F64, A::Scalar)], T::Str, F::Allocates);
    IntToFloat       => "gorget_int_to_float",       sig(&[(T::I64, A::Scalar)], T::F64, F::Pure);
    IntToStr         => "gorget_int_to_str",         sig_fresh(&[(T::I64, A::Scalar)], T::Str, F::Allocates);

    // ── String — by-value (Str, 32 bytes) ─────────────────────────────────
    StrByteAt           => "gorget_str_byte_at",           sig(&[(T::Str, A::GorgetString), (T::I64, A::Scalar)], T::U8,  F::Pure);
    StrByteLen          => "gorget_str_byte_len",          sig(&[(T::Str, A::GorgetString)], T::I64, F::Pure);
    StrByteSlice        => "gorget_str_byte_slice",        sig(&[(T::Str, A::GorgetString), (T::I64, A::Scalar), (T::I64, A::Scalar)], T::Str, F::Allocates);
    StrBytes            => "gorget_str_bytes",             sig(&[(T::Str, A::GorgetString)], T::Array, F::Allocates);
    StrCapacity         => "gorget_str_capacity",          sig(&[(T::Ptr, A::Ptr)], T::I64, F::ReadOnly);
    StrCat              => "gorget_str_cat",               sig_fresh(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::Str, F::Allocates);
    StrCharAt           => "gorget_str_char_at",           sig(&[(T::Str, A::GorgetString), (T::I64, A::Scalar)], T::Str, F::Allocates);
    StrChars            => "gorget_str_chars",             sig(&[(T::Str, A::GorgetString)], T::Array, F::Allocates);
    StrClear            => "gorget_str_clear",             sig(&[(T::Ptr, A::Ptr)], T::Void, F::Mutates);
    StrCmp              => "gorget_str_cmp",               sig(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::I64, F::Pure);
    StrCodepointAt      => "gorget_str_codepoint_at",      sig(&[(T::Str, A::GorgetString), (T::I64, A::Scalar)], T::Str, F::Allocates);
    StrCodepointCount   => "gorget_str_codepoint_count",   sig(&[(T::Str, A::GorgetString)], T::I64, F::Pure);
    StrCodepoints       => "gorget_str_codepoints",        sig(&[(T::Str, A::GorgetString)], T::Array, F::Allocates);
    StrContains         => "gorget_str_contains",          sig(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::Bool, F::Pure);
    StrCount            => "gorget_str_count",             sig(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::I64, F::Pure);
    StrEmpty            => "gorget_str_empty",             sig(&[], T::Str, F::Pure);
    StrEndsWith         => "gorget_str_ends_with",         sig(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::Bool, F::Pure);
    StrEq               => "gorget_str_eq",                sig(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::Bool, F::Pure);
    StrFind             => "gorget_str_find",              sig(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::I64, F::Pure);
    StrFindExt          => "gorget_str_find_ext",          sig(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString), (T::I64, A::Scalar), (T::Bool, A::Scalar)], T::I64, F::Pure);
    StrFindFrom         => "gorget_str_find_from",         sig(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString), (T::I64, A::Scalar)], T::I64, F::Pure);
    StrFromBool         => "gorget_str_from_bool",         sig(&[(T::I64, A::Scalar)], T::Str, F::Allocates);
    StrFromCstr         => "gorget_str_from_cstr",         sig(&[(T::Ptr, A::Opaque)], T::Str, F::Allocates);
    StrFromFloat        => "gorget_str_from_float",        sig(&[(T::I64, A::Scalar)], T::Str, F::Allocates);
    StrFromInt          => "gorget_str_from_int",          sig(&[(T::I64, A::Scalar)], T::Str, F::Allocates);
    StrFromLiteral      => "gorget_str_from_literal",      sig(&[(T::Ptr, A::Opaque), (T::I64, A::Scalar)], T::Str, F::Allocates);
    StrHasNull          => "gorget_str_has_null",          sig(&[(T::Str, A::GorgetString)], T::Bool, F::Pure);
    StrIndex            => "gorget_str_index",             sig(&[(T::Str, A::GorgetString), (T::I64, A::Scalar)], T::Str, F::Allocates);
    StrIndexOf          => "gorget_str_index_of",          sig(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::I64, F::Pure);
    StrIsAlpha          => "gorget_str_is_alpha",          sig(&[(T::Str, A::GorgetString)], T::Bool, F::Pure);
    StrIsAlphanumeric   => "gorget_str_is_alphanumeric",   sig(&[(T::Str, A::GorgetString)], T::Bool, F::Pure);
    StrIsAscii          => "gorget_str_is_ascii",          sig(&[(T::Str, A::GorgetString)], T::Bool, F::Pure);
    StrIsDigit          => "gorget_str_is_digit",          sig(&[(T::Str, A::GorgetString)], T::Bool, F::Pure);
    StrIsEmpty          => "gorget_str_is_empty",          sig(&[(T::Str, A::GorgetString)], T::Bool, F::Pure);
    StrIsHexDigit       => "gorget_str_is_hex_digit",      sig(&[(T::Str, A::GorgetString)], T::Bool, F::Pure);
    StrIsLower          => "gorget_str_is_lower",          sig(&[(T::Str, A::GorgetString)], T::Bool, F::Pure);
    StrIsUpper          => "gorget_str_is_upper",          sig(&[(T::Str, A::GorgetString)], T::Bool, F::Pure);
    StrIsWhitespace     => "gorget_str_is_whitespace",     sig(&[(T::Str, A::GorgetString)], T::Bool, F::Pure);
    StrJoin             => "gorget_str_join",              sig_fresh(&[(T::Str, A::GorgetString), (T::Array, A::ByValue)], T::Str, F::Allocates);
    StrLines            => "gorget_str_lines",             sig(&[(T::Str, A::GorgetString)], T::Array, F::Allocates);
    StrLstrip           => "gorget_str_lstrip",            sig(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::Str, F::Allocates);
    StrLstripWs         => "gorget_str_lstrip_ws",         sig(&[(T::Str, A::GorgetString)], T::Str, F::Allocates);
    StrOrd              => "gorget_str_ord",               sig(&[(T::Str, A::GorgetString)], T::I64, F::Pure);
    StrPadLeft          => "gorget_str_pad_left",          sig_fresh(&[(T::Str, A::GorgetString), (T::I64, A::Scalar), (T::Str, A::GorgetString)], T::Str, F::Allocates);
    StrPadRight         => "gorget_str_pad_right",         sig_fresh(&[(T::Str, A::GorgetString), (T::I64, A::Scalar), (T::Str, A::GorgetString)], T::Str, F::Allocates);
    StrPush             => "gorget_str_push",              sig(&[(T::Ptr, A::Ptr), (T::Str, A::GorgetString)], T::Void, F::Mutates);
    StrPushChar         => "gorget_str_push_char",         sig(&[(T::Ptr, A::Ptr), (T::Str, A::GorgetString)], T::Void, F::Mutates);
    StrPushLine         => "gorget_str_push_line",         sig(&[(T::Ptr, A::Ptr), (T::Str, A::GorgetString)], T::Void, F::Mutates);
    StrRemoveprefix     => "gorget_str_removeprefix",      sig(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::Str, F::Allocates);
    StrRemovesuffix     => "gorget_str_removesuffix",      sig(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::Str, F::Allocates);
    StrRepeat           => "gorget_str_repeat",            sig_fresh(&[(T::Str, A::GorgetString), (T::I64, A::Scalar)], T::Str, F::Allocates);
    StrReplace          => "gorget_str_replace",           sig_fresh(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::Str, F::Allocates);
    StrReplacen         => "gorget_str_replacen",          sig_fresh(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString), (T::Str, A::GorgetString), (T::I64, A::Scalar)], T::Str, F::Allocates);
    StrRstrip           => "gorget_str_rstrip",            sig(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::Str, F::Allocates);
    StrRstripWs         => "gorget_str_rstrip_ws",         sig(&[(T::Str, A::GorgetString)], T::Str, F::Allocates);
    StrSlice            => "gorget_str_slice",             sig(&[(T::Str, A::GorgetString), (T::I64, A::Scalar), (T::I64, A::Scalar)], T::Str, F::Allocates);
    StrSplit            => "gorget_str_split",             sig(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::Array, F::Allocates);
    StrSplitn           => "gorget_str_splitn",            sig(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString), (T::I64, A::Scalar)], T::Array, F::Allocates);
    StrStartsWith       => "gorget_str_starts_with",       sig(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::Bool, F::Pure);
    StrStr              => "gorget_str_str",               sig(&[(T::Ptr, A::Ptr)], T::Str, F::Pure);
    StrStrip            => "gorget_str_strip",             sig(&[(T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::Str, F::Allocates);
    StrToCstr           => "gorget_str_to_cstr",           sig(&[(T::Str, A::GorgetString)], T::Ptr, F::Allocates);
    StrToLower          => "gorget_str_to_lower",          sig_fresh(&[(T::Str, A::GorgetString)], T::Str, F::Allocates);
    StrToUpper          => "gorget_str_to_upper",          sig_fresh(&[(T::Str, A::GorgetString)], T::Str, F::Allocates);
    StrTrim             => "gorget_str_trim",              sig(&[(T::Str, A::GorgetString)], T::Str, F::Allocates);
    Utf8CodepointLenAt  => "gorget_utf8_codepoint_len_at", sig(&[(T::Str, A::GorgetString), (T::I64, A::Scalar)], T::I64, F::Pure);

    // ── String (gorget_string_*) — owned-string via pointer ───────────────
    StringAppend        => "gorget_string_append",         sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::Ptr)], T::Void, F::Mutates);
    StringClone         => "gorget_string_clone",          sig(&[(T::Ptr, A::Ptr)], T::Str, F::Allocates);
    StringCloneToOwned  => "gorget_string_clone_to_owned", sig(&[(T::Ptr, A::Ptr)], T::Str, F::Allocates);
    StringConcat        => "gorget_string_concat",         sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::Ptr)], T::Str, F::Allocates);
    StringCstr          => "gorget_string_cstr",           sig(&[(T::Ptr, A::Ptr)], T::Ptr, F::ReadOnly);
    StringDebug         => "gorget_string_debug",          sig(&[(T::Str, A::GorgetString)], T::Str, F::Pure);
    StringEq            => "gorget_string_eq",             sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::Ptr)], T::Bool, F::ReadOnly);
    StringFree          => "gorget_string_free",           sig(&[(T::Ptr, A::Ptr)], T::Void, F::Mutates);
    StringFromStr       => "gorget_string_from_str",       sig(&[(T::Str, A::GorgetString)], T::Str, F::Allocates);
    StringNew           => "gorget_string_new",            sig(&[(T::Ptr, A::Opaque)], T::Str, F::Allocates);
    StringPushLine      => "gorget_string_push_line",      sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::CStr)], T::Void, F::Mutates);

    // ── Array ─────────────────────────────────────────────────────────────
    ArrayBinarySearch  => "gorget_array_binary_search", sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::VoidElem)], T::I64, F::ReadOnly);
    ArrayCapacity      => "gorget_array_capacity",      sig(&[(T::Ptr, A::Ptr)], T::I64, F::ReadOnly);
    ArrayClear         => "gorget_array_clear",         sig(&[(T::Ptr, A::Ptr)], T::Void, F::Mutates);
    // gorget_array_clone: deep clone of every element into a fresh GorgetArray.
    // Tagged `sig_fresh` for the consume-site validator (Phase 2E).
    ArrayClone         => "gorget_array_clone",         sig_fresh(&[(T::Ptr, A::Ptr)], T::Array, F::Allocates);
    ArrayContains      => "gorget_array_contains",      sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::VoidElem), (T::I64, A::Scalar)], T::Bool, F::ReadOnly);
    ArrayDedup         => "gorget_array_dedup",         sig(&[(T::Ptr, A::Ptr)], T::Void, F::Mutates);
    ArrayExtend        => "gorget_array_extend",        sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::Ptr)], T::Void, F::Mutates);
    ArrayFill          => "gorget_array_fill",          sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar), (T::Ptr, A::VoidElem)], T::Void, F::Mutates);
    ArrayFirst         => "gorget_array_first",         sig(&[(T::Ptr, A::Ptr)], T::Ptr, F::ReadOnly);
    ArrayFree          => "gorget_array_free",          sig(&[(T::Ptr, A::Ptr)], T::Void, F::Mutates);
    ArrayGet           => "gorget_array_get",           sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar)], T::Ptr, F::ReadOnly);
    ArrayIndexOf       => "gorget_array_index_of",      sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::VoidElem)], T::I64, F::ReadOnly);
    ArrayInsert        => "gorget_array_insert",        sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar), (T::Ptr, A::VoidElem)], T::Void, F::Mutates);
    ArrayIsEmpty       => "gorget_array_is_empty",      sig(&[(T::Ptr, A::Ptr)], T::Bool, F::ReadOnly);
    ArrayLast          => "gorget_array_last",          sig(&[(T::Ptr, A::Ptr)], T::Ptr, F::ReadOnly);
    ArrayLen           => "gorget_array_len",           sig(&[(T::Ptr, A::Ptr)], T::I64, F::ReadOnly);
    // gorget_array_new / gorget_array_with_capacity: brand-new empty
    // GorgetArray on the heap. Phase 2E: `sig_fresh` so the consume-site
    // validator routes results identically with the rest of the
    // collection-allocator family.
    ArrayNew           => "gorget_array_new",           sig_fresh(&[(T::I64, A::Scalar)], T::Array, F::Allocates);
    ArrayPop           => "gorget_array_pop",           sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar)], T::Ptr, F::Mutates);
    ArrayPush          => "gorget_array_push",          sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::VoidElem)], T::Void, F::Mutates);
    ArrayRemove        => "gorget_array_remove",        sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar)], T::Void, F::Mutates);
    ArrayRemoveOpt     => "gorget_array_remove_opt",    sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar)], T::Ptr, F::Mutates);
    ArrayReserve       => "gorget_array_reserve",       sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar)], T::Void, F::Mutates);
    ArrayReverse       => "gorget_array_reverse",       sig(&[(T::Ptr, A::Ptr)], T::Void, F::Mutates);
    ArraySafeGet       => "gorget_array_safe_get",      sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar)], T::Ptr, F::ReadOnly);
    ArraySafePop       => "gorget_array_safe_pop",      sig(&[(T::Ptr, A::Ptr)], T::Ptr, F::Mutates);
    ArraySet           => "gorget_array_set",           sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar), (T::Ptr, A::VoidElem)], T::Void, F::Mutates);
    ArraySlice         => "gorget_array_slice",         sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar), (T::I64, A::Scalar)], T::Array, F::Allocates);
    ArraySwap          => "gorget_array_swap",          sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar), (T::I64, A::Scalar)], T::Void, F::Mutates);
    ArraySwapRemove    => "gorget_array_swap_remove",   sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar)], T::Void, F::Mutates);
    ArrayWithCapacity  => "gorget_array_with_capacity", sig_fresh(&[(T::I64, A::Scalar), (T::I64, A::Scalar)], T::Array, F::Allocates);

    // ── Map / Dict ────────────────────────────────────────────────────────
    // gorget_{dict,map}_new[_str|_like] / gorget_map_clone: fresh-allocator
    // / deep-clone family. Phase 2E typed signal for the consume-site
    // validator.
    DictNew         => "gorget_dict_new",         sig_fresh(&[(T::I64, A::Scalar), (T::I64, A::Scalar)], T::Map, F::Allocates);
    DictNewStr      => "gorget_dict_new_str",     sig_fresh(&[(T::I64, A::Scalar)], T::Map, F::Allocates);
    MapClear        => "gorget_map_clear",        sig(&[(T::Ptr, A::Ptr)], T::Void, F::Mutates);
    MapClone        => "gorget_map_clone",        sig_fresh(&[(T::Ptr, A::Ptr)], T::Map, F::Allocates);
    MapContains     => "gorget_map_contains",     sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::VoidElem)], T::Bool, F::ReadOnly);
    MapFree         => "gorget_map_free",         sig(&[(T::Ptr, A::Ptr)], T::Void, F::Mutates);
    MapGet          => "gorget_map_get",          sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::VoidElem)], T::Ptr, F::ReadOnly);
    MapIsEmpty      => "gorget_map_is_empty",     sig(&[(T::Ptr, A::Ptr)], T::Bool, F::ReadOnly);
    MapItems        => "gorget_map_items",        sig(&[(T::Ptr, A::Ptr)], T::Array, F::Allocates);
    MapKeys         => "gorget_map_keys",         sig(&[(T::Ptr, A::Ptr)], T::Array, F::Allocates);
    MapLen          => "gorget_map_len",          sig(&[(T::Ptr, A::Ptr)], T::I64, F::ReadOnly);
    MapNew          => "gorget_map_new",          sig_fresh(&[(T::I64, A::Scalar), (T::I64, A::Scalar)], T::Map, F::Allocates);
    MapNewLike      => "gorget_map_new_like",     sig_fresh(&[(T::Ptr, A::Ptr)], T::Map, F::Allocates);
    MapNewStr       => "gorget_map_new_str",      sig_fresh(&[(T::I64, A::Scalar)], T::Map, F::Allocates);
    MapPut          => "gorget_map_put",          sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::VoidElem), (T::Ptr, A::VoidElem)], T::Void, F::Mutates);
    MapPutCloned    => "gorget_map_put_cloned",   sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::VoidElem), (T::Ptr, A::VoidElem)], T::Void, F::Mutates);
    MapRemove       => "gorget_map_remove",       sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::VoidElem)], T::Bool, F::Mutates);
    MapRemoveOpt    => "gorget_map_remove_opt",   sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::VoidElem)], T::Ptr, F::Mutates);
    MapReserve      => "gorget_map_reserve",      sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar)], T::Void, F::Mutates);
    MapValues       => "gorget_map_values",       sig(&[(T::Ptr, A::Ptr)], T::Array, F::Allocates);

    // ── Set ───────────────────────────────────────────────────────────────
    // gorget_{set,ordered_set}_{new,new_str,new_like} / gorget_set_clone:
    // fresh-allocator / deep-clone family for set collections. Phase 2E
    // typed signal for the consume-site validator.
    OrderedSetNew    => "gorget_ordered_set_new",     sig_fresh(&[(T::I64, A::Scalar)], T::Set, F::Allocates);
    OrderedSetNewStr => "gorget_ordered_set_new_str", sig_fresh(&[], T::Set, F::Allocates);
    SetAdd           => "gorget_set_add",             sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::VoidElem)], T::Void, F::Mutates);
    SetClear         => "gorget_set_clear",           sig(&[(T::Ptr, A::Ptr)], T::Void, F::Mutates);
    SetClone         => "gorget_set_clone",           sig_fresh(&[(T::Ptr, A::Ptr)], T::Set, F::Allocates);
    SetContains      => "gorget_set_contains",        sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::VoidElem)], T::Bool, F::ReadOnly);
    SetFree          => "gorget_set_free",            sig(&[(T::Ptr, A::Ptr)], T::Void, F::Mutates);
    SetIsEmpty       => "gorget_set_is_empty",        sig(&[(T::Ptr, A::Ptr)], T::Bool, F::ReadOnly);
    SetLen           => "gorget_set_len",             sig(&[(T::Ptr, A::Ptr)], T::I64, F::ReadOnly);
    SetNew           => "gorget_set_new",             sig_fresh(&[(T::I64, A::Scalar)], T::Set, F::Allocates);
    SetNewLike       => "gorget_set_new_like",        sig_fresh(&[(T::Ptr, A::Ptr)], T::Set, F::Allocates);
    SetNewStr        => "gorget_set_new_str",         sig_fresh(&[], T::Set, F::Allocates);
    SetRemove        => "gorget_set_remove",          sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::VoidElem)], T::Bool, F::Mutates);
    SetReserve       => "gorget_set_reserve",         sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar)], T::Void, F::Mutates);
    SetToArray       => "gorget_set_to_array",        sig(&[(T::Ptr, A::Ptr)], T::Array, F::Allocates);

    // ── Heap ──────────────────────────────────────────────────────────────
    HeapFree => "gorget_heap_free", sig(&[(T::Ptr, A::Ptr)], T::Void, F::Mutates);
    HeapLen  => "gorget_heap_len",  sig(&[(T::Ptr, A::Ptr)], T::I64, F::ReadOnly);
    HeapNew  => "gorget_heap_new",  sig(&[(T::I64, A::Scalar)], T::Ptr, F::Allocates);
    HeapPeek => "gorget_heap_peek", sig(&[(T::Ptr, A::Ptr)], T::Ptr, F::ReadOnly);
    HeapPop  => "gorget_heap_pop",  sig(&[(T::Ptr, A::Ptr)], T::Ptr, F::Mutates);
    HeapPush => "gorget_heap_push", sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::VoidElem)], T::Void, F::Mutates);

    // ── Mutex / Guard ─────────────────────────────────────────────────────
    GuardGet     => "gorget_guard_get",      sig(&[(T::Ptr, A::Opaque)], T::Ptr, F::Concurrent);
    GuardGetPtr  => "gorget_guard_get_ptr",  sig(&[(T::Ptr, A::Opaque)], T::Ptr, F::Concurrent);
    GuardRelease => "gorget_guard_release",  sig(&[(T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    GuardSet     => "gorget_guard_set",      sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::VoidElem), (T::I64, A::Scalar)], T::Void, F::Mutates);
    MutexFree    => "gorget_mutex_free",     sig(&[(T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    MutexLock    => "gorget_mutex_lock",     sig(&[(T::Ptr, A::Opaque)], T::Ptr, F::Concurrent);
    MutexLockTo  => "gorget_mutex_lock_to",  sig(&[(T::Ptr, A::Opaque), (T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    MutexNew     => "gorget_mutex_new",      sig(&[(T::I64, A::Scalar), (T::Ptr, A::VoidElem)], T::Ptr, F::Concurrent);

    // ── Shared / Weak ─────────────────────────────────────────────────────
    SharedClone       => "gorget_shared_clone",        sig(&[(T::Ptr, A::Opaque)], T::Ptr, F::Concurrent);
    SharedDowngrade   => "gorget_shared_downgrade",    sig(&[(T::Ptr, A::Opaque)], T::Ptr, F::Concurrent);
    SharedDrop        => "gorget_shared_drop",         sig(&[(T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    SharedGet         => "gorget_shared_get",          sig(&[(T::Ptr, A::Opaque)], T::Ptr, F::Concurrent);
    SharedGetPtr      => "gorget_shared_get_ptr",      sig(&[(T::Ptr, A::Opaque)], T::Ptr, F::Concurrent);
    SharedNew         => "gorget_shared_new",          sig(&[(T::I64, A::Scalar), (T::Ptr, A::VoidElem)], T::Ptr, F::Allocates);
    SharedStrongCount => "gorget_shared_strong_count", sig(&[(T::Ptr, A::Opaque)], T::I64, F::Concurrent);
    WeakClone         => "gorget_weak_clone",          sig(&[(T::Ptr, A::Opaque)], T::Ptr, F::Concurrent);
    WeakDrop          => "gorget_weak_drop",           sig(&[(T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    WeakUpgrade       => "gorget_weak_upgrade",        sig(&[(T::Ptr, A::Opaque)], T::I64, F::Concurrent);

    // ── Channel ───────────────────────────────────────────────────────────
    ChannelCapacity => "gorget_channel_capacity",  sig(&[(T::Ptr, A::Opaque)], T::I64, F::Concurrent);
    ChannelClose    => "gorget_channel_close",     sig(&[(T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    ChannelFree     => "gorget_channel_free",      sig(&[(T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    ChannelIsClosed => "gorget_channel_is_closed", sig(&[(T::Ptr, A::Opaque)], T::Bool, F::Concurrent);
    ChannelLen      => "gorget_channel_len",       sig(&[(T::Ptr, A::Opaque)], T::I64, F::Concurrent);
    ChannelNew      => "gorget_channel_new",       sig(&[(T::I64, A::Scalar), (T::I64, A::Scalar)], T::Ptr, F::Concurrent);
    ChannelRecv     => "gorget_channel_recv",      sig(&[(T::Ptr, A::Opaque), (T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    ChannelRelease  => "gorget_channel_release",   sig(&[(T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    ChannelRetain   => "gorget_channel_retain",    sig(&[(T::Ptr, A::Opaque)], T::Ptr, F::Concurrent);
    ChannelSend     => "gorget_channel_send",      sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::VoidElem)], T::Void, F::Concurrent);

    // ── RWLock / ReadGuard / WriteGuard ───────────────────────────────────
    ReadGuardGet      => "gorget_read_guard_get",      sig(&[(T::Ptr, A::Opaque)], T::Ptr, F::Concurrent);
    ReadGuardGetPtr   => "gorget_read_guard_get_ptr",  sig(&[(T::Ptr, A::Opaque)], T::Ptr, F::Concurrent);
    ReadGuardRelease  => "gorget_read_guard_release",  sig(&[(T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    RwlockFree        => "gorget_rwlock_free",         sig(&[(T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    RwlockNew         => "gorget_rwlock_new",          sig(&[(T::I64, A::Scalar), (T::Ptr, A::VoidElem)], T::Ptr, F::Concurrent);
    RwlockRead        => "gorget_rwlock_read",         sig(&[(T::Ptr, A::Opaque)], T::Ptr, F::Concurrent);
    RwlockReadTo      => "gorget_rwlock_read_to",      sig(&[(T::Ptr, A::Opaque), (T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    RwlockWrite       => "gorget_rwlock_write",        sig(&[(T::Ptr, A::Opaque)], T::Ptr, F::Concurrent);
    RwlockWriteTo     => "gorget_rwlock_write_to",     sig(&[(T::Ptr, A::Opaque), (T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    WriteGuardGet     => "gorget_write_guard_get",     sig(&[(T::Ptr, A::Opaque)], T::Ptr, F::Concurrent);
    WriteGuardGetPtr  => "gorget_write_guard_get_ptr", sig(&[(T::Ptr, A::Opaque)], T::Ptr, F::Concurrent);
    WriteGuardRelease => "gorget_write_guard_release", sig(&[(T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    WriteGuardSet     => "gorget_write_guard_set",     sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::VoidElem), (T::I64, A::Scalar)], T::Void, F::Mutates);

    // ── Random ────────────────────────────────────────────────────────────
    Rand      => "gorget_rand",       sig(&[], T::I64, F::Mutates);
    RandRange => "gorget_rand_range", sig(&[(T::I64, A::Scalar), (T::I64, A::Scalar)], T::I64, F::Mutates);
    Seed      => "gorget_seed",       sig(&[(T::I64, A::Scalar)], T::Void, F::Mutates);

    // ── Time ──────────────────────────────────────────────────────────────
    FormatTime     => "gorget_format_time",      sig(&[(T::I64, A::Scalar), (T::Ptr, A::CStr)], T::Ptr, F::Allocates);
    ParseTime      => "gorget_parse_time",       sig(&[(T::Ptr, A::CStr), (T::Ptr, A::CStr)], T::I64, F::Pure);
    ReactorSleepMs => "gorget_reactor_sleep_ms", sig(&[(T::I64, A::Scalar)], T::Void, F::Io);
    SleepMs        => "gorget_sleep_ms",         sig(&[(T::I64, A::Scalar)], T::Void, F::Io);
    Time           => "gorget_time",             sig(&[], T::I64, F::Io);
    TimeMs         => "gorget_time_ms",          sig(&[], T::I64, F::Io);

    // ── Barrier / CondVar ─────────────────────────────────────────────────
    BarrierFree      => "gorget_barrier_free",       sig(&[(T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    BarrierNew       => "gorget_barrier_new",        sig(&[(T::I64, A::Scalar)], T::Ptr, F::Concurrent);
    BarrierWait      => "gorget_barrier_wait",       sig(&[(T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    CondvarFree      => "gorget_condvar_free",       sig(&[(T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    CondvarNew       => "gorget_condvar_new",        sig(&[], T::Ptr, F::Concurrent);
    CondvarNotifyAll => "gorget_condvar_notify_all", sig(&[(T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    CondvarNotifyOne => "gorget_condvar_notify_one", sig(&[(T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    CondvarWaitGuard => "gorget_condvar_wait_guard", sig(&[(T::Ptr, A::Opaque), (T::Ptr, A::Opaque)], T::Void, F::Concurrent);

    // ── Atomic ────────────────────────────────────────────────────────────
    AtomicBoolCompareExchange => "gorget_atomic_bool_compare_exchange", sig(&[(T::Ptr, A::Opaque), (T::Bool, A::Scalar), (T::Bool, A::Scalar)], T::Bool, F::Concurrent);
    AtomicBoolFree            => "gorget_atomic_bool_free",             sig(&[(T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    AtomicBoolLoad            => "gorget_atomic_bool_load",             sig(&[(T::Ptr, A::Opaque)], T::Bool, F::Concurrent);
    AtomicBoolNew             => "gorget_atomic_bool_new",              sig(&[(T::Bool, A::Scalar)], T::Ptr, F::Concurrent);
    AtomicBoolStore           => "gorget_atomic_bool_store",            sig(&[(T::Ptr, A::Opaque), (T::Bool, A::Scalar)], T::Void, F::Concurrent);
    AtomicBoolSwap            => "gorget_atomic_bool_swap",             sig(&[(T::Ptr, A::Opaque), (T::Bool, A::Scalar)], T::Bool, F::Concurrent);
    AtomicIntAdd              => "gorget_atomic_int_add",               sig(&[(T::Ptr, A::Opaque), (T::I64, A::Scalar)], T::I64, F::Concurrent);
    AtomicIntCompareExchange  => "gorget_atomic_int_compare_exchange",  sig(&[(T::Ptr, A::Opaque), (T::I64, A::Scalar), (T::I64, A::Scalar)], T::Bool, F::Concurrent);
    AtomicIntFree             => "gorget_atomic_int_free",              sig(&[(T::Ptr, A::Opaque)], T::Void, F::Concurrent);
    AtomicIntLoad             => "gorget_atomic_int_load",              sig(&[(T::Ptr, A::Opaque)], T::I64, F::Concurrent);
    AtomicIntNew              => "gorget_atomic_int_new",               sig(&[(T::I64, A::Scalar)], T::Ptr, F::Concurrent);
    AtomicIntStore            => "gorget_atomic_int_store",             sig(&[(T::Ptr, A::Opaque), (T::I64, A::Scalar)], T::Void, F::Concurrent);
    AtomicIntSub              => "gorget_atomic_int_sub",               sig(&[(T::Ptr, A::Opaque), (T::I64, A::Scalar)], T::I64, F::Concurrent);

    // ── Process ───────────────────────────────────────────────────────────
    ProcessCloseStdin => "gorget_process_close_stdin", sig(&[(T::Ptr, A::Opaque)], T::Void, F::Io);
    ProcessKill       => "gorget_process_kill",        sig(&[(T::Ptr, A::Opaque)], T::Void, F::Io);
    ProcessPid        => "gorget_process_pid",         sig(&[(T::Ptr, A::Opaque)], T::I64, F::Io);
    ProcessReadStderr => "gorget_process_read_stderr", sig(&[(T::Ptr, A::Opaque)], T::Str, F::Io);
    ProcessReadStdout => "gorget_process_read_stdout", sig(&[(T::Ptr, A::Opaque)], T::Str, F::Io);
    ProcessSpawn      => "gorget_process_spawn",       sig(&[(T::Ptr, A::CStr), (T::Ptr, A::Ptr)], T::Ptr, F::Io);
    ProcessWait       => "gorget_process_wait",        sig(&[(T::Ptr, A::Opaque)], T::I64, F::Io);
    ProcessWriteStdin => "gorget_process_write_stdin", sig(&[(T::Ptr, A::Opaque), (T::Str, A::CStr)], T::Void, F::Io);

    // ── Panic / abort ─────────────────────────────────────────────────────
    AssertFail       => "gorget_assert_fail",        sig(&[(T::Ptr, A::CStr), (T::Ptr, A::CStr), (T::I64, A::Scalar)], T::Void, F::Aborts);
    AssertFailValues => "gorget_assert_fail_values", sig(&[(T::Ptr, A::CStr), (T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::Void, F::Aborts);
    OverflowAdd      => "gorget_overflow_add",       sig(&[], T::Void, F::Aborts);
    OverflowMul      => "gorget_overflow_mul",       sig(&[], T::Void, F::Aborts);
    OverflowSub      => "gorget_overflow_sub",       sig(&[], T::Void, F::Aborts);
    Panic            => "gorget_panic",              sig(&[(T::Ptr, A::CStr)], T::Void, F::Aborts);

    // ── Parse int/float ───────────────────────────────────────────────────
    ParseFloat => "gorget_parse_float", sig(&[(T::Ptr, A::CStr)], T::F64, F::Pure);
    ParseInt   => "gorget_parse_int",   sig(&[(T::Ptr, A::CStr)], T::I64, F::Pure);

    // ── Regex ─────────────────────────────────────────────────────────────
    RegexCompile   => "gorget_regex_compile",   sig(&[(T::Ptr, A::CStr), (T::Ptr, A::CStr)], T::Regex, F::Allocates);
    RegexFind      => "gorget_regex_find",      sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::CStr), (T::I64, A::Scalar)], T::Match, F::Allocates);
    RegexFindAll   => "gorget_regex_find_all",  sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::CStr)], T::Array, F::Allocates);
    RegexFindAt    => "gorget_regex_find_at",   sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::CStr), (T::I64, A::Scalar)], T::Match, F::Allocates);
    RegexFullmatch => "gorget_regex_fullmatch", sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::CStr)], T::Match, F::Allocates);
    RegexIsMatch   => "gorget_regex_is_match",  sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::CStr)], T::Bool, F::ReadOnly);
    RegexReplace   => "gorget_regex_replace",   sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::CStr), (T::Ptr, A::CStr)], T::Str, F::Allocates);
    RegexSplit     => "gorget_regex_split",     sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::CStr), (T::I64, A::Scalar)], T::Array, F::Allocates);

    // ── Bytes ─────────────────────────────────────────────────────────────
    BytesConcat     => "gorget_bytes_concat",      sig(&[(T::Ptr, A::Ptr), (T::Ptr, A::Ptr)], T::Array, F::Allocates);
    BytesFromHex    => "gorget_bytes_from_hex",    sig(&[(T::Ptr, A::CStr)], T::Array, F::Allocates);
    BytesFromStr    => "gorget_bytes_from_str",    sig(&[(T::Ptr, A::CStr)], T::Array, F::Allocates);
    BytesReadF32Le  => "gorget_bytes_read_f32_le", sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar)], T::F64, F::ReadOnly);
    BytesReadF64Le  => "gorget_bytes_read_f64_le", sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar)], T::F64, F::ReadOnly);
    BytesReadI32Le  => "gorget_bytes_read_i32_le", sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar)], T::I64, F::ReadOnly);
    BytesReadI64Le  => "gorget_bytes_read_i64_le", sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar)], T::I64, F::ReadOnly);
    BytesReadU16Be  => "gorget_bytes_read_u16_be", sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar)], T::I64, F::ReadOnly);
    BytesReadU16Le  => "gorget_bytes_read_u16_le", sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar)], T::I64, F::ReadOnly);
    BytesReadU32Be  => "gorget_bytes_read_u32_be", sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar)], T::I64, F::ReadOnly);
    BytesReadU32Le  => "gorget_bytes_read_u32_le", sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar)], T::I64, F::ReadOnly);
    BytesSlice      => "gorget_bytes_slice",       sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar), (T::I64, A::Scalar)], T::Array, F::Allocates);
    BytesToHex      => "gorget_bytes_to_hex",      sig(&[(T::Ptr, A::Ptr)], T::Str, F::Allocates);
    BytesToStr      => "gorget_bytes_to_str",      sig(&[(T::Ptr, A::Ptr)], T::Str, F::Allocates);
    BytesUtf8Valid  => "gorget_bytes_utf8_valid",  sig(&[(T::Ptr, A::Ptr)], T::Bool, F::Pure);
    BytesWriteF32Le => "gorget_bytes_write_f32_le",sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar), (T::F64, A::Scalar)], T::Void, F::Mutates);
    BytesWriteF64Le => "gorget_bytes_write_f64_le",sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar), (T::F64, A::Scalar)], T::Void, F::Mutates);
    BytesWriteI32Le => "gorget_bytes_write_i32_le",sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar), (T::I64, A::Scalar)], T::Void, F::Mutates);
    BytesWriteI64Le => "gorget_bytes_write_i64_le",sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar), (T::I64, A::Scalar)], T::Void, F::Mutates);
    BytesWriteU16Be => "gorget_bytes_write_u16_be",sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar), (T::I64, A::Scalar)], T::Void, F::Mutates);
    BytesWriteU16Le => "gorget_bytes_write_u16_le",sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar), (T::I64, A::Scalar)], T::Void, F::Mutates);
    BytesWriteU32Be => "gorget_bytes_write_u32_be",sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar), (T::I64, A::Scalar)], T::Void, F::Mutates);
    BytesWriteU32Le => "gorget_bytes_write_u32_le",sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar), (T::I64, A::Scalar)], T::Void, F::Mutates);

    // ── File I/O ──────────────────────────────────────────────────────────
    AppendFile  => "gorget_append_file",  sig(&[(T::Ptr, A::Auto), (T::Ptr, A::Auto)], T::Void, F::Io);
    FileExists  => "gorget_file_exists",  sig(&[(T::Ptr, A::Auto)], T::Bool, F::Io);
    IsDir       => "gorget_is_dir",       sig(&[(T::Ptr, A::Auto)], T::Bool, F::Io);
    ReadFile    => "gorget_read_file",    sig(&[(T::Ptr, A::Auto)], T::Str, F::Io);
    WriteFile   => "gorget_write_file",   sig(&[(T::Ptr, A::Auto), (T::Ptr, A::Auto)], T::Void, F::Io);
}

// ── Methods on RuntimeFn ────────────────────────────────────────────────────

impl RuntimeFn {
    /// The C symbol name for this runtime function.
    #[inline]
    pub fn c_name(self) -> &'static str {
        REGISTRY[self as usize].name
    }

    /// The signature in const-buildable abstract types.
    #[inline]
    pub fn signature(self) -> &'static RuntimeSig {
        &REGISTRY[self as usize].sig
    }

    /// Resolve the abstract signature to concrete `LirType` form using the
    /// module's struct registry. Returns parameter types + ABI tags + return
    /// type, ready to be threaded into a `LirExtern` declaration.
    pub fn resolve_lir_sig(self, sr: &StructRegistry) -> ResolvedSig {
        let sig = self.signature();
        let mut params = Vec::with_capacity(sig.params.len());
        let mut param_abis = Vec::with_capacity(sig.params.len());
        for (ty, abi) in sig.params {
            params.push(ty.to_lir_type(sr));
            param_abis.push(*abi);
        }
        ResolvedSig {
            params,
            ret: sig.ret.to_lir_type(sr),
            param_abis,
        }
    }

    /// Look up a runtime function by its C symbol name.
    ///
    /// Backed by a once-initialised `FxHashMap<&str, RuntimeFn>` over
    /// [`REGISTRY`]; subsequent calls are O(1) hash lookups.
    pub fn from_c_name(name: &str) -> Option<Self> {
        static MAP: OnceLock<FxHashMap<&'static str, RuntimeFn>> = OnceLock::new();
        let map = MAP.get_or_init(|| {
            // SAFETY: `as u16` of an enum repr(u16) variant is the variant's
            // ordinal, which is precisely its REGISTRY index. We construct
            // RuntimeFn values from those ordinals — never any other source.
            REGISTRY
                .iter()
                .enumerate()
                .map(|(i, e)| {
                    // RuntimeFn is repr(u16); `i < REGISTRY.len()` so the
                    // ordinal is always a valid variant.
                    let v: RuntimeFn = unsafe { std::mem::transmute(i as u16) };
                    (e.name, v)
                })
                .collect()
        });
        map.get(name).copied()
    }

    /// Total number of registered runtime functions.
    pub const fn count() -> usize {
        REGISTRY.len()
    }

    /// True if this runtime function may mutate observable state.
    /// Convenience wrapper over the `SideEffects` tag.
    pub fn may_mutate(self) -> bool {
        self.signature().side_effects.may_mutate()
    }
}

// ── Module pass: promote CallExtern → CallRuntime ───────────────────────────

/// Walk every `Inst::CallExtern` in `module` and convert to `Inst::CallRuntime`
/// whenever the called name matches a known [`RuntimeFn`] variant.
///
/// Run after [`crate::lir::types::wire_collection_bridges`] (which still scans
/// `CallExtern { name, original_name }` shape) and before BIR lowering (which
/// rewrites `CallRuntime` back into `CallExtern` for backend uniformity).
/// The window between is where the validator + optimizer + future
/// CollectionCtor lowering see the typed form.
///
/// Idempotent: a second call is a no-op (already-converted `CallRuntime`s
/// don't match the `if let CallExtern` arm).
pub fn promote_runtime_calls(module: &mut crate::lir::LirModule) {
    use crate::lir::Inst;
    for func in &mut module.functions {
        for block in &mut func.blocks {
            for inst in &mut block.insts {
                if let Inst::CallExtern { dst, name, args, arg_abis, .. } = inst {
                    if let Some(callee) = RuntimeFn::from_c_name(name) {
                        // Strip-family arity overload: `s.strip()` (1 arg)
                        // means whitespace-trim; `s.strip(chars)` (2 args)
                        // is the char-set version. Same for lstrip/rstrip.
                        // Backends today rewrite the C name based on arg
                        // count; we instead pick the right typed variant
                        // here so the callee's signature matches the call.
                        let callee = match (callee, args.len()) {
                            (RuntimeFn::StrStrip, 1) => RuntimeFn::StrTrim,
                            (RuntimeFn::StrLstrip, 1) => RuntimeFn::StrLstripWs,
                            (RuntimeFn::StrRstrip, 1) => RuntimeFn::StrRstripWs,
                            _ => callee,
                        };
                        // The CallExtern's arg_abis comes from the LirExtern
                        // declaration's param_abis — which can be longer than
                        // the actual call's args (e.g. strip's extern decl
                        // has 2 abi tags but a no-chars call passes 1 arg).
                        // Truncate to match the call shape; the CallRuntime
                        // is canonical from here on.
                        let moved_args = std::mem::take(args);
                        let mut moved_abis = std::mem::take(arg_abis);
                        moved_abis.truncate(moved_args.len());
                        *inst = Inst::CallRuntime {
                            dst: *dst,
                            callee,
                            args: moved_args,
                            arg_abis: moved_abis,
                        };
                    }
                }
            }
        }
    }
}

// ── Module pass: promote collection-ctor CallExterns → Inst::CollectionCtor ─

/// Walk every collection-constructor `Inst::CallExtern` in `module` and
/// promote it to `Inst::CollectionCtor` with structured metadata parsed
/// from the call's `original_name` field.
///
/// The qualifying calls are the ones that today carry a runtime name
/// (`gorget_array_new`, `gorget_dict_new`, `gorget_set_new`, …) AND a
/// monomorphized GIR name as `original_name` (`Vector__T__new`,
/// `Dict__K__V__new`, `Set__T__new`, …). The promote pass parses the
/// monomorphized name into a `(CollectionCtorKind, ElemMeta, val?)`
/// triple and replaces the `CallExtern` with the typed `CollectionCtor`
/// inst — backed by the existing `original_name` so the wire-up passes
/// (`wire_collection_bridges`, `find_hashable_key_types`) can read either
/// shape during the migration window. After A3's full landing the
/// `original_name` field deletes (audit's #4).
///
/// Run AFTER LIR construction (so `original_name` is still on the call)
/// and BEFORE `wire_collection_bridges` (which today reads CallExtern,
/// will read CollectionCtor after migration).
///
/// Idempotent: a second call is a no-op (already-converted CollectionCtor
/// insts don't match the `if let CallExtern` arm).
pub fn promote_collection_ctors(module: &mut crate::lir::LirModule) {
    use crate::lir::{Block, CollectionCtorKind, ElemMeta, Inst, ResourceKind, StructId};

    fn classify_runtime_name(name: &str) -> Option<RuntimeCtorShape> {
        // Returns (is_array_ctor, is_map_ctor, is_set_ctor, with_capacity, str_keyed).
        let with_capacity = name.ends_with("_with_capacity");
        let str_keyed = name.ends_with("_new_str");
        let shape = if name == "gorget_array_new" || name == "gorget_array_with_capacity" {
            RuntimeCtorShape::Array { with_capacity }
        } else if name == "gorget_dict_new" || name == "gorget_dict_new_str"
            || name == "gorget_map_new" || name == "gorget_map_new_str"
        {
            RuntimeCtorShape::Map { with_capacity, str_keyed }
        } else if name == "gorget_set_new" || name == "gorget_set_new_str"
            || name == "gorget_ordered_set_new" || name == "gorget_ordered_set_new_str"
        {
            RuntimeCtorShape::Set { with_capacity, str_keyed }
        } else {
            return None;
        };
        Some(shape)
    }

    enum RuntimeCtorShape {
        Array { with_capacity: bool },
        Map { with_capacity: bool, str_keyed: bool },
        Set { with_capacity: bool, str_keyed: bool },
    }

    /// Parse the monomorphized GIR name (`Dict__K__V__new`, `Vector__T__new`,
    /// `Set__T__new`, …) into a `(kind, elem_or_key_name, val_name?)` triple.
    /// Falls back to `None` if the prefix doesn't match a known shape.
    fn strip_ctor_suffix(s: &str) -> &str {
        s.strip_suffix("__new_str")
            .or_else(|| s.strip_suffix("__new"))
            .or_else(|| s.strip_suffix("__with_capacity"))
            .unwrap_or(s)
    }

    fn parse_original(orig: &str) -> Option<(CollectionCtorKind, String, Option<String>)> {
        let strip_suffix = strip_ctor_suffix;
        if let Some(rest) = orig.strip_prefix("Vector__") {
            return Some((CollectionCtorKind::Vector, strip_suffix(rest).to_string(), None));
        }
        if let Some(rest) = orig.strip_prefix("Deque__") {
            return Some((CollectionCtorKind::Deque, strip_suffix(rest).to_string(), None));
        }
        if let Some(rest) = orig.strip_prefix("Dict__") {
            let stripped = strip_suffix(rest);
            // Find the `__` that splits K and V. We split at the FIRST `__`
            // we see — but type names themselves can carry `__` (e.g.
            // `Vector__int64_t`). Strategy: scan for top-level `__` that
            // separates two type names. Heuristic: the K side ends at the
            // first `__` whose right neighbor starts a type-token (capital,
            // primitive, or known prefix). For now, use the legacy
            // `splitn(2, "__")` shape — matches what wire_collection_bridges
            // does today; correct for non-nested key types.
            if let Some(pos) = stripped.find("__") {
                let key = &stripped[..pos];
                let val = &stripped[pos + 2..];
                return Some((
                    CollectionCtorKind::Dict,
                    key.to_string(),
                    Some(val.to_string()),
                ));
            }
        }
        if let Some(rest) = orig.strip_prefix("HashMap__") {
            let stripped = strip_suffix(rest);
            if let Some(pos) = stripped.find("__") {
                let key = &stripped[..pos];
                let val = &stripped[pos + 2..];
                return Some((
                    CollectionCtorKind::HashMap,
                    key.to_string(),
                    Some(val.to_string()),
                ));
            }
        }
        if let Some(rest) = orig.strip_prefix("Set__") {
            return Some((CollectionCtorKind::Set, strip_suffix(rest).to_string(), None));
        }
        if let Some(rest) = orig.strip_prefix("HashSet__") {
            return Some((CollectionCtorKind::HashSet, strip_suffix(rest).to_string(), None));
        }
        None
    }

    /// Map a parsed type name to `ElemMeta`. Built-in resource types and
    /// primitives hit explicit branches; unknown names fall through to a
    /// `module.structs` lookup. If the name still isn't found (e.g. the
    /// type was DCE'd), falls back to `Primitive(LirType::Ptr)` — a
    /// best-effort tag that doesn't trip downstream wiring.
    fn elem_meta_from_name(name: &str, structs: &[crate::lir::StructDef]) -> ElemMeta {
        match name {
            "int64_t" | "uint64_t" => ElemMeta::Primitive(crate::lir::LirType::I64),
            "int32_t" | "uint32_t" => ElemMeta::Primitive(crate::lir::LirType::I32),
            "int16_t" | "uint16_t" => ElemMeta::Primitive(crate::lir::LirType::I16),
            "int8_t" => ElemMeta::Primitive(crate::lir::LirType::I8),
            "uint8_t" => ElemMeta::Primitive(crate::lir::LirType::U8),
            "double" => ElemMeta::Primitive(crate::lir::LirType::F64),
            "float" => ElemMeta::Primitive(crate::lir::LirType::F32),
            "bool" | "_Bool" => ElemMeta::Primitive(crate::lir::LirType::Bool),
            "GorgetString" | "Str" => ElemMeta::Resource(ResourceKind::GorgetString),
            "GorgetArray" => ElemMeta::Resource(ResourceKind::GorgetArray),
            "GorgetMap" => ElemMeta::Resource(ResourceKind::GorgetMap),
            "GorgetSet" => ElemMeta::Resource(ResourceKind::GorgetSet),
            "GorgetClosure" => ElemMeta::Resource(ResourceKind::GorgetClosure),
            n if n.starts_with("Vector__") || n.starts_with("Deque__") => {
                ElemMeta::Resource(ResourceKind::GorgetArray)
            }
            n if n.starts_with("Dict__") || n.starts_with("HashMap__") => {
                ElemMeta::Resource(ResourceKind::GorgetMap)
            }
            n if n.starts_with("Set__") || n.starts_with("HashSet__") => {
                ElemMeta::Resource(ResourceKind::GorgetSet)
            }
            n => {
                // Phase A residual #1: Callable / MutCallable / ConsumeCallable
                // tagged via the LIR StructDef's `c_runtime_alias`. Read the
                // struct table so name-prefix matching isn't needed.
                if let Some((_, sd)) = structs.iter().enumerate().find(|(_, s)| s.name == n) {
                    if sd.c_runtime_alias.as_deref() == Some("GorgetClosure") {
                        return ElemMeta::Resource(ResourceKind::GorgetClosure);
                    }
                }
                structs
                    .iter()
                    .position(|s| s.name == n)
                    .map(|i| ElemMeta::UserType(StructId(i as u32)))
                    .unwrap_or(ElemMeta::Primitive(crate::lir::LirType::Ptr))
            }
        }
    }

    let structs = module.structs.clone(); // borrow split: walk funcs mutably + read structs.
    for func in &mut module.functions {
        for block in &mut func.blocks {
            promote_block(block, &structs, &classify_runtime_name, &parse_original, &elem_meta_from_name);
        }
    }

    fn promote_block(
        block: &mut Block,
        structs: &[crate::lir::StructDef],
        classify: &dyn Fn(&str) -> Option<RuntimeCtorShape>,
        parse_original: &dyn Fn(&str) -> Option<(CollectionCtorKind, String, Option<String>)>,
        elem_meta_from_name: &dyn Fn(&str, &[crate::lir::StructDef]) -> ElemMeta,
    ) {
        for inst in &mut block.insts {
            let (dst, name, original_name, args, arg_abis) = match inst {
                Inst::CallExtern { dst, name, original_name: Some(orig), args, arg_abis, .. } => {
                    (*dst, name.clone(), orig.clone(), std::mem::take(args), std::mem::take(arg_abis))
                }
                _ => continue,
            };

            let shape = match classify(&name) {
                Some(s) => s,
                None => {
                    // Restore — this CallExtern wasn't a collection ctor.
                    *inst = Inst::CallExtern {
                        dst, name, args, arg_abis,
                        original_name: Some(original_name),
                    };
                    continue;
                }
            };
            let parsed = match parse_original(&original_name) {
                Some(p) => p,
                None => {
                    *inst = Inst::CallExtern {
                        dst, name, args, arg_abis,
                        original_name: Some(original_name),
                    };
                    continue;
                }
            };
            let (kind, elem_or_key_name, val_name) = parsed;

            let kind_matches_shape = matches!(
                (&shape, kind),
                (RuntimeCtorShape::Array { .. }, CollectionCtorKind::Vector | CollectionCtorKind::Deque)
                | (RuntimeCtorShape::Map { .. },
                    CollectionCtorKind::Dict | CollectionCtorKind::HashMap)
                | (RuntimeCtorShape::Set { .. },
                    CollectionCtorKind::Set | CollectionCtorKind::HashSet)
            );
            if !kind_matches_shape {
                *inst = Inst::CallExtern {
                    dst, name, args, arg_abis,
                    original_name: Some(original_name),
                };
                continue;
            }

            let (with_capacity, str_keyed) = match shape {
                RuntimeCtorShape::Array { with_capacity } => (with_capacity, false),
                RuntimeCtorShape::Map { with_capacity, str_keyed } => (with_capacity, str_keyed),
                RuntimeCtorShape::Set { with_capacity, str_keyed } => (with_capacity, str_keyed),
            };

            let elem_or_key = elem_meta_from_name(&elem_or_key_name, structs);
            let val = val_name.as_deref().map(|n| elem_meta_from_name(n, structs));

            let Some(d) = dst else {
                // Constructors always produce a value; restore on failure.
                *inst = Inst::CallExtern {
                    dst, name, args, arg_abis,
                    original_name: Some(original_name),
                };
                continue;
            };

            *inst = Inst::CollectionCtor {
                dst: d,
                kind,
                elem_or_key,
                val,
                args,
                arg_abis,
                with_capacity,
                str_keyed,
            };
        }
    }
}

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// Round-trip: c_name → from_c_name → original variant.
    /// Indirectly verifies enum/registry sync — if the macro lockstep is
    /// broken (impossible without macro edits), the round-trip fails.
    #[test]
    fn name_round_trip() {
        for entry in REGISTRY {
            let parsed = RuntimeFn::from_c_name(entry.name)
                .expect("registered name should parse");
            assert_eq!(parsed.c_name(), entry.name, "round-trip mismatch");
        }
    }

    /// Names are unique across the registry.
    #[test]
    fn names_unique() {
        let mut seen = std::collections::HashSet::new();
        for entry in REGISTRY {
            assert!(seen.insert(entry.name), "duplicate c_name: {}", entry.name);
        }
    }

    #[test]
    fn from_c_name_unknown() {
        assert!(RuntimeFn::from_c_name("not_a_runtime_fn").is_none());
        assert!(RuntimeFn::from_c_name("").is_none());
        assert!(RuntimeFn::from_c_name("SDL_CreateWindow").is_none());
    }

    /// Spot-check signatures against the shapes in `runtime_extern_sig()`.
    #[test]
    fn signatures_spot_check() {
        // gorget_array_push: (Ptr, Ptr) -> Void, mutates.
        let s = RuntimeFn::ArrayPush.signature();
        assert_eq!(s.params.len(), 2);
        assert_eq!(s.params[0], (CRuntimeType::Ptr, AbiKind::Ptr));
        assert_eq!(s.params[1], (CRuntimeType::Ptr, AbiKind::VoidElem));
        assert_eq!(s.ret, CRuntimeType::Void);
        assert!(s.side_effects.may_mutate());

        // gorget_str_eq: (Str, Str) -> Bool, pure.
        let s = RuntimeFn::StrEq.signature();
        assert_eq!(s.params.len(), 2);
        assert_eq!(s.params[0], (CRuntimeType::Str, AbiKind::GorgetString));
        assert_eq!(s.ret, CRuntimeType::Bool);
        assert!(!s.side_effects.may_mutate());

        // malloc: (I64) -> Ptr, allocates.
        let s = RuntimeFn::Malloc.signature();
        assert_eq!(s.ret, CRuntimeType::Ptr);
        assert!(matches!(s.side_effects, SideEffects::Allocates));
    }

    /// Sanity-check `resolve_lir_sig` against an empty StructRegistry — every
    /// named-struct type should fall back to `LirType::Ptr`, matching the
    /// existing `runtime_extern_sig()` contract.
    #[test]
    fn resolve_falls_back_to_ptr() {
        let sr = StructRegistry::new();
        let r = RuntimeFn::ArrayNew.resolve_lir_sig(&sr);
        assert_eq!(r.ret, LirType::Ptr); // Array → Ptr fallback.
        assert_eq!(r.params, vec![LirType::I64]);
        assert_eq!(r.param_abis, vec![AbiKind::Scalar]);
    }

    /// Cross-table consistency: every `RuntimeFn` in `REGISTRY` whose c_name
    /// is known to `runtime_extern_sig` (the LirExtern declarations used by
    /// the GIR→LIR lowerer) must agree on arity, ABI tags, and return type.
    ///
    /// This is the regression test for Tier E §8.3's "9 latent runtime arity
    /// bugs" — `gorget_array_slice` declared with 1 param at runtime.rs:378
    /// but emitted with 3 args at lower/insts.rs:888, etc. Both tables are
    /// authoritative for different consumers (validator vs. lowerer); when
    /// they drift, calls go through one shape and validate against the
    /// other, surfacing only at integration-test time. This test catches the
    /// drift at unit-test time.
    ///
    /// Functions that are NOT registered in `runtime_extern_sig` (the lowerer
    /// uses ABI heuristics for them) are skipped — that's a valid pattern;
    /// only DECLARED entries must match.
    #[test]
    fn runtime_sig_matches_extern_sig() {
        use crate::lir::lower::calls::runtime_extern_sig;

        // Use the canonical builtin struct registry so resolve_lir_sig and
        // runtime_extern_sig both produce concrete struct types (not the
        // Ptr fallback).
        let mut sr = StructRegistry::new();
        for (i, def) in crate::lir::types::builtin_struct_defs().iter().enumerate() {
            sr.register(&def.name, crate::lir::StructId(i as u32));
        }

        let mut mismatches: Vec<String> = Vec::new();
        for entry in REGISTRY {
            let runtime_resolved = (entry.sig.params.iter()
                .map(|(t, _)| t.to_lir_type(&sr))
                .collect::<Vec<_>>(),
                entry.sig.params.iter().map(|(_, a)| *a).collect::<Vec<_>>(),
                entry.sig.ret.to_lir_type(&sr));

            let Some(extern_sig) = runtime_extern_sig(entry.name, &sr) else {
                // Not declared in the calls.rs table — lowerer uses heuristics.
                // Acceptable; only declared entries are required to match.
                continue;
            };

            let runtime_arity = runtime_resolved.0.len();
            let extern_arity = extern_sig.params.len();
            if runtime_arity != extern_arity {
                mismatches.push(format!(
                    "{}: runtime.rs has {} param(s), calls.rs has {}",
                    entry.name, runtime_arity, extern_arity,
                ));
                continue;
            }
            if extern_sig.param_abis.len() != extern_arity {
                mismatches.push(format!(
                    "{}: calls.rs param_abis len ({}) != params len ({})",
                    entry.name, extern_sig.param_abis.len(), extern_arity,
                ));
                continue;
            }
            for (i, (rt_abi, ex_abi)) in runtime_resolved.1.iter()
                .zip(extern_sig.param_abis.iter()).enumerate()
            {
                if rt_abi != ex_abi {
                    mismatches.push(format!(
                        "{}: arg[{}] ABI runtime.rs={:?} vs calls.rs={:?}",
                        entry.name, i, rt_abi, ex_abi,
                    ));
                }
            }
            if runtime_resolved.2 != extern_sig.ret {
                mismatches.push(format!(
                    "{}: ret runtime.rs={:?} vs calls.rs={:?}",
                    entry.name, runtime_resolved.2, extern_sig.ret,
                ));
            }
        }
        assert!(mismatches.is_empty(),
            "runtime.rs ↔ calls.rs cross-table mismatches:\n  {}",
            mismatches.join("\n  "));
    }

    /// Every `RuntimeFn` that returns a struct type (Array/Map/Set/String/etc.)
    /// must be recognized by `lir::types::infer_call_extern_type`'s
    /// struct-returning name list — otherwise the dst type falls through to
    /// the `LirType::I64` default and downstream backends emit scalar stores
    /// against aggregate slots (the LLVM symptom that bit `gorget_str_trim`
    /// after the strip-family routing change).
    #[test]
    fn struct_returns_are_recognized_by_infer() {
        let mut sr = StructRegistry::new();
        let builtins = crate::lir::types::builtin_struct_defs();
        for (i, def) in builtins.iter().enumerate() {
            sr.register(&def.name, crate::lir::StructId(i as u32));
        }
        // Build a minimal LirModule whose `structs` list lets
        // infer_call_extern_type's struct lookups succeed.
        let mut module = crate::lir::LirModule::default();
        module.structs = builtins;

        let mut mismatches: Vec<String> = Vec::new();
        for entry in REGISTRY {
            let ret = entry.sig.ret.to_lir_type(&sr);
            // Only struct-returning runtime fns need to be in the rt_struct
            // list. Scalars and Ptr falls-through paths are fine via the
            // module.externs lookup or the I64 default.
            if !ret.is_aggregate() {
                continue;
            }
            // The infer function takes args + val_types; pass empty since
            // none of the name-based branches read those.
            let inferred = crate::lir::types::infer_call_extern_type_for_test(
                entry.name, &[], &module, &[],
            );
            match inferred {
                Some(LirType::Struct(_)) => { /* OK */ }
                other => mismatches.push(format!(
                    "{} (returns {:?}): infer_call_extern_type returned {:?}",
                    entry.name, ret, other,
                )),
            }
        }
        assert!(mismatches.is_empty(),
            "RuntimeFn struct-return ↔ infer_call_extern_type mismatches:\n  {}",
            mismatches.join("\n  "));
    }
}
