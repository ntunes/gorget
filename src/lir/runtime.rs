//! Typed runtime function table.
//!
//! Single source of truth for the Gorget runtime API at the IR level.
//! Enum whose variants are the only legal way to name a runtime function
//! in `Inst::CallRuntime` (added in A1 — see
//! `docs/devbook/18-runtime-abi.md`). Retired the legacy
//! string-keyed `runtime_extern_sig()` parallel mirror 2026-05-19; the
//! `RuntimeFn::from_c_name + resolve_lir_sig` path is the only canonical
//! lookup now.
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
}

impl CRuntimeType {
    /// Resolve to a concrete `LirType` using the module's struct registry.
    /// If a named struct hasn't been registered yet (e.g. the module doesn't
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
        }
    }

    /// If this type is a named struct, return the canonical struct name.
    pub fn struct_name(self) -> Option<&'static str> {
        match self {
            CRuntimeType::Str => Some("GorgetString"),
            CRuntimeType::Array => Some("GorgetArray"),
            CRuntimeType::Map => Some("GorgetMap"),
            CRuntimeType::Set => Some("GorgetSet"),
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
    FloatToBits      => "gorget_float_to_bits",      sig(&[(T::F64, A::Scalar)], T::I64, F::Pure);
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
    // gorget_string_borrow_view: shallow copy with cap forced to 0 — the lazy
    // loop-carried CoW bind (emit_lazy_loopcarried_borrow). F::Allocates is
    // the uniform coarse tag for view-returners that build cap=0 views (see
    // the registry doc above); the view-vs-fresh axis is `returns_fresh`
    // (false via `sig`).
    StringBorrowView    => "gorget_string_borrow_view",    sig(&[(T::Ptr, A::Ptr)], T::Str, F::Allocates);
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
    // Dict/Set for-loop iteration accessors. Arg 2 is the `void* out` slot the
    // accessor writes the (owned, resource-cloned) key/value INTO — tagged
    // `OutPtr` so drop-elaboration marks the destination slot Initialized and
    // keeps its `drop_if_alive` (else the cloned key/value leaks — #11).
    MapIterKey      => "gorget_map_iter_key",     sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar), (T::Ptr, A::OutPtr)], T::Void, F::Mutates);
    MapIterValue    => "gorget_map_iter_value",   sig(&[(T::Ptr, A::Ptr), (T::I64, A::Scalar), (T::Ptr, A::OutPtr)], T::Void, F::Mutates);
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
    // Arg 1 is OutPtr: the callee writes the Guard value into the destination
    // slot. Without OutPtr, drop_elab treats the slot as Uninitialized and
    // deletes DropIfAlive — expression-statement guard temps (and any path
    // that drops the ReadTo destination itself rather than a post-Move copy)
    // would never unlock (Round XIX Track Y; same #11 OutPtr pattern as MapIterKey).
    MutexLockTo  => "gorget_mutex_lock_to",  sig(&[(T::Ptr, A::Opaque), (T::Ptr, A::OutPtr)], T::Void, F::Concurrent);
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
    // Arg 1 OutPtr — see MutexLockTo; without it statement-end DropIfAlive on
    // the ReadGuard/WriteGuard temp is deleted by drop_elab (Track Y hang).
    RwlockReadTo      => "gorget_rwlock_read_to",      sig(&[(T::Ptr, A::Opaque), (T::Ptr, A::OutPtr)], T::Void, F::Concurrent);
    RwlockWrite       => "gorget_rwlock_write",        sig(&[(T::Ptr, A::Opaque)], T::Ptr, F::Concurrent);
    RwlockWriteTo     => "gorget_rwlock_write_to",     sig(&[(T::Ptr, A::Opaque), (T::Ptr, A::OutPtr)], T::Void, F::Concurrent);
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
    // 4-arg (D11): the leading CStr is the `T_AssertFailed` code (typed data
    // from src/trap.rs, approach (a)); then op, left, right as before.
    AssertFailValues => "gorget_assert_fail_values", sig(&[(T::Ptr, A::CStr), (T::Ptr, A::CStr), (T::Str, A::GorgetString), (T::Str, A::GorgetString)], T::Void, F::Aborts);
    OverflowAdd      => "gorget_overflow_add",       sig(&[], T::Void, F::Aborts);
    OverflowMul      => "gorget_overflow_mul",       sig(&[], T::Void, F::Aborts);
    OverflowSub      => "gorget_overflow_sub",       sig(&[], T::Void, F::Aborts);
    Panic            => "gorget_panic",              sig(&[(T::Ptr, A::CStr)], T::Void, F::Aborts);

    // ── Parse int/float ───────────────────────────────────────────────────
    ParseFloat => "gorget_parse_float", sig(&[(T::Ptr, A::CStr)], T::F64, F::Pure);
    ParseInt   => "gorget_parse_int",   sig(&[(T::Ptr, A::CStr)], T::I64, F::Pure);

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

    /// Spot-check canonical signature shapes.
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
    /// named-struct type should fall back to `LirType::Ptr`.
    #[test]
    fn resolve_falls_back_to_ptr() {
        let sr = StructRegistry::new();
        let r = RuntimeFn::ArrayNew.resolve_lir_sig(&sr);
        assert_eq!(r.ret, LirType::Ptr); // Array → Ptr fallback.
        assert_eq!(r.params, vec![LirType::I64]);
        assert_eq!(r.param_abis, vec![AbiKind::Scalar]);
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
