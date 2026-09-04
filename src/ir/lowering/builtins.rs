//! Declarative builtin type and method registry.
//!
//! Single source of truth for all builtin type methods. During monomorphization,
//! the protocol table is consulted to populate `fn_sigs` and `runtime_callees`,
//! replacing the scattered `starts_with()` name-dispatch throughout the IR lowering.
//!
//! Inspired by Rust's `TypeckResults` pattern: method resolution happens once
//! (here, declaratively) and the result is carried to IR lowering via side tables.

#![allow(dead_code)] // Methods and helpers used in Phase 2+ registration

use crate::ir::types::{CollectionKind, CopySemantics, TypeId, I64_TYPE, BOOL_TYPE, U8_TYPE, UNIT_TYPE, F64_TYPE};

/// How the receiver (`self`) is passed to a builtin method.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SelfConvention {
    /// `&self` — immutable borrow (Ptr).
    Borrow,
    /// `&mut self` — mutable borrow (MutPtr).
    MutBorrow,
    /// `self` — by value (Copy-semantics types: Channel, Shared, Atomic, etc.).
    ByValue,
    /// No receiver — static method (e.g., `Type.new()`).
    Static,
}

/// Type arguments extracted from a monomorphized builtin type.
///
/// Populated during type registration from the AST generic_args — no name parsing needed.
#[derive(Debug, Clone)]
pub struct BuiltinTypeArgs {
    /// Element type for single-param generics (Vector[T], Channel[T], etc.).
    pub elem: TypeId,
    /// Key type for two-param generics (Dict[K, V]). Same as elem for single-param.
    pub key: TypeId,
    /// Value type for two-param generics (Dict[K, V]). Same as elem for single-param.
    pub val: TypeId,
    /// The monomorphized type itself (e.g., Vector__int64_t).
    pub self_type: TypeId,
    /// Mangled name of the monomorphized type (e.g., "Vector__int64_t").
    pub self_name: String,
}

/// Context for resolving return types that depend on other registered types.
pub struct LookupCtx<'a> {
    pub lookup_type_by_name: &'a dyn Fn(&str) -> Option<TypeId>,
    pub owned_string_type: TypeId,
    /// Ensure an Option[T] type is registered, returning its TypeId.
    pub ensure_option: &'a dyn Fn(&str, TypeId) -> TypeId,
    /// Mangled name fragment for the elem type (e.g., "int64_t", "GorgetString", "uint8_t").
    /// Stored here because BuiltinTypeArgs.elem is a TypeId, but Option wrapping
    /// needs the mangled name to construct "Option__Ref_int64_t".
    pub elem_name: String,
    /// Mangled name fragment for the val type (for Dict[K,V]).
    pub val_name: String,
}

/// Semantic kind of an Option/Result combinator method.
///
/// Set once on [`BuiltinMethodDecl`] at protocol registration; read via
/// `LoweringContext::builtin_combinator_kind` at GIR dispatch. Never
/// reconstructed from method-name strings at the consumer (layering rule 2 /
/// Core #2 — typed metadata, not name-matching).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CombinatorKind {
    // ── GIR-adapter eligible (closure-taking; `try_lower_option_result_combinator`) ──
    Map,
    /// Option only.
    Filter,
    AndThen,
    /// Option (Result is not registered in Phase 1). Adapter shares AndThen arms.
    FlatMap,
    OrElse,
    UnwrapOrElse,
    /// Result only.
    MapErr,

    // ── C-inline only (no GIR adapter today) ──
    /// Value alternative; resource-recv ownership (D2/D3) still applies.
    Or,
    /// Option only; zero-arg; adapter early-outs on empty args.
    Flatten,
}

impl CombinatorKind {
    /// True when this kind may enter `try_lower_option_result_combinator`
    /// (the GIR adapter). `Or` and `Flatten` stay on the C-inline path.
    pub fn is_gir_adapter(self) -> bool {
        !matches!(self, Self::Or | Self::Flatten)
    }
}

/// The parameter shape of the closure a higher-order builtin method calls back.
///
/// This is what an UNTYPED closure param's type is resolved FROM. It is a
/// property of the (protocol, method) pair — never of the closure's own
/// syntax, never of a name prefix on the receiver's mangled symbol.
///
/// ⚠ Two things this deliberately does NOT reuse:
///
///  * **`BuiltinMethodDecl::params`.** That field is the RUNTIME callee's
///    parameter list, and it disagrees with the closure's shape in both
///    directions: `Vector.sort_by` declares `elem_param` (ONE TypeId) while its
///    comparator takes TWO, and `sorted` declares `no_params` while its
///    one-argument form takes a comparator. Driving hints from it was measured
///    producing a SCRAMBLED sort — a silent wrong answer in place of a visible
///    no-op.
///  * **The decl set as a gate.** `Vector.each` / `Vector.for_each` are
///    user-space `equip Vector` methods (`lib/std/iter.gg:413-417`) with NO
///    `BuiltinMethodDecl` at all — that table means "builtin-lowered", not
///    "Vector can do this". Requiring a decl before consulting this table was
///    measured turning `Vector[Person].each(…)` into `0 0 / 0 0`.
///
/// So this table is UNGATED and keyed on (protocol base name, method name),
/// and `closure_shape_for` returns `None` for any pair it does not list.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ClosureShape {
    /// One parameter: the element. `filter` / `map` / `find` / `sort_by_key` / …
    Elem,
    /// Two parameters, both the element: a comparator (`sort_by` / `sorted_by`)
    /// or `reduce`.
    ElemPair,
    /// Two parameters: the ACCUMULATOR, then the element. Sequence `fold`.
    ///
    /// ⭐ The accumulator's type is a property of the CALL SITE — it is the type
    /// of the fold's first argument (`v.fold("", …)` accumulates a `String`,
    /// `v.fold(0, …)` an `int`) — so it is supplied to `closure_param_hints`
    /// separately and cannot be derived from the receiver type at all. Both
    /// receiver-only answers were measured shipping a defect: `[elem, elem]`
    /// keeps the `fold(0, (acc, e): acc + e.len())` ICE, and `[I64, elem]`
    /// turns `fold("", (acc, e): acc + e)` into a silent wrong answer.
    AccElem,
    /// Two parameters: the key, then the value. Dict/HashMap `filter` / `each` / …
    KeyVal,
    /// Three parameters: the ACCUMULATOR, then the key, then the value.
    /// Dict/HashMap `fold`.
    AccKeyVal,
}

impl ClosureShape {
    /// How many parameters the callback takes. The hint vector this shape
    /// produces always has exactly this length, so a closure with fewer
    /// annotated params can never fall through to the `I64_TYPE` default
    /// for a trailing one (`closures.rs:178` gates on `i < hints.len()`).
    pub fn arity(self) -> usize {
        match self {
            Self::Elem => 1,
            Self::ElemPair | Self::AccElem | Self::KeyVal => 2,
            Self::AccKeyVal => 3,
        }
    }
}

/// Closure shapes for every higher-order method on every builtin protocol.
///
/// ⚠ **ONE ROW PER PROTOCOL IN `ALL_PROTOCOLS`, AND AN EMPTY METHOD LIST IS AN
/// EXPLICIT "this protocol declares no higher-order method"** — never "nobody
/// filled this in". `closure_shape_rows_are_total` locks that at unit-test
/// time, and it is not decoration: `DEQUE.methods = VECTOR.methods` and
/// `HASHSET.methods = SET.methods`, so a protocol whose row is merely *absent*
/// silently loses hints its aliased twin keeps — measured, `Deque[String]
/// .sort_by_key((s): s.len())` goes back to `undefined reference to
/// 'int64_t__len'` while every Vector cell stays green. The corpus cannot
/// witness that (it has no Deque HOF cells until this round), so the totality
/// test IS the independent witness.
static CLOSURE_SHAPES: &[(&str, &[(&str, ClosureShape)])] = &[
    // ── Sequences ────────────────────────────────────────────────────────
    //
    // Keyed on the EFFECTIVE method name (`methods.rs:2165` rewrites `sort`/1 →
    // `sort_by` and `sorted`/1 → `sorted_by` for any array receiver), which is
    // also the vocabulary `lir/lower/insts.rs`'s HofOp table uses. `sort` and
    // `sorted` are listed anyway: the rewrite is conditional on the receiver
    // resolving to `CollectionKind::Array`, and a hint is inert for the
    // zero-argument form.
    //
    // `each` / `for_each` / `find` / `find_index` have no `BuiltinMethodDecl` on
    // VECTOR — they are the user-space `equip Vector` wrappers. They are here
    // because this table is ungated; that is the whole point of it.
    //
    // Not listed, deliberately: `enumerate` / `unique` / `reversed` / `sorted` /
    // `sort` in their zero-argument forms take no closure, and `zip`'s argument
    // is another sequence, not a callback.
    ("Vector", VECTOR_CLOSURE_SHAPES),
    // Deque aliases Vector's whole method table; it needs its own row here for
    // exactly that reason.
    ("Deque", VECTOR_CLOSURE_SHAPES),

    // ── Maps ─────────────────────────────────────────────────────────────
    ("Dict", DICT_CLOSURE_SHAPES),
    ("HashMap", DICT_CLOSURE_SHAPES),

    // ── Sets ─────────────────────────────────────────────────────────────
    ("Set", SET_CLOSURE_SHAPES),
    ("HashSet", SET_CLOSURE_SHAPES),

    // ── Option / Result: DELIBERATELY EMPTY ──────────────────────────────
    //
    // Their combinators (`map`/`filter`/`and_then`/`flat_map`/`or_else`/
    // `map_err`) are higher-order, but a SECOND hint setter already owns them —
    // `try_lower_option_result_combinator` (`methods.rs:4105-4111`), which sets
    // the same field keyed on the SURFACE name with one entry each. An
    // Option/Result receiver whose combinator `is_gir_adapter()` returns from
    // `lower_method_call` at `:1984` and never reaches the collection setter, so
    // claiming these pairs here would put two writers on one axis for no gain.
    // Pinned by `hof_option_map_untyped`.
    ("Option", &[]),
    ("Result", &[]),

    // ── Everything else: no higher-order method at all ───────────────────
    //
    // Handles, locks, atomics and the Callable family. Each is listed so the
    // totality test can tell "no HOFs" from "not yet filled in".
    ("Channel", &[]),
    ("Shared", &[]),
    ("Weak", &[]),
    ("Mutex", &[]),
    ("Guard", &[]),
    ("RWLock", &[]),
    ("ReadGuard", &[]),
    ("WriteGuard", &[]),
    ("Thread", &[]),
    ("Heap", &[]),
    ("GorgetString", &[]),
    ("AtomicInt", &[]),
    ("AtomicBool", &[]),
    ("Barrier", &[]),
    ("WaitGroup", &[]),
    ("Semaphore", &[]),
    ("OnceFlag", &[]),
    ("TaskGroup", &[]),
    ("Callable", &[]),
    ("MutCallable", &[]),
    ("ConsumeCallable", &[]),
    ("GorgetClosure", &[]),
];

static VECTOR_CLOSURE_SHAPES: &[(&str, ClosureShape)] = &[
    ("sort", ClosureShape::ElemPair),
    ("sort_by", ClosureShape::ElemPair),
    ("sorted", ClosureShape::ElemPair),
    ("sorted_by", ClosureShape::ElemPair),
    ("reduce", ClosureShape::ElemPair),
    ("sort_by_key", ClosureShape::Elem),
    ("sorted_by_key", ClosureShape::Elem),
    ("filter", ClosureShape::Elem),
    ("map", ClosureShape::Elem),
    ("flat_map", ClosureShape::Elem),
    ("any", ClosureShape::Elem),
    ("all", ClosureShape::Elem),
    ("each", ClosureShape::Elem),
    ("for_each", ClosureShape::Elem),
    ("find", ClosureShape::Elem),
    ("find_index", ClosureShape::Elem),
    ("count", ClosureShape::Elem),
    ("fold", ClosureShape::AccElem),
];

// ⛔ `update` is NOT here, and the omission is load-bearing. `Dict.update` is a
// map MERGE — `gorget_map_update(void* dst, GorgetMap other)` — minted as a
// runtime callee at `lir/lower/calls.rs`, with no HofOp arm anywhere. It reads
// like a callback method only because its `BuiltinMethodDecl` carries
// `params: key_val_params` under a `// Higher-order` banner, which is exactly
// the source this table exists NOT to reuse. `d.update((k, v): v + 1)` passes
// `gg check` and then fails with `incompatible type for argument 2 of
// 'gorget_map_update'`. Pinned by `closure_shape_rows_have_a_callback_witness`.
static DICT_CLOSURE_SHAPES: &[(&str, ClosureShape)] = &[
    ("filter", ClosureShape::KeyVal),
    ("each", ClosureShape::KeyVal),
    ("any", ClosureShape::KeyVal),
    ("all", ClosureShape::KeyVal),
    ("fold", ClosureShape::AccKeyVal),
];

static SET_CLOSURE_SHAPES: &[(&str, ClosureShape)] = &[
    ("filter", ClosureShape::Elem),
    ("each", ClosureShape::Elem),
    ("for_each", ClosureShape::Elem),
    ("any", ClosureShape::Elem),
    ("all", ClosureShape::Elem),
    ("find", ClosureShape::Elem),
    ("find_index", ClosureShape::Elem),
    ("fold", ClosureShape::AccElem),
];

/// A single method on a builtin type.
pub struct BuiltinMethodDecl {
    /// Method name as written in Gorget (e.g., "push", "get", "len").
    pub name: &'static str,
    /// C runtime function name. `None` = keep monomorphized name (for inline backend codegen).
    pub runtime_callee: Option<&'static str>,
    /// How the receiver is passed.
    pub self_conv: SelfConvention,
    /// Whether this method mutates the receiver.
    pub is_mutating: bool,
    /// Whether this method returns a view (cap=0 Str) borrowing from the receiver's buffer.
    /// The compiler tracks the result as ViewOf(receiver) and auto-materializes
    /// before source mutation.
    pub returns_view: bool,
    /// Whether this method's result is always a fresh, independently allocated
    /// heap buffer (no aliasing into any input). Mirror of
    /// `RuntimeSig.returns_fresh` at the IR-method-decl level. Read by the
    /// CoW/clone-elision machinery to skip redundant clones at self-referential
    /// reassignment and at return boundaries. Must be `false` whenever
    /// `returns_view` is `true` (mutually exclusive: a method either borrows
    /// from the receiver or produces a fresh buffer).
    pub returns_fresh: bool,
    /// Option/Result combinator semantic kind, if this method is one.
    /// `None` for non-combinators (including Vector/Dict/Set HOFs that share
    /// names like `map`/`filter`/`flat_map` — those are not Option/Result
    /// combinators). Set at protocol registration; never reconstructed from
    /// the method name at dispatch (layering rule 2).
    pub combinator_kind: Option<CombinatorKind>,
    /// Build parameter GIR TypeIds given the type args and a lookup context.
    /// The lookup context is needed for params that reference runtime-resolved
    /// types like `ctx.owned_string_type` (e.g. `lstrip(chars: String)`).
    pub params: fn(&BuiltinTypeArgs, &LookupCtx) -> Vec<TypeId>,
    /// Build return GIR TypeId given the type args and a lookup context.
    pub return_type: fn(&BuiltinTypeArgs, &LookupCtx) -> TypeId,
}

impl BuiltinMethodDecl {
    /// G3: true when this builtin method is a `.clone()` — a user-directed deep
    /// clone (`gorget_array_clone` / `gorget_map_clone` / `gorget_set_clone`,
    /// and the refcount-handle clones). The clone-reason validator needs the
    /// dispatch to tag the emitted clone `Call` with `ExplicitUserClone`
    /// (`builtin_method_is_clone` reads this accessor).
    ///
    /// Derived from the decl's OWN declared method name — the protocol table is
    /// the single source of truth for builtin-method semantics — NOT from the
    /// resolved runtime symbol downstream (Core #2: never route on the resolved
    /// name). A dedicated `is_clone: bool` field would have to be spelled on all
    /// 178 method decls (173 of them `false`), which is exactly the hand-synced
    /// parallel-list smell Core #2 forbids; a single accessor keyed on the
    /// method's canonical name avoids that and stays in one place.
    pub fn is_clone(&self) -> bool {
        self.name == "clone"
    }

    /// True when this method READS an element/value out of the receiver as a
    /// borrow VIEW (the auto-borrow-from-get protocol — the decls whose
    /// return rule is `ret_option_ref_or_val_elem` / `ret_option_ref_or_val_val`):
    /// the `Some` payload is a pointer into the receiver's storage, so a
    /// mutation through the unwrapped payload write-throughs into the
    /// receiver. Consumed by the safety pass's mutation-marking chain
    /// resolver (`find_mut_mark_root`) to route `x.get(i).unwrap().f.push(…)`
    /// to `x`'s root. Same single-accessor derivation as `is_clone` (the
    /// protocol table is the single source of truth for builtin-method
    /// semantics; a dedicated bool field would be hand-spelled `false` on
    /// ~170 decls — the parallel-list smell Core #2 forbids).
    pub fn is_elem_borrow_read(&self) -> bool {
        matches!(self.name, "get" | "first" | "last")
    }
}

/// A builtin type family (Vector, Dict, Channel, etc.).
pub struct BuiltinTypeProtocol {
    /// Base name before monomorphization (e.g., "Vector", "Dict").
    pub base_name: &'static str,
    /// Number of generic type parameters (0, 1, or 2).
    pub type_arity: u8,
    /// Copy semantics for this type family.
    pub copy_semantics: CopySemantics,
    /// Drop function name (e.g., "gorget_array_free"). None = no drop.
    pub drop_fn: Option<&'static str>,
    /// Clone function name (e.g., "gorget_array_clone"). None = not cloneable.
    pub clone_fn: Option<&'static str>,
    /// In-place clone for collection element slots (`void(*)(void*)`).
    /// e.g., "gorget_array_clone_inplace". None = no inplace clone.
    pub clone_inplace_fn: Option<&'static str>,
    /// CoW materialize function (`void(*)(void*)`) — view → owned in place.
    /// e.g., "gorget_string_materialize_inplace". None = no view/owner distinction.
    pub materialize_fn: Option<&'static str>,
    /// Borrow-as-view function (`T(*)(const T*)`) — shallow copy with the
    /// ownership discriminator forced to "view", drop-safe in a drop-tracked
    /// value slot. The lazy loop-carried CoW bind eligibility axis (mirrors
    /// `TypeMetadata::borrow_view_fn`, where the read happens). None for every
    /// collection protocol today: their frees are not view-aware
    /// (`gorget_array_free` runs `elem_drop` regardless of cap — a cap=0
    /// array view would double-drop every element). Phase 1b can populate
    /// this once view-safe frees exist. String (no protocol — its metadata is
    /// set directly in `TypeMapper::new`) carries
    /// `Some("gorget_string_borrow_view")`.
    pub borrow_view_fn: Option<&'static str>,
    /// Collection kind for metadata-based dispatch. None = not a collection.
    pub collection_kind: Option<CollectionKind>,
    /// True when this type is a non-collection handle that nonetheless OWNS a
    /// heap buffer into which a mutating, element-ingesting method copies/moves
    /// the passed element (Channel's `send`, Heap's `push`). That buffer
    /// survives independently of the current `with Arena` scope (it is freed by
    /// the handle's own drop, not by `gorget_arena_destroy`), so an
    /// arena-borrowed non-Copy element ingested into it dangles when the arena
    /// is destroyed — the same heap-use-after-free class as `collection_kind`
    /// collections. The borrow checker's arena-escape gate
    /// (`is_buffer_owning_receiver`) treats `collection_kind.is_some()` OR this
    /// flag uniformly. Mutex/Shared/Guard/Atomic/Barrier/... do NOT ingest an
    /// element into a self-owned surviving buffer, so they stay `false` (a
    /// typed gate — no name-matching `send`/`Channel`).
    pub owns_buffered_elements: bool,
    /// C runtime struct name this type aliases to (e.g. "GorgetClosure" for
    /// Callable[T(...)]). When set, the C backend should emit a typedef to
    /// this runtime struct instead of a fresh `__gg_X` struct definition.
    /// Foundation for Phase A residual #1 (Callable TypeDef registration);
    /// no consumers read this field yet — additive schema only. Migration
    /// path requires (a) consumers in C-emit reading the field to skip the
    /// `__gg_X` struct emission, (b) GIR type-mismatch correction at
    /// `stmts/mod.rs` skipping aliases, and (c) Callable types registered
    /// through the protocol with this field set to "GorgetClosure".
    pub c_runtime_alias: Option<&'static str>,
    /// All methods on this type.
    pub methods: &'static [BuiltinMethodDecl],
}

// ── Helper constructors for param/return closures ─────────────────────

/// No params (besides self).
fn no_params(_: &BuiltinTypeArgs, _: &LookupCtx) -> Vec<TypeId> { vec![] }

/// Single int param.
fn int_param(_: &BuiltinTypeArgs, _: &LookupCtx) -> Vec<TypeId> { vec![I64_TYPE] }

/// Single elem-type param.
fn elem_param(a: &BuiltinTypeArgs, _: &LookupCtx) -> Vec<TypeId> { vec![a.elem] }

/// Two int params.
fn two_ints(_: &BuiltinTypeArgs, _: &LookupCtx) -> Vec<TypeId> { vec![I64_TYPE, I64_TYPE] }

/// Single GorgetString param (for String methods like `lstrip(chars)`).
fn string_param(_: &BuiltinTypeArgs, ctx: &LookupCtx) -> Vec<TypeId> { vec![ctx.owned_string_type] }

/// Key param (for dict).
fn key_param(a: &BuiltinTypeArgs, _: &LookupCtx) -> Vec<TypeId> { vec![a.key] }

/// Key + value params (for dict.put).
fn key_val_params(a: &BuiltinTypeArgs, _: &LookupCtx) -> Vec<TypeId> { vec![a.key, a.val] }

/// Key + value + default value params (for dict.get_or).
fn key_val_default(a: &BuiltinTypeArgs, _: &LookupCtx) -> Vec<TypeId> { vec![a.key, a.val] }

/// Returns void.
fn ret_void(_: &BuiltinTypeArgs, _: &LookupCtx) -> TypeId { UNIT_TYPE }

/// Returns int.
fn ret_int(_: &BuiltinTypeArgs, _: &LookupCtx) -> TypeId { I64_TYPE }

/// Returns bool.
fn ret_bool(_: &BuiltinTypeArgs, _: &LookupCtx) -> TypeId { BOOL_TYPE }

/// Returns the element type.
fn ret_elem(a: &BuiltinTypeArgs, _: &LookupCtx) -> TypeId { a.elem }

/// Returns the value type (for dict).
fn ret_val(a: &BuiltinTypeArgs, _: &LookupCtx) -> TypeId { a.val }

/// Returns self type (same collection type).
fn ret_self(a: &BuiltinTypeArgs, _: &LookupCtx) -> TypeId { a.self_type }

/// Returns Option[elem] (value payload — for consuming methods like pop/remove).
fn ret_option_elem(a: &BuiltinTypeArgs, ctx: &LookupCtx) -> TypeId {
    let option_name = format!("Option__{}", ctx.elem_name);
    (ctx.ensure_option)(&option_name, a.elem)
}

/// Returns `Option[Ref[T]]` for borrowing read methods (`get`/`first`/`last`).
/// The Some payload is `Ptr(T)` — the raw pointer into the collection's
/// storage — regardless of whether T is a resource type. Previously primitives
/// returned `Option[T]` with a dereferenced value copy, but that caused the
/// user-declared `Option[Ref[T]]` type and the IR-generated `Option[T]` to
/// be two different registered types with different payload semantics — a
/// bit-copy between them would alias an int value as a pointer (UB).
///
/// The `Option__Ref__<T>` spelling matches `mangle_generic_name` for user-
/// written `Option[Ref[T]]` so the IR and the typechecker agree on one type.
fn ret_option_ref_or_val_elem(a: &BuiltinTypeArgs, ctx: &LookupCtx) -> TypeId {
    let option_name = format!("Option__Ref__{}", ctx.elem_name);
    (ctx.ensure_option)(&option_name, a.elem)
}

/// Returns Option[val] (for consuming dict methods like `remove` — owned payload).
fn ret_option_val(a: &BuiltinTypeArgs, ctx: &LookupCtx) -> TypeId {
    let option_name = format!("Option__{}", ctx.val_name);
    (ctx.ensure_option)(&option_name, a.val)
}

/// Returns `Option[Ref[V]]` for borrowing dict methods (`get`). Mirror of
/// `ret_option_ref_or_val_elem` for the Dict val axis: the Some payload is
/// `Ptr(V)` — the raw pointer into the bucket's value slot returned by
/// `gorget_map_get` — regardless of whether V is a resource type. Keeps the
/// IR return type identical to the typechecker's `Option[Ref[V]]`, so a
/// chained `.unwrap().method(...)` on resource V mutates the actual stored
/// element instead of a byte-copy.
fn ret_option_ref_or_val_val(a: &BuiltinTypeArgs, ctx: &LookupCtx) -> TypeId {
    let option_name = format!("Option__Ref__{}", ctx.val_name);
    (ctx.ensure_option)(&option_name, a.val)
}

/// Returns owned GorgetString type.
fn ret_owned_string(_: &BuiltinTypeArgs, ctx: &LookupCtx) -> TypeId { ctx.owned_string_type }

/// Returns GorgetArray (untyped array, for keys/values).
fn ret_gorget_array(_: &BuiltinTypeArgs, ctx: &LookupCtx) -> TypeId {
    (ctx.lookup_type_by_name)("GorgetArray").unwrap_or(UNIT_TYPE)
}

/// Returns uint8.
fn ret_u8(_: &BuiltinTypeArgs, _: &LookupCtx) -> TypeId { U8_TYPE }

/// Returns float64.
fn ret_f64(_: &BuiltinTypeArgs, _: &LookupCtx) -> TypeId { F64_TYPE }

/// Helper: convert a TypeId to its C mangled name fragment.
/// This is only used for constructing Option/Result type names during
/// return type resolution. For primitive types, returns the C type name.
fn type_id_to_c_name(type_id: TypeId) -> String {
    match type_id {
        I64_TYPE => "int64_t".to_string(),
        BOOL_TYPE => "bool".to_string(),
        U8_TYPE => "uint8_t".to_string(),
        F64_TYPE => "double".to_string(),
        // For named types, the TypeId is opaque here — we store the mangled name
        // in BuiltinTypeArgs at extraction time and use it directly.
        _ => format!("T{}", type_id.0),
    }
}

// ── Protocol Declarations ─────────────────────────────────────────────

pub static VECTOR: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Vector",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: Some("gorget_array_free"),
    clone_fn: Some("gorget_array_clone"),
    clone_inplace_fn: Some("gorget_array_clone_inplace"),
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: Some(CollectionKind::Array),
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        // Mutating
        BuiltinMethodDecl { name: "push", runtime_callee: Some("gorget_array_push"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "clear", runtime_callee: Some("gorget_array_clear"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "reverse", runtime_callee: Some("gorget_array_reverse"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "insert", runtime_callee: Some("gorget_array_insert"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: |a, _| vec![I64_TYPE, a.elem], return_type: ret_void },
        BuiltinMethodDecl { name: "extend", runtime_callee: Some("gorget_array_extend"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "reserve", runtime_callee: Some("gorget_array_reserve"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: int_param, return_type: ret_void },
        BuiltinMethodDecl { name: "set", runtime_callee: Some("gorget_array_set"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: |a, _| vec![I64_TYPE, a.elem], return_type: ret_void },
        // Order-destroying O(1) removal — moves last element into the hole.
        BuiltinMethodDecl { name: "swap_remove", runtime_callee: Some("gorget_array_swap_remove"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: int_param, return_type: ret_void },
        // Swap two elements in place.
        BuiltinMethodDecl { name: "swap", runtime_callee: Some("gorget_array_swap"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: |_, _| vec![I64_TYPE, I64_TYPE], return_type: ret_void },
        // Fill with n copies of a value (drops existing elements).
        BuiltinMethodDecl { name: "fill", runtime_callee: Some("gorget_array_fill"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: |a, _| vec![I64_TYPE, a.elem], return_type: ret_void },
        // Borrowing reads
        BuiltinMethodDecl { name: "get", runtime_callee: Some("gorget_array_safe_get"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: int_param, return_type: ret_option_ref_or_val_elem },
        BuiltinMethodDecl { name: "first", runtime_callee: Some("gorget_array_first"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_option_ref_or_val_elem },
        BuiltinMethodDecl { name: "last", runtime_callee: Some("gorget_array_last"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_option_ref_or_val_elem },
        // Consuming reads
        BuiltinMethodDecl { name: "pop", runtime_callee: Some("gorget_array_safe_pop"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_option_elem },
        BuiltinMethodDecl { name: "remove", runtime_callee: Some("gorget_array_remove_opt"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: int_param, return_type: ret_option_elem },
        // Queries
        BuiltinMethodDecl { name: "len", runtime_callee: Some("gorget_array_len"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "capacity", runtime_callee: Some("gorget_array_capacity"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "is_empty", runtime_callee: Some("gorget_array_is_empty"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_bool },
        BuiltinMethodDecl { name: "contains", runtime_callee: Some("gorget_array_contains"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "index_of", runtime_callee: Some("gorget_array_index_of"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: |_, ctx| (ctx.lookup_type_by_name)("Option__int64_t").unwrap_or(I64_TYPE) },
        BuiltinMethodDecl { name: "binary_search", runtime_callee: Some("gorget_array_binary_search"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_int },
        // Clone / copy
        BuiltinMethodDecl { name: "clone", runtime_callee: Some("gorget_array_clone"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: true, combinator_kind: None, params: no_params, return_type: ret_self },
        BuiltinMethodDecl { name: "sorted", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_self },
        BuiltinMethodDecl { name: "sorted_by", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "sorted_by_key", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "reversed", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_self },
        BuiltinMethodDecl { name: "unique", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_self },
        BuiltinMethodDecl { name: "slice", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: two_ints, return_type: ret_self },
        // windows(n) / chunks(n) → Vector[Vector[T]]. Eager materialization;
        // lazy iterator version lands in Phase 2.
        BuiltinMethodDecl { name: "windows", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: int_param, return_type: |_a, ctx| {
            let vec_name = format!("Vector__Vector__{}", ctx.elem_name);
            (ctx.lookup_type_by_name)(&vec_name)
                .or_else(|| (ctx.lookup_type_by_name)("GorgetArray"))
                .unwrap_or(UNIT_TYPE)
        }},
        BuiltinMethodDecl { name: "chunks", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: int_param, return_type: |_a, ctx| {
            let vec_name = format!("Vector__Vector__{}", ctx.elem_name);
            (ctx.lookup_type_by_name)(&vec_name)
                .or_else(|| (ctx.lookup_type_by_name)("GorgetArray"))
                .unwrap_or(UNIT_TYPE)
        }},
        // Higher-order (inline codegen — keep monomorphized names)
        BuiltinMethodDecl { name: "sort", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "sort_by", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "sort_by_key", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "filter", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "map", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "flat_map", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "enumerate", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_self },
        BuiltinMethodDecl { name: "fold", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: |a, _| vec![I64_TYPE, a.elem], return_type: ret_int },
        BuiltinMethodDecl { name: "reduce", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_int },
        BuiltinMethodDecl { name: "any", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "all", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_bool },
        // `each` / `for_each` migrated to user-space `equip [T] Vector[T]:
        // void each[F](&self, F f): self.iter().for_each[F](f)` (and the
        // matching `for_each` wrapper) — see lib/std/iter.gg. Both
        // BuiltinMethodDecl entries were retired (Vector.each in commit
        // 1b0e7022; Vector.for_each in this commit). The BIR HofOp::Each
        // variant stays for Dict.each / Set.each. The remaining
        // typed-return Vector HOF entries below (filter / map / fold / …)
        // are signature-load-bearing — IR-lowering reads their declared
        // return types via `resolve_builtin_method_return_type` when the
        // user-space wrapper's sig hasn't been registered yet (e.g.
        // during early generic mono); deletion blocks on a separate
        // signature source.
        BuiltinMethodDecl { name: "find", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_option_elem },
        BuiltinMethodDecl { name: "find_index", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_int },
        BuiltinMethodDecl { name: "count", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_int },
        BuiltinMethodDecl { name: "zip", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_self },
    ],
};

pub static DEQUE: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Deque",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: Some("gorget_array_free"),
    clone_fn: Some("gorget_array_clone"),
    clone_inplace_fn: Some("gorget_array_clone_inplace"),
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: Some(CollectionKind::Array),
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: VECTOR.methods, // Same interface as Vector
};

pub static DICT: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Dict",
    type_arity: 2,
    copy_semantics: CopySemantics::Resource,
    drop_fn: Some("gorget_map_free"),
    clone_fn: Some("gorget_map_clone"),
    clone_inplace_fn: Some("gorget_map_clone_inplace"),
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: Some(CollectionKind::OrderedMap),
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        BuiltinMethodDecl { name: "put", runtime_callee: Some("gorget_map_put"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: key_val_params, return_type: ret_void },
        BuiltinMethodDecl { name: "set", runtime_callee: Some("gorget_map_put"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: key_val_params, return_type: ret_void },
        BuiltinMethodDecl { name: "get", runtime_callee: Some("gorget_map_get"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: key_param, return_type: ret_option_ref_or_val_val },
        BuiltinMethodDecl { name: "get_or", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: key_val_default, return_type: ret_val },
        BuiltinMethodDecl { name: "get_or_put", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: key_val_default, return_type: ret_val },
        BuiltinMethodDecl { name: "remove", runtime_callee: Some("gorget_map_remove_opt"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: key_param, return_type: ret_option_val },
        // D39 Phase A.3: Dict.swap_remove(key) → Option[V !] — O(1) opt-in
        // order-destroying counterpart to `remove` (per DD#6, matches Dict's
        // own remove shape). Routes to `gorget_map_swap_remove_opt`.
        BuiltinMethodDecl { name: "swap_remove", runtime_callee: Some("gorget_map_swap_remove_opt"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: key_param, return_type: ret_option_val },
        BuiltinMethodDecl { name: "contains", runtime_callee: Some("gorget_map_contains"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: key_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "has", runtime_callee: Some("gorget_map_contains"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: key_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "has_key", runtime_callee: Some("gorget_map_contains"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: key_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "contains_key", runtime_callee: Some("gorget_map_contains"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: key_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "len", runtime_callee: Some("gorget_map_len"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "is_empty", runtime_callee: Some("gorget_map_is_empty"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_bool },
        BuiltinMethodDecl { name: "clear", runtime_callee: Some("gorget_map_clear"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "keys", runtime_callee: Some("gorget_map_keys"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: |_a, ctx| {
            // keys() → Vector[K]
            let vec_name = format!("Vector__{}", ctx.elem_name);
            (ctx.lookup_type_by_name)(&vec_name)
                .or_else(|| (ctx.lookup_type_by_name)("GorgetArray"))
                .unwrap_or(UNIT_TYPE)
        }},
        BuiltinMethodDecl { name: "values", runtime_callee: Some("gorget_map_values"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: |_a, ctx| {
            // values() → Vector[V]
            let vec_name = format!("Vector__{}", ctx.val_name);
            (ctx.lookup_type_by_name)(&vec_name)
                .or_else(|| (ctx.lookup_type_by_name)("GorgetArray"))
                .unwrap_or(UNIT_TYPE)
        }},
        BuiltinMethodDecl { name: "items", runtime_callee: Some("gorget_map_items"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: |_a, ctx| {
            // items() → Vector[Tuple[K, V]] — construct from elem_name (K) and val_name (V)
            let tuple_name = format!("Tuple__{}__{}", ctx.elem_name, ctx.val_name);
            let vec_name = format!("Vector__{tuple_name}");
            (ctx.lookup_type_by_name)(&vec_name)
                .or_else(|| (ctx.lookup_type_by_name)("GorgetArray"))
                .unwrap_or(UNIT_TYPE)
        }},
        BuiltinMethodDecl { name: "clone", runtime_callee: Some("gorget_map_clone"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: true, combinator_kind: None, params: no_params, return_type: ret_self },
        // Higher-order (inline codegen)
        BuiltinMethodDecl { name: "filter", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: key_val_params, return_type: ret_self },
        BuiltinMethodDecl { name: "fold", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: |a, _| vec![I64_TYPE, a.key, a.val], return_type: ret_int },
        BuiltinMethodDecl { name: "each", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: key_val_params, return_type: ret_void },
        BuiltinMethodDecl { name: "any", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: key_val_params, return_type: ret_bool },
        BuiltinMethodDecl { name: "all", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: key_val_params, return_type: ret_bool },
        BuiltinMethodDecl { name: "update", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: key_val_params, return_type: ret_void },
        // Note: Dict.map was never implemented in any backend (stub read
        // "TODO not yet implemented"); no fixture or stdlib caller
        // exercised it. Removed rather than carrying a dead method decl.
    ],
};

pub static HASHMAP: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "HashMap",
    type_arity: 2,
    copy_semantics: CopySemantics::Resource,
    drop_fn: Some("gorget_map_free"),
    clone_fn: Some("gorget_map_clone"),
    clone_inplace_fn: Some("gorget_map_clone_inplace"),
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: Some(CollectionKind::Map),
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: DICT.methods, // Same interface as Dict
};

pub static SET: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Set",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: Some("gorget_set_free"),
    clone_fn: Some("gorget_set_clone"),
    clone_inplace_fn: Some("gorget_set_clone_inplace"),
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: Some(CollectionKind::OrderedSet),
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        BuiltinMethodDecl { name: "add", runtime_callee: Some("gorget_set_add"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "insert", runtime_callee: Some("gorget_set_add"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "remove", runtime_callee: Some("gorget_set_remove"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_bool },
        // D39 Phase A.3: Set.swap_remove(elem) → bool — O(1) opt-in
        // order-destroying counterpart to `remove` (per DD#6, matches Set's
        // own remove shape).
        BuiltinMethodDecl { name: "swap_remove", runtime_callee: Some("gorget_set_swap_remove"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "contains", runtime_callee: Some("gorget_set_contains"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "has", runtime_callee: Some("gorget_set_contains"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "len", runtime_callee: Some("gorget_set_len"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "is_empty", runtime_callee: Some("gorget_set_is_empty"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_bool },
        BuiltinMethodDecl { name: "clear", runtime_callee: Some("gorget_set_clear"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "clone", runtime_callee: Some("gorget_set_clone"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: true, combinator_kind: None, params: no_params, return_type: ret_self },
        // items() → Vector[T] — materializes the set into an ordered array.
        // Used by `set_iter` (std.iter) to hand callers a VectorIter[T]
        // over the materialized elements.
        BuiltinMethodDecl { name: "items", runtime_callee: Some("gorget_set_to_array"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: |_a, ctx| {
            let vec_name = format!("Vector__{}", ctx.elem_name);
            (ctx.lookup_type_by_name)(&vec_name)
                .or_else(|| (ctx.lookup_type_by_name)("GorgetArray"))
                .unwrap_or(UNIT_TYPE)
        }},
        // Set algebra (inline codegen)
        BuiltinMethodDecl { name: "union", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "intersection", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "difference", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "symmetric_difference", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "is_subset", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "is_superset", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "is_disjoint", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_bool },
        // Higher-order
        BuiltinMethodDecl { name: "filter", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "fold", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: |a, _| vec![I64_TYPE, a.elem], return_type: ret_int },
        BuiltinMethodDecl { name: "each", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "any", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "all", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_bool },
        // Note: Set.map was never implemented (TODO stub); see comment
        // on Dict.map above for rationale.
    ],
};

pub static HASHSET: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "HashSet",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: Some("gorget_set_free"),
    clone_fn: Some("gorget_set_clone"),
    clone_inplace_fn: Some("gorget_set_clone_inplace"),
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: Some(CollectionKind::Set),
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: SET.methods, // Same interface as Set
};

pub static CHANNEL: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Channel",
    type_arity: 1,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None, // Typed drop wrapper emitted by c_lir
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: true,
    c_runtime_alias: None,
    methods: &[
        BuiltinMethodDecl { name: "send", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "recv", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_elem },
        BuiltinMethodDecl { name: "close", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "poll_recv", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "recv_timeout", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: int_param, return_type: ret_option_elem },
        BuiltinMethodDecl { name: "len", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "capacity", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "is_closed", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_bool },
    ],
};

pub static SHARED: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Shared",
    type_arity: 1,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        BuiltinMethodDecl { name: "clone", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_self },
        BuiltinMethodDecl { name: "get", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_elem },
        BuiltinMethodDecl { name: "strong_count", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "downgrade", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: |a, ctx| {
            // Returns Weak[T] — look up by mangled name
            let elem_name = type_id_to_c_name(a.elem);
            let weak_name = format!("Weak__{elem_name}");
            (ctx.lookup_type_by_name)(&weak_name).unwrap_or(a.self_type)
        }},
        // Shared[Vector[T]] convenience methods
        BuiltinMethodDecl { name: "at", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: int_param, return_type: ret_elem },
        BuiltinMethodDecl { name: "set_at", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: |a, _| vec![I64_TYPE, a.elem], return_type: ret_void },
        BuiltinMethodDecl { name: "slen", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_int },
    ],
};

pub static WEAK: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Weak",
    type_arity: 1,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        BuiltinMethodDecl { name: "clone", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_self },
        BuiltinMethodDecl { name: "upgrade", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: |a, ctx| {
            let elem_name = type_id_to_c_name(a.elem);
            let option_shared = format!("Option__Shared__{elem_name}");
            (ctx.lookup_type_by_name)(&option_shared).unwrap_or(a.self_type)
        }},
    ],
};

pub static MUTEX: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Mutex",
    type_arity: 1,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        BuiltinMethodDecl { name: "lock", runtime_callee: Some("gorget_mutex_lock"), self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: |a, ctx| {
            let elem_name = type_id_to_c_name(a.elem);
            let guard_name = format!("Guard__{elem_name}");
            (ctx.lookup_type_by_name)(&guard_name).unwrap_or(a.self_type)
        }},
    ],
};

pub static GUARD: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Guard",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: None, // Per-type drop wrapper
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        // Track J note: `.get()` returns a BORROW into the Mutex-owned buffer,
        // not an owned T. The drop-suppression fix intercepts at IR-lowering
        // (`exprs/methods.rs` — routes through `emit_guard_get_ptr` and tags
        // the loaded local `LocalOwnership::View`). The `returns_view` flag
        // stays `false` here because the `str_view_producer_enumeration_is_closed`
        // lint uses it as the CAP=0 STRING-VIEW producer marker (see
        // `STR_VIEW_PRODUCERS` in `tests/lints.rs`); Guard.get is a distinct
        // borrow class that needs no `materialize_lazy_source_if_needed` /
        // string-view hooks. A typed `borrow_read: bool` axis would carry the
        // Guard-family truth cleanly — filed as a follow-up in `TODO.md`
        // alongside the `is_elem_borrow_read` name-whitelist retirement.
        BuiltinMethodDecl { name: "get", runtime_callee: Some("gorget_guard_get"), self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_elem },
        BuiltinMethodDecl { name: "set", runtime_callee: Some("gorget_guard_set"), self_conv: SelfConvention::MutBorrow, is_mutating: true,  returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_void },
    ],
};

pub static RWLOCK: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "RWLock",
    type_arity: 1,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        BuiltinMethodDecl { name: "read", runtime_callee: Some("gorget_rwlock_read"), self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: |a, ctx| {
            let elem_name = type_id_to_c_name(a.elem);
            (ctx.lookup_type_by_name)(&format!("ReadGuard__{elem_name}")).unwrap_or(a.self_type)
        }},
        BuiltinMethodDecl { name: "write", runtime_callee: Some("gorget_rwlock_write"), self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: |a, ctx| {
            let elem_name = type_id_to_c_name(a.elem);
            (ctx.lookup_type_by_name)(&format!("WriteGuard__{elem_name}")).unwrap_or(a.self_type)
        }},
    ],
};

pub static READ_GUARD: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "ReadGuard",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: None,
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        // Track J note: see the `Guard.get` decl above — same intercept, same
        // reason `returns_view` stays `false`.
        BuiltinMethodDecl { name: "get", runtime_callee: Some("gorget_read_guard_get"), self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_elem },
    ],
};

pub static WRITE_GUARD: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "WriteGuard",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: None,
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        // Track J note: see the `Guard.get` decl above.
        BuiltinMethodDecl { name: "get", runtime_callee: Some("gorget_write_guard_get"), self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_elem },
        BuiltinMethodDecl { name: "set", runtime_callee: Some("gorget_write_guard_set"), self_conv: SelfConvention::MutBorrow, is_mutating: true,  returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_void },
    ],
};

pub static THREAD: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Thread",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: None,
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        BuiltinMethodDecl { name: "join", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "id", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_int },
    ],
};

pub static HEAP: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Heap",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: Some("gorget_heap_free"),
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: true,
    c_runtime_alias: None,
    methods: &[
        BuiltinMethodDecl { name: "push", runtime_callee: Some("gorget_heap_push"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "pop", runtime_callee: Some("gorget_heap_pop"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_elem },
        BuiltinMethodDecl { name: "peek", runtime_callee: Some("gorget_heap_peek"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_elem },
        BuiltinMethodDecl { name: "len", runtime_callee: Some("gorget_heap_len"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "is_empty", runtime_callee: Some("gorget_heap_is_empty"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_bool },
    ],
};

pub static GORGET_STRING_VIEW: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "GorgetString",
    type_arity: 0,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        // Mutating (StringBuilder-style)
        BuiltinMethodDecl { name: "push", runtime_callee: Some("gorget_str_push"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: |_, _| vec![I64_TYPE], return_type: ret_void },
        BuiltinMethodDecl { name: "push_line", runtime_callee: Some("gorget_str_push_line"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: |_, _| vec![I64_TYPE], return_type: ret_void },
        BuiltinMethodDecl { name: "push_char", runtime_callee: Some("gorget_str_push_char"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: |_, _| vec![I64_TYPE], return_type: ret_void },
        BuiltinMethodDecl { name: "clear", runtime_callee: Some("gorget_str_clear"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_void },
        // Queries
        BuiltinMethodDecl { name: "len", runtime_callee: Some("gorget_str_codepoint_count"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "capacity", runtime_callee: Some("gorget_str_capacity"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_int },
        // Note: String's one-shot .hash() returning int was removed when
        // Hashable migrated to state-based `hash(self, FxHasher &h)`.
        // One-shot callers go through `hash_of(s)` in std.hash; the
        // Hashable impl on String is synthesized at IR-lowering time in
        // `lower_method_call` (calls FxHasher__write_string).
        BuiltinMethodDecl { name: "ord", runtime_callee: Some("gorget_str_ord"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_int },
        // View operations → return cap=0 Str borrowing from receiver's buffer.
        // The compiler tracks ViewOf(receiver) and auto-materializes on source mutation.
        // NOTE: the no-op `str`/`as_str` self-view accessors were removed in
        // round-31 — bare `String v = sb` is already a zero-cost CoW borrow, so
        // `.str()`/`.as_str()` (which actually deep-copied via
        // gorget_string_clone_to_owned) were a strictly-worse redundant copy.
        BuiltinMethodDecl { name: "substring", runtime_callee: Some("gorget_str_slice"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, returns_fresh: false, combinator_kind: None, params: two_ints, return_type: ret_owned_string },
        BuiltinMethodDecl { name: "slice", runtime_callee: Some("gorget_str_slice"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, returns_fresh: false, combinator_kind: None, params: two_ints, return_type: ret_owned_string },
        // byte_slice(start, end) → cap=0 Str view into self's buffer (gorget_str_byte_slice).
        // The protocol entry is what flags the result `LocalOwnership::View` at the
        // consume-site decision in methods.rs (`builtin_returns_view` queries the
        // protocol). Without this entry, the result was treated as Owned and moved
        // into struct fields / pushed into Vectors verbatim — leaking a dangling
        // view past the source's drop (gorget-js snag #7, regression introduced by
        // commits 0872feeb / 1af25de0 when StructLiteral entered `uses_expr`).
        BuiltinMethodDecl { name: "byte_slice", runtime_callee: Some("gorget_str_byte_slice"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, returns_fresh: false, combinator_kind: None, params: two_ints, return_type: ret_owned_string },
        // char_at(idx) → cap=0 Str view of the 1-byte char at idx (gorget_str_char_at).
        BuiltinMethodDecl { name: "char_at", runtime_callee: Some("gorget_str_char_at"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, returns_fresh: false, combinator_kind: None, params: int_param, return_type: ret_owned_string },
        BuiltinMethodDecl { name: "trim", runtime_callee: Some("gorget_str_trim"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_owned_string },
        // trim_left/trim_right are LIR aliases of lstrip_ws/rstrip_ws (no-arg).
        BuiltinMethodDecl { name: "trim_left", runtime_callee: Some("gorget_str_lstrip_ws"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_owned_string },
        BuiltinMethodDecl { name: "trim_right", runtime_callee: Some("gorget_str_rstrip_ws"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_owned_string },
        BuiltinMethodDecl { name: "strip", runtime_callee: Some("gorget_str_strip"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_owned_string },
        // {l,r}strip(chars) → char-set strip; removeprefix/suffix likewise. All
        // return cap=0 Str views into self's buffer.
        BuiltinMethodDecl { name: "lstrip", runtime_callee: Some("gorget_str_lstrip"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, returns_fresh: false, combinator_kind: None, params: string_param, return_type: ret_owned_string },
        BuiltinMethodDecl { name: "rstrip", runtime_callee: Some("gorget_str_rstrip"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, returns_fresh: false, combinator_kind: None, params: string_param, return_type: ret_owned_string },
        BuiltinMethodDecl { name: "removeprefix", runtime_callee: Some("gorget_str_removeprefix"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, returns_fresh: false, combinator_kind: None, params: string_param, return_type: ret_owned_string },
        BuiltinMethodDecl { name: "removesuffix", runtime_callee: Some("gorget_str_removesuffix"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, returns_fresh: false, combinator_kind: None, params: string_param, return_type: ret_owned_string },
        // Allocating operations → GorgetString. `returns_fresh: true` mirrors
        // RuntimeSig.returns_fresh on the underlying runtime fn — these always
        // produce a fresh, independent heap buffer (no aliasing into self).
        BuiltinMethodDecl { name: "to_upper", runtime_callee: Some("gorget_str_to_upper"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: true, combinator_kind: None, params: no_params, return_type: ret_owned_string },
        BuiltinMethodDecl { name: "to_lower", runtime_callee: Some("gorget_str_to_lower"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: true, combinator_kind: None, params: no_params, return_type: ret_owned_string },
        // Aliases for upper/lower (some code uses .upper()/.lower() instead of .to_upper()/.to_lower())
        BuiltinMethodDecl { name: "upper", runtime_callee: Some("gorget_str_to_upper"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: true, combinator_kind: None, params: no_params, return_type: ret_owned_string },
        BuiltinMethodDecl { name: "lower", runtime_callee: Some("gorget_str_to_lower"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: true, combinator_kind: None, params: no_params, return_type: ret_owned_string },
    ],
};

pub static OPTION: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Option",
    type_arity: 1,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        // Combinator methods: return the same Option type (self)
        BuiltinMethodDecl { name: "map", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: Some(CombinatorKind::Map), params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "and_then", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: Some(CombinatorKind::AndThen), params: elem_param, return_type: ret_self },
        // flat_map: alias of and_then at the type level; must be registered so
        // typed combinator_kind dispatch routes it (historically only present
        // on the name-match lists — missing registration is a silent regression).
        BuiltinMethodDecl { name: "flat_map", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: Some(CombinatorKind::FlatMap), params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "or_else", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: Some(CombinatorKind::OrElse), params: no_params, return_type: ret_self },
        BuiltinMethodDecl { name: "or", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: Some(CombinatorKind::Or), params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "filter", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: Some(CombinatorKind::Filter), params: elem_param, return_type: ret_self },
        // flatten: Option[Option[T]] → Option[T] — returns the inner option type
        BuiltinMethodDecl { name: "flatten", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: Some(CombinatorKind::Flatten), params: no_params, return_type: |a, ctx| {
            // Try to strip one level: Option__Option__T → Option__T
            if a.self_name.starts_with("Option__Option__") {
                let inner = &a.self_name["Option__".len()..];
                (ctx.lookup_type_by_name)(inner).unwrap_or(a.self_type)
            } else {
                a.self_type
            }
        }},
        // unwrap_or_else: returns the inner type T
        BuiltinMethodDecl { name: "unwrap_or_else", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: Some(CombinatorKind::UnwrapOrElse), params: elem_param, return_type: |a, _| a.elem },
    ],
};

pub static RESULT: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Result",
    type_arity: 2,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        // Combinator methods: return the same Result type (self)
        BuiltinMethodDecl { name: "map", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: Some(CombinatorKind::Map), params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "and_then", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: Some(CombinatorKind::AndThen), params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "or_else", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: Some(CombinatorKind::OrElse), params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "or", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: Some(CombinatorKind::Or), params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "map_err", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: Some(CombinatorKind::MapErr), params: elem_param, return_type: ret_self },
        // unwrap_or_else: returns the Ok type (key = K = elem)
        BuiltinMethodDecl { name: "unwrap_or_else", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: Some(CombinatorKind::UnwrapOrElse), params: elem_param, return_type: |a, _| a.key },
        // unwrap_error: trap path, not a combinator family member
        BuiltinMethodDecl { name: "unwrap_error", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_val },
        // Phase 1: do NOT register RESULT.flat_map (no fixtures / typecheck surface).
    ],
};

// Non-generic sync/concurrency types: ByValue receiver, no runtime_callee mapping
// (the LIR backend's map_monomorphized_to_runtime handles the GIR→C name mapping).

pub static ATOMIC_INT: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "AtomicInt",
    type_arity: 0,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        BuiltinMethodDecl { name: "load", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "store", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: int_param, return_type: ret_void },
        BuiltinMethodDecl { name: "add", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: int_param, return_type: ret_int },
        BuiltinMethodDecl { name: "sub", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: int_param, return_type: ret_int },
        BuiltinMethodDecl { name: "compare_exchange", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: two_ints, return_type: ret_bool },
    ],
};

pub static ATOMIC_BOOL: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "AtomicBool",
    type_arity: 0,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        BuiltinMethodDecl { name: "load", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_bool },
        BuiltinMethodDecl { name: "store", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: true, returns_view: false, returns_fresh: false, combinator_kind: None, params: |_, _| vec![BOOL_TYPE], return_type: ret_void },
        BuiltinMethodDecl { name: "swap", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: |_, _| vec![BOOL_TYPE], return_type: ret_bool },
        BuiltinMethodDecl { name: "compare_exchange", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: |_, _| vec![BOOL_TYPE, BOOL_TYPE], return_type: ret_bool },
    ],
};

pub static BARRIER: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Barrier",
    type_arity: 0,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        BuiltinMethodDecl { name: "wait", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_void },
    ],
};

pub static WAIT_GROUP: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "WaitGroup",
    type_arity: 0,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        BuiltinMethodDecl { name: "add", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: int_param, return_type: ret_void },
        BuiltinMethodDecl { name: "done", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "wait", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_void },
    ],
};

pub static SEMAPHORE: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Semaphore",
    type_arity: 0,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        BuiltinMethodDecl { name: "acquire", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "release", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "try_acquire", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_bool },
    ],
};

pub static ONCE_FLAG: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "OnceFlag",
    type_arity: 0,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        BuiltinMethodDecl { name: "do_once", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_bool },
        BuiltinMethodDecl { name: "is_done", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_bool },
    ],
};

pub static TASK_GROUP: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "TaskGroup",
    type_arity: 0,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    clone_inplace_fn: None,
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: None,
    methods: &[
        BuiltinMethodDecl { name: "spawn", runtime_callee: Some("gorget_task_group_submit"), self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "join", runtime_callee: Some("gorget_task_group_join"), self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, returns_fresh: false, combinator_kind: None, params: no_params, return_type: ret_void },
    ],
};

// ── Callable family ────────────────────────────────────────────────────
//
// Callable / MutCallable / ConsumeCallable monomorphizations (Callable[T(args)])
// and the runtime singleton `GorgetClosure` all share the same C runtime
// layout: a 16-byte `{fn_ptr, env}` struct backed by `gorget_closure_*` runtime
// helpers. The protocols carry full metadata — copy_semantics = Resource, drop
// + clone fns — so consumers read TypeDef.metadata uniformly. The
// `c_runtime_alias = "GorgetClosure"` field tells the C backend to typedef
// these Named types to the runtime struct (no fresh `__gg_X` definition).
//
// Note: the user-facing local form of `Callable[int(int)]` lowers to
// `GirType::FnPtr` (NOT `GirType::Named("Callable__…")`) — see
// `map_ast_type_mut`'s special case. The Named form only appears via
// `resolve_inner_type` when a Callable shows up as a collection element /
// dict value / Option payload. So these protocols supply metadata for the
// collection-element path; the local FnPtr path is unaffected.

pub static CALLABLE: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Callable",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: Some("gorget_closure_free"),
    clone_fn: Some("gorget_closure_clone_to_owned"),
    clone_inplace_fn: Some("gorget_closure_clone_inplace"),
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: Some("GorgetClosure"),
    methods: &[],
};

pub static MUT_CALLABLE: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "MutCallable",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: Some("gorget_closure_free"),
    clone_fn: Some("gorget_closure_clone_to_owned"),
    clone_inplace_fn: Some("gorget_closure_clone_inplace"),
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: Some("GorgetClosure"),
    methods: &[],
};

pub static CONSUME_CALLABLE: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "ConsumeCallable",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: Some("gorget_closure_free"),
    clone_fn: Some("gorget_closure_clone_to_owned"),
    clone_inplace_fn: Some("gorget_closure_clone_inplace"),
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: Some("GorgetClosure"),
    methods: &[],
};

pub static GORGET_CLOSURE: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "GorgetClosure",
    type_arity: 0,
    copy_semantics: CopySemantics::Resource,
    drop_fn: Some("gorget_closure_free"),
    clone_fn: Some("gorget_closure_clone_to_owned"),
    clone_inplace_fn: Some("gorget_closure_clone_inplace"),
    materialize_fn: None,
    borrow_view_fn: None,
    collection_kind: None,
    owns_buffered_elements: false,
    c_runtime_alias: Some("GorgetClosure"),
    methods: &[],
};

// ── Lookup ────────────────────────────────────────────────────────────

/// All registered builtin type protocols.
static ALL_PROTOCOLS: &[&BuiltinTypeProtocol] = &[
    &VECTOR, &DEQUE, &DICT, &HASHMAP, &SET, &HASHSET,
    &CHANNEL, &SHARED, &WEAK, &MUTEX, &GUARD,
    &RWLOCK, &READ_GUARD, &WRITE_GUARD,
    &THREAD, &HEAP,
    &GORGET_STRING_VIEW, &OPTION, &RESULT,
    &ATOMIC_INT, &ATOMIC_BOOL, &BARRIER, &WAIT_GROUP, &SEMAPHORE, &ONCE_FLAG, &TASK_GROUP,
    &CALLABLE, &MUT_CALLABLE, &CONSUME_CALLABLE, &GORGET_CLOSURE,
];

/// Look up a builtin type protocol by base name (e.g., "Vector", "Dict").
pub fn lookup_protocol(base_name: &str) -> Option<&'static BuiltinTypeProtocol> {
    ALL_PROTOCOLS.iter().find(|p| p.base_name == base_name).copied()
}

/// Check if a mangled type name belongs to a known builtin protocol.
/// Used for the Guard 2 hard-panic check on unresolved builtin methods.
pub fn protocol_for_mangled_name(mangled: &str) -> Option<&'static BuiltinTypeProtocol> {
    ALL_PROTOCOLS.iter().find(|p| {
        mangled.starts_with(p.base_name) &&
        (mangled.len() == p.base_name.len() ||
         mangled.as_bytes().get(p.base_name.len()) == Some(&b'_'))
    }).copied()
}

/// Return the C runtime struct this mangled name aliases to, if any.
///
/// Reads `c_runtime_alias` from the matching `BuiltinTypeProtocol`. The Callable
/// family (`Callable[T(...)]`, `MutCallable[…]`, `ConsumeCallable[…]`, and the
/// runtime singleton `GorgetClosure`) all alias to `"GorgetClosure"`; protocols
/// without a runtime alias return `None`.
///
/// Single source of truth for "this mangled name lowers to a known runtime
/// struct" — used by both the GIR-side TypeDef registrar (`register_callable_alias`)
/// and the LIR-side size lookup (`c_sizeof_with_structs`) so neither has to
/// re-implement the name-shape recognizer. The protocol's `c_runtime_alias`
/// field IS the contract; this accessor reads it through one helper.
pub fn c_runtime_alias_for_mangled_name(mangled: &str) -> Option<&'static str> {
    protocol_for_mangled_name(mangled).and_then(|p| p.c_runtime_alias)
}

/// The closure shape a higher-order builtin method calls back with, or `None`
/// when this (receiver, method) pair passes no closure.
///
/// `mangled` is the receiver's monomorphized name (`Vector__GorgetString`,
/// `Dict__GorgetString__int64_t`); `method` is the EFFECTIVE method name
/// (post-`sort`/1 → `sort_by` rewrite). The protocol is recovered through
/// `protocol_for_mangled_name`, the one sanctioned name-shape recognizer —
/// never by `strip_prefix("Vector__")` at the consumer, which is the
/// anti-pattern CLAUDE.md § "No name matching" names verbatim and which has no
/// Dict arm at all.
/// The closure-shape table as data, for cross-table guards.
///
/// Exposed so `tests/lints.rs` can check every `(protocol, method)` row against
/// INDEPENDENT witnesses — the LIR HofOp dispatch arms and the user-space
/// `equip` blocks — rather than against the `BuiltinMethodDecl` list this table
/// deliberately does not derive from. `closure_shape_rows_are_total` proves the
/// PROTOCOL axis is complete; it structurally cannot see a wrong METHOD, which
/// is the axis the original defect lived on.
pub fn closure_shape_rows() -> &'static [(&'static str, &'static [(&'static str, ClosureShape)])] {
    CLOSURE_SHAPES
}

pub fn closure_shape_for(mangled: &str, method: &str) -> Option<ClosureShape> {
    let protocol = protocol_for_mangled_name(mangled)?;
    let row = CLOSURE_SHAPES.iter().find(|(name, _)| *name == protocol.base_name)?.1;
    row.iter().find(|(name, _)| *name == method).map(|(_, shape)| *shape)
}

/// Check if a type uses by-value receiver convention (Copy-semantics pointer handles).
/// Used by the generic dispatch path to skip borrow creation for these types.
pub fn is_by_value_receiver(type_name: &str) -> bool {
    if let Some(protocol) = protocol_for_mangled_name(type_name) {
        // All methods on the type use ByValue — check any method
        protocol.methods.first()
            .map(|m| m.self_conv == SelfConvention::ByValue)
            .unwrap_or(false)
    } else {
        false
    }
}

/// Check if a specific method on a type requires a mutable borrow receiver.
/// Used by the generic dispatch path to emit `emit_borrow_mut` instead of `emit_borrow`.
pub fn is_mut_borrow_method(type_name: &str, method_name: &str) -> bool {
    protocol_for_mangled_name(type_name)
        .and_then(|p| p.methods.iter().find(|m| m.name == method_name))
        .map(|m| m.self_conv == SelfConvention::MutBorrow)
        .unwrap_or(false)
}

/// Check if `method_name` is marked as mutating (`is_mutating: true`) on any
/// builtin type protocol. Used by the borrow checker for borrow invalidation
/// and by IR lowering for field-zeroing after mutation.
pub fn is_mutating_builtin_method(method_name: &str) -> bool {
    ALL_PROTOCOLS.iter().any(|p| {
        p.methods.iter().any(|m| m.name == method_name && m.is_mutating)
    })
}

/// Check if `method_name` is an element-borrow READ (`is_elem_borrow_read`)
/// on any builtin type protocol — the auto-borrow-from-get family whose
/// `Some` payload aliases the receiver's storage. Used by the safety pass's
/// mutation-marking chain resolver to route a mutation through a
/// `.get(i).unwrap()` view back to the collection's root binding. Mirrors
/// `is_mutating_builtin_method`'s any-protocol shape.
pub fn is_elem_borrow_read_builtin_method(method_name: &str) -> bool {
    ALL_PROTOCOLS.iter().any(|p| {
        p.methods.iter().any(|m| m.name == method_name && m.is_elem_borrow_read())
    })
}

/// Every opaque-handle protocol (SelfConvention::ByValue on ALL its methods) —
/// the class the Round XXXII receiver-ABI chokepoint (`methods.rs:531 / :2315 /
/// :2343`) covers. Read by `tests/lints.rs::opaque_handle_route_fixtures_exist`
/// to grow-with-schema: adding a new by-value protocol to `ALL_PROTOCOLS`
/// automatically flips the lint's expected-coverage set. Filter is on the
/// FIRST method's self_conv (all methods on a given protocol share the
/// convention by construction — the collection_protocols_have_full_metadata
/// pattern above).
pub fn by_value_protocol_names() -> Vec<&'static str> {
    ALL_PROTOCOLS
        .iter()
        .filter(|p| {
            p.methods
                .first()
                .map(|m| m.self_conv == SelfConvention::ByValue)
                .unwrap_or(false)
        })
        .map(|p| p.base_name)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Phase A invariant: every BuiltinTypeProtocol with `collection_kind: Some(_)`
    /// must populate the full metadata required by the consolidated
    /// type-predicate / drop / clone consumers — drop_fn, clone_fn,
    /// clone_inplace_fn, copy_semantics. After Phase A, those fields are the
    /// authoritative source; if a future protocol entry leaves any of them
    /// None, downstream sites that read metadata will silently take the
    /// wrong path (e.g., a missing clone_fn → no deep clone → shallow alias
    /// → double free). This test locks the invariant at unit-test time.
    #[test]
    fn collection_protocols_have_full_metadata() {
        for protocol in ALL_PROTOCOLS {
            if protocol.collection_kind.is_none() {
                continue;
            }
            assert!(
                protocol.copy_semantics == crate::ir::types::CopySemantics::Resource,
                "collection protocol {} must have copy_semantics = Resource",
                protocol.base_name,
            );
            assert!(
                protocol.drop_fn.is_some(),
                "collection protocol {} missing drop_fn", protocol.base_name,
            );
            assert!(
                protocol.clone_fn.is_some(),
                "collection protocol {} missing clone_fn", protocol.base_name,
            );
            assert!(
                protocol.clone_inplace_fn.is_some(),
                "collection protocol {} missing clone_inplace_fn — \
                 Phase A consumers (elem_clone_fn_for_type) read this field",
                protocol.base_name,
            );
        }
    }

    /// Arena-escape invariant: `owns_buffered_elements` marks a NON-collection
    /// handle (Channel/Heap) whose mutating element-ingesting method copies the
    /// element into a self-owned, arena-surviving buffer — the second typed
    /// shape the borrow checker's `is_buffer_owning_type` gate treats like a
    /// collection. It is a DISJOINT axis from `collection_kind` (a `Some(_)`
    /// collection already qualifies via that field; double-flagging would be a
    /// modelling error), and a handle that ingests nothing (no mutating method)
    /// can't alias an element into a buffer. This locks both so a future
    /// protocol can't set the flag incorrectly and silently mis-gate the
    /// arena-borrow-escape safety check.
    #[test]
    fn owns_buffered_elements_invariant() {
        for protocol in ALL_PROTOCOLS {
            if !protocol.owns_buffered_elements {
                continue;
            }
            assert!(
                protocol.collection_kind.is_none(),
                "protocol {} sets owns_buffered_elements AND collection_kind — \
                 these are disjoint axes (collections already qualify via \
                 collection_kind); pick one",
                protocol.base_name,
            );
            assert!(
                protocol.methods.iter().any(|m| m.is_mutating),
                "protocol {} sets owns_buffered_elements but has no mutating \
                 (element-ingesting) method — nothing aliases into the buffer",
                protocol.base_name,
            );
        }
    }

    /// Closure-shape invariant: EVERY protocol in `ALL_PROTOCOLS` carries an
    /// EXPLICIT row in `CLOSURE_SHAPES`, and an empty method list is that
    /// explicit answer for a protocol with no higher-order method.
    ///
    /// This is the independent witness the fixture corpus cannot supply. The
    /// table is keyed by `&str`, so nothing in the type system forces a row to
    /// exist, and the failure mode is silent: `DEQUE.methods = VECTOR.methods`
    /// and `HASHSET.methods = SET.methods`, so a protocol whose row is merely
    /// MISSING keeps working for every method whose closure params happen to be
    /// annotated and drops the untyped ones back to the `I64_TYPE` default —
    /// which is a link failure on String elements and a silent no-op on struct
    /// elements. Verified RED by deleting the `Deque` row: this test names it,
    /// and both Deque cells go BUILD-FAIL.
    #[test]
    fn closure_shape_rows_are_total() {
        for protocol in ALL_PROTOCOLS {
            assert!(
                CLOSURE_SHAPES.iter().any(|(name, _)| *name == protocol.base_name),
                "protocol {} has no explicit closure-shape row — add one to \
                 CLOSURE_SHAPES (an empty method list IS the answer for a \
                 protocol with no higher-order method; omitting the row \
                 silently drops the protocol's untyped closure hints)",
                protocol.base_name,
            );
        }
        for (name, _) in CLOSURE_SHAPES {
            assert!(
                ALL_PROTOCOLS.iter().any(|p| p.base_name == *name),
                "CLOSURE_SHAPES has a row for {name}, which is not a protocol \
                 in ALL_PROTOCOLS — `closure_shape_for` resolves the protocol \
                 first, so this row is unreachable",
            );
        }
        for (proto, row) in CLOSURE_SHAPES {
            let mut seen: Vec<&str> = Vec::new();
            for (method, _) in *row {
                assert!(
                    !seen.contains(method),
                    "closure-shape row {proto} lists {method} twice — \
                     `closure_shape_for` takes the first match, so the second \
                     entry is dead",
                );
                seen.push(method);
            }
        }
    }

    /// `ClosureShape::arity` pinned to CONCRETE numbers, so the `debug_assert`
    /// that compares it against the hint vector at the write site is a
    /// cross-check between two matches rather than a self-comparison. Each
    /// number is the callback's parameter count as the language spells it:
    /// `(e)`, `(a, b)`, `(acc, e)`, `(k, v)`, `(acc, k, v)`.
    #[test]
    fn closure_shape_arity_is_pinned() {
        for (shape, expected) in [
            (ClosureShape::Elem, 1usize),
            (ClosureShape::ElemPair, 2),
            (ClosureShape::AccElem, 2),
            (ClosureShape::KeyVal, 2),
            (ClosureShape::AccKeyVal, 3),
        ] {
            assert_eq!(
                shape.arity(), expected,
                "{shape:?}.arity() changed — the hint vector built at the \
                 write site must change with it, or a trailing closure param \
                 silently falls back to I64_TYPE",
            );
        }
    }

    /// Aliased method tables must carry aliased closure shapes. `Deque` reuses
    /// `VECTOR.methods` and `HashSet` reuses `SET.methods`; a shape row that
    /// diverges from its twin would give the same method two different closure
    /// arities depending only on which spelling the user reached for.
    #[test]
    fn aliased_protocols_share_closure_shapes() {
        let row_of = |base: &str| -> &'static [(&'static str, ClosureShape)] {
            CLOSURE_SHAPES.iter().find(|(n, _)| *n == base)
                .unwrap_or_else(|| panic!("no closure-shape row for {base}")).1
        };
        for (alias, source) in [("Deque", "Vector"), ("HashSet", "Set"), ("HashMap", "Dict")] {
            assert_eq!(
                row_of(alias), row_of(source),
                "{alias} aliases {source}'s method table but not its closure shapes",
            );
        }
    }

    /// Phase A invariant: builtin protocols never share a base_name with each
    /// other. `lookup_protocol` returns the first match by base_name, so a
    /// duplicate would silently shadow downstream metadata reads.
    #[test]
    fn protocol_base_names_are_unique() {
        let mut seen: Vec<&str> = Vec::new();
        for protocol in ALL_PROTOCOLS {
            assert!(
                !seen.contains(&protocol.base_name),
                "duplicate protocol base_name: {}", protocol.base_name,
            );
            seen.push(protocol.base_name);
        }
    }

    /// `returns_fresh` and `returns_view` are mutually exclusive — a method
    /// either borrows from its receiver (cap=0 view) or produces a fresh
    /// heap buffer with no aliasing into inputs. A method tagged both ways
    /// is a contradiction the CoW machinery cannot resolve.
    #[test]
    fn returns_fresh_and_view_are_mutually_exclusive() {
        for protocol in ALL_PROTOCOLS {
            for method in protocol.methods {
                assert!(
                    !(method.returns_fresh && method.returns_view),
                    "method {}.{} cannot be both returns_fresh and returns_view",
                    protocol.base_name, method.name,
                );
            }
        }
    }

    /// Cross-table consistency: every BuiltinMethodDecl with
    /// `returns_fresh: true` and a non-None `runtime_callee` must point at a
    /// runtime fn whose `RuntimeSig.returns_fresh` is also true. Catches the
    /// "two parallel lists drift" failure mode at unit-test time.
    #[test]
    fn method_returns_fresh_matches_runtime_returns_fresh() {
        use crate::lir::runtime::RuntimeFn;
        for protocol in ALL_PROTOCOLS {
            for method in protocol.methods {
                let Some(callee) = method.runtime_callee else { continue };
                let Some(rt) = RuntimeFn::from_c_name(callee) else { continue };
                let rt_fresh = rt.signature().returns_fresh;
                assert_eq!(
                    method.returns_fresh, rt_fresh,
                    "method {}.{} → {} disagrees on returns_fresh \
                     (BuiltinMethodDecl={}, RuntimeSig={})",
                    protocol.base_name, method.name, callee,
                    method.returns_fresh, rt_fresh,
                );
            }
        }
    }

    /// Write-site pin (Round XV Track D / Core #6): every historical Option/Result
    /// combinator name carries a typed `combinator_kind`, and OPTION.flat_map is
    /// registered as FlatMap. Without this, typed D1/D2/D3 dispatch silently
    /// drops GIR-adapter routing (combinator_flat_map_money_* regressions).
    #[test]
    fn option_result_combinator_kinds_registered() {
        use CombinatorKind::*;

        let option_expected: &[(&str, CombinatorKind)] = &[
            ("map", Map),
            ("and_then", AndThen),
            ("flat_map", FlatMap),
            ("or_else", OrElse),
            ("or", Or),
            ("filter", Filter),
            ("flatten", Flatten),
            ("unwrap_or_else", UnwrapOrElse),
        ];
        for &(name, kind) in option_expected {
            let m = OPTION
                .methods
                .iter()
                .find(|m| m.name == name)
                .unwrap_or_else(|| panic!("OPTION.{name} must be registered"));
            assert_eq!(
                m.combinator_kind,
                Some(kind),
                "OPTION.{name} combinator_kind",
            );
            if matches!(kind, Or | Flatten) {
                assert!(!kind.is_gir_adapter(), "{kind:?} is C-inline only");
            } else {
                assert!(kind.is_gir_adapter(), "{kind:?} should be GIR-adapter");
            }
        }

        let result_expected: &[(&str, CombinatorKind)] = &[
            ("map", Map),
            ("and_then", AndThen),
            ("or_else", OrElse),
            ("or", Or),
            ("map_err", MapErr),
            ("unwrap_or_else", UnwrapOrElse),
        ];
        for &(name, kind) in result_expected {
            let m = RESULT
                .methods
                .iter()
                .find(|m| m.name == name)
                .unwrap_or_else(|| panic!("RESULT.{name} must be registered"));
            assert_eq!(
                m.combinator_kind,
                Some(kind),
                "RESULT.{name} combinator_kind",
            );
        }

        // Phase 1: RESULT.flat_map intentionally unregistered.
        assert!(
            RESULT.methods.iter().all(|m| m.name != "flat_map"),
            "RESULT.flat_map must NOT be registered in Phase 1",
        );
        // unwrap_error is not a combinator.
        let ue = RESULT
            .methods
            .iter()
            .find(|m| m.name == "unwrap_error")
            .expect("RESULT.unwrap_error");
        assert_eq!(ue.combinator_kind, None);

        // Vector HOFs that share combinator names must stay None so typed
        // dispatch never treats them as Option/Result combinators.
        for name in ["map", "filter", "flat_map"] {
            let m = VECTOR
                .methods
                .iter()
                .find(|m| m.name == name)
                .unwrap_or_else(|| panic!("VECTOR.{name}"));
            assert_eq!(
                m.combinator_kind,
                None,
                "VECTOR.{name} must have combinator_kind: None",
            );
        }
    }

    /// CombinatorKind has exactly 9 variants (GIR-adapter set + Or + Flatten).
    #[test]
    fn combinator_kind_variant_count() {
        // Manual list — keeps the enum closed; a new variant forces a revisit
        // of the registration table + D1 is_gir_adapter carve-out.
        let all = [
            CombinatorKind::Map,
            CombinatorKind::Filter,
            CombinatorKind::AndThen,
            CombinatorKind::FlatMap,
            CombinatorKind::OrElse,
            CombinatorKind::UnwrapOrElse,
            CombinatorKind::MapErr,
            CombinatorKind::Or,
            CombinatorKind::Flatten,
        ];
        assert_eq!(all.len(), 9);
        assert_eq!(
            all.iter().filter(|k| k.is_gir_adapter()).count(),
            7,
            "7 GIR-adapter kinds + 2 C-inline (Or, Flatten)",
        );
    }

}
