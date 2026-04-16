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
    /// Check if a TypeId is a resource type (owns heap allocations).
    pub is_resource: &'a dyn Fn(TypeId) -> bool,
    /// Ensure an Option[T] type is registered, returning its TypeId.
    pub ensure_option: &'a dyn Fn(&str, TypeId) -> TypeId,
    /// Mangled name fragment for the elem type (e.g., "int64_t", "GorgetString", "uint8_t").
    /// Stored here because BuiltinTypeArgs.elem is a TypeId, but Option wrapping
    /// needs the mangled name to construct "Option__Ref_int64_t".
    pub elem_name: String,
    /// Mangled name fragment for the val type (for Dict[K,V]).
    pub val_name: String,
}

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
    /// Build parameter GIR TypeIds given the type args.
    pub params: fn(&BuiltinTypeArgs) -> Vec<TypeId>,
    /// Build return GIR TypeId given the type args and a lookup context.
    pub return_type: fn(&BuiltinTypeArgs, &LookupCtx) -> TypeId,
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
    /// Collection kind for metadata-based dispatch. None = not a collection.
    pub collection_kind: Option<CollectionKind>,
    /// All methods on this type.
    pub methods: &'static [BuiltinMethodDecl],
}

// ── Helper constructors for param/return closures ─────────────────────

/// No params (besides self).
fn no_params(_: &BuiltinTypeArgs) -> Vec<TypeId> { vec![] }

/// Single int param.
fn int_param(_: &BuiltinTypeArgs) -> Vec<TypeId> { vec![I64_TYPE] }

/// Single elem-type param.
fn elem_param(a: &BuiltinTypeArgs) -> Vec<TypeId> { vec![a.elem] }

/// Two int params.
fn two_ints(_: &BuiltinTypeArgs) -> Vec<TypeId> { vec![I64_TYPE, I64_TYPE] }

/// Key param (for dict).
fn key_param(a: &BuiltinTypeArgs) -> Vec<TypeId> { vec![a.key] }

/// Key + value params (for dict.put).
fn key_val_params(a: &BuiltinTypeArgs) -> Vec<TypeId> { vec![a.key, a.val] }

/// Key + value + default value params (for dict.get_or).
fn key_val_default(a: &BuiltinTypeArgs) -> Vec<TypeId> { vec![a.key, a.val] }

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

/// Returns Option[Ref_elem] for resource types, Option[elem] for primitives.
/// Used by borrowing read methods (get/first/last) that return a reference
/// to resource-type elements but a value copy for primitive elements.
fn ret_option_ref_or_val_elem(a: &BuiltinTypeArgs, ctx: &LookupCtx) -> TypeId {
    if (ctx.is_resource)(a.elem) {
        let option_name = format!("Option__Ref_{}", ctx.elem_name);
        (ctx.ensure_option)(&option_name, a.elem)
    } else {
        let option_name = format!("Option__{}", ctx.elem_name);
        (ctx.ensure_option)(&option_name, a.elem)
    }
}

/// Returns Option[val] (for dict.get).
fn ret_option_val(a: &BuiltinTypeArgs, ctx: &LookupCtx) -> TypeId {
    let option_name = format!("Option__{}", ctx.val_name);
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
    collection_kind: Some(CollectionKind::Array),
    methods: &[
        // Mutating
        BuiltinMethodDecl { name: "push", runtime_callee: Some("gorget_array_push"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "clear", runtime_callee: Some("gorget_array_clear"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "reverse", runtime_callee: Some("gorget_array_reverse"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "insert", runtime_callee: Some("gorget_array_insert"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: |a| vec![I64_TYPE, a.elem], return_type: ret_void },
        BuiltinMethodDecl { name: "extend", runtime_callee: Some("gorget_array_extend"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "reserve", runtime_callee: Some("gorget_array_reserve"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: int_param, return_type: ret_void },
        BuiltinMethodDecl { name: "set", runtime_callee: Some("gorget_array_set"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: |a| vec![I64_TYPE, a.elem], return_type: ret_void },
        // Borrowing reads
        BuiltinMethodDecl { name: "get", runtime_callee: Some("gorget_array_safe_get"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: int_param, return_type: ret_option_ref_or_val_elem },
        BuiltinMethodDecl { name: "first", runtime_callee: Some("gorget_array_first"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_option_ref_or_val_elem },
        BuiltinMethodDecl { name: "last", runtime_callee: Some("gorget_array_last"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_option_ref_or_val_elem },
        // Consuming reads
        BuiltinMethodDecl { name: "pop", runtime_callee: Some("gorget_array_safe_pop"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: no_params, return_type: ret_option_elem },
        BuiltinMethodDecl { name: "remove", runtime_callee: Some("gorget_array_remove_opt"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: int_param, return_type: ret_option_elem },
        // Queries
        BuiltinMethodDecl { name: "len", runtime_callee: Some("gorget_array_len"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "capacity", runtime_callee: Some("gorget_array_capacity"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "is_empty", runtime_callee: Some("gorget_array_is_empty"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_bool },
        BuiltinMethodDecl { name: "contains", runtime_callee: Some("gorget_array_contains"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "index_of", runtime_callee: Some("gorget_array_index_of"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: |_, ctx| (ctx.lookup_type_by_name)("Option__int64_t").unwrap_or(I64_TYPE) },
        BuiltinMethodDecl { name: "binary_search", runtime_callee: Some("gorget_array_binary_search"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_int },
        // Clone / copy
        BuiltinMethodDecl { name: "clone", runtime_callee: Some("gorget_array_clone"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_self },
        BuiltinMethodDecl { name: "sorted", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_self },
        BuiltinMethodDecl { name: "reversed", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_self },
        BuiltinMethodDecl { name: "unique", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_self },
        BuiltinMethodDecl { name: "slice", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: two_ints, return_type: ret_self },
        // Higher-order (inline codegen — keep monomorphized names)
        BuiltinMethodDecl { name: "sort", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "filter", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "map", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "flat_map", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "enumerate", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_self },
        BuiltinMethodDecl { name: "fold", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: |a| vec![I64_TYPE, a.elem], return_type: ret_int },
        BuiltinMethodDecl { name: "reduce", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_int },
        BuiltinMethodDecl { name: "any", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_int },
        BuiltinMethodDecl { name: "all", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_int },
        BuiltinMethodDecl { name: "each", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "for_each", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "find", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_option_elem },
        BuiltinMethodDecl { name: "find_index", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_int },
        BuiltinMethodDecl { name: "count", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_int },
        BuiltinMethodDecl { name: "zip", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
    ],
};

pub static DEQUE: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Deque",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: Some("gorget_array_free"),
    clone_fn: Some("gorget_array_clone"),
    collection_kind: Some(CollectionKind::Array),
    methods: VECTOR.methods, // Same interface as Vector
};

pub static DICT: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Dict",
    type_arity: 2,
    copy_semantics: CopySemantics::Resource,
    drop_fn: Some("gorget_map_free"),
    clone_fn: Some("gorget_map_clone"),
    collection_kind: Some(CollectionKind::OrderedMap),
    methods: &[
        BuiltinMethodDecl { name: "put", runtime_callee: Some("gorget_map_put"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: key_val_params, return_type: ret_void },
        BuiltinMethodDecl { name: "set", runtime_callee: Some("gorget_map_put"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: key_val_params, return_type: ret_void },
        BuiltinMethodDecl { name: "get", runtime_callee: Some("gorget_map_get"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: key_param, return_type: ret_option_val },
        BuiltinMethodDecl { name: "get_or", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: key_val_default, return_type: ret_val },
        BuiltinMethodDecl { name: "get_or_put", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: key_val_default, return_type: ret_val },
        BuiltinMethodDecl { name: "remove", runtime_callee: Some("gorget_map_remove"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: key_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "contains", runtime_callee: Some("gorget_map_contains"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: key_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "has", runtime_callee: Some("gorget_map_contains"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: key_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "len", runtime_callee: Some("gorget_map_len"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "is_empty", runtime_callee: Some("gorget_map_is_empty"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_bool },
        BuiltinMethodDecl { name: "clear", runtime_callee: Some("gorget_map_clear"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "keys", runtime_callee: Some("gorget_map_keys"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: |_a, ctx| {
            // keys() → Vector[K]
            let vec_name = format!("Vector__{}", ctx.elem_name);
            (ctx.lookup_type_by_name)(&vec_name)
                .or_else(|| (ctx.lookup_type_by_name)("GorgetArray"))
                .unwrap_or(UNIT_TYPE)
        }},
        BuiltinMethodDecl { name: "values", runtime_callee: Some("gorget_map_values"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: |_a, ctx| {
            // values() → Vector[V]
            let vec_name = format!("Vector__{}", ctx.val_name);
            (ctx.lookup_type_by_name)(&vec_name)
                .or_else(|| (ctx.lookup_type_by_name)("GorgetArray"))
                .unwrap_or(UNIT_TYPE)
        }},
        BuiltinMethodDecl { name: "items", runtime_callee: Some("gorget_map_items"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: |_a, ctx| {
            // items() → Vector[Tuple[K, V]] — construct from elem_name (K) and val_name (V)
            let tuple_name = format!("Tuple__{}__{}", ctx.elem_name, ctx.val_name);
            let vec_name = format!("Vector__{tuple_name}");
            (ctx.lookup_type_by_name)(&vec_name)
                .or_else(|| (ctx.lookup_type_by_name)("GorgetArray"))
                .unwrap_or(UNIT_TYPE)
        }},
        BuiltinMethodDecl { name: "clone", runtime_callee: Some("gorget_map_clone"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_self },
        // Higher-order (inline codegen)
        BuiltinMethodDecl { name: "filter", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: key_val_params, return_type: ret_self },
        BuiltinMethodDecl { name: "map", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: key_val_params, return_type: ret_self },
        BuiltinMethodDecl { name: "fold", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: |a| vec![I64_TYPE, a.key, a.val], return_type: ret_int },
        BuiltinMethodDecl { name: "each", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: key_val_params, return_type: ret_void },
        BuiltinMethodDecl { name: "any", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: key_val_params, return_type: ret_int },
        BuiltinMethodDecl { name: "all", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: key_val_params, return_type: ret_int },
        BuiltinMethodDecl { name: "update", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: key_val_params, return_type: ret_void },
    ],
};

pub static HASHMAP: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "HashMap",
    type_arity: 2,
    copy_semantics: CopySemantics::Resource,
    drop_fn: Some("gorget_map_free"),
    clone_fn: Some("gorget_map_clone"),
    collection_kind: Some(CollectionKind::Map),
    methods: DICT.methods, // Same interface as Dict
};

pub static SET: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Set",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: Some("gorget_set_free"),
    clone_fn: Some("gorget_set_clone"),
    collection_kind: Some(CollectionKind::OrderedSet),
    methods: &[
        BuiltinMethodDecl { name: "add", runtime_callee: Some("gorget_set_add"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "insert", runtime_callee: Some("gorget_set_add"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "remove", runtime_callee: Some("gorget_set_remove"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: elem_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "contains", runtime_callee: Some("gorget_set_contains"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "has", runtime_callee: Some("gorget_set_contains"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "len", runtime_callee: Some("gorget_set_len"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "is_empty", runtime_callee: Some("gorget_set_is_empty"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_bool },
        BuiltinMethodDecl { name: "clear", runtime_callee: Some("gorget_set_clear"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "clone", runtime_callee: Some("gorget_set_clone"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_self },
        // Set algebra (inline codegen)
        BuiltinMethodDecl { name: "union", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "intersection", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "difference", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "symmetric_difference", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "is_subset", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "is_superset", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "is_disjoint", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_bool },
        // Higher-order
        BuiltinMethodDecl { name: "filter", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "map", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "fold", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: |a| vec![I64_TYPE, a.elem], return_type: ret_int },
        BuiltinMethodDecl { name: "each", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "any", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_int },
        BuiltinMethodDecl { name: "all", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_int },
    ],
};

pub static HASHSET: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "HashSet",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: Some("gorget_set_free"),
    clone_fn: Some("gorget_set_clone"),
    collection_kind: Some(CollectionKind::Set),
    methods: SET.methods, // Same interface as Set
};

pub static CHANNEL: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Channel",
    type_arity: 1,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None, // Typed drop wrapper emitted by c_lir
    clone_fn: None,
    collection_kind: None,
    methods: &[
        BuiltinMethodDecl { name: "send", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "recv", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_elem },
        BuiltinMethodDecl { name: "close", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "poll_recv", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_bool },
        BuiltinMethodDecl { name: "recv_timeout", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, params: int_param, return_type: ret_option_elem },
        BuiltinMethodDecl { name: "len", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "capacity", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "is_closed", runtime_callee: None, self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_bool },
    ],
};

pub static SHARED: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Shared",
    type_arity: 1,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    collection_kind: None,
    methods: &[
        BuiltinMethodDecl { name: "clone", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_self },
        BuiltinMethodDecl { name: "get", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_elem },
        BuiltinMethodDecl { name: "strong_count", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "downgrade", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: |a, ctx| {
            // Returns Weak[T] — look up by mangled name
            let elem_name = type_id_to_c_name(a.elem);
            let weak_name = format!("Weak__{elem_name}");
            (ctx.lookup_type_by_name)(&weak_name).unwrap_or(a.self_type)
        }},
        // Shared[Vector[T]] convenience methods
        BuiltinMethodDecl { name: "at", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: int_param, return_type: ret_elem },
        BuiltinMethodDecl { name: "set_at", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: true, returns_view: false, params: |a| vec![I64_TYPE, a.elem], return_type: ret_void },
        BuiltinMethodDecl { name: "slen", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_int },
    ],
};

pub static WEAK: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Weak",
    type_arity: 1,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    collection_kind: None,
    methods: &[
        BuiltinMethodDecl { name: "clone", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_self },
        BuiltinMethodDecl { name: "upgrade", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: |a, ctx| {
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
    collection_kind: None,
    methods: &[
        BuiltinMethodDecl { name: "lock", runtime_callee: Some("gorget_mutex_lock"), self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: |a, ctx| {
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
    collection_kind: None,
    methods: &[
        BuiltinMethodDecl { name: "get", runtime_callee: Some("gorget_guard_get"), self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_elem },
        BuiltinMethodDecl { name: "set", runtime_callee: Some("gorget_guard_set"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: elem_param, return_type: ret_void },
    ],
};

pub static RWLOCK: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "RWLock",
    type_arity: 1,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    collection_kind: None,
    methods: &[
        BuiltinMethodDecl { name: "read", runtime_callee: Some("gorget_rwlock_read"), self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: |a, ctx| {
            let elem_name = type_id_to_c_name(a.elem);
            (ctx.lookup_type_by_name)(&format!("ReadGuard__{elem_name}")).unwrap_or(a.self_type)
        }},
        BuiltinMethodDecl { name: "write", runtime_callee: Some("gorget_rwlock_write"), self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: |a, ctx| {
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
    collection_kind: None,
    methods: &[
        BuiltinMethodDecl { name: "get", runtime_callee: Some("gorget_read_guard_get"), self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_elem },
    ],
};

pub static WRITE_GUARD: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "WriteGuard",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: None,
    clone_fn: None,
    collection_kind: None,
    methods: &[
        BuiltinMethodDecl { name: "get", runtime_callee: Some("gorget_write_guard_get"), self_conv: SelfConvention::MutBorrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_elem },
        BuiltinMethodDecl { name: "set", runtime_callee: Some("gorget_write_guard_set"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: elem_param, return_type: ret_void },
    ],
};

pub static THREAD: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Thread",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: None,
    clone_fn: None,
    collection_kind: None,
    methods: &[
        BuiltinMethodDecl { name: "join", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "id", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_int },
    ],
};

pub static HEAP: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Heap",
    type_arity: 1,
    copy_semantics: CopySemantics::Resource,
    drop_fn: Some("gorget_heap_free"),
    clone_fn: None,
    collection_kind: None,
    methods: &[
        BuiltinMethodDecl { name: "push", runtime_callee: Some("gorget_heap_push"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "pop", runtime_callee: Some("gorget_heap_pop"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: no_params, return_type: ret_elem },
        BuiltinMethodDecl { name: "peek", runtime_callee: Some("gorget_heap_peek"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_elem },
        BuiltinMethodDecl { name: "len", runtime_callee: Some("gorget_heap_len"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "is_empty", runtime_callee: Some("gorget_heap_is_empty"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_bool },
    ],
};

pub static GORGET_STRING_VIEW: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "GorgetString",
    type_arity: 0,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    collection_kind: None,
    methods: &[
        // Mutating (StringBuilder-style)
        BuiltinMethodDecl { name: "push", runtime_callee: Some("gorget_str_push"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: |_| vec![I64_TYPE], return_type: ret_void },
        BuiltinMethodDecl { name: "push_line", runtime_callee: Some("gorget_str_push_line"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: |_| vec![I64_TYPE], return_type: ret_void },
        BuiltinMethodDecl { name: "push_char", runtime_callee: Some("gorget_str_push_char"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: |_| vec![I64_TYPE], return_type: ret_void },
        BuiltinMethodDecl { name: "clear", runtime_callee: Some("gorget_str_clear"), self_conv: SelfConvention::MutBorrow, is_mutating: true, returns_view: false, params: no_params, return_type: ret_void },
        // Queries
        BuiltinMethodDecl { name: "len", runtime_callee: Some("gorget_str_codepoint_count"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "capacity", runtime_callee: Some("gorget_str_capacity"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "hash", runtime_callee: Some("gorget_str_hash"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "ord", runtime_callee: Some("gorget_str_ord"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_int },
        // View operations → return cap=0 Str borrowing from receiver's buffer.
        // The compiler tracks ViewOf(receiver) and auto-materializes on source mutation.
        BuiltinMethodDecl { name: "str", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, params: no_params, return_type: ret_owned_string },
        BuiltinMethodDecl { name: "as_str", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, params: no_params, return_type: ret_owned_string },
        BuiltinMethodDecl { name: "substring", runtime_callee: Some("gorget_str_slice"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, params: two_ints, return_type: ret_owned_string },
        BuiltinMethodDecl { name: "slice", runtime_callee: Some("gorget_str_slice"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, params: two_ints, return_type: ret_owned_string },
        BuiltinMethodDecl { name: "trim", runtime_callee: Some("gorget_str_trim"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, params: no_params, return_type: ret_owned_string },
        BuiltinMethodDecl { name: "strip", runtime_callee: Some("gorget_str_strip"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: true, params: no_params, return_type: ret_owned_string },
        // Allocating operations → GorgetString
        BuiltinMethodDecl { name: "to_upper", runtime_callee: Some("gorget_str_to_upper"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_owned_string },
        BuiltinMethodDecl { name: "to_lower", runtime_callee: Some("gorget_str_to_lower"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_owned_string },
        // Aliases for upper/lower (some code uses .upper()/.lower() instead of .to_upper()/.to_lower())
        BuiltinMethodDecl { name: "upper", runtime_callee: Some("gorget_str_to_upper"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_owned_string },
        BuiltinMethodDecl { name: "lower", runtime_callee: Some("gorget_str_to_lower"), self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_owned_string },
    ],
};

pub static OPTION: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Option",
    type_arity: 1,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    collection_kind: None,
    methods: &[
        // Combinator methods: return the same Option type (self)
        BuiltinMethodDecl { name: "map", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "and_then", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "or_else", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_self },
        BuiltinMethodDecl { name: "or", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "filter", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        // flatten: Option[Option[T]] → Option[T] — returns the inner option type
        BuiltinMethodDecl { name: "flatten", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: |a, ctx| {
            // Try to strip one level: Option__Option__T → Option__T
            if a.self_name.starts_with("Option__Option__") {
                let inner = &a.self_name["Option__".len()..];
                (ctx.lookup_type_by_name)(inner).unwrap_or(a.self_type)
            } else {
                a.self_type
            }
        }},
        // unwrap_or_else: returns the inner type T
        BuiltinMethodDecl { name: "unwrap_or_else", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: |a, _| a.elem },
    ],
};

pub static RESULT: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Result",
    type_arity: 2,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    collection_kind: None,
    methods: &[
        // Combinator methods: return the same Result type (self)
        BuiltinMethodDecl { name: "map", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "and_then", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "or_else", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "or", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        BuiltinMethodDecl { name: "map_err", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_self },
        // unwrap_or_else: returns the Ok type (key = K = elem)
        BuiltinMethodDecl { name: "unwrap_or_else", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: elem_param, return_type: |a, _| a.key },
        // unwrap_error: returns the Err type (val = V)
        BuiltinMethodDecl { name: "unwrap_error", runtime_callee: None, self_conv: SelfConvention::Borrow, is_mutating: false, returns_view: false, params: no_params, return_type: ret_val },
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
    collection_kind: None,
    methods: &[
        BuiltinMethodDecl { name: "load", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_int },
        BuiltinMethodDecl { name: "store", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: true, returns_view: false, params: int_param, return_type: ret_void },
        BuiltinMethodDecl { name: "add", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: int_param, return_type: ret_int },
        BuiltinMethodDecl { name: "sub", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: int_param, return_type: ret_int },
        BuiltinMethodDecl { name: "compare_exchange", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: two_ints, return_type: ret_bool },
    ],
};

pub static ATOMIC_BOOL: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "AtomicBool",
    type_arity: 0,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    collection_kind: None,
    methods: &[
        BuiltinMethodDecl { name: "load", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_bool },
        BuiltinMethodDecl { name: "store", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: true, returns_view: false, params: |_| vec![BOOL_TYPE], return_type: ret_void },
        BuiltinMethodDecl { name: "swap", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: |_| vec![BOOL_TYPE], return_type: ret_bool },
        BuiltinMethodDecl { name: "compare_exchange", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: |_| vec![BOOL_TYPE, BOOL_TYPE], return_type: ret_bool },
    ],
};

pub static BARRIER: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Barrier",
    type_arity: 0,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    collection_kind: None,
    methods: &[
        BuiltinMethodDecl { name: "wait", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_void },
    ],
};

pub static WAIT_GROUP: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "WaitGroup",
    type_arity: 0,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    collection_kind: None,
    methods: &[
        BuiltinMethodDecl { name: "add", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: int_param, return_type: ret_void },
        BuiltinMethodDecl { name: "done", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "wait", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_void },
    ],
};

pub static SEMAPHORE: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "Semaphore",
    type_arity: 0,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    collection_kind: None,
    methods: &[
        BuiltinMethodDecl { name: "acquire", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "release", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_void },
        BuiltinMethodDecl { name: "try_acquire", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_bool },
    ],
};

pub static ONCE_FLAG: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "OnceFlag",
    type_arity: 0,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    collection_kind: None,
    methods: &[
        BuiltinMethodDecl { name: "do_once", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_bool },
        BuiltinMethodDecl { name: "is_done", runtime_callee: None, self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_bool },
    ],
};

pub static TASK_GROUP: BuiltinTypeProtocol = BuiltinTypeProtocol {
    base_name: "TaskGroup",
    type_arity: 0,
    copy_semantics: CopySemantics::Trivial,
    drop_fn: None,
    clone_fn: None,
    collection_kind: None,
    methods: &[
        BuiltinMethodDecl { name: "spawn", runtime_callee: Some("gorget_task_group_submit"), self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: elem_param, return_type: ret_void },
        BuiltinMethodDecl { name: "join", runtime_callee: Some("gorget_task_group_join"), self_conv: SelfConvention::ByValue, is_mutating: false, returns_view: false, params: no_params, return_type: ret_void },
    ],
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
