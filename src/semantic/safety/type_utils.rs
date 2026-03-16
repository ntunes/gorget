use rustc_hash::{FxHashMap, FxHashSet};

use crate::parser::ast::*;
use crate::span::Spanned;

use crate::semantic::ids::{DefId, ScopeId, TypeId};
use crate::semantic::scope::ScopeTable;
use crate::semantic::types::{ResolvedType, TypeTable};

// ─── Copy Type Detection ───────────────────────────────────

/// Returns true if a type is Copy (trivially copyable, no `!` needed).
///
/// `str` is Copy — an immutable view (`const char*`) that never owns memory.
/// `String` (PrimitiveType::StringType) is non-Copy — it owns a heap buffer
/// (GorgetString struct) and must be moved with `!`.
pub(super) fn is_copy_type(type_id: TypeId, types: &TypeTable, scopes: &ScopeTable) -> bool {
    match types.get(type_id) {
        ResolvedType::Primitive(prim) => {
            use PrimitiveType::*;
            matches!(
                prim,
                Int | Int8
                    | Int16
                    | Int32
                    | Int64
                    | Uint
                    | Uint8
                    | Uint16
                    | Uint32
                    | Uint64
                    | Float
                    | Float32
                    | Float64
                    | Bool
                    | Str
                    | CStr
            )
        }
        ResolvedType::Void | ResolvedType::Never | ResolvedType::Error => true,
        ResolvedType::Tuple(elems) => {
            let elems = elems.clone();
            elems.iter().all(|e| is_copy_type(*e, types, scopes))
        }
        ResolvedType::Generic(def_id, _) => {
            // Channel[T], Shared[T], Weak[T], and Mutex[T] are Copy — they're opaque pointers.
            // Guard[T] and TaskGroup are NOT Copy — they hold exclusive resources.
            matches!(scopes.get_def(*def_id).name.as_str(), "Channel" | "Shared" | "Weak" | "Mutex")
        }
        ResolvedType::Defined(def_id) => {
            // Arena/TrackingAllocator/PoolAllocator/TlsfAllocator/FixedBufferAllocator/FallbackAllocator are Copy — they're pointers
            matches!(scopes.get_def(*def_id).name.as_str(), "Arena" | "TrackingAllocator" | "PoolAllocator" | "TlsfAllocator" | "FixedBufferAllocator" | "FallbackAllocator")
        }
        // Everything else is non-Copy (String, structs, enums, etc.)
        _ => false,
    }
}

// ─── Reference-Type Struct Detection ──────────────────────

/// Check if an AST Type refers to a reference type: `str`, `Slice`, or a named
/// type whose DefId is in `ref_structs`.
pub(super) fn is_ast_type_ref(ty: &Type, scopes: &ScopeTable, ref_structs: &FxHashSet<DefId>) -> bool {
    match ty {
        Type::Primitive(PrimitiveType::Str) => true,
        Type::Slice { .. } => true,
        Type::Named { name, .. } => {
            // Search from module scope (scope 0) since struct defs are module-level.
            // scopes.current may be at a nested scope after prior passes.
            if let Some(def_id) = scopes.lookup_from_scope(ScopeId(0), &name.node) {
                ref_structs.contains(&def_id)
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Recursively collect all `Spanned<Item>`s, descending into `Item::Module` wrappers
/// so that imported-module contents are visited by every borrow-checker pass.
pub(super) fn all_spanned_items(items: &[Spanned<Item>]) -> Vec<&Spanned<Item>> {
    let mut result = Vec::new();
    collect_spanned_items(items, &mut result);
    result
}

pub(super) fn collect_spanned_items<'a>(items: &'a [Spanned<Item>], out: &mut Vec<&'a Spanned<Item>>) {
    for item in items {
        if let Item::Module { items: inner, .. } = &item.node {
            collect_spanned_items(inner, out);
        } else {
            out.push(item);
        }
    }
}

/// Scan the module's struct definitions and compute which structs contain
/// reference-type fields (directly or transitively). Returns their DefIds.
pub(super) fn compute_ref_type_structs(module: &Module, scopes: &ScopeTable) -> FxHashSet<DefId> {
    // Collect all struct defs with their DefId and fields
    let mut struct_infos: Vec<(DefId, &[Spanned<FieldDef>])> = Vec::new();
    // Collect all enum defs with their DefId and variant field types
    let mut enum_infos: Vec<(DefId, Vec<&Type>)> = Vec::new();
    for item in all_spanned_items(&module.items) {
        match &item.node {
            Item::Struct(s) => {
                if let Some(def_id) = scopes.lookup_from_scope(ScopeId(0), &s.name.node) {
                    struct_infos.push((def_id, &s.fields));
                }
            }
            Item::Enum(e) => {
                if let Some(def_id) = scopes.lookup_from_scope(ScopeId(0), &e.name.node) {
                    let field_types: Vec<&Type> = e.variants.iter()
                        .flat_map(|v| match &v.node.fields {
                            VariantFields::Tuple(types) => types.iter().map(|t| &t.node).collect::<Vec<_>>(),
                            VariantFields::Unit => vec![],
                        })
                        .collect();
                    enum_infos.push((def_id, field_types));
                }
            }
            _ => {}
        }
    }

    // Fixpoint iteration: keep adding structs/enums until stable
    let mut ref_structs = FxHashSet::default();
    loop {
        let prev_len = ref_structs.len();
        for (def_id, fields) in &struct_infos {
            if ref_structs.contains(def_id) {
                continue;
            }
            for field in *fields {
                if is_ast_type_ref(&field.node.type_.node, scopes, &ref_structs) {
                    ref_structs.insert(*def_id);
                    break;
                }
            }
        }
        for (def_id, field_types) in &enum_infos {
            if ref_structs.contains(def_id) {
                continue;
            }
            for field_type in field_types {
                if is_ast_type_ref(field_type, scopes, &ref_structs) {
                    ref_structs.insert(*def_id);
                    break;
                }
            }
        }
        if ref_structs.len() == prev_len {
            break;
        }
    }
    ref_structs
}

/// Build a per-struct map of which field indices are reference types.
/// Used to select which struct literal args contribute to the borrow origin.
pub(super) fn compute_struct_field_ref_flags(
    module: &Module,
    scopes: &ScopeTable,
    ref_type_structs: &FxHashSet<DefId>,
) -> FxHashMap<DefId, Vec<bool>> {
    let mut result = FxHashMap::default();
    for item in all_spanned_items(&module.items) {
        if let Item::Struct(s) = &item.node {
            if let Some(def_id) = scopes.lookup_from_scope(ScopeId(0), &s.name.node) {
                if ref_type_structs.contains(&def_id) {
                    let flags: Vec<bool> = s.fields.iter()
                        .map(|f| is_ast_type_ref(&f.node.type_.node, scopes, ref_type_structs))
                        .collect();
                    result.insert(def_id, flags);
                }
            }
        }
    }
    result
}
