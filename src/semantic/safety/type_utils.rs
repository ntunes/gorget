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
            let def = scopes.get_def(*def_id);
            // D4 (D12): Copy and custom Drop are mutually exclusive (Rust
            // precedent). A drop-tainted type is NEVER Copy — copying it would
            // duplicate the drop side effect — even when all its fields are
            // scalars.
            if def.is_drop_tainted {
                return false;
            }
            // Enums: Copy if all variant fields are Copy
            if def.kind == crate::semantic::scope::DefKind::Enum {
                if let Some(ref variant_types) = def.variant_field_types {
                    return variant_types.iter().all(|vt| {
                        vt.iter().all(|&field_tid| is_copy_type(field_tid, types, scopes))
                    });
                }
                return true; // No variant field info → assume Copy (ordinal enums)
            }
            // Structs: Copy if all fields are Copy
            if def.kind == crate::semantic::scope::DefKind::Struct {
                if let Some(ref field_tids) = def.field_types {
                    return field_tids.iter().all(|&tid| is_copy_type(tid, types, scopes));
                }
                // No field info → conservative: check name whitelist
                return matches!(def.name.as_str(),
                    "Arena" | "TrackingAllocator" | "PoolAllocator"
                    | "TlsfAllocator" | "FixedBufferAllocator" | "FallbackAllocator"
                );
            }
            // Arena/TrackingAllocator/PoolAllocator/TlsfAllocator/FixedBufferAllocator/FallbackAllocator are Copy — they're pointers
            matches!(def.name.as_str(), "Arena" | "TrackingAllocator" | "PoolAllocator" | "TlsfAllocator" | "FixedBufferAllocator" | "FallbackAllocator")
        }
        // Type & is Copy — it's a pointer/reference
        ResolvedType::Ref(_) => true,
        // Type ! is non-Copy — it owns the value
        ResolvedType::Owned(_) => false,
        // Everything else is non-Copy (String, structs, enums, etc.)
        _ => false,
    }
}

/// Returns true for the single-owner carve-out types that ALWAYS require an
/// explicit `!` (move) or `.clone()` at an ownership boundary — they are NOT
/// CoW-eligible, so there is no implicit clone-if-live path. Closures hold env
/// captures whose semantics are load-bearing on the move; `Owned[T]`/`Box[T]`
/// are by-value-of-heap; `Task`/`TaskGroup`/`Guard` are single-owner handles.
///
/// This is the single source of truth for the carve-out set, shared by the
/// bare-assign check (`check_stmt`) AND the constructor / struct-literal /
/// enum-variant init checks (`check_expr`). For these types the IR lowering has
/// no clone boundary — passing one bare (no `!`) into a constructor would be
/// accepted by the checker and then panic the lowering as an untracked consumed
/// source — so the checker must require the explicit move here.
pub(super) fn needs_explicit_move(type_id: TypeId, types: &TypeTable, scopes: &ScopeTable) -> bool {
    // D4 (D12): drop-tainted types join the single-owner set automatically —
    // ONE PRINCIPLED RULE (side-effectful drop ⇒ single-owner) plus the
    // by-design members below. Implicit clones are only available to types
    // whose transitive drop is side-effect-free.
    if crate::semantic::is_drop_tainted_type(type_id, types, scopes) {
        return true;
    }
    match types.get(type_id) {
        ResolvedType::Function { .. }
        | ResolvedType::CallableTrait(_)
        | ResolvedType::MutCallableTrait(_)
        | ResolvedType::ConsumeCallableTrait(_)
        | ResolvedType::BoxedCallable { .. }
        | ResolvedType::Owned(_) => true,
        ResolvedType::Generic(def_id, _) => {
            matches!(
                scopes.get_def(*def_id).name.as_str(),
                "Box" | "Task" | "TaskGroup" | "Guard"
            )
        }
        _ => false,
    }
}

// ─── Reference-Type Struct Detection ──────────────────────

/// Check if an AST Type refers to a reference type: `str`, `Slice`, sigil
/// `T &`, user-written `Ref[T]` / `MutRef[T]` borrow-field types, or a named
/// type whose DefId is in `ref_structs` (struct that transitively holds a
/// reference field).
pub(super) fn is_ast_type_ref(ty: &Type, scopes: &ScopeTable, ref_structs: &FxHashSet<DefId>) -> bool {
    match ty {
        Type::Ref(_) => true,
        Type::Slice { .. } => true,
        Type::Named { name, generic_args } => {
            // User-written borrow-field types — `Ref[T]` / `MutRef[T]` —
            // are reference types regardless of T. Greppable, no DefId needed.
            if generic_args.len() == 1 && (name.node == "Ref" || name.node == "MutRef") {
                return true;
            }
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

/// Like `is_ast_type_ref` but returns true ONLY for `MutRef[T]` field types
/// (and the sigil `T &` form). Distinguishes exclusive-borrow fields from
/// shared `Ref[T]` ones for aliasing checks.
pub(super) fn is_ast_type_mut_ref(ty: &Type) -> bool {
    match ty {
        Type::Ref(_) => true, // sigil `T &` is mutable
        Type::Named { name, generic_args } => {
            generic_args.len() == 1 && name.node == "MutRef"
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

/// Per-struct vector: true for fields whose type is `MutRef[T]` (or the
/// sigil `T &`). Mirrors `compute_struct_field_ref_flags` but distinguishes
/// exclusive-borrow fields. Only structs already in `ref_type_structs`
/// appear in the map.
pub(super) fn compute_struct_field_mut_ref_flags(
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
                        .map(|f| is_ast_type_mut_ref(&f.node.type_.node))
                        .collect();
                    result.insert(def_id, flags);
                }
            }
        }
    }
    result
}

/// Per-struct field NAMES in declaration order, keyed by the struct's DefId.
/// The index of a name here lines up with `DefInfo.field_types` (both walk
/// `s.fields` in source order), so `field_types[names.position(name)]` recovers
/// a named field's TypeId. Built for ALL structs (unlike the ref-flag maps,
/// which cover only `ref_type_structs`) because the arena-escape lvalue-type
/// resolver needs the field type of any outer struct target. One-source-of-
/// truth typed metadata; no name-matching.
pub(super) fn compute_struct_field_names(
    module: &Module,
    scopes: &ScopeTable,
) -> FxHashMap<DefId, Vec<String>> {
    let mut result = FxHashMap::default();
    for item in all_spanned_items(&module.items) {
        if let Item::Struct(s) = &item.node {
            if let Some(def_id) = scopes.lookup_from_scope(ScopeId(0), &s.name.node) {
                let names: Vec<String> =
                    s.fields.iter().map(|f| f.node.name.node.clone()).collect();
                result.insert(def_id, names);
            }
        }
    }
    result
}
