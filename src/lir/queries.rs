//! LIR-module queries shared across backends.
//!
//! These are property-style functions (no I/O, no mutation) that ask
//! "does the LIR module have this shape?" Both the C and LLVM backends
//! consume them — they live here, in the LIR module, rather than inside
//! one backend, so neither backend has to reach into the other.
//!
//! Each function takes `&LirModule` and queries declared types,
//! function signatures, or struct shapes — never module state owned by a
//! specific backend.

use crate::lir::{LirFunction, LirModule, LirType, StructId};
use std::collections::HashSet;

/// Whether the (post-mapping) extern name is a runtime constructor for
/// a Dict / Set / HashMap / HashSet that takes element-size argument(s)
/// and whose result needs `hash_fn` / `eq_fn` post-construction wiring
/// when the element/key type is a user `@derive(Hashable, Equatable)`
/// struct. Five canonical names — kept in one place so the LLVM backend
/// (bridge-decl scan + post-ctor wiring) and any future bridge-aware
/// pass agree on the set.
pub fn is_user_keyable_collection_ctor(extern_name: &str) -> bool {
    matches!(extern_name,
        "gorget_dict_new"
        | "gorget_map_new"
        | "gorget_set_new"
        | "gorget_set_with_capacity"
        | "gorget_ordered_set_new")
}

/// Whether `func` is a spawn-wrapper synthesized by IR lowering or the
/// shared-async transform. Spawn wrappers are called from the C-emitted
/// runtime (`__spawn_run_*`) which uses gcc's AArch64 PCS rule B.4 (>16-byte
/// composites passed via hidden pointer in the next NGRN register). The
/// LLVM backend defaults to its own ABI for `(%Struct %arg)` (split across
/// up to 8 i64 regs) — these don't match. The LLVM backend therefore
/// declares wrapper params as plain `ptr` instead of by-value `%Struct`.
///
/// Three naming patterns flow through:
///
/// - `__spawn_method_wrap_<sym>`  (`Type.method` spawns; `lowering/exprs/spawn.rs:131`)
/// - `__spawn_wrap_<struct>`      (bare-closure / async-fn spawns; same file:375)
/// - `__shared_token_<sym>`       (shared-token transform; `transforms/shared_async.rs`)
pub fn is_spawn_wrapper(func: &LirFunction) -> bool {
    let n = &func.name;
    n.starts_with("__spawn_method_wrap_")
        || n.starts_with("__spawn_wrap_")
        || n.starts_with("__shared_token_")
}

/// Walks the struct's field graph and returns true if any (transitively-
/// reached) field type is a runtime resource — `GorgetString`,
/// `GorgetArray`, `GorgetMap`, `GorgetSet`, or `GorgetClosure`. Used to
/// pick the right ABI for closure-arg passing (resource aggregates
/// travel by pointer to match `__adapt_*` shims; non-resource go
/// by-value at the call site).
///
/// Visits each struct at most once; `Ptr` fields don't recurse (a bare
/// pointer doesn't itself "contain" the pointee — ownership lives with
/// whoever holds the value).
pub fn struct_contains_resource(sid: StructId, module: &LirModule) -> bool {
    fn walk(sid: StructId, module: &LirModule, visited: &mut HashSet<u32>) -> bool {
        if !visited.insert(sid.0) { return false; }
        let sdef = match module.structs.get(sid.0 as usize) {
            Some(s) => s,
            None => return false,
        };
        if matches!(sdef.name.as_str(),
            "GorgetString" | "GorgetArray" | "GorgetMap" | "GorgetSet" | "GorgetClosure") {
            return true;
        }
        for (_, fty) in &sdef.fields {
            if let LirType::Struct(inner_sid) = fty {
                if walk(*inner_sid, module, visited) { return true; }
            }
        }
        false
    }
    let mut visited = HashSet::new();
    walk(sid, module, &mut visited)
}

/// Whether `type_c` is a user struct/enum used as a Dict / Set key that
/// has both `T__hash` (Hashable) and `T__eq` (Equatable) methods in the
/// LIR. Primitive types (int64_t, double, etc.), `Str` / `GorgetString`,
/// and monomorphized collection types return false — they have their
/// own runtime paths (byte-FNV for POD scalars, `_str` constructors for
/// strings).
pub fn is_user_hashable_key(type_c: &str, module: &LirModule) -> bool {
    if matches!(type_c,
        "int64_t" | "int32_t" | "int16_t" | "int8_t"
        | "uint64_t" | "uint32_t" | "uint16_t" | "uint8_t"
        | "double" | "float" | "bool" | "_Bool"
        | "Str" | "GorgetString"
    ) {
        return false;
    }
    // Skip monomorphized wrapper types (Option__T, Result__T__E, Vector__T, …).
    if type_c.contains("__") {
        return false;
    }
    hashable_key_fn_names(type_c, module).is_some()
}

/// Returns the concrete LIR function names for `{type}.hash(&h)` /
/// `{type}.eq(other)` as emitted by the IR lowerer. Trait-impl methods
/// use the `Trait_for_Type` prefix; direct inherent methods use just
/// `Type__method`. Both forms are checked so the bridge targets the
/// correct symbol.
pub fn hashable_key_fn_names(type_c: &str, module: &LirModule) -> Option<(String, String)> {
    let direct_hash = format!("{type_c}__hash");
    let direct_eq = format!("{type_c}__eq");
    let trait_hash = format!("Hashable_for_{type_c}__hash");
    let trait_eq = format!("Equatable_for_{type_c}__eq");
    let hash_name = if module.functions.iter().any(|f| f.name == direct_hash) {
        direct_hash
    } else if module.functions.iter().any(|f| f.name == trait_hash) {
        trait_hash
    } else {
        return None;
    };
    let eq_name = if module.functions.iter().any(|f| f.name == direct_eq) {
        direct_eq
    } else if module.functions.iter().any(|f| f.name == trait_eq) {
        trait_eq
    } else {
        return None;
    };
    Some((hash_name, eq_name))
}
