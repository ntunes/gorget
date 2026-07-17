use rustc_hash::{FxHashMap, FxHashSet};

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use super::errors::{SemanticError, SemanticErrorKind};
use super::ids::{DefId, TypeId};
use super::resolve::{ResolutionMap, validate_default_param_ordering};
use super::scope::{DefKind, ScopeTable};
use super::types::{self, TypeTable};

/// Function signature for trait methods.
#[derive(Debug, Clone)]
pub struct FunctionSig {
    pub params: Vec<TypeId>,
    pub return_type: TypeId,
    pub has_self: bool,
    pub self_ownership: Option<Ownership>,
}

/// AST-level signature shape preserved for late-bound method-generic inference.
///
/// `FunctionSig` stores resolved TypeIds with method-level generics erased to
/// `error_id` (because their param defs aren't in scope at trait-collection /
/// impl-collection time — see ec694db6). Inference at the call site needs the
/// AST-level structure intact: which named generic appears in which param-
/// type slot, which params are callables whose return type carries another
/// generic, etc.
///
/// Populated for every trait method and every equip method whose
/// `generic_params` is non-empty. Skipped (None) for non-generic methods to
/// keep the registry small.
#[derive(Debug, Clone)]
pub struct MethodSigShape {
    /// Method-level generic param names in declaration order.
    pub generic_params: Vec<String>,
    /// AST param types in declaration order, excluding `self`.
    pub param_types: Vec<Type>,
    /// AST return type.
    pub return_type: Type,
}

/// AST-level sig info for a trait default method. Populated when the
/// method has a default body (Block/Expression) so call-site resolution
/// can substitute `Self` and trait generic params against the concrete
/// receiver — `FunctionSig` stores already-resolved TypeIds that erase
/// the Self slot to `error_id` and the trait's `T` to the trait's
/// generic-param placeholder, neither of which round-trips.
#[derive(Debug, Clone)]
pub struct DefaultMethodSig {
    /// Method-level generic param names (e.g., `[U, F]`); empty for
    /// non-generic defaults.
    pub method_generic_params: Vec<String>,
    /// AST param types in declaration order, excluding `self`.
    pub param_types: Vec<Type>,
    /// AST return type.
    pub return_type: Type,
    /// AST `throws E` clause (the error type `E`), if the default method is
    /// `throws`. Kept as AST — like `return_type`/`param_types` — so it can be
    /// resolved (and `Self`/trait-`T`-substituted) at the call site. D23: this
    /// is the ONLY place a trait-DEFAULT method's throws is reachable by the
    /// call-site totality gate, because `resolve_method`/`resolve_method_by_name`
    /// hand back the *trait* def_id for a default (not a `function_info` key).
    pub throws_ast: Option<Type>,
    /// Per-non-self-param ownership sigils.
    pub param_ownerships: Vec<Ownership>,
    pub has_self: bool,
    pub self_ownership: Option<Ownership>,
}

/// Information about a trait definition.
#[derive(Debug, Clone)]
pub struct TraitInfo {
    pub def_id: DefId,
    pub name: String,
    pub methods: FxHashMap<String, FunctionSig>,
    pub has_default_body: FxHashMap<String, bool>,
    pub extends: Vec<DefId>,
    /// AST-level signature shapes for method-level-generic methods. Used by
    /// the call-site method-generic inference in typecheck. Populated only
    /// for methods whose `generic_params` is non-empty.
    pub method_shapes: FxHashMap<String, MethodSigShape>,
    /// Trait's own generic parameter names (e.g., `["T"]` for
    /// `trait Iterator[T]`). Used when substituting trait Ts into a
    /// default sig at call time.
    pub trait_generic_params: Vec<String>,
    /// AST sigs for default-bodied methods. Used by call-site
    /// `Self`/trait-T substitution when resolve_method falls through to
    /// the trait default. Populated only for methods with a Block or
    /// Expression body.
    pub default_method_sigs: FxHashMap<String, DefaultMethodSig>,
}

/// Information about an equip block.
#[derive(Debug, Clone)]
pub struct EquipInfo {
    pub self_type: TypeId,
    pub self_type_name: String,
    pub trait_: Option<DefId>,
    pub trait_name: Option<String>,
    pub methods: FxHashMap<String, (DefId, FunctionSig)>,
    pub span: Span,
    /// Generic args from the trait type, e.g. [Type::Primitive(Int)] for Iterator[int].
    pub trait_generic_args: Vec<Type>,
    /// Field name for `via` delegation (auto-forward unimplemented methods through this field).
    pub via_field: Option<String>,
    /// AST-level signature shapes for method-level-generic methods declared
    /// inside this equip block. Used by the call-site method-generic
    /// inference in typecheck. Populated only for methods whose
    /// `generic_params` is non-empty.
    pub method_shapes: FxHashMap<String, MethodSigShape>,
    /// AST-level self type (e.g., `VectorIter[T]` for
    /// `equip [T] VectorIter[T] with Iterator[T]`). Used together with
    /// `impl_generic_params` to bind the impl's local generics to the
    /// receiver's concrete type args at a call site.
    pub self_type_ast: Type,
    /// Names of the impl's local generic params (e.g., `["T"]` for the
    /// example above). Empty for non-generic impls.
    pub impl_generic_params: Vec<String>,
}

/// Registry of all traits and implementations.
pub struct TraitRegistry {
    pub traits: FxHashMap<DefId, TraitInfo>,
    pub impls: Vec<EquipInfo>,
    /// type -> indices into impls for inherent impls
    pub inherent_impls: FxHashMap<TypeId, Vec<usize>>,
    /// type -> indices into impls for trait impls (any trait, any args).
    /// Used by `resolve_method`/`resolve_method_shape` to skip the
    /// per-call linear scan over every impl in the module — they now
    /// visit only the impls whose `self_type` matches the receiver.
    /// Source of truth remains `impls`; this is a pure lookup index.
    pub trait_impls_by_type: FxHashMap<TypeId, Vec<usize>>,
    /// `self_type_name` -> indices into impls. Parallel to
    /// `trait_impls_by_type`, but keyed by the impl's *name* so the
    /// name-based fallback paths (cross-module equip blocks where TypeIds
    /// don't match, and `meta if implements(T, Trait)`) skip the per-call
    /// linear scan over every impl. Different key, same shape.
    ///
    /// Layering note: this is name-keyed because the underlying data
    /// (`self_type_name: String`) is already name-keyed at the source —
    /// we are not introducing a new name-matching axis, only indexing an
    /// existing one.
    pub impls_by_name: FxHashMap<String, Vec<usize>>,
    /// (trait DefId, type TypeId, trait type args) -> index into impls
    /// The Vec<TypeId> holds the resolved trait generic arguments (empty for non-generic traits).
    /// This allows multiple impls of the same parameterized trait (e.g. `From[int]` and `From[str]`).
    pub trait_impls: FxHashMap<(DefId, TypeId, Vec<TypeId>), usize>,
}

impl TraitRegistry {
    fn new() -> Self {
        Self {
            traits: FxHashMap::default(),
            impls: Vec::new(),
            inherent_impls: FxHashMap::default(),
            trait_impls_by_type: FxHashMap::default(),
            impls_by_name: FxHashMap::default(),
            trait_impls: FxHashMap::default(),
        }
    }

    /// Look up a method on a type: check inherent impls first, then trait impls.
    pub fn resolve_method(
        &self,
        type_id: TypeId,
        method: &str,
    ) -> Option<(&DefId, &FunctionSig)> {
        // Check inherent impls first
        if let Some(impl_indices) = self.inherent_impls.get(&type_id) {
            for &idx in impl_indices {
                if let Some((def_id, sig)) = self.impls[idx].methods.get(method) {
                    return Some((def_id, sig));
                }
            }
        }

        // Trait impls + default fallback: single pass over the impl
        // indices for this type. An override (impl-supplied method) wins
        // immediately; otherwise remember the first impl whose trait has
        // a default body for `method` and return that on fallthrough.
        if let Some(impl_indices) = self.trait_impls_by_type.get(&type_id) {
            let mut default_hit: Option<&TraitInfo> = None;
            for &idx in impl_indices {
                let impl_info = &self.impls[idx];
                if let Some((def_id, sig)) = impl_info.methods.get(method) {
                    return Some((def_id, sig));
                }
                if default_hit.is_none() {
                    if let Some(trait_def_id) = impl_info.trait_ {
                        default_hit = self.find_default_method(trait_def_id, method);
                    }
                }
            }
            if let Some(trait_info) = default_hit {
                if let Some(sig) = trait_info.methods.get(method) {
                    return Some((&trait_info.def_id, sig));
                }
            }
        }

        None
    }

    /// Walk a trait + its `extends` parents for a default method body (D23 hole fix).
    /// Mirrors `collect_all_required_methods_inner`'s parent walk; cycle-guarded.
    fn find_default_method<'a>(
        &'a self,
        trait_def_id: DefId,
        method: &str,
    ) -> Option<&'a TraitInfo> {
        let mut visited = FxHashSet::default();
        let mut stack = vec![trait_def_id];
        while let Some(id) = stack.pop() {
            if !visited.insert(id) {
                continue;
            }
            let Some(info) = self.traits.get(&id) else {
                continue;
            };
            let has_default = info
                .has_default_body
                .get(method)
                .copied()
                .unwrap_or(false);
            if has_default && info.methods.contains_key(method) {
                return Some(info);
            }
            for &parent in &info.extends {
                stack.push(parent);
            }
        }
        None
    }

    /// Look up a method's AST signature shape on a type, mirroring
    /// `resolve_method`. Returns `Some(&MethodSigShape)` only for
    /// method-level-generic methods; non-generic methods return `None`.
    /// Used by typecheck's call-site method-generic inference.
    pub fn resolve_method_shape(
        &self,
        type_id: TypeId,
        method: &str,
    ) -> Option<&MethodSigShape> {
        // Check inherent impls first
        if let Some(impl_indices) = self.inherent_impls.get(&type_id) {
            for &idx in impl_indices {
                if let Some(shape) = self.impls[idx].method_shapes.get(method) {
                    return Some(shape);
                }
            }
        }
        // Trait impls (overrides) + default-method fallback via the
        // self-type index — same shape as `resolve_method`.
        if let Some(impl_indices) = self.trait_impls_by_type.get(&type_id) {
            for &idx in impl_indices {
                if let Some(shape) = self.impls[idx].method_shapes.get(method) {
                    return Some(shape);
                }
            }
            for &idx in impl_indices {
                let impl_info = &self.impls[idx];
                let Some(trait_def_id) = impl_info.trait_ else { continue };
                if let Some(trait_info) = self.find_default_method(trait_def_id, method) {
                    if let Some(shape) = trait_info.method_shapes.get(method) {
                        return Some(shape);
                    }
                }
            }
        }
        None
    }

    /// Same as `resolve_method_shape`, but looks up by type name. Mirrors
    /// `resolve_method_by_name`.
    pub fn resolve_method_shape_by_name(
        &self,
        type_name: &str,
        method: &str,
    ) -> Option<&MethodSigShape> {
        for impl_info in &self.impls {
            if impl_info.self_type_name == type_name {
                if let Some(shape) = impl_info.method_shapes.get(method) {
                    return Some(shape);
                }
            }
        }
        for impl_info in &self.impls {
            if impl_info.self_type_name != type_name { continue; }
            let trait_def_id = match impl_info.trait_ { Some(id) => id, None => continue };
            if let Some(trait_info) = self.find_default_method(trait_def_id, method) {
                if let Some(shape) = trait_info.method_shapes.get(method) {
                    return Some(shape);
                }
            }
        }
        None
    }

    /// Fallback: look up a method by type name (string) when TypeId-based lookup fails.
    /// Used for cross-module equip blocks where TypeIds don't match.
    pub fn resolve_method_by_name(
        &self,
        type_name: &str,
        method: &str,
    ) -> Option<(&DefId, &FunctionSig)> {
        for impl_info in &self.impls {
            if impl_info.self_type_name == type_name {
                if let Some((def_id, sig)) = impl_info.methods.get(method) {
                    return Some((def_id, sig));
                }
            }
        }
        // Same default-method fallback as `resolve_method` (incl. extends walk).
        for impl_info in &self.impls {
            if impl_info.self_type_name != type_name { continue; }
            let trait_def_id = match impl_info.trait_ {
                Some(id) => id,
                None => continue,
            };
            if let Some(trait_info) = self.find_default_method(trait_def_id, method) {
                if let Some(sig) = trait_info.methods.get(method) {
                    return Some((&trait_info.def_id, sig));
                }
            }
        }
        None
    }

    /// Check if a type (by name) has any impl registered.
    pub fn has_any_impl_by_name(&self, type_name: &str) -> bool {
        self.impls_by_name
            .get(type_name)
            .is_some_and(|v| !v.is_empty())
    }

    /// Check if a type has ONLY inherent impls (no trait impls, no via delegation).
    /// Types with trait impls may have default or via-forwarded methods that aren't
    /// in the equip's methods map, so we can't reliably detect missing methods.
    pub fn has_inherent_only_impls(&self, type_name: &str) -> bool {
        let Some(indices) = self.impls_by_name.get(type_name) else {
            return false;
        };
        let mut has_inherent = false;
        for &idx in indices {
            let impl_info = &self.impls[idx];
            if impl_info.trait_.is_some() || impl_info.via_field.is_some() {
                return false; // has trait impl or via delegation
            }
            has_inherent = true;
        }
        has_inherent
    }

    /// Check if a type (by name) has an implementation for a trait (by name).
    pub fn has_trait_impl_by_name(&self, type_name: &str, trait_name: &str) -> bool {
        if let Some(indices) = self.impls_by_name.get(type_name) {
            if indices.iter().any(|&idx| {
                self.impls[idx].trait_name.as_deref() == Some(trait_name)
            }) {
                return true;
            }
        }
        // Intrinsic satisfaction: numeric primitives satisfy numeric traits.
        if is_numeric_primitive(type_name) && is_numeric_trait(trait_name) {
            return true;
        }
        // Intrinsic satisfaction: hashable/equatable primitives.
        if is_hashable_primitive(type_name) && is_hashable_trait(trait_name) {
            return true;
        }
        // Intrinsic satisfaction: all primitives (+ String) are Cloneable,
        // Displayable, and Debuggable. Cloning a POD is a copy; Display/Debug
        // route to runtime stringification helpers at IR lowering time.
        is_copy_displayable_primitive(type_name)
            && matches!(trait_name, "Cloneable" | "Displayable" | "Debuggable")
    }

    /// Get the trait's generic AST type args for a specific trait impl on a type (by name).
    /// Returns the first matching impl's trait_generic_args.
    pub fn trait_generic_args_by_name(&self, type_name: &str, trait_name: &str) -> &[Type] {
        if let Some(indices) = self.impls_by_name.get(type_name) {
            for &idx in indices {
                let impl_info = &self.impls[idx];
                if impl_info.trait_name.as_deref() == Some(trait_name) {
                    return &impl_info.trait_generic_args;
                }
            }
        }
        &[]
    }

    /// Check if `held_trait` (by name) satisfies `required_trait` (by name),
    /// either by being the same trait or by extending it (supertrait relationship).
    pub fn trait_satisfies(&self, held_trait: &str, required_trait: &str) -> bool {
        if held_trait == required_trait {
            return true;
        }
        // Find the held trait's DefId and check its extends list
        for info in self.traits.values() {
            if info.name == held_trait {
                for &parent_def_id in &info.extends {
                    if let Some(parent_info) = self.traits.get(&parent_def_id) {
                        if parent_info.name == required_trait {
                            return true;
                        }
                    }
                }
                break;
            }
        }
        false
    }

    /// Check if a type (by name) has a specific method in any equip block.
    pub fn has_method_for_type(&self, type_name: &str, method_name: &str) -> bool {
        let Some(indices) = self.impls_by_name.get(type_name) else {
            return false;
        };
        indices.iter().any(|&idx| {
            self.impls[idx].methods.contains_key(method_name)
        })
    }
}

/// Check if a type name is a numeric primitive (int, float, and their sized variants).
fn is_numeric_primitive(name: &str) -> bool {
    matches!(name,
        "int" | "int8" | "int16" | "int32" | "int64"
        | "uint" | "uint8" | "uint16" | "uint32" | "uint64"
        | "float" | "float32" | "float64"
    )
}

/// Check if a trait name is one that numeric primitives intrinsically satisfy.
fn is_numeric_trait(name: &str) -> bool {
    matches!(name,
        "Numeric" | "Add" | "Sub" | "Mul" | "Div" | "Rem" | "Mod" | "Neg"
        | "Comparable" | "Equatable" | "Default" | "One"
    )
}

/// Check if a type name is a primitive that intrinsically supports hashing and equality.
/// Includes: all numeric types, str, bool, char.
fn is_hashable_primitive(name: &str) -> bool {
    is_numeric_primitive(name) || matches!(name, "str" | "bool" | "char" | "String")
}

/// Check if a trait name is one that hashable primitives intrinsically satisfy.
fn is_hashable_trait(name: &str) -> bool {
    matches!(name, "Hashable" | "Equatable")
}

/// Primitives plus String that are Cloneable / Displayable / Debuggable
/// intrinsically (no derive needed).
fn is_copy_displayable_primitive(name: &str) -> bool {
    is_numeric_primitive(name) || matches!(name, "str" | "bool" | "char" | "String")
}

/// Build the trait and impl registry from the module.
pub fn build_registry(
    module: &Module,
    scopes: &ScopeTable,
    types: &mut TypeTable,
    _resolution_map: &ResolutionMap,
    errors: &mut Vec<SemanticError>,
) -> TraitRegistry {
    let mut registry = TraitRegistry::new();

    // Register built-in core traits before processing user-defined ones.
    register_builtin_traits(scopes, types, &mut registry);

    // First pass: collect all trait definitions (recursing into Item::Module
    // wrappers so imported-module traits land in the registry too).
    collect_traits_from_items(&module.items, scopes, types, &mut registry, errors);

    // Second pass: process all impl blocks (same recursion).
    process_impls_from_items(&module.items, scopes, types, &mut registry, errors);

    // Third pass: detect trait inheritance cycles (before validate_trait_impls to avoid stack overflow)
    validate_trait_cycles(&registry, errors);

    // Fourth pass: validate trait impls (check all required methods are present)
    validate_trait_impls(&registry, module, types, errors);

    registry
}

fn collect_traits_from_items(
    items: &[Spanned<Item>],
    scopes: &ScopeTable,
    types: &mut TypeTable,
    registry: &mut TraitRegistry,
    errors: &mut Vec<SemanticError>,
) {
    for item in items {
        match &item.node {
            Item::Trait(trait_def) => {
                collect_trait(trait_def, scopes, types, registry, errors);
            }
            Item::Module { items: inner, .. } => {
                collect_traits_from_items(inner, scopes, types, registry, errors);
            }
            _ => {}
        }
    }
}

fn process_impls_from_items(
    items: &[Spanned<Item>],
    scopes: &ScopeTable,
    types: &mut TypeTable,
    registry: &mut TraitRegistry,
    errors: &mut Vec<SemanticError>,
) {
    for item in items {
        match &item.node {
            Item::Equip(impl_block) => {
                process_impl(impl_block, scopes, types, registry, errors);
            }
            Item::Module { items: inner, .. } => {
                process_impls_from_items(inner, scopes, types, registry, errors);
            }
            _ => {}
        }
    }
}

/// Register built-in core traits.
fn register_builtin_traits(
    scopes: &ScopeTable,
    types: &TypeTable,
    registry: &mut TraitRegistry,
) {
    let builtin_traits: Vec<(&str, FxHashMap<String, FunctionSig>)> = vec![
        // Displayable: String display(self)
        ("Displayable", {
            let mut m = FxHashMap::default();
            m.insert("display".into(), FunctionSig {
                params: vec![],
                return_type: types.owned_string_id,
                has_self: true,
                self_ownership: None,
            });
            m
        }),
        // Debuggable: String debug(self)
        ("Debuggable", {
            let mut m = FxHashMap::default();
            m.insert("debug".into(), FunctionSig {
                params: vec![],
                return_type: types.owned_string_id,
                has_self: true,
                self_ownership: None,
            });
            m
        }),
        // Equatable: bool eq(self, Self other)
        ("Equatable", {
            let mut m = FxHashMap::default();
            m.insert("eq".into(), FunctionSig {
                params: vec![types.error_id], // Self placeholder
                return_type: types.bool_id,
                has_self: true,
                self_ownership: None,
            });
            m
        }),
        // Cloneable: Self clone(self)
        ("Cloneable", {
            let mut m = FxHashMap::default();
            m.insert("clone".into(), FunctionSig {
                params: vec![],
                return_type: types.error_id, // Self placeholder
                has_self: true,
                self_ownership: None,
            });
            m
        }),
        // Hashable: void hash[Hasher H](self, H &h) — state-based hashing,
        // generic over the Hasher implementation. The `H &h` param is
        // erased to `error_id` because (a) H is a method-level generic
        // resolved at the call site, and (b) the FxHasher TypeId isn't
        // available at built-in-registration time; impl validation
        // accepts both forms via shape matching.
        ("Hashable", {
            let mut m = FxHashMap::default();
            m.insert("hash".into(), FunctionSig {
                params: vec![types.error_id],
                return_type: types.void_id,
                has_self: true,
                self_ownership: None,
            });
            m
        }),
        // Hasher: narrow-waist role trait for hash state accumulators.
        // Implementors provide write_int / write_bytes / write_string /
        // finish. The concrete FxHasher lives in std.hash.
        ("Hasher", {
            let mut m = FxHashMap::default();
            m.insert("write_int".into(), FunctionSig {
                params: vec![types.int_id],
                return_type: types.void_id,
                has_self: true,
                self_ownership: Some(Ownership::MutableBorrow),
            });
            m.insert("write_bytes".into(), FunctionSig {
                params: vec![types.error_id], // Vector[byte] placeholder
                return_type: types.void_id,
                has_self: true,
                self_ownership: Some(Ownership::MutableBorrow),
            });
            m.insert("write_string".into(), FunctionSig {
                params: vec![types.owned_string_id],
                return_type: types.void_id,
                has_self: true,
                self_ownership: Some(Ownership::MutableBorrow),
            });
            m.insert("finish".into(), FunctionSig {
                params: vec![],
                return_type: types.int_id,
                has_self: true,
                self_ownership: None,
            });
            m
        }),
        // Ordinal: int ordinal(self)
        ("Ordinal", {
            let mut m = FxHashMap::default();
            m.insert("ordinal".into(), FunctionSig {
                params: vec![],
                return_type: types.int_id,
                has_self: true,
                self_ownership: None,
            });
            m
        }),
        // Drop: void drop(!self)
        ("Drop", {
            let mut m = FxHashMap::default();
            m.insert("drop".into(), FunctionSig {
                params: vec![],
                return_type: types.void_id,
                has_self: true,
                self_ownership: Some(Ownership::Move),
            });
            m
        }),
        // Iterator[T]: declared in `lib/std/iter.gg` as a user-space trait
        // so default-method bodies (count / any / all / find / fold / collect)
        // can ride on the standard `register_equip_sigs_with_defaults` path.
        // The placeholder def reserved at `resolve.rs` lets equip blocks parse
        // before the iter.gg declaration loads.
        // Iterable[T]: IterType iter(&self) — return type is placeholder (concrete from equip)
        ("Iterable", {
            let mut m = FxHashMap::default();
            m.insert("iter".into(), FunctionSig {
                params: vec![],
                return_type: types.error_id, // placeholder — concrete iterator type from equip block
                has_self: true,
                self_ownership: Some(Ownership::MutableBorrow),
            });
            m
        }),
        // Add[Out]: Out add(self, Self rhs)
        ("Add", {
            let mut m = FxHashMap::default();
            m.insert("add".into(), FunctionSig {
                params: vec![types.error_id], // Self placeholder
                return_type: types.error_id,  // Out placeholder
                has_self: true,
                self_ownership: None,
            });
            m
        }),
        // Sub[Out]: Out sub(self, Self rhs)
        ("Sub", {
            let mut m = FxHashMap::default();
            m.insert("sub".into(), FunctionSig {
                params: vec![types.error_id],
                return_type: types.error_id,
                has_self: true,
                self_ownership: None,
            });
            m
        }),
        // Mul[Out]: Out mul(self, Self rhs)
        ("Mul", {
            let mut m = FxHashMap::default();
            m.insert("mul".into(), FunctionSig {
                params: vec![types.error_id],
                return_type: types.error_id,
                has_self: true,
                self_ownership: None,
            });
            m
        }),
        // Div[Out]: Out div(self, Self rhs)
        ("Div", {
            let mut m = FxHashMap::default();
            m.insert("div".into(), FunctionSig {
                params: vec![types.error_id],
                return_type: types.error_id,
                has_self: true,
                self_ownership: None,
            });
            m
        }),
        // Rem[Out]: Out rem(self, Self rhs)
        ("Rem", {
            let mut m = FxHashMap::default();
            m.insert("rem".into(), FunctionSig {
                params: vec![types.error_id],
                return_type: types.error_id,
                has_self: true,
                self_ownership: None,
            });
            m
        }),
        // Mod[Out]: Out mod(self, Self rhs)
        ("Mod", {
            let mut m = FxHashMap::default();
            m.insert("mod".into(), FunctionSig {
                params: vec![types.error_id],
                return_type: types.error_id,
                has_self: true,
                self_ownership: None,
            });
            m
        }),
        // Neg[Out]: Out neg(self)
        ("Neg", {
            let mut m = FxHashMap::default();
            m.insert("neg".into(), FunctionSig {
                params: vec![],
                return_type: types.error_id, // Out placeholder
                has_self: true,
                self_ownership: None,
            });
            m
        }),
        // Comparable: int compare(self, Self other)
        ("Comparable", {
            let mut m = FxHashMap::default();
            m.insert("compare".into(), FunctionSig {
                params: vec![types.error_id], // Self placeholder
                return_type: types.int_id,
                has_self: true,
                self_ownership: None,
            });
            m
        }),
        // Index[K, V]: V get(self, K key)
        ("Index", {
            let mut m = FxHashMap::default();
            m.insert("get".into(), FunctionSig {
                params: vec![types.error_id], // K placeholder
                return_type: types.error_id,  // V placeholder
                has_self: true,
                self_ownership: None,
            });
            m
        }),
        // IndexMut[K, V]: void set(&self, K key, V value)
        ("IndexMut", {
            let mut m = FxHashMap::default();
            m.insert("set".into(), FunctionSig {
                params: vec![types.error_id, types.error_id], // K, V placeholders
                return_type: types.void_id,
                has_self: true,
                self_ownership: Some(Ownership::MutableBorrow),
            });
            m
        }),
        // Default: Self default() — static factory, no self
        ("Default", {
            let mut m = FxHashMap::default();
            m.insert("default".into(), FunctionSig {
                params: vec![],
                return_type: types.error_id, // Self placeholder
                has_self: false,
                self_ownership: None,
            });
            m
        }),
        // From[T]: Self from(T value) — static conversion, no self
        ("From", {
            let mut m = FxHashMap::default();
            m.insert("from".into(), FunctionSig {
                params: vec![types.error_id], // T placeholder
                return_type: types.error_id,  // Self placeholder
                has_self: false,
                self_ownership: None,
            });
            m
        }),
        // TryFrom[T]: Result[Self, str] try_from(T value) — fallible static conversion
        ("TryFrom", {
            let mut m = FxHashMap::default();
            m.insert("try_from".into(), FunctionSig {
                params: vec![types.error_id], // T placeholder
                return_type: types.error_id,  // Result[Self, str] placeholder
                has_self: false,
                self_ownership: None,
            });
            m
        }),
        // Measurable: int len(self) — types that have a length
        ("Measurable", {
            let mut m = FxHashMap::default();
            m.insert("len".into(), FunctionSig {
                params: vec![],
                return_type: types.int_id,
                has_self: true,
                self_ownership: None, // plain self (immutable borrow)
            });
            m
        }),
        // Parseable: Option[Self] parse(str s) — static parsing, no self
        ("Parseable", {
            let mut m = FxHashMap::default();
            m.insert("parse".into(), FunctionSig {
                params: vec![types.string_id], // str argument
                return_type: types.error_id,   // Option[Self] placeholder
                has_self: false,
                self_ownership: None,
            });
            m
        }),
        // One: Self one() — static factory returning multiplicative identity, no self
        ("One", {
            let mut m = FxHashMap::default();
            m.insert("one".into(), FunctionSig {
                params: vec![],
                return_type: types.error_id, // Self placeholder
                has_self: false,
                self_ownership: None,
            });
            m
        }),
        // Numeric: composite trait — Add + Sub + Mul + Div + Rem + Neg + Comparable + Default + One
        // Empty method map; all methods come from parent traits via `extends`.
        ("Numeric", {
            FxHashMap::default()
        }),
    ];

    for (name, methods) in builtin_traits {
        // lookup_type for namespace consistency with collect_trait/process_impl
        // (traits are type-namespace-only; a value-first lookup can be diverted
        // by a same-named value-namespace def).
        if let Some(def_id) = scopes.lookup_type(name) {
            let has_default_body: FxHashMap<String, bool> = methods
                .keys()
                .map(|k| (k.clone(), false))
                .collect();
            registry.traits.insert(def_id, TraitInfo {
                def_id,
                name: name.to_string(),
                methods,
                has_default_body,
                extends: Vec::new(),
                method_shapes: FxHashMap::default(),
                trait_generic_params: Vec::new(),
                default_method_sigs: FxHashMap::default(),
            });
        }
    }

    // Wire Numeric's extends: Add + Sub + Mul + Div + Rem + Neg + Comparable + Default + One
    if let Some(numeric_def_id) = scopes.lookup_type("Numeric") {
        let parent_names = ["Add", "Sub", "Mul", "Div", "Rem", "Mod", "Neg", "Comparable", "Default", "One"];
        let parent_ids: Vec<DefId> = parent_names.iter()
            .filter_map(|name| scopes.lookup_type(name))
            .collect();
        if let Some(info) = registry.traits.get_mut(&numeric_def_id) {
            info.extends = parent_ids;
        }
    }
}

fn collect_trait(
    trait_def: &TraitDef,
    scopes: &ScopeTable,
    types: &mut TypeTable,
    registry: &mut TraitRegistry,
    errors: &mut Vec<SemanticError>,
) {
    // TYPE-namespace lookup, NOT the value-first `lookup`: a `from mod import
    // Trait` placeholder (DefKind::Import, registered in BOTH namespaces) is
    // only overwritten by the module's `export_non_private` in the TYPE
    // namespace (traits are type-namespace-only, so the module scope has no
    // value entry to export). The stale value-namespace placeholder then wins
    // a value-first `lookup`, keying the registry under the IMPORT def while
    // `process_impl` resolves the equip's trait via `lookup_type` to the REAL
    // trait def — the key mismatch made every cross-module trait-DEFAULT
    // method invisible to `resolve_method`/`resolve_method_by_name` (the call
    // typed as `error_id`, silently unifying with anything: the D23
    // silent-miscompile hole, measured `int x = 1 + s.risky()` → garbage).
    let Some(def_id) = scopes.lookup_type(&trait_def.name.node) else {
        return;
    };

    let mut methods = FxHashMap::default();
    let mut has_default_body = FxHashMap::default();
    let mut method_shapes = FxHashMap::default();
    let mut default_method_sigs = FxHashMap::default();

    for item in &trait_def.items {
        if let TraitItem::Method(method) = &item.node {
            validate_default_param_ordering(&method.params, errors);
            let sig = build_function_sig(method, scopes, types);
            let has_body = !matches!(method.body, FunctionBody::Declaration | FunctionBody::Extern(_));
            has_default_body.insert(method.name.node.clone(), has_body);
            methods.insert(method.name.node.clone(), sig);
            if let Some(shape) = build_method_sig_shape(method) {
                method_shapes.insert(method.name.node.clone(), shape);
            }
            if has_body {
                default_method_sigs.insert(
                    method.name.node.clone(),
                    build_default_method_sig(method),
                );
            }
        }
    }

    // Extract the trait's own generic param names (e.g., `["T"]` for
    // `trait Iterator[T]:`). Used by call-site substitution to pair
    // trait generic positions with an impl's `trait_generic_args`.
    let trait_generic_params: Vec<String> = trait_def.generic_params.as_ref().map(|gp| {
        gp.node.params.iter().filter_map(|p| match &p.node {
            GenericParam::Type { name, .. } => Some(name.node.clone()),
            GenericParam::Const { .. } => None,
        }).collect()
    }).unwrap_or_default();

    // Resolve extends — same type-namespace rule as the registry key above.
    // A cross-module parent trait would otherwise bind the import placeholder
    // and inherited-REQUIRED-method VALIDATION would silently flip to accept
    // (`validate_trait_impls`' extends walk keys dead → a missing parent
    // method passes check; mutation-tested). Supertrait DEFAULT-method
    // resolution walks these `extends` edges at resolve time via
    // `find_default_method` (resolve_method / by_name / shape / shape_by_name).
    let mut extends = Vec::new();
    for bound in &trait_def.extends {
        if let Some(parent_id) = scopes.lookup_type(&bound.node.name.node) {
            extends.push(parent_id);
        }
    }

    registry.traits.insert(
        def_id,
        TraitInfo {
            def_id,
            name: trait_def.name.node.clone(),
            methods,
            has_default_body,
            extends,
            method_shapes,
            trait_generic_params,
            default_method_sigs,
        },
    );
}

/// Build the AST-level default-method sig for call-site Self/T substitution.
/// Captures exactly what `FunctionSig` erases (Self → error_id, trait T →
/// error_id or a placeholder generic-param def).
pub fn build_default_method_sig(func: &FunctionDef) -> DefaultMethodSig {
    let method_generic_params: Vec<String> = func.generic_params.as_ref().map(|gp| {
        gp.node.params.iter().filter_map(|p| match &p.node {
            GenericParam::Type { name, .. } => Some(name.node.clone()),
            GenericParam::Const { .. } => None,
        }).collect()
    }).unwrap_or_default();

    let mut param_types = Vec::new();
    let mut param_ownerships = Vec::new();
    let mut has_self = false;
    let mut self_ownership = None;

    for param in &func.params {
        if param.node.name.node == "self" {
            has_self = true;
            self_ownership = Some(param.node.ownership);
            continue;
        }
        param_types.push(param.node.type_.node.clone());
        param_ownerships.push(param.node.ownership);
    }

    DefaultMethodSig {
        method_generic_params,
        param_types,
        return_type: func.return_type.node.clone(),
        throws_ast: func.throws.explicit_type().map(|t| t.node.clone()),
        param_ownerships,
        has_self,
        self_ownership,
    }
}

fn process_impl(
    impl_block: &EquipBlock,
    scopes: &ScopeTable,
    types: &mut TypeTable,
    registry: &mut TraitRegistry,
    errors: &mut Vec<SemanticError>,
) {
    // Resolve the self type
    let self_type_name = type_name(&impl_block.type_.node);
    let self_type_id = types::ast_type_to_resolved(
        &impl_block.type_.node,
        impl_block.type_.span,
        scopes,
        types,
    )
    .unwrap_or(types.error_id);

    // Resolve the trait (if any). Use type-namespace lookup so that,
    // e.g., `equip IoError with Error:` finds the user-defined `trait
    // Error` rather than the `Result.Error` variant that shares the
    // name in the value namespace.
    let trait_def_id = impl_block.trait_.as_ref().and_then(|t| {
        if let Type::Named { name, .. } = &t.trait_name.node {
            scopes.lookup_type(&name.node)
        } else {
            None
        }
    });

    let trait_name = impl_block.trait_.as_ref().map(|t| {
        if let Type::Named { name, .. } = &t.trait_name.node {
            name.node.clone()
        } else {
            "<unknown>".into()
        }
    });

    // Validate `via` is only used with `with Trait`
    let via_field = impl_block.via_field.as_ref().map(|v| v.node.clone());
    if via_field.is_some() && trait_def_id.is_none() {
        errors.push(SemanticError {
            kind: SemanticErrorKind::ViaWithoutTrait,
            span: impl_block.span,
        });
        return;
    }

    // Reject equipping a scalar primitive with a trait. A scalar primitive
    // (int/int8-64/uint/uint8-64/float/float32-64/bool) has no addressable
    // heap `self` and no vtable slot, so any trait method dispatch — direct
    // `x.m()` OR `Box[Trait]` trait-object dispatch — miscompiles (C backend
    // SEGVs dereferencing the value-as-pointer; LLVM rejects the NULL-degraded
    // vtable). Intrinsic trait satisfaction (Hashable/Cloneable/Displayable/
    // Numeric) does NOT go through user equip blocks — it is short-circuited by
    // `has_trait_impl_by_name` and special-cased in lowering (`x.hash(&h)` →
    // write_int) — so this reject leaves it untouched. `String`/`str`(CStr) are
    // heap/view-backed and excluded (`equip String with Writer` is legitimate),
    // as is `Void`. Keying off the RESOLVED type (Core #2, typed metadata) is
    // alias-robust: `type MyInt = int` is meta-inlined to `int` before this
    // point, so it rejects too. Fixing the class at the producer (equip
    // registration) kills both crash shapes at once.
    if trait_name.is_some() {
        if let types::ResolvedType::Primitive(pt) = types.get(self_type_id) {
            if !matches!(
                pt,
                PrimitiveType::StringType | PrimitiveType::CStr | PrimitiveType::Void
            ) {
                errors.push(SemanticError {
                    kind: SemanticErrorKind::PrimitiveTraitImpl {
                        trait_: trait_name.clone().unwrap_or_default(),
                        type_: self_type_name.clone(),
                    },
                    span: impl_block.span,
                });
                return;
            }
        }
    }

    // Resolve trait generic args to TypeIds for duplicate detection.
    // e.g. From[int] → [int_type_id], From[str] → [str_type_id], Displayable → []
    let trait_arg_type_ids: Vec<TypeId> = impl_block.trait_.as_ref()
        .map(|t| {
            if let Type::Named { generic_args, .. } = &t.trait_name.node {
                generic_args.iter()
                    .filter_map(|a| types::ast_type_to_resolved(&a.node, a.span, scopes, types).ok())
                    .collect()
            } else {
                Vec::new()
            }
        })
        .unwrap_or_default();

    // Check for duplicate trait impl (same trait + type + type args)
    if let Some(trait_id) = trait_def_id {
        if registry.trait_impls.contains_key(&(trait_id, self_type_id, trait_arg_type_ids.clone())) {
            errors.push(SemanticError {
                kind: SemanticErrorKind::DuplicateImpl {
                    trait_: trait_name.clone().unwrap_or_default(),
                    type_: self_type_name.clone(),
                },
                span: impl_block.span,
            });
            return;
        }
    }

    // Orphan rule: at least one of (trait, type) must be defined in this module.
    // A definition is "local" if it has a real span (not built-in) and is not an import.
    if trait_def_id.is_some() {
        let type_is_local = scopes.lookup(&self_type_name)
            .map(|def_id| {
                let info = scopes.get_def(def_id);
                info.span != Span::dummy() && info.kind != DefKind::Import
            })
            .unwrap_or(false);

        let trait_is_local = trait_def_id
            .map(|def_id| {
                let info = scopes.get_def(def_id);
                info.span != Span::dummy() && info.kind != DefKind::Import
            })
            .unwrap_or(false);

        if !type_is_local && !trait_is_local {
            errors.push(SemanticError {
                kind: SemanticErrorKind::OrphanImpl {
                    trait_: trait_name.clone().unwrap_or_default(),
                    type_: self_type_name.clone(),
                },
                span: impl_block.span,
            });
            return;
        }
    }

    // Collect methods
    let mut methods = FxHashMap::default();
    let mut method_shapes = FxHashMap::default();
    for method in &impl_block.items {
        let method_def_id = scopes.lookup_def_by_span(&method.node.name.node, method.node.name.span);
        let sig = build_function_sig(&method.node, scopes, types);
        let def_id = method_def_id.unwrap_or(DefId(0));
        methods.insert(method.node.name.node.clone(), (def_id, sig));
        if let Some(shape) = build_method_sig_shape(&method.node) {
            method_shapes.insert(method.node.name.node.clone(), shape);
        }
    }

    // Extract generic args from the trait type (e.g., [int] from Iterator[int])
    let trait_generic_args = impl_block.trait_.as_ref()
        .and_then(|t| {
            if let Type::Named { generic_args, .. } = &t.trait_name.node {
                Some(generic_args.iter().map(|a| a.node.clone()).collect())
            } else {
                None
            }
        })
        .unwrap_or_default();

    // Impl's own generic params (e.g., `["T"]` for
    // `equip [T] VectorIter[T]:`). Used to bind impl locals from the
    // receiver's concrete type args at call time.
    let impl_generic_params: Vec<String> = impl_block.generic_params.as_ref().map(|gp| {
        gp.node.params.iter().filter_map(|p| match &p.node {
            GenericParam::Type { name, .. } => Some(name.node.clone()),
            GenericParam::Const { .. } => None,
        }).collect()
    }).unwrap_or_default();

    let impl_idx = registry.impls.len();
    registry.impls.push(EquipInfo {
        self_type: self_type_id,
        self_type_name: self_type_name.clone(),
        trait_: trait_def_id,
        trait_name,
        methods,
        span: impl_block.span,
        trait_generic_args,
        via_field,
        method_shapes,
        self_type_ast: impl_block.type_.node.clone(),
        impl_generic_params,
    });

    // Mirror `trait_impls_by_type` but keyed by self-type name. Drives
    // the name-keyed fallback methods (`has_any_impl_by_name`, etc.).
    registry
        .impls_by_name
        .entry(self_type_name.clone())
        .or_default()
        .push(impl_idx);

    if let Some(trait_id) = trait_def_id {
        registry.trait_impls.insert((trait_id, self_type_id, trait_arg_type_ids), impl_idx);
        registry
            .trait_impls_by_type
            .entry(self_type_id)
            .or_default()
            .push(impl_idx);
    } else {
        // Multiple inherent `equip T:` blocks are fine — they just
        // accumulate methods on T. What's NOT fine is two different
        // blocks declaring the same method; that's a real duplicate.
        // Rust follows the same rule.
        let new_methods: Vec<&String> = registry.impls[impl_idx].methods.keys().collect();
        if let Some(existing_indices) = registry.inherent_impls.get(&self_type_id) {
            for &prev_idx in existing_indices {
                let prev = &registry.impls[prev_idx];
                for m in &new_methods {
                    if prev.methods.contains_key(m.as_str()) {
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::DuplicateImpl {
                                trait_: format!("(inherent method `{m}`)"),
                                type_: self_type_name.clone(),
                            },
                            span: impl_block.span,
                        });
                        return;
                    }
                }
            }
        }
        registry
            .inherent_impls
            .entry(self_type_id)
            .or_default()
            .push(impl_idx);
    }
}

fn validate_trait_impls(registry: &TraitRegistry, module: &Module, types: &TypeTable, errors: &mut Vec<SemanticError>) {
    for impl_info in &registry.impls {
        let Some(trait_def_id) = impl_info.trait_ else {
            continue;
        };
        let Some(trait_info) = registry.traits.get(&trait_def_id) else {
            continue;
        };

        // Validate `via` delegation field
        if let Some(ref via_field_name) = impl_info.via_field {
            validate_via_field(
                via_field_name,
                &impl_info.self_type_name,
                trait_info.name.as_str(),
                module,
                registry,
                impl_info.span,
                errors,
            );
        }

        // Collect all required methods including inherited parent methods
        let all_methods = collect_all_required_methods(trait_info, registry);

        for (method_name, has_default, source_trait_name) in &all_methods {
            if !has_default && !impl_info.methods.contains_key(method_name) {
                // If `via` delegation is active, skip missing method errors
                // (they'll be auto-forwarded in codegen)
                if impl_info.via_field.is_some() {
                    continue;
                }
                // Split-equip satisfaction: an inherited method can be
                // supplied by a sibling equip block on the same type
                // (e.g. `equip ParseErr with Error:` inherits display
                // from Displayable; `equip ParseErr with Displayable:`
                // provides it elsewhere in the file). Look for the
                // method on *any* impl of *any* trait for the same
                // self-type name.
                let satisfied_elsewhere = *source_trait_name != trait_info.name
                    && registry.impls.iter().any(|other| {
                        other.self_type_name == impl_info.self_type_name
                            && other.methods.contains_key(method_name)
                    });
                if satisfied_elsewhere {
                    continue;
                }
                errors.push(SemanticError {
                    kind: SemanticErrorKind::MissingTraitMethod {
                        trait_: source_trait_name.clone(),
                        method: method_name.clone(),
                        type_: impl_info.self_type_name.clone(),
                    },
                    span: impl_info.span,
                });
            }

            // Validate signature of methods present in the equip block
            let owner_trait_info = if *source_trait_name == trait_info.name {
                trait_info
            } else {
                registry.traits.values()
                    .find(|t| t.name == *source_trait_name)
                    .unwrap_or(trait_info)
            };
            let Some(trait_sig) = owner_trait_info.methods.get(method_name) else { continue };
            let Some((_def_id, impl_sig)) = impl_info.methods.get(method_name) else { continue };

            // Return type — skip if trait uses error_id as a placeholder
            // (Self or generic T at the top level), or if the trait's return
            // type is a generic that contains error_id internally
            // (e.g. `Option[T]` where T resolves to error_id at trait
            // registration). The latter shows up for user-space generic
            // traits like `Iterator[T]: Option[T] next(&self)` — the impl
            // has `Option[int]` but the trait sig has `Option[<error>]`.
            if trait_sig.return_type != types.error_id
                && trait_sig.return_type != impl_sig.return_type
                && !type_contains_error(types, trait_sig.return_type)
            {
                let trait_display = types.display(trait_sig.return_type);
                let impl_display = types.display(impl_sig.return_type);
                if trait_display != impl_display {
                    errors.push(SemanticError {
                        kind: SemanticErrorKind::MethodSignatureMismatch {
                            trait_: source_trait_name.clone(),
                            method: method_name.clone(),
                            detail: format!(
                                "return type is `{}`, expected `{}`",
                                impl_display,
                                trait_display,
                            ),
                        },
                        span: impl_info.span,
                    });
                }
            }

            if trait_sig.params.len() != impl_sig.params.len() {
                errors.push(SemanticError {
                    kind: SemanticErrorKind::MethodSignatureMismatch {
                        trait_: source_trait_name.clone(),
                        method: method_name.clone(),
                        detail: format!(
                            "has {} parameter(s), expected {}",
                            impl_sig.params.len(),
                            trait_sig.params.len(),
                        ),
                    },
                    span: impl_info.span,
                });
            } else {
                for (i, (trait_param, impl_param)) in
                    trait_sig.params.iter().zip(&impl_sig.params).enumerate()
                {
                    // Skip if trait uses error_id as a placeholder (e.g. Self, generic T)
                    // — same logic as return-type check above. Generics that
                    // contain error_id internally (e.g. `Vector[T]`) also
                    // count as placeholders.
                    if *trait_param == types.error_id
                        || type_contains_error(types, *trait_param)
                    {
                        continue;
                    }
                    if trait_param != impl_param {
                        // Type-level equality is strict but the trait
                        // registry interns types separately from the
                        // impl builder, so legitimately-equivalent types
                        // (same displayed form, notably trait objects
                        // like `<trait object>`) can disagree at the
                        // TypeId level. Fall back to comparing displayed
                        // forms before flagging the mismatch.
                        let trait_display = types.display(*trait_param);
                        let impl_display = types.display(*impl_param);
                        if trait_display == impl_display {
                            continue;
                        }
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::MethodSignatureMismatch {
                                trait_: source_trait_name.clone(),
                                method: method_name.clone(),
                                detail: format!(
                                    "parameter {} type is `{}`, expected `{}`",
                                    i + 1,
                                    impl_display,
                                    trait_display,
                                ),
                            },
                            span: impl_info.span,
                        });
                    }
                }
            }

            if trait_sig.has_self != impl_sig.has_self {
                let detail = if trait_sig.has_self {
                    "missing `self` parameter"
                } else {
                    "unexpected `self` parameter"
                };
                errors.push(SemanticError {
                    kind: SemanticErrorKind::MethodSignatureMismatch {
                        trait_: source_trait_name.clone(),
                        method: method_name.clone(),
                        detail: detail.to_string(),
                    },
                    span: impl_info.span,
                });
            }

            // Skip self ownership check when trait uses None as a wildcard (builtin traits
            // that don't enforce a specific ownership mode).
            if let Some(expected_ownership) = trait_sig.self_ownership {
                if impl_sig.self_ownership != Some(expected_ownership) {
                    errors.push(SemanticError {
                        kind: SemanticErrorKind::MethodSignatureMismatch {
                            trait_: source_trait_name.clone(),
                            method: method_name.clone(),
                            detail: format!(
                                "self ownership is `{:?}`, expected `{:?}`",
                                impl_sig.self_ownership, expected_ownership,
                            ),
                        },
                        span: impl_info.span,
                    });
                }
            }
        }
    }
}

/// Recursively check whether a type contains `Error` anywhere in its
/// type-argument tree. Two callers rely on this:
///  - trait method-signature validation (this file): trait signatures store
///    unresolved type parameters (T, Self) as `error_id`, so any composite
///    type whose args reach `error_id` is a placeholder awaiting substitution
///    at the impl site, and strict signature checks are skipped for it.
///  - generic-struct field access (`typecheck.rs` `Expr::FieldAccess`): a
///    generic-param field resolves to `Error` in `field_types` (module-scope
///    resolution), so the concrete-field slice only trusts — and type-checks
///    against — field types that are fully Error-free.
///
/// The base case matches `ResolvedType::Error` directly (catching any
/// Error-typed TypeId, not just the canonical `types.error_id`), and the
/// recursion covers the full composite-variant set so that an `Error` nested
/// inside any container (`A[]`, `ref A`, `A!`, `Callable[A(...)]`, a tuple, a
/// function type, …) is detected — a type containing `Error` anywhere is not
/// "concrete". One source of truth (Core #3); do not add a second copy.
pub(crate) fn type_contains_error(types: &TypeTable, type_id: TypeId) -> bool {
    use crate::semantic::types::ResolvedType;
    match types.get(type_id) {
        ResolvedType::Error => true,
        ResolvedType::Generic(_, args) => {
            args.iter().any(|&arg| type_contains_error(types, arg))
        }
        ResolvedType::Tuple(elems) => {
            elems.iter().any(|&e| type_contains_error(types, e))
        }
        ResolvedType::Array(e, _)
        | ResolvedType::Slice(e)
        | ResolvedType::Ref(e)
        | ResolvedType::Owned(e)
        | ResolvedType::CallableTrait(e)
        | ResolvedType::MutCallableTrait(e)
        | ResolvedType::ConsumeCallableTrait(e) => type_contains_error(types, *e),
        ResolvedType::BoxedCallable { inner, .. } => type_contains_error(types, *inner),
        ResolvedType::Function { params, return_type, .. } => {
            params.iter().any(|&p| type_contains_error(types, p))
                || type_contains_error(types, *return_type)
        }
        _ => false,
    }
}

/// Validate that a `via` field exists on the struct and its type implements the target trait.
fn validate_via_field(
    field_name: &str,
    type_name: &str,
    trait_name: &str,
    module: &Module,
    registry: &TraitRegistry,
    span: Span,
    errors: &mut Vec<SemanticError>,
) {
    // Find the struct definition in the module
    let struct_def = module.items.iter().find_map(|item| {
        if let Item::Struct(s) = &item.node {
            if s.name.node == type_name { Some(s) } else { None }
        } else {
            None
        }
    });
    let Some(struct_def) = struct_def else {
        // Not a struct (could be an enum or primitive) — skip validation
        return;
    };

    // Check the field exists
    let field = struct_def.fields.iter().find(|f| f.node.name.node == field_name);
    let Some(field) = field else {
        errors.push(SemanticError {
            kind: SemanticErrorKind::ViaFieldNotFound {
                field: field_name.to_string(),
                type_: type_name.to_string(),
            },
            span,
        });
        return;
    };

    // Extract the field's type name
    let field_type_name = type_name_from_ast(&field.node.type_.node);
    let Some(ref field_type_name) = field_type_name else {
        return; // Complex type — skip validation, C compiler will catch issues
    };

    // Check the field's type implements the target trait
    if !registry.has_trait_impl_by_name(field_type_name, trait_name) {
        errors.push(SemanticError {
            kind: SemanticErrorKind::ViaFieldTypeMissingTrait {
                field: field_name.to_string(),
                field_type: field_type_name.clone(),
                trait_: trait_name.to_string(),
            },
            span,
        });
    }
}

/// Extract a simple type name from an AST type (returns None for complex types).
fn type_name_from_ast(ty: &Type) -> Option<String> {
    match ty {
        Type::Named { name, .. } => Some(name.node.clone()),
        Type::Primitive(p) => Some(format!("{p:?}").to_lowercase()),
        _ => None,
    }
}

/// Detect cycles in trait inheritance via DFS. Emits `TraitCycle` errors.
fn validate_trait_cycles(registry: &TraitRegistry, errors: &mut Vec<SemanticError>) {
    let mut visited = FxHashSet::default();
    let mut in_stack: Vec<(DefId, String)> = Vec::new();

    let ids: Vec<DefId> = registry.traits.keys().copied().collect();
    for id in ids {
        if !visited.contains(&id) {
            dfs_detect_cycle(id, registry, &mut visited, &mut in_stack, errors);
        }
    }
}

fn dfs_detect_cycle(
    id: DefId,
    registry: &TraitRegistry,
    visited: &mut FxHashSet<DefId>,
    in_stack: &mut Vec<(DefId, String)>,
    errors: &mut Vec<SemanticError>,
) {
    let Some(trait_info) = registry.traits.get(&id) else { return };

    // Cycle found — id is already in the current DFS path
    if let Some(cycle_start) = in_stack.iter().position(|(sid, _)| *sid == id) {
        let mut path: Vec<String> = in_stack[cycle_start..]
            .iter()
            .map(|(_, name)| name.clone())
            .collect();
        path.push(trait_info.name.clone());
        let cycle_str = path.join(" → ");
        errors.push(SemanticError {
            kind: SemanticErrorKind::TraitCycle {
                trait_: trait_info.name.clone(),
                cycle: cycle_str,
            },
            span: Span::dummy(),
        });
        return;
    }

    if visited.contains(&id) {
        return;
    }

    in_stack.push((id, trait_info.name.clone()));
    let extends = trait_info.extends.clone();
    for parent_id in extends {
        dfs_detect_cycle(parent_id, registry, visited, in_stack, errors);
    }
    in_stack.pop();
    visited.insert(id);
}

/// Collect all methods required by a trait, including inherited parent methods.
/// Returns (method_name, has_default, source_trait_name) tuples.
/// Uses a visited set to guard against cycles (which are reported separately).
fn collect_all_required_methods(
    trait_info: &TraitInfo,
    registry: &TraitRegistry,
) -> Vec<(String, bool, String)> {
    let mut visited = FxHashSet::default();
    collect_all_required_methods_inner(trait_info, registry, &mut visited)
}

fn collect_all_required_methods_inner(
    trait_info: &TraitInfo,
    registry: &TraitRegistry,
    visited: &mut FxHashSet<DefId>,
) -> Vec<(String, bool, String)> {
    if !visited.insert(trait_info.def_id) {
        return vec![]; // cycle guard — already visited
    }
    let mut methods = Vec::new();

    // Recursively collect parent trait methods
    for &parent_id in &trait_info.extends {
        if let Some(parent_info) = registry.traits.get(&parent_id) {
            methods.extend(collect_all_required_methods_inner(parent_info, registry, visited));
        }
    }

    // Add own methods
    for (method_name, _sig) in &trait_info.methods {
        let has_default = trait_info
            .has_default_body
            .get(method_name)
            .copied()
            .unwrap_or(false);
        methods.push((method_name.clone(), has_default, trait_info.name.clone()));
    }

    methods
}

/// Build a `MethodSigShape` from a method's AST `FunctionDef`. Returns `None`
/// for non-generic methods.
pub fn build_method_sig_shape(func: &FunctionDef) -> Option<MethodSigShape> {
    let gp = func.generic_params.as_ref()?;
    let names: Vec<String> = gp.node.params.iter().filter_map(|p| match &p.node {
        GenericParam::Type { name, .. } => Some(name.node.clone()),
        GenericParam::Const { .. } => None,
    }).collect();
    if names.is_empty() {
        return None;
    }
    let param_types: Vec<Type> = func.params.iter()
        .filter(|p| p.node.name.node != "self")
        .map(|p| p.node.type_.node.clone())
        .collect();
    Some(MethodSigShape {
        generic_params: names,
        param_types,
        return_type: func.return_type.node.clone(),
    })
}

/// Substitute named method-level generic params in an AST `Type` against a
/// binding map. Walks the type recursively, replacing every
/// `Type::Named { name, generic_args: [] }` whose `name` is a key in the
/// bindings with the bound AST type. Other shapes recurse into their
/// children.
///
/// This is the core kernel for method-level generic inference — once
/// inference resolves `F → bool(int)` and `U → int`, callers use this to
/// produce a fully-instantiated AST sig (no remaining generic placeholders).
pub fn substitute_ast_type(ty: &Type, bindings: &FxHashMap<String, Type>) -> Type {
    match ty {
        Type::Named { name, generic_args } => {
            if generic_args.is_empty() {
                if let Some(replacement) = bindings.get(&name.node) {
                    return replacement.clone();
                }
                return ty.clone();
            }
            let new_args: Vec<Spanned<Type>> = generic_args.iter().map(|a| Spanned {
                node: substitute_ast_type(&a.node, bindings),
                span: a.span,
            }).collect();
            Type::Named { name: name.clone(), generic_args: new_args }
        }
        Type::Array { element, size } => Type::Array {
            element: Box::new(Spanned {
                node: substitute_ast_type(&element.node, bindings),
                span: element.span,
            }),
            size: size.clone(),
        },
        Type::Slice { element } => Type::Slice {
            element: Box::new(Spanned {
                node: substitute_ast_type(&element.node, bindings),
                span: element.span,
            }),
        },
        Type::Tuple(elems) => Type::Tuple(elems.iter().map(|e| Spanned {
            node: substitute_ast_type(&e.node, bindings),
            span: e.span,
        }).collect()),
        Type::Function { return_type, params, param_ownerships } => Type::Function {
            return_type: Box::new(Spanned {
                node: substitute_ast_type(&return_type.node, bindings),
                span: return_type.span,
            }),
            params: params.iter().map(|p| Spanned {
                node: substitute_ast_type(&p.node, bindings),
                span: p.span,
            }).collect(),
            param_ownerships: param_ownerships.clone(),
        },
        Type::Ref(inner) => Type::Ref(Box::new(Spanned {
            node: substitute_ast_type(&inner.node, bindings),
            span: inner.span,
        })),
        Type::Owned(inner) => Type::Owned(Box::new(Spanned {
            node: substitute_ast_type(&inner.node, bindings),
            span: inner.span,
        })),
        Type::Pointer(inner) => Type::Pointer(Box::new(Spanned {
            node: substitute_ast_type(&inner.node, bindings),
            span: inner.span,
        })),
        Type::SelfType => {
            // Treat `Self` as a bindable placeholder so call-site
            // substitution in `Iterator[T]` default sigs (e.g.,
            // `TakeIter[Self, T] take(self, int n)`) can bind
            // `Self → VectorIter[int]` against a concrete receiver.
            // Other callers that don't include a "Self" entry in
            // `bindings` (method-generic inference) still get
            // SelfType through unchanged.
            if let Some(replacement) = bindings.get("Self") {
                return replacement.clone();
            }
            ty.clone()
        }
        Type::Primitive(_) | Type::Inferred => ty.clone(),
    }
}

fn build_function_sig(func: &FunctionDef, scopes: &ScopeTable, types: &mut TypeTable) -> FunctionSig {
    let raw_return_type = types::ast_type_to_resolved(
        &func.return_type.node,
        func.return_type.span,
        scopes,
        types,
    )
    .unwrap_or(types.error_id);

    // Async methods expose Future[T] as their return type at call sites, matching
    // how top-level async functions are registered in typecheck.rs (line ~2963).
    let return_type = if func.qualifiers.is_async {
        if let Some(future_def_id) = scopes.lookup("Future") {
            types.insert(crate::semantic::types::ResolvedType::Generic(
                future_def_id,
                vec![raw_return_type],
            ))
        } else {
            raw_return_type
        }
    } else {
        raw_return_type
    };

    let mut params = Vec::new();
    let mut has_self = false;
    let mut self_ownership = None;

    for param in &func.params {
        if param.node.name.node == "self" {
            has_self = true;
            self_ownership = Some(param.node.ownership);
            continue;
        }
        let param_type = types::ast_type_to_resolved(
            &param.node.type_.node,
            param.node.type_.span,
            scopes,
            types,
        )
        .unwrap_or(types.error_id);
        params.push(param_type);
    }

    FunctionSig {
        params,
        return_type,
        has_self,
        self_ownership,
    }
}

/// Get a human-readable name for an AST type.
fn type_name(ty: &Type) -> String {
    match ty {
        Type::Named { name, .. } => name.node.clone(),
        Type::Primitive(p) => format!("{p:?}").to_lowercase(),
        Type::SelfType => "Self".into(),
        _ => "<complex type>".into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use crate::semantic::resolve;

    fn analyze(source: &str) -> (TraitRegistry, Vec<SemanticError>) {
        let mut parser = Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);

        let mut scopes = ScopeTable::new();
        let mut types = TypeTable::new();
        let mut errors = Vec::new();
        let mut ctx = resolve::collect_top_level(&module, &mut scopes, &mut types, &mut errors);
        let resolution_map = resolve::resolve_bodies(&module, &mut scopes, &mut types, &mut errors, &mut ctx.function_info, &mut ctx.function_body_scopes, &ctx.file_module_scopes);
        let registry = build_registry(&module, &scopes, &mut types, &resolution_map, &mut errors);
        (registry, errors)
    }

    #[test]
    fn inherent_impl() {
        let source = "\
struct Point:
    float x
    float y

equip Point:
    float distance(self):
        return 0.0
";
        let (registry, errors) = analyze(source);
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(registry.impls.len(), 1);
        assert!(registry.impls[0].trait_.is_none());
        assert!(registry.impls[0].methods.contains_key("distance"));
    }

    #[test]
    fn trait_impl() {
        let source = "\
trait Drawable:
    void draw(self)

struct Circle:
    float radius

equip Circle with Drawable:
    void draw(self):
        pass
";
        let (registry, errors) = analyze(source);
        assert!(errors.is_empty(), "errors: {:?}", errors);
        // 25 built-in traits + 1 user-defined trait (Iterator moved to user-space in lib/std/iter.gg)
        assert_eq!(registry.traits.len(), 26);
        assert_eq!(registry.impls.len(), 1);
        assert!(registry.impls[0].trait_.is_some());
    }

    #[test]
    fn missing_trait_method() {
        let source = "\
trait Drawable:
    void draw(self)
    float area(self)

struct Circle:
    float radius

equip Circle with Drawable:
    void draw(self):
        pass
";
        let (_, errors) = analyze(source);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::MissingTraitMethod { method, .. } if method == "area"
        )));
    }

    #[test]
    fn builtin_traits_registered() {
        let (registry, errors) = analyze("");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        // Four built-in traits should always be present
        let trait_names: Vec<&str> = registry.traits.values().map(|t| t.name.as_str()).collect();
        assert!(trait_names.contains(&"Displayable"));
        assert!(trait_names.contains(&"Equatable"));
        assert!(trait_names.contains(&"Cloneable"));
        assert!(trait_names.contains(&"Hashable"));
        assert!(trait_names.contains(&"Drop"));
        // Iterator moved to user-space (lib/std/iter.gg); only Iterable remains as a builtin placeholder.
        assert!(trait_names.contains(&"Iterable"));
        assert!(trait_names.contains(&"Measurable"));
    }

    #[test]
    fn iterator_trait_impl() {
        let source = "\
struct Counter:
    int current
    int max

equip Counter with Iterator[int]:
    Option[int] next(&self):
        if self.current >= self.max:
            return None
        int val = self.current
        self.current = self.current + 1
        return Some(val)
";
        let (registry, errors) = analyze(source);
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert!(registry.has_trait_impl_by_name("Counter", "Iterator"));
        // Check that trait_generic_args is populated
        let iter_impl = registry.impls.iter().find(|i| i.trait_name.as_deref() == Some("Iterator")).unwrap();
        assert_eq!(iter_impl.trait_generic_args.len(), 1);
    }

    #[test]
    fn iterable_trait_impl() {
        let source = "\
struct NumberRange:
    int start
    int end_val

struct NumberRangeIter:
    int current
    int end_val

equip NumberRangeIter with Iterator[int]:
    Option[int] next(&self):
        if self.current >= self.end_val:
            return None
        int val = self.current
        self.current = self.current + 1
        return Some(val)

equip NumberRange with Iterable[int]:
    NumberRangeIter iter(&self):
        return NumberRangeIter(self.start, self.end_val)
";
        let (registry, errors) = analyze(source);
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert!(registry.has_trait_impl_by_name("NumberRange", "Iterable"));
        assert!(registry.has_trait_impl_by_name("NumberRangeIter", "Iterator"));
        // Check that trait_generic_args is populated for Iterable
        let iterable_impl = registry.impls.iter()
            .find(|i| i.trait_name.as_deref() == Some("Iterable"))
            .unwrap();
        assert_eq!(iterable_impl.trait_generic_args.len(), 1);
    }

    #[test]
    fn iterable_missing_iter_method() {
        let source = "\
struct MyCollection:
    int size

equip MyCollection with Iterable[int]:
    int count(self):
        return self.size
";
        let (_, errors) = analyze(source);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::MissingTraitMethod { trait_, method, .. }
                if trait_ == "Iterable" && method == "iter"
        )));
    }

    #[test]
    fn iterator_missing_next_method() {
        // Inline trait declaration mirrors lib/std/iter.gg; the test
        // doesn't import the stdlib so we declare the trait locally.
        let source = "\
trait Iterator[T]:
    Option[T] next(&self)

struct Counter:
    int current
    int max

equip Counter with Iterator[int]:
    int count(self):
        return self.current
";
        let (_, errors) = analyze(source);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::MissingTraitMethod { trait_, method, .. }
                if trait_ == "Iterator" && method == "next"
        )));
    }

    #[test]
    fn equip_with_builtin_trait() {
        let source = "\
struct Point:
    float x
    float y

equip Point with Equatable:
    bool eq(self, Point other):
        return self.x == other.x
";
        let (registry, errors) = analyze(source);
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert!(registry.has_trait_impl_by_name("Point", "Equatable"));
        assert!(!registry.has_trait_impl_by_name("Point", "Displayable"));
    }

    #[test]
    fn missing_builtin_trait_method() {
        let source = "\
struct Point:
    float x
    float y

equip Point with Equatable:
    bool wrong_name(self, Point other):
        return true
";
        let (_, errors) = analyze(source);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::MissingTraitMethod { trait_, method, .. }
                if trait_ == "Equatable" && method == "eq"
        )));
    }

    #[test]
    fn default_method_not_required() {
        let source = "\
trait Greeter:
    str name(self)
    str greeting(self):
        return \"hello\"

struct Foo:
    str s

equip Foo with Greeter:
    str name(self):
        return self.s
";
        let (registry, errors) = analyze(source);
        assert!(errors.is_empty(), "default method should not be required: {:?}", errors);
        assert!(registry.has_trait_impl_by_name("Foo", "Greeter"));
    }

    #[test]
    fn trait_inheritance_requires_parent_methods() {
        let source = "\
trait Base:
    int value(self)

trait Child extends Base:
    int extra(self)

struct Foo:
    int x

equip Foo with Child:
    int extra(self):
        return 99
";
        let (_, errors) = analyze(source);
        // Should error about missing `value` from parent trait Base
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::MissingTraitMethod { method, .. }
                if method == "value"
        )), "Should require parent trait methods: {:?}", errors);
    }

    #[test]
    fn trait_inheritance_parent_default_not_required() {
        let source = "\
trait Base:
    int value(self):
        return 0

trait Child extends Base:
    int extra(self)

struct Foo:
    int x

equip Foo with Child:
    int extra(self):
        return 99
";
        let (registry, errors) = analyze(source);
        assert!(errors.is_empty(), "parent default should not be required: {:?}", errors);
        assert!(registry.has_trait_impl_by_name("Foo", "Child"));
    }

    #[test]
    fn via_delegation_skips_missing_method() {
        let source = "\
trait Showable:
    str show(self)

struct Inner:
    int value

equip Inner with Showable:
    str show(self):
        return \"inner\"

struct Outer:
    Inner inner

equip Outer with Showable via inner:
    pass
";
        let (registry, errors) = analyze(source);
        assert!(errors.is_empty(), "via delegation should skip missing method errors: {:?}", errors);
        assert!(registry.has_trait_impl_by_name("Outer", "Showable"));
        let outer_impl = registry.impls.iter().find(|i| i.self_type_name == "Outer").unwrap();
        assert_eq!(outer_impl.via_field.as_deref(), Some("inner"));
    }

    #[test]
    fn via_without_trait_errors() {
        let source = "\
struct Foo:
    int x

equip Foo via x:
    pass
";
        let (_, errors) = analyze(source);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::ViaWithoutTrait
        )), "via without trait should error: {:?}", errors);
    }

    #[test]
    fn via_field_not_found_errors() {
        let source = "\
trait Showable:
    str show(self)

struct Inner:
    int value

equip Inner with Showable:
    str show(self):
        return \"inner\"

struct Outer:
    Inner inner

equip Outer with Showable via nonexistent:
    pass
";
        let (_, errors) = analyze(source);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::ViaFieldNotFound { field, .. } if field == "nonexistent"
        )), "via with nonexistent field should error: {:?}", errors);
    }

    #[test]
    fn via_field_type_missing_trait_errors() {
        let source = "\
trait Showable:
    str show(self)

struct Inner:
    int value

struct Outer:
    Inner inner

equip Outer with Showable via inner:
    pass
";
        let (_, errors) = analyze(source);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::ViaFieldTypeMissingTrait { field, field_type, trait_, .. }
                if field == "inner" && field_type == "Inner" && trait_ == "Showable"
        )), "via with field type not implementing trait should error: {:?}", errors);
    }

    #[test]
    fn orphan_rule_local_type_builtin_trait() {
        // Local type + built-in trait → allowed
        let source = "\
struct MyPoint:
    float x
    float y

equip MyPoint with Equatable:
    bool eq(self, MyPoint other):
        return true
";
        let (_, errors) = analyze(source);
        assert!(
            !errors.iter().any(|e| matches!(&e.kind, SemanticErrorKind::OrphanImpl { .. })),
            "local type with built-in trait should be allowed: {:?}", errors
        );
    }

    #[test]
    fn orphan_rule_local_trait_local_type() {
        // Both local → allowed
        let source = "\
trait MyTrait:
    int value(self)

struct MyStruct:
    int x

equip MyStruct with MyTrait:
    int value(self):
        return self.x
";
        let (_, errors) = analyze(source);
        assert!(
            !errors.iter().any(|e| matches!(&e.kind, SemanticErrorKind::OrphanImpl { .. })),
            "both local should be allowed: {:?}", errors
        );
    }

    #[test]
    fn orphan_rule_rejects_both_foreign() {
        // Built-in type (String is heap-backed, won't be found in scopes) +
        // built-in trait (Displayable) → both foreign → orphan error.
        // (`String` rather than a scalar primitive here: a scalar like `int`
        // now hits the earlier `PrimitiveTraitImpl` reject before the orphan
        // check runs. String is a legitimate equip target — heap-backed, not
        // a scalar — so it still reaches and exercises the orphan rule.)
        let source = "\
equip String with Displayable:
    String display(self):
        return \"x\"
";
        let (_, errors) = analyze(source);
        // `type_` renders as "stringtype" — the lowercased Debug of
        // `PrimitiveType::StringType` (a pre-existing `type_name` rendering
        // quirk, orthogonal to this test). The orphan rule firing on a
        // both-foreign equip is what's under test.
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                SemanticErrorKind::OrphanImpl { trait_, type_ }
                    if trait_ == "Displayable" && type_ == "stringtype"
            )),
            "both foreign should be rejected: {:?}", errors
        );
    }

    #[test]
    fn orphan_rule_local_trait_foreign_type() {
        // Local trait + built-in type → allowed.
        // (`String` rather than a scalar primitive: a scalar `int` target now
        // hits the `PrimitiveTraitImpl` reject, which would make this test
        // "pass" over a program that is actually rejected — a misleading
        // fossil. String is a legitimate foreign non-scalar equip target, so
        // the orphan rule (local trait ⇒ no orphan violation) is still what's
        // under test.)
        let source = "\
trait MyTrait:
    String value(self)

equip String with MyTrait:
    String value(self):
        return \"x\"
";
        let (_, errors) = analyze(source);
        assert!(
            !errors.iter().any(|e| matches!(&e.kind, SemanticErrorKind::OrphanImpl { .. })),
            "local trait with foreign type should be allowed: {:?}", errors
        );
    }

    #[test]
    fn numeric_intrinsic_satisfaction() {
        let (registry, _) = analyze("");
        // Numeric primitives satisfy Numeric and its components
        assert!(registry.has_trait_impl_by_name("int", "Numeric"));
        assert!(registry.has_trait_impl_by_name("float", "Numeric"));
        assert!(registry.has_trait_impl_by_name("int", "Add"));
        assert!(registry.has_trait_impl_by_name("float", "Sub"));
        assert!(registry.has_trait_impl_by_name("int", "Comparable"));
        assert!(registry.has_trait_impl_by_name("float", "Default"));
        assert!(registry.has_trait_impl_by_name("int", "One"));
        assert!(registry.has_trait_impl_by_name("int8", "Numeric"));
        assert!(registry.has_trait_impl_by_name("uint64", "Numeric"));
        assert!(registry.has_trait_impl_by_name("float32", "Numeric"));
        // Non-numeric types do NOT satisfy Numeric
        assert!(!registry.has_trait_impl_by_name("str", "Numeric"));
        assert!(!registry.has_trait_impl_by_name("bool", "Numeric"));
        assert!(!registry.has_trait_impl_by_name("char", "Numeric"));
    }

    #[test]
    fn numeric_extends_has_9_parents() {
        let (registry, _) = analyze("");
        let numeric_info = registry.traits.values()
            .find(|t| t.name == "Numeric")
            .expect("Numeric trait not found");
        assert_eq!(numeric_info.extends.len(), 10,
            "Numeric should extend 10 traits (Add, Sub, Mul, Div, Rem, Mod, Neg, Comparable, Default, One)");
    }

    #[test]
    fn method_return_type_mismatch() {
        let source = "\
trait Drawable:
    void draw(self)

struct Circle:
    float radius

equip Circle with Drawable:
    int draw(self):
        return 42
";
        let (_, errors) = analyze(source);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::MethodSignatureMismatch { method, .. } if method == "draw"
        )), "expected MethodSignatureMismatch for draw, got: {:?}", errors);
    }

    #[test]
    fn method_param_count_mismatch() {
        let source = "\
trait Transformer:
    int transform(self, int x)

struct Wrapper:
    int val

equip Wrapper with Transformer:
    int transform(self):
        return 0
";
        let (_, errors) = analyze(source);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::MethodSignatureMismatch { method, .. } if method == "transform"
        )), "expected MethodSignatureMismatch for transform, got: {:?}", errors);
    }

    #[test]
    fn method_signature_correct_no_error() {
        let source = "\
trait Drawable:
    void draw(self)

struct Circle:
    float radius

equip Circle with Drawable:
    void draw(self):
        pass
";
        let (_, errors) = analyze(source);
        assert!(!errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::MethodSignatureMismatch { .. }
        )), "correct signature should produce no mismatch errors: {:?}", errors);
    }

    #[test]
    fn split_inherent_equip_blocks_ok() {
        // Rust-style: multiple `equip T:` blocks with DIFFERENT
        // methods just accumulate. No error.
        let source = "\
struct Point:
    float x

equip Point:
    float get_x(self):
        return self.x

equip Point:
    float get_y(self):
        return 0.0
";
        let (_, errors) = analyze(source);
        assert!(!errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::DuplicateImpl { .. }
        )), "splitting inherent equip blocks across declarations is fine, got: {:?}", errors);
    }

    #[test]
    fn duplicate_inherent_method_errors() {
        // Same method name in two `equip T:` blocks → error.
        let source = "\
struct Point:
    float x

equip Point:
    float get_x(self):
        return self.x

equip Point:
    float get_x(self):
        return 0.0
";
        let (_, errors) = analyze(source);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::DuplicateImpl { type_, .. } if type_ == "Point"
        )), "expected DuplicateImpl for duplicate method in split equip blocks, got: {:?}", errors);
    }

    #[test]
    fn duplicate_trait_equip_errors() {
        let source = "\
trait Drawable:
    void draw(self)

struct Circle:
    float radius

equip Circle with Drawable:
    void draw(self):
        pass

equip Circle with Drawable:
    void draw(self):
        pass
";
        let (_, errors) = analyze(source);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::DuplicateImpl { trait_, type_ }
                if trait_ == "Drawable" && type_ == "Circle"
        )), "expected DuplicateImpl for duplicate trait equip, got: {:?}", errors);
    }

    #[test]
    fn trait_cycle_detected() {
        let source = "\
trait A extends B:
    void a(self)

trait B extends A:
    void b(self)
";
        let (_, errors) = analyze(source);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::TraitCycle { .. }
        )), "expected TraitCycle, got: {:?}", errors);
    }

    #[test]
    fn trait_self_cycle_detected() {
        let source = "\
trait A extends A:
    void a(self)
";
        let (_, errors) = analyze(source);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::TraitCycle { .. }
        )), "expected TraitCycle for self-extension, got: {:?}", errors);
    }

    #[test]
    fn trait_no_cycle_no_error() {
        let source = "\
trait Base:
    void base(self)

trait Child extends Base:
    void child(self)
";
        let (_, errors) = analyze(source);
        assert!(!errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::TraitCycle { .. }
        )), "correct inheritance should produce no cycle errors: {:?}", errors);
    }

    fn ident(s: &str) -> Type {
        Type::Named {
            name: Spanned { node: s.to_string(), span: Span { start: 0, end: 0 } },
            generic_args: vec![],
        }
    }
    fn spanned<T>(node: T) -> Spanned<T> {
        Spanned { node, span: Span { start: 0, end: 0 } }
    }

    #[test]
    fn substitute_ast_type_replaces_named_generics() {
        let mut bindings = FxHashMap::default();
        bindings.insert("T".to_string(), Type::Primitive(crate::parser::ast::PrimitiveType::Int));
        bindings.insert("U".to_string(), ident("String"));
        // T → int
        assert!(matches!(
            substitute_ast_type(&ident("T"), &bindings),
            Type::Primitive(crate::parser::ast::PrimitiveType::Int)
        ));
        // Vector[T] → Vector[int]
        let v_t = Type::Named {
            name: spanned("Vector".to_string()),
            generic_args: vec![spanned(ident("T"))],
        };
        match substitute_ast_type(&v_t, &bindings) {
            Type::Named { name, generic_args } => {
                assert_eq!(name.node, "Vector");
                assert_eq!(generic_args.len(), 1);
                assert!(matches!(generic_args[0].node,
                    Type::Primitive(crate::parser::ast::PrimitiveType::Int)));
            }
            _ => panic!("expected Named"),
        }
        // (T, U) → (int, String)
        let tup = Type::Tuple(vec![spanned(ident("T")), spanned(ident("U"))]);
        match substitute_ast_type(&tup, &bindings) {
            Type::Tuple(elems) => {
                assert_eq!(elems.len(), 2);
                assert!(matches!(elems[0].node,
                    Type::Primitive(crate::parser::ast::PrimitiveType::Int)));
                assert!(matches!(&elems[1].node,
                    Type::Named { name, .. } if name.node == "String"));
            }
            _ => panic!("expected Tuple"),
        }
        // Function: U(T) → String(int)
        let fn_t = Type::Function {
            return_type: Box::new(spanned(ident("U"))),
            params: vec![spanned(ident("T"))],
            param_ownerships: vec![Ownership::Borrow],
        };
        match substitute_ast_type(&fn_t, &bindings) {
            Type::Function { return_type, params, .. } => {
                assert!(matches!(&return_type.node,
                    Type::Named { name, .. } if name.node == "String"));
                assert!(matches!(&params[0].node,
                    Type::Primitive(crate::parser::ast::PrimitiveType::Int)));
            }
            _ => panic!("expected Function"),
        }
    }

    #[test]
    fn substitute_ast_type_leaves_unbound_alone() {
        let bindings: FxHashMap<String, Type> = FxHashMap::default();
        // T stays T (no binding)
        assert!(matches!(substitute_ast_type(&ident("T"), &bindings),
            Type::Named { name, .. } if name.node == "T"));
        // Generic args with no bindings: structure preserved.
        let v_t = Type::Named {
            name: spanned("Vector".to_string()),
            generic_args: vec![spanned(ident("T"))],
        };
        match substitute_ast_type(&v_t, &bindings) {
            Type::Named { name, generic_args } => {
                assert_eq!(name.node, "Vector");
                assert!(matches!(&generic_args[0].node,
                    Type::Named { name, .. } if name.node == "T"));
            }
            _ => panic!(),
        }
    }

    #[test]
    fn build_method_sig_shape_extracts_generic_method() {
        let source = "\
trait Iter[T]:
    bool any[F](&self, F pred):
        return false
";
        let (registry, errors) = analyze(source);
        assert!(errors.is_empty(), "errors: {:?}", errors);
        let trait_info = registry.traits.values()
            .find(|t| t.name == "Iter")
            .expect("Iter trait registered");
        let shape = trait_info.method_shapes.get("any")
            .expect("any has a shape (method-level generic)");
        assert_eq!(shape.generic_params, vec!["F".to_string()]);
        assert_eq!(shape.param_types.len(), 1);
        assert!(matches!(&shape.param_types[0],
            Type::Named { name, .. } if name.node == "F"));
        assert!(matches!(&shape.return_type, Type::Primitive(_)));
    }

    #[test]
    fn build_method_sig_shape_skips_non_generic_method() {
        let source = "\
trait Plain:
    void noop(self)
";
        let (registry, errors) = analyze(source);
        assert!(errors.is_empty(), "errors: {:?}", errors);
        let trait_info = registry.traits.values()
            .find(|t| t.name == "Plain")
            .expect("Plain trait registered");
        assert!(trait_info.method_shapes.get("noop").is_none(),
            "non-generic methods should not get shapes");
    }
}
