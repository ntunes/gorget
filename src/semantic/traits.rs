use rustc_hash::{FxHashMap, FxHashSet};

use crate::parser::ast::*;
use crate::span::Span;

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

/// Information about a trait definition.
#[derive(Debug, Clone)]
pub struct TraitInfo {
    pub def_id: DefId,
    pub name: String,
    pub methods: FxHashMap<String, FunctionSig>,
    pub has_default_body: FxHashMap<String, bool>,
    pub extends: Vec<DefId>,
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
}

/// Registry of all traits and implementations.
pub struct TraitRegistry {
    pub traits: FxHashMap<DefId, TraitInfo>,
    pub impls: Vec<EquipInfo>,
    /// type -> indices into impls for inherent impls
    pub inherent_impls: FxHashMap<TypeId, Vec<usize>>,
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

        // Check trait impls
        for impl_info in &self.impls {
            if impl_info.self_type == type_id && impl_info.trait_.is_some() {
                if let Some((def_id, sig)) = impl_info.methods.get(method) {
                    return Some((def_id, sig));
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
        None
    }

    /// Check if a type (by name) has any impl registered.
    pub fn has_any_impl_by_name(&self, type_name: &str) -> bool {
        self.impls.iter().any(|impl_info| impl_info.self_type_name == type_name)
    }

    /// Check if a type has ONLY inherent impls (no trait impls, no via delegation).
    /// Types with trait impls may have default or via-forwarded methods that aren't
    /// in the equip's methods map, so we can't reliably detect missing methods.
    pub fn has_inherent_only_impls(&self, type_name: &str) -> bool {
        let mut has_inherent = false;
        for impl_info in &self.impls {
            if impl_info.self_type_name == type_name {
                if impl_info.trait_.is_some() || impl_info.via_field.is_some() {
                    return false; // has trait impl or via delegation
                }
                has_inherent = true;
            }
        }
        has_inherent
    }

    /// Check if a type (by name) has an implementation for a trait (by name).
    pub fn has_trait_impl_by_name(&self, type_name: &str, trait_name: &str) -> bool {
        if self.impls.iter().any(|impl_info| {
            impl_info.self_type_name == type_name
                && impl_info.trait_name.as_deref() == Some(trait_name)
        }) {
            return true;
        }
        // Intrinsic satisfaction: numeric primitives satisfy numeric traits.
        if is_numeric_primitive(type_name) && is_numeric_trait(trait_name) {
            return true;
        }
        // Intrinsic satisfaction: hashable/equatable primitives.
        is_hashable_primitive(type_name) && is_hashable_trait(trait_name)
    }

    /// Get the trait's generic AST type args for a specific trait impl on a type (by name).
    /// Returns the first matching impl's trait_generic_args.
    pub fn trait_generic_args_by_name(&self, type_name: &str, trait_name: &str) -> &[Type] {
        for impl_info in &self.impls {
            if impl_info.self_type_name == type_name
                && impl_info.trait_name.as_deref() == Some(trait_name)
            {
                return &impl_info.trait_generic_args;
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
        self.impls.iter().any(|impl_info| {
            impl_info.self_type_name == type_name
                && impl_info.methods.contains_key(method_name)
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

    // First pass: collect all trait definitions
    for item in &module.items {
        if let Item::Trait(trait_def) = &item.node {
            collect_trait(trait_def, scopes, types, &mut registry, errors);
        }
    }

    // Second pass: process all impl blocks
    for item in &module.items {
        if let Item::Equip(impl_block) = &item.node {
            process_impl(impl_block, scopes, types, &mut registry, errors);
        }
    }

    // Third pass: detect trait inheritance cycles (before validate_trait_impls to avoid stack overflow)
    validate_trait_cycles(&registry, errors);

    // Fourth pass: validate trait impls (check all required methods are present)
    validate_trait_impls(&registry, module, types, errors);

    registry
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
        // Hashable: int hash(self)
        ("Hashable", {
            let mut m = FxHashMap::default();
            m.insert("hash".into(), FunctionSig {
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
        // Iterator[T]: Option[T] next(&self)  — &self parses as MutableBorrow
        ("Iterator", {
            let mut m = FxHashMap::default();
            m.insert("next".into(), FunctionSig {
                params: vec![],
                return_type: types.error_id, // placeholder — Option[T] depends on concrete T
                has_self: true,
                self_ownership: Some(Ownership::MutableBorrow),
            });
            m
        }),
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
        if let Some(def_id) = scopes.lookup(name) {
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
            });
        }
    }

    // Wire Numeric's extends: Add + Sub + Mul + Div + Rem + Neg + Comparable + Default + One
    if let Some(numeric_def_id) = scopes.lookup("Numeric") {
        let parent_names = ["Add", "Sub", "Mul", "Div", "Rem", "Mod", "Neg", "Comparable", "Default", "One"];
        let parent_ids: Vec<DefId> = parent_names.iter()
            .filter_map(|name| scopes.lookup(name))
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
    let Some(def_id) = scopes.lookup(&trait_def.name.node) else {
        return;
    };

    let mut methods = FxHashMap::default();
    let mut has_default_body = FxHashMap::default();

    for item in &trait_def.items {
        if let TraitItem::Method(method) = &item.node {
            validate_default_param_ordering(&method.params, errors);
            let sig = build_function_sig(method, scopes, types);
            let has_body = !matches!(method.body, FunctionBody::Declaration | FunctionBody::Extern(_));
            has_default_body.insert(method.name.node.clone(), has_body);
            methods.insert(method.name.node.clone(), sig);
        }
    }

    // Resolve extends
    let mut extends = Vec::new();
    for bound in &trait_def.extends {
        if let Some(parent_id) = scopes.lookup(&bound.node.name.node) {
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
        },
    );
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

    // Resolve the trait (if any)
    let trait_def_id = impl_block.trait_.as_ref().and_then(|t| {
        if let Type::Named { name, .. } = &t.trait_name.node {
            scopes.lookup(&name.node)
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
    for method in &impl_block.items {
        let method_def_id = scopes.lookup_def_by_span(&method.node.name.node, method.node.name.span);
        let sig = build_function_sig(&method.node, scopes, types);
        let def_id = method_def_id.unwrap_or(DefId(0));
        methods.insert(method.node.name.node.clone(), (def_id, sig));
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
    });

    if let Some(trait_id) = trait_def_id {
        registry.trait_impls.insert((trait_id, self_type_id, trait_arg_type_ids), impl_idx);
    } else {
        // Duplicate inherent equip check
        if registry.inherent_impls.contains_key(&self_type_id) {
            errors.push(SemanticError {
                kind: SemanticErrorKind::DuplicateImpl {
                    trait_: "(inherent)".to_string(),
                    type_: self_type_name.clone(),
                },
                span: impl_block.span,
            });
            return;
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

            // Return type — skip if trait uses error_id as a placeholder (e.g. Self or Option[T])
            if trait_sig.return_type != types.error_id
                && trait_sig.return_type != impl_sig.return_type
            {
                errors.push(SemanticError {
                    kind: SemanticErrorKind::MethodSignatureMismatch {
                        trait_: source_trait_name.clone(),
                        method: method_name.clone(),
                        detail: format!(
                            "return type is `{}`, expected `{}`",
                            types.display(impl_sig.return_type),
                            types.display(trait_sig.return_type),
                        ),
                    },
                    span: impl_info.span,
                });
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
                    if *trait_param == types.error_id {
                        continue;
                    }
                    if trait_param != impl_param {
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::MethodSignatureMismatch {
                                trait_: source_trait_name.clone(),
                                method: method_name.clone(),
                                detail: format!(
                                    "parameter {} type is `{}`, expected `{}`",
                                    i + 1,
                                    types.display(*impl_param),
                                    types.display(*trait_param),
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
        // 24 built-in traits + 1 user-defined trait
        assert_eq!(registry.traits.len(), 25);
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
        assert!(trait_names.contains(&"Iterator"));
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
        let source = "\
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
        // Built-in type (int is a primitive, won't be found in scopes) +
        // built-in trait (Displayable) → both foreign → orphan error
        let source = "\
equip int with Displayable:
    String display(self):
        return \"x\"
";
        let (_, errors) = analyze(source);
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                SemanticErrorKind::OrphanImpl { trait_, type_ }
                    if trait_ == "Displayable" && type_ == "int"
            )),
            "both foreign should be rejected: {:?}", errors
        );
    }

    #[test]
    fn orphan_rule_local_trait_foreign_type() {
        // Local trait + built-in type → allowed
        let source = "\
trait MyTrait:
    int value(self)

equip int with MyTrait:
    int value(self):
        return 42
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
    fn duplicate_inherent_equip_errors() {
        let source = "\
struct Point:
    float x

equip Point:
    float x(self):
        return self.x

equip Point:
    float y(self):
        return 0.0
";
        let (_, errors) = analyze(source);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::DuplicateImpl { type_, .. } if type_ == "Point"
        )), "expected DuplicateImpl for inherent equip, got: {:?}", errors);
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
}
