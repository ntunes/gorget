use rustc_hash::FxHashMap;

use crate::parser::ast::*;
use crate::span::Span;

use super::errors::{SemanticError, SemanticErrorKind};
use super::ids::{DefId, TypeId};
use super::resolve::ResolutionMap;
use super::scope::ScopeTable;
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
    /// (trait DefId, type TypeId) -> index into impls
    pub trait_impls: FxHashMap<(DefId, TypeId), usize>,
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

    /// Check if a type (by name) has an implementation for a trait (by name).
    pub fn has_trait_impl_by_name(&self, type_name: &str, trait_name: &str) -> bool {
        self.impls.iter().any(|impl_info| {
            impl_info.self_type_name == type_name
                && impl_info.trait_name.as_deref() == Some(trait_name)
        })
    }
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

    // Third pass: validate trait impls (check all required methods are present)
    validate_trait_impls(&registry, module, errors);

    registry
}

/// Register the four built-in core traits (Displayable, Equatable, Cloneable, Hashable).
fn register_builtin_traits(
    scopes: &ScopeTable,
    types: &TypeTable,
    registry: &mut TraitRegistry,
) {
    let builtin_traits: Vec<(&str, FxHashMap<String, FunctionSig>)> = vec![
        // Displayable: str display(self)
        ("Displayable", {
            let mut m = FxHashMap::default();
            m.insert("display".into(), FunctionSig {
                params: vec![],
                return_type: types.string_id,
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
        // Drop: void drop(self)
        ("Drop", {
            let mut m = FxHashMap::default();
            m.insert("drop".into(), FunctionSig {
                params: vec![],
                return_type: types.void_id,
                has_self: true,
                self_ownership: None,
            });
            m
        }),
        // Iterator[T]: Option[T] next(&self)
        ("Iterator", {
            let mut m = FxHashMap::default();
            m.insert("next".into(), FunctionSig {
                params: vec![],
                return_type: types.error_id, // placeholder — Option[T] depends on concrete T
                has_self: true,
                self_ownership: Some(Ownership::Borrow),
            });
            m
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
}

fn collect_trait(
    trait_def: &TraitDef,
    scopes: &ScopeTable,
    types: &mut TypeTable,
    registry: &mut TraitRegistry,
    _errors: &mut Vec<SemanticError>,
) {
    let Some(def_id) = scopes.lookup(&trait_def.name.node) else {
        return;
    };

    let mut methods = FxHashMap::default();
    let mut has_default_body = FxHashMap::default();

    for item in &trait_def.items {
        if let TraitItem::Method(method) = &item.node {
            let sig = build_function_sig(method, scopes, types);
            let has_body = !matches!(method.body, FunctionBody::Declaration);
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

    // Check for duplicate trait impl
    if let Some(trait_id) = trait_def_id {
        if registry.trait_impls.contains_key(&(trait_id, self_type_id)) {
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

    // Collect methods
    let mut methods = FxHashMap::default();
    for method in &impl_block.items {
        let method_def_id = scopes.lookup(&method.node.name.node);
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
        registry.trait_impls.insert((trait_id, self_type_id), impl_idx);
    } else {
        registry
            .inherent_impls
            .entry(self_type_id)
            .or_default()
            .push(impl_idx);
    }
}

fn validate_trait_impls(registry: &TraitRegistry, module: &Module, errors: &mut Vec<SemanticError>) {
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

/// Collect all methods required by a trait, including inherited parent methods.
/// Returns (method_name, has_default, source_trait_name) tuples.
fn collect_all_required_methods(
    trait_info: &TraitInfo,
    registry: &TraitRegistry,
) -> Vec<(String, bool, String)> {
    let mut methods = Vec::new();

    // Recursively collect parent trait methods
    for &parent_id in &trait_info.extends {
        if let Some(parent_info) = registry.traits.get(&parent_id) {
            methods.extend(collect_all_required_methods(parent_info, registry));
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
    let return_type = types::ast_type_to_resolved(
        &func.return_type.node,
        func.return_type.span,
        scopes,
        types,
    )
    .unwrap_or(types.error_id);

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
        resolve::collect_top_level(&module, &mut scopes, &mut types, &mut errors);
        let resolution_map = resolve::resolve_bodies(&module, &mut scopes, &mut types, &mut errors);
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
        // 6 built-in traits + 1 user-defined trait
        assert_eq!(registry.traits.len(), 7);
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
}
