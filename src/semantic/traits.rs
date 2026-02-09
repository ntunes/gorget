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

/// Information about an impl block.
#[derive(Debug, Clone)]
pub struct ImplInfo {
    pub self_type: TypeId,
    pub self_type_name: String,
    pub trait_: Option<DefId>,
    pub trait_name: Option<String>,
    pub methods: FxHashMap<String, (DefId, FunctionSig)>,
    pub span: Span,
}

/// Registry of all traits and implementations.
pub struct TraitRegistry {
    pub traits: FxHashMap<DefId, TraitInfo>,
    pub impls: Vec<ImplInfo>,
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

    // First pass: collect all trait definitions
    for item in &module.items {
        if let Item::Trait(trait_def) = &item.node {
            collect_trait(trait_def, scopes, types, &mut registry, errors);
        }
    }

    // Second pass: process all impl blocks
    for item in &module.items {
        if let Item::Implement(impl_block) = &item.node {
            process_impl(impl_block, scopes, types, &mut registry, errors);
        }
    }

    // Third pass: validate trait impls (check all required methods are present)
    validate_trait_impls(&registry, errors);

    registry
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
    impl_block: &ImplBlock,
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

    let impl_idx = registry.impls.len();
    registry.impls.push(ImplInfo {
        self_type: self_type_id,
        self_type_name: self_type_name.clone(),
        trait_: trait_def_id,
        trait_name,
        methods,
        span: impl_block.span,
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

fn validate_trait_impls(registry: &TraitRegistry, errors: &mut Vec<SemanticError>) {
    for impl_info in &registry.impls {
        let Some(trait_def_id) = impl_info.trait_ else {
            continue;
        };
        let Some(trait_info) = registry.traits.get(&trait_def_id) else {
            continue;
        };

        // Check that all required methods are implemented
        for (method_name, _sig) in &trait_info.methods {
            let has_default = trait_info
                .has_default_body
                .get(method_name)
                .copied()
                .unwrap_or(false);
            if !has_default && !impl_info.methods.contains_key(method_name) {
                errors.push(SemanticError {
                    kind: SemanticErrorKind::MissingTraitMethod {
                        trait_: trait_info.name.clone(),
                        method: method_name.clone(),
                        type_: impl_info.self_type_name.clone(),
                    },
                    span: impl_info.span,
                });
            }
        }
    }
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

implement Point:
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

implement Drawable for Circle:
    void draw(self):
        pass
";
        let (registry, errors) = analyze(source);
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(registry.traits.len(), 1);
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

implement Drawable for Circle:
    void draw(self):
        pass
";
        let (_, errors) = analyze(source);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::MissingTraitMethod { method, .. } if method == "area"
        )));
    }
}
