use rustc_hash::FxHashMap;

use crate::span::Span;

use super::errors::{SemanticError, SemanticErrorKind};
use super::ids::{DefId, ScopeId, TypeId};

/// What a name resolves to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefKind {
    Function,
    Struct,
    Enum,
    Variant,
    Trait,
    TypeAlias,
    Newtype,
    Variable,
    Const,
    Static,
    GenericParam,
    Import,
}

/// Metadata for a definition.
#[derive(Debug, Clone)]
pub struct DefInfo {
    pub name: String,
    pub kind: DefKind,
    pub span: Span,
    pub scope: ScopeId,
    pub type_id: Option<TypeId>,
    /// Whether this variable was declared with `mutable` (only meaningful for Variable kind).
    pub is_mutable: bool,
    /// Whether this variable is a function parameter (borrowed from caller, safe to re-bind).
    pub is_param: bool,
}

/// A lexical scope.
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub kind: ScopeKind,
    pub names: FxHashMap<String, DefId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScopeKind {
    Module,
    /// A file-based module scope (created for each non-entry `Item::Module` wrapper).
    /// Its names are exported to the parent `Module` scope after collection,
    /// except for items explicitly marked `private`.
    FileModule { path: Vec<String> },
    Function,
    Block,
    EquipBlock { self_type: Option<TypeId> },
    TraitDef,
    ForLoop,
}

/// The scope tree manager.
pub struct ScopeTable {
    scopes: Vec<Scope>,
    definitions: Vec<DefInfo>,
    current: ScopeId,
}

impl ScopeTable {
    pub fn new() -> Self {
        let root = Scope {
            parent: None,
            kind: ScopeKind::Module,
            names: FxHashMap::default(),
        };
        Self {
            scopes: vec![root],
            definitions: Vec::new(),
            current: ScopeId(0),
        }
    }

    /// Create a child scope and set it as current.
    pub fn push_scope(&mut self, kind: ScopeKind) -> ScopeId {
        let id = ScopeId(self.scopes.len() as u32);
        self.scopes.push(Scope {
            parent: Some(self.current),
            kind,
            names: FxHashMap::default(),
        });
        self.current = id;
        id
    }

    /// Return to parent scope.
    pub fn pop_scope(&mut self) {
        if let Some(parent) = self.scopes[self.current.0 as usize].parent {
            self.current = parent;
        }
    }

    /// Add a definition to the current scope. Returns error on duplicate.
    /// An actual definition (function, struct, etc.) may replace a prior `Import` placeholder.
    pub fn define(
        &mut self,
        name: String,
        kind: DefKind,
        span: Span,
    ) -> Result<DefId, SemanticError> {
        self.define_with_mutability(name, kind, span, false)
    }

    /// Allocate a DefId and DefInfo for a name without inserting it into scope's name map.
    /// Used for user-defined enum variants that should only be accessible via qualified paths.
    pub fn alloc_def(&mut self, name: String, kind: DefKind, span: Span) -> DefId {
        let def_id = DefId(self.definitions.len() as u32);
        self.definitions.push(DefInfo {
            name,
            kind,
            span,
            scope: self.current,
            type_id: None,
            is_mutable: false,
            is_param: false,
        });
        def_id
    }

    /// Add a definition with an explicit mutability flag.
    pub fn define_with_mutability(
        &mut self,
        name: String,
        kind: DefKind,
        span: Span,
        is_mutable: bool,
    ) -> Result<DefId, SemanticError> {
        let scope = &self.scopes[self.current.0 as usize];
        if let Some(&existing_id) = scope.names.get(&name) {
            let existing = &self.definitions[existing_id.0 as usize];
            // Allow a real definition to replace an import placeholder,
            // a real import to replace a built-in placeholder (dummy span),
            // a user definition to shadow a built-in trait (dummy span),
            // or a user-defined variant to shadow a built-in prelude variant (dummy span).
            if (existing.kind == DefKind::Import && kind != DefKind::Import)
                || (existing.kind == DefKind::Import && existing.span == Span::dummy())
                || (existing.kind == DefKind::Trait && existing.span == Span::dummy() && kind != DefKind::Trait)
                || (existing.kind == DefKind::Variant && existing.span == Span::dummy() && kind == DefKind::Variant)
            {
                let def_id = DefId(self.definitions.len() as u32);
                self.definitions.push(DefInfo {
                    name: name.clone(),
                    kind,
                    span,
                    scope: self.current,
                    type_id: None,
                    is_mutable,
                    is_param: false,
                });
                self.scopes[self.current.0 as usize]
                    .names
                    .insert(name, def_id);
                return Ok(def_id);
            }
            let original_span = existing.span;
            return Err(SemanticError {
                kind: SemanticErrorKind::DuplicateDefinition {
                    name,
                    original: original_span,
                },
                span,
            });
        }

        let def_id = DefId(self.definitions.len() as u32);
        self.definitions.push(DefInfo {
            name: name.clone(),
            kind,
            span,
            scope: self.current,
            type_id: None,
            is_mutable,
            is_param: false,
        });
        self.scopes[self.current.0 as usize]
            .names
            .insert(name, def_id);
        Ok(def_id)
    }

    /// Look up a name, walking the parent chain.
    pub fn lookup(&self, name: &str) -> Option<DefId> {
        let mut scope_id = Some(self.current);
        while let Some(sid) = scope_id {
            let scope = &self.scopes[sid.0 as usize];
            if let Some(&def_id) = scope.names.get(name) {
                return Some(def_id);
            }
            scope_id = scope.parent;
        }
        None
    }

    /// Look up a name starting from a given scope, walking the parent chain.
    pub fn lookup_from_scope(&self, scope_id: ScopeId, name: &str) -> Option<DefId> {
        let mut sid = Some(scope_id);
        while let Some(s) = sid {
            let scope = &self.scopes[s.0 as usize];
            if let Some(&def_id) = scope.names.get(name) {
                return Some(def_id);
            }
            sid = scope.parent;
        }
        None
    }

    /// Look up a name within a function scope tree: searches the scope itself,
    /// all descendant scopes, and all ancestor scopes. Returns the most recent
    /// definition (highest DefId) whose scope is within the function tree.
    pub fn lookup_within_function(&self, fn_scope_id: ScopeId, name: &str) -> Option<DefId> {
        let mut best: Option<DefId> = None;
        for (i, def) in self.definitions.iter().enumerate() {
            if def.name == name
                && matches!(
                    def.kind,
                    DefKind::Variable | DefKind::Const | DefKind::Function
                )
                && self.is_descendant_of(def.scope, fn_scope_id)
            {
                best = Some(DefId(i as u32));
            }
        }
        // Also check ancestors (module scope, etc.)
        if best.is_none() {
            best = self.lookup_from_scope(fn_scope_id, name);
        }
        best
    }

    /// Check if `child` is `parent` or a descendant of `parent`.
    fn is_descendant_of(&self, child: ScopeId, parent: ScopeId) -> bool {
        let mut sid = Some(child);
        while let Some(s) = sid {
            if s == parent {
                return true;
            }
            sid = self.scopes[s.0 as usize].parent;
        }
        false
    }

    /// Look in a specific scope only (no parent chain walk).
    pub fn lookup_in_scope(&self, scope_id: ScopeId, name: &str) -> Option<DefId> {
        self.scopes[scope_id.0 as usize].names.get(name).copied()
    }

    pub fn get_def(&self, id: DefId) -> &DefInfo {
        &self.definitions[id.0 as usize]
    }

    pub fn get_def_mut(&mut self, id: DefId) -> &mut DefInfo {
        &mut self.definitions[id.0 as usize]
    }

    pub fn current_scope(&self) -> ScopeId {
        self.current
    }

    pub fn scope_kind(&self, id: ScopeId) -> &ScopeKind {
        &self.scopes[id.0 as usize].kind
    }

    /// Walk up the scope chain to find the nearest function scope.
    pub fn enclosing_function_scope(&self) -> Option<ScopeId> {
        let mut scope_id = Some(self.current);
        while let Some(sid) = scope_id {
            if self.scopes[sid.0 as usize].kind == ScopeKind::Function {
                return Some(sid);
            }
            scope_id = self.scopes[sid.0 as usize].parent;
        }
        None
    }

    /// Check if we are inside a loop.
    pub fn is_in_loop(&self) -> bool {
        let mut scope_id = Some(self.current);
        while let Some(sid) = scope_id {
            match &self.scopes[sid.0 as usize].kind {
                ScopeKind::ForLoop => return true,
                ScopeKind::Function => return false, // stop at function boundary
                _ => {}
            }
            scope_id = self.scopes[sid.0 as usize].parent;
        }
        false
    }

    /// Set the current scope directly (used for re-entering a scope during type checking).
    pub fn set_current(&mut self, id: ScopeId) {
        self.current = id;
    }

    pub fn def_count(&self) -> usize {
        self.definitions.len()
    }

    pub fn scope_count(&self) -> usize {
        self.scopes.len()
    }

    pub fn scope_parent(&self, id: ScopeId) -> Option<ScopeId> {
        self.scopes[id.0 as usize].parent
    }

    /// Look up a definition by name and definition span. This is reliable even with
    /// shadowing because each definition has a unique (name, span) pair.
    pub fn lookup_def_by_span(&self, name: &str, span: Span) -> Option<DefId> {
        for (i, def) in self.definitions.iter().enumerate() {
            if def.name == name && def.span == span {
                return Some(DefId(i as u32));
            }
        }
        None
    }

    /// Check if a name refers to a global definition (function, enum variant, struct, etc.)
    /// that doesn't need to be captured by closures.
    pub fn is_global_def(&self, name: &str) -> bool {
        for def in self.definitions.iter().rev() {
            if def.name == name {
                return matches!(
                    def.kind,
                    DefKind::Function
                        | DefKind::Variant
                        | DefKind::Enum
                        | DefKind::Struct
                        | DefKind::Newtype
                        | DefKind::Trait
                );
            }
        }
        false
    }

    /// Return all `(name, DefId)` pairs defined directly in the current scope.
    pub fn names_in_current_scope(&self) -> Vec<(String, DefId)> {
        self.scopes[self.current.0 as usize]
            .names
            .iter()
            .map(|(n, d)| (n.clone(), *d))
            .collect()
    }

    /// Copy all non-private names from the current scope into the parent scope.
    ///
    /// Called after collecting a `FileModule` scope's items to make public items
    /// accessible from the enclosing (global Module) scope.
    pub fn export_non_private(&mut self, private_names: &rustc_hash::FxHashSet<String>) {
        let current_idx = self.current.0 as usize;
        let parent_idx = match self.scopes[current_idx].parent {
            Some(p) => p.0 as usize,
            None => return, // root scope — nothing to export to
        };

        let names: Vec<(String, DefId)> = self.scopes[current_idx]
            .names
            .iter()
            .filter(|(name, _)| !private_names.contains(name.as_str()))
            .map(|(n, d)| (n.clone(), *d))
            .collect();

        for (name, def_id) in names {
            // Allow real definitions to replace Import placeholders or dummy entries.
            // Import placeholders are created in the parent scope by `from X import Y`
            // in the entry module — the actual definition from the FileModule scope must
            // take over. Don't overwrite a real (non-Import) definition.
            let existing = self.scopes[parent_idx].names.get(&name).copied();
            let should_insert = match existing {
                None => true,
                Some(existing_id) => {
                    let existing_def = &self.definitions[existing_id.0 as usize];
                    existing_def.kind == DefKind::Import
                        || existing_def.span == crate::span::Span::dummy()
                }
            };
            if should_insert {
                self.scopes[parent_idx].names.insert(name, def_id);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn push_pop_scope() {
        let mut table = ScopeTable::new();
        assert_eq!(table.current_scope(), ScopeId(0));

        let child = table.push_scope(ScopeKind::Function);
        assert_eq!(child, ScopeId(1));
        assert_eq!(table.current_scope(), ScopeId(1));

        table.pop_scope();
        assert_eq!(table.current_scope(), ScopeId(0));
    }

    #[test]
    fn define_and_lookup() {
        let mut table = ScopeTable::new();
        let def = table
            .define("foo".into(), DefKind::Variable, Span::dummy())
            .unwrap();
        assert_eq!(table.lookup("foo"), Some(def));
        assert_eq!(table.lookup("bar"), None);
    }

    #[test]
    fn parent_chain_lookup() {
        let mut table = ScopeTable::new();
        let outer = table
            .define("x".into(), DefKind::Variable, Span::dummy())
            .unwrap();

        table.push_scope(ScopeKind::Block);
        // Should find x from parent scope
        assert_eq!(table.lookup("x"), Some(outer));

        // Define y in inner scope
        let inner = table
            .define("y".into(), DefKind::Variable, Span::dummy())
            .unwrap();
        assert_eq!(table.lookup("y"), Some(inner));

        table.pop_scope();
        // y should not be visible in outer scope
        assert_eq!(table.lookup("y"), None);
    }

    #[test]
    fn shadowing() {
        let mut table = ScopeTable::new();
        let outer = table
            .define("x".into(), DefKind::Variable, Span::dummy())
            .unwrap();

        table.push_scope(ScopeKind::Block);
        let inner = table
            .define("x".into(), DefKind::Variable, Span::dummy())
            .unwrap();

        // Inner x shadows outer
        assert_eq!(table.lookup("x"), Some(inner));
        assert_ne!(outer, inner);

        table.pop_scope();
        // Back to outer x
        assert_eq!(table.lookup("x"), Some(outer));
    }

    #[test]
    fn duplicate_in_same_scope() {
        let mut table = ScopeTable::new();
        table
            .define("x".into(), DefKind::Variable, Span::new(0, 5))
            .unwrap();
        let err = table
            .define("x".into(), DefKind::Variable, Span::new(10, 15))
            .unwrap_err();
        match err.kind {
            SemanticErrorKind::DuplicateDefinition { name, original } => {
                assert_eq!(name, "x");
                assert_eq!(original, Span::new(0, 5));
            }
            _ => panic!("expected DuplicateDefinition"),
        }
    }

    #[test]
    fn is_in_loop() {
        let mut table = ScopeTable::new();
        assert!(!table.is_in_loop());

        table.push_scope(ScopeKind::Function);
        assert!(!table.is_in_loop());

        table.push_scope(ScopeKind::ForLoop);
        assert!(table.is_in_loop());

        table.push_scope(ScopeKind::Block);
        assert!(table.is_in_loop()); // nested block inside loop

        table.pop_scope();
        table.pop_scope();
        assert!(!table.is_in_loop()); // back in function
    }
}
