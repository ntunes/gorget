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
    Function,
    Block,
    ImplBlock { self_type: Option<TypeId> },
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
    pub fn define(
        &mut self,
        name: String,
        kind: DefKind,
        span: Span,
    ) -> Result<DefId, SemanticError> {
        let scope = &self.scopes[self.current.0 as usize];
        if let Some(&existing_id) = scope.names.get(&name) {
            let original_span = self.definitions[existing_id.0 as usize].span;
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
