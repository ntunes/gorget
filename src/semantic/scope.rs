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
    /// If this is a function parameter, its ownership annotation.
    pub param_ownership: Option<crate::parser::ast::Ownership>,
    /// Whether this variable was declared with `shared` (for CFA).
    pub shared: crate::parser::ast::SharedKind,
    /// For struct defs: field TypeIds in declaration order.
    /// Populated during type checking. Used by is_copy_type for transitive checks.
    pub field_types: Option<Vec<TypeId>>,
    /// For enum defs: variant field TypeIds (Vec per variant, in declaration order).
    /// Populated during type checking. Used by is_copy_type for transitive checks.
    pub variant_field_types: Option<Vec<Vec<TypeId>>>,
}

/// A lexical scope.
///
/// Each scope keeps two disjoint name maps — type-namespace names
/// (structs, enums, traits, type aliases, newtypes, generic params)
/// and value-namespace names (variables, functions, constants,
/// statics, variants). This lets e.g. `Error` live simultaneously as
/// a user-defined trait *and* the `Result.Error` variant constructor
/// — the former is looked up at type positions, the latter at
/// expression / pattern positions.
///
/// Imports register in both namespaces (they can refer to either).
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub kind: ScopeKind,
    pub types: FxHashMap<String, DefId>,
    pub values: FxHashMap<String, DefId>,
}

/// Classify a DefKind into the type or value namespace (some kinds —
/// notably `Import` — span both; for those this returns `Type` and
/// the define path also inserts into `values`).
pub fn def_namespace(kind: DefKind) -> Namespace {
    match kind {
        // Pure type names — trait, type alias, generic param.
        DefKind::Trait
        | DefKind::TypeAlias
        | DefKind::GenericParam => Namespace::Type,
        // Struct / Enum / Newtype names are dual-role in Gorget: the
        // same identifier is used as a type (`Vector[int] v`) AND as a
        // constructor / path head (`Vector[int]()`, `Option.None`).
        // Register in both namespaces so expression-position callers
        // find them the same way type-position callers do.
        DefKind::Struct
        | DefKind::Enum
        | DefKind::Newtype => Namespace::Both,
        DefKind::Function
        | DefKind::Variant
        | DefKind::Variable
        | DefKind::Const
        | DefKind::Static => Namespace::Value,
        // Imports are ambiguous — we register in both namespaces.
        DefKind::Import => Namespace::Both,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Namespace {
    Type,
    Value,
    Both,
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
            types: FxHashMap::default(),
            values: FxHashMap::default(),
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
            types: FxHashMap::default(),
            values: FxHashMap::default(),
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

    /// Temporarily enter an existing scope (e.g. a FileModule scope for body resolution).
    /// Returns the previous scope ID so it can be restored via `restore_scope`.
    pub fn enter_scope(&mut self, scope_id: ScopeId) -> ScopeId {
        let prev = self.current;
        self.current = scope_id;
        prev
    }

    /// Restore the current scope to a previously saved scope ID.
    pub fn restore_scope(&mut self, scope_id: ScopeId) {
        self.current = scope_id;
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
            param_ownership: None,
            shared: crate::parser::ast::SharedKind::None,
            field_types: None,
            variant_field_types: None,
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
        let ns = def_namespace(kind);
        // Check for duplicates only within the same namespace.
        // A type named `Error` and a variant named `Error` can coexist.
        let scope = &self.scopes[self.current.0 as usize];
        let existing_ids = match ns {
            Namespace::Type => vec![scope.types.get(&name).copied()],
            Namespace::Value => vec![scope.values.get(&name).copied()],
            Namespace::Both => vec![
                scope.types.get(&name).copied(),
                scope.values.get(&name).copied(),
            ],
        };
        for existing_opt in existing_ids.iter().copied().flatten() {
            let existing = &self.definitions[existing_opt.0 as usize];
            // Allow a real definition to replace an import placeholder,
            // a real import to replace a built-in placeholder (dummy span),
            // a user definition to shadow a built-in trait (dummy span),
            // or a user-defined variant to shadow a built-in prelude variant (dummy span).
            let can_replace = (existing.kind == DefKind::Import && kind != DefKind::Import)
                || (existing.kind == DefKind::Import && existing.span == Span::dummy())
                || (existing.kind == DefKind::Trait && existing.span == Span::dummy())
                || (existing.kind == DefKind::Variant && existing.span == Span::dummy() && kind == DefKind::Variant)
                // An Import can shadow any dummy-span prelude entry
                // (prelude variant, built-in trait, etc.). The user
                // wrote `from X import Y` because they want the
                // imported Y — not the prelude placeholder with the
                // same name.
                || (kind == DefKind::Import && existing.span == Span::dummy());
            if !can_replace {
                let original_span = existing.span;
                return Err(SemanticError {
                    kind: SemanticErrorKind::DuplicateDefinition {
                        name,
                        original: original_span,
                    },
                    span,
                });
            }
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
            param_ownership: None,
            shared: crate::parser::ast::SharedKind::None,
            field_types: None,
            variant_field_types: None,
        });
        let scope = &mut self.scopes[self.current.0 as usize];
        match ns {
            Namespace::Type => {
                scope.types.insert(name, def_id);
            }
            Namespace::Value => {
                scope.values.insert(name, def_id);
            }
            Namespace::Both => {
                scope.types.insert(name.clone(), def_id);
                scope.values.insert(name, def_id);
            }
        }
        Ok(def_id)
    }

    /// Look up a name, walking the parent chain.
    /// Look up a name, walking the parent chain. Checks the value
    /// namespace first then the type namespace — preserves the
    /// single-namespace behavior for all names that exist in one
    /// namespace. When a name exists in both (e.g. `Error` as both
    /// the `Result.Error` variant and a user-defined `trait Error`),
    /// callers that care about the type meaning should use
    /// `lookup_type`; callers that care about the value meaning
    /// should use `lookup_value`. The generic `lookup` is fine for
    /// ambiguous contexts where either meaning would make sense.
    pub fn lookup(&self, name: &str) -> Option<DefId> {
        self.lookup_value(name).or_else(|| self.lookup_type(name))
    }

    /// Look up a name in the type namespace only.
    pub fn lookup_type(&self, name: &str) -> Option<DefId> {
        let mut scope_id = Some(self.current);
        while let Some(sid) = scope_id {
            let scope = &self.scopes[sid.0 as usize];
            if let Some(&def_id) = scope.types.get(name) {
                return Some(def_id);
            }
            scope_id = scope.parent;
        }
        None
    }

    /// Look up a name in the value namespace only.
    pub fn lookup_value(&self, name: &str) -> Option<DefId> {
        let mut scope_id = Some(self.current);
        while let Some(sid) = scope_id {
            let scope = &self.scopes[sid.0 as usize];
            if let Some(&def_id) = scope.values.get(name) {
                return Some(def_id);
            }
            scope_id = scope.parent;
        }
        None
    }

    /// Look up a name starting from a given scope, walking the parent chain.
    pub fn lookup_from_scope(&self, scope_id: ScopeId, name: &str) -> Option<DefId> {
        // Preserve old behavior: value first, then type.
        self.lookup_value_from_scope(scope_id, name)
            .or_else(|| self.lookup_type_from_scope(scope_id, name))
    }

    pub fn lookup_value_from_scope(&self, scope_id: ScopeId, name: &str) -> Option<DefId> {
        let mut sid = Some(scope_id);
        while let Some(s) = sid {
            let scope = &self.scopes[s.0 as usize];
            if let Some(&def_id) = scope.values.get(name) {
                return Some(def_id);
            }
            sid = scope.parent;
        }
        None
    }

    pub fn lookup_type_from_scope(&self, scope_id: ScopeId, name: &str) -> Option<DefId> {
        let mut sid = Some(scope_id);
        while let Some(s) = sid {
            let scope = &self.scopes[s.0 as usize];
            if let Some(&def_id) = scope.types.get(name) {
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
    /// Value namespace first, then type namespace.
    pub fn lookup_in_scope(&self, scope_id: ScopeId, name: &str) -> Option<DefId> {
        let scope = &self.scopes[scope_id.0 as usize];
        scope.values.get(name).copied().or_else(|| scope.types.get(name).copied())
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

    /// Collect all visible names from the current scope and its parent chain.
    pub fn visible_names(&self) -> Vec<String> {
        let mut seen = rustc_hash::FxHashSet::default();
        let mut result = Vec::new();
        let mut scope_id = Some(self.current);
        while let Some(sid) = scope_id {
            let scope = &self.scopes[sid.0 as usize];
            for name in scope.types.keys().chain(scope.values.keys()) {
                if seen.insert(name.clone()) {
                    result.push(name.clone());
                }
            }
            scope_id = scope.parent;
        }
        result
    }

    /// Suggest the closest visible name to `target` using edit distance.
    /// Returns `None` if no name is close enough.
    pub fn suggest_name(&self, target: &str) -> Option<String> {
        if target.len() <= 1 {
            return None;
        }
        let names = self.visible_names();
        let threshold = std::cmp::max(2, target.len() * 2 / 5);
        let mut best: Option<(usize, String)> = None;
        for name in names {
            if name.len() <= 1 {
                continue;
            }
            let dist = edit_distance(target, &name);
            if dist <= threshold {
                if best.as_ref().map_or(true, |(d, _)| dist < *d) {
                    best = Some((dist, name));
                }
            }
        }
        best.map(|(_, name)| name)
    }

    /// Return all `(name, DefId)` pairs defined directly in the current scope.
    /// Combines both namespaces.
    pub fn names_in_current_scope(&self) -> Vec<(String, DefId)> {
        let scope = &self.scopes[self.current.0 as usize];
        let mut out = Vec::with_capacity(scope.types.len() + scope.values.len());
        for (n, d) in scope.types.iter().chain(scope.values.iter()) {
            out.push((n.clone(), *d));
        }
        out
    }

    /// Copy all non-private names from the current scope into the parent scope.
    ///
    /// Called after collecting a `FileModule` scope's items to make public items
    /// accessible from the enclosing (global Module) scope. Both namespaces
    /// are exported.
    /// Export non-private definitions from the current FileModule scope to its parent.
    /// Returns a list of cross-module collisions detected during the export — each entry
    /// is `(name, existing_def_id, new_def_id)` where both DefIds refer to real (non-Import,
    /// non-prelude) definitions in different modules. The caller emits these as
    /// `DuplicateDefinition` errors so users see a clear "X here shadows X from module Y"
    /// instead of broken C codegen at link time (the type registry is currently flat at
    /// the C-mangling layer, so two same-named user types from different modules collapse
    /// to one C struct).
    pub fn export_non_private(
        &mut self,
        private_names: &rustc_hash::FxHashSet<String>,
    ) -> Vec<(String, DefId, DefId)> {
        let current_idx = self.current.0 as usize;
        let parent_idx = match self.scopes[current_idx].parent {
            Some(p) => p.0 as usize,
            None => return Vec::new(), // root scope — nothing to export to
        };

        // Collect per-namespace so we can export into the matching map.
        let type_entries: Vec<(String, DefId)> = self.scopes[current_idx]
            .types
            .iter()
            .filter(|(name, _)| !private_names.contains(name.as_str()))
            .map(|(n, d)| (n.clone(), *d))
            .collect();
        let value_entries: Vec<(String, DefId)> = self.scopes[current_idx]
            .values
            .iter()
            .filter(|(name, _)| !private_names.contains(name.as_str()))
            .map(|(n, d)| (n.clone(), *d))
            .collect();

        let mut collisions: Vec<(String, DefId, DefId)> = Vec::new();

        for (name, def_id) in type_entries {
            let existing = self.scopes[parent_idx].types.get(&name).copied();
            let action = match existing {
                None => Action::Insert,
                Some(existing_id) if existing_id == def_id => Action::Skip, // same def re-exported
                Some(existing_id) => {
                    let existing_def = &self.definitions[existing_id.0 as usize];
                    if existing_def.kind == DefKind::Import
                        || existing_def.span == crate::span::Span::dummy()
                    {
                        Action::Insert
                    } else {
                        Action::Collide(existing_id)
                    }
                }
            };
            match action {
                Action::Insert => { self.scopes[parent_idx].types.insert(name, def_id); }
                Action::Skip => {}
                Action::Collide(existing_id) => collisions.push((name, existing_id, def_id)),
            }
        }
        // VALUE namespace: do NOT report collisions here. Multiple stdlib
        // modules legitimately re-declare the same extern with the same C
        // symbol (e.g. `__bytes_to_str_raw` lives in both `std.io` and
        // `std.bytes`, both bound to `gorget_bytes_to_str`). The TYPE-
        // namespace collision check above is the load-bearing one — that's
        // where the C struct-layout mismatch happens at link time. Function
        // collisions don't have the same structural problem because their
        // call sites resolve through the call site's type, not by name
        // alone.
        for (name, def_id) in value_entries {
            let existing = self.scopes[parent_idx].values.get(&name).copied();
            let should_insert = match existing {
                None => true,
                Some(existing_id) => {
                    let existing_def = &self.definitions[existing_id.0 as usize];
                    existing_def.kind == DefKind::Import
                        || existing_def.span == crate::span::Span::dummy()
                }
            };
            if should_insert {
                self.scopes[parent_idx].values.insert(name, def_id);
            }
        }
        collisions
    }
}

enum Action {
    Insert,
    Skip,
    Collide(DefId),
}

/// Standard Levenshtein edit distance between two strings.
fn edit_distance(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let (m, n) = (a.len(), b.len());
    let mut prev = (0..=n).collect::<Vec<_>>();
    let mut curr = vec![0; n + 1];
    for i in 1..=m {
        curr[0] = i;
        for j in 1..=n {
            let cost = if a[i - 1] == b[j - 1] { 0 } else { 1 };
            curr[j] = (prev[j] + 1)
                .min(curr[j - 1] + 1)
                .min(prev[j - 1] + cost);
        }
        std::mem::swap(&mut prev, &mut curr);
    }
    prev[n]
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
