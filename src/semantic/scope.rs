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

/// RV-A field-access disposition for a BUILTIN smart-pointer / guard wrapper
/// type. Seeded ONCE at registration onto `DefInfo.deref_wrapper_kind`
/// (`None` = not a wrapper) and read via the typed flag at the field-access
/// reject site.
/// The three variants key the 3-way diagnostic table in the RV-A brief
/// (`the RV-A fieldaccess brief (git history)`).
///
/// ⚠ THIS FLAG IS THE TYPED SOURCE OF TRUTH ONLY IN SEMANTIC ANALYSIS. Lowering
/// does NOT read it — it RE-DERIVES "is this a guard wrapper?" from the mangled
/// type NAME, via `guard_inner_suffix` (`src/ir/lowering/exprs/shared.rs`, a
/// `strip_prefix("Guard__"/"ReadGuard__"/"WriteGuard__")` test) at four call
/// sites: `ir/lowering/stmts/assigns.rs` (plain field assign) and three in
/// `ir/lowering/exprs/mod.rs` (field read, the shared `&`-place typing arm, and
/// the write-place producer). That is a standing violation of layering rule 2,
/// not a description of current design: it is why a USER generic named
/// `Guard[T]` collides with the builtin (see
/// `tests/fixtures/known_gaps/fieldaccess_user_generic_guard_collision.gg`).
/// The reference-grade shape is to carry this flag into GIR and read it there.
/// Filed in TODO.md; do not restate "never re-derived from a name downstream"
/// until that lands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DerefWrapperKind {
    /// Guard / ReadGuard / WriteGuard — a lock/borrow guard whose `.field`
    /// auto-derefs to the inner value: present field → ACCEPT, absent field →
    /// `E_NoFieldFound`.
    ///
    /// ⚠ "ACCEPT" IS THIS PASS'S DISPOSITION, NOT A CLAIM THAT THE ACCESS IS
    /// CORRECT. Do not read the old "WORKS today (green fixtures)" wording back
    /// into it — measured at HEAD: a write through a `ReadGuard` field place is
    /// silently DROPPED at all three faces, and a `Guard` field reached through
    /// a PARAMETER is memory-unsafe. Both are filed with `known_gaps` repros
    /// (`sound_readguard_write_faces_dropped`, `sound_guard_param_field_unsafe`).
    /// Accepting here is still right — these are not the deref-coercion class —
    /// but the acceptance is not evidence of correctness downstream.
    GuardAccept,
    /// Box — the SOLE §9.4 deref-coercion target (design-doc :1707-1712).
    /// A field present on the inner is `E_DerefCoercionUnimplemented` (the
    /// deref-field-read backend is not yet built); an absent or primitive
    /// inner is `E_NoFieldFound` (the §9.4 message would lie).
    DerefTarget,
    /// Shared / Weak / Mutex / RWLock — accessed through an explicit method
    /// (`.get()` / `.upgrade()` §9.2 / `.lock()` / `.read()`), never deref:
    /// direct `.field` is always `E_NoFieldFound`, even when the inner has it.
    NonDerefContainer,
}

impl DerefWrapperKind {
    /// The ONE allowed registration-time name-match (mirrors the
    /// `compute_drop_taint` seeding precedent): map a builtin wrapper type
    /// name to its field-access disposition, `None` for every non-wrapper
    /// name. Callers seed `DefInfo.deref_wrapper_kind` from this ONLY for
    /// definitions in the builtin registry / builtin modules, so a user
    /// struct sharing the name never gets a kind.
    pub fn for_builtin_name(name: &str) -> Option<DerefWrapperKind> {
        match name {
            "Box" => Some(DerefWrapperKind::DerefTarget),
            "Guard" | "ReadGuard" | "WriteGuard" => Some(DerefWrapperKind::GuardAccept),
            "Shared" | "Weak" | "Mutex" | "RWLock" => Some(DerefWrapperKind::NonDerefContainer),
            _ => None,
        }
    }
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
    /// D4 drop-purity (D12 enforcement): true if this type has a custom `Drop`
    /// anywhere in its transitive field/payload graph. Seeded from `equip T
    /// with Drop` registrations and closed under the field-graph fixpoint by
    /// `compute_drop_taint` (semantic/mod.rs); read via
    /// `is_drop_tainted_type`. Typed metadata, never derived from names
    /// (layering rule 2). Mirrors ggdef's `tainted` set
    /// (spec/ggdef/src/elaborate/mod.rs:253-255, :458-487).
    pub is_drop_tainted: bool,
    /// RV-A field-access soundness: `Some(kind)` iff this DefId is a BUILTIN
    /// smart-pointer / guard wrapper whose `.field` disposition is `kind`
    /// (see `DerefWrapperKind`); `None` = not a wrapper. Seeded ONCE at
    /// registration (`BUILTIN_GENERIC_TYPES` imports + builtin-module structs)
    /// via `DerefWrapperKind::for_builtin_name`; a USER struct that shadows the
    /// name gets a distinct DefId with `None`, so it no longer escapes
    /// `E_NoFieldFound` (the garbage-0 miscompile). `.is_some()` is the
    /// is-a-wrapper predicate; the `Some(kind)` carries the 3-way split.
    /// Retires the `is_field_deref_wrapper` name-match (layering rule 2).
    pub deref_wrapper_kind: Option<DerefWrapperKind>,
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
    /// Reverse index: name → DefIds of every definition with that name, in
    /// insertion order (so the last entry has the highest DefId). Maintained
    /// incrementally alongside `definitions`. Turns the O(N_defs) linear scans
    /// in `lookup_within_function`, `is_global_def`, and `lookup_def_by_span`
    /// into O(K) lookups (K = number of defs sharing a name, typically 1–5).
    /// At self-host-lowerer scale, the safety pass calls `find_def_by_name`
    /// thousands of times across ~10K total defs — the linear scan was
    /// quadratic-in-module-size and dominated the semantic phase (~75%).
    name_index: FxHashMap<String, Vec<DefId>>,
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
            name_index: FxHashMap::default(),
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
        self.name_index.entry(name.clone()).or_default().push(def_id);
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
            is_drop_tainted: false,
            deref_wrapper_kind: None,
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
            // Replacement is allowed only against placeholders, never against
            // user-written declarations:
            //  - a dummy-span Import (built-in placeholder for Vector/Box/etc.,
            //    or a prelude entry) can be replaced by anything;
            //  - a dummy-span Trait or Variant (prelude placeholder) can be
            //    shadowed by a user definition of the matching kind;
            //  - an `import` can shadow any dummy-span prelude entry — the user
            //    wrote `from X import Y` because they want the imported Y, not
            //    the prelude placeholder.
            //
            // Snag #29 follow-up #2 (2026-05-10): the historical clause
            // `(existing.kind == Import && kind != Import)` allowed a real
            // user-written import to be silently replaced by a same-named user
            // definition (`from std.math import PI; enum PI:` … and `PI` no
            // longer pointed at the import). The reverse order — user def then
            // import — already errored, so the asymmetric "first one loses"
            // behaviour silently shadowed the import and produced wrong
            // resolution at use sites. Now both orders error consistently:
            // the user must rename one or remove one. Dummy-span built-in
            // imports remain replaceable via clause 1.
            let can_replace = (existing.kind == DefKind::Import && existing.span == Span::dummy())
                || (existing.kind == DefKind::Trait && existing.span == Span::dummy())
                || (existing.kind == DefKind::Variant && existing.span == Span::dummy() && kind == DefKind::Variant)
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

        // Track P (2026-07-28): when the user's `from std.sync import Mutex`
        // creates a NEW def replacing the dummy-span builtin-wrapper
        // placeholder (registered in `resolve.rs::collect_top_level` via
        // `BUILTIN_GENERIC_TYPES` + `DerefWrapperKind::for_builtin_name`),
        // inherit the `deref_wrapper_kind` so the imported alias keeps the
        // typed metadata. Layering rule 3: resolve once, write through — one
        // seed at the builtin, propagated at every user-import shadow.
        //
        // Gated on `kind == DefKind::Import` so a USER `struct Guard` /
        // `struct Mutex` (shadowing the name with their own type) does NOT
        // inherit the wrapper semantics — the typed-flag fix's whole point
        // is that a USER struct with the same name gets `None` and stops
        // escaping E_NoFieldFound (see `fieldaccess_user_guard_missing_field_reject`
        // fixture + scope.rs::DerefWrapperKind::for_builtin_name doc-comment).
        let inherited_deref_kind = if kind == DefKind::Import {
            existing_ids
                .iter()
                .copied()
                .flatten()
                .find_map(|id| {
                    let d = &self.definitions[id.0 as usize];
                    if d.span == Span::dummy() {
                        d.deref_wrapper_kind
                    } else {
                        None
                    }
                })
        } else {
            None
        };
        let def_id = DefId(self.definitions.len() as u32);
        self.name_index.entry(name.clone()).or_default().push(def_id);
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
            is_drop_tainted: false,
            deref_wrapper_kind: inherited_deref_kind,
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
    ///
    /// Uses the per-name DefId index (`name_index`) so the inner loop runs over
    /// only the (typically 1–5) defs sharing this name, rather than scanning
    /// all ~10K module-wide definitions. The previous linear scan was
    /// quadratic-in-module-size and dominated the semantic phase.
    pub fn lookup_within_function(&self, fn_scope_id: ScopeId, name: &str) -> Option<DefId> {
        if let Some(ids) = self.name_index.get(name) {
            // Iterate in reverse so the highest matching DefId wins (matches
            // the previous "best = last-match-in-order" semantic).
            for &def_id in ids.iter().rev() {
                let def = &self.definitions[def_id.0 as usize];
                if matches!(def.kind, DefKind::Variable | DefKind::Const | DefKind::Function)
                    && self.is_descendant_of(def.scope, fn_scope_id)
                {
                    return Some(def_id);
                }
            }
        }
        // Fall back to ancestor walk for module-scope names.
        self.lookup_from_scope(fn_scope_id, name)
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

    /// All def-ids sharing `name`, in registration order. Backed by the
    /// per-name `name_index`, so it sees cross-module / un-imported defs.
    /// Track P (2026-07-28): used by `trait_name_of_inner` to detect that a
    /// `DefKind::Import` placeholder points at a Trait defined elsewhere in
    /// the program.
    pub fn defs_named(&self, name: &str) -> Vec<DefId> {
        self.name_index.get(name).cloned().unwrap_or_default()
    }

    /// True if any definition with this name exists ANYWHERE in the program —
    /// in scope or not, any namespace, any kind. Backed by the per-name
    /// `name_index`, so it sees cross-module / un-imported types (e.g. an
    /// `std.sync` `ReadGuard` referenced without an explicit import, or a
    /// runtime struct decl) that the lexical-scope `lookup` misses. Used to
    /// distinguish a genuinely-undefined type name (a typo) from a real type
    /// that simply isn't in the current lexical scope.
    pub fn name_defined_anywhere(&self, name: &str) -> bool {
        self.name_index.get(name).is_some_and(|ids| !ids.is_empty())
    }

    /// True if `name` is the name of at least one enum variant defined anywhere
    /// in this module (in scope or not). Used by the resolver to suppress the
    /// `undefined name` diagnostic for bare-variant constructor calls whose
    /// qualification was dropped by the loader's ambiguity dedup (see
    /// `build_variant_map_from_all` in `src/loader.rs`). The typechecker
    /// disambiguates via `decl_type_hint` once it sees the call site.
    ///
    /// Non-generic enum variants are allocated via `alloc_def` (which inserts
    /// into `name_index`) but NOT inserted into any scope's value namespace —
    /// they are accessed via qualified paths (`EnumName.Variant`). When the
    /// loader's pre-merge qualifier drops an ambiguous bare name, the
    /// resolver hits an `Identifier(name)` that isn't in scope but is still a
    /// legitimate variant ref; this lookup returns true for those names so
    /// the resolver can stay silent. Real undefined names (no variant
    /// anywhere) still report normally.
    pub fn is_known_variant_name(&self, name: &str) -> bool {
        if let Some(ids) = self.name_index.get(name) {
            for &def_id in ids {
                if self.definitions[def_id.0 as usize].kind == DefKind::Variant {
                    return true;
                }
            }
        }
        false
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
        // Use the per-name index: only scan defs sharing this name.
        let ids = self.name_index.get(name)?;
        for &def_id in ids {
            let def = &self.definitions[def_id.0 as usize];
            if def.span == span {
                return Some(def_id);
            }
        }
        None
    }

    /// Check if a name refers to a global definition (function, enum variant, struct, etc.)
    /// that doesn't need to be captured by closures.
    pub fn is_global_def(&self, name: &str) -> bool {
        let Some(ids) = self.name_index.get(name) else { return false };
        // Reverse iteration mirrors the prior `definitions.iter().rev()` semantic:
        // the most recent definition with this name decides the answer.
        if let Some(&def_id) = ids.last() {
            let def = &self.definitions[def_id.0 as usize];
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

    /// Rebind an Import-placeholder name in the current scope to point at the
    /// same DefId as another name (used by `from X import Y as Z` after the
    /// imported module's exports have been merged into the parent scope).
    ///
    /// Walks the parent chain to find `source_name`'s DefId, then overwrites
    /// the `local_name` entry in the current scope's type and value namespaces.
    /// Returns the source DefId on success, or `None` if `source_name` is not
    /// in scope. Caller decides what to do on miss (likely no-op — the source
    /// module didn't export the requested name, which is a different error).
    pub fn rebind_alias(&mut self, source_name: &str, local_name: &str) -> Option<DefId> {
        let src_id = self.lookup(source_name)?;
        let scope = &mut self.scopes[self.current.0 as usize];
        // Overwrite only entries that currently exist (i.e. the placeholder we
        // registered at parse time). The placeholder was inserted into BOTH
        // namespaces (DefKind::Import → Namespace::Both), so update both.
        if scope.types.contains_key(local_name) {
            scope.types.insert(local_name.to_string(), src_id);
        }
        if scope.values.contains_key(local_name) {
            scope.values.insert(local_name.to_string(), src_id);
        }
        Some(src_id)
    }

    /// Bind `local_name` in the current scope to the same DefId as `source_name`,
    /// creating the entries (inserting into both type and value namespaces) if
    /// they don't already exist. Used by wildcard imports (`from X import *`).
    /// Returns `None` if `source_name` is not in scope.
    pub fn bind_wildcard(&mut self, source_name: &str, local_name: &str) -> Option<DefId> {
        let src_id = self.lookup(source_name)?;
        let scope = &mut self.scopes[self.current.0 as usize];
        scope.types.entry(local_name.to_string()).or_insert(src_id);
        scope.values.entry(local_name.to_string()).or_insert(src_id);
        Some(src_id)
    }

    /// Return all unique names defined directly in `scope_id` across both namespaces.
    pub fn names_in_scope(&self, scope_id: ScopeId) -> Vec<String> {
        let scope = &self.scopes[scope_id.0 as usize];
        let mut seen = rustc_hash::FxHashSet::default();
        let mut out = Vec::with_capacity(scope.types.len() + scope.values.len());
        for name in scope.types.keys().chain(scope.values.keys()) {
            if seen.insert(name.clone()) {
                out.push(name.clone());
            }
        }
        out
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
