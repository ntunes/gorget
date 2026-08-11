use rustc_hash::{FxHashMap, FxHashSet};

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use super::errors::{SemanticError, SemanticErrorKind};
use super::ids::{DefId, TypeId};
use super::scope::{DefKind, DerefWrapperKind, ScopeKind, ScopeTable};
use super::types::{self, TypeTable};

pub use crate::parser::ast::Ownership;

/// Built-in generic types that are always available (no `import` needed) and
/// resolve through the builtin-generic `base_name` machinery in IR lowering
/// rather than a real struct def. Registered as dummy-span Import placeholders
/// at resolution start; recognized by the unresolved-import check so that a
/// redundant `from std.sync import Channel` (where `Channel` is a builtin, not a
/// std.sync export) is not flagged.
const BUILTIN_GENERIC_TYPES: &[&str] = &[
    "Vector", "Deque", "Dict", "HashMap", "Set", "HashSet", "Box", "Future", "Task",
    "Channel", "Shared", "Weak", "Mutex", "Guard", "TaskGroup", "FxHasher",
];

/// Side table for struct field info.
#[derive(Debug, Clone)]
pub struct StructFieldInfo {
    pub fields: Vec<(String, Span)>,
    /// Generic type parameter names for the struct (e.g. ["A", "B"] for
    /// `Pair[A, B]`). Empty for non-generic structs. Stored UNCONDITIONALLY
    /// (unlike `struct_generic_bounds`, which records names only when a param
    /// carries a trait bound) so generic-struct field access can build a
    /// name→targ substitution even for `struct Pair[A, B]` with no bounds.
    pub generic_param_names: Vec<String>,
    /// AST field types, in declaration order (parallel to `fields`). Used to
    /// resolve concrete AND generic-param field types at a generic-struct
    /// field access (`Pair[int,String] p; p.first` → substitute A=int).
    pub field_ast_types: Vec<Spanned<Type>>,
}

/// Side table for enum variant info.
#[derive(Debug, Clone)]
pub struct EnumVariantInfo {
    pub variants: Vec<(String, DefId)>,
    /// AST types per variant's fields — for resolving pattern-bound var types.
    /// Key: variant name, Value: AST field types. Empty for built-in enums.
    pub variant_field_types: Vec<(String, Vec<Spanned<Type>>)>,
    /// Generic type parameter names for the enum (e.g. ["T", "E"] for Result[T, E]).
    /// Empty for non-generic or built-in enums.
    pub generic_param_names: Vec<String>,
}

/// Maps name-use spans to their definitions (the resolution map).
pub type ResolutionMap = FxHashMap<usize, DefId>;

/// Stored info about functions for type checking.
#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub def_id: DefId,
    pub return_type_id: Option<TypeId>,
    pub param_type_ids: Vec<Option<TypeId>>,
    pub param_ownerships: Vec<Ownership>,
    pub param_names: Vec<String>,
    /// Default value expressions for each parameter (None if no default).
    pub param_defaults: Vec<Option<Spanned<Expr>>>,
    pub throws: bool,
    /// For `throws E` functions, the TypeId of `E`. The function's "raw"
    /// return type is `T` (stored in `return_type_id`), but the
    /// `Result[T, E]` wrapping happens at the call-site boundary — so the
    /// inferred type of a Call to this function is `Result[T, E]`, not `T`.
    /// `None` for non-throws functions. Populated by the typechecker's
    /// `register_function_signature` after resolving the throws-type AST.
    /// Snag #35: without this, the typechecker can't model the
    /// Result[T, E] return-at-call-site and silently coerces the Result
    /// handle into a bare-T destination.
    pub throws_type_id: Option<TypeId>,
    pub is_async: bool,
    pub is_blocking: bool,
    /// `noreturn` extern functions — call never returns; type system treats
    /// the call as `Never` and the IR terminates the basic block after it.
    pub is_noreturn: bool,
    pub scope_id: super::ids::ScopeId,
    /// Names of generic type parameters, in declaration order.
    pub generic_param_names: Vec<String>,
    /// Where-clause bounds: `(param_name, [trait_name, ...])`.
    pub trait_bounds: Vec<(String, Vec<String>)>,
    /// Param indices whose data flows to the return value (lifetime inference).
    /// Computed by borrow checker Pass 5a.
    pub return_borrows_from: Vec<usize>,
    /// Whether the function has a body (Block or Expression, not Declaration/Extern).
    pub has_body: bool,
    /// True if the function has a body and body analysis proved return value is static
    /// (no parameter data flows to the return). Set by Pass 5a.
    pub return_origin_is_static: bool,
}

/// Shared context passed around during resolution.
pub struct ResolveContext {
    pub struct_fields: FxHashMap<DefId, StructFieldInfo>,
    pub enum_variants: FxHashMap<DefId, EnumVariantInfo>,
    pub function_info: FxHashMap<DefId, FunctionInfo>,
    pub resolution_map: ResolutionMap,
    /// Maps (function_name, span_start) → body scope id (for ALL functions including equip methods).
    /// Composite key avoids span collisions between different source files.
    pub function_body_scopes: FxHashMap<(String, usize), super::ids::ScopeId>,
    /// Private name sets per module path — for detecting imports of private items.
    /// Populated during FileModule collection, validated after all modules are processed.
    pub module_private_names: Vec<(Vec<String>, FxHashSet<String>)>,
    /// Scope IDs for FileModule scopes — keyed by joined module path.
    /// Used during body resolution so that private names are visible within the module.
    pub file_module_scopes: FxHashMap<String, super::ids::ScopeId>,
    /// Generic type parameter bounds for structs/enums:
    /// (param_names, [(param_name, [trait_name, ...])])
    pub struct_generic_bounds: FxHashMap<DefId, (Vec<String>, Vec<(String, Vec<String>)>)>,
    /// Map from import-alias local name → source-module name.
    /// Populated for every `from X import Y as Z`; rewritten in pass 2.6.
    /// Empty when no aliased imports exist in the entry file.
    pub import_aliases: FxHashMap<String, String>,
}

impl ResolveContext {
    fn new() -> Self {
        Self {
            struct_fields: FxHashMap::default(),
            enum_variants: FxHashMap::default(),
            function_info: FxHashMap::default(),
            resolution_map: FxHashMap::default(),
            function_body_scopes: FxHashMap::default(),
            module_private_names: Vec::new(),
            file_module_scopes: FxHashMap::default(),
            struct_generic_bounds: FxHashMap::default(),
            import_aliases: FxHashMap::default(),
        }
    }
}

// ─── Pass 1: Top-Level Collection ──────────────────────────────

/// Collect all top-level definitions into the module scope.
pub fn collect_top_level(
    module: &Module,
    scopes: &mut ScopeTable,
    types: &mut TypeTable,
    errors: &mut Vec<SemanticError>,
) -> ResolveContext {
    let mut ctx = ResolveContext::new();
    // Register built-in core traits.
    for trait_name in &[
        "Displayable", "Debuggable", "Equatable", "Cloneable", "Hashable", "Hasher", "Drop", "Iterator", "Iterable",
        "Add", "Sub", "Mul", "Div", "Rem", "Mod", "Neg", "Comparable", "Index", "IndexMut",
        "Default", "From", "TryFrom", "Measurable", "Parseable", "One", "Numeric",
    ] {
        let _ = scopes.define(trait_name.to_string(), DefKind::Trait, Span::dummy());
    }
    // Register String constructor as a built-in function.
    let _ = scopes.define("String".to_string(), DefKind::Function, Span::dummy());
    // Register built-in collection types as Import placeholders so they're always
    // available for type resolution (e.g. Result[Vector[uint8], str] in synthetic modules).
    // The real struct definitions from std.collections replace these when imported.
    for type_name in BUILTIN_GENERIC_TYPES {
        if let Ok(did) = scopes.define(type_name.to_string(), DefKind::Import, Span::dummy()) {
            // RV-A: seed the typed deref-wrapper kind on the BUILTIN def (the
            // ONE allowed registration name-match; every downstream read is the
            // typed flag). Non-wrapper builtins (Vector/Dict/…) map to `None`.
            if let Some(kind) = DerefWrapperKind::for_builtin_name(type_name) {
                scopes.get_def_mut(did).deref_wrapper_kind = Some(kind);
            }
        }
    }
    // Register built-in Option[T] and Result[T,E] enum types with their variants.
    for (enum_name, variant_names) in &[
        ("Option", vec!["Some", "None"]),
        ("Result", vec!["Ok", "Error"]),
    ] {
        if let Ok(enum_def_id) = scopes.define(enum_name.to_string(), DefKind::Enum, Span::dummy()) {
            let mut variant_infos = Vec::new();
            for vname in variant_names {
                if let Ok(variant_def_id) = scopes.define(vname.to_string(), DefKind::Variant, Span::dummy()) {
                    variant_infos.push((vname.to_string(), variant_def_id));
                }
            }
            ctx.enum_variants.insert(enum_def_id, EnumVariantInfo { variants: variant_infos, variant_field_types: Vec::new(), generic_param_names: Vec::new() });
        }
    }
    // D26 (Round XXXIII Batch C1): the compiler-internal `ArithError` prelude
    // enum — payload-free variants for the two error channels produced by the
    // fallible arithmetic operators (`+!` / `-!` / `*!` etc). Its variants are
    // QUALIFIED-ONLY (`ArithError.Overflow`) — allocated WITHOUT a bare-name
    // scope insertion, unlike the prelude-bare Some/Ok/Error above. (The
    // `Fault` prelude enum this once mirrored was retired with the
    // fault-catch form by D25 — see `E_FaultCatchRemoved`.) Twin
    // registrations: IR lowering's `inject_builtin_enums`
    // (generics/substitute.rs) + `src/ir/lowering/mod.rs` TypeDef pass.
    if let Ok(ae_def_id) = scopes.define("ArithError".to_string(), DefKind::Enum, Span::dummy()) {
        let mut variant_infos = Vec::new();
        let mut variant_field_types = Vec::new();
        for vname in &["Overflow", "DivByZero"] {
            let variant_def_id = scopes.alloc_def(vname.to_string(), DefKind::Variant, Span::dummy());
            variant_infos.push((vname.to_string(), variant_def_id));
            variant_field_types.push((vname.to_string(), Vec::new()));
        }
        ctx.enum_variants.insert(ae_def_id, EnumVariantInfo { variants: variant_infos, variant_field_types, generic_param_names: Vec::new() });
    }
    // Register trait bounds for built-in collection types.
    // Dict[K,V] / HashMap[K,V] require K: Hashable + Equatable.
    // Set[T] / HashSet[T] require T: Hashable + Equatable.
    for (type_name, param_name) in &[("Dict", "K"), ("HashMap", "K"), ("Set", "T"), ("HashSet", "T")] {
        if let Some(def_id) = scopes.lookup(type_name) {
            let param_names = if *type_name == "Dict" || *type_name == "HashMap" {
                vec!["K".to_string(), "V".to_string()]
            } else {
                vec!["T".to_string()]
            };
            ctx.struct_generic_bounds.insert(
                def_id,
                (param_names, vec![(param_name.to_string(), vec!["Hashable".to_string(), "Equatable".to_string()])]),
            );
        }
    }

    collect_top_level_inner(module, scopes, types, errors, &mut ctx);

    // Second pass: handle glob imports (`from X import EnumName.*`).
    // All enums are now defined in scope, so we can look them up and register
    // their variants as bare names (shadowing any prelude variant with the same name).
    for item in &module.items {
        if let Item::Import(ImportStmt::From { glob_types, .. }) = &item.node {
            for glob_name in glob_types {
                // Register the type itself as Import (in case it wasn't in `names`)
                let _ = scopes.define(glob_name.node.clone(), DefKind::Import, glob_name.span);
                // Find the enum's variant info and bring each variant into scope
                if let Some(enum_def_id) = scopes.lookup(&glob_name.node) {
                    if let Some(variant_info) = ctx.enum_variants.get(&enum_def_id).cloned() {
                        for (vname, _vdef_id) in &variant_info.variants {
                            let _ = scopes.define(vname.clone(), DefKind::Variant, glob_name.span);
                        }
                    }
                }
            }
        }
    }

    // Third pass: handle aliased imports (`from X import Y as Z`).
    // The placeholder `Z` was registered at parse time; the source name `Y`
    // is now bound to the real def via the FileModule export pass. Rebind
    // `Z` → that DefId so name lookups during body resolution find the
    // right def. We also record `Z → Y` in `ctx.import_aliases` so that
    // a post-resolve AST rewrite can rename `Z` references back to `Y` —
    // the IR backend lowers calls by surface name, so without the rename
    // it would emit references to the local alias instead of the real
    // C symbol.
    for item in &module.items {
        if let Item::Import(ImportStmt::From { names, .. }) = &item.node {
            for n in names {
                if let Some(alias) = &n.alias {
                    let _ = scopes.rebind_alias(&n.name.node, &alias.node);
                    ctx.import_aliases.insert(alias.node.clone(), n.name.node.clone());
                }
            }
        }
    }

    // Fourth pass: handle module-level wildcard imports (`from X import *`).
    // The imported module's public names have already been exported to the
    // current (parent) scope via `export_non_private`, so the names are
    // technically already available. This pass exists for two reasons:
    //   1. Symmetry with the private-import check below — any private name
    //      that somehow leaked through the wildcard surface here would still
    //      need to be flagged.
    //   2. Future-proofing: if we ever tighten `export_non_private` to gate
    //      exports on explicit imports, wildcard expansion becomes load-bearing.
    // For now, the wildcard is mostly a no-op semantically (the path-based
    // loader still loads the target module via `extract_imports`), but we
    // record it as exercised so the unused-import check ignores it.
    for item in &module.items {
        if let Item::Import(ImportStmt::From { path, wildcard, .. }) = &item.node {
            if !*wildcard {
                continue;
            }
            // Find the imported FileModule scope and re-bind each public name
            // into the current scope (no-op when the name is already bound to
            // the same def from `export_non_private`; insert otherwise).
            let mod_key = path.iter().map(|s| s.node.as_str()).collect::<Vec<_>>().join(".");
            if let Some(&file_scope_id) = ctx.file_module_scopes.get(&mod_key) {
                let names: Vec<String> = scopes.names_in_scope(file_scope_id);
                for name in &names {
                    let _ = scopes.bind_wildcard(name, name);
                }
            }
        }
    }

    // Validate: detect imports of private items.
    // After all FileModules have exported their public names and glob imports ran,
    // any remaining Import placeholder whose name matches a module's private_names
    // set is a "cannot import private item" error.
    if !ctx.module_private_names.is_empty() {
        let global_names = scopes.names_in_current_scope();
        for (name, def_id) in &global_names {
            let def = scopes.get_def(*def_id);
            if def.kind != DefKind::Import || def.span == Span::dummy() {
                continue; // not an unresolved user import
            }
            for (mod_path, priv_names) in &ctx.module_private_names {
                if priv_names.contains(name.as_str()) {
                    errors.push(SemanticError {
                        kind: SemanticErrorKind::PrivateImport {
                            name: name.clone(),
                            module: mod_path.join("."),
                        },
                        span: def.span,
                    });
                }
            }
        }
    }


    // Fixup: re-resolve function return types that failed during collection.
    // In cross-module scenarios, entry file items come before imported module items
    // in the merged AST, so a function whose return type is an imported type gets
    // return_type_id: None on the first pass — the type isn't in scope yet.
    for item in &module.items {
        if let Item::Function(f) = &item.node {
            if let Some(def_id) = scopes.lookup(&f.name.node) {
                if let Some(fi) = ctx.function_info.get_mut(&def_id) {
                    if fi.return_type_id.is_none() {
                        let ret_type = types::ast_type_to_resolved(
                            &f.return_type.node,
                            f.return_type.span,
                            scopes,
                            types,
                        )
                        .ok();
                        // Async functions return Future[T] at call sites
                        fi.return_type_id = if f.qualifiers.is_async {
                            ret_type.map(|inner_tid| {
                                let future_def_id = scopes.lookup("Future").expect("Future not registered");
                                types.intern_generic(future_def_id, vec![inner_tid])
                            })
                        } else {
                            ret_type
                        };
                    }
                }
            }
        }
    }

    ctx
}

/// Collect the names of every type alias declared anywhere in `module`
/// (recursively, across the per-file FileModule wrappers). Must be called
/// BEFORE the meta pass (`evaluate_meta_consts`), which inlines aliases to
/// their targets and removes the `type X = …` declarations from the AST.
/// The returned set feeds `check_unresolved_imports` so a valid alias import
/// (`from xtd.ecs import Entity`, where `Entity = SlotKey`) is not flagged.
pub fn collect_type_alias_names(module: &Module) -> FxHashSet<String> {
    let mut out = FxHashSet::default();
    for it in module.all_items() {
        if let Item::TypeAlias(a) = it {
            out.insert(a.name.node.clone());
        }
    }
    out
}

/// Validate `from X import Y`: report `Y` when no loaded module defines it.
///
/// The authoritative "is this name defined" set is built directly from the
/// merged AST (`module.all_items()` unwraps the per-file FileModule wrappers)
/// plus the built-in generics and the pre-meta `alias_names` (type aliases are
/// erased by the meta pass before this runs, so they must be passed in). This
/// is intentionally NOT a scope lookup: non-generic enum variants and imported
/// type aliases never enter a scope's value/type maps (variants resolve lazily
/// at use sites via qualified paths; aliases are inlined at the import
/// boundary), so a `name_index` / namespace lookup would miss them and produce
/// false positives.
///
/// Catches the wrong-import footgun — `from std.async import sleep` when
/// std.async only exports `async_sleep`: the unresolved `sleep` otherwise
/// lowers to a bare C `sleep(...)` call that silently links against libc's
/// seconds-granularity `sleep`. Leaves resolvable imports untouched: builtin
/// generics (`Channel`), type aliases (`Entity`), bare enum-variant imports
/// (`from colors import Red`).
///
/// NOTE: must run on the fully-merged module (all loaded files concatenated).
/// A single non-entry source file analyzed in isolation (siblings not loaded)
/// can still report a false positive for a sibling import; that's acceptable
/// because the pipeline always analyzes via the project entry point.
pub fn check_unresolved_imports(
    module: &Module,
    alias_names: &FxHashSet<String>,
    errors: &mut Vec<SemanticError>,
) {
    let mut defined: FxHashSet<&str> = FxHashSet::default();
    for n in BUILTIN_GENERIC_TYPES {
        defined.insert(n);
    }
    for n in alias_names {
        defined.insert(n.as_str());
    }
    for it in module.all_items() {
        match it {
            Item::Function(f) => { defined.insert(f.name.node.as_str()); }
            Item::Struct(s) => { defined.insert(s.name.node.as_str()); }
            Item::Enum(e) => {
                defined.insert(e.name.node.as_str());
                for v in &e.variants {
                    defined.insert(v.node.name.node.as_str());
                }
            }
            Item::Trait(t) => { defined.insert(t.name.node.as_str()); }
            Item::TypeAlias(a) => { defined.insert(a.name.node.as_str()); }
            Item::Newtype(nt) => { defined.insert(nt.name.node.as_str()); }
            Item::ConstDecl(c) => { defined.insert(c.name.node.as_str()); }
            Item::StaticDecl(s) => { defined.insert(s.name.node.as_str()); }
            Item::ExternBlock(eb) => {
                for decl in &eb.items {
                    defined.insert(decl.node.name.node.as_str());
                }
            }
            _ => {}
        }
    }
    for item in &module.items {
        if let Item::Import(ImportStmt::From { path, names, wildcard, .. }) = &item.node {
            if *wildcard {
                continue;
            }
            for n in names {
                // Validate the SOURCE name (what the module must export), not the
                // local alias: `from std.math import sin as msin` is checked on
                // `sin`. The alias `msin` is only the local binding.
                if !defined.contains(n.name.node.as_str()) {
                    errors.push(SemanticError {
                        kind: SemanticErrorKind::UnresolvedImport {
                            name: n.name.node.clone(),
                            module: path.iter().map(|s| s.node.as_str()).collect::<Vec<_>>().join("."),
                        },
                        span: n.name.span,
                    });
                }
            }
        }
    }
}

fn collect_top_level_inner(
    module: &Module,
    scopes: &mut ScopeTable,
    types: &mut TypeTable,
    errors: &mut Vec<SemanticError>,
    ctx: &mut ResolveContext,
) {
    for item in &module.items {
        collect_item(&item.node, scopes, types, errors, ctx);
    }
}

/// Reject required parameters that follow a parameter with a default value.
/// Positional call-site resolution is ambiguous otherwise.
pub(super) fn validate_default_param_ordering(params: &[Spanned<Param>], errors: &mut Vec<SemanticError>) {
    let mut seen_default = false;
    for p in params {
        if p.node.default.is_some() {
            seen_default = true;
        } else if seen_default {
            errors.push(SemanticError {
                kind: SemanticErrorKind::RequiredAfterDefault {
                    name: p.node.name.node.clone(),
                },
                span: p.node.name.span,
            });
        }
    }
}


fn collect_item(
    item: &Item,
    scopes: &mut ScopeTable,
    types: &mut TypeTable,
    errors: &mut Vec<SemanticError>,
    ctx: &mut ResolveContext,
) {
    match item {
        Item::Function(f) => {
            match scopes.define(f.name.node.clone(), DefKind::Function, f.name.span) {
                Ok(def_id) => {
                    // Try to resolve return type
                    let ret_type = types::ast_type_to_resolved(
                        &f.return_type.node,
                        f.return_type.span,
                        scopes,
                        types,
                    )
                    .ok();

                    // Async functions return Future[T] at call sites
                    let ret_type = if f.qualifiers.is_async {
                        ret_type.map(|inner_tid| {
                            let future_def_id = scopes.lookup("Future").expect("Future not registered");
                            types.intern_generic(future_def_id, vec![inner_tid])
                        })
                    } else {
                        ret_type
                    };

                    let param_ownerships: Vec<Ownership> =
                        f.params.iter().map(|p| p.node.ownership).collect();
                    let param_names: Vec<String> =
                        f.params.iter().map(|p| p.node.name.node.clone()).collect();
                    let param_defaults: Vec<Option<Spanned<Expr>>> =
                        f.params.iter().map(|p| p.node.default.clone()).collect();

                    let param_type_ids: Vec<Option<TypeId>> = f
                        .params
                        .iter()
                        .map(|p| {
                            types::ast_type_to_resolved(
                                &p.node.type_.node,
                                p.node.type_.span,
                                scopes,
                                types,
                            )
                            .ok()
                        })
                        .collect();

                    validate_default_param_ordering(&f.params, errors);

                    // D26 auto-infer: by the time `collect_top_level` runs, the
                    // `rewrite_d26_auto_infer_throws` pass has already mutated
                    // `f.throws` to `Explicit(ArithError)` for any body
                    // containing `+!` / `-!` / `*!` / etc — so the
                    // `f.throws.explicit_type()` lookup below returns Some as if
                    // the user had written it, and `FunctionInfo.throws_type_id`
                    // reflects the auto-inferred signature transparently.
                    let throws_type_id = f.throws.explicit_type().and_then(|t| {
                        types::ast_type_to_resolved(&t.node, t.span, scopes, types).ok()
                    });
                    let generic_param_names = extract_generic_param_names(&f.generic_params);
                    let trait_bounds = extract_generic_bounds(&f.generic_params);
                    ctx.function_info.insert(
                        def_id,
                        FunctionInfo {
                            def_id,
                            return_type_id: ret_type,
                            param_type_ids,
                            param_ownerships,
                            param_names,
                            param_defaults,
                            throws: f.throws.declares_throws(),
                            throws_type_id,
                            is_async: f.qualifiers.is_async,
                            is_blocking: f.qualifiers.is_blocking,
                            is_noreturn: f.qualifiers.is_noreturn,
                            scope_id: scopes.current_scope(),
                            generic_param_names,
                            trait_bounds,
                            return_borrows_from: Vec::new(),
                            has_body: matches!(f.body, crate::parser::ast::FunctionBody::Block(_) | crate::parser::ast::FunctionBody::Expression(_)),
                            return_origin_is_static: false,
                        },
                    );
                }
                Err(e) => errors.push(e),
            }
        }

        Item::Struct(s) => {
            // Reject two fields with the same name in the declaration
            // (`struct P: int x; int x`). Previously this slipped through
            // resolution and only failed downstream at the C compiler
            // ("duplicate member"); reject it up front with a real
            // diagnostic on the SECOND field's name. The self-host mirrors
            // this in self_host_typechecker/typecheck.gg's IStruct arm.
            let mut seen_fields: std::collections::HashSet<&str> = std::collections::HashSet::new();
            for f in &s.fields {
                if !seen_fields.insert(f.node.name.node.as_str()) {
                    errors.push(SemanticError {
                        kind: SemanticErrorKind::DuplicateStructFieldDecl {
                            field: f.node.name.node.clone(),
                        },
                        span: f.node.name.span,
                    });
                }
            }
            match scopes.define(s.name.node.clone(), DefKind::Struct, s.name.span) {
                Ok(def_id) => {
                    let fields: Vec<(String, Span)> = s
                        .fields
                        .iter()
                        .map(|f| (f.node.name.node.clone(), f.span))
                        .collect();
                    // Populate BOTH unconditionally (NOT gated on trait bounds):
                    // a plain `struct Pair[A, B]` with no bounds must still get
                    // its param names so generic-struct field access can build a
                    // name→targ substitution. Mirrors the enum path.
                    let field_ast_types: Vec<Spanned<Type>> =
                        s.fields.iter().map(|f| f.node.type_.clone()).collect();
                    let generic_param_names = extract_generic_param_names(&s.generic_params);
                    ctx.struct_fields.insert(
                        def_id,
                        StructFieldInfo { fields, generic_param_names, field_ast_types },
                    );
                    let bounds = extract_generic_bounds(&s.generic_params);
                    if !bounds.is_empty() {
                        let param_names = extract_generic_param_names(&s.generic_params);
                        ctx.struct_generic_bounds.insert(def_id, (param_names, bounds));
                    }
                    // RV-A: seed the typed deref-wrapper kind on builtin-module
                    // wrapper STRUCTS (Box in std.collections; RWLock/ReadGuard/
                    // WriteGuard in std.sync), gated on the defining scope being
                    // a BUILTIN module — a USER `struct ReadGuard` gets `None`
                    // and so stops escaping E_NoFieldFound. `Box` needs seeding
                    // here as well as in BUILTIN_GENERIC_TYPES because
                    // `from std.collections import Box` resolves to THIS struct
                    // def, not the Import placeholder (the mid-scout miss).
                    if let Some(kind) = DerefWrapperKind::for_builtin_name(s.name.node.as_str()) {
                        let in_builtin = matches!(
                            scopes.scope_kind(scopes.current_scope()),
                            ScopeKind::FileModule { path } if crate::stdlib::is_builtin_module(path)
                        );
                        if in_builtin {
                            scopes.get_def_mut(def_id).deref_wrapper_kind = Some(kind);
                        }
                    }
                }
                Err(e) => errors.push(e),
            }
        }

        Item::Enum(e) => {
            match scopes.define(e.name.node.clone(), DefKind::Enum, e.name.span) {
                Ok(enum_def_id) => {
                    let mut variant_infos = Vec::new();
                    let mut variant_field_types = Vec::new();
                    // Non-generic user enum variants are NOT inserted into global scope.
                    // They are only accessible via qualified paths: Color.Red().
                    // Generic enum variants remain in scope since there is no feasible
                    // qualified syntax for them (e.g., Maybe[int].Just(42) doesn't parse).
                    let is_generic = e.generic_params.is_some();
                    for variant in &e.variants {
                        let variant_def_id = if is_generic {
                            // Generic enum: keep variant in scope (bare name still works)
                            match scopes.define(
                                variant.node.name.node.clone(),
                                DefKind::Variant,
                                variant.node.name.span,
                            ) {
                                Ok(id) => id,
                                Err(err) => { errors.push(err); continue; }
                            }
                        } else {
                            // Non-generic user enum: allocate without inserting into scope
                            scopes.alloc_def(
                                variant.node.name.node.clone(),
                                DefKind::Variant,
                                variant.node.name.span,
                            )
                        };
                        variant_infos.push((
                            variant.node.name.node.clone(),
                            variant_def_id,
                        ));
                        // Collect AST field types for pattern type inference
                        let field_types = match &variant.node.fields {
                            VariantFields::Tuple(types) => types.clone(),
                            VariantFields::Unit => Vec::new(),
                        };
                        variant_field_types.push((
                            variant.node.name.node.clone(),
                            field_types,
                        ));
                    }
                    let generic_param_names = extract_generic_param_names(&e.generic_params);
                    let bounds = extract_generic_bounds(&e.generic_params);
                    if !bounds.is_empty() {
                        let param_names_for_bounds = generic_param_names.clone();
                        ctx.struct_generic_bounds.insert(enum_def_id, (param_names_for_bounds, bounds));
                    }
                    ctx.enum_variants.insert(
                        enum_def_id,
                        EnumVariantInfo {
                            variants: variant_infos,
                            variant_field_types,
                            generic_param_names,
                        },
                    );
                }
                Err(e) => errors.push(e),
            }
        }

        Item::Trait(t) => {
            if let Err(e) = scopes.define(t.name.node.clone(), DefKind::Trait, t.name.span) {
                errors.push(e);
            }
        }

        Item::TypeAlias(a) => {
            if let Err(e) = scopes.define(a.name.node.clone(), DefKind::TypeAlias, a.name.span) {
                errors.push(e);
            }
        }

        Item::Newtype(n) => {
            if let Err(e) = scopes.define(n.name.node.clone(), DefKind::Newtype, n.name.span) {
                errors.push(e);
            }
        }

        Item::ConstDecl(c) => {
            if let Err(e) = scopes.define(c.name.node.clone(), DefKind::Const, c.name.span) {
                errors.push(e);
            }
        }

        Item::StaticDecl(s) => {
            if let Err(e) = scopes.define(s.name.node.clone(), DefKind::Static, s.name.span) {
                errors.push(e);
            }
        }

        Item::Import(import) => {
            collect_import(import, scopes, errors);
        }

        Item::Equip(impl_block) => {
            // Define equip method names in a temporary scope so they get unique DefIds
            // and populate function_info (needed for borrow checker origin/temporary tracking).
            scopes.push_scope(super::scope::ScopeKind::EquipBlock { self_type: None });
            for method in &impl_block.items {
                let f = &method.node;
                match scopes.define(f.name.node.clone(), DefKind::Function, f.name.span) {
                    Ok(def_id) => {
                        let ret_type = types::ast_type_to_resolved(
                            &f.return_type.node,
                            f.return_type.span,
                            scopes,
                            types,
                        )
                        .ok();

                        // Async methods return Future[T] at call sites
                        let ret_type = if f.qualifiers.is_async {
                            ret_type.map(|inner_tid| {
                                let future_def_id = scopes.lookup("Future").expect("Future not registered");
                                types.intern_generic(future_def_id, vec![inner_tid])
                            })
                        } else {
                            ret_type
                        };

                        let param_ownerships: Vec<Ownership> =
                            f.params.iter().map(|p| p.node.ownership).collect();
                        let param_names: Vec<String> =
                            f.params.iter().map(|p| p.node.name.node.clone()).collect();
                        let param_defaults: Vec<Option<Spanned<Expr>>> =
                            f.params.iter().map(|p| p.node.default.clone()).collect();
                        let param_type_ids: Vec<Option<TypeId>> = f
                            .params
                            .iter()
                            .map(|p| {
                                types::ast_type_to_resolved(
                                    &p.node.type_.node,
                                    p.node.type_.span,
                                    scopes,
                                    types,
                                )
                                .ok()
                            })
                            .collect();
                        validate_default_param_ordering(&f.params, errors);
                        // D26 auto-infer: the pre-`collect_top_level` rewrite
                        // pass already promoted this fn to `throws ArithError`
                        // if its body contained a fallible-arith op (silent
                        // owner ruling 2026-08-06), so `explicit_type()`
                        // returns Some transparently.
                        let throws_type_id = f.throws.explicit_type().and_then(|t| {
                            types::ast_type_to_resolved(&t.node, t.span, scopes, types).ok()
                        });
                        let generic_param_names = extract_generic_param_names(&f.generic_params);
                        let trait_bounds = extract_generic_bounds(&f.generic_params);
                        ctx.function_info.insert(
                            def_id,
                            FunctionInfo {
                                def_id,
                                return_type_id: ret_type,
                                param_type_ids,
                                param_ownerships,
                                param_names,
                                param_defaults,
                                throws: f.throws.declares_throws(),
                                throws_type_id,
                                is_async: f.qualifiers.is_async,
                                is_blocking: f.qualifiers.is_blocking,
                            is_noreturn: f.qualifiers.is_noreturn,
                                scope_id: scopes.current_scope(),
                                generic_param_names,
                                trait_bounds,
                                return_borrows_from: Vec::new(),
                                has_body: matches!(f.body, crate::parser::ast::FunctionBody::Block(_) | crate::parser::ast::FunctionBody::Expression(_)),
                                return_origin_is_static: false,
                            },
                        );
                    }
                    Err(e) => errors.push(e),
                }
            }
            scopes.pop_scope();
        }

        Item::ExternBlock(ext) => {
            for func in &ext.items {
                match scopes.define(
                    func.node.name.node.clone(),
                    DefKind::Function,
                    func.node.name.span,
                ) {
                    Ok(def_id) => {
                        let ret_type = types::ast_type_to_resolved(
                            &func.node.return_type.node,
                            func.node.return_type.span,
                            scopes,
                            types,
                        ).ok();
                        // Async extern functions return Future[T] at call sites
                        let ret_type = if func.node.qualifiers.is_async {
                            ret_type.map(|inner_tid| {
                                let future_def_id = scopes.lookup("Future").expect("Future not registered");
                                types.intern_generic(future_def_id, vec![inner_tid])
                            })
                        } else {
                            ret_type
                        };
                        let param_ownerships: Vec<Ownership> =
                            func.node.params.iter().map(|p| p.node.ownership).collect();
                        let param_names: Vec<String> =
                            func.node.params.iter().map(|p| p.node.name.node.clone()).collect();
                        let param_type_ids: Vec<Option<TypeId>> = func.node.params.iter()
                            .map(|p| types::ast_type_to_resolved(
                                &p.node.type_.node, p.node.type_.span, scopes, types,
                            ).ok())
                            .collect();
                        let param_count = func.node.params.len();
                        let throws_type_id = func.node.throws.explicit_type().and_then(|t| {
                            types::ast_type_to_resolved(&t.node, t.span, scopes, types).ok()
                        });
                        let generic_param_names = extract_generic_param_names(&func.node.generic_params);
                        ctx.function_info.insert(def_id, FunctionInfo {
                            def_id,
                            return_type_id: ret_type,
                            param_type_ids,
                            param_ownerships,
                            param_names,
                            param_defaults: vec![None; param_count],
                            throws: func.node.throws.declares_throws(),
                            throws_type_id,
                            is_async: func.node.qualifiers.is_async,
                            is_blocking: func.node.qualifiers.is_blocking,
                            is_noreturn: func.node.qualifiers.is_noreturn,
                            scope_id: scopes.current_scope(),
                            generic_param_names,
                            trait_bounds: Vec::new(),
                            return_borrows_from: Vec::new(),
                            has_body: false,
                            return_origin_is_static: true,
                        });
                    }
                    Err(e) => errors.push(e),
                }
            }
        }

        Item::Directive(_) => {
            // Directives are handled during codegen, not name resolution.
        }

        Item::Test(_) | Item::Bench(_) | Item::SuiteSetup(_) | Item::SuiteTeardown(_) => {
            // Test/bench items don't define top-level names.
        }

        Item::MetaConst(_) | Item::MetaType(_) | Item::MetaTypeFunc(_)
        | Item::MetaAssert(_) | Item::MetaIf(_) | Item::MetaLog(_) => {
            // Meta items resolved during meta evaluation pass (not yet implemented).
        }

        Item::Module { path, items } => {
            // Compute the set of explicitly-private names in this module.
            let private_names: FxHashSet<String> = items.iter()
                .filter_map(|si| {
                    let vis = match &si.node {
                        Item::Function(f) => f.visibility,
                        Item::Struct(s) => s.visibility,
                        Item::Enum(e) => e.visibility,
                        Item::Trait(t) => t.visibility,
                        Item::ConstDecl(c) => c.visibility,
                        Item::StaticDecl(s) => s.visibility,
                        Item::TypeAlias(a) => a.visibility,
                        Item::Newtype(n) => n.visibility,
                        _ => return None,
                    };
                    if vis == Visibility::Private {
                        Some(match &si.node {
                            Item::Function(f) => f.name.node.clone(),
                            Item::Struct(s) => s.name.node.clone(),
                            Item::Enum(e) => e.name.node.clone(),
                            Item::Trait(t) => t.name.node.clone(),
                            Item::ConstDecl(c) => c.name.node.clone(),
                            Item::StaticDecl(s) => s.name.node.clone(),
                            Item::TypeAlias(a) => a.name.node.clone(),
                            Item::Newtype(n) => n.name.node.clone(),
                            _ => unreachable!(),
                        })
                    } else {
                        None
                    }
                })
                .collect();

            // Push a file-module scope and collect all items (public + private) into it.
            let file_scope_id = scopes.push_scope(ScopeKind::FileModule { path: path.clone() });
            ctx.file_module_scopes.insert(path.join("."), file_scope_id);

            for si in items {
                collect_item(&si.node, scopes, types, errors, ctx);
            }

            // Promote non-private names to the enclosing global scope. Cross-module
            // collisions (two modules both publicly declaring the same name) come back
            // as `(name, existing_def_id, new_def_id)`. Emit a clear DuplicateDefinition
            // error citing both spans so users see the conflict at semantic time, instead
            // of broken C codegen at link time — the type-mangling layer is currently
            // flat across modules, so two user types named `ParseError` (e.g. one in
            // `std.conv`, one in the user's `parser.gg`) collapse to a single C symbol
            // and the linker either picks the wrong layout or fails outright.
            let collisions = scopes.export_non_private(&private_names);
            for (name, existing_id, new_id) in collisions {
                let existing_span = scopes.get_def(existing_id).span;
                let new_span = scopes.get_def(new_id).span;
                errors.push(SemanticError {
                    kind: SemanticErrorKind::DuplicateDefinition {
                        name,
                        original: existing_span,
                    },
                    span: new_span,
                });
            }

            // Remember private names for post-collection import validation.
            if !private_names.is_empty() {
                ctx.module_private_names.push((path.clone(), private_names));
            }

            scopes.pop_scope();
        }
    }
}

fn collect_import(import: &ImportStmt, scopes: &mut ScopeTable, errors: &mut Vec<SemanticError>) {
    match import {
        ImportStmt::Simple { path, .. } => {
            // `import std.io` — define the last segment
            if let Some(last) = path.last() {
                if let Err(e) =
                    scopes.define(last.node.clone(), DefKind::Import, last.span)
                {
                    errors.push(e);
                }
            }
        }
        ImportStmt::Grouped { names, .. } => {
            for name in names {
                if let Err(e) = scopes.define(name.node.clone(), DefKind::Import, name.span) {
                    errors.push(e);
                }
            }
        }
        ImportStmt::From { names, glob_types, wildcard, .. } => {
            // Module-level wildcard `from X import *` registers nothing up front —
            // the wildcard expansion happens in a post-merge fixup pass once the
            // imported module's public names are visible in the parent scope.
            if !*wildcard {
                for n in names {
                    let local = n.local_name();
                    if let Err(e) = scopes.define(local.node.clone(), DefKind::Import, local.span) {
                        errors.push(e);
                    }
                }
            }
            // Glob type names (`EnumName.*`) register the type itself as Import (if not already).
            // Their variant registration happens in the second pass of collect_top_level.
            for name in glob_types {
                // Silently ignore duplicate: the type may already be imported by a regular name.
                let _ = scopes.define(name.node.clone(), DefKind::Import, name.span);
            }
        }
    }
}

// ─── Pass 2: Resolve Bodies ────────────────────────────────────

/// Resolve names inside all function bodies, returning the resolution map.
pub fn resolve_bodies(
    module: &Module,
    scopes: &mut ScopeTable,
    types: &mut TypeTable,
    errors: &mut Vec<SemanticError>,
    function_info: &mut FxHashMap<DefId, FunctionInfo>,
    function_body_scopes: &mut FxHashMap<(String, usize), super::ids::ScopeId>,
    file_module_scopes: &FxHashMap<String, super::ids::ScopeId>,
) -> ResolutionMap {
    let mut resolution_map = ResolutionMap::default();

    for item in &module.items {
        resolve_item_body(&item.node, scopes, types, errors, &mut resolution_map, function_info, function_body_scopes, file_module_scopes);
    }

    resolution_map
}

fn resolve_item_body(
    item: &Item,
    scopes: &mut ScopeTable,
    types: &mut TypeTable,
    errors: &mut Vec<SemanticError>,
    resolution_map: &mut ResolutionMap,
    function_info: &mut FxHashMap<DefId, FunctionInfo>,
    function_body_scopes: &mut FxHashMap<(String, usize), super::ids::ScopeId>,
    file_module_scopes: &FxHashMap<String, super::ids::ScopeId>,
) {
    match item {
        Item::Function(f) => {
            resolve_function(f, scopes, types, errors, resolution_map, function_info, function_body_scopes);
        }
        Item::Equip(impl_block) => {
            resolve_equip_block(impl_block, scopes, types, errors, resolution_map, function_info, function_body_scopes);
        }
        Item::ConstDecl(c) => {
            resolve_expr(&c.value, scopes, errors, resolution_map);
        }
        Item::StaticDecl(s) => {
            resolve_expr(&s.value, scopes, errors, resolution_map);
        }
        Item::Test(t) => {
            scopes.push_scope(super::scope::ScopeKind::Function);
            resolve_block(&t.body, scopes, types, errors, resolution_map);
            scopes.pop_scope();
        }
        Item::Bench(b) => {
            scopes.push_scope(super::scope::ScopeKind::Function);
            resolve_block(&b.body, scopes, types, errors, resolution_map);
            scopes.pop_scope();
        }
        Item::SuiteSetup(s) => {
            scopes.push_scope(super::scope::ScopeKind::Function);
            resolve_block(&s.body, scopes, types, errors, resolution_map);
            scopes.pop_scope();
        }
        Item::SuiteTeardown(s) => {
            scopes.push_scope(super::scope::ScopeKind::Function);
            resolve_block(&s.body, scopes, types, errors, resolution_map);
            scopes.pop_scope();
        }
        // Nested module: enter the FileModule scope so private names are visible.
        Item::Module { path, items } => {
            let mod_key = path.join(".");
            let prev = if let Some(&scope_id) = file_module_scopes.get(&mod_key) {
                Some(scopes.enter_scope(scope_id))
            } else {
                None
            };
            for si in items {
                resolve_item_body(&si.node, scopes, types, errors, resolution_map, function_info, function_body_scopes, file_module_scopes);
            }
            if let Some(prev_scope) = prev {
                scopes.restore_scope(prev_scope);
            }
        }
        // Other items don't have bodies to resolve
        _ => {}
    }
}

fn resolve_function(
    f: &FunctionDef,
    scopes: &mut ScopeTable,
    types: &mut TypeTable,
    errors: &mut Vec<SemanticError>,
    resolution_map: &mut ResolutionMap,
    function_info: &mut FxHashMap<DefId, FunctionInfo>,
    function_body_scopes: &mut FxHashMap<(String, usize), super::ids::ScopeId>,
) {
    let body_scope = scopes.push_scope(super::scope::ScopeKind::Function);

    // Record the body scope for this function (used by codegen for scope-aware lookups).
    // Keyed by (name, span_start) to avoid collisions between different source files.
    function_body_scopes.insert((f.name.node.clone(), f.name.span.start), body_scope);

    // Update the FunctionInfo with the actual body scope (was set to module scope during collection)
    if let Some(def_id) = scopes.lookup_def_by_span(&f.name.node, f.name.span) {
        if let Some(fi) = function_info.get_mut(&def_id) {
            fi.scope_id = body_scope;
        }
    }

    // Define generic type params
    if let Some(generics) = &f.generic_params {
        for param in &generics.node.params {
            match &param.node {
                GenericParam::Type { name, .. } => {
                    if let Err(e) =
                        scopes.define(name.node.clone(), DefKind::GenericParam, name.span)
                    {
                        errors.push(e);
                    }
                }
                GenericParam::Const { name, .. } => {
                    if let Err(e) =
                        scopes.define(name.node.clone(), DefKind::Const, name.span)
                    {
                        errors.push(e);
                    }
                }
            }
        }
    }

    // Define parameters (always mutable — can reassign params in function body)
    for param in &f.params {
        match scopes.define_with_mutability(
            param.node.name.node.clone(),
            DefKind::Variable,
            param.node.name.span,
            true,
        ) {
            Ok(def_id) => {
                let def = scopes.get_def_mut(def_id);
                def.is_param = true;
                def.param_ownership = Some(param.node.ownership);
            }
            Err(e) => {
                errors.push(e);
            }
        }
    }

    // Resolve body
    match &f.body {
        FunctionBody::Block(block) => {
            resolve_block(block, scopes, types, errors, resolution_map);
        }
        FunctionBody::Expression(expr) => {
            resolve_expr(expr, scopes, errors, resolution_map);
        }
        FunctionBody::Declaration | FunctionBody::Extern(_) => {}
    }

    scopes.pop_scope();
}

fn resolve_equip_block(
    impl_block: &EquipBlock,
    scopes: &mut ScopeTable,
    types: &mut TypeTable,
    errors: &mut Vec<SemanticError>,
    resolution_map: &mut ResolutionMap,
    function_info: &mut FxHashMap<DefId, FunctionInfo>,
    function_body_scopes: &mut FxHashMap<(String, usize), super::ids::ScopeId>,
) {
    scopes.push_scope(super::scope::ScopeKind::EquipBlock { self_type: None });

    // Define generic params for the impl block
    if let Some(generics) = &impl_block.generic_params {
        for param in &generics.node.params {
            if let GenericParam::Type { name, .. } = &param.node {
                if let Err(e) =
                    scopes.define(name.node.clone(), DefKind::GenericParam, name.span)
                {
                    errors.push(e);
                }
            }
        }
    }

    // Resolve each method
    for method in &impl_block.items {
        resolve_function(&method.node, scopes, types, errors, resolution_map, function_info, function_body_scopes);
    }

    scopes.pop_scope();
}

fn resolve_block(
    block: &Block,
    scopes: &mut ScopeTable,
    types: &mut TypeTable,
    errors: &mut Vec<SemanticError>,
    resolution_map: &mut ResolutionMap,
) {
    for stmt in &block.stmts {
        resolve_stmt(&stmt.node, stmt.span, scopes, types, errors, resolution_map);
    }
}

fn resolve_stmt(
    stmt: &Stmt,
    _span: Span,
    scopes: &mut ScopeTable,
    types: &mut TypeTable,
    errors: &mut Vec<SemanticError>,
    resolution_map: &mut ResolutionMap,
) {
    match stmt {
        Stmt::VarDecl {
            is_const, is_mutable, shared, pattern, value, ..
        } => {
            // Resolve value first (before defining the variable, so `int x = x` refers to outer x)
            resolve_expr(value, scopes, errors, resolution_map);
            // Define bindings from pattern
            let (kind, mutable) = if *is_const {
                (DefKind::Const, false)
            } else {
                (DefKind::Variable, *is_mutable)
            };
            define_pattern_bindings_with_kind(&pattern.node, pattern.span, scopes, errors, kind, mutable);
            // Mark shared bindings on their DefInfo + add to resolution_map for IR lowering
            if *shared != crate::parser::ast::SharedKind::None {
                if let crate::parser::ast::Pattern::Binding(name) = &pattern.node {
                    if let Some(def_id) = scopes.lookup_from_scope(scopes.current_scope(), name) {
                        scopes.get_def_mut(def_id).shared = *shared;
                        resolution_map.insert(pattern.span.start, def_id);
                    }
                }
            }
        }

        Stmt::Expr(expr) => {
            resolve_expr(expr, scopes, errors, resolution_map);
        }

        Stmt::Assign { target, value } => {
            resolve_expr(target, scopes, errors, resolution_map);
            resolve_expr(value, scopes, errors, resolution_map);
        }

        Stmt::CompoundAssign { target, value, .. } => {
            resolve_expr(target, scopes, errors, resolution_map);
            resolve_expr(value, scopes, errors, resolution_map);
        }

        Stmt::Return(expr) => {
            if let Some(expr) = expr {
                resolve_expr(expr, scopes, errors, resolution_map);
            }
        }

        Stmt::Throw(expr) => {
            resolve_expr(expr, scopes, errors, resolution_map);
        }

        Stmt::Break | Stmt::Continue | Stmt::Pass => {}

        Stmt::For {
            pattern,
            iterable,
            body,
            else_body,
            ..
        } => {
            resolve_expr(iterable, scopes, errors, resolution_map);
            scopes.push_scope(super::scope::ScopeKind::ForLoop);
            define_pattern_bindings(&pattern.node, pattern.span, scopes, errors, false);
            resolve_block(body, scopes, types, errors, resolution_map);
            scopes.pop_scope();
            if let Some(else_body) = else_body {
                scopes.push_scope(super::scope::ScopeKind::Block);
                resolve_block(else_body, scopes, types, errors, resolution_map);
                scopes.pop_scope();
            }
        }

        Stmt::While {
            condition,
            body,
            else_body,
        } => {
            // Push body scope first so compound `is` bindings are visible to guards
            scopes.push_scope(super::scope::ScopeKind::Block);
            if has_is_patterns(&condition.node) {
                resolve_is_condition(condition, scopes, errors, resolution_map);
            } else {
                resolve_expr(condition, scopes, errors, resolution_map);
            }
            resolve_block(body, scopes, types, errors, resolution_map);
            scopes.pop_scope();
            if let Some(else_body) = else_body {
                scopes.push_scope(super::scope::ScopeKind::Block);
                resolve_block(else_body, scopes, types, errors, resolution_map);
                scopes.pop_scope();
            }
        }

        Stmt::Loop { body } => {
            scopes.push_scope(super::scope::ScopeKind::ForLoop); // reuse ForLoop kind for loops
            resolve_block(body, scopes, types, errors, resolution_map);
            scopes.pop_scope();
        }

        Stmt::If {
            condition,
            then_body,
            elif_branches,
            else_body,
        } => {
            // Push body scope first so compound `is` bindings are visible to guards
            scopes.push_scope(super::scope::ScopeKind::Block);
            if has_is_patterns(&condition.node) {
                resolve_is_condition(condition, scopes, errors, resolution_map);
            } else {
                resolve_expr(condition, scopes, errors, resolution_map);
            }
            resolve_block(then_body, scopes, types, errors, resolution_map);
            scopes.pop_scope();

            for (cond, body) in elif_branches {
                scopes.push_scope(super::scope::ScopeKind::Block);
                if has_is_patterns(&cond.node) {
                    resolve_is_condition(cond, scopes, errors, resolution_map);
                } else {
                    resolve_expr(cond, scopes, errors, resolution_map);
                }
                resolve_block(body, scopes, types, errors, resolution_map);
                scopes.pop_scope();
            }

            if let Some(else_body) = else_body {
                scopes.push_scope(super::scope::ScopeKind::Block);
                resolve_block(else_body, scopes, types, errors, resolution_map);
                scopes.pop_scope();
            }
        }

        Stmt::Match {
            scrutinee,
            arms,
            else_arm,
        } => {
            resolve_expr(scrutinee, scopes, errors, resolution_map);
            for arm in arms.iter().filter_map(|i| i.arm()) {
                scopes.push_scope(super::scope::ScopeKind::Block);
                define_match_arm_pattern(&arm.pattern.node, arm.pattern.span, scopes, errors, resolution_map);
                if let Some(guard) = &arm.guard {
                    resolve_expr(guard, scopes, errors, resolution_map);
                }
                resolve_expr(&arm.body, scopes, errors, resolution_map);
                scopes.pop_scope();
            }
            if let Some(else_arm) = else_arm {
                scopes.push_scope(super::scope::ScopeKind::Block);
                resolve_block(else_arm, scopes, types, errors, resolution_map);
                scopes.pop_scope();
            }
        }

        Stmt::Select { arms, else_arm } => {
            for arm in arms {
                scopes.push_scope(super::scope::ScopeKind::Block);
                match &arm.op {
                    SelectOp::Recv { name, channel, .. } => {
                        resolve_expr(channel, scopes, errors, resolution_map);
                        if let Ok(def_id) = scopes.define(name.node.clone(), super::scope::DefKind::Variable, name.span) {
                            resolution_map.insert(name.span.start, def_id);
                        }
                    }
                    SelectOp::Send { channel, value } => {
                        resolve_expr(channel, scopes, errors, resolution_map);
                        resolve_expr(value, scopes, errors, resolution_map);
                    }
                }
                resolve_block(&arm.body, scopes, types, errors, resolution_map);
                scopes.pop_scope();
            }
            if let Some(else_arm) = else_arm {
                scopes.push_scope(super::scope::ScopeKind::Block);
                resolve_block(else_arm, scopes, types, errors, resolution_map);
                scopes.pop_scope();
            }
        }

        Stmt::With { bindings, body } => {
            scopes.push_scope(super::scope::ScopeKind::Block);
            for binding in bindings {
                resolve_expr(&binding.expr, scopes, errors, resolution_map);
                if let Err(e) = scopes.define_with_mutability(
                    binding.name.node.clone(),
                    DefKind::Variable,
                    binding.name.span,
                    false,
                ) {
                    errors.push(e);
                }
            }
            resolve_block(body, scopes, types, errors, resolution_map);
            scopes.pop_scope();
        }

        Stmt::Unsafe { body } => {
            scopes.push_scope(super::scope::ScopeKind::Block);
            resolve_block(body, scopes, types, errors, resolution_map);
            scopes.pop_scope();
        }

        Stmt::NamedScope { body, .. } => {
            scopes.push_scope(super::scope::ScopeKind::Block);
            resolve_block(body, scopes, types, errors, resolution_map);
            scopes.pop_scope();
        }

        Stmt::Assert { condition, message } => {
            resolve_expr(condition, scopes, errors, resolution_map);
            if let Some(msg) = message {
                resolve_expr(msg, scopes, errors, resolution_map);
            }
        }
        Stmt::AssertReturn { condition, message } => {
            resolve_expr(condition, scopes, errors, resolution_map);
            if let Some(msg) = message {
                resolve_expr(msg, scopes, errors, resolution_map);
            }
        }

        Stmt::Snapshot { value, .. } => {
            resolve_expr(value, scopes, errors, resolution_map);
        }

        Stmt::Item(item) => {
            // Nested item definitions
            let mut ctx = ResolveContext::new();
            collect_item(item, scopes, types, errors, &mut ctx);
            resolve_item_body(item, scopes, types, errors, resolution_map, &mut ctx.function_info, &mut ctx.function_body_scopes, &ctx.file_module_scopes);
        }

        Stmt::MetaIf { then_body, elif_branches, else_body, .. } => {
            // Conditions are meta expressions (not runtime): skip resolve_expr on them.
            // Bodies contain regular code that must be fully resolved.
            scopes.push_scope(super::scope::ScopeKind::Block);
            resolve_block(then_body, scopes, types, errors, resolution_map);
            scopes.pop_scope();
            for (_, body) in elif_branches {
                scopes.push_scope(super::scope::ScopeKind::Block);
                resolve_block(body, scopes, types, errors, resolution_map);
                scopes.pop_scope();
            }
            if let Some(eb) = else_body {
                scopes.push_scope(super::scope::ScopeKind::Block);
                resolve_block(eb, scopes, types, errors, resolution_map);
                scopes.pop_scope();
            }
        }

        Stmt::MetaFor { vars, body, .. } => {
            // Range is a meta expression: skip resolve_expr on it.
            // Body contains regular code that must be resolved. Iter-vars
            // (single-var integer/string range; two-var variant_payloads
            // destructure) are bound as DkVariable in the body's Block scope
            // so `f"{fname}"` inside the body resolves rather than being
            // swallowed by a sink. Materialisation happens per-mono at
            // lowering; here we just register the names. Mirrors SH
            // reference (`self_host_resolver/resolve.gg:618-627`).
            scopes.push_scope(super::scope::ScopeKind::Block);
            for v in vars {
                if let Err(e) = scopes.define(v.node.clone(), DefKind::Variable, v.span) {
                    errors.push(e);
                }
            }
            resolve_block(body, scopes, types, errors, resolution_map);
            scopes.pop_scope();
        }

        Stmt::MetaMatch { arms, else_arm, .. } => {
            // Scrutinee and case exprs are meta expressions: skip resolve_expr on them.
            // Bodies contain regular code that must be fully resolved.
            for (_, body) in arms {
                scopes.push_scope(super::scope::ScopeKind::Block);
                resolve_block(body, scopes, types, errors, resolution_map);
                scopes.pop_scope();
            }
            if let Some(eb) = else_arm {
                scopes.push_scope(super::scope::ScopeKind::Block);
                resolve_block(eb, scopes, types, errors, resolution_map);
                scopes.pop_scope();
            }
        }

        Stmt::MetaWhile { body, .. } => {
            // Condition is a meta expression: skip resolve_expr on it.
            // Body contains regular code that must be fully resolved.
            scopes.push_scope(super::scope::ScopeKind::Block);
            resolve_block(body, scopes, types, errors, resolution_map);
            scopes.pop_scope();
        }

        Stmt::MetaConst { name, .. } => {
            // Value is entirely a meta expression — evaluated at monomorphization
            // time; do NOT resolve it against scope. But BIND the const name so
            // subsequent statements in the same block (e.g. `f"{idx}"` after
            // `meta const idx = enum_ordinal(T, vname)`) resolve rather than
            // being swallowed by a sink. Per language reference §19.11.1, the
            // binding leaks to subsequent statements — no scope push/pop.
            // Mirrors SH reference (`self_host_resolver/resolve.gg:610-616`).
            if let Err(e) = scopes.define(name.node.clone(), DefKind::Variable, name.span) {
                errors.push(e);
            }
        }

        Stmt::MetaLog { .. } => {
            // Args are meta expressions (typename(T), sizeof(T), etc.) — skip resolution.
        }

        Stmt::OnError { body } => {
            scopes.push_scope(super::scope::ScopeKind::Block);
            resolve_block(body, scopes, types, errors, resolution_map);
            scopes.pop_scope();
        }
    }
}

fn resolve_expr(
    expr: &Spanned<Expr>,
    scopes: &mut ScopeTable,
    errors: &mut Vec<SemanticError>,
    resolution_map: &mut ResolutionMap,
) {
    match &expr.node {
        // Literals — no resolution needed
        Expr::IntLiteral(_)
        | Expr::FloatLiteral(_)
        | Expr::BoolLiteral(_)
        | Expr::NoneLiteral
        | Expr::It => {}

        // `self` is bound as an ordinary parameter (DefKind::Variable,
        // is_param = true) in `resolve_function`. Wire each usage site to
        // that DefId so the safety layer's place primitives
        // (`find_root_def_id[_with_path]`) can root self-projected places
        // (`self.a.b`) for aliasing / move / borrow checks — exactly as
        // for an identifier-rooted place. `lookup` returns None outside a
        // method (SelfExpr cannot appear there), leaving behavior unchanged.
        Expr::SelfExpr => {
            if let Some(def_id) = scopes.lookup("self") {
                resolution_map.insert(expr.span.start, def_id);
            }
        }

        Expr::StringLiteral(_, interp_exprs) => {
            // Resolve each pre-parsed interpolation expression against the
            // SHARED errors vec so genuine undefined names inside `f"{...}"`
            // reject just as they would outside a f-string. Meta-for iter-vars
            // and meta-const names are bound as DkVariable in their body
            // scopes (see `Stmt::MetaFor` / `Stmt::MetaConst` arms above), so
            // interpolations like `f"{fname}"` inside `meta for fname in ...`
            // continue to resolve. Mirrors SH reference
            // (`self_host_resolver/resolve.gg:668-676`).
            for interp in interp_exprs {
                resolve_expr(interp, scopes, errors, resolution_map);
            }
        }

        Expr::Identifier(name) => {
            match scopes.lookup(name) {
                Some(def_id) => {
                    resolution_map.insert(expr.span.start, def_id);
                }
                None => {
                    // Don't error on built-in functions like `print`, or synthetic
                    // identifiers like `__return__` (bound during IR lowering).
                    //
                    // Also skip when `name` is a known enum-variant name: the loader's
                    // pre-merge variant qualifier (`build_variant_map_from_all` in
                    // `src/loader.rs`) drops ambiguous bare names from its rewrite map,
                    // leaving the bare `Identifier` for the downstream typechecker to
                    // resolve via `decl_type_hint` (the constructor-call expected-type
                    // path, mirroring how `lower_pattern_condition` uses the scrutinee
                    // type for the pattern path). Non-generic variants are allocated
                    // via `alloc_def` (recorded in `name_index`) but not inserted into
                    // any scope; `is_known_variant_name` consults that index so we
                    // stay silent here. Real undefined names — no variant anywhere
                    // by that spelling — still report normally.
                    if !is_builtin(name)
                        && name != "__return__"
                        && !scopes.is_known_variant_name(name)
                    {
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::UndefinedName {
                                name: name.clone(),
                                suggestion: scopes.suggest_name(name),
                            },
                            span: expr.span,
                        });
                    }
                }
            }
        }

        Expr::Path { segments } => {
            // Resolve the first segment
            if let Some(first) = segments.first() {
                match scopes.lookup(&first.node) {
                    Some(def_id) => {
                        resolution_map.insert(first.span.start, def_id);
                    }
                    None => {
                        if !is_builtin(&first.node) {
                            errors.push(SemanticError {
                                kind: SemanticErrorKind::UndefinedName {
                                    name: first.node.clone(),
                                    suggestion: scopes.suggest_name(&first.node),
                                },
                                span: first.span,
                            });
                        }
                    }
                }
            }
        }

        Expr::UnaryOp { operand, .. } => {
            resolve_expr(operand, scopes, errors, resolution_map);
        }

        Expr::BinaryOp { left, right, .. } => {
            resolve_expr(left, scopes, errors, resolution_map);
            resolve_expr(right, scopes, errors, resolution_map);
        }

        Expr::Call { callee, args, .. } => {
            // field_value(obj, fname) and field_set(obj, fname, value) are
            // compile-time rewrite builtins. The callee is not a real function and the
            // field-name arg is a meta-loop variable or string literal — not a runtime
            // identifier. Skip their resolution to avoid spurious "undefined name" errors;
            // the actual rewrite happens during meta substitution or the rewrite pass.
            if let Expr::Identifier(cname) = &callee.node {
                if cname == "field_value" && args.len() == 2 {
                    // Only resolve arg0 (the object expression).
                    resolve_expr(&args[0].node.value, scopes, errors, resolution_map);
                    return;
                }
                if cname == "field_set" && args.len() == 3 {
                    // Resolve arg0 (the object) and arg2 (the value). Skip arg1 (field name).
                    resolve_expr(&args[0].node.value, scopes, errors, resolution_map);
                    resolve_expr(&args[2].node.value, scopes, errors, resolution_map);
                    return;
                }
                // make_variant(T, "Variant") is a compile-time rewrite builtin.
                // arg0 is a type name (no runtime resolution needed), arg1 is a string literal.
                // Skip callee and arg1 resolution to avoid spurious errors.
                if cname == "make_variant" && args.len() == 2 {
                    // arg0 is a type name — no resolution needed (types aren't in the value scope).
                    return;
                }
            }
            resolve_expr(callee, scopes, errors, resolution_map);
            for arg in args {
                resolve_expr(&arg.node.value, scopes, errors, resolution_map);
            }
        }

        Expr::MethodCall {
            receiver, args, ..
        } => {
            resolve_expr(receiver, scopes, errors, resolution_map);
            for arg in args {
                resolve_expr(&arg.node.value, scopes, errors, resolution_map);
            }
            // Method name is resolved during type checking
        }

        Expr::FieldAccess { object, .. } => {
            resolve_expr(object, scopes, errors, resolution_map);
        }

        Expr::TupleFieldAccess { object, .. } => {
            resolve_expr(object, scopes, errors, resolution_map);
        }

        Expr::Index { object, index } => {
            resolve_expr(object, scopes, errors, resolution_map);
            resolve_expr(index, scopes, errors, resolution_map);
        }

        Expr::Range { start, end, .. } => {
            if let Some(start) = start {
                resolve_expr(start, scopes, errors, resolution_map);
            }
            if let Some(end) = end {
                resolve_expr(end, scopes, errors, resolution_map);
            }
        }

        Expr::OptionalChain { object, .. } => {
            resolve_expr(object, scopes, errors, resolution_map);
        }

        Expr::DefaultOp { lhs, rhs } => {
            resolve_expr(lhs, scopes, errors, resolution_map);
            resolve_expr(rhs, scopes, errors, resolution_map);
        }

        Expr::Move { expr: inner }
        | Expr::Propagate { expr: inner }
        | Expr::MutableBorrow { expr: inner }
        | Expr::Deref { expr: inner }
        | Expr::Await { expr: inner }
        | Expr::Spawn { expr: inner, .. }
        | Expr::SpawnBlocking { expr: inner, .. } => {
            resolve_expr(inner, scopes, errors, resolution_map);
        }

        Expr::If {
            condition,
            then_branch,
            elif_branches,
            else_branch,
        } => {
            resolve_expr(condition, scopes, errors, resolution_map);
            resolve_expr(then_branch, scopes, errors, resolution_map);
            for (cond, body) in elif_branches {
                resolve_expr(cond, scopes, errors, resolution_map);
                resolve_expr(body, scopes, errors, resolution_map);
            }
            if let Some(else_branch) = else_branch {
                resolve_expr(else_branch, scopes, errors, resolution_map);
            }
        }

        Expr::Match {
            scrutinee,
            arms,
            else_arm,
        } => {
            resolve_expr(scrutinee, scopes, errors, resolution_map);
            for arm in arms {
                scopes.push_scope(super::scope::ScopeKind::Block);
                define_match_arm_pattern(&arm.pattern.node, arm.pattern.span, scopes, errors, resolution_map);
                if let Some(guard) = &arm.guard {
                    resolve_expr(guard, scopes, errors, resolution_map);
                }
                resolve_expr(&arm.body, scopes, errors, resolution_map);
                scopes.pop_scope();
            }
            if let Some(else_arm) = else_arm {
                resolve_expr(else_arm, scopes, errors, resolution_map);
            }
        }

        Expr::Block(block) => {
            scopes.push_scope(super::scope::ScopeKind::Block);
            resolve_block(block, scopes, &mut TypeTable::new(), errors, resolution_map);
            scopes.pop_scope();
        }

        Expr::Do { body } => {
            scopes.push_scope(super::scope::ScopeKind::Block);
            resolve_block(body, scopes, &mut TypeTable::new(), errors, resolution_map);
            scopes.pop_scope();
        }

        Expr::Closure {
            params, body, ..
        } => {
            scopes.push_scope(super::scope::ScopeKind::Function);
            for param in params {
                if let Err(e) = scopes.define(
                    param.node.name.node.clone(),
                    DefKind::Variable,
                    param.node.name.span,
                ) {
                    errors.push(e);
                }
            }
            resolve_expr(body, scopes, errors, resolution_map);
            scopes.pop_scope();
        }

        Expr::ImplicitClosure { body } => {
            scopes.push_scope(super::scope::ScopeKind::Function);
            // Define implicit `it` parameter
            if let Ok(def_id) = scopes.define("it".into(), DefKind::Variable, expr.span) {
                scopes.get_def_mut(def_id).is_param = true;
            }
            resolve_expr(body, scopes, errors, resolution_map);
            scopes.pop_scope();
        }

        Expr::ListComprehension {
            expr: comp_expr,
            variable,
            iterable,
            condition,
            ..
        } => {
            resolve_expr(iterable, scopes, errors, resolution_map);
            scopes.push_scope(super::scope::ScopeKind::ForLoop);
            define_pattern_bindings(&variable.node, variable.span, scopes, errors, false);
            resolve_expr(comp_expr, scopes, errors, resolution_map);
            if let Some(cond) = condition {
                resolve_expr(cond, scopes, errors, resolution_map);
            }
            scopes.pop_scope();
        }

        Expr::DictComprehension {
            key,
            value,
            variables,
            iterable,
            condition,
        } => {
            resolve_expr(iterable, scopes, errors, resolution_map);
            scopes.push_scope(super::scope::ScopeKind::ForLoop);
            for var in variables {
                if let Err(e) = scopes.define(var.node.clone(), DefKind::Variable, var.span) {
                    errors.push(e);
                }
            }
            resolve_expr(key, scopes, errors, resolution_map);
            resolve_expr(value, scopes, errors, resolution_map);
            if let Some(cond) = condition {
                resolve_expr(cond, scopes, errors, resolution_map);
            }
            scopes.pop_scope();
        }

        Expr::SetComprehension {
            expr: comp_expr,
            variable,
            iterable,
            condition,
        } => {
            resolve_expr(iterable, scopes, errors, resolution_map);
            scopes.push_scope(super::scope::ScopeKind::ForLoop);
            if let Err(e) =
                scopes.define(variable.node.clone(), DefKind::Variable, variable.span)
            {
                errors.push(e);
            }
            resolve_expr(comp_expr, scopes, errors, resolution_map);
            if let Some(cond) = condition {
                resolve_expr(cond, scopes, errors, resolution_map);
            }
            scopes.pop_scope();
        }

        Expr::ArrayLiteral(elements) | Expr::TupleLiteral(elements) => {
            for elem in elements {
                resolve_expr(elem, scopes, errors, resolution_map);
            }
        }

        Expr::DictLiteral(pairs) => {
            for (k, v) in pairs {
                resolve_expr(k, scopes, errors, resolution_map);
                resolve_expr(v, scopes, errors, resolution_map);
            }
        }

        Expr::StructLiteral { name, args, .. } => {
            // Resolve struct name
            match scopes.lookup(&name.node) {
                Some(def_id) => {
                    resolution_map.insert(name.span.start, def_id);
                }
                None => {
                    errors.push(SemanticError {
                        kind: SemanticErrorKind::UndefinedName {
                            name: name.node.clone(),
                            suggestion: scopes.suggest_name(&name.node),
                        },
                        span: name.span,
                    });
                }
            }
            for arg in args {
                resolve_expr(arg, scopes, errors, resolution_map);
            }
        }

        Expr::As { expr: inner, .. } => {
            resolve_expr(inner, scopes, errors, resolution_map);
        }

        Expr::Is { expr: inner, .. } => {
            resolve_expr(inner, scopes, errors, resolution_map);
        }

        // Dot-shorthand: resolve arg expressions; enum name resolved at type-check time
        Expr::DotShorthand { args, .. } => {
            for arg in args {
                resolve_expr(&arg.node.value, scopes, errors, resolution_map);
            }
        }
        Expr::MetaOpInfix { left, right, .. } => {
            resolve_expr(left, scopes, errors, resolution_map);
            resolve_expr(right, scopes, errors, resolution_map);
        }
        Expr::MetaOpToken(_) => {}
        Expr::Rethrow { expr, error_binding, transform } => {
            resolve_expr(expr, scopes, errors, resolution_map);
            // Snag #37: the `(Type name)` payload-binding form introduces
            // `name` into scope for the transform expression. Without the
            // scope-and-define dance, the resolver reports "undefined name"
            // when the user references the bound name in the transform —
            // even though IR-lowering correctly binds it at codegen time.
            // Type assignment happens later in the typechecker; resolver
            // only registers the name.
            if let Some((_ty, name)) = error_binding {
                scopes.push_scope(super::scope::ScopeKind::Block);
                if let Err(e) = scopes.define(name.node.clone(), DefKind::Variable, name.span) {
                    errors.push(e);
                }
                resolve_expr(transform, scopes, errors, resolution_map);
                scopes.pop_scope();
            } else {
                resolve_expr(transform, scopes, errors, resolution_map);
            }
        }
        Expr::Catch { expr, error_binding, recovery } => {
            resolve_expr(expr, scopes, errors, resolution_map);
            // Snag #37: `catch (name): recovery` binds the error value to
            // `name` in the recovery expression. The binding's type is
            // inferred from the throws-error type of `expr` at typecheck
            // time; here we just register the name so resolver lookups
            // succeed.
            scopes.push_scope(super::scope::ScopeKind::Block);
            if let Err(e) = scopes.define(
                error_binding.node.clone(),
                DefKind::Variable,
                error_binding.span,
            ) {
                errors.push(e);
            }
            resolve_expr(recovery, scopes, errors, resolution_map);
            scopes.pop_scope();
        }
    }
}

/// Check if a condition expression contains any `is` patterns (possibly compound).
fn has_is_patterns(expr: &Expr) -> bool {
    match expr {
        Expr::Is { negated: false, .. } => true,
        Expr::BinaryOp { left, op: BinaryOp::And, right } => {
            has_is_patterns(&left.node) || has_is_patterns(&right.node)
        }
        _ => false,
    }
}

/// Resolve a condition that may contain `is` patterns, defining bindings left-to-right
/// so that guards in compound conditions can reference earlier bindings.
/// For example, in `a is Some(x) and x > 10`, `x` is defined before `x > 10` is resolved.
/// Must be called with the body scope already pushed.
fn resolve_is_condition(
    expr: &Spanned<Expr>,
    scopes: &mut ScopeTable,
    errors: &mut Vec<SemanticError>,
    resolution_map: &mut ResolutionMap,
) {
    match &expr.node {
        Expr::BinaryOp { left, op: BinaryOp::And, right } => {
            resolve_is_condition(left, scopes, errors, resolution_map);
            resolve_is_condition(right, scopes, errors, resolution_map);
        }
        Expr::Is { expr: inner, negated: false, pattern, .. } => {
            resolve_expr(inner, scopes, errors, resolution_map);
            define_pattern_bindings(&pattern.node, expr.span, scopes, errors, false);
        }
        _ => {
            resolve_expr(expr, scopes, errors, resolution_map);
        }
    }
}

/// Collect all binding names introduced by a pattern (for or-pattern validation).
fn collect_pattern_names(pattern: &Pattern) -> Vec<String> {
    match pattern {
        Pattern::Binding(name) => vec![name.clone()],
        Pattern::Constructor { fields, .. } | Pattern::DotShorthand { fields, .. } => {
            fields.iter().flat_map(|f| collect_pattern_names(&f.node)).collect()
        }
        Pattern::Tuple(elems) => {
            elems.iter().flat_map(|e| collect_pattern_names(&e.node)).collect()
        }
        Pattern::Or(alts) => {
            alts.first().map(|a| collect_pattern_names(&a.node)).unwrap_or_default()
        }
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => vec![],
    }
}

/// Define bindings introduced by a pattern (always as `DefKind::Variable`).
fn define_pattern_bindings(
    pattern: &Pattern,
    span: Span,
    scopes: &mut ScopeTable,
    errors: &mut Vec<SemanticError>,
    is_mutable: bool,
) {
    define_pattern_bindings_with_kind(pattern, span, scopes, errors, DefKind::Variable, is_mutable);
}

/// Match-arm-specific pattern resolution: when a `Pattern::Binding(name)`
/// at top level of a `case` pattern resolves to an outer-scope
/// `DefKind::Const` or `DefKind::Static`, treat the pattern as a value
/// comparison (`case FOO:` ≡ `case <FOO's value>:`) rather than a new
/// variable binding. Records the resolution in `resolution_map` so
/// IR-lowering can emit equality-compare instead of always-true.
///
/// Recursion handles tuple destructure (each element checked) and
/// or-patterns (each alternative checked). Constructor and DotShorthand
/// patterns delegate to the existing `define_pattern_bindings` path —
/// their nested binding names are variable bindings against the
/// destructured payload, not constant comparisons against the scrutinee.
///
/// Snag (2026-05-13): `match x: case CONST_NAME:` previously bound
/// `CONST_NAME` as a fresh variable shadowing the constant, making
/// every input route to the first arm. Filed against
/// `format_type_id` / `binop_name` / etc. integer-tag dispatch shape.
fn define_match_arm_pattern(
    pattern: &Pattern,
    span: Span,
    scopes: &mut ScopeTable,
    errors: &mut Vec<SemanticError>,
    resolution_map: &mut ResolutionMap,
) {
    if let Pattern::Binding(name) = pattern {
        if let Some(def_id) = scopes.lookup(name) {
            let kind = scopes.get_def(def_id).kind;
            if matches!(kind, DefKind::Const | DefKind::Static) {
                resolution_map.insert(span.start, def_id);
                return; // skip new-variable definition — pattern is a constant compare
            }
        }
        // Fall through: not a constant, bind as variable.
    }
    if let Pattern::Tuple(elements) = pattern {
        for elem in elements {
            define_match_arm_pattern(&elem.node, elem.span, scopes, errors, resolution_map);
        }
        return;
    }
    if let Pattern::Or(alternatives) = pattern {
        // Validate that all alternatives bind the same set of names.
        if alternatives.len() >= 2 {
            let first_names: std::collections::BTreeSet<_> =
                collect_pattern_names(&alternatives[0].node).into_iter().collect();
            for alt in &alternatives[1..] {
                let alt_names: std::collections::BTreeSet<_> =
                    collect_pattern_names(&alt.node).into_iter().collect();
                if first_names != alt_names {
                    let missing: Vec<_> = first_names.difference(&alt_names).cloned().collect();
                    let extra: Vec<_> = alt_names.difference(&first_names).cloned().collect();
                    errors.push(SemanticError {
                        kind: SemanticErrorKind::OrPatternBindingMismatch { missing, extra },
                        span: alt.span,
                    });
                }
            }
        }
        for alt in alternatives {
            define_match_arm_pattern(&alt.node, alt.span, scopes, errors, resolution_map);
        }
        return;
    }
    // Constructor / DotShorthand / Literal / Wildcard / Rest — delegate.
    define_pattern_bindings(pattern, span, scopes, errors, false);
}

/// Define bindings introduced by a pattern with an explicit DefKind.
fn define_pattern_bindings_with_kind(
    pattern: &Pattern,
    span: Span,
    scopes: &mut ScopeTable,
    errors: &mut Vec<SemanticError>,
    kind: DefKind,
    is_mutable: bool,
) {
    match pattern {
        Pattern::Binding(name) => {
            if let Err(e) = scopes.define_with_mutability(name.clone(), kind, span, is_mutable) {
                errors.push(e);
            }
        }
        Pattern::Constructor { fields, .. } => {
            for field in fields {
                define_pattern_bindings_with_kind(&field.node, field.span, scopes, errors, kind, is_mutable);
            }
        }
        Pattern::Tuple(elements) => {
            for elem in elements {
                define_pattern_bindings_with_kind(&elem.node, elem.span, scopes, errors, kind, is_mutable);
            }
        }
        Pattern::Or(alternatives) => {
            // Validate that all alternatives bind the same set of names.
            if alternatives.len() >= 2 {
                let first_names: std::collections::BTreeSet<_> =
                    collect_pattern_names(&alternatives[0].node).into_iter().collect();
                for alt in &alternatives[1..] {
                    let alt_names: std::collections::BTreeSet<_> =
                        collect_pattern_names(&alt.node).into_iter().collect();
                    if first_names != alt_names {
                        let missing: Vec<_> = first_names.difference(&alt_names).cloned().collect();
                        let extra: Vec<_> = alt_names.difference(&first_names).cloned().collect();
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::OrPatternBindingMismatch { missing, extra },
                            span: alt.span,
                        });
                    }
                }
            }
            // Bind from the first alternative.
            if let Some(first) = alternatives.first() {
                define_pattern_bindings_with_kind(&first.node, first.span, scopes, errors, kind, is_mutable);
            }
        }
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => {}

        // Dot-shorthand: define bindings in sub-patterns
        Pattern::DotShorthand { fields, .. } => {
            for field in fields {
                define_pattern_bindings_with_kind(&field.node, field.span, scopes, errors, kind, is_mutable);
            }
        }
    }
}

/// Extract generic type-parameter names from a `GenericParams`.
fn extract_generic_param_names(generics: &Option<Spanned<GenericParams>>) -> Vec<String> {
    match generics {
        Some(g) => g
            .node
            .params
            .iter()
            .filter_map(|p| match &p.node {
                GenericParam::Type { name, .. } => Some(name.node.clone()),
                _ => None,
            })
            .collect(),
        None => Vec::new(),
    }
}

/// Extract inline trait bounds from generic params as `(param_name, [trait_name, ...])`.
/// Only includes params that have at least one bound.
fn extract_generic_bounds(
    generic_params: &Option<Spanned<GenericParams>>,
) -> Vec<(String, Vec<String>)> {
    match generic_params {
        Some(gp) => gp
            .node
            .params
            .iter()
            .filter_map(|p| match &p.node {
                GenericParam::Type { name, bounds } if !bounds.is_empty() => {
                    let traits: Vec<String> =
                        bounds.iter().map(|tb| tb.node.name.node.clone()).collect();
                    Some((name.node.clone(), traits))
                }
                _ => None,
            })
            .collect(),
        None => Vec::new(),
    }
}

/// Check if a name is a compiler intrinsic (always available without imports).
/// Collection types live in `std.collections`; traits and Option/Result are
/// pre-registered in `collect_top_level`.
fn is_builtin(name: &str) -> bool {
    matches!(name, "print" | "format" | "len" | "type" | "panic"
        | "int" | "int8" | "int16" | "int32" | "int64"
        | "uint" | "uint8" | "uint16" | "uint32" | "uint64"
        | "float" | "float32" | "float64"
        | "bool" | "byte" | "str"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn parse_and_collect(source: &str) -> (ScopeTable, TypeTable, Vec<SemanticError>) {
        let mut parser = Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);

        let mut scopes = ScopeTable::new();
        let mut types = TypeTable::new();
        let mut errors = Vec::new();
        collect_top_level(&module, &mut scopes, &mut types, &mut errors);
        (scopes, types, errors)
    }

    fn parse_and_resolve(source: &str) -> (ScopeTable, TypeTable, ResolutionMap, Vec<SemanticError>) {
        let mut parser = Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);

        let mut scopes = ScopeTable::new();
        let mut types = TypeTable::new();
        let mut errors = Vec::new();
        let mut ctx = collect_top_level(&module, &mut scopes, &mut types, &mut errors);
        let resolution_map = resolve_bodies(&module, &mut scopes, &mut types, &mut errors, &mut ctx.function_info, &mut ctx.function_body_scopes, &ctx.file_module_scopes);
        (scopes, types, resolution_map, errors)
    }

    #[test]
    fn collect_function() {
        let (scopes, _, errors) = parse_and_collect("int add(int a, int b): a + b\n");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert!(scopes.lookup("add").is_some());
    }

    #[test]
    fn collect_struct() {
        let (scopes, _, errors) = parse_and_collect("struct Point:\n    float x\n    float y\n");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert!(scopes.lookup("Point").is_some());
    }

    #[test]
    fn collect_enum_with_variants() {
        let (scopes, _, errors) =
            parse_and_collect("enum Color:\n    Red\n    Green\n    Blue\n");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        // Enum type is in scope; non-generic variants are NOT
        // (they are accessible only via qualified syntax: Color.Red())
        assert!(scopes.lookup("Color").is_some());
        assert!(scopes.lookup("Red").is_none(), "non-generic variant Red should not be in global scope");
        assert!(scopes.lookup("Green").is_none(), "non-generic variant Green should not be in global scope");
        assert!(scopes.lookup("Blue").is_none(), "non-generic variant Blue should not be in global scope");
    }

    #[test]
    fn builtin_option_result_registered() {
        let (scopes, _, errors) = parse_and_collect("");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert!(scopes.lookup("Option").is_some());
        assert!(scopes.lookup("Some").is_some());
        assert!(scopes.lookup("None").is_some());
        assert!(scopes.lookup("Result").is_some());
        assert!(scopes.lookup("Ok").is_some());
        assert!(scopes.lookup("Error").is_some());
    }

    #[test]
    fn duplicate_definition() {
        let (_, _, errors) = parse_and_collect("int foo(): 1\nint foo(): 2\n");
        assert_eq!(errors.len(), 1);
        match &errors[0].kind {
            SemanticErrorKind::DuplicateDefinition { name, .. } => {
                assert_eq!(name, "foo");
            }
            _ => panic!("expected DuplicateDefinition"),
        }
    }

    #[test]
    fn forward_reference() {
        let source = "void main():\n    auto x = helper()\nint helper(): 42\n";
        let (_, _, _, errors) = parse_and_resolve(source);
        // helper should be resolved (forward reference)
        assert!(
            errors.is_empty(),
            "expected no errors for forward reference, got: {:?}",
            errors
        );
    }

    #[test]
    fn undefined_variable() {
        let source = "void main():\n    int x = undefined_var\n";
        let (_, _, _, errors) = parse_and_resolve(source);
        assert!(!errors.is_empty());
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            SemanticErrorKind::UndefinedName { name, .. } if name == "undefined_var"
        )));
    }

    #[test]
    fn scoping_inner_shadows_outer() {
        let source = "\
void main():
    int x = 1
    if x > 0:
        int x = 2
        print(\"{x}\")
";
        let (_, _, _, errors) = parse_and_resolve(source);
        // Inner x shadows outer — no errors expected (inner scope allows redefinition)
        assert!(errors.is_empty(), "errors: {:?}", errors);
    }

    #[test]
    fn import_defines_names() {
        // The unresolved-import check lives in `analyze` (after the module is
        // loaded), not in `collect_top_level`, so collecting an import in
        // isolation registers the in-scope placeholders without error.
        let (scopes, _, errors) =
            parse_and_collect("from std.fmt import Formatter, format\n");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert!(scopes.lookup("Formatter").is_some());
        assert!(scopes.lookup("format").is_some());
    }

    #[test]
    fn str_plain_param_accepted() {
        let (_, _, errors) = parse_and_collect("void greet(str name): print(name)\n");
        assert!(errors.is_empty(), "expected no errors, got: {:?}", errors);
    }

    #[test]
    fn forward_return_type_fixup() {
        // Simulate cross-module ordering: function referencing a type defined later
        let source = "\
Point origin(): Point(0, 0)
struct Point:
    int x
    int y
";
        let mut parser = Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);

        let mut scopes = ScopeTable::new();
        let mut types = TypeTable::new();
        let mut errors = Vec::new();
        let ctx = collect_top_level(&module, &mut scopes, &mut types, &mut errors);

        // The fixup pass should have resolved the return type
        let origin_def_id = scopes.lookup("origin").expect("origin not defined");
        let fi = ctx.function_info.get(&origin_def_id).expect("no FunctionInfo for origin");
        assert!(
            fi.return_type_id.is_some(),
            "return_type_id should be resolved after fixup pass"
        );
    }

    /// Build a Module AST that simulates `from mymod import helper`
    /// where `helper` is private in `mymod`.
    fn make_private_import_module() -> Module {
        use crate::span::Span;
        let dummy = Span::new(0, 0);

        // Parse the private function from source text.
        let mut parser = Parser::new("private int helper(): 42\nint public_fn(): 1\n");
        let inner_module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);

        // Build the merged module:
        // 1. `from mymod import helper` (creates Import placeholder)
        // 2. Module { path: ["mymod"], items: [private helper, public_fn] }
        let import_item = Spanned {
            node: Item::Import(ImportStmt::From {
                path: vec![Spanned { node: "mymod".to_string(), span: dummy }],
                names: vec![ImportName {
                    name: Spanned { node: "helper".to_string(), span: Span::new(100, 106) },
                    alias: None,
                }],
                glob_types: vec![],
                wildcard: false,
                span: dummy,
            }),
            span: dummy,
        };
        let module_item = Spanned {
            node: Item::Module {
                path: vec!["mymod".to_string()],
                items: inner_module.items,
            },
            span: dummy,
        };

        Module {
            items: vec![import_item, module_item],
            span: dummy,
        }
    }

    #[test]
    fn private_import_error() {
        let module = make_private_import_module();
        let mut scopes = ScopeTable::new();
        let mut types = TypeTable::new();
        let mut errors = Vec::new();
        let _ctx = collect_top_level(&module, &mut scopes, &mut types, &mut errors);

        // Should produce a PrivateImport error for `helper`
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                SemanticErrorKind::PrivateImport { name, module }
                    if name == "helper" && module == "mymod"
            )),
            "expected PrivateImport error for 'helper', got: {:?}",
            errors
        );
    }

    #[test]
    fn public_import_no_error() {
        use crate::span::Span;
        let dummy = Span::new(0, 0);

        let mut parser = Parser::new("int public_fn(): 1\n");
        let inner_module = parser.parse_module();
        assert!(parser.errors.is_empty());

        let import_item = Spanned {
            node: Item::Import(ImportStmt::From {
                path: vec![Spanned { node: "mymod".to_string(), span: dummy }],
                names: vec![ImportName {
                    name: Spanned { node: "public_fn".to_string(), span: Span::new(100, 109) },
                    alias: None,
                }],
                glob_types: vec![],
                wildcard: false,
                span: dummy,
            }),
            span: dummy,
        };
        let module_item = Spanned {
            node: Item::Module {
                path: vec!["mymod".to_string()],
                items: inner_module.items,
            },
            span: dummy,
        };

        let module = Module {
            items: vec![import_item, module_item],
            span: dummy,
        };

        let mut scopes = ScopeTable::new();
        let mut types = TypeTable::new();
        let mut errors = Vec::new();
        let _ctx = collect_top_level(&module, &mut scopes, &mut types, &mut errors);

        // No PrivateImport errors — public_fn is public
        assert!(
            !errors.iter().any(|e| matches!(&e.kind, SemanticErrorKind::PrivateImport { .. })),
            "unexpected PrivateImport error: {:?}",
            errors
        );
        // public_fn should be accessible
        assert!(scopes.lookup("public_fn").is_some());
    }

    #[test]
    fn unresolved_import_flags_missing_name() {
        use crate::span::Span;
        let dummy = Span::new(0, 0);

        // Module defines `present_fn` but the import asks for `missing_fn` too.
        let mut parser = Parser::new("int present_fn(): 1\n");
        let inner_module = parser.parse_module();
        assert!(parser.errors.is_empty());

        let import_item = Spanned {
            node: Item::Import(ImportStmt::From {
                path: vec![Spanned { node: "mymod".to_string(), span: dummy }],
                names: vec![
                    ImportName { name: Spanned { node: "present_fn".to_string(), span: Span::new(1, 2) }, alias: None },
                    ImportName { name: Spanned { node: "missing_fn".to_string(), span: Span::new(3, 4) }, alias: None },
                ],
                glob_types: vec![],
                wildcard: false,
                span: dummy,
            }),
            span: dummy,
        };
        let module_item = Spanned {
            node: Item::Module { path: vec!["mymod".to_string()], items: inner_module.items },
            span: dummy,
        };
        let module = Module { items: vec![import_item, module_item], span: dummy };

        let alias_names = collect_type_alias_names(&module);
        let mut errors = Vec::new();
        check_unresolved_imports(&module, &alias_names, &mut errors);

        // Exactly one UnresolvedImport, for `missing_fn` — `present_fn` resolves.
        let unresolved: Vec<_> = errors.iter().filter_map(|e| match &e.kind {
            SemanticErrorKind::UnresolvedImport { name, .. } => Some(name.as_str()),
            _ => None,
        }).collect();
        assert_eq!(unresolved, vec!["missing_fn"], "errors: {:?}", errors);
    }

    #[test]
    fn unresolved_import_allows_alias_target() {
        use crate::span::Span;
        let dummy = Span::new(0, 0);

        // `from mymod import real_fn as r` — the SOURCE name must resolve, not the alias.
        let mut parser = Parser::new("int real_fn(): 1\n");
        let inner_module = parser.parse_module();
        assert!(parser.errors.is_empty());

        let import_item = Spanned {
            node: Item::Import(ImportStmt::From {
                path: vec![Spanned { node: "mymod".to_string(), span: dummy }],
                names: vec![ImportName {
                    name: Spanned { node: "real_fn".to_string(), span: Span::new(1, 2) },
                    alias: Some(Spanned { node: "r".to_string(), span: Span::new(3, 4) }),
                }],
                glob_types: vec![],
                wildcard: false,
                span: dummy,
            }),
            span: dummy,
        };
        let module_item = Spanned {
            node: Item::Module { path: vec!["mymod".to_string()], items: inner_module.items },
            span: dummy,
        };
        let module = Module { items: vec![import_item, module_item], span: dummy };

        let alias_names = collect_type_alias_names(&module);
        let mut errors = Vec::new();
        check_unresolved_imports(&module, &alias_names, &mut errors);
        assert!(
            !errors.iter().any(|e| matches!(e.kind, SemanticErrorKind::UnresolvedImport { .. })),
            "aliased import of a real name should not flag: {:?}",
            errors
        );
    }

    #[test]
    fn unresolved_import_allows_type_alias() {
        use crate::span::Span;
        let dummy = Span::new(0, 0);

        // `from mymod import MyAlias` where `type MyAlias = int` — must not flag.
        let mut parser = Parser::new("type MyAlias = int\n");
        let inner_module = parser.parse_module();
        assert!(parser.errors.is_empty());

        let import_item = Spanned {
            node: Item::Import(ImportStmt::From {
                path: vec![Spanned { node: "mymod".to_string(), span: dummy }],
                names: vec![ImportName {
                    name: Spanned { node: "MyAlias".to_string(), span: Span::new(1, 2) },
                    alias: None,
                }],
                glob_types: vec![],
                wildcard: false,
                span: dummy,
            }),
            span: dummy,
        };
        let module_item = Spanned {
            node: Item::Module { path: vec!["mymod".to_string()], items: inner_module.items },
            span: dummy,
        };
        let module = Module { items: vec![import_item, module_item], span: dummy };

        // Capture alias names while the `type` decl is still in the AST (mirrors
        // the real pipeline, where this runs before the meta pass erases aliases).
        let alias_names = collect_type_alias_names(&module);
        let mut errors = Vec::new();
        check_unresolved_imports(&module, &alias_names, &mut errors);
        assert!(
            !errors.iter().any(|e| matches!(e.kind, SemanticErrorKind::UnresolvedImport { .. })),
            "imported type alias should not flag: {:?}",
            errors
        );
    }

    #[test]
    fn private_struct_import_error() {
        use crate::span::Span;
        let dummy = Span::new(0, 0);

        let mut parser = Parser::new("private struct Secret:\n    int value\n");
        let inner_module = parser.parse_module();
        assert!(parser.errors.is_empty());

        let import_item = Spanned {
            node: Item::Import(ImportStmt::From {
                path: vec![Spanned { node: "mymod".to_string(), span: dummy }],
                names: vec![ImportName {
                    name: Spanned { node: "Secret".to_string(), span: Span::new(200, 206) },
                    alias: None,
                }],
                glob_types: vec![],
                wildcard: false,
                span: dummy,
            }),
            span: dummy,
        };
        let module_item = Spanned {
            node: Item::Module {
                path: vec!["mymod".to_string()],
                items: inner_module.items,
            },
            span: dummy,
        };

        let module = Module {
            items: vec![import_item, module_item],
            span: dummy,
        };

        let mut scopes = ScopeTable::new();
        let mut types = TypeTable::new();
        let mut errors = Vec::new();
        let _ctx = collect_top_level(&module, &mut scopes, &mut types, &mut errors);

        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                SemanticErrorKind::PrivateImport { name, .. } if name == "Secret"
            )),
            "expected PrivateImport error for 'Secret', got: {:?}",
            errors
        );
    }

    #[test]
    fn private_enum_glob_import_error() {
        use crate::span::Span;
        let dummy = Span::new(0, 0);

        let mut parser = Parser::new("private enum Status:\n    Active\n    Inactive\n");
        let inner_module = parser.parse_module();
        assert!(parser.errors.is_empty());

        // `from mymod import Status.*` — glob import of private enum
        let import_item = Spanned {
            node: Item::Import(ImportStmt::From {
                path: vec![Spanned { node: "mymod".to_string(), span: dummy }],
                names: vec![],
                glob_types: vec![Spanned { node: "Status".to_string(), span: Span::new(300, 306) }],
                wildcard: false,
                span: dummy,
            }),
            span: dummy,
        };
        let module_item = Spanned {
            node: Item::Module {
                path: vec!["mymod".to_string()],
                items: inner_module.items,
            },
            span: dummy,
        };

        let module = Module {
            items: vec![import_item, module_item],
            span: dummy,
        };

        let mut scopes = ScopeTable::new();
        let mut types = TypeTable::new();
        let mut errors = Vec::new();
        let _ctx = collect_top_level(&module, &mut scopes, &mut types, &mut errors);

        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                SemanticErrorKind::PrivateImport { name, .. } if name == "Status"
            )),
            "expected PrivateImport error for 'Status' glob import, got: {:?}",
            errors
        );
        // Variants should NOT be in scope (enum is private)
        assert!(scopes.lookup("Active").is_none(), "Active variant should not be imported from private enum");
        assert!(scopes.lookup("Inactive").is_none(), "Inactive variant should not be imported from private enum");
    }

    #[test]
    fn required_after_default_rejected() {
        let (_, _, errors) = parse_and_collect("int foo(int a = 1, int b): a + b\n");
        assert_eq!(errors.len(), 1);
        match &errors[0].kind {
            SemanticErrorKind::RequiredAfterDefault { name } => {
                assert_eq!(name, "b");
            }
            other => panic!("expected RequiredAfterDefault, got: {other:?}"),
        }
    }

    #[test]
    fn defaults_at_end_accepted() {
        let (_, _, errors) = parse_and_collect("int foo(int a, int b = 2, int c = 3): a + b + c\n");
        assert!(errors.is_empty(), "expected no errors, got: {:?}", errors);
    }

    #[test]
    fn no_defaults_accepted() {
        let (_, _, errors) = parse_and_collect("int foo(int a, int b): a + b\n");
        assert!(errors.is_empty(), "expected no errors, got: {:?}", errors);
    }

    #[test]
    fn two_traits_same_file() {
        let (scopes, _, errors) = parse_and_collect(
            "trait Drawable:\n    str draw(self)\n\ntrait Measurable:\n    float measure(self)\n",
        );
        assert!(errors.is_empty(), "expected no errors, got: {:?}", errors);
        assert!(scopes.lookup("Drawable").is_some());
        assert!(scopes.lookup("Measurable").is_some());
    }
}
