pub mod safety;
pub mod cycle_check;
pub mod derive;
pub mod errors;
pub mod ids;
pub mod lint_suggest_throws;
pub mod lint_xor_likely_power;
pub mod meta;
pub mod purity;
pub mod resolve;
pub mod rewrite;
pub mod scope;
pub mod traits;
pub mod type_utils;
pub mod typecheck;
pub mod types;

use rustc_hash::FxHashMap;
use std::time::{Duration, Instant};

use crate::parser::ast::{Item, Module};
use crate::span::Span;
use errors::{SemanticError, SemanticErrorKind, SemanticWarning};
use ids::{DefId, TypeId};
use resolve::{EnumVariantInfo, FunctionInfo, ResolutionMap, StructFieldInfo};
use scope::ScopeTable;
use scope::DerefWrapperKind as ScopeDerefWrapperKind;
use traits::TraitRegistry;
use types::TypeTable;

/// The resolution of a method call — the resolved definition plus, when
/// D36 auto-deref fired, the wrapper kind whose inner supplied the method.
///
/// Design (`docs/define-gorget/decisions.md` D36, 2026-07-27): the ratified
/// shape is a single record on `method_resolutions`, NOT a parallel
/// sidecar. Consumers that only care about the def read `.def_id`; the
/// lowering reads `.auto_deref` to decide whether to project the receiver
/// through the wrapper's `get_ptr` helper before dispatch.
///
/// `def_id` is `None` when the resolved method is a BUILTIN on the inner
/// type (no user `FunctionInfo`) — the borrow checker skips such entries
/// (nothing to consult on ownership), and the lowering dispatches through
/// the builtin path once the receiver is projected through `get_ptr`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MethodResolution {
    pub def_id: Option<DefId>,
    pub auto_deref: Option<ScopeDerefWrapperKind>,
}

impl MethodResolution {
    /// Convenience: a resolution with no auto-deref (direct method-on-type
    /// dispatch). Every call site that previously stored a bare `DefId`
    /// now stores `MethodResolution::direct(def_id)`.
    pub fn direct(def_id: DefId) -> Self {
        MethodResolution { def_id: Some(def_id), auto_deref: None }
    }
}

/// CFA (Custody Flow Analysis) decision for a `shared` binding.
/// Determines what synchronization primitive the compiler wraps the binding in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SharedStrategy {
    /// `Shared[T]` — no mutable borrows cross concurrency boundaries.
    ArcOnly,
    /// `Mutex[T]` — mutable borrows cross spawn boundaries.
    ArcMutex,
    /// `RwLock[T]` — user override via `shared(rwlock)`.
    ArcRwLock,
    /// Atomic — user override via `shared(atomic)` (scalars only).
    ArcAtomic,
}

/// The result of semantic analysis.
pub struct AnalysisResult {
    pub scopes: ScopeTable,
    pub types: TypeTable,
    pub traits: TraitRegistry,
    pub errors: Vec<SemanticError>,
    pub resolution_map: ResolutionMap,
    pub struct_fields: FxHashMap<DefId, StructFieldInfo>,
    pub enum_variants: FxHashMap<DefId, EnumVariantInfo>,
    pub function_info: FxHashMap<DefId, FunctionInfo>,
    /// Map from expression span to inferred TypeId (for Result-based `?` codegen).
    pub expr_types: FxHashMap<Span, TypeId>,
    /// Maps (function_name, span_start) → body scope id (for ALL functions including equip methods).
    pub function_body_scopes: FxHashMap<(String, usize), ids::ScopeId>,
    /// Maps method-call span start → resolved method + optional auto-deref
    /// wrapper kind (D36: extended value type replaces the parallel sidecar).
    /// Threaded from typechecker through borrow checker to codegen for
    /// ownership-aware move-zeroing at call sites.
    pub method_resolutions: FxHashMap<usize, MethodResolution>,
    /// Snag #11: for each cross-error-type auto-propagation site whose error
    /// is convertible via an equipped `From[CalleeE]` on the caller's
    /// `CallerE`, the resolved `From::from` method DefId, keyed by the
    /// producing call expression's span. The IR lowering reads this to emit
    /// the conversion on the error value before re-wrapping it in the caller's
    /// `Result`. Empty when all propagations are same-error-type.
    pub from_conversions: FxHashMap<Span, DefId>,
    /// CFA decisions for `shared` bindings: DefId → resolved sync strategy.
    pub shared_bindings: FxHashMap<DefId, SharedStrategy>,
    /// Non-fatal warnings (e.g., unnecessary `shared`).
    pub warnings: Vec<SemanticWarning>,
    /// Inferred function purity: name → Purity level.
    pub fn_purity: purity::PurityByName,
    /// Borrow dependencies: borrower DefId → Vec<source DefId>.
    /// Used by the drop elaborator to order drops correctly.
    pub borrow_deps: rustc_hash::FxHashMap<DefId, Vec<DefId>>,
    /// Per-sub-pass cumulative wall-clock time. Empty unless instrumentation
    /// was enabled (via `analyze_with_stats` or surfaced through `gg profile`).
    /// Mirrors the LIR `OptStats::pass_times` pattern so the dominant sub-pass
    /// shows up in `gg profile` JSON without per-call-site instrumentation.
    pub pass_times: FxHashMap<&'static str, Duration>,
}

#[inline]
fn time_pass<R>(
    pass_times: &mut FxHashMap<&'static str, Duration>,
    name: &'static str,
    f: impl FnOnce() -> R,
) -> R {
    let t = Instant::now();
    let r = f();
    *pass_times.entry(name).or_default() += t.elapsed();
    r
}

/// Run all semantic analysis passes on a parsed module.
/// `features` is the list of enabled build-time feature flags (from `--feature` CLI args).
pub fn analyze(module: &mut Module, features: &[String]) -> AnalysisResult {
    analyze_with_source_dir(module, features, None, false)
}

/// Like [`analyze`], but also provides the source file's directory so that
/// `embed_file("relative/path")` is resolved relative to the source file rather than CWD.
pub fn analyze_with_source_dir(
    module: &mut Module,
    features: &[String],
    source_dir: Option<std::path::PathBuf>,
    warn_const: bool,
) -> AnalysisResult {
    let mut scopes = ScopeTable::new();
    let mut types = TypeTable::new();
    let mut errors = Vec::new();
    let mut pass_times: FxHashMap<&'static str, Duration> = FxHashMap::default();

    // Capture type-alias names BEFORE the meta pass inlines them and removes the
    // `type X = …` declarations — `check_unresolved_imports` needs them to avoid
    // flagging a valid alias import (e.g. `from xtd.ecs import Entity`).
    let alias_names = resolve::collect_type_alias_names(module);

    // Pass 0: Evaluate and substitute meta constants
    time_pass(&mut pass_times, "meta_consts", || {
        errors.extend(meta::evaluate_meta_consts_with_source_dir(module, features, source_dir));
    });

    // Expand @derive(...) attributes into equip blocks
    let derive_records = time_pass(&mut pass_times, "expand_derives", || {
        derive::expand_derives(module, &mut errors)
    });

    // Validate directives
    for item in &module.items {
        if let Item::Directive(d) = &item.node {
            match d.name.as_str() {
                "strip-asserts" | "trace" | "hot-reload" => {
                    if let Some(val) = d.value.as_deref() {
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::UnknownDirective {
                                name: format!("{}={}", d.name, val),
                            },
                            span: d.span,
                        });
                    }
                }
                "scheduler" => {
                    match d.value.as_deref() {
                        Some("pool") | Some("thread") | Some("inline") | Some("single") => {}
                        _ => {
                            errors.push(SemanticError {
                                kind: SemanticErrorKind::UnknownDirective {
                                    name: format!(
                                        "scheduler={}",
                                        d.value.as_deref().unwrap_or("(missing value)")
                                    ),
                                },
                                span: d.span,
                            });
                        }
                    }
                }
                _ => {
                    errors.push(SemanticError {
                        kind: SemanticErrorKind::UnknownDirective {
                            name: d.name.clone(),
                        },
                        span: d.span,
                    });
                }
            }
        }
    }

    // Validate item-level attributes (@derive, @tag, etc.)
    fn validate_attributes(items: &[crate::span::Spanned<Item>], errors: &mut Vec<SemanticError>) {
        for item in items {
            let attrs: &[crate::span::Spanned<crate::parser::ast::Attribute>] = match &item.node {
                Item::Struct(s) => &s.attributes,
                Item::Enum(e) => &e.attributes,
                Item::Function(f) => &f.attributes,
                Item::Test(t) => &t.attributes,
                Item::Module { items: inner, .. } => {
                    validate_attributes(inner, errors);
                    continue;
                }
                _ => continue,
            };
            for attr in attrs {
                match attr.node.name.node.as_str() {
                    "derive" | "tag" | "should_panic" | "skip" | "timeout" => {}
                    _ => {
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::UnknownDirective {
                                name: format!("@{}", attr.node.name.node),
                            },
                            span: attr.span,
                        });
                    }
                }
            }
        }
    }
    let _vt_start = Instant::now();
    validate_attributes(&module.items, &mut errors);
    *pass_times.entry("validate_directives").or_default() += _vt_start.elapsed();

    // Validate test blocks
    {
        let mut seen_setup = false;
        let mut seen_teardown = false;
        for item in &module.items {
            match &item.node {
                Item::SuiteSetup(_) => {
                    if seen_setup {
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::DuplicateSuiteBlock { kind: "setup".to_string() },
                            span: item.span,
                        });
                    }
                    seen_setup = true;
                }
                Item::SuiteTeardown(_) => {
                    if seen_teardown {
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::DuplicateSuiteBlock { kind: "teardown".to_string() },
                            span: item.span,
                        });
                    }
                    seen_teardown = true;
                }
                _ => {}
            }
        }
    }

    // Pass 0.9: D26 (Round XXXIII Batch C1) auto-infer `throws ArithError`
    // for every fn whose body contains a fallible-arith op (`+! -! *! /! %!
    // <<! >>!`), silently — owner ruling 2026-08-06. Runs BEFORE
    // `collect_top_level` so `FunctionInfo.throws_type_id` picks up the
    // mutation, AND before the IR lowering (which reads
    // `func.throws.declares_throws()` directly at ten+ sites and would
    // otherwise stay non-throws). The rewrite mutates `f.throws` to
    // `ThrowsSpec::Explicit(ArithError)` in place, so every downstream reader
    // sees an EXPLICITLY-typed throws-signature identical to what the user
    // could have written by hand. Explicit `throws E` (any E) wins over
    // auto-infer.
    time_pass(&mut pass_times, "rewrite_d26_auto_infer_throws", || {
        rewrite::rewrite_d26_auto_infer_throws(module);
    });

    // Pass 1: Collect top-level definitions
    let mut resolve_ctx = time_pass(&mut pass_times, "collect_top_level", || {
        resolve::collect_top_level(module, &mut scopes, &mut types, &mut errors)
    });

    // Validate `from X import Y` against what the loaded program actually defines.
    time_pass(&mut pass_times, "check_unresolved_imports", || {
        resolve::check_unresolved_imports(module, &alias_names, &mut errors);
    });

    // Pass 1.5: Rewrite import-alias names back to their source names.
    // `from X import Y as Z` is handled mostly in resolution (rebinding the
    // placeholder), but the IR backend lowers identifiers by surface name —
    // so we have to physically rename `Z → Y` in the AST before body resolution.
    // No-op when no aliases were declared.
    time_pass(&mut pass_times, "rewrite_import_aliases", || {
        rewrite::rewrite_import_aliases(module, &resolve_ctx.import_aliases);
    });

    // Pass 2: Resolve names in all bodies
    let mut resolution_map = time_pass(&mut pass_times, "resolve_bodies", || {
        resolve::resolve_bodies(module, &mut scopes, &mut types, &mut errors, &mut resolve_ctx.function_info, &mut resolve_ctx.function_body_scopes, &resolve_ctx.file_module_scopes)
    });
    // Merge any resolutions collected during pass 1
    resolution_map.extend(resolve_ctx.resolution_map);

    // Pass 2.5: Rewrite struct constructor calls to StructLiteral nodes.
    // After resolution we know which identifiers refer to structs, so we can
    // convert Call { callee: Identifier("Foo"), .. } → StructLiteral { name: "Foo", .. }.
    time_pass(&mut pass_times, "rewrite_struct_calls", || {
        let rewrite_errors = rewrite::rewrite_struct_calls(module, &resolution_map, &scopes);
        for (kind, span) in rewrite_errors {
            errors.push(SemanticError { kind, span });
        }
    });

    // Pass 2.6: LHS-type-driven `.collect()` target selection.
    time_pass(&mut pass_times, "apply_collect_target_rewrites", || {
        typecheck::apply_collect_target_rewrites(module);
    });

    // Pass 3: Build trait/impl registry
    let trait_registry = time_pass(&mut pass_times, "build_trait_registry", || {
        traits::build_registry(module, &scopes, &mut types, &resolution_map, &mut errors)
    });

    // Pass 3.5: Validate @derive field types against trait requirements
    time_pass(&mut pass_times, "validate_derive_field_traits", || {
        derive::validate_derive_field_traits(&derive_records, &trait_registry, &mut errors);
    });

    // Populate struct/enum field types on DefInfo BEFORE typecheck.
    time_pass(&mut pass_times, "populate_def_field_types", || {
        populate_def_field_types(module, &mut scopes, &mut types, &mut errors);
    });

    // Pass 3.55: D4 drop-purity taint (D12 enforcement). Seed `is_drop_tainted`
    // from `equip T with Drop` registrations, close under the field-graph
    // fixpoint. Must run after populate_def_field_types (needs field TypeIds)
    // and before the safety pass (which reads the flag).
    time_pass(&mut pass_times, "compute_drop_taint", || {
        compute_drop_taint(&mut scopes, &types, &trait_registry);
    });

    // Pass 3.6: Detect unbounded recursive types BEFORE typecheck.
    time_pass(&mut pass_times, "cycle_check", || {
        cycle_check::check_recursive_type_cycles(module, &scopes, &types, &mut errors);
    });

    // Pass 4: Type check everything
    let (expr_types, method_resolutions, inferred_method_targs, inferred_call_targs, from_conversions) = time_pass(&mut pass_times, "typecheck_module", || {
        typecheck::check_module(
            module,
            &mut scopes,
            &mut types,
            &trait_registry,
            &resolution_map,
            &resolve_ctx.function_info,
            &resolve_ctx.enum_variants,
            &resolve_ctx.struct_fields,
            &resolve_ctx.function_body_scopes,
            &resolve_ctx.struct_generic_bounds,
            &mut errors,
        )
    });

    // Pass 4.5: Sync typecheck-inferred method-generic args into the AST.
    time_pass(&mut pass_times, "apply_inferred_targs", || {
        if !inferred_method_targs.is_empty() {
            typecheck::apply_inferred_method_targs(module, &inferred_method_targs);
        }
        if !inferred_call_targs.is_empty() {
            typecheck::apply_inferred_call_targs(module, &inferred_call_targs);
        }
    });

    // Pass 4.6: `lint:suggest_throws` — flag functions returning Result[T, E]
    // (not throws) that contain `T x = match expr: case Ok(v): v;
    // case Error(e): return Error(e)` patterns. The match-Result rethrow
    // shape is what `throws E` + auto-prop is designed to replace. See
    // `src/semantic/lint_suggest_throws.rs` for detection criteria.
    // Must run after typecheck (consumes `expr_types`).
    let mut lint_warnings: Vec<SemanticWarning> = Vec::new();
    time_pass(&mut pass_times, "lint_suggest_throws", || {
        lint_suggest_throws::check_module(
            module,
            &scopes,
            &mut types,
            &resolution_map,
            &resolve_ctx.enum_variants,
            &expr_types,
            &mut lint_warnings,
        );
    });
    // D28 XOR-fix-it lint (`W_XorLikelyPower`) — flag `2 ^ N` / `10 ^ N` in
    // the GCC-12 shape (narrow per ledger `:959-960`). Non-fatal.
    time_pass(&mut pass_times, "lint_xor_likely_power", || {
        lint_xor_likely_power::check_module(module, &mut lint_warnings);
    });

    // Pass 5: Borrow checking (two sub-passes: 5a computes return_borrows_from, 5b does full check)
    let (shared_bindings, mut warnings, fn_purity, borrow_deps, safety_pt) = time_pass(&mut pass_times, "safety_check_module", || {
        safety::check_module(
            module,
            &scopes,
            &types,
            &resolution_map,
            &mut resolve_ctx.function_info,
            &resolve_ctx.function_body_scopes,
            &expr_types,
            &method_resolutions,
            &mut errors,
            warn_const,
        )
    });
    // Merge fine-grained safety sub-pass timings under prefixed keys so they
    // appear alongside the top-level pass_times in profile JSON.
    for (k, v) in safety_pt {
        let prefixed: &'static str = Box::leak(format!("safety::{}", k).into_boxed_str());
        *pass_times.entry(prefixed).or_default() += v;
    }
    // Append `lint:suggest_throws` warnings after the safety pass so they're
    // surfaced through the same reporting path.
    warnings.extend(lint_warnings);

    AnalysisResult {
        scopes,
        types,
        traits: trait_registry,
        errors,
        resolution_map,
        struct_fields: resolve_ctx.struct_fields,
        enum_variants: resolve_ctx.enum_variants,
        function_info: resolve_ctx.function_info,
        expr_types,
        function_body_scopes: resolve_ctx.function_body_scopes,
        method_resolutions,
        from_conversions,
        shared_bindings,
        warnings,
        fn_purity,
        borrow_deps,
        pass_times,
    }
}

/// Populate `field_types` and `variant_field_types` on DefInfo for structs/enums.
/// Used by:
/// - `is_copy_type` to transitively check if all fields are Copy.
/// - `Expr::FieldAccess` typecheck to return the actual field type
///   (instead of `error_id`, which silently accepts any downstream
///   parameter type and was the root cause of `parse_meta_for_var_name`
///   passing a `Keyword` payload to an `int` parameter without a
///   typecheck error — the antipattern in self-host parser.gg).
///
/// Uses `ast_type_to_resolved` for full Named-type resolution (the
/// previous `types.resolve_type` only handled primitives, dropping
/// every Named-type field — including the critical `Token lex_token`
/// case that exposed the FieldAccess hole). For fields whose type
/// can't be resolved (e.g., references to types declared later or
/// in unloaded modules), the slot is filled with `error_id` to keep
/// vector indices aligned with field order.
fn populate_def_field_types(
    module: &crate::parser::ast::Module,
    scopes: &mut scope::ScopeTable,
    types: &mut types::TypeTable,
    errors: &mut Vec<SemanticError>,
) {
    use crate::parser::ast::{Item, VariantFields};
    fn resolve_or_error(
        ast_ty: &crate::parser::ast::Type,
        span: crate::span::Span,
        scopes: &ScopeTable,
        types: &mut types::TypeTable,
        errors: &mut Vec<SemanticError>,
    ) -> TypeId {
        match types::ast_type_to_resolved(ast_ty, span, scopes, types) {
            Ok(tid) => tid,
            Err(e) => {
                errors.push(e);
                types.error_id
            }
        }
    }
    fn scan_items(
        items: &[crate::span::Spanned<Item>],
        scopes: &mut scope::ScopeTable,
        types: &mut types::TypeTable,
        errors: &mut Vec<SemanticError>,
    ) {
        for item in items {
            match &item.node {
                Item::Struct(s) => {
                    if let Some(def_id) = scopes.lookup(&s.name.node) {
                        let field_tids: Vec<TypeId> = s.fields.iter()
                            .map(|f| resolve_or_error(&f.node.type_.node, f.node.type_.span, scopes, types, errors))
                            .collect();
                        if !field_tids.is_empty() {
                            scopes.get_def_mut(def_id).field_types = Some(field_tids);
                        }
                    }
                }
                Item::Enum(e) => {
                    if let Some(def_id) = scopes.lookup(&e.name.node) {
                        let variant_tids: Vec<Vec<TypeId>> = e.variants.iter()
                            .map(|v| {
                                match &v.node.fields {
                                    VariantFields::Unit => Vec::new(),
                                    VariantFields::Tuple(fields) => {
                                        fields.iter()
                                            .map(|f| resolve_or_error(&f.node, f.span, scopes, types, errors))
                                            .collect()
                                    }
                                }
                            })
                            .collect();
                        if variant_tids.iter().any(|v| !v.is_empty()) {
                            scopes.get_def_mut(def_id).variant_field_types = Some(variant_tids);
                        }
                    }
                }
                Item::Module { items: inner, .. } => {
                    scan_items(inner, scopes, types, errors);
                }
                _ => {}
            }
        }
    }
    scan_items(&module.items, scopes, types, errors);
}

/// D4 drop-purity (D12): whether a type is (transitively) drop-tainted — a
/// named type whose `DefInfo.is_drop_tainted` flag is set, or a container /
/// tuple / array carrying one. Mirrors ggdef's `ty_tainted`
/// (spec/ggdef/src/elaborate/mod.rs:493-501): `Vector[T]`/`Set[T]` taint on
/// the element, `Dict[K,V]` on either side, tuples on any element.
///
/// Deliberate carve-outs (NOT tainted by a tainted type argument):
/// - `Shared[T]`/`Weak[T]`/`Mutex[T]`/`Channel[T]`: refcounted / handle types
///   — the sanctioned multi-owner escape hatch. Copying the HANDLE is a
///   pointer copy (refcount), not a value clone; drop-count determinism is
///   owned by the refcount, not by the copy site. (ggdef has no model for
///   these; divergence is noted in the D12 scout report.)
/// - `Ref[T]` / `T &`: borrows are not implicit copies.
/// `Box[T]`/`Task`/`TaskGroup`/`Guard`/`Owned[T]`/closures are already
/// single-owner via `needs_explicit_move` — the taint check unions with that
/// set rather than replacing it.
pub fn is_drop_tainted_type(type_id: TypeId, types: &TypeTable, scopes: &ScopeTable) -> bool {
    match types.get(type_id) {
        types::ResolvedType::Defined(def_id) => scopes.get_def(*def_id).is_drop_tainted,
        types::ResolvedType::Generic(def_id, args) => {
            let def = scopes.get_def(*def_id);
            if def.is_drop_tainted {
                return true;
            }
            // Handle types: copying the handle never duplicates a drop.
            // (`is_copy_type`-Copy generics — Channel/Shared/Weak/Mutex.)
            if matches!(def.name.as_str(), "Channel" | "Shared" | "Weak" | "Mutex") {
                return false;
            }
            let args = args.clone();
            args.iter().any(|&a| is_drop_tainted_type(a, types, scopes))
        }
        types::ResolvedType::Tuple(elems) => {
            let elems = elems.clone();
            elems.iter().any(|&e| is_drop_tainted_type(e, types, scopes))
        }
        types::ResolvedType::Array(elem, _) | types::ResolvedType::Slice(elem) => {
            is_drop_tainted_type(*elem, types, scopes)
        }
        types::ResolvedType::Owned(inner) => is_drop_tainted_type(*inner, types, scopes),
        // Borrows are not implicit copies; everything else can't carry a
        // custom Drop.
        _ => false,
    }
}

/// D4 drop-purity taint computation (D12). Seeds `DefInfo.is_drop_tainted`
/// on every type with an `equip T with Drop` impl, then runs the transitive
/// fixpoint over struct-field / enum-variant-payload graphs: a type carrying
/// a tainted type anywhere in its field graph is itself tainted. Mirrors
/// ggdef's seed (spec/ggdef/src/elaborate/mod.rs:448-451) + `compute_taint`
/// fixpoint (:458-487).
fn compute_drop_taint(scopes: &mut ScopeTable, types: &TypeTable, registry: &TraitRegistry) {
    // Seed: `equip T with Drop` impls. The Drop trait is a builtin registered
    // by name (traits.rs builtin table); resolve the name ONCE here at the
    // seed — every downstream read is the typed DefInfo flag.
    let mut seeds: Vec<DefId> = Vec::new();
    for impl_info in &registry.impls {
        if impl_info.trait_name.as_deref() == Some("Drop") {
            match types.get(impl_info.self_type) {
                types::ResolvedType::Defined(d) | types::ResolvedType::Generic(d, _) => {
                    seeds.push(*d)
                }
                _ => {}
            }
        }
    }
    if seeds.is_empty() {
        return;
    }
    for d in seeds {
        scopes.get_def_mut(d).is_drop_tainted = true;
    }
    // Transitive fixpoint over the field graph.
    loop {
        let mut changed = false;
        for i in 0..scopes.def_count() {
            let def_id = DefId(i as u32);
            let def = scopes.get_def(def_id);
            if def.is_drop_tainted {
                continue;
            }
            let tainted = if let Some(fts) = &def.field_types {
                fts.iter().any(|&t| is_drop_tainted_type(t, types, scopes))
            } else if let Some(vts) = &def.variant_field_types {
                vts.iter()
                    .flatten()
                    .any(|&t| is_drop_tainted_type(t, types, scopes))
            } else {
                false
            };
            if tainted {
                scopes.get_def_mut(def_id).is_drop_tainted = true;
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
}
