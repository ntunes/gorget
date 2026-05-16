pub mod safety;
pub mod cycle_check;
pub mod derive;
pub mod errors;
pub mod ids;
pub mod meta;
pub mod purity;
pub mod resolve;
pub mod rewrite;
pub mod scope;
pub mod traits;
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
use traits::TraitRegistry;
use types::TypeTable;

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
    /// Maps method-call span start → DefId of the resolved method.
    /// Threaded from typechecker through borrow checker to codegen for
    /// ownership-aware move-zeroing at call sites.
    pub method_resolutions: FxHashMap<usize, DefId>,
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
                "overflow" => {
                    if d.value.as_deref() != Some("wrap") {
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::UnknownDirective {
                                name: format!(
                                    "overflow={}",
                                    d.value.as_deref().unwrap_or("(missing value)")
                                ),
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

    // Pass 1: Collect top-level definitions
    let mut resolve_ctx = time_pass(&mut pass_times, "collect_top_level", || {
        resolve::collect_top_level(module, &mut scopes, &mut types, &mut errors)
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
        populate_def_field_types(module, &mut scopes, &mut types);
    });

    // Pass 3.6: Detect unbounded recursive types BEFORE typecheck.
    time_pass(&mut pass_times, "cycle_check", || {
        cycle_check::check_recursive_type_cycles(module, &scopes, &types, &mut errors);
    });

    // Pass 4: Type check everything
    let (expr_types, method_resolutions, inferred_method_targs, inferred_call_targs) = time_pass(&mut pass_times, "typecheck_module", || {
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

    // Pass 5: Borrow checking (two sub-passes: 5a computes return_borrows_from, 5b does full check)
    let (shared_bindings, warnings, fn_purity, borrow_deps, safety_pt) = time_pass(&mut pass_times, "safety_check_module", || {
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
) {
    use crate::parser::ast::{Item, VariantFields};
    fn scan_items(
        items: &[crate::span::Spanned<Item>],
        scopes: &mut scope::ScopeTable,
        types: &mut types::TypeTable,
    ) {
        for item in items {
            match &item.node {
                Item::Struct(s) => {
                    if let Some(def_id) = scopes.lookup(&s.name.node) {
                        let field_tids: Vec<TypeId> = s.fields.iter()
                            .map(|f| {
                                types::ast_type_to_resolved(
                                    &f.node.type_.node, f.node.type_.span, scopes, types,
                                ).unwrap_or_else(|_| types.error_id)
                            })
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
                                            .map(|f| {
                                                types::ast_type_to_resolved(
                                                    &f.node, f.span, scopes, types,
                                                ).unwrap_or_else(|_| types.error_id)
                                            })
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
                    scan_items(inner, scopes, types);
                }
                _ => {}
            }
        }
    }
    scan_items(&module.items, scopes, types);
}
