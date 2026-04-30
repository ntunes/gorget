pub mod safety;
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

    // Pass 0: Evaluate and substitute meta constants
    errors.extend(meta::evaluate_meta_consts_with_source_dir(module, features, source_dir));

    // Expand @derive(...) attributes into equip blocks
    let derive_records = derive::expand_derives(module, &mut errors);

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
    validate_attributes(&module.items, &mut errors);

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
    let mut resolve_ctx = resolve::collect_top_level(module, &mut scopes, &mut types, &mut errors);

    // Pass 2: Resolve names in all bodies
    let mut resolution_map = resolve::resolve_bodies(module, &mut scopes, &mut types, &mut errors, &mut resolve_ctx.function_info, &mut resolve_ctx.function_body_scopes, &resolve_ctx.file_module_scopes);
    // Merge any resolutions collected during pass 1
    resolution_map.extend(resolve_ctx.resolution_map);

    // Pass 2.5: Rewrite struct constructor calls to StructLiteral nodes.
    // After resolution we know which identifiers refer to structs, so we can
    // convert Call { callee: Identifier("Foo"), .. } → StructLiteral { name: "Foo", .. }.
    let rewrite_errors = rewrite::rewrite_struct_calls(module, &resolution_map, &scopes);
    for (kind, span) in rewrite_errors {
        errors.push(SemanticError { kind, span });
    }

    // Pass 2.6: LHS-type-driven `.collect()` target selection. Walks
    // VarDecls whose declared type is `Set[T]` and rewrites an inner
    // `.collect()` call to `.to_set()` so typecheck + IR lowering
    // dispatch the Set-targeted `Iterator[T]::to_set(&self)` trait
    // default instead of the Vector-targeted `.collect()`. Lets callers
    // write `Set[int] s = v.iter().filter(p).collect()` without a
    // turbofish or explicit `.to_set()` spelling. Purely AST-level —
    // no type inference needed since the declared type is at the
    // syntactic position.
    typecheck::apply_collect_target_rewrites(module);

    // Pass 3: Build trait/impl registry
    let trait_registry =
        traits::build_registry(module, &scopes, &mut types, &resolution_map, &mut errors);

    // Pass 3.5: Validate @derive field types against trait requirements
    derive::validate_derive_field_traits(&derive_records, &trait_registry, &mut errors);

    // Populate struct/enum field types on DefInfo BEFORE typecheck.
    // typecheck's Expr::FieldAccess inference reads field_types to
    // return the actual field type — without this, field access types
    // as <error> and downstream Keyword→int (and similar enum→int)
    // calls slip through silently. See populate_def_field_types
    // header for details.
    populate_def_field_types(module, &mut scopes, &mut types);

    // Pass 4: Type check everything
    let (expr_types, method_resolutions, inferred_method_targs, inferred_call_targs) = typecheck::check_module(
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
    );

    // Pass 4.5: Sync typecheck-inferred method-generic args into the AST.
    // Typecheck records `v.my_map(double)` → `[int, int(int)]` in a side-
    // table (method-level inference — shape 1/2/3); this walk mutates the
    // matching MethodCall nodes' `generic_args` from None to Some(inferred)
    // so the downstream generic-collector + IR lowering see them just like
    // explicit `[T1, T2]` args. See docs/internals/method-level-inference.md.
    if !inferred_method_targs.is_empty() {
        typecheck::apply_inferred_method_targs(module, &inferred_method_targs);
    }
    // Pass 4.5b: Same sync but for *generic free-function* calls — patches
    // `Expr::Call.generic_args` from typecheck's per-call-site
    // fresh-instantiation. Without this, IR-lowering's monomorphisation has
    // no concrete targs to mangle a symbol from and link-fails with
    // `undefined reference to <fn>`.
    if !inferred_call_targs.is_empty() {
        typecheck::apply_inferred_call_targs(module, &inferred_call_targs);
    }

    // Pass 5: Borrow checking (two sub-passes: 5a computes return_borrows_from, 5b does full check)
    let (shared_bindings, warnings, fn_purity, borrow_deps) = safety::check_module(
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
    );

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
