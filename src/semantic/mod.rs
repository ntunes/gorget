pub mod safety;
pub mod derive;
pub mod errors;
pub mod ids;
pub mod meta;
pub mod provenance;
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

    // Pass 3: Build trait/impl registry
    let trait_registry =
        traits::build_registry(module, &scopes, &mut types, &resolution_map, &mut errors);

    // Pass 3.5: Validate @derive field types against trait requirements
    derive::validate_derive_field_traits(&derive_records, &trait_registry, &mut errors);

    // Pass 4: Type check everything
    let (expr_types, method_resolutions) = typecheck::check_module(
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

    // Pass 4.5: String provenance inference — downgrades view String bindings to Str
    provenance::infer_string_provenance(
        module, &mut scopes, &types, &resolution_map, &expr_types,
        &mut resolve_ctx.function_info, &method_resolutions,
    );

    // Pass 4.6: Rewrite AST type annotations to match provenance-adjusted type_ids.
    // After str→StringType parser unification, all string annotations are StringType.
    // Provenance downgrades some to Str (view). This pass rewrites the AST to match,
    // so the IR lowering sees the correct type for drop elaboration.
    provenance::rewrite_ast_string_types(module, &scopes, &types, &resolve_ctx.function_info);

    // Pass 5: Borrow checking (two sub-passes: 5a computes return_borrows_from, 5b does full check)
    let (shared_bindings, warnings, fn_purity) = safety::check_module(
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
    }
}
