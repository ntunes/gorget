pub mod borrow;
pub mod derive;
pub mod errors;
pub mod ids;
pub mod intern;
pub mod meta;
pub mod resolve;
pub mod rewrite;
pub mod scope;
pub mod traits;
pub mod typecheck;
pub mod types;

use rustc_hash::FxHashMap;

use crate::parser::ast::{Item, Module};
use crate::span::Span;
use errors::{SemanticError, SemanticErrorKind};
use ids::{DefId, TypeId};
use resolve::{EnumVariantInfo, FunctionInfo, ResolutionMap, StructFieldInfo};
use scope::ScopeTable;
use traits::TraitRegistry;
use types::TypeTable;

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
}

/// Run all semantic analysis passes on a parsed module.
/// `features` is the list of enabled build-time feature flags (from `--feature` CLI args).
pub fn analyze(module: &mut Module, features: &[String]) -> AnalysisResult {
    analyze_with_source_dir(module, features, None)
}

/// Like [`analyze`], but also provides the source file's directory so that
/// `embed_file("relative/path")` is resolved relative to the source file rather than CWD.
pub fn analyze_with_source_dir(
    module: &mut Module,
    features: &[String],
    source_dir: Option<std::path::PathBuf>,
) -> AnalysisResult {
    let mut scopes = ScopeTable::new();
    let mut types = TypeTable::new();
    let mut errors = Vec::new();

    // Pass 0: Evaluate and substitute meta constants
    errors.extend(meta::evaluate_meta_consts_with_source_dir(module, features, source_dir));

    // Expand @derive(...) attributes into equip blocks
    derive::expand_derives(module, &mut errors);

    // Validate directives
    for item in &module.items {
        if let Item::Directive(d) = &item.node {
            match d.name.as_str() {
                "strip-asserts" => {
                    if d.value.is_some() {
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::UnknownDirective {
                                name: format!("strip-asserts={}", d.value.as_deref().unwrap()),
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
                "immutable-by-default" => {
                    if d.value.is_some() {
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::UnknownDirective {
                                name: format!("immutable-by-default={}", d.value.as_deref().unwrap()),
                            },
                            span: d.span,
                        });
                    }
                }
                "name-first" => {
                    if d.value.is_some() {
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::UnknownDirective {
                                name: format!("name-first={}", d.value.as_deref().unwrap()),
                            },
                            span: d.span,
                        });
                    }
                }
                "trace" => {
                    if d.value.is_some() {
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::UnknownDirective {
                                name: format!("trace={}", d.value.as_deref().unwrap()),
                            },
                            span: d.span,
                        });
                    }
                }
                "hot-reload" => {
                    if d.value.is_some() {
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::UnknownDirective {
                                name: format!("hot-reload={}", d.value.as_deref().unwrap()),
                            },
                            span: d.span,
                        });
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
    let mut resolution_map = resolve::resolve_bodies(module, &mut scopes, &mut types, &mut errors, &mut resolve_ctx.function_info, &mut resolve_ctx.function_body_scopes);
    // Merge any resolutions collected during pass 1
    resolution_map.extend(resolve_ctx.resolution_map);

    // Pass 2.5: Rewrite struct constructor calls to StructLiteral nodes.
    // After resolution we know which identifiers refer to structs, so we can
    // convert Call { callee: Identifier("Foo"), .. } → StructLiteral { name: "Foo", .. }.
    rewrite::rewrite_struct_calls(module, &resolution_map, &scopes);

    // Pass 3: Build trait/impl registry
    let trait_registry =
        traits::build_registry(module, &scopes, &mut types, &resolution_map, &mut errors);

    // Pass 4: Type check everything
    let (expr_types, method_resolutions) = typecheck::check_module(
        module,
        &mut scopes,
        &mut types,
        &trait_registry,
        &resolution_map,
        &resolve_ctx.function_info,
        &resolve_ctx.enum_variants,
        &resolve_ctx.function_body_scopes,
        &mut errors,
    );

    // Pass 5: Borrow checking (two sub-passes: 5a computes return_borrows_from, 5b does full check)
    let immutable_by_default = module.items.iter().any(|item| {
        matches!(&item.node, Item::Directive(d) if d.name == "immutable-by-default")
    });
    borrow::check_module(
        module,
        &scopes,
        &types,
        &resolution_map,
        &mut resolve_ctx.function_info,
        &resolve_ctx.function_body_scopes,
        immutable_by_default,
        &expr_types,
        &method_resolutions,
        &mut errors,
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
    }
}
