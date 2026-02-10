pub mod borrow;
pub mod errors;
pub mod ids;
pub mod intern;
pub mod resolve;
pub mod scope;
pub mod traits;
pub mod typecheck;
pub mod types;

use rustc_hash::FxHashMap;

use crate::parser::ast::Module;
use errors::SemanticError;
use ids::DefId;
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
}

/// Run all semantic analysis passes on a parsed module.
pub fn analyze(module: &Module) -> AnalysisResult {
    let mut scopes = ScopeTable::new();
    let mut types = TypeTable::new();
    let mut errors = Vec::new();

    // Pass 1: Collect top-level definitions
    let resolve_ctx = resolve::collect_top_level(module, &mut scopes, &mut types, &mut errors);

    // Pass 2: Resolve names in all bodies
    let mut resolution_map = resolve::resolve_bodies(module, &mut scopes, &mut types, &mut errors);
    // Merge any resolutions collected during pass 1
    resolution_map.extend(resolve_ctx.resolution_map);

    // Pass 3: Build trait/impl registry
    let trait_registry =
        traits::build_registry(module, &scopes, &mut types, &resolution_map, &mut errors);

    // Pass 4: Type check everything
    typecheck::check_module(
        module,
        &mut scopes,
        &mut types,
        &trait_registry,
        &resolution_map,
        &resolve_ctx.function_info,
        &mut errors,
    );

    // Pass 5: Borrow checking
    borrow::check_module(
        module,
        &scopes,
        &types,
        &resolution_map,
        &resolve_ctx.function_info,
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
    }
}
