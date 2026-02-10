use rustc_hash::FxHashMap;

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use super::errors::{SemanticError, SemanticErrorKind};
use super::ids::{DefId, TypeId};
use super::scope::{DefKind, ScopeTable};
use super::types::{self, TypeTable};

pub use crate::parser::ast::Ownership;

/// Side table for struct field info.
#[derive(Debug, Clone)]
pub struct StructFieldInfo {
    pub fields: Vec<(String, Span)>,
}

/// Side table for enum variant info.
#[derive(Debug, Clone)]
pub struct EnumVariantInfo {
    pub variants: Vec<(String, DefId)>,
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
    pub scope_id: super::ids::ScopeId,
    /// Names of generic type parameters, in declaration order.
    pub generic_param_names: Vec<String>,
    /// Where-clause bounds: `(param_name, [trait_name, ...])`.
    pub where_bounds: Vec<(String, Vec<String>)>,
}

/// Shared context passed around during resolution.
pub struct ResolveContext {
    pub struct_fields: FxHashMap<DefId, StructFieldInfo>,
    pub enum_variants: FxHashMap<DefId, EnumVariantInfo>,
    pub function_info: FxHashMap<DefId, FunctionInfo>,
    pub resolution_map: ResolutionMap,
}

impl ResolveContext {
    fn new() -> Self {
        Self {
            struct_fields: FxHashMap::default(),
            enum_variants: FxHashMap::default(),
            function_info: FxHashMap::default(),
            resolution_map: FxHashMap::default(),
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
    // Register built-in collection types so they can be resolved as types.
    for name in &["Vector", "List", "Array", "Dict", "HashMap", "Map", "Set", "HashSet", "Box", "File"] {
        let _ = scopes.define(name.to_string(), DefKind::Struct, Span::dummy());
    }
    // Register built-in core traits.
    for trait_name in &["Displayable", "Equatable", "Cloneable", "Hashable", "Drop", "Iterator"] {
        let _ = scopes.define(trait_name.to_string(), DefKind::Trait, Span::dummy());
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
            ctx.enum_variants.insert(enum_def_id, EnumVariantInfo { variants: variant_infos });
        }
    }
    collect_top_level_inner(module, scopes, types, errors, &mut ctx);
    ctx
}

fn collect_top_level_inner(
    module: &Module,
    scopes: &mut ScopeTable,
    types: &mut TypeTable,
    errors: &mut Vec<SemanticError>,
    ctx: &mut ResolveContext,
) {
    for item in &module.items {
        collect_item(&item.node, item.span, scopes, types, errors, ctx);
    }
}

fn collect_item(
    item: &Item,
    _span: Span,
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

                    let param_ownerships: Vec<Ownership> =
                        f.params.iter().map(|p| p.node.ownership).collect();
                    let param_names: Vec<String> =
                        f.params.iter().map(|p| p.node.name.node.clone()).collect();
                    let param_defaults: Vec<Option<Spanned<Expr>>> =
                        f.params.iter().map(|p| p.node.default.clone()).collect();

                    let generic_param_names = extract_generic_param_names(&f.generic_params);
                    let where_bounds = extract_where_bounds(&f.where_clause);

                    ctx.function_info.insert(
                        def_id,
                        FunctionInfo {
                            def_id,
                            return_type_id: ret_type,
                            param_type_ids: Vec::new(),
                            param_ownerships,
                            param_names,
                            param_defaults,
                            throws: f.throws.is_some(),
                            scope_id: scopes.current_scope(),
                            generic_param_names,
                            where_bounds,
                        },
                    );
                }
                Err(e) => errors.push(e),
            }
        }

        Item::Struct(s) => {
            match scopes.define(s.name.node.clone(), DefKind::Struct, s.name.span) {
                Ok(def_id) => {
                    let fields: Vec<(String, Span)> = s
                        .fields
                        .iter()
                        .map(|f| (f.node.name.node.clone(), f.span))
                        .collect();
                    ctx.struct_fields
                        .insert(def_id, StructFieldInfo { fields });
                }
                Err(e) => errors.push(e),
            }
        }

        Item::Enum(e) => {
            match scopes.define(e.name.node.clone(), DefKind::Enum, e.name.span) {
                Ok(enum_def_id) => {
                    let mut variant_infos = Vec::new();
                    for variant in &e.variants {
                        // Define each variant at module scope so `Some(x)` works
                        match scopes.define(
                            variant.node.name.node.clone(),
                            DefKind::Variant,
                            variant.node.name.span,
                        ) {
                            Ok(variant_def_id) => {
                                variant_infos.push((
                                    variant.node.name.node.clone(),
                                    variant_def_id,
                                ));
                            }
                            Err(e) => errors.push(e),
                        }
                    }
                    ctx.enum_variants.insert(
                        enum_def_id,
                        EnumVariantInfo {
                            variants: variant_infos,
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

        Item::Equip(_) => {
            // Processed in Step 7 (trait registry)
        }

        Item::ExternBlock(ext) => {
            for func in &ext.items {
                if let Err(e) = scopes.define(
                    func.node.name.node.clone(),
                    DefKind::Function,
                    func.node.name.span,
                ) {
                    errors.push(e);
                }
            }
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
        ImportStmt::Grouped { names, .. } | ImportStmt::From { names, .. } => {
            for name in names {
                if let Err(e) = scopes.define(name.node.clone(), DefKind::Import, name.span) {
                    errors.push(e);
                }
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
) -> ResolutionMap {
    let mut resolution_map = ResolutionMap::default();

    for item in &module.items {
        resolve_item_body(&item.node, scopes, types, errors, &mut resolution_map);
    }

    resolution_map
}

fn resolve_item_body(
    item: &Item,
    scopes: &mut ScopeTable,
    types: &mut TypeTable,
    errors: &mut Vec<SemanticError>,
    resolution_map: &mut ResolutionMap,
) {
    match item {
        Item::Function(f) => {
            resolve_function(f, scopes, types, errors, resolution_map);
        }
        Item::Equip(impl_block) => {
            resolve_equip_block(impl_block, scopes, types, errors, resolution_map);
        }
        Item::ConstDecl(c) => {
            resolve_expr(&c.value, scopes, errors, resolution_map);
        }
        Item::StaticDecl(s) => {
            resolve_expr(&s.value, scopes, errors, resolution_map);
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
) {
    scopes.push_scope(super::scope::ScopeKind::Function);

    // Define generic type params
    if let Some(generics) = &f.generic_params {
        for param in &generics.node.params {
            match &param.node {
                GenericParam::Type(name) => {
                    if let Err(e) =
                        scopes.define(name.node.clone(), DefKind::GenericParam, name.span)
                    {
                        errors.push(e);
                    }
                }
                GenericParam::Lifetime(_) => { /* lifetimes deferred */ }
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

    // Define parameters
    for param in &f.params {
        if let Err(e) = scopes.define(
            param.node.name.node.clone(),
            DefKind::Variable,
            param.node.name.span,
        ) {
            errors.push(e);
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
        FunctionBody::Declaration => {}
    }

    scopes.pop_scope();
}

fn resolve_equip_block(
    impl_block: &EquipBlock,
    scopes: &mut ScopeTable,
    types: &mut TypeTable,
    errors: &mut Vec<SemanticError>,
    resolution_map: &mut ResolutionMap,
) {
    scopes.push_scope(super::scope::ScopeKind::EquipBlock { self_type: None });

    // Define generic params for the impl block
    if let Some(generics) = &impl_block.generic_params {
        for param in &generics.node.params {
            if let GenericParam::Type(name) = &param.node {
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
        resolve_function(&method.node, scopes, types, errors, resolution_map);
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
            pattern, value, ..
        } => {
            // Resolve value first (before defining the variable, so `int x = x` refers to outer x)
            resolve_expr(value, scopes, errors, resolution_map);
            // Define bindings from pattern
            define_pattern_bindings(&pattern.node, pattern.span, scopes, errors);
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

        Stmt::Break(expr) => {
            if let Some(expr) = expr {
                resolve_expr(expr, scopes, errors, resolution_map);
            }
        }

        Stmt::Continue | Stmt::Pass => {}

        Stmt::For {
            pattern,
            iterable,
            body,
            else_body,
            ..
        } => {
            resolve_expr(iterable, scopes, errors, resolution_map);
            scopes.push_scope(super::scope::ScopeKind::ForLoop);
            define_pattern_bindings(&pattern.node, pattern.span, scopes, errors);
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
            resolve_expr(condition, scopes, errors, resolution_map);
            scopes.push_scope(super::scope::ScopeKind::Block);
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
            resolve_expr(condition, scopes, errors, resolution_map);
            scopes.push_scope(super::scope::ScopeKind::Block);
            resolve_block(then_body, scopes, types, errors, resolution_map);
            scopes.pop_scope();

            for (cond, body) in elif_branches {
                resolve_expr(cond, scopes, errors, resolution_map);
                scopes.push_scope(super::scope::ScopeKind::Block);
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
            for arm in arms {
                scopes.push_scope(super::scope::ScopeKind::Block);
                define_pattern_bindings(&arm.pattern.node, arm.pattern.span, scopes, errors);
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

        Stmt::With { bindings, body } => {
            scopes.push_scope(super::scope::ScopeKind::Block);
            for binding in bindings {
                resolve_expr(&binding.expr, scopes, errors, resolution_map);
                if let Err(e) = scopes.define(
                    binding.name.node.clone(),
                    DefKind::Variable,
                    binding.name.span,
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

        Stmt::Item(item) => {
            // Nested item definitions
            let mut ctx = ResolveContext::new();
            collect_item(item, Span::dummy(), scopes, types, errors, &mut ctx);
            resolve_item_body(item, scopes, types, errors, resolution_map);
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
        | Expr::CharLiteral(_)
        | Expr::NoneLiteral
        | Expr::SelfExpr
        | Expr::It => {}

        Expr::StringLiteral(s) => {
            // Resolve interpolated expressions inside string
            for segment in &s.segments {
                if let crate::lexer::token::StringSegment::Interpolation(interp_expr) = segment {
                    // The interpolation contains a string that was parsed — we'd need to
                    // re-parse it. For now, we skip resolution inside string interpolations.
                    let _ = interp_expr;
                }
            }
        }

        Expr::Identifier(name) => {
            match scopes.lookup(name) {
                Some(def_id) => {
                    resolution_map.insert(expr.span.start, def_id);
                }
                None => {
                    // Don't error on built-in functions like `print`
                    if !is_builtin(name) {
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::UndefinedName {
                                name: name.clone(),
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

        Expr::NilCoalescing { lhs, rhs } => {
            resolve_expr(lhs, scopes, errors, resolution_map);
            resolve_expr(rhs, scopes, errors, resolution_map);
        }

        Expr::Try { expr: inner }
        | Expr::Move { expr: inner }
        | Expr::MutableBorrow { expr: inner }
        | Expr::Deref { expr: inner }
        | Expr::Await { expr: inner }
        | Expr::Spawn { expr: inner }
        | Expr::TryCapture { expr: inner } => {
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
                define_pattern_bindings(&arm.pattern.node, arm.pattern.span, scopes, errors);
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
            let _ = scopes.define("it".into(), DefKind::Variable, expr.span);
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
            define_pattern_bindings(&variable.node, variable.span, scopes, errors);
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

        Expr::StructLiteral { name, args } => {
            // Resolve struct name
            match scopes.lookup(&name.node) {
                Some(def_id) => {
                    resolution_map.insert(name.span.start, def_id);
                }
                None => {
                    errors.push(SemanticError {
                        kind: SemanticErrorKind::UndefinedName {
                            name: name.node.clone(),
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
    }
}

/// Define bindings introduced by a pattern.
fn define_pattern_bindings(
    pattern: &Pattern,
    span: Span,
    scopes: &mut ScopeTable,
    errors: &mut Vec<SemanticError>,
) {
    match pattern {
        Pattern::Binding(name) => {
            if let Err(e) = scopes.define(name.clone(), DefKind::Variable, span) {
                errors.push(e);
            }
        }
        Pattern::Constructor { fields, .. } => {
            for field in fields {
                define_pattern_bindings(&field.node, field.span, scopes, errors);
            }
        }
        Pattern::Tuple(elements) => {
            for elem in elements {
                define_pattern_bindings(&elem.node, elem.span, scopes, errors);
            }
        }
        Pattern::Or(alternatives) => {
            // In an or pattern, each alternative must bind the same names.
            // For simplicity, just bind from the first alternative.
            if let Some(first) = alternatives.first() {
                define_pattern_bindings(&first.node, first.span, scopes, errors);
            }
        }
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => {}
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
                GenericParam::Type(name) => Some(name.node.clone()),
                _ => None,
            })
            .collect(),
        None => Vec::new(),
    }
}

/// Extract where-clause bounds as `(param_name, [trait_name, ...])`.
fn extract_where_bounds(
    where_clause: &Option<Spanned<WhereClause>>,
) -> Vec<(String, Vec<String>)> {
    match where_clause {
        Some(wc) => wc
            .node
            .bounds
            .iter()
            .map(|wb| {
                let param = wb.node.type_name.node.clone();
                let traits: Vec<String> = wb
                    .node
                    .bounds
                    .iter()
                    .map(|tb| tb.node.name.node.clone())
                    .collect();
                (param, traits)
            })
            .collect(),
        None => Vec::new(),
    }
}

/// Check if a name is a built-in function or type.
fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "print" | "println" | "len" | "range" | "enumerate" | "zip" | "map" | "filter" | "type"
        | "Vector" | "Dict" | "Set" | "HashMap" | "HashSet" | "List" | "Array" | "Map"
        | "Option" | "Result" | "Some" | "None" | "Ok" | "Error"
        | "Displayable" | "Equatable" | "Cloneable" | "Hashable" | "Drop" | "Iterator"
        | "Box"
        | "File" | "read_file" | "write_file" | "append_file" | "file_exists" | "delete_file"
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
        collect_top_level(&module, &mut scopes, &mut types, &mut errors);
        let resolution_map = resolve_bodies(&module, &mut scopes, &mut types, &mut errors);
        (scopes, types, resolution_map, errors)
    }

    #[test]
    fn collect_function() {
        let (scopes, _, errors) = parse_and_collect("int add(int a, int b) = a + b\n");
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
        assert!(scopes.lookup("Color").is_some());
        assert!(scopes.lookup("Red").is_some());
        assert!(scopes.lookup("Green").is_some());
        assert!(scopes.lookup("Blue").is_some());
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
        let (_, _, errors) = parse_and_collect("int foo() = 1\nint foo() = 2\n");
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
        let source = "void main():\n    auto x = helper()\nint helper() = 42\n";
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
            SemanticErrorKind::UndefinedName { name } if name == "undefined_var"
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
        let (scopes, _, errors) =
            parse_and_collect("from std.fmt import Formatter, format\n");
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert!(scopes.lookup("Formatter").is_some());
        assert!(scopes.lookup("format").is_some());
    }
}
