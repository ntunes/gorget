//! `lint:suggest_throws` — detect the verbose `match Result` rethrow pattern
//! and suggest converting the enclosing function to `throws E`.
//!
//! The pattern we flag:
//!
//! ```text
//! T x = match expr:
//!     case Ok(v): v
//!     case Error(e): return Error(e)   // or `return e`, or `throw e`
//! ```
//!
//! When the enclosing function returns `Result[T, E]` (and is NOT already
//! declared `throws`), the same code can be written as
//!
//! ```text
//! <fn_name> throws <E>:
//!     T x = expr
//! ```
//!
//! with auto-propagation handling the error path.
//!
//! Design notes (see CLAUDE.md "No name matching"):
//!
//! * `Ok` / `Error` variants are identified by *DefId*, not by string
//!   compare. We look up `Result` once via the scope table, read the
//!   variant DefIds from the typed `EnumVariantInfo`, then check the
//!   `Pattern::Constructor` path's resolved DefId against them.
//! * The scrutinee type is consulted via the typed `expr_types` map
//!   populated by the typechecker, not by inspecting AST shapes.
//! * Detection is precision-over-recall: we accept a small handful of
//!   well-known rethrow shapes and skip anything ambiguous. False
//!   negatives are fine; false positives are user-hostile.
//! * One suggestion per function, anchored at the function name span.

use rustc_hash::FxHashMap;

use crate::parser::ast::{
    Expr, FunctionBody, FunctionDef, Item, MatchArm, Module, Pattern, Stmt,
};
use crate::parser::visitor::ExprVisitor;
use crate::semantic::errors::{SemanticWarning, SemanticWarningKind};
use crate::semantic::ids::{DefId, TypeId};
use crate::semantic::resolve::{EnumVariantInfo, ResolutionMap};
use crate::semantic::scope::ScopeTable;
use crate::semantic::types::{ResolvedType, TypeTable};
use crate::span::{Span, Spanned};

/// Run the lint over every function in `module` and append a
/// `SuggestThrowsRefactor` warning per function that matches.
pub fn check_module(
    module: &Module,
    scopes: &ScopeTable,
    types: &mut TypeTable,
    resolution_map: &ResolutionMap,
    enum_variants: &FxHashMap<DefId, EnumVariantInfo>,
    expr_types: &FxHashMap<Span, TypeId>,
    warnings: &mut Vec<SemanticWarning>,
) {
    let ctx_ids = match ContextIds::new(scopes, enum_variants) {
        Some(c) => c,
        // No `Result` enum (prelude not loaded? synthetic module?) → bail.
        None => return,
    };
    visit_items(&ctx_ids, module, scopes, types, resolution_map, expr_types, warnings);
}

/// Result/Ok/Error DefIds resolved once at the entry point.
struct ContextIds {
    result_def_id: DefId,
    ok_def_id: DefId,
    error_def_id: DefId,
}

impl ContextIds {
    fn new(
        scopes: &ScopeTable,
        enum_variants: &FxHashMap<DefId, EnumVariantInfo>,
    ) -> Option<Self> {
        let result_def_id = scopes.lookup("Result")?;
        let info = enum_variants.get(&result_def_id)?;
        let ok_def_id = info.variants.iter().find(|(n, _)| n == "Ok").map(|(_, d)| *d)?;
        let error_def_id = info
            .variants
            .iter()
            .find(|(n, _)| n == "Error")
            .map(|(_, d)| *d)?;
        Some(ContextIds { result_def_id, ok_def_id, error_def_id })
    }
}

/// Read-only context — all the typed information needed by the matchers.
/// (`TypeTable` is borrowed immutably here; the entry point keeps the
/// mutable borrow alive but converts it to immutable for each visit.)
struct Context<'a> {
    scopes: &'a ScopeTable,
    types: &'a TypeTable,
    resolution_map: &'a ResolutionMap,
    expr_types: &'a FxHashMap<Span, TypeId>,
    ids: &'a ContextIds,
}

impl<'a> Context<'a> {
    /// True if `type_id` resolves to `Result[T, E]` for some T and E.
    fn is_result_type(&self, type_id: TypeId) -> bool {
        match self.types.get(type_id) {
            ResolvedType::Generic(def_id, args) => {
                *def_id == self.ids.result_def_id && args.len() == 2
            }
            _ => false,
        }
    }

    /// Extract the `E` (error) TypeId from a `Result[T, E]` type, if applicable.
    fn result_error_type(&self, type_id: TypeId) -> Option<TypeId> {
        match self.types.get(type_id) {
            ResolvedType::Generic(def_id, args)
                if *def_id == self.ids.result_def_id && args.len() == 2 =>
            {
                Some(args[1])
            }
            _ => None,
        }
    }

    /// Resolve a Constructor pattern's variant identifier to a DefId.
    /// The variant identifier is the LAST segment of `path` (e.g. for
    /// `Result.Ok` the path is `["Result", "Ok"]` and we want `Ok`).
    /// We go through the resolution map first (typed lookup at the path-
    /// segment's span); if that doesn't have an entry — variants in
    /// patterns are not always recorded by the resolver — fall back to
    /// the plain scope lookup, which is still typed (DefId-based).
    fn resolve_variant_def(&self, path: &[Spanned<String>]) -> Option<DefId> {
        let last = path.last()?;
        if let Some(def_id) = self.resolution_map.get(&last.span.start) {
            return Some(*def_id);
        }
        self.scopes.lookup(&last.node)
    }

    /// Format `Result[T, E]`'s `E` slot for the diagnostic message.
    fn describe_error_type(&self, type_id: TypeId) -> String {
        match self.types.get(type_id) {
            ResolvedType::Defined(def_id) | ResolvedType::Generic(def_id, _) => {
                let name = self.scopes.get_def(*def_id).name.clone();
                if let ResolvedType::Generic(_, args) = self.types.get(type_id) {
                    if !args.is_empty() {
                        let arg_strs: Vec<_> =
                            args.iter().map(|a| self.describe_error_type(*a)).collect();
                        return format!("{}[{}]", name, arg_strs.join(", "));
                    }
                }
                name
            }
            _ => self.types.display(type_id),
        }
    }
}

fn visit_items(
    ids: &ContextIds,
    module: &Module,
    scopes: &ScopeTable,
    types: &mut TypeTable,
    resolution_map: &ResolutionMap,
    expr_types: &FxHashMap<Span, TypeId>,
    warnings: &mut Vec<SemanticWarning>,
) {
    fn walk(
        ids: &ContextIds,
        items: &[Spanned<Item>],
        scopes: &ScopeTable,
        types: &mut TypeTable,
        resolution_map: &ResolutionMap,
        expr_types: &FxHashMap<Span, TypeId>,
        warnings: &mut Vec<SemanticWarning>,
    ) {
        for item in items {
            match &item.node {
                Item::Function(f) => {
                    visit_function(ids, f, scopes, types, resolution_map, expr_types, warnings)
                }
                Item::Equip(eq) => {
                    for m in &eq.items {
                        visit_function(
                            ids, &m.node, scopes, types, resolution_map, expr_types, warnings,
                        );
                    }
                }
                Item::Trait(td) => {
                    for ti in &td.items {
                        if let crate::parser::ast::TraitItem::Method(m) = &ti.node {
                            visit_function(
                                ids, m, scopes, types, resolution_map, expr_types, warnings,
                            );
                        }
                    }
                }
                Item::Module { items: inner, .. } => {
                    walk(ids, inner, scopes, types, resolution_map, expr_types, warnings)
                }
                _ => {}
            }
        }
    }
    walk(ids, &module.items, scopes, types, resolution_map, expr_types, warnings);
}

fn visit_function(
    ids: &ContextIds,
    func: &FunctionDef,
    scopes: &ScopeTable,
    types: &mut TypeTable,
    resolution_map: &ResolutionMap,
    expr_types: &FxHashMap<Span, TypeId>,
    warnings: &mut Vec<SemanticWarning>,
) {
    // Only fire for functions whose declared return type is `Result[T, E]`
    // AND that are NOT already declared `throws` (the suggestion would be a no-op).
    if func.throws.declares_throws() {
        return;
    }
    // Reject extern / declaration bodies up-front.
    if matches!(
        func.body,
        FunctionBody::Declaration | FunctionBody::Extern(_)
    ) {
        return;
    }
    // Resolve the return type to a TypeId so we can detect `Result[T, E]`.
    let return_tid = match crate::semantic::types::ast_type_to_resolved(
        &func.return_type.node,
        func.return_type.span,
        scopes,
        types,
    ) {
        Ok(t) => t,
        Err(_) => return,
    };
    // Now build the read-only context for the walker.
    let ctx = Context {
        scopes,
        types,
        resolution_map,
        expr_types,
        ids,
    };
    if !ctx.is_result_type(return_tid) {
        return;
    }
    let err_tid = match ctx.result_error_type(return_tid) {
        Some(t) => t,
        None => return,
    };

    // Walk the body collecting offending sites. The ExprVisitor default
    // walks cover every Stmt/Expr variant — we only override `visit_stmt`
    // to detect the `<type> x = match ...` / `target = match ...` shapes
    // at their parent Stmt position. Everything else recurses normally.
    let mut walker = SiteCollector { ctx: &ctx, sites: Vec::new() };
    match &func.body {
        FunctionBody::Block(b) => walker.visit_block(b),
        FunctionBody::Expression(e) => walker.visit_expr(e),
        FunctionBody::Declaration | FunctionBody::Extern(_) => unreachable!(),
    }
    if walker.sites.is_empty() {
        return;
    }

    // One suggestion per function, anchored at the function's name span.
    warnings.push(SemanticWarning {
        kind: SemanticWarningKind::SuggestThrowsRefactor {
            fn_name: func.name.node.clone(),
            error_type: ctx.describe_error_type(err_tid),
            occurrence_count: walker.sites.len(),
        },
        span: func.name.span,
    });
}

/// AST walker that examines every `Stmt::VarDecl` / `Stmt::Assign` for the
/// match-unwrap-or-rethrow shape, then defers to the default walker to
/// continue recursion into nested bodies.
struct SiteCollector<'a, 'b> {
    ctx: &'a Context<'b>,
    sites: Vec<Span>,
}

impl<'a, 'b> crate::parser::visitor::ExprVisitor for SiteCollector<'a, 'b> {
    fn visit_stmt(&mut self, stmt: &Spanned<Stmt>) {
        match &stmt.node {
            Stmt::VarDecl { value, .. } | Stmt::Assign { value, .. } => {
                if let Some(span) = match_unwrap_rethrow_site(self.ctx, value) {
                    self.sites.push(span);
                }
            }
            _ => {}
        }
        crate::parser::visitor::walk_stmt(self, stmt);
    }
}

/// Returns the scrutinee span if `expr` is a `match` expression matching the
/// unwrap-or-rethrow shape on a Result scrutinee.
fn match_unwrap_rethrow_site(ctx: &Context, expr: &Spanned<Expr>) -> Option<Span> {
    let (scrutinee, arms, else_arm) = match &expr.node {
        Expr::Match { scrutinee, arms, else_arm } => (scrutinee, arms, else_arm),
        _ => return None,
    };
    // Strictly two arms, no else.
    if arms.len() != 2 || else_arm.is_some() {
        return None;
    }
    // Scrutinee must type as Result[T, E].
    let scrut_tid = ctx.expr_types.get(&scrutinee.span).copied()?;
    // The TypeId may still be an unresolved Var (e.g. when inference
    // bottomed out at an error). Resolve through the substitution map
    // — but we don't have access to that here, so we accept a direct
    // `Generic(Result, [_, _])` hit only. Anything else (Var, error,
    // primitive) is a no-fire.
    if !ctx.is_result_type(scrut_tid) {
        return None;
    }

    // Identify which arm is Ok and which is Error.
    let (ok_arm, err_arm) = classify_arms(ctx, &arms[0], &arms[1])?;

    // Ok arm: pattern `Ok(v)` with body that returns the bound name `v`.
    if !is_ok_unwrap_arm(ctx, ok_arm) {
        return None;
    }
    // Error arm: pattern `Error(e)` whose body rethrows `e` unchanged.
    if !is_error_rethrow_arm(ctx, err_arm) {
        return None;
    }
    Some(scrutinee.span)
}

/// Classify the two arms by which one binds `Ok` and which binds `Error`.
/// Returns `(ok_arm, error_arm)` or None if the arms aren't the expected
/// 2-arm Result destructure.
fn classify_arms<'a>(
    ctx: &Context,
    a: &'a MatchArm,
    b: &'a MatchArm,
) -> Option<(&'a MatchArm, &'a MatchArm)> {
    let a_var = constructor_variant_def(ctx, &a.pattern.node)?;
    let b_var = constructor_variant_def(ctx, &b.pattern.node)?;
    if a_var == ctx.ids.ok_def_id && b_var == ctx.ids.error_def_id {
        Some((a, b))
    } else if a_var == ctx.ids.error_def_id && b_var == ctx.ids.ok_def_id {
        Some((b, a))
    } else {
        None
    }
}

/// Return the DefId of the variant a Constructor pattern matches, or None
/// if the pattern isn't a single variant constructor.
fn constructor_variant_def(ctx: &Context, pattern: &Pattern) -> Option<DefId> {
    if let Pattern::Constructor { path, .. } = pattern {
        return ctx.resolve_variant_def(path);
    }
    None
}

/// `case Ok(v): v` — single binding field whose name is returned as the
/// match arm's value. Also accepts `case Ok(_): <anything>` if the arm
/// type-checks (we don't currently special-case that — only the strict
/// "binds v, returns v" shape is detected, to keep false positives low).
fn is_ok_unwrap_arm(_ctx: &Context, arm: &MatchArm) -> bool {
    if arm.guard.is_some() {
        return false;
    }
    let bound = match &arm.pattern.node {
        Pattern::Constructor { fields, .. } if fields.len() == 1 => match &fields[0].node {
            Pattern::Binding(name) => name.clone(),
            _ => return false,
        },
        _ => return false,
    };
    // The arm body should evaluate to the bound name unchanged.
    body_returns_identifier(&arm.body, &bound)
}

/// `case Error(e): return Error(e)` (or equivalent rethrow shapes).
/// We accept:
///   * `return Error(e)`     — same variant, same bound name
///   * `return e`            — if E is the error type (cheap to verify)
///   * `throw e`             — when the surrounding fn becomes `throws E`
///                              this would be the post-refactor shape
///   * a `Block` whose single statement is one of the above
fn is_error_rethrow_arm(ctx: &Context, arm: &MatchArm) -> bool {
    if arm.guard.is_some() {
        return false;
    }
    let bound = match &arm.pattern.node {
        Pattern::Constructor { fields, .. } if fields.len() == 1 => match &fields[0].node {
            Pattern::Binding(name) => name.clone(),
            _ => return false,
        },
        _ => return false,
    };
    body_is_rethrow_of(ctx, &arm.body, &bound)
}

/// True iff `expr` evaluates exactly to the identifier `name`.
/// Also unwraps a `Block` whose sole non-trivial statement is `name`.
fn body_returns_identifier(expr: &Spanned<Expr>, name: &str) -> bool {
    match &expr.node {
        Expr::Identifier(n) => n == name,
        Expr::Block(b) => {
            let mut tail: Option<&Spanned<Expr>> = None;
            for s in &b.stmts {
                match &s.node {
                    Stmt::Pass => {}
                    Stmt::Expr(e) => {
                        if tail.is_some() {
                            return false;
                        }
                        tail = Some(e);
                    }
                    _ => return false,
                }
            }
            tail.map_or(false, |e| body_returns_identifier(e, name))
        }
        _ => false,
    }
}

/// True iff `expr` is a rethrow of the binding `name` from the Error arm.
fn body_is_rethrow_of(ctx: &Context, expr: &Spanned<Expr>, name: &str) -> bool {
    // Unwrap a single-statement block.
    if let Expr::Block(b) = &expr.node {
        let mut stmts = b
            .stmts
            .iter()
            .filter(|s| !matches!(s.node, Stmt::Pass));
        let first = match stmts.next() {
            Some(s) => s,
            None => return false,
        };
        if stmts.next().is_some() {
            return false; // multi-stmt body → not the canonical shape
        }
        return stmt_is_rethrow_of(ctx, &first.node, name);
    }
    false
}

fn stmt_is_rethrow_of(ctx: &Context, stmt: &Stmt, name: &str) -> bool {
    match stmt {
        Stmt::Return(Some(e)) => return_value_is_rethrow_of(ctx, e, name),
        Stmt::Throw(e) => expr_is_identifier(e, name),
        _ => false,
    }
}

/// `return Error(name)` or `return name`.
fn return_value_is_rethrow_of(ctx: &Context, expr: &Spanned<Expr>, name: &str) -> bool {
    if expr_is_identifier(expr, name) {
        return true;
    }
    if let Some((variant_def, single_arg_is_name)) = call_variant_single_arg(ctx, expr, name) {
        return variant_def == ctx.ids.error_def_id && single_arg_is_name;
    }
    false
}

fn expr_is_identifier(expr: &Spanned<Expr>, name: &str) -> bool {
    matches!(&expr.node, Expr::Identifier(n) if n == name)
}

/// If `expr` is a call to a variant constructor with exactly one positional
/// argument that is the identifier `name`, return `(variant_def_id, true)`.
fn call_variant_single_arg(
    ctx: &Context,
    expr: &Spanned<Expr>,
    name: &str,
) -> Option<(DefId, bool)> {
    // Bare `Error(name)` — function call where callee is an Identifier.
    if let Expr::Call { callee, args, .. } = &expr.node {
        if args.len() != 1 || args[0].node.name.is_some() {
            return None;
        }
        let arg_is_name = expr_is_identifier(&args[0].node.value, name);
        if let Expr::Identifier(callee_name) = &callee.node {
            // Use resolution map to get the variant DefId from the callee's span.
            let def_id = ctx
                .resolution_map
                .get(&callee.span.start)
                .copied()
                .or_else(|| ctx.scopes.lookup(callee_name))?;
            return Some((def_id, arg_is_name));
        }
    }
    // Qualified `Result.Error(name)` parses as MethodCall.
    if let Expr::MethodCall { receiver, method, args, .. } = &expr.node {
        if args.len() != 1 || args[0].node.name.is_some() {
            return None;
        }
        let arg_is_name = expr_is_identifier(&args[0].node.value, name);
        if let Expr::Identifier(_) = &receiver.node {
            // Variants are top-level definitions for Result/Option from prelude;
            // scope lookup by the method name gives the variant DefId.
            if let Some(def_id) = ctx.scopes.lookup(&method.node) {
                return Some((def_id, arg_is_name));
            }
        }
    }
    None
}
