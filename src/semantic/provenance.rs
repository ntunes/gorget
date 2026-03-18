//! Pass 4.5: String provenance inference.
//!
//! After type checking (Pass 4) assigns all String-typed bindings `owned_string_id`
//! (conservative default), this pass downgrades provably-view bindings to `string_id`
//! (Str/Copy/reference type). The borrow checker (Pass 5) then sees the real
//! representation and enforces lifetimes for views, move semantics for owned.
//!
//! Only these bindings are downgraded:
//! 1. Bare borrow String parameters (no `&`/`!`)
//! 2. For-loop bindings over String collections
//! 3. Match bindings from String scrutinees
//! 4. Function return types where ALL return expressions are views
//!
//! VarDecl bindings stay Owned (conservative). The borrow checker handles
//! ownership tracking for them.

use rustc_hash::FxHashMap;

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use super::ids::{DefId, ScopeId, TypeId};
use super::resolve::{FunctionInfo, ResolutionMap};
use super::scope::ScopeTable;
use super::types::{ResolvedType, TypeTable};

/// Provenance of a String binding: View (Str, Copy) or Owned (GorgetString, Move).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringProvenance {
    View,
    Owned,
}

/// View-returning string methods (return a pointer into the receiver, no allocation).
const VIEW_METHODS: &[&str] = &[
    "trim", "strip", "lstrip", "rstrip",
    "removeprefix", "removesuffix",
    "byte_slice", "substring", "char_at", "as_str",
];

/// Run provenance inference on all functions in the module.
/// Rewrites `DefInfo.type_id` from `owned_string_id` → `string_id` for view bindings.
pub fn infer_string_provenance(
    module: &Module,
    scopes: &mut ScopeTable,
    types: &TypeTable,
    resolution_map: &ResolutionMap,
    expr_types: &FxHashMap<Span, TypeId>,
    function_info: &mut FxHashMap<DefId, FunctionInfo>,
    method_resolutions: &FxHashMap<usize, DefId>,
) {
    let string_id = types.string_id;
    let owned_string_id = types.owned_string_id;

    let mut ctx = ProvenanceCtx {
        scopes,
        types,
        resolution_map,
        expr_types,
        function_info,
        method_resolutions,
        string_id,
        owned_string_id,
    };

    infer_items(&mut ctx, &module.items);
}

struct ProvenanceCtx<'a> {
    scopes: &'a mut ScopeTable,
    types: &'a TypeTable,
    resolution_map: &'a ResolutionMap,
    expr_types: &'a FxHashMap<Span, TypeId>,
    function_info: &'a mut FxHashMap<DefId, FunctionInfo>,
    method_resolutions: &'a FxHashMap<usize, DefId>,
    string_id: TypeId,
    owned_string_id: TypeId,
}

impl<'a> ProvenanceCtx<'a> {
    fn is_owned_string(&self, tid: TypeId) -> bool {
        tid == self.owned_string_id
    }

    fn is_any_string(&self, tid: TypeId) -> bool {
        tid == self.string_id || tid == self.owned_string_id
    }

    /// Classify an expression's string provenance.
    ///
    /// View = borrows from another variable (pointer copy).
    /// Owned = allocates new data or takes ownership (GorgetString).
    fn classify_expr(&self, expr: &Spanned<Expr>) -> StringProvenance {
        match &expr.node {
            // String literals → Owned (allocates a GorgetString in IR)
            // Even plain literals create owned values at the variable level.
            Expr::StringLiteral(_) => StringProvenance::Owned,

            // Concat → Owned (allocates new GorgetString)
            Expr::BinaryOp { op: BinaryOp::Add, .. } => StringProvenance::Owned,

            // Move → Owned (takes ownership)
            Expr::Move { .. } => StringProvenance::Owned,

            // Identifier (without move) → View (borrows from source variable)
            Expr::Identifier(_) | Expr::SelfExpr => StringProvenance::View,

            // Function call → check callee's return type; if view, result is a view
            Expr::Call { callee, .. } => {
                if let Expr::Identifier(name) = &callee.node {
                    if let Some(def_id) = self.scopes.lookup_from_scope(ScopeId(0), &name) {
                        if self.callee_returns_view(def_id) {
                            return StringProvenance::View;
                        }
                    }
                }
                StringProvenance::Owned
            }

            // Method call → check VIEW_METHODS list, then callee's return type
            Expr::MethodCall { receiver, method, .. } => {
                if self.is_string_expr(receiver) && VIEW_METHODS.contains(&method.node.as_str()) {
                    return StringProvenance::View;
                }
                // Check if the resolved method returns a view (keyed by method name span)
                if let Some(&def_id) = self.method_resolutions.get(&method.span.start) {
                    if self.callee_returns_view(def_id) {
                        return StringProvenance::View;
                    }
                }
                StringProvenance::Owned
            }

            // Field access → View (borrows from object's field)
            Expr::FieldAccess { object, .. } => self.classify_expr(object),

            // Match expression → conservative: Owned if any arm is Owned
            Expr::Match { arms, else_arm, .. } => {
                let any_owned = arms.iter().any(|arm| {
                    self.classify_expr(&arm.body) == StringProvenance::Owned
                }) || else_arm.as_ref().map_or(false, |e| {
                    self.classify_expr(e) == StringProvenance::Owned
                });
                if any_owned { StringProvenance::Owned } else { StringProvenance::View }
            }

            // If expression → conservative: Owned if any branch is Owned
            Expr::If { then_branch, elif_branches, else_branch, .. } => {
                let mut any_owned = self.classify_expr(then_branch) == StringProvenance::Owned;
                for (_, body) in elif_branches {
                    if self.classify_expr(body) == StringProvenance::Owned {
                        any_owned = true;
                    }
                }
                if let Some(else_br) = else_branch {
                    if self.classify_expr(else_br) == StringProvenance::Owned {
                        any_owned = true;
                    }
                }
                if any_owned { StringProvenance::Owned } else { StringProvenance::View }
            }

            // Everything else → Owned (conservative)
            _ => StringProvenance::Owned,
        }
    }

    /// Check if a callee's return type has been downgraded to view (string_id).
    fn callee_returns_view(&self, def_id: DefId) -> bool {
        if let Some(fi) = self.function_info.get(&def_id) {
            if let Some(ret_tid) = fi.return_type_id {
                return ret_tid == self.string_id;
            }
        }
        false
    }

    /// Classify a return expression's provenance. Unlike `classify_expr`, plain
    /// string literals (no interpolation) are View here because they point to static
    /// data that outlives any function call (no dangling risk). F-strings allocate
    /// and are Owned.
    fn classify_return_expr(&self, expr: &Spanned<Expr>) -> StringProvenance {
        match &expr.node {
            Expr::StringLiteral(lit) if !lit.has_interpolation() => StringProvenance::View,
            _ => self.classify_expr(expr),
        }
    }

    fn is_string_expr(&self, expr: &Spanned<Expr>) -> bool {
        if let Some(&tid) = self.expr_types.get(&expr.span) {
            return self.is_any_string(tid);
        }
        // For identifiers, check the variable's type in the scope table
        if let Expr::Identifier(name) = &expr.node {
            if let Some(def_id) = self.find_binding_def(name, expr.span) {
                let def = self.scopes.get_def(def_id);
                if let Some(tid) = def.type_id {
                    return self.is_any_string(tid);
                }
            }
        }
        matches!(&expr.node, Expr::StringLiteral(_))
    }

    fn is_string_ast_type(&self, ty: &Type) -> bool {
        matches!(ty, Type::Primitive(PrimitiveType::StringType | PrimitiveType::Str))
    }

    fn downgrade_to_view(&mut self, def_id: DefId) {
        let def = self.scopes.get_def_mut(def_id);
        if let Some(tid) = def.type_id {
            if tid == self.owned_string_id {
                def.type_id = Some(self.string_id);
            }
        }
    }

    fn find_binding_def(&self, name: &str, span: Span) -> Option<DefId> {
        self.scopes.lookup_def_by_span(name, span)
            .or_else(|| self.resolution_map.get(&span.start).copied())
    }

    /// Infer provenance for a function's parameters and body.
    fn infer_function(&mut self, func: &FunctionDef) {
        // Look up function def_id for updating function_info
        let func_def_id = self.scopes.lookup_from_scope(ScopeId(0), &func.name.node)
            .or_else(|| self.scopes.lookup_def_by_span(&func.name.node, func.name.span))
            .or_else(|| self.resolution_map.get(&func.name.span.start).copied());

        // Parameters — bare borrow String params become views
        for (i, param) in func.params.iter().enumerate() {
            if !self.is_string_ast_type(&param.node.type_.node) {
                continue;
            }
            if param.node.ownership == Ownership::Borrow {
                if let Some(def_id) = self.find_binding_def(
                    &param.node.name.node, param.node.name.span
                ) {
                    let def = self.scopes.get_def(def_id);
                    if let Some(tid) = def.type_id {
                        if self.is_owned_string(tid) {
                            self.downgrade_to_view(def_id);
                            // Also update param_type_ids in function_info so
                            // return_borrows_from elision can see the param as ref
                            if let Some(fid) = func_def_id {
                                if let Some(fi) = self.function_info.get_mut(&fid) {
                                    if i < fi.param_type_ids.len() {
                                        fi.param_type_ids[i] = Some(self.string_id);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Recurse into body for for-loop/match bindings
        match &func.body {
            FunctionBody::Block(block) => self.infer_block(block),
            _ => {}
        }

        // Return type — only downgrade explicitly-owned `String` returns to view
        // when provably safe. `str` returns are already string_id from the type checker.
        // Currently disabled: String returns keep owned semantics.
        // Future: enable for String functions that provably return only views.
        if false && self.is_string_ast_type(&func.return_type.node) {
            if let Some(def_id) = func_def_id {
                let should_downgrade = match &func.body {
                    FunctionBody::Block(block) => {
                        // Body present: downgrade if ALL returns are views/static
                        let mut returns = Vec::new();
                        collect_return_exprs(&block.stmts, &mut returns);
                        !returns.is_empty() && returns.iter().all(|e| {
                            self.classify_return_expr(e) == StringProvenance::View
                        })
                    }
                    FunctionBody::Expression(expr) => {
                        self.classify_return_expr(expr.as_ref()) == StringProvenance::View
                    }
                    FunctionBody::Declaration | FunctionBody::Extern(_) => {
                        // Bodyless function: if it has bare-borrow String params,
                        // the return likely borrows from them → downgrade to view.
                        // This is conservative in the safety direction.
                        func.params.iter().any(|p| {
                            self.is_string_ast_type(&p.node.type_.node)
                                && p.node.ownership == Ownership::Borrow
                        })
                    }
                };

                if should_downgrade {
                    if let Some(fi) = self.function_info.get_mut(&def_id) {
                        if let Some(ret_tid) = fi.return_type_id {
                            if ret_tid == self.owned_string_id {
                                fi.return_type_id = Some(self.string_id);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Infer provenance for all statements in a block.
    fn infer_block(&mut self, block: &Block) {
        // Collect String VarDecl bindings and classify their RHS
        let mut string_vars: Vec<(DefId, StringProvenance)> = Vec::new();
        for stmt in &block.stmts {
            self.collect_string_decls(&stmt.node, &mut string_vars);
        }

        // Scan ALL assignments to check for reassignment that changes provenance
        for stmt in &block.stmts {
            self.check_assignments(&stmt.node, &mut string_vars);
        }

        // Apply downgrades for variables that stayed View
        for (def_id, prov) in &string_vars {
            if *prov == StringProvenance::View {
                self.downgrade_to_view(*def_id);
            }
        }

        // Recurse into nested blocks
        for stmt in &block.stmts {
            self.recurse_into_stmt(&stmt.node);
        }
    }

    /// Collect String-typed VarDecl bindings with their initial provenance.
    fn collect_string_decls(&self, stmt: &Stmt, out: &mut Vec<(DefId, StringProvenance)>) {
        if let Stmt::VarDecl { type_, pattern, value, .. } = stmt {
            if !self.is_string_ast_type(&type_.node) {
                return;
            }
            let prov = self.classify_expr(value);
            if let Pattern::Binding(name) = &pattern.node {
                if let Some(def_id) = self.find_binding_def(name, pattern.span) {
                    let def = self.scopes.get_def(def_id);
                    if let Some(tid) = def.type_id {
                        if self.is_owned_string(tid) {
                            out.push((def_id, prov));
                        }
                    }
                }
            }
        }
    }

    /// Check assignments that might upgrade a View variable to Owned.
    fn check_assignments(&self, stmt: &Stmt, vars: &mut Vec<(DefId, StringProvenance)>) {
        match stmt {
            Stmt::Assign { target, value } => {
                if let Some(&def_id) = self.resolution_map.get(&target.span.start) {
                    for (vid, prov) in vars.iter_mut() {
                        if *vid == def_id && *prov == StringProvenance::View {
                            if self.classify_expr(value) == StringProvenance::Owned {
                                *prov = StringProvenance::Owned;
                            }
                        }
                    }
                }
            }
            Stmt::CompoundAssign { target, .. } => {
                if let Some(&def_id) = self.resolution_map.get(&target.span.start) {
                    for (vid, prov) in vars.iter_mut() {
                        if *vid == def_id {
                            *prov = StringProvenance::Owned; // += always allocates
                        }
                    }
                }
            }
            Stmt::If { then_body, elif_branches, else_body, .. } => {
                for s in &then_body.stmts { self.check_assignments(&s.node, vars); }
                for (_, branch) in elif_branches {
                    for s in &branch.stmts { self.check_assignments(&s.node, vars); }
                }
                if let Some(else_br) = else_body {
                    for s in &else_br.stmts { self.check_assignments(&s.node, vars); }
                }
            }
            Stmt::While { body, .. } | Stmt::Loop { body, .. } | Stmt::For { body, .. } => {
                for s in &body.stmts { self.check_assignments(&s.node, vars); }
            }
            Stmt::Match { else_arm, .. } => {
                if let Some(else_block) = else_arm {
                    for s in &else_block.stmts { self.check_assignments(&s.node, vars); }
                }
            }
            Stmt::Select { arms, else_arm, .. } => {
                for arm in arms {
                    for s in &arm.body.stmts { self.check_assignments(&s.node, vars); }
                }
                if let Some(else_block) = else_arm {
                    for s in &else_block.stmts { self.check_assignments(&s.node, vars); }
                }
            }
            Stmt::With { body, .. } | Stmt::NamedScope { body, .. } | Stmt::Unsafe { body, .. } => {
                for s in &body.stmts { self.check_assignments(&s.node, vars); }
            }
            _ => {}
        }
    }

    fn recurse_into_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::If { then_body, elif_branches, else_body, .. } => {
                self.infer_block(then_body);
                for (_, branch) in elif_branches {
                    self.infer_block(branch);
                }
                if let Some(else_br) = else_body {
                    self.infer_block(else_br);
                }
            }
            Stmt::While { body, .. } | Stmt::Loop { body, .. } => {
                self.infer_block(body);
            }
            Stmt::For { pattern, iterable, body, .. } => {
                self.maybe_downgrade_for_binding(pattern, iterable);
                self.infer_block(body);
            }
            Stmt::Match { scrutinee, arms, else_arm, .. } => {
                let scrutinee_is_string = self.is_string_expr(scrutinee);
                for arm in arms {
                    if let MatchItem::Arm(a) = arm {
                        if scrutinee_is_string {
                            self.downgrade_pattern_bindings(&a.pattern);
                        }
                    }
                }
                if let Some(else_block) = else_arm {
                    self.infer_block(else_block);
                }
            }
            Stmt::Select { arms, else_arm, .. } => {
                for arm in arms {
                    self.infer_block(&arm.body);
                }
                if let Some(else_block) = else_arm {
                    self.infer_block(else_block);
                }
            }
            Stmt::With { body, .. } | Stmt::NamedScope { body, .. } | Stmt::Unsafe { body, .. } => {
                self.infer_block(body);
            }
            _ => {}
        }
    }

    fn maybe_downgrade_for_binding(&mut self, pattern: &Spanned<Pattern>, iterable: &Spanned<Expr>) {
        if let Some(&tid) = self.expr_types.get(&iterable.span) {
            if let ResolvedType::Generic(_, args) = self.types.get(tid) {
                if args.len() == 1 && self.is_any_string(args[0]) {
                    self.downgrade_pattern_bindings(pattern);
                }
            }
        }
    }

    fn downgrade_pattern_bindings(&mut self, pattern: &Spanned<Pattern>) {
        match &pattern.node {
            Pattern::Binding(name) => {
                if let Some(def_id) = self.find_binding_def(name, pattern.span) {
                    let def = self.scopes.get_def(def_id);
                    if let Some(tid) = def.type_id {
                        if self.is_owned_string(tid) {
                            self.downgrade_to_view(def_id);
                        }
                    }
                }
            }
            Pattern::Tuple(pats) | Pattern::Constructor { fields: pats, .. } => {
                for p in pats {
                    self.downgrade_pattern_bindings(p);
                }
            }
            Pattern::Or(pats) => {
                for p in pats {
                    self.downgrade_pattern_bindings(p);
                }
            }
            _ => {}
        }
    }
}

/// Collect all return expressions from a list of statements.
fn collect_return_exprs<'a>(stmts: &'a [Spanned<Stmt>], out: &mut Vec<&'a Spanned<Expr>>) {
    for stmt in stmts {
        match &stmt.node {
            Stmt::Return(Some(expr)) => out.push(expr),
            Stmt::If { then_body, elif_branches, else_body, .. } => {
                collect_return_exprs(&then_body.stmts, out);
                for (_, branch) in elif_branches {
                    collect_return_exprs(&branch.stmts, out);
                }
                if let Some(else_br) = else_body {
                    collect_return_exprs(&else_br.stmts, out);
                }
            }
            Stmt::While { body, .. } | Stmt::Loop { body, .. } => {
                collect_return_exprs(&body.stmts, out);
            }
            Stmt::For { body, .. } => {
                collect_return_exprs(&body.stmts, out);
            }
            Stmt::Match { else_arm, .. } => {
                if let Some(else_block) = else_arm {
                    collect_return_exprs(&else_block.stmts, out);
                }
            }
            Stmt::Select { arms, else_arm, .. } => {
                for arm in arms {
                    collect_return_exprs(&arm.body.stmts, out);
                }
                if let Some(else_block) = else_arm {
                    collect_return_exprs(&else_block.stmts, out);
                }
            }
            Stmt::With { body, .. } | Stmt::NamedScope { body, .. } | Stmt::Unsafe { body, .. } => {
                collect_return_exprs(&body.stmts, out);
            }
            _ => {}
        }
    }
}

/// Walk all items in the module, recursing into Module wrappers.
fn infer_items(ctx: &mut ProvenanceCtx, items: &[Spanned<Item>]) {
    for item in items {
        match &item.node {
            Item::Module { items: inner, .. } => infer_items(ctx, inner),
            Item::Function(f) => ctx.infer_function(f),
            Item::Equip(equip) => {
                for method in &equip.items {
                    ctx.infer_function(&method.node);
                }
            }
            Item::Test(t) => ctx.infer_block(&t.body),
            Item::Bench(b) => ctx.infer_block(&b.body),
            Item::SuiteSetup(s) => ctx.infer_block(&s.body),
            Item::SuiteTeardown(s) => ctx.infer_block(&s.body),
            _ => {}
        }
    }
}
