/// Closure codegen: lambda lifting, free variable collection, and mutation detection.
use crate::parser::ast::{Expr, SelectOp};
use crate::span::Spanned;

use super::c_mangle;
use super::c_types;
use super::CaptureMode;
use super::CodegenContext;
use super::LiftedClosure;

impl CodegenContext<'_> {
    /// Generate a closure expression via lambda lifting.
    pub(super) fn gen_closure_expr(
        &mut self,
        params: &[Spanned<crate::parser::ast::ClosureParam>],
        body: &Spanned<Expr>,
        is_move: bool,
    ) -> String {
        let id = {
            let id = self.closure_counter;
            self.closure_counter += 1;
            id
        };

        let fn_name = c_mangle::mangle_closure(id);
        let env_name = c_mangle::mangle_closure_env(id);
        let struct_name = format!("__Closure_{id}");

        // Build parameter list
        let closure_params: Vec<(String, String)> = params
            .iter()
            .map(|p| {
                let name = c_mangle::escape_keyword(&p.node.name.node);
                let ty = p
                    .node
                    .type_
                    .as_ref()
                    .map(|t| c_types::ast_type_to_c(&t.node, self.scopes))
                    .or_else(|| {
                        self.scopes
                            .lookup_def_by_span(&p.node.name.node, p.node.name.span)
                            .and_then(|def_id| self.scopes.get_def(def_id).type_id)
                            .map(|tid| self.type_id_to_c_substituted(tid))
                    })
                    .unwrap_or_else(|| "int64_t".to_string());
                (name, ty)
            })
            .collect();

        // Collect free variables
        let param_names: std::collections::HashSet<&str> =
            params.iter().map(|p| p.node.name.node.as_str()).collect();
        let free_vars = self.collect_free_vars(body, &param_names);

        // Detect mutations: walk body for assignments to captured variables
        let mutated = if is_move {
            // Move closures own their captures — all by value
            std::collections::HashSet::new()
        } else {
            self.detect_mutations(body, &param_names)
        };

        // Build captures with modes.
        // For escaping closures (heap-allocated env), force all captures to ByValue
        // to prevent use-after-free: ByMutRef stores a pointer to the caller's
        // stack variable, which is invalid after the function returns.
        let force_by_value = self.closure_heap_alloc;
        let captures: Vec<(String, String, CaptureMode)> = free_vars
            .into_iter()
            .map(|(name, ty)| {
                let mode = if !force_by_value && mutated.contains(&name) {
                    CaptureMode::ByMutRef
                } else {
                    CaptureMode::ByValue
                };
                (name, ty, mode)
            })
            .collect();

        // Set mutable_captures so that gen_expr emits (*__env->NAME)
        let prev_mutable = std::mem::take(&mut self.mutable_captures);
        for (name, _, mode) in &captures {
            if *mode == CaptureMode::ByMutRef {
                self.mutable_captures.insert(name.clone());
            }
        }

        // Generate the body expression
        let body_expr = self.gen_expr(body);

        // Restore previous mutable_captures (for nested closures)
        self.mutable_captures = prev_mutable;

        let return_type = self.closure_return_type_hint.take()
            .unwrap_or_else(|| self.infer_c_type_from_expr(&body.node));
        let lifted = LiftedClosure {
            id,
            struct_name: struct_name.clone(),
            captures: captures.clone(),
            params: closure_params,
            return_type,
            body: body_expr,
        };

        self.lifted_closures.push(lifted);

        // At the creation site: emit a bare function name (no captures)
        // or instantiate the per-closure struct.
        if captures.is_empty() {
            fn_name
        } else if self.closure_heap_alloc {
            // Heap-allocate closure struct for escaping closures (returned from function).
            let fields: Vec<String> = captures
                .iter()
                .map(|(cap_name, _, mode)| match mode {
                    CaptureMode::ByMutRef => format!(".{cap_name} = &{cap_name}"),
                    CaptureMode::ByValue => format!(".{cap_name} = {cap_name}"),
                })
                .collect();
            let field_init = fields.join(", ");
            // Also emit GorgetClosure for backward compat during transition — the
            // per-closure struct is heap-allocated and wrapped in GorgetClosure.
            format!(
                "({{ {env_name}* __heap_env = ({env_name}*)GORGET_ALLOC(sizeof({env_name})); \
                *__heap_env = ({env_name}){{{field_init}}}; \
                (GorgetClosure){{.fn_ptr = (void*){fn_name}, .env = (void*)__heap_env}}; }})"
            )
        } else {
            // Stack-allocated per-closure struct instance.
            // Return as GorgetClosure for backward compat — callers that use the
            // new per-closure dispatch will check closure_var_info for direct call.
            let fields: Vec<String> = captures
                .iter()
                .map(|(cap_name, _, mode)| match mode {
                    CaptureMode::ByMutRef => format!(".{cap_name} = &{cap_name}"),
                    CaptureMode::ByValue => format!(".{cap_name} = {cap_name}"),
                })
                .collect();
            let field_init = fields.join(", ");
            format!(
                "(GorgetClosure){{.fn_ptr = (void*){fn_name}, .env = (void*)&({env_name}){{{field_init}}}}}"
            )
        }
    }

    /// Collect free variable references from an expression.
    /// Uses the ExprVisitor trait for exhaustive variant coverage.
    pub(super) fn collect_free_vars(
        &mut self,
        expr: &Spanned<Expr>,
        bound: &std::collections::HashSet<&str>,
    ) -> Vec<(String, String)> {
        use crate::parser::visitor::ExprVisitor;
        let mut collector = FreeVarCollector {
            ctx: self,
            bound: bound.iter().map(|s| s.to_string()).collect(),
            seen: std::collections::HashSet::new(),
            free: Vec::new(),
        };
        collector.visit_expr(expr);
        std::mem::take(&mut collector.free)
    }

    /// Detect which free variables are mutated inside a closure body.
    /// Returns a set of escaped variable names that are assigned to.
    fn detect_mutations(
        &self,
        expr: &Spanned<Expr>,
        param_names: &std::collections::HashSet<&str>,
    ) -> std::collections::HashSet<String> {
        use crate::parser::visitor::ExprVisitor;
        let mut detector = MutationDetector {
            bound: param_names.iter().map(|s| s.to_string()).collect(),
            mutated: std::collections::HashSet::new(),
        };
        detector.visit_expr(expr);
        detector.mutated
    }
}

/// Visitor-based mutation detector for closure captures.
///
/// Walks a closure body to find which free variables are assigned to
/// (via `=` or `+=` etc.).  Uses the ExprVisitor trait for exhaustive
/// expression coverage; only overrides statement handling for scope
/// tracking and assignment detection.
struct MutationDetector {
    /// Variable names in scope (closure params + block-local declarations).
    bound: std::collections::HashSet<String>,
    /// Free variable names that are assigned to (the result).
    mutated: std::collections::HashSet<String>,
}

impl crate::parser::visitor::ExprVisitor for MutationDetector {
    // visit_expr: default walk_expr handles all expression variants exhaustively.

    fn visit_block(&mut self, block: &crate::parser::ast::Block) {
        // Save/restore bound for block scoping: VarDecls inside a block
        // should not leak to the enclosing scope.
        let saved = self.bound.clone();
        crate::parser::visitor::walk_block(self, block);
        self.bound = saved;
    }

    fn visit_stmt(&mut self, stmt: &crate::span::Spanned<crate::parser::ast::Stmt>) {
        use crate::parser::ast::{Pattern, Stmt};
        match &stmt.node {
            Stmt::Assign { target, value } => {
                if let Expr::Identifier(name) = &target.node {
                    if !self.bound.contains(name.as_str()) {
                        self.mutated.insert(c_mangle::escape_keyword(name));
                    }
                }
                self.visit_expr(value);
            }
            Stmt::CompoundAssign { target, value, .. } => {
                if let Expr::Identifier(name) = &target.node {
                    if !self.bound.contains(name.as_str()) {
                        self.mutated.insert(c_mangle::escape_keyword(name));
                    }
                }
                self.visit_expr(value);
            }
            Stmt::VarDecl { pattern, value, .. } => {
                self.visit_expr(value);
                if let Pattern::Binding(name) = &pattern.node {
                    self.bound.insert(name.clone());
                }
            }
            Stmt::For {
                iterable,
                body,
                pattern,
                else_body,
                ..
            } => {
                self.visit_expr(iterable);
                let saved = self.bound.clone();
                if let Pattern::Binding(name) = &pattern.node {
                    self.bound.insert(name.clone());
                }
                self.visit_block(body);
                self.bound = saved;
                if let Some(eb) = else_body {
                    self.visit_block(eb);
                }
            }
            _ => crate::parser::visitor::walk_stmt(self, stmt),
        }
    }
}

// ─── Visitor: Free Variable Collector ────────────────────────

/// Walks a closure body collecting free variable references (unbound
/// identifiers that need to be captured).  Uses the ExprVisitor trait
/// for exhaustive variant coverage; overrides expression, statement,
/// and block handling for scope tracking (VarDecl adds to bound,
/// blocks save/restore, patterns extend bound per match arm / for loop).
struct FreeVarCollector<'a, 'b> {
    ctx: &'a mut CodegenContext<'b>,
    /// Variable names currently in scope (closure params + block-local).
    bound: std::collections::HashSet<String>,
    /// Already-collected names (dedup).
    seen: std::collections::HashSet<String>,
    /// Collected free variables: (escaped_name, c_type).
    free: Vec<(String, String)>,
}

impl crate::parser::visitor::ExprVisitor for FreeVarCollector<'_, '_> {
    fn visit_expr(&mut self, expr: &crate::span::Spanned<Expr>) {
        match &expr.node {
            Expr::Identifier(name)
                if !self.bound.contains(name.as_str()) && name != "self" =>
            {
                let is_global = self.ctx.scopes.is_global_def(name);
                if !is_global && self.seen.insert(name.clone()) {
                    let ty = self.ctx.infer_c_type_from_expr(&expr.node);
                    self.free.push((c_mangle::escape_keyword(name), ty));
                }
            }
            // Nested closure: extend bound with its params, recurse into body
            Expr::Closure { params, body, .. } => {
                let saved = self.bound.clone();
                for p in params {
                    self.bound.insert(p.node.name.node.clone());
                }
                self.visit_expr(body);
                self.bound = saved;
            }
            // Match (expr): extend bound with pattern bindings per arm
            Expr::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                self.visit_expr(scrutinee);
                for arm in arms {
                    let saved = self.bound.clone();
                    collect_pattern_names_owned(&arm.pattern.node, &mut self.bound);
                    if let Some(guard) = &arm.guard {
                        self.visit_expr(guard);
                    }
                    self.visit_expr(&arm.body);
                    self.bound = saved;
                }
                if let Some(eb) = else_arm {
                    self.visit_expr(eb);
                }
            }
            // Default walk handles all other variants exhaustively
            _ => crate::parser::visitor::walk_expr(self, expr),
        }
    }

    fn visit_block(&mut self, block: &crate::parser::ast::Block) {
        // Save/restore bound for block scoping: VarDecls inside a block
        // should not leak to the enclosing scope.
        let saved = self.bound.clone();
        crate::parser::visitor::walk_block(self, block);
        self.bound = saved;
    }

    fn visit_stmt(&mut self, stmt: &crate::span::Spanned<crate::parser::ast::Stmt>) {
        use crate::parser::ast::{Pattern, Stmt};
        match &stmt.node {
            Stmt::VarDecl { pattern, value, .. } => {
                // Value is evaluated before binding, so walk it first
                self.visit_expr(value);
                // Then add the declared name to bound
                if let Pattern::Binding(name) = &pattern.node {
                    self.bound.insert(name.clone());
                }
            }
            Stmt::For {
                iterable,
                body,
                pattern,
                else_body,
                ..
            } => {
                self.visit_expr(iterable);
                let saved = self.bound.clone();
                collect_pattern_names_owned(&pattern.node, &mut self.bound);
                self.visit_block(body);
                self.bound = saved;
                if let Some(eb) = else_body {
                    self.visit_block(eb);
                }
            }
            // Match (stmt): extend bound with pattern bindings per arm
            Stmt::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                self.visit_expr(scrutinee);
                for arm in arms {
                    let saved = self.bound.clone();
                    collect_pattern_names_owned(&arm.pattern.node, &mut self.bound);
                    if let Some(guard) = &arm.guard {
                        self.visit_expr(guard);
                    }
                    self.visit_expr(&arm.body);
                    self.bound = saved;
                }
                if let Some(eb) = else_arm {
                    self.visit_block(eb);
                }
            }
            Stmt::Select { arms, else_arm } => {
                for arm in arms {
                    let saved = self.bound.clone();
                    match &arm.op {
                        SelectOp::Recv { channel, name, .. } => {
                            self.visit_expr(channel);
                            self.bound.insert(name.node.clone());
                        }
                        SelectOp::Send { channel, value } => {
                            self.visit_expr(channel);
                            self.visit_expr(value);
                        }
                    }
                    self.visit_block(&arm.body);
                    self.bound = saved;
                }
                if let Some(eb) = else_arm {
                    self.visit_block(eb);
                }
            }
            _ => crate::parser::visitor::walk_stmt(self, stmt),
        }
    }
}

/// Collect binding names from a pattern into a `HashSet<String>`.
fn collect_pattern_names_owned(
    pattern: &crate::parser::ast::Pattern,
    bound: &mut std::collections::HashSet<String>,
) {
    use crate::parser::ast::Pattern;
    match pattern {
        Pattern::Binding(name) => {
            bound.insert(name.clone());
        }
        Pattern::Constructor { fields, .. } => {
            for f in fields {
                collect_pattern_names_owned(&f.node, bound);
            }
        }
        Pattern::Tuple(pats) | Pattern::Or(pats) => {
            for p in pats {
                collect_pattern_names_owned(&p.node, bound);
            }
        }
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => {}
    }
}
