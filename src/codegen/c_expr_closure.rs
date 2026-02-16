/// Closure codegen: lambda lifting, free variable collection, and mutation detection.
use crate::parser::ast::Expr;
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
                            .map(|tid| c_types::type_id_to_c(tid, self.types, self.scopes))
                    })
                    .unwrap_or_else(|| "int64_t".to_string());
                (name, ty)
            })
            .collect();

        // Collect free variables
        let param_names: std::collections::HashSet<&str> =
            params.iter().map(|p| p.node.name.node.as_str()).collect();
        let free_vars = self.collect_free_vars(&body.node, &param_names);

        // Detect mutations: walk body for assignments to captured variables
        let mutated = if is_move {
            // Move closures own their captures — all by value
            std::collections::HashSet::new()
        } else {
            self.detect_mutations(&body.node, &param_names)
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

        let lifted = LiftedClosure {
            id,
            captures: captures.clone(),
            params: closure_params,
            return_type: self.infer_c_type_from_expr(&body.node),
            body: body_expr,
        };

        self.lifted_closures.push(lifted);

        // At the creation site: emit a bare function pointer (no captures)
        // or allocate env and create GorgetClosure.
        if captures.is_empty() {
            fn_name
        } else if self.closure_heap_alloc {
            // Heap-allocate env for escaping closures (returned from function).
            // Uses a GCC statement expression to malloc, init, and return the closure.
            let fields: Vec<String> = captures
                .iter()
                .map(|(cap_name, _, mode)| match mode {
                    CaptureMode::ByMutRef => format!(".{cap_name} = &{cap_name}"),
                    CaptureMode::ByValue => format!(".{cap_name} = {cap_name}"),
                })
                .collect();
            let field_init = fields.join(", ");
            format!(
                "({{ {env_name}* __heap_env = ({env_name}*)malloc(sizeof({env_name})); \
                *__heap_env = ({env_name}){{{field_init}}}; \
                (GorgetClosure){{.fn_ptr = (void*){fn_name}, .env = (void*)__heap_env}}; }})"
            )
        } else {
            // C99 compound literal: the env struct has automatic storage duration
            // tied to the enclosing block, so no malloc/free needed. The env lives
            // on the stack as long as the closure variable's scope.
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

    /// Collect free variable references from an expression (simple walk).
    pub(super) fn collect_free_vars(
        &mut self,
        expr: &Expr,
        bound: &std::collections::HashSet<&str>,
    ) -> Vec<(String, String)> {
        let mut free = Vec::new();
        let mut seen = std::collections::HashSet::new();
        self.walk_free_vars(expr, bound, &mut seen, &mut free);
        free
    }

    fn walk_free_vars(
        &mut self,
        expr: &Expr,
        bound: &std::collections::HashSet<&str>,
        seen: &mut std::collections::HashSet<String>,
        free: &mut Vec<(String, String)>,
    ) {
        match expr {
            Expr::Identifier(name) if !bound.contains(name.as_str()) && name != "self" => {
                // Skip global definitions (functions, enum variants, structs, etc.)
                // — they don't need to be captured, they're available globally in C.
                let is_global = self.scopes.is_global_def(name);
                if !is_global && seen.insert(name.clone()) {
                    let ty = self.infer_c_type_from_expr(expr);
                    free.push((c_mangle::escape_keyword(name), ty));
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                self.walk_free_vars(&left.node, bound, seen, free);
                self.walk_free_vars(&right.node, bound, seen, free);
            }
            Expr::UnaryOp { operand, .. } => {
                self.walk_free_vars(&operand.node, bound, seen, free);
            }
            Expr::Call { callee, args, .. } => {
                self.walk_free_vars(&callee.node, bound, seen, free);
                for arg in args {
                    self.walk_free_vars(&arg.node.value.node, bound, seen, free);
                }
            }
            Expr::FieldAccess { object, .. } => {
                self.walk_free_vars(&object.node, bound, seen, free);
            }
            Expr::TupleFieldAccess { object, .. } => {
                self.walk_free_vars(&object.node, bound, seen, free);
            }
            Expr::MethodCall {
                receiver, args, ..
            } => {
                self.walk_free_vars(&receiver.node, bound, seen, free);
                for arg in args {
                    self.walk_free_vars(&arg.node.value.node, bound, seen, free);
                }
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.walk_free_vars(&condition.node, bound, seen, free);
                self.walk_free_vars(&then_branch.node, bound, seen, free);
                if let Some(eb) = else_branch {
                    self.walk_free_vars(&eb.node, bound, seen, free);
                }
            }
            Expr::Index { object, index } => {
                self.walk_free_vars(&object.node, bound, seen, free);
                self.walk_free_vars(&index.node, bound, seen, free);
            }
            Expr::Block(block) | Expr::Do { body: block } => {
                self.walk_free_vars_in_block(block, &mut bound.clone(), seen, free);
            }
            Expr::Match { scrutinee, arms, else_arm } => {
                self.walk_free_vars(&scrutinee.node, bound, seen, free);
                for arm in arms {
                    // Pattern bindings are local to the arm
                    let mut arm_bound = bound.clone();
                    self.collect_pattern_names(&arm.pattern.node, &mut arm_bound);
                    if let Some(guard) = &arm.guard {
                        self.walk_free_vars(&guard.node, &arm_bound, seen, free);
                    }
                    self.walk_free_vars(&arm.body.node, &arm_bound, seen, free);
                }
                if let Some(else_body) = else_arm {
                    self.walk_free_vars(&else_body.node, bound, seen, free);
                }
            }
            Expr::Closure { params, body, .. } => {
                // Nested closure: its params are bound, recurse into body
                let mut inner_bound = bound.clone();
                for p in params {
                    inner_bound.insert(p.node.name.node.as_str());
                }
                self.walk_free_vars(&body.node, &inner_bound, seen, free);
            }
            Expr::StringLiteral(s) => {
                for seg in &s.segments {
                    if let crate::lexer::token::StringSegment::Interpolation(name) = seg {
                        // Treat interpolated names as identifier references
                        let fake = Expr::Identifier(name.clone());
                        self.walk_free_vars(&fake, bound, seen, free);
                    }
                }
            }
            _ => {}
        }
    }

    /// Walk a block's statements collecting free variables.
    fn walk_free_vars_in_block<'b>(
        &mut self,
        block: &'b crate::parser::ast::Block,
        bound: &mut std::collections::HashSet<&'b str>,
        seen: &mut std::collections::HashSet<String>,
        free: &mut Vec<(String, String)>,
    ) {
        for stmt in &block.stmts {
            self.walk_free_vars_in_stmt(&stmt.node, bound, seen, free);
        }
    }

    /// Walk a statement collecting free variables (with mutable bound set
    /// so that VarDecl names become bound for subsequent statements).
    fn walk_free_vars_in_stmt<'b>(
        &mut self,
        stmt: &'b crate::parser::ast::Stmt,
        bound: &mut std::collections::HashSet<&'b str>,
        seen: &mut std::collections::HashSet<String>,
        free: &mut Vec<(String, String)>,
    ) {
        use crate::parser::ast::{Pattern, Stmt};
        match stmt {
            Stmt::VarDecl { pattern, value, .. } => {
                // Value is evaluated before binding, so walk it first
                self.walk_free_vars(&value.node, bound, seen, free);
                // Then add the declared name to bound
                if let Pattern::Binding(name) = &pattern.node {
                    bound.insert(name.as_str());
                }
            }
            Stmt::Assign { target, value } => {
                self.walk_free_vars(&target.node, bound, seen, free);
                self.walk_free_vars(&value.node, bound, seen, free);
            }
            Stmt::CompoundAssign { target, value, .. } => {
                self.walk_free_vars(&target.node, bound, seen, free);
                self.walk_free_vars(&value.node, bound, seen, free);
            }
            Stmt::Expr(expr) => {
                self.walk_free_vars(&expr.node, bound, seen, free);
            }
            Stmt::Return(Some(expr)) => {
                self.walk_free_vars(&expr.node, bound, seen, free);
            }
            Stmt::For { iterable, body, else_body, pattern, .. } => {
                self.walk_free_vars(&iterable.node, bound, seen, free);
                let mut for_bound = bound.clone();
                self.collect_pattern_names(&pattern.node, &mut for_bound);
                self.walk_free_vars_in_block(body, &mut for_bound, seen, free);
                if let Some(eb) = else_body {
                    self.walk_free_vars_in_block(eb, bound, seen, free);
                }
            }
            Stmt::While { condition, body, else_body } => {
                self.walk_free_vars(&condition.node, bound, seen, free);
                self.walk_free_vars_in_block(body, &mut bound.clone(), seen, free);
                if let Some(eb) = else_body {
                    self.walk_free_vars_in_block(eb, bound, seen, free);
                }
            }
            Stmt::If { condition, then_body, elif_branches, else_body } => {
                self.walk_free_vars(&condition.node, bound, seen, free);
                self.walk_free_vars_in_block(then_body, &mut bound.clone(), seen, free);
                for (cond, body) in elif_branches {
                    self.walk_free_vars(&cond.node, bound, seen, free);
                    self.walk_free_vars_in_block(body, &mut bound.clone(), seen, free);
                }
                if let Some(eb) = else_body {
                    self.walk_free_vars_in_block(eb, &mut bound.clone(), seen, free);
                }
            }
            Stmt::Match { scrutinee, arms, else_arm } => {
                self.walk_free_vars(&scrutinee.node, bound, seen, free);
                for arm in arms {
                    let mut arm_bound: std::collections::HashSet<&str> = bound.iter().copied().collect();
                    self.collect_pattern_names(&arm.pattern.node, &mut arm_bound);
                    self.walk_free_vars(&arm.body.node, &arm_bound, seen, free);
                }
                if let Some(eb) = else_arm {
                    let mut eb_bound = bound.clone();
                    self.walk_free_vars_in_block(eb, &mut eb_bound, seen, free);
                }
            }
            _ => {}
        }
    }

    /// Collect binding names from a pattern into a bound set.
    fn collect_pattern_names<'b>(
        &self,
        pattern: &'b crate::parser::ast::Pattern,
        bound: &mut std::collections::HashSet<&'b str>,
    ) {
        use crate::parser::ast::Pattern;
        match pattern {
            Pattern::Binding(name) => { bound.insert(name.as_str()); }
            Pattern::Constructor { fields, .. } => {
                for f in fields {
                    self.collect_pattern_names(&f.node, bound);
                }
            }
            Pattern::Tuple(pats) => {
                for p in pats {
                    self.collect_pattern_names(&p.node, bound);
                }
            }
            Pattern::Or(pats) => {
                for p in pats {
                    self.collect_pattern_names(&p.node, bound);
                }
            }
            _ => {}
        }
    }

    /// Detect which free variables are mutated inside a closure body.
    /// Returns a set of escaped variable names that are assigned to.
    fn detect_mutations(
        &self,
        expr: &Expr,
        param_names: &std::collections::HashSet<&str>,
    ) -> std::collections::HashSet<String> {
        let mut mutated = std::collections::HashSet::new();
        self.walk_mutations(expr, param_names, &mut mutated);
        mutated
    }

    fn walk_mutations(
        &self,
        expr: &Expr,
        bound: &std::collections::HashSet<&str>,
        mutated: &mut std::collections::HashSet<String>,
    ) {
        match expr {
            Expr::Block(block) | Expr::Do { body: block } => {
                self.walk_mutations_in_block(block, &mut bound.clone(), mutated);
            }
            Expr::If { condition, then_branch, else_branch, .. } => {
                self.walk_mutations(&condition.node, bound, mutated);
                self.walk_mutations(&then_branch.node, bound, mutated);
                if let Some(eb) = else_branch {
                    self.walk_mutations(&eb.node, bound, mutated);
                }
            }
            Expr::Match { scrutinee, arms, else_arm } => {
                self.walk_mutations(&scrutinee.node, bound, mutated);
                for arm in arms {
                    self.walk_mutations(&arm.body.node, bound, mutated);
                }
                if let Some(eb) = else_arm {
                    self.walk_mutations(&eb.node, bound, mutated);
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                self.walk_mutations(&left.node, bound, mutated);
                self.walk_mutations(&right.node, bound, mutated);
            }
            Expr::UnaryOp { operand, .. } => {
                self.walk_mutations(&operand.node, bound, mutated);
            }
            Expr::Call { callee, args, .. } => {
                self.walk_mutations(&callee.node, bound, mutated);
                for arg in args {
                    self.walk_mutations(&arg.node.value.node, bound, mutated);
                }
            }
            _ => {}
        }
    }

    fn walk_mutations_in_block<'b>(
        &self,
        block: &'b crate::parser::ast::Block,
        bound: &mut std::collections::HashSet<&'b str>,
        mutated: &mut std::collections::HashSet<String>,
    ) {
        for stmt in &block.stmts {
            self.walk_mutations_in_stmt(&stmt.node, bound, mutated);
        }
    }

    fn walk_mutations_in_stmt<'b>(
        &self,
        stmt: &'b crate::parser::ast::Stmt,
        bound: &mut std::collections::HashSet<&'b str>,
        mutated: &mut std::collections::HashSet<String>,
    ) {
        use crate::parser::ast::{Pattern, Stmt};
        match stmt {
            Stmt::Assign { target, value, .. } => {
                if let Expr::Identifier(name) = &target.node {
                    if !bound.contains(name.as_str()) {
                        mutated.insert(c_mangle::escape_keyword(name));
                    }
                }
                self.walk_mutations(&value.node, bound, mutated);
            }
            Stmt::CompoundAssign { target, value, .. } => {
                if let Expr::Identifier(name) = &target.node {
                    if !bound.contains(name.as_str()) {
                        mutated.insert(c_mangle::escape_keyword(name));
                    }
                }
                self.walk_mutations(&value.node, bound, mutated);
            }
            Stmt::VarDecl { pattern, value, .. } => {
                self.walk_mutations(&value.node, bound, mutated);
                if let Pattern::Binding(name) = &pattern.node {
                    bound.insert(name.as_str());
                }
            }
            Stmt::Expr(expr) => {
                self.walk_mutations(&expr.node, bound, mutated);
            }
            Stmt::Return(Some(expr)) => {
                self.walk_mutations(&expr.node, bound, mutated);
            }
            Stmt::For { iterable, body, pattern, .. } => {
                self.walk_mutations(&iterable.node, bound, mutated);
                let mut for_bound = bound.clone();
                if let Pattern::Binding(name) = &pattern.node {
                    for_bound.insert(name.as_str());
                }
                self.walk_mutations_in_block(body, &mut for_bound, mutated);
            }
            Stmt::While { condition, body, .. } => {
                self.walk_mutations(&condition.node, bound, mutated);
                self.walk_mutations_in_block(body, &mut bound.clone(), mutated);
            }
            Stmt::If { condition, then_body, elif_branches, else_body } => {
                self.walk_mutations(&condition.node, bound, mutated);
                self.walk_mutations_in_block(then_body, &mut bound.clone(), mutated);
                for (cond, body) in elif_branches {
                    self.walk_mutations(&cond.node, bound, mutated);
                    self.walk_mutations_in_block(body, &mut bound.clone(), mutated);
                }
                if let Some(eb) = else_body {
                    self.walk_mutations_in_block(eb, &mut bound.clone(), mutated);
                }
            }
            _ => {}
        }
    }
}
