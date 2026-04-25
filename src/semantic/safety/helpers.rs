use rustc_hash::FxHashSet;

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use crate::semantic::errors::SemanticErrorKind;
use crate::semantic::ids::DefId;
use crate::semantic::scope::DefKind;
use crate::semantic::types::{self as types, ResolvedType};

use super::{BorrowChecker, BorrowOrigin, BorrowCaptureMode, CaptureSet, SharedDerivedInfo, VarState, WithGuardKind, BLOCKING_CALL_NAMES};
use super::type_utils::is_ast_type_ref;
use super::return_borrows::{CapturedRefOriginCollector, CapturedMutationCollector, CaptureSetCollector};

impl<'a> BorrowChecker<'a> {
    pub(super) fn check_stale_condition(&mut self, condition: &Spanned<Expr>) {
        if let Some((info, condition_span)) = self.find_stale_in_condition(condition) {
            self.stale_warnings.push(crate::semantic::errors::SemanticWarning {
                kind: crate::semantic::errors::SemanticWarningKind::StaleSharedCondition {
                    local_name: info.local_name,
                    shared_name: info.shared_name,
                    derivation_span: Some(info.derivation_span),
                    await_span: info.await_span,
                },
                span: condition_span,
            });
        }
    }

    /// Find the first shared-variable taint in an expression (transitive dataflow).
    /// Returns the name of the originating shared variable if any sub-expression
    /// references a shared variable directly OR a local already known to be derived
    /// from a shared variable. This gives us transitive taint propagation:
    ///   shared int x = 0
    ///   int a = x          // a is derived from x (direct)
    ///   int b = a + 1      // b is derived from x (transitive via a)
    ///   int c = some_fn(b) // c is derived from x (transitive via b)
    pub(super) fn find_shared_ref_in_expr_spanned(&self, expr: &Spanned<Expr>) -> Option<String> {
        match &expr.node {
            Expr::Identifier(_) => {
                if let Some(&def_id) = self.resolution_map.get(&expr.span.start) {
                    // Direct: identifier IS a shared variable
                    if self.shared_var_defs.contains_key(&def_id) {
                        let name = self.scopes.get_def(def_id).name.clone();
                        return Some(name);
                    }
                    // Transitive: identifier is derived from a shared variable
                    if let Some(info) = self.shared_derived.get(&def_id) {
                        return Some(info.shared_name.clone());
                    }
                }
                None
            }
            Expr::BinaryOp { left, right, .. }
            | Expr::DefaultOp { lhs: left, rhs: right } => {
                self.find_shared_ref_in_expr_spanned(left)
                    .or_else(|| self.find_shared_ref_in_expr_spanned(right))
            }
            Expr::UnaryOp { operand, .. } => self.find_shared_ref_in_expr_spanned(operand),
            Expr::Call { callee, args, .. } => {
                self.find_shared_ref_in_expr_spanned(callee)
                    .or_else(|| args.iter().find_map(|a| self.find_shared_ref_in_expr_spanned(&a.node.value)))
            }
            Expr::MethodCall { receiver, args, .. } => {
                self.find_shared_ref_in_expr_spanned(receiver)
                    .or_else(|| args.iter().find_map(|a| self.find_shared_ref_in_expr_spanned(&a.node.value)))
            }
            Expr::FieldAccess { object, .. }
            | Expr::TupleFieldAccess { object, .. }
            | Expr::OptionalChain { object, .. } => self.find_shared_ref_in_expr_spanned(object),
            Expr::Index { object, index } => {
                self.find_shared_ref_in_expr_spanned(object)
                    .or_else(|| self.find_shared_ref_in_expr_spanned(index))
            }
            Expr::TupleLiteral(elems)
            | Expr::ArrayLiteral(elems) => {
                elems.iter().find_map(|e| self.find_shared_ref_in_expr_spanned(e))
            }
            Expr::DictLiteral(pairs) => {
                pairs.iter().find_map(|(k, v)| {
                    self.find_shared_ref_in_expr_spanned(k)
                        .or_else(|| self.find_shared_ref_in_expr_spanned(v))
                })
            }
            Expr::StructLiteral { args, .. } => {
                args.iter().find_map(|a| self.find_shared_ref_in_expr_spanned(a))
            }
            Expr::If { condition, then_branch, elif_branches, else_branch } => {
                self.find_shared_ref_in_expr_spanned(condition)
                    .or_else(|| self.find_shared_ref_in_expr_spanned(then_branch))
                    .or_else(|| elif_branches.iter().find_map(|(c, b)| {
                        self.find_shared_ref_in_expr_spanned(c)
                            .or_else(|| self.find_shared_ref_in_expr_spanned(b))
                    }))
                    .or_else(|| else_branch.as_ref().and_then(|b| self.find_shared_ref_in_expr_spanned(b)))
            }
            Expr::Match { scrutinee, arms, else_arm } => {
                self.find_shared_ref_in_expr_spanned(scrutinee)
                    .or_else(|| arms.iter().find_map(|a| self.find_shared_ref_in_expr_spanned(&a.body)))
                    .or_else(|| else_arm.as_ref().and_then(|b| self.find_shared_ref_in_expr_spanned(b)))
            }
            Expr::Range { start, end, .. } => {
                start.as_ref().and_then(|e| self.find_shared_ref_in_expr_spanned(e))
                    .or_else(|| end.as_ref().and_then(|e| self.find_shared_ref_in_expr_spanned(e)))
            }
            Expr::As { expr: inner, .. }
            | Expr::MutableBorrow { expr: inner }
            | Expr::Move { expr: inner }
            | Expr::Deref { expr: inner }
            | Expr::Await { expr: inner }
            | Expr::Spawn { expr: inner, .. }
            | Expr::SpawnBlocking { expr: inner, .. }
            | Expr::Is { expr: inner, .. }
            | Expr::ImplicitClosure { body: inner } => self.find_shared_ref_in_expr_spanned(inner),
            Expr::ListComprehension { expr, iterable, condition, .. } => {
                self.find_shared_ref_in_expr_spanned(iterable)
                    .or_else(|| self.find_shared_ref_in_expr_spanned(expr))
                    .or_else(|| condition.as_ref().and_then(|c| self.find_shared_ref_in_expr_spanned(c)))
            }
            // Closures create a new scope — taint doesn't escape them
            Expr::Closure { .. } => None,
            _ => None,
        }
    }

    /// Find all `with`-tracked shared variables referenced in a condition expression.
    pub(super) fn find_with_tracked_in_condition(&self, expr: &Spanned<Expr>) -> Vec<String> {
        match &expr.node {
            Expr::Identifier(name) => {
                if let Some(&def_id) = self.resolution_map.get(&expr.span.start) {
                    if self.with_shared_tracked.contains(&def_id) {
                        return vec![name.clone()];
                    }
                }
                vec![]
            }
            Expr::BinaryOp { left, right, .. }
            | Expr::DefaultOp { lhs: left, rhs: right } => {
                let mut names = self.find_with_tracked_in_condition(left);
                names.extend(self.find_with_tracked_in_condition(right));
                names
            }
            Expr::UnaryOp { operand, .. } => self.find_with_tracked_in_condition(operand),
            Expr::Call { callee, args, .. } => {
                let mut names = self.find_with_tracked_in_condition(callee);
                for a in args {
                    names.extend(self.find_with_tracked_in_condition(&a.node.value));
                }
                names
            }
            Expr::MethodCall { receiver, args, .. } => {
                let mut names = self.find_with_tracked_in_condition(receiver);
                for a in args {
                    names.extend(self.find_with_tracked_in_condition(&a.node.value));
                }
                names
            }
            Expr::FieldAccess { object, .. }
            | Expr::TupleFieldAccess { object, .. }
            | Expr::OptionalChain { object, .. } => self.find_with_tracked_in_condition(object),
            Expr::Index { object, index } => {
                let mut names = self.find_with_tracked_in_condition(object);
                names.extend(self.find_with_tracked_in_condition(index));
                names
            }
            Expr::As { expr: inner, .. }
            | Expr::MutableBorrow { expr: inner }
            | Expr::Move { expr: inner }
            | Expr::Deref { expr: inner }
            | Expr::Is { expr: inner, .. } => self.find_with_tracked_in_condition(inner),
            Expr::TupleLiteral(elems)
            | Expr::ArrayLiteral(elems) => {
                elems.iter().flat_map(|e| self.find_with_tracked_in_condition(e)).collect()
            }
            _ => vec![],
        }
    }

    /// Emit a check-then-act or iterator-invalidation warning if we are inside
    /// a branch/loop guarded by a `with`-tracked shared variable.
    pub(super) fn check_with_check_then_act(&mut self, yield_span: Span) {
        if let Some((shared_names, guard_span, kind)) = self.with_guarded_conditions.last().cloned() {
            let warning_kind = match kind {
                WithGuardKind::BranchCondition => {
                    crate::semantic::errors::SemanticWarningKind::WithCheckThenAct {
                        shared_names,
                        condition_span: guard_span,
                        yield_span,
                    }
                }
                WithGuardKind::Iteration => {
                    crate::semantic::errors::SemanticWarningKind::SharedIteratorInvalidation {
                        shared_name: shared_names.into_iter().next().unwrap_or_default(),
                        iterable_span: guard_span,
                        yield_span,
                    }
                }
            };
            self.stale_warnings.push(crate::semantic::errors::SemanticWarning {
                kind: warning_kind,
                span: yield_span,
            });
        }
    }

    /// Determine whether a callee expression represents a yield point (a call
    /// that may suspend or release the shared-variable token).
    ///
    /// Uses purity inference: Pure/ReadOnly/MutatesArgs calls are proven safe.
    /// HasSideEffects user-defined functions are treated as yield points.
    /// Unknown/extern functions fall back to the hardcoded BLOCKING_CALL_NAMES list
    /// for backwards compatibility (builtins like `print` are not yield points).
    pub(super) fn is_yield_point_call(&self, callee: &Expr) -> bool {
        if let Expr::Identifier(name) = callee {
            // Backwards compat: hardcoded blocking calls are always yield points
            if BLOCKING_CALL_NAMES.contains(&name.as_str()) {
                return true;
            }
            // Check purity: Pure/ReadOnly/MutatesArgs → definitely not a yield point
            // HasSideEffects user-defined function → yield point
            if let Some(purity) = self.fn_purity.get(name.as_str()) {
                return matches!(purity, crate::semantic::purity::Purity::HasSideEffects);
            }
            // Not in purity map (extern/builtin) and not in blocking list → not a yield point
            return false;
        }
        // Non-identifier callee (e.g. closure call) → not a yield point
        false
    }

    /// Check whether an expression tree contains a yield point (await or
    /// blocking/side-effecting call). Purity-aware: pure/read-only calls
    /// are never treated as yield points.
    pub(super) fn expr_contains_yield_point(&self, expr: &Spanned<Expr>) -> bool {
        match &expr.node {
            Expr::Await { .. } => true,
            Expr::Call { callee, args, .. } => {
                if self.is_yield_point_call(&callee.node) {
                    return true;
                }
                self.expr_contains_yield_point(callee)
                    || args.iter().any(|a| self.expr_contains_yield_point(&a.node.value))
            }
            Expr::MethodCall { receiver, args, .. } => {
                self.expr_contains_yield_point(receiver)
                    || args.iter().any(|a| self.expr_contains_yield_point(&a.node.value))
            }
            Expr::BinaryOp { left, right, .. }
            | Expr::DefaultOp { lhs: left, rhs: right } => {
                self.expr_contains_yield_point(left) || self.expr_contains_yield_point(right)
            }
            Expr::UnaryOp { operand, .. }
            | Expr::As { expr: operand, .. }
            | Expr::MutableBorrow { expr: operand }
            | Expr::Move { expr: operand }
            | Expr::Deref { expr: operand }
            | Expr::Is { expr: operand, .. } => self.expr_contains_yield_point(operand),
            Expr::FieldAccess { object, .. }
            | Expr::TupleFieldAccess { object, .. }
            | Expr::OptionalChain { object, .. } => self.expr_contains_yield_point(object),
            Expr::Index { object, index } => {
                self.expr_contains_yield_point(object) || self.expr_contains_yield_point(index)
            }
            Expr::TupleLiteral(elems) | Expr::ArrayLiteral(elems) => {
                elems.iter().any(|e| self.expr_contains_yield_point(e))
            }
            Expr::If { condition, then_branch, else_branch, elif_branches, .. } => {
                self.expr_contains_yield_point(condition)
                    || self.expr_contains_yield_point(then_branch)
                    || elif_branches.iter().any(|(c, b)| self.expr_contains_yield_point(c) || self.expr_contains_yield_point(b))
                    || else_branch.as_ref().map_or(false, |e| self.expr_contains_yield_point(e))
            }
            // Closures execute separately — don't recurse
            Expr::Closure { .. } => false,
            _ => false,
        }
    }

    /// Check a condition expression for uses of stale-shared-derived locals.
    /// Returns the stale info and the span where the stale local is used in the condition.
    pub(super) fn find_stale_in_condition(&self, expr: &Spanned<Expr>) -> Option<(SharedDerivedInfo, Span)> {
        match &expr.node {
            Expr::Identifier(_) => {
                if let Some(&def_id) = self.resolution_map.get(&expr.span.start) {
                    if let Some(info) = self.stale_shared_derived.get(&def_id) {
                        return Some((info.clone(), expr.span));
                    }
                }
                None
            }
            Expr::BinaryOp { left, right, .. }
            | Expr::DefaultOp { lhs: left, rhs: right } => {
                self.find_stale_in_condition(left)
                    .or_else(|| self.find_stale_in_condition(right))
            }
            Expr::UnaryOp { operand, .. } => self.find_stale_in_condition(operand),
            Expr::Call { callee, args, .. } => {
                self.find_stale_in_condition(callee)
                    .or_else(|| args.iter().find_map(|a| self.find_stale_in_condition(&a.node.value)))
            }
            Expr::MethodCall { receiver, args, .. } => {
                self.find_stale_in_condition(receiver)
                    .or_else(|| args.iter().find_map(|a| self.find_stale_in_condition(&a.node.value)))
            }
            Expr::FieldAccess { object, .. }
            | Expr::TupleFieldAccess { object, .. }
            | Expr::OptionalChain { object, .. } => self.find_stale_in_condition(object),
            Expr::Index { object, index } => {
                self.find_stale_in_condition(object)
                    .or_else(|| self.find_stale_in_condition(index))
            }
            Expr::As { expr: inner, .. }
            | Expr::MutableBorrow { expr: inner }
            | Expr::Move { expr: inner }
            | Expr::Deref { expr: inner }
            | Expr::Is { expr: inner, .. } => self.find_stale_in_condition(inner),
            Expr::TupleLiteral(elems)
            | Expr::ArrayLiteral(elems) => {
                elems.iter().find_map(|e| self.find_stale_in_condition(e))
            }
            _ => None,
        }
    }

    /// Check if a DefId refers to a ConsumeCallable-typed variable.
    pub(super) fn is_consume_callable_var(&self, def_id: DefId) -> bool {
        let def = self.scopes.get_def(def_id);
        if def.kind != DefKind::Variable {
            return false;
        }
        if let Some(type_id) = def.type_id {
            match self.types.get(type_id) {
                ResolvedType::ConsumeCallableTrait(_) => true,
                ResolvedType::BoxedCallable { kind, .. }
                    if *kind == super::types::ClosureKind::ConsumeCallable =>
                {
                    true
                }
                _ => false,
            }
        } else {
            false
        }
    }

    // ─── Expression Walking ────────────────────────────────

    // ─── Call/Lookup Helpers ─────────────────────────────────

    pub(super) fn find_def_by_name(&self, name: &str) -> Option<DefId> {
        if let Some(scope_id) = self.current_fn_scope {
            self.scopes.lookup_within_function(scope_id, name)
        } else {
            self.scopes.lookup(name)
        }
    }

    /// Resolve a Call callee expression to its DefId (if it's a simple identifier).
    pub(super) fn resolve_callee_def_id(&self, callee: &Spanned<Expr>) -> Option<DefId> {
        match &callee.node {
            Expr::Identifier(_) => self.resolution_map.get(&callee.span.start).copied(),
            Expr::Path { segments } => {
                segments.first().and_then(|s| self.resolution_map.get(&s.span.start).copied())
            }
            _ => None,
        }
    }

    /// Walk up FieldAccess/TupleFieldAccess/Index/OptionalChain chains to find the root identifier's DefId.
    pub(super) fn find_root_def_id(&self, expr: &Spanned<Expr>) -> Option<DefId> {
        match &expr.node {
            Expr::Identifier(_) => self.resolution_map.get(&expr.span.start).copied(),
            Expr::FieldAccess { object, .. }
            | Expr::TupleFieldAccess { object, .. }
            | Expr::Index { object, .. }
            | Expr::OptionalChain { object, .. } => self.find_root_def_id(object),
            Expr::SelfExpr => self.resolution_map.get(&expr.span.start).copied(),
            _ => None,
        }
    }

    /// Check if an expression is an index/get access on a collection.
    /// Returns the collection's root DefId if the expression is:
    /// - `vec[i]` (Index expression)
    /// - `vec.get(i)` (method call on collection)
    /// - `vec.get(i).unwrap()` (method chain through unwrap/expect)
    /// - `vec.first()`, `vec.last()` (element access methods)
    pub(super) fn find_collection_source(&self, expr: &Spanned<Expr>) -> Option<DefId> {
        match &expr.node {
            // vec[i]
            Expr::Index { object, .. } => self.find_root_def_id(object),
            // vec.get(i).unwrap() — unwrap/expect on a .get() result
            Expr::MethodCall { receiver, method, .. }
                if matches!(method.node.as_str(), "unwrap" | "expect") =>
            {
                self.find_collection_source(receiver)
            }
            // vec.get(i), vec.first(), vec.last()
            Expr::MethodCall { receiver, method, .. }
                if matches!(method.node.as_str(), "get" | "first" | "last") =>
            {
                self.find_root_def_id(receiver)
            }
            _ => None,
        }
    }

    /// If `expr`'s root is a `&` (MutableBorrow) parameter, mark it as mutated.
    pub(super) fn mark_mut_param_if_applicable(&mut self, expr: &Spanned<Expr>) {
        if let Some(def_id) = self.find_root_def_id(expr) {
            let def = self.scopes.get_def(def_id);
            if def.is_param && def.param_ownership == Some(Ownership::MutableBorrow) {
                self.mut_param_mutated.insert(def_id);
            }
        }
    }

    /// Enforce `MutRef[T]` exclusivity at struct construction. When a new
    /// borrow-field struct is being built that takes `src_def_id` as a
    /// `MutRef[T]` field arg, no other live borrow-field struct may already
    /// borrow from `src_def_id` (shared OR exclusive). Mirrors Rust's "one
    /// `&mut T` xor many `&T`" invariant, applied to user `MutRef[T]` fields.
    pub(super) fn check_mut_ref_exclusive(&mut self, src_def_id: DefId, span: Span) {
        let src_name = self.scopes.get_def(src_def_id).name.clone();
        let mut existing: Option<String> = None;
        for (&var_id, origin) in self.var_origins.iter() {
            if !origin.references_def(src_def_id) { continue; }
            let def = self.scopes.get_def(var_id);
            let is_borrow_field_struct = match def.type_id {
                Some(tid) => match self.types.get(tid) {
                    crate::semantic::types::ResolvedType::Defined(struct_def_id)
                    | crate::semantic::types::ResolvedType::Generic(struct_def_id, _) =>
                        self.ref_type_structs.contains(struct_def_id),
                    _ => false,
                },
                None => false,
            };
            if !is_borrow_field_struct { continue; }
            let is_alive = !matches!(
                self.var_states.get(&var_id),
                Some(VarState::Moved { .. })
            );
            if is_alive {
                existing = Some(self.scopes.get_def(var_id).name.clone());
                break;
            }
        }
        if let Some(borrow_name) = existing {
            // Reuse MutationWhileBorrowed phrasing — semantically: the new
            // exclusive borrow conflicts with the existing one (much like a
            // mutation would). A dedicated error variant could be cleaner
            // but reusing keeps the user-facing surface small for now.
            self.error(
                SemanticErrorKind::MutationWhileBorrowed {
                    source: src_name,
                    borrow: borrow_name,
                },
                span,
            );
        }
    }

    /// Reject mutation of `src_def_id` while any live local is a borrow-field
    /// struct whose origin references `src_def_id`. Mirrors the in-line check
    /// in `check_expr` for builtin mutating methods, but applied at the call
    /// site of `f(&v)` (function call with mutable-borrow arg) where the
    /// existing checker has no mutation-while-borrowed coverage. Sigil
    /// `T &` borrows of `v` are NOT flagged here — they have their own rules
    /// elsewhere; we only want to plug the gap for new borrow-field structs.
    pub(super) fn check_borrow_field_mutation(&mut self, src_def_id: DefId, span: Span) {
        let src_name = self.scopes.get_def(src_def_id).name.clone();
        let mut to_flag: Option<String> = None;
        for (&var_id, origin) in self.var_origins.iter() {
            if !origin.references_def(src_def_id) { continue; }
            let def = self.scopes.get_def(var_id);
            let is_borrow_field_struct = match def.type_id {
                Some(tid) => match self.types.get(tid) {
                    crate::semantic::types::ResolvedType::Defined(struct_def_id)
                    | crate::semantic::types::ResolvedType::Generic(struct_def_id, _) =>
                        self.ref_type_structs.contains(struct_def_id),
                    _ => false,
                },
                None => false,
            };
            if !is_borrow_field_struct { continue; }
            let is_alive = !matches!(
                self.var_states.get(&var_id),
                Some(VarState::Moved { .. })
            );
            if is_alive {
                to_flag = Some(self.scopes.get_def(var_id).name.clone());
                break;
            }
        }
        if let Some(borrow_name) = to_flag {
            self.error(
                SemanticErrorKind::MutationWhileBorrowed {
                    source: src_name,
                    borrow: borrow_name,
                },
                span,
            );
        }
    }

    /// Check that call-site ownership annotations match the parameter declarations.
    pub(super) fn check_call_ownership(
        &mut self,
        callee: &Spanned<Expr>,
        args: &[Spanned<CallArg>],
    ) {
        let def_id = match self.resolve_callee_def_id(callee) {
            Some(id) => id,
            None => return,
        };

        // Skip constructors (structs, enum variants) — they don't have FunctionInfo
        let kind = self.scopes.get_def(def_id).kind;
        if matches!(kind, DefKind::Struct | DefKind::Variant) {
            return;
        }

        let info = match self.function_info.get(&def_id) {
            Some(info) => info,
            None => return, // builtins, extern, etc.
        };

        for (i, arg) in args.iter().enumerate() {
            if i >= info.param_ownerships.len() {
                break; // varargs or mismatched count (caught by type checker)
            }

            let expected = info.param_ownerships[i];
            let found = arg.node.ownership;

            if expected != found {
                let param_name = info.param_names[i].clone();
                let expected_str = match expected {
                    Ownership::Borrow => "borrow (bare)",
                    Ownership::MutableBorrow => "mutable borrow (&)",
                    Ownership::Move => "consume (!)",
                };
                let found_str = match found {
                    Ownership::Borrow => "borrow (bare)",
                    Ownership::MutableBorrow => "mutable borrow (&)",
                    Ownership::Move => "consume (!)",
                };
                self.error(
                    SemanticErrorKind::OwnershipMismatch {
                        param_name,
                        expected: expected_str.to_string(),
                        found: found_str.to_string(),
                    },
                    arg.span,
                );
            }
        }
    }

    /// Detect aliasing conflicts within a single call's arguments.
    /// e.g., f(&x, &x) — double mutable borrow
    /// e.g., f(x, &x) — immutable read + mutable borrow
    /// e.g., f(&x, !x) — mutable borrow + move
    pub(super) fn check_call_aliasing(&mut self, args: &[Spanned<CallArg>]) {
        // Collect (DefId, Ownership, span) for identifier arguments
        let mut arg_vars: Vec<(DefId, Ownership, Span)> = Vec::new();

        for arg in args {
            let (inner_expr, ownership) = match arg.node.ownership {
                Ownership::Move => (&arg.node.value, Ownership::Move),
                Ownership::MutableBorrow => (&arg.node.value, Ownership::MutableBorrow),
                Ownership::Borrow => (&arg.node.value, Ownership::Borrow),
            };

            if let Expr::Identifier(_) = &inner_expr.node {
                if let Some(&def_id) = self.resolution_map.get(&inner_expr.span.start) {
                    let kind = self.scopes.get_def(def_id).kind;
                    if kind == DefKind::Variable {
                        arg_vars.push((def_id, ownership, arg.span));
                    }
                }
            }
        }

        // Check pairs for conflicts
        for i in 0..arg_vars.len() {
            for j in (i + 1)..arg_vars.len() {
                let (id_a, own_a, span_a) = &arg_vars[i];
                let (id_b, own_b, _span_b) = &arg_vars[j];

                if id_a != id_b {
                    continue;
                }

                let name = self.scopes.get_def(*id_a).name.clone();

                let conflict = match (own_a, own_b) {
                    // Double mutable borrow
                    (Ownership::MutableBorrow, Ownership::MutableBorrow) => {
                        Some("cannot borrow mutably more than once in the same call")
                    }
                    // Mutable borrow + move (either order)
                    (Ownership::MutableBorrow, Ownership::Move)
                    | (Ownership::Move, Ownership::MutableBorrow) => {
                        Some("cannot borrow and move the same variable in a call")
                    }
                    // Borrow (bare read) + mutable borrow
                    (Ownership::Borrow, Ownership::MutableBorrow)
                    | (Ownership::MutableBorrow, Ownership::Borrow) => {
                        Some("cannot use bare and mutable borrow of the same variable in a call")
                    }
                    _ => None,
                };

                if let Some(detail) = conflict {
                    self.error(
                        SemanticErrorKind::BorrowConflict {
                            name,
                            detail: detail.to_string(),
                        },
                        *span_a,
                    );
                }
            }
        }
    }

    /// Collect origins of captured reference-type variables in a closure body.
    /// Uses the ExprVisitor trait for exhaustive variant coverage.
    pub(super) fn collect_captured_ref_origins(
        &self,
        expr: &Spanned<Expr>,
        param_names: &FxHashSet<&str>,
    ) -> Vec<BorrowOrigin> {
        use crate::parser::visitor::ExprVisitor;
        let mut collector = CapturedRefOriginCollector {
            resolution_map: self.resolution_map,
            scopes: self.scopes,
            types: self.types,
            ref_type_structs: &self.ref_type_structs,
            var_origins: &self.var_origins,
            param_names,
            origins: Vec::new(),
        };
        collector.visit_expr(expr);
        collector.origins
    }

    /// Compute the full capture set for a closure with the given params and body.
    /// Identifies all free variables, classifies them as Read/Mutable, and checks
    /// whether each has a borrowed origin.
    /// Check spawn call arguments for borrowed origins.
    /// Spawned tasks may outlive the caller, so any non-Static reference arg is unsound.
    /// Perform CFA at a spawn site: for each shared binding arg, determine the sync strategy
    /// based on whether the callee's corresponding parameter uses mutable borrow.
    pub(super) fn cfa_at_spawn(&mut self, callee_def_id: Option<DefId>, args: &[Spanned<CallArg>]) {
        for (i, arg) in args.iter().enumerate() {
            if let Expr::Identifier(_) = &arg.node.value.node {
                if let Some(&def_id) = self.resolution_map.get(&arg.node.value.span.start) {
                    if let Some((shared_kind, _, _)) = self.shared_var_defs.get(&def_id) {
                        let shared_kind = *shared_kind;
                        self.shared_spawned.insert(def_id);
                        // Only auto-decide for SharedKind::Auto; overrides already set
                        if shared_kind == crate::parser::ast::SharedKind::Auto {
                            let fi = callee_def_id
                                .and_then(|cid| self.function_info.get(&cid));
                            let is_mutable = fi
                                .map_or(false, |fi| {
                                    fi.param_ownerships.get(i).map_or(false, |o| {
                                        matches!(o, Ownership::MutableBorrow)
                                    })
                                });
                            // Also check callee param type for explicit wrapper types
                            let param_type_name = fi
                                .and_then(|fi| fi.param_type_ids.get(i).and_then(|t| *t))
                                .map(|tid| {
                                    match self.types.get(tid) {
                                        crate::semantic::types::ResolvedType::Generic(did, _) => self.scopes.get_def(*did).name.clone(),
                                        crate::semantic::types::ResolvedType::Defined(did) => self.scopes.get_def(*did).name.clone(),
                                        _ => String::new(),
                                    }
                                })
                                .unwrap_or_default();
                            let strategy = if is_mutable || param_type_name == "Mutex" {
                                crate::semantic::SharedStrategy::ArcMutex
                            } else if param_type_name == "AtomicInt" || param_type_name == "AtomicBool" {
                                crate::semantic::SharedStrategy::ArcAtomic
                            } else if param_type_name == "RWLock" {
                                crate::semantic::SharedStrategy::ArcRwLock
                            } else {
                                crate::semantic::SharedStrategy::ArcOnly
                            };
                            // Upgrade: ArcOnly → ArcMutex if already ArcOnly and now mutable
                            let entry = self.shared_out.entry(def_id).or_insert(strategy);
                            if is_mutable && *entry == crate::semantic::SharedStrategy::ArcOnly {
                                *entry = crate::semantic::SharedStrategy::ArcMutex;
                            }
                        }
                    }
                }
            }
        }
    }

    pub(super) fn check_spawn_args(&mut self, args: &[Spanned<CallArg>]) {
        for arg in args {
            let (is_borrowed, var_name) = if let Expr::Identifier(name) = &arg.node.value.node {
                if let Some(&def_id) = self.resolution_map.get(&arg.node.value.span.start) {
                    // Skip error for shared bindings — they're explicitly safe to cross boundaries
                    if self.shared_var_defs.contains_key(&def_id) {
                        (false, None)
                    } else {
                        // Check the variable's type — value types and callables are safe to pass to spawn
                        let type_safe = self.scopes.get_def(def_id).type_id
                            .map_or(false, |tid| {
                                types::is_callable_type(tid, self.types)
                                || !types::is_reference_type(tid, self.types, &self.ref_type_structs)
                            });
                        if type_safe {
                            (false, None)
                        } else {
                            let borrowed = self.var_origins
                                .get(&def_id)
                                .map_or(false, |o| !matches!(o, BorrowOrigin::Static));
                            (borrowed, Some(name.clone()))
                        }
                    }
                } else {
                    (false, None)
                }
            } else {
                // Value types (int, float, bool, etc.) are always safe to pass to spawned tasks
                if let Some(&tid) = self.expr_types.get(&arg.node.value.span) {
                    if !types::is_reference_type(tid, self.types, &self.ref_type_structs)
                        && !types::is_callable_type(tid, self.types)
                    {
                        continue;
                    }
                } else if matches!(arg.node.value.node, Expr::FieldAccess { .. } | Expr::TupleFieldAccess { .. }) {
                    // Field access without type info — can't determine if the field is a
                    // reference type. Skip conservatively to avoid false positives from
                    // value-type field accesses (int, float, etc.) on params.
                    continue;
                }
                (!matches!(self.compute_expr_origin(&arg.node.value), BorrowOrigin::Static), None)
            };
            if is_borrowed {
                self.error(SemanticErrorKind::SpawnWithBorrowedRef { name: var_name }, arg.span);
            }
        }
    }

    pub(super) fn check_spawn_closure_captures(&mut self, cs: &CaptureSet, span: Span) {
        for entry in &cs.captures {
            if self.shared_var_defs.contains_key(&entry.def_id) {
                self.error(SemanticErrorKind::SpawnClosureCaptureShared {
                    var_name: entry.name.clone(),
                }, span);
            }
            if entry.has_borrowed_origin {
                self.error(SemanticErrorKind::SpawnClosureCaptureBorrowed {
                    var_name: entry.name.clone(),
                }, span);
            }
            if entry.mode == BorrowCaptureMode::Mutable {
                self.error(SemanticErrorKind::SpawnClosureCaptureMutable {
                    var_name: entry.name.clone(),
                }, span);
            }
        }
    }

    pub(super) fn compute_capture_set(
        &self,
        params: &[Spanned<ClosureParam>],
        body: &Spanned<Expr>,
    ) -> CaptureSet {
        use crate::parser::visitor::ExprVisitor;

        let param_names: FxHashSet<&str> = params.iter()
            .map(|p| p.node.name.node.as_str()).collect();

        // Phase 1: detect mutations inside the closure body
        let mut mutation_collector = CapturedMutationCollector {
            locals: FxHashSet::default(),
            mutated: FxHashSet::default(),
            method_resolutions: self.method_resolutions,
            function_info: self.function_info,
        };
        mutation_collector.visit_expr(body);

        // Phase 2: collect all captured variables
        let mut collector = CaptureSetCollector {
            resolution_map: self.resolution_map,
            scopes: self.scopes,
            types: self.types,
            ref_type_structs: &self.ref_type_structs,
            var_origins: &self.var_origins,
            param_names: &param_names,
            local_names: FxHashSet::default(),
            seen: FxHashSet::default(),
            captures: Vec::new(),
            mutated_names: &mutation_collector.mutated,
        };
        collector.visit_expr(body);

        CaptureSet { captures: collector.captures }
    }

    /// Check if a return expression contains closures that capture local variables.
    /// Walks through struct literals, arrays, and identifiers to find closures whose
    /// capture sets contain non-param locals (would dangle after return).
    pub(super) fn check_return_for_escaping_closures(&mut self, expr: &Spanned<Expr>) {
        self.check_expr_for_escaping_closures(expr);
    }

    /// Recursive helper: walk an expression looking for escaping closures.
    /// Returns true if an error was emitted (to stop after first error).
    fn check_expr_for_escaping_closures(&mut self, expr: &Spanned<Expr>) -> bool {
        match &expr.node {
            Expr::Identifier(_) => {
                if let Some(&def_id) = self.resolution_map.get(&expr.span.start) {
                    // Direct closure variable
                    if let Some(cs) = self.closure_capture_sets.get(&def_id).cloned() {
                        for entry in &cs.captures {
                            let cap_def = self.scopes.get_def(entry.def_id);
                            if cap_def.kind == DefKind::Variable && !cap_def.is_param {
                                self.error(
                                    SemanticErrorKind::ClosureEscapesScope {
                                        closure_name: self.scopes.get_def(def_id).name.clone(),
                                        captured_name: entry.name.clone(),
                                    },
                                    expr.span,
                                );
                                return true;
                            }
                        }
                    }
                    // Struct/array variable known to contain an escaping closure
                    if self.vars_containing_closures.contains(&def_id) {
                        let name = self.scopes.get_def(def_id).name.clone();
                        self.error(
                            SemanticErrorKind::ClosureEscapesScope {
                                closure_name: name.clone(),
                                captured_name: "<local>".to_string(),
                            },
                            expr.span,
                        );
                        return true;
                    }
                }
                false
            }
            Expr::StructLiteral { args, .. } => {
                for arg in args {
                    if self.check_expr_for_escaping_closures(arg) {
                        return true;
                    }
                }
                false
            }
            Expr::ArrayLiteral(elems) => {
                for elem in elems {
                    if self.check_expr_for_escaping_closures(elem) {
                        return true;
                    }
                }
                false
            }
            _ => false,
        }
    }

    /// Check if a struct/array literal value contains closures that capture locals.
    /// If so, mark the variable as tainted for escape checking.
    pub(super) fn mark_var_if_contains_closures(&mut self, var_def_id: DefId, value: &Spanned<Expr>) {
        if self.expr_contains_escaping_closure(value) {
            self.vars_containing_closures.insert(var_def_id);
        }
    }

    /// Check if an expression contains a closure (identifier) that captures local variables.
    fn expr_contains_escaping_closure(&self, expr: &Spanned<Expr>) -> bool {
        match &expr.node {
            Expr::Identifier(_) => {
                if let Some(&def_id) = self.resolution_map.get(&expr.span.start) {
                    if let Some(cs) = self.closure_capture_sets.get(&def_id) {
                        return cs.captures.iter().any(|entry| {
                            let cap_def = self.scopes.get_def(entry.def_id);
                            cap_def.kind == DefKind::Variable && !cap_def.is_param
                        });
                    }
                    if self.vars_containing_closures.contains(&def_id) {
                        return true;
                    }
                }
                false
            }
            Expr::StructLiteral { args, .. } => {
                args.iter().any(|arg| self.expr_contains_escaping_closure(arg))
            }
            Expr::ArrayLiteral(elems) => {
                elems.iter().any(|elem| self.expr_contains_escaping_closure(elem))
            }
            _ => false,
        }
    }

    /// Check if a function returns a temporary (non-reference owning type) with
    /// no `return_borrows_from`. Used by both Call and MethodCall detection.
    pub(super) fn is_temporary_from_function(&self, def_id: DefId) -> bool {
        let Some(info) = self.function_info.get(&def_id) else { return false };
        // Only check user-defined functions with bodies (not declarations/externs)
        if !info.has_body {
            return false;
        }
        let Some(ret_type_id) = info.return_type_id else { return false };
        // Skip generic functions — return type might be a type param that resolves
        // to a reference type at instantiation site
        if !info.generic_param_names.is_empty() {
            return false;
        }
        // Owning return type + no return_borrows_from = temporary
        !types::is_reference_type(ret_type_id, self.types, &self.ref_type_structs)
            && info.return_borrows_from.is_empty()
    }

    /// Check if a call expression will be auto-propagated: the function returns
    /// `Result[T, E]` and we're in a propagation context (throws or returns Result).
    /// In this case the Ok value is moved out of the Result — not borrowed from it —
    /// so `TemporaryBorrow` should not fire.
    pub(super) fn is_auto_propagated_call(&self, value: &Spanned<Expr>) -> bool {
        let def_id = match &value.node {
            Expr::Call { callee, .. } => self.resolve_callee_def_id(callee),
            Expr::MethodCall { method, .. } => self.method_resolutions.get(&method.span.start).copied(),
            _ => None,
        };
        let Some(def_id) = def_id else { return false };
        let Some(info) = self.function_info.get(&def_id) else { return false };
        // Check if the function returns a Result type
        let Some(ret_type_id) = info.return_type_id else { return false };
        let resolved = self.types.get(ret_type_id);
        let is_result = if let ResolvedType::Generic(gdef, args) = resolved {
            self.scopes.get_def(*gdef).name == "Result" && args.len() == 2
        } else {
            false
        };
        if !is_result && !info.throws {
            return false;
        }
        // Check if current function is a propagation context
        self.current_function_throws || self.current_function_returns_result()
    }

    /// Check if the current function returns a Result type.
    pub(super) fn current_function_returns_result(&self) -> bool {
        let Some(ret_type) = self.current_return_type_id else { return false };
        let resolved = self.types.get(ret_type);
        if let ResolvedType::Generic(def_id, args) = resolved {
            self.scopes.get_def(*def_id).name == "Result" && args.len() == 2
        } else {
            false
        }
    }

    /// Check if a call/method-call expression returns a temporary.
    /// Binding a reference type to such a value is an error
    /// because the temporary will be dropped immediately.
    pub(super) fn is_temporary_borrow(&self, value: &Spanned<Expr>) -> bool {
        match &value.node {
            Expr::Call { callee, .. } => {
                let Some(def_id) = self.resolve_callee_def_id(callee) else { return false };
                self.is_temporary_from_function(def_id)
            }
            Expr::MethodCall { method, .. } => {
                let Some(&def_id) = self.method_resolutions.get(&method.span.start) else { return false };
                self.is_temporary_from_function(def_id)
            }
            _ => false,
        }
    }

    /// Extract the callee name from a Call expression (for error messages).
    pub(super) fn extract_callee_name(callee: &Spanned<Expr>) -> String {
        match &callee.node {
            Expr::Identifier(name) => name.clone(),
            Expr::Path { segments } => {
                segments.iter().map(|s| s.node.as_str()).collect::<Vec<_>>().join(".")
            }
            _ => "<expression>".to_string(),
        }
    }

    /// Extract a readable name from a method receiver expression (for error messages).
    pub(super) fn extract_receiver_name(receiver: &Spanned<Expr>) -> String {
        match &receiver.node {
            Expr::Identifier(name) => name.clone(),
            Expr::FieldAccess { object, field } => {
                format!("{}.{}", Self::extract_receiver_name(object), field.node)
            }
            _ => "<expr>".to_string(),
        }
    }

    /// Check if a DefId refers to any allocator type (Arena, TrackingAllocator, PoolAllocator, TlsfAllocator, FixedBufferAllocator, or FallbackAllocator).
    pub(super) fn is_allocator_type(&self, def_id: DefId) -> bool {
        self.scopes.get_def(def_id).type_id.map_or(false, |tid| {
            matches!(self.types.get(tid), ResolvedType::Defined(d)
                if matches!(self.scopes.get_def(*d).name.as_str(), "Arena" | "TrackingAllocator" | "PoolAllocator" | "TlsfAllocator" | "FixedBufferAllocator" | "FallbackAllocator"))
        })
    }

    /// Check if a variable is a reference type, using its type annotation or resolved type.
    pub(super) fn is_var_reference_type(&self, def_id: DefId, type_annotation: Option<&Spanned<Type>>) -> bool {
        // Try the type annotation first (works even before type checking)
        if let Some(ann) = type_annotation {
            return is_ast_type_ref(&ann.node, self.scopes, &self.ref_type_structs);
        }
        // Fall back to resolved type
        let def = self.scopes.get_def(def_id);
        if let Some(type_id) = def.type_id {
            return types::is_reference_type(type_id, self.types, &self.ref_type_structs);
        }
        false
    }
}
