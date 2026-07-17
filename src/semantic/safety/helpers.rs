use rustc_hash::FxHashSet;

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use crate::semantic::errors::{MoveReason, MoveShape, SemanticErrorKind};
use crate::semantic::ids::DefId;
use crate::semantic::scope::DefKind;
use crate::semantic::types::{self as types, ResolvedType};

use super::{BorrowChecker, BorrowOrigin, BorrowCaptureMode, CaptureSet, EscapeCtx, SharedDerivedInfo, VarState, WithGuardKind, BLOCKING_CALL_NAMES};
use super::type_utils::is_ast_type_ref;
use super::return_borrows::{CapturedRefOriginCollector, CapturedMutationCollector, CaptureSetCollector};

/// D4/D12: whether an expression is a PLACE (identifier / self / field /
/// non-range index chain). Fresh temps (ctor / call / method results,
/// literals, slices) are not places — they move, never copy. Mirrors ggdef's
/// `ast_is_place` (spec/ggdef/src/elaborate/mod.rs:2207-2219).
pub(super) fn expr_is_place(e: &Expr) -> bool {
    match e {
        Expr::Identifier(_) | Expr::SelfExpr => true,
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            expr_is_place(&object.node)
        }
        // `x[i]` is a place; `x[a..b]` (slice) is a fresh value.
        Expr::Index { object, index } => {
            !matches!(index.node, Expr::Range { .. }) && expr_is_place(&object.node)
        }
        _ => false,
    }
}

/// D12: the PLACE SHAPE of a place expr — a WHOLE identifier/self, or a
/// FIELD/INDEX sub-place. Decides the valid remedy in the
/// `E_MoveWithoutOperator` message (pin-4): a whole place accepts `!x`/`.clone()`,
/// a sub-place accepts `.clone()` only (a bare `!obj.f` is a partial move). A
/// pure function of the expr — call it on an expr already known to be a place
/// (`expr_is_place`); a non-place falls to `FieldIndex` (never reached for the
/// tainted-place sites, which gate on `expr_is_place` first).
pub(super) fn place_shape(e: &Expr) -> MoveShape {
    match e {
        Expr::Identifier(_) | Expr::SelfExpr => MoveShape::Whole,
        _ => MoveShape::FieldIndex,
    }
}

/// D10(b): two projection paths under the SAME root OVERLAP iff one is a prefix
/// of the other. `zip` stops at the shorter path, so all-equal-so-far ⇒ the
/// shorter is a prefix of the longer ⇒ overlap; a divergence at any position
/// (`["a"]` vs `["b"]`) ⇒ disjoint siblings ⇒ no overlap. An empty path (the
/// whole binding) overlaps every sub-place of the same root.
fn paths_overlap(a: &[String], b: &[String]) -> bool {
    a.iter().zip(b.iter()).all(|(x, y)| x == y)
}

/// Render a place for a diagnostic: `root` followed by its projection segments.
/// Field segments are plain names (`field` → `.field`); tuple segments arrive
/// pre-dotted from `find_root_def_id_with_path` (`.0` → `.0`).
fn render_place(root: &str, path: &[String]) -> String {
    let mut s = root.to_string();
    for seg in path {
        s.push('.');
        s.push_str(seg.strip_prefix('.').unwrap_or(seg));
    }
    s
}

/// Prefix a rendered place with its call-arg sigil for the diagnostic
/// (`&place` for a writer, `!place` for a mover, bare for a reader).
fn sigil_place(own: Ownership, place: &str) -> String {
    match own {
        Ownership::Borrow => place.to_string(),
        Ownership::MutableBorrow => format!("&{place}"),
        Ownership::Move => format!("!{place}"),
    }
}

impl<'a> BorrowChecker<'a> {
    /// D4/D12: if `e` is a PLACE whose type is drop-tainted, return a display
    /// name for the error PLUS its place shape (Whole vs field/index sub-place),
    /// which selects the remedy in the `E_MoveWithoutOperator` message. The
    /// implicit copy of a live drop-tainted place at an ownership boundary is
    /// `E_MoveWithoutOperator`. Mirrors ggdef's `reject_if_tainted_live_place`
    /// (spec/ggdef/src/elaborate/mod.rs:571-583).
    pub(super) fn tainted_place_name(&self, e: &Spanned<Expr>) -> Option<(String, MoveShape)> {
        if !expr_is_place(&e.node) {
            return None;
        }
        // The place's VALUE type, resolved STRUCTURALLY through
        // `lvalue_value_type` — NOT the sparse `expr_types` map. The
        // typechecker records `expr_types` only at call / method / scrutinee
        // spans; FieldAccess / Index / TupleField spans are never inserted
        // (`typecheck.rs:2272+`), so an `expr_types`-primary lookup silently
        // misses `hh.r` / `v[i]` and lets a live drop-tainted field-place
        // bind UNREJECTED (a real double-drop). `lvalue_value_type` walks the
        // identifier/self/field/tuple/index chain via `struct_field_names` +
        // `DefInfo.field_types` (ggdef's `infer_ast_ty` shape) and covers
        // every place shape this way.
        let tid = self.lvalue_value_type(e)?;
        if !crate::semantic::is_drop_tainted_type(tid, self.types, self.scopes) {
            return None;
        }
        // Display name: the root binding's name (good enough for the fix-it;
        // the exact sub-place text `hh.r.clone()` vs `hh.clone()` is a filed
        // LOW follow-up — `find_root_def_id` returns only the ROOT name).
        let name = self
            .find_root_def_id(e)
            .map(|d| self.scopes.get_def(d).name.clone())
            .unwrap_or_else(|| "<place>".to_string());
        Some((name, place_shape(&e.node)))
    }

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
            | Expr::Propagate { expr: inner }
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
            | Expr::Propagate { expr: inner }
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
            | Expr::Propagate { expr: operand }
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
            | Expr::Propagate { expr: inner }
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

    /// Like `find_root_def_id` but also returns the field path traversed
    /// from the root to the expression. Used by the CoW-borrow checker
    /// to track field-level disjointness: borrows from `gpu.shader_cache`
    /// shouldn't be invalidated by mutations to disjoint sibling
    /// `gpu.deform_face_indices`. The returned `Vec<String>` contains
    /// field names in outer-to-inner order (`["shader_cache"]` for
    /// `gpu.shader_cache`); empty for a direct binding reference.
    /// Index/OptionalChain segments don't append to the path — they
    /// dereference, not project (an index borrow is from the collection
    /// itself, not from a sub-path of it).
    pub(super) fn find_root_def_id_with_path(
        &self, expr: &Spanned<Expr>,
    ) -> Option<(DefId, Vec<String>)> {
        match &expr.node {
            Expr::Identifier(_) => self.resolution_map.get(&expr.span.start)
                .copied()
                .map(|d| (d, Vec::new())),
            Expr::SelfExpr => self.resolution_map.get(&expr.span.start)
                .copied()
                .map(|d| (d, Vec::new())),
            Expr::FieldAccess { object, field } => {
                let (root, mut path) = self.find_root_def_id_with_path(object)?;
                path.push(field.node.clone());
                Some((root, path))
            }
            Expr::TupleFieldAccess { object, index } => {
                let (root, mut path) = self.find_root_def_id_with_path(object)?;
                path.push(format!(".{index}"));
                Some((root, path))
            }
            Expr::Index { object, .. }
            | Expr::OptionalChain { object, .. } =>
                self.find_root_def_id_with_path(object),
            _ => None,
        }
    }

    /// Check if an expression is an index/get access on a collection.

    /// Like `find_collection_source` but also returns the field path
    /// from the root binding to the collection. Used by the CoW-borrow
    /// checker to record where a borrow points, so that disjoint
    /// sibling-field mutations don't invalidate it.
    /// `gpu.shader_cache.get(i)` returns `Some((gpu_def_id, ["shader_cache"]))`;
    /// `vec.get(i)` returns `Some((vec_def_id, []))`.
    pub(super) fn find_collection_source_with_path(
        &self, expr: &Spanned<Expr>,
    ) -> Option<(DefId, Vec<String>)> {
        match &expr.node {
            Expr::Index { object, .. } => self.find_root_def_id_with_path(object),
            Expr::MethodCall { receiver, method, .. }
                if matches!(method.node.as_str(), "unwrap" | "expect") =>
            {
                self.find_collection_source_with_path(receiver)
            }
            Expr::MethodCall { receiver, method, .. }
                if matches!(method.node.as_str(), "get" | "first" | "last") =>
            {
                self.find_root_def_id_with_path(receiver)
            }
            _ => None,
        }
    }

    /// For a borrow-producing collection-read expression (`v.get(0).unwrap()`,
    /// `dict.get(k).unwrap()`, `v[i]`, `v.first()`/`.last()`), return the
    /// **bound element type** the borrow aliases — i.e. the type of the value
    /// being read OUT of the collection, peeled of the borrow-view `Ref`
    /// wrapper. Used by the arena-escape sibling check at collection-mutation
    /// consume positions: the in-buffer element is what dangles when the arena
    /// is destroyed, so the Copy/non-Copy decision MUST be on this element
    /// type (`Vector[String]` → `String` non-Copy → fires; `Vector[int]` →
    /// `int` Copy → must NOT fire), NOT on the whole source-collection type.
    ///
    /// Primary source: the type checker records the read result as
    /// `Ref(elem)` (a borrow view) in `expr_types`; peeling the `Ref` recovers
    /// the bound element type precisely (and uniformly across Vector/Set/Dict —
    /// a Dict read binds the *value* type, which `expr_types` already resolved).
    /// Fallback: derive from the source collection's declared generic args
    /// (last arg: `Vector[T]`→`T`, `Set[T]`→`T`, `Dict[K,V]`→`V`).
    pub(super) fn arena_borrowed_element_type(
        &self, arg: &Spanned<Expr>,
    ) -> Option<crate::semantic::ids::TypeId> {
        // Primary: peel the borrow-view `Ref` off the recorded read result.
        if let Some(&tid) = self.expr_types.get(&arg.span) {
            if let ResolvedType::Ref(inner) = self.types.get(tid) {
                return Some(*inner);
            }
            // Already a non-Ref concrete type (some reads aren't wrapped) — use
            // it directly only when it isn't the collection itself. Fall through
            // to the generic-arg derivation if it doesn't look like an element.
        }
        // Fallback: derive the element from the source collection's generic args.
        let recv = self.collection_read_receiver(arg)?;
        let coll_tid = self.expr_types.get(&recv.span).copied()
            .or_else(|| {
                // Direct binding: use the declared type of the root def.
                self.find_root_def_id_with_path(recv)
                    .and_then(|(root, _)| self.scopes.get_def(root).type_id)
            })?;
        match self.types.get(coll_tid) {
            ResolvedType::Generic(_, args) if !args.is_empty() => {
                // Vector[T]/Set[T] → T; Dict[K,V] → V (the read binds the value).
                args.last().copied()
            }
            ResolvedType::Array(elem, _) | ResolvedType::Slice(elem) => Some(*elem),
            _ => None,
        }
    }

    /// True when `recv` has a buffer-owning builtin type whose mutating,
    /// element-ingesting method copies/moves the passed element INTO a
    /// self-owned heap buffer that the arena does NOT free at `with`-scope
    /// exit. Two typed shapes qualify (uniformly — same heap-use-after-free
    /// class), read off the protocol:
    ///   - `collection_kind: Some(_)` — Vector/Deque/Dict/HashMap/Set/HashSet,
    ///     whose `push`/`put`/`insert` copy the element into the collection's
    ///     heap buffer.
    ///   - `owns_buffered_elements: true` — Channel (`send`) and Heap (`push`):
    ///     non-collection handles that nonetheless own a heap buffer the
    ///     ingested element is copied into.
    /// An arena-borrowed non-Copy element aliased into either buffer dangles
    /// when the arena is destroyed. Mutex/Shared/Guard/Atomic/Barrier/... carry
    /// `collection_kind: None` AND `owns_buffered_elements: false` (no
    /// element-ingesting owned buffer) and are correctly excluded — a TYPED
    /// gate on the protocol, no name-matching of `send`/`push`/`Channel`.
    pub(super) fn is_buffer_owning_receiver(
        &self, recv: &Spanned<Expr>,
    ) -> bool {
        // Resolve the receiver's type (inferred result or, for a bare binding,
        // its declared type), then test it via the typed predicate.
        let tid = self.expr_types.get(&recv.span).copied().or_else(|| {
            self.find_root_def_id_with_path(recv)
                .and_then(|(root, path)| {
                    if path.is_empty() {
                        self.scopes.get_def(root).type_id
                    } else {
                        None
                    }
                })
        });
        tid.map_or(false, |t| self.is_buffer_owning_type(t))
    }

    /// Typed predicate underlying `is_buffer_owning_receiver`: does `type_id`
    /// name a builtin protocol whose mutating element-ingesting method copies
    /// the element into a self-owned heap buffer (`collection_kind.is_some()`
    /// OR `owns_buffered_elements`)? Single source of truth so the arena-escape
    /// gate AND the arena-scoped-binding tracker agree on which receivers own a
    /// surviving buffer. Critically, a buffer-owning handle declared INSIDE the
    /// arena scope (e.g. a `Channel`, which `is_copy_type` treats as a Copy
    /// pointer) must be tracked as arena-scoped so the gate's "receiver
    /// outlives the arena" test is correct — `is_copy_type` alone would miss it.
    pub(super) fn is_buffer_owning_type(
        &self, type_id: crate::semantic::ids::TypeId,
    ) -> bool {
        let base_def = match self.types.get(type_id) {
            ResolvedType::Defined(d) | ResolvedType::Generic(d, _) => *d,
            _ => return false,
        };
        let base_name = &self.scopes.get_def(base_def).name;
        crate::ir::lowering::builtins::lookup_protocol(base_name)
            .map_or(false, |p| p.collection_kind.is_some() || p.owns_buffered_elements)
    }

    /// The builtin `CollectionKind` of the value `expr` denotes (its recorded
    /// or declared type resolved through `lookup_protocol`), or `None` when
    /// `expr` isn't a builtin collection. Used by the arena-escape Assign gate
    /// to decide whether an `Expr::Index` STORE target materializes its
    /// key/value (map/set put) — an INGEST — or stores a view (array set) — a
    /// BIND — from the SAME typed fact the runtime store path uses.
    pub(super) fn collection_kind_of_expr(
        &self, expr: &Spanned<Expr>,
    ) -> Option<crate::ir::types::CollectionKind> {
        let tid = self.lvalue_value_type(expr)?;
        let base_def = match self.types.get(tid) {
            ResolvedType::Defined(d) | ResolvedType::Generic(d, _) => *d,
            _ => return None,
        };
        let base_name = &self.scopes.get_def(base_def).name;
        crate::ir::lowering::builtins::lookup_protocol(base_name)
            .and_then(|p| p.collection_kind)
    }

    /// ONE PRODUCER for the arena-escape source classification (Core #4 —
    /// fix the class, not the instance). Only meaningful when
    /// `arena_depth > 0`; every escape gate (assign-to-outer, return,
    /// element-ingest into an outer-rooted buffer, compound-assign) calls
    /// this instead of classifying source shapes locally.
    ///
    /// Classifies whether the VALUE of `expr` is arena-backed — i.e. whether
    /// its heap bytes are freed when the innermost `with <allocator>` block
    /// exits. Under the dynamic `with` redirect EVERY allocation in the block
    /// draws from the block allocator, so the classification is by
    /// PROVENANCE, not by shape-specific tracing:
    ///
    ///   SAFE   — bare identifier (incl. `!ident`) of a var bound OUTSIDE the
    ///            arena scope: pure alias/move of outer-backed bytes, no
    ///            materialization (ASan-verified CLEAN).
    ///   SAFE   — Copy-typed values (value-copied out, no heap bytes).
    ///   SAFE   — plain string literals (static storage; ASan-verified CLEAN).
    ///   ESCAPE — bare identifier of an arena-scoped var (`arena_scoped_vars`).
    ///   ESCAPE — everything else non-Copy: ctor / function / method calls,
    ///            f-strings, list/dict/set literals, concatenation,
    ///            comprehensions … — all materialize under the redirect.
    ///            This INCLUDES chains rooted in OUTER collections
    ///            (`src.get(0).unwrap()`, `src.pop().unwrap()`): their
    ///            transient/clone materializes in the arena (ASan-verified
    ///            UAF), and element provenance through a mutable collection
    ///            is statically unknowable anyway.
    ///
    /// Returns `(diagnostic_name, span)` of the offending source.
    /// `fallback_tid` is the DESTINATION type, used for the Copy test when
    /// the expression's own span has no recorded type (unknown = non-Copy,
    /// conservative).
    ///
    /// `ctx` distinguishes the two consume shapes that treat a PLAIN STRING
    /// LITERAL differently:
    ///   - `EscapeCtx::Bind` (assign / return): a literal binds/returns as a
    ///     static view (`gorget_str_from_literal`, cap==0, no arena
    ///     allocation) — ASan-verified safe, so exempt it.
    ///   - `EscapeCtx::Ingest` (element push/put/insert/add/send): the
    ///     collection must OWN its element, so the literal materializes an
    ///     owned heap copy through the arena allocator (ASan-verified UAF at
    ///     arena teardown) — do NOT exempt; treat as arena-backed.
    /// Every OTHER shape is context-independent.
    pub(super) fn arena_backed_source(
        &self,
        expr: &Spanned<Expr>,
        fallback_tid: Option<crate::semantic::ids::TypeId>,
        ctx: EscapeCtx,
    ) -> Option<(String, Span)> {
        match &expr.node {
            Expr::Move { expr: inner }
            | Expr::Propagate { expr: inner }
            | Expr::MutableBorrow { expr: inner } => {
                self.arena_backed_source(inner, fallback_tid, ctx)
            }
            Expr::Identifier(name) => {
                let did = self.resolution_map.get(&expr.span.start)?;
                if self.arena_scoped_vars.contains(did) {
                    Some((name.clone(), expr.span))
                } else {
                    None
                }
            }
            // Receiver alias (`self`), never a fresh materialization here;
            // the return-of-borrowed-receiver case is the return gate's
            // param rule, not the producer's.
            Expr::SelfExpr => None,
            Expr::IntLiteral(_)
            | Expr::FloatLiteral(_)
            | Expr::BoolLiteral(_)
            | Expr::NoneLiteral => None,
            // Plain string literal: static bytes when BOUND (a static view,
            // safe), but an owned heap copy when INGESTED into a collection —
            // decided explicitly per context rather than via the Copy test
            // (the recorded literal type may be the Copy `str` VIEW, which
            // would wrongly clear the ingest case). An f-string (has
            // interpolations) always materializes in the arena, both contexts.
            Expr::StringLiteral(_, interps) if interps.is_empty() => match ctx {
                EscapeCtx::Bind => None,
                EscapeCtx::Ingest => {
                    Some(("<arena-allocated temporary>".to_string(), expr.span))
                }
            },
            _ => {
                if self.expr_value_is_copy(expr, fallback_tid) {
                    None
                } else {
                    Some(("<arena-allocated temporary>".to_string(), expr.span))
                }
            }
        }
    }

    /// The COMPLETE ingest-escape rule set for ONE ingested operand — the
    /// single producer (Core #4) shared by EVERY ingest position: collection
    /// `push`/`put`/`insert`/`add`/`send`, and the `c[k] = v` / `c[k] += v`
    /// index-store KEY & VALUE on a materializing (map/set) collection. Sharing
    /// this one helper is what makes `d.put(k, v)`, `d[k] = v`, and the
    /// `d[k] += v` key BEHAVIORALLY IDENTICAL — no per-position ingest rule can
    /// drift out of sync again (the R-A/#1 class).
    ///
    /// Two rules, applied in order (the caller reports the returned error):
    ///   (1) `arena_backed_source(.., Ingest)` — a plain string literal is NOT
    ///       exempt in the Ingest context (the collection OWNS its element, so
    ///       the literal is copied into an arena-allocated slot); an
    ///       arena-scoped identifier and any fresh non-Copy materialization
    ///       escape → `AssignOuter`.
    ///   (2) a bare non-`!`, non-Copy OUTER identifier → `IngestLiveOuter`: at
    ///       a consume boundary a still-live source is clone-if-live, and under
    ///       the `with` redirect that clone lands in the arena (ASan-verified
    ///       UAF). `!ident` moves the outer-backed bytes (ASan-verified safe)
    ///       and stays accepted — so rule (2) is skipped when `is_moved`.
    ///
    /// `is_moved` is the position's move signal: a method-call arg carries it
    /// as `CallArg.ownership == Move`; an assignment operand carries it as an
    /// `Expr::Move` wrapper (which also cannot match the bare-`Identifier`
    /// shape rule (2) tests, so a mis-passed flag can never wrongly fire).
    pub(super) fn classify_ingest_escape(
        &self,
        arg: &Spanned<Expr>,
        fallback_tid: Option<crate::semantic::ids::TypeId>,
        is_moved: bool,
        target_name: &str,
    ) -> Option<(SemanticErrorKind, Span)> {
        use crate::semantic::errors::ArenaEscapeKind;
        // Rule (1): the one producer, in Ingest context (no literal exemption).
        if let Some((name, span)) =
            self.arena_backed_source(arg, fallback_tid, EscapeCtx::Ingest)
        {
            return Some((
                SemanticErrorKind::ArenaEscape {
                    name,
                    kind: ArenaEscapeKind::AssignOuter {
                        target: target_name.to_string(),
                    },
                },
                span,
            ));
        }
        // Rule (2): bare non-`!`, non-Copy OUTER identifier (clone-if-live).
        if !is_moved {
            if let Expr::Identifier(name) = &arg.node {
                if let Some(&did) = self.resolution_map.get(&arg.span.start) {
                    let non_copy = self.scopes.get_def(did).type_id.map_or(false, |t| {
                        !super::type_utils::is_copy_type(t, self.types, self.scopes)
                    });
                    if non_copy {
                        return Some((
                            SemanticErrorKind::ArenaEscape {
                                name: name.clone(),
                                kind: ArenaEscapeKind::IngestLiveOuter {
                                    target: target_name.to_string(),
                                },
                            },
                            arg.span,
                        ));
                    }
                }
            }
        }
        None
    }

    /// Copy test for the value an expression produces: the recorded
    /// `expr_types` entry (peeled of the borrow-view `Ref` wrapper), falling
    /// back to the destination type; unknown counts as non-Copy
    /// (conservative). Buffer-owning handles (Channel/Heap) count as
    /// non-Copy even though `is_copy_type` treats them as Copy pointers —
    /// their owned buffer is what dangles.
    pub(super) fn expr_value_is_copy(
        &self,
        expr: &Spanned<Expr>,
        fallback_tid: Option<crate::semantic::ids::TypeId>,
    ) -> bool {
        let tid = self
            .expr_types
            .get(&expr.span)
            .copied()
            .map(|t| match self.types.get(t) {
                ResolvedType::Ref(inner) => *inner,
                _ => t,
            })
            .or(fallback_tid);
        match tid {
            Some(t) => {
                super::type_utils::is_copy_type(t, self.types, self.scopes)
                    && !self.is_buffer_owning_type(t)
            }
            None => false,
        }
    }

    /// Resolve the VALUE type an lvalue (assign/compound-assign target)
    /// denotes, so the Copy test can decide whether writing through it can
    /// arena-escape. The typechecker does NOT record `expr_types` at
    /// field/tuple/index target spans (its `infer_expr` returns those types
    /// without inserting them), so we resolve structurally from the object's
    /// type:
    ///   - identifier / `self`  → recorded type, else the decl type;
    ///   - `s.field`            → the struct's `field_types[idx]` via the
    ///                            index of `field` in `struct_field_names`;
    ///   - `t.0`                → the tuple element type;
    ///   - `c[i]`               → the collection element type (Vector[T]/
    ///                            Set[T]→T, Dict[K,V]→V, array/slice elem).
    /// Returns `None` when unknown (caller treats unknown as non-Copy —
    /// conservative, so an unresolved shape still errs toward rejection).
    pub(super) fn lvalue_value_type(
        &self, target: &Spanned<Expr>,
    ) -> Option<crate::semantic::ids::TypeId> {
        use crate::semantic::types::ResolvedType;
        match &target.node {
            Expr::Identifier(_) | Expr::SelfExpr => self
                .expr_types
                .get(&target.span)
                .copied()
                .or_else(|| {
                    self.resolution_map
                        .get(&target.span.start)
                        .and_then(|&d| self.scopes.get_def(d).type_id)
                }),
            Expr::FieldAccess { object, field } => {
                let obj_tid = self.lvalue_value_type(object)?;
                let did = match self.types.get(obj_tid) {
                    ResolvedType::Defined(d) | ResolvedType::Generic(d, _) => *d,
                    _ => return None,
                };
                let idx = self
                    .struct_field_names
                    .get(&did)?
                    .iter()
                    .position(|n| n == &field.node)?;
                self.scopes.get_def(did).field_types.as_ref()?.get(idx).copied()
            }
            Expr::TupleFieldAccess { object, index } => {
                let obj_tid = self.lvalue_value_type(object)?;
                match self.types.get(obj_tid) {
                    ResolvedType::Tuple(elems) => elems.get(*index).copied(),
                    _ => None,
                }
            }
            Expr::Index { object, .. } => {
                let obj_tid = self.lvalue_value_type(object)?;
                match self.types.get(obj_tid) {
                    ResolvedType::Generic(_, args) if !args.is_empty() => {
                        args.last().copied()
                    }
                    ResolvedType::Array(elem, _) | ResolvedType::Slice(elem) => {
                        Some(*elem)
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// For a borrow-producing collection-read expression, return the receiver
    /// expression that is the collection being read (peeling `.unwrap()`/
    /// `.expect()` and the `.get()`/`.first()`/`.last()`/index layers).
    fn collection_read_receiver<'e>(
        &self, expr: &'e Spanned<Expr>,
    ) -> Option<&'e Spanned<Expr>> {
        match &expr.node {
            Expr::Index { object, .. } => Some(object),
            Expr::MethodCall { receiver, method, .. }
                if matches!(method.node.as_str(), "unwrap" | "expect") =>
            {
                self.collection_read_receiver(receiver)
            }
            Expr::MethodCall { receiver, method, .. }
                if matches!(method.node.as_str(), "get" | "first" | "last") =>
            {
                Some(receiver)
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

    /// Dead-write lint: record a mutation through a tracked bare
    /// resource param. `span` is the mutation site (used as the warning
    /// span). Must be called AFTER the target/receiver walk so the write's
    /// clock exceeds any read recorded by that walk.
    pub(super) fn mark_bare_param_write_def(&mut self, def_id: DefId, span: Span) {
        // D4/D12 position 6 (materialize-on-write): a write through a bare
        // BORROW-view param of a drop-tainted type would materialize
        // (privatise) an implicit copy — rejected. Mirrors ggdef's
        // `reject_materialize_on_write`
        // (spec/ggdef/src/elaborate/mod.rs:588-596, :1664-1676).
        {
            let def = self.scopes.get_def(def_id);
            if def.is_param
                && def.param_ownership == Some(Ownership::Borrow)
                && def.type_id.map_or(false, |t| {
                    crate::semantic::is_drop_tainted_type(t, self.types, self.scopes)
                })
            {
                let name = def.name.clone();
                // Materialize-on-write is always a WHOLE bare borrow-param place.
                self.error(
                    SemanticErrorKind::MoveWithoutOperator {
                        name,
                        reason: MoveReason::DropTaint,
                        shape: MoveShape::Whole,
                    },
                    span,
                );
            }
        }
        if !self.deadwrite_params.contains_key(&def_id) {
            return;
        }
        let clock = self.deadwrite_clock;
        self.deadwrite_clock += 1;
        let loops = self.deadwrite_loop_stack.clone();
        if let Some(info) = self.deadwrite_params.get_mut(&def_id) {
            info.last_write = Some((clock, span));
            info.write_loops = loops;
        }
    }

    /// Dead-write lint: record a genuine read of a tracked bare
    /// resource param. Skipped when the read is the target/receiver of the
    /// mutation currently being marked (`deadwrite_write_root`).
    pub(super) fn mark_bare_param_read(&mut self, def_id: DefId) {
        if self.deadwrite_write_root == Some(def_id) {
            return;
        }
        if !self.deadwrite_params.contains_key(&def_id) {
            return;
        }
        let clock = self.deadwrite_clock;
        self.deadwrite_clock += 1;
        let loop_ids: Vec<u32> = self.deadwrite_loop_stack.clone();
        if let Some(info) = self.deadwrite_params.get_mut(&def_id) {
            info.last_read = Some(clock);
            info.read_loops.extend(loop_ids);
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

    /// Detect aliasing conflicts within a single call's arguments (D10(b)
    /// place-overlap check — `decisions.md` D10 + the 2026-07-12 D10(b) ADDENDUM
    /// + Rider 1 REVISED 2026-07-14). Two args whose PLACES overlap under
    /// conflicting sigils are rejected. "Places overlap" = same root binding AND
    /// one projection path is a prefix of the other (disjoint siblings like
    /// `m.a` / `m.b` do NOT overlap). "Conflicting" = at least one arg is a
    /// writer (`&`) or mover (`!`) and they are not both bare readers.
    ///
    /// The check ranges over LIVE ALIASES, not syntactic reads: a bare read of a
    /// COPY-typed place is a value SNAPSHOT (evaluated into an independent value
    /// before the callee runs) and participates in no overlap — so
    /// `f(&s, s.copy_int)` is legal. A non-Copy bare read IS a live borrow under
    /// CoW-default, so it conflicts with an overlapping writer.
    ///
    /// Examples: `f(&x, &x)` / `f(&x, &x.field)` (both writers, overlap) —
    /// reject; `f(x, &x)` (non-Copy read + writer) — reject; `f(x, !x)` (read +
    /// move) — reject; `f(&s, s.copy_int)` (writer + Copy snapshot) — OK;
    /// `f(&m.a, &m.b)` (disjoint siblings) — OK.
    ///
    /// Two AXES are deliberately kept out of this check:
    ///   * `(Move, Move)` overlap (`f(!x, !x)`, `f(!n, !n.field)`) is handled by
    ///     E_DoubleMove — a projection move root-marks the whole DefId, so the
    ///     second move already trips DoubleMove. Duplicating it here would
    ///     double-diagnose.
    ///   * `f(!x, x.copy_field)` (move THEN Copy read of the moved slot) is
    ///     rejected UPSTREAM by the move-tracker (E_UseAfterMove) — a LIVENESS
    ///     violation, not place-overlap. This check neither needs nor grants a
    ///     mover Copy-exemption (Rider 1 REVISED).
    pub(super) fn check_call_aliasing(&mut self, args: &[Spanned<CallArg>]) {
        // Collect the PLACE args: (root DefId, projection path, sigil, Copy-ness,
        // span). Non-places (fresh temps, literals, `.clone()`, `.get()`) resolve
        // to `None` and are skipped — they cannot alias.
        struct ArgPlace {
            root: DefId,
            path: Vec<String>,
            ownership: Ownership,
            is_copy: bool,
            span: Span,
        }
        let mut places: Vec<ArgPlace> = Vec::new();

        for arg in args {
            let Some((root, path)) = self.find_root_def_id_with_path(&arg.node.value) else {
                continue;
            };
            // Only VARIABLE roots participate. Params ARE `DefKind::Variable`
            // (there is no `DefKind::Parameter`), so `f(&p, p)` on a param is
            // still checked. `self`-rooted places have no `resolution_map` entry
            // → already skipped above (see the `#[ignore]`d self-root fixture +
            // TODO for wiring that later).
            if self.scopes.get_def(root).kind != DefKind::Variable {
                continue;
            }
            // Copy-ness of the arg VALUE via the TYPED axis (Rider 2 — NO
            // name/shape heuristic). `lvalue_value_type` is the STRUCTURAL
            // fallback: the typechecker does not record `expr_types` at
            // field/tuple/index spans, so a `None` fallback would wrongly report
            // a Copy field read (`s.copy_int`) as non-Copy and over-reject
            // `f(&s, s.copy_int)`.
            let is_copy = self.expr_value_is_copy(
                &arg.node.value,
                self.lvalue_value_type(&arg.node.value),
            );
            places.push(ArgPlace {
                root,
                path,
                ownership: arg.node.ownership,
                is_copy,
                span: arg.span,
            });
        }

        // Pairwise conflict: same root + overlapping projection prefix +
        // conflicting sigils (after dropping Copy bare readers).
        for i in 0..places.len() {
            for j in (i + 1)..places.len() {
                let a = &places[i];
                let b = &places[j];

                if a.root != b.root {
                    continue;
                }
                if !paths_overlap(&a.path, &b.path) {
                    // Disjoint siblings (e.g. `m.a` vs `m.b`) — no live-alias edge.
                    continue;
                }

                // Copy-read exemption: a bare read of a Copy-typed place is a
                // value snapshot, not a live alias. Drop any such arg from the
                // pair; a single remaining arg (or none) cannot conflict.
                let a_copy_reader = a.ownership == Ownership::Borrow && a.is_copy;
                let b_copy_reader = b.ownership == Ownership::Borrow && b.is_copy;
                if a_copy_reader || b_copy_reader {
                    continue;
                }

                // `(Move, Move)` overlap is E_DoubleMove's job (a projection move
                // root-marks) — keep it out to avoid double-diagnosing.
                if a.ownership == Ownership::Move && b.ownership == Ownership::Move {
                    continue;
                }

                // Conflict iff ≥1 remaining arg is a writer/mover AND they are
                // not both bare readers.
                let has_writer_or_mover =
                    matches!(a.ownership, Ownership::MutableBorrow | Ownership::Move)
                        || matches!(b.ownership, Ownership::MutableBorrow | Ownership::Move);
                let both_bare =
                    a.ownership == Ownership::Borrow && b.ownership == Ownership::Borrow;
                if !has_writer_or_mover || both_bare {
                    continue;
                }

                let name = self.scopes.get_def(a.root).name.clone();
                let place_a = render_place(&name, &a.path);
                let place_b = render_place(&name, &b.path);
                let detail = format!(
                    "cannot pass `{}` and `{}` in the same call — their places overlap",
                    sigil_place(a.ownership, &place_a),
                    sigil_place(b.ownership, &place_b),
                );
                // Point the span at the SECOND (overlapping) arg — it reads as
                // "this later arg conflicts with an earlier one".
                self.error(
                    SemanticErrorKind::BorrowConflict { name, detail },
                    b.span,
                );
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

    /// If `expr` is `<receiver>.lock()` on a resolvable `Mutex` identifier,
    /// returns the receiver's DefId and its name. Used by the double-lock
    /// detector at VarDecl sites. RwLock is intentionally out of scope for
    /// this first cut — its read/write semantics differ enough that a
    /// strict same-receiver rule would generate noise on legitimate
    /// patterns.
    pub(super) fn lock_call_target(&self, expr: &Spanned<Expr>) -> Option<(DefId, String)> {
        let (receiver, method_name) = match &expr.node {
            Expr::MethodCall { receiver, method, .. } => (receiver.as_ref(), method.node.as_str()),
            _ => return None,
        };
        if method_name != "lock" {
            return None;
        }
        let recv_name = match &receiver.node {
            Expr::Identifier(name) => name.clone(),
            _ => return None,
        };
        let recv_def = self.resolution_map.get(&receiver.span.start).copied()
            .or_else(|| self.scopes.lookup_def_by_span(&recv_name, receiver.span))
            .or_else(|| self.find_def_by_name(&recv_name))?;
        let tid = self.scopes.get_def(recv_def).type_id?;
        let type_def_name = match self.types.get(tid) {
            ResolvedType::Generic(did, _) | ResolvedType::Defined(did) => {
                self.scopes.get_def(*did).name.clone()
            }
            _ => return None,
        };
        if type_def_name != "Mutex" { return None; }
        Some((recv_def, recv_name))
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
