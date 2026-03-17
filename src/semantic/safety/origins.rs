use rustc_hash::{FxHashMap, FxHashSet};

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use crate::semantic::errors::SemanticErrorKind;
use crate::semantic::ids::DefId;
use crate::semantic::scope::DefKind;
use crate::semantic::types::{self as types};

use super::{ActiveOutlives, BorrowChecker, BorrowOrigin, BranchState, FallibleState, VarState};

impl<'a> BorrowChecker<'a> {
    pub(super) fn mark_live(&mut self, def_id: DefId) {
        self.var_states.insert(def_id, VarState::Live);
        // Track variables declared inside loops so we can allow safe
        // per-iteration moves (variable is re-created each iteration).
        if let Some(local_set) = self.loop_local_defs.last_mut() {
            local_set.insert(def_id);
        }
    }

    /// Check that a variable is usable (Live). Error if Moved.
    /// Also checks if a reference-typed variable's source has been invalidated.
    pub(super) fn check_use(&mut self, def_id: DefId, span: Span) {
        if let Some(VarState::Moved { moved_at }) = self.var_states.get(&def_id) {
            let name = self.scopes.get_def(def_id).name.clone();
            self.error(
                SemanticErrorKind::UseAfterMove {
                    name,
                    moved_at: *moved_at,
                },
                span,
            );
            return;
        }

        // Phase 11: Check if this variable's borrow source was reassigned.
        if let Some((source_name, reassigned_at)) = self.reassignment_invalidated.get(&def_id) {
            let name = self.scopes.get_def(def_id).name.clone();
            self.error(
                SemanticErrorKind::UseAfterSourceMoved {
                    name,
                    source_name: source_name.clone(),
                    moved_at: *reassigned_at,
                },
                span,
            );
            return;
        }

        // Lifetime check: if this variable has a reference type, check that its
        // source hasn't been moved/invalidated.
        if let Some(origin) = self.var_origins.get(&def_id).cloned() {
            for &invalidated_id in &self.invalidated_origins {
                if origin.references_def(invalidated_id) {
                    let name = self.scopes.get_def(def_id).name.clone();
                    let source_name = self.scopes.get_def(invalidated_id).name.clone();
                    // Find the span where the source was moved
                    let moved_at = if let Some(VarState::Moved { moved_at }) = self.var_states.get(&invalidated_id) {
                        *moved_at
                    } else {
                        span
                    };
                    self.error(
                        SemanticErrorKind::UseAfterSourceMoved {
                            name,
                            source_name,
                            moved_at,
                        },
                        span,
                    );
                    return;
                }
            }
        }

        // Async: check if variable was alive before an await suspension point.
        if self.await_invalidated.contains(&def_id) {
            let name = self.scopes.get_def(def_id).name.clone();
            self.error(SemanticErrorKind::BorrowAcrossAwait { name }, span);
            self.await_invalidated.remove(&def_id); // prevent duplicate errors
            return;
        }
    }

    // ─── Origin Tracking ──────────────────────────────────

    /// Merge multiple origins into a single origin.
    pub(super) fn merge_origins(origins: Vec<BorrowOrigin>) -> BorrowOrigin {
        match origins.len() {
            0 => BorrowOrigin::Static,
            1 => origins.into_iter().next().unwrap(),
            _ => BorrowOrigin::CallResult(origins),
        }
    }

    /// Compute the borrow origin of an expression.
    pub(super) fn compute_expr_origin(&self, expr: &Spanned<Expr>) -> BorrowOrigin {
        match &expr.node {
            // String literals are always valid (static storage).
            // NOTE: f-strings allocate heap memory (GorgetString) with local lifetime,
            // but the IR layer handles ownership via string_backing + mark-moved.
            // A future enhancement could add BorrowOrigin::Owned to catch dangling
            // str views of f-strings, but this requires updating many existing fixtures.
            Expr::StringLiteral(_) => BorrowOrigin::Static,

            Expr::Identifier(_) => {
                if let Some(&def_id) = self.resolution_map.get(&expr.span.start) {
                    let def = self.scopes.get_def(def_id);

                    // Is it a parameter?
                    if def.is_param {
                        if let Some((_, idx)) = self.current_param_def_ids.iter().find(|(id, _)| *id == def_id) {
                            return BorrowOrigin::Param { param_index: *idx, def_id };
                        }
                    }

                    // Is it a local variable with a known origin?
                    if let Some(origin) = self.var_origins.get(&def_id) {
                        return origin.clone();
                    }

                    // Only mark as Local if this variable owns data (non-reference, non-callable type).
                    // Reference-type locals that aren't in var_origins (e.g. pattern bindings
                    // from match arms) are views into existing data — not new local sources.
                    // Callable-type locals get their capture origins stored in var_origins;
                    // if absent, the closure captures no ref-type data and is safe to return.
                    if def.kind == DefKind::Variable {
                        if let Some(type_id) = def.type_id {
                            if !types::is_reference_type(type_id, self.types, &self.ref_type_structs)
                                && !types::is_callable_type(type_id, self.types)
                            {
                                return BorrowOrigin::Local(def_id);
                            }
                        } else {
                            // No type info — conservative: treat as local
                            return BorrowOrigin::Local(def_id);
                        }
                    }
                }
                BorrowOrigin::Unknown
            }

            // Field access propagates from the object
            Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
                self.compute_expr_origin(object)
            }

            // Index/slice propagates from the container
            Expr::Index { object, .. } => self.compute_expr_origin(object),

            // Call/method call: use callee's return_borrows_from if available (Phase 2+)
            Expr::Call { callee, args, .. } => {
                self.compute_call_origin(callee, args)
            }
            Expr::MethodCall { receiver, method, args, .. } => {
                self.compute_method_call_origin(receiver, method, args)
            }

            // If/Match expressions: union of branch origins (conservative)
            Expr::If { then_branch, elif_branches, else_branch, .. } => {
                let mut origins = vec![self.compute_expr_origin(then_branch)];
                for (_, body) in elif_branches {
                    origins.push(self.compute_expr_origin(body));
                }
                if let Some(else_br) = else_branch {
                    origins.push(self.compute_expr_origin(else_br));
                }
                Self::merge_origins(origins)
            }

            Expr::Match { arms, else_arm, .. } => {
                let mut origins: Vec<_> = arms.iter()
                    .map(|arm| self.compute_expr_origin(&arm.body))
                    .collect();
                if let Some(else_arm) = else_arm {
                    origins.push(self.compute_expr_origin(else_arm));
                }
                Self::merge_origins(origins)
            }

            Expr::Block(block) | Expr::Do { body: block } => {
                // Origin comes from the last expression statement
                if let Some(last) = block.stmts.last() {
                    if let Stmt::Expr(e) = &last.node {
                        return self.compute_expr_origin(e);
                    }
                }
                BorrowOrigin::Unknown
            }

            // Struct literal: origin is the union of all reference-type field args
            Expr::StructLiteral { name, args, .. } => {
                if let Some(def_id) = self.resolution_map.get(&name.span.start).copied()
                    .or_else(|| self.scopes.lookup(&name.node))
                {
                    if let Some(ref_flags) = self.struct_field_ref_flags.get(&def_id) {
                        let origins: Vec<BorrowOrigin> = args.iter()
                            .zip(ref_flags.iter())
                            .filter(|(_, is_ref)| **is_ref)
                            .map(|(arg, _)| self.compute_expr_origin(arg))
                            .collect();
                        return Self::merge_origins(origins);
                    }
                }
                BorrowOrigin::Unknown
            }

            // Closure: origin is the union of captured ref-type variables' origins
            Expr::Closure { params, body, .. } => {
                let param_names: FxHashSet<&str> = params.iter()
                    .map(|p| p.node.name.node.as_str()).collect();
                let captured_origins = self.collect_captured_ref_origins(body, &param_names);
                Self::merge_origins(captured_origins)
            }

            // Transparent wrappers: propagate inner origin
            Expr::Move { expr: inner }
            | Expr::Deref { expr: inner }
            | Expr::As { expr: inner, .. } => {
                self.compute_expr_origin(inner)
            }

            // Default operator: either branch could provide the result
            Expr::DefaultOp { lhs, rhs } => {
                let origins = vec![
                    self.compute_expr_origin(lhs),
                    self.compute_expr_origin(rhs),
                ];
                Self::merge_origins(origins)
            }

            // Collection literals: propagate element origins
            Expr::ArrayLiteral(elems) | Expr::TupleLiteral(elems) => {
                let origins: Vec<_> = elems.iter()
                    .map(|e| self.compute_expr_origin(e))
                    .collect();
                Self::merge_origins(origins)
            }

            Expr::DictLiteral(pairs) => {
                let origins: Vec<_> = pairs.iter()
                    .flat_map(|(k, v)| [self.compute_expr_origin(k), self.compute_expr_origin(v)])
                    .collect();
                Self::merge_origins(origins)
            }

            // Self in equip methods: param 0
            Expr::SelfExpr => {
                if let Some(&(def_id, idx)) = self.current_param_def_ids.first() {
                    BorrowOrigin::Param { param_index: idx, def_id }
                } else {
                    BorrowOrigin::Unknown
                }
            }

            // Value-type literals: always produce new owned values, never references
            Expr::IntLiteral(_)
            | Expr::FloatLiteral(_)
            | Expr::BoolLiteral(_) => BorrowOrigin::Static,

            // Binary/unary ops produce new values (arithmetic, comparison, logical)
            Expr::BinaryOp { .. } | Expr::UnaryOp { .. } => BorrowOrigin::Static,

            // Range produces a new value
            Expr::Range { .. } => BorrowOrigin::Static,

            // `is` check produces a bool
            Expr::Is { .. } => BorrowOrigin::Static,

            // Comprehensions produce new collections
            Expr::ListComprehension { .. }
            | Expr::DictComprehension { .. }
            | Expr::SetComprehension { .. } => BorrowOrigin::Static,

            // Optional chain propagates from object
            Expr::OptionalChain { object, .. } => self.compute_expr_origin(object),

            // Mutable borrow propagates from inner
            Expr::MutableBorrow { expr: inner } => self.compute_expr_origin(inner),

            // Await/spawn propagate from inner expression
            Expr::Await { expr: inner } | Expr::Spawn { expr: inner } | Expr::SpawnBlocking { expr: inner } => {
                self.compute_expr_origin(inner)
            }

            // Path expressions (e.g., Module.name): resolve like identifiers
            Expr::Path { .. } => {
                if let Some(&def_id) = self.resolution_map.get(&expr.span.start) {
                    if let Some(origin) = self.var_origins.get(&def_id) {
                        return origin.clone();
                    }
                    let def = self.scopes.get_def(def_id);
                    if def.kind == DefKind::Variable {
                        return BorrowOrigin::Local(def_id);
                    }
                }
                // Qualified path to a constant/static — treat as static
                BorrowOrigin::Static
            }

            // Everything else: unknown (conservative)
            _ => BorrowOrigin::Unknown,
        }
    }

    /// Compute the origin of a function call result using callee's `return_borrows_from`.
    /// Struct constructor calls are handled by `compute_expr_origin()` for `StructLiteral`
    /// (struct calls are rewritten to StructLiteral by the post-resolution rewrite pass).
    pub(super) fn compute_call_origin(&self, callee: &Spanned<Expr>, args: &[Spanned<CallArg>]) -> BorrowOrigin {
        let callee_def_id = self.resolve_callee_def_id(callee);
        if let Some(def_id) = callee_def_id {
            if let Some(info) = self.function_info.get(&def_id) {
                if !info.return_borrows_from.is_empty() {
                    let origins: Vec<BorrowOrigin> = info.return_borrows_from.iter()
                        .filter_map(|&idx| args.get(idx).map(|a| self.compute_expr_origin(&a.node.value)))
                        .collect();
                    return if origins.len() == 1 {
                        origins.into_iter().next().unwrap()
                    } else {
                        BorrowOrigin::CallResult(origins)
                    };
                }
                if info.return_origin_is_static {
                    return BorrowOrigin::Static;
                }
                // If callee returns an owned (non-reference, non-callable) type,
                // the call produces fresh data — always Static.
                if let Some(ret_tid) = info.return_type_id {
                    if !types::is_reference_type(ret_tid, self.types, &self.ref_type_structs)
                        && !types::is_callable_type(ret_tid, self.types)
                    {
                        return BorrowOrigin::Static;
                    }
                }
                // Function body was analyzed but no borrows found and not explicitly static.
                // The return value is fresh data (e.g., newly constructed ref-type value).
                // Only applies to functions with bodies — bodyless functions with ambiguous
                // elision (multiple ref params, no `live`) should remain Unknown.
                if info.has_body {
                    return BorrowOrigin::Static;
                }
            }

            // Struct constructors always produce fresh data — never borrow from args.
            // Generic collection constructors (Dict, Vector, Set, etc.) reach this path
            // because they aren't rewritten to StructLiteral by post-resolution.
            let def = self.scopes.get_def(def_id);
            if def.kind == DefKind::Struct {
                return BorrowOrigin::Static;
            }

            // Callable variable calls: `h(req)` where `h: Callable[T(...)]`.
            // We can't inspect h's body, so propagate conservatively from h's own
            // captured origin (analogous to method receiver) plus all arg origins.
            if def.kind == DefKind::Variable {
                let callable_origin = self.compute_expr_origin(callee);
                let mut origins = vec![callable_origin];
                origins.extend(args.iter().map(|a| self.compute_expr_origin(&a.node.value)));
                return Self::merge_origins(origins);
            }

            // Enum variant constructors: `Ok(x)`, `Error("msg")`, `Some(y)`, etc.
            // The variant transparently wraps its arguments; propagate their origins.
            if def.kind == DefKind::Variant {
                let origins: Vec<BorrowOrigin> = args.iter()
                    .map(|a| self.compute_expr_origin(&a.node.value))
                    .collect();
                return Self::merge_origins(origins);
            }
        }
        BorrowOrigin::Unknown
    }

    /// Compute the origin of a method call result using `return_borrows_from` data.
    pub(super) fn compute_method_call_origin(&self, receiver: &Spanned<Expr>, method: &Spanned<String>, args: &[Spanned<CallArg>]) -> BorrowOrigin {
        let method_def_id_opt = self.method_resolutions.get(&method.span.start);
        if let Some(&def_id) = method_def_id_opt {
            let info_opt = self.function_info.get(&def_id);
            if let Some(info) = info_opt {
                if !info.return_borrows_from.is_empty() {
                    // Methods have self as param 0: index 0 = receiver, N>0 = args[N-1]
                    let origins: Vec<BorrowOrigin> = info.return_borrows_from.iter()
                        .filter_map(|&idx| {
                            if idx == 0 {
                                Some(self.compute_expr_origin(receiver))
                            } else {
                                args.get(idx - 1).map(|a| self.compute_expr_origin(&a.node.value))
                            }
                        })
                        .collect();
                    return Self::merge_origins(origins);
                }
                if info.return_origin_is_static {
                    return BorrowOrigin::Static;
                }
                // If callee returns an owned (non-reference, non-callable) type,
                // the call produces fresh data — always Static.
                if let Some(ret_tid) = info.return_type_id {
                    if !types::is_reference_type(ret_tid, self.types, &self.ref_type_structs)
                        && !types::is_callable_type(ret_tid, self.types)
                    {
                        return BorrowOrigin::Static;
                    }
                }
            }
        }
        // If the receiver is a struct/enum/newtype TYPE NAME (not a value instance),
        // any unresolved static factory method constructs a fresh value — always Static.
        // This covers patterns like `HttpServerResponse.not_found()` where the struct
        // definition's type_id is None so method_resolutions was never populated.
        if let Expr::Identifier(_) = &receiver.node {
            if let Some(def_id) = self.resolve_callee_def_id(receiver) {
                let def = self.scopes.get_def(def_id);
                if matches!(def.kind, DefKind::Struct | DefKind::Enum | DefKind::Newtype) {
                    return BorrowOrigin::Static;
                }
            }
        }
        // Fallback: conservatively propagate receiver origin
        self.compute_expr_origin(receiver)
    }

    /// Record outlives constraints for a call to a function with `where a outlives b` bounds.
    /// Maps group names to actual argument origins via `param_live_groups`.
    pub(super) fn record_call_outlives(
        &mut self,
        callee: &Spanned<Expr>,
        args: &[Spanned<CallArg>],
        call_span: Span,
    ) {
        let callee_def_id = match self.resolve_callee_def_id(callee) {
            Some(id) => id,
            None => return,
        };
        let info = match self.function_info.get(&callee_def_id) {
            Some(i) => i,
            None => return,
        };
        if info.outlives_bounds.is_empty() {
            return;
        }

        // Build group name → argument origin mapping
        let mut group_origins: FxHashMap<String, Vec<DefId>> = FxHashMap::default();
        for (i, group) in info.param_live_groups.iter().enumerate() {
            if let Some(group_name) = group {
                if let Some(arg) = args.get(i) {
                    let origin = self.compute_expr_origin(&arg.node.value);
                    group_origins
                        .entry(group_name.clone())
                        .or_default()
                        .extend(origin.source_def_ids());
                }
            }
        }

        // Create ActiveOutlives entries for each bound
        for (longer, shorter) in &info.outlives_bounds {
            let longer_ids = group_origins.get(longer).cloned().unwrap_or_default();
            let shorter_ids = group_origins.get(shorter).cloned().unwrap_or_default();
            if !longer_ids.is_empty() || !shorter_ids.is_empty() {
                self.active_outlives.push(ActiveOutlives {
                    longer_group: longer.clone(),
                    shorter_group: shorter.clone(),
                    longer_source_def_ids: longer_ids,
                    shorter_source_def_ids: shorter_ids,
                    _call_span: call_span,
                });
            }
        }
    }

    /// Check outlives constraints when a variable is moved.
    /// For `where a outlives b`: if the moved DefId is a source for group `a` ("longer"),
    /// and any source for group `b` ("shorter") is not yet invalidated, that's a violation.
    pub(super) fn check_outlives_on_move(&mut self, moved_def_id: DefId, span: Span) {
        let mut violations = Vec::new();

        for constraint in &self.active_outlives {
            // Check if the moved variable is a source for the "longer" group
            if constraint.longer_source_def_ids.contains(&moved_def_id) {
                // Check if any "shorter" group source is still alive
                for &shorter_id in &constraint.shorter_source_def_ids {
                    if !self.invalidated_origins.contains(&shorter_id) {
                        let longer_name = self.scopes.get_def(moved_def_id).name.clone();
                        let shorter_name = self.scopes.get_def(shorter_id).name.clone();
                        violations.push((
                            constraint.longer_group.clone(),
                            constraint.shorter_group.clone(),
                            longer_name,
                            shorter_name,
                            span,
                        ));
                    }
                }
            }
        }

        for (longer_group, shorter_group, longer_source, shorter_source, span) in violations {
            self.error(
                SemanticErrorKind::OutlivesViolation {
                    longer_group,
                    shorter_group,
                    longer_source,
                    shorter_source,
                },
                span,
            );
        }
    }

    /// Move a variable: mark as Moved. Error if already moved or inside a loop.
    pub(super) fn check_move(&mut self, def_id: DefId, span: Span) {
        let name = self.scopes.get_def(def_id).name.clone();

        // Check if already moved
        if let Some(VarState::Moved { moved_at }) = self.var_states.get(&def_id) {
            self.error(
                SemanticErrorKind::DoubleMove {
                    name: name.clone(),
                    first_move: *moved_at,
                },
                span,
            );
            return;
        }

        // Check if inside a loop — but allow moves of variables declared within
        // the innermost loop body (they are re-created each iteration).
        if self.loop_depth > 0 {
            let is_loop_local = self.loop_local_defs.last()
                .map_or(false, |set| set.contains(&def_id));
            if !is_loop_local {
                self.error(SemanticErrorKind::MoveInLoop { name }, span);
                return;
            }
        }

        self.var_states.insert(def_id, VarState::Moved { moved_at: span });
        // Invalidate this def for lifetime tracking — any reference-typed variable
        // whose origin chain includes this DefId becomes dangling.
        self.invalidated_origins.insert(def_id);

        // Phase 5: Check outlives constraints — if we're moving a "longer" group's source,
        // check that all "shorter" group sources are already invalidated.
        self.check_outlives_on_move(def_id, span);

        // B6: If the moved variable is a closure that had mutable captures,
        // release those capture locks — the closure is no longer live.
        if let Some(captured_def_ids) = self.mut_capture_owners.remove(&def_id) {
            for captured_id in captured_def_ids {
                if let Some(entries) = self.mut_captured_vars.get_mut(&captured_id) {
                    entries.retain(|(_, cid, _)| *cid != def_id);
                    if entries.is_empty() {
                        self.mut_captured_vars.remove(&captured_id);
                    }
                }
            }
        }
    }


    // ─── Branch State (combined VarState + Origin) ───────

    /// Save combined branch state (var states + origin tracking).
    pub(super) fn save_branch_state(&self) -> BranchState {
        BranchState {
            var_states: self.var_states.clone(),
            origins: self.var_origins.clone(),
            invalidated: self.invalidated_origins.clone(),
            reassignment_invalidated: self.reassignment_invalidated.clone(),
            await_invalidated: self.await_invalidated.clone(),
            mut_captured_vars: self.mut_captured_vars.clone(),
            mut_capture_owners: self.mut_capture_owners.clone(),
            shared_derived: self.shared_derived.clone(),
            stale_shared_derived: self.stale_shared_derived.clone(),
            diverges: self.diverged,
            fallible_states: self.fallible_states.clone(),
        }
    }

    /// Restore combined branch state.
    pub(super) fn restore_branch_state(&mut self, state: &BranchState) {
        self.var_states = state.var_states.clone();
        self.var_origins = state.origins.clone();
        self.invalidated_origins = state.invalidated.clone();
        self.reassignment_invalidated = state.reassignment_invalidated.clone();
        self.await_invalidated = state.await_invalidated.clone();
        self.mut_captured_vars = state.mut_captured_vars.clone();
        self.mut_capture_owners = state.mut_capture_owners.clone();
        self.shared_derived = state.shared_derived.clone();
        self.stale_shared_derived = state.stale_shared_derived.clone();
        self.diverged = state.diverges;
        self.fallible_states = state.fallible_states.clone();
    }

    /// Merge multiple branch states: union var states (moved in either = moved),
    /// union origins, union invalidated sets.
    /// Branches that diverge (return/break/continue/throw) are excluded from
    /// the merge because their state never reaches the join point. If ALL
    /// branches diverge, the merged state is marked diverged.
    pub(super) fn merge_branch_states(&mut self, states: &[BranchState]) {
        if states.is_empty() {
            return;
        }

        // Filter to branches that actually reach the join point.
        let live: Vec<&BranchState> = states.iter().filter(|s| !s.diverges).collect();

        if live.is_empty() {
            // Every branch diverges — keep first state, mark diverged.
            self.var_states = states[0].var_states.clone();
            self.var_origins = states[0].origins.clone();
            self.invalidated_origins = states[0].invalidated.clone();
            self.reassignment_invalidated = states[0].reassignment_invalidated.clone();
            self.await_invalidated = states[0].await_invalidated.clone();
            self.mut_captured_vars = states[0].mut_captured_vars.clone();
            self.mut_capture_owners = states[0].mut_capture_owners.clone();
            self.shared_derived = states[0].shared_derived.clone();
            self.stale_shared_derived = states[0].stale_shared_derived.clone();
            self.fallible_states = states[0].fallible_states.clone();
            self.diverged = true;
            return;
        }

        let mut merged_vars = live[0].var_states.clone();
        let mut merged_origins = live[0].origins.clone();
        let mut merged_invalidated = live[0].invalidated.clone();
        let mut merged_reassignment_invalidated = live[0].reassignment_invalidated.clone();
        let mut merged_await_invalidated = live[0].await_invalidated.clone();
        let mut merged_mut_captured = live[0].mut_captured_vars.clone();
        let mut merged_mut_owners = live[0].mut_capture_owners.clone();
        let mut merged_shared_derived = live[0].shared_derived.clone();
        let mut merged_stale_shared = live[0].stale_shared_derived.clone();
        let mut merged_fallible = live[0].fallible_states.clone();

        for state in &live[1..] {
            // Merge var states: moved in either = moved
            for (def_id, b_state) in &state.var_states {
                match (merged_vars.get(def_id), b_state) {
                    (Some(VarState::Moved { .. }), _) => {}
                    (_, VarState::Moved { moved_at }) => {
                        merged_vars.insert(*def_id, VarState::Moved { moved_at: *moved_at });
                    }
                    _ => {}
                }
            }
            // Merge origins: union (keep both)
            for (def_id, origin) in &state.origins {
                merged_origins.entry(*def_id).or_insert_with(|| origin.clone());
            }
            // Merge invalidated: union (conservative)
            merged_invalidated.extend(&state.invalidated);
            // Merge reassignment_invalidated: union (conservative)
            for (def_id, info) in &state.reassignment_invalidated {
                merged_reassignment_invalidated.entry(*def_id).or_insert_with(|| info.clone());
            }
            // Merge await_invalidated: union (conservative — if any branch has await, use after merge is suspect)
            merged_await_invalidated.extend(&state.await_invalidated);
            // Merge mut_captured_vars: union — if a capture lock exists in any branch,
            // it must be conservatively assumed live after the merge.
            for (def_id, entries) in &state.mut_captured_vars {
                let existing = merged_mut_captured.entry(*def_id).or_default();
                for entry in entries {
                    if !existing.iter().any(|(_, cid, _)| *cid == entry.1) {
                        existing.push(entry.clone());
                    }
                }
            }
            // Merge mut_capture_owners: union.
            for (closure_id, captured_ids) in &state.mut_capture_owners {
                let existing = merged_mut_owners.entry(*closure_id).or_default();
                for id in captured_ids {
                    if !existing.contains(id) {
                        existing.push(*id);
                    }
                }
            }
            // Merge shared_derived: union (conservative — if any branch has it, keep)
            for (def_id, info) in &state.shared_derived {
                merged_shared_derived.entry(*def_id).or_insert_with(|| info.clone());
            }
            // Merge stale_shared_derived: union (conservative — stale in any branch = stale)
            for (def_id, info) in &state.stale_shared_derived {
                merged_stale_shared.entry(*def_id).or_insert_with(|| info.clone());
            }
            // Merge fallible_states: conservative — unchecked in any branch = unchecked
            for (def_id, fs) in &state.fallible_states {
                match (merged_fallible.get(def_id), fs) {
                    (Some(FallibleState::Unchecked), _) | (_, FallibleState::Unchecked) => {
                        merged_fallible.insert(*def_id, FallibleState::Unchecked);
                    }
                    _ => {
                        merged_fallible.entry(*def_id).or_insert(*fs);
                    }
                }
            }
        }

        self.var_states = merged_vars;
        self.var_origins = merged_origins;
        self.invalidated_origins = merged_invalidated;
        self.reassignment_invalidated = merged_reassignment_invalidated;
        self.await_invalidated = merged_await_invalidated;
        self.mut_captured_vars = merged_mut_captured;
        self.mut_capture_owners = merged_mut_owners;
        self.shared_derived = merged_shared_derived;
        self.stale_shared_derived = merged_stale_shared;
        self.fallible_states = merged_fallible;
        self.diverged = false;
    }
}
