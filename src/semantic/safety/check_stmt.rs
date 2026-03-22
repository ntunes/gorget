use rustc_hash::FxHashSet;

use crate::parser::ast::*;
use crate::span::Spanned;

use crate::semantic::errors::{ArenaEscapeKind, SemanticErrorKind};
use crate::semantic::ids::DefId;
use crate::semantic::scope::DefKind;
use crate::semantic::types::{self as types};

use super::{BorrowChecker, BorrowOrigin, BorrowCaptureMode, FallibleState, SharedDerivedInfo, WithGuardKind};
use super::type_utils::{is_copy_type, is_ast_type_ref};

impl<'a> BorrowChecker<'a> {
    pub(super) fn check_stmt(&mut self, stmt: &Spanned<Stmt>) {
        match &stmt.node {
            Stmt::VarDecl {
                pattern, value, type_, shared, ..
            } => {
                // Check the value expression
                self.check_expr(value);

                // Check: if value is a bare identifier of non-Copy type, needs `!`
                // Skip for view-string targets: creating a view from an owned string
                // is a borrow (not a move). The provenance pass + AST rewrite set
                // the target type to Str for view bindings.
                // Destructuring patterns (Tuple) implicitly move the value,
                // so suppress MoveWithoutOperator for them.
                let target_is_view_str = matches!(type_.node, Type::Primitive(PrimitiveType::Str));
                if !target_is_view_str {
                    let is_destructure = matches!(&pattern.node, Pattern::Tuple(_));
                    if is_destructure {
                        self.in_destructuring_bind = true;
                    }
                    self.check_value_needs_move(value);
                    if is_destructure {
                        self.in_destructuring_bind = false;
                    }
                }

                // Mark new bindings as Live
                self.mark_pattern_live_spanned(pattern);

                // Phase 3: Register local variable for unused-variable tracking
                if let Pattern::Binding(name) = &pattern.node {
                    if let Some(def_id) = self.scopes.lookup_def_by_span(name, pattern.span)
                        .or_else(|| self.find_def_by_name(name))
                    {
                        self.local_var_usage.insert(def_id, (name.clone(), pattern.span, false));
                    }
                } else if let Pattern::Tuple(bindings) = &pattern.node {
                    // Tuple destructuring: register each binding
                    for binding in bindings {
                        if let Pattern::Binding(name) = &binding.node {
                            if let Some(def_id) = self.scopes.lookup_def_by_span(name, binding.span)
                                .or_else(|| self.find_def_by_name(name))
                            {
                                self.local_var_usage.insert(def_id, (name.clone(), binding.span, false));
                            }
                        }
                    }
                }

                // Track Option/Result variables for fallible unwrap detection
                if let Pattern::Binding(name) = &pattern.node {
                    if let Some(def_id) = self.scopes.lookup_def_by_span(name, pattern.span)
                        .or_else(|| self.find_def_by_name(name))
                    {
                        let is_fallible = self.scopes.get_def(def_id).type_id
                            .and_then(|tid| self.is_option_or_result_type(tid))
                            .is_some();
                        if is_fallible {
                            self.fallible_states.insert(def_id, FallibleState::Unchecked);
                        }
                    }
                }

                // Track shared bindings for CFA
                if *shared != crate::parser::ast::SharedKind::None {
                    if let Pattern::Binding(name) = &pattern.node {
                        if let Some(def_id) = self.scopes.lookup_def_by_span(name, pattern.span)
                            .or_else(|| self.find_def_by_name(name))
                        {
                            self.shared_var_defs.insert(def_id, (*shared, name.clone(), pattern.span));
                            // Mark origin as Static — shared bindings are pointer-wrapped, safe to pass
                            self.var_origins.insert(def_id, BorrowOrigin::Static);
                            // For user overrides, immediately populate CFA decision
                            match shared {
                                crate::parser::ast::SharedKind::RwLock => {
                                    self.shared_out.insert(def_id, crate::semantic::SharedStrategy::ArcRwLock);
                                }
                                crate::parser::ast::SharedKind::Atomic => {
                                    self.shared_out.insert(def_id, crate::semantic::SharedStrategy::ArcAtomic);
                                }
                                _ => {} // Auto — decided at spawn sites
                            }
                        }
                    }
                }

                // Track locals derived from shared variables (§3.4 stale-condition).
                // Only when the VarDecl itself is not `shared` and we're in an async fn.
                if *shared == crate::parser::ast::SharedKind::None && self.current_function_is_async {
                    if let Pattern::Binding(local_name) = &pattern.node {
                        if let Some(shared_name) = self.find_shared_ref_in_expr_spanned(value) {
                            if let Some(def_id) = self.scopes.lookup_def_by_span(local_name, pattern.span)
                                .or_else(|| self.find_def_by_name(local_name))
                            {
                                self.shared_derived.insert(def_id, SharedDerivedInfo {
                                    local_name: local_name.clone(),
                                    shared_name,
                                    derivation_span: value.span,
                                    await_span: None,
                                });
                            }
                        }
                    }
                }

                // Track non-Copy variables declared inside arena scope
                if self.arena_depth > 0 {
                    if let Pattern::Binding(name) = &pattern.node {
                        if let Some(def_id) = self.scopes.lookup_def_by_span(name, pattern.span)
                            .or_else(|| self.find_def_by_name(name))
                        {
                            if let Some(type_id) = self.scopes.get_def(def_id).type_id {
                                if !is_copy_type(type_id, self.types, self.scopes) {
                                    self.arena_scoped_vars.insert(def_id);
                                }
                            }
                        }
                    }
                }

                // Track `alloc=` arena propagation: if the value is a Call with
                // alloc=<arena_scoped_var>, mark the new variable as arena-scoped.
                if let Pattern::Binding(name) = &pattern.node {
                    if let Expr::Call { args, .. } = &value.node {
                        for arg in args {
                            if arg.node.name.as_ref().map_or(false, |n| n.node == "alloc") {
                                if let Expr::Identifier(alloc_name) = &arg.node.value.node {
                                    let alloc_def = self.scopes.lookup_def_by_span(alloc_name, arg.node.value.span)
                                        .or_else(|| self.find_def_by_name(alloc_name));
                                    if let Some(alloc_did) = alloc_def {
                                        if self.arena_scoped_vars.contains(&alloc_did)
                                            || (self.arena_depth > 0 && self.is_allocator_type(alloc_did))
                                        {
                                            if let Some(var_did) = self.scopes.lookup_def_by_span(name, pattern.span)
                                                .or_else(|| self.find_def_by_name(name))
                                            {
                                                self.arena_scoped_vars.insert(var_did);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Track origin for reference-typed variables
                if let Pattern::Binding(name) = &pattern.node {
                    let def_id_opt = self.scopes.lookup_def_by_span(name, pattern.span)
                        .or_else(|| self.find_def_by_name(name));
                    if let Some(def_id) = def_id_opt {
                        let is_ref = self.is_var_reference_type(def_id, Some(type_));
                        if is_ref {
                            let origin = self.compute_expr_origin(value);
                            self.var_origins.insert(def_id, origin.clone());

                            // Phase 6: Detect binding a ref type to a temporary
                            // Skip when auto-propagation applies: the Ok value is moved
                            // out of the Result wrapper, not borrowed from it.
                            if self.is_temporary_borrow(value)
                                && !self.is_auto_propagated_call(value)
                            {
                                let callee_name = match &value.node {
                                    Expr::Call { callee, .. } => Self::extract_callee_name(callee),
                                    Expr::MethodCall { receiver, method, .. } => {
                                        format!("{}.{}", Self::extract_receiver_name(receiver), method.node)
                                    }
                                    _ => "<unknown>".to_string(),
                                };
                                self.error(
                                    SemanticErrorKind::TemporaryBorrow {
                                        name: name.clone(),
                                        callee: callee_name,
                                        temp_at: None,
                                    },
                                    value.span,
                                );
                            }
                        }

                        // Phase 9+10: Track origin for callable-typed variables.
                        // Covers both direct closures and callable values from function calls.
                        if !self.var_origins.contains_key(&def_id) {
                            let is_callable = self.scopes.get_def(def_id).type_id
                                .map_or(false, |tid| types::is_callable_type(tid, self.types));
                            // Also check expression form for `auto` vars where type_id may not be set
                            if is_callable || matches!(&value.node, Expr::Closure { .. }) {
                                let origin = self.compute_expr_origin(value);
                                self.var_origins.insert(def_id, origin);
                            }
                        }

                        // Capture set tracking: associate the pending capture set
                        // (computed during check_expr(Closure)) with this variable.
                        if let Some(cs) = self.pending_capture_set.take() {
                            // B3: MutCallable aliasing — register mutable captures so that
                            // direct reads/writes to the captured variable are rejected
                            // while this closure is live.
                            for entry in &cs.captures {
                                if entry.mode == BorrowCaptureMode::Mutable {
                                    self.mut_captured_vars
                                        .entry(entry.def_id)
                                        .or_default()
                                        .push((name.clone(), def_id, pattern.span));
                                    self.mut_capture_owners
                                        .entry(def_id)
                                        .or_default()
                                        .push(entry.def_id);
                                }
                            }

                            // §3.9 Closure capture of with binding: if a closure inside
                            // a `with` block captures a with-tracked shared variable,
                            // the captured value may become stale after a yield.
                            if self.with_depth > 0 && self.current_function_is_async {
                                for entry in &cs.captures {
                                    if self.with_shared_tracked.contains(&entry.def_id) {
                                        let var_name = self.scopes.get_def(entry.def_id).name.clone();
                                        self.stale_warnings.push(crate::semantic::errors::SemanticWarning {
                                            kind: crate::semantic::errors::SemanticWarningKind::ClosureCapturesWithBinding {
                                                var_name,
                                            },
                                            span: pattern.span,
                                        });
                                    }
                                }
                            }

                            self.closure_capture_sets.insert(def_id, cs);
                        } else {
                            // Track struct/array variables that contain closures capturing locals.
                            // Even without a pending_capture_set (not a direct closure assignment),
                            // the value might be a StructLiteral whose args include closures.
                            self.mark_var_if_contains_closures(def_id, value);
                        }
                    }
                }
            }

            Stmt::Expr(expr) => {
                self.check_expr(expr);
            }

            Stmt::Assign { target, value } => {
                // Track writes to shared variables for CFA
                if let Expr::Identifier(_) = &target.node {
                    if let Some(&def_id) = self.resolution_map.get(&target.span.start) {
                        if self.shared_var_defs.contains_key(&def_id) {
                            self.shared_written.insert(def_id);
                        }

                        // §3.6 Stale write-back: if writing to a shared/with-tracked
                        // variable and the RHS contains a stale-derived value, warn.
                        if self.current_function_is_async {
                            let is_shared_target = self.shared_var_defs.contains_key(&def_id)
                                || self.with_shared_tracked.contains(&def_id);
                            if is_shared_target {
                                let target_shared_name = if let Some((_, name, _)) = self.shared_var_defs.get(&def_id) {
                                    name.clone()
                                } else {
                                    self.scopes.get_def(def_id).name.clone()
                                };
                                if let Some((info, _use_span)) = self.find_stale_in_condition(value) {
                                    self.stale_warnings.push(crate::semantic::errors::SemanticWarning {
                                        kind: crate::semantic::errors::SemanticWarningKind::StaleSharedWriteBack {
                                            local_name: info.local_name,
                                            source_shared_name: info.shared_name,
                                            target_shared_name,
                                            derivation_span: Some(info.derivation_span),
                                            yield_span: info.await_span,
                                        },
                                        span: value.span,
                                    });
                                }
                            }
                        }

                        // §3.8 Compound yield race: `x = x + blocking_call()` reads x before
                        // the yield (RHS), writes after. Detect when assigning to a with-tracked
                        // var and the RHS both references the same var AND contains a yield point.
                        if self.current_function_is_async && self.with_shared_tracked.contains(&def_id) {
                            let rhs_names = self.find_with_tracked_in_condition(value);
                            let target_name = self.scopes.get_def(def_id).name.clone();
                            if rhs_names.contains(&target_name) && self.expr_contains_yield_point(value) {
                                self.stale_warnings.push(crate::semantic::errors::SemanticWarning {
                                    kind: crate::semantic::errors::SemanticWarningKind::CompoundYieldRace {
                                        shared_name: target_name,
                                        yield_span: value.span,
                                    },
                                    span: value.span,
                                });
                            }
                        }

                        // §3.4: Clear stale status if this local is reassigned (fresh value)
                        self.stale_shared_derived.remove(&def_id);
                        self.shared_derived.remove(&def_id);
                        // If reassigning from a shared var, re-track as derived
                        if self.current_function_is_async {
                            if let Some(shared_name) = self.find_shared_ref_in_expr_spanned(value) {
                                let name = self.scopes.get_def(def_id).name.clone();
                                self.shared_derived.insert(def_id, SharedDerivedInfo {
                                    local_name: name,
                                    shared_name,
                                    derivation_span: value.span,
                                    await_span: None,
                                });
                            }
                        }
                    }
                }
                // B6: When reassigning a closure variable with a new closure,
                // release the old closure's mutable capture locks BEFORE checking
                // the RHS. This prevents false positives where the new closure
                // body reads a variable that the old closure held mutably.
                if matches!(&value.node, Expr::Closure { .. }) {
                    if let Expr::Identifier(_) = &target.node {
                        if let Some(&def_id) = self.resolution_map.get(&target.span.start) {
                            if self.mut_capture_owners.contains_key(&def_id) {
                                if let Some(captured_def_ids) = self.mut_capture_owners.remove(&def_id) {
                                    for captured_id in &captured_def_ids {
                                        if let Some(entries) = self.mut_captured_vars.get_mut(captured_id) {
                                            entries.retain(|(_, cid, _)| *cid != def_id);
                                            if entries.is_empty() {
                                                self.mut_captured_vars.remove(captured_id);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                self.check_expr(value);

                // Borrowed resource params cannot be stored in struct fields.
                // The field would hold a shallow copy sharing the caller's heap data.
                // Only check bare identifiers — field access (b.value) reads a single
                // field which may be a Copy type, not the whole resource param.
                if matches!(&target.node, Expr::FieldAccess { .. } | Expr::Index { .. }) {
                    if let Expr::Identifier(_) = &value.node {
                        if let Some((param_name, kind)) = self.check_non_owned_param_move(value) {
                            self.error(
                                SemanticErrorKind::MutationOfBareParam {
                                    name: param_name,
                                    detail: format!("cannot store {kind} parameter in a field — use `!` to transfer ownership"),
                                },
                                value.span,
                            );
                        }
                    }
                }

                // Check: if value is a bare identifier of non-Copy type, needs `!`
                // Skip for view-string targets (creating a view from owned is a borrow).
                let target_is_view_str = if let Expr::Identifier(_) = &target.node {
                    self.resolution_map.get(&target.span.start)
                        .and_then(|&did| self.scopes.get_def(did).type_id)
                        .map_or(false, |tid| tid == self.types.string_id)
                } else {
                    false
                };
                if !target_is_view_str {
                    self.check_value_needs_move(value);
                }

                // Arena escape check: cannot assign arena-scoped value to outer variable
                if self.arena_depth > 0 {
                    if let Expr::Identifier(target_name) = &target.node {
                        if let Some(&target_def_id) = self.resolution_map.get(&target.span.start) {
                            let rhs_def_id = match &value.node {
                                Expr::Identifier(_) => self.resolution_map.get(&value.span.start).copied(),
                                Expr::Move { expr: inner } => {
                                    if let Expr::Identifier(_) = &inner.node {
                                        self.resolution_map.get(&inner.span.start).copied()
                                    } else { None }
                                }
                                _ => None,
                            };
                            if let Some(rhs_id) = rhs_def_id {
                                if self.arena_scoped_vars.contains(&rhs_id)
                                    && !self.arena_scoped_vars.contains(&target_def_id)
                                {
                                    self.error(
                                        SemanticErrorKind::ArenaEscape {
                                            name: self.scopes.get_def(rhs_id).name.clone(),
                                            kind: ArenaEscapeKind::AssignOuter {
                                                target: target_name.clone(),
                                            },
                                        },
                                        value.span,
                                    );
                                }
                            }
                        }
                    }
                }

                // Track reassignment for const promotion and `&` param mutation
                if let Expr::Identifier(_) = &target.node {
                    if let Some(&def_id) = self.resolution_map.get(&target.span.start) {
                        self.var_reassigned.insert(def_id);
                        // Direct assignment to `&` param counts as mutation
                        let def = self.scopes.get_def(def_id);
                        if def.is_param && def.param_ownership == Some(Ownership::MutableBorrow) {
                            self.mut_param_mutated.insert(def_id);
                        }
                    }
                }

                // Check immutability/const constraints on identifier targets
                match &target.node {
                    Expr::Identifier(target_var_name) => {
                        if let Some(&def_id) = self.resolution_map.get(&target.span.start) {
                            let def = self.scopes.get_def(def_id);
                            if def.kind == DefKind::Const {
                                self.error(
                                    SemanticErrorKind::AssignmentToConst { name: def.name.clone() },
                                    target.span,
                                );
                            }
                            // B5: MutCallable aliasing — writing to a variable while it is
                            // mutably captured by a live closure is unsound.
                            if let Some(entries) = self.mut_captured_vars.get(&def_id) {
                                if let Some((closure_name, _, _)) = entries.first() {
                                    let closure_name = closure_name.clone();
                                    self.error(
                                        SemanticErrorKind::WriteWhileMutCaptured {
                                            var_name: target_var_name.clone(),
                                            closure_name,
                                        },
                                        target.span,
                                    );
                                }
                            }
                            // Reassignment revives a moved variable
                            self.mark_live(def_id);
                            // Also un-invalidate: if this variable was moved and
                            // reassigned, it's no longer a dangling source.
                            self.invalidated_origins.remove(&def_id);

                            // Phase 11: If the dependent variable itself is reassigned,
                            // clear its stale-borrow entry — it now has a fresh value.
                            self.reassignment_invalidated.remove(&def_id);

                            // Reset fallible state on reassignment (new value is unchecked)
                            if self.fallible_states.contains_key(&def_id) {
                                self.fallible_states.insert(def_id, FallibleState::Unchecked);
                            }

                            // Async: reassignment after await clears suspension-point invalidation.
                            self.await_invalidated.remove(&def_id);

                            // If the new value is a closure, register its mutable captures.
                            // Only consume pending_capture_set when the RHS is actually a closure
                            // (not for arbitrary assignments like `x = 1` inside a closure body).
                            if matches!(&value.node, Expr::Closure { .. }) {
                                if let Some(cs) = self.pending_capture_set.take() {
                                    for entry in &cs.captures {
                                        if entry.mode == BorrowCaptureMode::Mutable {
                                            self.mut_captured_vars
                                                .entry(entry.def_id)
                                                .or_default()
                                                .push((target_var_name.clone(), def_id, target.span));
                                            self.mut_capture_owners
                                                .entry(def_id)
                                                .or_default()
                                                .push(entry.def_id);
                                        }
                                    }
                                    self.closure_capture_sets.insert(def_id, cs);
                                }
                            }

                            // Phase 11: Reassignment invalidation.
                            // When a non-Copy owning variable is reassigned, all existing
                            // references borrowing from the old value become dangling.
                            let is_copy = self.scopes.get_def(def_id).type_id
                                .map_or(false, |tid| is_copy_type(tid, self.types, self.scopes));
                            if !is_copy {
                                let source_name = self.scopes.get_def(def_id).name.clone();
                                let dependents: Vec<DefId> = self.var_origins.iter()
                                    .filter(|(vid, origin)| **vid != def_id && origin.references_def(def_id))
                                    .map(|(vid, _)| *vid)
                                    .collect();
                                for dep_id in dependents {
                                    self.reassignment_invalidated.insert(dep_id, (source_name.clone(), value.span));
                                }
                            }

                            // Update origin tracking for reference-typed variables
                            if self.var_origins.contains_key(&def_id) {
                                let origin = self.compute_expr_origin(value);
                                self.var_origins.insert(def_id, origin);
                            } else {
                                // Phase 10: Insert fresh origin for callable-typed reassignments
                                let is_callable = self.scopes.get_def(def_id).type_id
                                    .map_or(false, |tid| types::is_callable_type(tid, self.types));
                                if is_callable || matches!(&value.node, Expr::Closure { .. }) {
                                    let origin = self.compute_expr_origin(value);
                                    self.var_origins.insert(def_id, origin);
                                }
                            }
                        }
                    }
                    // For field/index assignments, check the base object
                    _ => {
                        // Check for mutation through a bare (immutable) parameter
                        if let Some(param_name) = self.check_bare_param_mutation(target) {
                            self.error(
                                SemanticErrorKind::MutationOfBareParam {
                                    name: param_name,
                                    detail: "use `&` to declare a mutable parameter".to_string(),
                                },
                                target.span,
                            );
                        }
                        // Track mutation of `&` params
                        self.mark_mut_param_if_applicable(target);
                        self.check_expr(target);
                    }
                }
            }

            Stmt::CompoundAssign { target, value, .. } => {
                // Track reassignment for const promotion and `&` param mutation
                if let Expr::Identifier(_) = &target.node {
                    if let Some(&def_id) = self.resolution_map.get(&target.span.start) {
                        self.var_reassigned.insert(def_id);
                        let def = self.scopes.get_def(def_id);
                        if def.is_param && def.param_ownership == Some(Ownership::MutableBorrow) {
                            self.mut_param_mutated.insert(def_id);
                        }
                    }
                }
                // Track compound assignment to field/index on `&` param
                self.mark_mut_param_if_applicable(target);
                // Check immutability/const constraints on identifier targets
                if let Expr::Identifier(target_var_name) = &target.node {
                    if let Some(&def_id) = self.resolution_map.get(&target.span.start) {
                        let def = self.scopes.get_def(def_id);
                        if def.kind == DefKind::Const {
                            self.error(
                                SemanticErrorKind::AssignmentToConst { name: def.name.clone() },
                                target.span,
                            );
                        }
                        // B5: MutCallable aliasing — writing to a variable while it is
                        // mutably captured by a live closure is unsound.
                        if let Some(entries) = self.mut_captured_vars.get(&def_id) {
                            if let Some((closure_name, _, _)) = entries.first() {
                                let closure_name = closure_name.clone();
                                self.error(
                                    SemanticErrorKind::WriteWhileMutCaptured {
                                        var_name: target_var_name.clone(),
                                        closure_name,
                                    },
                                    target.span,
                                );
                            }
                        }
                        // Track writes to shared variables for CFA
                        if self.shared_var_defs.contains_key(&def_id) {
                            self.shared_written.insert(def_id);
                        }

                        // §3.6 Stale write-back: compound assign to shared/with-tracked target
                        if self.current_function_is_async {
                            let is_shared_target = self.shared_var_defs.contains_key(&def_id)
                                || self.with_shared_tracked.contains(&def_id);
                            if is_shared_target {
                                let target_shared_name = if let Some((_, name, _)) = self.shared_var_defs.get(&def_id) {
                                    name.clone()
                                } else {
                                    self.scopes.get_def(def_id).name.clone()
                                };
                                if let Some((info, _use_span)) = self.find_stale_in_condition(value) {
                                    self.stale_warnings.push(crate::semantic::errors::SemanticWarning {
                                        kind: crate::semantic::errors::SemanticWarningKind::StaleSharedWriteBack {
                                            local_name: info.local_name,
                                            source_shared_name: info.shared_name,
                                            target_shared_name,
                                            derivation_span: Some(info.derivation_span),
                                            yield_span: info.await_span,
                                        },
                                        span: value.span,
                                    });
                                }
                            }
                        }
                    }
                }
                // Check for mutation through a bare (immutable) parameter
                if let Some(param_name) = self.check_bare_param_mutation(target) {
                    self.error(
                        SemanticErrorKind::MutationOfBareParam {
                            name: param_name,
                            detail: "use `&` to declare a mutable parameter".to_string(),
                        },
                        target.span,
                    );
                }
                self.check_expr(target);
                self.check_expr(value);
            }

            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    let saved_in_return = self.in_return_expr;
                    self.in_return_expr = true;
                    self.check_expr(expr);
                    self.in_return_expr = saved_in_return;

                    // Arena escape check: cannot return arena-scoped values
                    if self.arena_depth > 0 {
                        if let Expr::Identifier(name) = &expr.node {
                            if let Some(&def_id) = self.resolution_map.get(&expr.span.start) {
                                if self.arena_scoped_vars.contains(&def_id) {
                                    self.error(
                                        SemanticErrorKind::ArenaEscape {
                                            name: name.clone(),
                                            kind: ArenaEscapeKind::Return,
                                        },
                                        expr.span,
                                    );
                                }
                            }
                        }
                    }

                    // Lifetime check: if the function returns a reference or callable type,
                    // verify the return expression doesn't reference local data.
                    // Skip for imported module functions: their borrow safety is validated by
                    // their own project's checker, and cross-module origin tracking has
                    // limitations (built-in methods like unwrap() aren't in method_resolutions,
                    // causing false Local origins for Result/Option variables).
                    if self.imported_module_depth == 0 {
                    if let Some(ret_type_id) = self.current_return_type_id {
                        if types::is_reference_type(ret_type_id, self.types, &self.ref_type_structs)
                            || types::is_callable_type(ret_type_id, self.types)
                        {
                            let origin = self.compute_expr_origin(expr);
                            if origin.contains_local() {
                                let return_name = match &expr.node {
                                    Expr::Identifier(n) => n.clone(),
                                    _ => "<expression>".to_string(),
                                };
                                if origin.contains_unknown() && !matches!(&origin, BorrowOrigin::Local(_)) {
                                    self.error(
                                        SemanticErrorKind::UnresolvedBorrowOrigin {
                                            name: return_name,
                                        },
                                        expr.span,
                                    );
                                } else {
                                    let local_names = origin.local_names(self.scopes);
                                    let local_name = local_names.first().cloned().unwrap_or_else(|| "<local>".to_string());
                                    let local_declared_at = origin.source_def_ids().first()
                                        .map(|&did| self.scopes.get_def(did).span);
                                    self.error(
                                        SemanticErrorKind::DanglingReturn {
                                            name: return_name,
                                            local_name,
                                            local_declared_at,
                                        },
                                        expr.span,
                                    );
                                }
                            }
                        }
                    }
                    } // imported_module_depth == 0

                    // Borrowed resource params cannot be returned — shallow copy
                    // shares the caller's heap data, freed at caller scope exit.
                    if let Expr::Identifier(_) = &expr.node {
                        if let Some((param_name, kind)) = self.check_non_owned_param_move(expr) {
                            self.error(
                                SemanticErrorKind::MutationOfBareParam {
                                    name: param_name,
                                    detail: format!("cannot return {kind} parameter — use `!` to transfer ownership"),
                                },
                                expr.span,
                            );
                        }
                    }

                    // Check if returning a closure (or a struct/array containing one)
                    // that captures local variables — use-after-free.
                    self.check_return_for_escaping_closures(expr);
                }
                self.diverged = true;
            }

            Stmt::Throw(expr) => {
                self.check_expr(expr);
                self.diverged = true;
            }

            Stmt::Break(expr) => {
                if let Some(expr) = expr {
                    self.check_expr(expr);
                }
                self.diverged = true;
            }

            Stmt::Continue => {
                self.diverged = true;
            }

            Stmt::Pass => {}

            Stmt::For {
                pattern,
                iterable,
                body,
                else_body,
                ..
            } => {
                self.check_expr(iterable);

                // Track borrow origins for for-loop pattern bindings.
                // The iterable origin propagates to pattern variables so that
                // reference-type bindings (str, Slice, etc.) are tracked for
                // dangling return detection.
                let iterable_origin = self.compute_expr_origin(iterable);
                self.mark_pattern_origins(pattern, &iterable_origin);

                // §3.7 Iterator invalidation: track if iterable is a with-tracked shared
                let iter_guard = if self.current_function_is_async {
                    let names = self.find_with_tracked_in_condition(iterable);
                    if names.is_empty() { None } else { Some((names, iterable.span, WithGuardKind::Iteration)) }
                } else {
                    None
                };
                if let Some(ref guard) = iter_guard {
                    self.with_guarded_conditions.push(guard.clone());
                }

                let before = self.save_branch_state();
                let saved_in_return = self.in_return_expr;
                self.in_return_expr = false;
                self.loop_depth += 1;
                self.loop_local_defs.push(FxHashSet::default());
                self.check_block(body);
                self.loop_local_defs.pop();
                self.loop_depth -= 1;
                self.in_return_expr = saved_in_return;

                if iter_guard.is_some() {
                    self.with_guarded_conditions.pop();
                }

                let after_body = self.save_branch_state();
                if let Some(else_body) = else_body {
                    // for-else: else only runs if body never does (0 iterations)
                    self.restore_branch_state(&before);
                    self.check_block(else_body);
                    let after_else = self.save_branch_state();
                    self.merge_branch_states(&[after_body, after_else]);
                } else {
                    // Body may execute 0+ times: merge pre-loop with post-body
                    self.merge_branch_states(&[before, after_body]);
                }
            }

            Stmt::While {
                condition,
                body,
                else_body,
            } => {
                self.check_expr(condition);
                self.check_stale_condition(condition);
                // Mark borrow origins for all `is` pattern bindings (including compound conditions)
                self.mark_compound_is_origins(&condition.node);

                // §3.5 Check-then-act: track if condition depends on with-tracked shared
                let with_guard = if self.current_function_is_async {
                    let names = self.find_with_tracked_in_condition(condition);
                    if names.is_empty() { None } else { Some((names, condition.span, WithGuardKind::BranchCondition)) }
                } else {
                    None
                };
                if let Some(ref guard) = with_guard {
                    self.with_guarded_conditions.push(guard.clone());
                }

                let before = self.save_branch_state();
                let saved_in_return = self.in_return_expr;
                self.in_return_expr = false;
                self.loop_depth += 1;
                self.loop_local_defs.push(FxHashSet::default());
                self.check_block(body);
                self.loop_local_defs.pop();
                self.loop_depth -= 1;
                self.in_return_expr = saved_in_return;

                if with_guard.is_some() {
                    self.with_guarded_conditions.pop();
                }

                let after_body = self.save_branch_state();
                if let Some(else_body) = else_body {
                    self.restore_branch_state(&before);
                    self.check_block(else_body);
                    let after_else = self.save_branch_state();
                    self.merge_branch_states(&[after_body, after_else]);
                } else {
                    self.merge_branch_states(&[before, after_body]);
                }
            }

            Stmt::Loop { body } => {
                let before = self.save_branch_state();
                let saved_in_return = self.in_return_expr;
                self.in_return_expr = false;
                self.loop_depth += 1;
                self.loop_local_defs.push(FxHashSet::default());
                self.check_block(body);
                self.loop_local_defs.pop();
                self.loop_depth -= 1;
                self.in_return_expr = saved_in_return;
                let after_body = self.save_branch_state();
                // Infinite loop, but break can exit: merge pre+post for break paths
                self.merge_branch_states(&[before, after_body]);
            }

            Stmt::If {
                condition,
                then_body,
                elif_branches,
                else_body,
            } => {
                self.check_expr(condition);
                self.check_stale_condition(condition);
                // Mark borrow origins for all `is` pattern bindings (including compound conditions)
                self.mark_compound_is_origins(&condition.node);

                // §3.5 Check-then-act: track if condition depends on with-tracked shared
                let with_guard = if self.current_function_is_async {
                    let names = self.find_with_tracked_in_condition(condition);
                    if names.is_empty() { None } else { Some((names, condition.span, WithGuardKind::BranchCondition)) }
                } else {
                    None
                };
                if let Some(ref guard) = with_guard {
                    self.with_guarded_conditions.push(guard.clone());
                }

                let before = self.save_branch_state();
                self.check_block(then_body);
                let mut branch_states = vec![self.save_branch_state()];

                if with_guard.is_some() {
                    self.with_guarded_conditions.pop();
                }

                for (cond, body) in elif_branches {
                    self.restore_branch_state(&before);
                    self.check_expr(cond);
                    self.check_stale_condition(cond);
                    self.mark_compound_is_origins(&cond.node);

                    let elif_guard = if self.current_function_is_async {
                        let names = self.find_with_tracked_in_condition(cond);
                        if names.is_empty() { None } else { Some((names, cond.span, WithGuardKind::BranchCondition)) }
                    } else {
                        None
                    };
                    if let Some(ref guard) = elif_guard {
                        self.with_guarded_conditions.push(guard.clone());
                    }

                    self.check_block(body);
                    branch_states.push(self.save_branch_state());

                    if elif_guard.is_some() {
                        self.with_guarded_conditions.pop();
                    }
                }

                if let Some(else_body) = else_body {
                    self.restore_branch_state(&before);
                    self.check_block(else_body);
                    branch_states.push(self.save_branch_state());
                } else {
                    branch_states.push(before);
                }

                self.merge_branch_states(&branch_states);
            }

            Stmt::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                self.check_expr(scrutinee);
                self.check_stale_condition(scrutinee);
                let scrutinee_origin = self.compute_expr_origin(scrutinee);

                // Mark matched Option/Result as Checked — destructuring proves variant handling
                if let Expr::Identifier(_) = &scrutinee.node {
                    if let Some(&scr_def_id) = self.resolution_map.get(&scrutinee.span.start) {
                        if self.fallible_states.contains_key(&scr_def_id) {
                            self.fallible_states.insert(scr_def_id, FallibleState::Checked);
                        }
                    }
                }

                // §3.5 Check-then-act: track if scrutinee depends on with-tracked shared
                let with_guard = if self.current_function_is_async {
                    let names = self.find_with_tracked_in_condition(scrutinee);
                    if names.is_empty() { None } else { Some((names, scrutinee.span, WithGuardKind::BranchCondition)) }
                } else {
                    None
                };
                if let Some(ref guard) = with_guard {
                    self.with_guarded_conditions.push(guard.clone());
                }

                let before = self.save_branch_state();
                let mut branch_states = Vec::new();

                for arm in arms.iter().filter_map(|i| i.arm()) {
                    self.restore_branch_state(&before);
                    self.mark_pattern_origins(&arm.pattern, &scrutinee_origin);
                    if let Some(guard) = &arm.guard {
                        self.check_expr(guard);
                    }
                    self.check_expr(&arm.body);
                    branch_states.push(self.save_branch_state());
                }

                if let Some(else_arm) = else_arm {
                    self.restore_branch_state(&before);
                    self.check_block(else_arm);
                    branch_states.push(self.save_branch_state());
                } else {
                    branch_states.push(before);
                }

                if with_guard.is_some() {
                    self.with_guarded_conditions.pop();
                }

                self.merge_branch_states(&branch_states);
            }

            Stmt::Select { arms, else_arm } => {
                let before = self.save_branch_state();
                let mut branch_states = Vec::new();

                for arm in arms {
                    self.restore_branch_state(&before);
                    match &arm.op {
                        SelectOp::Recv { channel, .. } => {
                            self.check_expr(channel);
                        }
                        SelectOp::Send { channel, value } => {
                            self.check_expr(channel);
                            self.check_expr(value);
                        }
                    }
                    self.check_block(&arm.body);
                    branch_states.push(self.save_branch_state());
                }

                if let Some(else_arm) = else_arm {
                    self.restore_branch_state(&before);
                    self.check_block(else_arm);
                    branch_states.push(self.save_branch_state());
                } else {
                    branch_states.push(before);
                }

                self.merge_branch_states(&branch_states);
            }

            Stmt::With { bindings, body } => {
                for binding in bindings {
                    self.check_expr(&binding.expr);
                }

                // Detect if any binding tracks a shared variable (bare `with x:` form).
                // These bindings are exempt from stale-condition warnings — the compiler
                // guarantees they are auto-refreshed after every await point.
                if self.current_function_is_async {
                    for binding in bindings {
                        if let Some(_shared_name) = self.find_shared_ref_in_expr_spanned(&binding.expr) {
                            if let Some(def_id) = self.scopes.lookup_def_by_span(&binding.name.node, binding.name.span) {
                                self.with_shared_tracked.insert(def_id);
                            }
                        }
                    }
                }

                // Detect if any binding is an allocator type (Arena or TrackingAllocator)
                let is_arena_with = bindings.iter().any(|b| {
                    self.scopes.lookup_def_by_span(&b.name.node, b.name.span)
                        .map_or(false, |did| self.is_allocator_type(did))
                });

                if is_arena_with {
                    self.arena_depth += 1;
                }
                self.with_depth += 1;
                self.check_block(body);
                self.with_depth -= 1;
                if is_arena_with {
                    self.arena_depth -= 1;
                }
            }

            Stmt::Unsafe { body } => {
                self.check_block(body);
            }

            Stmt::NamedScope { body, .. } => {
                self.check_block(body);
            }

            Stmt::Assert { condition, message } => {
                self.check_expr(condition);
                if let Some(msg) = message {
                    self.check_expr(msg);
                }
            }
            Stmt::AssertReturn { condition, message } => {
                self.check_expr(condition);
                if let Some(msg) = message {
                    self.check_expr(msg);
                }
            }

            Stmt::Snapshot { value, .. } => {
                self.check_expr(value);
            }

            Stmt::Item(_) => {}

            Stmt::MetaIf { then_body, elif_branches, else_body, .. } => {
                // Conditions are meta expressions not evaluated by borrow checker.
                // Walk bodies conservatively (all branches are potentially live).
                self.check_block(then_body);
                for (_, body) in elif_branches {
                    self.check_block(body);
                }
                if let Some(eb) = else_body {
                    self.check_block(eb);
                }
            }

            Stmt::MetaFor { body, .. } => {
                // Range is a meta expression: skip; just check the body.
                self.check_block(body);
            }

            Stmt::MetaMatch { arms, else_arm, .. } => {
                // Scrutinee and case exprs are meta expressions not evaluated by borrow checker.
                // Walk bodies conservatively (all branches are potentially live).
                for (_, body) in arms {
                    self.check_block(body);
                }
                if let Some(eb) = else_arm {
                    self.check_block(eb);
                }
            }

            Stmt::MetaWhile { body, .. } => {
                // Condition is a meta expression: skip; just check the body.
                self.check_block(body);
            }

            Stmt::MetaConst { .. } => {
                // Entirely a meta expression — evaluated at monomorphization time; skip.
            }

            Stmt::MetaLog { .. } => {
                // Compile-time diagnostic — removed before GIR lowering; skip.
            }
            Stmt::OnError { body } => {
                self.check_block(body);
            }
        }
    }

    pub(super) fn check_block(&mut self, block: &Block) {
        // Collect closure variables declared in this block so we can release their
        // mutable capture locks when the block exits (closures go out of scope).
        let mut block_closure_defs: Vec<DefId> = Vec::new();

        for stmt in &block.stmts {
            // Phase 2: Unreachable code — if the previous statement unconditionally
            // diverged, any subsequent non-meta statement is unreachable.
            if self.diverged {
                // Skip meta blocks (they're compile-time constructs, not runtime code)
                let is_meta = matches!(&stmt.node,
                    Stmt::MetaFor { .. } | Stmt::MetaIf { .. }
                );
                if !is_meta {
                    self.stale_warnings.push(crate::semantic::errors::SemanticWarning {
                        kind: crate::semantic::errors::SemanticWarningKind::UnreachableCode,
                        span: stmt.span,
                    });
                    break; // Stop checking — unreachable code shouldn't trigger cascading errors
                }
            }

            self.check_stmt(stmt);

            // Track closure variables that were declared in this block.
            if let Stmt::VarDecl { pattern, value, .. } = &stmt.node {
                if matches!(&value.node, Expr::Closure { .. }) {
                    if let Pattern::Binding(name) = &pattern.node {
                        if let Some(def_id) = self.scopes.lookup_def_by_span(name, pattern.span)
                            .or_else(|| self.find_def_by_name(name))
                        {
                            block_closure_defs.push(def_id);
                        }
                    }
                }
            }
        }

        // B6: Release mutable capture locks for closures that went out of scope.
        for closure_def_id in block_closure_defs {
            if let Some(captured_def_ids) = self.mut_capture_owners.remove(&closure_def_id) {
                for captured_id in captured_def_ids {
                    if let Some(entries) = self.mut_captured_vars.get_mut(&captured_id) {
                        entries.retain(|(_, cid, _)| *cid != closure_def_id);
                        if entries.is_empty() {
                            self.mut_captured_vars.remove(&captured_id);
                        }
                    }
                }
            }
        }
    }

    /// Check if a value expression is a bare identifier of a non-Copy type (needs `!`).
    pub(super) fn check_value_needs_move(&mut self, value: &Spanned<Expr>) {
        // Destructuring binds (Pattern::Tuple) implicitly move the value,
        // like Rust's `let (x, y) = expr`. No `!` operator required.
        if self.in_destructuring_bind {
            return;
        }
        if let Expr::Identifier(_) = &value.node {
            if let Some(&def_id) = self.resolution_map.get(&value.span.start) {
                let def = self.scopes.get_def(def_id);
                // Only check local variables, not functions/types/imports.
                // Skip parameters — they're borrowed from the caller, so
                // re-binding them is just copying a pointer, not transferring ownership.
                if def.kind == DefKind::Variable && !def.is_param {
                    if let Some(type_id) = def.type_id {
                        // String unification: owned strings (owned_string_id) in VarDecl/Assign
                        // contexts create views — the target borrows from the source. Skip the
                        // move check; lifetime tracking handles dangling-pointer safety.
                        if type_id == self.types.owned_string_id {
                            return;
                        }
                        if !is_copy_type(type_id, self.types, self.scopes) {
                            self.error(
                                SemanticErrorKind::MoveWithoutOperator {
                                    name: def.name.clone(),
                                },
                                value.span,
                            );
                        }
                    }
                }
            }
        }
    }

    /// Walk compound `And` chains in conditions, marking borrow origins for each
    /// `is` sub-expression's pattern bindings.
    pub(super) fn mark_compound_is_origins(&mut self, expr: &Expr) {
        match expr {
            Expr::Is { expr: scrutinee, negated: false, pattern, .. } => {
                let origin = self.compute_expr_origin(scrutinee);
                self.mark_pattern_origins(pattern, &origin);
            }
            Expr::BinaryOp { left, op: BinaryOp::And, right } => {
                self.mark_compound_is_origins(&left.node);
                self.mark_compound_is_origins(&right.node);
            }
            _ => {}
        }
    }

    /// Walk a pattern from a match arm, mark bindings as Live, and assign
    /// origins derived from the scrutinee to any reference-type bindings.
    /// Uses span-based DefId lookup to avoid name collisions between arms.
    pub(super) fn mark_pattern_origins(&mut self, pattern: &Spanned<Pattern>, scrutinee_origin: &BorrowOrigin) {
        match &pattern.node {
            Pattern::DotShorthand { fields, .. } => {
                for field in fields {
                    self.mark_pattern_origins(field, scrutinee_origin);
                }
                return;
            }
            Pattern::Binding(name) => {
                // Use span-based lookup to find the correct DefId for this exact binding.
                // Name-based lookup (find_def_by_name) can return the wrong DefId when
                // multiple match arms bind the same name.
                let def_id = self.scopes.lookup_def_by_span(name, pattern.span)
                    .or_else(|| self.find_def_by_name(name));
                if let Some(def_id) = def_id {
                    self.mark_live(def_id);
                    // All match bindings get MatchBinding origin. In Gorget, match
                    // destructuring always moves data out of the scrutinee (no `ref`
                    // patterns), so bindings own their data. `is_ref` is false: the
                    // binding's lifetime is independent of the scrutinee's lifetime.
                    // The scrutinee_origin is still stored for diagnostics and for
                    // reference/callable bindings where the data itself is a borrow.
                    let is_ref = self.scopes.get_def(def_id).type_id
                        .map_or(false, |tid| {
                            types::is_callable_type(tid, self.types)
                            || types::is_reference_type(tid, self.types, &self.ref_type_structs)
                        });
                    self.var_origins.insert(def_id, BorrowOrigin::MatchBinding {
                        binding_def_id: def_id,
                        scrutinee_origin: Box::new(scrutinee_origin.clone()),
                        is_ref,
                    });
                }
            }
            Pattern::Constructor { fields, .. } => {
                for field in fields {
                    self.mark_pattern_origins(field, scrutinee_origin);
                }
            }
            Pattern::Tuple(elements) => {
                for elem in elements {
                    self.mark_pattern_origins(elem, scrutinee_origin);
                }
            }
            Pattern::Or(alternatives) => {
                if let Some(first) = alternatives.first() {
                    self.mark_pattern_origins(first, scrutinee_origin);
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => {}
        }
    }

    /// Mark all bindings in a pattern as Live.
    pub(super) fn mark_pattern_live_spanned(&mut self, pattern: &Spanned<Pattern>) {
        match &pattern.node {
            Pattern::DotShorthand { fields, .. } => {
                for field in fields {
                    self.mark_pattern_live_spanned(field);
                }
                return;
            }
            Pattern::Binding(name) => {
                // Use span-based lookup to find the exact DefId for this binding,
                // avoiding confusion when multiple variables share the same name
                // in different scopes within the same function.
                if let Some(def_id) = self.scopes.lookup_def_by_span(name, pattern.span) {
                    self.mark_live(def_id);
                } else if let Some(def_id) = self.find_def_by_name(name) {
                    // Fallback for cases where span doesn't match exactly
                    self.mark_live(def_id);
                }
            }
            Pattern::Constructor { fields, .. } => {
                for field in fields {
                    self.mark_pattern_live_spanned(field);
                }
            }
            Pattern::Tuple(elements) => {
                for elem in elements {
                    self.mark_pattern_live_spanned(elem);
                }
            }
            Pattern::Or(alternatives) => {
                if let Some(first) = alternatives.first() {
                    self.mark_pattern_live_spanned(first);
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => {}
        }
    }

    /// Find the DefId for a variable name by looking it up in the scope table.

    pub(super) fn check_function(&mut self, func: &FunctionDef) {
        // Reset state for each function
        self.var_states.clear();
        self.loop_depth = 0;
        self.loop_local_defs.clear();
        self.arena_depth = 0;
        self.arena_scoped_vars.clear();
        self.diverged = false;
        self.var_origins.clear();
        self.invalidated_origins.clear();
        self.current_param_def_ids.clear();
        self.active_outlives.clear();
        self.current_function_is_async = func.qualifiers.is_async;
        self.current_function_throws = func.throws.is_some();
        self.await_invalidated.clear();
        self.shared_derived.clear();
        self.stale_shared_derived.clear();
        self.closure_capture_sets.clear();
        self.vars_containing_closures.clear();
        self.pending_capture_set = None;
        self.mut_captured_vars.clear();
        self.mut_capture_owners.clear();
        self.with_guarded_conditions.clear();
        self.with_depth = 0;
        self.local_var_usage.clear();
        self.fallible_states.clear();
        self.var_reassigned.clear();
        self.mut_param_mutated.clear();
        self.current_mut_params.clear();

        // Set scope-aware lookup context for this function
        self.current_fn_scope = self.function_body_scopes
            .get(&(func.name.node.clone(), func.name.span.start))
            .copied();
        if self.current_fn_scope.is_none() {
            // Module-level or equip function — fall back to global lookup.
        }

        // Set up return type for lifetime checking
        if let Some(def_id) = self.scopes.lookup(&func.name.node) {
            if let Some(info) = self.function_info.get(&def_id) {
                self.current_return_type_id = info.return_type_id;
            }
        } else {
            self.current_return_type_id = None;
        }

        // Set up param origins for lifetime tracking
        for (i, param) in func.params.iter().enumerate() {
            if let Some(def_id) = self.find_def_by_name(&param.node.name.node) {
                self.current_param_def_ids.push((def_id, i));

                // If this param is a reference type, track its origin as Param.
                // After string unification, view string params keep StringType in AST
                // but have string_id (Str/view) DEF type_id — treat them as ref types.
                let is_ref = is_ast_type_ref(&param.node.type_.node, self.scopes, &self.ref_type_structs)
                    || self.scopes.get_def(def_id).type_id == Some(self.types.string_id);
                if is_ref {
                    self.var_origins.insert(def_id, BorrowOrigin::Param { param_index: i, def_id });
                }

                // Track Option/Result parameters as Unchecked for fallible unwrap detection
                let param_def = self.scopes.get_def(def_id);
                if let Some(type_id) = param_def.type_id {
                    if self.is_option_or_result_type(type_id).is_some() {
                        self.fallible_states.insert(def_id, FallibleState::Unchecked);
                    }
                }

                // Track `&` (MutableBorrow) parameters for needless-mut detection
                if param.node.ownership == Ownership::MutableBorrow {
                    self.current_mut_params.push((def_id, param.node.name.node.clone(), param.node.name.span));
                }
            }
        }

        match &func.body {
            FunctionBody::Block(block) => {
                self.check_block(block);
            }
            FunctionBody::Expression(expr) => {
                // Expression-body functions: also validate dangling returns.
                // Skip for imported module functions (same rationale as Stmt::Return above).
                if self.imported_module_depth == 0 {
                if let Some(ret_type_id) = self.current_return_type_id {
                    if types::is_reference_type(ret_type_id, self.types, &self.ref_type_structs)
                        || types::is_callable_type(ret_type_id, self.types)
                    {
                        let origin = self.compute_expr_origin(expr);
                        if origin.contains_local() {
                            let return_name = match &expr.node {
                                Expr::Identifier(n) => n.clone(),
                                _ => "<expression>".to_string(),
                            };
                            if origin.contains_unknown() && !matches!(&origin, BorrowOrigin::Local(_)) {
                                self.error(
                                    SemanticErrorKind::UnresolvedBorrowOrigin {
                                        name: return_name,
                                    },
                                    expr.span,
                                );
                            } else {
                                let local_names = origin.local_names(self.scopes);
                                let local_name = local_names.first().cloned().unwrap_or_else(|| "<local>".to_string());
                                let local_declared_at = origin.source_def_ids().first()
                                    .map(|&did| self.scopes.get_def(did).span);
                                self.error(
                                    SemanticErrorKind::DanglingReturn {
                                        name: return_name,
                                        local_name,
                                        local_declared_at,
                                    },
                                    expr.span,
                                );
                            }
                        }
                    }
                }
                } // imported_module_depth == 0

                // Borrowed resource params cannot be returned — shallow copy
                // shares the caller's heap data, freed at caller scope exit.
                if let Expr::Identifier(_) = &expr.node {
                    if let Some((param_name, kind)) = self.check_non_owned_param_move(expr) {
                        self.error(
                            SemanticErrorKind::MutationOfBareParam {
                                name: param_name,
                                detail: format!("cannot return {kind} parameter — use `!` to transfer ownership"),
                            },
                            expr.span,
                        );
                    }
                }

                self.check_expr(expr);
                // Also check closure escape for expression-body functions.
                self.check_return_for_escaping_closures(expr);
            }
            FunctionBody::Declaration | FunctionBody::Extern(_) => {}
        }

        // Phase 3: Emit unused variable warnings (skip in imported modules)
        if self.imported_module_depth == 0 {
            for (_, (name, span, is_used)) in &self.local_var_usage {
                if !is_used && !name.starts_with('_') {
                    self.stale_warnings.push(crate::semantic::errors::SemanticWarning {
                        kind: crate::semantic::errors::SemanticWarningKind::UnusedVariable {
                            name: name.clone(),
                        },
                        span: *span,
                    });
                }
            }
        }

        // Emit needless mutable borrow warnings (skip in imported modules)
        if self.imported_module_depth == 0 {
            let mut_params = self.current_mut_params.clone();
            for (def_id, name, span) in &mut_params {
                if name.starts_with('_') { continue; }
                if self.mut_param_mutated.contains(def_id) { continue; }
                // Skip non-Copy (Move) types: `&` on a Move type is a borrow,
                // removing it would move the value and break the caller.
                let def = self.scopes.get_def(*def_id);
                if let Some(type_id) = def.type_id {
                    if !is_copy_type(type_id, self.types, self.scopes) {
                        continue;
                    }
                }
                self.stale_warnings.push(crate::semantic::errors::SemanticWarning {
                    kind: crate::semantic::errors::SemanticWarningKind::NeedlessMutableBorrow {
                        name: name.clone(),
                    },
                    span: *span,
                });
            }
        }

        // Emit const promotion warnings (opt-in via --warn-const, skip in imported modules)
        if self.warn_const && self.imported_module_depth == 0 {
            for (def_id, (name, span, _)) in &self.local_var_usage {
                if name.starts_with('_') { continue; }
                if self.var_reassigned.contains(def_id) { continue; }
                let def = self.scopes.get_def(*def_id);
                if def.kind == DefKind::Const { continue; }
                if def.is_param { continue; }
                // Only warn for Copy types — non-Copy types can't be const
                let is_copy = def.type_id
                    .map_or(false, |tid| is_copy_type(tid, self.types, self.scopes));
                if !is_copy { continue; }
                self.stale_warnings.push(crate::semantic::errors::SemanticWarning {
                    kind: crate::semantic::errors::SemanticWarningKind::CouldBeConst {
                        name: name.clone(),
                    },
                    span: *span,
                });
            }
        }

    }
}
