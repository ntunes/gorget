use rustc_hash::FxHashSet;

use crate::parser::ast::*;
use crate::span::Spanned;

use crate::semantic::errors::{ArenaEscapeKind, MoveReason, MoveShape, SemanticErrorKind};
use crate::semantic::ids::DefId;
use crate::semantic::scope::DefKind;
use crate::semantic::types::{self as types};

use super::{BorrowChecker, BorrowOrigin, BorrowCaptureMode, EscapeCtx, FallibleState, SharedDerivedInfo, WithGuardKind};
use super::type_utils::{is_copy_type, is_ast_type_ref, needs_explicit_move};

impl<'a> BorrowChecker<'a> {
    pub(super) fn check_stmt(&mut self, stmt: &Spanned<Stmt>) {
        match &stmt.node {
            Stmt::VarDecl {
                pattern, value, type_, shared, ..
            } => {
                // Check the value expression
                self.check_expr(value);

                // Check: if value is a bare identifier of non-Copy type, needs `!`
                // Destructuring patterns (Tuple) implicitly move the value,
                // so suppress MoveWithoutOperator for them.
                {
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

                // Mutex double-lock detection: `Guard[T] g2 = m.lock()`
                // while a prior guard from the same Mutex is still in scope.
                // Errors at compile time instead of deadlocking at runtime.
                if let Pattern::Binding(name) = &pattern.node {
                    if let Some((mutex_def, mutex_name)) = self.lock_call_target(value) {
                        if let Some((_, prior_name, prior_span)) = self.live_guards.get(&mutex_def).cloned() {
                            self.error(
                                SemanticErrorKind::MutexDoubleLock {
                                    mutex_name,
                                    prior_guard_name: prior_name,
                                    prior_lock_at: prior_span,
                                },
                                value.span,
                            );
                        } else if let Some(guard_def) = self.scopes.lookup_def_by_span(name, pattern.span)
                            .or_else(|| self.find_def_by_name(name))
                        {
                            self.live_guards.insert(mutex_def, (guard_def, name.clone(), value.span));
                        }
                    }
                }

                // Track implicit CoW borrows from collection indexing/get.
                // When `auto x = vec.get(0).unwrap()` or `auto x = vec[i]`,
                // x implicitly borrows from vec. Record this so
                // MutationWhileBorrowed can catch `vec.push(y)` while x is alive.
                if let Pattern::Binding(name) = &pattern.node {
                    if let Some(def_id) = self.scopes.lookup_def_by_span(name, pattern.span)
                        .or_else(|| self.find_def_by_name(name))
                    {
                        if let Some((root, field_path)) =
                            self.find_collection_source_with_path(value)
                        {
                            // Only track for non-Copy element types — Copy types
                            // are independent values, not borrows.
                            let elem_is_resource = self.scopes.get_def(def_id).type_id
                                .map_or(false, |tid| !is_copy_type(tid, self.types, self.scopes));
                            if elem_is_resource {
                                self.index_borrow_sources.insert(def_id,
                                    super::IndexBorrowSource { root, field_path });
                            }
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

                // Track non-Copy variables declared inside arena scope. Also
                // track buffer-owning HANDLE types (Channel/Heap): a Channel is
                // `is_copy_type`-Copy (opaque pointer) yet owns a heap buffer
                // freed at arena exit, so an arena-scoped channel must be marked
                // here or the arena-escape gate would wrongly flag a send into a
                // channel that does NOT outlive the arena (false positive).
                if self.arena_depth > 0 {
                    if let Pattern::Binding(name) = &pattern.node {
                        if let Some(def_id) = self.scopes.lookup_def_by_span(name, pattern.span)
                            .or_else(|| self.find_def_by_name(name))
                        {
                            if let Some(type_id) = self.scopes.get_def(def_id).type_id {
                                if !is_copy_type(type_id, self.types, self.scopes)
                                    || self.is_buffer_owning_type(type_id)
                                {
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
                // Dead-write lint: a mutating method call whose
                // result is discarded (statement position) is a pure write;
                // in value position the copy's data flows out (a read).
                let dw_prev = self.deadwrite_stmt_expr_start;
                self.deadwrite_stmt_expr_start = Some(expr.span.start);
                self.check_expr(expr);
                self.deadwrite_stmt_expr_start = dw_prev;
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
                // Set rebind context: while walking the RHS, `check_move`
                // treats `!x` where x is the same variable being assigned to
                // here as safe-in-loop. The next iteration's read of x sees
                // the value being assigned in this statement.
                let prev_rebind = self.assignment_rebind_target;
                if let Expr::Identifier(_) = &target.node {
                    if let Some(&did) = self.resolution_map.get(&target.span.start) {
                        self.assignment_rebind_target = Some(did);
                    }
                }
                self.check_expr(value);
                self.assignment_rebind_target = prev_rebind;

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

                // Arena escape check — ONE PRODUCER (`arena_backed_source`)
                // classifies every source shape (named arena var, fresh
                // ctor/call/method temp, f-string, literal collection,
                // concat, borrow chain — ALL materialize under the `with`
                // redirect). Destination = direct identifier target OR the
                // root of a field/tuple-field/index path
                // (`h.data = ...`, `outer[i] = ...`).
                if self.arena_depth > 0 {
                    let dest = match &target.node {
                        Expr::Identifier(name) => self
                            .resolution_map
                            .get(&target.span.start)
                            .map(|&d| (d, name.clone())),
                        Expr::FieldAccess { .. }
                        | Expr::TupleFieldAccess { .. }
                        | Expr::Index { .. } => {
                            self.find_root_def_id_with_path(target).map(|(d, _)| {
                                (d, self.scopes.get_def(d).name.clone())
                            })
                        }
                        _ => None,
                    };
                    if let Some((target_def_id, target_name)) = dest {
                        if !self.arena_scoped_vars.contains(&target_def_id) {
                            // A `c[k] = v` store into a materializing collection
                            // (map/set put) COPIES both key and value into a
                            // self-owned slot allocated from the arena — the
                            // same INGEST class as `c.put(k, v)`. Array index-
                            // store (`gorget_array_set`) writes the view
                            // directly (no copy) → stays a BIND. Decision from
                            // the typed `CollectionKind`, shared with the
                            // runtime store path — not the collection's name.
                            let ingest_index = matches!(&target.node, Expr::Index { object, .. }
                                if self
                                    .collection_kind_of_expr(object)
                                    .map_or(false, |k| k.index_store_materializes()));
                            // Copy-test fallback = the actual slot written
                            // (field/element type for a path target), not the
                            // root binding.
                            let fallback = self
                                .lvalue_value_type(target)
                                .or_else(|| self.scopes.get_def(target_def_id).type_id);
                            let hit = if ingest_index {
                                // INGEST position — route the VALUE and (for a
                                // materializing store) the KEY through the ONE
                                // shared ingest producer so this stays
                                // behaviorally identical to `c.put(k, v)`
                                // (literal rejected; arena-scoped rejected; bare
                                // live-outer non-Copy → require `!`).
                                let val_moved =
                                    matches!(&value.node, Expr::Move { .. });
                                let mut hit = self.classify_ingest_escape(
                                    value, fallback, val_moved, &target_name,
                                );
                                if hit.is_none() {
                                    if let Expr::Index { index, .. } = &target.node {
                                        let key_fallback =
                                            self.expr_types.get(&index.span).copied();
                                        let key_moved =
                                            matches!(&index.node, Expr::Move { .. });
                                        hit = self.classify_ingest_escape(
                                            index, key_fallback, key_moved, &target_name,
                                        );
                                    }
                                }
                                hit
                            } else {
                                // BIND position (identifier / field / tuple /
                                // non-materializing array index) — rule (1)
                                // only, literal exempt (a static view); a bare
                                // outer identifier here is a safe rebind/move,
                                // NOT an ingest, so rule (2) must not apply.
                                self.arena_backed_source(value, fallback, EscapeCtx::Bind)
                                    .map(|(name, span)| {
                                        (
                                            SemanticErrorKind::ArenaEscape {
                                                name,
                                                kind: ArenaEscapeKind::AssignOuter {
                                                    target: target_name.clone(),
                                                },
                                            },
                                            span,
                                        )
                                    })
                            };
                            if let Some((kind, span)) = hit {
                                self.error(kind, span);
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
                        // Dead-write lint: a full rebind (`p = expr`)
                        // detaches the name from the caller's value — Python
                        // behaves identically (rebind never writes through), so
                        // this is NOT the CoW footgun. Retire tracking.
                        self.deadwrite_params.remove(&def_id);
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
                        // Track mutation of `&` params
                        self.mark_mut_param_if_applicable(target);
                        // 2T: the SEMANTIC taint gate fires on the UNFILTERED
                        // root (incl. `self` / `_`-named params) — never on
                        // the lint's tracking subset.
                        if let Some(root) = self.find_root_def_id(target) {
                            self.reject_tainted_materialize_on_write(root, target.span);
                        }
                        // Dead-write lint: the target walk is a
                        // write-position read of the root, not a use of the
                        // materialized copy.
                        let dw_root = self
                            .find_root_def_id(target)
                            .filter(|d| self.deadwrite_params.contains_key(d));
                        let dw_prev = self.deadwrite_write_root;
                        if dw_root.is_some() {
                            self.deadwrite_write_root = dw_root;
                        }
                        self.check_expr(target);
                        self.deadwrite_write_root = dw_prev;
                        if let Some(root) = dw_root {
                            self.mark_bare_param_write_def(root, target.span);
                        }
                    }
                }
            }

            Stmt::CompoundAssign { target, value, .. } => {
                // Arena escape check — sibling of the Assign gate. A
                // non-Copy compound assign (`outer += "-more"`) lowers to a
                // fresh materialization of the combined value; under the
                // `with` redirect that lands in the arena, so an
                // outer-rooted target dangles at block exit (ASan-verified
                // UAF) regardless of the RHS shape.
                if self.arena_depth > 0 {
                    let dest = match &target.node {
                        Expr::Identifier(name) => self
                            .resolution_map
                            .get(&target.span.start)
                            .map(|&d| (d, name.clone())),
                        Expr::FieldAccess { .. }
                        | Expr::TupleFieldAccess { .. }
                        | Expr::Index { .. } => {
                            self.find_root_def_id_with_path(target).map(|(d, _)| {
                                (d, self.scopes.get_def(d).name.clone())
                            })
                        }
                        _ => None,
                    };
                    if let Some((target_def_id, target_name)) = dest {
                        if !self.arena_scoped_vars.contains(&target_def_id) {
                            // VALUE dimension: `c[k] += v` stores the COMBINED
                            // temporary (`c[k] op v`), materialized iff the
                            // TARGET's value type is non-Copy — this is a
                            // materialize test on the stored temporary, NOT an
                            // ingest of the `v` operand, so it stays a direct
                            // Copy-test (which also catches `String += char`,
                            // where the operand is Copy but the result is not).
                            // `outer[i] += 1` mutates an int element in place
                            // (Copy → safe); `c.n += 5` mutates a Copy int
                            // field. The typechecker records no `expr_types`
                            // entry at field/tuple/index target spans, so
                            // resolve the lvalue value type structurally.
                            let fallback = self.lvalue_value_type(target);
                            let mut hit = if !self.expr_value_is_copy(target, fallback) {
                                Some((
                                    SemanticErrorKind::ArenaEscape {
                                        name: "<arena-allocated temporary>".to_string(),
                                        kind: ArenaEscapeKind::AssignOuter {
                                            target: target_name.clone(),
                                        },
                                    },
                                    value.span,
                                ))
                            } else {
                                None
                            };
                            // KEY dimension: a materializing index-store
                            // (`outer["newkey"] += 5`) copies the KEY into an
                            // owned arena slot exactly like `d[k] = v` does —
                            // route it through the SAME `classify_ingest_escape`
                            // producer so the two spellings agree (the int
                            // value being Copy must NOT hide the escaping key).
                            if hit.is_none() {
                                if let Expr::Index { object, index } = &target.node {
                                    if self
                                        .collection_kind_of_expr(object)
                                        .map_or(false, |k| k.index_store_materializes())
                                    {
                                        let key_fallback =
                                            self.expr_types.get(&index.span).copied();
                                        let key_moved =
                                            matches!(&index.node, Expr::Move { .. });
                                        hit = self.classify_ingest_escape(
                                            index, key_fallback, key_moved, &target_name,
                                        );
                                    }
                                }
                            }
                            if let Some((kind, span)) = hit {
                                self.error(kind, span);
                            }
                        }
                    }
                }
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
                // 2T: unfiltered-root taint gate (sibling of the Assign site).
                if let Some(root) = self.find_root_def_id(target) {
                    self.reject_tainted_materialize_on_write(root, target.span);
                }
                // Dead-write lint: compound assign mutates through
                // the target root (e.g. `p[0] += 1`, `p.f += x`, `p += x` on a
                // resource). The target walk is a write-position read; the RHS
                // walk stays a genuine (pre-write) read.
                let dw_root = self
                    .find_root_def_id(target)
                    .filter(|d| self.deadwrite_params.contains_key(d));
                let dw_prev = self.deadwrite_write_root;
                if dw_root.is_some() {
                    self.deadwrite_write_root = dw_root;
                }
                self.check_expr(target);
                self.deadwrite_write_root = dw_prev;
                self.check_expr(value);
                if let Some(root) = dw_root {
                    self.mark_bare_param_write_def(root, target.span);
                }
            }

            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    let saved_in_return = self.in_return_expr;
                    self.in_return_expr = true;
                    self.check_expr(expr);
                    self.in_return_expr = saved_in_return;

                    // D4/D12 position 4 (return): a bare return of a live
                    // drop-tainted PLACE is an implicit copy at the return
                    // ownership boundary — require `return !place` (or
                    // `.clone()`). Fresh temps (`return R(1)`) are not places
                    // and move. Mirrors ggdef position 4
                    // (spec/ggdef/src/elaborate/mod.rs:689-690).
                    if self.imported_module_depth == 0 {
                        if let Some((name, shape)) = self.tainted_place_name(expr) {
                            self.error(
                                SemanticErrorKind::MoveWithoutOperator {
                                    name,
                                    reason: MoveReason::DropTaint,
                                    shape,
                                    // return boundary: no write-through hint.
                                    write_through_available: false,
                                },
                                expr.span,
                            );
                        }
                    }

                    // Arena escape check — ONE PRODUCER (`arena_backed_source`)
                    // for every returned-source shape. One extra rule the
                    // producer can't decide alone: a bare borrow-param return
                    // (`return v` where `v` is a non-`!` param) CLONES at the
                    // return ownership boundary, and under the redirect that
                    // clone lands in the arena (ASan-verified UAF). A bare
                    // LOCAL at `return` always moves (ASan-verified safe), so
                    // only params are gated here.
                    if self.arena_depth > 0 {
                        let param_clone_escape = if let Expr::Identifier(name) = &expr.node {
                            self.resolution_map.get(&expr.span.start).and_then(|&d| {
                                let def = self.scopes.get_def(d);
                                let non_copy = def.type_id.map_or(false, |t| {
                                    !is_copy_type(t, self.types, self.scopes)
                                });
                                if def.is_param
                                    && def.param_ownership != Some(Ownership::Move)
                                    && non_copy
                                {
                                    Some((name.clone(), expr.span))
                                } else {
                                    None
                                }
                            })
                        } else {
                            None
                        };
                        if let Some((name, span)) = param_clone_escape.or_else(|| {
                            self.arena_backed_source(
                                expr,
                                self.current_return_type_id,
                                EscapeCtx::Bind,
                            )
                        }) {
                            self.error(
                                SemanticErrorKind::ArenaEscape {
                                    name,
                                    kind: ArenaEscapeKind::Return,
                                },
                                span,
                            );
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

            Stmt::Break => {
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

                // Track the iterable's DefId so that mutating collection
                // methods inside the loop body are rejected (iterator invalidation).
                let iter_def_id = self.find_root_def_id(iterable);
                if let Some(did) = iter_def_id {
                    self.for_loop_iterables.insert(did);
                }

                let before = self.save_branch_state();
                let saved_in_return = self.in_return_expr;
                self.in_return_expr = false;
                self.loop_depth += 1;
                self.loop_local_defs.push(FxHashSet::default());
                // Dead-write lint: unique loop id for loop-carried
                // read-after-write detection.
                let dw_loop_id = self.deadwrite_next_loop_id;
                self.deadwrite_next_loop_id += 1;
                self.deadwrite_loop_stack.push(dw_loop_id);
                self.check_block(body);
                self.deadwrite_loop_stack.pop();
                self.loop_local_defs.pop();
                self.loop_depth -= 1;
                self.in_return_expr = saved_in_return;

                if let Some(did) = iter_def_id {
                    self.for_loop_iterables.remove(&did);
                }

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
                // Dead-write lint: the while CONDITION re-evaluates
                // every iteration, so its reads are loop-carried — walk it
                // inside the same loop id as the body (a `for` iterable, by
                // contrast, evaluates once and stays outside).
                let dw_while_loop_id = self.deadwrite_next_loop_id;
                self.deadwrite_next_loop_id += 1;
                self.deadwrite_loop_stack.push(dw_while_loop_id);
                self.check_expr(condition);
                // (popped after the body walk — body writes must record this
                // id so condition reads intersect them)
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
                // Dead-write lint: unique loop id for loop-carried
                // read-after-write detection.
                let dw_loop_id = self.deadwrite_next_loop_id;
                self.deadwrite_next_loop_id += 1;
                self.deadwrite_loop_stack.push(dw_loop_id);
                self.check_block(body);
                self.deadwrite_loop_stack.pop();
                // Dead-write lint: close the while-condition loop id
                // (pushed before the condition walk above).
                self.deadwrite_loop_stack.pop();
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
                // Dead-write lint: unique loop id for loop-carried
                // read-after-write detection.
                let dw_loop_id = self.deadwrite_next_loop_id;
                self.deadwrite_next_loop_id += 1;
                self.deadwrite_loop_stack.push(dw_loop_id);
                self.check_block(body);
                self.deadwrite_loop_stack.pop();
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
        // Snapshot of live_guards entries owned at block-entry. Any new
        // entry added during this block must be removed on exit so the
        // guard's release at end-of-scope unblocks subsequent locks.
        let live_guards_at_entry: Vec<DefId> = self.live_guards.keys().copied().collect();

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

        // Release Mutex Guards that went out of scope at this block's exit.
        // Drop any live_guards entry not present at block entry — those were
        // added inside this block, so their Guard binding's scope ended here.
        let entry_set: FxHashSet<DefId> = live_guards_at_entry.into_iter().collect();
        self.live_guards.retain(|mutex_def, _| entry_set.contains(mutex_def));
    }

    /// Check if a value expression is a bare identifier of a non-Copy type (needs `!`).
    pub(super) fn check_value_needs_move(&mut self, value: &Spanned<Expr>) {
        // Destructuring binds (Pattern::Tuple) implicitly move the value,
        // like Rust's `let (x, y) = expr`. No `!` operator required.
        if self.in_destructuring_bind {
            return;
        }
        // D4/D12 position 1 (bind): a bare bind of a drop-tainted PLACE is an
        // implicit copy — require `!place` / `.clone()`. Unlike the by-design
        // single-owner set below (identifier-local-only), `tainted_place_name`
        // resolves the place's type STRUCTURALLY (via `lvalue_value_type`), so
        // it covers field/index places (`obj.field`, `v[i]`) as well as
        // identifier/self/param — mirroring ggdef's bind-position rejection
        // (spec/ggdef/src/elaborate/mod.rs:970).
        if let Some((name, shape)) = self.tainted_place_name(value) {
            self.error(
                SemanticErrorKind::MoveWithoutOperator {
                    name,
                    reason: MoveReason::DropTaint,
                    shape,
                    // bind boundary (`R b = place`): no write-through hint.
                    write_through_available: false,
                },
                value.span,
            );
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
                        // Collection types (Vector, Dict, Set) are auto-cloned on assignment —
                        // no `!` needed.
                        //
                        // **CoW-by-default for bare-assign RHS (2026-05-12).** User struct /
                        // enum non-Copy types are also routed through CoW: the IR-lowering's
                        // Phase D4 decision tree (`lower_var_decl_assign_mode` Branch C/D) lifts
                        // resource-typed user types into a Ptr alias (`set_ref`) when CoW-safe,
                        // and falls back to clone-and-Move when CoW-unsafe (assignment to the
                        // destination later). Bare-assign now mirrors the other consume
                        // positions (function params, match scrutinees, closure captures): all
                        // borrow by default with CoW-sever on mutation. Explicit `!` is kept
                        // as the use-site move opt-in; explicit `.clone()` stays for explicit
                        // independent-copy intent.
                        // Single-owner carve-out types (closures/Callable, Owned[T],
                        // Box[T], Task/TaskGroup/Guard) are not CoW-eligible — bare-assign
                        // requires explicit `!`. Shared predicate `needs_explicit_move` is
                        // the single source of truth (also used at the constructor / struct /
                        // enum init boundaries in `check_expr`).
                        if !is_copy_type(type_id, self.types, self.scopes)
                            && needs_explicit_move(type_id, self.types, self.scopes)
                        {
                            self.error(
                                SemanticErrorKind::MoveWithoutOperator {
                                    name: def.name.clone(),
                                    reason: MoveReason::SingleOwner,
                                    shape: MoveShape::Whole,
                                    // bare-assign single-owner: no write-through fix.
                                    write_through_available: false,
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

    /// Reset all per-function state shared between `check_function` and the
    /// per-Item entry points (`Item::Test` / `Bench` / `SuiteSetup` /
    /// `SuiteTeardown`). Function-specific bits (the async / throws flags,
    /// param def-ids) are set by the caller after this returns.
    ///
    /// Forgetting to reset `diverged` here used to make the FIRST statement
    /// of every test body look unreachable when the previous module item
    /// was a function ending with `return` — cosmetic warning regression
    /// reported 2026-05-05 in the JS-interpreter porting feedback.
    pub(super) fn reset_per_function_state(&mut self) {
        self.var_states.clear();
        self.loop_depth = 0;
        self.loop_local_defs.clear();
        self.arena_depth = 0;
        self.arena_scoped_vars.clear();
        self.diverged = false;
        self.var_origins.clear();
        self.invalidated_origins.clear();
        self.await_invalidated.clear();
        self.closure_capture_sets.clear();
        self.vars_containing_closures.clear();
        self.pending_capture_set = None;
        // Per-function caches that previously leaked across function bodies.
        // `index_borrow_sources` dominates `save_branch_state` cost — without
        // a per-function clear it accumulated every `.get()`/`vec[i]` binding
        // module-wide, so each branch-state save cloned a map growing linearly
        // with module size (LW: ~170 entries/save × 11k saves → ~2M clones).
        // `reassignment_invalidated` has the same semantic shape; clearing it
        // here also keeps the diagnostic state strictly per-function.
        self.index_borrow_sources.clear();
        self.reassignment_invalidated.clear();
    }

    pub(super) fn check_function(&mut self, func: &FunctionDef) {
        // Reset state for each function
        self.reset_per_function_state();
        self.current_param_def_ids.clear();
        self.current_function_is_async = func.qualifiers.is_async;
        self.current_function_throws = func.throws.declares_throws();
        self.shared_derived.clear();
        self.stale_shared_derived.clear();
        self.mut_captured_vars.clear();
        self.mut_capture_owners.clear();
        self.with_guarded_conditions.clear();
        self.with_depth = 0;
        self.local_var_usage.clear();
        self.fallible_states.clear();
        self.var_reassigned.clear();
        self.mut_param_mutated.clear();
        self.current_mut_params.clear();
        self.deadwrite_params.clear();
        self.deadwrite_clock = 0;
        self.deadwrite_loop_stack.clear();
        self.deadwrite_next_loop_id = 0;
        self.deadwrite_write_root = None;
        self.live_guards.clear();

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

                // Dead-write lint: track bare (Borrow) resource params for
                // DeadBareParamWrite. Exclusions (v1 non-goals, deliberate):
                // - Copy types: mutation of a by-value copy is the
                //   Python-identical rebind model, not the CoW footgun.
                // - `self` (D2-rider scout): tracked like any other bare
                //   param — a write materializes a private copy under 2E even
                //   for Copy value structs (self is always pointer-passed), so
                //   `self` skips the Copy exclusion. SelfExpr reads are marked
                //   by the check_expr SelfExpr arm, so the scratch-copy idiom
                //   stays silent. Eligibility rule = "the write MATERIALIZES
                //   a private copy": by-value Copy params never materialize;
                //   pointer-passed self always does.
                // - `_`-prefixed names (deliberate-ignore convention).
                // Also out of scope for v1: writes via `f(&p)` call args
                // (needs callee purity — §3.8 MutatesArgs — to avoid
                // false-positiving on read-only `&` callees), closure-captured
                // params, and `for x in &p` iteration.
                if param.node.ownership == Ownership::Borrow
                    && !param.node.name.node.starts_with('_')
                {
                    let is_self = param.node.name.node == "self";
                    let is_resource = param_def.type_id.map_or(false, |tid| {
                        !is_copy_type(tid, self.types, self.scopes)
                    });
                    if is_self || is_resource {
                        self.deadwrite_params.insert(def_id, super::DeadWriteInfo {
                            name: param.node.name.node.clone(),
                            param_span: param.node.name.span,
                            last_read: None,
                            last_write: None,
                            write_loops: Vec::new(),
                            read_loops: FxHashSet::default(),
                        });
                    }
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

                // D4/D12 position 4 (expression-body tail): an expression-body
                // function `R passthru(R x): x` returns the place `x` at the
                // return ownership boundary — the same implicit-copy rejection
                // as `Stmt::Return`, mirroring ggdef's expr-body tail
                // (spec/ggdef/src/elaborate/mod.rs:349-352). Fresh-temp bodies
                // (`R make(): R(7)`) are not places and move — stay legal.
                if let Some((name, shape)) = self.tainted_place_name(expr) {
                    self.error(
                        SemanticErrorKind::MoveWithoutOperator {
                            name,
                            reason: MoveReason::DropTaint,
                            shape,
                            // expr-body tail (return boundary): no write-through hint.
                            write_through_available: false,
                        },
                        expr.span,
                    );
                }
                } // imported_module_depth == 0

                self.check_expr(expr);
                // Also check closure escape for expression-body functions.
                self.check_return_for_escaping_closures(expr);
            }
            FunctionBody::Declaration | FunctionBody::Extern(_) => {}
        }

        // Export borrow dependency data for drop ordering.
        // For each reference-typed local, record which locals it borrows from.
        for (borrower_id, origin) in &self.var_origins {
            let sources: Vec<DefId> = origin.source_def_ids()
                .into_iter()
                .filter(|&did| did != *borrower_id) // no self-references
                .collect();
            if !sources.is_empty() {
                self.borrow_deps.entry(*borrower_id).or_default().extend(sources);
            }
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

        // Emit dead-write warnings (DeadBareParamWrite): a bare (borrow)
        // resource param was mutated — materializing a private CoW copy
        // (docs/language-design.md §3.1-3.2) — and that copy is never read
        // afterwards. The write is dead: the caller's value is unchanged, so
        // the user almost certainly meant `&param` (write-through).
        // Skipped in imported modules like the other per-function warnings;
        // GG_DEADWRITE_ALL=1 is a diagnostic-only bypass of that gate, kept
        // for corpus sweeps (measuring the lint over merged stdlib/self-host
        // modules) — it changes nothing for normal builds.
        let deadwrite_all = std::env::var("GG_DEADWRITE_ALL").is_ok();
        if self.imported_module_depth == 0 || deadwrite_all {
            let mut entries: Vec<(String, crate::span::Span, crate::span::Span)> =
                self.deadwrite_params.values()
                    .filter_map(|info| {
                        let (wclock, wspan) = info.last_write?;
                        // A genuine read after the last write → scratch copy, OK.
                        if info.last_read.map_or(false, |r| r > wclock) {
                            return None;
                        }
                        // Loop-carried: a read anywhere inside a loop that also
                        // contains the write dynamically follows it on later
                        // iterations.
                        if info.write_loops.iter().any(|l| info.read_loops.contains(l)) {
                            return None;
                        }
                        Some((info.name.clone(), wspan, info.param_span))
                    })
                    .collect();
            // deadwrite_params is a hash map — sort by mutation site for
            // deterministic emission order.
            entries.sort_by_key(|(_, wspan, _)| (wspan.start, wspan.end));
            for (name, wspan, param_span) in entries {
                self.stale_warnings.push(crate::semantic::errors::SemanticWarning {
                    kind: crate::semantic::errors::SemanticWarningKind::DeadBareParamWrite {
                        name,
                        param_span,
                    },
                    span: wspan,
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
