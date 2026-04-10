use rustc_hash::FxHashSet;

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use crate::semantic::errors::SemanticErrorKind;
use crate::semantic::ids::DefId;
use crate::semantic::scope::DefKind;

use super::{BorrowChecker, BorrowOrigin, FallibleState, VarState};
use super::type_utils::is_copy_type;

impl<'a> BorrowChecker<'a> {
    pub(super) fn check_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            // Literals — no ownership concerns
            Expr::IntLiteral(_)
            | Expr::FloatLiteral(_)
            | Expr::BoolLiteral(_)
            | Expr::NoneLiteral
            | Expr::SelfExpr
            | Expr::It => {}

            Expr::StringLiteral(lit) => {
                // Re-parse and check interpolation expressions for borrow safety.
                // Interpolations are stored as raw strings; the borrow checker must
                // parse them to catch use-after-move, bare param mutation, etc.
                for seg in &lit.segments {
                    if let crate::lexer::token::StringSegment::Interpolation(var_name, _) = seg {
                        // Try to parse as a full expression
                        if let Ok(parsed_expr) = crate::parser::Parser::new(var_name).parse_expr() {
                            // Re-resolve identifiers against current scope.
                            // Use the f-string's span for error reporting since the
                            // re-parsed expression has synthetic spans.
                            self.check_interpolation_expr(&parsed_expr, expr.span);
                        } else {
                            // Fallback: mark root variable as used
                            let root = var_name.split(['.', '(', '[']).next().unwrap_or(var_name);
                            if let Some(def_id) = self.find_def_by_name(root) {
                                if let Some(entry) = self.local_var_usage.get_mut(&def_id) {
                                    entry.2 = true;
                                }
                            }
                        }
                    }
                }
            }

            Expr::Identifier(var_name) => {
                // Check that the variable is still live
                if let Some(&def_id) = self.resolution_map.get(&expr.span.start) {
                    // Phase 3: Mark variable as used
                    if let Some(entry) = self.local_var_usage.get_mut(&def_id) {
                        entry.2 = true;
                    }
                    let kind = self.scopes.get_def(def_id).kind;
                    if kind == DefKind::Variable {
                        self.check_use(def_id, expr.span);
                        // B4: MutCallable aliasing — reading a variable while it is
                        // mutably captured by a live closure is unsound.
                        if let Some(entries) = self.mut_captured_vars.get(&def_id) {
                            if let Some((closure_name, _, _)) = entries.first() {
                                let closure_name = closure_name.clone();
                                self.error(
                                    SemanticErrorKind::ReadWhileMutCaptured {
                                        var_name: var_name.clone(),
                                        closure_name,
                                    },
                                    expr.span,
                                );
                            }
                        }
                    }
                }
            }

            Expr::Path { segments } => {
                if let Some(first) = segments.first() {
                    if let Some(&def_id) = self.resolution_map.get(&first.span.start) {
                        let kind = self.scopes.get_def(def_id).kind;
                        if kind == DefKind::Variable {
                            self.check_use(def_id, first.span);
                        }
                    }
                }
            }

            Expr::Move { expr: inner } => {
                // The `!` operator: move the value
                if let Expr::Identifier(_) = &inner.node {
                    if let Some(&def_id) = self.resolution_map.get(&inner.span.start) {
                        let kind = self.scopes.get_def(def_id).kind;
                        if kind == DefKind::Variable {
                            self.check_move(def_id, expr.span);
                        }
                    }
                } else {
                    // Move of a field/index expression (e.g., !p.field) — mark
                    // the root variable as moved since the struct is now partial.
                    if let Some(root_def_id) = self.find_root_def_id(inner) {
                        let kind = self.scopes.get_def(root_def_id).kind;
                        if kind == DefKind::Variable {
                            self.check_move(root_def_id, expr.span);
                        }
                    }
                    self.check_expr(inner);
                }
            }

            Expr::MutableBorrow { expr: inner } => {
                // The `&` operator: mutable borrow
                // Check that the inner expression is still usable
                self.check_expr(inner);
            }

            Expr::Deref { expr: inner } => {
                self.check_expr(inner);
            }

            Expr::UnaryOp { operand, .. } => {
                self.check_expr(operand);
            }

            Expr::BinaryOp { left, right, .. } => {
                self.check_expr(left);
                self.check_expr(right);
            }

            Expr::Call { callee, args, .. } => {
                // If callee is a ConsumeCallable variable, the call consumes it
                let consumed = if let Expr::Identifier(_) = &callee.node {
                    if let Some(&def_id) = self.resolution_map.get(&callee.span.start) {
                        if self.is_consume_callable_var(def_id) {
                            self.check_move(def_id, expr.span);
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                };

                if !consumed {
                    self.check_expr(callee);
                }
                self.check_call_ownership(callee, args);
                self.check_call_aliasing(args);

                // Determine if callee is a constructor (variant/newtype) —
                // their args implicitly consume non-Copy values.
                let is_constructor = self.resolve_callee_def_id(callee)
                    .map(|id| {
                        let kind = self.scopes.get_def(id).kind;
                        matches!(kind, DefKind::Variant | DefKind::Newtype)
                    })
                    .unwrap_or(false);

                for arg in args {
                    match arg.node.ownership {
                        Ownership::Move => {
                            // Argument passed with `!` — check the move
                            if let Expr::Identifier(_) = &arg.node.value.node {
                                if let Some(&def_id) =
                                    self.resolution_map.get(&arg.node.value.span.start)
                                {
                                    let kind = self.scopes.get_def(def_id).kind;
                                    if kind == DefKind::Variable {
                                        self.check_move(def_id, arg.span);
                                    }
                                }
                            } else {
                                // Field move (e.g., f(!p.field)) — mark root as moved
                                if let Some(root_def_id) = self.find_root_def_id(&arg.node.value) {
                                    let kind = self.scopes.get_def(root_def_id).kind;
                                    if kind == DefKind::Variable {
                                        self.check_move(root_def_id, arg.span);
                                    }
                                }
                                self.check_expr(&arg.node.value);
                            }
                        }
                        Ownership::MutableBorrow | Ownership::Borrow => {
                            if arg.node.ownership == Ownership::MutableBorrow {
                                // Track passing `&` param with `&` to callee as mutation
                                self.mark_mut_param_if_applicable(&arg.node.value);
                            }
                            // For constructor calls, bare non-Copy identifier
                            // args are implicitly consumed (moved into fields).
                            if is_constructor {
                                if let Expr::Identifier(_) = &arg.node.value.node {
                                    if let Some(&var_def_id) = self.resolution_map.get(&arg.node.value.span.start) {
                                        let def = self.scopes.get_def(var_def_id);
                                        if def.kind == DefKind::Variable {
                                            if let Some(type_id) = def.type_id {
                                                if !is_copy_type(type_id, self.types, self.scopes) {
                                                    let skip_implicit_move = self.loop_depth > 0
                                                        && !self.loop_local_defs.last()
                                                            .map_or(false, |s| s.contains(&var_def_id))
                                                        && self.in_return_expr;
                                                    if !skip_implicit_move {
                                                        self.check_move(var_def_id, arg.span);
                                                        continue;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            self.check_expr(&arg.node.value);
                        }
                    }
                }

                // §3.4 Stale-condition: blocking calls release the token just like await.
                if self.current_function_is_async {
                    if self.is_yield_point_call(&callee.node) {
                        let call_span = expr.span;
                        let drained: Vec<_> = self.shared_derived.drain().collect();
                        for (def_id, mut info) in drained {
                            if self.with_shared_tracked.contains(&def_id) {
                                self.shared_derived.insert(def_id, info);
                            } else {
                                info.await_span = Some(call_span);
                                self.stale_shared_derived.insert(def_id, info);
                            }
                        }

                        // §3.5 Check-then-act: yield inside a with-guarded branch
                        self.check_with_check_then_act(call_span);
                    }
                }
            }

            Expr::MethodCall {
                receiver, method, args, ..
            } => {
                self.check_expr(receiver);
                self.check_call_aliasing(args);

                // Track `&` param mutation via &self method call
                if let Some(def_id) = self.find_root_def_id(receiver) {
                    let def = self.scopes.get_def(def_id);
                    if def.is_param && def.param_ownership == Some(Ownership::MutableBorrow) {
                        if let Some(&method_def_id) = self.method_resolutions.get(&method.span.start) {
                            if let Some(info) = self.function_info.get(&method_def_id) {
                                if info.param_ownerships.first() == Some(&Ownership::MutableBorrow) {
                                    self.mut_param_mutated.insert(def_id);
                                }
                            }
                        }
                    }
                }

                // !self consuming methods: mark receiver as moved.
                // Temps (method chains, function returns) don't have a def_id
                // and are handled automatically — only named vars need checking.
                if let Expr::Identifier(_) = &receiver.node {
                    if let Some(&recv_def_id) = self.resolution_map.get(&receiver.span.start) {
                        let kind = self.scopes.get_def(recv_def_id).kind;
                        if kind == DefKind::Variable {
                            if let Some(&method_def_id) = self.method_resolutions.get(&method.span.start) {
                                if let Some(info) = self.function_info.get(&method_def_id) {
                                    if info.param_ownerships.first() == Some(&Ownership::Move) {
                                        self.check_move(recv_def_id, expr.span);
                                    }
                                }
                            }
                        }
                    }
                }

                // Borrow invalidation: mutating collection methods invalidate
                // outstanding T & borrows from the receiver. This prevents use of
                // references after the collection is structurally modified.
                // Only checks variables with explicit Ref types (from T & syntax),
                // not legacy str view tracking from provenance.
                {
                    let method_name = method.node.as_str();
                    let is_mutating_collection_method =
                        crate::ir::lowering::builtins::is_mutating_builtin_method(method_name);
                    if is_mutating_collection_method {
                        if let Some(recv_def_id) = self.find_root_def_id(receiver) {
                            let recv_name = self.scopes.get_def(recv_def_id).name.clone();
                            // Check explicit T & borrows
                            for (&var_id, origin) in self.var_origins.iter() {
                                if origin.references_def(recv_def_id) {
                                    // Only flag explicit T & borrows, not legacy str views
                                    let def = self.scopes.get_def(var_id);
                                    let is_ref_type = def.type_id.map_or(false, |tid| {
                                        matches!(self.types.get(tid), super::super::types::ResolvedType::Ref(_))
                                    });
                                    if !is_ref_type { continue; }
                                    let is_alive = !matches!(
                                        self.var_states.get(&var_id),
                                        Some(VarState::Moved { .. })
                                    );
                                    if is_alive {
                                        let var_name = self.scopes.get_def(var_id).name.clone();
                                        self.error(
                                            SemanticErrorKind::MutationWhileBorrowed {
                                                source: recv_name.clone(),
                                                borrow: var_name,
                                            },
                                            expr.span,
                                        );
                                        break;
                                    }
                                }
                            }
                            // Check implicit CoW borrows (from .get().unwrap(), vec[i]).
                            // These variables don't have explicit Ref types but still
                            // borrow from the collection via the CoW system.
                            // Emitted as a warning (not error) because the CoW system
                            // handles correctness by materializing before mutation.
                            for (&var_id, &source_collection) in self.index_borrow_sources.iter() {
                                if source_collection == recv_def_id {
                                    let is_alive = !matches!(
                                        self.var_states.get(&var_id),
                                        Some(VarState::Moved { .. })
                                    );
                                    if is_alive {
                                        let var_name = self.scopes.get_def(var_id).name.clone();
                                        self.stale_warnings.push(
                                            crate::semantic::errors::SemanticWarning {
                                                kind: crate::semantic::errors::SemanticWarningKind::CowBorrowMutation {
                                                    source: recv_name.clone(),
                                                    borrow: var_name,
                                                },
                                                span: expr.span,
                                            }
                                        );
                                        break;
                                    }
                                }
                            }
                            // Check for-loop iterator invalidation: mutating a collection
                            // currently being iterated over is always an error, regardless
                            // of element type (the for-loop shallow-copies the array struct,
                            // so reallocation from push/insert dangles the iterator).
                            if self.for_loop_iterables.contains(&recv_def_id) {
                                self.error(
                                    SemanticErrorKind::MutationWhileBorrowed {
                                        source: recv_name.clone(),
                                        borrow: format!("for-loop over `{}`", recv_name),
                                    },
                                    expr.span,
                                );
                            }
                        }
                    }
                }

                // Fallible unwrap tracking: detect guard calls and unwrap on Option/Result
                if let Expr::Identifier(_) = &receiver.node {
                    if let Some(&recv_def_id) = self.resolution_map.get(&receiver.span.start) {
                        let method_name = method.node.as_str();
                        match method_name {
                            "is_some" | "is_ok" | "is_none" | "is_err" | "is_error" => {
                                if self.fallible_states.contains_key(&recv_def_id) {
                                    self.fallible_states.insert(recv_def_id, FallibleState::Checked);
                                }
                            }
                            "unwrap" | "expect" => {
                                if let Some(&FallibleState::Unchecked) = self.fallible_states.get(&recv_def_id) {
                                    let recv_def = self.scopes.get_def(recv_def_id);
                                    let recv_name = recv_def.name.clone();
                                    let type_name = recv_def.type_id
                                        .and_then(|tid| self.is_option_or_result_type(tid))
                                        .unwrap_or_else(|| "Option".to_string());
                                    self.stale_warnings.push(crate::semantic::errors::SemanticWarning {
                                        kind: crate::semantic::errors::SemanticWarningKind::UncheckedUnwrap {
                                            name: recv_name,
                                            type_name,
                                        },
                                        span: method.span,
                                    });
                                }
                            }
                            _ => {}
                        }
                    }
                }

                // Detect qualified enum variant constructors: EnumName.VariantName(args)
                // When the receiver resolves to an Enum def, treat args as implicitly consumed.
                let is_enum_constructor = if let Expr::Identifier(_) = &receiver.node {
                    self.resolution_map.get(&receiver.span.start)
                        .map(|&def_id| self.scopes.get_def(def_id).kind == DefKind::Enum)
                        .unwrap_or(false)
                } else {
                    false
                };
                for arg in args {
                    match arg.node.ownership {
                        Ownership::Move => {
                            if let Expr::Identifier(_) = &arg.node.value.node {
                                if let Some(&def_id) =
                                    self.resolution_map.get(&arg.node.value.span.start)
                                {
                                    let kind = self.scopes.get_def(def_id).kind;
                                    if kind == DefKind::Variable {
                                        self.check_move(def_id, arg.span);
                                    }
                                }
                            } else {
                                self.check_expr(&arg.node.value);
                            }
                        }
                        Ownership::MutableBorrow | Ownership::Borrow => {
                            // Track passing `&` param with `&` to callee as mutation
                            if arg.node.ownership == Ownership::MutableBorrow {
                                self.mark_mut_param_if_applicable(&arg.node.value);
                            }
                            // For qualified enum variant constructors, bare non-Copy
                            // identifier args are implicitly consumed (moved into fields).
                            // String types are excluded: the IR clones/borrows strings into
                            // enum fields (CoW), so the source is not consumed.
                            if is_enum_constructor {
                                if let Expr::Identifier(_) = &arg.node.value.node {
                                    if let Some(&var_def_id) = self.resolution_map.get(&arg.node.value.span.start) {
                                        let def = self.scopes.get_def(var_def_id);
                                        if def.kind == DefKind::Variable && !def.is_param {
                                            if let Some(type_id) = def.type_id {
                                                let is_string = type_id == self.types.string_id
                                                    || type_id == self.types.owned_string_id;
                                                if !is_copy_type(type_id, self.types, self.scopes) && !is_string {
                                                    let skip_implicit_move = self.loop_depth > 0
                                                        && !self.loop_local_defs.last()
                                                            .map_or(false, |s| s.contains(&var_def_id))
                                                        && self.in_return_expr;
                                                    if !skip_implicit_move {
                                                        self.check_move(var_def_id, arg.span);
                                                        continue;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            self.check_expr(&arg.node.value);
                        }
                    }
                }
            }

            Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
                self.check_expr(object);
            }

            Expr::Index { object, index } => {
                self.check_expr(object);
                self.check_expr(index);
            }

            Expr::Range { start, end, .. } => {
                if let Some(s) = start {
                    self.check_expr(s);
                }
                if let Some(e) = end {
                    self.check_expr(e);
                }
            }

            Expr::OptionalChain { object, .. } => {
                self.check_expr(object);
            }

            Expr::DefaultOp { lhs, rhs } => {
                self.check_expr(lhs);
                self.check_expr(rhs);
            }

            Expr::Await { expr: inner } => {
                self.check_expr(inner);
                if self.current_function_is_async {
                    let to_invalidate: Vec<DefId> = self.var_origins.iter()
                        .filter(|(_, origin)| {
                            // Static origins are always safe.
                            if matches!(origin, BorrowOrigin::Static) { return false; }
                            // Param-only origins are safe across await: the caller is blocked
                            // at the direct-await call site, so all params remain alive.
                            // This is sound because Change 1 (Spawn enforcement) prevents
                            // borrowed refs from reaching fire-and-forget spawns.
                            if !origin.contains_local() { return false; }
                            true
                        })
                        .filter_map(|(def_id, _)| {
                            // Not in var_states = never moved = implicitly live (e.g. params).
                            if !matches!(self.var_states.get(def_id), Some(VarState::Moved { .. })) {
                                Some(*def_id)
                            } else {
                                None
                            }
                        })
                        .collect();
                    for def_id in to_invalidate {
                        self.await_invalidated.insert(def_id);
                    }
                    // §3.4 Stale-condition: move shared_derived entries to stale
                    // (token released at await → cached values are stale).
                    // Skip with-tracked bindings — they are auto-refreshed.
                    let await_span = expr.span;
                    let drained: Vec<_> = self.shared_derived.drain().collect();
                    for (def_id, mut info) in drained {
                        if self.with_shared_tracked.contains(&def_id) {
                            // Re-insert: with-tracked bindings stay fresh
                            self.shared_derived.insert(def_id, info);
                        } else {
                            info.await_span = Some(await_span);
                            self.stale_shared_derived.insert(def_id, info);
                        }
                    }

                    // §3.5 Check-then-act: yield inside a with-guarded branch
                    self.check_with_check_then_act(expr.span);
                }
            }

            Expr::Spawn { expr: inner } => {
                self.check_expr(inner);
                // spawn supports:
                //   1. Direct function calls: `spawn fn_name(args)`
                //   2. Closure variable calls: `spawn c(args)` where c is a closure variable
                //   3. Inline closure calls: `spawn ((): body)(args)`
                // All other forms (method calls, indirect calls) are rejected.
                if let Expr::Call { callee, args, .. } = &inner.node {
                    // Classify the callee
                    let callee_kind = match &callee.node {
                        Expr::Identifier(_) => {
                            self.resolution_map.get(&callee.span.start)
                                .map(|&id| self.scopes.get_def(id).kind)
                        }
                        _ => None,
                    };

                    if matches!(callee_kind, Some(DefKind::Function)) {
                        // Case 1: Direct function call — check args for borrowed origins.
                        self.check_spawn_args(args);
                        let callee_did = self.resolution_map.get(&callee.span.start).copied();
                        self.cfa_at_spawn(callee_did, args);
                    } else if matches!(callee_kind, Some(DefKind::Variable)) {
                        // Case 2: Closure variable call — check capture set + args.
                        let callee_def_id = self.resolution_map.get(&callee.span.start).copied();
                        if let Some(cs) = callee_def_id.and_then(|id| self.closure_capture_sets.get(&id)) {
                            let cs = cs.clone(); // borrow checker appeasement
                            self.check_spawn_closure_captures(&cs, inner.span);
                        } else {
                            // No capture set (closure returned from function, etc.) — conservative reject.
                            self.error(SemanticErrorKind::SpawnRequiresDirectCall, inner.span);
                        }
                        self.check_spawn_args(args);
                    } else if let Expr::Closure { params, body, .. } = &callee.node {
                        // Case 3: Inline closure call — `spawn ((): body)(args)`
                        let cs = self.compute_capture_set(params, body);
                        self.check_spawn_closure_captures(&cs, inner.span);
                        self.check_spawn_args(args);
                    } else {
                        // Fallback: method calls, arbitrary expressions, etc.
                        self.error(SemanticErrorKind::SpawnRequiresDirectCall, inner.span);
                    }
                } else if let Expr::MethodCall { receiver, args, .. } = &inner.node {
                    // Case 4: Method call — spawn receiver.method(args)
                    // Validate receiver origin: owned locals and statics are safe to transfer.
                    // Reject borrowed references (Param, Unknown, etc.) that might dangle.
                    let recv_origin = self.compute_expr_origin(receiver);
                    if !matches!(recv_origin, BorrowOrigin::Static | BorrowOrigin::Local(_)) {
                        let recv_name = if let Expr::Identifier(n) = &receiver.node { Some(n.clone()) } else { None };
                        self.error(SemanticErrorKind::SpawnWithBorrowedRef { name: recv_name }, receiver.span);
                    }
                    self.check_spawn_args(args);
                } else {
                    self.error(SemanticErrorKind::SpawnRequiresDirectCall, inner.span);
                }

                // §3.8 Spawn inside `with`: warn if spawn args include with-tracked bindings.
                if self.with_depth > 0 && self.current_function_is_async {
                    if let Expr::Call { args, .. } = &inner.node {
                        for arg in args {
                            if let Expr::Identifier(name) = &arg.node.value.node {
                                if let Some(&def_id) = self.resolution_map.get(&arg.node.value.span.start) {
                                    if self.with_shared_tracked.contains(&def_id) {
                                        self.stale_warnings.push(crate::semantic::errors::SemanticWarning {
                                            kind: crate::semantic::errors::SemanticWarningKind::SpawnWithTrackedBinding {
                                                shared_name: name.clone(),
                                                spawn_span: inner.span,
                                            },
                                            span: arg.node.value.span,
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            }

            Expr::SpawnBlocking { expr: inner } => {
                self.check_expr(inner);
                if let Expr::Call { callee, args, .. } = &inner.node {
                    if matches!(&callee.node, Expr::Identifier(_)) {
                        self.check_spawn_args(args);
                    } else {
                        self.error(SemanticErrorKind::SpawnRequiresDirectCall, inner.span);
                    }
                } else {
                    self.error(SemanticErrorKind::SpawnRequiresDirectCall, inner.span);
                }
            }

            Expr::If {
                condition,
                then_branch,
                elif_branches,
                else_branch,
            } => {
                self.check_expr(condition);

                let before = self.save_branch_state();
                self.check_expr(then_branch);
                let mut branch_states = vec![self.save_branch_state()];

                for (cond, body) in elif_branches {
                    self.restore_branch_state(&before);
                    self.check_expr(cond);
                    self.check_expr(body);
                    branch_states.push(self.save_branch_state());
                }

                if let Some(else_br) = else_branch {
                    self.restore_branch_state(&before);
                    self.check_expr(else_br);
                    branch_states.push(self.save_branch_state());
                } else {
                    branch_states.push(before);
                }

                self.merge_branch_states(&branch_states);
            }

            Expr::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                self.check_expr(scrutinee);
                let scrutinee_origin = self.compute_expr_origin(scrutinee);
                let before = self.save_branch_state();
                let mut branch_states = Vec::new();

                for arm in arms {
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
                    self.check_expr(else_arm);
                    branch_states.push(self.save_branch_state());
                } else {
                    branch_states.push(before);
                }

                self.merge_branch_states(&branch_states);
            }

            Expr::Block(block) => {
                self.check_block(block);
            }

            Expr::Do { body } => {
                self.check_block(body);
            }

            Expr::Closure { params, body, .. } => {
                // Compute the capture set before checking the body, so it's
                // available for spawn enforcement via pending_capture_set.
                let capture_set = self.compute_capture_set(params, body);
                self.pending_capture_set = Some(capture_set);

                // A closure definition must not leak state into the enclosing
                // scope.  Moves/borrows inside the body execute when the
                // closure is *called*, not when it is *defined*.  Snapshot the
                // enclosing state, check the body in isolation, then restore.
                let saved = self.save_branch_state();
                let saved_loop_depth = self.loop_depth;
                let saved_loop_locals = std::mem::take(&mut self.loop_local_defs);
                let saved_in_return = self.in_return_expr;
                let saved_is_async = self.current_function_is_async;
                let saved_arena_depth = self.arena_depth;
                self.loop_depth = 0;
                self.arena_depth = 0;
                self.in_return_expr = false;
                self.current_function_is_async = false; // closures are not async (async closures deferred)
                self.check_expr(body);
                self.restore_branch_state(&saved);
                self.loop_depth = saved_loop_depth;
                self.loop_local_defs = saved_loop_locals;
                self.in_return_expr = saved_in_return;
                self.current_function_is_async = saved_is_async;
                self.arena_depth = saved_arena_depth;
            }

            Expr::ImplicitClosure { body } => {
                let saved = self.save_branch_state();
                let saved_loop_depth = self.loop_depth;
                let saved_loop_locals = std::mem::take(&mut self.loop_local_defs);
                let saved_in_return = self.in_return_expr;
                let saved_is_async = self.current_function_is_async;
                let saved_arena_depth = self.arena_depth;
                self.loop_depth = 0;
                self.arena_depth = 0;
                self.in_return_expr = false;
                self.current_function_is_async = false;
                self.check_expr(body);
                self.restore_branch_state(&saved);
                self.loop_depth = saved_loop_depth;
                self.loop_local_defs = saved_loop_locals;
                self.in_return_expr = saved_in_return;
                self.current_function_is_async = saved_is_async;
                self.arena_depth = saved_arena_depth;
            }

            Expr::ListComprehension {
                expr: comp_expr,
                iterable,
                condition,
                ..
            } => {
                self.check_expr(iterable);
                let saved_in_return = self.in_return_expr;
                self.in_return_expr = false;
                self.loop_depth += 1;
                self.loop_local_defs.push(FxHashSet::default());
                self.check_expr(comp_expr);
                if let Some(cond) = condition {
                    self.check_expr(cond);
                }
                self.loop_local_defs.pop();
                self.loop_depth -= 1;
                self.in_return_expr = saved_in_return;
            }

            Expr::DictComprehension {
                key,
                value,
                iterable,
                condition,
                ..
            } => {
                self.check_expr(iterable);
                let saved_in_return = self.in_return_expr;
                self.in_return_expr = false;
                self.loop_depth += 1;
                self.loop_local_defs.push(FxHashSet::default());
                self.check_expr(key);
                self.check_expr(value);
                if let Some(cond) = condition {
                    self.check_expr(cond);
                }
                self.loop_local_defs.pop();
                self.loop_depth -= 1;
                self.in_return_expr = saved_in_return;
            }

            Expr::SetComprehension {
                expr: comp_expr,
                iterable,
                condition,
                ..
            } => {
                self.check_expr(iterable);
                let saved_in_return = self.in_return_expr;
                self.in_return_expr = false;
                self.loop_depth += 1;
                self.loop_local_defs.push(FxHashSet::default());
                self.check_expr(comp_expr);
                if let Some(cond) = condition {
                    self.check_expr(cond);
                }
                self.loop_local_defs.pop();
                self.loop_depth -= 1;
                self.in_return_expr = saved_in_return;
            }

            Expr::ArrayLiteral(elements) | Expr::TupleLiteral(elements) => {
                for elem in elements {
                    self.check_expr(elem);
                }
            }

            Expr::DictLiteral(pairs) => {
                for (k, v) in pairs {
                    self.check_expr(k);
                    self.check_expr(v);
                }
            }

            Expr::StructLiteral { args, .. } => {
                // Struct fields own their data — non-Copy identifier args
                // are implicitly consumed (moved into the struct).
                for arg in args {
                    if let Expr::Identifier(_) = &arg.node {
                        if let Some(&var_def_id) = self.resolution_map.get(&arg.span.start) {
                            let def = self.scopes.get_def(var_def_id);
                            if def.kind == DefKind::Variable {
                                if let Some(type_id) = def.type_id {
                                    if !is_copy_type(type_id, self.types, self.scopes) {
                                        let skip_implicit_move = self.loop_depth > 0
                                            && !self.loop_local_defs.last()
                                                .map_or(false, |s| s.contains(&var_def_id))
                                            && self.in_return_expr;
                                        if !skip_implicit_move {
                                            self.check_move(var_def_id, arg.span);
                                            continue;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    self.check_expr(arg);
                }
            }

            Expr::As { expr: inner, .. } | Expr::Is { expr: inner, .. } => {
                self.check_expr(inner);
            }

            Expr::DotShorthand { args, .. } => {
                for arg in args {
                    self.check_expr(&arg.node.value);
                }
            }
            Expr::MetaOpInfix { left, right, .. } => {
                self.check_expr(left);
                self.check_expr(right);
            }
            Expr::MetaOpToken(_) => {}
            Expr::Rethrow { expr, transform, .. } => {
                self.check_expr(expr);
                self.check_expr(transform);
            }
            Expr::Catch { expr, recovery, .. } => {
                self.check_expr(expr);
                self.check_expr(recovery);
            }
        }
    }

    /// Check a re-parsed f-string interpolation expression for borrow safety.
    /// The expression was parsed from raw text, so its spans don't match the
    /// resolution map. We resolve identifiers by name and use `fstring_span`
    /// for error reporting.
    pub(super) fn check_interpolation_expr(&mut self, expr: &Spanned<Expr>, fstring_span: Span) {
        match &expr.node {
            Expr::Identifier(name) => {
                if let Some(def_id) = self.find_def_by_name(name) {
                    let kind = self.scopes.get_def(def_id).kind;
                    if kind == DefKind::Variable {
                        self.check_use(def_id, fstring_span);
                    }
                    // Phase 3: Mark as used
                    if let Some(entry) = self.local_var_usage.get_mut(&def_id) {
                        entry.2 = true;
                    }
                }
            }
            Expr::FieldAccess { object, .. }
            | Expr::TupleFieldAccess { object, .. }
            | Expr::OptionalChain { object, .. }
            | Expr::Index { object, .. } => {
                self.check_interpolation_expr(object, fstring_span);
            }
            Expr::MethodCall { receiver, args, .. } => {
                self.check_interpolation_expr(receiver, fstring_span);
                for arg in args {
                    self.check_interpolation_expr(&arg.node.value, fstring_span);
                }
            }
            Expr::Call { callee, args, .. } => {
                self.check_interpolation_expr(callee, fstring_span);
                for arg in args {
                    self.check_interpolation_expr(&arg.node.value, fstring_span);
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                self.check_interpolation_expr(left, fstring_span);
                self.check_interpolation_expr(right, fstring_span);
            }
            Expr::UnaryOp { operand, .. } => {
                self.check_interpolation_expr(operand, fstring_span);
            }
            _ => {}
        }
    }
}
