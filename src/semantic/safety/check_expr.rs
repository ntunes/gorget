use rustc_hash::FxHashSet;

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use crate::semantic::errors::SemanticErrorKind;
use crate::semantic::ids::{DefId, ScopeId};
use crate::semantic::scope::DefKind;

use super::{BorrowChecker, BorrowOrigin, FallibleState, VarState};
use super::type_utils::needs_explicit_move;

impl<'a> BorrowChecker<'a> {
    /// At a constructor / struct-literal / enum-variant init boundary, a bare
    /// (non-`!`) identifier arg of a CoW-eligible type is fine — the lowering
    /// clones-if-live / moves-if-dead. But the single-owner carve-out types
    /// (closures/`Callable`, `Owned[T]`, `Box[T]`, `Task`/`TaskGroup`/`Guard`)
    /// have NO clone path: passing one bare would be accepted here and then
    /// panic the IR lowering as an untracked consumed source — so they still
    /// require an explicit `!`. Emit `MoveWithoutOperator` (liveness-independent,
    /// mirroring the bare-assign carve-out in `check_stmt`). Explicit `!arg`
    /// goes through the `Ownership::Move` arm / `Expr::Move` walk and is never
    /// an `Expr::Identifier` here, so it is correctly not flagged.
    fn require_explicit_move_for_single_owner_init(&mut self, arg: &Spanned<Expr>) {
        if let Expr::Identifier(_) = &arg.node {
            if let Some(&var_def_id) = self.resolution_map.get(&arg.span.start) {
                let def = self.scopes.get_def(var_def_id);
                if def.kind == DefKind::Variable {
                    if let Some(type_id) = def.type_id {
                        let name = def.name.clone();
                        if needs_explicit_move(type_id, self.types, self.scopes) {
                            self.error(
                                SemanticErrorKind::MoveWithoutOperator { name },
                                arg.span,
                            );
                        }
                    }
                }
            }
        }
    }

    pub(super) fn check_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            // Literals — no ownership concerns
            Expr::IntLiteral(_)
            | Expr::FloatLiteral(_)
            | Expr::BoolLiteral(_)
            | Expr::NoneLiteral
            | Expr::SelfExpr
            | Expr::It => {}

            Expr::StringLiteral(lit, _) => {
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
                                // Dead-write lint: f-string reads count.
                                self.mark_bare_param_read(def_id);
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
                    // Dead-write lint: genuine read of a bare param.
                    self.mark_bare_param_read(def_id);
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
                // The `!` operator: move the value. `check_move` itself
                // marks the variable as used (a move IS a use), so no extra
                // bookkeeping needed here.
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

                // CoW rule (b): a constructor (Variant/Newtype) arg no longer
                // implicitly-moves / rejects a live CoW-eligible source — the
                // lowering clones-if-live. But the single-owner carve-out types
                // still require explicit `!` at a constructor (the carve-out in
                // the `Ownership::Borrow` arm below), and that carve-out must NOT
                // fire for a plain function call (where the arg is borrowed, no
                // `!` needed) — so we still need to know whether the callee IS a
                // constructor.
                let is_constructor = self
                    .resolve_callee_def_id(callee)
                    .map(|id| matches!(self.scopes.get_def(id).kind, DefKind::Variant | DefKind::Newtype))
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
                                // Borrow-fields: passing `&v` to a callee that
                                // mutates v invalidates any borrow-field struct
                                // currently borrowing from v. Only flag when the
                                // borrower is a borrow-field struct — sigil
                                // borrows have their own existing rules.
                                if let Some(src_def_id) = self.find_root_def_id(&arg.node.value) {
                                    self.check_borrow_field_mutation(src_def_id, arg.node.value.span);
                                }
                            }
                            // CoW rule (b): a bare non-Copy identifier arg at a
                            // constructor (Variant/Newtype) is NOT an implicit
                            // move that rejects a live source. The lowering
                            // clones-if-live and moves-if-dead at the init
                            // boundary (`clone_resource_args_for_init` →
                            // `is_last_use_at`), exactly as collection
                            // mutators / tuple-array literals do. So we fall
                            // through to `check_expr`, which marks the use
                            // without consuming the source. Explicit `!arg`
                            // moves still go through the `Ownership::Move` arm
                            // above. Carve-out: single-owner types (closures/
                            // `Box`/`Owned`/`Task`/...) still require explicit
                            // `!` at a CONSTRUCTOR (they have no clone path) — but
                            // NOT at a plain function call, where the arg is just
                            // borrowed. Hence the `is_constructor` gate.
                            if is_constructor {
                                self.require_explicit_move_for_single_owner_init(&arg.node.value);
                            }
                            self.check_expr(&arg.node.value);
                            // Dead-write lint: `&p` args are NOT
                            // counted as writes in v1 — without callee purity
                            // info a read-only `&` callee (NeedlessMutableBorrow
                            // shape) would false-positive. Follow-up: gate on
                            // callee purity == MutatesArgs+.
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
                // Dead-write lint: if this method call mutates its
                // receiver (builtin `is_mutating` collection/String method OR
                // user method whose first param is `&self`), the receiver is
                // rooted at a tracked bare resource param, AND the call is in
                // statement position (result discarded — in value position the
                // copy's data flows out, which is a read), the receiver walk
                // below is a write-position read of the root.
                //
                // The stmt-position test is `span.start` equality with the
                // enclosing `Stmt::Expr`, NOT "the MethodCall IS the statement
                // expression" — intentionally. An inner mutating call of a
                // chain in statement position (`p.pop().unwrap()` as a
                // statement) shares its `span.start` with the statement
                // expression, so it classifies as a write and WARNS: the whole
                // chain's result is discarded and the caller is unchanged
                // (think `handlers.pop().run()`). Do not "tighten" this to
                // exact-node identity — that silently flips this class to
                // silent (pinned by fixture deadwrite_warn_chained_stmt.gg).
                let deadwrite_mut_root: Option<crate::semantic::ids::DefId> = {
                    let in_stmt_position =
                        self.deadwrite_stmt_expr_start == Some(expr.span.start);
                    let root = if in_stmt_position {
                        self.find_root_def_id(receiver)
                            .filter(|d| self.deadwrite_params.contains_key(d))
                    } else {
                        None
                    };
                    root.filter(|_| {
                        // Builtin mutating method: gate the (name-keyed)
                        // protocol flag on the RECEIVER's type actually being
                        // a buffer-owning builtin (collection/Channel/Heap) or
                        // the owned String — interior-mutability handles
                        // (AtomicInt, WaitGroup, ...) are FFI-backed and write
                        // through, not CoW.
                        let recv_tid = self
                            .expr_types
                            .get(&receiver.span)
                            .copied()
                            .or_else(|| self.lvalue_value_type(receiver));
                        let is_builtin_mut = crate::ir::lowering::builtins::is_mutating_builtin_method(
                            method.node.as_str(),
                        ) && (self.is_buffer_owning_receiver(receiver)
                            || recv_tid.map_or(false, |t| {
                                self.is_buffer_owning_type(t)
                                    || t == self.types.owned_string_id
                            }));
                        let is_user_mut = self
                            .method_resolutions
                            .get(&method.span.start)
                            .and_then(|mdid| self.function_info.get(mdid))
                            .map_or(false, |info| {
                                info.param_ownerships.first() == Some(&Ownership::MutableBorrow)
                            });
                        is_builtin_mut || is_user_mut
                    })
                };
                let deadwrite_prev_root = self.deadwrite_write_root;
                if deadwrite_mut_root.is_some() {
                    self.deadwrite_write_root = deadwrite_mut_root;
                }
                self.check_expr(receiver);
                self.deadwrite_write_root = deadwrite_prev_root;
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
                        if let Some((recv_def_id, recv_field_path)) =
                            self.find_root_def_id_with_path(receiver)
                        {
                            let recv_name = self.scopes.get_def(recv_def_id).name.clone();
                            // Check explicit T & borrows AND borrow-field structs
                            for (&var_id, origin) in self.var_origins.iter() {
                                if origin.references_def(recv_def_id) {
                                    let def = self.scopes.get_def(var_id);
                                    // Sigil `T &` borrow OR a struct that
                                    // transitively holds a `Ref[T]`/`MutRef[T]`
                                    // field borrowing from the source.
                                    let is_ref_type = def.type_id.map_or(false, |tid| {
                                        matches!(self.types.get(tid), super::super::types::ResolvedType::Ref(_))
                                    });
                                    let is_borrow_field_struct = match def.type_id {
                                        Some(tid) => match self.types.get(tid) {
                                            super::super::types::ResolvedType::Defined(struct_def_id)
                                            | super::super::types::ResolvedType::Generic(struct_def_id, _) =>
                                                self.ref_type_structs.contains(struct_def_id),
                                            _ => false,
                                        },
                                        None => false,
                                    };
                                    if !is_ref_type && !is_borrow_field_struct { continue; }
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
                            //
                            // Field-level disjointness: only invalidate borrows whose
                            // source path is a prefix of (or equal to) the mutated
                            // path. `gpu.shader_cache.get(i)` records source path
                            // `gpu.shader_cache`; `gpu.deform_face_indices.push(...)`
                            // mutates `gpu.deform_face_indices` — disjoint, no clone.
                            for (&var_id, source) in self.index_borrow_sources.iter() {
                                if source.root != recv_def_id { continue; }
                                // The mutation invalidates a borrow iff the mutated
                                // path is a prefix of the borrow's source path
                                // (mutating `gpu` invalidates `gpu.shader_cache`
                                // borrows; mutating `gpu.shader_cache` does too;
                                // mutating `gpu.deform_face_indices` does NOT).
                                let is_prefix = recv_field_path.len() <= source.field_path.len()
                                    && recv_field_path.iter()
                                        .zip(source.field_path.iter())
                                        .all(|(a, b)| a == b);
                                if !is_prefix { continue; }
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

                // CoW rule (b): a qualified enum variant constructor arg
                // (`EnumName.VariantName(args)`) no longer implicitly-moves /
                // rejects a live CoW-eligible source — the lowering clones-if-live.
                // But single-owner carve-out types still require explicit `!` at a
                // constructor, and that carve-out must NOT fire for a plain method
                // call (where the arg is borrowed) — so we still detect whether
                // the receiver resolves to an Enum (qualified-variant construction).
                let is_enum_constructor = if let Expr::Identifier(_) = &receiver.node {
                    self.resolution_map
                        .get(&receiver.span.start)
                        .map(|&def_id| self.scopes.get_def(def_id).kind == DefKind::Enum)
                        .unwrap_or(false)
                } else {
                    false
                };

                // Arena escape check — element-INGEST consume positions
                // (sibling of the AssignOuter / Return checks in check_stmt).
                //
                // `outer.push(v.get(0).unwrap())` (collection), `ch.send(...)`
                // (Channel), or `heap.push(...)` (Heap) where the receiver
                // OUTLIVES the arena and the ingested element is a borrow into
                // an arena-scoped collection `v` is a heap-use-after-free: the
                // borrowed arena-String is aliased into the receiver's
                // self-owned buffer, then dangles when the arena is destroyed
                // at `with` exit. `gg check` must reject it (mirrors the
                // AssignOuter borrow-escape path; the self_host has no such
                // pattern). Fires only when ALL hold:
                //   1. inside a `with Arena` scope (`arena_depth > 0`),
                //   2. METHOD is a typed mutating builtin method
                //      (`is_mutating_builtin_method` — no name-matching),
                //   3a. the RECEIVER is a buffer-owning receiver: typed
                //      `collection_kind.is_some()` (Vector/Dict/Set/...) OR
                //      `owns_buffered_elements` (Channel `send`, Heap `push`) —
                //      this is what makes the ingested element ALIAS into a
                //      self-owned buffer that the arena later frees. Mutex,
                //      Shared, Guard, Atomic etc. carry neither flag (no
                //      element-ingesting owned buffer) → correctly excluded,
                //   3b. the receiver handle's root OUTLIVES the arena
                //      (root ∉ arena_scoped_vars),
                //   4. some arg escapes per the shared ingest producer
                //      `classify_ingest_escape` — arena-scoped / fresh
                //      materialization / plain literal (owned-copied into the
                //      buffer here) → AssignOuter, OR a bare LIVE outer
                //      non-Copy identifier (clone-if-live lands in the arena)
                //      → IngestLiveOuter (suggests `!name`; a true move stays
                //      accepted). This is the SAME helper the index-store
                //      gates use, so `d.put(k,v)` / `d[k]=v` / `d[k]+=v` are
                //      behaviorally identical. `.clone()` is NOT suggested —
                //      cloning into the in-scope arena UAFs too.
                if self.arena_depth > 0
                    && crate::ir::lowering::builtins::is_mutating_builtin_method(
                        method.node.as_str(),
                    )
                    && self.is_buffer_owning_receiver(receiver)
                {
                    if let Some((recv_root, _)) = self.find_root_def_id_with_path(receiver) {
                        if !self.arena_scoped_vars.contains(&recv_root) {
                            let dest_name = self.scopes.get_def(recv_root).name.clone();
                            for arg in args {
                                // Route EVERY ingested method arg through the
                                // shared `classify_ingest_escape` producer so
                                // `d.put(k, v)` stays behaviorally identical to
                                // the `d[k] = v` / `d[k] += v` index-store
                                // ingest positions (Core #4 — one helper, no
                                // per-position rule drift). The element fallback
                                // is the bound element type for a borrow-read
                                // arg (`v.get(0).unwrap()`).
                                let elem_fallback =
                                    self.arena_borrowed_element_type(&arg.node.value);
                                let is_moved =
                                    arg.node.ownership == Ownership::Move;
                                if let Some((kind, span)) = self.classify_ingest_escape(
                                    &arg.node.value,
                                    elem_fallback,
                                    is_moved,
                                    &dest_name,
                                ) {
                                    self.error(kind, span);
                                }
                            }
                        }
                    }
                }

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
                            // CoW rule (b): a bare non-Copy identifier arg at a
                            // qualified enum variant constructor (`E.W(x)`) is NOT
                            // an implicit move that rejects a live source. String
                            // was already excluded here (the IR clones strings
                            // into enum fields); the relaxation extends the same
                            // clone-if-live / move-if-dead treatment to all
                            // CoW-eligible payloads, matching the lowering's
                            // `clone_resource_args_for_init` → `is_last_use_at`
                            // decision. Fall through to `check_expr` (marks the
                            // use, doesn't consume). Explicit `!arg` still moves
                            // via the `Ownership::Move` arm above. Carve-out:
                            // single-owner types still require explicit `!` at a
                            // qualified-enum CONSTRUCTOR (not a plain method call).
                            if is_enum_constructor {
                                self.require_explicit_move_for_single_owner_init(&arg.node.value);
                            }
                            self.check_expr(&arg.node.value);
                        }
                    }
                }
                // Dead-write lint: record the receiver mutation
                // after receiver AND args are walked (arg reads evaluate
                // before the mutation lands).
                if let Some(root) = deadwrite_mut_root {
                    self.mark_bare_param_write_def(root, expr.span);
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
                // `lhs ?? rhs`: lhs is always evaluated; rhs runs only on
                // None/Error. If rhs diverges (throw/return/exit), the
                // overall expression is NOT divergent — the Some/Ok path
                // still reaches the post-?? continuation. Save/restore
                // branch state around the rhs walk so the conditional
                // divergence doesn't leak past the ?? boundary. Same
                // shape as Snag #39's `Expr::Catch` / `Expr::Rethrow`
                // fix; surfaces when item #2 from the gorget-js critique
                // (`o ?? throw err()`) lowered the rhs as a divergent
                // Expr::Block.
                self.check_expr(lhs);
                let before = self.save_branch_state();
                self.check_expr(rhs);
                self.restore_branch_state(&before);
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

            Expr::Spawn { expr: inner, unchecked } => {
                self.check_expr(inner);
                // `spawn unchecked` is the programmer-opt-out escape hatch:
                // skip all spawn-capture safety checks below. The inner
                // expression was still recursed into above for normal
                // checks (move/borrow semantics within the argument
                // expression); only the cross-boundary capture rules
                // are bypassed.
                if *unchecked {
                    return;
                }
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

            Expr::SpawnBlocking { expr: inner, unchecked } => {
                self.check_expr(inner);
                if *unchecked {
                    return;
                }
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

            Expr::StructLiteral { name, args, .. } => {
                // Struct fields own their data by default — non-Copy identifier
                // args are implicitly consumed (moved into the struct).
                // Exception: if the target field's type is `Ref[T]` / `MutRef[T]`
                // (or the legacy sigil form), the arg is BORROWED — don't move.
                // Look up the struct's per-field reference flags computed once
                // by `compute_struct_field_ref_flags`.
                let struct_def_id = self.scopes.lookup_from_scope(ScopeId(0), &name.node);
                let field_ref_flags = struct_def_id
                    .and_then(|d| self.struct_field_ref_flags.get(&d).cloned())
                    .unwrap_or_default();
                let field_mut_ref_flags = struct_def_id
                    .and_then(|d| self.struct_field_mut_ref_flags.get(&d).cloned())
                    .unwrap_or_default();
                // Track sources borrowed by this struct construction's MutRef
                // fields so we can enforce exclusivity *before* the new struct
                // becomes part of `var_origins`.
                let mut mut_ref_sources: Vec<(DefId, Span)> = Vec::new();
                for (i, arg) in args.iter().enumerate() {
                    let target_is_ref = field_ref_flags.get(i).copied().unwrap_or(false);
                    let target_is_mut_ref = field_mut_ref_flags.get(i).copied().unwrap_or(false);
                    if !target_is_ref {
                        // CoW rule (b): a bare non-Copy identifier arg at a struct
                        // literal field is NOT an implicit move that rejects a
                        // live source. The lowering clones-if-live and
                        // moves-if-dead at the struct-init boundary
                        // (`clone_multi_use_resource_args` → `is_last_use_at`,
                        // then `move_zero_consumed_args`). Fall through to
                        // `check_expr` (marks the use, doesn't consume). Explicit
                        // `!arg` moves are still handled by `Expr::Move`'s own
                        // walk inside `check_expr`. Borrow fields (`Ref`/`MutRef`)
                        // keep their genuine-borrow handling in the
                        // `target_is_mut_ref` arm below. Carve-out: single-owner
                        // types (closures/`Box`/`Owned`/`Task`/...) still require
                        // explicit `!` — they have no clone path.
                        self.require_explicit_move_for_single_owner_init(arg);
                    } else if target_is_mut_ref {
                        // Identify the source local being mutably-borrowed.
                        if let Some(src) = self.find_root_def_id(arg) {
                            mut_ref_sources.push((src, arg.span));
                        }
                    }
                    self.check_expr(arg);
                }
                // MutRef[T] exclusivity: for each source taken as a `MutRef[T]`
                // field, no other live borrow-field struct may already borrow
                // from it (shared OR exclusive). Conservative — disallows any
                // overlap with a MutRef participant.
                for (src_def_id, span) in &mut_ref_sources {
                    self.check_mut_ref_exclusive(*src_def_id, *span);
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
                // Snag #39 follow-up: rethrow's transform builds the
                // re-throw value; on the Ok path the rethrow falls through
                // with the Ok payload. The transform's divergence (if any)
                // shouldn't leak into `self.diverged` for the surrounding
                // continuation, because the Ok path is always reachable.
                // Mirrors the save/restore dance in If/Match.
                let before = self.save_branch_state();
                self.check_expr(transform);
                self.restore_branch_state(&before);
            }
            Expr::Catch { expr, recovery, .. } => {
                self.check_expr(expr);
                // Snag #39 follow-up: catch's recovery only runs on the
                // Error path; the Ok path falls through with the unwrapped
                // value. A divergent recovery (e.g. `catch (msg): print(msg);
                // exit(1); return`) shouldn't mark `self.diverged` for the
                // surrounding continuation, because the Ok path is always
                // reachable. Without this, post-catch code triggers a false
                // "unreachable code after diverging statement" warning.
                // Same shape as If/Match's save_branch_state machinery —
                // branch divergence doesn't escape the branch boundary.
                let before = self.save_branch_state();
                self.check_expr(recovery);
                self.restore_branch_state(&before);
            }
            Expr::FaultCatch { expr, handler, .. } => {
                self.check_expr(expr);
                // Like `Expr::Catch`: the handler only runs on the fault path,
                // so its divergence must not escape to the surrounding
                // continuation (the no-fault path is always reachable).
                let before = self.save_branch_state();
                self.check_expr(handler);
                self.restore_branch_state(&before);
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
                    // Dead-write lint: f-string reads count.
                    self.mark_bare_param_read(def_id);
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
