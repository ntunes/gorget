//! Assignment lowering: simple assign, field assign, index assign, compound assign.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{self, Expr};
use crate::span::Spanned;

use super::super::context::{LoweringContext, SharedLocalKind};
use super::super::exprs::{
    lower_expr, infer_operand_type_full, maybe_auto_propagate,
    guard_inner_suffix, emit_guard_get_ptr,
    emit_shared_mutex_lock_get, emit_shared_mutex_lock_set,
    atomic_type_name_for, emit_atomic_load, emit_atomic_store,
    emit_rwlock_write_get, emit_rwlock_write_set, emit_rwlock_write_finish,
    try_resolve_field_place, try_resolve_tuple_field_place,
    materialize_global_field_base, extract_field_path_string,
    infer_collection_element_type, resolve_projection_root_local,
};

/// Lower an assignment.
pub(super) fn lower_assign(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    target: &Spanned<Expr>,
    value: &Spanned<Expr>,
) {
    match &target.node {
        Expr::Identifier(name) => {
            if let Some((mut local_id, _type_id)) = ctx.lookup_local(name) {
                // Shared variable: dispatch based on wrapper kind
                if let Some(info) = ctx.shared.locals.get(&local_id) {
                    let (hidden_local, inner_type, kind) = (info.hidden_local, info.inner_type, info.kind);
                    match kind {
                        SharedLocalKind::Mutex => {
                            let operand = lower_expr(ctx, builder, value);
                            let inner_c = ctx.c_type_name_for_id(inner_type);
                            let mutex_type = ctx.type_mapper.lookup_named(&format!("Mutex__{inner_c}")).unwrap_or(inner_type);
                            emit_shared_mutex_lock_set(ctx, builder, hidden_local, mutex_type, inner_type, operand);
                            return;
                        }
                        SharedLocalKind::Atomic => {
                            let operand = lower_expr(ctx, builder, value);
                            let atomic_name = atomic_type_name_for(inner_type);
                            emit_atomic_store(ctx, builder, hidden_local, operand, &atomic_name);
                            return;
                        }
                        SharedLocalKind::RwLock => {
                            let operand = lower_expr(ctx, builder, value);
                            emit_rwlock_write_set(ctx, builder, hidden_local, inner_type, operand);
                            return;
                        }
                        SharedLocalKind::SharedArc => {
                            // ArcOnly: assignment shouldn't happen (CFA upgrades to ArcMutex)
                        }
                    }
                }
                let type_id = _type_id;
                // CoW: if this local is a source with aliases, sever them first.
                // Aliases keep the old value; this local gets the new value.
                if ctx.cow_has_aliases(builder, local_id) {
                    ctx.cow_sever_all_aliases_from(builder, local_id, target.span);
                }
                // CoW: if this local is an alias, just remove from alias maps.
                // The reassignment naturally replaces the binding value.
                if ctx.cow_is_alias(builder, local_id) {
                    ctx.unset_ownership(builder, local_id);
                }
                // CoW: if this collection has element refs, clone them out.
                // Reassignment replaces the buffer; outstanding refs would dangle.
                if ctx.cow_has_collection_refs(builder, local_id) {
                    ctx.cow_before_mutation(builder, local_id, target.span);
                }
                // CoW clone-on-mutate: if LHS is an immutable borrow (Ptr, not MutPtr),
                // materialize to owned before computing RHS. This is the CoW clone.
                // MutPtr (& params) and unique-borrow locals pass through without cloning.
                {
                    use crate::ir::types::GirType;
                    let is_mut = ctx.is_param_borrow_unique(builder, local_id)
                        || matches!(ctx.type_registry.get(type_id), Some(GirType::MutPtr(_)));
                    if !is_mut {
                        if let Some(GirType::Ptr(inner)) = ctx.type_registry.get(type_id).cloned() {
                            if let Some(clone_fn) = ctx.clone_fn_for_ptr(inner) {
                                let cloned = ctx.emit_clone(builder, &clone_fn, vec![FunctionBuilder::copy(local_id)], value.span, inner, crate::ir::ImplicitCloneReason::NamedToNamed);
                                // matcluster #3: a full rebind of a bare-VALUE Ptr param
                                // (`xs = <expr>`, `xs` a `Vector[T]`/resource param stored
                                // as `void* __v0 = (void*)__p0`) binds the name to the FRESH
                                // owned `cloned` local; the param slot (`local_id`) stays
                                // `void*` and untouched. `cloned` reads the OLD binding, so a
                                // self-referential RHS (`xs = xs.slice(..)`) still slices the
                                // caller's live buffer before the tail clones/moves it in.
                                //
                                // REMOVED (was the bug): the old `assign_mode(Move, local_id,
                                // cloned)` + `builder.locals[local_id].type_id = inner` in-place
                                // slot upgrade. Upgrading the param slot retro-typed the entry
                                // binding `void* __v0 = (void*)__p0` to the owned type — invalid
                                // C (`GorgetArray = (void*)` at cc) / invalid LLVM (ptr vs
                                // aggregate at llc) — AND marked the borrowed param slot owned,
                                // double-freeing the caller's buffer at fn exit. Mirrors the
                                // self-host oracle (lower_stmt.gg:945-994: a bare param is
                                // LoBorrowed → drop_old=false → consume+assign into the binding),
                                // which lowers `xs = [9,9]` correctly (3,1). No independent-vs-
                                // self-derived distinction: uniform for every bare-param rebind.
                                ctx.register_local(name, cloned, inner);
                                ctx.drops.register_local(cloned, inner, &ctx.type_registry);
                                ctx.set_owned(builder, cloned);
                                local_id = cloned;
                            }
                        }
                    }
                }
                let type_id = builder.local_type(local_id); // re-read after possible CoW upgrade
                // Check if old value needs dropping before reassignment.
                // All droppable types — including collections — drop the old value
                // before the new one is assigned. The RHS is fully computed (and
                // cloned for named locals) before the Drop fires, so even
                // self-referential RHS like `v = v.slice(...)` is safe.
                let needs_drop = ctx.type_registry.needs_drop(type_id);
                // Compute new value (now operating on owned copy if CoW upgraded)
                let prev_expected = ctx.func_state.expected_type;
                ctx.func_state.expected_type = Some(type_id);
                let operand = lower_expr(ctx, builder, value);
                // `s = MODULE_GLOBAL_RESOURCE`: clone the global so the
                // reassigned local owns its own allocation. See
                // `clone_resource_global_ref`.
                let operand = super::clone_resource_global_ref(ctx, builder, operand, value.span);
                // Auto-propagate: if RHS is Result-typed but target is not, unwrap
                let mut operand = maybe_auto_propagate(ctx, builder, operand, value.span);
                ctx.func_state.expected_type = prev_expected;
                // Auto-deref Ref[T] → T at reassignment when target is bare T.
                // `int ai = 1; ai = a.get(da).unwrap()` — declared int but RHS
                // is Ptr(i64) post-1.7b. Mirrors VarDecl's deref branch so the
                // stored value is the int, not the pointer bits.
                {
                    use crate::ir::types::GirType;
                    let rhs_type = if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                        if p.projections.is_empty() && (p.local.0 as usize) < builder.locals.len() {
                            Some(builder.local_type(p.local))
                        } else { None }
                    } else { None };
                    if let Some(rhs) = rhs_type {
                        if let Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) = ctx.type_registry.get(rhs).cloned() {
                            if !matches!(ctx.type_registry.get(type_id), Some(GirType::Ptr(_) | GirType::MutPtr(_)))
                                && !ctx.type_registry.is_resource_type(inner)
                                && ctx.clone_fn_for_ptr(inner).is_none()
                            {
                                if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                                    let tmp = builder.add_local(inner, None);
                                    builder.assign(
                                        Place::local(tmp),
                                        Operand::Copy(Place {
                                            local: p.local,
                                            projections: vec![Projection::Deref],
                                        }),
                                    );
                                    operand = FunctionBuilder::copy(tmp);
                                }
                            } else if !matches!(ctx.type_registry.get(type_id), Some(GirType::Ptr(_) | GirType::MutPtr(_))) {
                                // Ptr(T) → T (value type) where T is a resource with a clone function.
                                // The pointer is a field borrow — the backing data is owned by the
                                // parent struct. Materialise an independent owned copy now so that
                                // dropping the parent doesn't invalidate this binding.
                                // Without this, `match obj.field: case Variant(items):` followed
                                // by `my_vec = items` shallow-copies the GorgetArray header while
                                // the backing buffer is still owned by `obj`; `obj`'s drop then
                                // frees the buffer, and subsequent clones or reads use-after-free.
                                if let Some(clone_fn) = ctx.clone_fn_for_ptr(inner) {
                                    if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                                        let cloned = ctx.emit_clone(
                                            builder,
                                            &clone_fn,
                                            vec![crate::ir::builder::FunctionBuilder::copy(p.local)],
                                            value.span,
                                            inner,
                                            crate::ir::ImplicitCloneReason::NamedToNamed,
                                        );
                                        ctx.drops.register_local(cloned, inner, &ctx.type_registry);
                                        ctx.set_owned(builder, cloned);
                                        operand = crate::ir::builder::FunctionBuilder::copy(cloned);
                                    }
                                }
                            }
                        }
                    }
                }
                // String self-referential reassignment fix: if the RHS might be a
                // view into the old LHS (e.g., `s = s.trim()` or `s = s[0..1]`),
                // clone-to-owned before dropping. gorget_string_clone_to_owned
                // allocates a new buffer even for owned strings, so skip when the
                // RHS is a fresh allocation (user function call result — independent
                // buffer, cannot be a view into the old LHS).
                if needs_drop && ctx.type_mapper.is_string_type(type_id) {
                    if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                        let src_type = builder.local_type(place.local);
                        if ctx.type_mapper.is_string_type(src_type)
                            && !ctx.is_fresh_string(builder, place.local)
                        {
                            let owned = ctx.type_mapper.owned_string_type;
                            let cloned = ctx.emit_clone(
                                builder,
                                "gorget_string_clone_to_owned",
                                vec![FunctionBuilder::copy(place.local)],
                                value.span,
                                owned,
                                crate::ir::ImplicitCloneReason::NamedToNamed,
                            );
                            ctx.drops.register_local(cloned, owned, &ctx.type_registry);
                            ctx.set_owned(builder, cloned);
                            operand = FunctionBuilder::copy(cloned);
                        }
                    }
                }
                // P2.6: Drop old value AFTER computing new value, BEFORE assigning
                if needs_drop {
                    // For enums with resource payloads but no DropStrategy,
                    // emit an explicit call to {Name}__drop instead of a GIR Drop
                    // instruction (which would lower to Nop for DropStrategy::None).
                    let used_explicit_drop = if let Some(crate::ir::types::GirType::Named(tn)) =
                        ctx.type_registry.get(type_id).cloned()
                    {
                        let tn = tn.clone();
                        if let Some(td) = ctx.type_registry.get_type_def(&tn) {
                            if td.metadata.drop_strategy == DropStrategy::None {
                                if let crate::ir::types::TypeDefKind::Enum(_) = &td.kind {
                                    let drop_fn = format!("{tn}__drop");
                                    let ptr_type = ctx.register_ptr_type(type_id);
                                    let ptr_local = builder.add_local(ptr_type, None);
                                    builder.emit_borrow_mut(ptr_local, Place::local(local_id));
                                    builder.call_void(&drop_fn, vec![FunctionBuilder::copy(ptr_local)]);
                                    true
                                } else { false }
                            } else { false }
                        } else { false }
                    } else { false };
                    if !used_explicit_drop {
                        // Use DropIfAlive if the variable may have been moved
                        // (e.g., pushed into a collection earlier).  MoveSlot
                        // marks the slot as dead; DropIfAlive lets the drop
                        // elaboration pass skip the drop when the slot is
                        // provably uninitialized.
                        if ctx.drops.is_moved(local_id) {
                            builder.drop_if_alive(Place::local(local_id));
                        } else {
                            builder.drop(Place::local(local_id));
                        }
                    }
                }
                // If this is a mutable capture pointer, write through the pointer
                if ctx.is_param_borrow_unique(builder, local_id) {
                    let deref_place = Place {
                        local: local_id,
                        projections: vec![Projection::Deref],
                    };
                    builder.assign(deref_place, operand.clone());
                    // Unregister RHS temp — the deref store took ownership.
                    if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                        if place.projections.is_empty() && !ctx.is_named_local(place.local) {
                            ctx.drops.unregister(place.local);
                        }
                    }
                } else {
                    // Determine assignment mode (same decision tree as VarDecl).
                    use crate::ir::instructions::AssignMode;
                    let mut assign_mode = AssignMode::Copy;

                    // Phase D4 typed signals — same lift as `lower_var_decl`.
                    // Branches below progressively migrate from sidecar predicates
                    // (`is_named_local`, `drops.is_registered`, `needs_drop`) to
                    // `(target_resource, source_live, source_own)`.
                    let _target_resource = ctx.type_registry.is_resource_type(type_id);
                    let _source_live = ctx.source_live_past(&operand, value.span, builder);
                    let _source_own = ctx.source_ownership(&operand, builder);

                    if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                        if place.projections.is_empty() && place.local != local_id {
                            let rhs_type = builder.local_type(place.local);

                            if rhs_type == ctx.type_mapper.owned_string_type
                                && type_id == ctx.type_mapper.owned_string_type
                            {
                                ctx.drops.unregister(place.local);
                                assign_mode = AssignMode::Move;
                            }
                            // Branch B (cross-type): named non-resource RHS
                            // with a clone_fn whose return type matches the
                            // target — e.g. Str→GorgetString conversion. The
                            // clone returns the TARGET type, not the source
                            // type; without this, the safety-net Move would
                            // either skip (rhs non-resource) or zero a
                            // still-live primitive local. Mirrors
                            // lower_var_decl's branch B exactly. The
                            // is_named_local guard is genuine — see
                            // commit cd9357f8 (probed: 16 fixtures regress
                            // when removed; unnamed temps of types like
                            // Result[Config, String] hit clone_fn_for_ptr
                            // true via the recursively-droppable scan and
                            // get routed through cross-type clone when they
                            // should fall through to Move).
                            else if ctx.is_named_local(place.local)
                                && !ctx.type_registry.is_resource_type(rhs_type)
                                && ctx.clone_fn_for_ptr(rhs_type).is_some()
                            {
                                ctx.warn_clone_and_hit(builder, value.span, rhs_type, crate::ir::ImplicitCloneReason::NamedToNamed);
                                let clone_fn = ctx.clone_fn_for_ptr(rhs_type)
                                    .expect("BUG: clone_fn_for_ptr returned None after is_some check");
                                let ptr_type = ctx.register_ptr_type(rhs_type);
                                let ptr_local = builder.add_local(ptr_type, None);
                                builder.emit_borrow(ptr_local, place.clone());
                                // Clone returns the TARGET type (type_id),
                                // not the source type. clone_fn may
                                // produce an owned T from a view/borrow
                                // shape distinct from the RHS type.
                                let cloned = builder.call_clone(&clone_fn, vec![FunctionBuilder::copy(ptr_local)], type_id, crate::ir::ImplicitCloneReason::NamedToNamed);
                                ctx.set_owned(builder, cloned);
                                operand = FunctionBuilder::copy(cloned);
                                assign_mode = AssignMode::Move;
                            } else if ctx.is_named_local(place.local) {
                                // Last-use optimization: if the RHS is dead after this
                                // assignment, move instead of cloning. The source is
                                // MoveZeroed after the assignment (see line ~207).
                                let rhs_name = builder.local_name(place.local).map(|s| s.to_string());
                                let is_last_use = rhs_name.as_ref()
                                    .map(|n| ctx.is_last_use_at(n, value.span))
                                    .unwrap_or(false);
                                if is_last_use && ctx.type_registry.is_resource_type(rhs_type) {
                                    // Move: unregister source from drops, assign, MoveZero
                                    ctx.drops.unregister(place.local);
                                    assign_mode = AssignMode::Move;
                                } else if let Some(clone_fn) = ctx.clone_fn_for_ptr(rhs_type) {
                                    ctx.warn_clone_and_hit(builder, value.span, rhs_type, crate::ir::ImplicitCloneReason::NamedToNamed);
                                    let ptr_type = ctx.register_ptr_type(rhs_type);
                                    let ptr_local = builder.add_local(ptr_type, None);
                                    builder.emit_borrow(ptr_local, place.clone());
                                    let cloned = builder.call_clone(&clone_fn, vec![FunctionBuilder::copy(ptr_local)], rhs_type, crate::ir::ImplicitCloneReason::NamedToNamed);
                                    ctx.set_owned(builder, cloned);
                                    operand = FunctionBuilder::copy(cloned);
                                    assign_mode = AssignMode::Move;
                                }
                            } else if ctx.drops.is_registered(place.local) {
                                assign_mode = AssignMode::Move;
                            }
                            // Branch D (rhs==string && type==string) was a
                            // duplicate of branch A's predicate — unreachable
                            // today; deleted in the D4 cleanup. The
                            // string-to-string Move case is handled by branch A
                            // (with the additional drops.unregister side effect).
                            // Safety net: no Copy for resource RHS types.
                            //
                            // Phase D4 probe (2026-05-04): switching to
                            // target-keyed `target_resource` regressed 7
                            // fixtures (dataframe_*, self_host_bootstrap*).
                            // Unlike `lower_var_decl`'s branch G, this site
                            // sees cross-type cases where rhs is non-resource
                            // and target is resource — Move'ing the source
                            // would zero a non-resource (e.g. primitive)
                            // local that's still alive elsewhere. Adding a
                            // Branch B analog (cross-type clone) catches the
                            // named-source Str→GorgetString cases but the
                            // dataframe regressions persist — there are
                            // unnamed-temp paths where rhs is non-resource
                            // and target is resource, and Branch B's
                            // is_named_local guard skips them. Kept as
                            // RHS-keyed; the mirror with branch G is not
                            // appropriate at this site.
                            if assign_mode == AssignMode::Copy && ctx.type_registry.is_resource_type(rhs_type) {
                                assign_mode = AssignMode::Move;
                            }
                        }
                    }

                    builder.assign_mode(assign_mode, Place::local(local_id), operand.clone());

                    // The reassignment gives `local_id` a fresh owning value
                    // (Move or Copy of a live source). Clear any stale
                    // `maybe_moved` flag set by a *prior* consume of this slot
                    // (e.g. `vec.push(local_id)` earlier in a branch that
                    // already MoveZero'd the slot before this re-bind). Without
                    // this reset, downstream consume-site staging sees
                    // `is_moved(local_id) == true` and skips the required
                    // post-consume `move_zero`, leaving the unconditional
                    // scope-exit `drop_if_alive` to free data the consumer now
                    // also owns. Mirrors the `set_owned` propagation below —
                    // ownership state and drop-flag state must agree on
                    // "slot has a live value here". (gorget-js snag #3)
                    ctx.drops.clear_moved(local_id);

                    // Mark source as moved + emit GIR-level move-zero.
                    if assign_mode == AssignMode::Move {
                        if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                            if place.projections.is_empty()
                                && place.local != local_id
                                && !ctx.drops.is_moved(place.local)
                            {
                                ctx.move_zero_and_mark(builder, place.local);
                            }
                        }
                    }
                    // Propagate ownership: if RHS local owned its data (call result),
                    // the target local now owns the data after move/clone.
                    if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                        if ctx.is_owned_local(builder, p.local) {
                            ctx.set_owned(builder, local_id);
                        }
                    }
                }
                // Lazy loop-carried CoW (W4 write-site clearing): the local
                // was REASSIGNED wholesale, so its collection-element
                // provenance no longer holds. Remove the flag-map entry — a
                // stale tag+flag pair would emit a pointless guarded clone at
                // the next collection mutation / W3 read hook and leak the
                // new buffer via the materialize's Move-assign overwrite.
                // Cleared AFTER the RHS is lowered (deliberate): clearing up
                // front would break self-referential RHS that mutates the
                // source mid-expression (`s = s + poke(&v)` — the `&v`
                // dispatch must still find the tag to materialize `s` before
                // the concat reads it). If the write-back didn't already
                // re-tag the slot, drop the stale
                // Borrowed{CollectionElement} tag too.
                if ctx.func_state.cow_lazy_mat_flag.remove(&local_id).is_some()
                    && ctx.collection_ref_source(builder, local_id).is_some()
                {
                    ctx.unset_ownership(builder, local_id);
                }
            } else if ctx.global_names.contains(name.as_str()) {
                // Module-level static variable — store into a static is a
                // CONSUMING POSITION (the static must OWN its value), exactly
                // like a collection put / struct-field init / return. The old
                // plain `global_assign` did a shallow memcpy with no
                // clone/move/drop, so `CACHE = d` (owned local Dict → static
                // Dict) shallow-aliased `d`'s heap buffer; when the function
                // returned, `d`'s scope-exit drop freed that buffer and the
                // next read of `CACHE` was a use-after-free (Conformance Bug 2).
                // Mirror the consuming-position discipline the local-assign
                // path and `lower_index_assign` already run:
                //   1. clone-or-move the RHS (`ensure_owned_at_consuming_arg`),
                //   2. snapshot the static's OLD value into a temp (an EAGER
                //      Load — `Constant::GlobalRef` lowers to GlobalAddr+Load
                //      at the point it's consumed, so the snapshot MUST be
                //      materialized BEFORE the store or it would read the NEW
                //      value and double-free it),
                //   3. store the new value (this is the consuming use of the
                //      source),
                //   4. MoveZero the moved source so its scope-exit drop can't
                //      double-free the buffer the static now owns — emitted
                //      AFTER the store, exactly like `lower_index_assign` runs
                //      `maybe_move_zero` AFTER the consuming `call_void`,
                //   5. drop the OLD-value snapshot.
                // Ordering is load-bearing — compute-new → snapshot-old →
                // store-new → move-zero-source → drop-old — which makes a
                // self-referential RHS (`CACHE = grow(CACHE)`) safe.
                let operand = lower_expr(ctx, builder, value);
                // (1) Clone a borrowed/live source; leave an owned-dead temp
                //     to move (the canonical decision tree).
                let operand = ctx.ensure_owned_at_consuming_arg(
                    builder, operand, value,
                    crate::ir::ImplicitCloneReason::ConsumingArg);
                // Capture the move-zero target BEFORE the store consumes the
                // operand. Gate on resource-or-contains-resource (the wide
                // `needs_drop` predicate, not the narrow is_resource_type),
                // skip Ptr wrappers (already materialized) and already-moved
                // slots.
                let move_zero_target = match &operand {
                    Operand::Copy(place) | Operand::Move(place)
                        if place.projections.is_empty()
                            && ctx.type_registry.is_resource_or_contains_resource(
                                builder.local_type(place.local))
                            && !ctx.drops.is_moved(place.local)
                            && !matches!(
                                ctx.type_registry.get(builder.local_type(place.local)),
                                Some(crate::ir::types::GirType::Ptr(_))
                                    | Some(crate::ir::types::GirType::MutPtr(_))) =>
                    {
                        Some(place.local)
                    }
                    _ => None,
                };
                // (2) EAGER-snapshot the OLD static value into a temp BEFORE
                //     the store — ONLY for resource statics. The snapshot's
                //     type comes from `global_type_names` (same lookup the
                //     static compound-assign path uses). A primitive static
                //     (`static int counter`) needs no drop-on-overwrite, and
                //     emitting a `Drop` on a non-droppable type fails GIR
                //     validation — gate on `is_resource_or_contains_resource`.
                let old_type = ctx.global_type_names.get(name)
                    .and_then(|tn| ctx.type_mapper.lookup_named(tn))
                    .unwrap_or(I64_TYPE);
                let snap = if ctx.type_registry.is_resource_or_contains_resource(old_type) {
                    let snap = builder.add_local(old_type, None);
                    builder.assign(
                        Place::local(snap),
                        Operand::Constant(Constant::GlobalRef(name.clone())));
                    Some(snap)
                } else {
                    None
                };
                // (3) Store the new value (consuming use of the source).
                builder.global_assign(name.clone(), operand);
                // (4) MoveZero the moved source — AFTER the store has read it.
                if let Some(src) = move_zero_target {
                    ctx.move_zero_and_mark(builder, src);
                }
                // (5) Drop the OLD-value snapshot (frees the prior buffer while
                //     the static holds the new one).
                if let Some(snap) = snap {
                    builder.drop(Place::local(snap));
                }
            }
        }
        Expr::FieldAccess { object, field } => {
            lower_field_assign(ctx, builder, object, &field.node, value);
        }
        Expr::TupleFieldAccess { object, index } => {
            // `t.0 = v` — a tuple field is a valid mutable place (a struct field
            // at a numeric index). Pre-fix this fell to the `_ =>` no-op below
            // and SILENTLY DROPPED the write (`x.0 = 9` printed 1). Resolve the
            // write-through place (Core #10 lower, don't drop) and store — the
            // sibling of the compound `t.0 OP= v` arm (Core #4, one class).
            lower_tuple_field_assign(ctx, builder, object, *index, value);
        }
        Expr::Index { object, index } => {
            lower_index_assign(ctx, builder, object, index, value);
        }
        // Snag #26: `*box = val` and `*ptr = val` write through the
        // pointer to the heap (Box) or pointee (Ptr). Without this arm
        // the assignment was silently dropped — `_` did nothing, and
        // `lower_expr(*box)` would have produced a value-copy in the
        // RHS-only sense rather than an lvalue place.
        Expr::Deref { expr: inner } => {
            let inner_op = lower_expr(ctx, builder, inner);
            if let Operand::Copy(ref inner_place) | Operand::Move(ref inner_place) = inner_op {
                let mut deref_place = inner_place.clone();
                deref_place.projections.push(Projection::Deref);
                let pointee_type = {
                    let local_idx = inner_place.local.0 as usize;
                    let mut t = if local_idx < builder.locals.len() {
                        builder.local_type(inner_place.local)
                    } else { UNIT_TYPE };
                    for proj in &inner_place.projections {
                        if let Projection::Deref = proj {
                            t = ctx.deref_inner_type(t).unwrap_or(t);
                        } else if let Projection::Field(idx) = proj {
                            if let Some(tn) = ctx.type_name_for_id(t).map(|s| s.to_string()) {
                                if let Some(td) = ctx.type_registry.get_type_def(&tn) {
                                    if let crate::ir::types::TypeDefKind::Struct(ref s) = td.kind {
                                        if (*idx as usize) < s.fields.len() {
                                            t = s.fields[*idx as usize].type_id;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    ctx.deref_inner_type(t).unwrap_or(t)
                };
                // Propagate the pointee type as expected_type so an
                // `Expr::NoneLiteral` RHS materialises a tagged None at
                // lower_expr time. The chokepoint coercion in
                // `emit_field_store_with_cleanup` catches any case this
                // propagation misses (Snag #32).
                let prev_expected = ctx.func_state.expected_type;
                ctx.func_state.expected_type = Some(pointee_type);
                let mut rhs = lower_expr(ctx, builder, value);
                ctx.func_state.expected_type = prev_expected;
                // CoW: a `*box = borrowed` / `*ptr = borrowed` store moves a
                // value into an OWNED pointee — exactly the consuming-position
                // discipline the field-store path runs (`lower_field_assign`
                // at :508/:531). Route the RHS through the SAME helper so a
                // borrowed/live source is cloned (and a fresh/move-eligible
                // one still moves). Gate on the pointee being a resource type
                // so non-resource fresh-value stores (e.g. `*b = Inner(99)` in
                // box_deref_write.gg) stay a plain Copy and don't regress to a
                // clone. The validator's `Instruction::Assign` arm enforces the
                // same `is_resource_type(pointee)` predicate. (Enum-payload
                // pointees like `Box[Option[String]]` stay SKIPPED here because
                // `is_resource_type` doesn't descend enum variants — a deferred
                // gap recorded in TODO.md.)
                if ctx.type_registry.is_resource_type(pointee_type) {
                    clone_ptr_rhs_if_needed(ctx, builder, &mut rhs, value);
                }
                emit_field_store_with_cleanup(ctx, builder, &deref_place, pointee_type, &rhs);
            }
        }
        _ => {
            // Core #10 (lower-or-reject): the assignable PLACE forms are handled
            // above (Identifier / FieldAccess / TupleFieldAccess / Index /
            // Deref). A non-lvalue target (`5 = 1`, `foo() = 1`) is REJECTED at
            // check time by `check_assign_target_lvalue` (E_InvalidAssignTarget),
            // so accepted code never reaches here. This arm formerly SILENTLY
            // DROPPED the write (e.g. `x.0 = 9` before the TupleFieldAccess arm
            // above) — now unreachable, backed by the check-time guard.
            unreachable!(
                "lower_assign reached a non-lvalue target ({:?}) that \
                 check_assign_target_lvalue should have rejected (E_InvalidAssignTarget)",
                target.node,
            );
        }
    }
}

/// Emit a drop for a field place if its type is droppable (has a non-None DropStrategy).
fn emit_field_drop_if_needed(
    ctx: &LoweringContext,
    builder: &mut FunctionBuilder,
    place: &Place,
    field_type: TypeId,
) {
    use crate::ir::types::GirType;
    // Check if the field type has a drop strategy
    let needs_drop = if let Some(GirType::Named(type_name)) = ctx.type_registry.get(field_type) {
        if ctx.type_registry.is_collection_type_name(type_name) {
            true
        } else if let Some(type_def) = ctx.type_registry.get_type_def(type_name) {
            type_def.metadata.drop_strategy != DropStrategy::None
        } else {
            type_name == "GorgetString"
        }
    } else {
        false
    };
    if needs_drop {
        builder.drop(place.clone());
    }
}

/// Lower a field assignment: `obj.field = value`
pub(super) fn lower_field_assign(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    field_name: &str,
    value: &Spanned<Expr>,
) {
    // CoW: a field write mutates the object. Materialize the immutable-in-context
    // ROOT (decide-at-root, Core #1/#4) so the write lands on an owned copy; a
    // no-op on `&`/owned roots keeps `&`-chain write-through. The subsequent
    // `try_resolve_field_place` re-resolves against the rebound owned local.
    // Routed through the SHARED assign-target-root prologue (planner round 3):
    // identical dispatch to `lower_index_assign` / the compound path — one class,
    // one helper, one plan lookup (was three open-coded copies, Core #4).
    materialize_assign_target_root(ctx, builder, object);

    // CoW UAF fix (round-33, class fix): snapshot the local range spanned by the
    // WHOLE statement's lowering (object projection chain + RHS value). A
    // projected object (`v[i].field = x`, `m[i][j].field = x`) mints a transient
    // CollectionElement/FieldPath handle for EACH index-load level (`m[i]` → h1,
    // `m[i][j]` → h2), AND the RHS may mint element-refs into the SAME collection
    // (`v[0].name = v[1].name`); ALL are store-adjacent read handles, dead after
    // this statement. Any that stays CoW-tracked dangles when a later
    // same-collection mutation reallocates the private copy the G1 root-
    // materialize created — see `untrack_transient_element_refs_in_range`.
    //
    // Hoisted ABOVE `try_resolve_field_place` (was fallback-only): the
    // `Expr::Index` arm of that helper resolves `v[i].field` / `PTS[i].field` to
    // a WRITE-THROUGH element pointer, and for a MULTILEVEL base (`m[i][j].field`)
    // its inner `lower_expr(m[i])` mints the same transient CollectionElement
    // handle the fallback does — so the try_resolve early-return path needs the
    // same untrack, or a `m[i][j].field = x` + same-collection `push` heap-UAFs.
    let stmt_locals_start = builder.locals.len();

    // Try to resolve the full field projection chain without materializing
    // intermediate struct values. This handles nested field writes like
    // `gs.current_weapon.ammo = x` by building Place { local: gs, projections: [Deref, Field(5), Field(2)] }
    // instead of copying the intermediate struct to a temp.
    if let Some((target_place, field_type)) = try_resolve_field_place(ctx, builder, object, field_name) {
        // Propagate the field type as expected_type so an `Expr::NoneLiteral`
        // RHS materialises a tagged None at lower_expr time (see Snag #29b
        // runtime fix). The chokepoint coercion in `emit_field_store_with_cleanup`
        // catches any case this propagation misses.
        let prev_expected = ctx.func_state.expected_type;
        ctx.func_state.expected_type = Some(field_type);
        let mut rhs = lower_expr(ctx, builder, value);
        ctx.func_state.expected_type = prev_expected;
        clone_ptr_rhs_if_needed(ctx, builder, &mut rhs, value);
        emit_field_store_with_cleanup(ctx, builder, &target_place, field_type, &rhs);
        // Untrack the transient index handles minted by the `Expr::Index` arm
        // (and any RHS element-refs) — mirrors the fallback's end-of-stmt untrack.
        ctx.untrack_transient_element_refs_in_range(builder, stmt_locals_start, builder.locals.len());
        return;
    }

    // Fallback: lower_expr on object (may copy intermediate structs).
    // Object-operand computation + write-through place resolution are shared
    // with the compound `OP=` fallback (`lower_compound_assign`) via
    // `lower_field_object_operand` + `resolve_ptr_field_place` — one class, one
    // resolver, so the two write-through fallbacks cannot drift (Core #4; the
    // `compound_assign_fieldaccess_fallback_present` lint pins both callers).
    let obj = lower_field_object_operand(ctx, builder, object);
    let mut rhs = lower_expr(ctx, builder, value);
    clone_ptr_rhs_if_needed(ctx, builder, &mut rhs, value);

    // Untrack EVERY transient element/field-path handle minted across the WHOLE
    // statement (object chain + RHS), not just the object — an RHS element-ref
    // into the SAME collection this store root-materialized (`v[0].name =
    // v[1].name` + a later `v.push()`) points into the private owned copy and
    // dangles when the push reallocs it (Case 3 clones freed memory). Safe now:
    // `clone_ptr_rhs_if_needed` (ensure_owned) has already captured the stored
    // value by clone, so every remaining element/field-path handle in range is a
    // dead READ ref. Bounded to [before-object, here] so nothing beyond this
    // statement is touched. Store-neutral: the store uses the Place, not the tag.
    ctx.untrack_transient_element_refs_in_range(builder, stmt_locals_start, builder.locals.len());

    match resolve_ptr_field_place(ctx, builder, &obj, field_name) {
        PtrFieldPlace::Resolved(target_place, field_type) => {
            emit_field_store_with_cleanup(ctx, builder, &target_place, field_type, &rhs);
        }
        // ReadGuard: writes forbidden (type checker should catch in future).
        PtrFieldPlace::ReadGuardSkip => {}
        // Unknown / non-place object — no store (pre-existing behavior).
        PtrFieldPlace::Unresolved => {}
    }
}

/// Lower a tuple-field assignment: `obj.INDEX = value`. The numeric-index
/// sibling of `lower_field_assign` — a tuple field is a struct field at a
/// positional index, so this resolves the write-through place via
/// `try_resolve_tuple_field_place` and stores through it, routing the RHS through
/// the shared consuming-position ownership rule. Core #10: lower, never silently
/// drop (pre-fix `t.0 = v` fell to `lower_assign`'s `_ =>` no-op); Core #4: one
/// class with the compound `t.0 OP= v` arm.
pub(super) fn lower_tuple_field_assign(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    index: usize,
    value: &Spanned<Expr>,
) {
    // CoW: a tuple-field write mutates the object — materialize the
    // immutable-in-context root so the write lands on an owned copy (no-op on
    // `&`/owned roots, preserving write-through). Mirrors lower_field_assign.
    materialize_assign_target_root(ctx, builder, object);
    let stmt_locals_start = builder.locals.len();
    if let Some((target_place, elem_type)) =
        try_resolve_tuple_field_place(ctx, builder, object, index)
    {
        let prev_expected = ctx.func_state.expected_type;
        ctx.func_state.expected_type = Some(elem_type);
        let mut rhs = lower_expr(ctx, builder, value);
        ctx.func_state.expected_type = prev_expected;
        clone_ptr_rhs_if_needed(ctx, builder, &mut rhs, value);
        emit_field_store_with_cleanup(ctx, builder, &target_place, elem_type, &rhs);
        ctx.untrack_transient_element_refs_in_range(builder, stmt_locals_start, builder.locals.len());
    }
}

/// Compute the object OPERAND for the field-store fallback (`obj.field = v` /
/// `obj.field OP= v` when `try_resolve_field_place` returned `None`). Faithful
/// mirror of the branch that used to be open-coded in `lower_field_assign`: a
/// unique-borrow (`&`/`!`) param is returned as its RAW pointer local (not
/// deref'd, so the store writes THROUGH the caller); a module-level static base
/// is materialized into an addressable `MutPtr`; everything else — including a
/// `.get().unwrap()` method chain, which lowers to a Ref per the ratified
/// auto-borrow-from-get — goes through `lower_expr`. Shared by plain `=`
/// (`lower_field_assign`) AND compound `OP=` (`lower_compound_assign`) so the
/// two write-through fallbacks cannot drift (Core #4).
fn lower_field_object_operand(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
) -> Operand {
    if let Expr::Identifier(name) = &object.node {
        if let Some((local_id, _)) = ctx.lookup_local(name) {
            if ctx.is_param_borrow_unique(builder, local_id) {
                // Return the raw pointer local (not deref'd)
                Operand::Copy(Place::local(local_id))
            } else {
                lower_expr(ctx, builder, object)
            }
        } else if let Some(global_ptr) = materialize_global_field_base(ctx, builder, object) {
            // Bug #1: a module-level static base — materialize into an addressable
            // MutPtr local so the field-store writes THROUGH to the global.
            global_ptr
        } else {
            lower_expr(ctx, builder, object)
        }
    } else {
        lower_expr(ctx, builder, object)
    }
}

/// Result of resolving a field-store target reached through a POINTER operand
/// (the `try_resolve_field_place → None` fallback: a `.get().unwrap()` Ref
/// chain, a `&`/`!` unique-borrow param, a `Guard[T]` receiver, or a
/// materialized global base).
enum PtrFieldPlace {
    /// A write-through place was resolved: read/store here.
    Resolved(Place, TypeId),
    /// A `ReadGuard` receiver — writes are forbidden; emit no store.
    ReadGuardSkip,
    /// Could not resolve the field place (non-place operand / unknown type) —
    /// emit no store.
    Unresolved,
}

/// Resolve the deref'd field-store PLACE for `obj.field` when `obj` lowered to a
/// pointer operand. Faithful port of the `lower_field_assign` fallback's
/// resolution (`Guard[T]` auto-deref + the general pointee path), but it RETURNS
/// the place instead of emitting the store — so a read-modify-write caller
/// (compound `OP=`) can read AND write THROUGH the SAME place, evaluating the
/// base `.get()` exactly ONCE (no double-eval; the exact bug class filed at
/// TODO:282/865). The ONLY instruction it emits is the `Guard` get-ptr call in
/// the mutable-guard case — identical to the inline path — so plain `=` stays
/// byte-identical when routed through here. Shared by plain `=` and compound
/// `OP=` (Core #4 — one resolver, one class; the fallback-presence lint pins
/// both callers).
fn resolve_ptr_field_place(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    obj: &Operand,
    field_name: &str,
) -> PtrFieldPlace {
    use crate::ir::types::TypeDefKind;
    if let Operand::Copy(ref place) | Operand::Move(ref place) = *obj {
        let local_idx = place.local.0 as usize;
        if local_idx < builder.locals.len() {
            let local_type_id = builder.locals[local_idx].type_id;

            // Guard[T] auto-deref for writes: guard.field = val →
            // (*get_ptr(&guard)).field = val
            if let Some(type_name) = ctx.type_name_for_id(local_type_id) {
                let type_name = type_name.to_string();
                if let Some((inner_suffix, is_read_only)) = guard_inner_suffix(&type_name) {
                    if is_read_only {
                        // ReadGuard: writes are forbidden — skip.
                        return PtrFieldPlace::ReadGuardSkip;
                    }
                    let (inner_ptr_local, inner_type) = emit_guard_get_ptr(
                        ctx, builder, place, local_type_id, &type_name, inner_suffix,
                    );
                    let deref_place = Place {
                        local: inner_ptr_local,
                        projections: vec![Projection::Deref],
                    };
                    if let Some(inner_type_name) = ctx.type_name_for_id(inner_type) {
                        let inner_type_name = inner_type_name.to_string();
                        if let Some((field_idx, field_type)) = ctx.lookup_field(&inner_type_name, field_name) {
                            let mut target_place = deref_place;
                            target_place.projections.push(Projection::Field(field_idx));
                            return PtrFieldPlace::Resolved(target_place, field_type);
                        }
                        let inner_field: Option<(u32, TypeId)> = ctx.type_registry.get_type_def(&inner_type_name)
                            .and_then(|td| {
                                if let TypeDefKind::Struct(ref s) = td.kind {
                                    s.fields.iter().enumerate().find(|(_, f)| f.name == field_name)
                                        .map(|(i, f)| (i as u32, f.type_id))
                                } else {
                                    None
                                }
                            });
                        if let Some((field_idx, field_type)) = inner_field {
                            let mut target_place = deref_place;
                            target_place.projections.push(Projection::Field(field_idx));
                            return PtrFieldPlace::Resolved(target_place, field_type);
                        }
                    }
                    // Mutable Guard matched but the field is not on the inner
                    // struct — fall through to the general pointer path below
                    // (mirrors the inline plain-`=` fallback, which does NOT
                    // early-return here).
                }
            }

            // If the local is a pointer, dereference to get the struct type.
            let (effective_type_id, base_place) =
                if let Some(pointee) = ctx.pointee_type(local_type_id) {
                    let mut deref_place = place.clone();
                    deref_place.projections.push(Projection::Deref);
                    (pointee, deref_place)
                } else {
                    (local_type_id, place.clone())
                };

            if let Some(type_name) = ctx.type_name_for_id(effective_type_id) {
                let type_name = type_name.to_string();
                if let Some((field_idx, field_type)) = ctx.lookup_field(&type_name, field_name) {
                    let mut target_place = base_place;
                    target_place.projections.push(Projection::Field(field_idx));
                    return PtrFieldPlace::Resolved(target_place, field_type);
                }
                // Fallback: look up from TypeDef
                let field_match: Option<(u32, TypeId)> = ctx.type_registry.get_type_def(&type_name)
                    .and_then(|td| {
                        if let TypeDefKind::Struct(ref s) = td.kind {
                            s.fields.iter().enumerate().find(|(_, f)| f.name == field_name)
                                .map(|(i, f)| (i as u32, f.type_id))
                        } else {
                            None
                        }
                    });
                if let Some((field_idx, field_type)) = field_match {
                    let mut target_place = base_place;
                    target_place.projections.push(Projection::Field(field_idx));
                    return PtrFieldPlace::Resolved(target_place, field_type);
                }
            }
        }
    }
    PtrFieldPlace::Unresolved
}

/// Apply the 3-way ownership rule (auto-move if dead / auto-clone if live /
/// always-clone if borrow) to a field-store RHS. Mirrors
/// `ensure_owned_at_consuming_arg` semantics — used at every other consuming
/// position (`push`, `put`, `set`, `insert`, `send`, `v[i]=x`, `Struct(field)`,
/// `Variant(payload)`). Field-assign was the only outlier prior to 2026-05-05;
/// routing through the same helper closes snag #8 (heap corruption when a
/// non-Copy local was field-stored across alternative branches) by giving
/// each live-source case its own clone, and closes the user-reported
/// "Compiler internal panic" — `f.a = items; print(items)` previously
/// hit `local _N read after MoveZero` GIR validation.
fn clone_ptr_rhs_if_needed(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    rhs: &mut Operand,
    value: &Spanned<Expr>,
) {
    let taken = std::mem::replace(rhs, Operand::Constant(Constant::Unit));
    *rhs = ctx.ensure_owned_at_consuming_arg(
        builder, taken, value, crate::ir::ImplicitCloneReason::StructFieldFromBorrow,
    );
}

/// Emit a field store with full cleanup: drop old value, unregister string temps,
/// assign, move-zero the RHS temp to prevent scope-exit double-free.
fn emit_field_store_with_cleanup(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    target_place: &Place,
    field_type: TypeId,
    rhs: &Operand,
) {
    // Snag #32 (None-literal materialisation at writer boundaries):
    // chokepoint coercion for `Constant::Null → enum_init None` when the
    // store target is Option-typed. Every emit_field_store call routes
    // through here, so any future caller automatically gets the rewrite
    // — see `LoweringContext::coerce_null_to_option_none`. No-op for
    // non-Null operands and non-Option target types.
    let rhs_owned = ctx.coerce_null_to_option_none(builder, rhs.clone(), field_type);
    let rhs = &rhs_owned;
    emit_field_drop_if_needed(ctx, builder, target_place, field_type);
    maybe_unregister_string_temp(ctx, builder, rhs, field_type);
    maybe_unregister_owned_string_temp(ctx, builder, rhs, field_type);
    builder.assign(target_place.clone(), rhs.clone());
    if let Operand::Copy(ref p) | Operand::Move(ref p) = *rhs {
        if p.projections.is_empty()
            && !ctx.drops.is_moved(p.local)
            && ctx.drops.is_registered(p.local)
        {
            ctx.move_zero_and_mark(builder, p.local);
        }
    }
}

/// Resolve the operator-overload method for a compound-assign `OP=` on a Named
/// type, if one is defined (the `Type__method` mangled form, or an
/// `equip`-generated `_for_Type__method`). Factored out of the five open-coded
/// copies (the Identifier arm + the four place-based arms) so the lookup cannot
/// drift (Core #4 — one class, one resolver).
fn resolve_compound_overload(
    ctx: &LoweringContext,
    op: ast::BinaryOp,
    type_id: TypeId,
) -> Option<String> {
    let method = match op {
        ast::BinaryOp::Add => "add",
        ast::BinaryOp::Sub => "sub",
        ast::BinaryOp::Mul => "mul",
        ast::BinaryOp::Div => "div",
        ast::BinaryOp::Rem => "rem",
        ast::BinaryOp::Mod => "mod",
        _ => return None,
    };
    let type_name = match ctx.type_registry.get(type_id) {
        Some(GirType::Named(name)) => name.clone(),
        _ => return None,
    };
    let mangled = format!("{type_name}__{method}");
    if ctx.fn_sigs.contains_key(&mangled) {
        return Some(mangled);
    }
    let suffix = format!("_for_{type_name}__{method}");
    ctx.fn_sigs.keys().find(|k| k.ends_with(&suffix)).cloned()
}

/// Shared read-modify-write for a compound-assign (`place OP= rhs`) whose LHS
/// resolves to a STABLE write-through place — a struct field, a tuple field, or
/// a deref pointee. Called by all four place-based compound arms (Some /
/// None-fallback / tuple / deref), so the read-shape decision lives in ONE spot
/// (Core #4).
///
/// The R-STRING fix: a RESOURCE-typed field must NOT be read through an
/// intermediate `assign(cur, Copy(place))` — a shallow Copy of a bare
/// resource local trips the resource-move validator ("shallow copy of
/// resource", `validate.rs` `assign_read_site`), an ICE on typecheck-accepted
/// code. The result is written back through `emit_field_store_with_cleanup`,
/// the single place that does drop-old + move-new + coerce, so the drop of the
/// OLD value is uniform across String / overload / value fields. Docs:
/// `docs/language-reference.md` §9.6 (compound-assign is a materialize-on-write
/// position).
///
/// Read shape, chosen from the field's TYPE (typed predicates, never a name):
///   * String `+`      → pass `Operand::Copy(place)` DIRECTLY into
///                       `gorget_str_cat` (mirrors the Identifier arm); no
///                       intermediate copy. `gorget_str_cat` reads the old value
///                       BEFORE the cleanup-store drops it, and its fresh
///                       (`returns_fresh`) result does not alias the field.
///   * Array-kind `+`  → pass `Operand::Copy(place)` DIRECTLY into `bin_op`
///                       Add (LIR CollectionKind::Array → clone+extend); no
///                       intermediate shallow Copy. Cleanup-store drops the
///                       old buffer when writing the fresh concat result.
///   * resource + `OP` overload → `emit_borrow(place)` for the `self` arg. The
///                       overload takes `self` by borrow, so it does not consume
///                       the old value before the cleanup-store drops it.
///   * value type      → the plain shallow `Operand::Copy(place)` read is safe
///                       (non-droppable); kept READ-first (byte-identical to the
///                       pre-fix value path; the resource paths become RHS-first
///                       — the eval-order split RV-C already tracks, TODO:285).
fn emit_compound_place_rmw(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    field_place: &Place,
    field_type: TypeId,
    op: ast::BinaryOp,
    value: &Spanned<Expr>,
) {
    // String concatenation: `field += str` → gorget_str_cat. Read the old value
    // as `Operand::Copy(field_place)` DIRECTLY at the extern call (no shallow
    // intermediate — that intermediate IS the ICE), then write the fresh result
    // back through the cleanup-store (drops the old value).
    let is_string = field_type == ctx.type_mapper.owned_string_type;
    if is_string && matches!(op, ast::BinaryOp::Add) {
        let owned_type = ctx.type_mapper.owned_string_type;
        let rhs = lower_expr(ctx, builder, value);
        let tmp = ctx.call_extern_tracked(
            builder,
            "gorget_str_cat",
            vec![Operand::Copy(field_place.clone()), rhs],
            owned_type,
        );
        emit_field_store_with_cleanup(
            ctx, builder, field_place, field_type, &FunctionBuilder::copy(tmp),
        );
        return;
    }

    // Array-kind (Vector/Deque/GorgetArray) concat: `field += other` → bin_op
    // Add (LIR clone+extend). Pass the field place DIRECTLY into bin_op — no
    // intermediate shallow Copy (that intermediate IS the resource ICE). The
    // cleanup-store drops the old buffer when writing the fresh concat result.
    // Typed `collection_kind` — never name-match.
    let is_array_kind = ctx.type_registry.collection_kind(field_type)
        == Some(CollectionKind::Array);
    if is_array_kind && matches!(op, ast::BinaryOp::Add) {
        let rhs = lower_expr(ctx, builder, value);
        let result = builder.bin_op(
            BinOp::Add,
            field_type,
            Operand::Copy(field_place.clone()),
            rhs,
        );
        emit_field_store_with_cleanup(
            ctx, builder, field_place, field_type, &FunctionBuilder::copy(result),
        );
        return;
    }

    // A resource field (has a drop strategy / Resource copy-semantics) cannot be
    // read via the shallow `assign(cur, Copy)`; when an operator overload exists,
    // borrow the field place directly for `self`. Same typed predicate the
    // Identifier arm uses for its move/copy decision (`is_resource_type ||
    // needs_drop`, functions.rs:28) — no name-matching.
    let is_resource = ctx.type_registry.is_resource_type(field_type)
        || ctx.type_registry.needs_drop(field_type);

    if let Some(effective_name) = resolve_compound_overload(ctx, op, field_type) {
        let (self_ptr, rhs) = if is_resource {
            // RHS-first: borrow the stable field place directly (no shallow copy).
            let rhs = lower_expr(ctx, builder, value);
            let ptr_type = ctx.register_ptr_type(field_type);
            let ptr_local = builder.add_local(ptr_type, None);
            builder.emit_borrow(ptr_local, field_place.clone());
            (FunctionBuilder::copy(ptr_local), rhs)
        } else {
            // Value type with an overload: the shallow read is safe; keep it
            // READ-first (borrow the read temp) to preserve the prior eval order.
            let cur = builder.add_local(field_type, None);
            builder.assign(Place::local(cur), Operand::Copy(field_place.clone()));
            let rhs = lower_expr(ctx, builder, value);
            let ptr_type = ctx.register_ptr_type(field_type);
            let ptr_local = builder.add_local(ptr_type, None);
            builder.emit_borrow(ptr_local, Place::local(cur));
            (FunctionBuilder::copy(ptr_local), rhs)
        };
        let result = builder.call(effective_name, vec![self_ptr, rhs], field_type);
        emit_field_store_with_cleanup(
            ctx, builder, field_place, field_type, &FunctionBuilder::copy(result),
        );
        return;
    }

    // Primitive binop path — value types only. Kept READ-first.
    // NOTE: non-Add resource/String OP= is typecheck-rejected
    // (`E_UnsupportedOperator`). Array-kind `+=` and String `+=` / Add overload
    // route above (String-Add special case, Array-kind bin_op, overload resolve).
    // Reaching this shallow `Copy` of a resource is a typecheck bug — not an
    // expected path.
    let cur = builder.add_local(field_type, None);
    builder.assign(Place::local(cur), Operand::Copy(field_place.clone()));
    let rhs = lower_expr(ctx, builder, value);
    let gir_op = compound_op_to_gir(op);
    let result = builder.bin_op(gir_op, field_type, FunctionBuilder::copy(cur), rhs);
    emit_field_store_with_cleanup(
        ctx, builder, field_place, field_type, &FunctionBuilder::copy(result),
    );
}

/// If the RHS is a bare GorgetString local being assigned to a GorgetString field,
/// unregister it from drop tracking to prevent double-free. The field now owns the
/// data; the temp should not be freed when it goes out of scope.
fn maybe_unregister_owned_string_temp(
    ctx: &mut LoweringContext,
    builder: &FunctionBuilder,
    rhs: &Operand,
    target_type: TypeId,
) {
    if target_type != ctx.type_mapper.owned_string_type {
        return;
    }
    let place = match rhs {
        Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => place,
        _ => return,
    };
    if builder.local_type(place.local) == ctx.type_mapper.owned_string_type {
        ctx.drops.unregister(place.local);
    }
}

/// If a GorgetString temp is being assigned to another string local,
/// unregister the temp from drop tracking. The CoW alias shares the
/// underlying buffer — the new owner is responsible for freeing it.
fn maybe_unregister_string_temp(
    ctx: &mut LoweringContext,
    builder: &FunctionBuilder,
    rhs: &Operand,
    target_type: TypeId,
) {
    if target_type != ctx.type_mapper.owned_string_type {
        return;
    }
    let place = match rhs {
        Operand::Copy(place) | Operand::Move(place) => Some(place),
        _ => None,
    };
    if let Some(place) = place {
        if place.projections.is_empty() {
            let rhs_type = builder.local_type(place.local);
            if rhs_type == ctx.type_mapper.owned_string_type {
                ctx.drops.unregister(place.local);
            }
        }
    }
}

/// Build the mutable-self pointer arg for an index-assign setter dispatch
/// (`Vector__set` / `Dict__put` / user `IndexMut::set`).
///
/// When `obj` is ALREADY a pointer to the collection — a nested-index element
/// handle (`m[i]` for a resource element lowers to `Ptr(inner)`) — pass that
/// pointer DIRECTLY. Taking its address (`&place`) double-indirects: the setter
/// receives `&handle` and reads it as the collection struct, an over-read past
/// the 8-byte pointer slot (heap/stack-buffer-overflow). The LIR passthrough
/// that elides `&` on an SSA-temp pointer does NOT fire for a materialized
/// MUTABLE local (the shape the G1 projected-root materialize produces for
/// `m[i][j] = x`), so the elision must be explicit here. Mirrors the method-call
/// `is_ptr` receiver fast-path (exprs/methods.rs). Otherwise (a value place),
/// borrow it mutably.
fn index_assign_self_ptr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    place: &Place,
    obj_type: TypeId,
    obj_is_ptr: bool,
) -> Operand {
    if obj_is_ptr && place.projections.is_empty() {
        FunctionBuilder::copy(place.local)
    } else {
        let ptr_type = ctx.register_mut_ptr_type(obj_type);
        let ptr_local = builder.add_local(ptr_type, None);
        builder.emit_borrow_mut(ptr_local, place.clone());
        FunctionBuilder::copy(ptr_local)
    }
}

/// Lower an index assignment: `obj[index] = value`
pub(super) fn lower_index_assign(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    index: &Spanned<Expr>,
    value: &Spanned<Expr>,
) {
    // CoW: an index write mutates the object. Materialize the immutable-in-context
    // ROOT (decide-at-root, Core #1/#4); a no-op on `&`/owned roots keeps
    // `&`-chain write-through. Routed through the SHARED assign-target-root
    // prologue (planner round 3) — identical dispatch to `lower_field_assign` /
    // the compound path (root-first, then field-path sever). One class, one
    // helper, one plan lookup.
    materialize_assign_target_root(ctx, builder, object);

    // When the object is a struct field access (e.g. self.dict_field[key] = val),
    // resolve the field to a Place in-place to avoid copying the Dict struct.
    // This ensures hash table resizes and metadata updates propagate to the original.
    //
    // CoW UAF fix (round-33, class fix): snapshot the WHOLE statement's lowering
    // range (object chain + index + RHS value). A projected object
    // (`m[i][j] = x`, `m[i][j][k] = x`) lowers `m[i]` → h1, `m[i][j]` → h2, …,
    // AND the RHS/index may mint element-refs into the SAME collection
    // (`m[0][0] = m[1][0]`) — EACH a transient CollectionElement/FieldPath handle,
    // all dead after this statement. The untrack runs at the END (after the
    // setter's `ensure_owned_at_consuming_arg` has cloned the stored value), so
    // the remaining handles are dead READ refs. (Sibling of lower_field_assign.)
    let stmt_locals_start = builder.locals.len();
    let (obj, resolved_field_type) = if let Expr::FieldAccess { object: inner_obj, field } = &object.node {
        if let Some((field_place, field_type)) = try_resolve_field_place(ctx, builder, inner_obj, &field.node) {
            (Operand::Copy(field_place), Some(field_type))
        } else {
            (lower_expr(ctx, builder, object), None)
        }
    } else if let Some(global_ptr) = materialize_global_field_base(ctx, builder, object) {
        // T5: bare-identifier module-level `static` base (`SHARED[i] = x`). A plain
        // `lower_expr` yields a `GlobalRef` Constant, NOT a Copy/Move Place, so the
        // setter's `Operand::Copy | Move` guard below silently DROPS the store (the
        // write-back is lost — reads see the un-mutated static). Materialize the
        // static into an addressable `MutPtr(<coll>)` local (mirrors
        // lower_field_assign:667) so the setter writes THROUGH to the global. Track A's
        // mutation prescan records index-stored statics as mutated, so the static is
        // already emitted with writable imperative storage — this only closes the
        // write path.
        (global_ptr, None)
    } else {
        (lower_expr(ctx, builder, object), None)
    };
    let idx = lower_expr(ctx, builder, index);

    // Determine the receiver type to dispatch correctly.
    // Use the resolved field type if we resolved through a field access,
    // since infer_operand_type_full doesn't walk projections.
    let obj_type_raw = resolved_field_type.unwrap_or_else(|| infer_operand_type_full(ctx, &obj, builder));
    let obj_type = ctx.pointee_type(obj_type_raw).unwrap_or(obj_type_raw);
    // When `obj` is ALREADY a pointer to the collection, the setter's
    // mutable-self arg is that pointer DIRECTLY — see `index_assign_self_ptr`.
    let obj_is_ptr = matches!(
        ctx.type_registry.get(obj_type_raw),
        Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))
    );

    // Propagate the collection's element/value type as `expected_type` so an
    // empty `[]` / `{}` RHS sizes its allocation correctly. Without this,
    // `Dict[String, Vector[String]] g = {}; g["k"] = []` lowers `[]` with
    // expected_type=None → elem_size=sizeof(I64)=8, silently truncating
    // 32-byte Str elements on subsequent push. Mirrors the bare-init path
    // (line 109) which sets expected_type = declared local type.
    let value_expected_type = infer_collection_element_type(ctx, obj_type);
    let prev_expected = ctx.func_state.expected_type;
    ctx.func_state.expected_type = Some(value_expected_type);
    let val = lower_expr(ctx, builder, value);
    ctx.func_state.expected_type = prev_expected;

    let type_name = ctx.type_name_for_id(obj_type).unwrap_or("").to_string();
    // Read typed `collection_kind` from TypeMetadata (Phase A) instead of
    // matching `type_name.starts_with("Vector__"/"Dict__"/...)`. The kind
    // covers Vector/Deque/GorgetArray as Array; Dict as OrderedMap; HashMap/
    // GorgetMap as Map; Set as OrderedSet; HashSet/GorgetSet as Set.
    let kind = ctx.type_registry.get_type_def(&type_name)
        .and_then(|td| td.metadata.collection_kind);
    let is_vector = kind == Some(CollectionKind::Array);
    let is_dict = matches!(kind, Some(CollectionKind::OrderedMap) | Some(CollectionKind::Map));

    // Auto-clone borrow sources for consuming args at `Dict[k]=v` / `Vec[i]=v` sites.
    // The runtime is symmetric with gorget_array_push: gorget_map_put / _set /
    // _insert take ownership via raw memcpy without an internal clone, so the
    // compiler must guarantee independence at the call site. Delegate the
    // decision to the shared `ensure_owned_at_consuming_arg` helper — same rule
    // used by `lower_method_call` for push/put/set method calls.
    //
    // Helper: emit MoveZero on an operand's local after the call to transfer
    // ownership to the collection. For resource-typed locals that are the
    // source of a consuming arg, the slot's bytes are memcpy'd into the
    // collection — zeroing the source prevents the scope-exit drop from
    // double-freeing. Skips Ptr-wrapped locals (already materialized) and
    // primitives.
    //
    // Cluster 5 widening (2026-05-10): the gate is `needs_drop` (via
    // is_resource_or_contains_resource), not the narrow is_resource_type.
    // For `v[i] = opt_vec` where opt_vec is Option[Vector[int]], the wrapper
    // contains a heap pointer transitively — without MoveZero, the caller's
    // drop at scope-exit and the collection's drop of the inserted payload
    // race on the same heap allocation. Mirrors the same widening at
    // exprs/calls.rs sites 678/1080 and exprs/spawn.rs:450 Cluster 3.
    let maybe_move_zero = |ctx: &mut LoweringContext,
                           builder: &mut FunctionBuilder,
                           op: &Operand| {
        if let Operand::Copy(place) | Operand::Move(place) = op {
            if place.projections.is_empty()
                && ctx.type_registry.is_resource_or_contains_resource(
                    builder.local_type(place.local))
            {
                let ty = builder.local_type(place.local);
                let is_ptr = matches!(
                    ctx.type_registry.get(ty),
                    Some(crate::ir::types::GirType::Ptr(_)) | Some(crate::ir::types::GirType::MutPtr(_))
                );
                if !is_ptr {
                    builder.move_zero(Place::local(place.local));
                    ctx.drops.mark_moved(place.local);
                }
            }
        }
    };

    if is_vector {
        // Vector[i] = val → Vector__T__set(&arr, index, val)
        let val = ctx.ensure_owned_at_consuming_arg(
            builder, val, value, crate::ir::ImplicitCloneReason::ConsumingArg);
        if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
            let self_ptr = index_assign_self_ptr(ctx, builder, place, obj_type, obj_is_ptr);
            let mangled = format!("{type_name}__set");
            builder.call_void(
                mangled,
                vec![self_ptr, idx, val.clone()],
            );
            maybe_move_zero(ctx, builder, &val);
        }
    } else if is_dict {
        // Dict[key] = val → Dict__K__V__put(&dict, key, val)
        let idx = ctx.ensure_owned_at_consuming_arg(
            builder, idx, index, crate::ir::ImplicitCloneReason::ConsumingArg);
        let val = ctx.ensure_owned_at_consuming_arg(
            builder, val, value, crate::ir::ImplicitCloneReason::ConsumingArg);
        if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
            let self_ptr = index_assign_self_ptr(ctx, builder, place, obj_type, obj_is_ptr);
            let mangled = format!("{type_name}__put");
            builder.call_void(
                mangled,
                vec![self_ptr, idx.clone(), val.clone()],
            );
            maybe_move_zero(ctx, builder, &idx);
            maybe_move_zero(ctx, builder, &val);
        }
    } else {
        // Check for IndexMut / set equip method (operator overload)
        if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
            let candidates = [
                format!("{type_name}__set"),
                format!("IndexMut_for_{type_name}__set"),
                format!("{type_name}____setitem__"),
            ];
            for set_name in &candidates {
                if ctx.fn_sigs.contains_key(set_name.as_str()) {
                    let self_ptr = index_assign_self_ptr(ctx, builder, place, obj_type, obj_is_ptr);
                    builder.call_void(
                        set_name.clone(),
                        vec![self_ptr, idx, val],
                    );
                    // Untrack the whole statement's transient read handles before
                    // returning (see the end-of-fn call for the fall-through paths).
                    ctx.untrack_transient_element_refs_in_range(builder, stmt_locals_start, builder.locals.len());
                    return;
                }
            }
            // HARD ICE, never a silent no-op (Chain C item 6): every
            // index-assign target the typechecker accepts must dispatch
            // to a setter here. Strings are rejected at check time
            // (`SemanticErrorKind::StringIndexAssign`); a type with no
            // setter reaching this point means typecheck and lowering
            // disagree — dropping the write silently was how
            // `s[0] = "x"` compiled as a no-op.
            panic!(
                "BUG: index-assign on `{type_name}` found no setter \
                 (tried {candidates:?}) — typecheck accepted an \
                 index-assign the lowering cannot dispatch"
            );
        }
    }
    // Untrack EVERY transient element/field-path handle minted across the WHOLE
    // statement (object chain + index + RHS), at the END so the setter's
    // `ensure_owned_at_consuming_arg` has already cloned the stored value — every
    // remaining handle in range is a dead READ ref. An RHS/index element-ref into
    // the SAME collection this store root-materialized (`m[0][0] = m[1][0]` + a
    // later `m.push()`) would otherwise dangle when the push reallocs the private
    // copy (Case 3 clones freed memory). Store-neutral: the setter borrowed the
    // Place, never read the ownership tag.
    ctx.untrack_transient_element_refs_in_range(builder, stmt_locals_start, builder.locals.len());
}

/// Lower a compound assignment (e.g., `x += 1`).
pub(super) fn lower_compound_assign(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    target: &Spanned<Expr>,
    op: ast::BinaryOp,
    value: &Spanned<Expr>,
) {
    if let Expr::Identifier(name) = &target.node {
        if let Some((local_id, type_id)) = ctx.lookup_local(name) {
            // Shared variable: dispatch based on wrapper kind
            if let Some(info) = ctx.shared.locals.get(&local_id) {
                let (hidden_local, inner_type, kind) = (info.hidden_local, info.inner_type, info.kind);
                match kind {
                    SharedLocalKind::Mutex => {
                        let inner_c = ctx.c_type_name_for_id(inner_type);
                        let mutex_type = ctx.type_mapper.lookup_named(&format!("Mutex__{inner_c}")).unwrap_or(inner_type);
                        let cur_val = emit_shared_mutex_lock_get(ctx, builder, hidden_local, mutex_type, inner_type);
                        let rhs = lower_expr(ctx, builder, value);
                        let gir_op = match op {
                            ast::BinaryOp::Add => BinOp::Add,
                            ast::BinaryOp::Sub => BinOp::Sub,
                            ast::BinaryOp::Mul => BinOp::Mul,
                            ast::BinaryOp::Div => BinOp::Div,
                            ast::BinaryOp::Rem => BinOp::Rem,
                            ast::BinaryOp::Mod => BinOp::Mod,
                            ast::BinaryOp::BitAnd => BinOp::BitAnd,
                            ast::BinaryOp::BitOr => BinOp::BitOr,
                            ast::BinaryOp::BitXor => BinOp::BitXor,
                            ast::BinaryOp::Shl => BinOp::Shl,
                            ast::BinaryOp::Shr => BinOp::Shr,
                            _ => BinOp::Add,
                        };
                        let new_val = builder.bin_op(gir_op, inner_type, cur_val, rhs);
                        emit_shared_mutex_lock_set(ctx, builder, hidden_local, mutex_type, inner_type, FunctionBuilder::copy(new_val));
                        return;
                    }
                    SharedLocalKind::Atomic => {
                        // For += and -=, use native atomic add/sub (lock-free)
                        // For other ops, fall back to load → compute → CAS loop
                        let rhs = lower_expr(ctx, builder, value);
                        let atomic_name = atomic_type_name_for(inner_type);
                        match op {
                            ast::BinaryOp::Add => {
                                let add_fn = format!("{atomic_name}__add");
                                builder.call(&add_fn, vec![FunctionBuilder::copy(hidden_local), rhs], inner_type);
                                return;
                            }
                            ast::BinaryOp::Sub => {
                                let sub_fn = format!("{atomic_name}__sub");
                                builder.call(&sub_fn, vec![FunctionBuilder::copy(hidden_local), rhs], inner_type);
                                return;
                            }
                            _ => {
                                // Fallback: atomic load → compute → atomic store (NOT atomic, but functional)
                                let cur_val = emit_atomic_load(ctx, builder, hidden_local, inner_type, &atomic_name);
                                let gir_op = match op {
                                    ast::BinaryOp::Mul => BinOp::Mul,
                                    ast::BinaryOp::Div => BinOp::Div,
                                    ast::BinaryOp::Rem => BinOp::Rem,
                                    ast::BinaryOp::Mod => BinOp::Mod,
                                    ast::BinaryOp::BitAnd => BinOp::BitAnd,
                                    ast::BinaryOp::BitOr => BinOp::BitOr,
                                    ast::BinaryOp::BitXor => BinOp::BitXor,
                                    ast::BinaryOp::Shl => BinOp::Shl,
                                    ast::BinaryOp::Shr => BinOp::Shr,
                                    _ => BinOp::Add,
                                };
                                let new_val = builder.bin_op(gir_op, inner_type, cur_val, rhs);
                                emit_atomic_store(ctx, builder, hidden_local, FunctionBuilder::copy(new_val), &atomic_name);
                                return;
                            }
                        }
                    }
                    SharedLocalKind::RwLock => {
                        // Write-lock, get current value, compute, set, release — all under one lock
                        let (guard_ptr, cur_val) = emit_rwlock_write_get(ctx, builder, hidden_local, inner_type);
                        let rhs = lower_expr(ctx, builder, value);
                        let gir_op = match op {
                            ast::BinaryOp::Add => BinOp::Add,
                            ast::BinaryOp::Sub => BinOp::Sub,
                            ast::BinaryOp::Mul => BinOp::Mul,
                            ast::BinaryOp::Div => BinOp::Div,
                            ast::BinaryOp::Rem => BinOp::Rem,
                            ast::BinaryOp::Mod => BinOp::Mod,
                            ast::BinaryOp::BitAnd => BinOp::BitAnd,
                            ast::BinaryOp::BitOr => BinOp::BitOr,
                            ast::BinaryOp::BitXor => BinOp::BitXor,
                            ast::BinaryOp::Shl => BinOp::Shl,
                            ast::BinaryOp::Shr => BinOp::Shr,
                            _ => BinOp::Add,
                        };
                        let new_val = builder.bin_op(gir_op, inner_type, cur_val, rhs);
                        emit_rwlock_write_finish(ctx, builder, guard_ptr, inner_type, FunctionBuilder::copy(new_val));
                        return;
                    }
                    SharedLocalKind::SharedArc => {
                        // ArcOnly: compound-assign shouldn't happen (CFA upgrades to ArcMutex)
                    }
                }
            }

            let is_mut_capture = ctx.is_param_borrow_unique(builder, local_id);
            let value_type = if is_mut_capture {
                // The local's GIR type is MutPtr(T); pointee_type returns T.
                ctx.pointee_type(builder.local_type(local_id)).unwrap_or(type_id)
            } else {
                type_id
            };
            // Read current value (deref if mutable capture)
            let cur_val = if is_mut_capture {
                let deref_place = Place {
                    local: local_id,
                    projections: vec![Projection::Deref],
                };
                let tmp = builder.add_local(value_type, None);
                builder.assign(Place::local(tmp), Operand::Copy(deref_place));
                FunctionBuilder::copy(tmp)
            } else {
                FunctionBuilder::copy(local_id)
            };

            let rhs = lower_expr(ctx, builder, value);
            let is_string = value_type == ctx.type_mapper.owned_string_type;

            // String concatenation via += → gorget_str_cat (returns GorgetString)
            if is_string && matches!(op, ast::BinaryOp::Add) {
                let owned_type = ctx.type_mapper.owned_string_type;
                let tmp = ctx.call_extern_tracked(builder, "gorget_str_cat", vec![cur_val, rhs], owned_type);
                let dst = if is_mut_capture {
                    Place { local: local_id, projections: vec![Projection::Deref] }
                } else {
                    Place::local(local_id)
                };
                // Phase C: tmp is fresh from gorget_str_cat, dead after this
                // single assign — Move transfers ownership.
                builder.assign_mode(
                    crate::ir::instructions::AssignMode::Move,
                    dst,
                    FunctionBuilder::copy(tmp),
                );
                // Mark the temp as moved so the drop elaborator doesn't free it
                // (the destination variable now owns the GorgetString)
                ctx.move_zero_and_mark(builder, tmp);
            // Lazy loop-carried CoW (W4 write-site clearing): compound assign
            // writes the local wholesale — same clearing as `lower_assign`'s
            // Identifier arm (which see), AFTER the RHS ran. This is the
            // string-concat fast path's sibling clear: the path RETURNS
            // EARLY, so the generic-tail clear below never runs for it.
            if ctx.func_state.cow_lazy_mat_flag.remove(&local_id).is_some()
                && ctx.collection_ref_source(builder, local_id).is_some()
            {
                ctx.unset_ownership(builder, local_id);
            }
                return;
            }

            // Array-kind (Vector/Deque/GorgetArray) concat via += → bin_op Add
            // (LIR clone+extend). Same semantics as `a = a + b`. Drop the old
            // owned value BEFORE rebinding so the prior buffer doesn't leak
            // (mirror plain-assign drop-old). Typed `collection_kind` — never
            // name-match. Non-Add Array ops stay typecheck-rejected.
            let is_array_kind = ctx.type_registry.collection_kind(value_type)
                == Some(CollectionKind::Array);
            if is_array_kind && matches!(op, ast::BinaryOp::Add) {
                let tmp = builder.bin_op(BinOp::Add, value_type, cur_val, rhs);
                // Drop old AFTER computing new, BEFORE assign (plain assign
                // mirror at lower_assign Identifier arm). Mut-capture writes
                // through the pointee place.
                let dst = if is_mut_capture {
                    Place { local: local_id, projections: vec![Projection::Deref] }
                } else {
                    Place::local(local_id)
                };
                if ctx.type_registry.needs_drop(value_type) {
                    if is_mut_capture {
                        builder.drop(dst.clone());
                    } else if ctx.drops.is_moved(local_id) {
                        builder.drop_if_alive(Place::local(local_id));
                    } else {
                        builder.drop(Place::local(local_id));
                    }
                }
                builder.assign_mode(
                    crate::ir::instructions::AssignMode::Move,
                    dst,
                    FunctionBuilder::copy(tmp),
                );
                if ctx.func_state.cow_lazy_mat_flag.remove(&local_id).is_some()
                    && ctx.collection_ref_source(builder, local_id).is_some()
                {
                    ctx.unset_ownership(builder, local_id);
                }
                return;
            }

            // Check for operator overload on Named types
            let overload_method = match op {
                ast::BinaryOp::Add => Some("add"),
                ast::BinaryOp::Sub => Some("sub"),
                ast::BinaryOp::Mul => Some("mul"),
                ast::BinaryOp::Div => Some("div"),
                ast::BinaryOp::Rem => Some("rem"),
                ast::BinaryOp::Mod => Some("mod"),
                _ => None,
            }.and_then(|method| {
                if let Some(GirType::Named(type_name)) = ctx.type_registry.get(value_type).cloned() {
                    let mangled = format!("{type_name}__{method}");
                    let has_method = ctx.fn_sigs.contains_key(&mangled)
                        || ctx.fn_sigs.keys().any(|k| k.ends_with(&format!("_for_{type_name}__{method}")));
                    if has_method {
                        let effective_name = if ctx.fn_sigs.contains_key(&mangled) {
                            mangled
                        } else {
                            ctx.fn_sigs.keys()
                                .find(|k| k.ends_with(&format!("_for_{type_name}__{method}")))
                                .cloned()
                                .unwrap_or(mangled)
                        };
                        Some(effective_name)
                    } else { None }
                } else { None }
            });

            let tmp = if let Some(effective_name) = overload_method {
                // Borrow lhs for self parameter
                let self_ptr = if let Operand::Copy(ref place) | Operand::Move(ref place) = cur_val {
                    let ptr_type = ctx.register_ptr_type(value_type);
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow(ptr_local, place.clone());
                    FunctionBuilder::copy(ptr_local)
                } else {
                    cur_val
                };
                builder.call(effective_name, vec![self_ptr, rhs], value_type)
            } else {
                let gir_op = match op {
                    ast::BinaryOp::Add => BinOp::Add,
                    ast::BinaryOp::Sub => BinOp::Sub,
                    ast::BinaryOp::Mul => BinOp::Mul,
                    ast::BinaryOp::Div => BinOp::Div,
                    ast::BinaryOp::Rem => BinOp::Rem,
                    ast::BinaryOp::Mod => BinOp::Mod,
                    ast::BinaryOp::AddWrap => BinOp::AddWrap,
                    ast::BinaryOp::SubWrap => BinOp::SubWrap,
                    ast::BinaryOp::MulWrap => BinOp::MulWrap,
                    ast::BinaryOp::BitAnd => BinOp::BitAnd,
                    ast::BinaryOp::BitOr => BinOp::BitOr,
                    ast::BinaryOp::BitXor => BinOp::BitXor,
                    ast::BinaryOp::Shl => BinOp::Shl,
                    ast::BinaryOp::Shr => BinOp::Shr,
                    _ => BinOp::Add, // fallback
                };
                builder.bin_op(gir_op, value_type, cur_val, rhs)
            };
            let dst = if is_mut_capture {
                Place { local: local_id, projections: vec![Projection::Deref] }
            } else {
                Place::local(local_id)
            };
            // Phase C: tmp is fresh op-result (binop or overload call), dead
            // after this assign. Move for resource types, Copy for primitives.
            // Cluster 5 probe (2026-05-10): the disjunction
            // `is_resource_type || needs_drop` is NOT redundant. See
            // `lowering/functions.rs:28` for the full reasoning
            // (upgrade-scan-dependent `needs_drop` vs upgrade-scan-independent
            // transitive `is_resource_type`).
            let cmp_mode = if ctx.type_registry.is_resource_type(value_type)
                || ctx.type_registry.needs_drop(value_type)
            {
                crate::ir::instructions::AssignMode::Move
            } else {
                crate::ir::instructions::AssignMode::Copy
            };
            builder.assign_mode(cmp_mode, dst, FunctionBuilder::copy(tmp));
            // Lazy loop-carried CoW (W4 write-site clearing): generic compound
            // tail — see the fast-path clear above and `lower_assign`'s
            // Identifier arm for the rationale.
            if ctx.func_state.cow_lazy_mat_flag.remove(&local_id).is_some()
                && ctx.collection_ref_source(builder, local_id).is_some()
            {
                ctx.unset_ownership(builder, local_id);
            }
        } else if ctx.global_names.contains(name.as_str()) {
            // Module-level static variable — read via GlobalRef, compute, write via GlobalAssign
            let cur_val = Operand::Constant(Constant::GlobalRef(name.clone()));
            let rhs = lower_expr(ctx, builder, value);
            // Determine a type for the binop — look up from global_type_names
            let value_type = ctx.global_type_names.get(name)
                .and_then(|tn| ctx.type_mapper.lookup_named(tn))
                .unwrap_or(I64_TYPE);
            let gir_op = match op {
                ast::BinaryOp::Add => BinOp::Add,
                ast::BinaryOp::Sub => BinOp::Sub,
                ast::BinaryOp::Mul => BinOp::Mul,
                ast::BinaryOp::Div => BinOp::Div,
                ast::BinaryOp::Rem => BinOp::Rem,
                ast::BinaryOp::Mod => BinOp::Mod,
                ast::BinaryOp::AddWrap => BinOp::AddWrap,
                ast::BinaryOp::SubWrap => BinOp::SubWrap,
                ast::BinaryOp::MulWrap => BinOp::MulWrap,
                ast::BinaryOp::BitAnd => BinOp::BitAnd,
                ast::BinaryOp::BitOr => BinOp::BitOr,
                ast::BinaryOp::BitXor => BinOp::BitXor,
                ast::BinaryOp::Shl => BinOp::Shl,
                ast::BinaryOp::Shr => BinOp::Shr,
                _ => BinOp::Add,
            };
            let tmp = builder.bin_op(gir_op, value_type, cur_val, rhs);
            builder.global_assign(name.clone(), FunctionBuilder::copy(tmp));
        }
    } else if let Expr::FieldAccess { object, field } = &target.node {
        // Compound assign on struct field: obj.field OP= val
        // Desugar to: read field → compute → write field back
        // SCOUT-PROTO #1: root-materialize prologue (mirror lower_field_assign
        // 604-625) — the compound arm previously skipped it, so `v[i].n += 1` /
        // `s.field += x` on a bare param WROTE THROUGH the caller.
        materialize_assign_target_root(ctx, builder, object);
        // Snapshot for the same round-33 CoW untrack the plain field-assign does:
        // `try_resolve_field_place`'s `Expr::Index` arm resolves `v[i].field OP= x`
        // / `PTS[i].field OP= x` to a write-through element pointer, and a
        // multilevel base (`m[i][j].field OP= x`) mints a transient
        // CollectionElement handle that would dangle on a later same-collection
        // mutation if left CoW-tracked.
        let stmt_locals_start = builder.locals.len();
        if let Some((field_place, field_type)) = try_resolve_field_place(ctx, builder, object, &field.node) {
            // R-STRING: resource-safe read-modify-write via the shared helper
            // (String → gorget_str_cat, resource overload → borrow-read, value →
            // shallow read; drop-old handled uniformly by the cleanup store).
            // Replaces the open-coded shallow `assign(cur, Copy(field_place))`
            // that ICE'd the resource-move validator on a resource field.
            emit_compound_place_rmw(ctx, builder, &field_place, field_type, op, value);
            ctx.untrack_transient_element_refs_in_range(builder, stmt_locals_start, builder.locals.len());
        } else {
            // None: the object is a method-call chain (`coll.get(i).unwrap()`)
            // or another non-nameable place, so `try_resolve_field_place`
            // couldn't resolve it. Lower it to a POINTER (auto-borrow-from-get
            // yields a Ref), resolve the deref'd field place, and read-modify-
            // write THROUGH it. Without this branch the write was SILENTLY
            // DROPPED (Core #10; the T2 miscompile: `coll.get(i).unwrap().f += 1`
            // printed the unchanged value). Mirrors the plain-`=` write-through
            // fallback (`lower_field_assign`) — the same
            // `lower_field_object_operand` + `resolve_ptr_field_place` shared
            // resolver (Core #4). The place is resolved ONCE and reused for the
            // read AND the store, so the base `.get()` is evaluated exactly once
            // (no double-eval — TODO:282's class).
            let obj = lower_field_object_operand(ctx, builder, object);
            match resolve_ptr_field_place(ctx, builder, &obj, &field.node) {
                PtrFieldPlace::Resolved(field_place, field_type) => {
                    // R-STRING: resource-safe read-modify-write via the shared
                    // helper (same String/overload/value split + cleanup-store
                    // drop-old as the Some-arm). Reads THROUGH the resolved
                    // write-through place; no shallow resource copy.
                    emit_compound_place_rmw(ctx, builder, &field_place, field_type, op, value);
                }
                // ReadGuard: writes forbidden — skip.
                PtrFieldPlace::ReadGuardSkip => {}
                // Unknown / non-place object — no store.
                PtrFieldPlace::Unresolved => {}
            }
            ctx.untrack_transient_element_refs_in_range(builder, stmt_locals_start, builder.locals.len());
        }
    } else if let Expr::Index { object, index } = &target.node {
        // Compound assign on index: obj[i] OP= val
        // Desugar to: current = obj[i]; result = current OP val; obj[i] = result
        //
        // SCOUT-PROTO #1: root-materialize prologue (mirror lower_index_assign
        // 931-947) — the compound arm previously skipped it, so `xs[0] += 1` /
        // `s.counts[0] += 1` on a bare param WROTE THROUGH the caller.
        materialize_assign_target_root(ctx, builder, object);
        //
        // Sibling of the FieldAccess arm: the `try_resolve_field_place` call
        // below (`m[i].field[key] OP= x`) can fire the new `Expr::Index` arm when
        // `inner_obj` is itself an index (`m[j].field[key]`), minting the same
        // transient CollectionElement handle the round-33 untrack clears. Snapshot
        // here and untrack at the arm's exit so it can't dangle on a later
        // same-collection mutation (Core #4, one fix all siblings).
        let stmt_locals_start = builder.locals.len();

        // Resolve the object — handle field access (self.vec) by resolving in-place
        let (obj, resolved_field_type) = if let Expr::FieldAccess { object: inner_obj, field } = &object.node {
            if let Some((field_place, field_type)) = try_resolve_field_place(ctx, builder, inner_obj, &field.node) {
                (Operand::Copy(field_place), Some(field_type))
            } else {
                (lower_expr(ctx, builder, object), None)
            }
        } else {
            (lower_expr(ctx, builder, object), None)
        };

        // T5 sibling: `SHARED[i] OP= x` on a bare-identifier module-level
        // `static`. A bare `lower_expr` yields an `Operand::Constant(GlobalRef)`,
        // NOT a Copy/Move Place, so the `Operand::Copy | Move` guard below drops
        // BOTH the read and the write-back (the compound update is silently lost).
        // Unlike the plain-store arm (which uses `materialize_global_field_base`'s
        // MutPtr write-through), this arm's read (`index_load`) and write
        // (`emit_borrow_mut` + `__set`) both assume a DIRECT value place with no
        // `obj_is_ptr` handling — so mirror the READ path (`lower_index_access`,
        // methods.rs): materialize the GlobalRef into a direct-value local via
        // `Borrow` (a zero-cost shallow header aliasing the global's heap buffer;
        // the global retains ownership). An in-bounds element `__set` writes to the
        // shared buffer in place → visible in the global, with no realloc/header
        // drift. Value-typed statics `Copy`.
        let obj = if let Operand::Constant(Constant::GlobalRef(_)) = obj {
            let base_type = infer_operand_type_full(ctx, &obj, builder);
            let local = builder.add_local(base_type, None);
            let mode = if ctx.type_registry.is_resource_type(base_type) {
                crate::ir::instructions::AssignMode::Borrow
            } else {
                crate::ir::instructions::AssignMode::Copy
            };
            builder.assign_mode(mode, Place::local(local), obj);
            Operand::Copy(Place::local(local))
        } else {
            obj
        };

        let idx_raw = lower_expr(ctx, builder, index);
        let obj_type = resolved_field_type.unwrap_or_else(|| infer_operand_type_full(ctx, &obj, builder));
        let obj_type = ctx.pointee_type(obj_type).unwrap_or(obj_type);
        let type_name = ctx.type_name_for_id(obj_type).unwrap_or("").to_string();
        // Typed dispatch via `collection_kind` (mirrors the lower_index_assign
        // arm above; see its comment for the kind-to-prefix mapping).
        let kind = ctx.type_registry.get_type_def(&type_name)
            .and_then(|td| td.metadata.collection_kind);
        let is_vector = kind == Some(CollectionKind::Array);
        let is_dict = matches!(kind, Some(CollectionKind::OrderedMap) | Some(CollectionKind::Map));

        if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
            // Save index into a local so it can be reused for both read and write.
            // Phase C: for resource-typed indices (e.g. Dict[String, V] keys),
            // Move from the source (key local) — the idx_local is consumed by
            // both the read and the eventual put/set, but cap=0 propagation
            // makes the shallow shape correct for literal sources today.
            let idx_type = infer_operand_type_full(ctx, &idx_raw, builder);
            let idx_local = builder.add_local(idx_type, None);
            let idx_mode = if let Operand::Copy(ref p) | Operand::Move(ref p) = idx_raw {
                let src_ty = builder.local_type(p.local);
                // Cluster 5 probe (2026-05-10): the disjunction
                // `is_resource_type || needs_drop` is NOT redundant. See
                // `lowering/functions.rs:28` for the full reasoning.
                if p.projections.is_empty()
                    && (ctx.type_registry.is_resource_type(src_ty)
                        || ctx.type_registry.needs_drop(src_ty))
                {
                    crate::ir::instructions::AssignMode::Borrow
                } else {
                    crate::ir::instructions::AssignMode::Copy
                }
            } else {
                crate::ir::instructions::AssignMode::Copy
            };
            builder.assign_mode(idx_mode, Place::local(idx_local), idx_raw);

            // Resource (non-string) vector/dict elements are read by BORROW (a
            // Ptr aliasing the element in place) instead of by Clone: the
            // operator-overload branch below borrows the element for its
            // read-only `self` receiver, and the Clone path's shallow
            // copy-into-self-local (`builder.assign(cur_local, cur_val)`) trips
            // the resource-move validator (ICE at ir/lowering/mod.rs:1763,
            // "shallow copy of resource"). The clone is also pure waste since
            // `add`/`sub`/... only READ `self`; the write-back `__set`/`__put`
            // pre-drops the old element → drop-once. Strings keep Clone (the
            // concat path consumes+drops the owned old value); primitives keep
            // Clone (Copy — no ICE).
            let coll_elem_type = if is_vector || is_dict {
                Some(infer_collection_element_type(ctx, obj_type))
            } else {
                None
            };
            let borrow_in_place = coll_elem_type.map_or(false, |et| {
                ctx.type_registry.is_resource_type(et)
                    && et != ctx.type_mapper.owned_string_type
            });

            // R1 — close the borrow-in-place aliasing window. Both the element
            // `self`-Ptr AND the field-collection header copy (`read_place`
            // below) are taken AFTER the RHS is lowered, so a RHS that reallocs
            // the SAME collection (`v[0] += grow(&v)`, `h.v[0] += grow(&h)`,
            // dict insert-realloc) cannot dangle either — a hazard the old Clone
            // path was immune to (it materialized an owned clone before the
            // RHS). D10(b) same-call place-overlap is FILED-but-unimplemented
            // (won't reject it), so close the window here by lowering the RHS
            // into an owned temp FIRST — BEFORE the `read_place` header capture
            // and the borrow read. Only the borrow path reorders; the Clone
            // paths keep read-then-RHS order so the non-tainted fixtures stay
            // byte-identical.
            let rhs_pre = if borrow_in_place {
                Some(lower_expr(ctx, builder, value))
            } else {
                None
            };

            // For field-accessed collections (e.g. self.scores[i]), copy to a temp local
            // so the C backend can determine the collection type from the local's TypeId.
            // index_load doesn't handle Places with Field projections correctly.
            // Phase C: temp is a non-owning view of the field — Borrow. Taken
            // AFTER `rhs_pre` (above) so a realloc inside the RHS cannot stale
            // this copied header (R1).
            let read_place = if resolved_field_type.is_some() {
                let temp = builder.add_local(obj_type, None);
                let temp_mode = if ctx.type_registry.is_resource_type(obj_type) {
                    crate::ir::instructions::AssignMode::Borrow
                } else {
                    crate::ir::instructions::AssignMode::Copy
                };
                builder.assign_mode(temp_mode, Place::local(temp), Operand::Copy(place.clone()));
                Place::local(temp)
            } else {
                place.clone()
            };

            // Step 1: Read current value at index
            let (cur_val, elem_type, cur_is_borrow) = if is_vector || is_dict {
                let elem_type = coll_elem_type.unwrap();
                if borrow_in_place {
                    let ptr_type = ctx.register_ptr_type(elem_type);
                    let dst = builder.index_load_borrow(read_place, FunctionBuilder::copy(idx_local), ptr_type);
                    (FunctionBuilder::copy(dst), elem_type, true)
                } else {
                    let dst = builder.index_load(read_place, FunctionBuilder::copy(idx_local), elem_type);
                    (FunctionBuilder::copy(dst), elem_type, false)
                }
            } else {
                // Custom type: try Type__get / Index_for_Type__get
                let candidates = [
                    format!("{type_name}__get"),
                    format!("Index_for_{type_name}__get"),
                    format!("{type_name}____getitem__"),
                ];
                let mut found = None;
                for get_name in &candidates {
                    if ctx.fn_sigs.contains_key(get_name.as_str()) {
                        let ret_type = ctx.fn_sigs.get(get_name.as_str())
                            .map(|(_, ret)| *ret)
                            .unwrap_or(I64_TYPE);
                        let pt = ctx.register_ptr_type(obj_type);
                        let pl = builder.add_local(pt, None);
                        builder.emit_borrow(pl, place.clone());
                        let dst = builder.call(
                            get_name.clone(),
                            vec![FunctionBuilder::copy(pl), FunctionBuilder::copy(idx_local)],
                            ret_type,
                        );
                        found = Some((FunctionBuilder::copy(dst), ret_type));
                        break;
                    }
                }
                if let Some((cv, et)) = found {
                    // Custom `__get` returns an OWNED value (Clone semantics) —
                    // the legacy borrow-of-copy self path handles it. R2 NOTE:
                    // a custom Index type with a RESOURCE (custom-Drop) element
                    // AND an `add` overload still reaches the shallow-copy path
                    // → the SAME ICE. See TODO (custom-indexable sibling).
                    (cv, et, false)
                } else {
                    // Fallback: string indexing or unknown type
                    let elem_type = if obj_type == ctx.type_mapper.owned_string_type {
                        ctx.type_mapper.owned_string_type
                    } else {
                        I64_TYPE
                    };
                    let dst = builder.index_load(read_place, FunctionBuilder::copy(idx_local), elem_type);
                    (FunctionBuilder::copy(dst), elem_type, false)
                }
            };

            // Step 2: Lower RHS (already lowered above for the borrow case, to
            // close the R1 realloc window — reuse it there).
            let rhs = rhs_pre.unwrap_or_else(|| lower_expr(ctx, builder, value));

            // Step 3: Compute result
            let is_string = elem_type == ctx.type_mapper.owned_string_type;

            let result = if is_string && matches!(op, ast::BinaryOp::Add) {
                // String concatenation via gorget_str_cat. `cur_val` is an OWNED
                // clone of the old element (`index_load` clones resource-typed
                // elements to owned), and the runtime `gorget_str_cat` reads both
                // args BY VALUE without freeing them, so the old-element clone must
                // be dropped here or it leaks. It is NOT drop-registered
                // (`builder.index_load` is called directly, not via a
                // drop-registering ctx helper), so an unconditional Drop frees it
                // exactly once — no double-free. Pre-existing leak on `V[i] += s` /
                // `M[k] += s` for local AND static resource-element collections
                // (ASan: 4-byte leak, both backends); surfaced by the T5 static
                // index-store fixtures. The concat result is an independent fresh
                // allocation, so it does not alias `cur_val`.
                let owned_type = ctx.type_mapper.owned_string_type;
                let cur_local = match &cur_val {
                    Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => {
                        Some(p.local)
                    }
                    _ => None,
                };
                let tmp = builder.call_extern(
                    "gorget_str_cat",
                    vec![cur_val, rhs],
                    owned_type,
                );
                if let Some(local) = cur_local {
                    builder.drop(Place::local(local));
                }
                FunctionBuilder::copy(tmp)
            } else {
                // Check for operator overload on Named types
                let overload_method = match op {
                    ast::BinaryOp::Add => Some("add"),
                    ast::BinaryOp::Sub => Some("sub"),
                    ast::BinaryOp::Mul => Some("mul"),
                    ast::BinaryOp::Div => Some("div"),
                    ast::BinaryOp::Rem => Some("rem"),
                    ast::BinaryOp::Mod => Some("mod"),
                    _ => None,
                }.and_then(|method| {
                    if let Some(GirType::Named(tn)) = ctx.type_registry.get(elem_type).cloned() {
                        let mangled = format!("{tn}__{method}");
                        let has_method = ctx.fn_sigs.contains_key(&mangled)
                            || ctx.fn_sigs.keys().any(|k| k.ends_with(&format!("_for_{tn}__{method}")));
                        if has_method {
                            let effective_name = if ctx.fn_sigs.contains_key(&mangled) {
                                mangled
                            } else {
                                ctx.fn_sigs.keys()
                                    .find(|k| k.ends_with(&format!("_for_{tn}__{method}")))
                                    .cloned()
                                    .unwrap_or(mangled)
                            };
                            Some(effective_name)
                        } else { None }
                    } else { None }
                });

                if let Some(effective_name) = overload_method {
                    let self_ptr = if cur_is_borrow {
                        // `cur_val` already borrows the element in place (a Ptr
                        // from index_load_borrow) — pass it straight as the
                        // read-only `self` receiver. NO shallow copy of the
                        // resource element (that copy is the ICE).
                        cur_val
                    } else {
                        // Non-resource element (e.g. a Copy struct with an Add
                        // overload): borrow a fresh copy for `self`.
                        let cur_local = builder.add_local(elem_type, None);
                        builder.assign(Place::local(cur_local), cur_val);
                        let ptr_type = ctx.register_ptr_type(elem_type);
                        let ptr_local = builder.add_local(ptr_type, None);
                        builder.emit_borrow(ptr_local, Place::local(cur_local));
                        FunctionBuilder::copy(ptr_local)
                    };
                    let dst = builder.call(effective_name, vec![self_ptr, rhs], elem_type);
                    FunctionBuilder::copy(dst)
                } else {
                    let gir_op = compound_op_to_gir(op);
                    let tmp = builder.bin_op(gir_op, elem_type, cur_val, rhs);
                    FunctionBuilder::copy(tmp)
                }
            };

            // Step 4: Write back via collection set method
            if is_vector {
                let ptr_type = ctx.register_mut_ptr_type(obj_type);
                let ptr_local = builder.add_local(ptr_type, None);
                builder.emit_borrow_mut(ptr_local, place.clone());
                let mangled = format!("{type_name}__set");
                builder.call_void(
                    mangled,
                    vec![FunctionBuilder::copy(ptr_local), FunctionBuilder::copy(idx_local), result],
                );
            } else if is_dict {
                let ptr_type = ctx.register_mut_ptr_type(obj_type);
                let ptr_local = builder.add_local(ptr_type, None);
                builder.emit_borrow_mut(ptr_local, place.clone());
                let mangled = format!("{type_name}__put");
                builder.call_void(
                    mangled,
                    vec![FunctionBuilder::copy(ptr_local), FunctionBuilder::copy(idx_local), result],
                );
            } else {
                // Custom type: try Type__set / IndexMut_for_Type__set
                let set_candidates = [
                    format!("{type_name}__set"),
                    format!("IndexMut_for_{type_name}__set"),
                    format!("{type_name}____setitem__"),
                ];
                let mut dispatched = false;
                for set_name in &set_candidates {
                    if ctx.fn_sigs.contains_key(set_name.as_str()) {
                        let ptr_type = ctx.register_mut_ptr_type(obj_type);
                        let ptr_local = builder.add_local(ptr_type, None);
                        builder.emit_borrow_mut(ptr_local, place.clone());
                        builder.call_void(
                            set_name.clone(),
                            vec![FunctionBuilder::copy(ptr_local), FunctionBuilder::copy(idx_local), result],
                        );
                        dispatched = true;
                        break;
                    }
                }
                // HARD ICE sibling of `lower_index_assign`'s fall-through
                // (Chain C item 6): the write-back of `x[i] += v` must
                // dispatch — this silent fall-through was how
                // `s[0] += "x"` compiled as a no-op. Strings are rejected
                // at check time (`SemanticErrorKind::StringIndexAssign`).
                if !dispatched {
                    panic!(
                        "BUG: compound index-assign on `{type_name}` found \
                         no setter (tried {set_candidates:?}) — typecheck \
                         accepted an index-assign the lowering cannot \
                         dispatch"
                    );
                }
            }
        }
        // Untrack the transient CollectionElement handles minted by the
        // `try_resolve_field_place` `Expr::Index` arm (`m[i].field[key] OP= x`) —
        // mirrors the FieldAccess arm's end-of-stmt untrack (Core #4).
        ctx.untrack_transient_element_refs_in_range(builder, stmt_locals_start, builder.locals.len());
    } else if let Expr::Deref { expr: inner } = &target.node {
        // Compound assign THROUGH a pointer: `*p OP= v` (Box / &-ref / Ptr).
        // Plain `*p = v` already lowers (`lower_assign`'s Expr::Deref arm); the
        // compound path must too — NOT silently drop (Core #10 lower-or-reject;
        // measured pre-fix: `*b += 5` on `Box[int]` left the pointee unchanged).
        // Read the pointee THROUGH the deref place, apply the op, store back —
        // the deref place is built ONCE and reused for the read AND the write
        // (base `p` evaluated once). Mirrors `lower_assign`'s Deref arm for the
        // place + pointee-type resolution.
        let inner_op = lower_expr(ctx, builder, inner);
        if let Operand::Copy(ref inner_place) | Operand::Move(ref inner_place) = inner_op {
            let mut deref_place = inner_place.clone();
            deref_place.projections.push(Projection::Deref);
            let pointee_type = {
                let local_idx = inner_place.local.0 as usize;
                let mut t = if local_idx < builder.locals.len() {
                    builder.local_type(inner_place.local)
                } else { UNIT_TYPE };
                for proj in &inner_place.projections {
                    if let Projection::Deref = proj {
                        t = ctx.deref_inner_type(t).unwrap_or(t);
                    } else if let Projection::Field(idx) = proj {
                        if let Some(tn) = ctx.type_name_for_id(t).map(|s| s.to_string()) {
                            if let Some(td) = ctx.type_registry.get_type_def(&tn) {
                                if let crate::ir::types::TypeDefKind::Struct(ref s) = td.kind {
                                    if (*idx as usize) < s.fields.len() {
                                        t = s.fields[*idx as usize].type_id;
                                    }
                                }
                            }
                        }
                    }
                }
                ctx.deref_inner_type(t).unwrap_or(t)
            };
            // R-STRING: resource-safe read-modify-write via the shared helper.
            // The deref place is a stable write-through place (Box / &-ref / Ptr
            // pointee), so the same String/overload/value read-shape split +
            // cleanup-store drop-old applies as for a struct field. No shallow
            // resource copy of the pointee.
            emit_compound_place_rmw(ctx, builder, &deref_place, pointee_type, op, value);
        }
    } else if let Expr::TupleFieldAccess { object, index } = &target.node {
        // Compound assign on a TUPLE field: `t.INDEX OP= v`. A tuple field is a
        // valid mutable place (a struct field at a numeric index) — pre-fix this
        // fell to the catch-all and ICE'd on typecheck-ACCEPTED code (`x.0 += 5`
        // passes `gg check`). Resolve the write-through place ONCE and read-
        // modify-write through it (Core #10 lower, not reject; Core #4, one class
        // with plain `t.0 = v` and struct `s.f OP= v`). The place is reused for
        // the read AND the store (base evaluated once).
        materialize_assign_target_root(ctx, builder, object);
        let stmt_locals_start = builder.locals.len();
        if let Some((field_place, field_type)) =
            try_resolve_tuple_field_place(ctx, builder, object, *index)
        {
            // R-STRING: resource-safe read-modify-write via the shared helper.
            // A tuple field is a stable write-through place, so the same
            // String/overload/value read-shape split + cleanup-store drop-old
            // applies as for a struct field. No shallow resource copy.
            emit_compound_place_rmw(ctx, builder, &field_place, field_type, op, value);
        }
        ctx.untrack_transient_element_refs_in_range(builder, stmt_locals_start, builder.locals.len());
    } else {
        // Core #10 (lower-or-reject): every ASSIGNABLE compound-assign target is
        // lowered above (Identifier / FieldAccess / TupleFieldAccess / Index /
        // Deref). A non-lvalue target (`5 += 1`, `foo() += 1`) is REJECTED at
        // check time by `check_assign_target_lvalue` (E_InvalidAssignTarget), so
        // accepted code never reaches here — genuinely unreachable, backed by the
        // check-time guard (NOT the false "typecheck rejects these" claim the
        // original panic carried; the checker had no lvalue gate then, which is
        // why `x.0 += 5` ICE'd). Defense-in-depth: a shape that slips the guard
        // is a compiler bug, not user error.
        unreachable!(
            "lower_compound_assign reached a non-lvalue target ({:?}) that \
             check_assign_target_lvalue should have rejected (E_InvalidAssignTarget)",
            target.node,
        );
    }
}

/// Map compound assignment operator to GIR binary operator.
fn compound_op_to_gir(op: ast::BinaryOp) -> BinOp {
    match op {
        ast::BinaryOp::Add => BinOp::Add,
        ast::BinaryOp::Sub => BinOp::Sub,
        ast::BinaryOp::Mul => BinOp::Mul,
        ast::BinaryOp::Div => BinOp::Div,
        ast::BinaryOp::Rem => BinOp::Rem,
        ast::BinaryOp::Mod => BinOp::Mod,
        ast::BinaryOp::AddWrap => BinOp::AddWrap,
        ast::BinaryOp::SubWrap => BinOp::SubWrap,
        ast::BinaryOp::MulWrap => BinOp::MulWrap,
        ast::BinaryOp::BitAnd => BinOp::BitAnd,
        ast::BinaryOp::BitOr => BinOp::BitOr,
        ast::BinaryOp::BitXor => BinOp::BitXor,
        ast::BinaryOp::Shl => BinOp::Shl,
        ast::BinaryOp::Shr => BinOp::Shr,
        _ => BinOp::Add,
    }
}

/// The SHARED root-materialize prologue for EVERY assign-target object — field
/// store (`s.field = x`), index store (`d[k] = x` / `v[i] = x`), and compound
/// (`xs[i] OP= x` / `obj.field OP= x`). Planner campaign round 3, first at-site
/// client: this is the ONE place the assign-target-root class materializes, and
/// it routes through the per-function `MaterializePlan` (`plan_materialize_at_site`
/// → the single reason-stamping funnel) instead of an open-coded
/// `cow_before_mutation` call. `lower_field_assign` and `lower_index_assign`
/// previously OPEN-CODED this identical prologue (Core #4 sibling-site drift —
/// three copies of the same identifier-vs-projected dispatch); they now both
/// call here, collapsing six `cow_before_mutation` sites to zero direct calls
/// (the `ratchet_b_materialize_site_count` convergence meter drops 20 → 14).
/// Mirrors the self-host lane's already-consolidated `cow_materialize_projected_root`.
///
/// Materialize the immutable-in-context ROOT (bare-param / alias / element) so
/// the write lands on an owned copy; a no-op on `&`/owned roots keeps `&`-chain
/// write-through. For a projected field-path object the field-path collection-ref
/// sever (`cow_before_field_mutation` — a distinct primitive, NOT a materialize
/// site) still fires on top, as before.
fn materialize_assign_target_root(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
) {
    if let Expr::Identifier(obj_name) = &object.node {
        // Direct `s.field = x` / `d[k] = x`: the root IS the object.
        if let Some((local_id, _)) = ctx.lookup_local(obj_name) {
            ctx.plan_materialize_at_site(builder, local_id, object.span);
        }
    } else {
        // Projected object (`v[i].field = x`, `s.inner[k] = x`, `m[i][j] = x`):
        // materialize the ROOT struct/collection FIRST, then sever field-path
        // refs (the two touch disjoint state — the root local vs the FieldPath
        // refs — so no double-clone; the sever stays needed on the `&`-root
        // no-op path).
        if let Some(root_local) = resolve_projection_root_local(ctx, &object.node) {
            ctx.plan_materialize_at_site(builder, root_local, object.span);
        }
        if let Some(field_path) = extract_field_path_string(&object.node) {
            ctx.cow_before_field_mutation(builder, &field_path, object.span);
        }
    }
}
