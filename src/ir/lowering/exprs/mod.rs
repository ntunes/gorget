mod calls;
mod collections;
mod methods;
mod operators;
mod shared;
pub(crate) mod spawn;
pub(in crate::ir::lowering) mod type_reg;

pub(in crate::ir::lowering) use calls::*;
use collections::*;
pub(in crate::ir::lowering) use methods::*;
use operators::*;
pub(in crate::ir::lowering) use shared::*;
pub(in crate::ir::lowering) use spawn::*;
pub(in crate::ir::lowering) use type_reg::*;

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::lexer::token::StringSegment;
use crate::parser::ast::{self, Expr, Ownership};
use crate::span::Spanned;

use super::context::LoweringContext;

/// Known blocking function names that should trigger `with shared_var:` auto-refresh.
/// These are yield points where another task could modify a shared variable.
const BLOCKING_CALL_NAMES: &[&str] = &[
    "sleep", "read_file", "write_file", "append_file",
    "readdir", "http_get", "http_post", "http_put", "http_delete",
];

/// Check if an expression is a call to a known blocking function.
fn is_blocking_call_name(expr: &Expr) -> bool {
    if let Expr::Identifier(name) = expr {
        BLOCKING_CALL_NAMES.contains(&name.as_str())
    } else {
        false
    }
}

/// Lower an expression to GIR instructions, returning the result `Operand`.
///
/// Applies a centralized **producer-side** `Result → T` auto-propagation hook
/// at the tail when the expression is a `Call` or `MethodCall`: if the call
/// returns `Result[T, E]` AND the enclosing function can propagate (`throws E`
/// or `Result[_,_]` return) AND the surrounding destination doesn't want a
/// Result, the Result is unwrapped (forwarding the `Error` branch to the
/// function's throws/Result return slot).
///
/// The hook centralizes what used to be N consumer-side `maybe_auto_propagate`
/// calls (Snag #43 call args, Snag #46 constructor args, Snag #48 match
/// scrutinees and similarly for-iter / if-cond / index — see TODO entry
/// "Plug the `Result→T` auto-propagation consumer-site whack-a-mole class").
/// The decision is purely typed: it reads the operand's IR type and the
/// surrounding `expected_type`, no name-matching anywhere.
///
/// **Why only Call / MethodCall.** Auto-propagation is a transformation at the
/// *producer* of a `Result`: the throws-sugar is what synthesizes the unwrap.
/// Sub-expressions whose value happens to be `Result`-typed but didn't *just*
/// come out of a call (identifier references to a Result-typed local, field
/// access on a Result-typed struct, `risky().method()` *receiver*) are NOT
/// candidates — `.unwrap()`-style methods on `Result` rely on receiving the
/// raw value. Firing the hook on every `lower_expr` would auto-prop those
/// receivers and break canonical Result discrimination. Restricting to Call
/// / MethodCall mirrors the existing manual-call-site pattern.
///
/// Sites that need the raw `Result` operand even at a Call/MethodCall position
/// (match scrutinee with Ok/Error patterns, rethrow inner, catch inner) set
/// `func_state.suppress_auto_prop` to true before calling; it is a one-shot
/// consumed at lower_expr entry so nested sub-expressions still auto-prop
/// normally.
pub fn lower_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    expr: &Spanned<Expr>,
) -> Operand {
    // D29: `expr!` (Propagate) is TRANSPARENT — unwrap it BEFORE the suppress
    // one-shot is consumed, so the inner call inherits the match-scrutinee /
    // catch / rethrow suppress signal (otherwise the mark eats it, the call
    // auto-props to `T`, and a match on the raw Result reads garbage → SIGSEGV;
    // scout Finding 5). Mirror of the typecheck-side transparency fix.
    if let Expr::Propagate { expr: inner } = &expr.node {
        return lower_expr(ctx, builder, inner);
    }
    let suppress = std::mem::replace(&mut ctx.func_state.suppress_auto_prop, false);
    let is_producer = matches!(&expr.node, Expr::Call { .. } | Expr::MethodCall { .. });
    let op = lower_expr_inner(ctx, builder, expr, None);
    if suppress || !is_producer {
        op
    } else {
        // Snag #11: key the auto-prop on the producing call expr's span — the
        // same key the typechecker's Route-A producer-peel records under, so a
        // recorded `From` conversion is found and emitted on the error value.
        maybe_auto_propagate(ctx, builder, op, expr.span)
    }
}

/// Lower an expression with optional type registry access for mutable operations.
fn lower_expr_inner(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    expr: &Spanned<Expr>,
    _registry: Option<&mut TypeRegistry>,
) -> Operand {
    builder.set_span(expr.span);
    match &expr.node {
        Expr::IntLiteral(n) => Operand::Constant(Constant::I64(*n)),

        Expr::FloatLiteral(n) => Operand::Constant(Constant::F64(*n)),

        Expr::BoolLiteral(b) => Operand::Constant(Constant::Bool(*b)),

        Expr::StringLiteral(lit, interp_exprs) => {
            if !lit.has_interpolation() {
                let text = lit.as_plain_text();
                Operand::Constant(Constant::Str(text))
            } else {
                lower_string_interpolation(ctx, builder, lit, interp_exprs)
            }
        }

        Expr::Identifier(name) => {
            if let Some((local_id, _)) = ctx.lookup_local(name) {
                // If this is a shared variable:
                // - In spawn arg context (shared_pass_raw), return the raw Mutex local
                // - Otherwise, auto-emit lock+get for transparent access
                if let Some(info) = ctx.shared.locals.get(&local_id) {
                    let (hidden_local, inner_type, kind) = (info.hidden_local, info.inner_type, info.kind);
                    use super::context::SharedLocalKind;
                    if ctx.shared.pass_raw {
                        return Operand::Copy(Place::local(hidden_local));
                    }
                    return match kind {
                        SharedLocalKind::SharedArc => emit_shared_get(ctx, builder, hidden_local, inner_type),
                        SharedLocalKind::Atomic => {
                            let atomic_name = atomic_type_name_for(inner_type);
                            emit_atomic_load(ctx, builder, hidden_local, inner_type, &atomic_name)
                        }
                        SharedLocalKind::Mutex => {
                            let inner_c = ctx.c_type_name_for_id(inner_type);
                            let mutex_type = ctx.type_mapper.lookup_named(&format!("Mutex__{inner_c}")).unwrap_or(inner_type);
                            emit_shared_mutex_lock_get(ctx, builder, hidden_local, mutex_type, inner_type)
                        }
                        SharedLocalKind::RwLock => emit_rwlock_read_get(ctx, builder, hidden_local, inner_type),
                    };
                }
                // If this is a &/! param (MutPtr), deref to get the value.
                // ref_locals (bare-borrow Ptr params) are NOT auto-deref'd —
                // they stay as Ptr throughout the callee body.
                let value_type = if ctx.is_param_borrow_unique(builder, local_id) {
                    ctx.pointee_type(builder.local_type(local_id))
                } else {
                    None
                };
                if let Some(value_type) = value_type {
                    let deref_place = Place {
                        local: local_id,
                        projections: vec![Projection::Deref],
                    };
                    let tmp = builder.add_local(value_type, None);
                    // ! params (owned): use Move to transfer ownership (memcpy, no clone).
                    // & params (mutable borrow): Copy through the pointer.
                    let is_move_param = ctx.is_owned_local(builder, local_id);
                    if is_move_param {
                        builder.assign_mode(
                            crate::ir::instructions::AssignMode::Move,
                            Place::local(tmp),
                            Operand::Move(deref_place),
                        );
                        ctx.set_owned(builder, tmp);
                    } else {
                        builder.assign(Place::local(tmp), Operand::Copy(deref_place));
                        // Tag what the backend ACTUALLY produced (Core #1 — the
                        // ownership tag was lying about the emitted code).
                        //
                        // A Copy-mode store of a String-family value read through a
                        // pointer is NOT a shallow struct copy: both backends lower it
                        // to `gorget_string_copy_cow` (`backend/c_lir/mod.rs`,
                        // `backend/llvm/mod.rs`), which yields an INDEPENDENTLY
                        // DROPPABLE value — `cap>0` deep-copies into a fresh
                        // allocation, `cap==0` struct-copies a view whose
                        // `gorget_string_free` is a no-op
                        // (`backend/c/runtime/runtime_string.c`). Every OTHER resource
                        // type gets a plain shallow `memcpy` and stays a borrow.
                        //
                        // Leaving the String temp `Untracked` made downstream
                        // boundaries clone it a SECOND time and orphan this buffer:
                        // `String f(String &s): return s` leaked the copy_cow result
                        // (LSan), and every String `&`-param crossing an ownership
                        // boundary paid two clones where a hand-writer pays one.
                        if value_type == ctx.type_mapper.owned_string_type {
                            ctx.drops.register_local(tmp, value_type, &ctx.type_registry);
                            ctx.set_owned_fresh(builder, tmp);
                        } else if ctx.type_registry.is_resource_type(value_type)
                            && !builder.locals[local_id.0 as usize].is_owning_param
                        {
                            // Every OTHER resource type IS a shallow `memcpy` here,
                            // so the temp's heap data is the CALLER's. Carry the
                            // borrow provenance (`Param(p)` with `p != tmp`, so
                            // `is_bare_param` / `is_param_borrow_unique` — both of
                            // which discriminate on `p == local` — stay false and
                            // this temp is never re-auto-deref'd). Minting it
                            // `Untracked` was the lie behind the whole
                            // return-borrow double-free family: an unnamed
                            // untracked resource looks dead-and-owned to every
                            // downstream decision, so a bind/re-assign happily
                            // Move'd it into an Owned destination that then
                            // double-freed the caller's buffer.
                            builder.locals[tmp.0 as usize].ownership =
                                crate::ir::LocalOwnership::Borrowed {
                                    origin: crate::ir::BorrowOrigin::Param(local_id),
                                    mutability: crate::ir::Mutability::Unique,
                                };
                        }
                    }
                    // T-A (snag #1 ctor extension): if this is an owning `!` resource
                    // param, record the deref-temp → param provenance (typed field on
                    // Local, one source of truth — no sidecar map) so a downstream
                    // ctor/boundary consuming position can MOVE the value (zeroing the
                    // param slot) instead of defensively cloning the untracked temp.
                    if (local_id.0 as usize) < builder.locals.len()
                        && builder.locals[local_id.0 as usize].is_owning_param
                    {
                        builder.locals[tmp.0 as usize].deref_of_owning_param = Some(local_id);
                    }
                    Operand::Copy(Place::local(tmp))
                } else {
                    Operand::Copy(Place::local(local_id))
                }
            } else if let Some(constant) = ctx.module_constants.get(name) {
                Operand::Constant(constant.clone())
            } else if ctx.global_names.contains(name.as_str()) {
                // Module-level static variable — reference by name in C
                Operand::Constant(Constant::GlobalRef(name.clone()))
            } else if ctx.fn_sigs.contains_key(name.as_str()) {
                // Named function reference (for passing as Callable argument)
                Operand::Constant(Constant::FuncRef(name.clone()))
            } else if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant_typed(name, ctx.func_state.expected_type) {
                // Bare nullary enum variant (e.g., `Red` after glob import).
                // SSOT: type-aware to disambiguate same-named variants across enums.
                let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
                let dst = builder.enum_init(&enum_name, &variant_name, type_id, vec![]);
                FunctionBuilder::copy(dst)
            } else {
                // Could be a function name or unknown — produce a constant placeholder
                Operand::Constant(Constant::I64(0))
            }
        }

        Expr::BinaryOp { left, op, right } => {
            lower_binary_op(ctx, builder, left, *op, right)
        }

        Expr::UnaryOp { op, operand } => {
            lower_unary_op(ctx, builder, *op, operand)
        }

        Expr::Call { callee, args, generic_args } => {
            // None() call. The materialise-into-Option-struct logic is shared
            // with the bare `Expr::NoneLiteral` arm below — see
            // `materialise_none_for_expected_type` for the rationale.
            if matches!(callee.node, Expr::NoneLiteral) {
                return materialise_none_for_expected_type(ctx, builder);
            }
            // Check if this is a blocking call that should trigger with-shared refresh
            let is_blocking = is_blocking_call_name(&callee.node);
            let result = lower_call(ctx, builder, callee, args, generic_args.as_deref());
            if is_blocking {
                shared::emit_with_shared_refresh(ctx, builder);
            }
            result
        }

        // -- P2.1: Struct operations --
        Expr::StructLiteral { name, args, generic_args } => {
            lower_struct_literal(ctx, builder, &name.node, args, generic_args.as_deref())
        }

        Expr::FieldAccess { object, field } => {
            lower_field_access(ctx, builder, object, &field.node)
        }

        // -- P2.2: Method calls --
        Expr::MethodCall { receiver, method, generic_args, args } => {
            lower_method_call(ctx, builder, receiver, &method.node, method.span.start, generic_args.as_deref(), args)
        }

        // -- Index access --
        Expr::Index { object, index } => {
            lower_index_access(ctx, builder, object, index)
        }

        // D29: `expr!` propagation lowers transparently to inner. (Auto-prop is
        // inserted by the existing throws lowering; the mark carries no extra
        // lowering. The `lower_expr` entry normally intercepts Propagate before
        // reaching here — this arm covers any direct `lower_expr_inner` call.)
        Expr::Propagate { expr: inner } => lower_expr(ctx, builder, inner),

        // -- P2.6: Move/Borrow --
        Expr::Move { expr: inner } => {
            // CoW: if moving a borrowed local, materialize first.
            //
            // For `!`-sigil resource parameters (`is_owning_param`),
            // the explicit `!x` at the use site is a transfer — the
            // bytes at `*x` move to whatever consumes the resulting
            // operand (struct field, push, send, etc.). Capture the
            // source local up front so we can MoveZero it after the
            // transfer; without this, the function-exit owning-param
            // drop's flag stays `true` and the data is freed twice
            // (once at exit, once by the recipient's drop).
            let owning_param_source: Option<LocalId> = if let Expr::Identifier(name) = &inner.node {
                if let Some((local_id, _)) = ctx.lookup_local(name) {
                    // CoW: a move transfers ownership — sever aliases FIRST,
                    // for ANY local source (mirrors the call-arg move sibling
                    // in calls.rs "sever aliases first"; was bare-params-only,
                    // textbook sibling-site drift). Without this, a live
                    // element borrow (e.g. `String s = v.get(0).unwrap()`
                    // then `Vector[String] w = !v`) stays a deferred
                    // CollectionRef into the moved buffer: mutating `w`
                    // read-through-corrupts `s`, and a realloc (push past
                    // cap) leaves `s` dangling — SIGSEGV. cow_before_mutation
                    // dispatches the lazy in-place materialize and the legacy
                    // ref/alias/view severs; the stale-refs unset_ownership
                    // loop below stays (harmless post-materialize).
                    ctx.cow_before_mutation(builder, local_id, inner.span);
                    let idx = local_id.0 as usize;
                    if idx < builder.locals.len() && builder.locals[idx].is_owning_param {
                        Some(local_id)
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            };
            let val = lower_expr(ctx, builder, inner);
            // Copy value to a temp BEFORE zeroing the source, so we don't read
            // zeroed data. Phase C: emit Move mode for resource types — the
            // source is about to be zeroed via move_zero_and_mark, so the
            // semantic IS a transfer of ownership. Copy mode would mark the
            // GIR as a shallow alias that the validator (correctly) rejects.
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                let place_clone = place.clone();
                let local_type = if (place_clone.local.0 as usize) < builder.locals.len() {
                    builder.local_type(place_clone.local)
                } else {
                    I64_TYPE
                };
                let tmp = builder.add_local(local_type, None);
                let mode = if ctx.type_registry.is_resource_type(local_type) {
                    crate::ir::instructions::AssignMode::Move
                } else {
                    crate::ir::instructions::AssignMode::Copy
                };
                builder.assign_mode(mode, Place::local(tmp), val);
                ctx.move_zero_and_mark(builder, place_clone.local);
                // Tier 2a Phase 2A: Move-mode receives ownership at the
                // IR semantic level. Tag `tmp` as Owned so downstream
                // consumers (EnumInit / StructInit) see a sound
                // `(Owned, dead, _)` tuple at the validator. Skip for
                // non-resource (Copy-mode) — primitives have no
                // ownership.
                if mode == crate::ir::instructions::AssignMode::Move {
                    ctx.set_owned(builder, tmp);
                }
                // Clean up CoW tracking — moved local's data is zeroed, so
                // any collection refs keyed on it are stale.
                // Remove any collection refs keyed on this local (now zeroed/stale)
                let stale_refs = ctx.cow_collection_refs_for(builder, place_clone.local);
                for r in stale_refs {
                    ctx.unset_ownership(builder, r);
                }
                // Owning-`!`-param transferred via explicit `!x`: invalidate the
                // original param slot too. The Identifier-Move-Deref path above
                // produced `tmp_a` (a memcpy of `*x`) and we just zeroed it,
                // but the param slot still holds the pointer to the source
                // bytes — the function-exit `DropIfAlive { *x }` would free
                // that data, which is now also owned by the recipient (the
                // struct field, collection element, etc.) the caller of
                // this `Expr::Move` is about to construct. The MoveZero on
                // the param slot flips the LIR drop flag to false.
                if let Some(src) = owning_param_source {
                    ctx.move_zero_and_mark(builder, src);
                }
                FunctionBuilder::copy(tmp)
            } else {
                val
            }
        }

        Expr::MutableBorrow { expr: inner } => {
            // Special case: &name where name is already a pointer param.
            // Skip the auto-deref that Identifier normally does — just forward the pointer.
            if let Expr::Identifier(name) = &inner.node {
                if let Some((local_id, _)) = ctx.lookup_local(name) {
                    // CoW `&`-of-a-bare-value FORMATION (G2, site 2 — the
                    // standalone sibling of `lower_call_arg`'s `&name` arg):
                    // `auto r = &x` must materialize a bare param / bare alias so
                    // a later `r.push(..)` lands on the private copy, not the
                    // shared source. cow_before_mutation rebinds the name on
                    // materialize, so RE-RESOLVE before the fast-path checks;
                    // forwarding the stale (pre-materialize) Ptr would write
                    // through to the source and orphan the copy. No-op (no
                    // rebind) on a real `&`-param / owned root → byte-identical.
                    ctx.cow_before_mutation(builder, local_id, inner.span);
                    let local_id = ctx.lookup_local(name).map(|(l, _)| l).unwrap_or(local_id);
                    if ctx.is_ref_local(builder, local_id)
                        || ctx.is_param_borrow_unique(builder, local_id)
                    {
                        return FunctionBuilder::copy(local_id);
                    }
                }
            }
            // (Snag #26's `&*box` / `&*ptr` Deref-lvalue block used to sit HERE;
            // it is now the `Expr::Deref` arm of the shared `try_resolve_place`
            // producer invoked below — the STANDALONE face of the two
            // `&`-formation faces, resolving through the same producer as the
            // CALL-ARG face in `calls.rs` so a projection form cannot be served
            // at one face and dropped at the other. Order is preserved: the G2
            // block immediately below explicitly excludes `Expr::Deref` from
            // root-materialize, so `&*b` still reaches the producer with
            // `g2_projected_untrack_start == None`, exactly as before.)
            // CoW `&`-of-a-PROJECTED-bare-value FORMATION (G2, site 3, standalone
            // form): `auto r = &b.data`, `auto r = &b.data[i]` where the
            // projection ROOT is a bare param / bare alias. Materialize the root
            // BEFORE the single lowering of `inner` (below) so the projection
            // re-reads out of the private owned copy and a later write through
            // `r` lands there, not on the shared source. Same UAF-fold class as
            // the call-arg form and the G1 method-receiver materialize — the
            // transient element/field handles the projection mints MUST be
            // untracked. Identifier / Deref shapes are handled above; only
            // genuine projections reach here.
            //
            // 2T (wave-2 executor T1.2c): the `Expr::SelfExpr` arm added to
            // `resolve_projection_root_local` makes `&self.field` resolvable
            // here too, but a tainted-self DOUBLE-DROP cannot arise at THIS
            // (standalone-formation) site: the only surface form that lowers a
            // standalone `Expr::MutableBorrow` of a projection is a NAMED
            // `&`-binding (`auto r = &self.field` / `= &p.field`), and the
            // safety pass rejects every named `&`-binding with `E_LocalBorrowBind`
            // BEFORE lowering (measured, all spellings). So this call is
            // source-unreachable for the tainted case — the CALL-ARG formation
            // (`calls.rs`), gated by `reject_tainted_formation_arg`, is the only
            // reachable formation-materialize. No fixture pins an unreachable arm.
            let mut g2_projected_untrack_start: Option<usize> = None;
            if !matches!(&inner.node, Expr::Identifier(_) | Expr::Deref { .. }) {
                if let Some(root_local) = resolve_projection_root_local(ctx, &inner.node) {
                    let start = builder.locals.len();
                    let root_name = builder.local_name(root_local).map(|s| s.to_string());
                    let before = root_name.as_deref().and_then(|n| ctx.lookup_local(n).map(|(l, _)| l));
                    ctx.cow_before_mutation(builder, root_local, inner.span);
                    let after = root_name.as_deref().and_then(|n| ctx.lookup_local(n).map(|(l, _)| l));
                    // Untrack ONLY when the root actually materialized (no-op on
                    // a unique / owned root → byte-identical for non-materializing
                    // projected borrows).
                    if before != after {
                        g2_projected_untrack_start = Some(start);
                    }
                }
            }
            // FAMILY-1 CHOKEPOINT (STANDALONE face). Same producer and the same
            // ordering contract as the CALL-ARG face in `calls.rs`: AFTER the G2
            // root-materialize (so a rebound root is addressed, not the shared
            // source), BEFORE `lower_expr` and returning early (so `inner` is
            // lowered exactly once), with the G2 untrack still closing over the
            // handles the projection minted.
            //
            // ⚠ THIS FACE IS GENUINELY GUARD-FREE, and that is not an oversight:
            // unlike `lower_call_arg` there is NO ownership field to gate on here
            // — the NODE ITSELF is the sigil. The two faces DIFFER; do not
            // "unify" them by deleting the call-arg gate.
            //
            // The live, RATIFIED shape reaching this face with a projection is
            // the list-comprehension iterable (`[e for x in &s.items]`, D32
            // rider): `lower_list_comprehension`'s non-range path lowers the
            // iterable through `lower_expr`, landing here. Its emission changes
            // from a forwarded field-load `Ptr(T)` to an `emit_borrow_mut`
            // `MutPtr(T)`; pinned by `cow_comprehension_amp_projection_source.gg`.
            if let Some((place, place_type)) = try_resolve_place(ctx, builder, inner) {
                if let Some(s) = g2_projected_untrack_start {
                    ctx.untrack_transient_element_refs_in_range(builder, s, builder.locals.len());
                }
                let ptr_type = ctx.register_mut_ptr_type(place_type);
                let dst = builder.add_local(ptr_type, None);
                builder.emit_borrow_mut(dst, place);
                return FunctionBuilder::copy(dst);
            }
            let val = lower_expr(ctx, builder, inner);
            // G2 site-3 UAF-fold close: reset the transient element/field handles
            // the projection minted INTO the private copy so a later
            // same-collection push can't Case-3-clone a dangling temp. Gated on
            // materialize-happened; named borrows spared by the helper's guard.
            if let Some(s) = g2_projected_untrack_start {
                ctx.untrack_transient_element_refs_in_range(builder, s, builder.locals.len());
            }
            // GlobalRef → GlobalRefPtr: emit &global_name directly.
            if let Operand::Constant(Constant::GlobalRef(name)) = &val {
                return Operand::Constant(Constant::GlobalRefPtr(name.clone()));
            }
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                let local_type = if (place.local.0 as usize) < builder.locals.len() {
                    builder.local_type(place.local)
                } else {
                    UNIT_TYPE
                };
                // If the value is already a pointer (e.g., &self where self is Node*),
                // just forward it — don't create a double pointer.
                let is_already_ptr = matches!(
                    ctx.type_registry.get(local_type),
                    Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))
                );
                if is_already_ptr {
                    return FunctionBuilder::copy(place.local);
                }
                let ptr_type = ctx.register_mut_ptr_type(local_type);
                let dst = builder.add_local(ptr_type, None);
                builder.emit_borrow_mut(dst, place.clone());
                return FunctionBuilder::copy(dst);
            }
            val
        }

        // -- P2.4: Closures --
        Expr::Closure { params, body, is_move, .. } => {
            let mut cl = std::mem::take(&mut ctx.closures);
            let result = cl.lower_closure(ctx, builder, params, body, *is_move, expr.span);
            ctx.closures = cl;
            result
        }

        // -- If expression (ternary) --
        Expr::If { condition, then_branch, elif_branches, else_branch, .. } => {
            lower_if_expr(ctx, builder, condition, then_branch, elif_branches, else_branch.as_deref())
        }

        // -- P3.2: Match expression --
        Expr::Match { scrutinee, arms, else_arm } => {
            lower_match_expr(ctx, builder, scrutinee, arms, else_arm.as_deref())
        }

        // -- P3.4: Miscellaneous expressions --

        Expr::NoneLiteral => {
            // Materialise into a tagged Option struct when expected_type is an
            // Option[T] — call args, return values, struct-field inits set
            // expected_type and never flow through the Assign handler that
            // would otherwise rewrite Null → tagged struct downstream. Without
            // this, `f(None)` lowered to `f(*(Option[T]*)NULL)` → SEGV at the
            // call site (Snag #29b runtime follow-up). VarDecl-style sites
            // continue to work because the Assign handler still catches the
            // bare-Null fallback when expected_type isn't set / isn't Option.
            materialise_none_for_expected_type(ctx, builder)
        }

        Expr::SelfExpr => {
            if let Some((local_id, _)) = ctx.lookup_local("self") {
                Operand::Copy(Place::local(local_id))
            } else {
                Operand::Constant(Constant::Unit)
            }
        }

        Expr::It => {
            if let Some((local_id, _)) = ctx.lookup_local("it") {
                Operand::Copy(Place::local(local_id))
            } else {
                Operand::Constant(Constant::Unit)
            }
        }

        Expr::Block(block) => {
            lower_block_expr(ctx, builder, block)
        }

        Expr::Do { body } => {
            lower_block_expr(ctx, builder, body)
        }

        Expr::As { expr: inner, type_ } => {
            let val = lower_expr(ctx, builder, inner);
            let target_type = ctx.type_mapper.map_ast_type(&type_.node);
            // Auto-deref Ref[T] before the cast: `r as int` where r is Ref[uint8]
            // should cast the BYTE VALUE to int, not the pointer bits.
            // String, resource types, and other-pointer-targets keep the Ptr —
            // their cast handlers know what to do with it.
            let val = if let Operand::Copy(ref p) | Operand::Move(ref p) = val {
                if p.projections.is_empty() {
                    let src_type = builder.local_type(p.local);
                    if let Some(inner_ty) = ctx.pointee_type(src_type) {
                        let target_is_ptr = matches!(
                            ctx.type_registry.get(target_type),
                            Some(GirType::Ptr(_) | GirType::MutPtr(_))
                        );
                        if !target_is_ptr
                            && !ctx.type_registry.is_resource_type(inner_ty)
                            && !ctx.type_mapper.is_string_type(inner_ty)
                        {
                            let tmp = builder.add_local(inner_ty, None);
                            builder.assign(
                                Place::local(tmp),
                                Operand::Copy(Place {
                                    local: p.local,
                                    projections: vec![Projection::Deref],
                                }),
                            );
                            FunctionBuilder::copy(tmp)
                        } else { val }
                    } else { val }
                } else { val }
            } else { val };
            let dst = builder.cast(target_type, val);
            FunctionBuilder::copy(dst)
        }

        Expr::TupleLiteral(elems) => {
            // Per-element `expected_type`: set it to the destination tuple's
            // element type when the enclosing context declares a tuple (incl.
            // a `Result[Tuple..]` throws/Result return slot, which we peel),
            // else CLEAR it. This is load-bearing for `Result→T` auto-prop:
            // a `return (throws_call(), 5)` from a `throws` function sets
            // `expected_type` to the function's `Result[Tuple.., E]` return
            // slot (via `lower_return`). Leaving that `Result` in place while
            // lowering the throwing-call element suppresses
            // `maybe_auto_propagate`'s peel (it skips when the destination
            // expects a `Result`), leaving the raw `Result` memcpy'd into the
            // tuple's element slot — a silent miscompile (the element reads as
            // its zero-init default). Mirrors the per-element override in
            // `lower_array_literal` and the `expected_type` clear in
            // `lower_binary_op` (operators.rs).
            let saved_expected = ctx.func_state.expected_type;
            // Peel a Result wrapper, then keep only a tuple destination.
            let dest_tuple = saved_expected.and_then(|t| {
                let inner = if ctx.type_registry.enum_category(t)
                    == Some(EnumCategory::Result)
                {
                    extract_result_ok_type(ctx, t)
                } else { t };
                match ctx.type_name_for_id(inner) {
                    Some(name) if name.starts_with("Tuple__") => Some(inner),
                    _ => None,
                }
            });
            let mut operands: Vec<Operand> = elems.iter()
                .enumerate()
                .map(|(i, e)| {
                    ctx.func_state.expected_type = dest_tuple
                        .map(|tup| resolve_tuple_field_type(ctx, tup, i));
                    let op = lower_expr(ctx, builder, e);
                    ctx.func_state.expected_type = saved_expected;
                    op
                })
                .collect();
            // Ownership boundary: tuple fields need independently owned values.
            // First pass: `ensure_owned_at_boundary` clones Ptr(T) borrows and
            // ref-state locals (SharedHeap, Borrowed string views, bare params).
            // Also handles Ptr(Str) deref (replaces the old Ptr(Str)-only loop).
            for (i, op) in operands.iter_mut().enumerate() {
                let span = elems.get(i).map(|e| e.span)
                    .unwrap_or(crate::span::Span { start: 0, end: 0 });
                let new_op = ctx.ensure_owned_at_boundary(
                    builder,
                    std::mem::replace(op, Operand::Constant(Constant::Unit)),
                    span,
                    crate::ir::ImplicitCloneReason::StructFieldFromBorrow,
                );
                *op = new_op;
            }
            // Second pass: clone_multi_use_resource_args handles by-value
            // multi-use, loop-carried, and untracked resource locals.
            // Mirrors the struct literal init path.
            clone_multi_use_resource_args(ctx, builder, &mut operands, elems);
            // Track which locals are used as tuple elements AFTER ownership
            // processing (for return MoveZero). Must be post-processing so
            // that cloned replacements are tracked, not the original sources
            // (which must not be zeroed since they still own their data).
            let elem_locals: Vec<LocalId> = operands.iter()
                .filter_map(|op| match op {
                    Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => Some(p.local),
                    _ => None,
                })
                .collect();
            // Re-infer types after boundary processing (Ptr→Str change etc.)
            let elem_types: Vec<TypeId> = operands.iter()
                .map(|op| infer_operand_type_full(ctx, op, builder))
                .collect();
            let type_id = register_tuple_type(ctx, &elem_types);
            let dst = builder.tuple_init(operands, type_id);
            // Register ownership at the value's birth (CLAUDE.md rule 3,
            // struct-literal precedent in `lower_struct_literal`): the
            // freshly-materialized tuple owns its elements (the boundary
            // passes above cloned/owned each one). Without this tag the dst
            // is Untracked and a destructure consume picks Copy — a shallow
            // copy of a resource tuple that the GIR validator rejects.
            ctx.set_owned(builder, dst);
            // Phase D §6.3: tag each element local as TupleElement of dst.
            // Perf gate on `needs_drop(elem_ty)`: the return-path reader
            // (`tuple_element_sources`) only MoveZero's droppable element
            // locals, so primitives in `local_ownership` would be dead
            // weight. Several helpers (cow_aliases_of, views_of_source,
            // tuple_element_sources) walk that map; keeping it lean
            // matters for self-host where functions can have thousands
            // of locals.
            for (index, &elem_local) in elem_locals.iter().enumerate() {
                let elem_ty = builder.local_type(elem_local);
                if ctx.type_registry.needs_drop(elem_ty) {
                    ctx.set_tuple_element_borrow(builder, elem_local, dst, index as u32);
                }
            }
            FunctionBuilder::copy(dst)
        }

        Expr::ArrayLiteral(elems) => {
            lower_array_literal(ctx, builder, elems)
        }

        Expr::Deref { expr: inner } => {
            let val = lower_expr(ctx, builder, inner);
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                let mut deref_place = place.clone();
                deref_place.projections.push(Projection::Deref);
                // Need to determine the dereferenced type.
                // Box[T] types are GirType::Named("Box__X"), not GirType::Ptr,
                // so use deref_inner_type() which handles both.
                let local_idx = place.local.0 as usize;
                let mut deref_type = if local_idx < builder.locals.len() {
                    let ptr_type = builder.locals[local_idx].type_id;
                    ctx.deref_inner_type(ptr_type).unwrap_or(I64_TYPE)
                } else {
                    I64_TYPE
                };
                // Bare-param wrapping: a `Box[T]` param is represented internally
                // as `*Box__T`. The user-level `*box` is a single source-level
                // deref, but at GIR level we need TWO peels — first the implicit
                // Ptr from the param wrapping, then the Box itself. After the
                // first peel above, if the result is still a `Box__T`, peel again
                // so the dst local matches the boxed value's type. Without this
                // the dst slot is sized for `Box__T` (8 bytes, the heap pointer)
                // and a downstream assign-into-T-typed-local would memcpy
                // sizeof(T) > 8 bytes, reading past the slot.
                // Detect whether we are deref'ing through a Box wrapper.
                // Two shapes reach here:
                //   (a) local_type = `*Box__T` (bare-param wrapping) — first
                //       peel gives `Box__T`, then we add a second Deref to
                //       peel the Box so dst is sized for T.
                //   (b) local_type = `Box__T` (pattern extract, local var) —
                //       first peel already gives T (via the Box name-based
                //       fallback in `deref_inner_type`); dst is sized for T.
                // In BOTH cases the resulting value is a shallow memcpy of
                // the boxed data and its heap buffers are shared with the
                // box's own drop chain — double-free if we drop-register dst.
                // Read the typed `metadata.is_box` flag at every Box TypeDef
                // registration site rather than probing by name prefix.
                let source_is_box = {
                    let src_ty = if local_idx < builder.locals.len() {
                        builder.locals[local_idx].type_id
                    } else { I64_TYPE };
                    let direct_box = ctx.type_registry.is_box(src_ty);
                    let ptr_to_box = ctx.pointee_type(src_ty)
                        .map_or(false, |inner| ctx.type_registry.is_box(inner));
                    direct_box || ptr_to_box
                };
                if ctx.type_registry.is_box(deref_type) {
                    if let Some(inner_ty) = ctx.deref_inner_type(deref_type) {
                        deref_place.projections.push(Projection::Deref);
                        deref_type = inner_ty;
                    }
                }
                // For resource-containing types, emit a deep clone via the
                // type's `_clone` runtime helper so dst owns independent
                // resources. The shallow memcpy aliases the Box's heap
                // buffers; dropping the shallow copy would double-free.
                //
                // Snag #41 audit follow-up (2026-05-13): previously gated
                // on `!is_string_type(deref_type)` — `Box[String]` deref
                // fell through to the value-typed Borrow fallback below.
                // The 1107-test sweep with GG_AUDIT_DEREF_FALLBACK
                // confirmed the fallback fired ONLY for `Box[String]` (4
                // sites) and all ran clean under valgrind because
                // downstream `ensure_owned_at_boundary` /
                // `auto_clone_if_ptr` at consume positions (push,
                // struct-init, return, fn-arg) injected the clone
                // anyway. The gate was defensive — left the architectural
                // risk in place that any new consume site without the
                // boundary helper would expose a double-free of the
                // box's String heap. Removing the gate routes Box[String]
                // through the same uniform clone path as other Box[T]
                // resources; `clone_fn_for_ptr(GorgetString)` returns
                // `gorget_str_clone` via the metadata-based protocol
                // registration at `clone_fn_name_for_def`. Closes the
                // remaining Snag #41-audit potential-bug entry.
                if source_is_box
                    && ctx.type_registry.is_resource_type(deref_type)
                {
                    if let Some(clone_fn) = ctx.clone_fn_for_ptr(deref_type) {
                        ctx.warn_clone_and_hit(builder, inner.span, deref_type, crate::ir::ImplicitCloneReason::CoWMaterialization);
                        let shallow = builder.add_local(deref_type, None);
                        // Phase C: shallow is intentionally non-owning view of
                        // the Box's content (no drop registration). Borrow
                        // mode encodes that contract — Copy would label it as
                        // a shallow alias of an owned resource.
                        builder.assign_mode(
                            crate::ir::instructions::AssignMode::Borrow,
                            Place::local(shallow),
                            Operand::Copy(deref_place),
                        );
                        // NOTE: deliberately NO drops.register_local(shallow, ...)
                        let ptr_ty = ctx.register_ptr_type(deref_type);
                        let ptr_local = builder.add_local(ptr_ty, None);
                        builder.emit_borrow(ptr_local, Place::local(shallow));
                        let cloned = builder.call_clone(&clone_fn, vec![FunctionBuilder::copy(ptr_local)], deref_type, crate::ir::ImplicitCloneReason::CoWMaterialization);
                        let dst = builder.add_local(deref_type, None);
                        // Phase C: cloned is fresh + dead — Move into dst.
                        builder.assign_mode(
                            crate::ir::instructions::AssignMode::Move,
                            Place::local(dst),
                            FunctionBuilder::copy(cloned),
                        );
                        ctx.drops.register_local(dst, deref_type, &ctx.type_registry);
                        ctx.set_owned(builder, dst);
                        return FunctionBuilder::copy(dst);
                    }
                }
                let dst = builder.add_local(deref_type, None);
                // Phase C: dst is a non-owning view of the deref'd box/ptr
                // content (no drop registration). Borrow for resource types;
                // Copy for primitives (bit-copy is correct).
                let mode = if ctx.type_registry.is_resource_type(deref_type) {
                    crate::ir::instructions::AssignMode::Borrow
                } else {
                    crate::ir::instructions::AssignMode::Copy
                };
                builder.assign_mode(mode, Place::local(dst), Operand::Copy(deref_place));
                return FunctionBuilder::copy(dst);
            }
            val
        }

        Expr::TupleFieldAccess { object, index } => {
            let obj = lower_expr(ctx, builder, object);
            if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
                // Resolve the field type from the tuple's TypeDef
                let local_idx = place.local.0 as usize;
                let elem_type = if local_idx < builder.locals.len() {
                    let tuple_type_id = builder.locals[local_idx].type_id;
                    resolve_tuple_field_type(ctx, tuple_type_id, *index)
                } else {
                    I64_TYPE
                };
                // Phase C FieldLoad migration (2026-05-06): mirror the
                // lower_field_access path — resource-typed tuple elements
                // load as Ptr(elem_type) (a borrow into the tuple's
                // storage). Auto-clone fires at ownership boundaries when
                // the dst must own.
                let result_type = if ctx.type_registry.is_resource_type(elem_type) {
                    ctx.type_registry.insert(GirType::Ptr(elem_type))
                } else {
                    elem_type
                };
                let base_local = place.local;
                let dst = builder.field_load(place.clone(), *index as u32, result_type);
                if matches!(ctx.type_registry.get(result_type), Some(GirType::Ptr(_))) {
                    ctx.set_field_borrow(builder, dst, base_local, *index as u32);
                }
                return FunctionBuilder::copy(dst);
            }
            Operand::Constant(Constant::Unit)
        }

        Expr::Is { expr: inner, negated, pattern } => {
            let val = lower_expr(ctx, builder, inner);
            let scrut_type = infer_operand_type_full(ctx, &val, builder);
            let scrut_local = builder.add_local(scrut_type, None);
            // Phase C: `is` tests the scrutinee — never consumes it. Mirror
            // the mode-picking logic from `lower_match_stmt` (stmts/patterns.rs).
            // Resource sources stage as Borrow so the scrutinee_local is a
            // non-owning view; non-resource sources stay Copy (bit-copy is
            // correct for primitives / value structs).
            let mode = {
                use crate::ir::instructions::AssignMode;
                if !ctx.type_registry.is_resource_type(scrut_type) {
                    AssignMode::Copy
                } else if let Operand::Copy(ref p) | Operand::Move(ref p) = val {
                    if p.projections.is_empty() && ctx.is_owned_local(builder, p.local) {
                        AssignMode::Move
                    } else {
                        AssignMode::Borrow
                    }
                } else {
                    AssignMode::Copy
                }
            };
            builder.assign_mode(mode, Place::local(scrut_local), val);

            // Bug-1 fix (double-eval of `is`-scrutinee): record the single
            // scrutinee local so the LATER `emit_is_bindings` pass (which runs
            // in the then/body block to bind the pattern payload) reuses it
            // instead of RE-LOWERING `inner` — a re-lower re-invokes a
            // side-effecting scrutinee (e.g. a mutating `&self` method returning
            // Option), calling it twice. Only non-negated forms bind payloads,
            // so only they need the memo. Keyed by this Is-node's span start.
            if !*negated {
                ctx.func_state
                    .is_scrut_memo
                    .insert(expr.span.start, (scrut_local, scrut_type));
            }

            let cond = super::stmts::lower_pattern_condition(
                ctx, builder, pattern, scrut_local, scrut_type,
            );
            if *negated {
                let neg = builder.un_op(UnOp::Not, BOOL_TYPE, cond);
                FunctionBuilder::copy(neg)
            } else {
                cond
            }
        }

        Expr::DefaultOp { lhs, rhs } => {
            // `lhs ?? rhs`: if lhs is Some/Ok, unwrap to T; else evaluate rhs.
            //
            // Snag #43 companion (2026-05-13): previously the result_id was
            // typed `Option[T]` (same as lhs_type) and the Some-branch copied
            // the whole Option into result_id — a shallow alias on Option's
            // Some_0 field. For non-Copy T (e.g. Option[JsValue]) this fired
            // the resource-moves validator (`shallow copy of resource _N :
            // Option__JsValue`) and aborted compilation. Surfaced by the
            // gorget-js eval.gg's `object_get_own(...) ?? JsValue.Undefined`.
            //
            // Fix: type result_id as the inner T (extracted from the first
            // variant's field 0). The Some-branch uses `enum_field_load_move`
            // to extract field 0 with Move semantics — the LIR zeros the
            // source's payload field, preventing the alias. The None-branch
            // assigns rhs_val (already typed T) into result_id with a mode
            // chosen to avoid shallow-copy when T is a resource.
            let lhs_val = lower_expr(ctx, builder, lhs);
            let raw_lhs_type = infer_operand_type_full(ctx, &lhs_val, builder);
            // Non-Copy params pass by pointer (`*Option[T]`); peel one layer
            // so the variant lookup hits the actual enum's TypeDef. Without
            // this, gorget-js critique item #2 follow-on bug: `??` on an
            // Option-typed param mis-classifies the variant (lhs_type stays
            // Ptr) and the lowering emits an Option-clone wrapped around a
            // T-typed phi — the LIR phi typed the merged path's bytes as
            // Option[T], called Option__T__clone on a T pointer, and the
            // returned "Option" was garbage.
            let lhs_type = match ctx.type_registry.get(raw_lhs_type) {
                Some(crate::ir::types::GirType::Ptr(inner))
                | Some(crate::ir::types::GirType::MutPtr(inner)) => *inner,
                _ => raw_lhs_type,
            };

            // Inner T = variant 0's field 0 type. Variant name = "Some" for
            // Option, "Ok" for Result — derive from the type def rather than
            // hardcoding, so user-defined `__some/__none`-shaped enums also
            // work if anyone wires them up.
            let (variant_name, inner_type) = {
                let mut found: Option<(String, TypeId)> = None;
                if let Some(name) = ctx.type_registry.type_name(lhs_type) {
                    if let Some(td) = ctx.type_registry.get_type_def(&name) {
                        if let crate::ir::types::TypeDefKind::Enum(ref e) = td.kind {
                            if let Some(v) = e.variants.first() {
                                if let Some(f) = v.fields.first() {
                                    found = Some((v.name.clone(), f.type_id));
                                }
                            }
                        }
                    }
                }
                found.unwrap_or_else(|| (String::from("Some"), lhs_type))
            };

            // Pick the right staging shape for the source.
            //   (a) Source is a bare named local that owns its data — use
            //       directly as `scrut_place`; subsequent Move-extract
            //       zeros the source's Some_0 (correct: source's drop is
            //       a no-op on zeroed payload via cap=0 / NULL-pointer).
            //   (b) Source is a borrowed Ptr (non-Copy param) — clone the
            //       whole Option into a fresh owned local first, then use
            //       that. Necessary because Move-extracting from a borrow
            //       would zero the caller's slot, and Borrow-extracting
            //       + cloning the inner value tripped a separate alias
            //       (the user's match-on-result_id later observed both an
            //       extracted-then-zeroed scrut and the alive result_id).
            //   (c) Source is a constant / complex operand — stage Copy
            //       into a fresh local.
            let raw_src_place = if let Operand::Copy(ref p) | Operand::Move(ref p) = lhs_val {
                if p.projections.is_empty() { Some(p.clone()) } else { None }
            } else { None };
            let src_is_borrowed = if let Some(ref p) = raw_src_place {
                let lty = builder.local_type(p.local);
                matches!(
                    ctx.type_registry.get(lty),
                    Some(crate::ir::types::GirType::Ptr(_) | crate::ir::types::GirType::MutPtr(_))
                ) || ctx.is_ref_local(builder, p.local)
            } else { false };
            let scrut_place = if src_is_borrowed {
                // Clone the whole Option via its `_clone` runtime helper
                // so the rest of the lowering sees an owned scrutinee.
                // Mirrors `lower_call_arg`'s Ownership::Move from-borrow
                // path at `src/ir/lowering/exprs/calls.rs:237-245`.
                let p = raw_src_place.expect("borrowed source implies place");
                if let Some(clone_fn) = ctx.clone_fn_for_ptr(lhs_type) {
                    let cloned = ctx.emit_clone(builder, &clone_fn, vec![FunctionBuilder::copy(p.local)], lhs.span, lhs_type, crate::ir::ImplicitCloneReason::CoWMaterialization);
                    let lhs_local = builder.add_local(lhs_type, None);
                    builder.assign_mode(
                        crate::ir::instructions::AssignMode::Move,
                        Place::local(lhs_local),
                        FunctionBuilder::copy(cloned),
                    );
                    ctx.drops.register_local(lhs_local, lhs_type, &ctx.type_registry);
                    ctx.set_owned(builder, lhs_local);
                    Place::local(lhs_local)
                } else {
                    // No clone fn available — fall through to plain
                    // assign as a best-effort. Should be unreachable for
                    // resource-typed Options (they all have generated
                    // clones), but guard against type-registry races.
                    let lhs_local = builder.add_local(lhs_type, None);
                    builder.assign(Place::local(lhs_local), lhs_val);
                    Place::local(lhs_local)
                }
            } else if let Some(p) = raw_src_place {
                p
            } else {
                let lhs_local = builder.add_local(lhs_type, None);
                builder.assign(Place::local(lhs_local), lhs_val);
                Place::local(lhs_local)
            };

            // Tag check: variant 0 (Some/Ok) means "has value".
            let tag = builder.tag_of(Operand::Copy(scrut_place.clone()));
            let is_some = builder.cmp(
                CmpOp::Eq,
                I32_TYPE,
                FunctionBuilder::copy(tag),
                Operand::Constant(Constant::I32(0)),
            );

            // result_id typed T (the inner type) — NOT Option[T].
            let result_id = builder.add_local(inner_type, None);
            let then_bb = builder.new_block();
            let else_bb = builder.new_block();
            let merge_bb = builder.new_block();

            builder.branch(FunctionBuilder::copy(is_some), then_bb, else_bb);

            // Some/Ok path: extract field 0 with Move semantics (zeros the
            // source's payload field), then Move the extracted T into
            // result_id. The extracted local owns the bytes after
            // enum_field_load_move — mark Owned + register for drop so the
            // Tier 2a "AssignIntoOwnedSlot from untracked source" validator
            // sees a tracked Owned source on the subsequent Move.
            builder.switch_to(then_bb);
            // scrut is now always owned (the borrowed-source clone above
            // forces it). Move-extract zeros source's Some_0 and gives us
            // an owned T; Move-assign that into result_id.
            let extracted = builder.enum_field_load_move(
                scrut_place.clone(),
                variant_name,
                0,
                inner_type,
            );
            if ctx.type_registry.is_resource_type(inner_type) {
                ctx.set_owned(builder, extracted);
                builder.assign_mode(
                    crate::ir::instructions::AssignMode::Move,
                    Place::local(result_id),
                    FunctionBuilder::copy(extracted),
                );
            } else {
                builder.assign(Place::local(result_id), FunctionBuilder::copy(extracted));
            }
            builder.jump(merge_bb);

            // None path: lower rhs and assign into result_id. Move mode for
            // resource T so a fresh-constructed rhs (e.g. `JsValue.Undefined`)
            // doesn't shallow-alias result_id.
            builder.switch_to(else_bb);
            let rhs_val = lower_expr(ctx, builder, rhs);
            if ctx.type_registry.is_resource_type(inner_type) {
                builder.assign_mode(
                    crate::ir::instructions::AssignMode::Move,
                    Place::local(result_id),
                    rhs_val,
                );
            } else {
                builder.assign(Place::local(result_id), rhs_val);
            }
            builder.jump(merge_bb);

            builder.switch_to(merge_bb);
            // Register result_id for drop when T needs one (resource T).
            // result_id owns the data after either branch — Some-path moved
            // it out of lhs_local, None-path moved it from a fresh rhs.
            if ctx.type_registry.needs_drop(inner_type) {
                ctx.drops.register_local(result_id, inner_type, &ctx.type_registry);
                ctx.set_owned(builder, result_id);
            }
            FunctionBuilder::copy(result_id)
        }

        Expr::Path { segments } => {
            // Qualified enum variant path: Color.Red (2+ segments)
            if segments.len() >= 2 {
                let enum_name = &segments[0].node;
                let variant_name = &segments.last().unwrap().node;
                if let Some(type_id) = ctx.type_mapper.lookup_named(enum_name) {
                    if let Some(type_def) = ctx.type_registry.get_type_def(enum_name) {
                        if let TypeDefKind::Enum(ref e) = type_def.kind {
                            if e.variants.iter().any(|v| &v.name == variant_name) {
                                let dst = builder.enum_init(enum_name, variant_name, type_id, vec![]);
                                return FunctionBuilder::copy(dst);
                            }
                        }
                    }
                }
            }
            // Single-segment path — try as enum variant (prelude: None, Some, Ok, Error).
            // SSOT: type-aware to disambiguate same-named variants across enums.
            if let Some(last) = segments.last() {
                if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant_typed(&last.node, ctx.func_state.expected_type) {
                    let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
                    let dst = builder.enum_init(&enum_name, &variant_name, type_id, vec![]);
                    return FunctionBuilder::copy(dst);
                }
                // Try as identifier
                if let Some((local_id, _)) = ctx.lookup_local(&last.node) {
                    return Operand::Copy(Place::local(local_id));
                }
            }
            Operand::Constant(Constant::Unit)
        }

        // P3.5.2: Dict literals
        Expr::DictLiteral(pairs) => {
            lower_dict_literal(ctx, builder, pairs)
        }

        // P3.5.3: List comprehensions
        Expr::ListComprehension { expr: comp_expr, variable, iterable, condition, .. } => {
            lower_list_comprehension(ctx, builder, comp_expr, variable, iterable, condition.as_deref())
        }

        // P3.5.4: Dict comprehensions
        Expr::DictComprehension { key, value, variables, iterable, condition } => {
            lower_dict_comprehension(ctx, builder, key, value, variables, iterable, condition.as_deref())
        }

        // P3.5.4: Set comprehensions
        Expr::SetComprehension { expr: comp_expr, variable, iterable, condition } => {
            lower_set_comprehension(ctx, builder, comp_expr, variable, iterable, condition.as_deref())
        }

        // P3.5.5: Implicit closures
        Expr::ImplicitClosure { body } => {
            let param = ast::ClosureParam {
                type_: None,
                ownership: ast::Ownership::Borrow,
                name: Spanned::dummy("it".to_string()),
                destructure: None,
            };
            let params = vec![Spanned::dummy(param)];
            let mut cl = std::mem::take(&mut ctx.closures);
            let result = cl.lower_closure(ctx, builder, &params, body, false, expr.span);
            ctx.closures = cl;
            result
        }

        // P3.5.6: Optional chaining
        Expr::OptionalChain { object, field } => {
            lower_optional_chain(ctx, builder, object, field)
        }

        // P3.5.7: Range expressions (standalone)
        Expr::Range { start, end, inclusive } => {
            lower_range_expr(ctx, builder, start.as_deref(), end.as_deref(), *inclusive)
        }

        // Await: check if this is awaiting a Task (spawn result) and dispatch via __gorget_await_<fn>.
        // In synchronous GIR mode for non-task expressions, just lower the inner expression.
        Expr::Await { expr } => {
            let inner = lower_expr(ctx, builder, expr);
            // Extract receiver local before inner is consumed by the call.
            let inner_local = match &inner {
                Operand::Copy(place) | Operand::Move(place)
                    if place.projections.is_empty() => Some(place.local),
                _ => None,
            };
            // Direct local lookup (simple `await task` case)
            let task_local = inner_local.and_then(|lid| {
                ctx.spawn.result_locals.get(&lid).cloned()
                    .map(|fn_name| (Some(lid), fn_name))
            });
            // Fallback: type-based lookup for indexed tasks (e.g., `await tasks[j]`)
            let resolved = task_local.or_else(|| {
                let type_id = inner_local.map(|lid| builder.local_type(lid));
                type_id.and_then(|tid| {
                    ctx.spawn.task_type_fns.get(&tid).and_then(|fns| {
                        if fns.len() == 1 { Some((None, fns[0].clone())) } else { None }
                    })
                })
            });
            if let Some((maybe_local_id, fn_name)) = resolved {
                let ret_type = ctx.fn_sigs.get(fn_name.as_str())
                    .map(|(_, r)| *r)
                    .unwrap_or(UNIT_TYPE);

                let await_fn = format!("__gorget_await_{fn_name}");
                let result = if ret_type == UNIT_TYPE {
                    builder.call_void(&await_fn, vec![inner]);
                    Operand::Constant(Constant::Unit)
                } else {
                    let dst = builder.call(&await_fn, vec![inner], ret_type);
                    FunctionBuilder::copy(dst)
                };

                // Zero out the Task local after await to prevent double-join in drop.
                // For direct spawn results, maybe_local_id is Some. For tasks from
                // other sources (e.g. Vector.remove().unwrap()), use inner_local.
                let zero_local = maybe_local_id.or(inner_local);
                if let Some(local_id) = zero_local {
                    ctx.move_zero_and_mark(builder, local_id);
                }

                // Auto-refresh `with shared_var:` bindings after await
                emit_with_shared_refresh(ctx, builder);

                return result;
            }
            // Value-routed fallback for a collection-sourced Task[void] whose
            // TypeId maps to >1 DISTINCT producer fn (SIBLING of the postfix
            // `.await()` site in methods.rs — fix the class, not the instance).
            // Dispatch through the value's own carried __drop pointer via
            // Task__void__await. `inner_local` is in scope here (declared at the
            // top of the arm).
            if let Some(local_id) = inner_local {
                let tid = builder.local_type(local_id);
                // Equality against the registered type name (typed accessor) —
                // the C-emit-symbol-boundary idiom, not a prefix heuristic.
                if ctx.type_name_for_id(tid) == Some("Task__void") {
                    builder.call_void("Task__void__await", vec![inner]);
                    ctx.move_zero_and_mark(builder, local_id);
                    // Prefix-ONLY: refresh `with shared_var:` bindings after the
                    // value-route, mirroring the named path (:1079). The postfix
                    // `.await()` site has no such refresh, so it stays absent
                    // there. Keeps the named/value routes consistent for a
                    // `.await()` inside a `with shared:` block.
                    emit_with_shared_refresh(ctx, builder);
                    return Operand::Constant(Constant::Unit);
                }
            }
            inner
        }

        // Spawn: emit __gorget_spawn_<fn>(args) call, which creates a pthread.
        // Task result locals are tracked in spawn_result_locals for await dispatch.
        Expr::Spawn { expr, .. } => {
            if let Expr::Call { callee, args: call_args, .. } = &expr.node {
                // ── Case A: spawn c(args) where c is a local closure variable ──
                if let Expr::Identifier(fn_name) = &callee.node {
                    if let Some((local_id, local_type_id)) = ctx.lookup_local(fn_name) {
                        if let Some(type_name) = ctx.type_name_for_id(local_type_id).map(|s| s.to_string()) {
                            if ctx.lookup_closure_info(&type_name).is_some() {
                                let (call_fn_name, struct_type_id, captures) =
                                    ctx.lookup_closure_info(&type_name)
                                        .map(|(cfn, stid, caps)| {
                                            (cfn.to_string(), stid, caps.to_vec())
                                        })
                                        .unwrap();
                                let call_args_cloned: Vec<_> = call_args.iter().cloned().collect();
                                return lower_closure_spawn(
                                    ctx, builder,
                                    local_id, local_type_id,
                                    &type_name, &call_fn_name, struct_type_id,
                                    &captures, &call_args_cloned,
                                );
                            }
                        }
                    }
                }

                // ── Case B: spawn ((): body)(args) — inline closure literal ──
                if let Expr::Closure { params, body, is_move, .. } = &callee.node {
                    let params_cloned = params.clone();
                    let body_cloned = body.clone();
                    let is_move_val = *is_move;
                    let call_args_cloned: Vec<_> = call_args.iter().cloned().collect();

                    let mut cl = std::mem::take(&mut ctx.closures);
                    let closure_op = cl.lower_closure(ctx, builder, &params_cloned, &body_cloned, is_move_val, expr.span);
                    ctx.closures = cl;

                    if let Operand::Copy(ref place) | Operand::Move(ref place) = closure_op {
                        if place.projections.is_empty() {
                            let closure_local = place.local;
                            let closure_type_id = builder.local_type(closure_local);
                            if let Some(type_name) = ctx.type_name_for_id(closure_type_id).map(|s| s.to_string()) {
                                if ctx.lookup_closure_info(&type_name).is_some() {
                                    let (call_fn_name, struct_type_id, captures) =
                                        ctx.lookup_closure_info(&type_name)
                                            .map(|(cfn, stid, caps)| {
                                                (cfn.to_string(), stid, caps.to_vec())
                                            })
                                            .unwrap();
                                    return lower_closure_spawn(
                                        ctx, builder,
                                        closure_local, closure_type_id,
                                        &type_name, &call_fn_name, struct_type_id,
                                        &captures, &call_args_cloned,
                                    );
                                }
                            }
                        }
                    }
                    // Inline closure lowering succeeded but no closure info found — fall through
                }

                // ── Direct function call spawn (original path) ──
                if let Expr::Identifier(fn_name) = &callee.node {
                    // Resolve the actual C symbol name (Phase 5 mangled for module functions,
                    // or bare Gorget name for entry-module functions).  The spawn infrastructure
                    // (context struct, thread wrapper, spawn/await helpers) is keyed by this
                    // C name so that the internal call uses the right symbol.
                    let c_name = ctx.extern_bindings.get(fn_name.as_str())
                        .cloned()
                        .unwrap_or_else(|| fn_name.clone());

                    // fn_sigs is keyed by the Gorget bare name for lookup purposes.
                    let callee_param_types = ctx.fn_sigs.get(fn_name.as_str())
                        .map(|(p, _)| p.clone())
                        .unwrap_or_default();
                    let fn_ret_type = ctx.fn_sigs.get(fn_name.as_str())
                        .map(|(_, r)| *r)
                        .unwrap_or(I64_TYPE);

                    // Detect shared args: check each call arg against shared_locals
                    let param_ownerships = ctx.fn_param_ownerships.get(fn_name.as_str())
                        .cloned()
                        .unwrap_or_default();

                    let mut shared_spawn_args: Vec<SharedSpawnArg> = Vec::new();
                    let mut has_any_shared = false;
                    for (i, arg) in call_args.iter().enumerate() {
                        if let Expr::Identifier(arg_name) = &arg.node.value.node {
                            if let Some((local_id, _)) = ctx.lookup_local(arg_name) {
                                if let Some(info) = ctx.shared.locals.get(&local_id) {
                                    let (inner_type, wrapper_type, kind, ast_shared) = (info.inner_type, info.wrapper_type, info.kind, info.ast_shared);
                                    has_any_shared = true;
                                    // Only auto-decided shared vars get token wrappers.
                                    // User overrides (shared(atomic), shared(rwlock)) pass
                                    // the raw sync primitive — the callee uses it directly.
                                    if ast_shared == ast::SharedKind::Auto {
                                        let is_mutable = param_ownerships.get(i)
                                            .map_or(false, |o| matches!(o, Ownership::MutableBorrow));
                                        shared_spawn_args.push(SharedSpawnArg {
                                            arg_index: i,
                                            kind,
                                            inner_type,
                                            wrapper_type,
                                            is_mutable,
                                            decl_order: local_id.0,
                                        });
                                    }
                                }
                            }
                        }
                    }

                    // Detect inner shared spawn: when an arg is a param of the
                    // current function (not a declared shared), record the mapping
                    // so the shared_async transform can propagate the wrapper.
                    if shared_spawn_args.is_empty() {
                        let mut inner_mappings: Vec<(usize, usize)> = Vec::new();
                        for (i, arg) in call_args.iter().enumerate() {
                            if let Expr::Identifier(arg_name) = &arg.node.value.node {
                                if let Some((local_id, _)) = ctx.lookup_local(arg_name) {
                                    let idx = local_id.0 as usize;
                                    // Is this a param? params are locals _1.._N
                                    if idx >= 1 && idx <= builder.params.len() {
                                        let param_idx = idx - 1; // 0-based param index
                                        // Only record if the callee expects a mutable borrow
                                        let callee_is_mut = param_ownerships.get(i)
                                            .map_or(false, |o| matches!(o, Ownership::MutableBorrow));
                                        if callee_is_mut {
                                            inner_mappings.push((i, param_idx));
                                        }
                                    }
                                }
                            }
                        }
                        if !inner_mappings.is_empty() {
                            let callee_has_awaits = ctx.shared.fn_ast_bodies.get(fn_name.as_str())
                                .map_or(false, |func_def| {
                                    if let crate::parser::ast::FunctionBody::Block(block) = &func_def.body {
                                        block.stmts.iter().any(|s| super::context::stmt_has_await(&s.node))
                                    } else {
                                        false
                                    }
                                });
                            builder.inner_shared_spawns.push(crate::ir::InnerSharedSpawn {
                                callee_name: c_name.clone(),
                                callee_param_types: callee_param_types.clone(),
                                callee_return_type: fn_ret_type,
                                shared_arg_mappings: inner_mappings,
                                callee_has_awaits,
                                callee_param_ownerships: param_ownerships.clone(),
                            });
                        }
                    }

                    // Map return TypeId → C type name → Task__<c_type> name.
                    let ret_c = ctx.type_name_for_id(fn_ret_type)
                        .unwrap_or("int64_t")
                        .to_string();
                    let task_name = if fn_ret_type == UNIT_TYPE {
                        "Task__void".to_string()
                    } else {
                        format!("Task__{ret_c}")
                    };
                    let task_type = if let Some(tid) = ctx.type_mapper.lookup_named(&task_name) {
                        tid
                    } else {
                        // Register Task TypeDef with Move semantics + RAII join-on-drop.
                        ctx.type_registry.add_type_def(TypeDef {
                            name: task_name.clone(),
                            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
                            metadata: TypeMetadata {
                                size: None,
                                align: None,
                                drop_strategy: DropStrategy::Trivial(format!("{task_name}__drop")),
                                copy_semantics: CopySemantics::Resource,
                                ..Default::default()
                            },
                        });
                        let tid = ctx.type_registry.insert(GirType::Named(task_name.clone()));
                        ctx.type_mapper.register_named(task_name.clone(), tid);
                        tid
                    };

                    if !shared_spawn_args.is_empty() {
                        // Check if callee is async with await points — needs
                        // async-aware token management (release at await, reacquire after).
                        let callee_has_awaits = ctx.shared.fn_ast_bodies.get(fn_name.as_str())
                            .map_or(false, |func_def| {
                                if let crate::parser::ast::FunctionBody::Block(block) = &func_def.body {
                                    block.stmts.iter().any(|s| super::context::stmt_has_await(&s.node))
                                } else {
                                    false
                                }
                            });

                        let wrapper_name = if callee_has_awaits {
                            format!("__shared_async_{c_name}")
                        } else {
                            format!("__shared_token_{c_name}")
                        };

                        if !ctx.spawn.fn_names.contains_key(&wrapper_name) {
                            if callee_has_awaits {
                                // Async-aware variant: defer generation until after all functions
                                // are lowered. The GIR-to-GIR transform will operate on the
                                // already-lowered source function.
                                use crate::ir::transforms::shared_async::{SharedArgSpec, PendingSharedVariant};
                                let specs: Vec<SharedArgSpec> = shared_spawn_args.iter().map(|sa| {
                                    let inner_c = ctx.c_type_name_for_id(sa.inner_type);
                                    let mutex_mangled = format!("Mutex__{inner_c}");
                                    let guard_mangled = format!("Guard__{inner_c}");
                                    let mutex_type = ctx.type_mapper.lookup_named(&mutex_mangled)
                                        .unwrap_or(sa.inner_type);
                                    let guard_type = ctx.type_mapper.lookup_named(&guard_mangled)
                                        .unwrap_or(sa.inner_type);
                                    SharedArgSpec {
                                        arg_index: sa.arg_index,
                                        inner_type: sa.inner_type,
                                        wrapper_type: sa.wrapper_type,
                                        mutex_type,
                                        guard_type,
                                        is_mutable: sa.is_mutable,
                                        decl_order: sa.decl_order,
                                        inner_c_name: inner_c,
                                    }
                                }).collect();
                                ctx.shared.pending_variants.push(PendingSharedVariant {
                                    source_fn_name: c_name.clone(),
                                    variant_name: wrapper_name.clone(),
                                    shared_args: specs,
                                    return_type: fn_ret_type,
                                });
                            } else {
                                // Synchronous wrapper: lock for entire call, no await points.
                                let wrapper_fn = build_shared_token_wrapper(
                                    ctx,
                                    &wrapper_name,
                                    &c_name,
                                    &callee_param_types,
                                    &shared_spawn_args,
                                    fn_ret_type,
                                );
                                ctx.spawn.wrapper_fns.push(wrapper_fn);
                            }

                            // Register wrapper signature: wrapper params → return type
                            let wrapper_param_types: Vec<TypeId> = callee_param_types.iter().enumerate()
                                .map(|(i, &callee_type)| {
                                    shared_spawn_args.iter()
                                        .find(|sa| sa.arg_index == i)
                                        .map(|sa| sa.wrapper_type)
                                        .unwrap_or(callee_type)
                                })
                                .collect();
                            ctx.fn_sigs.insert(wrapper_name.clone(), (wrapper_param_types, fn_ret_type));

                            let param_names: Vec<String> = (0..callee_param_types.len())
                                .map(|i| format!("__p{i}"))
                                .collect();
                            ctx.fn_param_names.insert(wrapper_name.clone(), param_names);
                        }

                        ctx.spawn.pending_fn = Some(wrapper_name.clone());
                        ctx.spawn.fn_names.insert(wrapper_name.clone(), true);
                        ctx.spawn.register_task_type_fn(task_type, wrapper_name.clone());

                        // Lower args: shared vars pass the raw sync primitive
                        ctx.shared.pass_raw = true;
                        let lowered_args: Vec<Operand> = call_args.iter()
                            .map(|arg| lower_expr(ctx, builder, &arg.node.value))
                            .collect();
                        ctx.shared.pass_raw = false;

                        let spawn_fn = format!("__gorget_spawn_{wrapper_name}");
                        let dst = builder.call(&spawn_fn, lowered_args, task_type);
                        return FunctionBuilder::copy(dst);
                    } else {
                        // No Auto shared args — spawn the original function directly.
                        // If there are user-overridden shared vars, pass them raw.
                        ctx.spawn.pending_fn = Some(c_name.clone());
                        ctx.spawn.fn_names.insert(c_name.clone(), true);
                        ctx.spawn.register_task_type_fn(task_type, c_name.clone());

                        if has_any_shared {
                            ctx.shared.pass_raw = true;
                        }
                        let lowered_args: Vec<Operand> = call_args.iter()
                            .map(|arg| lower_expr(ctx, builder, &arg.node.value))
                            .collect();
                        ctx.shared.pass_raw = false;
                        let spawn_fn = format!("__gorget_spawn_{c_name}");
                        let dst = builder.call(&spawn_fn, lowered_args, task_type);
                        return FunctionBuilder::copy(dst);
                    }
                }
            }
            // ── Case D: spawn receiver.method(args) — method call ──
            if let Expr::MethodCall { receiver, method, args: call_args, .. } = &expr.node {
                return lower_method_spawn(ctx, builder, receiver, &method.node, call_args);
            }
            // Fallback: direct call (no tracking)
            lower_expr(ctx, builder, expr)
        }

        // spawn blocking fn(args) — runs on the expandable blocking pool
        Expr::SpawnBlocking { expr, .. } => {
            if let Expr::Call { callee, args: call_args, .. } = &expr.node {
                if let Expr::Identifier(fn_name) = &callee.node {
                    let c_name = ctx.extern_bindings.get(fn_name.as_str())
                        .cloned()
                        .unwrap_or_else(|| fn_name.clone());

                    let fn_ret_type = ctx.fn_sigs.get(fn_name.as_str())
                        .map(|(_, r)| *r)
                        .unwrap_or(I64_TYPE);

                    // Map return TypeId → C type name → Task__<c_type> name.
                    let ret_c = ctx.type_name_for_id(fn_ret_type)
                        .unwrap_or("int64_t")
                        .to_string();
                    let task_name = if fn_ret_type == UNIT_TYPE {
                        "Task__void".to_string()
                    } else {
                        format!("Task__{ret_c}")
                    };
                    let task_type = if let Some(tid) = ctx.type_mapper.lookup_named(&task_name) {
                        tid
                    } else {
                        ctx.type_registry.add_type_def(TypeDef {
                            name: task_name.clone(),
                            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
                            metadata: TypeMetadata {
                                size: None,
                                align: None,
                                drop_strategy: DropStrategy::Trivial(format!("{task_name}__drop")),
                                copy_semantics: CopySemantics::Resource,
                                ..Default::default()
                            },
                        });
                        let tid = ctx.type_registry.insert(GirType::Named(task_name.clone()));
                        ctx.type_mapper.register_named(task_name.clone(), tid);
                        tid
                    };

                    ctx.spawn.pending_fn = Some(c_name.clone());
                    ctx.spawn.fn_names.insert(c_name.clone(), true);
                    ctx.spawn.blocking_fn_names.insert(c_name.clone());
                    ctx.spawn.register_task_type_fn(task_type, c_name.clone());

                    let lowered_args: Vec<Operand> = call_args.iter()
                        .map(|arg| lower_expr(ctx, builder, &arg.node.value))
                        .collect();
                    let spawn_fn = format!("__gorget_spawn_{c_name}");
                    let dst = builder.call(&spawn_fn, lowered_args, task_type);
                    return FunctionBuilder::copy(dst);
                }
            }
            lower_expr(ctx, builder, expr)
        }

        // Dot-shorthand variant: .Red() or .Blue(42)
        // Resolves to the enum variant using the expected type from context.
        Expr::DotShorthand { variant, args } => {
            let variant_name = variant.node.clone();
            let arg_spans: Vec<Option<crate::span::Span>> = args.iter()
                .map(|a| Some(a.node.value.span))
                .collect();
            let lowered_args: Vec<Operand> = args.iter()
                .map(|a| lower_expr(ctx, builder, &a.node.value))
                .collect();

            // 1. Try expected_type (set by VarDecl, Assign, Return, or function arg)
            if let Some(et) = ctx.func_state.expected_type {
                if let Some(type_name) = ctx.type_registry.type_name(et) {
                    if let Some(type_def) = ctx.type_registry.get_type_def(&type_name) {
                        if let TypeDefKind::Enum(ref e) = type_def.kind {
                            if e.variants.iter().any(|v| v.name == variant_name) {
                                let dst = ctx.emit_enum_init_owned(builder, &type_name, &variant_name, et, lowered_args, Some(arg_spans));
                                return FunctionBuilder::copy(dst);
                            }
                        }
                    }
                }
            }

            // 2. Fallback: variant map (for user-defined non-generic enums).
            // SSOT: route through the type-aware helper for consistency with the
            // other bare ctor sites — the membership-gated expected_type check at
            // (1) already fired, so this re-fails it and falls to the flat map
            // (harmless no-op here, but keeps every bare ctor read on one accessor).
            if let Some((enum_name, vn)) = ctx.resolve_enum_variant_typed(&variant_name, ctx.func_state.expected_type) {
                let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
                let dst = ctx.emit_enum_init_owned(builder, &enum_name, &vn, type_id, lowered_args, Some(arg_spans));
                return FunctionBuilder::copy(dst);
            }

            Operand::Constant(Constant::Unit)
        }
        Expr::MetaOpInfix { .. } => {
            // Should have been substituted by the meta pass before lowering.
            panic!("MetaOpInfix not substituted before GIR lowering — meta substitution pass incomplete")
        }
        Expr::MetaOpToken(_) => {
            // Should have been filtered out at the call site before reaching here.
            panic!("MetaOpToken not filtered out before GIR lowering — call lowering incomplete")
        }
        Expr::Rethrow { expr: inner, error_binding, transform } => {
            lower_rethrow_expr(ctx, builder, inner, error_binding.as_ref(), transform)
        }
        Expr::Catch { expr: inner, error_binding, recovery } => {
            lower_catch_expr(ctx, builder, inner, error_binding, recovery)
        }
        Expr::FaultCatch { expr: inner, pattern, handler } => {
            lower_fault_catch_expr(ctx, builder, inner, pattern, handler)
        }
    }
}

/// Lower a struct literal (constructor call).
/// Resolve Option/Result variant constructors (Some, None, Ok, Error) with type-aware logic.
/// Returns Some(operand) if the call is a recognized built-in variant, None otherwise.
fn resolve_option_result_variant(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    name: &str,
    args: &[Spanned<Expr>],
) -> Option<Operand> {
    match name {
        "Some" if args.len() == 1 => {
            let arg_span = args[0].span;
            let field_op = lower_expr(ctx, builder, &args[0]);
            let inner_type = infer_operand_type_full(ctx, &field_op, builder);
            let mangled = format!("Option__{}", format_type_for_mangle(inner_type, &ctx.type_registry));
            let type_id = ctx.type_mapper.lookup_named(&mangled)
                .or_else(|| {
                    // Fall back to expected type from context (e.g., VarDecl target)
                    ctx.func_state.expected_type.and_then(|et| {
                        let is_option = ctx.type_registry.enum_category(et) == Some(EnumCategory::Option);
                        if is_option {
                            Some(et)
                        } else {
                            None
                        }
                    })
                })
                .unwrap_or_else(|| {
                    // Register Option__<T> on demand. Phase 1.7b shifted borrowing
                    // builtins to Option__Ref__<T>, so Option__<T> isn't always
                    // pre-registered when a user writes a bare `Some(x)`.
                    ctx.ensure_option_type_registered(&mangled, inner_type);
                    ctx.type_mapper.lookup_named(&mangled).unwrap_or(UNIT_TYPE)
                });
            let type_name = ctx.type_registry.type_name(type_id).unwrap_or_else(|| mangled.clone());
            let dst = ctx.emit_enum_init_owned(builder, &type_name, "Some", type_id, vec![field_op], Some(vec![Some(arg_span)]));
            Some(FunctionBuilder::copy(dst))
        }
        "None" if args.is_empty() => {
            // None() has no arguments — determine type from context
            let (type_name, type_id) = if let Some(et) = ctx.func_state.expected_type {
                let name = ctx.type_registry.type_name(et)
                    .unwrap_or_else(|| "Option__int64_t".to_string());
                let is_option = ctx.type_registry.enum_category(et) == Some(EnumCategory::Option);
                if is_option {
                    (name, et)
                } else {
                    // Expected type isn't Option — fall back to enum_variants
                    return None;
                }
            } else {
                // No context — fall back to enum_variants
                return None;
            };
            let dst = builder.enum_init(&type_name, "None", type_id, vec![]);
            ctx.set_owned(builder, dst);
            Some(FunctionBuilder::copy(dst))
        }
        "Ok" if args.len() == 1 => {
            // Ok(value) — determine Result type from context (expected_type).
            // Use emit_enum_init_owned to clone borrowed resource args (e.g. BareParam strings).
            let arg_span = args[0].span;
            if let Some(et) = ctx.func_state.expected_type {
                let name = ctx.type_registry.type_name(et).unwrap_or_default();
                let is_result = ctx.type_registry.enum_category(et) == Some(EnumCategory::Result);
                if is_result {
                    let field_op = lower_expr(ctx, builder, &args[0]);
                    let dst = ctx.emit_enum_init_owned(builder, &name, "Ok", et, vec![field_op], Some(vec![Some(arg_span)]));
                    return Some(FunctionBuilder::copy(dst));
                }
            }
            // Also check current_throws_result_type
            if let Some(rt) = ctx.func_state.current_throws_result_type {
                let name = ctx.type_registry.type_name(rt).unwrap_or_default();
                let is_result = ctx.type_registry.enum_category(rt) == Some(EnumCategory::Result);
                if is_result {
                    let field_op = lower_expr(ctx, builder, &args[0]);
                    let dst = ctx.emit_enum_init_owned(builder, &name, "Ok", rt, vec![field_op], Some(vec![Some(arg_span)]));
                    return Some(FunctionBuilder::copy(dst));
                }
            }
            None // Fall through to generic enum_variants
        }
        "Error" if args.len() == 1 => {
            // Error(value) — determine Result type from context.
            // Use emit_enum_init_owned to clone non-owned string views and
            // MoveZero consumed args — matches Ok/Some/EnumConstructor paths.
            let arg_span = args[0].span;
            if let Some(et) = ctx.func_state.expected_type {
                let name = ctx.type_registry.type_name(et).unwrap_or_default();
                let is_result = ctx.type_registry.enum_category(et) == Some(EnumCategory::Result);
                if is_result {
                    let field_op = lower_expr(ctx, builder, &args[0]);
                    let dst = ctx.emit_enum_init_owned(builder, &name, "Error", et, vec![field_op], Some(vec![Some(arg_span)]));
                    return Some(FunctionBuilder::copy(dst));
                }
            }
            if let Some(rt) = ctx.func_state.current_throws_result_type {
                let name = ctx.type_registry.type_name(rt).unwrap_or_default();
                let is_result = ctx.type_registry.enum_category(rt) == Some(EnumCategory::Result);
                if is_result {
                    let field_op = lower_expr(ctx, builder, &args[0]);
                    let dst = ctx.emit_enum_init_owned(builder, &name, "Error", rt, vec![field_op], Some(vec![Some(arg_span)]));
                    return Some(FunctionBuilder::copy(dst));
                }
            }
            None
        }
        _ => None,
    }
}

fn lower_struct_literal(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    name: &str,
    args: &[Spanned<Expr>],
    generic_args: Option<&[Spanned<ast::Type>]>,
) -> Operand {
    // Intercept String("...") constructor → gorget_string_from_str(str)
    // Intercept String(capacity) constructor → gorget_string_with_capacity(int)
    if name == "String" && args.len() == 1 {
        let arg_op = lower_expr(ctx, builder, &args[0]);
        let owned_type = ctx.type_mapper.owned_string_type;
        // Check if the arg is an integer (capacity) vs string (content).
        // All 8 int widths route to the capacity ctor (shared predicate,
        // `is_int_type_id`, same routing as the named-arg sibling in
        // exprs/calls.rs); non-int/non-String args are rejected at typecheck.
        let arg_type = super::exprs::infer_operand_type_full(ctx, &arg_op, builder);
        let fn_name = if is_int_type_id(arg_type) {
            "gorget_string_with_capacity"
        } else {
            "gorget_string_from_str"
        };
        let dst = ctx.call_extern_tracked(builder, fn_name, vec![arg_op], owned_type);
        return FunctionBuilder::copy(dst);
    }
    // String() with no args → empty GorgetString
    if name == "String" && args.is_empty() {
        return Operand::Constant(Constant::Unit); // C backend handles Unit → gorget_string_new("")
    }

    // Box(value) constructor → heap allocation via __gorget_box_alloc
    if (name == "Box" || name.starts_with("Box__")) && args.len() == 1 {
        let mut val_op = lower_expr(ctx, builder, &args[0]);
        let raw_type = super::exprs::infer_operand_type_full(ctx, &val_op, builder);
        // Unwrap Ptr(T) → T: when the argument is a bare-borrowed resource
        // type (passed by pointer), Box should box the value, not the pointer.
        let val_type = match ctx.type_registry.get(raw_type) {
            Some(crate::ir::types::GirType::Ptr(inner)) | Some(crate::ir::types::GirType::MutPtr(inner)) => {
                let pointee = *inner;
                if let Operand::Copy(ref place) | Operand::Move(ref place) = val_op {
                    let derefed = builder.load_ref(place.clone(), pointee);
                    val_op = FunctionBuilder::copy(derefed);
                }
                pointee
            }
            _ => raw_type,
        };
        // Determine inner type name for the mangled Box type
        let inner_c = if let Some(rest) = name.strip_prefix("Box__") {
            // Already mangled (e.g., "Box__int64_t") — use the suffix directly
            rest.to_string()
        } else {
            ctx.type_name_for_id(val_type)
                .unwrap_or("int64_t")
                .to_string()
        };
        let box_mangled = format!("Box__{inner_c}");
        let box_type = if let Some(tid) = ctx.type_mapper.lookup_named(&box_mangled) {
            tid
        } else {
            let tid = ctx.type_registry.insert(crate::ir::types::GirType::Named(box_mangled.clone()));
            ctx.type_mapper.register_named(box_mangled.clone(), tid);
            super::exprs::ensure_box_type_def(ctx, &box_mangled, val_type);
            tid
        };
        // Emit: __gorget_box_alloc_T(value) → T* with heap alloc
        let alloc_fn = format!("__gorget_box_alloc_{inner_c}");
        // Tier 2c (snag #23 class): register this alloc fn as a
        // shallow-copy heap-allocating consumer so
        // `validate_drop_pre_rebind` recognises it via typed metadata
        // rather than name matching. See
        // `Module::heap_alloc_consumer_externs`.
        ctx.heap_alloc_consumer_externs.insert(alloc_fn.clone());
        let dst = builder.call_extern(&alloc_fn, vec![val_op], box_type);
        // Tier 2a Phase 2A: Box allocation returns a fresh heap
        // allocation. Tag FreshOwned so the consume-site validator
        // sees a sound `(FreshOwned, dead, _)` tuple at downstream
        // consumers (EnumInit / StructInit / Call args).
        if !ctx.drops.is_registered(dst) {
            ctx.drops.register_local(dst, box_type, &ctx.type_registry);
        }
        ctx.set_owned_fresh(builder, dst);
        return FunctionBuilder::copy(dst);
    }

    // WaitGroup() → gorget_waitgroup_new()
    if name == "WaitGroup" && args.is_empty() {
        let wg_type = ctx.type_mapper.lookup_named("WaitGroup").unwrap_or(I64_TYPE);
        let dst = builder.call_extern("gorget_waitgroup_new", vec![], wg_type);
        return FunctionBuilder::copy(dst);
    }

    // Semaphore(n) → gorget_semaphore_new(n)
    if name == "Semaphore" && args.len() == 1 {
        let n_op = lower_expr(ctx, builder, &args[0]);
        let s_type = ctx.type_mapper.lookup_named("Semaphore").unwrap_or(I64_TYPE);
        let dst = builder.call_extern("gorget_semaphore_new", vec![n_op], s_type);
        return FunctionBuilder::copy(dst);
    }

    // OnceFlag() → gorget_onceflag_new()
    if name == "OnceFlag" && args.is_empty() {
        let of_type = ctx.type_mapper.lookup_named("OnceFlag").unwrap_or(I64_TYPE);
        let dst = builder.call_extern("gorget_onceflag_new", vec![], of_type);
        return FunctionBuilder::copy(dst);
    }

    // Determine the effective type name (mangled if generic)
    let effective_name = if let Some(type_args) = generic_args {
        if !type_args.is_empty() {
            let mangled = super::types::mangle_generic_name(name, type_args);
            // Apply type name substitutions for generic monomorphization
            ctx.resolve_type_name(&mangled)
        } else {
            name.to_string()
        }
    } else {
        name.to_string()
    };

    // Intercept Channel__T constructor → Channel__T__new(cap) — capacity arg would be dropped
    // by generic struct init, so we route through a named constructor function.
    if effective_name.starts_with("Channel__") && args.len() == 1 {
        let cap = lower_expr(ctx, builder, &args[0]);
        let chan_type = get_or_register_type(ctx, &effective_name, Some(&|c| ensure_channel_type_def(c, &effective_name)));
        let new_fn = format!("{effective_name}__new");
        let dst = builder.call(&new_fn, vec![cap], chan_type);
        return FunctionBuilder::copy(dst);
    }

    // Intercept Shared__T constructor → gorget_shared_new(sizeof(T), &val) → GorgetShared*
    if (effective_name == "Shared" || effective_name.starts_with("Shared__")) && args.len() == 1 {
        let val_op = lower_expr(ctx, builder, &args[0]);
        let val_type = super::exprs::infer_operand_type_full(ctx, &val_op, builder);
        let inner_c = if let Some(rest) = effective_name.strip_prefix("Shared__") {
            rest.to_string()
        } else {
            ctx.c_type_name_for_id(val_type)
        };
        let shared_mangled = format!("Shared__{inner_c}");
        let vt = val_type;
        let shared_type = get_or_register_type(ctx, &shared_mangled, Some(&|c| ensure_shared_type_def(c, &shared_mangled, vt)));
        // Pack closure → GorgetClosure when the inner is a Callable alias.
        // See `pack_closure_for_smart_ptr_ctor` in calls.rs for rationale.
        let val_op = pack_closure_for_smart_ptr_ctor(ctx, builder, val_op, &inner_c);
        let new_fn = format!("{shared_mangled}__new");
        let dst = builder.call(&new_fn, vec![val_op.clone()], shared_type);
        // Shared[T](v) takes ownership of v's data via a shallow memcpy into the shared
        // block. If v is a Move-semantics local (e.g. Vector/GorgetArray), mark it as
        // moved so the drop elaborator emits a null-guarded DropIfAlive instead of an
        // unconditional free — otherwise the shared block would hold a dangling data pointer.
        if let Operand::Copy(place) = &val_op {
            if place.projections.is_empty() {
                if is_resource_type_local(place.local, builder, &ctx.type_registry) {
                    ctx.move_zero_and_mark(builder, place.local);
                }
            }
        }
        // SCOUT-PROTO #1b (Defect B): the fresh Shared handle owns a new
        // control block — tag it FreshOwned at birth (Core #3). Without this
        // the Untracked temp trips the consuming-position "clone conservatively
        // if Untracked" branch and gets a spurious incref (a fresh temp must
        // MOVE into its consumer, not clone).
        ctx.set_owned_fresh(builder, dst);
        return FunctionBuilder::copy(dst);
    }

    // Intercept Mutex__T constructor → gorget_mutex_new(sizeof(T), &val) → GorgetMutex*
    if (effective_name == "Mutex" || effective_name.starts_with("Mutex__")) && args.len() == 1 {
        let val_op = lower_expr(ctx, builder, &args[0]);
        let val_type = super::exprs::infer_operand_type_full(ctx, &val_op, builder);
        let inner_c = if let Some(rest) = effective_name.strip_prefix("Mutex__") {
            rest.to_string()
        } else {
            ctx.c_type_name_for_id(val_type)
        };
        let mutex_mangled = format!("Mutex__{inner_c}");
        let vt = val_type;
        let mutex_type = get_or_register_type(ctx, &mutex_mangled, Some(&|c| ensure_mutex_type_def(c, &mutex_mangled, vt)));
        let val_op = pack_closure_for_smart_ptr_ctor(ctx, builder, val_op, &inner_c);
        let new_fn = format!("{mutex_mangled}__new");
        let dst = builder.call(&new_fn, vec![val_op], mutex_type);
        return FunctionBuilder::copy(dst);
    }

    // AtomicInt(val) → gorget_atomic_int_new(val)
    if effective_name == "AtomicInt" && args.len() == 1 {
        let val_op = lower_expr(ctx, builder, &args[0]);
        let at_type = ctx.type_mapper.lookup_named("AtomicInt").unwrap_or(I64_TYPE);
        let dst = builder.call_extern("gorget_atomic_int_new", vec![val_op], at_type);
        return FunctionBuilder::copy(dst);
    }

    // AtomicBool(val) → gorget_atomic_bool_new(val)
    if effective_name == "AtomicBool" && args.len() == 1 {
        let val_op = lower_expr(ctx, builder, &args[0]);
        let at_type = ctx.type_mapper.lookup_named("AtomicBool").unwrap_or(BOOL_TYPE);
        let dst = builder.call_extern("gorget_atomic_bool_new", vec![val_op], at_type);
        return FunctionBuilder::copy(dst);
    }

    // Barrier(n) → gorget_barrier_new(n)
    if effective_name == "Barrier" && args.len() == 1 {
        let n_op = lower_expr(ctx, builder, &args[0]);
        let b_type = ctx.type_mapper.lookup_named("Barrier").unwrap_or(I64_TYPE);
        let dst = builder.call_extern("gorget_barrier_new", vec![n_op], b_type);
        return FunctionBuilder::copy(dst);
    }

    // CondVar() → gorget_condvar_new()
    if effective_name == "CondVar" && args.is_empty() {
        let cv_type = ctx.type_mapper.lookup_named("CondVar").unwrap_or(I64_TYPE);
        let dst = builder.call_extern("gorget_condvar_new", vec![], cv_type);
        return FunctionBuilder::copy(dst);
    }

    // RWLock[T](val) → RWLock__T__new(val) — follows the Mutex pattern
    if effective_name == "RWLock" || effective_name.starts_with("RWLock__") {
        if !args.is_empty() {
            let val_op = lower_expr(ctx, builder, &args[0]);
            let val_type = super::exprs::infer_operand_type_full(ctx, &val_op, builder);
            let inner_c = if let Some(rest) = effective_name.strip_prefix("RWLock__") {
                rest.to_string()
            } else {
                ctx.c_type_name_for_id(val_type)
            };
            let rw_mangled = format!("RWLock__{inner_c}");
            let rw_type = get_or_register_type(ctx, &rw_mangled, None);
            let val_op = pack_closure_for_smart_ptr_ctor(ctx, builder, val_op, &inner_c);
            let new_fn = format!("{rw_mangled}__new");
            let dst = builder.call(&new_fn, vec![val_op], rw_type);
            return FunctionBuilder::copy(dst);
        }
    }

    // Intercept TaskGroup.new() static constructor
    if effective_name == "TaskGroup" && args.is_empty() {
        let tg_mangled = "TaskGroup";
        let tg_type = get_or_register_type(ctx, tg_mangled, Some(&|c| ensure_task_group_type_def(c, tg_mangled)));
        let dst = builder.call("gorget_task_group_new", vec![], tg_type);
        return FunctionBuilder::copy(dst);
    }

    // Check if this is an Option/Result variant constructor — resolve with type-aware logic
    // to avoid ambiguity when multiple monomorphized types share variant names.
    if let Some(result) = resolve_option_result_variant(ctx, builder, name, args) {
        return result;
    }

    // Check if this is an enum variant constructor.
    // SSOT: honour the typechecker-determined expected type to disambiguate
    // same-named variants across enums (e.g. Type.TArray vs CRuntimeType.TArray).
    if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant_typed(&effective_name, ctx.func_state.expected_type) {
        let arg_spans: Vec<Option<crate::span::Span>> = args.iter()
            .map(|arg| Some(arg.span))
            .collect();
        // Look up variant field types so each arg sees the correct expected_type;
        // this gates auto-propagation (the lookup skips when the field itself is Result).
        let variant_field_types: Vec<Option<TypeId>> = ctx.type_registry
            .get_type_def(&enum_name)
            .and_then(|td| match &td.kind {
                crate::ir::types::TypeDefKind::Enum(ed) => Some(ed),
                _ => None,
            })
            .and_then(|ed| ed.variants.iter().find(|v| v.name == variant_name))
            .map(|v| v.fields.iter().map(|f| Some(f.type_id)).collect())
            .unwrap_or_else(|| vec![None; args.len()]);
        let mut field_operands: Vec<Operand> = args.iter()
            .enumerate()
            .map(|(i, arg)| {
                let prev = ctx.func_state.expected_type;
                if let Some(ft) = variant_field_types.get(i).and_then(|f| *f) {
                    ctx.func_state.expected_type = Some(ft);
                }
                let op = lower_expr(ctx, builder, arg);
                // Snag #46: auto-propagate Result→T at the variant-field boundary.
                let op = maybe_auto_propagate(ctx, builder, op, arg.span);
                ctx.func_state.expected_type = prev;
                op
            })
            .collect();
        // Clone multi-use resource args (loop-carried locals, field accesses, etc.)
        // that can't be safely moved into the enum variant.
        clone_multi_use_resource_args(ctx, builder, &mut field_operands, args);
        let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
        let dst = ctx.emit_enum_init_owned(builder, &enum_name, &variant_name, type_id, field_operands, Some(arg_spans));
        return FunctionBuilder::copy(dst);
    }
    // Also check the base name for non-generic enum variants.
    // SSOT: type-aware to disambiguate same-named variants across enums.
    if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant_typed(name, ctx.func_state.expected_type) {
        let arg_spans: Vec<Option<crate::span::Span>> = args.iter()
            .map(|arg| Some(arg.span))
            .collect();
        let variant_field_types: Vec<Option<TypeId>> = ctx.type_registry
            .get_type_def(&enum_name)
            .and_then(|td| match &td.kind {
                crate::ir::types::TypeDefKind::Enum(ed) => Some(ed),
                _ => None,
            })
            .and_then(|ed| ed.variants.iter().find(|v| v.name == variant_name))
            .map(|v| v.fields.iter().map(|f| Some(f.type_id)).collect())
            .unwrap_or_else(|| vec![None; args.len()]);
        let mut field_operands: Vec<Operand> = args.iter()
            .enumerate()
            .map(|(i, arg)| {
                let prev = ctx.func_state.expected_type;
                if let Some(ft) = variant_field_types.get(i).and_then(|f| *f) {
                    ctx.func_state.expected_type = Some(ft);
                }
                let op = lower_expr(ctx, builder, arg);
                // Snag #46: auto-propagate Result→T at the variant-field boundary.
                let op = maybe_auto_propagate(ctx, builder, op, arg.span);
                ctx.func_state.expected_type = prev;
                op
            })
            .collect();
        // Clone multi-use resource args for enum variant init.
        clone_multi_use_resource_args(ctx, builder, &mut field_operands, args);
        let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
        let dst = ctx.emit_enum_init_owned(builder, &enum_name, &variant_name, type_id, field_operands, Some(arg_spans));
        return FunctionBuilder::copy(dst);
    }

    // Regular struct literal — set expected_type per field so empty array literals
    // get the correct element size (e.g., Vector[LargeStruct] initialized as []).
    let field_types: Vec<Option<TypeId>> = if let Some(td) = ctx.type_registry.get_type_def(&effective_name) {
        if let crate::ir::types::TypeDefKind::Struct(ref sd) = td.kind {
            sd.fields.iter().map(|f| Some(f.type_id)).collect()
        } else { vec![] }
    } else { vec![] };
    let mut field_operands: Vec<Operand> = args.iter().enumerate()
        .map(|(i, arg)| {
            let prev = ctx.func_state.expected_type;
            if let Some(&Some(ft)) = field_types.get(i) {
                ctx.func_state.expected_type = Some(ft);
            }
            let op = lower_expr(ctx, builder, arg);
            // Snag #46: a throws-fn call result at a struct-field position is
            // `Result[T, E]`, but the field expects `T`. Mirror `lower_call_arg`
            // — auto-propagate Result → T at the boundary so the field receives
            // the unwrapped value rather than a memcpy of the Result struct
            // (which the field then reads as the type's zero-init default).
            let op = maybe_auto_propagate(ctx, builder, op, arg.span);
            ctx.func_state.expected_type = prev;
            op
        })
        .collect();

    // Ownership boundary: struct fields need independently owned values.
    // `ensure_owned_at_boundary` clones Ptr(T) borrows, ref-state locals, and
    // by-value resource borrows. Owned locals pass through unchanged — the
    // last-use auto-move of single-use sources runs below via
    // `move_zero_consumed_args`.
    //
    // EXCEPT for borrow fields (`Ref[T]` / `MutRef[T]`): the field IS a Ptr,
    // so a Ptr-typed source operand is exactly what we want stored. Cloning
    // would dereference and copy the pointee (and then take the address of
    // a stack-local — see Phase 1.7 dangling-pointer issue), but the field
    // semantics are "alias the source, don't copy." Skip the boundary for
    // Ptr-typed fields so the operand passes through unchanged.
    for (i, op) in field_operands.iter_mut().enumerate() {
        let field_is_ptr = matches!(
            field_types.get(i).and_then(|f| f.as_ref()).and_then(|tid| ctx.type_registry.get(*tid)),
            Some(crate::ir::types::GirType::Ptr(_) | crate::ir::types::GirType::MutPtr(_))
        );
        if field_is_ptr {
            continue;
        }
        // Explicit move (`!arg`) at field position: caller transferred
        // ownership. If the lowered operand is a Ptr-typed temp (e.g. self
        // is `!self` lowered to a MutPtr at the body level), deref to get
        // the pointee value and store that into the field. Skip the clone
        // — the source's MoveZero (already scheduled by `Expr::Move`)
        // ensures no double-drop.
        //
        // ONLY safe when the source genuinely owns its pointee. The shape
        // we accept here is `is_owning_param` (a `!`-sigil resource param,
        // whose `Expr::Move` lowering MoveZero's the param slot — making
        // the recipient's shallow copy the sole owner). For non-owning
        // borrows (Field, regular bare param, CowBorrow), the post-move
        // zero only blanks the 8-byte Ptr local — the source struct
        // field / collection element / parameter cell still owns the
        // bytes, so a `load_ref` here would alias them and double-free
        // at scope exit. Fall through to `ensure_owned_at_boundary`,
        // which clones the pointee into a fresh owned value.
        if matches!(args.get(i).map(|a| &a.node), Some(Expr::Move { .. })) {
            let move_info = match &*op {
                Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
                    let local_type = builder.local_type(place.local);
                    let lidx = place.local.0 as usize;
                    let source_owns = lidx < builder.locals.len() && builder.locals[lidx].is_owning_param;
                    match ctx.type_registry.get(local_type) {
                        Some(crate::ir::types::GirType::Ptr(inner)) | Some(crate::ir::types::GirType::MutPtr(inner))
                            if source_owns => Some((place.clone(), *inner)),
                        _ => None,
                    }
                }
                _ => None,
            };
            if let Some((place, pointee_ty)) = move_info {
                let loaded = builder.load_ref(place, pointee_ty);
                *op = FunctionBuilder::copy(loaded);
                continue;
            }
        }
        let span = args.get(i).map(|a| a.span).unwrap_or(crate::span::Span { start: 0, end: 0 });
        *op = ctx.ensure_owned_at_boundary(
            builder, std::mem::replace(op, Operand::Constant(Constant::Unit)),
            span, crate::ir::ImplicitCloneReason::StructFieldFromBorrow,
        );
    }

    // Clone by-value resource args that can't be moved (multi-use, field
    // access, loop-carried, string view, bare param) BEFORE struct init.
    // Complements the preceding `ensure_owned_at_boundary` loop which handled
    // Ptr(T) and ref-state borrow cases.
    clone_multi_use_resource_args(ctx, builder, &mut field_operands, args);

    let type_id = ctx.type_mapper.lookup_named(&effective_name).unwrap_or(UNIT_TYPE);
    let dst = builder.struct_init(&effective_name, type_id, field_operands.clone());
    ctx.set_owned(builder, dst);

    // MoveZero owned single-use/temp sources AFTER struct init so they don't
    // double-free on scope exit (the struct now owns the data).
    move_zero_consumed_args(ctx, builder, &field_operands);

    FunctionBuilder::copy(dst)
}

/// Lower a field access expression.
fn lower_field_access(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    field_name: &str,
) -> Operand {
    // Check for qualified enum variant without parens: Color.Red
    // If the object is a type name (not a local) and the field is a variant, emit EnumInit.
    if let Expr::Identifier(name) = &object.node {
        // Snag #29 mirror of methods.rs site: type-position priority over
        // module_constants. Without this, `enum E` shadowed by hardcoded
        // `E = 2.718…` constant turned `E.A` into a field access on a
        // float local. See full justification at the methods.rs call.
        let is_type_name = ctx.type_mapper.lookup_named(name).is_some()
            || ctx.resolve_enum_variant(name).is_some();
        let is_local = ctx.lookup_local(name).is_some();
        if !is_local && (is_type_name || !ctx.module_constants.contains_key(name)) {
            if let Some(type_id) = ctx.type_mapper.lookup_named(name) {
                if let Some(type_def) = ctx.type_registry.get_type_def(name) {
                    if let TypeDefKind::Enum(ref e) = type_def.kind {
                        if e.variants.iter().any(|v| v.name == field_name) {
                            let dst = builder.enum_init(name, field_name, type_id, vec![]);
                            return FunctionBuilder::copy(dst);
                        }
                    }
                }
            }
        }
    }

    // For unique-borrow params (& or !), use the pointer directly
    // so field access goes through the pointer (*ptr).field instead of copying
    let obj = if let Expr::Identifier(name) = &object.node {
        if let Some((local_id, _)) = ctx.lookup_local(name) {
            if ctx.is_param_borrow_unique(builder, local_id) {
                Operand::Copy(Place::local(local_id))
            } else {
                lower_expr(ctx, builder, object)
            }
        } else if let Some(global_ptr) = materialize_global_field_base(ctx, builder, object) {
            // Bug #1: a module-level static base lowers to Operand::Constant(GlobalRef)
            // which the place-guard below would skip → returns Constant::Unit and
            // silently drops the read. Materialize the global into an addressable
            // MutPtr local so the pointer-deref field path below fires.
            global_ptr
        } else {
            lower_expr(ctx, builder, object)
        }
    } else {
        lower_expr(ctx, builder, object)
    };

    if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
        let local_idx = place.local.0 as usize;
        if local_idx < builder.locals.len() {
            let local_type_id = builder.locals[local_idx].type_id;

            // Guard[T] auto-deref: guard.field → (*get_ptr(&guard)).field.
            // `guard_of` peels Ptr/MutPtr, so a guard reached through a `&`/`!`
            // param takes this same branch (READS are legal through every guard
            // kind, ReadGuard included — only WRITES discriminate).
            {
                if let Some(info) = guard_of(ctx, local_type_id) {
                    let (inner_ptr_local, inner_type) = emit_guard_get_ptr(ctx, builder, place, &info);
                    let deref_place = Place {
                        local: inner_ptr_local,
                        projections: vec![Projection::Deref],
                    };
                    if let Some(inner_type_name) = ctx.type_name_for_id(inner_type) {
                        let inner_type_name = inner_type_name.to_string();
                        if let Some((field_idx, field_type)) = ctx.lookup_field(&inner_type_name, field_name) {
                            // Resource-type fields → Ptr(T) reference (same as non-Guard path).
                            let result_type = if field_read_yields_ptr(ctx, field_type) {
                                ctx.type_registry.insert(GirType::Ptr(field_type))
                            } else {
                                field_type
                            };
                            let base_local = deref_place.local;
                            let dst = builder.field_load(deref_place, field_idx, result_type);
                            if matches!(ctx.type_registry.get(result_type), Some(GirType::Ptr(_))) {
                                ctx.set_field_or_elem_borrow(builder, dst, base_local, field_idx);
                            }
                            return FunctionBuilder::copy(dst);
                        }
                        if let Some(type_def) = ctx.type_registry.get_type_def(&inner_type_name) {
                            if let TypeDefKind::Struct(ref s) = type_def.kind {
                                for (i, field) in s.fields.iter().enumerate() {
                                    if field.name == field_name {
                                        let result_type = if field_read_yields_ptr(ctx, field.type_id) {
                                            ctx.type_registry.insert(GirType::Ptr(field.type_id))
                                        } else {
                                            field.type_id
                                        };
                                        let base_local = deref_place.local;
                                        let dst = builder.field_load(deref_place, i as u32, result_type);
                                        if matches!(ctx.type_registry.get(result_type), Some(GirType::Ptr(_))) {
                                            ctx.set_field_or_elem_borrow(builder, dst, base_local, i as u32);
                                        }
                                        return FunctionBuilder::copy(dst);
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // If the local is a raw pointer (e.g., self in equip methods), dereference it
            // to get the underlying struct type for field access.
            // Box[T] types use explicit `*box` dereference in Gorget, handled by Expr::Deref.
            // `base_is_ptr` was historically read at the resource-field Ptr-wrap predicate,
            // retired by the Phase C FieldLoad migration (2026-05-06): owned-base resource
            // user-struct fields ALSO need Ptr-wrapping to prevent shallow-copy double-free.
            let (effective_type_id, base_place, _base_is_ptr) =
                if let Some(pointee) = ctx.pointee_type(local_type_id) {
                    // Pointer type: add Deref projection → (*_N).field
                    let mut deref_place = place.clone();
                    deref_place.projections.push(Projection::Deref);
                    (pointee, deref_place, true)
                } else {
                    (local_type_id, place.clone(), false)
                };

            // Detect consuming self (!self) field access: self.field returns
            // owned values via MoveZeroSource instead of Ptr borrows.
            let is_consuming_self_access = ctx.func_state.consuming_self
                && (matches!(&object.node, Expr::Identifier(n) if n == "self")
                    || matches!(&object.node, Expr::SelfExpr));

            // Look up the type name, then the field info
            if let Some(type_name) = ctx.type_name_for_id(effective_type_id) {
                // First try the struct_fields cache
                if let Some((field_idx, field_type)) = ctx.lookup_field(type_name, field_name) {
                    // !self consuming access: resource fields are moved out (owned),
                    // not borrowed. The source field is zeroed, and the struct's
                    // drop function handles cleanup of any unconsumed fields.
                    if is_consuming_self_access
                        && field_read_yields_ptr(ctx, field_type)
                    {
                        let dst = builder.field_load(base_place.clone(), field_idx, field_type);
                        // Zero the source field to prevent double-free when
                        // the struct is dropped at scope exit.
                        builder.move_zero(Place {
                            local: base_place.local,
                            projections: {
                                let mut p = base_place.projections.clone();
                                p.push(Projection::Field(field_idx));
                                p
                            },
                        });
                        ctx.set_owned(builder, dst);
                        ctx.drops.register_local(dst, field_type, &ctx.type_registry);
                        return FunctionBuilder::copy(dst);
                    }
                    // Resource-type fields: return a Ptr(T) reference instead of
                    // a shallow copy. Prevents shared heap buffer double-free.
                    // Auto-clone fires when assigned to an explicit-type variable
                    // or passed to a function that takes ownership.
                    //
                    // Phase C FieldLoad migration (2026-05-06): drop the
                    // `base_is_ptr &&` guard — owned-base struct field reads of
                    // resource user-structs (PeerId, UdpAddr, Token, Expr,
                    // etc.) also need Ptr-wrapping, otherwise the load is a
                    // shallow alias of the field's heap data and a drop of
                    // either side double-frees. `is_resource_type` already
                    // covers GorgetString and collections, but the explicit
                    // `is_collection_type` / `owned_string_type` cases stay
                    // for self-documentation and to keep the predicate stable
                    // if `is_resource_type` ever narrows.
                    let result_type = if field_read_yields_ptr(ctx, field_type) {
                        ctx.type_registry.insert(GirType::Ptr(field_type))
                    } else {
                        field_type
                    };
                    let base_local = base_place.local;
                    let dst = builder.field_load(base_place.clone(), field_idx, result_type);
                    if matches!(ctx.type_registry.get(result_type), Some(GirType::Ptr(_))) {
                        ctx.set_field_or_elem_borrow(builder, dst, base_local, field_idx);
                    }
                    return FunctionBuilder::copy(dst);
                }
                // Fallback: read directly from TypeDef
                // Extract field info first to avoid borrow conflict with ctx
                let field_info: Option<(u32, TypeId)> = ctx.type_registry.get_type_def(type_name)
                    .and_then(|td| if let TypeDefKind::Struct(ref s) = td.kind {
                        s.fields.iter().enumerate()
                            .find(|(_, f)| f.name == field_name)
                            .map(|(i, f)| (i as u32, f.type_id))
                    } else { None });
                if let Some((field_idx, field_type)) = field_info {
                    // !self consuming access (fallback path)
                    if is_consuming_self_access
                        && field_read_yields_ptr(ctx, field_type)
                    {
                        let dst = builder.field_load(base_place.clone(), field_idx, field_type);
                        builder.move_zero(Place {
                            local: base_place.local,
                            projections: {
                                let mut p = base_place.projections.clone();
                                p.push(Projection::Field(field_idx));
                                p
                            },
                        });
                        ctx.set_owned(builder, dst);
                        ctx.drops.register_local(dst, field_type, &ctx.type_registry);
                        return FunctionBuilder::copy(dst);
                    }
                    // Same FieldLoad migration as above — drop `base_is_ptr &&`.
                    let result_type = if field_read_yields_ptr(ctx, field_type) {
                        ctx.type_registry.insert(GirType::Ptr(field_type))
                    } else {
                        field_type
                    };
                    let base_local = base_place.local;
                    let dst = builder.field_load(base_place.clone(), field_idx, result_type);
                    if matches!(ctx.type_registry.get(result_type), Some(GirType::Ptr(_))) {
                        ctx.set_field_or_elem_borrow(builder, dst, base_local, field_idx);
                    }
                    return FunctionBuilder::copy(dst);
                }
            }
        }
    }

    // Fallback: can't resolve field
    Operand::Constant(Constant::Unit)
}

/// Auto-deref a Ptr-typed non-string iterable to a value borrow, mirroring the
/// legacy collection deref that both the statement-for loop and the list
/// comprehension need. When the lowered iterable is `Ptr(Coll)` (a `&coll`
/// borrow or a borrowed `Vector[T]` param), the downstream `.Field(2)` len-read
/// and `IndexLoad(iter_local, …)` element-load assume `iter_local` is the VALUE
/// collection, not the pointer — without the deref they read adjacent slots
/// (an off-by-one panic in the for-loop, 0 iterations → an EMPTY result in the
/// comprehension). Strings keep the Ptr: `lower_for_string` /
/// `lower_string_comprehension` detect a Ptr-typed source themselves.
///
/// This is the SINGLE iterable-deref site (campaign "one shared iterable-mode
/// helper, not two parallel fixes"): `lower_for` (`stmts/for_loops.rs`) and
/// `lower_list_comprehension` (`exprs/collections.rs`) both call it, so the
/// comprehension can never again drift from the for-loop's deref handling.
pub(in crate::ir::lowering) fn deref_ptr_collection_iterable(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    iter_op: Operand,
    iter_type: crate::ir::types::TypeId,
) -> (Operand, crate::ir::types::TypeId) {
    let pointee = ctx.pointee_type(iter_type);
    let pointee_is_string = pointee.map_or(false, |p| ctx.type_mapper.is_string_type(p));
    if pointee.is_some() && !pointee_is_string {
        // Non-string Ptr iterable: deref to a value borrow.
        if let (Operand::Copy(p) | Operand::Move(p), Some(inner)) = (&iter_op, pointee) {
            let deref_place = Place {
                local: p.local,
                projections: vec![Projection::Deref],
            };
            let tmp = builder.add_local(inner, None);
            builder.assign_mode(
                AssignMode::Borrow,
                Place::local(tmp),
                Operand::Copy(deref_place),
            );
            (Operand::Copy(Place::local(tmp)), inner)
        } else {
            (iter_op, iter_type)
        }
    } else {
        // String case OR non-Ptr iterable — keep iter_op as-is.
        (iter_op, pointee.unwrap_or(iter_type))
    }
}

/// G1 PROTOTYPE (r33 materialize scout): walk a projection chain
/// (`v[i]`, `v.f`, `v[i].f[j]`) to its base identifier and return that
/// local. Returns None when the base is not a simple named local (a call
/// result, `self`, a static). Used at projected-mutation sites to find the
/// immutable-in-context root that must materialize before the write.
pub(super) fn resolve_projection_root_local(
    ctx: &mut LoweringContext,
    expr: &Expr,
) -> Option<crate::ir::types::LocalId> {
    match expr {
        Expr::Identifier(name) => ctx.lookup_local(name).map(|(l, _)| l),
        // 2E (D2): plain `self` is a bare param like any other — a write
        // rooted at `self` must reach `cow_before_mutation` so the existing
        // bare-Ptr-param case materializes a private copy (`&self` roots are
        // Unique borrows, a no-op there — write-through preserved).
        Expr::SelfExpr => ctx.lookup_local("self").map(|(l, _)| l),
        Expr::Index { object, .. }
        | Expr::FieldAccess { object, .. }
        | Expr::TupleFieldAccess { object, .. } => {
            resolve_projection_root_local(ctx, &object.node)
        }
        // Descend a builtin-collection element-getter chain to the collection's
        // root. `c.get(i)`/`.first()`/`.last()` return an in-place element BORROW
        // (auto-borrow-from-get, e0d5a554); `.unwrap()`/`.expect()` peel the
        // `Option` over it. A mutation projected through the returned handle
        // (`f.blocks.get(0).unwrap().term = 99`) therefore reaches the collection
        // root, which — for a bare/owned root — must materialize a private copy
        // (the ratified rule, ledger 783c9817): the caller stays unchanged, like
        // the sibling `f.blocks[i].term = x` / `f.blocks.push(..)` stores.
        //
        // Gated on the RECEIVER's collection-KIND ∈ {Array (Vector/Deque),
        // OrderedMap (Dict)} — exactly the field-write-addressable builtin kinds
        // (the `try_resolve_field_place` set; EXCLUDE Set/HashMap, not
        // field-write-addressable). A USER `get`/`first`/`last` (non-collection
        // receiver) returns an OWNED temp whose root is the temp, NOT the
        // receiver — descending it would materialize a struct the write never
        // touches, a wasted clone == CoW-charter breach. The safety pass's
        // dead-write lint mirrors this precise descent
        // (`find_get_chain_taint_root`, safety/helpers.rs) so the "write is dead"
        // warning fires exactly for these materializing stores. (The 2T tainted-
        // materialize REJECT for get-chains is a filed cross-lane follow-up —
        // ggdef has the same descent gap; see TODO.md.)
        Expr::MethodCall { receiver, method, .. } => {
            let descend = match method.node.as_str() {
                // `c.get(i)`/`.first()`/`.last()` — the receiver IS the collection.
                "get" | "first" | "last" => is_field_addressable_collection(ctx, receiver),
                // `c.get(i).unwrap()` — the receiver is the getter; its receiver
                // is the collection. A plain `Option.unwrap()` (Identifier
                // receiver) or a USER-get `.unwrap()` is NOT descended.
                "unwrap" | "expect" => match &receiver.node {
                    Expr::MethodCall { receiver: inner, method: inner_m, .. }
                        if matches!(inner_m.node.as_str(), "get" | "first" | "last") =>
                    {
                        is_field_addressable_collection(ctx, inner)
                    }
                    _ => false,
                },
                _ => false,
            };
            if descend {
                resolve_projection_root_local(ctx, &receiver.node)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// True if `recv`'s resolved type is a builtin Array (Vector/Deque) or
/// OrderedMap (Dict) — the field-write-addressable builtin collection kinds
/// whose `.get()`/`.first()`/`.last()` return an in-place element borrow
/// (Set/HashMap excluded: their elements aren't field-write-addressable).
/// Resolved TYPE-ONLY (no lowering), the exact mechanism the `Expr::Index`
/// write-through pre-check uses (`index_base_kind_type_only`) so the getter
/// chain and the sibling index store classify a receiver identically.
fn is_field_addressable_collection(ctx: &mut LoweringContext, recv: &Spanned<Expr>) -> bool {
    matches!(
        index_base_kind_type_only(ctx, recv),
        Some(crate::ir::types::CollectionKind::Array)
            | Some(crate::ir::types::CollectionKind::OrderedMap)
    )
}

/// True if a projection chain contains an `Expr::Index` anywhere on its spine
/// (`v[i]`, `v[i].f`, `m[i][j].f`, `s.f[k].g`). Used at the method-call
/// field-receiver site to decide whether `try_resolve_field_place`'s
/// `Expr::Index` arm could have minted a transient CollectionElement handle that
/// must be untracked before a later same-collection mutation (Core #4 sibling of
/// lower_field_assign's hoisted CoW untrack).
pub(super) fn expr_projection_contains_index(expr: &Expr) -> bool {
    match expr {
        Expr::Index { .. } => true,
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            expr_projection_contains_index(&object.node)
        }
        _ => false,
    }
}

/// Extract a dot-separated field path string from a field-access expression.
/// Returns `Some("self.data")` for `FieldAccess { SelfExpr, "data" }`,
/// `Some("game.entities")` for `FieldAccess { Identifier("game"), "entities" }`,
/// `Some("self.game.entities")` for nested chains.
pub(super) fn extract_field_path_string(expr: &Expr) -> Option<String> {
    match expr {
        Expr::FieldAccess { object, field } => {
            let prefix = match &object.node {
                Expr::SelfExpr => Some("self".to_string()),
                Expr::Identifier(name) => Some(name.clone()),
                Expr::FieldAccess { .. } => extract_field_path_string(&object.node),
                _ => None,
            };
            prefix.map(|p| format!("{}.{}", p, field.node))
        }
        _ => None,
    }
}

/// If `object` is a bare identifier naming a module-level `static`, materialize
/// the global into a fresh addressable `MutPtr(<struct>)` local and return an
/// `Operand::Copy` of that pointer local.
///
/// Bug #1 (static-struct field access returns garbage): `Place` roots only at a
/// LOCAL (`src/ir/instructions.rs`), so a static base has no `Place` to project
/// into — field reads degrade to `const unit`/0 and field stores are silently
/// dropped. The fix makes the static addressable: emit `&NAME` via
/// `Constant::GlobalRefPtr` (→ `Inst::GlobalAddr`, a real `*mut <T>` pointer,
/// `src/lir/lower/operands.rs`) into a fresh local typed `MutPtr(<struct>)`, then
/// let the EXISTING pointer-deref field path (`pointee_type` → `Projection::Deref`)
/// project through it. Resource fields still get the `Ptr(field)` borrow wrap, so
/// `B.items.push()` borrows in place.
///
/// This MIRRORS the shipped index-load precedent (`lower_index_access`,
/// `methods.rs`, 2026-06-04) but DIVERGES deliberately: index-load uses
/// `Borrow`/`Copy` of a value local; field access uses `MutPtr`+`Deref` because
/// the STORE path (`P.x = 99`) must write THROUGH to the global, not to a copy.
/// The local is typed via `register_mut_ptr_type` — NOT `GlobalRefPtr`'s type
/// inference (`type_reg.rs`), which returns the BASE type, not `MutPtr(base)`.
///
/// Returns `None` for non-globals (the caller proceeds with its existing path).
pub(super) fn materialize_global_field_base(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
) -> Option<Operand> {
    let Expr::Identifier(name) = &object.node else {
        return None;
    };
    // A local of the same name shadows the global; only materialize true globals.
    if ctx.lookup_local(name).is_some() || !ctx.global_names.contains(name.as_str()) {
        return None;
    }
    // Look up the global's struct type so the pointer local is typed
    // `MutPtr(<struct>)` — the pointee type drives the existing Deref field path.
    let base_type = ctx
        .global_type_names
        .get(name)
        .and_then(|tn| lookup_global_type(ctx, tn))?;
    let ptr_type = ctx.register_mut_ptr_type(base_type);
    let ptr_local = builder.add_local(ptr_type, None);
    builder.assign(
        Place::local(ptr_local),
        Operand::Constant(Constant::GlobalRefPtr(name.clone())),
    );
    Some(Operand::Copy(Place::local(ptr_local)))
}

/// THE field-read Ptr-wrap predicate — one accessor, one source of truth
/// (devbook/24 rule 3).
///
/// Answers: does a READ of a field of this type yield a `Ptr(T)` reference INTO
/// the parent rather than a shallow value copy? True for collections, owned
/// `String`s, and resource user-structs — the shallow copy would alias the
/// field's heap data and a drop of either side would double-free.
///
/// ⚠ This predicate was open-coded SIX times in this file, which is how the
/// `&`-argument path came to disagree with itself: the four types this returns
/// `true` for were the four whose `&`-of-a-projection write-through worked BY
/// ACCIDENT (the read handed back a pointer, so `emit_borrow_mut` addressed real
/// storage), while the six it returns `false` for silently lost the callee's
/// write. That accident is no longer load-bearing — the `&`-formation faces now
/// resolve a PLACE through `try_resolve_place` instead of inferring one from
/// this predicate's output — but the predicate itself still governs read
/// semantics, so it gets one home rather than six.
///
/// The explicit `is_collection_type` / `owned_string_type` arms are redundant
/// with `is_resource_type` today and are KEPT deliberately: they document the
/// intent and keep the predicate stable if `is_resource_type` ever narrows.
pub(in crate::ir::lowering) fn field_read_yields_ptr(ctx: &LoweringContext, field_type: TypeId) -> bool {
    ctx.type_registry.is_collection_type(field_type)
        || field_type == ctx.type_mapper.owned_string_type
        || ctx.type_registry.is_resource_type(field_type)
}

/// Type-only resolution of a PURE place expression to its `TypeId`, WITHOUT
/// lowering it — no IR is emitted, so a side-effecting base is not evaluated.
///
/// # `None` means UNKNOWN, and callers must treat it as such
///
/// This is the contract that matters, because two consumers key SAFETY on it:
///
/// * `lower_call_arg`'s `safe_to_skip_auto_propagate` — decides whether the
///   Family-1 chokepoint may return early and skip `maybe_auto_propagate`. It
///   matches `None => false` (DECLINE) explicitly. An earlier revision computed
///   the inverse with `.unwrap_or(false)` and branched on the negation, so
///   `None` meant SKIP; a `Result`-typed argument bound for a non-`Result`
///   parameter then had its `Error` silently swallowed and the callee received a
///   pointer to a `Result`. Never return a GUESS from this function: a plausible
///   wrong type reads as a definite answer and no downstream fail-safe can
///   distinguish it from a real one. That is why the `TupleFieldAccess` arm does
///   its lookup inline instead of calling `resolve_tuple_field_type`, whose miss
///   returns `I64_TYPE`.
/// * `index_base_kind_type_only` — the collection-kind pre-check gating
///   `try_resolve_index_element_ptr`, so a side-effecting collection producer
///   (`make()[k].field = x`) is resolved without being lowered twice.
///
/// # `None` is memory-safe but NOT free
///
/// Declining the early return falls back to the READ path, and for a by-value
/// projection that path loads a value copy and loses the callee's write — the
/// Family-1 defect itself. So an unmodelled form costs a LOST WRITE on that
/// shape, not merely a missed optimisation. Widening this function is a fix.
///
/// # Not quite side-effect-free
///
/// The previous doc claimed "no side effects". Not strictly true: the `Index`
/// arm reaches `infer_collection_element_type`, which INTERNS a `FnPtr` type for
/// `Callable` elements. Idempotent and benign, but it now runs on every `&` call
/// argument, and the claim was unenforced — stated accurately rather than
/// repeated.
pub(in crate::ir::lowering) fn place_expr_type_only(
    ctx: &mut LoweringContext,
    expr: &Spanned<Expr>,
) -> Option<TypeId> {
    match &expr.node {
        Expr::Identifier(name) => {
            if let Some((_, tid)) = ctx.lookup_local(name) {
                Some(tid)
            } else {
                let tn = ctx.global_type_names.get(name).cloned()?;
                lookup_global_type(ctx, &tn)
            }
        }
        Expr::SelfExpr => ctx.lookup_local("self").map(|(_, t)| t),
        Expr::FieldAccess { object, field } => {
            let obj_t = place_expr_type_only(ctx, object)?;
            let resolved = ctx.pointee_type(obj_t).unwrap_or(obj_t);
            let name = ctx.type_name_for_id(resolved)?.to_string();
            if let Some((_, ft)) = ctx.lookup_field(&name, &field.node) {
                return Some(ft);
            }
            // GUARD AUTO-DEREF — `g.f` where `g: Guard[T]` and `f` lives on the
            // GUARDED value, not on the wrapper. `try_resolve_field_place`
            // projects through the guard's inner pointer, so this function must
            // be able to TYPE that same projection or `&g.f` never reaches the
            // Family-1 chokepoint and falls back to the read path — which loads a
            // value COPY of the field and borrows the dying temp, silently losing
            // the callee's write while `g.f = v` works. That is Family-1's own
            // class, at the one object form whose resolution is TYPE-driven
            // rather than a syntactic `Expr::` arm.
            //
            // ⚠ Mirrors the producer's branch and must keep mirroring it,
            // including the `ReadGuard` early-out: a read-only guard resolves no
            // WRITE place there, so typing one here would send `&rg.f` down the
            // chokepoint for a place the producer will refuse.
            let info = guard_of(ctx, resolved)?;
            if info.is_read_only() {
                return None;
            }
            let inner_name = ctx.type_name_for_id(info.inner_type)?.to_string();
            ctx.lookup_field(&inner_name, &field.node).map(|(_, ft)| ft)
        }
        // ⚠ THIS ARM IS LOAD-BEARING FOR CORRECTNESS, not just for coverage.
        // `place_expr_type_only`'s expression domain MUST be a superset of
        // `try_resolve_place`'s, because the Family-1 chokepoint asks THIS
        // function whether an argument would auto-propagate and then, if the
        // answer is "no", lets the PRODUCER resolve it and returns early —
        // skipping `maybe_auto_propagate`. A form the producer resolves but this
        // function returns `None` for therefore gets the early return with the
        // auto-propagate question never asked.
        //
        // MEASURED when this arm was missing: `void take(int &x)` called as
        // `take(&t.0)` on a `(Result[int,int], int)` tuple seeded `Error(mk())`
        // printed `in take` / `1` / `ok` — the `Error` SWALLOWED, the callee
        // handed a pointer to a `Result` where an `int` was expected, printing
        // the tag word and then writing THROUGH it. Base correctly propagates.
        // Identical on both backends, `gg check` clean. The struct-field twin
        // (`&h.r`) was fixed first and this one shipped broken: an instance fix
        // where a class existed (Core #4). The arm-superset lint
        // `place_type_only_covers_the_producer_forms` now guards the pairing.
        Expr::TupleFieldAccess { object, index } => {
            let obj_t = place_expr_type_only(ctx, object)?;
            let resolved = ctx.pointee_type(obj_t).unwrap_or(obj_t);
            // ⚠ DELIBERATELY NOT `resolve_tuple_field_type` HERE, even though it
            // is the obvious helper. That function falls back to `I64_TYPE` when
            // the lookup misses (`exprs/type_reg.rs`) — a WRONG type rather than
            // an unknown one. This function's contract is "the type, or `None`",
            // and the call site's fail-safe can only protect against `None`: a
            // bogus `I64_TYPE` reads as "not a propagating `Result`", so the
            // chokepoint would skip `maybe_auto_propagate` on a form it never
            // actually typed. Doing the lookup here and returning `None` on a
            // miss keeps `None` meaning UNKNOWN. (The fallback stays for
            // `resolve_tuple_field_type`'s other callers, which want a type
            // unconditionally; it is filed as a latent silent-disagreement
            // source.)
            let type_name = ctx.type_name_for_id(resolved)?;
            let type_def = ctx.type_registry.get_type_def(type_name)?;
            match &type_def.kind {
                TypeDefKind::Struct(s) => s.fields.get(*index).map(|f| f.type_id),
                _ => None,
            }
        }
        Expr::Index { object, .. } => {
            let coll_t = place_expr_type_only(ctx, object)?;
            Some(infer_collection_element_type(ctx, coll_t))
        }
        // `(*b).f` / `(*b)[i]` — resolve through the pointee.
        //
        // ⚠ THIS ARM EXISTS FOR COVERAGE, NOT FOR SAFETY, and the distinction
        // matters because an earlier revision had it backwards. A `None` from
        // this function does NOT mean "be conservative": the call site treats an
        // unknown form as *provably safe to skip* unless it is written to do
        // otherwise. That inversion shipped the same swallowed-`Error`
        // miscompile three times. The fail-safe now lives at the call site
        // (`lower_call_arg`'s `safe_to_skip_auto_propagate` matches on `None`
        // explicitly); this arm's job is to keep `Deref`-object projections on
        // the fast path, not to prevent a miscompile.
        Expr::Deref { expr: inner } => {
            let inner_t = place_expr_type_only(ctx, inner)?;
            ctx.deref_inner_type(inner_t)
        }
        // ⚠ NOT EXHAUSTIVE. A `None` here is MEMORY-SAFE by construction — the
        // call site's `None => false` declines the early return — but it is NOT
        // free, and an earlier revision of this comment said it was.
        //
        // 🚨 DECLINING THE EARLY RETURN MEANS FALLING BACK TO THE READ PATH, AND
        // THE READ PATH IS THE FAMILY-1 BUG. The early return IS the fix: it
        // borrows the PLACE. Fall back and a by-value projection loads a value
        // COPY and the callee's write is silently lost. So an unmodelled form
        // costs a LOST WRITE, not "the early-return optimisation". The old
        // wording ("each costs the early-return optimisation on that shape and
        // nothing else") named `g.f` specifically as harmless — and `&g.a` was
        // measurably dropping its write at the time, while `g.a = v` worked.
        // That is Family-1's own signature. Corrected rather than swapped
        // (Core #14).
        //
        // Forms still absent: method-chain objects (`&v.get(i).unwrap().f`,
        // filed as its own gap and NOT closed by this function). Adding an arm
        // here is a real fix, not a tidy-up — measure a write-through probe
        // before assuming otherwise.
        _ => None,
    }
}

/// The `CollectionKind` of the base of an `Expr::Index`, resolved TYPE-ONLY
/// (no lowering). `None` when the base type can't be resolved without lowering
/// (a side-effecting producer) or isn't a builtin collection.
fn index_base_kind_type_only(
    ctx: &mut LoweringContext,
    coll: &Spanned<Expr>,
) -> Option<CollectionKind> {
    let tid = place_expr_type_only(ctx, coll)?;
    let resolved = ctx.pointee_type(tid).unwrap_or(tid);
    let name = ctx.type_name_for_id(resolved)?.to_string();
    ctx.type_registry
        .get_type_def(&name)
        .and_then(|td| td.metadata.collection_kind)
}

/// Resolve `coll[idx]` to a write-through element *pointer* local + the element
/// type. Shared producer for every consumer that needs an in-buffer element place
/// (field-store via `try_resolve_field_place`'s Index arm; mut method receiver
/// `v[i].bump()` via the method-call self-arg builder).
///
/// Forces `Ptr(elem)` from `index_load` for BOTH value-type and resource-type
/// elements — `lower_index_access` returns a value COPY for value elements, so a
/// plain read would make subsequent mutations land on a throwaway. The returned
/// Place is a bare local holding `Ptr(elem)` (no projections); callers that need
/// the element value place append `Projection::Deref`.
///
/// TYPE-ONLY pre-check (`index_base_kind_type_only`) gates admission before any
/// lowering so a side-effecting collection producer is evaluated at most once.
/// Only `Array` (Vector/Deque) and `OrderedMap` (Dict) are addressable today —
/// Set/HashMap/user Index return `None` without lowering (see the Index arm of
/// `try_resolve_field_place` for the HashMap exclusion rationale). Module-level
/// `static` bases (GlobalRef) materialize into an addressable local (Borrow for
/// resource collections, Copy for value).
pub(super) fn try_resolve_index_element_ptr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    coll: &Spanned<Expr>,
    index: &Spanned<Expr>,
) -> Option<(Place, TypeId)> {
    let kind = index_base_kind_type_only(ctx, coll);
    let is_addressable = matches!(
        kind,
        Some(crate::ir::types::CollectionKind::Array)
            | Some(crate::ir::types::CollectionKind::OrderedMap)
    );
    if !is_addressable {
        return None;
    }
    let mut coll_obj = lower_expr(ctx, builder, coll);
    // Materialize a `static` base (GlobalRef) into an addressable local:
    // a resource-typed collection Borrows (zero-cost header aliasing the
    // global's heap buffer, so the element `Ptr` writes THROUGH to the
    // global); a value-typed base Copies.
    if let Operand::Constant(Constant::GlobalRef(_)) = coll_obj {
        let base_ty = infer_operand_type_full(ctx, &coll_obj, builder);
        let local = builder.add_local(base_ty, None);
        let mode = if ctx.type_registry.is_resource_type(base_ty) {
            crate::ir::instructions::AssignMode::Borrow
        } else {
            crate::ir::instructions::AssignMode::Copy
        };
        builder.assign_mode(mode, Place::local(local), coll_obj);
        coll_obj = Operand::Copy(Place::local(local));
    }
    let coll_place = match &coll_obj {
        Operand::Copy(p) | Operand::Move(p) => p.clone(),
        _ => return None,
    };
    let base_ty = infer_operand_type_full(ctx, &coll_obj, builder);
    let elem_type = infer_collection_element_type(ctx, base_ty);
    let idx = lower_expr(ctx, builder, index);
    let elem_ptr_type = ctx.register_ptr_type(elem_type);
    let elem_ptr = builder.index_load(coll_place, idx, elem_ptr_type);
    Some((Place::local(elem_ptr), elem_type))
}

/// THE shared place resolver — the SINGLE entry point mapping a projection
/// expression to a write-through `Place` plus the type of the value AT that
/// place. Both `&`-borrow FORMATION faces dispatch here (`lower_call_arg`'s
/// `MutableBorrow` arm and the standalone `Expr::MutableBorrow` handler); the
/// three specialist resolvers below are the grammar's productions, not entry
/// points.
///
/// # Postcondition 1 — the returned `Place` is an lvalue OF THE VALUE
///
/// Never of a pointer to it. `try_resolve_index_element_ptr` hands back a bare
/// local holding `Ptr(elem)`, so the `Projection::Deref` normalisation happens
/// HERE, once, at the producer — rather than every caller re-deriving "is this
/// one already a pointer?" from the shape it got back. That re-derivation is
/// exactly how the `&`-argument path grew its `is_already_ptr` special case, and
/// a caller that forgets it emits `**T`, so the callee reads pointer bits as
/// payload (gorget-js snag #1). With this postcondition every caller may
/// `emit_borrow_mut` the result UNCONDITIONALLY and get a pointer to real
/// storage. That invariant is what makes one producer safe to share.
///
/// # Postcondition 2 — EMISSION: a `None` may arrive AFTER instructions emitted
///
/// The specialist resolvers can emit and THEN return `None`. On that path the
/// caller falls through to `lower_expr` and the base is evaluated a SECOND time.
/// This is a surface the Family-1 chokepoint CREATES at the `&`-formation faces
/// (before it, those faces had only the fall-through path, so the question never
/// arose). TOTAL enumeration of the `return None` sites, one row each:
///
/// | site | emits before returning? | reachable from an `&`-formation face? |
/// |---|---|---|
/// | `try_resolve_index_element_ptr` kind-gate | no (type-only pre-check) | yes — `Set`/`HashMap`/user-`Index` bases |
/// | `try_resolve_index_element_ptr` non-place coll | YES (`lower_expr(coll)`) | yes |
/// | `try_resolve_field_place` Identifier-not-a-local | no | yes |
/// | `try_resolve_field_place` SelfExpr-not-bound | no | no (checker rejects) |
/// | `try_resolve_field_place` nested-FieldAccess recursion | inherits the inner row | yes |
/// | `try_resolve_field_place` Deref non-place | YES (`lower_expr(inner)`) | yes |
/// | `try_resolve_field_place` Index recursion (`?`) | inherits the index rows | yes |
/// | `try_resolve_field_place` head `_ =>` | no | yes — method chains, temps |
/// | `try_resolve_field_place` ReadGuard early-out | only if the OBJECT arm emitted | yes, when combined with an emitting base |
/// | `try_resolve_field_place` field-lookup fall-off | only if the OBJECT arm emitted | yes — see the `Deque` row below |
/// | `try_resolve_tuple_field_place` Identifier / SelfExpr / recursions / Deref / head `_ =>` | mirrors the field resolver row-for-row | yes |
/// | `try_resolve_tuple_field_place` non-place obj / out-of-range local / walk fall-off | only if the OBJECT arm emitted | yes |
///
/// A REACHABLE emit-then-`None` row exists and is measured: a `Deque[S]` element
/// base. `Deque` is ADMITTED by the kind-gate (`builtins.rs` gives it
/// `CollectionKind::Array`) but `infer_collection_element_type` has no `Deque__`
/// arm, so the element type falls to `I64_TYPE`, the field walk finds nothing,
/// and control falls off the field resolver AFTER `lower_expr(coll)`,
/// `lower_expr(index)` and `index_load` have all emitted. The double evaluation
/// is ACCEPTED here rather than remedied: the remedy ("never emit before you can
/// return `None`") edits the SHARED resolvers, which would change assign-face
/// and method-receiver emission on their `None` paths too — a semantic change on
/// faces this chokepoint deliberately leaves byte-identical. The `Deque` shape
/// is filed with a committed `known_gaps` repro; see
/// `known_gaps/sound_amp_deque_element_field.gg`.
///
/// # `None` means FALL THROUGH, never DROP
///
/// Returns `None` for anything that is not a resolvable place — a whole
/// identifier, a temp, a call result, a literal, an out-of-domain collection
/// kind. The caller must fall through to its existing behaviour; `None` must
/// never mean "discard the construct" (Core #10).
///
/// # Parity with the ASSIGN face is held by the GUARD, not by shared code
///
/// `lower_assign` deliberately does NOT route through this producer:
/// `lower_index_assign` must be able to INSERT a missing Dict key, which a write
/// through a resolved element pointer cannot do, and the field/tuple-field
/// assign paths differ in their materialize / untrack / clone prologues. The two
/// faces are kept in parity by `tests/lints.rs`'s
/// `amp_formation_and_assign_cover_the_same_place_forms`, NOT by calling the
/// same code. Do not assume otherwise.
pub(in crate::ir::lowering) fn try_resolve_place(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    expr: &Spanned<Expr>,
) -> Option<(Place, TypeId)> {
    let resolved = match &expr.node {
        Expr::FieldAccess { object, field } => {
            try_resolve_field_place(ctx, builder, object, &field.node)
        }
        Expr::TupleFieldAccess { object, index } => {
            try_resolve_tuple_field_place(ctx, builder, object, *index)
        }
        Expr::Index { object, index } => {
            let (ptr_place, elem_type) = try_resolve_index_element_ptr(ctx, builder, object, index)?;
            // Postcondition 1: the index resolver returns a local holding
            // `Ptr(elem)`; normalise it to an lvalue OF THE ELEMENT here, once.
            let mut place = ptr_place;
            place.projections.push(Projection::Deref);
            Some((place, elem_type))
        }
        // Snag #26 lvalue-through-deref, hoisted out of the TWO open-coded
        // `&`-formation copies (`lower_call_arg`'s `&*b` block and the standalone
        // `Expr::MutableBorrow` `&*b` block) into this shared producer, so `&*b`
        // and `&b.fd` take ONE path instead of two.
        //
        // ⚠ A THIRD copy remains open-coded in `lower_assign`'s `Expr::Deref` arm
        // (`stmts/assigns.rs`), deliberately. Absorbing it is NOT a move: that
        // copy walks the inner place's projections to compute the pointee type
        // (peeling `Field` through struct definitions as well as `Deref`), where
        // this arm reads the local's type directly. They agree on every measured
        // shape, but reconciling them changes emission for multi-projection deref
        // targets, so it owes its own fixtures and lane census. Filed, not fixed.
        Expr::Deref { expr: inner } => {
            let inner_op = lower_expr(ctx, builder, inner);
            let inner_place = match &inner_op {
                Operand::Copy(p) | Operand::Move(p) => p.clone(),
                _ => return None,
            };
            let local_idx = inner_place.local.0 as usize;
            let pointee = if local_idx < builder.locals.len() {
                let t = builder.local_type(inner_place.local);
                ctx.deref_inner_type(t).unwrap_or(t)
            } else {
                UNIT_TYPE
            };
            let mut place = inner_place;
            place.projections.push(Projection::Deref);
            Some((place, pointee))
        }
        // Fall through — NOT a drop (Core #10). A bare `Expr::Identifier` lands
        // here BY DESIGN: `&x` on a whole local is served by the bare-identifier
        // fast paths that run BEFORE this producer at both faces.
        _ => None,
    };

    // 🚨 POSTCONDITION 1, ENFORCED — never hand back a place whose VALUE is
    // ITSELF A POINTER.
    //
    // A field can be DECLARED as a pointer: `Ref[T]` / `MutRef[T]` struct fields
    // (`struct Holder: Ref[Vector[int]] vec`) and extern `T*` fields. The place
    // resolvers resolve those perfectly well — the place is the field slot — but
    // the VALUE living there is already a `*T`. A caller that then
    // `emit_borrow_mut`s it gets `**T`, and the callee reads the pointer's bits
    // as payload: the gorget-js snag #1 shape.
    //
    // MEASURED, and this guard exists because of it: `push_it(&h.vec)` on a
    // `Ref[Vector[int]]` field printed `3 / 4 / 3` before the Family-1
    // chokepoint and SIGSEGV'd (exit 139) with the chokepoint but WITHOUT this
    // guard. Returning `None` falls the shape through to `lower_call_arg`'s
    // surviving `is_already_ptr` fast-path, which FORWARDS the stored pointer
    // instead of taking its address.
    //
    // ⚠ SCOPE OF THAT EVIDENCE, stated honestly. An earlier revision cited "the
    // projection matrix proves that fall-through path right for all ten types".
    // That citation does not support this guard: post-fix those ten cells are
    // RESOLVED and return early, so they never reach the fall-through at all.
    // The already-a-pointer cell is sampled at exactly ONE pointee type
    // (`Vector[int]`, in `cow_amp_ref_field_forward.gg`) — plus the `MutRef`
    // variant of the same pointee, which is what reds the fall-through guard
    // itself. Other pointee types (extern `T*` fields, `Ref[<user struct>]`)
    // are UNSAMPLED here.
    //
    // This is what makes postcondition 1 literally true rather than aspirational:
    // "the returned Place is an lvalue OF THE VALUE, never of a pointer to it"
    // now holds by construction, so `emit_borrow_mut` really is unconditional at
    // every caller. Pinned by `cow_amp_ref_field_forward.gg`.
    if let Some((_, place_type)) = resolved {
        if matches!(
            ctx.type_registry.get(place_type),
            Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))
        ) {
            return None;
        }
    }
    resolved
}

/// Resolve a field access expression to a Place (with projections) and the field's type,
/// WITHOUT copying the field to a temp. This allows borrowing the field in-place.
/// Returns `Some((place, field_type_id))` if the expression is a resolvable field access.
pub(super) fn try_resolve_field_place(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    field_name: &str,
) -> Option<(Place, TypeId)> {
    // Lower the object expression to get its local
    let obj = match &object.node {
        Expr::Identifier(name) => {
            if let Some((local_id, _)) = ctx.lookup_local(name) {
                Operand::Copy(Place::local(local_id))
            } else if let Some(global_ptr) =
                materialize_global_field_base(ctx, builder, object)
            {
                // Bug #1: a module-level static base — materialize into an
                // addressable MutPtr local; the place walk below appends Deref.
                global_ptr
            } else {
                return None;
            }
        }
        Expr::SelfExpr => {
            if let Some((local_id, _)) = ctx.lookup_local("self") {
                Operand::Copy(Place::local(local_id))
            } else {
                return None;
            }
        }
        // Recursive case: chained field access (e.g., o.nested.items)
        Expr::FieldAccess { object: inner_obj, field: inner_field } => {
            if let Some((inner_place, _inner_type)) = try_resolve_field_place(ctx, builder, inner_obj, &inner_field.node) {
                Operand::Copy(inner_place)
            } else {
                return None;
            }
        }
        // Snag #26: lvalue through deref. `(*box).field = val` and
        // `(*ptr).field = val` need a write-through-pointer Place, not a
        // copy-of-deref. Recurse into the inner expression and append
        // Deref to its projections so the field-store at the call site
        // writes to the heap (Box) or pointee (Ptr) rather than a stack
        // temp.
        Expr::Deref { expr: inner } => {
            let inner_op = lower_expr(ctx, builder, inner);
            if let Operand::Copy(ref inner_place) | Operand::Move(ref inner_place) = inner_op {
                let mut deref_place = inner_place.clone();
                deref_place.projections.push(Projection::Deref);
                Operand::Copy(deref_place)
            } else {
                return None;
            }
        }
        // `v[i].field = x` / `PTS[i].field = x` / `d[k].field = x`: resolve the
        // Index ELEMENT to a WRITE-THROUGH pointer so the field-store lands in
        // the collection's heap buffer, not on a stack copy. Shared producer
        // `try_resolve_index_element_ptr` (forces `Ptr(elem)` for value AND
        // resource elements; type-only pre-check; Array|OrderedMap only;
        // GlobalRef → local). The pointer-deref path below (`pointee_type` →
        // `Projection::Deref` + field) projects the field through it. Caller's
        // root `cow_before_mutation` already materialized a private copy for a
        // shared local, so the pointer aliases the owned buffer.
        Expr::Index { object: coll, index } => {
            let (elem_ptr_place, _elem_type) =
                try_resolve_index_element_ptr(ctx, builder, coll, index)?;
            Operand::Copy(elem_ptr_place)
        }
        _ => return None,
    };

    if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
        let local_idx = place.local.0 as usize;
        if local_idx < builder.locals.len() {
            let mut current_type = builder.locals[local_idx].type_id;

            // Walk existing projections to find the effective type at the end
            for proj in &place.projections {
                match proj {
                    Projection::Deref => {
                        // deref_inner_type covers both raw Ptr/MutPtr and
                        // Box[T] (a Named struct with a single `_0` field).
                        // Snag #26: pointee_type alone returns None for Box,
                        // so the walk left current_type as Box__T and field
                        // lookup for the user-visible field name failed,
                        // sending the assignment down the value-copy path.
                        if let Some(pointee) = ctx.deref_inner_type(current_type) {
                            current_type = pointee;
                        }
                    }
                    Projection::Field(idx) => {
                        if let Some(tn) = ctx.type_name_for_id(current_type) {
                            if let Some(type_def) = ctx.type_registry.get_type_def(tn) {
                                if let TypeDefKind::Struct(ref s) = type_def.kind {
                                    if (*idx as usize) < s.fields.len() {
                                        current_type = s.fields[*idx as usize].type_id;
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }

            // Guard[T] auto-deref for writes: guard.field → (*get_ptr(&guard)).field.
            // The guard wraps a pointer to the guarded value; field access goes
            // THROUGH the inner pointer (docs/devbook/11-copy-on-write.md, Guard
            // single-owner carve-out). Centralized here so EVERY field-place
            // consumer projects through the guard identically. ⚠ That consumer set
            // is NOT confined to stmts/assigns.rs (an earlier version of this
            // comment said so, and also named `lower_assign`, which does not call
            // this): re-derive it with
            // `grep -rn 'try_resolve_field_place(' src/ir/lowering/` — at time of
            // writing 8 sites across stmts/assigns.rs, exprs/methods.rs (the
            // mutating-method receiver) and this file. Uses the SAME emit_guard_get_ptr
            // helper as the read path (`lower_field_access`) and the plain-assign
            // path (`lower_field_assign`, stmts/assigns.rs).
            //
            // Detection is the TYPED `TypeMapper::guard_types` channel via
            // `guard_of` (which also peels `Ptr`/`MutPtr`, so a guard behind a
            // `&`/`!` param resolves here too — the axis three of the four read
            // sites silently ignored while the guard branch was gated on a
            // name-prefix test on the OUTER TypeId's name). Layering rule 2
            // discharged: the channel is written at the ONE `register_named`
            // funnel and read via `guard_of`; the same `emit_guard_get_ptr`
            // helper drives the read path (`lower_field_access`) and the
            // plain-assign path (`resolve_ptr_field_place`, stmts/assigns.rs).
            //
            // RESIDUAL — user-generic `Guard[T]` collision: a USER generic
            // named `Guard[T]` still mangles to `Guard__…` and is treated as
            // the builtin (`known_gaps/fieldaccess_user_generic_guard_collision.gg`,
            // filed in TODO.md). The unblocker is module-qualifying
            // `GenericCollector::struct_templates` (bare-name-keyed), NOT
            // widening the funnel's prefix list.
            {
                if let Some(info) = guard_of(ctx, current_type) {
                    if info.is_read_only() {
                        // ReadGuard: writes are forbidden — don't resolve a write
                        // place (type checker should reject in future). Returning
                        // None matches the plain-assign guard arm's early-out.
                        return None;
                    }
                    let (inner_ptr_local, inner_type) = emit_guard_get_ptr(ctx, builder, place, &info);
                    let deref_place = Place {
                        local: inner_ptr_local,
                        projections: vec![Projection::Deref],
                    };
                    if let Some(inner_type_name) = ctx.type_name_for_id(inner_type) {
                        let inner_type_name = inner_type_name.to_string();
                        if let Some((field_idx, field_type)) = ctx.lookup_field(&inner_type_name, field_name) {
                            let mut target_place = deref_place;
                            target_place.projections.push(Projection::Field(field_idx));
                            return Some((target_place, field_type));
                        }
                        if let Some(type_def) = ctx.type_registry.get_type_def(&inner_type_name) {
                            if let TypeDefKind::Struct(ref s) = type_def.kind {
                                for (i, field) in s.fields.iter().enumerate() {
                                    if field.name == field_name {
                                        let mut target_place = deref_place;
                                        target_place.projections.push(Projection::Field(i as u32));
                                        return Some((target_place, field.type_id));
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // If the resolved type is a pointer, dereference it
            let (effective_type_id, mut base_place) =
                if let Some(pointee) = ctx.pointee_type(current_type) {
                    let mut deref_place = place.clone();
                    deref_place.projections.push(Projection::Deref);
                    (pointee, deref_place)
                } else {
                    (current_type, place.clone())
                };

            // Look up the field
            if let Some(type_name) = ctx.type_name_for_id(effective_type_id) {
                if let Some((field_idx, field_type)) = ctx.lookup_field(type_name, field_name) {
                    base_place.projections.push(Projection::Field(field_idx));
                    return Some((base_place, field_type));
                }
                if let Some(type_def) = ctx.type_registry.get_type_def(type_name) {
                    if let TypeDefKind::Struct(ref s) = type_def.kind {
                        for (i, field) in s.fields.iter().enumerate() {
                            if field.name == field_name {
                                base_place.projections.push(Projection::Field(i as u32));
                                return Some((base_place, field.type_id));
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

/// Resolve `object.INDEX` (a TUPLE-field lvalue) to a write-through `Place` +
/// element type — the numeric-index sibling of `try_resolve_field_place`. A
/// tuple is a `TypeDefKind::Struct` with positionally-indexed fields, so the
/// target place is `object_place` + `Projection::Field(index)` — the SAME shape
/// a struct-field store uses, just resolved by position instead of a name lookup
/// (mirrors the tuple-field READ path, `lower_expr`'s `Expr::TupleFieldAccess`
/// arm which does `field_load(place, index, …)`). The object is resolved via the
/// same recursion `try_resolve_field_place` uses (Identifier local / static base
/// / SelfExpr / nested struct-field via `try_resolve_field_place` / nested
/// tuple-field via this fn / `*ptr` deref), then the effective type is walked and
/// a pointer base is deref'd — so `t.0 = v` on a local, a `&`-param, and a
/// nested `s.tup.0 = v` / `a.0.field = v` all write THROUGH. Returns `None` for a
/// non-place object (a fresh temp / method-call result) so the caller can fall
/// through to a graceful reject rather than dropping the write (Core #10).
/// Shared by plain `=` (`lower_assign`) and compound `OP=`
/// (`lower_compound_assign`).
pub(super) fn try_resolve_tuple_field_place(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    index: usize,
) -> Option<(Place, TypeId)> {
    // Resolve the object to a base place (mirror try_resolve_field_place's
    // object arm — reuse it for struct-field objects, recurse here for
    // tuple-field objects).
    let obj = match &object.node {
        Expr::Identifier(name) => {
            if let Some((local_id, _)) = ctx.lookup_local(name) {
                Operand::Copy(Place::local(local_id))
            } else if let Some(global_ptr) = materialize_global_field_base(ctx, builder, object) {
                global_ptr
            } else {
                return None;
            }
        }
        Expr::SelfExpr => {
            if let Some((local_id, _)) = ctx.lookup_local("self") {
                Operand::Copy(Place::local(local_id))
            } else {
                return None;
            }
        }
        Expr::FieldAccess { object: inner_obj, field } => {
            let (inner_place, _) = try_resolve_field_place(ctx, builder, inner_obj, &field.node)?;
            Operand::Copy(inner_place)
        }
        Expr::TupleFieldAccess { object: inner_obj, index: inner_index } => {
            let (inner_place, _) = try_resolve_tuple_field_place(ctx, builder, inner_obj, *inner_index)?;
            Operand::Copy(inner_place)
        }
        Expr::Deref { expr: inner } => {
            let inner_op = lower_expr(ctx, builder, inner);
            if let Operand::Copy(ref p) | Operand::Move(ref p) = inner_op {
                let mut dp = p.clone();
                dp.projections.push(Projection::Deref);
                Operand::Copy(dp)
            } else {
                return None;
            }
        }
        _ => return None,
    };

    let place = match obj {
        Operand::Copy(ref p) | Operand::Move(ref p) => p.clone(),
        _ => return None,
    };
    let local_idx = place.local.0 as usize;
    if local_idx >= builder.locals.len() {
        return None;
    }
    // Walk existing projections to the effective type (mirror
    // try_resolve_field_place: Deref peels a pointer, Field descends a struct).
    let mut current_type = builder.locals[local_idx].type_id;
    for proj in &place.projections {
        match proj {
            Projection::Deref => {
                if let Some(pointee) = ctx.deref_inner_type(current_type) {
                    current_type = pointee;
                }
            }
            Projection::Field(idx) => {
                if let Some(tn) = ctx.type_name_for_id(current_type) {
                    if let Some(td) = ctx.type_registry.get_type_def(tn) {
                        if let TypeDefKind::Struct(ref s) = td.kind {
                            if (*idx as usize) < s.fields.len() {
                                current_type = s.fields[*idx as usize].type_id;
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    // Deref if the effective type is a pointer (& param / Box / element Ptr).
    let (tuple_type_id, mut base_place) = if let Some(pointee) = ctx.pointee_type(current_type) {
        let mut dp = place.clone();
        dp.projections.push(Projection::Deref);
        (pointee, dp)
    } else {
        (current_type, place.clone())
    };
    let elem_type = resolve_tuple_field_type(ctx, tuple_type_id, index);
    base_place.projections.push(Projection::Field(index as u32));
    Some((base_place, elem_type))
}

/// Convert an index expression to a mangle fragment for generic type name construction.
/// e.g. `SparseSet[Health].new()` → receiver is `Index { object: "SparseSet", index: "Health" }`
/// Returns `Some("Health")` for `Identifier("Health")` or `Some("int64_t")` for `Identifier("int")`.
pub(super) fn index_expr_to_mangle_fragment(expr: &Expr) -> Option<String> {
    if let Expr::Identifier(name) = expr {
        let fragment = match name.as_str() {
            "int" => "int64_t",
            "float" => "double",
            "bool" => "bool",
            "str" | "String" => "GorgetString",
            "char" => "char",
            "byte" | "uint8" => "uint8_t",
            "uint16" => "uint16_t",
            "uint32" => "uint32_t",
            "uint64" => "uint64_t",
            "int8" => "int8_t",
            "int16" => "int16_t",
            "int32" => "int32_t",
            other => other,
        };
        Some(fragment.to_string())
    } else {
        None
    }
}

/// Lower a method call on a concrete (non-trait-object) type.
/// Lower an if expression (used as ternary).
fn lower_if_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
    then_branch: &Spanned<Expr>,
    elif_branches: &[(Spanned<Expr>, Spanned<Expr>)],
    else_branch: Option<&Spanned<Expr>>,
) -> Operand {
    let cond = lower_expr(ctx, builder, condition);

    // Allocate result local — we use I64_TYPE initially, then retroactively fix
    // the type after lowering the then-branch so the C backend sees the correct type.
    let result_id = builder.add_local(I64_TYPE, None);

    let then_bb = builder.new_block();
    let else_bb = builder.new_block();
    let merge_bb = builder.new_block();

    builder.branch(cond, then_bb, else_bb);

    // Then branch
    builder.switch_to(then_bb);
    let then_val = lower_expr(ctx, builder, then_branch);
    // Fix the result local's type to match the actual then-branch type
    let result_type = infer_operand_type_full(ctx, &then_val, builder);
    if result_type != I64_TYPE {
        builder.set_local_type(result_id, result_type);
    }
    assign_match_arm_to_result(ctx, builder, result_id, then_val, then_branch.span);
    builder.jump(merge_bb);

    // Elif branches — chain as nested if-else in the else block
    let mut current_else_bb = else_bb;
    for (elif_cond, elif_body) in elif_branches {
        builder.switch_to(current_else_bb);
        let elif_cond_val = lower_expr(ctx, builder, elif_cond);
        let elif_then_bb = builder.new_block();
        let next_else_bb = builder.new_block();
        builder.branch(elif_cond_val, elif_then_bb, next_else_bb);

        builder.switch_to(elif_then_bb);
        let elif_val = lower_expr(ctx, builder, elif_body);
        assign_match_arm_to_result(ctx, builder, result_id, elif_val, elif_body.span);
        builder.jump(merge_bb);

        current_else_bb = next_else_bb;
    }

    // Final else branch
    builder.switch_to(current_else_bb);
    if let Some(else_expr) = else_branch {
        let else_val = lower_expr(ctx, builder, else_expr);
        assign_match_arm_to_result(ctx, builder, result_id, else_val, else_expr.span);
    } else {
        builder.assign(Place::local(result_id), Operand::Constant(Constant::Unit));
    }
    builder.jump(merge_bb);

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_id)
}


/// Resolve the inner TypeId from a type name (e.g., "int64_t" → I64_TYPE).
pub fn resolve_none_tag(ctx: &LoweringContext, type_id: TypeId) -> i32 {
    if let Some(GirType::Named(name)) = ctx.type_registry.get(type_id) {
        if let Some(type_def) = ctx.type_registry.get_type_def(name) {
            if let TypeDefKind::Enum(ref e) = type_def.kind {
                // Find "None" variant by name, or fall back to last variant
                for (i, v) in e.variants.iter().enumerate() {
                    if v.name == "None" {
                        return i as i32;
                    }
                }
                return (e.variants.len() - 1) as i32;
            }
        }
    }
    1 // Default None tag for Option
}

/// Lower a call argument, respecting ownership (MutableBorrow creates a BorrowMut).
///
/// `callee_param_type` is the callee's declared parameter type from fn_sigs.
/// When the callee has a resource-type param, it's passed by pointer (const Ptr for bare,
/// MutPtr for &). We use the callee's param type (not the caller's local type) to decide,
/// avoiding mismatches like passing String to a function taking str.

/// Single-source-of-truth for "arm value crosses the match-result boundary"
/// (Snag #28; consume-site discipline, structural-guards.md Tier 2a).
///
/// Three semantic gates fire here, in order:
/// 1. **Borrow → owned clone** (`ensure_owned_at_boundary`): when the arm
///    value is a Ptr<T> binding (e.g., a variant binding bound as Ref because
///    the scrutinee was a Ptr) and the result slot expects owned T, clone
///    the pointee. Without this, `[Mv] result = copy ptr` memcpys the
///    pointee struct (a resource like GorgetString) into the result and
///    both alias the same heap data — double-free at scope exit.
/// 2. **Move-mode assign** (Phase C): the arm produces a fresh single-use
///    value flowing into the result slot — Move avoids a shallow-copy
///    flagging from the resource-moves validator.
/// 3. **Move follow-through** (drop-registered source → `move_zero_and_mark`):
///    transfers ownership so the source slot logically dies and scope-exit
///    drops don't double-free the heap allocation that now lives in the
///    result slot.
///
/// Used by both the per-arm site and the else-arm site of `lower_match_expr`
/// — encoding the boundary discipline in one place so the two structural
/// branches can't drift.
/// Materialise a `None` literal into a properly-tagged Option struct when
/// `expected_type` is a known `Option[T]`. Falls back to `Constant::Null` only
/// when there's no usable expected type — the Assign handler on `VarDecl` /
/// `StoreSlot` rewrites the null into a tagged struct downstream for that
/// case. Used by both `Expr::NoneLiteral` and `Expr::Call { callee:
/// NoneLiteral }` (the parenthesised `None()` form) so the two surface forms
/// can't disagree.
///
/// NOTE: the `name.starts_with("Option__")` test is the conventional GIR-layer
/// shape for Option-detection at this boundary — the IR layer doesn't yet
/// carry the `EnumKind::Option` typed flag that LIR has. Switching this to a
/// typed flag is filed under the layering audit.
fn materialise_none_for_expected_type(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
) -> Operand {
    if let Some(expected) = ctx.func_state.expected_type {
        if let Some(name) = ctx.type_registry.type_name(expected) {
            if name.starts_with("Option__") && !name.starts_with("Option__Ref__") {
                let inner = ctx.type_registry
                    .get_type_def(&name)
                    .and_then(|td| match &td.kind {
                        crate::ir::types::TypeDefKind::Enum(e) => {
                            e.variants.iter().find(|v| v.name == "Some")
                                .and_then(|v| v.fields.first().map(|f| f.type_id))
                        }
                        _ => None,
                    });
                if let Some(inner_type) = inner {
                    ctx.ensure_option_type_registered(&name, inner_type);
                    let dst = builder.enum_init(&name, "None", expected, vec![]);
                    return FunctionBuilder::copy(dst);
                }
            }
        }
    }
    Operand::Constant(Constant::Null)
}

fn assign_match_arm_to_result(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    result_local: LocalId,
    arm_val: Operand,
    span: crate::span::Span,
) {
    let arm_val = ctx.ensure_owned_at_boundary(
        builder,
        arm_val,
        span,
        crate::ir::ImplicitCloneReason::ConsumingArg,
    );
    let move_source: Option<LocalId> = match &arm_val {
        Operand::Copy(p) | Operand::Move(p)
            if p.projections.is_empty() && ctx.drops.is_registered(p.local) =>
            Some(p.local),
        _ => None,
    };
    builder.assign_mode(
        crate::ir::instructions::AssignMode::Move,
        Place::local(result_local),
        arm_val,
    );
    if let Some(src) = move_source {
        ctx.move_zero_and_mark(builder, src);
    }
    // Snag #31 (2026-05-10): tag result_local as Owned after the
    // Move-mode assign. Without this, the merge-bb's `[Mv] user_var =
    // copy result_local` shape (e.g. `Completion c = match … : case
    // Ok(x): !x …`) sees result_local as Untracked → AssignIntoOwnedSlot
    // validator fires on what the CoW spec considers a sound
    // `!arg → Move → tagged Owned dst` chain. The Move-mode assign IS
    // the writer-side commitment that result_local owns the data; the
    // typed tag must follow.
    ctx.set_owned(builder, result_local);
}

fn lower_match_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    scrutinee: &Spanned<Expr>,
    arms: &[ast::MatchArm],
    else_arm: Option<&Spanned<Expr>>,
) -> Operand {
    // Lower scrutinee to a temp local. Phase C: stage with the right
    // AssignMode by source shape — see stage_match_scrutinee.
    //
    // Snag #48: when the scrutinee is a `throws`-fn call (or any
    // expression yielding a `Result[T, E]`) inside a `throws E`
    // context, the operand is `Result[T, E]` at the IR layer. The
    // match patterns are written against T (the unwrapped variant
    // shape), so without auto-propagation the pattern condition /
    // extraction reads Result's layout as if it were T — variant
    // payloads come out as zero/discriminant garbage. Mirror the
    // call-arg path: auto-propagate before staging so the scrutinee
    // is the unwrapped T.
    //
    // GATE: skip auto-prop when arm patterns explicitly match
    // Ok/Error/Some/None — that's user-written Result/Option
    // discrimination, NOT throws-sugar. See `arms_match_result_or_option_arm`.
    //
    // `lower_expr`'s centralized auto-prop hook (producer-side, Call /
    // MethodCall only) handles the throws-sugar unwrap when the scrutinee
    // is a call. For Identifier / field-access scrutinees the hook does
    // not fire — fall back to an explicit `maybe_auto_propagate` so a
    // Result-typed local scrutinee against non-Ok/Error arms still
    // unwraps. Clear `expected_type` so a Result-typed surrounding
    // destination (`Result[T,E] r = match …`) doesn't block the hook /
    // fallback — the surrounding type describes the MATCH RESULT slot,
    // not the scrutinee.
    let user_matches_result_option = super::stmts::arms_match_result_or_option_arm(arms);
    let saved_expected = ctx.func_state.expected_type.take();
    ctx.func_state.suppress_auto_prop = user_matches_result_option;
    let scrut_op = lower_expr(ctx, builder, scrutinee);
    let scrut_op = if user_matches_result_option {
        scrut_op
    } else {
        // Idempotent: no-op on Call/MethodCall (the hook already fired)
        // or on non-Result operands.
        maybe_auto_propagate(ctx, builder, scrut_op, scrutinee.span)
    };
    ctx.func_state.expected_type = saved_expected;
    let scrut_type = infer_operand_type_full(ctx, &scrut_op, builder);
    let source_at_last_use = if let Expr::Identifier(name) = &scrutinee.node {
        ctx.is_last_use_at(name, scrutinee.span)
    } else { false };
    let arms_consume_payload = super::stmts::arms_have_move_extract_exprs(arms, else_arm);
    let (scrut_local, scrut_type) = super::stmts::stage_match_scrutinee(ctx, builder, &scrut_op, scrut_type, source_at_last_use, arms_consume_payload);

    // Allocate result local with the surrounding context's expected type when
    // available (VarDecl `T x = match …`, return-position match, struct
    // field init, etc.). Without the expected type the local is sized as
    // I64 — fine for primitive arms but corrupts struct/String/Vector arms
    // because the slot allocation undersizes the actual value. The type is
    // refined further once we lower the first non-divergent arm so that
    // contexts without an expected_type (statement-level `match` whose
    // expression value is later coerced) still produce a correctly-sized
    // result.
    //
    // Snag #36 follow-up: when `expected_type` is a `Result[T, E]` wrapper
    // (e.g. `return match v: case A(b): b ...` from a throws function), the
    // match arms typically produce the bare Ok-type `T` — the Ok-wrap
    // happens at the outer `lower_return` boundary as part of the `throws`
    // sugar. Defer the slot's final type to the first non-divergent arm
    // so the arm's assign doesn't write a bare `T` into a `Result[T, E]`
    // slot. If the arms DO produce Result explicitly (`case A: Ok(true)`
    // / `case B: Error("...")`), the refinement picks up Result from the
    // first non-divergent arm — same end state, just discovered
    // arm-by-arm. The typechecker requires non-divergent arms to share
    // their type via unify, so the first arm's type is authoritative.
    let result_type_init = ctx.func_state.expected_type.unwrap_or(I64_TYPE);
    let result_local = builder.add_local(result_type_init, None);
    let expected_is_result_wrapper = ctx.func_state.expected_type
        .map_or(false, |t| ctx.type_registry.enum_category(t) == Some(EnumCategory::Result));
    let mut result_type_refined = ctx.func_state.expected_type.is_some()
        && !expected_is_result_wrapper;
    let merge_bb = builder.new_block();

    for (i, arm) in arms.iter().enumerate() {
        let arm_body_bb = builder.new_block();
        let next_test_bb = if i + 1 < arms.len() || else_arm.is_some() {
            builder.new_block()
        } else {
            merge_bb
        };

        let cond = super::stmts::lower_pattern_condition(
            ctx, builder, &arm.pattern, scrut_local, scrut_type,
        );
        builder.branch(cond, arm_body_bb, next_test_bb);

        builder.switch_to(arm_body_bb);
        // Snag #50: save/restore per-arm name→local bindings. CoW
        // materialization inside one arm body (e.g. `&v` triggering
        // `cow_before_mutation` on a bare-param Ptr) rebinds the name in
        // `func_state.locals` to a fresh owned local. Without the snapshot,
        // the rebind leaks into the SIBLING arm's body — a subsequent
        // `v`-reference resolves to the dead arm's materialized clone,
        // which was never initialized along the live path, and reads back
        // as the type's zero-init default.
        //
        // We only snapshot the NAME map (`func_state.locals`) — the full
        // `save_locals` path also rewinds `builder.locals[i].ownership`,
        // which would clobber the `set_owned(result_local)` the
        // `assign_match_arm_to_result` helper performs (Snag #31's
        // invariant — `result_local`'s Owned tag must survive the arm
        // boundary so the merge-bb's downstream `[Mv] user_var = copy
        // result_local` passes the AssignIntoOwnedSlot validator).
        let saved_arm_locals = ctx.func_state.locals.clone();
        super::stmts::emit_pattern_bindings(ctx, builder, &arm.pattern, scrut_local, scrut_type);
        let arm_val = lower_expr(ctx, builder, &arm.body);
        if !builder.is_terminated() {
            if !result_type_refined {
                let arm_ty = infer_operand_type_full(ctx, &arm_val, builder);
                // Skip refinement when the arm produces Unit — refining the
                // result_local to Unit makes its slot lower to LirType::Void
                // (declared as `void *` in C), and the next arm storing a
                // primitive value triggers `assignment to 'void *' from
                // 'int32_t'`. Unit-arm matches are statement-form; the result
                // is unused, so leaving it at the I64 default is harmless.
                if arm_ty != UNIT_TYPE {
                    builder.locals[result_local.0 as usize].type_id = arm_ty;
                    result_type_refined = true;
                }
            }
            assign_match_arm_to_result(ctx, builder, result_local, arm_val, arm.body.span);
            builder.jump(merge_bb);
        }
        ctx.func_state.locals = saved_arm_locals;

        if next_test_bb != merge_bb {
            builder.switch_to(next_test_bb);
        }
    }

    if let Some(else_expr) = else_arm {
        let saved_else_locals = ctx.func_state.locals.clone();
        let else_val = lower_expr(ctx, builder, else_expr);
        if !builder.is_terminated() {
            if !result_type_refined {
                let else_ty = infer_operand_type_full(ctx, &else_val, builder);
                if else_ty != UNIT_TYPE {
                    builder.locals[result_local.0 as usize].type_id = else_ty;
                }
            }
            assign_match_arm_to_result(ctx, builder, result_local, else_val, else_expr.span);
            builder.jump(merge_bb);
        }
        ctx.func_state.locals = saved_else_locals;
    }

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_local)
}

/// Emit Result unwrap with error propagation.
/// Takes an already-lowered Result operand, branches on tag:
///   Ok → returns extracted Ok value
///   Error → emits on_error cleanups, early-exit drops, returns error
pub fn emit_result_auto_propagate(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    result_operand: Operand,
    result_type: TypeId,
    prop_span: crate::span::Span,
) -> Operand {
    // Determine a working place to read the tag/Ok/Error payloads from.
    //
    // Resource path (Tier 1c): when result_operand is a bare-place operand on
    // a *resource* Result, Move it into a fresh `val_local` so it owns the data
    // without shallow-aliasing the source, and MoveZero the source. The
    // resource Ok/Error field loads below zero the slot they read from, so they
    // must read from the owned `val_local`, not the source.
    //
    // Non-resource fast path: when the operand is a bare-place operand on a
    // *non-resource* Result, read tag/fields DIRECTLY from the source place —
    // the redundant `val_local` memcpy buys nothing. The source is a
    // materialized place (a call result / enum-init) that is dead immediately
    // after, the loads are non-destructive for non-resource fields, and a
    // non-resource Result carries no drop, so no MoveZero/unregister is owed.
    //
    // Fallback: a non-place operand (constant / projected place) still needs a
    // temp so `tag_of` / `enum_field_load_move` have a `Place` to address.
    let src_local = if let Operand::Copy(ref p) | Operand::Move(ref p) = result_operand {
        if p.projections.is_empty() { Some(p.local) } else { None }
    } else { None };
    let is_resource = ctx.type_registry.is_resource_type(result_type);
    let (work_place, owns_work) = if let Some(src) = src_local {
        if is_resource {
            let val_local = builder.add_local(result_type, None);
            builder.assign_mode(
                crate::ir::instructions::AssignMode::Move,
                Place::local(val_local),
                result_operand,
            );
            if !ctx.drops.is_moved(src) {
                ctx.move_zero_and_mark(builder, src);
            }
            (Place::local(val_local), true)
        } else {
            // Read straight from the source — no copy.
            (Place::local(src), false)
        }
    } else {
        let val_local = builder.add_local(result_type, None);
        builder.assign(Place::local(val_local), result_operand);
        (Place::local(val_local), true)
    };

    // Look up Ok/Error field types from the Result type definition
    let (ok_field_type, err_field_type) = extract_result_field_types(ctx, result_type);

    // Check tag: 0 = Ok, 1 = Error
    let tag = builder.tag_of(Operand::Copy(work_place.clone()));
    let is_ok = builder.cmp(
        CmpOp::Eq,
        I32_TYPE,
        FunctionBuilder::copy(tag),
        Operand::Constant(Constant::I32(0)),
    );

    let ok_bb = builder.new_block();
    let err_bb = builder.new_block();
    let merge_bb = builder.new_block();

    builder.branch(FunctionBuilder::copy(is_ok), ok_bb, err_bb);

    // Ok path: extract Ok value (field 0 of variant 0)
    builder.switch_to(ok_bb);
    let ok_val = builder.enum_field_load_move(
        work_place.clone(),
        "Ok",
        0,
        ok_field_type,
    );
    // Move-if-dead: the owned Result temp is consumed by ? — unregister +
    // MoveZero. Skipped on the non-resource fast path: `work_place` is the
    // borrowed source (not ours to zero) and carries no drop.
    if owns_work {
        ctx.drops.unregister(work_place.local);
        builder.move_zero(work_place.clone());
        ctx.drops.mark_moved(work_place.local);
    }
    builder.jump(merge_bb);

    // Error path: propagate error via early return
    builder.switch_to(err_bb);
    let err_val = builder.enum_field_load_move(
        work_place.clone(),
        "Error",
        0,
        err_field_type,
    );
    // work_place already unregistered above (both paths share the unregister)
    if owns_work {
        builder.move_zero(work_place.clone());
        ctx.drops.mark_moved(work_place.local);
    }
    // Re-wrap error in the *current* function's Result type and return.
    let fn_result_type = ctx.func_state.current_throws_result_type.or_else(|| {
        let ret_type = builder.locals[0].type_id;
        let is_result = ctx.type_registry.enum_category(ret_type) == Some(EnumCategory::Result);
        if is_result {
            Some(ret_type)
        } else {
            None
        }
    });
    // Snag #11: if the typechecker recorded a `From` conversion for this
    // propagation site (the callee-E differs from the caller-E but is
    // convertible), convert the loaded error value to the caller's error type
    // BEFORE re-wrapping it. Without this, the `enum_init` below memcpy's a
    // `sizeof(calleeE)` value into a `sizeof(callerE)` Error slot — the
    // type-confused over-read. The same-error-type case never records anything
    // here, so this is a true no-op then (byte-identical emitted C).
    //
    // The conversion produces a FRESH, owned caller-error value; that owned
    // local — not the original callee-error `err_val` — is what gets moved into
    // the Error slot, so it is the one we mark moved below (preventing its drop
    // from freeing the heap data now shared with the return slot — a double-free
    // the original code never hit because the un-converted `err_val` carried no
    // independent owned allocation). The original `err_val` (consumed by the
    // From call as a borrow) is still move-zeroed at its load site.
    let err_val_for_wrap = if let Some(fn_res_type) = fn_result_type {
        if ctx.analysis.from_conversions.contains_key(&prop_span) {
            let (_ok_caller, caller_err_type) = extract_result_field_types(ctx, fn_res_type);
            maybe_emit_from_conversion(ctx, builder, err_val, err_field_type, caller_err_type)
        } else {
            err_val
        }
    } else {
        err_val
    };
    if let Some(fn_res_type) = fn_result_type {
        let type_name = ctx.type_registry.type_name(fn_res_type).unwrap_or_else(|| "Result".to_string());
        let err_dst = builder.enum_init(type_name, "Error", fn_res_type, vec![FunctionBuilder::copy(err_val_for_wrap)]);
        // Tier 1c: Move + MoveZero — err_dst is freshly built (Owned)
        // and dead immediately after; Copy would shallow-alias the
        // return slot and double-free at scope exit now that
        // Option/Result are Resource.
        if ctx.type_registry.is_resource_type(fn_res_type) {
            builder.assign_mode(
                crate::ir::instructions::AssignMode::Move,
                Place::local(LocalId(0)),
                FunctionBuilder::copy(err_dst),
            );
            ctx.move_zero_and_mark(builder, err_dst);
        } else {
            builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(err_dst));
        }
    } else {
        builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(err_val_for_wrap));
    }
    // Mark consumed error value as moved to prevent double-free during early-exit drops.
    // This is the value that flowed into the Error slot: the From-converted owned
    // local when a conversion fired, else the original loaded `err_val`.
    ctx.move_zero_and_mark(builder, err_val_for_wrap);
    super::stmts::emit_on_error_cleanups(ctx, builder);
    ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, super::drops::DropScopeKind::Function, None);
    builder.ret(FunctionBuilder::copy(LocalId(0)));

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(ok_val)
}

/// The `Ok` payload type of a `Result[T, E]` (the `T`). Used to peel a
/// throws/Result return slot down to its inner destination when propagating
/// a per-element/per-value `expected_type` into an aggregate literal.
fn extract_result_ok_type(ctx: &LoweringContext, result_type: TypeId) -> TypeId {
    extract_result_field_types(ctx, result_type).0
}

/// Snag #11 — emit a `From[CalleeE]` conversion on a propagated error value.
/// Given the loaded callee-error operand (`err_val` of type `callee_err_type`)
/// and the caller's error type (`caller_err_type`), call the equipped
/// `CallerE from(CalleeE)` static method and return the converted operand. The
/// typechecker already proved the impl exists (and recorded it in
/// `from_conversions`); this resolves the emitted symbol the same way an
/// explicit `CallerE.from(e)` call does — via the `_for_<CallerE>__from`
/// `fn_sigs` suffix, disambiguated by the callee-error arg type. If the symbol
/// can't be found (e.g. nothing got emitted), fall back to the raw value so the
/// build doesn't regress (the typecheck error is the real guard).
fn maybe_emit_from_conversion(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    err_val: LocalId,
    callee_err_type: TypeId,
    caller_err_type: TypeId,
) -> LocalId {
    // No-op when the error types already match (defensive; the metadata is
    // only recorded for genuinely-differing types).
    if callee_err_type == caller_err_type {
        return err_val;
    }
    let Some(caller_name) = ctx.type_name_for_id(caller_err_type).map(|s| s.to_string()) else {
        return err_val;
    };
    let suffix = format!("_for_{caller_name}__from");
    let candidates: Vec<String> = ctx
        .fn_sigs
        .keys()
        .filter(|k| k.ends_with(&suffix))
        .cloned()
        .collect();
    // Pick the overload whose single param matches the callee error type.
    let symbol = if candidates.len() == 1 {
        Some(candidates[0].clone())
    } else {
        candidates.into_iter().find(|k| {
            ctx.fn_sigs
                .get(k.as_str())
                .map(|(params, _)| params.len() == 1 && params[0] == callee_err_type)
                .unwrap_or(false)
        })
    };
    let Some(symbol) = symbol else {
        return err_val;
    };
    // Call `CallerE from(CalleeE)`: pass the loaded error value, get the
    // converted caller-error value back. `call_tracked` registers the result
    // for drop the same way the explicit `.from()` path does.
    let converted = ctx.call_tracked(
        builder,
        symbol,
        vec![FunctionBuilder::copy(err_val)],
        caller_err_type,
    );
    converted
}

/// The Ok-payload (success) type of a `Result[T, E]` slot — i.e. the declared
/// return type `T` of a `throws` fn whose synthesized return slot is `result_type`.
///
/// Returns `result_type` unchanged when it is not a Result enum (defensive — a
/// `throws` fn's slot is always a `Result`, so the fallback only guards
/// mis-calls). Used to set `expected_type` to the *user-level* return type when
/// lowering a `throws` fn's tail/return value, so the auto-prop Result-gate and
/// `Ok(...)`/`Error(...)` constructor resolution see `T`, not the slot. This is
/// what makes a `throws` fn whose declared `T` is itself a `Result` lower
/// correctly (the inner value keeps its Result layer; the outer Ok-wrap then
/// fires exactly once).
pub fn result_ok_payload_type(ctx: &LoweringContext, result_type: TypeId) -> TypeId {
    if ctx.type_registry.enum_category(result_type) == Some(EnumCategory::Result) {
        extract_result_field_types(ctx, result_type).0
    } else {
        result_type
    }
}

/// Extract Ok and Error field types from a Result type definition.
fn extract_result_field_types(ctx: &LoweringContext, result_type: TypeId) -> (TypeId, TypeId) {
    let type_name = ctx.type_registry.type_name(result_type);
    if let Some(ref name) = type_name {
        if let Some(td) = ctx.type_registry.get_type_def(name) {
            if let crate::ir::types::TypeDefKind::Enum(ref e) = td.kind {
                let ok_ty = e.variants.iter().find(|v| v.name == "Ok")
                    .and_then(|v| v.fields.first().map(|f| f.type_id))
                    .unwrap_or(I64_TYPE);
                let err_ty = e.variants.iter().find(|v| v.name == "Error")
                    .and_then(|v| v.fields.first().map(|f| f.type_id))
                    .unwrap_or(I64_TYPE);
                return (ok_ty, err_ty);
            }
        }
    }
    (I64_TYPE, I64_TYPE)
}

/// Check if a type is a Result type and the current function can propagate errors.
/// Returns the Result TypeId if auto-propagation should occur.
///
/// Triggers when:
/// 1. The operand type is `Result__*`, AND
/// 2. The current function can propagate: has `throws` OR returns `Result`
pub fn should_auto_propagate(ctx: &LoweringContext, builder: &FunctionBuilder, type_id: TypeId) -> Option<TypeId> {
    let is_result = ctx.type_registry.enum_category(type_id) == Some(EnumCategory::Result);
    if !is_result {
        return None;
    }
    // Check if current function can propagate
    if ctx.func_state.current_throws_result_type.is_some() {
        return Some(type_id);
    }
    let ret_type = builder.locals[0].type_id;
    let ret_is_result = ctx.type_registry.enum_category(ret_type) == Some(EnumCategory::Result);
    if ret_is_result {
        return Some(type_id);
    }
    None
}

/// If operand is Result-typed and current function can propagate, auto-unwrap.
/// Otherwise return operand unchanged.
///
/// Skips auto-propagation when the expected destination type is itself a Result
/// (e.g., `Result[int, str] r = risky()` should keep the Result).
pub fn maybe_auto_propagate(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    operand: Operand,
    prop_span: crate::span::Span,
) -> Operand {
    // If the destination expects a Result, don't unwrap
    if let Some(expected) = ctx.func_state.expected_type {
        let is_result = ctx.type_registry.enum_category(expected) == Some(EnumCategory::Result);
        if is_result {
            return operand;
        }
    }
    let op_type = infer_operand_type_full(ctx, &operand, builder);
    if let Some(result_type) = should_auto_propagate(ctx, builder, op_type) {
        emit_result_auto_propagate(ctx, builder, operand, result_type, prop_span)
    } else {
        operand
    }
}

/// Lower a rethrow expression:
///   `expr rethrow (Type name): transform`  (binding form)
///   `expr rethrow transform`               (bare form)
///
/// Like auto-propagation, but the error path evaluates a transform expression
/// and throws that instead. The binding form makes the original error available
/// to the transform; the bare form discards it.
fn lower_rethrow_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    inner: &Spanned<Expr>,
    error_binding: Option<&(Spanned<crate::parser::ast::Type>, Spanned<String>)>,
    transform: &Spanned<Expr>,
) -> Operand {
    use crate::ir::instructions::AssignMode;

    // Phase C: pick Move mode for resource sources at boundary assigns —
    // mirrors the C2.17 mode_for closure in lower_catch_expr.
    let mode_for = |ctx: &LoweringContext, builder: &FunctionBuilder, op: &Operand, ty: TypeId| {
        if !ctx.type_registry.is_resource_type(ty)
            && !ctx.type_registry.needs_drop(ty)
        {
            return AssignMode::Copy;
        }
        match op {
            Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => {
                let src_ty = builder.local_type(p.local);
                if ctx.type_registry.needs_drop(src_ty)
                    || ctx.type_registry.is_resource_type(src_ty)
                {
                    AssignMode::Move
                } else {
                    AssignMode::Copy
                }
            }
            _ => AssignMode::Copy,
        }
    };

    // Suppress lower_expr's centralized auto-prop hook — rethrow operates
    // on the raw `Result[T, E]` value (extracts Ok/Error payloads itself).
    ctx.func_state.suppress_auto_prop = true;
    let val = lower_expr(ctx, builder, inner);
    let val_type = infer_operand_type_full(ctx, &val, builder);
    let val_local = builder.add_local(val_type, None);
    let val_mode = mode_for(ctx, builder, &val, val_type);
    // Tier 1c: snapshot the source before assign_mode consumes the
    // operand, so we can mark it moved after a Move-mode assign.
    // Without this, the source Result shares heap data with val_local;
    // both drop at scope exit → double-free now that Result is Resource.
    let val_src_local = if let Operand::Copy(ref p) | Operand::Move(ref p) = val {
        if p.projections.is_empty() { Some(p.local) } else { None }
    } else { None };
    builder.assign_mode(val_mode, Place::local(val_local), val);
    if matches!(val_mode, crate::ir::instructions::AssignMode::Move) {
        if let Some(src_local) = val_src_local {
            if !ctx.drops.is_moved(src_local) {
                ctx.move_zero_and_mark(builder, src_local);
            }
        }
    }

    // Look up Ok/Error field types from the Result type definition
    let (ok_field_type, err_field_type) = extract_result_field_types(ctx, val_type);

    // Check tag: 0 = Ok, 1 = Error
    let tag = builder.tag_of(FunctionBuilder::copy(val_local));
    let is_ok = builder.cmp(
        CmpOp::Eq,
        I32_TYPE,
        FunctionBuilder::copy(tag),
        Operand::Constant(Constant::I32(0)),
    );

    let ok_bb = builder.new_block();
    let err_bb = builder.new_block();
    let merge_bb = builder.new_block();

    builder.branch(FunctionBuilder::copy(is_ok), ok_bb, err_bb);

    // Ok path: extract Ok value (identical to lower_try_expr)
    builder.switch_to(ok_bb);
    let ok_val = builder.enum_field_load_move(
        Place::local(val_local),
        "Ok",
        0,
        ok_field_type,
    );
    builder.move_zero(Place::local(val_local));
    ctx.drops.mark_moved(val_local);
    builder.jump(merge_bb);

    // Error path: optionally bind error to name, evaluate transform, throw that
    builder.switch_to(err_bb);
    if let Some((_error_type, error_name)) = error_binding {
        let err_val = builder.enum_field_load_move(
            Place::local(val_local),
            "Error",
            0,
            err_field_type,
        );
        builder.move_zero(Place::local(val_local));
        ctx.drops.mark_moved(val_local);
        let err_local = builder.add_local(err_field_type, Some(&error_name.node));
        let err_op = FunctionBuilder::copy(err_val);
        let err_mode = mode_for(ctx, builder, &err_op, err_field_type);
        builder.assign_mode(err_mode, Place::local(err_local), err_op);
        ctx.register_local(&error_name.node, err_local, err_field_type);
    }

    // Evaluate the transform expression — this produces the new error value
    let new_err = lower_expr(ctx, builder, transform);

    // Wrap the transformed error in the current function's Result.Error and return
    let new_err_local = if let Operand::Copy(ref place) | Operand::Move(ref place) = new_err {
        if place.projections.is_empty() { Some(place.local) } else { None }
    } else { None };
    if let Some(result_type) = ctx.func_state.current_throws_result_type {
        let type_name = ctx.type_registry.type_name(result_type).unwrap_or_else(|| "Result".to_string());
        let err_dst = builder.enum_init(type_name, "Error", result_type, vec![new_err]);
        let dst_op = FunctionBuilder::copy(err_dst);
        let dst_mode = mode_for(ctx, builder, &dst_op, result_type);
        builder.assign_mode(dst_mode, Place::local(LocalId(0)), dst_op);
    } else {
        let ret_ty = builder.locals[0].type_id;
        let new_err_mode = mode_for(ctx, builder, &new_err, ret_ty);
        builder.assign_mode(new_err_mode, Place::local(LocalId(0)), new_err);
    }
    // Mark consumed operand as moved to prevent double-free during early-exit drops
    if let Some(local) = new_err_local {
        ctx.move_zero_and_mark(builder, local);
    }
    // Emit on_error cleanups before drops
    super::stmts::emit_on_error_cleanups(ctx, builder);
    ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, super::drops::DropScopeKind::Function, None);
    builder.ret(FunctionBuilder::copy(LocalId(0)));

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(ok_val)
}

/// Lower a `catch` expression: `expr catch (name): recovery`.
/// On Ok: returns the unwrapped Ok value.
/// On Error: binds error to `name`, evaluates `recovery`, returns that.
/// The overall expression always succeeds (never throws).
fn lower_catch_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    inner: &Spanned<Expr>,
    error_binding: &Spanned<String>,
    recovery: &Spanned<Expr>,
) -> Operand {
    use crate::ir::instructions::AssignMode;

    // Phase C: pick Move mode whenever a resource is being staged (val,
    // extracted ok/err payload, recovery result). The sources here are
    // either fresh enum-field-load values (already moved out) or
    // expression results that own their data — Move is the typed
    // contract. Copy mode left shallow aliases the validator flagged.
    let mode_for = |ctx: &LoweringContext, builder: &FunctionBuilder, op: &Operand, ty: TypeId| {
        if !ctx.type_registry.is_resource_type(ty)
            && !ctx.type_registry.needs_drop(ty)
        {
            return AssignMode::Copy;
        }
        match op {
            Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => {
                let src_ty = builder.local_type(p.local);
                if ctx.type_registry.needs_drop(src_ty)
                    || ctx.type_registry.is_resource_type(src_ty)
                {
                    AssignMode::Move
                } else {
                    AssignMode::Copy
                }
            }
            _ => AssignMode::Copy,
        }
    };

    // Suppress lower_expr's centralized auto-prop hook — catch operates
    // on the raw `Result[T, E]` value (extracts Ok / binds Error itself).
    ctx.func_state.suppress_auto_prop = true;
    let val = lower_expr(ctx, builder, inner);
    let val_type = infer_operand_type_full(ctx, &val, builder);
    let val_local = builder.add_local(val_type, None);
    let val_mode = mode_for(ctx, builder, &val, val_type);
    // Tier 1c: snapshot the source before assign_mode consumes the
    // operand, so we can mark it moved after a Move-mode assign.
    // Without this, the source Result shares heap data with val_local;
    // both drop at scope exit → double-free now that Result is Resource.
    let val_src_local = if let Operand::Copy(ref p) | Operand::Move(ref p) = val {
        if p.projections.is_empty() { Some(p.local) } else { None }
    } else { None };
    builder.assign_mode(val_mode, Place::local(val_local), val);
    if matches!(val_mode, crate::ir::instructions::AssignMode::Move) {
        if let Some(src_local) = val_src_local {
            if !ctx.drops.is_moved(src_local) {
                ctx.move_zero_and_mark(builder, src_local);
            }
        }
    }

    // Look up Ok/Error field types from the Result type definition
    let (ok_field_type, err_field_type) = extract_result_field_types(ctx, val_type);

    // Check tag: 0 = Ok, 1 = Error
    let tag = builder.tag_of(FunctionBuilder::copy(val_local));
    let is_ok = builder.cmp(
        CmpOp::Eq,
        I32_TYPE,
        FunctionBuilder::copy(tag),
        Operand::Constant(Constant::I32(0)),
    );

    let ok_bb = builder.new_block();
    let err_bb = builder.new_block();
    let merge_bb = builder.new_block();

    // Allocate result local for the merged value (Ok type)
    let result_local = builder.add_local(ok_field_type, None);

    builder.branch(FunctionBuilder::copy(is_ok), ok_bb, err_bb);

    // Ok path: extract Ok value, store into result. Zero `val_local`
    // after the variant payload is moved out — this turns the
    // structural pattern into the canonical "EnumFieldLoad followed by
    // MoveZero of base" shape recognised by `tag_ownership` (mirrors
    // `lower_question_op`'s handling at `:2441`). Without this, the
    // extracted slot stayed Untracked and the downstream Move-mode
    // assign tripped Tier 2a's `AssignIntoOwnedSlot` validator. The
    // base `val_local` was already going to be dead from this point
    // (its tag is read once into `tag`; both branches consume the
    // payload), so the zero is structurally sound.
    builder.switch_to(ok_bb);
    let ok_val = builder.enum_field_load_move(
        Place::local(val_local),
        "Ok",
        0,
        ok_field_type,
    );
    builder.move_zero(Place::local(val_local));
    let ok_op = FunctionBuilder::copy(ok_val);
    let ok_mode = mode_for(ctx, builder, &ok_op, ok_field_type);
    builder.assign_mode(ok_mode, Place::local(result_local), ok_op);
    builder.jump(merge_bb);

    // Error path: bind error, evaluate recovery, store into result.
    // Same MoveZero of `val_local` as the Ok path — payload moved out.
    builder.switch_to(err_bb);
    let err_val = builder.enum_field_load_move(
        Place::local(val_local),
        "Error",
        0,
        err_field_type,
    );
    builder.move_zero(Place::local(val_local));
    let err_local = builder.add_local(err_field_type, Some(&error_binding.node));
    let err_op = FunctionBuilder::copy(err_val);
    let err_mode = mode_for(ctx, builder, &err_op, err_field_type);
    builder.assign_mode(err_mode, Place::local(err_local), err_op);
    // Tier 2a (Core invariant #3): the error binding is born OWNING the
    // moved-out `Error` payload — `enum_field_load_move` + `MoveZero` of
    // `val_local` transferred ownership of the heap data into `err_val`,
    // which the Move-mode assign forwards into `err_local`. Tag the typed
    // ownership at this writer site so a recovery expression that returns the
    // bare binding (`… catch (e): e`) flows a tracked Owned source into
    // `result_local`, instead of an Untracked one that trips Tier 2a's
    // `AssignIntoOwnedSlot` validator. Mirrors the Snag #38 `set_owned` of
    // `result_local` below. Guarded on Move mode: a primitive (Copy-mode)
    // error payload is not drop-tracked, so leaving it Untracked is correct.
    if matches!(err_mode, crate::ir::instructions::AssignMode::Move) {
        ctx.set_owned(builder, err_local);
    }
    ctx.register_local(&error_binding.node, err_local, err_field_type);

    let recovery_val = lower_expr(ctx, builder, recovery);
    // Snag #39 (2026-05-12): if the recovery is divergent (ends in
    // `return`/`exit`/`throw`/`unreachable`), the builder is already
    // terminated. Emitting `assign_mode` + `jump(merge_bb)` would
    // clobber the divergent terminator (per `set_terminator`'s
    // unconditional overwrite) and the C-emit would synthesise a
    // bogus default initializer at the merge — `__sN (V) = (int32_t)0LL`
    // for non-Copy enum result types. Same gating as Snag #33's
    // `lower_match_stmt_as_expr` post-loop fallthrough fix.
    if !builder.is_terminated() {
        let recovery_mode = mode_for(ctx, builder, &recovery_val, ok_field_type);
        // Mirror the Ok/Error payload move-out above (the `val_local` path): a
        // Move-mode assign from an OWNING temp/local must zero+mark the source,
        // else the recovery's heap data is moved into `result_local` AND dropped
        // again at the merge → double-free. Only the bare/atom recoveries
        // (`catch (e): e` / a static `"literal"`) dodged it; an ALLOCATING
        // recovery (`catch (e): "[" + e + "]"`, a concat/fn-call returning an
        // owned String) hit it. The `is_moved` guard keeps `catch (e): e`
        // (already-moved err binding) a no-op.
        let recovery_src_local = if let Operand::Copy(ref p) | Operand::Move(ref p) =
            recovery_val
        {
            if p.projections.is_empty() { Some(p.local) } else { None }
        } else {
            None
        };
        builder.assign_mode(recovery_mode, Place::local(result_local), recovery_val);
        if matches!(recovery_mode, crate::ir::instructions::AssignMode::Move) {
            if let Some(src_local) = recovery_src_local {
                if !ctx.drops.is_moved(src_local) {
                    ctx.move_zero_and_mark(builder, src_local);
                }
            }
        }
        builder.jump(merge_bb);
    }

    builder.switch_to(merge_bb);
    // Snag #38 (2026-05-12): tag result_local as Owned after both branches'
    // Move-mode assigns. Same shape as `assign_match_arm_to_result`'s Snag
    // #31 fix — without this, a downstream `[Mv] user_var = copy
    // result_local` (e.g. `V x = throws_fn() catch (msg): V.B(msg)` for
    // non-Copy V) sees result_local as Untracked and trips Tier 2a's
    // AssignIntoOwnedSlot validator. Both writer paths in this function
    // (Ok extraction + recovery expression) are Move-mode assigns from
    // sources that own their data; the typed tag must follow.
    ctx.set_owned(builder, result_local);
    FunctionBuilder::copy(result_local)
}

/// Lower a fault-`catch` expression (error-model.md §11): a faultable op in the
/// wrapped expression's OWN basic blocks branches to a local handler instead of
/// panicking; the result is the wrapped value on the no-fault path or the
/// handler value on the fault path. Pure local control flow — no unwinding.
///
/// CFG (binding form catches BOTH fault categories, each constructing its own
/// `Fault` variant; pattern form catches just the named category):
/// ```text
///   …faultable ops…  ── fault(overflow) ──▶ overflow_entry ─▶ handler ─▶ merge
///        │           ── fault(div0)     ──▶ divzero_entry  ─▶ handler ─▶ merge
///        └── no fault ──▶ store wrapped value ─▶ merge
/// ```
fn lower_fault_catch_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    inner: &Spanned<Expr>,
    pattern: &ast::FaultCatchPattern,
    handler: &Spanned<Expr>,
) -> Operand {
    use super::context::FaultScope;

    // Which fault categories does this catch intercept?
    // - binding form `catch f:` → ALL (Overflow + DivByZero + Bounds);
    // - pattern form `catch Fault.Overflow:` → Overflow only;
    // - pattern form `catch Fault.DivByZero:` → DivByZero only;
    // - pattern form `catch Fault.Bounds:` → Bounds only.
    let (catch_overflow, catch_divzero, catch_bounds, binding_name) = match pattern {
        ast::FaultCatchPattern::Binding(name) => (true, true, true, Some(name.node.clone())),
        ast::FaultCatchPattern::Variant { variant, .. } => match variant.node.as_str() {
            "Overflow" => (true, false, false, None),
            "DivByZero" => (false, true, false, None),
            "Bounds" => (false, false, true, None),
            // An unknown variant was already reported at typecheck; lower it as
            // "catch nothing" so the build stays well-formed (ops panic).
            _ => (false, false, false, None),
        },
    };

    // Per-category handler-entry blocks + the merge block. Created up front so
    // the faultable ops (lowered next) can branch to them; their bodies are
    // filled after we know the result type.
    let merge_bb = builder.new_block();
    let overflow_entry = if catch_overflow { Some(builder.new_block()) } else { None };
    let divzero_entry = if catch_divzero { Some(builder.new_block()) } else { None };
    let bounds_entry = if catch_bounds { Some(builder.new_block()) } else { None };

    // Per-category PANIC blocks for the UNCAUGHT Div/Rem fault. A single signed
    // Div has two fault categories; in a partial-catch (`catch Fault.DivByZero:`
    // doesn't catch the `TYPE_MIN/-1` overflow, and vice-versa) the uncaught
    // category must PANIC — uniformly on both backends. Emit the panic at GIR
    // (a `gorget_panic` block) so the LIR Div lowering only ever BRANCHES (no
    // backend-specific LIR panic; error-model.md §11 (C) partial-catch). Filled
    // below; if no Div/Rem appears in the subtree these blocks are dead and DCE
    // removes them. (Add/Sub/Mul never reach here — they only fault when caught.)
    let div_overflow_panic = builder.new_block();
    let div_zero_panic = builder.new_block();
    let bounds_panic = builder.new_block();
    let saved_active = builder.current_block;
    builder.switch_to(div_overflow_panic);
    builder.call_extern(
        "gorget_panic",
        vec![Operand::Constant(Constant::Str("integer overflow".to_string()))],
        UNIT_TYPE,
    );
    builder.unreachable();
    builder.switch_to(div_zero_panic);
    builder.call_extern(
        "gorget_panic",
        vec![Operand::Constant(Constant::Str("division by zero".to_string()))],
        UNIT_TYPE,
    );
    builder.unreachable();
    builder.switch_to(bounds_panic);
    builder.call_extern(
        "gorget_panic",
        vec![Operand::Constant(Constant::Str("index out of bounds".to_string()))],
        UNIT_TYPE,
    );
    builder.unreachable();
    builder.switch_to(saved_active);

    // Push the fault scope for the inner expression's subtree. Save/restore the
    // outer scope so a nested fault-catch composes (innermost wins).
    let saved_scope = ctx.func_state.fault_scope.take();
    ctx.func_state.fault_scope = Some(FaultScope {
        overflow_handler: overflow_entry,
        divzero_handler: divzero_entry,
        bounds_handler: bounds_entry,
        div_overflow_panic,
        div_zero_panic,
        bounds_panic,
    });
    // The wrapped expression is a plain value; suppress the throws-call
    // auto-prop peel (a faultable `a*b` is never a Result — mirror typecheck).
    ctx.func_state.suppress_auto_prop = true;
    let inner_val = lower_expr(ctx, builder, inner);
    // Pop the fault scope — faults outside this expression panic again.
    ctx.func_state.fault_scope = saved_scope;

    // A faultable index read of a RESOURCE element (e.g. `Vector[String]`)
    // yields a `Ptr(T)` borrow on the no-fault path, but a pattern-form handler
    // (`catch Fault.Bounds: "fallback"`) yields an OWNED `T`. The fault-catch
    // result ESCAPES the catch (an ownership boundary), so materialize the
    // no-fault borrow to an owned value here — mirroring the Ptr→owned clone the
    // outer `T x = …`/`return …` boundary would do anyway — so BOTH branches
    // share the OWNED result type (else the C/LLVM result slot is `void*` and the
    // handler stores a `Str` into it). Primitive int results (the common
    // faultable-arith case) are pass-through.
    let inner_val = ctx.ensure_owned_at_boundary(
        builder, inner_val, inner.span, crate::ir::ImplicitCloneReason::ReturnFromBorrow,
    );

    let result_type = infer_operand_type_full(ctx, &inner_val, builder);
    let result_local = builder.add_local(result_type, None);

    // Stage a value into `result_local` with the right AssignMode. For a
    // resource result type (e.g. a `Vector` wrapped expr that has no faultable
    // op) a plain Copy is a shallow alias the resource-move validator rejects —
    // pick Move, mirroring `lower_catch_expr`'s `mode_for`. Primitive int
    // results (the common faultable case) stay Copy.
    fn store_result(
        ctx: &mut LoweringContext,
        builder: &mut FunctionBuilder,
        result_local: LocalId,
        result_type: TypeId,
        val: Operand,
    ) {
        use crate::ir::instructions::AssignMode;
        let mode = if ctx.type_registry.is_resource_type(result_type)
            || ctx.type_registry.needs_drop(result_type)
        {
            AssignMode::Move
        } else {
            AssignMode::Copy
        };
        // Snapshot the source BEFORE assign_mode consumes the operand so a
        // Move-mode store can move-zero it (mirrors `lower_catch_expr`): without
        // this, the source (e.g. a freshly-cloned owned String from the
        // `ensure_owned_at_boundary` materialization, or a Move-staged Vector)
        // shares heap data with `result_local` and BOTH drop at scope exit →
        // double-free.
        let src_local = if let Operand::Copy(ref p) | Operand::Move(ref p) = val {
            if p.projections.is_empty() { Some(p.local) } else { None }
        } else { None };
        builder.assign_mode(mode, Place::local(result_local), val);
        if mode == AssignMode::Move {
            ctx.set_owned(builder, result_local);
            if let Some(src) = src_local {
                if !ctx.drops.is_moved(src) {
                    ctx.move_zero_and_mark(builder, src);
                }
            }
        }
    }

    // No-fault path: the wrapped value flows to the result, then to merge. Skip
    // when a divergent inner already terminated the block (mirrors lower_catch).
    if !builder.is_terminated() {
        store_result(ctx, builder, result_local, result_type, inner_val);
        builder.jump(merge_bb);
    }

    // Fill each handler-entry block: (binding) construct the Fault variant and
    // bind it; then lower the handler into the result and jump to merge. The
    // handler body is lowered ONCE PER ENTRY so each sees the right bound value
    // — small + correct for the two-category Increment-1 set.
    let fault_type = ctx.type_mapper.lookup_named("Fault").unwrap_or(UNIT_TYPE);
    let lower_entry = |ctx: &mut LoweringContext, builder: &mut FunctionBuilder, entry: BlockId, variant: &str| {
        builder.switch_to(entry);
        if let Some(ref name) = binding_name {
            let fault_val = ctx.emit_enum_init_owned(builder, "Fault", variant, fault_type, vec![], None);
            ctx.register_local(name, fault_val, fault_type);
        }
        let handler_val = lower_expr(ctx, builder, handler);
        if !builder.is_terminated() {
            store_result(ctx, builder, result_local, result_type, handler_val);
            builder.jump(merge_bb);
        }
    };
    if let Some(entry) = overflow_entry {
        lower_entry(ctx, builder, entry, "Overflow");
    }
    if let Some(entry) = divzero_entry {
        lower_entry(ctx, builder, entry, "DivByZero");
    }
    if let Some(entry) = bounds_entry {
        lower_entry(ctx, builder, entry, "Bounds");
    }

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_local)
}

/// Lower a block expression — the last expression in the block is the value.
fn lower_block_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    block: &ast::Block,
) -> Operand {
    if block.stmts.is_empty() {
        return Operand::Constant(Constant::Unit);
    }

    for stmt in &block.stmts[..block.stmts.len() - 1] {
        super::stmts::lower_stmt(ctx, builder, stmt);
    }

    let last = &block.stmts[block.stmts.len() - 1];
    lower_stmt_as_tail_value(ctx, builder, last)
        .unwrap_or(Operand::Constant(Constant::Unit))
}

/// Lower the *last* statement of a block as a tail value. Recognises the
/// three tail-value shapes that any block-as-expression context produces:
///   - `Stmt::Expr(expr)` — bare trailing expression
///   - `Stmt::If { ... }` — `if`/`elif`/`else` chain used as a value
///   - `Stmt::Match { ... }` — match statement used as a value
/// Returns `Some(op)` for the three tail-value shapes. For any other
/// statement form, lowers it as a regular statement and returns `None`;
/// callers should treat that as "no tail value produced" (block-as-Unit,
/// or implicit-return slot left at its zero-init default).
///
/// Both `lower_block_expr` and the closure-body lowerer must dispatch
/// through this helper — keeping the recognised-tail-shapes list in one
/// place is what closed snag #51 (the closure-body tail dispatcher only
/// handled `Stmt::Expr`, so `match`/`if`-as-tail values vanished into
/// `LocalId(0)`'s zero-init default). Add new tail-value shapes here.
pub(super) fn lower_stmt_as_tail_value(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    last: &Spanned<ast::Stmt>,
) -> Option<Operand> {
    match &last.node {
        ast::Stmt::Expr(expr) => Some(lower_expr(ctx, builder, expr)),
        ast::Stmt::If { condition, then_body, elif_branches, else_body } => {
            Some(build_if_chain_expr(ctx, builder, condition, then_body, elif_branches, else_body))
        }
        ast::Stmt::Match { scrutinee, arms, else_arm } => {
            Some(lower_match_stmt_as_expr(ctx, builder, scrutinee, arms.as_slice(), else_arm.as_ref()))
        }
        _ => {
            super::stmts::lower_stmt(ctx, builder, last);
            None
        }
    }
}

/// Lower a Stmt::Match used as a tail expression in a block.
fn lower_match_stmt_as_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    scrutinee: &Spanned<Expr>,
    arms: &[ast::MatchItem],
    else_arm: Option<&ast::Block>,
) -> Operand {
    // Phase C: stage with the right AssignMode by source shape. This is
    // the path that nested `match X:` inside an arm body takes (the inner
    // match becomes the last stmt of the arm-body block, which is lowered
    // as Expr::Block, which routes the trailing match here). Without
    // this, the @DataFrame__col_* cluster's inner `match b:` produced
    // `_scrut = copy _b` shallow aliases.
    //
    // Snag #48: auto-propagate a Result-typed scrutinee in throws
    // context — see `lower_match_expr` for the full rationale (and the
    // arm-pattern gate that skips auto-prop when the user writes
    // Ok/Error/Some/None arms).
    // See `lower_match_expr` for the gate / fallback rationale.
    let user_matches_result_option = super::stmts::arms_match_result_or_option_item(arms);
    let saved_expected = ctx.func_state.expected_type.take();
    ctx.func_state.suppress_auto_prop = user_matches_result_option;
    let scrut_op = lower_expr(ctx, builder, scrutinee);
    let scrut_op = if user_matches_result_option {
        scrut_op
    } else {
        maybe_auto_propagate(ctx, builder, scrut_op, scrutinee.span)
    };
    ctx.func_state.expected_type = saved_expected;
    let scrut_type = infer_operand_type_full(ctx, &scrut_op, builder);
    let source_at_last_use = if let Expr::Identifier(name) = &scrutinee.node {
        ctx.is_last_use_at(name, scrutinee.span)
    } else { false };
    let arms_consume_payload = super::stmts::arms_have_move_extract_items(arms, else_arm);
    let (scrut_local, scrut_type) = super::stmts::stage_match_scrutinee(ctx, builder, &scrut_op, scrut_type, source_at_last_use, arms_consume_payload);

    // Mirror lower_match_expr: size the result slot from expected_type when
    // available (set by an enclosing VarDecl/Assign/Return/arg), otherwise
    // refine from the first non-divergent arm. Snag #29b: previously hardcoded
    // I64 here, so a nested `match` returning a struct/enum miscompiled — the
    // arms wrote `&__sNN` (Ptr) into an I64-sized slot, and the outer match's
    // boundary path then assigned an int64_t into the surrounding enum struct,
    // producing `incompatible types` C errors.
    //
    // Snag #36 follow-up: same Result-wrapper deferral as
    // `lower_match_expr` — see the comment there.
    let result_type_init = ctx.func_state.expected_type.unwrap_or(I64_TYPE);
    let result_local = builder.add_local(result_type_init, None);
    let expected_is_result_wrapper = ctx.func_state.expected_type
        .map_or(false, |t| ctx.type_registry.enum_category(t) == Some(EnumCategory::Result));
    let mut result_type_refined = ctx.func_state.expected_type.is_some()
        && !expected_is_result_wrapper;
    let merge_bb = builder.new_block();

    let concrete_arms: Vec<&ast::MatchArm> = arms.iter().filter_map(|i| i.arm()).collect();
    for (i, arm) in concrete_arms.iter().enumerate() {
        let arm_body_bb = builder.new_block();
        let next_test_bb = if i + 1 < concrete_arms.len() || else_arm.is_some() {
            builder.new_block()
        } else {
            merge_bb
        };

        let cond = super::stmts::lower_pattern_condition(
            ctx, builder, &arm.pattern, scrut_local, scrut_type,
        );
        builder.branch(cond, arm_body_bb, next_test_bb);

        builder.switch_to(arm_body_bb);
        // Snag #50: per-arm name→local snapshot — see `lower_match_expr`
        // for the full rationale (snapshot the name map only, not the
        // builder.locals ownership state, so `assign_match_arm_to_result`'s
        // `set_owned(result_local)` survives the arm boundary).
        let saved_arm_locals = ctx.func_state.locals.clone();
        super::stmts::emit_pattern_bindings(ctx, builder, &arm.pattern, scrut_local, scrut_type);
        let arm_val = lower_expr(ctx, builder, &arm.body);
        // Don't overwrite return/break/continue terminators with jump
        if !builder.is_terminated() {
            if !result_type_refined {
                let arm_ty = infer_operand_type_full(ctx, &arm_val, builder);
                // Skip refinement when the arm produces Unit — refining the
                // result_local to Unit makes its slot lower to LirType::Void
                // (declared as `void *` in C), and a sibling arm storing a
                // primitive value triggers `assignment to 'void *' from
                // 'int32_t'`. Unit-arm matches are statement-form; the
                // result is unused so the I64 default is harmless.
                if arm_ty != UNIT_TYPE {
                    builder.locals[result_local.0 as usize].type_id = arm_ty;
                    result_type_refined = true;
                }
            }
            assign_match_arm_to_result(ctx, builder, result_local, arm_val, arm.body.span);
            builder.jump(merge_bb);
        }
        ctx.func_state.locals = saved_arm_locals;

        if next_test_bb != merge_bb {
            builder.switch_to(next_test_bb);
        }
    }

    if let Some(else_block) = else_arm {
        let saved_else_locals = ctx.func_state.locals.clone();
        let else_val = lower_block_expr(ctx, builder, else_block);
        if !builder.is_terminated() {
            if !result_type_refined {
                let else_ty = infer_operand_type_full(ctx, &else_val, builder);
                if else_ty != UNIT_TYPE {
                    builder.locals[result_local.0 as usize].type_id = else_ty;
                }
            }
            // Use a synthetic span — block expressions don't carry one,
            // and the helper only consults span for the implicit-clone
            // diagnostic (which the else branch rarely triggers anyway).
            let span = crate::span::Span { start: 0, end: 0 };
            assign_match_arm_to_result(ctx, builder, result_local, else_val, span);
            builder.jump(merge_bb);
        }
        ctx.func_state.locals = saved_else_locals;
    } else if !concrete_arms.is_empty() {
        // No else arm. After the loop, builder.current_block is the last
        // arm's body block (because next_test_bb == merge_bb for the last
        // arm, so the post-arm `switch_to(next_test_bb)` is skipped). That
        // body is already terminated — either by `return`/`break`/`continue`
        // emitted inside the arm, or by the post-arm `jump(merge_bb)` the
        // loop emits when not terminated.
        //
        // Snag #33: this branch previously emitted `builder.jump(merge_bb)`
        // unconditionally, which calls `set_terminator` and *overwrites* the
        // already-set Return terminator. The result was a nested
        // match-as-expression where `return X` in the inner arm silently
        // fell through to the outer merge block instead of returning from
        // the function. Gate on `!is_terminated()` so the jump only fires
        // for genuine fallthrough (defensive — current code-paths never
        // hit it). The sibling `lower_match_expr` doesn't have this branch
        // at all; both forms now agree.
        if !builder.is_terminated() {
            builder.jump(merge_bb);
        }
    }

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_local)
}

/// Build a value-producing if-chain from Stmt::If components.
/// Each branch body's last statement is treated as the result expression.
///
/// Sizes `result_id` the same way `lower_match_stmt_as_expr` sizes its
/// `result_local`: from `expected_type` if set (writer-side hint), else
/// refined from the first non-divergent / non-Unit branch. Pre-Snag-#51
/// this was hardcoded to I64, mirroring the bug snag #29b fixed on the
/// match-as-tail side — `if true: "yes" else: "no"` at a String-returning
/// tail position landed in an I64 slot, producing the `Str = int64_t`
/// C type clash. Sizing rule, sibling refinement loop, and Unit-arm skip
/// are kept lockstep with `lower_match_stmt_as_expr` so the two
/// tail-value forms stay layering-coherent.
fn build_if_chain_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
    then_body: &ast::Block,
    elif_branches: &[(Spanned<Expr>, ast::Block)],
    else_body: &Option<ast::Block>,
) -> Operand {
    let cond = lower_expr(ctx, builder, condition);
    let result_type_init = ctx.func_state.expected_type.unwrap_or(I64_TYPE);
    let result_id = builder.add_local(result_type_init, None);
    let expected_is_result_wrapper = ctx.func_state.expected_type
        .map_or(false, |t| ctx.type_registry.enum_category(t) == Some(EnumCategory::Result));
    let mut result_type_refined = ctx.func_state.expected_type.is_some()
        && !expected_is_result_wrapper;
    let merge_bb = builder.new_block();

    let then_bb = builder.new_block();
    let else_bb = builder.new_block();
    builder.branch(cond, then_bb, else_bb);

    // Then branch
    builder.switch_to(then_bb);
    super::stmts::emit_is_bindings(ctx, builder, condition);
    let then_val = lower_block_expr(ctx, builder, then_body);
    if !builder.is_terminated() {
        if !result_type_refined {
            let ty = infer_operand_type_full(ctx, &then_val, builder);
            if ty != UNIT_TYPE {
                builder.locals[result_id.0 as usize].type_id = ty;
                result_type_refined = true;
            }
        }
        assign_match_arm_to_result(ctx, builder, result_id, then_val, then_body.span);
        builder.jump(merge_bb);
    }

    // Elif branches
    let mut current_else_bb = else_bb;
    for (elif_cond, elif_body) in elif_branches {
        builder.switch_to(current_else_bb);
        let elif_cond_val = lower_expr(ctx, builder, elif_cond);
        let elif_then_bb = builder.new_block();
        let next_else_bb = builder.new_block();
        builder.branch(elif_cond_val, elif_then_bb, next_else_bb);

        builder.switch_to(elif_then_bb);
        super::stmts::emit_is_bindings(ctx, builder, elif_cond);
        let elif_val = lower_block_expr(ctx, builder, elif_body);
        if !builder.is_terminated() {
            if !result_type_refined {
                let ty = infer_operand_type_full(ctx, &elif_val, builder);
                if ty != UNIT_TYPE {
                    builder.locals[result_id.0 as usize].type_id = ty;
                    result_type_refined = true;
                }
            }
            assign_match_arm_to_result(ctx, builder, result_id, elif_val, elif_body.span);
            builder.jump(merge_bb);
        }

        current_else_bb = next_else_bb;
    }

    // Else branch
    builder.switch_to(current_else_bb);
    if let Some(else_block) = else_body {
        let else_val = lower_block_expr(ctx, builder, else_block);
        if !builder.is_terminated() {
            if !result_type_refined {
                let ty = infer_operand_type_full(ctx, &else_val, builder);
                if ty != UNIT_TYPE {
                    builder.locals[result_id.0 as usize].type_id = ty;
                    // No more branches after this; no need to track refined state.
                }
            }
            assign_match_arm_to_result(ctx, builder, result_id, else_val, else_block.span);
            builder.jump(merge_bb);
        }
    } else {
        builder.assign(Place::local(result_id), Operand::Constant(Constant::I64(0)));
        builder.jump(merge_bb);
    }

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_id)
}

// ---- P3.5.0: String Interpolation ----

/// Lower an interpolated string literal to `gorget_string_format(fmt, args...)`.
fn lower_string_interpolation(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    lit: &crate::lexer::token::StringLiteral,
    interp_exprs: &[Spanned<Expr>],
) -> Operand {
    let mut format_str = String::new();
    let mut args: Vec<Operand> = Vec::new();

    let mut interp_idx = 0usize;
    for segment in &lit.segments {
        match segment {
            StringSegment::Literal(text) => {
                format_str.push_str(text);
            }
            StringSegment::Interpolation(var_name, fmt_spec) => {
                let pre_parsed = interp_exprs.get(interp_idx);
                interp_idx += 1;
                lower_interp_segment(ctx, builder, var_name, pre_parsed,
                    &mut format_str, &mut args, fmt_spec.as_deref());
            }
        }
    }

    // Emit CallExtern("gorget_string_format", [fmt_str, ...args]) → GorgetString
    let owned_string_type = ctx.type_mapper.owned_string_type;
    let mut all_args = vec![Operand::Constant(Constant::Str(format_str))];
    all_args.extend(args);
    let dst = builder.call_extern("gorget_string_format", all_args, owned_string_type);
    // Register for drop — needs_drop() handles type filtering.
    ctx.drops.register_local(dst, owned_string_type, &ctx.type_registry);
    // gorget_string_format always allocates a fresh buffer — mark as fresh so
    // the self-referential clone guard in assigns.rs skips the redundant clone.
    // Phase D4: typed-only signal — sidecar writer retired.
    ctx.set_owned_fresh(builder, dst);
    FunctionBuilder::copy(dst)
}

/// Clone by-value resource args that must not be moved into a StructInit/
/// EnumInit slot. Specifically: bare borrow params, multi-use named locals,
/// field-access sources, loop-carried named locals, and string views. Owned
/// single-use locals and expression temps pass through — the post-init
/// `move_zero_consumed_args` pass zeroes them for ownership transfer.
///
/// Runs AFTER `ensure_owned_at_boundary`, which already handles the Ptr(T)
/// and ref-state borrow cases.
fn clone_multi_use_resource_args(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    args: &mut Vec<Operand>,
    ast_args: &[Spanned<Expr>],
) {
    // Ptr(resource) borrows are handled by the preceding
    // `ensure_owned_at_boundary` call in `lower_struct_init`. This pass only
    // handles the by-value multi-use / field-access / loop-carried / string-view
    // cases that need a clone even though the local isn't in a ref ownership
    // state.
    for (i, op) in args.iter_mut().enumerate() {
        if let Operand::Copy(place) = op {
            if place.projections.is_empty() {
                let local = place.local;
                let local_type = builder.local_type(local);

                // SCOUT-PROTO #1b (Defect B): refcount handles (Shared/Weak/
                // Channel) are NOT `is_resource_type` (thin-pointer, Trivial
                // copy) but STILL need clone-if-live at a consuming position —
                // their "clone" is a by-value incref. Admit them here.
                if is_resource_type_local(local, builder, &ctx.type_registry)
                    || ctx.type_registry.is_refcount_clone_type(local_type)
                {
                    // Already owned (call results, cloned temps) — skip
                    if ctx.is_owned_local(builder, local) && !ctx.is_named_local(local) {
                        continue;
                    }
                    // String views (non-owned) ALWAYS need clone for struct/enum storage.
                    let is_non_owned_string = ctx.is_string_type(local_type)
                        && !ctx.is_owned_local(builder, local);
                    // Must clone if: bare borrow param, multi-use named local,
                    // field access on a struct, or non-owned string view.
                    let is_borrow_param = ctx.is_bare_param(builder, local);
                    let is_field_access = ast_args.get(i)
                        .map(|arg| matches!(&arg.node, Expr::FieldAccess { .. }))
                        .unwrap_or(false);
                    let is_multi_use = ast_args.get(i)
                        .and_then(|arg| if let Expr::Identifier(name) = &arg.node {
                            Some(!ctx.is_last_use_at(name, arg.span))
                        } else { None })
                        .unwrap_or(false);
                    // In a loop body, named locals declared BEFORE the loop are
                    // effectively multi-use (each iteration reads the same local).
                    // Locals declared INSIDE the loop are fresh each iteration —
                    // they can be safely moved on last use.
                    let in_loop = ctx.current_loop().is_some();
                    let is_named_in_loop = in_loop && ctx.is_named_local(local)
                        && !ctx.is_loop_body_local(local);
                    // Untracked locals have unknown ownership — clone conservatively.
                    // Named owned locals with no span info fall through is_multi_use=false
                    // when the arg isn't a plain Identifier; clone them too.
                    let is_untracked = matches!(
                        builder.locals.get(local.0 as usize).map(|l| &l.ownership),
                        Some(crate::ir::LocalOwnership::Untracked)
                    );
                    let is_named_no_span = ctx.is_named_local(local)
                        && ast_args.get(i).map(|arg| !matches!(&arg.node, Expr::Identifier(_))).unwrap_or(true);
                    if is_borrow_param || is_multi_use || is_field_access || is_named_in_loop || is_non_owned_string || is_untracked || is_named_no_span {
                        // T-A: owning `!` param deref-temp at its single-use last use
                        // MOVES into the ctor field instead of cloning (user enums /
                        // struct-literal by-value path — sibling site 3 of 3, shared
                        // `maybe_move_owning_param_ctor_temp`). Uses the arg span for
                        // last-use.
                        let move_span = ast_args.get(i).map(|a| a.span)
                            .unwrap_or(crate::span::Span { start: 0, end: 0 });
                        if let Some(moved) = ctx.maybe_move_owning_param_ctor_temp(builder, &*op, move_span) {
                            *op = moved;
                            continue;
                        }
                        let inner_type = ctx.pointee_type(local_type).unwrap_or(local_type);
                        // SCOUT-PROTO #1b (Defect B): a REFCOUNT handle
                        // (Shared/Weak/Channel — thin-pointer, copy_semantics
                        // Trivial, `{name}__clone` = a by-VALUE incref) is
                        // cloned by passing the handle directly, NOT by taking
                        // its address (deep-clone fns like gorget_array_clone
                        // are by-pointer). Detect via the typed clone_fn +
                        // Trivial-copy shape.
                        let is_refcount_clone = ctx.pointee_type(local_type).is_none()
                            && ctx.type_registry.is_refcount_clone_type(local_type);
                        if let Some(clone_fn) = ctx.clone_fn_for_ptr(inner_type) {
                            if let Some(span) = ast_args.get(i).map(|a| a.span) {
                                ctx.warn_clone_and_hit(builder, span, inner_type, crate::ir::ImplicitCloneReason::ConsumingArg);
                            }
                            let clone_arg = if ctx.pointee_type(local_type).is_some() || is_refcount_clone {
                                // Already Ptr, OR a refcount handle cloned
                                // by-value — pass directly.
                                FunctionBuilder::copy(local)
                            } else {
                                let ptr_type = ctx.register_ptr_type(inner_type);
                                let ptr = builder.add_local(ptr_type, None);
                                builder.emit_borrow(ptr, crate::ir::instructions::Place::local(local));
                                FunctionBuilder::copy(ptr)
                            };
                            let cloned = builder.call_clone(&clone_fn, vec![clone_arg], inner_type, crate::ir::ImplicitCloneReason::ConsumingArg);
                            ctx.drops.register_local(cloned, inner_type, &ctx.type_registry);
                            // Tier 2a Phase 2A: clone temps own a fresh
                            // heap allocation, so tag FreshOwned. Mirrors
                            // the same fix in
                            // `clone_resource_args_for_init`.
                            ctx.set_owned_fresh(builder, cloned);
                            *op = FunctionBuilder::copy(cloned);
                        }
                    }
                }
            }
        }
    }
}

/// MoveZero resource-type operands AFTER StructInit/EnumInit.
/// Single-use/temp sources are zeroed (zero-cost transfer). Multi-use sources
/// that were cloned by clone_multi_use_resource_args are already replaced —
/// the clone local gets MoveZero'd (it's single-use by definition).
fn move_zero_consumed_args(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    args: &[Operand],
) {
    for op in args {
        if let Operand::Copy(place) = op {
            if place.projections.is_empty() {
                let is_resource = is_resource_type_local(place.local, builder, &ctx.type_registry);
                let is_string = builder.local_type(place.local) == ctx.type_mapper.owned_string_type;
                // SCOUT-PROTO #1b (Defect B): a refcount handle transferred
                // into the struct on the MOVE path (dead source, not cloned)
                // must be move-zeroed too, or its scope-exit drop double-decs
                // the control block the struct field now owns.
                let is_refcount = ctx.type_registry.is_refcount_clone_type(builder.local_type(place.local));
                if (is_resource || is_string || is_refcount) && !ctx.drops.is_moved(place.local) {
                    ctx.move_zero_and_mark(builder, place.local);
                }
            }
        }
    }
}

/// Register a GorgetString temp for drop at function scope.
/// Uses function scope (not block scope) because `str` views into the GorgetString
/// Register a GorgetString temp for drop at the current block scope.
/// Callers that consume the temp for str views (VarDecl, Assign, field assign)
/// must call `ctx.drops.unregister()` to prevent use-after-free.
/// Callers that consume the temp for String variables must call `mark_moved()`
/// to prevent double-free.
///
/// Note: block scope means loop-body temps are freed each iteration (good),
/// but temps whose str views escape the block (e.g. passed to functions that
/// store the view in structs) will cause use-after-free. Those call sites
/// need to use `String` parameters instead of `str`.
/// Check whether GorgetString temps should be unregistered (leaked) for a call.
/// Infer the GIR type of an operand by examining its structure.
/// Register (or reuse) a Tuple TypeDef for the given element types.
/// Infer operand type using both ctx locals and builder locals.
/// This handles compiler temporaries (tuples, struct inits, etc.) that aren't in ctx.func_state.locals.
/// Extract the local ID from an operand if it's a simple local reference.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::types::TypeRegistry;
    use crate::lexer::token::{StringKind, StringLiteral};
    use crate::parser::ast::CallArg;
    use crate::span::Span;

    fn spanned<T>(node: T) -> Spanned<T> {
        Spanned {
            node,
            span: Span { start: 0, end: 0 },
        }
    }

    fn make_test_ctx() -> (crate::semantic::AnalysisResult, LoweringContext<'static>) {
        // We need a 'static AnalysisResult to satisfy lifetime requirements.
        // Use a leaked box for tests only.
        let analysis = Box::leak(Box::new(crate::ir::lowering::empty_analysis_for_test()));
        let mut reg = TypeRegistry::new();
        let mapper = super::super::types::TypeMapper::new(&mut reg);
        let ctx = LoweringContext::new(analysis, mapper, reg);
        // Return a dummy analysis (not used) and the context
        (crate::ir::lowering::empty_analysis_for_test(), ctx)
    }

    #[test]
    fn lower_literals() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let int_op = lower_expr(&mut ctx, &mut builder, &spanned(Expr::IntLiteral(42)));
        assert!(matches!(int_op, Operand::Constant(Constant::I64(42))));

        let float_op = lower_expr(&mut ctx, &mut builder, &spanned(Expr::FloatLiteral(3.14)));
        assert!(matches!(float_op, Operand::Constant(Constant::F64(f)) if (f - 3.14).abs() < 1e-10));

        let bool_op = lower_expr(&mut ctx, &mut builder, &spanned(Expr::BoolLiteral(true)));
        assert!(matches!(bool_op, Operand::Constant(Constant::Bool(true))));

        let str_op = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::StringLiteral(StringLiteral {
                kind: StringKind::Normal,
                segments: vec![StringSegment::Literal("hello".into())],
            }, Vec::new())),
        );
        assert!(matches!(str_op, Operand::Constant(Constant::Str(ref s)) if s == "hello"));
    }

    #[test]
    fn lower_binary_op_test() {
        let (_analysis, mut ctx) = make_test_ctx();
        let a_id = LocalId(1);
        let b_id = LocalId(2);
        ctx.register_local("a", a_id, I64_TYPE);
        ctx.register_local("b", b_id, I64_TYPE);

        let mut builder = FunctionBuilder::new(
            "test",
            I64_TYPE,
            &[(I64_TYPE, Some("a")), (I64_TYPE, Some("b"))],
        );

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::BinaryOp {
                left: Box::new(spanned(Expr::Identifier("a".into()))),
                op: ast::BinaryOp::Add,
                right: Box::new(spanned(Expr::Identifier("b".into()))),
            }),
        );

        assert!(matches!(result, Operand::Copy(_)));
        assert_eq!(builder.blocks[0].instructions.len(), 1);
        assert!(matches!(
            builder.blocks[0].instructions[0],
            Instruction::BinOp { op: BinOp::Add, .. }
        ));
    }

    #[test]
    fn lower_print_interpolation() {
        let (_analysis, mut ctx) = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let lit = StringLiteral {
            kind: StringKind::Normal,
            segments: vec![StringSegment::Interpolation("x".into(), None)],
        };
        let args = vec![spanned(CallArg {
            name: None,
            ownership: ast::Ownership::Borrow,
            value: spanned(Expr::StringLiteral(lit, Vec::new())),
        })];

        lower_print_call(&mut ctx, &mut builder, &args);

        assert!(!builder.blocks[0].instructions.is_empty());
        assert!(matches!(
            builder.blocks[0].instructions.last().unwrap(),
            Instruction::CallExtern { func, .. } if func == "printf"
        ));
    }

    // ---- P3.2: Match expression tests ----

    #[test]
    fn lower_match_expr_literal() {
        let (_analysis, mut ctx) = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", I64_TYPE, &[(I64_TYPE, Some("x"))]);

        use crate::parser::ast::{MatchArm, Pattern};

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::Match {
                scrutinee: Box::new(spanned(Expr::Identifier("x".into()))),
                arms: vec![
                    MatchArm {
                        pattern: spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(1))))),
                        guard: None,
                        body: spanned(Expr::IntLiteral(10)),
                        span: Span { start: 0, end: 0 },
                    },
                    MatchArm {
                        pattern: spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(2))))),
                        guard: None,
                        body: spanned(Expr::IntLiteral(20)),
                        span: Span { start: 0, end: 0 },
                    },
                ],
                else_arm: Some(Box::new(spanned(Expr::IntLiteral(0)))),
            }),
        );

        // Result should be a Copy of the result local
        assert!(matches!(result, Operand::Copy(_)));

        // Should have Branch terminators for pattern checks
        let has_branch = builder.blocks.iter().any(|bb| {
            matches!(bb.terminator, Some(Terminator::Branch { .. }))
        });
        assert!(has_branch, "Match expr should have Branch terminators");

        // Should have Assign to result local in arm bodies
        let assign_count: usize = builder.blocks.iter()
            .map(|bb| bb.instructions.iter()
                .filter(|inst| matches!(inst, Instruction::Assign { .. }))
                .count())
            .sum();
        assert!(assign_count >= 3, "Should have assigns for scrutinee + arms");
    }

    // ---- P3.4: Miscellaneous expression tests ----

    #[test]
    fn lower_self_expr() {
        let (_analysis, mut ctx) = make_test_ctx();
        let self_id = LocalId(1);
        ctx.register_local("self", self_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("self"))]);

        let result = lower_expr(&mut ctx, &mut builder, &spanned(Expr::SelfExpr));
        assert!(matches!(result, Operand::Copy(ref p) if p.local == LocalId(1)));
    }

    #[test]
    fn lower_block_expr_test() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", I64_TYPE, &[]);

        use crate::parser::ast::{Block, Stmt};

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::Block(Block {
                stmts: vec![spanned(Stmt::Expr(spanned(Expr::IntLiteral(42))))],
                span: Span { start: 0, end: 0 },
            })),
        );

        // The block's last expression (42) should be the value
        assert!(matches!(result, Operand::Constant(Constant::I64(42))));
    }

    #[test]
    fn lower_cast_expr() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::As {
                expr: Box::new(spanned(Expr::IntLiteral(42))),
                type_: spanned(ast::Type::Primitive(ast::PrimitiveType::Float)),
            }),
        );

        // Should produce a Copy of the cast result local
        assert!(matches!(result, Operand::Copy(_)));
        // Should have a Cast instruction
        let has_cast = builder.blocks[0].instructions.iter().any(|inst| {
            matches!(inst, Instruction::Cast { .. })
        });
        assert!(has_cast, "Should have Cast instruction for 'as' expression");
    }

    #[test]
    fn lower_tuple_literal() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::TupleLiteral(vec![
                spanned(Expr::IntLiteral(1)),
                spanned(Expr::IntLiteral(2)),
                spanned(Expr::IntLiteral(3)),
            ])),
        );

        assert!(matches!(result, Operand::Copy(_)));
        // Should have a TupleInit instruction
        let has_tuple_init = builder.blocks[0].instructions.iter().any(|inst| {
            matches!(inst, Instruction::TupleInit { .. })
        });
        assert!(has_tuple_init, "Should have TupleInit instruction");
    }

    #[test]
    fn lower_is_expr() {
        let (_analysis, mut ctx) = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        use crate::parser::ast::Pattern;

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::Is {
                expr: Box::new(spanned(Expr::Identifier("x".into()))),
                negated: false,
                pattern: spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(5))))),
            }),
        );

        // Should produce a boolean condition (Copy of Cmp result)
        assert!(matches!(result, Operand::Copy(_)));
    }

    #[test]
    fn lower_none_literal() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(&mut ctx, &mut builder, &spanned(Expr::NoneLiteral));
        assert!(matches!(result, Operand::Constant(Constant::Null)));
    }

    // ---- P3.5.0: String Interpolation ----

    #[test]
    fn lower_plain_string_stays_constant() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::StringLiteral(StringLiteral {
                kind: StringKind::Normal,
                segments: vec![StringSegment::Literal("hello".into())],
            }, Vec::new())),
        );
        assert!(
            matches!(result, Operand::Constant(Constant::Str(ref s)) if s == "hello"),
            "Plain string should stay as Constant::Str"
        );
    }

    #[test]
    fn lower_interpolated_string_calls_format() {
        let (_analysis, mut ctx) = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::StringLiteral(StringLiteral {
                kind: StringKind::Normal,
                segments: vec![
                    StringSegment::Literal("value: ".into()),
                    StringSegment::Interpolation("x".into(), None),
                ],
            }, Vec::new())),
        );
        // Should return Copy (of the gorget_string_format result local)
        assert!(matches!(result, Operand::Copy(_)));
        // Should have a CallExtern to gorget_string_format
        let has_format = builder.blocks[0].instructions.iter().any(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func == "gorget_string_format")
        });
        assert!(has_format, "Interpolated string should call gorget_string_format");
    }

    // ---- P3.5.1: Array Literals ----

    #[test]
    fn lower_array_literal_nonempty() {
        let (_analysis, mut ctx) = make_test_ctx();
        // Register GorgetArray type
        let array_type = ctx.type_registry.insert(GirType::Named("GorgetArray".to_string()));
        ctx.type_mapper.register_named("GorgetArray".to_string(), array_type);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::ArrayLiteral(vec![
                spanned(Expr::IntLiteral(1)),
                spanned(Expr::IntLiteral(2)),
                spanned(Expr::IntLiteral(3)),
            ])),
        );
        assert!(matches!(result, Operand::Copy(_)));
        // Count gorget_array_new + gorget_array_push calls
        let all_insts: Vec<_> = builder.blocks.iter()
            .flat_map(|bb| bb.instructions.iter())
            .collect();
        let new_count = all_insts.iter().filter(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func == "gorget_array_new")
        }).count();
        let push_count = all_insts.iter().filter(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func == "gorget_array_push")
        }).count();
        assert_eq!(new_count, 1, "Should have 1 gorget_array_new call");
        assert_eq!(push_count, 3, "Should have 3 gorget_array_push calls");
    }

    #[test]
    fn lower_array_literal_empty() {
        let (_analysis, mut ctx) = make_test_ctx();
        let array_type = ctx.type_registry.insert(GirType::Named("GorgetArray".to_string()));
        ctx.type_mapper.register_named("GorgetArray".to_string(), array_type);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::ArrayLiteral(vec![])),
        );
        assert!(matches!(result, Operand::Copy(_)));
        let has_new = builder.blocks[0].instructions.iter().any(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func == "gorget_array_new")
        });
        assert!(has_new, "Empty array should still call gorget_array_new");
        let push_count: usize = builder.blocks.iter()
            .flat_map(|bb| bb.instructions.iter())
            .filter(|inst| matches!(inst, Instruction::CallExtern { func, .. } if func == "gorget_array_push"))
            .count();
        assert_eq!(push_count, 0, "Empty array should have no push calls");
    }

    // ---- P3.5.2: Dict Literals ----

    #[test]
    fn lower_dict_literal_nonempty() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::DictLiteral(vec![
                (spanned(Expr::IntLiteral(1)), spanned(Expr::IntLiteral(10))),
                (spanned(Expr::IntLiteral(2)), spanned(Expr::IntLiteral(20))),
            ])),
        );
        assert!(matches!(result, Operand::Copy(_)));
        let all_insts: Vec<_> = builder.blocks.iter()
            .flat_map(|bb| bb.instructions.iter())
            .collect();
        // Should have a __new call
        let new_count = all_insts.iter().filter(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func.ends_with("__new"))
        }).count();
        assert_eq!(new_count, 1, "Should have 1 dict __new call");
        // Should have 2 __put calls
        let put_count = all_insts.iter().filter(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func.ends_with("__put"))
        }).count();
        assert_eq!(put_count, 2, "Should have 2 dict __put calls");
    }

    // ---- P3.5.3: List Comprehensions ----

    #[test]
    fn lower_list_comprehension_range() {
        let (_analysis, mut ctx) = make_test_ctx();
        let array_type = ctx.type_registry.insert(GirType::Named("GorgetArray".to_string()));
        ctx.type_mapper.register_named("GorgetArray".to_string(), array_type);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        use crate::parser::ast::{Ownership, Pattern};

        // [x * x for x in 0..5]
        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::ListComprehension {
                expr: Box::new(spanned(Expr::BinaryOp {
                    left: Box::new(spanned(Expr::Identifier("x".into()))),
                    op: ast::BinaryOp::Mul,
                    right: Box::new(spanned(Expr::Identifier("x".into()))),
                })),
                variable: spanned(Pattern::Binding("x".into())),
                ownership: Ownership::Borrow,
                iterable: Box::new(spanned(Expr::Range {
                    start: Some(Box::new(spanned(Expr::IntLiteral(0)))),
                    end: Some(Box::new(spanned(Expr::IntLiteral(5)))),
                    inclusive: false,
                })),
                condition: None,
            }),
        );
        assert!(matches!(result, Operand::Copy(_)));
        // Should have gorget_array_new + gorget_array_push
        let all_insts: Vec<_> = builder.blocks.iter()
            .flat_map(|bb| bb.instructions.iter())
            .collect();
        let has_new = all_insts.iter().any(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func == "gorget_array_new")
        });
        let has_push = all_insts.iter().any(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func == "gorget_array_push")
        });
        assert!(has_new, "List comprehension should have gorget_array_new");
        assert!(has_push, "List comprehension should have gorget_array_push");
        // Should have a Cmp (loop condition) and Branch
        let has_cmp = builder.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| matches!(inst, Instruction::Cmp { .. }))
        });
        assert!(has_cmp, "List comprehension should have loop condition Cmp");
    }

    #[test]
    fn lower_list_comprehension_with_filter() {
        let (_analysis, mut ctx) = make_test_ctx();
        let array_type = ctx.type_registry.insert(GirType::Named("GorgetArray".to_string()));
        ctx.type_mapper.register_named("GorgetArray".to_string(), array_type);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        use crate::parser::ast::{Ownership, Pattern};

        // [x for x in 0..10 if x > 5]
        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::ListComprehension {
                expr: Box::new(spanned(Expr::Identifier("x".into()))),
                variable: spanned(Pattern::Binding("x".into())),
                ownership: Ownership::Borrow,
                iterable: Box::new(spanned(Expr::Range {
                    start: Some(Box::new(spanned(Expr::IntLiteral(0)))),
                    end: Some(Box::new(spanned(Expr::IntLiteral(10)))),
                    inclusive: false,
                })),
                condition: Some(Box::new(spanned(Expr::BinaryOp {
                    left: Box::new(spanned(Expr::Identifier("x".into()))),
                    op: ast::BinaryOp::Gt,
                    right: Box::new(spanned(Expr::IntLiteral(5))),
                }))),
            }),
        );
        assert!(matches!(result, Operand::Copy(_)));
        // Should have at least 2 Branch terminators (loop condition + filter)
        let branch_count = builder.blocks.iter()
            .filter(|bb| matches!(bb.terminator, Some(Terminator::Branch { .. })))
            .count();
        assert!(branch_count >= 2, "Should have >= 2 Branch terminators (loop + filter), got {branch_count}");
    }

    // ---- P3.5.4: Dict and Set Comprehensions ----

    #[test]
    fn lower_dict_comprehension_range() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        // {x: x * 10 for x in 0..3}
        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::DictComprehension {
                key: Box::new(spanned(Expr::Identifier("x".into()))),
                value: Box::new(spanned(Expr::BinaryOp {
                    left: Box::new(spanned(Expr::Identifier("x".into()))),
                    op: ast::BinaryOp::Mul,
                    right: Box::new(spanned(Expr::IntLiteral(10))),
                })),
                variables: vec![spanned("x".to_string())],
                iterable: Box::new(spanned(Expr::Range {
                    start: Some(Box::new(spanned(Expr::IntLiteral(0)))),
                    end: Some(Box::new(spanned(Expr::IntLiteral(3)))),
                    inclusive: false,
                })),
                condition: None,
            }),
        );
        assert!(matches!(result, Operand::Copy(_)));
        let all_insts: Vec<_> = builder.blocks.iter()
            .flat_map(|bb| bb.instructions.iter())
            .collect();
        let has_new = all_insts.iter().any(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func.ends_with("__new"))
        });
        let has_put = all_insts.iter().any(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func.ends_with("__put"))
        });
        assert!(has_new, "Dict comprehension should have __new call");
        assert!(has_put, "Dict comprehension should have __put call");
    }

    #[test]
    fn lower_set_comprehension_with_filter() {
        let (_analysis, mut ctx) = make_test_ctx();
        let array_type = ctx.type_registry.insert(GirType::Named("GorgetArray".to_string()));
        ctx.type_mapper.register_named("GorgetArray".to_string(), array_type);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        // {x for x in 0..10 if x > 5}
        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::SetComprehension {
                expr: Box::new(spanned(Expr::Identifier("x".into()))),
                variable: spanned("x".to_string()),
                iterable: Box::new(spanned(Expr::Range {
                    start: Some(Box::new(spanned(Expr::IntLiteral(0)))),
                    end: Some(Box::new(spanned(Expr::IntLiteral(10)))),
                    inclusive: false,
                })),
                condition: Some(Box::new(spanned(Expr::BinaryOp {
                    left: Box::new(spanned(Expr::Identifier("x".into()))),
                    op: ast::BinaryOp::Gt,
                    right: Box::new(spanned(Expr::IntLiteral(5))),
                }))),
            }),
        );
        assert!(matches!(result, Operand::Copy(_)));
        // Should have condition branch
        let branch_count = builder.blocks.iter()
            .filter(|bb| matches!(bb.terminator, Some(Terminator::Branch { .. })))
            .count();
        assert!(branch_count >= 2, "Set comprehension with filter should have >= 2 branches");
    }

    // ---- P3.5.6: Optional Chaining ----

    #[test]
    fn lower_optional_chain_produces_branch() {
        let (_analysis, mut ctx) = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::OptionalChain {
                object: Box::new(spanned(Expr::Identifier("x".into()))),
                field: spanned("field".to_string()),
            }),
        );
        assert!(matches!(result, Operand::Copy(_)));
        // Should have a Cmp (not null check) and Branch
        let has_cmp = builder.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| matches!(inst, Instruction::Cmp { op: CmpOp::Ne, .. }))
        });
        assert!(has_cmp, "Optional chain should have a Ne comparison");
        let has_branch = builder.blocks.iter().any(|bb| {
            matches!(bb.terminator, Some(Terminator::Branch { .. }))
        });
        assert!(has_branch, "Optional chain should have a Branch");
        // Null path should assign Null
        let has_null_assign = builder.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| {
                matches!(inst, Instruction::Assign { value: Operand::Constant(Constant::Null), .. })
            })
        });
        assert!(has_null_assign, "Optional chain should assign Null on else path");
    }

    // ---- P3.5.7: Range Expressions ----

    #[test]
    fn lower_range_expr_produces_struct_init() {
        let (_analysis, mut ctx) = make_test_ctx();
        // Register GorgetRange type
        let range_def = crate::ir::types::TypeDef {
            name: "GorgetRange".to_string(),
            kind: crate::ir::types::TypeDefKind::Struct(crate::ir::types::StructDef {
                fields: vec![
                    crate::ir::types::StructField { name: "start".to_string(), type_id: I64_TYPE },
                    crate::ir::types::StructField { name: "end".to_string(), type_id: I64_TYPE },
                    crate::ir::types::StructField { name: "inclusive".to_string(), type_id: BOOL_TYPE },
                ],
            }),
            metadata: crate::ir::types::TypeMetadata::default(),
        };
        ctx.type_registry.add_type_def(range_def);
        let range_type = ctx.type_registry.insert(GirType::Named("GorgetRange".to_string()));
        ctx.type_mapper.register_named("GorgetRange".to_string(), range_type);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::Range {
                start: Some(Box::new(spanned(Expr::IntLiteral(0)))),
                end: Some(Box::new(spanned(Expr::IntLiteral(10)))),
                inclusive: false,
            }),
        );
        assert!(matches!(result, Operand::Copy(_)));
        let has_struct_init = builder.blocks[0].instructions.iter().any(|inst| {
            matches!(inst, Instruction::StructInit { type_name, .. } if type_name == "GorgetRange")
        });
        assert!(has_struct_init, "Range expr should produce a StructInit for GorgetRange");
    }
}
