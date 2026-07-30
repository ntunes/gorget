//! Function call lowering, argument resolution, and print/string interpolation.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::lexer::token::StringSegment;
use crate::parser::ast::{self, Expr, Ownership};
use crate::parser::Parser;
use crate::span::Spanned;

use super::super::context::{LoweringContext, ParamABI};
use super::{lower_expr, infer_operand_type_full,
            ensure_box_type_def, ensure_mutex_type_def, ensure_shared_type_def,
            ensure_task_group_type_def, get_or_register_type,
            resolve_option_result_variant, lower_string_interpolation};

pub(super) fn lower_call_arg(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    arg: &Spanned<ast::CallArg>,
    callee_param_type: Option<TypeId>,
    callee_name: &str,
    arg_idx: usize,
) -> Operand {
    // Look up the unified ParamABI (single source of truth when available).
    let abi = ctx.fn_param_abis.get(callee_name)
        .and_then(|abis| abis.get(arg_idx))
        .copied();

    // Whether the callee's parameter is a Move type (passed by pointer).
    // Use ParamABI when available, fall back to type-based derivation for extern/runtime fns.
    let callee_is_move_param = match abi {
        Some(abi) => matches!(abi, ParamABI::ByPtr | ParamABI::ByMutPtr),
        None => callee_param_type.map(|pt| ctx.type_registry.is_resource_type(pt)).unwrap_or(false),
    };

    // The callee expects a pointer for this param.
    let callee_passes_by_ptr = match abi {
        Some(abi) => abi != ParamABI::ByValue,
        None => {
            let callee_param_ownership = ctx.fn_param_ownerships.get(callee_name)
                .and_then(|ownerships| ownerships.get(arg_idx))
                .copied();
            let callee_param_is_mut_borrow = callee_param_ownership
                .map(|o| matches!(o, Ownership::MutableBorrow))
                .unwrap_or(false);
            callee_is_move_param || callee_param_is_mut_borrow
        }
    };

    // Special case: &name where name is already a pass-by-pointer param.
    // Skip the auto-deref that Identifier would do — just forward the pointer.
    // Only forward when the call site explicitly has & — bare args must not
    // silently forward a MutPtr.
    if matches!(arg.node.ownership, Ownership::MutableBorrow) {
        if let Expr::Identifier(name) = &arg.node.value.node {
            if let Some((local_id, _)) = ctx.lookup_local(name) {
                // CoW `&`-of-a-bare-value FORMATION (G2, site 1): materialize a
                // private copy of a bare param / bare alias so the callee's
                // write-through lands on the copy, not the shared source.
                ctx.cow_before_mutation(builder, local_id, arg.span);
                // Re-resolve: cow_before_mutation may have rebound `name` to a
                // freshly-materialized OWNED local. Forwarding the stale
                // `local_id` (the pre-materialize borrowed Ptr) would write
                // through to the source buffer and orphan the copy.
                let local_id = ctx.lookup_local(name).map(|(l, _)| l).unwrap_or(local_id);
                let is_already_ptr = {
                    let lid = local_id.0 as usize;
                    lid < builder.locals.len() && matches!(
                        ctx.type_registry.get(builder.locals[lid].type_id),
                        Some(GirType::MutPtr(_)) | Some(GirType::Ptr(_))
                    )
                };
                if ctx.is_ref_local(builder, local_id)
                    || ctx.is_param_borrow_unique(builder, local_id)
                    || is_already_ptr
                {
                    return FunctionBuilder::copy(local_id);
                }
            }
        }
    }

    // Track B1 A-2 (Option (b), 2026-07-27): bare-identifier arg forwarded to a
    // callee that expects pass-by-`&` (`callee_passes_by_ptr`). If the source
    // local is a `&`-param whose stored value IS the caller's `*mut T`
    // (`is_param_borrow_unique`), forward that pointer directly — the
    // `Ownership::Borrow` arm below would first `lower_expr` the identifier,
    // which for a `&`-param AUTO-DEREFS into a value-typed temp, losing the
    // pointer and re-borrowing a dying stack temp instead. Two indirect-call
    // arg-emit loops (`__callable_N` UNIT_TYPE + `__gorget_closure_call_N`
    // FnPtr) used to compensate by SKIPPING `lower_call_arg` entirely for this
    // exact shape (the pre-Option-(b) shortcut); routing them through
    // `lower_call_arg` requires this fast-path to preserve their observation
    // AND, unlike the shortcut, run `cow_before_mutation` on the source so
    // aliases of the caller's slot are severed before the callee's
    // write-through fires. Mirrors the `MutableBorrow` special-case above —
    // that arm handles `f(&c)`; this arm handles `f(c)` when `f` is
    // `Callable[void(&T)]` (indirect) or `void f(T &x)` (direct D31 bare-arg).
    // The plain-local case (`int a = 5; cb(a)`) is UNAFFECTED: it falls
    // through to the `Ownership::Borrow if callee_passes_by_ptr` arm below,
    // which emits a fresh `borrow` on the int slot — exactly right.
    if matches!(arg.node.ownership, Ownership::Borrow) && callee_passes_by_ptr {
        if let Expr::Identifier(name) = &arg.node.value.node {
            if let Some((local_id, _)) = ctx.lookup_local(name) {
                if ctx.is_param_borrow_unique(builder, local_id) {
                    ctx.cow_before_mutation(builder, local_id, arg.span);
                    // Re-resolve in case cow_before_mutation rebound `name` to a
                    // freshly-materialized owned local; forwarding the stale
                    // pre-materialize borrow would escape the caller's slot.
                    let local_id = ctx.lookup_local(name).map(|(l, _)| l).unwrap_or(local_id);
                    if ctx.is_param_borrow_unique(builder, local_id) {
                        return FunctionBuilder::copy(local_id);
                    }
                }
            }
        }
    }

    // Special case: !name where name is a `!`-sigil resource parameter (the
    // local already holds a MutPtr to caller-owned data). Forward the pointer
    // directly and emit MoveZero on the param slot, bypassing the
    // Identifier-path's deref-into-temp + memcpy. Without this, the temp
    // and the caller's R buffer would alias the same heap data; both the
    // inner callee's exit drop (on the temp's transferred ownership) and
    // this function's exit drop (on its own `!`-param) would fire,
    // double-freeing the resource.
    //
    // Detection: `is_owning_param` is the typed bit set at param
    // registration for `Ownership::Move` resource params. The flag drives
    // both this fast-path and the `lower_drop` deref-aware emission, so
    // there's no name-matching or shape inference downstream.
    if matches!(arg.node.ownership, Ownership::Move) {
        if let Expr::Identifier(name) = &arg.node.value.node {
            if let Some((local_id, _)) = ctx.lookup_local(name) {
                let is_owning_param = (local_id.0 as usize) < builder.locals.len()
                    && builder.locals[local_id.0 as usize].is_owning_param;
                // Guard: a loop-carried accumulator (`x = f(!x)`) must NOT take the
                // pointer-forward + whole-slot MoveZero fast-path. The `move_zero _p`
                // here marks the reused pointer slot dead, then the back-edge
                // reassignment `_p.* = ...` reads _p and trips the GIR
                // "read after MoveZero" validator. (`move_zero` on a bare pointer
                // slot lowers to a MoveSlot annotation, so the emitted code is
                // runtime-benign, but the GIR is genuinely inconsistent — it claims
                // _p dead then uses it — and validate.rs correctly rejects it.) The
                // detection: we are lowering inside a loop AND `name` is the target
                // of an assignment somewhere in a loop body (`loop_reassigned_names`,
                // pre-scanned). Such moves fall through to the temp-materialize path
                // (identical to the CoW auto-move of a bare arg), which is correct
                // for the reassigned shape. The fast-path is KEPT for every non-loop
                // onward-move (`consume(!r)`, `out.push(!item)`) — those need the
                // pointer-forward + MoveSlot to suppress the exit-drop double-free
                // that the temp-materialize path would cause with no reassignment.
                // Sibling of `maybe_move_owning_param_ctor_temp`'s guard (Core #4).
                let loop_carried_reassign = ctx.current_loop().is_some()
                    && ctx.func_state.loop_reassigned_names.contains(name.as_str());
                if is_owning_param && !loop_carried_reassign {
                    // Sever any CoW aliases of the source slot before transfer.
                    ctx.cow_before_mutation(builder, local_id, arg.span);
                    // Forward the pointer (the local holds a MutPtr already).
                    // Schedule a post-call MoveZero on the param slot so the
                    // exit drop's flag flips to false — this function no
                    // longer owns the pointee.
                    ctx.drops.mark_moved(local_id);
                    ctx.func_state.pending_move_zeros.push(local_id);
                    return FunctionBuilder::copy(local_id);
                }
            }
        }
    }
    // (Snag #26's `mutate(&*box)` Deref-lvalue block used to sit HERE; it is now
    // the `Expr::Deref` arm of the shared `try_resolve_place` producer invoked
    // below, so `&*b` and `&b.fd` take ONE path instead of two. Order is
    // preserved: the G2 block immediately below explicitly excludes
    // `Expr::Deref` from root-materialize, so a `&*b` arg still reaches the
    // producer with `g2_projected_untrack_start == None`, exactly as before.)
    // CoW `&`-of-a-PROJECTED-bare-value FORMATION (G2, site 3, call-arg form):
    // `f(&s.field)`, `f(&arr[i])` where the projection ROOT is a bare param /
    // bare alias. Materialize the root BEFORE the SINGLE lowering of the arg
    // (below) so the projection re-reads out of the private owned copy and the
    // callee's write-through lands there, not on the shared source. Mirrors the
    // G1 method-receiver root-materialize (`methods.rs`) — same UAF-fold class,
    // so the transient element/field handles the projection mints MUST be
    // untracked (a leftover CowBorrow into the private copy would Case-3-dangle
    // on a later same-collection push). The Identifier / Deref shapes are
    // handled by the whole-value blocks above; only genuine projections reach
    // here. Materialize-before-single-lower (NOT lower→materialize→re-lower)
    // keeps a side-effecting index (`&arr[side_effect()]`) evaluated once.
    let mut g2_projected_untrack_start: Option<usize> = None;
    if matches!(arg.node.ownership, Ownership::MutableBorrow)
        && !matches!(&arg.node.value.node, Expr::Identifier(_) | Expr::Deref { .. })
    {
        if let Some(root_local) = super::resolve_projection_root_local(ctx, &arg.node.value.node) {
            let start = builder.locals.len();
            let root_name = builder.local_name(root_local).map(|s| s.to_string());
            let before = root_name.as_deref().and_then(|n| ctx.lookup_local(n).map(|(l, _)| l));
            ctx.cow_before_mutation(builder, root_local, arg.span);
            let after = root_name.as_deref().and_then(|n| ctx.lookup_local(n).map(|(l, _)| l));
            // Untrack ONLY when the root actually materialized: a unique / owned
            // root is a no-op (before == after) → no private copy → no dangling
            // handles, and gating keeps non-materializing projected `&`-args
            // byte-identical (sites 1+2 self-host neutrality guarantee).
            if before != after {
                g2_projected_untrack_start = Some(start);
            }
        }
    }
    // FAMILY-1 CHOKEPOINT (CALL-ARG face): an `&`-argument naming a PLACE
    // borrows THAT PLACE.
    //
    // Before this, the `MutableBorrow` arm below lowered the argument through
    // the READ path and then `emit_borrow_mut`'d whatever temp came back. For a
    // thin-pointer field (`String`/`Vector`/`Dict`/resource struct) the read
    // path yields a `Ptr` INTO the real slot and the `is_already_ptr`
    // fast-path forwarded it — write-through worked BY ACCIDENT. For a by-value
    // place (`int`/`float`/`bool`/value struct/tuple/value-payload enum) the read
    // path yields a value COPY, so the callee got a pointer to a dying temp and
    // its write was silently discarded, `gg check` clean, on BOTH backends.
    // Same expression, same sigil, opposite semantics, decided by the projected
    // TYPE — nothing in the CoW rule mentions the type of the projected value.
    // That is the wrong-layer read-site symptom (Core #1): the read arms had
    // already discarded place identity and no work here could recover it. The
    // fix is to never lose it — ask the shared place producer FIRST.
    //
    // ⚠ THE OWNERSHIP GATE IS LOAD-BEARING. `lower_call_arg` serves all three
    // ownership modes; the match that distinguishes them is far below. Without
    // this gate the block would fire for bare and `!` arguments too, turning
    // every projected argument into a `MutPtr` borrow and bypassing
    // `callee_passes_by_ptr`, the `Borrow` arm and the `Move` arm entirely. Both
    // neighbouring blocks gate themselves the same way.
    //
    // ORDER IS THE WHOLE DESIGN (Core #15e Q5) — this call sits exactly here:
    //   * AFTER the G2 root-materialize above. `cow_before_mutation` REBINDS the
    //     root's name when it materialises a private copy, so a place resolved
    //     BEFORE it would address the SHARED source and the callee's write would
    //     escape to the caller — turning a borrow into a mutable alias and
    //     breaking CoW's central guarantee. Pinned by
    //     `sound_amp_bareparam_root_materialize.gg`, which must print 11 / 10;
    //     a naive fix prints 11 / 11 and looks correct on every other fixture.
    //   * BEFORE `lower_expr`, and returning early, so the argument is lowered
    //     EXACTLY ONCE — `&arr[side_effect()]` still evaluates its index once
    //     (`cow_amp_index_side_effect_once.gg`).
    //   * The G2 untrack still runs on the handles the projection minted, before
    //     the borrow escapes into the call, preserving the UAF-fold close.
    //
    // ⚠ THE EARLY RETURN SKIPS `maybe_auto_propagate` (below, `Snag #43`), which
    // is NOT unconditionally safe. The auto-propagate PRE-CHECK immediately
    // above the `try_resolve_place` call is what makes it safe; the full
    // explanation, the measurements, and the two probe masks live THERE, in one
    // place, next to the code they govern. Do not restate them here — an earlier
    // revision carried the whole account three times in this one function body,
    // which is three places for it to rot independently.
    //
    // (Two earlier explanations of why the skip "could not" matter were also
    // wrong and are recorded so nobody re-derives them: it is NOT "a projection
    // is never a throws-call result" — `should_auto_propagate` is TYPE-gated,
    // not shape-gated; and it is NOT "`Result` is resource-typed so the read
    // takes the Ptr branch" — `is_resource_name` has no enum-variant clause, and
    // a value-payload `Result` field measurably takes the value-copy branch,
    // i.e. the OPPOSITE way.)
    //
    // `methods.rs`'s FxHasher caller cannot exercise this axis at all: its
    // argument is a Hasher by the method contract, never a `Result`.
    // BORROW PROVENANCE — verdict: CONVERGENCE, not a new shape. Before this,
    // a RESOURCE-typed field `&`-arg flowed through `lower_field_access`, which
    // tags the forwarded `Ptr` local with borrow provenance
    // (`ctx.set_field_or_elem_borrow`) — typed metadata that `cow_before_mutation`
    // and the var-decl default-borrow branch read. The producer path returns a
    // place and `emit_borrow_mut`s a FRESH, UNTAGGED local, so those four
    // resource cells stop carrying it. That is not a regression in kind: the
    // pre-existing fall-through borrow below is equally untagged, so this makes
    // the resource cells behave like every other `&`-arg rather than like a
    // special case.
    //
    // ⚠ REGENERATE, DO NOT TRUST THE FIGURES BELOW (Core #5) — the command is
    // what is being asserted, not the number:
    //   gg build --clones=stats <fixture> -o /tmp/cs && /tmp/cs   # read [clone-stats]
    // On `security/sound_amp_field_thinptr_control.gg` (a resource-field
    // program) the line was byte-identical before and after this change. On
    // `tests/fixtures/cow_amp_deref_box_projection.gg` it strictly IMPROVES:
    // `string_clone` 60→35, `array_clone` 20→10, `map_clone` 20→10,
    // `total_allocs` 154→88, and `live_bytes` 2→0 — the whole-struct clone the
    // `&(*box).field` read used to mint per projection is gone, which is the
    // same clone that leaked (`security/sound_amp_deref_box_field_leak.gg`).
    // (An earlier revision of this comment quoted `9 → 1` / `27 → 19`; those
    // came from a scratch probe, not from the committed fixture, and were
    // unreproducible as cited. Corrected rather than silently swapped.)
    //
    // 🚨 THE AUTO-PROPAGATE PRE-CHECK IS LOAD-BEARING — do not remove it.
    // The early return below SKIPS `maybe_auto_propagate` (Snag #43), and that
    // is NOT unconditionally inert. When the ARGUMENT is `Result`-typed and the
    // CALLEE's parameter is NOT (`void take(int &x)` called as `take(&h.r)`),
    // auto-propagation is what makes the call typecheck at all: it unwraps the
    // `Result`, and on an `Error` it PROPAGATES instead of calling. Skipping it
    // would swallow the error AND hand the callee a pointer to a `Result` where
    // an `int` is expected — a lost propagation plus a type confusion.
    //
    // MEASURED (this is a regression this chokepoint introduced and this gate
    // fixes): `void take(int &x)` + `Holder{Result[int,int] r}` seeded
    // `Error(5)`, called as `take(&h.r)` inside a `throws int` fn. Base prints
    // `ERR(propagated)` — correct. Without this pre-check the chokepoint printed
    // `in take` / `ok`, silently swallowing the error. With it, base and post
    // agree. Pinned by `cow_amp_projection_autoprop_arg.gg`.
    //
    // The check is TYPE-ONLY (`place_expr_type_only`) and runs BEFORE any
    // lowering, so a side-effecting base is not evaluated twice — the same
    // discipline `try_resolve_index_element_ptr`'s kind-gate uses. When it says
    // the argument would auto-propagate, we fall through to the normal path and
    // let the existing machinery own the semantics.
    // ⚠ THE SIGNAL IS THE CALLEE'S DECLARED PARAMETER TYPE, not
    // `func_state.expected_type`. `expected_type` is AMBIENT state: the
    // free-call and method-call paths set it, the closure-var and IIFE paths do
    // not, and at an indirect call site its value is therefore whatever earlier
    // lowering happened to leave there rather than anything about this call.
    // Keying on it makes the decision depend on WHICH CALLER lowered the
    // argument instead of on what the call MEANS. Measured consequence: at an
    // indirect call site the surrounding SOURCE SPELLING changes it — an
    // explicit `Callable[void(&Result[int,int])] f = …` declaration leaves it in
    // a state that blocks the unwrap while an `auto f = …` declaration does not,
    // so the same call is served differently by a decl one line above it.
    // (Whether the enclosing constructor also contributes was NOT isolated —
    // that hypothesis was tested and REFUTED as the explanation for the
    // closure-var behaviour: the skip persists with the construction moved into
    // a helper fn and an intervening statement.)
    // `callee_param_type` is the typed fact:
    //   * param IS a `Result`  → the argument is meant to arrive whole; an
    //     unwrap here would be WRONG, so the chokepoint proceeds. (This is the
    //     cell where BOTH indirect call kinds — closure-variable call AND IIFE —
    //     measurably LOSE the call at base, because neither caller sets
    //     `expected_type` and the unwrap fires unguarded. A pre-existing defect
    //     this chokepoint fixes for a resolvable projection argument; the
    //     bare-identifier siblings still lose it and are filed. Pinned by
    //     `cow_amp_projection_indirect_call_arg.gg`.)
    //     ⚠ MEASURING THIS NEEDS TWO PRECAUTIONS, both learned the hard way in
    //     the round that introduced this code: the payload must be `Error` (an
    //     `Ok` unwraps successfully and the call proceeds either way), and the
    //     closure variable must be `auto`-annotated — an explicit
    //     `Callable[void(&Result[int,int])] f = …` leaves `expected_type` in a
    //     state that blocks the unwrap and HIDES the defect completely.
    //   * param is NOT a `Result` while the ARG is → auto-propagation is what
    //     makes the call typecheck at all, and on an `Error` it must PROPAGATE
    //     rather than call, so we fall through and let it.
    //
    // ⚠ CORRECTIONS ON RECORD (Core #14 — a false measurement in source is worse
    // than a wrong explanation, so these are kept rather than quietly swapped).
    // Earlier revisions of this comment asserted, as MEASUREMENTS: that the call
    // "HAPPENS in every one, pre-fix and post-fix alike"; that emitted programs
    // were "byte-identical across this change"; and that the skip is "inert on
    // all five callers". ALL THREE FALSE — reached with annotated closure
    // variables (mask 2) and without ever testing an IIFE. A further revision
    // blamed the enclosing constructor for polluting `expected_type`; that
    // hypothesis was tested and REFUTED (the skip persists with the construction
    // moved into a helper fn and an intervening statement) — the ANNOTATION was
    // the discriminator.
    //
    // ⚠⚠ AND THE PRE-CHECK'S DOMAIN MUST TRACK THE PRODUCER'S. This gate asks
    // `place_expr_type_only`, whose `match` must cover every form
    // `try_resolve_place` resolves. It shipped without a `TupleFieldAccess` arm
    // while the producer had one, so `take(&t.0)` on a `(Result[int,int], int)`
    // took the early return with this question never asked and SWALLOWED the
    // error — the same miscompile, one costume over, introduced by the very
    // commit that fixed the struct-field costume. Pinned by
    // `place_type_only_covers_the_producer_forms` in `tests/lints.rs` and by row
    // C of `cow_amp_projection_autoprop_arg.gg`.
    // 🚨 FAIL-SAFE BY CONSTRUCTION — an UNKNOWN form must DECLINE the early
    // return, never take it.
    //
    // This predicate is phrased as "is it PROVABLY SAFE to skip
    // `maybe_auto_propagate`?" rather than "would it auto-propagate?", because
    // the two differ exactly on the `None` case and that difference is a
    // miscompile. An earlier revision computed `arg_would_auto_propagate` with
    // `.unwrap_or(false)` and then branched on `!arg_would_auto_propagate`, so an
    // untypeable form produced `false` → `!false` → SKIP. Its comment claimed the
    // opposite ("a missing arm costs a lost optimisation, never a lost
    // propagation"); the comment was INVERTED, and reasoning from it shipped the
    // same swallowed-`Error` miscompile THREE times in successive costumes
    // (struct field, tuple field, then `Deref`/`Guard` objects). The invariant is
    // now enforced by the `match` below instead of asserted in prose.
    // MutableBorrow (`&arg`): always TRY the shared place producer first.
    // Auto-propagate safety is decided from the RESOLVED place's type — not
    // from a pre-check that can return None for forms the producer still
    // resolves (get-chain field `v.get(i).unwrap().fd`: Family-3; `place_expr_type_only`
    // lacked a MethodCall object arm, so the early path was skipped, lower_expr
    // materialised a field temp, and `borrow_mut` wrote to the temp — silent
    // wrong output with hist total_misses=0, Core #13 / Some(wrong_root) trap).
    //
    // Fail-safe for auto-prop: if the resolved type would auto-propagate and
    // the callee does not take Result, fall through to lower_expr + maybe_auto_propagate.
    if matches!(arg.node.ownership, Ownership::MutableBorrow) {
        if let Some((place, place_type)) = super::try_resolve_place(ctx, builder, &arg.node.value)
        {
            let param_is_result = callee_param_type
                .map(|p| ctx.type_registry.enum_category(p) == Some(EnumCategory::Result))
                .unwrap_or(false);
            let safe_to_skip_auto_propagate = param_is_result
                || super::should_auto_propagate(ctx, builder, place_type).is_none();
            if safe_to_skip_auto_propagate {
                if let Some(s) = g2_projected_untrack_start {
                    ctx.untrack_transient_element_refs_in_range(
                        builder,
                        s,
                        builder.locals.len(),
                    );
                }
                // The producer guarantees `place` is an lvalue of the VALUE itself
                // (postcondition 1), so this borrow is unconditional.
                let ptr_type = ctx.register_mut_ptr_type(place_type);
                let dst = builder.add_local(ptr_type, None);
                builder.emit_borrow_mut(dst, place);
                return FunctionBuilder::copy(dst);
            }
        }
    }
    let val = lower_expr(ctx, builder, &arg.node.value);
    // Snag #43 (2026-05-13): a throws-call passed in arg position
    // (`v.push(sub())` where `sub() throws E`, in a fn that itself
    // `throws E`) is type `Result[T, E]`, but the callee's param
    // expects `T`. The auto-propagation must fire HERE — without it,
    // the LIR pushes the whole Result struct's bytes into the
    // collection slot, and reads of the collection element see the
    // Result's tag/padding instead of T's fields. Previously this
    // step lived only in the free-function-call args lowering
    // (`lower_call` at calls.rs:1168), and method-call args
    // (`lower_method_call` at methods.rs:1746) routed through
    // `lower_call_arg` without it — so `v.push(throws_fn())` silently
    // corrupted non-Copy fields of the returned T while
    // `T tmp = throws_fn(); v.push(!tmp)` worked. Hoisting the
    // auto-prop step inside `lower_call_arg` makes every caller pay
    // it uniformly; the `expected_type` gate (set by the caller when
    // the param type is itself a Result, e.g. `Vector[Result[T,E]]
    // .push(Ok(...))`) prevents over-unwrapping.
    let val = super::maybe_auto_propagate(ctx, builder, val, arg.node.value.span);
    // Track N2 (2026-07-28): if the callee expects `Box[Trait]` and the arg
    // is `Box[Concrete]`, materialise a Box[Trait] TraitObj temp so the
    // downstream borrow / value / MutPtr paths see a well-typed 16-byte
    // `{data, vtable}` operand. Same coercion the LIR `try_trait_object_construct`
    // pass fires on SlotStore; this hoists it to the call-arg boundary so a
    // ctor / user fn taking `Box[Trait]` doesn't memcpy(16) from an 8-byte
    // `Box[Concrete]` slot (the SIGBUS class closed by this track). Helper
    // is a no-op when either the callee's param or the arg source isn't a
    // Box, when both Boxes have the same inner, or when the destination
    // inner isn't a trait — so it costs a single string-strip + typedef
    // lookup on the non-firing paths.
    let val = maybe_pack_trait_object_at_arg(ctx, builder, val, callee_param_type);
    // G2 site-3 UAF-fold close: the projection above minted transient
    // element/field handles INTO the freshly-materialized private copy. Reset
    // their CoW tags now (before the borrow is built and the arg is forwarded)
    // so a later same-collection push can't Case-3-clone a dangling temp. Only
    // fires when the root materialized (see `g2_projected_untrack_start`), so
    // non-materializing projected `&`-args stay byte-identical. Named borrows
    // are spared by the helper's `local_name(local).is_none()` guard.
    if let Some(s) = g2_projected_untrack_start {
        ctx.untrack_transient_element_refs_in_range(builder, s, builder.locals.len());
    }
    match arg.node.ownership {
        Ownership::MutableBorrow => {
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
                // Gorget-js snag #1: when the inner expression already yields a
                // pointer (e.g. `&xs.get(i).unwrap()` where unwrap returns
                // Ptr(T)), `&value` is the borrow itself, not the address of
                // the temp slot holding it. Without this check we emit
                // `_21 = borrow_mut _20` producing `*mut *JsVal` — callee then
                // reads the pointer's bits as the payload (zeros for
                // page-aligned addresses). Mirrors `is_already_ptr` in the
                // standalone `Expr::MutableBorrow` handler (`is_already_ptr`,
                // exprs/mod.rs). Both now guard only the FALL-THROUGH path: a
                // resolvable projection returns early at the Family-1 chokepoint
                // above, whose producer postcondition makes the borrow
                // unconditional.
                //
                // 🚨 THIS GUARD IS LIVE — DEMONSTRATED, not assumed. Disable it
                // and `push_it(&c.p)` on a `struct HMut { MutRef[Vector[int]] p }`
                // SIGSEGVs (exit 139) where it prints `3` with the guard in
                // place. Pinned by the `MutRef` row of
                // `cow_amp_ref_field_forward.gg`.
                //
                // ⚠ WHY THAT ROW, AND WHY A GREEN RUN PROVES NOTHING HERE. The
                // LIR ALREADY forwards a stored pointer for SOME bare places:
                // `lir/lower/operands.rs` emits `SlotLoad` (reading the pointer
                // bits) instead of `SlotAddr` when the local is
                // `SlotKind::BorrowedPtr`-kinded OR its slot is specifically
                // `PtrTo(GorgetString)`, and there is no `Deref` projection.
                // This GIR-level guard's condition is BROADER — it tests the GIR
                // TYPE (`Ptr`/`MutPtr`) with no `slot_kind` component. The two
                // predicates are DIFFERENT SETS, and the gap between them is
                // exactly where this guard earns its keep: a `PtrTo` slot whose
                // pointee is NOT `GorgetString` (a `Vector`, above) takes
                // `SlotAddr`, so without the guard the callee receives `**T`.
                // Most shapes are covered by the LIR path and never exercise
                // this arm, so a green suite is NOT evidence that it is dead.
                // ⚠ An earlier revision claimed `&xs.get(i).unwrap()` as the
                // shape needing it. That claim is NOT reproducible — disabling
                // the guard leaves the get-chain fixtures green, because those
                // locals ARE `BorrowedPtr`-kinded — and it is corrected here
                // rather than silently swapped (Core #14).
                if place.projections.is_empty()
                    && matches!(
                        ctx.type_registry.get(local_type),
                        Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))
                    )
                {
                    return FunctionBuilder::copy(place.local);
                }
                let ptr_type = ctx.register_mut_ptr_type(local_type);
                let dst = builder.add_local(ptr_type, None);
                builder.emit_borrow_mut(dst, place.clone());
                return FunctionBuilder::copy(dst);
            }
            val
        }
        Ownership::Borrow if callee_passes_by_ptr => {
            // Bare call-site: emit const Ptr by default.
            // Exception: when the callee's param ownership is Move (e.g., generic functions
            // that return a Move-type parameter directly), use MutPtr to transfer ownership.
            let callee_param_ownership = ctx.fn_param_ownerships.get(callee_name)
                .and_then(|ownerships| ownerships.get(arg_idx))
                .copied();
            let use_mut_ptr = matches!(callee_param_ownership, Some(Ownership::Move));
            // GlobalRef → GlobalRefPtr: emit &global_name directly.
            if let Operand::Constant(Constant::GlobalRef(name)) = &val {
                return Operand::Constant(Constant::GlobalRefPtr(name.clone()));
            }
            // For Copy/Move operands of plain locals, borrow in place.
            // For constants or complex expressions, materialize into a temp first.
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                if place.projections.is_empty() {
                    let local_type = builder.local_type(place.local);
                    // Already a Ptr / MutPtr (borrowed resource param, or `&`-param
                    // whose stored local is `*mut T`) — forward directly, don't
                    // wrap in another Ptr layer. Track B1 (2026-07-27): the
                    // MutPtr arm was missing; the MutableBorrow arm above already
                    // handles both — same completeness there. Without it, a
                    // bare-arg call `cb(a)` where `a` is a `&`-param and the
                    // callable's declared param is `&T` re-wraps the caller's
                    // MutPtr(T) into a fresh Ptr(MutPtr(T)) and the callee's
                    // write-through lands on the wrong indirection level.
                    if matches!(
                        ctx.type_registry.get(local_type),
                        Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))
                    ) {
                        return FunctionBuilder::copy(place.local);
                    }
                    if use_mut_ptr {
                        let ptr_type = ctx.register_mut_ptr_type(local_type);
                        let dst = builder.add_local(ptr_type, None);
                        builder.emit_borrow_mut(dst, place.clone());
                        // Mark source as moved — callee takes ownership
                        ctx.drops.mark_moved(place.local);
                        // Schedule MoveZero AFTER the call — the callee reads from
                        // this address, so we can't zero before. Matches the
                        // Ownership::Move path at line ~212.
                        ctx.func_state.pending_move_zeros.push(place.local);
                        return FunctionBuilder::copy(dst);
                    } else if place.projections.is_empty()
                        && !ctx.is_named_local(place.local)
                        && ctx.is_owned_local(builder, place.local)
                        && ctx.type_registry.needs_drop(local_type)
                        && !ctx.drops.is_registered(place.local)
                        && !ctx.drops.is_moved(place.local)
                    {
                        // `place.local` is an owning temporary built for this
                        // argument (e.g. `f(Node(...))`). The callee borrows it
                        // (const Ptr) and does NOT drop it, so the caller owns it
                        // and must free it once the call expression completes.
                        //
                        // A field-constructed temp is initialized via field-address
                        // writes, which the LIR drop-flag dataflow does not see as
                        // a slot init — so a drop emitted on it directly is deleted
                        // (treated as Uninitialized). Re-home it into a fresh slot
                        // via a whole-slot store (recognized → Initialized), mirror
                        // of the named-local path, then borrow that and schedule a
                        // post-call drop on it.
                        let owned = builder.add_local(local_type, None);
                        builder.assign_mode(
                            crate::ir::instructions::AssignMode::Move,
                            Place::local(owned),
                            FunctionBuilder::mov(place.local),
                        );
                        ctx.set_owned(builder, owned);
                        let ptr_type = ctx.register_ptr_type(local_type);
                        let dst = builder.add_local(ptr_type, None);
                        builder.emit_borrow(dst, Place::local(owned));
                        ctx.func_state.pending_temp_drops.push(owned);
                        return FunctionBuilder::copy(dst);
                    } else {
                        let ptr_type = ctx.register_ptr_type(local_type);
                        let dst = builder.add_local(ptr_type, None);
                        builder.emit_borrow(dst, place.clone());
                        return FunctionBuilder::copy(dst);
                    }
                }
            }
            // Materialize non-place values (constants, call results) into a temp local
            if let Some(pt) = callee_param_type {
                // String constants need a Str-typed temp (32 bytes), not a Ptr-typed
                // temp (8 bytes), even when callee_param_type is Ptr(Str).
                let mat_type = if matches!(val, Operand::Constant(Constant::Str(_)))
                    && ctx.pointee_type(pt).map_or(false, |inner| ctx.type_mapper.is_string_type(inner))
                {
                    ctx.pointee_type(pt).unwrap_or(pt)
                } else {
                    pt
                };
                let tmp = builder.add_local(mat_type, None);
                builder.assign(Place::local(tmp), val);
                if use_mut_ptr {
                    let ptr_type = ctx.register_mut_ptr_type(pt);
                    let dst = builder.add_local(ptr_type, None);
                    builder.emit_borrow_mut(dst, Place::local(tmp));
                    return FunctionBuilder::copy(dst);
                } else {
                    let ptr_type = ctx.register_ptr_type(pt);
                    let dst = builder.add_local(ptr_type, None);
                    builder.emit_borrow(dst, Place::local(tmp));
                    return FunctionBuilder::copy(dst);
                }
            }
            // Fallback: materialize as GorgetString if the value is a string constant.
            if let Operand::Constant(Constant::Str(_)) = &val {
                let sv_type = ctx.type_mapper.owned_string_type;
                let tmp = builder.add_local(sv_type, None);
                builder.assign(Place::local(tmp), val);
                let ptr_type = ctx.register_ptr_type(sv_type);
                let dst = builder.add_local(ptr_type, None);
                builder.emit_borrow(dst, Place::local(tmp));
                return FunctionBuilder::copy(dst);
            }
            val // pass through for non-string constants
        }
        Ownership::Move if callee_is_move_param => {
            // If the operand is Ptr(T) (borrowed ref), auto-clone to create
            // an owned value before moving to the callee.
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                if place.projections.is_empty() {
                    let local_type = builder.local_type(place.local);
                    if let Some(inner) = ctx.pointee_type(local_type) {
                        if let Some(clone_fn) = ctx.clone_fn_for_ptr(inner) {
                            let cloned = ctx.emit_clone(builder, &clone_fn, vec![FunctionBuilder::copy(place.local)], arg.span, inner, crate::ir::ImplicitCloneReason::MoveParamFromBorrow);
                            let ptr_type = ctx.register_mut_ptr_type(inner);
                            let dst = builder.add_local(ptr_type, None);
                            builder.emit_borrow_mut(dst, Place::local(cloned));
                            return FunctionBuilder::copy(dst);
                        }
                    }
                }
            }
            // Move of a Move-type value: callee expects MutPtr. Emit borrow_mut.
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                if place.projections.is_empty() {
                    let local_type = builder.local_type(place.local);
                    // Pragmatic: skip Move for named string locals (! is no-op for strings).
                    // TODO: borrow checker should reject multi-use ! on strings.
                    // Lazy loop-carried CoW note: this short-circuit is WHY
                    // `consume(!s)` needs no `cow_lazy_mat_flag` clearing —
                    // the local is passed as a const-Ptr borrow with no
                    // MoveZero, so `s` is unchanged and its tag/flag stay
                    // accurate. If this short-circuit is ever retired, the
                    // generic move path below must clear the lazy pair like
                    // the `lower_assign` write sites do.
                    let inner = ctx.pointee_type(local_type).unwrap_or(local_type);
                    if ctx.type_mapper.is_string_type(inner) && ctx.is_named_local(place.local) {
                        // Pass as const Ptr (borrow), no MoveZero
                        let ptr_type = ctx.register_ptr_type(local_type);
                        let dst = builder.add_local(ptr_type, None);
                        builder.emit_borrow(dst, place.clone());
                        return FunctionBuilder::copy(dst);
                    }
                    // CoW: move transfers ownership. Sever aliases first.
                    ctx.cow_before_mutation(builder, place.local, arg.span);
                    let ptr_type = ctx.register_mut_ptr_type(local_type);
                    let dst = builder.add_local(ptr_type, None);
                    builder.emit_borrow_mut(dst, place.clone());
                    // Mark the source as moved in the caller
                    ctx.drops.mark_moved(place.local);
                    // Schedule MoveZero AFTER the call — the callee reads from this
                    // address, so we can't zero before. The post-call MoveZero
                    // prevents the scope-exit drop from double-freeing.
                    ctx.func_state.pending_move_zeros.push(place.local);
                    return FunctionBuilder::copy(dst);
                }
            }
            // Materialize non-place values into a temp
            if let Some(pt) = callee_param_type {
                let tmp = builder.add_local(pt, None);
                builder.assign(Place::local(tmp), val);
                let ptr_type = ctx.register_mut_ptr_type(pt);
                let dst = builder.add_local(ptr_type, None);
                builder.emit_borrow_mut(dst, Place::local(tmp));
                return FunctionBuilder::copy(dst);
            }
            val
        }
        Ownership::Move => {
            // Refcounted types (Shared / Weak / Channel — Trivial copy semantics
            // but needing a drop at scope exit). `!x` at the call site means the
            // callee takes ownership of the refcount; the caller's slot must be
            // zeroed so its scope-exit drop doesn't fire a second time.
            // Without this, `drop_all(!s)` compiles to a plain by-value pass,
            // callee drops s, then caller ALSO drops s — heap-use-after-free
            // inside gorget_shared_drop the second time around.
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                if place.projections.is_empty() {
                    let local_type = builder.local_type(place.local);
                    if ctx.type_registry.needs_param_drop(local_type) {
                        ctx.drops.mark_moved(place.local);
                        ctx.func_state.pending_move_zeros.push(place.local);
                        return val;
                    }
                }
            }
            ctx.auto_clone_if_ptr(builder, val, arg.span)
        }
        _ => ctx.auto_clone_if_ptr(builder, val, arg.span), // Auto-clone Ptr(T) → T at boundary
    }
}

/// Resolve named arguments and default parameter values for a function call.
/// Reorders named args to match parameter order and fills in defaults for missing params.
fn resolve_call_args<'a>(
    ctx: &LoweringContext,
    fn_name: &str,
    args: &'a [Spanned<ast::CallArg>],
) -> Vec<Spanned<ast::CallArg>> {
    let param_names = match ctx.fn_param_names.get(fn_name) {
        Some(names) => names,
        None => return args.to_vec(), // no param info → pass through unchanged
    };

    let has_named = args.iter().any(|a| a.node.name.is_some());
    let has_defaults = ctx.fn_defaults.contains_key(fn_name);

    if !has_named && !has_defaults {
        return args.to_vec();
    }
    if !has_named && args.len() >= param_names.len() {
        return args.to_vec(); // all params supplied positionally, no reorder needed
    }

    // Build a slot array matching parameter order
    let mut slots: Vec<Option<Spanned<ast::CallArg>>> = vec![None; param_names.len()];

    // Place positional args first
    let mut positional_idx = 0;
    for arg in args {
        if arg.node.name.is_some() {
            // Named arg — place by name
            let arg_name = arg.node.name.as_ref().unwrap().node.as_str();
            if let Some(pos) = param_names.iter().position(|p| p == arg_name) {
                slots[pos] = Some(arg.clone());
            }
        } else {
            // Positional — skip past already-filled slots from named args
            while positional_idx < slots.len() && slots[positional_idx].is_some() {
                positional_idx += 1;
            }
            if positional_idx < slots.len() {
                slots[positional_idx] = Some(arg.clone());
                positional_idx += 1;
            }
        }
    }

    // Fill in defaults for any remaining empty slots
    if let Some(defaults) = ctx.fn_defaults.get(fn_name) {
        for (param_idx, default_expr) in defaults {
            if *param_idx < slots.len() && slots[*param_idx].is_none() {
                slots[*param_idx] = Some(Spanned::dummy(ast::CallArg {
                    name: None,
                    ownership: ast::Ownership::Borrow,
                    value: Spanned::dummy(default_expr.clone()),
                }));
            }
        }
    }

    // Collect filled slots (skip any remaining None — shouldn't happen for valid code)
    slots.into_iter().flatten().collect()
}

/// Smart-pointer constructors (`Shared[Callable[T]](closure)`,
/// `Mutex[Callable[T]](closure)`, etc.) lower to a static-inline C wrapper
/// `XXX__T__new(T val)` that takes the inner type by value. When the inner
/// is a Callable family alias (`c_runtime_alias = "GorgetClosure"`), the GIR
/// arg is a `__Closure_N` env struct — but the C wrapper expects a packed
/// 16-byte `GorgetClosure` (fn_ptr + env_ptr).
///
/// The LIR's `try_closure_pack` (in `operands.rs`) already handles the
/// packing, but it fires only on `Assign` instructions where the destination
/// slot type is `GorgetClosure`. Direct `Call` arguments bypass it.
///
/// This helper bridges the two: it allocates an intermediate local typed as
/// the Callable alias (which lowers to `Struct(GorgetClosure)`), assigns the
/// closure into it (triggering `try_closure_pack`), and returns an operand
/// pointing at the now-packed local. The constructor then sees a proper
/// `GorgetClosure` value, identical to what `Box.new(closure)` synthesises
/// via its own special-case path at `methods.rs:78-84`.
///
/// Decision driven by typed metadata (`c_runtime_alias`), not by name —
/// per CLAUDE.md "no name matching" + "layering discipline §3 (one source
/// of truth per axis)". Same shape as `is_callable_alias_name` in
/// `methods.rs:2728` and `infer_drop_strategy` in `lir/lower/drops.rs:698`.
pub(super) fn pack_closure_for_smart_ptr_ctor(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    val_op: Operand,
    inner_c: &str,
) -> Operand {
    // Read the typed signal: is `inner_c` a Callable family alias of
    // GorgetClosure? `register_callable_alias` and the eager-walk path in
    // `types.rs:257` install `c_runtime_alias = "GorgetClosure"` on every
    // such TypeDef.
    let is_callable_alias = ctx.type_registry.get_type_def(inner_c)
        .and_then(|td| td.metadata.c_runtime_alias.as_deref())
        == Some("GorgetClosure");
    if !is_callable_alias { return val_op; }

    // Look up the alias TypeId. If the alias hasn't been registered yet
    // (e.g. the smart-pointer path bypassed `register_callable_inner_if_any`),
    // fall through — the C compile would fail anyway, but better not to
    // guess at a TypeId out of thin air.
    let alias_tid = match ctx.type_mapper.lookup_named(inner_c) {
        Some(tid) => tid,
        None => return val_op,
    };

    // Materialise into a typed temp. The slot's LIR type resolves to
    // `Struct(GorgetClosure)` (via the `c_runtime_alias` path in
    // `lir/lower/mod.rs:700+`), so the SlotStore here triggers
    // `try_closure_pack` which packs the env into a real GorgetClosure.
    let tmp = builder.add_local(alias_tid, None);
    builder.assign(Place::local(tmp), val_op);
    FunctionBuilder::copy(tmp)
}

/// Track N2 general-call-arg entry: peel the callee's declared param TypeId
/// down to its Box[Trait] alias name and dispatch to
/// `pack_trait_object_for_smart_ptr_ctor`. Callee params come typed as either
/// `Box[Trait]` (a `GirType::Named("Box__<Trait>")`) or as `Ptr(Box[Trait])` /
/// `MutPtr(Box[Trait])` (the pass-by-pointer ABI is decided downstream). We
/// peel one layer of Ptr/MutPtr, then delegate to the same helper the smart-
/// pointer ctors use — the helper's typed-metadata checks (Box + trait_obj
/// registration + source-is-different-Box) provide the fine filtering.
pub(super) fn maybe_pack_trait_object_at_arg(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    val: Operand,
    callee_param_type: Option<TypeId>,
) -> Operand {
    let pt = match callee_param_type {
        Some(t) => t,
        None => return val,
    };
    // Peel one layer of Ptr / MutPtr: the callee's ABI carries `Box[Trait]`
    // as a bare Named type OR as a pointer to it; both shapes reach here.
    let pointee_or_self = match ctx.type_registry.get(pt) {
        Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => *inner,
        _ => pt,
    };
    let name = match ctx.type_registry.get(pointee_or_self) {
        Some(GirType::Named(n)) => n.clone(),
        _ => return val,
    };
    pack_trait_object_for_smart_ptr_ctor(ctx, builder, val, &name)
}

/// Round XII Track N2 (2026-07-28): closes the `Mutex[Box[Trait]](Box.new(Concrete))`
/// SIGBUS class filed by Round XI Track M as `sound_guard_get_boxed_trait_sigbus.gg`.
///
/// SIBLING of `pack_closure_for_smart_ptr_ctor` — same shape, different coercion.
/// Smart-pointer constructors (`Mutex/RWLock/Shared[Box[Trait]](Box.new(Concrete))`)
/// lower to `Mutex__Box__Trait__new(val)` etc., which memcpy `sizeof(Box[Trait])`
/// bytes (16, the {data, vtable} TraitObj layout) out of the arg's slot. But
/// `Box.new(Concrete)` produces a `Box__Concrete` (void*, 8 bytes) — the
/// mutex-new memcpy then reads 8 bytes of stack garbage as the vtable pointer,
/// producing a SIGBUS at the first method dispatch through the guard.
///
/// The LIR's `try_trait_object_construct` (in `operands.rs`) already handles
/// the Box[Trait]←Box[Concrete] coercion, but it fires only on `Assign` /
/// `SlotStore` instructions where the destination slot's typed `is_box`
/// metadata matches the source's and the inner names differ. Direct `Call`
/// arguments bypass it — the ctor call is emitted with the raw
/// `Box__Concrete`-typed operand.
///
/// This helper bridges the two: it allocates an intermediate local typed as
/// the Box[Trait] alias, assigns the concrete Box into it (triggering
/// `try_trait_object_construct`'s SlotStore path), and returns an operand
/// pointing at the now-packed local. The constructor then sees a proper
/// 16-byte TraitObj value.
///
/// Layering rule 4 (resolve once, write through): the coercion decision is
/// still made at ONE place in the LIR (`try_trait_object_construct`); this
/// helper's job is only to route the ctor path through that same SlotStore
/// site the SlotStore consumers use.
///
/// Decision driven by typed metadata (`is_box` on both types + the trait's
/// registered `<Trait>_TraitObj` TypeDef), not by name — per CLAUDE.md
/// "no name matching" + "layering discipline §3".
///
/// **Predicate scope (pass-1 R5 polish, 2026-07-28) — what this helper WILL NOT
/// widen.** No-ops on:
///   1. `inner_c` not prefixed `Box__` (destination isn't a Box — no coercion
///       applies).
///   2. No `<inner>_TraitObj` registered (inner of the Box isn't a trait — no
///      TraitObj shape to construct into).
///   3. `Box[Trait]` alias TypeId not resolvable via `lookup_named` (the
///      typedef hasn't been registered on the type_mapper — bail rather than
///      invent a TypeId).
///   4. Source operand is NOT a plain `Copy` / `Move` of a local with empty
///      projections — Constants, projections-into-fields, and complex
///      operands are out of scope. The LIR SlotStore trigger requires a
///      direct local as the RHS to fire, so a `Constant(Constant::...)` or a
///      `Place` with projections wouldn't produce the shape
///      `try_trait_object_construct` matches; passing through unchanged
///      preserves behavior for those cases (they either aren't Box-typed
///      or route through a different LIR path).
///   5. Source's type isn't a `GirType::Named` starting with `Box__` (not a
///      Box source, e.g. someone passed a raw pointer or a primitive).
///   6. Source's Box-inner equals destination's Box-inner (already the right
///      Box[Trait] type — no coercion needed).
/// Future readers: widening the accepted-operand-shape (e.g. to constants or
/// projections) requires teaching `try_trait_object_construct` to fire on the
/// widened source shape as well (Layering rule 4 — resolve once, write
/// through: the IR side only constructs the triggering shape; the decision
/// itself lives in LIR).
pub(super) fn pack_trait_object_for_smart_ptr_ctor(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    val_op: Operand,
    inner_c: &str,
) -> Operand {
    // Fast-out: `inner_c` must be a Box type (Box__X).
    let inner_c_inner = match inner_c.strip_prefix("Box__") {
        Some(i) => i,
        None => return val_op,
    };
    // The inner-of-inner must be a trait (has a registered `<name>_TraitObj`).
    let trait_obj_name = format!("{inner_c_inner}_TraitObj");
    if ctx.type_registry.get_type_def(&trait_obj_name).is_none() {
        return val_op;
    }
    // Look up the Box[Trait] TypeId (the destination we want to coerce into).
    let box_trait_tid = match ctx.type_mapper.lookup_named(inner_c) {
        Some(tid) => tid,
        None => return val_op,
    };
    // Inspect the source operand's type. Only fire if source is a Box[X] with
    // a DIFFERENT inner (Box[Concrete] not Box[Trait]) — same predicate as
    // `try_trait_object_construct`.
    let src_tid = match &val_op {
        Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => {
            builder.local_type(p.local)
        }
        _ => return val_op,
    };
    let src_name = match ctx.type_registry.get(src_tid) {
        Some(GirType::Named(n)) => n.clone(),
        _ => return val_op,
    };
    let src_inner = match src_name.strip_prefix("Box__") {
        Some(i) => i,
        None => return val_op,
    };
    if src_inner == inner_c_inner {
        // Already a Box[Trait] of the correct kind — no coercion needed.
        return val_op;
    }
    // Materialise into a Box[Trait]-typed temp. The SlotStore here triggers
    // `try_trait_object_construct` at LIR which constructs the
    // `{data, vtable}` TraitObj into the temp's slot.
    let tmp = builder.add_local(box_trait_tid, None);
    builder.assign(Place::local(tmp), val_op);
    FunctionBuilder::copy(tmp)
}

/// Lower a function call.
pub(super) fn lower_call(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    callee: &Spanned<Expr>,
    args: &[Spanned<ast::CallArg>],
    generic_args: Option<&[Spanned<ast::Type>]>,
) -> Operand {
    if let Expr::Identifier(name) = &callee.node {
        if name == "print" {
            lower_print_call(ctx, builder, args);
            return Operand::Constant(Constant::Unit);
        }
        // `panic(msg)` — noreturn builtin (option (b) from the pre-existing
        // TODO entry / gorget-js critique #5, 2026-05-13). Routes through
        // the same `gorget_panic` runtime symbol that `assert` lowering
        // already uses, then emits `unreachable` so post-panic code is
        // unreachable (matches Never-typed callsite contract). The
        // typecheck pass at `semantic/typecheck.rs` returns `never_id` for
        // `panic`, and the resolver's `is_builtin` predicate accepts
        // `panic` so users can call it without an explicit import. Pairs
        // with the `noreturn_fns` registration of `gorget_panic` so
        // `lower_call_extern` would handle indirect uses too.
        if name == "panic" && args.len() == 1 {
            let msg_op = lower_expr(ctx, builder, &args[0].node.value);
            // D11: route through gorget_trap(T_Panic, msg) — was gorget_panic.
            // The C/LLVM boundary rewrites gorget_trap→gorget_trap_at threading
            // the span, the same machinery gorget_panic used.
            let code_op = Operand::Constant(Constant::Str(
                crate::trap::TrapKind::Panic.code().to_string()));
            builder.call_void("gorget_trap", vec![code_op, msg_op]);
            builder.unreachable();
            return Operand::Constant(Constant::Unit);
        }

        // len(x) free function → dispatch to the correct runtime function
        // based on the argument type (string, vector, dict, set).
        if name == "len" && args.len() == 1 {
            let recv = lower_expr(ctx, builder, &args[0].node.value);
            let recv_type = infer_operand_type_full(ctx, &recv, builder);
            let resolved = ctx.pointee_type(recv_type).unwrap_or(recv_type);
            let runtime_fn = if ctx.type_mapper.is_string_type(resolved) {
                "gorget_str_codepoint_count"
            } else if ctx.type_registry.is_collection_type(resolved) {
                // Read typed `collection_kind` (Phase A) — covers OrderedMap/Map
                // (Dict/HashMap), OrderedSet/Set (Set/HashSet), and Array
                // (default fall-through).
                use crate::ir::types::CollectionKind;
                match ctx.type_registry.collection_kind(resolved) {
                    Some(CollectionKind::OrderedMap) | Some(CollectionKind::Map) => "gorget_map_len",
                    Some(CollectionKind::OrderedSet) | Some(CollectionKind::Set) => "gorget_set_len",
                    _ => "gorget_array_len",
                }
            } else {
                // Check for user-defined Measurable.len() before falling back
                if let Some(crate::ir::types::GirType::Named(n)) = ctx.type_registry.get(resolved) {
                    let method_name = format!("{n}__len");
                    if ctx.fn_sigs.contains_key(&method_name) {
                        let ptr_type = ctx.register_ptr_type(resolved);
                        let borrow = match &recv {
                            Operand::Copy(p) | Operand::Move(p) => builder.borrow(p.clone(), ptr_type),
                            _ => {
                                let l = builder.add_local(resolved, None);
                                builder.assign(Place::local(l), recv.clone());
                                builder.borrow(Place::local(l), ptr_type)
                            }
                        };
                        let dst = builder.call(&method_name, vec![FunctionBuilder::copy(borrow)], I64_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                }
                "gorget_array_len"
            };
            let dst = builder.call_extern(runtime_fn, vec![recv], I64_TYPE);
            return FunctionBuilder::copy(dst);
        }

        // Box(value) constructor → heap allocation via __gorget_box_alloc
        if (name == "Box" || name.starts_with("Box__")) && args.len() == 1 {
            let mut val_op = lower_expr(ctx, builder, &args[0].node.value);
            let raw_type = infer_operand_type_full(ctx, &val_op, builder);
            // Unwrap Ptr(T) → T: bare-borrowed resource params are passed by pointer.
            // Box should box the value, not the pointer.
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
            let inner_c = if let Some(rest) = name.strip_prefix("Box__") {
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
                ensure_box_type_def(ctx, &box_mangled, val_type);
                tid
            };
            let alloc_fn = format!("__gorget_box_alloc_{inner_c}");
            // Box() is a consuming position: the runtime's `*p = val` shallow-
            // copies the value into the heap, so resource-typed values whose
            // source is borrowed (bare param, non-last-use local, Ref, etc.)
            // would alias the box's interior with the caller's data —
            // identical pattern to push/put/insert. Apply the standard
            // consuming-arg ownership shim before the alloc call so a clone
            // is inserted when the source can't be moved. Without this, code
            // like `parse_function_type(named_ty, ...)` ends up with the
            // returned TFunction's box and the caller's named_ty sharing the
            // same Vector data — a use-after-free that, on this specific
            // shape, manifests as infinite Type__clone recursion.
            val_op = ctx.ensure_owned_at_consuming_arg(
                builder,
                val_op,
                &args[0].node.value,
                crate::ir::ImplicitCloneReason::ConsumingArg,
            );
            // Box takes ownership: after the alloc shallow-copies the value
            // into the heap, the source's slot still holds the same interior
            // pointers (Box children, String data, Vector handles). If we
            // only `unregister` from the drop tracker, the slot stays alive
            // for any subsequent INSTRUCTION-LEVEL drop — notably the
            // pre-rebind `drop x` that `lower_assign` emits when the
            // assignment target itself owns a resource. That drop frees
            // the interior pointers the new Box now owns, leaving the
            // freshly-allocated Box with dangling children. Zero the source
            // slot and mark it moved so both scope-exit drops AND
            // pre-rebind drops see it as already-dead.
            //
            // This matters specifically for the left-fold-into-self pattern
            //   `lhs = Node.Op(..., Box.new(!lhs), Box.new(!rhs))`
            // where `lhs` is being read for the `Box.new` AND rebound by the
            // surrounding assignment. Without the zero+mark, iteration 2's
            // pre-rebind drop frees iteration 1's heap-copied interior, and
            // iteration 3 segfaults reading dangling pointers from the box.
            let consumed_source: Option<LocalId> = match &val_op {
                Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => Some(p.local),
                _ => None,
            };
            if let Some(src) = consumed_source {
                ctx.drops.unregister(src);
            }
            // Tier 2c (snag #23 class): register this alloc fn as a
            // shallow-copy heap-allocating consumer so
            // `validate_drop_pre_rebind` recognises it via typed metadata
            // rather than name matching. See
            // `Module::heap_alloc_consumer_externs`.
            ctx.heap_alloc_consumer_externs.insert(alloc_fn.clone());
            let dst = builder.call_extern(&alloc_fn, vec![val_op], box_type);
            // Tier 2a Phase 2A: Box allocation returns a fresh heap
            // allocation that doesn't alias any other slot. Tag the
            // result FreshOwned so the consume-site validator sees a
            // sound `(FreshOwned, dead, _)` tuple at the EnumInit /
            // StructInit consumer instead of `Untracked`. Mirrors
            // `call_extern_tracked` for the Box-alloc shape.
            if !ctx.drops.is_registered(dst) {
                ctx.drops.register_local(dst, box_type, &ctx.type_registry);
            }
            ctx.set_owned_fresh(builder, dst);
            if let Some(src) = consumed_source {
                ctx.move_zero_and_mark(builder, src);
            }
            return FunctionBuilder::copy(dst);
        }

        // String constructors — content, capacity, and allocator forms:
        //   String("hi")  → gorget_string_from_str      String(n) / String(cap=n) → gorget_string_with_capacity
        //   String(..., alloc=a) → same ctor under a push/pop-allocator bracket —
        //   the one-shot allocator form, the SAME mechanism as the collection-ctor
        //   bracket below (language-reference §15.3 "Composable allocation"). The
        //   runtime ctor snapshots `__gorget_current_alloc` into the Str's `alloc`
        //   field, so growth reallocs stay in the chosen allocator (sticky).
        //   Before round-33 this branch was named-arg-BLIND: `String(alloc=a)`
        //   lowered the Arena value as CONTENT into `gorget_string_from_str`,
        //   reinterpreting the allocator struct as a Str (SIGSEGV / arena-overflow
        //   panic from safe code), and any 2-arg form fell through to an
        //   unintelligible cc/llc error.
        if name == "String" {
            let alloc_arg = args.iter().find(|a| {
                a.node.name.as_ref().map_or(false, |n| n.node == "alloc")
            });
            let cap_arg = args.iter().find(|a| {
                a.node.name.as_ref().map_or(false, |n| n.node == "cap")
            });
            let positional_args: Vec<&Spanned<ast::CallArg>> = args.iter()
                .filter(|a| a.node.name.is_none())
                .collect();
            // Exactly one content source (positional content/capacity OR cap=),
            // and no named args beyond cap=/alloc= (unknown names + multi-source
            // shapes are rejected at typecheck; don't intercept them here).
            let named_accounted =
                positional_args.len() + args.iter().filter(|a| {
                    a.node.name.as_ref().map_or(false, |n| n.node == "cap" || n.node == "alloc")
                }).count();
            let shape_ok = positional_args.len() + usize::from(cap_arg.is_some()) <= 1
                && named_accounted == args.len();
            if shape_ok {
                let owned_type = ctx.type_mapper.owned_string_type;
                // Lower the allocator FIRST and push the bracket so the ctor's
                // allocation (and the Str's recorded `alloc`) come from it.
                let bracketed = if let Some(alloc_a) = alloc_arg {
                    let alloc_op = lower_expr(ctx, builder, &alloc_a.node.value);
                    builder.push_allocator(alloc_op);
                    true
                } else {
                    false
                };
                let dst = if let Some(cap_a) = cap_arg {
                    let cap_op = lower_expr(ctx, builder, &cap_a.node.value);
                    ctx.call_extern_tracked(builder, "gorget_string_with_capacity", vec![cap_op], owned_type)
                } else if let Some(pos) = positional_args.first() {
                    let arg_op = lower_expr(ctx, builder, &pos.node.value);
                    // All 8 int widths route to the capacity ctor (shared predicate,
                    // `is_int_type_id`, same routing as the positional sibling in
                    // exprs/mod.rs); non-int/non-String args are rejected at typecheck.
                    let arg_type = infer_operand_type_full(ctx, &arg_op, builder);
                    let fn_name = if is_int_type_id(arg_type) {
                        "gorget_string_with_capacity"
                    } else {
                        "gorget_string_from_str"
                    };
                    ctx.call_extern_tracked(builder, fn_name, vec![arg_op], owned_type)
                } else if bracketed {
                    // Empty + alloc=: `gorget_string_from_str("")` returns the
                    // shared GORGET_EMPTY_STR view, which records NO allocator —
                    // the one-shot alloc= would silently not bind and growth
                    // would fall back to the global allocator. Route to
                    // with_capacity(0) (runtime clamps to a 16-byte minimum) so
                    // the Str records the allocator and growth sticks to it.
                    ctx.call_extern_tracked(
                        builder,
                        "gorget_string_with_capacity",
                        vec![Operand::Constant(Constant::I64(0))],
                        owned_type,
                    )
                } else {
                    ctx.call_extern_tracked(
                        builder,
                        "gorget_string_from_str",
                        vec![Operand::Constant(Constant::Str(String::new()))],
                        owned_type,
                    )
                };
                if bracketed {
                    builder.pop_allocator();
                }
                return FunctionBuilder::copy(dst);
            }
        }

        // format("...") → string interpolation or gorget_string_from_str
        if name == "format" && args.len() == 1 {
            if let Expr::StringLiteral(lit, interp_exprs) = &args[0].node.value.node {
                if lit.segments.iter().any(|s| matches!(s, StringSegment::Interpolation(_, _))) {
                    return lower_string_interpolation(ctx, builder, lit, interp_exprs);
                } else {
                    // Plain string literal → gorget_string_from_str(str_literal)
                    let str_op = lower_expr(ctx, builder, &args[0].node.value);
                    let owned_type = ctx.type_mapper.owned_string_type;
                    let dst = ctx.call_extern_tracked(builder, "gorget_string_from_str", vec![str_op], owned_type);
                    return FunctionBuilder::copy(dst);
                }
            }
        }

        // Allocator constructors → runtime functions. Name-aware
        // cap=/alloc=/positional split, mirroring the String-ctor branch
        // above (round-33). Every allocator's runtime ctor captures
        // `__gorget_current_alloc` as its parent — struct AND backing
        // buffer come from it, and destroy frees back into it
        // (runtime_arena_alloc.c gorget_arena_new; same shape in
        // runtime_{tlsf,pool,fixedbuf,fallback,tracking}_alloc.c) — so
        // `alloc=a` is the one-shot spelling of the documented `with`-
        // nesting semantics (language-reference §15.3 "Nesting": "An
        // allocator created inside a `with` block uses the outer allocator
        // for its own internal metadata"), delivered via the SAME push/pop
        // bracket as the collection ctors below. For TrackingAllocator the
        // parent is also the wrapped allocator (`t->inner = pa`), so
        // `TrackingAllocator(alloc=a)` means "instrument allocator `a`".
        // Before round-33 these branches were named-arg-BLIND:
        // `Arena(alloc=outer)` passed the Arena STRUCT as the byte
        // capacity (runtime panic on C, llc ptr-vs-i64 on LLVM), and any
        // named/0-arg shape fell past the arity-gated intercepts into an
        // unintelligible cc/llc/ld error. Shapes beyond the accepted ones
        // (unknown/duplicate named args, multi-source capacity, wrong
        // positional arity, non-int capacity) are rejected at typecheck —
        // don't intercept them here.
        //
        // (runtime ctor, Gorget type, positional arity). `None` arity =
        // one OPTIONAL capacity (cap= or a single positional); omitting it
        // passes 0, which hits the runtime default (Arena 4096, TLSF
        // 65536 — documented for TLSF in §15.3; `with Arena() as pool:`
        // is the design-doc flagship spelling). `Some(n)` = fixed n-arg
        // positional signature, no capacity axis.
        let alloc_ctor: Option<(&str, &str, Option<usize>)> = match name.as_str() {
            "Arena" => Some(("gorget_arena_new", "Arena", None)),
            "TlsfAllocator" => Some(("gorget_tlsf_new", "TlsfAllocator", None)),
            "FixedBufferAllocator" => Some(("gorget_fba_new", "FixedBufferAllocator", None)),
            "TrackingAllocator" => Some(("gorget_tracking_new", "TrackingAllocator", Some(0))),
            "PoolAllocator" => Some(("gorget_pool_new", "PoolAllocator", Some(2))),
            "FallbackAllocator" => Some(("gorget_fallback_new", "FallbackAllocator", Some(2))),
            _ => None,
        };
        if let Some((rt_fn, ty_name, fixed_arity)) = alloc_ctor {
            let alloc_arg = args.iter().find(|a| {
                a.node.name.as_ref().map_or(false, |n| n.node == "alloc")
            });
            let cap_arg = args.iter().find(|a| {
                a.node.name.as_ref().map_or(false, |n| n.node == "cap")
            });
            let positional_args: Vec<&Spanned<ast::CallArg>> = args.iter()
                .filter(|a| a.node.name.is_none())
                .collect();
            let named_accounted = positional_args.len() + args.iter().filter(|a| {
                a.node.name.as_ref().map_or(false, |n| n.node == "cap" || n.node == "alloc")
            }).count();
            let shape_ok = named_accounted == args.len()
                && match fixed_arity {
                    // Single optional capacity: cap= OR one positional.
                    None => positional_args.len() + usize::from(cap_arg.is_some()) <= 1,
                    // Fixed positional signature; no cap= axis.
                    Some(n) => positional_args.len() == n && cap_arg.is_none(),
                };
            if shape_ok {
                let alloc_type = ctx.type_mapper.lookup_named(ty_name).unwrap_or(I64_TYPE);
                // Lower the allocator FIRST and push the bracket so the
                // ctor's own struct + backing buffer come from it (the
                // runtime records it as parent, so destroy stays balanced).
                let bracketed = if let Some(alloc_a) = alloc_arg {
                    let alloc_op = lower_expr(ctx, builder, &alloc_a.node.value);
                    builder.push_allocator(alloc_op);
                    true
                } else {
                    false
                };
                let ctor_args: Vec<Operand> = match fixed_arity {
                    None => {
                        let cap_op = if let Some(cap_a) = cap_arg {
                            lower_expr(ctx, builder, &cap_a.node.value)
                        } else if let Some(pos) = positional_args.first() {
                            lower_expr(ctx, builder, &pos.node.value)
                        } else {
                            Operand::Constant(Constant::I64(0))
                        };
                        vec![cap_op]
                    }
                    Some(_) => positional_args.iter()
                        .map(|a| lower_expr(ctx, builder, &a.node.value))
                        .collect(),
                };
                let dst = builder.call_extern(rt_fn, ctor_args, alloc_type);
                if bracketed {
                    builder.pop_allocator();
                }
                return FunctionBuilder::copy(dst);
            }
        }

        // Channel[T](capacity) constructor → Channel__T__new(capacity).
        // Same name-aware cap=/alloc= split as the allocator ctors above —
        // §15.3 lists Channel among the alloc=-accepting ctors, and the
        // runtime snapshots `__gorget_current_alloc` into `ch->alloc` at
        // construction (channel_runtime.c gorget_channel_new), so the
        // bracket is sticky for the ring buffer and its waiter arrays.
        // Before round-33 this branch read args[0] blindly:
        // `Channel[int](alloc=a)` passed the Arena struct as the capacity
        // (NULL ring buffer → SIGSEGV on first send, BOTH backends) and
        // `Channel[int](cap=4, alloc=a)` silently ignored the allocator.
        if name == "Channel" {
            if let Some(type_args) = generic_args {
                if !type_args.is_empty() {
                    let mangled = super::super::types::mangle_generic_name(name, type_args);
                    let mangled = ctx.resolve_type_name(&mangled);
                    let chan_type = get_or_register_type(ctx, &mangled, None);
                    let alloc_arg = args.iter().find(|a| {
                        a.node.name.as_ref().map_or(false, |n| n.node == "alloc")
                    });
                    let cap_arg = args.iter().find(|a| {
                        a.node.name.as_ref().map_or(false, |n| n.node == "cap")
                    });
                    let positional_args: Vec<&Spanned<ast::CallArg>> = args.iter()
                        .filter(|a| a.node.name.is_none())
                        .collect();
                    let bracketed = if let Some(alloc_a) = alloc_arg {
                        let alloc_op = lower_expr(ctx, builder, &alloc_a.node.value);
                        builder.push_allocator(alloc_op);
                        true
                    } else {
                        false
                    };
                    let cap_op = if let Some(cap_a) = cap_arg {
                        lower_expr(ctx, builder, &cap_a.node.value)
                    } else if let Some(pos) = positional_args.first() {
                        lower_expr(ctx, builder, &pos.node.value)
                    } else {
                        Operand::Constant(Constant::I64(0))
                    };
                    let new_fn = format!("{mangled}__new");
                    let dst = builder.call(&new_fn, vec![cap_op], chan_type);
                    if bracketed {
                        builder.pop_allocator();
                    }
                    return FunctionBuilder::copy(dst);
                }
            }
        }

        // Shared[T](value) constructor → Shared__T__new(value)
        if name == "Shared" {
            if let Some(type_args) = generic_args {
                if !type_args.is_empty() {
                    let val_op = if !args.is_empty() {
                        lower_expr(ctx, builder, &args[0].node.value)
                    } else {
                        Operand::Constant(Constant::I64(0))
                    };
                    let val_type = infer_operand_type_full(ctx, &val_op, builder);
                    let mangled = super::super::types::mangle_generic_name(name, type_args);
                    let mangled = ctx.resolve_type_name(&mangled);
                    let vt = val_type;
                    let shared_type = get_or_register_type(ctx, &mangled, Some(&|c| ensure_shared_type_def(c, &mangled, vt)));
                    // Pack closure → GorgetClosure when the inner is a Callable
                    // alias. See `pack_closure_for_smart_ptr_ctor` for rationale.
                    let inner_c = mangled.strip_prefix("Shared__").unwrap_or("");
                    let val_op = pack_closure_for_smart_ptr_ctor(ctx, builder, val_op, inner_c);
                    let val_op = pack_trait_object_for_smart_ptr_ctor(ctx, builder, val_op, inner_c);
                    let new_fn = format!("{mangled}__new");
                    let dst = builder.call(&new_fn, vec![val_op.clone()], shared_type);
                    // Shared[T](v) takes ownership of v's data. Mark Move-type locals
                    // as moved so the drop elaborator skips them (avoids dangling ptr).
                    //
                    // Cluster 5 widening (2026-05-10): the gate is `needs_drop`
                    // (via is_resource_or_contains_resource), not the narrow
                    // is_resource_type_local. For `Shared[Option[Vector[int]]](v)`
                    // where v is Option[Vector[int]], the wrapper contains a heap
                    // pointer transitively — without MoveZero, the caller's drop
                    // at scope-exit and Shared's drop of the consumed payload
                    // race on the same heap allocation. Mirrors the same widening
                    // at the post-call MoveZero collection (this file:~1080).
                    // No fixture currently exercises Shared/Mutex with
                    // Option/Result-of-resource — correctness-driven for future
                    // code (mirrors spawn.rs:450 Cluster 3).
                    if let Operand::Copy(place) = &val_op {
                        if place.projections.is_empty()
                            && ctx.type_registry.is_resource_or_contains_resource(
                                builder.local_type(place.local))
                        {
                            builder.move_zero(place.clone());
                            ctx.drops.mark_moved(place.local);
                        }
                    }
                    // SCOUT-PROTO #1b (Defect B): tag the fresh Shared handle
                    // FreshOwned at birth (Core #3) so it MOVES into its
                    // consumer rather than tripping the "clone-if-Untracked"
                    // consuming-position branch. Sibling of the StructLiteral
                    // Shared ctor in exprs/mod.rs (Core #4, one fix all sites).
                    ctx.set_owned_fresh(builder, dst);
                    return FunctionBuilder::copy(dst);
                }
            }
        }

        // Mutex[T](value) constructor → Mutex__T__new(value)
        if name == "Mutex" {
            if let Some(type_args) = generic_args {
                if !type_args.is_empty() {
                    let val_op = if !args.is_empty() {
                        lower_expr(ctx, builder, &args[0].node.value)
                    } else {
                        Operand::Constant(Constant::I64(0))
                    };
                    let val_type = infer_operand_type_full(ctx, &val_op, builder);
                    let mangled = super::super::types::mangle_generic_name(name, type_args);
                    let mangled = ctx.resolve_type_name(&mangled);
                    let vt = val_type;
                    let mutex_type = get_or_register_type(ctx, &mangled, Some(&|c| ensure_mutex_type_def(c, &mangled, vt)));
                    let inner_c = mangled.strip_prefix("Mutex__").unwrap_or("");
                    let val_op = pack_closure_for_smart_ptr_ctor(ctx, builder, val_op, inner_c);
                    let val_op = pack_trait_object_for_smart_ptr_ctor(ctx, builder, val_op, inner_c);
                    let new_fn = format!("{mangled}__new");
                    let dst = builder.call(&new_fn, vec![val_op], mutex_type);
                    return FunctionBuilder::copy(dst);
                }
            }
        }

        // TaskGroup.new() static constructor
        if name == "TaskGroup" && args.is_empty() {
            let tg_name = "TaskGroup";
            let tg_type = if let Some(tid) = ctx.type_mapper.lookup_named(tg_name) {
                tid
            } else {
                let tid = ctx.type_registry.insert(GirType::Named(tg_name.to_string()));
                ctx.type_mapper.register_named(tg_name.to_string(), tid);
                ensure_task_group_type_def(ctx, tg_name);
                tid
            };
            let dst = builder.call("gorget_task_group_new", vec![], tg_type);
            return FunctionBuilder::copy(dst);
        }

        // AtomicInt(initial_value) → gorget_atomic_int_new(val)
        if name == "AtomicInt" && args.len() == 1 {
            let val_op = lower_expr(ctx, builder, &args[0].node.value);
            let at_type = ctx.type_mapper.lookup_named("AtomicInt").unwrap_or(I64_TYPE);
            let dst = builder.call_extern("gorget_atomic_int_new", vec![val_op], at_type);
            return FunctionBuilder::copy(dst);
        }

        // AtomicBool(initial_value) → gorget_atomic_bool_new(val)
        if name == "AtomicBool" && args.len() == 1 {
            let val_op = lower_expr(ctx, builder, &args[0].node.value);
            let at_type = ctx.type_mapper.lookup_named("AtomicBool").unwrap_or(BOOL_TYPE);
            let dst = builder.call_extern("gorget_atomic_bool_new", vec![val_op], at_type);
            return FunctionBuilder::copy(dst);
        }

        // Barrier(n) → gorget_barrier_new(n)
        if name == "Barrier" && args.len() == 1 {
            let n_op = lower_expr(ctx, builder, &args[0].node.value);
            let b_type = ctx.type_mapper.lookup_named("Barrier").unwrap_or(I64_TYPE);
            let dst = builder.call_extern("gorget_barrier_new", vec![n_op], b_type);
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
            let n_op = lower_expr(ctx, builder, &args[0].node.value);
            let s_type = ctx.type_mapper.lookup_named("Semaphore").unwrap_or(I64_TYPE);
            let dst = builder.call_extern("gorget_semaphore_new", vec![n_op], s_type);
            return FunctionBuilder::copy(dst);
        }

        // RWLock[T](initial_value) → RWLock__T__new(value)
        if name == "RWLock" {
            if let Some(type_args) = generic_args {
                if !type_args.is_empty() && !args.is_empty() {
                    let mangled = super::super::types::mangle_generic_name(name, type_args);
                    let mangled = ctx.resolve_type_name(&mangled);
                    let rw_type = get_or_register_type(ctx, &mangled, None);
                    let val_op = lower_expr(ctx, builder, &args[0].node.value);
                    let inner_c = mangled.strip_prefix("RWLock__").unwrap_or("");
                    let val_op = pack_closure_for_smart_ptr_ctor(ctx, builder, val_op, inner_c);
                    let val_op = pack_trait_object_for_smart_ptr_ctor(ctx, builder, val_op, inner_c);
                    let new_fn = format!("{mangled}__new");
                    let dst = builder.call(&new_fn, vec![val_op], rw_type);
                    return FunctionBuilder::copy(dst);
                }
            }
        }

        // thread_spawn(fn_name) → __gorget_thread_spawn_fn_name()
        // thread_spawn(fn_name, stack_size) → same symbol, stack_size rides as a typed
        // field (one size per fn; 0 = OS default = byte-identical to the 1-arg emit).
        // The intrinsic runs on RAW args BEFORE the default-fill, so a 1-arg call arrives
        // as 1 arg (stack_size 0) and a 2-arg call as 2 args; handle both arities.
        // V1: only bare function references supported. Closures are a follow-up (see TODO.md).
        if name == "thread_spawn" && (args.len() == 1 || args.len() == 2) {
            if let ast::Expr::Identifier(fn_name) = &args[0].node.value.node {
                let fn_name = fn_name.clone();
                // stack_size: 0 for the 1-arg form; for the 2-arg form, const-fold the
                // 2nd arg (a 0 or non-foldable value also routes to the plain wrapper).
                let stack_size = if args.len() == 2 {
                    match super::super::eval_const_expr(&args[1].node.value.node, &Default::default()) {
                        Some(Constant::I64(v)) => v,
                        _ => 0,
                    }
                } else {
                    0
                };
                let fn_ret_type = ctx.fn_sigs.get(fn_name.as_str())
                    .map(|(_, r)| *r)
                    .unwrap_or(I64_TYPE);
                // Payload name for the `Thread__{name}` symbols. MUST match the
                // declaration-side mangling of `Thread[T]` (the join call site
                // derives its symbol from the RECEIVER's type name, which for a
                // `Thread[float] t = ...` local is the declared `Thread__double`).
                // `c_type_name_for_id` spells primitives ("double", "bool",
                // "int32_t", ...) like the mangler; bare `type_name_for_id`
                // misses primitives and fell back to "int64_t", silently keying
                // a float payload's helpers on `Thread__int64_t`.
                let ret_name = if fn_ret_type == UNIT_TYPE {
                    "void".to_string()
                } else {
                    ctx.c_type_name_for_id(fn_ret_type)
                };
                let thread_name = format!("Thread__{ret_name}");
                let thread_type = get_or_register_type(ctx, &thread_name, None);
                // Typed payload channel (sibling of the TypeMapper protocol-
                // branch write for annotated `Thread[T]` types): an
                // unannotated `thread_spawn(f).join()` chain mints the handle
                // type HERE, so this site must record the payload too. Read
                // by the join/id intercept in `methods.rs`.
                ctx.type_mapper.thread_payload_types.insert(thread_type, fn_ret_type);
                ctx.spawn.thread_fns.entry(fn_name.clone())
                    .or_insert((fn_ret_type, ret_name, stack_size));
                let spawn_fn = format!("__gorget_thread_spawn_{fn_name}");
                let dst = builder.call(&spawn_fn, vec![], thread_type);
                return FunctionBuilder::copy(dst);
            }
        }

        // current_thread_id() → gorget_current_thread_id()
        if name == "current_thread_id" && args.is_empty() {
            let dst = builder.call_extern("gorget_current_thread_id", vec![], I64_TYPE);
            return FunctionBuilder::copy(dst);
        }

        // getpid() → gorget_getpid()
        if name == "getpid" && args.is_empty() {
            let dst = builder.call_extern("gorget_getpid", vec![], I64_TYPE);
            return FunctionBuilder::copy(dst);
        }

        // Collection constructors: Dict[K,V](), HashMap[K,V](), Set[K](), HashSet[K](),
        // Vector[T](), Deque[T]() — Array-kind Deque shares Vector's runtime.
        if matches!(name.as_str(), "Dict" | "HashMap" | "Set" | "HashSet" | "Vector" | "Deque") {
            if let Some(type_args) = generic_args {
                if !type_args.is_empty() {
                    let mangled = super::super::types::mangle_generic_name(name, type_args);
                    let mangled = ctx.resolve_type_name(&mangled);
                    // Register the collection type if not present
                    let coll_type = get_or_register_type(ctx, &mangled, None);
                    // Check for alloc= and cap= named arguments
                    let alloc_arg = args.iter().find(|a| {
                        a.node.name.as_ref().map_or(false, |n| n.node == "alloc")
                    });
                    let cap_arg = args.iter().find(|a| {
                        a.node.name.as_ref().map_or(false, |n| n.node == "cap")
                    });
                    let positional_args: Vec<&Spanned<ast::CallArg>> = args.iter()
                        .filter(|a| !a.node.name.as_ref().map_or(false, |n| n.node == "alloc" || n.node == "cap"))
                        .collect();

                    if positional_args.is_empty() {
                        let new_fn = format!("{mangled}__new");
                        // Tier 2a Phase 2A: collection constructors return
                        // a fresh heap allocation. Use `call_extern_tracked`
                        // which registers the result for drop AND tags
                        // ownership as Owned (consume-site validator
                        // accepts `(Owned, dead, _)` at downstream
                        // EnumInit/StructInit sites). Bumping to FreshOwned
                        // afterward signals the strictly-stronger
                        // "independent heap, no aliasing" axis.
                        if let Some(alloc_a) = alloc_arg {
                            let alloc_op = lower_expr(ctx, builder, &alloc_a.node.value);
                            builder.push_allocator(alloc_op);
                            let coll_local = ctx.call_extern_tracked(builder, &new_fn, vec![], coll_type);
                            ctx.set_owned_fresh(builder, coll_local);
                            builder.pop_allocator();
                            if let Some(cap_a) = cap_arg {
                                let cap_op = lower_expr(ctx, builder, &cap_a.node.value);
                                let ptr_type = ctx.type_registry.insert(crate::ir::types::GirType::MutPtr(coll_type));
                                let ptr = builder.borrow_mut(Place::local(coll_local), ptr_type);
                                let reserve_fn = format!("{mangled}__reserve");
                                builder.call_extern_void(&reserve_fn, vec![FunctionBuilder::copy(ptr), cap_op]);
                            }
                            return FunctionBuilder::copy(coll_local);
                        } else {
                            let coll_local = ctx.call_extern_tracked(builder, &new_fn, vec![], coll_type);
                            ctx.set_owned_fresh(builder, coll_local);
                            if let Some(cap_a) = cap_arg {
                                let cap_op = lower_expr(ctx, builder, &cap_a.node.value);
                                let ptr_type = ctx.type_registry.insert(crate::ir::types::GirType::MutPtr(coll_type));
                                let ptr = builder.borrow_mut(Place::local(coll_local), ptr_type);
                                let reserve_fn = format!("{mangled}__reserve");
                                builder.call_extern_void(&reserve_fn, vec![FunctionBuilder::copy(ptr), cap_op]);
                            }
                            return FunctionBuilder::copy(coll_local);
                        }
                    }
                    // Fall through for positional args — type is registered, regular call will use correct return type
                }
            }
        }

        // Determine effective function name (mangled if generic call).
        // For meta op calls, also append per-op suffixes so the name matches
        // the mangled name produced by GenericCollector::register_instance_with_ops.
        let effective_name = if let Some(type_args) = generic_args {
            if !type_args.is_empty() {
                let mut mangled = super::super::types::mangle_generic_name(name, type_args);
                // Append __<op_suffix> for each MetaOpToken arg (same order as params)
                for arg in args.iter() {
                    if let Expr::MetaOpToken(op) = &arg.node.value.node {
                        mangled.push_str("__");
                        mangled.push_str(super::super::types::op_mangle_suffix(*op));
                    }
                }
                // Apply type name substitutions for generic monomorphization
                ctx.resolve_type_name(&mangled)
            } else {
                name.clone()
            }
        } else {
            name.clone()
        };

        // Cross-module resolution: when the semantic resolver mapped this call site
        // to a specific module-qualified function, use the mangled name.  This prevents
        // bare-name collisions when multiple modules define the same function name
        // (e.g., `parse_float` in both std.conv and game.entity_parser).
        let effective_name = if let Some(resolved) = ctx.call_resolved_names.get(&callee.span.start) {
            resolved.clone()
        } else {
            effective_name
        };

        // Check if this is an Option/Result variant constructor — resolve with type-aware logic
        {
            let call_arg_values: Vec<Spanned<Expr>> = args.iter()
                .map(|a| a.node.value.clone())
                .collect();
            if let Some(result) = resolve_option_result_variant(ctx, builder, name, &call_arg_values) {
                return result;
            }
        }

        // Check if this is an enum variant constructor.
        // SSOT: honour the typechecker-determined expected type to disambiguate
        // same-named variants across enums (e.g. Type.TArray vs CRuntimeType.TArray).
        if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant_typed(&effective_name, ctx.func_state.expected_type) {
            let arg_spans: Vec<Option<crate::span::Span>> = args.iter()
                .map(|a| Some(a.node.value.span))
                .collect();
            let ast_args: Vec<_> = args.iter().map(|a| a.node.value.clone()).collect();
            // Look up variant field types so that nested constructors like
            // `R2.A(Some(s))` see the expected `Option[GorgetString]` payload
            // type when lowering the inner `Some(s)`. Without this, `Some(s)`
            // infers from the operand type of `s` (a `*GorgetString` borrow)
            // and produces `Option[*GorgetString]` (Option__T<n>) — the
            // resulting struct is 16 bytes vs the variant's 40-byte slot,
            // and the memcpy into the variant payload reads past the source.
            let field_types: Vec<Option<TypeId>> = ctx.type_registry
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
                    if let Some(ft) = field_types.get(i).and_then(|f| *f) {
                        ctx.func_state.expected_type = Some(ft);
                    }
                    let op = lower_expr(ctx, builder, &arg.node.value);
                    // Snag #46: auto-propagate Result→T at the variant-field boundary.
                    let op = super::maybe_auto_propagate(ctx, builder, op, arg.node.value.span);
                    ctx.func_state.expected_type = prev;
                    op
                })
                .collect();
            // Clone multi-use resource args that can't be safely moved into the enum variant.
            super::clone_multi_use_resource_args(ctx, builder, &mut field_operands, &ast_args);
            let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
            let dst = ctx.emit_enum_init_owned(builder, &enum_name, &variant_name, type_id, field_operands, Some(arg_spans));
            return FunctionBuilder::copy(dst);
        }
        // Also check base name for non-generic enum variants.
        // SSOT: type-aware to disambiguate same-named variants across enums.
        if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant_typed(name, ctx.func_state.expected_type) {
            let arg_spans: Vec<Option<crate::span::Span>> = args.iter()
                .map(|a| Some(a.node.value.span))
                .collect();
            let ast_args: Vec<_> = args.iter().map(|a| a.node.value.clone()).collect();
            let field_types: Vec<Option<TypeId>> = ctx.type_registry
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
                    if let Some(ft) = field_types.get(i).and_then(|f| *f) {
                        ctx.func_state.expected_type = Some(ft);
                    }
                    let op = lower_expr(ctx, builder, &arg.node.value);
                    // Snag #46: auto-propagate Result→T at the variant-field boundary.
                    let op = super::maybe_auto_propagate(ctx, builder, op, arg.node.value.span);
                    ctx.func_state.expected_type = prev;
                    op
                })
                .collect();
            // Clone multi-use resource args for enum variant init.
            super::clone_multi_use_resource_args(ctx, builder, &mut field_operands, &ast_args);
            let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
            let dst = ctx.emit_enum_init_owned(builder, &enum_name, &variant_name, type_id, field_operands, Some(arg_spans));
            return FunctionBuilder::copy(dst);
        }

        // Check if this is a closure variable call (e.g., `add_x(5)` where `add_x` is a closure)
        if let Some((local_id, local_type_id)) = ctx.lookup_local(&effective_name) {
            let type_name = ctx.type_name_for_id(local_type_id);
            if let Some(type_name) = type_name {
                let type_name = type_name.to_string();
                if let Some((call_fn, _, _)) = ctx.lookup_closure_info(&type_name) {
                    let call_fn = call_fn.to_string();
                    // Closure call: __Closure_N__call(&closure_var, args...)
                    // The __call function expects a pointer to the closure struct
                    let ptr_type = ctx.type_registry.insert(GirType::Ptr(local_type_id));
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow(ptr_local, Place::local(local_id));
                    let mut call_args = vec![FunctionBuilder::copy(ptr_local)];
                    // Route closure args through lower_call_arg for unified Ptr ABI
                    let sig_params = ctx.fn_sigs.get(call_fn.as_str()).map(|(p, _)| p.clone());
                    for (i, arg) in args.iter().enumerate() {
                        let param_type = sig_params.as_ref().and_then(|p| p.get(i + 1).copied());
                        call_args.push(lower_call_arg(ctx, builder, arg, param_type, &call_fn, i + 1));
                    }
                    let ret_type = if let Some((_, ret)) = ctx.fn_sigs.get(call_fn.as_str()) {
                        *ret
                    } else {
                        I64_TYPE
                    };
                    if ret_type == UNIT_TYPE {
                        builder.call_void(call_fn, call_args);
                        return Operand::Constant(Constant::Unit);
                    } else {
                        let dst = builder.call(call_fn, call_args, ret_type);
                        return FunctionBuilder::copy(dst);
                    }
                }
            }
            // Callable parameter call: local exists with void* type (UNIT_TYPE)
            // Emit as __callable_N where N is the local ID, which the C backend
            // will recognize and emit as an indirect function pointer call.
            if local_type_id == UNIT_TYPE {
                let callable_name = format!("__callable_{}", local_id.0);
                // TRACK B1 SIGSEGV FIX (write-site).
                //
                // Track B1 root: this arm used to decide pointer-vs-value from the
                // ARGUMENT LOCAL's provenance (`is_param_borrow_unique` — was the
                // arg a borrow param?) — which meant a `Callable[void(&int)]`
                // called with a plain-local arg forwarded the VALUE bits, and
                // the closure body's write-through dereferenced them as a
                // pointer → SIGSEGV on BOTH backends (C + LLVM). The correct
                // signal is the CALLEE's declared param type + ownership spelled
                // on the callable, carried through the parallel
                // `callable_param_types` / `callable_param_ownerships` sidecars
                // (populated at the four param registration sites plus the
                // var-decl site — every place that already sets
                // `callable_return_types`). We rewire the arg emit to
                // `lower_call_arg` — the sigil+callee-aware path every other
                // call uses — by transplanting the sidecars into the
                // conventional `fn_sigs` / `fn_param_ownerships` axes under the
                // synthetic `callable_name` key. `lower_call_arg` then picks the
                // pointer-vs-value forwarding exactly as if this were a direct
                // call to a function of that signature. When the sidecars are
                // missing (untyped closure whose signature was never
                // registered), the legacy fallback below preserves the
                // pre-existing behaviour so the change is strictly additive.
                let sig_params: Option<Vec<TypeId>> = ctx.callable_param_types(local_id).map(|s| s.to_vec());
                let sig_owns: Option<Vec<Ownership>> = ctx.callable_param_ownerships(local_id).map(|s| s.to_vec());
                let mut call_args = vec![FunctionBuilder::copy(local_id)];
                if let (Some(sig_params), Some(sig_owns)) = (sig_params, sig_owns) {
                    // Transplant into the axes lower_call_arg reads. Idempotent
                    // — the synthetic name embeds `local_id` so two writes for
                    // the same callable local match.
                    ctx.fn_sigs.insert(callable_name.clone(), (sig_params.clone(), UNIT_TYPE));
                    ctx.fn_param_ownerships.insert(callable_name.clone(), sig_owns.clone());
                    for (i, arg) in args.iter().enumerate() {
                        // Track B1 A-2 Option (b), 2026-07-27: the pre-fix
                        // "already-a-pointer bare-arg forwarding" shortcut (a
                        // bare-arg `cb(a)` on a `&`-param local, skipping
                        // `lower_call_arg` — and with it `cow_before_mutation`)
                        // used to sit here. Retired to the sanctioned path — the
                        // fast-path lives inside `lower_call_arg` now (see the
                        // `Ownership::Borrow && callee_passes_by_ptr` special-case
                        // at the top of that fn), so ALL arg loops route through
                        // ONE gate and `cow_before_mutation` becomes a hard
                        // invariant, not a bypass-conditional call.
                        let param_type = sig_params.get(i).copied();
                        call_args.push(lower_call_arg(ctx, builder, arg, param_type, &callable_name, i));
                    }
                } else {
                    for arg in args {
                        // Legacy fallback: for borrow params passed to callable,
                        // preserve the pointer (don't auto-deref). The adapter
                        // function expects the pointer type.
                        if let Expr::Identifier(arg_name) = &arg.node.value.node {
                            if let Some((arg_local, _)) = ctx.lookup_local(arg_name) {
                                if ctx.is_param_borrow_unique(builder, arg_local) {
                                    call_args.push(FunctionBuilder::copy(arg_local));
                                    continue;
                                }
                            }
                        }
                        let val = lower_expr(ctx, builder, &arg.node.value);
                        // Auto-deref Ptr(T) → T for non-resource value types. A
                        // closure declared `(Entity e): ...` expects an Entity by
                        // value, but the caller's local may hold a Ref[Entity]
                        // (from a collection `.get().unwrap()` or a `Ref[T]`
                        // field). Resource types stay as Ptr since their adapter
                        // expects the pointer form.
                        let val = ctx.auto_clone_if_ptr(builder, val, arg.span);
                        call_args.push(val);
                    }
                }
                // Look up tracked callable return type, fall back to I64_TYPE
                let ret_type = ctx.callable_return_type(local_id).unwrap_or(I64_TYPE);
                if ret_type == UNIT_TYPE {
                    builder.call_void(callable_name, call_args);
                    return Operand::Constant(Constant::Unit);
                }
                let dst = builder.call(callable_name, call_args, ret_type);
                return FunctionBuilder::copy(dst);
            }
            // FnPtr-typed local: escaped closure returned from a function, stored as GorgetClosure.
            // Emit __gorget_closure_call_N; the C backend expands it to fn_ptr+env dispatch.
            if let Some(GirType::FnPtr { return_type: fn_ret, .. }) = ctx.type_registry.get(local_type_id).cloned() {
                let callable_name = format!("__gorget_closure_call_{}", local_id.0);
                // TRACK B1 SIGSEGV FIX (write-site, LOCAL cell). Same class as
                // the UNIT_TYPE arm above; same fix. This arm used to lower
                // every argument through `lower_expr(&arg.node.value)`, which
                // strips the outer `arg.node.ownership` sigil AND ignores the
                // FnPtr's own declared param types — so a
                // `Callable[void(&int)] cb = bump; cb(&a)` LOCAL forwarded the
                // VALUE of `a` and the closure body's write-through
                // segfaulted on it.
                let sig_params: Option<Vec<TypeId>> = ctx.callable_param_types(local_id).map(|s| s.to_vec());
                let sig_owns: Option<Vec<Ownership>> = ctx.callable_param_ownerships(local_id).map(|s| s.to_vec());
                let mut call_args = vec![FunctionBuilder::copy(local_id)];
                if let (Some(sig_params), Some(sig_owns)) = (sig_params, sig_owns) {
                    ctx.fn_sigs.insert(callable_name.clone(), (sig_params.clone(), fn_ret));
                    ctx.fn_param_ownerships.insert(callable_name.clone(), sig_owns.clone());
                    for (i, arg) in args.iter().enumerate() {
                        // Track B1 A-2 Option (b), 2026-07-27: same retirement as
                        // the UNIT_TYPE arm above. The bare-arg-`is_param_borrow_unique`
                        // fast-path lives inside `lower_call_arg` now, on the
                        // sanctioned path with `cow_before_mutation`.
                        let param_type = sig_params.get(i).copied();
                        call_args.push(lower_call_arg(ctx, builder, arg, param_type, &callable_name, i));
                    }
                } else {
                    for arg in args {
                        call_args.push(lower_expr(ctx, builder, &arg.node.value));
                    }
                }
                if fn_ret == UNIT_TYPE {
                    builder.call_void(callable_name, call_args);
                    return Operand::Constant(Constant::Unit);
                } else {
                    let dst = builder.call(callable_name, call_args, fn_ret);
                    return FunctionBuilder::copy(dst);
                }
            }
        }

        // Regular function call (use effective name for generic functions)
        // Filter out MetaOpToken args — they are compile-time only and have no
        // runtime representation in the lowered GIR call.
        let runtime_args_buf: Vec<Spanned<ast::CallArg>>;
        let runtime_args: &[Spanned<ast::CallArg>] =
            if args.iter().any(|a| matches!(a.node.value.node, Expr::MetaOpToken(_))) {
                runtime_args_buf = args
                    .iter()
                    .filter(|a| !matches!(a.node.value.node, Expr::MetaOpToken(_)))
                    .cloned()
                    .collect();
                &runtime_args_buf
            } else {
                args
            };
        // Resolve named args + default params before lowering
        let resolved_args = resolve_call_args(ctx, &effective_name, runtime_args);
        // Extract parameter types to thread expected_type for dot-shorthand args
        let param_types: Vec<TypeId> = ctx.fn_sigs.get(effective_name.as_str())
            .map(|(params, _)| params.clone())
            .unwrap_or_default();
        // Save pending_move_zeros baseline so we only drain entries added
        // by THIS call's argument lowering (not from nested/prior calls).
        let move_zero_baseline = ctx.func_state.pending_move_zeros.len();
        let temp_drop_baseline = ctx.func_state.pending_temp_drops.len();
        let mut lowered_args: Vec<Operand> = resolved_args
            .iter()
            .enumerate()
            .map(|(i, arg)| {
                let prev_expected = ctx.func_state.expected_type;
                let callee_pt = param_types.get(i).copied();
                if let Some(pt) = callee_pt {
                    ctx.func_state.expected_type = Some(pt);
                }
                let op = lower_call_arg(ctx, builder, arg, callee_pt, &effective_name, i);
                // Snag #35: a throws-call result at this arg site is a
                // Result[T, E] operand. The typecheck pass already
                // certified that this is either a capture (param type is
                // Result[T, E], the destination grabs the whole Result —
                // no unwrap) or an auto-propagation (param type is T and
                // the enclosing function can propagate — unwrap here).
                // maybe_auto_propagate's `expected_type` check returns the
                // operand unchanged for the capture case and emits the
                // Result-unwrap-or-return chain for the propagation case.
                let op = super::maybe_auto_propagate(ctx, builder, op, arg.node.value.span);
                ctx.func_state.expected_type = prev_expected;
                op
            })
            .collect();

        // Collect Move-ownership Move-type arg locals for post-call MoveZero.
        // Resolve the original source local from the arg expression (not the
        // lowered MutPtr, which is_resource_type_local doesn't recognize).
        //
        // Cluster 5 widening (2026-05-10): the gate is `needs_drop` (via
        // is_resource_or_contains_resource), not the narrow is_resource_type.
        // For `f(!opt_vec)` where opt_vec is Option[Vector[int]], the wrapper
        // contains a heap pointer transitively — without MoveZero, the
        // caller's drop at scope-exit and the callee's drop of the consumed
        // arg race on the same heap allocation. Mirrors the spawn.rs:450
        // Cluster 3 widening (`is_resource_or_contains_resource` on closure
        // captures). No fixture currently exercises an Option/Result-of-
        // resource via `!arg` ownership — the widening is correctness-
        // driven for future code.
        let move_zero_locals: Vec<Place> = resolved_args.iter()
            .filter_map(|arg| {
                if !matches!(arg.node.ownership, Ownership::Move) { return None; }
                if let Expr::Identifier(name) = &arg.node.value.node {
                    if let Some((local_id, _)) = ctx.lookup_local(name) {
                        let local_ty = builder.local_type(local_id);
                        if ctx.type_registry.is_resource_or_contains_resource(local_ty) {
                            return Some(Place::local(local_id));
                        }
                    }
                }
                None
            })
            .collect();

        // Suggest `!arg` for last-use resource-type arguments where the callee
        // clones the param at an ownership boundary. Both conditions must hold:
        // (1) the callee actually clones this param (recorded in fn_consumed_params)
        // (2) the caller's argument is the last use of a named resource-type local
        // Resolve the callee name through extern_bindings (same as the call emission path).
        let resolved_callee = ctx.extern_bindings.get(effective_name.as_str())
            .cloned()
            .unwrap_or_else(|| effective_name.clone());
        if let Some(consumed) = ctx.fn_consumed_params.get(resolved_callee.as_str()).cloned() {
            let param_names = ctx.fn_param_names.get(effective_name.as_str())
                .or_else(|| ctx.fn_param_names.get(resolved_callee.as_str()))
                .cloned();
            if let Some(param_names) = param_names {
                for (i, arg) in resolved_args.iter().enumerate() {
                    if matches!(arg.node.ownership, Ownership::Move) { continue; } // already !
                    if let Expr::Identifier(ref arg_name) = arg.node.value.node {
                        if let Some(pname) = param_names.get(i) {
                            if consumed.contains(pname) {
                                if let Some((local_id, _)) = ctx.lookup_local(arg_name) {
                                    let local_type = builder.local_type(local_id);
                                    let is_resource = ctx.type_registry.is_resource_type(local_type)
                                        || ctx.pointee_type(local_type)
                                            .map_or(false, |inner| ctx.type_registry.is_resource_type(inner));
                                    if is_resource && ctx.is_last_use_at(arg_name, arg.span) {
                                        let type_name = ctx.type_registry.type_name(local_type)
                                            .map(|n| crate::ir::lowering::context::demangle_type_name(&n))
                                            .unwrap_or_else(|| "resource".to_string());
                                        ctx.move_suggestions.push(crate::ir::MoveSuggestion {
                                            span: arg.node.value.span,
                                            name: arg_name.clone(),
                                            type_name,
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        let ret_type = if let Some((_, ret)) = ctx.fn_sigs.get(effective_name.as_str()) {
            *ret
        } else {
            I64_TYPE // fallback
        };

        // Cross-frame fault propagation (error-model.md §11, Inc-2.1a/2.1c): does
        // the callee PARTICIPATE? If so it has a synthesized trailing `MutPtr<i32>`
        // fault-slot param, so EVERY direct caller must pass the trailing arg
        // (uniform signature, D5). Keyed by the USER name (`effective_name`),
        // which is also the C `call_name` for participating fns (they're never
        // extern).
        //
        // Per-category handler resolution (FOLD-6): when this call site is INSIDE
        // a fault scope, resolve BOTH arithmetic categories to ALWAYS-SOME blocks
        // — the user's catch entry if the scope catches that category, else the
        // scope's panic block (so an uncaught-by-this-scope category that the
        // callee CAN raise re-panics automatically, mirroring the local
        // `FaultableBinOp` precedent). The emitted `FaultableCall` then carries a
        // per-category tag-dispatch (§2.3 silent-miscompile fix). When NOT inside
        // a fault scope, both stay `None` → pass NULL + a plain `Call`
        // (panic-by-default).
        let callee_participates_in_fault = ctx.participates_in_fault(&effective_name);
        let (fault_overflow_handler, fault_divzero_handler, fault_bounds_handler) = if callee_participates_in_fault {
            match ctx.func_state.fault_scope {
                Some(s) => (
                    Some(s.overflow_handler.unwrap_or(s.div_overflow_panic)),
                    Some(s.divzero_handler.unwrap_or(s.div_zero_panic)),
                    Some(s.bounds_handler.unwrap_or(s.bounds_panic)),
                ),
                None => (None, None, None),
            }
        } else {
            (None, None, None)
        };

        // Resolve extern bindings: use the C symbol name instead of the Gorget name
        let call_name = if let Some(c_symbol) = ctx.extern_bindings.get(effective_name.as_str()) {
            c_symbol.clone()
        } else {
            effective_name
        };

        // Unregister GorgetString temps when the callee might store str views.

        // Collect drop-registered collection TEMPS (not named variables) passed
        // as args. These need move-zero after the call to prevent double-free:
        // the callee received a shallow copy of the buffer, so the caller must
        // relinquish ownership of the anonymous temp.
        // Named variables (e.g., `len(nums)`) must NOT be zeroed — caller still owns them.
        let collection_arg_locals: Vec<LocalId> = lowered_args.iter()
            .filter_map(|op| {
                if let Operand::Copy(place) | Operand::Move(place) = op {
                    if place.projections.is_empty()
                        && !ctx.is_named_local(place.local)
                        && ctx.drops.is_registered(place.local)
                        && !ctx.drops.is_moved(place.local)
                    {
                        let ty = builder.local_type(place.local);
                        if ctx.type_registry.is_collection_type(ty) {
                            return Some(place.local);
                        }
                    }
                }
                None
            })
            .collect();

        // Upgrade consuming call args from Copy to Move (Rust-style ownership
        // on operand).  Enables generic LIR post-call zeroing.
        for arg in lowered_args.iter_mut() {
            if let Operand::Copy(place) = arg {
                if place.projections.is_empty() {
                    let dominated = move_zero_locals.iter().any(|mz| mz.local == place.local)
                        || collection_arg_locals.contains(&place.local);
                    if dominated {
                        *arg = Operand::Move(place.clone());
                    }
                }
            }
        }

        // Cross-frame fault: append the trailing fault-slot arg for a
        // participating callee. A CATCHING caller (inside ANY arith fault scope —
        // Overflow OR DivByZero, FOLD-1) allocates a zero-init `i32` slot and
        // passes `&slot`; a NON-CATCHING caller passes `Constant::Null` (→ C
        // `NULL`) and the callee's fault arm panics by default. `fault_slot_place`
        // is `Some` only for the catching path (the FaultableCall reads its tag
        // after the call, branch-before-read).
        let fault_slot_place: Option<Place> = if callee_participates_in_fault {
            if fault_overflow_handler.is_some() || fault_divzero_handler.is_some() || fault_bounds_handler.is_some() {
                // Catching caller: zero-init slot + pass `&slot`.
                let slot = builder.add_local(I32_TYPE, Some("__fault_slot"));
                builder.assign(Place::local(slot), FunctionBuilder::const_i32(0));
                let slot_ptr_ty = ctx.register_mut_ptr_type(I32_TYPE);
                let slot_ref = builder.borrow_mut(Place::local(slot), slot_ptr_ty);
                lowered_args.push(FunctionBuilder::copy(slot_ref));
                Some(Place::local(slot))
            } else {
                // Non-catching caller: pass NULL, plain Call, panic-by-default.
                lowered_args.push(Operand::Constant(Constant::Null));
                None
            }
        } else {
            None
        };

        let result = if let Some(slot_place) = fault_slot_place {
            // Catching deep call: emit a FaultableCall threading BOTH per-category
            // (always-Some) handlers. The GIR→LIR split adds the tag-dispatch
            // AFTER the call (branch-before-read to the matching category entry).
            // The result is read only on the no-fault continuation.
            if ret_type == UNIT_TYPE {
                builder.fault_call_void(&call_name, lowered_args, slot_place, fault_overflow_handler, fault_divzero_handler, fault_bounds_handler);
                Operand::Constant(Constant::Unit)
            } else {
                let dst = builder.fault_call(&call_name, lowered_args, ret_type, slot_place, fault_overflow_handler, fault_divzero_handler, fault_bounds_handler);
                if ctx.type_registry.needs_drop(ret_type) {
                    ctx.drops.register_local(dst, ret_type, &ctx.type_registry);
                }
                ctx.set_owned(builder, dst);
                FunctionBuilder::copy(dst)
            }
        } else if ret_type == UNIT_TYPE {
            builder.call_void(&call_name, lowered_args);
            Operand::Constant(Constant::Unit)
        } else {
            let dst = ctx.call_tracked(builder, &call_name, lowered_args, ret_type);
            // `extern borrowed T f(...)` — the callee returned a non-owning
            // alias into FFI-owned storage. Materialize an independent owned
            // copy at the call boundary so the caller's slot survives
            // subsequent FFI state mutations that may invalidate the buffer
            // (e.g. SDL_GetError, errno-style accessors, libc strerror).
            //
            // Mirrors the by-value-resource branch of `ensure_owned_at_boundary`:
            // unregister the borrowed alias from drop tracking (its buffer
            // belongs to the FFI; we must not free it), clone via the type's
            // owned-clone routine, then register and set_owned on the clone.
            //
            // Keyed by the resolved C symbol (`call_name`) — the lowering
            // pass at `mod.rs` inserts BOTH the Gorget name and the C symbol
            // into `fn_returns_borrowed`, so either lookup hits.
            if ctx.fn_returns_borrowed.contains(call_name.as_str()) {
                if let Some(clone_fn) = ctx.clone_fn_for_ptr(ret_type) {
                    ctx.drops.unregister(dst);
                    ctx.warn_clone_and_hit(
                        builder,
                        callee.span,
                        ret_type,
                        crate::ir::ImplicitCloneReason::BorrowedExternReturn,
                    );
                    let cloned = builder.call_clone(
                        &clone_fn,
                        vec![FunctionBuilder::copy(dst)],
                        ret_type,
                        crate::ir::ImplicitCloneReason::BorrowedExternReturn,
                    );
                    ctx.drops.register_local(cloned, ret_type, &ctx.type_registry);
                    ctx.set_owned(builder, cloned);
                    FunctionBuilder::copy(cloned)
                } else {
                    FunctionBuilder::copy(dst)
                }
            } else {
                FunctionBuilder::copy(dst)
            }
        };

        // MoveZero Move-ownership args.  The LIR's emit_post_call_zeros handles
        // args directly in lowered_args as Operand::Move; the GIR MoveZero
        // covers args wrapped in borrow ptrs.
        for place in &move_zero_locals {
            builder.move_zero(place.clone());
            ctx.drops.mark_moved(place.local);
        }

        // MoveZero collection temps passed as args.
        for local in &collection_arg_locals {
            ctx.move_zero_and_mark(builder, *local);
        }

        // MoveZero locals from Move-argument lowering (e.g., !expr.clone()).
        // These were borrowed (borrow_mut) for the callee; now that the call
        // has returned, zero the source to prevent double-free at scope exit.
        // Only drain entries added during THIS call's arg lowering.
        let pending: Vec<LocalId> = ctx.func_state.pending_move_zeros.drain(move_zero_baseline..).collect();
        for local in pending {
            builder.move_zero(Place::local(local));
            ctx.drops.mark_moved(local);
        }

        // Drop owning temporaries that were materialized as borrow-arguments
        // for THIS call. The callee only borrowed them; their temporary
        // lifetime ends here, so free them now (prevents arg-temp leaks).
        let temp_drops: Vec<LocalId> = ctx.func_state.pending_temp_drops.drain(temp_drop_baseline..).collect();
        for local in temp_drops {
            // Unconditional: a bare-borrow callee never moves the temp, so it is
            // always alive here. (DropIfAlive would be stripped — the temp isn't
            // in drop_elab's tracked flag set, so its flag defaults to false.)
            builder.drop(Place::local(local));
        }

        // `noreturn` extern calls (exit, abort, …) never return to the caller.
        // Terminate the basic block with `unreachable` so divergent uses
        // (e.g. an Error-arm `exit(2)` in a `T x = match …` expression) compose
        // with the surrounding result type — the match-expr lowerer's
        // `is_terminated()` check then correctly skips the arm-value assign.
        if ctx.noreturn_fns.contains(call_name.as_str()) {
            builder.unreachable();
        }

        result
    } else if let Expr::Closure { params, body, is_move, .. } = &callee.node {
        // IIFE: ((int x): x * x)(5) — inline closure called immediately
        let mut cl = std::mem::take(&mut ctx.closures);
        let closure_op = cl.lower_closure(ctx, builder, params, body, *is_move, callee.span);
        ctx.closures = cl;

        if let Operand::Copy(ref place) | Operand::Move(ref place) = closure_op {
            if place.projections.is_empty() {
                let closure_local = place.local;
                let closure_type_id = builder.local_type(closure_local);
                if let Some(type_name) = ctx.type_name_for_id(closure_type_id).map(|s| s.to_string()) {
                    if let Some((call_fn, _, _)) = ctx.lookup_closure_info(&type_name) {
                        let call_fn = call_fn.to_string();
                        // Build args: pointer to closure struct + call arguments
                        let ptr_type = ctx.type_registry.insert(GirType::Ptr(closure_type_id));
                        let ptr_local = builder.add_local(ptr_type, None);
                        builder.emit_borrow(ptr_local, Place::local(closure_local));
                        let mut call_args = vec![FunctionBuilder::copy(ptr_local)];
                        // Route IIFE args through lower_call_arg for unified Ptr ABI
                        let sig_params = ctx.fn_sigs.get(call_fn.as_str()).map(|(p, _)| p.clone());
                        for (i, arg) in args.iter().enumerate() {
                            let param_type = sig_params.as_ref().and_then(|p| p.get(i + 1).copied());
                            call_args.push(lower_call_arg(ctx, builder, arg, param_type, &call_fn, i + 1));
                        }
                        let ret_type = if let Some((_, ret)) = ctx.fn_sigs.get(call_fn.as_str()) {
                            *ret
                        } else {
                            I64_TYPE
                        };
                        if ret_type == UNIT_TYPE {
                            builder.call_void(&call_fn, call_args);
                            return Operand::Constant(Constant::Unit);
                        } else {
                            let dst = builder.call(&call_fn, call_args, ret_type);
                            return FunctionBuilder::copy(dst);
                        }
                    }
                }
            }
        }
        // Fallback if closure info not found
        Operand::Constant(Constant::Unit)
    } else {
        // Non-identifier, non-closure callee — typically an expression that
        // produces a `Callable` value (e.g. `shared_callable.get()()`,
        // `make_adder(3)(5)`, `arr[0](x)`). Lower the callee to a value, then
        // dispatch via the LIR `__gorget_closure_call_N` shape — `insts.rs`
        // promotes that name to `Inst::CallClosure` regardless of whether the
        // GIR type is `FnPtr` or a Callable family alias (typed via
        // `c_runtime_alias = "GorgetClosure"`).
        let callee_op = lower_expr(ctx, builder, callee);
        let callee_local = match &callee_op {
            Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
                place.local
            }
            _ => {
                // Materialise into a local so we have a stable ValueId to
                // pass through __gorget_closure_call_N.
                let ty = infer_operand_type_full(ctx, &callee_op, builder);
                let tmp = builder.add_local(ty, None);
                builder.assign(Place::local(tmp), callee_op);
                tmp
            }
        };
        let callee_type_id = builder.local_type(callee_local);

        // Resolve sig (params, ownerships, return) from the FnPtr GIR type.
        // TRACK K: previously this arm only pulled `return_type` and lowered
        // every arg via naive `lower_expr` — dropping the `&`-sigil on args
        // to a callable stored in a Vector/Dict, so `arr[0](&a)` on a
        // `Callable[void(int &)]` element forwarded the VALUE of `a` and the
        // callee's write-through segfaulted on both backends. The
        // `Vector[Callable[...]]` element-type inferrer at `methods.rs`
        // now consults `callable_alias_sigs` to produce the FULL FnPtr
        // signature — so `sig_params` / `sig_owns` are populated whenever
        // the callable was declared with a spelled signature.
        let (sig_params, sig_owns, ret_type) = match ctx.type_registry.get(callee_type_id).cloned() {
            Some(GirType::FnPtr { params, return_type, param_ownerships }) => {
                (Some(params), Some(param_ownerships), return_type)
            }
            _ => (None, None, I64_TYPE),
        };

        let callable_name = format!("__gorget_closure_call_{}", callee_local.0);
        let mut call_args = vec![FunctionBuilder::copy(callee_local)];
        if let (Some(sig_params), Some(sig_owns)) = (sig_params, sig_owns) {
            // Transplant the sig onto the synthetic callable_name key so
            // `lower_call_arg` picks pointer-vs-value forwarding the same
            // way as a direct call. Mirrors the B1 identifier-callee fix
            // in the two `Callable`-local arms above.
            ctx.fn_sigs.insert(callable_name.clone(), (sig_params.clone(), ret_type));
            ctx.fn_param_ownerships.insert(callable_name.clone(), sig_owns.clone());
            for (i, arg) in args.iter().enumerate() {
                let param_type = sig_params.get(i).copied();
                call_args.push(lower_call_arg(ctx, builder, arg, param_type, &callable_name, i));
            }
        } else {
            for arg in args {
                call_args.push(lower_expr(ctx, builder, &arg.node.value));
            }
        }
        if ret_type == UNIT_TYPE {
            builder.call_void(callable_name, call_args);
            Operand::Constant(Constant::Unit)
        } else {
            let dst = builder.call(callable_name, call_args, ret_type);
            FunctionBuilder::copy(dst)
        }
    }
}

/// Lower a `print(...)` call to a `printf` extern call.
///
/// Kwargs:
///  - `terminator: String` (default `"\n"`) — string appended after the
///    printed value. Use `""` to suppress the newline; `"\t"` or `", "`
///    for tabular / CSV-style output.
///  - `file: stderr` — route to stderr instead of stdout.
pub fn lower_print_call(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    args: &[Spanned<ast::CallArg>],
) {
    if args.is_empty() {
        // print() with no args → printf("\n")
        let fmt = Operand::Constant(Constant::Str("\n".to_string()));
        builder.call_extern("printf", vec![fmt], I32_TYPE);
        return;
    }

    // Check for named arguments: terminator="…", file=stderr
    let mut terminator: String = "\n".to_string();
    let mut use_stderr = false;
    for arg in args.iter().skip(1) {
        if let Some(ref name) = arg.node.name {
            match name.node.as_str() {
                "terminator" => {
                    // Accept a plain (non-interpolated) string literal — the
                    // terminator has to be known at compile time to splice
                    // into the printf format string. Empty string (""),
                    // single-segment literals, and escapes all flow through
                    // `as_plain_text`. Interpolation segments are silently
                    // dropped; a user passing `terminator=f"{x}"` would only
                    // see the literal chunks, but that's not a real use case.
                    if let Expr::StringLiteral(lit, _) = &arg.node.value.node {
                        let has_interp = lit.segments.iter().any(|s| matches!(s, StringSegment::Interpolation(_, _)));
                        if !has_interp {
                            terminator = lit.as_plain_text();
                        }
                    }
                }
                "file" => {
                    if let Expr::Identifier(id) = &arg.node.value.node {
                        if id == "stderr" {
                            use_stderr = true;
                        }
                    }
                }
                _ => {}
            }
        }
    }

    let arg_expr = &args[0].node.value;

    match &arg_expr.node {
        Expr::StringLiteral(lit, interp_exprs) => {
            let mut format_str = String::new();
            let mut printf_args: Vec<Operand> = Vec::new();

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
                            &mut format_str, &mut printf_args, fmt_spec.as_deref());
                    }
                }
            }

            format_str.push_str(&terminator);

            let mut all_args = Vec::new();
            if use_stderr {
                all_args.push(Operand::Constant(Constant::Null)); // stderr placeholder
                all_args.push(Operand::Constant(Constant::Str(format_str)));
                all_args.extend(printf_args);
                builder.call_extern("fprintf_stderr", all_args, I32_TYPE);
            } else {
                all_args.push(Operand::Constant(Constant::Str(format_str)));
                all_args.extend(printf_args);
                builder.call_extern("printf", all_args, I32_TYPE);
            }
        }
        _ => {
            // General expression (identifier, method call, etc.) — lower and infer type
            let val = lower_expr(ctx, builder, arg_expr);
            let type_id = infer_operand_type_full(ctx, &val, builder);
            let (spec, extra_args) = format_for_printf(ctx, builder, type_id, val, None);
            let fmt = format!("{spec}{terminator}");
            let fmt_op = Operand::Constant(Constant::Str(fmt));
            let mut all_args = Vec::new();
            if use_stderr {
                all_args.push(Operand::Constant(Constant::Null)); // stderr placeholder
                all_args.push(fmt_op);
                all_args.extend(extra_args);
                builder.call_extern("fprintf_stderr", all_args, I32_TYPE);
            } else {
                all_args.push(fmt_op);
                all_args.extend(extra_args);
                builder.call_extern("printf", all_args, I32_TYPE);
            }
        }
    }
}

/// Lower a single interpolation segment in a print/format context.
/// Handles simple variable lookups and re-parses complex expressions (method calls, field access, etc.).
/// `pre_parsed` is the parser-supplied AST for the segment text (populated for
/// every f-string segment so the expression participates in resolution and
/// typecheck/method-mangling rewrites). When `Some`, lowering uses it
/// directly; when `None` (constructed-during-lowering literals or parse
/// failures during early f-string sub-parse), falls back to re-parsing the
/// raw text — that path bypasses the rewriter and may emit un-mangled
/// symbols, but is preserved as a backstop so synthesised f-strings still
/// work.
/// `fmt_spec` is an optional format specifier like ".2f", "x", "08d", etc.
/// Pick an `AssignMode` for the f-string interp temp `tmp = lower(expr)`.
/// The temp is single-use (consumed by the format call). For resource
/// types, the right semantic is Move (transfer ownership from the
/// expression's owned result) when the source is a place, or Clone when
/// the source can't be moved. For non-resource types, Copy (bit-copy of
/// a primitive) is correct.
///
/// Tier 2a Phase 3 (residual): when the source local has Borrowed /
/// View ownership, Move is unsound — the source doesn't own its heap.
/// `r.output.trim()` is the canonical case: `trim` returns a `View`-
/// tagged GorgetString aliasing the receiver's buffer; the
/// f-string-interp `[Mv] tmp = copy view_local` paired with `move_zero
/// view_local` was a borrow-into-owned consume that the
/// `AssignIntoOwnedSlot` validator (correctly) flagged. Fall back to
/// Clone for borrow-shaped sources so the temp owns independent
/// resources.
fn interp_temp_mode(
    ctx: &LoweringContext,
    builder: &FunctionBuilder,
    val: &Operand,
    type_id: crate::ir::types::TypeId,
) -> crate::ir::instructions::AssignMode {
    use crate::ir::instructions::AssignMode;
    use crate::ir::LocalOwnership;
    if !ctx.type_registry.is_resource_type(type_id) {
        return AssignMode::Copy;
    }
    match val {
        Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => {
            // Borrow / View sources can't be moved — clone for
            // independence. (`r.output.trim()` returns a `View`-tagged
            // GorgetString; Move was a borrow-into-owned bug.)
            if let Some(own) = ctx.source_ownership(val, builder) {
                if matches!(own, LocalOwnership::Borrowed { .. } | LocalOwnership::View { .. }) {
                    return AssignMode::Clone;
                }
            }
            AssignMode::Move
        }
        // Source has projections (field/index/deref) or is a constant/computation:
        // can't safely move. Clone gives us an owned independent copy.
        _ => AssignMode::Clone,
    }
}

pub(super) fn lower_interp_segment(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    pre_parsed: Option<&Spanned<Expr>>,
    format_str: &mut String,
    printf_args: &mut Vec<Operand>,
    fmt_spec: Option<&str>,
) {
    // 1. Try simple variable lookup first
    if let Some((local_id, type_id)) = ctx.lookup_local(var_name) {
        // If this is a pointer param, deref to get the value for formatting.
        // Covers &/! params (Borrowed/Unique) and borrowed resource params (ref_locals).
        let ptr_value_type = if ctx.is_param_borrow_unique(builder, local_id) || ctx.is_ref_local(builder, local_id) {
            ctx.pointee_type(builder.local_type(local_id))
        } else {
            None
        };
        if let Some(value_type) = ptr_value_type {
            let deref_place = Place {
                local: local_id,
                projections: vec![Projection::Deref],
            };
            // For resource-containing struct types (e.g. `struct { String name }`),
            // a plain deref+memcpy aliases the borrowed struct's interior resources
            // (String buffers, nested collections). Registering the resulting temp
            // for drop would double-free them. Use the type's clone function when
            // available so the temp owns independent resources.
            //
            // For primitives / Str / GorgetString — the existing Assign path is
            // already correct (C backend emits a deep clone for Ptr→String loads
            // and a by-value load for primitives).
            let needs_deep_clone = !ctx.type_mapper.is_string_type(value_type)
                && ctx.type_registry.is_resource_type(value_type);
            let tmp = if needs_deep_clone {
                if let Some(clone_fn) = ctx.clone_fn_for_ptr(value_type) {
                    // G3: a borrowed resource-struct param deref-cloned so the
                    // f-string interpolation gets an OWNED independent copy at the
                    // format-call boundary — CallArg ("borrowed reference cloned
                    // at call boundary"). Tags the instruction; unwarned as before.
                    ctx.call_tracked_clone(builder, &clone_fn, vec![FunctionBuilder::copy(local_id)], value_type, crate::ir::ImplicitCloneReason::CallArg)
                } else {
                    let t = builder.add_local(value_type, None);
                    builder.assign(Place::local(t), Operand::Copy(deref_place));
                    ctx.drops.register_local(t, value_type, &ctx.type_registry);
                    t
                }
            } else {
                // String / primitive deref: emit the typed AssignMode for the
                // type. Strings need Clone (the SlotStore handler emits a deep
                // copy via gorget_string_copy_cow); primitives stay Copy
                // (bit-copy is correct). Phase C: explicit modes replace the
                // C-backend's "deep clone for Ptr→String loads" magic, so the
                // GIR layer carries the typed contract.
                use crate::ir::instructions::AssignMode;
                let mode = if ctx.type_mapper.is_string_type(value_type) {
                    AssignMode::Clone
                } else {
                    AssignMode::Copy
                };
                let t = builder.add_local(value_type, None);
                builder.assign_mode(mode, Place::local(t), Operand::Copy(deref_place));
                ctx.drops.register_local(t, value_type, &ctx.type_registry);
                t
            };
            let (spec, args) = format_for_printf(ctx, builder, value_type, FunctionBuilder::copy(tmp), fmt_spec);
            format_str.push_str(&spec);
            printf_args.extend(args);
        } else {
            let (spec, args) = format_for_printf(ctx, builder, type_id, FunctionBuilder::copy(local_id), fmt_spec);
            format_str.push_str(&spec);
            printf_args.extend(args);
        }
        return;
    }

    // 2. Lower the parser-supplied Expr if available — it has been through
    //    resolution and typecheck so method calls dispatch to mangled symbols.
    if let Some(expr) = pre_parsed {
        let val = lower_expr(ctx, builder, expr);
        let type_id = infer_operand_type_full(ctx, &val, builder);
        let tmp = builder.add_local(type_id, None);
        let mode = interp_temp_mode(ctx, builder, &val, type_id);
        // Tier 1b Move follow-through: when the temp is staged with Move,
        // the source's ownership transfers to the temp. If the source is
        // a drop-registered bare local, retire its drop registration with
        // `move_zero_and_mark` so the scope-exit drop doesn't double-free
        // the heap allocation that `tmp` (and the `format_for_printf`
        // expansion) now owns. Mirrors the snag #19 / #23 fixes
        // (commits `952b403f`, `4ebefe44`).
        let move_source: Option<LocalId> = if mode == AssignMode::Move {
            match &val {
                Operand::Copy(p) | Operand::Move(p)
                    if p.projections.is_empty() && ctx.drops.is_registered(p.local) =>
                    Some(p.local),
                _ => None,
            }
        } else { None };
        builder.assign_mode(mode, Place::local(tmp), val);
        if let Some(src) = move_source {
            ctx.move_zero_and_mark(builder, src);
        }
        // Register the interp temp at its birth: under Move it now owns the
        // heap the (retired) source owned; under Clone-of-a-String it owns a
        // fresh deep copy (`gorget_string_copy_cow` at the SlotStore).
        // Without this the printf/format consumer reads it and nobody frees
        // it (print-temp leak class). Clone of a NON-string aggregate is a
        // shallow memcpy today (the Assign lowering only distinguishes
        // Move) — registering that alias would double-free, so it stays
        // unregistered until Clone lowers deep for aggregates.
        if mode == AssignMode::Move
            || (mode == AssignMode::Clone && ctx.type_mapper.is_string_type(type_id))
        {
            ctx.drops.register_local(tmp, type_id, &ctx.type_registry);
        }
        let (spec, args) = format_for_printf(ctx, builder, type_id, FunctionBuilder::copy(tmp), fmt_spec);
        format_str.push_str(&spec);
        printf_args.extend(args);
        return;
    }

    // 3. Backstop — re-parse the raw text. Reached only for synthesised f-strings
    //    constructed during lowering (no parse-time AST attached) or when the
    //    parser's sub-expression parse failed. Bypasses semantic passes; complex
    //    expressions here may produce un-mangled symbols.
    if let Ok(parsed_expr) = Parser::new(var_name).parse_expr() {
        let val = lower_expr(ctx, builder, &parsed_expr);
        let type_id = infer_operand_type_full(ctx, &val, builder);
        let tmp = builder.add_local(type_id, None);
        let mode = interp_temp_mode(ctx, builder, &val, type_id);
        // Tier 1b Move follow-through: see branch (2) above.
        let move_source: Option<LocalId> = if mode == AssignMode::Move {
            match &val {
                Operand::Copy(p) | Operand::Move(p)
                    if p.projections.is_empty() && ctx.drops.is_registered(p.local) =>
                    Some(p.local),
                _ => None,
            }
        } else { None };
        builder.assign_mode(mode, Place::local(tmp), val);
        if let Some(src) = move_source {
            ctx.move_zero_and_mark(builder, src);
        }
        // Register the interp temp at its birth — see branch (2) above
        // (same owning-modes-only gate).
        if mode == AssignMode::Move
            || (mode == AssignMode::Clone && ctx.type_mapper.is_string_type(type_id))
        {
            ctx.drops.register_local(tmp, type_id, &ctx.type_registry);
        }
        let (spec, args) = format_for_printf(ctx, builder, type_id, FunctionBuilder::copy(tmp), fmt_spec);
        format_str.push_str(&spec);
        printf_args.extend(args);
        return;
    }

    // 4. Last-resort — insert literal text
    format_str.push_str(var_name);
}

/// Given a type and an operand, return the printf format specifier and the
/// argument list. For Str types, returns `%.*s` with two args (len, data).
/// For bool, returns `%s` with ternary. For other types, returns the standard specifier.
/// When `fmt_spec` is provided (e.g., ".2f", "x", "08d"), it overrides the default format.
fn format_for_printf(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    type_id: TypeId,
    operand: Operand,
    fmt_spec: Option<&str>,
) -> (String, Vec<Operand>) {
    // If a format spec is provided, try to generate a custom printf format
    if let Some(spec) = fmt_spec {
        if let Some(result) = apply_format_spec(ctx, builder, type_id, operand.clone(), spec) {
            return result;
        }
        // If apply_format_spec returns None, fall through to default
    }

    if ctx.type_mapper.is_string_type(type_id) {
        // Str/GorgetString → %.*s with (int)expr.len, expr.data
        ("%.*s".to_string(), vec![operand])
    } else if ctx.pointee_type(type_id).map_or(false, |inner| ctx.type_mapper.is_string_type(inner)) {
        // Ptr(String) — dereference to get the String, then format as %.*s.
        // Assign from the pointer into a String-typed local so the Printf expansion
        // can extract .len and .data fields via SlotAddr + FieldPtr.
        let str_ty = ctx.type_mapper.owned_string_type;
        let tmp = builder.add_local(str_ty, None);
        builder.assign(builder.local(tmp), operand);
        // Register the materialized copy at its birth: the backend emits
        // `gorget_string_copy_cow` for this Ptr→String store (deep clone for
        // owned sources, 32-byte view copy for cap=0 views — the free no-ops
        // on views), so the temp owns its heap and must be dropped.
        // Print-temp leak class: `print(s.field)`, `f"{v.get(i).unwrap()}"`.
        ctx.drops.register_local(tmp, str_ty, &ctx.type_registry);
        ("%.*s".to_string(), vec![FunctionBuilder::copy(tmp)])
    } else if let Some(pointee) = ctx.pointee_type(type_id) {
        // Ptr(T) / MutPtr(T) for primitives or user types — auto-deref to the
        // pointee value. Covers user-written Ref[T]/MutRef[T] field loads and
        // field accesses on borrow-param receivers. Recurse so the pointee can
        // pick up its own formatting (narrow-int widening, Displayable, etc.).
        let deref_place = match &operand {
            Operand::Copy(place) | Operand::Move(place) => {
                let mut p = place.clone();
                p.projections.push(Projection::Deref);
                p
            }
            // Non-place operand (constant, literal) — can't add a Deref
            // projection; fall through to default int formatting.
            _ => return (
                ctx.type_mapper.format_specifier(type_id).to_string(),
                vec![operand],
            ),
        };
        let tmp = builder.add_local(pointee, None);
        builder.assign(Place::local(tmp), Operand::Copy(deref_place));
        return format_for_printf(ctx, builder, pointee, FunctionBuilder::copy(tmp), fmt_spec);
    } else if type_id == BOOL_TYPE {
        ("%s".to_string(), vec![operand])
    } else if let Some(GirType::Named(ref type_name)) = ctx.type_registry.get(type_id).cloned() {
        // Struct type — check if it has a Displayable `display` method
        let display_method = format!("{type_name}__display");
        let has_display = ctx.fn_sigs.contains_key(&display_method)
            || ctx.fn_sigs.keys().any(|k| k.ends_with(&format!("_for_{type_name}__display")));
        if has_display {
            // Call Type__display(&val) → Str, then use %.*s
            let effective_method = if ctx.fn_sigs.contains_key(&display_method) {
                display_method
            } else {
                ctx.fn_sigs.keys()
                    .find(|k| k.ends_with(&format!("_for_{type_name}__display")))
                    .cloned()
                    .unwrap_or(display_method)
            };
            // Create borrow of the operand for self parameter
            let self_type = ctx.register_ptr_type(type_id);
            let self_ptr = builder.add_local(self_type, None);
            if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                builder.emit_borrow(self_ptr, place.clone());
            }
            let owned_string_type = ctx.type_mapper.owned_string_type;
            let result = builder.call(effective_method, vec![FunctionBuilder::copy(self_ptr)], owned_string_type);
            // Register the display result at its birth: `display` returns an
            // owned String (return position = ownership boundary); literal
            // returns are cap=0 views whose free no-ops. Print-temp leak class.
            ctx.drops.register_local(result, owned_string_type, &ctx.type_registry);
            ("%.*s".to_string(), vec![FunctionBuilder::copy(result)])
        } else {
            // No display method — fall through to default formatting
            let spec = ctx.type_mapper.format_specifier(type_id);
            (spec.to_string(), vec![operand])
        }
    } else {
        // For narrow integer types, cast to int64_t/uint64_t to match %lld/%llu format
        let needs_widen = type_id == I8_TYPE || type_id == I16_TYPE || type_id == I32_TYPE;
        let needs_unsigned_widen = type_id == U8_TYPE || type_id == U16_TYPE || type_id == U32_TYPE;
        let effective_op = if needs_widen {
            let tmp = builder.cast(I64_TYPE, operand);
            FunctionBuilder::copy(tmp)
        } else if needs_unsigned_widen {
            let tmp = builder.cast(U64_TYPE, operand);
            FunctionBuilder::copy(tmp)
        } else {
            operand
        };
        let spec = ctx.type_mapper.format_specifier(type_id);
        (spec.to_string(), vec![effective_op])
    }
}

/// Apply a user-provided format spec (e.g., ".2f", "x", "08d") to produce a
/// printf format string. Returns None if the spec is not recognized.
///
/// Supported specs:
///   Integer: d, x, X, o, b, #x, #X, #o, #b, with optional width/zero-pad (e.g., "08x", "5d")
///   Float: f, e, E, with optional precision (e.g., ".2f", ".4e")
///   String: s, with optional width (e.g., "10s")
fn apply_format_spec(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    type_id: TypeId,
    operand: Operand,
    spec: &str,
) -> Option<(String, Vec<Operand>)> {
    if spec.is_empty() {
        return None;
    }

    let is_signed_int = type_id == I8_TYPE || type_id == I16_TYPE || type_id == I32_TYPE
        || type_id == I64_TYPE;
    let is_unsigned_int = type_id == U8_TYPE || type_id == U16_TYPE || type_id == U32_TYPE
        || type_id == U64_TYPE;
    let is_any_int = is_signed_int || is_unsigned_int;
    let is_float = type_id == F32_TYPE || type_id == F64_TYPE;
    let is_str = ctx.type_mapper.is_string_type(type_id);

    // Parse the spec: [#][0][width][.precision][type_char]
    let bytes = spec.as_bytes();
    let mut pos = 0;

    // Check for '#' (alternate form: 0x, 0o, 0b prefix)
    let alt = if pos < bytes.len() && bytes[pos] == b'#' {
        pos += 1;
        true
    } else {
        false
    };

    // Check for '0' (zero-pad)
    let zero_pad = if pos < bytes.len() && bytes[pos] == b'0'
        && pos + 1 < bytes.len() && bytes[pos + 1].is_ascii_digit()
    {
        pos += 1;
        true
    } else {
        false
    };

    // Parse width digits
    let width_start = pos;
    while pos < bytes.len() && bytes[pos].is_ascii_digit() {
        pos += 1;
    }
    let width: Option<&str> = if pos > width_start {
        Some(&spec[width_start..pos])
    } else {
        None
    };

    // Parse precision: .N
    let precision: Option<&str> = if pos < bytes.len() && bytes[pos] == b'.' {
        pos += 1;
        let prec_start = pos;
        while pos < bytes.len() && bytes[pos].is_ascii_digit() {
            pos += 1;
        }
        Some(&spec[prec_start..pos])
    } else {
        None
    };

    // Parse type character
    if pos >= bytes.len() {
        // No type char — just width/precision with default type
        if is_any_int && (width.is_some() || zero_pad) {
            let w = width.unwrap_or("0");
            let z = if zero_pad { "0" } else { "" };
            let len_mod = if is_unsigned_int { "llu" } else { "lld" };
            let op = widen_int(builder, type_id, operand);
            return Some((format!("%{z}{w}{len_mod}"), vec![op]));
        }
        if is_float && precision.is_some() {
            let p = precision.unwrap();
            return Some((format!("%.{p}f"), vec![operand]));
        }
        return None;
    }

    let type_char = bytes[pos] as char;

    match type_char {
        // ── Integer formats ──
        'd' if is_any_int => {
            let w = width.unwrap_or("");
            let z = if zero_pad { "0" } else { "" };
            let len_mod = if is_unsigned_int { "llu" } else { "lld" };
            let op = widen_int(builder, type_id, operand);
            Some((format!("%{z}{w}{len_mod}"), vec![op]))
        }
        'x' if is_any_int => {
            let w = width.unwrap_or("");
            let z = if zero_pad { "0" } else { "" };
            let prefix = if alt { "#" } else { "" };
            let op = widen_int(builder, type_id, operand);
            Some((format!("%{prefix}{z}{w}llx"), vec![op]))
        }
        'X' if is_any_int => {
            let w = width.unwrap_or("");
            let z = if zero_pad { "0" } else { "" };
            let prefix = if alt { "#" } else { "" };
            let op = widen_int(builder, type_id, operand);
            Some((format!("%{prefix}{z}{w}llX"), vec![op]))
        }
        'o' if is_any_int => {
            let w = width.unwrap_or("");
            let z = if zero_pad { "0" } else { "" };
            let prefix = if alt { "#" } else { "" };
            let op = widen_int(builder, type_id, operand);
            Some((format!("%{prefix}{z}{w}llo"), vec![op]))
        }
        'b' if is_any_int => {
            // Binary has no printf equivalent — call runtime helper returning const char*
            let op = widen_int(builder, type_id, operand);
            let owned_string_type = ctx.type_mapper.owned_string_type;
            let alt_arg = Operand::Constant(Constant::I64(if alt { 1 } else { 0 }));
            let result = builder.call_extern(
                "gorget_int_to_binary",
                vec![op, alt_arg],
                owned_string_type,
            );
            // Register the conversion result at its birth: the runtime
            // returns an owned Str (heap buffer). Print-temp leak class.
            ctx.drops.register_local(result, owned_string_type, &ctx.type_registry);
            Some(("%.*s".to_string(), vec![FunctionBuilder::copy(result)]))
        }

        // ── Float formats ──
        'f' if is_float => {
            let p = precision.unwrap_or("6");
            let w = width.unwrap_or("");
            let z = if zero_pad { "0" } else { "" };
            Some((format!("%{z}{w}.{p}f"), vec![operand]))
        }
        'e' if is_float => {
            let p = precision.unwrap_or("6");
            let w = width.unwrap_or("");
            Some((format!("%{w}.{p}e"), vec![operand]))
        }
        'E' if is_float => {
            let p = precision.unwrap_or("6");
            let w = width.unwrap_or("");
            Some((format!("%{w}.{p}E"), vec![operand]))
        }

        // ── String format ──
        's' if is_str => {
            if let Some(w) = width {
                Some((format!("%-{w}.*s"), vec![operand]))
            } else {
                None // no spec effect, use default
            }
        }

        _ => None, // unrecognized spec — fall through to default
    }
}

/// Widen narrow integer types to 64-bit for printf length modifiers.
fn widen_int(builder: &mut FunctionBuilder, type_id: TypeId, operand: Operand) -> Operand {
    let needs_widen = type_id == I8_TYPE || type_id == I16_TYPE || type_id == I32_TYPE;
    let needs_unsigned_widen = type_id == U8_TYPE || type_id == U16_TYPE || type_id == U32_TYPE;
    if needs_widen {
        let tmp = builder.cast(I64_TYPE, operand);
        FunctionBuilder::copy(tmp)
    } else if needs_unsigned_widen {
        let tmp = builder.cast(U64_TYPE, operand);
        FunctionBuilder::copy(tmp)
    } else {
        operand
    }
}

/// Emit a user operator-overload call (`Type__add` / equip `_for_Type__add` /
/// `Type__eq` / `Type__compare` / `Type__neg`, …) with free-call-equivalent
/// resource-arg lifetime + result drop-registration.
///
/// **Inputs are already-lowered `Operand`s.** Call sites lower `lhs`/`rhs` (or
/// compound RHS / self-borrow) first, then hand the operands here. Re-running
/// `lower_call_arg` on the original `Spanned` exprs would double-eval.
///
/// Order mirrors free-call lowering (`lower_call` arg loop + post-call drain):
/// 1. Snapshot `move_zero_baseline` / `temp_drop_baseline` **FIRST**
/// 2. For each resource ByPtr arg, apply the same predicates as
///    `lower_call_arg` ~254–323 (named/live/registered → borrow; unregistered
///    owning temp → re-home Move + `pending_temp_drops`; already-Ptr → forward)
/// 3. `ctx.call_tracked(...)` so a resource result is drop-registered
/// 4. Drain **this call’s** `pending_temp_drops` (unconditional drop) +
///    `pending_move_zeros`
///
/// Core #4: every bare user-overload call routes here (binary/unary ops,
/// Identifier/Index compound, place RMW, assert Type__eq/compare). Atomic
/// `__add` / runtime symbols stay on their own paths.
pub(in crate::ir::lowering) fn emit_operator_overload_call(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    func_name: impl Into<String>,
    args: Vec<Operand>,
    return_type: TypeId,
) -> LocalId {
    let func_name = func_name.into();

    // 1. Baselines FIRST — only drain entries this call adds.
    let move_zero_baseline = ctx.func_state.pending_move_zeros.len();
    let temp_drop_baseline = ctx.func_state.pending_temp_drops.len();

    // 2. ByPtr prep per arg (Operand-level mirror of lower_call_arg Borrow|ByPtr).
    let mut call_args = Vec::with_capacity(args.len());
    for (i, arg) in args.into_iter().enumerate() {
        call_args.push(prep_overload_call_arg(ctx, builder, arg, &func_name, i));
    }

    // 3. Tracked call — resource results land in the drop set.
    let dst = ctx.call_tracked(builder, &func_name, call_args, return_type);

    // 4. Drain THIS call's pending move-zeros + temp drops.
    let pending: Vec<LocalId> = ctx
        .func_state
        .pending_move_zeros
        .drain(move_zero_baseline..)
        .collect();
    for local in pending {
        builder.move_zero(Place::local(local));
        ctx.drops.mark_moved(local);
    }
    let temp_drops: Vec<LocalId> = ctx
        .func_state
        .pending_temp_drops
        .drain(temp_drop_baseline..)
        .collect();
    for local in temp_drops {
        // Unconditional: bare-borrow callees never move the temp (same as
        // free-call drain). DropIfAlive would be stripped — temp isn't in the
        // drop-elab flag set.
        builder.drop(Place::local(local));
    }

    dst
}

/// Operand-level ByPtr preparation for one overload-call argument.
///
/// Mirrors `lower_call_arg`'s `Ownership::Borrow if callee_passes_by_ptr` arm
/// (calls.rs ~254–323) without re-lowering the source expression. Non-ByPtr
/// args (value primitives) pass through unchanged.
fn prep_overload_call_arg(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    val: Operand,
    callee_name: &str,
    arg_idx: usize,
) -> Operand {
    let abi = ctx
        .fn_param_abis
        .get(callee_name)
        .and_then(|abis| abis.get(arg_idx))
        .copied();

    let callee_is_move_param = match abi {
        Some(a) => matches!(a, ParamABI::ByPtr | ParamABI::ByMutPtr),
        None => {
            // Fallback when ParamABI isn't recorded: derive from the callee's
            // declared param type, else from the operand's local type.
            let param_ty = ctx
                .fn_sigs
                .get(callee_name)
                .and_then(|(params, _)| params.get(arg_idx).copied());
            if let Some(pt) = param_ty {
                ctx.type_registry.is_resource_type(pt)
            } else if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                if place.projections.is_empty() {
                    let lt = builder.local_type(place.local);
                    ctx.type_registry.is_resource_type(lt)
                        || ctx.type_registry.needs_drop(lt)
                } else {
                    false
                }
            } else {
                false
            }
        }
    };

    let callee_passes_by_ptr = match abi {
        Some(a) => a != ParamABI::ByValue,
        None => {
            let callee_param_ownership = ctx
                .fn_param_ownerships
                .get(callee_name)
                .and_then(|ownerships| ownerships.get(arg_idx))
                .copied();
            let callee_param_is_mut_borrow = callee_param_ownership
                .map(|o| matches!(o, Ownership::MutableBorrow))
                .unwrap_or(false);
            callee_is_move_param || callee_param_is_mut_borrow
        }
    };

    if !callee_passes_by_ptr {
        return val;
    }

    let callee_param_ownership = ctx
        .fn_param_ownerships
        .get(callee_name)
        .and_then(|ownerships| ownerships.get(arg_idx))
        .copied();
    let use_mut_ptr = matches!(callee_param_ownership, Some(Ownership::Move));

    // GlobalRef → GlobalRefPtr: emit &global_name directly.
    if let Operand::Constant(Constant::GlobalRef(name)) = &val {
        return Operand::Constant(Constant::GlobalRefPtr(name.clone()));
    }

    if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
        if place.projections.is_empty() {
            let local_type = builder.local_type(place.local);
            // Already a Ptr/MutPtr (self-borrow, index_load_borrow, bare param) —
            // forward; don't wrap another layer.
            if matches!(
                ctx.type_registry.get(local_type),
                Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))
            ) {
                return FunctionBuilder::copy(place.local);
            }
            if use_mut_ptr {
                let ptr_type = ctx.register_mut_ptr_type(local_type);
                let dst = builder.add_local(ptr_type, None);
                builder.emit_borrow_mut(dst, place.clone());
                ctx.drops.mark_moved(place.local);
                ctx.func_state.pending_move_zeros.push(place.local);
                return FunctionBuilder::copy(dst);
            } else if !ctx.is_named_local(place.local)
                && ctx.is_owned_local(builder, place.local)
                && ctx.type_registry.needs_drop(local_type)
                && !ctx.drops.is_registered(place.local)
                && !ctx.drops.is_moved(place.local)
            {
                // Owning temporary (e.g. inline `Acc(...)` ctor) built for this
                // argument. Callee borrows (const Ptr) and does NOT drop it, so
                // the caller must free it once the call completes. Re-home into
                // a fresh whole-slot store (LIR drop-flag Initialized) then
                // schedule a post-call drop — same as free-call path.
                let owned = builder.add_local(local_type, None);
                builder.assign_mode(
                    crate::ir::instructions::AssignMode::Move,
                    Place::local(owned),
                    FunctionBuilder::mov(place.local),
                );
                ctx.set_owned(builder, owned);
                let ptr_type = ctx.register_ptr_type(local_type);
                let dst = builder.add_local(ptr_type, None);
                builder.emit_borrow(dst, Place::local(owned));
                ctx.func_state.pending_temp_drops.push(owned);
                return FunctionBuilder::copy(dst);
            } else {
                // Named / live / registered (call_tracked result) → borrow in place.
                let ptr_type = ctx.register_ptr_type(local_type);
                let dst = builder.add_local(ptr_type, None);
                builder.emit_borrow(dst, place.clone());
                return FunctionBuilder::copy(dst);
            }
        }
    }

    // Materialize non-place values (constants) into a temp, then borrow.
    let param_ty = ctx
        .fn_sigs
        .get(callee_name)
        .and_then(|(params, _)| params.get(arg_idx).copied());
    if let Some(pt) = param_ty {
        let mat_type = if matches!(val, Operand::Constant(Constant::Str(_)))
            && ctx
                .pointee_type(pt)
                .map_or(false, |inner| ctx.type_mapper.is_string_type(inner))
        {
            ctx.pointee_type(pt).unwrap_or(pt)
        } else {
            // Prefer the non-pointer declared type when the ABI is ByPtr of T.
            ctx.pointee_type(pt).unwrap_or(pt)
        };
        let tmp = builder.add_local(mat_type, None);
        builder.assign(Place::local(tmp), val);
        if use_mut_ptr {
            let ptr_type = ctx.register_mut_ptr_type(mat_type);
            let dst = builder.add_local(ptr_type, None);
            builder.emit_borrow_mut(dst, Place::local(tmp));
            // Move ownership into the callee (MutPtr of a Move param) — do
            // NOT schedule a post-call drop on `tmp`.
            return FunctionBuilder::copy(dst);
        } else {
            let ptr_type = ctx.register_ptr_type(mat_type);
            let dst = builder.add_local(ptr_type, None);
            builder.emit_borrow(dst, Place::local(tmp));
            // Constant / non-place materialize: callee borrows (const Ptr)
            // and does not drop. If the materialized temp needs drop,
            // schedule it for post-call drain (same class as the owning-
            // ctor-temp arm above). Free-call `lower_call_arg` still has
            // the sibling gap for non-overload ByPtr constants — file that
            // separately if a leak shows up there.
            if ctx.type_registry.needs_drop(mat_type) {
                ctx.set_owned(builder, tmp);
                ctx.func_state.pending_temp_drops.push(tmp);
            }
            return FunctionBuilder::copy(dst);
        }
    }
    if let Operand::Constant(Constant::Str(_)) = &val {
        let sv_type = ctx.type_mapper.owned_string_type;
        let tmp = builder.add_local(sv_type, None);
        builder.assign(Place::local(tmp), val);
        let ptr_type = ctx.register_ptr_type(sv_type);
        let dst = builder.add_local(ptr_type, None);
        builder.emit_borrow(dst, Place::local(tmp));
        if ctx.type_registry.needs_drop(sv_type) {
            ctx.set_owned(builder, tmp);
            ctx.func_state.pending_temp_drops.push(tmp);
        }
        return FunctionBuilder::copy(dst);
    }
    val
}
