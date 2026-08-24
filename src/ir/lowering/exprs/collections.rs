//! Array/dict/set literal lowering, comprehensions, optional chaining, and range expressions.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{Expr, Pattern};
use crate::span::Spanned;

use super::super::context::{LoweringContext, SlotType};
use super::{lower_expr, infer_operand_type_full, infer_collection_element_type};
use super::calls::pack_trait_object_for_smart_ptr_ctor;

/// Lower `[e1, e2, ...]` to `gorget_array_new(sizeof(elem))` + N `gorget_array_push` calls.
pub(super) fn lower_array_literal(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    elems: &[Spanned<Expr>],
) -> Operand {
    // Set-literal disambiguation: the parser produces `Expr::ArrayLiteral`
    // for BOTH `[a, b, c]` AND `{a, b, c}` (the AST node is shared with a
    // "set vs array distinguished by context" convention — see
    // `src/parser/expr.rs:1663`). When the surrounding context declares a
    // Set / HashSet type, the literal must lower to `GorgetSet`, not
    // `GorgetArray` — otherwise the C-emit memcpys array bytes into a
    // Set slot (different layouts), producing silent UB at runtime.
    // Pre-fix repro: `Set[int] s = {1, 2, 3}; print(s.len())` printed a
    // garbage memory-address-like value instead of 3.
    if let Some(outer) = ctx.func_state.expected_type {
        let kind = ctx.type_registry.collection_kind(outer);
        if matches!(kind,
            Some(crate::ir::types::CollectionKind::OrderedSet)
            | Some(crate::ir::types::CollectionKind::Set))
        {
            return lower_set_literal_from_array(ctx, builder, elems);
        }
    }

    // If the surrounding context has an expected type like Vector[Option[T]],
    // propagate `Option[T]` as the per-element expected type so bare
    // expressions like `None()` resolve to the right Option variant
    // instead of falling through to Constant::Null. Without this,
    // `Vector[Option[int]] v = [None(), Some(1)]` lowers None() to
    // Null (i64 0), which the C backend assigns into a void* slot —
    // silently wrong values at runtime.
    //
    // Only override expected_type for the non-empty branch — the
    // empty-array branch reads ctx.func_state.expected_type to compute
    // elem_size and needs the OUTER (Vector[T]) type there.
    let saved_expected = ctx.func_state.expected_type;
    let nonempty_expected_override = if !elems.is_empty() {
        if let Some(outer) = saved_expected {
            // Read typed `collection_kind` (Phase A) — Vector/Deque/GorgetArray
            // all carry `Array` from the protocol registration.
            let outer_is_vector = ctx.type_registry.collection_kind(outer)
                == Some(crate::ir::types::CollectionKind::Array);
            if outer_is_vector {
                Some(infer_collection_element_type(ctx, outer))
            } else { None }
        } else { None }
    } else { None };
    if let Some(elem_t) = nonempty_expected_override {
        ctx.func_state.expected_type = Some(elem_t);
    }

    // Infer element type from first element. When the surrounding context
    // declares `Vector[T]` (nonempty_expected_override), prefer that T over
    // the inferred first-elem type so `Vector[Box[Trait]] = [Box.new(C)]`
    // sizes the buffer and elem slots for Box[Trait] (16B TraitObj), not
    // Box[Concrete] (8B void*) — Round XIX Track N2 cell F.
    let elem_type = if !elems.is_empty() {
        let first = lower_expr(ctx, builder, &elems[0]);
        // ⚠ NARROW to the trait-box destination (cell F's stated intent). Taking
        // the context override UNCONDITIONALLY mis-sizes elem slots whenever the
        // surrounding expected type is less precise than the real element — e.g.
        // a `Vector[(int, int)]` literal, where the wrong slot width makes
        // `&v[i].0` read uninitialized memory (garbage pointer, both backends,
        // non-deterministic). The override exists solely so
        // `Vector[Box[Trait]] = [Box.new(Concrete)]` sizes for the 16B TraitObj
        // instead of the 8B void*; outside that case the inferred first-element
        // type is authoritative. Predicate mirrors
        // `pack_trait_object_for_smart_ptr_ctor`'s own guard: `Box__<X>` whose
        // `<X>_TraitObj` is registered.
        let override_is_trait_box = nonempty_expected_override.is_some_and(|ov| {
            match ctx.type_registry.get(ov) {
                Some(GirType::Named(n)) => n
                    .strip_prefix("Box__")
                    .is_some_and(|inner| {
                        ctx.type_registry
                            .get_type_def(&format!("{inner}_TraitObj"))
                            .is_some()
                    }),
                _ => false,
            }
        });
        // ⭐ MATERIALIZE FIRST, THEN MINT. The element slot does not exist yet,
        // so `FromOperand` is the honest answer and the type comes back FROM
        // the materialization — "minted slot type == materialized operand type"
        // by construction. Minting first and materializing afterwards is what
        // made `Vector[T] out = [borrowed_elem]` allocate 8-byte `Ptr` slots
        // with no element vtable and then store a 32-byte clone into them.
        //
        // The one `Known` case is the trait-box destination: `Vector[Box[Trait]]
        // = [Box.new(Concrete)]` must size for the 16B TraitObj, and the packing
        // adapter below reconciles the operand to it. Everywhere else the
        // "expected" element type is recovered by `infer_collection_element_type`
        // — a mangled-name strip with an `I64` fallback — so it is INFERRED, not
        // DECLARED, and passing it as `Known` mis-sizes every non-trait-box
        // `Vector[T]` (measured: a garbage read on a `Vector[(int, int)]`).
        let slot = match (override_is_trait_box, nonempty_expected_override) {
            (true, Some(target)) => SlotType::Known(target),
            _ => SlotType::FromOperand,
        };
        let (first_owned, etype) = ctx.materialize_for_slot(
            builder, first, &elems[0], slot,
            crate::ir::ImplicitCloneReason::ConsumingArg);
        // Type the fresh local as the monomorphized `Vector__<elem>` (carries
        // the element type for a downstream `v[i]` / `for x in v` / element-drop)
        // rather than the bare `GorgetArray`. Mirrors `lower_dict_literal`'s
        // `Dict__K__V` typing — the producer is the source of truth for the
        // element invariant, written through to the local here so an `auto`
        // re-infer recovers it.
        let vec_type = collection_accumulator_type(ctx, "Vector", etype);
        // Create the array
        let arr_local = builder.call_extern(
            "gorget_array_new",
            vec![Operand::Constant(Constant::SizeOf(etype))],
            vec_type,
        );
        // The literal owns a fresh allocation. Without this tag, downstream
        // ownership-sensitive sinks (Some(arr), struct field init, return)
        // see Untracked → emit clone-then-leak: the literal's buffer is
        // cloned into the consumer and the original is orphaned.
        ctx.set_owned(builder, arr_local);
        ctx.drops.register_local(arr_local, vec_type, &ctx.type_registry);
        // Phase C: pick mode by source — owned call results / unnamed
        // temps (e.g., nested vector literals) get Move; primitives stay
        // Copy. Mirrors the broadened predicate in lower_return /
        // assign_to_return_slot (C2.10 / C2.13): bare-place + needs_drop
        // is owned-equivalent at this site since the source temp is dead
        // immediately after the assign (only the new local + its borrow
        // are used).
        // Pack Box[Concrete]→Box[Trait] when the element slot is a trait-box
        // (cell F Class B). Named destination drives the pack adapter.
        let etype_name: Option<String> = match ctx.type_registry.get(etype) {
            Some(GirType::Named(n)) => Some(n.clone()),
            _ => None,
        };
        // Push first element. It was already routed through the shared
        // consuming-position materializer ABOVE, by `materialize_for_slot` —
        // that call is what produced `etype`, so re-materializing here would
        // double-clone.
        let elem_local = builder.add_local(etype, None);
        let first_owned = if let Some(ref name) = etype_name {
            pack_trait_object_for_smart_ptr_ctor(ctx, builder, first_owned, name)
        } else {
            first_owned
        };
        let first_mode = element_assign_mode(ctx, builder, &first_owned, etype);
        let first_clone = first_owned.clone();
        builder.assign_mode(first_mode, Place::local(elem_local), first_owned);
        // Emit MoveZero + mark_moved so drop-tracking knows the source is
        // dead. Without this, registering owned temps for drop (so they don't
        // leak) turns Move-into-elem-slot into double-free: both source and
        // dest retain the data pointer (memcpy alone) and both fire scope-exit
        // drops. The LIR backend elides the actual zero when liveness proves
        // the source is unobservable; this is just the IR-level signal.
        if first_mode == crate::ir::instructions::AssignMode::Move {
            if let Operand::Copy(ref place) | Operand::Move(ref place) = first_clone {
                if place.projections.is_empty()
                    && place.local != elem_local
                    && !ctx.drops.is_moved(place.local)
                {
                    ctx.move_zero_and_mark(builder, place.local);
                }
            }
        }
        let ref_local = builder.borrow(Place::local(elem_local), ctx.register_ptr_type(etype));
        let arr_ref = builder.borrow_mut(Place::local(arr_local), ctx.register_mut_ptr_type(vec_type));
        builder.call_extern(
            "gorget_array_push",
            vec![FunctionBuilder::copy(arr_ref), FunctionBuilder::copy(ref_local)],
            UNIT_TYPE,
        );
        // Push remaining elements
        for elem_expr in &elems[1..] {
            let elem_val = lower_expr(ctx, builder, elem_expr);
            let el = builder.add_local(etype, None);
            // Clone-if-live / move-if-dead via the shared consuming-position
            // materializer. For elements 2..N the slot IS declared — the first
            // element minted it — so this is a genuine `Known`, and it reaches
            // the same deref arm the first element did.
            let (elem_val, _) = ctx.materialize_for_slot(
                builder, elem_val, elem_expr,
                SlotType::Known(etype),
                crate::ir::ImplicitCloneReason::ConsumingArg);
            let elem_val = if let Some(ref name) = etype_name {
                pack_trait_object_for_smart_ptr_ctor(ctx, builder, elem_val, name)
            } else {
                elem_val
            };
            let mode = element_assign_mode(ctx, builder, &elem_val, etype);
            let elem_val_clone = elem_val.clone();
            builder.assign_mode(mode, Place::local(el), elem_val);
            if mode == crate::ir::instructions::AssignMode::Move {
                if let Operand::Copy(ref place) | Operand::Move(ref place) = elem_val_clone {
                    if place.projections.is_empty()
                        && place.local != el
                        && !ctx.drops.is_moved(place.local)
                    {
                        ctx.move_zero_and_mark(builder, place.local);
                    }
                }
            }
            let el_ref = builder.borrow(Place::local(el), ctx.register_ptr_type(etype));
            let ar_ref = builder.borrow_mut(Place::local(arr_local), ctx.register_mut_ptr_type(vec_type));
            builder.call_extern(
                "gorget_array_push",
                vec![FunctionBuilder::copy(ar_ref), FunctionBuilder::copy(el_ref)],
                UNIT_TYPE,
            );
        }
        FunctionBuilder::copy(arr_local)
    } else {
        // Empty array — infer element size from expected type if available.
        // Without this, Vector[LargeStruct] initialized as [] gets elem_size=8
        // instead of sizeof(LargeStruct), causing buffer overflows on push.
        let elem_size_type = ctx.func_state.expected_type
            .map(|et| infer_collection_element_type(ctx, et))
            .unwrap_or(I64_TYPE);
        // Type the local with the monomorphized `Vector__<elem>` so an `auto`
        // re-infer recovers the element. With an expected type the element is
        // known; without one (`auto v = []`) it falls to I64 — a safe
        // empty-literal fallback (there is no element to refine it here).
        let vec_type = collection_accumulator_type(ctx, "Vector", elem_size_type);
        let arr_local = builder.call_extern(
            "gorget_array_new",
            vec![Operand::Constant(Constant::SizeOf(elem_size_type))],
            vec_type,
        );
        ctx.set_owned(builder, arr_local);
        ctx.drops.register_local(arr_local, vec_type, &ctx.type_registry);
        FunctionBuilder::copy(arr_local)
    };
    ctx.func_state.expected_type = saved_expected;
    elem_type
}

/// Lower `{a, b, c}` to `gorget_set_new(sizeof(elem))` + N `gorget_set_add`
/// calls. Mirrors `lower_array_literal` but uses Set runtime fns and the
/// `gorget_set_*` ABI. Dispatched from `lower_array_literal` when the
/// surrounding `decl_type_hint` is `Set` / `HashSet`.
fn lower_set_literal_from_array(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    elems: &[Spanned<Expr>],
) -> Operand {
    // Determine ordering BEFORE any mutation of ctx.func_state.expected_type.
    // The non-empty branch below overrides expected_type to the element type
    // before the accumulator is built, so reading after that yields
    // collection_kind(int)==None → unordered → silent bug.  Capturing here
    // at function entry is the only safe read point.
    let is_ordered = ctx.func_state.expected_type
        .and_then(|et| ctx.type_registry.collection_kind(et))
        == Some(CollectionKind::OrderedSet);
    // Protocol base for the monomorphized accumulator type / ctor name.
    // `Set__T__new` and `HashSet__T__new` both family-route through the
    // resources table; the `Set__` prefix is what selects the ORDERED runtime
    // ctor (`src/lir/lower/calls.rs`), so ordering stays typed, not guessed.
    let base = if is_ordered { "Set" } else { "HashSet" };

    if elems.is_empty() {
        // Empty set — infer element size from expected type if available.
        let elem_size_type = ctx.func_state.expected_type
            .map(|et| infer_collection_element_type(ctx, et))
            .unwrap_or(I64_TYPE);
        let set_type = collection_accumulator_type(ctx, base, elem_size_type);
        let new_fn = collection_ctor_name(ctx, base, elem_size_type);
        let set_local = builder.call_extern(&new_fn, vec![], set_type);
        ctx.set_owned(builder, set_local);
        ctx.drops.register_local(set_local, set_type, &ctx.type_registry);
        return FunctionBuilder::copy(set_local);
    }

    // Lower first element with the per-element expected-type override.
    // Mirrors lower_array_literal's nonempty_expected_override.
    let saved_expected = ctx.func_state.expected_type;
    let elem_override = ctx.func_state.expected_type
        .map(|outer| infer_collection_element_type(ctx, outer));
    if let Some(elem_t) = elem_override {
        ctx.func_state.expected_type = Some(elem_t);
    }

    let first = lower_expr(ctx, builder, &elems[0]);
    // ⭐ MATERIALIZE FIRST, THEN MINT — the array/dict inversion, applied to the
    // set. Without it a borrow-typed element mints `Ptr(T)`, which mangles to an
    // opaque name and therefore selects NEITHER key channel (no `_str`, no
    // ktable bridge) as well as sizing the slot wrong.
    let (first, etype) = ctx.materialize_for_slot(
        builder, first, &elems[0],
        SlotType::FromOperand,
        crate::ir::ImplicitCloneReason::ConsumingArg);

    // ⭐ WRITE THE ELEMENT TYPE THROUGH, exactly as `lower_dict_literal` does
    // for `Dict__K__V`. The set was the ONE literal that typed its local with
    // the BARE `GorgetSet` and emitted the raw runtime symbol with a `SizeOf`
    // argument — and BOTH downstream key channels read precisely what that
    // withheld: `set_elem_type_from_monomorphized` (the `_str` hash selection)
    // reads the monomorphized ctor NAME, and `wire_collection_bridges` (the
    // user-hashable ktable) reads the accumulator TYPE. The explicit `Set[T]()`
    // constructor has always been correct on both channels for this reason;
    // the literal was sibling-site drift from it. The `SizeOf` argument was
    // itself load-bearing in the wrong direction — it defeated the
    // `lir_args.is_empty()` gate that performs the `_str` upgrade, so the
    // upgrade could never run. Layering rule 4: resolve once, write through.
    let set_type = collection_accumulator_type(ctx, base, etype);
    let new_fn = collection_ctor_name(ctx, base, etype);
    let set_local = builder.call_extern(&new_fn, vec![], set_type);
    ctx.set_owned(builder, set_local);
    ctx.drops.register_local(set_local, set_type, &ctx.type_registry);


    // Insert first element with per-elem Move + MoveZero discipline
    // (parallel to lower_array_literal's element handling — sets share
    // the same consume-position semantics as arrays).
    let insert_elem = |ctx: &mut LoweringContext, builder: &mut FunctionBuilder, val: Operand| {
        let mode = element_assign_mode(ctx, builder, &val, etype);
        let el = builder.add_local(etype, None);
        let val_clone = val.clone();
        builder.assign_mode(mode, Place::local(el), val);
        if mode == crate::ir::instructions::AssignMode::Move {
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val_clone {
                if place.projections.is_empty()
                    && place.local != el
                    && !ctx.drops.is_moved(place.local)
                {
                    ctx.move_zero_and_mark(builder, place.local);
                }
            }
        }
        let el_ref = builder.borrow(Place::local(el), ctx.register_ptr_type(etype));
        let s_ref = builder.borrow_mut(Place::local(set_local), ctx.register_mut_ptr_type(set_type));
        builder.call_extern(
            "gorget_set_add",
            vec![FunctionBuilder::copy(s_ref), FunctionBuilder::copy(el_ref)],
            UNIT_TYPE,
        );
    };
    insert_elem(ctx, builder, first);
    for elem_expr in &elems[1..] {
        if let Some(elem_t) = elem_override {
            ctx.func_state.expected_type = Some(elem_t);
        }
        let val = lower_expr(ctx, builder, elem_expr);
        // Elements 2..N land in the slot the first element minted — a genuine
        // `Known` target, reaching the same materialization the first did.
        let (val, _) = ctx.materialize_for_slot(
            builder, val, elem_expr,
            SlotType::Known(etype),
            crate::ir::ImplicitCloneReason::ConsumingArg);
        insert_elem(ctx, builder, val);
    }
    ctx.func_state.expected_type = saved_expected;
    FunctionBuilder::copy(set_local)
}

// ---- Dict Literals ----

/// Lower `{"a": 1, "b": 2}` to `Dict__K__V__new()` + N `Dict__K__V__put()` calls.
pub(super) fn lower_dict_literal(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    pairs: &[(Spanned<Expr>, Spanned<Expr>)],
) -> Operand {
    if pairs.is_empty() {
        // Use expected type from VarDecl context to determine dict type.
        // The parser produces `Expr::DictLiteral(vec![])` for `{}` — it
        // cannot know from syntax alone whether the target is a Dict/HashMap
        // (pair store) or a Set/HashSet (single-value store), so this branch
        // routes by the expected type's `collection_kind` (Phase A typed
        // metadata, not name-matching).
        use crate::ir::types::CollectionKind;
        if let Some(expected_type) = ctx.func_state.expected_type {
            let kind = ctx.type_registry.collection_kind(expected_type);
            // Set / HashSet empty `{}` — dispatch to the Set producer so the
            // slot receives a `GorgetSet` allocated via `gorget_set_new`
            // (with the element-type-derived elem_size). Before this arm the
            // empty branch below fell to `Constant::Unit` (int32_t 0), which
            // the C-emit assigned into a `GorgetSet` slot ("incompatible
            // types when assigning to type 'GorgetSet' from type 'int32_t'")
            // and the LLVM backend rejected on the same shape. Same producer
            // as non-empty `{a, b}` (dispatched from `lower_array_literal`);
            // `lower_set_literal_from_array` reads `is_ordered` from the same
            // `expected_type` we just read here.
            if matches!(kind, Some(CollectionKind::OrderedSet) | Some(CollectionKind::Set)) {
                return lower_set_literal_from_array(ctx, builder, &[]);
            }
            let is_map = matches!(kind,
                Some(CollectionKind::OrderedMap) | Some(CollectionKind::Map));
            if is_map {
                if let Some(type_name) = ctx.type_registry.type_name(expected_type) {
                    let new_fn = format!("{type_name}__new");
                    let dict_local = builder.call_extern(&new_fn, vec![], expected_type);
                    // Tier 2a Phase 3: tag the fresh dict as Owned so the
                    // var_decl Move-mode picker sees a concrete state.
                    // Without this, downstream Inst::Assign emits Move
                    // mode but the source remains Untracked, which the
                    // AssignIntoOwnedSlot validator flags. Mirrors
                    // `lower_array_literal`'s `set_owned` at the
                    // `gorget_array_new` call.
                    ctx.set_owned(builder, dict_local);
                    return FunctionBuilder::copy(dict_local);
                }
            }
        }
        return Operand::Constant(Constant::Unit);
    }

    // Propagate the dict's value-type expected_type when the outer context
    // declares a known `Dict[K, V]` (bare-init / Some(...) / fn-arg). This
    // lets a nested array literal in the value position type-resolve as
    // `Vector[T]` instead of `T[N]` (Snag #35-class — without it,
    // `Dict[String, Vector[int]] d = {"a": [1,2,3]}` fails typecheck with
    // `expected Vector[int], found int[3]`).
    //
    // Mirrors the override pattern in `lower_array_literal` for nested
    // `Vector[Option[T]]`-shape literals. The `Some({...})` case works
    // today by accident — the outer Option's lowering pre-propagates V.
    use crate::ir::types::CollectionKind;
    let saved_expected = ctx.func_state.expected_type;
    let val_expected_override = saved_expected.and_then(|outer| {
        let is_map = matches!(ctx.type_registry.collection_kind(outer),
            Some(CollectionKind::OrderedMap) | Some(CollectionKind::Map));
        if is_map { Some(infer_collection_element_type(ctx, outer)) } else { None }
    });
    // Lower first key BEFORE setting the value-override (keys can be any
    // type, not the value-type), then lower first value with the override.
    //
    // Override the per-value `expected_type` to the dict's VALUE type when
    // known (`val_expected_override`), else CLEAR it (`None`). Clearing is
    // load-bearing for `Result→T` auto-prop: a `return {"a": throws_call()}`
    // from a `throws`/`Result` function sets `expected_type` to the
    // function's `Result[Dict[..], E]` return slot (via `lower_return`),
    // whose `collection_kind` is NOT a Map → `val_expected_override` is
    // `None`. If we left `saved_expected` (the `Result`) in place while
    // lowering the value, `maybe_auto_propagate`'s peel on the throwing-call
    // value would be suppressed (it skips when the destination expects a
    // `Result`), leaking the raw `Result` into the dict's `put` — a
    // miscompile (Tier 2a consume-site violation for resource values, or a
    // zero-init garbage value for primitives). Mirrors the `expected_type`
    // clear in `lower_binary_op` (operators.rs) and the per-element override
    // in `lower_array_literal`. Keys keep `saved_expected` (their type is
    // unrelated to the value type).
    let first_key = lower_expr(ctx, builder, &pairs[0].0);
    ctx.func_state.expected_type = val_expected_override;
    let first_val = lower_expr(ctx, builder, &pairs[0].1);
    ctx.func_state.expected_type = saved_expected;
    // ⭐ MATERIALIZE FIRST, THEN MINT — same inversion as the array literal, and
    // the reason the materializer moved OUT of `stage_dict_arg`: staging ran
    // AFTER `Dict__K__V` was minted, so a borrow-typed key or value minted
    // `Dict__Ptr__…` (8-byte slots, no key/val vtable) and the clone was stored
    // into it. The K and V slots do not exist until this call returns, so
    // `FromOperand` is the honest answer for both. (`val_expected_override`
    // comes from `infer_collection_element_type` — a mangled-name strip with an
    // `I64` fallback — so it is INFERRED, never `Known`.)
    let first_key_m = MaterializedElement::produce(
        ctx, builder, first_key, &pairs[0].0, SlotType::FromOperand);
    let first_val_m = MaterializedElement::produce(
        ctx, builder, first_val, &pairs[0].1, SlotType::FromOperand);

    // The SINGLE `Dict__K__V` mangle chokepoint, shared with
    // `lower_dict_comprehension`. Both construction sites route here, and both
    // can only reach it with MATERIALIZED key/value types — this one used to
    // re-wrap raw `materialize_for_slot` results in a struct literal, which is
    // now unrepresentable (see `mod materialized`).
    let mangled = dict_mangled_name(ctx, &first_key_m, &first_val_m);
    let (first_key, key_type) = first_key_m.into_parts();
    let (first_val, val_type) = first_val_m.into_parts();

    // Phase A: ensure_collection_type populates protocol-derived metadata
    // (collection_kind / drop_strategy / clone_fn) for downstream consumers
    // like collection_runtime_type, elem_drop_fn_for_type, is_resource_type.
    let dict_type = ctx.ensure_collection_type(&mangled);

    let new_fn = format!("{mangled}__new");
    let put_fn = format!("{mangled}__put");
    // Tier 2a strengthening: register the mangled put fn as a consume-shape
    // extern so the validator's `is_runtime_collection_mutator` allowlist
    // — which name-matches the runtime symbol (`gorget_map_put`), not the
    // mangled IR-stage name (`Dict__K__V__put`) — picks up via the typed
    // registry instead. See Module::consume_externs for rationale.
    ctx.consume_externs.insert(put_fn.clone());

    // Create the dict
    let dict_local = builder.call_extern(&new_fn, vec![], dict_type);
    // Tier 2a Phase 3: tag the fresh dict as Owned so var_decl /
    // collection-element / call-arg consume sites see a concrete
    // ownership state. Mirrors `lower_array_literal:61`.
    ctx.set_owned(builder, dict_local);

    // Insert pairs. Each put memcpys the key/value structs into the slot;
    // for resource-typed key/value operands, that aliases the temp's heap
    // buffer with the slot. Without the Move-mode staging + MoveZero
    // discipline below, the temp's scope-exit drop and the dict's elem_drop
    // both free the same buffer — double-free. Mirrors lower_array_literal.
    insert_pair(ctx, builder, dict_local, dict_type, &put_fn, first_key, first_val, key_type, val_type, &pairs[0].0, &pairs[0].1);
    for (key_expr, val_expr) in &pairs[1..] {
        let k = lower_expr(ctx, builder, key_expr);
        // Same value-override / clear as the first pair (see above) so a
        // throwing-call value in any pair auto-props consistently.
        ctx.func_state.expected_type = val_expected_override;
        let v = lower_expr(ctx, builder, val_expr);
        ctx.func_state.expected_type = saved_expected;
        // Pairs 2..N land in slots the first pair already minted, so these are
        // genuine `Known` targets — and they reach the same deref arm.
        let (k, _) = ctx.materialize_for_slot(
            builder, k, key_expr,
            SlotType::Known(key_type),
            crate::ir::ImplicitCloneReason::ConsumingArg);
        let (v, _) = ctx.materialize_for_slot(
            builder, v, val_expr,
            SlotType::Known(val_type),
            crate::ir::ImplicitCloneReason::ConsumingArg);
        insert_pair(ctx, builder, dict_local, dict_type, &put_fn, k, v, key_type, val_type, key_expr, val_expr);
    }

    FunctionBuilder::copy(dict_local)
}

/// Stage a (key, value) pair through fresh per-elem locals with Move-mode
/// + MoveZero for resource-typed operands, then emit the `put_fn` call.
/// Mirrors the per-element discipline in `lower_array_literal` so the dict
/// takes ownership of resource values without double-freeing on temp drop.
fn insert_pair(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    dict_local: LocalId,
    dict_type: TypeId,
    put_fn: &str,
    key_op: Operand,
    val_op: Operand,
    key_type: TypeId,
    val_type: TypeId,
    key_expr: &Spanned<Expr>,
    val_expr: &Spanned<Expr>,
) {
    let key_arg = stage_dict_arg(ctx, builder, key_op, key_type, key_expr);
    let val_arg = stage_dict_arg(ctx, builder, val_op, val_type, val_expr);
    let dict_ref = builder.borrow_mut(Place::local(dict_local), ctx.register_mut_ptr_type(dict_type));
    builder.call_extern(
        put_fn,
        vec![FunctionBuilder::copy(dict_ref), key_arg, val_arg],
        UNIT_TYPE,
    );
}

/// For a resource-typed operand at a dict-put position, stage it through a
/// fresh per-elem local with Move-mode + MoveZero on the source. The
/// dict's `val_drop` / `key_drop` hooks own the slot's lifecycle from
/// here; the per-elem local is intentionally NOT drop-registered so it
/// won't fire a scope-exit drop on the data the dict now owns. For
/// non-resource operands, pass through unchanged.
///
/// ⚠ This helper does NOT materialize. The consuming-position materialization
/// (clone-if-live / move-if-dead, plus the `Ptr`→owned answer) happens at both
/// `insert_pair` CALL SITES, *above* the `Dict__K__V` mint — see
/// `lower_dict_literal`. It used to live here, which put it AFTER the mint and
/// let a borrow-typed key or value mint `Dict__Ptr__…`. Re-adding it here would
/// double-clone.
fn stage_dict_arg(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    op: Operand,
    ty: TypeId,
    _arg_expr: &Spanned<Expr>,
) -> Operand {
    use crate::ir::instructions::AssignMode;
    // Primitives pass straight through: the backend auto-takes the address of
    // the by-value operand for the put's `void* value` param. Deep resources
    // AND refcount handles need the fresh-slot + MoveZero staging below (so the
    // source temp's scope-exit drop does not double-free / double-decref the
    // slot the dict now owns).
    let is_resource = ctx.type_registry.is_resource_type(ty);
    let is_refcount = ctx.type_registry.is_refcount_clone_type(ty);
    if !is_resource && !is_refcount {
        return op;
    }
    // Pick Move when source is owned (last-use by construction at this site
    // for unnamed temps) or an unnamed temp that needs drop (the literal /
    // call-result shape from lower_expr); Copy otherwise. Mirrors the
    // elem_mode picker in lower_array_literal.
    let mode = match &op {
        Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => {
            let src_ty = builder.local_type(p.local);
            if ctx.is_owned_local(builder, p.local)
                || (!ctx.is_named_local(p.local)
                    && (ctx.type_registry.needs_drop(src_ty)
                        || ctx.type_registry.is_resource_type(src_ty)))
            {
                AssignMode::Move
            } else {
                AssignMode::Copy
            }
        }
        _ => AssignMode::Copy,
    };
    let elem_local = builder.add_local(ty, None);
    let op_clone = op.clone();
    builder.assign_mode(mode, Place::local(elem_local), op);
    if mode == AssignMode::Move {
        if let Operand::Copy(ref place) | Operand::Move(ref place) = op_clone {
            if place.projections.is_empty()
                && place.local != elem_local
                && !ctx.drops.is_moved(place.local)
            {
                ctx.move_zero_and_mark(builder, place.local);
            }
        }
    }
    if is_refcount {
        // A refcount handle is pointer-represented (`void*`), so the backend's
        // auto-address-of for a by-value operand does NOT fire — it would pass
        // the handle itself where `gorget_map_put` expects `&handle`, memcpying
        // the control block's bytes into the slot (garbage → SIGSEGV at the
        // slot's val_drop, and the incref never balances). Pass an EXPLICIT
        // borrow of the staged slot, exactly as lower_array_literal borrows its
        // element local before `gorget_array_push`.
        let ref_local = builder.borrow(Place::local(elem_local), ctx.register_ptr_type(ty));
        return FunctionBuilder::copy(ref_local);
    }
    FunctionBuilder::copy(elem_local)
}

/// Build the monomorphized collection TypeId (`Vector__<elem>`) for a
/// freshly-built array runtime local, carrying the element type so a
/// downstream `v[i]` / `for x in v` / element-drop can recover it. Without
/// this the producer types the local with the BARE runtime struct
/// (`GorgetArray`), which `infer_collection_element_type` cannot decompose
/// (it works by `Vector__`-name-prefix) → the element falls to I64 (Layering
/// rule 4: the element type is a typed invariant resolved here and written
/// through to the local). Mirrors `lower_dict_literal`'s `Dict__K__V` typing
/// and the self-host `collection_accumulator_tid` helper (gorget-1 2fc65622).
///
/// Backend-NEUTRAL: the `ensure_collection_type("Vector__<elem>")` TypeDef
/// carries `c_runtime_alias: None`; layout-neutrality comes from the LIR
/// `Vector__`-prefix-strip-to-runtime mapping (`src/lir/lower/types.rs:18`,
/// `src/lir/lower/mod.rs`), NOT a `c_runtime_alias`. `base` is the protocol
/// base ("Vector").
fn collection_accumulator_type(ctx: &mut LoweringContext, base: &str, elem_type: TypeId) -> TypeId {
    let elem_c = type_id_to_mangle_name(ctx, elem_type);
    ctx.ensure_collection_type(&format!("{base}__{elem_c}"))
}

/// The monomorphized constructor name for a freshly-built collection local
/// (`Set__GorgetString__new`), the SECOND half of writing the element type
/// through. The accumulator TYPE feeds `wire_collection_bridges` (the
/// user-hashable ktable channel); the ctor NAME feeds
/// `set_elem_type_from_monomorphized` (the string-hash `_str` channel). They
/// are different channels reading the same invariant, so a producer that
/// supplies only one closes only half the class — emit both from the same
/// `elem_type`, never from two separately-derived facts.
///
/// Emitting the monomorphized name rather than the raw runtime symbol is also
/// what lets the LIR ctor-arg synthesis run at all: it is gated on
/// `lir_args.is_empty()`, so a producer that passes its own `SizeOf` argument
/// silently disables the very upgrade it needs. Mirrors `lower_dict_literal`'s
/// `Dict__K__V__new`.
fn collection_ctor_name(ctx: &LoweringContext, base: &str, elem_type: TypeId) -> String {
    let elem_c = type_id_to_mangle_name(ctx, elem_type);
    format!("{base}__{elem_c}__new")
}

/// Map a TypeId to a C-compatible mangle fragment for dict/set type names.
fn type_id_to_mangle_name(ctx: &LoweringContext, type_id: TypeId) -> String {
    if type_id == I64_TYPE { return "int64_t".to_string(); }
    if type_id == I32_TYPE { return "int32_t".to_string(); }
    if type_id == I16_TYPE { return "int16_t".to_string(); }
    if type_id == I8_TYPE { return "int8_t".to_string(); }
    if type_id == U64_TYPE { return "uint64_t".to_string(); }
    if type_id == U32_TYPE { return "uint32_t".to_string(); }
    if type_id == U16_TYPE { return "uint16_t".to_string(); }
    if type_id == U8_TYPE { return "uint8_t".to_string(); }
    if type_id == F64_TYPE { return "double".to_string(); }
    if type_id == F32_TYPE { return "float".to_string(); }
    if type_id == BOOL_TYPE { return "bool".to_string(); }
    if type_id == ctx.type_mapper.owned_string_type { return "GorgetString".to_string(); }
    if type_id == ctx.type_mapper.owned_string_type { return "GorgetString".to_string(); }
    // Named types
    if let Some(name) = ctx.type_name_for_id(type_id) {
        return name.to_string();
    }
    "int64_t".to_string() // fallback
}

// ---- The consuming-position element producer (shared by literals and comprehensions) ----

/// An element that has been through the consuming-position materializer.
///
/// The ONLY way to build one is [`MaterializedElement::produce`], which calls
/// `ctx.materialize_for_slot`. Every accumulator producer below takes a
/// `&MaterializedElement` rather than a bare `TypeId`, so a caller is not in a
/// position to hand a *source*-derived element type to a mint at all — the
/// wrong type is unrepresentable, not merely discouraged (AGENTS.md Core #6 /
/// #15e Q2: a guard that can catch its own class).
///
/// Doctrine: `docs/devbook/11-copy-on-write.md` §"Container literals: the
/// materializer mints the slot" — *"The element type must be taken from the
/// materialized value, never from the raw operand"* — and AGENTS.md
/// §"Ownership at Consuming Positions", which names `push` literally.
/// ⚠ The fields are PRIVATE TO THIS MODULE, and that is the whole guard.
/// While `MaterializedElement` was a plain struct in the parent module, the
/// claim above was FALSE: `lower_dict_literal` built one with a struct literal
/// at the dict-mangle chokepoint, in the same file, bypassing `produce`
/// entirely — a guard that green-lit the exact class it was written to retire
/// (AGENTS.md Core #15e Q2). Moving it one module down makes that a COMPILE
/// ERROR, so the claim is now enforced by the type system rather than asserted
/// in prose (Core #14: an invariant-asserting comment needs an enforcing
/// guard, or it gets deleted).
mod materialized {
    use super::*;

    pub(super) struct MaterializedElement {
        operand: Operand,
        ty: TypeId,
    }

    impl MaterializedElement {
        pub(super) fn produce(
            ctx: &mut LoweringContext,
            builder: &mut FunctionBuilder,
            value: Operand,
            expr: &Spanned<Expr>,
            slot: SlotType,
        ) -> Self {
            let (operand, ty) = ctx.materialize_for_slot(
                builder, value, expr, slot,
                crate::ir::ImplicitCloneReason::ConsumingArg,
            );
            MaterializedElement { operand, ty }
        }

        /// The materialized element's TYPE — the only thing an accumulator
        /// mint is ever allowed to read.
        pub(super) fn ty(&self) -> TypeId { self.ty }

        /// Consume into `(operand, ty)` for the store-and-push tail.
        pub(super) fn into_parts(self) -> (Operand, TypeId) { (self.operand, self.ty) }
    }
}

use materialized::MaterializedElement;

/// Pick the assign mode for storing a materialized element into its slot.
/// Lifted verbatim from `lower_array_literal`'s local closure so the literal
/// and the comprehensions cannot drift (AGENTS.md Core #4: fix the class).
///
/// This is the limb that closes the `[resource-moves]` ICE. `builder.assign`
/// is `AssignMode::Copy` (`builder.rs:243`), and `validate_read`
/// (`src/ir/validate.rs:1370-1380`) rejects a Copy-mode read of a resource-typed
/// source — *"shallow copy of resource"*. Materializing the operand alone does
/// NOT fix that: it produces an OWNED resource, and a Copy-mode assign then
/// shallow-copies the owned value, so the violation simply moves to the new
/// local. Materialize **and** Move.
fn element_assign_mode(
    ctx: &LoweringContext,
    builder: &FunctionBuilder,
    op: &Operand,
    ty: TypeId,
) -> AssignMode {
    if !ctx.type_registry.is_resource_type(ty) { return AssignMode::Copy; }
    match op {
        Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => {
            let src_ty = builder.local_type(p.local);
            if ctx.is_owned_local(builder, p.local)
                || (!ctx.is_named_local(p.local)
                    && (ctx.type_registry.needs_drop(src_ty)
                        || ctx.type_registry.is_resource_type(src_ty)))
            {
                AssignMode::Move
            } else {
                AssignMode::Copy
            }
        }
        _ => AssignMode::Copy,
    }
}

/// Store a materialized element into a fresh slot and hand a borrow of it to a
/// runtime collection mutator (`gorget_array_push` / `gorget_set_add`).
/// Mirrors `lower_array_literal`'s per-element tail, MoveZero included.
fn store_and_push_element(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    elem: MaterializedElement,
    acc_local: LocalId,
    acc_type: TypeId,
    mutator: &str,
) {
    let (operand, ty) = elem.into_parts();
    let slot = builder.add_local(ty, None);
    let mode = element_assign_mode(ctx, builder, &operand, ty);
    let for_zero = operand.clone();
    builder.assign_mode(mode, Place::local(slot), operand);
    if mode == AssignMode::Move {
        if let Operand::Copy(ref place) | Operand::Move(ref place) = for_zero {
            if place.projections.is_empty()
                && place.local != slot
                && !ctx.drops.is_moved(place.local)
            {
                ctx.move_zero_and_mark(builder, place.local);
            }
        }
    }
    let slot_ref = builder.borrow(Place::local(slot), ctx.register_ptr_type(ty));
    let acc_ref = builder.borrow_mut(Place::local(acc_local), ctx.register_mut_ptr_type(acc_type));
    builder.call_extern(
        mutator,
        vec![FunctionBuilder::copy(acc_ref), FunctionBuilder::copy(slot_ref)],
        UNIT_TYPE,
    );
}

/// The comprehension accumulator producer: mint the accumulator TYPE from a
/// materialized element and write it through onto the reserved local.
/// Returns the monomorphized `Vector__<elem>` / `Set__<elem>` TypeId.
fn mint_accumulator_type(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    acc_local: LocalId,
    base: &str,
    elem: &MaterializedElement,
) -> TypeId {
    let acc_type = collection_accumulator_type(ctx, base, elem.ty());
    builder.set_local_type(acc_local, acc_type);
    acc_type
}

/// The dict constructor-name producer — the SINGLE chokepoint for the
/// `Dict__K__V` mangle. Two construction sites exist (the literal and the
/// comprehension) and both route here, so the hand-rolled
/// `"Dict__int64_t__int64_t"` string is unrepresentable (Core #4: N=2, and
/// nothing stopped N+1). Takes the materialized KEY and VALUE, because
/// `dict_key_type_from_monomorphized` (`src/lir/lower/types.rs:75`) needs BOTH
/// halves of the mangle to select the `_str` key channel — a one-type ctor
/// name returns `None` there and the sizes fall back to `(8, 8)`.
fn dict_mangled_name(
    ctx: &LoweringContext,
    key: &MaterializedElement,
    val: &MaterializedElement,
) -> String {
    let key_c = type_id_to_mangle_name(ctx, key.ty());
    let val_c = type_id_to_mangle_name(ctx, val.ty());
    format!("Dict__{key_c}__{val_c}")
}

// ---- Comprehensions ----


/// Reserve a comprehension accumulator local plus the block its constructor
/// will be emitted into, and enter a fresh block for the loop.
///
/// The accumulator's element type is only knowable from the MATERIALIZED
/// element (`docs/devbook/11-copy-on-write.md` §"Container literals: the
/// materializer mints the slot": *"the element type must be taken from the
/// materialized value, never from the raw operand"*), and a comprehension's
/// element only exists inside the loop. So the ctor block is left UNTERMINATED
/// until [`close_accumulator_mint`] fills it. Minting up front is the whole
/// defect: it can only be done from the SOURCE element type or a hardcoded
/// guess, which is what sized a `Vector[Q]` accumulator at 8 bytes and
/// installed `gorget_string_free` as a `Vector[int]`'s element destructor.
struct PendingAccumulator {
    local: LocalId,
    mint_bb: BlockId,
    entry_bb: BlockId,
}

fn reserve_accumulator(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    placeholder: TypeId,
) -> PendingAccumulator {
    let local = builder.add_local(placeholder, None);
    ctx.set_owned(builder, local);
    let mint_bb = builder.new_block();
    let entry_bb = builder.new_block();
    builder.jump(mint_bb);
    builder.switch_to(entry_bb);
    PendingAccumulator { local, mint_bb, entry_bb }
}

/// Emit the accumulator's constructor into the reserved block and rejoin.
/// `ctor_args` is EMPTY for the monomorphized set/dict ctors: the LIR ctor-arg
/// synthesis that performs the `_str` key-channel upgrade is gated on
/// `lir_args.is_empty()` (`src/lir/lower/insts.rs:3716-3718` / `:3742`), so a
/// producer that passes its own `SizeOf` silently disables the very upgrade it
/// needs (ratified in `lower_set_literal_from_array`'s ctor-arg comment in
/// this file — cite by grep-anchor: this diff moved ~470 lines and every
/// line-number cite into it went stale).
fn close_accumulator_mint(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    pending: &PendingAccumulator,
    ctor: &str,
    ctor_args: Vec<Operand>,
    acc_type: TypeId,
) {
    let after_bb = builder.current_block;
    builder.switch_to(pending.mint_bb);
    builder.call_extern_into(pending.local, ctor, ctor_args);
    ctx.drops.register_local(pending.local, acc_type, &ctx.type_registry);
    builder.jump(pending.entry_bb);
    builder.switch_to(after_bb);
}

/// A comprehension whose SOURCE lowered no loop at all: there is no element,
/// therefore no element type, therefore no accumulator to mint.
///
/// This replaces three `.expect("comprehension body_fn always runs — the loop
/// body is lowered unconditionally")` sites. That string was a FALSE invariant
/// with no enforcing guard (AGENTS.md Core #14): `lower_for_iterable_with` has
/// five paths that find no usable `iter()`/`next()` signature and lower
/// nothing, so `body_fn` demonstrably does NOT always run. The three sites
/// aborted with a message asserting exactly the thing that had just failed.
///
/// ⚠ WHY THIS IS STILL LOUD, and why it is not the reference-grade answer.
/// Core #10 is lower-**or-reject**, and the reject belongs at CHECK time: the
/// typechecker accepts `[x * 2 for x in p]` for a `p` with no `iter()` (it
/// discards the comprehension's element type entirely and returns `error_id`
/// — `src/semantic/typecheck.rs`, all three comprehension arms), and there is
/// no "is this iterable?" gate anywhere in the front end. So the case has no
/// SUBJECT to widen, not a rule that is too narrow (Core #15e Q4). Adding one
/// changes accept/reject and therefore owes all three lanes (Core #9) plus the
/// ggdef corpus — out of this track's reach, FILED with a durable repro at
/// `tests/fixtures/known_gaps/comprehension_over_non_iterable_struct.gg` and a
/// `TODO.md` bullet.
///
/// Until then this aborts LOUDLY rather than minting a placeholder
/// accumulator: a silently-empty `Vector`/`Set`/`Dict` from a program the
/// front end wrongly accepted is the Core #8 trap (a loud failure traded for a
/// silent wrong value), and the statement-`for` form of the same program
/// already runs zero iterations silently — that divergence is part of the
/// filed defect, not something to "fix" by making the comprehension silent too.
fn comprehension_source_not_iterable(
    iterable: &Spanned<Expr>,
    dispatched: bool,
    kind: &str,
) -> ! {
    panic!(
        "cannot lower a {kind} comprehension: its source expression at {:?} lowered no loop, so no element -- and hence no accumulator element type -- was ever produced (the source dispatched to an arm: {dispatched}). This is a program the TYPECHECKER should have accepted or rejected and did neither: the source has no usable `iter()`/`next()` pair, and there is no is-it-iterable gate in the front end. Filed in TODO.md with the repro `tests/fixtures/known_gaps/comprehension_over_non_iterable_struct.gg`; the fix is a check-time rejection on every lane, not a lowering fallback -- a placeholder accumulator here would trade a loud failure for a silently empty collection.",
        iterable.span,
    );
}

fn comprehension_var_name(variable: &Spanned<Pattern>) -> String {
    match &variable.node {
        Pattern::Binding(name) => name.clone(),
        _ => "_comp_var".to_string(),
    }
}

/// Lower `[expr for var in iterable if condition]` to a loop that builds an array.
pub(super) fn lower_list_comprehension(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    comp_expr: &Spanned<Expr>,
    variable: &Spanned<Pattern>,
    iterable: &Spanned<Expr>,
    condition: Option<&Spanned<Expr>>,
) -> Operand {
    let array_type = ctx.type_mapper.lookup_named("GorgetArray").unwrap_or(UNIT_TYPE);
    // CoW-2G pre-header hoist, BODY channel. The statement-`for` performs it
    // at the top of `lower_for` (`stmts/for_loops.rs`); a comprehension body is
    // an EXPRESSION, and the shared driver never sees it (it receives
    // `body_fn`, not the expression), so the hoist has to happen HERE, at each
    // emitter, while the body expression is still in scope. It is fed through the `condition`
    // slot because `cow_mutations_in_loop` already has an expression entry
    // (`cow_after_expr_moves`). The FILTER channel is hoisted once at the
    // producer instead — see `drive_comprehension_loop`.
    // ⚠ There are FOUR body-channel calls, not three: the dict carries
    // `key_expr` AND `val_expr`, and a val-only hoist leaves the key side
    // wrong (measured `xs.len()/ys.len()/a.len()`, correct `2/2/4`: a set or
    // dict comprehension came out of the dispatcher refactor at `4/2/4`,
    // WORSE than HEAD's `3/2/4`, until its own call landed).
    {
        let empty = crate::parser::ast::Block::synthetic(Vec::new(), comp_expr.span);
        crate::ir::lowering::stmts::materialize_loop_carried_bare_params(
            ctx, builder, &empty, Some(&comp_expr.node), None, comp_expr.span,
        );
    }
    let pending = reserve_accumulator(ctx, builder, array_type);
    let var_name = comprehension_var_name(variable);

    let mut minted: Option<(TypeId, TypeId)> = None;
    let dispatched;
    {
        let minted_ref = &mut minted;
        let acc_local = pending.local;
        let mut body_fn = |ctx: &mut LoweringContext, builder: &mut FunctionBuilder| {
            let elem_val = lower_expr(ctx, builder, comp_expr);
            let elem = MaterializedElement::produce(
                ctx, builder, elem_val, comp_expr, SlotType::FromOperand);
            let elem_type = elem.ty();
            let acc_type = mint_accumulator_type(ctx, builder, acc_local, "Vector", &elem);
            store_and_push_element(ctx, builder, elem, acc_local, acc_type, "gorget_array_push");
            *minted_ref = Some((elem_type, acc_type));
        };
        dispatched = crate::ir::lowering::stmts::for_loops::drive_comprehension_loop(
            ctx, builder, &var_name, iterable, condition, &mut body_fn);
    }

    let Some((elem_type, acc_type)) = minted else {
        comprehension_source_not_iterable(iterable, dispatched, "list")
    };
    close_accumulator_mint(
        ctx, builder, &pending,
        "gorget_array_new",
        vec![Operand::Constant(Constant::SizeOf(elem_type))],
        acc_type,
    );
    FunctionBuilder::copy(pending.local)
}

/// Lower `{expr for var in iterable if condition}` (set comprehension).
pub(super) fn lower_set_comprehension(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    comp_expr: &Spanned<Expr>,
    variable: &Spanned<String>,
    iterable: &Spanned<Expr>,
    condition: Option<&Spanned<Expr>>,
) -> Operand {
    let set_type_placeholder = ctx.type_mapper.lookup_named("GorgetSet")
        .or_else(|| ctx.type_mapper.lookup_named("GorgetArray"))
        .unwrap_or(UNIT_TYPE);

    // Ordering comes from the declared destination, read BEFORE any mutation.
    let is_ordered = ctx.func_state.expected_type
        .and_then(|et| ctx.type_registry.collection_kind(et))
        == Some(CollectionKind::OrderedSet);
    let base = if is_ordered { "Set" } else { "HashSet" };

    // CoW-2G pre-header hoist, BODY channel — see `lower_list_comprehension`
    // for the full rationale. Its own call, not the list's: the four
    // body-channel sites are list ×1, set ×1, dict ×2.
    {
        let empty = crate::parser::ast::Block::synthetic(Vec::new(), comp_expr.span);
        crate::ir::lowering::stmts::materialize_loop_carried_bare_params(
            ctx, builder, &empty, Some(&comp_expr.node), None, comp_expr.span,
        );
    }
    let pending = reserve_accumulator(ctx, builder, set_type_placeholder);
    let var_name = variable.node.clone();

    let mut minted: Option<(TypeId, String)> = None;
    let dispatched;
    {
        let minted_ref = &mut minted;
        let acc_local = pending.local;
        let mut body_fn = |ctx: &mut LoweringContext, builder: &mut FunctionBuilder| {
            let elem_val = lower_expr(ctx, builder, comp_expr);
            let elem = MaterializedElement::produce(
                ctx, builder, elem_val, comp_expr, SlotType::FromOperand);
            // BOTH key channels off the SAME materialized type: the accumulator
            // TYPE feeds `wire_collection_bridges` (user-hashable ktable), the
            // ctor NAME feeds `set_elem_type_from_monomorphized` (the `_str`
            // string-hash upgrade). A producer that supplies only one closes
            // half the class (see `collection_ctor_name` and
            // `mint_accumulator_type` in this file).
            let acc_type = mint_accumulator_type(ctx, builder, acc_local, base, &elem);
            let ctor = collection_ctor_name(ctx, base, elem.ty());
            store_and_push_element(ctx, builder, elem, acc_local, acc_type, "gorget_set_add");
            *minted_ref = Some((acc_type, ctor));
        };
        dispatched = crate::ir::lowering::stmts::for_loops::drive_comprehension_loop(
            ctx, builder, &var_name, iterable, condition, &mut body_fn);
    }

    let Some((acc_type, ctor)) = minted else {
        comprehension_source_not_iterable(iterable, dispatched, "set")
    };
    close_accumulator_mint(ctx, builder, &pending, &ctor, vec![], acc_type);
    FunctionBuilder::copy(pending.local)
}

/// Lower `{key: value for var in iterable if condition}`.
pub(super) fn lower_dict_comprehension(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    key_expr: &Spanned<Expr>,
    val_expr: &Spanned<Expr>,
    variables: &[Spanned<String>],
    iterable: &Spanned<Expr>,
    condition: Option<&Spanned<Expr>>,
) -> Operand {
    let dict_placeholder = ctx.type_mapper.lookup_named("GorgetMap")
        .or_else(|| ctx.type_mapper.lookup_named("GorgetArray"))
        .unwrap_or(UNIT_TYPE);
    // CoW-2G pre-header hoist, BODY channel — TWICE, because a dict body has
    // two expressions. See `lower_list_comprehension` for the full rationale;
    // a val-only hoist leaves the KEY side unmaterialized.
    {
        let empty = crate::parser::ast::Block::synthetic(Vec::new(), key_expr.span);
        crate::ir::lowering::stmts::materialize_loop_carried_bare_params(
            ctx, builder, &empty, Some(&key_expr.node), None, key_expr.span,
        );
    }
    {
        let empty = crate::parser::ast::Block::synthetic(Vec::new(), val_expr.span);
        crate::ir::lowering::stmts::materialize_loop_carried_bare_params(
            ctx, builder, &empty, Some(&val_expr.node), None, val_expr.span,
        );
    }
    let pending = reserve_accumulator(ctx, builder, dict_placeholder);
    let var_name = variables.first()
        .map(|v| v.node.clone())
        .unwrap_or_else(|| "_dict_comp_var".to_string());

    let mut minted: Option<(TypeId, String)> = None;
    let dispatched;
    {
        let minted_ref = &mut minted;
        let acc_local = pending.local;
        let mut body_fn = |ctx: &mut LoweringContext, builder: &mut FunctionBuilder| {
            let k_val = lower_expr(ctx, builder, key_expr);
            let key = MaterializedElement::produce(
                ctx, builder, k_val, key_expr, SlotType::FromOperand);
            let v_val = lower_expr(ctx, builder, val_expr);
            let val = MaterializedElement::produce(
                ctx, builder, v_val, val_expr, SlotType::FromOperand);

            // The SINGLE dict-mangle chokepoint, shared with `lower_dict_literal`.
            let mangled = dict_mangled_name(ctx, &key, &val);
            let dict_type = ctx.ensure_collection_type(&mangled);
            builder.set_local_type(acc_local, dict_type);
            let put_fn = format!("{mangled}__put");
            ctx.consume_externs.insert(put_fn.clone());
            let (key_op, key_ty) = key.into_parts();
            let (val_op, val_ty) = val.into_parts();
            insert_pair(
                ctx, builder, acc_local, dict_type, &put_fn,
                key_op, val_op, key_ty, val_ty, key_expr, val_expr,
            );
            *minted_ref = Some((dict_type, format!("{mangled}__new")));
        };
        dispatched = crate::ir::lowering::stmts::for_loops::drive_comprehension_loop(
            ctx, builder, &var_name, iterable, condition, &mut body_fn);
    }

    let Some((acc_type, ctor)) = minted else {
        comprehension_source_not_iterable(iterable, dispatched, "dict")
    };
    close_accumulator_mint(ctx, builder, &pending, &ctor, vec![], acc_type);
    FunctionBuilder::copy(pending.local)
}

// ---- Optional Chaining ----

/// Lower `obj?.field` to a null-check + conditional field access.
pub(super) fn lower_optional_chain(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    field: &Spanned<String>,
) -> Operand {
    let obj = lower_expr(ctx, builder, object);
    let obj_type = infer_operand_type_full(ctx, &obj, builder);
    let obj_local = builder.add_local(obj_type, None);
    builder.assign(Place::local(obj_local), obj);

    // Check if not null
    let not_null = builder.cmp(
        CmpOp::Ne,
        obj_type,
        FunctionBuilder::copy(obj_local),
        Operand::Constant(Constant::Null),
    );

    let result_local = builder.add_local(I64_TYPE, None); // placeholder result type
    let then_bb = builder.new_block();
    let else_bb = builder.new_block();
    let merge_bb = builder.new_block();

    builder.branch(FunctionBuilder::copy(not_null), then_bb, else_bb);

    // then: access the field
    builder.switch_to(then_bb);
    // Try to resolve field via struct field cache
    let field_val = if let Some(type_name) = ctx.type_name_for_id(obj_type) {
        if let Some((field_idx, field_type)) = ctx.lookup_field(type_name, &field.node) {
            let dst = builder.field_load(Place::local(obj_local), field_idx, field_type);
            FunctionBuilder::copy(dst)
        } else {
            Operand::Constant(Constant::Null)
        }
    } else {
        // Try through pointer dereference
        if let Some(pointee) = ctx.pointee_type(obj_type) {
            if let Some(type_name) = ctx.type_name_for_id(pointee) {
                if let Some((field_idx, field_type)) = ctx.lookup_field(type_name, &field.node) {
                    let mut deref_place = Place::local(obj_local);
                    deref_place.projections.push(Projection::Deref);
                    let dst = builder.field_load(deref_place, field_idx, field_type);
                    FunctionBuilder::copy(dst)
                } else {
                    Operand::Constant(Constant::Null)
                }
            } else {
                Operand::Constant(Constant::Null)
            }
        } else {
            Operand::Constant(Constant::Null)
        }
    };
    builder.assign(Place::local(result_local), field_val);
    builder.jump(merge_bb);

    // else: assign null
    builder.switch_to(else_bb);
    builder.assign(Place::local(result_local), Operand::Constant(Constant::Null));
    builder.jump(merge_bb);

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_local)
}

// ---- Range Expressions ----

/// Lower `start..end` or `start..=end` to a `GorgetRange` struct init.
pub(super) fn lower_range_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    start: Option<&Spanned<Expr>>,
    end: Option<&Spanned<Expr>>,
    inclusive: bool,
) -> Operand {
    let start_val = if let Some(s) = start {
        lower_expr(ctx, builder, s)
    } else {
        Operand::Constant(Constant::I64(0))
    };
    // D22 open-end slice (`v[a:]` / `v[:]`): the range's end is None; pass
    // `i64::MAX` so the runtime clamp (`end > arr->len → arr->len`) reduces
    // it to the receiver's length at the actual slice call. The for-loop
    // range fast path (the `Expr::Range { start: Some, end: Some }` arm of
    // `lower_for`, `src/ir/lowering/stmts/for_loops.rs`) requires
    // Some(end) so it never observes the sentinel — only the slice path
    // does, and that path is guarded by the runtime clamp landed in the
    // same round (Track C, C-2). Pre-C-2, an open-end slice traps with the
    // OOB message; post-C-2, it succeeds as the empty/full clamped slice.
    let end_val = if let Some(e) = end {
        lower_expr(ctx, builder, e)
    } else {
        Operand::Constant(Constant::I64(i64::MAX))
    };
    let inclusive_val = Operand::Constant(Constant::Bool(inclusive));

    let range_type = ctx.type_mapper.lookup_named("GorgetRange").unwrap_or(UNIT_TYPE);
    let dst = builder.struct_init(
        "GorgetRange",
        range_type,
        vec![start_val, end_val, inclusive_val],
    );
    FunctionBuilder::copy(dst)
}
