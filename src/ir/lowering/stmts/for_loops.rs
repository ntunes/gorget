//! For-loop lowering: range, string, array, dict, set, iterable, enumerate variants.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{Block, Expr, Ownership, Pattern};
use crate::span::Spanned;

use super::super::context::LoweringContext;
use super::super::drops::DropScopeKind;
use super::super::exprs::{lower_expr, infer_operand_type_full};
use super::{lower_block, lower_block_scoped, emit_pattern_bindings};

// ============================================================================
// Shared scaffolding helpers (Layer 1 + 2 dedup from per-collection lowerings).
// ============================================================================

/// Block IDs for a for-loop. `incr_bb` is `None` when the lowering advances
/// the iterator inside the header (iterator protocol). `break_exit_bb` and
/// `else_exit_bb` collapse to `exit_bb` when there is no `else` arm.
struct ForBlocks {
    header_bb: BlockId,
    body_bb: BlockId,
    incr_bb: Option<BlockId>,
    exit_bb: BlockId,
    break_exit_bb: BlockId,
    else_exit_bb: BlockId,
}

/// Allocate the standard 4-6 blocks for a for-loop in the order
/// `header, body, [incr,] exit, [break, else]`. Order is load-bearing for
/// LIR/snapshot stability.
fn alloc_for_blocks(builder: &mut FunctionBuilder, has_incr: bool, has_else: bool) -> ForBlocks {
    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let incr_bb = if has_incr { Some(builder.new_block()) } else { None };
    let exit_bb = builder.new_block();
    let (break_exit_bb, else_exit_bb) = if has_else {
        (builder.new_block(), builder.new_block())
    } else {
        (exit_bb, exit_bb)
    };
    ForBlocks { header_bb, body_bb, incr_bb, exit_bb, break_exit_bb, else_exit_bb }
}

/// Bind `iter_op` to a fresh local. Resource-typed iters use `Borrow` (the
/// local is a non-owning view; the source still owns the heap data); value
/// types use `Copy`. Shared by array/enumerate/dict/set/iterable.
fn init_borrow_iter_local(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    iter_op: Operand,
) -> (LocalId, TypeId) {
    let iter_type = infer_operand_type_full(ctx, &iter_op, builder);
    let iter_local = builder.add_local(iter_type, None);
    let mode = if ctx.type_registry.is_resource_type(iter_type) {
        AssignMode::Borrow
    } else {
        AssignMode::Copy
    };
    builder.assign_mode(mode, Place::local(iter_local), iter_op);
    (iter_local, iter_type)
}

/// Emit the trailing else-arm + break-exit dispatch. No-op when `else_arm`
/// is `None`. Caller must have allocated distinct `break_exit_bb`/
/// `else_exit_bb` (via `alloc_for_blocks(_, _, has_else=true)`).
fn emit_else_arm_tail(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    else_arm: Option<&Block>,
    blocks: &ForBlocks,
) {
    if let Some(else_body) = else_arm {
        builder.switch_to(blocks.else_exit_bb);
        lower_block_scoped(ctx, builder, else_body);
        builder.jump(blocks.exit_bb);
        builder.switch_to(blocks.break_exit_bb);
        builder.jump(blocks.exit_bb);
    }
}

/// Emit a `state == USED` filter for dict/set iteration: read the slot
/// state at `idx_local`, branch to a fresh `elem_bb` on match (and switch
/// to it), or to `incr_bb` on miss. Returns the new `elem_bb`.
fn emit_state_filter(
    builder: &mut FunctionBuilder,
    ptr_local: LocalId,
    idx_local: LocalId,
    incr_bb: BlockId,
) -> BlockId {
    let state = builder.call_extern(
        "gorget_map_iter_state",
        vec![FunctionBuilder::copy(ptr_local), FunctionBuilder::copy(idx_local)],
        I64_TYPE,
    );
    let state_ok = builder.cmp(
        CmpOp::Eq,
        I64_TYPE,
        FunctionBuilder::copy(state),
        Operand::Constant(Constant::I64(1)),
    );
    let elem_bb = builder.new_block();
    builder.branch(FunctionBuilder::copy(state_ok), elem_bb, incr_bb);
    builder.switch_to(elem_bb);
    elem_bb
}

/// Lower a for loop over a range (`for i in start..end`).
pub(super) fn lower_for(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    pattern: &Spanned<Pattern>,
    // Track 1A: `for x in &coll` carries `&` in the statement's ownership field
    // (the parser splits it off the iterable — `for_stmt`'s
    // `parse_ownership_modifier`), so it arrives here, NOT as `Expr::MutableBorrow`.
    ownership: Ownership,
    iterable: &Spanned<Expr>,
    body: &Block,
    else_arm: Option<&Block>,
) {
    // CoW 2G: materialize loop-carried bare-param mutations in the PRE-HEADER
    // (before the iterable is lowered and before any for-variant's
    // save_locals), so the body's read-before-write / mutate reads the
    // persistent private copy rather than a per-iteration throwaway clone.
    // Shared across every for variant (range/vector/string/dict/set/enum/
    // iterable). No condition to scan — the iterable is evaluated once
    // pre-loop, so body-only detection. A no-op on an owned root or a body
    // that does not mutate the param.
    super::materialize_loop_carried_bare_params(ctx, builder, body, None, iterable.span);

    if let Pattern::Binding(var_name) = &pattern.node {
        if let Expr::Range {
            start: Some(start),
            end: Some(end),
            inclusive,
        } = &iterable.node
        {
            lower_for_range(ctx, builder, var_name, start, end, *inclusive, body, else_arm);
            return;
        }
    }

    // Detect `for (i, elem) in collection.enumerate():` and lower as index-tracked loop.
    // Track 1A remediation: the `&` write-through mode applies to enumerate too —
    // `for i, x in &a.enumerate():` carries the `&` in the statement's ownership
    // field (or, parenthesized, as `Expr::MutableBorrow` around the call / the
    // receiver). Strip every shape and thread `write_through` so the element
    // binds as a write-through place (§3.1: an unbroken `&` chain writes through).
    if let Pattern::Tuple(parts) = &pattern.node {
        if parts.len() == 2 {
            let (en_wt_stmt, en_iterable): (bool, &Spanned<Expr>) =
                match (&ownership, &iterable.node) {
                    (Ownership::MutableBorrow, Expr::MutableBorrow { expr }) => (true, expr),
                    (Ownership::MutableBorrow, _) => (true, iterable),
                    (_, Expr::MutableBorrow { expr }) => (true, expr),
                    _ => (false, iterable),
                };
            if let Expr::MethodCall { receiver, method, args: call_args, .. } = &en_iterable.node {
                if method.node == "enumerate" && call_args.is_empty() {
                    // `(&a).enumerate()`: the `&` lands on the RECEIVER instead.
                    let (en_wt_recv, receiver): (bool, &Spanned<Expr>) =
                        if let Expr::MutableBorrow { expr } = &receiver.node {
                            (true, expr)
                        } else {
                            (false, receiver)
                        };
                    lower_for_enumerate(
                        ctx, builder, parts, receiver, body, else_arm,
                        en_wt_stmt || en_wt_recv,
                    );
                    return;
                }
            }
        }
    }

    // Track 1A: `for x in &coll` write-through mode. The `&` normally arrives in
    // the statement's `ownership` field (the `for_stmt` parser splits it off the
    // iterable); a parenthesized `for x in (&coll)` instead lands as
    // `Expr::MutableBorrow` on the iterable. Either way, `write_through` drives
    // the element binding (a body `x.f = v` writes THROUGH into the collection),
    // and `ident_expr` (the `&`-stripped underlying collection) feeds the
    // identity capture + alias-root sever below. Bare `for x in coll` keeps the
    // materialize-on-write element binding.
    let (write_through, ident_expr): (bool, &Spanned<Expr>) =
        match (&ownership, &iterable.node) {
            (Ownership::MutableBorrow, Expr::MutableBorrow { expr }) => (true, expr),
            (Ownership::MutableBorrow, _) => (true, iterable),
            (_, Expr::MutableBorrow { expr }) => (true, expr),
            _ => (false, iterable),
        };

    // Alias-root SEVER (Track 1A — the hard cross-lane case). A write-through
    // loop over a lazy borrow-alias root (`Vector[T] b = a; for c in &b:`) would
    // otherwise write THROUGH into a's SHARED buffer (`b = a` is a 0-clone lazy
    // alias in Rust gg). Materialize the root ONCE before the loop
    // (`cow_before_mutation` Case 1 clones the alias source in, rebinding the
    // name to an owned local) so every through-write lands in b's PRIVATE buffer;
    // a no-op on an owned root (no alias → 0 clones). Runs before the iterable is
    // lowered so `&b` points into the materialized buffer, and before the loop's
    // `save_locals` so the rebind survives `restore_locals`. Identifier roots
    // only — a projected root (`&self.field`, `&v[i]`) routes through its own
    // field/element mutation path (deferred write facets).
    if write_through {
        if let Expr::Identifier(root_name) = &ident_expr.node {
            if let Some((root_local, _)) = ctx.lookup_local(root_name) {
                ctx.cow_before_mutation(builder, root_local, ident_expr.span);
            }
        }
    }

    // Lower the iterable and check its type for string/collection iteration
    let iter_op = lower_expr(ctx, builder, iterable);
    let iter_type = infer_operand_type_full(ctx, &iter_op, builder);
    // Capture the iterable's identity from the AST (the lowered operand is
    // often an unnamed borrow/deref tmp for `&`-param iterables): a named
    // local resolves to `CollectionId::Local`, a field-access chain to
    // `CollectionId::FieldPath` — mirroring the `.get()` provenance derivation
    // in exprs/methods.rs. Element-FIELD borrows need this identity for CoW
    // provenance (threaded into lower_for_array's element tag). Derived from the
    // `&`-stripped `ident_expr` so a `for c in &a` loop carries a's identity.
    let iter_source_coll: Option<crate::ir::lowering::context::CollectionId> =
        if let Expr::Identifier(name) = &ident_expr.node {
            ctx.lookup_local(name)
                .map(|(lid, _)| crate::ir::lowering::context::CollectionId::Local(lid))
        } else {
            super::super::exprs::extract_field_path_string(&ident_expr.node)
                .filter(|p| !ctx.is_source_mut_unsafe_at(p, ident_expr.span.start))
                .map(crate::ir::lowering::context::CollectionId::FieldPath)
        };

    // §6.8 Stage 5: when iterating a string AND the iterable is Ptr-typed
    // (e.g. a borrowed resource param), preserve iter_op as the Ptr — so
    // lower_for_string's source-aware picker can route through
    // slot_kind=BorrowedPtr without going through a value-typed shallow
    // alias. For collections, auto-deref the Ptr (shared with the list
    // comprehension via `deref_ptr_collection_iterable`).
    let (iter_op, iter_type) =
        super::super::exprs::deref_ptr_collection_iterable(ctx, builder, iter_op, iter_type);

    // Extract the binding name (or use a temp for pattern destructuring)
    let var_name = if let Pattern::Binding(name) = &pattern.node {
        name.clone()
    } else {
        "__for_elem".to_string()
    };

    if ctx.type_mapper.is_string_type(iter_type) {
        lower_for_string(ctx, builder, &var_name, iter_op, iterable, body, else_arm);
    } else {
        // Determine collection kind from type metadata (set by BuiltinTypeProtocol).
        // Falls back to name-based detection, then Iterable/Iterator trait dispatch.
        use crate::ir::types::CollectionKind;
        // Derive `collection_kind` from `iter_type` (already pointee-resolved for
        // the Copy/Move/non-Ptr branches above), not from the `iter_op` place.
        // The place-keyed lookup only fired for Copy/Move operands, so a
        // module-level `static Vector[…]` — lowered to `Constant(GlobalRef(name))`,
        // not a place — dropped to None and the loop emitted nothing. `iter_type`
        // resolves the static's `Vector__T` correctly regardless of operand shape.
        // ⚠ KEEP the pointee unwrap: the `:170-171 else` path can leave `iter_type`
        // as `Ptr(_)`, which would otherwise → None → drop.
        let collection_kind = {
            let tid = ctx.pointee_type(iter_type).unwrap_or(iter_type);
            if let Some(GirType::Named(name)) = ctx.type_registry.get(tid) {
                // Metadata-based: read `collection_kind` from the TypeDef.
                // Both runtime singletons (GorgetArray/GorgetMap/GorgetSet —
                // registered in `lowering/mod.rs`) and monomorphized aliases
                // (Vector__T/Dict__K__V/... — registered via
                // `register_collection_alias` in `lowering/types.rs:766`)
                // carry `collection_kind` in their TypeMetadata. The earlier
                // name-prefix fallback was dead — Phase A made it so.
                ctx.type_registry.get_type_def(name)
                    .and_then(|td| td.metadata.collection_kind)
            } else {
                None
            }
        };
        match collection_kind {
            Some(CollectionKind::Array) =>
                lower_for_array(ctx, builder, &var_name, iter_op, body, else_arm, pattern, iter_source_coll, write_through),
            Some(CollectionKind::OrderedMap | CollectionKind::Map) =>
                lower_for_dict(ctx, builder, iter_op, body, else_arm, pattern),
            Some(CollectionKind::OrderedSet | CollectionKind::Set) =>
                lower_for_set(ctx, builder, &var_name, iter_op, body, else_arm),
            None => {
                // Try Iterable/Iterator trait dispatch for user-defined types
                if let Some(type_name) = ctx.type_registry.get(iter_type)
                    .and_then(|gt| if let GirType::Named(n) = gt { Some(n.clone()) } else { None })
                {
                    lower_for_iterable(ctx, builder, &var_name, iter_op, &type_name, body, else_arm, pattern);
                }
            }
        }
    }
}

/// Lower `for ch in str_value: body` — iterate UTF-8 codepoints.
/// `pub(in crate::ir::lowering)`: also the loop shape behind String-based
/// list comprehensions (`exprs/collections.rs::lower_string_comprehension`,
/// Chain C item 7) — single UTF-8 pass, W3d lazy-source hook included.
pub(in crate::ir::lowering) fn lower_for_string(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    iter_op: Operand,
    iterable_expr: &Spanned<Expr>,
    body: &Block,
    else_arm: Option<&Block>,
) {
    let owned_string_type = ctx.type_mapper.owned_string_type;

    // Lazy loop-carried CoW, hook W3d (for-string SOURCE): `for c in s:`
    // emits the synthetic `gorget_str_codepoint_at`, which both backends
    // materialize as a `gorget_str_view_region` view into the source buffer
    // (c_lir/emit_call_extern.rs / c_lir/emit_types.rs) — on none of the
    // W3a/W3b/W3c paths. If the iterated source is a lazy-tagged local,
    // materialize it in place before the picker below captures it. The flag
    // guard keeps nested-loop / never-reached-loop cases correct and
    // zero-clone.
    ctx.materialize_lazy_source_if_needed(builder, &iter_op, iterable_expr.span);

    // §6.8 Stage 5 — source-aware picker (option (c) from the prior
    // comment). Three iter_op shapes existed:
    //   1. literal cap=0 view — iter_local can be a bit-copy; drop is no-op.
    //   2. owned clone from CoW upstream — iter_local takes ownership; drop frees.
    //   3. shared borrow — iter_local must NOT drop (source frees).
    //
    // With the SlotKind axis in place (§6.8 Stages 2-4), shape #3 can
    // now be expressed honestly: make iter_local a Ptr(String) and tag
    // its slot as BorrowedPtr. SlotLoad routing gives the pointer; the
    // .len field projection auto-decodes through the pointer at the LIR
    // layer; the source's drop is the only one. Shapes #1 and #2 keep
    // the old Copy + drop_register path — the bit-copy is harmless when
    // the source isn't drop-tracked.
    //
    // Detection: iter_op's source is "owning" if it's a bare-place Copy
    // or Move on a drop-registered local. That's the source of truth
    // for whether the source will free the heap independently.
    // Two flavors of "source has an owner besides this loop":
    //   (a) named local that's drop-registered (e.g. `String s = f"..."`)
    //   (b) Ptr-typed source (e.g. `String input` param after set_bare_param,
    //       or any local already tagged BorrowedPtr by an earlier setter)
    // Both let iter_local borrow without taking ownership. Different inits
    // (`emit_borrow` to take &source vs `assign` to copy the pointer
    // value), same end state: iter_local is Ptr(String) with
    // slot_kind=BorrowedPtr, the source's drop is the only one.
    let (source_is_owning_named, source_is_ptr_typed) = match &iter_op {
        Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => {
            let owning = ctx.drops.is_registered(p.local);
            let ptr_typed = builder.locals.get(p.local.0 as usize).map_or(false, |l| {
                matches!(ctx.type_registry.get(l.type_id),
                    Some(GirType::Ptr(_) | GirType::MutPtr(_)))
            });
            (owning && !ptr_typed, ptr_typed)
        }
        _ => (false, false),
    };
    let iter_local = if source_is_owning_named {
        let ptr_type = ctx.register_ptr_type(owned_string_type);
        let local = builder.add_local(ptr_type, None);
        if let Operand::Copy(p) | Operand::Move(p) = &iter_op {
            builder.emit_borrow(local, p.clone());
        }
        ctx.set_ref(builder, local);
        local
    } else if source_is_ptr_typed {
        // Source is already a pointer to String; copy the pointer value.
        let ptr_type = ctx.register_ptr_type(owned_string_type);
        let local = builder.add_local(ptr_type, None);
        builder.assign(Place::local(local), iter_op);
        ctx.set_ref(builder, local);
        local
    } else {
        let local = builder.add_local(owned_string_type, None);
        builder.assign(Place::local(local), iter_op);
        ctx.drops.register_local(local, owned_string_type, &ctx.type_registry);
        local
    };

    // byte_pos = 0
    let byte_pos = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(byte_pos), Operand::Constant(Constant::I64(0)));

    // len = iter.len (byte length)
    let len_local = builder.add_local(I64_TYPE, None);
    // Access .len field (field index 2 for 32-byte Str: {data, cap, len, alloc})
    let len_place = Place {
        local: iter_local,
        projections: vec![Projection::Field(2)],
    };
    builder.assign(Place::local(len_local), Operand::Copy(len_place));

    let blocks = alloc_for_blocks(builder, /*has_incr=*/ false, else_arm.is_some());
    let ForBlocks { header_bb, body_bb, exit_bb, break_exit_bb, else_exit_bb, .. } = blocks;

    builder.jump(header_bb);

    // Header: byte_pos < len
    builder.switch_to(header_bb);
    let cond = builder.cmp(CmpOp::Lt, I64_TYPE, FunctionBuilder::copy(byte_pos), FunctionBuilder::copy(len_local));
    builder.branch(FunctionBuilder::copy(cond), body_bb, else_exit_bb);

    // Body
    builder.switch_to(body_bb);
    let saved_str = ctx.save_locals(builder);
    ctx.push_loop(header_bb, break_exit_bb, builder.locals.len() as u32);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // cplen = gorget_utf8_codepoint_len((unsigned char)data[byte_pos])
    // We'll call this as an extern that takes a Str and byte offset → returns int
    let cplen = builder.call_extern(
        "gorget_utf8_codepoint_len_at",
        vec![FunctionBuilder::copy(iter_local), FunctionBuilder::copy(byte_pos)],
        I64_TYPE,
    );

    // ch = (Str){ .data = iter.data + byte_pos, .len = cplen }
    // We'll construct this as a StructInit with computed fields via extern.
    // Both backends emit `gorget_str_codepoint_at` as a cap=0
    // `gorget_str_view_region` view into the source buffer (it does NOT
    // allocate). Drop-registering the local is still correct — the
    // cap-driven free no-ops on views — and keeps the slot sound if a
    // future backend materializes an owned copy here.
    let ch_local = builder.call_extern(
        "gorget_str_codepoint_at",
        vec![FunctionBuilder::copy(iter_local), FunctionBuilder::copy(byte_pos)],
        owned_string_type,
    );
    ctx.register_local(var_name, ch_local, owned_string_type);
    ctx.drops.register_local(ch_local, owned_string_type, &ctx.type_registry);
    // Tag FreshOwned so the consume-site validator sees a concrete state at
    // downstream sinks (e.g., `stack.push(ch)` in
    // `string_algorithms::is_balanced`). Without this the local stays at
    // default Untracked, surfaced by the Tier 2a `consume_externs`
    // promotion 2026-05-12. NOTE the value is actually a cap=0 VIEW (see
    // above) — the tag is run-proven sound because every owning consume
    // site routes through the collection-put materialize hooks / boundary
    // clones, which upgrade cap=0 views on entry.
    ctx.set_owned_fresh(builder, ch_local);

    // Lower the body
    lower_block(ctx, builder, body);

    // byte_pos += cplen
    let new_pos = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(byte_pos), FunctionBuilder::copy(cplen));
    builder.assign(Place::local(byte_pos), FunctionBuilder::copy(new_pos));

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(builder, saved_str);
    builder.jump(header_bb);

    emit_else_arm_tail(ctx, builder, else_arm, &blocks);
    builder.switch_to(exit_bb);
}

/// Bind a Vector for-loop element to `elem_name`, mode-driven per Track 1A.
/// Shared by `lower_for_array` AND `lower_for_enumerate` so the two never drift.
///
/// Returns `Some(elem_ptr_local)` when the Ptr-borrow-alias path was taken —
/// the element is a `Ptr(elem)` borrow into the collection buffer (zero per-iter
/// clone on read), and the caller must NOT drop-register it (the collection
/// owns the data). Returns `None` when the caller should fall through to its
/// value/string/tuple path (`index_load_borrow` value copy + drop reg).
///
/// The MODE decides the mutation behavior via the ownership tag:
///  - `write_through` (`for x in &coll`): `CowBorrowPending` — `cow_before_mutation`
///    does NOT sever it, so a body `x.f = v` writes THROUGH the pointer into the
///    collection's buffer. Taken for ANY struct/enum element (value OR resource).
///  - bare (`for x in coll`) over a RESOURCE struct/enum: `CollectionElement` —
///    `cow_before_mutation` Case 1b clones the element into a private owned copy
///    on the first `x.f = v`, so the write does NOT reach the collection (§3.1
///    immutable for-element). A read-only scan never triggers the clone. Value
///    structs are NOT taken here — they keep the eager value-copy path, which
///    already realizes correct materialize semantics.
fn bind_for_vector_element(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    elem_name: &str,
    is_binding: bool,
    iter_local: LocalId,
    idx: LocalId,
    elem_type: TypeId,
    write_through: bool,
    iter_source_coll: Option<crate::ir::lowering::context::CollectionId>,
) -> Option<LocalId> {
    let is_string = ctx.type_mapper.is_string_type(elem_type);
    let elem_is_struct_or_enum = ctx.type_registry
        .get(elem_type)
        .and_then(|gt| if let GirType::Named(n) = gt { Some(n.clone()) } else { None })
        .and_then(|name| ctx.type_registry.get_type_def(&name)
            .map(|td| matches!(td.kind,
                crate::ir::types::TypeDefKind::Struct(_)
                | crate::ir::types::TypeDefKind::Enum(_))))
        .unwrap_or(false);
    let is_resource_struct = !is_string
        && elem_is_struct_or_enum
        && ctx.type_registry.is_resource_type(elem_type)
        && !ctx.type_registry.is_collection_type(elem_type);
    // Ptr-borrow-alias path: `&coll` takes it for any struct/enum element; bare
    // `coll` only for a resource struct/enum (value structs keep the value-copy).
    if !(!is_string && elem_is_struct_or_enum && is_binding && (write_through || is_resource_struct)) {
        return None;
    }
    let ptr_type = ctx.register_ptr_type(elem_type);
    let elem = builder.index_load_borrow(
        Place::local(iter_local),
        FunctionBuilder::copy(idx),
        ptr_type,
    );
    ctx.register_local(elem_name, elem, ptr_type);
    if write_through {
        // `for x in &coll`: write-through. CowBorrowPending is left untouched by
        // cow_before_mutation, so `x.f = v` writes THROUGH (the alias root was
        // already severed at loop entry).
        ctx.set_cow_borrow(builder, elem);
    } else {
        // bare resource: materialize-on-write via the CollectionElement tag.
        // Name the element so `cow_materialize_collection_ref` can rebind it to
        // the private owned copy on the first `x.f = v` (it rebinds by the
        // builder name hint — `index_load_borrow` mints an anonymous temp, so
        // without this the clone is emitted but the write still targets the
        // collection element).
        builder.set_local_name(elem, elem_name);
        let cid = iter_source_coll.clone()
            .unwrap_or(crate::ir::lowering::context::CollectionId::Local(iter_local));
        ctx.set_collection_ref(builder, elem, cid);
    }
    // Record the source collection so element-FIELD borrows
    // (`String x = elem.name`, routed through set_field_or_elem_borrow) carry
    // collection provenance to the var-decl default-borrow branch. AST-derived
    // identities only — an unnamed iterable temp carries no identity that
    // downstream mutation tracking could route back to.
    if let Some(src) = iter_source_coll {
        ctx.set_cow_borrow_source(elem, src);
    }
    // Borrow alias — collection owns the data; do NOT register for drop.
    Some(elem)
}

/// Lower `for elem in array: body` — iterate array elements by index.
fn lower_for_array(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    iter_op: Operand,
    body: &Block,
    else_arm: Option<&Block>,
    pattern: &Spanned<Pattern>,
    // The iterable's collection identity, derived by the caller from the AST
    // (the lowered operand is an unnamed borrow/deref tmp for `&`-params):
    // element-FIELD borrows need it for CoW provenance (see the
    // set_cow_borrow_source below).
    iter_source_coll: Option<crate::ir::lowering::context::CollectionId>,
    // Track 1A: true for `for x in &coll` — the element binds write-through.
    write_through: bool,
) {
    let (iter_local, iter_type) = init_borrow_iter_local(ctx, builder, iter_op);

    // idx = 0
    let idx = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(idx), Operand::Constant(Constant::I64(0)));

    // len = iter.len (field index 2 of GorgetArray: {data, cap, len, elem_size})
    let len = builder.add_local(I64_TYPE, None);
    let len_place = Place {
        local: iter_local,
        projections: vec![Projection::Field(2)],
    };
    builder.assign(Place::local(len), Operand::Copy(len_place));

    let blocks = alloc_for_blocks(builder, /*has_incr=*/ true, else_arm.is_some());
    let ForBlocks { header_bb, body_bb, incr_bb, exit_bb, break_exit_bb, else_exit_bb } = blocks;
    let incr_bb = incr_bb.unwrap();

    builder.jump(header_bb);

    // Header: idx < len
    builder.switch_to(header_bb);
    let cond = builder.cmp(CmpOp::Lt, I64_TYPE, FunctionBuilder::copy(idx), FunctionBuilder::copy(len));

    builder.branch(FunctionBuilder::copy(cond), body_bb, else_exit_bb);

    // Body
    builder.switch_to(body_bb);
    let saved_arr = ctx.save_locals(builder);
    ctx.push_loop(incr_bb, break_exit_bb, builder.locals.len() as u32);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // elem = iter[idx] — borrow element from array (view for strings, clone for collections).
    let elem_type = super::super::exprs::infer_collection_element_type(ctx, iter_type);

    // Track 1A mode-driven element binding (shared with `lower_for_enumerate`).
    // For a struct/enum element bound to a plain identifier, the element is a
    // Ptr(elem) borrow alias into the array — no per-iter `{Type}__clone`, no
    // drop reg (the IndexLoad `Ptr`-dst early path returns the raw element
    // pointer; FieldLoad auto-derefs a Ptr base; enum method/match dispatch
    // resolves through a Ptr receiver, so body reads work unchanged). The MODE
    // (`&coll` write-through vs bare materialize-on-write) is encoded in the
    // element's ownership tag — see `bind_for_vector_element`. Tuple
    // destructuring / value-struct-bare / direct-collection / string elements
    // fall through to the value-copy path below.
    let elem_is_binding = matches!(pattern.node, Pattern::Binding(_));
    if bind_for_vector_element(
        ctx, builder, var_name, elem_is_binding, iter_local, idx, elem_type,
        write_through, iter_source_coll.clone(),
    ).is_some() {
        lower_block(ctx, builder, body);

        ctx.drops.pop_scope(builder, &ctx.type_registry);
        ctx.pop_loop();
        ctx.restore_locals(builder, saved_arr);

        builder.jump(incr_bb);
        builder.switch_to(incr_bb);
        let new_idx = builder.bin_op(
            BinOp::Add, I64_TYPE,
            FunctionBuilder::copy(idx),
            Operand::Constant(Constant::I64(1)),
        );
        builder.assign(Place::local(idx), FunctionBuilder::copy(new_idx));
        builder.jump(header_bb);

        emit_else_arm_tail(ctx, builder, else_arm, &blocks);
        builder.switch_to(exit_bb);
        return;
    }

    let elem = builder.index_load_borrow(Place::local(iter_local), FunctionBuilder::copy(idx), elem_type);
    ctx.register_local(var_name, elem, elem_type);
    // String borrows are pointer copies — do NOT register for drops.
    // The collection still owns the data; the borrow just reads through the pointer.
    // Non-string resource types still need drops (they have separate allocations).
    if !ctx.type_mapper.is_string_type(elem_type) {
        ctx.drops.register_local(elem, elem_type, &ctx.type_registry);
    }
    if ctx.type_mapper.is_string_type(elem_type) {
        ctx.func_state.has_string_borrows = true;
    }

    // If pattern is a destructuring tuple, emit bindings
    if !matches!(pattern.node, Pattern::Binding(_)) {
        emit_pattern_bindings(ctx, builder, pattern, elem, elem_type);
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(builder, saved_arr);

    // Increment idx
    builder.jump(incr_bb);
    builder.switch_to(incr_bb);
    let new_idx = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(idx), Operand::Constant(Constant::I64(1)));
    builder.assign(Place::local(idx), FunctionBuilder::copy(new_idx));
    builder.jump(header_bb);

    emit_else_arm_tail(ctx, builder, else_arm, &blocks);
    builder.switch_to(exit_bb);
}

/// Lower `for (i, elem) in collection.enumerate(): body` — iterate with index tracking.
/// Instead of materializing an enumerate array, we emit a regular for-loop with a counter.
/// `write_through` (Track 1A): true for `for i, x in &coll.enumerate()` — the element
/// binds as a write-through place, exactly like the plain `for x in &coll` loop
/// (the caller has already stripped the `&` off the receiver / statement ownership).
fn lower_for_enumerate(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    parts: &[Spanned<Pattern>],
    receiver: &Spanned<Expr>,
    body: &Block,
    else_arm: Option<&Block>,
    write_through: bool,
) {
    // Alias-root SEVER — the enumerate sibling of the `lower_for` entry sever: a
    // write-through enumerate loop over a lazy borrow-alias root (`b = a;
    // for i, c in &b.enumerate():`) must materialize the root ONCE before the
    // loop (`cow_before_mutation` Case 1) so through-writes land in b's PRIVATE
    // buffer; a no-op on an owned root. Before the receiver is lowered and
    // before `save_locals`, so the rebind survives.
    if write_through {
        if let Expr::Identifier(root_name) = &receiver.node {
            if let Some((root_local, _)) = ctx.lookup_local(root_name) {
                ctx.cow_before_mutation(builder, root_local, receiver.span);
            }
        }
    }

    // Lower the receiver collection
    let mut iter_op = lower_expr(ctx, builder, receiver);
    let raw_iter_type = infer_operand_type_full(ctx, &iter_op, builder);
    // Capture the iterable's collection identity from the AST — see the
    // sibling capture in lower_for (threaded to lower_for_array).
    let iter_source_coll: Option<crate::ir::lowering::context::CollectionId> =
        if let Expr::Identifier(name) = &receiver.node {
            ctx.lookup_local(name)
                .map(|(lid, _)| crate::ir::lowering::context::CollectionId::Local(lid))
        } else {
            super::super::exprs::extract_field_path_string(&receiver.node)
                .filter(|p| !ctx.is_source_mut_unsafe_at(p, receiver.span.start))
                .map(crate::ir::lowering::context::CollectionId::FieldPath)
        };

    // Auto-deref Ptr-typed iterables (Snag 2026-05-13). When `receiver`
    // is a borrowed parameter (`Vector[T] xs` → iter_op is
    // Ptr<GorgetArray>), the downstream `iter_local.Field(2)` len-read
    // and `index_load_borrow(iter_local, ...)` element-load assume
    // iter_local is the value type, NOT the pointer type. Without the
    // deref, the field access reads adjacent stack slots — manifests as
    // an off-by-one (or worse) `index out of bounds` panic at the first
    // post-end iteration. Mirrors the deref step at the top of `lower_for`
    // for non-enumerate Ptr iterables.
    let pointee = ctx.pointee_type(raw_iter_type);
    let (iter_op, iter_type) = if let Some(inner) = pointee {
        if let Operand::Copy(p) | Operand::Move(p) = &iter_op {
            let deref_place = Place {
                local: p.local,
                projections: vec![Projection::Deref],
            };
            let tmp = builder.add_local(inner, None);
            builder.assign_mode(
                crate::ir::instructions::AssignMode::Borrow,
                Place::local(tmp),
                Operand::Copy(deref_place),
            );
            (Operand::Copy(Place::local(tmp)), inner)
        } else {
            (iter_op, raw_iter_type)
        }
    } else {
        let _ = &mut iter_op;
        (iter_op, raw_iter_type)
    };

    let (iter_local, _) = init_borrow_iter_local(ctx, builder, iter_op);

    // idx = 0
    let idx = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(idx), Operand::Constant(Constant::I64(0)));

    // len = iter.len (field index 2 for both Str and GorgetArray under uniform layout)
    // Str: {data, cap, len, alloc}. GorgetArray: {data, cap, len, elem_size}.
    let len = builder.add_local(I64_TYPE, None);
    let len_place = Place {
        local: iter_local,
        projections: vec![Projection::Field(2)],
    };
    builder.assign(Place::local(len), Operand::Copy(len_place));

    let blocks = alloc_for_blocks(builder, /*has_incr=*/ true, else_arm.is_some());
    let ForBlocks { header_bb, body_bb, incr_bb, exit_bb, break_exit_bb, else_exit_bb } = blocks;
    let incr_bb = incr_bb.unwrap();

    builder.jump(header_bb);

    // Header: idx < len
    builder.switch_to(header_bb);
    let cond = builder.cmp(CmpOp::Lt, I64_TYPE, FunctionBuilder::copy(idx), FunctionBuilder::copy(len));

    builder.branch(FunctionBuilder::copy(cond), body_bb, else_exit_bb);

    // Body
    builder.switch_to(body_bb);
    let saved_enum = ctx.save_locals(builder);
    ctx.push_loop(incr_bb, break_exit_bb, builder.locals.len() as u32);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // Bind index variable (first tuple element)
    if let Pattern::Binding(idx_name) = &parts[0].node {
        let idx_local = builder.add_local(I64_TYPE, Some(idx_name));
        builder.assign(Place::local(idx_local), FunctionBuilder::copy(idx));
        ctx.register_local(idx_name, idx_local, I64_TYPE);
    }

    // Bind element variable (second tuple element) — load from array.
    let elem_type = super::super::exprs::infer_collection_element_type(ctx, iter_type);

    // Track 1A enumerate twin: bind the element via the SHARED mode-driven
    // helper (Core #4 — one binding path, never two drifting copies), same as
    // the plain for-loop: `&coll.enumerate()` → write-through place; bare
    // `v.enumerate()` over a RESOURCE struct/enum element → materialize-on-write
    // (a body `x.f = v` clones a private copy; the collection is untouched —
    // §3.1, the A2 enumerate-twin fix). Read-only scans (the dominant
    // `m.structs.enumerate()` self-host consumers) still pay zero per-iter
    // clone. Bare value structs / strings / non-bindings fall through.
    let elem_binding_name = if let Pattern::Binding(n) = &parts[1].node { n.as_str() } else { "" };
    let elem_is_binding = matches!(parts[1].node, Pattern::Binding(_));
    if elem_is_binding && bind_for_vector_element(
        ctx, builder, elem_binding_name, elem_is_binding, iter_local, idx, elem_type,
        write_through, iter_source_coll.clone(),
    ).is_some() {
        lower_block(ctx, builder, body);

        ctx.drops.pop_scope(builder, &ctx.type_registry);
        ctx.pop_loop();
        ctx.restore_locals(builder, saved_enum);

        builder.jump(incr_bb);
        builder.switch_to(incr_bb);
        let new_idx = builder.bin_op(
            BinOp::Add, I64_TYPE,
            FunctionBuilder::copy(idx),
            Operand::Constant(Constant::I64(1)),
        );
        builder.assign(Place::local(idx), FunctionBuilder::copy(new_idx));
        builder.jump(header_bb);

        emit_else_arm_tail(ctx, builder, else_arm, &blocks);
        builder.switch_to(exit_bb);
        return;
    }

    let elem = builder.index_load_borrow(Place::local(iter_local), FunctionBuilder::copy(idx), elem_type);
    if let Pattern::Binding(elem_name) = &parts[1].node {
        ctx.register_local(elem_name, elem, elem_type);
    }
    // String borrows: don't register for drops (pointer copy, collection owns the data).
    if !ctx.type_mapper.is_string_type(elem_type) {
        ctx.drops.register_local(elem, elem_type, &ctx.type_registry);
    }
    if ctx.type_mapper.is_string_type(elem_type) {
        ctx.func_state.has_string_borrows = true;
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(builder, saved_enum);

    // Increment idx
    builder.jump(incr_bb);
    builder.switch_to(incr_bb);
    let new_idx = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(idx), Operand::Constant(Constant::I64(1)));
    builder.assign(Place::local(idx), FunctionBuilder::copy(new_idx));
    builder.jump(header_bb);

    emit_else_arm_tail(ctx, builder, else_arm, &blocks);
    builder.switch_to(exit_bb);
}

/// Lower `for k, v in dict: body` — iterate Dict or HashMap entries.
fn lower_for_dict(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    iter_op: Operand,
    body: &Block,
    else_arm: Option<&Block>,
    pattern: &Spanned<Pattern>,
) {
    let (iter_local, iter_type) = init_borrow_iter_local(ctx, builder, iter_op);

    // Create a pointer to the dict for iterator accessor calls.
    let dict_ptr_type = ctx.register_ptr_type(iter_type);
    let dict_ptr = builder.add_local(dict_ptr_type, None);
    builder.emit_borrow(dict_ptr, Place::local(iter_local));

    // Determine key/value type names from the collection type
    let type_name = ctx.type_name_for_id(iter_type)
        .map(|s| s.to_string())
        .unwrap_or_default();
    let (key_gir_type, val_gir_type) = parse_dict_kv_types(&type_name);
    let key_type = ctx.lookup_type_by_name(&key_gir_type).unwrap_or(I64_TYPE);
    let val_type = ctx.lookup_type_by_name(&val_gir_type).unwrap_or(I64_TYPE);

    // oi = 0 (outer iteration index)
    let oi = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(oi), Operand::Constant(Constant::I64(0)));

    // limit = cap (iterate over all slots, check states for USED)
    let limit = builder.call_extern(
        "gorget_map_iter_cap",
        vec![FunctionBuilder::copy(dict_ptr)],
        I64_TYPE,
    );

    let blocks = alloc_for_blocks(builder, /*has_incr=*/ true, else_arm.is_some());
    let ForBlocks { header_bb, body_bb, incr_bb, exit_bb, break_exit_bb, else_exit_bb } = blocks;
    let incr_bb = incr_bb.unwrap();

    builder.jump(header_bb);

    // Header: oi < limit
    builder.switch_to(header_bb);
    let cond = builder.cmp(CmpOp::Lt, I64_TYPE, FunctionBuilder::copy(oi), FunctionBuilder::copy(limit));

    builder.branch(FunctionBuilder::copy(cond), body_bb, else_exit_bb);

    // Body
    builder.switch_to(body_bb);
    let saved_dict = ctx.save_locals(builder);
    ctx.push_loop(incr_bb, break_exit_bb, builder.locals.len() as u32);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // idx = oi (direct index into capacity)
    let idx = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(idx), FunctionBuilder::copy(oi));

    // Skip empty/tombstone slots; switch to elem_bb on match.
    emit_state_filter(builder, dict_ptr, idx, incr_bb);

    // Extract key/value bindings via runtime output-parameter functions.
    match &pattern.node {
        Pattern::Tuple(parts) if parts.len() == 2 => {
            let k_name = if let Pattern::Binding(n) = &parts[0].node { n.clone() } else { "__k".to_string() };
            let v_name = if let Pattern::Binding(n) = &parts[1].node { n.clone() } else { "__v".to_string() };

            let k_local = builder.add_local(key_type, Some(&k_name));
            let k_ptr_type = ctx.register_ptr_type(key_type);
            let k_ptr = builder.borrow_mut(Place::local(k_local), k_ptr_type);
            builder.call_extern_void(
                "gorget_map_iter_key",
                vec![FunctionBuilder::copy(dict_ptr), FunctionBuilder::copy(idx), FunctionBuilder::copy(k_ptr)],
            );
            ctx.register_local(&k_name, k_local, key_type);
            // The accessor wrote an OWNED (resource-cloned) key into `k_local`
            // via its out-param — register it for drop-at-scope-exit, else the
            // clone leaks once per iteration (#11). `register_local` no-ops for
            // Copy-typed keys (int, bool), so only resource keys cost a drop.
            ctx.drops.register_local(k_local, key_type, &ctx.type_registry);

            let v_local = builder.add_local(val_type, Some(&v_name));
            let v_ptr_type = ctx.register_ptr_type(val_type);
            let v_ptr = builder.borrow_mut(Place::local(v_local), v_ptr_type);
            builder.call_extern_void(
                "gorget_map_iter_value",
                vec![FunctionBuilder::copy(dict_ptr), FunctionBuilder::copy(idx), FunctionBuilder::copy(v_ptr)],
            );
            ctx.register_local(&v_name, v_local, val_type);
            ctx.drops.register_local(v_local, val_type, &ctx.type_registry);
        }
        Pattern::Binding(name) => {
            let k_local = builder.add_local(key_type, Some(name));
            let k_ptr_type = ctx.register_ptr_type(key_type);
            let k_ptr = builder.borrow_mut(Place::local(k_local), k_ptr_type);
            builder.call_extern_void(
                "gorget_map_iter_key",
                vec![FunctionBuilder::copy(dict_ptr), FunctionBuilder::copy(idx), FunctionBuilder::copy(k_ptr)],
            );
            ctx.register_local(name, k_local, key_type);
            ctx.drops.register_local(k_local, key_type, &ctx.type_registry);
        }
        _ => {}
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(builder, saved_dict);

    builder.jump(incr_bb);
    builder.switch_to(incr_bb);
    let new_oi = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(oi), Operand::Constant(Constant::I64(1)));
    builder.assign(Place::local(oi), FunctionBuilder::copy(new_oi));
    builder.jump(header_bb);

    emit_else_arm_tail(ctx, builder, else_arm, &blocks);
    builder.switch_to(exit_bb);
}

/// Lower `for elem in set: body` — iterate Set elements.
fn lower_for_set(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    iter_op: Operand,
    body: &Block,
    else_arm: Option<&Block>,
) {
    let (iter_local, iter_type) = init_borrow_iter_local(ctx, builder, iter_op);
    // Create pointer for iterator accessor calls
    let set_ptr_type = ctx.register_ptr_type(iter_type);
    let set_ptr = builder.add_local(set_ptr_type, None);
    builder.emit_borrow(set_ptr, Place::local(iter_local));

    // Parse element type from Set__T
    let type_name = ctx.type_name_for_id(iter_type)
        .map(|s| s.to_string())
        .unwrap_or_default();
    // Read typed `collection_kind` to discriminate ordered Set vs unordered
    // HashSet (the iteration shape differs: ordered walks `order[]`, unordered
    // walks `states[]`). Replaces a name-prefix probe on the receiver type.
    let is_ordered = ctx.type_registry.get_type_def(&type_name)
        .and_then(|td| td.metadata.collection_kind)
        == Some(CollectionKind::OrderedSet);
    let elem_gir_type = parse_set_elem_type(&type_name);
    let elem_type = ctx.lookup_type_by_name(&elem_gir_type).unwrap_or(I64_TYPE);

    // i = 0  (for ordered: index into order array; for unordered: index into states)
    let i_local = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(i_local), Operand::Constant(Constant::I64(0)));

    // limit: ordered uses order_len, unordered uses cap
    let limit = if is_ordered {
        builder.call_extern(
            "gorget_map_iter_order_len",
            vec![FunctionBuilder::copy(set_ptr)],
            I64_TYPE,
        )
    } else {
        builder.call_extern(
            "gorget_map_iter_cap",
            vec![FunctionBuilder::copy(set_ptr)],
            I64_TYPE,
        )
    };

    let blocks = alloc_for_blocks(builder, /*has_incr=*/ true, else_arm.is_some());
    let ForBlocks { header_bb, body_bb, incr_bb, exit_bb, break_exit_bb, else_exit_bb } = blocks;
    let incr_bb = incr_bb.unwrap();

    builder.jump(header_bb);

    builder.switch_to(header_bb);
    let cond = builder.cmp(CmpOp::Lt, I64_TYPE, FunctionBuilder::copy(i_local), FunctionBuilder::copy(limit));

    builder.branch(FunctionBuilder::copy(cond), body_bb, else_exit_bb);

    builder.switch_to(body_bb);
    let saved_set = ctx.save_locals(builder);
    ctx.push_loop(incr_bb, break_exit_bb, builder.locals.len() as u32);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // Ordered sets index through `order[]`; unordered sets walk `states[]` directly.
    // Both paths still need a state-USED filter (ordered: stale tombstones in order).
    let key_idx = if is_ordered {
        let real_i = builder.call_extern(
            "gorget_map_iter_order",
            vec![FunctionBuilder::copy(set_ptr), FunctionBuilder::copy(i_local)],
            I64_TYPE,
        );
        real_i
    } else {
        i_local
    };
    emit_state_filter(builder, set_ptr, key_idx, incr_bb);

    // Bind element via output-parameter call.
    let elem_local = builder.add_local(elem_type, Some(var_name));
    let elem_ptr_type = ctx.register_ptr_type(elem_type);
    let elem_ptr = builder.borrow_mut(Place::local(elem_local), elem_ptr_type);
    builder.call_extern_void(
        "gorget_map_iter_key",
        vec![FunctionBuilder::copy(set_ptr), FunctionBuilder::copy(key_idx), FunctionBuilder::copy(elem_ptr)],
    );
    ctx.register_local(var_name, elem_local, elem_type);
    // Owned (resource-cloned) element from the out-param accessor — register
    // for drop-at-scope-exit so resource elements don't leak per iteration
    // (#11). No-ops for Copy-typed elements.
    ctx.drops.register_local(elem_local, elem_type, &ctx.type_registry);

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(builder, saved_set);

    builder.jump(incr_bb);
    builder.switch_to(incr_bb);
    let new_i = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(i_local), Operand::Constant(Constant::I64(1)));
    builder.assign(Place::local(i_local), FunctionBuilder::copy(new_i));
    builder.jump(header_bb);

    emit_else_arm_tail(ctx, builder, else_arm, &blocks);
    builder.switch_to(exit_bb);
}

/// Lower `for var in iterable: body` for user-defined Iterable[T] types.
/// Generates: iter = Type__iter(&collection); loop { opt = Iter__next(&iter); if None: break; var = opt.Some._0; body }
fn lower_for_iterable(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    iter_op: Operand,
    type_name: &str,
    body: &Block,
    else_arm: Option<&Block>,
    pattern: &Spanned<Pattern>,
) {
    // 1. Find the iter() method for this type, or — if the type implements
    // Iterator directly (has `next()` but no `iter()`) — treat `self` as the
    // iterator by skipping the iter() call.
    // Search fn_sigs for patterns like: Iterable__T_for_TypeName__iter or TypeName__iter
    let iter_fn_name = ctx.fn_sigs.keys()
        .find(|k| k.ends_with("__iter") && k.contains(&format!("_for_{type_name}__")))
        .cloned()
        .or_else(|| {
            let direct = format!("{type_name}__iter");
            if ctx.fn_sigs.contains_key(&direct) { Some(direct) } else { None }
        });

    // Detect types that directly implement Iterator (have next() but no iter()).
    let has_next_direct = {
        let direct_next = format!("{type_name}__next");
        ctx.fn_sigs.contains_key(&direct_next)
            || ctx.fn_sigs.keys().any(|k| k.ends_with("__next") && k.contains(&format!("_for_{type_name}__")))
    };
    let self_as_iterator = iter_fn_name.is_none() && has_next_direct;

    // 2. Determine the iterator return type. If there's an iter() method, use
    // its return type; otherwise the iterator IS the collection itself.
    let iter_type_name;
    let iter_ret_type;
    if let Some(ref iter_fn) = iter_fn_name {
        let (_, ret) = match ctx.fn_sigs.get(iter_fn) {
            Some(sig) => sig.clone(),
            None => return,
        };
        iter_ret_type = ret;
        iter_type_name = match ctx.type_registry.type_name(iter_ret_type) {
            Some(name) => name,
            None => return,
        };
    } else if self_as_iterator {
        // Self-as-iterator: iter type = collection type.
        iter_ret_type = infer_operand_type_full(ctx, &iter_op, builder);
        iter_type_name = type_name.to_string();
    } else {
        return;
    }

    // 3. Find the next() method for the iterator type
    let next_fn_name = ctx.fn_sigs.keys()
        .find(|k| k.ends_with("__next") && k.contains(&format!("_for_{iter_type_name}__")))
        .cloned()
        .or_else(|| {
            let direct = format!("{iter_type_name}__next");
            if ctx.fn_sigs.contains_key(&direct) { Some(direct) } else { None }
        });

    let next_fn_name = match next_fn_name {
        Some(name) => name,
        None => return,
    };

    // 4. Get the Option return type from next()
    let (_, option_ret_type) = match ctx.fn_sigs.get(&next_fn_name) {
        Some(sig) => sig.clone(),
        None => return,
    };

    // Determine the element type from the Option type name (Option__T → T)
    let option_type_name = ctx.type_registry.type_name(option_ret_type)
        .unwrap_or_else(|| "Option__int64_t".to_string());
    let elem_c_type = option_type_name.strip_prefix("Option__")
        .unwrap_or("int64_t");
    let elem_type = ctx.type_mapper.lookup_named(elem_c_type).unwrap_or(I64_TYPE);

    // 5. Store the iterable and (optionally) call iter(). The collection_local
    // is a non-owning view into the caller's data — same shape as the deref-Ptr
    // case at line 54.
    let (collection_local, iter_type_full) = init_borrow_iter_local(ctx, builder, iter_op);

    let iterator_local = if let Some(ref iter_fn) = iter_fn_name {
        // Iterable path: Call iter(&collection) → iterator
        let self_ptr_type = ctx.register_ptr_type(iter_type_full);
        let self_ref = builder.borrow(Place::local(collection_local), self_ptr_type);
        builder.call_extern(
            iter_fn,
            vec![FunctionBuilder::copy(self_ref)],
            iter_ret_type,
        )
    } else {
        // Direct-Iterator path: the collection IS the iterator.
        collection_local
    };

    // 6. Build the loop structure. No incr_bb — `next()` advances the iterator
    // internally, so `continue` jumps back to `header_bb`.
    let blocks = alloc_for_blocks(builder, /*has_incr=*/ false, else_arm.is_some());
    let ForBlocks { header_bb, body_bb, exit_bb, break_exit_bb, else_exit_bb, .. } = blocks;

    builder.jump(header_bb);

    // Header: call next(&iterator) → Option
    builder.switch_to(header_bb);
    let iter_ptr_type = ctx.register_mut_ptr_type(iter_ret_type);
    let iter_ref = builder.borrow_mut(Place::local(iterator_local), iter_ptr_type);
    let opt_result = builder.call_extern(
        &next_fn_name,
        vec![FunctionBuilder::copy(iter_ref)],
        option_ret_type,
    );

    // Check tag: if tag != 0 (None), exit
    let tag_val = builder.tag_of(FunctionBuilder::copy(opt_result));
    let is_none = builder.cmp(
        CmpOp::Ne,
        I32_TYPE,
        FunctionBuilder::copy(tag_val),
        Operand::Constant(Constant::I32(0)),
    );
    
    builder.branch(FunctionBuilder::copy(is_none), else_exit_bb, body_bb);

    // Body: extract value from Some variant
    builder.switch_to(body_bb);
    let saved_iter = ctx.save_locals(builder);
    ctx.push_loop(header_bb, break_exit_bb, builder.locals.len() as u32);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // Extract: elem = opt_result.data.Some._0
    let elem_local = builder.enum_field_load_move(
        Place::local(opt_result),
        "Some",
        0,
        elem_type,
    );
    ctx.register_local(var_name, elem_local, elem_type);

    // If pattern is a destructuring tuple, emit bindings
    if !matches!(pattern.node, Pattern::Binding(_)) {
        emit_pattern_bindings(ctx, builder, pattern, elem_local, elem_type);
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(builder, saved_iter);
    builder.jump(header_bb);

    emit_else_arm_tail(ctx, builder, else_arm, &blocks);
    builder.switch_to(exit_bb);
}

/// Parse Dict/HashMap type name to extract key/value C type strings.
fn parse_dict_kv_types(type_name: &str) -> (String, String) {
    // Dict__Str__int64_t → ("Str", "int64_t")
    // HashMap__int64_t__Str → ("int64_t", "Str")
    let stripped = type_name
        .strip_prefix("Dict__")
        .or_else(|| type_name.strip_prefix("HashMap__"))
        .or_else(|| type_name.strip_prefix("GorgetDict__"))
        .or_else(|| type_name.strip_prefix("GorgetMap__"))
        .unwrap_or(type_name);
    // Split on __ to get key and value types
    // But type names themselves can contain __ (e.g. Option__int64_t)
    // Simple heuristic: first known type boundary
    if let Some(pos) = find_kv_split(stripped) {
        (stripped[..pos].to_string(), stripped[pos + 2..].to_string())
    } else {
        ("int64_t".to_string(), "int64_t".to_string())
    }
}

/// Parse Set type name to extract element C type string.
fn parse_set_elem_type(type_name: &str) -> String {
    type_name
        .strip_prefix("Set__")
        .or_else(|| type_name.strip_prefix("HashSet__"))
        .or_else(|| type_name.strip_prefix("GorgetSet__"))
        .unwrap_or("int64_t")
        .to_string()
}

/// Find the __ separator between key and value types.
fn find_kv_split(s: &str) -> Option<usize> {
    // Known primitive suffixes to try splitting on
    let primitives = ["int64_t", "int32_t", "int16_t", "int8_t",
                      "uint64_t", "uint32_t", "uint16_t", "uint8_t",
                      "double", "float", "bool", "GorgetString"];
    for prim in &primitives {
        if s.starts_with(prim) && s[prim.len()..].starts_with("__") {
            return Some(prim.len());
        }
    }
    // Try splitting at each __ position
    let mut i = 0;
    while let Some(pos) = s[i..].find("__") {
        let abs_pos = i + pos;
        if abs_pos > 0 && abs_pos + 2 < s.len() {
            return Some(abs_pos);
        }
        i = abs_pos + 2;
    }
    None
}


/// Lower `for var in start..end: body` or `for var in start..=end: body`.
fn lower_for_range(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    start: &Spanned<Expr>,
    end: &Spanned<Expr>,
    inclusive: bool,
    body: &Block,
    else_arm: Option<&Block>,
) {
    // Create loop variable — type inferred from the start expression.
    // For literal bounds (e.g. `0..n`) this gives I64_TYPE; for typed variables
    // (e.g. `start..end` where start: uint8) it preserves the narrower type.
    let saved_range = ctx.save_locals(builder);
    let start_val = lower_expr(ctx, builder, start);
    let loop_type = infer_operand_type_full(ctx, &start_val, builder);
    let loop_var = builder.add_local(loop_type, Some(var_name));
    builder.assign(Place::local(loop_var), start_val);
    ctx.register_local(var_name, loop_var, loop_type);

    let blocks = alloc_for_blocks(builder, /*has_incr=*/ true, else_arm.is_some());
    let ForBlocks { header_bb, body_bb, incr_bb, exit_bb, break_exit_bb, else_exit_bb } = blocks;
    let incr_bb = incr_bb.unwrap();

    // Jump to header
    builder.jump(header_bb);

    // Header: compare loop var with end
    builder.switch_to(header_bb);
    let end_val = lower_expr(ctx, builder, end);
    let cmp_op = if inclusive { CmpOp::Le } else { CmpOp::Lt };
    let cond = builder.cmp(cmp_op, loop_type, FunctionBuilder::copy(loop_var), end_val);
    builder.branch(FunctionBuilder::copy(cond), body_bb, else_exit_bb);

    // Body (wrapped in Loop scope for drop cleanup)
    // Continue target is incr_bb (not header_bb) so the loop variable gets incremented
    builder.switch_to(body_bb);
    ctx.push_loop(incr_bb, break_exit_bb, builder.locals.len() as u32);
    ctx.drops.push_scope(DropScopeKind::Loop);
    lower_block(ctx, builder, body);
    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    builder.jump(incr_bb);

    // Increment: loop_var = loop_var + 1
    builder.switch_to(incr_bb);
    let one = Operand::Constant(Constant::I64(1));
    let incremented = builder.bin_op(BinOp::Add, loop_type, FunctionBuilder::copy(loop_var), one);
    builder.assign(Place::local(loop_var), FunctionBuilder::copy(incremented));
    builder.jump(header_bb);

    ctx.restore_locals(builder, saved_range);

    emit_else_arm_tail(ctx, builder, else_arm, &blocks);
    builder.switch_to(exit_bb);
}
