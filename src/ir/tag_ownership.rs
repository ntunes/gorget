//! Post-lowering ownership inference pass.
//!
//! Walks every function's IR and tags `LocalOwnership::Owned` /
//! `LocalOwnership::FreshOwned` on locals whose typed shape unambiguously
//! produces a fresh, owned resource — but whose ownership is currently
//! `Untracked` because the lowering didn't tag it at the producer site.
//!
//! This is the structural complement to `LoweringContext::set_owned`'s
//! per-call-site setters: many lowering paths (raw `builder.call` /
//! `builder.call_extern` / `builder.enum_field_load_move` / `HeapAlloc`)
//! emit fresh-owned values without going through `call_tracked` /
//! `call_extern_tracked`. The result is `Untracked` ownership on
//! producers that are obviously fresh.
//!
//! Tier 2a (CoW consume-site discipline) requires the validator to see
//! `(Owned, dead, _)` — not `(Untracked, _, _)` — at every consuming
//! position so the unified rule can decide soundness. Rather than
//! migrate 200+ raw `builder.call*` sites individually (high-risk, low
//! gain), this pass infers ownership from the IR shape itself: the same
//! information `validate_consume_sites::preceded_by_clone` derives at
//! consume time, but written through to the typed `Local.ownership`
//! field at producer time so downstream readers see the canonical state.
//!
//! ## Inferred shapes
//!
//! For each instruction the pass recognises, the destination `dst` is
//! tagged when its type needs drop AND its current ownership is
//! `Untracked` (we never overwrite an explicit tag — that would erase
//! Branch A/C/D's borrow / shared-heap shapes from `lower_var_decl`):
//!
//! * **`HeapAlloc { dst, .. }`** — a fresh heap allocation.
//!   `dst` becomes `FreshOwned`.
//! * **`Call { dst, func, .. }`** where the callee is recognised as a
//!   clone fn or fresh-allocator (typed via `clone_fn_names_set` +
//!   `RuntimeFn::returns_fresh`) → `dst = FreshOwned`.
//! * **`CallExtern { dst, func, .. }`** — same predicate.
//! * **`Call { dst, .. }`** to any internal function whose return type
//!   needs drop AND no aliasing into inputs (i.e., not a borrow-returning
//!   builtin like `.get()`, which lowers to a `Ptr<T>` return and never
//!   needs ownership tagging at this site). The conservative test:
//!   `dst.type` is droppable, AND the result type is a non-Ptr value
//!   type. Function returns are owned by value at the IR level — that's
//!   the contract of `AbiKind::ByValue`. → `dst = Owned`.
//! * **`EnumFieldLoad { dst, base, .. }`** followed by `MoveZero` of the
//!   `base` (anywhere later in the same block) → the extracted slot owns
//!   the heap, the base is dead. → `dst = Owned`.
//! * **`FieldLoad { dst, base, .. }`** with same MoveZero-of-base shape
//!   → `dst = Owned`.
//!
//! ## What this pass does NOT do
//!
//! * Does not change the IR shape — only sets typed ownership metadata.
//! * Does not handle Borrow / View / SharedHeap shapes — those are set
//!   by the lowering at structural sites (CoW alias, field borrow, etc.)
//!   and the pass never overwrites a non-Untracked tag.
//! * Does not introduce drops or registrations — drop tracking is
//!   independent of ownership inference.
//!
//! See `docs/devbook/25-structural-guards.md` Tier 2a for the framing.

use rustc_hash::FxHashSet;

use super::instructions::{Instruction, Place};
use super::types::LocalId;
use super::{Function, LocalOwnership, Module};
use crate::ir::types::TypeRegistry;

/// Run the ownership inference pass over every function in the module.
///
/// Builds the module-wide clone-fn name set once via
/// [`TypeRegistry::clone_fn_names_set`] and the typed
/// `RuntimeFn::returns_fresh` predicate for runtime calls. Also reads
/// `module.runtime_callees` so per-monomorphization names like
/// `Dict__GorgetString__GorgetString__new` route to the underlying
/// `gorget_dict_new` runtime fn for the typed `returns_fresh` lookup.
///
/// Tags `Owned` / `FreshOwned` on producer destinations whose ownership
/// is currently `Untracked`.
///
/// Idempotent: running twice produces the same result.
pub fn infer_fresh_owned(module: &mut Module) {
    let clone_fns = module.type_registry.clone_fn_names_set();
    let runtime_callees = module.runtime_callees.clone();
    // Snapshot the registry as `&` so we can mutate `module.functions`
    // without borrow conflicts.
    let registry_ptr: *const TypeRegistry = &module.type_registry;
    for func in &mut module.functions {
        // SAFETY: registry_ptr lives for the duration of the loop;
        // `func` borrows from `module.functions`, but `module.type_registry`
        // is a separate field so the borrows don't alias. We use an
        // explicit raw pointer here only because the borrow checker can't
        // reason about disjoint fields through `&mut module`.
        let registry = unsafe { &*registry_ptr };
        infer_func(func, registry, &clone_fns, &runtime_callees);
    }
}

fn infer_func(
    func: &mut Function,
    registry: &TypeRegistry,
    clone_fns: &FxHashSet<String>,
    runtime_callees: &rustc_hash::FxHashMap<String, crate::ir::RuntimeCalleeInfo>,
) {
    // Two-phase per function:
    //   Phase 1 (immutable walk): collect ownership decisions as
    //           `(LocalId, LocalOwnership)` pairs. Per-block MoveZero
    //           sets give us the "field load followed by zero" pattern.
    //   Phase 2 (mutable apply): write the decisions into
    //           `func.locals[idx].ownership`, respecting the
    //           Untracked-only invariant.
    //
    // The MoveZero collection is per-block because the IR's move-zero is
    // emitted in the same basic block as the consuming load (it's a
    // local invariant, not flow-sensitive).
    let mut decisions: Vec<(LocalId, LocalOwnership)> = Vec::new();
    for bb in &func.blocks {
        // Collect: every MoveZero in this block. `zeroed_locals` is the
        // fast set for bare-local MoveZero (whole-local zero); we also
        // keep the full Vec of projection-bearing MoveZero places so we
        // can `Vec::iter().any(...)` for the projection-aware match.
        // Projection isn't Hash so we use a Vec rather than a FxHashSet
        // — block sizes are typically <100 instructions so the linear
        // scan cost is trivial.
        let mut zeroed_locals: FxHashSet<u32> = FxHashSet::default();
        let mut zeroed_field_paths: Vec<Place> = Vec::new();
        for inst in &bb.instructions {
            if let Instruction::MoveZero { place } = inst {
                if place.projections.is_empty() {
                    zeroed_locals.insert(place.local.0);
                } else {
                    zeroed_field_paths.push(place.clone());
                }
            }
        }

        for inst in &bb.instructions {
            match inst {
                Instruction::HeapAlloc { dst, .. }
                | Instruction::HeapAllocArray { dst, .. } => {
                    decisions.push((*dst, LocalOwnership::FreshOwned));
                }
                Instruction::Call { dst: Some(d), func: callee, .. } => {
                    if is_clone_or_fresh_call_name(callee, clone_fns, runtime_callees) {
                        decisions.push((*d, LocalOwnership::FreshOwned));
                    } else if call_result_is_owned(func, *d, registry) {
                        // Internal function returning a droppable resource
                        // by value: the result is owned. Conservative: only
                        // tag when the dst type is droppable AND not a
                        // pointer (Ptr returns are borrow-shaped — they
                        // alias into the callee's input).
                        decisions.push((*d, LocalOwnership::Owned));
                    }
                }
                Instruction::CallExtern { dst: Some(d), func: callee, .. } => {
                    if is_clone_or_fresh_call_name(callee, clone_fns, runtime_callees) {
                        decisions.push((*d, LocalOwnership::FreshOwned));
                    }
                    // Non-fresh extern calls: do not tag. Many extern
                    // returns are views / pointers / non-owned scalars —
                    // tagging them would be unsound. The runtime
                    // `returns_fresh` flag is the typed gate for the
                    // "owned by value" axis.
                }
                Instruction::EnumFieldLoad { dst, base, variant, field, .. } => {
                    // Tag the extracted slot as Owned when the base is
                    // move-zeroed (whole local OR matching projection)
                    // somewhere later in this block — OR when this exact
                    // (base.local, field) projection is zeroed. The
                    // pattern-extract lowering uses both shapes (whole-
                    // base zero for `enum_field_load_move`, per-field
                    // zero for tuple destructure with FieldLoad).
                    //
                    // Tier 2a Phase 3 (residual): also tag when the
                    // EXTRACTED slot itself is move-zeroed later in the
                    // block. This is the `match Error(e): ...` clone-
                    // and-rewrap shape: `_35 = enum_field_load _17,
                    // Error, 0; [Mv] _36 = copy _35; move_zero _35;`.
                    // The extracted `_35` is treated as owning the
                    // extracted variant payload for the brief window
                    // before it's transferred into `_36`. Without this,
                    // `_35` stayed Untracked and the
                    // `[Mv] _36 = copy _35` Assign tripped the
                    // AssignIntoOwnedSlot validator.
                    if base_zeroed_in_block(base, &zeroed_locals, &zeroed_field_paths)
                        || enum_field_path_zeroed_in_block(
                            base.local,
                            variant,
                            *field,
                            &zeroed_field_paths,
                        )
                        || zeroed_locals.contains(&dst.0)
                    {
                        decisions.push((*dst, LocalOwnership::Owned));
                    }
                }
                Instruction::FieldLoad { dst, base, field } => {
                    if base_zeroed_in_block(base, &zeroed_locals, &zeroed_field_paths)
                        || field_path_zeroed_in_block(
                            base.local,
                            *field,
                            &zeroed_field_paths,
                        )
                        || zeroed_locals.contains(&dst.0)
                    {
                        decisions.push((*dst, LocalOwnership::Owned));
                    }
                }
                // Tier 2a Phase 3 (Snag #28 follow-up): constructor
                // instructions produce owned values by definition — the
                // dst is a fresh aggregate built from the field/element
                // operands. Without these decisions, downstream Inst::Assign
                // sites consuming the aggregate (e.g.
                // `Option[T] x = Some(...)`) see Untracked and trip the
                // AssignIntoOwnedSlot validator. The aggregate's owned-
                // ness is structural — not name-matched, not heuristic.
                Instruction::EnumInit { dst, .. }
                | Instruction::StructInit { dst, .. }
                | Instruction::TupleInit { dst, .. } => {
                    decisions.push((*dst, LocalOwnership::Owned));
                }
                // Tier 2a Phase 3: BinOp results on resource types are
                // fresh by construction (e.g. `Vector[T] + Vector[T]`
                // lowers to a concat producing a fresh array). Same for
                // UnOp (unary negate / not — unlikely on resources but
                // safe). `apply_decision` filters non-droppable dsts via
                // its `needs_drop` check, so blanket tagging is sound.
                // This catches the iterator default-method `acc = acc + x`
                // shape in `sum`/`product` over `Vector[T]`-yielding
                // iterators (WindowsIter, ChunksIter, etc).
                Instruction::BinOp { dst, .. }
                | Instruction::UnOp { dst, .. } => {
                    decisions.push((*dst, LocalOwnership::Owned));
                }
                // Tier 2a Phase 3: `IndexLoad` with `ReadMode::Clone`
                // returns a fresh clone of the element (the C runtime
                // dispatches through the element's `__clone` fn). The
                // `Borrow` mode is the zero-copy view path and is NOT
                // tagged here. Catches the `Dict[K, V][k]` shape where
                // the value is read by clone (default for resource Vs).
                Instruction::IndexLoad { dst, read, .. } => {
                    use crate::ir::instructions::ReadMode;
                    if matches!(read, ReadMode::Clone) {
                        decisions.push((*dst, LocalOwnership::Owned));
                    }
                }
                // Gorget-arena snag #1: `expr as T` where T is a droppable
                // resource (e.g. `"x" as String`, `n as String`,
                // `other_string as String`) lowers in the backend to a
                // fresh allocation: `gorget_str_from_cstr` for literal /
                // ptr sources, `gorget_int_to_str`/`gorget_float_to_str`/
                // `gorget_bool_to_str` for scalar sources, and
                // `gorget_string_clone` for same-type String→String.
                // Every branch produces an owned value — but the Cast
                // instruction itself wasn't tagged, so the dst stayed
                // Untracked and the very next `s = s + …` re-assign tripped
                // the AssignIntoOwnedSlot validator. Tag structurally; the
                // `apply_decision` `needs_drop` gate filters out scalar
                // casts (int→float, etc.) that don't allocate.
                Instruction::Cast { dst, .. } => {
                    decisions.push((*dst, LocalOwnership::Owned));
                }
                // Tier 2a Phase 3: `Inst::Assign { dst, value: Constant::Str }`
                // for a resource-typed dst materialises a fresh heap
                // allocation at codegen (`String out = ""` → `_out = const ""`
                // → C runtime allocates a fresh GorgetString). The dst owns
                // its data immediately. Restricted to `Constant::Str` —
                // other Constant variants either don't produce heap data
                // (Bool/I*/U*/F*/Null/Unit/SizeOf/FuncRef/GlobalRef) or
                // would be filtered by `apply_decision`'s `needs_drop` gate
                // anyway; explicit narrowing makes the intent clear.
                // Catches the empty-string-init pattern in iterator
                // `join` default-method bodies and similar fresh-literal
                // var-decls.
                Instruction::Assign { dst, value, .. } if dst.projections.is_empty() => {
                    use crate::ir::instructions::{Constant, Operand, Projection};
                    if let Operand::Constant(Constant::Str(_)) = value {
                        decisions.push((dst.local, LocalOwnership::Owned));
                    } else if let Operand::Copy(p) | Operand::Move(p) = value {
                        // Tier 2a Phase 3: auto-deref consume shape for
                        // `!`-move params. `_dst = copy _ptr.*` followed
                        // by `move_zero _ptr` later in this block means
                        // the pointee was transferred OUT — dst owns the
                        // value. Catches `lex_emit(&self, Token !tok)` →
                        // `_7 = copy _2.*; move_zero _2;` shape.
                        if p.projections.len() == 1
                            && matches!(p.projections[0], Projection::Deref)
                            && zeroed_locals.contains(&p.local.0)
                        {
                            decisions.push((dst.local, LocalOwnership::Owned));
                        }
                        // Tier 2a Phase 3 (residual): bare-local
                        // assign-then-zero is an ownership transfer.
                        // `_dst = copy _src` (or move) followed by
                        // `move_zero _src` later in the same block
                        // structurally moves the heap from src to dst —
                        // this is the same shape `EnumFieldLoad +
                        // MoveZero` handles a few rules above. Catches
                        // the match-arm-result merge: `bb_arm_n: [Mv]
                        // _result = copy _temp; move_zero _temp` where
                        // _temp was filled by a clone or fresh
                        // allocation in this block, then jumps to a
                        // common merge block consuming _result.
                        if p.projections.is_empty()
                            && zeroed_locals.contains(&p.local.0)
                        {
                            decisions.push((dst.local, LocalOwnership::Owned));
                        }
                        // Tier 2a Phase 3 (residual): when the Assign's
                        // dst is itself move-zeroed later in the block,
                        // the lowering structurally treats this slot as
                        // owning. Mirrors the EnumFieldLoad/FieldLoad
                        // self-zero rule. Pattern:
                        //   `_85 = copy _2.*` (auto-deref of borrow
                        //   param)
                        //   `[Mv] _84 = copy _85`
                        //   `move_zero _85`
                        // Without this, `_85` stayed Untracked and the
                        // downstream `[Mv] _84 = copy _85` tripped the
                        // AssignIntoOwnedSlot validator. The dst-zero
                        // is the typed signal of "this slot is the
                        // owning hand-off"; ownership tagging it makes
                        // the validator's view match the lowering's.
                        if zeroed_locals.contains(&dst.local.0) {
                            decisions.push((dst.local, LocalOwnership::Owned));
                        }
                    }
                }
                _ => {}
            }
        }
    }
    for (local, ownership) in decisions {
        apply_decision(func, local, ownership, registry);
    }
}

fn apply_decision(
    func: &mut Function,
    local: LocalId,
    ownership: LocalOwnership,
    registry: &TypeRegistry,
) {
    let idx = local.0 as usize;
    if idx >= func.locals.len() {
        return;
    }
    let l = &mut func.locals[idx];
    if !registry.needs_drop(l.type_id) {
        return;
    }
    if !matches!(l.ownership, LocalOwnership::Untracked) {
        return;
    }
    l.ownership = ownership;
}

/// Tuple-destructure shape: `dst = field_load base.local, FIELD; ...;
/// move_zero base.local.FIELD`. The extracted slot owns the heap
/// because the parent's specific slot was zeroed.
fn field_path_zeroed_in_block(
    base_local: LocalId,
    field: u32,
    zeroed_field_paths: &[Place],
) -> bool {
    use super::instructions::Projection;
    for p in zeroed_field_paths {
        if p.local != base_local || p.projections.len() != 1 {
            continue;
        }
        if matches!(p.projections[0], Projection::Field(f) if f == field) {
            return true;
        }
    }
    false
}

/// EnumFieldLoad equivalent of `field_path_zeroed_in_block`. The IR uses
/// the same `Projection::Field(N)` for enum-variant payloads (the LIR
/// flattens variant.field to a single field index), so the lookup is
/// identical to the tuple/struct case.
fn enum_field_path_zeroed_in_block(
    base_local: LocalId,
    _variant: &str,
    field: u32,
    zeroed_field_paths: &[Place],
) -> bool {
    field_path_zeroed_in_block(base_local, field, zeroed_field_paths)
}

/// Did this `base` Place have a `MoveZero` in the same block?
///
/// Two acceptable shapes:
/// * Bare-local `MoveZero { place: { local, projections: [] } }` covers
///   the entire `base.local` — anything extracted from it is then
///   uniquely owned.
/// * Field-projected `MoveZero { place: { local, projections: [Field(N)] } }`
///   matches the EnumFieldLoad / FieldLoad's exact projection — the
///   extracted slot itself is zeroed.
fn base_zeroed_in_block(
    base: &Place,
    zeroed_locals: &FxHashSet<u32>,
    zeroed_field_paths: &[Place],
) -> bool {
    if zeroed_locals.contains(&base.local.0) {
        return true;
    }
    // Projection-aware match: the EnumFieldLoad's base may itself be a
    // projection (rare but possible — match arms on nested fields). We
    // accept the exact-path match. Linear scan — projection lists are
    // tiny in practice (≤2 entries each).
    if !base.projections.is_empty() {
        for p in zeroed_field_paths {
            if p.local == base.local && p.projections == base.projections {
                return true;
            }
        }
    }
    false
}

/// Read-only predicate: "is this `Call`'s destination an owned-by-value
/// resource result?". Conservative — only true when the dst type is
/// droppable AND not a pointer / mut-pointer (those alias into inputs
/// and are NOT owned at this call site). `apply_decision` re-checks
/// these conditions before writing.
fn call_result_is_owned(
    func: &Function,
    local: LocalId,
    registry: &TypeRegistry,
) -> bool {
    let idx = local.0 as usize;
    if idx >= func.locals.len() {
        return false;
    }
    let ty = func.locals[idx].type_id;
    if !registry.needs_drop(ty) {
        return false;
    }
    if matches!(
        registry.get(ty),
        Some(crate::ir::types::GirType::Ptr(_))
            | Some(crate::ir::types::GirType::MutPtr(_))
    ) {
        return false;
    }
    true
}

/// Cheap wrapper: name-only clone-or-fresh check. Mirrors the validator's
/// `is_clone_or_fresh_call` (both predicates resolve through Phase 2E's
/// typed `RuntimeFn::returns_fresh` table + the module's
/// `clone_fn_names_set`).
///
/// Per-monomorphization callees (e.g. `Dict__GorgetString__GorgetString__new`)
/// route through `runtime_callees` to their underlying runtime symbol
/// (`gorget_dict_new`) before the typed lookup, so collection / Box-alloc
/// constructors get tagged FreshOwned identically to direct
/// `gorget_*_new` calls.
fn is_clone_or_fresh_call_name(
    name: &str,
    clone_fns: &FxHashSet<String>,
    runtime_callees: &rustc_hash::FxHashMap<String, crate::ir::RuntimeCalleeInfo>,
) -> bool {
    // Direct typed lookup: the callee name IS the runtime symbol.
    if let Some(rt) = crate::lir::runtime::RuntimeFn::from_c_name(name) {
        if rt.signature().returns_fresh {
            return true;
        }
    }
    // Per-mono lookup: per-instantiation symbols (`Dict__K__V__new`,
    // `Vector__T__new`, …) map through `runtime_callees` to the runtime
    // family member (`gorget_dict_new`). Rerun the typed predicate on
    // the resolved name.
    if let Some(info) = runtime_callees.get(name) {
        if let Some(rt) = crate::lir::runtime::RuntimeFn::from_c_name(&info.name) {
            if rt.signature().returns_fresh {
                return true;
            }
        }
    }
    clone_fns.contains(name)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::builder::FunctionBuilder;
    use crate::ir::types::{
        CopySemantics, DropStrategy, GirType, StructDef, TypeDef, TypeDefKind, TypeId,
        TypeMetadata,
    };
    use crate::ir::ExternDecl;

    fn add_resource_type(module: &mut Module, name: &str) -> TypeId {
        module.type_registry.add_type_def(TypeDef {
            name: name.into(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                copy_semantics: CopySemantics::Resource,
                drop_strategy: DropStrategy::Trivial(format!("{name}_free")),
                ..Default::default()
            },
        });
        module.type_registry.insert(GirType::Named(name.into()))
    }

    #[test]
    fn heap_alloc_tags_fresh() {
        let mut module = Module::new();
        let ty = add_resource_type(&mut module, "Buf");
        let mut b = FunctionBuilder::new(
            "f",
            crate::ir::types::UNIT_TYPE,
            &[],
        );
        let alloc_local = b.heap_alloc(
            ty,
            crate::ir::instructions::Operand::Constant(
                crate::ir::instructions::Constant::I64(0),
            ),
        );
        b.ret(crate::ir::instructions::Operand::Constant(
            crate::ir::instructions::Constant::Unit,
        ));
        module.functions.push(b.build());

        infer_fresh_owned(&mut module);

        let f = &module.functions[0];
        assert!(matches!(
            f.locals[alloc_local.0 as usize].ownership,
            LocalOwnership::FreshOwned
        ));
    }

    #[test]
    fn untracked_borrow_not_overwritten() {
        // Verify the pass NEVER overwrites a non-Untracked tag.
        let mut module = Module::new();
        let ty = add_resource_type(&mut module, "Buf");
        let mut b = FunctionBuilder::new(
            "f",
            crate::ir::types::UNIT_TYPE,
            &[],
        );
        let alloc_local = b.heap_alloc(
            ty,
            crate::ir::instructions::Operand::Constant(
                crate::ir::instructions::Constant::I64(0),
            ),
        );
        // Manually pre-set ownership to Borrowed — the pass must not
        // clobber this.
        b.locals[alloc_local.0 as usize].ownership = LocalOwnership::Borrowed {
            origin: crate::ir::BorrowOrigin::Param(LocalId(0)),
            mutability: crate::ir::Mutability::Shared,
        };
        b.ret(crate::ir::instructions::Operand::Constant(
            crate::ir::instructions::Constant::Unit,
        ));
        module.functions.push(b.build());

        infer_fresh_owned(&mut module);

        let f = &module.functions[0];
        assert!(matches!(
            f.locals[alloc_local.0 as usize].ownership,
            LocalOwnership::Borrowed { .. }
        ));
    }

    #[test]
    fn primitive_type_not_tagged() {
        let mut module = Module::new();
        let mut b = FunctionBuilder::new(
            "f",
            crate::ir::types::I64_TYPE,
            &[],
        );
        let dst = b.call(
            "fortytwo",
            vec![],
            crate::ir::types::I64_TYPE,
        );
        b.ret(FunctionBuilder::copy(dst));
        module.functions.push(b.build());
        // Add the function to externs to satisfy validator, irrelevant here.
        module.externs.push(ExternDecl {
            name: "fortytwo".into(),
            params: vec![],
            return_type: crate::ir::types::I64_TYPE,
            is_variadic: false,
            param_abis: vec![],
            returns_borrowed: false,
        });

        infer_fresh_owned(&mut module);

        let f = &module.functions[0];
        assert!(matches!(
            f.locals[dst.0 as usize].ownership,
            LocalOwnership::Untracked
        ));
    }

    #[test]
    fn enum_field_load_with_move_zero_tags_owned() {
        let mut module = Module::new();
        let ty = add_resource_type(&mut module, "Payload");
        // Fake enum type for the EnumFieldLoad to reference.
        module.type_registry.add_type_def(TypeDef {
            name: "Wrap".into(),
            kind: TypeDefKind::Enum(crate::ir::types::EnumDef {
                variants: vec![crate::ir::types::EnumVariant {
                    name: "P".into(),
                    fields: vec![crate::ir::types::StructField {
                        name: "0".into(),
                        type_id: ty,
                    }],
                }],
            }),
            metadata: TypeMetadata::default(),
        });
        let wrap_ty = module
            .type_registry
            .insert(GirType::Named("Wrap".into()));
        let mut b = FunctionBuilder::new(
            "f",
            crate::ir::types::UNIT_TYPE,
            &[],
        );
        let scrut = b.add_local(wrap_ty, Some("scrut"));
        // Mark scrut as Owned so MoveZero is sound.
        b.locals[scrut.0 as usize].ownership = LocalOwnership::Owned;
        let extracted = b.enum_field_load_move(Place::local(scrut), "P", 0, ty);
        b.move_zero(Place::local(scrut));
        b.ret(crate::ir::instructions::Operand::Constant(
            crate::ir::instructions::Constant::Unit,
        ));
        module.functions.push(b.build());

        infer_fresh_owned(&mut module);

        let f = &module.functions[0];
        assert!(matches!(
            f.locals[extracted.0 as usize].ownership,
            LocalOwnership::Owned
        ));
    }
}
