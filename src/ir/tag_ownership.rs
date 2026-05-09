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
//! See `docs/internals/structural-guards.md` Tier 2a for the framing.

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
                Instruction::EnumFieldLoad { dst, base, variant, field } => {
                    // Tag the extracted slot as Owned when the base is
                    // move-zeroed (whole local OR matching projection)
                    // somewhere later in this block — OR when this exact
                    // (base.local, field) projection is zeroed. The
                    // pattern-extract lowering uses both shapes (whole-
                    // base zero for `enum_field_load_move`, per-field
                    // zero for tuple destructure with FieldLoad).
                    if base_zeroed_in_block(base, &zeroed_locals, &zeroed_field_paths)
                        || enum_field_path_zeroed_in_block(
                            base.local,
                            variant,
                            *field,
                            &zeroed_field_paths,
                        )
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
                    {
                        decisions.push((*dst, LocalOwnership::Owned));
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
