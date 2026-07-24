//! Drop lowering for GIR → LIR.

use super::*;

impl<'a> FuncLowering<'a> {
    /// Elaborate a GIR Drop/DropIfAlive into LIR call sequences.
    /// Walk GIR-level projections to resolve the TypeId at the end of a Place.
    /// For `bot.nav.path` (Place { local: bot, projections: [Deref, Field(nav), Field(path)] }),
    /// returns the TypeId of the `path` field, not the type of `bot`.
    pub(super) fn resolve_gir_place_type(&self, place: &Place) -> GirTypeId {
        let mut current = self.gir_func.locals[place.local.0 as usize].type_id;
        for proj in &place.projections {
            match proj {
                Projection::Field(idx) => {
                    let mut resolved = false;
                    if let Some(GirType::Named(name)) = self.gir_types.get(current) {
                        if let Some(def) = self.gir_types.get_type_def(&name) {
                            match &def.kind {
                                gir_types::TypeDefKind::Struct(sdef) => {
                                    if let Some(f) = sdef.fields.get(*idx as usize) {
                                        current = f.type_id;
                                        resolved = true;
                                    }
                                }
                                gir_types::TypeDefKind::Enum(edef) => {
                                    // Enum fields are flattened: tag(0), Variant0_0(1), ...
                                    let mut field_offset = 1u32;
                                    'outer: for variant in &edef.variants {
                                        for f in &variant.fields {
                                            if field_offset == *idx {
                                                current = f.type_id;
                                                resolved = true;
                                                break 'outer;
                                            }
                                            field_offset += 1;
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    if !resolved {
                        return current; // can't resolve further
                    }
                }
                Projection::Deref => {
                    current = self.resolve_deref_gir_type_id(current);
                }
                _ => {}
            }
        }
        current
    }

    pub(super) fn lower_drop(&mut self, place: &Place, bb: BlockId, conditional: bool) {
        use crate::ir::types::DropStrategy;

        let local_idx = place.local.0 as usize;

        // `!`-sigil resource params: the slot holds a `MutPtr` to the
        // caller-supplied value, but the callee owns the pointee. Drop must
        // dereference through the pointer; the GIR drop accountant emits a
        // `Place { local, projections: [Deref] }` to opt into this path.
        // The guard checks the slot's pointer bits (8 bytes — null after a
        // `MoveZero`/`MoveSlot` upstream) so val_to_slot maps the guard
        // value back to the slot for `drop_elab.rs` to replace with a bool
        // drop flag. The drop call receives the loaded pointer.
        let is_owning_param = self.gir_func.locals.get(local_idx)
            .map_or(false, |l| l.is_owning_param);
        if is_owning_param
            && place.projections.first() == Some(&Projection::Deref)
            && place.projections.len() == 1
        {
            self.lower_owning_param_drop(place, bb, conditional);
            return;
        }

        // Skip drops for pure-borrow locals — they're borrows with no
        // chance of materialization (self-rooted Param/Alias placeholders,
        // Field-projected borrows, CowBorrowPending). The owner drops the
        // data; this slot has no claim. Other Borrowed/View/MaybeOwned
        // states fall through to the conditional-drop path below.
        if place.projections.is_empty() {
            if let Some(local) = self.gir_func.locals.get(local_idx) {
                if local.ownership.is_pure_borrow_for(place.local) {
                    self.push_inst(bb, Inst::Nop);
                    return;
                }
            }
        }

        // Resolve the actual type at the end of the projection chain.
        // For `bot.nav.path` this gives GorgetArray, not Ptr(BotState).
        let type_id = self.resolve_gir_place_type(place);

        // Look up the type name and drop strategy from the type registry.
        let (type_name, strategy) = if let Some(GirType::Named(name)) = self.gir_types.get(type_id) {
            let strat = if let Some(type_def) = self.gir_types.get_type_def(name) {
                type_def.metadata.drop_strategy.clone()
            } else {
                // Fallback: infer drop strategy from name for collection types
                // that may be registered without a TypeDef (e.g., cross-module imports).
                self.infer_drop_strategy(name)
            };
            (Some(name.clone()), strat)
        } else if matches!(self.gir_types.get(type_id), Some(GirType::FnPtr { .. })) {
            // Bare `Callable[Sig]` locals lower to `GirType::FnPtr` rather than
            // a named type. The runtime layout is still a `GorgetClosure` with
            // a heap-alloc'd env, so the drop must call `gorget_closure_free`
            // to release the env (otherwise `f = fns.get(i).unwrap().clone()`
            // leaks one env per loop iteration).
            (Some("GorgetClosure".to_string()),
             DropStrategy::Trivial("gorget_closure_free".to_string()))
        } else {
            (None, DropStrategy::None)
        };

        // Sanity check: non-None drop strategies should only apply to named types.
        // A scalar or unknown type getting a Trivial/Custom/Recursive drop is a type system bug.
        debug_assert!(
            matches!(strategy, DropStrategy::None) || type_name.is_some(),
            "Non-None drop strategy ({:?}) for unnamed/scalar type (type_id={:?}, local={}). \
             This indicates a type resolution bug in the drop elaboration.",
            strategy, type_id, local_idx,
        );

        match strategy {
            DropStrategy::None => {
                // Fallback for force-registered Option/Result types. Read
                // typed `enum_category` (Phase A) from the GIR TypeDef
                // instead of name-prefix matching.
                let enum_drop_fn = type_name.as_ref()
                    .filter(|tn| self.gir_types.get_type_def(tn).map_or(false, |td|
                        td.metadata.enum_category.is_some())
                        && self.recursive_drop_enums.contains_key(tn.as_str()))
                    .map(|tn| format!("{tn}__drop"));
                if let Some(drop_fn) = enum_drop_fn {
                    let addr = self.lower_place_addr(place, bb);
                    if conditional {
                        let byte_size = self.compute_place_byte_size(place);
                        self.push_inst(bb, Inst::DropGuardOpen {
                            kind: DropGuardKind::NonZero { size: byte_size as u32 },
                            value: addr,
                        });
                    }
                    let addr2 = self.lower_place_addr(place, bb);
                    self.push_inst(bb, Inst::CallExtern {
                        dst: None, name: drop_fn, args: vec![addr2],
                        arg_abis: vec![crate::ir::abi::AbiKind::Opaque],
                    });
                    if conditional {
                        self.push_inst(bb, Inst::DropGuardClose);
                    }
                } else {
                    self.push_inst(bb, Inst::Nop);
                }
            }
            DropStrategy::Trivial(ref fn_name) if fn_name == "free" => {
                // Wrap with guard when conditional (value may have been moved).
                if conditional {
                    let guard_addr = self.lower_place_addr(place, bb);
                    let byte_size = self.compute_place_byte_size(place);
                    self.push_inst(bb, Inst::DropGuardOpen {
                        kind: DropGuardKind::NonZero { size: byte_size as u32 },
                        value: guard_addr,
                    });
                }
                let slot = self.local_to_slot[local_idx];

                // Check if this is a trait-object Box (struct with data+vtable)
                // vs a regular Box (raw pointer). Trait boxes drop through the
                // vtable's `__drop` slot (the concrete type's
                // `Box__<Concrete>__drop` wrapper: payload drop + tracked free
                // + slot null). Read from the typed LIR flag set at
                // registration time — no GIR registry probe needed here.
                let is_trait_box = type_name.as_deref()
                    .and_then(|n| self.struct_reg.lookup(n))
                    .and_then(|sid| self.module_structs.get(sid.0 as usize))
                    .map(|sd| sd.is_trait_box)
                    .unwrap_or(false);

                if is_trait_box {
                    let addr = self.lower_place_addr(place, bb);
                    // The vtable struct + its `__drop` slot index. The trait
                    // name is the Box mangling's inner (`Box__<Trait>`), same
                    // contract `try_trait_object_construct` reads.
                    let vtable_drop = type_name
                        .as_deref()
                        .and_then(|n| n.strip_prefix("Box__"))
                        .and_then(|trait_name| {
                            let vt_name = format!("{trait_name}_VTable");
                            let vt_sid = self.struct_reg.lookup(&vt_name)?;
                            let drop_idx = self
                                .module_structs
                                .get(vt_sid.0 as usize)?
                                .fields
                                .iter()
                                .position(|(fname, _)| fname == crate::ir::lowering::traits::VTABLE_DROP_FIELD)?;
                            Some((vt_sid, drop_idx as u32))
                        });
                    // Find the struct_id for this Box type. Both lookups are
                    // guaranteed by construction: `is_trait_box` was READ off
                    // the registered struct (so the box lookup can't miss),
                    // and `register_trait_types` appends the `__drop` slot to
                    // every `_VTable` unconditionally. A miss here is a
                    // compiler invariant violation — fail loudly (Core #6
                    // structural guard) instead of silently emitting the old
                    // leak-y bare `free(data)` path, which would drop the
                    // concrete payload's resources on the floor.
                    let (Some(sid), Some((vt_sid, drop_idx))) = (
                        self.struct_reg.lookup(type_name.as_deref().unwrap_or("")),
                        vtable_drop,
                    ) else {
                        panic!(
                            "ICE: trait-box drop for `{}` could not resolve the vtable \
                             `__drop` glue slot (box struct or `<Trait>_VTable`/`__drop` \
                             field missing) — register_trait_types must append the drop \
                             slot to every vtable",
                            type_name.as_deref().unwrap_or("<unknown>"),
                        );
                    };
                    {
                        // vtable ptr (field 1) → __drop slot → CallPtr(&data).
                        // `Box__<Concrete>__drop(void** slot)` takes the ADDRESS
                        // of the data pointer (drops payload, frees box, nulls
                        // *slot — idempotent with the surrounding drop guard).
                        let vt_ptr_addr = self.lir_func.next_value();
                        self.push_inst(bb, Inst::FieldPtr {
                            dst: vt_ptr_addr,
                            base: addr,
                            struct_id: sid,
                            field: 1, // vtable is field 1
                        });
                        let vt_ptr = self.lir_func.next_value();
                        self.push_inst(bb, Inst::Load {
                            dst: vt_ptr,
                            ptr: vt_ptr_addr,
                            ty: LirType::Ptr,
                        });
                        let drop_fn_addr = self.lir_func.next_value();
                        self.push_inst(bb, Inst::FieldPtr {
                            dst: drop_fn_addr,
                            base: vt_ptr,
                            struct_id: vt_sid,
                            field: drop_idx,
                        });
                        let drop_fn = self.lir_func.next_value();
                        self.push_inst(bb, Inst::Load {
                            dst: drop_fn,
                            ptr: drop_fn_addr,
                            ty: LirType::Ptr,
                        });
                        let data_ptr = self.lir_func.next_value();
                        self.push_inst(bb, Inst::FieldPtr {
                            dst: data_ptr,
                            base: addr,
                            struct_id: sid,
                            field: 0, // data is field 0
                        });
                        self.push_inst(bb, Inst::CallPtr {
                            dst: None,
                            callee: drop_fn,
                            args: vec![data_ptr],
                            ret_ty: LirType::Void,
                        });
                    }
                } else {
                    // Regular Box: check if inner type has a custom drop, call it first.
                    // Box__Tracked → inner = "Tracked", look up Tracked's drop strategy.
                    let inner_name = type_name.as_deref()
                        .and_then(|n| n.strip_prefix("Box__"));
                    if let Some(inner) = inner_name {
                        use crate::ir::types::DropStrategy as DS;
                        let inner_drop = self.gir_types.get_type_def(inner)
                            .map(|td| td.metadata.drop_strategy.clone())
                            .unwrap_or(DS::None);
                        // Resolve the payload's drop fn. This MUST agree with
                        // `box_inner_drop_fn` (src/backend/c_lir/emit_types.rs),
                        // which the `Box__<inner>__drop` wrapper body uses — the
                        // two compute the same fact (how a Box's payload drops)
                        // for the scope-exit local vs the wrapped (Vector/Option/
                        // field) contexts, so they must not diverge. EXHAUSTIVE
                        // over DropStrategy on purpose: the old `_ => None`
                        // wildcard silently dropped `Recursive` (struct/enum with
                        // a resource field) and the nested `Box[Box[R]]`
                        // `Trivial("free")` case on the floor, leaking the payload
                        // at scope exit (Core #6 — the wildcard was the leak).
                        let inner_drop_fn = match &inner_drop {
                            DS::None => None,
                            // String / collection / opaque-handle payloads: their
                            // own single free fn (e.g. `gorget_string_free`).
                            DS::Trivial(fn_name) if fn_name != "free" => Some(fn_name.clone()),
                            // `Trivial("free")` recurses no further here. A
                            // nested `Box[Box[R]]` (inner Box is `Trivial("free")`
                            // but still owns a payload) SHOULD route through its
                            // inner wrapper, but doing so trips a separate,
                            // pre-existing C-emit name collision — `helpers.rs`
                            // emits `static inline Box__R__drop(Box__R self)`
                            // (bare free) which clashes with the recursive
                            // `void Box__R__drop(void* slot)` wrapper the moment
                            // both are referenced. That leak + collision is
                            // tracked in TODO.md; keep pre-fix behavior (None)
                            // until the collision is resolved so we don't emit
                            // uncompilable C.
                            DS::Trivial(_) => None,
                            // struct/enum with droppable fields: the same unified
                            // `<inner>__drop` a plain local of this type and the
                            // Box wrapper use (mirrors `box_inner_drop_fn`).
                            DS::Recursive => Some(
                                if self.recursive_drop_structs.contains_key(inner)
                                    || self.recursive_drop_enums.contains_key(inner)
                                {
                                    format!("{inner}__drop")
                                } else if let Some(info) = self.type_drop_fns.get(inner) {
                                    info.drop_fn_name.clone()
                                } else {
                                    format!("{inner}__drop")
                                },
                            ),
                            // User `Drop` impl. NOTE: `fn_name` is the USER fn
                            // only (`<inner>__drop`), which skips field drops for
                            // a user-drop struct that ALSO owns resource fields —
                            // a pre-existing leak shared with `box_inner_drop_fn`,
                            // tracked in TODO.md (needs the wrapper-side fix too).
                            DS::Custom(fn_name) => Some(fn_name.clone()),
                        };
                        if let Some(drop_fn) = inner_drop_fn {
                            // Call inner drop: drop_fn(box_ptr)
                            // box_ptr IS the pointer to the inner value (Box is just a pointer).
                            // Load as Ptr (not the struct type) so the C backend passes by
                            // value instead of by reference — Box is typedef'd as void*.
                            let box_val = self.lir_func.next_value();
                            self.push_inst(bb, Inst::SlotLoad {
                                dst: box_val, slot, ty: LirType::Ptr,
                            });
                            if let Some(&fid) = self.func_index.get(drop_fn.as_str()) {
                                self.push_inst(bb, Inst::Call {
                                    dst: None, func: fid, args: vec![box_val],
                                });
                            } else {
                                self.push_inst(bb, Inst::CallExtern {
                                    dst: None, name: drop_fn, args: vec![box_val],
                                    arg_abis: vec![crate::ir::abi::AbiKind::Opaque],
                                });
                            }
                        }
                    }
                    // Then free the allocation through the per-type
                    // `__gorget_box_free_<inner>` helper so the tracking
                    // allocator sees the dealloc. Raw `free(p)` would
                    // unbalance `total_allocs` vs `total_frees` and trip
                    // `--clone-stats` leak-counter false positives. The
                    // helper itself is emitted in
                    // `src/backend/c_lir/emit_types.rs:emit_runtime_helpers`
                    // alongside the matching `__gorget_box_alloc_<inner>`.
                    let val = self.lir_func.next_value();
                    self.push_inst(bb, Inst::SlotLoad {
                        dst: val, slot, ty: LirType::Ptr,
                    });
                    let free_fn = inner_name
                        .map(|inner| format!("__gorget_box_free_{inner}"))
                        .unwrap_or_else(|| "free".to_string());
                    self.push_inst(bb, Inst::CallExtern {
                        dst: None,
                        name: free_fn,
                        args: vec![val],
                        arg_abis: vec![crate::ir::abi::AbiKind::Opaque],
                    });
                }
                if conditional {
                    self.push_inst(bb, Inst::DropGuardClose);
                }
            }
            DropStrategy::Trivial(ref fn_name) => {
                // Trivial drop: single free/cleanup call. fn_name(&place)
                // Wrap with guard when conditional (value may have been moved).
                let addr = self.lower_place_addr(place, bb);
                if conditional {
                    let byte_size = self.compute_place_byte_size(place);
                    self.push_inst(bb, Inst::DropGuardOpen {
                        kind: DropGuardKind::NonZero { size: byte_size as u32 },
                        value: addr,
                    });
                }
                let drop_addr = if conditional { self.lower_place_addr(place, bb) } else { addr };
                if let Some(&fid) = self.func_index.get(fn_name.as_str()) {
                    self.push_inst(bb, Inst::Call {
                        dst: None, func: fid, args: vec![drop_addr],
                    });
                } else {
                    self.push_inst(bb, Inst::CallExtern {
                        dst: None, name: fn_name.clone(), args: vec![drop_addr],
                        arg_abis: vec![crate::ir::abi::AbiKind::Opaque],
                    });
                }
                if conditional {
                    self.push_inst(bb, Inst::DropGuardClose);
                }
            }
            DropStrategy::Custom(ref fn_name) => {
                // Custom drop: call user drop, then drop fields.
                // Always guarded — the elaboration pass resolves statically.
                let addr = self.lower_place_addr(place, bb);
                {
                    let byte_size = self.compute_place_byte_size(place);
                    self.push_inst(bb, Inst::DropGuardOpen {
                        kind: DropGuardKind::NonZero { size: byte_size as u32 },
                        value: addr,
                    });
                }
                // Use unified __gorget_dtor_Type which calls user fn + field drops.
                let unified_drop_fn = type_name.as_ref()
                    .and_then(|tn| self.type_drop_fns.get(tn.as_str()))
                    .map(|info| info.drop_fn_name.clone());
                if let Some(drop_fn) = unified_drop_fn {
                    let addr2 = self.lower_place_addr(place, bb);
                    self.push_inst(bb, Inst::CallExtern {
                        dst: None, name: drop_fn, args: vec![addr2],
                        arg_abis: vec![crate::ir::abi::AbiKind::Opaque],
                    });
                } else {
                    // Fallback: call user fn + inline field drops
                    let addr2 = self.lower_place_addr(place, bb);
                    if let Some(&fid) = self.func_index.get(fn_name.as_str()) {
                        self.push_inst(bb, Inst::Call {
                            dst: None, func: fid, args: vec![addr2],
                        });
                    } else {
                        self.push_inst(bb, Inst::CallExtern {
                            dst: None, name: fn_name.clone(), args: vec![addr2],
                            arg_abis: vec![crate::ir::abi::AbiKind::Opaque],
                        });
                    }
                    self.lower_field_drops(place, &type_name, bb);
                }
                {
                    self.push_inst(bb, Inst::DropGuardClose);
                }
            }
            DropStrategy::Recursive => {
                // Always guarded — the elaboration pass resolves statically.
                let addr = self.lower_place_addr(place, bb);
                {
                    let byte_size = self.compute_place_byte_size(place);
                    self.push_inst(bb, Inst::DropGuardOpen {
                        kind: DropGuardKind::NonZero { size: byte_size as u32 },
                        value: addr,
                    });
                }
                // Use unified Type__drop from type_drop_fns when available.
                let unified_drop_fn = type_name.as_ref()
                    .and_then(|tn| self.type_drop_fns.get(tn.as_str()))
                    .map(|info| info.drop_fn_name.clone());
                if let Some(drop_fn) = unified_drop_fn {
                    let addr2 = self.lower_place_addr(place, bb);
                    self.push_inst(bb, Inst::CallExtern {
                        dst: None, name: drop_fn, args: vec![addr2],
                        arg_abis: vec![crate::ir::abi::AbiKind::Opaque],
                    });
                } else {
                    // Fallback: enum dispatch or inline field drops
                    let is_enum_drop = type_name.as_ref()
                        .map(|tn| self.recursive_drop_enums.contains_key(tn.as_str())
                            && !self.recursive_drop_structs.contains_key(tn.as_str()))
                        .unwrap_or(false);
                    if is_enum_drop {
                        let drop_fn = format!("{}__drop", type_name.as_ref().unwrap());
                        let addr2 = self.lower_place_addr(place, bb);
                        self.push_inst(bb, Inst::CallExtern {
                            dst: None, name: drop_fn, args: vec![addr2],
                            arg_abis: vec![crate::ir::abi::AbiKind::Opaque],
                        });
                    } else {
                        self.lower_field_drops(place, &type_name, bb);
                    }
                }
                {
                    self.push_inst(bb, Inst::DropGuardClose);
                }
            }
        }
    }

    /// Emit the drop sequence for an owning `!`-sigil resource parameter.
    ///
    /// The slot holds an 8-byte `MutPtr` to the caller-supplied value. The
    /// callee owns the pointee and must drop it at exit unless the body
    /// transferred ownership onward (which emitted a `MoveZero` →
    /// `Inst::MoveSlot { slot }` upstream, flipping the LIR drop flag).
    ///
    /// Sequence:
    /// 1. `SlotAddr(slot) → guard_addr` — `&slot`. `val_to_slot` maps this
    ///    back to the slot, so `drop_elab.rs::insert_drop_flags` can
    ///    replace the runtime `memcmp` guard with a bool flag check.
    /// 2. `DropGuardOpen { NonZero { size: 8 }, value: guard_addr }` —
    ///    checks the slot's pointer bits (null after a MoveSlot, non-null
    ///    otherwise). Size is sizeof(Ptr), not sizeof(R), so the guard
    ///    inspects the slot's bytes — never reads through a possibly-null
    ///    pointer.
    /// 3. Inside the guard: `SlotLoad(slot, ty=Ptr) → drop_arg`, then call
    ///    the drop fn with `drop_arg` (the pointer to the underlying R).
    /// 4. `DropGuardClose`.
    fn lower_owning_param_drop(&mut self, place: &Place, bb: BlockId, conditional: bool) {
        use crate::ir::types::DropStrategy;
        let local_idx = place.local.0 as usize;
        let slot = self.local_to_slot[local_idx];

        // Resolve the pointee type (the Deref projection's target).
        let type_id = self.resolve_gir_place_type(place);
        let (type_name, strategy) = if let Some(GirType::Named(name)) = self.gir_types.get(type_id) {
            let strat = if let Some(type_def) = self.gir_types.get_type_def(name) {
                type_def.metadata.drop_strategy.clone()
            } else {
                self.infer_drop_strategy(name)
            };
            (Some(name.clone()), strat)
        } else {
            (None, DropStrategy::None)
        };

        // No drop strategy registered → nothing to do.
        if matches!(strategy, DropStrategy::None) {
            self.push_inst(bb, Inst::Nop);
            return;
        }

        // Open the guard on the slot's pointer bits (8 bytes).
        if conditional {
            let guard_addr = self.lir_func.next_value();
            self.push_inst(bb, Inst::SlotAddr {
                dst: guard_addr,
                slot,
            });
            self.push_inst(bb, Inst::DropGuardOpen {
                kind: DropGuardKind::NonZero { size: 8 },
                value: guard_addr,
            });
        }

        // Load the pointer value for the drop call.
        let drop_arg = self.lir_func.next_value();
        self.push_inst(bb, Inst::SlotLoad {
            dst: drop_arg,
            slot,
            ty: LirType::Ptr,
        });

        // Dispatch to the drop fn based on the strategy. Mirrors the
        // Custom/Recursive/Trivial paths in `lower_drop`, but always
        // uses the unified `Type__drop` helper when present (which is
        // the case for any user-defined `equip ... with Drop`).
        match strategy {
            DropStrategy::Custom(ref fn_name) | DropStrategy::Trivial(ref fn_name) => {
                let unified_drop_fn = type_name.as_ref()
                    .and_then(|tn| self.type_drop_fns.get(tn.as_str()))
                    .map(|info| info.drop_fn_name.clone());
                let drop_fn = unified_drop_fn.unwrap_or_else(|| fn_name.clone());
                if let Some(&fid) = self.func_index.get(drop_fn.as_str()) {
                    self.push_inst(bb, Inst::Call {
                        dst: None, func: fid, args: vec![drop_arg],
                    });
                } else {
                    self.push_inst(bb, Inst::CallExtern {
                        dst: None, name: drop_fn, args: vec![drop_arg],
                        arg_abis: vec![crate::ir::abi::AbiKind::Opaque],
                    });
                }
            }
            DropStrategy::Recursive => {
                let unified_drop_fn = type_name.as_ref()
                    .and_then(|tn| self.type_drop_fns.get(tn.as_str()))
                    .map(|info| info.drop_fn_name.clone());
                if let Some(drop_fn) = unified_drop_fn {
                    self.push_inst(bb, Inst::CallExtern {
                        dst: None, name: drop_fn, args: vec![drop_arg],
                        arg_abis: vec![crate::ir::abi::AbiKind::Opaque],
                    });
                } else if let Some(ref tn) = type_name {
                    let drop_fn = format!("{tn}__drop");
                    self.push_inst(bb, Inst::CallExtern {
                        dst: None, name: drop_fn, args: vec![drop_arg],
                        arg_abis: vec![crate::ir::abi::AbiKind::Opaque],
                    });
                }
            }
            DropStrategy::None => unreachable!("guarded above"),
        }

        if conditional {
            self.push_inst(bb, Inst::DropGuardClose);
        }
    }

    /// Compute the byte size of a place's type for memcmp zero checks.
    pub(super) fn compute_place_byte_size(&self, place: &Place) -> usize {
        let local_idx = place.local.0 as usize;
        // Walk projections to find the *final* GIR type, so the memcmp
        // guard reads exactly the bytes behind the place's address — not
        // the parent local's full size when the place is a field/elem.
        // Without this, `Inst::DropGuardOpen { kind: NonZero, value: &p->f }`
        // emits `memcmp(&p->f, 0, sizeof(P))` which reads up to
        // sizeof(P) - sizeof(F) bytes past the field's end into adjacent
        // fields. Stays inside the parent's allocation (so not UB), but
        // the "non-zero" check then triggers spurious drops on already-
        // zeroed fields whose neighbours happen to be non-zero.
        let mut current_gir_type = if local_idx < self.gir_func.locals.len() {
            self.gir_func.locals[local_idx].type_id
        } else {
            crate::ir::types::I64_TYPE
        };
        // Mirror lower_place_addr's ref-local short-circuit: a Ptr-typed
        // local without an explicit Deref projection produces the
        // pointee's address (via SlotLoad), so the "current type" we
        // walk projections from is the pointee, not the Ptr.
        // §6.8 Stage 4: was `ownership.is_ref()`.
        let is_ref_local = self.gir_func.locals.get(local_idx)
            .map_or(false, |l| l.slot_kind == crate::ir::SlotKind::BorrowedPtr);
        let has_deref = place.projections.first() == Some(&Projection::Deref);
        if is_ref_local && !has_deref {
            if let Some(GirType::Ptr(inner)) = self.gir_types.get(current_gir_type) {
                current_gir_type = *inner;
            }
        }
        for proj in &place.projections {
            match proj {
                Projection::Field(field) => {
                    current_gir_type = self.resolve_field_gir_type_id(current_gir_type, *field);
                }
                Projection::Deref => {
                    current_gir_type = self.resolve_deref_gir_type_id(current_gir_type);
                }
                Projection::Index(_) => {
                    // Array/Vector element: type resolution not implemented
                    // here. Drops on collection elements go through the
                    // ElemDropAction path, not this size-guard, so this is
                    // unreachable in practice.
                }
            }
        }
        let lir_ty = self.map_type(&current_gir_type);
        match &lir_ty {
            LirType::Struct(_) => c_sizeof_lir_type(&lir_ty, &self.module_structs),
            _ => crate::lir::types::scalar_size(&lir_ty).unwrap_or(8) as usize,
        }
    }

    /// Emit field-by-field drops for a struct value (used by Recursive and Custom strategies).
    pub(super) fn lower_field_drops(&mut self, place: &Place, type_name: &Option<String>, bb: BlockId) {
        use crate::ir::types::DropStrategy;
        if let Some(type_name) = type_name {
            if let Some(type_def) = self.gir_types.get_type_def(type_name) {
                if let crate::ir::types::TypeDefKind::Struct(ref sdef) = type_def.kind {
                    let base_addr = self.lower_place_addr(place, bb);
                    let struct_id = self.struct_reg.lookup(type_name).unwrap_or(StructId(0));
                    for (field_idx, field) in sdef.fields.iter().enumerate() {
                        let field_type_name = match self.gir_types.get(field.type_id) {
                            Some(GirType::Named(n)) => Some(n.clone()),
                            _ => None,
                        };
                        let field_drop = field_type_name.as_ref().map(|n| {
                            self.infer_drop_strategy(n)
                        }).unwrap_or(DropStrategy::None);
                        let drop_fn = match &field_drop {
                            DropStrategy::Trivial(fn_name) | DropStrategy::Custom(fn_name) => Some(fn_name.clone()),
                            DropStrategy::Recursive => {
                                if let Some(ref ftn) = field_type_name {
                                    // Check for naming collision (detected during
                                    // populate_recursive_drop_structs).
                                    if self.drop_collision_types.contains(ftn.as_str()) {
                                        None // Will be handled by inline sub-field drops below
                                    } else {
                                        Some(format!("{ftn}__drop"))
                                    }
                                } else {
                                    None
                                }
                            }
                            DropStrategy::None => None,
                        };
                        if let Some(drop_fn_name) = drop_fn {
                            let field_ptr = self.lir_func.next_value();
                            self.push_inst(bb, Inst::FieldPtr {
                                dst: field_ptr,
                                base: base_addr,
                                struct_id,
                                field: field_idx as u32,
                            });
                            if let Some(&fid) = self.func_index.get(drop_fn_name.as_str()) {
                                self.push_inst(bb, Inst::Call {
                                    dst: None, func: fid, args: vec![field_ptr],
                                });
                            } else {
                                self.push_inst(bb, Inst::CallExtern {
                                    dst: None, name: drop_fn_name, args: vec![field_ptr],
                                    arg_abis: vec![crate::ir::abi::AbiKind::Opaque],
                                });
                            }
                        } else if matches!(&field_drop, DropStrategy::Recursive) {
                            // Naming collision: inline the sub-struct's field drops
                            // instead of calling {Name}__drop.
                            if let Some(ref ftn) = field_type_name {
                                if let Some(sub_def) = self.gir_types.get_type_def(ftn) {
                                    if let crate::ir::types::TypeDefKind::Struct(ref sub_sdef) = sub_def.kind {
                                        let sub_struct_id = self.struct_reg.lookup(ftn).unwrap_or(StructId(0));
                                        let field_ptr = self.lir_func.next_value();
                                        self.push_inst(bb, Inst::FieldPtr {
                                            dst: field_ptr,
                                            base: base_addr,
                                            struct_id,
                                            field: field_idx as u32,
                                        });
                                        for (sub_idx, sub_field) in sub_sdef.fields.iter().enumerate() {
                                            let sub_type_name = match self.gir_types.get(sub_field.type_id) {
                                                Some(GirType::Named(n)) => Some(n.clone()),
                                                _ => None,
                                            };
                                            let sub_drop = sub_type_name.as_ref().map(|n| {
                                                self.infer_drop_strategy(n)
                                            }).unwrap_or(DropStrategy::None);
                                            let sub_drop_fn = match &sub_drop {
                                                DropStrategy::Trivial(fn_name) | DropStrategy::Custom(fn_name) => Some(fn_name.clone()),
                                                DropStrategy::Recursive => sub_type_name.as_ref().map(|n| format!("{n}__drop")),
                                                DropStrategy::None => None,
                                            };
                                            if let Some(sub_fn) = sub_drop_fn {
                                                let sub_ptr = self.lir_func.next_value();
                                                self.push_inst(bb, Inst::FieldPtr {
                                                    dst: sub_ptr,
                                                    base: field_ptr,
                                                    struct_id: sub_struct_id,
                                                    field: sub_idx as u32,
                                                });
                                                if let Some(&fid) = self.func_index.get(sub_fn.as_str()) {
                                                    self.push_inst(bb, Inst::Call {
                                                        dst: None, func: fid, args: vec![sub_ptr],
                                                    });
                                                } else {
                                                    self.push_inst(bb, Inst::CallExtern {
                                                        dst: None, name: sub_fn, args: vec![sub_ptr],
                                                        arg_abis: vec![crate::ir::abi::AbiKind::Opaque],
                                                    });
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Infer drop strategy for a type from its TypeDef metadata.
    ///
    /// All collection types (Vector__/Deque__/Dict__/HashMap__/Set__/HashSet__),
    /// runtime-named singletons (GorgetString/GorgetArray/GorgetMap/GorgetSet),
    /// and the Callable family (Callable__/MutCallable__/ConsumeCallable__/
    /// GorgetClosure — Phase A residual #1) carry `drop_strategy` set at
    /// registration via BuiltinTypeProtocol. See the four registration paths
    /// in `src/ir/lowering/types.rs` and `src/ir/lowering/mod.rs`.
    ///
    /// Fallback path: when a type appears in LIR without a corresponding
    /// GIR TypeDef (cross-module imports, certain monomorphized synthetic
    /// names), look up the matching LIR StructDef and read its
    /// `c_runtime_alias` to recover the drop fn for the Callable family.
    /// Without this fallback, httpserver fixtures (which build Callables via
    /// trait dispatch through paths that bypass `resolve_inner_type`'s
    /// TypeDef registration) double-free closures.
    pub(super) fn infer_drop_strategy(&self, type_name: &str) -> crate::ir::types::DropStrategy {
        use crate::ir::types::DropStrategy;
        if let Some(td) = self.gir_types.get_type_def(type_name) {
            return td.metadata.drop_strategy.clone();
        }
        // Phase A residual #1 fallback: typed read from the LIR StructDef.
        // The struct registry may carry a `c_runtime_alias` ("GorgetClosure")
        // even when the GIR TypeDef wasn't materialized for this name
        // (cross-module imports, certain monomorphized synthetic names).
        if let Some(sid) = self.struct_reg.lookup(type_name) {
            if let Some(sd) = self.module_structs.get(sid.0 as usize) {
                if sd.c_runtime_alias.as_deref() == Some("GorgetClosure") {
                    return DropStrategy::Trivial("gorget_closure_free".to_string());
                }
            }
        }
        DropStrategy::None
    }

}
