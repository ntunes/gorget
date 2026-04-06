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

    pub(super) fn lower_drop(&mut self, place: &Place, bb: BlockId) {
        use crate::ir::types::DropStrategy;

        let local_idx = place.local.0 as usize;

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
        } else {
            (None, DropStrategy::None)
        };

        match strategy {
            DropStrategy::None => {
                self.lir_func.block_mut(bb).insts.push(Inst::Nop);
            }
            DropStrategy::Trivial(ref fn_name) if fn_name == "free" => {
                let slot = self.local_to_slot[local_idx];
                let slot_ty = self.lir_func.slots[slot.0 as usize].ty.clone();

                // Check if this is a trait-object Box (struct with data+vtable)
                // vs a regular Box (raw pointer). Trait boxes need free(val.data).
                let is_trait_box = type_name.as_deref()
                    .and_then(|n| n.strip_prefix("Box__"))
                    .map(|inner| {
                        self.gir_types.get_type_def(&format!("{inner}_TraitObj")).is_some()
                    })
                    .unwrap_or(false);

                if is_trait_box {
                    // Trait box: free the .data field
                    let addr = self.lower_place_addr(place, bb);
                    // Find the struct_id for this Box type
                    if let Some(sid) = self.struct_reg.lookup(type_name.as_deref().unwrap_or("")) {
                        let data_ptr = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                            dst: data_ptr,
                            base: addr,
                            struct_id: sid,
                            field: 0, // data is field 0
                        });
                        let data_val = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::Load {
                            dst: data_val,
                            ptr: data_ptr,
                            ty: LirType::Ptr,
                        });
                        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                            dst: None,
                            name: "free".to_string(),
                            args: vec![data_val],
                            original_name: None,
                        });
                    } else {
                        // Fallback: just free the whole value
                        let val = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::SlotLoad {
                            dst: val, slot, ty: slot_ty,
                        });
                        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                            dst: None,
                            name: "free".to_string(),
                            args: vec![val],
                            original_name: None,
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
                        let inner_drop_fn = match &inner_drop {
                            DS::Custom(fn_name) => Some(fn_name.clone()),
                            DS::Trivial(fn_name) if fn_name != "free" => Some(fn_name.clone()),
                            _ => None,
                        };
                        if let Some(drop_fn) = inner_drop_fn {
                            // Call inner drop: drop_fn(box_ptr)
                            // box_ptr IS the pointer to the inner value (Box is just a pointer).
                            // Load as Ptr (not the struct type) so the C backend passes by
                            // value instead of by reference — Box is typedef'd as void*.
                            let box_val = self.lir_func.next_value();
                            self.lir_func.block_mut(bb).insts.push(Inst::SlotLoad {
                                dst: box_val, slot, ty: LirType::Ptr,
                            });
                            if let Some(&fid) = self.func_index.get(drop_fn.as_str()) {
                                self.lir_func.block_mut(bb).insts.push(Inst::Call {
                                    dst: None, func: fid, args: vec![box_val],
                                });
                            } else {
                                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                                    dst: None, name: drop_fn, args: vec![box_val],
                                    original_name: None,
                                });
                            }
                        }
                    }
                    // Then free the allocation
                    let val = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::SlotLoad {
                        dst: val, slot, ty: slot_ty,
                    });
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: None,
                        name: "free".to_string(),
                        args: vec![val],
                        original_name: None,
                    });
                }
            }
            DropStrategy::Trivial(ref fn_name) => {
                // Trivial drop: single free/cleanup call. fn_name(&place)
                let addr = self.lower_place_addr(place, bb);

                // Self-cleaning: collections drop elements via elem_drop/val_drop.

                if let Some(&fid) = self.func_index.get(fn_name.as_str()) {
                    self.lir_func.block_mut(bb).insts.push(Inst::Call {
                        dst: None, func: fid, args: vec![addr],
                    });
                } else {
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: None, name: fn_name.clone(), args: vec![addr],
                        original_name: None,
                    });
                }
            }
            DropStrategy::Custom(ref fn_name) => {
                // Custom drop: call user drop, then drop fields.
                // Guard with memcmp zero check — if the struct was moved (zeroed),
                // skip the drop entirely.
                let addr = self.lower_place_addr(place, bb);
                let byte_size = self.compute_place_byte_size(place);
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: format!("__gorget_drop_if_alive_open__{byte_size}"),
                    args: vec![addr],
                    original_name: None,
                });
                // Use unified __gorget_dtor_Type which calls user fn + field drops.
                let unified_drop_fn = type_name.as_ref()
                    .and_then(|tn| self.type_drop_fns.get(tn.as_str()))
                    .map(|info| info.drop_fn_name.clone());
                if let Some(drop_fn) = unified_drop_fn {
                    let addr2 = self.lower_place_addr(place, bb);
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: None, name: drop_fn, args: vec![addr2],
                        original_name: None,
                    });
                } else {
                    // Fallback: call user fn + inline field drops
                    let addr2 = self.lower_place_addr(place, bb);
                    if let Some(&fid) = self.func_index.get(fn_name.as_str()) {
                        self.lir_func.block_mut(bb).insts.push(Inst::Call {
                            dst: None, func: fid, args: vec![addr2],
                        });
                    } else {
                        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                            dst: None, name: fn_name.clone(), args: vec![addr2],
                            original_name: None,
                        });
                    }
                    self.lower_field_drops(place, &type_name, bb);
                }
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: "__gorget_drop_if_alive_close".to_string(),
                    args: vec![],
                    original_name: None,
                });
            }
            DropStrategy::Recursive => {
                // Guard with memcmp zero check for recursive drops too.
                let addr = self.lower_place_addr(place, bb);
                let byte_size = self.compute_place_byte_size(place);
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: format!("__gorget_drop_if_alive_open__{byte_size}"),
                    args: vec![addr],
                    original_name: None,
                });
                // Use unified Type__drop from type_drop_fns when available.
                let unified_drop_fn = type_name.as_ref()
                    .and_then(|tn| self.type_drop_fns.get(tn.as_str()))
                    .map(|info| info.drop_fn_name.clone());
                if let Some(drop_fn) = unified_drop_fn {
                    let addr2 = self.lower_place_addr(place, bb);
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: None, name: drop_fn, args: vec![addr2],
                        original_name: None,
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
                        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                            dst: None, name: drop_fn, args: vec![addr2],
                            original_name: None,
                        });
                    } else {
                        self.lower_field_drops(place, &type_name, bb);
                    }
                }
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: "__gorget_drop_if_alive_close".to_string(),
                    args: vec![],
                    original_name: None,
                });
            }
        }
    }

    /// Compute the byte size of a place's type for memcmp zero checks.
    pub(super) fn compute_place_byte_size(&self, place: &Place) -> usize {
        let local_idx = place.local.0 as usize;
        let type_id = self.gir_func.locals[local_idx].type_id;
        let lir_ty = self.map_type(&type_id);
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
                            self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                                dst: field_ptr,
                                base: base_addr,
                                struct_id,
                                field: field_idx as u32,
                            });
                            if let Some(&fid) = self.func_index.get(drop_fn_name.as_str()) {
                                self.lir_func.block_mut(bb).insts.push(Inst::Call {
                                    dst: None, func: fid, args: vec![field_ptr],
                                });
                            } else {
                                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                                    dst: None, name: drop_fn_name, args: vec![field_ptr],
                                    original_name: None,
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
                                        self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
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
                                                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                                                    dst: sub_ptr,
                                                    base: field_ptr,
                                                    struct_id: sub_struct_id,
                                                    field: sub_idx as u32,
                                                });
                                                if let Some(&fid) = self.func_index.get(sub_fn.as_str()) {
                                                    self.lir_func.block_mut(bb).insts.push(Inst::Call {
                                                        dst: None, func: fid, args: vec![sub_ptr],
                                                    });
                                                } else {
                                                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                                                        dst: None, name: sub_fn, args: vec![sub_ptr],
                                                        original_name: None,
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

    /// Infer drop strategy for a type, using name-based fallback for collection types.
    pub(super) fn infer_drop_strategy(&self, type_name: &str) -> crate::ir::types::DropStrategy {
        use crate::ir::types::DropStrategy;
        if let Some(td) = self.gir_types.get_type_def(type_name) {
            return td.metadata.drop_strategy.clone();
        }
        if type_name.starts_with("Vector__") || type_name.starts_with("Deque__") {
            return DropStrategy::Trivial("gorget_array_free".to_string());
        }
        if type_name.starts_with("Dict__") || type_name.starts_with("HashMap__") {
            return DropStrategy::Trivial("gorget_map_free".to_string());
        }
        if type_name.starts_with("Set__") || type_name.starts_with("HashSet__") {
            return DropStrategy::Trivial("gorget_set_free".to_string());
        }
        if type_name.starts_with("Box__") {
            return DropStrategy::Trivial("free".to_string());
        }
        DropStrategy::None
    }

}
