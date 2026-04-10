//! Operand and place handling, type resolution, enum helpers for GIR → LIR lowering.

use super::*;

impl<'a> FuncLowering<'a> {
    /// Get the address of a GIR place.
    pub(super) fn lower_place_addr(&mut self, place: &Place, bb: BlockId) -> ValueId {
        let slot = self.local_to_slot[place.local.0 as usize];
        let local_gir_type = if (place.local.0 as usize) < self.gir_func.locals.len() {
            self.gir_func.locals[place.local.0 as usize].type_id
        } else { crate::ir::types::I64_TYPE };
        let mut addr = self.lir_func.next_value();

        // Collection ref locals (Ptr-typed from borrowing reads):
        // - With Deref projection: use SlotAddr — the Deref loads the pointer
        // - Without Deref: use SlotLoad — directly provides the pointer value
        //   (needed for borrows, method calls, indexing on the Ptr variable)
        let is_ref_local = self.gir_func.locals.get(place.local.0 as usize)
            .map_or(false, |l| l.ownership == ir::OwnershipState::Ref);
        // Only treat PtrTo(GorgetString) slots as implicit ref locals.
        // Other PtrTo slots carry type information but are not reference locals.
        let is_ptr_to_slot = match &self.lir_func.slots[slot.0 as usize].ty {
            LirType::PtrTo(sid) => self.struct_reg.lookup("GorgetString") == Some(*sid),
            _ => false,
        };
        let has_deref = place.projections.first() == Some(&Projection::Deref);
        if (is_ref_local || is_ptr_to_slot) && !has_deref {
            self.lir_func
                .block_mut(bb)
                .insts
                .push(Inst::SlotLoad { dst: addr, slot, ty: LirType::Ptr });
        } else {
            self.lir_func
                .block_mut(bb)
                .insts
                .push(Inst::SlotAddr { dst: addr, slot });
        }

        // Track the current GIR type through each projection step.
        // For Ptr ref locals without Deref, resolve to the pointee type.
        let mut current_gir_type = if is_ref_local && !has_deref {
            match self.gir_types.get(local_gir_type) {
                Some(GirType::Ptr(inner)) => *inner,
                _ => local_gir_type,
            }
        } else {
            local_gir_type
        };

        for proj in &place.projections {
            match proj {
                Projection::Field(field) => {
                    let struct_id = self.resolve_struct_id_for_field(current_gir_type, *field, self.module_structs);
                    let next = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: next,
                        base: addr,
                        struct_id,
                        field: *field,
                    });
                    addr = next;
                    // Update type to the field's type for subsequent projections.
                    current_gir_type = self.resolve_field_gir_type_id(current_gir_type, *field);
                }
                Projection::Index(idx_local) => {
                    let idx_slot = self.local_to_slot[idx_local.0 as usize];
                    let idx = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::SlotLoad {
                        dst: idx,
                        slot: idx_slot,
                        ty: LirType::I64,
                    });
                    let next = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::ElemPtr {
                        dst: next,
                        base: addr,
                        index: idx,
                        elem_size: 8,
                    });
                    addr = next;
                }
                Projection::Deref => {
                    // Load the pointer from addr, then use that as the new addr.
                    let ptr_val = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: ptr_val,
                        ptr: addr,
                        ty: LirType::Ptr,
                    });
                    addr = ptr_val;
                    // Update type to the pointee type.
                    current_gir_type = self.resolve_deref_gir_type_id(current_gir_type);
                }
            }
        }

        addr
    }

    /// Lower a GIR constant to a LIR value.
    pub(super) fn lower_constant(&mut self, c: &Constant, bb: BlockId) -> ValueId {
        let dst = self.lir_func.next_value();
        let inst = match c {
            Constant::Bool(v) => Inst::BoolConst { dst, value: *v },
            Constant::I8(v) => Inst::IConst { dst, ty: LirType::I8, value: *v as i64 },
            Constant::I16(v) => Inst::IConst { dst, ty: LirType::I16, value: *v as i64 },
            Constant::I32(v) => Inst::IConst { dst, ty: LirType::I32, value: *v as i64 },
            Constant::I64(v) => Inst::IConst { dst, ty: LirType::I64, value: *v },
            Constant::U8(v) => Inst::IConst { dst, ty: LirType::U8, value: *v as i64 },
            Constant::U16(v) => Inst::IConst { dst, ty: LirType::U16, value: *v as i64 },
            Constant::U32(v) => Inst::IConst { dst, ty: LirType::U32, value: *v as i64 },
            Constant::U64(v) => Inst::IConst { dst, ty: LirType::U64, value: *v as i64 },
            Constant::F32(v) => Inst::FConst { dst, ty: LirType::F32, bits: (*v as f64).to_bits() },
            Constant::F64(v) => Inst::FConst { dst, ty: LirType::F64, bits: v.to_bits() },
            Constant::Str(s) => Inst::StrLit { dst, value: s.clone() },
            Constant::Null => Inst::NullPtr { dst },
            Constant::Unit => Inst::IConst { dst, ty: LirType::I32, value: 0 }, // unit = zero
            Constant::SizeOf(type_id) => {
                let ty = self.map_type(type_id);
                let size = c_sizeof_lir_type(&ty, self.module_structs);
                Inst::IConst { dst, ty: LirType::I64, value: size as i64 }
            }
            Constant::FuncRef(name) => {
                if let Some(fid) = self.func_index.get(name) {
                    Inst::FuncAddr { dst, func: *fid }
                } else {
                    // Unknown function — emit as a string for now.
                    Inst::IConst { dst, ty: LirType::I64, value: 0 }
                }
            }
            Constant::GlobalRef(name) => {
                if let Some(&gid) = self.global_index.get(name) {
                    // Load the global's value: take address, then load.
                    let addr = self.lir_func.next_value();
                    let global_ty = self.module_globals[gid.0 as usize].ty.clone();
                    self.lir_func.block_mut(bb).insts.push(Inst::GlobalAddr { dst: addr, global: gid });
                    Inst::Load { dst, ptr: addr, ty: global_ty }
                } else {
                    Inst::NullPtr { dst }
                }
            }
            Constant::GlobalRefPtr(name) => {
                if let Some(&gid) = self.global_index.get(name) {
                    Inst::GlobalAddr { dst, global: gid }
                } else {
                    Inst::NullPtr { dst }
                }
            }
        };
        self.lir_func.block_mut(bb).insts.push(inst);
        dst
    }

    // ── Store helpers ───────────────────────────────────────────────────────

    /// Derive the LIR type of a GIR operand.
    /// Get the GIR type name for an operand (for type-aware dispatch).
    pub(super) fn operand_gir_type_name(&self, operand: &Operand) -> Option<String> {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => {
                let idx = place.local.0 as usize;
                if idx < self.gir_func.locals.len() {
                    let gir_ty = self.gir_func.locals[idx].type_id;
                    match self.gir_types.get(gir_ty) {
                        Some(GirType::Named(name)) => Some(name.clone()),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub(super) fn operand_lir_type(&self, operand: &Operand) -> LirType {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => {
                let idx = place.local.0 as usize;
                if idx < self.gir_func.locals.len() {
                    let gir_ty = self.gir_func.locals[idx].type_id;
                    self.map_type(&gir_ty)
                } else {
                    LirType::Ptr
                }
            }
            Operand::Constant(c) => match c {
                Constant::I8(_) | Constant::I16(_) | Constant::I32(_) | Constant::I64(_)
                | Constant::U8(_) | Constant::U16(_) | Constant::U32(_) | Constant::U64(_)
                | Constant::SizeOf(_) => LirType::I64,
                Constant::F32(_) | Constant::F64(_) => LirType::F64,
                Constant::Bool(_) => LirType::Bool,
                Constant::Str(_) | Constant::Null | Constant::FuncRef(_) | Constant::GlobalRef(_) | Constant::GlobalRefPtr(_) => LirType::Ptr,
                Constant::Unit => LirType::Void,
            },
        }
    }

    /// Ensure a synthetic extern declaration exists for an unknown function.
    /// If the extern already exists from a previous call site, merge parameter types
    /// by preferring more specific types (e.g., Struct over Ptr).
    pub(super) fn ensure_extern(&mut self, name: &str, arg_types: &[LirType], ret_ty: &LirType) {
        // Look up extern ABI kinds from module declarations.
        // Look up extern ABI by exact name only (no prefix stripping —
        // it causes collisions, e.g. gorget_regex_is_match → regex_is_match
        // which is a different function).
        let abi_tags: Vec<crate::ir::abi::AbiKind> = self.extern_abi_kinds.get(name)
            .cloned()
            .unwrap_or_default();

        let ret_abi: crate::ir::abi::AbiKind = self.return_abi_kinds.get(name)
            .copied()
            .unwrap_or_default();

        // For known runtime functions, use canonical signatures instead of call-site inference.
        if let Some((canon_params, canon_ret)) = runtime_extern_sig(name, self.struct_reg) {
            if let Some(existing) = self.pending_externs.iter_mut().find(|e| e.name == name) {
                existing.params = canon_params;
                existing.return_type = canon_ret;
            } else {
                self.pending_externs.push(LirExtern {
                    name: name.to_string(),
                    params: canon_params,
                    return_type: canon_ret,
                    is_variadic: false,
                    param_abis: abi_tags.clone(),
                    return_abi: ret_abi,
                });
            }
            return;
        }

        // Detect newtype constructors: if the function name matches a struct name,
        // the return type should be that struct (not i64 or i32 from GIR's extern decl).
        let actual_ret = if let Some(sid) = self.struct_reg.lookup(name) {
            LirType::Struct(sid)
        } else {
            ret_ty.clone()
        };

        if let Some(existing) = self.pending_externs.iter_mut().find(|e| e.name == name) {
            // Merge param types: prefer aggregate/specific types over Ptr.
            for (i, new_ty) in arg_types.iter().enumerate() {
                if i < existing.params.len() {
                    if matches!(existing.params[i], LirType::Ptr) && !matches!(new_ty, LirType::Ptr) {
                        existing.params[i] = new_ty.clone();
                    }
                }
            }
            // Also update return type if existing is I64 and new is more specific.
            if matches!(existing.return_type, LirType::I64 | LirType::I32) && !matches!(actual_ret, LirType::I64 | LirType::I32) {
                existing.return_type = actual_ret;
            }
            return;
        }
        self.pending_externs.push(LirExtern {
            name: name.to_string(),
            params: arg_types.to_vec(),
            return_type: actual_ret,
            is_variadic: false,
            param_abis: abi_tags,
            return_abi: ret_abi,
        });
    }

    pub(super) fn store_to_local(&mut self, local: ir::types::LocalId, value: ValueId, bb: BlockId) {
        let slot = self.local_to_slot[local.0 as usize];
        self.lir_func
            .block_mut(bb)
            .insts
            .push(Inst::SlotStore { slot, value, is_move: false });
    }

    pub(super) fn store_to_place(&mut self, place: &Place, value: ValueId, bb: BlockId) {
        if place.projections.is_empty() {
            self.store_to_local(place.local, value, bb);
        } else {
            let addr = self.lower_place_addr(place, bb);
            self.lir_func
                .block_mut(bb)
                .insts
                .push(Inst::Store { ptr: addr, value });
        }
    }

    /// Emit an I64 constant and return its ValueId.
    pub(super) fn emit_i64_const(&mut self, bb: BlockId, value: i64) -> ValueId {
        let val = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::IConst {
            dst: val, ty: LirType::I64, value,
        });
        val
    }

    /// Emit an I32 constant and return its ValueId.
    pub(super) fn emit_i32_const(&mut self, bb: BlockId, value: i64) -> ValueId {
        let val = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::IConst {
            dst: val, ty: LirType::I32, value,
        });
        val
    }

    /// Map a GIR type ID to an LIR type, using the current type registry and struct registry.
    pub(super) fn map_type(&self, type_id: &crate::ir::types::TypeId) -> LirType {
        map_gir_type_with_structs(type_id, self.gir_types, Some(self.struct_reg))
    }

    // ── Type resolution helpers ─────────────────────────────────────────────

    pub(super) fn resolve_struct_id(&self, gir_type_id: GirTypeId) -> StructId {
        let gir_type = self.gir_types.get(gir_type_id);
        match gir_type {
            Some(GirType::Named(name)) => {
                if let Some(sid) = self.struct_reg.lookup(&name) {
                    return sid;
                }
            }
            // Unwrap pointer/ref types to find the inner Named type (e.g. &Color → Color).
            Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => {
                return self.resolve_struct_id(*inner);
            }
            _ => {}
        }
        StructId(0) // fallback
    }

    /// Resolve struct ID with field-count safety: if the resolved struct has
    /// fewer fields than the field index, try a wider compatible type.
    pub(super) fn resolve_struct_id_for_field(&self, gir_type_id: GirTypeId, _field: u32, _structs: &[StructDef]) -> StructId {
        let sid = self.resolve_struct_id(gir_type_id);
        // Str now has 4 fields (data, len, cap, alloc) matching GorgetString — no promotion needed.
        sid
    }

    pub(super) fn resolve_type_name(&self, gir_type_id: GirTypeId) -> String {
        let gir_type = self.gir_types.get(gir_type_id);
        // Unwrap Ptr/MutPtr to find the inner Named type.
        let inner = match gir_type {
            Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => self.gir_types.get(*inner),
            other => other,
        };
        if let Some(GirType::Named(name)) = inner {
            name.clone()
        } else {
            String::new()
        }
    }

    pub(super) fn resolve_field_type(&self, gir_type_id: GirTypeId, field: u32) -> LirType {
        let gir_type = self.gir_types.get(gir_type_id);
        // Unwrap Ptr/MutPtr to find the inner Named type.
        let inner_type = match gir_type {
            Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => self.gir_types.get(*inner),
            other => other,
        };
        if let Some(GirType::Named(name)) = inner_type {
            if let Some(def) = self.gir_types.get_type_def(name) {
                if let gir_types::TypeDefKind::Struct(sdef) = &def.kind {
                    if let Some(f) = sdef.fields.get(field as usize) {
                        return self.map_type(&f.type_id);
                    }
                }
            }
        }
        LirType::I64 // fallback
    }

    /// Return the GIR TypeId of a struct field (for tracking types through projection chains).
    pub(super) fn resolve_field_gir_type_id(&self, gir_type_id: GirTypeId, field: u32) -> GirTypeId {
        let gir_type = self.gir_types.get(gir_type_id);
        if let Some(GirType::Named(name)) = gir_type {
            if let Some(def) = self.gir_types.get_type_def(name) {
                if let gir_types::TypeDefKind::Struct(sdef) = &def.kind {
                    if let Some(f) = sdef.fields.get(field as usize) {
                        return f.type_id;
                    }
                }
            }
        }
        gir_type_id // fallback: keep same type
    }

    /// Resolve the pointee type for a Deref projection.
    pub(super) fn resolve_deref_gir_type_id(&self, gir_type_id: GirTypeId) -> GirTypeId {
        match self.gir_types.get(gir_type_id) {
            Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => *inner,
            Some(GirType::Named(name)) if name.starts_with("Box__") => {
                // Box types are Named("Box__X") — the inner type is encoded in the name.
                if let Some(type_def) = self.gir_types.get_type_def(name.as_str()) {
                    if let crate::ir::types::TypeDefKind::Struct(ref s) = type_def.kind {
                        if let Some(f) = s.fields.first() {
                            return f.type_id;
                        }
                    }
                }
                gir_type_id // fallback — resolve_place_type has name-based fallback
            }
            _ => gir_type_id, // fallback
        }
    }

    /// Compute the effective GIR type after following all projections in a place.
    pub(super) fn effective_place_type(&self, place: &Place) -> GirTypeId {
        let mut ty = if (place.local.0 as usize) < self.gir_func.locals.len() {
            self.gir_func.locals[place.local.0 as usize].type_id
        } else { crate::ir::types::I64_TYPE };
        for proj in &place.projections {
            match proj {
                Projection::Field(field) => {
                    ty = self.resolve_field_gir_type_id(ty, *field);
                }
                Projection::Deref => {
                    ty = self.resolve_deref_gir_type_id(ty);
                }
                Projection::Index(_) => {
                    // Element type — keep as-is for now (array element type tracking TBD)
                }
            }
        }
        ty
    }

    pub(super) fn resolve_enum_field_type(
        &self,
        gir_type_id: GirTypeId,
        variant_name: &str,
        field: u32,
    ) -> LirType {
        // Unwrap Ptr/MutPtr to get to the Named enum type.
        let mut tid = gir_type_id;
        loop {
            match self.gir_types.get(tid) {
                Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) => tid = *inner,
                _ => break,
            }
        }
        if let Some(GirType::Named(name)) = self.gir_types.get(tid) {
            if let Some(def) = self.gir_types.get_type_def(name) {
                if let gir_types::TypeDefKind::Enum(edef) = &def.kind {
                    for v in &edef.variants {
                        if v.name == variant_name {
                            if let Some(f) = v.fields.get(field as usize) {
                                return self.map_type(&f.type_id);
                            }
                        }
                    }
                }
            }
        }
        LirType::I64 // fallback
    }

    pub(super) fn resolve_variant_ordinal(&self, type_name: &str, variant_name: &str) -> usize {
        if let Some(def) = self.gir_types.get_type_def(type_name) {
            if let gir_types::TypeDefKind::Enum(edef) = &def.kind {
                for (i, v) in edef.variants.iter().enumerate() {
                    if v.name == variant_name {
                        return i;
                    }
                }
            }
        }
        0
    }

    pub(super) fn resolve_variant_field_offset(&self, type_name: &str, variant_name: &str) -> usize {
        // Field offset = 1 (tag) + sum of field counts of preceding variants.
        if let Some(def) = self.gir_types.get_type_def(type_name) {
            if let gir_types::TypeDefKind::Enum(edef) = &def.kind {
                let mut offset = 1; // tag field
                for v in &edef.variants {
                    if v.name == variant_name {
                        return offset;
                    }
                    offset += v.fields.len();
                }
            }
        }
        1
    }

    /// Get the GIR type IDs for a specific variant's fields.
    pub(super) fn resolve_variant_field_types(&self, type_name: &str, variant_name: &str) -> Vec<Option<GirTypeId>> {
        if let Some(def) = self.gir_types.get_type_def(type_name) {
            if let gir_types::TypeDefKind::Enum(edef) = &def.kind {
                for v in &edef.variants {
                    if v.name == variant_name {
                        return v.fields.iter().map(|f| Some(f.type_id)).collect();
                    }
                }
            }
        }
        vec![]
    }

    /// Materialize a properly tagged null-variant enum for an Assign { dst, Null }.
    /// Handles both simple locals (`dst.projections.is_empty()`) and projected
    /// field assignments (`local.field[i] = Null`).
    pub(super) fn try_materialize_null_for_assign(&mut self, dst: &Place, bb: BlockId) -> Option<()> {
        let local_idx = dst.local.0 as usize;
        if local_idx >= self.gir_func.locals.len() { return None; }

        // Resolve the target type through projections.
        let gir_ty = if dst.projections.is_empty() {
            self.gir_func.locals[local_idx].type_id
        } else {
            self.resolve_projected_gir_type(dst)?
        };

        let (struct_id, tag_ordinal) = self.find_enum_null_variant(gir_ty)?;

        if dst.projections.is_empty() {
            // Simple local: write tag into the local's slot.
            let slot = self.local_to_slot[local_idx];
            let base = self.lir_func.next_value();
            self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr { dst: base, slot });
            self.emit_enum_tag_store(base, struct_id, tag_ordinal, bb);
        } else {
            // Projected field: compute the field address, then write tag there.
            let base = self.lower_place_addr(dst, bb);
            self.emit_enum_tag_store(base, struct_id, tag_ordinal, bb);
        }
        Some(())
    }

    /// When a GIR Assign copies from an Option/Result-typed source to a
    /// non-Option/Result destination, the GIR C backend implicitly extracts
    /// the payload (e.g. `_21 = _23.data.Some._0`).  We replicate this by
    /// emitting FieldPtr(field=1) + Load on the source enum struct.
    pub(super) fn try_enum_payload_extract(
        &mut self,
        dst: &Place,
        value: &Operand,
        bb: BlockId,
    ) -> Option<ValueId> {
        // Only applies to Copy/Move of a simple local (no projections on source).
        let src_local = match value {
            Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => p.local,
            _ => return None,
        };

        let src_idx = src_local.0 as usize;
        let dst_idx = dst.local.0 as usize;
        if src_idx >= self.gir_func.locals.len() || dst_idx >= self.gir_func.locals.len() {
            return None;
        }

        let src_type_id = self.gir_func.locals[src_idx].type_id;
        let dst_type_id = self.gir_func.locals[dst_idx].type_id;

        // Check: source is Option__* or Result__*, destination is NOT.
        let src_name = match self.gir_types.get(src_type_id) {
            Some(GirType::Named(n)) => n.clone(),
            _ => return None,
        };
        let is_option = src_name.starts_with("Option__");
        let is_result = src_name.starts_with("Result__");
        if !is_option && !is_result {
            return None;
        }

        // Destination must not be the same enum type.
        let dst_is_same = match self.gir_types.get(dst_type_id) {
            Some(GirType::Named(n)) => *n == src_name,
            _ => false,
        };
        if dst_is_same {
            return None;
        }

        // Also skip if destination is another Option/Result.
        let dst_is_enum = match self.gir_types.get(dst_type_id) {
            Some(GirType::Named(n)) => n.starts_with("Option__") || n.starts_with("Result__"),
            _ => false,
        };
        if dst_is_enum {
            return None;
        }

        // Extract the payload: field 1 for Option (Some_0), field 1 for Result (Ok_0).
        let struct_id = self.resolve_struct_id(src_type_id);
        let payload_field: u32 = 1;

        let src_slot = self.local_to_slot[src_idx];
        let base = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr { dst: base, slot: src_slot });

        let fptr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
            dst: fptr,
            base,
            struct_id,
            field: payload_field,
        });

        let field_ty = self.resolve_enum_field_type(src_type_id, if is_option { "Some" } else { "Ok" }, 0);
        let result = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::Load {
            dst: result,
            ptr: fptr,
            ty: field_ty,
        });

        Some(result)
    }

    /// Detect `Box[Trait] ← Box[Concrete]` assignments and construct the trait object
    /// by setting field 0 (data) = src value and field 1 (vtable) = &Trait_for_Concrete_vtable.
    pub(super) fn try_trait_object_construct(
        &mut self,
        dst: &Place,
        value: &Operand,
        bb: BlockId,
    ) -> bool {
        let src_local = match value {
            Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => p.local,
            _ => return false,
        };
        let src_idx = src_local.0 as usize;
        let dst_idx = dst.local.0 as usize;
        if src_idx >= self.gir_func.locals.len() || dst_idx >= self.gir_func.locals.len() {
            return false;
        }
        let dst_type_id = self.gir_func.locals[dst_idx].type_id;
        let src_type_id = self.gir_func.locals[src_idx].type_id;
        let dst_name = match self.gir_types.get(dst_type_id) {
            Some(GirType::Named(n)) => n.clone(),
            _ => return false,
        };
        let src_name = match self.gir_types.get(src_type_id) {
            Some(GirType::Named(n)) => n.clone(),
            _ => return false,
        };
        // Both must be Box__ types with different inner types.
        if !dst_name.starts_with("Box__") || !src_name.starts_with("Box__") {
            return false;
        }
        let dst_inner = &dst_name[5..];
        let src_inner = &src_name[5..];
        if dst_inner == src_inner {
            return false;
        }
        // Check that a VTable type exists for the trait (dst_inner is the trait name).
        let vtable_type = format!("{dst_inner}_VTable");
        if self.gir_types.get_type_def(&vtable_type).is_none() {
            return false;
        }
        // Find the trait object struct (e.g. Describer_TraitObj).
        let trait_obj_type = format!("{dst_inner}_TraitObj");
        let trait_obj_sid = match self.struct_reg.lookup(&trait_obj_type) {
            Some(sid) => sid,
            None => return false,
        };
        // Find the vtable global (e.g. Describer_for_Widget_vtable).
        let vtable_global_name = format!("{dst_inner}_for_{src_inner}_vtable");
        let vtable_gid = match self.global_index.get(&vtable_global_name) {
            Some(&gid) => gid,
            None => return false,
        };

        // Construct the trait object:
        // field 0 (data) = src value (cast to void*)
        // field 1 (vtable) = &vtable_global
        let dst_slot = self.local_to_slot[dst_idx];
        let dst_base = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
            dst: dst_base,
            slot: dst_slot,
        });

        // Load src value (Box__Concrete = void*).
        // Box types are represented as LirType::Struct in LIR but are actually void*
        // at runtime. lower_operand returns the slot address for aggregates, so we
        // need to explicitly load the pointer value from the slot.
        let src_slot = self.local_to_slot[src_idx];
        let src_addr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
            dst: src_addr,
            slot: src_slot,
        });
        let src_val = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::Load {
            dst: src_val,
            ptr: src_addr,
            ty: LirType::Ptr,
        });

        // Store data pointer (field 0).
        let data_ptr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
            dst: data_ptr,
            base: dst_base,
            struct_id: trait_obj_sid,
            field: 0,
        });
        self.lir_func.block_mut(bb).insts.push(Inst::Store {
            ptr: data_ptr,
            value: src_val,
        });

        // Store vtable pointer (field 1).
        let vtable_addr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::GlobalAddr {
            dst: vtable_addr,
            global: vtable_gid,
        });
        let vtable_ptr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
            dst: vtable_ptr,
            base: dst_base,
            struct_id: trait_obj_sid,
            field: 1,
        });
        self.lir_func.block_mut(bb).insts.push(Inst::Store {
            ptr: vtable_ptr,
            value: vtable_addr,
        });

        true
    }

    /// Emit instructions to set the tag field of an enum at `base` address.
    pub(super) fn emit_enum_tag_store(&mut self, base: ValueId, struct_id: StructId, tag_ordinal: usize, bb: BlockId) {
        let tag_val = self.emit_i32_const(bb, tag_ordinal as i64);
        let tag_ptr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
            dst: tag_ptr, base, struct_id, field: 0,
        });
        self.lir_func.block_mut(bb).insts.push(Inst::Store {
            ptr: tag_ptr, value: tag_val,
        });
    }

    /// Resolve the GIR type of a Place by walking projections.
    pub(super) fn resolve_projected_gir_type(&self, place: &Place) -> Option<GirTypeId> {
        let mut current_type = self.gir_func.locals[place.local.0 as usize].type_id;
        for proj in &place.projections {
            match proj {
                Projection::Field(field) => {
                    if let Some(GirType::Named(name)) = self.gir_types.get(current_type) {
                        if let Some(def) = self.gir_types.get_type_def(&name) {
                            match &def.kind {
                                gir_types::TypeDefKind::Struct(sdef) => {
                                    if let Some(f) = sdef.fields.get(*field as usize) {
                                        current_type = f.type_id;
                                        continue;
                                    }
                                }
                                gir_types::TypeDefKind::Enum(edef) => {
                                    // Field 0 = tag, field 1+ = variant payloads
                                    // The payload fields are numbered across variants.
                                    let mut fi = 0u32;
                                    for v in &edef.variants {
                                        for vf in &v.fields {
                                            fi += 1; // tag takes field 0
                                            if fi == *field {
                                                current_type = vf.type_id;
                                                break;
                                            }
                                        }
                                    }
                                    continue;
                                }
                                _ => {}
                            }
                        }
                    }
                    return None;
                }
                Projection::Deref | Projection::Index(_) => {
                    return None; // Conservative: can't resolve through deref/index.
                }
            }
        }
        Some(current_type)
    }

    /// If `gir_ty` is an enum, find the first variant with no fields (the "null" variant,
    /// e.g. None for Option, Error for Result).  Returns `(StructId, tag_ordinal)`.
    pub(super) fn find_enum_null_variant(&self, gir_ty: GirTypeId) -> Option<(StructId, usize)> {
        let gir_type = self.gir_types.get(gir_ty)?;
        if let GirType::Named(name) = gir_type {
            let def = self.gir_types.get_type_def(&name)?;
            if let gir_types::TypeDefKind::Enum(edef) = &def.kind {
                let struct_id = self.struct_reg.lookup(&name)?;
                for (i, v) in edef.variants.iter().enumerate() {
                    if v.fields.is_empty() {
                        return Some((struct_id, i));
                    }
                }
            }
        }
        None
    }

    /// For a collection method call like `Vector__Option__int64_t__push`,
    /// when a `Constant::Null` arg is passed as the element, create a properly
    /// tagged enum slot on the stack and return its address.
    /// Returns `None` if we can't determine the element type.
    pub(super) fn materialize_null_enum_for_collection_arg(&mut self, func_name: &str, bb: BlockId) -> Option<ValueId> {
        // Extract the element type name from monomorphized call names.
        // Patterns: Vector__ELEM__push, Vector__ELEM__set, Set__ELEM__add,
        //           Heap__ELEM__push, gorget_channel_send, etc.
        let elem_type_name = Self::extract_elem_type_from_method_name(func_name)?;

        // Look up the struct and find the null variant (first fieldless variant).
        let struct_id = self.struct_reg.lookup(&elem_type_name)?;
        let _lir_struct = self.module_structs.get(struct_id.0 as usize)?;

        // Find the null variant tag by looking up the GIR type def.
        let tag_ordinal = self.find_null_variant_tag_by_name(&elem_type_name)?;

        // Create a temporary slot of the enum type.
        let slot = self.lir_func.add_slot(LirType::Struct(struct_id), None);
        let base = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr { dst: base, slot });

        // Zero-init the slot first (memset 0).
        let _zero = self.emit_i32_const(bb, 0);
        // Set the tag field to the null variant ordinal.
        let tag_val = self.emit_i32_const(bb, tag_ordinal as i64);
        let tag_ptr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
            dst: tag_ptr, base, struct_id, field: 0,
        });
        self.lir_func.block_mut(bb).insts.push(Inst::Store {
            ptr: tag_ptr, value: tag_val,
        });
        Some(base)
    }

    /// Find the null variant tag ordinal by struct name.
    pub(super) fn find_null_variant_tag_by_name(&self, name: &str) -> Option<usize> {
        let def = self.gir_types.get_type_def(name)?;
        if let gir_types::TypeDefKind::Enum(edef) = &def.kind {
            for (i, v) in edef.variants.iter().enumerate() {
                if v.fields.is_empty() {
                    return Some(i);
                }
            }
        }
        None
    }

    /// Extract element type name from a monomorphized collection method name.
    /// E.g., "Vector__Option__int64_t__push" → "Option__int64_t"
    pub(super) fn extract_elem_type_from_method_name(func_name: &str) -> Option<String> {
        // Collection prefixes and their method suffixes
        let prefixes = ["Vector__", "Set__", "Heap__", "HashSet__", "Deque__"];
        let suffixes = ["__push", "__add", "__set", "__contains", "__remove",
                        "__insert", "__index_of", "__binary_search"];
        for prefix in &prefixes {
            if let Some(rest) = func_name.strip_prefix(prefix) {
                for suffix in &suffixes {
                    if let Some(elem) = rest.strip_suffix(suffix) {
                        if !elem.is_empty() {
                            return Some(elem.to_string());
                        }
                    }
                }
            }
        }
        None
    }

    pub(super) fn resolve_place_type(&self, place: &Place) -> LirType {
        let local_type = self.gir_func.locals[place.local.0 as usize].type_id;
        if place.projections.is_empty() {
            return self.map_type(&local_type);
        }

        // Walk projections to determine final type.
        let mut current_type = local_type;
        for proj in &place.projections {
            match proj {
                Projection::Field(field) => {
                    if let Some(GirType::Named(name)) = self.gir_types.get(current_type) {
                        if let Some(def) = self.gir_types.get_type_def(&name) {
                            if let gir_types::TypeDefKind::Struct(sdef) = &def.kind {
                                if let Some(f) = sdef.fields.get(*field as usize) {
                                    current_type = f.type_id;
                                    continue;
                                }
                            }
                        }
                    }
                    return LirType::I64; // fallback
                }
                Projection::Deref => {
                    let resolved = self.resolve_deref_gir_type_id(current_type);
                    if resolved == current_type {
                        // resolve_deref_gir_type_id couldn't resolve — try Box name parsing
                        if let Some(GirType::Named(name)) = self.gir_types.get(current_type) {
                            if let Some(inner) = name.strip_prefix("Box__") {
                                return match inner {
                                    "int64_t" => LirType::I64,
                                    "int32_t" => LirType::I32,
                                    "int16_t" => LirType::I16,
                                    "int8_t" => LirType::I8,
                                    "uint8_t" => LirType::U8,
                                    "double" => LirType::F64,
                                    "float" => LirType::F32,
                                    "bool" => LirType::Bool,
                                    "GorgetString" => LirType::Struct(
                                        self.struct_reg.lookup("GorgetString").unwrap_or(StructId(0))
                                    ),
                                    _ => {
                                        // Named inner type — look up as struct
                                        if let Some(sid) = self.struct_reg.lookup(inner) {
                                            LirType::Struct(sid)
                                        } else {
                                            LirType::I64
                                        }
                                    }
                                };
                            }
                        }
                    }
                    current_type = resolved;
                }
                Projection::Index(_) => {
                    return LirType::I64; // default element type
                }
            }
        }

        self.map_type(&current_type)
    }
}
