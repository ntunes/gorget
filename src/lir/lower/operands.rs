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
        // §6.8 Stage 4: slot_kind is the canonical signal — was
        // `ownership.is_ref()`. SlotKind::BorrowedPtr means the slot's
        // bytes ARE the pointer; SlotLoad reads them directly.
        let is_ref_local = self.gir_func.locals.get(place.local.0 as usize)
            .map_or(false, |l| l.slot_kind == crate::ir::SlotKind::BorrowedPtr);
        // Only treat PtrTo(GorgetString) slots as implicit ref locals.
        // Other PtrTo slots carry type information but are not reference locals.
        let is_ptr_to_slot = match &self.lir_func.slots[slot.0 as usize].ty {
            LirType::PtrTo(sid) => self.struct_reg.lookup("GorgetString") == Some(*sid),
            _ => false,
        };
        let has_deref = place.projections.first() == Some(&Projection::Deref);
        if (is_ref_local || is_ptr_to_slot) && !has_deref {
            self.push_inst(bb, Inst::SlotLoad { dst: addr, slot, ty: LirType::Ptr });
        } else {
            self.push_inst(bb, Inst::SlotAddr { dst: addr, slot });
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
                    self.push_inst(bb, Inst::FieldPtr {
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
                    self.push_inst(bb, Inst::SlotLoad {
                        dst: idx,
                        slot: idx_slot,
                        ty: LirType::I64,
                    });
                    let next = self.lir_func.next_value();
                    self.push_inst(bb, Inst::ElemPtr {
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
                    self.push_inst(bb, Inst::Load {
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
                    self.push_inst(bb, Inst::GlobalAddr { dst: addr, global: gid });
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
        self.push_inst(bb, inst);
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

    /// The callee's DECLARED per-parameter ownership sigils for an operand
    /// that holds a callable (`Callable[T]` local, escaped closure, any
    /// `FnPtr`-typed local), read from `GirType::FnPtr::param_ownerships`.
    ///
    /// This is the typed fact the indirect-call ABI must be WRITTEN from
    /// (devbook/24 rules 2 and 4): a `&` (`Ownership::MutableBorrow`) param is
    /// a **pointer** in the callee's declared signature — which is exactly what
    /// the `__adapt_*` shim emitter derives from `LirFunction.params` — so the
    /// call site forwards the pointer and never dereferences it.
    ///
    /// Returns an EMPTY vec when the operand's GIR type is not an `FnPtr`,
    /// which is the *unknown* case, not the *no-borrows* case: a
    /// `Callable[..]` PARAMETER's GIR local type is erased to `unit`, and a
    /// container element's is `fn() -> i64`. Callers must leave such args
    /// alone rather than tagging them by value; the
    /// `GG_REPORT_CLOSURE_ABI_GUESS` guard reports exactly those sites.
    pub(super) fn operand_param_ownerships(
        &self,
        operand: &Operand,
    ) -> Vec<crate::parser::ast::Ownership> {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => {
                let idx = place.local.0 as usize;
                let Some(local) = self.gir_func.locals.get(idx) else {
                    return Vec::new();
                };
                match self.gir_types.get(local.type_id) {
                    Some(GirType::FnPtr { param_ownerships, .. }) => param_ownerships.clone(),
                    _ => Vec::new(),
                }
            }
            Operand::Constant(_) => Vec::new(),
        }
    }

    /// Per-parameter "the callee declares a POINTER here" for an indirect
    /// (`Callable`/closure) call — the fact `Inst::CallClosure`'s `arg_abis`
    /// must be written from.
    ///
    /// Two channels carry the ONE fact, in preference order, because the
    /// callable's declared signature reaches this point by two routes:
    ///  1. the closure operand's own `GirType::FnPtr` (annotated locals,
    ///     escaped closures), and
    ///  2. the module's declared `fn_param_abis`, published at the GIR call
    ///     site under [`crate::ir::abi::indirect_callee_key`] — the route that
    ///     survives the `Callable[..]` PARAMETER's erasure to `unit`.
    ///
    /// An EMPTY result means the signature reached neither channel (a
    /// container element, an `auto`-bound callable): that is *unknown*, not
    /// *no borrows*, and the caller must leave those args as the by-value
    /// promotion left them.
    pub(super) fn declared_closure_param_by_ptr(
        &self,
        closure_operand: Option<&Operand>,
        synthetic_callee: &str,
    ) -> Vec<bool> {
        use crate::ir::lowering::context::ParamABI;
        if let Some(op) = closure_operand {
            let owns = self.operand_param_ownerships(op);
            if !owns.is_empty() {
                return owns
                    .iter()
                    .map(|o| *o == crate::parser::ast::Ownership::MutableBorrow)
                    .collect();
            }
        }
        let key = crate::ir::abi::indirect_callee_key(synthetic_callee, &self.lir_func.name);
        self.fn_param_abis
            .get(&key)
            .map(|abis| abis.iter().map(|a| *a == ParamABI::ByMutPtr).collect())
            .unwrap_or_default()
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

        // For known runtime functions, use canonical signatures + ABI tags.
        if let Some(rsig) = crate::lir::runtime::RuntimeFn::from_c_name(name)
            .map(|f| f.resolve_lir_sig(self.struct_reg))
        {
            // Prefer runtime ABI tags; fall back to user-declared extern ABI.
            let abis = if rsig.param_abis.iter().any(|a| *a != crate::ir::abi::AbiKind::Auto) {
                rsig.param_abis
            } else {
                abi_tags.clone()
            };
            if let Some(existing) = self.pending_externs.iter_mut().find(|e| e.name == name) {
                existing.params = rsig.params;
                existing.return_type = rsig.ret;
                if existing.param_abis.iter().all(|a| *a == crate::ir::abi::AbiKind::Auto) {
                    existing.param_abis = abis;
                }
            } else {
                self.pending_externs.push(LirExtern {
                    name: name.to_string(),
                    params: rsig.params,
                    return_type: rsig.ret,
                    is_variadic: false,
                    param_abis: abis,
                    return_abi: ret_abi,
                    combinator_result_struct_id: None,
                });
            }
            return;
        }

        // Higher-order collection helpers (Vector__T__filter, Dict__K__V__map, Set__T__any, etc.)
        // All take self-by-ptr as arg 0, remaining args are closures/scalars (Opaque).
        //
        // Phase A SSoT: the kind-detection arm previously matched six prefix
        // patterns to decide "is this a collection method?". The same decision
        // now reads from `compiler/data/resources.gg`'s typed
        // `collection_kind` (per layering-discipline rule 2). The table is
        // fully populated (Vector__/Deque__/Dict__/HashMap__/Set__/HashSet__
        // all have MkPrefix entries), so the former prefix fallback was dead
        // and has been removed — an unknown name is simply not a collection.
        {
            use crate::ir::abi::AbiKind;
            use crate::resource_schema::CollectionKind as SchemaCollectionKind;
            let is_collection_name = crate::resources::table().lookup(name)
                .map(|m| matches!(m.collection_kind,
                    SchemaCollectionKind::Vector | SchemaCollectionKind::Deque
                    | SchemaCollectionKind::Dict
                    | SchemaCollectionKind::OrderedSet | SchemaCollectionKind::HashSet))
                .unwrap_or(false);
            let is_higher_order = is_collection_name
                && name.rfind("__").map_or(false, |pos| {
                    let method = &name[pos + 2..];
                    matches!(method, "filter" | "map" | "flat_map" | "fold" | "reduce"
                        | "any" | "all" | "each" | "find" | "find_index"
                        | "sorted" | "sort" | "sorted_by" | "sort_by"
                        | "sorted_by_key" | "sort_by_key"
                        | "windows" | "chunks" | "unique" | "count"
                        | "update" | "union" | "intersection" | "difference"
                        | "symmetric_difference")
                });
            if is_higher_order {
                let method = name.rfind("__").map(|pos| &name[pos + 2..]).unwrap_or("");
                let is_binary_collection_op = matches!(method,
                    "union" | "intersection" | "difference" | "symmetric_difference" | "update");
                let mut abis = vec![AbiKind::Ptr]; // self-by-ptr
                for i in 1..arg_types.len() {
                    if is_binary_collection_op && i == 1 {
                        // Second set/dict arg: aggregate by value (may arrive as Ptr from SlotAddr)
                        abis.push(AbiKind::ByValue);
                    } else {
                        abis.push(AbiKind::Opaque);
                    }
                }
                let actual_ret = ret_ty.clone();
                self.pending_externs.push(LirExtern {
                    name: name.to_string(),
                    params: arg_types.to_vec(),
                    return_type: actual_ret,
                    is_variadic: false,
                    param_abis: abis,
                    return_abi: crate::ir::abi::AbiKind::Auto,
                    combinator_result_struct_id: None,
                });
                return;
            }
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
            combinator_result_struct_id: None,
        });
    }

    /// Look up ABI tags for an extern function already registered via `ensure_extern`.
    /// Returns the `param_abis` from the `LirExtern`, or empty if not found.
    pub(super) fn lookup_arg_abis(&self, name: &str) -> Vec<crate::ir::abi::AbiKind> {
        self.pending_externs.iter()
            .find(|e| e.name == name)
            .map(|e| e.param_abis.clone())
            .unwrap_or_default()
    }

    pub(super) fn store_to_local(&mut self, local: ir::types::LocalId, value: ValueId, bb: BlockId) {
        let slot = self.local_to_slot[local.0 as usize];
        self.push_inst(bb, Inst::SlotStore { slot, value, is_move: false });
    }

    pub(super) fn store_to_place(&mut self, place: &Place, value: ValueId, bb: BlockId) {
        if place.projections.is_empty() {
            self.store_to_local(place.local, value, bb);
        } else {
            let addr = self.lower_place_addr(place, bb);
            self.push_inst(bb, Inst::Store { ptr: addr, value });
        }
    }

    /// Emit an I64 constant and return its ValueId.
    pub(super) fn emit_i64_const(&mut self, bb: BlockId, value: i64) -> ValueId {
        let val = self.lir_func.next_value();
        self.push_inst(bb, Inst::IConst {
            dst: val, ty: LirType::I64, value,
        });
        val
    }

    /// Emit an I32 constant and return its ValueId.
    pub(super) fn emit_i32_const(&mut self, bb: BlockId, value: i64) -> ValueId {
        let val = self.lir_func.next_value();
        self.push_inst(bb, Inst::IConst {
            dst: val, ty: LirType::I32, value,
        });
        val
    }

    /// Emit `Inst::SizeOf { dst, ty }` and return the resulting value id.
    ///
    /// BIR lowering (`src/bir/lower.rs`) resolves this to a concrete
    /// `IConst` via the shared `c_sizeof_lir_type` table before backends
    /// see the module.
    pub(super) fn emit_size_of(&mut self, bb: BlockId, ty: LirType) -> ValueId {
        let val = self.lir_func.next_value();
        self.push_inst(bb, Inst::SizeOf {
            dst: val, ty,
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
                match &def.kind {
                    gir_types::TypeDefKind::Struct(sdef) => {
                        if let Some(f) = sdef.fields.get(field as usize) {
                            return self.map_type(&f.type_id);
                        }
                    }
                    // Enum with flat layout (Option, Result, etc.): the LIR struct
                    // has [("tag", I32), ("<variant>_<idx>", payload_type), …]
                    // built in `lower_type_defs` Pass 2. Reading it back from the
                    // struct registry gives us the authoritative LirType — without
                    // this, Field(1) on `Option__Ref_T` falls through to I64 and
                    // the C backend emits `*(int64_t*)(&Some_0)` which trips the
                    // Ptr-ABI debug_assert when passed to a clone function.
                    // (Soundness-wise the cast is cosmetic on 64-bit targets, but
                    // we want the LIR type tag to match the declared field type.)
                    gir_types::TypeDefKind::Enum(_) => {
                        if let Some(sid) = self.struct_reg.lookup(name) {
                            if let Some(sdef) = self.module_structs.get(sid.0 as usize) {
                                if let Some((_name, ty)) = sdef.fields.get(field as usize) {
                                    return ty.clone();
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        LirType::I64 // fallback
    }

    /// Return the GIR TypeId of a struct field (for tracking types through projection chains).
    /// Unwraps `Ptr(T)` / `MutPtr(T)` once so callers using a Ptr-typed base
    /// (e.g. closure env params, `*Heap__T` self) still resolve the underlying
    /// struct's field type rather than falling back to the Ptr itself.
    pub(super) fn resolve_field_gir_type_id(&self, gir_type_id: GirTypeId, field: u32) -> GirTypeId {
        let gir_type = self.gir_types.get(gir_type_id);
        let inner_type = match gir_type {
            Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => self.gir_types.get(*inner),
            other => other,
        };
        if let Some(GirType::Named(name)) = inner_type {
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
            Some(GirType::Named(name)) if self.gir_types.is_box(gir_type_id) => {
                // Box types — read the typed `metadata.is_box` flag rather
                // than name-prefix probing. The inner type lives in the
                // canonical `_0` field of the TypeDef.
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

        let base = if dst.projections.is_empty() {
            // Simple local: write tag into the local's slot.
            let slot = self.local_to_slot[local_idx];
            let base = self.lir_func.next_value();
            self.push_inst(bb, Inst::SlotAddr { dst: base, slot });
            base
        } else {
            // Projected field: compute the field address, then write tag there.
            self.lower_place_addr(dst, bb)
        };
        // Canonical `Inst::EnumInit` — unit variant with explicit parent type.
        self.push_inst(bb, Inst::EnumInit {
            target: base,
            struct_id,
            variant_tag: tag_ordinal as u32,
            fields: vec![],
        });
        Some(())
    }

    /// When a GIR Assign copies from an Option/Result-typed source to a
    /// non-Option/Result destination, the GIR C backend implicitly extracts
    /// the payload (e.g. `_21 = _23.data.Some._0`).  We replicate this by
    /// emitting FieldPtr(field=1) + Load on the source enum struct.
    pub(super) fn try_enum_payload_extract(
        &mut self,
        mode: crate::ir::instructions::AssignMode,
        dst: &Place,
        value: &Operand,
        bb: BlockId,
    ) -> Option<ValueId> {
        // A Borrow-mode assign is an aliasing bind, never an implicit
        // payload unwrap — the GIR producer hands us typed metadata
        // (`mode`) that says so. Extracting the payload into a
        // pointer-typed destination would make downstream deref the
        // payload as a pointer — SIGSEGV (Chain C item 3, consumer
        // hardening; the producer-side Branch-C suppress is the primary
        // fix per devbook/24).
        if matches!(mode, crate::ir::instructions::AssignMode::Borrow) {
            return None;
        }
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
        // Walk projections so a store into a struct field (e.g. `fm.desc = some_opt`)
        // compares against the actual field type, not the base struct's type. Without
        // this, an Option-typed field whose enclosing struct isn't itself an Option
        // would trigger the payload-extract path and silently drop the discriminant.
        // Snag #4b (2026-05-01).
        let dst_type_id = self.effective_place_type(dst);
        // For the SAME-ENUM comparison only, see through one Ptr level: a
        // Branch-C bind retypes the dst to Ptr(enum), and a pointer to the
        // SAME enum is still "same enum" (a `GirType::Named`-only match let
        // Ptr(enum) fall through to the payload-extract — item 3's
        // mis-classification). The Ptr-unwrap must NOT feed the
        // another-Option/Result skip below: `Option[Ref[T]]` lifts emit a
        // LEGITIMATE extract into a `Ptr(Option__T)` dst (src
        // `Option__Ref__Option__T`), and unwrapping there made the skip
        // swallow it (test_collections_nested regression, caught by the
        // full suite 2026-06-11).
        let dst_same_cmp_type = match self.gir_types.get(dst_type_id) {
            Some(GirType::Ptr(inner)) => *inner,
            _ => dst_type_id,
        };

        // Check: source is Option__* or Result__*, destination is NOT.
        // Read typed `enum_category` from TypeMetadata (Phase A) instead of
        // matching the type name's prefix — same source of truth set at
        // GIR type registration.
        use crate::ir::types::EnumCategory;
        let src_cat = self.gir_types.enum_category(src_type_id);
        let is_option = match src_cat {
            Some(EnumCategory::Option) => true,
            Some(EnumCategory::Result) => false,
            None => return None,
        };
        let src_name = match self.gir_types.get(src_type_id) {
            Some(GirType::Named(n)) => n.clone(),
            _ => return None,
        };

        // Destination must not be the same enum type (through one Ptr).
        let dst_is_same = match self.gir_types.get(dst_same_cmp_type) {
            Some(GirType::Named(n)) => *n == src_name,
            _ => false,
        };
        if dst_is_same {
            return None;
        }

        // Also skip if destination is another Option/Result.
        if self.gir_types.enum_category(dst_type_id).is_some() {
            return None;
        }

        // Extract the payload: field 1 for Option (Some_0), field 1 for Result (Ok_0).
        let struct_id = self.resolve_struct_id(src_type_id);
        let payload_field: u32 = 1;

        let src_slot = self.local_to_slot[src_idx];
        let base = self.lir_func.next_value();
        self.push_inst(bb, Inst::SlotAddr { dst: base, slot: src_slot });

        let fptr = self.lir_func.next_value();
        self.push_inst(bb, Inst::FieldPtr {
            dst: fptr,
            base,
            struct_id,
            field: payload_field,
        });

        let field_ty = self.resolve_enum_field_type(src_type_id, if is_option { "Some" } else { "Ok" }, 0);
        let result = self.lir_func.next_value();
        self.push_inst(bb, Inst::Load {
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
        // Both must be Box types with different inner types. Read the typed
        // `metadata.is_box` flag rather than probing the name prefix.
        if !self.gir_types.is_box(dst_type_id) || !self.gir_types.is_box(src_type_id) {
            return false;
        }
        // The Box__ prefix carries the mangled inner-type name; we still
        // string-strip it because the inner name is what drives vtable /
        // trait-obj struct lookup downstream (the mangling boundary).
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
        //
        // The construction goes through a fresh TEMP slot + a canonical
        // `SlotStore` into the destination — NOT direct field stores into the
        // destination slot. Field stores through `SlotAddr`+`FieldPtr` are
        // invisible to drop-elaboration's init dataflow (`apply_inst_effect`
        // in `src/lir/drop_elab.rs` treats `SlotStore` as the slot-init
        // signal), so writing the dst slot directly left it "Uninitialized"
        // and its scope-exit `DropIfAlive` was DELETED — leaking the trait
        // object's data box (16B on dynamic_dispatch, 261B on serializable).
        // Write-site fix per the layering-discipline debugging heuristic:
        // emit the shape every downstream pass already understands.
        let dst_slot = self.local_to_slot[dst_idx];
        let dst_slot_ty = self.lir_func.slots[dst_slot.0 as usize].ty.clone();
        let tmp_slot = self.lir_func.add_slot(dst_slot_ty, None);
        let dst_base = self.lir_func.next_value();
        self.push_inst(bb, Inst::SlotAddr {
            dst: dst_base,
            slot: tmp_slot,
        });

        // Load src value (Box__Concrete = void*).
        // Box types are represented as LirType::Struct in LIR but are actually void*
        // at runtime. lower_operand returns the slot address for aggregates, so we
        // need to explicitly load the pointer value from the slot.
        let src_slot = self.local_to_slot[src_idx];
        let src_addr = self.lir_func.next_value();
        self.push_inst(bb, Inst::SlotAddr {
            dst: src_addr,
            slot: src_slot,
        });
        let src_val = self.lir_func.next_value();
        self.push_inst(bb, Inst::Load {
            dst: src_val,
            ptr: src_addr,
            ty: LirType::Ptr,
        });

        // Store data pointer (field 0).
        let data_ptr = self.lir_func.next_value();
        self.push_inst(bb, Inst::FieldPtr {
            dst: data_ptr,
            base: dst_base,
            struct_id: trait_obj_sid,
            field: 0,
        });
        self.push_inst(bb, Inst::Store {
            ptr: data_ptr,
            value: src_val,
        });

        // Store vtable pointer (field 1).
        let vtable_addr = self.lir_func.next_value();
        self.push_inst(bb, Inst::GlobalAddr {
            dst: vtable_addr,
            global: vtable_gid,
        });
        let vtable_ptr = self.lir_func.next_value();
        self.push_inst(bb, Inst::FieldPtr {
            dst: vtable_ptr,
            base: dst_base,
            struct_id: trait_obj_sid,
            field: 1,
        });
        self.push_inst(bb, Inst::Store {
            ptr: vtable_ptr,
            value: vtable_addr,
        });

        // Canonical init of the destination: aggregate SlotStore (memcpy of
        // the 16-byte {data, vtable} temp). This is the write drop-elab's
        // init dataflow reads, so the destination's scope-exit DropIfAlive
        // survives elaboration.
        let tmp_addr = self.lir_func.next_value();
        self.push_inst(bb, Inst::SlotAddr {
            dst: tmp_addr,
            slot: tmp_slot,
        });
        self.push_inst(bb, Inst::SlotStore {
            slot: dst_slot,
            value: tmp_addr,
            is_move: true,
        });

        true
    }

    /// Detect primitive → Result/Option slot stores and emit explicit wrapping:
    /// memset to zero, set tag = 0 (Ok/Some), store payload into the Ok_0/Some_0 field.
    /// Returns true if wrapping was emitted; false to fall through to normal store.
    pub(super) fn try_result_option_wrap(
        &mut self,
        dst_local: ir::types::LocalId,
        value: &Operand,
        bb: BlockId,
    ) -> bool {
        let dst_idx = dst_local.0 as usize;
        if dst_idx >= self.local_to_slot.len() { return false; }
        let dst_slot = self.local_to_slot[dst_idx];
        let slot_ty = self.lir_func.slots[dst_slot.0 as usize].ty.clone();

        // Check if destination slot is a Result__ or Option__ struct.
        let slot_sid = match &slot_ty {
            LirType::Struct(sid) => *sid,
            _ => return false,
        };
        // Read the LIR `enum_kind` flag (typed Phase A metadata propagated to
        // the LIR StructDef in `lir/lower/mod.rs` from GIR's `enum_category`).
        // Replaces a `slot_name.starts_with("Result__"/"Option__")` probe.
        use crate::lir::EnumKind;
        let slot_kind = self.module_structs.get(slot_sid.0 as usize)
            .map(|s| s.enum_kind).unwrap_or(EnumKind::NotEnum);
        let is_result = slot_kind == EnumKind::Result;
        let is_option = slot_kind == EnumKind::Option;
        if !is_result && !is_option { return false; }

        // Check the source operand's GIR type — skip if already an Option/Result
        // (e.g. int8_t__parse returns Option[int8] but GIR types the temp as I64;
        // the C backend inlines the parse+range-check, so we must not re-wrap).
        // Also skip when the GIR source type doesn't match the slot's payload type
        // (e.g. I64 temp → Option__int8_t: the I64 is a sentinel-encoded Option,
        // not a bare int to be wrapped).
        if let Operand::Copy(place) | Operand::Move(place) = value {
            let src_idx = place.local.0 as usize;
            if place.projections.is_empty() && src_idx < self.gir_func.locals.len() {
                let src_gir_ty = self.gir_func.locals[src_idx].type_id;
                // Read typed `enum_category` from GIR TypeMetadata (Phase A).
                if self.gir_types.enum_category(src_gir_ty).is_some() {
                    return false;
                }
                // Source GIR type maps to I64/F64 but destination is an Option/Result
                // with a narrower payload (e.g. Option__int8_t): the I64 is a sentinel-
                // encoded Option from a parse function, not a bare value to be wrapped.
                let src_lir = self.map_type(&src_gir_ty);
                if src_lir != self.map_type(&self.gir_func.locals[dst_idx].type_id) {
                    // GIR source and dest types differ at the LIR level — this is a
                    // normal store (possibly sentinel-encoded), not a primitive wrap.
                    return false;
                }
            }
        }

        // Check the source operand's LIR type — only wrap primitives.
        let src_lir_ty = self.operand_lir_type(value);
        let is_primitive = matches!(src_lir_ty,
            LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64
            | LirType::U8 | LirType::U16 | LirType::U32 | LirType::U64
            | LirType::F32 | LirType::F64 | LirType::Bool
        );
        if !is_primitive { return false; }

        // Find the payload field index (Ok_0 or Some_0).
        let prefix = if is_result { "Ok" } else { "Some" };
        let payload_field_idx = self.module_structs.get(slot_sid.0 as usize)
            .and_then(|sd| sd.fields.iter().enumerate()
                .find(|(_, (n, _))| n.starts_with(prefix))
                .map(|(i, _)| i as u32));
        let payload_field_idx = match payload_field_idx {
            Some(idx) => idx,
            None => return false,
        };

        // Lower the value operand.
        let val = self.lower_operand(value, bb);

        // Get slot address.
        let slot_addr = self.lir_func.next_value();
        self.push_inst(bb, Inst::SlotAddr {
            dst: slot_addr, slot: dst_slot,
        });

        // 1. memset(&slot, 0, sizeof(slot)) — zero the whole struct.
        let size = c_sizeof_lir_type(&slot_ty, self.module_structs) as i64;
        let size_val = self.emit_i64_const(bb, size);
        let zero_byte = self.emit_i32_const(bb, 0);
        self.ensure_extern("memset", &[LirType::Ptr, LirType::I32, LirType::I64], &LirType::Ptr);
        let abis = self.lookup_arg_abis("memset");
        self.push_inst(bb, Inst::CallExtern {
            dst: None,
            name: "memset".to_string(),
            args: vec![slot_addr, zero_byte, size_val],
            arg_abis: abis,
        });

        // 2. Emit canonical `Inst::EnumInit` for the Ok(val) / Some(val) wrap.
        //    The parent struct id (Result__T__E or Option__T) is explicit on the
        //    instruction, so backends don't need to infer the parent enum type
        //    from dst or surrounding context — they read it off the inst.
        self.push_inst(bb, Inst::EnumInit {
            target: slot_addr,
            struct_id: slot_sid,
            variant_tag: 0,
            fields: vec![(payload_field_idx, val)],
        });

        true
    }

    // `emit_enum_tag_store` was removed — all enum-tag writes now go through
    // the canonical `Inst::EnumInit` op with `fields: vec![]` for unit
    // variants. BIR lowering expands the tag-write sequence uniformly.

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
        self.push_inst(bb, Inst::SlotAddr { dst: base, slot });

        // Canonical `Inst::EnumInit` — no payload, just the tag for the null
        // variant. BIR expansion writes the tag field; the rest of the slot
        // stays zero from the slot's zero-init.
        self.push_inst(bb, Inst::EnumInit {
            target: base,
            struct_id,
            variant_tag: tag_ordinal as u32,
            fields: vec![],
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
                            match &def.kind {
                                gir_types::TypeDefKind::Struct(sdef) => {
                                    if let Some(f) = sdef.fields.get(*field as usize) {
                                        current_type = f.type_id;
                                        continue;
                                    }
                                }
                                // Enum with flat layout (Option/Result, etc.): the
                                // LIR struct has [("tag", I32), ("<variant>_<idx>", payload), …].
                                // Reading from the LIR struct registry gives the
                                // authoritative LirType for the projected field —
                                // without this, Field(1) on `Option__Ref_T` falls
                                // through to I64 and the C backend emits
                                // `*(int64_t*)(&Some_0)`, tripping the Ptr-ABI
                                // debug_assert when the value is passed to a clone
                                // function. Return the LIR type directly (no further
                                // projection possible — enum payload reads are terminal).
                                gir_types::TypeDefKind::Enum(_) => {
                                    if let Some(sid) = self.struct_reg.lookup(&name) {
                                        if let Some(sdef) = self.module_structs.get(sid.0 as usize) {
                                            if let Some((_n, ty)) = sdef.fields.get(*field as usize) {
                                                return ty.clone();
                                            }
                                        }
                                    }
                                }
                                _ => {}
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

    /// Emit Memset(0) for Null → aggregate destination.
    /// Handles both simple locals (SlotAddr + Memset) and projected fields (FieldPtr + Memset).
    /// Returns true if the destination is aggregate and was zero-filled.
    pub(super) fn try_null_memset(&mut self, dst: &Place, bb: BlockId) -> bool {
        use crate::lir::lower::types::c_sizeof_lir_type;

        // Determine the LIR type of the destination.
        let dst_ty = if dst.projections.is_empty() {
            let slot = self.local_to_slot[dst.local.0 as usize];
            self.lir_func.slots[slot.0 as usize].ty.clone()
        } else {
            // Projected destination — resolve through the projection chain.
            let gir_ty = match self.resolve_projected_gir_type(dst) {
                Some(ty) => ty,
                None => return false,
            };
            self.map_type(&gir_ty)
        };

        // Only aggregate types benefit from Memset — scalars and pointers
        // can use a normal NullPtr store.
        if !dst_ty.is_aggregate() { return false; }

        let size = c_sizeof_lir_type(&dst_ty, self.module_structs);
        if size == 0 { return false; }

        // Get the address of the destination.
        let ptr = if dst.projections.is_empty() {
            let slot = self.local_to_slot[dst.local.0 as usize];
            let addr = self.lir_func.next_value();
            self.push_inst(bb, Inst::SlotAddr { dst: addr, slot });
            addr
        } else {
            self.lower_place_addr(dst, bb)
        };

        // Emit: memset(ptr, 0, size)
        let zero = self.emit_i32_const(bb, 0);
        let sz = self.emit_i64_const(bb, size as i64);
        self.push_inst(bb, Inst::Memset { ptr, byte: zero, size: sz });
        true
    }

    /// Detect closure/function-ref → `GorgetClosure` slot and emit explicit
    /// ClosurePack.  Two cases:
    /// 1. `__Closure_N` local → heap-alloc env + memcpy + ClosurePack(needs_adapter=false)
    /// 2. `FuncRef` constant → NullPtr env + ClosurePack(needs_adapter=true)
    /// Returns `true` if handled.
    pub(super) fn try_closure_pack(
        &mut self,
        dst_local: ir::types::LocalId,
        value: &Operand,
        bb: BlockId,
    ) -> bool {
        use crate::lir::lower::types::c_sizeof_lir_type;

        let dst_idx = dst_local.0 as usize;
        if dst_idx >= self.local_to_slot.len() { return false; }
        let dst_slot = self.local_to_slot[dst_idx];
        let slot_ty = &self.lir_func.slots[dst_slot.0 as usize].ty;

        // Destination must be GorgetClosure (the runtime singleton) or one of
        // its `c_runtime_alias`-tagged Callable family monomorphizations
        // (`Callable__GorgetClosure`, `MutCallable__…`, etc.). Phase A
        // residual #2 (commit 629c13eb) gave the aliases their own StructDefs
        // with `c_runtime_alias = "GorgetClosure"`; without reading the alias
        // here, a SlotStore into a `Callable__GorgetClosure`-typed slot
        // (e.g. the temp materialised by `pack_closure_for_smart_ptr_ctor`
        // in front of `Shared__T__new`) would fall through to a raw memcpy
        // of the closure env struct rather than emitting a real ClosurePack
        // — exactly the bug we'd be trying to fix in the smart-pointer
        // constructor path. CLAUDE.md "layering discipline §3 (one source
        // of truth per axis)": resolve through `c_runtime_alias` first,
        // fall back to the literal name (mirrors the pattern already in
        // `wrap_single_closure_arg` at line 1413-1421 of this file).
        let is_closure_slot = match slot_ty {
            LirType::Struct(sid) => self.module_structs.get(sid.0 as usize)
                .map_or(false, |sd| {
                    sd.c_runtime_alias.as_deref() == Some("GorgetClosure")
                        || sd.name == "GorgetClosure"
                }),
            _ => false,
        };
        if !is_closure_slot { return false; }

        // Case 1: FuncRef constant → bare function ref, env = NULL.
        if let Operand::Constant(Constant::FuncRef(name)) = value {
            if let Some(&func_id) = self.func_index.get(name) {
                let null_env = self.lir_func.next_value();
                self.push_inst(bb, Inst::NullPtr { dst: null_env });
                self.push_inst(bb, Inst::ClosurePack {
                    slot: dst_slot,
                    env_ptr: null_env,
                    call_func: func_id,
                    needs_adapter: true,
                });
                return true;
            }
        }

        // Case 2: __Closure_N local → heap-alloc env + ClosurePack.
        let src_closure_name = match value {
            Operand::Copy(place) | Operand::Move(place) => {
                if !place.projections.is_empty() { return false; }
                let src_idx = place.local.0 as usize;
                if src_idx >= self.gir_func.locals.len() { return false; }
                let gir_ty = self.gir_func.locals[src_idx].type_id;
                match self.gir_types.get(gir_ty) {
                    Some(ir::types::GirType::Named(name)) if name.starts_with("__Closure_") =>
                        name.clone(),
                    _ => return false,
                }
            }
            _ => return false,
        };

        // Look up the __Closure_N__call function.
        let call_fn_name = format!("{src_closure_name}__call");
        let call_func = match self.func_index.get(&call_fn_name) {
            Some(&fid) => fid,
            None => return false,
        };

        // Get the source slot's LIR struct type for sizeof.
        let src_place = match value {
            Operand::Copy(place) | Operand::Move(place) => place,
            _ => unreachable!(),
        };
        let src_slot = self.local_to_slot[src_place.local.0 as usize];
        let src_ty = self.lir_func.slots[src_slot.0 as usize].ty.clone();
        let env_size = c_sizeof_lir_type(&src_ty, self.module_structs);

        // Emit: env_ptr = malloc(env_size)
        // Allocate via the closure-specific allocator that prefixes an 8-byte
        // size header. `gorget_closure_free` and `gorget_closure_clone_to_owned`
        // walk back to the header to recover the env size, so the GorgetClosure
        // value carries enough info for both deep clone and drop without
        // growing its 16-byte ABI (fn_ptr + env).
        let size_val = self.emit_i64_const(bb, env_size as i64);
        let heap_ptr = self.lir_func.next_value();
        self.ensure_extern("__gorget_closure_env_alloc", &[LirType::I64], &LirType::Ptr);
        let alloc_abis = self.lookup_arg_abis("__gorget_closure_env_alloc");
        self.push_inst(bb, Inst::CallExtern {
            dst: Some(heap_ptr),
            name: "__gorget_closure_env_alloc".to_string(),
            args: vec![size_val],
            arg_abis: alloc_abis,
        });

        // Emit: memcpy(env_ptr, &src_slot, env_size)
        let src_addr = self.lir_func.next_value();
        self.push_inst(bb, Inst::SlotAddr {
            dst: src_addr,
            slot: src_slot,
        });
        self.push_inst(bb, Inst::Memcpy {
            dst_ptr: heap_ptr,
            src_ptr: src_addr,
            size: size_val,
        });

        // Emit: ClosurePack { slot: dst_slot, env_ptr: heap_ptr, call_func }
        self.push_inst(bb, Inst::ClosurePack {
            slot: dst_slot,
            env_ptr: heap_ptr,
            call_func,
            needs_adapter: false,
        });

        true
    }

    /// Wrap closure args only at parameter positions that take an element by
    /// pointer (`AbiKind::VoidElem` — collection storage like `gorget_array_push`,
    /// `gorget_map_put`). The runtime memcpys `elem_size` bytes from the arg
    /// pointer into the slot, so for `Vector[Callable].push(closure)` the arg
    /// must already point at a packed `GorgetClosure` (16 bytes) rather than
    /// the source `__Closure_N` env struct. Other ABIs (struct-by-value,
    /// scalar, etc.) leave the arg untouched — combinators like `Result.map_err`
    /// take the closure as a struct value through their lowered wrapper, and
    /// wrapping there would replace the struct with a pointer and break the
    /// call.
    ///
    /// Picking the wrap site by parameter ABI rather than callee name avoids a
    /// brittle allow-list of "is this a collection-storage method"; new
    /// runtimes that adopt the `VoidElem` ABI participate automatically.
    pub(super) fn wrap_closure_args_at_void_elem(
        &mut self,
        gir_args: &[Operand],
        lir_args: &mut [ValueId],
        param_abis: &[crate::ir::abi::AbiKind],
        bb: BlockId,
    ) {
        use crate::ir::abi::AbiKind;
        for (i, abi) in param_abis.iter().enumerate() {
            if i >= gir_args.len() || i >= lir_args.len() { break; }
            if !matches!(abi, AbiKind::VoidElem) { continue; }
            self.wrap_single_closure_arg(i, &gir_args[i], lir_args, bb);
        }
    }

    /// Wrap one argument if it's a closure (`__Closure_N` local or `FuncRef`).
    /// No-op otherwise. Shared between the all-args wrapper used for user
    /// function calls and the ABI-filtered wrapper used for extern calls.
    pub(super) fn wrap_single_closure_arg(
        &mut self,
        i: usize,
        gir_arg: &Operand,
        lir_args: &mut [ValueId],
        bb: BlockId,
    ) {
        use crate::lir::lower::types::c_sizeof_lir_type;

        // Case 1: FuncRef constant → bare function ref, env = NULL.
        if let Operand::Constant(Constant::FuncRef(name)) = gir_arg {
            if let Some(&func_id) = self.func_index.get(name) {
                let gc_sid = match self.struct_reg.lookup("GorgetClosure") {
                    Some(sid) => sid,
                    None => return,
                };
                let tmp_slot = self.lir_func.add_slot(LirType::Struct(gc_sid), None);
                let null_env = self.lir_func.next_value();
                self.push_inst(bb, Inst::NullPtr { dst: null_env });
                self.push_inst(bb, Inst::ClosurePack {
                    slot: tmp_slot,
                    env_ptr: null_env,
                    call_func: func_id,
                    needs_adapter: true,
                });
                let addr = self.lir_func.next_value();
                self.push_inst(bb, Inst::SlotAddr {
                    dst: addr,
                    slot: tmp_slot,
                });
                lir_args[i] = addr;
                return;
            }
        }

        // Case 2a: pre-packed `Callable` local (`GirType::FnPtr` or
        // `Named("Callable__…")`) — already a 16-byte GorgetClosure with a
        // heap-alloc'd env. The collection is about to take ownership, so
        // deep-clone the closure (fresh env via `gorget_closure_clone_to_owned`,
        // size-prefix preserved) and pass a pointer to the cloned slot.
        // Without this the source local and the slot share the same env
        // pointer; both then drop and double-free (e.g. router.route stores
        // a Callable param into a Dict — both the local and the Dict slot
        // would call gorget_closure_free on the same allocation).
        if let Operand::Copy(place) | Operand::Move(place) = gir_arg {
            if place.projections.is_empty() {
                let src_idx = place.local.0 as usize;
                if src_idx < self.gir_func.locals.len() {
                    let gir_ty = self.gir_func.locals[src_idx].type_id;
                    let is_packed_callable = match self.gir_types.get(gir_ty) {
                        Some(ir::types::GirType::FnPtr { .. }) => true,
                        Some(ir::types::GirType::Named(n)) => {
                            // Phase A residual #1: typed read via the GIR
                            // TypeDef metadata (or the LIR StructDef as a
                            // fallback for cross-module / mangling-only paths
                            // that bypass `register_callable_alias`). Both
                            // produce `c_runtime_alias = "GorgetClosure"`.
                            self.gir_types.get_type_def(n)
                                .and_then(|td| td.metadata.c_runtime_alias.as_deref())
                                == Some("GorgetClosure")
                            || self.struct_reg.lookup(n)
                                .and_then(|sid| self.module_structs.get(sid.0 as usize))
                                .and_then(|sd| sd.c_runtime_alias.as_deref())
                                == Some("GorgetClosure")
                        }
                        _ => false,
                    };
                    if is_packed_callable {
                        let gc_sid = match self.struct_reg.lookup("GorgetClosure") {
                            Some(sid) => sid,
                            None => return,
                        };
                        let tmp_slot = self.lir_func.add_slot(LirType::Struct(gc_sid), None);
                        let src_slot = self.local_to_slot[src_idx];
                        let src_addr = self.lir_func.next_value();
                        self.push_inst(bb, Inst::SlotAddr {
                            dst: src_addr, slot: src_slot,
                        });
                        let cloned = self.lir_func.next_value();
                        self.ensure_extern(
                            "gorget_closure_clone_to_owned",
                            &[LirType::Ptr],
                            &LirType::Struct(gc_sid),
                        );
                        let abis = self.lookup_arg_abis("gorget_closure_clone_to_owned");
                        self.push_inst(bb, Inst::CallExtern {
                            dst: Some(cloned),
                            name: "gorget_closure_clone_to_owned".to_string(),
                            args: vec![src_addr],
                            arg_abis: abis,
                        });
                        self.push_inst(bb, Inst::SlotStore {
                            slot: tmp_slot, value: cloned, is_move: false,
                        });
                        let addr = self.lir_func.next_value();
                        self.push_inst(bb, Inst::SlotAddr {
                            dst: addr, slot: tmp_slot,
                        });
                        lir_args[i] = addr;
                        return;
                    }
                }
            }
        }

        // Case 2: __Closure_N local → heap-alloc env + ClosurePack.
        let closure_name = match gir_arg {
            Operand::Copy(place) | Operand::Move(place) => {
                if !place.projections.is_empty() { return; }
                let src_idx = place.local.0 as usize;
                if src_idx >= self.gir_func.locals.len() { return; }
                let gir_ty = self.gir_func.locals[src_idx].type_id;
                match self.gir_types.get(gir_ty) {
                    Some(ir::types::GirType::Named(name)) if name.starts_with("__Closure_") =>
                        name.clone(),
                    _ => return,
                }
            }
            _ => return,
        };

        let call_fn_name = format!("{closure_name}__call");
        let call_func = match self.func_index.get(&call_fn_name) {
            Some(&fid) => fid,
            None => return,
        };

        let gc_sid = match self.struct_reg.lookup("GorgetClosure") {
            Some(sid) => sid,
            None => return,
        };
        let tmp_slot = self.lir_func.add_slot(LirType::Struct(gc_sid), None);

        let src_place = match gir_arg {
            Operand::Copy(place) | Operand::Move(place) => place,
            _ => unreachable!(),
        };
        let src_slot = self.local_to_slot[src_place.local.0 as usize];
        let src_ty = self.lir_func.slots[src_slot.0 as usize].ty.clone();
        let env_size = c_sizeof_lir_type(&src_ty, self.module_structs);

        // Allocate via the closure-specific allocator that prefixes an 8-byte
        // size header. `gorget_closure_free` and `gorget_closure_clone_to_owned`
        // walk back to the header to recover the env size, so the GorgetClosure
        // value carries enough info for both deep clone and drop without
        // growing its 16-byte ABI (fn_ptr + env).
        let size_val = self.emit_i64_const(bb, env_size as i64);
        let heap_ptr = self.lir_func.next_value();
        self.ensure_extern("__gorget_closure_env_alloc", &[LirType::I64], &LirType::Ptr);
        let alloc_abis = self.lookup_arg_abis("__gorget_closure_env_alloc");
        self.push_inst(bb, Inst::CallExtern {
            dst: Some(heap_ptr),
            name: "__gorget_closure_env_alloc".to_string(),
            args: vec![size_val],
            arg_abis: alloc_abis,
        });

        let src_addr = self.lir_func.next_value();
        self.push_inst(bb, Inst::SlotAddr {
            dst: src_addr,
            slot: src_slot,
        });
        self.push_inst(bb, Inst::Memcpy {
            dst_ptr: heap_ptr,
            src_ptr: src_addr,
            size: size_val,
        });

        self.push_inst(bb, Inst::ClosurePack {
            slot: tmp_slot,
            env_ptr: heap_ptr,
            call_func,
            needs_adapter: false,
        });
        let addr = self.lir_func.next_value();
        self.push_inst(bb, Inst::SlotAddr {
            dst: addr,
            slot: tmp_slot,
        });
        lir_args[i] = addr;
    }

    /// Wrap closure/function-ref arguments at call sites into GorgetClosure slots.
    ///
    /// For each argument that is a `__Closure_N` struct or a `FuncRef` constant,
    /// emits ClosurePack into a temporary GorgetClosure slot and replaces the
    /// argument with a pointer to that slot.  This lifts closure→callable wrapping
    /// from backends into LIR.
    pub(super) fn wrap_closure_call_args(
        &mut self,
        gir_args: &[Operand],
        lir_args: &mut [ValueId],
        bb: BlockId,
    ) {
        for (i, gir_arg) in gir_args.iter().enumerate() {
            if i >= lir_args.len() { break; }
            self.wrap_single_closure_arg(i, gir_arg, lir_args, bb);
        }
    }
}
