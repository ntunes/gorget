//! Instruction and terminator lowering for FuncLowering.
//!
//! Extracted from mod.rs — covers lower_instruction, lower_terminator,
//! mark_inline_c_referenced_slots, lower_operand, operand_is_str,
//! emit_extern_call, lower_printf_args, and lower_place_load.

use super::*;

impl<'a> FuncLowering<'a> {
    pub(super) fn lower_instruction(&mut self, inst: &Instruction, bb: BlockId) {
        match inst {
            Instruction::Assign { mode, dst, value, .. } => {
                // Special-case: Constant::Null assigned to an enum-typed local.
                if let Operand::Constant(Constant::Null) = value {
                    if let Some(()) = self.try_materialize_null_for_assign(dst, bb) {
                        return;
                    }
                }
                // Special-case: Option/Result source → non-Option/Result dest.
                if let Some(val) = self.try_enum_payload_extract(dst, value, bb) {
                    self.store_to_place(dst, val, bb);
                    return;
                }
                // Special-case: Box[Trait] ← Box[Concrete] trait object construction.
                if self.try_trait_object_construct(dst, value, bb) {
                    return;
                }
                let is_move = matches!(mode, ir::instructions::AssignMode::Move);
                let val = self.lower_operand(value, bb);
                if is_move && dst.projections.is_empty() {
                    // Move: emit SlotStore with is_move flag so C backend can use
                    // memcpy instead of clone for resource types (strings, etc.).
                    self.ensure_local(dst.local);
                    let slot = self.local_to_slot[dst.local.0 as usize];
                    self.lir_func.block_mut(bb).insts.push(Inst::SlotStore {
                        slot, value: val, is_move: true,
                    });
                } else {
                    self.store_to_place(dst, val, bb);
                }
            }

            Instruction::BinOp {
                dst,
                op,
                type_id,
                lhs,
                rhs,
            } => {
                let l = self.lower_operand(lhs, bb);
                let r = self.lower_operand(rhs, bb);

                // Check for Vector + Vector → clone lhs then extend with rhs
                let is_vector_add = *op == GirBinOp::Add && matches!(
                    self.gir_types.get(*type_id),
                    Some(GirType::Named(name)) if name.starts_with("Vector__")
                );

                if is_vector_add {
                    // Emit: result = gorget_array_clone(&lhs); gorget_array_extend(&result, &rhs);
                    // The c_lir backend handles &-address-of for array functions via
                    // takes_array_ptr_args / collection_self_by_ptr.
                    let result = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: Some(result),
                        name: "gorget_array_clone".to_string(),
                        args: vec![l],
                        original_name: None,
                    });
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: None,
                        name: "gorget_array_extend".to_string(),
                        args: vec![result, r],
                        original_name: None,
                    });
                    self.store_to_local(*dst, result, bb);
                } else {
                    let result = self.lir_func.next_value();
                    let ty = self.map_type(type_id);
                    let inst = lower_binop(result, *op, l, r, ty, self.overflow_wrap);
                    self.lir_func.block_mut(bb).insts.push(inst);
                    self.store_to_local(*dst, result, bb);
                }
            }

            Instruction::UnOp {
                dst,
                op,
                type_id,
                operand,
            } => {
                let val = self.lower_operand(operand, bb);
                let result = self.lir_func.next_value();
                let ty = self.map_type(type_id);
                let inst = lower_unop(result, *op, val, ty);
                self.lir_func.block_mut(bb).insts.push(inst);
                self.store_to_local(*dst, result, bb);
            }

            Instruction::Cmp {
                dst,
                op,
                type_id: _,
                lhs,
                rhs,
            } => {
                let l = self.lower_operand(lhs, bb);
                let r = self.lower_operand(rhs, bb);
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::Cmp {
                    dst: result,
                    op: map_cmp_op(*op),
                    lhs: l,
                    rhs: r,
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::Cast {
                dst,
                target_type,
                value,
            } => {
                let val = self.lower_operand(value, bb);
                let to = self.map_type(target_type);

                // Check if target is Str — emit conversion call instead of invalid (Str)(val) cast.
                let is_str_target = matches!(&to, LirType::Struct(sid) if {
                    self.module_structs.get(sid.0 as usize)
                        .map_or(false, |s| s.name == "GorgetString")
                });
                if is_str_target {
                    // Determine source GIR type to pick the right conversion function.
                    let src_gir_ty = match value {
                        Operand::Copy(place) | Operand::Move(place) => {
                            let idx = place.local.0 as usize;
                            if idx < self.gir_func.locals.len() {
                                Some(self.gir_func.locals[idx].type_id)
                            } else {
                                None
                            }
                        }
                        Operand::Constant(c) => match c {
                            Constant::I8(_) | Constant::I16(_) | Constant::I32(_) | Constant::I64(_)
                            | Constant::U8(_) | Constant::U16(_) | Constant::U32(_) | Constant::U64(_)
                            | Constant::SizeOf(_) => Some(gir_types::I64_TYPE),
                            Constant::F32(_) | Constant::F64(_) => Some(gir_types::F64_TYPE),
                            Constant::Bool(_) => Some(gir_types::BOOL_TYPE),
                            _ => None,
                        },
                    };
                    let is_int = src_gir_ty.map_or(false, |t| {
                        t == gir_types::I64_TYPE || t == gir_types::I32_TYPE
                        || t == gir_types::I16_TYPE || t == gir_types::I8_TYPE
                        || t == gir_types::U8_TYPE || t == gir_types::U16_TYPE
                        || t == gir_types::U32_TYPE || t == gir_types::U64_TYPE
                    });
                    let is_float = src_gir_ty.map_or(false, |t| {
                        t == gir_types::F64_TYPE || t == gir_types::F32_TYPE
                    });
                    let is_bool = src_gir_ty.map_or(false, |t| t == gir_types::BOOL_TYPE);
                    let is_ptr = src_gir_ty.map_or(false, |t| {
                        self.gir_types.get(t).map_or(false, |gt| matches!(gt, GirType::Ptr(_) | GirType::MutPtr(_)))
                    });

                    if is_ptr {
                        // Ptr source (const char*) → GorgetString: wrap directly with gorget_str_from_cstr.
                        let str_ty = self.struct_reg.lookup("GorgetString")
                            .map(LirType::Struct).unwrap_or(LirType::Ptr);
                        let cstr_result = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                            dst: Some(cstr_result),
                            name: "gorget_str_from_cstr".to_string(),
                            args: vec![val],
                            original_name: None,
                        });
                        self.ensure_extern("gorget_str_from_cstr", &[LirType::Ptr], &str_ty);
                        self.store_to_local(*dst, cstr_result, bb);
                    } else {
                    let conv_fn = if is_int {
                        "gorget_int_to_str"
                    } else if is_float {
                        "gorget_float_to_str"
                    } else if is_bool {
                        "gorget_bool_to_str"
                    } else {
                        // Unknown source → use int_to_str as fallback (most casts are int→str).
                        "gorget_int_to_str"
                    };
                    // Emit CallExtern to the conversion function (returns const char*).
                    let cstr_result = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: Some(cstr_result),
                        name: conv_fn.to_string(),
                        args: vec![val],
                        original_name: None,
                    });
                    let str_ty = if let Some(sid) = self.struct_reg.lookup("Str") { LirType::Struct(sid) } else { LirType::Ptr };
                    self.ensure_extern(conv_fn, &[if is_float { LirType::F64 } else if is_bool { LirType::Bool } else { LirType::I64 }], &str_ty);
                    // The result is a Str struct (returned by gorget_string_adopt in the C runtime).
                    self.store_to_local(*dst, cstr_result, bb);
                    } // close else (non-ptr) branch
                } else if matches!(to, LirType::Void) {
                    // Cast to void — just evaluate for side effects, don't generate (void)(val).
                    // No store needed.
                } else {
                    let result = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::IntCast {
                        dst: result,
                        value: val,
                        to,
                    });
                    self.store_to_local(*dst, result, bb);
                }
            }

            Instruction::BitCast {
                dst,
                target_type,
                value,
            } => {
                let val = self.lower_operand(value, bb);
                let to = self.map_type(target_type);
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::Bitcast {
                    dst: result,
                    value: val,
                    to,
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::PtrCast { dst, value, .. } => {
                let val = self.lower_operand(value, bb);
                let result = self.lir_func.next_value();
                self.lir_func
                    .block_mut(bb)
                    .insts
                    .push(Inst::PtrCast { dst: result, value: val });
                self.store_to_local(*dst, result, bb);
            }

            // -- Calls --
            Instruction::Call { dst, func, args, .. } => {
                if let Some(fid) = self.func_index.get(func) {
                    let lir_args: Vec<ValueId> =
                        args.iter().map(|a| self.lower_operand(a, bb)).collect();
                    let result = dst.map(|_| self.lir_func.next_value());
                    self.lir_func.block_mut(bb).insts.push(Inst::Call {
                        dst: result,
                        func: *fid,
                        args: lir_args,
                    });
                    if let (Some(d), Some(r)) = (*dst, result) {
                        self.store_to_local(d, r, bb);
                    }
                } else {
                    // Unknown function — treat as extern.
                    // Map monomorphized collection/method names to runtime function names.
                    let emit_name = map_monomorphized_to_runtime_with_table(func, self.runtime_callees)
                        .unwrap_or_else(|| func.clone());
                    // For collection/concurrency methods that take self by pointer,
                    // if the first arg is a GlobalRef, emit GlobalAddr (pointer)
                    // instead of GlobalAddr+Load (copy), so mutations affect the global.
                    let needs_self_by_ptr = is_self_by_ptr_method(func);
                    let lir_args: Vec<ValueId> =
                        args.iter().enumerate().map(|(i, a)| {
                            if i == 0 && needs_self_by_ptr {
                                if let Operand::Constant(Constant::GlobalRef(name)) = a {
                                    if let Some(&gid) = self.global_index.get(name) {
                                        let addr = self.lir_func.next_value();
                                        self.lir_func.block_mut(bb).insts.push(
                                            Inst::GlobalAddr { dst: addr, global: gid },
                                        );
                                        return addr;
                                    }
                                }
                            }
                            // Null arg to collection push/set/send → properly tagged enum slot
                            if matches!(a, Operand::Constant(Constant::Null)) && i > 0 {
                                if let Some(slot_addr) = self.materialize_null_enum_for_collection_arg(func, bb) {
                                    return slot_addr;
                                }
                            }
                            self.lower_operand(a, bb)
                        }).collect();
                    // Dispatch abs/min/max to float variants when args are float.
                    let emit_name = if matches!(emit_name.as_str(), "gorget_abs" | "gorget_min" | "gorget_max") {
                        let has_float_arg = args.iter().any(|a| {
                            matches!(self.operand_lir_type(a), LirType::F32 | LirType::F64)
                        });
                        if has_float_arg {
                            match emit_name.as_str() {
                                "gorget_abs" => "gorget_fabs".to_string(),
                                "gorget_min" => "gorget_fmin".to_string(),
                                "gorget_max" => "gorget_fmax".to_string(),
                                _ => emit_name,
                            }
                        } else { emit_name }
                    } else { emit_name };
                    let mut lir_args = lir_args;
                    // Type-aware dispatch for bare `len` free function
                    let mut len_handled = false;
                    let emit_name = if func == "len" && args.len() == 1 {
                        let arg_type = self.operand_gir_type_name(&args[0]);
                        if arg_type.as_deref().map_or(false, |n| n.starts_with("Vector__") || n == "GorgetArray") {
                            "gorget_array_len".to_string()
                        } else if arg_type.as_deref().map_or(false, |n| n.starts_with("Dict__") || n.starts_with("HashMap__") || n == "GorgetMap") {
                            "gorget_map_len".to_string()
                        } else if arg_type.as_deref().map_or(false, |n| n.starts_with("Set__") || n.starts_with("HashSet__") || n == "GorgetSet") {
                            "gorget_set_len".to_string()
                        } else if arg_type.as_deref().map_or(false, |n| n == "str" || n == "GorgetString") {
                            "gorget_str_codepoint_count".to_string()
                        } else if arg_type.as_deref().map_or(false, |n| n == "String" || n == "GorgetString") {
                            "gorget_str_codepoint_count".to_string()
                        } else if let Some(type_name) = arg_type.as_deref() {
                            // User type: dispatch to TypeName__len as a direct Call if available
                            let method_name = format!("{type_name}__len");
                            if let Some(&fid) = self.func_index.get(method_name.as_str()) {
                                let result = dst.map(|_| self.lir_func.next_value());
                                self.lir_func.block_mut(bb).insts.push(Inst::Call {
                                    dst: result,
                                    func: fid,
                                    args: lir_args.clone(),
                                });
                                if let (Some(d), Some(r)) = (*dst, result) {
                                    self.store_to_local(d, r, bb);
                                }
                                len_handled = true;
                            }
                            method_name
                        } else {
                            emit_name
                        }
                    } else {
                        emit_name
                    };
                    // gorget_regex_find/split take 3 args but GIR only passes 2 — inject default 0
                    if (emit_name == "gorget_regex_find" || emit_name == "gorget_regex_split") && lir_args.len() == 2 {
                        let zero_val = self.emit_i64_const(bb, 0);
                        lir_args.push(zero_val);
                    }
                    // Delegate to the shared extern-call emitter (same logic as CallExtern).
                    if !len_handled {
                        self.emit_extern_call(func, &emit_name, dst, args, lir_args, bb);
                    }
                }
            }

            Instruction::CallExtern { dst, func, args } => {
                // If the callee is actually a defined function in this module (GIR uses
                // call_extern for user-defined iterator/trait methods), emit a direct Call.
                if let Some(fid) = self.func_index.get(func) {
                    let lir_args: Vec<ValueId> =
                        args.iter().map(|a| self.lower_operand(a, bb)).collect();
                    let result = dst.map(|_| self.lir_func.next_value());
                    self.lir_func.block_mut(bb).insts.push(Inst::Call {
                        dst: result,
                        func: *fid,
                        args: lir_args,
                    });
                    if let (Some(d), Some(r)) = (*dst, result) {
                        self.store_to_local(d, r, bb);
                    }
                } else {
                // Remap monomorphized names to runtime equivalents
                // (e.g., Vector__int64_t__push → gorget_array_push).
                let mut emit_name = map_monomorphized_to_runtime_with_table(func, self.runtime_callees)
                    .unwrap_or_else(|| func.clone());
                // Dispatch abs/min/max to float variants (fabs/fmin/fmax) when args are float.
                if matches!(emit_name.as_str(), "gorget_abs" | "gorget_min" | "gorget_max") {
                    let has_float_arg = args.iter().any(|a| {
                        let ty = self.operand_lir_type(a);
                        matches!(ty, LirType::F32 | LirType::F64)
                    });
                    if has_float_arg {
                        emit_name = match emit_name.as_str() {
                            "gorget_abs" => "gorget_fabs".to_string(),
                            "gorget_min" => "gorget_fmin".to_string(),
                            "gorget_max" => "gorget_fmax".to_string(),
                            _ => emit_name,
                        };
                    }
                }
                let is_printf_like = emit_name == "printf" || emit_name == "fprintf_stderr"
                    || emit_name == "gorget_string_format" || emit_name == "gorget_string_format_alloc"
                    || emit_name == "snprintf" || emit_name == "sprintf";
                let lir_args: Vec<ValueId> = if is_printf_like {
                    // For printf, expand Str-typed args into (int)len, data pairs.
                    self.lower_printf_args(args, bb)
                } else {
                    {
                    // For collection/concurrency methods that take self by pointer,
                    // if the first arg is a GlobalRef, emit GlobalAddr (pointer)
                    // instead of GlobalAddr+Load (copy), so mutations affect the global.
                    let needs_self_by_ptr = is_self_by_ptr_method(func);
                    args.iter().enumerate().map(|(i, a)| {
                        if i == 0 && needs_self_by_ptr {
                            if let Operand::Constant(Constant::GlobalRef(name)) = a {
                                if let Some(&gid) = self.global_index.get(name) {
                                    let addr = self.lir_func.next_value();
                                    self.lir_func.block_mut(bb).insts.push(
                                        Inst::GlobalAddr { dst: addr, global: gid },
                                    );
                                    return addr;
                                }
                            }
                        }
                        // Null arg to collection push/set/send → create a properly tagged
                        // enum slot (e.g. None for Option) and pass its address, instead of
                        // passing a raw NULL pointer that would crash memcpy in the runtime.
                        if matches!(a, Operand::Constant(Constant::Null)) && i > 0 {
                            if let Some(slot_addr) = self.materialize_null_enum_for_collection_arg(func, bb) {
                                return slot_addr;
                            }
                        }
                        self.lower_operand(a, bb)
                    }).collect()
                    }
                };
                self.emit_extern_call(func, &emit_name, dst, args, lir_args, bb);
                }
            }

            Instruction::CallIndirect { dst, callee, args } => {
                let callee_val = self.lower_operand(callee, bb);
                let lir_args: Vec<ValueId> =
                    args.iter().map(|a| self.lower_operand(a, bb)).collect();
                let result = dst.map(|_| self.lir_func.next_value());
                self.lir_func.block_mut(bb).insts.push(Inst::CallPtr {
                    dst: result,
                    callee: callee_val,
                    args: lir_args,
                });
                if let (Some(d), Some(r)) = (*dst, result) {
                    self.store_to_local(d, r, bb);
                }
            }

            // -- Struct/aggregate init --
            Instruction::StructInit {
                dst,
                type_name,
                fields,
            } => {
                // Get or create the struct type.
                let struct_id = self
                    .struct_reg
                    .lookup(type_name)
                    .unwrap_or(StructId(0)); // fallback

                let slot = self.local_to_slot[dst.0 as usize];
                let base = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                    dst: base,
                    slot,
                });

                // Look up struct field types for Null → enum promotion.
                let field_type_ids: Vec<Option<GirTypeId>> = self.gir_types.get_type_def(type_name)
                    .and_then(|td| {
                        if let gir_types::TypeDefKind::Struct(sd) = &td.kind {
                            Some(sd.fields.iter().map(|f| Some(f.type_id)).collect())
                        } else { None }
                    })
                    .unwrap_or_else(|| vec![None; fields.len()]);

                for (i, field_op) in fields.iter().enumerate() {
                    // Special-case: Null operand for an enum-typed field (e.g. Option<T> = None).
                    // Instead of emitting NullPtr (memcpy from NULL → segfault), properly
                    // initialize the field with the null variant tag.
                    if matches!(field_op, Operand::Constant(Constant::Null)) {
                        if let Some(Some(fty)) = field_type_ids.get(i) {
                            if let Some((field_enum_sid, tag_ordinal)) = self.find_enum_null_variant(*fty) {
                                // The parent struct slot is zero-initialized (= {0}), so the
                                // payload bytes are already zero.  We only need to set the tag
                                // to the null-variant ordinal (e.g. None=1).
                                let fptr = self.lir_func.next_value();
                                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                                    dst: fptr,
                                    base,
                                    struct_id,
                                    field: i as u32,
                                });
                                let tag_val = self.emit_i32_const(bb, tag_ordinal as i64);
                                let tag_ptr = self.lir_func.next_value();
                                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                                    dst: tag_ptr,
                                    base: fptr,
                                    struct_id: field_enum_sid,
                                    field: 0,
                                });
                                self.lir_func.block_mut(bb).insts.push(Inst::Store {
                                    ptr: tag_ptr,
                                    value: tag_val,
                                });
                                continue;
                            }
                        }
                    }

                    let val = self.lower_operand(field_op, bb);
                    let fptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: fptr,
                        base,
                        struct_id,
                        field: i as u32,
                    });
                    self.lir_func.block_mut(bb).insts.push(Inst::Store {
                        ptr: fptr,
                        value: val,
                    });
                }
            }

            Instruction::FieldLoad {
                dst,
                base,
                field,
                ..
            } => {
                let mut base_val = self.lower_place_addr(base, bb);
                // Use effective type after base projections (e.g., Deref→Field chain).
                let effective_type = self.effective_place_type(base);
                // If the effective type is a pointer (e.g., closure env param),
                // load the pointer value first so FieldPtr operates on the struct, not the slot.
                // Skip for ref_locals — they're already pointers from collection reads;
                // lower_place_addr already does the SlotLoad to get the pointer value.
                let is_ref_local = base.projections.is_empty()
                    && self.gir_func.ref_locals.contains(&base.local);
                if !is_ref_local && matches!(self.gir_types.get(effective_type), Some(GirType::Ptr(_) | GirType::MutPtr(_))) {
                    let deref = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: deref,
                        ptr: base_val,
                        ty: LirType::Ptr,
                    });
                    base_val = deref;
                }
                let struct_id = self.resolve_struct_id_for_field(effective_type, *field, self.module_structs);
                let fptr = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                    dst: fptr,
                    base: base_val,
                    struct_id,
                    field: *field,
                });
                // If destination is Ptr(T), return field address as pointer reference.
                let dst_gir_type = self.gir_func.locals[dst.0 as usize].type_id;
                if matches!(self.gir_types.get(dst_gir_type), Some(GirType::Ptr(_))) {
                    self.store_to_local(*dst, fptr, bb);
                } else {
                    let field_ty = self.resolve_field_type(effective_type, *field);
                    // If field is Ptr but dst is a value type (Str), double-deref:
                    // load Ptr from field, then load Str value through Ptr.
                    let dst_slot = self.local_to_slot[dst.0 as usize];
                    let dst_slot_ty = self.lir_func.slots[dst_slot.0 as usize].ty.clone();
                    if matches!(field_ty, LirType::Ptr) && dst_slot_ty.is_aggregate() {
                        let ptr_val = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::Load {
                            dst: ptr_val, ptr: fptr, ty: LirType::Ptr,
                        });
                        let result = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::Load {
                            dst: result, ptr: ptr_val, ty: dst_slot_ty,
                        });
                        self.store_to_local(*dst, result, bb);
                    } else {
                        let result = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::Load {
                            dst: result,
                            ptr: fptr,
                            ty: field_ty,
                        });
                        self.store_to_local(*dst, result, bb);
                    }
                }
            }

            Instruction::IndexLoad { dst, base, index } => {
                // Determine base type name and index type to dispatch appropriately.
                let base_type = self.effective_place_type(base);
                let base_type_name = self.resolve_type_name(base_type);
                let idx_type_name = match index {
                    Operand::Copy(p) | Operand::Move(p) => {
                        let ity = self.gir_func.locals[p.local.0 as usize].type_id;
                        self.resolve_type_name(ity)
                    }
                    _ => String::new(),
                };
                let is_range = idx_type_name == "GorgetRange";
                let is_str = base_type_name == "GorgetString";
                let is_array = base_type_name.starts_with("Vector__")
                    || base_type_name == "GorgetArray";
                let is_dict = base_type_name.starts_with("Dict__")
                    || base_type_name.starts_with("GorgetMap")
                    || base_type_name.starts_with("HashMap__");

                if (is_str || is_array) && is_range {
                    // Str[range] → gorget_str_slice(str, start, end)
                    // Vector[range] → gorget_array_slice(&arr, start, end)
                    let base_val = self.lower_place_addr(base, bb);
                    let range_place = match index {
                        Operand::Copy(p) | Operand::Move(p) => p,
                        _ => unreachable!(),
                    };
                    let range_val = self.lower_place_addr(range_place, bb);
                    let range_sid = self.struct_reg.lookup("GorgetRange").unwrap_or(StructId(0));
                    let start_ptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: start_ptr, base: range_val, struct_id: range_sid, field: 0,
                    });
                    let start = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: start, ptr: start_ptr, ty: LirType::I64,
                    });
                    let end_ptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: end_ptr, base: range_val, struct_id: range_sid, field: 1,
                    });
                    let end = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: end, ptr: end_ptr, ty: LirType::I64,
                    });
                    let fn_name = if is_str { "gorget_str_slice" } else { "gorget_array_slice" };
                    let dst_gir_ty = self.gir_func.locals[dst.0 as usize].type_id;
                    let ret_ty = self.map_type(&dst_gir_ty);
                    let str_ty = self.struct_reg.lookup("GorgetString")
                        .map(LirType::Struct).unwrap_or(LirType::Ptr);
                    let arg_types = if is_str {
                        vec![str_ty, LirType::I64, LirType::I64]
                    } else {
                        vec![LirType::Ptr, LirType::I64, LirType::I64]
                    };
                    self.ensure_extern(fn_name, &arg_types, &ret_ty);
                    let result = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: Some(result),
                        name: fn_name.to_string(),
                        args: vec![base_val, start, end],
                        original_name: None,
                    });
                    self.store_to_local(*dst, result, bb);
                } else if is_str {
                    // Str[int] → gorget_str_index(str, idx)
                    let base_val = self.lower_place_addr(base, bb);
                    let idx = self.lower_operand(index, bb);
                    let str_ty = self.struct_reg.lookup("GorgetString")
                        .map(LirType::Struct).unwrap_or(LirType::Ptr);
                    // Return type is Str by value (the C function returns Str, not Ptr).
                    self.ensure_extern("gorget_str_index", &[str_ty.clone(), LirType::I64], &str_ty);
                    let result = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: Some(result),
                        name: "gorget_str_index".to_string(),
                        args: vec![base_val, idx],
                        original_name: None,
                    });
                    self.store_to_local(*dst, result, bb);
                } else if is_array || is_dict {
                    // Vector[int] → gorget_array_get(&arr, idx)
                    // Dict[key] → gorget_map_get(&map, &key)
                    let mut base_val = self.lower_place_addr(base, bb);
                    // If base is Ptr-typed (field load ref) but NOT a ref_local (borrowed param),
                    // deref to get the actual collection pointer. ref_locals already get SlotLoad
                    // in lower_place_addr, so base_val is the pointer value — no extra deref needed.
                    let base_gir = self.gir_func.locals[base.local.0 as usize].type_id;
                    let is_ref_local = self.gir_func.ref_locals.contains(&base.local);
                    if matches!(self.gir_types.get(base_gir), Some(GirType::Ptr(_)))
                        && base.projections.is_empty()
                        && !is_ref_local
                    {
                        let deref = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::Load {
                            dst: deref, ptr: base_val, ty: LirType::Ptr,
                        });
                        base_val = deref;
                    }
                    let idx = self.lower_operand(index, bb);
                    let fn_name = if is_dict { "gorget_map_get" } else { "gorget_array_get" };
                    self.ensure_extern(fn_name, &[LirType::Ptr, LirType::I64], &LirType::Ptr);
                    let ptr_val = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: Some(ptr_val),
                        name: fn_name.to_string(),
                        args: vec![base_val, idx],
                        original_name: None,
                    });
                    // gorget_array_get / gorget_map_get return void* pointing to the element.
                    // If dst is Ptr(T), return the raw pointer (borrowed reference).
                    let dst_gir_type = self.gir_func.locals[dst.0 as usize].type_id;
                    if matches!(self.gir_types.get(dst_gir_type), Some(GirType::Ptr(_))) {
                        // Mark Ptr(Str) element reads for C backend deref decisions.
                        if let Some(GirType::Ptr(inner)) = self.gir_types.get(dst_gir_type) {
                            if let Some(GirType::Named(name)) = self.gir_types.get(*inner) {
                                if name == "GorgetString" {
                                    self.lir_func.str_ptr_values.insert(ptr_val);
                                }
                            }
                        }
                        self.store_to_local(*dst, ptr_val, bb);
                        return;
                    }
                    // Otherwise dereference to get the actual element value.
                    let dst_slot = self.local_to_slot[dst.0 as usize];
                    let mut elem_ty = self.lir_func.slots[dst_slot.0 as usize].ty.clone();
                    // Closures are 16 bytes (GorgetClosure) but may be typed as I64 in LIR.
                    // Fix: re-derive from GIR type with struct registry to get the correct
                    // struct type, so Load reads the full closure (not just 8 bytes).
                    // Closures are 16 bytes (GorgetClosure) but typed as I64 in GIR/LIR.
                    // When reading from a collection of closures, the Load with I64 reads
                    // only 8 bytes (fn_ptr), corrupting subsequent memcpy of the full closure.
                    // Fix: detect closure-element collections by base type name and use
                    // the GorgetClosure struct type instead, so Load reads full 16 bytes.
                    if matches!(elem_ty, LirType::I64) && (
                        base_type_name.contains("Callable") || base_type_name.contains("FnPtr")
                    ) {
                        if let Some(sid) = self.struct_reg.lookup("GorgetClosure") {
                            elem_ty = LirType::Struct(sid);
                        }
                    }
                    // Determine element type name for clone/drop decisions.
                    let elem_type_name = base_type_name
                        .strip_prefix("Vector__")
                        .or_else(|| base_type_name.strip_prefix("Deque__"))
                        .or_else(|| {
                            // Dict__K__V → value type is everything after first "__" past key
                            let rest = base_type_name.strip_prefix("Dict__")
                                .or_else(|| base_type_name.strip_prefix("HashMap__"))?;
                            let idx = rest.find("__")?;
                            Some(&rest[idx + 2..])
                        })
                        .unwrap_or("");

                    // For collection/string elements (Vector, Dict, Set, Str), clone
                    // instead of move+zero so the parent collection retains the original.
                    // Other resource types (Task, user structs) are still moved+zeroed
                    // since they may be intentionally consumed (e.g., task.await()).
                    let clone_fn = clone_fn_for_collection_element(elem_type_name);

                    if let Some(clone_fn_name) = clone_fn {
                        // Clone: call gorget_*_clone(elem_ptr) → new deep copy
                        let ret_ty = elem_ty.clone();
                        self.ensure_extern(clone_fn_name, &[LirType::Ptr], &ret_ty);
                        let result = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                            dst: Some(result),
                            name: clone_fn_name.to_string(),
                            args: vec![ptr_val],
                            original_name: None,
                        });
                        self.store_to_local(*dst, result, bb);
                    } else {
                        let elem_drop = self.infer_drop_strategy(elem_type_name);
                        if matches!(elem_drop, crate::ir::types::DropStrategy::Recursive) {
                            // Recursive-drop struct: deep-clone via {Type}__clone(ptr)
                            // to produce an independently-owned copy. The collection
                            // retains its original element.
                            let clone_fn = format!("{elem_type_name}__clone");
                            let ret_ty = elem_ty.clone();
                            self.ensure_extern(&clone_fn, &[LirType::Ptr], &ret_ty);
                            let result = self.lir_func.next_value();
                            self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                                dst: Some(result),
                                name: clone_fn,
                                args: vec![ptr_val],
                                original_name: None,
                            });
                            self.store_to_local(*dst, result, bb);
                        } else {
                            // Other non-collection element: Load + move-zero
                            let result = self.lir_func.next_value();
                            self.lir_func.block_mut(bb).insts.push(Inst::Load {
                                dst: result,
                                ty: elem_ty.clone(),
                                ptr: ptr_val,
                            });
                            self.store_to_local(*dst, result, bb);
                        }

                        // Zero source slot for non-Recursive move semantics.
                        // Recursive types don't zero — the clone makes the copy independent.
                        let elem_needs_zero = match &elem_drop {
                            crate::ir::types::DropStrategy::None
                            | crate::ir::types::DropStrategy::Recursive => false,
                            _ => true,
                        };
                        if elem_needs_zero {
                            let byte_size = c_sizeof_lir_type(&elem_ty, &self.module_structs) as i64;
                            if byte_size > 0 {
                                let zero = self.emit_i32_const(bb, 0);
                                let sz = self.emit_i64_const(bb, byte_size);
                                self.lir_func.block_mut(bb).insts.push(Inst::Memset {
                                    ptr: ptr_val, byte: zero, size: sz,
                                });
                            }
                        }
                    }
                } else {
                    // Fallback: generic element access via ElemPtr
                    let base_val = self.lower_place_addr(base, bb);
                    let idx = self.lower_operand(index, bb);
                    let dst_slot = self.local_to_slot[dst.0 as usize];
                    let elem_ty = self.lir_func.slots[dst_slot.0 as usize].ty.clone();
                    let elem_size = match &elem_ty {
                        LirType::Struct(sid) => {
                            let sdef = &self.module_structs[sid.0 as usize];
                            (sdef.fields.len() as u32) * 8
                        }
                        LirType::Bool | LirType::I8 | LirType::U8 => 1,
                        LirType::I16 | LirType::U16 => 2,
                        LirType::I32 | LirType::U32 | LirType::F32 => 4,
                        _ => 8,
                    };
                    let elem_ptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::ElemPtr {
                        dst: elem_ptr,
                        base: base_val,
                        index: idx,
                        elem_size,
                    });
                    let result = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: result,
                        ptr: elem_ptr,
                        ty: elem_ty,
                    });
                    self.store_to_local(*dst, result, bb);
                }
            }

            // -- Enum --
            Instruction::EnumInit {
                dst,
                type_name,
                variant,
                fields,
            } => {
                let struct_id = self
                    .struct_reg
                    .lookup(type_name)
                    .unwrap_or(StructId(0));

                let slot = self.local_to_slot[dst.0 as usize];
                let base = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                    dst: base,
                    slot,
                });

                // Store tag (field 0).
                let tag_ordinal = self.resolve_variant_ordinal(type_name, variant);
                let tag_val = self.emit_i32_const(bb, tag_ordinal as i64);
                let tag_ptr = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                    dst: tag_ptr,
                    base,
                    struct_id,
                    field: 0,
                });
                self.lir_func.block_mut(bb).insts.push(Inst::Store {
                    ptr: tag_ptr,
                    value: tag_val,
                });

                // Store variant fields (offset: 1 + sum of preceding variant fields).
                let field_offset = self.resolve_variant_field_offset(type_name, variant);
                // Look up field types for Null → enum promotion (same as StructInit).
                let variant_field_types = self.resolve_variant_field_types(type_name, variant);
                for (i, field_op) in fields.iter().enumerate() {
                    // Special-case: Null field for an enum type (e.g. Some(None)).
                    if matches!(field_op, Operand::Constant(Constant::Null)) {
                        if let Some(Some(fty)) = variant_field_types.get(i) {
                            if let Some((field_enum_sid, fld_tag_ordinal)) = self.find_enum_null_variant(*fty) {
                                let fptr = self.lir_func.next_value();
                                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                                    dst: fptr, base, struct_id,
                                    field: (field_offset + i) as u32,
                                });
                                self.emit_enum_tag_store(fptr, field_enum_sid, fld_tag_ordinal, bb);
                                continue;
                            }
                        }
                    }

                    let val = self.lower_operand(field_op, bb);
                    let fptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: fptr,
                        base,
                        struct_id,
                        field: (field_offset + i) as u32,
                    });
                    self.lir_func.block_mut(bb).insts.push(Inst::Store {
                        ptr: fptr,
                        value: val,
                    });
                }

                // Post-init zero: after moving a resource-type local into an enum variant
                // (e.g. Some(vec)), zero the source to prevent double-free. The enum now
                // owns the data. This mirrors the old GIR→C backend's post-EnumInit zeroing.
                // Collect slots to zero first to avoid borrow conflicts.
                let slots_to_zero: Vec<(SlotId, i64)> = fields.iter().filter_map(|field_op| {
                    if let Operand::Copy(place) | Operand::Move(place) = field_op {
                        if place.projections.is_empty() {
                            let local_idx = place.local.0 as usize;
                            if local_idx < self.local_to_slot.len() {
                                let src_slot = self.local_to_slot[local_idx];
                                let src_ty = &self.lir_func.slots[src_slot.0 as usize].ty;
                                if let LirType::Struct(sid) = src_ty {
                                    let needs_zero = self.module_structs.get(sid.0 as usize)
                                        .map_or(false, |s| matches!(s.name.as_str(),
                                            "GorgetArray" | "GorgetMap" | "GorgetSet" | "GorgetString" | "GorgetClosure"
                                        ));
                                    if needs_zero {
                                        let byte_size = c_sizeof_lir_type(src_ty, &self.module_structs) as i64;
                                        return Some((src_slot, byte_size));
                                    }
                                }
                            }
                        }
                    }
                    None
                }).collect();
                for (src_slot, byte_size) in slots_to_zero {
                    let addr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                        dst: addr,
                        slot: src_slot,
                    });
                    let zero = self.emit_i32_const(bb, 0);
                    let size = self.emit_i64_const(bb, byte_size);
                    self.lir_func.block_mut(bb).insts.push(Inst::Memset {
                        ptr: addr,
                        byte: zero,
                        size,
                    });
                }
            }

            Instruction::TagOf { dst, operand } => {
                let val = self.lower_operand(operand, bb);
                // Tag is at field 0 of the enum struct. Load it via FieldPtr.
                let tag_ptr = self.lir_func.next_value();
                // We need the struct id. For TagOf on an operand that's a local:
                let struct_id = if let Operand::Copy(p) | Operand::Move(p) = operand {
                    let gir_type_id = self.gir_func.locals[p.local.0 as usize].type_id;
                    self.resolve_struct_id(gir_type_id)
                } else {
                    StructId(0) // fallback
                };
                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                    dst: tag_ptr,
                    base: val,
                    struct_id,
                    field: 0,
                });
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::Load {
                    dst: result,
                    ptr: tag_ptr,
                    ty: LirType::I32,
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::EnumFieldLoad {
                dst,
                base,
                variant,
                field,
            } => {
                let mut base_val = self.lower_place_addr(base, bb);
                let gir_type_id = self.gir_func.locals[base.local.0 as usize].type_id;
                // If after resolving projections we still have a pointer type,
                // the base_val is a SlotAddr of a pointer local — load the pointer
                // to get the actual enum struct address.
                let effective_ty = self.effective_place_type(base);
                if let Some(GirType::Ptr(_) | GirType::MutPtr(_)) = self.gir_types.get(effective_ty) {
                    let deref = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: deref,
                        ptr: base_val,
                        ty: LirType::Ptr,
                    });
                    base_val = deref;
                }
                let struct_id = self.resolve_struct_id(gir_type_id);
                let type_name = self.resolve_type_name(gir_type_id);
                let field_offset = self.resolve_variant_field_offset(&type_name, variant);


                let fptr = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                    dst: fptr,
                    base: base_val,
                    struct_id,
                    field: (field_offset + *field as usize) as u32,
                });
                // If destination is Ptr(T), return field address as pointer reference.
                // This happens when the scrutinee is a borrowed enum (Ptr param).
                let dst_gir_type = self.gir_func.locals[dst.0 as usize].type_id;
                if matches!(self.gir_types.get(dst_gir_type), Some(GirType::Ptr(_))) {
                    self.store_to_local(*dst, fptr, bb);
                } else {
                    let result = self.lir_func.next_value();
                    let field_ty = self.resolve_enum_field_type(gir_type_id, variant, *field);
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: result,
                        ptr: fptr,
                        ty: field_ty,
                    });
                    self.store_to_local(*dst, result, bb);
                }
            }

            Instruction::TupleInit { dst, elements } => {
                // Tuples are stored as struct slots. Store each element by field index.
                let slot = self.local_to_slot[dst.0 as usize];
                let base = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                    dst: base,
                    slot,
                });
                // Need the struct_id for the tuple type.
                let gir_type_id = self.gir_func.locals[dst.0 as usize].type_id;
                let struct_id = self.resolve_struct_id(gir_type_id);

                for (i, elem) in elements.iter().enumerate() {
                    let val = self.lower_operand(elem, bb);
                    let fptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: fptr,
                        base,
                        struct_id,
                        field: i as u32,
                    });
                    self.lir_func.block_mut(bb).insts.push(Inst::Store {
                        ptr: fptr,
                        value: val,
                    });
                }
            }

            // -- Ownership / lifetime (pass-through as calls or nops) --
            Instruction::Drop { place } => {
                self.lower_drop(place, bb);
            }

            Instruction::DropIfAlive { place } => {
                self.lower_drop(place, bb);
            }

            Instruction::MoveZero { place } => {
                // Zero out a place after move. Emit memset(addr, 0, sizeof).
                // For PtrTo locals (pointer-wrapped strings), zero the POINTER SLOT
                // (set to NULL), not the pointee. lower_place_addr for PtrTo does
                // SlotLoad (returns pointer value), so memset would corrupt pointee.
                let slot = self.local_to_slot[place.local.0 as usize];
                let is_ptr_slot = matches!(self.lir_func.slots[slot.0 as usize].ty, LirType::PtrTo(_));
                let addr = if is_ptr_slot && place.projections.is_empty() {
                    let a = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr { dst: a, slot });
                    a
                } else {
                    self.lower_place_addr(place, bb)
                };
                let zero = self.emit_i32_const(bb, 0);
                // Resolve the actual type being zeroed, following projections.
                let effective_ty = if place.projections.is_empty() {
                    let slot_idx = place.local.0 as usize;
                    self.lir_func.slots[self.local_to_slot[slot_idx].0 as usize].ty.clone()
                } else {
                    // Follow projections to find the leaf type.
                    let mut gir_type = self.gir_func.locals[place.local.0 as usize].type_id;
                    for proj in &place.projections {
                        match proj {
                            Projection::Field(field) => {
                                gir_type = self.resolve_field_gir_type_id(gir_type, *field);
                            }
                            Projection::Deref => {
                                gir_type = self.resolve_deref_gir_type_id(gir_type);
                            }
                            Projection::Index(_) => {
                                // Index projection: element type unknown at this level.
                                break;
                            }
                        }
                    }
                    self.map_type(&gir_type)
                };
                let byte_size = match &effective_ty {
                    LirType::Struct(_) => c_sizeof_lir_type(&effective_ty, &self.module_structs) as i64,
                    _ => crate::lir::types::scalar_size(&effective_ty).unwrap_or(8) as i64,
                };
                let size = self.emit_i64_const(bb, byte_size);
                self.lir_func.block_mut(bb).insts.push(Inst::Memset {
                    ptr: addr,
                    byte: zero,
                    size,
                });
            }

            Instruction::Borrow { dst, place } | Instruction::BorrowMut { dst, place } => {
                let addr = self.lower_place_addr(place, bb);
                self.store_to_local(*dst, addr, bb);
            }

            // -- Ref load/store (explicit Ptr dereference) --
            Instruction::LoadRef { dst, src } => {
                // Load through Ptr: deref src to get value, store in dst.
                // Same as FieldLoad with Deref projection, but explicit.
                let src_addr = self.lower_place_addr(src, bb);
                let src_type = self.effective_place_type(src);
                let pointee = self.resolve_deref_gir_type_id(src_type);
                let field_ty = self.map_type(&pointee);
                let deref_val = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::Load {
                    dst: deref_val,
                    ptr: src_addr,
                    ty: field_ty,
                });
                self.store_to_local(*dst, deref_val, bb);
            }
            Instruction::StoreRef { dst, value } => {
                // Store through Ptr: write value to the address held by dst.
                let val = self.lower_operand(value, bb);
                let dst_addr = self.lower_place_addr(dst, bb);
                // Deref the Ptr to get the target address
                let target = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::Load {
                    dst: target,
                    ptr: dst_addr,
                    ty: LirType::Ptr,
                });
                self.lir_func.block_mut(bb).insts.push(Inst::Store {
                    ptr: target,
                    value: val,
                });
            }

            // -- Allocator --
            Instruction::HeapAlloc {
                dst,
                type_id: _,
                allocator,
            } => {
                // Placeholder: lower as CallExtern to malloc-like.
                let alloc = self.lower_operand(allocator, bb);
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: Some(result),
                    name: "__gorget_alloc".into(),
                    args: vec![alloc],
                    original_name: None,
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::HeapAllocArray {
                dst,
                type_id: _,
                count,
                allocator,
            } => {
                let cnt = self.lower_operand(count, bb);
                let alloc = self.lower_operand(allocator, bb);
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: Some(result),
                    name: "__gorget_alloc_array".into(),
                    args: vec![cnt, alloc],
                    original_name: None,
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::Dealloc { ptr, allocator } => {
                let p = self.lower_operand(ptr, bb);
                let a = self.lower_operand(allocator, bb);
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: "__gorget_dealloc".into(),
                    args: vec![p, a],
                    original_name: None,
                });
            }

            Instruction::LoadThreadLocal { dst, name } => {
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: Some(result),
                    name: format!("__gorget_tls_{name}"),
                    args: vec![],
                    original_name: None,
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::PushAllocator { allocator } => {
                let alloc = self.lower_operand(allocator, bb);
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: "__gorget_push_allocator".into(),
                    args: vec![alloc],
                    original_name: None,
                });
            }

            Instruction::PopAllocator => {
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: "__gorget_pop_allocator".into(),
                    args: vec![],
                    original_name: None,
                });
            }

            Instruction::InlineC { code } => {
                // InlineC is a C-backend-specific escape hatch. Parse assignment patterns
                // like `_X = (int64_t)_Y.field;` to wire up slot store for the destination.

                // Emit SlotAddr for all slots referenced in the expression part.
                // This prevents SSA from promoting those slots, since InlineC reads
                // them by name (__sN) and SSA can't rewrite opaque C strings.
                let expr_part = if let Some(eq_pos) = code.find(" = ") {
                    &code[eq_pos + 3..]
                } else {
                    code.as_str()
                };
                self.mark_inline_c_referenced_slots(expr_part, bb);

                let dst_val = if let Some(eq_pos) = code.find(" = ") {
                    let dst_part = code[..eq_pos].trim().trim_start_matches('_');
                    if let Ok(local_idx) = dst_part.parse::<u32>() {
                        let slot = self.local_to_slot[local_idx as usize];
                        // Mark destination slot as address-taken so SSA won't
                        // promote it.  The C backend's type inference relies on
                        // the InlineC→SlotStore pattern to determine the value's
                        // type; SSA promotion removes the SlotStore and the type
                        // defaults to void*, which breaks collection push/put
                        // for scalar Dict keys.
                        let addr_dummy = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                            dst: addr_dummy,
                            slot,
                        });
                        let val = self.lir_func.next_value();
                        // Emit InlineC with a dst, then store to slot.
                        self.lir_func.block_mut(bb).insts.push(Inst::InlineC {
                            dst: Some(val),
                            code: code.clone(),
                        });
                        self.lir_func.block_mut(bb).insts.push(Inst::SlotStore {
                            slot,
                            value: val,
                            is_move: false,
                        });
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };
                if !dst_val {
                    // No assignment pattern — emit as passthrough without dst.
                    self.lir_func.block_mut(bb).insts.push(Inst::InlineC {
                        dst: None,
                        code: code.clone(),
                    });
                }
            }

            Instruction::GlobalAssign { name, value } => {
                if let Some(&gid) = self.global_index.get(name) {
                    let val = self.lower_operand(value, bb);
                    let addr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::GlobalAddr { dst: addr, global: gid });
                    let global_ty = &self.module_globals[gid.0 as usize].ty;
                    if global_ty.is_scalar() {
                        // Scalar store: dereference and assign.
                        self.lir_func.block_mut(bb).insts.push(Inst::Store { ptr: addr, value: val });
                    } else {
                        // Aggregate store: memcpy.
                        self.lir_func.block_mut(bb).insts.push(Inst::Store { ptr: addr, value: val });
                    }
                }
            }

            Instruction::Nop => {
                self.lir_func.block_mut(bb).insts.push(Inst::Nop);
            }
        }
    }

    pub(super) fn lower_terminator(&mut self, term: &Terminator, bb: BlockId) -> Term {
        match term {
            Terminator::Return(operand) => {
                let ret_type = self.map_type(&self.gir_func.return_type);
                if ret_type == LirType::Void {
                    Term::RetVoid
                } else {
                    let val = self.lower_operand(operand, bb);
                    Term::Ret(val)
                }
            }
            Terminator::Jump(target) => {
                let lir_target = self.block_map[target.0 as usize];
                Term::Jump(lir_target, vec![])
            }
            Terminator::Branch {
                cond,
                then_block,
                else_block,
            } => {
                let cond_val = self.lower_operand(cond, bb);
                Term::Branch {
                    cond: cond_val,
                    then_block: self.block_map[then_block.0 as usize],
                    then_args: vec![],
                    else_block: self.block_map[else_block.0 as usize],
                    else_args: vec![],
                }
            }
            Terminator::Switch {
                value,
                cases,
                default,
            } => {
                let val = self.lower_operand(value, bb);
                let lir_cases: Vec<(i64, BlockId, Vec<ValueId>)> = cases
                    .iter()
                    .map(|(v, b)| (*v, self.block_map[b.0 as usize], vec![]))
                    .collect();
                Term::Switch {
                    value: val,
                    cases: lir_cases,
                    default: self.block_map[default.0 as usize],
                    default_args: vec![],
                }
            }
            Terminator::Invoke {
                func,
                args,
                dst,
                normal,
                error,
            } => {
                // Invoke = call that can throw + branch on success/error.
                // Emit the call in the block, then jump to normal.
                // TODO: Phase 2.6 — proper try/catch lowering with error path.
                let lir_args: Vec<ValueId> =
                    args.iter().map(|a| self.lower_operand(a, bb)).collect();
                let result = dst.map(|_| self.lir_func.next_value());

                if let Some(fid) = self.func_index.get(func) {
                    self.lir_func.block_mut(bb).insts.push(Inst::Call {
                        dst: result,
                        func: *fid,
                        args: lir_args,
                    });
                } else {
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: result,
                        name: func.clone(),
                        args: lir_args,
                        original_name: None,
                    });
                }

                if let (Some(d), Some(r)) = (*dst, result) {
                    self.store_to_local(d, r, bb);
                }

                let _ = error; // error path not yet lowered
                Term::Jump(self.block_map[normal.0 as usize], vec![])
            }
            Terminator::Unreachable => Term::Unreachable,
        }
    }

    /// Emit SlotAddr for all GIR local references (`_N`) found in an InlineC
    /// expression string. This marks those slots as address-taken so SSA will
    /// not promote them — the InlineC code reads/writes them by name.
    pub(super) fn mark_inline_c_referenced_slots(&mut self, expr: &str, bb: BlockId) {
        let bytes = expr.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            if bytes[i] == b'_'
                && (i == 0 || !bytes[i - 1].is_ascii_alphanumeric())
            {
                let start = i + 1;
                let mut end = start;
                while end < bytes.len() && bytes[end].is_ascii_digit() {
                    end += 1;
                }
                if end > start
                    && (end >= bytes.len() || !bytes[end].is_ascii_alphanumeric())
                {
                    if let Ok(local_idx) = expr[start..end].parse::<usize>() {
                        if local_idx < self.local_to_slot.len() {
                            let slot = self.local_to_slot[local_idx];
                            let dummy = self.lir_func.next_value();
                            self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                                dst: dummy,
                                slot,
                            });
                        }
                    }
                    i = end;
                    continue;
                }
            }
            i += 1;
        }
    }

    // ── Operand lowering ────────────────────────────────────────────────────

    /// Lower a GIR operand, emitting load instructions into block `bb`.
    pub(super) fn lower_operand(&mut self, operand: &Operand, bb: BlockId) -> ValueId {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => self.lower_place_load(place, bb),
            Operand::Constant(c) => self.lower_constant(c, bb),
        }
    }

    /// Check if a GIR operand refers to a Str-typed local (simple, no projections).
    pub(super) fn operand_is_str(&self, operand: &Operand) -> bool {
        let str_sid = self.struct_reg.lookup("GorgetString");
        match operand {
            Operand::Copy(place) | Operand::Move(place) => {
                if !place.projections.is_empty() { return false; }
                let idx = place.local.0 as usize;
                if idx >= self.local_to_slot.len() { return false; }
                let slot = self.local_to_slot[idx];
                let slot_ty = &self.lir_func.slots[slot.0 as usize].ty;
                matches!(slot_ty, LirType::Struct(sid) if Some(*sid) == str_sid)
            }
            _ => false,
        }
    }

    /// Shared extern-call emitter used by both `Instruction::Call` (unresolved)
    /// and `Instruction::CallExtern`.  Handles sizeof synthesis for collection
    /// and concurrency constructors, and struct-return rewriting for mutex lock /
    /// rwlock read/write.
    pub(super) fn emit_extern_call(
        &mut self,
        original_name: &str,  // GIR name (before mapping) — used for sizeof extraction
        emit_name: &str,      // runtime name (after mapping)
        dst: &Option<ir::types::LocalId>,
        args: &[Operand],
        mut lir_args: Vec<ValueId>,
        bb: BlockId,
    ) {
        // Guard/ReadGuard/WriteGuard get/get_ptr: inline as FieldPtr + Load
        // instead of calling the runtime function. This preserves the concrete
        // inner type through the LIR so the c_lir backend emits correct code.
        // gorget_guard_get(guard*) → load guard->ptr, then load *(T*)ptr
        // gorget_guard_get_ptr(guard*) → load guard->ptr (returns void*)
        if matches!(emit_name, "gorget_guard_get" | "gorget_read_guard_get" | "gorget_write_guard_get") {
            if let Some(d) = *dst {
                let guard_ptr = lir_args[0]; // pointer to guard struct
                // Look up the guard struct type from the original GIR name.
                // E.g., "Guard__int64_t__get" → struct name "Guard__int64_t".
                let guard_struct_name = original_name.rsplit_once("__")
                    .map(|(prefix, _method)| prefix);
                let guard_sid = guard_struct_name
                    .and_then(|name| self.struct_reg.lookup(name));
                if let Some(sid) = guard_sid {
                    // Determine the concrete inner type from the destination local.
                    let inner_ty = {
                        let gir_ty = self.gir_func.locals[d.0 as usize].type_id;
                        self.map_type(&gir_ty)
                    };
                    // Load the `ptr` field (field index 1: "ptr")
                    let ptr_val = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: ptr_val,
                        base: guard_ptr,
                        struct_id: sid,
                        field: 1,
                    });
                    let data_ptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: data_ptr,
                        ptr: ptr_val,
                        ty: LirType::Ptr,
                    });
                    // Dereference to the concrete inner type.
                    let result = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: result,
                        ptr: data_ptr,
                        ty: inner_ty,
                    });
                    self.store_to_local(d, result, bb);
                    return;
                }
                // Fallthrough: if we can't find the struct, use the runtime call.
            }
        }

        // gorget_guard_get_ptr / gorget_read_guard_get_ptr / gorget_write_guard_get_ptr:
        // return the raw data pointer (no final dereference).
        if matches!(emit_name, "gorget_guard_get_ptr" | "gorget_read_guard_get_ptr" | "gorget_write_guard_get_ptr") {
            if let Some(d) = *dst {
                let guard_ptr = lir_args[0];
                // Derive method name from emit_name to correctly strip from original_name.
                // E.g., emit_name "gorget_guard_get_ptr" → method "get_ptr",
                //        original_name "Guard__int64_t__get_ptr" → struct "Guard__int64_t".
                // rsplit_once("__") would incorrectly split "get_ptr" at the underscore.
                let method = if emit_name.starts_with("gorget_write_guard_") {
                    &emit_name["gorget_write_guard_".len()..]
                } else if emit_name.starts_with("gorget_read_guard_") {
                    &emit_name["gorget_read_guard_".len()..]
                } else {
                    &emit_name["gorget_guard_".len()..]
                };
                let suffix = format!("__{method}");
                let guard_struct_name = original_name.strip_suffix(&suffix);
                let guard_sid = guard_struct_name
                    .and_then(|name| self.struct_reg.lookup(name));
                if let Some(sid) = guard_sid {
                    let ptr_val = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: ptr_val,
                        base: guard_ptr,
                        struct_id: sid,
                        field: 1,
                    });
                    let data_ptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: data_ptr,
                        ptr: ptr_val,
                        ty: LirType::Ptr,
                    });
                    self.store_to_local(d, data_ptr, bb);
                    return;
                }
            }
        }

        // gorget_shared_get(shared*) → dereference the inner data pointer.
        // gorget_shared_get_ptr returns the raw void* — handled via normal call.
        // For shared_get, we can't inline it (the data pointer is inside the
        // GorgetShared control block), so we leave it as a runtime call.

        // Track override for the emitted function name (e.g., map_new → map_new_str).
        let mut actual_emit_name: Option<String> = None;

        // Collection constructors need synthesized sizeof arguments.
        // gorget_array_new(elem_size), gorget_set_new/gorget_ordered_set_new(elem_size)
        if (emit_name == "gorget_array_new" || emit_name == "gorget_set_new" || emit_name == "gorget_ordered_set_new")
            && lir_args.is_empty()
        {
            // For Set with Str elements, use *_str() variant which sets
            // up the string hash function (no size arg needed).
            if emit_name == "gorget_set_new" || emit_name == "gorget_ordered_set_new" {
                let elem_type = set_elem_type_from_monomorphized(original_name);
                if elem_type.as_deref() == Some("GorgetString") {
                    let str_variant = if emit_name == "gorget_ordered_set_new" {
                        "gorget_ordered_set_new_str"
                    } else {
                        "gorget_set_new_str"
                    };
                    actual_emit_name = Some(str_variant.into());
                }
            }
            if actual_emit_name.is_none() {
                let elem_sz = elem_size_from_monomorphized(original_name, self.module_structs).unwrap_or(8) as i64;
                let sz_val = self.emit_i64_const(bb, elem_sz);
                lir_args.push(sz_val);
            }
        }
        // gorget_map_new / gorget_dict_new — need sizeof args.
        // For Str/GorgetString keys, use _str variant which
        // sets up the string hash function.
        if (emit_name == "gorget_map_new" || emit_name == "gorget_dict_new") && lir_args.is_empty() {
            let is_dict = emit_name == "gorget_dict_new";
            let (key_sz, val_sz) = dict_elem_sizes_from_monomorphized(original_name, self.module_structs);
            let key_type = dict_key_type_from_monomorphized(original_name);
            if key_type.as_deref() == Some("GorgetString") {
                // Use _str variant for string keys.
                let str_variant = if is_dict { "gorget_dict_new_str" } else { "gorget_map_new_str" };
                actual_emit_name = Some(str_variant.into());
                let v = self.emit_i64_const(bb, val_sz as i64);
                lir_args.push(v);
            } else {
                let k = self.emit_i64_const(bb, key_sz as i64);
                let v = self.emit_i64_const(bb, val_sz as i64);
                lir_args.push(k);
                lir_args.push(v);
            }
        }
        let emit_name = actual_emit_name.as_deref().unwrap_or(emit_name);
        // gorget_array_contains needs elem_size appended.
        if emit_name == "gorget_array_contains" && args.len() >= 2 {
            let elem_lir_ty = self.operand_lir_type(&args[1]);
            let elem_sz = lir_type_sizeof(&elem_lir_ty) as i64;
            let sz_val = self.emit_i64_const(bb, elem_sz);
            lir_args.push(sz_val);
        }

        // Concurrency constructors: gorget_mutex_new(size, &val),
        // gorget_shared_new(size, &val), gorget_rwlock_new(size, &val).
        // The GIR emits a single arg (the initial value). We prepend sizeof.
        if matches!(emit_name, "gorget_mutex_new" | "gorget_shared_new" | "gorget_rwlock_new")
            && lir_args.len() == 1
        {
            let elem_sz = concurrency_elem_size(original_name, self.module_structs).unwrap_or(8) as i64;
            let sz_val = self.emit_i64_const(bb, elem_sz);
            lir_args.insert(0, sz_val);
        }

        // gorget_channel_new(capacity, elem_size) — GIR passes (capacity).
        if emit_name == "gorget_channel_new" && lir_args.len() == 1 {
            let elem_sz = concurrency_elem_size(original_name, self.module_structs).unwrap_or(8) as i64;
            let sz_val = self.emit_i64_const(bb, elem_sz);
            lir_args.push(sz_val);
        }

        // gorget_guard_set(guard, &val, sizeof) and gorget_write_guard_set
        if matches!(emit_name, "gorget_guard_set" | "gorget_write_guard_set")
            && lir_args.len() == 2
        {
            let elem_sz = concurrency_elem_size(original_name, self.module_structs).unwrap_or(8) as i64;
            let sz_val = self.emit_i64_const(bb, elem_sz);
            lir_args.push(sz_val);
        }

        // gorget_mutex_lock / gorget_rwlock_read / gorget_rwlock_write return
        // structs by value — use `_to` output-pointer variants instead.
        if matches!(emit_name, "gorget_mutex_lock" | "gorget_rwlock_read" | "gorget_rwlock_write") {
            if let Some(d) = *dst {
                let to_name = format!("{emit_name}_to");
                let slot = self.local_to_slot[d.0 as usize];
                let slot_ptr = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                    dst: slot_ptr,
                    slot,
                });
                lir_args.push(slot_ptr);
                let mut arg_types: Vec<LirType> = args.iter().map(|a| self.operand_lir_type(a)).collect();
                arg_types.push(LirType::Ptr);
                self.ensure_extern(&to_name, &arg_types, &LirType::Void);
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: to_name,
                    args: lir_args,
                    original_name: None,
                });
                return;
            }
        }

        // Derive arg types from GIR operand types (for proper extern declarations).
        let is_printf_like = emit_name == "printf" || emit_name == "fprintf_stderr"
            || emit_name == "gorget_string_format" || emit_name == "gorget_string_format_alloc"
            || emit_name == "snprintf" || emit_name == "sprintf";
        let arg_types: Vec<LirType> = if is_printf_like {
            lir_args.iter().map(|_| LirType::Ptr).collect()
        } else {
            let mut types: Vec<LirType> = args.iter().map(|a| self.operand_lir_type(a)).collect();
            while types.len() < lir_args.len() {
                types.push(LirType::I64);
            }
            types
        };
        let ret_ty = dst.map(|d| {
            let gir_ty = self.gir_func.locals[d.0 as usize].type_id;
            self.map_type(&gir_ty)
        }).unwrap_or(LirType::Void);
        // __callable_N and __gorget_closure_call_N use function-scoped local IDs.
        // Different functions can have __callable_3 with different return types.
        // Make the extern name unique per function to avoid type conflicts.
        let actual_emit_name = if emit_name.starts_with("__callable_") || emit_name.starts_with("__gorget_closure_call_") {
            format!("{}__{}", emit_name, self.lir_func.name.replace("::", "__"))
        } else {
            emit_name.to_string()
        };
        self.ensure_extern(&actual_emit_name, &arg_types, &ret_ty);

        // Self-cleaning: gorget_array_set calls elem_drop internally.

        let is_void_ret = matches!(ret_ty, LirType::Void);
        let result = if is_void_ret { None } else { dst.map(|_| self.lir_func.next_value()) };
        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
            dst: result,
            name: actual_emit_name,
            args: lir_args,
            original_name: Some(original_name.to_string()),
        });
        if let (Some(d), Some(r)) = (*dst, result) {
            self.store_to_local(d, r, bb);
        }

        // Post-call zeroing: after push/set/send that consumes a value by move,
        // zero the source local to prevent double-free at scope-end Drop.
        // The GIR backend does this inline; we do it here in the LIR lowering
        // because the GIR's MoveZero doesn't cover all push cases.
        let consuming_arg_gir_idx: Option<usize> = match emit_name {
            "gorget_array_push" | "gorget_set_add" | "gorget_heap_push" => Some(1),
            "gorget_array_insert" | "gorget_array_set" | "gorget_map_put" => Some(2),
            "gorget_channel_send" => Some(1),
            _ => None,
        };
        if let Some(arg_idx) = consuming_arg_gir_idx {
            if let Some(arg) = args.get(arg_idx) {
                if let Operand::Copy(place) | Operand::Move(place) = arg {
                    if place.projections.is_empty() {
                        let local_idx = place.local.0 as usize;
                        if local_idx < self.gir_func.locals.len() {
                            let type_id = self.gir_func.locals[local_idx].type_id;
                            if let Some(GirType::Named(name)) = self.gir_types.get(type_id) {
                                // Only zero types that need dropping AND are user/struct types
                                // (not primitive scalars). Direct resource types (GorgetArray etc.)
                                // are already handled by the c_lir backend's post-push zero.
                                let needs_zero = self.gir_types.get_type_def(name).map_or(false, |td| {
                                    matches!(td.metadata.drop_strategy,
                                        crate::ir::types::DropStrategy::Custom(_) |
                                        crate::ir::types::DropStrategy::Recursive)
                                });
                                if needs_zero {
                                    let slot = self.local_to_slot[local_idx];
                                    let slot_ty = self.lir_func.slots[slot.0 as usize].ty.clone();
                                    let byte_size = match &slot_ty {
                                        LirType::Struct(_) => c_sizeof_lir_type(&slot_ty, &self.module_structs) as i64,
                                        _ => crate::lir::types::scalar_size(&slot_ty).unwrap_or(8) as i64,
                                    };
                                    let addr = self.lir_func.next_value();
                                    self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                                        dst: addr, slot,
                                    });
                                    let zero_val = self.emit_i32_const(bb, 0);
                                    let size_val = self.emit_i64_const(bb, byte_size);
                                    self.lir_func.block_mut(bb).insts.push(Inst::Memset {
                                        ptr: addr, byte: zero_val, size: size_val,
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Lower printf/fprintf args, expanding Str-typed operands to (int)len, data.
    pub(super) fn lower_printf_args(&mut self, args: &[Operand], bb: BlockId) -> Vec<ValueId> {
        let mut lir_args = Vec::new();
        // Pre-scan: which args (1-based) are Str-typed? We need this to fix the format string.
        let str_arg_indices: Vec<bool> = args.iter().enumerate()
            .map(|(i, a)| i > 0 && self.operand_is_str(a))
            .collect();
        let has_str_args = str_arg_indices.iter().any(|&b| b);

        for (i, arg) in args.iter().enumerate() {
            if i == 0 {
                // First arg is always the format string (const char*).
                // If any subsequent args are Str, fix the format string:
                // replace corresponding %lld with %.*s.
                if has_str_args {
                    if let Operand::Constant(Constant::Str(fmt_str)) = arg {
                        let fixed = fix_printf_str_format(fmt_str, &str_arg_indices[1..]);
                        let fixed_val = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::StrLit {
                            dst: fixed_val,
                            value: fixed,
                        });
                        lir_args.push(fixed_val);
                    } else {
                        lir_args.push(self.lower_operand(arg, bb));
                    }
                } else {
                    lir_args.push(self.lower_operand(arg, bb));
                }
            } else if self.operand_is_str(arg) {
                // Str-typed arg: expand to (int)len, (const char*)data for %.*s.
                if let Operand::Copy(place) | Operand::Move(place) = arg {
                    let slot = self.local_to_slot[place.local.0 as usize];
                    let slot_ty = self.lir_func.slots[slot.0 as usize].ty.clone();
                    let struct_id = match &slot_ty {
                        LirType::Struct(sid) => *sid,
                        _ => unreachable!(),
                    };

                    // Str fields: 0=data (Ptr), 1=len (I64), 2=cap (I64), 3=alloc (Ptr)
                    // Load .len (field 1) → cast to I32 for printf %.*s precision
                    let base = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                        dst: base,
                        slot,
                    });
                    let len_ptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: len_ptr,
                        base,
                        struct_id,
                        field: 1,
                    });
                    let len_load = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: len_load,
                        ptr: len_ptr,
                        ty: LirType::I64,
                    });
                    let len_i32 = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::IntCast {
                        dst: len_i32,
                        value: len_load,
                        to: LirType::I32,
                    });
                    lir_args.push(len_i32);

                    // Load .data (field 0) — const char*
                    let base2 = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                        dst: base2,
                        slot,
                    });
                    let data_ptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: data_ptr,
                        base: base2,
                        struct_id,
                        field: 0,
                    });
                    let data_load = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: data_load,
                        ptr: data_ptr,
                        ty: LirType::Ptr,
                    });
                    lir_args.push(data_load);
                } else {
                    lir_args.push(self.lower_operand(arg, bb));
                }
            } else {
                lir_args.push(self.lower_operand(arg, bb));
            }
        }
        lir_args
    }

    /// Load a value from a GIR place.
    pub(super) fn lower_place_load(&mut self, place: &Place, bb: BlockId) -> ValueId {
        if place.projections.is_empty() {
            // Simple local — SlotLoad.
            self.ensure_local(place.local);
            let slot = self.local_to_slot[place.local.0 as usize];
            let slot_ty = self.lir_func.slots[slot.0 as usize].ty.clone();
            if slot_ty.is_aggregate() {
                // For aggregates, return address of slot.
                let addr = self.lir_func.next_value();
                self.lir_func
                    .block_mut(bb)
                    .insts
                    .push(Inst::SlotAddr { dst: addr, slot });
                addr
            } else {
                let dst = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::SlotLoad {
                    dst,
                    slot,
                    ty: slot_ty,
                });
                dst
            }
        } else {
            // Projected place — compute address then load.
            let addr = self.lower_place_addr(place, bb);
            let ty = self.resolve_place_type(place);
            // For Box deref of aggregate types (e.g. Box[Str]), we must emit a Load
            // because the pointer points to heap data that needs to be read.
            let is_box_deref = place.projections.first() == Some(&Projection::Deref)
                && self.gir_types.get(self.gir_func.locals[place.local.0 as usize].type_id)
                    .map_or(false, |t| matches!(t, GirType::Named(n) if n.starts_with("Box__")));
            if ty.is_aggregate() && !is_box_deref {
                addr // aggregates: the address IS the value
            } else {
                let dst = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::Load {
                    dst,
                    ptr: addr,
                    ty,
                });
                dst
            }
        }
    }

}
