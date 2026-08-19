//! Instruction and terminator lowering for FuncLowering.
//!
//! Extracted from mod.rs — covers lower_instruction, lower_terminator,
//! mark_inline_c_referenced_slots, lower_operand, operand_is_str,
//! emit_extern_call, lower_printf_args, and lower_place_load.

use super::*;

impl<'a> FuncLowering<'a> {
    pub(super) fn lower_instruction(&mut self, inst: &Instruction, mut bb: BlockId) -> BlockId {
        match inst {
            Instruction::Assign { mode, dst, value, .. } => {
                // Special-case: Constant::Null assigned to an enum-typed local.
                if let Operand::Constant(Constant::Null) = value {
                    if let Some(()) = self.try_materialize_null_for_assign(dst, bb) {
                        return bb;
                    }
                    // Null → aggregate destination: emit Memset(0) instead of
                    // NullPtr + Store so backends don't need to scan for NullPtr origin.
                    if self.try_null_memset(dst, bb) {
                        return bb;
                    }
                }
                // Special-case: Option/Result source → non-Option/Result dest.
                // `*mode` is threaded so Borrow-mode aliasing binds are never
                // mis-classified as payload unwraps (Chain C item 3).
                if let Some(val) = self.try_enum_payload_extract(*mode, dst, value, bb) {
                    self.store_to_place(dst, val, bb);
                    return bb;
                }
                // Special-case: Box[Trait] ← Box[Concrete] trait object construction.
                if self.try_trait_object_construct(dst, value, bb) {
                    return bb;
                }
                // Special-case: primitive → Result/Option slot wrapping.
                if dst.projections.is_empty() {
                    if self.try_result_option_wrap(dst.local, value, bb) {
                        return bb;
                    }
                }
                // Special-case: __Closure_N → GorgetClosure slot (closure escape).
                // Heap-allocate the env, memcpy, and emit ClosurePack.
                if dst.projections.is_empty() {
                    if self.try_closure_pack(dst.local, value, bb) {
                        return bb;
                    }
                }
                let is_move = matches!(mode, ir::instructions::AssignMode::Move);
                let val = self.lower_operand(value, bb);
                if is_move && dst.projections.is_empty() {
                    // Move: emit SlotStore with is_move flag so C backend can use
                    // memcpy instead of clone for resource types (strings, etc.).
                    let slot = self.local_to_slot[dst.local.0 as usize];
                    self.push_inst(bb, Inst::SlotStore {
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

                // Check for Vector + Vector → clone lhs then extend with rhs.
                // Read typed `collection_kind` (Phase A) — Vector/Deque/
                // GorgetArray all carry `Array` from the protocol registration.
                let is_vector_add = *op == GirBinOp::Add
                    && self.gir_types.collection_kind(*type_id)
                        == Some(crate::ir::types::CollectionKind::Array);

                if is_vector_add {
                    // Emit: result = gorget_array_clone(&lhs); gorget_array_extend(&result, &rhs);
                    // The c_lir backend handles &-address-of via arg_abis (AbiKind::Ptr).
                    let result = self.lir_func.next_value();
                    let arr_ty = self.struct_reg.lookup("GorgetArray")
                        .map(LirType::Struct).unwrap_or(LirType::Ptr);
                    self.ensure_extern("gorget_array_clone", &[LirType::Ptr], &arr_ty);
                    let abis = self.lookup_arg_abis("gorget_array_clone");
                    self.push_inst(bb, Inst::CallExtern {
                        dst: Some(result),
                        name: "gorget_array_clone".to_string(),
                        args: vec![l],
                        arg_abis: abis,
                    });
                    self.ensure_extern("gorget_array_extend", &[LirType::Ptr, LirType::Ptr], &LirType::Void);
                    let abis = self.lookup_arg_abis("gorget_array_extend");
                    self.push_inst(bb, Inst::CallExtern {
                        dst: None,
                        name: "gorget_array_extend".to_string(),
                        args: vec![result, r],
                        arg_abis: abis,
                    });
                    self.store_to_local(*dst, result, bb);
                } else {
                    let result = self.lir_func.next_value();
                    let ty = self.map_type(type_id);
                    // D28: when `lower_binop` returns a runtime `CallExtern`
                    // (currently only `**` → `gorget_pow{_checked_iN,f,}`),
                    // register the extern up front so `infer_call_extern_type`
                    // returns the correct return type instead of the
                    // I64 fallback (which would silently reinterpret the
                    // double result — measured on the fixture pow_float_positive).
                    if let Some(ext_name) = super::calls::runtime_extern_name_for_binop(*op, &ty) {
                        self.ensure_extern(ext_name, &[ty.clone(), ty.clone()], &ty);
                    }
                    let inst = lower_binop(result, *op, l, r, ty);
                    self.push_inst(bb, inst);
                    self.store_to_local(*dst, result, bb);
                }
            }

            // Fault-catch checked arithmetic (error-model.md §11). Split the
            // block at the op: compute a fault FLAG per CAUGHT category, branch
            // to the handler (mapped GIR→LIR via block_map) on fault, else
            // compute `dst = lhs op rhs` in the continuation. The shared
            // `Inst::FaultCheck` + `Term::Branch` shape both backends already
            // emit — no goto, no backend-specific routing.
            //
            // Add/Sub/Mul: a single overflow check → `overflow_handler`. Div/Rem
            // have TWO fault categories (Increment 2 (C) split): the signed
            // `TYPE_MIN/-1` overflow → `overflow_handler` (always `Some`: user
            // catch OR the GIR panic block), and `rhs == 0` → `divzero_handler`
            // (likewise always `Some`). Each emits its own check+branch; the
            // continuation is reached only when ALL caught conditions are false,
            // so the bare commit's residual checks are statically false there.
            Instruction::FaultableBinOp { dst, op, type_id, lhs, rhs, overflow_handler, divzero_handler } => {
                let l = self.lower_operand(lhs, bb);
                let r = self.lower_operand(rhs, bb);
                let ty = self.map_type(type_id);
                let is_div_rem = matches!(op, GirBinOp::Div | GirBinOp::Rem);
                if !matches!(op, GirBinOp::Add | GirBinOp::Sub | GirBinOp::Mul | GirBinOp::Div | GirBinOp::Rem) {
                    // Only the five integer faultable ops reach here (gated at
                    // GIR build in lower_binary_op); any other op is a lowering
                    // bug — fall back to a non-faulting compute so the build is
                    // still well-formed.
                    let result = self.lir_func.next_value();
                    if let Some(ext_name) = super::calls::runtime_extern_name_for_binop(*op, &ty) {
                        self.ensure_extern(ext_name, &[ty.clone(), ty.clone()], &ty);
                    }
                    let inst = lower_binop(result, *op, l, r, ty);
                    self.push_inst(bb, inst);
                    self.store_to_local(*dst, result, bb);
                    return bb;
                }

                // Emit one FaultCheck + Branch for a fault condition, splitting
                // the current block. Returns the new continuation block.
                let emit_check = |this: &mut Self, cur: BlockId, fault_op: crate::lir::FaultOp, handler: ir::types::BlockId| -> BlockId {
                    let flag = this.lir_func.next_value();
                    this.push_inst(cur, Inst::FaultCheck { dst: flag, op: fault_op, ty: ty.clone(), lhs: l, rhs: r });
                    let handler_lir = this.block_map[handler.0 as usize];
                    let cont = this.lir_func.add_block();
                    this.set_terminator(cur, Term::Branch {
                        cond: flag,
                        then_block: handler_lir,
                        then_args: vec![],
                        else_block: cont,
                        else_args: vec![],
                    });
                    cont
                };

                let mut cur = bb;
                if is_div_rem {
                    // Div/Rem: overflow (`TYPE_MIN/-1`) THEN div0 (`rhs == 0`).
                    // Both handlers are always `Some` (user entry or panic block).
                    if let Some(h) = overflow_handler {
                        cur = emit_check(self, cur, crate::lir::FaultOp::DivOverflow, *h);
                    }
                    if let Some(h) = divzero_handler {
                        let dz_op = if matches!(op, GirBinOp::Div) { crate::lir::FaultOp::Div } else { crate::lir::FaultOp::Rem };
                        cur = emit_check(self, cur, dz_op, *h);
                    }
                } else {
                    // Add/Sub/Mul: a single overflow check (always caught here —
                    // the GIR build only makes these faultable when overflow is
                    // caught).
                    let fault_op = match op {
                        GirBinOp::Add => crate::lir::FaultOp::Add,
                        GirBinOp::Sub => crate::lir::FaultOp::Sub,
                        _ => crate::lir::FaultOp::Mul,
                    };
                    if let Some(h) = overflow_handler {
                        cur = emit_check(self, cur, fault_op, *h);
                    }
                }

                // Continuation: now safe to compute `dst = lhs op rhs`.
                //   Add/Sub/Mul use WRAP mode (the FaultCheck already caught the
                //   overflow, so the wrapped value is the correct committed
                //   result on this path and never re-traps). Div/Rem use the
                //   normal checked inst — its div0/TYPE_MIN traps are all
                //   statically false here (the fault branches excluded them).
                let result = self.lir_func.next_value();
                let commit_op = match op {
                    GirBinOp::Add => GirBinOp::AddWrap,
                    GirBinOp::Sub => GirBinOp::SubWrap,
                    GirBinOp::Mul => GirBinOp::MulWrap,
                    other => *other, // Div / Rem
                };
                let inst = lower_binop(result, commit_op, l, r, ty);
                self.push_inst(cur, inst);
                self.store_to_local(*dst, result, cur);
                // Continue lowering subsequent instructions in the final block.
                bb = cur;
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
                self.push_inst(bb, inst);
                self.store_to_local(*dst, result, bb);
            }

            Instruction::Cmp {
                dst,
                op,
                type_id,
                lhs,
                rhs,
            } => {
                // Check if either operand is Null — that's a pointer null-check
                // (e.g. Option unwrap), not a string content comparison.
                let has_null = matches!(lhs, Operand::Constant(Constant::Null))
                    || matches!(rhs, Operand::Constant(Constant::Null));
                let is_string_type = match self.gir_types.get(*type_id) {
                    Some(GirType::Named(name)) if name == "GorgetString" => true,
                    Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => {
                        matches!(self.gir_types.get(*inner), Some(GirType::Named(n)) if n == "GorgetString")
                    }
                    _ => false,
                };
                let is_string = is_string_type && !has_null;

                if is_string {
                    let l = self.lower_operand(lhs, bb);
                    let r = self.lower_operand(rhs, bb);
                    let lir_op = map_cmp_op(*op);
                    let str_ty = self.struct_reg.lookup("GorgetString")
                        .map(LirType::Struct).unwrap_or(LirType::Ptr);

                    match lir_op {
                        CmpOp::Eq | CmpOp::Ne => {
                            // gorget_str_eq(lhs, rhs) → bool
                            self.ensure_extern("gorget_str_eq",
                                &[str_ty.clone(), str_ty.clone()], &LirType::Bool);
                            let abis = self.lookup_arg_abis("gorget_str_eq");
                            let eq_result = self.lir_func.next_value();
                            self.push_inst(bb, Inst::CallExtern {
                                dst: Some(eq_result),
                                name: "gorget_str_eq".to_string(),
                                args: vec![l, r],
                                arg_abis: abis,
                            });
                            if lir_op == CmpOp::Ne {
                                let not_result = self.lir_func.next_value();
                                self.push_inst(bb, Inst::Not {
                                    dst: not_result,
                                    operand: eq_result,
                                });
                                self.store_to_local(*dst, not_result, bb);
                            } else {
                                self.store_to_local(*dst, eq_result, bb);
                            }
                        }
                        _ => {
                            // gorget_str_cmp(lhs, rhs) → int, then compare with 0
                            self.ensure_extern("gorget_str_cmp",
                                &[str_ty.clone(), str_ty.clone()], &LirType::I64);
                            let abis = self.lookup_arg_abis("gorget_str_cmp");
                            let cmp_result = self.lir_func.next_value();
                            self.push_inst(bb, Inst::CallExtern {
                                dst: Some(cmp_result),
                                name: "gorget_str_cmp".to_string(),
                                args: vec![l, r],
                                arg_abis: abis,
                            });
                            let zero = self.emit_i64_const(bb, 0);
                            let result = self.lir_func.next_value();
                            self.push_inst(bb, Inst::Cmp {
                                dst: result,
                                op: lir_op,
                                lhs: cmp_result,
                                rhs: zero,
                            });
                            self.store_to_local(*dst, result, bb);
                        }
                    }
                } else {
                    let l = self.lower_operand(lhs, bb);
                    let r = self.lower_operand(rhs, bb);
                    let result = self.lir_func.next_value();
                    self.push_inst(bb, Inst::Cmp {
                        dst: result,
                        op: map_cmp_op(*op),
                        lhs: l,
                        rhs: r,
                    });
                    self.store_to_local(*dst, result, bb);
                }
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
                    // String-to-String cast: source is already a GorgetString struct.
                    // This happens when e.g. `char_at(i) as String` — char_at returns String.
                    // val is a Ptr (SlotAddr of aggregate). Clone via gorget_string_clone.
                    let str_sid = self.struct_reg.lookup("GorgetString")
                        .or_else(|| self.struct_reg.lookup("Str"));
                    let is_str_source = src_gir_ty.map_or(false, |t| {
                        matches!(self.map_type(&t), LirType::Struct(sid) if Some(sid) == str_sid)
                    });
                    // Gorget-arena snag #1: `"literal" as String` —
                    // `infer_operand_type` types a `Constant::Str` as
                    // GorgetString, but the LIR-level Cast input is
                    // produced by `Inst::StrLit` (a `Str` struct value,
                    // not a pointer), and the `src_gir_ty` lookup for a
                    // `Constant::Str` falls through to `None` because
                    // the Constant arm above doesn't list it. Result:
                    // all three flags were false and the cast routed to
                    // the `gorget_int_to_str` fallback, then cc choked
                    // on the `Str → int64_t` mismatch. Treat
                    // `Constant::Str` like any other GorgetString source:
                    // route through `gorget_string_clone`, which the
                    // `is_str_source` branch handles by taking the
                    // address of the Str slot — exactly what the literal
                    // produces.
                    let is_str_literal = matches!(value, Operand::Constant(Constant::Str(_)));
                    let is_str_source = is_str_source || is_str_literal;

                    if is_ptr {
                        // Ptr source (const char*) → GorgetString: wrap directly with gorget_str_from_cstr.
                        let str_ty = self.struct_reg.lookup("GorgetString")
                            .map(LirType::Struct).unwrap_or(LirType::Ptr);
                        self.ensure_extern("gorget_str_from_cstr", &[LirType::Ptr], &str_ty);
                        let abis = self.lookup_arg_abis("gorget_str_from_cstr");
                        let cstr_result = self.lir_func.next_value();
                        self.push_inst(bb, Inst::CallExtern {
                            dst: Some(cstr_result),
                            name: "gorget_str_from_cstr".to_string(),
                            args: vec![val],
                            arg_abis: abis,
                        });
                        self.store_to_local(*dst, cstr_result, bb);
                    } else if is_str_source {
                        // String → String: no-op cast. Clone the source to produce an owned copy.
                        // val is a Ptr (SlotAddr) since GorgetString is an aggregate.
                        let str_ty = str_sid.map(LirType::Struct).unwrap_or(LirType::Ptr);
                        self.ensure_extern("gorget_string_clone", &[LirType::Ptr], &str_ty);
                        let abis = self.lookup_arg_abis("gorget_string_clone");
                        let clone_result = self.lir_func.next_value();
                        self.push_inst(bb, Inst::CallExtern {
                            dst: Some(clone_result),
                            name: "gorget_string_clone".to_string(),
                            args: vec![val],
                            arg_abis: abis,
                        });
                        self.store_to_local(*dst, clone_result, bb);
                    } else {
                    let conv_fn = if is_int {
                        "gorget_int_to_str"
                    } else if is_float {
                        "gorget_float_to_str"
                    } else if is_bool {
                        "gorget_bool_to_str"
                    } else {
                        // Unknown source — use int_to_str as best-effort fallback.
                        "gorget_int_to_str"
                    };
                    let str_ty = if let Some(sid) = self.struct_reg.lookup("Str") { LirType::Struct(sid) } else { LirType::Ptr };
                    self.ensure_extern(conv_fn, &[if is_float { LirType::F64 } else if is_bool { LirType::Bool } else { LirType::I64 }], &str_ty);
                    let abis = self.lookup_arg_abis(conv_fn);
                    // Emit CallExtern to the conversion function (returns const char*).
                    let cstr_result = self.lir_func.next_value();
                    self.push_inst(bb, Inst::CallExtern {
                        dst: Some(cstr_result),
                        name: conv_fn.to_string(),
                        args: vec![val],
                        arg_abis: abis,
                    });
                    // The result is a Str struct (returned by gorget_string_adopt in the C runtime).
                    self.store_to_local(*dst, cstr_result, bb);
                    } // close else (non-ptr/non-str) branch
                } else if matches!(to, LirType::Void) {
                    // Cast to void — just evaluate for side effects, don't generate (void)(val).
                    // No store needed.
                } else {
                    // GorgetString → int: extract first codepoint via gorget_str_ord.
                    let src_is_str = match value {
                        Operand::Copy(place) | Operand::Move(place) => {
                            let idx = place.local.0 as usize;
                            if idx < self.gir_func.locals.len() {
                                let gir_ty = self.gir_func.locals[idx].type_id;
                                match self.gir_types.get(gir_ty) {
                                    Some(GirType::Named(n)) if n == "GorgetString" => true,
                                    Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => {
                                        matches!(self.gir_types.get(*inner), Some(GirType::Named(n)) if n == "GorgetString")
                                    }
                                    _ => false,
                                }
                            } else { false }
                        }
                        _ => false,
                    };
                    if src_is_str && to.is_integer() {
                        let str_ty = self.struct_reg.lookup("GorgetString")
                            .map(LirType::Struct).unwrap_or(LirType::Ptr);
                        self.ensure_extern("gorget_str_ord",
                            &[str_ty], &LirType::I64);
                        let abis = self.lookup_arg_abis("gorget_str_ord");
                        let ord_result = self.lir_func.next_value();
                        self.push_inst(bb, Inst::CallExtern {
                            dst: Some(ord_result),
                            name: "gorget_str_ord".to_string(),
                            args: vec![val],
                            arg_abis: abis,
                        });
                        self.store_to_local(*dst, ord_result, bb);
                    } else {
                        // Pick the right LIR cast kind based on source/target numeric
                        // families so float→int goes through `Inst::FloatToInt`
                        // (saturating, Rust-style) instead of a raw `Inst::IntCast`
                        // that emits a UB C cast.
                        let src_gir_ty = match value {
                            Operand::Copy(place) | Operand::Move(place) => {
                                let idx = place.local.0 as usize;
                                if idx < self.gir_func.locals.len() {
                                    Some(self.gir_func.locals[idx].type_id)
                                } else { None }
                            }
                            Operand::Constant(c) => match c {
                                Constant::F32(_) | Constant::F64(_) => Some(gir_types::F64_TYPE),
                                _ => None,
                            },
                        };
                        let src_is_float = src_gir_ty.map_or(false, |t| {
                            t == gir_types::F64_TYPE || t == gir_types::F32_TYPE
                        });
                        let result = self.lir_func.next_value();
                        let inst = if src_is_float && to.is_integer() {
                            Inst::FloatToInt { dst: result, value: val, to }
                        } else if src_is_float && to.is_float() {
                            Inst::FloatCast { dst: result, value: val, to }
                        } else if !src_is_float && to.is_float() {
                            Inst::IntToFloat { dst: result, value: val, to }
                        } else {
                            Inst::IntCast { dst: result, value: val, to }
                        };
                        self.push_inst(bb, inst);
                        self.store_to_local(*dst, result, bb);
                    }
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
                self.push_inst(bb, Inst::Bitcast {
                    dst: result,
                    value: val,
                    to,
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::PtrCast { dst, value, .. } => {
                let val = self.lower_operand(value, bb);
                let result = self.lir_func.next_value();
                self.push_inst(bb, Inst::PtrCast { dst: result, value: val });
                self.store_to_local(*dst, result, bb);
            }

            // -- Calls --
            Instruction::Call { dst, func, args, .. } => {
                // Intercept unwrap/expect pseudo-functions before func_index lookup.
                if is_unwrap_like_name(func) {
                    let lir_args: Vec<ValueId> =
                        args.iter().map(|a| self.lower_operand(a, bb)).collect();
                    let emit_name = func.clone();
                    bb = self.emit_extern_call(func, &emit_name, dst, args, lir_args, bb);
                } else if let Some(fid) = self.func_index.get(func) {
                    let mut lir_args: Vec<ValueId> =
                        args.iter().map(|a| self.lower_operand(a, bb)).collect();
                    // Closure→callable wrapping: detect __Closure_N args and FuncRef
                    // args and pack them into GorgetClosure slots so backends don't
                    // need to detect and wrap at code-gen time.
                    if !func.contains("__call") {
                        self.wrap_closure_call_args(args, &mut lir_args, bb);
                    }
                    let result = dst.map(|_| self.lir_func.next_value());
                    self.push_inst(bb, Inst::Call {
                        dst: result,
                        func: *fid,
                        args: lir_args,
                    });
                    if let (Some(d), Some(r)) = (*dst, result) {
                        self.store_to_local(d, r, bb);
                    }
                    // Post-call zeroing for Move operands (Rust-style ownership).
                    self.emit_post_call_zeros(args, bb);
                } else {
                    // Unknown function — treat as extern.
                    // Map monomorphized collection/method names to runtime function names.
                    // Item 7e-r2: route through the typed overload so sort/sorted/unique
                    // dispatch consults `operand_types[0]` (receiver's LirType) instead
                    // of stripping `Vector__` off the callee name. Pre-7e-r1 most operands
                    // arrive as `Struct(sid)` rather than `Resource{..}`, so the typed
                    // fast path is a no-op and the legacy name-strip path inside
                    // `map_monomorphized_to_runtime` handles them.
                    let operand_types: Vec<Option<LirType>> = args.iter()
                        .map(|a| Some(self.operand_lir_type(a)))
                        .collect();
                    let emit_name = map_monomorphized_to_runtime_with_operand_types(
                        func, &operand_types, self.runtime_callees)
                        .unwrap_or_else(|| func.clone());
                    // For collection/concurrency methods that take self by pointer
                    // (SelfConvention::Borrow | MutBorrow), if the first arg is a
                    // GlobalRef, emit GlobalAddr (pointer) instead of GlobalAddr+Load
                    // (copy), so mutations affect the global.  The flag is set from
                    // BuiltinMethodDecl.self_conv at protocol registration time and
                    // carried through runtime_callees — no name-prefix tests here.
                    let needs_self_by_ptr = self.runtime_callees.get(func)
                        .map_or(false, |info| info.self_by_ptr);
                    let lir_args: Vec<ValueId> =
                        args.iter().enumerate().map(|(i, a)| {
                            if i == 0 && needs_self_by_ptr {
                                if let Operand::Constant(Constant::GlobalRef(name)) = a {
                                    if let Some(&gid) = self.global_index.get(name) {
                                        let addr = self.lir_func.next_value();
                                        self.push_inst(bb, 
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
                        // Read typed `collection_kind` (Phase A) — covers
                        // Vector/Deque/GorgetArray (Array), Dict (OrderedMap),
                        // HashMap/GorgetMap (Map), Set (OrderedSet), HashSet/
                        // GorgetSet (Set). Replaces six name-prefix arms.
                        let kind = arg_type.as_deref()
                            .and_then(|n| self.gir_types.get_type_def(n))
                            .and_then(|td| td.metadata.collection_kind);
                        use crate::ir::types::CollectionKind;
                        if kind == Some(CollectionKind::Array) {
                            "gorget_array_len".to_string()
                        } else if matches!(kind, Some(CollectionKind::OrderedMap) | Some(CollectionKind::Map)) {
                            "gorget_map_len".to_string()
                        } else if matches!(kind, Some(CollectionKind::OrderedSet) | Some(CollectionKind::Set)) {
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
                                self.push_inst(bb, Inst::Call {
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
                    // Same VoidElem-driven wrap as the CallExtern path above —
                    // monomorphized collection methods like
                    // `Vector__Callable__push` land here (not via func_index)
                    // and get rewritten to `gorget_array_push` etc. Look up
                    // the resolved runtime name's ABI tags and wrap any closure
                    // arg whose param is `VoidElem` (attack_82). Picking by
                    // ABI rather than by callee name means new runtimes that
                    // adopt `VoidElem` participate automatically.
                    let abis = crate::lir::runtime::RuntimeFn::from_c_name(&emit_name)
                        .map(|f| f.resolve_lir_sig(self.struct_reg).param_abis)
                        .unwrap_or_default();
                    if !abis.is_empty() {
                        self.wrap_closure_args_at_void_elem(args, &mut lir_args, &abis, bb);
                    }
                    // Delegate to the shared extern-call emitter (same logic as CallExtern).
                    if !len_handled {
                        bb = self.emit_extern_call(func, &emit_name, dst, args, lir_args, bb);
                    }
                }
            }

            Instruction::CallExtern { dst, func, args } => {
                // Intercept unwrap/expect before func_index — these pseudo-functions
                // may be in func_index but have no C implementation.
                if is_unwrap_like_name(func) {
                    let lir_args: Vec<ValueId> =
                        args.iter().map(|a| self.lower_operand(a, bb)).collect();
                    let emit_name = func.clone();
                    bb = self.emit_extern_call(func, &emit_name, dst, args, lir_args, bb);
                }
                // If the callee is actually a defined function in this module (GIR uses
                // call_extern for user-defined iterator/trait methods), emit a direct Call.
                else if let Some(fid) = self.func_index.get(func) {
                    let lir_args: Vec<ValueId> =
                        args.iter().map(|a| self.lower_operand(a, bb)).collect();
                    let result = dst.map(|_| self.lir_func.next_value());
                    self.push_inst(bb, Inst::Call {
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
                // Item 7e-r2: route through the typed overload (see Call branch above).
                let operand_types: Vec<Option<LirType>> = args.iter()
                    .map(|a| Some(self.operand_lir_type(a)))
                    .collect();
                let mut emit_name = map_monomorphized_to_runtime_with_operand_types(
                    func, &operand_types, self.runtime_callees)
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
                let mut lir_args: Vec<ValueId> = if is_printf_like {
                    // For printf, expand Str-typed args into (int)len, data pairs.
                    self.lower_printf_args(args, bb)
                } else {
                    {
                    // Same self-by-ptr decision as the non-printf path above —
                    // driven by runtime_callees.self_by_ptr, not name-prefix tests.
                    let needs_self_by_ptr = self.runtime_callees.get(func)
                        .map_or(false, |info| info.self_by_ptr);
                    args.iter().enumerate().map(|(i, a)| {
                        if i == 0 && needs_self_by_ptr {
                            if let Operand::Constant(Constant::GlobalRef(name)) = a {
                                if let Some(&gid) = self.global_index.get(name) {
                                    let addr = self.lir_func.next_value();
                                    self.push_inst(bb, 
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
                // Closure→callable wrapping for extern calls too — without this,
                // `Vector[Callable].push(closure_literal)` lowers to
                // `gorget_array_push(&__Closure_N_env)` and the runtime memcpys
                // sizeof(GorgetClosure)=16 bytes from a smaller env struct,
                // leaving the closure's env pointer uninitialized
                // (attack_82_vector_of_closures.gg). Skip for printf-like to
                // preserve their custom Str→(len, data) expansion.
                // Closure→callable wrapping at extern positions tagged
                // `AbiKind::VoidElem` (collection element pointer — push, set,
                // put, add, insert, send). The runtime memcpys `elem_size`
                // bytes from the arg pointer into the slot, so for
                // `Vector[Callable].push(closure)` the arg must already point
                // at a packed `GorgetClosure` (16 bytes), not the source
                // `__Closure_N` env struct (attack_82). Other ABIs (struct,
                // scalar, etc.) leave the arg untouched, so combinators like
                // `Result.map_err` still receive the closure as a struct value.
                if !is_printf_like {
                    let abis = crate::lir::runtime::RuntimeFn::from_c_name(&emit_name)
                        .map(|f| f.resolve_lir_sig(self.struct_reg).param_abis)
                        .unwrap_or_default();
                    if !abis.is_empty() {
                        self.wrap_closure_args_at_void_elem(args, &mut lir_args, &abis, bb);
                    }
                }
                bb = self.emit_extern_call(func, &emit_name, dst, args, lir_args, bb);
                // Free the String temps `lower_printf_args` synthesized for
                // this call (bool→str conversions). The call has consumed
                // their bytes (printf wrote them / string_format copied
                // them), so the free is sound and closes the print-temp
                // leak class at the temp's birth layer.
                if !self.printf_str_temps.is_empty() {
                    let temps = std::mem::take(&mut self.printf_str_temps);
                    self.ensure_extern("gorget_string_free", &[LirType::Ptr], &LirType::Void);
                    let abis = self.lookup_arg_abis("gorget_string_free");
                    for slot in temps {
                        let addr = self.lir_func.next_value();
                        self.push_inst(bb, Inst::SlotAddr { dst: addr, slot });
                        self.push_inst(bb, Inst::CallExtern {
                            dst: None,
                            name: "gorget_string_free".to_string(),
                            args: vec![addr],
                            arg_abis: abis.clone(),
                        });
                    }
                }
                }
            }

            Instruction::CallIndirect { dst, callee, args } => {
                let callee_val = self.lower_operand(callee, bb);
                let lir_args: Vec<ValueId> =
                    args.iter().map(|a| self.lower_operand(a, bb)).collect();
                let result = dst.map(|_| self.lir_func.next_value());
                let ret_ty = match dst {
                    Some(d) => {
                        let gir_ty = self.gir_func.locals[d.0 as usize].type_id;
                        self.map_type(&gir_ty)
                    }
                    None => LirType::Void,
                };
                self.push_inst(bb, Inst::CallPtr {
                    dst: result,
                    callee: callee_val,
                    args: lir_args,
                    ret_ty,
                });
                if let (Some(d), Some(r)) = (*dst, result) {
                    self.store_to_local(d, r, bb);
                }
                // Post-call zeroing for Move operands (Rust-style ownership).
                self.emit_post_call_zeros(args, bb);
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
                self.push_inst(bb, Inst::SlotAddr {
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

                // Collect field-index/value pairs that StructInit will write.
                // Null-for-enum-field needs special handling (nested EnumInit) and
                // is emitted inline; everything else flows through StructInit.
                let mut init_fields: Vec<(u32, ValueId)> = Vec::with_capacity(fields.len());

                for (i, field_op) in fields.iter().enumerate() {
                    // Special-case: Null operand for an enum-typed field (e.g. Option<T> = None).
                    // Instead of emitting NullPtr (memcpy from NULL → segfault), properly
                    // initialize the field with the null variant tag.
                    if matches!(field_op, Operand::Constant(Constant::Null)) {
                        if let Some(Some(fty)) = field_type_ids.get(i) {
                            if let Some((field_enum_sid, tag_ordinal)) = self.find_enum_null_variant(*fty) {
                                // The parent struct slot is zero-initialized (= {0}), so the
                                // payload bytes are already zero. We only need to initialize the
                                // nested enum's tag — emit FieldPtr + EnumInit (payload=None).
                                let fptr = self.lir_func.next_value();
                                self.push_inst(bb, Inst::FieldPtr {
                                    dst: fptr,
                                    base,
                                    struct_id,
                                    field: i as u32,
                                });
                                self.push_inst(bb, Inst::EnumInit {
                                    target: fptr,
                                    struct_id: field_enum_sid,
                                    variant_tag: tag_ordinal as u32,
                                    fields: vec![],
                                });
                                continue;
                            }
                            // Non-enum aggregate field: struct is already zero-initialized,
                            // so Null is a no-op. Skip to avoid NullPtr → Store(aggregate)
                            // which forces backends to scan for null origin.
                            let field_lir_ty = self.map_type(fty);
                            if field_lir_ty.is_aggregate() {
                                continue;
                            }
                        }
                    }

                    let val = self.lower_operand(field_op, bb);
                    init_fields.push((i as u32, val));
                }

                if !init_fields.is_empty() {
                    self.push_inst(bb, Inst::StructInit {
                        target: base,
                        struct_id,
                        fields: init_fields,
                    });
                }
            }

            Instruction::FieldLoad {
                dst,
                base,
                field,
            } => {
                let mut base_val = self.lower_place_addr(base, bb);
                // Use effective type after base projections (e.g., Deref→Field chain).
                let effective_type = self.effective_place_type(base);
                // If the effective type is a pointer (e.g., closure env param),
                // load the pointer value first so FieldPtr operates on the struct, not the slot.
                // Skip for ref_locals — they're already pointers from collection reads;
                // lower_place_addr already does the SlotLoad to get the pointer value.
                // §6.8 Stage 4: was `ownership.is_ref()`.
                let is_ref_local = base.projections.is_empty()
                    && self.gir_func.locals.get(base.local.0 as usize)
                        .map_or(false, |l| l.slot_kind == crate::ir::SlotKind::BorrowedPtr);
                if !is_ref_local && matches!(self.gir_types.get(effective_type), Some(GirType::Ptr(_) | GirType::MutPtr(_))) {
                    let deref = self.lir_func.next_value();
                    self.push_inst(bb, Inst::Load {
                        dst: deref,
                        ptr: base_val,
                        ty: LirType::Ptr,
                    });
                    base_val = deref;
                }
                let struct_id = self.resolve_struct_id_for_field(effective_type, *field, self.module_structs);
                let fptr = self.lir_func.next_value();
                self.push_inst(bb, Inst::FieldPtr {
                    dst: fptr,
                    base: base_val,
                    struct_id,
                    field: *field,
                });
                // If destination is Ptr(T), return field address as pointer reference.
                // Exception: if the FIELD itself is already a Ptr/MutPtr (user-written
                // `Ref[T]` / `MutRef[T]` field), the field's storage holds a pointer
                // value — we must Load it through fptr, not return fptr (which would
                // be a pointer-to-pointer-field, not the stored pointer).
                let dst_gir_type = self.gir_func.locals[dst.0 as usize].type_id;
                let field_gir_id = self.resolve_field_gir_type_id(effective_type, *field);
                let field_is_ptr = matches!(self.gir_types.get(field_gir_id),
                    Some(GirType::Ptr(_) | GirType::MutPtr(_)));
                if matches!(self.gir_types.get(dst_gir_type), Some(GirType::Ptr(_))) && !field_is_ptr {
                    self.store_to_local(*dst, fptr, bb);
                } else {
                    let field_ty = self.resolve_field_type(effective_type, *field);
                    // If field is Ptr but dst is a value type (Str), double-deref:
                    // load Ptr from field, then load Str value through Ptr.
                    let dst_slot = self.local_to_slot[dst.0 as usize];
                    let dst_slot_ty = self.lir_func.slots[dst_slot.0 as usize].ty.clone();
                    if matches!(field_ty, LirType::Ptr) && dst_slot_ty.is_aggregate() {
                        let ptr_val = self.lir_func.next_value();
                        self.push_inst(bb, Inst::Load {
                            dst: ptr_val, ptr: fptr, ty: LirType::Ptr,
                        });
                        let result = self.lir_func.next_value();
                        self.push_inst(bb, Inst::Load {
                            dst: result, ptr: ptr_val, ty: dst_slot_ty,
                        });
                        self.store_to_local(*dst, result, bb);
                    } else {
                        let result = self.lir_func.next_value();
                        self.push_inst(bb, Inst::Load {
                            dst: result,
                            ptr: fptr,
                            ty: field_ty,
                        });
                        self.store_to_local(*dst, result, bb);
                    }
                }
            }

            Instruction::IndexLoad { dst, base, index, read } => {
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
                // Read typed `collection_kind` (Phase A) — both runtime
                // singletons and monomorphized aliases carry it.
                let kind = self.gir_types.get_type_def(&base_type_name)
                    .and_then(|td| td.metadata.collection_kind);
                let is_array = kind == Some(crate::ir::types::CollectionKind::Array);
                let is_dict = matches!(kind,
                    Some(crate::ir::types::CollectionKind::OrderedMap)
                    | Some(crate::ir::types::CollectionKind::Map));

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
                    self.push_inst(bb, Inst::FieldPtr {
                        dst: start_ptr, base: range_val, struct_id: range_sid, field: 0,
                    });
                    let start = self.lir_func.next_value();
                    self.push_inst(bb, Inst::Load {
                        dst: start, ptr: start_ptr, ty: LirType::I64,
                    });
                    let end_ptr = self.lir_func.next_value();
                    self.push_inst(bb, Inst::FieldPtr {
                        dst: end_ptr, base: range_val, struct_id: range_sid, field: 1,
                    });
                    let end = self.lir_func.next_value();
                    self.push_inst(bb, Inst::Load {
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
                    let abis = self.lookup_arg_abis(fn_name);
                    let result = self.lir_func.next_value();
                    self.push_inst(bb, Inst::CallExtern {
                        dst: Some(result),
                        name: fn_name.to_string(),
                        args: vec![base_val, start, end],
                        arg_abis: abis,
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
                    let abis = self.lookup_arg_abis("gorget_str_index");
                    let result = self.lir_func.next_value();
                    self.push_inst(bb, Inst::CallExtern {
                        dst: Some(result),
                        name: "gorget_str_index".to_string(),
                        args: vec![base_val, idx],
                        arg_abis: abis,
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
                    // §6.8 Stage 4: was `ownership.is_ref()`.
                    let is_ref_local = self.gir_func.locals.get(base.local.0 as usize)
                        .map_or(false, |l| l.slot_kind == crate::ir::SlotKind::BorrowedPtr);
                    if matches!(self.gir_types.get(base_gir), Some(GirType::Ptr(_)))
                        && base.projections.is_empty()
                        && !is_ref_local
                    {
                        let deref = self.lir_func.next_value();
                        self.push_inst(bb, Inst::Load {
                            dst: deref, ptr: base_val, ty: LirType::Ptr,
                        });
                        base_val = deref;
                    }
                    let idx = self.lower_operand(index, bb);
                    let fn_name = if is_dict { "gorget_map_get" } else { "gorget_array_get" };
                    self.ensure_extern(fn_name, &[LirType::Ptr, LirType::I64], &LirType::Ptr);
                    let abis = self.lookup_arg_abis(fn_name);
                    let ptr_val = self.lir_func.next_value();
                    self.push_inst(bb, Inst::CallExtern {
                        dst: Some(ptr_val),
                        name: fn_name.to_string(),
                        args: vec![base_val, idx],
                        arg_abis: abis,
                    });
                    // Materialize the element from `ptr_val` (Ptr-return-vs-deref
                    // split + clone/move-zero/str-ptr logic). SHARED with the
                    // faultable-index path (error-model.md §11 `Fault.Bounds`).
                    self.materialize_collection_element(*dst, ptr_val, &base_type_name, *read, bb);
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
                    self.push_inst(bb, Inst::ElemPtr {
                        dst: elem_ptr,
                        base: base_val,
                        index: idx,
                        elem_size,
                    });
                    let result = self.lir_func.next_value();
                    self.push_inst(bb, Inst::Load {
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
                self.push_inst(bb, Inst::SlotAddr {
                    dst: base,
                    slot,
                });

                let tag_ordinal = self.resolve_variant_ordinal(type_name, variant);
                let field_offset = self.resolve_variant_field_offset(type_name, variant);
                // Look up field types for Null → enum promotion (same as StructInit).
                let variant_field_types = self.resolve_variant_field_types(type_name, variant);

                // Collect payload (field_index, value) pairs for canonical
                // `Inst::EnumInit`. `Null`-for-nested-enum fields are handled
                // inline by emitting a per-field `FieldPtr + EnumInit` pair —
                // those nested inits write their own tag byte into the parent
                // struct's zero-initialized slot, so they must stay separate.
                let mut init_fields: Vec<(u32, ValueId)> = Vec::with_capacity(fields.len());
                let mut nested_null_inits: Vec<(u32, StructId, u32)> = Vec::new();

                for (i, field_op) in fields.iter().enumerate() {
                    let abs_field_idx = (field_offset + i) as u32;
                    if matches!(field_op, Operand::Constant(Constant::Null)) {
                        if let Some(Some(fty)) = variant_field_types.get(i) {
                            if let Some((field_enum_sid, fld_tag_ordinal)) = self.find_enum_null_variant(*fty) {
                                nested_null_inits.push((abs_field_idx, field_enum_sid, fld_tag_ordinal as u32));
                                continue;
                            }
                        }
                    }
                    let val = self.lower_operand(field_op, bb);
                    init_fields.push((abs_field_idx, val));
                }

                // Emit the canonical EnumInit — writes tag + all non-Null payload fields.
                self.push_inst(bb, Inst::EnumInit {
                    target: base,
                    struct_id,
                    variant_tag: tag_ordinal as u32,
                    fields: init_fields,
                });

                // For Null-for-nested-enum fields: emit FieldPtr to the payload
                // slot, then a unit EnumInit to set the nested enum's tag byte.
                for (abs_field_idx, nested_sid, nested_tag) in nested_null_inits {
                    let fptr = self.lir_func.next_value();
                    self.push_inst(bb, Inst::FieldPtr {
                        dst: fptr, base, struct_id,
                        field: abs_field_idx,
                    });
                    self.push_inst(bb, Inst::EnumInit {
                        target: fptr,
                        struct_id: nested_sid,
                        variant_tag: nested_tag,
                        fields: vec![],
                    });
                }

                // Post-init move: mark resource-type source slots as moved.
                // The GIR's move_zero_consumed_args + move_zero_and_mark on
                // error/rethrow paths handle mark_moved for scope-exit drops.
                // MoveSlot feeds the dataflow; C zero-initialization of locals
                // provides null-safety for uninitialized paths.
                for field_op in fields.iter() {
                    if let Operand::Copy(place) | Operand::Move(place) = field_op {
                        if place.projections.is_empty() {
                            let local_idx = place.local.0 as usize;
                            if local_idx < self.local_to_slot.len() {
                                let src_slot = self.local_to_slot[local_idx];
                                let src_ty = &self.lir_func.slots[src_slot.0 as usize].ty;
                                if let LirType::Struct(sid) = src_ty {
                                    let needs_move = self.module_structs.get(sid.0 as usize)
                                        .map_or(false, |s| matches!(s.name.as_str(),
                                            "GorgetArray" | "GorgetMap" | "GorgetSet" | "GorgetString" | "GorgetClosure"
                                        ));
                                    if needs_move {
                                        self.push_inst(bb, Inst::MoveSlot {
                                            slot: src_slot,
                                        });
                                    }
                                }
                            }
                        }
                    }
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
                self.push_inst(bb, Inst::FieldPtr {
                    dst: tag_ptr,
                    base: val,
                    struct_id,
                    field: 0,
                });
                let result = self.lir_func.next_value();
                self.push_inst(bb, Inst::Load {
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
                mode,
            } => {
                let mut base_val = self.lower_place_addr(base, bb);
                let gir_type_id = self.gir_func.locals[base.local.0 as usize].type_id;
                // If after resolving projections we still have a pointer type,
                // the base_val is a SlotAddr of a pointer local — load the pointer
                // to get the actual enum struct address.
                //
                // BUT skip the extra Load when `base` is a BorrowedPtr ref local:
                // `lower_place_addr` already emitted a `SlotLoad` for it
                // (returning the pointer value directly), so an additional
                // `Load` would dereference the pointee struct AS a pointer,
                // reading the first 8 bytes of the enum (its tag + padding)
                // as a bogus address. Mirrors the same `is_ref_local` skip
                // that `Instruction::FieldLoad` (line 789-792) has. Pre-fix,
                // `if item.tag is Some(h):` where `item.tag` is a struct
                // field whose `field_load` produces a `BorrowedPtr`-tagged
                // local silently mis-extracted `Some_0`.
                let is_ref_local = base.projections.is_empty()
                    && self.gir_func.locals.get(base.local.0 as usize)
                        .map_or(false, |l| l.slot_kind == crate::ir::SlotKind::BorrowedPtr);
                let effective_ty = self.effective_place_type(base);
                if !is_ref_local && matches!(self.gir_types.get(effective_ty), Some(GirType::Ptr(_) | GirType::MutPtr(_))) {
                    let deref = self.lir_func.next_value();
                    self.push_inst(bb, Inst::Load {
                        dst: deref,
                        ptr: base_val,
                        ty: LirType::Ptr,
                    });
                    base_val = deref;
                }
                let struct_id = self.resolve_struct_id(gir_type_id);
                let type_name = self.resolve_type_name(gir_type_id);
                let field_offset = self.resolve_variant_field_offset(&type_name, variant);
                let payload_field_idx = (field_offset + *field as usize) as u32;

                // If destination is Ptr(T), return the field address (not value)
                // — but only when the enum's variant field holds T by value.
                // For Option[Ref[T]] / Option[Ref_T], the Some_0 field is already
                // a Ptr, so taking &Some_0 produces a void** (wrong indirection
                // level). In that case, EnumExtract loads the pointer value
                // directly and stores it into dst's slot.
                let dst_gir_type = self.gir_func.locals[dst.0 as usize].type_id;
                let variant_field_is_ptr = self.gir_types
                    .get_type_def(&type_name)
                    .and_then(|def| match &def.kind {
                        ir::types::TypeDefKind::Enum(e) => Some(e),
                        _ => None,
                    })
                    .and_then(|e| e.variants.iter().find(|v| v.name == *variant))
                    .and_then(|v| v.fields.get(*field as usize))
                    .map_or(false, |f| matches!(
                        self.gir_types.get(f.type_id),
                        Some(GirType::Ptr(_) | GirType::MutPtr(_))
                    ));
                if matches!(self.gir_types.get(dst_gir_type), Some(GirType::Ptr(_)))
                    && !variant_field_is_ptr
                {
                    let fptr = self.lir_func.next_value();
                    self.push_inst(bb, Inst::FieldPtr {
                        dst: fptr,
                        base: base_val,
                        struct_id,
                        field: payload_field_idx,
                    });
                    self.store_to_local(*dst, fptr, bb);
                } else {
                    let field_ty = self.resolve_enum_field_type(gir_type_id, variant, *field);
                    // Check BEFORE field_ty is moved into the EnumExtract
                    // instruction. Any payload type whose drop frees shared
                    // bytes (string / collection thin pointers, recursive
                    // structs/enums with custom drop) needs the source field
                    // zeroed after extraction to prevent shallow-copy
                    // double-free — see `Instruction::EnumFieldLoad`'s contract
                    // on the GIR side. Was previously gated to GorgetString
                    // only ("is_str_field"); 2026-05-06 widening makes this
                    // unconditional for all resource payloads, dropping the
                    // ShallowCopyOfEnumPayload validator class to zero.
                    let payload_is_resource = self.payload_needs_post_extract_zero(&field_ty);
                    // Canonical `Inst::EnumExtract` — carries the struct_id +
                    // payload_field + declared field type explicitly. BIR
                    // expands to `FieldPtr + Load` (same as before).
                    let result = self.lir_func.next_value();
                    self.push_inst(bb, Inst::EnumExtract {
                        dst: result,
                        value: base_val,
                        struct_id,
                        payload_field: payload_field_idx,
                        ty: field_ty,
                    });
                    self.store_to_local(*dst, result, bb);
                    // Resource payload: zero the source field after extraction
                    // to prevent double-free. Extraction copies the thin
                    // pointer / handle bytes — both source and dest now alias
                    // the same heap allocation. The Store of NullPtr through
                    // a FieldPtr lowers to `memset(fptr, 0, sizeof(field_ty))`
                    // in c_lir (FieldPtr's ptr_pointee is the field type), so
                    // the whole struct payload is zeroed regardless of size.
                    // Skip the source-zero step for Borrow-mode reads
                    // (lower_pattern_condition tests). The Move-mode default
                    // (emit_pattern_bindings) keeps the destructive
                    // shallow-copy-safety semantic.
                    let do_zero = payload_is_resource
                        && matches!(mode, crate::ir::instructions::EnumFieldLoadMode::Move);
                    if do_zero {
                        let fptr = self.lir_func.next_value();
                        self.push_inst(bb, Inst::FieldPtr {
                            dst: fptr,
                            base: base_val,
                            struct_id,
                            field: payload_field_idx,
                        });
                        let null_val = self.lir_func.next_value();
                        self.push_inst(bb, Inst::NullPtr { dst: null_val });
                        self.push_inst(bb, Inst::Store {
                            ptr: fptr,
                            value: null_val,
                        });
                    }
                }
            }

            Instruction::TupleInit { dst, elements } => {
                // Tuples are stored as struct slots. Canonical `Inst::StructInit`
                // carries the explicit struct_id + (field_index, value) pairs;
                // BIR expansion produces the same FieldPtr + Store sequence the
                // open-coded version emitted. Parity with `Instruction::StructInit`.
                let slot = self.local_to_slot[dst.0 as usize];
                let base = self.lir_func.next_value();
                self.push_inst(bb, Inst::SlotAddr {
                    dst: base,
                    slot,
                });
                let gir_type_id = self.gir_func.locals[dst.0 as usize].type_id;
                let struct_id = self.resolve_struct_id(gir_type_id);

                let init_fields: Vec<(u32, ValueId)> = elements.iter()
                    .enumerate()
                    .map(|(i, elem)| (i as u32, self.lower_operand(elem, bb)))
                    .collect();

                if !init_fields.is_empty() {
                    self.push_inst(bb, Inst::StructInit {
                        target: base,
                        struct_id,
                        fields: init_fields,
                    });
                }
            }

            // -- Ownership / lifetime (pass-through as calls or nops) --
            Instruction::Drop { place } => {
                self.lower_drop(place, bb, false);
            }

            Instruction::DropIfAlive { place } => {
                self.lower_drop(place, bb, true);
            }

            Instruction::MoveZero { place } => {
                // Mark a place as moved after ownership transfer.
                // Simple (non-projected) places: MoveSlot only (zero runtime cost).
                // Projected places (field-level moves): Memset (MoveSlot is whole-slot).
                if place.projections.is_empty() {
                    let slot = self.local_to_slot[place.local.0 as usize];
                    self.push_inst(bb, Inst::MoveSlot { slot });
                } else {
                    let addr = self.lower_place_addr(place, bb);
                    let zero = self.emit_i32_const(bb, 0);
                    let mut gir_type = self.gir_func.locals[place.local.0 as usize].type_id;
                    for proj in &place.projections {
                        match proj {
                            Projection::Field(field) => {
                                gir_type = self.resolve_field_gir_type_id(gir_type, *field);
                            }
                            Projection::Deref => {
                                gir_type = self.resolve_deref_gir_type_id(gir_type);
                            }
                            Projection::Index(_) => break,
                        }
                    }
                    let effective_ty = self.map_type(&gir_type);
                    let byte_size = match &effective_ty {
                        LirType::Struct(_) => c_sizeof_lir_type(&effective_ty, &self.module_structs) as i64,
                        _ => crate::lir::types::scalar_size(&effective_ty).unwrap_or(8) as i64,
                    };
                    let size = self.emit_i64_const(bb, byte_size);
                    self.push_inst(bb, Inst::Memset {
                        ptr: addr,
                        byte: zero,
                        size,
                    });
                }
            }

            Instruction::Borrow { dst, place } | Instruction::BorrowMut { dst, place } => {
                let addr = self.lower_place_addr(place, bb);
                self.store_to_local(*dst, addr, bb);
            }

            // -- Ref load/store (explicit Ptr dereference) --
            Instruction::LoadRef { dst, src } => {
                // Load through Ptr: deref src to get value, store in dst.
                //
                // `lower_place_addr` already resolves ref-locals (is_ref
                // ownership or PtrTo(GorgetString) slot) to the pointer VALUE
                // via SlotLoad. For those, a single Load with the pointee type
                // reads the pointee correctly.
                //
                // For a bare `Ptr(T)` local that *isn't* a ref-local (e.g. a
                // result of `.unwrap()` on `Option[Ref[T]]`, bound to a plain
                // local), `lower_place_addr` emits `SlotAddr` — src_addr is
                // `&slot`, and the slot contains the pointer bits. A single
                // Load with ty=T would read the pointer bits as T. We have to
                // load the Ptr value from the slot first, then deref it.
                let src_addr = self.lower_place_addr(src, bb);
                let src_type = self.effective_place_type(src);
                let pointee = self.resolve_deref_gir_type_id(src_type);
                let field_ty = self.map_type(&pointee);
                let has_deref = src.projections.first()
                    == Some(&crate::ir::instructions::Projection::Deref);
                // §6.8 Stage 4: was `ownership.is_ref()`.
                let local_ownership_is_ref = self.gir_func.locals
                    .get(src.local.0 as usize)
                    .map_or(false, |l| l.slot_kind == crate::ir::SlotKind::BorrowedPtr);
                let slot = self.local_to_slot[src.local.0 as usize];
                let is_ptr_to_slot = match &self.lir_func.slots[slot.0 as usize].ty {
                    LirType::PtrTo(sid) => self.struct_reg.lookup("GorgetString") == Some(*sid),
                    _ => false,
                };
                // Only double-load when `lower_place_addr` gave us a slot
                // address (not the pointer value). Matches the else branch in
                // `lower_place_addr` — bare local, not ref-local, not the
                // PtrTo(GorgetString) special case.
                let needs_two_step = matches!(
                    self.gir_types.get(src_type),
                    Some(GirType::Ptr(_) | GirType::MutPtr(_))
                ) && !has_deref && !local_ownership_is_ref && !is_ptr_to_slot;
                let deref_val = self.lir_func.next_value();
                if needs_two_step {
                    let ptr_val = self.lir_func.next_value();
                    self.push_inst(bb, Inst::Load {
                        dst: ptr_val,
                        ptr: src_addr,
                        ty: LirType::Ptr,
                    });
                    self.push_inst(bb, Inst::Load {
                        dst: deref_val,
                        ptr: ptr_val,
                        ty: field_ty,
                    });
                } else {
                    self.push_inst(bb, Inst::Load {
                        dst: deref_val,
                        ptr: src_addr,
                        ty: field_ty,
                    });
                }
                self.store_to_local(*dst, deref_val, bb);
            }
            Instruction::StoreRef { dst, value } => {
                // Store through Ptr: write value to the address held by dst.
                let val = self.lower_operand(value, bb);
                let dst_addr = self.lower_place_addr(dst, bb);
                // Deref the Ptr to get the target address
                let target = self.lir_func.next_value();
                self.push_inst(bb, Inst::Load {
                    dst: target,
                    ptr: dst_addr,
                    ty: LirType::Ptr,
                });
                self.push_inst(bb, Inst::Store {
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
                self.ensure_extern("__gorget_alloc", &[LirType::Ptr], &LirType::Ptr);
                let abis = self.lookup_arg_abis("__gorget_alloc");
                let result = self.lir_func.next_value();
                self.push_inst(bb, Inst::CallExtern {
                    dst: Some(result),
                    name: "__gorget_alloc".into(),
                    args: vec![alloc],
                    arg_abis: abis,
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
                self.ensure_extern("__gorget_alloc_array", &[LirType::I64, LirType::Ptr], &LirType::Ptr);
                let abis = self.lookup_arg_abis("__gorget_alloc_array");
                let result = self.lir_func.next_value();
                self.push_inst(bb, Inst::CallExtern {
                    dst: Some(result),
                    name: "__gorget_alloc_array".into(),
                    args: vec![cnt, alloc],
                    arg_abis: abis,
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::Dealloc { ptr, allocator } => {
                let p = self.lower_operand(ptr, bb);
                let a = self.lower_operand(allocator, bb);
                self.ensure_extern("__gorget_dealloc", &[LirType::Ptr, LirType::Ptr], &LirType::Void);
                let abis = self.lookup_arg_abis("__gorget_dealloc");
                self.push_inst(bb, Inst::CallExtern {
                    dst: None,
                    name: "__gorget_dealloc".into(),
                    args: vec![p, a],
                    arg_abis: abis,
                });
            }

            Instruction::LoadThreadLocal { dst, name } => {
                let tls_name = format!("__gorget_tls_{name}");
                self.ensure_extern(&tls_name, &[], &LirType::Ptr);
                let result = self.lir_func.next_value();
                self.push_inst(bb, Inst::CallExtern {
                    dst: Some(result),
                    name: tls_name,
                    args: vec![],
                    arg_abis: vec![],
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::PushAllocator { allocator } => {
                let alloc = self.lower_operand(allocator, bb);
                self.ensure_extern("__gorget_push_allocator", &[LirType::Ptr], &LirType::Void);
                let abis = self.lookup_arg_abis("__gorget_push_allocator");
                self.push_inst(bb, Inst::CallExtern {
                    dst: None,
                    name: "__gorget_push_allocator".into(),
                    args: vec![alloc],
                    arg_abis: abis,
                });
            }

            Instruction::PopAllocator => {
                self.ensure_extern("__gorget_pop_allocator", &[], &LirType::Void);
                self.push_inst(bb, Inst::CallExtern {
                    dst: None,
                    name: "__gorget_pop_allocator".into(),
                    args: vec![],
                    arg_abis: vec![],
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
                        self.push_inst(bb, Inst::SlotAddr {
                            dst: addr_dummy,
                            slot,
                        });
                        let val = self.lir_func.next_value();
                        // Emit InlineC with a dst, then store to slot.
                        self.push_inst(bb, Inst::InlineC {
                            dst: Some(val),
                            code: code.clone(),
                        });
                        self.push_inst(bb, Inst::SlotStore {
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
                    self.push_inst(bb, Inst::InlineC {
                        dst: None,
                        code: code.clone(),
                    });
                }
            }

            Instruction::GlobalAssign { name, value } => {
                if let Some(&gid) = self.global_index.get(name) {
                    let val = self.lower_operand(value, bb);
                    let addr = self.lir_func.next_value();
                    self.push_inst(bb, Inst::GlobalAddr { dst: addr, global: gid });
                    let global_ty = &self.module_globals[gid.0 as usize].ty;
                    if global_ty.is_scalar() {
                        // Scalar store: dereference and assign.
                        self.push_inst(bb, Inst::Store { ptr: addr, value: val });
                    } else {
                        // Aggregate store: memcpy.
                        self.push_inst(bb, Inst::Store { ptr: addr, value: val });
                    }
                }
            }

            Instruction::Nop => {
                self.push_inst(bb, Inst::Nop);
            }
        }
        bb
    }

    /// Materialize a collection element from a raw element pointer (`ptr_val`,
    /// the void* result of `gorget_array_get` / `gorget_map_get`). Single source
    /// of truth for the element Ptr-return-vs-deref split, the str-ptr marking,
    /// and the clone-vs-move-zero element handling (collection clone, recursive
    /// deep-clone, plain Load + move-zero `Memset`). `ptr_val` must be a VALID
    /// element pointer here.
    fn materialize_collection_element(
        &mut self,
        dst: ir::types::LocalId,
        ptr_val: ValueId,
        base_type_name: &str,
        read: crate::ir::instructions::ReadMode,
        bb: BlockId,
    ) {
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
            self.store_to_local(dst, ptr_val, bb);
            return;
        }
        // Otherwise dereference to get the actual element value.
        let dst_slot = self.local_to_slot[dst.0 as usize];
        let mut elem_ty = self.lir_func.slots[dst_slot.0 as usize].ty.clone();
        // Closures are 16 bytes (GorgetClosure) but may be typed as I64 in LIR.
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
        let clone_fn = clone_fn_for_collection_element(elem_type_name, self.gir_types, self.module_structs);

        if let Some(clone_fn_name) = clone_fn {
            // Borrow mode: zero-copy view instead of clone for strings.
            // ReadMode::Borrow at this site == legacy `borrow: true`.
            let is_borrow = matches!(read, crate::ir::instructions::ReadMode::Borrow);
            let actual_fn = if is_borrow && clone_fn_name == "gorget_string_clone_to_owned" {
                "gorget_string_borrow".to_string()
            } else {
                clone_fn_name
            };
            let ret_ty = elem_ty.clone();
            self.ensure_extern(&actual_fn, &[LirType::Ptr], &ret_ty);
            let abis = self.lookup_arg_abis(&actual_fn);
            let result = self.lir_func.next_value();
            self.push_inst(bb, Inst::CallExtern {
                dst: Some(result),
                name: actual_fn,
                args: vec![ptr_val],
                arg_abis: abis,
            });
            self.store_to_local(dst, result, bb);
        } else {
            let elem_drop = self.infer_drop_strategy(elem_type_name);
            if matches!(elem_drop, crate::ir::types::DropStrategy::Recursive) {
                // Recursive-drop struct: deep-clone via {Type}__clone(ptr)
                // to produce an independently-owned copy. The collection
                // retains its original element.
                let clone_fn = format!("{elem_type_name}__clone");
                let ret_ty = elem_ty.clone();
                self.ensure_extern(&clone_fn, &[LirType::Ptr], &ret_ty);
                let abis = self.lookup_arg_abis(&clone_fn);
                let result = self.lir_func.next_value();
                self.push_inst(bb, Inst::CallExtern {
                    dst: Some(result),
                    name: clone_fn,
                    args: vec![ptr_val],
                    arg_abis: abis,
                });
                self.store_to_local(dst, result, bb);
            } else {
                // Other non-collection element: Load + move-zero
                let result = self.lir_func.next_value();
                self.push_inst(bb, Inst::Load {
                    dst: result,
                    ty: elem_ty.clone(),
                    ptr: ptr_val,
                });
                self.store_to_local(dst, result, bb);
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
                    self.push_inst(bb, Inst::Memset {
                        ptr: ptr_val, byte: zero, size: sz,
                    });
                }
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
                    self.push_inst(bb, Inst::Call {
                        dst: result,
                        func: *fid,
                        args: lir_args,
                    });
                } else {
                    let arg_types: Vec<LirType> = args.iter()
                        .map(|a| self.operand_lir_type(a)).collect();
                    let ret_ty = dst.map(|d| {
                        let gir_ty = self.gir_func.locals[d.0 as usize].type_id;
                        self.map_type(&gir_ty)
                    }).unwrap_or(LirType::Void);
                    self.ensure_extern(func, &arg_types, &ret_ty);
                    let abis = self.lookup_arg_abis(func);
                    self.push_inst(bb, Inst::CallExtern {
                        dst: result,
                        name: func.clone(),
                        args: lir_args,
                        arg_abis: abis,
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
                            self.push_inst(bb, Inst::SlotAddr {
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

    // ── Post-call zeroing for Move operands ────────────────────────────────

    /// Check whether a GIR local's type needs zeroing after ownership transfer.
    /// Returns true for types with Custom, Recursive, or Trivial drop strategy
    /// (any type that would be dropped at scope exit).
    fn local_needs_move_zero(&self, local_idx: usize) -> bool {
        if local_idx >= self.gir_func.locals.len() { return false; }
        let type_id = self.gir_func.locals[local_idx].type_id;
        if let Some(GirType::Named(name)) = self.gir_types.get(type_id) {
            self.gir_types.get_type_def(name).map_or(false, |td| {
                matches!(td.metadata.drop_strategy,
                    crate::ir::types::DropStrategy::Custom(_) |
                    crate::ir::types::DropStrategy::Recursive |
                    crate::ir::types::DropStrategy::Trivial(_))
            })
        } else { false }
    }

    /// Mark a GIR local's LIR slot as moved (ownership transferred).
    /// Emits `MoveSlot` — a pure dataflow annotation with no runtime cost.
    /// The drop elaboration pass uses this to determine slot liveness;
    /// the scope-exit drop (now always `DropIfAlive` for moved locals)
    /// is resolved statically by the elaboration pass.
    fn emit_move_zero_for_local(&mut self, local_idx: usize, bb: BlockId) {
        let slot = self.local_to_slot[local_idx];
        self.push_inst(bb, Inst::MoveSlot { slot });
    }

    /// Emit post-call zeroing for all Move operands in a call's argument list.
    /// This is the generic, data-driven replacement for the hardcoded
    /// function-name matching that previously covered gorget_array_push,
    /// gorget_map_put, etc.  Follows Rust MIR convention: Operand::Move
    /// signals ownership transfer; the caller zeros its source slot.
    /// Emit NamedFuncAddr + byte-offset Store instructions to set function
    /// pointers (elem_drop, elem_clone, etc.) on a freshly constructed collection.
    pub(super) fn emit_collection_fn_ptr_stores(
        &mut self,
        collection_val: ValueId,
        stores: &[(usize, String)],
        bb: BlockId,
    ) {
        for (offset, fn_name) in stores {
            let fn_ptr = self.lir_func.next_value();
            self.push_inst(bb, Inst::NamedFuncAddr {
                dst: fn_ptr,
                name: fn_name.clone(),
            });
            // Use ElemPtr with elem_size=1 to compute byte offset.
            let idx_val = self.emit_i64_const(bb, *offset as i64);
            let field_ptr = self.lir_func.next_value();
            self.push_inst(bb, Inst::ElemPtr {
                dst: field_ptr,
                base: collection_val,
                index: idx_val,
                elem_size: 1,
            });
            self.push_inst(bb, Inst::Store {
                ptr: field_ptr,
                value: fn_ptr,
            });
        }
    }

    /// Parse the effective original GIR name for a collection constructor call.
    ///
    /// Returns `(kind, elem_type, val_type?, with_capacity, str_keyed)` when
    /// the runtime `emit_name` and GIR `effective_orig` name together identify
    /// a collection constructor; `None` for non-ctor calls.
    ///
    /// Called ONCE in `emit_generic_call` so the name is parsed at the right
    /// layer — never re-parsed downstream.
    fn parse_collection_ctor_info(
        emit_name: &str,
        effective_orig: &str,
    ) -> Option<(crate::lir::CollectionCtorKind, String, Option<String>, bool, bool)> {
        use crate::lir::CollectionCtorKind;
        let is_array = emit_name == "gorget_array_new" || emit_name == "gorget_array_with_capacity";
        let is_map = emit_name == "gorget_dict_new" || emit_name == "gorget_dict_new_str"
            || emit_name == "gorget_map_new" || emit_name == "gorget_map_new_str";
        let is_set = emit_name == "gorget_set_new" || emit_name == "gorget_set_new_str"
            || emit_name == "gorget_ordered_set_new" || emit_name == "gorget_ordered_set_new_str"
            || emit_name == "gorget_set_with_capacity";
        if !is_array && !is_map && !is_set { return None; }

        let with_capacity = emit_name.ends_with("_with_capacity");
        let str_keyed = emit_name.ends_with("_new_str");

        fn strip_ctor_suffix(s: &str) -> &str {
            s.strip_suffix("__new_str")
                .or_else(|| s.strip_suffix("__new"))
                .or_else(|| s.strip_suffix("__with_capacity"))
                .unwrap_or(s)
        }

        if let Some(rest) = effective_orig.strip_prefix("Vector__") {
            return Some((CollectionCtorKind::Vector, strip_ctor_suffix(rest).to_string(), None, with_capacity, str_keyed));
        }
        if let Some(rest) = effective_orig.strip_prefix("Deque__") {
            return Some((CollectionCtorKind::Deque, strip_ctor_suffix(rest).to_string(), None, with_capacity, str_keyed));
        }
        if let Some(rest) = effective_orig.strip_prefix("Dict__") {
            let stripped = strip_ctor_suffix(rest);
            if let Some(pos) = stripped.find("__") {
                return Some((CollectionCtorKind::Dict, stripped[..pos].to_string(), Some(stripped[pos+2..].to_string()), with_capacity, str_keyed));
            }
        }
        if let Some(rest) = effective_orig.strip_prefix("HashMap__") {
            let stripped = strip_ctor_suffix(rest);
            if let Some(pos) = stripped.find("__") {
                return Some((CollectionCtorKind::HashMap, stripped[..pos].to_string(), Some(stripped[pos+2..].to_string()), with_capacity, str_keyed));
            }
        }
        if let Some(rest) = effective_orig.strip_prefix("Set__") {
            return Some((CollectionCtorKind::Set, strip_ctor_suffix(rest).to_string(), None, with_capacity, str_keyed));
        }
        if let Some(rest) = effective_orig.strip_prefix("HashSet__") {
            return Some((CollectionCtorKind::HashSet, strip_ctor_suffix(rest).to_string(), None, with_capacity, str_keyed));
        }

        None
    }

    /// Convert a LIR-level C type name to `ElemMeta`. Used when emitting
    /// `CollectionCtor` directly — reads typed LIR metadata rather than
    /// re-matching on name strings downstream.
    pub(super) fn elem_type_to_meta(&self, name: &str) -> crate::lir::ElemMeta {
        use crate::ir::types::CollectionKind;
        use crate::lir::{ElemMeta, ResourceKind};
        match name {
            "int64_t" | "uint64_t" => ElemMeta::Primitive(LirType::I64),
            "int32_t" | "uint32_t" => ElemMeta::Primitive(LirType::I32),
            "int16_t" | "uint16_t" => ElemMeta::Primitive(LirType::I16),
            "int8_t" => ElemMeta::Primitive(LirType::I8),
            "uint8_t" => ElemMeta::Primitive(LirType::U8),
            "double" => ElemMeta::Primitive(LirType::F64),
            "float" => ElemMeta::Primitive(LirType::F32),
            "bool" | "_Bool" => ElemMeta::Primitive(LirType::Bool),
            "GorgetString" | "Str" => ElemMeta::Resource(ResourceKind::GorgetString),
            "GorgetArray" => ElemMeta::Resource(ResourceKind::GorgetArray),
            "GorgetMap" => ElemMeta::Resource(ResourceKind::GorgetMap),
            "GorgetSet" => ElemMeta::Resource(ResourceKind::GorgetSet),
            "GorgetClosure" => ElemMeta::Resource(ResourceKind::GorgetClosure),
            n => {
                // Phase A: read typed `metadata.collection_kind` set at every
                // collection TypeDef registration path (register_collection_alias,
                // map_ast_type_mut, mod.rs pre-monomorphize pass, ensure_collection_type).
                // This replaces a name-prefix probe with a metadata-driven decision.
                // The legacy name-prefix arms remain as a defensive fallback for
                // names that surface from mangling but whose TypeDef wasn't
                // pre-registered (e.g. cross-module imports / monomorph synthetics
                // racing with this lookup) — they are no-op when the typed read
                // hits, but preserve correctness if registration order regresses.
                if let Some(kind) = self.gir_types.collection_kind_by_name(n) {
                    return match kind {
                        CollectionKind::Array => ElemMeta::Resource(ResourceKind::GorgetArray),
                        CollectionKind::Map | CollectionKind::OrderedMap =>
                            ElemMeta::Resource(ResourceKind::GorgetMap),
                        CollectionKind::Set | CollectionKind::OrderedSet =>
                            ElemMeta::Resource(ResourceKind::GorgetSet),
                    };
                }
                // Phase A SSoT fallback: consult `compiler/data/resources.gg`
                // before the legacy prefix arms (per layering-discipline rule 2).
                // The schema's typed `collection_kind` drives the mapping —
                // matching on `runtime_name` alone would over-rotate Heap__
                // (which has runtime_name="GorgetArray" because it wraps a
                // GorgetArray internally, but is itself a user-visible struct
                // that the LIR routes as UserType, not as Resource).
                if let Some(meta) = crate::resources::table().lookup(n) {
                    use crate::resource_schema::{CollectionKind as SchemaCollectionKind, CopySemantics};
                    match meta.collection_kind {
                        // Vector__/Deque__ → GorgetArray. Heap__ deliberately
                        // excluded — see comment above.
                        SchemaCollectionKind::Vector | SchemaCollectionKind::Deque =>
                            return ElemMeta::Resource(ResourceKind::GorgetArray),
                        SchemaCollectionKind::Dict =>
                            return ElemMeta::Resource(ResourceKind::GorgetMap),
                        SchemaCollectionKind::OrderedSet | SchemaCollectionKind::HashSet =>
                            return ElemMeta::Resource(ResourceKind::GorgetSet),
                        SchemaCollectionKind::NotCollection
                        | SchemaCollectionKind::Heap => {
                            // Non-collection resources: GorgetString singleton,
                            // and ref-counted handles (Box/Mutex/Channel/Shared/
                            // Weak/RWLock/Guard). Heap also falls here so it
                            // continues through to UserType lowering downstream.
                            if meta.runtime_name == "GorgetString" {
                                return ElemMeta::Resource(ResourceKind::GorgetString);
                            }
                            if matches!(meta.copy_semantics, CopySemantics::RefCounted) {
                                return ElemMeta::Resource(ResourceKind::RefCounted);
                            }
                            // Fall through (Box / Heap / TraitBox / etc.) —
                            // legacy code paths handle these as UserType, not
                            // Resource. Don't disturb that until item 8 wires
                            // the box_kind metadata into the consumer.
                        }
                    }
                }
                // The Phase A SSoT table above (resources.gg) is fully
                // populated for the Vector__/Deque__/Dict__/HashMap__/Set__/
                // HashSet__ collection prefixes, so the former legacy
                // prefix-fallback arms were dead and have been removed.
                // An unmatched name falls through to UserType / struct lookup.
                if let Some(sid) = self.struct_reg.lookup(n) {
                    // Callable variants (Callable__T_args, MutCallable__T_args, …) are
                    // registered with `c_runtime_alias = "GorgetClosure"`. Read the typed
                    // flag rather than matching on the name prefix.
                    if let Some(sd) = self.module_structs.get(sid.0 as usize) {
                        if sd.c_runtime_alias.as_deref() == Some("GorgetClosure") {
                            return ElemMeta::Resource(ResourceKind::GorgetClosure);
                        }
                    }
                    ElemMeta::UserType(sid)
                } else {
                    ElemMeta::Primitive(LirType::Ptr) // fallback: no metadata available
                }
            }
        }
    }

    /// Compute `(byte_offset, fn_name)` fn-ptr store pairs for a collection ctor
    /// from the already-parsed element type names. Called alongside `CollectionCtor`
    /// emission to wire elem_drop/elem_clone/elem_materialize at runtime.
    ///
    /// Replaces the name-parsing path in `infer_collection_elem_fns` — the
    /// elem type string is now passed in directly rather than re-extracted from
    /// the mangled GIR name.
    pub(super) fn infer_fn_ptr_stores_from_types(
        &self,
        kind: crate::lir::CollectionCtorKind,
        elem_type: &str,
        val_type: Option<&str>,
        str_keyed: bool,
    ) -> Vec<(usize, String)> {
        use crate::lir::CollectionCtorKind;
        let mut stores: Vec<(usize, String)> = Vec::new();

        match kind {
            CollectionCtorKind::Vector | CollectionCtorKind::Deque => {
                // GorgetArray offsets: elem_drop=40, elem_clone=48, elem_materialize=56
                if let Some(drop_fn) = super::types::elem_drop_fn_for_type(elem_type, self.gir_types) {
                    stores.push((40, drop_fn));
                } else if let Some(info) = self.type_drop_fns.get(elem_type) {
                    // Custom-drop element types register `__gorget_dtor_{name}`
                    // (user body + field recursion); Recursive types register
                    // `{name}__drop`. type_drop_fns is the one source of truth —
                    // covers Custom-with-trivial-fields (empty field_drops), which
                    // recursive_drop_structs skips (see populate_recursive_drop_structs).
                    stores.push((40, info.drop_fn_name.clone()));
                }
                if let Some(clone_fn) = super::types::elem_clone_fn_for_type(elem_type, self.gir_types) {
                    stores.push((48, clone_fn));
                } else if self.type_drop_fns.contains_key(elem_type) {
                    stores.push((48, format!("{elem_type}__clone_inplace")));
                }
                if elem_type == "GorgetString" {
                    stores.push((56, "gorget_string_materialize_inplace".into()));
                }
            }
            CollectionCtorKind::Dict | CollectionCtorKind::HashMap => {
                // GorgetMap offsets: val_drop=104, val_clone=112, key_drop=120, key_clone=128, val_materialize=136
                let val_type = val_type.unwrap_or("");
                if let Some(drop_fn) = super::types::elem_drop_fn_for_type(val_type, self.gir_types) {
                    stores.push((104, drop_fn));
                } else if let Some(info) = self.type_drop_fns.get(val_type) {
                    stores.push((104, info.drop_fn_name.clone()));
                }
                if let Some(clone_fn) = super::types::elem_clone_fn_for_type(val_type, self.gir_types) {
                    stores.push((112, clone_fn));
                } else if self.type_drop_fns.contains_key(val_type) {
                    stores.push((112, format!("{val_type}__clone_inplace")));
                }
                if !str_keyed {
                    if let Some(info) = self.type_drop_fns.get(elem_type) {
                        stores.push((120, info.drop_fn_name.clone()));
                        stores.push((128, format!("{elem_type}__clone_inplace")));
                    }
                }
                if val_type == "GorgetString" {
                    stores.push((136, "gorget_string_materialize_inplace".into()));
                }
            }
            CollectionCtorKind::Set | CollectionCtorKind::HashSet => {
                // GorgetSet: key_drop=120, key_clone=128 (same offsets as GorgetMap)
                if !str_keyed {
                    if let Some(info) = self.type_drop_fns.get(elem_type) {
                        stores.push((120, info.drop_fn_name.clone()));
                        stores.push((128, format!("{elem_type}__clone_inplace")));
                    }
                }
            }
        }

        stores
    }

    pub(super) fn emit_post_call_zeros(&mut self, args: &[Operand], bb: BlockId) {
        for arg in args {
            if let Operand::Move(place) = arg {
                if place.projections.is_empty() {
                    let local_idx = place.local.0 as usize;
                    if self.local_needs_move_zero(local_idx) {
                        self.emit_move_zero_for_local(local_idx, bb);
                    }
                }
            }
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
                // Direct GorgetString struct in slot
                if matches!(slot_ty, LirType::Struct(sid) if Some(*sid) == str_sid) {
                    return true;
                }
                // Ptr slot whose GIR type maps to GorgetString — the slot holds a
                // pointer to a GorgetString (e.g. from unwrap, collection get).
                if *slot_ty == LirType::Ptr && idx < self.gir_func.locals.len() {
                    let lir_ty = self.map_type(&self.gir_func.locals[idx].type_id);
                    if matches!(lir_ty, LirType::Struct(sid) if Some(sid) == str_sid) {
                        return true;
                    }
                }
                false
            }
            _ => false,
        }
    }

    /// If `original_name` is `Box__<Trait>__<method>`, emit
    /// `Inst::TraitCall` in place of the generic extern call. BIR
    /// synthesis then rewrites each TraitCall into a `Call` to one
    /// dedup'd `__gg_synth_trait_*` helper whose body is the vtable
    /// dispatch chain. Returns `None` if the call doesn't match the
    /// pattern or the trait metadata isn't fully registered.
    fn try_emit_trait_call(
        &mut self,
        original_name: &str,
        dst: &Option<ir::types::LocalId>,
        args: &[Operand],
        lir_args: &[ValueId],
        bb: BlockId,
    ) -> Option<BlockId> {
        // Parse `Box__<Trait>__<method>`. Method is everything after the
        // last `__`; trait name is between `Box__` and that separator.
        let rest = original_name.strip_prefix("Box__")?;
        let sep = rest.rfind("__")?;
        let trait_name = &rest[..sep];
        let method = &rest[sep + 2..];

        // GIR side: the VTable must exist and must carry the method as a
        // FnPtr-typed field. We reach into GIR for the method signature
        // because the extern declaration for `Box__Trait__method` was
        // registered with `void*` self and generic aggregate-as-Ptr
        // params — the VTable FnPtr carries the concrete types
        // (resolve_param_type in register_trait_sigs).
        let vtable_name = format!("{trait_name}_VTable");
        let trait_obj_name = format!("{trait_name}_TraitObj");
        // Both type defs must be registered in GIR (belt-and-braces).
        self.gir_types.get_type_def(&trait_obj_name)?;
        let vtable_def = self.gir_types.get_type_def(&vtable_name)?;
        let vtable_struct = match &vtable_def.kind {
            ir::types::TypeDefKind::Struct(s) => s,
            _ => return None,
        };
        let method_field = vtable_struct.fields.iter().find(|f| f.name == method)?;
        let (fn_params, fn_ret): (Vec<ir::types::TypeId>, ir::types::TypeId) = match self
            .gir_types
            .get(method_field.type_id)?
        {
            ir::types::GirType::FnPtr { params, return_type, .. } => {
                (params.clone(), *return_type)
            }
            _ => return None,
        };
        // `params[0]` is the synthetic void* data (self); user params
        // start at index 1. A zero-length FnPtr shouldn't be possible
        // for a VTable entry, but guard against it to avoid panics.
        if fn_params.is_empty() {
            return None;
        }

        // LIR side: both structs must also be in the struct registry so
        // BIR synthesis can resolve StructIds inside the helper body.
        self.struct_reg.lookup(&trait_obj_name)?;
        self.struct_reg.lookup(&vtable_name)?;

        // lir_args[0] is the `Box__Trait` (TraitObj*) self; the rest are
        // user args in order. An empty arg list is a broken call-site.
        if lir_args.is_empty() {
            return None;
        }
        let self_val = lir_args[0];

        // Per-arg ABI coercion: for each user arg, if the VTable's GIR
        // FnPtr param is `Ptr(T)`/`MutPtr(T)` but the caller's value is
        // an aggregate (not already a pointer), emit `Inst::AddressOf`
        // to spill the aggregate to a stack slot and produce a `Ptr`
        // suitable for the impl's `void*` param. This mirrors
        // `emit_call_extern`'s arg-abi marshalling — the old static-
        // inline wrapper path relied on that marshalling to auto-
        // address aggregate borrow args, and the synth helper needs
        // the same conversion in LIR primitives since it bypasses
        // emit_call_extern.
        let user_param_tys: Vec<LirType> = fn_params[1..]
            .iter()
            .map(|tid| self.map_type(tid))
            .collect();
        let ret_ty = self.map_type(&fn_ret);

        let mut user_args: Vec<ValueId> = Vec::with_capacity(lir_args.len() - 1);
        for (i, &arg_val) in lir_args[1..].iter().enumerate() {
            let vtable_pty = &fn_params[i + 1];
            let callee_expects_ptr = matches!(
                self.gir_types.get(*vtable_pty),
                Some(ir::types::GirType::Ptr(_)) | Some(ir::types::GirType::MutPtr(_))
            );
            // Whether the caller's value is already a pointer — check
            // the caller-side GIR operand type. If it's a Copy/Move of
            // a local whose GIR type is Ptr/MutPtr, the value itself is
            // a pointer; skip the address-of.
            let caller_already_ptr = match args.get(i + 1) {
                Some(Operand::Copy(p)) | Some(Operand::Move(p)) => {
                    self.gir_func.locals.get(p.local.0 as usize).map_or(false, |l| {
                        matches!(
                            self.gir_types.get(l.type_id),
                            Some(ir::types::GirType::Ptr(_))
                                | Some(ir::types::GirType::MutPtr(_))
                        )
                    })
                }
                _ => false,
            };

            if callee_expects_ptr && !caller_already_ptr {
                let inner_gid = match self.gir_types.get(*vtable_pty) {
                    Some(ir::types::GirType::Ptr(inner))
                    | Some(ir::types::GirType::MutPtr(inner)) => *inner,
                    _ => unreachable!("checked by callee_expects_ptr"),
                };
                let inner_lty = self.map_type(&inner_gid);
                let addr = self.lir_func.next_value();
                self.push_inst(bb, Inst::AddressOf {
                    dst: addr,
                    value: arg_val,
                    ty: inner_lty,
                });
                user_args.push(addr);
            } else {
                user_args.push(arg_val);
            }
        }

        // Pull arg_abis from the extern decl as authored by the GIR
        // lowering — BIR synthesis ignores this field today (the helper
        // has a typed signature), but we preserve it for future
        // inspection and for the invariant that TraitCall and the
        // CallExtern it replaces carry equivalent metadata.
        let arg_abis = self.lookup_arg_abis(original_name);

        // Allocate a ValueId for the result if the call produces one.
        let dst_val = dst.map(|_| self.lir_func.next_value());

        // Resolve trait-name + method-name to typed IDs at construction
        // time. The `{Trait}_TraitObj` and `{Trait}_VTable` structs are
        // emitted by GIR lowering's trait-object pass before LIR
        // lowering walks the function bodies, so they're guaranteed to
        // exist in `self.module_structs` here. A missing struct or
        // method would be a GIR-pass bug; panic with the (trait, method)
        // pair so the responsible pass surfaces.
        let trait_obj_name = format!("{trait_name}_TraitObj");
        let trait_obj_struct = match self.module_structs.iter().position(|s| s.name == trait_obj_name) {
            Some(idx) => crate::lir::StructId(idx as u32),
            None => panic!(
                "TraitCall lowering: missing `{trait_obj_name}` struct (trait `{trait_name}`, \
                 method `{method}`). GIR lowering's trait-object pass must register the \
                 `_TraitObj` struct before LIR lowering walks call sites."
            ),
        };
        let vtable_name = format!("{trait_name}_VTable");
        let vtable_sid = self.module_structs.iter().position(|s| s.name == vtable_name)
            .unwrap_or_else(|| panic!(
                "TraitCall lowering: missing `{vtable_name}` struct (trait `{trait_name}`, \
                 method `{method}`)."
            ));
        let method_idx = self.module_structs[vtable_sid].fields.iter()
            .position(|(n, _)| n == method)
            .unwrap_or_else(|| panic!(
                "TraitCall lowering: method `{method}` not found in `{vtable_name}` (trait `{trait_name}`)."
            )) as u32;

        self.push_inst(bb, Inst::TraitCall {
            dst: dst_val,
            object: self_val,
            trait_obj_struct,
            method_idx,
            args: user_args,
            arg_abis,
            param_tys: user_param_tys,
            ret_ty,
        });

        if let (Some(d), Some(v)) = (*dst, dst_val) {
            self.store_to_local(d, v, bb);
        }

        Some(bb)
    }

    /// If `original_name` is a `Vector__<elem>__<method>` HOF we've
    /// migrated (each, any, all), emit `Inst::HofExpand` in place of the
    /// generic extern call and return the (possibly updated) block id.
    /// Returns `None` when the call doesn't match, leaving the caller to
    /// dispatch normally.
    ///
    /// If `original_name` is a `Dict__<K>__<V>__<method>` HOF we've
    /// migrated, emit `Inst::HofExpand` with the `DictEach` / … tag.
    /// Otherwise returns `None`. For Dict HOFs:
    ///   * `element_ty` on `HofExpand` carries the KEY type (K).
    ///   * `value_ty` carries the VALUE type (V).
    ///
    /// Supported variants (pathfinder): `each`.
    fn try_emit_dict_hof(
        &mut self,
        original_name: &str,
        dst: &Option<ir::types::LocalId>,
        args: &[Operand],
        lir_args: &[ValueId],
        bb: BlockId,
    ) -> Option<BlockId> {
        // Dict__K__V__method or HashMap__K__V__method
        let rest = original_name
            .strip_prefix("Dict__")
            .or_else(|| original_name.strip_prefix("HashMap__"))?;
        let sep_pos = rest.rfind("__")?;
        let method = &rest[sep_pos + 2..];
        let (hof_op, produces_result, is_fold, is_filter) = match method {
            "each" => (HofOp::DictEach, false, false, false),
            "fold" => (HofOp::DictFold, true, true, false),
            "any" => (HofOp::DictAny, true, false, false),
            "all" => (HofOp::DictAll, true, false, false),
            "filter" => (HofOp::DictFilter, true, false, true),
            _ => return None,
        };
        if lir_args.len() < 2 {
            return None;
        }
        // Fold: (dict, init, closure) — three args.
        if is_fold && lir_args.len() < 3 {
            return None;
        }
        if produces_result && dst.is_none() {
            return None;
        }
        // Key/value type: split the `<K>__<V>` prefix.
        let type_part = &rest[..sep_pos];
        let key_sep = type_part.find("__")?;
        let key_c = &type_part[..key_sep];
        let val_c = &type_part[key_sep + 2..];
        let key_ty = super::component_to_lir_type(key_c, self.struct_reg, self.gir_types);
        let val_ty = super::component_to_lir_type(val_c, self.struct_reg, self.gir_types);

        // Closure signature lookup (same shape as Vector HOFs).
        let closure_idx = lir_args.len() - 1;
        let closure_call_sig = args.get(closure_idx).and_then(|op| {
            let key = match op {
                Operand::Constant(Constant::FuncRef(n)) => n.clone(),
                Operand::Copy(_) | Operand::Move(_) => {
                    let ty_name = self.operand_gir_type_name(op)?;
                    format!("{ty_name}__call")
                }
                _ => return None,
            };
            self.closure_call_sigs.get(&key).cloned()
        });
        let param_tys: Vec<LirType> = closure_call_sig
            .as_ref()
            .map(|sig| sig.param_tys.clone())
            .unwrap_or_default();
        let abi_from_param_ty = |ty: &LirType| -> crate::ir::abi::AbiKind {
            use crate::ir::abi::AbiKind;
            match ty {
                LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef => AbiKind::Ptr,
                LirType::Struct(_) => AbiKind::ByValue,
                _ => AbiKind::Scalar,
            }
        };
        // Only handle the case where the closure signature is known.
        // Without a signature we'd guess the per-arg ABI, which is
        // risky for aggregate types.
        if closure_call_sig.is_none() {
            return None;
        }

        // For fold the closure signature is (acc, K, V); for each it's
        // (K, V). Peel the accumulator from the ABI list when present.
        let (acc_abi, key_abi, val_abi) = if is_fold {
            let acc = param_tys
                .first()
                .map(|ty| abi_from_param_ty(ty))
                .unwrap_or(crate::ir::abi::AbiKind::Scalar);
            let k = param_tys
                .get(1)
                .map(|ty| abi_from_param_ty(ty))
                .unwrap_or_else(|| {
                    if key_ty.is_aggregate() {
                        crate::ir::abi::AbiKind::Ptr
                    } else {
                        crate::ir::abi::AbiKind::Scalar
                    }
                });
            let v = param_tys
                .get(2)
                .map(|ty| abi_from_param_ty(ty))
                .unwrap_or_else(|| {
                    if val_ty.is_aggregate() {
                        crate::ir::abi::AbiKind::Ptr
                    } else {
                        crate::ir::abi::AbiKind::Scalar
                    }
                });
            (Some(acc), k, v)
        } else {
            let k = param_tys
                .first()
                .map(|ty| abi_from_param_ty(ty))
                .unwrap_or_else(|| {
                    if key_ty.is_aggregate() {
                        crate::ir::abi::AbiKind::Ptr
                    } else {
                        crate::ir::abi::AbiKind::Scalar
                    }
                });
            let v = param_tys
                .get(1)
                .map(|ty| abi_from_param_ty(ty))
                .unwrap_or_else(|| {
                    if val_ty.is_aggregate() {
                        crate::ir::abi::AbiKind::Ptr
                    } else {
                        crate::ir::abi::AbiKind::Scalar
                    }
                });
            (None, k, v)
        };

        // Wrap the closure arg into a GorgetClosure pointer.
        let mut lir_args_wrapped = lir_args.to_vec();
        self.wrap_closure_call_args(args, &mut lir_args_wrapped, bb);

        // filter produces a fresh GorgetMap — pre-register the runtime
        // helpers the BIR expansion will call (mirror src's config
        // via gorget_map_new_like, insert via gorget_map_put_cloned).
        if is_filter {
            let map_ty = self.struct_reg
                .lookup("GorgetMap")
                .map(LirType::Struct)
                .unwrap_or(LirType::Ptr);
            self.ensure_extern("gorget_map_new_like", &[LirType::Ptr], &map_ty);
            self.ensure_extern("gorget_map_put_cloned",
                &[LirType::Ptr, LirType::Ptr, LirType::Ptr], &LirType::Void);
        }

        // closure_ret_ty: for fold, the accumulator type (= dst's
        // declared type); for each, void; for filter, Bool (predicate).
        // Aggregate accumulators go through AddressOf on the loop
        // back-edge so they work here the same way as the Vector fold
        // path.
        let closure_ret_ty = if is_fold {
            let d = dst.as_ref().expect("fold requires dst");
            let gir_ty = self.gir_func.locals[d.0 as usize].type_id;
            self.map_type(&gir_ty)
        } else if is_filter {
            LirType::Bool
        } else if produces_result {
            // DictAny / DictAll — predicate closures.
            LirType::Bool
        } else {
            LirType::Void
        };

        let closure_arg_abis = match acc_abi {
            Some(a) => vec![a, key_abi, val_abi],
            None => vec![key_abi, val_abi],
        };

        // When a result is produced, allocate a fresh ValueId and
        // plumb it to the caller's local slot.
        let result_id = if produces_result {
            Some(self.lir_func.next_value())
        } else {
            None
        };
        let init_id = if is_fold { Some(lir_args_wrapped[1]) } else { None };

        self.push_inst(bb, Inst::HofExpand {
            coll: lir_args_wrapped[0],
            hof_op,
            element_ty: key_ty,
            value_ty: Some(val_ty),
            closure: lir_args_wrapped[closure_idx],
            closure_kind: ClosureDispatchKind::EscapedClosure,
            closure_ret_ty,
            closure_arg_abis,
            dst: result_id,
            init: init_id,
        });

        if let (Some(d), Some(r)) = (*dst, result_id) {
            self.store_to_local(d, r, bb);
        }
        Some(bb)
    }

    /// If `original_name` is a `Set__<T>__<method>` or
    /// `HashSet__<T>__<method>` HOF we've migrated (`each`, `fold`,
    /// `any`, `all`), emit `Inst::HofExpand`. `value_ty` encodes
    /// `is_ordered` for the BIR expansion: `Some(Void)` = ordered
    /// (Set__, walks order[]), `Some(Ptr)` = unordered (HashSet__,
    /// walks states).
    fn try_emit_set_hof(
        &mut self,
        original_name: &str,
        dst: &Option<ir::types::LocalId>,
        args: &[Operand],
        lir_args: &[ValueId],
        bb: BlockId,
    ) -> Option<BlockId> {
        let (rest, is_ordered) = if let Some(r) = original_name.strip_prefix("Set__") {
            (r, true)
        } else if let Some(r) = original_name.strip_prefix("HashSet__") {
            (r, false)
        } else {
            return None;
        };
        let sep_pos = rest.rfind("__")?;
        let method = &rest[sep_pos + 2..];
        let (hof_op, produces_result, is_fold, is_filter) = match method {
            "each" => (HofOp::SetEach, false, false, false),
            "fold" => (HofOp::SetFold, true, true, false),
            "any" => (HofOp::SetAny, true, false, false),
            "all" => (HofOp::SetAll, true, false, false),
            "filter" => (HofOp::SetFilter, true, false, true),
            _ => return None,
        };
        if lir_args.len() < 2 {
            return None;
        }
        if is_fold && lir_args.len() < 3 {
            return None;
        }
        if produces_result && dst.is_none() {
            return None;
        }
        let elem_c = &rest[..sep_pos];
        let elem_ty = super::component_to_lir_type(elem_c, self.struct_reg, self.gir_types);

        let closure_idx = lir_args.len() - 1;
        let closure_call_sig = args.get(closure_idx).and_then(|op| {
            let key = match op {
                Operand::Constant(Constant::FuncRef(n)) => n.clone(),
                Operand::Copy(_) | Operand::Move(_) => {
                    let ty_name = self.operand_gir_type_name(op)?;
                    format!("{ty_name}__call")
                }
                _ => return None,
            };
            self.closure_call_sigs.get(&key).cloned()
        });
        if closure_call_sig.is_none() {
            return None;
        }
        let param_tys: Vec<LirType> = closure_call_sig
            .as_ref()
            .map(|sig| sig.param_tys.clone())
            .unwrap_or_default();
        let abi_from_param_ty = |ty: &LirType| -> crate::ir::abi::AbiKind {
            use crate::ir::abi::AbiKind;
            match ty {
                LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef => AbiKind::Ptr,
                LirType::Struct(_) => AbiKind::ByValue,
                _ => AbiKind::Scalar,
            }
        };
        let (acc_abi, elem_abi) = if is_fold {
            let acc = param_tys
                .first()
                .map(|ty| abi_from_param_ty(ty))
                .unwrap_or(crate::ir::abi::AbiKind::Scalar);
            let e = param_tys
                .get(1)
                .map(|ty| abi_from_param_ty(ty))
                .unwrap_or_else(|| {
                    if elem_ty.is_aggregate() {
                        crate::ir::abi::AbiKind::Ptr
                    } else {
                        crate::ir::abi::AbiKind::Scalar
                    }
                });
            (Some(acc), e)
        } else {
            let e = param_tys
                .first()
                .map(|ty| abi_from_param_ty(ty))
                .unwrap_or_else(|| {
                    if elem_ty.is_aggregate() {
                        crate::ir::abi::AbiKind::Ptr
                    } else {
                        crate::ir::abi::AbiKind::Scalar
                    }
                });
            (None, e)
        };

        let mut lir_args_wrapped = lir_args.to_vec();
        self.wrap_closure_call_args(args, &mut lir_args_wrapped, bb);

        // filter / map produce a fresh GorgetSet — pre-register the
        // runtime helper the BIR expansion will call to mint the result
        // with src's hash/eq/drop/clone config mirrored.
        if is_filter {
            let set_ty = self.struct_reg
                .lookup("GorgetSet")
                .map(LirType::Struct)
                .unwrap_or(LirType::Ptr);
            self.ensure_extern("gorget_set_new_like", &[LirType::Ptr], &set_ty);
            self.ensure_extern("gorget_map_put_cloned",
                &[LirType::Ptr, LirType::Ptr, LirType::Ptr], &LirType::Void);
        }

        let closure_ret_ty = if is_fold {
            let d = dst.as_ref().expect("fold requires dst");
            let gir_ty = self.gir_func.locals[d.0 as usize].type_id;
            self.map_type(&gir_ty)
        } else if produces_result {
            LirType::Bool
        } else {
            LirType::Void
        };

        let closure_arg_abis = match acc_abi {
            Some(a) => vec![a, elem_abi],
            None => vec![elem_abi],
        };

        let result_id = if produces_result {
            // filter routes its dst through SlotLoad in the BIR
            // expansion, so the HofExpand dst is an aggregate
            // ValueId; any/all/fold still produce scalar/aggregate
            // results the same way.
            Some(self.lir_func.next_value())
        } else {
            None
        };
        let init_id = if is_fold { Some(lir_args_wrapped[1]) } else { None };

        // Encode is_ordered via value_ty: Void → ordered, Ptr → unordered.
        let value_ty = if is_ordered {
            Some(LirType::Void)
        } else {
            Some(LirType::Ptr)
        };

        self.push_inst(bb, Inst::HofExpand {
            coll: lir_args_wrapped[0],
            hof_op,
            element_ty: elem_ty,
            value_ty,
            closure: lir_args_wrapped[closure_idx],
            closure_kind: ClosureDispatchKind::EscapedClosure,
            closure_ret_ty,
            closure_arg_abis,
            dst: result_id,
            init: init_id,
        });

        if let (Some(d), Some(r)) = (*dst, result_id) {
            self.store_to_local(d, r, bb);
        }
        Some(bb)
    }

    /// Intercept `Dict__K__V__get_or(map, key, default)` and the
    /// `__get_or_put` variant — both collapse into a
    /// `gorget_map_get` + null-check + conditional clone/insert at LIR
    /// emit time (no new canonical op, no per-type inline helper in
    /// the C backend, LLVM backend stays dumb).
    ///
    /// On hit, the value is loaded from the map's slot; for `String`
    /// vals the load is replaced with `gorget_string_clone_to_owned`
    /// so the result is independently owned. `__get_or_put` also
    /// inserts the default into the map on miss via `gorget_map_put`.
    ///
    /// Keys become a `*key` for `gorget_map_get`: already-pointer keys
    /// (borrowed String/aggregate params) pass through; scalars and
    /// by-value aggregates spill via `Inst::AddressOf`.
    fn try_emit_dict_get_or(
        &mut self,
        original_name: &str,
        dst: &Option<ir::types::LocalId>,
        args: &[Operand],
        lir_args: &[ValueId],
        bb: BlockId,
    ) -> Option<BlockId> {
        use crate::ir::abi::AbiKind;
        let rest = original_name
            .strip_prefix("Dict__")
            .or_else(|| original_name.strip_prefix("HashMap__"))?;
        let (is_put, base) = if let Some(b) = rest.strip_suffix("__get_or_put") {
            (true, b)
        } else if let Some(b) = rest.strip_suffix("__get_or") {
            (false, b)
        } else {
            return None;
        };
        let d = (*dst)?;
        if lir_args.len() < 3 {
            return None;
        }
        // Split base into K__V at the first `__` — simple types don't
        // nest `__` so this is unambiguous for every fixture shape. The value
        // type is taken from the destination local's GIR type (`val_ty` below),
        // and the per-value clone decision flows through the typed resolver
        // `resource_clone_fn_for_payload` — never a name-test on `base`.
        let _ = base.find("__")?;

        let map_arg = lir_args[0];
        let key_arg = lir_args[1];
        let default_arg = lir_args[2];
        // `operand_lir_type` maps `Constant::Str` to `Ptr`, but the
        // actual LIR value materialized for a string literal arg is
        // the `GorgetString` struct — the slot we're about to spill
        // into needs to match. Other constants (Int, Bool, Float)
        // have scalar types that `operand_lir_type` gets right.
        let key_ty = match &args[1] {
            Operand::Constant(Constant::Str(_)) => self
                .struct_reg
                .lookup("GorgetString")
                .map(LirType::Struct)
                .unwrap_or(LirType::Ptr),
            _ => self.operand_lir_type(&args[1]),
        };
        let val_ty = {
            let gir_ty = self.gir_func.locals[d.0 as usize].type_id;
            self.map_type(&gir_ty)
        };

        // Pre-register runtime externs the expansion will call.
        self.ensure_extern(
            "gorget_map_get",
            &[LirType::Ptr, LirType::Ptr],
            &LirType::Ptr,
        );
        if is_put {
            self.ensure_extern(
                "gorget_map_put",
                &[LirType::Ptr, LirType::Ptr, LirType::Ptr],
                &LirType::Void,
            );
        }
        // Resource-valued get_or/get_or_put must CLONE the default for EACH
        // owned output (result slot, hit-path value, map insert), leaving the
        // caller's borrowed default untouched — otherwise the result aliases
        // the default's heap and both drops double-free. This is the same
        // clone-at-ownership-boundary rule every collection insert obeys
        // (docs/devbook/11-copy-on-write.md §"Materialization points"). The
        // by-value deep-clone symbol comes from the typed single-source-of-
        // truth resolver (NOT a `val_is_str` name-test — layering rule 2):
        // `gorget_array_clone` / `gorget_map_clone` / `gorget_string_clone` /
        // `gorget_closure_clone_to_owned` / `{name}__clone`. It returns None
        // for scalars (int/bool/float) — the free trivial-gate, so those keep
        // the byte-for-byte move-and-return-same behavior, no regression.
        let val_clone_fn = self.resource_clone_fn_for_payload(&val_ty, false);
        if let Some(clone_fn) = &val_clone_fn {
            self.ensure_extern(clone_fn, &[LirType::Ptr], &val_ty);
        }

        // key_addr for gorget_map_get: a *key pointer.
        //
        // When the key operand is already a pointer (borrowed String /
        // aggregate param, or any Ptr/MutPtr local), use it directly —
        // AddressOf would produce **key and MapGet would hash/compare
        // the wrong memory (snag #54 / #55: `get_or` in a callee with a
        // String key param always missed and returned the default).
        // Scalars and by-value aggregates still go through AddressOf so
        // the map receives &key (mirrors Dict index / `.get` lowering,
        // and the trait-call ABI coercion above).
        let key_already_ptr = match &args[1] {
            Operand::Copy(p) | Operand::Move(p) => {
                self.gir_func.locals.get(p.local.0 as usize).map_or(false, |l| {
                    matches!(
                        self.gir_types.get(l.type_id),
                        Some(ir::types::GirType::Ptr(_))
                            | Some(ir::types::GirType::MutPtr(_))
                    )
                })
            }
            // Constant::Str and other constants materialize as values.
            _ => false,
        };
        let key_addr = if key_already_ptr {
            key_arg
        } else {
            let addr = self.lir_func.next_value();
            self.push_inst(bb, Inst::AddressOf {
                dst: addr,
                value: key_arg,
                ty: key_ty,
            });
            addr
        };

        // default_addr = &default, as a BORROW. We need a pointer to the
        // default to feed the by-value clone fns. `Inst::AddressOf` can't be
        // used here: its lowering spills via `SlotStore(is_move=false)`, which
        // for a String value is a `gorget_string_copy_cow` — a deep, OWNED copy
        // that nothing drops → a leak. Instead spill with `is_move=true`, a
        // SHALLOW byte-copy (an alias / borrow): no allocation, and the borrow
        // slot is never drop-tracked, so it can't double-free. The clone fns
        // read through this borrow and produce the independent owned copies.
        // Only materialized for resource values (scalars never clone).
        let default_addr = if val_clone_fn.is_some() {
            let borrow_slot = self.lir_func.add_slot(val_ty.clone(), None);
            self.push_inst(bb, Inst::SlotStore {
                slot: borrow_slot,
                value: default_arg,
                is_move: true,
            });
            let addr = self.lir_func.next_value();
            self.push_inst(bb, Inst::SlotAddr { dst: addr, slot: borrow_slot });
            Some(addr)
        } else {
            None
        };

        // Small closure: store an owned (cloned-when-resource) copy of the
        // default into result_slot at block `b`. For a resource value a bare
        // copy aliases the caller's default heap → double-free at both drops,
        // so deep-clone via the by-value resolver and MOVE the fresh clone in
        // (a `is_move=false` String store would re-CoW and orphan-leak the
        // clone). Scalars (val_clone_fn == None) keep the byte-for-byte copy.
        let result_slot = self.lir_func.add_slot(val_ty.clone(), None);
        let store_default_into_result = |this: &mut Self, b: BlockId| {
            let (result_default, result_is_move) = if let Some(clone_fn) = &val_clone_fn {
                let cloned = this.lir_func.next_value();
                this.push_inst(b, Inst::CallExtern {
                    dst: Some(cloned),
                    name: clone_fn.clone(),
                    args: vec![default_addr.expect("resource value has a borrow address")],
                    arg_abis: vec![AbiKind::Ptr],
                });
                (cloned, true)
            } else {
                (default_arg, false)
            };
            this.push_inst(b, Inst::SlotStore {
                slot: result_slot,
                value: result_default,
                is_move: result_is_move,
            });
        };
        // result_slot is filled in the per-path blocks below (hit_bb with the
        // map value, miss_bb with the default) — NOT pre-filled on the entry
        // block. A pre-fill would clone the default eagerly and then orphan-leak
        // that clone whenever the hit branch overwrites result_slot.

        // ptr = gorget_map_get(map, key_addr).
        let ptr = self.lir_func.next_value();
        self.push_inst(bb, Inst::CallExtern {
            dst: Some(ptr),
            name: "gorget_map_get".to_string(),
            args: vec![map_arg, key_addr],
            arg_abis: vec![AbiKind::Ptr, AbiKind::VoidElem],
        });

        // is_present = ptr != NULL.
        let null_val = self.lir_func.next_value();
        self.push_inst(bb, Inst::NullPtr { dst: null_val });
        let is_present = self.lir_func.next_value();
        self.push_inst(bb, Inst::Cmp {
            dst: is_present,
            op: CmpOp::Ne,
            lhs: ptr,
            rhs: null_val,
        });

        let hit_bb = self.lir_func.add_block();
        let merge_bb = self.lir_func.add_block();
        // miss_bb is ALWAYS its own block (even read-only get_or): it stores the
        // default-clone into result_slot, so the clone never executes on the hit
        // path (which would orphan-leak it). get_or_put additionally inserts the
        // default into the map there.
        let miss_bb = self.lir_func.add_block();

        self.set_terminator(bb, Term::Branch {
            cond: is_present,
            then_block: hit_bb,
            then_args: vec![],
            else_block: miss_bb,
            else_args: vec![],
        });

        // hit_bb: clone-from (resource) or load (scalar) the map's value into
        // result_slot. The returned value must be an INDEPENDENT owned copy —
        // a shallow Load would alias the map's storage, double-freeing against
        // the map's val_drop. The map pointer `ptr` is the address of the
        // element, fed straight to the by-value clone fn.
        let payload_val = self.lir_func.next_value();
        let hit_is_move = if let Some(clone_fn) = &val_clone_fn {
            self.push_inst(hit_bb, Inst::CallExtern {
                dst: Some(payload_val),
                name: clone_fn.clone(),
                args: vec![ptr],
                arg_abis: vec![AbiKind::Ptr],
            });
            // Fresh by-value clone — move it in (see result-slot store above).
            true
        } else {
            self.push_inst(hit_bb, Inst::Load {
                dst: payload_val,
                ptr,
                ty: val_ty.clone(),
            });
            false
        };
        self.push_inst(hit_bb, Inst::SlotStore {
            slot: result_slot,
            value: payload_val,
            is_move: hit_is_move,
        });
        self.set_terminator(hit_bb, Term::Jump(merge_bb, vec![]));

        // miss_bb: fill result_slot with an owned copy of the default, and (for
        // get_or_put) insert the default into the map; then jump to merge. The
        // result_slot fill lives HERE — not pre-filled on the entry block — so
        // the hit branch (which overwrites result_slot with the map value) never
        // orphan-leaks a default clone. For get_or_put on a resource value the
        // map must get its OWN clone too — inserting the caller's default bytes
        // shallowly would alias the caller's heap (map's val_drop double-frees
        // against the caller). Scalars insert the default bytes directly.
        if is_put {
            let insert_addr = if let Some(clone_fn) = &val_clone_fn {
                // Resource value: the map takes ownership of its OWN clone (a
                // shallow `gorget_map_put` memcpy of the caller's default would
                // make the map alias the caller's heap → its val_drop double-
                // frees). Clone through the borrow address, then spill the fresh
                // clone with a MOVE (shallow, no CoW re-copy) and hand its
                // address to map_put — which memcpy-adopts it.
                let map_clone = self.lir_func.next_value();
                self.push_inst(miss_bb, Inst::CallExtern {
                    dst: Some(map_clone),
                    name: clone_fn.clone(),
                    args: vec![default_addr.expect("resource value has a borrow address")],
                    arg_abis: vec![AbiKind::Ptr],
                });
                let map_clone_slot = self.lir_func.add_slot(val_ty.clone(), None);
                self.push_inst(miss_bb, Inst::SlotStore {
                    slot: map_clone_slot,
                    value: map_clone,
                    is_move: true,
                });
                let map_clone_addr = self.lir_func.next_value();
                self.push_inst(miss_bb, Inst::SlotAddr {
                    dst: map_clone_addr,
                    slot: map_clone_slot,
                });
                map_clone_addr
            } else {
                // Scalar value: insert the default's bytes directly (no clone).
                let scalar_addr = self.lir_func.next_value();
                self.push_inst(miss_bb, Inst::AddressOf {
                    dst: scalar_addr,
                    value: default_arg,
                    ty: val_ty.clone(),
                });
                scalar_addr
            };
            self.push_inst(miss_bb, Inst::CallExtern {
                    dst: None,
                    name: "gorget_map_put".to_string(),
                    args: vec![map_arg, key_addr, insert_addr],
                    arg_abis: vec![AbiKind::Ptr, AbiKind::VoidElem, AbiKind::VoidElem],
                });
        }
        // Fill result_slot with an owned copy of the default (independent of
        // both the caller's default and, for get_or_put, the map's clone), then
        // close the miss block. Runs for BOTH get_or and get_or_put.
        store_default_into_result(self, miss_bb);
        self.set_terminator(miss_bb, Term::Jump(merge_bb, vec![]));

        // merge_bb: result = SlotLoad(result_slot); store to dst.
        let result = self.lir_func.next_value();
        self.push_inst(merge_bb, Inst::SlotLoad {
            dst: result,
            slot: result_slot,
            ty: val_ty,
        });
        self.store_to_local(d, result, merge_bb);
        Some(merge_bb)
    }

    /// The closure argument is packed into a `GorgetClosure` pointer via
    /// `wrap_closure_call_args` so the BIR expansion can dispatch it through
    /// `Inst::CallClosure { kind: EscapedClosure }`. Closure arg ABI is set
    /// from the element type (aggregates → Ptr, scalars → Scalar); BIR
    /// expansion uses this to decide whether to Load the element pointer.
    fn try_emit_vector_each_hof(
        &mut self,
        original_name: &str,
        dst: &Option<ir::types::LocalId>,
        args: &[Operand],
        lir_args: &[ValueId],
        bb: BlockId,
    ) -> Option<BlockId> {
        // Deque shares Vector's underlying gorget_array runtime; a
        // `Deque__T__each/map/fold/...` name is the same HOF class and must
        // route through the same closure-wrap path. Pre-fix (Round XXVII
        // Track B) the Deque__-prefix bailed via `?`, leaving the caller
        // to emit an undefined `Deque__T__each` stub (C: implicit-decl
        // error; LLVM: unresolved link).
        let rest = original_name
            .strip_prefix("Vector__")
            .or_else(|| original_name.strip_prefix("Deque__"))?;
        let sep = rest.rfind("__")?;
        let method = &rest[sep + 2..];
        // `closure_ret_ty` for result-producing variants is derived from the
        // caller's dst declared type (populated below). The `each` variant
        // sets it to Void since the closure returns nothing.
        let (hof_op, produces_result, is_fold, is_reduce, is_count, is_find, is_find_index, is_filter, is_map, is_flat_map, is_sort, is_sort_key) =
            match method {
                "each" => (HofOp::Each, false, false, false, false, false, false, false, false, false, false, false),
                "any" => (HofOp::Any, true, false, false, false, false, false, false, false, false, false, false),
                "all" => (HofOp::All, true, false, false, false, false, false, false, false, false, false, false),
                "fold" => (HofOp::Fold, true, true, false, false, false, false, false, false, false, false, false),
                "reduce" => (HofOp::Reduce, true, false, true, false, false, false, false, false, false, false, false),
                "count" => (HofOp::Count, true, false, false, true, false, false, false, false, false, false, false),
                "find" => (HofOp::Find, true, false, false, false, true, false, false, false, false, false, false),
                "find_index" => (HofOp::FindIndex, true, false, false, false, false, true, false, false, false, false, false),
                "filter" => (HofOp::Filter, true, false, false, false, false, false, true, false, false, false, false),
                "map" => (HofOp::Map, true, false, false, false, false, false, false, true, false, false, false),
                "flat_map" => (HofOp::FlatMap, true, false, false, false, false, false, false, false, true, false, false),
                // Sort family: both the comparator (T, T -> int) and key-
                // function (T -> K) variants route through BIR SynthPool's
                // sort_impl (iterative bottom-up mergesort).
                "sort_by" => (HofOp::SortBy, false, false, false, false, false, false, false, false, false, true, false),
                "sorted_by" => (HofOp::SortedBy, true, false, false, false, false, false, false, false, false, true, false),
                "sort_by_key" => (HofOp::SortByKey, false, false, false, false, false, false, false, false, false, false, true),
                "sorted_by_key" => (HofOp::SortedByKey, true, false, false, false, false, false, false, false, false, false, true),
                _ => return None,
            };
        if lir_args.len() < 2 {
            return None;
        }
        // Fold takes (vec, init, closure): three args.
        if is_fold && lir_args.len() < 3 {
            return None;
        }
        let _ = is_reduce;
        // Result-producing HOFs must have a destination; `each` must not.
        if produces_result && dst.is_none() {
            return None;
        }
        let closure_idx = lir_args.len() - 1;
        // Only handle closures the wrapper can pack today (FuncRef constant
        // or a local of type `__Closure_…`). Callable parameters and other
        // shapes fall through to the backend inliners for now.
        let closure_gir = args.get(closure_idx)?;
        let wrappable = match closure_gir {
            Operand::Constant(Constant::FuncRef(_)) => true,
            Operand::Copy(_) | Operand::Move(_) => self
                .operand_gir_type_name(closure_gir)
                .map_or(false, |n| n.starts_with("__Closure_")),
            _ => false,
        };
        if !wrappable {
            return None;
        }

        let elem_c_name = &rest[..sep];
        let element_ty = super::component_to_lir_type(elem_c_name, self.struct_reg, self.gir_types);

        // Resolve the closure's signature from the pre-computed
        // table. The snapshot gives us both the return type (needed
        // for cross-typed map/flat_map) and the parameter LIR types
        // (needed to pick per-arg ABI tags that match the closure's
        // signature — pass-by-value vs. pass-by-pointer for
        // aggregates). Keys:
        //   - `__Closure_N` arg → `__Closure_N__call` (stored with
        //     `env` param already stripped from param_tys).
        //   - FuncRef constant  → the target function's own name
        //     (stored with all params intact).
        let closure_call_sig = args.get(closure_idx).and_then(|op| {
            let key = match op {
                Operand::Constant(Constant::FuncRef(n)) => n.clone(),
                Operand::Copy(_) | Operand::Move(_) => {
                    let ty_name = self.operand_gir_type_name(op)?;
                    format!("{ty_name}__call")
                }
                _ => return None,
            };
            self.closure_call_sigs.get(&key).cloned()
        });

        // For fold/reduce the closure return type = the accumulator
        // type = dst's declared type. For `map` / `flat_map` / `sort*`
        // use the closure's actual return type (from the signature
        // snapshot) — for maps the dst's declared element type can
        // diverge; for sort the closure returns an int. For
        // predicate-style HOFs (any/all/count/find/find_index) it's
        // `Bool`; for `each` it's `Void`.
        let closure_ret_ty = if is_fold || is_reduce {
            let d = dst.as_ref().expect("fold/reduce requires dst");
            let gir_ty = self.gir_func.locals[d.0 as usize].type_id;
            self.map_type(&gir_ty)
        } else if is_map || is_flat_map || is_sort || is_sort_key {
            match &closure_call_sig {
                Some(sig) => sig.ret_ty.clone(),
                None => return None,
            }
        } else if produces_result {
            LirType::Bool
        } else {
            LirType::Void
        };
        let param_tys: Vec<LirType> = closure_call_sig
            .as_ref()
            .map(|sig| sig.param_tys.clone())
            .unwrap_or_default();
        let sig_known = closure_call_sig.is_some();
        let _ = (is_count, is_find_index, is_filter, is_flat_map, is_sort, is_sort_key);

        // Aggregate-element HOFs require knowing the closure's
        // parameter ABI (pass-by-value vs pass-by-pointer), which we
        // now have via `closure_call_sigs`. If the closure signature
        // isn't in the table (e.g. FuncRef constants to a function
        // whose signature we haven't snapshotted), fall through to
        // the backend inliner so the heuristic ABI doesn't guess
        // wrong on aggregates.
        //
        // `find` / `find_index` also need EnumInit-style scalar
        // `Store` for the payload path (scalar element only).
        if (is_fold || is_reduce)
            && (closure_ret_ty.is_aggregate() || element_ty.is_aggregate())
            && !sig_known
        {
            return None;
        }
        if is_count && element_ty.is_aggregate() && !sig_known {
            return None;
        }
        // `find_index` doesn't touch the element payload — it just
        // threads the i64 index through block args — so aggregate
        // elements work as long as the closure signature is known
        // (predicate ABI). `find` handles aggregate elements via
        // Memcpy in its found_bb.
        if is_find_index && element_ty.is_aggregate() && !sig_known {
            return None;
        }
        if is_find && element_ty.is_aggregate() && !sig_known {
            return None;
        }
        if is_filter && element_ty.is_aggregate() && !sig_known {
            return None;
        }
        if is_map && element_ty.is_aggregate() && !sig_known {
            return None;
        }
        if is_flat_map && element_ty.is_aggregate() && !sig_known {
            return None;
        }
        // Sort body needs closure ABI to pick scalar-Load vs pass-by-ptr
        // for the compare call. Without a sig, fall back to the backend
        // TLS trampoline for now.
        if is_sort && !sig_known {
            return None;
        }
        // Sort-by-key: same sig requirement. Synth handles scalar K via
        // direct `Cmp Le`, Str K via `gorget_str_cmp`, and other struct
        // K via `memcmp`. No aggregate-K fallback to TLS anymore.
        if is_sort_key && !sig_known {
            return None;
        }

        // Wrap the closure arg into a `Ptr` to `GorgetClosure` so the BIR
        // expansion can dispatch via `Inst::CallClosure { EscapedClosure }`.
        let mut lir_args_wrapped = lir_args.to_vec();
        self.wrap_closure_call_args(args, &mut lir_args_wrapped, bb);

        // Derive per-arg ABI from the closure's `__call` parameter
        // types when available (from `closure_call_sig` above).
        // Falls back to a type-shape heuristic (aggregate → Ptr,
        // scalar → Scalar) when the closure signature isn't in the
        // table.
        let abi_from_param_ty = |ty: &LirType| -> crate::ir::abi::AbiKind {
            use crate::ir::abi::AbiKind;
            match ty {
                LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef => AbiKind::Ptr,
                LirType::Struct(_) => AbiKind::ByValue,
                _ => AbiKind::Scalar,
            }
        };
        let fallback_elem_abi = if element_ty.is_aggregate() {
            crate::ir::abi::AbiKind::Ptr
        } else {
            crate::ir::abi::AbiKind::Scalar
        };
        let elem_abi = param_tys
            .last()
            .map(|ty| abi_from_param_ty(ty))
            .unwrap_or(fallback_elem_abi);

        // closure_arg_abis layout depends on the HOF:
        //   each/any/all:   [elem_abi]
        //   fold / reduce:  [acc_abi, elem_abi]
        //   sort_by/sorted: [elem_abi, elem_abi]  (closure takes two T's)
        let closure_arg_abis = if is_fold || is_reduce {
            let fallback_acc_abi = if closure_ret_ty.is_aggregate() {
                crate::ir::abi::AbiKind::Ptr
            } else {
                crate::ir::abi::AbiKind::Scalar
            };
            let acc_abi = param_tys
                .first()
                .map(|ty| abi_from_param_ty(ty))
                .unwrap_or(fallback_acc_abi);
            vec![acc_abi, elem_abi]
        } else if is_sort {
            vec![elem_abi, elem_abi]
        } else if is_sort_key {
            // Key extractor takes one element; returns K (≠ Vector[T]'s T).
            vec![elem_abi]
        } else {
            vec![elem_abi]
        };

        // When a result is produced, allocate a fresh ValueId for the HOF
        // output and plumb it into the caller's local slot. The BIR
        // expansion reuses this ValueId as the `done_bb` block parameter.
        let result_id = if produces_result {
            Some(self.lir_func.next_value())
        } else {
            None
        };

        // Fold's init operand lives at `lir_args[1]`.
        let init_id = if is_fold { Some(lir_args_wrapped[1]) } else { None };

        // For `find`, `value_ty` carries the declared dst type
        // (typically `Struct(Option__T)`) so the BIR expansion can
        // allocate a slot of the right layout without needing to
        // reach back into GIR.
        let value_ty = if is_find {
            let d = dst.as_ref().expect("find requires dst");
            let gir_ty = self.gir_func.locals[d.0 as usize].type_id;
            Some(self.map_type(&gir_ty))
        } else {
            None
        };

        self.push_inst(bb, Inst::HofExpand {
            coll: lir_args_wrapped[0],
            hof_op,
            element_ty,
            value_ty,
            closure: lir_args_wrapped[closure_idx],
            closure_kind: ClosureDispatchKind::EscapedClosure,
            closure_ret_ty,
            closure_arg_abis,
            dst: result_id,
            init: init_id,
        });

        if let (Some(d), Some(r)) = (*dst, result_id) {
            self.store_to_local(d, r, bb);
        }
        Some(bb)
    }

    /// Emit the panic-by-default tag check that precedes a plain
    /// `unwrap()` / `expect()` / `unwrap_error()` payload extraction.
    ///
    /// Splits `bb`: loads the enum tag (I32 at field 0), compares it against
    /// the *valid* tag (`0` = Some/Ok for unwrap/expect; `1` = Error for
    /// unwrap_error), and branches to a fresh `ok` block (returned — the
    /// caller emits the extraction there) or a panic block that calls
    /// `gorget_panic(msg)` and is `Unreachable`.
    ///
    /// Tag convention (the actual runtime layout, established by the existing
    /// `unwrap_or` / `__option_is_some` emit paths below: Some/Ok = tag 0,
    /// Error = tag 1, consumed sentinel = tag 2). NOTE: `EnumKind::Option`'s
    /// doc comment claiming "Tag 0 = None" is STALE/WRONG — filed as its own
    /// cleanup; do not trust it, trust the emit paths.
    ///
    /// The message mirrors the executable definition (ggdef):
    /// `` called `unwrap()` on a `None` value ``, etc. Trap normalization
    /// (D11, proposed) will later re-format panic text via the `T_` registry;
    /// this keeps the current `gorget_panic` format so the fix doesn't
    /// pre-empt it. `gorget_panic(msg)` is auto-rewritten to
    /// `gorget_panic_at(file,line,col,msg)` at the C/LLVM emit boundary and to
    /// `unreachable` after the call.
    fn emit_unwrap_panic_guard(
        &mut self,
        bb: BlockId,
        arg_ptr: ValueId,
        is_unwrap_err: bool,
        variant_word: &str,
    ) -> BlockId {
        let tag_val = self.lir_func.next_value();
        self.push_inst(bb, Inst::Load {
            dst: tag_val, ptr: arg_ptr, ty: LirType::I32,
        });
        let valid_tag = self.emit_i32_const(bb, if is_unwrap_err { 1 } else { 0 });
        let is_valid = self.lir_func.next_value();
        self.push_inst(bb, Inst::Cmp {
            dst: is_valid, op: CmpOp::Eq, lhs: tag_val, rhs: valid_tag,
        });
        let ok_bb = self.lir_func.add_block();
        let panic_bb = self.lir_func.add_block();
        self.set_terminator(bb, Term::Branch {
            cond: is_valid,
            then_block: ok_bb, then_args: vec![],
            else_block: panic_bb, else_args: vec![],
        });
        // Panic block: gorget_trap(code, detail); unreachable. (D11 trap
        // normalization — was gorget_panic(msg).) The `T_` code is typed data
        // from the production TrapKind registry (src/trap.rs), derived from the
        // (receiver, method) shape — NOT a name match: unwrap-on-None →
        // T_UnwrapNone, unwrap-on-Error → T_UnwrapError, unwrap_error-on-Ok →
        // T_UnwrapErrorOnOk. Detail keeps the variant word (Q-C).
        let method = if is_unwrap_err { "unwrap_error" } else { "unwrap" };
        let msg = format!("called `{method}()` on a `{variant_word}` value");
        let trap_code = if is_unwrap_err {
            crate::trap::TrapKind::UnwrapErrorOnOk
        } else if variant_word == "None" {
            crate::trap::TrapKind::UnwrapNone
        } else {
            crate::trap::TrapKind::UnwrapError
        };
        let code_val = self.lower_constant(&Constant::Str(trap_code.code().to_string()), panic_bb);
        let msg_val = self.lower_constant(&Constant::Str(msg), panic_bb);
        self.push_inst(panic_bb, Inst::CallExtern {
            dst: None,
            name: "gorget_trap".to_string(),
            args: vec![code_val, msg_val],
            arg_abis: vec![crate::ir::abi::AbiKind::CStr, crate::ir::abi::AbiKind::CStr],
        });
        self.set_terminator(panic_bb, Term::Unreachable);
        ok_bb
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
    ) -> BlockId {
        // ── Step 7 of BIR lift plan: TraitCall intercept ──
        //
        // Trait-object virtual calls — `Box__<Trait>__<method>(obj, ...)` —
        // become `Inst::TraitCall`. BIR synthesis then rewrites each
        // TraitCall into a `Call` to one dedup'd `__gg_synth_trait_*`
        // helper whose body carries the vtable-dispatch chain. The synth
        // helper has a typed signature, so the C backend's normal Call
        // coercion marshals aggregate args (Str by-value, etc.) without
        // the old static-inline's impl-param peeking. See
        // `docs/devbook/16-bir.md` (`TraitCall` synthesis) and
        // `bir::synth::get_or_emit_trait_helper`.
        if let Some(bb2) = self.try_emit_trait_call(original_name, dst, args, &lir_args, bb) {
            self.emit_post_call_zeros(args, bb2);
            return bb2;
        }
        // ── Step 8 of BIR lift plan: HOF intercept ──
        // Vector `each` / `any` / `all` — emit `Inst::HofExpand` and let BIR
        // lowering generate the loop skeleton. Other HOF variants still flow
        // through the per-backend inline expanders until they are migrated.
        if let Some(bb2) = self.try_emit_dict_hof(original_name, dst, args, &lir_args, bb) {
            self.emit_post_call_zeros(args, bb2);
            return bb2;
        }
        if let Some(bb2) = self.try_emit_dict_get_or(original_name, dst, args, &lir_args, bb) {
            self.emit_post_call_zeros(args, bb2);
            return bb2;
        }
        if let Some(bb2) = self.try_emit_set_hof(original_name, dst, args, &lir_args, bb) {
            self.emit_post_call_zeros(args, bb2);
            return bb2;
        }
        if let Some(bb2) = self.try_emit_vector_each_hof(original_name, dst, args, &lir_args, bb) {
            self.emit_post_call_zeros(args, bb2);
            return bb2;
        }

        // Guard/ReadGuard/WriteGuard get/get_ptr: inline as FieldPtr + Load
        // instead of calling the runtime function. This preserves the concrete
        // inner type through the LIR so the c_lir backend emits correct code.
        // gorget_guard_get(guard*) → load guard->ptr, then load *(T*)ptr
        // gorget_guard_get_ptr(guard*) → load guard->ptr (returns void*)
        if matches!(emit_name, "gorget_guard_get" | "gorget_read_guard_get" | "gorget_write_guard_get") {
            // Track J: the explicit `.get()` method-call path is
            // intercepted at IR-lowering (`src/ir/lowering/exprs/methods.rs`)
            // and routes through `emit_guard_get_ptr` — that intercept is
            // what fixes the double-free / heap-use-after-free class for
            // heap-carrying inners. The `shared`-keyword desugaring path
            // (and possibly others) still reaches THIS LIR arm; for non-
            // heap inners the shallow copy below is SAFE (no drop chain to
            // double-fire). Regressions on heap inners are guarded by the
            // Track J fixture net (Guard/ReadGuard/WriteGuard × String/
            // Vector[int]/Vector[String]/Dict + ASan gate). See follow-up
            // TODO for a typed `borrow_read`-axis retirement that would
            // route ALL Guard-get paths uniformly and let this arm become
            // dead code.
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
                    self.push_inst(bb, Inst::FieldPtr {
                        dst: ptr_val,
                        base: guard_ptr,
                        struct_id: sid,
                        field: 1,
                    });
                    let data_ptr = self.lir_func.next_value();
                    self.push_inst(bb, Inst::Load {
                        dst: data_ptr,
                        ptr: ptr_val,
                        ty: LirType::Ptr,
                    });
                    // Dereference to the concrete inner type.
                    let result = self.lir_func.next_value();
                    self.push_inst(bb, Inst::Load {
                        dst: result,
                        ptr: data_ptr,
                        ty: inner_ty,
                    });
                    self.store_to_local(d, result, bb);
                    return bb;
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
                    self.push_inst(bb, Inst::FieldPtr {
                        dst: ptr_val,
                        base: guard_ptr,
                        struct_id: sid,
                        field: 1,
                    });
                    let data_ptr = self.lir_func.next_value();
                    self.push_inst(bb, Inst::Load {
                        dst: data_ptr,
                        ptr: ptr_val,
                        ty: LirType::Ptr,
                    });
                    self.store_to_local(d, data_ptr, bb);
                    return bb;
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
                let elem_sz = elem_size_from_monomorphized(original_name, self.module_structs, self.module_struct_aliases).unwrap_or(8) as i64;
                let sz_val = self.emit_i64_const(bb, elem_sz);
                lir_args.push(sz_val);
            }
        }
        // gorget_map_new / gorget_dict_new — need sizeof args.
        // For Str/GorgetString keys, use _str variant which
        // sets up the string hash function.
        if (emit_name == "gorget_map_new" || emit_name == "gorget_dict_new") && lir_args.is_empty() {
            let is_dict = emit_name == "gorget_dict_new";
            let (key_sz, val_sz) = dict_elem_sizes_from_monomorphized(original_name, self.module_structs, self.module_struct_aliases);
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
            let elem_sz = concurrency_elem_size(original_name, self.module_structs, self.module_struct_aliases).unwrap_or(8) as i64;
            let sz_val = self.emit_i64_const(bb, elem_sz);
            lir_args.insert(0, sz_val);
        }

        // gorget_channel_new(capacity, elem_size) — GIR passes (capacity).
        if emit_name == "gorget_channel_new" && lir_args.len() == 1 {
            let elem_sz = concurrency_elem_size(original_name, self.module_structs, self.module_struct_aliases).unwrap_or(8) as i64;
            let sz_val = self.emit_i64_const(bb, elem_sz);
            lir_args.push(sz_val);
        }

        // gorget_guard_set(guard, &val, sizeof) and gorget_write_guard_set
        if matches!(emit_name, "gorget_guard_set" | "gorget_write_guard_set")
            && lir_args.len() == 2
        {
            let elem_sz = concurrency_elem_size(original_name, self.module_structs, self.module_struct_aliases).unwrap_or(8) as i64;
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
                self.push_inst(bb, Inst::SlotAddr {
                    dst: slot_ptr,
                    slot,
                });
                lir_args.push(slot_ptr);
                let mut arg_types: Vec<LirType> = args.iter().map(|a| self.operand_lir_type(a)).collect();
                arg_types.push(LirType::Ptr);
                self.ensure_extern(&to_name, &arg_types, &LirType::Void);
                let abis = self.lookup_arg_abis(&to_name);
                self.push_inst(bb, Inst::CallExtern {
                    dst: None,
                    name: to_name,
                    args: lir_args,
                    arg_abis: abis,
                });
                return bb;
            }
        }

        // Derive arg types from GIR operand types (for proper extern declarations).
        let is_printf_like = emit_name == "printf" || emit_name == "fprintf_stderr"
            || emit_name == "gorget_string_format" || emit_name == "gorget_string_format_alloc"
            || emit_name == "snprintf" || emit_name == "sprintf";
        // Clone functions (e.g. EquipBlock__clone) take void* — force Ptr params.
        // operand_lir_type derives from GIR types (aggregate struct), but the
        // generated C clone function signature is always `T clone(void* __p)`.
        let is_clone_fn = emit_name.ends_with("__clone") && !emit_name.starts_with("gorget_");
        let arg_types: Vec<LirType> = if is_printf_like {
            lir_args.iter().map(|_| LirType::Ptr).collect()
        } else if is_clone_fn {
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
        // Closure dispatch: promote to CallClosure instead of CallExtern.
        if (emit_name.starts_with("__callable_") || emit_name.starts_with("__gorget_closure_call_"))
            && !lir_args.is_empty()
        {
            let kind = if emit_name.starts_with("__callable_") {
                ClosureDispatchKind::CallableParam
            } else {
                ClosureDispatchKind::EscapedClosure
            };
            let closure_val = lir_args[0];
            let user_args = lir_args[1..].to_vec();
            // Look up ABI tags from the canonical pipeline (ensure_extern populates them).
            let unique_name = format!("{}__{}", emit_name, self.lir_func.name.replace("::", "__"));
            self.ensure_extern(&unique_name, &arg_types, &ret_ty);
            let call_arg_abis = self.lookup_arg_abis(&unique_name);
            // Skip the closure arg's ABI — only user args need annotation.
            let mut user_abis = if call_arg_abis.len() > 1 {
                call_arg_abis[1..].to_vec()
            } else {
                vec![]
            };
            while user_abis.len() < user_args.len() {
                user_abis.push(crate::ir::abi::AbiKind::Auto);
            }
            // Step 6 of the BIR lift plan: annotate small-aggregate-by-value args
            // with `AbiKind::ByValue`. The LLVM backend formerly decided this by
            // scanning for `SlotAddr` producers of each arg (the heuristic in
            // commit 3a858bcb). Declared LIR signatures are authoritative here —
            // `operand_lir_type` returns the closure's declared param type, and
            // `is_small_aggregate` gives the same threshold the backend uses.
            let user_arg_types = if arg_types.len() > 1 { &arg_types[1..] } else { &arg_types[..0] };
            for (i, ty) in user_arg_types.iter().enumerate().take(user_abis.len()) {
                if user_abis[i] == crate::ir::abi::AbiKind::Auto {
                    if let LirType::Struct(sid) = ty {
                        let sdef = &self.module_structs[sid.0 as usize];
                        if !sdef.is_union_layout
                            && super::types::is_small_aggregate(ty, self.module_structs)
                        {
                            user_abis[i] = crate::ir::abi::AbiKind::ByValue;
                        }
                    }
                }
            }
            // ── The indirect-call argument ABI is a WRITE, not a guess ──────
            // The invariant, spelled once: **an argument's ABI at an indirect
            // call is the CALLEE's DECLARED parameter ABI** — the same fact the
            // `__adapt_*` shim emitter derives from `LirFunction.params`
            // (`src/backend/c_lir/mod.rs`, adapter emission). A `&`
            // (`MutableBorrow`) param is a POINTER in that declared signature,
            // so the call site forwards the pointer.
            //
            // Leaving it `Auto` here made BOTH backends reconstruct the
            // decision from the ARGUMENT's pointee SHAPE
            // (`is_aggregate() && !contains_resource`) — two independent
            // guesses at one missing fact. The guess coincides with the truth
            // for scalars and for resource aggregates, and DIVERGES for a
            // non-resource aggregate behind a `&`: the call site then passed
            // the struct by value where the callee (and its adapter) declared
            // `void*` — SIGSEGV, or a silently lost write-through where the
            // platform ABI hands large aggregates over in a hidden-pointer slot.
            //
            // Params whose ownership is not known HERE keep whatever the
            // by-value promotion above decided: `operand_param_ownerships`
            // returns empty when the callable's GIR type was erased (a
            // `Callable[..]` PARAMETER is typed `unit`, a container element
            // `fn() -> i64`), and empty means UNKNOWN, not "no borrows". Those
            // sites are what the `GG_REPORT_CLOSURE_ABI_GUESS` guard reports.
            let by_ptr = self.declared_closure_param_by_ptr(args.first(), emit_name);
            for (i, is_ptr) in by_ptr.iter().enumerate().take(user_abis.len()) {
                if *is_ptr {
                    user_abis[i] = crate::ir::abi::AbiKind::Ptr;
                }
            }
            // GUARD G1 (Core #6) — "the declared ABI never reached this write
            // site". An indirect call with user args whose callee signature is
            // in NEITHER channel is precisely the state in which the backends
            // must guess from the argument's shape, which is the defect class
            // this write site retires. Reported, not fatal: a legitimately
            // by-value large aggregate also leaves `Auto` here, so the terminal
            // state is a shrinking allowlist, not an assert. Census:
            //   GG_REPORT_CLOSURE_ABI_GUESS=1 gg build <fixture>
            // Ratchet: `closure_abi_guess_census` in tests/integration.rs.
            if by_ptr.is_empty()
                && !user_args.is_empty()
                && std::env::var_os("GG_REPORT_CLOSURE_ABI_GUESS").is_some()
            {
                eprintln!(
                    "[closure-abi-unknown] fn={} callee={} args={}",
                    self.lir_func.name,
                    emit_name,
                    user_args.len()
                );
            }
            let is_void_ret = matches!(ret_ty, LirType::Void);
            let result = if is_void_ret { None } else { dst.map(|_| self.lir_func.next_value()) };
            self.push_inst(bb, Inst::CallClosure {
                dst: result,
                kind,
                closure: closure_val,
                args: user_args,
                arg_abis: user_abis,
                ret_ty: ret_ty.clone(),
            });
            if let (Some(d), Some(r)) = (*dst, result) {
                self.store_to_local(d, r, bb);
            }
            self.emit_post_call_zeros(args, bb);
            return bb;
        }

        // ── Tier 1: Return-value wrapping lifts ──────────────────────────
        if let Some(d) = *dst {
            let dst_idx = d.0 as usize;
            if dst_idx < self.local_to_slot.len() {
                let slot_ty = self.lir_func.slots[self.local_to_slot[dst_idx].0 as usize].ty.clone();
                if let LirType::Struct(opt_sid) = slot_ty {
                    // Read typed `enum_kind` from LIR StructDef (set at LIR
                    // struct registration from GIR's `enum_category`).
                    let slot_kind = self.module_structs.get(opt_sid.0 as usize)
                        .map(|s| s.enum_kind).unwrap_or(crate::lir::EnumKind::NotEnum);

                    if slot_kind == crate::lir::EnumKind::Result {
                        if let Some(err_fn) = super::lifts::last_error_fn_lir(emit_name) {
                            let sdef_len = self.module_structs.get(opt_sid.0 as usize)
                                .map(|s| s.fields.len()).unwrap_or(0);
                            if sdef_len >= 3 {
                                return self.emit_last_error_result_wrap(
                                    emit_name, d, opt_sid, err_fn,
                                    &arg_types, lir_args, args, bb,
                                );
                            }
                        }
                    }

                    if slot_kind == crate::lir::EnumKind::Option {
                        let ext_ret = crate::lir::runtime::RuntimeFn::from_c_name(emit_name)
                            .map(|f| f.resolve_lir_sig(self.struct_reg).ret)
                            .or_else(|| {
                                self.pending_externs.iter()
                                    .find(|e| e.name == emit_name)
                                    .map(|e| e.return_type.clone())
                            });

                        let ext_ret_is_scalar = matches!(ext_ret.as_ref(), Some(
                            LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8
                            | LirType::U64 | LirType::U32 | LirType::U16 | LirType::U8
                            | LirType::F64 | LirType::F32
                        ));
                        let skip_scalar = emit_name.ends_with("__upgrade")
                            || emit_name.ends_with("__recv_timeout")
                            || emit_name.contains("try_parse");
                        if ext_ret_is_scalar && !skip_scalar {
                            return self.emit_sentinel_scalar_option_wrap(
                                emit_name, d, opt_sid,
                                ext_ret.unwrap(), &arg_types, lir_args, args, bb,
                            );
                        }

                        if super::lifts::is_collection_void_return_lir(emit_name) {
                            return self.emit_void_ptr_option_wrap(
                                emit_name, d, opt_sid,
                                &arg_types, lir_args, args, bb,
                            );
                        }

                        if super::lifts::is_nullable_cstr_fn_lir(emit_name) {
                            return self.emit_nullable_cstr_option_wrap(
                                emit_name, d, opt_sid,
                                &arg_types, lir_args, args, bb,
                            );
                        }

                        if super::lifts::is_sentinel_option_fn_lir(emit_name) {
                            return self.emit_sentinel_struct_option_wrap(
                                emit_name, d, opt_sid,
                                &arg_types, lir_args, args, bb,
                            );
                        }

                        if super::lifts::is_nullable_ptr_fn_lir(emit_name) {
                            return self.emit_nullable_ptr_option_wrap(
                                emit_name, d, opt_sid,
                                &arg_types, lir_args, args, bb,
                            );
                        }
                    }
                }
            }
        }

        // ── Tier 2a: Option/Result unwrap/expect ─────────────────────────
        // Extract payload from Option/Result struct: FieldPtr(field=1) + Load.
        // For unwrap_or: tag check + branch, payload or default.
        if !lir_args.is_empty() && args.len() >= 1 {
            let is_unwrap = emit_name == "__option_unwrap" || emit_name == "__result_unwrap"
                || emit_name == "gorget_option_unwrap"
                || emit_name == "__result_unwrap_error"
                || (emit_name.contains("Option__") && emit_name.ends_with("__unwrap"))
                || (emit_name.contains("Result__") && emit_name.ends_with("__unwrap"));
            let is_unwrap_or = emit_name == "__option_unwrap_or" || emit_name == "__result_unwrap_or"
                || (emit_name.contains("Option__") && emit_name.ends_with("__unwrap_or"))
                || (emit_name.contains("Result__") && emit_name.ends_with("__unwrap_or"));
            let is_expect = emit_name == "__option_expect" || emit_name == "__result_expect"
                || (emit_name.contains("Option__") && emit_name.ends_with("__expect"))
                || (emit_name.contains("Result__") && emit_name.ends_with("__expect"));

            if is_unwrap || is_unwrap_or || is_expect {
                if let Some(d) = *dst {
                    // Resolve the Option/Result struct from the arg's type.
                    let arg_lir_ty = self.operand_lir_type(&args[0]);
                    let opt_sid = match &arg_lir_ty {
                        LirType::Struct(sid) | LirType::PtrTo(sid) => Some(*sid),
                        _ => {
                            if let Operand::Copy(place) | Operand::Move(place) = &args[0] {
                                let idx = place.local.0 as usize;
                                // Strategy 2: slot type
                                let from_slot = if idx < self.local_to_slot.len() {
                                    let slot = self.local_to_slot[idx];
                                    match &self.lir_func.slots[slot.0 as usize].ty {
                                        LirType::Struct(sid) => Some(*sid),
                                        _ => None,
                                    }
                                } else { None };
                                // Strategy 3: GIR type name → struct registry
                                from_slot.or_else(|| {
                                    if idx < self.gir_func.locals.len() {
                                        let gir_ty = self.gir_func.locals[idx].type_id;
                                        if let Some(GirType::Named(name)) = self.gir_types.get(gir_ty) {
                                            self.struct_reg.lookup(name)
                                        } else { None }
                                    } else { None }
                                })
                            } else { None }
                        }
                    };
                    // Determine payload field index and type
                    let is_unwrap_err = emit_name.contains("unwrap_err")
                        || emit_name.contains("unwrap_error");
                    let payload_field: u32 = if is_unwrap_err { 2 } else { 1 };
                    let payload_ty = opt_sid.and_then(|sid| {
                        self.module_structs.get(sid.0 as usize)
                            .and_then(|s| s.fields.get(payload_field as usize))
                            .map(|(_, t)| t.clone())
                    }).unwrap_or_else(|| {
                        // Fallback: use destination local's LIR type
                        let dst_gir_ty = self.gir_func.locals[d.0 as usize].type_id;
                        self.map_type(&dst_gir_ty)
                    });
                    // The GIR signals a consuming unwrap (resource payload that
                    // needs drop) by passing the borrow as Operand::Move rather
                    // than Operand::Copy. Read the operand kind — a typed,
                    // upstream-resolved fact — instead of re-deriving it here
                    // from the drop registry.
                    let payload_is_resource = matches!(args[0], Operand::Move(_));
                    let arg_ptr = lir_args[0];

                    if let Some(sid) = opt_sid {

                        if is_unwrap_or && lir_args.len() > 1 {
                            // unwrap_or: tag check + branch
                            let tag_val = self.lir_func.next_value();
                            self.push_inst(bb, Inst::Load {
                                dst: tag_val, ptr: arg_ptr, ty: LirType::I32,
                            });
                            let zero = self.emit_i32_const(bb, 0);
                            let is_some = self.lir_func.next_value();
                            self.push_inst(bb, Inst::Cmp {
                                dst: is_some, op: CmpOp::Eq, lhs: tag_val, rhs: zero,
                            });

                            // Store default to temp slot for SSA threading
                            let result_slot = self.lir_func.add_slot(payload_ty.clone(), None);
                            let default_val = lir_args[1];
                            self.push_inst(bb, Inst::SlotStore {
                                slot: result_slot, value: default_val, is_move: false,
                            });

                            let some_bb = self.lir_func.add_block();
                            let merge_bb = self.lir_func.add_block();

                            self.set_terminator(bb, Term::Branch {
                                cond: is_some,
                                then_block: some_bb, then_args: vec![],
                                else_block: merge_bb, else_args: vec![],
                            });

                            // Some: extract payload, store to result slot
                            let fptr = self.lir_func.next_value();
                            self.push_inst(some_bb, Inst::FieldPtr {
                                dst: fptr, base: arg_ptr, struct_id: sid, field: payload_field,
                            });
                            let payload_val = self.lir_func.next_value();
                            self.push_inst(some_bb, Inst::Load {
                                dst: payload_val, ptr: fptr, ty: payload_ty.clone(),
                            });
                            self.push_inst(some_bb, Inst::SlotStore {
                                slot: result_slot, value: payload_val, is_move: false,
                            });
                            if payload_is_resource {
                                // Set tag to a consumed sentinel (2 = past all valid variants)
                                // so the source Option/Result won't double-drop the payload
                                // when the struct containing it is later dropped.
                                let tag_fptr = self.lir_func.next_value();
                                self.push_inst(some_bb, Inst::FieldPtr {
                                    dst: tag_fptr, base: arg_ptr, struct_id: sid, field: 0,
                                });
                                let consumed_tag = self.emit_i32_const(some_bb, 2);
                                self.push_inst(some_bb, Inst::Store {
                                    ptr: tag_fptr, value: consumed_tag,
                                });
                            }
                            self.set_terminator(some_bb, Term::Jump(merge_bb, vec![]));

                            // Merge: load result from slot
                            let result = self.lir_func.next_value();
                            self.push_inst(merge_bb, Inst::SlotLoad {
                                dst: result, slot: result_slot, ty: payload_ty,
                            });
                            self.store_to_local(d, result, merge_bb);
                            self.emit_post_call_zeros(args, merge_bb);
                            return merge_bb;
                        } else {
                            // Plain unwrap/expect/unwrap_error: panic-by-default
                            // tag check, THEN extract payload in the ok block.
                            // (Reference §15.2: unwrap panics on None/Error,
                            // unwrap_error panics on Ok.) `unwrap_or` can reach
                            // this else-arm when it has no default arg
                            // (`lir_args.len() <= 1`) — a defaulting extractor
                            // must never panic, so gate the guard on
                            // `!is_unwrap_or`.
                            let bb = if !is_unwrap_or {
                                let variant_word = if is_unwrap_err {
                                    "Ok"
                                } else {
                                    match self.module_structs.get(sid.0 as usize).map(|s| s.enum_kind) {
                                        Some(crate::lir::EnumKind::Option) => "None",
                                        _ => "Error",
                                    }
                                };
                                self.emit_unwrap_panic_guard(bb, arg_ptr, is_unwrap_err, variant_word)
                            } else {
                                bb
                            };
                            let fptr = self.lir_func.next_value();
                            self.push_inst(bb, Inst::FieldPtr {
                                dst: fptr, base: arg_ptr, struct_id: sid, field: payload_field,
                            });
                            let payload_val = self.lir_func.next_value();
                            self.push_inst(bb, Inst::Load {
                                dst: payload_val, ptr: fptr, ty: payload_ty,
                            });
                            self.store_to_local(d, payload_val, bb);
                            if payload_is_resource {
                                // Set tag to a consumed sentinel (2 = past all valid variants)
                                // so the source Option/Result won't double-drop the payload
                                // when the struct containing it is later dropped.
                                let tag_fptr = self.lir_func.next_value();
                                self.push_inst(bb, Inst::FieldPtr {
                                    dst: tag_fptr, base: arg_ptr, struct_id: sid, field: 0,
                                });
                                let consumed_tag = self.emit_i32_const(bb, 2);
                                self.push_inst(bb, Inst::Store {
                                    ptr: tag_fptr, value: consumed_tag,
                                });
                            }
                            self.emit_post_call_zeros(args, bb);
                            return bb;
                        }
                    } else {
                        // Fallback: no StructId known. Panic-by-default tag check
                        // first (tag is I32 at offset 0), THEN load payload via raw
                        // pointer arithmetic: payload at offset 8 (4 bytes tag +
                        // 4 bytes padding for 8-byte alignment). `unwrap_or` also
                        // reaches this fallback — a defaulting extractor must never
                        // panic, so gate the guard on `!is_unwrap_or`.
                        let bb = if !is_unwrap_or {
                            let variant_word = if is_unwrap_err {
                                "Ok"
                            } else if emit_name.contains("option") || emit_name.contains("Option") {
                                "None"
                            } else {
                                "Error"
                            };
                            self.emit_unwrap_panic_guard(bb, arg_ptr, is_unwrap_err, variant_word)
                        } else {
                            bb
                        };
                        let payload_offset = if is_unwrap_err { 16i64 } else { 8i64 };
                        let offset_val = self.emit_i64_const(bb, payload_offset);
                        // Cast arg_ptr to i64 for pointer arithmetic
                        let ptr_as_int = self.lir_func.next_value();
                        self.push_inst(bb, Inst::Bitcast {
                            dst: ptr_as_int, value: arg_ptr, to: LirType::I64,
                        });
                        let payload_addr_int = self.lir_func.next_value();
                        self.push_inst(bb, Inst::Add {
                            dst: payload_addr_int, ty: LirType::I64,
                            lhs: ptr_as_int, rhs: offset_val,
                            overflow: crate::lir::Overflow::Wrap,
                        });
                        let payload_addr = self.lir_func.next_value();
                        self.push_inst(bb, Inst::Bitcast {
                            dst: payload_addr, value: payload_addr_int, to: LirType::Ptr,
                        });
                        let payload_val = self.lir_func.next_value();
                        self.push_inst(bb, Inst::Load {
                            dst: payload_val, ptr: payload_addr, ty: payload_ty,
                        });
                        self.store_to_local(d, payload_val, bb);
                        if payload_is_resource {
                            // Set tag to consumed sentinel via direct store to arg_ptr
                            // (tag is I32 at offset 0 of the Option/Result struct).
                            let consumed_tag = self.emit_i32_const(bb, 2);
                            self.push_inst(bb, Inst::Store {
                                ptr: arg_ptr, value: consumed_tag,
                            });
                        }
                        self.emit_post_call_zeros(args, bb);
                        return bb;
                    }
                }
            }
        }

        // ── Tier 2b: Tag checks ──────────────────────────────────────────
        // __option_is_some, __option_is_none, *__is_some, *__is_ok, *__is_none, *__is_err
        // Read the tag field at offset 0 (I32) via FieldPtr + Load, then Cmp with 0.
        if !lir_args.is_empty() {
            let is_some_ok = emit_name == "__option_is_some"
                || emit_name.ends_with("__is_some")
                || emit_name.ends_with("__is_ok");
            let is_none_err = emit_name == "__option_is_none"
                || emit_name.ends_with("__is_none")
                || emit_name.ends_with("__is_err");
            if is_some_ok || is_none_err {
                if let Some(d) = *dst {
                    let ptr = lir_args[0]; // pointer to Option/Result struct
                    // Load tag as I32 from offset 0 (tag is always int32_t at field 0).
                    let tag_val = self.lir_func.next_value();
                    self.push_inst(bb, Inst::Load {
                        dst: tag_val, ptr, ty: LirType::I32,
                    });
                    let zero = self.emit_i32_const(bb, 0);
                    let result = self.lir_func.next_value();
                    let op = if is_some_ok { CmpOp::Eq } else { CmpOp::Ne };
                    self.push_inst(bb, Inst::Cmp {
                        dst: result, op, lhs: tag_val, rhs: zero,
                    });
                    self.store_to_local(d, result, bb);
                    self.emit_post_call_zeros(args, bb);
                    return bb;
                }
            }
        }

        // ── Tier 3c: Builtin type casts ──────────────────────────────────
        // float(x) → IntToFloat/FloatCast, int(x) → FloatToInt/IntCast,
        // bool(x) → IntCast.  int(string) → gorget_str_ord.
        if lir_args.len() == 1 && args.len() == 1 {
            if let Some(d) = *dst {
                let src_ty = self.operand_lir_type(&args[0]);
                let src_is_int = matches!(src_ty,
                    LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64
                    | LirType::U8 | LirType::U16 | LirType::U32 | LirType::U64);
                let src_is_float = matches!(src_ty, LirType::F32 | LirType::F64);
                let src_is_bool = matches!(src_ty, LirType::Bool);
                let src_is_str = self.operand_is_str(&args[0]);

                match emit_name {
                    "float" => {
                        let val = lir_args[0];
                        let result = self.lir_func.next_value();
                        if src_is_int || src_is_bool {
                            self.push_inst(bb, Inst::IntToFloat {
                                dst: result, value: val, to: LirType::F64,
                            });
                        } else if src_is_float {
                            self.push_inst(bb, Inst::FloatCast {
                                dst: result, value: val, to: LirType::F64,
                            });
                        } else {
                            // Fallback: IntToFloat for unknown types
                            self.push_inst(bb, Inst::IntToFloat {
                                dst: result, value: val, to: LirType::F64,
                            });
                        }
                        self.store_to_local(d, result, bb);
                        self.emit_post_call_zeros(args, bb);
                        return bb;
                    }
                    "int" if src_is_str => {
                        // int(string) → gorget_str_ord (Unicode codepoint)
                        let str_ty = self.struct_reg.lookup("GorgetString")
                            .map(LirType::Struct).unwrap_or(LirType::Ptr);
                        self.ensure_extern("gorget_str_ord", &[str_ty], &LirType::I64);
                        let abis = self.lookup_arg_abis("gorget_str_ord");
                        let result = self.lir_func.next_value();
                        self.push_inst(bb, Inst::CallExtern {
                            dst: Some(result),
                            name: "gorget_str_ord".to_string(),
                            args: lir_args,
                            arg_abis: abis,
                        });
                        self.store_to_local(d, result, bb);
                        self.emit_post_call_zeros(args, bb);
                        return bb;
                    }
                    "int" => {
                        let val = lir_args[0];
                        let result = self.lir_func.next_value();
                        if src_is_float {
                            self.push_inst(bb, Inst::FloatToInt {
                                dst: result, value: val, to: LirType::I64,
                            });
                        } else if src_is_bool || src_is_int {
                            self.push_inst(bb, Inst::IntCast {
                                dst: result, value: val, to: LirType::I64,
                            });
                        } else {
                            self.push_inst(bb, Inst::IntCast {
                                dst: result, value: val, to: LirType::I64,
                            });
                        }
                        self.store_to_local(d, result, bb);
                        self.emit_post_call_zeros(args, bb);
                        return bb;
                    }
                    "bool" => {
                        let val = lir_args[0];
                        let result = self.lir_func.next_value();
                        self.push_inst(bb, Inst::IntCast {
                            dst: result, value: val, to: LirType::Bool,
                        });
                        self.store_to_local(d, result, bb);
                        self.emit_post_call_zeros(args, bb);
                        return bb;
                    }
                    _ => {}
                }
            }
        }

        // ── Tier 3a: gorget_str_cat("", val) → type-specific conversion ──
        // When the first arg is an empty string literal, rewrite to
        // gorget_int_to_str / gorget_float_to_str / gorget_bool_to_str.
        if emit_name == "gorget_str_cat" && args.len() == 2 {
            let arg0_is_empty_str = matches!(&args[0], Operand::Constant(Constant::Str(s)) if s.is_empty());
            if arg0_is_empty_str {
                let arg1_ty = self.operand_lir_type(&args[1]);
                let is_int = matches!(arg1_ty,
                    LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64
                    | LirType::U8 | LirType::U16 | LirType::U32 | LirType::U64);
                let is_float = matches!(arg1_ty, LirType::F32 | LirType::F64);
                let is_bool = matches!(arg1_ty, LirType::Bool);

                if is_int || is_float || is_bool {
                    let conv_fn = if is_int { "gorget_int_to_str" }
                        else if is_float { "gorget_float_to_str" }
                        else { "gorget_bool_to_str" };
                    let str_ty = self.struct_reg.lookup("GorgetString")
                        .map(LirType::Struct).unwrap_or(LirType::Ptr);
                    let param_ty = if is_int { LirType::I64 }
                        else if is_float { LirType::F64 }
                        else { LirType::I32 };
                    self.ensure_extern(conv_fn, &[param_ty], &str_ty);
                    let abis = self.lookup_arg_abis(conv_fn);
                    if let Some(d) = *dst {
                        let result = self.lir_func.next_value();
                        self.push_inst(bb, Inst::CallExtern {
                            dst: Some(result),
                            name: conv_fn.to_string(),
                            args: vec![lir_args[1]], // skip the empty string, pass only the value
                            arg_abis: abis,
                        });
                        self.store_to_local(d, result, bb);
                    }
                    self.emit_post_call_zeros(args, bb);
                    return bb;
                }
            }
        }

        // ── Tier 3b: gorget_str_push / gorget_str_push_line type dispatch ──
        if (emit_name == "gorget_str_push" || emit_name == "gorget_str_push_line") && args.len() == 2 {
            let arg1_ty = self.operand_lir_type(&args[1]);
            let is_push_line = emit_name == "gorget_str_push_line";
            let variant = match arg1_ty {
                LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64
                | LirType::U8 | LirType::U16 | LirType::U32 | LirType::U64 =>
                    Some(if is_push_line { "gorget_string_push_line_int" }
                         else { "gorget_string_push_int" }),
                LirType::F32 | LirType::F64 =>
                    Some(if is_push_line { "gorget_string_push_line_float" }
                         else { "gorget_string_push_float" }),
                LirType::Bool =>
                    Some(if is_push_line { "gorget_string_push_line_bool" }
                         else { "gorget_string_push_bool" }),
                _ => None, // Str — use gorget_str_push/push_line as-is
            };
            if let Some(typed_fn) = variant {
                self.ensure_extern(typed_fn, &[LirType::Ptr, arg1_ty.clone()], &LirType::Void);
                // First arg is GorgetString* (pass by pointer), second is scalar value
                let abis = vec![crate::ir::abi::AbiKind::Ptr, crate::ir::abi::AbiKind::Scalar];
                self.push_inst(bb, Inst::CallExtern {
                    dst: None,
                    name: typed_fn.to_string(),
                    args: lir_args,
                    arg_abis: abis,
                });
                self.emit_post_call_zeros(args, bb);
                return bb;
            }
        }

        let actual_emit_name = emit_name.to_string();
        self.ensure_extern(&actual_emit_name, &arg_types, &ret_ty);

        // Array literal path: gorget_array_new(sizeof(T)) from lower_array_literal
        // doesn't carry element type info in original_name (it's just "gorget_array_new").
        // Synthesize a monomorphized name so downstream parsing can determine the element type.
        // Callable element types come through as GirType::FnPtr (no Named name); map
        // them to Vector__GorgetClosure__new so the runtime wires gorget_closure_free
        // as elem_drop (otherwise Vector[Callable].push(closure) leaks sizeof(env) per push).
        let effective_original_name: String = if original_name == "gorget_array_new" && !args.is_empty() {
            if let Some(Operand::Constant(Constant::SizeOf(type_id))) = args.first() {
                match self.gir_types.get(*type_id) {
                    Some(GirType::Named(name)) => format!("Vector__{name}__new"),
                    Some(GirType::FnPtr { .. }) => "Vector__GorgetClosure__new".to_string(),
                    _ => original_name.to_string(),
                }
            } else {
                original_name.to_string()
            }
        } else {
            original_name.to_string()
        };

        let is_void_ret = matches!(ret_ty, LirType::Void);
        let result = if is_void_ret { None } else { dst.map(|_| self.lir_func.next_value()) };

        // Look up ABI tags from the extern declaration (populated by ensure_extern
        // from RuntimeFn::resolve_lir_sig's explicit tags or user-declared extern "C" annotations).
        let call_arg_abis = self.lookup_arg_abis(&actual_emit_name);

        // Parse the effective_original_name ONCE at this layer to determine if this
        // is a collection constructor. If so, emit CollectionCtor directly and compute
        // fn-ptr stores from the parsed element type names.
        //
        // Layering contract: element type names are extracted here from GIR context;
        // infer_fn_ptr_stores_from_types reads GIR type metadata; elem_type_to_meta
        // reads typed LIR struct registry. No downstream pass re-parses the name.
        if let Some((kind, elem_type, val_type, with_capacity, str_keyed)) =
            Self::parse_collection_ctor_info(&actual_emit_name, &effective_original_name)
        {
            let dst_val = result.unwrap_or_else(|| self.lir_func.next_value());
            let elem_or_key = self.elem_type_to_meta(&elem_type);
            let val = val_type.as_deref().map(|n| self.elem_type_to_meta(n));
            let collection_elem_fns = self.infer_fn_ptr_stores_from_types(
                kind, &elem_type, val_type.as_deref(), str_keyed);
            self.push_inst(bb, Inst::CollectionCtor {
                dst: dst_val,
                kind,
                elem_or_key,
                val,
                args: lir_args,
                arg_abis: call_arg_abis,
                with_capacity,
                str_keyed,
            });
            if let (Some(d), Some(r)) = (*dst, result) {
                self.store_to_local(d, r, bb);
            }
            // Wire elem_drop/elem_clone/elem_materialize function pointers at runtime.
            // Must use the slot address (not the return value) since store_to_local
            // may have moved the value.
            if !collection_elem_fns.is_empty() {
                if let Some(d) = dst {
                    let slot = self.local_to_slot[d.0 as usize];
                    let slot_addr = self.lir_func.next_value();
                    self.push_inst(bb, Inst::SlotAddr {
                        dst: slot_addr,
                        slot,
                    });
                    self.emit_collection_fn_ptr_stores(slot_addr, &collection_elem_fns, bb);
                }
            }
            self.emit_post_call_zeros(args, bb);
            return bb;
        }

        self.push_inst(bb, Inst::CallExtern {
            dst: result,
            name: actual_emit_name,
            args: lir_args,
            arg_abis: call_arg_abis,
        });
        if let (Some(d), Some(r)) = (*dst, result) {
            self.store_to_local(d, r, bb);
        }

        // Generic post-call zeroing for Move operands.  Consuming args
        // are marked Operand::Move during GIR lowering; we zero their
        // source slots here.  Non-last-use args are cloned before the call
        // by ensure_owned_at_consuming_arg (the compiler contract), so only
        // Move operands (last-use / explicit !) need post-call zeroing.
        self.emit_post_call_zeros(args, bb);
        bb
    }

    /// Lower printf/fprintf args, expanding Str-typed operands to (int)len, data.
    pub(super) fn lower_printf_args(&mut self, args: &[Operand], bb: BlockId) -> Vec<ValueId> {
        use super::calls::PrintfArgKind;

        let mut lir_args = Vec::new();
        // Pre-scan: classify each arg (1-based) by type for format string rewriting.
        let arg_kinds: Vec<PrintfArgKind> = args.iter().enumerate()
            .map(|(i, a)| {
                if i == 0 { return PrintfArgKind::Int; } // format string itself
                if self.operand_is_str(a) { return PrintfArgKind::Str; }
                let ty = self.operand_lir_type(a);
                match ty {
                    LirType::F32 | LirType::F64 => PrintfArgKind::Float,
                    LirType::Bool => PrintfArgKind::Bool,
                    _ => PrintfArgKind::Int,
                }
            })
            .collect();
        let needs_format_fix = arg_kinds[1..].iter().any(|k| *k != PrintfArgKind::Int);

        for (i, arg) in args.iter().enumerate() {
            if i == 0 {
                // First arg is always the format string (const char*).
                // If any subsequent args need format fixes, rewrite the format string.
                if needs_format_fix {
                    if let Operand::Constant(Constant::Str(fmt_str)) = arg {
                        let fixed = fix_printf_format(fmt_str, &arg_kinds[1..]);
                        let fixed_val = self.lir_func.next_value();
                        self.push_inst(bb, Inst::StrLit {
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
            } else if arg_kinds[i] == PrintfArgKind::Str {
                // Str-typed arg: expand to (int)len, (const char*)data for %.*s.
                if let Operand::Copy(place) | Operand::Move(place) = arg {
                    let slot = self.local_to_slot[place.local.0 as usize];
                    let slot_ty = self.lir_func.slots[slot.0 as usize].ty.clone();
                    let str_sid = self.struct_reg.lookup("GorgetString");
                    let is_ptr_to_str = slot_ty == LirType::Ptr;
                    let struct_id = match &slot_ty {
                        LirType::Struct(sid) => *sid,
                        _ => str_sid.unwrap_or(StructId(0)),
                    };

                    // Get the base pointer to the GorgetString struct.
                    // For Struct slots: SlotAddr gives it directly.
                    // For Ptr slots: the slot holds a pointer TO the struct — load it.
                    let str_base = if is_ptr_to_str {
                        let addr = self.lir_func.next_value();
                        self.push_inst(bb, Inst::SlotAddr {
                            dst: addr, slot,
                        });
                        let loaded = self.lir_func.next_value();
                        self.push_inst(bb, Inst::Load {
                            dst: loaded, ptr: addr, ty: LirType::Ptr,
                        });
                        loaded
                    } else {
                        let base = self.lir_func.next_value();
                        self.push_inst(bb, Inst::SlotAddr {
                            dst: base, slot,
                        });
                        base
                    };

                    // 32-byte Str fields: 0=data (Ptr), 1=cap (I64), 2=len (I64), 3=alloc (Ptr)
                    // Load .len (field 2) → cast to I32 for printf %.*s precision
                    let len_ptr = self.lir_func.next_value();
                    self.push_inst(bb, Inst::FieldPtr {
                        dst: len_ptr,
                        base: str_base,
                        struct_id,
                        field: 2,
                    });
                    let len_load = self.lir_func.next_value();
                    self.push_inst(bb, Inst::Load {
                        dst: len_load,
                        ptr: len_ptr,
                        ty: LirType::I64,
                    });
                    let len_i32 = self.lir_func.next_value();
                    self.push_inst(bb, Inst::IntCast {
                        dst: len_i32,
                        value: len_load,
                        to: LirType::I32,
                    });
                    lir_args.push(len_i32);

                    // Load .data (field 0) — const char*
                    let data_ptr = self.lir_func.next_value();
                    self.push_inst(bb, Inst::FieldPtr {
                        dst: data_ptr,
                        base: str_base,
                        struct_id,
                        field: 0,
                    });
                    let data_load = self.lir_func.next_value();
                    self.push_inst(bb, Inst::Load {
                        dst: data_load,
                        ptr: data_ptr,
                        ty: LirType::Ptr,
                    });
                    lir_args.push(data_load);
                } else {
                    lir_args.push(self.lower_operand(arg, bb));
                }
            } else if arg_kinds[i] == PrintfArgKind::Float {
                // Float arg: promote F32 to F64 for C variadic, pass directly.
                let float_val = self.lower_operand(arg, bb);
                let ty = self.operand_lir_type(arg);
                if ty == LirType::F32 {
                    let promoted = self.lir_func.next_value();
                    self.push_inst(bb, Inst::FloatCast {
                        dst: promoted,
                        value: float_val,
                        to: LirType::F64,
                    });
                    lir_args.push(promoted);
                } else {
                    lir_args.push(float_val);
                }
            } else if arg_kinds[i] == PrintfArgKind::Bool {
                // Bool arg: convert to "true"/"false" via gorget_bool_to_str (returns Str struct),
                // then decompose into (i32 len, ptr data) like any Str arg.
                let bool_val = self.lower_operand(arg, bb);

                // Allocate a temp slot to hold the result Str struct
                let str_slot = self.lir_func.add_slot(
                    LirType::Struct(self.struct_reg.lookup("GorgetString").unwrap()),
                    Some("__bool_str".into()),
                );
                let str_struct_ty = LirType::Struct(self.struct_reg.lookup("GorgetString").unwrap());
                self.ensure_extern("gorget_bool_to_str", &[LirType::Bool], &str_struct_ty);
                let abis = self.lookup_arg_abis("gorget_bool_to_str");
                let str_result = self.lir_func.next_value();
                self.push_inst(bb, Inst::CallExtern {
                    dst: Some(str_result),
                    name: "gorget_bool_to_str".to_string(),
                    args: vec![bool_val],
                    arg_abis: abis,
                });
                // Store result to slot
                self.push_inst(bb, Inst::SlotStore {
                    slot: str_slot,
                    value: str_result,
                    is_move: true,
                });
                // The slot owns a fresh heap allocation ("true"/"false" copy,
                // gorget_string_adopt). It is born here — below GIR drop
                // registration — so record it for the post-call free emitted
                // by the printf-like call path (print-temp leak class).
                self.printf_str_temps.push(str_slot);

                // Decompose: load .len (field 2 under 32-byte layout) → i32, load .data (field 0) → ptr
                let str_sid = self.struct_reg.lookup("GorgetString").unwrap();
                let base = self.lir_func.next_value();
                self.push_inst(bb, Inst::SlotAddr { dst: base, slot: str_slot });
                let len_ptr = self.lir_func.next_value();
                self.push_inst(bb, Inst::FieldPtr { dst: len_ptr, base, struct_id: str_sid, field: 2 });
                let len_load = self.lir_func.next_value();
                self.push_inst(bb, Inst::Load { dst: len_load, ptr: len_ptr, ty: LirType::I64 });
                let len_i32 = self.lir_func.next_value();
                self.push_inst(bb, Inst::IntCast { dst: len_i32, value: len_load, to: LirType::I32 });
                lir_args.push(len_i32);

                let base2 = self.lir_func.next_value();
                self.push_inst(bb, Inst::SlotAddr { dst: base2, slot: str_slot });
                let data_ptr = self.lir_func.next_value();
                self.push_inst(bb, Inst::FieldPtr { dst: data_ptr, base: base2, struct_id: str_sid, field: 0 });
                let data_load = self.lir_func.next_value();
                self.push_inst(bb, Inst::Load { dst: data_load, ptr: data_ptr, ty: LirType::Ptr });
                lir_args.push(data_load);
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
            let slot = self.local_to_slot[place.local.0 as usize];
            let slot_ty = self.lir_func.slots[slot.0 as usize].ty.clone();
            if slot_ty.is_aggregate() {
                // For aggregates, return address of slot.
                let addr = self.lir_func.next_value();
                self.push_inst(bb, Inst::SlotAddr { dst: addr, slot });
                addr
            } else {
                let dst = self.lir_func.next_value();
                self.push_inst(bb, Inst::SlotLoad {
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
            // Read the typed `metadata.is_box` flag rather than a name probe.
            let is_box_deref = place.projections.first() == Some(&Projection::Deref)
                && self.gir_types.is_box(self.gir_func.locals[place.local.0 as usize].type_id);
            if ty.is_aggregate() && !is_box_deref {
                addr // aggregates: the address IS the value
            } else {
                let dst = self.lir_func.next_value();
                self.push_inst(bb, Inst::Load {
                    dst,
                    ptr: addr,
                    ty,
                });
                dst
            }
        }
    }

}

/// Check if a GIR function name is an Option/Result unwrap/expect pseudo-function
/// that should be intercepted before func_index lookup (these have no C implementation).
fn is_unwrap_like_name(name: &str) -> bool {
    name == "__option_unwrap" || name == "__result_unwrap"
        || name == "__option_unwrap_or" || name == "__result_unwrap_or"
        || name == "__result_unwrap_error"
        || name == "gorget_option_unwrap"
        || name == "__option_expect" || name == "__result_expect"
        || (name.contains("Option__") && (name.ends_with("__unwrap") || name.ends_with("__unwrap_or") || name.ends_with("__expect")))
        || (name.contains("Result__") && (name.ends_with("__unwrap") || name.ends_with("__unwrap_or") || name.ends_with("__expect")))
}
