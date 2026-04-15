//! Tier 1 semantic lifts from C backend to LIR lowerer.
//!
//! Each lift replaces a single CallExtern that the C backend would have expanded
//! inline with a sequence of LIR instructions (Call, Branch, FieldPtr, Load, Store,
//! etc.) that express the same logic portably.
//!
//! DISABLED: The block-splitting lifts write to the destination slot via
//! FieldPtr+Store in branch blocks, but the C backend's value type inference
//! assigns the slot's default-initialized value (0/NULL) to variables in later
//! blocks, causing NULL dereferences.  Requires fixing C backend slot tracking
//! across split blocks before enabling.

#![allow(dead_code)]

use super::*;
use super::types::c_sizeof_lir_type;

// ── Free helper functions ────────────────────────────────────────────────

/// Functions that return nullable `void*` (element pointer) — NULL means "not found" / empty.
pub(super) fn is_collection_void_return_lir(name: &str) -> bool {
    matches!(name,
        "gorget_array_get" | "gorget_array_pop" | "gorget_array_first" | "gorget_array_last"
        | "gorget_array_safe_pop" | "gorget_array_remove_opt"
        | "gorget_map_get"
        | "gorget_heap_pop" | "gorget_heap_peek"
        | "gorget_shared_get" | "gorget_shared_get_ptr"
        | "gorget_channel_recv"
    )
}

/// Functions that return nullable `const char*` — NULL means None.
pub(super) fn is_nullable_cstr_fn_lir(name: &str) -> bool {
    matches!(name,
        "gorget_regex_match_group" | "gorget_regex_match_group_by_name" | "gorget_getenv"
    )
}

/// Functions that return a struct with a sentinel field indicating "no match".
pub(super) fn is_sentinel_option_fn_lir(name: &str) -> bool {
    matches!(name,
        "gorget_regex_find" | "gorget_regex_find_at"
        | "gorget_regex_find_pat" | "gorget_regex_fullmatch"
    )
}

/// Functions that return nullable pointers (non-NULL = Some, NULL = None).
pub(super) fn is_nullable_ptr_fn_lir(name: &str) -> bool {
    name.starts_with("Weak__") && name.ends_with("__upgrade")
}

/// Map a runtime function name to its thread-local error-getter.
/// Must match the C backend's `last_error_fn` in helpers.rs exactly.
pub(super) fn last_error_fn_lir(name: &str) -> Option<&'static str> {
    if name.starts_with("gorget_udp_") { return Some("gorget_udp_last_error"); }
    if name.starts_with("gorget_server_socket_") { return Some("gorget_server_socket_last_error"); }
    if name.starts_with("gorget_socket_") { return Some("gorget_socket_last_error"); }
    // TlsServer before Tls to avoid prefix collision
    if name.starts_with("gorget_tls_server_") { return Some("gorget_tls_server_last_error"); }
    if name.starts_with("gorget_tls_") { return Some("gorget_tls_last_error"); }
    if name.starts_with("gorget_regex_") { return Some("gorget_regex_last_error"); }
    if name.starts_with("gorget_crypto_") { return Some("gorget_crypto_last_error"); }
    if name == "gorget_process_spawn" { return Some("gorget_process_spawn_err"); }
    if name == "gorget_parse_int" || name == "gorget_parse_float" { return Some("gorget_parse_last_error"); }
    None
}

/// Whether this collection method consumes the element (pop/remove — no clone needed).
fn is_consuming_method(name: &str) -> bool {
    matches!(name,
        "gorget_array_safe_pop" | "gorget_array_remove_opt"
        | "gorget_map_remove" | "gorget_set_remove"
    )
}

// ── FuncLowering expansion methods ───────────────────────────────────────

impl<'a> FuncLowering<'a> {

    // ── Tier 1a: nullable void* → Option wrapping ─────────────────────

    pub(super) fn emit_void_ptr_option_wrap(
        &mut self,
        emit_name: &str,
        original_name: &str,
        d: ir::types::LocalId,
        opt_sid: StructId,
        arg_types: &[LirType],
        lir_args: Vec<ValueId>,
        args: &[Operand],
        bb: BlockId,
    ) -> BlockId {
        let sdef = &self.module_structs[opt_sid.0 as usize];
        let payload_ty = sdef.fields.get(1).map(|(_, t)| t.clone())
            .unwrap_or(LirType::I64);
        let payload_is_ptr = payload_ty.is_ptr();
        let consuming = is_consuming_method(emit_name);

        // 1. Call the extern (returns void*)
        let raw_ptr = self.lir_func.next_value();
        self.ensure_extern(emit_name, arg_types, &LirType::Ptr);
        let call_abis = self.lookup_arg_abis(emit_name);
        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
            dst: Some(raw_ptr),
            name: emit_name.to_string(),
            args: lir_args,
            original_name: Some(original_name.to_string()),
            arg_abis: call_abis,
        });

        // 2. Get slot address and zero the struct
        let slot = self.local_to_slot[d.0 as usize];
        let slot_addr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
            dst: slot_addr, slot,
        });
        self.emit_memset_zero(slot_addr, &LirType::Struct(opt_sid), bb);

        // 3. Null check → Branch
        let null_val = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::NullPtr { dst: null_val });
        let is_not_null = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::Cmp {
            dst: is_not_null, op: CmpOp::Ne, lhs: raw_ptr, rhs: null_val,
        });

        let some_bb = self.lir_func.add_block();
        let none_bb = self.lir_func.add_block();
        let merge_bb = self.lir_func.add_block();

        self.lir_func.block_mut(bb).terminator = Term::Branch {
            cond: is_not_null,
            then_block: some_bb, then_args: vec![],
            else_block: none_bb, else_args: vec![],
        };

        // 4. Some branch: tag=0, store payload
        self.emit_enum_tag_store(slot_addr, opt_sid, 0, some_bb);
        let payload_ptr = self.lir_func.next_value();
        self.lir_func.block_mut(some_bb).insts.push(Inst::FieldPtr {
            dst: payload_ptr, base: slot_addr, struct_id: opt_sid, field: 1,
        });

        if payload_is_ptr {
            // Option[T &]: store pointer directly (borrowed reference)
            self.lir_func.block_mut(some_bb).insts.push(Inst::Store {
                ptr: payload_ptr, value: raw_ptr,
            });
        } else if let Some(clone_fn) = self.resource_clone_fn_for_payload(&payload_ty, consuming) {
            // Resource type from borrowing read: clone to avoid double-free
            let cloned = self.lir_func.next_value();
            self.ensure_extern(&clone_fn, &[LirType::Ptr], &payload_ty);
            let abis = self.lookup_arg_abis(&clone_fn);
            self.lir_func.block_mut(some_bb).insts.push(Inst::CallExtern {
                dst: Some(cloned),
                name: clone_fn,
                args: vec![raw_ptr],
                original_name: None, arg_abis: abis,
            });
            self.lir_func.block_mut(some_bb).insts.push(Inst::Store {
                ptr: payload_ptr, value: cloned,
            });
        } else if payload_ty.is_aggregate() {
            // Aggregate payload: memcpy from raw_ptr
            let sz = c_sizeof_lir_type(&payload_ty, self.module_structs) as i64;
            let sz_val = self.emit_i64_const(some_bb, sz);
            self.ensure_extern("memcpy", &[LirType::Ptr, LirType::Ptr, LirType::I64], &LirType::Ptr);
            let abis = self.lookup_arg_abis("memcpy");
            self.lir_func.block_mut(some_bb).insts.push(Inst::CallExtern {
                dst: None,
                name: "memcpy".to_string(),
                args: vec![payload_ptr, raw_ptr, sz_val],
                original_name: None, arg_abis: abis,
            });
        } else {
            // Scalar payload: dereference void* to concrete type
            let loaded = self.lir_func.next_value();
            self.lir_func.block_mut(some_bb).insts.push(Inst::Load {
                dst: loaded, ptr: raw_ptr, ty: payload_ty.clone(),
            });
            self.lir_func.block_mut(some_bb).insts.push(Inst::Store {
                ptr: payload_ptr, value: loaded,
            });
        }

        self.lir_func.block_mut(some_bb).terminator = Term::Jump(merge_bb, vec![]);

        // 5. None branch: tag=1
        self.emit_enum_tag_store(slot_addr, opt_sid, 1, none_bb);
        self.lir_func.block_mut(none_bb).terminator = Term::Jump(merge_bb, vec![]);

        // 6. Post-call zeros in merge block
        self.emit_post_call_zeros(args, merge_bb);
        merge_bb
    }

    // ── Tier 1b: nullable cstr → Option[String] ──────────────────────

    pub(super) fn emit_nullable_cstr_option_wrap(
        &mut self,
        emit_name: &str,
        original_name: &str,
        d: ir::types::LocalId,
        opt_sid: StructId,
        arg_types: &[LirType],
        lir_args: Vec<ValueId>,
        args: &[Operand],
        bb: BlockId,
    ) -> BlockId {
        // 1. Call the extern (returns const char*)
        let raw_ptr = self.lir_func.next_value();
        self.ensure_extern(emit_name, arg_types, &LirType::Ptr);
        let call_abis = self.lookup_arg_abis(emit_name);
        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
            dst: Some(raw_ptr),
            name: emit_name.to_string(),
            args: lir_args,
            original_name: Some(original_name.to_string()),
            arg_abis: call_abis,
        });

        // 2. Slot address + zero
        let slot = self.local_to_slot[d.0 as usize];
        let slot_addr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
            dst: slot_addr, slot,
        });
        self.emit_memset_zero(slot_addr, &LirType::Struct(opt_sid), bb);

        // 3. Null check → Branch
        let null_val = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::NullPtr { dst: null_val });
        let is_not_null = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::Cmp {
            dst: is_not_null, op: CmpOp::Ne, lhs: raw_ptr, rhs: null_val,
        });

        let some_bb = self.lir_func.add_block();
        let none_bb = self.lir_func.add_block();
        let merge_bb = self.lir_func.add_block();

        self.lir_func.block_mut(bb).terminator = Term::Branch {
            cond: is_not_null,
            then_block: some_bb, then_args: vec![],
            else_block: none_bb, else_args: vec![],
        };

        // 4. Some branch: tag=0, wrap cstr via gorget_str_from_cstr → Some_0
        self.emit_enum_tag_store(slot_addr, opt_sid, 0, some_bb);
        let str_ty = self.struct_reg.lookup("GorgetString")
            .map(LirType::Struct).unwrap_or(LirType::Ptr);
        self.ensure_extern("gorget_str_from_cstr", &[LirType::Ptr], &str_ty);
        let abis = self.lookup_arg_abis("gorget_str_from_cstr");
        let wrapped = self.lir_func.next_value();
        self.lir_func.block_mut(some_bb).insts.push(Inst::CallExtern {
            dst: Some(wrapped),
            name: "gorget_str_from_cstr".to_string(),
            args: vec![raw_ptr],
            original_name: None, arg_abis: abis,
        });
        let payload_ptr = self.lir_func.next_value();
        self.lir_func.block_mut(some_bb).insts.push(Inst::FieldPtr {
            dst: payload_ptr, base: slot_addr, struct_id: opt_sid, field: 1,
        });
        self.emit_value_to_field(wrapped, payload_ptr, &str_ty, some_bb);

        self.lir_func.block_mut(some_bb).terminator = Term::Jump(merge_bb, vec![]);

        // 5. None branch: tag=1
        self.emit_enum_tag_store(slot_addr, opt_sid, 1, none_bb);
        self.lir_func.block_mut(none_bb).terminator = Term::Jump(merge_bb, vec![]);

        // 6. Post-call zeros
        self.emit_post_call_zeros(args, merge_bb);
        merge_bb
    }

    // ── Tier 1c: last_error → Result wrapping ────────────────────────

    pub(super) fn emit_last_error_result_wrap(
        &mut self,
        emit_name: &str,
        original_name: &str,
        d: ir::types::LocalId,
        result_sid: StructId,
        err_fn_name: &str,
        arg_types: &[LirType],
        lir_args: Vec<ValueId>,
        args: &[Operand],
        bb: BlockId,
    ) -> BlockId {
        let sdef = &self.module_structs[result_sid.0 as usize];
        let ok_ty = sdef.fields.get(1).map(|(_, t)| t.clone()).unwrap_or(LirType::I64);

        // 1. Call the extern function (returns the raw ok value)
        // Register the extern with the ok type — the actual C function return type.
        // The _r wrapper in emit_types.rs has a fallback struct-name lookup
        // so it doesn't depend on this extern declaration's return type.
        let raw_result = self.lir_func.next_value();
        self.ensure_extern(emit_name, arg_types, &ok_ty);
        let call_abis = self.lookup_arg_abis(emit_name);
        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
            dst: Some(raw_result),
            name: emit_name.to_string(),
            args: lir_args,
            original_name: Some(original_name.to_string()),
            arg_abis: call_abis,
        });

        // 2. Call the error getter (returns const char* — NULL if no error)
        let err_ptr = self.lir_func.next_value();
        self.ensure_extern(err_fn_name, &[], &LirType::Ptr);
        let err_abis = self.lookup_arg_abis(err_fn_name);
        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
            dst: Some(err_ptr),
            name: err_fn_name.to_string(),
            args: vec![],
            original_name: None, arg_abis: err_abis,
        });

        // 3. Slot address + zero
        let slot = self.local_to_slot[d.0 as usize];
        let slot_addr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
            dst: slot_addr, slot,
        });
        self.emit_memset_zero(slot_addr, &LirType::Struct(result_sid), bb);

        // 4. Null check on error → Branch
        let null_val = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::NullPtr { dst: null_val });
        let has_error = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::Cmp {
            dst: has_error, op: CmpOp::Ne, lhs: err_ptr, rhs: null_val,
        });

        let err_bb = self.lir_func.add_block();
        let ok_bb = self.lir_func.add_block();
        let merge_bb = self.lir_func.add_block();

        self.lir_func.block_mut(bb).terminator = Term::Branch {
            cond: has_error,
            then_block: err_bb, then_args: vec![],
            else_block: ok_bb, else_args: vec![],
        };

        // 5. Error branch: tag=1, wrap error string
        self.emit_enum_tag_store(slot_addr, result_sid, 1, err_bb);
        let str_ty = self.struct_reg.lookup("GorgetString")
            .map(LirType::Struct).unwrap_or(LirType::Ptr);
        self.ensure_extern("gorget_str_from_cstr", &[LirType::Ptr], &str_ty);
        let abis = self.lookup_arg_abis("gorget_str_from_cstr");
        let err_str = self.lir_func.next_value();
        self.lir_func.block_mut(err_bb).insts.push(Inst::CallExtern {
            dst: Some(err_str),
            name: "gorget_str_from_cstr".to_string(),
            args: vec![err_ptr],
            original_name: None, arg_abis: abis,
        });
        // Store error string into Error_0 field (field 2)
        let err_field_ptr = self.lir_func.next_value();
        self.lir_func.block_mut(err_bb).insts.push(Inst::FieldPtr {
            dst: err_field_ptr, base: slot_addr, struct_id: result_sid, field: 2,
        });
        self.emit_value_to_field(err_str, err_field_ptr, &str_ty, err_bb);
        self.lir_func.block_mut(err_bb).terminator = Term::Jump(merge_bb, vec![]);

        // 6. Ok branch: tag=0, store raw value
        self.emit_enum_tag_store(slot_addr, result_sid, 0, ok_bb);
        let ok_field_ptr = self.lir_func.next_value();
        self.lir_func.block_mut(ok_bb).insts.push(Inst::FieldPtr {
            dst: ok_field_ptr, base: slot_addr, struct_id: result_sid, field: 1,
        });
        self.emit_value_to_field(raw_result, ok_field_ptr, &ok_ty, ok_bb);
        self.lir_func.block_mut(ok_bb).terminator = Term::Jump(merge_bb, vec![]);

        // 7. Post-call zeros
        self.emit_post_call_zeros(args, merge_bb);
        merge_bb
    }

    // ── Tier 1d: sentinel scalar → Option wrapping ───────────────────

    pub(super) fn emit_sentinel_scalar_option_wrap(
        &mut self,
        emit_name: &str,
        original_name: &str,
        d: ir::types::LocalId,
        opt_sid: StructId,
        ext_ret_ty: LirType,
        arg_types: &[LirType],
        lir_args: Vec<ValueId>,
        args: &[Operand],
        bb: BlockId,
    ) -> BlockId {
        let is_signed_int = matches!(ext_ret_ty,
            LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8);

        // 1. Call the extern
        let raw_val = self.lir_func.next_value();
        self.ensure_extern(emit_name, arg_types, &ext_ret_ty);
        let call_abis = self.lookup_arg_abis(emit_name);
        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
            dst: Some(raw_val),
            name: emit_name.to_string(),
            args: lir_args,
            original_name: Some(original_name.to_string()),
            arg_abis: call_abis,
        });

        // 2. Slot address + zero
        let slot = self.local_to_slot[d.0 as usize];
        let slot_addr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
            dst: slot_addr, slot,
        });
        self.emit_memset_zero(slot_addr, &LirType::Struct(opt_sid), bb);

        if is_signed_int {
            // 3. Sentinel check: raw >= 0 → Some, else None
            let zero = self.emit_i64_const(bb, 0);
            let is_valid = self.lir_func.next_value();
            self.lir_func.block_mut(bb).insts.push(Inst::Cmp {
                dst: is_valid, op: CmpOp::Ge, lhs: raw_val, rhs: zero,
            });

            let some_bb = self.lir_func.add_block();
            let none_bb = self.lir_func.add_block();
            let merge_bb = self.lir_func.add_block();

            self.lir_func.block_mut(bb).terminator = Term::Branch {
                cond: is_valid,
                then_block: some_bb, then_args: vec![],
                else_block: none_bb, else_args: vec![],
            };

            // Some branch: tag=0, store value
            self.emit_enum_tag_store(slot_addr, opt_sid, 0, some_bb);
            let payload_ptr = self.lir_func.next_value();
            self.lir_func.block_mut(some_bb).insts.push(Inst::FieldPtr {
                dst: payload_ptr, base: slot_addr, struct_id: opt_sid, field: 1,
            });
            self.lir_func.block_mut(some_bb).insts.push(Inst::Store {
                ptr: payload_ptr, value: raw_val,
            });
            self.lir_func.block_mut(some_bb).terminator = Term::Jump(merge_bb, vec![]);

            // None branch: tag=1
            self.emit_enum_tag_store(slot_addr, opt_sid, 1, none_bb);
            self.lir_func.block_mut(none_bb).terminator = Term::Jump(merge_bb, vec![]);

            self.emit_post_call_zeros(args, merge_bb);
            merge_bb
        } else {
            // Unsigned/float: always Some (this case is rare)
            self.emit_enum_tag_store(slot_addr, opt_sid, 0, bb);
            let payload_ptr = self.lir_func.next_value();
            self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                dst: payload_ptr, base: slot_addr, struct_id: opt_sid, field: 1,
            });
            self.lir_func.block_mut(bb).insts.push(Inst::Store {
                ptr: payload_ptr, value: raw_val,
            });
            self.emit_post_call_zeros(args, bb);
            bb
        }
    }

    // ── Sentinel struct → Option (gorget_regex_find etc.) ────────────

    pub(super) fn emit_sentinel_struct_option_wrap(
        &mut self,
        emit_name: &str,
        original_name: &str,
        d: ir::types::LocalId,
        opt_sid: StructId,
        arg_types: &[LirType],
        lir_args: Vec<ValueId>,
        args: &[Operand],
        bb: BlockId,
    ) -> BlockId {
        // The extern returns a struct (e.g., GorgetRegexMatch).
        // Sentinel: .start == -1 means no match → None.
        let match_sid = self.struct_reg.lookup("GorgetRegexMatch");
        let match_ty = match_sid.map(LirType::Struct)
            .unwrap_or(LirType::I64);

        // 1. Call the extern — returns match struct
        // Use output pointer for aggregate return.
        let match_slot = self.lir_func.add_slot(match_ty.clone(), None);
        let match_addr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
            dst: match_addr, slot: match_slot,
        });

        // Emit the call. For struct returns, append output pointer.
        let to_name = format!("{emit_name}_to");
        let mut call_args = lir_args;
        call_args.push(match_addr);
        let mut call_arg_types = arg_types.to_vec();
        call_arg_types.push(LirType::Ptr);
        self.ensure_extern(&to_name, &call_arg_types, &LirType::Void);
        let call_abis = self.lookup_arg_abis(&to_name);
        // Fallback: if _to variant isn't registered, use the original name
        let (final_name, final_args, final_abis) = if self.pending_externs.iter().any(|e| e.name == to_name) {
            (to_name, call_args, call_abis)
        } else {
            // Use original call, result stored in a temp value
            self.ensure_extern(emit_name, arg_types, &match_ty);
            let abis = self.lookup_arg_abis(emit_name);
            let raw_val = self.lir_func.next_value();
            self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                dst: Some(raw_val),
                name: emit_name.to_string(),
                args: call_args[..call_args.len()-1].to_vec(),
                original_name: Some(original_name.to_string()),
                arg_abis: abis,
            });
            self.lir_func.block_mut(bb).insts.push(Inst::SlotStore {
                slot: match_slot, value: raw_val, is_move: true,
            });
            // Skip the CallExtern below
            (String::new(), vec![], vec![])
        };
        if !final_name.is_empty() {
            self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                dst: None,
                name: final_name,
                args: final_args,
                original_name: Some(original_name.to_string()),
                arg_abis: final_abis,
            });
        }

        // 2. Read .start field (field 0) of the match struct
        let start_field = if let Some(sid) = match_sid {
            let fptr = self.lir_func.next_value();
            self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                dst: fptr, base: match_addr, struct_id: sid, field: 0,
            });
            let start_val = self.lir_func.next_value();
            self.lir_func.block_mut(bb).insts.push(Inst::Load {
                dst: start_val, ptr: fptr, ty: LirType::I64,
            });
            start_val
        } else {
            match_addr // fallback
        };

        // 3. Check start != -1
        let neg_one = self.emit_i64_const(bb, -1);
        let is_valid = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::Cmp {
            dst: is_valid, op: CmpOp::Ne, lhs: start_field, rhs: neg_one,
        });

        // 4. Slot address + zero
        let slot = self.local_to_slot[d.0 as usize];
        let slot_addr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
            dst: slot_addr, slot,
        });
        self.emit_memset_zero(slot_addr, &LirType::Struct(opt_sid), bb);

        let some_bb = self.lir_func.add_block();
        let none_bb = self.lir_func.add_block();
        let merge_bb = self.lir_func.add_block();

        self.lir_func.block_mut(bb).terminator = Term::Branch {
            cond: is_valid,
            then_block: some_bb, then_args: vec![],
            else_block: none_bb, else_args: vec![],
        };

        // 5. Some: tag=0, memcpy match struct into payload
        self.emit_enum_tag_store(slot_addr, opt_sid, 0, some_bb);
        let payload_ptr = self.lir_func.next_value();
        self.lir_func.block_mut(some_bb).insts.push(Inst::FieldPtr {
            dst: payload_ptr, base: slot_addr, struct_id: opt_sid, field: 1,
        });
        let sz = c_sizeof_lir_type(&match_ty, self.module_structs) as i64;
        let sz_val = self.emit_i64_const(some_bb, sz);
        self.lir_func.block_mut(some_bb).insts.push(Inst::Memcpy {
            dst_ptr: payload_ptr, src_ptr: match_addr, size: sz_val,
        });
        self.lir_func.block_mut(some_bb).terminator = Term::Jump(merge_bb, vec![]);

        // 6. None: tag=1
        self.emit_enum_tag_store(slot_addr, opt_sid, 1, none_bb);
        self.lir_func.block_mut(none_bb).terminator = Term::Jump(merge_bb, vec![]);

        self.emit_post_call_zeros(args, merge_bb);
        merge_bb
    }

    // ── Nullable ptr → Option (Weak__T__upgrade) ─────────────────────

    pub(super) fn emit_nullable_ptr_option_wrap(
        &mut self,
        emit_name: &str,
        original_name: &str,
        d: ir::types::LocalId,
        opt_sid: StructId,
        arg_types: &[LirType],
        lir_args: Vec<ValueId>,
        args: &[Operand],
        bb: BlockId,
    ) -> BlockId {
        let sdef = &self.module_structs[opt_sid.0 as usize];
        let payload_ty = sdef.fields.get(1).map(|(_, t)| t.clone())
            .unwrap_or(LirType::Ptr);

        // 1. Call the extern (returns nullable pointer — always Ptr at C level)
        let raw_ptr = self.lir_func.next_value();
        self.ensure_extern(emit_name, arg_types, &LirType::Ptr);
        let call_abis = self.lookup_arg_abis(emit_name);
        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
            dst: Some(raw_ptr),
            name: emit_name.to_string(),
            args: lir_args,
            original_name: Some(original_name.to_string()),
            arg_abis: call_abis,
        });

        // 2. Slot address + zero
        let slot = self.local_to_slot[d.0 as usize];
        let slot_addr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
            dst: slot_addr, slot,
        });
        self.emit_memset_zero(slot_addr, &LirType::Struct(opt_sid), bb);

        // 3. Null check → Branch
        let null_val = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::NullPtr { dst: null_val });
        let is_not_null = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::Cmp {
            dst: is_not_null, op: CmpOp::Ne, lhs: raw_ptr, rhs: null_val,
        });

        let some_bb = self.lir_func.add_block();
        let none_bb = self.lir_func.add_block();
        let merge_bb = self.lir_func.add_block();

        self.lir_func.block_mut(bb).terminator = Term::Branch {
            cond: is_not_null,
            then_block: some_bb, then_args: vec![],
            else_block: none_bb, else_args: vec![],
        };

        // 4. Some: tag=0, store value
        self.emit_enum_tag_store(slot_addr, opt_sid, 0, some_bb);
        let payload_ptr = self.lir_func.next_value();
        self.lir_func.block_mut(some_bb).insts.push(Inst::FieldPtr {
            dst: payload_ptr, base: slot_addr, struct_id: opt_sid, field: 1,
        });
        if payload_ty.is_aggregate() {
            let sz = c_sizeof_lir_type(&payload_ty, self.module_structs) as i64;
            let sz_val = self.emit_i64_const(some_bb, sz);
            self.lir_func.block_mut(some_bb).insts.push(Inst::Memcpy {
                dst_ptr: payload_ptr, src_ptr: raw_ptr, size: sz_val,
            });
        } else {
            self.lir_func.block_mut(some_bb).insts.push(Inst::Store {
                ptr: payload_ptr, value: raw_ptr,
            });
        }
        self.lir_func.block_mut(some_bb).terminator = Term::Jump(merge_bb, vec![]);

        // 5. None: tag=1
        self.emit_enum_tag_store(slot_addr, opt_sid, 1, none_bb);
        self.lir_func.block_mut(none_bb).terminator = Term::Jump(merge_bb, vec![]);

        self.emit_post_call_zeros(args, merge_bb);
        merge_bb
    }

    // ── Shared helpers ───────────────────────────────────────────────

    /// Emit memset(ptr, 0, sizeof(ty)).
    pub(super) fn emit_memset_zero(&mut self, ptr: ValueId, ty: &LirType, bb: BlockId) {
        let size = c_sizeof_lir_type(ty, self.module_structs) as i64;
        let size_val = self.emit_i64_const(bb, size);
        let zero_byte = self.emit_i32_const(bb, 0);
        self.ensure_extern("memset", &[LirType::Ptr, LirType::I32, LirType::I64], &LirType::Ptr);
        let abis = self.lookup_arg_abis("memset");
        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
            dst: None,
            name: "memset".to_string(),
            args: vec![ptr, zero_byte, size_val],
            original_name: None, arg_abis: abis,
        });
    }

    /// For a resource-type payload from a collection read, return the clone function
    /// name.  Returns None for Ptr payloads (borrowed) or consuming methods (moved out).
    fn resource_clone_fn_for_payload(&self, payload_ty: &LirType, consuming: bool) -> Option<String> {
        if consuming { return None; }
        match payload_ty {
            LirType::Struct(sid) => {
                let name = self.module_structs.get(sid.0 as usize)
                    .map(|s| s.name.as_str()).unwrap_or("");
                match name {
                    "GorgetArray" => Some("gorget_array_clone".into()),
                    "GorgetMap" => Some("gorget_map_clone".into()),
                    "GorgetSet" => Some("gorget_set_clone".into()),
                    "GorgetString" => Some("gorget_string_clone".into()),
                    _ => {
                        // Recursive/custom-drop types
                        if self.recursive_drop_structs.contains_key(name)
                            || self.recursive_drop_enums.contains_key(name) {
                            Some(format!("{name}__clone"))
                        } else {
                            None
                        }
                    }
                }
            }
            _ => None,
        }
    }

    /// Store a value into a struct field pointer, handling aggregate types correctly.
    /// For aggregates with known size > 0, stores to a temp slot then memcpy.
    /// For scalars or zero-size aggregates (opaque types), uses direct Store.
    pub(super) fn emit_value_to_field(
        &mut self,
        value: ValueId,
        field_ptr: ValueId,
        ty: &LirType,
        bb: BlockId,
    ) {
        let sz = c_sizeof_lir_type(ty, self.module_structs);
        if ty.is_aggregate() && sz > 0 {
            // Aggregate with known size: temp slot → memcpy
            let temp_slot = self.lir_func.add_slot(ty.clone(), None);
            self.lir_func.block_mut(bb).insts.push(Inst::SlotStore {
                slot: temp_slot, value, is_move: true,
            });
            let temp_addr = self.lir_func.next_value();
            self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                dst: temp_addr, slot: temp_slot,
            });
            let sz_val = self.emit_i64_const(bb, sz as i64);
            self.lir_func.block_mut(bb).insts.push(Inst::Memcpy {
                dst_ptr: field_ptr, src_ptr: temp_addr, size: sz_val,
            });
        } else {
            // Scalar or opaque type: direct store
            self.lir_func.block_mut(bb).insts.push(Inst::Store {
                ptr: field_ptr, value,
            });
        }
    }
}
