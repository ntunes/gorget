//! Tier 1 semantic lifts from C backend to LIR lowerer.
//!
//! Each lift replaces a single CallExtern that the C backend would have expanded
//! inline with a sequence of LIR instructions (Call, Branch, FieldPtr, Load, Store,
//! etc.) that express the same logic portably.
//!
//! The block-splitting lifts create new basic blocks mid-instruction-sequence.
//! The corresponding C backend handlers in emit_call_extern.rs must be stripped
//! when a lift is enabled, or the C backend will double-wrap the result.

use super::*;
use super::types::c_sizeof_lir_type;

// ── Free helper functions ────────────────────────────────────────────────

/// Functions that return nullable `void*` (element pointer) — NULL means "not found" / empty.
pub(super) fn is_collection_void_return_lir(name: &str) -> bool {
    matches!(name,
        "gorget_array_get" | "gorget_array_pop" | "gorget_array_first" | "gorget_array_last"
        | "gorget_array_safe_pop" | "gorget_array_remove_opt"
        | "gorget_map_get" | "gorget_map_remove_opt" | "gorget_map_swap_remove_opt"
        | "gorget_heap_pop" | "gorget_heap_peek"
        | "gorget_shared_get" | "gorget_shared_get_ptr"
        | "gorget_channel_recv"
    )
}

/// Functions that return nullable `const char*` — NULL means None.
pub(super) fn is_nullable_cstr_fn_lir(name: &str) -> bool {
    matches!(name,
        "gorget_getenv"
    )
}

/// Functions that return a struct with a sentinel field indicating "no match".
/// Currently none — the old gorget_regex_find family routed through here when
/// xtd.regex was a PCRE2 wrapper; the new pure-Gorget engine does its own
/// Option wrapping in Gorget.
pub(super) fn is_sentinel_option_fn_lir(_name: &str) -> bool {
    false
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
    if name.starts_with("gorget_crypto_") { return Some("gorget_crypto_last_error"); }
    if name == "gorget_process_spawn" { return Some("gorget_process_spawn_err"); }
    if name == "gorget_parse_int" || name == "gorget_parse_float" { return Some("gorget_parse_last_error"); }
    None
}

/// Whether this collection method consumes the element (pop/remove — no clone needed).
fn is_consuming_method(name: &str) -> bool {
    matches!(name,
        "gorget_array_safe_pop" | "gorget_array_remove_opt"
        | "gorget_map_remove" | "gorget_map_remove_opt" | "gorget_set_remove"
        | "gorget_map_swap_remove" | "gorget_map_swap_remove_opt" | "gorget_set_swap_remove"
    )
}

// ── FuncLowering expansion methods ───────────────────────────────────────

impl<'a> FuncLowering<'a> {

    // ── Tier 1a: nullable void* → Option wrapping ─────────────────────

    pub(super) fn emit_void_ptr_option_wrap(
        &mut self,
        emit_name: &str,
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
        self.push_inst(bb, Inst::CallExtern {
            dst: Some(raw_ptr),
            name: emit_name.to_string(),
            args: lir_args,
            arg_abis: call_abis,
        });

        // 2. Store raw_ptr to a temp slot so the SSA pass can thread it
        //    across the branch blocks (raw ValueIds aren't tracked by SSA).
        let raw_slot = self.lir_func.add_slot(LirType::Ptr, None);
        self.push_inst(bb, Inst::SlotStore {
            slot: raw_slot, value: raw_ptr, is_move: false,
        });

        // 3. Get slot address and zero the struct
        let slot = self.local_to_slot[d.0 as usize];
        let slot_addr = self.lir_func.next_value();
        self.push_inst(bb, Inst::SlotAddr {
            dst: slot_addr, slot,
        });
        self.emit_memset_zero(slot_addr, &LirType::Struct(opt_sid), bb);

        // 3. Null check → Branch
        let null_val = self.lir_func.next_value();
        self.push_inst(bb, Inst::NullPtr { dst: null_val });
        let is_not_null = self.lir_func.next_value();
        self.push_inst(bb, Inst::Cmp {
            dst: is_not_null, op: CmpOp::Ne, lhs: raw_ptr, rhs: null_val,
        });

        let some_bb = self.lir_func.add_block();
        let none_bb = self.lir_func.add_block();
        let merge_bb = self.lir_func.add_block();

        self.set_terminator(bb, Term::Branch {
            cond: is_not_null,
            then_block: some_bb, then_args: vec![],
            else_block: none_bb, else_args: vec![],
        });

        // 5. Some branch: reload raw_ptr from slot, materialize the payload
        // value, then EnumInit with tag=0.
        let raw_in_some = self.lir_func.next_value();
        self.push_inst(some_bb, Inst::SlotLoad {
            dst: raw_in_some, slot: raw_slot, ty: LirType::Ptr,
        });

        let payload_val: ValueId = if payload_is_ptr {
            raw_in_some
        } else if let Some(clone_fn) = self.resource_clone_fn_for_payload(&payload_ty, consuming) {
            let cloned = self.lir_func.next_value();
            self.ensure_extern(&clone_fn, &[LirType::Ptr], &payload_ty);
            let abis = self.lookup_arg_abis(&clone_fn);
            self.push_inst(some_bb, Inst::CallExtern {
                dst: Some(cloned),
                name: clone_fn,
                args: vec![raw_in_some],
                arg_abis: abis,
            });
            if payload_ty.is_aggregate() {
                // Clone returns an aggregate by value — stash in a slot so
                // EnumInit can Memcpy from its address.
                let slot = self.lir_func.add_slot(payload_ty.clone(), None);
                self.push_inst(some_bb, Inst::SlotStore {
                    slot, value: cloned, is_move: true,
                });
                let addr = self.lir_func.next_value();
                self.push_inst(some_bb, Inst::SlotAddr {
                    dst: addr, slot,
                });
                addr
            } else {
                cloned
            }
        } else if payload_ty.is_aggregate() {
            // EnumInit's BIR expansion issues a Memcpy for aggregate payloads
            // when `payload` is a ptr to the aggregate. `raw_in_some` IS that ptr.
            raw_in_some
        } else {
            let loaded = self.lir_func.next_value();
            self.push_inst(some_bb, Inst::Load {
                dst: loaded, ptr: raw_in_some, ty: payload_ty.clone(),
            });
            loaded
        };

        self.push_inst(some_bb, Inst::EnumInit {
            target: slot_addr, struct_id: opt_sid,
            variant_tag: 0, fields: vec![(1, payload_val)],
        });
        self.set_terminator(some_bb, Term::Jump(merge_bb, vec![]));

        // 5. None branch: tag=1 (no payload)
        self.push_inst(none_bb, Inst::EnumInit {
            target: slot_addr, struct_id: opt_sid,
            variant_tag: 1, fields: vec![],
        });
        self.set_terminator(none_bb, Term::Jump(merge_bb, vec![]));

        // 6. Post-call zeros in merge block
        self.emit_post_call_zeros(args, merge_bb);
        merge_bb
    }

    // ── Tier 1b: nullable cstr → Option[String] ──────────────────────

    pub(super) fn emit_nullable_cstr_option_wrap(
        &mut self,
        emit_name: &str,
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
        self.push_inst(bb, Inst::CallExtern {
            dst: Some(raw_ptr),
            name: emit_name.to_string(),
            args: lir_args,
            arg_abis: call_abis,
        });

        // 2. Slot address + zero
        let slot = self.local_to_slot[d.0 as usize];
        let slot_addr = self.lir_func.next_value();
        self.push_inst(bb, Inst::SlotAddr {
            dst: slot_addr, slot,
        });
        self.emit_memset_zero(slot_addr, &LirType::Struct(opt_sid), bb);

        // 3. Null check → Branch
        let null_val = self.lir_func.next_value();
        self.push_inst(bb, Inst::NullPtr { dst: null_val });
        let is_not_null = self.lir_func.next_value();
        self.push_inst(bb, Inst::Cmp {
            dst: is_not_null, op: CmpOp::Ne, lhs: raw_ptr, rhs: null_val,
        });

        let some_bb = self.lir_func.add_block();
        let none_bb = self.lir_func.add_block();
        let merge_bb = self.lir_func.add_block();

        self.set_terminator(bb, Term::Branch {
            cond: is_not_null,
            then_block: some_bb, then_args: vec![],
            else_block: none_bb, else_args: vec![],
        });

        // 4. Some branch: wrap cstr via gorget_str_from_cstr, then EnumInit
        // with tag=0, payload=wrapped.
        let str_ty = self.struct_reg.lookup("GorgetString")
            .map(LirType::Struct).unwrap_or(LirType::Ptr);
        self.ensure_extern("gorget_str_from_cstr", &[LirType::Ptr], &str_ty);
        let abis = self.lookup_arg_abis("gorget_str_from_cstr");
        // gorget_str_from_cstr returns a GorgetString by value; stash it in a
        // temp slot so we can pass its address to EnumInit (aggregate payloads
        // are copied via Memcpy from a ptr).
        let wrapped = self.lir_func.next_value();
        self.push_inst(some_bb, Inst::CallExtern {
            dst: Some(wrapped),
            name: "gorget_str_from_cstr".to_string(),
            args: vec![raw_ptr],
            arg_abis: abis,
        });
        let wrapped_slot = self.lir_func.add_slot(str_ty.clone(), None);
        self.push_inst(some_bb, Inst::SlotStore {
            slot: wrapped_slot, value: wrapped, is_move: true,
        });
        let wrapped_addr = self.lir_func.next_value();
        self.push_inst(some_bb, Inst::SlotAddr {
            dst: wrapped_addr, slot: wrapped_slot,
        });
        self.push_inst(some_bb, Inst::EnumInit {
            target: slot_addr, struct_id: opt_sid,
            variant_tag: 0, fields: vec![(1, wrapped_addr)],
        });
        self.set_terminator(some_bb, Term::Jump(merge_bb, vec![]));

        // 5. None branch: tag=1
        self.push_inst(none_bb, Inst::EnumInit {
            target: slot_addr, struct_id: opt_sid,
            variant_tag: 1, fields: vec![],
        });
        self.set_terminator(none_bb, Term::Jump(merge_bb, vec![]));

        // 6. Post-call zeros
        self.emit_post_call_zeros(args, merge_bb);
        merge_bb
    }

    // ── Tier 1c: last_error → Result wrapping ────────────────────────

    pub(super) fn emit_last_error_result_wrap(
        &mut self,
        emit_name: &str,
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
        self.push_inst(bb, Inst::CallExtern {
            dst: Some(raw_result),
            name: emit_name.to_string(),
            args: lir_args,
            arg_abis: call_abis,
        });

        // 2. Call the error getter (returns const char* — NULL if no error)
        let err_ptr = self.lir_func.next_value();
        self.ensure_extern(err_fn_name, &[], &LirType::Ptr);
        let err_abis = self.lookup_arg_abis(err_fn_name);
        self.push_inst(bb, Inst::CallExtern {
            dst: Some(err_ptr),
            name: err_fn_name.to_string(),
            args: vec![],
            arg_abis: err_abis,
        });

        // 3. Slot address + zero
        let slot = self.local_to_slot[d.0 as usize];
        let slot_addr = self.lir_func.next_value();
        self.push_inst(bb, Inst::SlotAddr {
            dst: slot_addr, slot,
        });
        self.emit_memset_zero(slot_addr, &LirType::Struct(result_sid), bb);

        // 4. Null check on error → Branch
        let null_val = self.lir_func.next_value();
        self.push_inst(bb, Inst::NullPtr { dst: null_val });
        let has_error = self.lir_func.next_value();
        self.push_inst(bb, Inst::Cmp {
            dst: has_error, op: CmpOp::Ne, lhs: err_ptr, rhs: null_val,
        });

        let err_bb = self.lir_func.add_block();
        let ok_bb = self.lir_func.add_block();
        let merge_bb = self.lir_func.add_block();

        self.set_terminator(bb, Term::Branch {
            cond: has_error,
            then_block: err_bb, then_args: vec![],
            else_block: ok_bb, else_args: vec![],
        });

        // 5. Error branch: wrap error cstr, then EnumInit with tag=1, variant_idx=1.
        //    (Result layout: field 0 = tag, field 1 = Ok payload, field 2 = Error payload.)
        let str_ty = self.struct_reg.lookup("GorgetString")
            .map(LirType::Struct).unwrap_or(LirType::Ptr);
        self.ensure_extern("gorget_str_from_cstr", &[LirType::Ptr], &str_ty);
        let abis = self.lookup_arg_abis("gorget_str_from_cstr");
        let err_str = self.lir_func.next_value();
        self.push_inst(err_bb, Inst::CallExtern {
            dst: Some(err_str),
            name: "gorget_str_from_cstr".to_string(),
            args: vec![err_ptr],
            arg_abis: abis,
        });
        let err_slot = self.lir_func.add_slot(str_ty.clone(), None);
        self.push_inst(err_bb, Inst::SlotStore {
            slot: err_slot, value: err_str, is_move: true,
        });
        let err_addr = self.lir_func.next_value();
        self.push_inst(err_bb, Inst::SlotAddr {
            dst: err_addr, slot: err_slot,
        });
        self.push_inst(err_bb, Inst::EnumInit {
            target: slot_addr, struct_id: result_sid,
            variant_tag: 1, fields: vec![(2, err_addr)],
        });
        self.set_terminator(err_bb, Term::Jump(merge_bb, vec![]));

        // 6. Ok branch: EnumInit with tag=0, variant_idx=0, payload=raw_result.
        //    For aggregate ok types, raw_result is the aggregate value — stash
        //    it in a slot so EnumInit can Memcpy from its address.
        let ok_payload: ValueId = if ok_ty.is_aggregate() {
            let ok_slot = self.lir_func.add_slot(ok_ty.clone(), None);
            self.push_inst(ok_bb, Inst::SlotStore {
                slot: ok_slot, value: raw_result, is_move: true,
            });
            let ok_addr = self.lir_func.next_value();
            self.push_inst(ok_bb, Inst::SlotAddr {
                dst: ok_addr, slot: ok_slot,
            });
            ok_addr
        } else {
            raw_result
        };
        self.push_inst(ok_bb, Inst::EnumInit {
            target: slot_addr, struct_id: result_sid,
            variant_tag: 0, fields: vec![(1, ok_payload)],
        });
        self.set_terminator(ok_bb, Term::Jump(merge_bb, vec![]));

        // 7. Post-call zeros
        self.emit_post_call_zeros(args, merge_bb);
        merge_bb
    }

    // ── Tier 1d: sentinel scalar → Option wrapping ───────────────────

    pub(super) fn emit_sentinel_scalar_option_wrap(
        &mut self,
        emit_name: &str,
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
        self.push_inst(bb, Inst::CallExtern {
            dst: Some(raw_val),
            name: emit_name.to_string(),
            args: lir_args,
            arg_abis: call_abis,
        });

        // 2. Slot address + zero
        let slot = self.local_to_slot[d.0 as usize];
        let slot_addr = self.lir_func.next_value();
        self.push_inst(bb, Inst::SlotAddr {
            dst: slot_addr, slot,
        });
        self.emit_memset_zero(slot_addr, &LirType::Struct(opt_sid), bb);

        if is_signed_int {
            // 3. Sentinel check: raw >= 0 → Some, else None
            let zero = self.emit_i64_const(bb, 0);
            let is_valid = self.lir_func.next_value();
            self.push_inst(bb, Inst::Cmp {
                dst: is_valid, op: CmpOp::Ge, lhs: raw_val, rhs: zero,
            });

            let some_bb = self.lir_func.add_block();
            let none_bb = self.lir_func.add_block();
            let merge_bb = self.lir_func.add_block();

            self.set_terminator(bb, Term::Branch {
                cond: is_valid,
                then_block: some_bb, then_args: vec![],
                else_block: none_bb, else_args: vec![],
            });

            // Some branch: EnumInit { tag=0, payload=raw_val }
            self.push_inst(some_bb, Inst::EnumInit {
                target: slot_addr, struct_id: opt_sid,
                variant_tag: 0, fields: vec![(1, raw_val)],
            });
            self.set_terminator(some_bb, Term::Jump(merge_bb, vec![]));

            // None branch: EnumInit { tag=1 }
            self.push_inst(none_bb, Inst::EnumInit {
                target: slot_addr, struct_id: opt_sid,
                variant_tag: 1, fields: vec![],
            });
            self.set_terminator(none_bb, Term::Jump(merge_bb, vec![]));

            self.emit_post_call_zeros(args, merge_bb);
            merge_bb
        } else {
            // Unsigned/float: always Some (this case is rare)
            self.push_inst(bb, Inst::EnumInit {
                target: slot_addr, struct_id: opt_sid,
                variant_tag: 0, fields: vec![(1, raw_val)],
            });
            self.emit_post_call_zeros(args, bb);
            bb
        }
    }

    // ── Sentinel struct → Option (gorget_regex_find etc.) ────────────

    pub(super) fn emit_sentinel_struct_option_wrap(
        &mut self,
        emit_name: &str,
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
        self.push_inst(bb, Inst::SlotAddr {
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
            self.push_inst(bb, Inst::CallExtern {
                dst: Some(raw_val),
                name: emit_name.to_string(),
                args: call_args[..call_args.len()-1].to_vec(),
                arg_abis: abis,
            });
            self.push_inst(bb, Inst::SlotStore {
                slot: match_slot, value: raw_val, is_move: true,
            });
            // Skip the CallExtern below
            (String::new(), vec![], vec![])
        };
        if !final_name.is_empty() {
            self.push_inst(bb, Inst::CallExtern {
                dst: None,
                name: final_name,
                args: final_args,
                arg_abis: final_abis,
            });
        }

        // 2. Read .start field (field 0) of the match struct
        let start_field = if let Some(sid) = match_sid {
            let fptr = self.lir_func.next_value();
            self.push_inst(bb, Inst::FieldPtr {
                dst: fptr, base: match_addr, struct_id: sid, field: 0,
            });
            let start_val = self.lir_func.next_value();
            self.push_inst(bb, Inst::Load {
                dst: start_val, ptr: fptr, ty: LirType::I64,
            });
            start_val
        } else {
            match_addr // fallback
        };

        // 3. Check start != -1
        let neg_one = self.emit_i64_const(bb, -1);
        let is_valid = self.lir_func.next_value();
        self.push_inst(bb, Inst::Cmp {
            dst: is_valid, op: CmpOp::Ne, lhs: start_field, rhs: neg_one,
        });

        // 4. Slot address + zero
        let slot = self.local_to_slot[d.0 as usize];
        let slot_addr = self.lir_func.next_value();
        self.push_inst(bb, Inst::SlotAddr {
            dst: slot_addr, slot,
        });
        self.emit_memset_zero(slot_addr, &LirType::Struct(opt_sid), bb);

        let some_bb = self.lir_func.add_block();
        let none_bb = self.lir_func.add_block();
        let merge_bb = self.lir_func.add_block();

        self.set_terminator(bb, Term::Branch {
            cond: is_valid,
            then_block: some_bb, then_args: vec![],
            else_block: none_bb, else_args: vec![],
        });

        // 5. Some: EnumInit with payload = ptr to match struct (aggregate → Memcpy).
        let _ = match_ty; // payload field type is looked up by struct_id in EnumInit
        self.push_inst(some_bb, Inst::EnumInit {
            target: slot_addr, struct_id: opt_sid,
            variant_tag: 0, fields: vec![(1, match_addr)],
        });
        self.set_terminator(some_bb, Term::Jump(merge_bb, vec![]));

        // 6. None: tag=1
        self.push_inst(none_bb, Inst::EnumInit {
            target: slot_addr, struct_id: opt_sid,
            variant_tag: 1, fields: vec![],
        });
        self.set_terminator(none_bb, Term::Jump(merge_bb, vec![]));

        self.emit_post_call_zeros(args, merge_bb);
        merge_bb
    }

    // ── Nullable ptr → Option (Weak__T__upgrade) ─────────────────────

    pub(super) fn emit_nullable_ptr_option_wrap(
        &mut self,
        emit_name: &str,
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
        self.push_inst(bb, Inst::CallExtern {
            dst: Some(raw_ptr),
            name: emit_name.to_string(),
            args: lir_args,
            arg_abis: call_abis,
        });

        // 2. Slot address + zero
        let slot = self.local_to_slot[d.0 as usize];
        let slot_addr = self.lir_func.next_value();
        self.push_inst(bb, Inst::SlotAddr {
            dst: slot_addr, slot,
        });
        self.emit_memset_zero(slot_addr, &LirType::Struct(opt_sid), bb);

        // 3. Null check → Branch
        let null_val = self.lir_func.next_value();
        self.push_inst(bb, Inst::NullPtr { dst: null_val });
        let is_not_null = self.lir_func.next_value();
        self.push_inst(bb, Inst::Cmp {
            dst: is_not_null, op: CmpOp::Ne, lhs: raw_ptr, rhs: null_val,
        });

        let some_bb = self.lir_func.add_block();
        let none_bb = self.lir_func.add_block();
        let merge_bb = self.lir_func.add_block();

        self.set_terminator(bb, Term::Branch {
            cond: is_not_null,
            then_block: some_bb, then_args: vec![],
            else_block: none_bb, else_args: vec![],
        });

        // 4. Some: EnumInit (tag=0, payload=raw_ptr). For aggregate payload types,
        //    raw_ptr is a ptr to the aggregate and BIR expansion will Memcpy.
        let _ = payload_ty; // payload type is carried via struct_id in EnumInit
        self.push_inst(some_bb, Inst::EnumInit {
            target: slot_addr, struct_id: opt_sid,
            variant_tag: 0, fields: vec![(1, raw_ptr)],
        });
        self.set_terminator(some_bb, Term::Jump(merge_bb, vec![]));

        // 5. None: tag=1
        self.push_inst(none_bb, Inst::EnumInit {
            target: slot_addr, struct_id: opt_sid,
            variant_tag: 1, fields: vec![],
        });
        self.set_terminator(none_bb, Term::Jump(merge_bb, vec![]));

        self.emit_post_call_zeros(args, merge_bb);
        merge_bb
    }

    // ── Shared helpers ───────────────────────────────────────────────

    /// Emit memset(ptr, 0, sizeof(ty)).
    pub(super) fn emit_memset_zero(&mut self, ptr: ValueId, ty: &LirType, bb: BlockId) {
        let size_val = self.emit_size_of(bb, ty.clone());
        let zero_byte = self.emit_i32_const(bb, 0);
        self.ensure_extern("memset", &[LirType::Ptr, LirType::I32, LirType::I64], &LirType::Ptr);
        let abis = self.lookup_arg_abis("memset");
        self.push_inst(bb, Inst::CallExtern {
            dst: None,
            name: "memset".to_string(),
            args: vec![ptr, zero_byte, size_val],
            arg_abis: abis,
        });
    }

    /// For a resource-type payload from a collection read, return the clone function
    /// name.  Returns None for Ptr payloads (borrowed) or consuming methods (moved out).
    ///
    /// **Asymmetry note (2026-05-04):** `Vector.get()` returns `Option[Ref[T]]` —
    /// payload is a Ptr, no clone. `Dict.get()` returns `Option[V]` BY VALUE,
    /// so the payload here is the actual element type. For resource payloads
    /// we MUST deep-clone or the Option's payload aliases the collection's
    /// storage; the unwrap result then carries a shallow copy whose drop
    /// double-frees against the collection's val_drop.
    /// Closures (`GorgetClosure`) hit the same shape — they own a heap-alloc'd
    /// env behind the 16-byte handle, so a memcpy aliases the source env.
    /// `gorget_closure_clone_to_owned` produces a fresh-env handle. Without
    /// this branch, `Dict[K, Callable].get().unwrap()` double-frees the env.
    pub(super) fn resource_clone_fn_for_payload(&self, payload_ty: &LirType, consuming: bool) -> Option<String> {
        if consuming { return None; }
        match payload_ty {
            LirType::Struct(sid) => {
                let sd = match self.module_structs.get(sid.0 as usize) {
                    Some(s) => s,
                    None => return None,
                };
                let name = sd.name.as_str();
                // Phase A residual #2 (2026-05-05): the previous shape
                // matched only on the runtime singleton's name (`GorgetClosure`,
                // `GorgetArray`, …) — load-bearing because, before residual #2,
                // mangled aliases like `Callable__GorgetClosure` and
                // `Vector__int64_t` shared the runtime's StructId so the name
                // came out as the runtime singleton's name. After residual #2,
                // `c_runtime_alias`-tagged aliases (Callable family) get their
                // OWN StructDef, so the name comes out as `Callable__GorgetClosure`
                // and the literal-name match misses. Read `c_runtime_alias`
                // first, falling back to the literal name match — this is the
                // typed-metadata-resolves-via-alias-chain shape.
                let resolved_name = sd.c_runtime_alias.as_deref().unwrap_or(name);
                match resolved_name {
                    "GorgetArray" => Some("gorget_array_clone".into()),
                    "GorgetMap" => Some("gorget_map_clone".into()),
                    "GorgetSet" => Some("gorget_set_clone".into()),
                    "GorgetString" => Some("gorget_string_clone".into()),
                    "GorgetClosure" => Some("gorget_closure_clone_to_owned".into()),
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

    /// Does this payload type need post-extract zeroing of its source slot to
    /// prevent shallow-copy double-free? Same predicate as
    /// `resource_clone_fn_for_payload` (read-mode, non-consuming) — any struct
    /// type that owns shared bytes through a thin pointer / handle. Used by
    /// `EnumFieldLoad`'s LIR auto-zero so the read site behaves as a Move at
    /// the value layer (the contract on `Instruction::EnumFieldLoad`).
    pub(super) fn payload_needs_post_extract_zero(&self, payload_ty: &LirType) -> bool {
        self.resource_clone_fn_for_payload(payload_ty, false).is_some()
    }

    /// Store a value into a struct field pointer, handling aggregate types correctly.
    /// For aggregates with known size > 0, stores to a temp slot then memcpy.
    /// For scalars or zero-size aggregates (opaque types), uses direct Store.
    #[allow(dead_code)]
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
            self.push_inst(bb, Inst::SlotStore {
                slot: temp_slot, value, is_move: true,
            });
            let temp_addr = self.lir_func.next_value();
            self.push_inst(bb, Inst::SlotAddr {
                dst: temp_addr, slot: temp_slot,
            });
            let sz_val = self.emit_size_of(bb, ty.clone());
            self.push_inst(bb, Inst::Memcpy {
                dst_ptr: field_ptr, src_ptr: temp_addr, size: sz_val,
            });
        } else {
            // Scalar or opaque type: direct store
            self.push_inst(bb, Inst::Store {
                ptr: field_ptr, value,
            });
        }
    }
}
