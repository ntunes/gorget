//! Higher-order-function inline expansion for Option/Result combinators.
//!
//! Lifted from `emit_call_extern.rs` to keep the CallExtern dispatch arm
//! tractable. This is a pure relocation — no logic changes; the generated
//! C is byte-identical before and after.
//!
//! Combinators handled: `map`, `filter`, `and_then`, `or_else`, `unwrap_err`
//! / `unwrap_error`, `map_err`, `or`, `flatten`, `unwrap_or_else`, `flat_map`.
//!
//! These are inlined per call site because the same GIR-mangled name (e.g.
//! `Option__int64_t__map`) can be used with different closure types
//! (same-type and cross-type map) and result-type spelling depends on the
//! specific LirExtern instance.

use super::*;

/// Try to emit the Option/Result combinator inline expansion.
///
/// Returns `true` if the call was a combinator and was emitted; the caller
/// should then `return` immediately. Returns `false` if the call is not a
/// combinator and the caller should continue with the normal dispatch.
pub(super) fn try_emit_option_result_combinator(
    out: &mut String,
    dst: &Option<ValueId>,
    name: &str,
    args: &[ValueId],
    ctx: &super::EmitContext,
    loc: &(String, u32, u32),
) -> bool {
    let module = ctx.module;
    let sn = ctx.sn;
    let val_types = ctx.val_types;
    let ptr_pointee = ctx.ptr_pointee;
    let v = |id: ValueId| -> String { format!("__v{}", id.0) };

    if let Some((type_prefix, method)) = parse_option_result_combinator(name) {
        if let Some(d) = dst {
            // Source enum type from name prefix.
            let src_ty = find_struct_c_name_by_prefix(type_prefix, module, sn)
                .unwrap_or_else(|| type_prefix.to_string());
            // Find the closure __call function by looking at the pointee type
            // of the closure arg (second arg is a SlotAddr of the closure struct).
            let call_fn = if args.len() > 1 {
                let closure_struct = ptr_pointee.get(args[1].0 as usize)
                    .and_then(|t| t.as_ref())
                    .map(|t| c_type_named(t, sn))
                    .or_else(|| {
                        val_types.get(args[1].0 as usize)
                            .and_then(|t| t.as_ref())
                            .filter(|t| matches!(t, LirType::Struct(_)))
                            .map(|t| c_type_named(t, sn))
                    });
                closure_struct.map(|n| find_closure_call_fn(module, &n, sn))
                    .unwrap_or_default()
            } else {
                String::new()
            };

            // Determine result type: read from the typed LirExtern field when available
            // (set by the LIR post-pass for cross-type maps), otherwise same as source.
            let result_ty = if let Some(sid) = module.externs.iter()
                .find(|e| e.name == *name)
                .and_then(|e| e.combinator_result_struct_id)
            {
                let idx = sid.0 as usize;
                sn.get(&(idx as u32)).cloned()
                    .or_else(|| module.structs.get(idx).map(|s| s.name.clone()))
                    .unwrap_or_else(|| src_ty.clone())
            } else if method == "flatten" {
                // flatten: result type is the payload type of the outer Option
                module.structs.iter().find(|s| s.name == type_prefix)
                    .and_then(|s| s.fields.get(1))
                    .map(|(_, t)| c_type_named(t, sn))
                    .unwrap_or_else(|| src_ty.clone())
            } else {
                // Non-map combinators: result type == source type
                // (and_then's closure already returns the full Option/Result)
                src_ty.clone()
            };

            let opt_ptr = v(args[0]);
            let closure_v = if args.len() > 1 { v(args[1]) } else { String::new() };
            let (ok_f, err_f) = enum_payload_fields(type_prefix, module);
            // Determine if closure params need & prefix (Ptr ABI for resource types)
            let comb_needs_ref = closure_params_need_ref(module, &call_fn);
            let cr = if comb_needs_ref.first().copied().unwrap_or(false) { "&" } else { "" };
            // Read typed `enum_kind` from the source struct (set at LIR
            // struct registration from GIR's `enum_category`) instead
            // of name-prefix matching at every site.
            let is_result = module.structs.iter()
                .find(|s| s.name == type_prefix)
                .map(|s| s.enum_kind == crate::lir::EnumKind::Result)
                .unwrap_or(false);

            match method {
                "map" => {
                    // For map result type, also look up the result struct's ok field
                    let result_ok = if is_result {
                        // Result map result type prefix may differ from source
                        let rp = name.rsplitn(2, "__").nth(1).unwrap_or(name);
                        enum_payload_fields(rp, module).0
                    } else {
                        ok_f.clone()
                    };
                    // For Result types, the Error branch must copy the error payload
                    let err_copy = if is_result {
                        format!(" __om_r.{err_f} = __om_src.{err_f};")
                    } else { String::new() };
                    if result_ty != src_ty {
                        // Cross-type map: result type differs from source. Use block + memcpy
                        write!(out, "{{ {result_ty} __om_r; {src_ty} __om_src = *({src_ty}*){opt_ptr}; \
                            if (__om_src.tag == 0) {{ __om_r.tag = 0; __om_r.{result_ok} = {call_fn}({closure_v}, {cr}__om_src.{ok_f}); }} \
                            else {{ __om_r.tag = 1;{err_copy} }} memcpy(&{dv}, &__om_r, sizeof({result_ty})); }}",
                            dv = v(*d)).unwrap();
                    } else {
                        write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; {result_ty} __om_r; \
                            if (__om_src.tag == 0) {{ __om_r.tag = 0; __om_r.{result_ok} = {call_fn}({closure_v}, {cr}__om_src.{ok_f}); }} \
                            else {{ __om_r.tag = 1;{err_copy} }} __om_r; }});",
                            v(*d)).unwrap();
                    }
                }
                "filter" => {
                    write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; {src_ty} __om_r; \
                        if (__om_src.tag == 0 && {call_fn}({closure_v}, {cr}__om_src.{ok_f})) {{ __om_r = __om_src; }} \
                        else {{ __om_r = ({src_ty}){{ .tag = 1 }}; }} __om_r; }});",
                        v(*d)).unwrap();
                }
                "and_then" => {
                    if is_result {
                        write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; {result_ty} __om_r; \
                            if (__om_src.tag == 0) {{ __om_r = {call_fn}({closure_v}, {cr}__om_src.{ok_f}); }} \
                            else {{ __om_r.tag = 1; __om_r.{err_f} = __om_src.{err_f}; }} __om_r; }});",
                            v(*d)).unwrap();
                    } else {
                        write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; {result_ty} __om_r; \
                            if (__om_src.tag == 0) {{ __om_r = {call_fn}({closure_v}, {cr}__om_src.{ok_f}); }} \
                            else {{ __om_r = ({result_ty}){{ .tag = 1 }}; }} __om_r; }});",
                            v(*d)).unwrap();
                    }
                }
                "or_else" => {
                    if is_result {
                        write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; {src_ty} __om_r; \
                            if (__om_src.tag == 0) {{ __om_r = __om_src; }} \
                            else {{ __om_r = {call_fn}({closure_v}, {cr}__om_src.{err_f}); }} __om_r; }});",
                            v(*d)).unwrap();
                    } else {
                        write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; {src_ty} __om_r; \
                            if (__om_src.tag == 0) {{ __om_r = __om_src; }} \
                            else {{ __om_r = {call_fn}({closure_v}); }} __om_r; }});",
                            v(*d)).unwrap();
                    }
                }
                "unwrap_err" | "unwrap_error" => {
                    // D11: `.unwrap_error()` on an `Ok` is `T_UnwrapErrorOnOk` — route
                    // through the registry (`gorget_trap_at`, exit 101 + real span),
                    // NOT a bare `abort()` (exit 134, off the normalized set).
                    write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; \
                        if (__om_src.tag != 1) {{ gorget_trap_at(\"{code}\", \"unwrap_error on Ok\", \"{f}\", {ln}, {cl}); }} \
                        __om_src.{err_f}; }});",
                        v(*d), code = crate::trap::TrapKind::UnwrapErrorOnOk.code(),
                        f = loc.0, ln = loc.1, cl = loc.2).unwrap();
                }
                "map_err" => {
                    // map_err may change the error type (cross-type error mapping)
                    let map_err_result = if !call_fn.is_empty() {
                        closure_call_return_type(module, &call_fn, sn)
                            .and_then(|ret_ty| {
                                let ok_ty = module.structs.iter().find(|s| s.name == type_prefix)
                                    .and_then(|s| s.fields.get(1))
                                    .map(|(_, t)| c_type_named(t, sn))?;
                                let target = format!("Result__{ok_ty}__{ret_ty}");
                                find_struct_c_name_by_prefix(&target, module, sn)
                            })
                    } else { None };
                    let me_result = map_err_result.as_deref().unwrap_or(&src_ty);
                    let me_err_f = if me_result != src_ty {
                        // Find the err field of the result type
                        module.structs.iter().find(|s| {
                            let c = sn.get(&(module.structs.iter().position(|x| std::ptr::eq(x, *s)).unwrap() as u32))
                                .cloned().unwrap_or_else(|| s.name.clone());
                            c == *me_result
                        }).and_then(|s| s.fields.get(2))
                            .map(|(n, _)| c_field_name(n))
                            .unwrap_or_else(|| err_f.clone())
                    } else { err_f.clone() };
                    if me_result != src_ty {
                        write!(out, "{{ {me_result} __om_r; {src_ty} __om_src = *({src_ty}*){opt_ptr}; \
                            if (__om_src.tag == 0) {{ __om_r.tag = 0; memcpy((char*)&__om_r + sizeof(int32_t), (char*)&__om_src + sizeof(int32_t), sizeof(__om_src.{ok_f})); }} \
                            else {{ __om_r.tag = 1; {{ __auto_type __me_val = {call_fn}({closure_v}, {cr}__om_src.{err_f}); memcpy(&__om_r.{me_err_f}, &__me_val, sizeof(__me_val)); }} }} memcpy(&{dv}, &__om_r, sizeof({me_result})); }}",
                            dv = v(*d)).unwrap();
                    } else {
                        write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; {src_ty} __om_r; \
                            if (__om_src.tag == 0) {{ __om_r = __om_src; }} \
                            else {{ __om_r.tag = 1; {{ __auto_type __me_val = {call_fn}({closure_v}, {cr}__om_src.{err_f}); memcpy(&__om_r.{err_f}, &__me_val, sizeof(__me_val)); }} }} __om_r; }});",
                            v(*d)).unwrap();
                    }
                }
                "or" => {
                    // or takes a second Option value (passed as pointer)
                    let other_v = if args.len() > 1 { v(args[1]) } else { String::new() };
                    let other_is_null = args.get(1).map_or(false, |a| ctx.is_null(*a));
                    if other_is_null {
                        // "or(None)" → if self is Some, return self, else return None
                        write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; \
                            (__om_src.tag == 0) ? __om_src : ({src_ty}){{ .tag = 1 }}; }});",
                            v(*d)).unwrap();
                    } else {
                        write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; \
                            (__om_src.tag == 0) ? __om_src : *({src_ty}*){other_v}; }});",
                            v(*d)).unwrap();
                    }
                }
                "flatten" => {
                    write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; \
                        (__om_src.tag == 0) ? __om_src.{ok_f} : ({result_ty}){{ .tag = 1 }}; }});",
                        v(*d)).unwrap();
                }
                "unwrap_or_else" => {
                    if is_result {
                        write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; \
                            (__om_src.tag == 0) ? __om_src.{ok_f} : {call_fn}({closure_v}, {cr}__om_src.{err_f}); }});",
                            v(*d)).unwrap();
                    } else {
                        write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; \
                            (__om_src.tag == 0) ? __om_src.{ok_f} : {call_fn}({closure_v}); }});",
                            v(*d)).unwrap();
                    }
                }
                "flat_map" => {
                    write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; {result_ty} __om_r; \
                        if (__om_src.tag == 0) {{ __om_r = {call_fn}({closure_v}, {cr}__om_src.{ok_f}); }} \
                        else {{ __om_r = ({result_ty}){{ .tag = 1 }}; }} __om_r; }});",
                        v(*d)).unwrap();
                }
                _ => {
                    write!(out, "/* TODO: combinator {name} */").unwrap();
                }
            }
        }
        return true;
    }
    false
}
