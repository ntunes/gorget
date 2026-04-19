//! CallExtern instruction emission -- the largest match arm in emit_inst().

use super::*;

/// Emit code for an `Inst::CallExtern` instruction.
pub(super) fn emit_call_extern(
    out: &mut String,
    inst: &Inst,
    dst: &Option<ValueId>,
    name: &str,
    args: &[ValueId],
    arg_abis: &[crate::ir::abi::AbiKind],
    ctx: &super::EmitContext,
) {
    let func = ctx.func;
    let module = ctx.module;
    let sn = ctx.sn;
    let val_types = ctx.val_types;
    let str_lit_vals = ctx.str_lit_vals;
    let null_vals = ctx.null_vals;
    let ptr_pointee = ctx.ptr_pointee;
    let func_addr_targets = ctx.func_addr_targets;
    let spawn_source_fn = ctx.spawn_source_fn;
    let v = |id: ValueId| -> String { format!("__v{}", id.0) };
    let _s = |id: SlotId| -> String { format!("__s{}", id.0) };

            let original_name = if let Inst::CallExtern { original_name, .. } = inst { original_name } else { &None };
            let _emit_args = args;
            // ── Closure dispatch is now Inst::CallClosure (not CallExtern) ──

            // ── Drop guards are now Inst::DropGuardOpen/Close (not CallExtern) ──

            // ── Builtin type casts ────────────────────────────────────────
            // `float`, `int`, `bool` are C keywords — can't emit as function calls.
            // Gorget's `float(x)`, `int(x)`, `bool(x)` are type casts. Emit inline C casts.
            // Special case: `int(string)` → gorget_str_ord (Unicode codepoint value).
            if args.len() == 1 {
                let arg_ty = val_types.get(args[0].0 as usize).and_then(|t| t.as_ref());
                let arg_is_str = matches!(arg_ty, Some(LirType::Struct(sid))
                    if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetString"));
                let arg_is_str_ptr = matches!(arg_ty, Some(LirType::Ptr))
                    && ptr_pointee.get(args[0].0 as usize)
                        .and_then(|p| p.as_ref())
                        .map_or(false, |p| is_str_struct(p, module));

                if name == "int" && (arg_is_str || arg_is_str_ptr) {
                    // int(string) → Unicode codepoint of first character
                    if let Some(d) = dst {
                        if arg_is_str {
                            write!(out, "{} = gorget_str_ord({});", v(*d), v(args[0])).unwrap();
                        } else {
                            write!(out, "{} = gorget_str_ord(*(Str*){});", v(*d), v(args[0])).unwrap();
                        }
                    }
                    return;
                }

                let cast_to = match name.as_ref() {
                    "float" => Some("double"),
                    "int"   => Some("int64_t"),
                    "bool"  => Some("bool"),
                    _ => None,
                };
                if let Some(c_type) = cast_to {
                    if let Some(d) = dst {
                        write!(out, "{} = ({c_type}){};", v(*d), v(args[0])).unwrap();
                    }
                    return;
                }
            }

            // ── Inline string codepoint helpers (synthetic GIR functions) ──
            if name == "gorget_utf8_codepoint_len_at" && args.len() == 2 {
                // gorget_utf8_codepoint_len_at(Str s, int64_t byte_pos) → int64_t
                // Str is a 32-byte struct — read data/len through the Str* pointer.
                if let Some(d) = dst {
                    let s = format!("((Str*){})", v(args[0]));
                    write!(out, "{} = gorget_utf8_codepoint_len(((const unsigned char*){s}->data)[{}]);",
                        v(*d), v(args[1])).unwrap();
                }
                return;
            }
            if name == "gorget_str_codepoint_at" && args.len() == 2 {
                // gorget_str_codepoint_at(Str s, int64_t byte_pos) → Str (owned copy)
                if let Some(d) = dst {
                    let s = format!("((Str*){})", v(args[0]));
                    let pos = v(args[1]);
                    write!(out, "{} = gorget_str_view_region((const char*){s}->data + {pos}, (size_t)gorget_utf8_codepoint_len(((const unsigned char*){s}->data)[{pos}]));",
                        v(*d)).unwrap();
                }
                return;
            }

            // ── Inline Option/Result helpers ────────────────────────────
            // These are pseudo-functions emitted by the GIR; they operate
            // on a pointer to an Option/Result struct.  The tag field is
            // always `int32_t` at offset 0; payload fields follow.
            if (name == "__option_is_some" || name == "__option_is_none"
                || name.ends_with("__is_some") || name.ends_with("__is_ok"))
                && !args.is_empty()
            {
                if let Some(d) = dst {
                    let op = if name.contains("is_some") || name.contains("is_ok") { "==" } else { "!=" };
                    write!(out, "{} = (*(int32_t*){}) {op} 0;", v(*d), v(args[0])).unwrap();
                }
                return;
            }
            if (name == "__option_is_none"
                || name.ends_with("__is_none") || name.ends_with("__is_err"))
                && !args.is_empty()
            {
                if let Some(d) = dst {
                    write!(out, "{} = (*(int32_t*){}) != 0;", v(*d), v(args[0])).unwrap();
                }
                return;
            }
            // ── Option/Result combinator inline expansion ──
            // map, filter, and_then, or_else are inlined at each call site
            // because the same GIR name (e.g., Option__int64_t__map) may be
            // used with different closure types (same-type and cross-type map).
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

                    // Determine result type for map: wraps the closure's return type.
                    // For same-type map (int→int), result == source.
                    // For cross-type map (int→str), find the Option__<ret> struct.
                    let result_ty = if method == "map" && !call_fn.is_empty() {
                        closure_call_return_type(module, &call_fn, sn)
                            .and_then(|ret_ty| {
                                if name.starts_with("Result__") {
                                    // For Result map, the error type stays the same.
                                    // Extract error type from source struct to build target name.
                                    let err_ty = module.structs.iter().find(|s| s.name == type_prefix)
                                        .and_then(|s| s.fields.get(2))
                                        .map(|(_, t)| c_type_named(t, sn));
                                    if let Some(err_c) = err_ty {
                                        let ret_m = type_name_to_monomorphized(&ret_ty);
                                        let err_m = type_name_to_monomorphized(&err_c);
                                        let target = format!("Result__{ret_m}__{err_m}");
                                        find_struct_c_name_by_prefix(&target, module, sn)
                                    } else {
                                        let ret_m = type_name_to_monomorphized(&ret_ty);
                                        let target = format!("Result__{ret_m}");
                                        find_struct_c_name_by_prefix(&target, module, sn)
                                    }
                                } else {
                                    let ret_m = type_name_to_monomorphized(&ret_ty);
                                    let target = format!("Option__{ret_m}");
                                    find_struct_c_name_by_prefix(&target, module, sn)
                                }
                            })
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

                    match method {
                        "map" => {
                            // For map result type, also look up the result struct's ok field
                            let result_ok = if name.starts_with("Result__") {
                                // Result map result type prefix may differ from source
                                let rp = name.rsplitn(2, "__").nth(1).unwrap_or(name);
                                enum_payload_fields(rp, module).0
                            } else {
                                ok_f.clone()
                            };
                            // For Result types, the Error branch must copy the error payload
                            let err_copy = if name.starts_with("Result__") {
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
                            if name.starts_with("Result__") {
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
                            if name.starts_with("Result__") {
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
                            write!(out, "{} = ({{ {src_ty} __om_src = *({src_ty}*){opt_ptr}; \
                                if (__om_src.tag != 1) {{ fprintf(stderr, \"unwrap_error on Ok\\n\"); abort(); }} \
                                __om_src.{err_f}; }});",
                                v(*d)).unwrap();
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
                            let other_is_null = args.get(1).map_or(false, |a| null_vals.get(a.0 as usize).copied().unwrap_or(false));
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
                            if name.starts_with("Result__") {
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
                return;
            }
            // ── Newtype constructors ────────────────────────────
            // If the extern name matches a struct name, emit a compound literal
            // instead of a function call: (StructType){ ._0 = arg }.
            if let Some(d) = dst {
                let is_newtype_ctor = module.structs.iter().enumerate().any(|(i, s)| {
                    let cname = sn.get(&(i as u32)).map(|s| s.as_str()).unwrap_or(&s.name);
                    // Match by original name (the extern uses the original struct name).
                    s.name == *name && s.fields.len() == 1
                        || cname == name && s.fields.len() == 1
                });
                if is_newtype_ctor && args.len() == 1 {
                    let struct_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                    let ty_name = struct_ty.map(|t| c_type_named(t, sn))
                        .unwrap_or_else(|| name.to_string());
                    let arg_val = &args[0];
                    write!(out, "{} = ({ty_name}){{ ._0 = {} }};", v(*d), v(*arg_val)).unwrap();
                    return;
                }
            }

            // ── Option/Result unwrap/unwrap_or/expect are now handled by the
            // LIR lowerer (Tier 2a in insts.rs).

            // ── Vector/Set/Dict constructor calls (type name without method) ──
            // Vector__int64_t(cap) → gorget_array_with_capacity(sizeof(int64_t), cap)
            // Vector__int64_t() → gorget_array_new(sizeof(int64_t))
            // Set__T(cap) / Dict__K__V(cap) → gorget_set_new(sizeof(T), cap) etc.
            if name.starts_with("Vector__") || name.starts_with("Set__") || name.starts_with("Dict__") || name.starts_with("HashMap__") || name.starts_with("HashSet__") {
                // Check if it's a constructor (name is just a type, no method suffix)
                let last_part = name.rsplit("__").next().unwrap_or("");
                if is_collection_type_constructor(last_part) {
                    emit_collection_constructor(out, name, dst, args, val_types, sn, module);
                    return;
                }
            }

            let is_stderr_print = name == "fprintf_stderr";
            let is_printf = name == "printf" || is_stderr_print
                || name == "gorget_string_format" || name == "gorget_string_format_alloc"
                || name == "snprintf" || name == "sprintf";
            let strip_ws_name: String;
            // Route user print() through capture layer; test runner uses raw printf.
            let emit_name = if name == "printf" { "__gorget_printf" }
            else if is_stderr_print { "fprintf" }
            else if args.len() == 1 {
                match name {
                    "gorget_str_strip" => { strip_ws_name = "gorget_str_trim".into(); strip_ws_name.as_str() }
                    "gorget_str_lstrip" => { strip_ws_name = "gorget_str_lstrip_ws".into(); strip_ws_name.as_str() }
                    "gorget_str_rstrip" => { strip_ws_name = "gorget_str_rstrip_ws".into(); strip_ws_name.as_str() }
                    _ => name
                }
            }
            else { name };

            // ── Out-parameter functions (image_load, audio_load, deflate_decompress) ──
            // These C runtime functions use void+out-param ABI but GIR calls them
            // as if they return a single Result/struct value. Rewrite to out-param form.
            if let Some(outparam_code) = try_emit_outparam_call_lir(
                emit_name, dst, args, val_types, str_lit_vals, sn, &module.structs,
            ) {
                write!(out, "{}", outparam_code).unwrap();
                return;
            }

            // ── File.create / File.open rewrite ──
            // gorget_file_create(path) → gorget_file_open(cstr, "w")
            // gorget_file_open(path) with 1 arg → gorget_file_open(cstr, "r")
            if emit_name == "gorget_file_create" && args.len() == 1 {
                let a = args[0];
                let is_str_lit = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
                let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                let is_gs = is_str_lit || matches!(arg_ty, Some(LirType::Struct(sid)) if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetString"));
                let path_expr = if is_gs {
                    format!("(const char*){}.data", v(a))
                } else {
                    format!("gorget_str_to_cstr({})", v(a))
                };
                if let Some(d) = dst {
                    write!(out, "{} = gorget_file_open({}, \"w\");", v(*d), path_expr).unwrap();
                } else {
                    write!(out, "gorget_file_open({}, \"w\");", path_expr).unwrap();
                }
                return;
            }
            // gorget_file_read_all(file_ptr) → wrap result in Result<GorgetString, Str> with UTF-8 validation
            if emit_name == "gorget_file_read_all" && args.len() == 1 {
                if let Some(d) = dst {
                    let dst_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                    // If destination is a Result struct, wrap with UTF-8 validation
                    if let Some(LirType::Struct(sid)) = dst_ty {
                        let sdef = module.structs.get(sid.0 as usize);
                        let is_result = sdef.map_or(false, |s| s.name.contains("Result"));
                        if is_result {
                            let result_c = c_type_named(&LirType::Struct(*sid), sn);
                            write!(out, "{d} = ({{ GorgetString __gs = gorget_file_read_all({a}); \
                                {result_c} __wr; if (gorget_utf8_validate((const char*)__gs.data, __gs.len)) {{ \
                                __wr.tag = 0; __wr.Ok_0 = __gs; }} else {{ \
                                gorget_string_free(&__gs); __wr.tag = 1; \
                                __wr.Error_0 = gorget_str_from_literal(\"invalid UTF-8\", 13); }} __wr; }});",
                                d = v(*d), a = v(args[0])).unwrap();
                            return;
                        }
                    }
                }
            }
            if emit_name == "gorget_file_open" && args.len() == 1 {
                let a = args[0];
                let is_str_lit = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
                let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                let is_gs = is_str_lit || matches!(arg_ty, Some(LirType::Struct(sid)) if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetString"));
                let path_expr = if is_gs {
                    format!("(const char*){}.data", v(a))
                } else {
                    format!("gorget_str_to_cstr({})", v(a))
                };
                if let Some(d) = dst {
                    write!(out, "{} = gorget_file_open({}, \"r\");", v(*d), path_expr).unwrap();
                } else {
                    write!(out, "gorget_file_open({}, \"r\");", path_expr).unwrap();
                }
                return;
            }

            // gorget_str_cat("", val) — str() coercion.
            // GIR represents str(int_val) as gorget_str_cat("", int_val).
            // Rewrite to gorget_int_to_str / gorget_float_to_str + wrap.
            if emit_name == "gorget_str_cat" && args.len() == 2 {
                let arg0_is_empty_str = str_lit_vals.get(args[0].0 as usize).copied().unwrap_or(false)
                    && func.blocks.iter().any(|blk| blk.insts.iter().any(|inst| {
                        matches!(inst, Inst::StrLit { dst, value } if *dst == args[0] && value.is_empty())
                    }));
                let arg1_ty = val_types.get(args[1].0 as usize).and_then(|t| t.as_ref());
                let is_arg1_int = matches!(arg1_ty, Some(LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64
                    | LirType::U8 | LirType::U16 | LirType::U32 | LirType::U64));
                let is_arg1_float = matches!(arg1_ty, Some(LirType::F32 | LirType::F64));
                let is_arg1_bool = matches!(arg1_ty, Some(LirType::Bool));
                if arg0_is_empty_str && (is_arg1_int || is_arg1_float || is_arg1_bool) {
                    let conv_fn = if is_arg1_int { "gorget_int_to_str" }
                        else if is_arg1_float { "gorget_float_to_str" }
                        else { "gorget_bool_to_str" };
                    if let Some(d) = dst {
                        // Conversion functions return Str (owned via gorget_string_adopt).
                        // Cast to the expected type — the LIR value may be an untyped slot
                        // pointer on some paths; clang treats the mismatch as a hard error.
                        let cast = if is_arg1_int { "(int64_t)" }
                            else if is_arg1_float { "(double)" }
                            else { "(int32_t)" };
                        write!(out, "{} = {}({}{});", v(*d), conv_fn, cast, v(args[1])).unwrap();
                    }
                    return;
                }
            }

            // gorget_str_push / gorget_str_push_line — dispatch by arg type.
            // The GIR emits a generic `gorget_str_push(ptr, i64)` but the
            // actual runtime has type-specific variants (push_int, push_float, push_bool).
            if (emit_name == "gorget_str_push" || emit_name == "gorget_str_push_line") && args.len() == 2 {
                let arg2_ty = val_types.get(args[1].0 as usize).and_then(|t| t.as_ref());
                let is_push_line = emit_name == "gorget_str_push_line";
                let variant = match arg2_ty {
                    Some(LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64
                         | LirType::U8 | LirType::U16 | LirType::U32 | LirType::U64) =>
                        if is_push_line { Some("gorget_string_push_line_int") }
                        else { Some("gorget_string_push_int") },
                    Some(LirType::F32 | LirType::F64) =>
                        if is_push_line { Some("gorget_string_push_line_float") }
                        else { Some("gorget_string_push_float") },
                    Some(LirType::Bool) =>
                        if is_push_line { Some("gorget_string_push_line_bool") }
                        else { Some("gorget_string_push_bool") },
                    _ => None, // Str — use gorget_str_push/push_line as-is
                };
                if let Some(typed_fn) = variant {
                    write!(out, "{typed_fn}({}, {});", v(args[0]), v(args[1])).unwrap();
                    return;
                }
            }

            // time() in C requires a NULL argument.
            if name == "time" && args.is_empty() {
                if let Some(d) = dst {
                    write!(out, "{} = ", v(*d)).unwrap();
                }
                write!(out, "time(NULL);").unwrap();
                return;
            }
            // ── Monomorphized parse methods ────────────────────────────
            // int8_t__parse, uint16_t__parse, double__parse, bool__parse, etc.
            // These return Option[T] but the C runtime has gorget_try_parse_int/float.
            if let Some(d) = dst {
                let dst_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                if let Some(LirType::Struct(sid)) = dst_ty {
                    let sdef = &module.structs[sid.0 as usize];
                    if sdef.name.starts_with("Option__") {
                        let opt_c = c_type_named(dst_ty.unwrap(), sn);
                        let payload_fname = sdef.fields.get(1)
                            .map(|(n, _)| c_field_name(n)).unwrap_or_else(|| "Some_0".to_string());
                        // Integer parse: int8_t__parse, uint16_t__parse, int64_t__parse, etc.
                        let is_int_parse = name.ends_with("__parse")
                            && (name.starts_with("int") || name.starts_with("uint"));
                        // Float parse: double__parse, float__parse
                        let is_float_parse = name == "double__parse" || name == "float__parse";
                        // Bool parse
                        let is_bool_parse = name == "bool__parse";
                        // Helper: coerce arg to `const char*` depending on whether it's Str, Ptr-to-Str, or already cstr
                        let coerce_arg_to_cstr = |a: ValueId| -> String {
                            let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                            let arg_is_str = arg_ty.map_or(false, |t| is_str_struct(t, module));
                            let is_ptr_to_str = is_str_ptr_opt(arg_ty, module)
                                || (matches!(arg_ty, Some(LirType::Ptr)) && {
                                    ptr_pointee.get(a.0 as usize).and_then(|t| t.as_ref()).map_or(false, |t| {
                                        is_str_struct(t, module)
                                    })
                                });
                            let is_str_lit = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
                            if is_str_lit || arg_is_str {
                                // 32-byte Str struct — extract .data for const char*
                                format!("(const char*){}.data", v(a))
                            } else if false { // arg_is_str handled above
                                format!("gorget_str_to_cstr({})", v(a))
                            } else if is_ptr_to_str {
                                format!("gorget_str_to_cstr(*(Str*){})", v(a))
                            } else {
                                // Fallback: assume already const char*
                                format!("(const char*){}", v(a))
                            }
                        };
                        if is_int_parse {
                            let cast_type = if name.contains("uint8") { "uint8_t" }
                                else if name.contains("uint16") { "uint16_t" }
                                else if name.contains("uint32") { "uint32_t" }
                                else if name.contains("uint64") { "uint64_t" }
                                else if name.contains("int8") { "int8_t" }
                                else if name.contains("int16") { "int16_t" }
                                else if name.contains("int32") { "int32_t" }
                                else { "int64_t" };
                            let range_check = match cast_type {
                                "int8_t" => " && __pr.value >= -128 && __pr.value <= 127",
                                "int16_t" => " && __pr.value >= -32768 && __pr.value <= 32767",
                                "int32_t" => " && __pr.value >= -2147483648LL && __pr.value <= 2147483647LL",
                                "uint8_t" => " && __pr.value >= 0 && __pr.value <= 255",
                                "uint16_t" => " && __pr.value >= 0 && __pr.value <= 65535",
                                "uint32_t" => " && __pr.value >= 0 && __pr.value <= 4294967295LL",
                                _ => "", // int64_t / uint64_t: full-width, no check needed
                            };
                            let cstr_arg = if !args.is_empty() { coerce_arg_to_cstr(args[0]) } else { "NULL".to_string() };
                            write!(out, "{dv} = ({{ const char* __pa = {cstr_arg}; GorgetParseIntResult __pr = gorget_try_parse_int(__pa, strlen(__pa)); \
                                {opt_c} __opt; if (__pr.ok{range_check}) {{ __opt.tag = 0; __opt.{payload_fname} = ({cast_type})__pr.value; }} \
                                else {{ __opt.tag = 1; }} __opt; }});",
                                dv = v(*d)).unwrap();
                            return;
                        } else if is_float_parse {
                            let cstr_arg = if !args.is_empty() { coerce_arg_to_cstr(args[0]) } else { "NULL".to_string() };
                            write!(out, "{dv} = ({{ const char* __pa = {cstr_arg}; GorgetParseFloatResult __pr = gorget_try_parse_float(__pa, strlen(__pa)); \
                                {opt_c} __opt; if (__pr.ok) {{ __opt.tag = 0; __opt.{payload_fname} = (double)__pr.value; }} \
                                else {{ __opt.tag = 1; }} __opt; }});",
                                dv = v(*d)).unwrap();
                            return;
                        } else if is_bool_parse {
                            let cstr_arg = if !args.is_empty() { coerce_arg_to_cstr(args[0]) } else { "NULL".to_string() };
                            write!(out, "{dv} = ({{ const char* __ps = {cstr_arg}; size_t __pl = strlen(__ps); \
                                {opt_c} __opt; \
                                if (__pl == 4 && memcmp(__ps, \"true\", 4) == 0) {{ __opt.tag = 0; __opt.{payload_fname} = true; }} \
                                else if (__pl == 5 && memcmp(__ps, \"false\", 5) == 0) {{ __opt.tag = 0; __opt.{payload_fname} = false; }} \
                                else {{ __opt.tag = 1; }} __opt; }});",
                                dv = v(*d)).unwrap();
                            return;
                        }
                    }
                }
            }

            // For fprintf_stderr, skip the first arg (Null placeholder).
            let emit_args: &[ValueId] = if is_stderr_print && !args.is_empty() {
                &args[1..]
            } else {
                args
            };
            let ext_decl = module.externs.iter().find(|e| &e.name == name);
            // For spawn/inline helpers, the LIR extern declares params as
            // (ptr, i64, ...) but the generated C helper uses real types (Str etc).
            // Look up the original function to get correct types for coercion.
            let fn_params_owned: Option<Vec<LirType>> = if emit_name.starts_with("__gorget_spawn_") {
                let lookup_name = emit_name.strip_prefix("__gorget_spawn_").unwrap();
                module.functions.iter()
                    .find(|f| f.name == lookup_name)
                    .map(|f| f.params.clone())
            } else { None };
            // For Dict/Set inline methods, String ptr methods, and spawn helpers,
            // the LIR extern params use ptr for Str args. Set a flag so we can
            // coerce string literal args to Str at the call site.
            let force_str_coerce = parse_dict_higher_order(name).is_some()
                || parse_set_higher_order(name).is_some();
            // For trait box method calls, determine which specific arg positions need Str coercion.
            let trait_str_arg_positions = trait_box_str_arg_positions(module, name);
            let ext_params: Option<&[LirType]> = if fn_params_owned.is_some() {
                fn_params_owned.as_deref()
            } else if let Some(e) = &ext_decl {
                Some(e.params.as_slice())
            } else {
                None
            };
            let ret_is_void = ext_decl.as_ref().map_or(false, |e| matches!(e.return_type, LirType::Void));

            // ── last_error → Result wrapping is now handled by the LIR lowerer (lifts.rs).

            // ── Sentinel scalar → Option wrapping is now handled by the LIR lowerer.

            // ── Collection void* return dereference ──────────────────
            // Functions like gorget_array_get return void* — dereference
            // to the concrete element type expected by the destination.
            let void_ret = is_collection_void_return(emit_name) || needs_opt_wrapping(emit_name);
            let dst_needs_deref = void_ret && dst.map_or(false, |d| {
                let ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                ty.map_or(false, |t| !matches!(t, LirType::Ptr))
            });

            // ── Collection void* → Option wrapping is now handled by the LIR lowerer.

            // ── __gorget_spawn_* → Task__T handling ──────────────
            // Spawn helpers now return Task__T. When LIR destination is a Task struct,
            // simple assignment works. When LIR destination is void*, extract .__task.
            let is_spawn = emit_name.starts_with("__gorget_spawn_");
            let dst_is_task_struct = is_spawn && dst.map_or(false, |d| {
                matches!(val_types.get(d.0 as usize).and_then(|t| t.as_ref()), Some(LirType::Struct(sid)) if {
                    module.structs.get(sid.0 as usize).map_or(false, |s| s.name.starts_with("Task__"))
                })
            });
            // Non-struct Task destination (void*) — extract .__task from returned struct.
            let dst_is_spawn_ptr = is_spawn && !dst_is_task_struct && dst.is_some();

            // ── Inline higher-order collection methods ─────────────
            // Vector/Dict/Set filter/map/fold/etc. must be inlined at each call
            // site to use the correct __Closure_N__call function for that site.
            // Helper: resolve __Closure_N__call for a closure arg at this specific call site.
            let resolve_call_fn = |closure_arg: Option<ValueId>| -> String {
                closure_arg.and_then(|ca| {
                    let try_from_val = val_types.get(ca.0 as usize).and_then(|t| t.as_ref()).and_then(|ty| {
                        if let LirType::Struct(sid) = ty {
                            let sdef = &module.structs[sid.0 as usize];
                            let call_name = format!("{}__call", sdef.name);
                            if module.functions.iter().any(|f| f.name == call_name) { Some(call_name) } else { None }
                        } else { None }
                    });
                    try_from_val.or_else(|| {
                        ptr_pointee.get(ca.0 as usize).and_then(|t| t.as_ref()).and_then(|ty| {
                            if let LirType::Struct(sid) = ty {
                                let sdef = &module.structs[sid.0 as usize];
                                let call_name = format!("{}__call", sdef.name);
                                if module.functions.iter().any(|f| f.name == call_name) { Some(call_name) } else { None }
                            } else { None }
                        })
                    }).or_else(|| {
                        // Check if the arg is a FuncAddr (named function as closure).
                        // Use the __adapt_* wrapper which follows the closure calling convention.
                        func_addr_targets.get(ca.0 as usize).and_then(|t| *t).map(|fid| {
                            format!("__adapt_{}", c_func_name(&module.functions[fid.0 as usize].name))
                        })
                    })
                }).unwrap_or_else(|| find_closure_call_fn(module, "void*", sn))
            };

            if let Some((_elem_ty, method)) = parse_vector_higher_order(emit_name) {
                // All closure-taking Vector HOFs (each/any/all/map/filter/fold/
                // reduce/count/flat_map/find/find_index) are lowered upstream
                // via `Inst::HofExpand`. Plain `sort`/`sorted`/`unique` route
                // to typed runtime stubs via `map_monomorphized_to_runtime`.
                // Only qsort-trampoline closures (`sort_by*`, `sorted_by*`)
                // and delegated helpers (`windows`, `chunks`) remain inline.
                if (dst.is_some() || method == "sort_by" || method == "sort_by_key")
                    && matches!(method,
                        "sort_by" | "sorted_by" | "sort_by_key" | "sorted_by_key"
                        | "windows" | "chunks"
                    )
                {
                    let d_opt = dst;
                    let dv = d_opt.map(|d| format!("__v{}", d.0)).unwrap_or_default();
                    let arr_arg = v(emit_args[0]);
                    let closure_arg = emit_args.last().copied();
                    let fn_arg = closure_arg.map(|ca| v(ca)).unwrap_or_default();
                    let closure_is_ptr = closure_arg.map_or(false, |ca| matches!(val_types.get(ca.0 as usize).and_then(|t| t.as_ref()), Some(LirType::Ptr)));
                    match method {
                        "sort_by" | "sorted_by" | "sort_by_key" | "sorted_by_key" => {
                            // Delegate to the helper emitted by emit_vector_helper.
                            // The helper takes the closure as const void* and stores a
                            // pointer in TLS (save/restored across calls). Pass either
                            // the closure pointer directly or the address of the struct.
                            let closure_pass = if closure_is_ptr {
                                fn_arg.clone()
                            } else {
                                format!("&{fn_arg}")
                            };
                            let is_mutating = method == "sort_by" || method == "sort_by_key";
                            if is_mutating {
                                write!(out, "{emit_name}({arr_arg}, {closure_pass});").unwrap();
                            } else {
                                write!(out, "{dv} = {emit_name}({arr_arg}, {closure_pass});").unwrap();
                            }
                        }
                        "windows" | "chunks" => {
                            // windows(n) / chunks(n) — non-closure arg (int n).
                            // fn_arg is the int `n` value already. Delegate to helper.
                            write!(out, "{dv} = {emit_name}({arr_arg}, {fn_arg});").unwrap();
                        }
                        _ => unreachable!(),
                    }
                    return;
                }
            }

            // ── Inline Dict higher-order methods ─────────────
            if let Some((key_ty, val_ty, method)) = parse_dict_higher_order(emit_name) {
                let has_closure = matches!(method, "filter" | "fold" | "each" | "any" | "all");
                if has_closure && (dst.is_some() || method == "each") {
                    let d_opt = dst;
                    let orig_to_c2: HashMap<String, String> = module.structs.iter().enumerate()
                        .map(|(i, def)| (def.name.clone(), sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"))))
                        .collect();
                    let key_c = elem_type_to_c_with_sn(key_ty, &orig_to_c2);
                    let val_c = elem_type_to_c_with_sn(val_ty, &orig_to_c2);
                    let closure_arg = emit_args.last().copied();
                    let call_fn = resolve_call_fn(closure_arg);
                    let dv = d_opt.map(|d| format!("__v{}", d.0)).unwrap_or_default();
                    let map_arg = v(emit_args[0]);
                    let fn_arg = closure_arg.map(|ca| v(ca)).unwrap_or_default();
                    let dict_closure_is_ptr = closure_arg.map_or(false, |ca| matches!(val_types.get(ca.0 as usize).and_then(|t| t.as_ref()), Some(LirType::Ptr)));
                    let dict_fn_ref = if dict_closure_is_ptr { fn_arg.clone() } else { format!("&{fn_arg}") };
                    let dict_needs_ref = closure_params_need_ref(module, &call_fn);
                    let dkr = if dict_needs_ref.first().copied().unwrap_or(false) { "&" } else { "" };
                    let dvr = if dict_needs_ref.get(1).copied().unwrap_or(false) { "&" } else { "" };
                    let is_dict = emit_name.starts_with("Dict__");
                    let ctor_fn = if key_c == "Str" {
                        if is_dict { "gorget_dict_new_str" } else { "gorget_map_new_str" }
                    } else {
                        if is_dict { "gorget_dict_new" } else { "gorget_map_new" }
                    };
                    let ctor_args = if key_c == "Str" { format!("sizeof({val_c})") } else { format!("sizeof({key_c}), sizeof({val_c})") };
                    match method {
                        "filter" => {
                            // __key/__val are shallow copies of slots in __src — use
                            // put_cloned so __result's inserted slot holds independent
                            // key/value copies instead of aliasing __src's buffers.
                            let val_setup = if val_c == "Str" || val_c == "GorgetString" {
                                " __result.val_drop = (__gorget_drop_fn)gorget_string_free; \
                                  __result.val_clone = (__gorget_drop_fn)gorget_string_clone_inplace; \
                                  __result.val_materialize = (__gorget_drop_fn)gorget_string_materialize_inplace;"
                            } else { "" };
                            write!(out, "{dv} = ({{ GorgetMap __src = *(GorgetMap*){map_arg}; \
                                GorgetMap __result = {ctor_fn}({ctor_args});{val_setup} \
                                for (size_t __i = 0; __i < __src.cap; __i++) {{ \
                                if (__src.states[__i] != 1) continue; \
                                {key_c} __key = *({key_c}*)((char*)__src.keys + __i * __src.key_size); \
                                {val_c} __val = *({val_c}*)((char*)__src.values + __i * __src.val_size); \
                                if ({call_fn}({dict_fn_ref}, {dkr}__key, {dvr}__val)) gorget_map_put_cloned(&__result, &__key, &__val); \
                                }} __result; }});").unwrap();
                        }
                        "fold" if emit_args.len() >= 3 => {
                            let acc_arg = v(emit_args[1]);
                            let fn_a = v(emit_args[2]);
                            let dict_fold_is_ptr = matches!(val_types.get(emit_args[2].0 as usize).and_then(|t| t.as_ref()), Some(LirType::Ptr));
                            let dict_fn_a_ref = if dict_fold_is_ptr { fn_a.clone() } else { format!("&{fn_a}") };
                            let call_fn2 = resolve_call_fn(Some(emit_args[2]));
                            let dfold_needs_ref = closure_params_need_ref(module, &call_fn2);
                            let dfar = if dfold_needs_ref.first().copied().unwrap_or(false) { "&" } else { "" };
                            let dfkr = if dfold_needs_ref.get(1).copied().unwrap_or(false) { "&" } else { "" };
                            let dfvr = if dfold_needs_ref.get(2).copied().unwrap_or(false) { "&" } else { "" };
                            write!(out, "{dv} = ({{ GorgetMap __src = *(GorgetMap*){map_arg}; \
                                __typeof__({acc_arg}) __acc = {acc_arg}; \
                                for (size_t __i = 0; __i < __src.cap; __i++) {{ \
                                if (__src.states[__i] != 1) continue; \
                                {key_c} __key = *({key_c}*)((char*)__src.keys + __i * __src.key_size); \
                                {val_c} __val = *({val_c}*)((char*)__src.values + __i * __src.val_size); \
                                __acc = {call_fn2}({dict_fn_a_ref}, {dfar}__acc, {dfkr}__key, {dfvr}__val); \
                                }} __acc; }});").unwrap();
                        }
                        "each" => {
                            write!(out, "{{ GorgetMap __src = *(GorgetMap*){map_arg}; \
                                for (size_t __i = 0; __i < __src.cap; __i++) {{ \
                                if (__src.states[__i] != 1) continue; \
                                {key_c} __key = *({key_c}*)((char*)__src.keys + __i * __src.key_size); \
                                {val_c} __val = *({val_c}*)((char*)__src.values + __i * __src.val_size); \
                                {call_fn}({dict_fn_ref}, {dkr}__key, {dvr}__val); \
                                }} }}").unwrap();
                        }
                        "any" => {
                            write!(out, "{dv} = ({{ GorgetMap __src = *(GorgetMap*){map_arg}; \
                                bool __any_r = false; \
                                for (size_t __i = 0; __i < __src.cap; __i++) {{ \
                                if (__src.states[__i] != 1) continue; \
                                {key_c} __key = *({key_c}*)((char*)__src.keys + __i * __src.key_size); \
                                {val_c} __val = *({val_c}*)((char*)__src.values + __i * __src.val_size); \
                                if ({call_fn}({dict_fn_ref}, {dkr}__key, {dvr}__val)) {{ __any_r = true; break; }} \
                                }} __any_r; }});").unwrap();
                        }
                        "all" => {
                            write!(out, "{dv} = ({{ GorgetMap __src = *(GorgetMap*){map_arg}; \
                                bool __all_r = true; \
                                for (size_t __i = 0; __i < __src.cap; __i++) {{ \
                                if (__src.states[__i] != 1) continue; \
                                {key_c} __key = *({key_c}*)((char*)__src.keys + __i * __src.key_size); \
                                {val_c} __val = *({val_c}*)((char*)__src.values + __i * __src.val_size); \
                                if (!{call_fn}({dict_fn_ref}, {dkr}__key, {dvr}__val)) {{ __all_r = false; break; }} \
                                }} __all_r; }});").unwrap();
                        }
                        _ => {
                            write!(out, "{}({map_arg}, {fn_arg});", emit_name).unwrap();
                        }
                    }
                    return;
                }
            }

            // ── Inline Set higher-order methods ─────────────
            if let Some((elem_ty, method)) = parse_set_higher_order(emit_name) {
                let has_closure = matches!(method, "filter" | "fold" | "each" | "any" | "all");
                if has_closure && (dst.is_some() || method == "each") {
                    let d_opt = dst;
                    let orig_to_c2: HashMap<String, String> = module.structs.iter().enumerate()
                        .map(|(i, def)| (def.name.clone(), sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"))))
                        .collect();
                    let elem_c = elem_type_to_c_with_sn(elem_ty, &orig_to_c2);
                    let closure_arg = emit_args.last().copied();
                    let call_fn = resolve_call_fn(closure_arg);
                    let dv = d_opt.map(|d| format!("__v{}", d.0)).unwrap_or_default();
                    let set_arg = v(emit_args[0]);
                    let fn_arg = closure_arg.map(|ca| v(ca)).unwrap_or_default();
                    let set_closure_is_ptr = closure_arg.map_or(false, |ca| matches!(val_types.get(ca.0 as usize).and_then(|t| t.as_ref()), Some(LirType::Ptr)));
                    let set_fn_ref = if set_closure_is_ptr { fn_arg.clone() } else { format!("&{fn_arg}") };
                    let set_needs_ref = closure_params_need_ref(module, &call_fn);
                    let ser = if set_needs_ref.first().copied().unwrap_or(false) { "&" } else { "" };
                    let set_is_ordered = !emit_name.starts_with("HashSet__");
                    let set_ctor = if set_is_ordered { "gorget_ordered_set_new" } else { "gorget_set_new" };
                    let (set_iter_var, set_iter_cond, set_idx_decl) = if set_is_ordered {
                        ("__j", "__src.order_len", "size_t __i = __src.order[__j]; if (__src.states[__i] != 1) continue; ")
                    } else {
                        ("__i", "__src.cap", "if (__src.states[__i] != 1) continue; ")
                    };
                    match method {
                        "filter" => {
                            write!(out, "{dv} = ({{ GorgetSet __src = *(GorgetSet*){set_arg}; \
                                GorgetSet __result = {set_ctor}(sizeof({elem_c})); \
                                for (size_t {set_iter_var} = 0; {set_iter_var} < {set_iter_cond}; {set_iter_var}++) {{ \
                                {set_idx_decl}\
                                {elem_c} __elem = *({elem_c}*)((char*)__src.keys + __i * __src.key_size); \
                                if ({call_fn}({set_fn_ref}, {ser}__elem)) gorget_set_add(&__result, &__elem); \
                                }} __result; }});").unwrap();
                        }
                        "fold" if emit_args.len() >= 3 => {
                            let acc_arg = v(emit_args[1]);
                            let fn_a = v(emit_args[2]);
                            let fold_closure_is_ptr2 = matches!(val_types.get(emit_args[2].0 as usize).and_then(|t| t.as_ref()), Some(LirType::Ptr));
                            let fn_a_ref2 = if fold_closure_is_ptr2 { fn_a.clone() } else { format!("&{fn_a}") };
                            let call_fn2 = resolve_call_fn(Some(emit_args[2]));
                            let sfold_needs_ref = closure_params_need_ref(module, &call_fn2);
                            let sfar = if sfold_needs_ref.first().copied().unwrap_or(false) { "&" } else { "" };
                            let sfer = if sfold_needs_ref.get(1).copied().unwrap_or(false) { "&" } else { "" };
                            write!(out, "{dv} = ({{ GorgetSet __src = *(GorgetSet*){set_arg}; \
                                __typeof__({acc_arg}) __acc = {acc_arg}; \
                                for (size_t {set_iter_var} = 0; {set_iter_var} < {set_iter_cond}; {set_iter_var}++) {{ \
                                {set_idx_decl}\
                                {elem_c} __elem = *({elem_c}*)((char*)__src.keys + __i * __src.key_size); \
                                __acc = {call_fn2}({fn_a_ref2}, {sfar}__acc, {sfer}__elem); \
                                }} __acc; }});").unwrap();
                        }
                        "each" => {
                            write!(out, "{{ GorgetSet __src = *(GorgetSet*){set_arg}; \
                                for (size_t {set_iter_var} = 0; {set_iter_var} < {set_iter_cond}; {set_iter_var}++) {{ \
                                {set_idx_decl}\
                                {elem_c} __elem = *({elem_c}*)((char*)__src.keys + __i * __src.key_size); \
                                {call_fn}({set_fn_ref}, {ser}__elem); \
                                }} }}").unwrap();
                        }
                        "any" => {
                            write!(out, "{dv} = ({{ GorgetSet __src = *(GorgetSet*){set_arg}; \
                                bool __any_r = false; \
                                for (size_t {set_iter_var} = 0; {set_iter_var} < {set_iter_cond}; {set_iter_var}++) {{ \
                                {set_idx_decl}\
                                {elem_c} __elem = *({elem_c}*)((char*)__src.keys + __i * __src.key_size); \
                                if ({call_fn}({set_fn_ref}, {ser}__elem)) {{ __any_r = true; break; }} \
                                }} __any_r; }});").unwrap();
                        }
                        "all" => {
                            write!(out, "{dv} = ({{ GorgetSet __src = *(GorgetSet*){set_arg}; \
                                bool __all_r = true; \
                                for (size_t {set_iter_var} = 0; {set_iter_var} < {set_iter_cond}; {set_iter_var}++) {{ \
                                {set_idx_decl}\
                                {elem_c} __elem = *({elem_c}*)((char*)__src.keys + __i * __src.key_size); \
                                if (!{call_fn}({set_fn_ref}, {ser}__elem)) {{ __all_r = false; break; }} \
                                }} __all_r; }});").unwrap();
                        }
                        _ => {
                            write!(out, "{}({set_arg}, {fn_arg});", emit_name).unwrap();
                        }
                    }
                    return;
                }
            }

            // ── Nullable cstr → Option wrapping is now handled by the LIR lowerer.

            // ── Channel__T__recv_timeout → Option wrapping ──
            // The wrapper returns the raw value, but the GIR expects Option<T>.
            // Wrap the call: call gorget_channel_recv_timeout directly and check return code.
            if emit_name.starts_with("Channel__") && emit_name.ends_with("__recv_timeout") {
                if let Some(d) = dst {
                    let dst_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                    let is_option_struct = matches!(dst_ty, Some(LirType::Struct(sid)) if {
                        module.structs.get(sid.0 as usize).map_or(false, |s| s.name.contains("Option"))
                    });
                    if is_option_struct {
                        let opt_ty = c_type_named(dst_ty.unwrap(), sn);
                        // Find the payload type (field 1 of Option struct)
                        let payload_ty = if let Some(LirType::Struct(sid)) = dst_ty {
                            module.structs.get(sid.0 as usize)
                                .and_then(|s| s.fields.get(1))
                                .map(|(_, t)| c_type_named(t, sn))
                        } else { None };
                        let val_c = payload_ty.unwrap_or_else(|| "int64_t".to_string());
                        // args: [channel_ptr, timeout_ms]
                        let ch_arg = v(emit_args[0]);
                        let ms_arg = v(emit_args[1]);
                        write!(out, "{dv} = ({{ {val_c} __val = {{0}}; int __rc = gorget_channel_recv_timeout(*(GorgetChannel**){ch_arg}, &__val, {ms_arg}); \
                            {opt_ty} __opt; if (__rc != 0) {{ __opt.tag = 0; __opt.Some_0 = __val; }} else {{ __opt.tag = 1; }} __opt; }});",
                            dv = v(*d)).unwrap();
                        return;
                    }
                }
            }

            // ── Sentinel-based Option wrapping (e.g. gorget_regex_find → Option<Match>) ──
            // gorget_regex_find returns GorgetRegexMatch; start==-1 means no match → None.
            if is_sentinel_option_fn(emit_name) {
                if let Some(d) = dst {
                    let dst_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                    let is_option_struct = matches!(dst_ty, Some(LirType::Struct(sid)) if {
                        module.structs.get(sid.0 as usize).map_or(false, |s| s.name.contains("Option"))
                    });
                    if is_option_struct {
                        let opt_ty = c_type_named(dst_ty.unwrap(), sn);
                        write!(out, "{} = ({{ GorgetRegexMatch __raw = {}(", v(*d), emit_name).unwrap();
                        for (i, a) in emit_args.iter().enumerate() {
                            if i > 0 { write!(out, ", ").unwrap(); }
                            let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                            let is_str_lit = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
                            // ABI-driven marshalling: explicit tag or whitelist-derived.
                            {
                                let inst_abi = arg_abis.get(i).copied().unwrap_or(crate::ir::abi::AbiKind::Auto);
                                if inst_abi != crate::ir::abi::AbiKind::Auto {
                                    if emit_abi_arg(out, &v(*a), inst_abi, arg_ty, is_str_lit) {
                                        continue;
                                    }
                                }
                                let abi = resolve_param_abi(ext_decl, emit_name, i);
                                if emit_abi_arg(out, &v(*a), abi, arg_ty, is_str_lit) {
                                    continue;
                                }
                            }
                            // CStr cases handled by resolve_param_abi above; fallback for non-CStr.
                            let ext_param = ext_params.and_then(|p| p.get(i));
                            emit_coerced_arg(out, a, ext_param, val_types, str_lit_vals, sn);
                        }
                        write!(out, "); {opt_ty} __opt; if (__raw.start != -1) {{ __opt.tag = 0; __opt.Some_0 = __raw; }} else {{ __opt.tag = 1; }} __opt; }});").unwrap();
                        return;
                    }
                }
            }

            // ── Nullable pointer → Option wrapping (e.g. Weak__T__upgrade) ──
            if is_nullable_ptr_fn(emit_name) {
                if let Some(d) = dst {
                    let dst_ty = val_types.get(d.0 as usize).and_then(|t| t.as_ref());
                    let is_option_struct = matches!(dst_ty, Some(LirType::Struct(sid)) if {
                        module.structs.get(sid.0 as usize).map_or(false, |s| s.name.contains("Option"))
                    });
                    if is_option_struct {
                        let opt_ty = c_type_named(dst_ty.unwrap(), sn);
                        // Find the Some payload type (field 1)
                        let inner_ty = if let Some(LirType::Struct(sid)) = dst_ty {
                            module.structs.get(sid.0 as usize)
                                .and_then(|s| s.fields.get(1))
                                .map(|(_, t)| c_type_named(t, sn))
                        } else { None };
                        let inner_c = inner_ty.unwrap_or_else(|| "void*".to_string());
                        write!(out, "{} = ({{ {inner_c} __raw = {}(", v(*d), emit_name).unwrap();
                        for (i, a) in emit_args.iter().enumerate() {
                            if i > 0 { write!(out, ", ").unwrap(); }
                            let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                            let is_str_lit = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
                            // ABI-driven marshalling.
                            {
                                let inst_abi = arg_abis.get(i).copied().unwrap_or(crate::ir::abi::AbiKind::Auto);
                                if inst_abi != crate::ir::abi::AbiKind::Auto {
                                    if emit_abi_arg(out, &v(*a), inst_abi, arg_ty, is_str_lit) {
                                        continue;
                                    }
                                }
                                let abi = resolve_param_abi(ext_decl, emit_name, i);
                                if emit_abi_arg(out, &v(*a), abi, arg_ty, is_str_lit) {
                                    continue;
                                }
                            }
                            let ext_param = ext_params.and_then(|p| p.get(i));
                            emit_coerced_arg(out, a, ext_param, val_types, str_lit_vals, sn);
                        }
                        write!(out, "); {opt_ty} __opt; if (__raw) {{ __opt.tag = 0; __opt.Some_0 = __raw; }} else {{ __opt.tag = 1; }} __opt; }});").unwrap();
                        return;
                    }
                }
            }

            // ── gorget_task_group_submit(group, task) ──
            // The macro expects task to be a Task struct (with .__task, .__drop),
            // but LIR may pass a pointer (void*) containing only .__task extracted
            // from a spawn result. In that case, reconstruct the full Task__void
            // struct with the correct __drop function from the spawn source.
            if emit_name == "gorget_task_group_submit" && emit_args.len() >= 2 {
                let task_arg = emit_args[1];
                let task_ty = val_types.get(task_arg.0 as usize).and_then(|t| t.as_ref());
                let is_ptr = matches!(task_ty, Some(LirType::Ptr) | None);
                let group_arg = v(emit_args[0]);
                let task_v = v(task_arg);
                if is_ptr {
                    // Check if this void* was produced by a spawn (.__task extraction).
                    // If so, reconstruct the full Task__void struct with the correct __drop fn.
                    if let Some(Some(spawn_fn)) = spawn_source_fn.get(task_arg.0 as usize) {
                        let drop_fn = format!("__spawn_drop_{spawn_fn}");
                        // Use gorget_task_group_submit_raw directly to avoid macro comma issues
                        // with compound literals.
                        write!(out, "gorget_task_group_submit_raw(*(TaskGroup*){group_arg}, {task_v}, {drop_fn});").unwrap();
                    } else {
                        // Fallback: dereference pointer to get Task struct (legacy path)
                        write!(out, "gorget_task_group_submit(*(TaskGroup*){group_arg}, *(Task__void*){task_v});").unwrap();
                    }
                } else {
                    let task_ty_name = task_ty.map(|t| c_type_named(t, sn)).unwrap_or_else(|| "Task__void".to_string());
                    write!(out, "gorget_task_group_submit(*(TaskGroup*){group_arg}, *({task_ty_name}*){task_v});").unwrap();
                }
                return;
            }

            let mut deref_clone_extra_close = false;
            let mut deref_deep_clone_ops: Option<Vec<String>> = None;
            if dst_needs_deref {
                let d = dst.unwrap();
                let dst_ty = val_types[d.0 as usize].as_ref().unwrap();
                let ty_name = c_type_named(dst_ty, sn);
                // For resource types, clone instead of shallow-copy to prevent double-free.
                let clone_fn = match ty_name.as_str() {
                    "GorgetArray" => Some("gorget_array_clone"),
                    "GorgetMap" => Some("gorget_map_clone"),
                    "GorgetSet" => Some("gorget_set_clone"),
                    "GorgetString" | "Str" => Some("gorget_string_clone_to_owned"),
                    _ => None,
                };
                // Deep-clone placeholder: when Phase 6 collection drops are enabled,
                // struct element reads need field-level cloning here.
                // Currently disabled — collection locals are not dropped at scope exit.
                let deep_clone_ops: Option<Vec<String>> = None;
                if let Some(cfn) = clone_fn {
                    // Emit: dst = clone_fn((Type*)call(args));  — extra ) needed after args
                    write!(out, "{} = {}(({ty_name}*)", v(d), cfn).unwrap();
                    deref_clone_extra_close = true;
                } else if deep_clone_ops.is_some() {
                    // Shallow copy then deep-clone resource fields.
                    write!(out, "{} = *({ty_name}*)", v(d)).unwrap();
                    deref_deep_clone_ops = deep_clone_ops;
                } else {
                    // Emit: dst = *(Type*)call(args);
                    write!(out, "{} = *({ty_name}*)", v(d)).unwrap();
                }
            } else if dst_is_task_struct {
                // Spawn now returns Task__T directly — simple assignment.
                let d = dst.as_ref().unwrap();
                let dst_ty = val_types[d.0 as usize].as_ref().unwrap();
                let _task_ty_name = c_type_named(dst_ty, sn);
                let fn_name_suffix = emit_name.strip_prefix("__gorget_spawn_").unwrap_or(emit_name);
                let spawn_param_c_types: Vec<String> = module.spawned_fns.iter()
                    .find(|sf| sf.fn_name == fn_name_suffix)
                    .map(|sf| sf.params.iter().map(|(_, ct)| ct.clone()).collect())
                    .unwrap_or_default();
                write!(out, "{} = {}(", v(*d), emit_name).unwrap();
                for (i, a) in emit_args.iter().enumerate() {
                    if i > 0 { write!(out, ", ").unwrap(); }
                    let spawn_c_ty = spawn_param_c_types.get(i).map(|s| s.as_str());
                    let arg_is_ptr = matches!(val_types.get(a.0 as usize).and_then(|t| t.as_ref()), Some(LirType::Ptr));
                    if arg_is_ptr && matches!(spawn_c_ty, Some("GorgetArray" | "GorgetMap" | "GorgetSet")) {
                        write!(out, "*({}*){}", spawn_c_ty.unwrap(), v(*a)).unwrap();
                    } else {
                        let ext_param = ext_params.and_then(|p| p.get(i));
                        emit_coerced_arg(out, a, ext_param, val_types, str_lit_vals, sn);
                    }
                }
                writeln!(out, ");").unwrap();
                return;
            } else if dst_is_spawn_ptr {
                // Spawn returns Task__T but dst is void*. Extract .__task pointer.
                let d = dst.as_ref().unwrap();
                let fn_name_suffix = emit_name.strip_prefix("__gorget_spawn_").unwrap_or(emit_name);
                let spawn_param_c_types: Vec<String> = module.spawned_fns.iter()
                    .find(|sf| sf.fn_name == fn_name_suffix)
                    .map(|sf| sf.params.iter().map(|(_, ct)| ct.clone()).collect())
                    .unwrap_or_default();
                write!(out, "{} = {}(", v(*d), emit_name).unwrap();
                for (i, a) in emit_args.iter().enumerate() {
                    if i > 0 { write!(out, ", ").unwrap(); }
                    let spawn_c_ty = spawn_param_c_types.get(i).map(|s| s.as_str());
                    let arg_is_ptr = matches!(val_types.get(a.0 as usize).and_then(|t| t.as_ref()), Some(LirType::Ptr));
                    let is_str_lit_arg = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
                    if arg_is_ptr && is_str_lit_arg && matches!(spawn_c_ty, Some("Str" | "GorgetString")) {
                        write!(out, "{v}", v = v(*a)).unwrap();
                    } else if arg_is_ptr && matches!(spawn_c_ty, Some("Str" | "GorgetString" | "GorgetArray" | "GorgetMap" | "GorgetSet")) {
                        write!(out, "*({}*){}", spawn_c_ty.unwrap(), v(*a)).unwrap();
                    } else {
                        let ext_param = ext_params.and_then(|p| p.get(i));
                        emit_coerced_arg(out, a, ext_param, val_types, str_lit_vals, sn);
                    }
                }
                writeln!(out, ").__task;").unwrap();
                return;
            } else if let Some(d) = dst {
                if !ret_is_void {
                    write!(out, "{} = ", v(*d)).unwrap();
                }
            }

            // ── void* param indices for collection functions ─────────
            let void_params = collection_void_param_indices(emit_name);

            // Fix printf format strings when float args use %lld.
            // The GIR generates %lld for all numeric args, but float args need %f.
            let fmt_arg_id = if is_printf && !emit_args.is_empty() {
                emit_args.first()
            } else { None };
            let _need_fmt_fix = is_printf && fmt_arg_id.map_or(false, |fid| {
                str_lit_vals.get(fid.0 as usize).copied().unwrap_or(false)
            }) && emit_args.iter().skip(1).any(|a| {
                let ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                let ptr_to_str = is_str_ptr_opt(ty, module)
                    || (matches!(ty, Some(LirType::Ptr)) && ptr_pointee.get(a.0 as usize)
                        .and_then(|p| p.as_ref())
                        .map_or(false, |p| is_str_struct(p, module)));
                matches!(ty, Some(LirType::F32 | LirType::F64))
                || matches!(ty, Some(LirType::Struct(sid)) if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetString"))
                || ptr_to_str
            });

            // Special-case single-arg printf: `print("hello")` lowers to a
            // `printf(str_arg)` with no format string. macOS clang rejects this
            // under -Wformat-security, and under 32-byte Str the arg is a struct,
            // not a char*, so we emit the decomposed form directly and return.
            let printf_needs_fmt_guard = is_printf && emit_args.len() == 1;
            if printf_needs_fmt_guard {
                let a = emit_args[0];
                let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                let is_gs_struct = matches!(arg_ty, Some(LirType::Struct(sid))
                    if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetString"));
                if is_gs_struct {
                    // `print(str)` → `__gorget_printf("%.*s", (int)str.len, (const char*)str.data);`
                    writeln!(out, "{}(\"%.*s\", (int){vv}.len, (const char*){vv}.data);",
                        emit_name, vv = v(a)).unwrap();
                    return;
                }
                // Ptr(Str) fallback: deref the pointer and use struct fields.
                let pointee_is_str = matches!(arg_ty, Some(LirType::Ptr))
                    && ptr_pointee.get(a.0 as usize)
                        .and_then(|p| p.as_ref())
                        .map_or(false, |p| is_str_struct(p, module));
                if pointee_is_str || is_str_ptr_opt(arg_ty, module) {
                    writeln!(out, "{}(\"%.*s\", (int)((Str*){vv})->len, (const char*)((Str*){vv})->data);",
                        emit_name, vv = v(a)).unwrap();
                    return;
                }
                // Rare non-Str path (shouldn't normally happen) — fall through to raw call.
            }

            write!(out, "{}(", emit_name).unwrap();
            if is_stderr_print {
                write!(out, "stderr").unwrap();
                if !emit_args.is_empty() {
                    write!(out, ", ").unwrap();
                }
            }
            // __gorget_await_* takes Task__T by value.
            // When the LIR arg is a Task struct, pass directly.
            // When the LIR arg is void* (non-vector case), construct a Task struct.
            let is_await = emit_name.starts_with("__gorget_await_");

            for (i, a) in emit_args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                let is_str_lit = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
                // ABI-driven marshalling: instruction-level tags (from runtime_extern_sig)
                // take priority, then extern declaration tags, then whitelist fallback.
                {
                    let inst_abi = arg_abis.get(i).copied().unwrap_or(crate::ir::abi::AbiKind::Auto);
                    if inst_abi != crate::ir::abi::AbiKind::Auto {
                        if emit_abi_arg(out, &v(*a), inst_abi, arg_ty, is_str_lit) {
                            continue;
                        }
                    }
                    let abi = resolve_param_abi(ext_decl, emit_name, i);
                    if emit_abi_arg(out, &v(*a), abi, arg_ty, is_str_lit) {
                        continue;
                    }
                }
                // For await helpers, coerce arg to Task__T.
                if is_await && i == 0 {
                    let is_task_struct = matches!(arg_ty, Some(LirType::Struct(sid)) if {
                        module.structs.get(sid.0 as usize).map_or(false, |s| s.name.starts_with("Task__"))
                    });
                    if is_task_struct {
                        // Already a Task struct — pass by value.
                        write!(out, "{}", v(*a)).unwrap();
                        continue;
                    } else if matches!(arg_ty, Some(LirType::Ptr)) {
                        // void* — could be:
                        // 1. Pointer to a Task struct (from SlotAddr of aggregate Task slot)
                        // 2. Raw SpawnCtx pointer (non-vector spawn, dst is void*)
                        // Check if the pointer points to a Task struct via ptr_pointee.
                        let pointee_is_task = ptr_pointee.get(a.0 as usize)
                            .and_then(|t| t.as_ref())
                            .map_or(false, |ty| matches!(ty, LirType::Struct(sid) if {
                                module.structs.get(sid.0 as usize).map_or(false, |s| s.name.starts_with("Task__"))
                            }));
                        if pointee_is_task {
                            // Dereference pointer to get Task struct value.
                            let await_fn_name = emit_name.strip_prefix("__gorget_await_").unwrap_or("");
                            let task_type = module.spawned_fns.iter()
                                .find(|sf| sf.fn_name == await_fn_name)
                                .map(|sf| if sf.ret_c_type == "void" { "Task__void".to_string() } else { format!("Task__{}", sf.ret_c_type) })
                                .unwrap_or_else(|| "Task__void".to_string());
                            write!(out, "*({task_type}*){}", v(*a)).unwrap();
                        } else {
                            // Raw SpawnCtx pointer — wrap in Task struct.
                            let await_fn_name = emit_name.strip_prefix("__gorget_await_").unwrap_or("");
                            let task_type = module.spawned_fns.iter()
                                .find(|sf| sf.fn_name == await_fn_name)
                                .map(|sf| if sf.ret_c_type == "void" { "Task__void".to_string() } else { format!("Task__{}", sf.ret_c_type) })
                                .unwrap_or_else(|| "Task__void".to_string());
                            let drop_fn = format!("__spawn_drop_{await_fn_name}");
                            write!(out, "({task_type}){{.__task = {v}, .__drop = {drop_fn}}}", v = v(*a)).unwrap();
                        }
                        continue;
                    }
                }
                // For spawn helpers, coerce void* collection args to the actual struct type.
                if is_spawn && matches!(arg_ty, Some(LirType::Ptr)) {
                    let spawn_fn_name = emit_name.strip_prefix("__gorget_spawn_").unwrap_or("");
                    let spawn_c_ty = module.spawned_fns.iter()
                        .find(|sf| sf.fn_name == spawn_fn_name)
                        .and_then(|sf| sf.params.get(i))
                        .map(|(_, ct)| ct.as_str());
                    let is_str_lit_arg = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
                    if matches!(spawn_c_ty, Some("Str" | "GorgetString")) && is_str_lit_arg {
                        // String literal → Str param: wrap
                        write!(out, "{v}", v = v(*a)).unwrap();
                        continue;
                    }
                    if matches!(spawn_c_ty, Some("GorgetArray" | "GorgetMap" | "GorgetSet" | "Str" | "GorgetString")) {
                        write!(out, "*({}*){}", spawn_c_ty.unwrap(), v(*a)).unwrap();
                        continue;
                    }
                }
                // Printf format rewriting for float/bool is handled by LIR lowering.
                // Str decomposition for CallExtern @printf with PtrTo(Str) args is still
                // needed here because the LIR lowering can't detect Str type for all values
                // (e.g., gorget_array_get returns generic void*).
                if _need_fmt_fix && i == 0 && is_str_lit {
                    // Rewrite format string: %lld → %.*s for Str args at emit time.
                    let fmt_val = *a;
                    let mut fmt_text: Option<&str> = None;
                    'find_fmt: for blk in &func.blocks {
                        for inst2 in &blk.insts {
                            if let Inst::StrLit { dst, value } = inst2 {
                                if *dst == fmt_val {
                                    fmt_text = Some(value.as_str());
                                    break 'find_fmt;
                                }
                            }
                        }
                    }
                    if let Some(fmt) = fmt_text {
                        // Fix Str and Float args (bool already handled by LIR lowering)
                        use crate::lir::lower::calls::PrintfArgKind;
                        let arg_kinds: Vec<PrintfArgKind> = emit_args[1..].iter()
                            .map(|ea| {
                                let ty = val_types.get(ea.0 as usize).and_then(|t| t.as_ref());
                                let ea_str_ptr = is_str_ptr_opt(ty, module)
                                    || (matches!(ty, Some(LirType::Ptr)) && ptr_pointee.get(ea.0 as usize)
                                        .and_then(|p| p.as_ref())
                                        .map_or(false, |p| is_str_struct(p, module)))
                                    || matches!(ty, Some(LirType::Struct(sid)) if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetString"));
                                if matches!(ty, Some(LirType::F32 | LirType::F64)) {
                                    PrintfArgKind::Float
                                } else if ea_str_ptr {
                                    PrintfArgKind::Str
                                } else {
                                    PrintfArgKind::Int
                                }
                            })
                            .collect();
                        if arg_kinds.iter().any(|k| *k != PrintfArgKind::Int) {
                            let fixed = crate::lir::lower::calls::fix_printf_format(fmt, &arg_kinds);
                            let escaped = escape_c_string(&fixed);
                            write!(out, "\"{}\"", escaped).unwrap();
                        } else {
                            write!(out, "{}", v(*a)).unwrap();
                        }
                    } else {
                        write!(out, "{}", v(*a)).unwrap();
                    }
                    continue;
                }
                // Decompose 32-byte Str / GorgetString args into (int)len, data for %.*s format.
                // Struct is a value type; empty strings are {"", 0, 0, NULL} so len/data read safely.
                if _need_fmt_fix && is_printf && i > 0
                    && matches!(arg_ty, Some(LirType::Struct(sid)) if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetString"))
                {
                    write!(out, "(int){v}.len, (const char*){v}.data", v = v(*a)).unwrap();
                    continue;
                }
                // PtrTo(Str) in printf — deref the pointer, then read struct fields.
                if _need_fmt_fix && is_printf && i > 0 && (is_str_ptr_opt(arg_ty, module)
                    || (matches!(arg_ty, Some(LirType::Ptr)) && ptr_pointee.get(a.0 as usize)
                        .and_then(|p| p.as_ref())
                        .map_or(false, |p| is_str_struct(p, module)))) {
                    write!(out, "(int)((Str*){v})->len, (const char*)((Str*){v})->data", v = v(*a)).unwrap();
                    continue;
                }
                // Pre-decomposed %.*s data arg: the LIR lowering splits Str into
                // (int32)len + (Ptr)data as separate args. The len was already emitted;
                // the data arg is a raw void* from a FieldPtr→Load on Str.data.
                // Cast to (const char*) to satisfy printf's %s expectation.
                if _need_fmt_fix && is_printf && i > 1
                    && matches!(arg_ty, Some(LirType::Ptr))
                {
                    // Check if the preceding arg was i32 (the len half of a %.*s pair)
                    let prev_ty = val_types.get(emit_args[i-1].0 as usize).and_then(|t| t.as_ref());
                    if matches!(prev_ty, Some(LirType::I32)) {
                        write!(out, "(const char*){}", v(*a)).unwrap();
                        continue;
                    }
                }
                if false {
                    // Placeholder
                }
                // Box[Str] alloc: StrLit arg → wrap with gorget_str_from_literal.
                // Ptr arg (from a Str variable) → deref as *(Str*).
                else if name == "__gorget_box_alloc_Str" || name == "__gorget_box_alloc_GorgetString" {
                    if is_str_lit {
                        write!(out, "{}", v(*a)).unwrap();
                    } else if matches!(arg_ty, Some(LirType::Ptr)) {
                        write!(out, "*(Str*){}", v(*a)).unwrap();
                    } else {
                        write!(out, "{}", v(*a)).unwrap();
                    }
                }
                // String literal arg to a gorget_str_* or Dict/Set inline function → Str wrap.
                // Skip if ext_params says this arg is not a Str type (e.g. gorget_str_join arg 1 = GorgetArray).
                // Also wrap for any extern function whose declared param at position i is Str.
                else if is_str_lit && trait_str_arg_positions.contains(&i) {
                    // StrLit to trait box method. Check wrapper param type:
                    // Ptr (void*) → &(Str){} address. Struct(Str) → gorget_str_from_literal value.
                    let wrapper_param_is_ptr = ext_params.and_then(|p| p.get(i)).map_or(false, |t| t.is_ptr());
                    if wrapper_param_is_ptr {
                        write!(out, "&{v}", v = v(*a)).unwrap();
                    } else {
                        write!(out, "{}", v(*a)).unwrap();
                    }
                }
                else if is_str_lit && (name.starts_with("gorget_str_") || force_str_coerce
                    || ext_param_is_str(ext_params, i, module))
                    && !str_fn_non_str_arg(name, i) {
                    write!(out, "{}", v(*a)).unwrap();
                }
                // Ptr arg to a trait box method or runtime function that expects Str → deref as *(Str*).
                // Skip deref if the wrapper's param is void* (Box trait vtable dispatch).
                else if trait_str_arg_positions.contains(&i)
                    && matches!(arg_ty, Some(LirType::Ptr)) && !is_str_lit {
                    let wrapper_param_is_ptr = ext_params.and_then(|p| p.get(i)).map_or(true, |t| t.is_ptr());
                    if wrapper_param_is_ptr {
                        // Wrapper takes void* — pass the pointer directly.
                        write!(out, "{}", v(*a)).unwrap();
                    } else {
                        // Wrapper takes Str by value — deref the pointer.
                        write!(out, "*(Str*){}", v(*a)).unwrap();
                    }
                }
                // GorgetString arg to a gorget_str_* function — same 32-byte struct, no coercion needed.
                else if name.starts_with("gorget_str_") && is_gorget_string_type(arg_ty, sn)
                    && !str_fn_non_str_arg(name, i) {
                    write!(out, "{}", v(*a)).unwrap();
                }
                // Ptr arg to a gorget_str_* function that expects Str by value → deref to Str.
                // Skip self-by-ptr methods (they take GorgetString*) and arg 0 of ptr methods.
                else if name.starts_with("gorget_str_") && matches!(arg_ty, Some(LirType::Ptr))
                    && !str_fn_non_str_arg(name, i) {
                    write!(out, "*(Str*){}", v(*a)).unwrap();
                }
                // Collection void* element params — wrap concrete values with &(Type){val}.
                // For Str/GorgetString (32-byte struct), the value is already the struct —
                // take its address directly rather than constructing a single-field compound literal.
                else if void_params.contains(&i) && arg_ty.map_or(false, |t| !matches!(t, LirType::Ptr)) {
                    let ty_name = c_type_named(arg_ty.unwrap(), sn);
                    if ty_name == "Str" || ty_name == "GorgetString" {
                        write!(out, "&{v}", v = v(*a)).unwrap();
                    } else {
                        write!(out, "&({ty_name}){{ {} }}", v(*a)).unwrap();
                    }
                }
                // String literal arg to a void* collection param → wrap as &(Str){...}.
                // This handles Dict/Set with Str keys: gorget_map_put(m, &(Str){..}, &val).
                else if void_params.contains(&i) && is_str_lit {
                    write!(out, "&{v}", v = v(*a)).unwrap();
                }
                // Ptr(Str) → Str by-value deref: when arg is a pointer to a Str
                // (tracked by ptr_pointee) and no other handler matched, deref
                // to pass Str by value. Handles functions like sqlite wrappers
                // whose C declarations take Str, not const char*.
                // Skip void* collection params and runtime-by-ptr params.
                else if matches!(arg_ty, Some(LirType::Ptr)) && !is_str_lit
                    && !void_params.contains(&i)
                    && ptr_pointee.get(a.0 as usize).and_then(|t| t.as_ref())
                        .map_or(false, |t| is_str_struct(t, module))
                {
                    write!(out, "*(Str*){}", v(*a)).unwrap();
                }
                // Use general coercion for extern params.
                else {
                    let ext_param = ext_params.and_then(|p| p.get(i));
                    emit_coerced_arg(out, a, ext_param, val_types, str_lit_vals, sn);
                }
            }
            if deref_clone_extra_close {
                write!(out, "));").unwrap();
            } else {
                write!(out, ");").unwrap();
            }
            // Deep-clone resource fields in user structs read from collections.
            if let Some(ops) = deref_deep_clone_ops {
                for op in ops {
                    write!(out, " {op}").unwrap();
                }
            }

            // Set elem_drop/val_drop on collection constructors.
            // Uses original_name to determine element type from the monomorphized name.
            if let Some(orig) = original_name.as_ref() {
                if let Some(d) = dst {
                    let dv = format!("__v{}", d.0);
                    // Vector/Array constructor: set elem_drop
                    if (name.starts_with("gorget_array_new") || name == "gorget_array_with_capacity")
                        && (orig.starts_with("Vector__") || orig.starts_with("Deque__"))
                    {
                        let raw_elem = orig.strip_prefix("Vector__")
                            .or_else(|| orig.strip_prefix("Deque__"))
                            .unwrap_or("");
                        let elem_type = raw_elem.strip_suffix("__new")
                            .or_else(|| raw_elem.strip_suffix("__with_capacity"))
                            .unwrap_or(raw_elem);
                        if let Some(drop_fn) = elem_drop_fn_for_c_type(elem_type) {
                            write!(out, " {dv}.elem_drop = (__gorget_drop_fn){drop_fn};").unwrap();
                        } else if module.recursive_drop_structs.contains_key(elem_type)
                            || module.recursive_drop_enums.contains_key(elem_type)
                        {
                            write!(out, " {dv}.elem_drop = (__gorget_drop_fn){elem_type}__drop;").unwrap();
                        }
                        if let Some(clone_fn) = elem_clone_fn_for_c_type(elem_type) {
                            write!(out, " {dv}.elem_clone = (__gorget_drop_fn){clone_fn};").unwrap();
                        } else if module.recursive_drop_structs.contains_key(elem_type)
                            || module.recursive_drop_enums.contains_key(elem_type)
                        {
                            write!(out, " {dv}.elem_clone = (__gorget_drop_fn){elem_type}__clone_inplace;").unwrap();
                        }
                        if let Some(mat_fn) = elem_materialize_fn_for_c_type(elem_type) {
                            write!(out, " {dv}.elem_materialize = (__gorget_drop_fn){mat_fn};").unwrap();
                        }
                    }
                    // Set/HashSet constructor: wire Hashable/Equatable bridges
                    // + key_drop / key_clone for user-type elements. GorgetSet
                    // is the same struct as GorgetMap with val_size=0, so the
                    // same `hash_fn` / `eq_fn` / `key_*` fields apply. The
                    // regular elem_drop/clone on the set's data array is
                    // handled via the Dict-style wiring below since
                    // `gorget_ordered_set_new` / `gorget_set_new` both return
                    // a GorgetMap.
                    if (name.starts_with("gorget_ordered_set_new") || name.starts_with("gorget_set_new"))
                        && (orig.starts_with("Set__") || orig.starts_with("HashSet__"))
                    {
                        let prefix = if orig.starts_with("Set__") { "Set__" } else { "HashSet__" };
                        if let Some(rest) = orig.strip_prefix(prefix) {
                            // Set constructor mangling is `Set__T__new` /
                            // `Set__T__new_str` / `Set__T__with_capacity` /
                            // `Set__T` (bare). Strip the method suffix if
                            // present; whatever remains is the element type.
                            let elem_type = rest.strip_suffix("__new_str")
                                .or_else(|| rest.strip_suffix("__new"))
                                .or_else(|| rest.strip_suffix("__with_capacity"))
                                .unwrap_or(rest);
                            if is_user_hashable_key(elem_type, module) {
                                write!(out, " {dv}.hash_fn = (__gorget_hash_fn)__gorget_ktable_hash__{elem_type};").unwrap();
                                write!(out, " {dv}.eq_fn = (__gorget_eq_fn)__gorget_ktable_eq__{elem_type};").unwrap();
                                if module.recursive_drop_structs.contains_key(elem_type)
                                    || module.recursive_drop_enums.contains_key(elem_type)
                                {
                                    write!(out, " {dv}.key_drop = (__gorget_drop_fn){elem_type}__drop;").unwrap();
                                    write!(out, " {dv}.key_clone = (__gorget_drop_fn){elem_type}__clone_inplace;").unwrap();
                                }
                            }
                        }
                    }
                    // Dict/HashMap constructor: set val_drop + val_clone
                    if (name.starts_with("gorget_dict_new") || name.starts_with("gorget_map_new"))
                        && (orig.starts_with("Dict__") || orig.starts_with("HashMap__"))
                    {
                        let prefix = if orig.starts_with("Dict__") { "Dict__" } else { "HashMap__" };
                        if let Some(rest) = orig.strip_prefix(prefix) {
                            let rest_stripped = rest.strip_suffix("__new_str")
                                .or_else(|| rest.strip_suffix("__new"))
                                .unwrap_or(rest);
                            if let Some(pos) = rest_stripped.find("__") {
                                let key_type = &rest_stripped[..pos];
                                let val_type = &rest_stripped[pos + 2..];
                                if let Some(drop_fn) = elem_drop_fn_for_c_type(val_type) {
                                    write!(out, " {dv}.val_drop = (__gorget_drop_fn){drop_fn};").unwrap();
                                } else if module.recursive_drop_structs.contains_key(val_type)
                                    || module.recursive_drop_enums.contains_key(val_type)
                                {
                                    write!(out, " {dv}.val_drop = (__gorget_drop_fn){val_type}__drop;").unwrap();
                                }
                                if let Some(clone_fn) = elem_clone_fn_for_c_type(val_type) {
                                    write!(out, " {dv}.val_clone = (__gorget_drop_fn){clone_fn};").unwrap();
                                } else if module.recursive_drop_structs.contains_key(val_type)
                                    || module.recursive_drop_enums.contains_key(val_type)
                                {
                                    write!(out, " {dv}.val_clone = (__gorget_drop_fn){val_type}__clone_inplace;").unwrap();
                                }
                                if val_type == "GorgetString" {
                                    write!(out, " {dv}.val_materialize = (__gorget_drop_fn)gorget_string_materialize_inplace;").unwrap();
                                }
                                // User-type key with Hashable+Equatable: wire the
                                // runtime bridges so Dict membership uses the user's
                                // hash/eq rather than byte-FNV/memcmp (see
                                // `emit_hashable_key_bridges` in c_lir/mod.rs).
                                if is_user_hashable_key(key_type, module) {
                                    write!(out, " {dv}.hash_fn = (__gorget_hash_fn)__gorget_ktable_hash__{key_type};").unwrap();
                                    write!(out, " {dv}.eq_fn = (__gorget_eq_fn)__gorget_ktable_eq__{key_type};").unwrap();
                                    // User key types often hold resource fields
                                    // (String / Vector / …). Wire the same
                                    // clone/drop the Dict uses for values so the
                                    // key slot is reclaimed on remove/drop and
                                    // duplicated on put.
                                    if module.recursive_drop_structs.contains_key(key_type)
                                        || module.recursive_drop_enums.contains_key(key_type)
                                    {
                                        write!(out, " {dv}.key_drop = (__gorget_drop_fn){key_type}__drop;").unwrap();
                                        write!(out, " {dv}.key_clone = (__gorget_drop_fn){key_type}__clone_inplace;").unwrap();
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Ownership transfer after consuming runtime calls is now always
            // emitted as a GIR `MoveZero` instruction at the lowering layer
            // (`lower_method_call`'s move_zero_locals + `lower_index_assign`
            // post-call moves). The C backend previously also inserted a
            // post-call zero for a fixed list of runtime functions; that
            // duplicated the same bytes for the same locals and has been
            // removed now that every consuming call site emits MoveZero
            // through the shared GIR helpers.
}
