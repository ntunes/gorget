//! CallExtern instruction emission -- the largest match arm in emit_inst().

use super::*;

/// Emit code for an `Inst::CallExtern` instruction.
pub(super) fn emit_call_extern(
    out: &mut String,
    dst: &Option<ValueId>,
    name: &str,
    args: &[ValueId],
    arg_abis: &[crate::ir::abi::AbiKind],
    ctx: &super::EmitContext,
    loc: &(String, u32, u32),
) {
    let func = ctx.func;
    let module = ctx.module;
    let sn = ctx.sn;
    let val_types = ctx.val_types;
    // Per-value origin queries go through the typed accessors
    // `ctx.is_str_lit / is_null / is_cstr / is_cstr_extern / spawn_source`.
    let ptr_pointee = ctx.ptr_pointee;
    let v = |id: ValueId| -> String { format!("__v{}", id.0) };
    let _s = |id: SlotId| -> String { format!("__s{}", id.0) };

            // `original_name` is no longer consumed by the C backend's
            // CallExtern arm — every collection-ctor side effect that used
            // to depend on it (elem_drop / val_drop / hash_fn / key_drop / …)
            // is now wired at LIR level. The field is still carried on
            // `Inst::CallExtern` because the `wire_collection_bridges` pass
            // and `infer_collection_elem_fns` consume it; we just don't
            // need to read it here.
            let _ = args;
            // ── Closure dispatch is now Inst::CallClosure (not CallExtern) ──

            // ── Drop guards are now Inst::DropGuardOpen/Close (not CallExtern) ──

            // (Previously: inline float/int/bool primitive casts +
            // `int(string)` → gorget_str_ord dispatch. LIR's Tier 3c
            // intercept at src/lir/lower/insts.rs ~3454 rewrites these to
            // IntToFloat/IntCast/FloatToInt/FloatCast (and gorget_str_ord
            // for Str→I64) before BIR lowering, so the backend branch is
            // dead when a destination is assigned. The rare dst=None case
            // has no observable effect on a pure cast.)

            // Route compiler-emit `gorget_panic` through `gorget_panic_at` so
            // the runtime message carries `file:line:col`. Runtime-internal
            // callers keep the 1-arg `gorget_panic` (wrapper degrades to
            // `<unknown>:0:0`) until per-runtime-fn span plumbing lands.
            if name == "gorget_panic" && args.len() == 1 {
                let a = args[0];
                let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                let is_str_lit = ctx.is_str_lit(a);
                let is_gs_struct = matches!(arg_ty, Some(LirType::Struct(sid))
                    if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetString" || s.name == "Str"));
                let msg_expr = if is_str_lit {
                    format!("(const char*){}.data", v(a))
                } else if is_gs_struct {
                    format!("(const char*){}.data", v(a))
                } else if matches!(arg_ty, Some(LirType::Ptr)) {
                    // Pointer to Str struct (typical case from the CStr ABI marshalling).
                    format!("(const char*)((Str*){})->data", v(a))
                } else {
                    format!("(const char*){}", v(a))
                };
                write!(out, "gorget_panic_at(\"{f}\", {ln}, {cl}, {msg_expr});",
                    f = loc.0, ln = loc.1, cl = loc.2).unwrap();
                return;
            }

            // Route compiler-emit `gorget_trap(code, detail)` through
            // `gorget_trap_at(code, detail, file, line, col)` threading the
            // call-site span — the SAME machinery as the gorget_panic rewrite
            // above (D11 trap normalization). Both args marshal to const char*.
            if name == "gorget_trap" && args.len() == 2 {
                let marshal = |a: ValueId| -> String {
                    let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                    let is_gs_struct = matches!(arg_ty, Some(LirType::Struct(sid))
                        if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetString" || s.name == "Str"));
                    if ctx.is_str_lit(a) || is_gs_struct {
                        format!("(const char*){}.data", v(a))
                    } else if matches!(arg_ty, Some(LirType::Ptr)) {
                        format!("(const char*)((Str*){})->data", v(a))
                    } else {
                        format!("(const char*){}", v(a))
                    }
                };
                let code_expr = marshal(args[0]);
                let detail_expr = marshal(args[1]);
                write!(out, "gorget_trap_at({code_expr}, {detail_expr}, \"{f}\", {ln}, {cl});",
                    f = loc.0, ln = loc.1, cl = loc.2).unwrap();
                return;
            }

            // ── Flagship `v[i]` READ: real source location (D11 Layer B) ──
            // Reroute the compiler-emitted Vector element-read
            // `gorget_array_get(arr, idx)` to the location-aware
            // `gorget_array_get_at(arr, idx, code, file, line, col)` so an
            // out-of-bounds subscript surfaces `trap[T_Bounds]: … at file:line:col`
            // + exit 101 (owner ruling). The `T_Bounds` code is threaded as DATA
            // from `TrapKind::Bounds.code()` (layering rule 2), the span from the
            // resolved `loc` tuple. The dst stays `Ptr` — `gorget_array_get_at`
            // returns the element `void*` and the downstream shared LIR
            // `Inst::Load` (materialize_collection_element) derefs it, so this is a
            // single early-return branch that does NOT deref here (a deref would
            // double-deref). Runtime-internal / self-host callers keep the
            // span-less `gorget_array_get` (normalized to `gorget_trap` in the
            // shared runtime), so those flip to `trap[T_Bounds]` for free.
            if name == "gorget_array_get" && args.len() == 2 {
                if let Some(d) = dst {
                    let code = crate::trap::TrapKind::Bounds.code();
                    write!(out, "{} = gorget_array_get_at({}, {}, \"{code}\", \"{f}\", {ln}, {cl});",
                        v(*d), v(args[0]), v(args[1]), f = loc.0, ln = loc.1, cl = loc.2).unwrap();
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
                // gorget_str_codepoint_at(Str s, int64_t byte_pos) → Str: a
                // cap=0 VIEW REGION into s's buffer (no allocation) — sibling
                // of the emit_types.rs helper-fn emission. Owning consumers
                // upgrade the view via the collection-put materialize hooks /
                // boundary clones; lazy-view sources are materialized first
                // by lower_for_string's W3d hook.
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
            // (Previously: inline Option/Result tag-check dispatch for
            // __option_is_some/is_none and `*__is_some/_ok/_none/_err`.
            // LIR's emit_extern_call Tier 2b (src/lir/lower/insts.rs
            // ~3423) intercepts these and emits `Load + Cmp` directly, so
            // by BIR time the CallExtern is gone.)
            // ── Option/Result combinator inline expansion ──
            // map, filter, and_then, or_else (and friends) are inlined per call site
            // in `emit_hof::try_emit_option_result_combinator`. Lifted out for
            // readability — see that module's docstring for the full list.
            if super::emit_hof::try_emit_option_result_combinator(out, dst, name, args, ctx, loc) {
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
            //
            // Read typed `struct_aliases` (Phase A residual #2) — a
            // monomorphized collection alias name is registered when its
            // alias-target StructDef is registered. Replaces five
            // `name.starts_with("Vector__"|"Set__"|"Dict__"|"HashMap__"|
            // "HashSet__")` arms with one typed map lookup.
            if module.struct_aliases.contains_key(name) {
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
                emit_name, dst, args, val_types, func, sn, &module.structs,
            ) {
                write!(out, "{}", outparam_code).unwrap();
                return;
            }

            // ── File.create / File.open rewrite ──
            // gorget_file_create(path) → gorget_file_open(cstr, "w")
            // gorget_file_open(path) with 1 arg → gorget_file_open(cstr, "r")
            if emit_name == "gorget_file_create" && args.len() == 1 {
                let a = args[0];
                let is_str_lit = ctx.is_str_lit(a);
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
                let is_str_lit = ctx.is_str_lit(a);
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
                let arg0_is_empty_str = ctx.is_str_lit(args[0])
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

            // (Previously: gorget_str_push / gorget_str_push_line scalar
            // dispatch lived here. LIR's `emit_extern_call` intercept at
            // `src/lir/lower/insts.rs` ~3581 rewrites the generic name to
            // `gorget_string_push_{int,float,bool}` before BIR lowering,
            // so this backend branch is dead. Str arg keeps the original
            // name; the `static inline gorget_str_push` shim in
            // emit_types.rs handles it.)

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
                    // Read typed `enum_kind` set at LIR struct registration
                    // from GIR's `enum_category`.
                    if sdef.enum_kind == crate::lir::EnumKind::Option {
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
                            let is_str_lit = ctx.is_str_lit(a);
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
                            // The cast type is the Option's payload LIR type —
                            // not the caller-provided name. This avoids a
                            // name-substring dispatch (uint8/int32/…) and
                            // derives the cast from the destination's
                            // canonical type.
                            let payload_lir_ty = sdef.fields.get(1).map(|(_, t)| t);
                            let cast_type = match payload_lir_ty {
                                Some(LirType::I8) => "int8_t",
                                Some(LirType::I16) => "int16_t",
                                Some(LirType::I32) => "int32_t",
                                Some(LirType::U8) => "uint8_t",
                                Some(LirType::U16) => "uint16_t",
                                Some(LirType::U32) => "uint32_t",
                                Some(LirType::U64) => "uint64_t",
                                _ => "int64_t", // default + I64
                            };
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

            // Vector/Dict/Set higher-order methods no longer dispatch here:
            // every surface method has a path — HofExpand (filter/map/fold/
            // reduce/any/all/each/find/find_index/count/flat_map), runtime
            // stubs (sort/sorted/unique/windows/chunks), or the BIR
            // SynthPool (sort_by / sorted_by / sort_by_key / sorted_by_key).
            // If a call ever falls through, the linker surfaces it —
            // we no longer quietly swallow it.

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
                            let is_str_lit = ctx.is_str_lit(*a);
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
                            emit_coerced_arg(out, a, ext_param, val_types, func, sn);
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
                            let is_str_lit = ctx.is_str_lit(*a);
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
                            emit_coerced_arg(out, a, ext_param, val_types, func, sn);
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
                    if let Some(spawn_fn) = ctx.spawn_source(task_arg) {
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
                        emit_coerced_arg(out, a, ext_param, val_types, func, sn);
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
                    let is_str_lit_arg = ctx.is_str_lit(*a);
                    if arg_is_ptr && is_str_lit_arg && matches!(spawn_c_ty, Some("Str" | "GorgetString")) {
                        write!(out, "{v}", v = v(*a)).unwrap();
                    } else if arg_is_ptr && matches!(spawn_c_ty, Some("Str" | "GorgetString" | "GorgetArray" | "GorgetMap" | "GorgetSet")) {
                        write!(out, "*({}*){}", spawn_c_ty.unwrap(), v(*a)).unwrap();
                    } else {
                        let ext_param = ext_params.and_then(|p| p.get(i));
                        emit_coerced_arg(out, a, ext_param, val_types, func, sn);
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

            // Printf format-string rewriting + Str-arg decomposition is in
            // `emit_printf.rs` (lifted 2026-05-18, mirroring the 2026-05-16
            // `emit_hof.rs` extraction). `_need_fmt_fix` is the gating flag
            // for the per-arg rewrites below; `try_emit_single_arg_printf`
            // is the `print("hello")` / `print(str_var)` fast path that
            // must run before the generic `emit_name(...)` open.
            let _need_fmt_fix = super::emit_printf::need_fmt_fix(is_printf, emit_args, ctx);
            if super::emit_printf::try_emit_single_arg_printf(
                out, is_printf, is_stderr_print, emit_name, emit_args, ctx,
            ) {
                return;
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
                let is_str_lit = ctx.is_str_lit(*a);
                // ABI-driven marshalling: instruction-level tags (from RuntimeFn::resolve_lir_sig)
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
                    let is_str_lit_arg = ctx.is_str_lit(*a);
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
                // (e.g., gorget_array_get returns generic void*). See `emit_printf.rs`
                // for the four-case rewrite cluster (format-string rewrite, Struct(GS)
                // decomposition, PtrTo(Str) deref, pre-decomposed (len, data) cast).
                if super::emit_printf::try_emit_printf_arg(
                    out, i, *a, arg_ty, is_str_lit, _need_fmt_fix, is_printf,
                    emit_args, ctx,
                ) {
                    continue;
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
                else if is_str_lit && (name.starts_with("gorget_str_")
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
                    emit_coerced_arg(out, a, ext_param, val_types, func, sn);
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

            // Collection constructor post-call wiring is now fully LIR-level.
            // `infer_collection_elem_fns` + `emit_collection_fn_ptr_stores` in
            // `src/lir/lower/insts.rs` emit `NamedFuncAddr + ElemPtr + Store`
            // for all of:
            //   - Vector/Deque: elem_drop=40, elem_clone=48, elem_materialize=56
            //   - Dict/HashMap: val_drop=104, val_clone=112, key_drop=120,
            //                   key_clone=128, val_materialize=136
            //   - Set/HashSet:  key_drop=120, key_clone=128
            //   - Hashable bridges (hash_fn=88, eq_fn=96): SetCollectionBridge
            //     inst (`wire_collection_bridges` pass).
            // Both backends compile those LIR insts uniformly.


            // Ownership transfer after consuming runtime calls is now always
            // emitted as a GIR `MoveZero` instruction at the lowering layer
            // (`lower_method_call`'s move_zero_locals + `lower_index_assign`
            // post-call moves). The C backend previously also inserted a
            // post-call zero for a fixed list of runtime functions; that
            // duplicated the same bytes for the same locals and has been
            // removed now that every consuming call site emits MoveZero
            // through the shared GIR helpers.
}
