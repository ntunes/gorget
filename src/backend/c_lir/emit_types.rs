//! Type definition emission, drop/clone functions, higher-order collection helpers,
//! Option/Result combinator helpers, spawn/thread helpers, and runtime module selection.

use super::*;

/// Find the __call function name for a closure struct type.
pub(super) fn find_closure_call_fn(module: &LirModule, struct_c_name: &str, sn: &HashMap<u32, String>) -> String {
    // Map c_name back to struct def to get the original name (e.g., "__Closure_0").
    for (i, def) in module.structs.iter().enumerate() {
        let c_name = sn.get(&(i as u32)).map(|s| s.as_str()).unwrap_or(&def.name);
        if c_name == struct_c_name {
            // Look for a function named `<original_name>__call`
            let call_name = format!("{}__call", def.name);
            if module.functions.iter().any(|f| f.name == call_name) {
                return call_name;
            }
        }
    }
    // Fallback: try interpreting struct_c_name as the original name directly.
    let call_name = format!("{struct_c_name}__call");
    if module.functions.iter().any(|f| f.name == call_name) {
        return call_name;
    }
    // Last resort: return a placeholder
    format!("/* UNKNOWN_CLOSURE_CALL for {struct_c_name} */")
}

/// Look up the return type of a closure's `__call` function in LIR.
pub(super) fn closure_call_return_type(module: &LirModule, call_fn_name: &str, sn: &HashMap<u32, String>) -> Option<String> {
    module.functions.iter()
        .find(|f| f.name == call_fn_name)
        .map(|f| c_type_named(&f.return_type, sn))
}

/// Check which closure params (skipping env pointer) are passed by pointer.
/// Returns a vec of bools — true means the template should use `&` prefix for that arg.
pub(super) fn closure_params_need_ref(module: &LirModule, call_fn: &str) -> Vec<bool> {
    if let Some(func) = module.functions.iter().find(|f| f.name == call_fn) {
        // Params: [0]=env_ptr, [1..]=closure params → skip env
        func.params.iter().skip(1)
            .map(|t| matches!(t, LirType::PtrTo(_) | LirType::Ptr))
            .collect()
    } else {
        Vec::new()
    }
}


/// Find the C name for a struct whose original name matches a prefix.
pub(super) fn find_struct_c_name_by_prefix(prefix: &str, module: &LirModule, sn: &HashMap<u32, String>) -> Option<String> {
    for (i, def) in module.structs.iter().enumerate() {
        if def.name == prefix {
            return Some(sn.get(&(i as u32)).cloned().unwrap_or_else(|| def.name.clone()));
        }
    }
    None
}

/// Convert a monomorphized element type name to its C type.
/// Option/Result combinator methods that the old C backend generates inline.
pub(super) const OPTION_COMBINATORS: &[&str] = &[
    "map", "filter", "and_then", "or_else", "unwrap_or_else", "flat_map", "or", "flatten", "zip",
];
pub(super) const RESULT_COMBINATORS: &[&str] = &[
    "map", "map_err", "and_then", "or_else", "unwrap_err", "unwrap_error",
];

/// Parse an Option/Result combinator name like `Option__int64_t__map` or
/// Returns None if not a combinator.
pub(super) fn parse_option_result_combinator(name: &str) -> Option<(&str, &str)> {
    if name.starts_with("Option__") {
        let rest = name.strip_prefix("Option__")?;
        let sep_pos = rest.rfind("__")?;
        let method = &rest[sep_pos + 2..];
        if OPTION_COMBINATORS.contains(&method) || RESULT_COMBINATORS.contains(&method) {
            return Some((&name[..name.len() - method.len() - 2], method));
        }
    }
    if name.starts_with("Result__") {
        let rest = name.strip_prefix("Result__")?;
        let sep_pos = rest.rfind("__")?;
        let method = &rest[sep_pos + 2..];
        if RESULT_COMBINATORS.contains(&method) || OPTION_COMBINATORS.contains(&method) {
            return Some((&name[..name.len() - method.len() - 2], method));
        }
    }
    None
}

/// For Option/Result structs, get the field names for the payload arms.
/// Returns (ok_field, err_field) — for Option: ("Some_0", "None_0"), for Result: ("Ok_0", "Error_0").
/// Falls back to ("Some_0", "None_0") if not found.
pub(super) fn enum_payload_fields(type_prefix: &str, module: &LirModule) -> (String, String) {
    // Look up the struct definition by matching the type_prefix to a struct name
    for def in &module.structs {
        if def.name == type_prefix {
            // tag is field 0; payload field is field 1 (ok/some); error field is field 2 if present
            let ok_f = def.fields.get(1)
                .map(|(n, _)| c_field_name(n))
                .unwrap_or_else(|| "Some_0".to_string());
            let err_f = def.fields.get(2)
                .map(|(n, _)| c_field_name(n))
                .unwrap_or_else(|| "None_0".to_string());
            return (ok_f, err_f);
        }
    }
    ("Some_0".to_string(), "None_0".to_string())
}
/// Generate static inline C helpers for Option/Result combinator methods.
pub(super) fn emit_option_result_combinator_helpers(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    // (full_name, src_c_type, result_c_type, method, closure_c_type, call_fn,
    //  ok_field, err_field, is_result)
    let mut helpers: Vec<(String, String, String, String, String, String, String, String, bool)> = Vec::new();

    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::CallExtern { name, .. } = inst {
                    if let Some((type_prefix, method)) = parse_option_result_combinator(name) {
                        if !seen.insert(name.clone()) {
                            continue;
                        }
                        let ext = module.externs.iter().find(|e| e.name == *name);
                        let closure_c_type = ext.and_then(|e| e.params.get(1))
                            .map(|t| c_type_named(t, sn))
                            .unwrap_or_else(|| "void*".into());
                        let closure_struct_name = closure_c_type.clone();
                        let call_fn = find_closure_call_fn(module, &closure_struct_name, sn);

                        let (ok_field, err_field) = enum_payload_fields(type_prefix, module);

                        let src_c = find_struct_c_name_by_prefix(type_prefix, module, sn)
                            .unwrap_or_else(|| type_prefix.to_string());

                        // Read typed `enum_kind` (set at LIR struct registration
                        // from GIR's `enum_category`) to discriminate Option vs
                        // Result without name-prefix matching.
                        let is_result = module.structs.iter()
                            .find(|s| s.name == type_prefix)
                            .map(|s| s.enum_kind == crate::lir::EnumKind::Result)
                            .unwrap_or(false);

                        // Result C type: read from the typed LirExtern field when available
                        // (set by the LIR post-pass for cross-type maps), otherwise same as source.
                        // For flatten: result is the Option/Result's inner payload type.
                        let result_c = if method == "flatten" {
                            module.structs.iter().find(|s| s.name == type_prefix)
                                .and_then(|s| s.fields.get(1))
                                .map(|(_, t)| c_type_named(t, sn))
                                .unwrap_or_else(|| src_c.clone())
                        } else if let Some(sid) = ext.and_then(|e| e.combinator_result_struct_id) {
                            let idx = sid.0 as usize;
                            sn.get(&(idx as u32)).cloned()
                                .or_else(|| module.structs.get(idx).map(|s| s.name.clone()))
                                .unwrap_or_else(|| src_c.clone())
                        } else {
                            src_c.clone()
                        };

                        helpers.push((name.clone(), src_c, result_c, method.to_string(), closure_c_type, call_fn, ok_field, err_field, is_result));
                    }
                }
            }
        }
    }

    if helpers.is_empty() {
        return;
    }

    writeln!(out, "/* ── Option/Result combinator helpers ── */").unwrap();
    for (full_name, src_c, result_c, method, closure_ty, call_fn, ok_field, err_field, is_result) in &helpers {
        // Determine if closure params need & prefix (Ptr ABI for resource types)
        let comb_needs_ref = closure_params_need_ref(module, call_fn);
        let cr = if comb_needs_ref.first().copied().unwrap_or(false) { "&" } else { "" };
        match method.as_str() {
            "map" => {
                // map: if tag==0 (Some/Ok): apply closure to payload, wrap; else propagate
                // For map on Result, we need the result type's ok field too
                let result_ok = if *is_result {
                    let result_prefix = full_name.rsplitn(2, "__").nth(1).unwrap_or(full_name);
                    let (rok, _) = enum_payload_fields(result_prefix, module);
                    rok
                } else {
                    ok_field.clone()
                };
                writeln!(out, "static inline {result_c} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    {result_c} __result;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{").unwrap();
                writeln!(out, "        __result.tag = 0;").unwrap();
                writeln!(out, "        __result.{result_ok} = {call_fn}(&__fn, {cr}__src.{ok_field});").unwrap();
                writeln!(out, "    }} else {{").unwrap();
                writeln!(out, "        __result.tag = 1;").unwrap();
                writeln!(out, "    }}").unwrap();
                writeln!(out, "    return __result;").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "filter" => {
                writeln!(out, "static inline {src_c} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0 && {call_fn}(&__fn, {cr}__src.{ok_field})) {{").unwrap();
                writeln!(out, "        return __src;").unwrap();
                writeln!(out, "    }}").unwrap();
                writeln!(out, "    return ({src_c}){{ .tag = 1 }};").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "and_then" => {
                writeln!(out, "static inline {result_c} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{").unwrap();
                writeln!(out, "        return {call_fn}(&__fn, {cr}__src.{ok_field});").unwrap();
                writeln!(out, "    }}").unwrap();
                writeln!(out, "    return ({result_c}){{ .tag = 1 }};").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "or_else" => {
                writeln!(out, "static inline {src_c} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{").unwrap();
                writeln!(out, "        return __src;").unwrap();
                writeln!(out, "    }}").unwrap();
                // Result or_else passes the error value; Option or_else takes no args
                if *is_result {
                    writeln!(out, "    return {call_fn}(&__fn, {cr}__src.{err_field});").unwrap();
                } else {
                    writeln!(out, "    return {call_fn}(&__fn);").unwrap();
                }
                writeln!(out, "}}").unwrap();
            }
            "unwrap_err" | "unwrap_error" => {
                // Look up the actual error type from the struct
                let err_ty_c = module.structs.iter().find(|s| {
                    let c = sn.get(&(module.structs.iter().position(|x| std::ptr::eq(x, *s)).unwrap() as u32))
                        .cloned().unwrap_or_else(|| s.name.clone());
                    c == *src_c
                }).and_then(|s| s.fields.get(2))
                    .map(|(_, t)| c_type_named(t, sn))
                    .unwrap_or_else(|| "void*".to_string());
                writeln!(out, "static inline {err_ty_c} {full_name}(void* __res_ptr) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__res_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 1) {{").unwrap();
                writeln!(out, "        return __src.{err_field};").unwrap();
                writeln!(out, "    }}").unwrap();
                // D11: `.unwrap_error()` on an `Ok` is `T_UnwrapErrorOnOk` — route
                // through the registry (`gorget_trap`, exit 101), NOT a bare
                // `abort()` (exit 134, off the normalized set). The trailing return
                // is unreachable (`gorget_trap` exits) but keeps the non-void
                // function well-formed under `-Wreturn-type`.
                writeln!(out, "    gorget_trap(\"{}\", \"unwrap_error on Ok\");",
                    crate::trap::TrapKind::UnwrapErrorOnOk.code()).unwrap();
                writeln!(out, "    return __src.{err_field};").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "map_err" => {
                // Result__T__E__map_err(result*, closure) → Result__T__E2
                // if Ok: copy Ok field; if Error: apply closure to error payload, wrap in Error
                let result_err = if *result_c != *src_c {
                    // Cross-type: look up the error field name in the result struct
                    let (_, rerr) = enum_payload_fields(
                        module.structs.iter().find(|s| {
                            let cn = sn.get(&(module.structs.iter().position(|x| std::ptr::eq(x, *s)).unwrap() as u32))
                                .cloned().unwrap_or_else(|| s.name.clone());
                            cn == *result_c
                        }).map(|s| s.name.as_str()).unwrap_or(""),
                        module,
                    );
                    rerr
                } else {
                    err_field.clone()
                };
                writeln!(out, "static inline {result_c} {full_name}(void* __res_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__res_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{").unwrap();
                // Cross-type: copy the Ok value into the result struct
                if *result_c != *src_c {
                    let result_ok = {
                        let (rok, _) = enum_payload_fields(
                            module.structs.iter().find(|s| {
                                let cn = sn.get(&(module.structs.iter().position(|x| std::ptr::eq(x, *s)).unwrap() as u32))
                                    .cloned().unwrap_or_else(|| s.name.clone());
                                cn == *result_c
                            }).map(|s| s.name.as_str()).unwrap_or(""),
                            module,
                        );
                        rok
                    };
                    writeln!(out, "        {result_c} __ok_result; __ok_result.tag = 0; __ok_result.{result_ok} = __src.{ok_field};").unwrap();
                    writeln!(out, "        return __ok_result;").unwrap();
                } else {
                    writeln!(out, "        return __src;").unwrap();
                }
                writeln!(out, "    }}").unwrap();
                writeln!(out, "    {result_c} __result;").unwrap();
                writeln!(out, "    __result.tag = 1;").unwrap();
                // Use memcpy to handle Str/GorgetString layout-compatible type mismatches
                writeln!(out, "    {{ __auto_type __me_val = {call_fn}(&__fn, {cr}__src.{err_field}); memcpy(&__result.{result_err}, &__me_val, sizeof(__me_val)); }}").unwrap();
                writeln!(out, "    return __result;").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "or" => {
                // Option__T__or(opt*, other) → Option__T
                // if Some: return self; else return other
                writeln!(out, "static inline {src_c} {full_name}(void* __opt_ptr, {src_c} __other) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{ return __src; }}").unwrap();
                writeln!(out, "    return __other;").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "flatten" => {
                // Option__Option__T__flatten(opt*) → Option__T
                // if outer is Some and inner is Some: return inner; else None
                // result_c is the inner Option type
                writeln!(out, "static inline {result_c} {full_name}(void* __opt_ptr) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{ return __src.{ok_field}; }}").unwrap();
                writeln!(out, "    return ({result_c}){{ .tag = 1 }};").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "unwrap_or_else" => {
                // unwrap_or_else: if tag==0 (Some/Ok): return payload; else call closure
                // For Option: closure takes no args. For Result: closure takes error value.
                let payload_ty = module.structs.iter().find(|s| {
                    let cn = sn.get(&(module.structs.iter().position(|x| std::ptr::eq(x, *s)).unwrap() as u32))
                        .cloned().unwrap_or_else(|| s.name.clone());
                    cn == *src_c
                }).and_then(|s| s.fields.get(1))
                    .map(|(_, t)| c_type_named(t, sn))
                    .unwrap_or_else(|| "int64_t".to_string());
                writeln!(out, "static inline {payload_ty} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{ return __src.{ok_field}; }}").unwrap();
                if *is_result {
                    writeln!(out, "    return {call_fn}(&__fn, {cr}__src.{err_field});").unwrap();
                } else {
                    writeln!(out, "    return {call_fn}(&__fn);").unwrap();
                }
                writeln!(out, "}}").unwrap();
            }
            "flat_map" => {
                writeln!(out, "static inline {result_c} {full_name}(void* __opt_ptr, {closure_ty} __fn) {{").unwrap();
                writeln!(out, "    {src_c} __src = *({src_c}*)__opt_ptr;").unwrap();
                writeln!(out, "    if (__src.tag == 0) {{ return {call_fn}(&__fn, {cr}__src.{ok_field}); }}").unwrap();
                writeln!(out, "    return ({result_c}){{ .tag = 1 }};").unwrap();
                writeln!(out, "}}").unwrap();
            }
            "zip" => {
                // Option__T__zip(opt*, other) → Option__Tuple (not commonly used in tests, but cover it)
                writeln!(out, "// TODO: {full_name} (zip) not yet implemented").unwrap();
            }
            _ => {
                writeln!(out, "// TODO: {full_name} not yet implemented").unwrap();
            }
        }
        writeln!(out).unwrap();
    }
}
/// Generate blocking spawn/await helpers for each spawned function.
///
/// For each spawned function `foo`, generates:
/// - `Task__<RetType>` typedef (if not already emitted)
/// - `__SpawnCtx_foo` struct (GorgetTask base + params + result)
/// - `__spawn_run_foo()` — worker thread entry, calls the real function
/// - `__spawn_drop_foo()` — RAII cleanup (wait + free)
/// - `__gorget_spawn_foo()` — allocate ctx, init sync, submit to executor
/// - `__gorget_await_foo()` — wait, extract result, free
/// - `Task__<RetType>__drop()` — dispatch to per-fn drop via __drop pointer
pub(super) fn emit_spawn_helpers(out: &mut String, module: &LirModule) {
    writeln!(out, "/* ── Spawn/await helpers (M:N executor pool) ── */").unwrap();

    // Build orig→C name map for resolving spawn param types.
    let sn = build_struct_names(module);
    let orig_to_c: HashMap<String, String> = module.structs.iter().enumerate()
        .map(|(i, def)| (def.name.clone(), sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"))))
        .collect();
    let resolve_type = |t: &str| -> String {
        orig_to_c.get(t).cloned().unwrap_or_else(|| t.to_string())
    };

    // Emit Task__T typedefs for return types not already emitted by the early Task typedef pass.
    let mut emitted_task_types: Vec<String> = Vec::new();
    // Collect already-emitted Task types from module structs (early pass).
    for def in &module.structs {
        if def.name.starts_with("Task__") {
            emitted_task_types.push(def.name.clone());
        }
    }
    for sf in &module.spawned_fns {
        let task_name = if sf.ret_c_type == "void" {
            "Task__void".to_string()
        } else {
            format!("Task__{}", sf.ret_c_type)
        };
        if !emitted_task_types.contains(&task_name) {
            writeln!(out, "typedef struct {{ void* __task; void (*__drop)(void*); }} {task_name};").unwrap();
            emitted_task_types.push(task_name);
        }
    }
    writeln!(out).unwrap();

    for sf in &module.spawned_fns {
        let fn_name = &sf.fn_name;
        let safe_fn_name = c_func_name(fn_name);
        let ret_c = &sf.ret_c_type;
        let is_void = ret_c == "void";
        let ctx_name = format!("__SpawnCtx_{fn_name}");

        // Context struct
        writeln!(out, "typedef struct {ctx_name} {{").unwrap();
        writeln!(out, "    GorgetTask base;").unwrap();
        for (param_name, param_c_type) in &sf.params {
            let resolved = resolve_type(param_c_type);
            writeln!(out, "    {resolved} __{param_name};").unwrap();
        }
        if !is_void {
            let resolved_ret = resolve_type(ret_c);
            writeln!(out, "    {resolved_ret} result;").unwrap();
        }
        writeln!(out, "}} {ctx_name};").unwrap();

        // Run function — called by worker thread
        writeln!(out, "static void __spawn_run_{fn_name}(GorgetTask* __base) {{").unwrap();
        writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)__base;").unwrap();
        let call_args: Vec<String> = sf.params.iter().enumerate().map(|(i, (name, c_type))| {
            if sf.ref_param_indices.contains(&i) {
                format!("&__ctx->__{name}")
            } else if matches!(c_type.as_str(), "GorgetArray" | "GorgetMap" | "GorgetSet") {
                // Collection resource params are void* in the LIR function signature
                // but stored as the actual struct in the spawn context.
                format!("(void*)&__ctx->__{name}")
            } else if matches!(c_type.as_str(), "Str" | "GorgetString") {
                // String params: check if the target function takes void* (Ptr) or Str (by value).
                // Find the target function's param type.
                let target_fn = module.functions.iter().find(|f| f.name == sf.fn_name);
                let target_param_is_ptr = target_fn
                    .and_then(|f| f.params.get(i))
                    .map_or(true, |p| p.is_ptr()); // default to Ptr if unknown
                if target_param_is_ptr {
                    format!("(void*)&__ctx->__{name}")
                } else {
                    format!("__ctx->__{name}")
                }
            } else {
                format!("__ctx->__{name}")
            }
        }).collect();
        let call_str = call_args.join(", ");
        if is_void {
            writeln!(out, "    {safe_fn_name}({call_str});").unwrap();
        } else {
            writeln!(out, "    __ctx->result = {safe_fn_name}({call_str});").unwrap();
        }
        writeln!(out, "}}").unwrap();

        // Drop helper
        writeln!(out, "static void __spawn_drop_{fn_name}(void* __ptr) {{").unwrap();
        writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)__ptr;").unwrap();
        writeln!(out, "    GORGET_SCHEDULER_WAIT(&__ctx->base);").unwrap();
        writeln!(out, "    pthread_mutex_destroy(&__ctx->base.mtx);").unwrap();
        writeln!(out, "    pthread_cond_destroy(&__ctx->base.cond);").unwrap();
        writeln!(out, "    GORGET_FREE(__ctx, sizeof({ctx_name}));").unwrap();
        writeln!(out, "}}").unwrap();

        // Spawn function — returns Task__T (matches GIR behavior).
        // When the LIR destination is a Task struct, the caller uses the struct directly.
        // When the LIR destination is void* (non-vector case), the call site wraps it.
        let task_type_name = if is_void { "Task__void".to_string() } else { format!("Task__{ret_c}") };
        let param_decls: Vec<String> = sf.params.iter().map(|(name, c_type)| {
            let resolved = resolve_type(c_type);
            format!("{resolved} {name}")
        }).collect();
        let param_decl_str = param_decls.join(", ");
        writeln!(out, "static inline {task_type_name} __gorget_spawn_{fn_name}({param_decl_str}) {{").unwrap();
        writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)GORGET_CALLOC(1, sizeof({ctx_name}));").unwrap();
        writeln!(out, "    __ctx->base.run = __spawn_run_{fn_name};").unwrap();
        writeln!(out, "    pthread_mutex_init(&__ctx->base.mtx, NULL);").unwrap();
        writeln!(out, "    pthread_cond_init(&__ctx->base.cond, NULL);").unwrap();
        for (i, (param_name, _c_type)) in sf.params.iter().enumerate() {
            // Clone refcounted params (Channel, Shared, Weak) to avoid dangling pointers.
            if let Some((_, gir_name)) = sf.clone_params.iter().find(|(idx, _)| *idx == i) {
                writeln!(out, "    __ctx->__{param_name} = {gir_name}__clone({param_name});").unwrap();
            } else {
                writeln!(out, "    __ctx->__{param_name} = {param_name};").unwrap();
            }
        }
        writeln!(out, "    GORGET_SCHEDULER_SUBMIT(&__ctx->base);").unwrap();
        writeln!(out, "    return ({task_type_name}){{.__task = __ctx, .__drop = __spawn_drop_{fn_name}}};").unwrap();
        writeln!(out, "}}").unwrap();

        // Await function — takes Task__T by value, extracts __task to get SpawnCtx.
        let resolved_ret = resolve_type(ret_c);
        if is_void {
            writeln!(out, "static inline void __gorget_await_{fn_name}({task_type_name} task) {{").unwrap();
        } else {
            writeln!(out, "static inline {resolved_ret} __gorget_await_{fn_name}({task_type_name} task) {{").unwrap();
        }
        writeln!(out, "    {ctx_name}* __ctx = ({ctx_name}*)task.__task;").unwrap();
        writeln!(out, "    GORGET_SCHEDULER_WAIT(&__ctx->base);").unwrap();
        if !is_void {
            writeln!(out, "    {resolved_ret} result = __ctx->result;").unwrap();
        }
        writeln!(out, "    pthread_mutex_destroy(&__ctx->base.mtx);").unwrap();
        writeln!(out, "    pthread_cond_destroy(&__ctx->base.cond);").unwrap();
        writeln!(out, "    GORGET_FREE(__ctx, sizeof({ctx_name}));").unwrap();
        if !is_void {
            writeln!(out, "    return result;").unwrap();
        }
        writeln!(out, "}}").unwrap();
        writeln!(out).unwrap();
    }

    // Task__T__drop for each unique Task type
    let mut emitted_task_drops: Vec<String> = Vec::new();
    for sf in &module.spawned_fns {
        let task_name = if sf.ret_c_type == "void" {
            "Task__void".to_string()
        } else {
            format!("Task__{}", sf.ret_c_type)
        };
        if emitted_task_drops.contains(&task_name) {
            continue;
        }
        emitted_task_drops.push(task_name.clone());
        writeln!(out, "static inline void {task_name}__drop({task_name}* self) {{").unwrap();
        writeln!(out, "    if (self && self->__task && self->__drop) {{").unwrap();
        writeln!(out, "        self->__drop(self->__task);").unwrap();
        writeln!(out, "        self->__task = NULL;").unwrap();
        writeln!(out, "    }}").unwrap();
        writeln!(out, "}}").unwrap();
        writeln!(out, "static void (*__unused_{task_name}__drop)({task_name}*) __attribute__((unused)) = {task_name}__drop;").unwrap();
        writeln!(out).unwrap();

        // Task__void__await — value-routed await for VOID tasks pulled out of a
        // collection, where the GIR await dispatcher cannot resolve a single
        // monomorphic __gorget_await_<fn> name (because the Task[void] TypeId
        // maps to multiple DISTINCT producer fns). For a void task, await ==
        // join + destroy + free, which is byte-identical to the per-instance
        // __drop the task already carries. So we route through the value's own
        // __drop pointer (per-value provenance, no name matching). Takes the
        // Task by value (await consumes it; the GIR move-zeroes the source so
        // scope-exit won't re-drop). Emitted INSIDE this loop so it reuses the
        // per-type `emitted_task_drops` dedup (exactly once), gated on the void
        // Task type. See methods.rs / exprs/mod.rs await dispatch.
        if task_name == "Task__void" {
            writeln!(out, "static inline void Task__void__await(Task__void task) {{").unwrap();
            writeln!(out, "    if (task.__task && task.__drop) {{").unwrap();
            writeln!(out, "        task.__drop(task.__task);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "}}").unwrap();
            writeln!(out).unwrap();
        }
    }
}
pub(super) fn emit_thread_helpers(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    if module.thread_spawned_fns.is_empty() {
        return;
    }
    writeln!(out, "\n/* ── Thread[T] wrappers ── */").unwrap();

    // The SYMBOL names (`Thread__{ret_name}` / `__GorgetThread__{ret_name}`)
    // come from the typed `ret_name` written through from the spawn intrinsic
    // — they must match the call sites byte-for-byte. The C TYPE of the
    // `_result` field / join return comes from `ret_c_type`, resolved through
    // the struct-name map for user struct/enum payloads (`Point` → `__gg_Point`),
    // mirroring emit_spawn_helpers' `resolve_type`.
    let orig_to_c: HashMap<String, String> = module.structs.iter().enumerate()
        .map(|(i, def)| (def.name.clone(), sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"))))
        .collect();
    let resolve_type = |t: &str| -> String {
        orig_to_c.get(t).cloned().unwrap_or_else(|| t.to_string())
    };

    // Collect unique return types for Thread__T typedefs
    let mut emitted_thread_types: Vec<String> = Vec::new();
    for tsf in &module.thread_spawned_fns {
        let ret_name = &tsf.ret_name;
        let ret_c = resolve_type(&tsf.ret_c_type);
        let is_void = ret_name == "void";
        let thread_name = format!("Thread__{ret_name}");
        if emitted_thread_types.contains(&thread_name) {
            continue;
        }
        emitted_thread_types.push(thread_name.clone());
        let ctx_type = format!("__GorgetThread__{ret_name}");
        if is_void {
            writeln!(out, "typedef struct {{ pthread_t _thr; }} {ctx_type};").unwrap();
        } else {
            writeln!(out, "typedef struct {{ pthread_t _thr; {ret_c} _result; }} {ctx_type};").unwrap();
        }
        writeln!(out, "typedef {ctx_type}* {thread_name};").unwrap();
        // id(self) -> int64_t
        writeln!(out, "static inline int64_t {thread_name}__id({thread_name} self) {{ return (int64_t)(uintptr_t)self->_thr; }}").unwrap();
        // join(self) -> T
        if is_void {
            writeln!(out, "static inline void {thread_name}__join({thread_name} self) {{ pthread_join(self->_thr, NULL); GORGET_FREE(self, sizeof(*self)); }}").unwrap();
        } else {
            writeln!(out, "static inline {ret_c} {thread_name}__join({thread_name} self) {{ pthread_join(self->_thr, NULL); {ret_c} _r = self->_result; GORGET_FREE(self, sizeof(*self)); return _r; }}").unwrap();
        }
        writeln!(out).unwrap();
    }

    // Per-function thread entry + spawn helpers
    for tsf in &module.thread_spawned_fns {
        let fn_name = &tsf.fn_name;
        let safe_fn_name = c_func_name(fn_name);
        let ret_name = &tsf.ret_name;
        let is_void = ret_name == "void";
        let thread_name = format!("Thread__{ret_name}");
        let ctx_type = format!("__GorgetThread__{ret_name}");

        // Thread entry
        writeln!(out, "static void* __gorget_thread_entry_{fn_name}(void* __arg) {{").unwrap();
        writeln!(out, "    {ctx_type}* __ctx = ({ctx_type}*)__arg;").unwrap();
        if is_void {
            writeln!(out, "    {safe_fn_name}();").unwrap();
        } else {
            writeln!(out, "    __ctx->_result = {safe_fn_name}();").unwrap();
        }
        writeln!(out, "    return NULL;\n}}").unwrap();

        // Spawn function. stack_size 0 => plain pthread_create (byte-identical to the
        // pre-stack-size emit); non-zero => a pthread_attr-sized wrapper.
        writeln!(out, "static inline {thread_name} __gorget_thread_spawn_{fn_name}(void) {{").unwrap();
        writeln!(out, "    {ctx_type}* __ctx = ({ctx_type}*)GORGET_CALLOC(1, sizeof({ctx_type}));").unwrap();
        if tsf.stack_size != 0 {
            let stack_size = tsf.stack_size;
            writeln!(out, "    pthread_attr_t __attr;").unwrap();
            writeln!(out, "    pthread_attr_init(&__attr);").unwrap();
            writeln!(out, "    pthread_attr_setstacksize(&__attr, {stack_size});").unwrap();
            writeln!(out, "    pthread_create(&__ctx->_thr, &__attr, __gorget_thread_entry_{fn_name}, __ctx);").unwrap();
            writeln!(out, "    pthread_attr_destroy(&__attr);").unwrap();
        } else {
            writeln!(out, "    pthread_create(&__ctx->_thr, NULL, __gorget_thread_entry_{fn_name}, __ctx);").unwrap();
        }
        writeln!(out, "    return __ctx;\n}}").unwrap();
        writeln!(out).unwrap();
    }
}
/// Rewrite GIR local references (`_N`) in inline C code to LIR slot names (`__sN`).
pub(super) fn rewrite_inline_c_locals(code: &str, func: &LirFunction) -> String {
    // Simple regex-free approach: find `_N` patterns and replace with `__sN`.
    let mut result = String::with_capacity(code.len() + 16);
    let bytes = code.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'_' && (i == 0 || !bytes[i-1].is_ascii_alphanumeric()) {
            // Check if followed by digits
            let start = i + 1;
            let mut end = start;
            while end < bytes.len() && bytes[end].is_ascii_digit() {
                end += 1;
            }
            if end > start && (end >= bytes.len() || !bytes[end].is_ascii_alphanumeric()) {
                let num: u32 = code[start..end].parse().unwrap_or(0);
                // Map GIR local index to LIR slot if possible
                if (num as usize) < func.slots.len() {
                    result.push_str(&format!("__s{}", num));
                } else {
                    result.push('_');
                    result.push_str(&code[start..end]);
                }
                i = end;
                continue;
            }
        }
        let ch = code[i..].chars().next().unwrap();
        result.push(ch);
        i += ch.len_utf8();
    }
    result
}
pub(super) fn emit_global_init(out: &mut String, init: &LirGlobalInit, ty: &LirType, funcs: &[LirFunction], structs: &[StructDef], struct_names: &HashMap<u32, String>) {
    write!(out, " = ").unwrap();
    emit_global_init_value(out, init, ty, funcs, structs, struct_names);
}

pub(super) fn emit_global_init_value(out: &mut String, init: &LirGlobalInit, ty: &LirType, funcs: &[LirFunction], structs: &[StructDef], struct_names: &HashMap<u32, String>) {
    match init {
        // Pointer-typed zeroed slot (e.g. the NULL-degraded vtable `__drop`
        // slot for an unresolvable concrete) → spell `NULL`, not a braced
        // scalar initializer. Aggregates and other scalars keep `{0}`.
        LirGlobalInit::Zeroed if matches!(ty, LirType::Ptr) => write!(out, "NULL").unwrap(),
        LirGlobalInit::Zeroed => write!(out, "{{0}}").unwrap(),
        LirGlobalInit::Bytes(b) => {
            let is_float = matches!(ty, LirType::F32 | LirType::F64);
            match (b.len(), is_float) {
                (4, true) => {
                    let val = f32::from_le_bytes([b[0], b[1], b[2], b[3]]);
                    if val.is_finite() {
                        write!(out, "{val:.17e}").unwrap();
                    } else {
                        write!(out, "{val}").unwrap();
                    }
                }
                (8, true) => {
                    let val = f64::from_le_bytes([b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7]]);
                    if val.is_finite() {
                        write!(out, "{val:.17e}").unwrap();
                    } else {
                        write!(out, "{val}").unwrap();
                    }
                }
                (1, _) => write!(out, "{}", b[0] as i8).unwrap(),
                (2, _) => write!(out, "{}", i16::from_le_bytes([b[0], b[1]])).unwrap(),
                (4, _) => write!(out, "{}", i32::from_le_bytes([b[0], b[1], b[2], b[3]])).unwrap(),
                (8, _) => write!(out, "{}LL", i64::from_le_bytes([b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7]])).unwrap(),
                _ => write!(out, "{{0}} /* {} bytes */", b.len()).unwrap(),
            }
        }
        LirGlobalInit::FuncAddr(fid) => {
            let fname = funcs.get(fid.0 as usize).map(|f| f.name.as_str()).unwrap_or("__unknown_fn");
            write!(out, "(void*)&{fname}").unwrap();
        }
        LirGlobalInit::BoxDropAddr(inner) => {
            // Trait-object vtable drop slot. The `Box__<inner>__drop` wrapper
            // is emitted by `emit_box_drop_wrappers` (discovered via the typed
            // `StructDef.box_inner_type` registered at `emit_vtable_globals`)
            // and forward-declared in `emit_runtime_helpers` ahead of the
            // globals. Symbol spelling at the C-emit boundary, driven by the
            // typed inner-type name.
            write!(out, "(void*)&Box__{inner}__drop").unwrap();
        }
        LirGlobalInit::Struct { struct_id, fields } => {
            write!(out, "{{").unwrap();
            let field_types: Option<&[(String, LirType)]> = structs.get(struct_id.0 as usize)
                .map(|sd| sd.fields.as_slice());
            for (i, f) in fields.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                let ft = field_types.and_then(|fts| fts.get(i).map(|(_, t)| t)).unwrap_or(&LirType::I64);
                emit_global_init_value(out, f, ft, funcs, structs, struct_names);
            }
            write!(out, "}}").unwrap();
        }
        LirGlobalInit::Extern { name, args } => {
            // Module-level string literal: `String FOO = "literal"` lowers
            // to `Extern { name: "gorget_str_from_literal", args: [StrLit, Int] }`
            // targeting a `GorgetString` slot. The runtime call would heap-
            // allocate via `str_alloc_copy`; instead emit a static struct
            // initializer that points into the C `.rodata` section. Layout
            // mirrors the per-call-site `__slit_N` views the backend already
            // emits for inline string literals — `cap = 0` marks the buffer
            // as non-owning so `gorget_string_free` is a no-op and reads of
            // the global require no clone.
            if is_str_literal_view_init(name, args, ty, structs) {
                if let (LirGlobalInitArg::StrLit(text), LirGlobalInitArg::Int(len)) = (&args[0], &args[1]) {
                    let escaped = crate::backend::c_lir::helpers::escape_c_string(text);
                    write!(
                        out,
                        "{{ .data = (char*)\"{escaped}\", .cap = 0, .len = {len}, .alloc = NULL }}"
                    ).unwrap();
                    return;
                }
            }
            // Runtime-initialized globals are populated by a constructor
            // call emitted at main()'s prologue. The compile-time
            // declaration just zero-inits the slot.
            write!(out, "{{0}}").unwrap();
        }
        LirGlobalInit::StaticArrayView { elem_ty, elems } => {
            // R34 Track A: a `cap = 0` GorgetArray view over a file-scope
            // compound-literal backing buffer. A compound literal at file
            // scope has static storage duration (C11 §6.5.2.5p5), so its
            // address is valid for the whole program — no separate backing
            // decl and no startup allocation. `cap == 0` marks the buffer
            // non-owning: `gorget_array_free` is a no-op and reads clone-out
            // on demand. elem_drop / elem_clone / elem_materialize default to
            // NULL via designated-init zeroing — safe for a view (never freed;
            // the IndexLoad read path clones through the element type's own
            // `__clone` for Recursive/collection/string elements).
            let elem_c = c_type_named(elem_ty, struct_names);
            if elems.is_empty() {
                // An empty compound literal `(T[]){}` is not valid ISO C (a
                // GNU extension that breaks under -pedantic-errors). Use a
                // NULL data pointer — a zero-length view never dereferences it.
                write!(
                    out,
                    "{{ .data = NULL, .cap = 0, .len = 0, .elem_size = sizeof({elem_c}), .alloc = NULL }}"
                ).unwrap();
            } else {
                write!(out, "{{ .data = ({elem_c}[]){{").unwrap();
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 { write!(out, ", ").unwrap(); }
                    emit_global_init_value(out, e, elem_ty, funcs, structs, struct_names);
                }
                write!(
                    out,
                    "}}, .cap = 0, .len = {}, .elem_size = sizeof({elem_c}), .alloc = NULL }}",
                    elems.len()
                ).unwrap();
            }
        }
    }
}
/// Map LirType to C type string.
/// Returns true if the function is provided by standard C headers
/// (stdio.h, stdlib.h, string.h) and should not be re-declared.
/// Emit a coerced argument value.
/// Emit an argument with explicit ABI marshalling. Returns true if handled,
/// false if the caller should fall back to existing logic.
pub(super) fn emit_abi_arg(
    out: &mut String,
    val: &str,
    abi: crate::ir::abi::AbiKind,
    arg_ty: Option<&LirType>,
    is_str_lit: bool,
) -> bool {
    use crate::ir::abi::AbiKind;
    let is_ptr = arg_ty.map_or(false, |t| t.is_ptr());
    let is_struct = arg_ty.map_or(false, |t| t.is_aggregate());
    match abi {
        AbiKind::CStr => {
            // Under 32-byte Str, extract .data for const char* params.
            if is_str_lit || is_struct {
                write!(out, "(const char*){val}.data").unwrap();
            } else if is_ptr {
                write!(out, "({val} ? gorget_str_to_cstr(*(Str*){val}) : NULL)").unwrap();
            } else {
                // Scalar/void reaching CStr is likely a misclassified ABI tag.
                // CStr should only receive Str structs (extract .data), Ptr-to-Str
                // (deref + extract), or raw const char* (pass through).
                debug_assert!(
                    false,
                    "CStr ABI received non-Str, non-Ptr value '{val}' (type {arg_ty:?}). \
                     This param should probably be Opaque, not CStr."
                );
                write!(out, "{val}").unwrap();
            }
            true
        }
        AbiKind::BytePtr => {
            if is_str_lit || is_struct {
                write!(out, "(const char*){val}.data").unwrap();
            } else if is_ptr {
                write!(out, "({val} ? (const char*)((Str*){val})->data : NULL)").unwrap();
            } else {
                debug_assert!(
                    false,
                    "BytePtr ABI received non-Str, non-Ptr value '{val}' (type {arg_ty:?}). \
                     This param should probably be Opaque, not BytePtr."
                );
                write!(out, "{val}").unwrap();
            }
            true
        }
        AbiKind::GorgetString => {
            // GorgetString should receive: StrLit (Str struct), Ptr (deref to Str), or Struct (pass through).
            debug_assert!(
                is_str_lit || is_ptr || is_struct,
                "GorgetString ABI received non-Str, non-Ptr value '{val}' (type {arg_ty:?}). \
                 Scalars should use Scalar, raw pointers should use Opaque."
            );
            if is_str_lit {
                write!(out, "{val}").unwrap();
            } else if is_ptr {
                write!(out, "*(Str*){val}").unwrap();
            } else {
                write!(out, "{val}").unwrap();
            }
            true
        }
        AbiKind::ByValue => {
            // Aggregate by value. If struct, pass through. If Ptr, fall back to
            // emit_coerced_arg which can deref using ext_param type info.
            if is_struct || is_str_lit {
                write!(out, "{val}").unwrap();
                return true;
            }
            // Ptr or scalar — fall back so the cascade can deref with correct type.
            return false;
        }
        AbiKind::Ptr => {
            // Ptr should receive: Struct (take address) or Ptr (pass through).
            // Scalars reaching Ptr means the ABI tag is wrong — scalars should use Scalar.
            debug_assert!(
                is_struct || is_ptr || arg_ty.map_or(true, |t| matches!(t, LirType::Void)),
                "Ptr ABI received scalar value '{val}' (type {arg_ty:?}). \
                 Scalars should use Scalar, not Ptr."
            );
            if is_struct {
                write!(out, "&{val}").unwrap();
            } else {
                write!(out, "{val}").unwrap();
            }
            true
        }
        AbiKind::Opaque | AbiKind::Scalar => {
            write!(out, "{val}").unwrap();
            true
        }
        AbiKind::VoidElem => {
            // void* element parameter: wrap non-pointer values with &(Type){val}.
            if is_str_lit {
                // StrLit (const char*) → take address for memcpy: &__vN.
                write!(out, "&{val}").unwrap();
            } else if is_ptr {
                // Already a pointer (SlotAddr, gorget_array_get result, etc.) — pass through.
                write!(out, "{val}").unwrap();
            } else if is_struct {
                // Struct value — take its address directly.
                write!(out, "&{val}").unwrap();
            } else {
                // Scalar — compound literal with the correct type to preserve bits.
                let c_ty = match arg_ty {
                    Some(LirType::F64) => "double",
                    Some(LirType::F32) => "float",
                    Some(LirType::Bool) => "_Bool",
                    Some(LirType::I32) => "int32_t",
                    Some(LirType::I16) => "int16_t",
                    Some(LirType::I8)  => "int8_t",
                    Some(LirType::U8)  => "uint8_t",
                    Some(LirType::U16) => "uint16_t",
                    Some(LirType::U32) => "uint32_t",
                    Some(LirType::U64) => "uint64_t",
                    _ => "int64_t",
                };
                write!(out, "&({c_ty}){{{val}}}").unwrap();
            }
            true
        }
        AbiKind::OutPtr => {
            // Output pointer: the argument is already the address of the
            // destination slot the callee writes into (a SlotAddr value).
            // Pass it through unchanged — identical marshalling to a
            // passthrough `Ptr`. The semantic meaning of `OutPtr` (callee
            // initializes the pointee) is consumed by drop-elaboration, not
            // by the C marshalling here.
            write!(out, "{val}").unwrap();
            true
        }
        AbiKind::Auto => false, // fall back to existing logic
    }
}

/// Handles: Ptr→Str (string literal wrapping), Ptr→Aggregate (dereference), GorgetString→Str.
///
/// Reads per-value origin info via `func.value_origins` (Phase D6) — no
/// parallel `str_lit_vals` bitmap parameter required.
pub(super) fn emit_coerced_arg(
    out: &mut String,
    a: &ValueId,
    param_ty: Option<&LirType>,
    val_types: &[Option<LirType>],
    func: &LirFunction,
    sn: &HashMap<u32, String>,
) {
    let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
    let is_str_lit = matches!(
        func.value_origins.get(a.0 as usize).and_then(|o| o.as_ref()),
        Some(ValueOrigin::StrLit)
    );
    let param_name = param_ty.map(|t| c_type_named(t, sn));
    let arg_name = arg_ty.map(|t| c_type_named(t, sn));

    // GorgetString ↔ Str coercion — both are the same 32-byte struct, identity.
    if (param_name.as_deref() == Some("Str") && arg_name.as_deref() == Some("GorgetString"))
        || (param_name.as_deref() == Some("GorgetString") && arg_name.as_deref() == Some("Str"))
    {
        write!(out, "{}", format!("__v{}", a.0)).unwrap();
        return;
    }

    // Str struct → char* param: extract .data (for printf format strings, legacy C FFI).
    // The Str's data pointer is valid and NUL-terminated for owned/literal strings.
    if matches!(param_ty, Some(LirType::Ptr))
        && (arg_name.as_deref() == Some("Str") || arg_name.as_deref() == Some("GorgetString"))
    {
        write!(out, "(const char*)__v{}.data", a.0).unwrap();
        return;
    }
    // PtrTo(Str) → void* param: pass the pointer directly (it's already void*).
    if param_ty.map_or(false, |t| t.is_ptr()) && arg_ty.map_or(false, |t| matches!(t, LirType::PtrTo(_))) {
        write!(out, "__v{}", a.0).unwrap();
        return;
    }

    if param_ty.map_or(false, |t| t.is_aggregate()) && arg_ty.map_or(false, |t| t.is_ptr()) {
        let ty_name = param_name.as_deref().unwrap_or("void");
        if is_str_lit && ty_name == "Str" {
            write!(out, "gorget_str_from_literal({v}, strlen({v}))", v = format!("__v{}", a.0)).unwrap();
        } else if is_str_lit && ty_name == "GorgetString" {
            // String literal → GorgetString: wrap with gorget_string_new.
            write!(out, "gorget_string_new({})", format!("__v{}", a.0)).unwrap();
        } else if ty_name == "Str" {
            // Ptr to Str (from SlotAddr of GorgetString slot?) — try coercion.
            write!(out, "*({ty_name}*)__v{}", a.0).unwrap();
        } else {
            write!(out, "*({ty_name}*)__v{}", a.0).unwrap();
        }
    }
    // Str struct arg → unknown callee (no param_ty info): extract .data for const char*.
    // This is the catch-all for runtime functions like gorget_file_open, gorget_file_write,
    // gorget_socket_write_str, etc. that take const char* but receive Str structs.
    else if param_ty.is_none() && (arg_name.as_deref() == Some("Str") || arg_name.as_deref() == Some("GorgetString")) {
        write!(out, "(const char*)__v{}.data", a.0).unwrap();
    }
    else {
        write!(out, "__v{}", a.0).unwrap();
    }
}
/// Returns true if the LIR type is a GorgetString struct.
pub(super) fn is_gorget_string_type(ty: Option<&LirType>, sn: &HashMap<u32, String>) -> bool {
    if let Some(LirType::Struct(sid)) = ty {
        let name = sn.get(&sid.0).map(|s| s.as_str()).unwrap_or("");
        name == "GorgetString"
    } else {
        false
    }
}
/// Resolve the actual drop function to CALL for a struct/enum field of type
/// `field_type_name`, whose drop was recorded as the bare `{Field}__drop` by
/// `populate_recursive_drop_structs`.
///
/// When the field's type has a user `Drop` impl (`DropStrategy::Custom`), the
/// recorded `{Field}__drop` is the BARE user destructor — it runs the user
/// body but does NOT drop the field type's OWN resource fields. Calling it for
/// a nested field would leak those inner resources (e.g. an `Inner{Vector}`
/// field of an `Outer` struct: `Outer__drop` calling bare `Inner__drop` leaks
/// the Vector). The unified `__gorget_dtor_{Field}` runs the user drop THEN the
/// field's field-drops — the same glue a top-level Custom-drop site invokes
/// (`src/lir/lower/drops.rs:318-327`). Return it so the field-drop CALL routes
/// through it; `None` means "keep the recorded drop fn".
///
/// The recorded `field_drops` value is deliberately left as the bare
/// `{Field}__drop` so the clone-side emitters (which key on the `__drop` suffix
/// to derive `{Field}__clone`) are unaffected — this is purely a drop-call
/// rewrite, not a metadata change.
fn field_drop_call_name(module: &LirModule, field_type_name: &str) -> Option<String> {
    let info = module.type_drop_fns.get(field_type_name)?;
    if info.user_drop_fn.is_some() {
        Some(info.drop_fn_name.clone())
    } else {
        None
    }
}

/// True when `field_type_name` is a registered trait-object Box (`is_trait_box`).
fn field_is_trait_box(module: &LirModule, field_type_name: &str) -> bool {
    module.structs.iter()
        .find(|s| s.name == field_type_name)
        .map_or(false, |s| s.is_trait_box)
}

/// Emit the C statements that drop a trait-box field (16B TraitObj) via its
/// vtable `__drop` slot. The concrete `Box__<Concrete>__drop` takes the
/// address of the data pointer (void** slot) — same ABI as LIR drops.rs.
/// Used by struct/enum field-drop emitters (Round XIX N2 cell D rider).
fn emit_trait_box_field_drop(out: &mut String, field_access: &str) {
    use std::fmt::Write;
    write!(out,
        "if (({field_access}).vtable && ({field_access}).vtable->__drop) {{ \
         ({field_access}).vtable->__drop((void*)&({field_access}).data); }} "
    ).unwrap();
}

/// Refcount-handle wrapper families (`Shared` / `Weak` / `Channel`) whose
/// struct/enum field is a thin pointer to a shared control block. Cloning the
/// CONTAINING aggregate must RETAIN (refcount++) the handle so the copy's drop
/// — which RELEASES via `{Family}__<mono>__drop` — is balanced. Without the
/// retain the clone and its source share one control block that is decremented
/// twice but incremented once → premature free / double-free / UAF (the
/// Shared-struct-field-clone under-incref class; the same asymmetry hit both
/// lanes — SH's sibling was fixed in `lir_codegen.gg`'s `field_clone_c`).
///
/// Returns the BY-VALUE runtime retain symbol, invoked `field = retain(field);`
/// (NOT `retain(&field)`, and NOT the `{Family}__<mono>__clone` wrapper: that
/// wrapper takes `self` by value too, but is emitted only on demand and would
/// still need a by-value call). This contrasts the deep-clone fns
/// (`gorget_string_clone_to_owned`, `X__clone`) which are by-POINTER
/// (`clone(&field)`). The field's C type is a thin-pointer typedef
/// (`typedef GorgetShared* Shared__<mono>`), so no cast is needed.
///
/// Detection keys off the `{Family}__` prefix on the drop wrapper — the
/// established runtime-symbol contract at the C-emit boundary (the whole
/// wrapper subsystem routes by it: `is_monomorphized_wrapper_type`,
/// `is_wrapper_method`, `parse_shared_method`; same basis as the `Box__`
/// mangling). This is the Core #2 name-matching exception for runtime symbols,
/// not a semantic decision made upstream.
///
/// Rc/Arc were removed (A2) — no arm. EVERY family whose field drop wrapper
/// RELEASES a refcount MUST appear here; `refcount_clone_arm_symmetry`
/// (tests/lints.rs) enforces that every clone-synthesis path consults this.
fn refcount_field_retain_fn(drop_fn: &str) -> Option<&'static str> {
    if !drop_fn.ends_with("__drop") {
        return None;
    }
    if drop_fn.starts_with("Shared__") {
        Some("gorget_shared_clone")
    } else if drop_fn.starts_with("Weak__") {
        Some("gorget_weak_clone")
    } else if drop_fn.starts_with("Channel__") {
        Some("gorget_channel_retain")
    } else {
        None
    }
}
/// Emit inline tag-checked clones for Option fields containing resources.
/// Drop-side intentionally does nothing — Option types have DropStrategy::None
/// to avoid double-free with match/unwrap paths, and struct-field drops rely
/// on that. Clone-side deep-copies because cloning is always safe.
fn emit_option_field_clones(
    out: &mut String,
    sdef: &crate::lir::StructDef,
    already_handled: &std::collections::HashSet<String>,
    module: &crate::lir::LirModule,
) {
    for (fname, fty) in &sdef.fields {
        if already_handled.contains(fname) { continue; }
        if let crate::lir::LirType::Struct(fsid) = fty {
            if let Some(fdef) = module.structs.get(fsid.0 as usize) {
                if fdef.enum_kind == crate::lir::EnumKind::Option {
                    for (vfname, vfty) in &fdef.fields {
                        if vfname == "tag" { continue; }
                        if let crate::lir::LirType::Struct(vfsid) = vfty {
                            if let Some(vfdef) = module.structs.get(vfsid.0 as usize) {
                                let clone_fn = match vfdef.name.as_str() {
                                    "GorgetString" => Some("gorget_string_clone_to_owned"),
                                    "GorgetArray"  => Some("gorget_array_clone"),
                                    "GorgetMap"    => Some("gorget_map_clone"),
                                    "GorgetSet"    => Some("gorget_set_clone"),
                                    _ => None,
                                };
                                if let Some(cfn) = clone_fn {
                                    writeln!(out, "    if (dst.{fname}.tag != 0) {{ dst.{fname}.{vfname} = {cfn}(&dst.{fname}.{vfname}); }}").unwrap();
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// When a struct has fields that need dropping (e.g., GorgetString), the drop
/// elaboration marks it as Recursive. When that struct appears as a field in
/// another struct, the parent's drop emits a call to `{Name}__drop`. This
/// function generates the actual `{Name}__drop` function body.
pub(super) fn emit_recursive_struct_drops(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    // Forward-declare every recursive-struct drop fn before emitting any body.
    // LIR-struct iteration order is not a topological order over the drop-call
    // graph: a parent struct `A { B field }` whose `A__drop` calls `B__drop`
    // may be emitted before `B`. Without forward decls, the C compiler errors
    // on the call to the not-yet-declared `B__drop`. The prelude makes the
    // emission-order question moot.
    for (idx, sdef) in module.structs.iter().enumerate() {
        let type_name = &sdef.name;
        if !module.recursive_drop_structs.contains_key(type_name.as_str()) {
            continue;
        }
        let drop_fn_name = format!("{type_name}__drop");
        // Skip if a user-defined drop already exists (it'll have its own decl)
        if module.functions.iter().any(|f| f.name == drop_fn_name) {
            continue;
        }
        let _ = idx;
        writeln!(out, "static inline void {drop_fn_name}(void* __p);").unwrap();
    }
    writeln!(out).unwrap();

    for (idx, sdef) in module.structs.iter().enumerate() {
        let type_name = &sdef.name;

        // Check if this is a struct that needs a Recursive drop function
        let drop_info = match module.recursive_drop_structs.get(type_name.as_str()) {
            Some(info) => info,
            None => continue,
        };

        // Check if a drop function already exists (custom Drop trait impl)
        let drop_fn_name = format!("{type_name}__drop");
        if module.functions.iter().any(|f| f.name == drop_fn_name) {
            continue;
        }

        // Use the C struct name (e.g., __lir_s10) instead of the Gorget name
        let c_name = sn.get(&(idx as u32)).cloned().unwrap_or_else(|| type_name.clone());

        // Generate the drop function.
        // NOTE: Option/Result fields are intentionally NOT dropped here — they
        // have DropStrategy::None to avoid double-free with match/unwrap paths.
        // The clone function DOES deep-copy them to prevent CoW aliasing.
        // Signature is `void(void*)`, not `void(T*)`, so storing the function
        // pointer in a runtime drop slot (typed `void(*)(void*)` — element /
        // value / key drop in GorgetArray / GorgetMap) and dispatching through
        // it round-trips per C11 6.3.2.3p8. Direct-call sites pass `&parent->field`
        // (typed `T*`) which implicitly converts to `void*` at the parameter,
        // so this signature change is callwise-compatible.
        writeln!(out, "static inline void {drop_fn_name}(void* __p) {{").unwrap();
        writeln!(out, "    {c_name}* self = ({c_name}*)__p;").unwrap();
        for (field_name, drop_fn, field_type_name) in drop_info {
            // Round XIX Track N2 Class A rider: trait-box fields are a 16-byte
            // {data, vtable} TraitObj, NOT a void* to free. Drop via the
            // vtable `__drop` slot (same shape as LIR drops.rs for locals) —
            // calling free(&field) is free(invalid pointer) (cell D).
            if field_is_trait_box(module, field_type_name) {
                write!(out, "    ").unwrap();
                emit_trait_box_field_drop(out, &format!("self->{field_name}"));
                writeln!(out).unwrap();
                continue;
            }
            let call = field_drop_call_name(module, field_type_name)
                .unwrap_or_else(|| drop_fn.clone());
            writeln!(out, "    {call}((void*)&self->{field_name});").unwrap();
        }
        writeln!(out, "}}").unwrap();
        writeln!(out).unwrap();
    }
}
/// Emit per-type clone functions for structs with Recursive drop strategy.
/// These produce independently-owned deep copies by memcpy + per-field clone.
/// Called from collection reads (IndexLoad, Option unwrap) so extracted elements
/// don't share resource field buffers with the collection.
pub(super) fn emit_recursive_struct_clones(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    // Forward-declare every recursive-struct clone fn before any body, for the
    // same topological reason as `emit_recursive_struct_drops`. A parent's
    // clone body may call a child's clone before the child is declared if
    // LIR struct iteration order isn't a topo order over the field-clone graph.
    for (idx, sdef) in module.structs.iter().enumerate() {
        let type_name = &sdef.name;
        if !module.recursive_drop_structs.contains_key(type_name.as_str()) {
            continue;
        }
        let clone_fn_name = format!("{type_name}__clone");
        if module.functions.iter().any(|f| f.name == clone_fn_name) {
            continue;
        }
        let c_name = sn.get(&(idx as u32)).cloned().unwrap_or_else(|| type_name.clone());
        writeln!(out, "{c_name} {clone_fn_name}(void* __p);").unwrap();
    }
    writeln!(out).unwrap();

    for (idx, sdef) in module.structs.iter().enumerate() {
        let type_name = &sdef.name;

        let drop_info = match module.recursive_drop_structs.get(type_name.as_str()) {
            Some(info) => info,
            None => continue,
        };

        // Skip if a user-defined clone already exists
        let clone_fn_name = format!("{type_name}__clone");
        if module.functions.iter().any(|f| f.name == clone_fn_name) {
            continue;
        }

        let c_name = sn.get(&(idx as u32)).cloned().unwrap_or_else(|| type_name.clone());

        // Generate: TypeName__clone(void* __p) → T with deep-cloned resource fields
        // NOT static — the IndexLoad path emits a non-static extern declaration.
        // Null-safe: return zero struct if __p is null (from uninitialized Ptr locals).
        writeln!(out, "{c_name} {clone_fn_name}(void* __p) {{").unwrap();
        writeln!(out, "    if (!__p) {{ {c_name} z = {{0}}; return z; }}").unwrap();
        writeln!(out, "    {c_name} dst = *({c_name}*)__p;").unwrap();
        for (field_name, drop_fn, _field_type_name) in drop_info {
            // Map drop function → clone function
            let clone_fn = match drop_fn.as_str() {
                // Clone to owned: CoW materializations must produce independently-owned
                // copies. The MoveZero gap that required view-preserving clones is fixed.
                "gorget_string_free" => "gorget_string_clone_to_owned",
                "gorget_array_free" => "gorget_array_clone",
                "gorget_map_free" => "gorget_map_clone",
                "gorget_set_free" => "gorget_set_clone",
                "gorget_closure_free" => "gorget_closure_clone_to_owned",
                d if d.starts_with("Box__") && d.ends_with("__drop") => {
                    // Box[T] field: allocate a fresh heap slot and deep-clone the
                    // inner T into it. Mirrors the enum-variant Box-clone branch.
                    // The shallow `dst = *src` already copied the box pointer; we
                    // overwrite it with a fresh allocation so dst owns independently.
                    //
                    // Trait boxes (`is_trait_box`): the shallow `dst = *src` already
                    // copied the 16-byte {data, vtable} TraitObj. Do NOT route
                    // through `__gorget_box_alloc_<Trait>` — the "inner" is a trait
                    // name with no concrete layout (Round XIX N2 cell E: Speaker
                    // undeclared). The data-box ownership is shared via the
                    // TraitObj handle; independent deep-clone of the data box is
                    // a separate future surface.
                    let box_type_name = &d[..d.len() - "__drop".len()];
                    let is_trait_box = module.structs.iter()
                        .find(|s| s.name == box_type_name)
                        .map_or(false, |s| s.is_trait_box);
                    if is_trait_box {
                        continue;
                    }
                    let inner = &d["Box__".len()..d.len() - "__drop".len()];
                    let alloc_fn = format!("__gorget_box_alloc_{inner}");
                    let inner_c_name = module.structs.iter().enumerate()
                        .find(|(_, s)| s.name == inner)
                        .and_then(|(i, _)| sn.get(&(i as u32)).cloned())
                        .unwrap_or_else(|| inner.to_string());
                    writeln!(
                        out,
                        "    dst.{field_name} = {alloc_fn}(*({inner_c_name}*)dst.{field_name});"
                    ).unwrap();
                    let has_inner_clone = module.recursive_drop_structs.contains_key(inner)
                        || module.recursive_drop_enums.contains_key(inner);
                    if has_inner_clone {
                        writeln!(
                            out,
                            "    {inner}__clone_inplace(dst.{field_name});"
                        ).unwrap();
                    }
                    continue;
                }
                d if refcount_field_retain_fn(d).is_some() => {
                    // Refcount-handle field (Shared/Weak/Channel): RETAIN by value
                    // to balance the containing struct's drop, which RELEASES via
                    // the `{Family}__<mono>__drop` wrapper. Emitting nothing here
                    // (the pre-fix behavior) shallow-copied the handle → the copy's
                    // drop underflowed the refcount → UAF. See refcount_field_retain_fn.
                    let retain = refcount_field_retain_fn(d).unwrap();
                    writeln!(out, "    dst.{field_name} = {retain}(dst.{field_name});").unwrap();
                    continue;
                }
                other if other.ends_with("__drop") => {
                    // Recursive or Custom-drop field: call its clone function if it exists.
                    // For Recursive fields, __clone is generated by this same pass.
                    // For Custom-drop fields, use deep_clone_resource_fields inline.
                    let base = &other[..other.len() - 6]; // strip "__drop"
                    let inner_clone = format!("{base}__clone");
                    // Check if this inner type also has a Recursive clone (will be generated)
                    if module.recursive_drop_structs.contains_key(base)
                        || module.recursive_drop_enums.contains_key(base)
                    {
                        writeln!(out, "    dst.{field_name} = {inner_clone}(&dst.{field_name});").unwrap();
                        continue;
                    }
                    // Custom-drop field: clone resource fields inline via deep_clone_resource_fields
                    if let Some((inner_sid, _)) = module.structs.iter().enumerate()
                        .find(|(_, s)| s.name == base)
                    {
                        if let Some(ops) = deep_clone_resource_fields(
                            crate::lir::StructId(inner_sid as u32),
                            &format!("dst.{field_name}"),
                            module,
                        ) {
                            for op in ops {
                                writeln!(out, "    {op}").unwrap();
                            }
                        }
                    }
                    continue;
                }
                _ => continue, // Unknown drop — skip cloning this field
            };
            writeln!(out, "    dst.{field_name} = {clone_fn}(&dst.{field_name});").unwrap();
        }
        // Inline clone for Option fields containing resources (mirrors drop logic).
        {
            let already_cloned: std::collections::HashSet<String> = drop_info.iter()
                .map(|(f, _, _)| f.clone())
                .collect();
            emit_option_field_clones(out, &module.structs[idx], &already_cloned, module);
        }
        writeln!(out, "    return dst;").unwrap();
        writeln!(out, "}}").unwrap();
        // In-place wrapper for use as elem_clone/val_clone function pointer.
        writeln!(out, "void {clone_fn_name}_inplace(void* __p) {{ *({c_name}*)__p = {clone_fn_name}(__p); }}").unwrap();
        writeln!(out).unwrap();
    }
}
/// Emit per-type clone functions for ENUM types with Recursive drop.
/// Uses tag-based dispatch to clone the active variant's resource fields.
pub(super) fn emit_recursive_enum_clones(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    for (idx, sdef) in module.structs.iter().enumerate() {
        let type_name = &sdef.name;

        let variant_info = match module.recursive_drop_enums.get(type_name.as_str()) {
            Some(info) => info,
            None => continue,
        };

        // Skip if a user-defined clone already exists
        let clone_fn_name = format!("{type_name}__clone");
        if module.functions.iter().any(|f| f.name == clone_fn_name) {
            continue;
        }

        let c_name = sn.get(&(idx as u32)).cloned().unwrap_or_else(|| type_name.clone());

        // Map drop function → clone function
        fn drop_to_clone(drop_fn: &str) -> String {
            match drop_fn {
                // Always use clone_to_owned: enum clones must independently own all
                // string data because gorget_string_free is called on drop.
                "gorget_string_free" => "gorget_string_clone_to_owned".into(),
                "gorget_array_free" => "gorget_array_clone".into(),
                "gorget_map_free" => "gorget_map_clone".into(),
                "gorget_set_free" => "gorget_set_clone".into(),
                "gorget_closure_free" => "gorget_closure_clone_to_owned".into(),
                // Box[T] enum-variant field: route to the box-alloc + inner-clone
                // emission path (see line ~1249 below). The synthetic
                // "__gorget_box_clone" name is a marker — there's no actual
                // function with that name; the emitter pattern-matches on it.
                "free" => "__gorget_box_clone".into(),
                d if d.starts_with("Box__") && d.ends_with("__drop") => "__gorget_box_clone".into(),
                other if other.ends_with("__drop") => {
                    let base = &other[..other.len() - 6];
                    format!("{base}__clone")
                }
                _ => String::new(),
            }
        }

        writeln!(out, "{c_name} {clone_fn_name}(void* __p) {{").unwrap();
        writeln!(out, "    if (!__p) {{ {c_name} z = {{0}}; return z; }}").unwrap();
        writeln!(out, "    {c_name} dst = *({c_name}*)__p;").unwrap();
        writeln!(out, "    switch (dst.tag) {{").unwrap();

        // Group variant_info by variant index
        let mut by_variant: std::collections::HashMap<u32, Vec<(&str, &str, &str, &str)>> = std::collections::HashMap::new();
        for (vi, vname, field_name, drop_fn, field_type_name) in variant_info {
            by_variant.entry(*vi).or_default().push((vname, field_name, drop_fn, field_type_name));
        }

        let mut indices: Vec<u32> = by_variant.keys().copied().collect();
        indices.sort();
        for vi in indices {
            let fields = &by_variant[&vi];
            write!(out, "        case {vi}: ").unwrap();
            for (variant_name, field_name, drop_fn, _field_type_name) in fields {
                // Refcount-handle variant payload (Shared/Weak/Channel): RETAIN by
                // value to balance the variant drop's RELEASE. Same asymmetry class
                // as the struct-clone path; see refcount_field_retain_fn.
                if let Some(retain) = refcount_field_retain_fn(drop_fn) {
                    let variant_prefix = format!("{variant_name}_");
                    let variant_field_count = sdef.fields.iter()
                        .filter(|(n, _)| n.starts_with(&variant_prefix))
                        .count();
                    let access = if sdef.is_union_layout && variant_field_count > 1 {
                        format!("data.{variant_name}.{field_name}")
                    } else if sdef.is_union_layout {
                        format!("data.{field_name}")
                    } else {
                        field_name.to_string()
                    };
                    write!(out, "dst.{access} = {retain}(dst.{access}); ").unwrap();
                    continue;
                }
                let clone_fn = drop_to_clone(drop_fn);
                // Only emit clone call if the function is a known runtime clone OR
                // will be generated (exists in recursive_drop_structs/enums).
                // Handle types like Task with Trivial drop but no clone are left
                // as shallow copies (from the initial `dst = *(Type*)__p`).
                let clone_exists = matches!(clone_fn.as_str(),
                    "gorget_string_clone_to_owned" | "gorget_array_clone"
                    | "gorget_map_clone" | "gorget_set_clone")
                    || clone_fn.ends_with("__clone") && {
                        let base = &clone_fn[..clone_fn.len() - 7];
                        module.recursive_drop_structs.contains_key(base)
                            || module.recursive_drop_enums.contains_key(base)
                            || module.functions.iter().any(|f| f.name == clone_fn)
                    };
                if !clone_fn.is_empty() && (clone_fn == "__gorget_box_clone" || clone_exists) {
                    let variant_prefix = format!("{variant_name}_");
                    let variant_field_count = sdef.fields.iter()
                        .filter(|(n, _)| n.starts_with(&variant_prefix))
                        .count();
                    let access = if sdef.is_union_layout && variant_field_count > 1 {
                        format!("data.{variant_name}.{field_name}")
                    } else if sdef.is_union_layout {
                        format!("data.{field_name}")
                    } else {
                        field_name.to_string()
                    };
                    if clone_fn == "__gorget_box_clone" {
                        // Box: alloc new box, copy content, deep-clone content.
                        // Trait boxes: shallow 16B TraitObj copy already done by
                        // `dst = *src` — skip alloc of the trait name (cell E).
                        let is_trait_box = module.structs.iter()
                            .find(|s| s.name == *_field_type_name)
                            .map_or(false, |s| s.is_trait_box);
                        if !is_trait_box {
                            let inner_type = _field_type_name.strip_prefix("Box__").unwrap_or(_field_type_name);
                            let inner_clone = format!("{inner_type}__clone_inplace");
                            let has_inner_clone = module.recursive_drop_structs.contains_key(inner_type)
                                || module.recursive_drop_enums.contains_key(inner_type);
                            let alloc_fn = format!("__gorget_box_alloc_{inner_type}");
                            let inner_c_name = module.structs.iter().enumerate()
                                .find(|(_, s)| s.name == inner_type)
                                .and_then(|(i, _)| sn.get(&(i as u32)).cloned())
                                .unwrap_or_else(|| inner_type.to_string());
                            write!(out, "dst.{access} = {alloc_fn}(*({inner_c_name}*)dst.{access}); ").unwrap();
                            if has_inner_clone {
                                write!(out, "{inner_clone}(dst.{access}); ").unwrap();
                            }
                        }
                    } else {
                        write!(out, "dst.{access} = {clone_fn}(&dst.{access}); ").unwrap();
                    }
                }
            }
            writeln!(out, "break;").unwrap();
        }

        writeln!(out, "    }}").unwrap();
        writeln!(out, "    return dst;").unwrap();
        writeln!(out, "}}").unwrap();
        writeln!(out, "void {clone_fn_name}_inplace(void* __p) {{ *({c_name}*)__p = {clone_fn_name}(__p); }}").unwrap();
        writeln!(out).unwrap();
    }
}
/// Emit drop functions for enums with resource-type variant payloads.
/// These are called explicitly from the GIR reassignment path for
/// enums that have needs_drop=true but DropStrategy::None.
pub(super) fn emit_enum_drop_fns(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    use std::fmt::Write;
    for (idx, sdef) in module.structs.iter().enumerate() {
        let type_name = &sdef.name;
        let variant_info = match module.recursive_drop_enums.get(type_name.as_str()) {
            Some(info) => info,
            None => continue,
        };
        let drop_fn_name = format!("{type_name}__drop");
        // Skip if already generated by emit_recursive_struct_drops or user-defined
        if module.functions.iter().any(|f| f.name == drop_fn_name) {
            continue;
        }
        // Skip types that have a real DropStrategy (already handled)
        // We only want enums with None strategy that have resource payloads
        // These are NOT in recursive_drop_structs (that's for structs)
        if module.recursive_drop_structs.contains_key(type_name.as_str()) {
            continue;
        }
        let c_name = sn.get(&(idx as u32)).cloned().unwrap_or_else(|| type_name.clone());
        let mut by_variant: std::collections::HashMap<u32, Vec<(&str, &str, &str, &str)>> = std::collections::HashMap::new();
        for (vi, vname, field_name, drop_fn, field_type_name) in variant_info {
            by_variant.entry(*vi).or_default().push((vname, field_name, drop_fn, field_type_name));
        }
        if by_variant.is_empty() { continue; }
        writeln!(out, "void {drop_fn_name}(void* __p) {{").unwrap();
        writeln!(out, "    {c_name}* self = ({c_name}*)__p;").unwrap();
        writeln!(out, "    switch (self->tag) {{").unwrap();
        let mut indices: Vec<u32> = by_variant.keys().copied().collect();
        indices.sort();
        for vi in indices {
            let fields = &by_variant[&vi];
            write!(out, "        case {vi}: ").unwrap();
            for (variant_name, field_name, drop_fn, field_type_name) in fields {
                let variant_prefix = format!("{variant_name}_");
                let variant_field_count = sdef.fields.iter()
                    .filter(|(n, _)| n.starts_with(&variant_prefix))
                    .count();
                let access = if sdef.is_union_layout && variant_field_count > 1 {
                    format!("data.{variant_name}.{field_name}")
                } else if sdef.is_union_layout {
                    format!("data.{field_name}")
                } else {
                    field_name.to_string()
                };
                // Self-cleaning: gorget_array_free/gorget_map_free drop elements.
                // Trait-box: vtable __drop (NOT free of the 16B field).
                // Non-trait Box via the wrapper routes through `&self->field`.
                if field_is_trait_box(module, field_type_name) {
                    emit_trait_box_field_drop(out, &format!("self->{access}"));
                } else if *drop_fn == "free" {
                    write!(out, "free(self->{access}); ").unwrap();
                } else {
                    let call = field_drop_call_name(module, field_type_name)
                        .unwrap_or_else(|| (*drop_fn).to_string());
                    write!(out, "{call}((void*)&self->{access}); ").unwrap();
                }
            }
            writeln!(out, "break;").unwrap();
        }
        writeln!(out, "    }}").unwrap();
        writeln!(out, "}}").unwrap();
        writeln!(out).unwrap();
    }
}
/// Determine the inner-T drop function name for a Box[T] wrapper, given
/// the LIR type-name suffix that appears after `Box__` in `__gorget_box_alloc_<inner>`.
///
/// Returns `None` for primitive / driveless inners (int, bool, etc.) where
/// the wrapper just frees the heap allocation without recursing.
fn box_inner_drop_fn(inner: &str, module: &LirModule) -> Option<String> {
    // Resolve the runtime-backing StructDef via the alias map
    // (Vector__T → GorgetArray, Dict__K__V → GorgetMap, etc.) and read
    // the typed `elem_drop_fn` — the canonical "uniform `void(void*)` free
    // function for this resource type" entry set at builtin registration
    // (`src/lir/types.rs:83/98/151/178`). Replaces four parallel
    // `name.starts_with(...)` prefix matches and a hardcoded
    // `c_runtime_alias == "GorgetClosure" → "gorget_closure_free"` shortcut.
    if let Some(sd) = module.struct_def_by_name(inner) {
        if let Some(ref drop_fn) = sd.elem_drop_fn {
            return Some(drop_fn.clone());
        }
        // Indirect via c_runtime_alias for monomorphizations whose alias
        // target carries the elem_drop_fn (Callable family → GorgetClosure).
        if let Some(ref alias) = sd.c_runtime_alias {
            if let Some(alias_sd) = module.struct_def_by_name(alias) {
                if let Some(ref drop_fn) = alias_sd.elem_drop_fn {
                    return Some(drop_fn.clone());
                }
            }
        }
    }
    // Nested Box[Box[T]]: route through the inner Box's wrapper. The inner
    // box's heap allocation is itself a slot containing the next-level
    // pointer — Box__T__drop takes a slot pointer, which is exactly what
    // the heap allocation is.
    if inner.starts_with("Box__") {
        return Some(format!("{inner}__drop"));
    }
    // User-defined types with a generated `T__drop`: structs/enums whose
    // payload contains resource fields.
    if module.recursive_drop_structs.contains_key(inner)
        || module.recursive_drop_enums.contains_key(inner)
    {
        return Some(format!("{inner}__drop"));
    }
    if module.type_drop_fns.contains_key(inner) {
        let info = &module.type_drop_fns[inner];
        return Some(info.drop_fn_name.clone());
    }
    // Primitive (int/bool/float/...) or no resources: nothing to recurse.
    None
}

/// Emit per-type Box__T__drop wrappers for every monomorphized Box[T].
///
/// Each wrapper takes a slot pointer (matching the void(*)(void*) ABI used
/// by collection elem_drop / val_drop / key_drop slots and by struct/enum
/// recursive-drop helpers). It reads the box pointer from the slot, no-ops
/// on null (drained slot), recurses into the inner T's drop, frees the box
/// heap allocation through __gorget_box_free_T (so --clone-stats accounting
/// stays balanced), and zeros the slot.
///
/// Called AFTER all per-type T__drop functions are emitted so the recursion
/// targets resolve at C-link time. The matching forward declarations are
/// emitted in `emit_runtime_helpers`.
pub(super) fn emit_box_drop_wrappers(out: &mut String, module: &LirModule) {
    use std::fmt::Write;
    let mut box_inners: Vec<String> = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    for ext in &module.externs {
        if let Some(inner) = ext.name.strip_prefix("__gorget_box_alloc_") {
            if seen.insert(inner.to_string()) {
                box_inners.push(inner.to_string());
            }
        }
    }
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let crate::lir::Inst::CallExtern { name, .. } = inst {
                    if let Some(inner) = name.strip_prefix("__gorget_box_alloc_") {
                        if seen.insert(inner.to_string()) {
                            box_inners.push(inner.to_string());
                        }
                    }
                }
            }
        }
    }
    // Read the typed `StructDef.box_inner_type` so the per-type
    // `Box__<inner>__drop` wrapper is emitted for every registered
    // Box[T] alias, including those reached only through generated
    // recursive-clone/drop helper text (no direct
    // `__gorget_box_alloc_T` Inst).
    for sd in &module.structs {
        if let Some(inner) = &sd.box_inner_type {
            if seen.insert(inner.clone()) {
                box_inners.push(inner.clone());
            }
        }
    }
    if box_inners.is_empty() {
        return;
    }
    box_inners.sort();
    for inner in &box_inners {
        let inner_drop = box_inner_drop_fn(inner, module);
        writeln!(out, "void Box__{inner}__drop(void* slot) {{").unwrap();
        writeln!(out, "    void** sp = (void**)slot;").unwrap();
        writeln!(out, "    void* p = *sp;").unwrap();
        writeln!(out, "    if (!p) return;").unwrap();
        if let Some(d) = inner_drop {
            writeln!(out, "    {d}(p);").unwrap();
        }
        writeln!(out, "    __gorget_box_free_{inner}(p);").unwrap();
        writeln!(out, "    *sp = NULL;").unwrap();
        writeln!(out, "}}").unwrap();
    }
    writeln!(out).unwrap();
}

/// Emit unified drop/clone functions from type_drop_fns.
/// Generates Type__drop(void*) for every type with droppable fields.
/// Skips types that already have a drop function from the old generators or user code.
pub(super) fn emit_type_drop_fns(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    use std::fmt::Write;
    for (idx, sdef) in module.structs.iter().enumerate() {
        let type_name = &sdef.name;
        let info = match module.type_drop_fns.get(type_name.as_str()) {
            Some(i) => i,
            None => continue,
        };

        let c_name = sn.get(&(idx as u32)).cloned().unwrap_or_else(|| type_name.clone());

        // --- Drop function ---
        // Skip if the exact function name already exists. For mangled names
        // (__gorget_dtor_*), always generate — the old generators don't produce them.
        let already_has_drop = module.functions.iter().any(|f| f.name == info.drop_fn_name)
            || (!info.drop_fn_name.starts_with("__gorget_dtor_") && (
                module.recursive_drop_structs.contains_key(type_name.as_str())
                || module.recursive_drop_enums.contains_key(type_name.as_str())
            ));
        if !already_has_drop {
            if let Some(ref variants) = info.enum_variants {
                // Enum drop: switch on tag
                writeln!(out, "void {}(void* __p) {{", info.drop_fn_name).unwrap();
                writeln!(out, "    {c_name}* self = ({c_name}*)__p;").unwrap();
                if let Some(ref user_fn) = info.user_drop_fn {
                    writeln!(out, "    {user_fn}(__p);").unwrap();
                }
                writeln!(out, "    switch (self->tag) {{").unwrap();
                let mut by_variant: std::collections::HashMap<u32, Vec<(&str, &str, &str, &str)>> = std::collections::HashMap::new();
                for (vi, vname, field_name, drop_fn, ftn) in variants {
                    by_variant.entry(*vi).or_default().push((vname, field_name, drop_fn, ftn));
                }
                let mut indices: Vec<u32> = by_variant.keys().copied().collect();
                indices.sort();
                for vi in indices {
                    let fields = &by_variant[&vi];
                    write!(out, "        case {vi}: ").unwrap();
                    for (variant_name, field_name, drop_fn, ftn) in fields {
                        let variant_prefix = format!("{variant_name}_");
                        let variant_field_count = sdef.fields.iter()
                            .filter(|(n, _)| n.starts_with(&variant_prefix))
                            .count();
                        let access = if sdef.is_union_layout && variant_field_count > 1 {
                            format!("data.{variant_name}.{field_name}")
                        } else if sdef.is_union_layout {
                            format!("data.{field_name}")
                        } else {
                            field_name.to_string()
                        };
                        if field_is_trait_box(module, ftn) {
                            emit_trait_box_field_drop(out, &format!("self->{access}"));
                        } else if *drop_fn == "free" {
                            write!(out, "free(self->{access}); ").unwrap();
                        } else {
                            let call = field_drop_call_name(module, ftn)
                                .unwrap_or_else(|| (*drop_fn).to_string());
                            write!(out, "{call}((void*)&self->{access}); ").unwrap();
                        }
                    }
                    writeln!(out, "break;").unwrap();
                }
                writeln!(out, "    }}").unwrap();
                writeln!(out, "}}").unwrap();
                writeln!(out).unwrap();
            } else {
                // Struct drop: call per-field drops
                writeln!(out, "void {}(void* __p) {{", info.drop_fn_name).unwrap();
                writeln!(out, "    {c_name}* self = ({c_name}*)__p;").unwrap();
                if let Some(ref user_fn) = info.user_drop_fn {
                    writeln!(out, "    {user_fn}(__p);").unwrap();
                }
                for (field_name, drop_fn, ftn) in &info.field_drops {
                    if field_is_trait_box(module, ftn) {
                        write!(out, "    ").unwrap();
                        emit_trait_box_field_drop(out, &format!("self->{field_name}"));
                        writeln!(out).unwrap();
                    } else if drop_fn == "free" {
                        writeln!(out, "    free(self->{field_name});").unwrap();
                    } else {
                        let call = field_drop_call_name(module, ftn)
                            .unwrap_or_else(|| drop_fn.clone());
                        writeln!(out, "    {call}(&self->{field_name});").unwrap();
                    }
                }
                writeln!(out, "}}").unwrap();
                writeln!(out).unwrap();
            }
        }

        // --- Clone function ---
        let clone_fn_name = format!("{type_name}__clone");
        let already_has_clone = module.functions.iter().any(|f| f.name == clone_fn_name)
            || module.recursive_drop_structs.contains_key(type_name.as_str())
            || module.recursive_drop_enums.contains_key(type_name.as_str());
        if !already_has_clone {
            fn drop_to_clone_fn(drop_fn: &str) -> Option<String> {
                match drop_fn {
                    "gorget_string_free" => Some("gorget_string_clone_to_owned".into()),
                    "gorget_array_free" => Some("gorget_array_clone".into()),
                    "gorget_map_free" => Some("gorget_map_clone".into()),
                    "gorget_set_free" => Some("gorget_set_clone".into()),
                    "gorget_closure_free" => Some("gorget_closure_clone_to_owned".into()),
                    other if other.ends_with("__drop") => {
                        let base = &other[..other.len() - 6];
                        Some(format!("{base}__clone"))
                    }
                    other if other.starts_with("__gorget_dtor_") => {
                        let base = &other["__gorget_dtor_".len()..];
                        Some(format!("{base}__clone"))
                    }
                    _ => None,
                }
            }

            if let Some(ref variants) = info.enum_variants {
                // Enum clone (null-safe)
                writeln!(out, "{c_name} {clone_fn_name}(void* __p) {{").unwrap();
                writeln!(out, "    if (!__p) {{ {c_name} z = {{0}}; return z; }}").unwrap();
                writeln!(out, "    {c_name} dst = *({c_name}*)__p;").unwrap();
                writeln!(out, "    switch (dst.tag) {{").unwrap();
                let mut by_variant: std::collections::HashMap<u32, Vec<(&str, &str, &str, &str)>> = std::collections::HashMap::new();
                for (vi, vname, fname, dfn, ftn) in variants {
                    by_variant.entry(*vi).or_default().push((vname, fname, dfn, ftn));
                }
                let mut indices: Vec<u32> = by_variant.keys().copied().collect();
                indices.sort();
                for vi in indices {
                    let fields = &by_variant[&vi];
                    write!(out, "        case {vi}: ").unwrap();
                    for (variant_name, field_name, drop_fn, _ftn) in fields {
                        let retain = refcount_field_retain_fn(drop_fn);
                        if retain.is_none() && drop_to_clone_fn(drop_fn).is_none() {
                            continue;
                        }
                        let variant_prefix = format!("{variant_name}_");
                        let variant_field_count = sdef.fields.iter()
                            .filter(|(n, _)| n.starts_with(&variant_prefix))
                            .count();
                        let access = if sdef.is_union_layout && variant_field_count > 1 {
                            format!("data.{variant_name}.{field_name}")
                        } else if sdef.is_union_layout {
                            format!("data.{field_name}")
                        } else {
                            field_name.to_string()
                        };
                        if let Some(retain) = retain {
                            // Refcount-handle variant payload: RETAIN by value.
                            write!(out, "dst.{access} = {retain}(dst.{access}); ").unwrap();
                        } else {
                            let cfn = drop_to_clone_fn(drop_fn).unwrap();
                            write!(out, "dst.{access} = {cfn}(&dst.{access}); ").unwrap();
                        }
                    }
                    writeln!(out, "break;").unwrap();
                }
                writeln!(out, "    }}").unwrap();
                writeln!(out, "    return dst;").unwrap();
                writeln!(out, "}}").unwrap();
                writeln!(out, "void {clone_fn_name}_inplace(void* __p) {{ *({c_name}*)__p = {clone_fn_name}(__p); }}").unwrap();
                writeln!(out).unwrap();
            } else {
                // Struct clone (null-safe)
                writeln!(out, "{c_name} {clone_fn_name}(void* __p) {{").unwrap();
                writeln!(out, "    if (!__p) {{ {c_name} z = {{0}}; return z; }}").unwrap();
                writeln!(out, "    {c_name} dst = *({c_name}*)__p;").unwrap();
                for (field_name, drop_fn, _ftn) in &info.field_drops {
                    // Refcount-handle field: RETAIN by value (mirror the release drop).
                    if let Some(retain) = refcount_field_retain_fn(drop_fn) {
                        writeln!(out, "    dst.{field_name} = {retain}(dst.{field_name});").unwrap();
                        continue;
                    }
                    if let Some(cfn) = drop_to_clone_fn(drop_fn) {
                        writeln!(out, "    dst.{field_name} = {cfn}(&dst.{field_name});").unwrap();
                    }
                }
                writeln!(out, "    return dst;").unwrap();
                writeln!(out, "}}").unwrap();
                writeln!(out, "void {clone_fn_name}_inplace(void* __p) {{ *({c_name}*)__p = {clone_fn_name}(__p); }}").unwrap();
                writeln!(out).unwrap();
            }
        }
    }
}
/// Emit typedefs and inline wrappers for monomorphized wrapper types
/// (Channel__T, Shared__T, Weak__T, AtomicInt, AtomicBool).
pub(super) fn emit_monomorphized_typedefs(out: &mut String, module: &LirModule, sn: &HashMap<u32, String>) {
    let mut type_seen = std::collections::HashSet::new();
    let mut method_seen = std::collections::HashSet::new();
    // Build original-name → C-name map for resolving element types in wrappers.
    let orig_to_c: HashMap<String, String> = module.structs.iter().enumerate()
        .map(|(i, def)| (def.name.clone(), sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"))))
        .collect();

    // Collect all wrapper type names from struct defs, struct_names, and spawned_fns.
    let mut type_names: Vec<String> = Vec::new();
    for def in &module.structs {
        if is_monomorphized_wrapper_type(&def.name) && type_seen.insert(def.name.clone()) {
            type_names.push(def.name.clone());
        }
    }
    for name in sn.values() {
        if is_monomorphized_wrapper_type(name) && type_seen.insert(name.clone()) {
            type_names.push(name.clone());
        }
    }
    for sf in &module.spawned_fns {
        for n in std::iter::once(&sf.ret_c_type).chain(sf.params.iter().map(|(_, t)| t)) {
            if is_monomorphized_wrapper_type(n) && type_seen.insert(n.clone()) {
                type_names.push(n.clone());
            }
        }
    }

    // Emit typedefs (skip unmonomorphized wrappers like Guard__T)
    for name in &type_names {
        if is_unmonomorphized_wrapper(name) { continue; }
        emit_wrapper_typedef(out, name, module, &orig_to_c);
    }

    // Collect all Channel/Shared/Weak/Mutex/RWLock method names from CallExtern instructions.
    let mut method_calls: Vec<String> = Vec::new();
    let is_wrapper_method = |n: &str| -> bool {
        n.starts_with("Channel__") || n.starts_with("Shared__")
        || n.starts_with("Weak__") || n.starts_with("Mutex__")
        || n.starts_with("RWLock__") || n.starts_with("Guard__")
        || n.starts_with("ReadGuard__") || n.starts_with("WriteGuard__")
        || n.starts_with("Box__")
    };
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                match inst {
                    Inst::CallExtern { name, .. } => {
                        if is_wrapper_method(name) && method_seen.insert(name.clone()) {
                            method_calls.push(name.clone());
                        }
                    }
                    // A per-mono wrapper `__drop` / `__clone` referenced ONLY as
                    // a function-POINTER value (never called by name) — the
                    // element-drop / element-clone slot of a freshly built
                    // collection whose element is a refcount handle, emitted by
                    // `emit_collection_fn_ptr_stores` as `NamedFuncAddr`. The
                    // `CallExtern` scan above misses it (it is a taken address,
                    // not a call), so `Vector[Shared[T]] v = [Shared[T](x)]`
                    // (the ONLY reference to `Shared__T__drop` is the array's
                    // elem_drop slot) link-failed on the undefined symbol. Same
                    // reference-site-completeness gap the recursive-drop-table
                    // walk below closes for inlined drop bodies.
                    Inst::NamedFuncAddr { name, .. } => {
                        if is_wrapper_method(name) && method_seen.insert(name.clone()) {
                            method_calls.push(name.clone());
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    // Also scan externs list
    for ext in &module.externs {
        if is_wrapper_method(&ext.name) && method_seen.insert(ext.name.clone()) {
            method_calls.push(ext.name.clone());
        }
    }
    // Synthesize clone method calls for refcounted types captured by spawn helpers.
    for sf in &module.spawned_fns {
        for (_idx, gir_name) in &sf.clone_params {
            let clone_name = format!("{gir_name}__clone");
            if method_seen.insert(clone_name.clone()) {
                method_calls.push(clone_name);
            }
        }
    }
    // Walk recursive-drop tables so per-mono Shared/Weak/Channel/Mutex/
    // RWLock/Guard `__drop` wrappers referenced only from inlined drop-fn
    // bodies get emitted. The inline drop-fn bodies are NOT in the
    // `Inst::CallExtern` instruction stream (they're emitted directly into
    // C by `emit_recursive_struct_drops` / `emit_enum_drop_fns`), so the
    // function/extern scans above can't pick them up. Without this, a
    // `Recursive` struct/enum that holds a `Shared[Vector[T]]`-class field
    // links-fails on the undefined `Shared__Vector__T__drop` symbol.
    //
    // Box is excluded — it has its own slot-ABI emission path through
    // `emit_box_drop_wrappers` (driven by `__gorget_box_alloc_<inner>`
    // externs and `StructDef.box_inner_type`). Routing it through the
    // wrapper-method scan as well would emit a second `static inline`
    // self-ABI variant under the same `Box__T__drop` symbol, colliding at
    // link.
    //
    // This closes the per-mono wrapper-emission dependency-tracking gap
    // identified by the Tier 1c `monomorphize_struct` migration revert
    // (commit `a59faf33`).
    let is_non_box_wrapper = |n: &str| is_wrapper_method(n) && !n.starts_with("Box__");
    for field_drops in module.recursive_drop_structs.values() {
        for (_field, drop_fn, _ty) in field_drops {
            if is_non_box_wrapper(drop_fn) && method_seen.insert(drop_fn.clone()) {
                method_calls.push(drop_fn.clone());
            }
        }
    }
    for variant_drops in module.recursive_drop_enums.values() {
        for (_idx, _variant, _field, drop_fn, _ty) in variant_drops {
            if is_non_box_wrapper(drop_fn) && method_seen.insert(drop_fn.clone()) {
                method_calls.push(drop_fn.clone());
            }
        }
    }

    // First pass: discover types from method calls and emit all typedefs.
    // Also discover and typedef element types (e.g., Vector__int64_t inside Shared__Vector__int64_t).
    for name in &method_calls {
        let type_prefix = if let Some((tp, _)) = parse_channel_method(name) {
            Some(tp)
        } else if let Some((tp, _)) = parse_shared_method(name) {
            Some(tp)
        } else if let Some((tp, _)) = parse_weak_method(name) {
            Some(tp)
        } else if let Some((tp, _)) = parse_mutex_method(name) {
            Some(tp)
        } else if let Some((tp, _)) = parse_rwlock_method(name) {
            Some(tp)
        } else if let Some((tp, _)) = parse_box_method(name) {
            Some(tp)
        } else {
            None
        };
        if let Some(ref tp) = type_prefix {
            // Skip unmonomorphized generic wrappers (e.g. Shared__Vector__T)
            if is_unmonomorphized_wrapper(tp) { continue; }
            // Auto-discover element types that may also need typedefs.
            let elem_name = if tp.starts_with("Channel__") {
                channel_elem_type(tp).to_string()
            } else if tp.starts_with("Mutex__") {
                mutex_elem_type(tp).to_string()
            } else if tp.starts_with("RWLock__") {
                rwlock_elem_type(tp).to_string()
            } else if tp.starts_with("Box__") {
                box_elem_type(tp).to_string()
            } else if tp.starts_with("Guard__") || tp.starts_with("ReadGuard__") || tp.starts_with("WriteGuard__") {
                guard_elem_type(tp).to_string()
            } else {
                shared_elem_type(tp).to_string()
            };
            let resolved = resolve_elem_type(&elem_name, &orig_to_c);
            if is_monomorphized_wrapper_type(&resolved) && type_seen.insert(resolved.clone()) {
                emit_wrapper_typedef(out, &resolved, module, &orig_to_c);
            }
            if type_seen.insert(tp.clone()) {
                emit_wrapper_typedef(out, tp, module, &orig_to_c);
            }
        }
    }
    // Second pass: emit inline wrappers (now that all typedefs are in place).
    for name in &method_calls {
        // Extract the type prefix from whichever wrapper pattern matches.
        let tp = if let Some((tp, _)) = parse_channel_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_shared_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_weak_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_mutex_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_rwlock_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_guard_method(name) { Some(tp) }
            else if let Some((tp, _)) = parse_box_method(name) { Some(tp) }
            else { None };
        if let Some(ref tp) = tp {
            if is_unmonomorphized_wrapper(tp) { continue; }
        }
        if let Some((type_prefix, method)) = parse_channel_method(name) {
            let elem = resolve_elem_type(channel_elem_type(&type_prefix), &orig_to_c);
            emit_channel_wrapper(out, &type_prefix, method, &elem);
        } else if let Some((type_prefix, method)) = parse_shared_method(name) {
            let elem = resolve_elem_type(shared_elem_type(&type_prefix), &orig_to_c);
            emit_shared_wrapper(out, &type_prefix, method, &elem);
        } else if let Some((type_prefix, method)) = parse_weak_method(name) {
            emit_weak_wrapper(out, &type_prefix, method, &orig_to_c);
        } else if let Some((type_prefix, method)) = parse_mutex_method(name) {
            let elem = resolve_elem_type(mutex_elem_type(&type_prefix), &orig_to_c);
            emit_mutex_wrapper(out, &type_prefix, method, &elem);
        } else if let Some((type_prefix, method)) = parse_rwlock_method(name) {
            let elem = resolve_elem_type(rwlock_elem_type(&type_prefix), &orig_to_c);
            emit_rwlock_wrapper(out, &type_prefix, method, &elem);
        } else if let Some((type_prefix, method)) = parse_guard_method(name) {
            let elem = resolve_elem_type(guard_elem_type(&type_prefix), &orig_to_c);
            emit_guard_wrapper(out, &type_prefix, method, &elem);
        } else if let Some((type_prefix, method)) = parse_box_method(name) {
            let elem = resolve_elem_type(box_elem_type(&type_prefix), &orig_to_c);
            emit_box_wrapper(out, &type_prefix, method, &elem, module, &orig_to_c);
        }
    }

    writeln!(out).unwrap();
}

/// Scan call names in the LIR module and conditionally include C runtime modules,
/// LIR helper functions, box allocators, and inline shim functions.
///
/// This covers everything that depends on `include_runtime == true`:
/// - Conditional runtime section inclusion (preamble, allocators, collections, async, etc.)
/// - LIR helpers (default value functions, comparators, hash functions)
/// - `__gorget_box_alloc_*` monomorphized box allocators
/// - Inline shims for str/array operations not provided by the C runtime
pub(super) fn emit_runtime_modules(out: &mut String, module: &LirModule, _struct_names: &HashMap<u32, String>) {
    // Scan ALL call names (externs + function names + CallExtern inside bodies)
    // to determine which optional runtime modules are needed.
    let mut all_call_names: Vec<&str> = module.externs.iter().map(|e| e.name.as_str())
        .chain(module.functions.iter().map(|f| f.name.as_str()))
        .collect();
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::CallExtern { name, .. } = inst {
                    all_call_names.push(name.as_str());
                }
            }
        }
    }
    // Globals' Extern initialisers also call into the runtime at module-init
    // (e.g., `gorget_math_infinity` from `lib/std/math.gg`'s INFINITY). Include
    // their names so the conditional runtime modules are pulled in.
    for g in &module.globals {
        if let LirGlobalInit::Extern { name, .. } = &g.init {
            all_call_names.push(name.as_str());
        }
    }
    let has = |pred: &dyn Fn(&str) -> bool| all_call_names.iter().any(|n| pred(n));

    // Also check struct names for monomorphized types that need specific runtimes.
    let _has_struct = |name: &str| module.structs.iter().any(|s| s.name == name);

    // ── Freestanding target: include minimal runtime, skip hosted modules ──
    if module.target.starts_with("freestanding") {
        out.push_str("/* Gorget freestanding runtime */\n");
        out.push_str("#include \"runtime.c\"\n\n");
        return;
    }

    // ── Minimal preamble (headers, allocator, scoped alloc stubs) ──
    out.push_str(crate::backend::c::c_runtime::RUNTIME_PREAMBLE);

    // ── Conditional allocators ──
    if has(&|n| n.starts_with("gorget_arena_") || n.starts_with("GorgetArena")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_ARENA_ALLOC);
    }
    if has(&|n| n.starts_with("gorget_tracking_")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_TRACKING_ALLOC);
    }
    if has(&|n| n.starts_with("gorget_pool_") || n.starts_with("GorgetPool")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_POOL_ALLOC);
    }
    if has(&|n| n.starts_with("gorget_tlsf_")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_TLSF_ALLOC);
    }
    if has(&|n| n.starts_with("gorget_fba_") || n.starts_with("gorget_fixed_buffer_")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_FIXEDBUF_ALLOC);
    }
    if has(&|n| n.starts_with("gorget_fallback_")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_FALLBACK_ALLOC);
    }

    // ── String types and operations ──
    out.push_str(crate::backend::c::c_runtime::RUNTIME_STRING);

    // Extended string methods (unicode tables, search, split/replace/trim/etc.)
    if has(&|n| n.starts_with("gorget_str_to_upper") || n.starts_with("gorget_str_to_lower")
        || n.starts_with("gorget_str_is_alpha") || n.starts_with("gorget_str_is_upper")
        || n.starts_with("gorget_str_is_lower") || n.starts_with("gorget_str_is_digit")
        || n.starts_with("gorget_str_is_whitespace")
        || n.starts_with("gorget_str_contains") || n.starts_with("gorget_str_starts_with")
        || n.starts_with("gorget_str_ends_with") || n.starts_with("gorget_str_find")
        || n.starts_with("gorget_memmem")
        || n.starts_with("gorget_str_trim") || n.starts_with("gorget_str_replace")
        || n.starts_with("gorget_str_repeat") || n.starts_with("gorget_str_pad")
        || n.starts_with("gorget_str_strip") || n.starts_with("gorget_str_lstrip")
        || n.starts_with("gorget_str_rstrip") || n.starts_with("gorget_str_removeprefix")
        || n.starts_with("gorget_str_removesuffix") || n.starts_with("gorget_str_index_of")
        || n.starts_with("gorget_str_count") || n.starts_with("gorget_str_center")
        || n.starts_with("gorget_str_ljust") || n.starts_with("gorget_str_rjust")
        || n.starts_with("gorget_str_zfill") || n.starts_with("gorget_str_reverse")
        || n.starts_with("gorget_str_encode_") || n.starts_with("gorget_str_decode_")
        || n.starts_with("gorget_base64_") || n.starts_with("gorget_json_escape")
        || n.starts_with("gorget_str_to_json") || n.starts_with("gorget_str_from_json")
        || n.starts_with("gorget_uint8_is_") || n.starts_with("gorget_uint8_to_")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_STRING_EXTENDED);
    }

    // Base string operations (Str-aware concat, append, cstr conversion)
    out.push_str(crate::backend::c::c_runtime::RUNTIME_STRING_BASE_OPS);

    // ── Alloc report (test/bench mode only) ──
    let is_test_or_bench = !module.test_fns.is_empty() || !module.bench_fns.is_empty() || module.is_test_module;
    if is_test_or_bench {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_ALLOC_REPORT);
    }

    // ── Clone stats report (--clone-stats only) ──
    // Safe to emit alongside the alloc-report: distinct atexit handler, distinct output line.
    if module.clone_stats {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_CLONE_STATS);
        // Per-site attribution: counter table sized by the module's CloneId
        // count + `__gorget_clone_site_hit` + atexit `[clone-site]` report.
        // Must precede function bodies (which call the hit function).
        out.push_str(&crate::backend::c::c_runtime::render_clone_sites_runtime(module.clone_site_count));
    }

    // ── Panic handler ──
    if !is_test_or_bench {
        out.push_str(crate::backend::c::c_runtime::PANIC_NORMAL);
    } else {
        out.push_str(crate::backend::c::c_runtime::PANIC_TEST);
    }

    // ── Conditional core sections (formerly RUNTIME_CORE) ──
    // Use flags to track what's been emitted and enforce dependencies.
    let mut emitted_array = false;
    let mut emitted_map = false;

    // Helper macro to emit RUNTIME_ARRAY if not yet emitted
    macro_rules! ensure_array {
        ($out:expr, $flag:expr) => {
            if !$flag {
                $out.push_str(crate::backend::c::c_runtime::RUNTIME_ARRAY);
                $flag = true;
            }
        };
    }
    macro_rules! ensure_map {
        ($out:expr, $aflag:expr, $mflag:expr) => {
            ensure_array!($out, $aflag); // MAP depends on ARRAY
            if !$mflag {
                $out.push_str(crate::backend::c::c_runtime::RUNTIME_MAP);
                $mflag = true;
            }
        };
    }

    // Checked arithmetic (macros used by integer overflow checks)
    if has(&|n| n.starts_with("gorget_checked_") || n.starts_with("GORGET_CHECKED_")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_CHECKED_ARITH);
    }

    // Collections: Array
    // Trigger on any gorget_array_* / Vector__* call, OR on the mere
    // presence of `Vector__` anywhere in a type name (including nested
    // Option__Vector__*, Result__Vector__*__IoError, etc.) — user-defined
    // types with Vector[T] fields emit clone/drop code that references
    // gorget_array_clone / gorget_array_free without ever showing up as
    // a CallExtern.
    // User structs/enums with collection fields emit
    // `gorget_array_clone` / `gorget_map_clone` / `gorget_set_clone`
    // text in their generated clone/drop helpers without ever surfacing
    // as a direct extern call. The recursive-drop tables hold the
    // resolved drop-fn name from `DropStrategy::Trivial(...)` — which
    // IS the runtime contract — so it's the right thing to check at
    // the C-emit boundary. (Box__<inner> goes through
    // `StructDef.box_inner_type` typed metadata; Vector/Map/Set drop
    // fns are stable runtime symbols, OK to spell at the boundary.)
    let recursive_drop_fn_used = |runtime_drop_fn: &str| {
        module.recursive_drop_structs.values().any(|fields| {
            fields.iter().any(|(_f, drop, _ty)| drop == runtime_drop_fn)
        }) || module.recursive_drop_enums.values().any(|variants| {
            variants.iter().any(|(_i, _v, _f, drop, _ty)| drop == runtime_drop_fn)
        })
    };
    let vector_struct_present = module.structs.iter().any(|s|
            s.elem_drop_fn.as_deref() == Some("gorget_array_free"))
        || recursive_drop_fn_used("gorget_array_free");
    if vector_struct_present
        || has(&|n| n.starts_with("gorget_array_") || n.starts_with("Vector__") || n.starts_with("Deque__"))
    {
        ensure_array!(out, emitted_array);
    }

    // String/Array operations (join, split, iterators — needs RUNTIME_ARRAY)
    if has(&|n| n.starts_with("gorget_str_join") || n.starts_with("gorget_str_split")
        || n.starts_with("gorget_str_bytes") || n.starts_with("gorget_str_codepoints")
        || n.starts_with("gorget_str_chars")) {
        ensure_array!(out, emitted_array);
        out.push_str(crate::backend::c::c_runtime::RUNTIME_STRING_ARRAY);
    }

    // Collections: Map (depends on Array for keys/values/items)
    let map_struct_present = module.structs.iter().any(|s|
            s.elem_drop_fn.as_deref() == Some("gorget_map_free"))
        || recursive_drop_fn_used("gorget_map_free");
    if map_struct_present
        || has(&|n| n.starts_with("gorget_map_") || n.starts_with("gorget_dict_")
            || n.starts_with("Dict__") || n.starts_with("HashMap__"))
    {
        ensure_map!(out, emitted_array, emitted_map);
    }

    // Collections: Set (depends on Map)
    let set_struct_present = module.structs.iter().any(|s|
            s.elem_drop_fn.as_deref() == Some("gorget_set_free"))
        || recursive_drop_fn_used("gorget_set_free");
    if set_struct_present
        || has(&|n| n.starts_with("gorget_set_") || n.starts_with("Set__") || n.starts_with("HashSet__"))
    {
        ensure_map!(out, emitted_array, emitted_map);
        out.push_str(crate::backend::c::c_runtime::RUNTIME_SET);
    }

    // Error handling (test/bench mode or explicit catch/throw)
    if is_test_or_bench || has(&|n| n.starts_with("gorget_catch") || n.starts_with("gorget_throw")
        || n.starts_with("gorget_cleanup_")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_ERROR);
    }

    // File I/O (depends on Array for read_file_bytes)
    if has(&|n| n.starts_with("gorget_file_") || n == "gorget_read_file"
        || n == "gorget_write_file" || n == "gorget_append_file"
        || n == "gorget_read_file_bytes"
        || n == "File__open" || n == "File__create") {  // codegen rewrites to gorget_file_open
        ensure_array!(out, emitted_array);
        out.push_str(crate::backend::c::c_runtime::RUNTIME_FILE);
    }

    // Path functions + readdir (depends on Array for readdir)
    if has(&|n| n.starts_with("gorget_path_") || n == "gorget_is_file" || n == "gorget_is_dir"
        || n.starts_with("gorget_mkdir") || n.starts_with("gorget_readdir")
        || n == "gorget_rename" || n == "gorget_copy_file" || n == "gorget_remove"
        || n == "gorget_basename" || n == "gorget_dirname" || n == "gorget_file_size"
        || n == "gorget_file_mtime") {
        ensure_array!(out, emitted_array);
        out.push_str(crate::backend::c::c_runtime::RUNTIME_PATH);
    }

    // CLI args (gorget_args — needs RUNTIME_ARRAY; gorget_init_args is in preamble)
    if has(&|n| n == "gorget_args") {
        ensure_array!(out, emitted_array);
        out.push_str(crate::backend::c::c_runtime::RUNTIME_ARGS);
    }

    // Parsing (also detects int__parse/float__parse codegen patterns)
    if has(&|n| n.starts_with("gorget_parse_int") || n.starts_with("gorget_parse_float")
        || n.starts_with("gorget_try_parse")
        || (n.ends_with("__parse") && (n.starts_with("int") || n.starts_with("uint")
            || n == "double__parse" || n == "float__parse" || n == "bool__parse"))) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_PARSE);
    }

    // to_str conversions
    if has(&|n| n.starts_with("gorget_int_to_str") || n.starts_with("gorget_float_to_str")
        || n.starts_with("gorget_bool_to_str") || n.starts_with("gorget_codepoint_to_utf8")
        || n.starts_with("gorget_char_to_str") || n.starts_with("gorget_int_to_binary")
        || n.starts_with("gorget_int_to_hex") || n.starts_with("gorget_int_to_octal")
        || n.starts_with("gorget_int_to_float") || n.starts_with("gorget_float_to_int")
        || n == "gorget_string_debug"
        || n == "gorget_char_chr" || n == "gorget_assert_fail_values") {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_TOSTR);
    }

    // Environment
    if has(&|n| n == "gorget_getenv" || n == "gorget_setenv" || n == "gorget_getcwd"
        || n == "gorget_platform" || n == "gorget_exit") {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_ENV);
    }

    // Interactive I/O, time, datetime, random, line input (depends on Array for dt_decompose)
    if has(&|n| n.starts_with("gorget_input") || n.starts_with("gorget_rand")
        || n.starts_with("gorget_seed") || n.starts_with("gorget_sleep_ms")
        || n == "sleep_ms"
        || n.starts_with("gorget_time") || n.starts_with("gorget_format_time")
        || n.starts_with("gorget_parse_time") || n.starts_with("gorget_readline")
        || n.starts_with("gorget_dt_decompose") || n.starts_with("gorget_getchar")
        || n.starts_with("gorget_term_") || n == "gorget_is_tty") {
        ensure_array!(out, emitted_array);
        out.push_str(crate::backend::c::c_runtime::RUNTIME_IO);
    }

    // Math
    if has(&|n| n.starts_with("gorget_sqrt") || n.starts_with("gorget_pow")
        || n.starts_with("gorget_floor") || n.starts_with("gorget_ceil")
        || n.starts_with("gorget_round") || n.starts_with("gorget_abs")
        || n.starts_with("gorget_sin") || n.starts_with("gorget_cos")
        || n.starts_with("gorget_tan") || n.starts_with("gorget_log")
        || n.starts_with("gorget_exp") || n.starts_with("gorget_atan2")
        || n.starts_with("gorget_fmod") || n == "gorget_min" || n == "gorget_max"
        || n.starts_with("gorget_math_")
        || n.starts_with("GORGET_PI") || n.starts_with("GORGET_E")
        || n.starts_with("GORGET_TAU") || n.starts_with("GORGET_INF")
        || n.starts_with("GORGET_NAN")) {
        out.push_str(crate::backend::c::c_runtime::RUNTIME_MATH);
    }

    // Sort comparators (depends on Array)
    if has(&|n| n.starts_with("__gorget_cmp_") || n.starts_with("gorget_array_sort")
        || n.starts_with("gorget_array_reverse") || n.starts_with("gorget_array_unique")) {
        ensure_array!(out, emitted_array);
        out.push_str(crate::backend::c::c_runtime::RUNTIME_SORT);
    }

    writeln!(out).unwrap();

    // Sync primitives (atomics, barriers, semaphores, etc.)
    let needs_sync = has(&|n| n.starts_with("gorget_atomic_int_") || n.starts_with("gorget_atomic_bool_")) || has(&|n| {
        n.starts_with("gorget_atomic_") || n.starts_with("gorget_barrier_")
        || n.starts_with("gorget_condvar_") || n.starts_with("gorget_rwlock_")
        || n.starts_with("gorget_waitgroup_") || n.starts_with("gorget_semaphore_")
        || n.starts_with("gorget_onceflag_")
        || n.starts_with("gorget_read_guard_") || n.starts_with("gorget_write_guard_")
        || n.starts_with("ReadGuard__") || n.starts_with("WriteGuard__")
    });
    if needs_sync {
        out.push_str(crate::backend::c::c_runtime::SYNC_RUNTIME);
    }

    // Async core
    let needs_async = has(&|n| {
        n.contains("channel") || n.contains("Channel")
        || n.starts_with("gorget_mutex_") || n.starts_with("gorget_guard_")
        || n.starts_with("gorget_executor_") || n == "gorget_spawn"
        || n.starts_with("__gorget_spawn_") || n.starts_with("__gorget_await_")
        || n.starts_with("gorget_task_group_") || n.starts_with("gorget_reactor_")
        || n.starts_with("Mutex__") || n.starts_with("RWLock__")
    });
    if needs_async {
        out.push_str(crate::backend::c::c_runtime::ASYNC_RUNTIME);
        out.push_str(crate::backend::c::c_runtime::TASK_COMMON);
        match module.scheduler_mode {
            crate::ir::SchedulerMode::Pool => out.push_str(crate::backend::c::c_runtime::SCHEDULER_POOL_RUNTIME),
            crate::ir::SchedulerMode::Thread => out.push_str(crate::backend::c::c_runtime::SCHEDULER_THREAD_RUNTIME),
            crate::ir::SchedulerMode::Inline => out.push_str(crate::backend::c::c_runtime::SCHEDULER_INLINE_RUNTIME),
            crate::ir::SchedulerMode::Single => out.push_str(crate::backend::c::c_runtime::SCHEDULER_SINGLE_RUNTIME),
        }
        out.push_str(crate::backend::c::c_runtime::MAIN_WAKER_RUNTIME);
        out.push_str(crate::backend::c::c_runtime::EXECUTOR_RUNTIME);
    }

    // Channels (also triggered by monomorphized Channel__T methods)
    if has(&|n| n.starts_with("gorget_channel_") || n.starts_with("Channel__")) {
        if !needs_async {
            out.push_str(crate::backend::c::c_runtime::ASYNC_RUNTIME);
        }
        out.push_str(crate::backend::c::c_runtime::CHANNEL_RUNTIME);
    }

    // Shared / Weak references (also triggered by monomorphized methods)
    if has(&|n| n.starts_with("gorget_shared_") || n.starts_with("gorget_weak_")
        || n.starts_with("Shared__") || n.starts_with("Weak__")) {
        out.push_str(crate::backend::c::c_runtime::SHARED_RUNTIME);
    }

    // Mutex / Guard (also triggered by Mutex__T monomorphized methods)
    if has(&|n| n.starts_with("gorget_mutex_") || n.starts_with("gorget_guard_")
        || n.starts_with("Mutex__") || n.starts_with("RWLock__")
        || n.starts_with("Guard__") || n.starts_with("ReadGuard__") || n.starts_with("WriteGuard__")
        || n.starts_with("gorget_rwlock_") || n.starts_with("gorget_read_guard_")
        || n.starts_with("gorget_write_guard_"))
    {
        if !needs_async {
            out.push_str(crate::backend::c::c_runtime::ASYNC_RUNTIME);
        }
        out.push_str(crate::backend::c::c_runtime::MUTEX_RUNTIME);
    }

    // Reactor (async I/O, sleep, timers)
    if has(&|n| n.starts_with("gorget_reactor_") || n.starts_with("gorget_sleep_async")) {
        out.push_str(crate::backend::c::c_runtime::REACTOR_RUNTIME);
    }

    // Blocking pool — also needed for spawned functions (blocking spawn approach)
    if has(&|n| n.starts_with("gorget_blocking_")) || !module.spawned_fns.is_empty() {
        out.push_str(crate::backend::c::c_runtime::BLOCKING_POOL_RUNTIME);
    }

    // Task groups
    if has(&|n| n.starts_with("gorget_task_group_")) {
        out.push_str(crate::backend::c::c_runtime::TASK_GROUP_RUNTIME);
    }

    // Bytes
    if has(&|n| n.starts_with("gorget_bytes_")) {
        out.push_str(crate::backend::c::c_runtime::BYTES_RUNTIME);
    }

    // (xtd.regex is now pure Gorget — no REGEX_RUNTIME injection.)

    // Crypto
    if has(&|n| n.starts_with("gorget_crypto_") || n.starts_with("gorget_sha") || n.starts_with("gorget_hmac") || n.starts_with("gorget_x25519") || n.starts_with("gorget_hkdf") || n.starts_with("gorget_aead")) {
        out.push_str(crate::backend::c::c_runtime::CRYPTO_RUNTIME);
    }

    // Socket (depends on Array for socket_read/read_exact)
    if has(&|n| n.starts_with("gorget_socket_") || n.starts_with("gorget_tcp_")) {
        ensure_array!(out, emitted_array);
        out.push_str(crate::backend::c::c_runtime::SOCKET_RUNTIME);
    }

    // Server socket (depends on Array)
    if has(&|n| n.starts_with("gorget_server_socket_") || n.starts_with("gorget_listener_")) {
        ensure_array!(out, emitted_array);
        out.push_str(crate::backend::c::c_runtime::SERVER_SOCKET_RUNTIME);
    }

    // UDP socket
    if has(&|n| n.starts_with("gorget_udp_")) {
        out.push_str(crate::backend::c::c_runtime::UDP_SOCKET_RUNTIME);
    }

    // TLS
    if has(&|n| n.starts_with("gorget_tls_")) {
        out.push_str(crate::backend::c::c_runtime::TLS_SOCKET_RUNTIME);
        out.push_str(crate::backend::c::c_runtime::TLS_SERVER_RUNTIME);
    }

    // Process spawn (fork+exec with pipes) + signal handling (signal functions live in PROCESS_SPAWN_RUNTIME)
    let needs_spawn = has(&|n| n.starts_with("gorget_process_spawn") || n.starts_with("gorget_process_wait")
        || n.starts_with("gorget_process_kill") || n.starts_with("gorget_process_pid")
        || n.starts_with("gorget_process_read_") || n.starts_with("gorget_process_write_")
        || n.starts_with("gorget_process_close_")
        || n.starts_with("gorget_signal_") || n == "gorget_getpid");

    // Process — also needed when spawn is used (ExecResult typedef lives here)
    if needs_spawn || has(&|n| n.starts_with("gorget_process_") || n.starts_with("gorget_exec") || n == "gorget_getenv" || n == "gorget_setenv") {
        out.push_str(crate::backend::c::c_runtime::PROCESS_RUNTIME);
    }

    if needs_spawn {
        ensure_array!(out, emitted_array); // gorget_process_spawn uses gorget_array_get
        out.push_str(crate::backend::c::c_runtime::PROCESS_SPAWN_RUNTIME);
    }

    // Thread
    if has(&|n| n.starts_with("gorget_thread_") || n.starts_with("gorget_current_thread_id")
        || n.starts_with("__gorget_thread_spawn_")) || !module.thread_spawned_fns.is_empty() {
        out.push_str(crate::backend::c::c_runtime::THREAD_RUNTIME);
    }

    // Trace
    if module.trace_filename.is_some() || has(&|n| n.starts_with("gorget_trace_")) {
        out.push_str(crate::backend::c::c_runtime::TRACE_RUNTIME);
    }

    // SDL
    if has(&|n| n.starts_with("sdl_") || n.starts_with("gorget_sdl_")) {
        if has(&|n| n == "sdl_load_texture" || n == "gorget_sdl_load_texture") {
            out.push_str("#define GORGET_USE_SDL_IMAGE\n");
        }
        if has(&|n| n == "sdl_load_font" || n == "sdl_close_font" || n == "sdl_draw_text"
            || n == "sdl_render_text" || n == "sdl_text_width" || n == "sdl_text_height"
            || n.starts_with("gorget_sdl_load_font") || n.starts_with("gorget_sdl_draw_text")
            || n.starts_with("gorget_sdl_render_text")) {
            out.push_str("#define GORGET_USE_SDL_TTF\n");
        }
        out.push_str(crate::backend::c::c_runtime::SDL_RUNTIME);
    }

    // Bytes f32/f64/i64 helpers
    if has(&|n| n.starts_with("gorget_bytes_") && (n.contains("f32") || n.contains("f64") || n.contains("i64"))) {
        out.push_str(crate::backend::c::c_runtime::BYTES_F32_RUNTIME);
    }

    // OpenGL
    if has(&|n| n.starts_with("gorget_gl_")) {
        out.push_str(crate::backend::c::c_runtime::GL_RUNTIME);
    }

    // Image loading (stb_image)
    if has(&|n| n.starts_with("gorget_image_")) {
        out.push_str("\n#define STB_IMAGE_IMPLEMENTATION\n");
        out.push_str("#define STBI_NO_STDIO\n");
        out.push_str("#define STBI_ONLY_PNG\n");
        out.push_str("#define STBI_ONLY_JPEG\n");
        out.push_str("#define STBI_ONLY_TGA\n");
        out.push_str("#define STBI_ONLY_BMP\n");
        out.push_str("#define GORGET_HAS_STB_IMAGE 1\n");
        out.push_str("#pragma GCC diagnostic push\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wunused-function\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wunused-parameter\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wsign-compare\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wshift-negative-value\"\n");
        out.push_str(crate::backend::c::c_runtime::STB_IMAGE_SOURCE);
        out.push_str("\n#pragma GCC diagnostic pop\n");
        out.push_str(crate::backend::c::c_runtime::IMAGE_RUNTIME);
    }

    // Audio (SDL2_mixer)
    if has(&|n| n.starts_with("gorget_audio_")) {
        out.push_str(crate::backend::c::c_runtime::AUDIO_RUNTIME);
    }

    // Compression (zlib/deflate)
    if has(&|n| n.starts_with("gorget_zlib_") || n.starts_with("gorget_deflate_") || n.starts_with("gorget_crc32_")) {
        out.push_str(crate::backend::c::c_runtime::COMPRESS_RUNTIME);
    }

    // Metal (macOS Objective-C wrappers)
    if has(&|n| n.starts_with("gorget_metal_") || n.starts_with("gorget_sdl_metal_")) {
        out.push_str(crate::backend::c::c_runtime::METAL_RUNTIME);
    }

    // SQLite
    let needs_sqlite = has(&|n| n.starts_with("gorget_sqlite_") || n == "sqlite_open");
    if needs_sqlite {
        out.push_str("\n#define SQLITE_MAX_MMAP_SIZE 0\n");
        out.push_str("#define HAVE_MREMAP 0\n");
        out.push_str("#pragma GCC diagnostic push\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wunused-parameter\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wunused-variable\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wunused-function\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wimplicit-fallthrough\"\n");
        out.push_str("#pragma GCC diagnostic ignored \"-Wpedantic\"\n");
        out.push_str(crate::backend::c::c_runtime::SQLITE_AMALGAMATION);
        out.push_str("\n#pragma GCC diagnostic pop\n");
        out.push_str(crate::backend::c::c_runtime::SQLITE_GORGET_WRAPPERS);
    }

    // Hot-reload runtime (dlopen/file-watcher helpers)
    if module.hot_reload {
        out.push_str(crate::backend::c::c_runtime::HOT_RELOAD_RUNTIME);
    }

    // Suppress "value never read" warnings on idempotent emit-once flags.
    let _ = (emitted_array, emitted_map);

    emit_lir_helpers(out, module);
}

/// LIR-specific helper functions: char operations, hash, default values,
/// comparison functions for sorted(), etc. Called from both emit_runtime_modules
/// and generate_llvm_wrappers.
pub(super) fn emit_lir_helpers(out: &mut String, module: &LirModule) {
    let has = |pred: &dyn Fn(&str) -> bool| -> bool {
        module.externs.iter().any(|e| pred(&e.name))
            || module.functions.iter().flat_map(|f| f.blocks.iter())
                .flat_map(|b| b.insts.iter())
                .any(|inst| matches!(inst, Inst::CallExtern { name, .. } if pred(name)))
    };

    writeln!(out, "// ── LIR helpers ──").unwrap();
    if has(&|n| n == "gorget_char_chr") {
        writeln!(out, "static inline Str gorget_char_chr(int64_t code) {{ return gorget_codepoint_to_utf8(code); }}").unwrap();
    }
    // Always emit gorget_str_ord — needed by .ord() method AND int(string) cast.
    writeln!(out, "static inline int64_t gorget_str_ord(Str s) {{ size_t pos = 0; return (int64_t)gorget_utf8_decode((const char*)s.data, s.len, &pos); }}").unwrap();
    // Default value functions for primitive types
    writeln!(out, "static inline Str gorget_str_default(void) {{ return GORGET_EMPTY_STR; }}").unwrap();
    writeln!(out, "static inline int64_t int64_t__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline int64_t int__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline int8_t int8_t__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline int16_t int16_t__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline int32_t int32_t__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline uint8_t uint8_t__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline uint16_t uint16_t__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline uint32_t uint32_t__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline uint64_t uint64_t__default(void) {{ return 0; }}").unwrap();
    writeln!(out, "static inline double double__default(void) {{ return 0.0; }}").unwrap();
    writeln!(out, "static inline double float__default(void) {{ return 0.0; }}").unwrap();
    writeln!(out, "static inline bool bool__default(void) {{ return false; }}").unwrap();
    // Hash functions
    writeln!(out, "static inline int64_t __gorget_hash_int(int64_t v) {{ return (int64_t)__gorget_fnv1a(&v, sizeof(v)); }}").unwrap();
    writeln!(out, "static inline int64_t gorget_str_hash(Str s) {{ return (int64_t)__gorget_hash_str_len((const char*)s.data, s.len); }}").unwrap();
    // Signal functions — defined in the main runtime (c_runtime.rs).
    // Only emit minimal stubs when the runtime signal module is NOT included.
    writeln!(out, "#ifndef _WIN32").unwrap();
    writeln!(out, "#include <signal.h>").unwrap();
    writeln!(out, "#endif").unwrap();
    // Comparison functions for sorted()
    writeln!(out, "static int gorget_generic_compare(const void* a, const void* b) {{ return memcmp(a, b, sizeof(int64_t)); }}").unwrap();
    writeln!(out, "static int gorget_int_compare(const void* a, const void* b) {{ int64_t va = *(const int64_t*)a, vb = *(const int64_t*)b; return (va > vb) - (va < vb); }}").unwrap();
    writeln!(out, "static int gorget_float_compare(const void* a, const void* b) {{ double da = *(const double*)a, db = *(const double*)b; return (da > db) - (da < db); }}").unwrap();
    writeln!(out, "static int gorget_str_compare(const void* a, const void* b) {{ Str sa = *(const Str*)a, sb = *(const Str*)b; size_t la = sa.len, lb = sb.len; size_t ml = la < lb ? la : lb; int r = ml > 0 ? memcmp(sa.data, sb.data, ml) : 0; if (r) return r; return (la > lb) - (la < lb); }}").unwrap();
    writeln!(out, "static inline int64_t int64_t__one(void) {{ return 1; }}").unwrap();
    writeln!(out, "static inline int64_t int__one(void) {{ return 1; }}").unwrap();
    writeln!(out, "static inline double double__one(void) {{ return 1.0; }}").unwrap();
    writeln!(out, "static inline double float__one(void) {{ return 1.0; }}").unwrap();

    // UTF-8 codepoint helpers (normally in emit_runtime_modules)
    if has(&|n| n == "gorget_utf8_codepoint_len_at") {
        writeln!(out, "static inline int64_t gorget_utf8_codepoint_len_at(Str s, int64_t byte_pos) {{ \
            if (byte_pos < 0 || byte_pos >= (int64_t)s.len) return 0; \
            return (int64_t)gorget_utf8_codepoint_len(((const unsigned char*)s.data)[byte_pos]); }}").unwrap();
    }
    if has(&|n| n == "gorget_str_codepoint_at") {
        writeln!(out, "static inline Str gorget_str_codepoint_at(Str s, int64_t byte_pos) {{ \
            if (byte_pos < 0 || byte_pos >= (int64_t)s.len) return GORGET_EMPTY_STR; \
            int cplen = gorget_utf8_codepoint_len(((const unsigned char*)s.data)[byte_pos]); \
            if (byte_pos + cplen > (int64_t)s.len) cplen = (int)(s.len - (size_t)byte_pos); \
            return gorget_str_view_region((const char*)s.data + byte_pos, (size_t)cplen); }}").unwrap();
    }
    // gorget_signal_ignore is already in the C runtime — no duplicate emission needed.

    // gorget_task_group_submit is a MACRO in the runtime, not a function.
    // The LLVM/C backend calls it as a function with (TaskGroup*, Task__T) args.
    // Every `Task__T` has layout { void* __task, void(*__drop)(void*) }, so we
    // emit a replacement that receives the task by address (cast to void*) and
    // reads the two fields through that pointer. This is struct-type-agnostic:
    // it works uniformly for Task__void / Task__int / Task__String / etc.,
    // avoiding C's nominal-type rejection when a concrete Task__T is passed
    // where the function signature nominally wants __TaskHandle.
    if has(&|n| n == "gorget_task_group_submit") {
        writeln!(out, "#undef gorget_task_group_submit").unwrap();
        writeln!(out, "#define gorget_task_group_submit(g, task) do {{ \\").unwrap();
        writeln!(out, "    gorget_task_group_submit_raw((g), (task).__task, (task).__drop); \\").unwrap();
        writeln!(out, "    (task).__task = NULL; \\").unwrap();
        writeln!(out, "}} while(0)").unwrap();
    }

    // gorget_file_create(GorgetString* path) is a synthetic not in the C runtime.
    // LLVM IR calls it directly; provide a real C function that calls gorget_file_open(path, "w").
    if has(&|n| n == "gorget_file_create") {
        writeln!(out, "GorgetFile gorget_file_create(GorgetString* path) {{").unwrap();
        writeln!(out, "    return gorget_file_open((const char*)path->data, \"w\");").unwrap();
        writeln!(out, "}}").unwrap();
    }
    // gorget_file_open(const char* path) — 1-arg LIR version means "open for reading".
    // The real C function takes 2 args (path, mode). Provide __gorget_file_open_r wrapper
    // that adds the "r" mode, called by the LLVM backend in place of gorget_file_open.
    if has(&|n| n == "gorget_file_open") {
        writeln!(out, "GorgetFile __gorget_file_open_r(const char* path) {{").unwrap();
        writeln!(out, "    return gorget_file_open(path, \"r\");").unwrap();
        writeln!(out, "}}").unwrap();
    }
    writeln!(out).unwrap();
}

/// Emit `__gorget_box_alloc_*` monomorphized box allocators and inline shim
/// functions for str/array operations that supplement the C runtime.
pub(super) fn emit_runtime_helpers(out: &mut String, module: &LirModule, struct_names: &HashMap<u32, String>) {
    // Generate __gorget_box_alloc_* helper functions.
    // These are monomorphized box allocators: malloc + store + return pointer.
    let mut box_allocs: Vec<(String, String)> = Vec::new();
    for ext in &module.externs {
        if ext.name.starts_with("__gorget_box_alloc_") && ext.params.len() == 1 {
            // Derive the C type from the function name suffix, not from the LIR param type,
            // because LIR represents Str as Ptr (void*) but the C box alloc needs the real type.
            let suffix = &ext.name["__gorget_box_alloc_".len()..];
            let param_ty = box_alloc_inner_c_type(suffix, &ext.params[0], struct_names);
            box_allocs.push((ext.name.clone(), param_ty));
        }
    }
    // Also scan CallExtern instructions for box allocs not in externs list.
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::CallExtern { name, args, .. } = inst {
                    if name.starts_with("__gorget_box_alloc_") && args.len() == 1 {
                        if !box_allocs.iter().any(|(n, _)| n == name) {
                            let suffix = &name["__gorget_box_alloc_".len()..];
                            let param_ty = box_alloc_suffix_to_c_type(suffix);
                            box_allocs.push((name.clone(), param_ty));
                        }
                    }
                }
            }
        }
    }
    // Box[T] alias structs carry typed inner-type metadata
    // (`StructDef.box_inner_type`) populated at LIR lowering. Read it here
    // so the matching `__gorget_box_alloc_<inner>` / `_free_<inner>`
    // helpers get emitted whenever the program registers a Box[T] type —
    // even when the only reference is from a generated clone/drop helper
    // text (e.g. `Node__clone` calling `__gorget_box_alloc_Node` for a
    // `Node1(Box[Node])` enum variant). Resolve the param's C type
    // through `struct_names` (rather than the bare LIR name) so the
    // generated `Box__T__drop` body and the allocator agree on the
    // mangled struct name.
    let resolve_inner_c_ty = |inner: &str| -> String {
        if let Some((sid, _)) = module.structs.iter().enumerate()
            .find(|(_, s)| s.name == inner)
        {
            if let Some(cn) = struct_names.get(&(sid as u32)) {
                return cn.clone();
            }
        }
        box_alloc_suffix_to_c_type(inner)
    };
    for sd in &module.structs {
        if let Some(inner) = &sd.box_inner_type {
            let alloc_name = format!("__gorget_box_alloc_{inner}");
            if !box_allocs.iter().any(|(n, _)| n == &alloc_name) {
                let param_ty = resolve_inner_c_ty(inner);
                box_allocs.push((alloc_name, param_ty));
            }
        }
    }
    for (name, param_ty) in &box_allocs {
        writeln!(out, "static inline void* {name}({param_ty} val) {{ __gorget_box_alloc_count++; {param_ty}* p = ({param_ty}*)GORGET_ALLOC(sizeof({param_ty})); *p = val; return (void*)p; }}").unwrap();
    }
    // Emit parallel `__gorget_box_free_<inner>` helpers that go through
    // `GORGET_FREE` so the tracking allocator sees the dealloc — raw
    // `free()` would unbalance `total_allocs` vs `total_frees` and look
    // like a leak in `--clone-stats`. Box's drop emission in
    // `src/lir/lower/drops.rs` calls these instead of `free` directly.
    for (name, param_ty) in &box_allocs {
        // alloc name shape: __gorget_box_alloc_<inner>; free is the
        // mirror with `_alloc_` → `_free_`.
        let free_name = name.replacen("_alloc_", "_free_", 1);
        writeln!(out, "static inline void {free_name}(void* p) {{ if (p) GORGET_FREE(p, sizeof({param_ty})); }}").unwrap();
    }
    // Forward-declare per-type Box drop wrappers so struct/enum drop
    // emitters can reference them. Bodies are emitted after T__drop is
    // defined (see emit_box_drop_wrappers, called after the drop pass).
    // The wrapper takes a slot pointer (not the box value) so its
    // signature matches the void(*)(void*) collection elem_drop ABI and
    // it can be called uniformly from struct field drops, enum variant
    // drops, Vector[Box[T]] elem_drop, and scope-exit Box drops.
    for (name, _) in &box_allocs {
        let inner = &name["__gorget_box_alloc_".len()..];
        writeln!(out, "void Box__{inner}__drop(void* slot);").unwrap();
    }
    if !box_allocs.is_empty() {
        writeln!(out).unwrap();
    }

    // Generate gorget_str_push/gorget_str_str/gorget_str_clear if called but not in runtime.
    //
    // `has_extern(n)` answers "is `n` declared as an extern OR referenced by any
    // CallExtern instruction?". The naive form re-scanned every extern + every
    // instruction in the module per query; with ~40 queries over a ~220k-inst
    // module that's millions of comparisons. Build the membership set once
    // (extern names ∪ CallExtern target names) — pure set membership, identical
    // semantics to the original `.any` short-circuit.
    let extern_call_names: HashSet<&str> = {
        let mut s: HashSet<&str> = HashSet::new();
        for e in &module.externs {
            s.insert(e.name.as_str());
        }
        for f in &module.functions {
            for b in &f.blocks {
                for inst in &b.insts {
                    if let Inst::CallExtern { name, .. } = inst {
                        s.insert(name.as_str());
                    }
                }
            }
        }
        s
    };
    let has_extern = |n: &str| extern_call_names.contains(n);
    if has_extern("gorget_str_push") {
        writeln!(out, "static inline void gorget_str_push(GorgetString* s, Str chunk) {{ gorget_string_push_char(s, chunk); }}").unwrap();
    }
    if has_extern("gorget_str_str") {
        // gorget_str_str: extract the immutable string from a builder.
        // Must clone because the builder and the result are separate owned strings.
        writeln!(out, "static inline Str gorget_str_str(GorgetString* s) {{ return gorget_string_clone_to_owned(s); }}").unwrap();
    }
    if has_extern("gorget_str_clear") {
        // Reset the len to 0 but keep the owned buffer for reuse. For views, len=0 is fine —
        // they'll re-materialize on next push/append.
        writeln!(out, "static inline void gorget_str_clear(GorgetString* s) {{ s->len = 0; if (s->cap > 0 && s->data) ((char*)s->data)[0] = '\\0'; }}").unwrap();
    }
    if has_extern("gorget_str_push_line") {
        writeln!(out, "static inline void gorget_str_push_line(GorgetString* s, Str chunk) {{ gorget_string_push_char(s, chunk); gorget_string_push_byte(s, '\\n'); }}").unwrap();
    }
    if has_extern("gorget_str_capacity") {
        writeln!(out, "static inline int64_t gorget_str_capacity(GorgetString* s) {{ return (int64_t)s->cap; }}").unwrap();
    }
    if has_extern("gorget_str_push_char") {
        writeln!(out, "static inline void gorget_str_push_char(GorgetString* s, Str c) {{ gorget_string_push_char(s, c); }}").unwrap();
    }
    if has_extern("gorget_array_sort") {
        // Thread-local to prevent data races when two threads sort concurrently.
        writeln!(out, "static _Thread_local size_t __gorget_sort_elem_size;").unwrap();
        writeln!(out, "static int __gorget_sort_cmp(const void* a, const void* b) {{ return memcmp(a, b, __gorget_sort_elem_size); }}").unwrap();
        writeln!(out, "static inline void gorget_array_sort(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; __gorget_sort_elem_size = a->elem_size; qsort(a->data, a->len, a->elem_size, __gorget_sort_cmp); }}").unwrap();
    }
    // Typed sort/unique variants — chosen by element type at LIR
    // emission time so qsort uses the right comparator:
    //   _int    → gorget_int_compare     (value-wise i64)
    //   _float  → gorget_float_compare   (value-wise double)
    //   _str    → gorget_str_compare     (lexical on Str struct)
    //   _generic → gorget_generic_compare (memcmp, for user structs)
    if has_extern("gorget_array_sort_int") {
        writeln!(out, "static inline void gorget_array_sort_int(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; qsort(a->data, a->len, a->elem_size, gorget_int_compare); }}").unwrap();
    }
    if has_extern("gorget_array_sort_float") {
        writeln!(out, "static inline void gorget_array_sort_float(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; qsort(a->data, a->len, a->elem_size, gorget_float_compare); }}").unwrap();
    }
    if has_extern("gorget_array_sort_str") {
        writeln!(out, "static inline void gorget_array_sort_str(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; qsort(a->data, a->len, a->elem_size, gorget_str_compare); }}").unwrap();
    }
    if has_extern("gorget_array_sort_generic") {
        writeln!(out, "static inline void gorget_array_sort_generic(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; qsort(a->data, a->len, a->elem_size, gorget_generic_compare); }}").unwrap();
    }
    if has_extern("gorget_array_sorted") {
        writeln!(out, "static inline GorgetArray gorget_array_sorted(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; GorgetArray r = gorget_array_clone(a); qsort(r.data, r.len, r.elem_size, gorget_generic_compare); return r; }}").unwrap();
    }
    if has_extern("gorget_array_sorted_int") {
        writeln!(out, "static inline GorgetArray gorget_array_sorted_int(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; GorgetArray r = gorget_array_clone(a); qsort(r.data, r.len, r.elem_size, gorget_int_compare); return r; }}").unwrap();
    }
    if has_extern("gorget_array_sorted_float") {
        writeln!(out, "static inline GorgetArray gorget_array_sorted_float(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; GorgetArray r = gorget_array_clone(a); qsort(r.data, r.len, r.elem_size, gorget_float_compare); return r; }}").unwrap();
    }
    if has_extern("gorget_array_sorted_str") {
        writeln!(out, "static inline GorgetArray gorget_array_sorted_str(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; GorgetArray r = gorget_array_clone(a); qsort(r.data, r.len, r.elem_size, gorget_str_compare); return r; }}").unwrap();
    }
    if has_extern("gorget_array_sorted_generic") {
        writeln!(out, "static inline GorgetArray gorget_array_sorted_generic(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; GorgetArray r = gorget_array_clone(a); qsort(r.data, r.len, r.elem_size, gorget_generic_compare); return r; }}").unwrap();
    }
    // gorget_array_reversed: clone + reverse (not in runtime, inlined by old backend)
    if has_extern("gorget_array_reversed") {
        writeln!(out, "static inline GorgetArray gorget_array_reversed(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; GorgetArray r = gorget_array_clone(a); gorget_array_reverse(&r); return r; }}").unwrap();
    }
    // gorget_array_unique: clone + sort + dedup (matches GIR backend semantics)
    if has_extern("gorget_array_unique") {
        writeln!(out, "static inline GorgetArray gorget_array_unique(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; GorgetArray r = gorget_array_clone(a); qsort(r.data, r.len, r.elem_size, gorget_generic_compare); gorget_array_dedup(&r); return r; }}").unwrap();
    }
    if has_extern("gorget_array_unique_int") {
        writeln!(out, "static inline GorgetArray gorget_array_unique_int(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; GorgetArray r = gorget_array_clone(a); qsort(r.data, r.len, r.elem_size, gorget_int_compare); gorget_array_dedup(&r); return r; }}").unwrap();
    }
    if has_extern("gorget_array_unique_float") {
        writeln!(out, "static inline GorgetArray gorget_array_unique_float(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; GorgetArray r = gorget_array_clone(a); qsort(r.data, r.len, r.elem_size, gorget_float_compare); gorget_array_dedup(&r); return r; }}").unwrap();
    }
    if has_extern("gorget_array_unique_str") {
        writeln!(out, "static inline GorgetArray gorget_array_unique_str(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; GorgetArray r = gorget_array_clone(a); qsort(r.data, r.len, r.elem_size, gorget_str_compare); gorget_array_dedup(&r); return r; }}").unwrap();
    }
    if has_extern("gorget_array_unique_generic") {
        writeln!(out, "static inline GorgetArray gorget_array_unique_generic(void* __arr_ptr) {{ GorgetArray* a = (GorgetArray*)__arr_ptr; GorgetArray r = gorget_array_clone(a); qsort(r.data, r.len, r.elem_size, gorget_generic_compare); gorget_array_dedup(&r); return r; }}").unwrap();
    }
    // Type-independent set predicates — each walks the argument
    // sets using their own `cap`/`states`/`key_size` fields, so a
    // single stub covers every element type.
    //
    // is_subset: every element of self is in other.
    // D39 Phase A.2b (DORMANT): dense branch walks packed `entries_keys`
    // directly (no tombstones, no state check); legacy branch keeps the
    // cap+states scan. `__self.entries_keys` is NULL for every Dict/Set
    // until A.2c flips the ctors → dense branch is unreachable in A.2b.
    if has_extern("gorget_set_is_subset") {
        writeln!(out, "static inline bool gorget_set_is_subset(void* __self_ptr, GorgetSet __other) {{ \
            GorgetSet __self = *(GorgetSet*)__self_ptr; \
            if (__self.entries_keys) {{ \
                for (size_t __i = 0; __i < __self.entries_len; __i++) {{ \
                    void* __k = (char*)__self.entries_keys + __i * __self.key_size; \
                    if (!gorget_set_contains(&__other, __k)) return false; \
                }} \
            }} else {{ \
                for (size_t __i = 0; __i < __self.cap; __i++) {{ \
                    if (__self.states[__i] != 1) continue; \
                    void* __k = (char*)__self.keys + __i * __self.key_size; \
                    if (!gorget_set_contains(&__other, __k)) return false; \
                }} \
            }} \
            return true; \
        }}").unwrap();
    }
    // is_superset(self, other) ≡ is_subset(other, self).
    // D39 Phase A.2b (DORMANT): dense branch on `__other.entries_keys`; see
    // is_subset above for the invariant.
    if has_extern("gorget_set_is_superset") {
        writeln!(out, "static inline bool gorget_set_is_superset(void* __self_ptr, GorgetSet __other) {{ \
            GorgetSet* __self = (GorgetSet*)__self_ptr; \
            if (__other.entries_keys) {{ \
                for (size_t __i = 0; __i < __other.entries_len; __i++) {{ \
                    void* __k = (char*)__other.entries_keys + __i * __other.key_size; \
                    if (!gorget_set_contains(__self, __k)) return false; \
                }} \
            }} else {{ \
                for (size_t __i = 0; __i < __other.cap; __i++) {{ \
                    if (__other.states[__i] != 1) continue; \
                    void* __k = (char*)__other.keys + __i * __other.key_size; \
                    if (!gorget_set_contains(__self, __k)) return false; \
                }} \
            }} \
            return true; \
        }}").unwrap();
    }
    // is_disjoint: no element appears in both.
    // D39 Phase A.2b (DORMANT): dense branch on `__self.entries_keys`; see
    // is_subset above for the invariant.
    if has_extern("gorget_set_is_disjoint") {
        writeln!(out, "static inline bool gorget_set_is_disjoint(void* __self_ptr, GorgetSet __other) {{ \
            GorgetSet __self = *(GorgetSet*)__self_ptr; \
            if (__self.entries_keys) {{ \
                for (size_t __i = 0; __i < __self.entries_len; __i++) {{ \
                    void* __k = (char*)__self.entries_keys + __i * __self.key_size; \
                    if (gorget_set_contains(&__other, __k)) return false; \
                }} \
            }} else {{ \
                for (size_t __i = 0; __i < __self.cap; __i++) {{ \
                    if (__self.states[__i] != 1) continue; \
                    void* __k = (char*)__self.keys + __i * __self.key_size; \
                    if (gorget_set_contains(&__other, __k)) return false; \
                }} \
            }} \
            return true; \
        }}").unwrap();
    }

    // gorget_map_new_like(src): fresh empty GorgetMap that mirrors
    // `src`'s config fields (key_size / val_size / hash / eq / all
    // drop/clone/materialize hooks). Used by the BIR expansion of
    // `Dict.filter` (see src/bir/lower.rs::expand_dict_filter) so the
    // result's per-element-type wiring matches the source exactly.
    // Discriminates ordered vs unordered via the BOTH-DISCRIMINATOR form
    // `(src->entries_keys || src->order)` (D39 Phase A.2b): under current
    // (legacy Dict → `->order != NULL`) and future (dense Dict → post-A.2c
    // `->entries_keys != NULL`) states both fire → correct Dict ctor picked.
    if has_extern("gorget_map_new_like") {
        writeln!(out, "static inline GorgetMap gorget_map_new_like(const GorgetMap* __src) {{ \
            GorgetMap __dst = (__src->entries_keys || __src->order) \
                ? gorget_dict_new(__src->key_size, __src->val_size) \
                : gorget_map_new(__src->key_size, __src->val_size); \
            __dst.hash_fn = __src->hash_fn; \
            __dst.eq_fn = __src->eq_fn; \
            __dst.key_drop = __src->key_drop; \
            __dst.key_clone = __src->key_clone; \
            __dst.key_materialize = __src->key_materialize; \
            __dst.val_drop = __src->val_drop; \
            __dst.val_clone = __src->val_clone; \
            __dst.val_materialize = __src->val_materialize; \
            return __dst; \
        }}").unwrap();
    }

    // gorget_map_update(dst, src): merge src's entries into dst via
    // put_cloned. Type-independent: walks src by `cap`/`states` using
    // its `key_size` / `val_size` fields (legacy) or by `entries_keys` /
    // `entries_values` / `entries_len` (dense). Used by `Dict.update(other)`
    // / `HashMap.update(other)`.
    // D39 Phase A.2b (DORMANT): dense branch on `__other.entries_keys`.
    if has_extern("gorget_map_update") {
        writeln!(out, "static inline void gorget_map_update(void* __dst_ptr, GorgetMap __other) {{ \
            GorgetMap* __dst = (GorgetMap*)__dst_ptr; \
            if (__other.entries_keys) {{ \
                for (size_t __i = 0; __i < __other.entries_len; __i++) {{ \
                    void* __k = (char*)__other.entries_keys + __i * __other.key_size; \
                    void* __v = (char*)__other.entries_values + __i * __other.val_size; \
                    gorget_map_put_cloned(__dst, __k, __v); \
                }} \
            }} else {{ \
                for (size_t __i = 0; __i < __other.cap; __i++) {{ \
                    if (__other.states[__i] != 1) continue; \
                    void* __k = (char*)__other.keys + __i * __other.key_size; \
                    void* __v = (char*)__other.values + __i * __other.val_size; \
                    gorget_map_put_cloned(__dst, __k, __v); \
                }} \
            }} \
        }}").unwrap();
    }

    // Type-independent set-op stubs (union / intersection / difference /
    // symmetric_difference). All walk `src->order[]` for ordered sets
    // (preserving insertion order) and `src->cap/states` for unordered;
    // the result inherits hash/eq/drop/clone/materialize from `__self`
    // via `gorget_set_new_like`. One stub covers every element type
    // since the iteration is driven by runtime `key_size`.
    //
    // `gorget_set_new_like` is also used directly by the BIR expansion
    // of `Set.filter` (see src/bir/lower.rs::expand_set_filter), so
    // gate its emission on any of the set-ops OR the helper itself
    // appearing as an extern.
    if has_extern("gorget_set_new_like")
        || has_extern("gorget_set_union")
        || has_extern("gorget_set_intersection")
        || has_extern("gorget_set_difference")
        || has_extern("gorget_set_symmetric_difference")
    {
        // Fresh GorgetSet that mirrors `src`'s config fields.
        // D39 Phase A.2b (DORMANT): BOTH-DISCRIMINATOR ternary picks the
        // Set (ordered) ctor under both current (legacy Set → `->order != NULL`)
        // and future (dense Set → post-A.2c `->entries_keys != NULL`) states.
        writeln!(out, "static inline GorgetSet gorget_set_new_like(const GorgetSet* __src) {{ \
            GorgetSet __dst = (__src->entries_keys || __src->order) \
                ? gorget_dict_new(__src->key_size, 0) \
                : gorget_map_new(__src->key_size, 0); \
            __dst.hash_fn = __src->hash_fn; \
            __dst.eq_fn = __src->eq_fn; \
            __dst.key_drop = __src->key_drop; \
            __dst.key_clone = __src->key_clone; \
            __dst.key_materialize = __src->key_materialize; \
            return __dst; \
        }}").unwrap();
    }
    // D39 Phase A.2b (DORMANT): three-way discriminator on both self and
    // other — dense (entries_keys) → walk entries_len; else ordered legacy
    // (order[]) → walk order_len; else unordered legacy → walk cap+states.
    if has_extern("gorget_set_union") {
        writeln!(out, "static inline GorgetSet gorget_set_union(void* __self_ptr, GorgetSet __other) {{ \
            GorgetSet* __self = (GorgetSet*)__self_ptr; \
            GorgetSet __result = gorget_set_new_like(__self); \
            if (__self->entries_keys) {{ \
                for (size_t __i = 0; __i < __self->entries_len; __i++) {{ \
                    gorget_map_put_cloned(&__result, (char*)__self->entries_keys + __i * __self->key_size, NULL); \
                }} \
            }} else if (__self->order) {{ \
                for (size_t __j = 0; __j < __self->order_len; __j++) {{ \
                    size_t __i = __self->order[__j]; \
                    if (__self->states[__i] != 1) continue; \
                    gorget_map_put_cloned(&__result, (char*)__self->keys + __i * __self->key_size, NULL); \
                }} \
            }} else {{ \
                for (size_t __i = 0; __i < __self->cap; __i++) {{ \
                    if (__self->states[__i] != 1) continue; \
                    gorget_map_put_cloned(&__result, (char*)__self->keys + __i * __self->key_size, NULL); \
                }} \
            }} \
            if (__other.entries_keys) {{ \
                for (size_t __i = 0; __i < __other.entries_len; __i++) {{ \
                    gorget_map_put_cloned(&__result, (char*)__other.entries_keys + __i * __other.key_size, NULL); \
                }} \
            }} else if (__other.order) {{ \
                for (size_t __j = 0; __j < __other.order_len; __j++) {{ \
                    size_t __i = __other.order[__j]; \
                    if (__other.states[__i] != 1) continue; \
                    gorget_map_put_cloned(&__result, (char*)__other.keys + __i * __other.key_size, NULL); \
                }} \
            }} else {{ \
                for (size_t __i = 0; __i < __other.cap; __i++) {{ \
                    if (__other.states[__i] != 1) continue; \
                    gorget_map_put_cloned(&__result, (char*)__other.keys + __i * __other.key_size, NULL); \
                }} \
            }} \
            return __result; \
        }}").unwrap();
    }
    // D39 Phase A.2b (DORMANT): three-way discriminator on self.
    if has_extern("gorget_set_intersection") {
        writeln!(out, "static inline GorgetSet gorget_set_intersection(void* __self_ptr, GorgetSet __other) {{ \
            GorgetSet* __self = (GorgetSet*)__self_ptr; \
            GorgetSet __result = gorget_set_new_like(__self); \
            if (__self->entries_keys) {{ \
                for (size_t __i = 0; __i < __self->entries_len; __i++) {{ \
                    void* __k = (char*)__self->entries_keys + __i * __self->key_size; \
                    if (gorget_set_contains(&__other, __k)) \
                        gorget_map_put_cloned(&__result, __k, NULL); \
                }} \
            }} else if (__self->order) {{ \
                for (size_t __j = 0; __j < __self->order_len; __j++) {{ \
                    size_t __i = __self->order[__j]; \
                    if (__self->states[__i] != 1) continue; \
                    void* __k = (char*)__self->keys + __i * __self->key_size; \
                    if (gorget_set_contains(&__other, __k)) \
                        gorget_map_put_cloned(&__result, __k, NULL); \
                }} \
            }} else {{ \
                for (size_t __i = 0; __i < __self->cap; __i++) {{ \
                    if (__self->states[__i] != 1) continue; \
                    void* __k = (char*)__self->keys + __i * __self->key_size; \
                    if (gorget_set_contains(&__other, __k)) \
                        gorget_map_put_cloned(&__result, __k, NULL); \
                }} \
            }} \
            return __result; \
        }}").unwrap();
    }
    // D39 Phase A.2b (DORMANT): three-way discriminator on self.
    if has_extern("gorget_set_difference") {
        writeln!(out, "static inline GorgetSet gorget_set_difference(void* __self_ptr, GorgetSet __other) {{ \
            GorgetSet* __self = (GorgetSet*)__self_ptr; \
            GorgetSet __result = gorget_set_new_like(__self); \
            if (__self->entries_keys) {{ \
                for (size_t __i = 0; __i < __self->entries_len; __i++) {{ \
                    void* __k = (char*)__self->entries_keys + __i * __self->key_size; \
                    if (!gorget_set_contains(&__other, __k)) \
                        gorget_map_put_cloned(&__result, __k, NULL); \
                }} \
            }} else if (__self->order) {{ \
                for (size_t __j = 0; __j < __self->order_len; __j++) {{ \
                    size_t __i = __self->order[__j]; \
                    if (__self->states[__i] != 1) continue; \
                    void* __k = (char*)__self->keys + __i * __self->key_size; \
                    if (!gorget_set_contains(&__other, __k)) \
                        gorget_map_put_cloned(&__result, __k, NULL); \
                }} \
            }} else {{ \
                for (size_t __i = 0; __i < __self->cap; __i++) {{ \
                    if (__self->states[__i] != 1) continue; \
                    void* __k = (char*)__self->keys + __i * __self->key_size; \
                    if (!gorget_set_contains(&__other, __k)) \
                        gorget_map_put_cloned(&__result, __k, NULL); \
                }} \
            }} \
            return __result; \
        }}").unwrap();
    }
    // D39 Phase A.2b (DORMANT): three-way discriminator on both self and other.
    if has_extern("gorget_set_symmetric_difference") {
        writeln!(out, "static inline GorgetSet gorget_set_symmetric_difference(void* __self_ptr, GorgetSet __other) {{ \
            GorgetSet* __self = (GorgetSet*)__self_ptr; \
            GorgetSet __result = gorget_set_new_like(__self); \
            if (__self->entries_keys) {{ \
                for (size_t __i = 0; __i < __self->entries_len; __i++) {{ \
                    void* __k = (char*)__self->entries_keys + __i * __self->key_size; \
                    if (!gorget_set_contains(&__other, __k)) \
                        gorget_map_put_cloned(&__result, __k, NULL); \
                }} \
            }} else if (__self->order) {{ \
                for (size_t __j = 0; __j < __self->order_len; __j++) {{ \
                    size_t __i = __self->order[__j]; \
                    if (__self->states[__i] != 1) continue; \
                    void* __k = (char*)__self->keys + __i * __self->key_size; \
                    if (!gorget_set_contains(&__other, __k)) \
                        gorget_map_put_cloned(&__result, __k, NULL); \
                }} \
            }} else {{ \
                for (size_t __i = 0; __i < __self->cap; __i++) {{ \
                    if (__self->states[__i] != 1) continue; \
                    void* __k = (char*)__self->keys + __i * __self->key_size; \
                    if (!gorget_set_contains(&__other, __k)) \
                        gorget_map_put_cloned(&__result, __k, NULL); \
                }} \
            }} \
            if (__other.entries_keys) {{ \
                for (size_t __i = 0; __i < __other.entries_len; __i++) {{ \
                    void* __k = (char*)__other.entries_keys + __i * __other.key_size; \
                    if (!gorget_set_contains(__self, __k)) \
                        gorget_map_put_cloned(&__result, __k, NULL); \
                }} \
            }} else if (__other.order) {{ \
                for (size_t __j = 0; __j < __other.order_len; __j++) {{ \
                    size_t __i = __other.order[__j]; \
                    if (__other.states[__i] != 1) continue; \
                    void* __k = (char*)__other.keys + __i * __other.key_size; \
                    if (!gorget_set_contains(__self, __k)) \
                        gorget_map_put_cloned(&__result, __k, NULL); \
                }} \
            }} else {{ \
                for (size_t __i = 0; __i < __other.cap; __i++) {{ \
                    if (__other.states[__i] != 1) continue; \
                    void* __k = (char*)__other.keys + __i * __other.key_size; \
                    if (!gorget_set_contains(__self, __k)) \
                        gorget_map_put_cloned(&__result, __k, NULL); \
                }} \
            }} \
            return __result; \
        }}").unwrap();
    }

    // gorget_array_windows(arr, n): Vector[Vector[T]] of sliding N-sized
    // slices. Elements are bit-copied (no clone); correct for POD types.
    // Uses src->elem_size so one stub covers every T.
    if has_extern("gorget_array_windows") {
        writeln!(out, "static inline GorgetArray gorget_array_windows(void* __arr_ptr, int64_t __n) {{ \
            GorgetArray* __src = (GorgetArray*)__arr_ptr; \
            GorgetArray __result = gorget_array_new_drop(sizeof(GorgetArray), (__gorget_drop_fn)gorget_array_free); \
            if (__n <= 0 || (size_t)__n > __src->len) return __result; \
            size_t __es = __src->elem_size; \
            for (size_t __i = 0; __i + (size_t)__n <= __src->len; __i++) {{ \
                GorgetArray __w = gorget_array_new(__es); \
                for (size_t __j = __i; __j < __i + (size_t)__n; __j++) {{ \
                    void* __e = (char*)__src->data + __j * __es; \
                    gorget_array_push(&__w, __e); \
                }} \
                gorget_array_push(&__result, &__w); \
            }} \
            return __result; \
        }}").unwrap();
    }
    // gorget_array_chunks(arr, n): Vector[Vector[T]] of N-sized disjoint
    // slices; last chunk may be shorter.
    if has_extern("gorget_array_chunks") {
        writeln!(out, "static inline GorgetArray gorget_array_chunks(void* __arr_ptr, int64_t __n) {{ \
            GorgetArray* __src = (GorgetArray*)__arr_ptr; \
            GorgetArray __result = gorget_array_new_drop(sizeof(GorgetArray), (__gorget_drop_fn)gorget_array_free); \
            if (__n <= 0) return __result; \
            size_t __es = __src->elem_size; \
            for (size_t __i = 0; __i < __src->len; __i += (size_t)__n) {{ \
                GorgetArray __c = gorget_array_new(__es); \
                size_t __end = __i + (size_t)__n; \
                if (__end > __src->len) __end = __src->len; \
                for (size_t __j = __i; __j < __end; __j++) {{ \
                    void* __e = (char*)__src->data + __j * __es; \
                    gorget_array_push(&__c, __e); \
                }} \
                gorget_array_push(&__result, &__c); \
            }} \
            return __result; \
        }}").unwrap();
    }
    // gorget_array_zip: pair elements from two arrays into an array of tuples
    if has_extern("gorget_array_zip") {
        // Tuple struct: { _0: A, _1: B }.  We compute tuple_size from the two elem_sizes.
        // Both fields are at least 8-byte aligned in Gorget, so offset_1 = round_up(a_size, 8).
        writeln!(out, "static inline GorgetArray gorget_array_zip(void* __arr_ptr, GorgetArray __b) {{ \
            GorgetArray* __a = (GorgetArray*)__arr_ptr; \
            size_t __min = __a->len < __b.len ? __a->len : __b.len; \
            size_t __a_sz = __a->elem_size; \
            size_t __b_sz = __b.elem_size; \
            size_t __off1 = (__a_sz + 7) & ~(size_t)7; \
            size_t __tuple_sz = __off1 + ((__b_sz + 7) & ~(size_t)7); \
            GorgetArray __r = gorget_array_new(__tuple_sz); \
            char __sbuf[256]; \
            char* __buf = __tuple_sz <= sizeof(__sbuf) ? __sbuf : (char*)malloc(__tuple_sz); \
            for (size_t __i = 0; __i < __min; __i++) {{ \
                memset(__buf, 0, __tuple_sz); \
                memcpy(__buf, (char*)__a->data + __i * __a_sz, __a_sz); \
                memcpy(__buf + __off1, (char*)__b.data + __i * __b_sz, __b_sz); \
                gorget_array_push(&__r, __buf); \
            }} \
            if (__buf != __sbuf) free(__buf); \
            return __r; }}").unwrap();
    }
    // codepoint_to_str: used by encoding/toml fixtures
    if has_extern("codepoint_to_str") {
        writeln!(out, "static inline Str codepoint_to_str(int64_t code) {{ return gorget_codepoint_to_utf8(code); }}").unwrap();
    }
    // __gorget_file_read_all_r: LLVM backend wrapper for gorget_file_read_all.
    // The C runtime returns GorgetString directly, but LIR expects Result<GorgetString,GorgetString>.
    // This wrapper calls gorget_file_read_all and wraps the result in the proper Result struct.
    // Must come AFTER struct definitions so __gg_Result__GorgetString__GorgetString is available.
    if has_extern("gorget_file_read_all") {
        let result_c_name = module.externs.iter()
            .find(|e| e.name == "gorget_file_read_all")
            .and_then(|e| if let crate::lir::LirType::Struct(sid) = &e.return_type {
                struct_names.get(&sid.0).cloned()
            } else { None })
            .or_else(|| {
                // Fallback: the LIR lowerer's last_error lift may have changed the
                // extern's return_type to the ok payload.  Find the Result struct by name.
                module.structs.iter().enumerate().find_map(|(i, s)| {
                    if s.name == "Result__GorgetString__GorgetString" {
                        struct_names.get(&(i as u32)).cloned()
                    } else { None }
                })
            });
        if let Some(result_ty) = result_c_name {
            writeln!(out, "{result_ty} __gorget_file_read_all_r(GorgetFile* f) {{").unwrap();
            writeln!(out, "    GorgetString gs = gorget_file_read_all(f);").unwrap();
            writeln!(out, "    {result_ty} r;").unwrap();
            writeln!(out, "    memset(&r, 0, sizeof(r));").unwrap();
            writeln!(out, "    if (gorget_utf8_validate((const char*)gs.data, gs.len)) {{").unwrap();
            writeln!(out, "        r.tag = 0; r.Ok_0 = gs;").unwrap();
            writeln!(out, "    }} else {{").unwrap();
            writeln!(out, "        gorget_string_free(&gs);").unwrap();
            writeln!(out, "        r.tag = 1;").unwrap();
            writeln!(out, "        r.Error_0 = gorget_str_from_literal(\"invalid UTF-8\", 13);").unwrap();
            writeln!(out, "    }}").unwrap();
            writeln!(out, "    return r;").unwrap();
            writeln!(out, "}}").unwrap();
        }
    }
    // NOTE: int64_t__parse, double__parse etc. are monomorphized parse methods.
    // They're too complex to emit as inline C here due to GorgetParseIntResult types
    // and Option struct name mismatches. They remain as link errors for now.
}
