//! LIR → C backend.
//!
//! Thin 1:1 translation from LIR to C code. No semantic decisions —
//! all type coercions, drop calls, vtable dispatch, etc. are already
//! explicit in LIR instructions.

use crate::lir::*;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;

mod emit_call_extern;
mod emit_types;
mod helpers;
use self::helpers::*;
use self::emit_types::*;

/// Per-function analysis context for instruction emission.
/// Consolidates the parallel arrays that were previously passed as
/// 10+ individual parameters to emit_inst() and emit_call_extern().
pub(crate) struct EmitContext<'a> {
    pub func: &'a LirFunction,
    pub module: &'a LirModule,
    /// StructId → C type name mapping.
    pub sn: &'a HashMap<u32, String>,
    /// Per-value inferred types (indexed by ValueId).
    pub val_types: &'a [Option<LirType>],
    /// Per-value: true if the value comes from a StrLit instruction.
    pub str_lit_vals: &'a [bool],
    /// Per-value: true if the value comes from a cstr-returning extern.
    pub cstr_vals: &'a [bool],
    /// Per-value: true if the value is from an extern with cstr return.
    pub extern_cstr_return_vals: &'a [bool],
    /// Per-value: true if the value is NullPtr.
    pub null_vals: &'a [bool],
    /// Per-value: the pointee type (if the value is a pointer).
    pub ptr_pointee: &'a [Option<LirType>],
    /// Per-value: the FuncId target (if the value is a FuncAddr).
    pub func_addr_targets: &'a [Option<FuncId>],
    /// Per-value: the source function name (for spawn).
    pub spawn_source_fn: &'a [Option<String>],
    /// String content → static literal index (Phase 3).
    pub string_lit_map: &'a HashMap<String, usize>,
}

/// Names of structs provided by the Gorget C runtime — these should NOT
/// be re-defined by the LIR backend.
const RUNTIME_STRUCTS: &[&str] = &[
    "Str", "GorgetString", "GorgetArray", "GorgetClosure",
    "TraitObj",
    "GorgetMap", "GorgetSet",
];

/// Structs defined by the LIR (not in the C runtime) that need their
/// original name preserved — not renamed to `__lir_sN`.
const LIR_NAMED_STRUCTS: &[&str] = &[
    "TaskHandle", "GorgetRange",
    "ExecResult", "GorgetCipherContext", "GorgetX25519KeyPair",
    "GorgetArena", "GorgetArenaCheckpoint",
    "GorgetPoolAllocator", "GorgetTlsfAllocator",
    "GorgetFixedBufferAllocator", "GorgetFallbackAllocator",
    "GorgetFile", "GorgetError",
];

/// Maps LIR struct names to their runtime C names when they differ.
fn lir_to_runtime_name(name: &str) -> Option<&'static str> {
    match name {
        "GorgetString" => Some("Str"),
        "ArenaCheckpoint" => Some("GorgetArenaCheckpoint"),
        "Socket" => Some("GorgetSocket"),
        "ServerSocket" => Some("GorgetServerSocket"),
        "TlsSocket" => Some("GorgetTlsSocket"),
        "TlsServerSocket" => Some("GorgetTlsServerSocket"),
        "UdpSocket" => Some("GorgetUdpSocket"),
        "UdpAddr" => Some("GorgetUdpAddr"),
        "UdpPacket" => Some("GorgetUdpPacket"),
        "Semaphore" => Some("GorgetSemaphore"),
        "WaitGroup" => Some("GorgetWaitGroup"),
        "OnceFlag" => Some("GorgetOnceFlag"),
        "CipherContext" => Some("GorgetCipherContext"),
        "BigNum" => Some("GorgetBigNum"),
        "RSAKey" => Some("GorgetRSAKey"),
        "Ed25519KeyPair" => Some("GorgetEd25519KeyPair"),
        "X25519KeyPair" => Some("GorgetX25519KeyPair"),
        "Regex" | "RegexMatch" | "Match" => {
            match name {
                "Regex" => Some("GorgetRegex"),
                "RegexMatch" | "Match" => Some("GorgetRegexMatch"),
                _ => None,
            }
        }
        "File" => Some("GorgetFile"),
        "Barrier" => Some("GorgetBarrier"),
        "CondVar" => Some("GorgetCondVar"),
        "AtomicInt" => Some("GorgetAtomicInt"),
        "AtomicBool" => Some("GorgetAtomicBool"),
        "Process" => Some("GorgetProcess"),
        // SDL types
        "SDLWindow" => Some("GorgetSDLWindow"),
        "SDLRenderer" => Some("GorgetSDLRenderer"),
        "SDLTexture" => Some("GorgetSDLTexture"),
        "SDLFont" => Some("GorgetSDLFont"),
        "SDLEvent" => Some("GorgetSDLEvent"),
        // Audio types
        "AudioChunk" => Some("GorgetAudioChunk"),
        "AudioMusic" => Some("GorgetAudioMusic"),
        // GL types — GorgetGLContext is typedef'd to int64_t in runtime
        "GLContext" => Some("GorgetGLContext"),
        _ => None,
    }
}

/// Build a mapping from StructId → C type name.
/// Runtime-provided structs use their real names; user structs use `__lir_s{id}`.
fn build_struct_names(module: &LirModule) -> HashMap<u32, String> {
    let mut map = HashMap::new();
    for (i, def) in module.structs.iter().enumerate() {
        if let Some(rt_name) = lir_to_runtime_name(&def.name) {
            map.insert(i as u32, rt_name.to_string());
        } else if RUNTIME_STRUCTS.contains(&def.name.as_str())
            || LIR_NAMED_STRUCTS.contains(&def.name.as_str()) {
            map.insert(i as u32, def.name.clone());
        } else if is_monomorphized_wrapper_type(&def.name) {
            // Channel__T, Shared__T, Weak__T, etc. — use their LIR name as C name.
            // The typedef will be emitted separately.
            map.insert(i as u32, def.name.clone());
        } else {
            // Use original name for debuggability, prefixed to avoid C name collisions.
            // Sanitize: replace non-alphanumeric chars with underscores.
            let sanitized: String = def.name.chars()
                .map(|c| if c.is_alphanumeric() || c == '_' { c } else { '_' })
                .collect();
            map.insert(i as u32, format!("__gg_{sanitized}"));
        }
    }
    map
}

/// Returns true for monomorphized wrapper types that need typedefs to runtime types.
fn is_monomorphized_wrapper_type(name: &str) -> bool {
    name.starts_with("Channel__")
        || name.starts_with("Shared__")
        || name.starts_with("Weak__")
        || name.starts_with("Vector__")
        || name.starts_with("Dict__")
        || name.starts_with("Set__")
        || name.starts_with("HashMap__")
        || name.starts_with("HashSet__")
        || name.starts_with("Mutex__")
        || name.starts_with("RWLock__")
        || name.starts_with("Guard__")
        || name.starts_with("ReadGuard__")
        || name.starts_with("WriteGuard__")
        || name == "AtomicInt"
        || name == "AtomicBool"
        || name == "TaskGroup"
        || name.starts_with("Task__")
        || name.starts_with("Box__")
}

/// Generate C code from an LIR module.
pub fn generate_c(module: &LirModule) -> String {
    generate_c_inner(module, true)
}

/// Generate C wrapper code for the LLVM backend.
///
/// Emits struct definitions, forward declarations, monomorphized wrappers
/// (drops, clones, combinators, spawn/await, channels, etc.), adapter
/// functions, globals, and test runner main — everything EXCEPT user
/// function bodies (those live in LLVM IR).
///
/// This output is appended to the C runtime source and compiled to a
/// separate .o that links with the LLVM-generated .o.
pub fn generate_llvm_wrappers(module: &LirModule) -> String {
    generate_c_inner_impl(module, false, true)
}

/// Generate C code from an LIR module, optionally including the Gorget runtime.
pub fn generate_c_inner(module: &LirModule, include_runtime: bool) -> String {
    generate_c_inner_impl(module, include_runtime, false)
}

/// Core C code generator.
///   - `include_runtime`:     embed the full Gorget C runtime at the top
///   - `wrappers_only`:       emit struct defs, forward decls, wrappers,
///                            globals, and test runner — but skip function bodies
fn generate_c_inner_impl(module: &LirModule, include_runtime: bool, wrappers_only: bool) -> String {
    let struct_names = build_struct_names(module);
    let mut out = String::with_capacity(if include_runtime { 256 * 1024 } else { 4096 });

    if include_runtime {
        emit_runtime_modules(&mut out, module, &struct_names);
    } else if wrappers_only {
        // Appended to runtime .c — headers already present.
        writeln!(out, "\n// ── LLVM Wrapper Glue ──").unwrap();
        // LIR helpers (char ops, hash, default values, comparison functions)
        // These are normally part of emit_runtime_modules but needed for wrappers too.
        emit_lir_helpers(&mut out, module);
    } else {
        // Minimal headers for standalone mode
        writeln!(out, "#include <stdint.h>").unwrap();
        writeln!(out, "#include <stdbool.h>").unwrap();
        writeln!(out, "#include <stdio.h>").unwrap();
        writeln!(out, "#include <string.h>").unwrap();
        writeln!(out, "#include <stdlib.h>").unwrap();
        writeln!(out).unwrap();
    }

    // Struct forward declarations (skip runtime-provided structs and monomorphized wrappers)
    // These structs are already defined in the C runtime, so emitting them again
    // would cause redefinition errors.
    let runtime_defined_named = &[
        "ExecResult", "GorgetCipherContext", "GorgetX25519KeyPair",
        "GorgetArena", "GorgetArenaCheckpoint",
        "GorgetPoolAllocator", "GorgetTlsfAllocator",
        "GorgetFixedBufferAllocator", "GorgetFallbackAllocator",
        "GorgetFile", "GorgetError",
        "GorgetSemaphore", "GorgetWaitGroup", "GorgetOnceFlag",
        "GorgetUdpAddr", "GorgetUdpPacket",
        "GorgetSocket", "GorgetServerSocket",
        "GorgetTlsSocket", "GorgetTlsServerSocket",
        "GorgetUdpSocket",
        "GorgetBigNum", "GorgetRSAKey", "GorgetEd25519KeyPair",
        "GorgetRegex", "GorgetRegexMatch",
    ];
    let skip_struct = |def: &StructDef| -> bool {
        RUNTIME_STRUCTS.contains(&def.name.as_str())
            || runtime_defined_named.contains(&def.name.as_str())
            || is_monomorphized_wrapper_type(&def.name)
            || lir_to_runtime_name(&def.name).is_some()
    };
    for (i, def) in module.structs.iter().enumerate() {
        if skip_struct(def) { continue; }
        let cname = &struct_names[&(i as u32)];
        writeln!(out, "typedef struct {cname} {cname};").unwrap();
    }
    // Early Task__* typedefs — these are referenced by Option__Task__*/Vector__Task__* field types
    // but their real typedef is emitted later in emit_spawn_helpers. Forward-declare them here.
    {
        let mut task_types_emitted = HashSet::new();
        for def in &module.structs {
            if def.name.starts_with("Task__") && task_types_emitted.insert(def.name.clone()) {
                writeln!(out, "typedef struct {{ void* __task; void (*__drop)(void*); }} {};", def.name).unwrap();
            }
        }
    }
    // Early Box__* typedefs — Box types appear in struct fields before their real typedef.
    // Non-trait boxes are void*, trait boxes are typedef'd to their TraitObj struct later.
    {
        let mut box_seen = HashSet::new();
        // Collect all Box__* types referenced in struct field types.
        for def in &module.structs {
            for (_, fty) in &def.fields {
                let ft = c_type_named(fty, &struct_names);
                if ft.starts_with("Box__") && box_seen.insert(ft.clone()) {
                    // Non-trait box: typedef as void*.
                    // Trait boxes will be re-typedef'd later by emit_monomorphized_typedefs.
                    writeln!(out, "typedef void* {ft};").unwrap();
                }
            }
        }
    }
    writeln!(out).unwrap();

    // Struct definitions — topologically sorted so inline struct fields are
    // defined before the structs that contain them.
    let struct_order = {
        let n = module.structs.len();
        // deps[i] = list of j where struct i depends on struct j (has Struct(j) field).
        let mut deps: Vec<Vec<usize>> = vec![Vec::new(); n];
        // dependents[j] = list of i where struct i depends on struct j.
        let mut dependents: Vec<Vec<usize>> = vec![Vec::new(); n];
        for (i, def) in module.structs.iter().enumerate() {
            for (_, fty) in &def.fields {
                if let LirType::Struct(sid) = fty {
                    let j = sid.0 as usize;
                    if j != i && j < n {
                        deps[i].push(j);
                        dependents[j].push(i);
                    }
                }
            }
        }
        // Kahn's algorithm: emit structs with no dependencies first.
        let mut in_degree: Vec<usize> = deps.iter().map(|d| d.len()).collect();
        let mut queue: std::collections::VecDeque<usize> = std::collections::VecDeque::new();
        for i in 0..n {
            if in_degree[i] == 0 {
                queue.push_back(i);
            }
        }
        let mut order = Vec::with_capacity(n);
        while let Some(k) = queue.pop_front() {
            order.push(k);
            for &i in &dependents[k] {
                in_degree[i] -= 1;
                if in_degree[i] == 0 {
                    queue.push_back(i);
                }
            }
        }
        // Any remaining nodes (cycles) — append in original order.
        if order.len() < n {
            let in_order: std::collections::HashSet<usize> = order.iter().copied().collect();
            for i in 0..n {
                if !in_order.contains(&i) {
                    order.push(i);
                }
            }
        }
        order
    };
    for &i in &struct_order {
        let def = &module.structs[i];
        if skip_struct(def) { continue; }
        let cname = &struct_names[&(i as u32)];
        let is_vtable = def.name.ends_with("_VTable");
        let is_traitobj = def.name.ends_with("_TraitObj");
        writeln!(out, "// {}", def.name).unwrap();
        writeln!(out, "struct {cname} {{").unwrap();
        if def.fields.is_empty() {
            // C doesn't allow empty structs — add a dummy byte.
            writeln!(out, "    char __pad;").unwrap();
        } else if def.is_union_layout && def.fields.len() > 1 {
            // Enum type: emit tag + union of variant structs.
            // Field 0 is always "tag" (I32). Fields 1+ are grouped by
            // variant prefix (e.g., IFunction_0, IFunction_1 → IFunction group).
            let (tag_name, tag_ty) = &def.fields[0];
            let tag_ty_str = c_type_named(tag_ty, &struct_names);
            writeln!(out, "    {} {};", tag_ty_str, c_field_name(tag_name)).unwrap();
            // Group remaining fields by variant name prefix
            let mut variants: Vec<(String, Vec<(&str, &LirType)>)> = Vec::new();
            for (fname, fty) in &def.fields[1..] {
                let variant_name = fname.rsplitn(2, '_').nth(1).unwrap_or(fname);
                if variants.last().map(|(n, _)| n.as_str()) == Some(variant_name) {
                    variants.last_mut().unwrap().1.push((fname.as_str(), fty));
                } else {
                    variants.push((variant_name.to_string(), vec![(fname.as_str(), fty)]));
                }
            }
            writeln!(out, "    union {{").unwrap();
            for (vname, fields) in &variants {
                if fields.len() == 1 {
                    let (fname, fty) = &fields[0];
                    let ty_str = if matches!(fty, LirType::Void) {
                        "uint8_t".to_string()
                    } else {
                        c_type_named(fty, &struct_names)
                    };
                    writeln!(out, "        {} {};  // {}", ty_str, c_field_name(fname), vname).unwrap();
                } else {
                    writeln!(out, "        struct {{  // {}", vname).unwrap();
                    for (fname, fty) in fields {
                        let ty_str = if matches!(fty, LirType::Void) {
                            "uint8_t".to_string()
                        } else {
                            c_type_named(fty, &struct_names)
                        };
                        writeln!(out, "            {} {};", ty_str, c_field_name(fname)).unwrap();
                    }
                    writeln!(out, "        }} {};", c_field_name(vname)).unwrap();
                }
            }
            writeln!(out, "    }} data;").unwrap();
        } else {
            for (fname, fty) in &def.fields {
                if is_vtable {
                    // VTable fields are function pointers.
                    // Look up the extern declaration for the Box__Trait__method to get full signature.
                    let trait_name = def.name.strip_suffix("_VTable").unwrap();
                    let ret_type = find_trait_method_return_type(module, trait_name, fname, &struct_names);
                    let box_method = format!("Box__{trait_name}__{fname}");
                    // Find impl function for better type info (extern may have Ptr where impl has Str).
                    let impl_fn_params: Option<&[LirType]> = module.functions.iter()
                        .find(|f| {
                            let prefix = format!("{trait_name}_for_");
                            let suffix = format!("__{fname}");
                            f.name.starts_with(&prefix) && f.name.ends_with(&suffix)
                        })
                        .map(|f| f.params.as_slice());
                    let extra_param_types: Vec<String> = module.externs.iter()
                        .find(|e| e.name == box_method)
                        .map(|e| e.params.iter().skip(1).enumerate() // skip self
                            .map(|(i, t)| {
                                let effective_ty = if matches!(t, LirType::Ptr) {
                                    impl_fn_params
                                        .and_then(|ps| ps.get(i + 1))
                                        .filter(|it| matches!(it, LirType::Struct(_)))
                                        .unwrap_or(t)
                                } else {
                                    t
                                };
                                c_type_named(effective_ty, &struct_names)
                            })
                            .collect())
                        .unwrap_or_default();
                    let params = if extra_param_types.is_empty() {
                        "const void*".to_string()
                    } else {
                        format!("const void*, {}", extra_param_types.join(", "))
                    };
                    writeln!(out, "    {ret_type} (*{})({});", c_field_name(fname), params).unwrap();
                } else if is_traitobj && fname == "vtable" {
                    // TraitObj vtable field should be a typed pointer to the VTable struct.
                    let trait_name = def.name.strip_suffix("_TraitObj").unwrap();
                    let vtable_cname = find_struct_cname_by_orig(module, &format!("{trait_name}_VTable"), &struct_names);
                    writeln!(out, "    const {vtable_cname}* {};", c_field_name(fname)).unwrap();
                } else {
                    // Void-typed fields are invalid in C — substitute uint8_t as a placeholder.
                    let ty_str = if matches!(fty, LirType::Void) {
                        "uint8_t".to_string()
                    } else {
                        c_type_named(fty, &struct_names)
                    };
                    writeln!(out, "    {} {};", ty_str, c_field_name(fname)).unwrap();
                }
            }
        }
        writeln!(out, "}};").unwrap();
        writeln!(out).unwrap();
    }

    // Emit monomorphized wrapper typedefs + inline wrappers AFTER struct definitions
    // so element types like __lir_s9 (Config) are already defined.
    if include_runtime || wrappers_only {
        emit_monomorphized_typedefs(&mut out, module, &struct_names);
    }

    // Collect thread-generated names early for extern skip logic.
    let thread_generated_names: std::collections::HashSet<String> = {
        let mut s = std::collections::HashSet::new();
        for tsf in &module.thread_spawned_fns {
            let ret_c = &tsf.ret_c_type;
            s.insert(format!("Thread__{ret_c}__join"));
            s.insert(format!("Thread__{ret_c}__id"));
            s.insert(format!("__gorget_thread_spawn_{}", tsf.fn_name));
            s.insert(format!("__gorget_thread_entry_{}", tsf.fn_name));
        }
        s
    };

    // Extern declarations (skip functions already provided by included headers or runtime)
    // Builtin type cast names that are C keywords — can't be used as function names.
    // Handled as inline casts in emit_call_extern; skip forward declarations.
    let builtin_cast_names: &[&str] = &["float", "int", "bool"];

    for ext in &module.externs {
        if is_std_header_fn(&ext.name) || is_runtime_fn(&ext.name)
            || ext.name == "codepoint_to_str"
            || ext.name == "gorget_array_reversed"
            || ext.name == "gorget_array_unique"
            || ext.name == "gorget_array_zip" {
            continue;
        }
        // Skip builtin type cast names — they're C keywords, handled as inline casts.
        if builtin_cast_names.contains(&ext.name.as_str()) {
            continue;
        }
        // Skip thread-generated functions — they're emitted by emit_thread_helpers.
        if thread_generated_names.contains(&ext.name) {
            continue;
        }
        // Skip variadic externs with no named params — these are Gorget runtime
        // functions that lack proper type info in the LIR; declaring them as
        // `int32_t foo(...)` is invalid C.  They'll be resolved at link time
        // when the runtime is included.
        if ext.is_variadic && ext.params.is_empty() {
            continue;
        }
        // Skip higher-order collection methods — generated as static inline helpers.
        if parse_vector_higher_order(&ext.name).is_some()
            || parse_dict_higher_order(&ext.name).is_some()
            || parse_set_higher_order(&ext.name).is_some() {
            continue;
        }
        // Skip Option/Result combinator methods — generated as inline helpers.
        if parse_option_result_combinator(&ext.name).is_some() {
            continue;
        }
        // Skip monomorphized wrapper methods — inline wrappers emitted separately.
        if ext.name.starts_with("Channel__") || ext.name.starts_with("Shared__")
            || ext.name.starts_with("Weak__") || ext.name.starts_with("Mutex__")
            || ext.name.starts_with("RWLock__") || ext.name.starts_with("Guard__")
            || ext.name.starts_with("ReadGuard__") || ext.name.starts_with("WriteGuard__")
            || ext.name.starts_with("Box__") {
            continue;
        }
        write!(out, "{} {}(", c_type_named(&ext.return_type, &struct_names), ext.name).unwrap();
        if ext.params.is_empty() && !ext.is_variadic {
            write!(out, "void").unwrap();
        } else {
            for (i, p) in ext.params.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                // Void as a non-sole parameter is invalid C; emit void* instead
                // (typically a closure env pointer that has no captures).
                if matches!(p, LirType::Void) {
                    write!(out, "void*").unwrap();
                } else {
                    write!(out, "{}", c_type_named(p, &struct_names)).unwrap();
                }
            }
            if ext.is_variadic {
                if !ext.params.is_empty() {
                    write!(out, ", ").unwrap();
                }
                write!(out, "...").unwrap();
            }
        }
        writeln!(out, ");").unwrap();
    }
    if !module.externs.is_empty() {
        writeln!(out).unwrap();
    }

    // Box allocators and inline runtime helpers
    if include_runtime || wrappers_only {
        emit_runtime_helpers(&mut out, module, &struct_names);
    }

    // Global declarations (split: plain globals first, then vtable globals after function forward decls)
    let has_func_addrs = |init: &LirGlobalInit| -> bool {
        fn check(init: &LirGlobalInit) -> bool {
            match init {
                LirGlobalInit::FuncAddr(_) => true,
                LirGlobalInit::Struct { fields, .. } => fields.iter().any(check),
                _ => false,
            }
        }
        check(init)
    };
    let mut deferred_globals: Vec<usize> = Vec::new();
    for (i, g) in module.globals.iter().enumerate() {
        if has_func_addrs(&g.init) {
            deferred_globals.push(i);
            continue;
        }
        let kw = if g.is_const { "const " } else { "" };
        write!(out, "{kw}{} __lir_g{i}", c_type_named(&g.ty, &struct_names)).unwrap();
        emit_global_init(&mut out, &g.init, &g.ty, &module.functions, &module.structs);
        writeln!(out, "; // {}", g.name).unwrap();
    }
    if !module.globals.is_empty() {
        writeln!(out).unwrap();
    }

    // Static string literals — collect unique StrLit values and emit static header+data
    // structs. Each gets cap=0 (literal, don't free). References use the data pointer.
    let string_lit_map: HashMap<String, usize> = {
        let mut map = HashMap::new();
        for func in &module.functions {
            for block in &func.blocks {
                for inst in &block.insts {
                    if let Inst::StrLit { value, .. } = inst {
                        let len = map.len();
                        map.entry(value.clone()).or_insert(len);
                    }
                }
            }
        }
        map
    };
    // Build reverse map: StrLit value_id → static index (for future use)
    let _max_val = module.functions.iter()
        .flat_map(|f| f.blocks.iter().flat_map(|b| b.insts.iter()))
        .filter_map(|i| i.dst().map(|d| d.0))
        .max().unwrap_or(0);
    let mut str_lit_static_idx: Vec<Option<usize>> = vec![None; _max_val as usize + 1];
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::StrLit { dst, value } = inst {
                    if let Some(&idx) = string_lit_map.get(value) {
                        if (dst.0 as usize) < str_lit_static_idx.len() {
                            str_lit_static_idx[dst.0 as usize] = Some(idx);
                        }
                    }
                }
            }
        }
    }
    if !string_lit_map.is_empty() {
        writeln!(out, "// ── Static string literals (views into .rodata, cap=0) ──").unwrap();
        let mut sorted_lits: Vec<(&String, &usize)> = string_lit_map.iter().collect();
        sorted_lits.sort_by_key(|(_, idx)| **idx);
        for (value, idx) in sorted_lits {
            let escaped = escape_c_string(value);
            let len = value.len();
            // 32-byte Str struct: { data, cap, len, alloc }.
            // cap=0 marks view into static .rodata (free is a no-op).
            writeln!(out, "static const Str __slit_{} = {{ .data = (char*)\"{}\", .cap = 0, .len = {}, .alloc = NULL }};",
                idx, escaped, len).unwrap();
        }
        writeln!(out).unwrap();
    }

    // Function forward declarations
    for func in &module.functions {
        if thread_generated_names.contains(&func.name) {
            continue;
        }
        if builtin_cast_names.contains(&func.name.as_str()) {
            continue;
        }
        // main() uses int main(int argc, char** argv) — must match the definition.
        if func.name == "main" {
            writeln!(out, "int main(int argc, char** argv);").unwrap();
            continue;
        }
        // For throws-int main, override Result return type to int.
        let ret_type_str = c_type_named(&func.return_type, &struct_names);
        write!(out, "{} {}(", ret_type_str, c_func_name(&func.name)).unwrap();
        if func.params.is_empty() {
            write!(out, "void").unwrap();
        } else {
            for (i, p) in func.params.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                let mut ty_str = if matches!(p, LirType::Void) { "void*".to_string() } else { c_type_named(p, &struct_names) };
                if func.const_params.get(i) == Some(&true) && p.is_ptr() {
                    ty_str = format!("const {ty_str}");
                }
                write!(out, "{ty_str} __p{i}").unwrap();
            }
        }
        writeln!(out, ");").unwrap();
    }
    writeln!(out).unwrap();

    // Deferred globals (vtable constants with function pointers — must come after function forward decls)
    for &i in &deferred_globals {
        let g = &module.globals[i];
        let kw = if g.is_const { "const " } else { "" };
        write!(out, "{kw}{} __lir_g{i}", c_type_named(&g.ty, &struct_names)).unwrap();
        emit_global_init(&mut out, &g.init, &g.ty, &module.functions, &module.structs);
        writeln!(out, "; // {}", g.name).unwrap();
    }
    if !deferred_globals.is_empty() {
        writeln!(out).unwrap();
    }

    // Higher-order collection helpers (filter, map, fold, any, all, etc.)
    // Must come after function forward declarations so closure __call functions are visible.
    if include_runtime || wrappers_only {
        emit_higher_order_collection_helpers(&mut out, module, &struct_names);
        emit_option_result_combinator_helpers(&mut out, module, &struct_names);
    }

    // Spawn/await helpers for async functions (blocking approach).
    if !module.spawned_fns.is_empty() && (include_runtime || wrappers_only) {
        emit_spawn_helpers(&mut out, module);
    }

    // Thread spawn/join helpers.
    if !module.thread_spawned_fns.is_empty() && (include_runtime || wrappers_only) {
        emit_thread_helpers(&mut out, module);
    }

    // Adapter functions for named functions passed as closures (FuncAddr → void* protocol).
    // When a named function is passed where a closure (void*) is expected, the call site
    // wraps it as (void*[2]){__adapt_fn, NULL}. The adapter ignores the env pointer and
    // forwards to the real function.
    {
        let mut adapter_fids: HashSet<u32> = HashSet::new();
        for func in &module.functions {
            for block in &func.blocks {
                for inst in &block.insts {
                    if let Inst::FuncAddr { func: fid, .. } = inst {
                        adapter_fids.insert(fid.0);
                    }
                }
            }
        }
        for fid_raw in &adapter_fids {
            let target = &module.functions[*fid_raw as usize];
            let ret_c = c_type_named(&target.return_type, &struct_names);
            let adapt_name = format!("__adapt_{}", c_func_name(&target.name));
            let target_name = c_func_name(&target.name);
            // Signature: ret_type __adapt_fn(void* __env, params...)
            write!(out, "{ret_c} {adapt_name}(void* __env").unwrap();
            for (i, p) in target.params.iter().enumerate() {
                let ty_str = if matches!(p, LirType::Void) { "void*".to_string() } else { c_type_named(p, &struct_names) };
                write!(out, ", {ty_str} __p{i}").unwrap();
            }
            write!(out, ") {{ ").unwrap();
            if !matches!(target.return_type, LirType::Void) {
                write!(out, "return ").unwrap();
            }
            write!(out, "{target_name}(").unwrap();
            for (i, _) in target.params.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write!(out, "__p{i}").unwrap();
            }
            writeln!(out, "); }}").unwrap();
        }
        if !adapter_fids.is_empty() {
            writeln!(out).unwrap();
        }
    }

    // Hot-reload: emit a typedef so the guest wrappers can use the original state type name.
    if module.hot_reload {
        if let Some(ref state_type) = module.hot_reload_state_type {
            // Find the LIR-mangled C name for this struct.
            if let Some((i, _)) = module.structs.iter().enumerate().find(|(_, s)| s.name == *state_type) {
                let c_name = struct_names.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"));
                if c_name != *state_type {
                    writeln!(out, "typedef {c_name} {state_type};").unwrap();
                }
            }
        }
    }

    // Forward-declare enum drop and clone functions (needed when enum A's
    // drop/clone calls B__drop/B__clone before B's definition).
    for (idx, sdef) in module.structs.iter().enumerate() {
        if module.recursive_drop_enums.contains_key(sdef.name.as_str()) {
            let c_name = struct_names.get(&(idx as u32)).cloned().unwrap_or_else(|| sdef.name.clone());
            let drop_fn = format!("{}__drop", sdef.name);
            if !module.functions.iter().any(|f| f.name == drop_fn) {
                writeln!(out, "void {drop_fn}(void*);").unwrap();
            }
            let clone_fn = format!("{}__clone", sdef.name);
            if !module.functions.iter().any(|f| f.name == clone_fn) {
                writeln!(out, "{c_name} {clone_fn}(void*);").unwrap();
            }
        }
    }
    // Emit struct drop functions for structs with Recursive drop strategy.
    // These are needed when a Recursive-drop struct appears as a field in
    // another struct — the parent's field drop calls {Name}__drop.
    emit_recursive_struct_drops(&mut out, module, &struct_names);
    emit_recursive_struct_clones(&mut out, module, &struct_names);
    emit_recursive_enum_clones(&mut out, module, &struct_names);
    // Forward-declare __gorget_dtor_* functions so enum drop functions
    // can reference them (they're defined later in emit_type_drop_fns).
    for info in module.type_drop_fns.values() {
        if info.drop_fn_name.starts_with("__gorget_dtor_") {
            writeln!(out, "void {}(void* __p);", info.drop_fn_name).unwrap();
        }
    }
    emit_enum_drop_fns(&mut out, module, &struct_names);
    emit_type_drop_fns(&mut out, module, &struct_names);

    // Function definitions (skipped in wrappers-only mode — bodies live in LLVM IR)
    if !wrappers_only {
        writeln!(out, "// ── Function Definitions ──").unwrap();
        let has_test_runner = !module.test_fns.is_empty() || !module.bench_fns.is_empty() || module.is_test_module;
        for func in &module.functions {
            if has_test_runner && func.name == "main" {
                continue;
            }
            emit_function(&mut out, func, module, &struct_names, &string_lit_map);
            writeln!(out).unwrap();
        }
    }

    // Bench runner main — bench function bodies are lowered to LIR as __bench_N functions.
    if !module.bench_fns.is_empty() && module.functions.iter().any(|f| f.name.starts_with("__bench_")) {
        emit_bench_runner_main(&mut out, module);
    } else if !module.test_fns.is_empty() || module.is_test_module {
        emit_test_runner_main(&mut out, module);
    }

    out
}


fn emit_function(out: &mut String, func: &LirFunction, module: &LirModule, sn: &HashMap<u32, String>, string_lit_map: &HashMap<String, usize>) {
    // For main() with a Result return type (throws-int main), override to int.
    let is_throws_main = func.name == "main" && matches!(&func.return_type, LirType::Struct(sid) if {
        module.structs.get(sid.0 as usize).map_or(false, |s| s.name.starts_with("Result__"))
    });
    let ret_type_str = if is_throws_main { "int".to_string() } else { c_type_named(&func.return_type, sn) };

    // Signature — special-case main() to accept argc/argv for sys.argv support.
    if func.name == "main" {
        writeln!(out, "int main(int argc, char** argv) {{").unwrap();
        writeln!(out, "    gorget_init_args(argc, argv);").unwrap();
        if let Some(ref trace_path) = module.trace_filename {
            let escaped = trace_path.replace('\\', "\\\\").replace('"', "\\\"");
            writeln!(out, "    __gorget_trace_init(\"{escaped}\");").unwrap();
        }
    } else {
        write!(out, "{} {}(", ret_type_str, c_func_name(&func.name)).unwrap();
        if func.params.is_empty() {
            write!(out, "void").unwrap();
        } else {
            for (i, p) in func.params.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                // Void as non-sole param is invalid C — use void* (closure env ptr).
                let mut ty_str = if matches!(p, LirType::Void) { "void*".to_string() } else { c_type_named(p, sn) };
                if func.const_params.get(i) == Some(&true) && p.is_ptr() {
                    ty_str = format!("const {ty_str}");
                }
                write!(out, "{ty_str} __p{i}").unwrap();
            }
        }
        writeln!(out, ") {{").unwrap();

        // Trace entry: emit call event with function name, parameter values, and depth.
        if module.trace_filename.is_some() {
            if let Some(ref display_name) = func.display_name {
                let escaped = display_name.replace('\\', "\\\\").replace('"', "\\\"");
                out.push_str("    if (__gorget_trace_fp) {\n");
                let _ = writeln!(out, "        fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"call\\\",\\\"fn\\\":\\\"{escaped}\\\",\\\"args\\\":{{\");");
                for (i, p) in func.params.iter().enumerate() {
                    let formatter = lir_trace_formatter(p, module);
                    let comma = if i == 0 { "" } else { "," };
                    let pname = func.param_names.get(i)
                        .and_then(|n| n.as_deref())
                        .unwrap_or("_");
                    let esc_name = pname.replace('\\', "\\\\").replace('"', "\\\"");
                    let _ = writeln!(out, "        fprintf(__gorget_trace_fp, \"{comma}\\\"{esc_name}\\\":\");");
                    let _ = writeln!(out, "        {formatter}(__gorget_trace_fp, __p{i});");
                }
                let _ = writeln!(out, "        fprintf(__gorget_trace_fp, \"}},\\\"depth\\\":%d}}\\n\", __gorget_trace_depth++);");
                out.push_str("    }\n");
            }
        }
    }

    // Value declarations — collect all values defined in the function.
    let mut max_val = 0u32;
    for block in &func.blocks {
        for (vid, _) in &block.params {
            if vid.0 >= max_val {
                max_val = vid.0 + 1;
            }
        }
        for inst in &block.insts {
            if let Some(dst) = inst.dst() {
                if dst.0 >= max_val {
                    max_val = dst.0 + 1;
                }
            }
        }
    }

    // Declare all values as their inferred types.
    // Build a type map from instructions (two passes for arithmetic type propagation).
    let mut val_types: Vec<Option<LirType>> = vec![None; max_val as usize];
    // Track which values originate from StrLit instructions (raw `const char*`).
    let mut str_lit_vals: Vec<bool> = vec![false; max_val as usize];
    // Track which values are raw C strings (const char*) from runtime functions.
    // Only these should be wrapped with gorget_str_from_literal when stored to Str slots.
    let mut cstr_vals: Vec<bool> = vec![false; max_val as usize];
    let mut extern_cstr_return_vals: Vec<bool> = vec![false; max_val as usize];
    // Track which values are NullPtr (so we can avoid memcpy from NULL).
    let mut null_vals: Vec<bool> = vec![false; max_val as usize];
    // Track which values are FuncAddr — maps value → FuncId for adapter generation.
    let mut func_addr_targets: Vec<Option<FuncId>> = vec![None; max_val as usize];
    // Track which spawn function produced a void* value (for task_group_submit reconstruction).
    // When a spawn result is extracted to .__task (void*), we need to reconstruct Task__T
    // with the correct __drop function when passing to gorget_task_group_submit.
    let mut spawn_source_fn: Vec<Option<String>> = vec![None; max_val as usize];
    // Track the pointee type for Ptr-typed values (e.g. SlotAddr → slot type, FieldPtr → field type).
    // Used by Inst::Store to emit correct sizeof() for memcpy of aggregates.
    let mut ptr_pointee: Vec<Option<LirType>> = vec![None; max_val as usize];
    // Propagate pointee types through Ptr-typed slots (SlotStore → SlotLoad).
    let mut slot_pointee: Vec<Option<LirType>> = vec![None; func.slots.len()];
    // Override the C type name for values whose LIR type can't represent runtime structs
    // (e.g. GorgetArray, GorgetMap — not in module.structs but needed for correct C declarations).
    let mut val_c_type_override: Vec<Option<String>> = vec![None; max_val as usize];
    // Track which values came from gorget_map_get/gorget_array_get (return void* into internal storage).
    // Previously used for clone-on-read; now kept for potential future use.
    let mut _collection_get_vals: Vec<bool> = vec![false; max_val as usize];
    // Normalize PtrTo → Ptr in val_types so generic pointer handling is
    // unaffected.  PtrTo info is available from slot/param types when needed.
    let norm = |ty: LirType| -> LirType {
        if matches!(ty, LirType::PtrTo(_)) { LirType::Ptr } else { ty }
    };
    for block in &func.blocks {
        for (vid, ty) in &block.params {
            val_types[vid.0 as usize] = Some(norm(ty.clone()));
        }
        for inst in &block.insts {
            if let Some(ty) = infer_inst_type(inst, module, &val_types, &ptr_pointee, func) {
                if let Some(dst) = inst.dst() {
                    let nty = norm(ty);
                    // Catch conflicting type assignments: a value should not be inferred
                    // as two different concrete types (Ptr is compatible with anything since
                    // it's the default for unresolved types).
                    if let Some(existing) = &val_types[dst.0 as usize] {
                        debug_assert!(
                            *existing == nty
                                || *existing == LirType::Ptr || nty == LirType::Ptr
                                || existing.is_aggregate() != nty.is_aggregate(),
                            "SSA type conflict for __v{}: previously {:?}, now {:?} (in {})",
                            dst.0, existing, nty, func.name,
                        );
                    }
                    val_types[dst.0 as usize] = Some(nty);
                }
            }
            // Detect runtime struct returns that aren't in module.structs
            if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                if let Some(rt_name) = runtime_fn_return_struct(name) {
                    let in_module = module.structs.iter().any(|s| s.name == rt_name);
                    if !in_module {
                        val_c_type_override[d.0 as usize] = Some(rt_name.to_string());
                    }
                }
            }
            if let Inst::StrLit { dst, .. } = inst {
                str_lit_vals[dst.0 as usize] = true;
            }
            if let Inst::FuncAddr { dst, func } = inst {
                func_addr_targets[dst.0 as usize] = Some(*func);
            }
            // Mark CallExtern results that return const char* (not struct pointers).
            // Also override their value type to Ptr so they're declared as void* in C.
            if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                let ret_abi = module.externs.iter().find(|e| &e.name == name)
                    .map(|e| e.return_abi).unwrap_or_default();
                if ret_abi == crate::ir::abi::AbiKind::CStr {
                    // Extern "C" return — may be static or heap, use safe copy.
                    cstr_vals[d.0 as usize] = true;
                    extern_cstr_return_vals[d.0 as usize] = true;
                    val_types[d.0 as usize] = Some(LirType::Ptr);
                }
            }
            if let Inst::NullPtr { dst } = inst {
                null_vals[dst.0 as usize] = true;
            }
            // Track collection get results — pointers into internal storage that need cloning on Load.
            if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                if name == "gorget_map_get" || name == "gorget_array_get" {
                    _collection_get_vals[d.0 as usize] = true;
                }
            }
            // Track Option unwrap results that produce Str pointers.
            if let Inst::CallExtern { dst: Some(d), name, args, .. } = inst {
                if is_option_result_unwrap(name) && !args.is_empty() {
                    let str_sid = module.structs.iter().position(|s| s.name == "GorgetString");
                    if let Some(sid) = str_sid {
                        let str_ty = LirType::Struct(crate::lir::StructId(sid as u32));
                        let arg0 = args[0].0 as usize;
                        // Check if the arg's pointee struct is an Option containing Str
                        let is_str_option = ptr_pointee.get(arg0).and_then(|t| t.as_ref()).map_or(false, |pt| {
                            if let LirType::Struct(opt_sid) = pt {
                                module.structs.get(opt_sid.0 as usize).map_or(false, |sd| {
                                    (sd.name.contains("GorgetString") || sd.name.contains("__Str"))
                                    && sd.fields.get(1).map_or(false, |(_, ft)| ft.is_ptr())
                                })
                            } else { false }
                        });
                        if is_str_option {
                            ptr_pointee[d.0 as usize] = Some(str_ty);
                        }
                    }
                }
            }
            // Track spawn source function for void* destinations.
            // When __gorget_spawn_X returns Task__T but dst is void*, the codegen
            // extracts .__task; we record X so task_group_submit can reconstruct
            // the full Task struct with the correct __drop fn.
            if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                if name.starts_with("__gorget_spawn_") {
                    let is_task_struct = matches!(val_types.get(d.0 as usize).and_then(|t| t.as_ref()), Some(LirType::Struct(sid)) if {
                        module.structs.get(sid.0 as usize).map_or(false, |s| s.name.starts_with("Task__"))
                    });
                    if !is_task_struct {
                        let fn_suffix = name.strip_prefix("__gorget_spawn_").unwrap_or("").to_string();
                        spawn_source_fn[d.0 as usize] = Some(fn_suffix);
                    }
                }
            }
            // Track pointee types for pointer-producing instructions.
            match inst {
                Inst::SlotAddr { dst, slot } => {
                    let slot_ty = &func.slots[slot.0 as usize].ty;
                    ptr_pointee[dst.0 as usize] = Some(slot_ty.clone());
                }
                Inst::FieldPtr { dst, struct_id, field, .. } => {
                    let sdef = &module.structs[struct_id.0 as usize];
                    if (*field as usize) < sdef.fields.len() {
                        ptr_pointee[dst.0 as usize] = Some(sdef.fields[*field as usize].1.clone());
                    }
                }
                Inst::ElemPtr { dst, .. } => {
                    // Element pointer — pointee type unknown without array element type info.
                    // Leave as None; Store will fall back to sizeof(*(val)).
                    let _ = dst;
                }
                Inst::GlobalAddr { dst, .. } => {
                    // Could track global type, but globals are rarely stored into via Store.
                    let _ = dst;
                }
                // Propagate pointee types through SlotStore→SlotLoad chains.
                // When a Ptr-typed slot stores a value with known pointee, propagate
                // to subsequent loads from that slot.
                Inst::SlotStore { slot, value, .. } => {
                    if let Some(pt) = ptr_pointee.get(value.0 as usize).and_then(|p| p.clone()) {
                        if matches!(func.slots[slot.0 as usize].ty, LirType::Ptr | LirType::PtrTo(_)) {
                            slot_pointee[slot.0 as usize] = Some(pt);
                        }
                    }
                }
                Inst::SlotLoad { dst, slot, .. } => {
                    if let Some(pt) = slot_pointee.get(slot.0 as usize).and_then(|p| p.clone()) {
                        ptr_pointee[dst.0 as usize] = Some(pt);
                    }
                }
                _ => {}
            }
        }
    }

    // Propagate str_ptr_values from LIR function into ptr_pointee.
    // The LIR lowering marks Ptr params that point to GorgetString.
    // Propagate this info through SlotStore→SlotLoad chains so the C backend
    // can deref Ptr(Str) args in printf, CmpOp, and CallExtern.
    {
        let str_struct_id = module.structs.iter().position(|s| s.name == "GorgetString");
        if let Some(sid) = str_struct_id {
            let str_ty = LirType::Struct(crate::lir::StructId(sid as u32));
            // Seed ptr_pointee from LIR's str_ptr_values
            for vid in &func.str_ptr_values {
                if (vid.0 as usize) < ptr_pointee.len() {
                    ptr_pointee[vid.0 as usize] = Some(str_ty.clone());
                }
            }
            // Track which slots hold Str ptrs
            let mut str_ptr_slots: rustc_hash::FxHashSet<u32> = rustc_hash::FxHashSet::default();
            // Seed str_ptr_slots from PtrTo(Str) slot types — these slots inherently hold Str pointers.
            for (idx, slot) in func.slots.iter().enumerate() {
                if is_str_ptr(&slot.ty, module) {
                    str_ptr_slots.insert(idx as u32);
                }
            }
            // Propagate through instruction chains (multiple passes for convergence)
            // DON'T mark Str-typed slots as str_ptr_slots — they hold Str VALUES, not pointers.
            // Only Ptr-typed slots that receive Str pointer values get marked (via SlotStore propagation).
            // Fixpoint: propagate through all instruction patterns
            for _ in 0..4 {
                for block in &func.blocks {
                    for inst in &block.insts {
                        // SlotStore of Str ptr → mark slot
                        if let Inst::SlotStore { slot, value, .. } = inst {
                            let is_str = ptr_pointee.get(value.0 as usize)
                                .and_then(|p| p.as_ref())
                                .map_or(false, |p| *p == str_ty);
                            if is_str {
                                str_ptr_slots.insert(slot.0);
                            }
                        }
                        // SlotLoad from Str ptr slot → mark loaded value
                        if let Inst::SlotLoad { dst, slot, .. } = inst {
                            if str_ptr_slots.contains(&slot.0)
                                && matches!(val_types.get(dst.0 as usize), Some(Some(LirType::Ptr)))
                            {
                                ptr_pointee[dst.0 as usize] = Some(str_ty.clone());
                            }
                        }
                        // Load through a Str ptr → result is also Str ptr (if Ptr-typed)
                        if let Inst::Load { dst, ptr, ty } = inst {
                            if ty.is_ptr() {
                                let is_str = ptr_pointee.get(ptr.0 as usize)
                                    .and_then(|p| p.as_ref())
                                    .map_or(false, |p| *p == str_ty);
                                if is_str {
                                    ptr_pointee[dst.0 as usize] = Some(str_ty.clone());
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Fix val_types for values with no inferred type (e.g. InlineC dst values).
    // For InlineC→SlotStore, use the slot's type.
    // For values passed as block parameter arguments, use the block param's type.
    for block in &func.blocks {
        let insts = &block.insts;
        for (i, inst) in insts.iter().enumerate() {
            if let Inst::InlineC { dst: Some(d), .. } = inst {
                if val_types.get(d.0 as usize) == Some(&None) {
                    if let Some(Inst::SlotStore { slot, value, .. }) = insts.get(i + 1) {
                        if *value == *d {
                            let slot_ty = func.slots[slot.0 as usize].ty.clone();
                            if !matches!(slot_ty, LirType::Ptr | LirType::PtrTo(_) | LirType::Void) {
                                val_types[d.0 as usize] = Some(norm(slot_ty));
                            }
                        }
                    }
                }
            }
        }
        // Infer or correct value types from block parameter types at jump/branch targets.
        // Block param types come from SSA slot types (deterministic). If the forward pass
        // inferred a different type (e.g., due to value numbering shifts from drop changes),
        // the block param type is authoritative.
        let infer_from_args = |target: BlockId, args: &[ValueId], val_types: &mut Vec<Option<LirType>>| {
            let target_params = &func.blocks[target.0 as usize].params;
            for (arg, (_, param_ty)) in args.iter().zip(target_params.iter()) {
                let normed = if matches!(param_ty, LirType::PtrTo(_)) { LirType::Ptr } else { param_ty.clone() };
                let current = val_types.get(arg.0 as usize).and_then(|t| t.as_ref());
                if current.is_none() || current != Some(&normed) {
                    val_types[arg.0 as usize] = Some(normed);
                }
            }
        };
        match &block.terminator {
            Term::Jump(target, args) => {
                infer_from_args(*target, args, &mut val_types);
            }
            Term::Branch { then_block, then_args, else_block, else_args, .. } => {
                infer_from_args(*then_block, then_args, &mut val_types);
                infer_from_args(*else_block, else_args, &mut val_types);
            }
            _ => {}
        }
    }

    // Fix val_types for CallExtern→SlotStore type mismatches. The extern
    // declaration's return type may be a scalar/Ptr (e.g. void*, int64_t),
    // but the GIR intended a richer type (e.g. Option[int], GorgetArray).
    // When the slot type disagrees with the inferred value type and the slot
    // type is not Ptr/Void, prefer the slot type — it comes from the GIR and
    // is more precise than the C runtime's generic signature.
    //
    // Exception: when the current type is Ptr and the slot is Str/GorgetString,
    // keep Ptr — the SlotStore handler wraps the pointer with gorget_str_from_literal.
    // Also skip cstr-returning functions which produce raw pointers handled at store time.
    for block in &func.blocks {
        let insts = &block.insts;
        for (i, inst) in insts.iter().enumerate() {
            if let Inst::CallExtern { dst: Some(d), .. } = inst {
                if let Some(Inst::SlotStore { slot, value, .. }) = insts.get(i + 1) {
                    if *value == *d {
                        let slot_ty = norm(func.slots[slot.0 as usize].ty.clone());
                        if !matches!(slot_ty, LirType::Ptr | LirType::Void) {
                            let current = val_types[d.0 as usize].as_ref();
                            let is_ptr_to_str = matches!(current, Some(LirType::Ptr)) && {
                                let raw_slot_ty = &func.slots[slot.0 as usize].ty;
                                is_str_struct(raw_slot_ty, module) || is_str_ptr(raw_slot_ty, module)
                            };
                            if !is_ptr_to_str && current != Some(&slot_ty) {
                                val_types[d.0 as usize] = Some(slot_ty);
                            }
                        }
                    }
                }
            }
        }
    }

    // Fix val_types for guard/shared accessor results.
    // gorget_guard_get / gorget_shared_get_ptr return void* but the actual
    // inner type can be inferred from consumers (printf format, arithmetic, etc.).
    // Default to I64 for these accessors when the consumer doesn't reveal the type.
    for block in &func.blocks {
        let insts = &block.insts;
        for (i, inst) in insts.iter().enumerate() {
            if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                let is_guard_value_accessor = matches!(name.as_str(),
                    "gorget_guard_get"
                    | "gorget_shared_get"
                    | "gorget_read_guard_get"
                    | "gorget_write_guard_get"
                );
                let is_guard_ptr_accessor = matches!(name.as_str(),
                    "gorget_guard_get_ptr"
                    | "gorget_shared_get_ptr"
                    | "gorget_read_guard_get_ptr"
                    | "gorget_write_guard_get_ptr"
                );
                // *_get_ptr functions return a raw pointer — keep as Ptr, never override to I64.
                if is_guard_ptr_accessor && matches!(val_types.get(d.0 as usize), Some(Some(LirType::Ptr)) | Some(None)) {
                    val_types[d.0 as usize] = Some(LirType::Ptr);
                }
                if is_guard_value_accessor && matches!(val_types.get(d.0 as usize), Some(Some(LirType::Ptr)) | Some(None)) {
                    // Look at the next few instructions for a consumer that reveals the type.
                    let mut inferred = None;
                    for ci in (i+1)..insts.len().min(i+10) {
                        match &insts[ci] {
                            Inst::Add { ty, lhs, .. } | Inst::Sub { ty, lhs, .. }
                            | Inst::Mul { ty, lhs, .. } | Inst::Div { ty, lhs, .. }
                            | Inst::Rem { ty, lhs, .. } if *lhs == *d => {
                                inferred = Some(ty.clone());
                                break;
                            }
                            Inst::IntCast { value, .. } if *value == *d => {
                                inferred = Some(LirType::I64);
                                break;
                            }
                            Inst::FloatCast { value, .. } if *value == *d => {
                                inferred = Some(LirType::F64);
                                break;
                            }
                            // Check SlotStore — the slot type reveals the inner type.
                            Inst::SlotStore { value, slot, .. } if *value == *d => {
                                let slot_ty = &func.slots[slot.0 as usize].ty;
                                if !slot_ty.is_ptr() {
                                    inferred = Some(slot_ty.clone());
                                    break;
                                }
                            }
                            // Check printf: if the value is an arg to printf with a float format
                            Inst::CallExtern { name: call_name, args, .. }
                                if call_name == "printf" && args.len() >= 2
                                    && args[1..].contains(d) => {
                                // Check if the format string contains %f — indicates float value.
                                if let Some(fmt_val) = args.first() {
                                    // Walk backwards to find the StrLit for the format string.
                                    for si in (0..ci).rev().take(10) {
                                        if let Inst::StrLit { dst: sd, value: fmt } = &insts[si] {
                                            if *sd == *fmt_val {
                                                if fmt.contains("%f") || fmt.contains("%.") {
                                                    inferred = Some(LirType::F64);
                                                }
                                                break; // found the StrLit for this format arg
                                            }
                                        }
                                    }
                                    if inferred.is_some() { break; }
                                }
                            }
                            _ => {}
                        }
                    }
                    val_types[d.0 as usize] = Some(inferred.unwrap_or(LirType::I64));
                }
            }
        }
    }

    // Fix cross-type map combinator types. When Option__T__map is called with
    // a closure that returns U≠T, the result should be Option__U. The GIR doesn't
    // track this, so the slot and val_types have the wrong type. Fix both: val_types
    // for the value declaration and slot_overrides for the slot declaration.
    let mut slot_overrides: HashMap<u32, LirType> = HashMap::new();
    for block in &func.blocks {
        let insts = &block.insts;
        for (idx, inst) in insts.iter().enumerate() {
            if let Inst::CallExtern { dst: Some(d), name, args: call_args, .. } = inst {
                if let Some((_type_prefix, "map")) = parse_option_result_combinator(name) {
                    if call_args.len() > 1 {
                        let closure_struct = ptr_pointee.get(call_args[1].0 as usize)
                            .and_then(|t| t.as_ref())
                            .map(|t| c_type_named(t, sn));
                        let call_fn = closure_struct
                            .map(|n| find_closure_call_fn(module, &n, sn))
                            .unwrap_or_default();
                        if !call_fn.is_empty() {
                            if let Some(ret_ty_name) = closure_call_return_type(module, &call_fn, sn) {
                                let ret_mono = type_name_to_monomorphized(&ret_ty_name);
                                let target_name = if name.starts_with("Option__") {
                                    format!("Option__{ret_mono}")
                                } else {
                                    // Result__OkType__ErrType__map → extract error type from source
                                    let type_prefix = _type_prefix;
                                    let err_suffix = type_prefix.strip_prefix("Result__")
                                        .and_then(|rest| rest.find("__").map(|pos| &rest[pos..]));
                                    if let Some(err) = err_suffix {
                                        format!("Result__{ret_mono}{err}")
                                    } else {
                                        format!("Result__{ret_mono}")
                                    }
                                };
                                if let Some(target_sid) = module.structs.iter().position(|s| s.name == target_name) {
                                    let target_ty = LirType::Struct(StructId(target_sid as u32));
                                    val_types[d.0 as usize] = Some(target_ty.clone());
                                    // Also fix the slot that receives this value.
                                    if let Some(Inst::SlotStore { slot, value, .. }) = insts.get(idx + 1) {
                                        if *value == *d {
                                            slot_overrides.insert(slot.0, target_ty);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Infer types for untyped values from their consumers (arithmetic, comparison, etc.)
    // This handles polymorphic extern results used in typed operations without an intervening slot store.
    for block in &func.blocks {
        for inst in &block.insts {
            let consumer_ty = match inst {
                Inst::Add { ty, lhs, rhs, .. } | Inst::Sub { ty, lhs, rhs, .. }
                | Inst::Mul { ty, lhs, rhs, .. } | Inst::Div { ty, lhs, rhs, .. }
                | Inst::Rem { ty, lhs, rhs, .. } | Inst::Mod { ty, lhs, rhs, .. } => {
                    Some((ty.clone(), vec![*lhs, *rhs]))
                }
                Inst::Neg { ty, operand, .. } => Some((ty.clone(), vec![*operand])),
                Inst::BitAnd { ty, lhs, rhs, .. } | Inst::BitOr { ty, lhs, rhs, .. }
                | Inst::BitXor { ty, lhs, rhs, .. }
                | Inst::Shl { ty, lhs, rhs, .. } | Inst::Shr { ty, lhs, rhs, .. } => {
                    Some((ty.clone(), vec![*lhs, *rhs]))
                }
                Inst::BitNot { ty, operand, .. } => Some((ty.clone(), vec![*operand])),
                // SlotStore: infer from the slot's declared type.
                Inst::SlotStore { slot, value, .. } => {
                    let sty = slot_overrides.get(&slot.0).unwrap_or(&func.slots[slot.0 as usize].ty).clone();
                    if !matches!(sty, LirType::Ptr | LirType::PtrTo(_) | LirType::Void) {
                        Some((sty, vec![*value]))
                    } else { None }
                }
                Inst::IntCast { value, .. } | Inst::FloatCast { value, .. }
                | Inst::IntToFloat { value, .. } | Inst::FloatToInt { value, .. } => {
                    // The source type can be inferred from the cast target for the *source* operand.
                    // But we don't know the source type from the cast alone — skip.
                    let _ = value;
                    None
                }
                _ => None,
            };
            if let Some((ty, operands)) = consumer_ty {
                let ty = norm(ty);
                for op in operands {
                    if val_types.get(op.0 as usize).and_then(|t| t.as_ref()).is_none() {
                        val_types[op.0 as usize] = Some(ty.clone());
                    }
                }
            }
            // Cmp: propagate peer type between operands.
            if let Inst::Cmp { lhs, rhs, .. } = inst {
                let lty = val_types.get(lhs.0 as usize).and_then(|t| t.as_ref()).cloned();
                let rty = val_types.get(rhs.0 as usize).and_then(|t| t.as_ref()).cloned();
                if lty.is_some() && rty.is_none() {
                    if let Some(lt) = &lty {
                        if !matches!(lt, LirType::Ptr | LirType::Void) {
                            val_types[rhs.0 as usize] = lty;
                        }
                    }
                } else if rty.is_some() && lty.is_none() {
                    if let Some(rt) = &rty {
                        if !matches!(rt, LirType::Ptr | LirType::Void) {
                            val_types[lhs.0 as usize] = rty;
                        }
                    }
                }
            }
        }
        // Also infer from block terminators: Ret(value) implies function return type.
        if let Term::Ret(val) = &block.terminator {
            if !matches!(func.return_type, LirType::Void | LirType::Ptr | LirType::PtrTo(_)) {
                if val_types.get(val.0 as usize).and_then(|t| t.as_ref()).is_none() {
                    val_types[val.0 as usize] = Some(norm(func.return_type.clone()));
                }
            }
        }
    }

    // Override slot types for runtime structs that are larger than a pointer.
    // gorget_mutex_lock_to / gorget_rwlock_{read,write}_lock_to write a struct
    // (gorget_guard_t / gorget_rw_guard_t) into a slot via output pointer.
    // The LIR types them as Ptr, but the C type must be the actual struct.
    let mut slot_c_overrides: HashMap<u32, &str> = HashMap::new();
    for block in &func.blocks {
        for inst in &block.insts {
            if let Inst::CallExtern { name, args, .. } = inst {
                let guard_c_type = if name == "gorget_mutex_lock_to" { Some("gorget_guard_t") }
                    else if name == "gorget_rwlock_read_lock_to" || name == "gorget_rwlock_write_lock_to" { Some("gorget_rw_guard_t") }
                    else if name == "gorget_channel_recv_to" { Some("gorget_guard_t") }
                    else { None };
                if let Some(c_type) = guard_c_type {
                    // The output pointer arg is the last arg.
                    if let Some(out_arg) = args.last() {
                        // Trace back to SlotAddr to find the slot.
                        if let Some(Inst::SlotAddr { slot, .. }) = func.blocks.iter()
                            .flat_map(|b| b.insts.iter())
                            .find(|i| matches!(i, Inst::SlotAddr { dst, .. } if *dst == *out_arg))
                        {
                            slot_c_overrides.insert(slot.0, c_type);
                        }
                    }
                }
            }
        }
    }

    // Slot declarations (emitted after cross-type fix-ups so slot_overrides are applied).
    for (i, slot) in func.slots.iter().enumerate() {
        let effective_ty = slot_overrides.get(&(i as u32)).unwrap_or(&slot.ty);
        let ty_str = if let Some(c_override) = slot_c_overrides.get(&(i as u32)) {
            c_override.to_string()
        } else {
            let ts = c_type_named(effective_ty, sn);
            if ts == "void" { "void*".to_string() } else { ts }
        };

        write!(out, "    {ty_str} __s{i}").unwrap();
        // Zero-initialize
        if slot_c_overrides.contains_key(&(i as u32)) {
            // C type override is always a struct — use aggregate init.
            write!(out, " = {{0}}").unwrap();
        } else if effective_ty.is_scalar() {
            write!(out, " = 0").unwrap();
        } else {
            write!(out, " = {{0}}").unwrap();
        }
        writeln!(out, ";").unwrap();
    }

    for (i, ty) in val_types.iter().enumerate() {
        // Use C type override if available (for runtime structs not in module.structs).
        if let Some(Some(c_override)) = val_c_type_override.get(i) {
            writeln!(out, "    {} __v{i};", c_override).unwrap();
            continue;
        }
        // cstr_vals are const char* from runtime functions — declare as such to avoid const-discard warnings.
        if cstr_vals.get(i).copied().unwrap_or(false) {
            writeln!(out, "    const char* __v{i};").unwrap();
            continue;
        }
        match ty {
            Some(ty) => {
                let ts = c_type_named(ty, sn);
                if ts == "void" {
                    // Void-typed values are used as opaque pointers — declare as void*.
                    writeln!(out, "    void* __v{i};").unwrap();
                } else {
                    writeln!(out, "    {} __v{i};", ts).unwrap();
                }
            }
            None => {
                // No type inferred — value is referenced but type couldn't be determined.
                // Declare as void* to avoid undeclared variable errors.
                writeln!(out, "    void* __v{i};").unwrap();
            }
        }
    }

    // Block parameter move variables (for parallel copy semantics).
    // Each block param needs a temporary for parallel moves.
    for block in &func.blocks {
        for (vid, ty) in &block.params {
            writeln!(out, "    {} __bp{};", c_type_named(ty, sn), vid.0).unwrap();
        }
    }

    writeln!(out).unwrap();

    // For the main function, emit runtime call initializers for globals.
    if func.name == "main" {
        // Build original→LIR struct name map for rewriting compound literals in RuntimeCall exprs.
        let orig_to_lir: Vec<(String, String)> = module.structs.iter().enumerate()
            .filter_map(|(i, def)| {
                let lir_name = sn.get(&(i as u32)).cloned().unwrap_or_else(|| format!("__lir_s{i}"));
                if lir_name != def.name {
                    Some((def.name.clone(), lir_name))
                } else {
                    None
                }
            })
            .collect();
        for (gid, g) in module.globals.iter().enumerate() {
            if let LirGlobalInit::RuntimeCall(expr) = &g.init {
                let mut rewritten = expr.clone();
                for (orig, lir_name) in &orig_to_lir {
                    rewritten = rewritten.replace(
                        &format!("({orig}){{"),
                        &format!("({lir_name}){{"),
                    );
                }
                writeln!(out, "    __lir_g{gid} = {rewritten};").unwrap();
            }
        }
    }

    // Track which slots have been registered on the cleanup stack (test functions only).
    let mut test_cleanup_pushed = std::collections::HashSet::<u32>::new();

    let tracing = module.trace_filename.is_some();
    let is_main = func.name == "main";

    // Pre-scan: collect which blocks are the "then" target of Branch terminators.
    let trace_then_blocks: std::collections::HashSet<u32> = if tracing {
        func.blocks.iter().filter_map(|b| {
            if let Term::Branch { then_block, .. } = &b.terminator {
                Some(then_block.0)
            } else {
                None
            }
        }).collect()
    } else {
        std::collections::HashSet::new()
    };

    // Build consolidated emission context for instruction dispatch.
    let ectx = EmitContext {
        func, module, sn,
        val_types: &val_types,
        str_lit_vals: &str_lit_vals,
        cstr_vals: &cstr_vals,
        extern_cstr_return_vals: &extern_cstr_return_vals,
        null_vals: &null_vals,
        ptr_pointee: &ptr_pointee,
        func_addr_targets: &func_addr_targets,
        spawn_source_fn: &spawn_source_fn,
        string_lit_map,
    };

    // Blocks
    for block in &func.blocks {
        writeln!(out, "__bb{}:", block.id.0).unwrap();

        // Branch event: emitted when a "then" block is actually entered.
        if tracing && trace_then_blocks.contains(&block.id.0) {
            let _ = writeln!(out, "    if (__gorget_trace_fp) {{ fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"branch\\\",\\\"depth\\\":%d}}\\n\", __gorget_trace_depth); }}");
        }

        // Stmt_start event: emitted at the start of each block.
        if tracing {
            let _ = writeln!(out, "    if (__gorget_trace_fp) {{ fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"stmt_start\\\",\\\"depth\\\":%d}}\\n\", __gorget_trace_depth++); }}");
        }

        // Move block params from temporaries.
        for (vid, _) in &block.params {
            writeln!(out, "    __v{} = __bp{};", vid.0, vid.0).unwrap();
        }

        // Instructions
        for inst in &block.insts {
            write!(out, "    ").unwrap();
            emit_inst(out, inst, &ectx);
            writeln!(out).unwrap();

            // In test functions, register droppable user-named slots on the cleanup stack
            // so they're cleaned up if gorget_panic() calls longjmp (test assertion fails).
            if func.is_test_fn {
                if let Inst::SlotStore { slot, .. } = inst {
                    let slot_idx = slot.0;
                    if !test_cleanup_pushed.contains(&slot_idx) {
                        if func.slots[slot_idx as usize].name.is_some() {
                            if let Some(push_code) = test_cleanup_push_code_lir(slot_idx, func, module, sn) {
                                out.push_str(&push_code);
                                test_cleanup_pushed.insert(slot_idx);
                            }
                        }
                    }
                }
            }
        }

        // Stmt_end event: emitted after instructions, before the terminator.
        if tracing {
            let _ = writeln!(out, "    if (__gorget_trace_fp) {{ fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"stmt_end\\\",\\\"depth\\\":%d}}\\n\", --__gorget_trace_depth); }}");
        }

        // Trace return event: inject before each return statement for non-main functions.
        if tracing && !is_main {
            if matches!(&block.terminator, Term::Ret(_) | Term::RetVoid) {
                if let Some(ref display_name) = func.display_name {
                    let escaped = display_name.replace('\\', "\\\\").replace('"', "\\\"");
                    let _ = writeln!(out, "    if (__gorget_trace_fp) {{ fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"return\\\",\\\"fn\\\":\\\"{escaped}\\\",\\\"depth\\\":%d}}\\n\", __gorget_trace_depth--); }}");
                }
            }
        }

        // Terminator
        write!(out, "    ").unwrap();
        emit_term(out, &block.terminator, func, module, sn, &val_types);
        writeln!(out).unwrap();
    }

    writeln!(out, "}}").unwrap();
}

fn emit_inst(out: &mut String, inst: &Inst, ctx: &EmitContext) {
    let func = ctx.func;
    let module = ctx.module;
    let sn = ctx.sn;
    let val_types = ctx.val_types;
    let str_lit_vals = ctx.str_lit_vals;
    let cstr_vals = ctx.cstr_vals;
    let extern_cstr_return_vals = ctx.extern_cstr_return_vals;
    let null_vals = ctx.null_vals;
    let ptr_pointee = ctx.ptr_pointee;
    let func_addr_targets = ctx.func_addr_targets;
    let _spawn_source_fn = ctx.spawn_source_fn;
    let v = |id: ValueId| -> String { format!("__v{}", id.0) };
    let s = |id: SlotId| -> String { format!("__s{}", id.0) };

    match inst {
        // Slot access
        Inst::SlotStore { slot, value, is_move } => {
            // Skip store to slot 0 (return slot) in void-returning functions.
            // LIR declares slot 0 as void* but unit values are int32_t — skip to avoid type mismatch.
            if slot.0 == 0 && matches!(func.return_type, LirType::Void) {
                return;
            }
            let slot_ty = &func.slots[slot.0 as usize].ty;
            let val_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
            let slot_is_str = matches!(slot_ty, LirType::Struct(sid) if sn.get(&sid.0).map_or(false, |n| n == "Str"));
            let slot_is_gs = matches!(slot_ty, LirType::Struct(sid) if sn.get(&sid.0).map_or(false, |n| n == "GorgetString"));
            let slot_is_closure = matches!(slot_ty, LirType::Struct(sid) if sn.get(&sid.0).map_or(false, |n| n == "GorgetClosure"));
            let is_str_lit_val = str_lit_vals.get(value.0 as usize).copied().unwrap_or(false);
            let is_cstr = cstr_vals.get(value.0 as usize).copied().unwrap_or(false);
            // __Closure_N struct → GorgetClosure slot: heap-alloc env + pack fn_ptr/env.
            let val_closure_name = if slot_is_closure {
                // Check direct struct type or pointee type for __Closure_N
                let check_sid = |sid: &StructId| -> Option<String> {
                    let name = sn.get(&sid.0).cloned().unwrap_or_default();
                    if name.starts_with("__Closure_") {
                        return Some(name);
                    }
                    // __lir_sN aliases — check the actual module struct name
                    module.structs.get(sid.0 as usize)
                        .filter(|sd| sd.name.starts_with("__Closure_"))
                        .map(|sd| sd.name.clone())
                };
                if let Some(LirType::Struct(val_sid)) = val_ty {
                    check_sid(val_sid)
                } else if matches!(val_ty, Some(LirType::Ptr)) {
                    // Pointer to a __Closure_N struct (e.g., from SlotAddr)
                    if let Some(Some(LirType::Struct(pt_sid))) = ptr_pointee.get(value.0 as usize) {
                        check_sid(pt_sid)
                    } else { None }
                } else { None }
            } else { None };
            if let Some(closure_struct_name) = val_closure_name {
                let call_fn = format!("{closure_struct_name}__call");
                let val_is_ptr = matches!(val_ty, Some(LirType::Ptr));
                // Use the LIR C name (e.g. __lir_s9) for the struct, not the GIR name (__Closure_0).
                let c_struct_name = if val_is_ptr {
                    if let Some(Some(LirType::Struct(pt_sid))) = ptr_pointee.get(value.0 as usize) {
                        c_type_named(&LirType::Struct(*pt_sid), sn)
                    } else { closure_struct_name.clone() }
                } else if let Some(LirType::Struct(sid)) = val_ty {
                    c_type_named(&LirType::Struct(*sid), sn)
                } else { closure_struct_name.clone() };
                if val_is_ptr {
                    write!(out, "{{ {c_struct_name}* __heap = ({c_struct_name}*)GORGET_ALLOC(sizeof({c_struct_name})); memcpy(__heap, {v}, sizeof({c_struct_name})); {s} = (GorgetClosure){{.fn_ptr = (void*){call_fn}, .env = (void*)__heap}}; }}",
                        v = v(*value), s = s(*slot)).unwrap();
                } else {
                    write!(out, "{{ {c_struct_name}* __heap = ({c_struct_name}*)GORGET_ALLOC(sizeof({c_struct_name})); *__heap = {v}; {s} = (GorgetClosure){{.fn_ptr = (void*){call_fn}, .env = (void*)__heap}}; }}",
                        v = v(*value), s = s(*slot)).unwrap();
                }
                return;
            }
            // FuncAddr → GorgetClosure slot: wrap named function with adapter.
            if slot_is_closure {
                if let Some(fid) = func_addr_targets.get(value.0 as usize).and_then(|t| *t) {
                    let adapt_name = format!("__adapt_{}", c_func_name(&module.functions[fid.0 as usize].name));
                    write!(out, "{s} = (GorgetClosure){{.fn_ptr = (void*){adapt}, .env = NULL}};",
                        s = s(*slot), adapt = adapt_name).unwrap();
                    return;
                }
            }
            // GorgetClosure slot with non-closure value (e.g. void*, int64_t, or void from another slot):
            // memcpy to avoid type mismatch. The value is always a pointer to closure data
            // (from SlotAddr, array_get, etc.), even when LIR types it as I64.
            if slot_is_closure && !matches!(val_ty, Some(LirType::Struct(_))) {
                write!(out, "memcpy(&{}, (void*){}, sizeof(GorgetClosure));", s(*slot), v(*value)).unwrap();
                return;
            }
            // Implicit Result::Ok / Option::Some wrapping: scalar or non-wrapper struct → Result/Option slot.
            if let LirType::Struct(slot_sid) = slot_ty {
                let slot_struct_name = module.structs.get(slot_sid.0 as usize).map(|sd| sd.name.as_str()).unwrap_or("");
                let is_result_slot = slot_struct_name.starts_with("Result__");
                let is_option_slot = slot_struct_name.starts_with("Option__");
                if is_result_slot || is_option_slot {
                    let val_is_same_wrapper = match val_ty {
                        Some(LirType::Struct(val_sid)) => {
                            let vn = module.structs.get(val_sid.0 as usize).map(|sd| sd.name.as_str()).unwrap_or("");
                            (is_result_slot && vn.starts_with("Result__")) || (is_option_slot && vn.starts_with("Option__"))
                        }
                        _ => false,
                    };
                    // Only wrap when val_ty is a primitive numeric/bool type (not Ptr, not struct, not void/unknown).
                    let val_is_primitive = matches!(val_ty, Some(
                        LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64
                        | LirType::U8 | LirType::U16 | LirType::U32 | LirType::U64
                        | LirType::F32 | LirType::F64 | LirType::Bool
                    ));
                    if !val_is_same_wrapper && val_is_primitive {
                        let ty_name = c_type_named(slot_ty, sn);
                        let payload_field = if is_result_slot {
                            // Find Ok field name from struct def
                            module.structs.get(slot_sid.0 as usize)
                                .and_then(|sd| sd.fields.iter().find(|(n, _)| n.starts_with("Ok")))
                                .map(|(n, _)| c_field_name(n))
                                .unwrap_or_else(|| "Ok_0".to_string())
                        } else {
                            module.structs.get(slot_sid.0 as usize)
                                .and_then(|sd| sd.fields.iter().find(|(n, _)| n.starts_with("Some")))
                                .map(|(n, _)| c_field_name(n))
                                .unwrap_or_else(|| "Some_0".to_string())
                        };
                        write!(out, "memset(&{s}, 0, sizeof({ty})); {s}.tag = 0; {s}.{f} = {val};",
                            s = s(*slot), ty = ty_name, f = payload_field, val = v(*value)).unwrap();
                        return;
                    }
                }
            }
            if slot_is_str && is_str_lit_val {
                // String literal → Str slot: direct assign (Phase 3 — static .rodata, zero alloc).
                write!(out, "{} = {};", s(*slot), v(*value)).unwrap();
            } else if slot_is_str && is_cstr {
                let is_extern_ret = extern_cstr_return_vals.get(value.0 as usize).copied().unwrap_or(false);
                if is_extern_ret {
                    // Extern "C" return — may be static or heap, use safe copy.
                    write!(out, "{} = gorget_str_from_cstr({});", s(*slot), v(*value)).unwrap();
                } else {
                    // Runtime function — returns heap-allocated string, adopt (no leak).
                    write!(out, "{} = gorget_string_adopt((char*){});", s(*slot), v(*value)).unwrap();
                }
            } else if slot_is_gs && is_str_lit_val {
                // String literal → GorgetString slot: direct assign (Phase 3 — static .rodata).
                write!(out, "{} = {};", s(*slot), v(*value)).unwrap();
            } else if slot_is_gs && is_cstr {
                let is_extern_ret = extern_cstr_return_vals.get(value.0 as usize).copied().unwrap_or(false);
                if is_extern_ret {
                    write!(out, "{} = gorget_str_from_cstr({});", s(*slot), v(*value)).unwrap();
                } else {
                    write!(out, "{} = gorget_string_adopt((char*){});", s(*slot), v(*value)).unwrap();
                }
            } else if slot_ty.is_aggregate() {
                // Aggregate store: source may be a pointer (SlotAddr) or a struct value (ParamRef, Call result).
                let val_is_ptr = matches!(val_ty, Some(LirType::Ptr));
                let val_is_null = null_vals.get(value.0 as usize).copied().unwrap_or(false);
                let ty_name = c_type_named(slot_ty, sn);
                if val_is_null {
                    // NullPtr → aggregate slot: zero out (e.g. None variant of Option).
                    write!(out, "memset(&{}, 0, sizeof({}));", s(*slot), ty_name).unwrap();
                } else if val_is_ptr && (slot_is_str || slot_is_gs) {
                    if *is_move {
                        // Move: transfer ownership via memcpy. The GIR MoveZero
                        // instruction will zero the source, preventing double-free.
                        write!(out, "memcpy(&{}, {}, sizeof({}));", s(*slot), v(*value), ty_name).unwrap();
                    } else {
                        // Copy: CoW-aware — views (cap=0) get a 32-byte struct copy
                        // (zero alloc), owned strings get a deep clone.
                        write!(out, "{} = gorget_string_copy_cow((const GorgetString*){});", s(*slot), v(*value)).unwrap();
                    }
                } else if val_is_ptr {
                    // Value is a pointer to source data — use memcpy.
                    write!(out, "memcpy(&{}, {}, sizeof({}));", s(*slot), v(*value), ty_name).unwrap();
                } else {
                    // Value is a struct by value (e.g., from ParamRef or function return).
                    // Str and GorgetString are the same 32-byte struct (unified);
                    // cross-type assigns are direct.
                    let val_is_gs = matches!(val_ty, Some(LirType::Struct(sid)) if sn.get(&sid.0).map_or(false, |n| n == "GorgetString"));
                    let val_is_str = matches!(val_ty, Some(LirType::Struct(sid)) if sn.get(&sid.0).map_or(false, |n| n == "Str"));
                    if slot_is_str && val_is_str {
                        // Both are Str views: shallow struct copy, no clone needed (cap=0, non-owning).
                        write!(out, "{} = {};", s(*slot), v(*value)).unwrap();
                    } else if (slot_is_str && val_is_gs) || (slot_is_gs && val_is_str)
                        || ((slot_is_str || slot_is_gs) && matches!(val_ty, Some(LirType::Struct(_))))
                    {
                        // Str/GorgetString by-value → string slot: direct struct assign.
                        // The source is a C local/temporary (function return, ParamRef);
                        // transferring ownership via memcpy is correct — the source won't
                        // be double-freed (C locals have no destructors, and GIR MoveZero
                        // handles source zeroing when needed).
                        write!(out, "memcpy(&{}, &{}, sizeof({}));", s(*slot), v(*value), ty_name).unwrap();
                    } else if !matches!(val_ty, Some(LirType::Struct(_)) | None) {
                        // Scalar → single-field struct coercion (newtype wrapping).
                        if let LirType::Struct(sid) = slot_ty {
                            let sdef = &module.structs[sid.0 as usize];
                            if sdef.fields.len() == 1 {
                                write!(out, "{} = ({}){{ .{} = {} }};",
                                    s(*slot), ty_name, sdef.fields[0].0, v(*value)).unwrap();
                            } else {
                                write!(out, "{} = {};", s(*slot), v(*value)).unwrap();
                            }
                        } else {
                            write!(out, "{} = {};", s(*slot), v(*value)).unwrap();
                        }
                    } else {
                        write!(out, "{} = {};", s(*slot), v(*value)).unwrap();
                    }
                }
            } else {
                // Scalar slot — check for single-field struct → scalar unwrapping (newtype).
                if let Some(LirType::Struct(val_sid)) = val_ty {
                    let sdef = &module.structs[val_sid.0 as usize];
                    if sdef.fields.len() == 1 && !matches!(slot_ty, LirType::Struct(_)) {
                        write!(out, "{} = {}.{};", s(*slot), v(*value), sdef.fields[0].0).unwrap();
                    } else {
                        write!(out, "{} = {};", s(*slot), v(*value)).unwrap();
                    }
                } else {
                    write!(out, "{} = {};", s(*slot), v(*value)).unwrap();
                }
            }
        }
        Inst::SlotLoad { dst, slot, .. } => {
            write!(out, "{} = {};", v(*dst), s(*slot)).unwrap();
        }
        Inst::SlotAddr { dst, slot } => {
            write!(out, "{} = &{};", v(*dst), s(*slot)).unwrap();
        }

        // Constants
        Inst::IConst { dst, value, ty } => {
            write!(out, "{} = ({}){}LL;", v(*dst), c_type_named(ty, sn), value).unwrap();
        }
        Inst::FConst { dst, bits, ty } => {
            let val = f64::from_bits(*bits);
            write!(out, "{} = ({})({});", v(*dst), c_type_named(ty, sn), format_float(val)).unwrap();
        }
        Inst::BoolConst { dst, value } => {
            write!(out, "{} = {};", v(*dst), if *value { "true" } else { "false" }).unwrap();
        }
        Inst::NullPtr { dst } => {
            write!(out, "{} = NULL;", v(*dst)).unwrap();
        }
        Inst::FuncAddr { dst, func } => {
            let name = c_func_name(&module.functions[func.0 as usize].name);
            let adapt_name = format!("__adapt_{}", name);
            // Emit as a static 2-element closure array {adapter_fn, NULL} so that
            // callable dispatch ((void**)cv)[0] / ((void**)cv)[1] works correctly.
            // The adapter ignores the env pointer and forwards to the real function.
            write!(out, "{{ static void* __fa_{}[] = {{ (void*){}, NULL }}; {} = (void*)__fa_{}; }}",
                dst.0, adapt_name, v(*dst), dst.0).unwrap();
        }
        Inst::GlobalAddr { dst, global } => {
            write!(out, "{} = &__lir_g{};", v(*dst), global.0).unwrap();
        }
        Inst::StrLit { dst, value } => {
            // Use static .rodata literal if available (zero allocation — just a struct copy).
            if let Some(&idx) = ctx.string_lit_map.get(value) {
                write!(out, "{} = __slit_{};", v(*dst), idx).unwrap();
            } else {
                // Fallback: raw char* literal. Caller-side SlotStore wraps this into
                // a Str when writing into a Str slot (see slot_is_str && is_str_lit_val).
                let escaped = escape_c_string(value);
                write!(out, "{} = \"{}\";", v(*dst), escaped).unwrap();
            }
        }
        Inst::ParamRef { dst, index, .. } => {
            if func.const_params.get(*index as usize) == Some(&true) {
                write!(out, "{} = (void*)__p{};", v(*dst), index).unwrap();
            } else {
                write!(out, "{} = __p{};", v(*dst), index).unwrap();
            }
        }

        // Arithmetic
        Inst::Add { dst, ty, lhs, rhs, overflow } => {
            if *overflow == Overflow::Trap && matches!(ty, LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8) {
                let ct = c_type_named(ty, sn);
                write!(out, "if (__builtin_add_overflow(({ct}){l}, ({ct}){r}, &{d})) {{ fprintf(stderr, \"gorget: integer overflow\\n\"); exit(1); }}",
                    d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            } else {
                let ct = c_type_named(ty, sn);
                write!(out, "{d} = ({ct}){l} + ({ct}){r};", d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            }
        }
        Inst::Sub { dst, ty, lhs, rhs, overflow } => {
            if *overflow == Overflow::Trap && matches!(ty, LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8) {
                let ct = c_type_named(ty, sn);
                write!(out, "if (__builtin_sub_overflow(({ct}){l}, ({ct}){r}, &{d})) {{ fprintf(stderr, \"gorget: integer overflow\\n\"); exit(1); }}",
                    d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            } else {
                let ct = c_type_named(ty, sn);
                write!(out, "{d} = ({ct}){l} - ({ct}){r};", d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            }
        }
        Inst::Mul { dst, ty, lhs, rhs, overflow } => {
            if *overflow == Overflow::Trap && matches!(ty, LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8) {
                let ct = c_type_named(ty, sn);
                write!(out, "if (__builtin_mul_overflow(({ct}){l}, ({ct}){r}, &{d})) {{ fprintf(stderr, \"gorget: integer overflow\\n\"); exit(1); }}",
                    d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            } else {
                let ct = c_type_named(ty, sn);
                write!(out, "{d} = ({ct}){l} * ({ct}){r};", d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            }
        }
        Inst::Div { dst, ty, lhs, rhs } => {
            if matches!(ty, LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8) {
                let ct = c_type_named(ty, sn);
                write!(out, "if (({ct}){r} == 0) {{ fprintf(stderr, \"gorget: division by zero\\n\"); exit(1); }} {d} = ({ct}){l} / ({ct}){r};",
                    d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            } else {
                write!(out, "{} = {} / {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
            }
        }
        Inst::Rem { dst, ty, lhs, rhs, .. } => {
            if matches!(ty, LirType::F32 | LirType::F64) {
                write!(out, "{} = fmod({}, {});", v(*dst), v(*lhs), v(*rhs)).unwrap();
            } else {
                let ct = c_type_named(ty, sn);
                write!(out, "{d} = ({ct}){l} % ({ct}){r};", d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            }
        }
        Inst::Mod { dst, ty, lhs, rhs, .. } => {
            if matches!(ty, LirType::F32 | LirType::F64) {
                // Python-style float modulo: fmod(a,b) + (result has different sign from b ? b : 0)
                write!(
                    out,
                    "{{ double __t = fmod({l}, {r}); {d} = __t + (__t != 0.0 && ((__t < 0) != ({r} < 0)) ? {r} : 0.0); }}",
                    d = v(*dst), l = v(*lhs), r = v(*rhs)
                ).unwrap();
            } else {
                // Python-style integer modulo
                write!(
                    out,
                    "{{ typeof({l}) __t = {l} % {r}; {d} = __t + (__t != 0 && (__t ^ {r}) < 0 ? {r} : 0); }}",
                    d = v(*dst), l = v(*lhs), r = v(*rhs)
                ).unwrap();
            }
        }
        Inst::Neg { dst, operand, .. } => {
            write!(out, "{} = -{};", v(*dst), v(*operand)).unwrap();
        }

        // Bitwise
        Inst::BitAnd { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} & {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }
        Inst::BitOr { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} | {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }
        Inst::BitXor { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} ^ {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }
        Inst::BitNot { dst, operand, .. } => {
            write!(out, "{} = ~{};", v(*dst), v(*operand)).unwrap();
        }
        Inst::Shl { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} << {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }
        Inst::Shr { dst, lhs, rhs, .. } => {
            write!(out, "{} = {} >> {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
        }

        // Comparison & logic
        Inst::Cmp { dst, op, lhs, rhs } => {
            // Detect Str-typed operands for string comparison.
            let is_str = |vid: &ValueId| -> bool {
                if str_lit_vals.get(vid.0 as usize).copied().unwrap_or(false) { return true; }
                if let Some(Some(pt)) = ptr_pointee.get(vid.0 as usize) {
                    if let LirType::Struct(sid) = pt {
                        let name = sn.get(&sid.0).map(|s| s.as_str()).unwrap_or("");
                        if name == "Str" || name == "GorgetString" { return true; }
                    }
                }
                if let Some(Some(LirType::Struct(sid))) = val_types.get(vid.0 as usize) {
                    let name = sn.get(&sid.0).map(|s| s.as_str()).unwrap_or("");
                    if name == "Str" || name == "GorgetString" { return true; }
                }
                false
            };
            let lhs_str = is_str(lhs);
            let rhs_str = is_str(rhs);
            // Don't use string comparison when either operand is null —
            // this is a pointer null-check (e.g., from GIR-level Option wrapping),
            // not a string equality test.
            let lhs_null = null_vals.get(lhs.0 as usize).copied().unwrap_or(false);
            let rhs_null = null_vals.get(rhs.0 as usize).copied().unwrap_or(false);
            if (lhs_str || rhs_str) && !lhs_null && !rhs_null {
                // String comparison — wrap operands into Str values for gorget_str_eq/gorget_str_cmp.
                let wrap = |vid: &ValueId| -> String {
                    if str_lit_vals.get(vid.0 as usize).copied().unwrap_or(false) {
                        // StrLit values are already valid Str (from static .rodata).
                        format!("{v}", v = v(*vid))
                    } else if let Some(Some(pt)) = ptr_pointee.get(vid.0 as usize) {
                        if pt.is_aggregate() {
                            // Known Ptr-to-Str slot — dereference.
                            format!("(*(Str*){v})", v = v(*vid))
                        } else {
                            v(*vid)
                        }
                    } else if is_str_ptr_opt(val_types.get(vid.0 as usize).and_then(|t| t.as_ref()), module) {
                        // PtrTo(Str) value (e.g., from Option unwrap) — deref to Str.
                        format!("(*(Str*){v})", v = v(*vid))
                    } else {
                        v(*vid)
                    }
                };
                let lhs_c = wrap(lhs);
                let rhs_c = wrap(rhs);
                match op {
                    CmpOp::Eq => write!(out, "{} = gorget_str_eq({}, {});", v(*dst), lhs_c, rhs_c).unwrap(),
                    CmpOp::Ne => write!(out, "{} = !gorget_str_eq({}, {});", v(*dst), lhs_c, rhs_c).unwrap(),
                    _ => {
                        let c_op = match op {
                            CmpOp::Lt => "<",
                            CmpOp::Le => "<=",
                            CmpOp::Gt => ">",
                            CmpOp::Ge => ">=",
                            _ => unreachable!(),
                        };
                        write!(out, "{} = gorget_str_cmp({}, {}) {} 0;", v(*dst), lhs_c, rhs_c, c_op).unwrap();
                    }
                }
            } else {
                let c_op = match op {
                    CmpOp::Eq => "==",
                    CmpOp::Ne => "!=",
                    CmpOp::Lt => "<",
                    CmpOp::Le => "<=",
                    CmpOp::Gt => ">",
                    CmpOp::Ge => ">=",
                };
                write!(out, "{} = {} {} {};", v(*dst), v(*lhs), c_op, v(*rhs)).unwrap();
            }
        }
        Inst::Not { dst, operand } => {
            write!(out, "{} = !{};", v(*dst), v(*operand)).unwrap();
        }

        // Type conversions
        Inst::IntCast { dst, value, to } => {
            // GorgetString → int: extract the first byte (ASCII codepoint), not cast the pointer.
            let src_is_str_ptr = ptr_pointee.get(value.0 as usize)
                .and_then(|t| t.as_ref())
                .map_or(false, |t| matches!(t, LirType::Struct(sid) if {
                    module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetString")
                }));
            let src_is_str_val = !src_is_str_ptr && val_types.get(value.0 as usize)
                .and_then(|t| t.as_ref())
                .map_or(false, |t| matches!(t, LirType::Struct(sid) if {
                    module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetString")
                }));
            if src_is_str_ptr {
                // Value is a pointer to Str struct — deref and extract first byte of .data.
                write!(out, "{} = ({})((uint8_t)((const char*)((Str*)({}))-> data)[0]);", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
            } else if src_is_str_val {
                // Value is a Str struct by value — extract first byte of .data.
                write!(out, "{} = ({})((uint8_t)((const char*){}.data)[0]);", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
            } else {
                write!(out, "{} = ({})({});", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
            }
        }
        Inst::FloatCast { dst, value, to } => {
            write!(out, "{} = ({})({});", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
        }
        Inst::IntToFloat { dst, value, to } => {
            write!(out, "{} = ({})({});", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
        }
        Inst::FloatToInt { dst, value, to } => {
            write!(out, "{} = ({})({});", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
        }
        Inst::PtrCast { dst, value } => {
            write!(out, "{} = (void*)({});", v(*dst), v(*value)).unwrap();
        }
        Inst::Bitcast { dst, value, to } => {
            // Use memcpy for type-punning to avoid strict aliasing violations.
            write!(
                out,
                "memcpy(&{d}, &{s}, sizeof({t}));",
                d = v(*dst),
                s = v(*value),
                t = c_type_named(to, sn)
            ).unwrap();
        }

        // Memory
        Inst::Load { dst, ptr, ty } => {
            // Load from a pointer — always shallow deref.  The GIR drop elaborator
            // already determines ownership: if the loaded value needs freeing, it
            // emits a Drop/MoveZero.  Cloning resource types here would leak if no
            // corresponding Drop exists (which is the common case for collection
            // element reads — the collection owns the data).
            write!(out, "{} = *({} *)({});", v(*dst), c_type_named(ty, sn), v(*ptr)).unwrap();
        }
        Inst::Store { ptr, value } => {
            // Generic store — type is determined by context.
            let val_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
            let is_str_lit = str_lit_vals.get(value.0 as usize).copied().unwrap_or(false);
            if is_str_lit {
                // String literal → store the static Str pointer directly.
                write!(out, "*(Str*)({}) = {};", v(*ptr), v(*value)).unwrap();
            } else if matches!(val_ty, Some(LirType::Ptr)) {
                // Source is a pointer — either an aggregate reference (memcpy) or a raw pointer value (direct store).
                let val_is_null = null_vals.get(value.0 as usize).copied().unwrap_or(false);
                let pointee = ptr_pointee.get(value.0 as usize).and_then(|t| t.as_ref());
                let dst_pointee = ptr_pointee.get(ptr.0 as usize).and_then(|t| t.as_ref());
                if val_is_null {
                    // NullPtr → zero out destination (e.g. None variant of nested Option).
                    let size_ty = pointee.or(dst_pointee);
                    if let Some(ty) = size_ty {
                        let ty_name = c_type_named(ty, sn);
                        write!(out, "memset({p}, 0, sizeof({ty_name}));", p = v(*ptr)).unwrap();
                    } else {
                        write!(out, "*(void**)({p}) = NULL;", p = v(*ptr)).unwrap();
                    }
                // If the destination field/slot itself holds a pointer (Ptr/Void), store the pointer
                // value directly — don't memcpy through it.  This happens for MutRef captures
                // (void* fields holding &outer_var).
                } else if matches!(dst_pointee, Some(LirType::Ptr) | Some(LirType::PtrTo(_)) | Some(LirType::Void)) {
                    write!(out, "*(void**)({p}) = {val};", p = v(*ptr), val = v(*value)).unwrap();
                } else {
                    // Str and GorgetString are the same 32-byte struct (unified).
                    // Cross-type stores are just memcpy.
                    {
                        // Prefer destination pointee type for sizing (it's the allocation we write into).
                        let size_ty = dst_pointee.or(pointee);
                        if let Some(ty) = size_ty {
                            let ty_name = c_type_named(ty, sn);
                            write!(out, "memcpy({p}, {val}, sizeof({ty_name}));", p = v(*ptr), val = v(*value)).unwrap();
                        } else {
                            // Last resort — sizeof(*(val)) is wrong for void* but we have no type info.
                            write!(out, "memcpy({p}, {val}, sizeof(*({val})));", p = v(*ptr), val = v(*value)).unwrap();
                        }
                    }
                }
            } else if val_ty.map_or(false, |t| t.is_scalar()) {
                // Scalar — simple dereference store.
                let ty_name = c_type_named(val_ty.unwrap(), sn);
                write!(out, "*({ty_name}*)({p}) = {val};", p = v(*ptr), val = v(*value)).unwrap();
            } else {
                // Struct by value — take address for memcpy source.
                write!(out, "memcpy({p}, &{val}, sizeof({val}));", p = v(*ptr), val = v(*value)).unwrap();
            }
        }
        Inst::FieldPtr { dst, base, struct_id, field } => {
            let struct_def = &module.structs[struct_id.0 as usize];
            let sname = sn.get(&struct_id.0).map(|s| s.as_str()).unwrap_or("void");
            // Under 32-byte Str, GorgetString is an ordinary 4-field struct:
            // { data, cap, len, alloc }. FieldPtr uses normal field-name access
            // via the generic path below — no STR_HDR special case needed.
            if (*field as usize) < struct_def.fields.len() {
                let field_name = &struct_def.fields[*field as usize].0;
                if struct_def.is_union_layout && *field > 0 {
                    // Enum union layout: access through data.field_name
                    // For multi-field variants (e.g., IFunction_0, IFunction_1),
                    // the variant name is a prefix (IFunction) and fields are
                    // inside the variant's anonymous struct.
                    let variant_prefix = field_name.rsplitn(2, '_').nth(1).unwrap_or(field_name);
                    // Check if this variant has multiple fields (needs struct access)
                    let variant_field_count = struct_def.fields[1..].iter()
                        .filter(|(n, _)| n.rsplitn(2, '_').nth(1).unwrap_or(n) == variant_prefix)
                        .count();
                    if variant_field_count > 1 {
                        // Multi-field variant: data.VariantName.field_name
                        write!(
                            out,
                            "{} = (void*)&(({} *)({}))->data.{}.{};",
                            v(*dst), sname, v(*base),
                            c_field_name(variant_prefix), c_field_name(field_name)
                        ).unwrap();
                    } else {
                        // Single-field variant: data.field_name (no variant struct)
                        write!(
                            out,
                            "{} = (void*)&(({} *)({}))->data.{};",
                            v(*dst), sname, v(*base), c_field_name(field_name)
                        ).unwrap();
                    }
                } else {
                    // Regular struct or field 0 (tag): direct access
                    write!(
                        out,
                        "{} = (void*)&(({} *)({}))->{};",
                        v(*dst),
                        sname,
                        v(*base),
                        c_field_name(field_name)
                    ).unwrap();
                }
            } else {
                // Fallback: field index exceeds struct definition — use byte offset.
                // Expected for runtime-opaque structs (GorgetArray, GorgetMap, etc.)
                // whose LIR definitions have fewer fields than the actual C struct.
                // Unexpected for user-defined structs — likely a lowering bug.
                debug_assert!(
                    struct_def.fields.is_empty()
                        || sname.starts_with("Gorget") || sname.starts_with("__gg_Gorget"),
                    "FieldPtr field index {} out of bounds for non-opaque struct '{sname}' \
                     ({} fields). This is likely a LIR lowering bug — the struct has known \
                     fields but FieldPtr accesses beyond them.",
                    field, struct_def.fields.len(),
                );
                write!(
                    out,
                    "{} = (void*)((char*)({}) + {} * sizeof(void*)); /* {}.{} (oob) */",
                    v(*dst),
                    v(*base),
                    field,
                    sname,
                    field
                ).unwrap();
            }
        }
        Inst::ElemPtr { dst, base, index, elem_size } => {
            write!(
                out,
                "{} = (void*)((char*)({}) + (int64_t)({}) * {});",
                v(*dst),
                v(*base),
                v(*index),
                elem_size
            ).unwrap();
        }
        Inst::Memset { ptr, byte, size } => {
            write!(out, "memset({}, (int){}, (size_t){});", v(*ptr), v(*byte), v(*size)).unwrap();
        }
        Inst::Memcpy { dst_ptr, src_ptr, size } => {
            write!(
                out,
                "memcpy({}, {}, (size_t){});",
                v(*dst_ptr),
                v(*src_ptr),
                v(*size)
            ).unwrap();
        }

        // Calls
        Inst::Call { dst, func, args } => {
            let target_func = &module.functions[func.0 as usize];
            let fname = c_func_name(&target_func.name);
            if let Some(d) = dst {
                write!(out, "{} = ", v(*d)).unwrap();
            }
            write!(out, "{}(", fname).unwrap();
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                // Closure→callable wrapping: when passing a __Closure_N to a void/Ptr param,
                // wrap in (void*)(void*[2]){(void*)__Closure_N__call, (void*)&env_struct}.
                // Skip for __Closure_N__call functions — they take the env pointer directly.
                let is_closure_call_fn = target_func.name.contains("__call");
                let param_is_void = !is_closure_call_fn && target_func.params.get(i)
                    .map_or(false, |p| p.is_ptr() || matches!(p, LirType::Void));
                let arg_closure_name = if param_is_void {
                    let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                    let check_closure = |sid: &StructId| -> Option<String> {
                        module.structs.get(sid.0 as usize)
                            .filter(|sd| sd.name.starts_with("__Closure_"))
                            .map(|sd| sd.name.clone())
                    };
                    if let Some(LirType::Struct(sid)) = arg_ty {
                        check_closure(sid)
                    } else if matches!(arg_ty, Some(LirType::Ptr)) {
                        ptr_pointee.get(a.0 as usize).and_then(|p| p.as_ref())
                            .and_then(|pt| if let LirType::Struct(sid) = pt { check_closure(sid) } else { None })
                    } else { None }
                } else { None };
                if let Some(closure_name) = arg_closure_name {
                    let call_fn = format!("{closure_name}__call");
                    let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                    if matches!(arg_ty, Some(LirType::Ptr)) {
                        // arg is already a pointer to the closure struct
                        write!(out, "(void*)(void*[2]){{(void*){call_fn}, (void*){}}}", v(*a)).unwrap();
                    } else {
                        // arg is the closure struct by value — take its address
                        write!(out, "(void*)(void*[2]){{(void*){call_fn}, (void*)&{}}}", v(*a)).unwrap();
                    }
                } else if param_is_void {
                    let is_str_lit_arg = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
                    let is_const = target_func.const_params.get(i).copied().unwrap_or(false);
                    if is_str_lit_arg && is_const {
                        // String literal → Str const param: stack-allocated literal with header.
                        write!(out, "&{v}", v = v(*a)).unwrap();
                    } else if let Some(fid) = func_addr_targets.get(a.0 as usize).and_then(|t| *t) {
                        let adapt_name = format!("__adapt_{}", c_func_name(&module.functions[fid.0 as usize].name));
                        write!(out, "(void*)(void*[2]){{(void*){adapt_name}, NULL}}").unwrap();
                    } else {
                        emit_coerced_arg(out, a, target_func.params.get(i), val_types, str_lit_vals, sn);
                    }
                } else {
                    emit_coerced_arg(out, a, target_func.params.get(i), val_types, str_lit_vals, sn);
                }
            }
            write!(out, ");").unwrap();
        }
        Inst::CallExtern { dst, name, args, arg_abis, .. } => {
            emit_call_extern::emit_call_extern(out, inst, dst, name, args, arg_abis, ctx);
        }
        Inst::CallPtr { dst, callee, args } => {
            if let Some(d) = dst {
                write!(out, "{} = ", v(*d)).unwrap();
            }
            // Build the function pointer cast using actual arg types instead of void*
            // to avoid ABI mismatches with struct-by-value parameters.
            let ret_ty = dst.and_then(|d| val_types.get(d.0 as usize).and_then(|t| t.as_ref()))
                .map(|t| c_type_named(t, sn))
                .unwrap_or_else(|| "void".to_string());
            write!(out, "(({ret_ty}(*)(").unwrap();
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                let pointee = ptr_pointee.get(a.0 as usize).and_then(|t| t.as_ref());
                if let Some(pt) = pointee {
                    if pt.is_aggregate() {
                        // Pointer-to-aggregate: the target function likely expects by-value.
                        // Use the actual struct type.
                        write!(out, "{}", c_type_named(pt, sn)).unwrap();
                        continue;
                    }
                }
                match arg_ty {
                    Some(t) if t.is_aggregate() => write!(out, "{}", c_type_named(t, sn)).unwrap(),
                    _ => write!(out, "void*").unwrap(),
                }
            }
            write!(out, "))({}))(", v(*callee)).unwrap();
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                let pointee = ptr_pointee.get(a.0 as usize).and_then(|t| t.as_ref());
                if let Some(pt) = pointee {
                    if pt.is_aggregate() {
                        // Dereference pointer to get the struct by value.
                        write!(out, "*({}*){}", c_type_named(pt, sn), v(*a)).unwrap();
                        continue;
                    }
                }
                let is_str_lit_val = str_lit_vals.get(a.0 as usize).copied().unwrap_or(false);
                if is_str_lit_val {
                    // String literal → Ptr param: stack-allocated literal with header.
                    write!(out, "&{v}", v = v(*a)).unwrap();
                } else {
                    match arg_ty {
                        Some(t) if t.is_aggregate() => write!(out, "{}", v(*a)).unwrap(),
                        _ => write!(out, "(void*){}", v(*a)).unwrap(),
                    }
                }
            }
            write!(out, ");").unwrap();
        }

        // Runtime checks
        Inst::BoundsCheck { index, len } => {
            write!(
                out,
                "if ((uint64_t){} >= (uint64_t){}) {{ fprintf(stderr, \"index out of bounds\\n\"); abort(); }}",
                v(*index),
                v(*len)
            ).unwrap();
        }
        Inst::DivCheck { divisor } => {
            write!(
                out,
                "if ({} == 0) {{ fprintf(stderr, \"division by zero\\n\"); abort(); }}",
                v(*divisor)
            ).unwrap();
        }
        Inst::Trap { msg } => {
            write!(out, "fprintf(stderr, \"{}\"); abort();", escape_c_string(msg)).unwrap();
        }

        // Printf
        Inst::Printf { fmt, args } => {
            write!(out, "printf(\"{}\"", escape_c_string(fmt)).unwrap();
            for a in args {
                write!(out, ", {}", v(*a)).unwrap();
            }
            write!(out, ");").unwrap();
        }
        Inst::Fprintf { fd, fmt, args } => {
            // fd is a FILE* or fd int — for now treat as FILE*.
            write!(out, "fprintf((FILE*){}, \"{}\"", v(*fd), escape_c_string(fmt)).unwrap();
            for a in args {
                write!(out, ", {}", v(*a)).unwrap();
            }
            write!(out, ");").unwrap();
        }

        Inst::Nop => {
            write!(out, "/* nop */;").unwrap();
        }

        Inst::InlineC { dst, code } => {
            // Emit inline C code. For assignment patterns like `_X = expr;`,
            // rewrite `_X` to `__vN` (the SSA value name).
            if let Some(d) = dst {
                // Parse `_X = expr;` and rewrite to `__vN = expr;`
                if let Some(eq_pos) = code.find(" = ") {
                    let expr = &code[eq_pos + 3..];
                    // Rewrite local references `_N` to slot names `__sN` in the expression.
                    let rewritten = rewrite_inline_c_locals(expr, func);
                    write!(out, "{} = {};", v(*d), rewritten.trim_end_matches(';')).unwrap();
                } else {
                    write!(out, "/* inline_c: {} */;", code).unwrap();
                }
            } else {
                let rewritten = rewrite_inline_c_locals(code, func);
                write!(out, "{}", rewritten).unwrap();
            }
        }
    }
}


fn emit_term(out: &mut String, term: &Term, func: &LirFunction, module: &LirModule, sn: &HashMap<u32, String>, val_types: &[Option<LirType>]) {
    let v = |id: ValueId| -> String { format!("__v{}", id.0) };

    match term {
        Term::Ret(val) => {
            // For throws-int main, unwrap Result to exit code.
            let is_throws_main = func.name == "main" && matches!(&func.return_type, LirType::Struct(sid) if {
                module.structs.get(sid.0 as usize).map_or(false, |s| s.name.starts_with("Result__"))
            });
            if is_throws_main {
                let val_ty = val_types.get(val.0 as usize).and_then(|t| t.as_ref());
                let ty_name = c_type_named(&func.return_type, sn);
                let val_expr = if matches!(val_ty, Some(LirType::Ptr)) {
                    format!("*({ty_name}*){}", v(*val))
                } else {
                    v(*val)
                };
                write!(out, "{{ {ty_name} __res = {val_expr}; if (__res.tag == 0) {{ return 0; }} else {{ return __res.Error_0; }} }}").unwrap();
                return;
            }
            // If the function returns an aggregate but the value is a pointer, dereference.
            let val_ty = val_types.get(val.0 as usize).and_then(|t| t.as_ref());
            if func.return_type.is_aggregate() && matches!(val_ty, Some(LirType::Ptr)) {
                let ty_name = c_type_named(&func.return_type, sn);
                write!(out, "return *({ty_name}*){};", v(*val)).unwrap();
            } else {
                write!(out, "return {};", v(*val)).unwrap();
            }
        }
        Term::RetVoid => {
            write!(out, "return;").unwrap();
        }
        Term::Jump(target, args) => {
            emit_jump_args(out, *target, args, func);
            write!(out, "goto __bb{};", target.0).unwrap();
        }
        Term::Branch {
            cond,
            then_block,
            then_args,
            else_block,
            else_args,
        } => {
            writeln!(out, "if ({}) {{", v(*cond)).unwrap();
            if !then_args.is_empty() {
                write!(out, "        ").unwrap();
                emit_jump_args(out, *then_block, then_args, func);
                writeln!(out).unwrap();
            }
            writeln!(out, "        goto __bb{};", then_block.0).unwrap();
            writeln!(out, "    }} else {{").unwrap();
            if !else_args.is_empty() {
                write!(out, "        ").unwrap();
                emit_jump_args(out, *else_block, else_args, func);
                writeln!(out).unwrap();
            }
            writeln!(out, "        goto __bb{};", else_block.0).unwrap();
            write!(out, "    }}").unwrap();
        }
        Term::Switch {
            value,
            cases,
            default,
            default_args,
        } => {
            writeln!(out, "switch ((int64_t){}) {{", v(*value)).unwrap();
            for (val, block, args) in cases {
                write!(out, "        case {val}: ").unwrap();
                emit_jump_args(out, *block, args, func);
                writeln!(out, "goto __bb{};", block.0).unwrap();
            }
            write!(out, "        default: ").unwrap();
            emit_jump_args(out, *default, default_args, func);
            writeln!(out, "goto __bb{};", default.0).unwrap();
            write!(out, "    }}").unwrap();
        }
        Term::Unreachable => {
            write!(out, "__builtin_unreachable();").unwrap();
        }
    }
}

/// Emit parallel-copy assignments for block parameter passing.
/// Stores args into the target block's param temporaries (__bp{vid}).
fn emit_jump_args(out: &mut String, target: BlockId, args: &[ValueId], func: &LirFunction) {
    if args.is_empty() {
        return;
    }
    let target_block = &func.blocks[target.0 as usize];
    for (arg, (param_vid, _)) in args.iter().zip(target_block.params.iter()) {
        write!(out, "__bp{} = __v{}; ", param_vid.0, arg.0).unwrap();
    }
}


// ── Backend trait implementation ──────────────────────────────────────

/// C backend that consumes LIR.
pub struct CLirBackend;

impl super::Backend for CLirBackend {
    fn name(&self) -> &str {
        "c-lir"
    }

    fn generate(&self, module: &LirModule) -> super::CodegenOutput {
        super::CodegenOutput {
            code: generate_c(module),
            extension: "c",
        }
    }

    fn features(&self) -> super::BackendFeatures {
        super::BackendFeatures {
            debug_info: false,
            hot_reload: true,
            per_function_emit: true,
        }
    }

    fn emit_function(&self, func: &crate::lir::LirFunction, module: &LirModule) -> Option<String> {
        let sn = build_struct_names(module);
        let string_lit_map = HashMap::new();
        let mut out = String::new();
        emit_function(&mut out, func, module, &sn, &string_lit_map);
        Some(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generate_minimal() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("main".into(), vec![], LirType::I32);
        let bb0 = func.add_block();
        let v0 = func.next_value();
        func.block_mut(bb0).insts.push(Inst::IConst {
            dst: v0,
            ty: LirType::I32,
            value: 0,
        });
        func.block_mut(bb0).terminator = Term::Ret(v0);
        module.add_function(func);

        let c = generate_c(&module);
        assert!(c.contains("int main(int argc, char** argv)"));
        assert!(c.contains("__v0 = (int32_t)0LL;"));
        assert!(c.contains("return __v0;"));
    }

    #[test]
    fn generate_arithmetic() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("add".into(), vec![LirType::I64, LirType::I64], LirType::I64);
        let bb0 = func.add_block();

        let v0 = func.next_value(); // param a
        let v1 = func.next_value(); // param b
        let v2 = func.next_value(); // result

        let s0 = func.add_slot(LirType::I64, Some("a".into()));
        let s1 = func.add_slot(LirType::I64, Some("b".into()));

        func.block_mut(bb0).insts = vec![
            Inst::SlotLoad { dst: v0, slot: s0, ty: LirType::I64 },
            Inst::SlotLoad { dst: v1, slot: s1, ty: LirType::I64 },
            Inst::Add { dst: v2, ty: LirType::I64, lhs: v0, rhs: v1, overflow: Overflow::Trap },
        ];
        func.block_mut(bb0).terminator = Term::Ret(v2);
        module.add_function(func);

        let c = generate_c(&module);
        // With Overflow::Trap, addition uses __builtin_add_overflow.
        assert!(c.contains("__builtin_add_overflow") || c.contains("__v2 = __v0 + __v1;"));
        assert!(c.contains("return __v2;"));
    }

    #[test]
    fn generate_branch() {
        let mut module = LirModule::new();
        let mut func = LirFunction::new("test".into(), vec![], LirType::I32);

        let bb0 = func.add_block();
        let bb1 = func.add_block();
        let bb2 = func.add_block();

        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb0).insts.push(Inst::BoolConst { dst: v0, value: true });
        func.block_mut(bb0).terminator = Term::Branch {
            cond: v0,
            then_block: bb1,
            then_args: vec![],
            else_block: bb2,
            else_args: vec![],
        };

        func.block_mut(bb1).insts.push(Inst::IConst { dst: v1, ty: LirType::I32, value: 1 });
        func.block_mut(bb1).terminator = Term::Ret(v1);

        func.block_mut(bb2).insts.push(Inst::IConst { dst: v2, ty: LirType::I32, value: 2 });
        func.block_mut(bb2).terminator = Term::Ret(v2);

        module.add_function(func);

        let c = generate_c(&module);
        assert!(c.contains("if (__v0)"));
        assert!(c.contains("goto __bb1;"));
        assert!(c.contains("goto __bb2;"));
    }

    #[test]
    fn generate_struct() {
        let mut module = LirModule::new();
        let sid = module.add_struct(StructDef {
            name: "Point".into(),
            fields: vec![("x".into(), LirType::F64), ("y".into(), LirType::F64)],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None,
        });

        let mut func = LirFunction::new("get_x".into(), vec![LirType::Ptr], LirType::F64);
        let bb0 = func.add_block();
        let s0 = func.add_slot(LirType::Ptr, Some("p".into()));

        let v0 = func.next_value();
        let v1 = func.next_value();
        let v2 = func.next_value();

        func.block_mut(bb0).insts = vec![
            Inst::SlotLoad { dst: v0, slot: s0, ty: LirType::Ptr },
            Inst::FieldPtr { dst: v1, base: v0, struct_id: sid, field: 0 },
            Inst::Load { dst: v2, ptr: v1, ty: LirType::F64 },
        ];
        func.block_mut(bb0).terminator = Term::Ret(v2);
        module.add_function(func);

        let c = generate_c(&module);
        assert!(c.contains("struct __gg_Point"), "expected __gg_Point in output:\n{c}");
        assert!(c.contains("double x;"));
        assert!(c.contains("((__gg_Point *)(__v0))->x"));
    }

    #[test]
    fn escape_strings() {
        assert_eq!(escape_c_string("hello"), "hello");
        assert_eq!(escape_c_string("a\"b"), "a\\\"b");
        assert_eq!(escape_c_string("line\nend"), "line\\nend");
    }
}
