//! LIR → C backend.
//!
//! Thin 1:1 translation from LIR to C code. No semantic decisions —
//! all type coercions, drop calls, vtable dispatch, etc. are already
//! explicit in LIR instructions.

use crate::lir::*;
use crate::trap::TrapKind;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;

mod emit_call_extern;
mod emit_hof;
mod emit_printf;
mod emit_types;
pub mod helpers;
use self::helpers::*;
use self::emit_types::*;

/// Per-function analysis context for instruction emission.
///
/// Phase D6 (`docs/devbook/14-lir-ssa.md`): per-value origin info
/// (StrLit / NullPtr / CStr / FuncAddr / SpawnSource) is read directly
/// from `func.value_origins` via the typed accessors below — no parallel
/// per-value bitmaps in this struct.
pub(crate) struct EmitContext<'a> {
    pub func: &'a LirFunction,
    pub module: &'a LirModule,
    /// StructId → C type name mapping.
    pub sn: &'a HashMap<u32, String>,
    /// Per-value inferred types (indexed by ValueId).
    pub val_types: &'a [Option<LirType>],
    /// Per-value: the pointee type (if the value is a pointer).
    /// Not part of value_origins (it's a propagated type, not an origin tag).
    pub ptr_pointee: &'a [Option<LirType>],
    /// String content → static literal index (Phase 3).
    pub string_lit_map: &'a HashMap<String, usize>,
}

impl<'a> EmitContext<'a> {
    /// Typed accessor: the per-value origin (Phase D6 — read once, dispatch
    /// by `match`). Returns `None` if the value carries no origin tag.
    #[inline]
    pub fn origin(&self, v: ValueId) -> Option<&'a ValueOrigin> {
        self.func.value_origins.get(v.0 as usize).and_then(|o| o.as_ref())
    }

    /// Convenience predicate: is this value a string literal (`Inst::StrLit`)?
    #[inline]
    pub fn is_str_lit(&self, v: ValueId) -> bool {
        matches!(self.origin(v), Some(ValueOrigin::StrLit))
    }

    /// Convenience predicate: is this value a NULL pointer (`Inst::NullPtr`)?
    #[inline]
    pub fn is_null(&self, v: ValueId) -> bool {
        matches!(self.origin(v), Some(ValueOrigin::NullPtr))
    }

    /// Convenience predicate: is this value a const-char* (CStr-returning
    /// extern call)?
    #[inline]
    pub fn is_cstr(&self, v: ValueId) -> bool {
        matches!(self.origin(v), Some(ValueOrigin::CStr { .. }))
    }

    /// Convenience predicate: is this value an extern-"C" CStr return
    /// (vs a runtime-fn CStr return)? Only valid when `is_cstr` is true.
    #[inline]
    pub fn is_cstr_extern(&self, v: ValueId) -> bool {
        matches!(self.origin(v), Some(ValueOrigin::CStr { from_extern: true }))
    }

    /// Typed accessor: the spawn-source function suffix carried by a
    /// reshaped `__gorget_spawn_*` return value, if any.
    #[inline]
    pub fn spawn_source(&self, v: ValueId) -> Option<&'a str> {
        match self.origin(v) {
            Some(ValueOrigin::SpawnSource(s)) => Some(s.as_str()),
            _ => None,
        }
    }

    /// Typed accessor: is this value from `Inst::FuncAddr` (adapter-wrapped
    /// function address — a pointer to a closure-pack struct {fn_ptr, env})?
    /// Distinguishes from `Inst::NamedFuncAddr` (a bare function pointer
    /// scalar). The C backend's Store routing branches on this — adapter-
    /// wrapped form needs `memcpy(dst, src, sizeof(GorgetClosure))` (the
    /// source is a pointer to the 16-byte closure pack), bare named form
    /// needs `memcpy(dst, &src, sizeof(src))` (the source is a function
    /// pointer scalar).
    #[inline]
    pub fn is_func_addr(&self, v: ValueId) -> bool {
        matches!(self.origin(v), Some(ValueOrigin::FuncAddr(_)))
    }
}

/// Resolve a span to the panic-site (file, line, col) triple emitted into
/// runtime panic messages. None (synthetic instruction or absent file
/// info) returns the conventional <unknown>:0:0 fallback. The filename
/// is C-string-escaped so callers can interpolate it directly into a
/// generated `fprintf(stderr, "%s:%d:%d: ...\n", ...)`.
///
/// `FileInfo::filename_c_escaped` is pre-baked at FileInfo construction
/// (see span.rs), so this is a clone of a small String + a binary-search
/// for the line, not a re-escape of the filename per call. Was the
/// dominant codegen regression after stack-traces v1 (~17ms on
/// self_host_lowerer) before that pre-baking landed.
pub(crate) fn resolve_panic_loc(
    span: Option<crate::span::Span>,
    file_infos: &[crate::span::FileInfo],
) -> (String, u32, u32) {
    if let Some(s) = span {
        if let Some((fi, line, col)) = crate::span::offset_to_location_full(file_infos, s.start) {
            return (fi.filename_c_escaped.clone(), line, col);
        }
    }
    ("<unknown>".to_string(), 0, 0)
}

/// Whether an instruction's C emission needs a resolved `(file, line, col)`
/// triple for inline panic messages. Must stay in lockstep with the match
/// arms in `emit_inst` that read `loc.*` — adding a new panic emit site
/// without updating this predicate will silently emit `<unknown>:0:0`.
///
/// `CallExtern` is conservatively included: two name-driven branches inside
/// `emit_call_extern` / `emit_hof::try_emit_option_result_combinator` emit
/// `file:line:col` strings (`gorget_panic` + the `unwrap_err` combinator).
/// Gating those by name here would re-do the runtime-symbol routing the
/// emit sites already own (CLAUDE.md "name matching" exception lives at
/// the C-emit boundary, not in this predicate); the cost is a small set
/// of unnecessary resolutions on plain CallExterns.
fn inst_needs_loc(inst: &Inst) -> bool {
    use crate::lir::{Inst, LirType, Overflow};
    match inst {
        Inst::Add { overflow, ty, .. }
        | Inst::Sub { overflow, ty, .. }
        | Inst::Mul { overflow, ty, .. } => {
            *overflow == Overflow::Trap
                && matches!(ty, LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8)
        }
        // Div / Rem / Mod emit divide-by-zero + signed-overflow guards on
        // integer types; the float paths fall through to plain `/` / fmod()
        // with no panic site (IEEE-754 semantics).
        Inst::Div { ty, .. } | Inst::Rem { ty, .. } | Inst::Mod { ty, .. } => {
            !matches!(ty, LirType::F32 | LirType::F64)
        }
        Inst::Shl { .. } | Inst::Shr { .. } => true,
        Inst::BoundsCheck { .. } | Inst::DivCheck { .. } | Inst::Trap { .. } => true,
        Inst::CallExtern { .. } => true,
        _ => false,
    }
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
/// Render a `LirGlobalInitArg` as a C expression. `orig_to_lir` remaps
/// any user-type names embedded in `Sizeof` / `AddrOfInline` to their
/// LIR-mangled spelling — `Counter` → `__gg_Counter` etc., consistent
/// with the rest of the C-output type renaming.
fn render_global_init_arg_c(
    out: &mut String,
    arg: &crate::lir::LirGlobalInitArg,
    orig_to_lir: &[(String, String)],
) {
    use crate::lir::LirGlobalInitArg;
    use std::fmt::Write;
    let lir_name = |orig: &str| -> String {
        orig_to_lir.iter().find(|(o, _)| o == orig).map(|(_, l)| l.clone())
            .unwrap_or_else(|| orig.to_string())
    };
    match arg {
        LirGlobalInitArg::Int(n) => write!(out, "{n}LL").unwrap(),
        LirGlobalInitArg::Float(x) => write!(out, "{x}").unwrap(),
        LirGlobalInitArg::Bool(b) => write!(out, "{}", if *b { "1" } else { "0" }).unwrap(),
        LirGlobalInitArg::Sizeof(t) => write!(out, "sizeof({})", lir_name(t)).unwrap(),
        LirGlobalInitArg::StrLit(s) => {
            // Same escaping shape as `eval_static_init` used to apply
            // before the typed lift — \\ first to avoid double-escaping.
            let escaped = s
                .replace('\\', "\\\\").replace('"', "\\\"")
                .replace('\n', "\\n").replace('\r', "\\r").replace('\t', "\\t");
            write!(out, "\"{escaped}\"").unwrap();
        }
        LirGlobalInitArg::AddrOfInline { c_type, value } => {
            write!(out, "&({}){{", lir_name(c_type)).unwrap();
            render_global_init_arg_c(out, value, orig_to_lir);
            write!(out, "}}").unwrap();
        }
    }
}

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
        // Regex/Match are now pure-Gorget structs in lib/xtd/regex.gg —
        // no runtime mapping. (Previously routed to GorgetRegex /
        // GorgetRegexMatch when xtd.regex was a PCRE2 wrapper.)
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
/// Emit the `__gorget_ktable_hash__T` / `__gorget_ktable_eq__T` bridges
/// for every user type that has both `T__hash` (Hashable) and `T__eq`
/// (Equatable) impls. The bridges adapt the Gorget-side method ABI to
/// the runtime's `hash_fn(const void*) -> uint64_t` / `eq_fn(const void*,
/// const void*) -> bool` signatures so `GorgetMap` can dispatch into
/// user code at lookup time.
///
/// Scans every Dict/Set constructor name in the LIR to know which types
/// actually end up as keys — wrappers for unused types would be dead
/// code.
fn emit_hashable_key_bridges(out: &mut String, module: &LirModule) {
    use std::collections::BTreeSet;
    use std::fmt::Write;
    use crate::lir::Inst;
    let mut key_types: BTreeSet<String> = BTreeSet::new();
    // The post-LIR-lower `wire_collection_bridges` pass (lir::types) emits
    // an `Inst::SetCollectionBridge` for every user-keyed collection ctor
    // it finds — its `key_struct: StructId` directly names the user type
    // we need a bridge for. Iterate those (typed) and skip the legacy
    // string-parse of CallExtern names.
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                if let Inst::SetCollectionBridge { key_struct, .. } = inst {
                    if let Some(sd) = module.structs.get(key_struct.0 as usize) {
                        key_types.insert(sd.name.clone());
                    }
                }
            }
        }
    }
    if key_types.is_empty() { return; }
    writeln!(out, "\n// ── Hashable/Equatable runtime bridges for user-type Dict/Set keys ──").unwrap();
    for ty in &key_types {
        let (hash_name, eq_name) = match helpers::hashable_key_fn_names(ty, module) {
            Some(pair) => pair,
            None => continue,
        };
        // FxHasher from std.hash is a single-field struct `{ int state }`.
        // We allocate an `int64_t` and pass its address — ABI-compatible
        // with `FxHasher*`, avoiding a dependency on the FxHasher typedef
        // (which might get DCE'd from modules that don't touch it directly).
        writeln!(out,
            "static uint64_t __gorget_ktable_hash__{ty}(const void* __kp) {{ \
               int64_t __h_state = 0; \
               {hash_name}(__kp, &__h_state); \
               return (uint64_t)__h_state; }}").unwrap();
        // `bool eq(self, Self other)` lowers in two distinct shapes
        // depending on whether `Self` carries resource fields:
        //   - POD (no resource fields):       Type__eq(*Self, Self)        — second by value
        //   - Resource (String / Vector / …): Type__eq(*Self, *Self)       — second by pointer
        // The bridge's caller hands us two `const void*`. For the POD
        // shape we deref `__b` to recover the value; for the resource
        // shape we pass `__b` straight through. Inspect the actual
        // LIR signature to pick. The C struct name follows the
        // `build_struct_names` convention: `__gg_{ty}` for user types.
        let eq_fn = module.functions.iter().find(|f| f.name == eq_name);
        let other_is_ptr = match eq_fn.and_then(|f| f.params.get(1)) {
            Some(t) => matches!(t, crate::lir::LirType::Ptr | crate::lir::LirType::PtrTo(_)),
            None => false,
        };
        if other_is_ptr {
            writeln!(out,
                "static bool __gorget_ktable_eq__{ty}(const void* __a, const void* __b) {{ \
                   return {eq_name}(__a, __b); }}").unwrap();
        } else {
            let c_struct = format!("__gg_{ty}");
            writeln!(out,
                "static bool __gorget_ktable_eq__{ty}(const void* __a, const void* __b) {{ \
                   return {eq_name}(__a, *(const {c_struct}*)__b); }}").unwrap();
        }
    }
    writeln!(out).unwrap();
}

fn is_monomorphized_wrapper_type(name: &str) -> bool {
    name.starts_with("Channel__")
        || name.starts_with("Shared__")
        || name.starts_with("Weak__")
        || name.starts_with("Vector__") || name.starts_with("Deque__")
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
    ];
    let skip_struct = |def: &StructDef| -> bool {
        RUNTIME_STRUCTS.contains(&def.name.as_str())
            || runtime_defined_named.contains(&def.name.as_str())
            || is_monomorphized_wrapper_type(&def.name)
            || lir_to_runtime_name(&def.name).is_some()
            // Phase A residual #1: Named types tagged with `c_runtime_alias`
            // (e.g. `Callable__T_args` → "GorgetClosure") are emitted as a
            // typedef to the runtime struct further down — skip the
            // synthetic `__gg_X` struct definition entirely.
            || def.c_runtime_alias.is_some()
    };
    for (i, def) in module.structs.iter().enumerate() {
        if skip_struct(def) {
            // Emit `typedef <runtime> <c-name>;` for c_runtime_alias-tagged
            // types so the C name resolves to the runtime layout. Other
            // skipped categories (RUNTIME_STRUCTS, runtime_defined_named,
            // monomorphized wrappers, lir_to_runtime_name aliases) are
            // typedef'd elsewhere — don't double-emit here.
            if let Some(ref rt) = def.c_runtime_alias {
                let cname = &struct_names[&(i as u32)];
                writeln!(out, "typedef {rt} {cname};").unwrap();
            }
            continue;
        }
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
    // Non-trait boxes are void* (8B). Trait boxes are the 16B TraitObj layout
    // (`is_trait_box`); they MUST NOT get a void* placeholder here — the later
    // monomorphized `typedef <Trait>_TraitObj Box__Trait` would conflict
    // (Round XIX Track N2 cell D/E: `conflicting types for 'Box__Speaker'`).
    // Trait-box typedefs are emitted immediately after their TraitObj struct
    // body is complete (see the struct-definition loop below), mirroring
    // `emit_wrapper_typedef`'s is_trait_box branch (helpers.rs).
    {
        let mut box_seen = HashSet::new();
        // Collect all Box__* types referenced in struct field types.
        for def in &module.structs {
            for (_, fty) in &def.fields {
                let ft = c_type_named(fty, &struct_names);
                if ft.starts_with("Box__") && box_seen.insert(ft.clone()) {
                    let is_trait_box = module.structs.iter()
                        .find(|s| s.name == ft)
                        .map_or(false, |s| s.is_trait_box);
                    if is_trait_box {
                        // Skip — post-TraitObj typedef below / monomorphized path.
                        continue;
                    }
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
        // Round XIX Track N2 Class A: as soon as a TraitObj body is complete,
        // typedef `Box__<Trait>` to it so subsequent structs/enums that hold
        // `Box[Trait]` fields (Holder, Option__Box__Trait, …) see the 16-byte
        // layout — never the void* placeholder that conflicted with the later
        // monomorphized re-typedef. Identical re-emit from
        // `emit_wrapper_typedef` is a no-op under C11 (same type).
        if is_traitobj {
            if let Some(trait_name) = def.name.strip_suffix("_TraitObj") {
                let box_name = format!("Box__{trait_name}");
                let is_trait_box = module.structs.iter()
                    .find(|s| s.name == box_name)
                    .map_or(false, |s| s.is_trait_box);
                if is_trait_box {
                    writeln!(out, "typedef {cname} {box_name};").unwrap();
                }
            }
        }
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
            let ret_name = &tsf.ret_name;
            s.insert(format!("Thread__{ret_name}__join"));
            s.insert(format!("Thread__{ret_name}__id"));
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
        // Skip Option/Result combinator methods — generated as inline helpers.
        if parse_option_result_combinator(&ext.name).is_some() {
            continue;
        }
        // Skip monomorphized wrapper methods — inline wrappers emitted separately.
        if ext.name.starts_with("Channel__") || ext.name.starts_with("Shared__")
            || ext.name.starts_with("Weak__") || ext.name.starts_with("Mutex__")
            || ext.name.starts_with("RWLock__") || ext.name.starts_with("Guard__")
            || ext.name.starts_with("ReadGuard__") || ext.name.starts_with("WriteGuard__")
            || ext.name.starts_with("Box__")
            || ext.name.starts_with("Task__") {
            // Task__T__drop / Task__void__await are emitted as `static inline`
            // helpers by emit_spawn_helpers; a non-static forward decl here
            // would conflict with the static definition (the GIR
            // call_void("Task__void__await", …) → ensure_extern would otherwise
            // emit one). `is_runtime_fn` doesn't match `Task__` (only
            // gorget_/GORGET_/__gorget_), so this prefix skip is genuinely
            // needed. Safe: every Task__* C symbol is static inline; the
            // spawn/await helpers are __gorget_/__spawn_-prefixed, not Task__.
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
                // BoxDropAddr references the `Box__<inner>__drop` wrapper —
                // forward-declared with the function decls, so defer with them.
                LirGlobalInit::FuncAddr(_) | LirGlobalInit::BoxDropAddr(_) => true,
                LirGlobalInit::Struct { fields, .. } => fields.iter().any(check),
                LirGlobalInit::StaticArrayView { elems, .. } => elems.iter().any(check),
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
        emit_global_init(&mut out, &g.init, &g.ty, &module.functions, &module.structs, &struct_names);
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
        emit_global_init(&mut out, &g.init, &g.ty, &module.functions, &module.structs, &struct_names);
        writeln!(out, "; // {}", g.name).unwrap();
    }
    if !deferred_globals.is_empty() {
        writeln!(out).unwrap();
    }

    // Option/Result combinator helpers.
    // Must come after function forward declarations so closure __call functions are visible.
    if include_runtime || wrappers_only {
        emit_option_result_combinator_helpers(&mut out, module, &struct_names);
    }

    // Spawn/await helpers for async functions (blocking approach).
    if !module.spawned_fns.is_empty() && (include_runtime || wrappers_only) {
        emit_spawn_helpers(&mut out, module);
    }

    // Thread spawn/join helpers.
    if !module.thread_spawned_fns.is_empty() && (include_runtime || wrappers_only) {
        emit_thread_helpers(&mut out, module, &struct_names);
    }

    // Adapter functions for named functions passed as closures (FuncAddr → void* protocol).
    // When a named function is passed where a closure (void*) is expected, the call site
    // wraps it as (void*[2]){__adapt_fn, NULL}. The adapter ignores the env pointer and
    // forwards to the real function.
    // NOTE: In LLVM wrapper-only mode, adapters are generated inline in the LLVM IR
    // (using the raw function name, not the C-mangled __gg_ name). Skip them here to
    // avoid undefined-reference errors from C-mangled names like __gg_double that have
    // no corresponding LLVM IR symbol.
    if !wrappers_only {
        let mut adapter_fids: HashSet<u32> = HashSet::new();
        for func in &module.functions {
            for block in &func.blocks {
                for inst in &block.insts {
                    if let Inst::FuncAddr { func: fid, .. } = inst {
                        adapter_fids.insert(fid.0);
                    }
                    if let Inst::ClosurePack { call_func, needs_adapter: true, .. } = inst {
                        adapter_fids.insert(call_func.0);
                    }
                }
            }
        }
        for fid_raw in &adapter_fids {
            let target = &module.functions[*fid_raw as usize];
            let ret_c = c_type_named(&target.return_type, &struct_names);
            let adapt_name = format!("__adapt_{}", c_func_name(&target.name));
            let target_name = c_func_name(&target.name);
            // Cross-frame fault (Inc-2.1a): a PARTICIPATING fn has synthesized
            // trailing `MutPtr<i32>` fault-slot param(s) that are NOT part of its
            // callable type. The adapter is invoked through the 2-arg callable
            // ABI, so it must declare ONLY the user params and pass `NULL` for the
            // trailing slot(s) — forwarding a phantom slot arg writes a fault tag
            // through a wild pointer (memory corruption). NULL makes the callee's
            // fault arm panic inline = panic-by-default for an indirectly-invoked
            // fault (indirect propagation is deferred to 2.3b). Typed count off
            // the LIR function, never name/shape-matched (devbook/24 rule 2).
            let user_param_count = target.params.len().saturating_sub(target.fault_slot_param_count);
            // Signature: ret_type __adapt_fn(void* __env, USER params...)
            write!(out, "{ret_c} {adapt_name}(void* __env").unwrap();
            for (i, p) in target.params.iter().take(user_param_count).enumerate() {
                let ty_str = if matches!(p, LirType::Void) { "void*".to_string() } else { c_type_named(p, &struct_names) };
                write!(out, ", {ty_str} __p{i}").unwrap();
            }
            write!(out, ") {{ ").unwrap();
            if !matches!(target.return_type, LirType::Void) {
                write!(out, "return ").unwrap();
            }
            write!(out, "{target_name}(").unwrap();
            for i in 0..target.params.len() {
                if i > 0 { write!(out, ", ").unwrap(); }
                if i < user_param_count {
                    write!(out, "__p{i}").unwrap();
                } else {
                    // Synthesized trailing fault-slot: pass NULL (panic-by-default).
                    write!(out, "NULL").unwrap();
                }
            }
            writeln!(out, "); }}").unwrap();
        }
        if !adapter_fids.is_empty() {
            writeln!(out).unwrap();
        }
    } // end if !wrappers_only (adapter generation)

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
    // drop/clone calls B__drop/B__clone before B's definition). Also
    // forward-declare the matching `_inplace` clone wrappers — recursive
    // enum/struct clone bodies emit `T__clone_inplace(box_field)` calls in
    // the body that defines `T__clone` itself, so without this declaration
    // the C compiler treats `_inplace` as an implicit-int-returning function
    // (warning + undefined behavior on mismatched signature).
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
                writeln!(out, "void {clone_fn}_inplace(void*);").unwrap();
            }
        }
        if module.recursive_drop_structs.contains_key(sdef.name.as_str()) {
            let clone_fn = format!("{}__clone", sdef.name);
            if !module.functions.iter().any(|f| f.name == clone_fn) {
                writeln!(out, "void {clone_fn}_inplace(void*);").unwrap();
            }
        }
    }
    // Forward-declare __gorget_dtor_* functions so the recursive struct drops
    // AND the enum drops below can reference them — they're DEFINED later in
    // emit_type_drop_fns. This MUST precede emit_recursive_struct_drops: a
    // Recursive-drop struct with a Custom-drop FIELD now emits a
    // `__gorget_dtor_{Field}(...)` call (so the field's own inner resources
    // are freed, not just the user drop body), which would otherwise be an
    // implicit-declaration compile ERROR under
    // -Werror=implicit-function-declaration (clang ≥16 / Xcode default; only a
    // warning under gcc, which is why the suite's gcc build didn't catch it).
    for info in module.type_drop_fns.values() {
        if info.drop_fn_name.starts_with("__gorget_dtor_") {
            writeln!(out, "void {}(void* __p);", info.drop_fn_name).unwrap();
        }
    }
    // Emit struct drop functions for structs with Recursive drop strategy.
    // These are needed when a Recursive-drop struct appears as a field in
    // another struct — the parent's field drop calls {Name}__drop.
    emit_recursive_struct_drops(&mut out, module, &struct_names);
    emit_recursive_struct_clones(&mut out, module, &struct_names);
    emit_recursive_enum_clones(&mut out, module, &struct_names);
    emit_enum_drop_fns(&mut out, module, &struct_names);
    emit_type_drop_fns(&mut out, module, &struct_names);
    // Box wrappers MUST emit after the per-type T__drop fns above so the
    // wrappers' inner-recursion targets are defined when the C compiler
    // sees them. Forward declarations are emitted in emit_runtime_helpers.
    emit_box_drop_wrappers(&mut out, module);

    // ── Runtime Hashable/Equatable key bridges ───────────────────
    //
    // For every user type that implements both Hashable and Equatable
    // and appears as a Dict/Set key, emit a `__gorget_ktable_hash__T`
    // wrapper that constructs an FxHasher, forwards into `T__hash`,
    // and returns the state, plus a `__gorget_ktable_eq__T` wrapper
    // that forwards into `T__eq`. `emit_collection_constructor` wires
    // these into `GorgetMap.hash_fn` / `eq_fn` for user-keyed
    // constructors — without them the map falls back to byte-FNV +
    // memcmp which is incorrect for keys holding pointer fields
    // (String, Vector, …).
    emit_hashable_key_bridges(&mut out, module);

    // Function definitions (skipped in wrappers-only mode — bodies live in LLVM IR)
    if !wrappers_only {
        writeln!(out, "// ── Function Definitions ──").unwrap();
        let has_test_runner = !module.test_fns.is_empty() || !module.bench_fns.is_empty() || module.is_test_module;
        // Module-invariant lookup tables consumed by every `emit_function`'s
        // per-instruction analysis prologue. Built once here instead of being
        // re-derived by a linear `.find`/`.any`-by-name scan on every CallExtern
        // (~10.8k scans × full externs vec on the self-host lowerer).
        //   - `extern_by_name`: extern name → declaration, FIRST match wins
        //     (insert in iteration order, never overwrite — mirrors `.find`).
        //   - `struct_orig_names`: set of original struct names for the
        //     "is this runtime struct emitted in-module?" membership test.
        let mut extern_by_name: HashMap<&str, &LirExtern> = HashMap::with_capacity(module.externs.len());
        for e in &module.externs {
            extern_by_name.entry(e.name.as_str()).or_insert(e);
        }
        let struct_orig_names: HashSet<&str> =
            module.structs.iter().map(|s| s.name.as_str()).collect();
        for func in &module.functions {
            if has_test_runner && func.name == "main" {
                continue;
            }
            emit_function(&mut out, func, module, &struct_names, &string_lit_map,
                &extern_by_name, &struct_orig_names);
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


fn emit_function(out: &mut String, func: &LirFunction, module: &LirModule, sn: &HashMap<u32, String>, string_lit_map: &HashMap<String, usize>,
    extern_by_name: &HashMap<&str, &LirExtern>, struct_orig_names: &HashSet<&str>) {
    // For main() with a Result return type (throws-int main), override to int.
    // Read typed `enum_kind` (Phase A) — set at LIR struct registration.
    let is_throws_main = func.name == "main" && matches!(&func.return_type, LirType::Struct(sid) if {
        module.structs.get(sid.0 as usize).map_or(false, |s| s.enum_kind == crate::lir::EnumKind::Result)
    });
    let ret_type_str = if is_throws_main { "int".to_string() } else { c_type_named(&func.return_type, sn) };

    // `main` runs the user body on thread 0 (the OS main thread) — a plain
    // `int main(argc, argv)`, identical on NATIVE and FREESTANDING targets.
    // Running on thread 0 is the macOS/Cocoa requirement (UI init must be on
    // the main thread); slot-coalescing lets the self-host bootstrap fit a
    // plain ~8MB stack, so the old 64MB-pthread main runner (Fix B) is gone.
    // void-main → exit-0 is guaranteed at the GIR layer (main is given an
    // I32_TYPE return type in `src/ir/lowering/functions.rs`, predating
    // Fix B; bare/implicit void returns coerce to `return 0`), so do NOT add
    // an explicit `return 0` here. The self-host twin emits the identical
    // plain main (the `c_emit_comparison` user_fn_count gate counts the `) {`
    // openers on both sides — keep them symmetric).

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
    //
    // Layering discipline (rule 3, one source of truth per axis): the
    // LIR-canonical `func.value_types` and `func.pointee_types` (populated
    // by `compute_module_value_types` / `compute_module_pointee_types` in
    // `src/lir/types.rs`) are the source of truth. The C backend seeds its
    // local `val_types` / `ptr_pointee` from those shared tables and then
    // layers backend-specific fixups on top — guard accessor inference
    // from consumers, cross-type combinator, consumer-driven back-propagation,
    // InlineC→SlotStore. The fixups don't contradict the shared info; they
    // refine it where the shared pass returns `None` (polymorphic combinators)
    // or where the value's declared type doesn't match the C ABI the
    // consumer expects.
    let mut val_types: Vec<Option<LirType>> = vec![None; max_val as usize];
    let mut ptr_pointee: Vec<Option<LirType>> = vec![None; max_val as usize];
    // Propagate pointee types through Ptr-typed slots (SlotStore → SlotLoad).
    let mut slot_pointee: Vec<Option<LirType>> = vec![None; func.slots.len()];
    // Override the C type name for values whose LIR type can't represent runtime structs.
    let mut val_c_type_override: Vec<Option<String>> = vec![None; max_val as usize];
    let mut _collection_get_vals: Vec<bool> = vec![false; max_val as usize];
    // Normalize PtrTo → Ptr in val_types so generic pointer handling is
    // unaffected.  PtrTo info is available from slot/param types when needed.
    let norm = |ty: LirType| -> LirType {
        if matches!(ty, LirType::PtrTo(_)) { LirType::Ptr } else { ty }
    };
    // Seed from LIR-canonical typed sidecars.
    for (i, ty) in func.value_types.iter().enumerate() {
        if i >= max_val as usize { break; }
        if let Some(t) = ty {
            val_types[i] = Some(norm(t.clone()));
        }
    }
    for (i, pt) in func.pointee_types.iter().enumerate() {
        if i >= max_val as usize { break; }
        if let Some(t) = pt {
            ptr_pointee[i] = Some(t.clone());
        }
    }
    for block in &func.blocks {
        for (vid, ty) in &block.params {
            // Shared seed should already have populated this; backfill only
            // when the LIR-canonical pass left it None (defensive).
            if val_types[vid.0 as usize].is_none() {
                val_types[vid.0 as usize] = Some(norm(ty.clone()));
            }
        }
        for inst in &block.insts {
            // The shared seed has typed every value whose type is derivable
            // from LIR alone. Run the local `infer_inst_type` only for holes
            // — polymorphic externs whose return type the shared pass leaves
            // `None` so backend context can resolve them.
            let dst_typed = inst.dst().map_or(false, |d| {
                val_types.get(d.0 as usize).and_then(|t| t.as_ref()).is_some()
            });
            if !dst_typed {
                if let Some(ty) = infer_inst_type(inst, module, &val_types, &ptr_pointee, func) {
                    if let Some(dst) = inst.dst() {
                        val_types[dst.0 as usize] = Some(norm(ty));
                    }
                }
            }
            // Detect runtime struct returns that aren't in module.structs.
            //
            // Layering rule 4 (resolve-once write-through): prefer the typed
            // LIR extern declaration over a hardcoded name list. The extern's
            // `return_type: LirType` carries the StructId (set at lowering
            // time from the GIR ExternDecl's typed `return_type: TypeId`),
            // so we can read the struct's name directly from `module.structs`.
            //
            // The legacy `runtime_fn_return_struct(name)` name-list fallback
            // remains for runtime symbols whose return type didn't survive
            // through the LIR layer as a Struct (e.g. some extern declarations
            // use `LirType::Ptr` as the return type and the typed struct info
            // travels via a sidecar). When the extern's return is `Struct(sid)`
            // and the struct isn't in `module.structs` (i.e. it's a runtime
            // singleton not emitted by the user), record the override.
            if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                let typed_override = extern_by_name.get(name.as_str())
                    .and_then(|e| match &e.return_type {
                        LirType::Struct(sid) => module.structs
                            .get(sid.0 as usize)
                            .map(|s| s.name.clone()),
                        _ => None,
                    });
                let rt_name_owned: Option<String> = typed_override
                    .or_else(|| runtime_fn_return_struct(name).map(String::from));
                if let Some(rt_name) = rt_name_owned {
                    let in_module = struct_orig_names.contains(rt_name.as_str());
                    if !in_module {
                        val_c_type_override[d.0 as usize] = Some(rt_name);
                    }
                }
            }
            // Phase D6 (`docs/devbook/14-lir-ssa.md`): origin info for
            // this value lives in `func.value_origins` (typed `ValueOrigin`).
            // Backend reads it via `EmitContext::is_str_lit / is_null /
            // is_cstr / spawn_source`. The only side-effect we still need
            // here is overriding val_types[d] = Ptr for CStr-origin values
            // so they're declared `const char*` (not whatever the extern's
            // declared return is) — that survives even after the bitmaps
            // are gone.
            if let Some(dst) = inst.dst() {
                let didx = dst.0 as usize;
                if didx < max_val as usize {
                    if matches!(
                        func.value_origins.get(didx).and_then(|o| o.as_ref()),
                        Some(ValueOrigin::CStr { .. })
                    ) {
                        val_types[didx] = Some(LirType::Ptr);
                    }
                }
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
                Inst::GlobalAddr { dst, global } => {
                    // Track global pointee type so a Store into the global
                    // address knows the destination's size. Without this, a
                    // module-level `global = value` assignment falls back to
                    // `memcpy(&global, val, sizeof(*(val)))` which is
                    // `sizeof(void)` (== 1 in gcc) for a void* `val`,
                    // partially copying multi-word destinations like
                    // GorgetArray (32B) and leaving the rest of the global
                    // in its prior state (all zeros at startup → null
                    // .data with non-zero len-from-source = invalid). Fixed
                    // by reading `module.globals[global]`'s declared type.
                    if let Some(g) = module.globals.get(global.0 as usize) {
                        ptr_pointee[dst.0 as usize] = Some(g.ty.clone());
                    }
                }
                // Propagate pointee types through SlotStore→SlotLoad chains.
                // When a Ptr-typed slot stores a value with known pointee, propagate
                // to subsequent loads from that slot.
                Inst::SlotStore { slot, value, .. } => {
                    if let Some(pt) = ptr_pointee.get(value.0 as usize).and_then(|p| p.clone()) {
                        if matches!(func.slots[slot.0 as usize].ty, LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef) {
                            slot_pointee[slot.0 as usize] = Some(pt);
                        }
                    }
                }
                Inst::SlotLoad { dst, slot, .. } => {
                    if let Some(pt) = slot_pointee.get(slot.0 as usize).and_then(|p| p.clone()) {
                        ptr_pointee[dst.0 as usize] = Some(pt);
                    }
                }
                // Propagate pointee through pointer-identity casts.
                // A `(void*)src` cast produces a pointer to the same memory
                // — losing the pointee type makes Store fall back to the
                // 8-byte `*(void**)p = NULL` form for null stores, which
                // partially zeros multi-word destinations like Str (32B)
                // and creates `data=NULL, len>0` invariant breaks.
                // PtrCast is pure type-punning at the C level (`(void*)v`),
                // and Bitcast is a memcpy-shaped cast — neither changes
                // what the pointer addresses.
                Inst::PtrCast { dst, value } | Inst::Bitcast { dst, value, .. } => {
                    let prop = ptr_pointee.get(value.0 as usize).and_then(|p| p.clone());
                    if let Some(pt) = prop {
                        ptr_pointee[dst.0 as usize] = Some(pt);
                    }
                }
                _ => {}
            }
        }
    }

    // Propagate pointee through block-arg → block-param passing.
    // Block params are SSA's phi equivalent; without this, a Ptr-typed
    // block param with a known-pointee source value loses the pointee
    // type, and downstream Stores fall back to the 8-byte `*(void**)p
    // = NULL` form — which partially zeros multi-word destinations
    // like Str (32B) and creates `data=NULL, len>0` invariant breaks.
    // Iterate to fixed point: a block param can feed another block
    // param (loops, diamonds), so one pass isn't enough.
    {
        let mut changed = true;
        let mut iters = 0;
        while changed && iters < 16 {
            changed = false;
            iters += 1;
            for block in &func.blocks {
                // Collect (target_block, args) pairs from this block's terminator.
                let pairs: Vec<(BlockId, Vec<ValueId>)> = match &block.terminator {
                    Term::Jump(target, args) => vec![(*target, args.clone())],
                    Term::Branch { then_block, then_args, else_block, else_args, .. } => {
                        vec![(*then_block, then_args.clone()), (*else_block, else_args.clone())]
                    }
                    Term::Switch { cases, default, default_args, .. } => {
                        let mut v: Vec<(BlockId, Vec<ValueId>)> = cases.iter()
                            .map(|(_, b, a)| (*b, a.clone()))
                            .collect();
                        v.push((*default, default_args.clone()));
                        v
                    }
                    _ => continue,
                };
                for (target_id, args) in pairs {
                    let target_block = match func.blocks.get(target_id.0 as usize) {
                        Some(b) => b,
                        None => continue,
                    };
                    for (i, arg) in args.iter().enumerate() {
                        let param_vid = match target_block.params.get(i) {
                            Some((vid, _ty)) => *vid,
                            None => continue,
                        };
                        let param_idx = param_vid.0 as usize;
                        if ptr_pointee.get(param_idx).map_or(true, |p| p.is_none()) {
                            if let Some(pt) = ptr_pointee.get(arg.0 as usize).and_then(|p| p.clone()) {
                                ptr_pointee[param_idx] = Some(pt);
                                changed = true;
                            }
                        }
                    }
                }
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
                            if !matches!(slot_ty, LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef | LirType::Void) {
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

    // CallExtern→SlotStore slot-type override moved upstream to
    // `src/lir/types.rs::apply_callextern_slotstore_override`. The shared
    // `func.value_types` seed at the top of this function already reflects
    // it (layering rule 4: resolve once, write through).

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
    // a closure that returns U≠T, the result should be Option__U. Read the correct
    // result struct from the typed LirExtern.combinator_result_struct_id field set
    // by the LIR post-pass, instead of re-deriving by splitting the extern name.
    let mut slot_overrides: HashMap<u32, LirType> = HashMap::new();
    for block in &func.blocks {
        let insts = &block.insts;
        for (idx, inst) in insts.iter().enumerate() {
            if let Inst::CallExtern { dst: Some(d), name, .. } = inst {
                if parse_option_result_combinator(name).is_some() {
                    if let Some(result_sid) = extern_by_name.get(name.as_str())
                        .and_then(|e| e.combinator_result_struct_id)
                    {
                        let target_ty = LirType::Struct(result_sid);
                        val_types[d.0 as usize] = Some(target_ty.clone());
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
                    if !matches!(sty, LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef | LirType::Void) {
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
            if !matches!(func.return_type, LirType::Void | LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef) {
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

    // Fix A (#37 flip): emit the BODY (main-globals init + blocks) into a
    // side buffer FIRST, then declare only the `__v` ids / `__s` slots the
    // emitted body actually references (`mark_used_value_ids` — exact-token
    // scan of the body text, the one choke point every reference flows
    // through: block-param head copies, terminator-arg copies/returns,
    // slot carriers, InlineC-rewritten locals and test-cleanup glue all
    // surface there, which no typed inst-operand walk can see).
    // Declaring all 0..max_val ids left thousands of dead decls per giant
    // function; at -O0 each costs frame bytes, and the self-host
    // `lower_expr_inner` frame sat within <1KB of the recursion stack
    // cliff that killed the bootstrap on stock ulimits (the mirror fix
    // lives in lir_codegen.gg emit_function).
    let mut fnbody = String::new();
    {
    // Shadow `out` with the side buffer so the body-emission code below is
    // byte-identical to the pre-Fix-A emission (only the destination moved).
    let out = &mut fnbody;

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
            if let LirGlobalInit::Extern { name, args } = &g.init {
                // Module-level string literals — initialized as cap=0 rodata
                // views by `emit_global_init_value`. Skip the runtime ctor
                // call here; the static initializer already populated the
                // slot at load time.
                if crate::backend::c_lir::helpers::is_str_literal_view_init(
                    name, args, &g.ty, &module.structs,
                ) {
                    continue;
                }
                let mut expr = String::new();
                use std::fmt::Write;
                write!(expr, "{name}(").unwrap();
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { write!(expr, ", ").unwrap(); }
                    render_global_init_arg_c(&mut expr, arg, &orig_to_lir);
                }
                write!(expr, ")").unwrap();
                writeln!(out, "    __lir_g{gid} = {expr};").unwrap();
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
        ptr_pointee: &ptr_pointee,
        string_lit_map,
    };

    // Sentinel passed to `emit_inst` when an instruction's predicate
    // says it doesn't need a resolved (file, line, col) triple — the
    // match arms in `emit_inst` for such instructions never read these
    // fields. Allocated once per function rather than per block /
    // per instruction.
    let unresolved_loc = ("<unknown>".to_string(), 0u32, 0u32);

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

        // Instructions. `resolve_panic_loc` allocates a filename String and
        // binary-searches line_starts; gated by `inst_needs_loc` because
        // eager per-inst resolution was the dominant codegen regression
        // after stack-traces v1 (~6 s on self_host_lowerer).
        for (idx, inst) in block.insts.iter().enumerate() {
            let loc_storage;
            let loc: &(String, u32, u32) = if inst_needs_loc(inst) {
                let span = block.span_map.get(idx).copied().flatten();
                loc_storage = resolve_panic_loc(span, &module.file_infos);
                &loc_storage
            } else {
                &unresolved_loc
            };
            write!(out, "    ").unwrap();
            emit_inst(out, inst, &ectx, loc);
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
    } // end of the `out`-shadow scope — `out` is the real sink again

    // Fix A: scan the emitted body for the ids it actually references.
    let mut v_used = vec![false; max_val as usize];
    let mut s_used = vec![false; func.slots.len()];
    mark_used_value_ids(&fnbody, &mut v_used, &mut s_used);

    // Slot declarations (referenced only; emitted after cross-type fix-ups so slot_overrides are applied).
    for (i, slot) in func.slots.iter().enumerate() {
        if !s_used[i] {
            continue;
        }
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

    // Exact C declaration type string per used value (single source of truth
    // for both the per-value decl path and the coalescing pass). This is the
    // Rust emitter's OWN decl-ctype shape (incl. the CStr → const char* and
    // void → void* specials) — the self-host keys on its own simpler subset.
    let decl_ctype = |i: usize| -> String {
        // Use C type override if available (for runtime structs not in module.structs).
        if let Some(Some(c_override)) = val_c_type_override.get(i) {
            return c_override.clone();
        }
        // CStr-origin values are const char* — declare as such to avoid
        // const-discard warnings. Reads `func.value_origins` directly.
        if matches!(
            func.value_origins.get(i).and_then(|o| o.as_ref()),
            Some(ValueOrigin::CStr { .. })
        ) {
            return "const char*".to_string();
        }
        match val_types.get(i).and_then(|t| t.as_ref()) {
            Some(ty) => {
                let ts = c_type_named(ty, sn);
                // Void-typed values are used as opaque pointers — declare as void*.
                if ts == "void" {
                    "void*".to_string()
                } else {
                    ts
                }
            }
            // No type inferred — declare as void* to avoid undeclared-var errors.
            None => "void*".to_string(),
        }
    };

    // ── Liveness-based value-slot coalescing (the frame fix; rustc-equivalent) ──
    // gg emits each function as ONE flat C scope: all `__v{N}` SSA value-locals
    // up front. At -O0 the C compiler gives each its own stack slot, so the
    // frame is the SUM of all (mutually-exclusive) match arms' locals. We
    // compute SSA value liveness over the block CFG (block-args as phi) and
    // greedily coalesce values with disjoint live ranges + an IDENTICAL C decl
    // type onto one C local, emitted as `#define __vN __coalK` aliases (ZERO
    // body rewrite) + `#undef` after the function close. Deterministic: the
    // grouping sorts decl-type keys and iterates value ids ASCENDING, so slot
    // NUMBERING is stable run-to-run (load-bearing for fixed_point byte-
    // identity). A slot is shared only between values whose block-live-sets are
    // provably disjoint — no slot-aliasing of simultaneously-live values.
    let coalesce_assign = coalesce_assign_exact(func, &v_used, &decl_ctype);
    if coalesce_assign.is_empty() {
        // No coalescing possible (e.g. zero blocks) — fall back to one C local
        // per used value, declared with its exact decl-ctype.
        for i in 0..val_types.len() {
            if !v_used.get(i).copied().unwrap_or(false) {
                continue;
            }
            writeln!(out, "    {} __v{i};", decl_ctype(i)).unwrap();
        }
    } else {
        // Declare one C local per coalesced slot. The slot's decl type is the
        // decl-ctype of the LOWEST value id mapped to it (deterministic, and
        // every value in the slot shares the identical type string by
        // construction). Slot ids are assigned in deterministic order by
        // coalesce_assign_exact, so iterate slot ids ascending.
        let n_slots = coalesce_assign
            .iter()
            .filter_map(|s| *s)
            .max()
            .map_or(0, |m| m + 1);
        let mut slot_type: Vec<Option<String>> = vec![None; n_slots];
        for (i, slot) in coalesce_assign.iter().enumerate() {
            if let Some(s) = slot {
                if slot_type[*s].is_none() {
                    slot_type[*s] = Some(decl_ctype(i));
                }
            }
        }
        for (s, ty) in slot_type.iter().enumerate() {
            if let Some(ty) = ty {
                writeln!(out, "    {ty} __coal{s};").unwrap();
            }
        }
        // #define each used value to its coalesced slot. The body emission is
        // byte-identical (it still references `__vN` literally); the macro
        // rewrites it to `__coalK` at the C preprocessor.
        for (i, slot) in coalesce_assign.iter().enumerate() {
            if let Some(s) = slot {
                writeln!(out, "    #define __v{i} __coal{s}").unwrap();
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

    // The pre-emitted body (main-globals init + blocks).
    out.push_str(&fnbody);

    writeln!(out, "}}").unwrap();

    // Coalescing: #undef the per-value aliases so the function-local macro
    // names don't leak into the next function (each function re-derives its own
    // value→slot map, and a leaked `#define __v3 __coal0` would corrupt the
    // next function's `__v3`).
    for (i, slot) in coalesce_assign.iter().enumerate() {
        if slot.is_some() {
            writeln!(out, "#undef __v{i}").unwrap();
        }
    }

}

/// Fix A (#37 flip): mark every `__v<N>` / `__s<N>` token the emitted body
/// text references. EXACT-TOKEN semantics: after `__v`/`__s` the full digit
/// run is consumed, so the token ends at the first non-digit by construction
/// and a live `__v12` can never retain a dead `__v1`. Scanning the EMITTED
/// BODY is the load-bearing design choice: a typed inst-operand walk cannot
/// see block-param head copies (`__vN = __bpN`), terminator-arg
/// copies/returns, slot carriers, InlineC-rewritten locals, or test-cleanup
/// glue — the body text is the single choke point every reference flows
/// through. If an enumerator-based derivation is ever preferred, its
/// contract must cover ALL those routes (the #37 flip-enable brief (git history)
/// W1). The self-host twin is `mark_used_value_ids` in lir_codegen.gg.
fn mark_used_value_ids(body: &str, v_used: &mut [bool], s_used: &mut [bool]) {
    let b = body.as_bytes();
    let n = b.len();
    let mut i = 0usize;
    while i + 3 < n {
        if b[i] == b'_' && b[i + 1] == b'_' && (b[i + 2] == b'v' || b[i + 2] == b's') {
            let mut j = i + 3;
            let mut num = 0usize;
            let mut saw = false;
            while j < n && b[j].is_ascii_digit() {
                num = num.saturating_mul(10).saturating_add((b[j] - b'0') as usize);
                saw = true;
                j += 1;
            }
            if saw {
                let used = if b[i + 2] == b'v' { &mut *v_used } else { &mut *s_used };
                if num < used.len() {
                    used[num] = true;
                }
                i = j;
            } else {
                i += 3;
            }
        } else {
            i += 1;
        }
    }
}

/// Liveness-based value-slot coalescing keyed on the EXACT C decl-type string
/// supplied by `decl_ctype` (so two values sharing a slot declare identically).
/// Returns, per value id, the coalesced slot index it maps to (None for unused
/// values). Empty `Vec` when there are no blocks (the caller falls back to the
/// one-local-per-value decl path).
///
/// DETERMINISM (load-bearing for `fixed_point` byte-identity): the grouping
/// uses a `BTreeMap` keyed on the decl-type string (sorted keys) and the value
/// ids within each group are inserted in ASCENDING order (we iterate `0..nval`),
/// so slot numbering is identical run-to-run. The greedy interval coloring
/// scans slots in creation order and places each value in the first slot whose
/// live-set is disjoint from the value's — a value reuses a slot ONLY when
/// their block-live-sets are provably disjoint, so no two simultaneously-live
/// values ever share a slot.
fn coalesce_assign_exact(
    func: &LirFunction,
    v_used: &[bool],
    decl_ctype: &dyn Fn(usize) -> String,
) -> Vec<Option<usize>> {
    let nval = v_used.len();
    let mut assign: Vec<Option<usize>> = vec![None; nval];
    if func.blocks.is_empty() {
        return Vec::new();
    }
    let live_blocks = compute_live_blocks(func, v_used);
    // Group used values by their exact decl-ctype string. BTreeMap → keys
    // visited in sorted (deterministic) order; the 0..nval scan inserts value
    // ids ascending within each group.
    let mut by_type: std::collections::BTreeMap<String, Vec<usize>> =
        std::collections::BTreeMap::new();
    for k in 0..nval {
        if v_used[k] {
            by_type.entry(decl_ctype(k)).or_default().push(k);
        }
    }
    let mut next_slot = 0usize;
    for vals in by_type.values() {
        // (live-set, global slot index) for each slot in this type group.
        let mut slots: Vec<(HashSet<usize>, usize)> = Vec::new();
        for &k in vals {
            let lb: HashSet<usize> = live_blocks[k].iter().copied().collect();
            let mut placed = None;
            for (sl, gidx) in &mut slots {
                if sl.is_disjoint(&lb) {
                    sl.extend(lb.iter().copied());
                    placed = Some(*gidx);
                    break;
                }
            }
            let gidx = placed.unwrap_or_else(|| {
                let g = next_slot;
                next_slot += 1;
                slots.push((lb.clone(), g));
                g
            });
            assign[k] = Some(gidx);
        }
    }
    assign
}

/// Per-value block-live-set (the block indices where each used value is live).
/// Standard SSA value liveness: backward dataflow over the block CFG with
/// block-args modelled as phi (an arg is live at the end of the predecessor via
/// the terminator's `uses()`; a block param is a def at the start of the
/// successor). Reads exactly `Block::params` (defs), `Inst::uses()`/`dst()`,
/// and `Term::uses()` — the COMPLETE operand surface. The coalescing granularity
/// is per-block (a LOWER BOUND on the achievable yield), which keeps it sound:
/// a value reported live in a block is conservatively treated as live across
/// the whole block.
fn compute_live_blocks(func: &LirFunction, v_used: &[bool]) -> Vec<Vec<usize>> {
    let nval = v_used.len();
    let nblocks = func.blocks.len();
    let mut idx_of: HashMap<u32, usize> = HashMap::new();
    for (i, b) in func.blocks.iter().enumerate() {
        idx_of.insert(b.id.0, i);
    }
    let mut succ: Vec<Vec<usize>> = vec![Vec::new(); nblocks];
    for (i, b) in func.blocks.iter().enumerate() {
        for s in b.terminator.successors() {
            if let Some(&j) = idx_of.get(&s.0) {
                succ[i].push(j);
            }
        }
    }
    // Per-block def / upward-exposed-use sets.
    let mut def: Vec<Vec<bool>> = vec![vec![false; nval]; nblocks];
    let mut upward_use: Vec<Vec<bool>> = vec![vec![false; nval]; nblocks];
    for (i, b) in func.blocks.iter().enumerate() {
        let d = &mut def[i];
        let u = &mut upward_use[i];
        // Block params are defs at the very top of the block.
        for (vid, _) in &b.params {
            if (vid.0 as usize) < nval {
                d[vid.0 as usize] = true;
            }
        }
        for inst in &b.insts {
            for used in inst.uses() {
                let k = used.0 as usize;
                if k < nval && !d[k] {
                    u[k] = true;
                }
            }
            if let Some(dst) = inst.dst() {
                let k = dst.0 as usize;
                if k < nval {
                    d[k] = true;
                }
            }
        }
        // Terminator uses (incl. block-args) — live at the end of the block.
        for used in b.terminator.uses() {
            let k = used.0 as usize;
            if k < nval && !d[k] {
                u[k] = true;
            }
        }
    }
    // Backward fixpoint: live_in = use ∪ (live_out − def); live_out = ∪ succ live_in.
    let mut live_in: Vec<Vec<bool>> = vec![vec![false; nval]; nblocks];
    let mut live_out: Vec<Vec<bool>> = vec![vec![false; nval]; nblocks];
    let mut changed = true;
    while changed {
        changed = false;
        for i in (0..nblocks).rev() {
            let mut new_out = vec![false; nval];
            for &s in &succ[i] {
                for k in 0..nval {
                    if live_in[s][k] {
                        new_out[k] = true;
                    }
                }
            }
            let mut new_in = vec![false; nval];
            for k in 0..nval {
                if upward_use[i][k] || (new_out[k] && !def[i][k]) {
                    new_in[k] = true;
                }
            }
            if new_out != live_out[i] {
                live_out[i] = new_out;
                changed = true;
            }
            if new_in != live_in[i] {
                live_in[i] = new_in;
                changed = true;
            }
        }
    }
    let mut live_blocks: Vec<Vec<usize>> = vec![Vec::new(); nval];
    for i in 0..nblocks {
        for k in 0..nval {
            if !v_used[k] {
                continue;
            }
            if live_in[i][k] || live_out[i][k] || def[i][k] || upward_use[i][k] {
                live_blocks[k].push(i);
            }
        }
    }
    live_blocks
}

fn emit_inst(out: &mut String, inst: &Inst, ctx: &EmitContext, loc: &(String, u32, u32)) {
    let func = ctx.func;
    let module = ctx.module;
    let sn = ctx.sn;
    let val_types = ctx.val_types;
    // Per-value origin queries go through the typed accessors
    // `ctx.is_str_lit / is_null / is_cstr / is_cstr_extern / spawn_source`.
    let ptr_pointee = ctx.ptr_pointee;
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
            let is_str_lit_val = ctx.is_str_lit(*value);
            let is_cstr = ctx.is_cstr(*value);
            // GorgetClosure slot with non-closure value (e.g. void*, int64_t, or void from another slot):
            // memcpy to avoid type mismatch. The value is always a pointer to closure data
            // (from SlotAddr, array_get, etc.), even when LIR types it as I64.
            if slot_is_closure && !matches!(val_ty, Some(LirType::Struct(_))) {
                write!(out, "memcpy(&{}, (void*){}, sizeof(GorgetClosure));", s(*slot), v(*value)).unwrap();
                return;
            }
            // Implicit Result::Ok / Option::Some wrapping for primitives is
            // handled by the LIR lowerer (memset + tag + FieldPtr/Store sequence).
            if slot_is_str && is_str_lit_val {
                // String literal → Str slot: direct assign (Phase 3 — static .rodata, zero alloc).
                write!(out, "{} = {};", s(*slot), v(*value)).unwrap();
            } else if slot_is_str && is_cstr {
                if ctx.is_cstr_extern(*value) {
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
                if ctx.is_cstr_extern(*value) {
                    write!(out, "{} = gorget_str_from_cstr({});", s(*slot), v(*value)).unwrap();
                } else {
                    write!(out, "{} = gorget_string_adopt((char*){});", s(*slot), v(*value)).unwrap();
                }
            } else if slot_ty.is_aggregate() {
                // Aggregate store: source may be a pointer (SlotAddr) or a struct value (ParamRef, Call result).
                let val_is_ptr = matches!(val_ty, Some(LirType::Ptr));
                let val_is_null = ctx.is_null(*value);
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

        // Canonical ops — must have been expanded away by bir::lower before reaching here.
        // The BirModule newtype + validator guarantee this, but keep an explicit arm so
        // the pattern match stays exhaustive.
        Inst::SizeOf { .. }
        | Inst::EnumInit { .. }
        | Inst::EnumCheck { .. }
        | Inst::EnumExtract { .. }
        | Inst::StructInit { .. }
        | Inst::CowClone { .. }
        | Inst::TraitCall { .. }
        | Inst::HofExpand { .. }
        | Inst::AddressOf { .. }
        | Inst::BoxAlloc { .. } => {
            unreachable!("canonical LIR op survived BIR lowering — validator should have rejected it");
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
        Inst::NamedFuncAddr { dst, name } => {
            let cname = c_func_name(name);
            write!(out, "{} = (void*){cname};", v(*dst)).unwrap();
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
                write!(out, "if (__builtin_add_overflow(({ct}){l}, ({ct}){r}, &{d})) {{ gorget_trap_at(\"{ov}\", \"integer overflow\", \"{f}\", {ln}, {cl}); }}",
                    d = v(*dst), l = v(*lhs), r = v(*rhs), ov = TrapKind::Overflow.code(), f = loc.0, ln = loc.1, cl = loc.2).unwrap();
            } else {
                let ct = c_type_named(ty, sn);
                write!(out, "{d} = ({ct}){l} + ({ct}){r};", d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            }
        }
        Inst::Sub { dst, ty, lhs, rhs, overflow } => {
            if *overflow == Overflow::Trap && matches!(ty, LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8) {
                let ct = c_type_named(ty, sn);
                write!(out, "if (__builtin_sub_overflow(({ct}){l}, ({ct}){r}, &{d})) {{ gorget_trap_at(\"{ov}\", \"integer overflow\", \"{f}\", {ln}, {cl}); }}",
                    d = v(*dst), l = v(*lhs), r = v(*rhs), ov = TrapKind::Overflow.code(), f = loc.0, ln = loc.1, cl = loc.2).unwrap();
            } else {
                let ct = c_type_named(ty, sn);
                write!(out, "{d} = ({ct}){l} - ({ct}){r};", d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            }
        }
        Inst::Mul { dst, ty, lhs, rhs, overflow } => {
            if *overflow == Overflow::Trap && matches!(ty, LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8) {
                let ct = c_type_named(ty, sn);
                write!(out, "if (__builtin_mul_overflow(({ct}){l}, ({ct}){r}, &{d})) {{ gorget_trap_at(\"{ov}\", \"integer overflow\", \"{f}\", {ln}, {cl}); }}",
                    d = v(*dst), l = v(*lhs), r = v(*rhs), ov = TrapKind::Overflow.code(), f = loc.0, ln = loc.1, cl = loc.2).unwrap();
            } else {
                let ct = c_type_named(ty, sn);
                write!(out, "{d} = ({ct}){l} * ({ct}){r};", d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            }
        }
        Inst::Div { dst, ty, lhs, rhs } => {
            // Signed integer division has TWO C-UB cases:
            //   1. `b == 0` (already guarded)
            //   2. `a == TYPE_MIN && b == -1` — mathematical result is TYPE_MAX+1
            //      which doesn't fit in a signed integer. On ARM64 this wraps
            //      silently to TYPE_MIN (what we observed pre-fix); on x86 it
            //      raises SIGFPE. Both are non-portable. Guard explicitly and
            //      trap with the same `integer overflow` message as `+`/`*`.
            if matches!(ty, LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8) {
                let ct = c_type_named(ty, sn);
                let tmin = match ty {
                    LirType::I64 => "INT64_MIN",
                    LirType::I32 => "INT32_MIN",
                    LirType::I16 => "INT16_MIN",
                    LirType::I8  => "INT8_MIN",
                    _ => unreachable!(),
                };
                write!(out,
                    "if (({ct}){r} == 0) {{ gorget_trap_at(\"{dz}\", \"division by zero\", \"{f}\", {ln}, {cl}); }} \
                     if (({ct}){l} == {tmin} && ({ct}){r} == -1) {{ gorget_trap_at(\"{ov}\", \"integer overflow\", \"{f}\", {ln}, {cl}); }} \
                     {d} = ({ct}){l} / ({ct}){r};",
                    d = v(*dst), l = v(*lhs), r = v(*rhs), dz = TrapKind::DivByZero.code(), ov = TrapKind::Overflow.code(), f = loc.0, ln = loc.1, cl = loc.2).unwrap();
            } else if matches!(ty, LirType::U64 | LirType::U32 | LirType::U16 | LirType::U8) {
                let ct = c_type_named(ty, sn);
                write!(out, "if (({ct}){r} == 0) {{ gorget_trap_at(\"{dz}\", \"division by zero\", \"{f}\", {ln}, {cl}); }} {d} = ({ct}){l} / ({ct}){r};",
                    d = v(*dst), l = v(*lhs), r = v(*rhs), dz = TrapKind::DivByZero.code(), f = loc.0, ln = loc.1, cl = loc.2).unwrap();
            } else {
                write!(out, "{} = {} / {};", v(*dst), v(*lhs), v(*rhs)).unwrap();
            }
        }
        Inst::Rem { dst, ty, lhs, rhs, .. } => {
            if matches!(ty, LirType::F32 | LirType::F64) {
                write!(out, "{} = fmod({}, {});", v(*dst), v(*lhs), v(*rhs)).unwrap();
            } else if matches!(ty, LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8) {
                // Signed `%` is UB on `TYPE_MIN % -1` (C standard says so; many
                // compilers silently return 0 — a cross-backend defect since
                // LLVM-Rem and C-Div TRAP it). Match the Div guard: TRAP with
                // `integer overflow`, unconditionally, like the div0 guard
                // (spec/prose/trap-codes.md).
                let ct = c_type_named(ty, sn);
                let tmin = match ty {
                    LirType::I64 => "INT64_MIN",
                    LirType::I32 => "INT32_MIN",
                    LirType::I16 => "INT16_MIN",
                    LirType::I8  => "INT8_MIN",
                    _ => unreachable!(),
                };
                write!(out,
                    "if (({ct}){r} == 0) {{ gorget_trap_at(\"{dz}\", \"division by zero\", \"{f}\", {ln}, {cl}); }} \
                     if (({ct}){l} == {tmin} && ({ct}){r} == -1) {{ gorget_trap_at(\"{ov}\", \"integer overflow\", \"{f}\", {ln}, {cl}); }} \
                     {d} = ({ct}){l} % ({ct}){r};",
                    d = v(*dst), l = v(*lhs), r = v(*rhs), dz = TrapKind::DivByZero.code(), ov = TrapKind::Overflow.code(), f = loc.0, ln = loc.1, cl = loc.2).unwrap();
            } else {
                let ct = c_type_named(ty, sn);
                write!(out, "if (({ct}){r} == 0) {{ gorget_trap_at(\"{dz}\", \"division by zero\", \"{f}\", {ln}, {cl}); }} {d} = ({ct}){l} % ({ct}){r};",
                    d = v(*dst), l = v(*lhs), r = v(*rhs), dz = TrapKind::DivByZero.code(), f = loc.0, ln = loc.1, cl = loc.2).unwrap();
            }
        }
        Inst::Mod { dst, ty, lhs, rhs, .. } => {
            if matches!(ty, LirType::F32 | LirType::F64) {
                // Python-style float modulo: fmod(a,b) + (result has different sign from b ? b : 0)
                // fmod(x, 0.0) is defined as NaN in C, so no guard needed (IEEE-754).
                write!(
                    out,
                    "{{ double __t = fmod({l}, {r}); {d} = __t + (__t != 0.0 && ((__t < 0) != ({r} < 0)) ? {r} : 0.0); }}",
                    d = v(*dst), l = v(*lhs), r = v(*rhs)
                ).unwrap();
            } else if matches!(ty, LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8) {
                // Python-style integer modulo. Guard `/0` AND `TYPE_MIN % -1`
                // (C UB). For the overflow case, the Python-style result is 0
                // (since TYPE_MIN mod -1 mathematically = 0 anyway).
                let ct = c_type_named(ty, sn);
                let tmin = match ty {
                    LirType::I64 => "INT64_MIN",
                    LirType::I32 => "INT32_MIN",
                    LirType::I16 => "INT16_MIN",
                    LirType::I8  => "INT8_MIN",
                    _ => unreachable!(),
                };
                write!(
                    out,
                    "if ({r} == 0) {{ gorget_trap_at(\"{dz}\", \"division by zero\", \"{f}\", {ln}, {cl}); }} \
                     if (({ct}){l} == {tmin} && ({ct}){r} == -1) {{ {d} = 0; }} else \
                     {{ __typeof__({l}) __t = {l} % {r}; {d} = __t + (__t != 0 && (__t ^ {r}) < 0 ? {r} : 0); }}",
                    d = v(*dst), l = v(*lhs), r = v(*rhs), dz = TrapKind::DivByZero.code(), f = loc.0, ln = loc.1, cl = loc.2
                ).unwrap();
            } else {
                // Unsigned path — no TYPE_MIN issue.
                write!(
                    out,
                    "if ({r} == 0) {{ gorget_trap_at(\"{dz}\", \"division by zero\", \"{f}\", {ln}, {cl}); }} {{ __typeof__({l}) __t = {l} % {r}; {d} = __t + (__t != 0 && (__t ^ {r}) < 0 ? {r} : 0); }}",
                    d = v(*dst), l = v(*lhs), r = v(*rhs), dz = TrapKind::DivByZero.code(), f = loc.0, ln = loc.1, cl = loc.2
                ).unwrap();
            }
        }
        // Fault-catch checked arithmetic: set the bool FLAG without trapping and
        // WITHOUT computing the arithmetic result (the result is computed only on
        // the no-fault continuation path the `Term::Branch` falls through to —
        // critical for Div/Rem so no division-by-zero ever executes). For
        // Add/Sub/Mul the `__builtin_*_overflow` return value IS the flag; its
        // result is written to a throwaway temp. For Div/Rem the flag is the
        // div0 (+ signed TYPE_MIN/-1) predicate.
        Inst::FaultCheck { dst, op, ty, lhs, rhs } => {
            let ct = c_type_named(ty, sn);
            if let Some(builtin) = op.overflow_builtin() {
                // Add/Sub/Mul overflow.
                write!(out, "{{ {ct} __fc_discard; {d} = __builtin_{builtin}_overflow(({ct}){l}, ({ct}){r}, &__fc_discard); }}",
                    d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
            } else if matches!(op, FaultOp::DivOverflow) {
                // Signed `TYPE_MIN/-1` overflow of a Div/Rem — its OWN condition
                // (split out of div0 — spec/prose/trap-codes.md). Only signed
                // integer types can overflow this way; unsigned never does.
                if matches!(ty, LirType::I64 | LirType::I32 | LirType::I16 | LirType::I8) {
                    let tmin = match ty {
                        LirType::I64 => "INT64_MIN",
                        LirType::I32 => "INT32_MIN",
                        LirType::I16 => "INT16_MIN",
                        LirType::I8  => "INT8_MIN",
                        _ => unreachable!(),
                    };
                    write!(out, "{d} = (({ct}){l} == {tmin} && ({ct}){r} == -1);",
                        d = v(*dst), l = v(*lhs), r = v(*rhs)).unwrap();
                } else {
                    write!(out, "{d} = 0;", d = v(*dst)).unwrap();
                }
            } else {
                // Div/Rem div-by-zero ONLY (`rhs == 0`).
                write!(out, "{d} = (({ct}){r} == 0);", d = v(*dst), r = v(*rhs)).unwrap();
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
        Inst::Shl { dst, ty, lhs, rhs } => {
            // Two classes of C UB to defeat:
            //   1. shift-count >= bit-width (and negative counts, via the unsigned cast).
            //   2. shift-into-the-sign-bit of a signed integer (`1 << 63` on int64_t).
            // (1) is guarded explicitly. (2) is avoided by widening through the unsigned
            // companion type before shifting, which is well-defined for every count in
            // [0, width), then casting back. Bit pattern is preserved.
            let ct = c_type_named(ty, sn);
            let uct = match ty {
                LirType::I64 | LirType::U64 => "uint64_t",
                LirType::I32 | LirType::U32 => "uint32_t",
                LirType::I16 | LirType::U16 => "uint16_t",
                LirType::I8  | LirType::U8  => "uint8_t",
                _ => ct.as_str(),
            };
            write!(out, "if ((uint64_t){r} >= (uint64_t)(sizeof({ct}) * 8)) {{ gorget_trap_at(\"{ov}\", \"shift out of range\", \"{f}\", {ln}, {cl}); }} {d} = ({ct})(({uct}){l} << {r});",
                d = v(*dst), l = v(*lhs), r = v(*rhs), ov = TrapKind::Overflow.code(), f = loc.0, ln = loc.1, cl = loc.2).unwrap();
        }
        Inst::Shr { dst, ty, lhs, rhs } => {
            // C `>>` on signed negatives is implementation-defined (arithmetic shift on
            // every real target), so only the shift-count needs guarding.
            let ct = c_type_named(ty, sn);
            write!(out, "if ((uint64_t){r} >= (uint64_t)(sizeof({ct}) * 8)) {{ gorget_trap_at(\"{ov}\", \"shift out of range\", \"{f}\", {ln}, {cl}); }} {d} = ({ct}){l} >> {r};",
                d = v(*dst), l = v(*lhs), r = v(*rhs), ov = TrapKind::Overflow.code(), f = loc.0, ln = loc.1, cl = loc.2).unwrap();
        }

        // Comparison & logic (purely scalar — string comparisons are lowered
        // to CallExtern(gorget_str_eq/gorget_str_cmp) by the LIR lowerer).
        Inst::Cmp { dst, op, lhs, rhs } => {
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
        Inst::Not { dst, operand } => {
            write!(out, "{} = !{};", v(*dst), v(*operand)).unwrap();
        }

        // Type conversions (purely scalar — GorgetString→int is lowered
        // to CallExtern(gorget_str_ord) by the LIR lowerer).
        Inst::IntCast { dst, value, to } => {
            write!(out, "{} = ({})({});", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
        }
        Inst::FloatCast { dst, value, to } => {
            write!(out, "{} = ({})({});", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
        }
        Inst::IntToFloat { dst, value, to } => {
            write!(out, "{} = ({})({});", v(*dst), c_type_named(to, sn), v(*value)).unwrap();
        }
        Inst::FloatToInt { dst, value, to } => {
            // Rust `as`-style saturating conversion: NaN → 0, value ≥ TYPE_MAX+1
            // → TYPE_MAX, value < TYPE_MIN → TYPE_MIN. A raw C cast is UB out of
            // range and, on x86_64 (cvttsd2si), returns INT64_MIN for NaN and
            // both overflow directions — platform-dependent garbage we don't
            // want. Upper bound uses "next value past MAX" so the check is
            // exact even when MAX itself isn't representable as double (I64/U64).
            let t = c_type_named(to, sn);
            let val = v(*value);
            let (upper_bound, type_max, lower_bound, type_min): (&str, &str, &str, &str) = match to {
                LirType::I8 =>  ("128.0",                    "INT8_MAX",   "-128.0",                    "INT8_MIN"),
                LirType::I16 => ("32768.0",                  "INT16_MAX",  "-32768.0",                  "INT16_MIN"),
                LirType::I32 => ("2147483648.0",             "INT32_MAX",  "-2147483648.0",             "INT32_MIN"),
                LirType::I64 => ("9223372036854775808.0",    "INT64_MAX",  "-9223372036854775808.0",    "INT64_MIN"),
                LirType::U8 =>  ("256.0",                    "UINT8_MAX",  "0.0",                       "0"),
                LirType::U16 => ("65536.0",                  "UINT16_MAX", "0.0",                       "0"),
                LirType::U32 => ("4294967296.0",             "UINT32_MAX", "0.0",                       "0"),
                LirType::U64 => ("18446744073709551616.0",   "UINT64_MAX", "0.0",                       "0"),
                _ => {
                    write!(out, "{} = ({})({});", v(*dst), t, val).unwrap();
                    return;
                }
            };
            write!(
                out,
                "{d} = (({val}) != ({val})) ? ({t})0 : (({val}) >= {ub}) ? ({t}){tmax} : (({val}) < {lb}) ? ({t}){tmin} : ({t})({val});",
                d = v(*dst), val = val, t = t,
                ub = upper_bound, tmax = type_max,
                lb = lower_bound, tmin = type_min,
            ).unwrap();
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
            // Load from a pointer — always shallow deref. The GIR drop elaborator
            // already determines ownership: if the loaded value needs freeing, it
            // emits a Drop/MoveZero. Cloning resource types here would leak if no
            // corresponding Drop exists (which is the common case for collection
            // element reads — the collection owns the data).
            //
            // Trust `ty` when concrete; if it's `LirType::Void` (which would emit
            // illegal C `*(void *)(ptr)`), fall back to the pointer's pointee type
            // from `ptr_pointee` — that table is populated by `compute_pointer_pointees`
            // and reflects the actual FieldPtr/SlotAddr struct_id info the LIR carries.
            // This keeps the backend "dumb" — it reads declared facts, never re-infers —
            // but fills in when the LIR lowerer emitted an under-specified `Load.ty`.
            let effective_ty: LirType = if matches!(ty, LirType::Void) {
                ptr_pointee
                    .get(ptr.0 as usize)
                    .and_then(|t| t.as_ref())
                    .cloned()
                    .unwrap_or(LirType::Void)
            } else {
                ty.clone()
            };
            write!(out, "{} = *({} *)({});", v(*dst), c_type_named(&effective_ty, sn), v(*ptr)).unwrap();
        }
        Inst::Store { ptr, value } => {
            // Generic store — type is determined by context.
            let val_ty = val_types.get(value.0 as usize).and_then(|t| t.as_ref());
            let is_str_lit = ctx.is_str_lit(*value);
            if is_str_lit {
                // String literal → store the static Str pointer directly.
                write!(out, "*(Str*)({}) = {};", v(*ptr), v(*value)).unwrap();
            } else if matches!(val_ty, Some(LirType::FuncRef)) && !ctx.is_func_addr(*value) {
                // FuncRef from `Inst::NamedFuncAddr` — a bare function pointer
                // scalar (8 bytes). The Ptr branch below assumes the source is
                // an aggregate REFERENCE (so it does memcpy through the pointee
                // type). For a bare named function address there's no pointee
                // struct, only the pointer-sized address — take the address of
                // the value to write the bytes into the destination slot.
                //
                // Before this branch existed, `Inst::NamedFuncAddr` values were
                // typed `None` by the C backend's local pass (it had no case
                // for them), so they fell through to the struct-by-value branch
                // below. Once `func.value_types` (the shared LIR type table)
                // became the seed and started tagging these as `FuncRef`, the
                // Ptr branch grabbed them and emitted `memcpy(p, val, sizeof(*(val)))`
                // — UB for `void*` source. The fix is here, not at the writer.
                //
                // Distinction from `Inst::FuncAddr` (handled by the Ptr branch
                // below): FuncAddr produces a pointer TO a closure-pack struct
                // ({fn_ptr, env}, 16 bytes — see Inst::FuncAddr emission in
                // emit_inst, line ~1960), so its Store needs memcpy through
                // the pointee (16 bytes). The `is_func_addr` typed accessor
                // (Phase D6 ValueOrigin) cleanly distinguishes the two cases.
                write!(out, "memcpy({p}, &{val}, sizeof({val}));", p = v(*ptr), val = v(*value)).unwrap();
            } else if matches!(val_ty, Some(LirType::Ptr) | Some(LirType::FuncRef)) {
                // Source is a pointer-shaped value (raw Ptr, or Tier E §8.6 FuncRef
                // which lowers identically at this layer) — either an aggregate
                // reference (memcpy) or a raw pointer value (direct store).
                let val_is_null = ctx.is_null(*value);
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
                // If the destination field/slot itself holds a pointer (Ptr/Void/FuncRef),
                // store the pointer value directly — don't memcpy through it.
                // This happens for MutRef captures (void* fields holding &outer_var)
                // and for direct FuncRef-typed slots.
                } else if matches!(dst_pointee, Some(LirType::Ptr) | Some(LirType::PtrTo(_)) | Some(LirType::Void) | Some(LirType::FuncRef)) {
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
            } else if matches!(val_ty, Some(LirType::Void)) {
                // Void val_ty happens for ByValue params whose source type was
                // erased (closure params for instance: `F f` with F→int(int)
                // resolves to UNIT_TYPE at the IR param layer, emitting as
                // `void*` in C). The value is a raw pointer; if the
                // destination has a known pointee type, copy that many bytes
                // from the source pointer. Falls back to the conservative
                // `&val, sizeof(val)` form when nothing is known.
                let dst_pointee = ptr_pointee.get(ptr.0 as usize).and_then(|t| t.as_ref());
                if let Some(ty) = dst_pointee {
                    let ty_name = c_type_named(ty, sn);
                    write!(out, "memcpy({p}, {val}, sizeof({ty_name}));", p = v(*ptr), val = v(*value)).unwrap();
                } else {
                    write!(out, "memcpy({p}, &{val}, sizeof({val}));", p = v(*ptr), val = v(*value)).unwrap();
                }
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
                        || sname.starts_with("Gorget") || sname.starts_with("__gg_Gorget")
                        || struct_def.is_union_layout,
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
                // Closure→callable wrapping is now done in LIR (ClosurePack).
                // Only handle string-literal→const-param and default coercion.
                let param_is_void = target_func.params.get(i)
                    .map_or(false, |p| p.is_ptr() || matches!(p, LirType::Void));
                if param_is_void {
                    let is_str_lit_arg = ctx.is_str_lit(*a);
                    let is_const = target_func.const_params.get(i).copied().unwrap_or(false);
                    if is_str_lit_arg && is_const {
                        // String literal → Str const param: stack-allocated literal with header.
                        write!(out, "&{v}", v = v(*a)).unwrap();
                    } else {
                        emit_coerced_arg(out, a, target_func.params.get(i), val_types, ctx.func, sn);
                    }
                } else {
                    emit_coerced_arg(out, a, target_func.params.get(i), val_types, ctx.func, sn);
                }
            }
            write!(out, ");").unwrap();
        }
        Inst::CallExtern { dst, name, args, arg_abis, .. } => {
            emit_call_extern::emit_call_extern(out, dst, name, args, arg_abis, ctx, loc);
        }
        Inst::CallRuntime { dst, callee, args, arg_abis, .. } => {
            // BIR lowering normally rewrites CallRuntime → CallExtern (see
            // `bir::lower::expand_func`); this arm covers the per-function
            // debug emit path (`Backend::emit_function`) which bypasses BIR.
            // Both backends carry an equivalent fallback.
            let name = callee.c_name().to_string();
            emit_call_extern::emit_call_extern(out, dst, &name, args, arg_abis, ctx, loc);
        }
        Inst::CollectionCtor { .. } => {
            // CollectionCtor is a canonical-op; BIR lowering expands it into
            // a CallExtern (or CallRuntime) before backends ever see it.
            // Emit-function debug paths bypass BIR but never traverse a
            // collection ctor (those tests target lower-level instructions).
            unreachable!(
                "CollectionCtor reached backend; BIR lowering should have \
                 expanded it. Check that `bir::lower::expand_func` ran."
            );
        }
        Inst::CallPtr { dst, callee, args, ret_ty: call_ret_ty } => {
            if let Some(d) = dst {
                write!(out, "{} = ", v(*d)).unwrap();
            }
            // Build the function pointer cast using actual arg types instead of void*
            // to avoid ABI mismatches with struct-by-value parameters.
            //
            // Prefer the `ret_ty` carried on the instruction — it's
            // authoritative and survives BIR expansion (e.g. TraitCall
            // → CallPtr preserves the method's real return type). Fall
            // back to `val_types[dst]` for legacy emission paths, and
            // `void` if neither is known.
            let ret_ty = if !matches!(call_ret_ty, LirType::Void) {
                c_type_named(call_ret_ty, sn)
            } else {
                dst.and_then(|d| val_types.get(d.0 as usize).and_then(|t| t.as_ref()))
                    .map(|t| c_type_named(t, sn))
                    .unwrap_or_else(|| "void".to_string())
            };
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
                let is_str_lit_val = ctx.is_str_lit(*a);
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
        // Tier E §8.6: typed indirect call through a `LirType::FuncRef`.
        // Identical lowering to `CallPtr` (FuncRef → void* / void(*)() at the
        // ABI). The IR-level distinction exists for WASM and to keep "raw fn
        // ref" separate from "boxed closure".
        Inst::CallByRef { dst, fref, args, ret_ty: call_ret_ty } => {
            if let Some(d) = dst {
                write!(out, "{} = ", v(*d)).unwrap();
            }
            let ret_ty = if !matches!(call_ret_ty, LirType::Void) {
                c_type_named(call_ret_ty, sn)
            } else {
                dst.and_then(|d| val_types.get(d.0 as usize).and_then(|t| t.as_ref()))
                    .map(|t| c_type_named(t, sn))
                    .unwrap_or_else(|| "void".to_string())
            };
            write!(out, "(({ret_ty}(*)(").unwrap();
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                let pointee = ptr_pointee.get(a.0 as usize).and_then(|t| t.as_ref());
                if let Some(pt) = pointee {
                    if pt.is_aggregate() {
                        write!(out, "{}", c_type_named(pt, sn)).unwrap();
                        continue;
                    }
                }
                match arg_ty {
                    Some(t) if t.is_aggregate() => write!(out, "{}", c_type_named(t, sn)).unwrap(),
                    _ => write!(out, "void*").unwrap(),
                }
            }
            write!(out, "))({}))(", v(*fref)).unwrap();
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ").unwrap();
                }
                let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
                let pointee = ptr_pointee.get(a.0 as usize).and_then(|t| t.as_ref());
                if let Some(pt) = pointee {
                    if pt.is_aggregate() {
                        write!(out, "*({}*){}", c_type_named(pt, sn), v(*a)).unwrap();
                        continue;
                    }
                }
                let is_str_lit_val = ctx.is_str_lit(*a);
                if is_str_lit_val {
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
                "if ((uint64_t){} >= (uint64_t){}) {{ fprintf(stderr, \"{f}:{ln}:{cl}: index out of bounds\\n\"); abort(); }}",
                v(*index),
                v(*len),
                f = loc.0, ln = loc.1, cl = loc.2
            ).unwrap();
        }
        Inst::DivCheck { divisor } => {
            write!(
                out,
                "if ({} == 0) {{ fprintf(stderr, \"{f}:{ln}:{cl}: division by zero\\n\"); abort(); }}",
                v(*divisor),
                f = loc.0, ln = loc.1, cl = loc.2
            ).unwrap();
        }
        Inst::Trap { msg } => {
            write!(out, "fprintf(stderr, \"{f}:{ln}:{cl}: {}\"); abort();", escape_c_string(msg),
                f = loc.0, ln = loc.1, cl = loc.2).unwrap();
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

        Inst::MoveSlot { .. } => {
            // No-op: consumed by drop elaboration. Should not reach backend normally.
        }

        Inst::ClosurePack { slot, env_ptr, call_func, needs_adapter } => {
            let raw_name = c_func_name(&module.functions[call_func.0 as usize].name);
            let fn_name = if *needs_adapter {
                format!("__adapt_{raw_name}")
            } else {
                raw_name
            };
            write!(out, "{s} = (GorgetClosure){{.fn_ptr = (void*){fn_name}, .env = (void*){env}}};",
                s = s(*slot), env = v(*env_ptr)).unwrap();
        }

        Inst::CallClosure { dst, kind, closure, args, arg_abis, ret_ty } => {
            // Indirect call through a closure: fn_ptr(env, args...).
            let cv = v(*closure);
            let (fp, ep) = match kind {
                ClosureDispatchKind::CallableParam => {
                    // void*[2]: fn_ptr at [0], env at [1]
                    (format!("((void**){cv})[0]"), format!("((void**){cv})[1]"))
                }
                ClosureDispatchKind::EscapedClosure => {
                    // GorgetClosure struct: fn_ptr field, env field.
                    let cv_ty = val_types.get(closure.0 as usize).and_then(|t| t.as_ref());
                    let is_ptr = !matches!(cv_ty, Some(LirType::Struct(_)));
                    if is_ptr {
                        (format!("((GorgetClosure*){cv})->fn_ptr"), format!("((GorgetClosure*){cv})->env"))
                    } else {
                        (format!("{cv}.fn_ptr"), format!("{cv}.env"))
                    }
                }
            };
            use crate::ir::abi::AbiKind;
            let ret_type = c_type_named(ret_ty, sn);
            // Build parameter types: env (void*) first, then user args.
            //
            // Closure ABI per-arg must match `__Closure_N__call` (see
            // `ir/lowering/closures.rs::resolve_param_type`) and the
            // `__adapt_*` shim (defined above in this file), which both use:
            //   - by-pointer (`void*`) for **resource-containing** aggregates
            //     (structs that hold heap buffers — String, Vector, Dict, or
            //     any user struct transitively containing one of those)
            //   - by-value for non-resource aggregates (e.g. `Option[int]`)
            //
            // Getting this wrong is an ABI mismatch that's silent on AAPCS64
            // (macOS arm64 passes large structs via a hidden-pointer slot,
            // which the adapter happens to read as a valid `void*`) but
            // SIGSEGVs on x86-64 SysV. That's what hid the httpserver_before/
            // middleware/router{,_extended} regressions from local runs.
            //
            // The original check only named five runtime structs as
            // "resource"; user structs that contained resources (e.g.
            // HttpServerResponse with a `Dict[String, String] headers` field)
            // slipped through as "non-resource", got dereffed, and reached
            // the adapter as a by-value struct where a pointer was expected.
            // `struct_contains_resource` walks the field graph transitively.
            let mut param_types = vec!["void*".to_string()];
            let mut deref_args: Vec<Option<String>> = Vec::new();
            for (i, a) in args.iter().enumerate() {
                let abi = arg_abis.get(i).copied().unwrap_or(AbiKind::Auto);
                let pointee = ptr_pointee.get(a.0 as usize).and_then(|t| t.as_ref());
                let pointee_is_resource = |pt: &LirType| {
                    if let LirType::Struct(sid) = pt {
                        struct_contains_resource(*sid, module)
                    } else { false }
                };
                let needs_deref = match abi {
                    // ByValue was promoted for small non-union aggregates.
                    // Only honour it when the pointee is a non-resource
                    // aggregate — resource aggregates must stay by-pointer
                    // to match the callee signature (adapters always take
                    // resource args as `void*`).
                    AbiKind::ByValue => pointee.map_or(false, |pt| pt.is_aggregate() && !pointee_is_resource(pt)),
                    AbiKind::Auto => pointee.map_or(false, |pt| pt.is_aggregate() && !pointee_is_resource(pt)),
                    _ => false,
                };
                if needs_deref {
                    if let Some(pt) = pointee {
                        param_types.push(c_type_named(pt, sn));
                        deref_args.push(Some(c_type_named(pt, sn)));
                    } else {
                        param_types.push("void*".to_string());
                        deref_args.push(None);
                    }
                } else {
                    let ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref())
                        .map(|t| c_type_named(t, sn)).unwrap_or_else(|| "int64_t".to_string());
                    param_types.push(ty);
                    deref_args.push(None);
                }
            }
            let cast = format!("{}(*)({})", ret_type, param_types.join(", "));
            if let Some(d) = dst {
                write!(out, "{} = ", v(*d)).unwrap();
            }
            write!(out, "(({cast})({fp}))({ep}").unwrap();
            for (i, a) in args.iter().enumerate() {
                if let Some(ref ty_name) = deref_args[i] {
                    write!(out, ", *({}*){}", ty_name, v(*a)).unwrap();
                } else {
                    write!(out, ", {}", v(*a)).unwrap();
                }
            }
            write!(out, ");").unwrap();
        }
        Inst::DropGuardOpen { kind, value } => {
            match kind {
                DropGuardKind::Bool => {
                    write!(out, "{{ if ({}) {{", v(*value)).unwrap();
                }
                DropGuardKind::NonZero { size } => {
                    let addr = v(*value);
                    write!(out, "{{ char __dia_z[{size}] = {{0}}; if (memcmp({addr}, __dia_z, {size}) != 0) {{").unwrap();
                }
            }
        }
        Inst::DropGuardClose => {
            write!(out, "}} }}").unwrap();
        }

        Inst::SetCollectionBridge { collection, is_set: _, key_struct } => {
            // Wire the runtime hash_fn / eq_fn slots to the user-derived
            // bridges. The bridge symbols (`__gorget_ktable_hash__T` /
            // `__gorget_ktable_eq__T`) are emitted earlier by
            // `emit_hashable_key_bridges`. The struct cast is the same
            // for Set and Dict — both alias `GorgetMap` at the runtime
            // layout level.
            let key_name = &module.structs[key_struct.0 as usize].name;
            write!(out,
                "{}.hash_fn = (__gorget_hash_fn)__gorget_ktable_hash__{key_name}; \
                 {}.eq_fn = (__gorget_eq_fn)__gorget_ktable_eq__{key_name};",
                v(*collection), v(*collection)
            ).unwrap();
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
            // For throws-int main, unwrap Result to exit code. Read typed
            // `enum_kind` (Phase A) — set at LIR struct registration.
            let is_throws_main = func.name == "main" && matches!(&func.return_type, LirType::Struct(sid) if {
                module.structs.get(sid.0 as usize).map_or(false, |s| s.enum_kind == crate::lir::EnumKind::Result)
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

    fn generate(&self, module: &crate::bir::BirModule) -> super::CodegenOutput {
        // Backends consume BIR (guaranteed by the newtype); the C emitter
        // works on the underlying LirModule for its 1:1 translation.
        super::CodegenOutput {
            code: generate_c(module.as_lir()),
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
        let mut extern_by_name: HashMap<&str, &LirExtern> = HashMap::with_capacity(module.externs.len());
        for e in &module.externs {
            extern_by_name.entry(e.name.as_str()).or_insert(e);
        }
        let struct_orig_names: HashSet<&str> =
            module.structs.iter().map(|s| s.name.as_str()).collect();
        emit_function(&mut out, func, module, &sn, &string_lit_map,
            &extern_by_name, &struct_orig_names);
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
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
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
