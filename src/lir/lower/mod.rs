//! GIR → LIR lowering.
//!
//! Converts a GIR `Module` into an `LirModule` in pre-SSA form (slot-based).
//! This is incremental — each phase adds support for more GIR constructs.
//!
//! Phase 2.1: Scalars, arithmetic, comparison, control flow, basic calls.
//! Phase 2.2: Function calls (Call, CallExtern, CallIndirect).
//! Phase 2.3: Structs + field access.
//! Phase 2.4: Enums + match (Switch).
//! Phase 2.5: Type conversions.

pub mod calls;
pub(super) mod drops;
pub(super) mod insts;
pub(super) mod lifts;
pub(super) mod operands;
pub mod types;

// Re-export functions from sub-modules so they're accessible within this module.
// Re-export the public entry point so it remains at `lir::lower::lower_module`.
pub use self::types::lower_module;

#[allow(unused_imports)]
use self::calls::{
    fix_printf_format, lower_binop, lower_unop, map_cmp_op,
    clone_fn_for_collection_element, is_type_name,
    map_monomorphized_to_runtime_with_table, map_monomorphized_to_runtime,
    map_monomorphized_to_runtime_with_operand_types,
};

#[allow(unused_imports)]
use self::types::{
    c_sizeof_lir_type, c_sizeof_with_structs, c_sizeof_struct_def, c_sizeof_tuple_fields,
    elem_size_from_monomorphized, concurrency_elem_size,
    dict_elem_sizes_from_monomorphized, dict_key_type_from_monomorphized,
    set_elem_type_from_monomorphized, lir_type_sizeof, lower_global_init,
    gir_type_to_c, spawn_param_c_type,
};

use crate::ir;
use crate::ir::instructions::{
    BinOp as GirBinOp, CmpOp as GirCmpOp, Constant, Instruction, Operand, Place,
    Projection, Terminator, UnOp as GirUnOp,
};
use crate::ir::types::{
    self as gir_types, GirType, TypeId as GirTypeId, TypeRegistry,
};

use super::types::{StructRegistry, builtin_struct_defs};
use super::*;

/// Context for lowering a single GIR module to LIR.
pub struct LoweringContext<'a> {
    /// The GIR module being lowered.
    gir: &'a ir::Module,
    /// The LIR module being built.
    pub module: LirModule,
    /// Struct name → StructId mapping.
    struct_reg: StructRegistry,
    /// GIR function name → LIR FuncId mapping (for Call resolution).
    func_index: std::collections::HashMap<String, FuncId>,
    /// GIR global name → LIR GlobalId mapping (for GlobalRef resolution).
    global_index: std::collections::HashMap<String, GlobalId>,
}

/// Context for lowering a single GIR function.
pub(super) struct FuncLowering<'a> {
    pub(super) gir_func: &'a ir::Function,
    pub(super) gir_types: &'a TypeRegistry,
    pub(super) lir_func: LirFunction,
    /// GIR LocalId → LIR SlotId mapping.
    pub(super) local_to_slot: Vec<SlotId>,
    /// GIR BlockId → LIR BlockId mapping.
    pub(super) block_map: Vec<BlockId>,
    /// Struct registry reference (for FieldPtr).
    pub(super) struct_reg: &'a StructRegistry,
    /// Function name → FuncId (for Call).
    pub(super) func_index: &'a std::collections::HashMap<String, FuncId>,
    /// Global name → GlobalId (for GlobalRef/GlobalAssign).
    pub(super) global_index: &'a std::collections::HashMap<String, GlobalId>,
    /// Module struct definitions (for field-count checking).
    pub(super) module_structs: &'a [StructDef],
    /// Mangled-collection-name → backing runtime StructId map. Threaded
    /// through size-of helpers (`c_sizeof_with_structs`,
    /// `elem_size_from_monomorphized`, `concurrency_elem_size`,
    /// `dict_elem_sizes_from_monomorphized`) so per-monomorphization size
    /// reads resolve through `LirModule::struct_aliases` to the backing
    /// runtime StructDef's typed `computed_c_size` instead of name-prefix
    /// table lookups in `opaque_runtime_size`. Layering-discipline §Rule 3.
    pub(super) module_struct_aliases: &'a std::collections::HashMap<String, StructId>,
    /// Module globals (for type lookup).
    pub(super) module_globals: &'a [LirGlobal],
    /// Synthetic externs discovered during lowering (for unknown Call targets).
    pub(super) pending_externs: Vec<LirExtern>,
    /// Types whose `{Name}__drop` collides with a user method (e.g., DataFrame.drop()).
    pub(super) drop_collision_types: &'a std::collections::HashSet<String>,
    /// Monomorphized method name → runtime callee metadata (from BuiltinTypeProtocol).
    pub(super) runtime_callees: &'a rustc_hash::FxHashMap<String, crate::ir::RuntimeCalleeInfo>,
    /// Enum types needing tag-based variant drop dispatch.
    pub(super) recursive_drop_enums: &'a std::collections::HashMap<String, Vec<(u32, String, String, String, String)>>,
    /// Struct types with field-level drop functions.
    pub(super) recursive_drop_structs: &'a std::collections::HashMap<String, Vec<(String, String, String)>>,
    pub(super) type_drop_fns: &'a std::collections::HashMap<String, crate::lir::TypeDropInfo>,
    /// Extern ABI kinds from module declarations (fn_name → Vec<AbiKind>).
    pub(super) extern_abi_kinds: &'a rustc_hash::FxHashMap<String, Vec<crate::ir::abi::AbiKind>>,
    /// Extern return ABI kinds (fn_name → AbiKind).
    pub(super) return_abi_kinds: &'a rustc_hash::FxHashMap<String, crate::ir::abi::AbiKind>,
    /// Pre-computed `__Closure_N__call` signatures, indexed by the
    /// full call-function name (e.g. "__Closure_2__call"). The HOF
    /// intercept uses `ret_ty` to resolve the closure's real return
    /// type (which can diverge from the dst's declared element type
    /// for cross-typed maps) and `param_tys` to pick per-arg ABI
    /// tags that match the closure's actual signature.
    pub(super) closure_call_sigs:
        &'a std::collections::HashMap<String, ClosureCallSig>,
    /// Source span attached to instructions pushed during the current
    /// lowering step. Set per GIR instruction by the dispatch loop in
    /// `lower()`; consumed by `push_inst`. `None` for synthetic preamble
    /// instructions (param load-in, etc.) that don't correspond to user
    /// source.
    pub(super) current_span: Option<crate::span::Span>,
    /// String temps synthesized by `lower_printf_args` (e.g. the
    /// `gorget_bool_to_str` result slot for a bool printf arg). These are
    /// born at the LIR layer — below GIR drop registration — so the
    /// printf-like call emitter frees them (`gorget_string_free`) right
    /// after the consuming call. Drained per call.
    pub(super) printf_str_temps: Vec<SlotId>,
}

/// Signature snapshot for a closure's `__call` function, used to
/// resolve the closure's return type and per-arg ABI at HOF
/// emission time.
#[derive(Debug, Clone)]
pub(super) struct ClosureCallSig {
    pub(super) ret_ty: LirType,
    /// User params only (the `env` parameter at index 0 is skipped).
    pub(super) param_tys: Vec<LirType>,
}

impl<'a> LoweringContext<'a> {
    pub fn new(gir: &'a ir::Module) -> Self {
        let mut module = LirModule::new();
        module.source_filename = gir.source_filename.clone();

        // Register builtin struct types.
        let mut struct_reg = StructRegistry::new();
        for def in builtin_struct_defs() {
            let id = module.add_struct(def.clone());
            struct_reg.register(&def.name, id);
        }
        Self {
            gir,
            module,
            struct_reg,
            func_index: std::collections::HashMap::new(),
            global_index: std::collections::HashMap::new(),
        }
    }

    /// Lower the entire GIR module to LIR.
    pub fn lower(mut self) -> LirModule {
        // Pre-register all function names so Call can resolve forward references.
        for (i, func) in self.gir.functions.iter().enumerate() {
            self.func_index
                .insert(func.name.clone(), FuncId(i as u32));
        }

        // Lower struct/enum type definitions from GIR type registry.
        self.lower_type_defs();

        // Register cross-module types (Result/Option from imported functions).
        self.register_extern_types();

        // Lower extern declarations.
        // Skip externs whose names map to runtime functions (e.g., Vector__int64_t__new
        // maps to gorget_array_new which is already in the C runtime).
        for ext in &self.gir.externs {
            if map_monomorphized_to_runtime_with_table(&ext.name, &self.gir.runtime_callees).is_some() {
                continue;
            }
            let mut ret_ty = map_gir_type_with_structs(&ext.return_type, &self.gir.type_registry, Some(&self.struct_reg));
            // String-returning runtime functions return Str by value in C,
            // even though the GIR types them as Ptr(GorgetString) (resource type).
            // Override Ptr → Struct(Str) to prevent the C backend from deref'ing.
            if matches!(ret_ty, LirType::Ptr) {
                if let Some(ir::types::GirType::Ptr(inner) | ir::types::GirType::MutPtr(inner)) = self.gir.type_registry.get(ext.return_type) {
                    if let Some(ir::types::GirType::Named(name)) = self.gir.type_registry.get(*inner) {
                        if name == "GorgetString" {
                            if let Some(sid) = self.struct_reg.lookup(name) {
                                ret_ty = LirType::Struct(sid);
                            }
                        }
                    }
                }
            }
            self.module.add_extern(LirExtern {
                name: ext.name.clone(),
                params: ext.params.iter().map(|t| map_gir_type_with_structs(t, &self.gir.type_registry, Some(&self.struct_reg))).collect(),
                return_type: ret_ty,
                is_variadic: ext.is_variadic,
                param_abis: ext.param_abis.clone(),
                return_abi: Default::default(),
                combinator_result_struct_id: None,
            });
        }

        // Lower globals.
        for global in &self.gir.globals {
            let ty = map_gir_type_with_structs(&global.type_id, &self.gir.type_registry, Some(&self.struct_reg));
            let gid = self.module.add_global(LirGlobal {
                name: global.name.clone(),
                ty: ty.clone(),
                init: lower_global_init(&global.init, &self.func_index, &ty, &self.struct_reg, &self.gir.type_registry),
                is_const: false, // GIR doesn't distinguish const vs mut globals
            });
            self.global_index.insert(global.name.clone(), gid);
        }

        // Generate recursive drop function metadata BEFORE function lowering,
        // so FuncLowering can access drop_collision_types for inline drops.
        self.populate_recursive_drop_structs();

        // Pre-compute function signatures the HOF intercept needs:
        //   - `__Closure_N__call`  (closure literal): key is the
        //     `__call` function name, `param_tys` excludes the env
        //     parameter at index 0.
        //   - user function `F`   (bare FuncRef closure): key is
        //     the function's own name, `param_tys` includes every
        //     declared param.
        //
        // The intercept ABI logic treats `sig.param_tys` as the user
        // arg list — both shapes satisfy this contract.
        //
        // We walk every GIR function once. The return type matters
        // for cross-typed maps (`nums.map(int → String)` where the
        // dst's declared element type diverges from the closure's
        // return). The parameter layout matters for aggregate-element
        // HOFs (fold/filter/map/…) — the closure decides per-arg
        // whether to take the struct by value or by pointer, and
        // BIR's per-arg Load/pointer choice must match.
        let closure_call_sigs: std::collections::HashMap<String, ClosureCallSig> = self
            .gir
            .functions
            .iter()
            .map(|f| {
                let ret_ty = map_gir_type_with_structs(
                    &f.return_type,
                    &self.gir.type_registry,
                    Some(&self.struct_reg),
                );
                let skip = if f.name.starts_with("__Closure_") && f.name.ends_with("__call") {
                    1
                } else {
                    0
                };
                let param_tys: Vec<LirType> = f
                    .params
                    .iter()
                    .skip(skip)
                    .map(|t| {
                        map_gir_type_with_structs(
                            t,
                            &self.gir.type_registry,
                            Some(&self.struct_reg),
                        )
                    })
                    .collect();
                (f.name.clone(), ClosureCallSig { ret_ty, param_tys })
            })
            .collect();

        // Lower functions.
        let mut all_pending_externs: Vec<LirExtern> = Vec::new();
        let funcs: Vec<LirFunction> = self
            .gir
            .functions
            .iter()
            .map(|f| {
                let mut fl = FuncLowering::new(
                    f,
                    &self.gir.type_registry,
                    &self.struct_reg,
                    &self.func_index,
                    &self.global_index,
                    &self.module.structs,
                    &self.module.struct_aliases,
                    &self.module.globals,
                    &self.module.drop_collision_types,
                    &self.gir.runtime_callees,
                    &self.module.recursive_drop_enums,
                    &self.module.recursive_drop_structs,
                    &self.module.type_drop_fns,
                    &self.gir.fn_extern_abi_kinds,
                    &self.gir.fn_return_abis,
                    &closure_call_sigs,
                );
                fl.lower();
                all_pending_externs.extend(fl.pending_externs.drain(..));
                fl.lir_func
            })
            .collect();

        // Register synthetic externs discovered during function lowering.
        for ext in all_pending_externs {
            if let Some(existing) = self.module.externs.iter_mut().find(|e| e.name == ext.name) {
                // Replace if existing is variadic, has fewer params, or has a less specific return type.
                // For runtime functions with known signatures (from ensure_extern + RuntimeFn::resolve_lir_sig),
                // always prefer the new declaration which has the canonical types.
                let should_replace = existing.is_variadic
                    || (existing.params.is_empty() && !ext.params.is_empty())
                    || (existing.return_type != ext.return_type && ext.return_type.is_aggregate());
                if should_replace {
                    *existing = ext;
                }
            } else {
                self.module.add_extern(ext);
            }
        }

        for func in funcs {
            self.module.add_function(func);
        }

        // Post-pass: populate combinator_result_struct_id on Option/Result HOF externs.
        // The GIR ExternDecl return type is `ret_self` (the source enum) for map/map_err/and_then;
        // when the closure maps to a different element type (cross-type map), the result struct
        // differs. We resolve it once here — from the closure struct's __call return type —
        // and store the StructId so the C backend can read a typed field instead of re-deriving
        // by splitting the name at `__` boundaries.
        {
            // Methods that produce a (potentially different) result struct from the HOF.
            const CROSS_TYPE_METHODS: &[&str] = &["map", "map_err", "and_then", "filter", "flat_map"];

            // Collect (extern_idx, result_struct_id) pairs before mutating.
            let mut updates: Vec<(usize, StructId)> = Vec::new();
            for (ext_idx, ext) in self.module.externs.iter().enumerate() {
                // Extract the method suffix from the extern name (the C symbol IS
                // the contract with the runtime — name parsing is acceptable here
                // per layering-discipline.md, only at this boundary).
                let sep_pos = match ext.name.rfind("__") { Some(p) => p, None => continue };
                let method = &ext.name[sep_pos + 2..];
                if !CROSS_TYPE_METHODS.contains(&method) { continue; }

                // Source struct: read the typed `enum_kind` for routing — never
                // re-derive Option/Result from the name prefix.
                let type_prefix = &ext.name[..sep_pos];
                let src_struct = match self.module.structs.iter().find(|s| s.name == type_prefix) {
                    Some(s) => s,
                    None => continue,
                };
                let is_option = match src_struct.enum_kind {
                    EnumKind::Option => true,
                    EnumKind::Result => false,
                    _ => continue,
                };

                // Closure struct is params[1].
                let closure_ty = match ext.params.get(1) { Some(t) => t, None => continue };
                let closure_struct_name = match closure_ty {
                    LirType::Struct(sid) => self.module.structs.get(sid.0 as usize).map(|s| s.name.as_str()),
                    _ => None,
                };
                let closure_struct_name = match closure_struct_name { Some(n) => n, None => continue };

                // Look up the closure's __call return type (typed field on LirFunction).
                let call_fn_name = format!("{closure_struct_name}__call");
                let closure_ret = match self.module.functions.iter().find(|f| f.name == call_fn_name) {
                    Some(f) => f.return_type.clone(),
                    None => continue,
                };

                // Find the result struct via typed `enum_kind` + field comparisons
                // (no `__`-splitting on candidate names).
                let result_sid: Option<StructId> = match method {
                    "map" | "filter" | "flat_map" => {
                        let src_payload = src_struct.fields.get(1).map(|(_, t)| t);
                        // Same-type map: no override needed.
                        if src_payload.map_or(true, |t| t == &closure_ret) { continue; }
                        if is_option {
                            self.module.structs.iter().find(|s| {
                                s.enum_kind == EnumKind::Option
                                    && s.fields.get(1).map_or(false, |(_, t)| t == &closure_ret)
                            }).and_then(|s| self.struct_reg.lookup(&s.name))
                        } else {
                            let err_lir = src_struct.fields.get(2).map(|(_, t)| t.clone());
                            self.module.structs.iter().find(|s| {
                                s.enum_kind == EnumKind::Result
                                    && s.fields.get(1).map_or(false, |(_, t)| t == &closure_ret)
                                    && s.fields.get(2).map_or(true, |(_, t)| err_lir.as_ref().map_or(true, |e| t == e))
                            }).and_then(|s| self.struct_reg.lookup(&s.name))
                        }
                    }
                    "map_err" => {
                        let src_err = src_struct.fields.get(2).map(|(_, t)| t);
                        if src_err.map_or(true, |t| t == &closure_ret) { continue; }
                        let ok_lir = src_struct.fields.get(1).map(|(_, t)| t.clone());
                        self.module.structs.iter().find(|s| {
                            s.enum_kind == EnumKind::Result
                                && s.fields.get(1).map_or(true, |(_, t)| ok_lir.as_ref().map_or(true, |o| t == o))
                                && s.fields.get(2).map_or(false, |(_, t)| t == &closure_ret)
                        }).and_then(|s| self.struct_reg.lookup(&s.name))
                    }
                    "and_then" => {
                        // Closure returns the full wrapped type; result IS the closure return.
                        match &closure_ret {
                            LirType::Struct(sid) => Some(*sid),
                            _ => None,
                        }
                    }
                    _ => None,
                };

                if let Some(sid) = result_sid {
                    updates.push((ext_idx, sid));
                }
            }
            for (ext_idx, sid) in updates {
                self.module.externs[ext_idx].combinator_result_struct_id = Some(sid);
            }
        }

        // Populate spawned_fns metadata for the LIR→C backend to generate
        // blocking spawn/await helper functions.
        for (fn_name, params, ret_type) in &self.gir.runtime.spawned_fns {
            let ret_c = gir_type_to_c(*ret_type, &self.gir.type_registry);
            let lir_params: Vec<(String, String)> = params.iter().map(|(name, type_id)| {
                let c_ty = spawn_param_c_type(*type_id, &self.gir.type_registry);
                (name.clone(), c_ty)
            }).collect();
            // Detect params that are passed by mutable reference in the actual function.
            let actual_fn = self.gir.functions.iter().find(|f| f.name == *fn_name);
            let ref_param_indices: Vec<usize> = params.iter().enumerate().filter_map(|(i, (_, stored_type))| {
                actual_fn.and_then(|f| {
                    f.params.get(i).and_then(|&actual_type| {
                        if actual_type != *stored_type {
                            if let Some(GirType::MutPtr(inner)) = self.gir.type_registry.get(actual_type) {
                                if *inner == *stored_type { return Some(i); }
                            }
                        }
                        None
                    })
                })
            }).collect();
            // Detect refcounted params that need cloning when captured.
            let clone_params: Vec<(usize, String)> = params.iter().enumerate().filter_map(|(i, (_, type_id))| {
                if let Some(GirType::Named(name)) = self.gir.type_registry.get(*type_id) {
                    if name.starts_with("Channel__") || name.starts_with("Shared__") || name.starts_with("Weak__") {
                        return Some((i, name.clone()));
                    }
                }
                None
            }).collect();
            // Cross the ABI boundary into LIR for the spawn ENTRY extern.
            //
            // The spawn entry `__gorget_spawn_<fn>` is generated as C (see
            // `emit_types.rs::emit_async_helpers`) and, in the LLVM build, that
            // C is compiled with `static inline` stripped — so it becomes a
            // real cross-function ABI taking each param BY VALUE (`Str path`,
            // `GorgetArray v`, …). The C backend inlines the same function, so
            // it never sees the boundary; only LLVM does.
            //
            // `ensure_extern` registered this extern from the *call-site* arg
            // LIR types. For aggregates whose SSA operand is already a struct
            // value (Vector → `Struct(GorgetArray)`), that is faithful and the
            // existing large-aggregate byval path fires. But a String arg's SSA
            // operand is a `Ptr` (address of a GorgetString), so the extern got
            // `params[i] = Ptr` / `abi = Auto` and NO byval — on x86_64 SysV the
            // C entry reads the 32-byte Str off the outgoing stack (memory
            // class) while LLVM passed a register pointer → garbage / segfault.
            //
            // `spawned_fns` is the source of truth for the entry's real param
            // types (resolve-once / write-through, devbook/24). Write the
            // `GorgetString` ABI tag through onto the matching extern so the
            // declaration- and call-site byval machinery (mod.rs ~1586 / ~6009,
            // keyed on `AbiKind::GorgetString`) emits `ptr byval(%GorgetString)`
            // on x86_64 and bare `ptr` per-AAPCS64 on aarch64 — arch-safe via
            // `large_agg_byval_attr`'s existing target gate. The spawn-extern
            // name IS the registry key (built from this same `fn_name`), so the
            // lookup is a typed link, not a name heuristic.
            let entry_name = format!("__gorget_spawn_{fn_name}");
            if let Some(ext) = self.module.externs.iter_mut().find(|e| e.name == entry_name) {
                // `ensure_extern` may have left `param_abis` empty (no entry in
                // `extern_abi_kinds` for synthetic spawn externs). Grow it to
                // the param count, defaulting to `Auto`, before writing tags.
                if ext.param_abis.len() < ext.params.len() {
                    ext.param_abis.resize(ext.params.len(), crate::ir::abi::AbiKind::Auto);
                }
                for (i, (_name, c_ty)) in lir_params.iter().enumerate() {
                    if matches!(c_ty.as_str(), "GorgetString" | "Str") && i < ext.param_abis.len() {
                        ext.param_abis[i] = crate::ir::abi::AbiKind::GorgetString;
                    }
                }
            }

            self.module.spawned_fns.push(SpawnedFn {
                fn_name: fn_name.clone(),
                params: lir_params,
                ret_c_type: ret_c,
                ref_param_indices,
                clone_params,
            });
        }

        // Populate thread_spawned_fns from GIR runtime.
        for (fn_name, ret_type, ret_name, stack_size) in &self.gir.runtime.thread_spawned_fns {
            let ret_c = gir_type_to_c(*ret_type, &self.gir.type_registry);
            self.module.thread_spawned_fns.push(crate::lir::ThreadSpawnedFn {
                fn_name: fn_name.clone(),
                ret_name: ret_name.clone(),
                ret_c_type: ret_c,
                stack_size: *stack_size,
            });
        }

        // Copy test/bench metadata from GIR runtime.
        for t in &self.gir.runtime.test_fns {
            self.module.test_fns.push(crate::lir::LirTestFn {
                fn_name: t.fn_name.clone(),
                display_name: t.display_name.clone(),
                should_panic: t.should_panic,
                expected_panic_msg: t.expected_panic_msg.clone(),
                skipped: t.skipped,
                skip_reason: t.skip_reason.clone(),
                timeout_ms: t.timeout_ms,
            });
        }
        for b in &self.gir.runtime.bench_fns {
            self.module.bench_fns.push(crate::lir::LirBenchFn {
                fn_name: b.fn_name.clone(),
                display_name: b.display_name.clone(),
            });
        }
        self.module.has_suite_setup = self.gir.runtime.has_suite_setup;
        self.module.has_suite_teardown = self.gir.runtime.has_suite_teardown;
        self.module.scheduler_mode = self.gir.runtime.scheduler_mode;
        self.module.trace_filename = self.gir.runtime.trace_filename.clone();
        self.module.is_test_module = self.gir.runtime.is_test_module;
        self.module.hot_reload = self.gir.runtime.hot_reload;
        self.module.hot_reload_state_type = self.gir.runtime.hot_reload_state_type.clone();
        self.module.hot_reload_state_hash = self.gir.runtime.hot_reload_state_hash;
        self.module.hot_reload_has_reload_fn = self.gir.runtime.hot_reload_has_reload_fn;

        // Compute and cache C sizeof for all struct definitions.
        self.module.compute_struct_sizes();

        self.module
    }

    /// Populate `recursive_drop_structs` on the LirModule.
    /// For each struct with `DropStrategy::Recursive`, collect its droppable fields
    /// and the drop function to call on each. The C backend uses this to emit
    /// `static inline void {Name}__drop({Name}* self)` function bodies.
    fn populate_recursive_drop_structs(&mut self) {
        use crate::ir::types::{DropStrategy, TypeDefKind};
        let type_defs: Vec<_> = self.gir.type_registry.type_defs().iter()
            .map(|td| (td.name.clone(), td.metadata.drop_strategy.clone(), td.kind.clone()))
            .collect();
        for (name, strategy, kind) in &type_defs {
            // Collect field→drop mappings for both Recursive and Custom-drop structs.
            // Both need {Name}__clone for deep-clone support.
            if !matches!(strategy, DropStrategy::Recursive | DropStrategy::Custom(_)) {
                continue;
            }
            let sdef = match kind {
                TypeDefKind::Struct(s) => s,
                _ => continue,
            };
            let mut field_drops: Vec<(String, String, String)> = Vec::new();
            for field in &sdef.fields {
                let (drop_fn, field_type_name) = match self.gir.type_registry.get(field.type_id) {
                    Some(GirType::Named(n)) => {
                        let field_type_name = n.clone();
                        let field_drop_strategy = self.infer_drop_strategy(&field_type_name);
                        let drop_fn = match &field_drop_strategy {
                            DropStrategy::Trivial(fn_name) => fn_name.clone(),
                            DropStrategy::Custom(fn_name) => fn_name.clone(),
                            DropStrategy::Recursive => {
                                // Check if {Name}__drop exists as a NON-destructor function
                                // (e.g., DataFrame.drop() column-drop method). If so, we can't
                                // use it as the field drop function. Skip this field — the LIR
                                // handles the drop inline via lower_field_drops.
                                let candidate = format!("{field_type_name}__drop");
                                // If a function with this name exists in the GIR and is NOT the
                                // Drop trait impl, skip. Check both exact match and module-prefixed
                                // names (GIR uses `mod___Name__drop` but C emits `Name__drop`).
                                let suffix = format!("___{candidate}");
                                let is_non_destructor = self.gir.functions.iter().any(|f| {
                                    (f.name == candidate || f.name.ends_with(&suffix)) && f.params.len() > 1
                                });
                                if is_non_destructor {
                                    self.module.drop_collision_types.insert(field_type_name.clone());
                                    continue;
                                }
                                candidate
                            }
                            DropStrategy::None => continue,
                        };
                        (drop_fn, field_type_name)
                    }
                    Some(GirType::FnPtr { .. }) => {
                        // Tier 1c follow-on: `Callable[…]` fields lower to
                        // `GirType::FnPtr` at the GIR level but to a 16-byte
                        // `GorgetClosure` struct at the LIR level (owned heap
                        // env). Emit a drop entry so scope-exit frees the env
                        // via `gorget_closure_free(&self->field)`. Without
                        // this, iterator-chain structs (`MapIter`, `FilterIter`,
                        // …) marked Recursive via their inner-iter field would
                        // leak the closure env and trip Tier 1a
                        // `validate_drop_completeness`.
                        ("gorget_closure_free".to_string(), "GorgetClosure".to_string())
                    }
                    _ => continue,
                };
                field_drops.push((field.name.clone(), drop_fn, field_type_name));
            }
            if !field_drops.is_empty() {
                self.module.recursive_drop_structs.insert(name.clone(), field_drops);
            }
        }

        // Second pass: collect enum variant→clone info for tag-based clone dispatch.
        // Any enum with cloneable variant payloads gets a __clone function,
        // regardless of the enum's own drop strategy. Clone is about deep copy,
        // independent from drop semantics.
        for (name, _strategy, kind) in &type_defs {
            let edef = match kind {
                TypeDefKind::Enum(e) => e,
                _ => continue,
            };
            let mut variant_drops: Vec<(u32, String, String, String, String)> = Vec::new();
            for (vi, variant) in edef.variants.iter().enumerate() {
                for (fi, field) in variant.fields.iter().enumerate() {
                    let (drop_fn, field_type_name) = match self.gir.type_registry.get(field.type_id) {
                        Some(GirType::Named(n)) => {
                            let field_type_name = n.clone();
                            let field_drop = self.infer_drop_strategy(&field_type_name);
                            let drop_fn = match &field_drop {
                                DropStrategy::Trivial(fn_name) => fn_name.clone(),
                                DropStrategy::Custom(fn_name) => fn_name.clone(),
                                DropStrategy::Recursive => {
                                    // Use mangled destructor name for collision types
                                    // (e.g., DataFrame.drop() is a user method, not a destructor)
                                    if self.module.drop_collision_types.contains(&field_type_name) {
                                        format!("__gorget_dtor_{field_type_name}")
                                    } else {
                                        format!("{field_type_name}__drop")
                                    }
                                }
                                DropStrategy::None => continue,
                            };
                            (drop_fn, field_type_name)
                        }
                        Some(GirType::FnPtr { .. }) => {
                            // Tier 1c follow-on: see the matching branch in
                            // `populate_recursive_drop_structs` above.
                            ("gorget_closure_free".to_string(), "GorgetClosure".to_string())
                        }
                        _ => continue,
                    };
                    // LIR field name: {VariantName}_{field_index_within_variant}
                    let field_name = format!("{}_{fi}", variant.name);
                    variant_drops.push((vi as u32, variant.name.clone(), field_name, drop_fn, field_type_name));
                }
            }
            if !variant_drops.is_empty() {
                self.module.recursive_drop_enums.insert(name.clone(), variant_drops);
            }
        }

        // Third pass: populate unified type_drop_fns for all types with droppable fields.
        // This covers structs (Recursive + Custom) and enums with resource payloads.
        // Naming collision handling: if Type__drop is a user method, use __gorget_dtor_Type.
        for (name, strategy, kind) in &type_defs {
            match kind {
                TypeDefKind::Struct(sdef) => {
                    if !matches!(strategy, DropStrategy::Recursive | DropStrategy::Custom(_)) {
                        continue;
                    }
                    // Tier 1a inverse: flag the LIR StructDef before the
                    // empty-field-drops skip below so `validate_drop_fn_presence`
                    // catches a populator that silently produced no entry for
                    // a metadata-says-Recursive type (the `_ => continue`
                    // skip class for non-Named/non-FnPtr field types).
                    if let Some(sd) = self.module.struct_def_by_name_mut(name) {
                        sd.expects_drop_fn = true;
                    }
                    let mut field_drops: Vec<(String, String, String)> = Vec::new();
                    for field in &sdef.fields {
                        let (drop_fn, field_type_name) = match self.gir.type_registry.get(field.type_id) {
                            Some(GirType::Named(n)) => {
                                let field_type_name = n.clone();
                                let field_drop_strategy = self.infer_drop_strategy(&field_type_name);
                                let drop_fn = match &field_drop_strategy {
                                    DropStrategy::Trivial(fn_name) => fn_name.clone(),
                                    DropStrategy::Custom(fn_name) => fn_name.clone(),
                                    DropStrategy::Recursive => {
                                        // For collision types, use mangled destructor name
                                        if self.module.drop_collision_types.contains(&field_type_name) {
                                            format!("__gorget_dtor_{field_type_name}")
                                        } else {
                                            format!("{field_type_name}__drop")
                                        }
                                    }
                                    DropStrategy::None => continue,
                                };
                                (drop_fn, field_type_name)
                            }
                            Some(GirType::FnPtr { .. }) => {
                                // Tier 1c follow-on: see the matching branch in
                                // `populate_recursive_drop_structs` above.
                                ("gorget_closure_free".to_string(), "GorgetClosure".to_string())
                            }
                            _ => continue,
                        };
                        field_drops.push((field.name.clone(), drop_fn, field_type_name));
                    }
                    if field_drops.is_empty() && !matches!(strategy, DropStrategy::Custom(_)) {
                        continue;
                    }
                    let user_drop_fn = if let DropStrategy::Custom(fn_name) = strategy {
                        Some(fn_name.clone())
                    } else {
                        None
                    };
                    // Determine drop function name. Custom-drop types need a mangled
                    // name because Type__drop is the USER's function.
                    let drop_fn_name = if user_drop_fn.is_some()
                        || self.module.drop_collision_types.contains(name)
                    {
                        format!("__gorget_dtor_{name}")
                    } else {
                        format!("{name}__drop")
                    };
                    self.module.type_drop_fns.insert(name.clone(), crate::lir::TypeDropInfo {
                        drop_fn_name,
                        field_drops,
                        user_drop_fn,
                        enum_variants: None,
                    });
                }
                TypeDefKind::Enum(edef) => {
                    // Tier 1a inverse: flag the LIR StructDef when the GIR
                    // enum has Recursive/Custom drop strategy, before the
                    // empty-variant-drops skip below. Trivial-drop enums
                    // (no resource payloads) intentionally stay unflagged.
                    if matches!(strategy, DropStrategy::Recursive | DropStrategy::Custom(_)) {
                        if let Some(sd) = self.module.struct_def_by_name_mut(name) {
                            sd.expects_drop_fn = true;
                        }
                    }
                    let mut variant_drops: Vec<(u32, String, String, String, String)> = Vec::new();
                    for (vi, variant) in edef.variants.iter().enumerate() {
                        for (fi, field) in variant.fields.iter().enumerate() {
                            let (drop_fn, field_type_name) = match self.gir.type_registry.get(field.type_id) {
                                Some(GirType::Named(n)) => {
                                    let field_type_name = n.clone();
                                    let field_drop = self.infer_drop_strategy(&field_type_name);
                                    let drop_fn = match &field_drop {
                                        DropStrategy::Trivial(fn_name) => fn_name.clone(),
                                        DropStrategy::Custom(fn_name) => fn_name.clone(),
                                        DropStrategy::Recursive => {
                                            if self.module.drop_collision_types.contains(&field_type_name) {
                                                format!("__gorget_dtor_{field_type_name}")
                                            } else {
                                                format!("{field_type_name}__drop")
                                            }
                                        }
                                        DropStrategy::None => continue,
                                    };
                                    (drop_fn, field_type_name)
                                }
                                Some(GirType::FnPtr { .. }) => {
                                    // Tier 1c follow-on: see the matching
                                    // branch in `populate_recursive_drop_structs`.
                                    ("gorget_closure_free".to_string(), "GorgetClosure".to_string())
                                }
                                _ => continue,
                            };
                            let field_name = format!("{}_{fi}", variant.name);
                            variant_drops.push((vi as u32, variant.name.clone(), field_name, drop_fn, field_type_name));
                        }
                    }
                    // Mirror the Struct branch above: a Custom-drop enum must
                    // record `user_drop_fn: Some(..)` + a mangled
                    // `__gorget_dtor_{name}` drop fn name. Without this, the
                    // recorded `{name}__drop` collides with the USER's drop
                    // function — and the two emitters disagree:
                    //   * a standalone Custom-drop enum drops via the user fn
                    //     (`{name}__drop`), so the variant payloads LEAK (the
                    //     variant-drop emitter skips a name the user already
                    //     defined);
                    //   * a Custom-drop enum nested as a struct FIELD drops via
                    //     the variant-drop `{name}__drop` (the user fn is DCE'd),
                    //     so the user `drop` body never runs.
                    // Routing both through `__gorget_dtor_{name}` (user body THEN
                    // variant drops, emitted by `emit_type_drop_fns`) fixes both,
                    // exactly as the Struct branch does for Custom-drop structs.
                    // A Custom-drop enum with NO resource payloads (empty
                    // `variant_drops`) is still recorded so its user body runs.
                    let user_drop_fn = if let DropStrategy::Custom(fn_name) = strategy {
                        Some(fn_name.clone())
                    } else {
                        None
                    };
                    if variant_drops.is_empty() && user_drop_fn.is_none() {
                        continue;
                    }
                    let drop_fn_name = if user_drop_fn.is_some()
                        || self.module.drop_collision_types.contains(name)
                    {
                        format!("__gorget_dtor_{name}")
                    } else {
                        format!("{name}__drop")
                    };
                    self.module.type_drop_fns.insert(name.clone(), crate::lir::TypeDropInfo {
                        drop_fn_name,
                        field_drops: Vec::new(),
                        user_drop_fn,
                        enum_variants: Some(variant_drops),
                    });
                }
                _ => {} // Alias types — skip
            }
        }
    }

    /// Infer the drop strategy for a type from its TypeDef metadata, with a
    /// Box-specific override.
    ///
    /// All collection types and runtime singletons carry `drop_strategy` set
    /// at registration via BuiltinTypeProtocol. Box[T] needs special routing:
    /// its registered TypeDef has `DropStrategy::Trivial("free")` (historical
    /// convention for the bare `free()` runtime call), but recursive-drop
    /// emitters need to call the per-type `Box__T__drop` wrapper so inner T's
    /// resources get freed and the tracked `__gorget_box_free_T` accounts for
    /// the dealloc. We detect the Box shape FIRST and route to the wrapper —
    /// except for trait-object boxes (Box__Trait where `{Trait}_TraitObj` is
    /// registered), which use a 16-byte `{data, vtable}` layout and are
    /// handled inline by drops.rs.
    fn infer_drop_strategy(&self, type_name: &str) -> crate::ir::types::DropStrategy {
        use crate::ir::types::DropStrategy;
        if let Some(inner) = type_name.strip_prefix("Box__") {
            let trait_obj = format!("{inner}_TraitObj");
            let is_trait_box = self.gir.type_registry.get_type_def(&trait_obj).is_some();
            if !is_trait_box {
                return DropStrategy::Trivial(format!("Box__{inner}__drop"));
            }
            // Trait box: keep the legacy "free" marker; emit_recursive_struct_drops
            // and emit_enum_drop_fns will route through the existing inline-handling
            // code. (TODO: full trait-box-as-field/variant support.)
            return DropStrategy::Trivial("free".to_string());
        }
        if let Some(td) = self.gir.type_registry.get_type_def(type_name) {
            return td.metadata.drop_strategy.clone();
        }
        DropStrategy::None
    }

    fn lower_type_defs(&mut self) {
        // Two-pass registration to handle forward references (e.g., an enum
        // whose variant payloads reference structs defined later in the file).

        // Pre-register Task__<T> LIR structs for every Named GIR type matching
        // the pattern. spawn() lowering registers these as `GirType::Named` but
        // many call sites omit the matching `add_type_def`, so they never reach
        // the iteration below. The runtime ABI is a fixed 16-byte
        // `(__task: ptr, __drop: ptr)` pair; pinning it here makes
        // `map_gir_type` of spawn dst values resolve to a 16-byte `Struct(sid)`
        // instead of falling through to `LirType::Ptr` — fixing the LLVM crash
        // where the spawn extern declared an 8-byte ptr return and downstream
        // task_group_submit_raw read .__drop from `(spawn_ret_ptr + 8)`,
        // off the end of the .__task referent.
        let task_names: std::collections::HashSet<String> = (0..self.gir.type_registry.len())
            .filter_map(|i| {
                let id = crate::ir::types::TypeId(i as u32);
                match self.gir.type_registry.get(id) {
                    Some(crate::ir::types::GirType::Named(name)) if name.starts_with("Task__") => Some(name.clone()),
                    _ => None,
                }
            })
            .collect();
        for task_name in task_names {
            if self.struct_reg.lookup(&task_name).is_some() {
                continue;
            }
            let sid = self.module.add_struct(StructDef {
                name: task_name.clone(),
                fields: vec![
                    ("__task".into(), LirType::Ptr),
                    ("__drop".into(), LirType::Ptr),
                ],
                enum_kind: EnumKind::NotEnum,
                is_union_layout: false,
                computed_c_size: Some(16),
                computed_c_align: Some(8), elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
            });
            self.struct_reg.register(&task_name, sid);
        }

        // Pass 1: Pre-register all type names with empty placeholder structs.
        let mut deferred: Vec<(StructId, usize)> = Vec::new();
        for (idx, def) in self.gir.type_registry.type_defs().iter().enumerate() {
            if self.struct_reg.lookup(&def.name).is_some() {
                continue; // Already registered (builtin).
            }

            // Map collection instantiations to their runtime struct.
            // e.g., Vector__Str → GorgetArray, Dict__Str__int64_t → GorgetMap.
            //
            // Two cases distinguished by whether the GIR TypeDef carries a
            // `c_runtime_alias`:
            //   1. Plain collection alias (Vector/Dict/Set/HashSet/HashMap):
            //      no `c_runtime_alias`. Register name→sid only; no separate
            //      StructDef. The C backend never sees the alias name as a
            //      type — runtime methods reroute via mangling.
            //   2. `c_runtime_alias`-tagged (Callable family): the C backend
            //      emits the alias name as a real C type (because user code
            //      can have `Shared[Callable[…]]` whose mangled wrapper
            //      references the alias name directly in
            //      `Shared__Callable__GorgetClosure__new(Callable__GorgetClosure)`).
            //      Skip the shortcut and fall through to the
            //      c_runtime_alias-tagged synthesis below at line 814 so a
            //      proper StructDef gets registered with `c_runtime_alias`
            //      set; that triggers the typedef emission in c_lir's
            //      struct loop. Phase A residual #2 (sub-TODO 1b extension):
            //      this gap surfaced when shared_callable.gg's
            //      `Shared[Callable[int()]]` shape didn't emit
            //      `typedef GorgetClosure Callable__GorgetClosure;`.
            let has_c_runtime_alias = self.gir.type_registry
                .get_type_def(&def.name)
                .and_then(|td| td.metadata.c_runtime_alias.as_ref())
                .is_some();
            if !has_c_runtime_alias {
                if let Some(runtime_name) = collection_runtime_type(&def.name, &self.gir.type_registry) {
                    if let Some(runtime_sid) = self.struct_reg.lookup(runtime_name) {
                        self.struct_reg.register(&def.name, runtime_sid);
                        // Phase A residual #2: record the alias on the LirModule
                        // so c_lir's `struct_def_by_name` can resolve the mangled
                        // collection name (`Vector__int64_t`, `Dict__K__V`, …) to
                        // the runtime StructDef without re-deriving the mapping
                        // from name prefixes. The runtime singleton's StructDef
                        // already carries `elem_drop_fn` / `elem_clone_fn` /
                        // `materialize_fn`, so the consumer reads typed metadata.
                        self.module.struct_aliases.insert(def.name.clone(), runtime_sid);
                        continue;
                    }
                }
            }

            // Opaque-pointer concurrency types (Mutex, Shared, Channel, RWLock)
            // are typedef'd as pointers in C — skip struct creation so LIR uses Ptr.
            if is_opaque_pointer_type(&def.name) {
                continue;
            }

            // Regular Box[T] is a heap pointer wrapper with a single `_0` field.
            // Hardcode _0: Ptr to break recursive type cycles (e.g., Expr →
            // Box__SpannedExpr → SpannedExpr → Expr). Trait boxes (Box[dyn Trait])
            // get the TraitObj layout (data + vtable, 16 bytes) — detected by
            // the matching `{Inner}_TraitObj` in the GIR registry. The trait
            // path runs FIRST and replaces any synthesized `_0` placeholder
            // from `monomorphize_struct` (which can't see trait registration
            // because monomorphization runs before trait collection).
            //
            // This name-prefix scan is preserved (rather than migrated to
            // `def.metadata.is_box`) because it's the LIR discovery loop —
            // it must succeed for every Box TypeDef, including any new
            // registration path that hasn't been audited to set `is_box`
            // yet. The downstream `box_inner_type` field on the LIR struct
            // is what carries the typed Box-ness past this point.
            if def.name.starts_with("Box__") {
                let inner_name = &def.name["Box__".len()..];
                let is_trait_box = self.gir.type_registry
                    .get_type_def(&format!("{inner_name}_TraitObj"))
                    .is_some();
                if is_trait_box {
                    // Trait box: register with the canonical trait-obj layout
                    // (data + vtable, 16 bytes) directly. We don't alias to the
                    // matching `{Inner}_TraitObj` because iteration order isn't
                    // guaranteed to register the trait-obj struct first; pinning
                    // the layout here keeps every Box[Trait] sized correctly even
                    // before the trait obj struct itself shows up. The C backend
                    // re-typedefs `Box__Inner` to `__gg_Inner_TraitObj` later
                    // (`emit_monomorphized_typedefs`), so layout-equality is
                    // preserved at the C call site.
                    let sid = self.module.add_struct(StructDef {
                        name: def.name.clone(),
                        fields: vec![
                            ("data".into(), LirType::Ptr),
                            ("vtable".into(), LirType::Ptr),
                        ],
                        enum_kind: EnumKind::NotEnum,
                        is_union_layout: false,
                        computed_c_size: Some(16),
                        computed_c_align: Some(8), elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None, box_inner_type: None, is_trait_box: true, expects_drop_fn: false,
                    });
                    self.struct_reg.register(&def.name, sid);
                    continue;
                }
                let is_regular_box = match &def.kind {
                    gir_types::TypeDefKind::Struct(sdef) => {
                        sdef.fields.len() == 1 && sdef.fields[0].name == "_0"
                    }
                    _ => false,
                };
                if is_regular_box {
                    let sid = self.module.add_struct(StructDef {
                        name: def.name.clone(),
                        fields: vec![("_0".into(), LirType::Ptr)],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None,
            // Typed Box-inner metadata: the C backend reads this to emit
            // `__gorget_box_alloc_<inner>` / `_free_<inner>` and the per-type
            // `Box__<inner>__drop` wrapper without name-prefix matching.
            box_inner_type: Some(inner_name.to_string()), is_trait_box: false, expects_drop_fn: false,
                                  });
                    self.struct_reg.register(&def.name, sid);
                    continue;
                }
            }

            // Guard struct types need a fixed layout: { ptr owner; ptr data; }
            if is_guard_struct_type(&def.name) {
                let fields = vec![
                    ("owner".into(), LirType::Ptr),
                    ("ptr".into(), LirType::Ptr),
                ];
                let sid = self.module.add_struct(StructDef {
                    name: def.name.clone(),
                    fields,
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
                              });
                self.struct_reg.register(&def.name, sid);
                continue;
            }

            // Task__<T> handled above by the up-front Named-type scan; if a
            // matching TypeDef also exists, it'd already have a struct
            // registered (the lookup at the top of this loop catches it).
            if def.name.starts_with("Task__") {
                continue;
            }

            match &def.kind {
                gir_types::TypeDefKind::Struct(_) | gir_types::TypeDefKind::Enum(_) => {
                    // Phase A residual #1: when a Named type carries
                    // `c_runtime_alias = Some(rt)` (e.g. `Callable__T_args` →
                    // "GorgetClosure"), the runtime struct already provides
                    // the layout. Synthesize a StructDef whose fields mirror
                    // the runtime struct (same size + same field-load offsets)
                    // and tag it with `c_runtime_alias` so the C backend
                    // emits `typedef <rt> <name>;` instead of a fresh struct
                    // definition. The fields are populated below from the
                    // runtime struct's LIR StructDef so SlotLoad / FieldStore
                    // through this Named TypeId still resolves to the right
                    // offsets at LIR-emit time.
                    if let Some(ref rt) = def.metadata.c_runtime_alias {
                        if let Some(rt_sid) = self.struct_reg.lookup(rt) {
                            let rt_def = self.module.struct_def(rt_sid).clone();
                            let sid = self.module.add_struct(StructDef {
                                name: def.name.clone(),
                                fields: rt_def.fields.clone(),
                                enum_kind: rt_def.enum_kind,
                                is_union_layout: rt_def.is_union_layout,
                                computed_c_size: rt_def.computed_c_size,
                                computed_c_align: rt_def.computed_c_align,
                                elem_drop_fn: rt_def.elem_drop_fn.clone(),
                                elem_clone_fn: rt_def.elem_clone_fn.clone(),
                                materialize_fn: rt_def.materialize_fn.clone(),
                                c_runtime_alias: Some(rt.clone()),
                                box_inner_type: rt_def.box_inner_type.clone(),
                                is_trait_box: false, expects_drop_fn: false,
                            });
                            self.struct_reg.register(&def.name, sid);
                            // Skip deferred Pass-2: the GIR TypeDef has no
                            // user-visible fields (Callable__T is opaque to
                            // user code), so leaving the runtime fields in
                            // place is correct.
                            continue;
                        }
                    }
                    let sid = self.module.add_struct(StructDef {
                        name: def.name.clone(),
                        fields: vec![],
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None,
            elem_drop_fn: match &def.metadata.drop_strategy {
                crate::ir::types::DropStrategy::Trivial(f) => Some(f.clone()),
                _ => None,
            },
            elem_clone_fn: def.metadata.clone_inplace_fn.clone(),
            materialize_fn: def.metadata.materialize_fn.clone(),
            c_runtime_alias: def.metadata.c_runtime_alias.clone(),
            box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
                                  });
                    self.struct_reg.register(&def.name, sid);
                    deferred.push((sid, idx));
                }
                gir_types::TypeDefKind::Alias(_) => {
                    // Type aliases are transparent — no LIR struct needed.
                }
            }
        }

        // Pass 2: Fill in actual fields now that all type names are resolvable.
        for (sid, idx) in deferred {
            let def = &self.gir.type_registry.type_defs()[idx];
            let fields = match &def.kind {
                gir_types::TypeDefKind::Struct(sdef) => {
                    sdef.fields
                        .iter()
                        .map(|f| {
                            (
                                f.name.clone(),
                                map_gir_type_with_structs(&f.type_id, &self.gir.type_registry, Some(&self.struct_reg)),
                            )
                        })
                        .collect()
                }
                gir_types::TypeDefKind::Enum(edef) => {
                    let mut fields: Vec<(String, LirType)> = vec![("tag".into(), LirType::I32)];
                    for variant in &edef.variants {
                        for (i, f) in variant.fields.iter().enumerate() {
                            fields.push((
                                format!("{}_{}", variant.name, i),
                                map_gir_type_with_structs(&f.type_id, &self.gir.type_registry, Some(&self.struct_reg)),
                            ));
                        }
                    }
                    fields
                }
                _ => vec![],
            };
            // Only use union layout for large enums (> 4 fields). Small enums like
            // Option (2 fields: tag + Some_0) and Result (3 fields: tag + Ok_0 + Error_0)
            // use flat layout because the C backend accesses their fields directly
            // in many places (__option_unwrap, collection wrapping, etc.).
            let is_large_enum = matches!(&def.kind, gir_types::TypeDefKind::Enum(_))
                && fields.len() > 4;
            self.module.structs[sid.0 as usize].fields = fields;
            // Read the typed `enum_category` from GIR TypeMetadata — set at GIR
            // type registration (the source of truth) — instead of pattern-matching
            // the type name. Preserves the legacy "EnumKind::General for everything
            // else" behavior bit-for-bit; downstream readers only special-case
            // Option/Result, treating General/NotEnum identically.
            self.module.structs[sid.0 as usize].enum_kind = match def.metadata.enum_category {
                Some(gir_types::EnumCategory::Option) => EnumKind::Option,
                Some(gir_types::EnumCategory::Result) => EnumKind::Result,
                None => EnumKind::General,
            };
            self.module.structs[sid.0 as usize].is_union_layout = is_large_enum;
        }
    }

    /// Register cross-module types that appear in extern declarations or function
    /// locals but don't have TypeDefs in the current module's type registry.
    ///
    /// This handles Result__X__Y and Option__X types from imported functions
    /// (e.g., `parse_float` returns `Result__double__Str`) that would otherwise
    /// fall back to LirType::Ptr because the struct registry is only populated
    /// from the current module's type defs.
    fn register_extern_types(&mut self) {
        // Collect all Named types referenced by externs and functions.
        let mut needed: Vec<String> = Vec::new();

        // Scan extern declarations.
        for ext in &self.gir.externs {
            self.collect_named_types(&ext.return_type, &mut needed);
            for param in &ext.params {
                self.collect_named_types(param, &mut needed);
            }
        }

        // Scan function signatures and locals.
        for func in &self.gir.functions {
            self.collect_named_types(&func.return_type, &mut needed);
            for param in &func.params {
                self.collect_named_types(param, &mut needed);
            }
            for local in &func.locals {
                self.collect_named_types(&local.type_id, &mut needed);
            }
        }

        // Scan globals.
        for global in &self.gir.globals {
            self.collect_named_types(&global.type_id, &mut needed);
        }

        // Deduplicate.
        needed.sort();
        needed.dedup();

        // Register types not yet in struct_reg.
        for name in &needed {
            if self.struct_reg.lookup(name).is_some() {
                continue;
            }
            // Skip collections and opaques — handled by existing logic.
            if collection_runtime_type(name, &self.gir.type_registry).is_some() {
                continue;
            }
            if is_opaque_pointer_type(name) {
                continue;
            }

            // Try to find a TypeDef in the registry (may exist for cross-module types).
            if let Some(def) = self.gir.type_registry.get_type_def(name) {
                match &def.kind {
                    gir_types::TypeDefKind::Struct(sdef) => {
                        let fields: Vec<(String, LirType)> = sdef
                            .fields
                            .iter()
                            .map(|f| {
                                (
                                    f.name.clone(),
                                    map_gir_type_with_structs(
                                        &f.type_id,
                                        &self.gir.type_registry,
                                        Some(&self.struct_reg),
                                    ),
                                )
                            })
                            .collect();
                        let sid = self.module.add_struct(StructDef {
                            name: name.clone(),
                            fields,
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
                                      });
                        self.struct_reg.register(name, sid);
                    }
                    gir_types::TypeDefKind::Enum(edef) => {
                        let mut fields: Vec<(String, LirType)> =
                            vec![("tag".into(), LirType::I32)];
                        for variant in &edef.variants {
                            for (i, f) in variant.fields.iter().enumerate() {
                                fields.push((
                                    format!("{}_{}", variant.name, i),
                                    map_gir_type_with_structs(
                                        &f.type_id,
                                        &self.gir.type_registry,
                                        Some(&self.struct_reg),
                                    ),
                                ));
                            }
                        }
                        let sid = self.module.add_struct(StructDef {
                            name: name.clone(),
                            fields,
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
                                      });
                        self.struct_reg.register(name, sid);
                    }
                    gir_types::TypeDefKind::Alias(_) => {}
                }
                continue;
            }

            // No TypeDef exists — synthesize from the name pattern.
            if let Some(fields) = synthesize_struct_fields(name, &self.struct_reg, &self.gir.type_registry) {
                let sid = self.module.add_struct(StructDef {
                    name: name.clone(),
                    fields,
            enum_kind: EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: None, computed_c_align: None, elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
                              });
                self.struct_reg.register(name, sid);
            }
        }
    }

    /// Collect all Named type names referenced by a GIR TypeId.
    fn collect_named_types(&self, type_id: &GirTypeId, out: &mut Vec<String>) {
        if let Some(gir_type) = self.gir.type_registry.get(*type_id) {
            match gir_type {
                GirType::Named(name) => {
                    out.push(name.clone());
                }
                GirType::Ptr(inner) | GirType::MutPtr(inner) => {
                    self.collect_named_types(inner, out);
                }
                GirType::FnPtr { params, return_type } => {
                    for p in params {
                        self.collect_named_types(p, out);
                    }
                    self.collect_named_types(return_type, out);
                }
                _ => {}
            }
        }
    }
}

/// Synthesize struct fields for a cross-module Result/Option type from its
/// monomorphized name. Returns `None` if the name doesn't match a known pattern.
///
/// Examples:
/// - `Result__double__Str` → `{ tag: I32, Ok_0: F64, Error_0: Struct(Str) }`
/// - `Option__int64_t` → `{ tag: I32, Some_0: I64 }`
pub(super) fn synthesize_struct_fields(
    name: &str,
    struct_reg: &StructRegistry,
    gir_types: &crate::ir::types::TypeRegistry,
) -> Option<Vec<(String, LirType)>> {
    if let Some(rest) = name.strip_prefix("Result__") {
        // Result__X__Y — split into Ok type (X) and Error type (Y).
        // Handle compound inner types (e.g., Result__Vector__int64_t__Str)
        // by finding the split point.
        let (ok_name, err_name) = split_result_components(rest)?;
        let ok_ty = component_to_lir_type(ok_name, struct_reg, gir_types);
        let err_ty = component_to_lir_type(err_name, struct_reg, gir_types);
        return Some(vec![
            ("tag".into(), LirType::I32),
            ("Ok_0".into(), ok_ty),
            ("Error_0".into(), err_ty),
        ]);
    }
    if let Some(inner) = name.strip_prefix("Option__") {
        let some_ty = component_to_lir_type(inner, struct_reg, gir_types);
        return Some(vec![
            ("tag".into(), LirType::I32),
            ("Some_0".into(), some_ty),
        ]);
    }
    None
}

/// Split a Result's inner components: "double__GorgetString" → ("double", "GorgetString").
/// Handles compound types like "Vector__int64_t__GorgetString" → ("Vector__int64_t", "GorgetString").
pub(super) fn split_result_components(rest: &str) -> Option<(&str, &str)> {
    // The error type is typically the last `__`-separated component.
    // But compound types like `Vector__int64_t` have internal `__` separators.
    // Strategy: try splitting from the right. Known simple error types: Str, int64_t, etc.
    // For compound ok-types, the rightmost `__` gives the error type.
    let pos = rest.rfind("__")?;
    let ok = &rest[..pos];
    let err = &rest[pos + 2..];
    if ok.is_empty() || err.is_empty() {
        return None;
    }
    Some((ok, err))
}

/// Map a monomorphized type name component to an LirType.
pub(super) fn component_to_lir_type(
    name: &str,
    struct_reg: &StructRegistry,
    gir_types: &crate::ir::types::TypeRegistry,
) -> LirType {
    match name {
        "bool" => LirType::Bool,
        "int8_t" => LirType::I8,
        "int16_t" => LirType::I16,
        "int32_t" => LirType::I32,
        "int64_t" => LirType::I64,
        "uint8_t" => LirType::U8,
        "uint16_t" => LirType::U16,
        "uint32_t" => LirType::U32,
        "uint64_t" => LirType::U64,
        "float" | "double" => LirType::F64,
        "float32" => LirType::F32,
        "void" => LirType::Void,
        _ => {
            // Try struct registry lookup.
            if let Some(sid) = struct_reg.lookup(name) {
                return LirType::Struct(sid);
            }
            // Collection types.
            if let Some(runtime_name) = collection_runtime_type(name, gir_types) {
                if let Some(sid) = struct_reg.lookup(runtime_name) {
                    return LirType::Struct(sid);
                }
            }
            // Fallback: pointer (opaque or unknown).
            LirType::Ptr
        }
    }
}

/// Map a generic collection type to its runtime struct name. Reads
/// `metadata.collection_kind` set during Phase A type registration,
/// falling back to a Callable* name fallback (Callable types still
/// don't have TypeDef registration today). Returns None for non-
/// collection types and for Result/Option (real enum types — no alias).
pub(super) fn collection_runtime_type(
    name: &str,
    gir_types: &crate::ir::types::TypeRegistry,
) -> Option<&'static str> {
    use crate::ir::types::CollectionKind;
    if let Some(td) = gir_types.get_type_def(name) {
        // Phase A residual #1: `c_runtime_alias` covers the Callable family
        // (and any future Named-types-that-are-runtime-typedef-aliases).
        // Reads precede `collection_kind` so a type with both falls back to
        // the alias spelling — currently no such type exists; the field is
        // mutually exclusive in practice.
        if let Some(ref rt) = td.metadata.c_runtime_alias {
            return match rt.as_str() {
                "GorgetClosure" => Some("GorgetClosure"),
                "GorgetArray"   => Some("GorgetArray"),
                "GorgetMap"     => Some("GorgetMap"),
                "GorgetSet"     => Some("GorgetSet"),
                "GorgetString"  => Some("GorgetString"),
                _               => None, // unknown runtime alias; refuse to invent
            };
        }
        return td.metadata.collection_kind.map(|kind| match kind {
            CollectionKind::Array => "GorgetArray",
            CollectionKind::Map | CollectionKind::OrderedMap => "GorgetMap",
            CollectionKind::Set | CollectionKind::OrderedSet => "GorgetSet",
        });
    }
    None
}

/// Maps Gorget type names to their C runtime struct names for opaque types.
pub(super) fn opaque_runtime_type_name(name: &str) -> Option<&'static str> {
    match name {
        "Socket" => Some("GorgetSocket"),
        "ServerSocket" => Some("GorgetServerSocket"),
        "TlsSocket" => Some("GorgetTlsSocket"),
        "TlsServerSocket" => Some("GorgetTlsServerSocket"),
        "UdpSocket" => Some("GorgetUdpSocket"),
        "UdpAddr" => Some("GorgetUdpAddr"),
        "Semaphore" => Some("GorgetSemaphore"),
        "WaitGroup" => Some("GorgetWaitGroup"),
        "OnceFlag" => Some("GorgetOnceFlag"),
        "Barrier" => Some("GorgetBarrier"),
        "CondVar" => Some("GorgetCondVar"),
        "AtomicInt" => Some("GorgetAtomicInt"),
        "AtomicBool" => Some("GorgetAtomicBool"),
        "Process" => Some("GorgetProcess"),
        _ => None,
    }
}

/// Returns true if the type name is an opaque-pointer concurrency type
/// (Mutex, Shared, Channel, RWLock). These are pointer wrappers at
/// runtime — they should NOT be lowered as structs, but rather skipped
/// so `map_gir_type_with_structs` falls through to `Ptr`.
pub(super) fn is_opaque_pointer_type(name: &str) -> bool {
    name.starts_with("Mutex__")
        || name.starts_with("Shared__")
        || name.starts_with("Weak__")
        || name.starts_with("Channel__")
        || name.starts_with("RWLock__")
        || name.starts_with("Thread__")
        || matches!(
            name,
            "Barrier" | "CondVar" | "AtomicInt" | "AtomicBool"
            | "Process" | "Thread"
            | "Semaphore" | "WaitGroup" | "OnceFlag"
        )
}

/// Returns true if the type name is a guard struct that needs a fixed
/// layout: `{ void* owner; void* ptr; }` (two pointers).
pub(super) fn is_guard_struct_type(name: &str) -> bool {
    name.starts_with("Guard__")
        || name.starts_with("ReadGuard__")
        || name.starts_with("WriteGuard__")
}

impl<'a> FuncLowering<'a> {
    pub(super) fn new(
        gir_func: &'a ir::Function,
        gir_types: &'a TypeRegistry,
        struct_reg: &'a StructRegistry,
        func_index: &'a std::collections::HashMap<String, FuncId>,
        global_index: &'a std::collections::HashMap<String, GlobalId>,
        module_structs: &'a [StructDef],
        module_struct_aliases: &'a std::collections::HashMap<String, StructId>,
        module_globals: &'a [LirGlobal],
        drop_collision_types: &'a std::collections::HashSet<String>,
        runtime_callees: &'a rustc_hash::FxHashMap<String, crate::ir::RuntimeCalleeInfo>,
        recursive_drop_enums: &'a std::collections::HashMap<String, Vec<(u32, String, String, String, String)>>,
        recursive_drop_structs: &'a std::collections::HashMap<String, Vec<(String, String, String)>>,
        type_drop_fns: &'a std::collections::HashMap<String, crate::lir::TypeDropInfo>,
        extern_abi_kinds: &'a rustc_hash::FxHashMap<String, Vec<crate::ir::abi::AbiKind>>,
        return_abi_kinds: &'a rustc_hash::FxHashMap<String, crate::ir::abi::AbiKind>,
        closure_call_sigs: &'a std::collections::HashMap<String, ClosureCallSig>,
    ) -> Self {
        let params: Vec<LirType> = gir_func
            .params
            .iter()
            .map(|t| map_gir_type_with_structs(t, gir_types, Some(struct_reg)))
            .collect();
        let return_type = map_gir_type_with_structs(&gir_func.return_type, gir_types, Some(struct_reg));
        let mut lir_func = LirFunction::new(gir_func.name.clone(), params, return_type);
        lir_func.is_test_fn = gir_func.is_test_fn;
        // Cross-frame fault (Inc-2.1a): propagate the synthesized trailing
        // fault-slot count so the first-class adapter generation passes NULL for
        // it instead of forwarding a phantom arg through the callable ABI.
        lir_func.fault_slot_param_count = if gir_func.participates_in_fault { 1 } else { 0 };
        lir_func.display_name = gir_func.display_name.clone();
        // Propagate param name hints (GIR locals[1..N] are the params).
        lir_func.param_names = (0..gir_func.params.len())
            .map(|i| gir_func.locals.get(i + 1).and_then(|l| l.name_hint.clone()))
            .collect();
        // Mark params that came from GirType::Ptr (bare borrow = const) vs MutPtr (& or !).
        lir_func.const_params = gir_func
            .params
            .iter()
            .map(|t| matches!(gir_types.get(*t), Some(GirType::Ptr(_))))
            .collect();

        // Create LIR slots for each GIR local.
        let local_to_slot: Vec<SlotId> = gir_func
            .locals
            .iter()
            .map(|local| {
                let ty = map_gir_type_with_structs(&local.type_id, gir_types, Some(struct_reg));
                lir_func.add_slot(ty, local.name_hint.clone())
            })
            .collect();

        // Pre-create LIR blocks for each GIR block.
        let block_map: Vec<BlockId> = (0..gir_func.blocks.len())
            .map(|_| lir_func.add_block())
            .collect();

        Self {
            gir_func,
            gir_types,
            lir_func,
            local_to_slot,
            block_map,
            struct_reg,
            func_index,
            global_index,
            module_structs,
            module_struct_aliases,
            module_globals,
            pending_externs: Vec::new(),
            drop_collision_types,
            runtime_callees,
            recursive_drop_enums,
            recursive_drop_structs,
            type_drop_fns,
            extern_abi_kinds,
            return_abi_kinds,
            closure_call_sigs,
            current_span: None,
            printf_str_temps: Vec::new(),
        }
    }

    /// Push an instruction into block `bb`, tagging it with the active
    /// `current_span` (set per-GIR-instruction by the dispatch loop).
    /// Maintains `span_map.len() == insts.len()` per-block invariant.
    pub(super) fn push_inst(&mut self, bb: BlockId, inst: Inst) {
        let span = self.current_span;
        self.lir_func.block_mut(bb).push_inst(inst, span);
    }

    /// Set a block's terminator together with its source span (the active
    /// `current_span`). Use for intra-instruction CFG splits emitted while
    /// lowering a single GIR instruction; the dispatch-loop entry/exit
    /// terminator path in `lower()` writes `terminator_span` directly from
    /// the GIR block's own `terminator_span`.
    pub(super) fn set_terminator(&mut self, bb: BlockId, term: Term) {
        let span = self.current_span;
        let block = self.lir_func.block_mut(bb);
        block.terminator = term;
        block.terminator_span = span;
    }

    pub(super) fn lower(&mut self) {
        // Copy function parameters into their corresponding local slots.
        // GIR convention: locals 1..=N correspond to the N function parameters.
        let num_params = self.gir_func.params.len();
        if num_params > 0 && !self.block_map.is_empty() {
            let entry_bb = self.block_map[0];
            for param_idx in 0..num_params {
                let local_id = ir::types::LocalId((param_idx + 1) as u32);
                if (local_id.0 as usize) < self.local_to_slot.len() {
                    let param_val = self.lir_func.next_value();
                    let slot = self.local_to_slot[local_id.0 as usize];
                    let slot_ty = self.lir_func.slots[slot.0 as usize].ty.clone();
                    // Mark Ptr(Str) params for C backend deref decisions.
                    let gir_type = self.gir_func.locals[local_id.0 as usize].type_id;
                    if slot_ty.is_ptr() {
                        if let Some(ir::types::GirType::Ptr(inner) | ir::types::GirType::MutPtr(inner)) = self.gir_types.get(gir_type) {
                            if let Some(ir::types::GirType::Named(name)) = self.gir_types.get(*inner) {
                                if name == "GorgetString" {
                                    self.lir_func.str_ptr_values.insert(param_val);
                                }
                            }
                        }
                    }
                    self.push_inst(entry_bb, Inst::ParamRef {
                        dst: param_val,
                        index: param_idx as u32,
                        ty: slot_ty,
                    });
                    self.push_inst(entry_bb, Inst::SlotStore {
                        slot,
                        value: param_val,
                        is_move: false,
                    });
                }
            }
        }

        for (i, gir_block) in self.gir_func.blocks.iter().enumerate() {
            let lir_bb = self.block_map[i];

            // Lower instructions.  `current_bb` tracks the active block — some
            // instruction expansions (e.g., nullable-void → Option wrapping) split
            // the block by creating new basic blocks and a Branch terminator, then
            // return the merge block as the new continuation point.
            let mut current_bb = lir_bb;
            for (idx, inst) in gir_block.instructions.iter().enumerate() {
                self.current_span = gir_block.span_map.get(idx).copied().flatten();
                current_bb = self.lower_instruction(inst, current_bb);
            }

            // Lower terminator into the current continuation block.
            if let Some(ref term) = gir_block.terminator {
                self.current_span = gir_block.terminator_span;
                let lir_term = self.lower_terminator(term, current_bb);
                let block = self.lir_func.block_mut(current_bb);
                block.terminator = lir_term;
                block.terminator_span = gir_block.terminator_span;
            }
            self.current_span = None;
            // If no terminator, leave as Unreachable (the default).
        }
    }
}


// ── Free functions ──────────────────────────────────────────────────────────

/// Map a GIR TypeId to an LIR type.
pub fn map_gir_type(type_id: &GirTypeId, registry: &TypeRegistry) -> LirType {
    map_gir_type_with_structs(type_id, registry, None)
}

pub fn map_gir_type_with_structs(
    type_id: &GirTypeId,
    registry: &TypeRegistry,
    struct_reg: Option<&StructRegistry>,
) -> LirType {
    // Check primitive type constants first.
    match *type_id {
        gir_types::BOOL_TYPE => return LirType::Bool,
        gir_types::I8_TYPE => return LirType::I8,
        gir_types::I16_TYPE => return LirType::I16,
        gir_types::I32_TYPE => return LirType::I32,
        gir_types::I64_TYPE => return LirType::I64,
        gir_types::U8_TYPE => return LirType::U8,
        gir_types::U16_TYPE => return LirType::U16,
        gir_types::U32_TYPE => return LirType::U32,
        gir_types::U64_TYPE => return LirType::U64,
        gir_types::F32_TYPE => return LirType::F32,
        gir_types::F64_TYPE => return LirType::F64,
        gir_types::UNIT_TYPE => return LirType::Void,
        _ => {}
    }

    if let Some(gir_type) = registry.get(*type_id) {
        match gir_type {
            GirType::Bool => LirType::Bool,
            GirType::I8 => LirType::I8,
            GirType::I16 => LirType::I16,
            GirType::I32 => LirType::I32,
            GirType::I64 => LirType::I64,
            GirType::U8 => LirType::U8,
            GirType::U16 => LirType::U16,
            GirType::U32 => LirType::U32,
            GirType::U64 => LirType::U64,
            GirType::F32 => LirType::F32,
            GirType::F64 => LirType::F64,
            GirType::Unit => LirType::Void,
            GirType::Ptr(inner) | GirType::MutPtr(inner) => {
                if let Some(sr) = struct_reg {
                    if let Some(GirType::Named(name)) = registry.get(*inner) {
                        // Resolve any registered struct type to PtrTo(sid).
                        if let Some(sid) = sr.lookup(name) {
                            return LirType::PtrTo(sid);
                        }
                        // Collection instantiations map to runtime structs.
                        if let Some(runtime_name) = collection_runtime_type(name, registry) {
                            if let Some(sid) = sr.lookup(runtime_name) {
                                return LirType::PtrTo(sid);
                            }
                        }
                    }
                }
                LirType::Ptr
            }
            GirType::FnPtr { .. } => {
                // FnPtr in GIR is a GorgetClosure struct (fn_ptr + env).
                if let Some(sr) = struct_reg {
                    if let Some(sid) = sr.lookup("GorgetClosure") {
                        return LirType::Struct(sid);
                    }
                }
                LirType::Ptr
            }
            GirType::Named(name) => {
                // Named types are aggregates — resolve to Struct if registered.
                if let Some(sr) = struct_reg {
                    if let Some(sid) = sr.lookup(name) {
                        return LirType::Struct(sid);
                    }
                    // Collection instantiations map to runtime structs.
                    // e.g., Dict__int64_t__int64_t → GorgetMap
                    if let Some(runtime_name) = collection_runtime_type(name, registry) {
                        if let Some(sid) = sr.lookup(runtime_name) {
                            return LirType::Struct(sid);
                        }
                    }
                }
                LirType::Ptr
            }
        }
    } else {
        LirType::I64 // fallback for unknown types
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::instructions::{BinOp, Constant, Instruction, Operand, Place, Terminator};
    use crate::ir::types::{LocalId, BlockId as GirBlockId, I32_TYPE, I64_TYPE, BOOL_TYPE};
    use crate::ir::{BasicBlock, Function, Local, Module};

    fn make_simple_gir_module() -> Module {
        let mut module = Module::new();

        // fn main() -> i32:
        //   _0: i32 (return)
        //   _1: i32 = 42
        //   _2: i32 = _1 + 1
        //   return _2
        let func = Function {
            name: "main".into(),
            params: vec![],
            return_type: I32_TYPE,
            locals: vec![
                Local { type_id: I32_TYPE, name_hint: Some("return".into()), ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },
                Local { type_id: I32_TYPE, name_hint: Some("x".into()), ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },
                Local { type_id: I32_TYPE, name_hint: Some("y".into()), ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },
            ],
            blocks: vec![BasicBlock {
                instructions: vec![
                    Instruction::Assign { mode: crate::ir::instructions::AssignMode::Copy, dst: Place::local(LocalId(1)),
                        value: Operand::Constant(Constant::I32(42)),
                    },
                    Instruction::BinOp {
                        dst: LocalId(2),
                        op: BinOp::Add,
                        type_id: I32_TYPE,
                        lhs: Operand::Copy(Place::local(LocalId(1))),
                        rhs: Operand::Constant(Constant::I32(1)),
                    },
                ],
                terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(2))))),
                span_map: vec![None, None],
                terminator_span: None,
            }],
            is_test_fn: false,
            display_name: Some("main".into()),
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
            participates_in_fault: false,
        };
        module.functions.push(func);
        module
    }

    #[test]
    fn lower_simple_function() {
        let gir = make_simple_gir_module();
        let lir = lower_module(&gir);

        assert_eq!(lir.functions.len(), 1);
        let func = &lir.functions[0];
        assert_eq!(func.name, "main");
        assert_eq!(func.return_type, LirType::I32);
        assert_eq!(func.blocks.len(), 1);

        // Should have: IConst(42), SlotStore, SlotLoad, IConst(1), SlotLoad, Add, SlotStore, SlotLoad, Ret
        let block = &func.blocks[0];
        assert!(!block.insts.is_empty());

        // Verify the terminator is Ret.
        assert!(matches!(block.terminator, Term::Ret(_)));
    }

    #[test]
    fn lower_branch() {
        let mut module = Module::new();
        let func = Function {
            name: "test_branch".into(),
            params: vec![],
            return_type: I64_TYPE,
            locals: vec![
                Local { type_id: I64_TYPE, name_hint: None, ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },
                Local { type_id: BOOL_TYPE, name_hint: Some("cond".into()), ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },
                Local { type_id: I64_TYPE, name_hint: Some("result".into()), ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },
            ],
            blocks: vec![
                BasicBlock {
                    instructions: vec![Instruction::Assign { mode: crate::ir::instructions::AssignMode::Copy, dst: Place::local(LocalId(1)),
                        value: Operand::Constant(Constant::Bool(true)),
                    }],
                    terminator: Some(Terminator::Branch {
                        cond: Operand::Copy(Place::local(LocalId(1))),
                        then_block: GirBlockId(1),
                        else_block: GirBlockId(2),
                    }),
                    span_map: vec![None],
                    terminator_span: None,
                },
                BasicBlock {
                    instructions: vec![Instruction::Assign { mode: crate::ir::instructions::AssignMode::Copy, dst: Place::local(LocalId(2)),
                        value: Operand::Constant(Constant::I64(10)),
                    }],
                    terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(2))))),
                    span_map: vec![None],
                    terminator_span: None,
                },
                BasicBlock {
                    instructions: vec![Instruction::Assign { mode: crate::ir::instructions::AssignMode::Copy, dst: Place::local(LocalId(2)),
                        value: Operand::Constant(Constant::I64(20)),
                    }],
                    terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(2))))),
                    span_map: vec![None],
                    terminator_span: None,
                },
            ],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
            participates_in_fault: false,
        };
        module.functions.push(func);

        let lir = lower_module(&module);
        assert_eq!(lir.functions[0].blocks.len(), 3);

        let term = &lir.functions[0].blocks[0].terminator;
        assert!(matches!(term, Term::Branch { .. }));
    }

    #[test]
    fn lower_call() {
        let mut module = Module::new();
        module.functions.push(Function {
            name: "callee".into(),
            params: vec![I64_TYPE],
            return_type: I64_TYPE,
            locals: vec![
                Local { type_id: I64_TYPE, name_hint: None, ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },
                Local { type_id: I64_TYPE, name_hint: None, ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },
            ],
            blocks: vec![BasicBlock {
                instructions: vec![],
                terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(1))))),
                span_map: vec![],
                terminator_span: None,
            }],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
            participates_in_fault: false,
        });
        module.functions.push(Function {
            name: "caller".into(),
            params: vec![],
            return_type: I64_TYPE,
            locals: vec![
                Local { type_id: I64_TYPE, name_hint: None, ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },
                Local { type_id: I64_TYPE, name_hint: Some("result".into()), ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },
            ],
            blocks: vec![BasicBlock {
                instructions: vec![Instruction::Call {
                    dst: Some(LocalId(1)),
                    func: "callee".into(),
                    args: vec![Operand::Constant(Constant::I64(5))],
                    reason: None,
                }],
                terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(1))))),
                span_map: vec![None],
                terminator_span: None,
            }],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
            participates_in_fault: false,
        });

        let lir = lower_module(&module);
        assert_eq!(lir.functions.len(), 2);

        // Caller's block should contain a Call instruction.
        let caller = &lir.functions[1];
        let has_call = caller.blocks[0].insts.iter().any(|inst| {
            matches!(inst, Inst::Call { func, .. } if *func == FuncId(0))
        });
        assert!(has_call, "expected Call to callee (FuncId(0))");
    }

    #[test]
    fn lower_extern_call() {
        let mut module = Module::new();
        module.externs.push(ir::ExternDecl {
            name: "puts".into(),
            params: vec![],
            return_type: I32_TYPE,
            is_variadic: false,
            param_abis: vec![],
            returns_borrowed: false,
        });
        module.functions.push(Function {
            name: "main".into(),
            params: vec![],
            return_type: I32_TYPE,
            locals: vec![
                Local { type_id: I32_TYPE, name_hint: None, ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },
                Local { type_id: I32_TYPE, name_hint: None, ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },
            ],
            blocks: vec![BasicBlock {
                instructions: vec![Instruction::CallExtern {
                    dst: Some(LocalId(1)),
                    func: "puts".into(),
                    args: vec![],
                }],
                terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(1))))),
                span_map: vec![None],
                terminator_span: None,
            }],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
            participates_in_fault: false,
        });

        let lir = lower_module(&module);
        assert_eq!(lir.externs.len(), 1);
        assert_eq!(lir.externs[0].name, "puts");

        let has_extern_call = lir.functions[0].blocks[0].insts.iter().any(|inst| {
            matches!(inst, Inst::CallExtern { name, .. } if name == "puts")
        });
        assert!(has_extern_call);
    }

    #[test]
    fn type_mapping_primitives() {
        let reg = TypeRegistry::new();
        assert_eq!(map_gir_type(&gir_types::BOOL_TYPE, &reg), LirType::Bool);
        assert_eq!(map_gir_type(&gir_types::I32_TYPE, &reg), LirType::I32);
        assert_eq!(map_gir_type(&gir_types::I64_TYPE, &reg), LirType::I64);
        assert_eq!(map_gir_type(&gir_types::F64_TYPE, &reg), LirType::F64);
        assert_eq!(map_gir_type(&gir_types::U8_TYPE, &reg), LirType::U8);
        assert_eq!(map_gir_type(&gir_types::UNIT_TYPE, &reg), LirType::Void);
    }

    #[test]
    fn lower_constants() {
        let mut module = Module::new();
        module.functions.push(Function {
            name: "consts".into(),
            params: vec![],
            return_type: I32_TYPE,
            locals: vec![
                Local { type_id: I32_TYPE, name_hint: None, ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },
                Local { type_id: I32_TYPE, name_hint: None, ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },
            ],
            blocks: vec![BasicBlock {
                instructions: vec![Instruction::Assign { mode: crate::ir::instructions::AssignMode::Copy, dst: Place::local(LocalId(1)),
                    value: Operand::Constant(Constant::I32(99)),
                }],
                terminator: Some(Terminator::Return(Operand::Constant(Constant::I32(0)))),
                span_map: vec![None],
                terminator_span: None,
            }],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
            participates_in_fault: false,
        });

        let lir = lower_module(&module);
        let func = &lir.functions[0];
        let has_iconst_99 = func.blocks[0].insts.iter().any(|inst| {
            matches!(inst, Inst::IConst { value: 99, .. })
        });
        assert!(has_iconst_99, "expected IConst 99");
    }

    #[test]
    fn dump_lowered_module() {
        let gir = make_simple_gir_module();
        let lir = lower_module(&gir);
        let output = super::super::display::dump_module(&lir);
        assert!(output.contains("fn @main"));
        assert!(output.contains("iconst 42"));
        assert!(output.contains("add"));
        assert!(output.contains("ret"));
    }

    #[test]
    fn map_ptr_named_to_ptr_to() {
        use crate::ir::types::{GirType, TypeRegistry};
        use crate::lir::{StructId};
        use crate::lir::types::StructRegistry;

        let mut registry = TypeRegistry::new();
        let mut struct_reg = StructRegistry::new();

        // Register a named type and a Ptr to it.
        let named_id = registry.insert(GirType::Named("TestStruct".to_string()));
        let ptr_id = registry.insert(GirType::Ptr(named_id));

        // Register the struct name in the struct registry.
        let test_sid = StructId(100);
        struct_reg.register("TestStruct", test_sid);

        // Ptr(Named("TestStruct")) should resolve to PtrTo(test_sid).
        let result = map_gir_type_with_structs(&ptr_id, &registry, Some(&struct_reg));
        assert_eq!(result, LirType::PtrTo(test_sid));

        // Ptr(I64) should fall through to Ptr (no Named inner).
        let ptr_prim = registry.insert(GirType::Ptr(I64_TYPE));
        let result2 = map_gir_type_with_structs(&ptr_prim, &registry, Some(&struct_reg));
        assert_eq!(result2, LirType::Ptr);
    }

    #[test]
    fn map_ptr_collection_to_ptr_to() {
        use crate::ir::types::{
            GirType, TypeRegistry, TypeDef, TypeDefKind, StructDef, TypeMetadata,
            CopySemantics, DropStrategy, CollectionKind,
        };
        use crate::lir::{StructId};
        use crate::lir::types::StructRegistry;

        let mut registry = TypeRegistry::new();
        let mut struct_reg = StructRegistry::new();

        // Register the runtime struct (Vector__* maps to GorgetArray).
        let array_sid = StructId(200);
        struct_reg.register("GorgetArray", array_sid);

        // Phase A registration discipline: every collection type carries
        // a TypeDef with collection_kind set. Name-prefix fallbacks are
        // gone, so the test fixture has to mirror what register_-
        // collection_alias / map_ast_type_mut / etc. would write.
        registry.add_type_def(TypeDef {
            name: "Vector__int64_t".into(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                copy_semantics: CopySemantics::Resource,
                drop_strategy: DropStrategy::Trivial("gorget_array_free".into()),
                clone_fn: Some("gorget_array_clone".into()),
                collection_kind: Some(CollectionKind::Array),
                ..Default::default()
            },
        });
        let vec_id = registry.insert(GirType::Named("Vector__int64_t".to_string()));
        let ptr_vec = registry.insert(GirType::Ptr(vec_id));

        // Ptr(Named("Vector__int64_t")) should resolve via collection_runtime_type.
        let result = map_gir_type_with_structs(&ptr_vec, &registry, Some(&struct_reg));
        assert_eq!(result, LirType::PtrTo(array_sid));
    }
}
