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

pub(super) mod calls;
pub(super) mod drops;
pub(super) mod operands;
pub(super) mod types;

// Re-export functions from sub-modules so they're accessible within this module.
// Re-export the public entry point so it remains at `lir::lower::lower_module`.
pub use self::types::lower_module;

#[allow(unused_imports)]
use self::calls::{
    fix_printf_str_format, runtime_extern_sig, lower_binop, lower_unop, map_cmp_op,
    clone_fn_for_collection_element, is_type_name, is_self_by_ptr_method,
    map_monomorphized_to_runtime_with_table, map_monomorphized_to_runtime,
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
    /// Module globals (for type lookup).
    pub(super) module_globals: &'a [LirGlobal],
    /// Synthetic externs discovered during lowering (for unknown Call targets).
    pub(super) pending_externs: Vec<LirExtern>,
    /// Whether `directive overflow=wrap` is active (integer overflow wraps).
    pub(super) overflow_wrap: bool,
    /// Types whose `{Name}__drop` collides with a user method (e.g., DataFrame.drop()).
    pub(super) drop_collision_types: &'a std::collections::HashSet<String>,
    /// Monomorphized method name → C runtime callee (from BuiltinTypeProtocol).
    pub(super) runtime_callees: &'a rustc_hash::FxHashMap<String, String>,
    /// Enum types needing tag-based variant drop dispatch.
    pub(super) recursive_drop_enums: &'a std::collections::HashMap<String, Vec<(u32, String, String, String, String)>>,
    /// Struct types with field-level drop functions.
    pub(super) recursive_drop_structs: &'a std::collections::HashMap<String, Vec<(String, String, String)>>,
    pub(super) type_drop_fns: &'a std::collections::HashMap<String, crate::lir::TypeDropInfo>,
    /// Extern ABI kinds from module declarations (fn_name → Vec<AbiKind>).
    pub(super) extern_abi_kinds: &'a rustc_hash::FxHashMap<String, Vec<crate::ir::abi::AbiKind>>,
    /// Extern return ABI kinds (fn_name → AbiKind).
    pub(super) return_abi_kinds: &'a rustc_hash::FxHashMap<String, crate::ir::abi::AbiKind>,
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
            // even though the GIR types them as Ptr(StringView) (resource type).
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
            });
        }

        // Lower globals.
        for global in &self.gir.globals {
            let ty = map_gir_type_with_structs(&global.type_id, &self.gir.type_registry, Some(&self.struct_reg));
            let gid = self.module.add_global(LirGlobal {
                name: global.name.clone(),
                ty: ty.clone(),
                init: lower_global_init(&global.init, &self.func_index, &ty),
                is_const: false, // GIR doesn't distinguish const vs mut globals
            });
            self.global_index.insert(global.name.clone(), gid);
        }

        // Generate recursive drop function metadata BEFORE function lowering,
        // so FuncLowering can access drop_collision_types for inline drops.
        self.populate_recursive_drop_structs();

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
                    &self.module.globals,
                    self.gir.runtime.overflow_wrap,
                    &self.module.drop_collision_types,
                    &self.gir.runtime_callees,
                    &self.module.recursive_drop_enums,
                    &self.module.recursive_drop_structs,
                    &self.module.type_drop_fns,
                    &self.gir.fn_extern_abi_kinds,
                    &self.gir.fn_return_abis,
                );
                fl.lower();
                all_pending_externs.extend(fl.pending_externs.drain(..));
                fl.lir_func
            })
            .collect();

        // Register synthetic externs discovered during function lowering.
        for ext in all_pending_externs {
            if let Some(existing) = self.module.externs.iter_mut().find(|e| e.name == ext.name) {
                // Replace if existing is variadic or has fewer params (less specific).
                if existing.is_variadic || (existing.params.is_empty() && !ext.params.is_empty()) {
                    *existing = ext;
                }
            } else {
                self.module.add_extern(ext);
            }
        }

        for func in funcs {
            self.module.add_function(func);
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
            self.module.spawned_fns.push(SpawnedFn {
                fn_name: fn_name.clone(),
                params: lir_params,
                ret_c_type: ret_c,
                ref_param_indices,
                clone_params,
            });
        }

        // Populate thread_spawned_fns from GIR runtime.
        for (fn_name, ret_type) in &self.gir.runtime.thread_spawned_fns {
            let ret_c = gir_type_to_c(*ret_type, &self.gir.type_registry);
            self.module.thread_spawned_fns.push(crate::lir::ThreadSpawnedFn {
                fn_name: fn_name.clone(),
                ret_c_type: ret_c,
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
                let field_type_name = match self.gir.type_registry.get(field.type_id) {
                    Some(GirType::Named(n)) => n.clone(),
                    _ => continue,
                };
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
            // Skip Option/Result — they have their own unwrap/clone handling.
            if name.starts_with("Option__") || name.starts_with("Result__") {
                continue;
            }
            let mut variant_drops: Vec<(u32, String, String, String, String)> = Vec::new();
            for (vi, variant) in edef.variants.iter().enumerate() {
                for (fi, field) in variant.fields.iter().enumerate() {
                    let field_type_name = match self.gir.type_registry.get(field.type_id) {
                        Some(GirType::Named(n)) => n.clone(),
                        _ => continue,
                    };
                    let field_drop = self.infer_drop_strategy(&field_type_name);
                    let drop_fn = match &field_drop {
                        DropStrategy::Trivial(fn_name) => fn_name.clone(),
                        DropStrategy::Custom(fn_name) => fn_name.clone(),
                        DropStrategy::Recursive => format!("{field_type_name}__drop"),
                        DropStrategy::None => continue,
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
                    let mut field_drops: Vec<(String, String, String)> = Vec::new();
                    for field in &sdef.fields {
                        let field_type_name = match self.gir.type_registry.get(field.type_id) {
                            Some(GirType::Named(n)) => n.clone(),
                            _ => continue,
                        };
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
                    if name.starts_with("Option__") || name.starts_with("Result__") {
                        continue;
                    }
                    let mut variant_drops: Vec<(u32, String, String, String, String)> = Vec::new();
                    for (vi, variant) in edef.variants.iter().enumerate() {
                        for (fi, field) in variant.fields.iter().enumerate() {
                            let field_type_name = match self.gir.type_registry.get(field.type_id) {
                                Some(GirType::Named(n)) => n.clone(),
                                _ => continue,
                            };
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
                            let field_name = format!("{}_{fi}", variant.name);
                            variant_drops.push((vi as u32, variant.name.clone(), field_name, drop_fn, field_type_name));
                        }
                    }
                    if !variant_drops.is_empty() {
                        let drop_fn_name = format!("{name}__drop");
                        self.module.type_drop_fns.insert(name.clone(), crate::lir::TypeDropInfo {
                            drop_fn_name,
                            field_drops: Vec::new(),
                            user_drop_fn: None,
                            enum_variants: Some(variant_drops),
                        });
                    }
                }
                _ => {} // Alias types — skip
            }
        }
    }

    /// Infer the drop strategy for a type, falling back to name-based detection
    /// for collection types that don't have TypeDefs in the registry.
    fn infer_drop_strategy(&self, type_name: &str) -> crate::ir::types::DropStrategy {
        use crate::ir::types::DropStrategy;
        // First try the type registry
        if let Some(td) = self.gir.type_registry.get_type_def(type_name) {
            return td.metadata.drop_strategy.clone();
        }
        // Collection types registered without TypeDef — infer from name
        if type_name.starts_with("Vector__") || type_name.starts_with("Deque__") {
            return DropStrategy::Trivial("gorget_array_free".to_string());
        }
        if type_name.starts_with("Dict__") || type_name.starts_with("HashMap__") {
            return DropStrategy::Trivial("gorget_map_free".to_string());
        }
        if type_name.starts_with("Set__") || type_name.starts_with("HashSet__") {
            return DropStrategy::Trivial("gorget_set_free".to_string());
        }
        if type_name.starts_with("Box__") {
            return DropStrategy::Trivial("free".to_string());
        }
        DropStrategy::None
    }

    fn lower_type_defs(&mut self) {
        // Two-pass registration to handle forward references (e.g., an enum
        // whose variant payloads reference structs defined later in the file).

        // Pass 1: Pre-register all type names with empty placeholder structs.
        let mut deferred: Vec<(StructId, usize)> = Vec::new();
        for (idx, def) in self.gir.type_registry.type_defs().iter().enumerate() {
            if self.struct_reg.lookup(&def.name).is_some() {
                continue; // Already registered (builtin).
            }

            // Map collection instantiations to their runtime struct.
            // e.g., Vector__Str → GorgetArray, Dict__Str__int64_t → GorgetMap
            if let Some(runtime_name) = collection_runtime_type(&def.name) {
                if let Some(runtime_sid) = self.struct_reg.lookup(runtime_name) {
                    self.struct_reg.register(&def.name, runtime_sid);
                    continue;
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
            // have data/vtable fields and go through normal two-pass registration.
            if def.name.starts_with("Box__") {
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
            is_enum: false,
            computed_c_size: None,
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
            is_enum: false,
            computed_c_size: None,
                              });
                self.struct_reg.register(&def.name, sid);
                continue;
            }

            match &def.kind {
                gir_types::TypeDefKind::Struct(_) | gir_types::TypeDefKind::Enum(_) => {
                    let sid = self.module.add_struct(StructDef {
                        name: def.name.clone(),
                        fields: vec![],
            is_enum: false,
            computed_c_size: None,
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
            self.module.structs[sid.0 as usize].is_enum = is_large_enum;
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
            if collection_runtime_type(name).is_some() {
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
            is_enum: false,
            computed_c_size: None,
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
            is_enum: false,
            computed_c_size: None,
                                      });
                        self.struct_reg.register(name, sid);
                    }
                    gir_types::TypeDefKind::Alias(_) => {}
                }
                continue;
            }

            // No TypeDef exists — synthesize from the name pattern.
            if let Some(fields) = synthesize_struct_fields(name, &self.struct_reg) {
                let sid = self.module.add_struct(StructDef {
                    name: name.clone(),
                    fields,
            is_enum: false,
            computed_c_size: None,
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
) -> Option<Vec<(String, LirType)>> {
    if let Some(rest) = name.strip_prefix("Result__") {
        // Result__X__Y — split into Ok type (X) and Error type (Y).
        // Handle compound inner types (e.g., Result__Vector__int64_t__Str)
        // by finding the split point.
        let (ok_name, err_name) = split_result_components(rest)?;
        let ok_ty = component_to_lir_type(ok_name, struct_reg);
        let err_ty = component_to_lir_type(err_name, struct_reg);
        return Some(vec![
            ("tag".into(), LirType::I32),
            ("Ok_0".into(), ok_ty),
            ("Error_0".into(), err_ty),
        ]);
    }
    if let Some(inner) = name.strip_prefix("Option__") {
        let some_ty = component_to_lir_type(inner, struct_reg);
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
pub(super) fn component_to_lir_type(name: &str, struct_reg: &StructRegistry) -> LirType {
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
            if let Some(runtime_name) = collection_runtime_type(name) {
                if let Some(sid) = struct_reg.lookup(runtime_name) {
                    return LirType::Struct(sid);
                }
            }
            // Fallback: pointer (opaque or unknown).
            LirType::Ptr
        }
    }
}

/// Map generic collection type names to their runtime struct name.
/// Returns None for non-collection types.
pub(super) fn collection_runtime_type(name: &str) -> Option<&'static str> {
    if name.starts_with("Vector__") {
        return Some("GorgetArray");
    }
    if name.starts_with("Dict__") || name.starts_with("GorgetDict__") {
        return Some("GorgetMap");
    }
    if name.starts_with("HashMap__") || name.starts_with("GorgetMap__") {
        return Some("GorgetMap");
    }
    if name.starts_with("Set__") || name.starts_with("GorgetSet__") {
        return Some("GorgetSet");
    }
    if name.starts_with("Result__") || name.starts_with("Option__") {
        // Result/Option are real structs with fields — don't alias.
        return None;
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
        module_globals: &'a [LirGlobal],
        overflow_wrap: bool,
        drop_collision_types: &'a std::collections::HashSet<String>,
        runtime_callees: &'a rustc_hash::FxHashMap<String, String>,
        recursive_drop_enums: &'a std::collections::HashMap<String, Vec<(u32, String, String, String, String)>>,
        recursive_drop_structs: &'a std::collections::HashMap<String, Vec<(String, String, String)>>,
        type_drop_fns: &'a std::collections::HashMap<String, crate::lir::TypeDropInfo>,
        extern_abi_kinds: &'a rustc_hash::FxHashMap<String, Vec<crate::ir::abi::AbiKind>>,
        return_abi_kinds: &'a rustc_hash::FxHashMap<String, crate::ir::abi::AbiKind>,
    ) -> Self {
        let params: Vec<LirType> = gir_func
            .params
            .iter()
            .map(|t| map_gir_type_with_structs(t, gir_types, Some(struct_reg)))
            .collect();
        let return_type = map_gir_type_with_structs(&gir_func.return_type, gir_types, Some(struct_reg));
        let mut lir_func = LirFunction::new(gir_func.name.clone(), params, return_type);
        lir_func.is_test_fn = gir_func.is_test_fn;
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
            module_globals,
            pending_externs: Vec::new(),
            overflow_wrap,
            drop_collision_types,
            runtime_callees,
            recursive_drop_enums,
            recursive_drop_structs,
            type_drop_fns,
            extern_abi_kinds,
            return_abi_kinds,
        }
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
                    self.lir_func.block_mut(entry_bb).insts.push(Inst::ParamRef {
                        dst: param_val,
                        index: param_idx as u32,
                        ty: slot_ty,
                    });
                    self.lir_func.block_mut(entry_bb).insts.push(Inst::SlotStore {
                        slot,
                        value: param_val,
                        is_move: false,
                    });
                }
            }
        }

        for (i, gir_block) in self.gir_func.blocks.iter().enumerate() {
            let lir_bb = self.block_map[i];

            // Lower instructions.
            for inst in &gir_block.instructions {
                self.lower_instruction(inst, lir_bb);
            }

            // Lower terminator (operand loads emitted into the same block).
            if let Some(ref term) = gir_block.terminator {
                let lir_term = self.lower_terminator(term, lir_bb);
                self.lir_func.block_mut(lir_bb).terminator = lir_term;
            }
            // If no terminator, leave as Unreachable (the default).
        }
    }

    pub(super) fn lower_instruction(&mut self, inst: &Instruction, bb: BlockId) {
        match inst {
            Instruction::Assign { mode, dst, value, .. } => {
                // Special-case: Constant::Null assigned to an enum-typed local.
                if let Operand::Constant(Constant::Null) = value {
                    if let Some(()) = self.try_materialize_null_for_assign(dst, bb) {
                        return;
                    }
                }
                // Special-case: Option/Result source → non-Option/Result dest.
                if let Some(val) = self.try_enum_payload_extract(dst, value, bb) {
                    self.store_to_place(dst, val, bb);
                    return;
                }
                // Special-case: Box[Trait] ← Box[Concrete] trait object construction.
                if self.try_trait_object_construct(dst, value, bb) {
                    return;
                }
                let is_move = matches!(mode, ir::instructions::AssignMode::Move);
                let val = self.lower_operand(value, bb);
                if is_move && dst.projections.is_empty() {
                    // Move: emit SlotStore with is_move flag so C backend can use
                    // memcpy instead of clone for resource types (strings, etc.).
                    let slot = self.local_to_slot[dst.local.0 as usize];
                    self.lir_func.block_mut(bb).insts.push(Inst::SlotStore {
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

                // Check for Vector + Vector → clone lhs then extend with rhs
                let is_vector_add = *op == GirBinOp::Add && matches!(
                    self.gir_types.get(*type_id),
                    Some(GirType::Named(name)) if name.starts_with("Vector__")
                );

                if is_vector_add {
                    // Emit: result = gorget_array_clone(&lhs); gorget_array_extend(&result, &rhs);
                    // The c_lir backend handles &-address-of for array functions via
                    // takes_array_ptr_args / collection_self_by_ptr.
                    let result = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: Some(result),
                        name: "gorget_array_clone".to_string(),
                        args: vec![l],
                        original_name: None,
                    });
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: None,
                        name: "gorget_array_extend".to_string(),
                        args: vec![result, r],
                        original_name: None,
                    });
                    self.store_to_local(*dst, result, bb);
                } else {
                    let result = self.lir_func.next_value();
                    let ty = self.map_type(type_id);
                    let inst = lower_binop(result, *op, l, r, ty, self.overflow_wrap);
                    self.lir_func.block_mut(bb).insts.push(inst);
                    self.store_to_local(*dst, result, bb);
                }
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
                self.lir_func.block_mut(bb).insts.push(inst);
                self.store_to_local(*dst, result, bb);
            }

            Instruction::Cmp {
                dst,
                op,
                type_id: _,
                lhs,
                rhs,
            } => {
                let l = self.lower_operand(lhs, bb);
                let r = self.lower_operand(rhs, bb);
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::Cmp {
                    dst: result,
                    op: map_cmp_op(*op),
                    lhs: l,
                    rhs: r,
                });
                self.store_to_local(*dst, result, bb);
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

                    if is_ptr {
                        // Ptr source (const char*) → GorgetString: wrap directly with gorget_str_from_cstr.
                        let str_ty = self.struct_reg.lookup("GorgetString")
                            .map(LirType::Struct).unwrap_or(LirType::Ptr);
                        let cstr_result = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                            dst: Some(cstr_result),
                            name: "gorget_str_from_cstr".to_string(),
                            args: vec![val],
                            original_name: None,
                        });
                        self.ensure_extern("gorget_str_from_cstr", &[LirType::Ptr], &str_ty);
                        self.store_to_local(*dst, cstr_result, bb);
                    } else {
                    let conv_fn = if is_int {
                        "gorget_int_to_str"
                    } else if is_float {
                        "gorget_float_to_str"
                    } else if is_bool {
                        "gorget_bool_to_str"
                    } else {
                        // Unknown source → use int_to_str as fallback (most casts are int→str).
                        "gorget_int_to_str"
                    };
                    // Emit CallExtern to the conversion function (returns const char*).
                    let cstr_result = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: Some(cstr_result),
                        name: conv_fn.to_string(),
                        args: vec![val],
                        original_name: None,
                    });
                    let str_ty = if let Some(sid) = self.struct_reg.lookup("Str") { LirType::Struct(sid) } else { LirType::Ptr };
                    self.ensure_extern(conv_fn, &[if is_float { LirType::F64 } else if is_bool { LirType::Bool } else { LirType::I64 }], &str_ty);
                    // The result is a Str struct (returned by gorget_string_adopt in the C runtime).
                    self.store_to_local(*dst, cstr_result, bb);
                    } // close else (non-ptr) branch
                } else if matches!(to, LirType::Void) {
                    // Cast to void — just evaluate for side effects, don't generate (void)(val).
                    // No store needed.
                } else {
                    let result = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::IntCast {
                        dst: result,
                        value: val,
                        to,
                    });
                    self.store_to_local(*dst, result, bb);
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
                self.lir_func.block_mut(bb).insts.push(Inst::Bitcast {
                    dst: result,
                    value: val,
                    to,
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::PtrCast { dst, value, .. } => {
                let val = self.lower_operand(value, bb);
                let result = self.lir_func.next_value();
                self.lir_func
                    .block_mut(bb)
                    .insts
                    .push(Inst::PtrCast { dst: result, value: val });
                self.store_to_local(*dst, result, bb);
            }

            // -- Calls --
            Instruction::Call { dst, func, args, .. } => {
                if let Some(fid) = self.func_index.get(func) {
                    let lir_args: Vec<ValueId> =
                        args.iter().map(|a| self.lower_operand(a, bb)).collect();
                    let result = dst.map(|_| self.lir_func.next_value());
                    self.lir_func.block_mut(bb).insts.push(Inst::Call {
                        dst: result,
                        func: *fid,
                        args: lir_args,
                    });
                    if let (Some(d), Some(r)) = (*dst, result) {
                        self.store_to_local(d, r, bb);
                    }
                } else {
                    // Unknown function — treat as extern.
                    // Map monomorphized collection/method names to runtime function names.
                    let emit_name = map_monomorphized_to_runtime_with_table(func, self.runtime_callees)
                        .unwrap_or_else(|| func.clone());
                    // For collection/concurrency methods that take self by pointer,
                    // if the first arg is a GlobalRef, emit GlobalAddr (pointer)
                    // instead of GlobalAddr+Load (copy), so mutations affect the global.
                    let needs_self_by_ptr = is_self_by_ptr_method(func);
                    let lir_args: Vec<ValueId> =
                        args.iter().enumerate().map(|(i, a)| {
                            if i == 0 && needs_self_by_ptr {
                                if let Operand::Constant(Constant::GlobalRef(name)) = a {
                                    if let Some(&gid) = self.global_index.get(name) {
                                        let addr = self.lir_func.next_value();
                                        self.lir_func.block_mut(bb).insts.push(
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
                        if arg_type.as_deref().map_or(false, |n| n.starts_with("Vector__") || n == "GorgetArray") {
                            "gorget_array_len".to_string()
                        } else if arg_type.as_deref().map_or(false, |n| n.starts_with("Dict__") || n.starts_with("HashMap__") || n == "GorgetMap") {
                            "gorget_map_len".to_string()
                        } else if arg_type.as_deref().map_or(false, |n| n.starts_with("Set__") || n.starts_with("HashSet__") || n == "GorgetSet") {
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
                                self.lir_func.block_mut(bb).insts.push(Inst::Call {
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
                    // gorget_regex_find/split take 3 args but GIR only passes 2 — inject default 0
                    if (emit_name == "gorget_regex_find" || emit_name == "gorget_regex_split") && lir_args.len() == 2 {
                        let zero_val = self.emit_i64_const(bb, 0);
                        lir_args.push(zero_val);
                    }
                    // Delegate to the shared extern-call emitter (same logic as CallExtern).
                    if !len_handled {
                        self.emit_extern_call(func, &emit_name, dst, args, lir_args, bb);
                    }
                }
            }

            Instruction::CallExtern { dst, func, args } => {
                // If the callee is actually a defined function in this module (GIR uses
                // call_extern for user-defined iterator/trait methods), emit a direct Call.
                if let Some(fid) = self.func_index.get(func) {
                    let lir_args: Vec<ValueId> =
                        args.iter().map(|a| self.lower_operand(a, bb)).collect();
                    let result = dst.map(|_| self.lir_func.next_value());
                    self.lir_func.block_mut(bb).insts.push(Inst::Call {
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
                let mut emit_name = map_monomorphized_to_runtime_with_table(func, self.runtime_callees)
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
                let lir_args: Vec<ValueId> = if is_printf_like {
                    // For printf, expand Str-typed args into (int)len, data pairs.
                    self.lower_printf_args(args, bb)
                } else {
                    {
                    // For collection/concurrency methods that take self by pointer,
                    // if the first arg is a GlobalRef, emit GlobalAddr (pointer)
                    // instead of GlobalAddr+Load (copy), so mutations affect the global.
                    let needs_self_by_ptr = is_self_by_ptr_method(func);
                    args.iter().enumerate().map(|(i, a)| {
                        if i == 0 && needs_self_by_ptr {
                            if let Operand::Constant(Constant::GlobalRef(name)) = a {
                                if let Some(&gid) = self.global_index.get(name) {
                                    let addr = self.lir_func.next_value();
                                    self.lir_func.block_mut(bb).insts.push(
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
                self.emit_extern_call(func, &emit_name, dst, args, lir_args, bb);
                }
            }

            Instruction::CallIndirect { dst, callee, args } => {
                let callee_val = self.lower_operand(callee, bb);
                let lir_args: Vec<ValueId> =
                    args.iter().map(|a| self.lower_operand(a, bb)).collect();
                let result = dst.map(|_| self.lir_func.next_value());
                self.lir_func.block_mut(bb).insts.push(Inst::CallPtr {
                    dst: result,
                    callee: callee_val,
                    args: lir_args,
                });
                if let (Some(d), Some(r)) = (*dst, result) {
                    self.store_to_local(d, r, bb);
                }
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
                self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
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

                for (i, field_op) in fields.iter().enumerate() {
                    // Special-case: Null operand for an enum-typed field (e.g. Option<T> = None).
                    // Instead of emitting NullPtr (memcpy from NULL → segfault), properly
                    // initialize the field with the null variant tag.
                    if matches!(field_op, Operand::Constant(Constant::Null)) {
                        if let Some(Some(fty)) = field_type_ids.get(i) {
                            if let Some((field_enum_sid, tag_ordinal)) = self.find_enum_null_variant(*fty) {
                                // The parent struct slot is zero-initialized (= {0}), so the
                                // payload bytes are already zero.  We only need to set the tag
                                // to the null-variant ordinal (e.g. None=1).
                                let fptr = self.lir_func.next_value();
                                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                                    dst: fptr,
                                    base,
                                    struct_id,
                                    field: i as u32,
                                });
                                let tag_val = self.emit_i32_const(bb, tag_ordinal as i64);
                                let tag_ptr = self.lir_func.next_value();
                                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                                    dst: tag_ptr,
                                    base: fptr,
                                    struct_id: field_enum_sid,
                                    field: 0,
                                });
                                self.lir_func.block_mut(bb).insts.push(Inst::Store {
                                    ptr: tag_ptr,
                                    value: tag_val,
                                });
                                continue;
                            }
                        }
                    }

                    let val = self.lower_operand(field_op, bb);
                    let fptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: fptr,
                        base,
                        struct_id,
                        field: i as u32,
                    });
                    self.lir_func.block_mut(bb).insts.push(Inst::Store {
                        ptr: fptr,
                        value: val,
                    });
                }
            }

            Instruction::FieldLoad {
                dst,
                base,
                field,
                ..
            } => {
                let mut base_val = self.lower_place_addr(base, bb);
                // Use effective type after base projections (e.g., Deref→Field chain).
                let effective_type = self.effective_place_type(base);
                // If the effective type is a pointer (e.g., closure env param),
                // load the pointer value first so FieldPtr operates on the struct, not the slot.
                // Skip for ref_locals — they're already pointers from collection reads;
                // lower_place_addr already does the SlotLoad to get the pointer value.
                let is_ref_local = base.projections.is_empty()
                    && self.gir_func.ref_locals.contains(&base.local);
                if !is_ref_local && matches!(self.gir_types.get(effective_type), Some(GirType::Ptr(_) | GirType::MutPtr(_))) {
                    let deref = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: deref,
                        ptr: base_val,
                        ty: LirType::Ptr,
                    });
                    base_val = deref;
                }
                let struct_id = self.resolve_struct_id_for_field(effective_type, *field, self.module_structs);
                let fptr = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                    dst: fptr,
                    base: base_val,
                    struct_id,
                    field: *field,
                });
                // If destination is Ptr(T), return field address as pointer reference.
                let dst_gir_type = self.gir_func.locals[dst.0 as usize].type_id;
                if matches!(self.gir_types.get(dst_gir_type), Some(GirType::Ptr(_))) {
                    self.store_to_local(*dst, fptr, bb);
                } else {
                    let field_ty = self.resolve_field_type(effective_type, *field);
                    // If field is Ptr but dst is a value type (Str), double-deref:
                    // load Ptr from field, then load Str value through Ptr.
                    let dst_slot = self.local_to_slot[dst.0 as usize];
                    let dst_slot_ty = self.lir_func.slots[dst_slot.0 as usize].ty.clone();
                    if matches!(field_ty, LirType::Ptr) && dst_slot_ty.is_aggregate() {
                        let ptr_val = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::Load {
                            dst: ptr_val, ptr: fptr, ty: LirType::Ptr,
                        });
                        let result = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::Load {
                            dst: result, ptr: ptr_val, ty: dst_slot_ty,
                        });
                        self.store_to_local(*dst, result, bb);
                    } else {
                        let result = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::Load {
                            dst: result,
                            ptr: fptr,
                            ty: field_ty,
                        });
                        self.store_to_local(*dst, result, bb);
                    }
                }
            }

            Instruction::IndexLoad { dst, base, index } => {
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
                let is_array = base_type_name.starts_with("Vector__")
                    || base_type_name == "GorgetArray";
                let is_dict = base_type_name.starts_with("Dict__")
                    || base_type_name.starts_with("GorgetMap")
                    || base_type_name.starts_with("HashMap__");

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
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: start_ptr, base: range_val, struct_id: range_sid, field: 0,
                    });
                    let start = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: start, ptr: start_ptr, ty: LirType::I64,
                    });
                    let end_ptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: end_ptr, base: range_val, struct_id: range_sid, field: 1,
                    });
                    let end = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
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
                    let result = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: Some(result),
                        name: fn_name.to_string(),
                        args: vec![base_val, start, end],
                        original_name: None,
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
                    let result = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: Some(result),
                        name: "gorget_str_index".to_string(),
                        args: vec![base_val, idx],
                        original_name: None,
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
                    let is_ref_local = self.gir_func.ref_locals.contains(&base.local);
                    if matches!(self.gir_types.get(base_gir), Some(GirType::Ptr(_)))
                        && base.projections.is_empty()
                        && !is_ref_local
                    {
                        let deref = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::Load {
                            dst: deref, ptr: base_val, ty: LirType::Ptr,
                        });
                        base_val = deref;
                    }
                    let idx = self.lower_operand(index, bb);
                    let fn_name = if is_dict { "gorget_map_get" } else { "gorget_array_get" };
                    self.ensure_extern(fn_name, &[LirType::Ptr, LirType::I64], &LirType::Ptr);
                    let ptr_val = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: Some(ptr_val),
                        name: fn_name.to_string(),
                        args: vec![base_val, idx],
                        original_name: None,
                    });
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
                        self.store_to_local(*dst, ptr_val, bb);
                        return;
                    }
                    // Otherwise dereference to get the actual element value.
                    let dst_slot = self.local_to_slot[dst.0 as usize];
                    let mut elem_ty = self.lir_func.slots[dst_slot.0 as usize].ty.clone();
                    // Closures are 16 bytes (GorgetClosure) but may be typed as I64 in LIR.
                    // Fix: re-derive from GIR type with struct registry to get the correct
                    // struct type, so Load reads the full closure (not just 8 bytes).
                    // Closures are 16 bytes (GorgetClosure) but typed as I64 in GIR/LIR.
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
                    let clone_fn = clone_fn_for_collection_element(elem_type_name);

                    if let Some(clone_fn_name) = clone_fn {
                        // Clone: call gorget_*_clone(elem_ptr) → new deep copy
                        let ret_ty = elem_ty.clone();
                        self.ensure_extern(clone_fn_name, &[LirType::Ptr], &ret_ty);
                        let result = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                            dst: Some(result),
                            name: clone_fn_name.to_string(),
                            args: vec![ptr_val],
                            original_name: None,
                        });
                        self.store_to_local(*dst, result, bb);
                    } else {
                        let elem_drop = self.infer_drop_strategy(elem_type_name);
                        if matches!(elem_drop, crate::ir::types::DropStrategy::Recursive) {
                            // Recursive-drop struct: deep-clone via {Type}__clone(ptr)
                            // to produce an independently-owned copy. The collection
                            // retains its original element.
                            let clone_fn = format!("{elem_type_name}__clone");
                            let ret_ty = elem_ty.clone();
                            self.ensure_extern(&clone_fn, &[LirType::Ptr], &ret_ty);
                            let result = self.lir_func.next_value();
                            self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                                dst: Some(result),
                                name: clone_fn,
                                args: vec![ptr_val],
                                original_name: None,
                            });
                            self.store_to_local(*dst, result, bb);
                        } else {
                            // Other non-collection element: Load + move-zero
                            let result = self.lir_func.next_value();
                            self.lir_func.block_mut(bb).insts.push(Inst::Load {
                                dst: result,
                                ty: elem_ty.clone(),
                                ptr: ptr_val,
                            });
                            self.store_to_local(*dst, result, bb);
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
                                self.lir_func.block_mut(bb).insts.push(Inst::Memset {
                                    ptr: ptr_val, byte: zero, size: sz,
                                });
                            }
                        }
                    }
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
                    self.lir_func.block_mut(bb).insts.push(Inst::ElemPtr {
                        dst: elem_ptr,
                        base: base_val,
                        index: idx,
                        elem_size,
                    });
                    let result = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
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
                self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                    dst: base,
                    slot,
                });

                // Store tag (field 0).
                let tag_ordinal = self.resolve_variant_ordinal(type_name, variant);
                let tag_val = self.emit_i32_const(bb, tag_ordinal as i64);
                let tag_ptr = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                    dst: tag_ptr,
                    base,
                    struct_id,
                    field: 0,
                });
                self.lir_func.block_mut(bb).insts.push(Inst::Store {
                    ptr: tag_ptr,
                    value: tag_val,
                });

                // Store variant fields (offset: 1 + sum of preceding variant fields).
                let field_offset = self.resolve_variant_field_offset(type_name, variant);
                // Look up field types for Null → enum promotion (same as StructInit).
                let variant_field_types = self.resolve_variant_field_types(type_name, variant);
                for (i, field_op) in fields.iter().enumerate() {
                    // Special-case: Null field for an enum type (e.g. Some(None)).
                    if matches!(field_op, Operand::Constant(Constant::Null)) {
                        if let Some(Some(fty)) = variant_field_types.get(i) {
                            if let Some((field_enum_sid, fld_tag_ordinal)) = self.find_enum_null_variant(*fty) {
                                let fptr = self.lir_func.next_value();
                                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                                    dst: fptr, base, struct_id,
                                    field: (field_offset + i) as u32,
                                });
                                self.emit_enum_tag_store(fptr, field_enum_sid, fld_tag_ordinal, bb);
                                continue;
                            }
                        }
                    }

                    let val = self.lower_operand(field_op, bb);
                    let fptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: fptr,
                        base,
                        struct_id,
                        field: (field_offset + i) as u32,
                    });
                    self.lir_func.block_mut(bb).insts.push(Inst::Store {
                        ptr: fptr,
                        value: val,
                    });
                }

                // Post-init zero: after moving a resource-type local into an enum variant
                // (e.g. Some(vec)), zero the source to prevent double-free. The enum now
                // owns the data. This mirrors the old GIR→C backend's post-EnumInit zeroing.
                // Collect slots to zero first to avoid borrow conflicts.
                let slots_to_zero: Vec<(SlotId, i64)> = fields.iter().filter_map(|field_op| {
                    if let Operand::Copy(place) | Operand::Move(place) = field_op {
                        if place.projections.is_empty() {
                            let local_idx = place.local.0 as usize;
                            if local_idx < self.local_to_slot.len() {
                                let src_slot = self.local_to_slot[local_idx];
                                let src_ty = &self.lir_func.slots[src_slot.0 as usize].ty;
                                if let LirType::Struct(sid) = src_ty {
                                    let needs_zero = self.module_structs.get(sid.0 as usize)
                                        .map_or(false, |s| matches!(s.name.as_str(),
                                            "GorgetArray" | "GorgetMap" | "GorgetSet" | "GorgetString" | "GorgetClosure"
                                        ));
                                    if needs_zero {
                                        let byte_size = c_sizeof_lir_type(src_ty, &self.module_structs) as i64;
                                        return Some((src_slot, byte_size));
                                    }
                                }
                            }
                        }
                    }
                    None
                }).collect();
                for (src_slot, byte_size) in slots_to_zero {
                    let addr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                        dst: addr,
                        slot: src_slot,
                    });
                    let zero = self.emit_i32_const(bb, 0);
                    let size = self.emit_i64_const(bb, byte_size);
                    self.lir_func.block_mut(bb).insts.push(Inst::Memset {
                        ptr: addr,
                        byte: zero,
                        size,
                    });
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
                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                    dst: tag_ptr,
                    base: val,
                    struct_id,
                    field: 0,
                });
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::Load {
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
            } => {
                let mut base_val = self.lower_place_addr(base, bb);
                let gir_type_id = self.gir_func.locals[base.local.0 as usize].type_id;
                // If after resolving projections we still have a pointer type,
                // the base_val is a SlotAddr of a pointer local — load the pointer
                // to get the actual enum struct address.
                let effective_ty = self.effective_place_type(base);
                if let Some(GirType::Ptr(_) | GirType::MutPtr(_)) = self.gir_types.get(effective_ty) {
                    let deref = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: deref,
                        ptr: base_val,
                        ty: LirType::Ptr,
                    });
                    base_val = deref;
                }
                let struct_id = self.resolve_struct_id(gir_type_id);
                let type_name = self.resolve_type_name(gir_type_id);
                let field_offset = self.resolve_variant_field_offset(&type_name, variant);


                let fptr = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                    dst: fptr,
                    base: base_val,
                    struct_id,
                    field: (field_offset + *field as usize) as u32,
                });
                // If destination is Ptr(T), return field address as pointer reference.
                // This happens when the scrutinee is a borrowed enum (Ptr param).
                let dst_gir_type = self.gir_func.locals[dst.0 as usize].type_id;
                if matches!(self.gir_types.get(dst_gir_type), Some(GirType::Ptr(_))) {
                    self.store_to_local(*dst, fptr, bb);
                } else {
                    let result = self.lir_func.next_value();
                    let field_ty = self.resolve_enum_field_type(gir_type_id, variant, *field);
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: result,
                        ptr: fptr,
                        ty: field_ty,
                    });
                    self.store_to_local(*dst, result, bb);
                }
            }

            Instruction::TupleInit { dst, elements } => {
                // Tuples are stored as struct slots. Store each element by field index.
                let slot = self.local_to_slot[dst.0 as usize];
                let base = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                    dst: base,
                    slot,
                });
                // Need the struct_id for the tuple type.
                let gir_type_id = self.gir_func.locals[dst.0 as usize].type_id;
                let struct_id = self.resolve_struct_id(gir_type_id);

                for (i, elem) in elements.iter().enumerate() {
                    let val = self.lower_operand(elem, bb);
                    let fptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: fptr,
                        base,
                        struct_id,
                        field: i as u32,
                    });
                    self.lir_func.block_mut(bb).insts.push(Inst::Store {
                        ptr: fptr,
                        value: val,
                    });
                }
            }

            // -- Ownership / lifetime (pass-through as calls or nops) --
            Instruction::Drop { place } => {
                self.lower_drop(place, bb);
            }

            Instruction::DropIfAlive { place } => {
                self.lower_drop(place, bb);
            }

            Instruction::MoveZero { place } => {
                // Zero out a place after move. Emit memset(addr, 0, sizeof).
                // For PtrTo locals (pointer-wrapped strings), zero the POINTER SLOT
                // (set to NULL), not the pointee. lower_place_addr for PtrTo does
                // SlotLoad (returns pointer value), so memset would corrupt pointee.
                let slot = self.local_to_slot[place.local.0 as usize];
                let is_ptr_slot = matches!(self.lir_func.slots[slot.0 as usize].ty, LirType::PtrTo(_));
                let addr = if is_ptr_slot && place.projections.is_empty() {
                    let a = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr { dst: a, slot });
                    a
                } else {
                    self.lower_place_addr(place, bb)
                };
                let zero = self.emit_i32_const(bb, 0);
                // Resolve the actual type being zeroed, following projections.
                let effective_ty = if place.projections.is_empty() {
                    let slot_idx = place.local.0 as usize;
                    self.lir_func.slots[self.local_to_slot[slot_idx].0 as usize].ty.clone()
                } else {
                    // Follow projections to find the leaf type.
                    let mut gir_type = self.gir_func.locals[place.local.0 as usize].type_id;
                    for proj in &place.projections {
                        match proj {
                            Projection::Field(field) => {
                                gir_type = self.resolve_field_gir_type_id(gir_type, *field);
                            }
                            Projection::Deref => {
                                gir_type = self.resolve_deref_gir_type_id(gir_type);
                            }
                            Projection::Index(_) => {
                                // Index projection: element type unknown at this level.
                                break;
                            }
                        }
                    }
                    self.map_type(&gir_type)
                };
                let byte_size = match &effective_ty {
                    LirType::Struct(_) => c_sizeof_lir_type(&effective_ty, &self.module_structs) as i64,
                    _ => super::types::scalar_size(&effective_ty).unwrap_or(8) as i64,
                };
                let size = self.emit_i64_const(bb, byte_size);
                self.lir_func.block_mut(bb).insts.push(Inst::Memset {
                    ptr: addr,
                    byte: zero,
                    size,
                });
            }

            Instruction::Borrow { dst, place } | Instruction::BorrowMut { dst, place } => {
                let addr = self.lower_place_addr(place, bb);
                self.store_to_local(*dst, addr, bb);
            }

            // -- Ref load/store (explicit Ptr dereference) --
            Instruction::LoadRef { dst, src } => {
                // Load through Ptr: deref src to get value, store in dst.
                // Same as FieldLoad with Deref projection, but explicit.
                let src_addr = self.lower_place_addr(src, bb);
                let src_type = self.effective_place_type(src);
                let pointee = self.resolve_deref_gir_type_id(src_type);
                let field_ty = self.map_type(&pointee);
                let deref_val = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::Load {
                    dst: deref_val,
                    ptr: src_addr,
                    ty: field_ty,
                });
                self.store_to_local(*dst, deref_val, bb);
            }
            Instruction::StoreRef { dst, value } => {
                // Store through Ptr: write value to the address held by dst.
                let val = self.lower_operand(value, bb);
                let dst_addr = self.lower_place_addr(dst, bb);
                // Deref the Ptr to get the target address
                let target = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::Load {
                    dst: target,
                    ptr: dst_addr,
                    ty: LirType::Ptr,
                });
                self.lir_func.block_mut(bb).insts.push(Inst::Store {
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
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: Some(result),
                    name: "__gorget_alloc".into(),
                    args: vec![alloc],
                    original_name: None,
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
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: Some(result),
                    name: "__gorget_alloc_array".into(),
                    args: vec![cnt, alloc],
                    original_name: None,
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::Dealloc { ptr, allocator } => {
                let p = self.lower_operand(ptr, bb);
                let a = self.lower_operand(allocator, bb);
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: "__gorget_dealloc".into(),
                    args: vec![p, a],
                    original_name: None,
                });
            }

            Instruction::LoadThreadLocal { dst, name } => {
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: Some(result),
                    name: format!("__gorget_tls_{name}"),
                    args: vec![],
                    original_name: None,
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::PushAllocator { allocator } => {
                let alloc = self.lower_operand(allocator, bb);
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: "__gorget_push_allocator".into(),
                    args: vec![alloc],
                    original_name: None,
                });
            }

            Instruction::PopAllocator => {
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: "__gorget_pop_allocator".into(),
                    args: vec![],
                    original_name: None,
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
                        self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                            dst: addr_dummy,
                            slot,
                        });
                        let val = self.lir_func.next_value();
                        // Emit InlineC with a dst, then store to slot.
                        self.lir_func.block_mut(bb).insts.push(Inst::InlineC {
                            dst: Some(val),
                            code: code.clone(),
                        });
                        self.lir_func.block_mut(bb).insts.push(Inst::SlotStore {
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
                    self.lir_func.block_mut(bb).insts.push(Inst::InlineC {
                        dst: None,
                        code: code.clone(),
                    });
                }
            }

            Instruction::GlobalAssign { name, value } => {
                if let Some(&gid) = self.global_index.get(name) {
                    let val = self.lower_operand(value, bb);
                    let addr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::GlobalAddr { dst: addr, global: gid });
                    let global_ty = &self.module_globals[gid.0 as usize].ty;
                    if global_ty.is_scalar() {
                        // Scalar store: dereference and assign.
                        self.lir_func.block_mut(bb).insts.push(Inst::Store { ptr: addr, value: val });
                    } else {
                        // Aggregate store: memcpy.
                        self.lir_func.block_mut(bb).insts.push(Inst::Store { ptr: addr, value: val });
                    }
                }
            }

            Instruction::Nop => {
                self.lir_func.block_mut(bb).insts.push(Inst::Nop);
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
                    self.lir_func.block_mut(bb).insts.push(Inst::Call {
                        dst: result,
                        func: *fid,
                        args: lir_args,
                    });
                } else {
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: result,
                        name: func.clone(),
                        args: lir_args,
                        original_name: None,
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
                            self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
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
                matches!(slot_ty, LirType::Struct(sid) if Some(*sid) == str_sid)
            }
            _ => false,
        }
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
    ) {
        // Guard/ReadGuard/WriteGuard get/get_ptr: inline as FieldPtr + Load
        // instead of calling the runtime function. This preserves the concrete
        // inner type through the LIR so the c_lir backend emits correct code.
        // gorget_guard_get(guard*) → load guard->ptr, then load *(T*)ptr
        // gorget_guard_get_ptr(guard*) → load guard->ptr (returns void*)
        if matches!(emit_name, "gorget_guard_get" | "gorget_read_guard_get" | "gorget_write_guard_get") {
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
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: ptr_val,
                        base: guard_ptr,
                        struct_id: sid,
                        field: 1,
                    });
                    let data_ptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: data_ptr,
                        ptr: ptr_val,
                        ty: LirType::Ptr,
                    });
                    // Dereference to the concrete inner type.
                    let result = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: result,
                        ptr: data_ptr,
                        ty: inner_ty,
                    });
                    self.store_to_local(d, result, bb);
                    return;
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
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: ptr_val,
                        base: guard_ptr,
                        struct_id: sid,
                        field: 1,
                    });
                    let data_ptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: data_ptr,
                        ptr: ptr_val,
                        ty: LirType::Ptr,
                    });
                    self.store_to_local(d, data_ptr, bb);
                    return;
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
                let elem_sz = elem_size_from_monomorphized(original_name, self.module_structs).unwrap_or(8) as i64;
                let sz_val = self.emit_i64_const(bb, elem_sz);
                lir_args.push(sz_val);
            }
        }
        // gorget_map_new / gorget_dict_new — need sizeof args.
        // For Str/GorgetString keys, use _str variant which
        // sets up the string hash function.
        if (emit_name == "gorget_map_new" || emit_name == "gorget_dict_new") && lir_args.is_empty() {
            let is_dict = emit_name == "gorget_dict_new";
            let (key_sz, val_sz) = dict_elem_sizes_from_monomorphized(original_name, self.module_structs);
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
            let elem_sz = concurrency_elem_size(original_name, self.module_structs).unwrap_or(8) as i64;
            let sz_val = self.emit_i64_const(bb, elem_sz);
            lir_args.insert(0, sz_val);
        }

        // gorget_channel_new(capacity, elem_size) — GIR passes (capacity).
        if emit_name == "gorget_channel_new" && lir_args.len() == 1 {
            let elem_sz = concurrency_elem_size(original_name, self.module_structs).unwrap_or(8) as i64;
            let sz_val = self.emit_i64_const(bb, elem_sz);
            lir_args.push(sz_val);
        }

        // gorget_guard_set(guard, &val, sizeof) and gorget_write_guard_set
        if matches!(emit_name, "gorget_guard_set" | "gorget_write_guard_set")
            && lir_args.len() == 2
        {
            let elem_sz = concurrency_elem_size(original_name, self.module_structs).unwrap_or(8) as i64;
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
                self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                    dst: slot_ptr,
                    slot,
                });
                lir_args.push(slot_ptr);
                let mut arg_types: Vec<LirType> = args.iter().map(|a| self.operand_lir_type(a)).collect();
                arg_types.push(LirType::Ptr);
                self.ensure_extern(&to_name, &arg_types, &LirType::Void);
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: to_name,
                    args: lir_args,
                    original_name: None,
                });
                return;
            }
        }

        // Derive arg types from GIR operand types (for proper extern declarations).
        let is_printf_like = emit_name == "printf" || emit_name == "fprintf_stderr"
            || emit_name == "gorget_string_format" || emit_name == "gorget_string_format_alloc"
            || emit_name == "snprintf" || emit_name == "sprintf";
        let arg_types: Vec<LirType> = if is_printf_like {
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
        // __callable_N and __gorget_closure_call_N use function-scoped local IDs.
        // Different functions can have __callable_3 with different return types.
        // Make the extern name unique per function to avoid type conflicts.
        let actual_emit_name = if emit_name.starts_with("__callable_") || emit_name.starts_with("__gorget_closure_call_") {
            format!("{}__{}", emit_name, self.lir_func.name.replace("::", "__"))
        } else {
            emit_name.to_string()
        };
        self.ensure_extern(&actual_emit_name, &arg_types, &ret_ty);

        // Self-cleaning: gorget_array_set calls elem_drop internally.

        let is_void_ret = matches!(ret_ty, LirType::Void);
        let result = if is_void_ret { None } else { dst.map(|_| self.lir_func.next_value()) };
        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
            dst: result,
            name: actual_emit_name,
            args: lir_args,
            original_name: Some(original_name.to_string()),
        });
        if let (Some(d), Some(r)) = (*dst, result) {
            self.store_to_local(d, r, bb);
        }

        // Post-call zeroing: after push/set/send that consumes a value by move,
        // zero the source local to prevent double-free at scope-end Drop.
        // The GIR backend does this inline; we do it here in the LIR lowering
        // because the GIR's MoveZero doesn't cover all push cases.
        let consuming_arg_gir_idx: Option<usize> = match emit_name {
            "gorget_array_push" | "gorget_set_add" | "gorget_heap_push" => Some(1),
            "gorget_array_insert" | "gorget_array_set" | "gorget_map_put" => Some(2),
            "gorget_channel_send" => Some(1),
            _ => None,
        };
        if let Some(arg_idx) = consuming_arg_gir_idx {
            if let Some(arg) = args.get(arg_idx) {
                if let Operand::Copy(place) | Operand::Move(place) = arg {
                    if place.projections.is_empty() {
                        let local_idx = place.local.0 as usize;
                        if local_idx < self.gir_func.locals.len() {
                            let type_id = self.gir_func.locals[local_idx].type_id;
                            if let Some(GirType::Named(name)) = self.gir_types.get(type_id) {
                                // Only zero types that need dropping AND are user/struct types
                                // (not primitive scalars). Direct resource types (GorgetArray etc.)
                                // are already handled by the c_lir backend's post-push zero.
                                let needs_zero = self.gir_types.get_type_def(name).map_or(false, |td| {
                                    matches!(td.metadata.drop_strategy,
                                        crate::ir::types::DropStrategy::Custom(_) |
                                        crate::ir::types::DropStrategy::Recursive)
                                });
                                if needs_zero {
                                    let slot = self.local_to_slot[local_idx];
                                    let slot_ty = self.lir_func.slots[slot.0 as usize].ty.clone();
                                    let byte_size = match &slot_ty {
                                        LirType::Struct(_) => c_sizeof_lir_type(&slot_ty, &self.module_structs) as i64,
                                        _ => super::types::scalar_size(&slot_ty).unwrap_or(8) as i64,
                                    };
                                    let addr = self.lir_func.next_value();
                                    self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                                        dst: addr, slot,
                                    });
                                    let zero_val = self.emit_i32_const(bb, 0);
                                    let size_val = self.emit_i64_const(bb, byte_size);
                                    self.lir_func.block_mut(bb).insts.push(Inst::Memset {
                                        ptr: addr, byte: zero_val, size: size_val,
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Lower printf/fprintf args, expanding Str-typed operands to (int)len, data.
    pub(super) fn lower_printf_args(&mut self, args: &[Operand], bb: BlockId) -> Vec<ValueId> {
        let mut lir_args = Vec::new();
        // Pre-scan: which args (1-based) are Str-typed? We need this to fix the format string.
        let str_arg_indices: Vec<bool> = args.iter().enumerate()
            .map(|(i, a)| i > 0 && self.operand_is_str(a))
            .collect();
        let has_str_args = str_arg_indices.iter().any(|&b| b);

        for (i, arg) in args.iter().enumerate() {
            if i == 0 {
                // First arg is always the format string (const char*).
                // If any subsequent args are Str, fix the format string:
                // replace corresponding %lld with %.*s.
                if has_str_args {
                    if let Operand::Constant(Constant::Str(fmt_str)) = arg {
                        let fixed = fix_printf_str_format(fmt_str, &str_arg_indices[1..]);
                        let fixed_val = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::StrLit {
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
            } else if self.operand_is_str(arg) {
                // Str-typed arg: expand to (int)len, (const char*)data for %.*s.
                if let Operand::Copy(place) | Operand::Move(place) = arg {
                    let slot = self.local_to_slot[place.local.0 as usize];
                    let slot_ty = self.lir_func.slots[slot.0 as usize].ty.clone();
                    let struct_id = match &slot_ty {
                        LirType::Struct(sid) => *sid,
                        _ => unreachable!(),
                    };

                    // Str fields: 0=data (Ptr), 1=len (I64), 2=cap (I64), 3=alloc (Ptr)
                    // Load .len (field 1) → cast to I32 for printf %.*s precision
                    let base = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                        dst: base,
                        slot,
                    });
                    let len_ptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: len_ptr,
                        base,
                        struct_id,
                        field: 1,
                    });
                    let len_load = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: len_load,
                        ptr: len_ptr,
                        ty: LirType::I64,
                    });
                    let len_i32 = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::IntCast {
                        dst: len_i32,
                        value: len_load,
                        to: LirType::I32,
                    });
                    lir_args.push(len_i32);

                    // Load .data (field 0) — const char*
                    let base2 = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                        dst: base2,
                        slot,
                    });
                    let data_ptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: data_ptr,
                        base: base2,
                        struct_id,
                        field: 0,
                    });
                    let data_load = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: data_load,
                        ptr: data_ptr,
                        ty: LirType::Ptr,
                    });
                    lir_args.push(data_load);
                } else {
                    lir_args.push(self.lower_operand(arg, bb));
                }
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
                self.lir_func
                    .block_mut(bb)
                    .insts
                    .push(Inst::SlotAddr { dst: addr, slot });
                addr
            } else {
                let dst = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::SlotLoad {
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
            let is_box_deref = place.projections.first() == Some(&Projection::Deref)
                && self.gir_types.get(self.gir_func.locals[place.local.0 as usize].type_id)
                    .map_or(false, |t| matches!(t, GirType::Named(n) if n.starts_with("Box__")));
            if ty.is_aggregate() && !is_box_deref {
                addr // aggregates: the address IS the value
            } else {
                let dst = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::Load {
                    dst,
                    ptr: addr,
                    ty,
                });
                dst
            }
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
                        if name == "GorgetString" {
                            if let Some(sid) = sr.lookup(name) {
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
                    if let Some(runtime_name) = collection_runtime_type(name) {
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
                Local { type_id: I32_TYPE, name_hint: Some("return".into()) },
                Local { type_id: I32_TYPE, name_hint: Some("x".into()) },
                Local { type_id: I32_TYPE, name_hint: Some("y".into()) },
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
            ref_locals: rustc_hash::FxHashSet::default(),
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
                Local { type_id: I64_TYPE, name_hint: None },
                Local { type_id: BOOL_TYPE, name_hint: Some("cond".into()) },
                Local { type_id: I64_TYPE, name_hint: Some("result".into()) },
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
            ref_locals: rustc_hash::FxHashSet::default(),
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
                Local { type_id: I64_TYPE, name_hint: None },
                Local { type_id: I64_TYPE, name_hint: None },
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
            ref_locals: rustc_hash::FxHashSet::default(),
        });
        module.functions.push(Function {
            name: "caller".into(),
            params: vec![],
            return_type: I64_TYPE,
            locals: vec![
                Local { type_id: I64_TYPE, name_hint: None },
                Local { type_id: I64_TYPE, name_hint: Some("result".into()) },
            ],
            blocks: vec![BasicBlock {
                instructions: vec![Instruction::Call {
                    dst: Some(LocalId(1)),
                    func: "callee".into(),
                    args: vec![Operand::Constant(Constant::I64(5))],
                    arg_owners: vec![],
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
            ref_locals: rustc_hash::FxHashSet::default(),
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
        });
        module.functions.push(Function {
            name: "main".into(),
            params: vec![],
            return_type: I32_TYPE,
            locals: vec![
                Local { type_id: I32_TYPE, name_hint: None },
                Local { type_id: I32_TYPE, name_hint: None },
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
            ref_locals: rustc_hash::FxHashSet::default(),
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
                Local { type_id: I32_TYPE, name_hint: None },
                Local { type_id: I32_TYPE, name_hint: None },
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
            ref_locals: rustc_hash::FxHashSet::default(),
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
}
